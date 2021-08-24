/*
 * File: obj-desc.c
 * Purpose: Create object name descriptions
 *
 * Copyright (c) 1997 - 2007 Angband contributors
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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
 * A modifier string, put where '#' goes in the basename below. The weird
 * games played with book names are to allow the non-essential part of the
 * name to be abbreviated when there is not much room to display.
 */
static const char *obj_desc_get_modstr(const struct object *obj)
{
    if (tval_is_skeleton(obj) && obj->pval)
        return "skeleton";

    if (tval_is_corpse(obj) && obj->pval)
        return "corpse";

    if (tval_can_have_flavor(obj))
        return (obj->kind->flavor? obj->kind->flavor->text: "");

    if (tval_is_book(obj))
        return obj->kind->name;

    return "";
}


/*
 * An object's basic name - a generic name for flavored objects (with the
 * actual name added later depending on awareness, the name from object.txt
 * for almost everything else, and a bit extra for books.
 */
static const char *obj_desc_get_basename(const struct object *obj, bool aware, bool terse, int mode)
{
    bool show_flavor = ((mode & ODESC_FLAVOR)? true: false);

    /* Artifacts are special */
    if (obj->artifact && (aware || obj->known->artifact || terse || !obj->kind->flavor))
        return obj->kind->name;

    /* Analyze the object */
    switch (obj->tval)
    {
        case TV_STONE:
        case TV_BOTTLE:
        case TV_DEED:
        case TV_CHEST:
        case TV_HORN:
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_MSTAFF:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_LIGHT:
        case TV_FOOD:
        case TV_FLASK:
        case TV_CROP:
        case TV_COOKIE:
            return obj->kind->name;

        case TV_SKELETON:
        {
            struct monster_race* race;

            if (!obj->pval) return "& Skeleton~";

            race = &r_info[obj->pval];
            return format("& %s #~", race->name);
        }

        case TV_CORPSE:
        {
            struct monster_race* race;

            if (!obj->pval) return "& Corpse~";

            race = &r_info[obj->pval];
            return format("& %s #~", race->name);
        }

        case TV_AMULET:
            return (show_flavor? "& # Amulet~": "& Amulet~");

        case TV_RING:
            return (show_flavor? "& # Ring~": "& Ring~");

        case TV_STAFF:
            return (show_flavor? "& # Sta|ff|ves|": "& Sta|ff|ves|");

        case TV_WAND:
            return (show_flavor? "& # Wand~": "& Wand~");

        case TV_ROD:
            return (show_flavor? "& # Rod~": "& Rod~");

        case TV_SCROLL:
            return (show_flavor? "& Scroll~ titled #": "& Scroll~");

        case TV_POTION:
            return (show_flavor? "& # Potion~": "& Potion~");

        case TV_MUSHROOM:
            return (show_flavor? "& # Mushroom~": "& Mushroom~");

        case TV_MAGIC_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Book~ of Magic Spells #";
        }

        case TV_PRAYER_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Holy Book~ of Prayers #";
        }

        case TV_NATURE_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Book~ of Nature Magics #";
        }

        case TV_SHADOW_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Book~ of Shadows #";
        }

        case TV_PSI_BOOK:
            return "& Crystal~ #";

        case TV_ELEM_BOOK:
        {
            if (terse) return "& Spellbook~ #";
            return "& Elemental Spellbook~ #";
        }
    }

    return "(nothing)";
}


/*
 * Start to description, indicating number/uniqueness
 */
static size_t obj_desc_name_prefix(char *buf, size_t max, size_t end, const struct object *obj,
    bool known, const char *basename, const char *modstr, bool terse)
{
    if (obj->number == 0)
        strnfcat(buf, max, &end, "no more ");
    else if (obj->number > 1)
        strnfcat(buf, max, &end, "%d ", obj->number);
    else if ((obj->known->artifact || known) && obj->artifact)
        strnfcat(buf, max, &end, "the ");

    else if (*basename == '&')
    {
        bool an = false;
        const char *lookahead = basename + 1;

        while (*lookahead == ' ') lookahead++;

        if (*lookahead == '#')
        {
            if (modstr && is_a_vowel(*modstr))
                an = true;
        }
        else if (is_a_vowel(*lookahead))
            an = true;

        if (!terse)
        {
            if (an)
                strnfcat(buf, max, &end, "an ");
            else
                strnfcat(buf, max, &end, "a ");
        }
    }

    return end;
}


/*
 * Format object obj's name into 'buf'.
 */
static size_t obj_desc_name(struct player *p, char *buf, size_t max, size_t end,
    const struct object *obj, bool prefix, int mode, bool terse, bool aware, bool known)
{
    const char *basename = obj_desc_get_basename(obj, aware, terse, mode);
    const char *modstr = obj_desc_get_modstr(obj);
    bool pluralise = false;

    /*
     * Pluralize if (not forced singular) and (not a known/visible artifact) and
     * (not one in stack or forced plural)
     */
    if ((obj->number > 1) || (mode & ODESC_PLURAL)) pluralise = true;
    if (mode & ODESC_SINGULAR) pluralise = false;

    /* Quantity prefix */
    if (prefix)
    {
        end = obj_desc_name_prefix(buf, max, end, obj, known, basename, modstr, terse);

        /* Pluralise for grammatical correctness */
        if (obj->number <= 0) pluralise = true;
    }

    /* Base name */
    end = obj_desc_name_format(buf, max, end, basename, modstr, pluralise);

    /* Append extra names of various kinds */
    if (aware && !obj->artifact && (obj->kind->flavor || tval_is_scroll_k(obj->kind)))
    {
        if (terse)
            strnfcat(buf, max, &end, " '%s'", obj->kind->name);
        else
            strnfcat(buf, max, &end, " of %s", obj->kind->name);
    }
    if ((obj->known->artifact || known) && obj->artifact)
    {
        if (obj->randart_seed)
        {
            char tmp_val[160];

            do_randart_name(obj->randart_seed, tmp_val, sizeof(tmp_val));
            strnfcat(buf, max, &end, " %s", tmp_val);
        }
        else
            strnfcat(buf, max, &end, " %s", obj->artifact->name);
    }
    if (obj->ego && obj->known->ego)
        strnfcat(buf, max, &end, " %s", obj->ego->name);

    return end;
}


/*
 * Is obj armor?
 */
static bool obj_desc_show_armor(const struct object *obj)
{
    if (obj->ac || tval_is_armor(obj)) return true;

    return false;
}


/*
 * Special descriptions for types of chest traps
 */
static size_t obj_desc_chest(const struct object *obj, char *buf, size_t max, size_t end,
    bool known)
{
    if (!tval_is_chest(obj)) return end;

    /* The chest is unopened, but we know nothing about its trap/lock */
    if (!known) return end;

    /* Describe the traps */
    strnfcat(buf, max, &end, format(" (%s)", chest_trap_name(obj)));

    return end;
}


/*
 * Describe combat properties of an item - damage dice, to-hit, to-dam, armor
 * class, missile multiplier
 */
static size_t obj_desc_combat(const struct object *obj, char *buf, size_t max, size_t end,
    bool known)
{
    s16b to_h, to_d;

    object_to_h(obj, &to_h);
    object_to_d(obj, &to_d);

    /* Display damage dice if they are known */
    if (kf_has(obj->kind->kind_flags, KF_SHOW_DICE))
    {
        if (known || (obj->known->dd && obj->known->ds))
            strnfcat(buf, max, &end, " (%dd%d)", obj->dd, obj->ds);
        else
            strnfcat(buf, max, &end, " (%dd%d)", obj->kind->dd, obj->kind->ds);
    }

    /* Display shooting power as part of the multiplier */
    if (kf_has(obj->kind->kind_flags, KF_SHOW_MULT))
    {
        s32b modifiers[OBJ_MOD_MAX];

        object_modifiers(obj, modifiers);

        if (modifiers[OBJ_MOD_MIGHT] && (known || obj->known->modifiers[OBJ_MOD_MIGHT]))
            strnfcat(buf, max, &end, " (x%d)", obj->pval + modifiers[OBJ_MOD_MIGHT]);
        else
            strnfcat(buf, max, &end, " (x%d)", obj->pval);
    }

    /* No more if the object hasn't been assessed */
    if (!(object_was_sensed(obj) || known)) return end;

    /* Special treatment for body armor with only a to-hit penalty */
    if ((obj->to_h < 0) && object_has_standard_to_h(obj))
        strnfcat(buf, max, &end, " (%+d)", obj->to_h);

    /* Show weapon bonuses if we know of any */
    else if ((known || (obj->known->to_h && obj->known->to_d)) &&
        (tval_is_weapon(obj) || to_d || to_h))
    {
        /* To-hit and to-dam runes known */
        strnfcat(buf, max, &end, " (%+d,%+d)", to_h, to_d);
    }
    else if (to_d && obj->known->to_d)
    {
        /* To-dam rune known only */
        strnfcat(buf, max, &end, " (%+d)", to_d);
    }
    else if (to_h && obj->known->to_h)
    {
        /* To-hit rune known only */
        strnfcat(buf, max, &end, " (%+d)", to_h);
    }

    /* Show armor bonuses */
    if (known || obj->known->to_a)
    {
        s16b to_a;

        object_to_a(obj, &to_a);

        if (obj_desc_show_armor(obj))
        {
            if (known || obj->known->ac)
                strnfcat(buf, max, &end, " [%d,%+d]", obj->ac, to_a);
            else
                strnfcat(buf, max, &end, " [%d,%+d]", obj->kind->ac, to_a);
        }
        else if (to_a)
            strnfcat(buf, max, &end, " [%+d]", to_a);
    }
    else if (obj_desc_show_armor(obj))
    {
        if (known || obj->known->ac)
            strnfcat(buf, max, &end, " [%d]", obj->ac);
        else
            strnfcat(buf, max, &end, " [%d]", obj->kind->ac);
    }

    return end;
}


/*
 * Describe remaining light for refuellable lights
 */
static size_t obj_desc_light(const struct object *obj, char *buf, size_t max, size_t end,
    bool known)
{
    /* Fuelled light sources get number of remaining turns appended */
    if (tval_is_light(obj) && known && !of_has(obj->flags, OF_NO_FUEL))
        strnfcat(buf, max, &end, " (%d turns)", obj->timeout);

    return end;
}


/*
 * Describe numerical modifiers to stats and other player qualities which
 * allow numerical bonuses - speed, stealth, etc
 */
static size_t obj_desc_mods(const struct object *obj, char *buf, size_t max, size_t end, bool known)
{
    int i, j, num_mods = 0;
    int mods[OBJ_MOD_MAX];
    s32b modifiers[OBJ_MOD_MAX];

    object_modifiers(obj, modifiers);

    /* Hack -- rings of polymorphing append the race name instead of its modifier */
    if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")))
    {
        struct monster_race *race = &r_info[obj->modifiers[OBJ_MOD_POLY_RACE]];

        if (known) strnfcat(buf, max, &end, " of %s", race->name);
        return end;
    }

    memset(mods, 0, OBJ_MOD_MAX * sizeof(int));

    /* Run through possible modifiers and store distinct ones */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        /* Check for known non-zero mods */
        if ((known || obj->known->modifiers[i]) && modifiers[i])
        {
            /* If no mods stored yet, store and move on */
            if (!num_mods)
            {
                mods[num_mods++] = modifiers[i];
                continue;
            }

            /* Run through the existing mods, quit on duplicates */
            for (j = 0; j < num_mods; j++)
            {
                if (mods[j] == modifiers[i]) break;
            }

            /* Add another mod if needed */
            if (j == num_mods)
                mods[num_mods++] = modifiers[i];
        }
    }

    if (!num_mods) return end;

    /* Print the modifiers */
    strnfcat(buf, max, &end, " <");
    for (j = 0; j < num_mods; j++)
    {
        if (j) strnfcat(buf, max, &end, ", ");
        strnfcat(buf, max, &end, "%+d", mods[j]);
    }
    strnfcat(buf, max, &end, ">");

    return end;
}


/*
 * Describe charges or charging status for re-usable items with magic effects
 */
static size_t obj_desc_charges(const struct object *obj, char *buf, size_t max, size_t end,
    bool aware)
{
    /* Wands and staves have charges, others may be charging */
    if (aware && tval_can_have_charges(obj))
        strnfcat(buf, max, &end, " (%d charge%s)", obj->pval, PLURAL(obj->pval));
    else if (obj->activation && (obj->timeout > 0))
    {
        if (obj->number > 1)
            strnfcat(buf, max, &end, " (%d charging)", number_charging(obj));
        else
            strnfcat(buf, max, &end, " (charging)");
    }

    return end;
}


/*
 * Add player-defined inscriptions or game-defined descriptions
 */
static size_t obj_desc_inscrip(struct player *p, const struct object *obj, char *buf,
    size_t max, size_t end, bool aware, bool known)
{
    const char *u[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
    int i, n = 0;

    /* Get inscription */
    if (obj->note)
        u[n++] = quark_str(obj->note);

    /* Use special inscription, if any */
    if (!known)
    {
        if (obj->origin == ORIGIN_WORTHLESS)
            u[n++] = "worthless";
        else if (tval_can_have_charges(obj) && (obj->pval == 0))
            u[n++] = "empty";
        else if (!aware && object_flavor_was_tried(p, obj))
            u[n++] = "tried";
    }

    /* Note curses */
    if ((known || obj->known->curses) && obj->curses)
        u[n++] = "cursed";

    /* Note ignore */
    if (p && ignore_item_ok(p, obj))
        u[n++] = "ignore";

    /* Note unknown properties */
    if (!object_runes_known(obj) && object_was_sensed(obj))
        u[n++] = "??";

    for (i = 0; i < n; i++)
    {
        if (i == 0) strnfcat(buf, max, &end, " {");
        strnfcat(buf, max, &end, "%s", u[i]);
        if (i < n - 1) strnfcat(buf, max, &end, ", ");
        else strnfcat(buf, max, &end, "}");
    }

    return end;
}


/*
 * Add "unseen" to the end of unaware items in stores
 */
static size_t obj_desc_aware(struct player *p, const struct object *obj, char *buf, size_t max,
    size_t end)
{
    if (p && !p->obj_aware[obj->kind->kidx]) strnfcat(buf, max, &end, " {unseen}");
    else if (obj->curses) strnfcat(buf, max, &end, " {cursed}");

    return end;
}


/*
 * Describes item "obj" into buffer "buf" of size "max".
 *
 * ODESC_BASE results in a base description.
 * ODESC_COMBAT will add to-hit, to-dam and AC info.
 * ODESC_EXTRA will add pval/charge/inscription/ignore info.
 * ODESC_STORE turns off ignore markers, for in-store display.
 * ODESC_PLURAL will pluralise regardless of the number in the stack.
 * ODESC_SINGULAR will never pluralise regardless of the number in the stack.
 * ODESC_ARTIFACT results in a description for artifacts (aware + known + without flavor).
 * ODESC_PREFIX prepends a 'the', 'a' or number.
 * ODESC_SALE turns off unseen and ignore markers, for items purchased from floor
 */
size_t object_desc(struct player *p, char *buf, size_t max, const struct object *obj, int mode)
{
    bool prefix = ((mode & ODESC_PREFIX)? true: false);
    bool terse = ((mode & ODESC_TERSE)? true: false);
    bool known;
    size_t end = 0;
    bool aware;

    /* Simple description for null item */
    if (!obj) return strnfmt(buf, max, "(nothing)");

    /* Unknown items and cash get straightforward descriptions */
    if (object_marked_aware(p, obj))
    {
        if (prefix) return strnfmt(buf, max, "an unknown item");
        return strnfmt(buf, max, "unknown item");
    }

    if (tval_is_money(obj))
    {
        return strnfmt(buf, max, "%d gold piece%s worth of %s", obj->pval, PLURAL(obj->pval),
            obj->kind->name);
    }

    /* Player is valid, description is not for artifacts and object is not in a store */
    if (p && !((mode & ODESC_ARTIFACT) || (mode & ODESC_STORE)))
    {
        bool show_flavor;

        /* See if the object is "aware" */
        aware = object_flavor_is_aware(p, obj);

        /* See if the object is "known" */
        known = object_is_known(p, obj);

        /* See if the object is "flavored" */
        show_flavor = (!terse && obj->kind->flavor);

        /* Allow flavors to be hidden when aware */
        if (aware && !OPT(p, show_flavors)) show_flavor = false;

        if (show_flavor) mode |= ODESC_FLAVOR;
    }

    /* Pretend known and aware, don't show flavors */
    else
    {
        aware = true;
        known = true;
    }

    /* Egos and kinds whose name we know are seen */
    if (p && obj->known->ego && obj->ego && !p->ego_everseen[obj->ego->eidx])
    {
        p->ego_everseen[obj->ego->eidx] = 1;
        Send_ego_everseen(p, obj->ego->eidx);
    }

    if (p && aware && !p->kind_everseen[obj->kind->kidx])
    {
        p->kind_everseen[obj->kind->kidx] = 1;
        Send_everseen(p, obj->kind->kidx);
    }

    /** Construct the name **/

    /* Copy the base name to the buffer */
    end = obj_desc_name(p, buf, max, end, obj, prefix, mode, terse, aware, known);

    /* Combat properties */
    if (mode & ODESC_COMBAT)
    {
        if (tval_is_chest(obj))
            end = obj_desc_chest(obj, buf, max, end, known);
        else if (tval_is_light(obj))
            end = obj_desc_light(obj, buf, max, end, known);

        end = obj_desc_combat(obj, buf, max, end, known);
    }

    /* Modifiers, charges, flavour details, inscriptions */
    if (mode & ODESC_EXTRA)
    {
        end = obj_desc_mods(obj, buf, max, end, known);

        end = obj_desc_charges(obj, buf, max, end, aware);

        /* Hack -- corpses slowly decompose */
        if (tval_is_corpse(obj))
        {
            /* Hack -- dump " (rotten)" if relevant */
            if (obj->decay <= obj->timeout / 5)
                strnfcat(buf, max, &end, " (rotten)");
        }

        if (mode & ODESC_SALE) return end;

        if (mode & ODESC_STORE)
            end = obj_desc_aware(p, obj, buf, max, end);
        else
            end = obj_desc_inscrip(p, obj, buf, max, end, aware, known);
    }

    return end;
}