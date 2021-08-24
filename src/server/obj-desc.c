/*
 * File: obj-desc.c
 * Purpose: Create object name descriptions
 *
 * Copyright (c) 1997 - 2007 Angband contributors
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


const char *inscrip_text[] =
{
    NULL,
    "average",
    "magical",
    "strange",
    "magical",
    "splendid",
    "excellent",
    "special"
};


/*
 * A modifier string, put where '#' goes in the basename below. The weird
 * games played with book names are to allow the non-essential part of the
 * name to be abbreviated when there is not much room to display.
 */
static const char *obj_desc_get_modstr(const struct object *obj)
{
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
        case TV_SKELETON:
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
            return obj->kind->name;

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

        case TV_SORCERY_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Book~ of Sorcery #";
        }

        case TV_SHADOW_BOOK:
            return "& Book~ of Shadows #";

        case TV_HUNT_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Book~ of Hunting #";
        }

        case TV_PSI_BOOK:
            return "& Crystal~ #";

        case TV_DEATH_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Book~ of Necromancy #";
        }

        case TV_ELEM_BOOK:
        {
            if (terse) return "& Spellbook~ #";
            return "& Elemental Spellbook~ #";
        }

        case TV_SUMMON_BOOK:
        {
            if (terse) return "& Book~ #";
            return "& Book~ of Summoning #";
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
    else if ((object_name_is_visible(obj) || known) && obj->artifact)
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

    if ((obj->number > 1) || (mode & ODESC_PLURAL)) pluralise = true;
    if (mode & ODESC_SINGULAR) pluralise = false;

    if (p && aware && !p->kind_everseen[obj->kind->kidx])
    {
        p->kind_everseen[obj->kind->kidx] = 1;
        Send_everseen(p, obj->kind->kidx);
    }

    if (prefix)
    {
        end = obj_desc_name_prefix(buf, max, end, obj, known, basename, modstr, terse);

        /* Pluralise for grammatical correctness */
        if (obj->number <= 0) pluralise = true;
    }

    end = obj_desc_name_format(buf, max, end, basename, modstr, pluralise);

    /** Append extra names of various kinds **/

    if (aware && !obj->artifact && (obj->kind->flavor || tval_is_scroll_k(obj->kind)))
    {
        if (terse)
            strnfcat(buf, max, &end, " '%s'", obj->kind->name);
        else
            strnfcat(buf, max, &end, " of %s", obj->kind->name);
    }

    if ((object_name_is_visible(obj) || known) && obj->artifact)
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

    if (object_ego_is_visible(obj))
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
    if (!known) return end;

    /* May be empty, disarmed or trapped */
    if (!obj->pval)
        strnfcat(buf, max, &end, " (empty)");
    else if (!is_locked_chest(obj))
    {
        if (chest_trap_type(obj) != 0)
            strnfcat(buf, max, &end, " (disarmed)");
        else
            strnfcat(buf, max, &end, " (unlocked)");
    }
    else
    {
        /* Describe the traps */
        switch (chest_trap_type(obj))
        {
            case 0:
                strnfcat(buf, max, &end, " (Locked)");
                break;

            case CHEST_LOSE_STR:
                strnfcat(buf, max, &end, " (Poison Needle)");
                break;

            case CHEST_LOSE_CON:
                strnfcat(buf, max, &end, " (Poison Needle)");
                break;

            case CHEST_POISON:
                strnfcat(buf, max, &end, " (Gas Trap)");
                break;

            case CHEST_PARALYZE:
                strnfcat(buf, max, &end, " (Gas Trap)");
                break;

            case CHEST_EXPLODE:
                strnfcat(buf, max, &end, " (Explosion Device)");
                break;

            case CHEST_SUMMON:
                strnfcat(buf, max, &end, " (Summoning Runes)");
                break;

            default:
                strnfcat(buf, max, &end, " (Multiple Traps)");
                break;
        }
    }

    return end;
}


/*
 * Describe combat properties of an item - damage dice, to-hit, to-dam, armor
 * class, missile multiplier
 */
static size_t obj_desc_combat(const struct object *obj, char *buf, size_t max, size_t end,
    bool known)
{
    if (kf_has(obj->kind->kind_flags, KF_SHOW_DICE))
    {
        /* Only display the real damage dice if the combat stats are known */
        if (known || object_attack_plusses_are_visible(obj))
            strnfcat(buf, max, &end, " (%dd%d)", obj->dd, obj->ds);
        else
            strnfcat(buf, max, &end, " (%dd%d)", obj->kind->dd, obj->kind->ds);
    }

    if (kf_has(obj->kind->kind_flags, KF_SHOW_MULT))
    {
        /* Display shooting power as part of the multiplier */
        if (obj->modifiers[OBJ_MOD_MIGHT] &&
            (known || object_this_mod_is_visible(obj, OBJ_MOD_MIGHT)))
        {
            strnfcat(buf, max, &end, " (x%d)", obj->pval + obj->modifiers[OBJ_MOD_MIGHT]);
        }
        else
            strnfcat(buf, max, &end, " (x%d)", obj->pval);
    }

    /* Show weapon bonuses */
    if (known || object_attack_plusses_are_visible(obj))
    {
        if (tval_is_weapon(obj) || obj->to_d || obj->to_h)
        {
            /* Make an exception for body armor with only a to-hit penalty */
            if ((obj->to_h < 0) && (obj->to_d == 0) && tval_is_body_armor(obj))
                strnfcat(buf, max, &end, " (%+d)", obj->to_h);

            /* Otherwise, always use the full tuple */
            else
                strnfcat(buf, max, &end, " (%+d,%+d)", obj->to_h, obj->to_d);
        }
    }

    /* Show armor bonuses */
    if (known || object_defence_plusses_are_visible(obj))
    {
        if (obj_desc_show_armor(obj))
            strnfcat(buf, max, &end, " [%d,%+d]", obj->ac, obj->to_a);
        else if (obj->to_a)
            strnfcat(buf, max, &end, " [%+d]", obj->to_a);
    }
    else if (obj_desc_show_armor(obj))
        strnfcat(buf, max, &end, " [%d]", (object_was_sensed(obj)? obj->ac: obj->kind->ac));

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

    /* Hack -- rings of polymorphing append the race name instead of its modifier */
    if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")))
    {
        struct monster_race *race = &r_info[obj->modifiers[OBJ_MOD_POLY_RACE]];

        if (known) strnfcat(buf, max, &end, " of %s", race->name);
        return end;
    }

    memset(mods, 0, sizeof(mods));

    /* Run through possible modifiers and store distinct ones */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        /* Check for known non-zero mods */
        if ((known || object_this_mod_is_visible(obj, i)) && obj->modifiers[i])
        {
            /* If no mods stored yet, store and move on */
            if (!num_mods)
            {
                mods[num_mods++] = obj->modifiers[i];
                continue;
            }

            /* Run through the existing mods, quit on duplicates */
            for (j = 0; j < num_mods; j++)
            {
                if (mods[j] == obj->modifiers[i]) break;
            }

            /* Add another mod if needed */
            if (j == num_mods)
                mods[num_mods++] = obj->modifiers[i];
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
    else if (obj->timeout > 0)
    {
        if (tval_can_have_timeout(obj) && (obj->number > 1))
            strnfcat(buf, max, &end, " (%d charging)", number_charging(obj));
        else if (!tval_is_corpse(obj) && !(tval_is_light(obj) && !obj->artifact))
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
    const char *u[4] = {NULL, NULL, NULL, NULL};
    int i, n = 0;
    obj_pseudo_t feel = object_pseudo(p, obj, aware, known);
    bitflag flags_known[OF_SIZE], f2[OF_SIZE];

    /* Hack -- non-sense machines don't give a feeling */
    if (!is_sense_machine(obj)) feel = INSCRIP_NULL;

    object_flags_known(obj, flags_known, aware);

    /* Get inscription */
    if (obj->note)
        u[n++] = quark_str(obj->note);

    /* Use special inscription, if any */
    if (!known)
    {
        if (obj->origin == ORIGIN_WORTHLESS)
            u[n++] = "worthless";
        else if (feel)
        {
            /* Cannot tell excellent vs strange vs splendid until wield */
            if (!object_was_worn(obj) && obj->ego)
                u[n++] = "ego";
            else
                u[n++] = inscrip_text[feel];
        }
        else if (tval_can_have_charges(obj) && (obj->pval == 0))
            u[n++] = "empty";
        else if (object_was_worn(obj))
        {
            if (tval_is_weapon(obj) || tval_is_tool(obj))
                u[n++] = "wielded";
            else
                u[n++] = "worn";
        }
        else if (!aware && object_flavor_was_tried(p, obj))
            u[n++] = "tried";
    }

    /* Note curses */
    create_mask(f2, false, OFT_CURSE, OFT_MAX);
    if (of_is_inter(flags_known, f2))
        u[n++] = "cursed";

    /* Note ignore */
    if (p && ignore_item_ok(p, obj))
        u[n++] = "ignore";

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
    bool unseen = (p? !p->obj_aware[obj->kind->kidx]: false);

    if (unseen) strnfcat(buf, max, &end, " {unseen}");

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
        if (aware && !OPT_P(p, show_flavors)) show_flavor = false;

        if (show_flavor) mode |= ODESC_FLAVOR;
    }

    /* Pretend known and aware, don't show flavors */
    else
    {
        aware = true;
        known = true;
    }

    /* Egos whose name we know are seen */
    if (p && object_name_is_visible(obj) && obj->ego && !p->ego_everseen[obj->ego->eidx])
    {
        p->ego_everseen[obj->ego->eidx] = 1;
        Send_ego_everseen(p, obj->ego->eidx);
    }

    /*** Some things get really simple descriptions ***/

    if (object_marked_aware(p, obj))
    {
        if (prefix) return strnfmt(buf, max, "an unknown item");
        return strnfmt(buf, max, "unknown item");
    }

    if (tval_is_money(obj))
        return strnfmt(buf, max, "%d gold pieces worth of %s", obj->pval, obj->kind->name);

    /** Construct the name **/

    /* Copy the base name to the buffer */
    end = obj_desc_name(p, buf, max, end, obj, prefix, mode, terse, aware, known);

    if (mode & ODESC_COMBAT)
    {
        if (tval_is_chest(obj))
            end = obj_desc_chest(obj, buf, max, end, known);
        else if (tval_is_light(obj))
            end = obj_desc_light(obj, buf, max, end, known);

        end = obj_desc_combat(obj, buf, max, end, known);
    }

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