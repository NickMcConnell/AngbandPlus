/*
 * File: obj-desc.c
 * Purpose: Create object name descriptions
 *
 * Copyright (c) 1997 - 2007 Angband contributors
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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


#include "../s-angband.h"
#include "../../common/tvalsval.h"
#include "pval.h"
#include "../squelch.h"


/*
 * Puts a very stripped-down version of an object's name into buf.
 *
 * Just truncates if the buffer isn't big enough.
 */
void object_kind_name(char *buf, size_t max, const object_kind *kind, bool aware)
{
    /* If not aware, use flavor */
    if (!aware && kind->flavor)
    {
        if ((kind->tval == TV_FOOD) && (kind->sval >= SV_FOOD_MIN_SHROOM))
            strnfmt(buf, max, "%s Mushroom", kind->flavor->text);
        else
        {
            /* Plain flavour (e.g. Copper) will do. */
            my_strcpy(buf, kind->flavor->text, max);
        }
    }

    /* Use proper name (Healing, or whatever) */
    else
    {
        char *t;

        if ((kind->tval == TV_FOOD) && (kind->sval >= SV_FOOD_MIN_SHROOM))
        {
            my_strcpy(buf, "Mushroom of ", max);
            max -= strlen(buf);
            t = buf + strlen(buf);
        }
        else
            t = buf;

        /* Format remainder of the string */
        obj_desc_name_format(t, max, 0, kind->name, NULL, FALSE);
    }
}


static const char *obj_desc_get_modstr(const object_type *o_ptr, bool aware)
{
    /* Known artifacts get special treatment */
    if (o_ptr->artifact && aware) return "";

    switch (o_ptr->tval)
    {
        case TV_CORPSE:
        {
            if (!o_ptr->pval[DEFAULT_PVAL]) break;
            return (o_ptr->kind->name);
        }

        case TV_AMULET:
        case TV_RING:
        case TV_STAFF:
        case TV_WAND:
        case TV_ROD:
        case TV_POTION:
        case TV_SCROLL:
            return (o_ptr->kind->flavor? o_ptr->kind->flavor->text: "");

        case TV_FOOD:
        {
            if (o_ptr->sval < SV_FOOD_MIN_SHROOM) break;
            return (o_ptr->kind->flavor? o_ptr->kind->flavor->text: "");
        }

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_SORCERY_BOOK:
        case TV_SHADOW_BOOK:
        case TV_HUNT_BOOK:
        case TV_PSI_BOOK:
        case TV_DEATH_BOOK:
        case TV_ELEM_BOOK:
        case TV_SUMMON_BOOK:
            return (o_ptr->kind->name);
    }

    return "";
}


static bool obj_desc_append_name(const object_type *o_ptr, bool aware)
{
    /* Known artifacts get special treatment */
    if (o_ptr->artifact && aware) return FALSE;

    switch (o_ptr->tval)
    {
        case TV_AMULET:
        case TV_RING:
        case TV_STAFF:
        case TV_WAND:
        case TV_ROD:
        case TV_SCROLL:
        case TV_POTION:
            return aware;

        case TV_FOOD:
        {
            if (o_ptr->sval < SV_FOOD_MIN_SHROOM) break;
            return aware;
        }
    }

    return FALSE;
}


static const char *obj_desc_get_basename(const object_type *o_ptr, bool aware,
    bool show_flavor)
{
    /* Known artifacts get special treatment */
    if (o_ptr->artifact && aware) return (o_ptr->kind->name);

    /* Analyze the object */
    switch (o_ptr->tval)
    {
        case TV_SKELETON:
        case TV_STONE:
        case TV_BOTTLE:
        case TV_SPIKE:
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
        case TV_FLASK:
        case TV_CROP:
            return (o_ptr->kind->name);

        case TV_CORPSE:
        {
            monster_race* r_ptr;

            if (!o_ptr->pval[DEFAULT_PVAL]) return "& Corpse~";

            r_ptr = &r_info[o_ptr->pval[DEFAULT_PVAL]];
            return format("& %s #~", r_ptr->name);
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

        case TV_FOOD:
        {
            if (o_ptr->sval >= SV_FOOD_MIN_SHROOM)
                return (show_flavor? "& # Mushroom~": "& Mushroom~");
            return (o_ptr->kind->name);
        }

        case TV_MAGIC_BOOK:
            return "& Book~ of Magic Spells #";

        case TV_PRAYER_BOOK:
            return "& Holy Book~ of Prayers #";

        case TV_SORCERY_BOOK:
            return "& Book~ of Sorcery #";

        case TV_SHADOW_BOOK:
            return "& Book~ of Shadows #";

        case TV_HUNT_BOOK:
            return "& Book~ of Hunting #";

        case TV_PSI_BOOK:
            return "& Crystal~ #";

        case TV_DEATH_BOOK:
            return "& Book~ of Necromancy #";

        case TV_ELEM_BOOK:
            return "& Elemental Spellbook~ #";

        case TV_SUMMON_BOOK:
            return "& Book~ of Summoning #";
    }

    return "(nothing)";
}


static size_t obj_desc_name_prefix(char *buf, size_t max, size_t end,
    const object_type *o_ptr, bool known, const char *basename, const char *modstr)
{
    if (o_ptr->number <= 0)
        strnfcat(buf, max, &end, "no more ");
    else if (o_ptr->number > 1)
        strnfcat(buf, max, &end, "%d ", o_ptr->number);
    else if ((object_name_is_visible(o_ptr) || known) && o_ptr->artifact)
        strnfcat(buf, max, &end, "the ");

    else if (*basename == '&')
    {
        bool an = FALSE;
        const char *lookahead = basename + 1;

        while (*lookahead == ' ') lookahead++;

        if (*lookahead == '#')
        {
            if (modstr && is_a_vowel(*modstr))
                an = TRUE;
        }
        else if (is_a_vowel(*lookahead))
            an = TRUE;

        if (an)
            strnfcat(buf, max, &end, "an ");
        else
            strnfcat(buf, max, &end, "a ");
    }

    return end;
}


/*
 * Format object o_ptr's name into 'buf'.
 */
static size_t obj_desc_name(struct player *p, char *buf, size_t max, size_t end,
    const object_type *o_ptr, bool prefix, odesc_detail_t mode, bool aware, bool known,
    bool show_flavor)
{
    const char *basename = obj_desc_get_basename(o_ptr, aware, show_flavor);
    const char *modstr = obj_desc_get_modstr(o_ptr, aware);
    bool append_name = obj_desc_append_name(o_ptr, aware);
    bool pluralise = FALSE;

    if (o_ptr->number > 1) pluralise = TRUE;
    if (mode == ODESC_SINGULAR) pluralise = FALSE;

    if (p && aware && !p->kind_everseen[o_ptr->kind->kidx]) p->kind_everseen[o_ptr->kind->kidx] = 1;

    /* Add a pseudo-numerical prefix if desired */
    if (prefix)
    {
        end = obj_desc_name_prefix(buf, max, end, o_ptr, known, basename, modstr);

        /* Pluralise for grammatical correctness */
        if (o_ptr->number <= 0) pluralise = TRUE;
    }

    end = obj_desc_name_format(buf, max, end, basename, modstr, pluralise);

    /** Append extra names of various kinds **/

    if (append_name)
        strnfcat(buf, max, &end, " of %s", o_ptr->kind->name);

    if ((object_name_is_visible(o_ptr) || known) && o_ptr->artifact)
    {
        if (o_ptr->randart_seed)
        {
            char tmp_val[160];

            do_randart_name(o_ptr->randart_seed, tmp_val, sizeof(tmp_val));
            strnfcat(buf, max, &end, " %s", tmp_val);
        }
        else
            strnfcat(buf, max, &end, " %s", o_ptr->artifact->name);
    }

    if (object_ego_is_visible(o_ptr))
        strnfcat(buf, max, &end, " %s", o_ptr->ego->name);

    return end;
}


/*
 * Is o_ptr armor?
 */
static bool obj_desc_show_armor(const object_type *o_ptr)
{
    if (o_ptr->ac) return TRUE;

    return armor_p(o_ptr);
}


/*
 * Is o_ptr weapon?
 */
static bool obj_desc_show_weapon(const object_type *o_ptr)
{
    if (o_ptr->to_d || o_ptr->to_h) return TRUE;

    return (wieldable_p(o_ptr) || (o_ptr->tval == TV_MSTAFF));
}


static size_t obj_desc_chest(const object_type *o_ptr, char *buf, size_t max,
    size_t end, bool known)
{
    if (o_ptr->tval != TV_CHEST) return end;
    if (!known) return end;

    /* May be "empty" */
    if (!o_ptr->pval[DEFAULT_PVAL])
        strnfcat(buf, max, &end, " (empty)");

    /* May be "disarmed" */
    else if (!is_locked_chest(o_ptr))
    {
        if (chest_trap_type(o_ptr) != 0)
            strnfcat(buf, max, &end, " (disarmed)");
        else
            strnfcat(buf, max, &end, " (unlocked)");
    }

    /* Describe the traps, if any */
    else
    {
        /* Describe the traps */
        switch (chest_trap_type(o_ptr))
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


static size_t obj_desc_combat(struct player *p, const object_type *o_ptr, char *buf,
    size_t max, size_t end, bool aware, bool known)
{
    bitflag flags[OF_SIZE];

    object_flags(o_ptr, flags);

    if (of_has(flags, OF_SHOW_DICE))
    {
        /* Only display the real damage dice if the combat stats are known */
        if (known || object_attack_plusses_are_visible(p, o_ptr))
            strnfcat(buf, max, &end, " (%dd%d)", o_ptr->dd, o_ptr->ds);
        else
            strnfcat(buf, max, &end, " (%dd%d)", o_ptr->kind->dd, o_ptr->kind->ds);
    }

    if (of_has(flags, OF_SHOW_MULT))
    {
        int power = (o_ptr->sval % 10);

        /* Display shooting power as part of the multiplier */
        if (of_has(flags, OF_MIGHT))
        {
            int i = which_pval(o_ptr, OF_MIGHT);

            if (known || object_this_pval_is_visible(o_ptr, i, aware))
                power += o_ptr->pval[i];
        }

        strnfcat(buf, max, &end, " (x%d)", power);
    }

    /* Show weapon bonuses */
    if (known || object_attack_plusses_are_visible(p, o_ptr))
    {
        if (obj_desc_show_weapon(o_ptr))
        {
            /* Make an exception for body armor with only a to-hit penalty */
            if ((o_ptr->to_h < 0) && (o_ptr->to_d == 0) && body_armor_p(o_ptr))
                strnfcat(buf, max, &end, " (%+d)", o_ptr->to_h);

            /* Otherwise, always use the full tuple */
            else
                strnfcat(buf, max, &end, " (%+d,%+d)", o_ptr->to_h, o_ptr->to_d);
        }
    }

    /* Show armor bonuses */
    if (known || object_defence_plusses_are_visible(p, o_ptr))
    {
        if (obj_desc_show_armor(o_ptr))
            strnfcat(buf, max, &end, " [%d,%+d]", o_ptr->ac, o_ptr->to_a);
        else if (o_ptr->to_a)
            strnfcat(buf, max, &end, " [%+d]", o_ptr->to_a);
    }
    else if (obj_desc_show_armor(o_ptr))
        strnfcat(buf, max, &end, " [%d]",
            (object_was_sensed(o_ptr)? o_ptr->ac: o_ptr->kind->ac));

    return end;
}


static size_t obj_desc_light(const object_type *o_ptr, char *buf, size_t max,
    size_t end, bool known)
{
    bitflag f[OF_SIZE];

    object_flags(o_ptr, f);

    if (o_ptr->tval != TV_LIGHT) return end;
    if (!known) return end;
    if (of_has(f, OF_NO_FUEL)) return end;

    /* Fuelled light sources get number of remaining turns appended */
    strnfcat(buf, max, &end, " (%d turns)", o_ptr->timeout);

    return end;
}


static size_t obj_desc_pval(const object_type *o_ptr, char *buf, size_t max, size_t end,
    bool aware, bool known)
{
    size_t i;
    bool any = FALSE;

    if (!o_ptr->num_pvals) return end;

    /* Hack -- Rings of Polymorphing append the race name instead of its pval */
    if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_POLYMORPHING))
    {
        monster_race *r_ptr = &r_info[o_ptr->pval[which_pval(o_ptr, OF_POLY_RACE)]];

        if (known) strnfcat(buf, max, &end, " of %s", r_ptr->name);
        return end;
    }

    strnfcat(buf, max, &end, " <");

    for (i = 0; i < o_ptr->num_pvals; i++)
    {
        if (known || object_this_pval_is_visible(o_ptr, i, aware))
        {
            if (any) strnfcat(buf, max, &end, ", ");
            strnfcat(buf, max, &end, "%+d", o_ptr->pval[i]);
            any = TRUE;
        }
    }

    strnfcat(buf, max, &end, ">");

    return end;
}


static size_t obj_desc_charges(const object_type *o_ptr, char *buf, size_t max,
    size_t end, bool aware)
{
    /* Wands and Staffs have charges */
    if (aware && ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)))
        strnfcat(buf, max, &end, " (%d charge%s)", o_ptr->pval[DEFAULT_PVAL],
            PLURAL(o_ptr->pval[DEFAULT_PVAL]));

    /* Charging things */
    else if (o_ptr->timeout > 0)
    {
        if ((o_ptr->tval == TV_ROD) && (o_ptr->number > 1))
        {
            int power;
            int time_base = randcalc(o_ptr->kind->time, 0, MINIMISE);

            if (!time_base) time_base = 1;

            /*
             * Find out how many rods are charging, by dividing
             * current timeout by each rod's maximum timeout.
             * Ensure that any remainder is rounded up.  Display
             * very discharged stacks as merely fully discharged.
             */
            power = (o_ptr->timeout + (time_base - 1)) / time_base;
            if (power > o_ptr->number) power = o_ptr->number;

            /* Display prettily */
            strnfcat(buf, max, &end, " (%d charging)", power);
        }

        /* Artifacts, single rods */
        else if ((o_ptr->tval != TV_CORPSE) && !((o_ptr->tval == TV_LIGHT) && !o_ptr->artifact))
            strnfcat(buf, max, &end, " (charging)");
    }

    return end;
}


static size_t obj_desc_inscrip(struct player *p, const object_type *o_ptr, char *buf,
    size_t max, size_t end, bool aware, bool known)
{
    const char *u[4] = {NULL, NULL, NULL, NULL};
    int i, n = 0;
    obj_pseudo_t feel = object_pseudo(p, o_ptr, aware, known);
    bitflag flags_known[OF_SIZE], f2[OF_SIZE];
    bool display_worn = FALSE;
    bool display_fired = FALSE;

    /* Hack -- Non-sense machines don't give a feeling */
    if (!is_sense_machine(o_ptr)) feel = INSCRIP_NULL;

    /* Hack -- Don't print "wielded" or "worn" if not equippable */
    if (p) display_worn = (wield_slot(p, o_ptr) != -1);

    /* Hack -- Don't print "fired" if not a missile */
    if (obj_is_ammo(p, o_ptr)) display_fired = TRUE;

    object_flags_known(o_ptr, flags_known, aware);

    /* Get inscription */
    if (o_ptr->note)
        u[n++] = quark_str(o_ptr->note);

    /* Use special inscription, if any */
    if (!known && (o_ptr->ident & IDENT_WORTHLESS))
        u[n++] = "worthless";
    else if (!known && feel)
    {
        /* Cannot tell excellent vs strange vs splendid until wield */
        if (!object_was_worn(o_ptr) && o_ptr->ego)
            u[n++] = "ego";
        else
            u[n++] = inscrip_text[feel];
    }
    else if ((o_ptr->ident & IDENT_EMPTY) && !known)
        u[n++] = "empty";
    else if (!known && object_was_worn(o_ptr) && display_worn)
    {
        switch (o_ptr->tval)
        {
            case TV_HAFTED:
            case TV_POLEARM:
            case TV_SWORD:
            case TV_MSTAFF:
            case TV_BOW:
            case TV_DIGGING:
            case TV_HORN:
            case TV_ROCK:
            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT: u[n++] = "wielded"; break;
            default: u[n++] = "worn";
        }
    }
    else if (!known && object_was_fired(o_ptr) && display_fired)
        u[n++] = "fired";
    else if (!aware && object_flavor_was_tried(p, o_ptr))
        u[n++] = "tried";

    /* Note curses */
    create_mask(f2, FALSE, OFT_CURSE, OFT_MAX);
    if (of_is_inter(flags_known, f2))
        u[n++] = "cursed";

    /* Note squelch */
    if (p && squelch_item_ok(p, o_ptr))
        u[n++] = "squelch";

    for (i = 0; i < n; i++)
    {
        if (i == 0) strnfcat(buf, max, &end, " {");
        strnfcat(buf, max, &end, "%s", u[i]);
        if (i < n - 1) strnfcat(buf, max, &end, ", ");
        else strnfcat(buf, max, &end, "}");
    }

    return end;
}


/* Add "unseen" to the end of unaware items in stores */
static size_t obj_desc_aware(struct player *p, const object_type *o_ptr, char *buf, size_t max,
    size_t end)
{
    bool unseen = (p? !p->obj_aware[o_ptr->kind->kidx]: FALSE);

    if (unseen) strnfcat(buf, max, &end, " {unseen}");

    return end;
}


static bool object_marked_aware(struct player *p, const object_type *obj)
{
    struct cave *c = cave_get(obj->depth);
    object_type *o_ptr;
    s16b this_o_idx, next_o_idx = 0;

    if (!p) return FALSE;
    if (!c) return FALSE;

    for (this_o_idx = c->o_idx[obj->iy][obj->ix]; this_o_idx; this_o_idx = next_o_idx)
    {
        o_ptr = object_byid(this_o_idx);
        next_o_idx = o_ptr->next_o_idx;
        if (o_ptr == obj) return (p->obj_marked[this_o_idx] == MARK_AWARE);
    }

    return FALSE;
}


/*
 * Describes item "o_ptr" into buffer "buf" of size "max".
 *
 * If ODESC_PREFIX is set, then the name will be prefixed with a pseudo-numeric
 * indicator of the number of items in the pile.
 *
 * Modes (ODESC_PREFIX is set):
 *   ODESC_BASE     -- An Adamant Amulet/An Amulet of Death
 *   ODESC_ARTIFACT -- The Amulet of Death
 *   ODESC_SALE     -- An Amulet of Death [1,+3] (+2 to Stealth)
 *   ODESC_STORE    -- Chain Mail of Death [1,+3] (+2 to Stealth) {nifty}
 *   ODESC_FULL     -- 5 Adamant Rings {nifty} (squelch)
 *                     5 Rings of Death [1,+3] (+2 to Stealth) {nifty} (squelch)
 *
 * Modes (ODESC_PREFIX is not set):
 *   ODESC_BASE     -- Adamant Amulet/Amulet of Death
 *   ODESC_ARTIFACT -- Amulet of Death
 *   ODESC_SALE     -- Amulet of Death [1,+3] (+2 to Stealth)
 *   ODESC_STORE    -- Chain Mail of Death [1,+3] (+2 to Stealth) {nifty}
 *   ODESC_FULL     -- Adamant Rings {nifty} (squelch)
 *                     Rings of Death [1,+3] (+2 to Stealth) {nifty} (squelch)
 *
 * ODESC_SINGULAR: same as ODESC_FULL, display as if the number of items was 1
 */
size_t object_desc(struct player *p, char *buf, size_t max, const object_type *o_ptr,
    odesc_detail_t mode)
{
    bool prefix = (mode & ODESC_PREFIX);
    bool aware;
    bool known;
    bool show_flavor;
    size_t end = 0, i = 0;

    /* Hack */
    mode &= ~ODESC_PREFIX;

    /*** Some things get really simple descriptions ***/

    /* Simple description for null item */
    if (!o_ptr->tval) return strnfmt(buf, max, "(nothing)");

    if (object_marked_aware(p, o_ptr))
    {
        if (prefix) return strnfmt(buf, max, "an unknown item");
        return strnfmt(buf, max, "unknown item");
    }

    if (o_ptr->tval == TV_GOLD)
    {
        return strnfmt(buf, max, "%ld gold pieces worth of %s", (long)o_ptr->pval[DEFAULT_PVAL],
            o_ptr->kind->name);
    }

    /*** Check some flags ***/

    /*
     * Player is valid, mode is not ODESC_ARTIFACT and object is not
     * in the inventory of a store
     */
    if (p && (mode != ODESC_ARTIFACT) && (mode != ODESC_STORE))
    {
        /* See if the object is "aware" */
        aware = object_flavor_is_aware(p, o_ptr);

        /* See if the object is "known" */
        known = object_is_known(p, o_ptr);

        /* See if the object is "flavored" */
        show_flavor = (o_ptr->kind->flavor? TRUE: FALSE);

        /* Allow flavors to be hidden when aware */
        if (aware && !OPT_P(p, show_flavors)) show_flavor = FALSE;
    }
    else
    {
        /* Don't show flavors */
        show_flavor = FALSE;

        /* Pretend known and aware */
        aware = TRUE;
        known = TRUE;
    }

    /* We've seen it at least once now we're aware of it */
    if (p && known && o_ptr->ego) p->ego_everseen[o_ptr->ego->eidx] = 1;

    /** Construct the name **/

    /* Copy the base name to the buffer */
    end = obj_desc_name(p, buf, max, end, o_ptr, prefix, mode, aware, known, show_flavor);

    if (mode < ODESC_SALE) return end;

    if (o_ptr->tval == TV_CHEST)
        end = obj_desc_chest(o_ptr, buf, max, end, known);
    else if (o_ptr->tval == TV_LIGHT)
        end = obj_desc_light(o_ptr, buf, max, end, known);

    end = obj_desc_combat(p, o_ptr, buf, max, end, aware, known);

    for (i = 0; i < o_ptr->num_pvals; i++)
    {
        /* Hack -- Exclude food, potions, ... */
        if (of_is_empty(o_ptr->pval_flags[i])) continue;

        if (known || object_this_pval_is_visible(o_ptr, i, aware))
        {
            end = obj_desc_pval(o_ptr, buf, max, end, aware, known);
            break;
        }
    }

    end = obj_desc_charges(o_ptr, buf, max, end, aware);

    /* Hack -- Corpses slowly decompose */
    if (o_ptr->tval == TV_CORPSE)
    {
        /* Hack -- Dump " (rotten)" if relevant */
        if (o_ptr->pval[DEFAULT_PVAL + 1] <= o_ptr->timeout / 5)
            strnfcat(buf, max, &end, " (rotten)");
    }

    if (mode < ODESC_STORE) return end;

    if (mode == ODESC_STORE)
        end = obj_desc_aware(p, o_ptr, buf, max, end);
    else
        end = obj_desc_inscrip(p, o_ptr, buf, max, end, aware, known);

    return end;
}