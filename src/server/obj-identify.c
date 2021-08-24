/*
 * File: obj-identify.c
 * Purpose: Object identification and knowledge routines
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2009 Brian Bull
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


/*
 * Object knowledge predicates
 * These answer questions about an object's ID status
 */


/*
 * Returns whether an object counts as "known" due to EASY_KNOW status
 */
bool easy_know(const struct object *obj, bool aware)
{
    return (aware && kf_has(obj->kind->kind_flags, KF_EASY_KNOW));
}


/*
 * Is the player aware of all of an object's flags?
 *
 * obj is the object
 */
static bool object_all_flags_are_known(const struct object *obj, bool aware)
{
    return (easy_know(obj, aware) || of_is_subset(obj->known->flags, obj->flags));
}


/*
 * Is the player aware of all of an object's modifiers?
 *
 * obj is the object
 */
static bool object_all_modifiers_are_known(const struct object *obj, bool aware)
{
    size_t i;

    if (easy_know(obj, aware)) return true;

    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (!obj->known->modifiers[i])
            return false;
    }

    return true;
}


/*
 * Is the player aware of all of an object's elemental properties?
 *
 * obj is the object
 */
static bool object_all_elements_are_known(const struct object *obj, bool aware)
{
    size_t i;

    if (easy_know(obj, aware)) return true;

    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Only check if the flags are set if there's something to look at */
        if ((obj->el_info[i].res_level != 0) && !obj->known->el_info[i].res_level)
            return false;
    }

    return true;
}


/*
 * Is the player aware of all of an object's brands and slays?
 *
 * obj is the object
 */
static bool object_all_brands_and_slays_are_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!brands_are_equal(obj->brands, obj->known->brands)) return false;
    if (!slays_are_equal(obj->slays, obj->known->slays)) return false;

    return true;
}


/*
 * Is the player aware of all of an object's miscellaneous properties?
 *
 * obj is the object
 */
static bool object_all_miscellaneous_are_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!obj->known->artifact) return false;
    if (!obj->known->ego) return false;
    if (!obj->known->pval) return false;
    if (!obj->known->dd) return false;
    if (!obj->known->ds) return false;
    if (!obj->known->ac) return false;
    if (!obj->known->to_a) return false;
    if (!obj->known->to_h) return false;
    if (!obj->known->to_d) return false;
    if (!obj->known->effect) return false;

    return true;
}


/*
 * Is the player aware of all of an object's properties?
 *
 * obj is the object
 */
static bool object_all_but_flavor_is_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!object_all_flags_are_known(obj, aware)) return false;
    if (!object_all_modifiers_are_known(obj, aware)) return false;
    if (!object_all_elements_are_known(obj, aware)) return false;
    if (!object_all_brands_and_slays_are_known(obj, aware)) return false;
    if (!object_all_miscellaneous_are_known(obj, aware)) return false;

    return true;
}


/*
 * Returns whether an object should be treated as fully known (e.g. ID'd)
 */
bool object_is_known(struct player *p, const struct object *obj)
{
    bool aware;

    /* Pretend known */
    if (!p) return true;

    aware = object_flavor_is_aware(p, obj);

    if (easy_know(obj, aware)) return true;
    if (!aware) return false;
    return object_all_but_flavor_is_known(obj, aware);
}


/*
 * Returns whether the object is known to be an artifact
 */
bool object_is_known_artifact(const struct object *obj)
{
    return (obj->artifact && obj->known->artifact);
}


/*
 * Returns whether the object is known to not be an artifact
 */
bool object_is_known_not_artifact(const struct object *obj)
{
    return (!obj->artifact && obj->known->artifact);
}


/*
 * Returns whether the object has been worn/wielded
 */
bool object_was_worn(const struct object *obj)
{
    return ((obj->known->notice & OBJ_NOTICE_WORN)? true: false);
}


/*
 * Returns whether the player is aware of the object's flavour
 */
bool object_flavor_is_aware(struct player *p, const struct object *obj)
{
    /* Pretend aware */
    if (!p) return true;

    return (p->obj_aware[obj->kind->kidx] || obj->bypass_aware);
}


/*
 * Returns whether the player has tried to use other objects of the same kind
 */
bool object_flavor_was_tried(struct player *p, const struct object *obj)
{
    /* Pretend tried */
    if (!p) return true;

    return p->obj_tried[obj->kind->kidx];
}


/*
 * Returns whether the player is aware of the object's effect when used
 */
bool object_effect_is_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware) || obj->known->effect)
        return true;

    return false;
}


/*
 * Returns whether any ego or artifact name is available to the player
 */
bool object_name_is_visible(const struct object *obj)
{
    bool ego = obj->ego && obj->known->ego;
    bool art = obj->artifact && obj->known->artifact;

    return (ego || art);
}


/*
 * Returns whether the object's ego knowledge is known
 */
bool object_ego_is_visible(const struct object *obj)
{
    if (obj->ego && obj->known->ego)
        return true;

    return false;
}


/*
 * Returns whether the object's attack plusses are known
 */
bool object_attack_plusses_are_visible(const struct object *obj)
{
    /* Bonuses have been revealed */
    if (obj->known->to_h && obj->known->to_d) return true;

    return false;
}


/*
 * Returns whether the object's defence bonuses are known
 */
bool object_defence_plusses_are_visible(const struct object *obj)
{
    /* Bonuses have been revealed */
    if (obj->known->to_a) return true;

    return false;
}


/*
 * Returns whether the player knows whether an object has a given flag
 */
bool object_flag_is_known(struct player *p, const struct object *obj, int flag)
{
    if (easy_know(obj, object_flavor_is_aware(p, obj)) || of_has(obj->known->flags, flag))
        return true;

    return false;
}


/*
 * Returns whether the player knows the given element properties of an object
 */
bool object_element_is_known(const struct object *obj, int element, bool aware)
{
    if (element < 0 || element >= ELEM_MAX) return false;

    if (easy_know(obj, aware) || obj->known->el_info[element].res_level)
        return true;

    return false;
}


/*
 * Returns whether a specific modifier is known to the player
 */
bool object_this_mod_is_visible(const struct object *obj, int mod)
{
    if (obj->known->modifiers[mod])
        return true;

    return false;
}


/*
 * Object knowledge improvers
 * These add to the player's knowledge of an object, where necessary
 */


/*
 * Set some knowledge for items where the flavour is already known
 */
static void object_id_set_aware(struct object *obj)
{
    int i;

    /* Know pval and effect */
    obj->known->pval = 1;
    obj->known->effect = (struct effect *)1;

    /* Jewelry with fixed bonuses gets more info now */
    if (tval_is_jewelry(obj) && !obj->artifact)
    {
        if (!randcalc_varies(obj->kind->to_h)) obj->known->to_h = 1;
        if (!randcalc_varies(obj->kind->to_d)) obj->known->to_d = 1;
        if (!randcalc_varies(obj->kind->to_a)) obj->known->to_a = 1;
        for (i = 0; i < OBJ_MOD_MAX; i++)
        {
            /* Unknown "LIGHT" modifier is never displayed */
            if (i == OBJ_MOD_LIGHT) continue;

            if (!randcalc_varies(obj->kind->modifiers[i]))
                obj->known->modifiers[i] = 1;
        }
    }
}


/*
 * Sets the basic details on a known object
 */
void object_set_base_known(struct player *p, struct object *obj)
{
    int i;
    bool aware = object_flavor_is_aware(p, obj);

    obj->known = object_new();

    obj->known->kind = obj->kind;
    obj->known->tval = obj->tval;
    obj->known->sval = obj->sval;
    obj->known->number = obj->number;
    obj->known->dd = 1;
    obj->known->ds = 1;

    /* If the object has no pval or effect, we know that */
    if (!obj->pval) obj->known->pval = 1;
    if (!object_effect(obj)) obj->known->effect = (struct effect *)1;

    /* Unresistables have no hidden properties */
    for (i = ELEM_HIGH_MAX + 1; i < ELEM_MAX; i++)
    {
        /* PWMAngband: some items have resist TIME/MANA */
        if ((i != ELEM_TIME) && (i != ELEM_MANA)) obj->known->el_info[i].res_level = 1;
    }

    /* Aware flavours get info now, easy_know things get everything */
    if (aware) object_id_set_aware(obj);
    if (easy_know(obj, aware)) object_notice_everything(p, obj);
}


/*
 * Checks for additional knowledge implied by what the player already knows.
 *
 * obj is the object to check
 */
bool object_check_for_ident(struct player *p, struct object *obj)
{
    bool aware = object_flavor_is_aware(p, obj);

    /* Check things which need to be learned */
    if (!object_all_flags_are_known(obj, aware)) return false;
    if (!object_all_elements_are_known(obj, aware)) return false;
    if (!object_all_brands_and_slays_are_known(obj, aware)) return false;

    /*
     * If we know attack bonuses, and defence bonuses, and effect, then
     * we effectively know everything, so mark as such
     */
    if ((object_attack_plusses_are_visible(obj) ||
        (object_was_sensed(obj) && (obj->to_h == 0) && (obj->to_d == 0))) &&
        (object_defence_plusses_are_visible(obj) ||
        (object_was_sensed(obj) && (obj->to_a == 0))) &&
        (object_effect_is_known(obj, aware) || !object_effect(obj)))
    {
        /*
         * In addition to knowing the pval flags, it is necessary to know
         * the modifiers to know everything
         */
        if (object_all_modifiers_are_known(obj, aware))
        {
            object_notice_everything(p, obj);
            return true;
        }
    }

    /* We still know all the flags, so if it's worn we know it's an ego */
    if (obj->ego && object_was_worn(obj)) object_notice_ego(p, obj);

    return false;
}


/*
 * Mark an object's flavour as one the player is aware of.
 *
 * obj is the object whose flavour should be marked as aware
 */
static void object_flavor_aware_aux(struct player *p, struct object *obj, bool send)
{
    int y, x;
    struct chunk *c;

    /* Pretend aware */
    if (!p) return;
    if (p->obj_aware[obj->kind->kidx]) return;

    /* Fully aware of the effects */
    p->obj_aware[obj->kind->kidx] = true;
    if (send) Send_aware(p, obj->kind->kidx);

    /* A bunch of things are now known */
    object_id_set_aware(obj);

    /* Update flags */
    p->upkeep->redraw |= PR_EQUIP;
    p->upkeep->notice |= PN_IGNORE;

    /* Quit if no dungeon yet */
    c = chunk_get(p->depth);
    if (!c) return;

    /* Some objects change tile on awareness, so update display for all floor objects of this kind */
    for (y = 1; y < c->height; y++)
    {
        for (x = 1; x < c->width; x++)
        {
            bool light = false;
            const struct object *floor_obj;

            for (floor_obj = square_object(c, y, x); floor_obj; floor_obj = floor_obj->next)
            {
                if (floor_obj->kind == obj->kind)
                {
                    light = true;
                    break;
                }
            }
            if (light) square_light_spot_aux(p, c, y, x);
        }
    }
}


void object_flavor_aware(struct player *p, struct object *obj)
{
    object_flavor_aware_aux(p, obj, true);
}


/*
 * Mark an object's flavour as tried.
 *
 * obj is the object whose flavour should be marked
 */
void object_flavor_tried(struct player *p, struct object *obj)
{
    my_assert(obj);

    p->obj_tried[obj->kind->kidx] = true;
}


/*
 * Make the player aware of all of an object's flags.
 *
 * obj is the object to mark
 */
static void object_know_all_flags(struct object *obj)
{
    of_setall(obj->known->flags);
}


/*
 * Make the player aware of all of an object's modifiers.
 *
 * obj is the object to mark
 */
static void object_know_all_modifiers(struct object *obj)
{
    size_t i;

    for (i = 0; i < OBJ_MOD_MAX; i++)
        obj->known->modifiers[i] = 1;
}


/*
 * Make the player aware of all of an object's elemental properties.
 *
 * obj is the object to mark
 */
static void object_know_all_elements(struct object *obj)
{
    size_t i;

    for (i = 0; i < ELEM_MAX; i++)
        obj->known->el_info[i].res_level = 1;
}


/*
 * Make the player aware of all of an object's brands and slays.
 *
 * obj is the object to mark
 */
void object_know_brands_and_slays(struct object *obj)
{
    /* Wipe all previous known and know everything */
    free_brand(obj->known->brands);
    obj->known->brands = NULL;
    copy_brand(&obj->known->brands, obj->brands);
    free_slay(obj->known->slays);
    obj->known->slays = NULL;
    copy_slay(&obj->known->slays, obj->slays);
}


/*
 * Make the player aware of all of an object's miscellaneous properties.
 *
 * obj is the object to mark
 */
static void object_know_all_miscellaneous(struct object *obj)
{
    obj->known->artifact = (struct artifact *)1;
    obj->known->ego = (struct ego_item *)1;
    obj->known->notice &= ~OBJ_NOTICE_WORN;
    obj->known->pval = 1;
    obj->known->dd = 1;
    obj->known->ds = 1;
    obj->known->ac = 1;
    obj->known->to_a = 1;
    obj->known->to_h = 1;
    obj->known->to_d = 1;
    obj->known->effect = (struct effect *)1;
}


/*
 * Make the player aware of all of an object' properties except flavor.
 *
 * obj is the object to mark
 */
static void object_know_all_but_flavor(struct object *obj)
{
    /* Know all flags there are to be known */
    object_know_all_flags(obj);

    /* Know all modifiers */
    object_know_all_modifiers(obj);

    /* Know all elemental properties */
    object_know_all_elements(obj);

    /* Know all brands and slays */
    object_know_brands_and_slays(obj);

    /* Know everything else */
    object_know_all_miscellaneous(obj);
}


/*
 * Mark an object as fully known, a.k.a identified.
 *
 * obj is the object to mark as identified
 */
void object_notice_everything_aux(struct player *p, struct object *obj, bool bypass_aware,
    bool send)
{
    /* Mark as known */
    if (bypass_aware) obj->bypass_aware = true;
    else object_flavor_aware_aux(p, obj, send);

    /* Know everything else */
    obj->known->notice |= OBJ_NOTICE_SENSED;
    object_know_all_but_flavor(obj);
}


void object_notice_everything(struct player *p, struct object *obj)
{
    object_notice_everything_aux(p, obj, false, true);
}


/*
 * Notice the ego on an ego item.
 */
void object_notice_ego(struct player *p, struct object *obj)
{
    bitflag learned_flags[OF_SIZE];
    bitflag xtra_flags[OF_SIZE];
    size_t i;

    if (!obj->ego) return;

    /* Learn ego flags */
    of_union(obj->known->flags, obj->ego->flags);

    /* Learn ego element properties */
    for (i = 0; i < ELEM_MAX; i++)
    {
        if (obj->ego->el_info[i].res_level != 0)
            obj->known->el_info[i].res_level = 1;
    }

    /* Learn all flags except random abilities */
    of_setall(learned_flags);

    /* Learn all brands and slays */
    object_know_brands_and_slays(obj);

    of_wipe(xtra_flags);

    /* Don't learn random ego extras */
    if (kf_has(obj->ego->kind_flags, KF_RAND_SUSTAIN))
    {
        create_mask(xtra_flags, false, OFT_SUST, OFT_MAX);
        of_diff(learned_flags, xtra_flags);
    }
    if (kf_has(obj->ego->kind_flags, KF_RAND_POWER))
    {
        create_mask(xtra_flags, false, OFT_MISC, OFT_PROT, OFT_MAX);
        of_diff(learned_flags, xtra_flags);
        create_mask(xtra_flags, false, OFT_ESP, OFT_MAX);
        of_diff(learned_flags, xtra_flags);
    }
    if (kf_has(obj->ego->kind_flags, KF_RAND_ESP))
    {
        create_mask(xtra_flags, false, OFT_ESP, OFT_MAX);
        of_diff(learned_flags, xtra_flags);
    }

    of_union(obj->known->flags, learned_flags);

    /* Learn all element properties except random high resists */
    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Don't learn random ego high resists */
        if ((i >= ELEM_HIGH_MIN) && (i <= ELEM_HIGH_MAX) &&
            kf_has(obj->ego->kind_flags, KF_RAND_HI_RES))
        {
            continue;
        }

        /* Learn all element properties */
        obj->known->el_info[i].res_level = 1;
    }

    /* If you know the ego, you know which it is of excellent or splendid */
    if (!obj->known->ego)
    {
        obj->known->ego = (struct ego_item *)1;
        object_notice_sensing(p, obj);
        object_check_for_ident(p, obj);
    }
}


/*
 * Notice the "effect" from activating an object.
 *
 * obj is the object to become aware of
 */
void object_notice_effect(struct player *p, struct object *obj)
{
    if (!obj->known->effect)
    {
        obj->known->effect = (struct effect *)1;
        object_check_for_ident(p, obj);
    }

    /* Noticing an effect gains awareness */
    if (!object_flavor_is_aware(p, obj)) object_flavor_aware(p, obj);
}


/*
 * Notice things which happen on defending.
 */
static void object_notice_defence_plusses(struct player *p, struct object *obj)
{
    if (!obj) return;
    if (object_defence_plusses_are_visible(obj)) return;

    if (!obj->known->to_a)
    {
        obj->known->to_a = 1;
        object_check_for_ident(p, obj);
    }

    if (obj->ac || obj->to_a)
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);
        msgt(p, MSG_PSEUDOID, "You know more about the %s you are wearing.", o_name);
    }

    p->upkeep->update |= (PU_BONUS);
    p->upkeep->redraw |= (PR_EQUIP | PR_ARMOR);
}


/*
 * Notice things which happen on attacking.
 */
void object_notice_attack_plusses(struct player *p, struct object *obj)
{
    char o_name[NORMAL_WID];
    bool aware;

    if (!obj) return;

    aware = object_flavor_is_aware(p, obj);
    if (object_attack_plusses_are_visible(obj) && aware) return;

    if (!obj->known->to_h)
    {
        obj->known->to_h = 1;
        object_check_for_ident(p, obj);
    }
    if (!obj->known->to_d)
    {
        obj->known->to_d = 1;
        object_check_for_ident(p, obj);
    }
    if (!(obj->known->dd && obj->known->ds))
    {
        obj->known->dd = 1;
        obj->known->ds = 1;
        object_check_for_ident(p, obj);
    }

    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

    if (equipped_item_by_slot_name(p, "weapon") == obj)
        msgt(p, MSG_PSEUDOID, "You know more about the %s you are using.", o_name);
    else if ((obj->to_d || obj->to_h) && !(tval_is_body_armor(obj) && (obj->to_h < 0)))
        msgt(p, MSG_PSEUDOID, "Your %s glow%s.", o_name, ((obj->number > 1) ? "" : "s"));

    if (object_all_but_flavor_is_known(obj, aware)) object_flavor_aware(p, obj);

    p->upkeep->update |= (PU_BONUS);
    p->upkeep->redraw |= (PR_EQUIP | PR_PLUSSES);
}


/*
 * Notice elemental resistance properties for an element on an object
 */
bool object_notice_element(struct player *p, struct object *obj, int element)
{
    if (element < 0 || element >= ELEM_MAX) return false;

    /* Already known */
    if (obj->known->el_info[element].res_level)
        return false;

    /* Learn about this element */
    obj->known->el_info[element].res_level = 1;

    object_check_for_ident(p, obj);

    return true;
}


/*
 * Notice a single flag - returns true if anything new was learned
 */
bool object_notice_flag(struct player *p, struct object *obj, int flag)
{
    if (of_has(obj->known->flags, flag)) return false;

    of_on(obj->known->flags, flag);
    object_check_for_ident(p, obj);

    return true;
}


/*
 * Notice a set of flags - returns true if anything new was learned
 */
bool object_notice_flags(struct player *p, struct object *obj, bitflag flags[OF_SIZE])
{
    if (of_is_subset(obj->known->flags, flags)) return false;

    of_union(obj->known->flags, flags);
    object_check_for_ident(p, obj);

    return true;
}


/*
 * Notice curses on an object.
 *
 * obj is the object to notice curses on
 */
bool object_notice_curses(struct player *p, struct object *obj)
{
    bitflag f[OF_SIZE], f2[OF_SIZE];

    object_flags(obj, f);

    /* Gather whatever curse flags there are to know */
    create_mask(f2, false, OFT_CURSE, OFT_MAX);

    /* Remove everything except the curse flags */
    of_inter(f, f2);

    /* Give knowledge of which curses are present */
    object_notice_flags(p, obj, f);
    object_check_for_ident(p, obj);

    if (p) p->upkeep->notice |= PN_IGNORE;

    return !of_is_empty(f);
}


/*
 * Notice object properties that become obvious on wielding or wearing.
 */
void object_notice_on_wield(struct player *p, struct object *obj)
{
    bitflag f[OF_SIZE], f2[OF_SIZE], obvious_mask[OF_SIZE];
    bool obvious = false;
    int i;

    /* Mark artifact as found */
    set_artifact_info(p, obj, ARTS_FOUND);

    /* Always set the worn flag, and know armour class */
    obj->known->notice |= OBJ_NOTICE_WORN;
    obj->known->ac = 1;

    /* EASY_KNOW is now known */
    if (easy_know(obj, object_flavor_is_aware(p, obj)))
    {
        object_notice_everything(p, obj);
        return;
    }

    /* Only deal with un-ID'd items */
    if (object_is_known(p, obj)) return;

    /* Worn means tried (for flavored wearables) */
    object_flavor_tried(p, obj);

    /* Save time of wield for later */
    ht_copy(&p->object_last_wield, &turn);

    /* Get the obvious object flags */
    create_mask(obvious_mask, true, OFID_WIELD, OFT_MAX);

    /* Special case FA, needed for mages wielding gloves */
    if (player_has(p, PF_CUMBER_GLOVE) && (obj->tval == TV_GLOVES) &&
        (obj->modifiers[OBJ_MOD_DEX] <= 0) && !kf_has(obj->kind->kind_flags, KF_SPELLS_OK))
    {
        of_on(obvious_mask, OF_FREE_ACT);
    }

    /* Extract the flags */
    object_flags(obj, f);

    /* Find obvious flags - curses left for special message later */
    create_mask(f2, false, OFT_CURSE, OFT_MAX);
    of_diff(obvious_mask, f2);

    /* Learn about obvious flags */
    if (of_is_inter(f, obvious_mask)) obvious = true;
    of_union(obj->known->flags, obvious_mask);

    /* Notice all modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (obj->modifiers[i]) obvious = true;
        obj->known->modifiers[i] = 1;
    }

    /* Notice any brands */
    object_notice_brands(p, obj, NULL);

    /* Automatically sense artifacts upon wield */
    object_sense_artifact(p, obj);

    /* Special cases for jewelry */
    if (tval_is_jewelry(obj))
    {
        /* Learn the flavor of jewelry with obvious flags */
        if (obvious)
            object_flavor_aware(p, obj);

        /* Learn all flags and elements on any aware non-artifact jewelry */
        if (object_flavor_is_aware(p, obj) && !obj->artifact)
        {
            object_know_all_flags(obj);
            object_know_all_elements(obj);
        }
    }

    object_check_for_ident(p, obj);

    if (!obvious) return;

    /* Special messages for individual properties */
    if (obj->modifiers[OBJ_MOD_STR] > 0)
        msg(p, "You feel stronger!");
    else if (obj->modifiers[OBJ_MOD_STR] < 0)
        msg(p, "You feel weaker!");
    if (obj->modifiers[OBJ_MOD_INT] > 0)
        msg(p, "You feel smarter!");
    else if (obj->modifiers[OBJ_MOD_INT] < 0)
        msg(p, "You feel more stupid!");
    if (obj->modifiers[OBJ_MOD_WIS] > 0)
        msg(p, "You feel wiser!");
    else if (obj->modifiers[OBJ_MOD_WIS] < 0)
        msg(p, "You feel more naive!");
    if (obj->modifiers[OBJ_MOD_DEX] > 0)
        msg(p, "You feel more dextrous!");
    else if (obj->modifiers[OBJ_MOD_DEX] < 0)
        msg(p, "You feel clumsier!");
    if (obj->modifiers[OBJ_MOD_CON] > 0)
        msg(p, "You feel healthier!");
    else if (obj->modifiers[OBJ_MOD_CON] < 0)
        msg(p, "You feel sicklier!");
    if (obj->modifiers[OBJ_MOD_STEALTH] > 0)
        msg(p, "You feel stealthier.");
    else if (obj->modifiers[OBJ_MOD_STEALTH] < 0)
        msg(p, "You feel noisier.");
    if (obj->modifiers[OBJ_MOD_SPEED] > 0)
        msg(p, "You feel strangely quick.");
    else if (obj->modifiers[OBJ_MOD_SPEED] < 0)
        msg(p, "You feel strangely sluggish.");
    if (obj->modifiers[OBJ_MOD_BLOWS] > 0)
        msg(p, "Your hands tingle.");
    else if (obj->modifiers[OBJ_MOD_BLOWS] < 0)
        msg(p, "Your hands ache.");
    if (obj->modifiers[OBJ_MOD_SHOTS] > 0)
        msg(p, "Your missiles tingle in your hands.");
    else if (obj->modifiers[OBJ_MOD_SHOTS] < 0)
        msg(p, "Your missiles ache in your hands.");
    if (obj->modifiers[OBJ_MOD_INFRA] > 0)
        msg(p, "Your eyes tingle.");
    else if (obj->modifiers[OBJ_MOD_INFRA] < 0)
        msg(p, "Your eyes ache.");
    if (obj->modifiers[OBJ_MOD_MANA] > 0)
        msg(p, "You feel more attuned to magic.");
    else if (obj->modifiers[OBJ_MOD_MANA] < 0)
        msg(p, "You feel less attuned to magic.");
    if (obj->modifiers[OBJ_MOD_LIGHT])
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);
        msg(p, "Your %s glows!", o_name);
    }
    create_mask(f2, false, OFT_ESP, OFT_MAX);
    if (of_is_inter(f, f2))
        msg(p, "Your mind feels strangely sharper!");
    if (of_has(f, OF_FREE_ACT) && of_has(obvious_mask, OF_FREE_ACT))
        msg(p, "You feel mobile!");
    if (of_has(f, OF_KNOWLEDGE))
        msg(p, "You feel more knowledgeable!");

    /* Remember the flags */
    object_notice_sensing(p, obj);
}


/*
 * Notice object properties that become obvious on use, mark it as
 * aware and reward the player with some experience.
 */
void object_notice_on_use(struct player *p, struct object *obj)
{
    /* Object level */
    int lev = obj->kind->level;

    object_flavor_aware(p, obj);
    object_notice_effect(p, obj);
    if (tval_is_rod(obj)) object_notice_everything(p, obj);
    player_exp_gain(p, (lev + (p->lev >> 1)) / p->lev);

    p->upkeep->notice |= PN_IGNORE;
}


/*
 * Equipment knowledge improvers
 * These add to the player's knowledge of objects in their equipment
 */


/*
 * Notice things which happen on defending.
 */
void equip_notice_on_defend(struct player *p)
{
    int i;

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (obj) object_notice_defence_plusses(p, obj);
    }
}


/*
 * Notice things about an object that would be noticed in time.
 */
static void equip_notice_after_time(struct player *p)
{
    int i;
    int flag;
    struct object *obj;
    char o_name[NORMAL_WID];
    bitflag f[OF_SIZE], timed_mask[OF_SIZE];
    bool redraw = false;

    create_mask(timed_mask, true, OFID_TIMED, OFT_MAX);

    /* Check every item the player is wearing */
    for (i = 0; i < p->body.count; i++)
    {
        obj = slot_object(p, i);
        if (!obj || object_is_known(p, obj)) continue;

        /* Check for timed notice flags */
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);
        object_flags(obj, f);
        of_inter(f, timed_mask);

        for (flag = of_next(f, FLAG_START); flag != FLAG_END; flag = of_next(f, flag + 1))
        {
            if (!of_has(obj->known->flags, flag))
            {
                /* Message */
                flag_message(p, flag, o_name);

                /* Notice the flag */
                object_notice_flag(p, obj, flag);
                redraw = true;

                /* Jewelry with a noticeable flag is obvious */
                if (tval_is_jewelry(obj) && (!object_effect(obj) ||
                    object_effect_is_known(obj, object_flavor_is_aware(p, obj))))
                {
                    object_flavor_aware(p, obj);
                    object_check_for_ident(p, obj);
                }
            }
        }
    }

    /* Notice new info */
    if (redraw) p->upkeep->redraw |= (PR_EQUIP);
}


/*
 * Notice a given special flag on wielded items.
 *
 * flag is the flag to notice
 */
void equip_notice_flag(struct player *p, int flag)
{
    int i;

    /* Sanity check */
    if (!flag) return;

    /* All wielded items eligible */
    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (!obj) continue;

        if (of_has(obj->flags, flag) && !of_has(obj->known->flags, flag))
        {
            char o_name[NORMAL_WID];

            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

            /* Notice the flag */
            object_notice_flag(p, obj, flag);

            /* Jewelry with a noticeable flag is obvious */
            if (tval_is_jewelry(obj))
            {
                object_flavor_aware(p, obj);
                object_check_for_ident(p, obj);
            }

            /* Message */
            flag_message(p, flag, o_name);
        }
        else
        {
            /* Notice that flag is absent */
            object_notice_flag(p, obj, flag);
        }
    }
}


/*
 * Notice the elemental resistance properties on wielded items.
 *
 * element is the element to notice
 */
void equip_notice_element(struct player *p, int element)
{
    int i;

    if (element < 0 || element >= ELEM_MAX) return;

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (!obj) continue;

        /* Already known */
        if (obj->known->el_info[element].res_level) continue;

        /* Notice the element properties */
        object_notice_element(p, obj, element);

        /* Comment if it actually does something */
        if (obj->el_info[element].res_level != 0)
        {
            char o_name[NORMAL_WID];

            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

            msg(p, "Your %s glows.", o_name);

            /* Jewelry with a noticeable element is obvious */
            if (tval_is_jewelry(obj))
            {
                object_flavor_aware(p, obj);
                object_check_for_ident(p, obj);
            }
        }
    }
}


/*
 * Notice to-hit bonus on attacking.
 *
 * Used e.g. for ranged attacks where the item's to_d is not involved.
 * Does not apply to weapon, missile or bow which should be done separately
 */
void equip_notice_to_hit_on_attack(struct player *p)
{
    int i;

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (i == slot_by_name(p, "weapon")) continue;
        if (i == slot_by_name(p, "shooting")) continue;

        if (obj && obj->to_h)
            object_notice_attack_plusses(p, obj);
    }
}


/*
 * Notice things which happen on attacking.
 *
 * Does not apply to weapon, missile or bow which should be done separately
 */
void equip_notice_on_attack(struct player *p)
{
    int i;

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (i == slot_by_name(p, "weapon")) continue;
        if (i == slot_by_name(p, "shooting")) continue;

        if (obj)
            object_notice_attack_plusses(p, obj);
    }
}


/*
 * Other functions
 */


/*
 * Returns whether the object's high resist knowledge is known
 */
bool object_high_resist_is_possible(const struct object *obj)
{
    size_t i;

    /* Look at all the high resists */
    for (i = ELEM_HIGH_MIN; i <= ELEM_HIGH_MAX; i++)
    {
        /* Object has the resist */
        /* Element properties unknown */
        if ((obj->el_info[i].res_level > 0) && !obj->known->el_info[i].res_level)
            return true;
    }

    /* PWMAngband: look at the extra high resists (TIME + MANA) */
    if ((obj->el_info[ELEM_TIME].res_level > 0) && !obj->known->el_info[ELEM_TIME].res_level)
        return true;
    if ((obj->el_info[ELEM_MANA].res_level > 0) && !obj->known->el_info[ELEM_MANA].res_level)
        return true;

    /* No possibilities */
    return false;
}


/*
 * Returns whether the object has been sensed with pseudo-ID
 */
bool object_was_sensed(const struct object *obj)
{
    return ((obj->known->notice & OBJ_NOTICE_SENSED)? true: false);
}


/*
 * Mark an object as sensed
 */
void object_notice_sensing(struct player *p, struct object *obj)
{
    if (object_was_sensed(obj)) return;

    if (obj->artifact)
        obj->known->artifact = (struct artifact *)1;

    object_notice_curses(p, obj);
    obj->known->notice |= OBJ_NOTICE_SENSED;
    object_check_for_ident(p, obj);

    /* For unflavoured objects we can rule out some things */
    if (!obj->artifact && !obj->ego && !obj->kind->flavor)
    {
        object_know_all_flags(obj);
        object_know_all_elements(obj);
        object_know_brands_and_slays(obj);
    }
}


/*
 * Sense artifacts
 */
void object_sense_artifact(struct player *p, struct object *obj)
{
    obj->known->artifact = (struct artifact *)1;
    if (obj->artifact) object_notice_sensing(p, obj);
}


/*
 * Given an object, return a short identifier which gives some idea of what
 * the item is.
 */
obj_pseudo_t object_pseudo(struct player *p, const struct object *obj, bool aware, bool known)
{
    int i;
    bitflag flags[OF_SIZE], f2[OF_SIZE];
    s16b to_h = (obj->to_h - randcalc(obj->kind->to_h, 0, MINIMISE));
    s16b to_d = (obj->to_d - randcalc(obj->kind->to_d, 0, MINIMISE));
    s16b to_a = (obj->to_a - randcalc(obj->kind->to_a, 0, MINIMISE));

    /*
     * Get the known and obvious flags on the object,
     * not including curses or properties of the kind
     */
    object_flags_known(obj, flags, aware);
    create_mask(f2, true, OFID_WIELD, OFT_MAX);

    /* FA on gloves is obvious to mage casters */
    if (p && player_has(p, PF_CUMBER_GLOVE) && (obj->tval == TV_GLOVES) &&
        (obj->modifiers[OBJ_MOD_DEX] <= 0) && !kf_has(obj->kind->kind_flags, KF_SPELLS_OK))
    {
        of_on(f2, OF_FREE_ACT);
    }

    /* Now we remove the non-obvious known flags */
    of_inter(flags, f2);

    /* Now we remove the cursed flags and the kind flags */
    create_mask(f2, false, OFT_CURSE, OFT_MAX);
    of_diff(flags, f2);
    of_diff(flags, obj->kind->flags);

    if ((object_was_sensed(obj) || object_was_worn(obj) || known) && obj->artifact)
        return INSCRIP_SPECIAL;

    /* Jewelry does not pseudo */
    if (tval_is_jewelry(obj)) return INSCRIP_NULL;

    /* Check modifiers for splendid - anything different from kind base modifier is splendid */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if ((obj->modifiers[i] != obj->kind->modifiers[i].base) && object_this_mod_is_visible(obj, i))
            return INSCRIP_SPLENDID;
    }

    /* Any remaining obvious-on-wield flags also mean splendid */
    if (!of_is_empty(flags)) return INSCRIP_SPLENDID;

    /* Known brands are also splendid */
    if (obj->known->brands) return INSCRIP_SPLENDID;

    if (!known && !object_was_sensed(obj)) return INSCRIP_NULL;

    /* Uncursed bad egos are not excellent */
    if (obj->ego)
    {
        if (of_is_inter(obj->ego->flags, f2))
            return INSCRIP_STRANGE;
        return INSCRIP_EXCELLENT;
    }

    if ((to_a == 0) && (to_h == 0) && (to_d == 0))
        return INSCRIP_AVERAGE;

    if ((to_a >= 0) && (to_h >= 0) && (to_d >= 0))
        return INSCRIP_MAGICAL_GOOD;

    if ((to_a <= 0) && (to_h <= 0) && (to_d <= 0))
        return INSCRIP_MAGICAL_BAD;

    return INSCRIP_STRANGE;
}


/*
 * Identify an item.
 */
void do_ident_item(struct player *p, struct object *obj)
{
    char o_name[NORMAL_WID];
    u32b msg_type = 0;
    bool bad = cursed_p(obj->flags) || worthless_p(obj);

    /* Identify it */
    object_notice_everything(p, obj);

    /* Update the gear */
    calc_inventory(p);

    /* Set ignore flag */
    p->upkeep->notice |= PN_IGNORE;

    /* Recalculate bonuses */
    p->upkeep->update |= (PU_BONUS);

    /* Redraw */
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_PLUSSES);

    /* Description */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    /* Determine the message type. */
    if (bad) msg_type = MSG_IDENT_BAD;
    else if (obj->artifact) msg_type = MSG_IDENT_ART;
    else if (obj->ego) msg_type = MSG_IDENT_EGO;
    else msg_type = MSG_GENERIC;

    /* Describe */
    if (object_is_equipped(p->body, obj))
    {
        int slot = equipped_item_slot(p->body, obj);

        /* Format and capitalise */
        char *msg = format("%s: %s (%c).", equip_describe(p, slot), o_name, I2A(slot));

        my_strcap(msg);
        msgt(p, msg_type, msg);
    }
    else if (object_is_carried(p, obj))
        msgt(p, msg_type, "In your pack: %s (%c).", o_name, gear_to_label(p, obj));
    else
        msgt(p, msg_type, "On the ground: %s.", o_name);
}


/*
 * Sense the inventory
 */
void sense_inventory(struct player *p)
{
    struct object *obj;
    char o_name[NORMAL_WID];
    unsigned int rate;

    /* No ID when confused */
    if (p->timed[TMD_CONFUSED]) return;

    /* Notice some things after a while */
    if (ht_diff(&turn, &p->object_last_wield) >= 3000)
    {
        equip_notice_after_time(p);
        ht_reset(&p->object_last_wield);
    }

    /* Get improvement rate */
    if (player_has(p, PF_PSEUDO_ID_IMPROV))
        rate = p->clazz->sense_base / (p->lev * p->lev + p->clazz->sense_div);
    else
        rate = p->clazz->sense_base / (p->lev + p->clazz->sense_div);

    /* Check if player may sense anything this time */
    if ((p->lev < 20) && !one_in_(rate)) return;

    /* Give each object one opportunity to have a chance at being sensed. */
    for (obj = p->gear; obj; obj = obj->next)
    {
        const char *text = NULL;
        obj_pseudo_t feel;
        bool cursed, worthless;
        bool equipped = object_is_equipped(p->body, obj);

        /* Valid tval codes only */
        if (!is_sense_machine(obj)) continue;

        /* It is known, no information needed */
        if (object_is_known(p, obj)) continue;

        /* It has already been sensed, do not sense it again */
        if (object_was_sensed(obj))
        {
            /* Small chance of wielded, sensed items getting complete ID */
            if (!obj->artifact && equipped && one_in_(1000))
                do_ident_item(p, obj);

            continue;
        }

        /* Occasional failure on inventory items */
        if (!equipped && one_in_(5)) continue;

        /* Sense the object */
        object_notice_sensing(p, obj);
        cursed = object_notice_curses(p, obj);

        /* Hack -- worthless objects */
        worthless = worthless_p(obj);

        /* Get the feeling */
        feel = object_pseudo(p, obj, object_flavor_is_aware(p, obj), object_is_known(p, obj));

        /* Stop everything */
        disturb(p, 0);

        if (cursed) text = "cursed";
        else if (worthless) text = "worthless";
        else text = inscrip_text[feel];

        /* Get an object description */
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

        /* Average pseudo-ID means full ID */
        if (feel == INSCRIP_AVERAGE)
        {
            object_notice_everything(p, obj);

            if (worthless) text = "worthless";
            else text = "average";
        }

        /* Message (equipment) */
        if (equipped)
        {
            msgt(p, MSG_PSEUDOID, "You feel the %s (%c) you are %s %s %s...", o_name,
                gear_to_label(p, obj),
                equip_describe(p, equipped_item_slot(p->body, obj)),
                VERB_AGREEMENT(obj->number, "is", "are"), text);
        }

        /* Message (inventory) */
        else
        {
            msgt(p, MSG_PSEUDOID, "You feel the %s (%c) in your pack %s %s...", o_name,
                gear_to_label(p, obj), VERB_AGREEMENT(obj->number, "is", "are"), text);
        }

        /* Set ignore flag as appropriate */
        if (!equipped) p->upkeep->notice |= PN_IGNORE;

        /* Combine the pack (later) */
        p->upkeep->notice |= (PN_COMBINE);

        /* Update the gear */
        p->upkeep->update |= (PU_INVEN);

        /* Redraw */
        p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
    }
}
