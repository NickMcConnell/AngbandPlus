/*
 * File: identify.c
 * Purpose: Object identification and knowledge routines
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2009 Brian Bull
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
#include "slays.h"
#include "../s-spells.h"


/*** Knowledge accessor functions ***/


/*
 * Returns whether an object counts as "known" due to EASY_KNOW status
 */
bool easy_know(const object_type *o_ptr, bool aware)
{
    return (aware && of_has(o_ptr->kind->flags, OF_EASY_KNOW));
}


/*
 * Returns whether an object should be treated as fully known (e.g. ID'd)
 */
bool object_is_known(struct player *p, const object_type *o_ptr)
{
    bool aware;

    /* Pretend known */
    if (!p) return TRUE;

    aware = object_flavor_is_aware(p, o_ptr);
    return (((o_ptr->ident & IDENT_KNOWN) && aware) || easy_know(o_ptr, aware));
}


/*
 * Returns whether the object is known to be an artifact
 */
bool object_is_known_artifact(const object_type *o_ptr)
{
    return (o_ptr->ident & IDENT_INDESTRUCT) ||
        (o_ptr->artifact && object_was_sensed(o_ptr));
}


/*
 * Returns whether the object is known to be blessed
 */
bool object_is_known_blessed(struct player *p, const object_type *o_ptr)
{
    bitflag f[OF_SIZE];

    object_flags_known(o_ptr, f, object_flavor_is_aware(p, o_ptr));

    return of_has(f, OF_BLESSED);
}


/*
 * Returns whether the object is known to not be an artifact
 */
bool object_is_known_not_artifact(const object_type *o_ptr)
{
    return ((o_ptr->ident & IDENT_NOTART)? TRUE: FALSE);
}


/*
 * Returns whether the object has been worn/wielded
 */
bool object_was_worn(const object_type *o_ptr)
{
    return ((o_ptr->ident & IDENT_WORN)? TRUE: FALSE);
}


/*
 * Returns whether the object has been fired/thrown
 */
bool object_was_fired(const object_type *o_ptr)
{
    return ((o_ptr->ident & IDENT_FIRED)? TRUE: FALSE);
}


/*
 * Returns whether the object has been sensed with pseudo-ID
 */
bool object_was_sensed(const object_type *o_ptr)
{
    return ((o_ptr->ident & IDENT_SENSE)? TRUE: FALSE);
}


/*
 * Returns whether the player is aware of the object's flavour
 */
bool object_flavor_is_aware(struct player *p, const object_type *o_ptr)
{
    /* Pretend aware */
    if (!p) return TRUE;

    my_assert(o_ptr->kind);

    return ((p->obj_aware[o_ptr->kind->kidx]) || (o_ptr->ident & IDENT_AWARE));
}


/*
 * Returns whether the player has tried to use other objects of the same kind
 */
bool object_flavor_was_tried(struct player *p, const object_type *o_ptr)
{
    /* Pretend tried */
    if (!p) return TRUE;

    my_assert(o_ptr->kind);

    return p->obj_tried[o_ptr->kind->kidx];
}


/*
 * Returns whether the player is aware of the object's effect when used
 */
bool object_effect_is_known(struct player *p, const object_type *o_ptr)
{
    bool aware = object_flavor_is_aware(p, o_ptr);

    my_assert(o_ptr->kind);

    return (easy_know(o_ptr, aware) || (o_ptr->ident & IDENT_EFFECT) ||
        (aware && o_ptr->kind->effect));
}


/*
 * Returns whether any ego or artifact name is available to the player
 */
bool object_name_is_visible(const object_type *o_ptr)
{
    return ((o_ptr->ident & IDENT_NAME)? TRUE: FALSE);
}


/*
 * Returns whether the object's ego knowledge is known
 */
bool object_ego_is_visible(const object_type *o_ptr)
{
    if (!o_ptr->ego) return FALSE;

    return object_name_is_visible(o_ptr);
}


/*
 * Returns whether the object's attack plusses are known
 */
bool object_attack_plusses_are_visible(struct player *p, const object_type *o_ptr)
{
    /* Bonuses have been revealed */
    if (o_ptr->ident & IDENT_ATTACK) return TRUE;

    /* Aware jewelry with non-variable bonuses (except artifacts) */
    if (object_is_jewelry(o_ptr) && !o_ptr->artifact && object_flavor_is_aware(p, o_ptr) &&
        !randcalc_varies(o_ptr->kind->to_h) && !randcalc_varies(o_ptr->kind->to_d))
    {
        return TRUE;
    }

    return FALSE;
}


/*
 * Returns whether the object's defence bonuses are known
 */
bool object_defence_plusses_are_visible(struct player *p, const object_type *o_ptr)
{
    /* Bonuses have been revealed */
    if (o_ptr->ident & IDENT_DEFENCE) return TRUE;

    /* Aware jewelry with non-variable bonuses (except artifacts) */
    if (object_is_jewelry(o_ptr) && !o_ptr->artifact && object_flavor_is_aware(p, o_ptr) &&
        !randcalc_varies(o_ptr->kind->to_a))
    {
        return TRUE;
    }

    return FALSE;
}


/*
 * Returns whether the player knows whether an object has a given flag
 */
bool object_flag_is_known(struct player *p, const object_type *o_ptr, int flag)
{
    bool aware = object_flavor_is_aware(p, o_ptr);

    return (easy_know(o_ptr, aware) || of_has(o_ptr->known_flags, flag));
}


/*
 * Returns whether the object's high resist knowledge is known
 */
bool object_high_resist_is_possible(const object_type *o_ptr)
{
    bitflag flags[OF_SIZE], f2[OF_SIZE];

    /* Actual object flags */
    object_flags(o_ptr, flags);

    /* Add player's uncertainty */
    of_comp_union(flags, o_ptr->known_flags);

    /* Check for possible high resist */
    create_mask(f2, FALSE, OFT_HRES, OFT_XRES, OFT_MAX);
    if (of_is_inter(flags, f2)) return TRUE;
    return FALSE;
}


/*
 * Sets some IDENT_ flags on an object
 *
 * o_ptr is the object to check
 * flags are the ident flags to be added
 *
 * Returns whether o_ptr->ident changed
 */
static bool object_add_ident_flags(object_type *o_ptr, u32b flags)
{
    if ((o_ptr->ident & flags) != flags)
    {
        o_ptr->ident |= flags;
        return TRUE;
    }

    return FALSE;
}


/*
 * Checks for additional knowledge implied by what the player already knows.
 *
 * o_ptr is the object to check
 */
bool object_check_for_ident(struct player *p, object_type *o_ptr)
{
    bitflag flags[OF_SIZE], known_flags[OF_SIZE], f2[OF_SIZE];
    bool aware = object_flavor_is_aware(p, o_ptr);

    object_flags(o_ptr, flags);
    object_flags_known(o_ptr, known_flags, aware);

    /* Some flags are irrelevant or never learned or too hard to learn */
    create_mask(f2, FALSE, OFT_INT, OFT_IGNORE, OFT_HATES, OFT_MAX);

    of_diff(flags, f2);
    of_diff(known_flags, f2);

    if (!of_is_equal(flags, known_flags)) return FALSE;

    /*
     * If we know attack bonuses, and defence bonuses, and effect, then
     * we effectively know everything, so mark as such
     */
    if ((object_attack_plusses_are_visible(p, o_ptr) ||
        (object_was_sensed(o_ptr) && (o_ptr->to_h == 0) && (o_ptr->to_d == 0))) &&
        (object_defence_plusses_are_visible(p, o_ptr) ||
        (object_was_sensed(o_ptr) && (o_ptr->to_a == 0))) &&
        (object_effect_is_known(p, o_ptr) || !o_ptr->effect))
    {
        int i;

        /* In addition to knowing the pval flags, it is necessary to know the pvals to know everything */
        for (i = 0; i < o_ptr->num_pvals; i++)
        {
            if (!object_this_pval_is_visible(o_ptr, i, aware)) break;
        }
        if (i == o_ptr->num_pvals)
        {
            object_notice_everything(p, o_ptr, FALSE);
            return TRUE;
        }
    }

    /* We still know all the flags, so we still know if it's an ego */
    /* Require worn status so you don't learn "of accuracy"/"of slaying" before wield */
    if (o_ptr->ego && object_was_worn(o_ptr)) object_notice_ego(p, o_ptr);

    return FALSE;
}


/*
 * Mark an object's flavour as one the player is aware of.
 *
 * o_ptr is the object whose flavour should be marked as aware
 */
void object_flavor_aware(struct player *p, object_type *o_ptr)
{
    int i;

    /* Pretend aware */
    if (!p) return;
    if (p->obj_aware[o_ptr->kind->kidx]) return;

    /* Fully aware of the effects */
    p->obj_aware[o_ptr->kind->kidx] = TRUE;

    /* Update flags */
    p->redraw |= PR_EQUIP;
    p->notice |= PN_SQUELCH;

    /* Some objects can change their "tile" when becoming aware */
    for (i = 1; i < o_max; i++)
    {
        const object_type *floor_o_ptr = object_byid(i);

        /* Skip dead objects */
        if (!floor_o_ptr->kind) continue;

        /* Skip held objects */
        if (floor_o_ptr->held_m_idx) continue;

        /* Skip objects not on this depth */
        if (floor_o_ptr->depth != p->depth) continue;

        /* Update display for all floor objects of this kind */
        if (floor_o_ptr->kind == o_ptr->kind)
            cave_light_spot_aux(p, cave_get(floor_o_ptr->depth), floor_o_ptr->iy,
                floor_o_ptr->ix);
    }
}


/*
 * Mark an object's flavour as tried.
 *
 * o_ptr is the object whose flavour should be marked
 */
void object_flavor_tried(int Ind, object_type *o_ptr)
{
    my_assert(o_ptr);
    my_assert(o_ptr->kind);

    player_get(Ind)->obj_tried[o_ptr->kind->kidx] = TRUE;
}


/*
 * Make the player aware of all of an object's flags.
 *
 * o_ptr is the object to mark
 */
void object_know_all_flags(object_type *o_ptr)
{
    of_setall(o_ptr->known_flags);
}


#define IDENTS_SET_BY_IDENTIFY \
    (IDENT_KNOWN | IDENT_ATTACK | IDENT_DEFENCE | IDENT_SENSE | \
        IDENT_EFFECT | IDENT_WORN | IDENT_FIRED | IDENT_NAME)


/*
 * Mark an object as fully known, a.k.a identified.
 *
 * o_ptr is the object to mark as identified
 */
void object_notice_everything(struct player *p, object_type *o_ptr, bool bypass_aware)
{
    /* Clear "empty" */
    o_ptr->ident &= ~IDENT_EMPTY;

    /* Mark as known */
    if (bypass_aware) o_ptr->ident |= IDENT_AWARE;
    else object_flavor_aware(p, o_ptr);
    object_add_ident_flags(o_ptr, IDENTS_SET_BY_IDENTIFY);

    /* Know all flags there are to be known */
    object_know_all_flags(o_ptr);
}


/*
 * Notice that an object is indestructible
 */
void object_notice_indestructible(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    if (object_add_ident_flags(o_ptr, IDENT_INDESTRUCT))
        object_check_for_ident(p_ptr, o_ptr);
}


/*
 * Notice the ego on an ego item.
 */
void object_notice_ego(struct player *p, object_type *o_ptr)
{
    bitflag learned_flags[OF_SIZE];
    bitflag xtra_flags[OF_SIZE];

    if (!o_ptr->ego) return;

    /* Learn ego flags */
    of_union(o_ptr->known_flags, o_ptr->ego->flags);

    /* Learn all flags except random abilities */
    of_setall(learned_flags);

    of_wipe(xtra_flags);

    switch (o_ptr->ego->xtra)
    {
        case OBJECT_XTRA_TYPE_SUSTAIN:
        {
            create_mask(xtra_flags, FALSE, OFT_SUST, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            break;
        }
        case OBJECT_XTRA_TYPE_RESIST:
        {
            create_mask(xtra_flags, FALSE, OFT_HRES, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            break;
        }
        case OBJECT_XTRA_TYPE_POWER:
        {
            create_mask(xtra_flags, FALSE, OFT_MISC, OFT_PROT, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            create_mask(xtra_flags, FALSE, OFT_ESP, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            break;
        }
        case OBJECT_XTRA_RESIST_POWER:
        {
            create_mask(xtra_flags, FALSE, OFT_HRES, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            create_mask(xtra_flags, FALSE, OFT_MISC, OFT_PROT, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            create_mask(xtra_flags, FALSE, OFT_ESP, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            break;
        }
        case OBJECT_XTRA_TYPE_ESP:
        {
            create_mask(xtra_flags, FALSE, OFT_ESP, OFT_MAX);
            of_diff(learned_flags, xtra_flags);
            break;
        }
    }

    of_union(o_ptr->known_flags, learned_flags);

    if (object_add_ident_flags(o_ptr, IDENT_NAME))
    {
        /* If you know the ego, you know which it is of excellent or splendid */
        object_notice_sensing(p, o_ptr);

        object_check_for_ident(p, o_ptr);
    }
}


/*
 * Mark an object as sensed
 */
void object_notice_sensing(struct player *p, object_type *o_ptr)
{
    if (object_was_sensed(o_ptr)) return;

    if (o_ptr->artifact) o_ptr->ident |= IDENT_NAME;

    object_notice_curses(p, o_ptr);
    if (object_add_ident_flags(o_ptr, IDENT_SENSE))
        object_check_for_ident(p, o_ptr);
}


/*
 * Sense artifacts
 */
void object_sense_artifact(struct player *p, object_type *o_ptr)
{
    /* Mark artifact as found */
    set_artifact_info(p, o_ptr, ARTS_FOUND);

    if (o_ptr->artifact) object_notice_sensing(p, o_ptr);
    else o_ptr->ident |= IDENT_NOTART;
}


/*
 * Notice the "effect" from activating an object.
 *
 * o_ptr is the object to become aware of
 */
void object_notice_effect(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    if (object_add_ident_flags(o_ptr, IDENT_EFFECT))
        object_check_for_ident(p_ptr, o_ptr);

    /* Noticing an effect gains awareness */
    if (!object_flavor_is_aware(p_ptr, o_ptr)) object_flavor_aware(p_ptr, o_ptr);
}


/*
 * Notice things which happen on defending.
 */
static void object_notice_defence_plusses(struct player *p, object_type *o_ptr)
{
    if (!o_ptr->kind) return;
    if (object_defence_plusses_are_visible(p, o_ptr)) return;

    if (object_add_ident_flags(o_ptr, IDENT_DEFENCE))
        object_check_for_ident(p, o_ptr);

    if (o_ptr->ac || o_ptr->to_a)
    {
        char o_name[NORMAL_WID];

        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
        msgt(p, MSG_PSEUDOID, "You know more about the %s you are wearing.", o_name);
    }

    p->update |= (PU_BONUS);
    p->redraw |= (PR_EQUIP | PR_ARMOR);
}


/*
 * Notice things which happen on attacking.
 */
void object_notice_attack_plusses(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    if (!o_ptr->kind) return;
    if (object_attack_plusses_are_visible(p_ptr, o_ptr)) return;

    if (object_add_ident_flags(o_ptr, IDENT_ATTACK))
        object_check_for_ident(p_ptr, o_ptr);

    if (wield_slot(p_ptr, o_ptr) == INVEN_WIELD)
    {
        char o_name[NORMAL_WID];

        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
        msgt(p_ptr, MSG_PSEUDOID, "You know more about the %s you are using.", o_name);
    }
    else if ((o_ptr->to_d || o_ptr->to_h) && !(body_armor_p(o_ptr) && (o_ptr->to_h < 0)))
    {
        char o_name[NORMAL_WID];

        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
        msgt(p_ptr, MSG_PSEUDOID, "Your %s glows.", o_name);
    }

    p_ptr->update |= (PU_BONUS);
    p_ptr->redraw |= (PR_EQUIP | PR_PLUSSES);
}


/*
 * Notice a single flag - returns TRUE if anything new was learned
 */
bool object_notice_flag(struct player *p, object_type *o_ptr, int flag)
{
    if (!of_has(o_ptr->known_flags, flag))
    {
        of_on(o_ptr->known_flags, flag);
        object_check_for_ident(p, o_ptr);

        return TRUE;
    }

    return FALSE;
}


/*
 * Notice a set of flags - returns TRUE if anything new was learned
 */
bool object_notice_flags(struct player *p, object_type *o_ptr, bitflag flags[OF_SIZE])
{
    if (!of_is_subset(o_ptr->known_flags, flags))
    {
        of_union(o_ptr->known_flags, flags);
        object_check_for_ident(p, o_ptr);

        return TRUE;
    }

    return FALSE;
}


/*
 * Notice curses on an object.
 *
 * o_ptr is the object to notice curses on
 */
bool object_notice_curses(struct player *p, object_type *o_ptr)
{
    bitflag f[OF_SIZE], f2[OF_SIZE];

    object_flags(o_ptr, f);

    /* Gather whatever curse flags there are to know */
    create_mask(f2, FALSE, OFT_CURSE, OFT_MAX);

    /* Remove everything except the curse flags */
    of_inter(f, f2);

    /* Give knowledge of which curses are present */
    object_notice_flags(p, o_ptr, f);
    object_check_for_ident(p, o_ptr);

    if (p) p->notice |= PN_SQUELCH;

    return !of_is_empty(f);
}


/*
 * Notice things which happen on defending.
 */
void object_notice_on_defend(struct player *p)
{
    int i;

    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        if (p->inventory[i].kind)
            object_notice_defence_plusses(p, &p->inventory[i]);
    }
}


/*
 * Notice stuff when firing or throwing objects.
 */
void object_notice_on_firing(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);

    if (object_add_ident_flags(o_ptr, IDENT_FIRED))
        object_check_for_ident(p_ptr, o_ptr);
}


/*
 * Determine whether an item is obviously {excellent} when worn/wielded.
 */
void object_notice_on_wield(int Ind, object_type *o_ptr)
{
    player_type *p_ptr = player_get(Ind);
    bitflag f[OF_SIZE], f2[OF_SIZE], obvious_mask[OF_SIZE];
    bool obvious = FALSE;

    create_mask(obvious_mask, TRUE, OFID_WIELD, OFT_MAX);

    /* Save time of wield for later */
    ht_copy(&p_ptr->object_last_wield, &turn);

    /* Only deal with un-ID'd items */
    if (object_is_known(p_ptr, o_ptr)) return;

    /* Wear it */
    object_flavor_tried(Ind, o_ptr);
    if (object_add_ident_flags(o_ptr, IDENT_WORN))
        object_check_for_ident(p_ptr, o_ptr);

    if (easy_know(o_ptr, object_flavor_is_aware(p_ptr, o_ptr)))
    {
        object_notice_everything(p_ptr, o_ptr, FALSE);
        return;
    }

    /* Automatically sense artifacts upon wield */
    object_sense_artifact(p_ptr, o_ptr);

    /* Special case FA, needed at least for mages wielding gloves */
    if (object_FA_would_be_obvious(p_ptr, o_ptr)) of_on(obvious_mask, OF_FREE_ACT);

    /* Extract the flags */
    object_flags(o_ptr, f);

    /* Find obvious things (disregarding curses) */
    create_mask(f2, FALSE, OFT_CURSE, OFT_MAX);
    of_diff(obvious_mask, f2);
    if (of_is_inter(f, obvious_mask)) obvious = TRUE;
    create_mask(obvious_mask, TRUE, OFID_WIELD, OFT_MAX);

    /* Notice any obvious brands or slays */
    object_notice_slays(p_ptr, o_ptr, obvious_mask);

    /* Learn about obvious flags */
    of_union(o_ptr->known_flags, obvious_mask);

    /* Hack -- jewelry */
    if (object_is_jewelry(o_ptr))
    {
        /* Learn the flavor of jewelry with obvious flags */
        if (EASY_LEARN && obvious)
            object_flavor_aware(p_ptr, o_ptr);

        /* Learn all flags on any aware non-artifact jewelry */
        if (object_flavor_is_aware(p_ptr, o_ptr) && !o_ptr->artifact)
            object_know_all_flags(o_ptr);
    }

    object_check_for_ident(p_ptr, o_ptr);

    if (!obvious) return;

    if (of_has(f, OF_STR))
        msg(p_ptr, "You feel %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_STR)] > 0)? "stronger": "weaker"));
    if (of_has(f, OF_INT))
        msg(p_ptr, "You feel %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_INT)] > 0)? "smarter": "more stupid"));
    if (of_has(f, OF_WIS))
        msg(p_ptr, "You feel %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_WIS)] > 0)? "wiser": "more naive"));
    if (of_has(f, OF_DEX))
        msg(p_ptr, "You feel %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_DEX)] > 0)? "more dextrous": "clumsier"));
    if (of_has(f, OF_CON))
        msg(p_ptr, "You feel %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_CON)] > 0)? "healthier": "sicklier"));
    if (of_has(f, OF_CHR))
        msg(p_ptr, "You feel %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_CHR)] > 0)? "cuter": "uglier"));
    if (of_has(f, OF_SPEED))
        msg(p_ptr, "You feel %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_SPEED)] > 0)? "quicker": "more sluggish"));
    if (of_has(f, OF_BLOWS))
        msg(p_ptr, "Your hands %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_BLOWS)] > 0)? "tingle": "ache"));
    if (of_has(f, OF_SHOTS))
        msg(p_ptr, "Your hands %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_SHOTS)] > 0)? "tingle": "ache"));
    if (of_has(f, OF_INFRA))
        msg(p_ptr, "Your eyes %s!",
            ((o_ptr->pval[which_pval(o_ptr, OF_INFRA)] > 0)? "tingle": "ache"));
    if (of_has(f, OF_MANA))
        msg(p_ptr, "You feel %s attuned to magic!",
            ((o_ptr->pval[which_pval(o_ptr, OF_MANA)] > 0)? "more": "less"));

    if (of_has(f, OF_LIGHT))
    {
        char o_name[NORMAL_WID];

        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
        msg(p_ptr, "Your %s glows!", o_name);
    }
    create_mask(f2, FALSE, OFT_ESP, OFT_MAX);
    if (of_is_inter(f, f2)) msg(p_ptr, "Your mind feels strangely sharper!");
    if (of_has(f, OF_KNOWLEDGE)) msg(p_ptr, "You feel more knowledgeable!");

    /* Remember the flags */
    object_notice_sensing(p_ptr, o_ptr);
}


/*
 * Notice things about an object that would be noticed in time.
 */
static void object_notice_after_time(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i, j;
    int flag;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    bitflag f[OF_SIZE], timed_mask[OF_SIZE], pval_f[MAX_PVALS][OF_SIZE];

    create_mask(timed_mask, TRUE, OFID_TIMED, OFT_MAX);

    /* Check every item the player is wearing */
    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        o_ptr = &p_ptr->inventory[i];

        if (!o_ptr->kind || object_is_known(p_ptr, o_ptr)) continue;

        /* Check for timed notice flags */
        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
        object_flags(o_ptr, f);
        object_pval_flags(o_ptr, pval_f);
        of_inter(f, timed_mask);
        for (j = 0; j < o_ptr->num_pvals; j++) of_inter(pval_f[j], timed_mask);

        for (flag = of_next(f, FLAG_START); flag != FLAG_END; flag = of_next(f, flag + 1))
        {
            if (!of_has(o_ptr->known_flags, flag))
            {
                /* Message */
                flag_message(p_ptr, flag, o_name);

                /* Notice the flag */
                object_notice_flag(p_ptr, o_ptr, flag);

                if (object_is_jewelry(o_ptr) && (!o_ptr->effect ||
                    object_effect_is_known(p_ptr, o_ptr)))
                {
                    object_flavor_aware(p_ptr, o_ptr);
                    object_check_for_ident(p_ptr, o_ptr);
                }
            }
            else
            {
                /* Notice the flag is absent */
                object_notice_flag(p_ptr, o_ptr, flag);
            }
        }

        for (j = 0; j < o_ptr->num_pvals; j++)
        {
            for (flag = of_next(pval_f[j], FLAG_START); flag != FLAG_END;
                flag = of_next(pval_f[j], flag + 1))
            {
                if (!of_has(o_ptr->known_flags, flag))
                {
                    /* Message */
                    flag_message(p_ptr, flag, o_name);

                    /* Notice the flag */
                    object_notice_flag(p_ptr, o_ptr, flag);

                    if (object_is_jewelry(o_ptr) && (!o_ptr->effect ||
                        object_effect_is_known(p_ptr, o_ptr)))
                    {
                        object_flavor_aware(p_ptr, o_ptr);
                        object_check_for_ident(p_ptr, o_ptr);
                    }
                }
                else
                {
                    /* Notice the flag is absent */
                    object_notice_flag(p_ptr, o_ptr, flag);
                }
            }
        }

        /* XXX Is this necessary? */
        object_check_for_ident(p_ptr, o_ptr);
    }
}


/*
 * Notice a given special flag on wielded items.
 *
 * flag is the flag to notice
 */
void wieldeds_notice_flag(struct player *p, int flag)
{
    int i;

    /* Sanity check */
    if (!flag) return;

    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &p->inventory[i];
        bitflag f[OF_SIZE];

        if (!o_ptr->kind) continue;

        object_flags(o_ptr, f);

        if (of_has(f, flag) && !of_has(o_ptr->known_flags, flag))
        {
            char o_name[NORMAL_WID];

            object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

            /* Notice the flag */
            object_notice_flag(p, o_ptr, flag);

            if (EASY_LEARN && object_is_jewelry(o_ptr))
            {
                object_flavor_aware(p, o_ptr);
                object_check_for_ident(p, o_ptr);
            }

            /* Message */
            flag_message(p, flag, o_name);
        }
        else
        {
            /* Notice that flag is absent */
            object_notice_flag(p, o_ptr, flag);
        }

        /* XXX */
        object_check_for_ident(p, o_ptr);
    }
}


/*
 * Notice to-hit bonus on attacking.
 */
void wieldeds_notice_to_hit_on_attack(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Does not apply to weapon, missile or bow which should be done separately */
    for (i = INVEN_WIELD + 2; i < INVEN_TOTAL; i++)
    {
        if (p_ptr->inventory[i].kind && p_ptr->inventory[i].to_h)
            object_notice_attack_plusses(Ind, &p_ptr->inventory[i]);
    }
}


/*
 * Notice things which happen on attacking.
 */
void wieldeds_notice_on_attack(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Does not apply to weapon, missile or bow which should be done separately */
    for (i = INVEN_WIELD + 2; i < INVEN_TOTAL; i++)
    {
        if (p_ptr->inventory[i].kind)
            object_notice_attack_plusses(Ind, &p_ptr->inventory[i]);
    }
}


bool object_FA_would_be_obvious(struct player *p, const object_type *o_ptr)
{
    if (!p) return FALSE;

    if (player_has(p, PF_CUMBER_GLOVE) && (wield_slot(p, o_ptr) == INVEN_HANDS))
    {
        bitflag flags[OF_SIZE];

        object_flags(o_ptr, flags);

        if (!of_has(flags, OF_DEX) && !of_has(flags, OF_SPELLS_OK)) return TRUE;
    }

    return FALSE;
}


obj_pseudo_t object_pseudo_other(const object_type *o_ptr)
{
    s16b to_h = (o_ptr->to_h - randcalc(o_ptr->kind->to_h, 0, MINIMISE));
    s16b to_d = (o_ptr->to_d - randcalc(o_ptr->kind->to_d, 0, MINIMISE));
    s16b to_a = (o_ptr->to_a - randcalc(o_ptr->kind->to_a, 0, MINIMISE));

    /* Uncursed bad egos are not excellent */
    if (o_ptr->ego)
    {
        bitflag f2[OF_SIZE];

        create_mask(f2, FALSE, OFT_CURSE, OFT_MAX);

        if (of_is_inter(o_ptr->ego->flags, f2))
            return INSCRIP_STRANGE;
        return INSCRIP_EXCELLENT;
    }

    if ((to_a == 0) && (to_h == 0) && (to_d == 0)) return INSCRIP_AVERAGE;

    if ((to_a >= 0) && (to_h >= 0) && (to_d >= 0))
        return INSCRIP_MAGICAL_GOOD;

    if ((to_a <= 0) && (to_h <= 0) && (to_d <= 0))
        return INSCRIP_MAGICAL_BAD;

    return INSCRIP_STRANGE;
}


/*
 * Given an object, return a short identifier which gives some idea of what
 * the item is.
 */
obj_pseudo_t object_pseudo(struct player *p, const object_type *o_ptr, bool aware,
    bool known)
{
    bitflag flags[OF_SIZE], f2[OF_SIZE];

    /*
     * Get the known and obvious flags on the object,
     * not including curses or properties of the kind
     */
    object_flags_known(o_ptr, flags, aware);
    create_mask(f2, TRUE, OFID_WIELD, OFT_MAX);

    /* FA on gloves is obvious to mage casters */
    if (object_FA_would_be_obvious(p, o_ptr)) of_on(f2, OF_FREE_ACT);

    /* Now we remove the non-obvious known flags */
    of_inter(flags, f2);

    /* Now we remove the cursed flags and the kind flags */
    create_mask(f2, FALSE, OFT_CURSE, OFT_MAX);
    of_diff(flags, f2);
    of_diff(flags, o_ptr->kind->flags);

    if (o_ptr->ident & IDENT_INDESTRUCT) return INSCRIP_SPECIAL;
    if ((object_was_sensed(o_ptr) || object_was_worn(o_ptr)) && o_ptr->artifact)
        return INSCRIP_SPECIAL;

    /* Jewelry does not pseudo */
    if (object_is_jewelry(o_ptr)) return INSCRIP_NULL;

    if (!of_is_empty(flags)) return INSCRIP_SPLENDID;

    if (!known && !object_was_sensed(o_ptr)) return INSCRIP_NULL;

    return object_pseudo_other(o_ptr);
}


/*
 * Sense the inventory
 */
void sense_inventory(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;
    char o_name[NORMAL_WID];
    unsigned int rate;
    int improv;

    /* No ID when confused */
    if (p_ptr->timed[TMD_CONFUSED]) return;

    /* Notice some things after a while */
    if (ht_diff(&turn, &p_ptr->object_last_wield) >= 3000)
    {
        object_notice_after_time(Ind);
        ht_reset(&p_ptr->object_last_wield);
    }

    /* Get improvement rate */
    improv = p_ptr->lev;
    if (player_has(p_ptr, PF_PSEUDO_ID_IMPROV)) improv *= p_ptr->lev;
    rate = p_ptr->clazz->sense_base / (improv + p_ptr->clazz->sense_div);

    /* Check if player may sense anything this time */
    if ((p_ptr->lev < 20) && !one_in_(rate)) return;

    /* Check everything */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        const char *text = NULL;
        object_type *o_ptr = &p_ptr->inventory[i];
        obj_pseudo_t feel;
        bool cursed, worthless;
        bool okay;

        /* Skip empty slots */
        if (!o_ptr->kind) continue;

        /* Valid "tval" codes */
        okay = is_sense_machine(o_ptr);

        /* Skip non-sense machines */
        if (!okay) continue;

        /* It is known, no information needed */
        if (object_is_known(p_ptr, o_ptr)) continue;

        /* It has already been sensed, do not sense it again */
        if (object_was_sensed(o_ptr))
        {
            /* Small chance of wielded, sensed items getting complete ID */
            if (!o_ptr->artifact && (i >= INVEN_WIELD) && one_in_(1000))
                do_ident_item(p_ptr, i, o_ptr);

            continue;
        }

        /* Occasional failure on inventory items */
        if ((i < INVEN_WIELD) && one_in_(5)) continue;

        /* Sense the object */
        object_notice_sensing(p_ptr, o_ptr);
        cursed = object_notice_curses(p_ptr, o_ptr);

        /* Hack -- Worthless objects */
        worthless = worthless_p(o_ptr);
        if (worthless) o_ptr->ident |= IDENT_WORTHLESS;

        /* Get the feeling */
        feel = object_pseudo(p_ptr, o_ptr, object_flavor_is_aware(p_ptr, o_ptr),
            object_is_known(p_ptr, o_ptr));

        /* Stop everything */
        disturb(p_ptr, 0, 0);

        if (cursed) text = "cursed";
        else if (worthless) text = "worthless";
        else text = inscrip_text[feel];

        /* Get an object description */
        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

        /* Average pseudo-ID means full ID */
        if (feel == INSCRIP_AVERAGE)
        {
            object_notice_everything(p_ptr, o_ptr, FALSE);

            if (worthless) text = "worthless";
            else text = "average";
        }

        /* Message (equipment) */
        if (i >= INVEN_WIELD)
            msgt(p_ptr, MSG_PSEUDOID,
                "You feel the %s (%c) you are %s %s %s...", o_name,
                index_to_label(i), describe_use(p_ptr, i),
                ((o_ptr->number == 1)? "is": "are"), text);

        /* Message (inventory) */
        else
            msgt(p_ptr, MSG_PSEUDOID,
                "You feel the %s (%c) in your pack %s %s...", o_name,
                index_to_label(i),
                ((o_ptr->number == 1)? "is": "are"), text);

        /* Set squelch flag as appropriate */
        if (i < INVEN_WIELD) p_ptr->notice |= PN_SQUELCH;

        /* Combine / Reorder the pack (later) */
        p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

        /* Redraw */
        p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
    }
}


/*
 * Mark an object as "aware" or "tried".
 */
void object_aware_tried(int Ind, object_type *o_ptr, bool ident, bool used)
{
    player_type *p_ptr = player_get(Ind);

    /*
     * If the player becomes aware of the item's function, then mark it as
     * aware and reward the player with some experience. Otherwise, mark
     * it as "tried".
     */
    if (ident && !p_ptr->was_aware)
    {
        /* Object level */
        int lev = o_ptr->kind->level;

        object_flavor_aware(p_ptr, o_ptr);
        if (o_ptr->tval == TV_ROD) object_notice_everything(p_ptr, o_ptr, FALSE);
        player_exp_gain(p_ptr, (lev + (p_ptr->lev >> 1)) / p_ptr->lev);
        p_ptr->notice |= PN_SQUELCH;
    }
    else if (used)
        object_flavor_tried(Ind, o_ptr);
}
