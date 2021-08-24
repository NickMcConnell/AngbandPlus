/* File: obj_kind.c */

#include "angband.h"

#include <assert.h>

bool object_is_mushroom(object_type *o_ptr)
{
    bool result = FALSE;
    assert(SV_FOOD_MIN_MUSHROOM == 0);
    if ( o_ptr->tval == TV_FOOD
      /*&& SV_FOOD_MIN_MUSHROOM <= o_ptr->sval gcc warning on 0 <= unsigned char always true */
      && o_ptr->sval <= SV_FOOD_MAX_MUSHROOM )
    {
        result = TRUE;
    }
    return result;
}

bool object_is_flavor(object_type *o_ptr)
{
    return object_is_mushroom(o_ptr) || o_ptr->tval == TV_POTION || o_ptr->tval == TV_SCROLL;
}

bool mon_is_wanted(int r_idx)
{
    return BOOL(mon_race_lookup(r_idx)->flagsx & RFX_WANTED);
}

bool object_is_shoukinkubi(object_type *o_ptr)
{
    /* Require corpse or skeleton */
    if (o_ptr->tval != TV_CORPSE) return FALSE;

    /* Today's wanted */
    if (plr->today_mon && o_ptr->race_id == today_mon) return TRUE;

    /* Tsuchinoko */
    if (sym_equals(o_ptr->race_id, "J.tsuchinoko")) return TRUE;

    /* Unique monster */
    if (mon_is_wanted(o_ptr->race_id)) return TRUE;

    /* Not wanted */
    return FALSE;
}


/*
 * Favorite weapons
 */
bool object_is_favorite(object_type *o_ptr)
{
    int class_idx;

    if (!obj_is_weapon(o_ptr) && !obj_is_bow(o_ptr))
        return FALSE;

    if (prace_is_(RACE_MON_GIANT))
        return giant_is_favorite(o_ptr);

    class_idx = plr_pseudo_class_id();
    if (class_idx == CLASS_WEAPONMASTER)
        return weaponmaster_is_favorite(o_ptr);

    /* Favorite weapons are varied depend on the class */
    switch (class_idx)
    {
    case CLASS_PRIEST:
    case CLASS_HIGH_PRIEST:
        if (priest_is_evil())
            return TRUE;
        else if (plr->realm1 == REALM_NATURE)
            return o_ptr->tval == TV_HAFTED;
        else if (o_ptr->tval != TV_HAFTED && !obj_has_known_flag(o_ptr, OF_BLESSED))
            return FALSE;
        break;

    case CLASS_MYSTIC:
    case CLASS_MONK:
    case CLASS_FORCETRAINER:
        return FALSE;

    case CLASS_NINJA:
        if (o_ptr->tval != TV_BOW && !skills_weapon_is_icky(o_ptr->tval, o_ptr->sval))
            return TRUE;
        return FALSE;

    case CLASS_DUELIST:
    case CLASS_SKILLMASTER:
        if (skills_weapon_is_icky(o_ptr->tval, o_ptr->sval))
            return FALSE;
        break;

    case CLASS_BEASTMASTER:
    case CLASS_CAVALRY:
        if (!obj_has_known_flag(o_ptr, OF_RIDING))
            return FALSE;
        break;

    case CLASS_MAULER:
        return o_ptr->weight >= 280;

    case CLASS_ARCHAEOLOGIST:
        return archaeologist_is_favored_weapon(o_ptr);

    case CLASS_ARCHER:
    case CLASS_RANGER:
        return obj_is_bow(o_ptr);

    default:
        /* All weapons are okay for non-special classes */
        return TRUE;
    }

    return TRUE;
}


/*
 * Rare weapons/aromors
 * including Blade of Chaos, Dragon armors, etc.
 */
bool object_is_rare(object_type *o_ptr)
{
    switch(o_ptr->tval)
    {
    case TV_BOW:
        if (o_ptr->sval == SV_HARP) return TRUE;
        break;

    case TV_HAFTED:
        if (o_ptr->sval == SV_MACE_OF_DISRUPTION ||
            o_ptr->sval == SV_WIZSTAFF) return TRUE;
        break;

    case TV_POLEARM:
        if (o_ptr->sval == SV_SCYTHE_OF_SLICING ||
            o_ptr->sval == SV_DEATH_SCYTHE) return TRUE;
        break;

    case TV_SWORD:
        if (o_ptr->sval == SV_BLADE_OF_CHAOS ||
            o_ptr->sval == SV_DIAMOND_EDGE ||
            o_ptr->sval == SV_DOKUBARI ||
            o_ptr->sval == SV_DRAGON_FANG ||
            o_ptr->sval == SV_HAYABUSA) return TRUE;
        break;

    case TV_SHIELD:
        if (o_ptr->sval == SV_DRAGON_SHIELD ||
            o_ptr->sval == SV_MIRROR_SHIELD) return TRUE;
        break;

    case TV_HELM:
        if (o_ptr->sval == SV_DRAGON_HELM) return TRUE;
        break;

    case TV_BOOTS:
        if (o_ptr->sval == SV_PAIR_OF_DRAGON_GREAVE) return TRUE;
        break;

    case TV_CLOAK:
        if (o_ptr->sval == SV_ELVEN_CLOAK ||
            o_ptr->sval == SV_ETHEREAL_CLOAK ||
            o_ptr->sval == SV_DRAGON_CLOAK ||
            o_ptr->sval == SV_SHADOW_CLOAK) return TRUE;
        break;

    case TV_GLOVES:
        if (o_ptr->sval == SV_SET_OF_DRAGON_GLOVES) return TRUE;
        break;

    case TV_SOFT_ARMOR:
        if (o_ptr->sval == SV_KUROSHOUZOKU ||
            o_ptr->sval == SV_ABUNAI_MIZUGI) return TRUE;
        break;

    case TV_DRAG_ARMOR:
        return TRUE;

    case TV_ARROW:
        if (o_ptr->sval == SV_SEEKER_ARROW || o_ptr->sval == SV_MITHRIL_ARROW)
            return TRUE;
        break;

    case TV_BOLT:
        if (o_ptr->sval == SV_SEEKER_BOLT || o_ptr->sval == SV_MITHRIL_BOLT || o_ptr->sval == SV_ADAMANTINE_BOLT)
            return TRUE;
        break;

    case TV_SHOT:
        if (o_ptr->sval == SV_MITHRIL_SHOT)
            return TRUE;
        break;
    }

    /* Any others are not "rare" objects. */
    return FALSE;
}

/*
 * Check if an object is weapon, armour or ammo
 */
bool enchantment_hack = FALSE;
bool object_is_weapon_armour_ammo(object_type *o_ptr)
{
    if (enchantment_hack && obj_is_specified_art(o_ptr, "\\.Hephaestus")) return FALSE;

    return obj_is_weapon(o_ptr) || obj_is_bow(o_ptr) || obj_is_armor(o_ptr) || obj_is_ammo(o_ptr);
}


/*
 * Poison needle can not be enchanted
 */
bool object_refuse_enchant_weapon(object_type *o_ptr)
{
    if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_DOKUBARI) return TRUE;

    return FALSE;
}


/*
 * Check if an object is weapon (including bows and ammo) and allows enchantment
 */
bool object_allow_enchant_weapon(object_type *o_ptr)
{
    if ((obj_is_weapon_ammo(o_ptr) || obj_is_bow(o_ptr)) && !object_refuse_enchant_weapon(o_ptr)) return TRUE;

    return FALSE;
}


/*
 * Check if an object is melee weapon and allows enchantment
 */
bool object_allow_enchant_melee_weapon(object_type *o_ptr)
{
    if (obj_is_weapon(o_ptr) && !object_refuse_enchant_weapon(o_ptr)) return TRUE;

    return FALSE;
}


/*
 * Check if an object is made by a smith's special ability
 */
bool object_is_smith(object_type *o_ptr)
{
    if (object_is_weapon_armour_ammo(o_ptr) && !obj_is_(o_ptr, TV_HAFTED, SV_WIZSTAFF) && o_ptr->xtra3) return TRUE;

    return FALSE;
}

/*
 * Check if an object is neither artifact, ego, nor 'smith' object
 */
bool object_is_nameless(object_type *o_ptr)
{
    if (!obj_is_art(o_ptr) && !obj_is_ego(o_ptr) && !object_is_smith(o_ptr))
        return TRUE;

    return FALSE;
}


/*
 * Check if an object is melee weapon and allows wielding with two-hands
 */
bool object_allow_two_hands_wielding(object_type *o_ptr)
{
    if (obj_is_weapon(o_ptr) && ((o_ptr->weight > 99) || (o_ptr->tval == TV_POLEARM))) return TRUE;

    /* Hack: Shield Mastery technique */
    if (weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH && obj_is_shield(o_ptr)) return TRUE;
    return FALSE;
}
