/*
 * File: obj-power.c
 * Purpose: Calculation of object power
 *
 * Copyright (c) 2001 Chris Carr, Chris Robertson
 * Revised in 2009-11 by Chris Carr, Peter Denison
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
 * Define a set of constants for dealing with launchers and ammo:
 * - the assumed average damage of ammo (for rating launchers)
 * (the current values assume normal (non-seeker) ammo enchanted to +9)
 * - the assumed bonus on launchers (for rating ego ammo)
 * - twice the assumed multiplier (for rating any ammo)
 * N.B. Ammo tvals are assumed to be consecutive! We access this array using
 * (obj->tval - TV_ROCK) for ammo, and
 * 1 + (obj->sval / 10) for launchers
 */
static struct archery
{
    int ammo_tval;
    int ammo_dam;
    int launch_dam;
    int launch_mult;
} archery[] =
{
    {TV_ROCK, 0, 0, 2},
    {TV_SHOT, 10, 9, 4},
    {TV_ARROW, 12, 9, 5},
    {TV_BOLT, 14, 9, 7}
};


/*
 * Set the weightings of flag types:
 * - factor for power increment for multiple flags
 * - additional power bonus for a "full set" of these flags
 * - number of these flags which constitute a "full set"
 */
static struct flag_set
{
    int type;
    int factor;
    int bonus;
    int size;
    int count;
    const char *desc;
} flag_sets[] =
{
    {OFT_SUST, 1, 10, 5, 0, "sustains"},
    {OFT_PROT, 3, 15, 4, 0, "protections"},
    {OFT_MISC, 1, 25, 7, 0, "misc abilities"},
    {OFT_ESP, 1, 25, 10, 0, "ESP flags"}
};


enum
{
    T_LRES,
    T_HRES,
    T_XRES
};


/*
 * Similar data for elements
 */
static struct element_set
{
    int type;
    int res_level;
    int factor;
    int bonus;
    int size;
    int count;
    const char *desc;
} element_sets[] =
{
    {T_LRES, 3, 6, INHIBIT_POWER, 4, 0, "immunities"},
    {T_LRES, 1, 1, 10, 4, 0, "low resists"},
    {T_HRES, 1, 2, 10, 9, 0, "high resists"},
    {T_XRES, 1, 2, 10, 2, 0, "extra high resists"}
};


/*
 * Power data for elements
 */
static struct element_power
{
    const char *name;
    int type;
    int ignore_power;
    int vuln_power;
    int res_power;
    int im_power;
} element_powers[] =
{
    {"acid", T_LRES, 3, -6, 5, 38},
    {"lightning", T_LRES, 1, -6, 6, 35},
    {"fire", T_LRES, 3, -6, 6, 40},
    {"cold", T_LRES, 1, -6, 6, 37},
    {"poison", T_HRES, 0, 0, 28, 0},
    {"light", T_HRES, 0, 0, 6, 0},
    {"dark", T_HRES, 0, 0, 16, 0},
    {"sound", T_HRES, 0, 0, 14, 0},
    {"shards", T_HRES, 0, 0, 8, 0},
    {"nexus", T_HRES, 0, 0, 15, 0},
    {"nether", T_HRES, 0, 0, 20, 0},
    {"chaos", T_HRES, 0, 0, 20, 0},
    {"disenchantment", T_HRES, 0, 0, 20, 0},
    {"time", T_XRES, 0, 0, 20, 0},
    {"mana", T_XRES, 0, 0, 20, 0}
};


/*
 * Boost ratings for combinations of ability bonuses
 * We go up to +24 here - anything higher is inhibited
 * N.B. Not all stats count equally towards this total
 */
static s16b ability_power[25] =
    {0, 0, 0, 0, 0, 0, 0, 2, 4, 6, 8,
    12, 16, 20, 24, 30, 36, 42, 48, 56, 64,
    74, 84, 96, 110};


/*
 * Calculate the multiplier we'll get with a given bow type.
 */
static int bow_multiplier(const struct object *obj)
{
    if (!tval_is_launcher(obj)) return 1;

    return obj->pval;
}


/*
 * To damage power
 */
static int to_damage_power(struct player *p, const struct object *obj)
{
    int power;

    /* Start with any damage boost from the item itself */
    power = (obj->to_d * DAMAGE_POWER / 2);
    if (obj->to_d >= INHIBIT_TO_DAM) return INHIBIT_POWER;

    /* PWMAngband: limit damage power on artifact ammo */
    if (obj->artifact && tval_is_ammo(obj) && (obj->to_d >= INHIBIT_TO_DAM_AMMO))
        return INHIBIT_POWER;

    /* Add 2nd lot of damage power for nonweapons */
    if ((wield_slot(p, obj) != slot_by_name(p, "shooting")) && !tval_is_melee_weapon(obj) &&
        !tval_is_ammo(obj))
    {
        power += (obj->to_d * DAMAGE_POWER);
    }

    /* PWMAngband: add 2nd lot of damage power for artifact ammo */
    /*if (obj->artifact && tval_is_ammo(obj)) power += (obj->to_d * DAMAGE_POWER);*/

    return power;
}


/*
 * Damage dice power or equivalent
 */
static int damage_dice_power(struct player *p, const struct object *obj)
{
    int dice = 0;

    /* Add damage from dice for any wieldable weapon or ammo */
    if (tval_is_melee_weapon(obj) || tval_is_mstaff(obj) || tval_is_ammo(obj))
        dice = (obj->dd * (obj->ds + 1) * DAMAGE_POWER / 4);

    /* Add power boost for nonweapons with combat flags */
    else if (wield_slot(p, obj) != slot_by_name(p, "shooting"))
    {
        if (obj->brands || obj->slays || (obj->modifiers[OBJ_MOD_BLOWS] > 0) ||
            (obj->modifiers[OBJ_MOD_SHOTS] > 0) || (obj->modifiers[OBJ_MOD_MIGHT] > 0))
        {
            dice = (WEAP_DAMAGE * DAMAGE_POWER);
        }
    }

    return dice;
}


/*
 * Add ammo damage for launchers, get multiplier and rescale
 */
static int ammo_damage_power(struct player *p, const struct object *obj)
{
    int q = 0;
    int launcher = 0;

    if (wield_slot(p, obj) == slot_by_name(p, "shooting"))
    {
        if (kf_has(obj->kind->kind_flags, KF_SHOOTS_SHOTS)) launcher = 1;
        else if (kf_has(obj->kind->kind_flags, KF_SHOOTS_ARROWS)) launcher = 2;
        else if (kf_has(obj->kind->kind_flags, KF_SHOOTS_BOLTS)) launcher = 3;

        q = (archery[launcher].ammo_dam * DAMAGE_POWER / 2);
    }

    return q;
}


/*
 * Add launcher bonus for ego ammo, multiply for launcher and rescale
 */
static int launcher_ammo_damage_power(const struct object *obj, int power)
{
    int ammo_type = 0;

    if (tval_is_ammo(obj))
    {
        if (obj->tval == TV_SHOT) ammo_type = 1;
        if (obj->tval == TV_ARROW) ammo_type = 2;
        if (obj->tval == TV_BOLT) ammo_type = 3;
        if (obj->ego)
            power += (archery[ammo_type].launch_dam * DAMAGE_POWER / 2);
        power = power * archery[ammo_type].launch_mult / (2 * MAX_BLOWS);
    }

    return power;
}


/*
 * Add power for extra blows
 */
static int extra_blows_power(const struct object *obj, int power, bool known)
{
    if (obj->modifiers[OBJ_MOD_BLOWS] == 0) return power;

    if (known || object_this_mod_is_visible(obj, OBJ_MOD_BLOWS))
    {
        /* Inhibit */
        if (obj->modifiers[OBJ_MOD_BLOWS] >= INHIBIT_BLOWS) return INHIBIT_POWER;

        power = power * (MAX_BLOWS + obj->modifiers[OBJ_MOD_BLOWS]) / MAX_BLOWS;

        /* Add boost for assumed off-weapon damage */
        power += (NONWEAP_DAMAGE * obj->modifiers[OBJ_MOD_BLOWS] * DAMAGE_POWER / 2);
    }

    return power;
}


/*
 * Add power for extra shots - note that we cannot handle negative shots
 */
static int extra_shots_power(const struct object *obj, int power, bool known)
{
    if (obj->modifiers[OBJ_MOD_SHOTS] == 0) return power;

    if (known || object_this_mod_is_visible(obj, OBJ_MOD_SHOTS))
    {
        /* Inhibit */
        if (obj->modifiers[OBJ_MOD_SHOTS] >= INHIBIT_SHOTS) return INHIBIT_POWER;

        if (obj->modifiers[OBJ_MOD_SHOTS] > 0)
            power = (power * (1 + obj->modifiers[OBJ_MOD_SHOTS]));
    }

    return power;
}


/*
 * Add power for extra might
 */
static int extra_might_power(const struct object *obj, int power, int mult, bool known)
{
    if (known || object_this_mod_is_visible(obj, OBJ_MOD_MIGHT))
    {
        /* Inhibit */
        if (obj->modifiers[OBJ_MOD_MIGHT] >= INHIBIT_MIGHT) return INHIBIT_POWER;

        mult += obj->modifiers[OBJ_MOD_MIGHT];
    }
    power *= mult;

    return power;
}


/*
 * Calculate the rating for a given slay combination
 */
static s32b slay_power(struct player *p, const struct object* obj, int power, int dice_pwr,
    bool known)
{
    u32b sv = 0;
    int i, num_brands, num_slays, num_kills;
    int mult;
    static u32b tot_mon_power = 0;
    struct brand *brands = obj->brands;
    struct slay *slays = obj->slays;

    /* Count the brands and slays */
    num_brands = brand_count(brands);
    num_slays = slay_count(slays);

    /* If there are no slays or brands return */
    if ((num_brands + num_slays) == 0) return power;

    brands = (known? obj->brands: obj->known->brands);
    slays = (known? obj->slays: obj->known->slays);

    /* Count the known brands and slays */
    num_brands = brand_count(brands);
    num_slays = 0;
    num_kills = 0;
    while (slays)
    {
        if (slays->multiplier <= 3) num_slays++;
        else num_kills++;
        slays = slays->next;
    }

    /* If there are no known slays or brands return */
    if ((num_brands + num_slays + num_kills) == 0) return power + dice_pwr;

    /* Determine total monster power */
    if (tot_mon_power == 0)
    {
        for (i = 0; i < z_info->r_max; i++)
            tot_mon_power += r_info[i].scaled_power;
    }

    /* Look in the cache to see if we know this one yet */
    sv = check_slay_cache(obj, known);

    /* If it's cached (or there are no slays), return the value */
    if (sv) {}

    /*
     * Otherwise we need to calculate the expected average multiplier
     * for this combination (multiplied by the total number of
     * monsters, which we'll divide out later).
     */
    else
    {
        for (i = 0; i < z_info->r_max; i++)
        {
            struct object *checked_obj = (known? (struct object *)obj: obj->known);
            struct monster *mon = mem_zalloc(sizeof(*mon));
            int best_mult = 1;
            char verb[30];
            struct actor who_body;
            struct actor *who = &who_body;
            bool dummy = false;

            mult = 1;
            mon->race = &r_info[i];
            ACTOR_MONSTER(who, mon);

            /* Find the best multiplier against this monster */
            improve_attack_modifier(p, checked_obj, who, &best_mult, &dummy, verb, sizeof(verb),
                true, false);
            mult = best_mult;

            /* Add up totals */
            sv += mult * mon->race->scaled_power;
            mem_free(mon);
        }

        /*
         * To get the expected damage for this weapon, multiply the
         * average damage from base dice by sv, and divide by the
         * total number of monsters.
         */

        /* Add to the cache */
        fill_slay_cache(obj, known, sv);
    }

    /* Apply the correct slay multiplier */
    power += (dice_pwr * (sv / 100)) / (tot_mon_power / 100);

    /* Bonuses for multiple brands and slays */
    if (num_slays > 1)
        power += (num_slays * num_slays * dice_pwr) / (DAMAGE_POWER * 5);
    if (num_brands > 1)
        power += (2 * num_brands * num_brands * dice_pwr) / (DAMAGE_POWER * 5);
    if (num_kills > 1)
        power += (3 * num_kills * num_kills * dice_pwr) / (DAMAGE_POWER * 5);
    if (num_slays == 8) power += 10;
    if (num_brands == 5) power += 20;
    if (num_kills == 3) power += 20;

    return power;
}


/*
 * Melee weapons assume MAX_BLOWS per turn, so we must divide by MAX_BLOWS
 * to get equal ratings for launchers.
 */
static int rescale_bow_power(struct player *p, const struct object *obj, int power)
{
    if (wield_slot(p, obj) == slot_by_name(p, "shooting")) power /= MAX_BLOWS;

    return power;
}


/*
 * Add power for +to_hit
 */
static int to_hit_power(const struct object *obj, int power)
{
    power += (obj->to_h * TO_HIT_POWER / 2);
    if (obj->to_h >= INHIBIT_TO_HIT) return INHIBIT_POWER;

    return power;
}


/*
 * Add power for base AC and adjust for weight
 */
static int ac_power(const struct object *obj, int power)
{
    int q = 0;

    if (obj->ac)
    {
        power += BASE_ARMOUR_POWER;
        q += (obj->ac * BASE_AC_POWER / 2);

        /* Add power for AC per unit weight */
        if (obj->weight > 0)
        {
            int i = 750 * (obj->ac + obj->to_a) / obj->weight;

            /* Avoid overpricing Elven Cloaks */
            if (i > 450) i = 450;

            q *= i;
            q /= 100;
        }

        /* Weightless (ethereal) armour items get fixed boost */
        else q *= 5;

        power += q;
    }

    return power;
}


/*
 * Add power for +to_ac
 */
static int to_ac_power(const struct object *obj, int power)
{
    if (obj->to_a == 0) return power;

    power += (obj->to_a * TO_AC_POWER / 2);
    if (obj->to_a >= HIGH_TO_AC)
        power += ((obj->to_a - (HIGH_TO_AC - 1)) * TO_AC_POWER);
    if (obj->to_a >= VERYHIGH_TO_AC)
        power += ((obj->to_a - (VERYHIGH_TO_AC -1)) * TO_AC_POWER * 2);
    if (obj->to_a >= INHIBIT_AC) return INHIBIT_POWER;

    return power;
}


/*
 * Add base power for jewelry
 */
static int jewelry_power(const struct object *obj, int power)
{
    if (tval_is_jewelry(obj)) power += BASE_JEWELRY_POWER;

    return power;
}


/*
 * Add power for modifiers
 */
static int modifier_power(struct player *p, const struct object *obj, int power, bool known)
{
    int i, k = 1, extra_stat_bonus = 0;

    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (known || object_this_mod_is_visible(obj, i))
        {
            k = obj->modifiers[i];
            extra_stat_bonus += (k * mod_mult(i));
        }
        else continue;

        if (mod_power(i))
            power += (k * mod_power(i) * mod_slot_mult(p, i, wield_slot(p, obj)));
    }

    /* Add extra power term if there are a lot of ability bonuses */
    if (extra_stat_bonus > 249) return INHIBIT_POWER;
    if (extra_stat_bonus < 0) extra_stat_bonus = 0;
    power += ability_power[extra_stat_bonus / 10];

    return power;
}


/*
 * Add power for non-derived flags (derived flags have flag_power 0)
 */
static int flags_power(struct player *p, const struct object *obj, int power, bool known)
{
    size_t i, j;
    bitflag flags[OF_SIZE];

    /* Extract the flags */
    if (known)
        object_flags(obj, flags);
    else
        object_flags_known(obj, flags, object_flavor_is_aware(p, obj));

    /* Zero the flag counts */
    for (i = 0; i < N_ELEMENTS(flag_sets); i++) flag_sets[i].count = 0;

    for (i = of_next(flags, FLAG_START); i != FLAG_END; i = of_next(flags, i + 1))
    {
        if (flag_power(i))
            power += (flag_power(i) * flag_slot_mult(p, i, wield_slot(p, obj)));

        /* Track combinations of flag types - note we ignore ESP_POWER */
        for (j = 0; j < N_ELEMENTS(flag_sets); j++)
        {
            if ((flag_sets[j].type == obj_flag_type(i)) && (i != OF_ESP_POWER))
                flag_sets[j].count++;
        }
    }

    /* Add extra power for multiple flags of the same type */
    for (i = 0; i < N_ELEMENTS(flag_sets); i++)
    {
        if (flag_sets[i].count > 1)
            power += (flag_sets[i].factor * flag_sets[i].count * flag_sets[i].count);

        /* Add bonus if item has a full set of these flags */
        if (flag_sets[i].count == flag_sets[i].size) power += flag_sets[i].bonus;
    }

    return power;
}


/*
 * Add power for elemental properties
 */
static int element_power(const struct object *obj, int power, bool known, bool aware)
{
    size_t i, j;

    /* Zero the set counts */
    for (i = 0; i < N_ELEMENTS(element_sets); i++) element_sets[i].count = 0;

    /* Analyse each element for ignore, vulnerability, resistance or immunity */
    for (i = 0; i < N_ELEMENTS(element_powers); i++)
    {
        if (!known && !object_element_is_known(obj, i, aware)) continue;

        if (obj->el_info[i].flags & EL_INFO_IGNORE)
        {
            if (element_powers[i].ignore_power)
                power += element_powers[i].ignore_power;
        }

        if (obj->el_info[i].res_level == -1)
        {
            if (element_powers[i].vuln_power)
                power += element_powers[i].vuln_power;
        }
        else if (obj->el_info[i].res_level == 1)
        {
            if (element_powers[i].res_power)
                power += element_powers[i].res_power;
        }
        else if (obj->el_info[i].res_level == 3)
        {
            if (element_powers[i].im_power)
                power += (element_powers[i].im_power + element_powers[i].res_power);
        }

        /* Track combinations of element properties */
        for (j = 0; j < N_ELEMENTS(element_sets); j++)
        {
            if ((element_sets[j].type == element_powers[i].type) &&
                (element_sets[j].res_level <= obj->el_info[i].res_level))
            {
                element_sets[j].count++;
            }
        }
    }

    /* Add extra power for multiple flags of the same type */
    for (i = 0; i < N_ELEMENTS(element_sets); i++)
    {
        if (element_sets[i].count > 1)
            power += (element_sets[i].factor * element_sets[i].count * element_sets[i].count);

        /* Add bonus if item has a full set of these flags */
        if (element_sets[i].count == element_sets[i].size) power += element_sets[i].bonus;
    }

    return power;
}


/*
 * Add power for activation
 */
static int effects_power(struct player *p, const struct object *obj, int power, bool known)
{
    /* Get activation */
    if (known || object_effect_is_known(obj, object_flavor_is_aware(p, obj)))
    {
        /* Add power for activation */
        if (obj->activation && randcalc(obj->time, 0, MAXIMISE))
            power += obj->activation->power;
    }

    return power;
}


/*
 * Evaluate the object's overall power level.
 */
s32b object_power(struct player *p, const struct object* obj)
{
    s32b power = 0, dice_pwr = 0;
    int mult = 1;
    bool known = object_is_known(p, obj);

    /* Get all the attack power */
    power = to_damage_power(p, obj);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;
    dice_pwr = damage_dice_power(p, obj);
    power += dice_pwr;
    power += ammo_damage_power(p, obj);
    mult = bow_multiplier(obj);
    power = launcher_ammo_damage_power(obj, power);
    power = extra_blows_power(obj, power, known);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;
    power = extra_shots_power(obj, power, known);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;
    power = extra_might_power(obj, power, mult, known);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;
    power = slay_power(p, obj, power, dice_pwr, known);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;
    power = rescale_bow_power(p, obj, power);
    power = to_hit_power(obj, power);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;

    /* Armour class power */
    power = ac_power(obj, power);
    power = to_ac_power(obj, power);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;

    /* Bonus for jewelry */
    power = jewelry_power(obj, power);

    /* PWMAngband - Add base power for tools */
    if (wield_slot(p, obj) == slot_by_name(p, "tool")) power += BASE_TOOL_POWER;

    /* Other object properties */
    power = modifier_power(p, obj, power, known);
    if (power >= INHIBIT_POWER) return INHIBIT_POWER;
    power = flags_power(p, obj, power, known);
    power = element_power(obj, power, known, object_flavor_is_aware(p, obj));
    power = effects_power(p, obj, power, known);

    if (power >= INHIBIT_POWER) return INHIBIT_POWER;
    return power;
}


/*
 * Return the "value" of an "unknown" non-wearable item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(struct player *p, const struct object *obj)
{
    /* Use template cost for aware objects */
    if (object_flavor_is_aware(p, obj)) return obj->kind->cost;

    /* Analyze the type */
    switch (obj->tval)
    {
        case TV_MUSHROOM: return 5;
        case TV_POTION:
        case TV_SCROLL: return 20;
        case TV_WAND: return 50;
        case TV_STAFF: return 70;
        case TV_ROD: return 90;
    }

    /* Paranoia (should never come here) */
    return 0;
}


/*
 * Return the real price of a known (or partly known) item.
 *
 * Wands and staves get cost for each charge.
 *
 * Wearable items (weapons, launchers, jewelry, lights, armour, tools, ammo)
 * are priced according to their power rating. All ammo, and normal (non-ego)
 * torches are scaled down by AMMO_RESCALER to reflect their impermanence.
 *
 * PWMAngband: artifacts are always sellable
 */
s32b object_value_real(struct player *p, const struct object *obj, int qty)
{
    s32b value, total_value;
    s32b power;
    int a = 1;
    int b = 5;
    s32b min_value = (obj->artifact? 1L: 0L);

    /* Hack -- worthless objects */
    if (obj->origin == ORIGIN_WORTHLESS) return (min_value);

    /* Wearables and ammo have prices that vary by individual item properties */
    if (tval_has_variable_power(obj))
    {
        bool normal_ammo = (tval_is_ammo(obj) && !obj->artifact);
        bool normal_torch = (tval_is_light(obj) && of_has(obj->flags, OF_BURNS_OUT) &&
            !obj->ego);

        /* Calculate power and value */
        power = object_power(p, obj);
        value = SGN(power) * (a * power * power + b * power);

        /* Rescale for expendables */
        if (normal_ammo || normal_torch)
        {
            value = value / AMMO_RESCALER;
            if (value < 1) value = 1;
        }

        /* PWMAngband -- boost magic ammo */
        if (magic_ammo_p(obj)) value += 100L;

        /* PWMAngband -- boost artifact missiles */
        if (tval_is_ammo(obj) && obj->artifact) value += 10000L;

        /* PWMAngband -- boost rings of polymorphing */
        if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")))
        {
            struct monster_race *race = &r_info[obj->modifiers[OBJ_MOD_POLY_RACE]];

            value += MAX(race->level, 1) * MAX(race->mexp, 100);
        }

        /* Get the total value */
        total_value = value * qty;
    }
    else
    {
        /* Worthless items */
        if (!obj->kind->cost) return (min_value);

        /* Base cost */
        value = obj->kind->cost;

        /* Calculate total value */
        total_value = value * qty;

        /* Wands/Staffs */
        if (tval_can_have_charges(obj))
        {
            int charges = obj->pval * qty;

            /* Calculate number of charges, rounded up */
            if (obj->number)
            {
                charges = obj->pval * qty / obj->number;
                if ((obj->pval * qty) % obj->number != 0) charges++;
            }

            /* Pay extra for charges, depending on standard number of charges */
            total_value += value * charges / 20;
        }
    }

    /* No negative value */
    if (total_value < min_value) total_value = min_value;

    /* Return the value */
    return (total_value);
}


/*
 * Return the price of an item including plusses (and charges)
 *
 * This function returns the "value" of the given item
 *
 * Never notice unknown bonuses or properties, including curses,
 * since that would give players information they did not have.
 */
s32b object_value(struct player *p, const struct object *obj, int qty)
{
    s32b value;

    /* Gold */
    if (tval_is_money(obj)) return obj->pval;

    /* Known items use the actual value */
    if (object_is_known(p, obj))
    {
        if (cursed_p((bitflag *)obj->flags)) return (0L);

        value = object_value_real(p, obj, qty);
    }

    /* Variable power items are assessed by what is known about them */
    else if (tval_has_variable_power(obj))
    {
        struct object *temp_obj;

        /* Hack -- felt cursed items */
        if (object_was_sensed(obj) && cursed_p((bitflag *)obj->flags)) return (0L);

        temp_obj = object_new();
        object_copy(temp_obj, obj);

        /* Give temp_obj only the flags known to be in obj */
        object_flags_known(obj, temp_obj->flags, object_flavor_is_aware(p, obj));

        if (!object_attack_plusses_are_visible(obj))
            temp_obj->to_h = temp_obj->to_d = 0;
        if (!object_defence_plusses_are_visible(obj))
            temp_obj->to_a = 0;

        value = object_value_real(p, temp_obj, qty);
        object_delete(&temp_obj);
    }

    /* Unknown constant-price items just get a base value */
    else
        value = object_value_base(p, obj) * qty;

    /* Return the final value */
    return (value);
}
