/*
 * File: obj-power.c
 * Purpose: Calculation of object power
 *
 * Copyright (c) 2001 Chris Carr, Chris Robertson
 * Revised in 2009-11 by Chris Carr, Peter Denison
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
#include "../init.h"
#include "../effects.h"
#include "../monster/mon-power.h"
#include "pval.h"
#include "slays.h"


/*
 * Constants for the power algorithm:
 * - fudge factor for extra damage from rings etc. (used if extra blows)
 * - assumed damage for off-weapon brands
 * - base power for jewelry
 * - base power for armour items (for halving acid damage)
 * - power per point of damage
 * - power per point of +to_hit
 * - power per point of base AC
 * - power per point of +to_ac
 * (these four are all halved in the algorithm)
 * - assumed max blows
 * - inhibiting values for +blows/might/shots/immunities (max is one less)
 */
#define NONWEAP_DAMAGE          15  /* Fudge to boost extra blows */
#define WEAP_DAMAGE             12  /* And for off-weapon combat flags */
#define BASE_JEWELRY_POWER      4
#define BASE_ARMOUR_POWER       1
#define BASE_TOOL_POWER         5   /* PWMAngband: adjust this if necessary */
#define DAMAGE_POWER            5
#define TO_HIT_POWER            3
#define BASE_AC_POWER           2
#define TO_AC_POWER             2
#define MAX_BLOWS               5
#define INHIBIT_IMMUNITIES      4


/*
 * Define a set of constants for dealing with launchers and ammo:
 * - the assumed average damage of ammo (for rating launchers)
 * (the current values assume normal (non-seeker) ammo enchanted to +9)
 * - the assumed bonus on launchers (for rating ego ammo)
 * - twice the assumed multiplier (for rating any ammo)
 * N.B. Ammo tvals are assumed to be consecutive! We access this array using
 * (o_ptr->tval - TV_ROCK) for ammo, and
 * 1 + (o_ptr->sval / 10) for launchers
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
 * - whether value is damage-dependent
 */
static struct set
{
    int type;
    int factor;
    int bonus;
    int size;
    bool dam_dep;
    int count;
    const char *desc;
} sets[] =
{
    {OFT_SUST, 1, 10, 5, FALSE, 0, "sustains"},
    {OFT_SLAY, 1, 10, 8, TRUE,  0, "normal slays"},
    {OFT_BRAND, 2, 20, 5, TRUE,  0, "brands"},
    {OFT_KILL, 3, 20, 3, TRUE,  0, "x5 slays"},
    {OFT_IMM, 6, INHIBIT_POWER, 4, FALSE, 0, "immunities"},
    {OFT_LRES, 1, 10, 4, FALSE, 0, "low resists"},
    {OFT_HRES, 2, 10, 9, FALSE, 0, "high resists"},
    {OFT_PROT, 3, 15, 4, FALSE, 0, "protections"},
    {OFT_MISC, 1, 25, 7, FALSE, 0, "misc abilities"},
    {OFT_ESP, 1, 25, 10, FALSE, 0, "ESP flags"},
    {OFT_XRES, 2, 10, 2, FALSE, 0, "extra high resists"}
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
 * Calculate the rating for a given slay combination
 */
static s32b slay_power(struct player *p, const object_type* o_ptr, bool known, bool aware)
{
    bitflag s_index[OF_SIZE], f[OF_SIZE], f2[OF_SIZE];
    u32b sv = 0;
    int i;
    int mult;
    const struct slay *best_s_ptr = NULL;
    monster_race *r_ptr;

    if (known)
        object_flags(o_ptr, f);
    else
        object_flags_known(o_ptr, f, aware);

    /* Combine the slay bytes into an index value, return if there are none */
    of_copy(s_index, f);
    create_mask(f2, FALSE, OFT_SLAY, OFT_KILL, OFT_BRAND, OFT_MAX);

    if (!of_is_inter(s_index, f2)) return tot_mon_power;
    of_inter(s_index, f2);

    /* Look in the cache to see if we know this one yet */
    sv = check_slay_cache(s_index);

    /* If it's cached (or there are no slays), return the value */
    if (sv) return sv;

    /*
     * Otherwise we need to calculate the expected average multiplier
     * for this combination (multiplied by the total number of
     * monsters, which we'll divide out later).
     */
    for (i = 0; i < z_info->r_max; i++)
    {
        best_s_ptr = NULL;
        mult = 1;
        r_ptr = &r_info[i];

        /* Find the best multiplier against this monster */
        improve_attack_modifier(p, (object_type *)o_ptr, 0, TRUE, i, &best_s_ptr, !known);
        if (best_s_ptr) mult = best_s_ptr->mult;

        /* Add the multiple to sv */
        sv += mult * r_ptr->scaled_power;
    }

    /*
     * To get the expected damage for this weapon, multiply the
     * average damage from base dice by sv, and divide by the
     * total number of monsters.
     */

    /* Add to the cache */
    fill_slay_cache(s_index, sv);

    return sv;
}


/*
 * Calculate the multiplier we'll get with a given bow type.
 * Note that this relies on the multiplier being the 2nd digit of the bow's
 * sval. We assume that sval has already been checked for legitimacy before
 * we get here.
 */
static int bow_multiplier(int sval)
{
    return sval - 10 * (sval / 10);
}


/*
 * Evaluate the object's overall power level.
 */
s32b object_power(struct player *p, const object_type* o_ptr)
{
    s32b pwr = 0, q = 0, slay_pwr = 0, dice_pwr = 0;
    unsigned int i, j;
    int extra_stat_bonus = 0, mult = 1, num_slays = 0, k = 1;
    bitflag flags[OF_SIZE], mask[OF_SIZE];
    int effect = 0;
    bool aware = object_flavor_is_aware(p, o_ptr);
    bool known = object_is_known(p, o_ptr);

    /* Zero the flag counts */
    for (i = 0; i < N_ELEMENTS(sets); i++) sets[i].count = 0;

    /* Extract the flags */
    if (known)
        object_flags(o_ptr, flags);
    else
        object_flags_known(o_ptr, flags, aware);

    /* Get the slay power and number of slay/brand types */
    create_mask(mask, FALSE, OFT_SLAY, OFT_KILL, OFT_BRAND, OFT_MAX);
    num_slays = list_slays(flags, mask, NULL, NULL, NULL, TRUE);
    if (num_slays) slay_pwr = slay_power(p, o_ptr, known, aware);

    /* Start with any damage boost from the item itself */
    pwr += (o_ptr->to_d * DAMAGE_POWER / 2);
    if (o_ptr->to_d >= INHIBIT_TO_DAM) return INHIBIT_POWER;

    /* Add damage from dice for any wieldable weapon or ammo */
    if ((wield_slot(p, o_ptr) == INVEN_WIELD) || obj_is_ammo(p, o_ptr))
        dice_pwr = (o_ptr->dd * (o_ptr->ds + 1) * DAMAGE_POWER / 4);

    /* Add 2nd lot of damage power for nonweapons */
    else if (wield_slot(p, o_ptr) != INVEN_BOW)
    {
        pwr += (o_ptr->to_d * DAMAGE_POWER);

        /* Add power boost for nonweapons with combat flags */
        if (num_slays || of_has(flags, OF_BLOWS) || of_has(flags, OF_SHOTS) || of_has(flags, OF_MIGHT))
            dice_pwr = (WEAP_DAMAGE * DAMAGE_POWER);
    }

    pwr += dice_pwr;

    /* Add ammo damage for launchers, get multiplier and rescale */
    if (wield_slot(p, o_ptr) == INVEN_BOW)
    {
        pwr += (archery[1 + o_ptr->sval / 10].ammo_dam * DAMAGE_POWER / 2);
        mult = bow_multiplier(o_ptr->sval);
    }

    /* Add launcher bonus for ego ammo, multiply for launcher and rescale */
    if (obj_is_ammo(p, o_ptr))
    {
        if (o_ptr->ego)
            pwr += (archery[o_ptr->tval - TV_ROCK].launch_dam * DAMAGE_POWER / 2);
        pwr = pwr * archery[o_ptr->tval - TV_ROCK].launch_mult / (2 * MAX_BLOWS);
    }

    /* Add power for extra blows */
    if (of_has(flags, OF_BLOWS))
    {
        j = which_pval(o_ptr, OF_BLOWS);
        if (known || object_this_pval_is_visible(o_ptr, j, aware))
        {
            /* Inhibit */
            if (o_ptr->pval[j] >= INHIBIT_BLOWS) return INHIBIT_POWER;

            pwr = pwr * (MAX_BLOWS + o_ptr->pval[j]) / MAX_BLOWS;

            /* Add boost for assumed off-weapon damage */
            pwr += (NONWEAP_DAMAGE * o_ptr->pval[j] * DAMAGE_POWER / 2);
        }
    }

    /* Add power for extra shots - note that we cannot handle negative shots */
    if (of_has(flags, OF_SHOTS))
    {
        j = which_pval(o_ptr, OF_SHOTS);
        if (known || object_this_pval_is_visible(o_ptr, j, aware))
        {
            /* Inhibit */
            if (o_ptr->pval[j] >= INHIBIT_SHOTS) return INHIBIT_POWER;

            if (o_ptr->pval[j] > 0)
                pwr = (pwr * (1 + o_ptr->pval[j]));
        }
    }

    /* Add power for extra might */
    if (of_has(flags, OF_MIGHT))
    {
        j = which_pval(o_ptr, OF_MIGHT);
        if (known || object_this_pval_is_visible(o_ptr, j, aware))
        {
            /* Inhibit */
            if (o_ptr->pval[j] >= INHIBIT_MIGHT) return INHIBIT_POWER;

            mult += o_ptr->pval[j];
        }
    }
    pwr *= mult;

    /* Apply the correct slay multiplier */
    if (slay_pwr) pwr += (dice_pwr * (slay_pwr / 100)) / (tot_mon_power / 100);

    /*
     * Melee weapons assume MAX_BLOWS per turn, so we must divide by MAX_BLOWS
     * to get equal ratings for launchers.
     */
    if (wield_slot(p, o_ptr) == INVEN_BOW) pwr /= MAX_BLOWS;

    /* Add power for +to_hit */
    pwr += (o_ptr->to_h * TO_HIT_POWER / 2);
    if (o_ptr->to_h >= INHIBIT_TO_HIT) return INHIBIT_POWER;

    /* Add power for base AC and adjust for weight */
    if (o_ptr->ac)
    {
        pwr += BASE_ARMOUR_POWER;
        q += (o_ptr->ac * BASE_AC_POWER / 2);

        /* Add power for AC per unit weight */
        if (o_ptr->weight > 0)
        {
            i = 750 * (o_ptr->ac + o_ptr->to_a) / o_ptr->weight;

            /* Avoid overpricing Elven Cloaks */
            if (i > 450) i = 450;

            q *= i;
            q /= 100;
        }

        /* Weightless (ethereal) armour items get fixed boost */
        else q *= 5;

        pwr += q;
    }

    /* Add power for +to_ac */
    pwr += (o_ptr->to_a * TO_AC_POWER / 2);
    if (o_ptr->to_a >= HIGH_TO_AC)
        pwr += ((o_ptr->to_a - (HIGH_TO_AC - 1)) * TO_AC_POWER);
    if (o_ptr->to_a >= VERYHIGH_TO_AC)
        pwr += ((o_ptr->to_a - (VERYHIGH_TO_AC -1)) * TO_AC_POWER * 2);
    if (o_ptr->to_a >= INHIBIT_AC) return INHIBIT_POWER;

    /* Add base power for jewelry */
    if (object_is_jewelry(o_ptr)) pwr += BASE_JEWELRY_POWER;

    /* PWMAngband - Add base power for tools */
    if (wield_slot(p, o_ptr) == INVEN_TOOL) pwr += BASE_TOOL_POWER;

    /* Add power for non-derived flags (derived flags have flag_power 0) */
    for (i = of_next(flags, FLAG_START); i != FLAG_END; i = of_next(flags, i + 1))
    {
        if (flag_uses_pval(i))
        {
            j = which_pval(o_ptr, i);
            if (known || object_this_pval_is_visible(o_ptr, j, aware))
            {
                k = o_ptr->pval[j];
                extra_stat_bonus += (k * pval_mult(i));
            }
        }
        else
            k = 1;

        if (flag_power(i))
            pwr += (k * flag_power(i) * slot_mult(i, wield_slot(p, o_ptr)));

        /* Track combinations of flag types - note we ignore SUST_CHR and ESP_POWER */
        for (j = 0; j < N_ELEMENTS(sets); j++)
        {
            if ((sets[j].type == obj_flag_type(i)) && (i != OF_SUST_CHR) && (i != OF_ESP_POWER))
                sets[j].count++;
        }
    }

    /* Add extra power term if there are a lot of ability bonuses */
    if (extra_stat_bonus > 249) return INHIBIT_POWER;
    if (extra_stat_bonus < 0) extra_stat_bonus = 0;
    pwr += ability_power[extra_stat_bonus / 10];

    /* Add extra power for multiple flags of the same type */
    for (i = 0; i < N_ELEMENTS(sets); i++)
    {
        if (sets[i].count > 1)
        {
            q = (sets[i].factor * sets[i].count * sets[i].count);

            /* Scale damage-dependent set bonuses by damage dice power */
            if (sets[i].dam_dep) q = q * dice_pwr / (DAMAGE_POWER * 5);

            pwr += q;
        }

        /* Add bonus if item has a full set of these flags */
        if (sets[i].count == sets[i].size) pwr += sets[i].bonus;
    }

    /* Get activation */
    object_activation(p, o_ptr, known, &effect);

    /* Add power for activation */
    if (effect && randcalc(o_ptr->time, 0, MAXIMISE))
        pwr += effect_power(effect);

    return pwr;
}
