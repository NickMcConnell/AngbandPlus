/*
 * File: obj-randart.c
 * Purpose: Random artifact generation
 *
 * Copyright (c) 1998 Greg Wooledge, Ben Harrison, Robert Ruhlmann
 * Copyright (c) 2001 Chris Carr, Chris Robertson
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
 * Original random artifact generator (randart) by Greg Wooledge.
 * Updated by Chris Carr / Chris Robertson 2001-2010.
 */


/* Arrays of indices by item type, used in frequency generation */
static s16b art_idx_bow[] =
{
    ART_IDX_BOW_SHOTS,
    ART_IDX_BOW_MIGHT,
    ART_IDX_BOW_BRAND,
    ART_IDX_BOW_SLAY
};

static s16b art_idx_weapon[] =
{
    ART_IDX_WEAPON_HIT,
    ART_IDX_WEAPON_DAM,
    ART_IDX_WEAPON_AGGR
};

static s16b art_idx_ammo[] =
{
    ART_IDX_AMMO_HIT,
    ART_IDX_AMMO_DAM
};

static s16b art_idx_nonweapon[] =
{
    ART_IDX_NONWEAPON_HIT,
    ART_IDX_NONWEAPON_DAM,
    ART_IDX_NONWEAPON_HIT_DAM,
    ART_IDX_NONWEAPON_AGGR,
    ART_IDX_NONWEAPON_BRAND,
    ART_IDX_NONWEAPON_SLAY,
    ART_IDX_NONWEAPON_BLOWS,
    ART_IDX_NONWEAPON_SHOTS
};

static s16b art_idx_melee[] =
{
    ART_IDX_MELEE_BLESS,
    ART_IDX_MELEE_BRAND,
    ART_IDX_MELEE_SLAY,
    ART_IDX_MELEE_SINV,
    ART_IDX_MELEE_BLOWS,
    ART_IDX_MELEE_AC,
    ART_IDX_MELEE_DICE,
    ART_IDX_MELEE_WEIGHT
};

static s16b art_idx_allarmor[] =
{
    ART_IDX_ALLARMOR_WEIGHT
};

static s16b art_idx_boot[] =
{
    ART_IDX_BOOT_AC,
    ART_IDX_BOOT_FEATHER,
    ART_IDX_BOOT_STEALTH,
    ART_IDX_BOOT_SPEED
};

static s16b art_idx_glove[] =
{
    ART_IDX_GLOVE_AC,
    ART_IDX_GLOVE_FA,
    ART_IDX_GLOVE_DEX,
    ART_IDX_GLOVE_MANA,
    ART_IDX_GLOVE_ID
};

static s16b art_idx_headgear[] =
{
    ART_IDX_HELM_AC,
    ART_IDX_HELM_RBLIND,
    ART_IDX_HELM_ESP,
    ART_IDX_HELM_SINV,
    ART_IDX_HELM_WIS,
    ART_IDX_HELM_INT,
    ART_IDX_HELM_ID
};

static s16b art_idx_shield[] =
{
    ART_IDX_SHIELD_AC,
    ART_IDX_SHIELD_LRES
};

static s16b art_idx_cloak[] =
{
    ART_IDX_CLOAK_AC,
    ART_IDX_CLOAK_STEALTH
};

static s16b art_idx_armor[] =
{
    ART_IDX_ARMOR_AC,
    ART_IDX_ARMOR_STEALTH,
    ART_IDX_ARMOR_HLIFE,
    ART_IDX_ARMOR_CON,
    ART_IDX_ARMOR_LRES,
    ART_IDX_ARMOR_ALLRES,
    ART_IDX_ARMOR_HRES
};

static s16b art_idx_digger[] =
{
    ART_IDX_DIGGER_TUNN
};

static s16b art_idx_mstaff[] =
{
    ART_IDX_MSTAFF_INT,
    ART_IDX_MSTAFF_SINV,
    ART_IDX_MSTAFF_ESP,
    ART_IDX_MSTAFF_FA,
    ART_IDX_MSTAFF_RBLIND,
    ART_IDX_MSTAFF_RCONF
};

static s16b art_idx_missile[] =
{
    ART_IDX_MISSILE_BRAND,
    ART_IDX_MISSILE_SLAY,
    ART_IDX_MISSILE_DICE
};

static s16b art_idx_gen[] =
{
    ART_IDX_GEN_STAT,
    ART_IDX_GEN_SUST,
    ART_IDX_GEN_STEALTH,
    ART_IDX_GEN_SEARCH,
    ART_IDX_GEN_INFRA,
    ART_IDX_GEN_SPEED,
    ART_IDX_GEN_IMMUNE,
    ART_IDX_GEN_FA,
    ART_IDX_GEN_HLIFE,
    ART_IDX_GEN_FEATHER,
    ART_IDX_GEN_LIGHT,
    ART_IDX_GEN_SINV,
    ART_IDX_GEN_ESP,
    ART_IDX_GEN_SDIG,
    ART_IDX_GEN_REGEN,
    ART_IDX_GEN_LRES,
    ART_IDX_GEN_RPOIS,
    ART_IDX_GEN_RFEAR,
    ART_IDX_GEN_RLIGHT,
    ART_IDX_GEN_RDARK,
    ART_IDX_GEN_RBLIND,
    ART_IDX_GEN_RCONF,
    ART_IDX_GEN_RSOUND,
    ART_IDX_GEN_RSHARD,
    ART_IDX_GEN_RNEXUS,
    ART_IDX_GEN_RNETHER,
    ART_IDX_GEN_RCHAOS,
    ART_IDX_GEN_RDISEN,
    ART_IDX_GEN_AC,
    ART_IDX_GEN_TUNN,
    ART_IDX_GEN_ACTIV,
    ART_IDX_GEN_PSTUN
};

static s16b art_idx_high_resist[] =
{
    ART_IDX_GEN_RPOIS,
    ART_IDX_GEN_RFEAR,
    ART_IDX_GEN_RLIGHT,
    ART_IDX_GEN_RDARK,
    ART_IDX_GEN_RBLIND,
    ART_IDX_GEN_RCONF,
    ART_IDX_GEN_RSOUND,
    ART_IDX_GEN_RSHARD,
    ART_IDX_GEN_RNEXUS,
    ART_IDX_GEN_RNETHER,
    ART_IDX_GEN_RCHAOS,
    ART_IDX_GEN_RDISEN,
    ART_IDX_GEN_PSTUN
};


/* Initialize the data structures for learned probabilities */
static s16b artprobs[ART_IDX_TOTAL];
static s16b art_bow_total = 0;
static s16b art_melee_total = 0;
static s16b art_boot_total = 0;
static s16b art_glove_total = 0;
static s16b art_headgear_total = 0;
static s16b art_shield_total = 0;
static s16b art_cloak_total = 0;
static s16b art_armor_total = 0;
static s16b art_mstaff_total = 0;
static s16b art_missile_total = 0;
static s16b art_other_total = 0;
static s16b art_total = 0;


/*
 * Working array for holding frequency values - global to avoid repeated
 * allocation of memory
 */
static s16b art_freq[ART_IDX_TOTAL];


/*
 * Mean start and increment values for to_hit, to_dam and AC.  Update these
 * if the algorithm changes.  They are used in frequency generation.
 */
static s16b mean_hit_increment = 4;
static s16b mean_dam_increment = 4;
static s16b mean_hit_startval = 10;
static s16b mean_dam_startval = 10;
static s16b mean_ac_startval = 15;
static s16b mean_ac_increment = 5;


/*
 * Store the original artifact power ratings
 */
static s32b *base_power;
static s16b max_power;
static s16b min_power;
static s16b avg_power;
static s16b var_power;


/* Activation list */
struct activation *activations;


/*** Wrapper functions for tvals (TODO: move this to obj-tval.c) ***/


static bool art_is_light(const struct artifact *art)
{
    return (art->tval == TV_LIGHT);
}


static bool art_is_ring(const struct artifact *art)
{
    return (art->tval == TV_RING);
}


static bool art_is_dark_sword(const struct artifact *art)
{
    return ((art->tval == TV_SWORD) && (art->sval == lookup_sval(art->tval, "Dark Sword")));
}


static bool art_is_pointy(const struct artifact *art)
{
    return ((art->tval == TV_SWORD) || (art->tval == TV_POLEARM));
}


static bool art_is_body_armor(const struct artifact *art)
{
    switch (art->tval)
    {
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
            return true;
        default:
            return false;
    }
}


static bool art_is_head_armor(const struct artifact *art)
{
    return ((art->tval == TV_HELM) || (art->tval == TV_CROWN));
}


static bool art_is_ammo(const struct artifact *art)
{
    switch (art->tval)
    {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_ROCK:
            return true;
        default:
            return false;
    }
}


static bool art_is_melee_weapon(const struct artifact *art)
{
    switch (art->tval)
    {
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
            return true;
        default:
            return false;
    }
}


static bool art_is_weapon(const struct artifact *art)
{
    return ((art->tval == TV_BOW) || art_is_melee_weapon(art));
}


static bool art_is_armor(const struct artifact *art)
{
    switch (art->tval)
    {
        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_BOOTS:
        case TV_GLOVES:
            return true;
        default:
            return false;
    }
}


static bool art_is_digger(const struct artifact *art)
{
    return (art->tval == TV_DIGGING);
}


static bool art_is_mstaff(const struct artifact *art)
{
    return (art->tval == TV_MSTAFF);
}


/*
 * Return the artifact power, by generating a "fake" object based on the
 * artifact, and calling the common object_power function
 */
static s32b artifact_power(struct artifact *art)
{
    struct object *fake = object_new();
    s32b power;

    if (!make_fake_artifact(fake, art)) return 0;

    power = object_power(NULL, fake);
    object_delete(&fake);

    return power;
}


/*
 * Store the original artifact power ratings as a baseline
 */
static void store_base_power(void)
{
    int i, j;
    int *fake_power;

    max_power = 0;
    min_power = 32767;
    var_power = 0;
    fake_power = mem_zalloc(z_info->a_max * sizeof(int));
    j = 0;

    for (i = 0; i < z_info->a_max; i++, j++)
    {
        base_power[i] = artifact_power(&a_info[i]);

        /* Capture power stats, ignoring cursed and uber arts */
        if ((base_power[i] > max_power) && (base_power[i] < INHIBIT_POWER))
            max_power = base_power[i];
        if ((base_power[i] < min_power) && (base_power[i] > 0))
            min_power = base_power[i];
        if ((base_power[i] > 0) && (base_power[i] < INHIBIT_POWER))
            fake_power[j] = (int)base_power[i];
        else j--;
    }

    avg_power = mean(fake_power, j);
    var_power = variance(fake_power, j);

    /* Store the number of different types, for use later */
    /* ToDo: replace this with full combination tracking */
    for (i = 0; i < z_info->a_max; i++)
    {
        switch (a_info[i].tval)
        {
            case TV_SWORD:
            case TV_POLEARM:
            case TV_HAFTED: art_melee_total++; break;
            case TV_BOW: art_bow_total++; break;
            case TV_SOFT_ARMOR:
            case TV_HARD_ARMOR:
            case TV_DRAG_ARMOR: art_armor_total++; break;
            case TV_SHIELD: art_shield_total++; break;
            case TV_CLOAK: art_cloak_total++; break;
            case TV_HELM:
            case TV_CROWN: art_headgear_total++; break;
            case TV_GLOVES: art_glove_total++; break;
            case TV_BOOTS: art_boot_total++; break;
            case TV_MSTAFF: art_mstaff_total++; break;
            case TV_ROCK:
            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT: art_missile_total++; break;
            case TV_NULL: break;
            default: art_other_total++;
        }
    }
    art_total = art_melee_total + art_bow_total + art_armor_total +
                art_shield_total + art_cloak_total + art_headgear_total +
                art_glove_total + art_boot_total + art_mstaff_total +
                art_missile_total + art_other_total;

    mem_free(fake_power);
}


/*
 * We've just added an ability which uses the mod bonus. Make sure it's
 * not zero. If it's currently negative, make it worse.
 */
static void do_mod(struct artifact *art, int mod)
{
    int factor = 1;

    /* Track whether we have blows, might or shots on this item */
    if (art->modifiers[OBJ_MOD_BLOWS] > 0) factor++;
    if (art->modifiers[OBJ_MOD_MIGHT] > 0) factor++;
    if (art->modifiers[OBJ_MOD_SHOTS] > 0) factor++;

    if (!art->modifiers[mod])
    {
        int value;

        /* Blows, might, shots handled separately */
        if (factor > 1)
        {
            value = randint1(2);

            /* Give it a shot at +3 */
            if (INHIBIT_STRONG) value = 3;
        }

        /* PWMAngband: mana handled separately */
        else if (mod == OBJ_MOD_MANA) value = 5 + randint0(6);

        /* Normal case */
        else value = randint1(4);

        art->modifiers[mod] = value;
    }
    else if (art->modifiers[mod] < 0)
    {
        if (one_in_(2)) art->modifiers[mod]--;
    }
    else if (one_in_(art->modifiers[mod] * factor))
    {
        /*
         * Made this a bit rarer and diminishing with higher mod -
         * also rarer if item has blows/might/shots already
         */
        art->modifiers[mod]++;
    }
}


/*
 * Clean up the artifact by removing illogical combinations of powers.
 */
static void remove_contradictory(struct artifact *art)
{
    if (of_has(art->flags, OF_AGGRAVATE))
        art->modifiers[OBJ_MOD_STEALTH] = 0;

    if (art->modifiers[OBJ_MOD_STR] < 0) of_off(art->flags, OF_SUST_STR);
    if (art->modifiers[OBJ_MOD_INT] < 0) of_off(art->flags, OF_SUST_INT);
    if (art->modifiers[OBJ_MOD_WIS] < 0) of_off(art->flags, OF_SUST_WIS);
    if (art->modifiers[OBJ_MOD_DEX] < 0) of_off(art->flags, OF_SUST_DEX);
    if (art->modifiers[OBJ_MOD_CON] < 0) of_off(art->flags, OF_SUST_CON);
    if (art->modifiers[OBJ_MOD_BLOWS] < 0) art->modifiers[OBJ_MOD_BLOWS] = 0;

    if (of_has(art->flags, OF_LIGHT_CURSE)) of_off(art->flags, OF_BLESSED);
    if (of_has(art->flags, OF_DRAIN_EXP)) of_off(art->flags, OF_HOLD_LIFE);

    /*
     * ESP evil bypasses ESP undead/demon
     *
     * Note: although "monster" orcs/trolls/giants/dragons are evil, ESP evil cannot
     * bypass ESP orc/troll/giant/dragon because of the corresponding player
     * races (a player of the Half-Orc race can be detected by ESP orc, but
     * not by ESP evil)
     */
    if (of_has(art->flags, OF_ESP_EVIL))
    {
        of_off(art->flags, OF_ESP_UNDEAD);
        of_off(art->flags, OF_ESP_DEMON);
    }

    /* ESP all bypasses all other ESPs */
    if (of_has(art->flags, OF_ESP_ALL))
    {
        bitflag f2[OF_SIZE];

        create_mask(f2, false, OFT_ESP, OFT_MAX);
        of_diff(art->flags, f2);
        of_on(art->flags, OF_ESP_ALL);
    }
}


/*
 * Adjust the parsed frequencies for any peculiarities of the
 * algorithm.  For example, if stat bonuses and sustains are
 * being added in a correlated fashion, it will tend to push
 * the frequencies up for both of them.  In this method we
 * compensate for cases like this by applying corrective
 * scaling.
 */
static void adjust_freqs(void)
{
    /*
     * Enforce minimum values for any frequencies that might potentially
     * be missing in the standard set, especially supercharged ones.
     * Numbers here represent the average number of times this ability
     * would appear if the entire randart set was eligible to receive
     * it (so in the case of a bow ability: if the set was all bows).
     *
     * Note that low numbers here for very specialized abilities could
     * mean that there's a good chance this ability will not appear in
     * a given randart set.  If this is a problem, raise the number.
     */
    if (artprobs[ART_IDX_GEN_RFEAR] < 5)
        artprobs[ART_IDX_GEN_RFEAR] = 5;
    if (artprobs[ART_IDX_MELEE_DICE_SUPER] < 5)
        artprobs[ART_IDX_MELEE_DICE_SUPER] = 5;
    if (artprobs[ART_IDX_BOW_SHOTS_SUPER] < 5)
        artprobs[ART_IDX_BOW_SHOTS_SUPER] = 5;
    if (artprobs[ART_IDX_BOW_MIGHT_SUPER] < 5)
        artprobs[ART_IDX_BOW_MIGHT_SUPER] = 5;
    if (artprobs[ART_IDX_MELEE_BLOWS_SUPER] < 5)
        artprobs[ART_IDX_MELEE_BLOWS_SUPER] = 5;
    if (artprobs[ART_IDX_GEN_SPEED_SUPER] < 5)
        artprobs[ART_IDX_GEN_SPEED_SUPER] = 5;
    if (artprobs[ART_IDX_GEN_AC] < 5)
        artprobs[ART_IDX_GEN_AC] = 5;
    if (artprobs[ART_IDX_GEN_TUNN] < 5)
        artprobs[ART_IDX_GEN_TUNN] = 5;
    if (artprobs[ART_IDX_NONWEAPON_BRAND] < 2)
        artprobs[ART_IDX_NONWEAPON_BRAND] = 2;
    if (artprobs[ART_IDX_NONWEAPON_SLAY] < 2)
        artprobs[ART_IDX_NONWEAPON_SLAY] = 2;
    if (artprobs[ART_IDX_BOW_BRAND] < 2)
        artprobs[ART_IDX_BOW_BRAND] = 2;
    if (artprobs[ART_IDX_BOW_SLAY] < 2)
        artprobs[ART_IDX_BOW_SLAY] = 2;
    if (artprobs[ART_IDX_NONWEAPON_BLOWS] < 2)
        artprobs[ART_IDX_NONWEAPON_BLOWS] = 2;
    if (artprobs[ART_IDX_NONWEAPON_SHOTS] < 2)
        artprobs[ART_IDX_NONWEAPON_SHOTS] = 2;
    if (artprobs[ART_IDX_GEN_AC_SUPER] < 5)
        artprobs[ART_IDX_GEN_AC_SUPER] = 5;
    if (artprobs[ART_IDX_MELEE_AC] < 5)
        artprobs[ART_IDX_MELEE_AC] = 5;
    if (artprobs[ART_IDX_GEN_PSTUN] < 3)
        artprobs[ART_IDX_GEN_PSTUN] = 3;

    /* Cut aggravation frequencies in half since they're used twice */
    artprobs[ART_IDX_NONWEAPON_AGGR] /= 2;
    artprobs[ART_IDX_WEAPON_AGGR] /= 2;

    /* PWMAngband */
    artprobs[ART_IDX_DIGGER_TUNN] = 4;
    artprobs[ART_IDX_MSTAFF_FA] = 10;
    artprobs[ART_IDX_MSTAFF_RBLIND] = 10;
    artprobs[ART_IDX_MSTAFF_RCONF] = 10;
    if (artprobs[ART_IDX_GLOVE_MANA] < 5)
        artprobs[ART_IDX_GLOVE_MANA] = 5;
    if (artprobs[ART_IDX_GLOVE_ID] < 5)
        artprobs[ART_IDX_GLOVE_ID] = 5;
    if (artprobs[ART_IDX_HELM_ID] < 5)
        artprobs[ART_IDX_HELM_ID] = 5;
    if (artprobs[ART_IDX_MISSILE_DICE_SUPER] < 5)
        artprobs[ART_IDX_MISSILE_DICE_SUPER] = 5;
}


static void artifact_count_slays(const struct artifact *art, int idx_brand, int idx_slay)
{
    /* Count brands and slays */
    if (art->slays)
        artprobs[idx_slay] += slay_count(art->slays);
    if (art->brands)
        artprobs[idx_brand] += brand_count(art->brands);
}


/*
 * Parse the list of artifacts and count up the frequencies of the various
 * abilities. This is used to give dynamic generation probabilities.
 */
static void parse_frequencies(void)
{
    size_t i;
    const struct artifact *art;
    struct object_kind *kind;
    s32b temp, temp2;
    bitflag f2[OF_SIZE];

    create_mask(f2, false, OFT_ESP, OFT_MAX);

    /* Zero the frequencies for artifact attributes */
    for (i = 0; i < ART_IDX_TOTAL; i++) artprobs[i] = 0;

    /* Go through the list of all artifacts */
    for (i = 0; i < (size_t)z_info->a_max; i++)
    {
        art = &a_info[i];

        /* Don't parse cursed or null items */
        if ((base_power[i] < 0) || (art->tval == 0)) continue;

        /* Get a pointer to the base item for this artifact */
        kind = lookup_kind(art->tval, art->sval);

        /* Special cases -- don't parse these! */
        if (strstr(art->name, "The One Ring") || kf_has(kind->kind_flags, KF_QUEST_ART) ||
            strstr(art->name, "of Maglor"))
            continue;

        /* Count up the abilities for this artifact */
        if (art->tval == TV_BOW)
        {
            if (art->modifiers[OBJ_MOD_SHOTS] > 0)
            {
                /* Do we have 3 or more extra shots? (Unlikely) */
                if (art->modifiers[OBJ_MOD_SHOTS] > 2)
                    artprobs[ART_IDX_BOW_SHOTS_SUPER]++;
                else artprobs[ART_IDX_BOW_SHOTS]++;
            }

            if (art->modifiers[OBJ_MOD_MIGHT] > 0)
            {
                /* Do we have 3 or more extra might? (Unlikely) */
                if (art->modifiers[OBJ_MOD_MIGHT] > 2)
                    artprobs[ART_IDX_BOW_MIGHT_SUPER]++;
                else artprobs[ART_IDX_BOW_MIGHT]++;
            }

            /* Brands or slays - count all together */
            artifact_count_slays(art, ART_IDX_BOW_BRAND, ART_IDX_BOW_SLAY);
        }

        /* Handle hit / dam ratings - are they higher than normal? */
        /* Also handle other weapon/nonweapon abilities */
        if (art_is_weapon(art))
        {
            temp = art->to_h - randcalc(kind->to_h, 0, MINIMISE) - mean_hit_startval;
            temp = temp / mean_hit_increment;
            artprobs[ART_IDX_WEAPON_HIT] += temp;

            temp = art->to_d - randcalc(kind->to_d, 0, MINIMISE) - mean_dam_startval;
            temp = temp / mean_dam_increment;
            artprobs[ART_IDX_WEAPON_DAM] += temp;

            /* Aggravation */
            if (of_has(art->flags, OF_AGGRAVATE)) artprobs[ART_IDX_WEAPON_AGGR]++;
        }
        else if (art_is_ammo(art))
        {
            temp = art->to_h - randcalc(kind->to_h, 0, MINIMISE);
            if (temp > 0)
            {
                temp = temp / mean_hit_increment;
                if (temp > 0) artprobs[ART_IDX_AMMO_HIT] += temp;
            }
            temp = art->to_d - randcalc(kind->to_d, 0, MINIMISE);
            if (temp > 0)
            {
                temp = temp / mean_dam_increment;
                if (temp > 0) artprobs[ART_IDX_AMMO_DAM] += temp;
            }
        }
        else
        {
            temp = art->to_h - randcalc(kind->to_h, 0, MINIMISE);
            temp2 = art->to_d - randcalc(kind->to_d, 0, MINIMISE);
            if ((temp > 0) && (temp == temp2))
            {
                /* Special case: both hit and dam bonuses present and equal */
                temp = 2 * temp / (mean_hit_increment + mean_dam_increment);
                if (temp > 0) artprobs[ART_IDX_NONWEAPON_HIT_DAM] += temp;
            }
            else
            {
                /* Uneven bonuses - handle separately */
                if (temp > 0)
                {
                    temp = temp / mean_hit_increment;
                    if (temp > 0) artprobs[ART_IDX_NONWEAPON_HIT] += temp;
                }
                if (temp2 > 0)
                {
                    temp = temp2 / mean_dam_increment;
                    if (temp > 0) artprobs[ART_IDX_NONWEAPON_DAM] += temp;
                }
            }

            /* Aggravation */
            if (of_has(art->flags, OF_AGGRAVATE)) artprobs[ART_IDX_NONWEAPON_AGGR]++;

            /* Brands or slays - count all together */
            artifact_count_slays(art, ART_IDX_NONWEAPON_BRAND, ART_IDX_NONWEAPON_SLAY);

            /* Adding 1 for extra blows on nonweapon */
            if (art->modifiers[OBJ_MOD_BLOWS] > 0)
                artprobs[ART_IDX_NONWEAPON_BLOWS]++;

            /* Adding 1 for extra shots on nonweapon */
            if (art->modifiers[OBJ_MOD_SHOTS] > 0)
                artprobs[ART_IDX_NONWEAPON_SHOTS]++;
        }

        if (art_is_melee_weapon(art))
        {
            /* Blessed weapon? */
            if (of_has(art->flags, OF_BLESSED)) artprobs[ART_IDX_MELEE_BLESS]++;

            /* See invisible? */
            if (of_has(art->flags, OF_SEE_INVIS)) artprobs[ART_IDX_MELEE_SINV]++;

            /* Does this weapon have extra blows? */
            if (art->modifiers[OBJ_MOD_BLOWS] > 0)
            {
                /* Do we have 3 or more extra blows? (Unlikely) */
                if (art->modifiers[OBJ_MOD_BLOWS] > 2)
                    artprobs[ART_IDX_MELEE_BLOWS_SUPER]++;
                else
                    artprobs[ART_IDX_MELEE_BLOWS]++;
            }

            /* Does this weapon have an unusual bonus to AC? */
            if ((art->to_a - randcalc(kind->to_a, 0, MAXIMISE)) > 0)
            {
                temp = (art->to_a - randcalc(kind->to_a, 0, MAXIMISE)) /
                    mean_ac_increment;
                if (temp > 0) artprobs[ART_IDX_MELEE_AC] += temp;
            }

            /* Check damage dice - are they more than normal? */
            if (art->dd > kind->dd)
            {
                /* Difference of 3 or more? */
                if ((art->dd - kind->dd) > 2)
                    artprobs[ART_IDX_MELEE_DICE_SUPER]++;
                else
                    artprobs[ART_IDX_MELEE_DICE]++;
            }

            /* Check weight - is it different from normal? */
            if (art->weight != kind->weight) artprobs[ART_IDX_MELEE_WEIGHT]++;

            /* Brands or slays - count all together */
            artifact_count_slays(art, ART_IDX_MELEE_BRAND, ART_IDX_MELEE_SLAY);
        }

        if (art_is_ammo(art))
        {
            /* Brands or slays - count all together */
            artifact_count_slays(art, ART_IDX_MISSILE_BRAND, ART_IDX_MISSILE_SLAY);

            /* Check damage dice - are they more than normal? */
            if (art->dd > kind->dd)
            {
                /* Difference of 2 or more? */
                if ((art->dd - kind->dd) > 1)
                    artprobs[ART_IDX_MISSILE_DICE_SUPER]++;
                else
                    artprobs[ART_IDX_MISSILE_DICE]++;
            }
        }

        /* Check for tunnelling ability */
        if (art->modifiers[OBJ_MOD_TUNNEL] > 0) artprobs[ART_IDX_GEN_TUNN]++;

        /*
         * Count up extra AC bonus values.
         * Could also add logic to subtract for lower values here, but it's
         * probably not worth the trouble since it's so rare.
         */
        temp = art->to_a - randcalc(kind->to_a, 0, MINIMISE) - mean_ac_startval;
        if (temp > 0)
        {
            temp = temp / mean_ac_increment;
            if (temp > 0)
            {
                if (art->to_a > 20)
                    artprobs[ART_IDX_GEN_AC_SUPER]++;
                else if (art->tval == TV_BOOTS)
                    artprobs[ART_IDX_BOOT_AC] += temp;
                else if (art->tval == TV_GLOVES)
                    artprobs[ART_IDX_GLOVE_AC] += temp;
                else if (art_is_head_armor(art))
                    artprobs[ART_IDX_HELM_AC] += temp;
                else if (art->tval == TV_SHIELD)
                    artprobs[ART_IDX_SHIELD_AC] += temp;
                else if (art->tval == TV_CLOAK)
                    artprobs[ART_IDX_CLOAK_AC] += temp;
                else if (art_is_body_armor(art))
                    artprobs[ART_IDX_ARMOR_AC] += temp;
                else
                    artprobs[ART_IDX_GEN_AC] += temp;
            }
        }

        /* Generic armor abilities */
        if (art_is_armor(art))
        {
            /* Check weight - is it different from normal? */
            /* ToDo: count higher and lower separately */
            if (art->weight != kind->weight) artprobs[ART_IDX_ALLARMOR_WEIGHT]++;
        }

        /*
         * General abilities.  This section requires a bit more work
         * than the others, because we have to consider cases where
         * a certain ability might be found in a particular item type.
         * For example, ESP is commonly found on headgear, so when
         * we count ESP we must add it to either the headgear or
         * general tally, depending on the base item. This permits
         * us to have general abilities appear more commonly on a
         * certain item type.
         */

        /* Stat bonus case.  Add up the number of individual bonuses */
        temp = 0;
        if (art->modifiers[OBJ_MOD_STR] > 0) temp++;
        if (art->modifiers[OBJ_MOD_INT] > 0) temp++;
        if (art->modifiers[OBJ_MOD_WIS] > 0) temp++;
        if (art->modifiers[OBJ_MOD_DEX] > 0) temp++;
        if (art->modifiers[OBJ_MOD_CON] > 0) temp++;

        /* Handle a few special cases separately. */
        if (art_is_head_armor(art) &&
            ((art->modifiers[OBJ_MOD_WIS] > 0) || (art->modifiers[OBJ_MOD_INT] > 0)))
        {
            /* Handle WIS and INT on helms and crowns */
            if (art->modifiers[OBJ_MOD_WIS] > 0)
            {
                artprobs[ART_IDX_HELM_WIS]++;

                /* Counted this one separately so subtract it here */
                temp--;
            }
            if (art->modifiers[OBJ_MOD_INT] > 0)
            {
                artprobs[ART_IDX_HELM_INT]++;

                /* Counted this one separately so subtract it here */
                temp--;
            }
        }
        else if (art_is_body_armor(art) && (art->modifiers[OBJ_MOD_CON] > 0))
        {
            /* Handle CON bonus on armor */
            artprobs[ART_IDX_ARMOR_CON]++;

            /* Counted this one separately so subtract it here */
            temp--;
        }
        else if ((art->tval == TV_GLOVES) && (art->modifiers[OBJ_MOD_DEX] > 0))
        {
            /* Handle DEX bonus on gloves */
            artprobs[ART_IDX_GLOVE_DEX]++;

            /* Counted this one separately so subtract it here */
            temp--;
        }
        else if (art_is_mstaff(art) && (art->modifiers[OBJ_MOD_INT] > 0))
        {
            /* Handle INT bonus on mage staves */
            artprobs[ART_IDX_MSTAFF_INT]++;

            /* Counted this one separately so subtract it here */
            temp--;
        }

        /* Now the general case */
        if (temp > 0)
        {
            /* There are some bonuses that weren't handled above */
            artprobs[ART_IDX_GEN_STAT] += temp;
        }

        if (flags_test(art->flags, OF_SIZE, OF_SUST_STR, OF_SUST_INT, OF_SUST_WIS,
            OF_SUST_DEX, OF_SUST_CON, FLAG_END))
        {
            /* Now do sustains, in a similar manner */
            temp = 0;
            if (of_has(art->flags, OF_SUST_STR)) temp++;
            if (of_has(art->flags, OF_SUST_INT)) temp++;
            if (of_has(art->flags, OF_SUST_WIS)) temp++;
            if (of_has(art->flags, OF_SUST_DEX)) temp++;
            if (of_has(art->flags, OF_SUST_CON)) temp++;
            artprobs[ART_IDX_GEN_SUST] += temp;
        }

        if ((art->tval == TV_GLOVES) && (art->modifiers[OBJ_MOD_MANA] > 0))
        {
            /* Handle mana bonus on gloves */
            artprobs[ART_IDX_GLOVE_MANA]++;
        }

        if (art->modifiers[OBJ_MOD_STEALTH] > 0)
        {
            /* Handle stealth, including a couple of special cases */
            if (art->tval == TV_BOOTS)
                artprobs[ART_IDX_BOOT_STEALTH]++;
            else if (art->tval == TV_CLOAK)
                artprobs[ART_IDX_CLOAK_STEALTH]++;
            else if (art_is_body_armor(art))
                artprobs[ART_IDX_ARMOR_STEALTH]++;
            else
            {
                /* General case */
                artprobs[ART_IDX_GEN_STEALTH]++;
            }
        }

        if (art->modifiers[OBJ_MOD_SEARCH] > 0)
        {
            /* Handle searching bonus - fully generic this time */
            artprobs[ART_IDX_GEN_SEARCH]++;
        }

        if (art->modifiers[OBJ_MOD_INFRA] > 0)
        {
            /* Handle infravision bonus - fully generic */
            artprobs[ART_IDX_GEN_INFRA]++;
        }

        if (art->modifiers[OBJ_MOD_SPEED] > 0)
        {
            /*
             * Speed - boots handled separately.
             * This is something of a special case in that we use the same
             * frequency for the supercharged value and the normal value.
             * We get away with this by using a somewhat lower average value
             * for the supercharged ability than in the basic set (around
             * +7 or +8 - c.f. Ringil and the others at +10 and upwards).
             * This then allows us to add an equal number of
             * small bonuses around +3 or so without unbalancing things.
             */
            if (art->modifiers[OBJ_MOD_SPEED] > 7)
            {
                /* Supercharge case */
                artprobs[ART_IDX_GEN_SPEED_SUPER]++;
            }
            else if (art->tval == TV_BOOTS)
            {
                /* Handle boots separately */
                artprobs[ART_IDX_BOOT_SPEED]++;
            }
            else
                artprobs[ART_IDX_GEN_SPEED]++;
        }

        /* Count up immunities for this item, if any */
        temp = 0;
        if (art->el_info[ELEM_ACID].res_level == 3) temp++;
        if (art->el_info[ELEM_ELEC].res_level == 3) temp++;
        if (art->el_info[ELEM_FIRE].res_level == 3) temp++;
        if (art->el_info[ELEM_COLD].res_level == 3) temp++;
        artprobs[ART_IDX_GEN_IMMUNE] += temp;

        if (of_has(art->flags, OF_FREE_ACT))
        {
            /* Free action - handle gloves separately */
            if (art->tval == TV_GLOVES)
                artprobs[ART_IDX_GLOVE_FA]++;
            else
                artprobs[ART_IDX_GEN_FA]++;
        }

        if (of_has(art->flags, OF_HOLD_LIFE))
        {
            /* Hold life - do body armor separately */
            if (art_is_body_armor(art))
                artprobs[ART_IDX_ARMOR_HLIFE]++;
            else
                artprobs[ART_IDX_GEN_HLIFE]++;
        }

        if (of_has(art->flags, OF_FEATHER))
        {
            /* Feather fall - handle boots separately */
            if (art->tval == TV_BOOTS)
                artprobs[ART_IDX_BOOT_FEATHER]++;
            else
                artprobs[ART_IDX_GEN_FEATHER]++;
        }

        if (art->modifiers[OBJ_MOD_LIGHT] > 0)
        {
            /* Handle permanent light */
            artprobs[ART_IDX_GEN_LIGHT]++;
        }

        if (of_has(art->flags, OF_SEE_INVIS))
        {
            /*
             * Handle see invisible - do helms / crowns separately
             * (Weapons were done already so exclude them)
             */
            if (!art_is_melee_weapon(art))
            {
                if (art_is_head_armor(art))
                    artprobs[ART_IDX_HELM_SINV]++;
                else if (art_is_mstaff(art))
                    artprobs[ART_IDX_MSTAFF_SINV]++;
                else
                    artprobs[ART_IDX_GEN_SINV]++;
            }
        }

        if (of_is_inter(art->flags, f2))
        {
            /* ESP case. Add up the number of individual bonuses */
            temp = 0;
            if (of_has(art->flags, OF_ESP_ORC)) temp++;
            if (of_has(art->flags, OF_ESP_TROLL)) temp++;
            if (of_has(art->flags, OF_ESP_GIANT)) temp++;
            if (of_has(art->flags, OF_ESP_ANIMAL)) temp++;
            if (of_has(art->flags, OF_ESP_DRAGON)) temp++;
            if (of_has(art->flags, OF_ESP_DEMON)) temp++;
            if (of_has(art->flags, OF_ESP_UNDEAD)) temp++;
            if (of_has(art->flags, OF_ESP_EVIL)) temp++;
            if (of_has(art->flags, OF_ESP_ALL)) temp++;
            if (of_has(art->flags, OF_ESP_RADIUS)) temp++;

            /* Handle helms/crowns separately. */
            if (art_is_head_armor(art))
                artprobs[ART_IDX_HELM_ESP] += temp;
            else if (art_is_mstaff(art))
                artprobs[ART_IDX_MSTAFF_ESP] += temp;
            else
                artprobs[ART_IDX_GEN_ESP] += temp;
        }

        if (of_has(art->flags, OF_SLOW_DIGEST))
        {
            /* Slow digestion case - generic. */
            artprobs[ART_IDX_GEN_SDIG]++;
        }

        if (of_has(art->flags, OF_REGEN))
        {
            /* Regeneration case - generic. */
            artprobs[ART_IDX_GEN_REGEN]++;
        }

        if (of_has(art->flags, OF_KNOWLEDGE))
        {
            /* Handle helms/crowns separately. */
            if (art_is_head_armor(art))
                artprobs[ART_IDX_HELM_ID]++;
            else if (art->tval == TV_GLOVES)
            {
                /* Handle knowledge bonus on gloves */
                artprobs[ART_IDX_GLOVE_ID]++;
            }
        }

        /* Count up low resists (not the type, just the number) */
        temp = 0;
        if (art->el_info[ELEM_ACID].res_level == 1) temp++;
        if (art->el_info[ELEM_ELEC].res_level == 1) temp++;
        if (art->el_info[ELEM_FIRE].res_level == 1) temp++;
        if (art->el_info[ELEM_COLD].res_level == 1) temp++;
        artprobs[ART_IDX_GEN_IMMUNE] += temp;

        if (temp)
        {
            /* Shields treated separately */
            if (art->tval == TV_SHIELD)
                artprobs[ART_IDX_SHIELD_LRES] += temp;

            /* Armor also treated separately */
            else if (art_is_body_armor(art))
            {
                /* Special case: armor has all four low resists */
                if (temp == 4)
                    artprobs[ART_IDX_ARMOR_ALLRES]++;

                /* Just tally up the resists as usual */
                else
                    artprobs[ART_IDX_ARMOR_LRES] += temp;
            }

            /* General case */
            else
                artprobs[ART_IDX_GEN_LRES] += temp;
        }

        /*
         * If the item is body armor then count up all the high resists before
         * going through them individually.  High resists are an important
         * component of body armor so we track probability for them separately.
         * The proportions of the high resists will be determined by the
         * generic frequencies - this number just tracks the total.
         */
        if (art_is_body_armor(art))
        {
            temp = 0;
            if (art->el_info[ELEM_POIS].res_level == 1) temp++;
            if (of_has(art->flags, OF_PROT_FEAR)) temp++;
            if (art->el_info[ELEM_LIGHT].res_level == 1) temp++;
            if (art->el_info[ELEM_DARK].res_level == 1) temp++;
            if (of_has(art->flags, OF_PROT_BLIND)) temp++;
            if (of_has(art->flags, OF_PROT_CONF)) temp++;
            if (art->el_info[ELEM_SOUND].res_level == 1) temp++;
            if (art->el_info[ELEM_SHARD].res_level == 1) temp++;
            if (art->el_info[ELEM_NEXUS].res_level == 1) temp++;
            if (art->el_info[ELEM_NETHER].res_level == 1) temp++;
            if (art->el_info[ELEM_CHAOS].res_level == 1) temp++;
            if (art->el_info[ELEM_DISEN].res_level == 1) temp++;
            if (of_has(art->flags, OF_PROT_STUN)) temp++;
            artprobs[ART_IDX_ARMOR_HRES] += temp;
        }

        /* Now do the high resists individually */
        if (art->el_info[ELEM_POIS].res_level == 1)
        {
            /* Resist poison ability */
            artprobs[ART_IDX_GEN_RPOIS]++;
        }

        if (of_has(art->flags, OF_PROT_FEAR))
        {
            /* Resist fear ability */
            artprobs[ART_IDX_GEN_RFEAR]++;
        }

        if (art->el_info[ELEM_LIGHT].res_level == 1)
        {
            /* Resist light ability */
            artprobs[ART_IDX_GEN_RLIGHT]++;
        }

        if (art->el_info[ELEM_DARK].res_level == 1)
        {
            /* Resist dark ability */
            artprobs[ART_IDX_GEN_RDARK]++;
        }

        if (of_has(art->flags, OF_PROT_BLIND))
        {
            /* Resist blind ability - helms/crowns are separate */
            if (art_is_head_armor(art))
                artprobs[ART_IDX_HELM_RBLIND]++;
            else
            {
                /* General case */
                artprobs[ART_IDX_GEN_RBLIND]++;
            }
        }

        if (of_has(art->flags, OF_PROT_CONF))
        {
            /* Resist confusion ability */
            artprobs[ART_IDX_GEN_RCONF]++;
        }

        if (art->el_info[ELEM_SOUND].res_level == 1)
        {
            /* Resist sound ability */
            artprobs[ART_IDX_GEN_RSOUND]++;
        }

        if (art->el_info[ELEM_SHARD].res_level == 1)
        {
            /* Resist shards ability */
            artprobs[ART_IDX_GEN_RSHARD]++;
        }

        if (art->el_info[ELEM_NEXUS].res_level == 1)
        {
            /* Resist nexus ability */
            artprobs[ART_IDX_GEN_RNEXUS]++;
        }

        if (art->el_info[ELEM_NETHER].res_level == 1)
        {
            /* Resist nether ability */
            artprobs[ART_IDX_GEN_RNETHER]++;
        }

        if (art->el_info[ELEM_CHAOS].res_level == 1)
        {
            /* Resist chaos ability */
            artprobs[ART_IDX_GEN_RCHAOS]++;
        }

        if (art->el_info[ELEM_DISEN].res_level == 1)
        {
            /* Resist disenchantment ability */
            artprobs[ART_IDX_GEN_RDISEN]++;
        }

        if (of_has(art->flags, OF_PROT_STUN))
        {
            /* Resist stunning ability */
            artprobs[ART_IDX_GEN_PSTUN]++;
        }

        if (art->activation)
        {
            /* Activation */
            artprobs[ART_IDX_GEN_ACTIV]++;
        }
    }

    /*
     * Rescale the abilities so that dependent / independent abilities are
     * comparable.  We do this by rescaling the frequencies for item-dependent
     * abilities as though the entire set was made up of that item type.  For
     * example, if one bow out of three has extra might, and there are 120
     * artifacts in the full set, we rescale the frequency for extra might to
     * 40 (if we had 120 randart bows, about 40 would have extra might).
     *
     * This will allow us to compare the frequencies of all ability types,
     * no matter what the dependency.  We assume that generic abilities (like
     * resist fear in the current version) don't need rescaling.  This
     * introduces some inaccuracy in cases where specific instances of an
     * ability (like INT bonus on helms) have been counted separately -
     * ideally we should adjust for this in the general case.  However, as
     * long as this doesn't occur too often, it shouldn't be a big issue.
     *
     * The following loops look complicated, but they are simply equivalent
     * to going through each of the relevant ability types one by one.
     */

    /* Bow-only abilities */
    for (i = 0; i < N_ELEMENTS(art_idx_bow); i++)
        artprobs[art_idx_bow[i]] = (artprobs[art_idx_bow[i]] * art_total) / art_bow_total;

    /* All weapon abilities */
    for (i = 0; i < N_ELEMENTS(art_idx_weapon); i++)
    {
        artprobs[art_idx_weapon[i]] = (artprobs[art_idx_weapon[i]] * art_total) /
            (art_bow_total + art_melee_total);
    }

    /* Corresponding ammo abilities */
    for (i = 0; i < N_ELEMENTS(art_idx_ammo); i++)
        artprobs[art_idx_ammo[i]] = (artprobs[art_idx_ammo[i]] * art_total) / art_missile_total;

    /* Corresponding non-weapon abilities */
    temp = art_total - art_melee_total - art_bow_total - art_missile_total;
    for (i = 0; i < N_ELEMENTS(art_idx_nonweapon); i++)
        artprobs[art_idx_nonweapon[i]] = (artprobs[art_idx_nonweapon[i]] * art_total) / temp;

    /* All melee weapon abilities */
    for (i = 0; i < N_ELEMENTS(art_idx_melee); i++)
        artprobs[art_idx_melee[i]] = (artprobs[art_idx_melee[i]] * art_total) / art_melee_total;

    /* All general armor abilities */
    temp = art_armor_total + art_boot_total + art_shield_total + art_headgear_total +
        art_cloak_total + art_glove_total;
    for (i = 0; i < N_ELEMENTS(art_idx_allarmor); i++)
        artprobs[art_idx_allarmor[i]] = (artprobs[art_idx_allarmor[i]] * art_total) / temp;

    /* Boots */
    for (i = 0; i < N_ELEMENTS(art_idx_boot); i++)
        artprobs[art_idx_boot[i]] = (artprobs[art_idx_boot[i]] * art_total) / art_boot_total;

    /* Gloves */
    for (i = 0; i < N_ELEMENTS(art_idx_glove); i++)
        artprobs[art_idx_glove[i]] = (artprobs[art_idx_glove[i]] * art_total) / art_glove_total;

    /* Headgear */
    for (i = 0; i < N_ELEMENTS(art_idx_headgear); i++)
    {
        artprobs[art_idx_headgear[i]] = (artprobs[art_idx_headgear[i]] * art_total) /
            art_headgear_total;
    }

    /* Shields */
    for (i = 0; i < N_ELEMENTS(art_idx_shield); i++)
        artprobs[art_idx_shield[i]] = (artprobs[art_idx_shield[i]] * art_total) / art_shield_total;

    /* Cloaks */
    for (i = 0; i < N_ELEMENTS(art_idx_cloak); i++)
        artprobs[art_idx_cloak[i]] = (artprobs[art_idx_cloak[i]] * art_total) / art_cloak_total;

    /* Body armor */
    for (i = 0; i < N_ELEMENTS(art_idx_armor); i++)
        artprobs[art_idx_armor[i]] = (artprobs[art_idx_armor[i]] * art_total) / art_armor_total;

    /* Mage staves */
    for (i = 0; i < N_ELEMENTS(art_idx_mstaff); i++)
        artprobs[art_idx_mstaff[i]] = (artprobs[art_idx_mstaff[i]] * art_total) / art_mstaff_total;

    /* Missiles */
    for (i = 0; i < N_ELEMENTS(art_idx_missile); i++)
        artprobs[art_idx_missile[i]] = (artprobs[art_idx_missile[i]] * art_total) / art_missile_total;

    /*
     * All others are general case and don't need to be rescaled,
     * unless the algorithm is getting too clever about separating
     * out individual cases (in which case some logic should be
     * added for them in the following method call).
     */

    /* Perform any additional rescaling and adjustment, if required. */
    adjust_freqs();
}


/*
 * Adds a flag to an artifact. Returns true when changes were made.
 */
static bool add_flag(struct artifact *art, int flag)
{
    if (of_has(art->flags, flag)) return false;

    of_on(art->flags, flag);
    return true;
}


/*
 * Adds a resist to an artifact. Returns true when changes were made.
 */
static bool add_resist(struct artifact *art, int element)
{
    if (art->el_info[element].res_level > 0) return false;

    art->el_info[element].res_level = 1;
    return true;
}


/*
 * Adds an immunity to an artifact. Returns true when changes were made.
 */
static void add_immunity(struct artifact *art)
{
    int r = randint0(4);

    art->el_info[r].res_level = 3;
}


/*
 * Adds a mod to an artifact. Always attempts to increase the mod.
 */
static void add_mod(struct artifact *art, int mod)
{
    do_mod(art, mod);
}


/*
 * Adds a mod to an artifact, but won't increase
 * the mod if present. Returns true when changes were made.
 */
static bool add_fixed_mod(struct artifact *art, int mod)
{
    if (art->modifiers[mod]) return false;

    do_mod(art, mod);
    return true;
}


/*
 * Adds a mod to an artifact. Returns true
 * when the mod was not present.
 */
static bool add_first_mod(struct artifact *art, int mod)
{
    if (!art->modifiers[mod])
    {
        art->modifiers[mod] = randint1(4);
        return true;
    }

    do_mod(art, mod);
    return false;
}


/*
 * Adds a stat modifier, if possible
 */
static void add_stat(struct artifact *art)
{
    int r;
    bool success = false;

    /* Break out if all stats are raised to avoid an infinite loop */
    if (art->modifiers[OBJ_MOD_STR] && art->modifiers[OBJ_MOD_INT] &&
        art->modifiers[OBJ_MOD_WIS] && art->modifiers[OBJ_MOD_DEX] &&
        art->modifiers[OBJ_MOD_CON])
    {
        return;
    }

    /* Make sure we add one that hasn't been added yet */
    while (!success)
    {
        r = randint0(STAT_MAX);
        if (r == 0) success = add_fixed_mod(art, OBJ_MOD_STR);
        else if (r == 1) success = add_fixed_mod(art, OBJ_MOD_INT);
        else if (r == 2) success = add_fixed_mod(art, OBJ_MOD_WIS);
        else if (r == 3) success = add_fixed_mod(art, OBJ_MOD_DEX);
        else if (r == 4) success = add_fixed_mod(art, OBJ_MOD_CON);
    }
}


/*
 * Adds a sustain, if possible
 */
static void add_sustain(struct artifact *art)
{
    int r;
    bool success = false;

    /* Break out if all stats are sustained to avoid an infinite loop */
    if (flags_test_all(art->flags, OF_SIZE, OF_SUST_STR, OF_SUST_INT, OF_SUST_WIS, OF_SUST_DEX,
        OF_SUST_CON, FLAG_END))
    {
        return;
    }

    while (!success)
    {
        r = randint0(STAT_MAX);
        if (r == 0) success = add_flag(art, OF_SUST_STR);
        else if (r == 1) success = add_flag(art, OF_SUST_INT);
        else if (r == 2) success = add_flag(art, OF_SUST_WIS);
        else if (r == 3) success = add_flag(art, OF_SUST_DEX);
        else if (r == 4) success = add_flag(art, OF_SUST_CON);
    }
}


/*
 * Adds a low resist, if possible
 */
static void add_low_resist(struct artifact *art)
{
    size_t r, i, count = 0;

    for (i = ELEM_BASE_MIN; i < ELEM_HIGH_MIN; i++)
    {
        if (art->el_info[i].res_level <= 0) count++;
    }

    if (!count) return;

    r = randint0(count);
    count = 0;

    for (i = ELEM_BASE_MIN; i < ELEM_HIGH_MIN; i++)
    {
        if (art->el_info[i].res_level > 0) continue;
        if (r == count++)
        {
            add_resist(art, i);
            return;
        }
    }
}


/*
 * Adds a high resist, if possible
 */
static void add_high_resist(struct artifact *art)
{
    size_t i;
    int r, temp;
    int count = 0;
    bool success = false;

    /* Add a high resist, according to the generated frequency distribution. */
    temp = 0;
    for (i = 0; i < N_ELEMENTS(art_idx_high_resist); i++)
        temp += artprobs[art_idx_high_resist[i]];

    /* The following will fail (cleanly) if all high resists already added */
    while (!success && (count < MAX_TRIES))
    {
        /* Randomize from 1 to this total amount */
        r = randint1(temp);

        /* Determine which (weighted) resist this number corresponds to */
        temp = artprobs[art_idx_high_resist[0]];
        i = 0;
        while ((r > temp) && (i < N_ELEMENTS(art_idx_high_resist)))
        {
            temp += artprobs[art_idx_high_resist[i]];
            i++;
        }

        /* Now i should give us the index of the correct high resist */
        if (i == 0) success = add_resist(art, ELEM_POIS);
        else if (i == 1) success = add_flag(art, OF_PROT_FEAR);
        else if (i == 2) success = add_resist(art, ELEM_LIGHT);
        else if (i == 3) success = add_resist(art, ELEM_DARK);
        else if (i == 4) success = add_flag(art, OF_PROT_BLIND);
        else if (i == 5) success = add_flag(art, OF_PROT_CONF);
        else if (i == 6) success = add_resist(art, ELEM_SOUND);
        else if (i == 7) success = add_resist(art, ELEM_SHARD);
        else if (i == 8) success = add_resist(art, ELEM_NEXUS);
        else if (i == 9) success = add_resist(art, ELEM_NETHER);
        else if (i == 10) success = add_resist(art, ELEM_CHAOS);
        else if (i == 11) success = add_resist(art, ELEM_DISEN);
        else if (i == 12) success = add_flag(art, OF_PROT_STUN);

        count++;
    }
}


/*
 * Adds a brand, if possible
 */
static void add_brand(struct artifact *art)
{
    int count;

    /* Hack -- do not allow slays/brands on nonweapons other than rings and gloves */
    if (!art_is_weapon(art) && !art_is_ammo(art) && !art_is_ring(art) &&
        (art->tval != TV_GLOVES))
    {
        return;
    }

    for (count = 0; count < MAX_TRIES; count++)
    {
        if (append_random_brand(&art->brands)) return;
    }
}


/*
 * Adds a slay, if possible
 */
static void add_slay(struct artifact *art, bool melee)
{
    int count;

    /* Hack -- do not allow slays/brands on nonweapons other than rings and gloves */
    if (!art_is_weapon(art) && !art_is_ammo(art) && !art_is_ring(art) &&
        (art->tval != TV_GLOVES))
    {
        return;
    }

    for (count = 0; count < MAX_TRIES; count++)
    {
        if (append_random_slay(art, melee)) return;
    }
}


/*
 * Adds one or two damage dice
 */
static void add_damage_dice(struct artifact *art)
{
    /* Changed this to increments 1 or 2 only */
    art->dd += (byte)randint1(2);
    if (art->dd > MAX_WEAPON_DICE) art->dd = MAX_WEAPON_DICE;
}


/*
 * Adds to_h, if not too high already
 */
static void add_to_hit(struct artifact *art, int fixed, int random)
{
    /* Mage weapons are always +0 +0 */
    if (art_is_mstaff(art)) return;

    /* Inhibit above certain thresholds */
    if (art->to_h >= VERYHIGH_TO_HIT)
    {
        /* Strongly inhibit */
        if (!INHIBIT_STRONG) return;
    }
    else if (art->to_h >= HIGH_TO_HIT)
    {
        /* Weakly inhibit */
        if (!INHIBIT_WEAK) return;
    }
    art->to_h += (s16b)(fixed + randint0(random));

    /* Hack -- dark swords never have high tohit (to keep high antimagic field) */
    if (art_is_dark_sword(art))
    {
        if (art->to_h > 0) art->to_h = randint1(art->to_h);
        if (art->to_h > 15) art->to_h = 15;
    }
}


/*
 * Adds to_d, if not too high already
 */
static void add_to_dam(struct artifact *art, int fixed, int random)
{
    /* Mage weapons are always +0 +0 */
    if (art_is_mstaff(art)) return;

    /* Inhibit above certain thresholds */
    if (art->to_d >= VERYHIGH_TO_DAM)
    {
        /* Strongly inhibit */
        if (!INHIBIT_STRONG) return;
    }
    else if (art->to_d >= HIGH_TO_DAM)
    {
        /* Weakly inhibit */
        if (!INHIBIT_WEAK) return;
    }

    /* PWMAngband: limit damage power on artifact ammo */
    if (art_is_ammo(art))
    {
        if (art->to_d >= VERYHIGH_TO_DAM_AMMO)
        {
            /* Strongly inhibit */
            if (!INHIBIT_STRONG) return;
        }
        else if (art->to_d >= HIGH_TO_DAM_AMMO)
        {
            /* Weakly inhibit */
            if (!INHIBIT_WEAK) return;
        }
    }

    art->to_d += (s16b)(fixed + randint0(random));

    /* Hack -- dark swords never have high todam (to keep high antimagic field) */
    if (art_is_dark_sword(art))
    {
        if (art->to_d > 0) art->to_d = randint1(art->to_d);
        if (art->to_d > 15) art->to_d = 15;
    }
}


/*
 * Adds to_a, if not too high already
 */
static void add_to_AC(struct artifact *art, int fixed, int random)
{
    /* Inhibit above certain thresholds */
    if (art->to_a >= VERYHIGH_TO_AC)
    {
        /* Strongly inhibit */
        if (!INHIBIT_STRONG) return;
    }
    else if (art->to_a >= HIGH_TO_AC)
    {
        /* Weakly inhibit */
        if (!INHIBIT_WEAK) return;
    }
    art->to_a += (s16b)(fixed + randint0(random));
}


/*
 * Lowers weight
 */
static void add_weight_mod(struct artifact *art)
{
    art->weight = (art->weight * 9) / 10;
}


/* Add an activation (called only if neither artifact nor base item has one) */
static void add_activation(struct artifact *art, s32b target_power)
{
    int i, x, p, max_effect = 0;
    int count = 0;

    /* Work out the maximum allowed activation power */
    for (i = 0; i < z_info->act_max; i++)
    {
        struct activation *act = &activations[i];

        if (act->power > max_effect) max_effect = act->power;
    }

    /* Select an activation at random */
    while (count < MAX_TRIES)
    {
        x = randint0(z_info->act_max);
        p = activations[x].power;

        /*
         * Check that activation is useful but not exploitable
         * and roughly proportionate to the overall power
         */
        if ((100 * p / max_effect > 50 * target_power / max_power) &&
            (100 * p / max_effect < 200 * target_power / max_power))
        {
            art->activation = &activations[x];
            art->time.base = (p * 8);
            art->time.dice = ((p > 5)? (p / 5): 1);
            art->time.sides = p;
            return;
        }
        count++;
    }
}


static void add_shots(struct artifact *art)
{
    /* Hack -- do not allow extra shots on nonweapons other than gloves */
    if ((art->tval != TV_BOW) && (art->tval != TV_GLOVES)) return;

    add_mod(art, OBJ_MOD_SHOTS);
}


static void add_blows(struct artifact *art)
{
    /* Hack -- do not allow extra blows on nonweapons other than gloves */
    if (!art_is_melee_weapon(art) && (art->tval != TV_GLOVES)) return;

    add_mod(art, OBJ_MOD_BLOWS);
}


/*
 * Obtain extra ESP power
 */
int get_new_esp(bitflag flags[OF_SIZE])
{
    int i, j, options = 0, flag = 0;
    bitflag newf[OF_SIZE];

    create_mask(newf, false, OFT_ESP, OFT_MAX);

    for (i = of_next(newf, FLAG_START); i != FLAG_END; i = of_next(newf, i + 1))
    {
        /* One chance at one of the 7 racial ESPs */
        int commonness = 1;

        /* Three chances at ESP evil */
        if (i == OF_ESP_EVIL) commonness = 3;

        /* Two chances at full ESP */
        if (i == OF_ESP_ALL) commonness = 2;

        /* Four chances at telepathic awareness */
        if (i == OF_ESP_RADIUS) commonness = 4;

        /* Skip this one if the flag is already present */
        if (of_has(flags, i)) continue;

        /* Skip this one if the flag is redundant */
        if ((i == OF_ESP_ORC) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_TROLL) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_GIANT) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_ANIMAL) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_DRAGON) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_DEMON) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_DEMON) && of_has(flags, OF_ESP_EVIL)) continue;
        if ((i == OF_ESP_UNDEAD) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_UNDEAD) && of_has(flags, OF_ESP_EVIL)) continue;
        if ((i == OF_ESP_EVIL) && of_has(flags, OF_ESP_ALL)) continue;
        if ((i == OF_ESP_RADIUS) && of_has(flags, OF_ESP_ALL)) continue;

        /*
         * Each time we find a new possible option, we have a 1-in-N chance of
         * choosing it and an (N-1)-in-N chance of keeping a previous one
         */
        for (j = 0; j < commonness; j++)
            if (one_in_(++options)) flag = i;
    }

    return flag;
}


static void add_missile_dice(struct artifact *art)
{
    art->dd++;
    if (art->dd > MAX_AMMO_DICE) art->dd = MAX_AMMO_DICE;
}


/*
 * Build a suitable frequency table for this item, based on the generated
 * frequencies.  The frequencies for any abilities that don't apply for
 * this item type will be set to zero.  First parameter is the artifact
 * for which to generate the frequency table.
 *
 * The second input parameter is a pointer to an array that the function
 * will use to store the frequency table.  The array must have size
 * ART_IDX_TOTAL.
 *
 * The resulting frequency table is cumulative for ease of use in the
 * weighted randomization algorithm.
 */
static void build_freq_table(struct artifact *art, s16b *freq)
{
    int i;
    size_t j;
    s16b f_temp[ART_IDX_TOTAL];

    /* First, set everything to zero */
    for (i = 0; i < ART_IDX_TOTAL; i++)
    {
        f_temp[i] = 0;
        freq[i] = 0;
    }

    /* Now copy over appropriate frequencies for applicable abilities */

    /* Bow abilities */
    if (art->tval == TV_BOW)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_bow); j++)
            f_temp[art_idx_bow[j]] = artprobs[art_idx_bow[j]];
    }

    /* General weapon abilities */
    if (art_is_weapon(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_weapon); j++)
            f_temp[art_idx_weapon[j]] = artprobs[art_idx_weapon[j]];
    }

    /* General ammo abilities */
    else if (art_is_ammo(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_ammo); j++)
            f_temp[art_idx_ammo[j]] = artprobs[art_idx_ammo[j]];
    }

    /* General non-weapon abilities */
    else
    {
        for (j = 0; j < N_ELEMENTS(art_idx_nonweapon); j++)
            f_temp[art_idx_nonweapon[j]] = artprobs[art_idx_nonweapon[j]];
    }

    /* General melee abilities */
    if (art_is_melee_weapon(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_melee); j++)
            f_temp[art_idx_melee[j]] = artprobs[art_idx_melee[j]];
    }

    /* General armor abilities */
    if (art_is_armor(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_allarmor); j++)
            f_temp[art_idx_allarmor[j]] = artprobs[art_idx_allarmor[j]];
    }

    /* Boot abilities */
    if (art->tval == TV_BOOTS)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_boot); j++)
            f_temp[art_idx_boot[j]] = artprobs[art_idx_boot[j]];
    }

    /* Glove abilities */
    if (art->tval == TV_GLOVES)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_glove); j++)
            f_temp[art_idx_glove[j]] = artprobs[art_idx_glove[j]];
    }

    /* Headgear abilities */
    if (art_is_head_armor(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_headgear); j++)
            f_temp[art_idx_headgear[j]] = artprobs[art_idx_headgear[j]];
    }

    /* Shield abilities */
    if (art->tval == TV_SHIELD)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_shield); j++)
            f_temp[art_idx_shield[j]] = artprobs[art_idx_shield[j]];
    }

    /* Cloak abilities */
    if (art->tval == TV_CLOAK)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_cloak); j++)
            f_temp[art_idx_cloak[j]] = artprobs[art_idx_cloak[j]];
    }

    /* Armor abilities */
    if (art_is_body_armor(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_armor); j++)
            f_temp[art_idx_armor[j]] = artprobs[art_idx_armor[j]];
    }

    /* Digger abilities */
    if (art_is_digger(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_digger); j++)
            f_temp[art_idx_digger[j]] = artprobs[art_idx_digger[j]];
    }

    /* Mage staff abilities */
    if (art_is_mstaff(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_mstaff); j++)
            f_temp[art_idx_mstaff[j]] = artprobs[art_idx_mstaff[j]];
    }

    /* Missile abilities */
    if (art_is_ammo(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_missile); j++)
            f_temp[art_idx_missile[j]] = artprobs[art_idx_missile[j]];
    }

    /* PWMAngband: missiles have only specific abilities */
    else
    {
        /* General abilities - no constraint */
        for (j = 0; j < N_ELEMENTS(art_idx_gen); j++)
            f_temp[art_idx_gen[j]] = artprobs[art_idx_gen[j]];
    }

    /*
     * Now we have the correct individual frequencies, we build a cumulative
     * frequency table for them.
     */
    for (i = 0; i < ART_IDX_TOTAL; i++)
    {
        for (j = i; j < ART_IDX_TOTAL; j++) freq[j] += f_temp[i];
    }
}


/*
 * Choose a random ability using weights based on the given cumulative frequency
 * table.  A pointer to the frequency array (which must be of size ART_IDX_TOTAL)
 * is passed as a parameter.  The function returns a number representing the
 * index of the ability chosen.
 */
static int choose_ability(s16b *freq_table)
{
    int r, ability;

    /* Generate a random number between 1 and the last value in the table */
    r = randint1(freq_table[ART_IDX_TOTAL - 1]);

    /* Find the entry in the table that this number represents. */
    ability = 0;
    while (r > freq_table[ability]) ability++;

    /*
     * The ability variable is now the index of the first value in the table
     * greater than or equal to r, which is what we want.
     */
    return ability;
}


/*
 * Add an ability given by the index r.  This is mostly just a long case
 * statement.
 *
 * Note that this method is totally general and imposes no restrictions on
 * appropriate item type for a given ability.  This is assumed to have
 * been done already.
 */
static void add_ability_aux(struct artifact *art, int r, s32b target_power)
{
    switch (r)
    {
        case ART_IDX_BOW_SHOTS:
        case ART_IDX_NONWEAPON_SHOTS:
            add_shots(art);
            break;

        case ART_IDX_BOW_MIGHT:
            add_mod(art, OBJ_MOD_MIGHT);
            break;

        case ART_IDX_WEAPON_HIT:
        case ART_IDX_AMMO_HIT:
        case ART_IDX_NONWEAPON_HIT:
            add_to_hit(art, 1, 2 * mean_hit_increment);
            break;

        case ART_IDX_WEAPON_DAM:
        case ART_IDX_AMMO_DAM:
        case ART_IDX_NONWEAPON_DAM:
            add_to_dam(art, 1, 2 * mean_dam_increment);
            break;

        case ART_IDX_NONWEAPON_HIT_DAM:
            add_to_hit(art, 1, 2 * mean_hit_increment);
            add_to_dam(art, 1, 2 * mean_dam_increment);
            break;

        case ART_IDX_WEAPON_AGGR:
        case ART_IDX_NONWEAPON_AGGR:
            if (target_power > AGGR_POWER) add_flag(art, OF_AGGRAVATE);
            break;

        case ART_IDX_MELEE_BLESS:
            add_flag(art, OF_BLESSED);
            break;

        case ART_IDX_BOW_BRAND:
        case ART_IDX_MELEE_BRAND:
        case ART_IDX_NONWEAPON_BRAND:
        case ART_IDX_MISSILE_BRAND:
            add_brand(art);
            break;

        case ART_IDX_BOW_SLAY:
        case ART_IDX_MELEE_SLAY:
        case ART_IDX_NONWEAPON_SLAY:
            add_slay(art, true);
            break;

        case ART_IDX_MELEE_SINV:
        case ART_IDX_HELM_SINV:
        case ART_IDX_GEN_SINV:
        case ART_IDX_MSTAFF_SINV:
            add_flag(art, OF_SEE_INVIS);
            break;

        case ART_IDX_MELEE_BLOWS:
        case ART_IDX_NONWEAPON_BLOWS:
            add_blows(art);
            break;

        case ART_IDX_MELEE_AC:
        case ART_IDX_BOOT_AC:
        case ART_IDX_GLOVE_AC:
        case ART_IDX_HELM_AC:
        case ART_IDX_SHIELD_AC:
        case ART_IDX_CLOAK_AC:
        case ART_IDX_ARMOR_AC:
        case ART_IDX_GEN_AC:
            add_to_AC(art, 1, 2 * mean_ac_increment);
            break;

        case ART_IDX_MELEE_DICE:
            add_damage_dice(art);
            break;

        case ART_IDX_MELEE_WEIGHT:
        case ART_IDX_ALLARMOR_WEIGHT:
            add_weight_mod(art);
            break;

        case ART_IDX_DIGGER_TUNN:
        case ART_IDX_GEN_TUNN:
            add_mod(art, OBJ_MOD_TUNNEL);
            break;

        case ART_IDX_BOOT_FEATHER:
        case ART_IDX_GEN_FEATHER:
            add_flag(art, OF_FEATHER);
            break;

        case ART_IDX_BOOT_STEALTH:
        case ART_IDX_CLOAK_STEALTH:
        case ART_IDX_ARMOR_STEALTH:
        case ART_IDX_GEN_STEALTH:
            add_mod(art, OBJ_MOD_STEALTH);
            break;

        case ART_IDX_BOOT_SPEED:
        case ART_IDX_GEN_SPEED:
            add_first_mod(art, OBJ_MOD_SPEED);
            break;

        case ART_IDX_GLOVE_FA:
        case ART_IDX_GEN_FA:
        case ART_IDX_MSTAFF_FA:
            add_flag(art, OF_FREE_ACT);
            break;

        case ART_IDX_GLOVE_DEX:
            add_fixed_mod(art, OBJ_MOD_DEX);
            break;

        case ART_IDX_GLOVE_MANA:
            add_mod(art, OBJ_MOD_MANA);
            break;

        case ART_IDX_GLOVE_ID:
        case ART_IDX_HELM_ID:
            add_flag(art, OF_KNOWLEDGE);
            break;

        case ART_IDX_HELM_RBLIND:
        case ART_IDX_GEN_RBLIND:
        case ART_IDX_MSTAFF_RBLIND:
            add_flag(art, OF_PROT_BLIND);
            break;

        case ART_IDX_HELM_ESP:
        case ART_IDX_GEN_ESP:
        case ART_IDX_MSTAFF_ESP:
        {
            /* PWMAngband: random ESP instead of full ESP */
            int esp_flag = get_new_esp(art->flags);

            if (esp_flag) add_flag(art, esp_flag);
            break;
        }

        case ART_IDX_HELM_WIS:
            add_fixed_mod(art, OBJ_MOD_WIS);
            break;

        case ART_IDX_HELM_INT:
        case ART_IDX_MSTAFF_INT:
            add_fixed_mod(art, OBJ_MOD_INT);
            break;

        case ART_IDX_SHIELD_LRES:
        case ART_IDX_ARMOR_LRES:
        case ART_IDX_GEN_LRES:
            add_low_resist(art);
            break;

        case ART_IDX_ARMOR_HLIFE:
        case ART_IDX_GEN_HLIFE:
            add_flag(art, OF_HOLD_LIFE);
            break;

        case ART_IDX_ARMOR_CON:
            add_fixed_mod(art, OBJ_MOD_CON);
            break;

        case ART_IDX_ARMOR_ALLRES:
            add_resist(art, ELEM_ACID);
            add_resist(art, ELEM_ELEC);
            add_resist(art, ELEM_FIRE);
            add_resist(art, ELEM_COLD);
            break;

        case ART_IDX_ARMOR_HRES:
            add_high_resist(art);
            break;

        case ART_IDX_MISSILE_SLAY:
            add_slay(art, false);
            break;

        case ART_IDX_MISSILE_DICE:
            add_missile_dice(art);
            break;

        case ART_IDX_GEN_STAT:
            add_stat(art);
            break;

        case ART_IDX_GEN_SUST:
            add_sustain(art);
            break;

        case ART_IDX_GEN_SEARCH:
            add_mod(art, OBJ_MOD_SEARCH);
            break;

        case ART_IDX_GEN_INFRA:
            add_mod(art, OBJ_MOD_INFRA);
            break;

        case ART_IDX_GEN_IMMUNE:
            add_immunity(art);
            break;

        case ART_IDX_GEN_LIGHT:
        {
            /* Only once (this also means not on light sources) */
            if (art->modifiers[OBJ_MOD_LIGHT]) break;

            art->modifiers[OBJ_MOD_LIGHT] = 1;
            break;
        }

        case ART_IDX_GEN_SDIG:
            add_flag(art, OF_SLOW_DIGEST);
            break;

        case ART_IDX_GEN_REGEN:
            add_flag(art, OF_REGEN);
            break;

        case ART_IDX_GEN_RPOIS:
            add_resist(art, ELEM_POIS);
            break;

        case ART_IDX_GEN_RFEAR:
            add_flag(art, OF_PROT_FEAR);
            break;

        case ART_IDX_GEN_RLIGHT:
            add_resist(art, ELEM_LIGHT);
            break;

        case ART_IDX_GEN_RDARK:
            add_resist(art, ELEM_DARK);
            break;

        case ART_IDX_GEN_RCONF:
        case ART_IDX_MSTAFF_RCONF:
            add_flag(art, OF_PROT_CONF);
            break;

        case ART_IDX_GEN_RSOUND:
            add_resist(art, ELEM_SOUND);
            break;

        case ART_IDX_GEN_RSHARD:
            add_resist(art, ELEM_SHARD);
            break;

        case ART_IDX_GEN_RNEXUS:
            add_resist(art, ELEM_NEXUS);
            break;

        case ART_IDX_GEN_RNETHER:
            add_resist(art, ELEM_NETHER);
            break;

        case ART_IDX_GEN_RCHAOS:
            add_resist(art, ELEM_CHAOS);
            break;

        case ART_IDX_GEN_RDISEN:
            add_resist(art, ELEM_DISEN);
            break;

        case ART_IDX_GEN_PSTUN:
            add_flag(art, OF_PROT_STUN);
            break;

        case ART_IDX_GEN_ACTIV:
            if (!art->activation) add_activation(art, target_power);
            break;
    }
}


/*
 * Randomly select an extra ability to be added to the artifact in question.
 */
static void add_ability(struct artifact *art, s32b target_power)
{
    int r;

    /* Choose a random ability using the frequency table previously defined */
    r = choose_ability(art_freq);

    /* Add the appropriate ability */
    add_ability_aux(art, r, target_power);

    /* Now remove contradictory or redundant powers. */
    remove_contradictory(art);

    /* Adding WIS to sharp weapons always blesses them */
    if ((art->modifiers[OBJ_MOD_WIS] > 0) && art_is_pointy(art))
        add_flag(art, OF_BLESSED);
}


/*
 * Try to supercharge this item by running through the list of the supercharge
 * abilities and attempting to add each in turn.  An artifact only gets one
 * chance at each of these up front (if applicable).
 */
static void try_supercharge(struct artifact *art, s32b target_power)
{
    /* Huge damage dice - missiles only */
    if (art_is_ammo(art))
    {
        if (randint0(z_info->a_max) < artprobs[ART_IDX_MISSILE_DICE_SUPER])
        {
            art->dd += 2 + randint0(3);
            if (art->dd > MAX_AMMO_DICE) art->dd = MAX_AMMO_DICE;
        }

        /* Missiles don't have any other supercharged abilities */
        return;
    }

    /* Huge damage dice or max blows - melee weapon only */
    if (art_is_melee_weapon(art))
    {
        /* Damage dice */
        if (randint0(z_info->a_max) < artprobs[ART_IDX_MELEE_DICE_SUPER])
        {
            art->dd += 3 + randint0(4);
            if (art->dd > MAX_WEAPON_DICE) art->dd = MAX_WEAPON_DICE;
        }

        /* Blows */
        else if (randint0(z_info->a_max) < artprobs[ART_IDX_MELEE_BLOWS_SUPER])
        {
            if (!art->modifiers[OBJ_MOD_BLOWS])
                art->modifiers[OBJ_MOD_BLOWS] = INHIBIT_BLOWS - 1;
        }
    }

    /* Bows - max might or shots */
    if (art->tval == TV_BOW)
    {
        if (randint0(z_info->a_max) < artprobs[ART_IDX_BOW_SHOTS_SUPER])
        {
            if (!art->modifiers[OBJ_MOD_SHOTS])
                art->modifiers[OBJ_MOD_SHOTS] = INHIBIT_SHOTS - 1;
        }
        else if (randint0(z_info->a_max) < artprobs[ART_IDX_BOW_MIGHT_SUPER])
        {
            if (!art->modifiers[OBJ_MOD_MIGHT])
                art->modifiers[OBJ_MOD_MIGHT] = INHIBIT_MIGHT - 1;
        }
    }

    /* Big speed bonus - any item (potentially) but more likely on boots */
    if ((randint0(z_info->a_max) < artprobs[ART_IDX_GEN_SPEED_SUPER]) ||
        ((art->tval == TV_BOOTS) && (randint0(z_info->a_max) < artprobs[ART_IDX_BOOT_SPEED])))
    {
        int value = 5 + randint0(6);

        if (INHIBIT_WEAK) value += randint1(3);
        if (INHIBIT_STRONG) value += 1 + randint1(6);
        art->modifiers[OBJ_MOD_SPEED] = value;
    }

    /* Big AC bonus */
    if (randint0(z_info->a_max) < artprobs[ART_IDX_GEN_AC_SUPER])
    {
        art->to_a += 19 + randint1(11);
        if (INHIBIT_WEAK) art->to_a += randint1(10);
        if (INHIBIT_STRONG) art->to_a += randint1(20);
    }

    /* Aggravation */
    if (art_is_weapon(art))
    {
        if ((randint0(z_info->a_max) < artprobs[ART_IDX_WEAPON_AGGR]) && (target_power > AGGR_POWER))
            of_on(art->flags, OF_AGGRAVATE);
    }
    else
    {
        if ((randint0(z_info->a_max) < artprobs[ART_IDX_NONWEAPON_AGGR]) &&
            (target_power > AGGR_POWER))
        {
            of_on(art->flags, OF_AGGRAVATE);
        }
    }
}


/*
 * Make it bad, or if it's already bad, make it worse!
 */
static void do_curse(struct artifact *art)
{
    int i;

    /* Add bad abilities */
    if (one_in_(7)) of_on(art->flags, OF_AGGRAVATE);
    if (one_in_(4)) of_on(art->flags, OF_DRAIN_EXP);
    if (one_in_(7)) of_on(art->flags, OF_TELEPORT);

    /* Reverse mods and bonuses */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if ((art->modifiers[i] > 0) && one_in_(2)) art->modifiers[i] = 0 - art->modifiers[i];
    }
    if ((art->to_a > 0) && one_in_(2)) art->to_a = 0 - art->to_a;
    if ((art->to_h > 0) && one_in_(2)) art->to_h = 0 - art->to_h;
    if ((art->to_d > 0) && one_in_(4)) art->to_d = 0 - art->to_d;

    /* If it's cursed, make it worse */
    if (of_has(art->flags, OF_LIGHT_CURSE))
    {
        if (one_in_(2)) of_on(art->flags, OF_HEAVY_CURSE);
        return;
    }

    /* Always cursed... */
    of_on(art->flags, OF_LIGHT_CURSE);

    /* ... and sometimes worse */
    if (one_in_(4)) of_on(art->flags, OF_HEAVY_CURSE);
}


static int mod_light(struct artifact *a)
{
    if (!art_is_light(a)) return 0;

    return a->modifiers[OBJ_MOD_LIGHT];
}


/*
 * Copy artifact fields from source to dest
 */
static void copy_artifact(struct artifact *dest, struct artifact *source)
{
    /* Free pointers */
    free_brand(dest->brands);
    free_slay(dest->slays);

    /* Copy info from the source artifact (this includes pointers!) */
    memcpy(dest, source, sizeof(*source));

    /* Reset pointers */
    dest->brands = NULL;
    dest->slays = NULL;

    /* Copy pointers from the source artifact */
    copy_brand(&dest->brands, source->brands);
    copy_slay(&dest->slays, source->slays);
}


/*
 * Scramble an artifact.
 *
 * Note the special cases (One Ring, Grond, Morgoth, ...).
 */
static bool scramble_artifact(struct artifact *art)
{
    struct object_kind *kind = lookup_kind(art->tval, art->sval);
    struct artifact *a_old = mem_zalloc(sizeof(*a_old));
    s32b power;
    int tries = 0;
    s32b ap = 0;
    bool curse_me = false;
    bool success = false;
    int i;
    bool special_artifact = kf_has(kind->kind_flags, KF_INSTA_ART);

    /* Skip unused artifacts */
    if (art->tval == 0)
    {
        free_artifact(a_old);
        return false;
    }

    /* Special cases -- don't randomize these! */
    if (strstr(art->name, "The One Ring") || kf_has(kind->kind_flags, KF_QUEST_ART) ||
        strstr(art->name, "of Maglor"))
    {
        free_artifact(a_old);
        return false;
    }

    /* Evaluate the original artifact to determine the power level. */
    if (art->aidx < (u32b)z_info->a_max)
        power = base_power[art->aidx];

    /* Choose a random power level for Rings of Power */
    else
        power = AGGR_POWER * (70 + randint0(41)) / 100;

    /* If it has a restricted ability then don't randomize it. */
    if (power >= INHIBIT_POWER)
    {
        free_artifact(a_old);
        return false;
    }

    if (power < 0) curse_me = true;

    /*
     * Flip the sign on power if it's negative, since it's only used for base
     * item choice
     */
    if (power < 0) power = 0 - power;

    /* Normal artifact */
    if (!special_artifact)
    {
        /* PWMAngband: keep the item kind */

        /* Keep base attributes */
        art->to_h = randcalc(kind->to_h, 0, MINIMISE);
        art->to_d = randcalc(kind->to_d, 0, MINIMISE);
        art->to_a = randcalc(kind->to_a, 0, MINIMISE);
        art->ac = kind->ac;
        art->dd = kind->dd;
        art->ds = kind->ds;
        art->weight = kind->weight;
        of_copy(art->flags, kind->flags);
        copy_slay(&art->slays, kind->slays);
        copy_brand(&art->brands, kind->brands);
        for (i = 0; i < OBJ_MOD_MAX; i++)
            art->modifiers[i] = randcalc(kind->modifiers[i], 0, MINIMISE);
        for (i = 0; i < ELEM_MAX; i++)
        {
            art->el_info[i].res_level = kind->el_info[i].res_level;
            art->el_info[i].flags = kind->el_info[i].flags;
        }

        /* PWMAngband: keep base activations */
        art->activation = kind->activation;
        art->time.base = kind->time.base;
        art->time.dice = kind->time.dice;
        art->time.sides = kind->time.sides;

        /* Assign basic stats to the artifact */
        switch (art->tval)
        {
            case TV_BOW:
            case TV_HAFTED:
            case TV_SWORD:
            case TV_POLEARM:
            {
                art->to_h += (s16b)(mean_hit_startval / 2 + randint0(mean_hit_startval));
                art->to_d += (s16b)(mean_dam_startval / 2 + randint0(mean_dam_startval));
                break;
            }
            case TV_BOOTS:
            case TV_GLOVES:
            case TV_HELM:
            case TV_CROWN:
            case TV_SHIELD:
            case TV_CLOAK:
            case TV_SOFT_ARMOR:
            case TV_HARD_ARMOR:
            case TV_DRAG_ARMOR:
            {
                art->to_a += (s16b)(mean_ac_startval / 2 + randint0(mean_ac_startval));
                break;
            }
        }
    }

    /* Special artifact (light source, ring, or amulet) */
    else
    {
        /* Keep the item kind */
        int mod = mod_light(art);

        /* Clear the following fields; leave the rest alone */
        art->to_h = art->to_d = art->to_a = 0;
        of_wipe(art->flags);
        for (i = 0; i < OBJ_MOD_MAX; i++)
            art->modifiers[i] = 0;
        for (i = 0; i < ELEM_MAX; i++)
        {
            art->el_info[i].res_level = 0;
            art->el_info[i].flags = 0;
        }

        /* Clear the activations for rings and amulets but not lights */
        if (!art_is_light(art))
            art->activation = NULL;

        /* Restore lights */
        else
        {
            of_on(art->flags, OF_NO_FUEL);
            art->modifiers[OBJ_MOD_LIGHT] = mod;
        }
    }

    /* Artifacts ignore everything */
    for (i = ELEM_BASE_MIN; i < ELEM_HIGH_MIN; i++)
        art->el_info[i].flags |= EL_INFO_IGNORE;

    /* Generate the cumulative frequency table for this base item type */
    build_freq_table(art, art_freq);

    /* Copy artifact info temporarily. */
    copy_artifact(a_old, art);

    /* Give this artifact a shot at being supercharged */
    try_supercharge(art, power);
    ap = artifact_power(art);
    if (ap > (power * 23) / 20 + 1)
    {
        /* Too powerful -- put it back */
        copy_artifact(art, a_old);
    }

    /* First draft: add two abilities, then curse it three times. */
    if (curse_me)
    {
        /* Copy artifact info temporarily. */
        copy_artifact(a_old, art);
        do
        {
            add_ability(art, power);
            add_ability(art, power);
            do_curse(art);
            do_curse(art);
            do_curse(art);
            remove_contradictory(art);
            ap = artifact_power(art);

            /* Accept if it doesn't have any inhibited abilities */
            if (ap < INHIBIT_POWER) success = true;

            /* Otherwise go back and try again */
            else copy_artifact(art, a_old);
        }
        while (!success);
    }
    else
    {
        /*
         * Select a random set of abilities which roughly matches the
         * original's in terms of overall power/usefulness.
         */
        for (tries = 0; tries < MAX_TRIES; tries++)
        {
            /* Copy artifact info temporarily. */
            copy_artifact(a_old, art);

            add_ability(art, power);
            ap = artifact_power(art);

            /* Pushed both limits up by about 5% */
            if (ap > (power * 23) / 20 + 1)
            {
                /* Too powerful -- put it back */
                copy_artifact(art, a_old);
                continue;
            }

            /* Just right */
            else if (ap >= (power * 19) / 20)
            {
                /* Add rescue for crappy weapons */
                if (art_is_weapon(art) && (art->to_d < 10))
                    art->to_d += randint0(10);

                break;
            }
        }

        if (tries >= MAX_TRIES)
        {
            /*
             * We couldn't generate an artifact within the number of permitted
             * iterations.
             */
            free_artifact(a_old);
            return false;
        }
    }

    /* Cleanup a_old */
    free_artifact(a_old);

    /* Set depth and rarity info according to power */
    /* This is currently very tricky for special artifacts */

    /* Flip cursed items to avoid overflows */
    if (ap < 0) ap = 0 - ap;

    if (special_artifact)
    {
        art->alloc_max = 127;
        if (ap > avg_power)
        {
            art->alloc_prob = 1;
            art->alloc_min = MAX(50, ((ap + 150) * 100 / max_power));
        }
        else if (ap > 30)
        {
            art->alloc_prob = MAX(2, (avg_power - ap) / 20);
            art->alloc_min = MAX(25, ((ap + 200) * 100 / max_power));
        }
        else
        {
            art->alloc_prob = 50 - ap;
            art->alloc_min = 5;
        }
    }
    else if (art->aidx < (u32b)z_info->a_max)
    {
        art->alloc_max = MIN(127, (ap * 4) / 5);
        art->alloc_min = MIN(100, ((ap + 100) * 100 / max_power));

        /* Sanity check */
        if (art->alloc_max < art->alloc_min) art->alloc_max = art->alloc_min;

        /* Leave alloc_prob consistent with base art total rarity */
    }

    /* Sanity check */
    if (art->alloc_prob > 99) art->alloc_prob = 99;
    if (art->alloc_prob < 1) art->alloc_prob = 1;

    /* Success */
    return true;
}


static void artifact_gen_name(char *buffer, int len, const char ***words)
{
    char word[MAX_RNAME_LEN + 1];
    const char **wordlist = words[RANDNAME_TOLKIEN];
    u32b i;
    bool nok = true;

    /* Take a random name */
    while (nok)
    {
        randname_make(RANDNAME_TOLKIEN, MIN_RNAME_LEN, MAX_RNAME_LEN, word, sizeof(word), words);

        /* Don't use existing Tolkien names */
        nok = false;
        for (i = 0; i < num_names[RANDNAME_TOLKIEN]; i++)
        {
            if (streq(word, wordlist[i]))
            {
                nok = true;
                break;
            }
        }
    }

    /* Capitalise first character */
    my_strcap(word);

    /* Either "... of something" or "... 'something'" form */
    if (one_in_(3))
        strnfmt(buffer, len, "'%s'", word);
    else
        strnfmt(buffer, len, "of %s", word);
}


/*
 * Generate a random artifact name
 */
void do_randart_name(s32b randart_seed, char *buffer, int len)
{
    u32b tmp_seed;
    bool rand_old;

    /* Save the RNG */
    tmp_seed = Rand_value;
    rand_old = Rand_quick;

    /* Prepare to use the Angband "simple" RNG. */
    Rand_value = randart_seed;
    Rand_quick = true;

    /* Generate a random name */
    artifact_gen_name(buffer, len, name_sections);

    /* When done, resume use of the Angband "complex" RNG. */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;
}


/* Dummy name for randart Rings of Power */
static char dummy[6] = "dummy";


/*
 * Call the artifact scrambling routine
 */
static struct artifact* do_randart_aux(struct artifact *a)
{
    struct artifact *art = mem_zalloc(sizeof(*art));

    /* Copy info from the corresponding static artifact */
    memcpy(art, a, sizeof(*a));

    /* Reset brands and slays */
    art->brands = NULL;
    art->slays = NULL;

    /* Hack in some values for Rings of Power */
    if (art->aidx >= (u32b)z_info->a_max)
    {
        art->tval = TV_RING;
        art->sval = lookup_sval(art->tval, "Black Ring of Power");
        art->name = dummy;
        art->alloc_prob = 100;
        art->alloc_min = 1;
        art->alloc_max = 127;
        art->weight = 2;
    }

    /* Randomize the artifact */
    if (!scramble_artifact(art))
    {
        /* Failure */
        free_artifact(art);
        return NULL;
    }

    /* Success */
    return art;
}


/*
 * Generate a random artifact
 */
struct artifact* do_randart(s32b randart_seed, struct artifact *a)
{
    u32b tmp_seed;
    bool rand_old;
    struct artifact *art;

    /* Save the RNG */
    tmp_seed = Rand_value;
    rand_old = Rand_quick;

    /* Prepare to use the Angband "simple" RNG. */
    Rand_value = randart_seed;
    Rand_quick = true;

    /* Generate the random artifact */
    art = do_randart_aux(a);

    /* When done, resume use of the Angband "complex" RNG. */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;

    /* Return the random artifact */
    return art;
}


/*
 * Initialize the random artifact generator
 */
void init_randart_generator(void)
{
    /* Allocate the "base powers" array */
    base_power = mem_zalloc(z_info->a_max * sizeof(s32b));

    /* Store the original power ratings */
    store_base_power();

    /* Determine the generation probabilities */
    parse_frequencies();
}


/*
 * Free memory associated to the random artifact generator
 */
void free_randart_generator(void)
{
    /* Free the "base powers" array */
    mem_free(base_power);
}


int get_artifact_level(const struct object *obj)
{
    int lev;
    struct artifact *art = obj->artifact;

    if (obj->randart_seed)
        art = do_randart(obj->randart_seed, obj->artifact);

    lev = art->level;

    if (obj->randart_seed)
        free_artifact(art);

    return lev;
}


void free_artifact(struct artifact *art)
{
    free_brand(art->brands);
    free_slay(art->slays);
    mem_free(art);
}
