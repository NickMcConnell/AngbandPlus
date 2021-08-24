/*
 * File: obj-randart.c
 * Purpose: Random artifact generation
 *
 * Copyright (c) 1998 Greg Wooledge, Ben Harrison, Robert Ruhlmann
 * Copyright (c) 2001 Chris Carr, Chris Robertson
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
 * Original random artifact generator (randart) by Greg Wooledge.
 * Updated by Chris Carr / Chris Robertson 2001-2010.
 */


/* Activation list */
struct activation *activations;


/*
 * Arrays of indices by item type, used in frequency generation
 */
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
    ART_IDX_MELEE_WEIGHT,
    ART_IDX_MELEE_TUNN,
    ART_IDX_MELEE_ANTIMAGIC
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
    ART_IDX_BOOT_TRAP_IMM,
    ART_IDX_BOOT_SPEED
};

static s16b art_idx_glove[] =
{
    ART_IDX_GLOVE_AC,
    ART_IDX_GLOVE_FA,
    ART_IDX_GLOVE_DEX,
    ART_IDX_GLOVE_HIT_DAM,
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
    ART_IDX_GEN_PSTUN,
    ART_IDX_GEN_DAM_RED,
    ART_IDX_GEN_MOVES,
    ART_IDX_GEN_TRAP_IMM
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


/*
 * Artifact data
 */
static struct artifact_set_data local_data;


/*
 * Wrapper functions for tvals (TODO: move this to obj-tval.c)
 */


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
 * Calculation of the statistics of an artifact set
 */


/*
 * Return the artifact power, by generating a "fake" object based on the
 * artifact, and calling the common object_power function
 */
static int artifact_power(struct artifact *art)
{
    struct object *fake;
    int power;

    if (!make_fake_artifact(&fake, art)) return 0;

    power = object_power(NULL, fake);
    object_delete(&fake);

    return power;
}


/*
 * Store the original artifact power ratings as a baseline
 */
static void store_base_power(struct artifact_set_data *data)
{
    int i, num;
    int *fake_total_power;
    int **fake_tv_power;
    int *fake_tv_num;

    data->max_power = 0;
    data->min_power = INHIBIT_POWER + 1;
    data->var_power = 0;
    fake_total_power = mem_zalloc(z_info->a_max * sizeof(int));
    fake_tv_power = mem_zalloc(TV_MAX * sizeof(int*));
    for (i = 0; i < TV_MAX; i++)
    {
        fake_tv_power[i] = mem_zalloc(z_info->a_max * sizeof(int));
        data->min_tv_power[i] = INHIBIT_POWER + 1;
        data->max_tv_power[i] = 0;
    }
    fake_tv_num = mem_zalloc(TV_MAX * sizeof(int));
    num = 0;

    for (i = 0; i < z_info->a_max; i++, num++)
    {
        int base_power = artifact_power(&a_info[i]);

        /* PWMAngband: store base power on the artifact to avoid keeping a pointless static array */
        a_info[i].base_power = base_power;

        /* Capture power stats, ignoring cursed and uber arts */
        if ((base_power > data->max_power) && (base_power < INHIBIT_POWER))
            data->max_power = base_power;
        if ((base_power < data->min_power) && (base_power > 0))
            data->min_power = base_power;
        if ((base_power > 0) && (base_power < INHIBIT_POWER))
        {
            int tval = a_info[i].tval;

            fake_total_power[num] = base_power;
            fake_tv_power[tval][fake_tv_num[tval]++] = base_power;
            if (base_power < data->min_tv_power[tval]) data->min_tv_power[tval] = base_power;
            if (base_power > data->max_tv_power[tval]) data->max_tv_power[tval] = base_power;
        }
        else num--;

        if (base_power < 0) data->neg_power_total++;
    }

    data->avg_power = mean(fake_total_power, num);
    data->var_power = variance(fake_total_power, num);
    for (i = 0; i < TV_MAX; i++)
    {
        if (fake_tv_num[i]) data->avg_tv_power[i] = mean(fake_tv_power[i], fake_tv_num[i]);
    }

    /* Store the number of different types, for use later */
    /* ToDo: replace this with full combination tracking */
    for (i = 0; i < z_info->a_max; i++)
    {
        switch (a_info[i].tval)
        {
            case TV_SWORD:
            case TV_POLEARM:
            case TV_HAFTED: data->melee_total++; break;
            case TV_BOW: data->bow_total++; break;
            case TV_SOFT_ARMOR:
            case TV_HARD_ARMOR:
            case TV_DRAG_ARMOR: data->armor_total++; break;
            case TV_SHIELD: data->shield_total++; break;
            case TV_CLOAK: data->cloak_total++; break;
            case TV_HELM:
            case TV_CROWN: data->headgear_total++; break;
            case TV_GLOVES: data->glove_total++; break;
            case TV_BOOTS: data->boot_total++; break;
            case TV_MSTAFF: data->mstaff_total++; break;
            case TV_ROCK:
            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT: data->missile_total++; break;
            case TV_NULL: break;
            default: data->other_total++;
        }
        data->total++;
    }

    for (i = 0; i < TV_MAX; i++) mem_free(fake_tv_power[i]);
    mem_free(fake_tv_power);
    mem_free(fake_total_power);
}


static void artifact_count_slays(const struct artifact *art, struct artifact_set_data *data,
    int idx_brand, int idx_slay)
{
    if (art->slays)
        data->art_probs[idx_slay] += slay_count(art->slays);
    if (art->brands)
        data->art_probs[idx_brand] += brand_count(art->brands);
}


/*
 * Handle weapon combat abilities
 */
static void count_weapon_abilities(const struct artifact *art, struct artifact_set_data *data)
{
    int bonus;
    struct object_kind *kind = lookup_kind(art->tval, art->sval);
    int min_to_h = randcalc(kind->to_h, 0, MINIMISE);
    int min_to_d = randcalc(kind->to_d, 0, MINIMISE);
    int min_to_a = randcalc(kind->to_a, 0, MINIMISE);

    /* To-hit and to-dam */
    bonus = (art->to_h - min_to_h - data->hit_startval) / data->hit_increment;
    data->art_probs[ART_IDX_WEAPON_HIT] += bonus;

    bonus = (art->to_d - min_to_d - data->dam_startval) / data->dam_increment;
    data->art_probs[ART_IDX_WEAPON_DAM] += bonus;

    /* Does this weapon have an unusual bonus to AC? */
    bonus = (art->to_a - min_to_a) / data->ac_increment;
    if (bonus > 0)
    {
        if (art->to_a > 20)
            data->art_probs[ART_IDX_MELEE_AC_SUPER]++;
        else
            data->art_probs[ART_IDX_MELEE_AC] += bonus;
    }

    /* Check damage dice - are they more than normal? */
    if (art->dd > kind->dd)
    {
        /* Difference of 3 or more? */
        if ((art->dd - kind->dd) > 2)
            data->art_probs[ART_IDX_MELEE_DICE_SUPER]++;
        else
            data->art_probs[ART_IDX_MELEE_DICE]++;
    }

    /* Check weight - is it different from normal? */
    if (art->weight != kind->weight) data->art_probs[ART_IDX_MELEE_WEIGHT]++;

    /* Do we have 3 or more extra blows? (Unlikely) */
    if (art->modifiers[OBJ_MOD_BLOWS] > 2)
        data->art_probs[ART_IDX_MELEE_BLOWS_SUPER]++;
    else if (art->modifiers[OBJ_MOD_BLOWS] > 0)
        data->art_probs[ART_IDX_MELEE_BLOWS]++;

    /* Aggravation */
    if (of_has(art->flags, OF_AGGRAVATE)) data->art_probs[ART_IDX_WEAPON_AGGR]++;

    /* Blessed weapon? */
    if (of_has(art->flags, OF_BLESSED)) data->art_probs[ART_IDX_MELEE_BLESS]++;

    /* See invisible? */
    if (of_has(art->flags, OF_SEE_INVIS)) data->art_probs[ART_IDX_MELEE_SINV]++;

    /* Check for tunnelling ability */
    if (art->modifiers[OBJ_MOD_TUNNEL] > 0)
        data->art_probs[ART_IDX_MELEE_TUNN]++;

    /* Count brands and slays */
    artifact_count_slays(art, data, ART_IDX_MELEE_BRAND, ART_IDX_MELEE_SLAY);

    /* Check for antimagic */
    if (art->modifiers[OBJ_MOD_ANTI_MAGIC] > 0)
        data->art_probs[ART_IDX_MELEE_ANTIMAGIC]++;
}


/*
 * Count combat abilities on bows
 */
static void count_bow_abilities(const struct artifact *art, struct artifact_set_data *data)
{
    int bonus;
    struct object_kind *kind = lookup_kind(art->tval, art->sval);
    int min_to_h = randcalc(kind->to_h, 0, MINIMISE);
    int min_to_d = randcalc(kind->to_d, 0, MINIMISE);

    /* To-hit */
    bonus = (art->to_h - min_to_h - data->hit_startval) / data->hit_increment;
    data->art_probs[ART_IDX_WEAPON_HIT] += bonus;

    /* To-dam */
    bonus = (art->to_d - min_to_d - data->dam_startval) / data->dam_increment;
    data->art_probs[ART_IDX_WEAPON_DAM] += bonus;

    /* Aggravation */
    if (of_has(art->flags, OF_AGGRAVATE)) data->art_probs[ART_IDX_WEAPON_AGGR]++;

    /* Do we have 3 or more extra shots? (Unlikely) */
    if (art->modifiers[OBJ_MOD_SHOTS] > 2)
        data->art_probs[ART_IDX_BOW_SHOTS_SUPER]++;
    else if (art->modifiers[OBJ_MOD_SHOTS] > 0)
        data->art_probs[ART_IDX_BOW_SHOTS]++;

    /* Do we have 3 or more extra might? (Unlikely) */
    if (art->modifiers[OBJ_MOD_MIGHT] > 2)
        data->art_probs[ART_IDX_BOW_MIGHT_SUPER]++;
    else if (art->modifiers[OBJ_MOD_MIGHT] > 0)
        data->art_probs[ART_IDX_BOW_MIGHT]++;

    /* Count brands and slays */
    artifact_count_slays(art, data, ART_IDX_BOW_BRAND, ART_IDX_BOW_SLAY);
}


/*
 * Count combat abilities on ammo
 */
static void count_ammo_abilities(const struct artifact *art, struct artifact_set_data *data)
{
    struct object_kind *kind = lookup_kind(art->tval, art->sval);

    /* Count brands and slays */
    artifact_count_slays(art, data, ART_IDX_MISSILE_BRAND, ART_IDX_MISSILE_SLAY);

    /* Check damage dice - are they more than normal? */
    if (art->dd > kind->dd)
    {
        /* Difference of 2 or more? */
        if ((art->dd - kind->dd) > 1)
            data->art_probs[ART_IDX_MISSILE_DICE_SUPER]++;
        else
            data->art_probs[ART_IDX_MISSILE_DICE]++;
    }
}


/*
 * Handle nonweapon combat abilities
 */
static void count_nonweapon_abilities(const struct artifact *art, struct artifact_set_data *data)
{
    int bonus;
    struct object_kind *kind = lookup_kind(art->tval, art->sval);
    int min_to_h = randcalc(kind->to_h, 0, MINIMISE);
    int min_to_d = randcalc(kind->to_d, 0, MINIMISE);
    int min_to_a = randcalc(kind->to_a, 0, MINIMISE);

    int to_hit = art->to_h - min_to_h;
    int to_dam = art->to_d - min_to_d;

    /* Armor class */
    bonus = (art->to_a - min_to_a - data->ac_startval) / data->ac_increment;
    if (bonus > 0)
    {
        if (art->to_a > 20)
            data->art_probs[ART_IDX_GEN_AC_SUPER]++;
        else if (art->tval == TV_BOOTS)
            data->art_probs[ART_IDX_BOOT_AC] += bonus;
        else if (art->tval == TV_GLOVES)
            data->art_probs[ART_IDX_GLOVE_AC] += bonus;
        else if (art_is_head_armor(art))
            data->art_probs[ART_IDX_HELM_AC] += bonus;
        else if (art->tval == TV_SHIELD)
            data->art_probs[ART_IDX_SHIELD_AC] += bonus;
        else if (art->tval == TV_CLOAK)
            data->art_probs[ART_IDX_CLOAK_AC] += bonus;
        else if (art_is_body_armor(art))
            data->art_probs[ART_IDX_ARMOR_AC] += bonus;
        else
            data->art_probs[ART_IDX_GEN_AC] += bonus;
    }

    /* To hit and dam to bonuses */
    if ((to_hit > 0) && (to_dam > 0))
    {
        bonus = (to_hit + to_dam) / (data->hit_increment + data->dam_increment);
        if (bonus > 0)
        {
            if (art->tval == TV_GLOVES) data->art_probs[ART_IDX_GLOVE_HIT_DAM] += bonus;
            else data->art_probs[ART_IDX_NONWEAPON_HIT_DAM] += bonus;
        }
    }
    else if (to_hit > 0)
    {
        bonus = to_hit / data->hit_increment;
        if (bonus > 0) data->art_probs[ART_IDX_NONWEAPON_HIT] += bonus;
    }
    else if (to_dam > 0)
    {
        bonus = to_dam / data->dam_increment;
        if (bonus > 0) data->art_probs[ART_IDX_NONWEAPON_DAM] += bonus;
    }

    /* Check weight - is it different from normal? */
    if ((art->weight != kind->weight) && art_is_armor(art))
        data->art_probs[ART_IDX_ALLARMOR_WEIGHT]++;

    /* Aggravation */
    if (of_has(art->flags, OF_AGGRAVATE)) data->art_probs[ART_IDX_NONWEAPON_AGGR]++;

    /* Count brands and slays */
    artifact_count_slays(art, data, ART_IDX_NONWEAPON_BRAND, ART_IDX_NONWEAPON_SLAY);

    /* Blows */
    if (art->modifiers[OBJ_MOD_BLOWS] > 0)
        data->art_probs[ART_IDX_NONWEAPON_BLOWS]++;

    /* Shots */
    if (art->modifiers[OBJ_MOD_SHOTS] > 0)
        data->art_probs[ART_IDX_NONWEAPON_SHOTS]++;

    /* Check for tunnelling ability */
    if (art->modifiers[OBJ_MOD_TUNNEL] > 0)
        data->art_probs[ART_IDX_GEN_TUNN]++;
}


/*
 * Count modifiers
 */
static void count_modifiers(const struct artifact *art, struct artifact_set_data *data)
{
    int num = 0;

    /* Stat bonuses. Add up the number of individual bonuses */
    if (art->modifiers[OBJ_MOD_STR] > 0) num++;
    if (art->modifiers[OBJ_MOD_INT] > 0) num++;
    if (art->modifiers[OBJ_MOD_WIS] > 0) num++;
    if (art->modifiers[OBJ_MOD_DEX] > 0) num++;
    if (art->modifiers[OBJ_MOD_CON] > 0) num++;

    /* Handle a few special cases separately. */
    if (art_is_head_armor(art) &&
        ((art->modifiers[OBJ_MOD_WIS] > 0) || (art->modifiers[OBJ_MOD_INT] > 0)))
    {
        /* Handle WIS and INT on helms and crowns */
        if (art->modifiers[OBJ_MOD_WIS] > 0)
        {
            data->art_probs[ART_IDX_HELM_WIS]++;

            /* Counted this one separately so subtract it here */
            num--;
        }
        if (art->modifiers[OBJ_MOD_INT] > 0)
        {
            data->art_probs[ART_IDX_HELM_INT]++;

            /* Counted this one separately so subtract it here */
            num--;
        }
    }
    else if (art_is_body_armor(art) && (art->modifiers[OBJ_MOD_CON] > 0))
    {
        /* Handle CON bonus on armor */
        data->art_probs[ART_IDX_ARMOR_CON]++;

        /* Counted this one separately so subtract it here */
        num--;
    }
    else if ((art->tval == TV_GLOVES) && (art->modifiers[OBJ_MOD_DEX] > 0))
    {
        /* Handle DEX bonus on gloves */
        data->art_probs[ART_IDX_GLOVE_DEX]++;

        /* Counted this one separately so subtract it here */
        num--;
    }
    else if (art_is_mstaff(art) && (art->modifiers[OBJ_MOD_INT] > 0))
    {
        /* Handle INT bonus on mage staves */
        data->art_probs[ART_IDX_MSTAFF_INT]++;

        /* Counted this one separately so subtract it here */
        num--;
    }

    /* Now the general case */
    if (num > 0)
    {
        /* There are some bonuses that weren't handled above */
        data->art_probs[ART_IDX_GEN_STAT] += num;
    }

    /* Handle stealth, including a couple of special cases */
    if (art->modifiers[OBJ_MOD_STEALTH] > 0)
    {
        if (art->tval == TV_BOOTS)
            data->art_probs[ART_IDX_BOOT_STEALTH]++;
        else if (art->tval == TV_CLOAK)
            data->art_probs[ART_IDX_CLOAK_STEALTH]++;
        else if (art_is_body_armor(art))
            data->art_probs[ART_IDX_ARMOR_STEALTH]++;
        else
        {
            /* General case */
            data->art_probs[ART_IDX_GEN_STEALTH]++;
        }
    }

    /* Handle searching bonus - fully generic this time */
    if (art->modifiers[OBJ_MOD_SEARCH] > 0)
        data->art_probs[ART_IDX_GEN_SEARCH]++;

    /* Handle infravision bonus - fully generic */
    if (art->modifiers[OBJ_MOD_INFRA] > 0)
        data->art_probs[ART_IDX_GEN_INFRA]++;

    /* Handle damage reduction bonus - fully generic */
    if (art->modifiers[OBJ_MOD_DAM_RED] > 0)
        data->art_probs[ART_IDX_GEN_DAM_RED]++;

    /* Handle moves bonus - fully generic */
    if (art->modifiers[OBJ_MOD_MOVES] > 0)
        data->art_probs[ART_IDX_GEN_MOVES]++;

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
    if (art->modifiers[OBJ_MOD_SPEED] > 0)
    {
        if (art->modifiers[OBJ_MOD_SPEED] > 7)
        {
            /* Supercharge case */
            data->art_probs[ART_IDX_GEN_SPEED_SUPER]++;
        }
        else if (art->tval == TV_BOOTS)
        {
            /* Handle boots separately */
            data->art_probs[ART_IDX_BOOT_SPEED]++;
        }
        else
            data->art_probs[ART_IDX_GEN_SPEED]++;
    }

    /* Handle permanent light */
    if (art->modifiers[OBJ_MOD_LIGHT] > 0)
        data->art_probs[ART_IDX_GEN_LIGHT]++;

    /* Handle mana bonus on gloves */
    if ((art->tval == TV_GLOVES) && (art->modifiers[OBJ_MOD_MANA] > 0))
        data->art_probs[ART_IDX_GLOVE_MANA]++;
}


/*
 * Count low resists and immunities.
 */
static void count_low_resists(const struct artifact *art, struct artifact_set_data *data)
{
    int num = 0;

    /* Count up immunities for this item, if any */
    if (art->el_info[ELEM_ACID].res_level == 3) num++;
    if (art->el_info[ELEM_ELEC].res_level == 3) num++;
    if (art->el_info[ELEM_FIRE].res_level == 3) num++;
    if (art->el_info[ELEM_COLD].res_level == 3) num++;
    data->art_probs[ART_IDX_GEN_IMMUNE] += num;

    /* Count up low resists (not the type, just the number) */
    num = 0;
    if (art->el_info[ELEM_ACID].res_level == 1) num++;
    if (art->el_info[ELEM_ELEC].res_level == 1) num++;
    if (art->el_info[ELEM_FIRE].res_level == 1) num++;
    if (art->el_info[ELEM_COLD].res_level == 1) num++;

    if (num)
    {
        /* Shields treated separately */
        if (art->tval == TV_SHIELD)
            data->art_probs[ART_IDX_SHIELD_LRES] += num;

        /* Armor also treated separately */
        else if (art_is_body_armor(art))
        {
            /* Special case: armor has all four low resists */
            if (num == 4)
                data->art_probs[ART_IDX_ARMOR_ALLRES]++;

            /* Just tally up the resists as usual */
            else
                data->art_probs[ART_IDX_ARMOR_LRES] += num;
        }

        /* General case */
        else
            data->art_probs[ART_IDX_GEN_LRES] += num;
    }
}


/*
 * Count high resists and protections.
 */
static void count_high_resists(const struct artifact *art, struct artifact_set_data *data)
{
    int num = 0;

    /*
     * If the item is body armor then count up all the high resists before
     * going through them individually. High resists are an important
     * component of body armor so we track probability for them separately.
     * The proportions of the high resists will be determined by the
     * generic frequencies - this number just tracks the total.
     */
    if (art_is_body_armor(art))
    {
        if (art->el_info[ELEM_POIS].res_level == 1) num++;
        if (of_has(art->flags, OF_PROT_FEAR)) num++;
        if (art->el_info[ELEM_LIGHT].res_level == 1) num++;
        if (art->el_info[ELEM_DARK].res_level == 1) num++;
        if (of_has(art->flags, OF_PROT_BLIND)) num++;
        if (of_has(art->flags, OF_PROT_CONF)) num++;
        if (art->el_info[ELEM_SOUND].res_level == 1) num++;
        if (art->el_info[ELEM_SHARD].res_level == 1) num++;
        if (art->el_info[ELEM_NEXUS].res_level == 1) num++;
        if (art->el_info[ELEM_NETHER].res_level == 1) num++;
        if (art->el_info[ELEM_CHAOS].res_level == 1) num++;
        if (art->el_info[ELEM_DISEN].res_level == 1) num++;
        if (of_has(art->flags, OF_PROT_STUN)) num++;
        data->art_probs[ART_IDX_ARMOR_HRES] += num;
    }

    /* Now do the high resists individually */
    if (art->el_info[ELEM_POIS].res_level == 1)
    {
        /* Resist poison ability */
        data->art_probs[ART_IDX_GEN_RPOIS]++;
    }

    if (of_has(art->flags, OF_PROT_FEAR))
    {
        /* Resist fear ability */
        data->art_probs[ART_IDX_GEN_RFEAR]++;
    }

    if (art->el_info[ELEM_LIGHT].res_level == 1)
    {
        /* Resist light ability */
        data->art_probs[ART_IDX_GEN_RLIGHT]++;
    }

    if (art->el_info[ELEM_DARK].res_level == 1)
    {
        /* Resist dark ability */
        data->art_probs[ART_IDX_GEN_RDARK]++;
    }

    if (of_has(art->flags, OF_PROT_BLIND))
    {
        /* Resist blind ability - helms/crowns are separate */
        if (art_is_head_armor(art))
            data->art_probs[ART_IDX_HELM_RBLIND]++;
        else
        {
            /* General case */
            data->art_probs[ART_IDX_GEN_RBLIND]++;
        }
    }

    if (of_has(art->flags, OF_PROT_CONF))
    {
        /* Resist confusion ability */
        data->art_probs[ART_IDX_GEN_RCONF]++;
    }

    if (art->el_info[ELEM_SOUND].res_level == 1)
    {
        /* Resist sound ability */
        data->art_probs[ART_IDX_GEN_RSOUND]++;
    }

    if (art->el_info[ELEM_SHARD].res_level == 1)
    {
        /* Resist shards ability */
        data->art_probs[ART_IDX_GEN_RSHARD]++;
    }

    if (art->el_info[ELEM_NEXUS].res_level == 1)
    {
        /* Resist nexus ability */
        data->art_probs[ART_IDX_GEN_RNEXUS]++;
    }

    if (art->el_info[ELEM_NETHER].res_level == 1)
    {
        /* Resist nether ability */
        data->art_probs[ART_IDX_GEN_RNETHER]++;
    }

    if (art->el_info[ELEM_CHAOS].res_level == 1)
    {
        /* Resist chaos ability */
        data->art_probs[ART_IDX_GEN_RCHAOS]++;
    }

    if (art->el_info[ELEM_DISEN].res_level == 1)
    {
        /* Resist disenchantment ability */
        data->art_probs[ART_IDX_GEN_RDISEN]++;
    }

    if (of_has(art->flags, OF_PROT_STUN))
    {
        /* Resist stunning ability */
        data->art_probs[ART_IDX_GEN_PSTUN]++;
    }
}


/*
 * General abilities. This section requires a bit more work
 * than the others, because we have to consider cases where
 * a certain ability might be found in a particular item type.
 * For example, ESP is commonly found on headgear, so when
 * we count ESP we must add it to either the headgear or
 * general tally, depending on the base item. This permits
 * us to have general abilities appear more commonly on a
 * certain item type.
 */
static void count_abilities(const struct artifact *art, struct artifact_set_data *data)
{
    int num = 0;
    struct object_kind *kind = lookup_kind(art->tval, art->sval);
    bitflag f2[OF_SIZE];

    create_obj_flag_mask(f2, 0, OFT_ESP, OFT_MAX);

    if (flags_test(art->flags, OF_SIZE, OF_SUST_STR, OF_SUST_INT, OF_SUST_WIS, OF_SUST_DEX,
        OF_SUST_CON, FLAG_END))
    {
        /* Now do sustains, in a similar manner */
        if (of_has(art->flags, OF_SUST_STR)) num++;
        if (of_has(art->flags, OF_SUST_INT)) num++;
        if (of_has(art->flags, OF_SUST_WIS)) num++;
        if (of_has(art->flags, OF_SUST_DEX)) num++;
        if (of_has(art->flags, OF_SUST_CON)) num++;
        data->art_probs[ART_IDX_GEN_SUST] += num;
    }

    if (of_has(art->flags, OF_FREE_ACT))
    {
        /* Free action - handle gloves separately */
        if (art->tval == TV_GLOVES)
            data->art_probs[ART_IDX_GLOVE_FA]++;
        else
            data->art_probs[ART_IDX_GEN_FA]++;
    }

    if (of_has(art->flags, OF_HOLD_LIFE))
    {
        /* Hold life - do body armor separately */
        if (art_is_body_armor(art))
            data->art_probs[ART_IDX_ARMOR_HLIFE]++;
        else
            data->art_probs[ART_IDX_GEN_HLIFE]++;
    }

    if (of_has(art->flags, OF_FEATHER))
    {
        /* Levitation - handle boots separately */
        if (art->tval == TV_BOOTS)
            data->art_probs[ART_IDX_BOOT_FEATHER]++;
        else
            data->art_probs[ART_IDX_GEN_FEATHER]++;
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
                data->art_probs[ART_IDX_HELM_SINV]++;
            else if (art_is_mstaff(art))
                data->art_probs[ART_IDX_MSTAFF_SINV]++;
            else
                data->art_probs[ART_IDX_GEN_SINV]++;
        }
    }

    if (of_is_inter(art->flags, f2))
    {
        /* ESP case. Add up the number of individual bonuses */
        num = 0;
        if (of_has(art->flags, OF_ESP_ORC)) num++;
        if (of_has(art->flags, OF_ESP_TROLL)) num++;
        if (of_has(art->flags, OF_ESP_GIANT)) num++;
        if (of_has(art->flags, OF_ESP_ANIMAL)) num++;
        if (of_has(art->flags, OF_ESP_DRAGON)) num++;
        if (of_has(art->flags, OF_ESP_DEMON)) num++;
        if (of_has(art->flags, OF_ESP_UNDEAD)) num++;
        if (of_has(art->flags, OF_ESP_EVIL)) num++;
        if (of_has(art->flags, OF_ESP_ALL)) num++;
        if (of_has(art->flags, OF_ESP_RADIUS)) num++;

        /* Handle helms/crowns separately. */
        if (art_is_head_armor(art))
            data->art_probs[ART_IDX_HELM_ESP] += num;
        else if (art_is_mstaff(art))
            data->art_probs[ART_IDX_MSTAFF_ESP] += num;
        else
            data->art_probs[ART_IDX_GEN_ESP] += num;
    }

    if (of_has(art->flags, OF_SLOW_DIGEST))
    {
        /* Slow digestion case - generic. */
        data->art_probs[ART_IDX_GEN_SDIG]++;
    }

    if (of_has(art->flags, OF_REGEN))
    {
        /* Regeneration case - generic. */
        data->art_probs[ART_IDX_GEN_REGEN]++;
    }

    if (of_has(art->flags, OF_TRAP_IMMUNE))
    {
        /* Trap immunity - handle boots separately */
        if (art->tval == TV_BOOTS)
            data->art_probs[ART_IDX_BOOT_TRAP_IMM]++;
        else
            data->art_probs[ART_IDX_GEN_TRAP_IMM]++;
    }

    if (of_has(art->flags, OF_KNOWLEDGE))
    {
        /* Handle helms/crowns separately. */
        if (art_is_head_armor(art))
            data->art_probs[ART_IDX_HELM_ID]++;
        else if (art->tval == TV_GLOVES)
        {
            /* Handle knowledge bonus on gloves */
            data->art_probs[ART_IDX_GLOVE_ID]++;
        }
    }

    if (art->activation || kind->activation)
    {
        /* Activation */
        data->art_probs[ART_IDX_GEN_ACTIV]++;
    }
}


/*
 * Parse the standard artifacts and count up the frequencies of the various
 * abilities.
 */
static void collect_artifact_data(struct artifact_set_data *data)
{
    size_t i;

    /* Go through the list of all artifacts */
    for (i = 0; i < (size_t)z_info->a_max; i++)
    {
        struct object_kind *kind;
        const struct artifact *art = &a_info[i];

        /* Don't parse cursed or null items */
        if ((art->base_power < 0) || (art->tval == 0)) continue;

        /* Get a pointer to the base item for this artifact */
        kind = lookup_kind(art->tval, art->sval);

        /* Special cases -- don't parse these! */
        if (strstr(art->name, "The One Ring") || kf_has(kind->kind_flags, KF_QUEST_ART) ||
            strstr(art->name, "of Maglor"))
            continue;

        /* Count combat abilities broken up by type */
        if (art_is_melee_weapon(art))
            count_weapon_abilities(art, data);
        else if (art->tval == TV_BOW)
            count_bow_abilities(art, data);
        else if (art_is_ammo(art))
            count_ammo_abilities(art, data);
        else
            count_nonweapon_abilities(art, data);

        /* Count other properties */
        count_modifiers(art, data);
        count_low_resists(art, data);
        count_high_resists(art, data);
        count_abilities(art, data);
    }
}


/*
 * Rescale the abilities so that dependent / independent abilities are
 * comparable. We do this by rescaling the frequencies for item-dependent
 * abilities as though the entire set was made up of that item type. For
 * example, if one bow out of three has extra might, and there are 120
 * artifacts in the full set, we rescale the frequency for extra might to
 * 40 (if we had 120 randart bows, about 40 would have extra might).
 *
 * This will allow us to compare the frequencies of all ability types,
 * no matter what the dependency. We assume that generic abilities (like
 * resist fear in the current version) don't need rescaling. This
 * introduces some inaccuracy in cases where specific instances of an
 * ability (like INT bonus on helms) have been counted separately -
 * ideally we should adjust for this in the general case. However, as
 * long as this doesn't occur too often, it shouldn't be a big issue.
 *
 * The following loops look complicated, but they are simply equivalent
 * to going through each of the relevant ability types one by one.
 */
static void rescale_freqs(struct artifact_set_data *data)
{
    size_t i;
    int temp;

    /* Bow-only abilities */
    for (i = 0; i < N_ELEMENTS(art_idx_bow); i++)
    {
        data->art_probs[art_idx_bow[i]] = (data->art_probs[art_idx_bow[i]] * data->total) /
            data->bow_total;
    }

    /* All weapon abilities */
    for (i = 0; i < N_ELEMENTS(art_idx_weapon); i++)
    {
        data->art_probs[art_idx_weapon[i]] = (data->art_probs[art_idx_weapon[i]] * data->total) /
            (data->bow_total + data->melee_total);
    }

    /* Corresponding non-weapon abilities */
    temp = data->total - data->melee_total - data->bow_total - data->missile_total;
    for (i = 0; i < N_ELEMENTS(art_idx_nonweapon); i++)
    {
        data->art_probs[art_idx_nonweapon[i]] = (data->art_probs[art_idx_nonweapon[i]] * data->total) /
            temp;
    }

    /* All melee weapon abilities */
    for (i = 0; i < N_ELEMENTS(art_idx_melee); i++)
    {
        data->art_probs[art_idx_melee[i]] = (data->art_probs[art_idx_melee[i]] * data->total) /
            data->melee_total;
    }

    /* All general armor abilities */
    temp = data->armor_total + data->boot_total + data->shield_total + data->headgear_total +
        data->cloak_total + data->glove_total;
    for (i = 0; i < N_ELEMENTS(art_idx_allarmor); i++)
    {
        data->art_probs[art_idx_allarmor[i]] = (data->art_probs[art_idx_allarmor[i]] * data->total) /
            temp;
    }

    /* Boots */
    for (i = 0; i < N_ELEMENTS(art_idx_boot); i++)
    {
        data->art_probs[art_idx_boot[i]] = (data->art_probs[art_idx_boot[i]] * data->total) /
            data->boot_total;
    }

    /* Gloves */
    for (i = 0; i < N_ELEMENTS(art_idx_glove); i++)
    {
        data->art_probs[art_idx_glove[i]] = (data->art_probs[art_idx_glove[i]] * data->total) /
            data->glove_total;
    }

    /* Headgear */
    for (i = 0; i < N_ELEMENTS(art_idx_headgear); i++)
    {
        data->art_probs[art_idx_headgear[i]] = (data->art_probs[art_idx_headgear[i]] * data->total) /
            data->headgear_total;
    }

    /* Shields */
    for (i = 0; i < N_ELEMENTS(art_idx_shield); i++)
    {
        data->art_probs[art_idx_shield[i]] = (data->art_probs[art_idx_shield[i]] * data->total) /
            data->shield_total;
    }

    /* Cloaks */
    for (i = 0; i < N_ELEMENTS(art_idx_cloak); i++)
    {
        data->art_probs[art_idx_cloak[i]] = (data->art_probs[art_idx_cloak[i]] * data->total) /
            data->cloak_total;
    }

    /* Body armor */
    for (i = 0; i < N_ELEMENTS(art_idx_armor); i++)
    {
        data->art_probs[art_idx_armor[i]] = (data->art_probs[art_idx_armor[i]] * data->total) /
            data->armor_total;
    }

    /* Mage staves */
    for (i = 0; i < N_ELEMENTS(art_idx_mstaff); i++)
    {
        data->art_probs[art_idx_mstaff[i]] = (data->art_probs[art_idx_mstaff[i]] * data->total) /
            data->mstaff_total;
    }

    /* Missiles */
    for (i = 0; i < N_ELEMENTS(art_idx_missile); i++)
    {
        data->art_probs[art_idx_missile[i]] = (data->art_probs[art_idx_missile[i]] * data->total) /
            data->missile_total;
    }

    /*
     * All others are general case and don't need to be rescaled,
     * unless the algorithm is getting too clever about separating
     * out individual cases (in which case some logic should be
     * added for them in the following method call).
     */
}


/*
 * Adjust the parsed frequencies for any peculiarities of the
 * algorithm. For example, if stat bonuses and sustains are
 * being added in a correlated fashion, it will tend to push
 * the frequencies up for both of them. In this method we
 * compensate for cases like this by applying corrective
 * scaling.
 */
static void adjust_freqs(struct artifact_set_data *data)
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
     * a given randart set. If this is a problem, raise the number.
     */
    if (data->art_probs[ART_IDX_GEN_RFEAR] < 5)
        data->art_probs[ART_IDX_GEN_RFEAR] = 5;
    if (data->art_probs[ART_IDX_MELEE_DICE_SUPER] < 5)
        data->art_probs[ART_IDX_MELEE_DICE_SUPER] = 5;
    if (data->art_probs[ART_IDX_BOW_SHOTS_SUPER] < 5)
        data->art_probs[ART_IDX_BOW_SHOTS_SUPER] = 5;
    if (data->art_probs[ART_IDX_BOW_MIGHT_SUPER] < 5)
        data->art_probs[ART_IDX_BOW_MIGHT_SUPER] = 5;
    if (data->art_probs[ART_IDX_MELEE_BLOWS_SUPER] < 5)
        data->art_probs[ART_IDX_MELEE_BLOWS_SUPER] = 5;
    if (data->art_probs[ART_IDX_GEN_SPEED_SUPER] < 5)
        data->art_probs[ART_IDX_GEN_SPEED_SUPER] = 5;
    if (data->art_probs[ART_IDX_GEN_AC] < 5)
        data->art_probs[ART_IDX_GEN_AC] = 5;
    if (data->art_probs[ART_IDX_GEN_TUNN] < 5)
        data->art_probs[ART_IDX_GEN_TUNN] = 5;
    if (data->art_probs[ART_IDX_NONWEAPON_BRAND] < 2)
        data->art_probs[ART_IDX_NONWEAPON_BRAND] = 2;
    if (data->art_probs[ART_IDX_NONWEAPON_SLAY] < 2)
        data->art_probs[ART_IDX_NONWEAPON_SLAY] = 2;
    if (data->art_probs[ART_IDX_BOW_BRAND] < 2)
        data->art_probs[ART_IDX_BOW_BRAND] = 2;
    if (data->art_probs[ART_IDX_BOW_SLAY] < 2)
        data->art_probs[ART_IDX_BOW_SLAY] = 2;
    if (data->art_probs[ART_IDX_NONWEAPON_BLOWS] < 2)
        data->art_probs[ART_IDX_NONWEAPON_BLOWS] = 2;
    if (data->art_probs[ART_IDX_NONWEAPON_SHOTS] < 2)
        data->art_probs[ART_IDX_NONWEAPON_SHOTS] = 2;
    if (data->art_probs[ART_IDX_GEN_AC_SUPER] < 5)
        data->art_probs[ART_IDX_GEN_AC_SUPER] = 5;
    if (data->art_probs[ART_IDX_MELEE_AC] < 5)
        data->art_probs[ART_IDX_MELEE_AC] = 5;
    if (data->art_probs[ART_IDX_GEN_PSTUN] < 3)
        data->art_probs[ART_IDX_GEN_PSTUN] = 3;
    if (data->art_probs[ART_IDX_MELEE_AC_SUPER] < 5)
        data->art_probs[ART_IDX_MELEE_AC_SUPER] = 5;

    /* Cut aggravation frequencies in half since they're used twice */
    data->art_probs[ART_IDX_NONWEAPON_AGGR] /= 2;
    data->art_probs[ART_IDX_WEAPON_AGGR] /= 2;

    /* PWMAngband */
    data->art_probs[ART_IDX_DIGGER_TUNN] = 4;
    data->art_probs[ART_IDX_MSTAFF_FA] = 10;
    data->art_probs[ART_IDX_MSTAFF_RBLIND] = 10;
    data->art_probs[ART_IDX_MSTAFF_RCONF] = 10;
    if (data->art_probs[ART_IDX_GLOVE_MANA] < 5)
        data->art_probs[ART_IDX_GLOVE_MANA] = 5;
    if (data->art_probs[ART_IDX_GLOVE_ID] < 5)
        data->art_probs[ART_IDX_GLOVE_ID] = 5;
    if (data->art_probs[ART_IDX_HELM_ID] < 5)
        data->art_probs[ART_IDX_HELM_ID] = 5;
    if (data->art_probs[ART_IDX_MISSILE_DICE_SUPER] < 5)
        data->art_probs[ART_IDX_MISSILE_DICE_SUPER] = 5;
}


/*
 * Parse the artifacts and write frequencies of their abilities and
 * base object kinds.
 *
 * This is used to give dynamic generation probabilities.
 */
static void parse_frequencies(struct artifact_set_data *data)
{
    size_t i;

    /* Zero the frequencies for artifact attributes */
    for (i = 0; i < ART_IDX_TOTAL; i++) data->art_probs[i] = 0;

    collect_artifact_data(data);

    /* Rescale frequencies */
    rescale_freqs(data);

    /* Perform any additional rescaling and adjustment, if required. */
    adjust_freqs(data);
}


/*
 * Generation of a random artifact
 */


static int mod_light(struct artifact *a)
{
    if (!art_is_light(a)) return 0;

    return a->modifiers[OBJ_MOD_LIGHT];
}


/*
 * Add basic data to an artifact of a given object kind
 */
static void artifact_prep(struct artifact *art, const struct object_kind *kind,
    struct artifact_set_data *data)
{
    int i, mod = mod_light(art);

    /* Keep base attributes */
    art->to_h = randcalc(kind->to_h, 0, MINIMISE);
    art->to_d = randcalc(kind->to_d, 0, MINIMISE);
    art->to_a = randcalc(kind->to_a, 0, MINIMISE);
    art->ac = kind->ac;
    art->dd = kind->dd;
    art->ds = kind->ds;
    art->weight = kind->weight;
    of_copy(art->flags, kind->flags);
    if (kind->slays)
    {
        size_t array_size = z_info->slay_max * sizeof(bool);

        art->slays = mem_zalloc(array_size);
        memcpy(art->slays, kind->slays, array_size);
    }
    if (kind->brands)
    {
        size_t array_size = z_info->brand_max * sizeof(bool);

        art->brands = mem_zalloc(array_size);
        memcpy(art->brands, kind->brands, array_size);
    }
    if (kind->curses)
    {
        size_t array_size = z_info->curse_max * sizeof(int);

        art->curses = mem_zalloc(array_size);
        memcpy(art->curses, kind->curses, array_size);
    }
    for (i = 0; i < OBJ_MOD_MAX; i++)
        art->modifiers[i] = randcalc(kind->modifiers[i], 0, MINIMISE);
    for (i = 0; i < ELEM_MAX; i++)
    {
        art->el_info[i].res_level = kind->el_info[i].res_level;
        art->el_info[i].flags = kind->el_info[i].flags;
    }

    /* Assign basic stats to the artifact */
    switch (art->tval)
    {
        case TV_BOW:
        case TV_HAFTED:
        case TV_SWORD:
        case TV_POLEARM:
        {
            /* Dark swords never get enchantments */
            if (!art_is_dark_sword(art))
            {
                art->to_h += (s16b)(data->hit_startval / 2 + randint0(data->hit_startval));
                art->to_d += (s16b)(data->dam_startval / 2 + randint0(data->dam_startval));
            }
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
            art->to_a += (s16b)(data->ac_startval / 2 + randint0(data->ac_startval));
            break;
        }

        case TV_RING:
        case TV_AMULET:
        case TV_LIGHT:
        {
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

            /* Lights get some extra properties */
            if (art_is_light(art))
            {
                of_on(art->flags, OF_NO_FUEL);
                art->modifiers[OBJ_MOD_LIGHT] = mod;
            }

            break;
        }
    }

    /* Clear the activations */
    art->activation = NULL;
    memset(&art->time, 0, sizeof(random_value));

    /* Artifacts ignore everything */
    for (i = ELEM_BASE_MIN; i < ELEM_HIGH_MIN; i++)
        art->el_info[i].flags |= EL_INFO_IGNORE;

    /* Artifact missiles always return when thrown */
    if (art_is_ammo(art))
        of_on(art->flags, OF_AMMO_MAGIC);

    /* Artifact dark swords always get maximum antimagic field */
    if (art_is_dark_sword(art)) art->modifiers[OBJ_MOD_ANTI_MAGIC] = 4;

    /* Artifact crystal DSMs get mana resistance */
    if ((art->tval == TV_DRAG_ARMOR) &&
        (art->sval == lookup_sval(art->tval, "Crystal Dragon Scale Mail")))
    {
        art->el_info[ELEM_MANA].res_level = 1;
    }
}


/*
 * Build a suitable frequency table for this item, based on the generated
 * frequencies. The frequencies for any abilities that don't apply for
 * this item type will be set to zero. First parameter is the artifact
 * for which to generate the frequency table.
 *
 * The second input parameter is a pointer to an array that the function
 * will use to store the frequency table. The array must have size
 * ART_IDX_TOTAL.
 *
 * The resulting frequency table is cumulative for ease of use in the
 * weighted randomization algorithm.
 */
static void build_freq_table(struct artifact *art, int *freq, struct artifact_set_data *data)
{
    int i;
    size_t j;
    int f_temp[ART_IDX_TOTAL];

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
            f_temp[art_idx_bow[j]] = data->art_probs[art_idx_bow[j]];
    }

    /* General weapon abilities */
    if (art_is_weapon(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_weapon); j++)
            f_temp[art_idx_weapon[j]] = data->art_probs[art_idx_weapon[j]];
    }

    /* General non-weapon abilities */
    /* PWMAngband: ammo don't get non-weapon abilities */
    else if (!art_is_ammo(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_nonweapon); j++)
            f_temp[art_idx_nonweapon[j]] = data->art_probs[art_idx_nonweapon[j]];
    }

    /* General melee abilities */
    if (art_is_melee_weapon(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_melee); j++)
            f_temp[art_idx_melee[j]] = data->art_probs[art_idx_melee[j]];
    }

    /* General armor abilities */
    if (art_is_armor(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_allarmor); j++)
            f_temp[art_idx_allarmor[j]] = data->art_probs[art_idx_allarmor[j]];
    }

    /* Boot abilities */
    if (art->tval == TV_BOOTS)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_boot); j++)
            f_temp[art_idx_boot[j]] = data->art_probs[art_idx_boot[j]];
    }

    /* Glove abilities */
    if (art->tval == TV_GLOVES)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_glove); j++)
            f_temp[art_idx_glove[j]] = data->art_probs[art_idx_glove[j]];
    }

    /* Headgear abilities */
    if (art_is_head_armor(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_headgear); j++)
            f_temp[art_idx_headgear[j]] = data->art_probs[art_idx_headgear[j]];
    }

    /* Shield abilities */
    if (art->tval == TV_SHIELD)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_shield); j++)
            f_temp[art_idx_shield[j]] = data->art_probs[art_idx_shield[j]];
    }

    /* Cloak abilities */
    if (art->tval == TV_CLOAK)
    {
        for (j = 0; j < N_ELEMENTS(art_idx_cloak); j++)
            f_temp[art_idx_cloak[j]] = data->art_probs[art_idx_cloak[j]];
    }

    /* Armor abilities */
    if (art_is_body_armor(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_armor); j++)
            f_temp[art_idx_armor[j]] = data->art_probs[art_idx_armor[j]];
    }

    /* Digger abilities */
    if (art_is_digger(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_digger); j++)
            f_temp[art_idx_digger[j]] = data->art_probs[art_idx_digger[j]];
    }

    /* Mage staff abilities */
    if (art_is_mstaff(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_mstaff); j++)
            f_temp[art_idx_mstaff[j]] = data->art_probs[art_idx_mstaff[j]];
    }

    /* Missile abilities */
    if (art_is_ammo(art))
    {
        for (j = 0; j < N_ELEMENTS(art_idx_missile); j++)
            f_temp[art_idx_missile[j]] = data->art_probs[art_idx_missile[j]];
    }

    /* PWMAngband: missiles have only specific abilities */
    else
    {
        /* General abilities - no constraint */
        for (j = 0; j < N_ELEMENTS(art_idx_gen); j++)
            f_temp[art_idx_gen[j]] = data->art_probs[art_idx_gen[j]];
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
 * Try to supercharge this item by running through the list of the supercharge
 * abilities and attempting to add each in turn. An artifact only gets one
 * chance at each of these up front (if applicable).
 */
static void try_supercharge(struct artifact *art, int target_power, struct artifact_set_data *data)
{
    bool weapon, other;

    /* Huge damage dice - missiles only */
    if (art_is_ammo(art))
    {
        if (randint0(z_info->a_max) < data->art_probs[ART_IDX_MISSILE_DICE_SUPER])
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
        if (randint0(z_info->a_max) < data->art_probs[ART_IDX_MELEE_DICE_SUPER])
        {
            art->dd += 3 + randint0(4);
            if (art->dd > MAX_WEAPON_DICE) art->dd = MAX_WEAPON_DICE;
        }

        /* Blows */
        else if (randint0(z_info->a_max) < data->art_probs[ART_IDX_MELEE_BLOWS_SUPER])
        {
            if (!art->modifiers[OBJ_MOD_BLOWS])
                art->modifiers[OBJ_MOD_BLOWS] = INHIBIT_BLOWS - 1;
        }
    }

    /* Bows - max might or shots */
    if (art->tval == TV_BOW)
    {
        if (randint0(z_info->a_max) < data->art_probs[ART_IDX_BOW_SHOTS_SUPER])
        {
            if (!art->modifiers[OBJ_MOD_SHOTS])
                art->modifiers[OBJ_MOD_SHOTS] = INHIBIT_SHOTS - 1;
        }
        else if (randint0(z_info->a_max) < data->art_probs[ART_IDX_BOW_MIGHT_SUPER])
        {
            if (!art->modifiers[OBJ_MOD_MIGHT])
                art->modifiers[OBJ_MOD_MIGHT] = INHIBIT_MIGHT - 1;
        }
    }

    /* Big speed bonus - any item (potentially) but more likely on boots */
    if ((randint0(z_info->a_max) < data->art_probs[ART_IDX_GEN_SPEED_SUPER]) ||
        ((art->tval == TV_BOOTS) && (randint0(z_info->a_max) < data->art_probs[ART_IDX_BOOT_SPEED])))
    {
        int value = 5 + randint0(6);

        if (INHIBIT_WEAK) value += randint1(3);
        if (INHIBIT_STRONG) value += 1 + randint1(6);
        art->modifiers[OBJ_MOD_SPEED] = value;
    }

    /* Big AC bonus (except on dark swords and bows) */
    weapon = art_is_melee_weapon(art) && !art_is_dark_sword(art) &&
        (randint0(z_info->a_max) < data->art_probs[ART_IDX_MELEE_AC_SUPER]);
    other = !art_is_weapon(art) && (randint0(z_info->a_max) < data->art_probs[ART_IDX_GEN_AC_SUPER]);
    if (weapon || other)
    {
        art->to_a += 19 + randint1(11);
        if (INHIBIT_WEAK) art->to_a += randint1(10);
        if (INHIBIT_STRONG) art->to_a += randint1(20);
    }

    /* Aggravation */
    weapon = art_is_weapon(art) && (randint0(z_info->a_max) < data->art_probs[ART_IDX_WEAPON_AGGR]);
    other = !art_is_weapon(art) && (randint0(z_info->a_max) < data->art_probs[ART_IDX_NONWEAPON_AGGR]);
    if ((weapon || other) && (target_power > AGGR_POWER))
        of_on(art->flags, OF_AGGRAVATE);
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
 * Adds, or increases the positive value of, or decreases the negative value
 * of, a modifier to an artifact.
 */
static void add_mod(struct artifact *art, int mod)
{
    /* Blows, might, shots, moves need special treatment */
    bool powerful = ((mod == OBJ_MOD_BLOWS) || (mod == OBJ_MOD_MIGHT) || (mod == OBJ_MOD_SHOTS) ||
        (mod == OBJ_MOD_MOVES));

    /* This code aims to favour a few larger bonuses over many small ones */
    if (art->modifiers[mod] < 0)
    {
        /* Negative mods just get a bit worse */
        if (one_in_(2)) art->modifiers[mod]--;
    }
    else if (powerful)
    {
        /* Powerful mods need to be applied sparingly */
        if (art->modifiers[mod] == 0)
            art->modifiers[mod] = (INHIBIT_STRONG? 3: randint1(2));
        else if (one_in_(2 * art->modifiers[mod]))
            art->modifiers[mod]++;
    }
    else
    {
        /* One-time bonus to anti-magic */
        if (mod == OBJ_MOD_ANTI_MAGIC)
        {
            if (art->modifiers[mod] == 0) art->modifiers[mod] = randint1(randint1(4));
        }

        /* New mods average 3, old ones are incremented by 1 or 2 */
        else if (art->modifiers[mod] == 0)
        {
            int value;

            /* PWMAngband: mana handled separately */
            if (mod == OBJ_MOD_MANA) value = 5 + randint0(6);
            else value = randint0(3) + randint1(3);

            art->modifiers[mod] = value;
        }
        else
            art->modifiers[mod] += randint1(2);
    }

    /* Hard cap of 6 on most mods */
    if ((mod != OBJ_MOD_SPEED) && (mod != OBJ_MOD_MANA) && (art->modifiers[mod] >= 6))
        art->modifiers[mod] = 6;
}


/*
 * Adds, or increases a stat modifier (probably)
 */
static void add_stat(struct artifact *art)
{
    int tries = 20;

    while (tries)
    {
        int r = randint0(STAT_MAX);

        if (art->modifiers[OBJ_MOD_STR + r] == 0)
        {
            add_mod(art, OBJ_MOD_STR + r);
            return;
        }

        tries--;
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
static void add_high_resist(struct artifact *art, struct artifact_set_data *data)
{
    size_t i;
    int r, temp;
    int count = 0;
    bool success = false;

    /* Add a high resist, according to the generated frequency distribution. */
    temp = 0;
    for (i = 0; i < N_ELEMENTS(art_idx_high_resist); i++)
        temp += data->art_probs[art_idx_high_resist[i]];

    /* The following will fail (cleanly) if all high resists already added */
    while (!success && (count < MAX_TRIES))
    {
        /* Randomize from 1 to this total amount */
        r = randint1(temp);

        /* Determine which (weighted) resist this number corresponds to */
        temp = data->art_probs[art_idx_high_resist[0]];
        i = 0;
        while ((r > temp) && (i < N_ELEMENTS(art_idx_high_resist)))
        {
            temp += data->art_probs[art_idx_high_resist[i]];
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
    struct brand *brand;

    /* Hack -- do not allow slays/brands on nonweapons other than rings and gloves */
    if (!art_is_weapon(art) && !art_is_ammo(art) && !art_is_ring(art) && (art->tval != TV_GLOVES))
        return;

    /* Mostly only one brand */
    if (art->brands && randint0(4)) return;

    /* Get a random brand */
    for (count = 0; count < MAX_TRIES; count++)
    {
        if (!append_random_brand(&art->brands, &brand, art_is_ammo(art))) continue;
        if (art_is_ammo(art)) break;

        /* Frequently add the corresponding resist */
        if (randint0(4))
        {
            size_t i;

            for (i = ELEM_BASE_MIN; i < ELEM_HIGH_MIN; i++)
            {
                if (streq(brand->name, projections[i].name) && (art->el_info[i].res_level <= 0))
                    add_resist(art, i);
            }
        }

        break;
    }
}


/*
 * Adds a slay, if possible
 */
static void add_slay(struct artifact *art)
{
    int count;
    struct slay *slay;
    int i;

    /* Hack -- do not allow slays/brands on nonweapons other than rings and gloves */
    if (!art_is_weapon(art) && !art_is_ammo(art) && !art_is_ring(art) && (art->tval != TV_GLOVES))
        return;

    /* PWMAngband: only one of KILL_XXX or SLAY_EVIL on ammo */
    if (art_is_ammo(art))
    {
        for (i = 0, count = 0; art->slays && (i < z_info->slay_max); i++)
        {
            if (!art->slays[i]) continue;
            slay = &slays[i];
            if ((slay->multiplier == 5) || streq(slay->name, "evil creatures")) count++;
        }
        if (count == 1) return;
    }

    for (count = 0; count < MAX_TRIES; count++)
    {
        if (!append_random_slay(&art->slays, &slay)) continue;
        if (art_is_ammo(art)) break;

        /* Frequently add more slays if the first choice is weak */
        if (randint0(4) && (slay->power < 105))
            add_slay(art);

        /* PWMAngband: chance of ESP_XXX */
        if (one_in_(slay->esp_chance))
            of_on(art->flags, slay->esp_flag);

        break;
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
    /* Mage weapons and dark swords are always +0 +0 */
    if (art_is_mstaff(art) || art_is_dark_sword(art)) return;

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
}


/*
 * Adds to_d, if not too high already
 */
static void add_to_dam(struct artifact *art, int fixed, int random)
{
    /* Mage weapons and dark swords are always +0 +0 */
    if (art_is_mstaff(art) || art_is_dark_sword(art)) return;

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

    art->to_d += (s16b)(fixed + randint0(random));
}


/*
 * Adds to_a, if not too high already
 */
static void add_to_AC(struct artifact *art, int fixed, int random)
{
    /* Dark swords never get enchantments */
    if (art_is_dark_sword(art)) return;

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


/*
 * Add an activation (called only if neither artifact nor base item has one)
 */
static void add_activation(struct artifact *art, int target_power, int max_power)
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
         * Check that activation is useful but not exploitable, and roughly
         * proportionate to the overall power
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

    /* No extra ESP power if OF_ESP_ALL is already present */
    if (of_has(flags, OF_ESP_ALL)) return 0;

    create_obj_flag_mask(newf, 0, OFT_ESP, OFT_MAX);

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

        /*
         * Skip this one if the flag is redundant
         *
         * Note:
         *   - all RF_ORC monsters are also RF_EVIL, but OF_ESP_ORC also detects PF_ORC players
         *   - all RF_TROLL monsters are also RF_EVIL, but OF_ESP_TROLL also detects PF_TROLL players
         *   - all RF_GIANT monsters are also RF_EVIL, but OF_ESP_GIANT also detects PF_GIANT players
         *   - all RF_DRAGON monsters (except Drolem which is RF_EMPTY_MIND) are also RF_EVIL,
         *     but OF_ESP_DRAGON also detects PF_THUNDERLORD and PF_DRAGON players
         *   - all RF_DEMON monsters (except Bronze golem which is RF_EMPTY_MIND) are also RF_EVIL,
         *     so OF_ESP_DEMON is redundant if OF_ESP_EVIL is already present
         *   - all RF_UNDEAD monsters (except Stairway to heaven and Bone golem which are RF_EMPTY_MIND)
         *     are also RF_EVIL, so OF_ESP_UNDEAD is redundant if OF_ESP_EVIL is already present
         */
        if ((i == OF_ESP_DEMON) && of_has(flags, OF_ESP_EVIL)) continue;
        if ((i == OF_ESP_UNDEAD) && of_has(flags, OF_ESP_EVIL)) continue;

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
 * Choose a random ability using weights based on the given cumulative frequency
 * table. A pointer to the frequency array (which must be of size ART_IDX_TOTAL)
 * is passed as a parameter. The function returns a number representing the
 * index of the ability chosen.
 */
static int choose_ability(int *freq_table)
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
 * Add an ability given by the index r. This is mostly just a long case
 * statement.
 *
 * Note that this method is totally general and imposes no restrictions on
 * appropriate item type for a given ability. This is assumed to have
 * been done already.
 */
static void add_ability_aux(struct artifact *art, int r, int target_power,
    struct artifact_set_data *data)
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
        case ART_IDX_NONWEAPON_HIT:
            add_to_hit(art, 1, 2 * data->hit_increment);
            break;

        case ART_IDX_WEAPON_DAM:
        case ART_IDX_NONWEAPON_DAM:
            add_to_dam(art, 1, 2 * data->dam_increment);
            break;

        case ART_IDX_NONWEAPON_HIT_DAM:
        case ART_IDX_GLOVE_HIT_DAM:
            add_to_hit(art, 1, 2 * data->hit_increment);
            add_to_dam(art, 1, 2 * data->dam_increment);
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
        case ART_IDX_MISSILE_SLAY:
            add_slay(art);
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
            add_to_AC(art, 1, 2 * data->ac_increment);
            break;

        case ART_IDX_MELEE_DICE:
            add_damage_dice(art);
            break;

        case ART_IDX_MELEE_WEIGHT:
        case ART_IDX_ALLARMOR_WEIGHT:
            add_weight_mod(art);
            break;

        case ART_IDX_MELEE_TUNN:
        case ART_IDX_DIGGER_TUNN:
        case ART_IDX_GEN_TUNN:
            add_mod(art, OBJ_MOD_TUNNEL);
            break;

        case ART_IDX_MELEE_ANTIMAGIC:
            add_mod(art, OBJ_MOD_ANTI_MAGIC);
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
            add_mod(art, OBJ_MOD_SPEED);
            break;

        case ART_IDX_GLOVE_FA:
        case ART_IDX_GEN_FA:
        case ART_IDX_MSTAFF_FA:
            add_flag(art, OF_FREE_ACT);
            break;

        case ART_IDX_GLOVE_DEX:
            if (art->modifiers[OBJ_MOD_DEX] == 0) add_mod(art, OBJ_MOD_DEX);
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
            if (art->modifiers[OBJ_MOD_WIS] == 0) add_mod(art, OBJ_MOD_WIS);
            break;

        case ART_IDX_HELM_INT:
        case ART_IDX_MSTAFF_INT:
            if (art->modifiers[OBJ_MOD_INT] == 0) add_mod(art, OBJ_MOD_INT);
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
            if (art->modifiers[OBJ_MOD_CON] == 0) add_mod(art, OBJ_MOD_CON);
            break;

        case ART_IDX_ARMOR_ALLRES:
            add_resist(art, ELEM_ACID);
            add_resist(art, ELEM_ELEC);
            add_resist(art, ELEM_FIRE);
            add_resist(art, ELEM_COLD);
            break;

        case ART_IDX_ARMOR_HRES:
            add_high_resist(art, data);
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
            if (art->modifiers[OBJ_MOD_LIGHT] == 0) art->modifiers[OBJ_MOD_LIGHT] = 1;
            break;

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

        case ART_IDX_BOOT_TRAP_IMM:
        case ART_IDX_GEN_TRAP_IMM:
            add_flag(art, OF_TRAP_IMMUNE);
            break;

        case ART_IDX_GEN_DAM_RED:
            add_mod(art, OBJ_MOD_DAM_RED);
            break;

        case ART_IDX_GEN_MOVES:
            add_mod(art, OBJ_MOD_MOVES);
            break;

        case ART_IDX_GEN_ACTIV:
        {
            struct object_kind *kind = lookup_kind(art->tval, art->sval);

            if (!art->activation && !kind->activation)
                add_activation(art, target_power, data->max_power);
            break;
        }
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

    if (of_has(art->flags, OF_DRAIN_EXP)) of_off(art->flags, OF_HOLD_LIFE);

    /*
     * ESP evil bypasses ESP demon
     *
     * Note: although all orcs/trolls/giants/dragons/undead monsters are evil, ESP evil cannot
     * bypass ESP orc/troll/giant/dragon/undead because we need them for players.
     *
     * - ESP orc detects Half-Orc players
     * - ESP troll detects Half-Troll players
     * - ESP giant detects Ent players
     * - ESP dragon detects Thunderlord and Dragon players
     * - ESP undead detects ghost players
     */
    if (of_has(art->flags, OF_ESP_EVIL)) of_off(art->flags, OF_ESP_DEMON);

    /* ESP all bypasses all other ESPs */
    if (of_has(art->flags, OF_ESP_ALL))
    {
        bitflag f2[OF_SIZE];

        create_obj_flag_mask(f2, 0, OFT_ESP, OFT_MAX);
        of_diff(art->flags, f2);
        of_on(art->flags, OF_ESP_ALL);
    }
}


/*
 * Randomly select an extra ability to be added to the artifact in question.
 */
static void add_ability(struct artifact *art, int target_power, int *freq,
    struct artifact_set_data *data)
{
    int r;

    /* Choose a random ability using the frequency table previously defined */
    r = choose_ability(freq);

    /* Add the appropriate ability */
    add_ability_aux(art, r, target_power, data);

    /* Adding WIS to sharp weapons always blesses them */
    if ((art->modifiers[OBJ_MOD_WIS] > 0) && art_is_pointy(art))
        add_flag(art, OF_BLESSED);
}


/*
 * Make it bad, or if it's already bad, make it worse!
 */
static void make_bad(struct artifact *art)
{
    int i;

    /* Add bad abilities */
    if (one_in_(7)) of_on(art->flags, OF_AGGRAVATE);
    if (one_in_(4)) of_on(art->flags, OF_DRAIN_EXP);
    if (one_in_(7)) of_on(art->flags, OF_NO_TELEPORT);

    /* Reverse mods and bonuses */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if ((art->modifiers[i] > 0) && one_in_(2))
            art->modifiers[i] = 0 - art->modifiers[i];
    }
    if ((art->to_a > 0) && one_in_(2)) art->to_a = 0 - art->to_a;
    if ((art->to_h > 0) && one_in_(2)) art->to_h = 0 - art->to_h;
    if ((art->to_d > 0) && one_in_(4)) art->to_d = 0 - art->to_d;

    /* Curse it */
    append_artifact_curse(art, art->level, art->tval);
}


/*
 * Copy artifact fields from source to dest
 */
static void copy_artifact(struct artifact *dest, struct artifact *source)
{
    /* Free pointers */
    mem_free(dest->brands);
    mem_free(dest->slays);
    mem_free(dest->curses);

    /* Copy info from the source artifact (this includes pointers!) */
    memcpy(dest, source, sizeof(*source));

    /* Reset pointers */
    dest->brands = NULL;
    dest->slays = NULL;
    dest->curses = NULL;

    /* Copy pointers from the source artifact */
    if (source->slays)
    {
        size_t array_size = z_info->slay_max * sizeof(bool);

        dest->slays = mem_zalloc(array_size);
        memcpy(dest->slays, source->slays, array_size);
    }
    if (source->brands)
    {
        size_t array_size = z_info->brand_max * sizeof(bool);

        dest->brands = mem_zalloc(array_size);
        memcpy(dest->brands, source->brands, array_size);
    }
    if (source->curses)
    {
        size_t array_size = z_info->curse_max * sizeof(int);

        dest->curses = mem_zalloc(array_size);
        memcpy(dest->curses, source->curses, array_size);
    }
}


/*
 * Generation of a set of random artifacts
 */


/*
 * Use W. Sheldon Simms' random name generator.
 */
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
 * Design a random artifact given a tval
 *
 * The artifact is assigned a power based on the range of powers for that tval
 * in the original artifact set. It is then given a base item type which is
 * suitable for that power, and than has properties added to it until it falls
 * close enough to the target power - currently this means between 19/20 and
 * 23/20 of the target power.
 */
static bool design_artifact(struct artifact *art, struct artifact_set_data *data)
{
    struct object_kind *kind = lookup_kind(art->tval, art->sval);
    int art_freq[ART_IDX_TOTAL];
    int tries;
    int ap = 0;
    bool hurt_me = false;

    /* Structure to hold the old artifact */
    struct artifact *a_old = mem_zalloc(sizeof(*a_old));

    /* Choose a power for the artifact */
    int power = Rand_sample(data->avg_tv_power[art->tval], data->max_tv_power[art->tval],
        data->min_tv_power[art->tval], 20, 20);

    /* Choose a name */
    /* PWMAngband: easier to regenerate the name from the randart seed when needed */

    /* Skip fixed artifacts */
    if (strstr(art->name, "The One Ring") || kf_has(kind->kind_flags, KF_QUEST_ART) ||
        strstr(art->name, "of Maglor"))
    {
        mem_free(a_old);
        return false;
    }

    /* Flip the sign on power if it's negative (unlikely) and damage */
    if (power < 0)
    {
        hurt_me = true;
        power = 0 - power;
    }

    /* Get the new item kind and do basic prep on it */
    /* PWMAngband: keep the item kind */
    artifact_prep(art, kind, data);

    /* Generate the cumulative frequency table for this base item type */
    build_freq_table(art, art_freq, data);

    /* Copy artifact info temporarily. */
    copy_artifact(a_old, art);

    /* Give this artifact a shot at being supercharged */
    try_supercharge(art, power, data);
    ap = artifact_power(art);
    if (ap > (power * 23) / 20 + 1)
    {
        /* Too powerful -- put it back */
        copy_artifact(art, a_old);
    }

    /* Give this artifact a chance to be cursed - note it retains its power */
    if (one_in_(z_info->a_max / MAX(2, data->neg_power_total)))
        hurt_me = true;

    /* Do the actual artifact design */
    for (tries = 0; tries < MAX_TRIES; tries++)
    {
        /* Copy artifact info temporarily. */
        copy_artifact(a_old, art);

        /* Add an ability */
        add_ability(art, power, art_freq, data);

        /* Curse the designated artifacts */
        if (hurt_me) make_bad(art);

        /* Now remove contradictory or redundant powers. */
        remove_contradictory(art);

        /* Check the power, handle negative power */
        ap = artifact_power(art);
        if (hurt_me)
        {
            /* Accept if negative */
            if (ap < 0) break;

            /* Otherwise go back and try again */
            copy_artifact(art, a_old);
            continue;
        }

        /* Too powerful -- put it back */
        if (ap > (power * 23) / 20 + 1)
        {
            copy_artifact(art, a_old);
            continue;
        }

        /* Just right */
        if (ap >= (power * 19) / 20)
        {
            /* Add rescue for crappy weapons */
            if (art_is_weapon(art) && !art_is_dark_sword(art) && (art->to_d < 10))
                art->to_d += randint0(10);

            break;
        }
    }

    /* Cleanup a_old */
    free_artifact(a_old);

    /* Couldn't generate an artifact with the number of permitted iterations */
    if (tries >= MAX_TRIES) return false;

    /* Flip cursed items to avoid overflows */
    if (ap < 0) ap = 0 - ap;

    if (art->aidx < (u32b)z_info->a_max)
    {
        /* Set rarity based on power */
        art->alloc_prob = 4000000 / (ap * ap);
        art->alloc_prob /= (kind->alloc_prob? kind->alloc_prob: 20);
        if (art->alloc_prob > 99) art->alloc_prob = 99;
        if (art->alloc_prob < 1) art->alloc_prob = 1;

        /* Set depth according to power */
        art->alloc_max = MIN(127, (ap * 3) / 5);
        art->alloc_min = MIN(100, ((ap + 100) * 100 / data->max_power));

        /* Have a chance to be less rare or deep, more likely the less power */
        if (one_in_(5 + power / 20))
        {
            art->alloc_prob += randint1(20);
            if (art->alloc_prob > 99) art->alloc_prob = 99;
        }
        else if (one_in_(5 + power / 20))
        {
            art->alloc_min /= 2;
            if (art->alloc_min < 1) art->alloc_min = 1;
        }

        /* Sanity check */
        art->alloc_max = MAX(art->alloc_max, MIN(art->alloc_min * 2, 127));
    }

    /* Success */
    return true;
}


/* Dummy name for randart Rings of Power */
static char dummy[6] = "dummy";


/*
 * Create a random artifact
 */
static struct artifact* create_artifact(struct artifact *a, struct artifact_set_data *data)
{
    struct artifact *art = mem_zalloc(sizeof(*art));

    /* Copy info from the corresponding static artifact */
    memcpy(art, a, sizeof(*a));

    /* Reset pointers */
    art->brands = NULL;
    art->slays = NULL;
    art->curses = NULL;

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
        art->base_power = AGGR_POWER * (70 + randint0(41)) / 100;
    }

    /* Randomize the artifact */
    if (!design_artifact(art, data))
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
    art = create_artifact(a, &local_data);

    /* When done, resume use of the Angband "complex" RNG. */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;

    /* Return the random artifact */
    return art;
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


/*
 * Initialize the random artifact generator
 */
void init_randart_generator(void)
{
    memset(&local_data, 0, sizeof(local_data));

    /*
     * Mean start and increment values for to_hit, to_dam and AC. Update these
     * if the algorithm changes. They are used in frequency generation.
     */
    local_data.hit_increment = 4;
    local_data.dam_increment = 4;
    local_data.hit_startval = 10;
    local_data.dam_startval = 10;
    local_data.ac_startval = 15;
    local_data.ac_increment = 5;

    /* Store the original power ratings */
    store_base_power(&local_data);

    /* Determine the generation probabilities */
    parse_frequencies(&local_data);
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
    mem_free(art->slays);
    mem_free(art->brands);
    mem_free(art->curses);
    mem_free(art);
}
