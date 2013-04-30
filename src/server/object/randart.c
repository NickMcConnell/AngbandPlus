/*
 * File: randart.c
 * Purpose: Random artifact generation
 *
 * Copyright (c) 1998 Greg Wooledge, Ben Harrison, Robert Ruhlmann
 * Copyright (c) 2001 Chris Carr, Chris Robertson
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
#include "../../common/randname.h"
#include "../../common/tvalsval.h"
#include "../effects.h"
#include "slays.h"

/*
 * Original random artifact generator (randart) by Greg Wooledge.
 * Updated by Chris Carr / Chris Robertson 2001-2010.
 */

#define MAX_TRIES 200

#define MIN_RNAME_LEN 5
#define MAX_RNAME_LEN 9

/*
 * Inhibiting factors for large bonus values
 * "HIGH" values use INHIBIT_WEAK
 * "VERYHIGH" values use INHIBIT_STRONG
 */
#define INHIBIT_STRONG  one_in_(6)
#define INHIBIT_WEAK    one_in_(2)

/*
 * Power rating below which uncursed randarts cannot aggravate
 * (so that aggravate is found only on endgame-quality items or
 * cursed items)
 */
#define AGGR_POWER 300

/*
 * Numerical index values for the different learned probabilities
 * These are to make the code more readable.
 */
enum
{
    ART_IDX_BOW_SHOTS = 0,
    ART_IDX_BOW_MIGHT,
    ART_IDX_BOW_BRAND,
    ART_IDX_BOW_SLAY,
    ART_IDX_WEAPON_HIT,
    ART_IDX_WEAPON_DAM,
    ART_IDX_AMMO_HIT,
    ART_IDX_AMMO_DAM,
    ART_IDX_NONWEAPON_HIT,
    ART_IDX_NONWEAPON_DAM,
    ART_IDX_NONWEAPON_HIT_DAM,
    ART_IDX_NONWEAPON_BRAND,
    ART_IDX_NONWEAPON_SLAY,
    ART_IDX_NONWEAPON_BLOWS,
    ART_IDX_NONWEAPON_SHOTS,

    ART_IDX_MELEE_BLESS,
    ART_IDX_MELEE_BRAND,
    ART_IDX_MELEE_SLAY,
    ART_IDX_MELEE_SINV,
    ART_IDX_MELEE_BLOWS,
    ART_IDX_MELEE_AC,
    ART_IDX_MELEE_DICE,
    ART_IDX_MELEE_WEIGHT,

    ART_IDX_ALLARMOR_WEIGHT,

    ART_IDX_BOOT_AC,
    ART_IDX_BOOT_FEATHER,
    ART_IDX_BOOT_STEALTH,
    ART_IDX_BOOT_SPEED,

    ART_IDX_GLOVE_AC,
    ART_IDX_GLOVE_FA,
    ART_IDX_GLOVE_DEX,
    ART_IDX_GLOVE_MANA,
    ART_IDX_GLOVE_ID,

    ART_IDX_HELM_AC,
    ART_IDX_HELM_RBLIND,
    ART_IDX_HELM_ESP,
    ART_IDX_HELM_SINV,
    ART_IDX_HELM_WIS,
    ART_IDX_HELM_INT,
    ART_IDX_HELM_ID,

    ART_IDX_SHIELD_AC,
    ART_IDX_SHIELD_LRES,

    ART_IDX_CLOAK_AC,
    ART_IDX_CLOAK_STEALTH,

    ART_IDX_ARMOR_AC,
    ART_IDX_ARMOR_STEALTH,
    ART_IDX_ARMOR_HLIFE,
    ART_IDX_ARMOR_CON,
    ART_IDX_ARMOR_LRES,
    ART_IDX_ARMOR_ALLRES,
    ART_IDX_ARMOR_HRES,

    ART_IDX_DIGGER_TUNN,

    ART_IDX_MSTAFF_INT,
    ART_IDX_MSTAFF_SINV,
    ART_IDX_MSTAFF_ESP,
    ART_IDX_MSTAFF_FA,
    ART_IDX_MSTAFF_RBLIND,
    ART_IDX_MSTAFF_RCONF,

    ART_IDX_MISSILE_BRAND,
    ART_IDX_MISSILE_SLAY,
    ART_IDX_MISSILE_DICE,

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

    /* Supercharged abilities - treated differently in algorithm */
    ART_IDX_MELEE_DICE_SUPER,
    ART_IDX_BOW_SHOTS_SUPER,
    ART_IDX_BOW_MIGHT_SUPER,
    ART_IDX_GEN_SPEED_SUPER,
    ART_IDX_MISSILE_DICE_SUPER,
    ART_IDX_MELEE_BLOWS_SUPER,
    ART_IDX_GEN_AC_SUPER,

    /* Aggravation - weapon and nonweapon */
    ART_IDX_WEAPON_AGGR,
    ART_IDX_NONWEAPON_AGGR,

    /* Total of abilities */
    ART_IDX_TOTAL
};

/* Tallies of different ability types */
/* ToDo: use N_ELEMENTS for these */
#define ART_IDX_BOW_COUNT 4
#define ART_IDX_WEAPON_COUNT 3
#define ART_IDX_AMMO_COUNT 2
#define ART_IDX_NONWEAPON_COUNT 8
#define ART_IDX_MELEE_COUNT 8
#define ART_IDX_ALLARMOR_COUNT 1
#define ART_IDX_BOOT_COUNT 4
#define ART_IDX_GLOVE_COUNT 5
#define ART_IDX_HELM_COUNT 7
#define ART_IDX_SHIELD_COUNT 2
#define ART_IDX_CLOAK_COUNT 2
#define ART_IDX_ARMOR_COUNT 7
#define ART_IDX_DIGGER_COUNT 1
#define ART_IDX_MSTAFF_COUNT 6
#define ART_IDX_MISSILE_COUNT 3
#define ART_IDX_GEN_COUNT 31
#define ART_IDX_HIGH_RESIST_COUNT 13

/* Arrays of indices by item type, used in frequency generation */
static s16b art_idx_bow[] =
    {ART_IDX_BOW_SHOTS, ART_IDX_BOW_MIGHT, ART_IDX_BOW_BRAND, ART_IDX_BOW_SLAY};
static s16b art_idx_weapon[] =
    {ART_IDX_WEAPON_HIT, ART_IDX_WEAPON_DAM, ART_IDX_WEAPON_AGGR};
static s16b art_idx_ammo[] =
    {ART_IDX_AMMO_HIT, ART_IDX_AMMO_DAM};
static s16b art_idx_nonweapon[] =
    {ART_IDX_NONWEAPON_HIT, ART_IDX_NONWEAPON_DAM, ART_IDX_NONWEAPON_HIT_DAM,
    ART_IDX_NONWEAPON_AGGR, ART_IDX_NONWEAPON_BRAND, ART_IDX_NONWEAPON_SLAY,
    ART_IDX_NONWEAPON_BLOWS, ART_IDX_NONWEAPON_SHOTS};
static s16b art_idx_melee[] =
    {ART_IDX_MELEE_BLESS, ART_IDX_MELEE_BRAND, ART_IDX_MELEE_SLAY, ART_IDX_MELEE_SINV,
    ART_IDX_MELEE_BLOWS, ART_IDX_MELEE_AC, ART_IDX_MELEE_DICE,
    ART_IDX_MELEE_WEIGHT};
static s16b art_idx_allarmor[] =
    {ART_IDX_ALLARMOR_WEIGHT};
static s16b art_idx_boot[] =
    {ART_IDX_BOOT_AC, ART_IDX_BOOT_FEATHER, ART_IDX_BOOT_STEALTH, ART_IDX_BOOT_SPEED};
static s16b art_idx_glove[] =
    {ART_IDX_GLOVE_AC, ART_IDX_GLOVE_FA, ART_IDX_GLOVE_DEX,
    ART_IDX_GLOVE_MANA, ART_IDX_GLOVE_ID};
static s16b art_idx_headgear[] =
    {ART_IDX_HELM_AC, ART_IDX_HELM_RBLIND, ART_IDX_HELM_ESP, ART_IDX_HELM_SINV,
    ART_IDX_HELM_WIS, ART_IDX_HELM_INT, ART_IDX_HELM_ID};
static s16b art_idx_shield[] =
    {ART_IDX_SHIELD_AC, ART_IDX_SHIELD_LRES};
static s16b art_idx_cloak[] =
    {ART_IDX_CLOAK_AC, ART_IDX_CLOAK_STEALTH};
static s16b art_idx_armor[] =
    {ART_IDX_ARMOR_AC, ART_IDX_ARMOR_STEALTH, ART_IDX_ARMOR_HLIFE, ART_IDX_ARMOR_CON,
    ART_IDX_ARMOR_LRES, ART_IDX_ARMOR_ALLRES, ART_IDX_ARMOR_HRES};
static s16b art_idx_digger[] =
    {ART_IDX_DIGGER_TUNN};
static s16b art_idx_mstaff[] =
    {ART_IDX_MSTAFF_INT, ART_IDX_MSTAFF_SINV, ART_IDX_MSTAFF_ESP,
    ART_IDX_MSTAFF_FA, ART_IDX_MSTAFF_RBLIND, ART_IDX_MSTAFF_RCONF};
static s16b art_idx_missile[] =
    {ART_IDX_MISSILE_BRAND, ART_IDX_MISSILE_SLAY, ART_IDX_MISSILE_DICE};
static s16b art_idx_gen[] =
    {ART_IDX_GEN_STAT, ART_IDX_GEN_SUST, ART_IDX_GEN_STEALTH,
    ART_IDX_GEN_SEARCH, ART_IDX_GEN_INFRA, ART_IDX_GEN_SPEED,
    ART_IDX_GEN_IMMUNE, ART_IDX_GEN_FA, ART_IDX_GEN_HLIFE,
    ART_IDX_GEN_FEATHER, ART_IDX_GEN_LIGHT, ART_IDX_GEN_SINV,
    ART_IDX_GEN_ESP, ART_IDX_GEN_SDIG, ART_IDX_GEN_REGEN,
    ART_IDX_GEN_LRES, ART_IDX_GEN_RPOIS, ART_IDX_GEN_RFEAR,
    ART_IDX_GEN_RLIGHT, ART_IDX_GEN_RDARK, ART_IDX_GEN_RBLIND,
    ART_IDX_GEN_RCONF, ART_IDX_GEN_RSOUND, ART_IDX_GEN_RSHARD,
    ART_IDX_GEN_RNEXUS, ART_IDX_GEN_RNETHER, ART_IDX_GEN_RCHAOS,
    ART_IDX_GEN_RDISEN, ART_IDX_GEN_AC, ART_IDX_GEN_TUNN,
    ART_IDX_GEN_ACTIV, ART_IDX_GEN_PSTUN};
static s16b art_idx_high_resist[] =
    {ART_IDX_GEN_RPOIS, ART_IDX_GEN_RFEAR,
    ART_IDX_GEN_RLIGHT, ART_IDX_GEN_RDARK, ART_IDX_GEN_RBLIND,
    ART_IDX_GEN_RCONF, ART_IDX_GEN_RSOUND, ART_IDX_GEN_RSHARD,
    ART_IDX_GEN_RNEXUS, ART_IDX_GEN_RNETHER, ART_IDX_GEN_RCHAOS,
    ART_IDX_GEN_RDISEN, ART_IDX_GEN_PSTUN};

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

/*
 * Prototype for the randomized artifact
 */
artifact_type randart;
static char dummy[6] = "dummy";


static void artifact_gen_name(char *buffer, int len, const char ***words)
{
    char word[MAX_RNAME_LEN + 1];

    /* Take a random name */
    randname_make(RANDNAME_TOLKIEN, MIN_RNAME_LEN, MAX_RNAME_LEN, word, sizeof(word), words);

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
    Rand_quick = TRUE;

    /* Generate a random name */
    artifact_gen_name(buffer, len, name_sections);

    /* When done, resume use of the Angband "complex" RNG. */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;
}


/*
 * Return the artifact power, by generating a "fake" object based on the
 * artifact, and calling the common object_power function
 */
static s32b artifact_power(artifact_type *a_ptr)
{
    object_type obj;

    if (!make_fake_artifact(&obj, a_ptr)) return 0;

    return object_power(NULL, &obj);
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
    fake_power = C_ZNEW(z_info->a_max, int);
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
}


/*
 * Assign the various fields corresponding to the base item type.
 */
static void choose_item(artifact_type *a_ptr)
{
    object_kind *k_ptr;
    int i;
    bitflag f[OF_SIZE];

    /* Get a pointer to the base item for this artifact */
    k_ptr = lookup_kind(a_ptr->tval, a_ptr->sval);

    /* Keep base attributes */
    for (i = 0; i < MAX_PVALS; i++) of_wipe(a_ptr->pval_flags[i]);
    for (i = 0; i < k_ptr->num_pvals; i++)
    {
        a_ptr->pval[i] = randcalc(k_ptr->pval[i], 0, MINIMISE);
        of_copy(a_ptr->pval_flags[i], k_ptr->pval_flags[i]);
    }
    a_ptr->num_pvals = k_ptr->num_pvals;
    a_ptr->to_h = randcalc(k_ptr->to_h, 0, MINIMISE);
    a_ptr->to_d = randcalc(k_ptr->to_d, 0, MINIMISE);
    a_ptr->to_a = randcalc(k_ptr->to_a, 0, MINIMISE);
    a_ptr->ac = k_ptr->ac;
    a_ptr->dd = k_ptr->dd;
    a_ptr->ds = k_ptr->ds;
    a_ptr->weight = k_ptr->weight;
    of_copy(a_ptr->flags, k_ptr->flags);

    /* PWMAngband: keep base activations */
    a_ptr->effect = k_ptr->effect;
    a_ptr->time.base = k_ptr->time.base;
    a_ptr->time.dice = k_ptr->time.dice;
    a_ptr->time.sides = k_ptr->time.sides;
    a_ptr->effect_msg = NULL;

    /* Artifacts ignore everything */
    create_mask(f, FALSE, OFT_IGNORE, OFT_MAX);
    of_union(a_ptr->flags, f);

    /* Assign basic stats to the artifact */
    switch (a_ptr->tval)
    {
        case TV_BOW:
        case TV_HAFTED:
        case TV_SWORD:
        case TV_POLEARM:
        {
            a_ptr->to_h += (s16b)(mean_hit_startval / 2 + randint0(mean_hit_startval));
            a_ptr->to_d += (s16b)(mean_dam_startval / 2 + randint0(mean_dam_startval));
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
            a_ptr->to_a += (s16b)(mean_ac_startval / 2 + randint0(mean_ac_startval));
            break;
        }
    }
}


/*
 * Hack -- Code duplication (object_dedup_pvals)
 */
static bool artifact_dedup_pvals(artifact_type *a_ptr)
{
    int i, j, k;

    /* Abort if there can be no duplicates */
    if (a_ptr->num_pvals < 2) return FALSE;

    /* Find the first pair of pvals which have the same value */
    for (i = 0; i < a_ptr->num_pvals; i++)
    {
        for (j = i + 1; j < a_ptr->num_pvals; j++)
        {
            if (a_ptr->pval[i] == a_ptr->pval[j])
            {
                /* Nuke the j pval and its flags, combining them with i's */
                of_union(a_ptr->pval_flags[i], a_ptr->pval_flags[j]);
                of_wipe(a_ptr->pval_flags[j]);
                a_ptr->pval[j] = 0;

                /* Move any remaining pvals down one to fill the void */
                for (k = j + 1; k < a_ptr->num_pvals; k++)
                {
                    of_copy(a_ptr->pval_flags[k - 1], a_ptr->pval_flags[k]);
                    of_wipe(a_ptr->pval_flags[k]);
                    a_ptr->pval[k - 1] = a_ptr->pval[k];
                    a_ptr->pval[k] = 0;
                }

                /* We now have one fewer pval */
                a_ptr->num_pvals--;
                return TRUE;
            }
        }
    }

    /* No two pvals are identical */
    return FALSE;
}


/*
 * Hack -- Code duplication (object_closest_pval)
 */
static int artifact_closest_pval(artifact_type *a_ptr, int value)
{
    int i, best_diff, best_pval = 0;

    if (!a_ptr->num_pvals) return -1;

    best_diff = value;

    for (i = 0; i < a_ptr->num_pvals; i++)
    {
        if (abs(a_ptr->pval[i] - value) < best_diff)
        {
            best_diff = abs(a_ptr->pval[i] - value);
            best_pval = i;
        }
    }

    return best_pval;
}


/*
 * Hack -- Code duplication (object_add_pval)
 */
static bool artifact_add_pval(artifact_type *a_ptr, int pval, int flag)
{
    bitflag f[OF_SIZE];
    int i, best_pval;

    for (i = 0; i < a_ptr->num_pvals; i++)
    {
        /* Flag should be unique across pvals */
        if (of_has(a_ptr->pval_flags[i], flag))
        {
            /* See if any other flags are associated with this pval */
            of_copy(f, a_ptr->pval_flags[i]);
            of_off(f, flag);
            if (of_is_empty(f))
            {
                /* Safe to increment and finish */
                a_ptr->pval[i] += pval;
                return artifact_dedup_pvals(a_ptr);
            }

            break;
        }
    }

    /* So it doesn't have the flag, or it does but that pval also has others */
    if (i == a_ptr->num_pvals) i = -1;

    /* Create a new pval if we can */
    if (a_ptr->num_pvals < MAX_PVALS)
    {
        a_ptr->pval[a_ptr->num_pvals] = pval;
        of_on(a_ptr->pval_flags[a_ptr->num_pvals], flag);

        /* We need to move the flag to the new pval */
        if (i != -1)
        {
            a_ptr->pval[a_ptr->num_pvals] += a_ptr->pval[i];
            of_off(a_ptr->pval_flags[i], flag);
        }

        /* We do this last because pvals start from zero */
        a_ptr->num_pvals++;

        /* We invert the logic because we've already added a pval */
        return (!artifact_dedup_pvals(a_ptr));
    }

    /* We use the closest existing pval */
    best_pval = artifact_closest_pval(a_ptr, pval + ((i == -1)? 0: a_ptr->pval[i]));

    /* Turn on the flag on the new pval */
    if (best_pval != i)
    {
        of_on(a_ptr->pval_flags[best_pval], flag);

        /* Turn it off on its old pval */
        if (i != -1) of_off(a_ptr->pval_flags[i], flag);
    }

    /* We haven't changed any pvals, so no need to de-dup */
    return FALSE;
}


/*
 * Hack -- Code duplication (which_pval)
 */
static int artifact_which_pval(const artifact_type *a_ptr, const int flag)
{
    int i;

    for (i = 0; i < a_ptr->num_pvals; i++)
    {
        /* Flag should be unique across pvals */
        if (of_has(a_ptr->pval_flags[i], flag)) return i;
    }

    my_assert(0);
    return -1;
}


/*
 * We've just added an ability which uses the pval bonus.  Make sure it's
 * not zero.  If it's currently negative, make it worse.
 */
static void do_pval(artifact_type *a_ptr, int flag)
{
    int factor = 1;

    /* Track whether we have blows, might or shots on this item */
    if (of_has(a_ptr->flags, OF_BLOWS)) factor++;
    if (of_has(a_ptr->flags, OF_MIGHT)) factor++;
    if (of_has(a_ptr->flags, OF_SHOTS)) factor++;

    if (!of_has(a_ptr->flags, flag))
    {
        int pval;

        /* Blows, might, shots handled separately */
        if (factor > 1)
        {
            pval = randint1(2);

            /* Give it a shot at +3 */
            if (INHIBIT_STRONG) pval = 3;
        }

        /* PWMAngband: mana handled separately */
        else if (flag == OF_MANA) pval = 5 + randint0(6);

        /* Normal case */
        else pval = randint1(4);

        artifact_add_pval(a_ptr, pval, flag);
        of_on(a_ptr->flags, flag);
    }
    else
    {
        int i = artifact_which_pval(a_ptr, flag);

        if (a_ptr->pval[i] < 0)
        {
            if (one_in_(2)) artifact_add_pval(a_ptr, -1, flag);
        }
        else if (one_in_(a_ptr->pval[i] * factor))
        {
            /*
             * Made this a bit rarer and diminishing with higher pval -
             * also rarer if item has blows/might/shots already
             */
            artifact_add_pval(a_ptr, 1, flag);
        }
    }
}


static void remove_contradictory(artifact_type *a_ptr)
{
    if (of_has(a_ptr->flags, OF_AGGRAVATE) && of_has(a_ptr->flags, OF_STEALTH))
    {
        of_off(a_ptr->flags, OF_STEALTH);
        of_off(a_ptr->pval_flags[artifact_which_pval(a_ptr, OF_STEALTH)], OF_STEALTH);
    }
    if (of_has(a_ptr->flags, OF_IM_ACID)) of_off(a_ptr->flags, OF_RES_ACID);
    if (of_has(a_ptr->flags, OF_IM_ELEC)) of_off(a_ptr->flags, OF_RES_ELEC);
    if (of_has(a_ptr->flags, OF_IM_FIRE)) of_off(a_ptr->flags, OF_RES_FIRE);
    if (of_has(a_ptr->flags, OF_IM_COLD)) of_off(a_ptr->flags, OF_RES_COLD);

    if (of_has(a_ptr->flags, OF_STR))
    {
        if (a_ptr->pval[artifact_which_pval(a_ptr, OF_STR)] < 0)
            of_off(a_ptr->flags, OF_SUST_STR);
    }
    if (of_has(a_ptr->flags, OF_INT))
    {
        if (a_ptr->pval[artifact_which_pval(a_ptr, OF_INT)] < 0)
            of_off(a_ptr->flags, OF_SUST_INT);
    }
    if (of_has(a_ptr->flags, OF_WIS))
    {
        if (a_ptr->pval[artifact_which_pval(a_ptr, OF_WIS)] < 0)
            of_off(a_ptr->flags, OF_SUST_WIS);
    }
    if (of_has(a_ptr->flags, OF_DEX))
    {
        if (a_ptr->pval[artifact_which_pval(a_ptr, OF_DEX)] < 0)
            of_off(a_ptr->flags, OF_SUST_DEX);
    }
    if (of_has(a_ptr->flags, OF_CON))
    {
        if (a_ptr->pval[artifact_which_pval(a_ptr, OF_CON)] < 0)
            of_off(a_ptr->flags, OF_SUST_CON);
    }
    if (of_has(a_ptr->flags, OF_CHR))
    {
        if (a_ptr->pval[artifact_which_pval(a_ptr, OF_CHR)] < 0)
            of_off(a_ptr->flags, OF_SUST_CHR);
    }
    if (of_has(a_ptr->flags, OF_BLOWS))
    {
        int i = artifact_which_pval(a_ptr, OF_BLOWS);

        if (a_ptr->pval[i] < 0)
        {
            of_off(a_ptr->flags, OF_BLOWS);
            of_off(a_ptr->pval_flags[i], OF_BLOWS);
        }
    }

    if (of_has(a_ptr->flags, OF_LIGHT_CURSE)) of_off(a_ptr->flags, OF_BLESSED);
    if (of_has(a_ptr->flags, OF_KILL_DRAGON)) of_off(a_ptr->flags, OF_SLAY_DRAGON);
    if (of_has(a_ptr->flags, OF_KILL_DEMON)) of_off(a_ptr->flags, OF_SLAY_DEMON);
    if (of_has(a_ptr->flags, OF_KILL_UNDEAD)) of_off(a_ptr->flags, OF_SLAY_UNDEAD);
    if (of_has(a_ptr->flags, OF_DRAIN_EXP)) of_off(a_ptr->flags, OF_HOLD_LIFE);

    /*
     * ESP evil bypasses ESP undead/demon
     *
     * Note: although "monster" orcs/trolls/giants/dragons are evil, ESP evil cannot
     * bypass ESP orc/troll/giant/dragon because of the corresponding player
     * races (a player of the Half-Orc race can be detected by ESP orc, but
     * not by ESP evil)
     */
    if (of_has(a_ptr->flags, OF_ESP_EVIL))
    {
        of_off(a_ptr->flags, OF_ESP_UNDEAD);
        of_off(a_ptr->flags, OF_ESP_DEMON);
    }

    /* ESP all bypasses all other ESPs */
    if (of_has(a_ptr->flags, OF_ESP_ALL))
    {
        bitflag f2[OF_SIZE];

        create_mask(f2, FALSE, OFT_ESP, OFT_MAX);
        of_diff(a_ptr->flags, f2);
        of_on(a_ptr->flags, OF_ESP_ALL);
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


static void artifact_count_slays(const artifact_type *a_ptr, int idx_brand, int idx_slay)
{
    s32b temp, temp2;
    bitflag mask[OF_SIZE], flags[OF_SIZE];

    of_copy(flags, a_ptr->flags);

    /* Brands or slays - count all together */
    create_mask(mask, FALSE, OFT_SLAY, OFT_BRAND, OFT_KILL, OFT_MAX);
    if (of_is_inter(a_ptr->flags, mask))
    {
        /* We have some brands or slays - count them */
        temp = list_slays(flags, mask, NULL, NULL, NULL, FALSE);
        create_mask(mask, FALSE, OFT_BRAND, OFT_MAX);
        temp2 = list_slays(flags, mask, NULL, NULL, NULL, FALSE);

        /* Add these to the frequency count */
        artprobs[idx_slay] += (temp - temp2);
        artprobs[idx_brand] += temp2;
    }
}


/*
 * Determine whether an artifact is ammo
 */
static bool art_is_ammo(const artifact_type *a_ptr)
{
    return ((a_ptr->tval == TV_SHOT) || (a_ptr->tval == TV_ARROW) ||
        (a_ptr->tval == TV_BOLT) || (a_ptr->tval == TV_ROCK));
}


/*
 * Parse the list of artifacts and count up the frequencies of the various
 * abilities.  This is used to give dynamic generation probabilities.
 */
static void parse_frequencies(void)
{
    int i;
    const artifact_type *a_ptr;
    object_kind *k_ptr;
    s32b temp, temp2;
    bitflag f2[OF_SIZE];

    create_mask(f2, FALSE, OFT_ESP, OFT_MAX);

    /* Zero the frequencies for artifact attributes */
    for (i = 0; i < ART_IDX_TOTAL; i++) artprobs[i] = 0;

    /* Go through the list of all artifacts */
    for (i = 0; i < z_info->a_max; i++)
    {
        a_ptr = &a_info[i];

        /* Special cases -- Don't parse these! */
        if ((i == ART_POWER) || (i == ART_GROND) || (i == ART_MORGOTH) || (i == ART_SILMARIL))
            continue;

        /* Also don't parse cursed or null items */
        if ((base_power[i] < 0) || (a_ptr->tval == 0)) continue;

        /* Get a pointer to the base item for this artifact */
        k_ptr = lookup_kind(a_ptr->tval, a_ptr->sval);

        /* Count up the abilities for this artifact */
        if (a_ptr->tval == TV_BOW)
        {
            if (of_has(a_ptr->flags, OF_SHOTS))
            {
                /* Do we have 3 or more extra shots? (Unlikely) */
                if (a_ptr->pval[artifact_which_pval(a_ptr, OF_SHOTS)] > 2)
                    artprobs[ART_IDX_BOW_SHOTS_SUPER]++;
                else artprobs[ART_IDX_BOW_SHOTS]++;
            }

            if (of_has(a_ptr->flags, OF_MIGHT))
            {
                /* Do we have 3 or more extra might? (Unlikely) */
                if (a_ptr->pval[artifact_which_pval(a_ptr, OF_MIGHT)] > 2)
                    artprobs[ART_IDX_BOW_MIGHT_SUPER]++;
                else artprobs[ART_IDX_BOW_MIGHT]++;
            }

            /* Brands or slays - count all together */
            artifact_count_slays(a_ptr, ART_IDX_BOW_BRAND, ART_IDX_BOW_SLAY);
        }

        /* Handle hit / dam ratings - are they higher than normal? */
        /* Also handle other weapon/nonweapon abilities */
        if (weapon_p(a_ptr))
        {
            temp = a_ptr->to_h - randcalc(k_ptr->to_h, 0, MINIMISE) - mean_hit_startval;
            temp = temp / mean_hit_increment;
            artprobs[ART_IDX_WEAPON_HIT] += temp;

            temp = a_ptr->to_d - randcalc(k_ptr->to_d, 0, MINIMISE) - mean_dam_startval;
            temp = temp / mean_dam_increment;
            artprobs[ART_IDX_WEAPON_DAM] += temp;

            /* Aggravation */
            if (of_has(a_ptr->flags, OF_AGGRAVATE)) artprobs[ART_IDX_WEAPON_AGGR]++;
        }
        else if (art_is_ammo(a_ptr))
        {
            temp = a_ptr->to_h - randcalc(k_ptr->to_h, 0, MINIMISE);
            if (temp > 0)
            {
                temp = temp / mean_hit_increment;
                if (temp > 0) artprobs[ART_IDX_AMMO_HIT] += temp;
            }
            temp = a_ptr->to_d - randcalc(k_ptr->to_d, 0, MINIMISE);
            if (temp > 0)
            {
                temp = temp / mean_dam_increment;
                if (temp > 0) artprobs[ART_IDX_AMMO_DAM] += temp;
            }
        }
        else
        {
            temp = a_ptr->to_h - randcalc(k_ptr->to_h, 0, MINIMISE);
            temp2 = a_ptr->to_d - randcalc(k_ptr->to_d, 0, MINIMISE);
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
            if (of_has(a_ptr->flags, OF_AGGRAVATE)) artprobs[ART_IDX_NONWEAPON_AGGR]++;

            /* Brands or slays - count all together */
            artifact_count_slays(a_ptr, ART_IDX_NONWEAPON_BRAND, ART_IDX_NONWEAPON_SLAY);

            /* Adding 1 for extra blows on nonweapon */
            if (of_has(a_ptr->flags, OF_BLOWS))
                artprobs[ART_IDX_NONWEAPON_BLOWS]++;

            /* Adding 1 for extra shots on nonweapon */
            if (of_has(a_ptr->flags, OF_SHOTS))
                artprobs[ART_IDX_NONWEAPON_SHOTS]++;
        }

        if (melee_p(a_ptr))
        {
            /* Blessed weapon? */
            if (of_has(a_ptr->flags, OF_BLESSED)) artprobs[ART_IDX_MELEE_BLESS]++;

            /* See invisible? */
            if (of_has(a_ptr->flags, OF_SEE_INVIS)) artprobs[ART_IDX_MELEE_SINV]++;

            /* Does this weapon have extra blows? */
            if (of_has(a_ptr->flags, OF_BLOWS))
            {
                /* Do we have 3 or more extra blows? (Unlikely) */
                if (a_ptr->pval[artifact_which_pval(a_ptr, OF_BLOWS)] > 2)
                    artprobs[ART_IDX_MELEE_BLOWS_SUPER]++;
                else
                    artprobs[ART_IDX_MELEE_BLOWS]++;
            }

            /* Does this weapon have an unusual bonus to AC? */
            if ((a_ptr->to_a - randcalc(k_ptr->to_a, 0, MAXIMISE)) > 0)
            {
                temp = (a_ptr->to_a - randcalc(k_ptr->to_a, 0, MAXIMISE)) /
                    mean_ac_increment;
                if (temp > 0) artprobs[ART_IDX_MELEE_AC] += temp;
            }

            /* Check damage dice - are they more than normal? */
            if (a_ptr->dd > k_ptr->dd)
            {
                /* Difference of 3 or more? */
                if ((a_ptr->dd - k_ptr->dd) > 2)
                    artprobs[ART_IDX_MELEE_DICE_SUPER]++;
                else
                    artprobs[ART_IDX_MELEE_DICE]++;
            }

            /* Check weight - is it different from normal? */
            if (a_ptr->weight != k_ptr->weight) artprobs[ART_IDX_MELEE_WEIGHT]++;

            /* Brands or slays - count all together */
            artifact_count_slays(a_ptr, ART_IDX_MELEE_BRAND, ART_IDX_MELEE_SLAY);
        }

        if (art_is_ammo(a_ptr))
        {
            /* Brands or slays - count all together */
            artifact_count_slays(a_ptr, ART_IDX_MISSILE_BRAND, ART_IDX_MISSILE_SLAY);

            /* Check damage dice - are they more than normal? */
            if (a_ptr->dd > k_ptr->dd)
            {
                /* Difference of 2 or more? */
                if ((a_ptr->dd - k_ptr->dd) > 1)
                    artprobs[ART_IDX_MISSILE_DICE_SUPER]++;
                else
                    artprobs[ART_IDX_MISSILE_DICE]++;
            }
        }

        /* Check for tunnelling ability */
        if (of_has(a_ptr->flags, OF_TUNNEL)) artprobs[ART_IDX_GEN_TUNN]++;

        /*
         * Count up extra AC bonus values.
         * Could also add logic to subtract for lower values here, but it's
         * probably not worth the trouble since it's so rare.
         */
        temp = a_ptr->to_a - randcalc(k_ptr->to_a, 0, MINIMISE) - mean_ac_startval;
        if (temp > 0)
        {
            temp = temp / mean_ac_increment;
            if (temp > 0)
            {
                if (a_ptr->to_a > 20)
                    artprobs[ART_IDX_GEN_AC_SUPER]++;
                else if (a_ptr->tval == TV_BOOTS)
                    artprobs[ART_IDX_BOOT_AC] += temp;
                else if (a_ptr->tval == TV_GLOVES)
                    artprobs[ART_IDX_GLOVE_AC] += temp;
                else if ((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN))
                    artprobs[ART_IDX_HELM_AC] += temp;
                else if (a_ptr->tval == TV_SHIELD)
                    artprobs[ART_IDX_SHIELD_AC] += temp;
                else if (a_ptr->tval == TV_CLOAK)
                    artprobs[ART_IDX_CLOAK_AC] += temp;
                else if (body_armor_p(a_ptr))
                    artprobs[ART_IDX_ARMOR_AC] += temp;
                else
                    artprobs[ART_IDX_GEN_AC] += temp;
            }
        }

        /* Generic armor abilities */
        if ((a_ptr->tval == TV_BOOTS) || (a_ptr->tval == TV_GLOVES) ||
            (a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN) ||
            (a_ptr->tval == TV_SHIELD) || (a_ptr->tval == TV_CLOAK) ||
            body_armor_p(a_ptr))
        {
            /* Check weight - is it different from normal? */
            /* ToDo: count higher and lower separately */
            if (a_ptr->weight != k_ptr->weight) artprobs[ART_IDX_ALLARMOR_WEIGHT]++;
        }

        /*
         * General abilities.  This section requires a bit more work
         * than the others, because we have to consider cases where
         * a certain ability might be found in a particular item type.
         * For example, ESP is commonly found on headgear, so when
         * we count ESP we must add it to either the headgear or
         * general tally, depending on the base item.  This permits
         * us to have general abilities appear more commonly on a
         * certain item type.
         */
        if (flags_test(a_ptr->flags, OF_SIZE, OF_STR, OF_INT, OF_WIS, OF_DEX, OF_CON,
            OF_CHR, FLAG_END))
        {
            /* Stat bonus case.  Add up the number of individual bonuses */
            temp = 0;
            if (of_has(a_ptr->flags, OF_STR)) temp++;
            if (of_has(a_ptr->flags, OF_INT)) temp++;
            if (of_has(a_ptr->flags, OF_WIS)) temp++;
            if (of_has(a_ptr->flags, OF_DEX)) temp++;
            if (of_has(a_ptr->flags, OF_CON)) temp++;
            if (of_has(a_ptr->flags, OF_CHR)) temp++;

            /* Handle a few special cases separately. */
            if (((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN)) &&
                (of_has(a_ptr->flags, OF_WIS) || of_has(a_ptr->flags, OF_INT)))
            {
                /* Handle WIS and INT on helms and crowns */
                if (of_has(a_ptr->flags, OF_WIS))
                {
                    artprobs[ART_IDX_HELM_WIS]++;

                    /* Counted this one separately so subtract it here */
                    temp--;
                }
                if (of_has(a_ptr->flags, OF_INT))
                {
                    artprobs[ART_IDX_HELM_INT]++;

                    /* Counted this one separately so subtract it here */
                    temp--;
                }
            }
            else if (body_armor_p(a_ptr) && of_has(a_ptr->flags, OF_CON))
            {
                /* Handle CON bonus on armor */
                artprobs[ART_IDX_ARMOR_CON]++;

                /* Counted this one separately so subtract it here */
                temp--;
            }
            else if ((a_ptr->tval == TV_GLOVES) && of_has(a_ptr->flags, OF_DEX))
            {
                /* Handle DEX bonus on gloves */
                artprobs[ART_IDX_GLOVE_DEX]++;

                /* Counted this one separately so subtract it here */
                temp--;
            }
            else if ((a_ptr->tval == TV_MSTAFF) && of_has(a_ptr->flags, OF_INT))
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
        }

        if (flags_test(a_ptr->flags, OF_SIZE, OF_SUST_STR, OF_SUST_INT, OF_SUST_WIS,
            OF_SUST_DEX, OF_SUST_CON, OF_SUST_CHR, FLAG_END))
        {
            /* Now do sustains, in a similar manner */
            temp = 0;
            if (of_has(a_ptr->flags, OF_SUST_STR)) temp++;
            if (of_has(a_ptr->flags, OF_SUST_INT)) temp++;
            if (of_has(a_ptr->flags, OF_SUST_WIS)) temp++;
            if (of_has(a_ptr->flags, OF_SUST_DEX)) temp++;
            if (of_has(a_ptr->flags, OF_SUST_CON)) temp++;
            if (of_has(a_ptr->flags, OF_SUST_CHR)) temp++;
            artprobs[ART_IDX_GEN_SUST] += temp;
        }

        if ((a_ptr->tval == TV_GLOVES) && of_has(a_ptr->flags, OF_MANA))
        {
            /* Handle mana bonus on gloves */
            artprobs[ART_IDX_GLOVE_MANA]++;
        }

        if (of_has(a_ptr->flags, OF_STEALTH))
        {
            /* Handle stealth, including a couple of special cases */
            if (a_ptr->tval == TV_BOOTS)
                artprobs[ART_IDX_BOOT_STEALTH]++;
            else if (a_ptr->tval == TV_CLOAK)
                artprobs[ART_IDX_CLOAK_STEALTH]++;
            else if (body_armor_p(a_ptr))
                artprobs[ART_IDX_ARMOR_STEALTH]++;
            else
            {
                /* General case */
                artprobs[ART_IDX_GEN_STEALTH]++;
            }
        }

        if (of_has(a_ptr->flags, OF_SEARCH))
        {
            /* Handle searching bonus - fully generic this time */
            artprobs[ART_IDX_GEN_SEARCH]++;
        }

        if (of_has(a_ptr->flags, OF_INFRA))
        {
            /* Handle infravision bonus - fully generic */
            artprobs[ART_IDX_GEN_INFRA]++;
        }

        if (of_has(a_ptr->flags, OF_SPEED))
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
            if (a_ptr->pval[artifact_which_pval(a_ptr, OF_SPEED)] > 7)
            {
                /* Supercharge case */
                artprobs[ART_IDX_GEN_SPEED_SUPER]++;
            }
            else if (a_ptr->tval == TV_BOOTS)
            {
                /* Handle boots separately */
                artprobs[ART_IDX_BOOT_SPEED]++;
            }
            else
                artprobs[ART_IDX_GEN_SPEED]++;
        }

        if (flags_test(a_ptr->flags, OF_SIZE, OF_IM_ACID, OF_IM_ELEC, OF_IM_FIRE,
            OF_IM_COLD, FLAG_END))
        {
            /* Count up immunities for this item, if any */
            temp = 0;
            if (of_has(a_ptr->flags, OF_IM_ACID)) temp++;
            if (of_has(a_ptr->flags, OF_IM_ELEC)) temp++;
            if (of_has(a_ptr->flags, OF_IM_FIRE)) temp++;
            if (of_has(a_ptr->flags, OF_IM_COLD)) temp++;
            artprobs[ART_IDX_GEN_IMMUNE] += temp;
        }

        if (of_has(a_ptr->flags, OF_FREE_ACT))
        {
            /* Free action - handle gloves separately */
            if (a_ptr->tval == TV_GLOVES)
                artprobs[ART_IDX_GLOVE_FA]++;
            else
                artprobs[ART_IDX_GEN_FA]++;
        }

        if (of_has(a_ptr->flags, OF_HOLD_LIFE))
        {
            /* Hold life - do body armor separately */
            if (body_armor_p(a_ptr))
                artprobs[ART_IDX_ARMOR_HLIFE]++;
            else
                artprobs[ART_IDX_GEN_HLIFE]++;
        }

        if (of_has(a_ptr->flags, OF_FEATHER))
        {
            /* Feather fall - handle boots separately */
            if (a_ptr->tval == TV_BOOTS)
                artprobs[ART_IDX_BOOT_FEATHER]++;
            else
                artprobs[ART_IDX_GEN_FEATHER]++;
        }

        if (of_has(a_ptr->flags, OF_LIGHT))
        {
            /* Handle permanent light */
            artprobs[ART_IDX_GEN_LIGHT]++;
        }

        if (of_has(a_ptr->flags, OF_SEE_INVIS))
        {
            /*
             * Handle see invisible - do helms / crowns separately
             * (Weapons were done already so exclude them)
             */
            if (!melee_p(a_ptr))
            {
                if ((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN))
                    artprobs[ART_IDX_HELM_SINV]++;
                else if (a_ptr->tval == TV_MSTAFF)
                    artprobs[ART_IDX_MSTAFF_SINV]++;
                else
                    artprobs[ART_IDX_GEN_SINV]++;
            }
        }

        if (of_is_inter(a_ptr->flags, f2))
        {
            /* ESP case.  Add up the number of individual bonuses */
            temp = 0;
            if (of_has(a_ptr->flags, OF_ESP_ORC)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_TROLL)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_GIANT)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_ANIMAL)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_DRAGON)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_DEMON)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_UNDEAD)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_EVIL)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_ALL)) temp++;
            if (of_has(a_ptr->flags, OF_ESP_RADIUS)) temp++;

            /* Handle helms/crowns separately. */
            if ((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN))
                artprobs[ART_IDX_HELM_ESP] += temp;
            else if (a_ptr->tval == TV_MSTAFF)
                artprobs[ART_IDX_MSTAFF_ESP] += temp;
            else
                artprobs[ART_IDX_GEN_ESP] += temp;
        }

        if (of_has(a_ptr->flags, OF_SLOW_DIGEST))
        {
            /* Slow digestion case - generic. */
            artprobs[ART_IDX_GEN_SDIG]++;
        }

        if (of_has(a_ptr->flags, OF_REGEN))
        {
            /* Regeneration case - generic. */
            artprobs[ART_IDX_GEN_REGEN]++;
        }

        if (of_has(a_ptr->flags, OF_KNOWLEDGE))
        {
            /* Handle helms/crowns separately. */
            if ((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN))
                artprobs[ART_IDX_HELM_ID]++;
            else if (a_ptr->tval == TV_GLOVES)
            {
                /* Handle knowledge bonus on gloves */
                artprobs[ART_IDX_GLOVE_ID]++;
            }
        }

        if (flags_test(a_ptr->flags, OF_SIZE, OF_RES_ACID, OF_RES_ELEC, OF_RES_FIRE, OF_RES_COLD,
            FLAG_END))
        {
            /* Count up low resists (not the type, just the number) */
            temp = 0;
            if (of_has(a_ptr->flags, OF_RES_ACID)) temp++;
            if (of_has(a_ptr->flags, OF_RES_ELEC)) temp++;
            if (of_has(a_ptr->flags, OF_RES_FIRE)) temp++;
            if (of_has(a_ptr->flags, OF_RES_COLD)) temp++;

            /* Shields treated separately */
            if (a_ptr->tval == TV_SHIELD)
                artprobs[ART_IDX_SHIELD_LRES] += temp;
            else if (body_armor_p(a_ptr))
            {
                /* Armor also treated separately */
                if (temp == 4)
                {
                    /* Special case: armor has all four low resists */
                    artprobs[ART_IDX_ARMOR_ALLRES]++;
                }
                else
                {
                    /* Just tally up the resists as usual */
                    artprobs[ART_IDX_ARMOR_LRES] += temp;
                }
            }
            else
            {
                /* General case */
                artprobs[ART_IDX_GEN_LRES] += temp;
            }
        }

        /*
         * If the item is body armor then count up all the high resists before
         * going through them individually.  High resists are an important
         * component of body armor so we track probability for them separately.
         * The proportions of the high resists will be determined by the
         * generic frequencies - this number just tracks the total.
         */
        if (body_armor_p(a_ptr))
        {
            temp = 0;
            if (of_has(a_ptr->flags, OF_RES_POIS)) temp++;
            if (of_has(a_ptr->flags, OF_RES_FEAR)) temp++;
            if (of_has(a_ptr->flags, OF_RES_LIGHT)) temp++;
            if (of_has(a_ptr->flags, OF_RES_DARK)) temp++;
            if (of_has(a_ptr->flags, OF_RES_BLIND)) temp++;
            if (of_has(a_ptr->flags, OF_RES_CONFU)) temp++;
            if (of_has(a_ptr->flags, OF_RES_SOUND)) temp++;
            if (of_has(a_ptr->flags, OF_RES_SHARD)) temp++;
            if (of_has(a_ptr->flags, OF_RES_NEXUS)) temp++;
            if (of_has(a_ptr->flags, OF_RES_NETHR)) temp++;
            if (of_has(a_ptr->flags, OF_RES_CHAOS)) temp++;
            if (of_has(a_ptr->flags, OF_RES_DISEN)) temp++;
            if (of_has(a_ptr->flags, OF_RES_STUN)) temp++;
            artprobs[ART_IDX_ARMOR_HRES] += temp;
        }

        /* Now do the high resists individually */
        if (of_has(a_ptr->flags, OF_RES_POIS))
        {
            /* Resist poison ability */
            artprobs[ART_IDX_GEN_RPOIS]++;
        }

        if (of_has(a_ptr->flags, OF_RES_FEAR))
        {
            /* Resist fear ability */
            artprobs[ART_IDX_GEN_RFEAR]++;
        }

        if (of_has(a_ptr->flags, OF_RES_LIGHT))
        {
            /* Resist light ability */
            artprobs[ART_IDX_GEN_RLIGHT]++;
        }

        if (of_has(a_ptr->flags, OF_RES_DARK))
        {
            /* Resist dark ability */
            artprobs[ART_IDX_GEN_RDARK]++;
        }

        if (of_has(a_ptr->flags, OF_RES_BLIND))
        {
            /* Resist blind ability - helms/crowns are separate */
            if ((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN))
                artprobs[ART_IDX_HELM_RBLIND]++;
            else
            {
                /* General case */
                artprobs[ART_IDX_GEN_RBLIND]++;
            }
        }

        if (of_has(a_ptr->flags, OF_RES_CONFU))
        {
            /* Resist confusion ability */
            artprobs[ART_IDX_GEN_RCONF]++;
        }

        if (of_has(a_ptr->flags, OF_RES_SOUND))
        {
            /* Resist sound ability */
            artprobs[ART_IDX_GEN_RSOUND]++;
        }

        if (of_has(a_ptr->flags, OF_RES_SHARD))
        {
            /* Resist shards ability */
            artprobs[ART_IDX_GEN_RSHARD]++;
        }

        if (of_has(a_ptr->flags, OF_RES_NEXUS))
        {
            /* Resist nexus ability */
            artprobs[ART_IDX_GEN_RNEXUS]++;
        }

        if (of_has(a_ptr->flags, OF_RES_NETHR))
        {
            /* Resist nether ability */
            artprobs[ART_IDX_GEN_RNETHER]++;
        }

        if (of_has(a_ptr->flags, OF_RES_CHAOS))
        {
            /* Resist chaos ability */
            artprobs[ART_IDX_GEN_RCHAOS]++;
        }

        if (of_has(a_ptr->flags, OF_RES_DISEN))
        {
            /* Resist disenchantment ability */
            artprobs[ART_IDX_GEN_RDISEN]++;
        }

        if (of_has(a_ptr->flags, OF_RES_STUN))
        {
            /* Resist stunning ability */
            artprobs[ART_IDX_GEN_PSTUN]++;
        }

        if (a_ptr->effect)
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
    for (i = 0; i < ART_IDX_BOW_COUNT; i++)
        artprobs[art_idx_bow[i]] = (artprobs[art_idx_bow[i]] * art_total) / art_bow_total;

    /* All weapon abilities */
    for (i = 0; i < ART_IDX_WEAPON_COUNT; i++)
    {
        artprobs[art_idx_weapon[i]] = (artprobs[art_idx_weapon[i]] * art_total) /
            (art_bow_total + art_melee_total);
    }

    /* Corresponding ammo abilities */
    for (i = 0; i < ART_IDX_AMMO_COUNT; i++)
        artprobs[art_idx_ammo[i]] = (artprobs[art_idx_ammo[i]] * art_total) / art_missile_total;

    /* Corresponding non-weapon abilities */
    temp = art_total - art_melee_total - art_bow_total - art_missile_total;
    for (i = 0; i < ART_IDX_NONWEAPON_COUNT; i++)
        artprobs[art_idx_nonweapon[i]] = (artprobs[art_idx_nonweapon[i]] * art_total) / temp;

    /* All melee weapon abilities */
    for (i = 0; i < ART_IDX_MELEE_COUNT; i++)
        artprobs[art_idx_melee[i]] = (artprobs[art_idx_melee[i]] * art_total) / art_melee_total;

    /* All general armor abilities */
    temp = art_armor_total + art_boot_total + art_shield_total + art_headgear_total +
        art_cloak_total + art_glove_total;
    for (i = 0; i < ART_IDX_ALLARMOR_COUNT; i++)
        artprobs[art_idx_allarmor[i]] = (artprobs[art_idx_allarmor[i]] * art_total) / temp;

    /* Boots */
    for (i = 0; i < ART_IDX_BOOT_COUNT; i++)
        artprobs[art_idx_boot[i]] = (artprobs[art_idx_boot[i]] * art_total) / art_boot_total;

    /* Gloves */
    for (i = 0; i < ART_IDX_GLOVE_COUNT; i++)
        artprobs[art_idx_glove[i]] = (artprobs[art_idx_glove[i]] * art_total) / art_glove_total;

    /* Headgear */
    for (i = 0; i < ART_IDX_HELM_COUNT; i++)
    {
        artprobs[art_idx_headgear[i]] = (artprobs[art_idx_headgear[i]] * art_total) /
            art_headgear_total;
    }

    /* Shields */
    for (i = 0; i < ART_IDX_SHIELD_COUNT; i++)
        artprobs[art_idx_shield[i]] = (artprobs[art_idx_shield[i]] * art_total) / art_shield_total;

    /* Cloaks */
    for (i = 0; i < ART_IDX_CLOAK_COUNT; i++)
        artprobs[art_idx_cloak[i]] = (artprobs[art_idx_cloak[i]] * art_total) / art_cloak_total;

    /* Body armor */
    for (i = 0; i < ART_IDX_ARMOR_COUNT; i++)
        artprobs[art_idx_armor[i]] = (artprobs[art_idx_armor[i]] * art_total) / art_armor_total;

    /* Mage staves */
    for (i = 0; i < ART_IDX_MSTAFF_COUNT; i++)
        artprobs[art_idx_mstaff[i]] = (artprobs[art_idx_mstaff[i]] * art_total) / art_mstaff_total;

    /* Missiles */
    for (i = 0; i < ART_IDX_MISSILE_COUNT; i++)
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
static bool add_flag(artifact_type *a_ptr, int flag)
{
    if (of_has(a_ptr->flags, flag)) return FALSE;

    of_on(a_ptr->flags, flag);
    return TRUE;
}


/*
 * Adds a flag and pval to an artifact. Always attempts to increase the pval.
 */
static void add_pval_flag(artifact_type *a_ptr, int flag)
{
    do_pval(a_ptr, flag);
}


/*
 * Adds a flag and a pval to an artifact, but won't increase
 * the pval if the flag is present. Returns true when changes were made.
 */
static bool add_fixed_pval_flag(artifact_type *a_ptr, int flag)
{
    if (of_has(a_ptr->flags, flag)) return FALSE;

    do_pval(a_ptr, flag);
    return TRUE;
}


/*
 * Adds a flag and an initial pval to an artifact.  Returns true
 * when the flag was not present.
 */
static bool add_first_pval_flag(artifact_type *a_ptr, int flag)
{
    if (!of_has(a_ptr->flags, flag))
    {
        artifact_add_pval(a_ptr, randint1(4), flag);
        of_on(a_ptr->flags, flag);
        return TRUE;
    }

    do_pval(a_ptr, flag);
    return FALSE;
}


static void add_stat(artifact_type *a_ptr)
{
    int r;
    bool success = FALSE;

    /* Hack: break out if all stats are raised to avoid an infinite loop */
    if (flags_test_all(a_ptr->flags, OF_SIZE, OF_STR, OF_INT, OF_WIS, OF_DEX, OF_CON, OF_CHR,
        FLAG_END))
    {
        return;
    }

    /* Make sure we add one that hasn't been added yet */
    while (!success)
    {
        r = randint0(6);
        if (r == 0) success = add_fixed_pval_flag(a_ptr, OF_STR);
        else if (r == 1) success = add_fixed_pval_flag(a_ptr, OF_INT);
        else if (r == 2) success = add_fixed_pval_flag(a_ptr, OF_WIS);
        else if (r == 3) success = add_fixed_pval_flag(a_ptr, OF_DEX);
        else if (r == 4) success = add_fixed_pval_flag(a_ptr, OF_CON);
        else if (r == 5) success = add_fixed_pval_flag(a_ptr, OF_CHR);
    }
}


static void add_sustain(artifact_type *a_ptr)
{
    int r;
    bool success = FALSE;

    /* Hack: break out if all stats are sustained to avoid an infinite loop */
    if (flags_test_all(a_ptr->flags, OF_SIZE, OF_SUST_STR, OF_SUST_INT, OF_SUST_WIS, OF_SUST_DEX,
        OF_SUST_CON, OF_SUST_CHR, FLAG_END))
    {
        return;
    }

    while (!success)
    {
        r = randint0(6);
        if (r == 0) success = add_flag(a_ptr, OF_SUST_STR);
        else if (r == 1) success = add_flag(a_ptr, OF_SUST_INT);
        else if (r == 2) success = add_flag(a_ptr, OF_SUST_WIS);
        else if (r == 3) success = add_flag(a_ptr, OF_SUST_DEX);
        else if (r == 4) success = add_flag(a_ptr, OF_SUST_CON);
        else if (r == 5) success = add_flag(a_ptr, OF_SUST_CHR);
    }
}


static void add_low_resist(artifact_type *a_ptr)
{
    int r;
    bool success = FALSE;

    /* Hack - if all low resists added already, exit to avoid infinite loop */
    if (flags_test_all(a_ptr->flags, OF_SIZE, OF_RES_ACID, OF_RES_ELEC, OF_RES_FIRE, OF_RES_COLD,
        FLAG_END))
    {
        return;
    }

    while (!success)
    {
        r = randint0(4);
        if (r == 0) success = add_flag(a_ptr, OF_RES_ACID);
        else if (r == 1) success = add_flag(a_ptr, OF_RES_ELEC);
        else if (r == 2) success = add_flag(a_ptr, OF_RES_FIRE);
        else if (r == 3) success = add_flag(a_ptr, OF_RES_COLD);
    }
}


static void add_high_resist(artifact_type *a_ptr)
{
    int r, i, temp;
    int count = 0;
    bool success = FALSE;

    /* Add a high resist, according to the generated frequency distribution. */
    temp = 0;
    for (i = 0; i < ART_IDX_HIGH_RESIST_COUNT; i++)
        temp += artprobs[art_idx_high_resist[i]];

    /* The following will fail (cleanly) if all high resists already added */
    while (!success && (count < MAX_TRIES))
    {
        /* Randomize from 1 to this total amount */
        r = randint1(temp);

        /* Determine which (weighted) resist this number corresponds to */
        temp = artprobs[art_idx_high_resist[0]];
        i = 0;
        while ((r > temp) && (i < ART_IDX_HIGH_RESIST_COUNT))
        {
            temp += artprobs[art_idx_high_resist[i]];
            i++;
        }

        /* Now i should give us the index of the correct high resist */
        if (i == 0) success = add_flag(a_ptr, OF_RES_POIS);
        else if (i == 1) success = add_flag(a_ptr, OF_RES_FEAR);
        else if (i == 2) success = add_flag(a_ptr, OF_RES_LIGHT);
        else if (i == 3) success = add_flag(a_ptr, OF_RES_DARK);
        else if (i == 4) success = add_flag(a_ptr, OF_RES_BLIND);
        else if (i == 5) success = add_flag(a_ptr, OF_RES_CONFU);
        else if (i == 6) success = add_flag(a_ptr, OF_RES_SOUND);
        else if (i == 7) success = add_flag(a_ptr, OF_RES_SHARD);
        else if (i == 8) success = add_flag(a_ptr, OF_RES_NEXUS);
        else if (i == 9) success = add_flag(a_ptr, OF_RES_NETHR);
        else if (i == 10) success = add_flag(a_ptr, OF_RES_CHAOS);
        else if (i == 11) success = add_flag(a_ptr, OF_RES_DISEN);
        else if (i == 12) success = add_flag(a_ptr, OF_RES_STUN);

        count++;
    }
}


static void add_slay(artifact_type *a_ptr, bool brand, bool melee)
{
    int count;
    const struct slay *s_ptr;
    bitflag mask[OF_SIZE];

    /* Hack -- Do not allow slays/brands on nonweapons other than rings and gloves */
    if (!weapon_p(a_ptr) && !art_is_ammo(a_ptr) && (a_ptr->tval != TV_RING) &&
        (a_ptr->tval != TV_GLOVES))
    {
        return;
    }

    if (brand)
        create_mask(mask, FALSE, OFT_BRAND, OFT_MAX);
    else
        create_mask(mask, FALSE, OFT_SLAY, OFT_KILL, OFT_MAX);

    for (count = 0; count < MAX_TRIES; count++)
    {
        s_ptr = random_slay(mask);
        if (!of_has(a_ptr->flags, s_ptr->object_flag))
        {
            of_on(a_ptr->flags, s_ptr->object_flag);

            /* PWMAngband: chance of ESP_XXX */
            if (!brand && melee && one_in_(s_ptr->esp_chance))
                of_on(a_ptr->flags, s_ptr->esp_flag);

            return;
        }
    }
}


static void add_damage_dice(artifact_type *a_ptr)
{
    /* Changed this to increments 1 or 2 only */
    a_ptr->dd += (byte)randint1(2);
    if (a_ptr->dd > MAX_WEAPON_DICE) a_ptr->dd = MAX_WEAPON_DICE;
}


static void add_to_hit(artifact_type *a_ptr, int fixed, int random)
{
    /* Inhibit above certain thresholds */
    if (a_ptr->to_h >= VERYHIGH_TO_HIT)
    {
        /* Strongly inhibit */
        if (!INHIBIT_STRONG) return;
    }
    else if (a_ptr->to_h >= HIGH_TO_HIT)
    {
        /* Weakly inhibit */
        if (!INHIBIT_WEAK) return;
    }
    a_ptr->to_h += (s16b)(fixed + randint0(random));

    /* Hack -- Dark swords never have high tohit (to keep high antimagic field) */
    if ((a_ptr->tval == TV_SWORD) && (a_ptr->sval == SV_DARK_SWORD))
    {
        if (a_ptr->to_h > 0) a_ptr->to_h = randint1(a_ptr->to_h);
        if (a_ptr->to_h > 15) a_ptr->to_h = 15;
    }
}


static void add_to_dam(artifact_type *a_ptr, int fixed, int random)
{
    /* Inhibit above certain thresholds */
    if (a_ptr->to_d >= VERYHIGH_TO_DAM)
    {
        /* Strongly inhibit */
        if (!INHIBIT_STRONG) return;
    }
    else if (a_ptr->to_d >= HIGH_TO_DAM)
    {
        /* Weakly inhibit */
        if (!INHIBIT_WEAK) return;
    }
    a_ptr->to_d += (s16b)(fixed + randint0(random));

    /* Hack -- Dark swords never have high todam (to keep high antimagic field) */
    if ((a_ptr->tval == TV_SWORD) && (a_ptr->sval == SV_DARK_SWORD))
    {
        if (a_ptr->to_d > 0) a_ptr->to_d = randint1(a_ptr->to_d);
        if (a_ptr->to_d > 15) a_ptr->to_d = 15;
    }
}


static void add_to_AC(artifact_type *a_ptr, int fixed, int random)
{
    /* Inhibit above certain thresholds */
    if (a_ptr->to_a >= VERYHIGH_TO_AC)
    {
        /* Strongly inhibit */
        if (!INHIBIT_STRONG) return;
    }
    else if (a_ptr->to_a >= HIGH_TO_AC)
    {
        /* Weakly inhibit */
        if (!INHIBIT_WEAK) return;
    }
    a_ptr->to_a += (s16b)(fixed + randint0(random));
}


static void add_weight_mod(artifact_type *a_ptr)
{
    a_ptr->weight = (a_ptr->weight * 9) / 10;
}


/*
 * Add a random immunity to this artifact
 * ASSUMPTION: All immunities are equally likely.
 * ToDo: replace with lookup once immunities are abstracted
 */
static void add_immunity(artifact_type *a_ptr)
{
    int imm_type = randint0(4);

    switch (imm_type)
    {
        case 0: of_on(a_ptr->flags, OF_IM_ACID); break;
        case 1: of_on(a_ptr->flags, OF_IM_ELEC); break;
        case 2: of_on(a_ptr->flags, OF_IM_FIRE); break;
        case 3: of_on(a_ptr->flags, OF_IM_COLD); break;
    }
}


/* Add an activation (called only if neither artifact nor base item has one) */
static void add_activation(artifact_type *a_ptr, s32b target_power)
{
    int i, x, p, max_effect = 0;
    int count = 0;

    /* Work out the maximum allowed effect power */
    for (i = 0; i < EF_MAX; i++)
    {
        p = effect_power(i);
        if ((p > max_effect) && (p < INHIBIT_POWER)) max_effect = p;
    }

    /* Select an effect at random */
    while (count < MAX_TRIES)
    {
        x = randint0(EF_MAX);
        p = effect_power(x);

        /*
         * Check that activation is useful but not exploitable
         * and roughly proportionate to the overall power
         */
        if ((p < INHIBIT_POWER) &&
            (100 * p / max_effect > 50 * target_power / max_power) &&
            (100 * p / max_effect < 200 * target_power / max_power))
        {
            a_ptr->effect = x;
            a_ptr->time.base = (p * 8);
            a_ptr->time.dice = ((p > 5)? (p / 5): 1);
            a_ptr->time.sides = p;
            return;
        }
        count++;
    }
}


static void add_shots(artifact_type *a_ptr)
{
    /* Hack -- Do not allow extra shots on nonweapons other than gloves */
    if ((a_ptr->tval != TV_BOW) && (a_ptr->tval != TV_GLOVES)) return;

    add_pval_flag(a_ptr, OF_SHOTS);
}


static void add_blows(artifact_type *a_ptr)
{
    /* Hack -- Do not allow extra blows on nonweapons other than gloves */
    if (!melee_p(a_ptr) && (a_ptr->tval != TV_GLOVES)) return;

    add_pval_flag(a_ptr, OF_BLOWS);
}


/*
 * Obtain extra ESP power
 */
int get_new_esp(bitflag flags[OF_SIZE])
{
    int i, j, options = 0, flag = 0;
    bitflag newf[OF_SIZE];

    create_mask(newf, FALSE, OFT_ESP, OFT_MAX);

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


static void add_missile_dice(artifact_type *a_ptr)
{
    a_ptr->dd++;
    if (a_ptr->dd > MAX_AMMO_DICE) a_ptr->dd = MAX_AMMO_DICE;
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
static void build_freq_table(artifact_type *a_ptr, s16b *freq)
{
    int i, j;
    s16b f_temp[ART_IDX_TOTAL];

    /* First, set everything to zero */
    for (i = 0; i < ART_IDX_TOTAL; i++)
    {
        f_temp[i] = 0;
        freq[i] = 0;
    }

    /* Now copy over appropriate frequencies for applicable abilities */

    /* Bow abilities */
    if (a_ptr->tval == TV_BOW)
    {
        for (j = 0; j < ART_IDX_BOW_COUNT; j++)
            f_temp[art_idx_bow[j]] = artprobs[art_idx_bow[j]];
    }

    /* General weapon abilities */
    if (weapon_p(a_ptr))
    {
        for (j = 0; j < ART_IDX_WEAPON_COUNT; j++)
            f_temp[art_idx_weapon[j]] = artprobs[art_idx_weapon[j]];
    }

    /* General ammo abilities */
    else if (art_is_ammo(a_ptr))
    {
        for (j = 0; j < ART_IDX_AMMO_COUNT; j++)
            f_temp[art_idx_ammo[j]] = artprobs[art_idx_ammo[j]];
    }

    /* General non-weapon abilities */
    else
    {
        for (j = 0; j < ART_IDX_NONWEAPON_COUNT; j++)
            f_temp[art_idx_nonweapon[j]] = artprobs[art_idx_nonweapon[j]];
    }

    /* General melee abilities */
    if (melee_p(a_ptr))
    {
        for (j = 0; j < ART_IDX_MELEE_COUNT; j++)
            f_temp[art_idx_melee[j]] = artprobs[art_idx_melee[j]];
    }

    /* General armor abilities */
    if ((a_ptr->tval == TV_BOOTS) || (a_ptr->tval == TV_GLOVES) ||
        (a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN) ||
        (a_ptr->tval == TV_SHIELD) || (a_ptr->tval == TV_CLOAK) ||
        body_armor_p(a_ptr))
    {
        for (j = 0; j < ART_IDX_ALLARMOR_COUNT; j++)
            f_temp[art_idx_allarmor[j]] = artprobs[art_idx_allarmor[j]];
    }

    /* Boot abilities */
    if (a_ptr->tval == TV_BOOTS)
    {
        for (j = 0; j < ART_IDX_BOOT_COUNT; j++)
            f_temp[art_idx_boot[j]] = artprobs[art_idx_boot[j]];
    }

    /* Glove abilities */
    if (a_ptr->tval == TV_GLOVES)
    {
        for (j = 0; j < ART_IDX_GLOVE_COUNT; j++)
            f_temp[art_idx_glove[j]] = artprobs[art_idx_glove[j]];
    }

    /* Headgear abilities */
    if ((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN))
    {
        for (j = 0; j < ART_IDX_HELM_COUNT; j++)
            f_temp[art_idx_headgear[j]] = artprobs[art_idx_headgear[j]];
    }

    /* Shield abilities */
    if (a_ptr->tval == TV_SHIELD)
    {
        for (j = 0; j < ART_IDX_SHIELD_COUNT; j++)
            f_temp[art_idx_shield[j]] = artprobs[art_idx_shield[j]];
    }

    /* Cloak abilities */
    if (a_ptr->tval == TV_CLOAK)
    {
        for (j = 0; j < ART_IDX_CLOAK_COUNT; j++)
            f_temp[art_idx_cloak[j]] = artprobs[art_idx_cloak[j]];
    }

    /* Armor abilities */
    if (body_armor_p(a_ptr))
    {
        for (j = 0; j < ART_IDX_ARMOR_COUNT; j++)
            f_temp[art_idx_armor[j]] = artprobs[art_idx_armor[j]];
    }

    /* Digger abilities */
    if (a_ptr->tval == TV_DIGGING)
    {
        for (j = 0; j < ART_IDX_DIGGER_COUNT; j++)
            f_temp[art_idx_digger[j]] = artprobs[art_idx_digger[j]];
    }

    /* Mage staff abilities */
    if (a_ptr->tval == TV_MSTAFF)
    {
        for (j = 0; j < ART_IDX_MSTAFF_COUNT; j++)
            f_temp[art_idx_mstaff[j]] = artprobs[art_idx_mstaff[j]];
    }

    /* Missile abilities */
    if (art_is_ammo(a_ptr))
    {
        for (j = 0; j < ART_IDX_MISSILE_COUNT; j++)
            f_temp[art_idx_missile[j]] = artprobs[art_idx_missile[j]];
    }

    /* PWMAngband: missiles have only specific abilities */
    else
    {
        /* General abilities - no constraint */
        for (j = 0; j < ART_IDX_GEN_COUNT; j++)
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
static void add_ability_aux(artifact_type *a_ptr, int r, s32b target_power)
{
    switch (r)
    {
        case ART_IDX_BOW_SHOTS:
        case ART_IDX_NONWEAPON_SHOTS:
            add_shots(a_ptr);
            break;

        case ART_IDX_BOW_MIGHT:
            add_pval_flag(a_ptr, OF_MIGHT);
            break;

        case ART_IDX_WEAPON_HIT:
        case ART_IDX_AMMO_HIT:
        case ART_IDX_NONWEAPON_HIT:
            add_to_hit(a_ptr, 1, 2 * mean_hit_increment);
            break;

        case ART_IDX_WEAPON_DAM:
        case ART_IDX_AMMO_DAM:
        case ART_IDX_NONWEAPON_DAM:
            add_to_dam(a_ptr, 1, 2 * mean_dam_increment);
            break;

        case ART_IDX_NONWEAPON_HIT_DAM:
            add_to_hit(a_ptr, 1, 2 * mean_hit_increment);
            add_to_dam(a_ptr, 1, 2 * mean_dam_increment);
            break;

        case ART_IDX_WEAPON_AGGR:
        case ART_IDX_NONWEAPON_AGGR:
            if (target_power > AGGR_POWER) add_flag(a_ptr, OF_AGGRAVATE);
            break;

        case ART_IDX_MELEE_BLESS:
            add_flag(a_ptr, OF_BLESSED);
            break;

        case ART_IDX_BOW_BRAND:
        case ART_IDX_MELEE_BRAND:
        case ART_IDX_NONWEAPON_BRAND:
        case ART_IDX_MISSILE_BRAND:
            add_slay(a_ptr, TRUE, TRUE);
            break;

        case ART_IDX_BOW_SLAY:
        case ART_IDX_MELEE_SLAY:
        case ART_IDX_NONWEAPON_SLAY:
            add_slay(a_ptr, FALSE, TRUE);
            break;

        case ART_IDX_MELEE_SINV:
        case ART_IDX_HELM_SINV:
        case ART_IDX_GEN_SINV:
        case ART_IDX_MSTAFF_SINV:
            add_flag(a_ptr, OF_SEE_INVIS);
            break;

        case ART_IDX_MELEE_BLOWS:
        case ART_IDX_NONWEAPON_BLOWS:
            add_blows(a_ptr);
            break;

        case ART_IDX_MELEE_AC:
        case ART_IDX_BOOT_AC:
        case ART_IDX_GLOVE_AC:
        case ART_IDX_HELM_AC:
        case ART_IDX_SHIELD_AC:
        case ART_IDX_CLOAK_AC:
        case ART_IDX_ARMOR_AC:
        case ART_IDX_GEN_AC:
            add_to_AC(a_ptr, 1, 2 * mean_ac_increment);
            break;

        case ART_IDX_MELEE_DICE:
            add_damage_dice(a_ptr);
            break;

        case ART_IDX_MELEE_WEIGHT:
        case ART_IDX_ALLARMOR_WEIGHT:
            add_weight_mod(a_ptr);
            break;

        case ART_IDX_DIGGER_TUNN:
        case ART_IDX_GEN_TUNN:
            add_pval_flag(a_ptr, OF_TUNNEL);
            break;

        case ART_IDX_BOOT_FEATHER:
        case ART_IDX_GEN_FEATHER:
            add_flag(a_ptr, OF_FEATHER);
            break;

        case ART_IDX_BOOT_STEALTH:
        case ART_IDX_CLOAK_STEALTH:
        case ART_IDX_ARMOR_STEALTH:
        case ART_IDX_GEN_STEALTH:
            add_pval_flag(a_ptr, OF_STEALTH);
            break;

        case ART_IDX_BOOT_SPEED:
        case ART_IDX_GEN_SPEED:
            add_first_pval_flag(a_ptr, OF_SPEED);
            break;

        case ART_IDX_GLOVE_FA:
        case ART_IDX_GEN_FA:
        case ART_IDX_MSTAFF_FA:
            add_flag(a_ptr, OF_FREE_ACT);
            break;

        case ART_IDX_GLOVE_DEX:
            add_fixed_pval_flag(a_ptr, OF_DEX);
            break;

        case ART_IDX_GLOVE_MANA:
            add_pval_flag(a_ptr, OF_MANA);
            break;

        case ART_IDX_GLOVE_ID:
        case ART_IDX_HELM_ID:
            add_flag(a_ptr, OF_KNOWLEDGE);
            break;

        case ART_IDX_HELM_RBLIND:
        case ART_IDX_GEN_RBLIND:
        case ART_IDX_MSTAFF_RBLIND:
            add_flag(a_ptr, OF_RES_BLIND);
            break;

        case ART_IDX_HELM_ESP:
        case ART_IDX_GEN_ESP:
        case ART_IDX_MSTAFF_ESP:
        {
            /* PWMAngband: random ESP instead of full ESP */
            int esp_flag = get_new_esp(a_ptr->flags);

            if (esp_flag) add_flag(a_ptr, esp_flag);
            break;
        }

        case ART_IDX_HELM_WIS:
            add_fixed_pval_flag(a_ptr, OF_WIS);
            break;

        case ART_IDX_HELM_INT:
        case ART_IDX_MSTAFF_INT:
            add_fixed_pval_flag(a_ptr, OF_INT);
            break;

        case ART_IDX_SHIELD_LRES:
        case ART_IDX_ARMOR_LRES:
        case ART_IDX_GEN_LRES:
            add_low_resist(a_ptr);
            break;

        case ART_IDX_ARMOR_HLIFE:
        case ART_IDX_GEN_HLIFE:
            add_flag(a_ptr, OF_HOLD_LIFE);
            break;

        case ART_IDX_ARMOR_CON:
            add_fixed_pval_flag(a_ptr, OF_CON);
            break;

        case ART_IDX_ARMOR_ALLRES:
            add_flag(a_ptr, OF_RES_ACID);
            add_flag(a_ptr, OF_RES_ELEC);
            add_flag(a_ptr, OF_RES_FIRE);
            add_flag(a_ptr, OF_RES_COLD);
            break;

        case ART_IDX_ARMOR_HRES:
            add_high_resist(a_ptr);
            break;

        case ART_IDX_MISSILE_SLAY:
            add_slay(a_ptr, FALSE, FALSE);
            break;

        case ART_IDX_MISSILE_DICE:
            add_missile_dice(a_ptr);
            break;

        case ART_IDX_GEN_STAT:
            add_stat(a_ptr);
            break;

        case ART_IDX_GEN_SUST:
            add_sustain(a_ptr);
            break;

        case ART_IDX_GEN_SEARCH:
            add_pval_flag(a_ptr, OF_SEARCH);
            break;

        case ART_IDX_GEN_INFRA:
            add_pval_flag(a_ptr, OF_INFRA);
            break;

        case ART_IDX_GEN_IMMUNE:
            add_immunity(a_ptr);
            break;

        case ART_IDX_GEN_LIGHT:
        {
            int i;

            /* Only once (this also means not on light sources) */
            if (of_has(a_ptr->flags, OF_LIGHT)) break;

            /* Check for existing pval = 1 (for combining) */
            for (i = 0; i < a_ptr->num_pvals; i++)
            {
                if (a_ptr->pval[i] == 1) break;
            }

            /* Combine flag */
            if (i < a_ptr->num_pvals)
            {
                of_on(a_ptr->pval_flags[i], OF_LIGHT);
                of_on(a_ptr->flags, OF_LIGHT);
            }

            /* Create a new pval if we can */
            else if (a_ptr->num_pvals < MAX_PVALS)
            {
                a_ptr->pval[a_ptr->num_pvals] = 1;
                of_on(a_ptr->pval_flags[a_ptr->num_pvals], OF_LIGHT);
                a_ptr->num_pvals++;
                of_on(a_ptr->flags, OF_LIGHT);
            }

            break;
        }

        case ART_IDX_GEN_SDIG:
            add_flag(a_ptr, OF_SLOW_DIGEST);
            break;

        case ART_IDX_GEN_REGEN:
            add_flag(a_ptr, OF_REGEN);
            break;

        case ART_IDX_GEN_RPOIS:
            add_flag(a_ptr, OF_RES_POIS);
            break;

        case ART_IDX_GEN_RFEAR:
            add_flag(a_ptr, OF_RES_FEAR);
            break;

        case ART_IDX_GEN_RLIGHT:
            add_flag(a_ptr, OF_RES_LIGHT);
            break;

        case ART_IDX_GEN_RDARK:
            add_flag(a_ptr, OF_RES_DARK);
            break;

        case ART_IDX_GEN_RCONF:
        case ART_IDX_MSTAFF_RCONF:
            add_flag(a_ptr, OF_RES_CONFU);
            break;

        case ART_IDX_GEN_RSOUND:
            add_flag(a_ptr, OF_RES_SOUND);
            break;

        case ART_IDX_GEN_RSHARD:
            add_flag(a_ptr, OF_RES_SHARD);
            break;

        case ART_IDX_GEN_RNEXUS:
            add_flag(a_ptr, OF_RES_NEXUS);
            break;

        case ART_IDX_GEN_RNETHER:
            add_flag(a_ptr, OF_RES_NETHR);
            break;

        case ART_IDX_GEN_RCHAOS:
            add_flag(a_ptr, OF_RES_CHAOS);
            break;

        case ART_IDX_GEN_RDISEN:
            add_flag(a_ptr, OF_RES_DISEN);
            break;

        case ART_IDX_GEN_ACTIV:
            if (!a_ptr->effect) add_activation(a_ptr, target_power);
            break;
    }
}


/*
 * Randomly select an extra ability to be added to the artifact in question.
 */
static void add_ability(artifact_type *a_ptr, s32b target_power)
{
    int r;

    /* Choose a random ability using the frequency table previously defined */
    r = choose_ability(art_freq);

    /* Add the appropriate ability */
    add_ability_aux(a_ptr, r, target_power);

    /* Now remove contradictory or redundant powers. */
    remove_contradictory(a_ptr);

    /* Adding WIS to sharp weapons always blesses them */
    if (of_has(a_ptr->flags, OF_WIS) && ((a_ptr->tval == TV_SWORD) || (a_ptr->tval == TV_POLEARM)))
        add_flag(a_ptr, OF_BLESSED);
}


/*
 * Try to supercharge this item by running through the list of the supercharge
 * abilities and attempting to add each in turn.  An artifact only gets one
 * chance at each of these up front (if applicable).
 */
static void try_supercharge(artifact_type *a_ptr, s32b target_power)
{
    /* Huge damage dice - missiles only */
    if (art_is_ammo(a_ptr))
    {
        if (randint0(z_info->a_max) < artprobs[ART_IDX_MISSILE_DICE_SUPER])
        {
            a_ptr->dd += 2 + randint0(3);
            if (a_ptr->dd > MAX_AMMO_DICE) a_ptr->dd = MAX_AMMO_DICE;
        }

        /* Missiles don't have any other supercharged abilities */
        return;
    }

    /* Huge damage dice or max blows - melee weapon only */
    if (melee_p(a_ptr))
    {
        if (randint0(z_info->a_max) < artprobs[ART_IDX_MELEE_DICE_SUPER])
        {
            a_ptr->dd += 3 + randint0(4);
            if (a_ptr->dd > MAX_WEAPON_DICE) a_ptr->dd = MAX_WEAPON_DICE;
        }
        else if (randint0(z_info->a_max) < artprobs[ART_IDX_MELEE_BLOWS_SUPER])
        {
            if (!of_has(a_ptr->flags, OF_BLOWS))
            {
                artifact_add_pval(a_ptr, INHIBIT_BLOWS - 1, OF_BLOWS);
                of_on(a_ptr->flags, OF_BLOWS);
            }
        }
    }

    /* Bows - max might or shots */
    if (a_ptr->tval == TV_BOW)
    {
        if (randint0(z_info->a_max) < artprobs[ART_IDX_BOW_SHOTS_SUPER])
        {
            if (!of_has(a_ptr->flags, OF_SHOTS))
            {
                artifact_add_pval(a_ptr, INHIBIT_SHOTS - 1, OF_SHOTS);
                of_on(a_ptr->flags, OF_SHOTS);
            }
        }
        else if (randint0(z_info->a_max) < artprobs[ART_IDX_BOW_MIGHT_SUPER])
        {
            if (!of_has(a_ptr->flags, OF_MIGHT))
            {
                artifact_add_pval(a_ptr, INHIBIT_MIGHT - 1, OF_MIGHT);
                of_on(a_ptr->flags, OF_MIGHT);
            }
        }
    }

    /* Big speed bonus - any item (potentially) but more likely on boots */
    if ((randint0(z_info->a_max) < artprobs[ART_IDX_GEN_SPEED_SUPER]) ||
        ((a_ptr->tval == TV_BOOTS) && (randint0(z_info->a_max) < artprobs[ART_IDX_BOOT_SPEED])))
    {
        int pval = 5 + randint0(6);

        if (INHIBIT_WEAK) pval += randint1(3);
        if (INHIBIT_STRONG) pval += 1 + randint1(6);
        artifact_add_pval(a_ptr, pval, OF_SPEED);
        of_on(a_ptr->flags, OF_SPEED);
    }

    /* Big AC bonus */
    if (randint0(z_info->a_max) < artprobs[ART_IDX_GEN_AC_SUPER])
    {
        a_ptr->to_a += 19 + randint1(11);
        if (INHIBIT_WEAK) a_ptr->to_a += randint1(10);
        if (INHIBIT_STRONG) a_ptr->to_a += randint1(20);
    }

    /* Aggravation */
    if (weapon_p(a_ptr))
    {
        if ((randint0(z_info->a_max) < artprobs[ART_IDX_WEAPON_AGGR]) && (target_power > AGGR_POWER))
            of_on(a_ptr->flags, OF_AGGRAVATE);
    }
    else
    {
        if ((randint0(z_info->a_max) < artprobs[ART_IDX_NONWEAPON_AGGR]) &&
            (target_power > AGGR_POWER))
        {
            of_on(a_ptr->flags, OF_AGGRAVATE);
        }
    }
}


/*
 * Make it bad, or if it's already bad, make it worse!
 */
static void do_curse(artifact_type *a_ptr)
{
    int i;

    /* Add bad abilities */
    if (one_in_(7)) of_on(a_ptr->flags, OF_AGGRAVATE);
    if (one_in_(4)) of_on(a_ptr->flags, OF_DRAIN_EXP);
    if (one_in_(7)) of_on(a_ptr->flags, OF_TELEPORT);

    /* Reverse pval and bonuses */
    for (i = 0; i < a_ptr->num_pvals; i++)
    {
        if ((a_ptr->pval[i] > 0) && one_in_(2)) a_ptr->pval[i] = 0 - a_ptr->pval[i];
    }
    if ((a_ptr->to_a > 0) && one_in_(2)) a_ptr->to_a = 0 - a_ptr->to_a;
    if ((a_ptr->to_h > 0) && one_in_(2)) a_ptr->to_h = 0 - a_ptr->to_h;
    if ((a_ptr->to_d > 0) && one_in_(4)) a_ptr->to_d = 0 - a_ptr->to_d;

    /* If it's cursed, make it worse */
    if (of_has(a_ptr->flags, OF_LIGHT_CURSE))
    {
        if (one_in_(2)) of_on(a_ptr->flags, OF_HEAVY_CURSE);
        return;
    }

    /* Always cursed... */
    of_on(a_ptr->flags, OF_LIGHT_CURSE);

    /* ... and sometimes worse */
    if (one_in_(4)) of_on(a_ptr->flags, OF_HEAVY_CURSE);
}


static int pval_light(struct artifact *a)
{
    int i;

    if (a->tval != TV_LIGHT) return 0;

    for (i = 0; i < a->num_pvals; i++)
    {
        if (of_has(a->pval_flags[i], OF_LIGHT)) return a->pval[i];
    }

    return 0;
}


/*
 * Note the three special cases (One Ring, Grond, Morgoth).
 */
static bool scramble_artifact(artifact_type *a_ptr)
{
    artifact_type a_old;
    s32b power;
    int tries = 0;
    s32b ap = 0;
    bool curse_me = FALSE;
    bool success = FALSE;
    int i;
    bitflag f[OF_SIZE];

    /* Special cases -- Don't randomize these! */
    if ((a_ptr->aidx == ART_POWER) || (a_ptr->aidx == ART_GROND) || (a_ptr->aidx == ART_MORGOTH) ||
        (a_ptr->aidx == ART_SILMARIL))
    {
        return FALSE;
    }

    /* Skip unused artifacts, too! */
    if (a_ptr->tval == 0) return FALSE;

    /* Evaluate the original artifact to determine the power level. */
    if (a_ptr->aidx < z_info->a_max)
        power = base_power[a_ptr->aidx];

    /* Choose a random power level for Rings of Power */
    else
        power = AGGR_POWER * (75 + randint0(51)) / 100;

    /* If it has a restricted ability then don't randomize it. */
    if (power >= INHIBIT_POWER) return FALSE;

    /* Curse artifacts with negative power */
    if (power < 0) curse_me = TRUE;

    /*
     * Flip the sign on power if it's negative, since it's only used for base
     * item choice
     */
    if (power < 0) power = 0 - power;

    /* PWMAngband: keep the item kind */
    if (!of_has(a_ptr->flags, OF_SPECIAL_ART))
    {
        /*
         * Normal artifact - assign the various fields corresponding to
         * the base item type
         */
        choose_item(a_ptr);
    }
    else
    {
        /*
         * Special artifact (light source, ring, or amulet).
         * Clear the following fields; leave the rest alone.
         */
        int pval = pval_light(a_ptr);

        a_ptr->to_h = a_ptr->to_d = a_ptr->to_a = 0;
        a_ptr->num_pvals = 0;
        of_wipe(a_ptr->flags);
        for (i = 0; i < MAX_PVALS; i++)
        {
            a_ptr->pval[i] = 0;
            of_wipe(a_ptr->pval_flags[i]);
        }

        /* Clear the activations for rings and amulets but not lights */
        if (a_ptr->tval != TV_LIGHT) a_ptr->effect = 0;

        /* Restore lights */
        else
        {
            of_on(a_ptr->flags, OF_NO_FUEL);
            if (pval)
            {
                a_ptr->pval[a_ptr->num_pvals] = pval;
                of_on(a_ptr->pval_flags[a_ptr->num_pvals], OF_LIGHT);
                a_ptr->num_pvals++;
                of_on(a_ptr->flags, OF_LIGHT);
            }
        }

        /* Artifacts ignore everything */
        create_mask(f, FALSE, OFT_IGNORE, OFT_MAX);
        of_union(a_ptr->flags, f);

        /* Restore "special" flag */
        of_on(a_ptr->flags, OF_SPECIAL_ART);
    }

    /* Generate the cumulative frequency table for this item type */
    build_freq_table(a_ptr, art_freq);

    /* Copy artifact info temporarily. */
    a_old = *a_ptr;

    /* Give this artifact a shot at being supercharged */
    try_supercharge(a_ptr, power);
    ap = artifact_power(a_ptr);
    if (ap > (power * 23) / 20 + 1)
    {
        /* Too powerful -- Put it back */
        *a_ptr = a_old;
    }

    /* First draft: add two abilities, then curse it three times. */
    if (curse_me)
    {
        /* Copy artifact info temporarily. */
        a_old = *a_ptr;
        do
        {
            add_ability(a_ptr, power);
            add_ability(a_ptr, power);
            do_curse(a_ptr);
            do_curse(a_ptr);
            do_curse(a_ptr);
            remove_contradictory(a_ptr);
            ap = artifact_power(a_ptr);

            /* Accept if it doesn't have any inhibited abilities */
            if (ap < INHIBIT_POWER) success = TRUE;

            /* Otherwise go back and try again */
            else *a_ptr = a_old;
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
            a_old = *a_ptr;

            add_ability(a_ptr, power);
            ap = artifact_power(a_ptr);

            /* Pushed both limits up by about 5% */
            if (ap > (power * 23) / 20 + 1)
            {
                /* Too powerful -- put it back */
                *a_ptr = a_old;
                continue;
            }

            /* Just right */
            else if (ap >= (power * 19) / 20)
            {
                /* Add rescue for crappy weapons */
                if (weapon_p(a_ptr) && (a_ptr->to_d < 10))
                    a_ptr->to_d += randint0(10);

                break;
            }
        }

        if (tries >= MAX_TRIES)
        {
            /*
             * We couldn't generate an artifact within the number of permitted
             * iterations.
             */
            return FALSE;
        }
    }

    /* Set depth and rarity info according to power */
    /* This is currently very tricky for special artifacts */

    /* Flip cursed items to avoid overflows */
    if (ap < 0) ap = 0 - ap;

    if (of_has(a_ptr->flags, OF_SPECIAL_ART))
    {
        a_ptr->alloc_max = 127;
        if (ap > avg_power)
        {
            a_ptr->alloc_prob = 1;
            a_ptr->alloc_min = MAX(50, ((ap + 150) * 100 / max_power));
        }
        else if (ap > 30)
        {
            a_ptr->alloc_prob = MAX(2, (avg_power - ap) / 20);
            a_ptr->alloc_min = MAX(25, ((ap + 200) * 100 / max_power));
        }
        else
        {
            a_ptr->alloc_prob = 50 - ap;
            a_ptr->alloc_min = 5;
        }
    }
    else if (a_ptr->aidx < z_info->a_max)
    {
        a_ptr->alloc_max = MIN(127, (ap * 4) / 5);
        a_ptr->alloc_min = MIN(100, ((ap + 100) * 100 / max_power));

        /* Sanity check */
        if (a_ptr->alloc_max < a_ptr->alloc_min) a_ptr->alloc_max = a_ptr->alloc_min;

        /* Leave alloc_prob consistent with base art total rarity */
    }

    /* Sanity check */
    if (a_ptr->alloc_prob > 99) a_ptr->alloc_prob = 99;
    if (a_ptr->alloc_prob < 1) a_ptr->alloc_prob = 1;

    /* Success */
    return TRUE;
}


static artifact_type* do_randart_aux(struct artifact *artifact)
{
    artifact_type *a_ptr;

    /* Get pointer to our artifact_type object */
    a_ptr = &randart;

    /* Wipe the artifact_type structure */
    WIPE(a_ptr, artifact_type);

    /* Copy info from the corresponding static artifact */
    COPY(a_ptr, artifact, artifact_type);

    /* Hack in some values for Rings of Power */
    if (a_ptr->aidx >= z_info->a_max)
    {
        a_ptr->tval = TV_RING;
        a_ptr->sval = SV_RING_NAZGUL;
        a_ptr->name = dummy;
        a_ptr->alloc_prob = 100;
        a_ptr->alloc_min = 1;
        a_ptr->alloc_max = 127;
    }

    /* Generate the artifact */
    if (!scramble_artifact(a_ptr)) WIPE(a_ptr, artifact_type);

    /* Success */
    return (a_ptr);
}


/*
 * Generate a random artifact
 */
artifact_type* do_randart(s32b randart_seed, struct artifact *artifact)
{
    u32b tmp_seed;
    bool rand_old;
    artifact_type *a_ptr;

    /* Save the RNG */
    tmp_seed = Rand_value;
    rand_old = Rand_quick;

    /* Prepare to use the Angband "simple" RNG. */
    Rand_value = randart_seed;
    Rand_quick = TRUE;

    /* Generate the random artifact */
    a_ptr = do_randart_aux(artifact);

    /* When done, resume use of the Angband "complex" RNG. */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;

    /* Return a pointer to the artifact_type */
    return (a_ptr);
}


/*
 * Initialize the random artifact generator
 */
void init_randart_generator(void)
{
    /* Allocate the "base powers" array */
    base_power = C_ZNEW(z_info->a_max, s32b);

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


artifact_type* get_artifact(const object_type *o_ptr)
{
    /* Randart */
    if (o_ptr->randart_seed) return do_randart(o_ptr->randart_seed, o_ptr->artifact);

    /* Static artifact */
    return o_ptr->artifact;
}
