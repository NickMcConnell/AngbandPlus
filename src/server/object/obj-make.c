/*
 * File: obj-make.c
 * Purpose: Object generation functions
 *
 * Copyright (c) 1987-2007 Angband contributors
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
#include "../effects.h"
#include "pval.h"
#include <math.h>


/*
 * The chance of inflating the requested object level (1/x).
 * Lower values yield better objects more often.
 */
#define GREAT_OBJ   20


/*
 * There is a 1/20 (5%) chance that ego items with an inflated base level are
 * generated when an object is turned into an ego item (see make_ego_item()
 * in obj-make.c). As above, lower values yield better ego items more often.
 */
#define GREAT_EGO   20


/* Define a value for minima which will be ignored. */
#define NO_MINIMUM  255


/*** Make an ego item ***/


/*
 * This is a safe way to choose a random new flag to add to an object.
 * It takes the existing flags and an array of new flags,
 * and returns an entry from newf, or 0 if there are no
 * new flags available.
 */
static int get_new_attr(bitflag flags[OF_SIZE], bitflag newf[OF_SIZE])
{
    int i, options = 0, flag = 0;

    for (i = of_next(newf, FLAG_START); i != FLAG_END; i = of_next(newf, i + 1))
    {
        /* Skip this one if the flag is already present */
        if (of_has(flags, i)) continue;

        /*
         * Each time we find a new possible option, we have a 1-in-N chance of
         * choosing it and an (N-1)-in-N chance of keeping a previous one
         */
        if (one_in_(++options)) flag = i;
    }

    return flag;
}


/*
 * Obtain extra power
 */
static int get_new_power(bitflag flags[OF_SIZE], int esp_flag)
{
    int i, options = 0, flag = 0;
    bitflag newf[OF_SIZE];

    create_mask(newf, FALSE, OFT_PROT, OFT_MISC, OFT_MAX);

    for (i = of_next(newf, FLAG_START); i != FLAG_END; i = of_next(newf, i + 1))
    {
        /* Skip this one if the flag is already present */
        if (of_has(flags, i)) continue;

        /* Skip ESP power if no extra ESP power can be added */
        if ((i == OF_ESP_POWER) && !esp_flag) continue;

        /*
         * Each time we find a new possible option, we have a 1-in-N chance of
         * choosing it and an (N-1)-in-N chance of keeping a previous one
         */
        if (one_in_(++options)) flag = i;
    }

    return flag;
}


/*
 * Generate random powers
 */
static void do_powers(object_type *o_ptr, byte type)
{
    bitflag flags[OF_SIZE], newf[OF_SIZE];

    object_flags(o_ptr, flags);

    switch (type)
    {
        case OBJECT_XTRA_TYPE_SUSTAIN:
        {
            create_mask(newf, FALSE, OFT_SUST, OFT_MAX);
            of_on(o_ptr->flags, get_new_attr(flags, newf));
            break;
        }
        case OBJECT_XTRA_TYPE_RESIST:
        {
            create_mask(newf, FALSE, OFT_HRES, OFT_MAX);
            of_on(o_ptr->flags, get_new_attr(flags, newf));
            break;
        }
        case OBJECT_XTRA_TYPE_POWER:
        {
            int esp_flag = get_new_esp(flags);
            int power_flag = get_new_power(flags, esp_flag);

            if ((power_flag == OF_ESP_POWER) && esp_flag)
                of_on(o_ptr->flags, esp_flag);
            else
                of_on(o_ptr->flags, power_flag);
            break;
        }
        case OBJECT_XTRA_RESIST_POWER:
        {
            int esp_flag = get_new_esp(flags);
            int power_flag = get_new_power(flags, esp_flag);

            create_mask(newf, FALSE, OFT_HRES, OFT_MAX);
            of_on(o_ptr->flags, get_new_attr(flags, newf));
            if ((power_flag == OF_ESP_POWER) && esp_flag)
                of_on(o_ptr->flags, esp_flag);
            else
                of_on(o_ptr->flags, power_flag);
            break;
        }
        case OBJECT_XTRA_TYPE_ESP:
        {
            int esp_flag = get_new_esp(flags);

            if (esp_flag)
                of_on(o_ptr->flags, esp_flag);
            break;
        }
    }
}


static void get_power_flags(const object_type *o_ptr, bitflag flags1[OF_SIZE],
    bitflag flags2[OF_SIZE])
{
    int type = OBJECT_XTRA_TYPE_NONE;

    of_wipe(flags1);
    of_wipe(flags2);

    /* Set Amulet of Terken extra power */
    if ((o_ptr->tval == TV_AMULET) && (o_ptr->sval == SV_AMULET_TERKEN))
        type = OBJECT_XTRA_TYPE_POWER;

    /* Set Amulet of ESP extra power */
    if ((o_ptr->tval == TV_AMULET) && (o_ptr->sval == SV_AMULET_ESP))
        type = OBJECT_XTRA_TYPE_ESP;

    /* Set extra powers */
    if (o_ptr->ego) type = o_ptr->ego->xtra;

    /* Get power flags */
    switch (type)
    {
        case OBJECT_XTRA_TYPE_SUSTAIN:
            create_mask(flags1, FALSE, OFT_SUST, OFT_MAX);
            break;
        case OBJECT_XTRA_TYPE_RESIST:
            create_mask(flags1, FALSE, OFT_HRES, OFT_MAX);
            break;
        case OBJECT_XTRA_TYPE_POWER:
            create_mask(flags1, FALSE, OFT_PROT, OFT_MISC, OFT_ESP, OFT_MAX);
            of_off(flags1, OF_ESP_POWER);
            break;
        case OBJECT_XTRA_RESIST_POWER:
            create_mask(flags1, FALSE, OFT_HRES, OFT_MAX);
            create_mask(flags2, FALSE, OFT_PROT, OFT_MISC, OFT_ESP, OFT_MAX);
            of_off(flags2, OF_ESP_POWER);
            break;
        case OBJECT_XTRA_TYPE_ESP:
            create_mask(flags1, FALSE, OFT_ESP, OFT_MAX);
            break;
    }
}


void init_powers(const object_type *o_ptr, int *xtra1, int *xtra2)
{
    bitflag flags1[OF_SIZE], flags2[OF_SIZE];

    get_power_flags(o_ptr, flags1, flags2);

    *xtra1 = of_next(flags1, FLAG_START);
    *xtra2 = of_next(flags2, FLAG_START);
}


static void dec_power(bitflag flags[OF_SIZE], int *xtra)
{
    int flag = of_next(flags, FLAG_START), prevflag;

    /* Item must have an extra power */
    if (*xtra == FLAG_END) return;

    /* Min bound */
    if (*xtra == flag) return;

    /* Decrease extra power */
    do
    {
        prevflag = flag;
        flag = of_next(flags, prevflag + 1);
    }
    while (*xtra != flag);
    *xtra = prevflag;
}


void dec_power1(const object_type *o_ptr, int *xtra1)
{
    bitflag flags1[OF_SIZE], flags2[OF_SIZE];

    get_power_flags(o_ptr, flags1, flags2);
    dec_power(flags1, xtra1);
}


void dec_power2(const object_type *o_ptr, int *xtra2)
{
    bitflag flags1[OF_SIZE], flags2[OF_SIZE];

    get_power_flags(o_ptr, flags1, flags2);
    dec_power(flags2, xtra2);
}


static void inc_power(bitflag flags[OF_SIZE], int *xtra)
{
    int flag;

    /* Item must have an extra power */
    if (*xtra == FLAG_END) return;

    /* Max bound */
    flag = of_next(flags, *xtra + 1);
    if (flag == FLAG_END) return;

    /* Increase extra power */
    *xtra = flag;
}


void inc_power1(const object_type *o_ptr, int *xtra1)
{
    bitflag flags1[OF_SIZE], flags2[OF_SIZE];

    get_power_flags(o_ptr, flags1, flags2);
    inc_power(flags1, xtra1);
}


void inc_power2(const object_type *o_ptr, int *xtra2)
{
    bitflag flags1[OF_SIZE], flags2[OF_SIZE];

    get_power_flags(o_ptr, flags1, flags2);
    inc_power(flags2, xtra2);
}


void do_fixed_powers(object_type *o_ptr, int xtra1, int xtra2)
{
    if (xtra1 != FLAG_END) of_on(o_ptr->flags, xtra1);
    if (xtra2 != FLAG_END) of_on(o_ptr->flags, xtra2);
}


void undo_fixed_powers(object_type *o_ptr, int xtra1, int xtra2)
{
    if (xtra1 != FLAG_END) of_off(o_ptr->flags, xtra1);
    if (xtra2 != FLAG_END) of_off(o_ptr->flags, xtra2);
}


static struct flag_mod
{
    int flag;
    const char *name;
} flag_mods[] =
{
    {OF_SUST_STR, "Sust STR"},
    {OF_SUST_INT, "Sust INT"},
    {OF_SUST_WIS, "Sust WIS"},
    {OF_SUST_DEX, "Sust DEX"},
    {OF_SUST_CON, "Sust CON"},
    {OF_SUST_CHR, "Sust CHR"},
    {OF_RES_POIS, "Pois"},
    {OF_RES_LIGHT, "Light"},
    {OF_RES_DARK, "Dark"},
    {OF_RES_SOUND, "Sound"},
    {OF_RES_SHARD, "Shard"},
    {OF_RES_NEXUS, "Nexus"},
    {OF_RES_NETHR, "Nethr"},
    {OF_RES_CHAOS, "Chaos"},
    {OF_RES_DISEN, "Disen"},
    {OF_RES_FEAR, "Fear"},
    {OF_RES_BLIND, "Blind"},
    {OF_RES_CONFU, "Confu"},
    {OF_RES_STUN, "Stun"},
    {OF_SLOW_DIGEST, "Slow Digest"},
    {OF_FEATHER, "Feather"},
    {OF_LIGHT, "Light"},
    {OF_REGEN, "Regen"},
    {OF_SEE_INVIS, "See Invis"},
    {OF_FREE_ACT, "Free Act"},
    {OF_HOLD_LIFE, "Hold Life"},
    {OF_ESP_ANIMAL, "Esp Animal"},
    {OF_ESP_EVIL, "Esp Evil"},
    {OF_ESP_UNDEAD, "Esp Undead"},
    {OF_ESP_DEMON, "Esp Demon"},
    {OF_ESP_ORC, "Esp Orc"},
    {OF_ESP_TROLL, "Esp Troll"},
    {OF_ESP_GIANT, "Esp Giant"},
    {OF_ESP_DRAGON, "Esp Dragon"},
    {OF_ESP_ALL, "Esp All"},
    {OF_ESP_RADIUS, "Esp Radius"}
};


static const char *get_extra_mod(int xtra)
{
    size_t i;

    for (i = 0; i < N_ELEMENTS(flag_mods); i++)
    {
        if (flag_mods[i].flag == xtra) return flag_mods[i].name;
    }

    return "";
}


void get_extra_mods(int xtra1, int xtra2, char *buf, int len)
{
    my_strcpy(buf, "Regular", len);

    if (xtra1 != FLAG_END)
        my_strcpy(buf, get_extra_mod(xtra1), len);
    if (xtra2 != FLAG_END)
    {
        my_strcat(buf, "/", len);
        my_strcat(buf, get_extra_mod(xtra2), len);
    }
}


/*
 * Select an ego-item that fits the object's tval and sval.
 */
static struct ego_item *ego_find_random(object_type *o_ptr, int level)
{
    int i, j;
    long total = 0L;
    alloc_entry *table = alloc_ego_table;
    ego_item_type *ego;

    /* Go through all possible ego items and find ones which fit this item */
    for (i = 0; i < alloc_ego_size; i++)
    {
        /* Reset any previous probability of this type being picked */
        table[i].prob3 = 0;

        if (level < table[i].level) continue;

        /* Access the ego item */
        ego = &e_info[table[i].index];

        /* XXX Ignore cursed items for now */
        if (cursed_p(ego->flags)) continue;

        /* Test if this is a legal ego item type for this object */
        for (j = 0; j < EGO_TVALS_MAX; j++)
        {
            /* Require identical base type */
            if ((o_ptr->tval == ego->tval[j]) && (o_ptr->sval >= ego->min_sval[j]) &&
                (o_ptr->sval <= ego->max_sval[j]))
            {
                table[i].prob3 = table[i].prob2;
                break;
            }
        }

        /* Total */
        total += table[i].prob3;
    }

    if (total)
    {
        long value = randint0(total);

        for (i = 0; i < alloc_ego_size; i++)
        {
            /* Found the entry */
            if (value < table[i].prob3) break;

            /* Decrement */
            value = value - table[i].prob3;
        }

        return &e_info[table[i].index];
    }

    return NULL;
}


/*
 * Apply minimum standards for ego-items.
 */
static void ego_apply_minima(object_type *o_ptr)
{
    if (!o_ptr->ego) return;

    if ((o_ptr->ego->min_to_h != NO_MINIMUM) && (o_ptr->to_h < o_ptr->ego->min_to_h))
        o_ptr->to_h = o_ptr->ego->min_to_h;
    if ((o_ptr->ego->min_to_d != NO_MINIMUM) && (o_ptr->to_d < o_ptr->ego->min_to_d))
        o_ptr->to_d = o_ptr->ego->min_to_d;
    if ((o_ptr->ego->min_to_a != NO_MINIMUM) && (o_ptr->to_a < o_ptr->ego->min_to_a))
        o_ptr->to_a = o_ptr->ego->min_to_a;

    ego_min_pvals(o_ptr);
}


/*
 * Apply generation magic to an ego-item.
 *
 * Returns the amount to increase the level rating by
 */
void ego_apply_magic(object_type *o_ptr, int lev)
{
    int i, flag, x;
    bitflag f[OF_SIZE];

    of_wipe(f);

    /* Apply extra ego bonuses */
    o_ptr->to_h += randcalc(o_ptr->ego->to_h, lev, RANDOMISE);
    o_ptr->to_d += randcalc(o_ptr->ego->to_d, lev, RANDOMISE);
    o_ptr->to_a += randcalc(o_ptr->ego->to_a, lev, RANDOMISE);

    /* Apply ego pvals */
    for (i = 0; i < o_ptr->ego->num_pvals; i++)
    {
        x = randcalc(o_ptr->ego->pval[i], lev, RANDOMISE);
        for (flag = of_next(o_ptr->ego->pval_flags[i], FLAG_START); flag != FLAG_END;
            flag = of_next(o_ptr->ego->pval_flags[i], flag + 1))
        {
            object_add_pval(o_ptr, x, flag);

            /* Prevent phantom flags */
            if (!of_has(o_ptr->flags, flag)) of_on(f, flag);
        }
    }

    /* Apply remaining flags */
    of_union(o_ptr->flags, o_ptr->ego->flags);
    of_diff(o_ptr->flags, f);

    /* Apply minima */
    ego_apply_minima(o_ptr);

    /* Cloak of lordly resistance can be activated for resistance */
    if (o_ptr->ego->eidx == EGO_CLOAK_LORDLY_RES)
    {
        o_ptr->effect = EF_RESIST_ALL_LORDLY;
        o_ptr->time.base = 150;
        o_ptr->time.dice = 1;
        o_ptr->time.sides = 50;
    }
}


/*
 * Apply minimum pvals to an ego item. Note that 0 is treated as meaning
 * "do not apply a minimum to this pval", so it leaves negative pvals alone.
 */
void ego_min_pvals(object_type *o_ptr)
{
    int i, j, flag;

    for (i = 0; i < o_ptr->num_pvals; i++)
    {
        for (j = 0; j < o_ptr->ego->num_pvals; j++)
        {
            for (flag = of_next(o_ptr->ego->pval_flags[j], FLAG_START); flag != FLAG_END;
                flag = of_next(o_ptr->ego->pval_flags[j], flag + 1))
            {
                if (of_has(o_ptr->pval_flags[i], flag) && (o_ptr->ego->min_pval[j] != NO_MINIMUM) &&
                    (o_ptr->pval[i] < o_ptr->ego->min_pval[j]))
                        object_add_pval(o_ptr, o_ptr->ego->min_pval[j] - o_ptr->pval[i], flag);
            }
        }
    }
}


/*
 * Try to find an ego-item for an object, setting o_ptr->ego if successful and
 * applying various bonuses.
 */
static void make_ego_item(object_type *o_ptr, int level)
{
    /* Cannot further improve artifacts or ego items */
    if (o_ptr->artifact || o_ptr->ego) return;

    /* Occasional "boost" */
    if ((level > 0) && one_in_(GREAT_EGO))
    {
        /* The bizarre calculation again */
        level = 1 + (level * MAX_DEPTH / randint1(MAX_DEPTH));
    }

    /* Try to get a legal ego type for this item */
    o_ptr->ego = ego_find_random(o_ptr, level);

    /* Actually apply the ego template to the item */
    if (o_ptr->ego)
    {
        /* Extra powers */
        do_powers(o_ptr, o_ptr->ego->xtra);

        ego_apply_magic(o_ptr, level);
    }
}


/*** Make an artifact ***/


/*
 * Copy artifact data to a normal object, and set various slightly hacky
 * globals.
 */
void copy_artifact_data(object_type *o_ptr, const artifact_type *a_ptr)
{
    int i;

    /* Extract the fields */
    for (i = 0; i < MAX_PVALS; i++) of_wipe(o_ptr->pval_flags[i]);
    for (i = 0; i < a_ptr->num_pvals; i++)
    {
        o_ptr->pval[i] = a_ptr->pval[i];
        of_copy(o_ptr->pval_flags[i], a_ptr->pval_flags[i]);
    }
    o_ptr->num_pvals = a_ptr->num_pvals;
    o_ptr->ac = a_ptr->ac;
    o_ptr->dd = a_ptr->dd;
    o_ptr->ds = a_ptr->ds;
    o_ptr->to_a = a_ptr->to_a;
    o_ptr->to_h = a_ptr->to_h;
    o_ptr->to_d = a_ptr->to_d;
    o_ptr->weight = a_ptr->weight;
    of_union(o_ptr->flags, a_ptr->flags);

    o_ptr->effect = a_ptr->effect;
    o_ptr->time.base = a_ptr->time.base;
    o_ptr->time.dice = a_ptr->time.dice;
    o_ptr->time.sides = a_ptr->time.sides;
}


/*
 * Create a fake artifact
 */
bool make_fake_artifact(object_type *o_ptr, struct artifact *artifact)
{
    object_kind *kind;

    /* Don't bother with empty artifacts */
    if (!artifact->tval) return FALSE;

    /* Get the "kind" index */
    kind = lookup_kind(artifact->tval, artifact->sval);
    if (!kind) return FALSE;

    /* Create the artifact */
    object_prep(o_ptr, kind, 0, MAXIMISE);

    /* Save the name */
    o_ptr->artifact = &a_info[artifact->aidx];

    /* Extract the fields */
    copy_artifact_data(o_ptr, artifact);

    /* Success */
    return (TRUE);
}


/*
 * Hack -- Attempt to create one of the "Special Objects"
 *
 * We are only called from "make_object()"
 *
 * Note -- see "make_artifact()" and "apply_magic()"
 */
static bool make_artifact_special(struct player *p, object_type *o_ptr, int level, int depth)
{
    int i;
    object_kind *kind;
    char o_name[NORMAL_WID];
    bool art_ok = TRUE;

    /* Make sure birth no artifacts isn't set */
    if (p && OPT_P(p, birth_no_artifacts)) art_ok = FALSE;

    /* Winners don't generate true artifacts */
    if (p && p->total_winner) art_ok = FALSE;

    /* No artifacts in the town or on special levels */
    if (forbid_special(depth)) return FALSE;

    /* Check the artifact list (just the "specials") */
    if (art_ok)
    {
        for (i = 0; i < z_info->a_max; i++)
        {
            artifact_type *a_ptr = &a_info[i];

            /* Skip "empty" artifacts */
            if (!a_ptr->name) continue;

            /* Skip normal artifacts */
            if (!of_has(a_ptr->flags, OF_SPECIAL_ART)) continue;

            /* Cannot make an artifact twice */
            if (a_ptr->created) continue;

            /* Cannot generate an artifact if disallowed by preservation mode  */
            if (p && (p->art_info[i] > cfg_preserve_artifacts)) continue;

            /* Enforce minimum "depth" (loosely) */
            if (a_ptr->alloc_min > depth)
            {
                /* Get the "out-of-depth factor" */
                int d = (a_ptr->alloc_min - depth) * 2;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Enforce maximum depth (strictly) */
            if (a_ptr->alloc_max < depth) continue;

            /* Artifact "rarity roll" */
            if (!magik(a_ptr->alloc_prob)) continue;

            /* Find the base object */
            kind = lookup_kind(a_ptr->tval, a_ptr->sval);

            /* Make sure the kind was found */
            if (!kind) continue;

            /* Enforce minimum "object" level (loosely) */
            if (kind->level > level)
            {
                /* Acquire the "out-of-depth factor" */
                int d = (kind->level - level) * 5;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Assign the template */
            object_prep(o_ptr, kind, a_ptr->alloc_min, RANDOMISE);

            /* Mark the item as an artifact */
            o_ptr->artifact = a_ptr;
            object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);
            plog_fmt("Special artifact %s created", o_name);

            /* Copy across all the data from the artifact struct */
            copy_artifact_data(o_ptr, a_ptr);

            /* Mark the artifact as "created" */
            a_ptr->created++;
            if (p)
            {
                /* Mark the artifact as "generated" if dungeon is ready */
                if (!ht_zero(&cave_get(depth)->generated))
                    set_artifact_info(p, o_ptr, ARTS_GENERATED);

                /* Otherwise, preserve artifacts from dungeon generation errors */
                else p->art_info[o_ptr->artifact->aidx] += ARTS_CREATED;
            }

            /* Success */
            return TRUE;
        }
    }

    /* An extra chance at being a randart */
    if (cfg_random_artifacts && p)
    {
        for (i = 0; i < z_info->a_max; i++)
        {
            artifact_type *a_ptr;
            s32b randart_seed;

            /* Skip "empty" artifacts */
            if (!a_info[i].name) continue;

            /* Skip normal artifacts */
            if (!of_has(a_info[i].flags, OF_SPECIAL_ART)) continue;

            /* Cannot make a randart twice */
            if (p->randart_created[i]) continue;

            /* Cannot generate a randart if disallowed by preservation mode  */
            if (p->randart_info[i] > cfg_preserve_artifacts) continue;

            /* Piece together a 32-bit random seed */
            randart_seed = randint0(0xFFFF) << 16;
            randart_seed += randint0(0xFFFF);

            /* Attempt to change the object into a random artifact */
            a_ptr = do_randart(randart_seed, &a_info[i]);

            /* Skip "empty" artifacts again */
            if (!a_ptr->name) continue;

            /* Enforce minimum "depth" (loosely) */
            if (a_ptr->alloc_min > depth)
            {
                /* Acquire the "out-of-depth factor" */
                int d = (a_ptr->alloc_min - depth) * 2;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Enforce maximum depth (strictly) */
            if (a_ptr->alloc_max < depth) continue;

            /* Artifact "rarity roll" */
            if (!magik(a_ptr->alloc_prob)) continue;

            /* Find the base object */
            kind = lookup_kind(a_ptr->tval, a_ptr->sval);

            /* Make sure the kind was found */
            if (!kind) continue;

            /* Enforce minimum "object" level (loosely) */
            if (kind->level > level)
            {
                /* Acquire the "out-of-depth factor" */
                int d = (kind->level - level) * 5;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Assign the template */
            object_prep(o_ptr, kind, a_ptr->alloc_min, RANDOMISE);

            /* Mark the item as a random artifact */
            make_randart(p, o_ptr, a_ptr, randart_seed);

            /* Success */
            return TRUE;
        }
    }

    /* Failure */
    return FALSE;
}


/*
 * Attempt to change an object into an artifact.  If the object is already
 * set to be an artifact, use that, or otherwise use a suitable randomly-
 * selected artifact.
 *
 * This routine should only be called by "apply_magic()"
 *
 * Note -- see "make_artifact_special()" and "apply_magic()"
 */
static bool make_artifact(struct player *p, object_type *o_ptr, int depth)
{
    artifact_type *a_ptr;
    int i;
    bool art_ok = TRUE;
    char o_name[NORMAL_WID];

    /* Make sure birth no artifacts isn't set */
    if (p && OPT_P(p, birth_no_artifacts)) art_ok = FALSE;

    /* Winners don't generate true artifacts */
    if (p && p->total_winner) art_ok = FALSE;

    /* Special handling of Grond/Morgoth */
    if (o_ptr->artifact)
    {
        switch (o_ptr->artifact->aidx)
        {
            case ART_GROND:
            case ART_MORGOTH: art_ok = TRUE;
        }
    }

    /* No artifacts in the town or on special levels */
    if (forbid_special(depth)) return FALSE;

    /* Paranoia -- no "plural" artifacts */
    if (o_ptr->number != 1) return FALSE;

    /* Check the artifact list (skip the "specials") */
    if (art_ok)
    {
        for (i = 0; !o_ptr->artifact && (i < z_info->a_max); i++)
        {
            a_ptr = &a_info[i];

            /* Skip "empty" artifacts */
            if (!a_ptr->name) continue;

            /* Skip special artifacts */
            if (of_has(a_ptr->flags, OF_SPECIAL_ART)) continue;

            /* Cannot make an artifact twice */
            if (a_ptr->created) continue;

            /* Cannot generate an artifact if disallowed by preservation mode  */
            if (p && (p->art_info[i] > cfg_preserve_artifacts))
                continue;

            /* Must have the correct fields */
            if (a_ptr->tval != o_ptr->tval) continue;
            if (a_ptr->sval != o_ptr->sval) continue;

            /* Enforce minimum "depth" (loosely) */
            if (a_ptr->alloc_min > depth)
            {
                /* Acquire the "out-of-depth factor" */
                int d = (a_ptr->alloc_min - depth) * 2;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Enforce maximum depth (strictly) */
            if (a_ptr->alloc_max < depth) continue;

            /* We must make the "rarity roll" */
            if (!magik(a_ptr->alloc_prob)) continue;

            /* Mark the item as an artifact */
            o_ptr->artifact = a_ptr;
            object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);
            plog_fmt("Artifact %s created", o_name);
        }

        if (o_ptr->artifact)
        {
            /* Copy across all the data from the artifact struct */
            copy_artifact_data(o_ptr, o_ptr->artifact);

            /* Mark the artifact as "created" */
            o_ptr->artifact->created++;
            if (p)
            {
                /* Mark the artifact as "generated" if dungeon is ready */
                if (!ht_zero(&cave_get(depth)->generated))
                    set_artifact_info(p, o_ptr, ARTS_GENERATED);

                /* Otherwise, preserve artifacts from dungeon generation errors */
                else p->art_info[o_ptr->artifact->aidx] += ARTS_CREATED;
            }

            /* Success */
            return TRUE;
        }
    }

    /* An extra chance at being a randart */
    if (cfg_random_artifacts && p)
    {
        for (i = 0; i < z_info->a_max; i++)
        {
            s32b randart_seed;

            /* Skip "empty" items */
            if (!a_info[i].name) continue;

            /* Skip special artifacts */
            if (of_has(a_info[i].flags, OF_SPECIAL_ART)) continue;

            /* Cannot make a randart twice */
            if (p->randart_created[i]) continue;

            /* Cannot generate a randart if disallowed by preservation mode  */
            if (p->randart_info[i] > cfg_preserve_artifacts) continue;

            /* Must have the correct fields */
            if (a_info[i].tval != o_ptr->tval) continue;
            if (a_info[i].sval != o_ptr->sval) continue;

            /* Piece together a 32-bit random seed */
            randart_seed = randint0(0xFFFF) << 16;
            randart_seed += randint0(0xFFFF);

            /* Attempt to change the object into a random artifact */
            a_ptr = do_randart(randart_seed, &a_info[i]);

            /* Skip "empty" items again */
            if (!a_ptr->name) continue;

            /* Enforce minimum "depth" (loosely) */
            if (a_ptr->alloc_min > depth)
            {
                /* Acquire the "out-of-depth factor" */
                int d = (a_ptr->alloc_min - depth) * 2;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Enforce maximum depth (strictly) */
            if (a_ptr->alloc_max < depth) continue;

            /* We must make the "rarity roll" */
            if (!magik(a_ptr->alloc_prob)) continue;

            /* Mark the item as a random artifact */
            make_randart(p, o_ptr, a_ptr, randart_seed);

            /* Success */
            return TRUE;
        }
    }

    /* Failure */
    return FALSE;
}


/*** Apply magic to an item ***/


/*
 * Apply magic to a weapon
 */
static void apply_magic_weapon(object_type *o_ptr, int level, int power)
{
    if (power <= 0) return;

    o_ptr->to_h += randint1(5) + m_bonus(5, level);
    o_ptr->to_d += randint1(5) + m_bonus(5, level);

    if (power > 1)
    {
        o_ptr->to_h += m_bonus(10, level);
        o_ptr->to_d += m_bonus(10, level);

        if (melee_p(o_ptr) || obj_is_ammo(NULL, o_ptr))
        {
            /* Super-charge the damage dice */
            while (one_in_(10L * o_ptr->dd * o_ptr->ds)) o_ptr->dd++;

            /* But not too high */
            if (melee_p(o_ptr) && (o_ptr->dd > MAX_WEAPON_DICE))
                o_ptr->dd = MAX_WEAPON_DICE;
            else if (obj_is_ammo(NULL, o_ptr) && (o_ptr->dd > MAX_AMMO_DICE))
                o_ptr->dd = MAX_AMMO_DICE;
        }
    }
}


/*
 * Apply magic to armour
 */
static void apply_magic_armour(object_type *o_ptr, int level, int power)
{
    if (power <= 0) return;

    o_ptr->to_a += randint1(5) + m_bonus(5, level);
    if (power > 1) o_ptr->to_a += m_bonus(10, level);

    /* Bad */
    if (o_ptr->to_a < 0)
    {
        size_t i;

        /* Hack -- Reverse base bonuses */
        for (i = 0; i < o_ptr->num_pvals; i++)
        {
            if (o_ptr->pval[i] > 0) o_ptr->pval[i] = 0 - o_ptr->pval[i];
        }
    }
}


/*
 * Wipe an object clean and make it a standard object of the specified kind.
 */
void object_prep(object_type *o_ptr, struct object_kind *k, int lev, aspect rand_aspect)
{
    int i;

    /* Clean slate */
    object_wipe(o_ptr);

    /* Assign the kind and copy across data */
    o_ptr->kind = k;
    o_ptr->tval = k->tval;
    o_ptr->sval = k->sval;
    o_ptr->ac = k->ac;
    o_ptr->dd = k->dd;
    o_ptr->ds = k->ds;
    o_ptr->weight = k->weight;
    if (k->base) of_copy(o_ptr->flags, k->base->flags);
    of_union(o_ptr->flags, k->flags);

    o_ptr->effect = k->effect;
    o_ptr->time.base = k->time.base;
    o_ptr->time.dice = k->time.dice;
    o_ptr->time.sides = k->time.sides;

    /* Default number */
    o_ptr->number = 1;

    /* Default "pvals" */
    for (i = 0; i < MAX_PVALS; i++) of_wipe(o_ptr->pval_flags[i]);
    for (i = 0; i < k->num_pvals; i++)
    {
        o_ptr->pval[i] = randcalc(k->pval[i], lev, rand_aspect);
        of_copy(o_ptr->pval_flags[i], k->pval_flags[i]);
    }
    o_ptr->num_pvals = k->num_pvals;
    object_dedup_pvals(o_ptr);

    /* Hack -- Amulets of speed can't give very much, and are rarely +5 */
    if ((o_ptr->tval == TV_AMULET) && (o_ptr->sval == SV_AMULET_SPEED))
    {
        i = which_pval(o_ptr, OF_SPEED);
        o_ptr->pval[i] = randint1(o_ptr->pval[i]);
    }

    /* Hack -- Rings of Polymorphing get a random race */
    if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_POLYMORPHING))
    {
        int r_idx, rarity;
        monster_race *r_ptr;

        /* Get a random race - try to roughly match monster power */
        /* Skip uniques and monsters that can't be generated */
        do
        {
            r_idx = randint1(z_info->r_max - 1);
            r_ptr = &r_info[r_idx];
            rarity = (int)sqrt((double)r_ptr->power);
        }
        while (randint0(rarity) || !r_ptr->name || rf_has(r_ptr->flags, RF_UNIQUE) ||
            (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters) ||
            (rf_has(r_ptr->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters));

        i = which_pval(o_ptr, OF_POLY_RACE);
        o_ptr->pval[i] = r_idx;
    }

    /* Assign charges (wands/staves only) */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
        o_ptr->pval[DEFAULT_PVAL] = randcalc(k->charge, lev, rand_aspect);

    /* Assign flagless pval for food or oil */
    if ((o_ptr->tval == TV_FOOD) || (o_ptr->tval == TV_POTION) || (o_ptr->tval == TV_FLASK))
        o_ptr->pval[DEFAULT_PVAL] = randcalc(k->pval[DEFAULT_PVAL], lev, rand_aspect);

    /* Default fuel for lamps */
    if (o_ptr->tval == TV_LIGHT) fuel_default(o_ptr);

    /* Default magic */
    o_ptr->to_h = randcalc(k->to_h, lev, rand_aspect);
    o_ptr->to_d = randcalc(k->to_d, lev, rand_aspect);
    o_ptr->to_a = randcalc(k->to_a, lev, rand_aspect);
}


/*
 * Applying magic to an object, which includes creating ego-items, and applying
 * random bonuses.
 *
 * The `good` argument forces the item to be at least `good`, and the `great`
 * argument does likewise.  Setting `allow_artifacts` to TRUE allows artifacts
 * to be created here.
 *
 * If `good` or `great` are not set, then the `lev` argument controls the
 * quality of item.
 *
 * Returns 0 if a normal object, 1 if a good object, 2 if an ego item, 3 if an
 * artifact.
 *
 * PWMAngband: returns -1 if invalid (not a "good" drop)
 */
s16b apply_magic(struct player *p, int depth, object_type *o_ptr, int lev, bool allow_artifacts,
    bool good, bool great)
{
    int i;
    s16b power = 0;

    /* Chance of being `good` and `great` */
    int good_chance = MIN(2 * lev + 5, 100);
    int great_chance = MIN(40, (lev * 3) / 4);

    /* Magic ammo are always +0 +0 (not a "good" drop) */
    if (magic_ammo_p(o_ptr)) return ((good || great)? -1: 0);

    if (lev >= MAX_DEPTH) lev = MAX_DEPTH - 1;

    /* Roll for "good" */
    if (good || magik(good_chance))
    {
        /* Assume "good" */
        power = 1;

        /* Roll for "great" */
        if (great || magik(great_chance)) power = 2;
    }

    /* Roll for artifact creation */
    if (allow_artifacts)
    {
        int rolls = 0;

        /* Get one roll if excellent */
        if (power >= 2) rolls = 1;

        /* Get four rolls if forced great */
        if (great) rolls = 4;

        /* Roll for artifacts if allowed */
        for (i = 0; i < rolls; i++)
        {
            if (make_artifact(p, o_ptr, depth)) return 3;
        }
    }

    /* Try to make an ego item */
    if (power == 2) make_ego_item(o_ptr, lev);

    /* Apply magic */
    switch (o_ptr->tval)
    {
        case TV_DIGGING:
        case TV_HORN:
        case TV_MSTAFF:
        {
            /* Not a "great" drop */
            if (great && !o_ptr->ego) return -1;

            /* Not a "good" drop */
            if (good && !o_ptr->ego) return -1;

            break;
        }

        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOW:
        case TV_ROCK:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            /* Not a "great" drop */
            if (great && !o_ptr->ego) return -1;

            apply_magic_weapon(o_ptr, lev, power);

            /* Not a "good" drop */
            if (good && !o_ptr->ego && ((o_ptr->to_h <= 0) || (o_ptr->to_d <= 0))) return -1;

            break;
        }

        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_HELM:
        case TV_CROWN:
        case TV_CLOAK:
        case TV_GLOVES:
        case TV_BOOTS:
        {
            /* Not a "great" drop */
            if (great && !o_ptr->ego) return -1;

            apply_magic_armour(o_ptr, lev, power);

            /* Not a "good" drop */
            if (good && !o_ptr->ego && (o_ptr->to_a <= 0)) return -1;

            break;
        }

        case TV_RING:
        {
            /* Analyze */
            switch (o_ptr->sval)
            {
                case SV_RING_SPEED:
                {
                    /* Super-charge the ring */
                    while (one_in_(2)) o_ptr->pval[which_pval(o_ptr, OF_SPEED)]++;

                    break;
                }
            }

            break;
        }

        case TV_AMULET:
        {
            /* Analyze */
            switch (o_ptr->sval)
            {
                case SV_AMULET_ESP:
                {
                    /* Add a random ESP power */
                    do_powers(o_ptr, OBJECT_XTRA_TYPE_ESP);

                    break;
                }

                case SV_AMULET_TERKEN:
                {
                    /* Add a random ability */
                    do_powers(o_ptr, OBJECT_XTRA_TYPE_POWER);

                    break;
                }
            }

            break;
        }

        case TV_LIGHT:
        {
            /* Not a "great" drop */
            if (great && !o_ptr->ego) return -1;

            /* Not a "good" drop */
            if (good && !o_ptr->ego) return -1;

            break;
        }

        case TV_CHEST:
        {
            /* Hack -- skip ruined chests */
            if (o_ptr->kind->level <= 0) break;

            /* Hack -- pick a "difficulty" */
            o_ptr->pval[DEFAULT_PVAL] = randint1(o_ptr->kind->level);

            /* Never exceed "difficulty" of 55 to 59 */
            if (o_ptr->pval[DEFAULT_PVAL] > 55)
                o_ptr->pval[DEFAULT_PVAL] = 55 + randint0(5);

            break;
        }
    }

    return power;
}


/*** Generate a random object ***/


/*
 * Hack -- determine if a template is "good"
 */
bool kind_is_good(const object_kind *kind)
{
    /* Analyze the item type */
    switch (kind->tval)
    {
        /* Armor */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN: return (TRUE);

        /* Weapons */
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_MSTAFF: return (TRUE);

        /* Tools */
        case TV_DIGGING:
        case TV_HORN: return (TRUE);

        /* Ammo */
        case TV_ROCK:
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            /* Magic ammo never get good (they cannot become ego/art anyway...) */
            return (kind->sval != SV_AMMO_MAGIC);
        }

        /* Light sources */
        case TV_LIGHT: return (TRUE);
    }

    /* Other item types */
    return kind_is_good_other(kind);
}


static u32b obj_total[MAX_DEPTH];
static byte *obj_alloc;


static u32b obj_total_great[MAX_DEPTH];
static byte *obj_alloc_great;


/* Don't worry about probabilities for anything past dlev100 */
#define MAX_O_DEPTH 100


/*
 * Free object allocation info
 */
void free_obj_alloc(void)
{
    mem_free(obj_alloc);
    mem_free(obj_alloc_great);
}


/*
 * Using k_info[], init rarity data for the entire dungeon
 */
bool init_obj_alloc(void)
{
    int k_max = z_info->k_max;
    int item, lev;

    /* Free obj_allocs if allocated */
    free_obj_alloc();

    /* Allocate and wipe */
    obj_alloc = C_ZNEW((MAX_O_DEPTH + 1) * k_max, byte);
    obj_alloc_great = C_ZNEW((MAX_O_DEPTH + 1) * k_max, byte);

    /* Wipe the totals */
    C_WIPE(obj_total, MAX_O_DEPTH + 1, u32b);
    C_WIPE(obj_total_great, MAX_O_DEPTH + 1, u32b);

    /* Init allocation data */
    for (item = 1; item < k_max; item++)
    {
        const object_kind *kind = &k_info[item];
        int min = kind->alloc_min;
        int max = kind->alloc_max;

        /* If an item doesn't have a rarity, move on */
        if (!kind->alloc_prob) continue;

        /* Go through all the dungeon levels */
        for (lev = 0; lev <= MAX_O_DEPTH; lev++)
        {
            int rarity = kind->alloc_prob;

            /* Save the probability in the standard table */
            if ((lev < min) || (lev > max)) rarity = 0;
            obj_total[lev] += rarity;
            obj_alloc[(lev * k_max) + item] = rarity;

            /* Save the probability in the "great" table if relevant */
            if (!kind_is_good(kind)) rarity = 0;
            obj_total_great[lev] += rarity;
            obj_alloc_great[(lev * k_max) + item] = rarity;
        }
    }

    return TRUE;
}


/*
 * Choose an object kind given a dungeon level to choose it for
 */
object_kind *get_obj_num(int level, bool good)
{
    size_t ind, item;
    u32b value;

    /* Occasional level boost */
    if ((level > 0) && one_in_(GREAT_OBJ))
    {
        /* What a bizarre calculation */
        level = 1 + (level * MAX_O_DEPTH / randint1(MAX_O_DEPTH));
    }

    /* Paranoia */
    level = MIN(level, MAX_O_DEPTH);
    level = MAX(level, 0);

    /* This is the base index into obj_alloc for this dlev */
    ind = level * z_info->k_max;

    /* Pick an object */
    if (!good)
    {
        value = randint0(obj_total[level]);
        for (item = 1; item < z_info->k_max; item++)
        {
            /* Found it */
            if (value < obj_alloc[ind + item]) break;

            /* Decrement */
            value -= obj_alloc[ind + item];
        }
    }
    else
    {
        value = randint0(obj_total_great[level]);
        for (item = 1; item < z_info->k_max; item++)
        {
            /* Found it */
            if (value < obj_alloc_great[ind + item]) break;

            /* Decrement */
            value -= obj_alloc_great[ind + item];
        }
    }

    /* Return the item index */
    return &k_info[item];
}


/*
 * Attempt to make an object (normal or good/great)
 *
 * This routine plays nasty games to generate the "special artifacts".
 * We assume that the given object has been "wiped". You can optionally
 * receive the object's value in value if you pass a non-null pointer.
 *
 * Returns whether or not creation worked.
 */
bool make_object(struct player *p, struct cave *c, object_type *j_ptr, int lev, bool good,
    bool great, s32b *value)
{
    int base;
    object_kind *kind;
    int i;
    int tries = 1;
    bool ok = FALSE;

    /* Try to make a special artifact */
    if (one_in_(good? 10: 1000))
    {
        if (make_artifact_special(p, j_ptr, lev, c->depth))
        {
            if (value) *value = object_value_real(p, j_ptr, 1);
            return TRUE;
        }

        /* If we failed to make an artifact, the player gets a good item */
        good = TRUE;
    }

    /* Base level for the object */
    base = (good? (lev + 10): lev);

    /* Try harder to generate a "good" object */
    if (good || great) tries = 3;

    for (i = 1; i <= tries; i++)
    {
        s16b res;

        /* Get the object, prep it and apply magic */
        kind = get_obj_num(base, good || great);
        if (!kind) return FALSE;
        object_prep(j_ptr, kind, lev, RANDOMISE);
        res = apply_magic(p, c->depth, j_ptr, lev, TRUE, good, great);

        /* Reroll "good" objects of incorrect kind (diggers, light sources...) */
        /* Reroll "great" objects when make_ego_item fails */
        if (res == -1)
        {
            /* Handle failure */
            object_wipe(j_ptr);
            continue;
        }

        /* We have a valid object */
        ok = TRUE;
        break;
    }

    /* Handle failure */
    if (!ok) return FALSE;

    /* Generate multiple items */
    if (!j_ptr->artifact)
    {
        if ((j_ptr->kind->gen_mult_prob >= 100) || (j_ptr->kind->gen_mult_prob >= randint1(100)))
            j_ptr->number = randcalc(j_ptr->kind->stack_size, lev, RANDOMISE);
    }

    if (j_ptr->number >= MAX_STACK_SIZE) j_ptr->number = MAX_STACK_SIZE - 1;

    /* Return value, increased for uncursed out-of-depth objects */
    if (value) *value = object_value_real(p, j_ptr, j_ptr->number);

    if (!cursed_p(j_ptr->flags) && (j_ptr->kind->alloc_min > object_level(c->depth)))
    {
        if (value) *value += (j_ptr->kind->alloc_min - object_level(c->depth)) * (*value / 5);
    }

    return TRUE;
}


/*** Make a gold item ***/


/* The largest possible average gold drop at max depth with biggest spread */
#define MAX_GOLD_DROP   (3 * MAX_DEPTH + 30)


/*
 * Make a money object
 */
void make_gold(struct player *p, object_type *j_ptr, int lev, int coin_type)
{
    int sval;

    /* This average is 20 at dlev0, 105 at dlev40, 220 at dlev100. */
    /* Follows the formula: y=2x+20 */
    s32b avg = 2 * lev + 20;
    s32b spread = lev + 10;
    s32b value = rand_spread(avg, spread);

    /* Increase the range to infinite, moving the average to 110% */
    while (one_in_(100) && (value * 10 <= MAX_SHORT)) value *= 10;

    /* Pick a treasure variety scaled by level, or force a type */
    if (coin_type != SV_GOLD_ANY)
        sval = coin_type;
    else
        sval = (((value * 100) / MAX_GOLD_DROP) * SV_GOLD_MAX) / 100;

    /* Do not create illegal treasure types */
    if (sval >= SV_GOLD_MAX) sval = SV_GOLD_MAX - 1;

    /* Prepare a gold object */
    object_prep(j_ptr, lookup_kind(TV_GOLD, sval), lev, RANDOMISE);

    /* If we're playing with no_selling, increase the value */
    if (p && OPT_P(p, birth_no_selling) && (p->depth > 0))
        value = value * MIN(5, p->depth);

    j_ptr->pval[DEFAULT_PVAL] = value;
}


void make_randart(struct player *p, object_type *o_ptr, artifact_type *a_ptr, s32b randart_seed)
{
    char o_name[NORMAL_WID];

    /* Mark the item as a random artifact */
    o_ptr->artifact = &a_info[a_ptr->aidx];
    o_ptr->randart_seed = randart_seed;
    object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);
    plog_fmt("Random artifact %s created", o_name);

    /* Copy across all the data from the artifact struct */
    copy_artifact_data(o_ptr, a_ptr);

    /* Mark the randart as "created" */
    p->randart_created[o_ptr->artifact->aidx] = 1;
    o_ptr->creator = p->id;

    /* Mark the artifact as "generated" if dungeon is ready */
    if (!ht_zero(&cave_get(p->depth)->generated)) set_artifact_info(p, o_ptr, ARTS_GENERATED);

    /* Otherwise, preserve artifacts from dungeon generation errors */
    else p->randart_info[o_ptr->artifact->aidx] += ARTS_CREATED;
}


void fuel_default(object_type *o_ptr)
{
    /* Default fuel levels */
    if (o_ptr->sval == SV_LIGHT_TORCH)
        o_ptr->timeout = DEFAULT_TORCH;
    else if (is_lamp(o_ptr))
        o_ptr->timeout = DEFAULT_LAMP;
}
