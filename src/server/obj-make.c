/*
 * File: obj-make.c
 * Purpose: Object generation functions
 *
 * Copyright (c) 1987-2007 Angband contributors
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


/** Arrays holding an index of objects to generate for a given level */
static u32b *obj_total;
static byte *obj_alloc;


static u32b *obj_total_great;
static byte *obj_alloc_great;


static s16b alloc_ego_size = 0;
static alloc_entry *alloc_ego_table;


struct money
{
    char *name;
    int type;
};


static struct money *money_type;
static int num_money_types;


static byte get_artifact_rarity(s32b value, struct object_kind *kind)
{
    s32b alloc = 50000000 / (value * (kind->alloc_prob? kind->alloc_prob: 100));

    if (alloc > 990) return 99;
    if (alloc < 10) return 1;

    return (byte)(((alloc - (alloc / 10) * 10) >= 5)? alloc / 10 + 1: alloc / 10);
}


static void init_obj_make(void)
{
    int i, item, lev;
    int k_max = z_info->k_max;
    alloc_entry *table;
    struct ego_item *ego;
    s16b *num;
    s16b *aux;
    int *money_svals;

    /*** Initialize object allocation info ***/

    /* Allocate and wipe */
    obj_alloc = mem_zalloc((z_info->max_obj_depth + 1) * k_max * sizeof(byte));
    obj_alloc_great = mem_zalloc((z_info->max_obj_depth + 1) * k_max * sizeof(byte));
    obj_total = mem_zalloc(z_info->max_depth * sizeof(u32b));
    obj_total_great = mem_zalloc(z_info->max_depth * sizeof(u32b));

    /* Init allocation data */
    for (item = 1; item < k_max; item++)
    {
        const struct object_kind *kind = &k_info[item];
        int min = kind->alloc_min;
        int max = kind->alloc_max;

        /* If an item doesn't have a rarity, move on */
        if (!kind->alloc_prob) continue;

        /* Go through all the dungeon levels */
        for (lev = 0; lev <= z_info->max_obj_depth; lev++)
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

    /* Hack -- automatically compute art rarities for PWMAngband's artifacts */
    for (i = ART_MAX_STATIC + 1; i < z_info->a_max; i++)
    {
        struct artifact *art = &a_info[i];
        struct object *fake = object_new();
        s32b value;

        /* Ignore "empty" artifacts */
        if (!art->tval) continue;

        /* Create a "forged" artifact */
        if (!make_fake_artifact(fake, art)) continue;

        /* Get the value */
        value = object_value_real(NULL, fake, 1);

        /* Allocation probability */
        art->alloc_prob = get_artifact_rarity(value, fake->kind);

        object_delete(&fake);
    }

    /*** Initialize ego-item allocation info ***/

    num = mem_zalloc(z_info->max_depth * sizeof(s16b));
    aux = mem_zalloc(z_info->max_depth * sizeof(s16b));

    /* Scan the ego items */
    for (i = 1; i < z_info->e_max; i++)
    {
        /* Get the i'th ego item */
        ego = &e_info[i];

        /* Legal items */
        if (ego->rarity)
        {
            /* Count the entries */
            alloc_ego_size++;

            /* Group by level */
            num[ego->level]++;
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < z_info->max_depth; i++)
    {
        /* Group by level */
        num[i] += num[i - 1];
    }

    /* Allocate the alloc_ego_table */
    alloc_ego_table = mem_zalloc(alloc_ego_size * sizeof(alloc_entry));

    /* Access the table entry */
    table = alloc_ego_table;

    /* Scan the ego items */
    for (i = 1; i < z_info->e_max; i++)
    {
        /* Get the i'th ego item */
        ego = &e_info[i];

        /* Count valid pairs */
        if (ego->rarity)
        {
            int p, x, y, z;

            /* Extract the base level */
            x = ego->level;

            /* Extract the base probability */
            p = (100 / ego->rarity);

            /* Skip entries preceding our locale */
            y = ((x > 0)? num[x - 1]: 0);

            /* Skip previous entries at this locale */
            z = y + aux[x];

            /* Load the entry */
            table[z].index = i;
            table[z].level = x;
            table[z].prob1 = p;
            table[z].prob2 = p;
            table[z].prob3 = p;

            /* Another entry complete for this locale */
            aux[x]++;
        }
    }

    mem_free(aux);
    mem_free(num);

    /*** Initialize money info ***/

    /* Count the money types and make a list */
    num_money_types = tval_sval_count("gold");
    money_type = mem_zalloc(num_money_types * sizeof(struct money));
    money_svals = mem_zalloc(num_money_types * sizeof(struct money));
    tval_sval_list("gold", money_svals, num_money_types);

    /* List the money types */
    for (i = 0; i < num_money_types; i++)
    {
        struct object_kind *kind = lookup_kind(TV_GOLD, money_svals[i]);

        money_type[i].name = string_make(kind->name);
        money_type[i].type = money_svals[i];
    }
    mem_free(money_svals);
}


static void cleanup_obj_make(void)
{
    int i;

    for (i = 0; i < num_money_types; i++) string_free(money_type[i].name);
    mem_free(money_type);
    mem_free(alloc_ego_table);
    mem_free(obj_total_great);
    mem_free(obj_total);
    mem_free(obj_alloc_great);
    mem_free(obj_alloc);
}


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

    create_mask(newf, false, OFT_PROT, OFT_MISC, OFT_MAX);

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
 * Get a random new high resist on an item
 */
static int random_high_resist(struct object *obj, int *resist)
{
    int i, r, count = 0;

    /* Count the available high resists */
    for (i = ELEM_HIGH_MIN; i <= ELEM_HIGH_MAX; i++)
    {
        if (obj->el_info[i].res_level == 0) count++;
    }

    if (count == 0) return false;

    /* Pick one */
    r = randint0(count);

    /* Find the one we picked */
    for (i = ELEM_HIGH_MIN; i <= ELEM_HIGH_MAX; i++)
    {
        if (obj->el_info[i].res_level != 0) continue;
        if (r == 0)
        {
            *resist = i;
            return true;
        }
        r--;
    }

    return false;
}


/*
 * Generate random powers
 */
static void do_powers(struct object *obj, bitflag kind_flags[KF_SIZE])
{
    int resist = 0;
    bitflag newf[OF_SIZE];

    /* Extra powers */
    if (kf_has(kind_flags, KF_RAND_SUSTAIN))
    {
        create_mask(newf, false, OFT_SUST, OFT_MAX);
        of_on(obj->flags, get_new_attr(obj->flags, newf));
    }
    if (kf_has(kind_flags, KF_RAND_HI_RES))
    {
        /* Get a high resist if available, mark it as random */
        if (random_high_resist(obj, &resist))
        {
            obj->el_info[resist].res_level = 1;
            obj->el_info[resist].flags |= EL_INFO_RANDOM;
        }
    }
    if (kf_has(kind_flags, KF_RAND_POWER))
    {
        int esp_flag = get_new_esp(obj->flags);
        int power_flag = get_new_power(obj->flags, esp_flag);

        if ((power_flag == OF_ESP_POWER) && esp_flag)
            of_on(obj->flags, esp_flag);
        else
            of_on(obj->flags, power_flag);
    }
    if (kf_has(kind_flags, KF_RAND_ESP))
    {
        int esp_flag = get_new_esp(obj->flags);

        if (esp_flag)
            of_on(obj->flags, esp_flag);
    }
}


static int get_power_flags(const struct object *obj, bitflag flags[OF_SIZE])
{
    bitflag *kind_flags;

    of_wipe(flags);

    /* Set ego extra powers */
    if (obj->ego) kind_flags = obj->ego->kind_flags;

    /* Set extra powers */
    else kind_flags = obj->kind->kind_flags;

    /* Get power flags */
    if (kf_has(kind_flags, KF_RAND_SUSTAIN))
        create_mask(flags, false, OFT_SUST, OFT_MAX);
    else if (kf_has(kind_flags, KF_RAND_POWER))
    {
        create_mask(flags, false, OFT_PROT, OFT_MISC, OFT_ESP, OFT_MAX);
        of_off(flags, OF_ESP_POWER);
    }
    if (kf_has(kind_flags, KF_RAND_ESP))
        create_mask(flags, false, OFT_ESP, OFT_MAX);
    
    /* Get resists */
    if (kf_has(kind_flags, KF_RAND_HI_RES))
        return ELEM_POIS;

    return -1;
}


void init_powers(const struct object *obj, int *power, int *resist)
{
    bitflag flags[OF_SIZE];

    *resist = get_power_flags(obj, flags);
    *power = of_next(flags, FLAG_START);
}


static void dec_power_aux(bitflag flags[OF_SIZE], int *power)
{
    int flag = of_next(flags, FLAG_START), prevflag;

    /* Item must have an extra power */
    if (*power == FLAG_END) return;

    /* Min bound */
    if (*power == flag) return;

    /* Decrease extra power */
    do
    {
        prevflag = flag;
        flag = of_next(flags, prevflag + 1);
    }
    while (*power != flag);
    *power = prevflag;
}


void dec_power(const struct object *obj, int *power)
{
    bitflag flags[OF_SIZE];

    get_power_flags(obj, flags);
    dec_power_aux(flags, power);
}


void dec_resist(const struct object *obj, int *resist)
{
    /* Item must have an extra resist */
    if (*resist == -1) return;

    /* Min bound */
    if (*resist == ELEM_POIS) return;

    /* Decrease extra resist */
    (*resist)--;
}


static void inc_power_aux(bitflag flags[OF_SIZE], int *power)
{
    int flag;

    /* Item must have an extra power */
    if (*power == FLAG_END) return;

    /* Max bound */
    flag = of_next(flags, *power + 1);
    if (flag == FLAG_END) return;

    /* Increase extra power */
    *power = flag;
}


void inc_power(const struct object *obj, int *power)
{
    bitflag flags[OF_SIZE];

    get_power_flags(obj, flags);
    inc_power_aux(flags, power);
}


void inc_resist(const struct object *obj, int *resist)
{
    /* Item must have an extra resist */
    if (*resist == -1) return;

    /* Max bound */
    if (*resist == ELEM_DISEN) return;

    /* Increase extra resist */
    (*resist)++;
}


void do_fixed_powers(struct object *obj, int power, int resist)
{
    if (power != FLAG_END) of_on(obj->flags, power);
    if (resist != -1) obj->el_info[resist].res_level = 1;
}


void undo_fixed_powers(struct object *obj, int power, int resist)
{
    if (power != FLAG_END) of_off(obj->flags, power);
    if (resist != -1) obj->el_info[resist].res_level = 0;
}


/*
 * Describes a flag-name pair
 */
struct flag_type
{
    int flag;
    const char *name;
};


/* Random powers */
static const struct flag_type power_flags[] =
{
    /* OFT_SUST */
    {OF_SUST_STR, "Sust STR"},
    {OF_SUST_INT, "Sust INT"},
    {OF_SUST_WIS, "Sust WIS"},
    {OF_SUST_DEX, "Sust DEX"},
    {OF_SUST_CON, "Sust CON"},

    /* OFT_PROT */
    {OF_PROT_FEAR, "Fear"},
    {OF_PROT_BLIND, "Blind"},
    {OF_PROT_CONF, "Confu"},
    {OF_PROT_STUN, "Stun"},

    /* OFT_MISC */
    {OF_SLOW_DIGEST, "Slow Digest"},
    {OF_FEATHER, "Feather"},
    {OF_REGEN, "Regen"},
    {OF_SEE_INVIS, "See Invis"},
    {OF_FREE_ACT, "Free Act"},
    {OF_HOLD_LIFE, "Hold Life"},

    /* OFT_ESP */
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


/* Random resists */
static const struct flag_type resist_flags[] =
{
    {ELEM_POIS, "Pois"},
    {ELEM_LIGHT, "Light"},
    {ELEM_DARK, "Dark"},
    {ELEM_SOUND, "Sound"},
    {ELEM_SHARD, "Shard"},
    {ELEM_NEXUS, "Nexus"},
    {ELEM_NETHER, "Nethr"},
    {ELEM_CHAOS, "Chaos"},
    {ELEM_DISEN, "Disen"}
};


static const char *get_flag_desc(const struct flag_type flags[], size_t size, int flag)
{
    size_t i;

    for (i = 0; i < size; i++)
    {
        if (flags[i].flag == flag) return flags[i].name;
    }

    return "";
}


void get_power_descs(int power, int resist, char *buf, int len)
{
    if (power != FLAG_END)
    {
        my_strcpy(buf, get_flag_desc(power_flags, N_ELEMENTS(power_flags), power), len);
        if (resist != -1)
        {
            my_strcat(buf, "/", len);
            my_strcat(buf, get_flag_desc(resist_flags, N_ELEMENTS(resist_flags), resist), len);
        }
    }
    else if (resist != -1)
        my_strcpy(buf, get_flag_desc(resist_flags, N_ELEMENTS(resist_flags), resist), len);
    else
        my_strcpy(buf, "Regular", len);
}


/*
 * Select an ego-item that fits the object's tval and sval.
 */
static struct ego_item *ego_find_random(struct object *obj, int level)
{
    int i, ood_chance;
    long total = 0L;
    alloc_entry *table = alloc_ego_table;
    struct ego_item *ego;
    struct ego_poss_item *poss;

    /* Go through all possible ego items and find ones which fit this item */
    for (i = 0; i < alloc_ego_size; i++)
    {
        /* Reset any previous probability of this type being picked */
        table[i].prob3 = 0;

        if (level < table[i].level) continue;

        /* Access the ego item */
        ego = &e_info[table[i].index];

        /* Enforce maximum */
        if (level > ego->alloc_max) continue;

        /* Roll for Out of Depth (ood) */
        if (level < ego->alloc_min)
        {
            ood_chance = MAX(2, (ego->alloc_min - level) / 3);
            if (!one_in_(ood_chance)) continue;
        }

        /* XXX Ignore cursed items for now */
        if (cursed_p(ego->flags)) continue;

        /* Test if this is a legal ego item type for this object */
        for (poss = ego->poss_items; poss; poss = poss->next)
        {
            if (poss->kidx == obj->kind->kidx)
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
static void ego_apply_minima(struct object *obj)
{
    int i;

    if (!obj->ego) return;

    if ((obj->ego->min_to_h != NO_MINIMUM) && (obj->to_h < obj->ego->min_to_h))
        obj->to_h = obj->ego->min_to_h;
    if ((obj->ego->min_to_d != NO_MINIMUM) && (obj->to_d < obj->ego->min_to_d))
        obj->to_d = obj->ego->min_to_d;
    if ((obj->ego->min_to_a != NO_MINIMUM) && (obj->to_a < obj->ego->min_to_a))
        obj->to_a = obj->ego->min_to_a;

    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (obj->modifiers[i] && (obj->ego->min_modifiers[i] != NO_MINIMUM) &&
            (obj->modifiers[i] < obj->ego->min_modifiers[i]))
        {
            obj->modifiers[i] = obj->ego->min_modifiers[i];
        }
    }
}


/*
 * Apply generation magic to an ego-item.
 *
 * Returns the amount to increase the level rating by
 */
void ego_apply_magic(struct object *obj, int level)
{
    int i, x;

    /* Apply extra ego bonuses */
    obj->to_h += randcalc(obj->ego->to_h, level, RANDOMISE);
    obj->to_d += randcalc(obj->ego->to_d, level, RANDOMISE);
    obj->to_a += randcalc(obj->ego->to_a, level, RANDOMISE);

    /* Apply modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        x = randcalc(obj->ego->modifiers[i], level, RANDOMISE);
        obj->modifiers[i] += x;
    }

    /* Apply flags */
    of_union(obj->flags, obj->ego->flags);

    /* Add slays and brands */
    copy_slay(&obj->slays, obj->ego->slays);
    copy_brand(&obj->brands, obj->ego->brands);

    /* Add resists */
    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Take the larger of ego and base object resist levels */
        obj->el_info[i].res_level =
            MAX(obj->ego->el_info[i].res_level, obj->el_info[i].res_level);

        /* Union of flags so as to know when ignoring is notable */
        obj->el_info[i].flags |= obj->ego->el_info[i].flags;
    }

    /* Apply minima */
    ego_apply_minima(obj);

    /* Add activation (ego activation will trump object activation, when there are any) */
    if (obj->ego->activation)
    {
        obj->activation = obj->ego->activation;
        obj->time = obj->ego->time;
    }
}


/*
 * Try to find an ego-item for an object, setting obj->ego if successful and
 * applying various bonuses.
 */
static void make_ego_item(struct object *obj, int level)
{
    /* Cannot further improve artifacts or ego items */
    if (obj->artifact || obj->ego) return;

    /* Occasionally boost the generation level of an item */
    if ((level > 0) && one_in_(z_info->great_ego))
    {
        /* The bizarre calculation again */
        level = 1 + (level * z_info->max_depth / randint1(z_info->max_depth));

        /* Ensure valid allocation level */
        if (level >= z_info->max_depth) level = z_info->max_depth - 1;
    }

    /* Try to get a legal ego type for this item */
    obj->ego = ego_find_random(obj, level);

    /* Actually apply the ego template to the item */
    if (obj->ego)
    {
        /* Extra powers */
        do_powers(obj, obj->ego->kind_flags);

        ego_apply_magic(obj, level);
    }
}


/*** Make an artifact ***/


/*
 * Copy artifact data to a normal object, and set various slightly hacky
 * globals.
 */
void copy_artifact_data(struct object *obj, const struct artifact *art)
{
    int i;

    /* Extract the data */
    for (i = 0; i < OBJ_MOD_MAX; i++)
        obj->modifiers[i] = art->modifiers[i];
    obj->ac = art->ac;
    obj->dd = art->dd;
    obj->ds = art->ds;
    obj->to_a = art->to_a;
    obj->to_h = art->to_h;
    obj->to_d = art->to_d;
    obj->weight = art->weight;
    obj->activation = art->activation;
    if (art->time.base != 0) obj->time = art->time;
    of_union(obj->flags, art->flags);
    copy_slay(&obj->slays, art->slays);
    copy_brand(&obj->brands, art->brands);
    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Take the larger of artifact and base object resist levels */
        obj->el_info[i].res_level =
            MAX(art->el_info[i].res_level, obj->el_info[i].res_level);

        /* Union of flags so as to know when ignoring is notable */
        obj->el_info[i].flags |= art->el_info[i].flags;
    }
}


/*
 * Create a fake artifact directly from a blank object
 *
 * This function is used for describing artifacts, and for creating them for
 * debugging.
 *
 * Since this is now in no way marked as fake, we must make sure this function
 * is never used to create an actual game object
 */
bool make_fake_artifact(struct object *obj, struct artifact *artifact)
{
    struct object_kind *kind;

    /* Don't bother with empty artifacts */
    if (!artifact->tval) return false;

    /* Get the "kind" index */
    kind = lookup_kind(artifact->tval, artifact->sval);
    if (!kind) return false;

    /* Create the artifact */
    object_prep(NULL, obj, kind, 0, MAXIMISE);

    /* Save the name */
    obj->artifact = &a_info[artifact->aidx];

    /* Extract the fields */
    copy_artifact_data(obj, artifact);

    /* Learn brands and slays */
    object_know_brands_and_slays(obj);

    /* Success */
    return true;
}


static bool artifact_pass_checks(struct artifact *art, int depth)
{
    /* Enforce minimum "depth" (loosely) */
    if (art->alloc_min > depth)
    {
        /* Get the "out-of-depth factor" */
        int d = (art->alloc_min - depth) * 2;

        /* Roll for out-of-depth creation */
        if (randint0(d)) return false;
    }

    /* Enforce maximum depth (strictly) */
    if (art->alloc_max < depth) return false;

    /* We must make the "rarity roll" */
    if (!magik(art->alloc_prob)) return false;

    return true;
}


/*
 * Hack -- attempt to create one of the "Special Objects"
 *
 * We are only called from "make_object()"
 *
 * Note -- see "make_artifact()" and "apply_magic()"
 */
static struct object *make_artifact_special(struct player *p, struct chunk *c, int level)
{
    int i;
    struct object *new_obj;
    bool art_ok = true;

    /* No artifacts, do nothing */
    if (p && OPT_P(p, birth_no_artifacts)) art_ok = false;

    /* Winners don't generate true artifacts */
    if (p && p->total_winner) art_ok = false;

    /* No artifacts in the town or on special levels */
    if (forbid_special(c->depth)) return false;

    /* Check the special artifacts */
    if (art_ok)
    {
        for (i = 0; i < z_info->a_max; i++)
        {
            struct artifact *art = &a_info[i];
            struct object_kind *kind = lookup_kind(art->tval, art->sval);
            char o_name[NORMAL_WID];

            /* Skip "empty" artifacts */
            if (!art->name) continue;

            /* Make sure the kind was found */
            if (!kind) continue;

            /* Skip non-special artifacts */
            if (!kf_has(kind->kind_flags, KF_INSTA_ART)) continue;

            /* Cannot make an artifact twice */
            if (art->created) continue;

            /* Cannot generate an artifact if disallowed by preservation mode  */
            if (p && (p->art_info[i] > cfg_preserve_artifacts)) continue;

            /* We must pass depth and rarity checks */
            if (!artifact_pass_checks(art, c->depth)) continue;

            /* Enforce minimum "object" level (loosely) */
            if (kind->level > level)
            {
                /* Acquire the "out-of-depth factor" */
                int d = (kind->level - level) * 5;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Assign the template */
            new_obj = object_new();
            object_prep(p, new_obj, kind, art->alloc_min, RANDOMISE);

            /* Mark the item as an artifact */
            new_obj->artifact = art;
            object_desc(NULL, o_name, sizeof(o_name), new_obj, ODESC_PREFIX | ODESC_BASE);
            plog_fmt("Special artifact %s created", o_name);

            /* Copy across all the data from the artifact struct */
            copy_artifact_data(new_obj, art);

            /* Mark the artifact as "created" */
            art->created++;
            if (p)
            {
                /* Mark the artifact as "generated" if dungeon is ready */
                if (!ht_zero(&c->generated))
                    set_artifact_info(p, new_obj, ARTS_GENERATED);

                /* Otherwise, preserve artifacts from dungeon generation errors */
                else p->art_info[new_obj->artifact->aidx] += ARTS_CREATED;
            }

            /* Success */
            return new_obj;
        }
    }

    /* An extra chance at being a randart */
    if (cfg_random_artifacts && p)
    {
        for (i = 0; i < z_info->a_max; i++)
        {
            struct artifact *art = &a_info[i];
            struct object_kind *kind = lookup_kind(art->tval, art->sval);
            s32b randart_seed;

            /* Skip "empty" artifacts */
            if (!art->name) continue;

            /* Make sure the kind was found */
            if (!kind) continue;

            /* Skip non-special artifacts */
            if (!kf_has(kind->kind_flags, KF_INSTA_ART)) continue;

            /* Enforce minimum "object" level (loosely) */
            if (kind->level > level)
            {
                /* Acquire the "out-of-depth factor" */
                int d = (kind->level - level) * 5;

                /* Roll for out-of-depth creation */
                if (randint0(d)) continue;
            }

            /* Cannot make a randart twice */
            if (p->randart_created[i]) continue;

            /* Cannot generate a randart if disallowed by preservation mode  */
            if (p->randart_info[i] > cfg_preserve_artifacts) continue;

            /* Piece together a 32-bit random seed */
            randart_seed = randint0(0xFFFF) << 16;
            randart_seed += randint0(0xFFFF);

            /* Attempt to change the object into a random artifact */
            art = do_randart(randart_seed, &a_info[i]);

            /* Skip "empty" artifacts again */
            if (!art) continue;

            /* We must pass depth and rarity checks */
            if (!artifact_pass_checks(art, c->depth))
            {
                free_artifact(art);
                continue;
            }

            /* Assign the template */
            new_obj = object_new();
            object_prep(p, new_obj, kind, art->alloc_min, RANDOMISE);

            /* Mark the item as a random artifact */
            make_randart(p, c, new_obj, art, randart_seed);

            /* Success */
            free_artifact(art);
            return new_obj;
        }
    }

    /* Failure */
    return NULL;
}


static bool create_randart_aux(struct player *p, struct chunk *c, struct object *obj,
    bool check)
{
    int i;

    for (i = 0; i < z_info->a_max; i++)
    {
        struct artifact *art = &a_info[i];
        struct object_kind *kind = lookup_kind(art->tval, art->sval);
        s32b randart_seed;

        /* Skip "empty" items */
        if (!art->name) continue;

        /* Make sure the kind was found */
        if (!kind) continue;

        /* Skip special artifacts */
        if (kf_has(kind->kind_flags, KF_INSTA_ART)) continue;

        /* Cannot make a randart twice */
        if (p->randart_created[i]) continue;

        /* Cannot generate a randart if disallowed by preservation mode  */
        if (p->randart_info[i] > cfg_preserve_artifacts) continue;

        /* Must have the correct fields */
        if (art->tval != obj->tval) continue;
        if (art->sval != obj->sval) continue;

        /* Piece together a 32-bit random seed */
        randart_seed = randint0(0xFFFF) << 16;
        randart_seed += randint0(0xFFFF);

        /* Attempt to change the object into a random artifact */
        art = do_randart(randart_seed, &a_info[i]);

        /* Skip "empty" items again */
        if (!art) continue;

        /* We must pass depth and rarity checks */
        if (check && !artifact_pass_checks(art, c->depth))
        {
            free_artifact(art);
            continue;
        }

        /* Mark the item as a random artifact */
        make_randart(p, c, obj, art, randart_seed);

        /* Success */
        free_artifact(art);
        return true;
    }

    /* Failure */
    return false;
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
static bool make_artifact(struct player *p, struct chunk *c, struct object *obj)
{
    int i;
    bool art_ok = true;

    /* Make sure birth no artifacts isn't set */
    if (p && OPT_P(p, birth_no_artifacts)) art_ok = false;

    /* Winners don't generate true artifacts */
    if (p && p->total_winner) art_ok = false;

    /* Special handling of quest artifacts */
    if (kf_has(obj->kind->kind_flags, KF_QUEST_ART)) art_ok = true;

    /* No artifacts in the town or on special levels */
    if (forbid_special(c->depth)) return false;

    /* Paranoia -- no "plural" artifacts */
    if (obj->number != 1) return false;

    /* Check the artifact list (skip the "specials") */
    if (art_ok)
    {
        for (i = 0; !obj->artifact && (i < z_info->a_max); i++)
        {
            struct artifact *art = &a_info[i];
            struct object_kind *kind = lookup_kind(art->tval, art->sval);
            char o_name[NORMAL_WID];

            /* Skip "empty" items */
            if (!art->name) continue;

            /* Make sure the kind was found */
            if (!kind) continue;

            /* Skip special artifacts */
            if (kf_has(kind->kind_flags, KF_INSTA_ART)) continue;

            /* Cannot make an artifact twice */
            if (art->created) continue;

            /* Cannot generate an artifact if disallowed by preservation mode  */
            if (p && (p->art_info[i] > cfg_preserve_artifacts))
                continue;

            /* Must have the correct fields */
            if (art->tval != obj->tval) continue;
            if (art->sval != obj->sval) continue;

            /* We must pass depth and rarity checks */
            if (!artifact_pass_checks(art, c->depth)) continue;

            /* Mark the item as an artifact */
            obj->artifact = art;
            object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
            plog_fmt("Artifact %s created", o_name);
        }

        if (obj->artifact)
        {
            /* Copy across all the data from the artifact struct */
            copy_artifact_data(obj, obj->artifact);

            /* Mark the artifact as "created" */
            obj->artifact->created++;
            if (p)
            {
                /* Mark the artifact as "generated" if dungeon is ready */
                if (!ht_zero(&c->generated))
                    set_artifact_info(p, obj, ARTS_GENERATED);

                /* Otherwise, preserve artifacts from dungeon generation errors */
                else p->art_info[obj->artifact->aidx] += ARTS_CREATED;
            }

            /* Success */
            return true;
        }
    }

    /* An extra chance at being a randart */
    if (cfg_random_artifacts && p)
    {
        if (create_randart_aux(p, c, obj, true))
        {
            /* Success */
            return true;
        }
    }

    /* Failure */
    return false;
}


/*** Apply magic to an item ***/


/*
 * Apply magic to a weapon
 */
static void apply_magic_weapon(struct object *obj, int level, int power)
{
    if (power <= 0) return;

    obj->to_h += randint1(5) + m_bonus(5, level);
    obj->to_d += randint1(5) + m_bonus(5, level);

    if (power > 1)
    {
        obj->to_h += m_bonus(10, level);
        obj->to_d += m_bonus(10, level);

        if (tval_is_melee_weapon(obj) || tval_is_ammo(obj))
        {
            /* Super-charge the damage dice */
            while (one_in_(10L * obj->dd * obj->ds)) obj->dd++;

            /* But not too high */
            if (tval_is_melee_weapon(obj) && (obj->dd > MAX_WEAPON_DICE))
                obj->dd = MAX_WEAPON_DICE;
            else if (tval_is_ammo(obj) && (obj->dd > MAX_AMMO_DICE))
                obj->dd = MAX_AMMO_DICE;
        }
    }
}


/*
 * Apply magic to armour
 */
static void apply_magic_armour(struct object *obj, int level, int power)
{
    if (power <= 0) return;

    obj->to_a += randint1(5) + m_bonus(5, level);
    if (power > 1) obj->to_a += m_bonus(10, level);

    /* Bad */
    if (obj->to_a < 0)
    {
        size_t i;

        /* Hack -- reverse base bonuses */
        for (i = 0; i < OBJ_MOD_MAX; i++)
        {
            if (obj->modifiers[i] > 0) obj->modifiers[i] = 0 - obj->modifiers[i];
        }
    }
}


/*
 * Wipe an object clean and make it a standard object of the specified kind.
 */
void object_prep(struct player *p, struct object *obj, struct object_kind *k, int lev,
    aspect rand_aspect)
{
    int i;

    /* Clean slate */
    object_wipe(obj);

    /* Assign the kind and copy across data */
    obj->kind = k;
    obj->tval = k->tval;
    obj->sval = k->sval;
    obj->ac = k->ac;
    obj->dd = k->dd;
    obj->ds = k->ds;
    obj->weight = k->weight;

    obj->effect = k->effect;
    obj->activation = k->activation;
    obj->time = k->time;

    /* Default number */
    obj->number = 1;

    /* Copy flags */
    if (k->base) of_copy(obj->flags, k->base->flags);
    of_union(obj->flags, k->flags);

    /* Assign modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
        obj->modifiers[i] = randcalc(k->modifiers[i], lev, rand_aspect);

    /* Hack -- amulets of speed can't give very much, and are rarely +5 */
    if (tval_is_amulet(obj) && (obj->sval == lookup_sval(obj->tval, "Speed")))
        obj->modifiers[OBJ_MOD_SPEED] = randint1(obj->modifiers[OBJ_MOD_SPEED]);

    /* Hack -- rings of polymorphing get a random race */
    if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")))
    {
        struct monster_race *race;
        int rarity;

        /* Pick a race (try to roughly match monster power) */
        do
        {
            race = get_mon_num_poly(lev);
            if (!race) break;
            rarity = 1 + MAX(race->level, 1) * race->mexp / 25000;
        }
        while (randint0(rarity) || !race->name);

        /* Handle failure smartly: create a useless ring of <player> */
        if (!race) race = &r_info[0];

        obj->modifiers[OBJ_MOD_POLY_RACE] = race->ridx;
    }

    /* Assign charges (wands/staves only) */
    if (tval_can_have_charges(obj))
        obj->pval = randcalc(k->charge, lev, rand_aspect);

    /* Assign flagless pval for food or oil */
    if (tval_can_have_nourishment(obj) || tval_is_fuel(obj) || tval_is_launcher(obj))
        obj->pval = randcalc(k->pval, lev, rand_aspect);

    /* Default fuel */
    if (tval_is_light(obj)) fuel_default(obj);

    /* Default magic */
    obj->to_h = randcalc(k->to_h, lev, rand_aspect);
    obj->to_d = randcalc(k->to_d, lev, rand_aspect);
    obj->to_a = randcalc(k->to_a, lev, rand_aspect);

    /* Default slays and brands */
    copy_slay(&obj->slays, k->slays);
    copy_brand(&obj->brands, k->brands);

    /* Default resists */
    for (i = 0; i < ELEM_MAX; i++)
    {
        obj->el_info[i].res_level = k->el_info[i].res_level;
        obj->el_info[i].flags = k->el_info[i].flags;
        if (k->base) obj->el_info[i].flags |= k->base->el_info[i].flags;
    }

    object_set_base_known(p, obj);
}


/*
 * Applying magic to an object, which includes creating ego-items, and applying
 * random bonuses.
 *
 * The `good` argument forces the item to be at least `good`, and the `great`
 * argument does likewise.  Setting `allow_artifacts` to true allows artifacts
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
int apply_magic(struct player *p, struct chunk *c, struct object *obj, int lev,
    bool allow_artifacts, bool good, bool great, bool extra_roll)
{
    int i;
    s16b power = 0;

    /* Chance of being `good` and `great` */
    int good_chance = MIN(33 + lev, 100);
    int great_chance = 30;

    /* Magic ammo are always +0 +0 (not a "good" drop) */
    if (magic_ammo_p(obj)) return ((good || great)? -1: 0);

    if (lev >= z_info->max_depth) lev = z_info->max_depth - 1;

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

        /* Get two rolls if forced great */
        if (great) rolls = 2;

        /* Give some extra rolls for uniques and acq scrolls */
        if (extra_roll) rolls += 2;

        /* Roll for artifacts if allowed */
        for (i = 0; i < rolls; i++)
        {
            if (make_artifact(p, c, obj)) return 3;
        }
    }

    /* Try to make an ego item */
    if (power == 2) make_ego_item(obj, lev);

    /* Apply magic */
    if (tval_is_tool(obj) || tval_is_mstaff(obj))
    {
        /* Not a "great" drop */
        if (great && !obj->ego) return -1;

        /* Not a "good" drop */
        if (good && !obj->ego) return -1;
    }
    else if (tval_is_enchantable_weapon(obj))
    {
        /* Not a "great" drop */
        if (great && !obj->ego) return -1;

        apply_magic_weapon(obj, lev, power);

        /* Not a "good" drop */
        if (good && !obj->ego && ((obj->to_h <= 0) || (obj->to_d <= 0))) return -1;
    }
    else if (tval_is_armor(obj))
    {
        /* Not a "great" drop */
        if (great && !obj->ego) return -1;

        apply_magic_armour(obj, lev, power);

        /* Not a "good" drop */
        if (good && !obj->ego && (obj->to_a <= 0)) return -1;
    }
    else if (tval_is_ring(obj))
    {
        if (obj->sval == lookup_sval(obj->tval, "Speed"))
        {
            /* Super-charge the ring */
            while (one_in_(2)) obj->modifiers[OBJ_MOD_SPEED]++;
        }
    }
    else if (tval_is_amulet(obj))
    {
        /* Extra powers */
        /* TODO: should be global to all objects */
        do_powers(obj, obj->kind->kind_flags);
    }
    else if (tval_is_light(obj))
    {
        /* Not a "great" drop */
        if (great && !obj->ego) return -1;

        /* Not a "good" drop */
        if (good && !obj->ego) return -1;
    }
    else if (tval_is_chest(obj))
    {
        /* Hack -- skip ruined chests */
        if (obj->kind->level > 0)
        {
            /* Hack -- pick a "difficulty" */
            obj->pval = randint1(obj->kind->level);

            /* Never exceed "difficulty" of 55 to 59 */
            if (obj->pval > 55)
                obj->pval = 55 + randint0(5);
        }
    }

    return power;
}


/*** Generate a random object ***/


/*
 * Hack -- determine if a template is "good"
 */
bool kind_is_good(const struct object_kind *kind)
{
    /* Some item types are (almost) always good */
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
        case TV_CROWN: return true;

        /* Weapons */
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_MSTAFF: return true;

        /* Tools */
        case TV_DIGGING:
        case TV_HORN: return true;

        /* Ammo */
        case TV_ROCK:
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            /* Magic ammo never get good (they cannot become ego/art anyway...) */
            return !kf_has(kind->kind_flags, KF_AMMO_MAGIC);
        }

        /* Light sources */
        case TV_LIGHT: return true;
    }

    /* Other item types */
    return kind_is_good_other(kind);
}


/*
 * Choose an object kind of a given tval given a dungeon level.
 */
static struct object_kind *get_obj_num_by_kind(int level, bool good, int tval)
{
    size_t ind, item;
    u32b value;
    int total = 0;
    byte *objects = (good? obj_alloc_great: obj_alloc);

    /* This is the base index into obj_alloc for this dlev */
    ind = level * z_info->k_max;

    /* Get new total */
    for (item = 1; item < (size_t)z_info->k_max; item++)
    {
        if (k_info[item].tval == tval) total += objects[ind + item];
    }

    /* No appropriate items of that tval */
    if (!total) return NULL;

    value = randint0(total);

    /* Pick an object */
    for (item = 1; item < (size_t)z_info->k_max; item++)
    {
        if (k_info[item].tval == tval)
        {
            if (value < (u32b)objects[ind + item]) break;
            value -= objects[ind + item];
        }
    }

    /* Return the item index */
    return &k_info[item];
}


/*
 * Choose an object kind given a dungeon level to choose it for.
 * If tval = 0, we can choose an object of any type.
 * Otherwise we can only choose one of the given tval.
 */
struct object_kind *get_obj_num(int level, bool good, int tval)
{
    size_t ind, item;
    u32b value;

    /* Occasional level boost */
    if ((level > 0) && one_in_(z_info->great_obj))
    {
        /* What a bizarre calculation */
        level = 1 + (level * z_info->max_obj_depth / randint1(z_info->max_obj_depth));
    }

    /* Paranoia */
    level = MIN(level, z_info->max_obj_depth);
    level = MAX(level, 0);

    if (tval) return get_obj_num_by_kind(level, good, tval);

    /* This is the base index into obj_alloc for this dlev */
    ind = level * z_info->k_max;

    /* Pick an object */
    if (!good)
    {
        value = randint0(obj_total[level]);
        for (item = 1; item < (size_t)z_info->k_max; item++)
        {
            /* Found it */
            if (value < (u32b)obj_alloc[ind + item]) break;

            /* Decrement */
            value -= obj_alloc[ind + item];
        }
    }
    else
    {
        value = randint0(obj_total_great[level]);
        for (item = 1; item < (size_t)z_info->k_max; item++)
        {
            /* Found it */
            if (value < (u32b)obj_alloc_great[ind + item]) break;

            /* Decrement */
            value -= obj_alloc_great[ind + item];
        }
    }

    /* Paranoia */
    if (item == (size_t)z_info->k_max) return NULL;

    /* Return the item index */
    return &k_info[item];
}


/*
 * Attempt to make an object
 *
 * c is the current dungeon level
 * lev is the creation level of the object (not necessarily == depth)
 * good is whether the object is to be good
 * great is whether the object is to be great
 * extra_roll is whether we get an extra roll in apply_magic()
 * value is the value to be returned to the calling function
 * tval is the desired tval, or 0 if we allow any tval
 *
 * Returns a pointer to the newly allocated object, or NULL on failure.
 */
struct object *make_object(struct player *p, struct chunk *c, int lev, bool good, bool great,
    bool extra_roll, s32b *value, int tval)
{
    int base;
    struct object_kind *kind;
    struct object *new_obj;
    int i;
    int tries = 1;
    bool ok = false;

    /* Try to make a special artifact */
    if (one_in_(good? 10: 1000))
    {
        new_obj = make_artifact_special(p, c, lev);
        if (new_obj)
        {
            if (value) *value = object_value_real(p, new_obj, 1);
            return new_obj;
        }

        /* If we failed to make an artifact, the player gets a good item */
        good = true;
    }

    /* Base level for the object */
    base = (good? (lev + 10): lev);

    /* Try harder to generate a "good" object */
    if (good || great) tries = 3;

    for (i = 1; i <= tries; i++)
    {
        s16b res;

        /* Try to choose an object kind */
        kind = get_obj_num(base, good || great, tval);
        if (!kind) return NULL;

        /* Make the object, prep it and apply magic */
        new_obj = object_new();
        object_prep(p, new_obj, kind, lev, RANDOMISE);
        res = apply_magic(p, c, new_obj, lev, true, good, great, extra_roll);

        /* Reroll "good" objects of incorrect kind (diggers, light sources...) */
        /* Reroll "great" objects when make_ego_item fails */
        if (res == -1)
        {
            /* Handle failure */
            object_delete(&new_obj);
            continue;
        }

        /* We have a valid object */
        ok = true;
        break;
    }

    /* Handle failure */
    if (!ok) return false;

    /* Generate multiple items */
    if (!new_obj->artifact)
    {
        if ((new_obj->kind->gen_mult_prob >= 100) || (new_obj->kind->gen_mult_prob >= randint1(100)))
            new_obj->number = randcalc(new_obj->kind->stack_size, lev, RANDOMISE);
    }

    if (new_obj->number > z_info->stack_size) new_obj->number = z_info->stack_size;

    /* Get the value */
    if (value) *value = object_value_real(p, new_obj, new_obj->number);

    /* Boost of 20% per level OOD for uncursed objects */
    if (!cursed_p(new_obj->flags) && (new_obj->kind->alloc_min > object_level(c->depth)))
    {
        if (value) *value += (new_obj->kind->alloc_min - object_level(c->depth)) * (*value / 5);
    }

    return new_obj;
}


/*
 * Scatter some "great" objects near the player
 */
void acquirement(struct player *p, struct chunk *c, int num, quark_t quark)
{
    struct object *nice_obj;

    /* Acquirement */
    while (num--)
    {
        /* Make a good (or great) object (if possible) */
        nice_obj = make_object(p, c, object_level(p->depth), true, true, true, NULL, 0);
        if (!nice_obj) continue;

        set_origin(nice_obj, ORIGIN_ACQUIRE, p->depth, 0);
        if (quark > 0) nice_obj->note = quark;

        /* Drop the object */
        drop_near(p, c, nice_obj, 0, p->py, p->px, true, DROP_FADE);
    }
}


/*** Make a gold item ***/


/*
 * Get a money kind by name, or level-appropriate
 */
struct object_kind *money_kind(const char *name, int value)
{
    int rank;

    /*
     * (Roughly) the largest possible gold drop at max depth - the precise
     * value is derivable from the calculations in make_gold(), but this is
     * near enough
     */
    int max_gold_drop = 3 * z_info->max_depth + 30;

    /* Check for specified treasure variety */
    for (rank = 0; rank < num_money_types; rank++)
    {
        if (streq(name, money_type[rank].name)) break;
    }

    /* Pick a treasure variety scaled by level */
    if (rank == num_money_types)
        rank = (((value * 100) / max_gold_drop) * num_money_types) / 100;

    /* Do not create illegal treasure types */
    if (rank >= num_money_types) rank = num_money_types - 1;

    return lookup_kind(TV_GOLD, money_type[rank].type);
}


/*
 * Make a money object
 *
 * lev the dungeon level
 * coin_type the name of the type of money object to make
 *
 * Returns a pointer to the newly minted cash (cannot fail)
 */
struct object *make_gold(struct player *p, int lev, char *coin_type)
{
    /* This average is 18 at dlev0, 90 at dlev40, 198 at dlev100. */
    s32b avg = (18 * lev) / 10 + 18;
    s32b spread = lev + 10;
    s32b value = rand_spread(avg, spread);
    struct object *new_gold = object_new();

    /* Increase the range to infinite, moving the average to 110% */
    while (one_in_(100) && (value * 10 <= SHRT_MAX)) value *= 10;

    /* Prepare a gold object */
    object_prep(p, new_gold, money_kind(coin_type, value), lev, RANDOMISE);

    /* If we're playing with no_selling, increase the value */
    if (p && OPT_P(p, birth_no_selling) && (p->depth > 0))
        value = value * MIN(5, p->depth);

    new_gold->pval = value;

    return new_gold;
}


void make_randart(struct player *p, struct chunk *c, struct object *obj, struct artifact *art,
    s32b randart_seed)
{
    char o_name[NORMAL_WID];

    /* Mark the item as a random artifact */
    obj->artifact = &a_info[art->aidx];
    obj->randart_seed = randart_seed;
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
    plog_fmt("Random artifact %s created", o_name);

    /* Copy across all the data from the artifact struct */
    copy_artifact_data(obj, art);

    /* Mark the randart as "created" */
    p->randart_created[obj->artifact->aidx] = 1;
    obj->creator = p->id;

    /* Mark the artifact as "generated" if dungeon is ready */
    if (!ht_zero(&c->generated))
        set_artifact_info(p, obj, ARTS_GENERATED);

    /* Otherwise, preserve artifacts from dungeon generation errors */
    else
        p->randart_info[obj->artifact->aidx] += ARTS_CREATED;
}


void fuel_default(struct object *obj)
{
    /* Default fuel levels */
    if (of_has(obj->flags, OF_BURNS_OUT))
        obj->timeout = z_info->fuel_torch;
    else if (of_has(obj->flags, OF_TAKES_FUEL))
        obj->timeout = z_info->default_lamp;
}


void create_randart(struct player *p, struct chunk *c)
{
    struct object *obj;

    if (!cfg_random_artifacts)
    {
        msg(p, "You cannot create random artifacts.");
        return;
    }

    /* Use the first object on the floor */
    obj = square_object(c, p->py, p->px);
    if (!obj)
    {
        msg(p, "There is nothing on the floor.");
        return;
    }

    /* Object must be unique, non ego, non artifact */
    if ((obj->number > 1) || obj->ego || obj->artifact)
    {
        msg(p, "The object is not suited for artifact creation.");
        return;
    }

    /* Set unidentified */
    if (obj->known)
    {
        free_brand(obj->known->brands);
        free_slay(obj->known->slays);
        mem_free(obj->known);
    }
    object_set_base_known(p, obj);

    if (create_randart_aux(p, c, obj, false))
    {
        /* Success */
        msg(p, "You manage to create a random artifact.");
        return;
    }

    /* Failure */
    msg(p, "You don't manage to create a random artifact.");
}


struct init_module obj_make_module =
{
    "obj-make",
    init_obj_make,
    cleanup_obj_make
};
