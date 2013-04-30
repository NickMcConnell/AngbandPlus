/*
 * File: slays.c
 * Purpose: Encapsulation of slay_table and accessor functions for slays/brands
 *
 * Copyright (c) 2010 Chris Carr and Peter Denison
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
#include "../attack.h"
#include "../s-spells.h"
#include "slays.h"


/*
 * Info about slays (see src/slays.h for structure)
 */
static const struct slay slay_table[] =
{
    #define SLAY(a, b, c, d, e, f, g, h, i, j, k, l) \
        {SL_##a, b, c, d, e, f, g, h, i, j, k, l},
    #include "list-slays.h"
    #undef SLAY
    {0}
};


/*
 * Slay cache. Used for looking up slay values in obj-power.c
 */
struct flag_cache
{
    bitflag flags[OF_SIZE]; /* Combination of slays and brands */
    s32b value;             /* Value of this combination */
};


/*
 * Cache of slay values (for object_power)
 */
static struct flag_cache *slay_cache;

static u16b cache_size, cache_index;

#define CACHE_INCREMENT 100


/*
 * Remove slays which are duplicates, i.e. they have exactly the same "monster
 * flag" and the same "resist flag". The one with highest multiplier is kept.
 *
 * flags is the flagset from which to remove duplicates.
 * count is the number of dups removed.
 */
int dedup_slays(bitflag *flags)
{
    int i, j;
    int count = 0;

    for (i = 0; i < SL_MAX; i++)
    {
        const struct slay *s_ptr = &slay_table[i];

        if (of_has(flags, s_ptr->object_flag))
        {
            for (j = i + 1; j < SL_MAX; j++)
            {
                const struct slay *t_ptr = &slay_table[j];

                if (of_has(flags, t_ptr->object_flag) &&
                    (t_ptr->monster_flag == s_ptr->monster_flag) &&
                    (t_ptr->resist_flag == s_ptr->resist_flag) &&
                    (t_ptr->mult != s_ptr->mult))
                {
                    count++;
                    if (t_ptr->mult > s_ptr->mult)
                        of_off(flags, s_ptr->object_flag);
                    else
                        of_off(flags, t_ptr->object_flag);
                }
            }
        }
    }

    return count;
}


/*
 * Get a random slay (or brand).
 * We use randint1 because the first entry in slay_table is null.
 *
 * mask is the set of slays from which we are choosing.
 */
const struct slay *random_slay(const bitflag mask[OF_SIZE])
{
    const struct slay *s_ptr;
    int size = SL_MAX - 1;

    /* PWMAngband: substract 2 RF_HURT_XXX flags */
    size -= 2;

    do
    {
        s_ptr = &slay_table[randint1(size)];
    }
    while (!of_has(mask, s_ptr->object_flag));

    return s_ptr;
}


/*
 * Match slays in flags against a chosen flag mask
 *
 * count is the number of matches
 * flags is the flagset to analyse for matches
 * mask is the flagset against which to test
 * desc[] is the array of descriptions of matching slays - can be null
 * brand[] is the array of descriptions of brands - can be null
 * mult[] is the array of multipliers of those slays - can be null
 * dedup is whether or not to remove duplicates
 *
 * desc[], brand[] and mult[] must be >= SL_MAX in size
 */
int list_slays(const bitflag flags[OF_SIZE], const bitflag mask[OF_SIZE],
    const char *desc[], const char *brand[], int mult[], bool dedup)
{
    int i, count = 0;
    bitflag f[OF_SIZE];

    /* We are only interested in the flags specified in mask */
    of_copy(f, flags);
    of_inter(f, mask);

    /* Remove "duplicate" flags if desired */
    if (dedup) i = dedup_slays(f);

    /* Collect slays */
    for (i = 0; i < SL_MAX; i++)
    {
        const struct slay *s_ptr = &slay_table[i];

        if (brand && !s_ptr->brand) continue;

        if (of_has(f, s_ptr->object_flag))
        {
            if (mult) mult[count] = s_ptr->mult;
            if (brand) brand[count] = s_ptr->brand;
            if (desc) desc[count] = s_ptr->desc;
            count++;
        }
    }

    return count;
}


/*
 * Notice any slays on a particular object which are in mask.
 *
 * o_ptr is the object on which we are noticing slays
 * mask is the flagset within which we are noticing them
 */
void object_notice_slays(struct player *p, object_type *o_ptr, const bitflag mask[OF_SIZE])
{
    bool learned;
    bitflag f[OF_SIZE];
    char o_name[NORMAL_WID];
    int i;

    /* We are only interested in the flags specified in mask */
    object_flags(o_ptr, f);
    of_inter(f, mask);

    /* If you learn a slay, learn the ego and print a message */
    for (i = 0; i < SL_MAX; i++)
    {
        const struct slay *s_ptr = &slay_table[i];

        if (of_has(f, s_ptr->object_flag))
        {
            learned = object_notice_flag(p, o_ptr, s_ptr->object_flag);
            if (EASY_LEARN && learned)
            {
                object_notice_ego(p, o_ptr);
                object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
                msg(p, "Your %s %s!", o_name, s_ptr->active_verb);
            }
        }
    }

    object_check_for_ident(p, o_ptr);
}


/*
 * Extract the multiplier from a given object hitting a given monster.
 *
 * o_ptr is the object being used to attack
 * m_idx is the target index (or 0 if a target race is specified)
 * is_ranged should be true for ranged attacks
 * r_idx is the target race (or 0 if a target index is specified)
 * best_s_ptr is the best applicable slay_table entry, or NULL if no slay already known
 * known_only is whether we are using all the object flags, or only
 * the ones we *already* know about
 */
void improve_attack_modifier(struct player *p, object_type *o_ptr, int m_idx, bool is_ranged,
    int r_idx, const struct slay **best_s_ptr, bool known_only)
{
    monster_race *r_ptr;
    monster_lore *l_ptr;
    bitflag f[OF_SIZE], known_f[OF_SIZE], note_f[OF_SIZE];
    monster_race r_local;
    bool ml;
    int i;
    player_type *q_ptr = NULL;

    /* Monster info */
    if (m_idx > 0)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), m_idx);

        r_ptr = &r_info[m_ptr->r_idx];
        l_ptr = &p->lore[m_ptr->r_idx];
        ml = p->mon_vis[m_idx];
    }

    /* Player info */
    else if (m_idx < 0)
    {
        q_ptr = player_get(0 - m_idx);
        r_ptr = &r_local;
        l_ptr = NULL;
        ml = FALSE;

        /* Handle polymorphed players */
        if (q_ptr->r_idx)
        {
            /* Create a copy of the monster race (player) */
            COPY(r_ptr, &r_info[q_ptr->r_idx], monster_race);
        }

        /* Handle special races */
        if (player_has(q_ptr, PF_ORC)) rf_on(r_ptr->flags, RF_ORC);
        if (player_has(q_ptr, PF_TROLL)) rf_on(r_ptr->flags, RF_TROLL);
        if (player_has(q_ptr, PF_GIANT)) rf_on(r_ptr->flags, RF_GIANT);
        if (player_has(q_ptr, PF_THUNDERLORD) || player_has(q_ptr, PF_DRAGON))
            rf_on(r_ptr->flags, RF_DRAGON);
        if (player_has(q_ptr, PF_ANIMAL)) rf_on(r_ptr->flags, RF_ANIMAL);

        /* Handle resistances/immunities */
        if (check_for_resist(q_ptr, GF_ACID, FALSE) > 0)
            rf_on(r_ptr->flags, RF_IM_ACID);
        if (check_for_resist(q_ptr, GF_ELEC, FALSE) > 0)
            rf_on(r_ptr->flags, RF_IM_ELEC);
        if (check_for_resist(q_ptr, GF_FIRE, FALSE) > 0)
            rf_on(r_ptr->flags, RF_IM_FIRE);
        if (check_for_resist(q_ptr, GF_COLD, FALSE) > 0)
            rf_on(r_ptr->flags, RF_IM_COLD);
        if (check_state(q_ptr, OF_RES_POIS))
            rf_on(r_ptr->flags, RF_IM_POIS);
    }

    /* Race info */
    else
    {
        r_ptr = &r_info[r_idx];
        l_ptr = NULL;
        ml = FALSE;
    }

    /* Clear */
    of_wipe(f);
    of_wipe(known_f);

    /* Extract the flags */
    if (o_ptr)
    {
        object_flags(o_ptr, f);
        object_flags_known(o_ptr, known_f, object_flavor_is_aware(p, o_ptr));
    }

    /* Handle polymorphed players */
    else if (!is_ranged && p->r_idx)
        apply_poly_brand(p->r_idx, randint0(MONSTER_BLOW_MAX), f, known_f);

    /* Hack -- Extract temp branding */
    else if (is_ranged && p->timed[TMD_BOWBRAND])
        apply_bow_brand(p->bow_brand_t, f, known_f);

    /* Hack -- Extract temp branding */
    else if (!is_ranged && p->timed[TMD_SGRASP])
    {
        of_on(f, OF_BRAND_ELEC);
        of_on(known_f, OF_BRAND_ELEC);
    }

    /* Notice flags for players */
    if (m_idx < 0)
    {
        if (of_has(f, OF_BRAND_ELEC))
        {
            wieldeds_notice_flag(q_ptr, OF_IM_ELEC);
            wieldeds_notice_flag(q_ptr, OF_RES_ELEC);
            wieldeds_notice_flag(q_ptr, OF_VULN_ELEC);
        }
        if (of_has(f, OF_BRAND_COLD))
        {
            wieldeds_notice_flag(q_ptr, OF_IM_COLD);
            wieldeds_notice_flag(q_ptr, OF_RES_COLD);
            wieldeds_notice_flag(q_ptr, OF_VULN_COLD);
        }
        if (of_has(f, OF_BRAND_FIRE))
        {
            wieldeds_notice_flag(q_ptr, OF_IM_FIRE);
            wieldeds_notice_flag(q_ptr, OF_RES_FIRE);
            wieldeds_notice_flag(q_ptr, OF_VULN_FIRE);
        }
        if (of_has(f, OF_BRAND_ACID))
        {
            wieldeds_notice_flag(q_ptr, OF_IM_ACID);
            wieldeds_notice_flag(q_ptr, OF_RES_ACID);
            wieldeds_notice_flag(q_ptr, OF_VULN_ACID);
        }
        if (of_has(f, OF_BRAND_POIS))
            wieldeds_notice_flag(q_ptr, OF_RES_POIS);
    }

    /* Slays and brands */
    for (i = 0; i < SL_MAX; i++)
    {
        const struct slay *s_ptr = &slay_table[i];

        if ((known_only && !of_has(known_f, s_ptr->object_flag)) ||
            (!known_only && !of_has(f, s_ptr->object_flag))) continue;

        /*
         * In a real attack, learn about monster resistance or slay match if:
         * EITHER the slay flag on the object is known,
         * OR the monster is vulnerable to the slay/brand
         */
        if (m_idx && (of_has(known_f, s_ptr->object_flag) ||
            (s_ptr->monster_flag && rf_has(r_ptr->flags, s_ptr->monster_flag)) ||
            (s_ptr->resist_flag && !rf_has(r_ptr->flags, s_ptr->resist_flag))))
        {
            /* Notice any brand or slay that would affect the monster */
            if (o_ptr)
            {
                of_wipe(note_f);
                of_on(note_f, s_ptr->object_flag);
                object_notice_slays(p, o_ptr, note_f);
            }

            if (ml && s_ptr->monster_flag)
                rf_on(l_ptr->flags, s_ptr->monster_flag);

            if (ml && s_ptr->resist_flag)
                rf_on(l_ptr->flags, s_ptr->resist_flag);
        }

        /* If the monster doesn't resist or the slay flag matches */
        if ((s_ptr->brand && !rf_has(r_ptr->flags, s_ptr->resist_flag)) ||
            (s_ptr->monster_flag && rf_has(r_ptr->flags, s_ptr->monster_flag)))
        {
            /* Compare multipliers to determine best attack */
            if ((*best_s_ptr == NULL) || ((*best_s_ptr)->mult < s_ptr->mult) ||
                (((*best_s_ptr)->mult == s_ptr->mult) && (s_ptr->object_flag == OF_BRAND_POIS)))
            {
                *best_s_ptr = s_ptr;
            }
        }
    }
}


/*
 * React to slays which hurt a monster
 *
 * obj_flags is the set of flags we're testing for slays
 * mon_flags is the set of flags we're adjusting as a result
 */
void react_to_slay(bitflag *obj_flags, bitflag *mon_flags)
{
    int i;
    int max = SL_MAX;

    /* PWMAngband: substract 2 RF_HURT_XXX flags */
    max -= 2;

    for (i = 0; i < max; i++)
    {
        const struct slay *s_ptr = &slay_table[i];

        if (of_has(obj_flags, s_ptr->object_flag) && s_ptr->monster_flag)
            rf_on(mon_flags, s_ptr->monster_flag);
    }
}


/*
 * Check the slay cache for a combination of slays and return a slay value
 *
 * index is the set of slay flags to look for
 */
s32b check_slay_cache(bitflag *index)
{
    for (cache_index = 0; !of_is_empty(slay_cache[cache_index].flags); cache_index++)
    {
        if (of_is_equal(index, slay_cache[cache_index].flags)) break;
    }

    return slay_cache[cache_index].value;
}


/*
 * Fill in a value in the slay cache.
 *
 * index is the set of slay flags whose value we are adding
 * value is the value of the slay flags in index
 */
void fill_slay_cache(bitflag *index, s32b value)
{
    int i;

    /* Expand the cache if necessary */
    if (cache_index == cache_size)
    {
        cache_size += CACHE_INCREMENT;
        slay_cache = mem_realloc(slay_cache, (cache_size + 1) * sizeof(struct flag_cache));
        for (i = cache_index + 1; i <= cache_size; i++)
            WIPE(&slay_cache[i], struct flag_cache);
    }

    /* Add to the cache */
    of_copy(slay_cache[cache_index].flags, index);
    slay_cache[cache_index].value = value;
}


/*
 * Create a cache of slay combinations, and the values of
 * these combinations. This is to speed up slay_power(), which will be called
 * many times during the game.
 */
void create_slay_cache(void)
{
    cache_size = CACHE_INCREMENT;
    slay_cache = C_ZNEW(cache_size + 1, struct flag_cache);
}


void free_slay_cache(void)
{
    mem_free(slay_cache);
}
