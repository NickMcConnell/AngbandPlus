/*
 * File: obj-slays.c
 * Purpose: Functions for manipulating slays/brands
 *
 * Copyright (c) 2010 Chris Carr and Peter Denison
 * Copyright (c) 2014 Nick McConnell
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
 * Slay cache. Used for looking up slay values in obj-power.c
 */
struct slay_cache
{
    struct brand *brands;   /* Brands */
    struct slay *slays;     /* Slays */
    s32b value;             /* Value of this combination */
};


/*
 * Cache of slay values (for object_power)
 */
static struct slay_cache *slay_cache;

static u16b cache_size, cache_index;

#define CACHE_INCREMENT 100


struct brand_info
{
    const char* name;
    const char *active_verb;
    const char *active_verb_plural;
    const char *melee_verb;
    const char *melee_verb_weak;
    int resist_flag;
};


/*
 * Brand info
 *
 * The verbs here probably should go in list-elements.h, when that's been
 * sorted properly, and there will also need to be a list of possibilities
 * in obj-randart.c
 */
static const struct brand_info brand_names[] =
{
    {"acid", "spits", "spit", "dissolve", "corrode", RF_IM_ACID},
    {"lightning", "crackles", "crackle", "shock", "zap", RF_IM_ELEC},
    {"fire", "flares", "flare", "burn", "singe", RF_IM_FIRE},
    {"cold", "grows cold", "grow cold", "freeze", "chill", RF_IM_COLD},
    {"poison", "seethes", "seethe", "poison", "sicken", RF_IM_POIS}
};


struct slay_info
{
    const char* name;
    int race_flag;
    int multiplier;
    int esp_chance;
    int esp_flag;
};


/*
 * Slay info
 *
 * These should go into obj-randart.c, but can wait for brands to be done
 */
static const struct slay_info slay_names[] =
{
    {"evil creatures", RF_EVIL, 2, 3, OF_ESP_EVIL},
    {"animals", RF_ANIMAL, 2, 2, OF_ESP_ANIMAL},
    {"orcs", RF_ORC, 3, 2, OF_ESP_ORC},
    {"trolls", RF_TROLL, 3, 2, OF_ESP_TROLL},
    {"giants", RF_GIANT, 3, 2, OF_ESP_GIANT},
    {"demons", RF_DEMON, 3, 2, OF_ESP_DEMON},
    {"dragons", RF_DRAGON, 3, 2, OF_ESP_DRAGON},
    {"undead", RF_UNDEAD, 3, 2, OF_ESP_UNDEAD},
    {"demons", RF_DEMON, 5, 1, OF_ESP_DEMON},
    {"dragons", RF_DRAGON, 5, 1, OF_ESP_DRAGON},
    {"undead", RF_UNDEAD, 5, 1, OF_ESP_UNDEAD}
};


/*
 * Copy all the slays from one structure to another
 *
 * dest the address the slays are going to
 * source the slays being copied
 */
void copy_slay(struct slay **dest, struct slay *source)
{
    struct slay *s = source;

    while (s)
    {
        struct slay *os = mem_zalloc(sizeof(*os));

        os->name = string_make(s->name);
        os->race_flag = s->race_flag;
        os->multiplier = s->multiplier;
        os->next = *dest;
        *dest = os;
        s = s->next;
    }
}


/*
 * Copy all the brands from one structure to another
 *
 * dest the address the brands are going to
 * source the brands being copied
 */
void copy_brand(struct brand **dest, struct brand *source)
{
    struct brand *b = source;

    while (b)
    {
        struct brand *ob = mem_zalloc(sizeof(*ob));

        ob->name = string_make(b->name);
        ob->element = b->element;
        ob->multiplier = b->multiplier;
        ob->next = *dest;
        *dest = ob;
        b = b->next;
    }
}


/*
 * Free all the slays in a structure
 *
 * source the slays being freed
 */
void free_slay(struct slay *source)
{
    struct slay *s = source, *s_next;

    while (s)
    {
        s_next = s->next;
        string_free(s->name);
        mem_free(s);
        s = s_next;
    }
}


/*
 * Free all the brands in a structure
 *
 * source the brands being freed
 */
void free_brand(struct brand *source)
{
    struct brand *b = source, *b_next;

    while (b)
    {
        b_next = b->next;
        string_free(b->name);
        mem_free(b);
        b = b_next;
    }
}


bool append_fixed_brand(struct brand **dest, int element, int mult)
{
    struct brand *b;

    if (element == -1) return false;

    /* Check for existence */
    for (b = *dest; b; b = b->next)
    {
        /* If we get the same one, check the multiplier */
        if (b->element == element)
        {
            /* Same multiplier or smaller, fail */
            if (b->multiplier >= mult) return false;

            /* Greater multiplier, increase and accept */
            b->multiplier = mult;
            return true;
        }
    }

    /* Add brand */
    b = mem_zalloc(sizeof(*b));
    b->name = string_make(brand_names[element].name);
    b->element = element;
    b->multiplier = mult;
    b->next = *dest;
    *dest = b;

    return true;
}


/*
 * Append a random brand, currently to a randart
 *
 * current the list of brands the object already has
 */
bool append_random_brand(struct brand **current)
{
    return append_fixed_brand(current, randint0(N_ELEMENTS(brand_names)), 2 + randint0(2));
}


bool append_fixed_slay(struct slay **dest, const char *name, int race_flag, int multiplier)
{
    struct slay *s;

    /* Check for existence */
    for (s = *dest; s; s = s->next)
    {
        /* If we get the same race, check the multiplier */
        if (streq(s->name, name) && (s->race_flag == race_flag))
        {
            /* Same multiplier or smaller, fail */
            if (multiplier <= s->multiplier) return false;

            /* Greater multiplier, increase and accept */
            s->multiplier = multiplier;
            return true;
        }
    }

    /* Add slay */
    s = mem_zalloc(sizeof(*s));
    s->name = string_make(name);
    s->race_flag = race_flag;
    s->multiplier = multiplier;
    s->next = *dest;
    *dest = s;

    return true;
}


/*
 * Append a random slay, currently to a randart
 *
 * art is the randart
 * melee is whether the item is a weapon or not
 */
bool append_random_slay(struct artifact *art, bool melee)
{
    int pick;
    struct slay *s;

    pick = randint0(N_ELEMENTS(slay_names));
    for (s = art->slays; s; s = s->next)
    {
        /* If we get the same race, check the multiplier */
        if (streq(s->name, slay_names[pick].name) && (s->race_flag == slay_names[pick].race_flag))
        {
            /* Same multiplier or smaller, fail */
            if (slay_names[pick].multiplier <= s->multiplier) return false;

            /* Greater multiplier, increase and accept */
            s->multiplier = slay_names[pick].multiplier;

            /* PWMAngband: chance of ESP_XXX */
            if (melee && one_in_(slay_names[pick].esp_chance))
                of_on(art->flags, slay_names[pick].esp_flag);

            return true;
        }
    }

    /* We can add the new one now */
    s = mem_zalloc(sizeof(*s));
    s->name = string_make(slay_names[pick].name);
    s->race_flag = slay_names[pick].race_flag;
    s->multiplier = slay_names[pick].multiplier;
    s->next = art->slays;
    art->slays = s;

    /* PWMAngband: chance of ESP_XXX */
    if (melee && one_in_(slay_names[pick].esp_chance))
        of_on(art->flags, slay_names[pick].esp_flag);

    return true;
}


/*
 * Count the brands in a struct brand
 */
int brand_count(struct brand *brands)
{
    int count = 0;
    struct brand *b;

    /* Count the brands */
    for (b = brands; b; b = b->next) count++;

    return count;
}


/*
 * Count the slays in a struct slay
 */
int slay_count(struct slay *slays)
{
    int count = 0;
    struct slay *s;

    /* Count the slays */
    for (s = slays; s; s = s->next) count++;

    return count;
}


/*
 * Collect the brands from a set of brands and an object into a linked array
 *
 * b the set of brands (can be NULL)
 * obj the object (can be NULL)
 *
 * Returns a pointer to the first brand.
 */
struct brand *brand_collect(struct brand *b, const struct object *obj)
{
    bool moved = false;
    struct brand *b_new = NULL;

    /* Use the object if there are no given brands */
    if (!b && obj)
    {
        b = obj->brands;
        moved = true;
    }

    /* Allocate and populate */
    while (b)
    {
        /* Fill in the data */
        append_fixed_brand(&b_new, b->element, b->multiplier);

        /* Move to the next brand */
        b = b->next;

        /* Move to the object if we're done with the given brands */
        if (!b && !moved && obj)
        {
            b = obj->brands;
            moved = true;
        }
    }

    return b_new;
}


/*
 * Collect the slays from a set of slays and an object into a linked array
 *
 * s the set of slays (can be NULL)
 * obj the object (can be NULL)
 *
 * Returns a pointer to the first slay.
 */
struct slay *slay_collect(struct slay *s, const struct object *obj)
{
    bool moved = false;
    struct slay *s_new = NULL;

    /* Use the object if there are no given slays */
    if (!s && obj)
    {
        s = obj->slays;
        moved = true;
    }

    /* Allocate and populate */
    while (s)
    {
        /* Fill in the data */
        append_fixed_slay(&s_new, s->name, s->race_flag, s->multiplier);

        /* Move to the next slay */
        s = s->next;

        /* Move to the object if we're done with the given slays */
        if (!s && !moved && obj)
        {
            s = obj->slays;
            moved = true;
        }
    }

    return s_new;
}


/*
 * React to slays which hurt a monster
 *
 * slay is the slay we're testing for effectiveness
 * race is the race of the target we're testing for being slain
 */
static bool react_to_specific_slay(struct slay *slay, const struct monster_race *race)
{
    if (!slay->name) return false;
    if (!race->base) return false;

    /* Check the race flag */
    if (rf_has(race->flags, slay->race_flag)) return true;

    /* Check for monster base */
    if (streq(slay->name, race->base->name)) return true;

    return false;
}


static bool has_brand(struct brand *brands, struct brand *brand)
{
    struct brand *b;

    for (b = brands; b; b = b->next)
    {
        if (streq(b->name, brand->name) && (b->element == brand->element) &&
            (b->multiplier == brand->multiplier)) return true;
    }

    return false;
}


/*
 * Notice any brands on a particular object which affect a particular monster race.
 *
 * obj is the object on which we are noticing brands
 * race is the race of the target we are hitting, if there is one
 */
void object_notice_brands(struct player *p, struct object *obj, const struct monster_race *race)
{
    char o_name[NORMAL_WID];
    struct brand *b;
    bool plural = ((obj->number > 1)? true: false);

    for (b = obj->brands; b; b = b->next)
    {
        const char *verb = (plural? brand_names[b->element].active_verb_plural:
            brand_names[b->element].active_verb);

        /* Already know it */
        if (has_brand(obj->known->brands, b)) continue;

        /* Not applicable */
        if (race && rf_has(race->flags, brand_names[b->element].resist_flag))
            continue;

        /* Copy over the new known brand */
        append_fixed_brand(&obj->known->brands, b->element, b->multiplier);

        /* Notice */
        object_notice_ego(p, obj);
        if (plural)
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE | ODESC_PLURAL);
        else
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE | ODESC_SINGULAR);
        msg(p, "Your %s %s!", o_name, verb);
    }

    object_check_for_ident(p, obj);
}


static bool has_slay(struct slay *slays, struct slay *slay)
{
    struct slay *s;

    for (s = slays; s; s = s->next)
    {
        if (streq(s->name, slay->name) && (s->race_flag == slay->race_flag) &&
            (s->multiplier == slay->multiplier)) return true;
    }

    return false;
}


/*
 * Notice any slays on a particular object which affect a particular monster race.
 *
 * obj is the object on which we are noticing slays
 * race is the race of the target we are trying to slay
 */
void object_notice_slays(struct player *p, struct object *obj, const struct monster_race *race)
{
    char o_name[NORMAL_WID];
    struct slay *s;

    for (s = obj->slays; s; s = s->next)
    {
        /* Already know it */
        if (has_slay(obj->known->slays, s)) continue;

        /* Not applicable */
        if (!react_to_specific_slay(s, race)) continue;

        /* Copy over the new known slay */
        append_fixed_slay(&obj->known->slays, s->name, s->race_flag, s->multiplier);

        /* Notice */
        object_notice_ego(p, obj);
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE | ODESC_SINGULAR);
        msg(p, "Your %s glows%s!", o_name, ((s->multiplier > 3)? " brightly": ""));
    }

    object_check_for_ident(p, obj);
}


static void improve_attack_modifier_aux(struct player *p, struct object *obj,
    struct monster_race *race, struct monster_lore *lore, bool ml, int element, int multiplier,
    int *best_mult, bool *do_poison, char *verb, size_t len, bool range, bool real)
{
    if (element == -1) return;

    /* If the monster is vulnerable, record and learn from real attacks */
    if (!rf_has(race->flags, brand_names[element].resist_flag))
    {
        /* Hack -- double damage if vulnerable to fire or cold */
        if ((rf_has(race->flags, RF_HURT_FIRE) && (element == ELEM_FIRE)) ||
            (rf_has(race->flags, RF_HURT_COLD) && (element == ELEM_COLD)))
        {
            multiplier *= 2;
        }

        if ((*best_mult < multiplier) || ((*best_mult == multiplier) && (element == ELEM_POIS)))
        {
            *best_mult = multiplier;

            if (multiplier > 3)
            {
                if (range)
                    my_strcpy(verb, "deeply ", len);
                else
                    my_strcpy(verb, "fiercely ", len);
                if (multiplier == 4)
                    my_strcat(verb, brand_names[element].melee_verb_weak, len);
                else
                    my_strcat(verb, brand_names[element].melee_verb, len);
            }
            else if (multiplier < 3)
                my_strcpy(verb, brand_names[element].melee_verb_weak, len);
            else
                my_strcpy(verb, brand_names[element].melee_verb, len);
            if (range)
                my_strcat(verb, "s", len);

            /* Hack -- poison */
            *do_poison = (element == ELEM_POIS);
        }
        if (real)
        {
            if (obj) object_notice_brands(p, obj, race);

            if (ml)
                rf_on(lore->flags, brand_names[element].resist_flag);
        }
    }
}


static void equip_notice_flags(struct player *p, int element)
{
    if (element == ELEM_ELEC) equip_notice_element(p, ELEM_ELEC);
    if (element == ELEM_COLD) equip_notice_element(p, ELEM_COLD);
    if (element == ELEM_FIRE) equip_notice_element(p, ELEM_FIRE);
    if (element == ELEM_ACID) equip_notice_element(p, ELEM_ACID);
    if (element == ELEM_POIS) equip_notice_element(p, ELEM_POIS);
}


/*
 * Extract the multiplier from a given object hitting a given target.
 *
 * p is the current player
 * obj is the object being used to attack
 * who is the is the monster (or player) being attacked
 * best_mult is the best applicable multiplier
 * do_poison is true if the best brand is poison
 * verb is the verb used in the attack ("smite", etc)
 * len is the size of the verb
 * range should be true for ranged attacks
 * real is whether this is a real attack (where we update lore) or a simulation (where we don't)
 */
void improve_attack_modifier(struct player *p, struct object *obj, struct actor *who,
    int *best_mult, bool *do_poison, char *verb, size_t len, bool range, bool real)
{
    struct monster_lore *lore = NULL;
    struct brand *b;
    struct slay *s;
    struct monster_race r_local;
    struct monster_race *race;
    bool ml = false;

    /* Monster info */
    if (who->mon)
    {
        race = who->mon->race;
        if (real)
        {
            lore = get_lore(p, who->mon->race);
            ml = mflag_has(p->mflag[who->idx], MFLAG_VISIBLE);
        }
    }

    /* Player info */
    else if (who->player)
    {
        race = &r_local;
        memset(race, 0, sizeof(struct monster_race));

        /* Handle polymorphed players */
        if (who->player->poly_race)
        {
            /* Create a copy of the monster race (player) */
            memcpy(race, who->player->poly_race, sizeof(struct monster_race));
        }

        /* Handle special races */
        if (player_has(who->player, PF_ORC)) rf_on(race->flags, RF_ORC);
        if (player_has(who->player, PF_TROLL)) rf_on(race->flags, RF_TROLL);
        if (player_has(who->player, PF_GIANT)) rf_on(race->flags, RF_GIANT);
        if (player_has(who->player, PF_THUNDERLORD) || player_has(who->player, PF_DRAGON))
            rf_on(race->flags, RF_DRAGON);
        if (player_has(who->player, PF_ANIMAL)) rf_on(race->flags, RF_ANIMAL);

        /* Handle resistances/immunities */
        if (player_resists(who->player, ELEM_ACID)) rf_on(race->flags, RF_IM_ACID);
        if (player_resists(who->player, ELEM_ELEC)) rf_on(race->flags, RF_IM_ELEC);
        if (player_resists(who->player, ELEM_FIRE)) rf_on(race->flags, RF_IM_FIRE);
        if (player_resists(who->player, ELEM_COLD)) rf_on(race->flags, RF_IM_COLD);
        if (player_resists(who->player, ELEM_POIS)) rf_on(race->flags, RF_IM_POIS);
    }

    /* Brands */
    if (obj)
    {
        for (b = obj->brands; b; b = b->next)
        {
            /* Notice flags for players */
            if (who->player) equip_notice_flags(who->player, b->element);

            improve_attack_modifier_aux(p, obj, race, lore, ml, b->element, b->multiplier,
                best_mult, do_poison, verb, len, range, real);

            /* Attack is real, learn about the monster */
            if (ml && real)
                rf_on(lore->flags, brand_names[b->element].resist_flag);
        }
    }

    /* Handle polymorphed players */
    else if (!range && p->poly_race)
    {
        int element = get_poly_brand(p->poly_race, randint0(z_info->mon_blows_max));

        /* Notice flags for players */
        if (who->player) equip_notice_flags(who->player, element);

        improve_attack_modifier_aux(p, NULL, race, lore, ml, element, 3, best_mult, do_poison,
            verb, len, range, real);

        /* Attack is real, learn about the monster */
        if ((element != -1) && ml && real)
            rf_on(lore->flags, brand_names[element].resist_flag);
    }

    /* Hack -- extract temp branding */
    else if (range && p->timed[TMD_BOWBRAND])
    {
        int element = get_bow_brand(&p->brand);

        /* Notice flags for players */
        if (who->player) equip_notice_flags(who->player, element);

        improve_attack_modifier_aux(p, NULL, race, lore, ml, element, 3, best_mult, do_poison,
            verb, len, range, real);

        /* Attack is real, learn about the monster */
        if ((element != -1) && ml && real)
            rf_on(lore->flags, brand_names[element].resist_flag);
    }

    /* Hack -- extract temp branding */
    else if (!range && p->timed[TMD_SGRASP])
    {
        /* Notice flags for players */
        if (who->player) equip_notice_flags(who->player, ELEM_ELEC);

        improve_attack_modifier_aux(p, NULL, race, lore, ml, ELEM_ELEC, 3, best_mult,
            do_poison, verb, len, range, real);

        /* Attack is real, learn about the monster */
        if (ml && real)
            rf_on(lore->flags, RF_IM_ELEC);
    }

    /* Slays */
    if (obj)
    {
        for (s = obj->slays; s; s = s->next)
        {
            /* If the monster is vulnerable, record and learn from real attacks */
            if (react_to_specific_slay(s, race))
            {
                if (*best_mult < s->multiplier)
                {
                    *best_mult = s->multiplier;

                    if (range)
                    {
                        if (s->multiplier <= 3)
                            my_strcpy(verb, "pierces", len);
                        else
                            my_strcpy(verb, "deeply pierces", len);
                    }
                    else
                    {
                        if (s->multiplier <= 3)
                            my_strcpy(verb, "smite", len);
                        else
                            my_strcpy(verb, "fiercely smite", len);
                    }
                }
                if (real)
                {
                    object_notice_slays(p, obj, race);

                    if (ml)
                        rf_on(lore->flags, s->race_flag);
                }
            }

            /* Attack is real, learn about the monster */
            if (ml && real)
                rf_on(lore->flags, s->race_flag);
        }
    }
}


/*
 * React to slays which hurt a monster
 *
 * obj is the object we're testing for slays
 * mon is the monster we're testing for being slain
 */
bool react_to_slay(struct object *obj, const struct monster *mon)
{
    struct slay *s;

    for (s = obj->slays; s; s = s->next)
    {
        if (react_to_specific_slay(s, mon->race)) return true;
    }

    return false;
}


/*
 * Determine whether two lists of brands are the same
 *
 * brand1, brand2 the lists being compared
 */
bool brands_are_equal(struct brand *brand1, struct brand *brand2)
{
    struct brand *b = brand1;
    int count = 0, match = 0;

    while (b)
    {
        count++;

        /* Count if the same */
        if (has_brand(brand2, b)) match++;

        /* Fail if we didn't find a match */
        if (match != count) return false;

        b = b->next;
    }

    /* Now count back and make sure brand2 isn't strictly bigger */
    b = brand2;
    while (b)
    {
        count--;
        b = b->next;
    }

    if (count != 0) return false;
    return true;
}


/*
 * Determine whether two lists of slays are the same
 *
 * slay1, slay2 the lists being compared
 */
bool slays_are_equal(struct slay *slay1, struct slay *slay2)
{
    struct slay *s = slay1;
    int count = 0, match = 0;

    while (s)
    {
        count++;

        /* Count if the same */
        if (has_slay(slay2, s)) match++;

        /* Fail if we didn't find a match */
        if (match != count) return false;

        s = s->next;
    }

    /* Now count back and make sure slay2 isn't strictly bigger */
    s = slay2;
    while (s)
    {
        count--;
        s = s->next;
    }

    if (count != 0) return false;
    return true;
}


/*
 * Check the slay cache for a combination of slays and brands
 *
 * obj is the object to check
 * known is whether the slays should be known or not
 *
 * Returns the power value of the combination.
 */
s32b check_slay_cache(const struct object *obj, bool known)
{
    struct slay_cache *cache_entry;
    struct brand *brands = (known? obj->brands: obj->known->brands);
    struct slay *slays = (known? obj->slays: obj->known->slays);

    cache_index = 0;
    cache_entry = &slay_cache[cache_index];

    while (cache_entry->brands || cache_entry->slays)
    {
        if (brands_are_equal(brands, cache_entry->brands) &&
            slays_are_equal(slays, cache_entry->slays)) break;
        cache_index++;
        cache_entry = &slay_cache[cache_index];
    }

    return slay_cache[cache_index].value;
}


/*
 * Fill in a value in the slay cache.
 *
 * obj is the object the combination is on
 * known is whether the slays should be known or not
 * value is the value of the slay flags on the object
 */
void fill_slay_cache(const struct object *obj, bool known, s32b value)
{
    int i;
    struct brand *brands = (known? obj->brands: obj->known->brands);
    struct slay *slays = (known? obj->slays: obj->known->slays);

    /* Expand the cache if necessary */
    if (cache_index == cache_size)
    {
        cache_size += CACHE_INCREMENT;
        slay_cache = mem_realloc(slay_cache, (cache_size + 1) * sizeof(struct slay_cache));
        for (i = cache_index + 1; i <= cache_size; i++)
            memset(&slay_cache[i], 0, sizeof(struct slay_cache));
    }

    /* Add to the cache */
    copy_brand(&slay_cache[cache_index].brands, brands);
    copy_slay(&slay_cache[cache_index].slays, slays);
    slay_cache[cache_index].value = value;
}


/*
 * Create a cache of slay/brand combinations, and the values of
 * these combinations. This is to speed up slay_power(), which will be called
 * many times during the game.
 */
void create_slay_cache(void)
{
    cache_size = CACHE_INCREMENT;
    slay_cache = mem_zalloc((cache_size + 1) * sizeof(struct slay_cache));
}


/*
 * Free the slay cache
 */
void free_slay_cache(void)
{
    int i;

    for (i = 0; i <= cache_size; i++)
    {
        free_brand(slay_cache[i].brands);
        free_slay(slay_cache[i].slays);
    }
    mem_free(slay_cache);
}


int get_poly_brand(struct monster_race *race, int method)
{
    /* No special attacks */
    if (!race->blow[method].method) return -1;

    /* Branded attacks give similar brand */
    switch (race->blow[method].effect)
    {
        case RBE_POISON:
        case RBE_DISEASE: return ELEM_POIS;
        case RBE_ACID: return ELEM_ACID;
        case RBE_ELEC: return ELEM_ELEC;
        case RBE_FIRE: return ELEM_FIRE;
        case RBE_COLD: return ELEM_COLD;
    }

    return -1;
}


int get_bow_brand(struct bow_brand *brand)
{
    if (brand->blast) return -1;

    switch (brand->type)
    {
        case GF_ELEC: return ELEM_ELEC;
        case GF_COLD: return ELEM_COLD;
        case GF_FIRE: return ELEM_FIRE;
        case GF_ACID: return ELEM_ACID;
        case GF_POIS: return ELEM_POIS;
    }

    return -1;
}
