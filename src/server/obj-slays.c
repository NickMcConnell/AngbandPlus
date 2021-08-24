/*
 * File: obj-slays.c
 * Purpose: Functions for manipulating slays/brands
 *
 * Copyright (c) 2010 Chris Carr and Peter Denison
 * Copyright (c) 2014 Nick McConnell
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


struct slay *slays;
struct brand *brands;


/*
 * Check if two slays affect the same set of monsters
 */
bool same_monsters_slain(int slay1, int slay2)
{
    if (slays[slay1].race_flag != slays[slay2].race_flag) return false;
    if (!slays[slay1].base && !slays[slay2].base) return true;
    if (slays[slay1].base && !slays[slay2].base) return false;
    if (!slays[slay1].base && slays[slay2].base) return false;
    if (streq(slays[slay1].base, slays[slay2].base)) return true;
    return false;
}


bool append_slay(bool **current, int index)
{
    int i;

    /* No existing slays means OK to add */
    if (!(*current))
    {
        *current = mem_zalloc(z_info->slay_max * sizeof(bool));
        (*current)[index] = true;
        return true;
    }

    /* Check the existing slays for base/flag matches */
    for (i = 0; i < z_info->slay_max; i++)
    {
        if (!(*current)[i]) continue;

        /* If we get the same one, check the multiplier */
        if (streq(slays[i].name, slays[index].name) &&
            (slays[i].race_flag == slays[index].race_flag))
        {
            /* Same multiplier or smaller, fail */
            if (slays[index].multiplier <= slays[i].multiplier) return false;

            /* Greater multiplier, replace and accept */
            (*current)[i] = false;
            (*current)[index] = true;

            return true;
        }
    }

    /* We can add the new one now */
    (*current)[index] = true;

    return true;
}


/*
 * Add all the slays from one structure to another
 *
 * dest the address the slays are going to
 * source the slays being copied
 */
bool copy_slays(bool **dest, bool *source)
{
    int i;
    bool result = false;

    /* Check structures */
    if (!source) return false;

    /* Copy */
    for (i = 0; i < z_info->slay_max; i++)
    {
        if (!source[i]) continue;
        if (append_slay(dest, i)) result = true;
    }

    return result;
}


bool append_brand(bool **current, int index)
{
    int i;

    if (index == -1) return false;

    /* No existing brands means OK to add */
    if (!(*current))
    {
        *current = mem_zalloc(z_info->brand_max * sizeof(bool));
        (*current)[index] = true;
        return true;
    }

    /* Check the existing brands for name matches */
    for (i = 0; i < z_info->brand_max; i++)
    {
        if (!(*current)[i]) continue;

        /* If we get the same one, check the multiplier */
        if (streq(brands[i].name, brands[index].name))
        {
            /* Same multiplier or smaller, fail */
            if (brands[index].multiplier <= brands[i].multiplier) return false;

            /* Greater multiplier, replace and accept */
            (*current)[i] = false;
            (*current)[index] = true;

            return true;
        }
    }

    /* We can add the new one now */
    (*current)[index] = true;

    return true;
}


/*
 * Add all the brands from one structure to another
 *
 * dest the address the brands are going to
 * source the brands being copied
 */
bool copy_brands(bool **dest, bool *source)
{
    int i;
    bool result = false;

    /* Check structures */
    if (!source) return false;

    /* Copy */
    for (i = 0; i < z_info->brand_max; i++)
    {
        if (!source[i]) continue;
        if (append_brand(dest, i)) result = true;
    }

    return result;
}


/*
 * Append a random brand, currently to a randart
 *
 * current the list of brands the object already has
 */
bool append_random_brand(bool **current, struct brand **brand, bool is_ammo)
{
    int pick;

    /* No life leech brand on ammo */
    do
    {
        pick = randint0(z_info->brand_max);
        *brand = &brands[pick];
    }
    while (is_ammo && streq((*brand)->name, "life leech"));

    return append_brand(current, pick);
}


/*
 * Append a random slay, currently to a randart
 *
 * current the list of slays the object already has
 */
bool append_random_slay(bool **current, struct slay **slay)
{
    int pick = randint0(z_info->slay_max);

    *slay = &slays[pick];
    return append_slay(current, pick);
}


/*
 * Count a set of brands
 */
int brand_count(bool *local_brands)
{
    int i, count = 0;

    /* Count the brands */
    for (i = 0; local_brands && (i < z_info->brand_max); i++)
    {
        if (local_brands[i]) count++;
    }

    return count;
}


/*
 * Count a set of slays
 */
int slay_count(bool *local_slays)
{
    int i, count = 0;

    /* Count the slays */
    for (i = 0; local_slays && (i < z_info->slay_max); i++)
    {
        if (local_slays[i]) count++;
    }

    return count;
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
    if (slay->base && streq(slay->base, race->base->name)) return true;

    return false;
}


/*
 * Notice a brand on a particular object which affects a particular monster race.
 *
 * obj is the object on which we are noticing brands
 * b is the brand we are learning
 */
static void object_notice_brand(struct player *p, struct object *obj, int i)
{
    char o_name[NORMAL_WID];
    bool plural = ((obj->number > 1)? true: false);
    const char *verb = (plural? brands[i].active_verb_plural: brands[i].active_verb);

    /* Already know it */
    if (obj->known->brands && obj->known->brands[i]) return;

    /* Copy over the new known brand */
    append_brand(&obj->known->brands, i);

    /* Notice */
    object_notice_ego(p, obj);
    if (plural)
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE | ODESC_PLURAL);
    else
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE | ODESC_SINGULAR);
    msg(p, "Your %s %s!", o_name, verb);

    /* Learn about the brand */
    object_learn_brand(p, i);

    object_check_for_ident(p, obj);
}


/*
 * Notice a slay on a particular object which affects a particular monster race.
 *
 * obj is the object on which we are noticing slays
 * s is the slay we are learning
 */
static void object_notice_slay(struct player *p, struct object *obj, int i)
{
    char o_name[NORMAL_WID];

    /* Already know it */
    if (obj->known->slays && obj->known->slays[i]) return;

    /* Copy over the new known slay */
    append_slay(&obj->known->slays, i);

    /* Notice */
    object_notice_ego(p, obj);
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE | ODESC_SINGULAR);
    msg(p, "Your %s glows%s!", o_name, ((slays[i].multiplier > 3)? " brightly": ""));

    /* Learn about the slay */
    object_learn_slay(p, i);

    object_check_for_ident(p, obj);
}


static void improve_attack_modifier_brand(struct player *p, struct object *obj, struct source *who,
    int i, int *best_mult, struct side_effects *effects, char *verb, size_t len, bool range)
{
    struct brand *b = &brands[i];
    struct monster_lore *lore = NULL;
    struct monster_race r_local;
    struct monster_race *race = NULL;
    bool ml = false;
    int multiplier = b->multiplier;

    /* Monster info */
    if (who->monster)
    {
        race = who->monster->race;
        lore = get_lore(p, who->monster->race);
        ml = monster_is_visible(p, who->idx);
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

        /* Handle resistances/immunities */
        if (player_resists(who->player, ELEM_ACID)) rf_on(race->flags, RF_IM_ACID);
        if (player_resists(who->player, ELEM_ELEC)) rf_on(race->flags, RF_IM_ELEC);
        if (player_resists(who->player, ELEM_FIRE)) rf_on(race->flags, RF_IM_FIRE);
        if (player_resists(who->player, ELEM_COLD)) rf_on(race->flags, RF_IM_COLD);
        if (player_resists(who->player, ELEM_POIS)) rf_on(race->flags, RF_IM_POIS);
    }

    /* Is the monster vulnerable? */
    if (!b->resist_flag || !rf_has(race->flags, b->resist_flag))
    {
        bool random_effect = false;

        /* Hack -- double damage if vulnerable to fire or cold */
        if ((rf_has(race->flags, RF_HURT_FIRE) && streq(b->name, "fire")) ||
            (rf_has(race->flags, RF_HURT_COLD) && streq(b->name, "cold")))
        {
            multiplier *= 2;
        }

        /* Hack -- status effect */
        if (*best_mult < multiplier) effects->count = 0;
        if (streq(b->name, "poison") || streq(b->name, "stunning") || streq(b->name, "cutting") ||
            streq(b->name, "life leech"))
        {
            if (*best_mult <= multiplier) effects->count++;

            /* Choose randomly when poison, stunning, cutting or life leech are available */
            if (*best_mult == multiplier) random_effect = one_in_(effects->count);
        }

        /* Record the best multiplier */
        if ((*best_mult < multiplier) || random_effect)
        {
            *best_mult = multiplier;

            if (multiplier > 3)
            {
                if (range)
                    my_strcpy(verb, "deeply ", len);
                else
                    my_strcpy(verb, "fiercely ", len);
                my_strcat(verb, b->verb, len);
            }
            else
                my_strcpy(verb, b->verb, len);
            if (range)
                my_strcat(verb, "s", len);

            /* Hack -- poison, stun, cut */
            effects->do_poison = streq(b->name, "poison");
            effects->do_stun = (streq(b->name, "stunning")? 1: 0);
            effects->do_cut = (streq(b->name, "cutting")? 1: 0);
            effects->do_leech = (streq(b->name, "life leech")? 1: 0);
        }

        /* Learn about the brand */
        if (obj) object_notice_brand(p, obj, i);

        /* Learn about the monster */
        if (b->resist_flag && ml) rf_on(lore->flags, b->resist_flag);
    }

    /* Learn about resistant monsters */
    else if (b->resist_flag && player_knows_brand(p, i) && ml)
        rf_on(lore->flags, b->resist_flag);
}


static void improve_attack_modifier_slay(struct player *p, struct object *obj, struct source *who,
    int i, int *best_mult, char *verb, size_t len, bool range)
{
    struct slay *s = &slays[i];
    struct monster_lore *lore = NULL;
    struct monster_race r_local;
    struct monster_race *race = NULL;
    bool ml = false;

    /* Monster info */
    if (who->monster)
    {
        race = who->monster->race;
        lore = get_lore(p, who->monster->race);
        ml = monster_is_visible(p, who->idx);
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
    }

    /* Is the monster vulnerable? */
    if (react_to_specific_slay(s, race))
    {
        /* Record the best multiplier */
        if (*best_mult < s->multiplier)
        {
            *best_mult = s->multiplier;

            if (range)
                my_strcpy(verb, s->range_verb, len);
            else
                my_strcpy(verb, s->melee_verb, len);
        }

        /* Learn about the slay */
        if (obj) object_notice_slay(p, obj, i);

        /* Learn about the monster */
        if (ml) rf_on(lore->flags, s->race_flag);
    }

    /* Learn about resistant monsters */
    else if (player_knows_slay(p, i) && ml)
        rf_on(lore->flags, s->race_flag);
}


static void equip_notice_flags(struct player *p, int index)
{
    if (streq(brands[index].name, "lightning")) equip_learn_element(p, ELEM_ELEC);
    if (streq(brands[index].name, "cold")) equip_learn_element(p, ELEM_COLD);
    if (streq(brands[index].name, "fire")) equip_learn_element(p, ELEM_FIRE);
    if (streq(brands[index].name, "acid")) equip_learn_element(p, ELEM_ACID);
    if (streq(brands[index].name, "poison")) equip_learn_element(p, ELEM_POIS);
}


/*
 * Player has a temporary brand
 */
bool player_has_temporary_brand(struct player *p, int idx)
{
    if (p->timed[TMD_ATT_ACID] && streq(brands[idx].code, "ACID_3")) return true;
    if (p->timed[TMD_ATT_ELEC] && streq(brands[idx].code, "ELEC_3")) return true;
    if (p->timed[TMD_ATT_FIRE] && streq(brands[idx].code, "FIRE_3")) return true;
    if (p->timed[TMD_ATT_COLD] && streq(brands[idx].code, "COLD_3")) return true;
    if (p->timed[TMD_ATT_POIS] && streq(brands[idx].code, "POIS_3")) return true;
    return false;
}


/*
 * Player has a temporary slay
 */
bool player_has_temporary_slay(struct player *p, int idx)
{
    if (p->timed[TMD_ATT_EVIL] && streq(slays[idx].code, "EVIL_2")) return true;
    if (p->timed[TMD_ATT_DEMON] && streq(slays[idx].code, "DEMON_5")) return true;
    return false;
}


/*
 * Extract the multiplier from a given object hitting a given target.
 *
 * p is the current player
 * obj is the object being used to attack
 * who is the is the monster (or player) being attacked
 * best_mult is the best applicable multiplier
 * effects tells if the best brand is poison, stun or cut
 * verb is the verb used in the attack ("smite", etc)
 * len is the size of the verb
 * range should be true for ranged attacks
 */
void improve_attack_modifier(struct player *p, struct object *obj, struct source *who,
    int *best_mult, struct side_effects *effects, char *verb, size_t len, bool range)
{
    int i;

    /* Slays */
    for (i = 0; obj && obj->slays && (i < z_info->slay_max); i++)
    {
        if (!obj->slays[i]) continue;
        improve_attack_modifier_slay(p, obj, who, i, best_mult, verb, len, range);
    }

    /* Brands */
    for (i = 0; obj && obj->brands && (i < z_info->brand_max); i++)
    {
        if (!obj->brands[i]) continue;

        /* Notice flags for players */
        if (who->player) equip_notice_flags(who->player, i);

        improve_attack_modifier_brand(p, obj, who, i, best_mult, effects, verb, len, range);
    }

    if (obj) return;

    /* Temporary branding (ranged) */
    if (range)
    {
        if (p->timed[TMD_BOWBRAND])
        {
            int index = get_bow_brand(&p->brand);

            if (index != -1)
            {
                /* Notice flags for players */
                if (who->player) equip_notice_flags(who->player, index);

                improve_attack_modifier_brand(p, NULL, who, index, best_mult, effects, verb, len,
                    range);
            }
        }

        return;
    }

    /* Handle racial/class slays */
    for (i = 0; i < z_info->slay_max; i++)
    {
        if (p->race->slays && p->race->slays[i].slay && (p->lev >= p->race->slays[i].lvl))
            improve_attack_modifier_slay(p, NULL, who, i, best_mult, verb, len, range);
        if (p->clazz->slays && p->clazz->slays[i].slay && (p->lev >= p->clazz->slays[i].lvl))
            improve_attack_modifier_slay(p, NULL, who, i, best_mult, verb, len, range);
    }

    /* Handle racial/class brands */
    for (i = 0; i < z_info->brand_max; i++)
    {
        if (p->race->brands && p->race->brands[i].brand && (p->lev >= p->race->brands[i].lvl))
        {
            /* Notice flags for players */
            if (who->player) equip_notice_flags(who->player, i);

            improve_attack_modifier_brand(p, NULL, who, i, best_mult, effects, verb, len, range);
        }
        if (p->clazz->brands && p->clazz->brands[i].brand && (p->lev >= p->clazz->brands[i].lvl))
        {
            /* Notice flags for players */
            if (who->player) equip_notice_flags(who->player, i);

            improve_attack_modifier_brand(p, NULL, who, i, best_mult, effects, verb, len, range);
        }
    }

    /* Handle polymorphed players */
    if (p->poly_race)
    {
        int index = get_poly_brand(p->poly_race, randint0(z_info->mon_blows_max));

        if (index != -1)
        {
            /* Notice flags for players */
            if (who->player) equip_notice_flags(who->player, index);

            improve_attack_modifier_brand(p, NULL, who, index, best_mult, effects, verb, len,
                range);
        }
    }

    /* Temporary slays */
    for (i = 0; i < z_info->slay_max; i++)
    {
        if (!player_has_temporary_slay(p, i)) continue;
        improve_attack_modifier_slay(p, NULL, who, i, best_mult, verb, len, range);
    }

    /* Temporary brands */
    for (i = 0; i < z_info->brand_max; i++)
    {
        if (!player_has_temporary_brand(p, i)) continue;

        /* Notice flags for players */
        if (who->player) equip_notice_flags(who->player, i);

        improve_attack_modifier_brand(p, NULL, who, i, best_mult, effects, verb, len, range);
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
    int i;

    for (i = 0; obj->slays && (i < z_info->slay_max); i++)
    {
        struct slay *s = &slays[i];

        if (!obj->slays[i]) continue;
        if (react_to_specific_slay(s, mon->race)) return true;
    }

    return false;
}


/*
 * Check whether two objects have the exact same brands
 *
 * obj1 the first object
 * obj2 the second object
 */
bool brands_are_equal(const struct object *obj1, const struct object *obj2)
{
    int i;

    if (!obj1->brands && !obj2->brands) return true;
    if (obj1->brands && !obj2->brands) return false;
    if (!obj1->brands && obj2->brands) return false;

    for (i = 0; i < z_info->brand_max; i++)
    {
        if (obj1->brands[i] != obj2->brands[i]) return false;
    }

    return true;
}


/*
 * Check whether two objects have the exact same slays
 *
 * obj1 the first object
 * obj2 the second object
 */
bool slays_are_equal(const struct object *obj1, const struct object *obj2)
{
    int i;

    if (!obj1->slays && !obj2->slays) return true;
    if (obj1->slays && !obj2->slays) return false;
    if (!obj1->slays && obj2->slays) return false;

    for (i = 0; i < z_info->slay_max; i++)
    {
        if (obj1->slays[i] != obj2->slays[i]) return false;
    }

    return true;
}


int get_brand(const char *name, int multiplier)
{
    int i;

    for (i = 0; i < z_info->brand_max; i++)
    {
        if (streq(brands[i].name, name) && (brands[i].multiplier == multiplier)) return i;
    }

    return -1;
}


int get_poly_brand(struct monster_race *race, int method)
{
    const char *name;

    /* No special attacks */
    if (!race->blow[method].method) return -1;

    /* Branded attacks give similar brand */
    if (streq(race->blow[method].effect->name, "POISON")) name = "poison";
    else if (streq(race->blow[method].effect->name, "DISEASE")) name = "poison";
    else if (streq(race->blow[method].effect->name, "ACID")) name = "acid";
    else if (streq(race->blow[method].effect->name, "ELEC")) name = "lightning";
    else if (streq(race->blow[method].effect->name, "FIRE")) name = "fire";
    else if (streq(race->blow[method].effect->name, "COLD")) name = "cold";
    else return -1;

    return get_brand(name, 3);
}


int get_bow_brand(struct bow_brand *brand)
{
    const char *name;

    if (brand->blast) return -1;

    switch (brand->type)
    {
        case PROJ_ELEC: name = "lightning"; break;
        case PROJ_COLD: name = "cold"; break;
        case PROJ_FIRE: name = "fire"; break;
        case PROJ_ACID: name = "acid"; break;
        case PROJ_POIS: name = "poison"; break;
        default: return -1;
    }

    return get_brand(name, 3);
}
