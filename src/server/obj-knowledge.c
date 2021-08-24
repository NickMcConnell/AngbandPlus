/*
 * File: obj-knowledge.c
 * Purpose: Object knowledge
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2009 Brian Bull
 * Copyright (c) 2016 Nick McConnell
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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
 * Overview
 * ========
 *
 * This file deals with the new "rune-based ID" system. This system operates
 * as follows:
 * - struct player has an object struct attached to it (obj_k) which contains
 *   the player's knowledge of object properties (runes)
 * - whenever the player learns a rune,
 *     if it's an object flag, that flag is set in obj_k
 *     if it's an integer value, that value in obj_k is set to 1
 *     if it's element info, the res_level value is set to 1
 *     if it's a brand, a brand is added to obj_k with the relevant element
 *     if it's a slay, a slay is added to obj_k with the right race flag or name
 * - every object has a known version which is filled in with details as the
 *   player learns them
 * - whenever the player learns a rune, that knowledge is applied to the known
 *   version of every object that the player has picked up or walked over
 */


/*
 * Object knowledge data
 * This section covers initialisation, access and cleanup of rune data
 */


static size_t rune_max;
static struct rune *rune_list;
static char *c_rune[] =
{
    "enchantment to armor",
    "enchantment to hit",
    "enchantment to damage"
};


/*
 * Initialise the rune module
 */
static void init_rune(void)
{
    int i, j, count;

    /* Count runes (combat runes are fixed) */
    count = COMBAT_RUNE_MAX;
    for (i = 1; i < OF_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_FLAG, i);

        if (prop->subtype == OFT_NONE) continue;
        if (prop->subtype == OFT_LIGHT) continue;
        if (prop->subtype == OFT_DIG) continue;
        count++;
    }
    for (i = 0; i < OBJ_MOD_MAX; i++)
        count++;
    for (i = 0; i <= ELEM_XHIGH_MAX; i++) /* PWMAngband: some items have resist TIME/MANA */
        count++;

    /* Note brand runes cover all brands with the same name */
    for (i = 0; i < z_info->brand_max; i++)
    {
        bool counted = false;

        if (brands[i].name)
        {
            for (j = 0; j < i; j++)
            {
                if (streq(brands[i].name, brands[j].name)) counted = true;
            }
            if (!counted) count++;
        }
    }

    /* Note slay runes cover all slays with the same flag/base */
    for (i = 0; i < z_info->slay_max; i++)
    {
        bool counted = false;

        if (slays[i].name)
        {
            for (j = 0; j < i; j++)
            {
                if (same_monsters_slain(i, j)) counted = true;
            }
            if (!counted) count++;
        }
    }

    for (i = 0; i < z_info->curse_max; i++)
    {
        if (curses[i].name) count++;
    }

    /* Now allocate and fill the rune list */
    rune_max = count;
    rune_list = mem_zalloc(rune_max * sizeof(struct rune));
    count = 0;
    for (i = 0; i < COMBAT_RUNE_MAX; i++)
    {
        rune_list[count].variety = RUNE_VAR_COMBAT;
        rune_list[count].index = i;
        rune_list[count].name = c_rune[i];
        count++;
    }
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_MOD, i);

        rune_list[count].variety = RUNE_VAR_MOD;
        rune_list[count].index = i;
        rune_list[count].name = prop->name;
        count++;
    }
    for (i = 0; i <= ELEM_XHIGH_MAX; i++) /* PWMAngband: some items have resist TIME/MANA */
    {
        rune_list[count].variety = RUNE_VAR_RESIST;
        rune_list[count].index = i;
        rune_list[count].name = projections[i].name;
        count++;
    }
    for (i = 0; i < z_info->brand_max; i++)
    {
        bool counted = false;

        if (brands[i].name)
        {
            for (j = 0; j < i; j++)
            {
                if (streq(brands[i].name, brands[j].name)) counted = true;
            }
            if (!counted)
            {
                rune_list[count].variety = RUNE_VAR_BRAND;
                rune_list[count].index = i;
                rune_list[count].name = brands[i].name;
                count++;
            }
        }
    }
    for (i = 0; i < z_info->slay_max; i++)
    {
        bool counted = false;

        if (slays[i].name)
        {
            for (j = 0; j < i; j++)
            {
                if (same_monsters_slain(i, j)) counted = true;
            }
            if (!counted)
            {
                rune_list[count].variety = RUNE_VAR_SLAY;
                rune_list[count].index = i;
                rune_list[count].name = slays[i].name;
                count++;
            }
        }
    }
    for (i = 0; i < z_info->curse_max; i++)
    {
        if (curses[i].name)
        {
            rune_list[count].variety = RUNE_VAR_CURSE;
            rune_list[count].index = i;
            rune_list[count].name = curses[i].name;
            count++;
        }
    }
    for (i = 1; i < OF_MAX; i++)
    {
        struct obj_property *prop = lookup_obj_property(OBJ_PROPERTY_FLAG, i);

        if (prop->subtype == OFT_NONE) continue;
        if (prop->subtype == OFT_LIGHT) continue;
        if (prop->subtype == OFT_DIG) continue;

        rune_list[count].variety = RUNE_VAR_FLAG;
        rune_list[count].index = i;
        rune_list[count].name = prop->name;
        count++;
    }
}


/*
 * Get a rune by variety and index
 */
static int rune_index(enum rune_variety variety, int index)
{
    size_t i;

    /* Look for the rune */
    for (i = 0; i < rune_max; i++)
    {
        if ((rune_list[i].variety == variety) && (rune_list[i].index == index))
            return i;
    }

    /* Can't find it */
    return -1;
}


/*
 * Cleanup the rune module
 */
static void cleanup_rune(void)
{
    mem_free(rune_list);
}


struct init_module rune_module =
{
    "rune",
    init_rune,
    cleanup_rune
};


/*
 * Rune knowledge functions
 * These functions provide details about the rune list for use in
 * player knowledge screens
 */


/*
 * The number of runes
 */
int max_runes(void)
{
    return rune_max;
}


/*
 * The variety of a rune
 */
enum rune_variety rune_variety(size_t i)
{
    return rune_list[i].variety;
}


/*
 * Reports if the player knows a given rune
 *
 * p is the player
 * i is the rune's number in the rune list
 */
bool player_knows_rune(struct player *p, size_t i)
{
    struct rune *r = &rune_list[i];

    switch (r->variety)
    {
        /* Combat runes */
        case RUNE_VAR_COMBAT:
        {
            if (r->index == COMBAT_RUNE_TO_A)
            {
                if (p->obj_k->to_a) return true;
            }
            else if (r->index == COMBAT_RUNE_TO_H)
            {
                if (p->obj_k->to_h) return true;
            }
            else if (r->index == COMBAT_RUNE_TO_D)
            {
                if (p->obj_k->to_d) return true;
            }
            break;
        }

        /* Mod runes */
        case RUNE_VAR_MOD:
        {
            if (p->obj_k->modifiers[r->index]) return true;
            break;
        }

        /* Element runes */
        case RUNE_VAR_RESIST:
        {
            if (p->obj_k->el_info[r->index].res_level) return true;
            break;
        }

        /* Brand runes */
        case RUNE_VAR_BRAND:
        {
            my_assert(r->index < z_info->brand_max);

            if (player_knows_brand(p, r->index)) return true;
            break;
        }

        /* Slay runes */
        case RUNE_VAR_SLAY:
        {
            my_assert(r->index < z_info->slay_max);

            if (player_knows_slay(p, r->index)) return true;
            break;
        }

        /* Curse runes */
        case RUNE_VAR_CURSE:
        {
            my_assert(r->index < z_info->curse_max);

            if (player_knows_curse(p, r->index)) return true;
            break;
        }

        /* Flag runes */
        case RUNE_VAR_FLAG:
        {
            if (of_has(p->obj_k->flags, r->index)) return true;
            break;
        }

        default: break;
    }

    return false;
}


/*
 * The name of a rune
 */
char *rune_name(size_t i)
{
    struct rune *r = &rune_list[i];

    if (r->variety == RUNE_VAR_BRAND) return format("%s brand", r->name);
    if (r->variety == RUNE_VAR_SLAY) return format("slay %s", r->name);
    if (r->variety == RUNE_VAR_CURSE) return format("%s curse", r->name);
    if (r->variety == RUNE_VAR_RESIST) return format("resist %s", r->name);
    return format("%s", r->name);
}


/*
 * The description of a rune
 */
char *rune_desc(size_t i)
{
    struct rune *r = &rune_list[i];

    switch (r->variety)
    {
        /* Combat runes */
        case RUNE_VAR_COMBAT:
        {
            if (r->index == COMBAT_RUNE_TO_A)
                return "Object magically increases the player's armor class.";
            if (r->index == COMBAT_RUNE_TO_H)
                return "Object magically increases the player's chance to hit.";
            if (r->index == COMBAT_RUNE_TO_D)
                return "Object magically increases the player's damage.";
            break;
        }

        /* Mod runes */
        case RUNE_VAR_MOD:
        {
            return format("Object gives the player a magical bonus to %s.", r->name);
        }

        /* Element runes */
        case RUNE_VAR_RESIST:
        {
            return format("Object affects the player's resistance to %s.", r->name);
        }

        /* Brand runes */
        case RUNE_VAR_BRAND:
        {
            return format("Object brands the player's attacks with %s.", r->name);
        }

        /* Slay runes */
        case RUNE_VAR_SLAY:
        {
            return format("Object makes the player's attacks against %s more powerful.", r->name);
        }

        /* Curse runes */
        case RUNE_VAR_CURSE:
        {
            my_assert(r->index < z_info->curse_max);

            return format("Object %s.", curses[r->index].desc);
        }

        /* Flag runes */
        case RUNE_VAR_FLAG:
        {
            return format("Object gives the player the property of %s.", r->name);
        }

        default: break;
    }

    return NULL;
}


/*
 * Object knowledge predicates
 * These functions tell how much the player knows about an object
 */


/*
 * Check if a brand is known to the player
 *
 * p is the player
 * i is the index of the brand
 */
bool player_knows_brand(struct player *p, int i)
{
    return p->obj_k->brands[i];
}


/*
 * Check if a slay is known to the player
 *
 * p is the player
 * i is the index of the slay
 */
bool player_knows_slay(struct player *p, int i)
{
    return p->obj_k->slays[i];
}


/*
 * Check if a curse is known to the player
 *
 * p is the player
 * index is the curse index
 */
bool player_knows_curse(struct player *p, int index)
{
    return (p->obj_k->curses[index].power == 1);
}


/*
 * Check if an ego item type is known to the player
 *
 * p is the player
 * ego is the ego item type
 */
bool player_knows_ego(struct player *p, struct ego_item *ego)
{
    int i;

    if (!ego) return false;

    /* All flags known */
    if (!of_is_subset(p->obj_k->flags, ego->flags)) return false;

    /* All modifiers known */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (randcalc(ego->modifiers[i], MAX_RAND_DEPTH, MAXIMISE) && !p->obj_k->modifiers[i])
            return false;
    }

    /* All elements known */
    for (i = 0; i < ELEM_MAX; i++)
    {
        if (ego->el_info[i].res_level && !p->obj_k->el_info[i].res_level)
            return false;
    }

    /* All brands known */
    for (i = 0; ego->brands && (i < z_info->brand_max); i++)
    {
        if (ego->brands[i] && !player_knows_brand(p, i)) return false;
    }

    /* All slays known */
    for (i = 0; ego->slays && (i < z_info->slay_max); i++)
    {
        if (ego->slays[i] && !player_knows_slay(p, i)) return false;
    }

    /* All curses known */
    for (i = 0; ego->curses && (i < z_info->curse_max); i++)
    {
        if (ego->curses[i] && !player_knows_curse(p, i)) return false;
    }

    return true;
}


/*
 * Checks whether the player is aware of the object's effect when used
 *
 * obj is the object
 */
bool object_effect_is_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware) || obj->known->effect)
        return true;

    return false;
}


/*
 * Checks whether the object is known to be an artifact
 *
 * obj is the object
 */
bool object_is_known_artifact(const struct object *obj)
{
    return (obj->artifact && obj->known->artifact);
}


/*
 * Checks whether the object has the usual to-hit value
 *
 * obj is the object
 */
bool object_has_standard_to_h(const struct object *obj)
{
    if (tval_is_body_armor(obj) && !randcalc_varies(obj->kind->to_h))
        return (obj->to_h == obj->kind->to_h.base);
    return (obj->to_h == 0);
}


static bool object_has_brand(const struct object *obj, int index)
{
    int i;

    if (!obj->brands) return false;

    for (i = 0; i < z_info->brand_max; i++)
    {
        if (obj->brands[i] && streq(brands[index].name, brands[i].name))
            return true;
    }

    return false;
}


static bool object_has_slay(const struct object *obj, int index)
{
    int i;

    if (!obj->slays) return false;

    for (i = 0; i < z_info->slay_max; i++)
    {
        if (obj->slays[i] && same_monsters_slain(index, i))
            return true;
    }

    return false;
}


/*
 * Check an object has an unknown rune
 *
 * obj is the object
 * rune_no is the rune's number in the rune list
 */
static bool object_has_unknown_rune(const struct object *obj, int rune_no)
{
    struct rune *r = &rune_list[rune_no];

    switch (r->variety)
    {
        /* Combat runes - just check them all */
        case RUNE_VAR_COMBAT:
        {
            if ((r->index == COMBAT_RUNE_TO_A) && obj->to_a && !obj->known->to_a)
                return true;
            if ((r->index == COMBAT_RUNE_TO_H) && !object_has_standard_to_h(obj) && !obj->known->to_h)
                return true;
            if ((r->index == COMBAT_RUNE_TO_D) && obj->to_d && !obj->known->to_d)
                return true;
            break;
        }

        /* Mod runes */
        case RUNE_VAR_MOD:
        {
            if (obj->modifiers[r->index] && !obj->known->modifiers[r->index])
                return true;
            break;
        }

        /* Element runes */
        case RUNE_VAR_RESIST:
        {
            if ((obj->el_info[r->index].res_level != 0) && !obj->known->el_info[r->index].res_level)
                return true;
            break;
        }

        /* Brand runes */
        case RUNE_VAR_BRAND:
        {
            /* Brand not on the object, or known */
            if (!object_has_brand(obj, r->index)) break;
            if (object_has_brand(obj->known, r->index)) break;

            /* If we're here we have an unknown brand */
            return true;
        }

        /* Slay runes */
        case RUNE_VAR_SLAY:
        {
            /* Slay not on the object, or known */
            if (!object_has_slay(obj, r->index)) break;
            if (object_has_slay(obj->known, r->index)) break;

            /* If we're here we have an unknown slay */
            return true;
        }

        /* Curse runes */
        case RUNE_VAR_CURSE:
        {
            /* Curse not on the object, or known */
            if (!(obj->curses && obj->curses[r->index].power)) break;
            if (obj->known->curses && obj->known->curses[r->index].power) break;

            /* If we're here we have an unknown curse */
            return true;
        }

        /* Flag runes */
        case RUNE_VAR_FLAG:
        {
            if (of_has(obj->flags, r->index) && !of_has(obj->known->flags, r->index))
                return true;
            break;
        }

        default: break;
    }

    return false;
}


/*
 * Check if all the runes on an object are known to the player
 *
 * obj is the object
 */
bool object_runes_known(const struct object *obj)
{
    int i;

    /* Not all combat details known */
    if (obj->to_a && !obj->known->to_a) return false;
    if (!object_has_standard_to_h(obj) && !obj->known->to_h) return false;
    if (obj->to_d && !obj->known->to_d) return false;

    /* Not all modifiers known */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (obj->modifiers[i] && !obj->known->modifiers[i]) return false;
    }

    /* Not all elements known */
    for (i = 0; i < ELEM_MAX; i++)
    {
        if ((obj->el_info[i].res_level != 0) && !obj->known->el_info[i].res_level)
            return false;
    }

    /* Not all brands known */
    if (!brands_are_equal(obj, obj->known)) return false;

    /* Not all slays known */
    if (!slays_are_equal(obj, obj->known)) return false;

    /* Not all curses known */
    if (!curses_are_equal(obj, obj->known)) return false;

    /* Not all flags known */
    if (!of_is_subset(obj->known->flags, obj->flags)) return false;

    return true;
}


/*
 * Check if an object is fully known to the player
 *
 * obj is the object
 */
bool object_fully_known(struct player *p, const struct object *obj)
{
    bool aware;

    /* Pretend known */
    if (!p) return true;

    aware = object_flavor_is_aware(p, obj);

    if (easy_know(obj, aware)) return true;
    if (!aware) return false;

    if (obj->artifact && !obj->known->artifact) return false;
    if (obj->ego && !obj->known->ego) return false;
    if (obj->pval && !obj->known->pval) return false;
    if (obj->dd && !obj->known->dd) return false;
    if (obj->ds && !obj->known->ds) return false;
    if (obj->ac && !obj->known->ac) return false;

    /* Not all runes known */
    if (!object_runes_known(obj)) return false;

    /* Effect not known */
    if (!object_effect_is_known(obj, aware)) return false;

    return true;
}


/*
 * Checks whether the player knows whether an object has a given flag
 *
 * obj is the object
 * flag is the flag
 */
bool object_flag_is_known(struct player *p, const struct object *obj, int flag)
{
    bitflag known_flags[OF_SIZE];

    if (easy_know(obj, object_flavor_is_aware(p, obj))) return true;

    /* Get known flags */
    of_wipe(known_flags);
    object_flags_aux(obj->known, known_flags);

    /* Object has had a chance to display the flag means OK */
    if (of_has(known_flags, flag)) return true;

    return false;
}


/*
 * Checks whether the player knows the given element properties of an object
 *
 * obj is the object
 * element is the element
 */
bool object_element_is_known(const struct object *obj, int element, bool aware)
{
    size_t i;

    if (element < 0 || element >= ELEM_MAX) return false;

    /* Object has been exposed to the element means OK */
    if (easy_know(obj, aware) || obj->known->el_info[element].res_level)
        return true;

    /* Check curses */
    for (i = 0; obj->known->curses && (i < (size_t)z_info->curse_max); i++)
    {
        if (obj->known->curses[i].power == 0) continue;
        if (curses[i].obj->el_info[element].res_level)
            return true;
    }

    return false;
}


/*
 * Object knowledge propagators
 * These functions transfer player knowledge to objects
 */


/*
 * Sets the basic details on a known object
 */
void object_set_base_known(struct player *p, struct object *obj)
{
    int i;
    bool aware = object_flavor_is_aware(p, obj);

    obj->known = object_new();

    obj->known->kind = obj->kind;
    obj->known->tval = obj->tval;
    obj->known->sval = obj->sval;
    obj->known->weight = obj->weight;
    obj->known->number = obj->number;

    /* Unresistables have no hidden properties */
    /* PWMAngband: some items have resist TIME/MANA */
    for (i = ELEM_XHIGH_MAX + 1; i < ELEM_MAX; i++)
        obj->known->el_info[i].res_level = 1;

    /* Aware flavours get info now, easy_know things get everything */
    if (aware && obj->kind->flavor)
    {
        obj->known->pval = 1;
        obj->known->effect = (struct effect *)1;
    }
    if (easy_know(obj, aware)) object_notice_everything(p, obj);
}


/*
 * Gain knowledge based on sensing an object on the floor
 */
void object_sense(struct player *p, struct object *obj)
{
    int y = obj->iy;
    int x = obj->ix;

    /* Make the new object */
    struct object *new_obj = object_new();

    /* Give it a fake kind */
    object_prep(p, new_obj, (tval_is_money(obj)? unknown_gold_kind: unknown_item_kind), 0, MINIMISE);

    /* Attach it to the current floor pile */
    new_obj->iy = y;
    new_obj->ix = x;
    memcpy(&new_obj->wpos, &obj->wpos, sizeof(struct worldpos));
    pile_insert_end(&p->cave->squares[y][x].obj, new_obj);
}


/*
 * Transfer player object knowledge to an object
 *
 * p is the player
 * obj is the object
 */
void player_know_object(struct player *p, struct object *obj)
{
    int i, flag;
    bool seen = true;

    if (p->obj_k->dd) obj->known->dd = 1;
    if (p->obj_k->ds) obj->known->ds = 1;
    if (p->obj_k->ac) obj->known->ac = 1;

    /* Set combat details */
    if (p->obj_k->to_a) obj->known->to_a = 1;
    if (p->obj_k->to_h && !object_has_standard_to_h(obj)) obj->known->to_h = 1;
    if (p->obj_k->to_d) obj->known->to_d = 1;

    /* Set modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (p->obj_k->modifiers[i])
            obj->known->modifiers[i] = 1;
    }

    /* Set elements */
    for (i = 0; i < ELEM_MAX; i++)
    {
        if (p->obj_k->el_info[i].res_level == 1)
            obj->known->el_info[i].res_level = 1;
    }

    /* Set object flags */
    for (flag = of_next(p->obj_k->flags, FLAG_START); flag != FLAG_END;
        flag = of_next(p->obj_k->flags, flag + 1))
    {
        if (of_has(obj->flags, flag))
            of_on(obj->known->flags, flag);
    }

    /* Set brands */
    for (i = 0; obj->brands && (i < z_info->brand_max); i++)
    {
        if (player_knows_brand(p, i) && obj->brands[i])
            append_brand(&obj->known->brands, i);
    }

    /* Set slays */
    for (i = 0; obj->slays && (i < z_info->slay_max); i++)
    {
        if (player_knows_slay(p, i) && obj->slays[i])
            append_slay(&obj->known->slays, i);
    }

    /* Set curses */
    for (i = 0; obj->curses && (i < z_info->curse_max); i++)
    {
        if (player_knows_curse(p, i) && obj->curses[i].power)
            append_curse(obj->known, obj, i);
    }

    /* Set ego type if known */
    if (player_knows_ego(p, obj->ego))
    {
        seen = p->ego_everseen[obj->ego->eidx];
        obj->known->ego = (struct ego_item *)1;
    }

    /* PWMAngband: special case for flavored items */
    if (tval_can_have_flavor(obj))
    {
        /* Check jewelry, other flavored items are IDed by use */
        if (tval_is_jewelry(obj))
        {
            bool aware = object_flavor_is_aware(p, obj);
            struct object *copy;

            /* Hack -- special case for normal jewelry with fixed bonuses */
            copy = object_new();
            object_copy(copy, obj);
            if (!obj->artifact)
            {
                /* We need this to ID basic amulets like resist acid when the rune is learned */
                if (!randcalc_varies(obj->kind->to_h)) copy->known->to_h = 1;
                if (!randcalc_varies(obj->kind->to_d)) copy->known->to_d = 1;
                if (!randcalc_varies(obj->kind->to_a)) copy->known->to_a = 1;
            }

            /* Set jewelry type if known */
            if (object_non_curse_runes_known(copy, aware))
            {
                seen = p->kind_everseen[obj->kind->kidx];
                object_flavor_aware(p, obj);
            }

            object_delete(&copy);
        }
    }
    else
        object_check_for_ident(p, obj);

    /* Report on new stuff */
    if (!seen)
    {
        char o_name[NORMAL_WID];

        /* Describe the object if it's available */
        if (object_is_carried(p, obj))
        {
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
            msg(p, "You have %s (%c).", o_name, gear_to_label(p, obj));
        }
        else if (square_holds_object(chunk_get(&p->wpos), p->py, p->px, obj))
        {
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
            msg(p, "On the ground: %s.", o_name);
        }
    }
}


/*
 * Propagate player knowledge of objects to all objects
 *
 * p is the player
 */
void update_player_object_knowledge(struct player *p)
{
    struct object *obj;
    struct chunk *c = chunk_get(&p->wpos);

    /* Level objects */
    /* PWMAngband: only objects under the player */
    if (c)
    {
        for (obj = square_object(c, p->py, p->px); obj; obj = obj->next)
            player_know_object(p, obj);
    }

    /* Player objects */
    for (obj = p->gear; obj; obj = obj->next)
        player_know_object(p, obj);

    /* Store objects */
    /* PWMAngband: only in the Home */
    for (obj = p->home->stock; obj; obj = obj->next)
        player_know_object(p, obj);

    /* Housekeeping */
    p->upkeep->update |= (PU_BONUS | PU_INVEN);
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
    if (c) redraw_floor(&p->wpos, p->py, p->px);
}


/*
 * Object knowledge learners
 * These functions are for increasing player knowledge of object properties
 */


/*
 * Learn a given rune
 *
 * p is the player
 * i is the rune index
 * message is whether or not to print a message
 */
static void player_learn_rune(struct player *p, size_t i, bool message)
{
    struct rune *r = &rune_list[i];
    bool learned = false;

    switch (r->variety)
    {
        /* Combat runes */
        case RUNE_VAR_COMBAT:
        {
            if (r->index == COMBAT_RUNE_TO_A)
            {
                if (!p->obj_k->to_a)
                {
                    p->obj_k->to_a = 1;
                    learned = true;
                }
            }
            else if (r->index == COMBAT_RUNE_TO_H)
            {
                if (!p->obj_k->to_h)
                {
                    p->obj_k->to_h = 1;
                    learned = true;
                }
            }
            else if (r->index == COMBAT_RUNE_TO_D)
            {
                if (!p->obj_k->to_d)
                {
                    p->obj_k->to_d = 1;
                    learned = true;
                }
            }
            break;
        }

        /* Mod runes */
        case RUNE_VAR_MOD:
        {
            if (!p->obj_k->modifiers[r->index])
            {
                p->obj_k->modifiers[r->index] = 1;
                learned = true;
            }
            break;
        }

        /* Element runes */
        case RUNE_VAR_RESIST:
        {
            if (!p->obj_k->el_info[r->index].res_level)
            {
                p->obj_k->el_info[r->index].res_level = 1;
                learned = true;
            }
            break;
        }

        /* Brand runes */
        case RUNE_VAR_BRAND:
        {
            my_assert(r->index < z_info->brand_max);

            /* If the brand was unknown, add it to known brands */
            if (!player_knows_brand(p, r->index))
            {
                int i;

                for (i = 0; i < z_info->brand_max; i++)
                {
                    /* Check name */
                    if (streq(brands[r->index].name, brands[i].name))
                    {
                        p->obj_k->brands[i] = true;
                        learned = true;
                    }
                }
            }
            break;
        }

        /* Slay runes */
        case RUNE_VAR_SLAY:
        {
            my_assert(r->index < z_info->slay_max);

            /* If the slay was unknown, add it to known slays */
            if (!player_knows_slay(p, r->index))
            {
                int i;

                for (i = 0; i < z_info->slay_max; i++)
                {
                    /* Check base and race flag */
                    if (same_monsters_slain(r->index, i))
                    {
                        p->obj_k->slays[i] = true;
                        learned = true;
                    }
                }
            }
            break;
        }

        /* Curse runes */
        case RUNE_VAR_CURSE:
        {
            my_assert(r->index < z_info->curse_max);

            /* If the curse was unknown, add it to known curses */
            if (!player_knows_curse(p, r->index))
            {
                p->obj_k->curses[r->index].power = 1;
                learned = true;
            }
            break;
        }

        /* Flag runes */
        case RUNE_VAR_FLAG:
        {
            if (of_on(p->obj_k->flags, r->index))
                learned = true;
            break;
        }

        default: break;
    }

    /* Nothing learned */
    if (!learned) return;

    /* Give a message */
    if (message) msgt(p, MSG_RUNE, "You have learned the rune of %s.", rune_name(i));

    /* Update knowledge */
    update_player_object_knowledge(p);
}


/*
 * Learn a flag
 */
void player_learn_flag(struct player *p, int flag)
{
    player_learn_rune(p, rune_index(RUNE_VAR_FLAG, flag), true);
}


/*
 * Learn all innate runes
 *
 * p is the player
 */
void player_learn_innate(struct player *p)
{
    int element, flag;

    /* Elements */
    for (element = 0; element < ELEM_MAX; element++)
    {
        if (p->race->el_info[element].res_level != 0)
            player_learn_rune(p, rune_index(RUNE_VAR_RESIST, element), false);
    }

    /* Flags */
    for (flag = of_next(p->race->flags, FLAG_START); flag != FLAG_END;
        flag = of_next(p->race->flags, flag + 1))
    {
        player_learn_rune(p, rune_index(RUNE_VAR_FLAG, flag), false);
    }
}


/*
 * Learn absolutely everything
 *
 * p is the player
 */
void player_learn_everything(struct player *p)
{
    size_t i;

    for (i = 0; i < rune_max; i++)
        player_learn_rune(p, i, false);
}


/*
 * Functions for learning from the behaviour of individual objects
 */


static struct mod_msg
{
    const char *msg;
    const char *neg_msg;
} mod_msgs[] =
{
    {"You feel stronger!", "You feel weaker!"}, /* OBJ_MOD_STR */
    {"You feel smarter!", "You feel more stupid!"}, /* OBJ_MOD_INT */
    {"You feel wiser!", "You feel more naive!"}, /* OBJ_MOD_WIS */
    {"You feel more dextrous!", "You feel clumsier!"}, /* OBJ_MOD_DEX */
    {"You feel healthier!", "You feel sicklier!"}, /* OBJ_MOD_CON */
    {"You feel more attuned to magic.", "You feel less attuned to magic."}, /* OBJ_MOD_MANA */
    {"You feel stealthier.", "You feel noisier."}, /* OBJ_MOD_STEALTH */
    {NULL, NULL}, /* OBJ_MOD_SEARCH */
    {"Your eyes tingle.", "Your eyes ache."}, /* OBJ_MOD_INFRA */
    {NULL, NULL}, /* OBJ_MOD_TUNNEL */
    {"You feel strangely quick.", "You feel strangely sluggish."}, /* OBJ_MOD_SPEED */
    {"Your hands tingle.", "Your hands ache."}, /* OBJ_MOD_BLOWS */
    {"Your missiles tingle in your hands.", "Your missiles ache in your hands."}, /* OBJ_MOD_SHOTS */
    {NULL, NULL}, /* OBJ_MOD_MIGHT */
    {"Your %s glows!", NULL}, /* OBJ_MOD_LIGHT */
    {NULL, NULL}, /* OBJ_MOD_POLY_RACE */
    {"You feel less attuned to magic.", "You feel more attuned to magic."} /* OBJ_MOD_ANTI_MAGIC */
};


/*
 * Print a message when an object modifier is identified by use.
 *
 * p is the player
 * obj is the object
 */
static void mod_message(struct player *p, int mod, const char *name, bool positive)
{
    /* This should be in object_property.txt */
    const char *message = (positive? mod_msgs[mod].msg: mod_msgs[mod].neg_msg);

    if (!message) return;

    /* Special messages for individual properties */
    if (mod == OBJ_MOD_LIGHT)
        msg(p, message, name);
    else
        msg(p, message);
}


/*
 * Get a random unknown rune from an object
 *
 * obj is the object
 *
 * Returns the index into the rune list, or -1 for no unknown runes
 */
static int object_find_unknown_rune(struct object *obj)
{
    size_t i, num = 0;
    int *poss_runes = mem_zalloc(rune_max * sizeof(int));
    int chosen = -1;

    for (i = 0; i < rune_max; i++)
    {
        if (object_has_unknown_rune(obj, i))
            poss_runes[num++] = i;
    }

    /* Grab a random rune from among the unknowns */
    if (num) chosen = poss_runes[randint0(num)];

    mem_free(poss_runes);
    return chosen;
}


/*
 * Learn a random unknown rune from an object
 *
 * p is the player
 * obj is the object
 */
void object_learn_unknown_rune(struct player *p, struct object *obj)
{
    /* Get a random unknown rune from the object */
    int i = object_find_unknown_rune(obj);

    /* No unknown runes */
    if (i < 0) return;

    /* Learn the rune */
    player_learn_rune(p, i, true);
}


/*
 * Learn object properties that become obvious on wielding or wearing.
 *
 * p is the player
 * obj is the wielded object
 */
void object_learn_on_wield(struct player *p, struct object *obj)
{
    bitflag f[OF_SIZE], obvious_mask[OF_SIZE], esp_flags[OF_SIZE];
    int i, flag;
    char o_name[NORMAL_WID];
    bool obvious = false;
    s32b modifiers[OBJ_MOD_MAX];

    /* Only deal with un-ID'd items */
    if (object_is_known(p, obj)) return;

    /* Check the worn flag */
    if (obj->known->notice & OBJ_NOTICE_WORN) return;
    obj->known->notice |= OBJ_NOTICE_WORN;

    /* EASY_KNOW is now known */
    if (easy_know(obj, object_flavor_is_aware(p, obj)))
    {
        object_notice_everything(p, obj);

        /* Learn curses (they are not EASY_KNOW) */
        for (i = 0; obj->curses && (i < z_info->curse_max); i++)
        {
            int index = rune_index(RUNE_VAR_CURSE, i);

            /* Learn the curse */
            if (obj->curses[i].power == 0) continue;
            if (index >= 0) player_learn_rune(p, index, true);
        }

        return;
    }

    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

    /* Worn means tried (for flavored wearables) */
    object_flavor_tried(p, obj);

    /* Get the obvious object flags */
    create_obj_flag_mask(obvious_mask, true, OFID_WIELD, OFT_MAX);

    object_flags(obj, f);
    object_modifiers(obj, modifiers);

    /* Make sustains obvious for items with that stat bonus */
    for (i = 0; i < STAT_MAX; i++)
    {
        if (modifiers[i]) of_on(obvious_mask, sustain_flag(i));
    }

    /* Special case FA, needed for mages wielding gloves */
    if (player_has(p, PF_CUMBER_GLOVE) && (obj->tval == TV_GLOVES) &&
        (modifiers[OBJ_MOD_DEX] <= 0) && !kf_has(obj->kind->kind_flags, KF_SPELLS_OK))
    {
        of_on(obvious_mask, OF_FREE_ACT);
    }

    /* Notice obvious flags */
    if (of_is_inter(f, obvious_mask)) obvious = true;
    of_union(obj->known->flags, obvious_mask);

    /* Notice all modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (modifiers[i]) obvious = true;
        obj->known->modifiers[i] = 1;
    }

    /* Notice curses */
    if (obj->curses) obvious = true;
    object_know_curses(obj);

    object_check_for_ident(p, obj);

    if (!obvious) return;

    create_obj_flag_mask(esp_flags, false, OFT_ESP, OFT_MAX);
    if (of_is_inter(f, esp_flags))
        msg(p, "Your mind feels strangely sharper!");
    if (of_has(f, OF_FREE_ACT) && of_has(obvious_mask, OF_FREE_ACT))
        msg(p, "You feel mobile!");
    if (of_has(f, OF_KNOWLEDGE))
        msg(p, "You feel more knowledgeable!");

    /* Learn about obvious, previously unknown flags */
    of_inter(f, obvious_mask);
    for (flag = of_next(f, FLAG_START); flag != FLAG_END; flag = of_next(f, flag + 1))
    {
        /* Learn the flag */
        player_learn_flag(p, flag);
        flag_message(p, flag, o_name);
    }

    /* Learn all modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        /* Learn the mod */
        if (modifiers[i])
        {
            bool positive = ((modifiers[i] > 0)? true: false);

            player_learn_rune(p, rune_index(RUNE_VAR_MOD, i), true);
            mod_message(p, i, o_name, positive);
        }
    }

    /* Learn curses */
    for (i = 0; obj->curses && (i < z_info->curse_max); i++)
    {
        int index = rune_index(RUNE_VAR_CURSE, i);

        /* Learn the curse */
        if (obj->curses[i].power == 0) continue;
        if (index >= 0) player_learn_rune(p, index, true);
    }

    /* Hack -- know activation on rings of polymorphing to bypass (unfair) learning by use */
    if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")))
        object_notice_effect(p, obj);
}


/*
 * Learn object properties that become obvious on use, mark it as
 * aware and reward the player with some experience.
 *
 * p is the player
 * obj is the used object
 */
void object_learn_on_use(struct player *p, struct object *obj)
{
    /* Object level */
    int lev = obj->kind->level;

    object_flavor_aware(p, obj);
    object_notice_effect(p, obj);
    if (tval_is_rod(obj)) object_notice_everything(p, obj);
    player_exp_gain(p, (lev + (p->lev >> 1)) / p->lev);

    p->upkeep->notice |= PN_IGNORE;
}


/*
 * Notice any slays on a particular object which affect a particular monster race.
 *
 * p is the player
 * index is index of the slay we are learning
 */
void object_learn_slay(struct player *p, int index)
{
    /* Learn about the slay */
    if (!player_knows_slay(p, index))
    {
        int i;

        /* Find the rune index */
        for (i = 0; i < z_info->slay_max; i++)
        {
            if (same_monsters_slain(i, index)) break;
        }
        my_assert(i < z_info->slay_max);

        /* Learn the rune */
        player_learn_rune(p, rune_index(RUNE_VAR_SLAY, i), true);
    }
}


/*
 * Notice any brands on a particular object which affect a particular monster race.
 *
 * p is the player
 * index is index of the brand we are learning
 */
void object_learn_brand(struct player *p, int index)
{
    /* Learn about the brand */
    if (!player_knows_brand(p, index))
    {
        int i;

        /* Find the rune index */
        for (i = 0; i < z_info->brand_max; i++)
        {
            if (streq(brands[i].name, brands[index].name)) break;
        }
        my_assert(i < z_info->brand_max);

        /* Learn the rune */
        player_learn_rune(p, rune_index(RUNE_VAR_BRAND, i), true);
    }
}


/*
 * Learn attack bonus on making a ranged attack.
 * Can be applied to the missile or the missile launcher
 *
 * p is the player
 * obj is the missile or launcher
 */
void missile_learn_on_ranged_attack(struct player *p, struct object *obj)
{
    object_notice_attack_plusses(p, obj);
}


/*
 * Functions for learning about equipment properties
 * These functions are for gaining object knowledge from the behaviour of
 * the player's equipment
 */


/*
 * Learn things which happen on defending.
 *
 * p is the player
 */
void equip_learn_on_defend(struct player *p)
{
    int i;

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (obj) object_notice_defence_plusses(p, obj);
    }
}


/*
 * Learn to-hit bonus on making a ranged attack.
 * Does not apply to weapon, missile or bow which should be done separately
 *
 * p is the player
 */
void equip_learn_on_ranged_attack(struct player *p)
{
    int i;

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (i == slot_by_name(p, "weapon")) continue;
        if (i == slot_by_name(p, "shooting")) continue;

        if (obj && !obj->known->to_h)
        {
            bool has_standard_to_h;
            s16b to_h;

            obj->known->to_h = 1;
            object_check_for_ident(p, obj);

            object_to_h(obj, &to_h);
            has_standard_to_h = (object_has_standard_to_h(obj) && (obj->to_h < 0) && !obj->curses);

            if (to_h && !has_standard_to_h)
            {
                char o_name[NORMAL_WID];
                int index = rune_index(RUNE_VAR_COMBAT, COMBAT_RUNE_TO_H);

                object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);
                msg(p, "Your %s glow%s.", o_name, ((obj->number > 1) ? "" : "s"));

                player_learn_rune(p, index, true);
            }

            p->upkeep->update |= (PU_BONUS);
            p->upkeep->redraw |= (PR_EQUIP | PR_PLUSSES);
        }
    }
}


/*
 * Learn things which happen on making a melee attack.
 * Does not apply to weapon, missile or bow which should be done separately
 *
 * p is the player
 */
void equip_learn_on_melee_attack(struct player *p)
{
    int i;

    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);

        if (i == slot_by_name(p, "weapon")) continue;
        if (i == slot_by_name(p, "shooting")) continue;

        if (obj) object_notice_attack_plusses(p, obj);
    }
}


/*
 * Learn a given object flag on wielded items.
 *
 * p is the player
 * flag is the flag to notice
 */
bool equip_learn_flag(struct player *p, int flag)
{
    int i;
    bool redraw = false;

    /* Sanity check */
    if (!flag) return false;

    /* All wielded items eligible */
    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);
        bitflag flags[OF_SIZE];

        if (!obj) continue;

        /* Already known */
        if (of_has(obj->known->flags, flag)) continue;

        object_flags(obj, flags);

        /* Notice the flag */
        object_notice_flag(p, obj, flag);

        /* Does the object have the flag? */
        if (of_has(flags, flag))
        {
            char o_name[NORMAL_WID];

            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

            /* Message */
            flag_message(p, flag, o_name);

            /* Learn the flag */
            player_learn_flag(p, flag);

            redraw = true;
        }
    }

    return redraw;
}


/*
 * Learn the elemental resistance properties on wielded items.
 *
 * p is the player
 * element is the element to notice
 */
void equip_learn_element(struct player *p, int element)
{
    int i;

    /* Sanity check */
    if (element < 0 || element >= ELEM_MAX) return;

    /* All wielded items eligible */
    for (i = 0; i < p->body.count; i++)
    {
        struct object *obj = slot_object(p, i);
        struct element_info el_info[ELEM_MAX];

        if (!obj) continue;

        /* Already known */
        if (obj->known->el_info[element].res_level) continue;

        object_elements(obj, el_info);

        /* Notice the element properties */
        object_notice_element(p, obj, element);

        /* Does the object affect the player's resistance to the element? */
        if (el_info[element].res_level != 0)
        {
            char o_name[NORMAL_WID];

            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

            /* Message */
            msg(p, "Your %s glows.", o_name);

            /* Learn the element properties */
            player_learn_rune(p, rune_index(RUNE_VAR_RESIST, element), true);
        }
    }
}


/*
 * Learn things that would be noticed in time.
 *
 * p is the player
 */
void equip_learn_after_time(struct player *p)
{
    int flag;
    bitflag timed_mask[OF_SIZE];
    bool redraw = false;

    /* Get the timed flags */
    create_obj_flag_mask(timed_mask, true, OFID_TIMED, OFT_MAX);

    /* Attempt to learn every flag */
    for (flag = of_next(timed_mask, FLAG_START); flag != FLAG_END;
        flag = of_next(timed_mask, flag + 1))
    {
        if (equip_learn_flag(p, flag)) redraw = true;
    }

    /* Notice new info */
    if (redraw) p->upkeep->redraw |= (PR_EQUIP);
}


/*
 * Object kind functions
 * These deal with knowledge of an object's kind
 */


/*
 * Checks whether an object counts as "known" due to EASY_KNOW status
 *
 * obj is the object
 */
bool easy_know(const struct object *obj, bool aware)
{
    return (aware && kf_has(obj->kind->kind_flags, KF_EASY_KNOW));
}


/*
 * Checks whether the player is aware of the object's flavour
 *
 * obj is the object
 */
bool object_flavor_is_aware(struct player *p, const struct object *obj)
{
    /* Pretend aware */
    if (!p) return true;

    return (p->obj_aware[obj->kind->kidx] || obj->bypass_aware);
}


/*
 * Checks whether the player has tried to use other objects of the same kind
 *
 * obj is the object
 */
bool object_flavor_was_tried(struct player *p, const struct object *obj)
{
    /* Pretend tried */
    if (!p) return true;

    return p->obj_tried[obj->kind->kidx];
}


/*
 * Mark an object's flavour as one the player is aware of.
 *
 * obj is the object whose flavour should be marked as aware
 */
static void object_flavor_aware_aux(struct player *p, struct object *obj, bool send)
{
    int y, x;
    struct chunk *c;

    /* Pretend aware */
    if (!p) return;
    if (p->obj_aware[obj->kind->kidx]) return;

    /* Fully aware of the effects */
    p->obj_aware[obj->kind->kidx] = true;
    if (send) Send_aware(p, obj->kind->kidx);

    /* A bunch of things are now known */
    obj->known->pval = 1;
    obj->known->effect = (struct effect *)1;
    if (object_has_standard_to_h(obj)) obj->known->to_h = 1;
    object_id_set_aware(obj);

    /* Update flags */
    p->upkeep->redraw |= PR_EQUIP;
    p->upkeep->notice |= PN_IGNORE;

    /* Quit if no dungeon yet */
    c = chunk_get(&p->wpos);
    if (!c) return;

    /* Some objects change tile on awareness, so update display for all floor objects of this kind */
    for (y = 1; y < c->height; y++)
    {
        for (x = 1; x < c->width; x++)
        {
            bool light = false;
            const struct object *floor_obj;

            for (floor_obj = square_object(c, y, x); floor_obj; floor_obj = floor_obj->next)
            {
                if (floor_obj->kind == obj->kind)
                {
                    light = true;
                    break;
                }
            }
            if (light) square_light_spot_aux(p, c, y, x);
        }
    }
}


void object_flavor_aware(struct player *p, struct object *obj)
{
    object_flavor_aware_aux(p, obj, true);
}


/*
 * Mark an object's flavour as tried.
 *
 * obj is the object whose flavour should be marked
 */
void object_flavor_tried(struct player *p, struct object *obj)
{
    my_assert(obj);

    p->obj_tried[obj->kind->kidx] = true;
}


/* PWMAngband: original object knowledge */


/*
 * Is the player aware of all of an object's flags?
 *
 * obj is the object
 */
static bool object_all_flags_are_known(const struct object *obj, bool aware)
{
    return (easy_know(obj, aware) || of_is_subset(obj->known->flags, obj->flags));
}


/*
 * Is the player aware of all of an object's modifiers?
 *
 * obj is the object
 */
static bool object_all_modifiers_are_known(const struct object *obj, bool aware)
{
    size_t i;

    if (easy_know(obj, aware)) return true;

    for (i = 0; i < OBJ_MOD_MAX; i++)
    {
        if (!obj->known->modifiers[i])
            return false;
    }

    return true;
}


/*
 * Is the player aware of all of an object's elemental properties?
 *
 * obj is the object
 */
static bool object_all_elements_are_known(const struct object *obj, bool aware)
{
    size_t i;

    if (easy_know(obj, aware)) return true;

    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Only check if the flags are set if there's something to look at */
        if ((obj->el_info[i].res_level != 0) && !obj->known->el_info[i].res_level)
            return false;
    }

    return true;
}


/*
 * Is the player aware of all of an object's brands and slays?
 *
 * obj is the object
 */
static bool object_all_brands_and_slays_are_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!brands_are_equal(obj, obj->known)) return false;
    if (!slays_are_equal(obj, obj->known)) return false;

    return true;
}


/*
 * Is the player aware of all of an object's curses?
 *
 * obj is the object
 */
static bool object_all_curses_are_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!curses_are_equal(obj, obj->known)) return false;

    return true;
}


/*
 * Is the player aware of all of an object's miscellaneous properties?
 *
 * obj is the object
 */
static bool object_all_miscellaneous_are_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!obj->known->artifact) return false;
    if (!obj->known->ego) return false;
    if (!obj->known->pval) return false;
    if (!obj->known->dd) return false;
    if (!obj->known->ds) return false;
    if (!obj->known->ac) return false;
    if (!obj->known->to_a) return false;
    if (!obj->known->to_h) return false;
    if (!obj->known->to_d) return false;
    if (!obj->known->effect) return false;

    return true;
}


/*
 * Is the player aware of all of an object's properties?
 *
 * obj is the object
 */
bool object_all_but_flavor_is_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!object_all_flags_are_known(obj, aware)) return false;
    if (!object_all_modifiers_are_known(obj, aware)) return false;
    if (!object_all_elements_are_known(obj, aware)) return false;
    if (!object_all_brands_and_slays_are_known(obj, aware)) return false;
    if (!object_all_curses_are_known(obj, aware)) return false;
    if (!object_all_miscellaneous_are_known(obj, aware)) return false;

    return true;
}


/*
 * Check if all non-curse runes on an object are known to the player
 *
 * obj is the object
 */
bool object_non_curse_runes_known(const struct object *obj, bool aware)
{
    if (easy_know(obj, aware)) return true;

    if (!object_all_flags_are_known(obj, aware)) return false;
    if (!object_all_modifiers_are_known(obj, aware)) return false;
    if (!object_all_elements_are_known(obj, aware)) return false;
    if (!object_all_brands_and_slays_are_known(obj, aware)) return false;
    if (!obj->known->artifact) return false;
    if (!obj->known->ego) return false;
    if (!obj->known->pval) return false;
    if (!obj->known->dd) return false;
    if (!obj->known->ds) return false;
    if (!obj->known->ac) return false;
    if (!obj->known->to_a) return false;
    if (!obj->known->to_h) return false;
    if (!obj->known->to_d) return false;

    return true;
}


/*
 * Returns whether an object should be treated as fully known
 */
bool object_is_known(struct player *p, const struct object *obj)
{
    bool aware;

    /* Pretend known */
    if (!p) return true;

    aware = object_flavor_is_aware(p, obj);

    if (easy_know(obj, aware)) return true;
    if (!aware) return false;
    return object_all_but_flavor_is_known(obj, aware);
}


/*
 * Returns whether the object has been worn/wielded
 */
bool object_was_worn(const struct object *obj)
{
    return ((obj->known->notice & OBJ_NOTICE_WORN)? true: false);
}


/*
 * Checks for additional knowledge implied by what the player already knows.
 *
 * obj is the object to check
 */
bool object_check_for_ident(struct player *p, struct object *obj)
{
    bool aware = object_flavor_is_aware(p, obj);

    /* Check things which need to be learned */
    if (!object_all_flags_are_known(obj, aware)) return false;
    if (!object_all_elements_are_known(obj, aware)) return false;
    if (!object_all_brands_and_slays_are_known(obj, aware)) return false;

    /*
     * If we know attack bonuses, and defence bonuses, and effect, then
     * we effectively know everything, so mark as such
     */
    if (obj->known->to_h && obj->known->to_d && obj->known->to_a &&
        (object_effect_is_known(obj, aware) || !object_effect(obj)))
    {
        /*
         * In addition to knowing the pval flags, it is necessary to know
         * the modifiers and curses to know everything
         */
        if (object_all_modifiers_are_known(obj, aware) && object_all_curses_are_known(obj, aware))
        {
            object_notice_everything(p, obj);
            return true;
        }
    }

    /* We still know all the flags, so if it's worn we know it's an ego */
    if (obj->ego && object_was_worn(obj)) object_notice_ego(p, obj);

    return false;
}


/*
 * Make the player aware of all of an object's flags.
 *
 * obj is the object to mark
 */
void object_know_all_flags(struct object *obj)
{
    of_setall(obj->known->flags);
}


/*
 * Make the player aware of all of an object's modifiers.
 *
 * obj is the object to mark
 */
static void object_know_all_modifiers(struct object *obj)
{
    size_t i;

    for (i = 0; i < OBJ_MOD_MAX; i++)
        obj->known->modifiers[i] = 1;
}


/*
 * Make the player aware of all of an object's elemental properties.
 *
 * obj is the object to mark
 */
void object_know_all_elements(struct object *obj)
{
    size_t i;

    for (i = 0; i < ELEM_MAX; i++)
        obj->known->el_info[i].res_level = 1;
}


/*
 * Make the player aware of all of an object's brands and slays.
 *
 * obj is the object to mark
 */
void object_know_brands_and_slays(struct object *obj)
{
    /* Wipe all previous known and know everything */
    mem_free(obj->known->brands);
    obj->known->brands = NULL;
    if (obj->brands)
    {
        size_t array_size = z_info->brand_max * sizeof(bool);

        obj->known->brands = mem_zalloc(array_size);
        memcpy(obj->known->brands, obj->brands, array_size);
    }
    mem_free(obj->known->slays);
    obj->known->slays = NULL;
    if (obj->slays)
    {
        size_t array_size = z_info->slay_max * sizeof(bool);

        obj->known->slays = mem_zalloc(array_size);
        memcpy(obj->known->slays, obj->slays, array_size);
    }
}


/*
 * Make the player aware of all of an object's curses.
 *
 * obj is the object to mark
 */
void object_know_curses(struct object *obj)
{
    /* Wipe all previous known and know everything */
    mem_free(obj->known->curses);
    obj->known->curses = NULL;
    if (obj->curses)
    {
        size_t array_size = z_info->curse_max * sizeof(struct curse_data);

        obj->known->curses = mem_zalloc(array_size);
        memcpy(obj->known->curses, obj->curses, array_size);
    }
}


/*
 * Make the player aware of all of an object's miscellaneous properties.
 *
 * obj is the object to mark
 */
static void object_know_all_miscellaneous(struct object *obj)
{
    obj->known->artifact = (struct artifact *)1;
    obj->known->ego = (struct ego_item *)1;
    obj->known->notice &= ~OBJ_NOTICE_WORN;
    obj->known->pval = 1;
    obj->known->dd = 1;
    obj->known->ds = 1;
    obj->known->ac = 1;
    obj->known->to_a = 1;
    obj->known->to_h = 1;
    obj->known->to_d = 1;
    obj->known->effect = (struct effect *)1;
}


/*
 * Make the player aware of all of an object' properties except flavor.
 *
 * obj is the object to mark
 */
static void object_know_all_but_flavor(struct object *obj)
{
    /* Know all flags there are to be known */
    object_know_all_flags(obj);

    /* Know all modifiers */
    object_know_all_modifiers(obj);

    /* Know all elemental properties */
    object_know_all_elements(obj);

    /* Know all brands and slays */
    object_know_brands_and_slays(obj);

    /* Know all curses */
    object_know_curses(obj);

    /* Know everything else */
    object_know_all_miscellaneous(obj);
}


/*
 * Mark an object as fully known, a.k.a identified.
 *
 * obj is the object to mark as identified
 */
void object_notice_everything_aux(struct player *p, struct object *obj, bool bypass_aware,
    bool send)
{
    /* Mark as known */
    if (bypass_aware) obj->bypass_aware = true;
    else object_flavor_aware_aux(p, obj, send);

    /* Know everything else */
    obj->known->notice |= OBJ_NOTICE_ASSESSED;
    object_know_all_but_flavor(obj);
}


void object_notice_everything(struct player *p, struct object *obj)
{
    object_notice_everything_aux(p, obj, false, true);
}


/*
 * Notice the ego on an ego item.
 */
void object_notice_ego(struct player *p, struct object *obj)
{
    bitflag learned_flags[OF_SIZE];
    bitflag xtra_flags[OF_SIZE];
    size_t i;

    if (!obj->ego) return;

    /* Learn ego flags */
    of_union(obj->known->flags, obj->ego->flags);

    /* Learn ego element properties */
    for (i = 0; i < ELEM_MAX; i++)
    {
        if (obj->ego->el_info[i].res_level != 0)
            obj->known->el_info[i].res_level = 1;
    }

    /* Learn all flags except random abilities */
    of_setall(learned_flags);

    /* Learn all brands and slays */
    object_know_brands_and_slays(obj);

    of_wipe(xtra_flags);

    /* Don't learn random ego extras */
    if (kf_has(obj->ego->kind_flags, KF_RAND_SUSTAIN))
    {
        create_obj_flag_mask(xtra_flags, false, OFT_SUST, OFT_MAX);
        of_diff(learned_flags, xtra_flags);
    }
    if (kf_has(obj->ego->kind_flags, KF_RAND_POWER) ||
        kf_has(obj->ego->kind_flags, KF_RAND_RES_POWER))
    {
        create_obj_flag_mask(xtra_flags, false, OFT_MISC, OFT_PROT, OFT_ESP, OFT_MAX);
        of_diff(learned_flags, xtra_flags);
    }
    if (kf_has(obj->ego->kind_flags, KF_RAND_ESP))
    {
        create_obj_flag_mask(xtra_flags, false, OFT_ESP, OFT_MAX);
        of_diff(learned_flags, xtra_flags);
    }

    of_union(obj->known->flags, learned_flags);

    /* Learn all element properties except random resists */
    for (i = 0; i < ELEM_MAX; i++)
    {
        /* Don't learn random ego base resists */
        if ((i < ELEM_HIGH_MIN) &&
            (kf_has(obj->ego->kind_flags, KF_RAND_BASE_RES) ||
            kf_has(obj->ego->kind_flags, KF_RAND_RES_POWER)))
        {
            continue;
        }

        /* Don't learn random ego high resists */
        if ((i >= ELEM_HIGH_MIN) && (i <= ELEM_HIGH_MAX) &&
            kf_has(obj->ego->kind_flags, KF_RAND_HI_RES))
        {
            continue;
        }

        /* Learn all element properties */
        obj->known->el_info[i].res_level = 1;
    }

    /* If you know the ego, you know which it is of excellent or splendid */
    if (!obj->known->ego)
    {
        obj->known->ego = (struct ego_item *)1;
        object_check_for_ident(p, obj);
    }
}


/*
 * Notice the "effect" from activating an object.
 *
 * obj is the object to become aware of
 */
void object_notice_effect(struct player *p, struct object *obj)
{
    if (!obj->known->effect)
    {
        obj->known->effect = (struct effect *)1;
        object_check_for_ident(p, obj);
    }

    /* Noticing an effect gains awareness */
    if (!object_flavor_is_aware(p, obj)) object_flavor_aware(p, obj);
}


/*
 * Notice things which happen on defending.
 */
void object_notice_defence_plusses(struct player *p, struct object *obj)
{
    char o_name[NORMAL_WID];
    s16b to_a;

    if (!obj) return;
    if (obj->known->to_a) return;

    obj->known->to_a = 1;
    object_check_for_ident(p, obj);

    object_to_a(obj, &to_a);

    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);
    msg(p, "You know more about the %s you are wearing.", o_name);

    if (to_a)
    {
        int index = rune_index(RUNE_VAR_COMBAT, COMBAT_RUNE_TO_A);

        player_learn_rune(p, index, true);
    }

    p->upkeep->update |= (PU_BONUS);
    p->upkeep->redraw |= (PR_EQUIP | PR_ARMOR);
}


/*
 * Notice things which happen on attacking.
 */
void object_notice_attack_plusses(struct player *p, struct object *obj)
{
    char o_name[NORMAL_WID];
    bool aware, has_standard_to_h;
    s16b to_h, to_d;

    if (!obj) return;

    aware = object_flavor_is_aware(p, obj);
    if (obj->known->to_h && obj->known->to_d && aware) return;

    obj->known->to_h = 1;
    obj->known->to_d = 1;
    object_check_for_ident(p, obj);

    object_to_h(obj, &to_h);
    has_standard_to_h = (object_has_standard_to_h(obj) && (obj->to_h < 0) && !obj->curses);
    object_to_d(obj, &to_d);

    object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);

    if (equipped_item_by_slot_name(p, "weapon") == obj)
        msg(p, "You know more about the %s you are using.", o_name);
    else if ((to_d || to_h) && !has_standard_to_h)
        msg(p, "Your %s glow%s.", o_name, ((obj->number > 1) ? "" : "s"));

    if (to_h && !has_standard_to_h)
    {
        int index = rune_index(RUNE_VAR_COMBAT, COMBAT_RUNE_TO_H);

        player_learn_rune(p, index, true);
    }
    if (to_d)
    {
        int index = rune_index(RUNE_VAR_COMBAT, COMBAT_RUNE_TO_D);

        player_learn_rune(p, index, true);
    }

    if (object_all_but_flavor_is_known(obj, aware)) object_flavor_aware(p, obj);

    p->upkeep->update |= (PU_BONUS);
    p->upkeep->redraw |= (PR_EQUIP | PR_PLUSSES);
}


/*
 * Notice elemental resistance properties for an element on an object
 */
bool object_notice_element(struct player *p, struct object *obj, int element)
{
    if (element < 0 || element >= ELEM_MAX) return false;

    /* Already known */
    if (obj->known->el_info[element].res_level)
        return false;

    /* Learn about this element */
    obj->known->el_info[element].res_level = 1;

    object_check_for_ident(p, obj);

    return true;
}


/*
 * Notice a single flag - returns true if anything new was learned
 */
bool object_notice_flag(struct player *p, struct object *obj, int flag)
{
    if (of_has(obj->known->flags, flag)) return false;

    of_on(obj->known->flags, flag);
    object_check_for_ident(p, obj);

    return true;
}


/*
 * Returns whether the object has been sensed
 */
bool object_was_sensed(const struct object *obj)
{
    return ((obj->known->notice & OBJ_NOTICE_ASSESSED)? true: false);
}


/*
 * Mark an object as sensed
 */
void object_notice_sensing(struct player *p, struct object *obj)
{
    obj->known->notice |= OBJ_NOTICE_ASSESSED;
    object_check_for_ident(p, obj);
}


/*
 * Identify an item.
 */
void object_know_everything(struct player *p, struct object *obj)
{
    char o_name[NORMAL_WID];
    u32b msg_type = 0;
    bool bad = (obj->curses || worthless_p(obj));

    /* Identify it */
    object_notice_everything(p, obj);

    /* Update the gear */
    calc_inventory(p);

    /* Set ignore flag */
    p->upkeep->notice |= PN_IGNORE;

    /* Recalculate bonuses */
    p->upkeep->update |= (PU_BONUS);

    /* Redraw */
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_PLUSSES);

    /* Description */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    /* Determine the message type. */
    if (bad) msg_type = MSG_IDENT_BAD;
    else if (obj->artifact) msg_type = MSG_IDENT_ART;
    else if (obj->ego) msg_type = MSG_IDENT_EGO;
    else msg_type = MSG_GENERIC;

    /* Describe */
    if (object_is_equipped(p->body, obj))
    {
        int slot = equipped_item_slot(p->body, obj);

        /* Format and capitalise */
        char *msg = format("%s: %s (%c).", equip_describe(p, slot), o_name, I2A(slot));

        my_strcap(msg);
        msgt(p, msg_type, msg);
    }
    else if (object_is_carried(p, obj))
        msgt(p, msg_type, "In your pack: %s (%c).", o_name, gear_to_label(p, obj));
    else
        msgt(p, msg_type, "On the ground: %s.", o_name);
}


/*
 * Set some knowledge for items where the flavour is already known
 */
void object_id_set_aware(struct object *obj)
{
    int i;

    /* Jewelry with fixed bonuses gets more info now */
    if (tval_is_jewelry(obj) && !obj->artifact)
    {
        if (!randcalc_varies(obj->kind->to_h)) obj->known->to_h = 1;
        if (!randcalc_varies(obj->kind->to_d)) obj->known->to_d = 1;
        if (!randcalc_varies(obj->kind->to_a)) obj->known->to_a = 1;
        for (i = 0; i < OBJ_MOD_MAX; i++)
        {
            /* Unknown "LIGHT" modifier is never displayed */
            if (i == OBJ_MOD_LIGHT) continue;

            if (!randcalc_varies(obj->kind->modifiers[i]))
                obj->known->modifiers[i] = 1;
        }
    }

    /* PWMAngband: normal light sources get missing info to be able to display turns of fuel */
    if (tval_is_light(obj) && !obj->artifact && !obj->ego)
    {
        obj->known->to_d = 1;
        obj->known->to_a = 1;
    }
}
