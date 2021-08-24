/* File: object2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Object code, part 2 */

#include "angband.h"

#include "int_map.h"

#include <assert.h>

/*
 * Delete a dungeon object
 */
void delete_object_idx(int o_idx)
{
    obj_ptr obj = dun_obj(cave, o_idx);
    point_t pos = point_create(obj->loc.v.floor.x, obj->loc.v.floor.y);

    dun_delete_obj(cave, o_idx);
    if (cave->id == plr->dun_id)
    {
        draw_pos(pos);
        plr->window |= PW_OBJECT_LIST;
    }
}


/*
 * Deletes all objects at given location
 */
void delete_object(point_t pos)
{
    dun_destroy_obj_at(cave, pos);
    if (cave->id == plr->dun_id)
    {
        draw_pos(pos);
        plr->window |= PW_OBJECT_LIST;
    }
}

/*
 * Apply a "object restriction function" to the "object allocation table"
 */
errr get_obj_num_prep(void)
{
    int i;

    /* Get the entry */
    alloc_entry *table = alloc_kind_table;

    /* Scan the allocation table */
    for (i = 0; i < alloc_kind_size; i++)
    {
        /* Accept objects which pass the restriction, if any */
        if (!get_obj_num_hook || (*get_obj_num_hook)(table[i].index))
        {
            /* Accept this object */
            table[i].prob2 = table[i].prob1;
        }

        /* Do not use this object */
        else
        {
            /* Decline this object */
            table[i].prob2 = 0;
        }
    }

    /* Success */
    return (0);
}

static int _spellbook_max(int tval, int sval)
{
    int       max = 0;
    const int limits[4] = { 10, 10, 3, 2 };

    if (tval >= TV_LIFE_BOOK)
    {
        if (tval == TV_ARCANE_BOOK)
            max = 10;
        else
            max = limits[sval];
    }

    return max;
}

static bool _allow_harp(void)
{
    if (plr->pclass == CLASS_BARD) return TRUE;
    if (plr->pclass != CLASS_SKILLMASTER) return FALSE;
    return skillmaster_is_valid_realm(REALM_MUSIC);
}
/*
 * Choose an object kind that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "object allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" object, in
 * a relatively efficient manner.
 *
 * Note that if no objects are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 */
s16b get_obj_num(int level)
{
    int             i;
    int             k_idx;
    long            value, total;
    object_kind     *k_ptr;
    alloc_entry     *table = alloc_kind_table;
    plr_class_ptr    class_ptr = plr_class();
    plr_race_ptr     race_ptr = plr_race();

    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;

    /* Boost level */
    if (level > 0)
    {
        /* Occasional "boost" */
        if (one_in_(GREAT_OBJ))
        {
            int boost = level;
            if (boost < 20)
                boost = 20;
            level += rand_range(boost/4, boost/2);

            /* What a bizarre calculation ... yes, but this did allow
             * you to find PDSM on DL1 which is no longer possible.
            level = 1 + (level * MAX_DEPTH / randint1(MAX_DEPTH)); */
        }
    }

    /* Reset total */
    total = 0L;

    /* Process probabilities */
    for (i = 0; i < alloc_kind_size; i++)
    {
        int p = table[i].prob2;
        int max = 0;

        /* Objects are sorted by depth */
        if (table[i].level > level) break;
        table[i].prob3 = 0;

        if (table[i].max_level && table[i].max_level < level) continue;

        k_idx = table[i].index;
        k_ptr = &k_info[k_idx];
        if (k_ptr->tval == TV_FOOD && k_ptr->sval == SV_FOOD_AMBROSIA && cave->type->id != D_OLYMPUS) continue;
        if (k_ptr->tval == TV_BOW && k_ptr->sval == SV_HARP && !_allow_harp()) continue;

        /* Hack -- prevent embedded chests */
        if (opening_chest && (k_ptr->tval == TV_CHEST)) continue;

        /* TODO: Add some sort of max_num field to limit certain objects (I'm looking at you, spellbooks!)
           Note, this also ensures an even distribution of spellbook kinds for high level books! */
        max = _spellbook_max(k_ptr->tval, k_ptr->sval);
        if (max && p)
        {
            int ct = k_ptr->counts.found - max;
            while (ct-- > 0)
                p /= 2;
            p = MAX(p, 1);
        }

        if (p && race_ptr->hooks.obj_alloc)
            p = race_ptr->hooks.obj_alloc(k_ptr, p);

        if (p && class_ptr->hooks.obj_alloc)
            p = class_ptr->hooks.obj_alloc(k_ptr, p);

        table[i].prob3 = p;
        total += p;
    }

    /* No legal objects */
    if (total <= 0)
        return 0;


    /* Pick an object */
    value = randint0(total);

    /* Find the object */
    for (i = 0; i < alloc_kind_size; i++)
    {
        /* Found the entry */
        if (value < table[i].prob3) break;

        /* Decrement */
        value = value - table[i].prob3;
    }

    /* Note: There used to be power boosting code here, but it gave very bad results in
     * some situations. For example, I want wands, rods and staves to allocate equally, but
     * would like wands to be available earlier than staves, which are earlier than rods.
     * Setting things up as:
     *   Wand:  A:1/1
     *   Staff: A:5/1
     *   Rod:   A:10/1
     * gave the following allocation distribution (10,000 objects, devices are 15%, deeper than DL10
     * so I would expect 5% allocation to each):
     * > Wands:  286 2.8%
     * > Staves: 486 4.8%
     * > Rods:   747 7.4%
     * Adding duplicate allocation entries helped, but the distribution was still unacceptably
     * skewed and this "feature" seemed more like a bug to me. -CTK
     */

    /* Result */
    return (table[i].index);
}

bool object_is_aware(object_type *o_ptr)
{
    return k_info[o_ptr->k_idx].aware;
}

/*
 * The player is now aware of the effects of the given object.
 */
void object_aware(object_type *o_ptr)
{
    k_info[o_ptr->k_idx].aware = TRUE;
}
/* Statistics
   We try hard not to leak information. For example, when picking up an
   unaware potion, we should wait for one of the following before counting it:
   [1] Identify
   [2] Sell to shop
   [3] Quaff and become aware.

   Note we might miss some counts if the user quaffs, but doesn't notice the
   effect. This is better than leaking kind info in the various browser screens,
   though.
*/
counts_t stats_rand_art_counts = {0};
gold_counts_t stats_gold_counts = {0};

void stats_reset(void)
{
    int i;

    for (i = 1; i < max_k_idx; i++)
    {
        object_kind *k_ptr = &k_info[i];

        WIPE(&k_ptr->counts, counts_t);
    }
    for (i = 1; i < max_e_idx; i++)
    {
        ego_type *e_ptr = &e_info[i];

        WIPE(&e_ptr->counts, counts_t);
    }

    WIPE(&stats_rand_art_counts, counts_t);
    WIPE(&stats_gold_counts, gold_counts_t);
    device_stats_reset();
}

void stats_on_load(savefile_ptr file)
{
    stats_rand_art_counts.generated = savefile_read_s32b(file);
    stats_rand_art_counts.found = savefile_read_s32b(file);
    stats_rand_art_counts.bought = savefile_read_s32b(file);
    stats_rand_art_counts.used = savefile_read_s32b(file);
    stats_rand_art_counts.destroyed = savefile_read_s32b(file);

    stats_gold_counts.found = savefile_read_s32b(file);
    stats_gold_counts.selling = savefile_read_s32b(file);
    stats_gold_counts.buying = savefile_read_s32b(file);
    stats_gold_counts.services = savefile_read_s32b(file);
    stats_gold_counts.winnings = savefile_read_s32b(file);
    stats_gold_counts.stolen = savefile_read_s32b(file);

    device_stats_on_load(file);
}

void stats_on_save(savefile_ptr file)
{
    savefile_write_s32b(file, stats_rand_art_counts.generated);
    savefile_write_s32b(file, stats_rand_art_counts.found);
    savefile_write_s32b(file, stats_rand_art_counts.bought);
    savefile_write_s32b(file, stats_rand_art_counts.used);      /* Artifact Devices */
    savefile_write_s32b(file, stats_rand_art_counts.destroyed); /* Certain Class Powers */

    savefile_write_s32b(file, stats_gold_counts.found);
    savefile_write_s32b(file, stats_gold_counts.selling);
    savefile_write_s32b(file, stats_gold_counts.buying);
    savefile_write_s32b(file, stats_gold_counts.services);
    savefile_write_s32b(file, stats_gold_counts.winnings);
    savefile_write_s32b(file, stats_gold_counts.stolen);

    device_stats_on_save(file);
}

void stats_on_gold_find(int au)
{
    stats_gold_counts.found += au;
}

void stats_on_gold_selling(int au)
{
    stats_gold_counts.selling += au;
}

void stats_on_gold_buying(int au)
{
    stats_gold_counts.buying += au;
}

void stats_on_gold_services(int au)
{
    stats_gold_counts.services += au;
}

void stats_on_gold_winnings(int au)
{
    stats_gold_counts.winnings += au;
}

void stats_on_gold_stolen(int au)
{
    stats_gold_counts.stolen += au;
}

void stats_on_purchase(object_type *o_ptr)
{
    if (!(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.bought += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }
    if (obj_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_purchase(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }
    if (o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.bought += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }
    if (o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
    {
        stats_rand_art_counts.bought += o_ptr->number;
        o_ptr->marked |= OM_ART_COUNTED;
    }
}

void stats_on_sell(object_type *o_ptr)
{
    if (!(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }
    if (obj_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_find(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }

    if (o_ptr->art_id)
    {
        art_ptr art = arts_lookup(o_ptr->art_id);
        assert(art->generated);
        art->found = TRUE;
    }

    if (o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }
    if (o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
    {
        stats_rand_art_counts.found += o_ptr->number;
        o_ptr->marked |= OM_ART_COUNTED;
    }
}

void stats_on_notice(object_type *o_ptr, int num)
{
    if (!(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += num;
        o_ptr->marked |= OM_COUNTED;
    }

    /* Note: Noticing the effect of a device now identifies the device */

    if (o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += num;
        o_ptr->marked |= OM_EGO_COUNTED;
    }
    if (o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
    {
        stats_rand_art_counts.found += num;
        o_ptr->marked |= OM_ART_COUNTED;
    }
}

void stats_on_combine(object_type *dest, object_type *src)
{
    if (object_is_aware(dest) && !(dest->marked & OM_COUNTED))
    {
        k_info[dest->k_idx].counts.found += dest->number;
        dest->marked |= OM_COUNTED;
    }
    if (object_is_aware(src) && !(src->marked & OM_COUNTED))
    {
        k_info[src->k_idx].counts.found += src->number;
        src->marked |= OM_COUNTED;
    }

    /* Note: Devices no longer stack */
}

void stats_on_use(object_type *o_ptr, int num)
{
    k_info[o_ptr->k_idx].counts.used += num;
    if (o_ptr->name2)
        e_info[o_ptr->name2].counts.used += num;
    if (o_ptr->art_name)
        stats_rand_art_counts.used += num;

    if (obj_is_device(o_ptr))
        device_stats_on_use(o_ptr, num);
}

void stats_on_p_destroy(object_type *o_ptr, int num)
{
    if (object_is_aware(o_ptr) && !(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }
    if (obj_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_find(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }
    if (o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }
    if (o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
    {
        stats_rand_art_counts.found += num;
        o_ptr->marked |= OM_ART_COUNTED;
    }

    k_info[o_ptr->k_idx].counts.destroyed += num;
    if (o_ptr->name2)
        e_info[o_ptr->name2].counts.destroyed += num;
    if (o_ptr->art_name)
        stats_rand_art_counts.destroyed += num;
    if (obj_is_device(o_ptr))
        device_stats_on_destroy(o_ptr);
}

void stats_on_m_destroy(object_type *o_ptr, int num)
{
    k_info[o_ptr->k_idx].counts.destroyed += num;
    if (o_ptr->name2)
        e_info[o_ptr->name2].counts.destroyed += num;
    if (obj_is_device(o_ptr))
        device_stats_on_destroy(o_ptr);
}

void stats_on_pickup(object_type *o_ptr)
{
    if (object_is_aware(o_ptr) && !(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }
    if (obj_is_known(o_ptr) && obj_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_find(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }
    if (obj_is_known(o_ptr) && o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }

    if (obj_is_known(o_ptr) && o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
    {
        stats_rand_art_counts.found += o_ptr->number;
        o_ptr->marked |= OM_ART_COUNTED;
    }
}

void stats_on_equip(object_type *o_ptr)
{
    if (object_is_aware(o_ptr) && !(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }

    if (obj_is_known(o_ptr) && o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }

    if (obj_is_known(o_ptr) && o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
    {
        stats_rand_art_counts.found += o_ptr->number;
        o_ptr->marked |= OM_ART_COUNTED;
    }
}

void stats_on_identify(object_type *o_ptr)
{
    if (!(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }

    if (obj_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_find(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }

    if (o_ptr->art_id)
    {
        art_ptr art = arts_lookup(o_ptr->art_id);
        assert(art->generated);
        art->found = TRUE;
    }

    if (o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }

    if (o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
    {
        stats_rand_art_counts.found += o_ptr->number;
        o_ptr->marked |= OM_ART_COUNTED;
    }
}

/*
 * Something has been "sampled"
 */
void object_tried(object_type *o_ptr)
{
    if (obj_is_device(o_ptr))
        o_ptr->ident |= IDENT_TRIED;
    else
        k_info[o_ptr->k_idx].tried = TRUE;
}

bool object_is_tried(object_type *o_ptr)
{
    if (obj_is_device(o_ptr))
        return (o_ptr->ident & IDENT_TRIED) ? TRUE : FALSE;
    else
        return k_info[o_ptr->k_idx].tried;
}

/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(object_type *o_ptr)
{
    /* Aware item -- use template cost */
    if (object_is_aware(o_ptr)) return (k_info[o_ptr->k_idx].cost);

    /* Analyze the type */
    switch (o_ptr->tval)
    {

        /* Un-aware Food */
        case TV_FOOD: return (5L);

        /* Un-aware Potions */
        case TV_POTION: return (20L);

        /* Un-aware Scrolls */
        case TV_SCROLL: return (20L);

        /* Un-aware Staffs */
        case TV_STAFF: return (70L);

        /* Un-aware Wands */
        case TV_WAND: return (50L);

        /* Un-aware Rods */
        case TV_ROD: return (90L);

        /* Un-aware Rings */
        case TV_RING: return (45L);

        /* Un-aware Amulets */
        case TV_AMULET: return (45L);

        /* Figurines, relative to monster level */
        case TV_FIGURINE:
        {
            int level = mon_race_lookup(o_ptr->race_id)->alloc.lvl;
            if (level < 20) return level*50L;
            else if (level < 30) return 1000+(level-20)*150L;
            else if (level < 40) return 2500+(level-30)*350L;
            else if (level < 50) return 6000+(level-40)*800L;
            else return 14000+(level-50)*2000L;
        }

        case TV_CAPTURE:
            if (!o_ptr->race_id) return 1000L;
            else return ((mon_race_lookup(o_ptr->race_id)->alloc.lvl) * 50L + 1000);
    }

    /* Paranoia -- Oops */
    return (0L);
}

/*
 * Return the "real" price of a "known" item, not including discounts
 *
 */
s32b obj_value_real(object_type *o_ptr)
{
    s32b value;

    /* Dave has been kind enough to come up with much better scoring.
       So use the new algorithms whenever possible.
    */
    if (obj_is_weapon(o_ptr)) return weapon_cost(o_ptr, COST_REAL);
    if (obj_is_ammo(o_ptr)) return ammo_cost(o_ptr, COST_REAL);
    if (o_ptr->tval == TV_BOW) return bow_cost(o_ptr, COST_REAL);
    if (obj_is_armor(o_ptr)) return armor_cost(o_ptr, COST_REAL);
    if (obj_is_jewelry(o_ptr) || (o_ptr->tval == TV_LIGHT && obj_is_art(o_ptr))) return jewelry_cost(o_ptr, COST_REAL);
    if (o_ptr->tval == TV_LIGHT) return light_cost(o_ptr, COST_REAL);
    if (o_ptr->tval == TV_QUIVER) return quiver_cost(o_ptr, COST_REAL);
    if (obj_is_device(o_ptr)) return device_value(o_ptr, COST_REAL);


    /* Hack -- "worthless" items */
    if (!k_info[o_ptr->k_idx].cost) return (0L);

    /* Base cost */
    value = k_info[o_ptr->k_idx].cost;

    switch (o_ptr->tval)
    {
        /* Figurines, relative to monster level */
        case TV_FIGURINE:
        {
            int level = mon_race_lookup(o_ptr->race_id)->alloc.lvl;
            if (level < 20) value = level*50L;
            else if (level < 30) value = 1000+(level-20)*150L;
            else if (level < 40) value = 2500+(level-30)*350L;
            else if (level < 50) value = 6000+(level-40)*800L;
            else value = 14000+(level-50)*2000L;
            break;
        }

        case TV_CAPTURE:
        {
            if (!o_ptr->pval) value = 1000L;
            else value = ((mon_race_lookup(o_ptr->race_id)->alloc.lvl) * 50L + 1000);
            break;
        }

        case TV_CHEST:
        {
            if (!o_ptr->pval) value = 0L;
            break;
        }
    }

    /* Worthless object */
    if (value < 0) return 0L;

    /* Return the value */
    return (value);
}


/*
 * Return the price of an item including plusses (and charges)
 *
 * This function returns the "value" of the given item (qty one)
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
 *
 * Note that discounted items stay discounted forever, even if
 * the discount is "forgotten" by the player via memory loss.
 */
s32b obj_value(object_type *o_ptr)
{
    s32b value;
    if (obj_is_known(o_ptr) || (o_ptr->ident & IDENT_STORE))
    {
        /* Identify now reveals accurate object scoring. The purpose of this
           is to assist the user in determining when to *Id* and object, or,
           better yet, when to equip an object and invest time in learning thru
           direct experience. Scoring is displayed in obj_display() and
           in the home inventory of the player.*/
        value = new_object_cost(o_ptr, COST_REAL);
        if (!value)
            value = obj_value_real(o_ptr);

        if (!obj_is_identified_fully(o_ptr))
        {
            if (obj_is_art(o_ptr))
                value += 1000;
            if (obj_is_cursed(o_ptr))
                value -= value/10;
        }
    }
    else
    {
        value = new_object_cost(o_ptr, 0);
        if (!value)
            value = object_value_base(o_ptr);

        if ( (o_ptr->ident & IDENT_SENSE)
          && (o_ptr->feeling == FEEL_EXCELLENT || o_ptr->feeling == FEEL_AWFUL)
          && obj_is_ego(o_ptr))
        {
            value += 500;
            if (obj_is_ammo(o_ptr))
                value = value / MAX(25, o_ptr->number);
            else
                value = value / o_ptr->number;
        }
        if ( (o_ptr->ident & IDENT_SENSE)
          && (o_ptr->feeling == FEEL_SPECIAL || o_ptr->feeling == FEEL_TERRIBLE)
          && obj_is_art(o_ptr))
        {
            value += 1000;
        }
        if ((o_ptr->ident & IDENT_SENSE) && obj_is_cursed(o_ptr))
            value = (value+2)/3;
    }
    if (o_ptr->discount)
        value -= (value * o_ptr->discount / 100L);
    return value;
}


/*
 * Determines whether an object can be destroyed, and makes fake inscription.
 */
bool can_player_destroy_object(object_type *o_ptr)
{
    /* Artifacts cannot be destroyed */
    if (!obj_is_art(o_ptr) || (o_ptr->rune == RUNE_SACRIFICE)) return TRUE;

    /* If object is unidentified, makes fake inscription */
    if (!obj_is_known(o_ptr))
    {
        byte feel = FEEL_SPECIAL;

        /* Hack -- Handle icky artifacts */
        if (obj_is_cursed(o_ptr) || obj_is_broken(o_ptr)) feel = FEEL_TERRIBLE;

        /* Hack -- inscribe the artifact */
        o_ptr->feeling = feel;

        /* We have "felt" it (again) */
        o_ptr->ident |= IDENT_SENSE;

        /* Combine the pack */
        plr->notice |= PN_OPTIMIZE_PACK;

        /* Window stuff */
        plr->window |= PW_INVEN | PW_EQUIP;

        /* Done */
        return FALSE;
    }

    /* Identified artifact -- Nothing to do */
    return FALSE;
}


/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_kind(int tval, int sval)
{
    int k;
    int num = 0;
    int bk = 0;

    /* Look for it */
    for (k = 1; k < max_k_idx; k++)
    {
        object_kind *k_ptr = &k_info[k];

        /* Require correct tval */
        if (k_ptr->tval != tval) continue;

        /* Found a match */
        if (k_ptr->sval == sval) return (k);

        /* Ignore illegal items */
        if (sval != SV_ANY) continue;

        /* Apply the randomizer */
        ++num;
        if (!one_in_(num)) continue; /* beware the evil of macros! */

        /* Use this value */
        bk = k;
    }

    /* Return this choice */
    if (sval == SV_ANY)
    {
        return bk;
    }

    /* Oops */
    return (0);
}


/*
 * Wipe an object clean.
 */
void object_wipe(object_type *o_ptr)
{
    /* Wipe the structure */
    (void)WIPE(o_ptr, object_type);
}


/*
 * Prepare an object based on an existing object
 */
void object_copy(object_type *o_ptr, object_type *j_ptr)
{
    /* Copy the structure */
    COPY(o_ptr, j_ptr, object_type);
}


/*
 * Prepare an object based on an object kind.
 */
void object_prep(object_type *o_ptr, int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Clear the record */
    object_wipe(o_ptr);

    /* Save the kind index */
    o_ptr->k_idx = k_idx;
    if (k_ptr->tval != TV_GOLD && !store_hack)
    {
        k_ptr->counts.generated++;
    }

    /* Efficiency -- tval/sval */
    o_ptr->tval = k_ptr->tval;
    o_ptr->sval = k_ptr->sval;

    /* Default "pval" */
    o_ptr->pval = k_ptr->pval;

    /* Default number */
    o_ptr->number = 1;

    /* Default weight */
    o_ptr->weight = k_ptr->weight;

    /* Default magic */
    o_ptr->to_h = k_ptr->to_h;
    o_ptr->to_d = k_ptr->to_d;
    o_ptr->to_a = k_ptr->to_a;

    /* Default power */
    o_ptr->ac = k_ptr->ac;
    o_ptr->dd = k_ptr->dd;
    o_ptr->ds = k_ptr->ds;
    o_ptr->mult = k_ptr->mult;

    /* Hack -- worthless items are always "broken" */
    if (k_info[o_ptr->k_idx].cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

    /* Hack -- cursed items are always "cursed" */
    if (k_ptr->gen_flags & (OFG_CURSED)) o_ptr->curse_flags |= (OFC_CURSED);
    if (k_ptr->gen_flags & (OFG_HEAVY_CURSE)) o_ptr->curse_flags |= (OFC_HEAVY_CURSE);
    if (k_ptr->gen_flags & (OFG_PERMA_CURSE)) o_ptr->curse_flags |= (OFC_PERMA_CURSE);
    if (k_ptr->gen_flags & (OFG_RANDOM_CURSE0)) o_ptr->curse_flags |= get_curse(0, o_ptr);
    if (k_ptr->gen_flags & (OFG_RANDOM_CURSE1)) o_ptr->curse_flags |= get_curse(1, o_ptr);
    if (k_ptr->gen_flags & (OFG_RANDOM_CURSE2)) o_ptr->curse_flags |= get_curse(2, o_ptr);
}


/*
 * Help determine an "enchantment bonus" for an object.
 *
 * To avoid floating point but still provide a smooth distribution of bonuses,
 * we simply round the results of division in such a way as to "average" the
 * correct floating point value.
 *
 * This function has been changed. It uses "randnor()" to choose values from
 * a normal distribution, whose mean moves from zero towards the max as the
 * level increases, and whose standard deviation is equal to 1/4 of the max,
 * and whose values are forced to lie between zero and the max, inclusive.
 *
 * Since the "level" rarely passes 100 before Morgoth is dead, it is very
 * rare to get the "full" enchantment on an object, even a deep levels.
 *
 * It is always possible (albeit unlikely) to get the "full" enchantment.
 *
 * A sample distribution of values from "m_bonus(10, L)" is shown below:
 *
 *   L       0     1     2     3     4     5     6     7     8     9    10
 * ---    ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
 *   0   66.37 13.01  9.73  5.47  2.89  1.31  0.72  0.26  0.12  0.09  0.03
 *   8   46.85 24.66 12.13  8.13  4.20  2.30  1.05  0.36  0.19  0.08  0.05
 *  16   30.12 27.62 18.52 10.52  6.34  3.52  1.95  0.90  0.31  0.15  0.05
 *  24   22.44 15.62 30.14 12.92  8.55  5.30  2.39  1.63  0.62  0.28  0.11
 *  32   16.23 11.43 23.01 22.31 11.19  7.18  4.46  2.13  1.20  0.45  0.41
 *  40   10.76  8.91 12.80 29.51 16.00  9.69  5.90  3.43  1.47  0.88  0.65
 *  48    7.28  6.81 10.51 18.27 27.57 11.76  7.85  4.99  2.80  1.22  0.94
 *  56    4.41  4.73  8.52 11.96 24.94 19.78 11.06  7.18  3.68  1.96  1.78
 *  64    2.81  3.07  5.65  9.17 13.01 31.57 13.70  9.30  6.04  3.04  2.64
 *  72    1.87  1.99  3.68  7.15 10.56 20.24 25.78 12.17  7.52  4.42  4.62
 *  80    1.02  1.23  2.78  4.75  8.37 12.04 27.61 18.07 10.28  6.52  7.33
 *  88    0.70  0.57  1.56  3.12  6.34 10.06 15.76 30.46 12.58  8.47 10.38
 *  96    0.27  0.60  1.25  2.28  4.30  7.60 10.77 22.52 22.51 11.37 16.53
 * 104    0.22  0.42  0.77  1.36  2.62  5.33  8.93 13.05 29.54 15.23 22.53
 * 112    0.15  0.20  0.56  0.87  2.00  3.83  6.86 10.06 17.89 27.31 30.27
 * 120    0.03  0.11  0.31  0.46  1.31  2.48  4.60  7.78 11.67 25.53 45.72
 * 128    0.02  0.01  0.13  0.33  0.83  1.41  3.24  6.17  9.57 14.22 64.07
 */
s16b m_bonus(int max, int level)
{
    int bonus, stand, extra, value;


    /* Paranoia -- enforce maximal "level" */
    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;


    /* The "bonus" moves towards the max */
    bonus = ((max * level) / MAX_DEPTH);

    /* Hack -- determine fraction of error */
    extra = ((max * level) % MAX_DEPTH);

    /* Hack -- simulate floating point computations */
    if (randint0(MAX_DEPTH) < extra) bonus++;


    /* The "stand" is equal to one quarter of the max */
    stand = (max / 4);

    /* Hack -- determine fraction of error */
    extra = (max % 4);

    /* Hack -- simulate floating point computations */
    if (randint0(4) < extra) stand++;


    /* Choose an "interesting" value */
    value = randnor(bonus, stand);

    /* Enforce the minimum value */
    if (value < 0) return (0);

    /* Enforce the maximum value */
    if (value > max) return (max);

    /* Result */
    return (value);
}


/*
 * Cheat -- describe a created object for the user
 */
void object_mention(object_type *o_ptr)
{
    char o_name[MAX_NLEN];

    /* Describe */
    object_desc(o_name, o_ptr, (OD_NAME_ONLY | OD_STORE));

    /* Artifact */
    if (obj_is_std_art(o_ptr))
    {
        /* Silly message */
        msg_format("Artifact (%s)", o_name);

    }

    /* Random Artifact */
    else if (o_ptr->art_name)
    {
        msg_print("Random artifact");

    }

    /* Ego-item */
    else if (obj_is_ego(o_ptr))
    {
        /* Silly message */
        msg_format("Ego-item (%s)", o_name);

    }

    /* Normal item */
    else
    {
        /* Silly message */
        msg_format("Object (%s)", o_name);

    }
}

static void dragon_resist(object_type * o_ptr)
{
    int ct = 0;
    do
    {
        if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_DRAGON_FANG && one_in_(3))
            one_ele_slay(o_ptr);
        else if (one_in_(4))
            one_dragon_ele_resistance(o_ptr);
        else
            one_high_resistance(o_ptr);
        ct++;
    }
    while (one_in_(1 + ct));
}

/*
 * Mega-Hack -- Attempt to create one of the "Special Objects"
 */
static bool make_artifact_special(object_type *o_ptr, int level, int mode)
{
    int i;
    int k_idx = 0;
    vec_ptr v;
    bool result = FALSE;

    /* No artifacts in the town */
    if (cave->type->id == D_SURFACE) return FALSE;
    if (no_artifacts) return FALSE;

    /* Themed object */
    if (get_obj_num_hook) return (FALSE);

    /* Check the artifact list (just the "specials") */
    v = arts_filter_special();
    for (i = 0; i < vec_length(v) && !result; i++)
    {
        art_ptr art = vec_get(v, i);

        /* XXX XXX Enforce minimum "depth" (loosely) */
        if (art->level > cave->dun_lvl)
        {
            /* Acquire the "out-of-depth factor" */
            int d = (art->level - cave->dun_lvl) * 2;

            /* Roll for out-of-depth creation */
            if (d > 24 || !one_in_(d)) continue;
        }

        art->tries++; /* Debug for statistics runs */

        /* Artifact "rarity roll" */
        if (!one_in_(art->rarity)) continue;

        /* Find the base object */
        k_idx = lookup_kind(art->tval, art->sval);

        /* XXX XXX Enforce minimum "object" level (loosely) */
        if (k_info[k_idx].level > level)
        {
            /* Acquire the "out-of-depth factor" */
            int d = (k_info[k_idx].level - level) * 5;

            /* Roll for out-of-depth creation */
            if (!one_in_(d)) continue;
        }

        if ( random_artifacts
          && !(art->gen_flags & OFG_FIXED_ART)
          && randint0(100) < random_artifact_pct )
        {
            if (art_create_replacement(o_ptr, art, mode))
                result = TRUE;
        }
        else
        {
            if (art_create_std(o_ptr, art, mode))
                result = TRUE;
        }
    }
    vec_free(v);

    return result;
}


/*
 * Attempt to change an object into an artifact
 *
 * This routine should only be called by "apply_magic()"
 *
 * Note -- see "make_artifact_special()" and "apply_magic()"
 */
static bool make_artifact(object_type *o_ptr, int level, int mode)
{
    int i;
    vec_ptr v;
    bool result = FALSE;

    /* No artifacts in the town */
    if (!level) return FALSE;
    if (no_artifacts) return FALSE;

    /* Paranoia -- no "plural" artifacts */
    if (o_ptr->number != 1) return FALSE;

    /* Check the artifact list (skip the "specials") */
    v = arts_filter(o_ptr);
    for (i = 0; i < vec_length(v) && !result; i++)
    {
        art_ptr art = vec_get(v, i);

        /* XXX XXX Enforce minimum "depth" (loosely) */
        if (art->level > level)
        {
            /* Acquire the "out-of-depth factor" */
            int d = art->level - level;

            /* Roll for out-of-depth creation */
            if (d > 24 || !one_in_(d)) continue;
        }

        art->tries++; /* Debug for statistics runs */

        if (!one_in_(art->rarity)) continue;

        if ( random_artifacts
          && !(art->gen_flags & OFG_FIXED_ART)
          && randint0(100) < random_artifact_pct )
        {
            if (art_create_replacement(o_ptr, art, mode))
                result = TRUE;
        }
        else
        {
            if (art_create_std(o_ptr, art, mode))
                result = TRUE;
        }
    }
    vec_free(v);
    return result;
}

bool add_esp_strong(object_type *o_ptr)
{
    bool nonliv = FALSE;

    switch (randint1(3))
    {
    case 1: add_flag(o_ptr->flags, OF_ESP_EVIL); break;
    case 2: add_flag(o_ptr->flags, OF_TELEPATHY); break;
    case 3: add_flag(o_ptr->flags, OF_ESP_NONLIVING); nonliv = TRUE; break;
    }

    return nonliv;
}


#define MAX_ESP_WEAK 9
void add_esp_weak(object_type *o_ptr, bool extra)
{
    int i = 0;
    int idx[MAX_ESP_WEAK];
    int flg[MAX_ESP_WEAK];
    int n = (extra) ? (3 + randint1(randint1(6))) : randint1(3);
    int left = MAX_ESP_WEAK;

    for (i = 0; i < MAX_ESP_WEAK; i++) flg[i] = i + 1;

    /* Shuffle esp flags */
    for (i = 0; i < n; i++)
    {
        int k = randint0(left--);

        idx[i] = flg[k];

        while (k < left)
        {
            flg[k] = flg[k + 1];
            k++;
        }
    }

    while (n--) switch (idx[n])
    {
    case 1: add_flag(o_ptr->flags, OF_ESP_ANIMAL); break;
    case 2: add_flag(o_ptr->flags, OF_ESP_UNDEAD); break;
    case 3: add_flag(o_ptr->flags, OF_ESP_DEMON); break;
    case 4: add_flag(o_ptr->flags, OF_ESP_ORC); break;
    case 5: add_flag(o_ptr->flags, OF_ESP_TROLL); break;
    case 6: add_flag(o_ptr->flags, OF_ESP_GIANT); break;
    case 7: add_flag(o_ptr->flags, OF_ESP_DRAGON); break;
    case 8: add_flag(o_ptr->flags, OF_ESP_HUMAN); break;
    case 9: add_flag(o_ptr->flags, OF_ESP_GOOD); break;
    }
}

/*
 * Hack -- help pick an item type
 */
static bool item_monster_okay(mon_race_ptr r_ptr)
{
    if (mon_race_is_unique(r_ptr)) return FALSE;
    if (mon_race_is_(r_ptr, "N.shadower")) return FALSE;
    if (mon_race_is_nazgul(r_ptr)) return FALSE;
    if (r_ptr->alloc.flags & RFA_FORCE_DEPTH) return FALSE;
    return TRUE;
}

/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
static void a_m_aux_4(object_type *o_ptr, int level, int power, int mode)
{
    /* Apply magic (good or bad) according to type */
    switch (o_ptr->tval)
    {
        case TV_WHISTLE:
        {
            break;
        }
        case TV_FLASK:
        {
            o_ptr->xtra4 = o_ptr->pval;
            o_ptr->pval = 0;
            break;
        }
        case TV_CAPTURE:
        {
            o_ptr->pval = 0;
            obj_identify(o_ptr);
            break;
        }

        case TV_FIGURINE:
        {
            sym_t r_idx = 0;

            /* Pick a random non-unique monster race */
            mon_alloc_push_filter(item_monster_okay);
            while (1)
            {
                mon_race_ptr race = mon_alloc_choose_aux2(mon_alloc_tbl, cave->difficulty, 0, 0);
                if (!race) break; /* panic */
                if (mon_race_is_(race, "J.tsuchinoko")) continue;
                if (!race->alloc.rarity || race->alloc.rarity > 100) continue;

                /* Prefer less out-of-depth monsters */
                if (cave->difficulty < race->alloc.lvl && !one_in_(race->alloc.lvl - cave->difficulty))
                    continue;

                r_idx = race->id;
                break;
            }
            mon_alloc_pop_filter();

            o_ptr->race_id = r_idx;
            if (one_in_(6)) o_ptr->curse_flags |= OFC_CURSED;
            break;
        }

        case TV_CORPSE:
        {
            sym_t r_idx = 0;
            int count = 100;
            u32b match = 0;

            if (o_ptr->sval == SV_SKELETON)
                match = RF_DROP_SKELETON;
            else if (o_ptr->sval == SV_CORPSE)
                match = RF_DROP_CORPSE;

            mon_alloc_push_filter(item_monster_okay);
            while (--count) /* This loop is spinning forever at deep levels ... */
            {
                mon_race_ptr race = mon_alloc_choose(cave->difficulty);

                if (!race->alloc.rarity || race->alloc.rarity > 100) continue;
                if (!(race->body.flags & match)) continue; /* XXX move this check to the 'filter' XXX */

                /* Prefer less out-of-depth monsters */
                if (cave->difficulty < race->alloc.lvl && !one_in_(race->alloc.lvl - cave->difficulty))
                    continue;

                r_idx = race->id;
                break;
            }
            mon_alloc_pop_filter();

            if (!r_idx) /* defense */
                r_idx = mon_race_parse("p.novice warrior")->id;

            o_ptr->race_id = r_idx;
            obj_identify(o_ptr);
            break;
        }

        case TV_STATUE:
        {
            mon_race_ptr race = vec_random(mon_alloc_tbl);
            o_ptr->race_id = race->id;
            obj_identify(o_ptr);
            break;
        }

        case TV_CHEST:
        {
            byte obj_level = k_info[o_ptr->k_idx].level;

            /* Hack -- skip ruined chests */
            if (obj_level <= 0) break;

            /* Hack -- pick a "difficulty" */
            o_ptr->pval = randint1(obj_level);
            if (o_ptr->sval == SV_CHEST_KANDUME)
            {
                o_ptr->pval = 6;
                o_ptr->xtra3 = cave->difficulty + 5;
            }
            else
                o_ptr->xtra3 = level + 5;

            /* Never exceed "difficulty" of 55 to 59 */
            if (o_ptr->pval > 55) o_ptr->pval = 55 + (byte)randint0(5);

            break;
        }
    }
}


/*
 * Complete the "creation" of an object by applying "magic" to the item
 *
 * This includes not only rolling for random bonuses, but also putting the
 * finishing touches on ego-items and artifacts, giving charges to wands and
 * staffs, giving fuel to lites, and placing traps on chests.
 *
 * In particular, note that "Instant Artifacts", if "created" by an external
 * routine, must pass through this function to complete the actual creation.
 *
 * The base "chance" of the item being "good" increases with the "level"
 * parameter, which is usually derived from the dungeon level, being equal
 * to the level plus 10, up to a maximum of 75. If "good" is true, then
 * the object is guaranteed to be "good". If an object is "good", then
 * the chance that the object will be "great" (ego-item or artifact), also
 * increases with the "level", being equal to half the level, plus 5, up to
 * a maximum of 20. If "great" is true, then the object is guaranteed to be
 * "great". At dungeon level 65 and below, 15/100 objects are "great".
 *
 * If the object is not "good", there is a chance it will be "cursed", and
 * if it is "cursed", there is a chance it will be "broken". These chances
 * are related to the "good" / "great" chances above.
 *
 * Otherwise "normal" rings and amulets will be "good" half the time and
 * "cursed" half the time, unless the ring/amulet is always good or cursed.
 *
 * If "okay" is true, and the object is going to be "great", then there is
 * a chance that an artifact will be created. This is true even if both the
 * "good" and "great" arguments are false. As a total hack, if "great" is
 * true, then the item gets 3 extra "attempts" to become an artifact.
 */
static int _max_f2(obj_ptr obj)
{
    /* XXX I'd like some mechanism to vary object quality at the tval level ... pending obj
     * re-design XXX */
    if (obj_is_ammo(obj)) return 30;
    return 20;
}
bool apply_magic(object_type *o_ptr, int lev, u32b mode)
{
    int i, rolls, f1, f2, power;
    int maxf1 = 75;
    int maxf2 = _max_f2(o_ptr);

    if (mode & AM_QUEST)
        lev += 10;
    if (mode & AM_UNIQUE)
        lev += 7;

    /* Maximum "level" for various things */
    if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

    if (!o_ptr->level)
        o_ptr->level = lev; /* Wizard statistics ... */

    /* Base chance of being "good" */
    f1 = lev + 10;

    /* Maximal chance of being "good" */
    if (f1 > maxf1) f1 = maxf1;

    /* Base chance of being "great" */
    f2 = f1 * 2 / 3;

    /* Maximal chance of being "great" */
    if (f2 > maxf2)
        f2 = maxf2;

    /* Temp Hack: It's a bit too hard to find good rings early on. Note we hack after
       calculating f2! */
    if (o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET)
    {
        f1 += 30;
        if (f1 > maxf1) f1 = maxf1;
    }

    if (plr->good_luck)
    {
        f1 += 5;
        f2 += 2;
    }
    else if(mut_present(MUT_BAD_LUCK))
    {
        f1 -= 5;
        f2 -= 2;
    }

    f1 += virtue_current(VIRTUE_CHANCE) / 50;
    f2 += virtue_current(VIRTUE_CHANCE) / 100;

    /* Assume normal */
    power = 0;

    /* Roll for "good" */
    if ((mode & AM_GOOD) || magik(f1))
    {
        /* Assume "good" */
        power = 1;

        /* Roll for "great" */
        if ((mode & AM_GREAT) || magik(f2))
        {
            power = 2;

            /* Roll for "special" */
            if (mode & AM_SPECIAL) power = 3;
        }
    }

    /* Roll for "cursed" */
    else if (magik((f1+2)/3))
    {
        /* Assume "cursed" */
        power = -1;

        /* Roll for "broken" */
        if (magik(f2))
        {
            power = -2;
        }

        /* "Cursed" items become tedious in the late game ... */
        if ( power == -1
          && o_ptr->tval != TV_RING
          && o_ptr->tval != TV_AMULET
          && !obj_is_device(o_ptr)
          && randint1(lev) > 10 )
        {
            power = 0;
        }
    }

    if (obj_drop_theme && !power && (o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET))
    {
        power = 1;
    }

    if (mode & AM_AVERAGE)
        power = 0;

    /* Apply curse */
    if (mode & AM_CURSED)
    {
        /* Assume 'cursed' */
        if (power > 0)
        {
            power = 0 - power;
        }
        /* Everything else gets more badly cursed */
        else
        {
            power--;
        }
    }

    /* Assume no rolls */
    rolls = 0;

    /* Get one roll if excellent */
    if (power >= 2) rolls = 1;

    /* Hack -- Get four rolls if forced great or special */
    if (mode & (AM_GREAT | AM_SPECIAL)) rolls = 4;

    if (rolls & (mode & AM_UNIQUE)) rolls++;
    if (rolls & (mode & AM_VAULT)) rolls += 3;

    /* Hack -- Get no rolls if not allowed */
    if ((mode & AM_NO_FIXED_ART) || o_ptr->art_id || o_ptr->replacement_art_id) rolls = 0;
    if (mode & AM_AVERAGE) rolls = 0;
    if (mode & AM_FORCE_EGO)
    {
        rolls = 0;
        /* AM_FORCE_EGO is used for granting quest rewards. Ego rings and amulets
           can be achieved with just power 1 ... Indeed, power 2 is often too much! */
        if ((o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET) && lev < 50)
            power = MAX(1, power);
        else
            power = 2;
    }
    else
        apply_magic_ego = 0;

    /* Roll for artifacts if allowed */
    for (i = 0; i < rolls; i++)
    {
        /* Roll for an artifact */
        if (make_artifact(o_ptr, lev, mode)) break;
        if (plr->good_luck && one_in_(77))
        {
            if (make_artifact(o_ptr, lev, mode)) break;
        }
    }

    /* Hack -- Creating an artifact will re-prep the object, zeroing out level field.
       Not everybody calls into artifact.c with a prep'd object, so I guess we need to
       handle this here. */
    if (!o_ptr->level)
        o_ptr->level = lev;

    if (o_ptr->replacement_art_id)
        return TRUE;

    if (o_ptr->art_id)
        return TRUE;

    if (o_ptr->art_name)
        return TRUE;


    /* Apply magic */
    switch (o_ptr->tval)
    {
        case TV_QUIVER:
            obj_create_quiver(o_ptr, lev, power, mode);
            break;

        case TV_HAFTED:
            if (o_ptr->sval == SV_WIZSTAFF)
                device_init(o_ptr, lev, mode);
            /* vvv---- Fall Thru ----vvv */
        case TV_DIGGING:
        case TV_BOW:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            /* I couldn't figure out where to put this ... in many ways,
               Harps are more like rings and amulets, so the aux function that
               normally rolls pvals should always be called ... */
            if (o_ptr->tval == TV_BOW && o_ptr->sval == SV_HARP)
                o_ptr->pval = 1 + m_bonus(2, lev);

            if (power) obj_create_weapon(o_ptr, lev, power, mode);
            break;
        }

        case TV_POLEARM:
        {
            if (power && !(o_ptr->sval == SV_DEATH_SCYTHE)) obj_create_weapon(o_ptr, lev, power, mode);
            break;
        }

        case TV_SWORD:
        {
            if (object_is_(o_ptr, TV_SWORD, SV_DRAGON_FANG) && !(mode & AM_CRAFTING))
            {
                dragon_resist(o_ptr);
                if (!one_in_(3)) power = 0;
            }

            if (!(o_ptr->sval == SV_DOKUBARI))
            {
                if (power) obj_create_weapon(o_ptr, lev, power, mode);
            }
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
            if ( object_is_(o_ptr, TV_CLOAK, SV_ELVEN_CLOAK)
              || object_is_(o_ptr, TV_SOFT_ARMOR, SV_BLACK_CLOTHES) )
            {
                o_ptr->pval = randint1(4);
            }

            if (obj_is_dragon_armor(o_ptr) && o_ptr->tval != TV_DRAG_ARMOR && !(mode & AM_CRAFTING))
            {
                dragon_resist(o_ptr);
                if (!one_in_(3)) power = 0;
            }
            if (power) obj_create_armor(o_ptr, lev, power, mode);
            break;
        case TV_RING:
            if (power) ego_create_ring(o_ptr, lev, power, mode);
            break;
        case TV_AMULET:
            if (power) ego_create_amulet(o_ptr, lev, power, mode);
            break;
        case TV_LIGHT:
            obj_create_light(o_ptr, lev, power, mode);
            break;
        case TV_WAND:
        case TV_ROD:
        case TV_STAFF:
            return obj_create_device(o_ptr, lev, power, mode);
        default:
        {
            a_m_aux_4(o_ptr, lev, power, mode);
            break;
        }
    }

    if ((o_ptr->tval == TV_SOFT_ARMOR) &&
        (o_ptr->sval == SV_ABUNAI_MIZUGI) &&
        (plr->personality == PERS_SEXY || demigod_is_(DEMIGOD_APHRODITE)))
    {
        o_ptr->pval = 3;
        add_flag(o_ptr->flags, OF_STR);
        add_flag(o_ptr->flags, OF_INT);
        add_flag(o_ptr->flags, OF_WIS);
        add_flag(o_ptr->flags, OF_DEX);
        add_flag(o_ptr->flags, OF_CON);
        add_flag(o_ptr->flags, OF_CHR);
    }

    if (o_ptr->art_name)
    {
        if (!store_hack)
            stats_rand_art_counts.generated++;
    }

    /* Examine real objects */
    if (o_ptr->k_idx)
    {
        object_kind *k_ptr = &k_info[o_ptr->k_idx];

        /* Hack -- acquire "broken" flag */
        if (!k_info[o_ptr->k_idx].cost) o_ptr->ident |= (IDENT_BROKEN);

        /* Hack -- acquire "cursed" flag */
        if (k_ptr->gen_flags & (OFG_CURSED)) o_ptr->curse_flags |= (OFC_CURSED);
        if (k_ptr->gen_flags & (OFG_HEAVY_CURSE)) o_ptr->curse_flags |= OFC_HEAVY_CURSE;
        if (k_ptr->gen_flags & (OFG_PERMA_CURSE)) o_ptr->curse_flags |= OFC_PERMA_CURSE;
        if (k_ptr->gen_flags & (OFG_RANDOM_CURSE0)) o_ptr->curse_flags |= get_curse(0, o_ptr);
        if (k_ptr->gen_flags & (OFG_RANDOM_CURSE1)) o_ptr->curse_flags |= get_curse(1, o_ptr);
        if (k_ptr->gen_flags & (OFG_RANDOM_CURSE2)) o_ptr->curse_flags |= get_curse(2, o_ptr);
    }
    return TRUE;
}

static bool _is_favorite_weapon(int tval, int sval)
{
    if (plr->pclass != CLASS_ARCHER)
    {
        object_type forge;
        int         k_idx = lookup_kind(tval, sval);

        object_prep(&forge, k_idx);
        return object_is_favorite(&forge);
    }
    return FALSE;
}

static bool _is_device_class(void)
{
    int class_id = plr->pclass;

    if (class_id == CLASS_MONSTER)
        class_id = get_race()->pseudo_class_id;

    switch (class_id)
    {
    case CLASS_ARCHAEOLOGIST:
    case CLASS_HIGH_MAGE:
    case CLASS_MAGE:
    case CLASS_MAGIC_EATER:
    case CLASS_MIRROR_MASTER:
    case CLASS_NECROMANCER:
    case CLASS_ROGUE:
    case CLASS_SORCERER:
    case CLASS_YELLOW_MAGE:
    case CLASS_GRAY_MAGE:
    case CLASS_BLUE_MAGE:
        return TRUE;
    }
    /* Note: Devicemasters only want their speciality, which is checked below. */
    return FALSE;
}

static bool kind_is_tailored(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    switch (k_ptr->tval)
    {
    case TV_SHIELD:
        if ( plr->pclass == CLASS_NINJA
          || plr->pclass == CLASS_MAULER
          || plr->pclass == CLASS_DUELIST 
          || weaponmaster_is_(WEAPONMASTER_STAVES) )
        {
            return FALSE; /* These classes cannot wear shields */
        }
        else if (plr_allow_martial_arts() || plr->pclass == CLASS_SCOUT)
        {
            int max_wgt = 50 + plr->lev;
            return k_ptr->weight <= max_wgt && equip_can_wield_kind(k_ptr->tval, k_ptr->sval);
        }
        return equip_can_wield_kind(k_ptr->tval, k_ptr->sval);

    case TV_HARD_ARMOR:
    case TV_SOFT_ARMOR:
    case TV_DRAG_ARMOR:
        if ( plr_allow_martial_arts()
          || plr->pclass == CLASS_DUELIST
          || plr->pclass == CLASS_SCOUT
          || plr->pclass == CLASS_NINJA )
        {
            int max_wgt = 100 + 2*plr->lev;
            return k_ptr->weight <= max_wgt && equip_can_wield_kind(k_ptr->tval, k_ptr->sval);
        }
        return equip_can_wield_kind(k_ptr->tval, k_ptr->sval);

    case TV_CLOAK:
    case TV_BOOTS:
    case TV_GLOVES:
    case TV_HELM:
    case TV_CROWN:
    case TV_BOW:
        return equip_can_wield_kind(k_ptr->tval, k_ptr->sval);

    case TV_RING:
    case TV_AMULET:
        if (plr->prace == RACE_MON_RING) return TRUE;
        else return equip_can_wield_kind(k_ptr->tval, k_ptr->sval);

    case TV_SWORD:
    case TV_HAFTED:
    case TV_POLEARM:
    case TV_DIGGING:
        return equip_can_wield_kind(k_ptr->tval, k_ptr->sval)
            && _is_favorite_weapon(k_ptr->tval, k_ptr->sval);

    case TV_SHOT:
        /*return equip_can_wield_kind(TV_BOW, SV_SLING);*/
        return FALSE;

    case TV_BOLT:
        /*return equip_can_wield_kind(TV_BOW, SV_LIGHT_XBOW);*/
        return FALSE;

    case TV_ARROW:
        /*return equip_can_wield_kind(TV_BOW, SV_LONG_BOW);*/
        return FALSE;

    case TV_LIFE_BOOK:
    case TV_SORCERY_BOOK:
    case TV_NATURE_BOOK:
    case TV_CHAOS_BOOK:
    case TV_DEATH_BOOK:
    case TV_TRUMP_BOOK:
    case TV_CRAFT_BOOK:
    case TV_DAEMON_BOOK:
    case TV_CRUSADE_BOOK:
    case TV_NECROMANCY_BOOK:
    case TV_ARMAGEDDON_BOOK:
    case TV_ILLUSION_BOOK:
    case TV_MUSIC_BOOK:
    case TV_HISSATSU_BOOK:
    case TV_HEX_BOOK:
    case TV_BURGLARY_BOOK:
    case TV_BLESS_BOOK:
        return check_book_realm(k_ptr->tval, k_ptr->sval)
            && k_ptr->sval >= SV_BOOK_MIN_GOOD
            && k_ptr->counts.found < 3;
    case TV_RAGE_BOOK:
        return check_book_realm(k_ptr->tval, k_ptr->sval)
            && k_ptr->sval >= SV_BOOK_MIN_GOOD
            && k_ptr->counts.found < 8;

    case TV_WAND:
        return devicemaster_is_(DEVICEMASTER_WANDS)
            || _is_device_class();

    case TV_ROD:
        return devicemaster_is_(DEVICEMASTER_RODS)
            || _is_device_class();

    case TV_STAFF:
        return devicemaster_is_(DEVICEMASTER_STAVES)
            || _is_device_class();

    case TV_POTION:
        return devicemaster_is_(DEVICEMASTER_POTIONS);

    case TV_SCROLL:
        return devicemaster_is_(DEVICEMASTER_SCROLLS);
    }

    return FALSE;
}

static bool _drop_tailored = FALSE;

bool kind_is_great(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    if (_drop_tailored && !kind_is_tailored(k_idx))
        return FALSE;

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- Good unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }

        /* Weapons -- Good unless damaged */
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_DIGGING:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }

        /* Ammo -- Arrows/Bolts are good */
        case TV_BOLT:
        case TV_ARROW:
        {
            return (TRUE);
        }

        /* Books -- High level books are good (except Arcane books) */
        case TV_LIFE_BOOK:
        case TV_SORCERY_BOOK:
        case TV_NATURE_BOOK:
        case TV_CHAOS_BOOK:
        case TV_DEATH_BOOK:
        case TV_TRUMP_BOOK:
        case TV_CRAFT_BOOK:
        case TV_DAEMON_BOOK:
        case TV_CRUSADE_BOOK:
        case TV_NECROMANCY_BOOK:
        case TV_ARMAGEDDON_BOOK:
        case TV_ILLUSION_BOOK:
        case TV_MUSIC_BOOK:
        case TV_HISSATSU_BOOK:
        case TV_HEX_BOOK:
        case TV_BURGLARY_BOOK:
        case TV_BLESS_BOOK:
        {
            if (k_ptr->sval == SV_BOOK_MIN_GOOD) return k_ptr->counts.found < 2; /* Third Spellbooks: I want ?Acquirement to grant these! */
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD + 1) return k_ptr->counts.found < 2;   /* Fourth Spellbooks */
            return FALSE;
        }
        case TV_RAGE_BOOK:
        {
            int max = (plr->pclass == CLASS_RAGE_MAGE) ? 8 : 2;
            if (k_ptr->sval == SV_BOOK_MIN_GOOD) return k_ptr->counts.found < max; /* Third Spellbooks: I want ?Acquirement to grant these! */
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD + 1) return k_ptr->counts.found < max;   /* Fourth Spellbooks */
            return FALSE;
        }
        case TV_POTION:
        {
            if (k_ptr->sval == SV_POTION_LIFE) return TRUE;
            if (k_ptr->sval == SV_POTION_STAR_HEALING) return TRUE;
            if (k_ptr->sval == SV_POTION_AUGMENTATION) return TRUE;
            return FALSE;
        }
        case TV_SCROLL:
        {
            if (k_ptr->sval == SV_SCROLL_ACQUIREMENT) return TRUE;
            if (k_ptr->sval == SV_SCROLL_STAR_ACQUIREMENT) return TRUE;
            if (k_ptr->sval == SV_SCROLL_STAR_DESTRUCTION) return TRUE;
            if (k_ptr->sval == SV_SCROLL_GENOCIDE) return TRUE;
            if (k_ptr->sval == SV_SCROLL_MASS_GENOCIDE) return TRUE;
            if (k_ptr->sval == SV_SCROLL_ARTIFACT) return TRUE;
            if (k_ptr->sval == SV_SCROLL_MANA) return TRUE;
            return FALSE;
        }
        case TV_WAND:
        case TV_STAFF:
        case TV_ROD:
            return TRUE;

        case TV_RING:
        case TV_AMULET:
            return TRUE;
    }

    /* Assume not good */
    return (FALSE);
}

bool kind_is_good(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    if (_drop_tailored && !kind_is_tailored(k_idx))
        return FALSE;

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- Good unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }

        /* Weapons -- Good unless damaged */
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_DIGGING:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }

        /* Ammo -- Arrows/Bolts are good */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:  /* XXX needed for ROOM_THEME_OBJECT in rooms.txt */
        {
            return (TRUE);
        }

        /* Books -- High level books are good (except Arcane books) */
        case TV_LIFE_BOOK:
        case TV_SORCERY_BOOK:
        case TV_NATURE_BOOK:
        case TV_CHAOS_BOOK:
        case TV_DEATH_BOOK:
        case TV_TRUMP_BOOK:
        case TV_CRAFT_BOOK:
        case TV_DAEMON_BOOK:
        case TV_CRUSADE_BOOK:
        case TV_NECROMANCY_BOOK:
        case TV_ARMAGEDDON_BOOK:
        case TV_ILLUSION_BOOK:
        case TV_MUSIC_BOOK:
        case TV_HISSATSU_BOOK:
        case TV_HEX_BOOK:
        case TV_BURGLARY_BOOK:
        case TV_BLESS_BOOK:
        {
            if (k_ptr->sval == SV_BOOK_MIN_GOOD) return k_ptr->counts.found < 2; /* Third Spellbooks */
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD + 1) return k_ptr->counts.found < 2;   /* Fourth Spellbooks */
            return FALSE;
        }
        case TV_RAGE_BOOK:
        {
            int max = (plr->pclass == CLASS_RAGE_MAGE) ? 8 : 2;
            if (k_ptr->sval == SV_BOOK_MIN_GOOD) return k_ptr->counts.found < max; /* Third Spellbooks: I want ?Acquirement to grant these! */
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD + 1) return k_ptr->counts.found < max;   /* Fourth Spellbooks */
            return FALSE;
        }
        case TV_POTION:
        {
            switch (k_ptr->sval)
            {
            case SV_POTION_RESISTANCE: return TRUE;
            case SV_POTION_LIFE: return TRUE;
            case SV_POTION_HEALING: return TRUE;
            case SV_POTION_STAR_HEALING: return TRUE;
            case SV_POTION_AUGMENTATION: return TRUE;
            case SV_POTION_RESTORE_MANA: return one_in_(3);
            case SV_POTION_INC_STR:
            case SV_POTION_INC_INT:
            case SV_POTION_INC_WIS:
            case SV_POTION_INC_DEX:
            case SV_POTION_INC_CON:
            case SV_POTION_INC_CHR: return TRUE;
            }
            return FALSE;
        }
        case TV_SCROLL:
        {
            if (k_ptr->sval == SV_SCROLL_ACQUIREMENT) return TRUE;
            if (k_ptr->sval == SV_SCROLL_FOREST_CREATION) return TRUE;
            if (k_ptr->sval == SV_SCROLL_WALL_CREATION) return TRUE;
            if (k_ptr->sval == SV_SCROLL_VENGEANCE) return TRUE;
            if (k_ptr->sval == SV_SCROLL_STAR_ACQUIREMENT) return TRUE;
            if (k_ptr->sval == SV_SCROLL_STAR_DESTRUCTION) return TRUE;
            if (k_ptr->sval == SV_SCROLL_GENOCIDE) return TRUE;
            if (k_ptr->sval == SV_SCROLL_MASS_GENOCIDE) return TRUE;
            /*if (k_ptr->sval == SV_SCROLL_ARTIFACT) return TRUE;*/
            if (k_ptr->sval == SV_SCROLL_FIRE) return TRUE;
            if (k_ptr->sval == SV_SCROLL_ICE) return TRUE;
            if (k_ptr->sval == SV_SCROLL_CHAOS) return TRUE;
            if (k_ptr->sval == SV_SCROLL_MANA) return TRUE;
            return FALSE;
        }
        case TV_WAND:
        case TV_STAFF:
        case TV_ROD:
            return TRUE;

        case TV_RING:
        case TV_AMULET:
            return TRUE;
    }

    /* Assume not good */
    return (FALSE);
}

typedef bool (*_kind_p)(int k_idx);
static _kind_p _kind_hook1;
static _kind_p _kind_hook2;
static bool _kind_hook(int k_idx) {
    if (_kind_hook1 && !_kind_hook1(k_idx))
        return FALSE;
    if (_kind_hook2 && !_kind_hook2(k_idx))
        return FALSE;
    return TRUE;
}
bool kind_is_other_armor(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_BOOTS: case TV_GLOVES:
    case TV_HELM: case TV_CROWN: case TV_CLOAK:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_helm_cloak(int k_idx) { return obj_kind_is_helmet(k_idx) || obj_kind_is_cloak(k_idx); }
static bool _kind_is_wand(int k_idx)  { return obj_kind_is_(k_idx, TV_WAND, SV_ANY); }
static bool _kind_is_rod(int k_idx)   { return obj_kind_is_(k_idx, TV_ROD, SV_ANY); }
static bool _kind_is_staff(int k_idx) { return obj_kind_is_(k_idx, TV_STAFF, SV_ANY); }
static bool _kind_is_whip(int k_idx)  { return obj_kind_is_(k_idx, TV_HAFTED, SV_WHIP); }
static bool _kind_is_lance(int k_idx) {
    if (k_info[k_idx].tval == TV_POLEARM && (k_info[k_idx].sval == SV_LANCE || k_info[k_idx].sval == SV_HEAVY_LANCE))
        return TRUE;
    return FALSE;
}
static bool _kind_is_bow_quiver(int k_idx) { return obj_kind_is_bow(k_idx) || obj_kind_is_quiver(k_idx); }
static bool _kind_is_bow2(int k_idx) {
    if (k_info[k_idx].tval == TV_BOW && k_info[k_idx].sval != SV_HARP) /* Assume tailored Archer reward, and a harp is just insulting! */
        return TRUE;
    return FALSE;
}
static bool _kind_is_weaponmaster(int k_idx)
{
    obj_kind_ptr kind = &k_info[k_idx];
    return weaponmaster_kind_is_favorite(kind);
}
bool kind_is_bow_ammo(int k_idx) { return obj_kind_is_bow(k_idx) || obj_kind_is_ammo(k_idx); }
typedef struct {
    _kind_p hook;
    int     base;
    int     good;
    int     great;
    int     slot_type;
} _kind_alloc_entry;
static _kind_alloc_entry _kind_alloc_table[] = {
    /* Equipment by Slot */
    { obj_kind_is_weapon,      195,    0,    0, EQUIP_SLOT_WEAPON }, /* jellies */
    { obj_kind_is_shield,       40,    0,    0, EQUIP_SLOT_WEAPON_SHIELD },
    { _kind_is_bow_quiver,      60,    0,    0, EQUIP_SLOT_BOW },
    { obj_kind_is_ring,         25,    0,    0, EQUIP_SLOT_RING },   /* beholders = rings only */
    { obj_kind_is_amulet,       25,    0,    0, EQUIP_SLOT_AMULET }, /* hydras = amulets only */
    { obj_kind_is_light,        10,    0,    0, EQUIP_SLOT_LIGHT },
    { obj_kind_is_body_armor,  160,    0,    0, EQUIP_SLOT_BODY_ARMOR },
    { obj_kind_is_cloak,        40,    0,    0, EQUIP_SLOT_CLOAK },
    { obj_kind_is_helmet,       40,    0,    0, EQUIP_SLOT_HELMET },
    { obj_kind_is_gloves,       40,    0,    0, EQUIP_SLOT_GLOVES },
    { obj_kind_is_boots,        40,    0,    0, EQUIP_SLOT_BOOTS },
    /*                         675              */

    { obj_kind_is_device,       90,  -40,  -60, EQUIP_SLOT_NONE },
    { obj_kind_is_potion,       50,  -25,  -45, EQUIP_SLOT_NONE },
    { obj_kind_is_scroll,       35,  -25,  -45, EQUIP_SLOT_NONE },
    { obj_kind_is_ammo,         75,    0,    0, EQUIP_SLOT_BOW },
    { obj_kind_is_spellbook,    25,   10,   15, EQUIP_SLOT_NONE }, /* R_DROP_MAGE is covering this ... */
    { obj_kind_is_food,         25,  -50,  -50, EQUIP_SLOT_NONE },
    { obj_kind_is_misc,         25,  -50,  -50, EQUIP_SLOT_NONE },
    /*                         325              */
    { NULL, 0}
};
static int _kind_alloc_weight(_kind_alloc_entry *entry, u32b mode)
{
    int w = 0;
    w = entry->base;
    if (mode & AM_GREAT)
        w += entry->great;
    else if (mode & AM_GOOD)
        w += entry->good;

    if (plr->prace == RACE_MON_RING && entry->hook == obj_kind_is_jewelry)
        w = w * 2;

    /* EXPERIMENTAL: Adjust frequencies of unusable objects down. For example, hounds
     * can neither wield weapons nor employ archery. Beholders are much worse, only using
     * rings, lights and a helmet. The effect should be noticeable, but not too strong. */
    if (entry->slot_type != EQUIP_SLOT_NONE && !equip_has_slot_type(entry->slot_type))
        w /= 2;
    else if (entry->slot_type == EQUIP_SLOT_WEAPON && plr_allow_martial_arts())
        w /= 2;

    return MAX(0, w);
}
/****************** Themed Drops ***************************************/
static bool _kind_is_(int k_idx, int tval, int sval)
{
    if ( k_info[k_idx].tval == tval
      && (sval == SV_ANY || k_info[k_idx].sval == sval) )
    {
        return TRUE;
    }
    return FALSE;
}
static bool _kind_theme_warrior(int k_idx) {
    obj_kind_ptr k = &k_info[k_idx];
    switch (k->tval)
    {
    case TV_SWORD:
        if (k_info[k_idx].sval >= SV_SABRE && k_info[k_idx].sval < SV_POISON_NEEDLE)
            return TRUE;
        break;
    case TV_POLEARM:
    case TV_BOOTS:
    case TV_GLOVES:
    case TV_HELM:
    case TV_SHIELD:
    case TV_HARD_ARMOR:
    case TV_DRAG_ARMOR:
        return TRUE;
    case TV_RING:
    case TV_AMULET:
        return one_in_(3);
    case TV_POTION:
        switch (k->sval)
        {
        case SV_POTION_HEROISM:
        case SV_POTION_BERSERK_STRENGTH:
            return TRUE;
        case SV_POTION_INC_STR:
        case SV_POTION_INC_DEX:
        case SV_POTION_INC_CON:
            return TRUE; /* one_in_(2) */;
        }
        return FALSE;
    }
    return FALSE;
}
static bool _kind_theme_warrior_shoot(int k_idx) {
    if ( _kind_is_(k_idx, TV_BOW,  SV_LIGHT_XBOW)
      || _kind_is_(k_idx, TV_BOW,  SV_HEAVY_XBOW)
      || _kind_is_(k_idx, TV_BOLT, SV_ANY)  )
    {
        return TRUE;
    }
    return _kind_theme_warrior(k_idx);
}
static bool _kind_theme_archer(int k_idx) {
    if ( _kind_is_(k_idx, TV_BOW,  SV_ANY)
      || _kind_is_(k_idx, TV_QUIVER, SV_ANY)
      || _kind_is_(k_idx, TV_ARROW,  SV_ANY) )
    {
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_RING)
        return TRUE;
    return FALSE;
}
static bool _kind_theme_mage(int k_idx) {
    if ( _kind_is_(k_idx, TV_HAFTED, SV_WIZSTAFF)
      || _kind_is_(k_idx, TV_SOFT_ARMOR, SV_ROBE) )
    {
        return TRUE;
    }
    switch (k_info[k_idx].tval)
    {
    case TV_RING:
    case TV_AMULET:
    case TV_WAND:
    case TV_ROD:
    case TV_STAFF:
    case TV_SORCERY_BOOK:
    case TV_NATURE_BOOK:
    case TV_CHAOS_BOOK:
    case TV_DEATH_BOOK:
    case TV_TRUMP_BOOK:
    case TV_ARCANE_BOOK:
    case TV_CRAFT_BOOK:
    case TV_DAEMON_BOOK:
    case TV_ARMAGEDDON_BOOK:
    case TV_ILLUSION_BOOK:
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_RESTORE_MANA:
        case SV_POTION_ENLIGHTENMENT:
        case SV_POTION_STAR_ENLIGHTENMENT:
        case SV_POTION_SELF_KNOWLEDGE:
        case SV_POTION_INVULNERABILITY:
        case SV_POTION_CLARITY:
        case SV_POTION_GREAT_CLARITY:
        case SV_POTION_INC_INT:
            return TRUE;
        }
    }
    if (k_info[k_idx].tval == TV_SCROLL)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_SCROLL_PHASE_DOOR:
        case SV_SCROLL_TELEPORT:
        case SV_SCROLL_TELEPORT_LEVEL:
        case SV_SCROLL_WORD_OF_RECALL:
        case SV_SCROLL_IDENTIFY:
        case SV_SCROLL_STAR_IDENTIFY:
        case SV_SCROLL_RECHARGING:
        case SV_SCROLL_STAR_DESTRUCTION:
        case SV_SCROLL_GENOCIDE:
        case SV_SCROLL_MASS_GENOCIDE:
        case SV_SCROLL_ACQUIREMENT:
        case SV_SCROLL_STAR_ACQUIREMENT:
        case SV_SCROLL_ARTIFACT:
        case SV_SCROLL_DETECT_MONSTERS:
        case SV_SCROLL_FIRE:
        case SV_SCROLL_ICE:
        case SV_SCROLL_CHAOS:
        case SV_SCROLL_MANA:
        case SV_SCROLL_BANISHMENT:
            return TRUE;
        }
    }
    return FALSE;
}
static bool _kind_theme_priest(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_HAFTED:
    case TV_LIFE_BOOK:
    case TV_CRUSADE_BOOK:
    case TV_AMULET:
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_CURE_LIGHT:
        case SV_POTION_CURE_SERIOUS:
        case SV_POTION_CURE_CRITICAL:
        case SV_POTION_HEALING:
        case SV_POTION_STAR_HEALING:
        case SV_POTION_LIFE:
        case SV_POTION_RESTORE_EXP:
        case SV_POTION_RES_STR:
        case SV_POTION_RES_INT:
        case SV_POTION_RES_WIS:
        case SV_POTION_RES_DEX:
        case SV_POTION_RES_CON:
        case SV_POTION_RES_CHR:
        case SV_POTION_INC_WIS:
        case SV_POTION_INC_CHR:
            return TRUE;
        }
    }
    if (k_info[k_idx].tval == TV_SCROLL)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_SCROLL_REMOVE_CURSE:
        case SV_SCROLL_STAR_REMOVE_CURSE:
        case SV_SCROLL_BLESSING:
        case SV_SCROLL_HOLY_CHANT:
        case SV_SCROLL_HOLY_PRAYER:
        case SV_SCROLL_PROTECTION_FROM_EVIL:
        case SV_SCROLL_RUNE_OF_PROTECTION:
        case SV_SCROLL_DISPEL_UNDEAD:
            return TRUE;
        }
    }
    return FALSE;
}
static bool _kind_theme_priest_evil(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_HAFTED:
    case TV_DEATH_BOOK:
    case TV_DAEMON_BOOK:
    case TV_AMULET:
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_DEATH:
        case SV_POTION_INC_WIS:
        case SV_POTION_INC_CHR:
            return TRUE;
        }
    }
    if (k_info[k_idx].tval == TV_SCROLL)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_SCROLL_GENOCIDE:
        case SV_SCROLL_MASS_GENOCIDE:
            return TRUE;
        }
    }
    return FALSE;
}
static bool _kind_theme_paladin(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_LIFE_BOOK:
    case TV_CRUSADE_BOOK:
    case TV_RING:
    case TV_AMULET:
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_CURE_LIGHT:
        case SV_POTION_CURE_SERIOUS:
        case SV_POTION_CURE_CRITICAL:
        case SV_POTION_HEALING:
        case SV_POTION_STAR_HEALING:
        case SV_POTION_LIFE:
        case SV_POTION_INC_STR:
        case SV_POTION_INC_WIS:
        case SV_POTION_INC_CHR:
            return TRUE;
        }
    }
    if (k_info[k_idx].tval == TV_SCROLL)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_SCROLL_BLESSING:
        case SV_SCROLL_HOLY_CHANT:
        case SV_SCROLL_HOLY_PRAYER:
        case SV_SCROLL_PROTECTION_FROM_EVIL:
        case SV_SCROLL_VENGEANCE:
            return TRUE;
        }
    }
    return _kind_theme_warrior(k_idx);
}
static bool _kind_theme_paladin_evil(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_DEATH_BOOK:
    case TV_DAEMON_BOOK:
    case TV_RING:
    case TV_AMULET:
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_DEATH:
        case SV_POTION_INC_STR:
        case SV_POTION_INC_WIS:
        case SV_POTION_INC_CHR:
            return TRUE;
        }
    }
    if (k_info[k_idx].tval == TV_SCROLL)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_SCROLL_GENOCIDE:
        case SV_SCROLL_MASS_GENOCIDE:
        case SV_SCROLL_STAR_DESTRUCTION:
        case SV_SCROLL_VENGEANCE:
            return TRUE;
        }
    }
    return _kind_theme_warrior(k_idx);
}
static bool _kind_theme_samurai(int k_idx) {
    if ( _kind_is_(k_idx, TV_SWORD, SV_KATANA)
      || _kind_is_(k_idx, TV_SWORD, SV_WAKIZASHI)
      || _kind_is_(k_idx, TV_HARD_ARMOR, SV_HARAMAKIDO) )
    {
        return TRUE;
    }
    switch (k_info[k_idx].tval)
    {
    case TV_RING:
    case TV_HISSATSU_BOOK:
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_INC_STR:
        case SV_POTION_INC_WIS:
            return TRUE;
        }
    }
    return FALSE;
}
static bool _kind_theme_ninja(int k_idx) {
    if ( _kind_is_(k_idx, TV_SWORD, SV_POISON_NEEDLE)
      || _kind_is_(k_idx, TV_SWORD, SV_FALCON_SWORD)
      || _kind_is_(k_idx, TV_SWORD, SV_DAGGER)
      || _kind_is_(k_idx, TV_SWORD, SV_NINJATO)
      || _kind_is_(k_idx, TV_SOFT_ARMOR, SV_KUROSHOUZOKU) )
    {
        return TRUE;
    }
    switch (k_info[k_idx].tval)
    {
    case TV_AMULET:
    case TV_SPIKE:
        return TRUE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_INC_DEX:
            return TRUE;
        }
    }
    return FALSE;
}
static bool _kind_theme_rogue(int k_idx) {
    if (_kind_is_(k_idx, TV_BOW, SV_SLING))
        return TRUE;

    switch (k_info[k_idx].tval)
    {
    case TV_AMULET:
    case TV_SOFT_ARMOR:
    case TV_CLOAK:
    case TV_GLOVES:
    case TV_SHOT:
    case TV_BURGLARY_BOOK:
        return TRUE;

    case TV_SWORD:
        if (k_info[k_idx].weight < 50)
            return TRUE;
        return FALSE;
    }
    if (k_info[k_idx].tval == TV_POTION)
    {
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_INC_DEX:
            return TRUE;
        }
    }
    return FALSE;
}
static bool _kind_theme_hobbit(int k_idx) {
    if ( _kind_is_(k_idx, TV_FOOD, SV_ANY)
      || _kind_is_(k_idx, TV_BOW, SV_SLING)
      || _kind_is_(k_idx, TV_SHOT, SV_PEBBLE) )
    {
        return TRUE;
    }
    return FALSE;
}
static bool _kind_theme_dwarf(int k_idx) {
    if ( _kind_is_(k_idx, TV_POLEARM, SV_LOCHABER_AXE)
      || _kind_is_(k_idx, TV_POLEARM, SV_BEAKED_AXE)
      || _kind_is_(k_idx, TV_POLEARM, SV_BROAD_AXE)
      || _kind_is_(k_idx, TV_POLEARM, SV_BATTLE_AXE)
      || _kind_is_(k_idx, TV_POLEARM, SV_GREAT_AXE)
      || _kind_is_(k_idx, TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS)
      || _kind_is_(k_idx, TV_DIGGING, SV_DWARVEN_PICK)
      || _kind_is_(k_idx, TV_DIGGING, SV_DWARVEN_SHOVEL)
      || _kind_is_(k_idx, TV_DIGGING, SV_MATTOCK)
      || _kind_is_(k_idx, TV_HELM, SV_IRON_HELM)
      || _kind_is_(k_idx, TV_HELM, SV_STEEL_HELM)
      || _kind_is_(k_idx, TV_SHIELD, SV_SMALL_METAL_SHIELD)
      || _kind_is_(k_idx, TV_HARD_ARMOR, SV_MITHRIL_CHAIN_MAIL)
      || _kind_is_(k_idx, TV_HARD_ARMOR, SV_MITHRIL_PLATE_MAIL) )
    {
        return TRUE;
    }
    switch (k_info[k_idx].tval)
    {
    case TV_AMULET:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_theme_junk(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_SKELETON:
    case TV_BOTTLE:
    case TV_JUNK:
    case TV_SPIKE:
    case TV_CORPSE:
    case TV_LIGHT:
    case TV_FLASK:
    case TV_FOOD:
        return TRUE;
    }
    return FALSE;

}

static bool _needs_book(void)
{
    int k_idx;
    if (plr->pclass == CLASS_SORCERER || plr->pclass == CLASS_RAGE_MAGE)
    {
        return TRUE;
    }
    else if (plr->pclass == CLASS_RED_MAGE)
    {
        return FALSE;
    }
    else if (plr->pclass == CLASS_GRAY_MAGE)
    {
        vec_ptr v = tv_lookup_(TVF_SPELLBOOK);
        int     i;
        bool    result = FALSE;
        for (i = 0; i < vec_length(v) && !result; i++)
        {
            tv_info_ptr info = vec_get(v, i);
            if (!gray_mage_is_allowed_book(info->id, 0)) continue;
            k_idx = lookup_kind(info->id, 2);
            if (k_info[k_idx].counts.found > 2) continue;
            k_idx = lookup_kind(info->id, 3);
            if (k_info[k_idx].counts.found > 1) continue;
            result = TRUE;
        }
        vec_free(v);
        return result;
    }
    else if (plr->pclass == CLASS_SKILLMASTER)
    {
        vec_ptr v = tv_lookup_(TVF_SPELLBOOK);
        int     i;
        bool    result = FALSE;
        for (i = 0; i < vec_length(v) && !result; i++)
        {
            tv_info_ptr info = vec_get(v, i);
            if (!skillmaster_is_allowed_book(info->id, 0)) continue;
            k_idx = lookup_kind(info->id, 2);
            if (k_info[k_idx].counts.found > 2) continue;
            k_idx = lookup_kind(info->id, 3);
            if (k_info[k_idx].counts.found > 1) continue;
            result = TRUE;
        }
        vec_free(v);
        return result;
    }
    if (plr->realm1)
    {
        k_idx = lookup_kind(REALM1_BOOK, 2);
        if (k_info[k_idx].counts.found < 3) return TRUE;
        k_idx = lookup_kind(REALM1_BOOK, 3);
        if (k_info[k_idx].counts.found < 2) return TRUE;
    }
    if (plr->realm2)
    {
        k_idx = lookup_kind(REALM2_BOOK, 2);
        if (k_info[k_idx].counts.found < 3) return TRUE;
        k_idx = lookup_kind(REALM2_BOOK, 3);
        if (k_info[k_idx].counts.found < 2) return TRUE;
    }
    return FALSE;
}
/****************** End of Themed Drops ***************************************/

static _kind_p _choose_obj_kind(u32b mode)
{
    int  i;
    int  tot = 0;

    _kind_hook1 = NULL;
    _kind_hook2 = NULL;

    if (mode & AM_GREAT)
        _kind_hook2 = kind_is_great;
    else if (mode & AM_GOOD)
        _kind_hook2 = kind_is_good;

    if (obj_drop_theme)
    {
        switch (obj_drop_theme)
        {
        case R_DROP_WARRIOR:
            _kind_hook1 = _kind_theme_warrior;
            break;
        case R_DROP_WARRIOR_SHOOT:
            _kind_hook1 = _kind_theme_warrior_shoot;
            break;
        case R_DROP_ARCHER:
            _kind_hook1 = _kind_theme_archer;
            break;
        case R_DROP_MAGE:
            _kind_hook1 = _kind_theme_mage;
            break;
        case R_DROP_PRIEST:
            _kind_hook1 = _kind_theme_priest;
            break;
        case R_DROP_PRIEST_EVIL:
            _kind_hook1 = _kind_theme_priest_evil;
            break;
        case R_DROP_PALADIN:
            _kind_hook1 = _kind_theme_paladin;
            break;
        case R_DROP_PALADIN_EVIL:
            _kind_hook1 = _kind_theme_paladin_evil;
            break;
        case R_DROP_SAMURAI:
            _kind_hook1 = _kind_theme_samurai;
            break;
        case R_DROP_NINJA:
            _kind_hook1 = _kind_theme_ninja;
            break;
        case R_DROP_ROGUE:
            _kind_hook1 = _kind_theme_rogue;
            break;
        case R_DROP_HOBBIT:
            _kind_hook1 = _kind_theme_hobbit;
            break;
        case R_DROP_DWARF:
            _kind_hook1 = _kind_theme_dwarf;
            break;
        case R_DROP_JUNK:
            _kind_hook1 = _kind_theme_junk;
            break;
        case R_DROP_SPELLBOOK:  /* e.g. Smaug reward */
            _kind_hook1 = obj_kind_is_spellbook;
            if (_drop_tailored)
                _kind_hook2 = kind_is_tailored;
            break;
        }
    }
    /* Circumvent normal object kind frequencies for good, great and tailored drops.
     * The goal here is, for example, to allow mages to find adequate spellbooks without
     * drowning other characters with useless junk.
     * Note: Themed drops (above) circumvent tailored objects ...*/
    else if (_drop_tailored)
    {
        _kind_hook2 = kind_is_tailored;
        switch (plr->pclass)
        {
        case CLASS_ARCHAEOLOGIST:
            if (one_in_(5))
                _kind_hook1 = _kind_is_whip; /* Diggers swamp out whips ... */
            else if (one_in_(5))
                _kind_hook1 = obj_kind_is_weapon;
            else if (one_in_(5))
                _kind_hook1 = obj_kind_is_device;
            break;
        case CLASS_WEAPONMASTER:
            if (one_in_(3))
                _kind_hook1 = _kind_is_weaponmaster;
            break;
        case CLASS_ARCHER:
        case CLASS_SNIPER:
            if (one_in_(5))
                _kind_hook1 = _kind_is_bow2;
            break;
        case CLASS_DEVICEMASTER:
            if (one_in_(5))
            {
                switch (plr->psubclass)
                {
                case DEVICEMASTER_POTIONS:
                    _kind_hook1 = obj_kind_is_potion;
                    break;
                case DEVICEMASTER_SCROLLS:
                    _kind_hook1 = obj_kind_is_scroll;
                    break;
                case DEVICEMASTER_RODS:
                    _kind_hook1 = _kind_is_rod;
                    break;
                case DEVICEMASTER_WANDS:
                    _kind_hook1 = _kind_is_wand;
                    break;
                case DEVICEMASTER_STAVES:
                    _kind_hook1 = _kind_is_staff;
                    break;
                }
            }
            break;
        case CLASS_MAGIC_EATER:
            if (one_in_(5))
                _kind_hook1 = obj_kind_is_device;
            break;
        case CLASS_RAGE_MAGE:
            if (one_in_(3))
                _kind_hook1 = obj_kind_is_spellbook;
            break;
        case CLASS_CAVALRY:
        case CLASS_BEASTMASTER:
            if (one_in_(7))
                _kind_hook1 = _kind_is_lance;
            break;

        case CLASS_MONSTER:
            switch (plr->prace)
            {
            case RACE_MON_BEHOLDER:
                if (one_in_(5))
                    _kind_hook1 = obj_kind_is_ring;
                else if (one_in_(7))
                    _kind_hook1 = obj_kind_is_helmet;
                break;
            case RACE_MON_CENTIPEDE:
                if (one_in_(7))
                    _kind_hook1 = obj_kind_is_boots;
                break;
            case RACE_MON_DRAGON:
                if (one_in_(5))
                    _kind_hook1 = obj_kind_is_ring;
                else if (one_in_(7))
                    _kind_hook1 = _kind_is_helm_cloak;
                break;
            case RACE_MON_HYDRA:
                if (one_in_(5))
                    _kind_hook1 = obj_kind_is_amulet;
                else if (one_in_(7))
                    _kind_hook1 = obj_kind_is_helmet;
                break;
            }
            break;
        }
        if (!_kind_hook1 && mut_present(MUT_DRACONIAN_METAMORPHOSIS))
        {
            if (one_in_(5))
                _kind_hook1 = obj_kind_is_ring;
            else if (one_in_(7))
                _kind_hook1 = _kind_is_helm_cloak;
        }
        if (!_kind_hook1)
        {
            if (_needs_book() && one_in_(10))
                _kind_hook1 = obj_kind_is_spellbook;
            else if (_is_device_class() && one_in_(7))
                _kind_hook1 = obj_kind_is_device;
        }
    }
    else if (plr->pclass == CLASS_WEAPONMASTER && one_in_(50))
        _kind_hook1 = _kind_is_weaponmaster;

    /* Otherwise, pick the kind of drop using the allocation table defined above.
     * This allows us to design, say, 3% of drops to be jewelry, 4% potions, or
     * whatever. */
    if (!_kind_hook1)
    {
        for (i = 0; ; i++)
        {
            if (!_kind_alloc_table[i].hook) break;
            tot += _kind_alloc_weight(&_kind_alloc_table[i], mode);
        }

        if (tot > 0)
        {
            int j = randint0(tot);

            for (i = 0; ; i++)
            {
                if (!_kind_alloc_table[i].hook) break;
                j -= _kind_alloc_weight(&_kind_alloc_table[i], mode);
                if (j < 0)
                {
                    _kind_hook1 = _kind_alloc_table[i].hook;
                    break;
                }
            }
        }
    }
    return _kind_hook;
}

void choose_obj_kind(int mode)
{
    if (!get_obj_num_hook)
        get_obj_num_hook = _choose_obj_kind(mode);
}

/*
 * Attempt to make an object (normal or good/great)
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * We assume that the given object has been "wiped".
 */
static bool _make_object_aux(object_type *j_ptr, int level, u32b mode)
{
    int prob, base, k_idx;

    /* Chance of "special object" */
    prob = ((mode & AM_GOOD) ? 10 : 1000);

    /* Base level for the object */
    base = ((mode & AM_GOOD) ? (level + 10) : level);

    /* Try for a "special" INSTA_ART (e.g. artifact lightsources) */
    if (one_in_(prob) && make_artifact_special(j_ptr, level, mode))
        return TRUE;

    /* pick an object type */
    if (!get_obj_num_hook)
        get_obj_num_hook = _choose_obj_kind(mode);

    if (get_obj_num_hook)
        get_obj_num_prep();

    k_idx = get_obj_num(base);

    if (get_obj_num_hook)
    {
        get_obj_num_hook = NULL;
        get_obj_num_prep();
    }
    if (!k_idx) return FALSE;

    /* Prepare the object */
    object_prep(j_ptr, k_idx);

    /* Apply magic (allow artifacts) */
    if (!apply_magic(j_ptr, level, mode))
        return FALSE;

    /* Note: It is important to do this *after* apply_magic rather than in, say,
       object_prep() since artifacts should never spawn multiple copies. Ego ammo
       should, but other egos (e.g. lights) should not. */
    obj_make_pile(j_ptr);

    return TRUE;
}

bool make_object(obj_ptr obj, int level, u32b mode)
{
    bool result = FALSE;
    int max_attempts = 1;
    int attempt = 0;

    _drop_tailored = FALSE;
    if (mode & AM_TAILORED)
    {
        _drop_tailored = TRUE;
        max_attempts = 1000; /* Tailored drops can fail for certain _choose_obj_kind()s */
    }
    else if ((mode & (AM_GOOD | AM_GREAT)) && !get_obj_num_hook)
    {
        max_attempts = 100; /* AM_GOOD devices often fail before OL50 ... see the effect tables */
    }

    for (; attempt < max_attempts; attempt++)
    {
        if (_make_object_aux(obj, level, mode))
        {
            result = TRUE;
            break;
        }
        object_wipe(obj);
    }
    obj_drop_theme = 0;
    _drop_tailored = FALSE;
    return result;
}

/*
 * Attempt to place an object (normal or good/great) at the given location.
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine requires a clean floor grid destination.
 */
void place_object(point_t pos, int level, u32b mode)
{
    object_type forge = {0};

    if (!dun_pos_interior(cave, pos)) return;
    if (!dun_allow_drop_at(cave, pos)) return;
    if (dun_obj_at(cave, pos)) return;

    if (!make_object(&forge, level, mode)) return;
    dun_place_obj(cave, &forge, pos);
    note_pos(pos);
    draw_pos(pos);
}

/*
 * Make a treasure object. Pass a non-zero boost argument
 * to increase the gold amounts for monster drops, chests,
 * and vault tiles. Typically, this will be cave->difficulty.
 */
bool make_gold_aux(obj_ptr obj, int sval, int boost)
{
    int au, base, k_idx, i;
    int rolls = 1 + boost/10;

    k_idx = lookup_kind(TV_GOLD, sval);
    if (!k_idx) return FALSE;

    /* Prepare a gold object */
    object_prep(obj, k_idx);

    /* Hack -- Base coin cost */
    base = k_info[k_idx].cost;

    /* Determine how much the treasure is "worth" */
    au = base + randint1(8);
    for (i = 0; i < rolls; i++)
        au += damroll(8, base);
    au = au * (625 - virtue_current(VIRTUE_SACRIFICE)) / 625;
    if (au > MAX_SHORT)
        au = MAX_SHORT;
    obj->pval = au;

    return TRUE;
}
bool make_gold(object_type *j_ptr, int boost)
{
    int i;

    /* Hack -- Pick a Treasure variety */
    i = ((randint1(cave->difficulty + 2) + 2) / 2);

    /* Apply "extra" magic */
    if (one_in_(GREAT_OBJ))
    {
        i += randint1(cave->difficulty + 1);
    }

    /* Do not create "illegal" Treasure Types */
    if (i > SV_MAX_GOLD) i = SV_MAX_GOLD;

    /* Prepare a gold object */
    return make_gold_aux(j_ptr, i, boost);
}


/*
 * Places a treasure (Gold or Gems) at given location
 *
 * The location must be a legal, clean, floor grid.
 */
void place_gold(point_t pos)
{
    object_type forge = {0};

    if (!dun_pos_interior(cave, pos)) return;
    if (!dun_allow_drop_at(cave, pos)) return;
    if (dun_obj_at(cave, pos)) return;

    if (!make_gold(&forge, 0)) return;

    dun_place_obj(cave, &forge, pos);
    note_pos(pos);
    draw_pos(pos);
}


/*
 * Let an object fall to the ground at or near a location.
 *
 * The initial location is assumed to be "in_bounds()".
 *
 * This function takes a parameter "chance". This is the percentage
 * chance that the item will "disappear" instead of drop. If the object
 * has been thrown, then this is the chance of disappearance on contact.
 *
 * Hack -- this function uses "chance" to determine if it should produce
 * some form of "description" of the drop event (under the player).
 *
 * We check several locations to see if we can find a location at which
 * the object can combine, stack, or be placed. Artifacts will try very
 * hard to be placed, including "teleporting" to a useful grid if needed.
 *
 * Returns TRUE if the object was dropped, FALSE if it was broken
 * or a legal drop location could not be found.
 */
bool drop_near(obj_ptr obj, point_t pos, int break_chance)
{
    if (break_chance > 0)
        return dun_drop_break_near(cave, obj, pos, break_chance);
    return dun_drop_near(cave, obj, pos);
    #if 0
    if (object_is_(j_ptr, TV_POTION, SV_POTION_BLOOD))
    {
        msg_print("The potion goes sour.");
        j_ptr->sval = SV_POTION_SALT_WATER;
        j_ptr->k_idx = lookup_kind(TV_POTION, SV_POTION_SALT_WATER);
    }
    #endif
}


/*
 * Scatter some "great" objects near the player
 */
void acquirement(int y1, int x1, int num, bool great, bool known)
{
    object_type *i_ptr;
    object_type object_type_body;
    u32b mode = AM_GOOD | (great ? (AM_GREAT | AM_TAILORED) : 0L);
    int  attempt = 0;

    /* Acquirement */
    while (num && attempt < 1000)
    {
        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        object_wipe(i_ptr);
        attempt++;

        /* Make a good (or great) object (if possible) */
        if (!make_object(i_ptr, cave->difficulty, mode)) continue;

        num--;
        if (known)
        {
            obj_identify(i_ptr);
        }

        /* Drop the object */
        drop_near(i_ptr, point_create(x1, y1), -1);
    }
}



/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readible books
 */
void display_koff(int k_idx)
{
    int y;

    object_type forge;
    object_type *q_ptr;
    int         sval;
    int         use_realm;
    rect_t      display = {0};

    char o_name[MAX_NLEN];

    display.x = 0;
    display.y = 2;
    display.cy = Term->hgt - 2;
    display.cx = Term->wid;


    /* Erase the window */
    for (y = 0; y < Term->hgt; y++)
    {
        /* Erase the line */
        Term_erase(0, y, 255);
    }

    /* No info */
    if (!k_idx) return;

    /* Get local object */
    q_ptr = &forge;

    /* Prepare the object */
    object_prep(q_ptr, k_idx);

    /* Describe */
    object_desc(o_name, q_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY | OD_STORE));

    /* Mention the object name */
    Term_putstr(0, 0, -1, TERM_WHITE, o_name);

    /* Access the item's sval */
    sval = q_ptr->sval;
    use_realm = tval2realm(q_ptr->tval);

    /* Warriors are illiterate */
    if (plr->realm1 || plr->realm2)
    {
        if ((use_realm != plr->realm1) && (use_realm != plr->realm2)) return;
    }
    else
    {
        if ((plr->pclass != CLASS_SORCERER) && (plr->pclass != CLASS_RED_MAGE)) return;
        if (!is_magic(use_realm)) return;
        if ((plr->pclass == CLASS_RED_MAGE) && (use_realm != REALM_ARCANE) && (sval > 1)) return;
    }

    /* Display spells in readible books */
    {
        int     spell = -1;
        int     num = 0;
        byte    spells[64];

        /* Extract spells */
        for (spell = 0; spell < 32; spell++)
        {
            /* Check for this spell */
            if (fake_spell_flags[sval] & (1L << spell))
            {
                /* Collect this spell */
                spells[num++] = spell;
            }
        }

        /* Print spells */
        print_spells(0, spells, num, display, use_realm);
    }
}

/* Choose one of items that have warning flag */
static bool _object_has_warning(object_type *o_ptr) { return obj_has_flag(o_ptr, OF_WARNING); }
object_type *choose_warning_item(void)
{
    int slot = equip_random_slot(_object_has_warning);
    if (slot)
        return equip_obj(slot);
    return NULL;
}

/* Examine the grid (xx,yy) and warn the player if there are any danger */
bool process_warning(point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    bool warn = FALSE;

    if (!easy_disarm && floor_has_known_trap(cell))
        warn = TRUE;
    if (floor_has_secret_trap(cell))
        warn = TRUE;
    if (warn && !one_in_(13))
    {
        obj_ptr obj = choose_warning_item();
        char name[MAX_NLEN];

        if (obj) object_desc(name, obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));
        else strcpy(name, "body"); /* Warning ability without item */
        msg_format("Your %s pulsates!", name);
        if (obj) obj_learn_flag(obj, OF_WARNING);
        disturb(0, 0);
        return get_check("Do you really want to go ahead? ");
    }
    return TRUE;
}
