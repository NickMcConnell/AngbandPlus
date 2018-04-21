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

#include "int-map.h"

#include <assert.h>

/*
 * Excise a dungeon object from any stacks
 */
void excise_object_idx(int o_idx)
{
    object_type *j_ptr;

    s16b this_o_idx, next_o_idx = 0;

    s16b prev_o_idx = 0;


    /* Object */
    j_ptr = &o_list[o_idx];

    /* Monster */
    if (j_ptr->held_m_idx)
    {
        monster_type *m_ptr;

        /* Monster */
        m_ptr = &m_list[j_ptr->held_m_idx];

        /* Scan all objects in the grid */
        for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Acquire object */
            o_ptr = &o_list[this_o_idx];

            /* Acquire next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Done */
            if (this_o_idx == o_idx)
            {
                /* No previous */
                if (prev_o_idx == 0)
                {
                    /* Remove from list */
                    m_ptr->hold_o_idx = next_o_idx;
                }

                /* Real previous */
                else
                {
                    object_type *k_ptr;

                    /* Previous object */
                    k_ptr = &o_list[prev_o_idx];

                    /* Remove from list */
                    k_ptr->next_o_idx = next_o_idx;
                }

                /* Forget next pointer */
                o_ptr->next_o_idx = 0;

                /* Done */
                break;
            }

            /* Save prev_o_idx */
            prev_o_idx = this_o_idx;
        }
    }

    /* Dungeon */
    else
    {
        cave_type *c_ptr;

        int y = j_ptr->iy;
        int x = j_ptr->ix;

        /* Grid */
        c_ptr = &cave[y][x];

        /* Scan all objects in the grid */
        for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Acquire object */
            o_ptr = &o_list[this_o_idx];

            /* Acquire next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Done */
            if (this_o_idx == o_idx)
            {
                /* No previous */
                if (prev_o_idx == 0)
                {
                    /* Remove from list */
                    c_ptr->o_idx = next_o_idx;
                }

                /* Real previous */
                else
                {
                    object_type *k_ptr;

                    /* Previous object */
                    k_ptr = &o_list[prev_o_idx];

                    /* Remove from list */
                    k_ptr->next_o_idx = next_o_idx;
                }

                /* Forget next pointer */
                o_ptr->next_o_idx = 0;

                /* Done */
                break;
            }

            /* Save prev_o_idx */
            prev_o_idx = this_o_idx;
        }
    }
    p_ptr->window |= PW_OBJECT_LIST;
}


/*
 * Delete a dungeon object
 *
 * Handle "stacks" of objects correctly.
 */
void delete_object_idx(int o_idx)
{
    object_type *j_ptr;

    /* Excise */
    excise_object_idx(o_idx);

    /* Object */
    j_ptr = &o_list[o_idx];

    /* Dungeon floor */
    if (!(j_ptr->held_m_idx))
    {
        int y, x;

        /* Location */
        y = j_ptr->iy;
        x = j_ptr->ix;

        /* Visual update */
        lite_spot(y, x);
    }

    /* Wipe the object */
    object_wipe(j_ptr);

    /* Count objects */
    o_cnt--;

    p_ptr->window |= PW_OBJECT_LIST;
}


/*
 * Deletes all objects at given location
 */
void delete_object(int y, int x)
{
    cave_type *c_ptr;

    s16b this_o_idx, next_o_idx = 0;


    /* Refuse "illegal" locations */
    if (!in_bounds(y, x)) return;


    /* Grid */
    c_ptr = &cave[y][x];

    /* Scan all objects in the grid */
    for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[this_o_idx];

        /* Acquire next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Wipe the object */
        object_wipe(o_ptr);

        /* Count objects */
        o_cnt--;
    }

    /* Objects are gone */
    c_ptr->o_idx = 0;

    /* Visual update */
    lite_spot(y, x);

    p_ptr->window |= PW_OBJECT_LIST;
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_objects_aux(int i1, int i2)
{
    int i;

    cave_type *c_ptr;

    object_type *o_ptr;


    /* Do nothing */
    if (i1 == i2) return;


    /* Repair objects */
    for (i = 1; i < o_max; i++)
    {
        /* Acquire object */
        o_ptr = &o_list[i];

        /* Skip "dead" objects */
        if (!o_ptr->k_idx) continue;

        /* Repair "next" pointers */
        if (o_ptr->next_o_idx == i1)
        {
            /* Repair */
            o_ptr->next_o_idx = i2;
        }
    }


    /* Acquire object */
    o_ptr = &o_list[i1];


    /* Monster */
    if (o_ptr->held_m_idx)
    {
        monster_type *m_ptr;

        /* Acquire monster */
        m_ptr = &m_list[o_ptr->held_m_idx];

        /* Repair monster */
        if (m_ptr->hold_o_idx == i1)
        {
            /* Repair */
            m_ptr->hold_o_idx = i2;
        }
    }

    /* Dungeon */
    else
    {
        int y, x;

        /* Acquire location */
        y = o_ptr->iy;
        x = o_ptr->ix;

        /* Acquire grid */
        c_ptr = &cave[y][x];

        /* Repair grid */
        if (c_ptr->o_idx == i1)
        {
            /* Repair */
            c_ptr->o_idx = i2;
        }
    }


    /* Structure copy */
    o_list[i2] = o_list[i1];

    /* Wipe the hole */
    object_wipe(o_ptr);
}


/*
 * Compact and Reorder the object list
 *
 * This function can be very dangerous, use with caution!
 *
 * When actually "compacting" objects, we base the saving throw on a
 * combination of object level, distance from player, and current
 * "desperation".
 *
 * After "compacting" (if needed), we "reorder" the objects into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_objects(int size)
{
    int i, y, x, num, cnt;
    int cur_lev, cur_dis, chance;
    object_type *o_ptr;


    /* Compact */
    if (size)
    {
        /* Message */
        msg_print("Compacting objects...");


        /* Redraw map */
        p_ptr->redraw |= (PR_MAP);

        /* Window stuff */
        p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
    }


    /* Compact at least 'size' objects */
    for (num = 0, cnt = 1; num < size; cnt++)
    {
        /* Get more vicious each iteration */
        cur_lev = 5 * cnt;

        /* Get closer each iteration */
        cur_dis = 5 * (20 - cnt);

        /* Examine the objects */
        for (i = 1; i < o_max; i++)
        {
            o_ptr = &o_list[i];

            /* Skip dead objects */
            if (!o_ptr->k_idx) continue;

            /* Hack -- High level objects start out "immune" */
            if (k_info[o_ptr->k_idx].level > cur_lev) continue;

            /* Monster */
            if (o_ptr->held_m_idx)
            {
                monster_type *m_ptr;

                /* Acquire monster */
                m_ptr = &m_list[o_ptr->held_m_idx];

                /* Get the location */
                y = m_ptr->fy;
                x = m_ptr->fx;

                /* Monsters protect their objects */
                if (randint0(100) < 90) continue;
            }

            /* Dungeon */
            else
            {
                /* Get the location */
                y = o_ptr->iy;
                x = o_ptr->ix;
            }

            /* Nearby objects start out "immune" */
            if ((cur_dis > 0) && (distance(py, px, y, x) < cur_dis)) continue;

            /* Saving throw */
            chance = 90;

            /* Hack -- only compact artifacts in emergencies */
            if ((object_is_fixed_artifact(o_ptr) || o_ptr->art_name) &&
                (cnt < 1000)) chance = 100;

            /* Apply the saving throw */
            if (randint0(100) < chance) continue;

            /* Delete the object */
            delete_object_idx(i);

            /* Count it */
            num++;
        }
    }


    /* Excise dead objects (backwards!) */
    for (i = o_max - 1; i >= 1; i--)
    {
        o_ptr = &o_list[i];

        /* Skip real objects */
        if (o_ptr->k_idx) continue;

        /* Move last object into open hole */
        compact_objects_aux(o_max - 1, i);

        /* Compress "o_max" */
        o_max--;
    }
}


/*
 * Delete all the items when player leaves the level
 *
 * Note -- we do NOT visually reflect these (irrelevant) changes
 *
 * Hack -- we clear the "c_ptr->o_idx" field for every grid,
 * and the "m_ptr->next_o_idx" field for every monster, since
 * we know we are clearing every object. Technically, we only
 * clear those fields for grids/monsters containing objects,
 * and we clear it once for every such object.
 */
void wipe_o_list(void)
{
    int i;

    /* Delete the existing objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Mega-Hack -- preserve artifacts */
        if (!character_dungeon || preserve_mode)
        {
            /* Hack -- Preserve unknown artifacts */
            if (object_is_fixed_artifact(o_ptr) && !object_is_known(o_ptr))
            {
                /* Mega-Hack -- Preserve the artifact */
                a_info[o_ptr->name1].generated = FALSE;
            }
            if (random_artifacts && o_ptr->name3 && !object_is_known(o_ptr))
            {
                /* Mega-Hack -- Preserve the artifact */
                a_info[o_ptr->name3].generated = FALSE;
            }
        }

        /* Monster */
        if (o_ptr->held_m_idx)
        {
            monster_type *m_ptr;

            /* Monster */
            m_ptr = &m_list[o_ptr->held_m_idx];

            /* Hack -- see above */
            m_ptr->hold_o_idx = 0;
        }

        /* Dungeon */
        else
        {
            cave_type *c_ptr;

            /* Access location */
            int y = o_ptr->iy;
            int x = o_ptr->ix;

            /* Access grid */
            c_ptr = &cave[y][x];

            /* Hack -- see above */
            c_ptr->o_idx = 0;
        }

        /* Wipe the object */
        object_wipe(o_ptr);
    }

    /* Reset "o_max" */
    o_max = 1;

    /* Reset "o_cnt" */
    o_cnt = 0;
}


/*
 * Acquires and returns the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
s16b o_pop(void)
{
    int i;


    /* Initial allocation */
    if (o_max < max_o_idx)
    {
        /* Get next space */
        i = o_max;

        /* Expand object array */
        o_max++;

        /* Count objects */
        o_cnt++;

        /* Use this object */
        return (i);
    }


    /* Recycle dead objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[i];

        /* Skip live objects */
        if (o_ptr->k_idx) continue;

        /* Count objects */
        o_cnt++;

        /* Use this object */
        return (i);
    }


    /* Warn the player (except during dungeon creation) */
    if (character_dungeon) msg_print("Too many objects!");


    /* Oops */
    return (0);
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

    if (!ironman_shops && tval >= TV_LIFE_BOOK)
    {
        if (tval == TV_ARCANE_BOOK)
            max = 10;
        else
            max = limits[sval];
    }

    return max;
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

    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;

    /* Boost level */
    if ((level > 0) && !(d_info[dungeon_type].flags1 & DF1_BEGINNER))
    {
        /* Occasional "boost" */
        if (one_in_(GREAT_OBJ))
        {
            int boost = level;
            if (boost < 20)
                boost = 20;
            level += rand_range(boost/4, boost/2);

            /* What a bizarre calculation
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
        if (k_ptr->tval == TV_FOOD && k_ptr->sval == SV_FOOD_AMBROSIA && dungeon_type != DUNGEON_OLYMPUS) continue;
        if (easy_id && k_ptr->tval == TV_SCROLL && k_ptr->sval == SV_SCROLL_STAR_IDENTIFY) continue;
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
    if (object_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
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
    if (object_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_find(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }

    if (o_ptr->name1)
    {
        assert(a_info[o_ptr->name1].generated);
        a_info[o_ptr->name1].found = TRUE;
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

    if (object_is_device(o_ptr))
        device_stats_on_use(o_ptr, num);
}

void stats_on_p_destroy(object_type *o_ptr, int num)
{
    if (object_is_aware(o_ptr) && !(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }
    if (object_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
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
    if (object_is_device(o_ptr))
        device_stats_on_destroy(o_ptr);
}

void stats_on_m_destroy(object_type *o_ptr, int num)
{
    k_info[o_ptr->k_idx].counts.destroyed += num;
    if (o_ptr->name2)
        e_info[o_ptr->name2].counts.destroyed += num;
    if (object_is_device(o_ptr))
        device_stats_on_destroy(o_ptr);
}

void stats_on_pickup(object_type *o_ptr)
{
    if (object_is_aware(o_ptr) && !(o_ptr->marked & OM_COUNTED))
    {
        k_info[o_ptr->k_idx].counts.found += o_ptr->number;
        o_ptr->marked |= OM_COUNTED;
    }
    if (object_is_known(o_ptr) && object_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_find(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }
    if (object_is_known(o_ptr) && o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }

    if (object_is_known(o_ptr) && o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
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

    if (object_is_known(o_ptr) && o_ptr->name2 && !(o_ptr->marked & OM_EGO_COUNTED))
    {
        e_info[o_ptr->name2].counts.found += o_ptr->number;
        o_ptr->marked |= OM_EGO_COUNTED;
    }

    if (object_is_known(o_ptr) && o_ptr->art_name && !(o_ptr->marked & OM_ART_COUNTED))
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

    if (object_is_device(o_ptr) && !(o_ptr->marked & OM_EFFECT_COUNTED))
    {
        device_stats_on_find(o_ptr);
        o_ptr->marked |= OM_EFFECT_COUNTED;
    }

    if (o_ptr->name1)
    {
        assert(a_info[o_ptr->name1].generated);
        a_info[o_ptr->name1].found = TRUE;
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
    if (object_is_device(o_ptr))
        o_ptr->ident |= IDENT_TRIED;
    else
        k_info[o_ptr->k_idx].tried = TRUE;
}

bool object_is_tried(object_type *o_ptr)
{
    if (object_is_device(o_ptr))
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
            int level = r_info[o_ptr->pval].level;
            if (level < 20) return level*50L;
            else if (level < 30) return 1000+(level-20)*150L;
            else if (level < 40) return 2500+(level-30)*350L;
            else if (level < 50) return 6000+(level-40)*800L;
            else return 14000+(level-50)*2000L;
        }

        case TV_CAPTURE:
            if (!o_ptr->pval) return 1000L;
            else return ((r_info[o_ptr->pval].level) * 50L + 1000);
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
    if (object_is_melee_weapon(o_ptr)) return weapon_cost(o_ptr, COST_REAL);
    if (object_is_ammo(o_ptr)) return ammo_cost(o_ptr, COST_REAL);
    if (o_ptr->tval == TV_BOW) return bow_cost(o_ptr, COST_REAL);
    if (object_is_armour(o_ptr) || object_is_shield(o_ptr)) return armor_cost(o_ptr, COST_REAL);
    if (object_is_jewelry(o_ptr) || (o_ptr->tval == TV_LITE && object_is_artifact(o_ptr))) return jewelry_cost(o_ptr, COST_REAL);
    if (o_ptr->tval == TV_LITE) return lite_cost(o_ptr, COST_REAL);
    if (object_is_device(o_ptr)) return device_value(o_ptr, COST_REAL);


    /* Hack -- "worthless" items */
    if (!k_info[o_ptr->k_idx].cost) return (0L);

    /* Base cost */
    value = k_info[o_ptr->k_idx].cost;

    switch (o_ptr->tval)
    {
        /* Figurines, relative to monster level */
        case TV_FIGURINE:
        {
            int level = r_info[o_ptr->pval].level;
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
            else value = ((r_info[o_ptr->pval].level) * 50L + 1000);
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
    if (object_is_known(o_ptr))
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
            if (object_is_artifact(o_ptr))
                value += 1000;
            if (object_is_cursed(o_ptr))
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
          && object_is_ego(o_ptr))
        {
            value += 500 / o_ptr->number;
        }
        if ( (o_ptr->ident & IDENT_SENSE)
          && (o_ptr->feeling == FEEL_SPECIAL || o_ptr->feeling == FEEL_TERRIBLE)
          && object_is_artifact(o_ptr))
        {
            value += 1000;
        }
        if ((o_ptr->ident & IDENT_SENSE) && object_is_cursed(o_ptr))
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
    if (!object_is_artifact(o_ptr) || (o_ptr->rune == RUNE_SACRIFICE)) return TRUE;

    /* If object is unidentified, makes fake inscription */
    if (!object_is_known(o_ptr))
    {
        byte feel = FEEL_SPECIAL;

        /* Hack -- Handle icky artifacts */
        if (object_is_cursed(o_ptr) || object_is_broken(o_ptr)) feel = FEEL_TERRIBLE;

        /* Hack -- inscribe the artifact */
        o_ptr->feeling = feel;

        /* We have "felt" it (again) */
        o_ptr->ident |= (IDENT_SENSE);

        /* Combine the pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Done */
        return FALSE;
    }

    /* Identified artifact -- Nothing to do */
    return FALSE;
}


/*
 * Distribute charges of rods or wands.
 *
 * o_ptr = source item
 * q_ptr = target item, must be of the same type as o_ptr
 * amt   = number of items that are transfered
 */
void distribute_charges(object_type *o_ptr, object_type *q_ptr, int amt)
{
    /*
     * Hack -- If rods or wands are dropped, the total maximum timeout or
     * charges need to be allocated between the two stacks. If all the items
     * are being dropped, it makes for a neater message to leave the original
     * stack's pval alone. -LM-
     */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD))
    {
        q_ptr->pval = o_ptr->pval * amt / o_ptr->number;
        if (amt < o_ptr->number) o_ptr->pval -= q_ptr->pval;

        /* Hack -- Rods also need to have their timeouts distributed. The
         * dropped stack will accept all time remaining to charge up to its
         * maximum.
         */
        if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
        {
            if (q_ptr->pval > o_ptr->timeout)
                q_ptr->timeout = o_ptr->timeout;
            else
                q_ptr->timeout = q_ptr->pval;

            if (amt < o_ptr->number) o_ptr->timeout -= q_ptr->timeout;
        }
    }
}

void reduce_charges(object_type *o_ptr, int amt)
{
    /*
     * Hack -- If rods or wand are destroyed, the total maximum timeout or
     * charges of the stack needs to be reduced, unless all the items are
     * being destroyed. -LM-
     */
    if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)) &&
        (amt < o_ptr->number))
    {
        o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
    }
}


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow staffs (if they are known to have equal charges
 * and both are either known or confirmed empty) and wands (if both are
 * either known or confirmed empty) and rods (in all cases) to combine.
 * Staffs will unstack (if necessary) when they are used, but wands and
 * rods will only unstack if one is dropped. -LM-
 *
 * If permitted, we allow weapons/armor to stack, if fully "known".
 *
 * Missiles will combine if both stacks have the same "known" status.
 * This is done to make unidentified stacks of missiles useful.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests, and activatable items, never stack (for various reasons).
 */

/*
 * A "stack" of items is limited to less than or equal to 99 items (hard-coded).
 */
#define MAX_STACK_SIZE 99


/*
 *  Determine if an item can partly absorb a second item.
 *  Return maximum number of stack.
 */
int object_similar_part(object_type *o_ptr, object_type *j_ptr)
{
    int i;

    /* Default maximum number of stack */
    int max_num = MAX_STACK_SIZE;

    /* Require identical object types */
    if (o_ptr->k_idx != j_ptr->k_idx) return 0;


    /* Analyze the items */
    switch (o_ptr->tval)
    {
        /* Chests and Statues*/
        case TV_CHEST:
        case TV_CARD:
        case TV_CAPTURE:
        case TV_RUNE:
        {
            /* Never okay */
            return 0;
        }

        case TV_STATUE:
        {
            if ((o_ptr->sval != SV_PHOTO) || (j_ptr->sval != SV_PHOTO)) return 0;
            if (o_ptr->pval != j_ptr->pval) return 0;
            break;
        }

        /* Figurines and Corpses*/
        case TV_FIGURINE:
        case TV_CORPSE:
        {
            /* Same monster */
            if (o_ptr->pval != j_ptr->pval) return 0;

            /* Assume okay */
            break;
        }

        /* Food and Potions and Scrolls */
        case TV_FOOD:
        case TV_POTION:
        case TV_SCROLL:
        {
            if (o_ptr->art_name || j_ptr->art_name)
                return 0;
            /* Assume okay */
            break;
        }

        /* Staffs */
        case TV_STAFF:
        case TV_WAND:
        case TV_ROD:
            return 0;

        /* Weapons and Armor */
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:

        /* Rings, Amulets, Lites */
        case TV_RING:
        case TV_AMULET:
        case TV_LITE:
        case TV_WHISTLE:
        {
            /* Require full knowledge of both items */
            if (!object_is_known(o_ptr) || !object_is_known(j_ptr)) return 0;

            /* Fall through */
        }

        /* Missiles */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            /* Require identical knowledge of both items */
            if (object_is_known(o_ptr) != object_is_known(j_ptr)) return 0;
            if (o_ptr->feeling != j_ptr->feeling) return 0;

            /* Require identical "bonuses" */
            if (o_ptr->to_h != j_ptr->to_h) return 0;
            if (o_ptr->to_d != j_ptr->to_d) return 0;
            if (o_ptr->to_a != j_ptr->to_a) return 0;

            /* Require identical "pval" code */
            if (o_ptr->pval != j_ptr->pval) return 0;

            /* Artifacts never stack */
            if (object_is_artifact(o_ptr) || object_is_artifact(j_ptr)) return 0;

            /* Require identical "ego-item" names */
            if (o_ptr->name2 != j_ptr->name2) return 0;

            /* Require identical added essence  */
            if (o_ptr->xtra3 != j_ptr->xtra3) return 0;
            if (o_ptr->xtra4 != j_ptr->xtra4) return 0;

            /* Hack -- Never stack "powerful" items */
            if (o_ptr->xtra1 || j_ptr->xtra1) return 0;

            /* Hack -- Never stack recharging items */
            if (o_ptr->timeout || j_ptr->timeout) return 0;

            /* Require identical "values" */
            if (o_ptr->ac != j_ptr->ac) return 0;
            if (o_ptr->dd != j_ptr->dd) return 0;
            if (o_ptr->ds != j_ptr->ds) return 0;

            /* Probably okay */
            break;
        }

        /* Various */
        default:
        {
            /* Require knowledge */
            if (!object_is_known(o_ptr) || !object_is_known(j_ptr)) return 0;

            /* Probably okay */
            break;
        }
    }


    /* Hack -- Identical art_flags! */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        if (o_ptr->flags[i] != j_ptr->flags[i]) return 0;

    /* Hack -- Require identical "cursed" status */
    if (o_ptr->curse_flags != j_ptr->curse_flags) return 0;

    /* Require identical activations */
    if ( o_ptr->activation.type != j_ptr->activation.type
      || o_ptr->activation.cost != j_ptr->activation.cost
      || o_ptr->activation.power != j_ptr->activation.power
      || o_ptr->activation.difficulty != j_ptr->activation.difficulty
      || o_ptr->activation.extra != j_ptr->activation.extra )
    {
        return 0;
    }

    /* Hack -- Require identical "broken" status */
    if ((o_ptr->ident & (IDENT_BROKEN)) != (j_ptr->ident & (IDENT_BROKEN))) return 0;


    /* Hack -- require semi-matching "inscriptions" */
    if (o_ptr->inscription && j_ptr->inscription &&
        (o_ptr->inscription != j_ptr->inscription))
        return 0;

    /* Hack -- normally require matching "inscriptions" */
    if (!stack_force_notes && (o_ptr->inscription != j_ptr->inscription)) return 0;

    /* Hack -- normally require matching "discounts" */
    if (!stack_force_costs && (o_ptr->discount != j_ptr->discount)) return 0;


    /* They match, so they must be similar */
    return max_num;
}

/*
 *  Determine if an item can absorb a second item.
 */
bool object_similar(object_type *o_ptr, object_type *j_ptr)
{
    int total = o_ptr->number + j_ptr->number;
    int max_num;

    /* Are these objects similar? */
    max_num = object_similar_part(o_ptr, j_ptr);

    /* Return if not similar */
    if (!max_num) return FALSE;

    /* Maximal "stacking" limit */
    if (total > max_num) return (0);


    /* They match, so they must be similar */
    return (TRUE);
}



/*
 * Allow one item to "absorb" another, assuming they are similar
 */
void object_absorb(object_type *o_ptr, object_type *j_ptr)
{
    int max_num = object_similar_part(o_ptr, j_ptr);
    int total = o_ptr->number + j_ptr->number;
    int diff = (total > max_num) ? total - max_num : 0;

    /* Combine quantity, lose excess items */
    o_ptr->number = (total > max_num) ? max_num : total;

    /* Hack -- blend "known" status */
    if (object_is_known(j_ptr)) obj_identify(o_ptr);

    /* Hack -- clear "storebought" if only one has it */
    if (((o_ptr->ident & IDENT_STORE) || (j_ptr->ident & IDENT_STORE)) &&
        (!((o_ptr->ident & IDENT_STORE) && (j_ptr->ident & IDENT_STORE))))
    {
        if (j_ptr->ident & IDENT_STORE) j_ptr->ident &= 0xEF;
        if (o_ptr->ident & IDENT_STORE) o_ptr->ident &= 0xEF;
    }

    /* Hack -- blend "inscriptions" */
    if (j_ptr->inscription) o_ptr->inscription = j_ptr->inscription;

    /* Hack -- blend "feelings" */
    if (j_ptr->feeling) o_ptr->feeling = j_ptr->feeling;

    /* Hack -- could average discounts XXX XXX XXX */
    /* Hack -- save largest discount XXX XXX XXX */
    if (o_ptr->discount < j_ptr->discount) o_ptr->discount = j_ptr->discount;

    /* Hack -- if rods are stacking, add the pvals (maximum timeouts) and current timeouts together. -LM- */
    if (o_ptr->tval == TV_ROD)
    {
        o_ptr->pval += j_ptr->pval * (j_ptr->number - diff) / j_ptr->number;
        o_ptr->timeout += j_ptr->timeout * (j_ptr->number - diff) / j_ptr->number;
    }

    /* Hack -- if wands are stacking, combine the charges. -LM- */
    if (o_ptr->tval == TV_WAND)
    {
        o_ptr->pval += j_ptr->pval * (j_ptr->number - diff) / j_ptr->number;
    }
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
static void object_mention(object_type *o_ptr)
{
    char o_name[MAX_NLEN];

    /* Describe */
    object_desc(o_name, o_ptr, (OD_NAME_ONLY | OD_STORE));

    /* Artifact */
    if (object_is_fixed_artifact(o_ptr))
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
    else if (object_is_ego(o_ptr))
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
    do
    {
        if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_DRAGON_FANG && one_in_(3))
            one_ele_slay(o_ptr);
        else if (one_in_(4))
            one_dragon_ele_resistance(o_ptr);
        else
            one_high_resistance(o_ptr);
    }
    while (one_in_(2));
}

/*
 * Mega-Hack -- Attempt to create one of the "Special Objects"
 *
 * We are only called from "make_object()", and we assume that
 * "apply_magic()" is called immediately after we return.
 *
 * Note -- see "make_artifact()" and "apply_magic()"
 */
static bool make_artifact_special(object_type *o_ptr)
{
    int i;
    int k_idx = 0;


    /* No artifacts in the town */
    if (!dun_level) return (FALSE);
    if (no_artifacts) return FALSE;

    /* Themed object */
    if (get_obj_num_hook) return (FALSE);

    /* Check the artifact list (just the "specials") */
    for (i = 0; i < max_a_idx; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        if (!a_ptr->name) continue;
        if (a_ptr->generated) continue;
        if (a_ptr->gen_flags & OFG_QUESTITEM) continue;
        if (!(a_ptr->gen_flags & OFG_INSTA_ART)) continue;

        /* XXX XXX Enforce minimum "depth" (loosely) */
        if (a_ptr->level > dun_level)
        {
            /* Acquire the "out-of-depth factor" */
            int d = (a_ptr->level - dun_level) * 2;

            /* Roll for out-of-depth creation */
            if (!one_in_(d)) continue;
        }

        /* Artifact "rarity roll" */
        if (!one_in_(a_ptr->rarity)) continue;

        /* Find the base object */
        k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

        /* XXX XXX Enforce minimum "object" level (loosely) */
        if (k_info[k_idx].level > object_level)
        {
            /* Acquire the "out-of-depth factor" */
            int d = (k_info[k_idx].level - object_level) * 5;

            /* Roll for out-of-depth creation */
            if (!one_in_(d)) continue;
        }

        if (random_artifacts)
        {
            create_replacement_art(i, o_ptr);
        }
        else
        {
            create_named_art_aux(i, o_ptr);
        }
        /* Success */
        return (TRUE);
    }

    /* Failure */
    return (FALSE);
}


/*
 * Attempt to change an object into an artifact
 *
 * This routine should only be called by "apply_magic()"
 *
 * Note -- see "make_artifact_special()" and "apply_magic()"
 */
static bool make_artifact(object_type *o_ptr)
{
    int i;


    /* No artifacts in the town */
    if (!dun_level) return FALSE;
    if (no_artifacts) return FALSE;

    /* Paranoia -- no "plural" artifacts */
    if (o_ptr->number != 1) return (FALSE);

    /* Check the artifact list (skip the "specials") */
    for (i = 0; i < max_a_idx; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        if (!a_ptr->name) continue;
        if (a_ptr->generated) continue;
        if (a_ptr->gen_flags & OFG_QUESTITEM) continue;
        if (a_ptr->gen_flags & OFG_INSTA_ART) continue;
        if (a_ptr->tval != o_ptr->tval) continue;
        if (a_ptr->sval != o_ptr->sval) continue;

        /* XXX XXX Enforce minimum "depth" (loosely) */
        if (a_ptr->level > dun_level)
        {
            /* Acquire the "out-of-depth factor" */
            int d = (a_ptr->level - dun_level) * (a_ptr->level - dun_level);

            /* Roll for out-of-depth creation */
            if (!one_in_(d)) continue;
        }

        if (!one_in_(a_ptr->rarity)) continue;

        if (random_artifacts)
        {
            create_replacement_art(i, o_ptr);
        }
        else
        {
            create_named_art_aux(i, o_ptr);
        }
        /* Success */
        return (TRUE);
    }

    /* Failure */
    return (FALSE);
}

bool add_esp_strong(object_type *o_ptr)
{
    bool nonliv = FALSE;

    switch (randint1(3))
    {
    case 1: add_flag(o_ptr->flags, OF_ESP_EVIL); break;
    case 2: add_flag(o_ptr->flags, OF_TELEPATHY); break;
    case 3:    add_flag(o_ptr->flags, OF_ESP_NONLIVING); nonliv = TRUE; break;
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
static bool item_monster_okay(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* No uniques */
    if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);
    if (r_ptr->flags7 & RF7_KAGE) return (FALSE);
    if (r_ptr->flagsr & RFR_RES_ALL) return (FALSE);
    if (r_ptr->flags7 & RF7_NAZGUL) return (FALSE);
    if (r_ptr->flags1 & RF1_FORCE_DEPTH) return (FALSE);
    if (r_ptr->flags7 & RF7_UNIQUE2) return (FALSE);

    /* Okay */
    return (TRUE);
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
#if 0
            /* Cursed */
            if (power < 0)
            {
                /* Broken */
                o_ptr->ident |= (IDENT_BROKEN);

                /* Cursed */
                o_ptr->curse_flags |= (TRC_CURSED);
            }
#endif
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
            int i = 1;
            int check;

            monster_race *r_ptr;

            /* Pick a random non-unique monster race */
            while (1)
            {
                i = randint1(max_r_idx - 1);

                if (!item_monster_okay(i)) continue;
                if (i == MON_TSUCHINOKO) continue;

                r_ptr = &r_info[i];

                check = (dun_level < r_ptr->level) ? (r_ptr->level - dun_level) : 0;

                /* Ignore dead monsters */
                if (!r_ptr->rarity) continue;

                /* Ignore uncommon monsters */
                if (r_ptr->rarity > 100) continue;

                /* Prefer less out-of-depth monsters */
                if (randint0(check)) continue;

                break;
            }

            o_ptr->pval = i;

            /* Some figurines are cursed */
            if (one_in_(6)) o_ptr->curse_flags |= OFC_CURSED;

            if (cheat_peek)
            {
                msg_format("Figurine of %s, depth +%d%s",

                              r_name + r_ptr->name, check - 1,
                              !object_is_cursed(o_ptr) ? "" : " {cursed}");
            }

            break;
        }

        case TV_CORPSE:
        {
            int i = 1;
            int check = 0;
            int count = 0;

            u32b match = 0;

            monster_race *r_ptr = 0;

            if (o_ptr->sval == SV_SKELETON)
            {
                match = RF9_DROP_SKELETON;
            }
            else if (o_ptr->sval == SV_CORPSE)
            {
                match = RF9_DROP_CORPSE;
            }

            /* Hack -- Remove the monster restriction */
            get_mon_num_prep(item_monster_okay, NULL);

            /* Pick a random non-unique monster race */
            while (1)
            {
                /* This loop is spinning forever at deep levels ... */
                count++;
                if (count > 100) break;

                i = get_mon_num(dun_level);

                r_ptr = &r_info[i];

                check = (dun_level < r_ptr->level) ? (r_ptr->level - dun_level) : 0;

                /* Ignore dead monsters */
                if (!r_ptr->rarity) continue;

                /* Ignore corpseless monsters */
                if (!(r_ptr->flags9 & match)) continue;

                /* Prefer less out-of-depth monsters */
                if (randint0(check)) continue;

                break;
            }

            o_ptr->pval = i;

            if (cheat_peek)
            {
                msg_format("Corpse of %s, depth +%d",

                              r_name + r_ptr->name, check - 1);
            }

            obj_identify(o_ptr);
            break;
        }

        case TV_STATUE:
        {
            int i = 1;

            monster_race *r_ptr;

            /* Pick a random monster race */
            while (1)
            {
                i = randint1(max_r_idx - 1);

                r_ptr = &r_info[i];

                /* Ignore dead monsters */
                if (!r_ptr->rarity) continue;

                break;
            }

            o_ptr->pval = i;

            if (cheat_peek)
            {
                msg_format("Statue of %s", r_name + r_ptr->name);

            }
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
            if (o_ptr->sval == SV_CHEST_KANDUME) o_ptr->pval = 6;

            o_ptr->xtra3 = dun_level + 5;

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
bool apply_magic(object_type *o_ptr, int lev, u32b mode)
{
    int i, rolls, f1, f2, power;

    if (p_ptr->personality == PERS_MUNCHKIN) lev += randint0(p_ptr->lev/2+10);

    /* Maximum "level" for various things */
    if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

    /* Base chance of being "good" */
    f1 = lev + 10;

    /* Maximal chance of being "good" */
    if (f1 > d_info[dungeon_type].obj_good) f1 = d_info[dungeon_type].obj_good;

    /* Base chance of being "great" */
    f2 = f1 * 2 / 3;

    /* Maximal chance of being "great" */
    if ((p_ptr->personality != PERS_MUNCHKIN) && (f2 > d_info[dungeon_type].obj_great))
        f2 = d_info[dungeon_type].obj_great;

    /* Temp Hack: It's a bit too hard to find good rings early on. Note we hack after
       calculating f2! */
    if (o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET)
    {
        f1 += 30;
        if (f1 > d_info[dungeon_type].obj_good) f1 = d_info[dungeon_type].obj_good;
    }

    if (p_ptr->good_luck)
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
        if (no_egos)
        {
        }
        else if ((mode & AM_GREAT) || magik(f2))
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
        if (no_egos)
        {
        }
        else if (magik(f2))
        {
            power = -2;
        }

        /* "Cursed" items become tedious in the late game ... */
        if ( power == -1
          && o_ptr->tval != TV_RING
          && o_ptr->tval != TV_AMULET
          && !object_is_device(o_ptr)
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

    /* Hack -- Get no rolls if not allowed */
    if ((mode & AM_NO_FIXED_ART) || o_ptr->name1 || o_ptr->name3) rolls = 0;
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
        if (make_artifact(o_ptr)) break;
        if (p_ptr->good_luck && one_in_(77))
        {
            if (make_artifact(o_ptr)) break;
        }
    }


    /* Hack -- analyze replacement artifacts */
    if (o_ptr->name3)
    {
        artifact_type *a_ptr = &a_info[o_ptr->name3];

        /* Hack -- Mark the artifact as "created" */
        a_ptr->generated = TRUE;

        /* Hack -- Memorize location of artifact in saved floors */
        if (character_dungeon)
            a_ptr->floor_id = p_ptr->floor_id;

        if (cheat_peek) object_mention(o_ptr);
        return TRUE;
    }

    if (object_is_fixed_artifact(o_ptr))
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];

        /* Hack -- Mark the artifact as "created" */
        a_ptr->generated = TRUE;

        /* Hack -- Memorize location of artifact in saved floors */
        if (character_dungeon)
            a_ptr->floor_id = p_ptr->floor_id;

        /* Extract the other fields */
        o_ptr->pval = a_ptr->pval;
        o_ptr->ac = a_ptr->ac;
        o_ptr->dd = a_ptr->dd;
        o_ptr->ds = a_ptr->ds;
        o_ptr->to_a = a_ptr->to_a;
        o_ptr->to_h = a_ptr->to_h;
        o_ptr->to_d = a_ptr->to_d;
        o_ptr->weight = a_ptr->weight;

        /* Hack -- extract the "broken" flag */
        if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

        /* Hack -- extract the "cursed" flag */
        if (a_ptr->gen_flags & OFG_CURSED) o_ptr->curse_flags |= (OFC_CURSED);
        if (a_ptr->gen_flags & OFG_HEAVY_CURSE) o_ptr->curse_flags |= (OFC_HEAVY_CURSE);
        if (a_ptr->gen_flags & OFG_PERMA_CURSE) o_ptr->curse_flags |= (OFC_PERMA_CURSE);
        if (a_ptr->gen_flags & (OFG_RANDOM_CURSE0)) o_ptr->curse_flags |= get_curse(0, o_ptr);
        if (a_ptr->gen_flags & (OFG_RANDOM_CURSE1)) o_ptr->curse_flags |= get_curse(1, o_ptr);
        if (a_ptr->gen_flags & (OFG_RANDOM_CURSE2)) o_ptr->curse_flags |= get_curse(2, o_ptr);


        /* Cheat -- peek at the item */
        if (cheat_peek) object_mention(o_ptr);

        /* Done */
        return TRUE;
    }

    if (o_ptr->art_name)
    {
        return TRUE;
    }


    /* Apply magic */
    switch (o_ptr->tval)
    {
        case TV_DIGGING:
        case TV_HAFTED:
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
                if (cheat_peek) object_mention(o_ptr);
                dragon_resist(o_ptr);
                if (!one_in_(3)) power = 0;
            }

            if (o_ptr->sval == SV_RUNESWORD)
            {
                o_ptr->curse_flags |= (OFC_PERMA_CURSE);
            }
            else
            {
                if (!(o_ptr->sval == SV_DOKUBARI))
                {
                    if (power) obj_create_weapon(o_ptr, lev, power, mode);
                }
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

            if (object_is_dragon_armor(o_ptr) && !(mode & AM_CRAFTING))
            {
                if (cheat_peek) object_mention(o_ptr);
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
        case TV_LITE:
            obj_create_lite(o_ptr, lev, power, mode);
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
        (p_ptr->personality == PERS_SEXY || demigod_is_(DEMIGOD_APHRODITE)))
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
    if (p_ptr->pclass != CLASS_ARCHER)
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
    int class_idx = p_ptr->pclass;

    if (class_idx == CLASS_MONSTER)
        return get_race()->pseudo_class_idx;

    switch (class_idx)
    {
    case CLASS_ARCHAEOLOGIST:
    case CLASS_BLOOD_MAGE:
    case CLASS_HIGH_MAGE:
    case CLASS_MAGE:
    case CLASS_MAGIC_EATER:
    case CLASS_MIRROR_MASTER:
    case CLASS_NECROMANCER:
    case CLASS_ROGUE:
    case CLASS_SORCERER:
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
        return equip_can_wield_kind(k_ptr->tval, k_ptr->sval)
            && p_ptr->pclass != CLASS_NINJA
            && p_ptr->pclass != CLASS_MAULER
            && p_ptr->pclass != CLASS_DUELIST;

    case TV_HARD_ARMOR:
    case TV_SOFT_ARMOR:
    case TV_DRAG_ARMOR:
        if ( p_ptr->pclass == CLASS_MONK
          || p_ptr->pclass == CLASS_FORCETRAINER
          || p_ptr->pclass == CLASS_MYSTIC
          || p_ptr->pclass == CLASS_DUELIST
          || p_ptr->pclass == CLASS_SCOUT
          || p_ptr->pclass == CLASS_NINJA )
        {
            return k_ptr->weight <= 200 && equip_can_wield_kind(k_ptr->tval, k_ptr->sval);
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
        if (p_ptr->prace == RACE_MON_RING) return TRUE;
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
    case TV_MUSIC_BOOK:
    case TV_HISSATSU_BOOK:
    case TV_HEX_BOOK:
    case TV_RAGE_BOOK:
    case TV_BURGLARY_BOOK:
        return check_book_realm(k_ptr->tval, k_ptr->sval)
            && k_ptr->sval >= SV_BOOK_MIN_GOOD;

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
        case TV_MUSIC_BOOK:
        case TV_HISSATSU_BOOK:
        case TV_HEX_BOOK:
        case TV_RAGE_BOOK:
        case TV_BURGLARY_BOOK:
        {
            if (k_ptr->sval == SV_BOOK_MIN_GOOD) return TRUE; /* Third Spellbooks: I want ?Acquirement to grant these! */
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD + 1) return TRUE;   /* Fourth Spellbooks */
            return (FALSE);
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
        case TV_MUSIC_BOOK:
        case TV_HISSATSU_BOOK:
        case TV_HEX_BOOK:
        case TV_RAGE_BOOK:
        case TV_BURGLARY_BOOK:
        {
            if (k_ptr->sval == SV_BOOK_MIN_GOOD) return TRUE; /* Third Spellbooks */
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD + 1) return TRUE;   /* Fourth Spellbooks */
            return (FALSE);
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
bool kind_is_device(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_WAND: case TV_ROD: case TV_STAFF:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_potion(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_POTION:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_scroll(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_SCROLL:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_wand(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_WAND:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_rod(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_ROD:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_staff(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_STAFF:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_potion_scroll(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_POTION:
    case TV_SCROLL:
        return TRUE;
    }
    return FALSE;
}
bool kind_is_wand_rod_staff(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_WAND:
    case TV_ROD:
    case TV_STAFF:
        return TRUE;
    }
    return FALSE;
}
bool kind_is_jewelry(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_RING: case TV_AMULET:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_ring(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_RING:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_amulet(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_AMULET:
        return TRUE;
    }
    return FALSE;
}
bool kind_is_book(int k_idx) {
    if (TV_LIFE_BOOK <= k_info[k_idx].tval && k_info[k_idx].tval <= TV_BURGLARY_BOOK)
        return TRUE;
    return FALSE;
}
bool kind_is_body_armor(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_SOFT_ARMOR: case TV_HARD_ARMOR: case TV_DRAG_ARMOR:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_shield(int k_idx) {
    if (k_info[k_idx].tval == TV_SHIELD)
        return TRUE;
    return FALSE;
}
static bool _kind_is_gloves(int k_idx) {
    if (k_info[k_idx].tval == TV_GLOVES)
        return TRUE;
    return FALSE;
}
static bool _kind_is_cloak(int k_idx) {
    if (k_info[k_idx].tval == TV_CLOAK)
        return TRUE;
    return FALSE;
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
static bool _kind_is_helm_cloak(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_HELM: case TV_CROWN: case TV_CLOAK:
        return TRUE;
    }
    return FALSE;
}
bool kind_is_helm(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_HELM: case TV_CROWN:
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_boots(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_BOOTS:
        return TRUE;
    }
    return FALSE;
}
bool kind_is_armor(int k_idx) {
    if (TV_ARMOR_BEGIN <= k_info[k_idx].tval && k_info[k_idx].tval <= TV_ARMOR_END)
        return TRUE;
    return FALSE;
}
bool kind_is_weapon(int k_idx) {
    if (TV_DIGGING <= k_info[k_idx].tval && k_info[k_idx].tval <= TV_WEAPON_END)
        return TRUE;
    return FALSE;
}
static bool _kind_is_whip(int k_idx) {
    if (k_info[k_idx].tval == TV_HAFTED && k_info[k_idx].sval == SV_WHIP)
        return TRUE;
    return FALSE;
}
static bool _kind_is_lance(int k_idx) {
    if (k_info[k_idx].tval == TV_POLEARM && (k_info[k_idx].sval == SV_LANCE || k_info[k_idx].sval == SV_HEAVY_LANCE))
        return TRUE;
    return FALSE;
}
static bool _kind_is_bow(int k_idx) {
    if (k_info[k_idx].tval == TV_BOW)
        return TRUE;
    return FALSE;
}
static bool _kind_is_lite(int k_idx) {
    if (k_info[k_idx].tval == TV_LITE)
        return TRUE;
    return FALSE;
}
static bool _kind_is_bow2(int k_idx) {
    if (k_info[k_idx].tval == TV_BOW && k_info[k_idx].sval != SV_HARP) /* Assume tailored Archer reward, and a harp is just insulting! */
        return TRUE;
    return FALSE;
}
static bool _kind_is_ammo(int k_idx) {
    if (TV_MISSILE_BEGIN <= k_info[k_idx].tval && k_info[k_idx].tval <= TV_MISSILE_END)
        return TRUE;
    return FALSE;
}
bool kind_is_bow_ammo(int k_idx) {
    if (k_info[k_idx].tval == TV_BOW)
        return TRUE;
    if (TV_MISSILE_BEGIN <= k_info[k_idx].tval && k_info[k_idx].tval <= TV_MISSILE_END)
        return TRUE;
    return FALSE;
}
bool kind_is_misc(int k_idx) {
    switch (k_info[k_idx].tval)
    {
    case TV_SKELETON: case TV_BOTTLE: case TV_JUNK: case TV_WHISTLE:
    case TV_SPIKE: case TV_CHEST: case TV_FIGURINE: case TV_STATUE:
    case TV_CAPTURE: case TV_FOOD: case TV_FLASK:
        return TRUE;
    }
    return FALSE;
}
typedef struct {
    _kind_p hook;
    int     base;
    int     good;
    int     great;
} _kind_alloc_entry;
/*
static _kind_alloc_entry _kind_alloc_table[] = {
    { kind_is_weapon,          210,    0,    0 },
    { kind_is_body_armor,      200,    0,    0 },
    { kind_is_other_armor,     210,    0,    0 },
    { kind_is_wand_rod_staff,   90,  -40,  -60 },
    { _kind_is_potion_scroll,  100,  -50,  -90 },
    { _kind_is_bow,             45,    0,   25 },
    { _kind_is_ammo,            30,    0,  -25 },
    { kind_is_book,             25,   10,   15 },
    { kind_is_jewelry,          40,    0,    0 },
    { kind_is_misc,             50,  -50,  -50 },
    { NULL, 0}
};*/
static _kind_alloc_entry _kind_alloc_table[] = {
    /* Equipment by Slot */
    { kind_is_weapon,          215,    0,    0 },
    { _kind_is_shield,          30,    0,    0 },
    { _kind_is_bow,             65,    0,    0 },
    { kind_is_jewelry,          40,    0,    0 },
    { _kind_is_lite,            10,    0,    0 },
    { kind_is_body_armor,      200,    0,    0 },
    { _kind_is_cloak,           30,    0,    0 },
    { kind_is_helm,             30,    0,    0 },
    { _kind_is_gloves,          30,    0,    0 },
    { _kind_is_boots,           30,    0,    0 },
    /*                         680              */

    { kind_is_wand_rod_staff,   95,  -40,  -60 },
    { _kind_is_potion_scroll,  105,  -50,  -90 },
    { _kind_is_ammo,            45,    0,  -30 },
    { kind_is_book,             25,   10,   15 }, /* R_DROP_MAGE is covering this ... */
    { kind_is_misc,             50,  -50,  -50 },
    /*                         320              */
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

    if (p_ptr->prace == RACE_MON_RING && entry->hook == kind_is_jewelry)
        w = w * 2;

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
    if ( _kind_is_(k_idx, TV_POTION, SV_POTION_BERSERK_STRENGTH)
      || _kind_is_(k_idx, TV_POTION, SV_POTION_HEROISM) )
    {
        return TRUE;
    }

    switch (k_info[k_idx].tval)
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
    if ( _kind_is_(k_idx, TV_BOW,  SV_SHORT_BOW)
      || _kind_is_(k_idx, TV_BOW,  SV_LONG_BOW)
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
        case SV_POTION_INC_STR:
        case SV_POTION_INC_INT:
        case SV_POTION_INC_WIS:
        case SV_POTION_INC_DEX:
        case SV_POTION_INC_CON:
        case SV_POTION_INC_CHR:
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
    return FALSE;
}
static bool _kind_theme_hobbit(int k_idx) {
    if ( _kind_is_(k_idx, TV_FOOD, SV_ANY)
      || _kind_is_(k_idx, TV_BOW, SV_SLING)
      || _kind_is_(k_idx, TV_SHOT, SV_AMMO_LIGHT) )
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
    case TV_LITE:
    case TV_FLASK:
    case TV_FOOD:
        return TRUE;
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
        }
    }
    /* Circumvent normal object kind frequencies for good, great and tailored drops.
     * The goal here is, for example, to allow mages to find adequate spellbooks without
     * drowning other characters with useless junk.
     * Note: Themed drops (above) circumvent tailored objects ...*/
    else if (_drop_tailored)
    {
        _kind_hook2 = kind_is_tailored;
        switch (p_ptr->pclass)
        {
        case CLASS_ARCHAEOLOGIST:
            if (one_in_(5))
                _kind_hook1 = _kind_is_whip; /* Diggers swamp out whips ... */
            else if (one_in_(5))
                _kind_hook1 = kind_is_weapon;
            else if (one_in_(5))
                _kind_hook1 = kind_is_wand_rod_staff;
            break;
        case CLASS_ARCHER:
        case CLASS_SNIPER:
            if (one_in_(5))
                _kind_hook1 = _kind_is_bow2;
            break;
        case CLASS_DEVICEMASTER:
            if (one_in_(5))
            {
                switch (p_ptr->psubclass)
                {
                case DEVICEMASTER_POTIONS:
                    _kind_hook1 = _kind_is_potion;
                    break;
                case DEVICEMASTER_SCROLLS:
                    _kind_hook1 = _kind_is_scroll;
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
                _kind_hook1 = kind_is_device;
            break;
        case CLASS_RAGE_MAGE:
            if (one_in_(3))
                _kind_hook1 = kind_is_book;
            break;
        case CLASS_CAVALRY:
        case CLASS_BEASTMASTER:
            if (one_in_(7))
                _kind_hook1 = _kind_is_lance;
            break;

        case CLASS_MONSTER:
            switch (p_ptr->prace)
            {
            case RACE_MON_BEHOLDER:
                if (one_in_(5))
                    _kind_hook1 = _kind_is_ring;
                else if (one_in_(7))
                    _kind_hook1 = kind_is_helm;
                break;
            case RACE_MON_CENTIPEDE:
                if (one_in_(7))
                    _kind_hook1 = _kind_is_boots;
                break;
            case RACE_MON_DRAGON:
                if (one_in_(5))
                    _kind_hook1 = _kind_is_ring;
                else if (one_in_(7))
                    _kind_hook1 = _kind_is_helm_cloak;
                break;
            case RACE_MON_HYDRA:
                if (one_in_(5))
                    _kind_hook1 = _kind_is_amulet;
                else if (one_in_(7))
                    _kind_hook1 = kind_is_helm;
                break;
            }
            break;
        }
        if (!_kind_hook1 && mut_present(MUT_DRACONIAN_METAMORPHOSIS))
        {
            if (one_in_(5))
                _kind_hook1 = _kind_is_ring;
            else if (one_in_(7))
                _kind_hook1 = _kind_is_helm_cloak;
        }
        if (!_kind_hook1)
        {
            if (is_magic(p_ptr->realm1) && one_in_(10))
                _kind_hook1 = kind_is_book;
            else if (_is_device_class() && one_in_(7))
                _kind_hook1 = kind_is_wand_rod_staff;
        }
    }

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
 * This routine uses "object_level" for the "generation level".
 *
 * We assume that the given object has been "wiped".
 */
bool make_object(object_type *j_ptr, u32b mode)
{
    int prob, base;
    byte obj_level;

    /* Chance of "special object" */
    prob = ((mode & AM_GOOD) ? 10 : 1000);

    /* Base level for the object */
    base = ((mode & AM_GOOD) ? (object_level + 10) : object_level);

    /* Generate a special object, or a normal object */
    if (!one_in_(prob) || !make_artifact_special(j_ptr))
    {
        int k_idx;
        int max_attempts = 1;
        int attempt = 1;

        _drop_tailored = FALSE;
        if (mode & AM_TAILORED)
        {
            _drop_tailored = TRUE;
            max_attempts = 1000; /* Tailored drops can fail for certain _choose_obj_kind()s */
        }

        for (;;)
        {
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

            if (k_idx)
                break;

            attempt++;
            if (attempt > max_attempts)
                break;
        }

        /* Handle failure */
        if (!k_idx)
        {
            obj_drop_theme = 0;
            return FALSE;
        }

        /* Prepare the object */
        object_prep(j_ptr, k_idx);
    }

    /* Apply magic (allow artifacts) */
    if (!apply_magic(j_ptr, object_level, mode))
    {
        obj_drop_theme = 0;
        return FALSE;
    }

    /* Note: It is important to do this *after* apply_magic rather than in, say,
       object_prep() since artifacts should never spawn multiple copies. Ego ammo
       should, but other egos (e.g. lights) should not. */
    mass_produce(j_ptr);

    obj_level = k_info[j_ptr->k_idx].level;
    if (object_is_fixed_artifact(j_ptr)) obj_level = a_info[j_ptr->name1].level;
    if (j_ptr->name3) obj_level = a_info[j_ptr->name3].level;

    /* Notice "okay" out-of-depth objects */
    if (!object_is_cursed(j_ptr) && !object_is_broken(j_ptr) &&
        (obj_level > dun_level))
    {
        /* Cheat -- peek at items */
        if (cheat_peek) object_mention(j_ptr);
    }

    /* Success */
    obj_drop_theme = 0;
    return TRUE;
}


/*
 * Attempt to place an object (normal or good/great) at the given location.
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "object_level" for the "generation level".
 *
 * This routine requires a clean floor grid destination.
 */
void place_object(int y, int x, u32b mode)
{
    s16b o_idx;

    /* Acquire grid */
    cave_type *c_ptr = &cave[y][x];

    object_type forge;
    object_type *q_ptr;


    /* Paranoia -- check bounds */
    if (!in_bounds(y, x)) return;

    /* Require floor space */
    if (!cave_drop_bold(y, x)) return;

    /* Avoid stacking on other objects */
    if (c_ptr->o_idx) return;


    /* Get local object */
    q_ptr = &forge;

    /* Wipe the object */
    object_wipe(q_ptr);

    /* Make an object (if possible) */
    if (!make_object(q_ptr, mode)) return;


    /* Make an object */
    o_idx = o_pop();

    /* Success */
    if (o_idx)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[o_idx];

        /* Structure Copy */
        object_copy(o_ptr, q_ptr);

        /* Location */
        o_ptr->iy = y;
        o_ptr->ix = x;

        /* Build a stack */
        o_ptr->next_o_idx = c_ptr->o_idx;

        /* Place the object */
        c_ptr->o_idx = o_idx;

        /* Notice */
        note_spot(y, x);

        /* Redraw */
        lite_spot(y, x);
    }
    else
    {
        /* Hack -- Preserve artifacts */
        if (object_is_fixed_artifact(q_ptr))
        {
            a_info[q_ptr->name1].generated = FALSE;
        }
        if (random_artifacts && q_ptr->name3)
            a_info[q_ptr->name3].generated = FALSE;
    }
}


/*
 * Make a treasure object
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_gold(object_type *j_ptr, bool do_boost)
{
    int i;

    s32b base;


    /* Hack -- Pick a Treasure variety */
    i = ((randint1(object_level + 2) + 2) / 2) - 1;

    /* Apply "extra" magic */
    if (one_in_(GREAT_OBJ))
    {
        i += randint1(object_level + 1);
    }

    /* Hack -- Creeping Coins only generate "themselves" */
    if (coin_type) i = coin_type;

    /* Do not create "illegal" Treasure Types */
    if (i >= MAX_GOLD) i = MAX_GOLD - 1;

    /* Prepare a gold object */
    object_prep(j_ptr, OBJ_GOLD_LIST + i);

    /* Hack -- Base coin cost */
    base = k_info[OBJ_GOLD_LIST+i].cost;

    /* Determine how much the treasure is "worth" */
    j_ptr->pval = (base + (8 * randint1(base)) + randint1(8));

    j_ptr->pval = j_ptr->pval * (625 - virtue_current(VIRTUE_SACRIFICE)) / 625;

    if (do_boost)
        j_ptr->pval += j_ptr->pval * object_level / 7;

    /* Success */
    return (TRUE);
}


/*
 * Places a treasure (Gold or Gems) at given location
 *
 * The location must be a legal, clean, floor grid.
 */
void place_gold(int y, int x)
{
    s16b o_idx;

    /* Acquire grid */
    cave_type *c_ptr = &cave[y][x];


    object_type forge;
    object_type *q_ptr;


    /* Paranoia -- check bounds */
    if (!in_bounds(y, x)) return;

    /* Require floor space */
    if (!cave_drop_bold(y, x)) return;

    /* Avoid stacking on other objects */
    if (c_ptr->o_idx) return;


    /* Get local object */
    q_ptr = &forge;

    /* Wipe the object */
    object_wipe(q_ptr);

    /* Make some gold */
    if (!make_gold(q_ptr, FALSE)) return;


    /* Make an object */
    o_idx = o_pop();

    /* Success */
    if (o_idx)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[o_idx];

        /* Copy the object */
        object_copy(o_ptr, q_ptr);

        /* Save location */
        o_ptr->iy = y;
        o_ptr->ix = x;

        /* Build a stack */
        o_ptr->next_o_idx = c_ptr->o_idx;

        /* Place the object */
        c_ptr->o_idx = o_idx;

        /* Notice */
        note_spot(y, x);

        /* Redraw */
        lite_spot(y, x);
    }
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
 */
s16b drop_near(object_type *j_ptr, int chance, int y, int x)
{
    int i, k, d, s;

    int bs, bn;
    int by, bx;
    int dy, dx;
    int ty, tx = 0;

    s16b o_idx = 0;

    s16b this_o_idx, next_o_idx = 0;

    cave_type *c_ptr;

    char o_name[MAX_NLEN];

    bool flag = FALSE;
    bool done = FALSE;

    /* Extract plural */
    bool plural = (j_ptr->number != 1);

    /* Describe object */
    object_desc(o_name, j_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));


    /* Handle normal "breakage" */
    if (!object_is_artifact(j_ptr) && (randint0(100) < chance))
    {
        /* Message */
        msg_format("The %s disappear%s.",
               o_name, (plural ? "" : "s"));


        /* Debug */
        if (p_ptr->wizard) msg_print("(breakage)");

        stats_on_m_destroy(j_ptr, 1);

        /* Failure */
        return (0);
    }


    /* Score */
    bs = -1;

    /* Picker */
    bn = 0;

    /* Default */
    by = y;
    bx = x;

    /* Scan local grids */
    for (dy = -3; dy <= 3; dy++)
    {
        /* Scan local grids */
        for (dx = -3; dx <= 3; dx++)
        {
            bool comb = FALSE;

            /* Calculate actual distance */
            d = (dy * dy) + (dx * dx);

            /* Ignore distant grids */
            if (d > 10) continue;

            /* Location */
            ty = y + dy;
            tx = x + dx;

            /* Skip illegal grids */
            if (!in_bounds(ty, tx)) continue;

            /* Require line of projection */
            if (!projectable(y, x, ty, tx)) continue;

            /* Obtain grid */
            c_ptr = &cave[ty][tx];

            /* Require floor space */
            if (!cave_drop_bold(ty, tx)) continue;

            /* No objects */
            k = 0;

            /* Scan objects in that grid */
            for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
            {
                object_type *o_ptr;

                /* Acquire object */
                o_ptr = &o_list[this_o_idx];

                /* Acquire next object */
                next_o_idx = o_ptr->next_o_idx;

                /* Check for possible combination */
                if (object_similar(o_ptr, j_ptr)) comb = TRUE;

                /* Count objects */
                k++;
            }

            /* Add new object */
            if (!comb) k++;

            /* Paranoia */
            if (k > 99) continue;

            /* Calculate score */
            s = 1000 - (d + k * 5);

            /* Skip bad values */
            if (s < bs) continue;

            /* New best value */
            if (s > bs) bn = 0;

            /* Apply the randomizer to equivalent values */
            if ((++bn >= 2) && !one_in_(bn)) continue;

            /* Keep score */
            bs = s;

            /* Track it */
            by = ty;
            bx = tx;

            /* Okay */
            flag = TRUE;
        }
    }


    /* Handle lack of space */
    if (!flag && !object_is_artifact(j_ptr))
    {
        /* Message */
        msg_format("The %s disappear%s.",
               o_name, (plural ? "" : "s"));


        /* Debug */
        if (p_ptr->wizard) msg_print("(no floor space)");


        /* Failure */
        return (0);
    }


    /* Find a grid */
    for (i = 0; !flag && (i < 1000); i++)
    {
        /* Bounce around */
        ty = rand_spread(by, 1);
        tx = rand_spread(bx, 1);

        /* Verify location */
        if (!in_bounds(ty, tx)) continue;

        /* Bounce to that location */
        by = ty;
        bx = tx;

        /* Require floor space */
        if (!cave_drop_bold(by, bx)) continue;

        /* Okay */
        flag = TRUE;
    }


    if (!flag)
    {
        int candidates = 0, pick;

        for (ty = 1; ty < cur_hgt - 1; ty++)
        {
            for (tx = 1; tx < cur_wid - 1; tx++)
            {
                /* A valid space found */
                if (cave_drop_bold(ty, tx)) candidates++;
            }
        }

        /* No valid place! */
        if (!candidates)
        {
            /* Message */
            msg_format("The %s disappear%s.", o_name, (plural ? "" : "s"));

            /* Debug */
            if (p_ptr->wizard) msg_print("(no floor space)");

            /* Mega-Hack -- preserve artifacts */
            if (preserve_mode)
            {
                /* Hack -- Preserve unknown artifacts */
                if (object_is_fixed_artifact(j_ptr) && !object_is_known(j_ptr))
                {
                    /* Mega-Hack -- Preserve the artifact */
                    a_info[j_ptr->name1].generated = FALSE;
                }

                if (random_artifacts && j_ptr->name3 && !object_is_known(j_ptr))
                {
                    /* Mega-Hack -- Preserve the artifact */
                    a_info[j_ptr->name3].generated = FALSE;
                }
            }

            /* Failure */
            return 0;
        }

        /* Choose a random one */
        pick = randint1(candidates);

        for (ty = 1; ty < cur_hgt - 1; ty++)
        {
            for (tx = 1; tx < cur_wid - 1; tx++)
            {
                if (cave_drop_bold(ty, tx))
                {
                    pick--;

                    /* Is this a picked one? */
                    if (!pick) break;
                }
            }

            if (!pick) break;
        }

        by = ty;
        bx = tx;
    }


    /* Grid */
    c_ptr = &cave[by][bx];

    /* Scan objects in that grid for combination */
    for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[this_o_idx];

        /* Acquire next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Check for combination */
        if (object_similar(o_ptr, j_ptr))
        {
            /* Combine the items */
            object_absorb(o_ptr, j_ptr);

            /* Success */
            done = TRUE;

            /* Done */
            break;
        }
    }

    /* Get new object */
    if (!done) o_idx = o_pop();

    /* Failure */
    if (!done && !o_idx)
    {
        /* Message */
        msg_format("The %s disappear%s.",
               o_name, (plural ? "" : "s"));


        /* Debug */
        if (p_ptr->wizard) msg_print("(too many objects)");


        /* Hack -- Preserve artifacts */
        if (object_is_fixed_artifact(j_ptr))
        {
            a_info[j_ptr->name1].generated = FALSE;
        }

        if (random_artifacts && j_ptr->name3)
        {
            a_info[j_ptr->name3].generated = FALSE;
        }

        /* Failure */
        return (0);
    }

    /* Stack */
    if (!done)
    {
        /* Structure copy */
        object_copy(&o_list[o_idx], j_ptr);

        /* Access new object */
        j_ptr = &o_list[o_idx];

        /* Locate */
        j_ptr->iy = by;
        j_ptr->ix = bx;

        /* No monster */
        j_ptr->held_m_idx = 0;

        /* Build a stack */
        j_ptr->next_o_idx = c_ptr->o_idx;

        /* Place the object */
        c_ptr->o_idx = o_idx;

        /* Success */
        done = TRUE;
    }

    /* Note the spot */
    note_spot(by, bx);

    /* Draw the spot */
    lite_spot(by, bx);

    /* Sound */
    sound(SOUND_DROP);

    /* Mega-Hack -- no message if "dropped" by player */
    /* Message when an object falls under the player */
    if (chance && player_bold(by, bx))
    {
        msg_print("You feel something roll beneath your feet.");

    }

    /* XXX XXX XXX */
    p_ptr->window |= PW_OBJECT_LIST;

    /* Result */
    return (o_idx);
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
        if (!make_object(i_ptr, mode)) continue;

        num--;
        if (known)
        {
            obj_identify(i_ptr);
        }

        /* Drop the object */
        (void)drop_near(i_ptr, -1, y1, x1);
    }
}


#define MAX_NORMAL_TRAPS 18

/* See init_feat_variables() in init2.c */
static s16b normal_traps[MAX_NORMAL_TRAPS];

/*
 * Initialize arrays for normal traps
 */
void init_normal_traps(void)
{
    int cur_trap = 0;

    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_TRAPDOOR");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_PIT");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_SPIKED_PIT");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_POISON_PIT");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_TY_CURSE");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_TELEPORT");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_FIRE");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_ACID");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_SLOW");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_LOSE_STR");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_LOSE_DEX");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_LOSE_CON");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_BLIND");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_CONFUSE");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_POISON");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_SLEEP");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_TRAPS");
    normal_traps[cur_trap++] = f_tag_to_index_in_init("TRAP_ALARM");
}

/*
 * Get random trap
 *
 * XXX XXX XXX This routine should be redone to reflect trap "level".
 * That is, it does not make sense to have spiked pits at 50 feet.
 * Actually, it is not this routine, but the "trap instantiation"
 * code, which should also check for "trap doors" on quest levels.
 */
s16b choose_random_trap(void)
{
    s16b feat;

    /* Pick a trap */
    while (1)
    {
        /* Hack -- pick a trap */
        feat = normal_traps[randint0(MAX_NORMAL_TRAPS)];

        /* Accept non-trapdoors */
        if (!have_flag(f_info[feat].flags, FF_MORE)) break;

        /* Hack -- no trap doors on special levels */
        if (p_ptr->inside_arena || quest_number(dun_level)) continue;

        /* Hack -- no trap doors on the deepest level */
        if (dun_level >= d_info[dungeon_type].maxdepth) continue;

        break;
    }

    return feat;
}

/*
 * Disclose an invisible trap
 */
void disclose_grid(int y, int x)
{
    cave_type *c_ptr = &cave[y][x];

    if (cave_have_flag_grid(c_ptr, FF_SECRET))
    {
        /* No longer hidden */
        cave_alter_feat(y, x, FF_SECRET);
    }
    else if (c_ptr->mimic)
    {
        /* No longer hidden */
        c_ptr->mimic = 0;

        /* Notice */
        note_spot(y, x);

        /* Redraw */
        lite_spot(y, x);
    }
}


/*
 * Places a random trap at the given location.
 *
 * The location must be a legal, naked, floor grid.
 *
 * Note that all traps start out as "invisible" and "untyped", and then
 * when they are "discovered" (by detecting them or setting them off),
 * the trap is "instantiated" as a visible, "typed", trap.
 */
void place_trap(int y, int x)
{
    cave_type *c_ptr = &cave[y][x];

    /* Paranoia -- verify location */
    if (!in_bounds(y, x)) return;

    /* Require empty, clean, floor grid */
    if (!cave_clean_bold(y, x)) return;

    /* Place an invisible trap */
    c_ptr->mimic = c_ptr->feat;
    c_ptr->feat = choose_random_trap();
}


/*
 * Describe the charges on an item in the inventory.
 */
void inven_item_charges(int item)
{
    object_type *o_ptr = &inventory[item];
    int          charges;

    if (!object_is_device(o_ptr)) return;
    if (!object_is_known(o_ptr)) return;
    if (!o_ptr->activation.cost) return; /* Just checking ... */

    charges = device_sp(o_ptr) / o_ptr->activation.cost;

    if (charges == 1)
        msg_print("You have 1 charge remaining.");
    else
        msg_format("You have %d charges remaining.", charges);
}


/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
    object_type *o_ptr = &inventory[item];
    char        o_name[MAX_NLEN];

    object_desc(o_name, o_ptr, OD_COLOR_CODED);
    msg_format("You have %s.", o_name);
}


/*
 * Increase the "number" of an item in the inventory
 */
void inven_item_increase(int item, int num)
{
    object_type *o_ptr = &inventory[item];

    /* Apply */
    num += o_ptr->number;

    /* Bounds check */
    if (num > 255) num = 255;
    else if (num < 0) num = 0;

    /* Un-apply */
    num -= o_ptr->number;

    /* Change the number and weight */
    if (num)
    {
        /* Add the number */
        o_ptr->number += num;

        /* Add the weight */
        p_ptr->total_weight += (num * o_ptr->weight);

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Recalculate mana XXX */
        p_ptr->update |= (PU_MANA);

        /* Combine the pack */
        p_ptr->notice |= (PN_COMBINE);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
    }
}


/*
 * Erase an inventory slot if it has no more items
 */
void inven_item_optimize(int item)
{
    object_type *o_ptr = &inventory[item];

    /* Only optimize real items */
    if (!o_ptr->k_idx) return;

    /* Only optimize empty items */
    if (o_ptr->number) return;

    /* The item is in the pack */
    if (item <= INVEN_PACK)
    {
        int i;

        /* One less item */
        inven_cnt--;

        /* Slide everything down */
        for (i = item; i < INVEN_PACK; i++)
        {
            /* Structure copy */
            inventory[i] = inventory[i+1];
        }

        /* Erase the "final" slot */
        object_wipe(&inventory[i]);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN);
    }

    /* The item is being wielded */
    else
    {
        /* Erase the empty slot */
        object_wipe(&inventory[item]);

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Recalculate torch */
        p_ptr->update |= (PU_TORCH);

        /* Recalculate mana XXX */
        p_ptr->update |= (PU_MANA);

        /* Window stuff */
        p_ptr->window |= (PW_EQUIP);
    }

    /* Window stuff */
    p_ptr->window |= (PW_SPELL);
}


/*
 * Describe the charges on an item on the floor.
 */
void floor_item_charges(int item)
{
    object_type *o_ptr = &o_list[item];
    int          charges;

    if (!object_is_device(o_ptr)) return;
    if (!object_is_known(o_ptr)) return;
    if (!o_ptr->activation.cost) return; /* Just checking ... */

    charges = device_sp(o_ptr) / o_ptr->activation.cost;

    if (charges == 1)
        msg_print("There is 1 charge remaining.");
    else
        msg_format("There are %d charges remaining.", charges);
}

/*
 * Describe an item in the inventory.
 */
void floor_item_describe(int item)
{
    object_type *o_ptr = &o_list[item];
    char        o_name[MAX_NLEN];

    object_desc(o_name, o_ptr, OD_COLOR_CODED);
    msg_format("You see %s.", o_name);
}


/*
 * Increase the "number" of an item on the floor
 */
void floor_item_increase(int item, int num)
{
    object_type *o_ptr = &o_list[item];

    /* Apply */
    num += o_ptr->number;

    /* Bounds check */
    if (num > 255) num = 255;
    else if (num < 0) num = 0;

    /* Un-apply */
    num -= o_ptr->number;

    /* Change the number */
    o_ptr->number += num;
}


/*
 * Optimize an item on the floor (destroy "empty" items)
 */
void floor_item_optimize(int item)
{
    object_type *o_ptr = &o_list[item];

    /* Paranoia -- be sure it exists */
    if (!o_ptr->k_idx) return;

    /* Only optimize empty items */
    if (o_ptr->number) return;

    /* Delete the object */
    delete_object_idx(item);
}


/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(object_type *o_ptr)
{
    int j;

    /* Empty slot? */
    if (inven_cnt < INVEN_PACK) return (TRUE);

    /* Similar slot? */
    for (j = 0; j < INVEN_PACK; j++)
    {
        object_type *j_ptr = &inventory[j];

        /* Skip non-objects */
        if (!j_ptr->k_idx) continue;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, o_ptr)) return (TRUE);
    }

    /* Nope */
    return (FALSE);
}


bool object_sort_comp(object_type *o_ptr, s32b o_value, object_type *j_ptr)
{
    int o_type, j_type;

    /* Use empty slots */
    if (!j_ptr->k_idx) return TRUE;

    /* Hack -- readable books always come first */
    if ((o_ptr->tval == REALM1_BOOK) &&
        (j_ptr->tval != REALM1_BOOK)) return TRUE;
    if ((j_ptr->tval == REALM1_BOOK) &&
        (o_ptr->tval != REALM1_BOOK)) return FALSE;

    if ((o_ptr->tval == REALM2_BOOK) &&
        (j_ptr->tval != REALM2_BOOK)) return TRUE;
    if ((j_ptr->tval == REALM2_BOOK) &&
        (o_ptr->tval != REALM2_BOOK)) return FALSE;

    /* Objects sort by decreasing type */
    if (o_ptr->tval > j_ptr->tval) return TRUE;
    if (o_ptr->tval < j_ptr->tval) return FALSE;

    /* Non-aware (flavored) items always come last */
    /* Can happen in the home */
    if (!object_is_aware(o_ptr)) return FALSE;
    if (!object_is_aware(j_ptr)) return TRUE;

    /* Objects sort by increasing sval */
    if (o_ptr->sval < j_ptr->sval) return TRUE;
    if (o_ptr->sval > j_ptr->sval) return FALSE;

    /* Unidentified objects always come last */
    /* Objects in the home can be unknown */
    if (!object_is_known(o_ptr)) return FALSE;
    if (!object_is_known(j_ptr)) return TRUE;

    /* Fixed artifacts, random artifacts and ego items */
    if (object_is_fixed_artifact(o_ptr)) o_type = 3;
    else if (o_ptr->art_name) o_type = 2;
    else if (object_is_ego(o_ptr)) o_type = 1;
    else o_type = 0;

    if (!object_is_device(o_ptr))
    {
        if (object_is_fixed_artifact(j_ptr)) j_type = 3;
        else if (j_ptr->art_name) j_type = 2;
        else if (object_is_ego(j_ptr)) j_type = 1;
        else j_type = 0;

        if (o_type < j_type) return TRUE;
        if (o_type > j_type) return FALSE;
    }

    switch (o_ptr->tval)
    {
    case TV_FIGURINE:
    case TV_STATUE:
    case TV_CORPSE:
    case TV_CAPTURE:
        if (r_info[o_ptr->pval].level < r_info[j_ptr->pval].level) return TRUE;
        if ((r_info[o_ptr->pval].level == r_info[j_ptr->pval].level) && (o_ptr->pval < j_ptr->pval)) return TRUE;
        return FALSE;

    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
        /* Objects sort by increasing hit/damage bonuses */
        if (o_ptr->to_h + o_ptr->to_d < j_ptr->to_h + j_ptr->to_d) return TRUE;
        if (o_ptr->to_h + o_ptr->to_d > j_ptr->to_h + j_ptr->to_d) return FALSE;
        break;

    case TV_ROD:
    case TV_WAND:
    case TV_STAFF:
        if (o_ptr->activation.type < j_ptr->activation.type) return TRUE;
        if (o_ptr->activation.type > j_ptr->activation.type) return FALSE;
        if (device_level(o_ptr) < device_level(j_ptr)) return TRUE;
        if (device_level(o_ptr) > device_level(j_ptr)) return FALSE;
        break;
    }

    /* Objects sort by decreasing value */
    return o_value > obj_value(j_ptr);
}


/*
 * Add an item to the players inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", else,
 * the item will be placed into the "proper" location in the inventory.
 *
 * This function can be used to "over-fill" the player's pack, but only
 * once, and such an action must trigger the "overflow" code immediately.
 * Note that when the pack is being "over-filled", the new item must be
 * placed into the "overflow" slot, and the "overflow" must take place
 * before the pack is reordered, but (optionally) after the pack is
 * combined. This may be tricky. See "dungeon.c" for info.
 *
 * Note that this code must remove any location/stack information
 * from the object once it is placed into the inventory.
 */
s16b inven_carry(object_type *o_ptr)
{
    int i, j, k;
    int n = -1;

    object_type *j_ptr;

    /* Check for combining */
    for (j = 0; j < INVEN_PACK; j++)
    {
        j_ptr = &inventory[j];

        /* Skip non-objects */
        if (!j_ptr->k_idx) continue;

        /* Hack -- track last item */
        n = j;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, o_ptr))
        {
            stats_on_pickup(o_ptr);

            /* Combine the items */
            object_absorb(j_ptr, o_ptr);

            /* Increase the weight */
            p_ptr->total_weight += (o_ptr->number * o_ptr->weight);

            /* Recalculate bonuses */
            p_ptr->update |= (PU_BONUS);

            /* Window stuff */
            p_ptr->window |= (PW_INVEN);

            /* Success */
            return (j);
        }
    }


    /* Paranoia */
    if (inven_cnt > INVEN_PACK) return (-1);

    /* Find an empty slot */
    for (j = 0; j <= INVEN_PACK; j++)
    {
        j_ptr = &inventory[j];

        /* Use it if found */
        if (!j_ptr->k_idx) break;
    }

    /* Use that slot */
    i = j;


    /* Reorder the pack */
    if (i < INVEN_PACK)
    {
        /* Get the "value" of the item */
        s32b o_value = obj_value(o_ptr);

        /* Scan every occupied slot */
        for (j = 0; j < INVEN_PACK; j++)
        {
            if (object_sort_comp(o_ptr, o_value, &inventory[j])) break;
        }

        /* Use that slot */
        i = j;

        /* Slide objects */
        for (k = n; k >= i; k--)
        {
            /* Hack -- Slide the item */
            object_copy(&inventory[k+1], &inventory[k]);
        }

        /* Wipe the empty slot */
        object_wipe(&inventory[i]);
    }


    /* Copy the item */
    stats_on_pickup(o_ptr);
    object_copy(&inventory[i], o_ptr);

    /* Access new object */
    j_ptr = &inventory[i];

    /* Forget stack */
    j_ptr->next_o_idx = 0;

    /* Forget monster */
    j_ptr->held_m_idx = 0;

    /* Forget location */
    j_ptr->iy = j_ptr->ix = 0;

    /* Player touches it, and no longer marked */
    j_ptr->marked &= (OM_WORN | OM_COUNTED | OM_EFFECT_COUNTED | OM_EGO_COUNTED | OM_ART_COUNTED);  /* Ah, but remember the "worn" status ... */
    j_ptr->marked |= OM_TOUCHED;

    /* Increase the weight */
    p_ptr->total_weight += (j_ptr->number * j_ptr->weight);

    /* Count the items */
    inven_cnt++;

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Combine and Reorder pack */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER);

    /* Window stuff */
    p_ptr->window |= (PW_INVEN);

    /* Return the slot */
    return (i);
}


/*
 * Take off (some of) a non-cursed equipment item
 *
 * Note that only one item at a time can be wielded per slot.
 *
 * Note that taking off an item when "full" may cause that item
 * to fall to the ground.
 *
 * Return the inventory slot into which the item is placed.
 */
s16b inven_takeoff(int item, int amt)
{
    int slot;

    object_type forge;
    object_type *q_ptr;

    object_type *o_ptr;

    cptr act = "";

    char o_name[MAX_NLEN];


    /* Get the item to take off */
    o_ptr = &inventory[item];

    /* Paranoia */
    if (amt <= 0) return (-1);

    /* Verify */
    if (amt > o_ptr->number) amt = o_ptr->number;

    /* Get local object */
    q_ptr = &forge;

    /* Obtain a local object */
    object_copy(q_ptr, o_ptr);

    /* Modify quantity */
    q_ptr->number = amt;

    /* Describe the object */
    object_desc(o_name, q_ptr, 0);

    if (equip_is_valid_slot(item))
        act = "You were wearing";

    /* Modify, Optimize */
    inven_item_increase(item, -amt);
    inven_item_optimize(item);

    /* Carry the object */
    slot = inven_carry(q_ptr);

    /* Message */
    msg_format("%s %s (%c).", act, o_name, index_to_label(slot));


    /* Return slot */
    return (slot);
}


/*
 * Drop (some of) a non-cursed inventory/equipment item
 *
 * The object will be dropped "near" the current location
 */
void inven_drop(int item, int amt)
{
    object_type  forge;
    object_type *q_ptr;
    object_type *o_ptr;
    int          old_number;
    char         o_name[MAX_NLEN];


    /* Access original object */
    o_ptr = &inventory[item];

    /* Error check */
    if (amt <= 0) return;

    /* Not too many */
    if (amt > o_ptr->number) amt = o_ptr->number;


    /* Take off equipment */
    if (equip_is_valid_slot(item))
    {
        /* Take off first */
        item = inven_takeoff(item, amt);

        /* Access original object */
        o_ptr = &inventory[item];
    }


    /* Get local object */
    q_ptr = &forge;

    /* Obtain local object */
    object_copy(q_ptr, o_ptr);

    /* Distribute charges of wands or rods */
    distribute_charges(o_ptr, q_ptr, amt);

    /* Modify quantity */
    old_number = q_ptr->number;
    q_ptr->number = amt;
    q_ptr->marked &= ~OM_WORN;

    /* Describe local object */
    object_desc(o_name, q_ptr, OD_COLOR_CODED);

    /* Message */
    msg_format("You drop %s (%c).", o_name, index_to_label(item));

    /* Drop it near the player */
    (void)drop_near(q_ptr, 0, py, px);

    /* Modify, Describe, Optimize */
    inven_item_increase(item, -amt);
    if (amt < old_number)
        inven_item_describe(item);
    inven_item_optimize(item);

    /* Runes confer benefits even when in inventory */
    p_ptr->update |= PU_BONUS;
}


/*
 * Combine items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void combine_pack(void)
{
    int             i, j, k;
    object_type     *o_ptr;
    object_type     *j_ptr;
    bool            flag = FALSE, combined;

    do
    {
        combined = FALSE;

        /* Combine the pack (backwards) */
        for (i = INVEN_PACK; i > 0; i--)
        {
            /* Get the item */
            o_ptr = &inventory[i];

            /* Skip empty items */
            if (!o_ptr->k_idx) continue;

            /* Scan the items above that item */
            for (j = 0; j < i; j++)
            {
                int max_num;

                /* Get the item */
                j_ptr = &inventory[j];

                /* Skip empty items */
                if (!j_ptr->k_idx) continue;

                /*
                 * Get maximum number of the stack if these
                 * are similar, get zero otherwise.
                 */
                max_num = object_similar_part(j_ptr, o_ptr);

                /* Can we (partialy) drop "o_ptr" onto "j_ptr"? */
                if (max_num && j_ptr->number < max_num)
                {
                    stats_on_combine(j_ptr, o_ptr);
                    if (o_ptr->number + j_ptr->number <= max_num)
                    {
                        /* Take note */
                        flag = TRUE;

                        /* Add together the item counts */
                        object_absorb(j_ptr, o_ptr);

                        /* One object is gone */
                        inven_cnt--;

                        /* Slide everything down */
                        for (k = i; k < INVEN_PACK; k++)
                        {
                            /* Structure copy */
                            inventory[k] = inventory[k+1];
                        }

                        /* Erase the "final" slot */
                        object_wipe(&inventory[k]);
                    }
                    else
                    {
                        int old_num = o_ptr->number;
                        int remain = j_ptr->number + o_ptr->number - max_num;
#if 0
                        o_ptr->number -= remain;
#endif
                        /* Add together the item counts */
                        object_absorb(j_ptr, o_ptr);

                        o_ptr->number = remain;

                        /* Hack -- if rods are stacking, add the pvals (maximum timeouts) and current timeouts together. -LM- */
                        if (o_ptr->tval == TV_ROD)
                        {
                            o_ptr->pval =  o_ptr->pval * remain / old_num;
                            o_ptr->timeout = o_ptr->timeout * remain / old_num;
                        }

                        /* Hack -- if wands are stacking, combine the charges. -LM- */
                        if (o_ptr->tval == TV_WAND)
                        {
                            o_ptr->pval = o_ptr->pval * remain / old_num;
                        }
                    }

                    /* Window stuff */
                    p_ptr->window |= (PW_INVEN);

                    /* Take note */
                    combined = TRUE;

                    /* Done */
                    break;
                }
            }
        }
    }
    while (combined);

    /* Message */
    if (flag) msg_print("You combine some items in your pack.");
}


/*
 * Reorder items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void reorder_pack(void)
{
    int             i, j, k;
    s32b            o_value;
    object_type     forge;
    object_type     *q_ptr;
    object_type     *o_ptr;
    bool            flag = FALSE;


    /* Re-order the pack (forwards) */
    for (i = 0; i < INVEN_PACK; i++)
    {
        /* Mega-Hack -- allow "proper" over-flow */
        if ((i == INVEN_PACK) && (inven_cnt == INVEN_PACK)) break;

        /* Get the item */
        o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        /* Get the "value" of the item */
        o_value = obj_value(o_ptr);

        /* Scan every occupied slot */
        for (j = 0; j < INVEN_PACK; j++)
        {
            if (object_sort_comp(o_ptr, o_value, &inventory[j])) break;
        }

        /* Never move down */
        if (j >= i) continue;

        /* Take note */
        flag = TRUE;

        /* Get local object */
        q_ptr = &forge;

        /* Save a copy of the moving item */
        object_copy(q_ptr, &inventory[i]);

        /* Slide the objects */
        for (k = i; k > j; k--)
        {
            /* Slide the item */
            object_copy(&inventory[k], &inventory[k-1]);
        }

        /* Insert the moving item */
        object_copy(&inventory[j], q_ptr);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN);
    }

    /* Message */
    if (flag) msg_print("You reorder some items in your pack.");

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
    if (p_ptr->realm1 || p_ptr->realm2)
    {
        if ((use_realm != p_ptr->realm1) && (use_realm != p_ptr->realm2)) return;
    }
    else
    {
        if ((p_ptr->pclass != CLASS_SORCERER) && (p_ptr->pclass != CLASS_RED_MAGE)) return;
        if (!is_magic(use_realm)) return;
        if ((p_ptr->pclass == CLASS_RED_MAGE) && (use_realm != REALM_ARCANE) && (sval > 1)) return;
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
static bool _object_has_warning(object_type *o_ptr) {
    u32b flgs[OF_ARRAY_SIZE];
    obj_flags(o_ptr, flgs);
    return have_flag(flgs, OF_WARNING);
}
object_type *choose_warning_item(void)
{
    int slot = equip_random_slot(_object_has_warning);
    if (slot)
        return equip_obj(slot);
    return NULL;
}

/* Calculate spell damages */
static void spell_damcalc(monster_type *m_ptr, int typ, int dam, int limit, int *max)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    int          rlev = r_ptr->level;
    bool         ignore_wraith_form = FALSE;

    if (limit) dam = (dam > limit) ? limit : dam;

    /* Vulnerability, resistance and immunity */
    switch (typ)
    {
    case GF_ELEC:
        dam = res_calc_dam(RES_ELEC, dam);
        if (dam == 0)
            ignore_wraith_form = TRUE;
        break;

    case GF_POIS:
        dam = res_calc_dam(RES_POIS, dam);
        break;

    case GF_ACID:
        dam = res_calc_dam(RES_ACID, dam);
        if (dam == 0)
            ignore_wraith_form = TRUE;
        break;

    case GF_COLD:
    case GF_ICE:
        dam = res_calc_dam(RES_COLD, dam);
        if (dam == 0)
            ignore_wraith_form = TRUE;
        break;

    case GF_FIRE:
        dam = res_calc_dam(RES_FIRE, dam);
        if (dam == 0)
            ignore_wraith_form = TRUE;
        break;

    case GF_PSY_SPEAR:
        ignore_wraith_form = TRUE;
        break;

    case GF_ARROW:
        if (!p_ptr->blind && equip_find_artifact(ART_ZANTETSU))
        {
            dam = 0;
            ignore_wraith_form = TRUE;
        }
        break;

    case GF_LITE:
        dam = res_calc_dam(RES_LITE, dam);
        /*
         * Cannot use "ignore_wraith_form" strictly (for "random one damage")
         * "dam *= 2;" for later "dam /= 2"
         */
        if (IS_WRAITH()) dam *= 2;
        break;

    case GF_DARK:
        dam = res_calc_dam(RES_DARK, dam);
        break;

    case GF_SHARDS:
        dam = res_calc_dam(RES_SHARDS, dam);
        break;

    case GF_SOUND:
        dam = res_calc_dam(RES_SOUND, dam);
        break;

    case GF_CONFUSION:
        dam = res_calc_dam(RES_CONF, dam);
        break;

    case GF_CHAOS:
        dam = res_calc_dam(RES_CHAOS, dam);
        break;

    case GF_NETHER:
        dam = res_calc_dam(RES_NETHER, dam);
        break;

    case GF_DISENCHANT:
        dam = res_calc_dam(RES_DISEN, dam);
        break;

    case GF_NEXUS:
        dam = res_calc_dam(RES_NEXUS, dam);
        break;

    case GF_TIME:
        dam = res_calc_dam(RES_TIME, dam);
        break;

    case GF_GRAVITY:
        if (p_ptr->levitation) dam = (dam * 2) / 3;
        break;

    case GF_ROCKET:
        dam = res_calc_dam(RES_SHARDS, dam);
        break;

    case GF_NUKE:
        dam = res_calc_dam(RES_POIS, dam);
        break;

    case GF_DEATH_RAY:
        if (get_race()->flags & RACE_IS_NONLIVING)
        {
            dam = 0;
            ignore_wraith_form = TRUE;
        }
        break;

    case GF_HOLY_FIRE:
        if (p_ptr->align > 10) dam /= 2;
        else if (p_ptr->align < -10) dam *= 2;
        break;

    case GF_HELL_FIRE:
        if (p_ptr->align > 10) dam *= 2;
        break;

    case GF_MIND_BLAST:
    case GF_BRAIN_SMASH:
        if (100 + rlev / 2 <= MAX(5, p_ptr->skills.sav))
        {
            dam = 0;
            ignore_wraith_form = TRUE;
        }
        break;

    case GF_CAUSE_1:
    case GF_CAUSE_2:
    case GF_CAUSE_3:
    case GF_HAND_DOOM:
        if (100 + rlev / 2 <= p_ptr->skills.sav)
        {
            dam = 0;
            ignore_wraith_form = TRUE;
        }
        break;

    case GF_CAUSE_4:
        if ((100 + rlev / 2 <= p_ptr->skills.sav) && (m_ptr->r_idx != MON_KENSHIROU))
        {
            dam = 0;
            ignore_wraith_form = TRUE;
        }
        break;
    }

    if (IS_WRAITH() && !ignore_wraith_form)
    {
        dam /= 2;
        if (!dam) dam = 1;
    }

    if (dam > *max) *max = dam;
}

/* Calculate blow damages */
static int blow_damcalc(monster_type *m_ptr, monster_blow *blow_ptr)
{
    int  dam = blow_ptr->d_dice * blow_ptr->d_side;
    int  dummy_max = 0;
    bool check_wraith_form = TRUE;

    if (blow_ptr->method != RBM_EXPLODE)
    {
        int ac = p_ptr->ac + p_ptr->to_a;

        switch (blow_ptr->effect)
        {
        case RBE_SUPERHURT:
        {
            int tmp_dam = dam - (dam * ((ac < 150) ? ac : 150) / 250);
            dam = MAX(dam, tmp_dam * 2);
            break;
        }

        case RBE_HURT:
        case RBE_SHATTER:
            dam -= (dam * ((ac < 150) ? ac : 150) / 250);
            break;

        case RBE_ACID:
            spell_damcalc(m_ptr, GF_ACID, dam, 0, &dummy_max);
            dam = dummy_max;
            check_wraith_form = FALSE;
            break;

        case RBE_ELEC:
            spell_damcalc(m_ptr, GF_ELEC, dam, 0, &dummy_max);
            dam = dummy_max;
            check_wraith_form = FALSE;
            break;

        case RBE_FIRE:
            spell_damcalc(m_ptr, GF_FIRE, dam, 0, &dummy_max);
            dam = dummy_max;
            check_wraith_form = FALSE;
            break;

        case RBE_COLD:
            spell_damcalc(m_ptr, GF_COLD, dam, 0, &dummy_max);
            dam = dummy_max;
            check_wraith_form = FALSE;
            break;

        case RBE_DR_MANA:
            dam = 0;
            check_wraith_form = FALSE;
            break;
        }

        if (check_wraith_form && IS_WRAITH())
        {
            dam /= 2;
            if (!dam) dam = 1;
        }
    }
    else
    {
        dam = (dam + 1) / 2;
        spell_damcalc(m_ptr, mbe_info[blow_ptr->effect].explode_type, dam, 0, &dummy_max);
        dam = dummy_max;
    }

    return dam;
}

/* Examine the grid (xx,yy) and warn the player if there are any danger */
bool process_warning(int xx, int yy)
{
    int mx, my;
    cave_type *c_ptr;
    char o_name[MAX_NLEN];

#define WARNING_AWARE_RANGE 12
    int dam_max = 0;
    static int old_damage = 0;

    for (mx = xx - WARNING_AWARE_RANGE; mx < xx + WARNING_AWARE_RANGE + 1; mx++)
    {
        for (my = yy - WARNING_AWARE_RANGE; my < yy + WARNING_AWARE_RANGE + 1; my++)
        {
            int dam_max0 = 0;
            monster_type *m_ptr;
            monster_race *r_ptr;

            if (!in_bounds(my, mx) || (distance(my, mx, yy, xx) > WARNING_AWARE_RANGE)) continue;

            c_ptr = &cave[my][mx];

            if (!c_ptr->m_idx) continue;

            m_ptr = &m_list[c_ptr->m_idx];

            if (MON_CSLEEP(m_ptr)) continue;
            if (!is_hostile(m_ptr)) continue;
            if (!is_aware(m_ptr)) continue;

            r_ptr = &r_info[m_ptr->r_idx];

            /* Monster spells (only powerful ones)*/
            if (projectable(my, mx, yy, xx))
            {
                int breath_dam_div3 = m_ptr->hp / 3;
                int breath_dam_div6 = m_ptr->hp / 6;
                u32b f4 = r_ptr->flags4;
                u32b f5 = r_ptr->flags5;
                u32b f6 = r_ptr->flags6;

                if (!(d_info[dungeon_type].flags1 & DF1_NO_MAGIC))
                {
                    int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
                    int storm_dam = rlev * 4 + 150;
                    bool powerful = (bool)(r_ptr->flags2 & RF2_POWERFUL);

                    if (f4 & RF4_BA_CHAO) spell_damcalc(m_ptr, GF_CHAOS, rlev * (powerful ? 3 : 2) + 100, 0, &dam_max0);
                    if (f5 & RF5_BA_MANA) spell_damcalc(m_ptr, GF_MANA, storm_dam, 0, &dam_max0);
                    if (f5 & RF5_BA_DARK) spell_damcalc(m_ptr, GF_DARK, storm_dam, 0, &dam_max0);
                    if (f5 & RF5_BA_LITE) spell_damcalc(m_ptr, GF_LITE, storm_dam, 0, &dam_max0);
                    if (f6 & RF6_HAND_DOOM) spell_damcalc(m_ptr, GF_HAND_DOOM, p_ptr->chp * 6 / 10, 0, &dam_max0);
                    if (f6 & RF6_PSY_SPEAR) spell_damcalc(m_ptr, GF_PSY_SPEAR, powerful ? (rlev * 2 + 150) : (rlev * 3 / 2 + 100), 0, &dam_max0);
                }
                if (f4 & RF4_ROCKET) spell_damcalc(m_ptr, GF_ROCKET, m_ptr->hp / 4, 800, &dam_max0);
                if (f4 & RF4_BR_ACID) spell_damcalc(m_ptr, GF_ACID, breath_dam_div3, 1600, &dam_max0);
                if (f4 & RF4_BR_ELEC) spell_damcalc(m_ptr, GF_ELEC, breath_dam_div3, 1600, &dam_max0);
                if (f4 & RF4_BR_FIRE) spell_damcalc(m_ptr, GF_FIRE, breath_dam_div3, 1600, &dam_max0);
                if (f4 & RF4_BR_COLD) spell_damcalc(m_ptr, GF_COLD, breath_dam_div3, 1600, &dam_max0);
                if (f4 & RF4_BR_POIS) spell_damcalc(m_ptr, GF_POIS, breath_dam_div3, 800, &dam_max0);
                if (f4 & RF4_BR_NETH) spell_damcalc(m_ptr, GF_NETHER, breath_dam_div6, 550, &dam_max0);
                if (f4 & RF4_BR_LITE) spell_damcalc(m_ptr, GF_LITE, breath_dam_div6, 400, &dam_max0);
                if (f4 & RF4_BR_DARK) spell_damcalc(m_ptr, GF_DARK, breath_dam_div6, 400, &dam_max0);
                if (f4 & RF4_BR_CONF) spell_damcalc(m_ptr, GF_CONFUSION, breath_dam_div6, 450, &dam_max0);
                if (f4 & RF4_BR_SOUN) spell_damcalc(m_ptr, GF_SOUND, breath_dam_div6, 450, &dam_max0);
                if (f4 & RF4_BR_CHAO) spell_damcalc(m_ptr, GF_CHAOS, breath_dam_div6, 600, &dam_max0);
                if (f4 & RF4_BR_DISE) spell_damcalc(m_ptr, GF_DISENCHANT, breath_dam_div6, 500, &dam_max0);
                if (f4 & RF4_BR_NEXU) spell_damcalc(m_ptr, GF_NEXUS, breath_dam_div3, 250, &dam_max0);
                if (f4 & RF4_BR_TIME) spell_damcalc(m_ptr, GF_TIME, breath_dam_div3, 150, &dam_max0);
                if (f4 & RF4_BR_INER) spell_damcalc(m_ptr, GF_INERT, breath_dam_div6, 200, &dam_max0);
                if (f4 & RF4_BR_GRAV) spell_damcalc(m_ptr, GF_GRAVITY, breath_dam_div3, 200, &dam_max0);
                if (f4 & RF4_BR_SHAR) spell_damcalc(m_ptr, GF_SHARDS, breath_dam_div6, 500, &dam_max0);
                if (f4 & RF4_BR_PLAS) spell_damcalc(m_ptr, GF_PLASMA, breath_dam_div6, 150, &dam_max0);
                if (f4 & RF4_BR_WALL) spell_damcalc(m_ptr, GF_FORCE, breath_dam_div6, 200, &dam_max0);
                if (f4 & RF4_BR_MANA) spell_damcalc(m_ptr, GF_MANA, breath_dam_div3, 250, &dam_max0);
                if (f4 & RF4_BR_NUKE) spell_damcalc(m_ptr, GF_NUKE, breath_dam_div3, 800, &dam_max0);
                if (f4 & RF4_BR_DISI) spell_damcalc(m_ptr, GF_DISINTEGRATE, breath_dam_div6, 150, &dam_max0);
            }

            /* Monster melee attacks */
            if (!(r_ptr->flags1 & RF1_NEVER_BLOW) && !(d_info[dungeon_type].flags1 & DF1_NO_MELEE))
            {
                if (mx <= xx + 1 && mx >= xx - 1 && my <= yy + 1 && my >= yy - 1)
                {
                    int m;
                    int dam_melee = 0;
                    for (m = 0; m < 4; m++)
                    {
                        /* Skip non-attacks */
                        if (!r_ptr->blow[m].method || (r_ptr->blow[m].method == RBM_SHOOT)) continue;

                        /* Extract the attack info */
                        dam_melee += blow_damcalc(m_ptr, &r_ptr->blow[m]);
                        if (r_ptr->blow[m].method == RBM_EXPLODE) break;
                    }
                    if (dam_melee > dam_max0) dam_max0 = dam_melee;
                }
            }

            /* Contribution from this monster */
            dam_max += dam_max0;
        }
    }

    /* Prevent excessive warning */
    if (dam_max > old_damage)
    {
        old_damage = dam_max * 3 / 2;

        if (dam_max > p_ptr->chp / 2)
        {
            object_type *o_ptr = choose_warning_item();

            if (o_ptr) object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
            else strcpy(o_name, "body"); /* Warning ability without item */
            msg_format("Your %s pulsates sharply!", o_name);
            if (o_ptr) obj_learn_flag(o_ptr, OF_WARNING);
            disturb(0, 0);
            return get_check("Really want to go ahead? ");
        }
    }
    else old_damage = old_damage / 2;

    c_ptr = &cave[yy][xx];
    if (((!easy_disarm && is_trap(c_ptr->feat))
        || (c_ptr->mimic && is_trap(c_ptr->feat))) && !one_in_(13))
    {
        object_type *o_ptr = choose_warning_item();

        if (o_ptr) object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
        else strcpy(o_name, "body"); /* Warning ability without item */
        msg_format("Your %s pulsates!", o_name);
        if (o_ptr) obj_learn_flag(o_ptr, OF_WARNING);
        disturb(0, 0);
        return get_check("Really want to go ahead? ");
    }

    return TRUE;
}
