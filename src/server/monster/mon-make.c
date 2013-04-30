/*
 * File: mon-make.c
 * Purpose: Monster creation / placement code.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "../cmds.h"
#include "../history.h"
#include "mon-lore.h"
#include "mon-make.h"
#include "mon-timed.h"
#include "mon-util.h"
#include "../s-spells.h"
#include "../target.h"


static bool clear_vis(int Ind, int depth, int m_idx)
{
    player_type *p_ptr = player_get(Ind);

    /* If he's not here, skip him */
    if (p_ptr->depth != depth) return FALSE;

    /* Clear some fields */
    p_ptr->mon_vis[m_idx] = FALSE;
    p_ptr->mon_los[m_idx] = FALSE;
    p_ptr->mon_det[m_idx] = 0;
    p_ptr->mon_hurt[m_idx] = FALSE;

    return TRUE;
}


static void delete_monster_objects(monster_type *m_ptr)
{
    s16b this_o_idx, next_o_idx = 0;

    /* Delete objects */
    for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Hack -- Efficiency */
        o_ptr->held_m_idx = -1;

        /* Hack -- Preserve artifacts */
        preserve_artifact(o_ptr);

        /* Delete the object */
        delete_object_idx(this_o_idx);
    }

    /* Delete mimicked objects */
    if (m_ptr->mimicked_o_idx > 0)
    {
        delete_object_idx(m_ptr->mimicked_o_idx);
        m_ptr->mimicked_o_idx = 0;
    }
}


/*
 * Deletes a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster_idx(struct cave *c, int m_idx)
{
    int x, y, Ind;
    monster_type *m_ptr;
    monster_race *r_ptr;

    my_assert(m_idx > 0);
    m_ptr = cave_monster(c, m_idx);
    r_ptr = &r_info[m_ptr->r_idx];

    /* Monster location */
    y = m_ptr->fy;
    x = m_ptr->fx;

    /* Unique is dead */
    r_ptr->lore.spawned = 0;

    /* Hack -- Decrease the number of clones */
    if (m_ptr->clone) c->num_clones--;

    /* Remove him from everybody's view */
    for (Ind = 1; Ind < NumPlayers + 1; Ind++)
    {
        player_type *p_ptr = player_get(Ind);

        /* If he's not here, skip him */
        if (!clear_vis(Ind, c->depth, m_idx)) continue;

        /* Hack -- Remove target monster */
        if (target_get_monster(Ind) == m_idx) target_set_monster(Ind, 0);

        /* Hack -- Remove tracked monster */
        if (p_ptr->health_who == m_idx) health_track(p_ptr, 0);

        /* Hack -- One less slave */
        if (p_ptr->id == m_ptr->master) p_ptr->slaves--;
    }

    /* Monster is gone */
    c->m_idx[y][x] = 0;

    /* Delete objects */
    delete_monster_objects(m_ptr);

    /* Delete mimicked features */
    if (r_ptr->base == lookup_monster_base("feature mimic"))
        cave_set_feat(c, y, x, FEAT_FLOOR);

    /* Wipe the Monster */
    WIPE(m_ptr, monster_type);

    /* Count monsters */
    c->mon_cnt--;

    /* Visual update */
    cave_light_spot(c, y, x);
}


/*
 * Deletes the monster, if any, at the given location.
 */
void delete_monster(int depth, int y, int x)
{
    my_assert(in_bounds(y, x));

    /* Paranoia */
    if (!cave_get(depth)) return;

    /* Delete the monster (if any) */
    if (cave_get(depth)->m_idx[y][x] > 0)
        delete_monster_idx(cave_get(depth), cave_get(depth)->m_idx[y][x]);
}


/*
 * Move a monster from index i1 to index i2 in the monster list.
 */
static void compact_monsters_aux(struct cave *c, int i1, int i2)
{
    int y, x, Ind;
    monster_type *m_ptr;
    s16b this_o_idx, next_o_idx = 0;

    /* Do nothing */
    if (i1 == i2) return;

    /* Old monster */
    m_ptr = cave_monster(c, i1);
    y = m_ptr->fy;
    x = m_ptr->fx;

    /* Update the cave */
    c->m_idx[y][x] = i2;

    /* Update midx */
    m_ptr->midx = i2;

    /* Repair objects being carried by monster */
    for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Reset monster pointer */
        o_ptr->held_m_idx = i2;
    }

    /* Move mimicked objects */
    if (m_ptr->mimicked_o_idx > 0)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(m_ptr->mimicked_o_idx);

        /* Reset monster pointer */
        o_ptr->mimicking_m_idx = i2;
    }

    /* Copy the visibility and los flags for the players */
    for (Ind = 1; Ind < NumPlayers + 1; Ind++)
    {
        player_type *p_ptr = player_get(Ind);

        /* If he's not here, skip him */
        if (p_ptr->depth != c->depth) continue;

        p_ptr->mon_vis[i2] = p_ptr->mon_vis[i1];
        p_ptr->mon_los[i2] = p_ptr->mon_los[i1];
        p_ptr->mon_det[i2] = p_ptr->mon_det[i1];
        p_ptr->mon_hurt[i2] = p_ptr->mon_hurt[i1];

        /* Hack -- Update the target */
        if (target_get_monster(Ind) == i1) target_set_monster(Ind, i2);

        /* Hack -- Update the health bar */
        if (p_ptr->health_who == i1) p_ptr->health_who = i2;
    }

    /* Hack -- Move monster */
    COPY(cave_monster(c, i2), cave_monster(c, i1), struct monster);

    /* Hack -- Wipe hole */
    WIPE(cave_monster(c, i1), monster_type);
}


/*
 * Compacts and reorders the monster list.
 *
 * This function can be very dangerous, use with caution!
 *
 * When `num_to_compact` is 0, we just reorder the monsters into a more compact
 * order, eliminating any "holes" left by dead monsters. If `num_to_compact` is
 * positive, then we delete at least that many monsters and then reorder.
 * We try not to delete monsters that are high level or close to the player.
 * Each time we make a full pass through the monster list, if we haven't
 * deleted enough monsters, we relax our bounds a little to accept
 * monsters of a slightly higher level, and monsters slightly closer to
 * the player.
 */
void compact_monsters(struct cave *c, int num_to_compact)
{
    int m_idx, num_compacted, iter;
    int max_lev, min_dis, chance;

    /* Message (only if compacting) */
    if (num_to_compact) plog("Compacting monsters...");

    /* Compact at least 'num_to_compact' monsters */
    for (num_compacted = 0, iter = 1; num_compacted < num_to_compact; iter++)
    {
        /* Get more vicious each iteration */
        max_lev = 5 * iter;

        /* Get closer each iteration */
        min_dis = 5 * (20 - iter);

        /* Check all the monsters */
        for (m_idx = 1; m_idx < cave_monster_max(c); m_idx++)
        {
            monster_type *m_ptr = cave_monster(c, m_idx);
            const monster_race *r_ptr = &r_info[m_ptr->r_idx];

            /* Skip "dead" monsters */
            if (!m_ptr->r_idx) continue;

            /* High level monsters start out "immune" */
            if (r_ptr->level > max_lev) continue;

            /* Ignore nearby monsters */
            if ((min_dis > 0) && (m_ptr->cdis < min_dis)) continue;

            /* Saving throw chance */
            chance = 90;

            /* Only compact "Quest" Monsters in emergencies */
            if (rf_has(r_ptr->flags, RF_QUESTOR) && (iter < 1000)) chance = 100;

            /* Try not to compact Unique Monsters */
            if (rf_has(r_ptr->flags, RF_UNIQUE)) chance = 99;

            /* Monsters in town don't have much of a chance */
            if (!c->depth) chance = 70;

            /* All monsters get a saving throw */
            if (magik(chance)) continue;

            /* Delete the monster */
            delete_monster_idx(c, m_idx);

            /* Count the monster */
            num_compacted++;
        }
    }

    /* Excise dead monsters (backwards!) */
    for (m_idx = cave_monster_max(c) - 1; m_idx >= 1; m_idx--)
    {
        monster_type *m_ptr = cave_monster(c, m_idx);

        /* Skip real monsters */
        if (m_ptr->r_idx) continue;

        /* Move last monster into open hole */
        compact_monsters_aux(c, cave_monster_max(c) - 1, m_idx);

        /* Compress "cave->mon_max" */
        c->mon_max--;
    }
}


/*
 * Deletes all the monsters when the player leaves the level.
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_monster()" function, with no visual effects.
 */
void wipe_mon_list(struct cave *c)
{
    int m_idx, Ind;

    /* Delete all the monsters */
    for (m_idx = cave_monster_max(c) - 1; m_idx >= 1; m_idx--)
    {
        monster_type *m_ptr = cave_monster(c, m_idx);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Unique is dead */
        r_ptr->lore.spawned = 0;

        /* Remove him from everybody's view */
        for (Ind = 1; Ind < NumPlayers + 1; Ind++)
        {
            player_type *p_ptr = player_get(Ind);

            clear_vis(Ind, c->depth, m_idx);

            /* Hack -- One less slave */
            if (p_ptr->id == m_ptr->master) p_ptr->slaves--;
        }

        /* Monster is gone */
        c->m_idx[m_ptr->fy][m_ptr->fx] = 0;

        /* Delete objects */
        delete_monster_objects(m_ptr);

        /* Delete mimicked features */
        if (r_ptr->base == lookup_monster_base("feature mimic"))
            cave_set_feat(c, m_ptr->fy, m_ptr->fx, FEAT_FLOOR);

        /* Wipe the Monster */
        WIPE(m_ptr, monster_type);
    }

    /* Reset "cave->mon_max" */
    c->mon_max = 1;

    /* Reset "mon_cnt" */
    c->mon_cnt = 0;

    /* Hack -- Reset the number of clones */
    c->num_clones = 0;

    for (Ind = 1; Ind < NumPlayers + 1; Ind++)
    {
        player_type *p_ptr = player_get(Ind);

        /* If he's not here, skip him */
        if (p_ptr->depth != c->depth) continue;

        /* Hack -- No more target */
        target_set_monster(Ind, 0);

        /* Hack -- No more tracking */
        health_track(p_ptr, 0);
    }
}


/*
 * Returns the index of a "free" monster, or 0 if no slot is available.
 *
 * This routine should almost never fail, but it *can* happen.
 * The calling code must check for and handle a 0 return.
 */
static s16b mon_pop(struct cave *c)
{
    int m_idx;

    /* Normal allocation */
    if (cave_monster_max(c) < z_info->m_max)
    {
        /* Get the next hole */
        m_idx = cave_monster_max(c);

        /* Expand the array */
        c->mon_max++;

        /* Count monsters */
        c->mon_cnt++;

        /* Return the index */
        return m_idx;
    }

    /* Recycle dead monsters if we've run out of room */
    for (m_idx = 1; m_idx < cave_monster_max(c); m_idx++)
    {
        monster_type *m_ptr;

        /* Get the monster */
        m_ptr = cave_monster(c, m_idx);

        /* Skip live monsters */
        if (m_ptr->r_idx) continue;

        /* Count monsters */
        c->mon_cnt++;

        /* Use this monster */
        return m_idx;
    }

    /* Warn the player if no index is available (except during dungeon creation) */
    if (!ht_zero(&c->generated)) plog("Too many monsters!");

    /* Try not to crash */
    return 0;
}


/*
 * Apply a "monster restriction function" to the "monster allocation table".
 * This way, we can use get_mon_num() to get a level-appropriate monster that
 * satisfies certain conditions (such as belonging to a particular monster
 * family).
 */
void get_mon_num_prep(void)
{
    int i;

    /* Scan the allocation table */
    for (i = 0; i < alloc_race_size; i++)
    {
        monster_race *r_ptr;

        /* Get the entry */
        alloc_entry *entry = &alloc_race_table[i];

        /* Skip non-entries */
        r_ptr = &r_info[entry->index];
        if (!r_ptr->name)
        {
            entry->prob2 = 0;
            continue;
        }

        /* Accept monsters which pass the restriction, if any */
        if (!get_mon_num_hook || (*get_mon_num_hook)(entry->index))
        {
            /* Accept this monster */
            entry->prob2 = entry->prob1;
        }

        /* Do not use this monster */
        else
        {
            /* Decline this monster */
            entry->prob2 = 0;
        }
    }
}


/*
 * Helper function for get_mon_num(). Scans the prepared monster allocation
 * table and picks a random monster. Returns the index of a monster in
 * `table`.
 */
static int get_mon_num_aux(long total, const alloc_entry *table)
{
    int i;
    long value;

    /* Pick a monster */
    value = randint0(total);

    /* Find the monster */
    for (i = 0; i < alloc_race_size; i++)
    {
        /* Found the entry */
        if (value < table[i].prob3) break;

        /* Decrement */
        value = value - table[i].prob3;
    }

    return i;
}


/* Scan all players on the level and see if at least one can find the unique */
static bool allow_unique_level(monster_race *r_ptr, int depth)
{
    int i;

    /* Uniques cannot be generated in the wilderness */
    if (depth < 0) return FALSE;

    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Is the player on the level and did he killed the unique already ? */
        if (!p_ptr->lore[r_ptr->ridx].pkills && (p_ptr->depth == depth))
        {
            /* One is enough */
            return TRUE;
        }
    }

    /* Yeah but we need at least ONE */
    return FALSE;
}


/*
 * Chooses a monster race that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "monster allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" monster, in
 * a relatively efficient manner.
 *
 * Note that "town" monsters will *only* be created in the town, and
 * "normal" monsters will *never* be created in the town, unless the
 * "level" is "modified", for example, by polymorph or summoning.
 *
 * There is a small chance (1/50) of "boosting" the given depth by
 * a small amount (up to four levels), except in the town.
 *
 * It is (slightly) more likely to acquire a monster of the given level
 * than one of a lower level.  This is done by choosing several monsters
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no monsters are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 */
s16b get_mon_num(int depth, int level)
{
    int i, j, p;
    int r_idx;
    long total;
    monster_race *r_ptr;
    alloc_entry *table = alloc_race_table;

    /* No monsters in the town or on special levels (no_recall servers) */
    if (cfg_no_recall && forbid_special(depth)) return (0);

    /* No monsters on special levels (more_towns servers) */
    if (cfg_more_towns && check_special_level(depth)) return (0);

    /* Limit the total number of townies */
    if (!depth && (cfg_max_townies != -1) && (cave_monster_count(cave_get(depth)) >= cfg_max_townies))
        return (0);

    /* Occasionally produce a nastier monster in the dungeon */
    if ((depth > 0) && one_in_(NASTY_MON))
        level += MIN(level / 4 + 2, MON_OOD_MAX);

    total = 0L;

    /* Process probabilities */
    for (i = 0; i < alloc_race_size; i++)
    {
        /* Monsters are sorted by depth */
        if (table[i].level > level) break;

        /* Default */
        table[i].prob3 = 0;

        /* Hack -- No town monsters outside of town */
        if (depth && (table[i].level <= 0)) continue;

        /* Get the chosen monster */
        r_idx = table[i].index;
        r_ptr = &r_info[r_idx];

        /* Hack -- "unique" monsters must be "unique" */
        if (rf_has(r_ptr->flags, RF_UNIQUE) &&
            (!allow_unique_level(r_ptr, depth) || r_ptr->lore.spawned))
        {
            continue;
        }

        /* Depth Monsters never appear out of depth */
        if (rf_has(r_ptr->flags, RF_FORCE_DEPTH) && (r_ptr->level > depth))
            continue;

        /* Handle PWMAngband base monsters */
        if (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters)
            continue;

        /* Handle PWMAngband extra monsters */
        if (rf_has(r_ptr->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters)
            continue;

        /* Accept */
        table[i].prob3 = table[i].prob2;

        /* Total */
        total += table[i].prob3;
    }

    /* No legal monsters */
    if (total <= 0) return (0);

    /* Pick a monster */
    i = get_mon_num_aux(total, table);

    /* Always try for a "harder" monster if too weak */
    if (table[i].level < (level / 2))
    {
        /* Save old */
        j = i;

        /* Pick a monster */
        i = get_mon_num_aux(total, table);

        /* Keep the deepest one */
        if (table[i].level < table[j].level) i = j;
    }        

    /* Always try for a "harder" monster deep in the dungeon */
    if (level >= 100)
    {
        /* Save old */
        j = i;

        /* Pick a monster */
        i = get_mon_num_aux(total, table);

        /* Keep the deepest one */
        if (table[i].level < table[j].level) i = j;
    }

    /* Try for a "harder" monster once (50%) or twice (10%) */
    p = randint0(100);
    if (p < 60)
    {
        /* Save old */
        j = i;

        /* Pick a monster */
        i = get_mon_num_aux(total, table);

        /* Keep the deepest one */
        if (table[i].level < table[j].level) i = j;
    }

    /* Try for a "harder" monster twice (10%) */
    if (p < 10)
    {
        /* Save old */
        j = i;

        /* Pick a monster */
        i = get_mon_num_aux(total, table);

        /* Keep the deepest one */
        if (table[i].level < table[j].level) i = j;
    }

    /* Result */
    return (table[i].index);
}


/*
 * Creates a specific monster's drop, including any drops specified
 * in the monster.txt file.
 */
static bool mon_create_drop(struct player *p, struct monster *m_ptr, byte origin)
{
    struct monster_drop *drop;
    monster_race *r_ptr;
    bool great, good, gold_ok, item_ok;
    bool any = FALSE;
    int number = 0, level, j;
    object_type *i_ptr;
    object_type object_type_body;
    quark_t quark = 0;

    r_ptr = &r_info[m_ptr->r_idx];
    great = rf_has(r_ptr->flags, RF_DROP_GREAT);
    good = (rf_has(r_ptr->flags, RF_DROP_GOOD) || great);
    gold_ok = !rf_has(r_ptr->flags, RF_ONLY_ITEM);
    item_ok = !rf_has(r_ptr->flags, RF_ONLY_GOLD);

    /* Hack -- Inscribe items that a unique drops */
    if (rf_has(r_ptr->flags, RF_UNIQUE)) quark = quark_add(r_ptr->name);

    /* Determine how much we can drop */
    if (rf_has(r_ptr->flags, RF_DROP_20) && magik(20)) number++;
    if (rf_has(r_ptr->flags, RF_DROP_40) && magik(40)) number++;
    if (rf_has(r_ptr->flags, RF_DROP_60) && magik(60)) number++;
    if (rf_has(r_ptr->flags, RF_DROP_4)) number += rand_range(2, 6);
    if (rf_has(r_ptr->flags, RF_DROP_3)) number += rand_range(2, 4);
    if (rf_has(r_ptr->flags, RF_DROP_2)) number += rand_range(1, 3);
    if (rf_has(r_ptr->flags, RF_DROP_1)) number++;

    /*
     * Take the best of (average of monster level and current depth)
     * and (monster level) - to reward fighting OOD monsters
     */
    level = MAX((m_ptr->level + object_level(m_ptr->depth)) / 2, m_ptr->level);

    /* Specified drops */
    for (drop = r_ptr->drops; drop; drop = drop->next)
    {
        bool ok = FALSE;

        if ((unsigned int)randint0(100) >= drop->percent_chance) continue;

        i_ptr = &object_type_body;

        /* Drop an artifact */
        if (drop->artifact)
        {
            object_prep(i_ptr, lookup_kind(drop->artifact->tval, drop->artifact->sval), level,
                RANDOMISE);
            i_ptr->artifact = drop->artifact;
            copy_artifact_data(i_ptr, i_ptr->artifact);
            i_ptr->artifact->created++;
            if (p)
            {
                if (!ht_zero(&cave_get(m_ptr->depth)->generated))
                    set_artifact_info(p, i_ptr, ARTS_GENERATED);
                else
                    p->art_info[i_ptr->artifact->aidx] += ARTS_CREATED;
            }

            ok = TRUE;
        }

        /* Hack -- "Nine rings for mortal men doomed to die"  */
        else if ((drop->kind->tval == TV_RING) && (drop->kind->sval == SV_RING_NAZGUL))
        {
            int i;

            if (!p || !cfg_random_artifacts) continue;

            object_prep(i_ptr, drop->kind, level, RANDOMISE);

            /* Make it a randart */
            for (i = z_info->a_max; i < z_info->a_max + 9; i++)
            {
                artifact_type *a_ptr;
                s32b randart_seed;

                /* Cannot make a randart twice */
                if (p->randart_created[i]) continue;

                /* Cannot generate a randart if disallowed by preservation mode  */
                if (p->randart_info[i] > cfg_preserve_artifacts) continue;

                /* Piece together a 32-bit random seed */
                randart_seed = randint0(0xFFFF) << 16;
                randart_seed += randint0(0xFFFF);

                /* Attempt to change the object into a random artifact */
                a_ptr = do_randart(randart_seed, &a_info[i]);

                /* Skip "empty" items */
                if (!a_ptr->name) continue;

                /* Mark the item as a random artifact */
                make_randart(p, i_ptr, a_ptr, randart_seed);

                /* Success */
                ok = TRUE;
                break;
            }
        }

        /* Drop an object */
        else
        {
            object_prep(i_ptr, drop->kind, level, RANDOMISE);
            apply_magic(p, m_ptr->depth, i_ptr, level, TRUE, good, great);

            ok = TRUE;
        }

        set_origin(i_ptr, origin, m_ptr->depth, m_ptr->r_idx);
        i_ptr->number = randint0(drop->max - drop->min) + drop->min;
        i_ptr->note = quark;
        if (ok && monster_carry(m_ptr, i_ptr, TRUE)) any = TRUE;
    }

    /* Make some objects */
    for (j = 0; j < number; j++)
    {
        i_ptr = &object_type_body;
        object_wipe(i_ptr);

        if (gold_ok && (!item_ok || magik(50)))
            make_gold(p, i_ptr, level, SV_GOLD_ANY);
        else
        {
            if (!make_object(p, cave_get(m_ptr->depth), i_ptr, level, good, great, NULL)) continue;
            i_ptr->note = quark;
        }

        set_origin(i_ptr, origin, m_ptr->depth, m_ptr->r_idx);
        if (monster_carry(m_ptr, i_ptr, TRUE)) any = TRUE;
    }

    return any;
}


/*
 * Creates monster drops, if not yet created
 */
void mon_create_drops(struct player *p, int m_idx)
{
    monster_type *m_ptr = cave_monster(cave_get(p->depth), m_idx);

    /* Create monster drops, if not yet created */
    if (m_ptr->origin)
    {
        mon_create_drop(p, m_ptr, m_ptr->origin);
        m_ptr->origin = ORIGIN_NONE;
    }
}


/*
 * Attempts to place a copy of the given monster at the given position in
 * the dungeon.
 *
 * All of the monster placement routines eventually call this function. This
 * is what actually puts the monster in the dungeon (i.e., it notifies the cave
 * and sets the monsters position). The dungeon loading code also calls this
 * function directly.
 *
 * `origin` is the item origin to use for any monster drops (e.g. ORIGIN_DROP,
 * ORIGIN_DROP_PIT, etc.) The dungeon loading code calls this with origin = 0,
 * which prevents the monster's drops from being generated again.
 *
 * Returns the m_idx of the newly copied monster, or 0 if the placement fails.
 */
s16b place_monster(struct player *p, monster_type *n_ptr, byte origin)
{
    s16b m_idx;
    monster_type *m_ptr;
    monster_race *r_ptr;
    struct cave *c = cave_get(n_ptr->depth);
    int y = n_ptr->fy;
    int x = n_ptr->fx;
    object_kind *kind = NULL;

    /* Paranoia: cave can be NULL (wilderness) */
    if (!c) return 0;

    my_assert(in_bounds(y, x));
    my_assert(c->m_idx[y][x] == 0);

    /* Get a new record */
    m_idx = mon_pop(c);

    if (!m_idx) return 0;
    n_ptr->midx = m_idx;

    /* Notify cave of the new monster */
    c->m_idx[y][x] = m_idx;

    /* Copy the monster */
    m_ptr = cave_monster(c, m_idx);
    COPY(m_ptr, n_ptr, monster_type);

    /* Get the new race */
    r_ptr = &r_info[m_ptr->r_idx];

    /* Hack -- Increase the number of clones */
    if (m_ptr->r_idx && m_ptr->clone) c->num_clones++;

    /* Create the monster's drop, if any */
    if (origin)
    {
        /* The dungeon is ready: create the drops */
        if (!ht_zero(&c->generated))
            mon_create_drop(p, m_ptr, origin);

        /* The dungeon is not ready: just set origin for later creation */
        else
            m_ptr->origin = origin;
    }

    /* Hack -- Random mimics */
    if (r_ptr->base == lookup_monster_base("random mimic"))
    {
        /* Random symbol from object set */
        while (1)
        {
            /* Select a random object */
            m_ptr->mimicked_k_idx = randint0(z_info->k_max - 1) + 1;

            /* Skip non-entries */
            if (!k_info[m_ptr->mimicked_k_idx].name) continue;

            /* Skip empty entries */
            if (!k_info[m_ptr->mimicked_k_idx].d_attr || !k_info[m_ptr->mimicked_k_idx].d_char)
                continue;

            /* Skip insta arts! */
            if (of_has(k_info[m_ptr->mimicked_k_idx].flags, OF_INSTA_ART)) continue;

            /* Force race attr */
            if (k_info[m_ptr->mimicked_k_idx].d_attr != r_ptr->d_attr) continue;

            /* Success */
            break;
        }

        kind = &k_info[m_ptr->mimicked_k_idx];
    }

    /* Hack -- Object mimics */
    else if (r_ptr->mimic_kinds)
    {
        struct monster_mimic *mimic_kind;
        int i = 1;

        /* Pick a random object kind to mimic */
        for (mimic_kind = r_ptr->mimic_kinds; mimic_kind; mimic_kind = mimic_kind->next, i++)
        {
            if (one_in_(i)) kind = mimic_kind->kind;
        }
    }

    /* Make mimics start mimicking */
    if (origin && kind)
    {
        object_type *i_ptr;
        object_type object_type_body;

        i_ptr = &object_type_body;

        if (kind->tval == TV_GOLD)
            make_gold(p, i_ptr, m_ptr->depth, kind->sval);
        else
        {
            object_prep(i_ptr, kind, r_ptr->level, RANDOMISE);
            apply_magic(p, m_ptr->depth, i_ptr, r_ptr->level, FALSE, FALSE, FALSE);
            i_ptr->number = 1;
        }

        set_origin(i_ptr, origin, m_ptr->depth, 0);
        i_ptr->mimicking_m_idx = m_idx;
        m_ptr->mimicked_o_idx = floor_carry(p, c, y, x, i_ptr, FALSE);
    }

    /* Hack -- Feature mimics */
    if (r_ptr->base == lookup_monster_base("feature mimic"))
    {
        /* Place corresponding feature under the monster */
        switch (r_ptr->d_char)
        {
            /* Place a door */
            case '+':
            {
                /* Push objects off the grid */
                if (c->o_idx[y][x]) push_object(p, y, x);

                /* Create a door */
                cave_set_feat(c, y, x, FEAT_DOOR_HEAD);

                break;
            }

            /* Place an up staircase */
            case '<':
            {
                /* Push objects off the grid */
                if (c->o_idx[y][x]) push_object(p, y, x);

                /* Create a staircase */
                cave_set_feat(c, y, x, FEAT_LESS);

                break;
            }

            /* Place a down staircase */
            case '>':
            {
                /* Push objects off the grid */
                if (c->o_idx[y][x]) push_object(p, y, x);

                /* Create a staircase */
                cave_set_feat(c, y, x, FEAT_MORE);

                break;
            }
        }
    }

    /* Result */
    return m_idx;
}


/*
 * Calculates hp for a monster. This function assumes that the Rand_normal
 * function has limits of +/- 4x std_dev. If that changes, this function
 * will become inaccurate.
 *
 * r_ptr is the race of the monster in question.
 * hp_aspect is the hp calc we want (min, max, avg, random).
 */
int mon_hp(const struct monster_race *r_ptr, aspect hp_aspect)
{
    int std_dev = (((r_ptr->avg_hp * 10) / 8) + 5) / 10;

    if (r_ptr->avg_hp > 1) std_dev++;

    switch (hp_aspect)
    {
        case MINIMISE:
            return (r_ptr->avg_hp - (4 * std_dev));
        case MAXIMISE:
            return (r_ptr->avg_hp + (4 * std_dev));
        case AVERAGE:
            return r_ptr->avg_hp;
        default:
            return Rand_normal(r_ptr->avg_hp, std_dev);
    }
}


/*
 * Attempts to place a monster of the given race at the given location.
 *
 * If `sleep` is true, the monster is placed with its default sleep value,
 * which is given in monster.txt.
 *
 * `origin` is the item origin to use for any monster drops (e.g. ORIGIN_DROP,
 * ORIGIN_DROP_PIT, etc.)
 *
 * To give the player a sporting chance, some especially dangerous
 * monsters are marked as "FORCE_SLEEP" in monster.txt, which will
 * cause them to be placed with low energy. This helps ensure that
 * if such a monster suddenly appears in line-of-sight (due to a
 * summon, for instance), the player gets a chance to move before
 * they do.
 *
 * This routine refuses to place out-of-depth "FORCE_DEPTH" monsters.
 *
 * This is the only function which may place a monster in the dungeon,
 * except for the savefile loading code, which calls place_monster()
 * directly.
 *
 * mon_flag = (MON_SLEEP, MON_CLONE)
 */
static bool place_new_monster_one(struct player *p, int depth, int y, int x, monster_race *r_ptr,
    byte mon_flag, byte origin)
{
    int i;
    monster_type *n_ptr;
    monster_type monster_type_body;
    struct cave *c = cave_get(depth);
    byte mspeed;

    my_assert(in_bounds(y, x));

    /* Require empty space */
    if (!cave_empty_bold(depth, y, x)) return (FALSE);

    /* No creation on glyph of warding */
    if (c->feat[y][x] == FEAT_GLYPH) return (FALSE);

    /* Hack -- No creation close to town inside house */
    if ((depth <= 0) && (depth > -16) && is_icky(depth, y, x)) return (FALSE);

    my_assert(r_ptr && r_ptr->name);

    /* "Unique" monsters must be "unique" */
    if (rf_has(r_ptr->flags, RF_UNIQUE) && (!allow_unique_level(r_ptr, depth) || r_ptr->lore.spawned))
        return (FALSE);

    /* Depth monsters may NOT be created out of depth */
    if (rf_has(r_ptr->flags, RF_FORCE_DEPTH) && (depth < r_ptr->level))
        return (FALSE);

    /* Handle PWMAngband base monsters */
    if (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters)
        return (FALSE);

    /* Handle PWMAngband extra monsters */
    if (rf_has(r_ptr->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters)
        return (FALSE);

    /* Get local monster */
    n_ptr = &monster_type_body;

    /* Clean out the monster */
    WIPE(n_ptr, monster_type);

    /* Save the race */
    n_ptr->r_idx = r_ptr->ridx;

    /* Enforce sleeping if needed */
    if ((mon_flag & MON_SLEEP) && r_ptr->sleep)
    {
        int val = r_ptr->sleep;

        n_ptr->m_timed[MON_TMD_SLEEP] = ((val * 2) + randint1(val * 10));
    }

    /* Uniques get a fixed amount of HP */
    if (rf_has(r_ptr->flags, RF_UNIQUE))
        n_ptr->maxhp = r_ptr->avg_hp;
    else
    {
        n_ptr->maxhp = mon_hp(r_ptr, RANDOMISE);
        n_ptr->maxhp = MAX(n_ptr->maxhp, 1);
    }

    /* Extract the monster base values */
    mspeed = r_ptr->speed;
    n_ptr->ac = r_ptr->ac;
    for (i = 0; i < 4; i++)
    {
        n_ptr->blow[i].method = r_ptr->blow[i].method;
        n_ptr->blow[i].effect = r_ptr->blow[i].effect;
        n_ptr->blow[i].d_dice = r_ptr->blow[i].d_dice;
        n_ptr->blow[i].d_side = r_ptr->blow[i].d_side;
    }
    n_ptr->level = r_ptr->level;

    /* Deep monsters are more powerful */
    if (depth > r_ptr->level)
    {
        /* Calculate a new level (up to +20) */
        n_ptr->level = r_ptr->level + ((r_ptr->level > 20)? 20: r_ptr->level) *
            (depth - r_ptr->level) / (MAX_DEPTH - 1 - r_ptr->level);

        for (i = 0; i < (n_ptr->level - r_ptr->level); i++)
        {
            s32b maxhp = (s32b)n_ptr->maxhp;

            /* Increase hp */
            maxhp += randint0(2 + r_ptr->avg_hp / 20);

            /* Put a cap on hp */
            n_ptr->maxhp = ((maxhp > 32000)? 32000: maxhp);

            /* Increase speed */
            mspeed += randint0(2);

            /* Increase ac */
            n_ptr->ac += randint0(2 + r_ptr->ac / 50);
        }

        /* Increase melee damage */
        for (i = 0; i < 4; i++)
        {
            n_ptr->blow[i].d_dice = (r_ptr->blow[i].d_dice *
                (n_ptr->level - r_ptr->level) * 3) % 200;
            n_ptr->blow[i].d_dice = ((n_ptr->blow[i].d_dice >= 100)?
                r_ptr->blow[i].d_dice + 1: r_ptr->blow[i].d_dice);
            n_ptr->blow[i].d_dice += r_ptr->blow[i].d_dice *
                (n_ptr->level - r_ptr->level) * 3 / 200;
            n_ptr->blow[i].d_side = (r_ptr->blow[i].d_side *
                (n_ptr->level - r_ptr->level) * 3) % 200;
            n_ptr->blow[i].d_side = ((n_ptr->blow[i].d_side >= 100)?
                r_ptr->blow[i].d_side + 1: r_ptr->blow[i].d_side);
            n_ptr->blow[i].d_side += r_ptr->blow[i].d_side *
                (n_ptr->level - r_ptr->level) * 3 / 200;
        }
    }

    /* And start out fully healthy */
    n_ptr->hp = n_ptr->maxhp;

    /* Extract the monster base speed */
    n_ptr->mspeed = mspeed;

    /* Give a random starting energy */
    n_ptr->energy = randint0(level_speed(0) >> 1);

    /* Force monster to wait for player */
    if (rf_has(r_ptr->flags, RF_FORCE_SLEEP))
        n_ptr->energy = randint0(level_speed(0) >> 4);

    /* Radiate light? */
    if (rf_has(r_ptr->flags, RF_HAS_LIGHT))
    {
        for (i = 1; i < NumPlayers + 1; i++)
        {
            player_type *p_ptr = player_get(i);

            if (p_ptr->depth == depth) p_ptr->update |= PU_UPDATE_VIEW;
        }
    }

    /* Is this obviously a monster? (Mimics etc. aren't) */
    if (rf_has(r_ptr->flags, RF_UNAWARE))
        n_ptr->unaware = TRUE;
    else
        n_ptr->unaware = FALSE;

    /* Unique has spawned */
    r_ptr->lore.spawned = 1;

    /* Hack -- Increase the number of clones */
    if (mon_flag & MON_CLONE) n_ptr->clone = 1;

    /* Place the monster in the dungeon */
    n_ptr->fy = y;
    n_ptr->fx = x;
    n_ptr->depth = depth;
    if (!place_monster(p, n_ptr, origin)) return (FALSE);

    /* Add to level feeling */
    c->mon_rating += r_ptr->power / 20;

    /* Check out-of-depth-ness */
    if (r_ptr->level > monster_level(depth))
    {
        /* Boost rating by power per 10 levels OOD */
        c->mon_rating += (r_ptr->level - monster_level(depth)) * r_ptr->power / 200;
    }

    for (i = 1; i < NumPlayers + 1; i++)
        clear_vis(i, depth, c->m_idx[y][x]);

    /* Update the monster */
    update_mon(depth, c->m_idx[y][x], TRUE);

    /* Success */
    return (TRUE);
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX   25


/*
 * Picks a monster group size. Used for monsters with the FRIENDS
 * flag and monsters with the ESCORT/ESCORTS flags.
 */
static int group_size_1(const monster_race *r_ptr, int depth)
{
    int total = 0, extra = 0;

    my_assert(r_ptr);

    /* Pick a group size */
    total = randint1(13);

    /* Hard monsters, small groups */
    if (r_ptr->level > depth)
        extra = 0 - randint1(r_ptr->level - depth);

    /* Easy monsters, large groups */
    else if (r_ptr->level < depth)
        extra = randint1(depth - r_ptr->level);

    total += extra;

    if (total < 1) total = 1;

    if (total > GROUP_MAX) total = GROUP_MAX;

    return total;
}


/*
 * Picks a monster group size. Used for monsters with the FRIEND
 * flag.
 */
static int group_size_2(const monster_race *r_ptr, int depth)
{
    int total = 0, extra = 0;

    my_assert(r_ptr);

    /* Start small */
    total = 1;

    /* Easy monsters, large groups */
    if (r_ptr->level < depth)
        extra = randint1(2 * (depth - r_ptr->level));

    total += extra;

    if (total > GROUP_MAX) total = GROUP_MAX;

    return total;
}


/*
 * Attempts to place a group of monsters of race `r_idx` around
 * the given location. The number of monsters to place is `total`.
 *
 * If `sleep` is true, the monster is placed with its default sleep value,
 * which is given in monster.txt.
 *
 * `origin` is the item origin to use for any monster drops (e.g. ORIGIN_DROP,
 * ORIGIN_DROP_PIT, etc.)
 *
 * mon_flag = (MON_SLEEP)
 */
static bool place_new_monster_group(struct player *p, struct cave *c, int y, int x,
    monster_race *r_ptr, byte mon_flag, int total, byte origin)
{
    int n, i;
    int hack_n;

    /* x and y coordinates of the placed monsters */
    byte hack_y[GROUP_MAX];
    byte hack_x[GROUP_MAX];

    my_assert(r_ptr);

    /* Start on the monster */
    hack_n = 1;
    hack_x[0] = x;
    hack_y[0] = y;

    /* Puddle monsters, breadth first, up to total */
    for (n = 0; (n < hack_n) && (hack_n < total); n++)
    {
        /* Grab the location */
        int hx = hack_x[n];
        int hy = hack_y[n];

        /* Check each direction, up to total */
        for (i = 0; (i < 8) && (hack_n < total); i++)
        {
            int mx = hx + ddx_ddd[i];
            int my = hy + ddy_ddd[i];

            /* Walls and Monsters block flow */
            if (!cave_empty_bold(c->depth, my, mx)) continue;

            /* Attempt to place another monster */
            if (place_new_monster_one(p, c->depth, my, mx, r_ptr, mon_flag, origin))
            {
                /* Add it to the "hack" set */
                hack_y[hack_n] = my;
                hack_x[hack_n] = mx;
                hack_n++;
            }
        }
    }

    /* Success */
    return (TRUE);
}


/*
 * Hack -- Help pick an escort type
 */
static int place_monster_idx = 0;


/*
 * Hack -- Helps pick an escort type. Requires place_monster_idx to be set.
 * Returns TRUE if monster race `r_idx` is appropriate as an escort for
 * the monster of race place_monster_idx.
 */
static bool place_monster_okay(int r_idx)
{
    monster_race *r_ptr;
    monster_race *z_ptr;

    my_assert(place_monster_idx > 0);
    r_ptr = &r_info[place_monster_idx];

    my_assert(r_idx > 0);
    z_ptr = &r_info[r_idx];

    /* Require identical monster template */
    if (z_ptr->base != r_ptr->base) return (FALSE);

    /* Skip more advanced monsters */
    if (z_ptr->level > r_ptr->level) return (FALSE);

    /* Skip unique monsters */
    if (rf_has(z_ptr->flags, RF_UNIQUE)) return (FALSE);

    /* Paranoia -- Skip identical monsters */
    if (place_monster_idx == r_idx) return (FALSE);

    /* Okay */
    return (TRUE);
}


/*
 * Attempts to place a monster of the given race at the given location.
 *
 * Note that certain monsters are placed with a large group of
 * identical or similar monsters. However, if `group_okay` is false,
 * then such monsters are placed by themselves.
 *
 * If `sleep` is true, the monster is placed with its default sleep value,
 * which is given in monster.txt.
 *
 * `origin` is the item origin to use for any monster drops (e.g. ORIGIN_DROP,
 * ORIGIN_DROP_PIT, etc.)
 *
 * Note the "bizarre" use of non-recursion to prevent annoying output
 * when running a code profiler.
 *
 * Note the use of the "monster allocation table" to restrict
 * the "get_mon_num()" function to "legal" escort types.
 *
 * mon_flag = (MON_SLEEP, MON_GROUP, MON_CLONE)
 */
bool place_new_monster(struct player *p, struct cave *c, int y, int x, int r_idx, byte mon_flag,
    byte origin)
{
    int i;
    monster_race *r_ptr;

    my_assert(c);

    my_assert(r_idx > 0);
    r_ptr = &r_info[r_idx];

    /* Place one monster, or fail */
    if (!place_new_monster_one(p, c->depth, y, x, r_ptr, mon_flag & ~(MON_GROUP), origin))
        return (FALSE);

    /* We're done unless the group flag is set */
    if (!(mon_flag & MON_GROUP)) return (TRUE);
    mon_flag &= ~(MON_GROUP | MON_CLONE);

    /* Friends for certain monsters */
    if (rf_has(r_ptr->flags, RF_FRIEND))
    {
        int total = group_size_2(r_ptr, c->depth);

        place_new_monster_group(p, c, y, x, r_ptr, mon_flag, total, origin);
    }

    /* Friends for certain monsters */
    if (rf_has(r_ptr->flags, RF_FRIENDS))
    {
        int total = group_size_1(r_ptr, c->depth);

        place_new_monster_group(p, c, y, x, r_ptr, mon_flag, total, origin);
    }

    /* Escorts for certain monsters */
    if (rf_has(r_ptr->flags, RF_ESCORT))
    {
        /* Try to place several "escorts" */
        for (i = 0; i < 50; i++)
        {
            int nx, ny, z, d = 3;
            monster_race *z_ptr;

            /* Pick a location */
            scatter(c->depth, &ny, &nx, y, x, d, FALSE);

            /* Require empty grids */
            if (!cave_empty_bold(c->depth, ny, nx)) continue;

            /* Set the escort index */
            place_monster_idx = r_idx;

            /* Set the escort hook */
            get_mon_num_hook = place_monster_okay;

            /* Prepare allocation table */
            get_mon_num_prep();

            /* Pick a random race */
            z = get_mon_num(c->depth, r_ptr->level);

            /* Remove restriction */
            get_mon_num_hook = NULL;

            /* Prepare allocation table */
            get_mon_num_prep();

            /* Handle failure */
            if (!z) break;

            /* Place a single escort */
            z_ptr = &r_info[z];
            place_new_monster_one(p, c->depth, ny, nx, z_ptr, mon_flag, origin);

            /* Place a "group" of escorts if needed */
            if (rf_has(z_ptr->flags, RF_FRIEND))
            {
                int total = group_size_2(z_ptr, c->depth);

                place_new_monster_group(p, c, ny, nx, z_ptr, mon_flag, total, origin);
            }

            /* Place a "group" of escorts if needed */
            if (rf_has(z_ptr->flags, RF_FRIENDS) || rf_has(r_ptr->flags, RF_ESCORTS))
            {
                int total = group_size_1(z_ptr, c->depth);

                place_new_monster_group(p, c, ny, nx, z_ptr, mon_flag, total, origin);
            }
        }
    }

    /* Success */
    return (TRUE);
}


/*
 * Picks a monster race, makes a new monster of that race, then attempts to
 * place it in the dungeon. The monster race chosen will be appropriate for
 * dungeon level equal to `depth`.
 *
 * If `sleep` is true, the monster is placed with its default sleep value,
 * which is given in monster.txt.
 *
 * If `group_okay` is true, we allow the placing of a group, if the chosen
 * monster appears with friends or an escort.
 *
 * `origin` is the item origin to use for any monster drops (e.g. ORIGIN_DROP,
 * ORIGIN_DROP_PIT, etc.)
 *
 * Returns TRUE if we successfully place a monster.
 *
 * mon_flag = (MON_SLEEP, MON_GROUP)
 */
bool pick_and_place_monster(struct player *p, struct cave *c, int y, int x, int depth,
    byte mon_flag, byte origin)
{
    int r_idx;

    /* Pick a monster race */
    r_idx = get_mon_num(c->depth, depth);

    /* Handle failure */
    if (!r_idx) return (FALSE);

    /* Attempt to place the monster */
    return (place_new_monster(p, c, y, x, r_idx, mon_flag, origin));
}


/*
 * Picks a monster race, makes a new monster of that race, then attempts to
 * place it in the dungeon at least `dis` away from the player. The monster
 * race chosen will be appropriate for dungeon level equal to `depth`.
 *
 * If `sleep` is true, the monster is placed with its default sleep value,
 * which is given in monster.txt.
 *
 * Returns TRUE if we successfully place a monster.
 *
 * mon_flag = (MON_SLEEP)
 */
bool pick_and_place_distant_monster(struct player *p, struct cave *c, int dis, byte mon_flag)
{
    int y = 0, x = 0;
    int attempts_left = 10000;

    my_assert(c);

    /* Find a legal, distant, unoccupied, space */
    while (--attempts_left)
    {
        int i, d, min_dis = 999;

        /* Pick a location */
        y = randint0(c->height);
        x = randint0(c->width);

        /* Require "naked" floor grid */
        if (!cave_isempty(c, y, x)) continue;

        /* Get min distance from all players on the level */
        for (i = 1; i < NumPlayers + 1; i++)
        {
            player_type *p_ptr = player_get(i);

            /* Skip him if he's not on this depth */
            if (p_ptr->depth != c->depth) continue;

            d = distance(y, x, p_ptr->py, p_ptr->px);
            if (d < min_dis) min_dis = d;
        }

        /* Accept far away grids */
        if (min_dis >= dis) break;
    }

    /* Abort */
    if (!attempts_left) return (FALSE);

    /* Attempt to place the monster, allow groups */
    if (pick_and_place_monster(p, c, y, x, monster_level(c->depth), mon_flag | MON_GROUP, ORIGIN_DROP))
        return (TRUE);

    /* Nope */
    return (FALSE);
}


/*
 * Split some experience between master and slaves.
 */
static void master_exp_gain(struct player *p, s32b *amount)
{
    int i;
    monster_type *m_ptr;
    s32b average_lev = p->lev, num_members = 1, modified_level;

    /* Calculate the average level */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        m_ptr = cave_monster(cave_get(p->depth), i);

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip non slaves */
        if (p->id != m_ptr->master) continue;

        /* Increase the "divisor" */
        average_lev += m_ptr->level;
        num_members++;
    }

    /* Calculate the master's experience */
    if (p->lev * num_members < average_lev)
    {
        /* Below average */
        if ((average_lev - p->lev * num_members) > 2 * num_members)
            modified_level = p->lev * num_members + 2 * num_members;
        else
            modified_level = average_lev;
    }
    else
    {
        /* Above average */
        if ((p->lev * num_members - average_lev) > 2 * num_members)
            modified_level = p->lev * num_members - 2 * num_members;
        else
            modified_level = average_lev;
    }

    *amount = (*amount * modified_level) / (average_lev * num_members);

    /* Always award 1 point */
    if (*amount < 1) *amount = 1;
}


/*
 * Handle the "death" of a monster: give experience
 */
void monster_give_xp(int Ind, struct monster *m_ptr, bool split)
{
    player_type *p_ptr = player_get(Ind);
    s32b new_exp, new_exp_frac, amount_exp;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Amount of experience earned */
    amount_exp = (long)r_ptr->mexp * m_ptr->level;

    /* Split experience between master and slaves */
    if (amount_exp && split) master_exp_gain(p_ptr, &amount_exp);

    /* Split experience if in a party */
    if (p_ptr->party)
    {
        /* Give experience to that party */
        party_exp_gain(Ind, p_ptr->party, amount_exp);
    }
    else
    {
        /* Give some experience */
        new_exp = amount_exp / p_ptr->lev;
        new_exp_frac = ((amount_exp % p_ptr->lev) * 0x10000L / p_ptr->lev) + p_ptr->exp_frac;

        /* Keep track of experience */
        if (new_exp_frac >= 0x10000L)
        {
            new_exp++;
            p_ptr->exp_frac = new_exp_frac - 0x10000L;
        }
        else
            p_ptr->exp_frac = new_exp_frac;

        /* Gain experience */
        player_exp_gain(p_ptr, new_exp);
    }
}


/*
 * Destroys the area around the winning player.
 * This is necessary to put the player, which will lose all true artifacts, in safety.
 */
static void crumble_angband(struct player *p, int fy, int fx)
{
    int y, x, k, t, j;
    int notice[MAX_PLAYERS];
    int count = 0;

    /* Huge area of effect */
    for (y = p->py - 50; y <= p->py + 50; y++)
    {
        for (x = p->px - 50; x <= p->px + 50; x++)
        {
            /* Skip illegal grids */
            if (!in_bounds_fully(y, x)) continue;

            /* Extract the distance */
            k = distance(p->py, p->px, y, x);

            /* Stay in the circle of death */
            if (k > 50) continue;

            /* Lose room and vault */
            cave_get(p->depth)->info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_NOTELE);

            /* Lose light */
            cave_get(p->depth)->info[y][x] &= ~(CAVE_GLOW);

            cave_light_spot(cave_get(p->depth), y, x);

            /* Hack -- Notice player */
            if (cave_get(p->depth)->m_idx[y][x] < 0)
            {
                /* Notice the player later */
                notice[count] = 0 - cave_get(p->depth)->m_idx[y][x];
                count++;

                /* Do not hurt this grid */
                continue;
            }

            /* Hack -- Skip the epicenter */
            if ((y == p->py) && (x == p->px)) continue;

            /* Hack -- Skip Morgoth (he will be removed later) */
            if ((y == fy) && (x == fx)) continue;

            /* Delete the monster (if any) */
            delete_monster(p->depth, y, x);

            /* Don't remove stairs */
            if (cave_isstairs(cave_get(p->depth), y, x)) continue;

            /* Lose knowledge (keeping knowledge of stairs) */
            forget_spot(p->depth, y, x);

            /* Destroy any grid that isn't a permanent wall */
            if (!cave_isperm(cave_get(p->depth), y, x))
            {
                int feat = FEAT_FLOOR;

                /* Delete the object (if any) */
                delete_object(p->depth, y, x);

                /* Wall (or floor) type */
                t = randint0(200);

                /* Granite */
                if (t < 20)
                {
                    /* Create granite wall */
                    feat = FEAT_WALL_EXTRA;
                }

                /* Quartz */
                else if (t < 70)
                {
                    /* Create quartz vein */
                    feat = FEAT_QUARTZ;
                }

                /* Magma */
                else if (t < 100)
                {
                    /* Create magma vein */
                    feat = FEAT_MAGMA;
                }

                /* Change the feature */
                cave_set_feat(cave_get(p->depth), y, x, feat);
            }
        }
    }

    /* Hack -- Update players */
    for (j = 0; j < count; j++)
    {
        player_type *p_ptr = player_get(notice[j]);

        /* Message */
        msg(p_ptr, "The ground shakes violently as the fortress of Angband starts to crumble down...");

        /* Fully update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Fully update the flow */
        p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

        /* Redraw */
        p_ptr->redraw |= (PR_MONLIST | PR_ITEMLIST);
    }
}


/*
 * Handles the "death" of a monster.
 *
 * Disperses treasures carried by the monster centered at the monster location.
 * Note that objects dropped may disappear in crowded rooms.
 *
 * Checks for "Quest" completion when a quest monster is killed.
 */
static void monster_death(int Ind, struct monster *m_ptr)
{
    player_type *p_ptr = player_get(Ind);
    int i, y, x;
    int dump_item = 0;
    int dump_gold = 0;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    bool visible;
    int winners = -1;

    visible = (p_ptr->mon_vis[m_ptr->midx] || rf_has(r_ptr->flags, RF_UNIQUE));

    /* Reward the player with experience */
    monster_give_xp(Ind, m_ptr, FALSE);

    /* Get the location */
    y = m_ptr->fy;
    x = m_ptr->fx;

    /* Hack -- Killing Morgoth marks some players as "winners" */
    if (r_ptr->base == lookup_monster_base("Morgoth"))
    {
        winners = 0;

        /* A bad day for evil... */
        crumble_angband(p_ptr, y, x);

        /* Total winners */
        for (i = 1; i <= NumPlayers; i++)
        {
            player_type *q_ptr = player_get(i);
            bool party_win = (party_share_with(Ind, p_ptr->party, i) && (q_ptr->lev >= 40) &&
                q_ptr->mon_hurt[m_ptr->midx]);

            /* Make the killer a total winner */
            /* Make high level party members on the same level total winners... */
            /* ... only if they participated in the fight! */
            /* Skip players that are already total winners */
            if (((i == Ind) || party_win) && !q_ptr->total_winner)
            {
                int j;

                /* Total winner */
                q_ptr->total_winner = TRUE;

                /* Redraw the "title" */
                q_ptr->redraw |= (PR_TITLE);

                /* Congratulations */
                msg(q_ptr, "*** CONGRATULATIONS ***");
                msg(q_ptr, "You have won the game!");
                msg(q_ptr, "You may retire (commit suicide) when you are ready.");

                /* "Winner dump" */
                my_strcpy(q_ptr->death_info.died_from, "winner", sizeof(q_ptr->death_info.died_from));
                player_dump(i);

                /* Set his retire_timer if necessary */
                if (cfg_retire_timer >= 0) q_ptr->retire_timer = cfg_retire_timer;

                /* Winners don't keep their true artifacts */
                for (j = ALL_INVEN_TOTAL - 1; j >= 0; j--)
                {
                    object_type *o_ptr = &q_ptr->inventory[j];
                    int amt = o_ptr->number;

                    if (true_artifact_p(o_ptr))
                    {
                        char o_name[NORMAL_WID];

                        /* Message */
                        object_desc(q_ptr, o_name, sizeof(o_name), o_ptr, ODESC_BASE);
                        msg(q_ptr, "The %s fades into the air!", o_name);

                        /* Preserve any true artifact */
                        preserve_artifact_aux(o_ptr);
                        history_lose_artifact(q_ptr, o_ptr);

                        /* Decrease the item, optimize. */
                        inven_item_increase(q_ptr, j, 0 - amt);
                        inven_item_optimize(q_ptr, j);
                    }
                }

                /* Notice */
                q_ptr->notice |= (PN_COMBINE | PN_REORDER);

                /* Redraw */
                q_ptr->redraw |= (PR_INVEN | PR_EQUIP);

                /* Hack -- Instantly retire any new winners if necessary */
                if (cfg_retire_timer == 0) do_cmd_suicide(i);

                /* If not, generate the rewards for that player */
                else winners++;
            }
        }
    }

    /* Drop objects being carried */
    monster_drop_carried(Ind, m_ptr, winners, visible, &dump_item, &dump_gold);

    /* Take note of any dropped treasure */
    if (visible && (dump_item || dump_gold))
        lore_treasure(Ind, m_ptr, dump_item, dump_gold);

    /* Process quest monsters */
    if (p_ptr->quest.r_idx && (p_ptr->quest.r_idx == m_ptr->r_idx))
    {
        p_ptr->quest.cur_num++;

        /* Note completed quests */
        if (p_ptr->quest.cur_num == p_ptr->quest.max_num)
        {
            quark_t quark_quest;
            char buf[MSG_LEN];

            msg(p_ptr, "You have just completed your current quest.");
            strnfmt(buf, sizeof(buf), "%s has won the %s quest!", p_ptr->name, r_ptr->name);
            msg_broadcast(p_ptr, buf);

            /* Quest is completed, clear it */
            p_ptr->quest.r_idx = 0;
            p_ptr->quest.cur_num = 0;
            p_ptr->quest.max_num = 0;

            /* Generate the reward */
            quark_quest = quark_add(format("%s quest", r_ptr->name));
            acquirement(p_ptr, p_ptr->depth, p_ptr->py, p_ptr->px, 1, quark_quest);
        }
        else
            msg(p_ptr, "%d more to go!", p_ptr->quest.max_num - p_ptr->quest.cur_num);
    }

    /* Drop a corpse */
    monster_drop_corpse(Ind, m_ptr);
}


/*
 * Decreases a monster's hit points by `dam` and handle monster death.
 *
 * Hack -- We "delay" fear messages by passing around a "fear" flag.
 *
 * We announce monster death using an optional "death message" (`note`)
 * if given, and a otherwise a generic killed/destroyed message.
 *
 * Returns TRUE if the monster has been killed (and deleted).
 */
bool mon_take_hit(int Ind, struct monster *m_ptr, int dam, bool *fear, const char *note)
{
    player_type *p_ptr = player_get(Ind);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
    char buf[MSG_LEN];
    char logbuf[MSG_LEN];
    int i;
    const char *p = get_title(p_ptr);
    bool cheeze;

    /* Killing an unique multiple times is cheezy! */
    /* Adding clones here to avoid cloning/breeding abuse */
    cheeze = ((rf_has(r_ptr->flags, RF_UNIQUE) && l_ptr->pkills) || m_ptr->clone);

    /* Redraw (later) if needed */
    update_health(m_ptr->midx);

    /* Wake it up */
    mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);

    /* Become aware of its presence */
    if (m_ptr->unaware) become_aware(p_ptr, m_ptr);

    /* Hurt it */
    m_ptr->hp -= dam;
    p_ptr->mon_hurt[m_ptr->midx] = TRUE;

    /* It is dead now */
    if (m_ptr->hp < 0)
    {
        char m_name[NORMAL_WID];

        /* Assume normal death sound */
        int soundfx = MSG_KILL;

        /* Play a special sound if the monster was unique */
        if (rf_has(r_ptr->flags, RF_UNIQUE))
        {
            if (r_ptr->base == lookup_monster_base("Morgoth"))
                soundfx = MSG_KILL_KING;
            else
                soundfx = MSG_KILL_UNIQUE;
        }

        /* Extract monster name */
        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

        /* Death by Missile/Spell attack */
        if (note)
        {
            /* Hack -- Allow message suppression */
            if (strlen(note) <= 1)
            {
                /* Be silent */
            }
            else
            {
                char name[NORMAL_WID];

                monster_desc(p_ptr, name, sizeof(name), m_ptr, MDESC_CAPITAL);
                msg_format_complex_near(p_ptr, MSG_GENERIC, "%s%s", name, note);
                msgt(p_ptr, soundfx, "%s%s", name, note);
            }
        }

        /* Death by physical attack -- Invisible monster */
        else if (!p_ptr->mon_vis[m_ptr->midx])
        {
            msg_format_near(p_ptr, MSG_GENERIC, " has killed %s.", m_name);
            msgt(p_ptr, soundfx, "You have killed %s.", m_name);
        }

        /* Death by physical attack -- Unusual monster */
        else if (monster_is_unusual(r_ptr))
        {
            msg_format_near(p_ptr, MSG_GENERIC, " has destroyed %s.", m_name);
            msgt(p_ptr, soundfx, "You have destroyed %s.", m_name);
        }

        /* Death by physical attack -- Normal monster */
        else
        {
            msg_format_near(p_ptr, MSG_GENERIC, " has slain %s.", m_name);
            msgt(p_ptr, soundfx, "You have slain %s.", m_name);
        }

        /* Take note of the killer (only the first time!) */
        if (rf_has(r_ptr->flags, RF_UNIQUE) && !l_ptr->pkills)
        {
            /* Give credit to the killer */
            strnfmt(buf, sizeof(buf), "%s was slain by %s %s.", r_ptr->name, p, p_ptr->name);

            /* Tell every player */
            msg_broadcast(p_ptr, buf);

            /* Message for event history */
            strnfmt(logbuf, sizeof(logbuf), "Killed %s", r_ptr->name);

            /* Record this kill in the event history */
            history_add_unique(p_ptr, logbuf, HISTORY_SLAY_UNIQUE);

            /* Give credit to party members who took part of the fight */
            for (i = 1; i <= NumPlayers; i++)
            {
                player_type *q_ptr = player_get(i);

                if (i == Ind) continue;

                /* Same party, same level, helped to kill */
                if (party_share_with(Ind, p_ptr->party, i) && q_ptr->mon_hurt[m_ptr->midx])
                {
                    /* Message for event history */
                    strnfmt(logbuf, sizeof(logbuf), "Helped to kill %s", r_ptr->name);

                    /* Record this kill in the event history */
                    history_add(q_ptr, logbuf, HISTORY_HELP_UNIQUE, NULL);
                }
            }
        }

        /* Take note of the kill */
        if (l_ptr->pkills < MAX_SHORT)
        {
            /* Remember */
            l_ptr->pkills++;
        }

        /* Count kills in all lives */
        if (r_ptr->lore.tkills < MAX_SHORT) r_ptr->lore.tkills++;

        /* Hack -- Auto-recall */
        monster_race_track(Ind, m_ptr->r_idx);

        /* Should we absorb its soul? */
        if (p_ptr->timed[TMD_SOUL] && !monster_is_nonliving(r_ptr))
        {
            int drain = 1 + (m_ptr->level / 2) + p_ptr->lev * 4 / 5;
            if (drain > m_ptr->maxhp) drain = m_ptr->maxhp;
            msg(p_ptr, "You absorb the life of the dying soul.");
            hp_player_safe(p_ptr, 1 + drain / 2);
        }

        /* Cheezy kills give neither xp nor loot! */
        if (!cheeze) monster_death(Ind, m_ptr);

        /* Redraw */
        p_ptr->redraw |= (PR_MONLIST | PR_ITEMLIST);

        /* Delete the monster */
        delete_monster_idx(cave_get(p_ptr->depth), m_ptr->midx);

        /* Not afraid */
        (*fear) = FALSE;

        /* Monster is dead */
        return (TRUE);
    }

    /* Mega-Hack -- Pain cancels fear */
    if (!(*fear) && m_ptr->m_timed[MON_TMD_FEAR] && (dam > 0))
    {
        int tmp = randint1(dam);

        /* Cure a little fear */
        if (tmp < m_ptr->m_timed[MON_TMD_FEAR])
        {
            /* Reduce fear */
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_FEAR, tmp, MON_TMD_FLG_NOMESSAGE, FALSE);
        }

        /* Cure all the fear */
        else
        {
            /* Cure fear */
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_FEAR, MON_TMD_FLG_NOMESSAGE, FALSE);

            /* No more fear */
            (*fear) = FALSE;
        }
    }

    /* Sometimes a monster gets scared by damage */
    if (!m_ptr->m_timed[MON_TMD_FEAR] && !rf_has(r_ptr->flags, RF_NO_FEAR))
    {
        int percentage;

        /* Percentage of fully healthy */
        percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

        /*
         * Run (sometimes) if at 10% or less of max hit points,
         * or (usually) when hit for half its current hit points
         */
        if (((percentage <= 10) && CHANCE(percentage, 10)) || ((dam >= m_ptr->hp) && magik(80)))
        {
            int timer = randint1(10) + (((dam >= m_ptr->hp) && (percentage > 7))? 20:
                ((11 - percentage) * 5));

            /* Hack -- note fear */
            (*fear) = TRUE;

            mon_inc_timed(p_ptr, m_ptr, MON_TMD_FEAR, timer,
                MON_TMD_FLG_NOMESSAGE | MON_TMD_FLG_NOFAIL, FALSE);
        }
    }

    /* Not dead yet */
    return (FALSE);
}


/*
 * Handle the "death" of a monster: drop carried objects
 */
void monster_drop_carried(int Ind, struct monster *m_ptr, int num, bool visible, int *dump_item,
    int *dump_gold)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    s16b this_o_idx, next_o_idx = 0;
    object_type *i_ptr;
    object_type object_type_body;

    /* Create monster drops, if not yet created */
    if (m_ptr->origin)
    {
        mon_create_drop(p_ptr, m_ptr, m_ptr->origin);
        m_ptr->origin = ORIGIN_NONE;
    }

    /* Get the location */
    y = m_ptr->fy;
    x = m_ptr->fx;

    /* Drop objects being carried */
    for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Line up the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Get local object, copy it and delete the original */
        i_ptr = &object_type_body;
        object_copy(i_ptr, o_ptr);
        o_ptr->held_m_idx = -1;
        delete_object_idx(this_o_idx);

        /* Count it and drop it - refactor once origin is a bitflag */
        if (dump_gold && (i_ptr->tval == TV_GOLD) && (i_ptr->origin != ORIGIN_STOLEN))
            (*dump_gold)++;
        else if (dump_item && (i_ptr->tval != TV_GOLD) && ((i_ptr->origin == ORIGIN_DROP) ||
            (i_ptr->origin == ORIGIN_DROP_PIT) || (i_ptr->origin == ORIGIN_DROP_VAULT) ||
            (i_ptr->origin == ORIGIN_DROP_SUMMON) || (i_ptr->origin == ORIGIN_DROP_SPECIAL) ||
            (i_ptr->origin == ORIGIN_DROP_BREED) || (i_ptr->origin == ORIGIN_DROP_POLY)))
        {
            (*dump_item)++;
        }

        /* Change origin if monster is invisible */
        if (!visible) i_ptr->origin = ORIGIN_DROP_UNKNOWN;

        /* Reset monster link */
        i_ptr->held_m_idx = 0;
        i_ptr->next_o_idx = 0;

        /* Special handling of Grond/Morgoth */
        if (i_ptr->artifact && ((i_ptr->artifact->aidx == ART_GROND) ||
            (i_ptr->artifact->aidx == ART_MORGOTH)))
        {
            if (num > 0) i_ptr->number = num;
            else continue;
        }

        drop_near(p_ptr, cave_get(m_ptr->depth), i_ptr, 0, y, x, TRUE);
    }

    /* Forget objects */
    m_ptr->hold_o_idx = 0;
}


/*
 * Handle the "death" of a monster: drop corpse
 */
void monster_drop_corpse(int Ind, struct monster *m_ptr)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Get the location */
    y = m_ptr->fy;
    x = m_ptr->fx;

    /* Sometimes, a dead monster leaves a corpse */
    if (rf_has(r_ptr->flags, RF_DROP_CORPSE) && one_in_(20))
    {
        object_type corpse;
        s32b timeout;

        /* Is the monster humanoid? */
        bool human = is_humanoid(r_ptr);

        /* Half chance to get a humanoid corpse from half-humanoids */
        if (is_half_humanoid(r_ptr)) human = magik(50);

        /* Prepare to make the corpse */
        object_prep(&corpse, lookup_kind(TV_CORPSE, (human? SV_CORPSE_HUMAN: SV_CORPSE_OTHER)), 0,
            MINIMISE);

        /* Remember the type of corpse */
        corpse.pval[DEFAULT_PVAL] = m_ptr->r_idx;

        /* Calculate length of time before decay */
        timeout = 5 + 2 * r_ptr->extra + randint0(2 * r_ptr->extra);
        if (timeout > 32000) timeout = 32000;
        corpse.pval[DEFAULT_PVAL + 1] = corpse.timeout = timeout;

        /* Set weight */
        corpse.weight = (r_ptr->extra + randint0(r_ptr->extra) / 10) + 1;

        /* Set origin */
        set_origin(&corpse, ORIGIN_DROP, m_ptr->depth, m_ptr->r_idx);

        /* Drop it in the dungeon */
        drop_near(p_ptr, cave_get(m_ptr->depth), &corpse, 0, y, x, TRUE);
    }

    /* Sometimes, a dead monster leaves a skeleton */
    else if (rf_has(r_ptr->flags, RF_DROP_SKELETON) && one_in_(m_ptr->depth? 40: 200))
    {
        object_type skeleton;
        int sval;

        /* Get the sval from monster race */
        if (r_ptr->base == lookup_monster_base("canine")) sval = SV_SKELETON_CANINE;
        else if (r_ptr->base == lookup_monster_base("rodent")) sval = SV_SKELETON_RODENT;
        else if ((r_ptr->base == lookup_monster_base("humanoid")) &&
            (strstr(r_ptr->name, "elf") || strstr(r_ptr->name, "elven")))
                sval = SV_SKELETON_ELF;
        else if (r_ptr->base == lookup_monster_base("kobold")) sval = SV_SKELETON_KOBOLD;
        else if (r_ptr->base == lookup_monster_base("orc")) sval = SV_SKELETON_ORC;
        else if (r_ptr->base == lookup_monster_base("person")) sval = SV_SKELETON_HUMAN;
        else if (streq(r_ptr->name, "Ettin")) sval = SV_SKELETON_ETTIN;
        else if (r_ptr->base == lookup_monster_base("troll")) sval = SV_SKELETON_TROLL;
        else if (r_ptr->level >= 15) sval = SV_SKELETON_SKULL;
        else sval = randint1(2);

        /* Prepare to make the skeleton */
        object_prep(&skeleton, lookup_kind(TV_SKELETON, sval), 0, MINIMISE);

        /* Set origin */
        set_origin(&skeleton, ORIGIN_DROP, m_ptr->depth, m_ptr->r_idx);

        /* Drop it in the dungeon */
        drop_near(p_ptr, cave_get(m_ptr->depth), &skeleton, 0, y, x, TRUE);
    }
}
