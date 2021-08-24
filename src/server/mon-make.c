/*
 * File: mon-make.c
 * Purpose: Monster creation / placement code.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


static s16b alloc_race_size;
static alloc_entry *alloc_race_table;


static void init_race_allocs(void)
{
    int i;
    struct monster_race *race;
    alloc_entry *table;
    s16b *num = mem_zalloc(z_info->max_depth * sizeof(s16b));
    s16b *aux = mem_zalloc(z_info->max_depth * sizeof(s16b));

    /*** Analyze monster allocation info ***/

    /* Size of "alloc_race_table" */
    alloc_race_size = 0;

    /* Scan the monsters */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Get the i'th race */
        race = &r_info[i];

        /* Legal monsters */
        if (race->rarity)
        {
            /* Count the entries */
            alloc_race_size++;

            /* Group by level */
            num[race->level]++;
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < z_info->max_depth; i++)
    {
        /* Group by level */
        num[i] += num[i - 1];
    }

    /* Paranoia */
    if (!num[0]) quit("No town monsters!");

    /*** Initialize monster allocation info ***/

    /* Allocate the alloc_race_table */
    alloc_race_table = mem_zalloc(alloc_race_size * sizeof(alloc_entry));

    /* Access the table entry */
    table = alloc_race_table;

    /* Scan the monsters */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Get the i'th race */
        race = &r_info[i];

        /* Count valid pairs */
        if (race->rarity)
        {
            int p, x, y, z;

            /* Extract the base level */
            x = race->level;

            /* Extract the base probability */
            p = (100 / race->rarity);

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
}


static void cleanup_race_allocs(void)
{
    mem_free(alloc_race_table);
}


static bool clear_vis(struct player *p, int depth, int m)
{
    /* If he's not here, skip him */
    if (p->depth != depth) return false;

    /* Clear some fields */
    mflag_wipe(p->mflag[m]);
    p->mon_det[m] = 0;

    return true;
}


/*
 * Deletes a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster_idx(struct chunk *c, int m_idx)
{
    int x, y, i;
    struct object *obj, *next;
    struct monster *mon;
    struct actor who_body;
    struct actor *who = &who_body;

    my_assert(m_idx > 0);

    mon = cave_monster(c, m_idx);
    ACTOR_MONSTER(who, mon);

    /* Monster location */
    y = mon->fy;
    x = mon->fx;
    my_assert(square_in_bounds(c, y, x));

    /* Unique is dead */
    mon->race->lore.spawned = 0;

    /* Hack -- decrease the number of clones */
    if (mon->clone) c->num_clones--;

    /* Remove him from everybody's view */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);
        struct actor *health_who = &p->upkeep->health_who;

        /* If he's not here, skip him */
        if (!clear_vis(p, c->depth, m_idx)) continue;

        /* Hack -- remove target monster */
        if (target_equals(p, who)) target_set_monster(p, NULL);

        /* Hack -- remove tracked monster */
        if (ACTOR_EQUAL(health_who, who)) health_track(p->upkeep, NULL);

        /* Hack -- one less slave */
        if (p->id == mon->master) p->slaves--;
    }

    /* Monster is gone */
    c->squares[y][x].mon = 0;

    /* Delete objects */
    obj = mon->held_obj;
    while (obj)
    {
        next = obj->next;

        /* Preserve unseen artifacts */
        preserve_artifact(obj);

        /* Delete the object */
        object_delete(&obj);
        obj = next;
    }

    /* Delete mimicked objects */
    obj = mon->mimicked_obj;
    if (obj)
    {
        square_excise_object(c, y, x, obj);
        object_delete(&obj);
    }

    /* Delete mimicked features */
    if (mon->race->base == lookup_monster_base("feature mimic"))
        square_set_feat(c, y, x, mon->feat);

    /* Wipe the Monster */
    mem_free(mon->blow);
    memset(mon, 0, sizeof(struct monster));

    /* Count monsters */
    c->mon_cnt--;

    /* Visual update */
    square_light_spot(c, y, x);
}


/*
 * Deletes the monster, if any, at the given location.
 */
void delete_monster(struct chunk *c, int y, int x)
{
    /* Paranoia */
    if (!c) return;

    my_assert(square_in_bounds(c, y, x));

    /* Delete the monster (if any) */
    if (c->squares[y][x].mon > 0)
        delete_monster_idx(c, c->squares[y][x].mon);
}


/*
 * Move a monster from index i1 to index i2 in the monster list.
 */
static void compact_monsters_aux(struct chunk *c, int i1, int i2)
{
    int y, x, i;
    struct monster *mon, *newmon;
    struct object *obj;
    struct actor mon1_body;
    struct actor *mon1 = &mon1_body;
    struct actor mon2_body;
    struct actor *mon2 = &mon2_body;
    struct monster_blow *blows;

    /* Do nothing */
    if (i1 == i2) return;

    /* Old monster */
    mon = cave_monster(c, i1);
    ACTOR_MONSTER(mon1, mon);
    y = mon->fy;
    x = mon->fx;

    /* New monster */
    newmon = cave_monster(c, i2);
    ACTOR_MONSTER(mon2, newmon);

    /* Update the cave */
    c->squares[y][x].mon = i2;

    /* Update midx */
    mon->midx = i2;

    /* Repair objects being carried by monster */
    for (obj = mon->held_obj; obj; obj = obj->next)
        obj->held_m_idx = i2;

    /* Move mimicked objects */
    if (mon->mimicked_obj)
        mon->mimicked_obj->mimicking_m_idx = i2;

    /* Copy the visibility and los flags for the players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);
        struct actor *health_who = &p->upkeep->health_who;

        /* If he's not here, skip him */
        if (p->depth != c->depth) continue;

        mflag_copy(p->mflag[i2], p->mflag[i1]);
        p->mon_det[i2] = p->mon_det[i1];

        /* Hack -- update the target */
        if (target_equals(p, mon1)) target_set_monster(p, mon2);

        /* Hack -- update the health bar */
        if (ACTOR_EQUAL(health_who, mon1)) health_track(p->upkeep, mon2);
    }

    /* Hack -- move monster */
    blows = newmon->blow;
    memcpy(newmon, mon, sizeof(struct monster));
    newmon->blow = blows;
    if (!newmon->blow)
        newmon->blow = mem_zalloc(z_info->mon_blows_max * sizeof(struct monster_blow));
    memcpy(newmon->blow, mon->blow, z_info->mon_blows_max * sizeof(struct monster_blow));

    /* Hack -- wipe hole */
    mem_free(mon->blow);
    memset(mon, 0, sizeof(struct monster));
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
void compact_monsters(struct chunk *c, int num_to_compact)
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
            struct monster *mon = cave_monster(c, m_idx);

            /* Skip "dead" monsters */
            if (!mon->race) continue;

            /* High level monsters start out "immune" */
            if (mon->race->level > max_lev) continue;

            /* Ignore nearby monsters */
            if ((min_dis > 0) && (mon->cdis < min_dis)) continue;

            /* Saving throw chance */
            chance = 90;

            /* Only compact "Quest" Monsters in emergencies */
            if (rf_has(mon->race->flags, RF_QUESTOR) && (iter < 1000)) chance = 100;

            /* Try not to compact Unique Monsters */
            if (rf_has(mon->race->flags, RF_UNIQUE)) chance = 99;

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
        struct monster *mon = cave_monster(c, m_idx);

        /* Skip real monsters */
        if (mon->race) continue;

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
 *
 * Note that we must delete the objects the monsters are carrying, but we
 * do nothing with mimicked objects.
 */
void wipe_mon_list(struct chunk *c)
{
    int m_idx, i;

    /* Delete all the monsters */
    for (m_idx = cave_monster_max(c) - 1; m_idx >= 1; m_idx--)
    {
        struct monster *mon = cave_monster(c, m_idx);
        struct object *held_obj;

        /* Skip dead monsters */
        if (!mon->race) continue;

        held_obj = mon->held_obj;

        /* Delete all the objects */
        while (held_obj)
        {
            struct object *next = held_obj->next;

            /* Go through all held objects and check for artifacts */
            preserve_artifact(held_obj);
            object_delete(&held_obj);
            held_obj = next;
        }

        /* Unique is dead */
        mon->race->lore.spawned = 0;

        /* Remove him from everybody's view */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *p = player_get(i);

            clear_vis(p, c->depth, m_idx);

            /* Hack -- one less slave */
            if (p->id == mon->master) p->slaves--;
        }

        /* Monster is gone */
        c->squares[mon->fy][mon->fx].mon = 0;

        /* Wipe the Monster */
        mem_free(mon->blow);
        memset(mon, 0, sizeof(struct monster));
    }

    /* Reset "cave->mon_max" */
    c->mon_max = 1;

    /* Reset "mon_cnt" */
    c->mon_cnt = 0;

    /* Hack -- reset the number of clones */
    c->num_clones = 0;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (p->depth != c->depth) continue;

        /* Hack -- no more target */
        target_set_monster(p, NULL);

        /* Hack -- no more tracking */
        health_track(p->upkeep, NULL);
    }
}


/*
 * Returns the index of a "free" monster, or 0 if no slot is available.
 *
 * This routine should almost never fail, but it *can* happen.
 * The calling code must check for and handle a 0 return.
 */
static s16b mon_pop(struct chunk *c)
{
    int m_idx;

    /* Normal allocation */
    if (cave_monster_max(c) < z_info->level_monster_max)
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
        struct monster *mon = cave_monster(c, m_idx);

        /* Skip live monsters */
        if (mon->race) continue;

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
void get_mon_num_prep(bool (*get_mon_num_hook)(struct monster_race *race))
{
    int i;

    /* Scan the allocation table */
    for (i = 0; i < alloc_race_size; i++)
    {
        struct monster_race *r;

        /* Get the entry */
        alloc_entry *entry = &alloc_race_table[i];

        /* Skip non-entries */
        r = &r_info[entry->index];
        if (!r->name)
        {
            entry->prob2 = 0;
            continue;
        }

        /* Accept monsters which pass the restriction, if any */
        if (!get_mon_num_hook || (*get_mon_num_hook)(r))
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
static struct monster_race *get_mon_race_aux(long total, const alloc_entry *table)
{
    int i;

    /* Pick a monster */
    long value = randint0(total);

    /* Find the monster */
    for (i = 0; i < alloc_race_size; i++)
    {
        /* Found the entry */
        if (value < table[i].prob3) break;

        /* Decrement */
        value -= table[i].prob3;
    }

    return &r_info[table[i].index];
}


/* Scan all players on the level and see if at least one can find the unique */
static bool allow_unique_level(struct monster_race *race, int depth)
{
    int i;

    /* Uniques cannot be generated in the wilderness */
    if (depth < 0) return false;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);
        struct monster_lore *lore = get_lore(p, race);

        /* Is the player on the level and did he killed the unique already ? */
        if (!lore->pkills && (p->depth == depth))
        {
            /* One is enough */
            return true;
        }
    }

    /* Yeah but we need at least ONE */
    return false;
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
struct monster_race *get_mon_num(struct chunk *c, int level)
{
    int i, p;
    long total;
    struct monster_race *race;
    alloc_entry *table = alloc_race_table;

    /* No monsters in the base town (no_recall servers) */
    if (cfg_no_recall && !c->depth) return (0);

    /* No monsters on special towns */
    if (special_town(c->depth)) return (0);

    /* Limit the total number of townies */
    if (!c->depth && (cfg_max_townies != -1) && (cave_monster_count(c) >= cfg_max_townies))
        return (0);

    /* Occasionally produce a nastier monster in the dungeon */
    if ((c->depth > 0) && one_in_(z_info->ood_monster_chance))
        level += MIN(level / 4 + 2, z_info->ood_monster_amount);

    total = 0L;

    /* Process probabilities */
    for (i = 0; i < alloc_race_size; i++)
    {
        /* Monsters are sorted by depth */
        if (table[i].level > level) break;

        /* Default */
        table[i].prob3 = 0;

        /* No town monsters outside of town */
        if (c->depth && (table[i].level <= 0)) continue;

        /* Get the chosen monster */
        race = &r_info[table[i].index];

        /* Only one copy of a a unique must be around at the same time */
        if (rf_has(race->flags, RF_UNIQUE) &&
            (!allow_unique_level(race, c->depth) || race->lore.spawned))
        {
            continue;
        }

        /* Some monsters never appear out of depth */
        if (rf_has(race->flags, RF_FORCE_DEPTH) && (race->level > c->depth))
            continue;

        /* Handle PWMAngband base monsters */
        if (rf_has(race->flags, RF_PWMANG_BASE) && !cfg_base_monsters)
            continue;

        /* Handle PWMAngband extra monsters */
        if (rf_has(race->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters)
            continue;

        /* Accept */
        table[i].prob3 = table[i].prob2;

        /* Total */
        total += table[i].prob3;
    }

    /* No legal monsters */
    if (total <= 0) return NULL;

    /* Pick a monster */
    race = get_mon_race_aux(total, table);

    /* Always try for a "harder" monster if too weak */
    if (race->level < (level / 2))
    {
        struct monster_race *old = race;

        /* Pick a new monster */
        race = get_mon_race_aux(total, table);

        /* Keep the deepest one */
        if (race->level < old->level) race = old;
    }

    /* Always try for a "harder" monster deep in the dungeon */
    if (level >= 100)
    {
        struct monster_race *old = race;

        /* Pick a new monster */
        race = get_mon_race_aux(total, table);

        /* Keep the deepest one */
        if (race->level < old->level) race = old;
    }

    /* Try for a "harder" monster once (50%) or twice (10%) */
    p = randint0(100);
    if (p < 60)
    {
        struct monster_race *old = race;

        /* Pick a new monster */
        race = get_mon_race_aux(total, table);

        /* Keep the deepest one */
        if (race->level < old->level) race = old;
    }

    /* Try for a "harder" monster twice (10%) */
    if (p < 10)
    {
        struct monster_race *old = race;

        /* Pick a new monster */
        race = get_mon_race_aux(total, table);

        /* Keep the deepest one */
        if (race->level < old->level) race = old;
    }

    /* Result */
    return race;
}


/*
 * Chooses a monster race for rings of polymorphing that seems "appropriate" to the given level.
 * This function uses most of the code from get_mon_num(), except depth checks.
 */
struct monster_race *get_mon_num_poly(int level)
{
    int i, p;
    long total;
    struct monster_race *race;
    alloc_entry *table = alloc_race_table;

    /* Occasionally produce a nastier monster */
    if (one_in_(z_info->ood_monster_chance))
        level += MIN(level / 4 + 2, z_info->ood_monster_amount);

    total = 0L;

    /* Process probabilities */
    for (i = 0; i < alloc_race_size; i++)
    {
        /* Monsters are sorted by depth */
        if (table[i].level > level) break;

        /* Default */
        table[i].prob3 = 0;

        /* Get the chosen monster */
        race = &r_info[table[i].index];

        /* Skip uniques */
        if (rf_has(race->flags, RF_UNIQUE)) continue;

        /* Handle PWMAngband base monsters */
        if (rf_has(race->flags, RF_PWMANG_BASE) && !cfg_base_monsters)
            continue;

        /* Handle PWMAngband extra monsters */
        if (rf_has(race->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters)
            continue;

        /* Accept */
        table[i].prob3 = table[i].prob2;

        /* Total */
        total += table[i].prob3;
    }

    /* No legal monsters */
    if (total <= 0) return NULL;

    /* Pick a monster */
    race = get_mon_race_aux(total, table);

    /* Try for a "harder" monster once (50%) or twice (10%) */
    p = randint0(100);
    if (p < 60)
    {
        struct monster_race *old = race;

        /* Pick a new monster */
        race = get_mon_race_aux(total, table);

        /* Keep the deepest one */
        if (race->level < old->level) race = old;
    }

    /* Try for a "harder" monster twice (10%) */
    if (p < 10)
    {
        struct monster_race *old = race;

        /* Pick a new monster */
        race = get_mon_race_aux(total, table);

        /* Keep the deepest one */
        if (race->level < old->level) race = old;
    }

    /* Result */
    return race;
}


/*
 * Return the number of things dropped by a monster.
 *
 * race is the monster race.
 * maximize should be set to false for a random number, true to find out the maximum count.
 */
int mon_create_drop_count(const struct monster_race *race, bool maximize)
{
    int number = 0;

    if (maximize)
    {
        if (rf_has(race->flags, RF_DROP_20)) number++;
        if (rf_has(race->flags, RF_DROP_40)) number++;
        if (rf_has(race->flags, RF_DROP_60)) number++;
        if (rf_has(race->flags, RF_DROP_4)) number += 6;
        if (rf_has(race->flags, RF_DROP_3)) number += 4;
        if (rf_has(race->flags, RF_DROP_2)) number += 3;
        if (rf_has(race->flags, RF_DROP_1)) number++;
    }
    else
    {
        if (rf_has(race->flags, RF_DROP_20) && magik(20)) number++;
        if (rf_has(race->flags, RF_DROP_40) && magik(40)) number++;
        if (rf_has(race->flags, RF_DROP_60) && magik(60)) number++;
        if (rf_has(race->flags, RF_DROP_4)) number += rand_range(2, 6);
        if (rf_has(race->flags, RF_DROP_3)) number += rand_range(2, 4);
        if (rf_has(race->flags, RF_DROP_2)) number += rand_range(1, 3);
        if (rf_has(race->flags, RF_DROP_1)) number++;
    }

    return number;
}


/*
 * Creates a specific monster's drop, including any drops specified
 * in the monster.txt file.
 */
static bool mon_create_drop(struct player *p, struct chunk *c, struct monster *mon, byte origin)
{
    struct monster_drop *drop;
    bool great, good, gold_ok, item_ok;
    bool extra_roll = false;
    bool any = false;
    int number = 0, level, j, monlevel;
    struct object *obj;
    quark_t quark = 0;

    my_assert(mon);

    great = rf_has(mon->race->flags, RF_DROP_GREAT);
    good = (rf_has(mon->race->flags, RF_DROP_GOOD) || great);
    gold_ok = !rf_has(mon->race->flags, RF_ONLY_ITEM);
    item_ok = !rf_has(mon->race->flags, RF_ONLY_GOLD);

    /* Hack -- inscribe items that a unique drops */
    if (rf_has(mon->race->flags, RF_UNIQUE)) quark = quark_add(mon->race->name);

    /* Determine how much we can drop */
    number = mon_create_drop_count(mon->race, false);

    /* Give added bonus for unique monters */
    monlevel = mon->level;
    if (rf_has(mon->race->flags, RF_UNIQUE))
    {
        monlevel = MIN(monlevel + 15, monlevel * 2);
        extra_roll = true;
    }

    /*
     * Take the best of (average of monster level and current depth)
     * and (monster level) - to reward fighting OOD monsters
     */
    level = MAX((monlevel + object_level(mon->depth)) / 2, monlevel);
    level = MIN(level, 100);

    /* Specified drops */
    for (drop = mon->race->drops; drop; drop = drop->next)
    {
        bool ok = false;

        if ((unsigned int)randint0(100) >= drop->percent_chance) continue;

        /* Allocate by hand, prep, apply magic */
        obj = object_new();
        if (drop->artifact)
        {
            object_prep(p, obj, lookup_kind(drop->artifact->tval, drop->artifact->sval), level,
                RANDOMISE);
            obj->artifact = drop->artifact;
            copy_artifact_data(obj, obj->artifact);
            obj->artifact->created++;
            if (p)
            {
                if (!ht_zero(&c->generated))
                    set_artifact_info(p, obj, ARTS_GENERATED);
                else
                    p->art_info[obj->artifact->aidx] += ARTS_CREATED;
            }

            ok = true;
        }

        /* Hack -- "Nine rings for mortal men doomed to die"  */
        else if (tval_is_ring_k(drop->kind) &&
            (drop->kind->sval == lookup_sval(TV_RING, "Black Ring of Power")))
        {
            int i;

            if (!p || !cfg_random_artifacts) continue;

            object_prep(p, obj, drop->kind, level, RANDOMISE);

            /* Make it a randart */
            for (i = z_info->a_max; i < z_info->a_max + 9; i++)
            {
                struct artifact *art;
                s32b randart_seed;

                /* Cannot make a randart twice */
                if (p->randart_created[i]) continue;

                /* Cannot generate a randart if disallowed by preservation mode  */
                if (p->randart_info[i] > cfg_preserve_artifacts) continue;

                /* Piece together a 32-bit random seed */
                randart_seed = randint0(0xFFFF) << 16;
                randart_seed += randint0(0xFFFF);

                /* Attempt to change the object into a random artifact */
                art = do_randart(randart_seed, &a_info[i]);

                /* Skip "empty" items */
                if (!art) continue;

                /* Mark the item as a random artifact */
                make_randart(p, c, obj, art, randart_seed);

                /* Success */
                free_artifact(art);
                ok = true;
                break;
            }
        }

        /* Drop an object */
        else
        {
            object_prep(p, obj, drop->kind, level, RANDOMISE);
            apply_magic(p, c, obj, level, true, good, great, extra_roll);

            ok = true;
        }

        /* Set origin details */
        set_origin(obj, origin, mon->depth, mon->race->ridx);
        obj->number = randint0(drop->max - drop->min) + drop->min;
        obj->note = quark;

        /* Try to carry */
        if (ok && monster_carry(mon, obj, true)) any = true;
        else
        {
            if (obj->artifact && obj->artifact->created) obj->artifact->created--;
            object_delete(&obj);
        }
    }

    /* Make some objects */
    for (j = 0; j < number; j++)
    {
        if (gold_ok && (!item_ok || magik(50)))
            obj = make_gold(p, level, "any");
        else
        {
            obj = make_object(p, c, level, good, great, extra_roll, NULL, 0);
            if (!obj) continue;
            obj->note = quark;
        }

        /* Set origin details */
        set_origin(obj, origin, mon->depth, mon->race->ridx);

        /* Try to carry */
        if (monster_carry(mon, obj, true)) any = true;
        else
        {
            if (obj->artifact && obj->artifact->created) obj->artifact->created--;
            object_delete(&obj);
        }
    }

    return any;
}


/*
 * Creates monster drops, if not yet created
 */
void mon_create_drops(struct player *p, struct chunk *c, struct monster *mon)
{
    /* Create monster drops, if not yet created */
    if (mon->origin)
    {
        mon_create_drop(p, c, mon, mon->origin);
        mon->origin = ORIGIN_NONE;
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
s16b place_monster(struct player *p, struct chunk *c, struct monster *mon, byte origin)
{
    s16b m_idx;
    struct monster *new_mon;
    int y = mon->fy;
    int x = mon->fx;
    struct object_kind *kind = NULL;

    /* Paranoia: cave can be NULL (wilderness) */
    if (!c) return 0;

    my_assert(square_in_bounds(c, y, x));
    my_assert(!square_monster(c, y, x));

    /* Get a new record */
    m_idx = mon_pop(c);
    if (!m_idx) return 0;

    /* Copy the monster */
    new_mon = cave_monster(c, m_idx);
    memcpy(new_mon, mon, sizeof(struct monster));
    new_mon->blow = mem_zalloc(z_info->mon_blows_max * sizeof(struct monster_blow));
    memcpy(new_mon->blow, mon->blow, z_info->mon_blows_max * sizeof(struct monster_blow));

    /* Set the ID */
    new_mon->midx = m_idx;

    /* Set the location */
    c->squares[y][x].mon = new_mon->midx;
    my_assert(square_monster(c, y, x) == new_mon);

    /* Hack -- increase the number of clones */
    if (new_mon->race->ridx && new_mon->clone) c->num_clones++;

    /* Done */
    if (!origin) return m_idx;

    /* The dungeon is ready: create the monster's drop, if any */
    if (!ht_zero(&c->generated))
        mon_create_drop(p, c, new_mon, origin);

    /* The dungeon is not ready: just set origin for later creation */
    else
        new_mon->origin = origin;

    /* Hack -- random mimics */
    if (new_mon->race->base == lookup_monster_base("random mimic"))
    {
        /* Random symbol from object set */
        while (1)
        {
            /* Select a random object */
            new_mon->mimicked_k_idx = randint0(z_info->k_max - 1) + 1;

            kind = &k_info[new_mon->mimicked_k_idx];

            /* Skip non-entries */
            if (!kind->name) continue;

            /* Skip empty entries */
            if (!kind->d_attr || !kind->d_char) continue;

            /* Skip insta arts! */
            if (kf_has(kind->kind_flags, KF_INSTA_ART) || kf_has(kind->kind_flags, KF_QUEST_ART))
                continue;

            /* Force race attr */
            if (kind->d_attr != new_mon->race->d_attr) continue;

            /* Success */
            break;
        }
    }

    /* Hack -- object mimics */
    else if (new_mon->race->mimic_kinds)
    {
        struct monster_mimic *mimic_kind;
        int i = 1;

        /* Pick a random object kind to mimic */
        for (mimic_kind = new_mon->race->mimic_kinds; mimic_kind; mimic_kind = mimic_kind->next, i++)
        {
            if (one_in_(i)) kind = mimic_kind->kind;
        }
    }

    /* Make mimics start mimicking */
    if (kind)
    {
        struct object *obj;

        if (tval_is_money_k(kind))
            obj = make_gold(p, new_mon->depth, kind->name);
        else
        {
            obj = object_new();
            object_prep(p, obj, kind, new_mon->race->level, RANDOMISE);
            apply_magic(p, c, obj, new_mon->race->level, false, false, false, false);
            obj->number = 1;
        }

        set_origin(obj, ORIGIN_DROP_MIMIC, new_mon->depth, 0);
        obj->mimicking_m_idx = m_idx;
        new_mon->mimicked_obj = obj;

        /* Put the object on the floor if it goes, otherwise no mimicry */
        if (!floor_carry(p, c, y, x, obj, false))
        {
            /* Clear the mimicry */
            obj->mimicking_m_idx = 0;
            new_mon->mimicked_obj = NULL;
            new_mon->unaware = false;

            /* Give the object to the monster if appropriate */
            /* Otherwise delete the mimicked object */
            if (!rf_has(new_mon->race->flags, RF_MIMIC_INV) || !monster_carry(new_mon, obj, true))
                object_delete(&obj);
        }
    }

    /* Hack -- feature mimics */
    if (new_mon->race->base == lookup_monster_base("feature mimic"))
    {
        /* Save original feature */
        new_mon->feat = c->squares[y][x].feat;

        /* Place corresponding feature under the monster */
        switch (new_mon->race->d_char)
        {
            /* Place a door */
            case '+':
            {
                /* Push objects off the grid */
                if (square_object(c, y, x)) push_object(p, c, y, x);

                /* Create a door */
                square_close_door(c, y, x);

                break;
            }

            /* Place an up staircase */
            case '<':
            {
                /* Push objects off the grid */
                if (square_object(c, y, x)) push_object(p, c, y, x);

                /* Create a staircase (fake depth to produce upstairs) */
                square_add_stairs(c, y, x, z_info->max_depth - 1);

                break;
            }

            /* Place a down staircase */
            case '>':
            {
                /* Push objects off the grid */
                if (square_object(c, y, x)) push_object(p, c, y, x);

                /* Create a staircase (fake depth to produce downstairs) */
                square_add_stairs(c, y, x, 0);

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
 * race is the race of the monster in question.
 * hp_aspect is the hp calc we want (min, max, avg, random).
 */
int mon_hp(const struct monster_race *race, aspect hp_aspect)
{
    int std_dev = (((race->avg_hp * 10) / 8) + 5) / 10;

    if (race->avg_hp > 1) std_dev++;

    switch (hp_aspect)
    {
        case MINIMISE:
            return (race->avg_hp - (4 * std_dev));
        case MAXIMISE:
            return (race->avg_hp + (4 * std_dev));
        case AVERAGE:
            return race->avg_hp;
        case RANDOMISE:
            return Rand_normal(race->avg_hp, std_dev);
    }

    return 0;
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
static bool place_new_monster_one(struct player *p, struct chunk *c, int y, int x,
    struct monster_race *race, byte mon_flag, byte origin)
{
    int i;
    struct monster *mon;
    struct monster monster_body;
    byte mspeed;

    my_assert(square_in_bounds(c, y, x));
    my_assert(race && race->name);

    /* Not where monsters already are */
    if (square_monster(c, y, x)) return false;

    /* Not where players already are */
    if (square_isplayer(c, y, x)) return false;

    /* Prevent monsters from being placed where they cannot walk, but allow other feature types */
    if (!square_is_monster_walkable(c, y, x)) return false;

    /* No creation on glyph of warding */
    if (square_iswarded(c, y, x)) return false;

    /* Hack -- no creation close to town inside house */
    if ((c->depth <= 0) && (c->depth > -16) && square_isvault(c, y, x)) return false;

    /* "Unique" monsters must be "unique" */
    if (rf_has(race->flags, RF_UNIQUE) && (!allow_unique_level(race, c->depth) || race->lore.spawned))
        return false;

    /* Depth monsters may NOT be created out of depth */
    if (rf_has(race->flags, RF_FORCE_DEPTH) && (c->depth < race->level)) return false;

    /* Handle PWMAngband base monsters */
    if (rf_has(race->flags, RF_PWMANG_BASE) && !cfg_base_monsters) return false;

    /* Handle PWMAngband extra monsters */
    if (rf_has(race->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters) return false;

    /* Get local monster */
    mon = &monster_body;

    /* Clean out the monster */
    memset(mon, 0, sizeof(struct monster));

    /* Save the race */
    mon->race = race;

    /* Enforce sleeping if needed */
    if ((mon_flag & MON_SLEEP) && race->sleep)
    {
        int val = race->sleep;

        mon->m_timed[MON_TMD_SLEEP] = ((val * 2) + randint1(val * 10));
    }

    /* Uniques get a fixed amount of HP */
    if (rf_has(race->flags, RF_UNIQUE))
        mon->maxhp = race->avg_hp;
    else
    {
        mon->maxhp = mon_hp(race, RANDOMISE);
        mon->maxhp = MAX(mon->maxhp, 1);
    }

    /* Extract the monster base values */
    mspeed = race->speed;
    mon->ac = race->ac;
    mon->blow = mem_zalloc(z_info->mon_blows_max * sizeof(struct monster_blow));
    for (i = 0; i < z_info->mon_blows_max; i++)
    {
        mon->blow[i].method = race->blow[i].method;
        mon->blow[i].effect = race->blow[i].effect;
        mon->blow[i].dice.dice = race->blow[i].dice.dice;
        mon->blow[i].dice.sides = race->blow[i].dice.sides;
    }
    mon->level = race->level;

    /* Deep monsters are more powerful */
    if (c->depth > race->level)
    {
        /* Calculate a new level (up to +20) */
        mon->level = race->level + ((race->level > 20)? 20: race->level) *
            (c->depth - race->level) / (z_info->max_depth - 1 - race->level);

        for (i = 0; i < (mon->level - race->level); i++)
        {
            s32b maxhp = (s32b)mon->maxhp;

            /* Increase hp */
            maxhp += randint0(2 + race->avg_hp / 20);

            /* Put a cap on hp */
            mon->maxhp = ((maxhp > 32000)? 32000: maxhp);

            /* Increase speed */
            mspeed += randint0(2);

            /* Increase ac */
            mon->ac += randint0(2 + race->ac / 50);
        }

        /* Increase melee damage */
        for (i = 0; i < z_info->mon_blows_max; i++)
        {
            mon->blow[i].dice.dice = (race->blow[i].dice.dice * (mon->level - race->level) * 3) % 200;
            mon->blow[i].dice.dice = ((mon->blow[i].dice.dice >= 100)?
                race->blow[i].dice.dice + 1: race->blow[i].dice.dice);
            mon->blow[i].dice.dice += race->blow[i].dice.dice * (mon->level - race->level) * 3 / 200;
            mon->blow[i].dice.sides = (race->blow[i].dice.sides * (mon->level - race->level) * 3) % 200;
            mon->blow[i].dice.sides = ((mon->blow[i].dice.sides >= 100)?
                race->blow[i].dice.sides + 1: race->blow[i].dice.sides);
            mon->blow[i].dice.sides += race->blow[i].dice.sides * (mon->level - race->level) * 3 / 200;
        }
    }

    /* And start out fully healthy */
    mon->hp = mon->maxhp;

    /* Extract the monster base speed */
    mon->mspeed = mspeed;

    /* Give a random starting energy */
    mon->energy = randint0(move_energy(0) >> 1);

    /* Force monster to wait for player */
    if (rf_has(race->flags, RF_FORCE_SLEEP))
        mon->energy = randint0(move_energy(0) >> 4);

    /* Radiate light? */
    if (rf_has(race->flags, RF_HAS_LIGHT))
    {
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *player = player_get(i);

            if (player->depth == c->depth) player->upkeep->update |= PU_UPDATE_VIEW;
        }
    }

    /* Is this obviously a monster? (Mimics etc. aren't) */
    if (rf_has(race->flags, RF_UNAWARE))
        mon->unaware = true;
    else
        mon->unaware = false;

    /* Unique has spawned */
    race->lore.spawned = 1;

    /* Hack -- increase the number of clones */
    if (mon_flag & MON_CLONE) mon->clone = 1;

    /* Place the monster in the dungeon */
    mon->fy = y;
    mon->fx = x;
    mon->depth = c->depth;
    if (!place_monster(p, c, mon, origin)) return false;

    /* Add to level feeling */
    c->mon_rating += race->power / 20;

    /* Check out-of-depth-ness */
    if (race->level > monster_level(c->depth))
    {
        /* Boost rating by power per 10 levels OOD */
        c->mon_rating += (race->level - monster_level(c->depth)) * race->power / 200;
    }

    for (i = 1; i <= NumPlayers; i++)
        clear_vis(player_get(i), c->depth, c->squares[y][x].mon);

    /* Update the monster */
    update_mon(square_monster(c, y, x), c, true);

    /* Success */
    return true;
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX   25


/*
 * Attempts to place a group of monsters of race `race` around
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
static bool place_new_monster_group(struct player *p, struct chunk *c, int y, int x,
    struct monster_race *race, byte mon_flag, int total, byte origin)
{
    int n, i;
    int hack_n;

    /* x and y coordinates of the placed monsters */
    byte hack_y[GROUP_MAX];
    byte hack_x[GROUP_MAX];

    my_assert(race);

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
            if (!square_isemptyfloor(c, my, mx)) continue;

            /* Attempt to place another monster */
            if (place_new_monster_one(p, c, my, mx, race, mon_flag, origin))
            {
                /* Add it to the "hack" set */
                hack_y[hack_n] = my;
                hack_x[hack_n] = mx;
                hack_n++;
            }
        }
    }

    /* Success */
    return true;
}


/* Maximum distance from center for a group of monsters */
#define GROUP_DISTANCE 5


static struct monster_base *place_monster_base = NULL;


/*
 * Predicate function for get_mon_num_prep
 * Check to see if the monster race has the same base as
 * place_monster_base.
 */
static bool place_monster_base_okay(struct monster_race *race)
{
    my_assert(place_monster_base);
    my_assert(race);

    /* Check if it matches */
    if (race->base != place_monster_base) return false;

    /* No uniques */
    if (rf_has(race->flags, RF_UNIQUE)) return false;

    return true;
}


/*
 * Helper function to place monsters that appear as friends or escorts
 */
static bool place_friends(struct player *p, struct chunk *c, int y, int x,
    struct monster_race *race, struct monster_race *friends_race, int total, byte mon_flag,
    byte origin)
{
    int level_difference, extra_chance, nx, ny, j;
    bool is_unique, success = true;

    /* Find the difference between current dungeon depth and monster level */
    level_difference = c->depth - friends_race->level + 5;

    /* Handle unique monsters */
    is_unique = rf_has(friends_race->flags, RF_UNIQUE);

    /* Make sure the unique hasn't been killed already */
    if (is_unique)
        total = ((allow_unique_level(friends_race, c->depth) && !friends_race->lore.spawned)? 1: 0);

    /* More than 4 levels OoD, no groups allowed */
    if ((level_difference <= 0) && !is_unique) return false;

    /* Reduce group size within 5 levels of natural depth */
    if ((level_difference < 10) && !is_unique)
    {
        extra_chance = (total * level_difference) % 10;
        total = total * level_difference / 10;

        /*
         * Instead of flooring the group value, we use the decimal place
         * as a chance of an extra monster
         */
        if (randint0(10) > extra_chance) total += 1;
    }

    /* No monsters in this group */
    if (total <= 0) return false;

    /* Handle friends same as original monster */
    if (race->ridx == friends_race->ridx)
        return place_new_monster_group(p, c, y, x, race, mon_flag, total, origin);

    /* Find a nearby place to put the other groups */
    for (j = 0; j < 50; j++)
    {
        scatter(c, &ny, &nx, y, x, GROUP_DISTANCE, false);
        if (square_isopen(c, ny, nx)) break;
    }

    /* Place the monsters */
    success = place_new_monster_one(p, c, ny, nx, friends_race, mon_flag, origin);
    if (total > 1)
        success = place_new_monster_group(p, c, ny, nx, friends_race, mon_flag, total, origin);

    return success;
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
 * mon_flag = (MON_SLEEP, MON_GROUP, MON_CLONE)
 */
bool place_new_monster(struct player *p, struct chunk *c, int y, int x,
    struct monster_race *race, byte mon_flag, byte origin)
{
    struct monster_friends *friends;
    struct monster_friends_base *friends_base;
    int total;

    my_assert(c);
    my_assert(race);

    /* Place one monster, or fail */
    if (!place_new_monster_one(p, c, y, x, race, mon_flag & ~(MON_GROUP), origin))
        return false;

    /* We're done unless the group flag is set */
    if (!(mon_flag & MON_GROUP)) return true;
    mon_flag &= ~(MON_GROUP | MON_CLONE);

    /* Go through friends flags */
    for (friends = race->friends; friends; friends = friends->next)
    {
        if ((unsigned int)randint0(100) >= friends->percent_chance)
            continue;

        /* Calculate the base number of monsters to place */
        total = damroll(friends->number_dice, friends->number_side);

        place_friends(p, c, y, x, race, friends->race, total, mon_flag, origin);
    }

    /* Go through the friends_base flags */
    for (friends_base = race->friends_base; friends_base; friends_base = friends_base->next)
    {
        struct monster_race *friends_race;

        /* Check if we pass chance for the monster appearing */
        if ((unsigned int)randint0(100) >= friends_base->percent_chance)
            continue;

        total = damroll(friends_base->number_dice, friends_base->number_side);

        /* Set the escort index base */
        place_monster_base = friends_base->base;

        /* Prepare allocation table */
        get_mon_num_prep(place_monster_base_okay);

        /* Pick a random race */
        friends_race = get_mon_num(c, race->level);

        /* Reset allocation table */
        get_mon_num_prep(NULL);

        /* Handle failure */
        if (!friends_race) break;

        place_friends(p, c, y, x, race, friends_race, total, mon_flag, origin);
    }

    /* Success */
    return true;
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
 * Returns true if we successfully place a monster.
 *
 * mon_flag = (MON_SLEEP, MON_GROUP)
 */
bool pick_and_place_monster(struct player *p, struct chunk *c, int y, int x, int depth,
    byte mon_flag, byte origin)
{
    /* Pick a monster race */
    struct monster_race *race = get_mon_num(c, depth);
    if (!race) return false;

    /* Attempt to place the monster */
    return (place_new_monster(p, c, y, x, race, mon_flag, origin));
}


/*
 * Picks a monster race, makes a new monster of that race, then attempts to
 * place it in the dungeon at least `dis` away from the player. The monster
 * race chosen will be appropriate for dungeon level equal to `depth`.
 *
 * If `sleep` is true, the monster is placed with its default sleep value,
 * which is given in monster.txt.
 *
 * Returns true if we successfully place a monster.
 *
 * mon_flag = (MON_SLEEP)
 */
bool pick_and_place_distant_monster(struct player *p, struct chunk *c, int dis, byte mon_flag)
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
        if (!square_isempty(c, y, x)) continue;

        /* Do not put random monsters in marked rooms. */
        if (square_ismon_restrict(c, y, x)) continue;

        /* Get min distance from all players on the level */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *player = player_get(i);

            /* Skip him if he's not on this depth */
            if (player->depth != c->depth) continue;

            d = distance(y, x, player->py, player->px);
            if (d < min_dis) min_dis = d;
        }

        /* Accept far away grids */
        if (min_dis >= dis) break;
    }

    /* Abort */
    if (!attempts_left) return false;

    /* Attempt to place the monster, allow groups */
    if (pick_and_place_monster(p, c, y, x, monster_level(c->depth), mon_flag | MON_GROUP,
        ORIGIN_DROP))
    {
        return true;
    }

    /* Nope */
    return false;
}


/*
 * Split some experience between master and slaves.
 */
static void master_exp_gain(struct player *p, struct chunk *c, s32b *amount)
{
    int i;
    struct monster *mon;
    s32b average_lev = p->lev, num_members = 1, modified_level;

    /* Calculate the average level */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        mon = cave_monster(c, i);

        /* Skip dead monsters */
        if (!mon->race) continue;

        /* Skip non slaves */
        if (p->id != mon->master) continue;

        /* Increase the "divisor" */
        average_lev += mon->level;
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
void monster_give_xp(struct player *p, struct chunk *c, struct monster *mon, bool split)
{
    s32b new_exp, new_exp_frac, amount_exp;

    /* Amount of experience earned */
    amount_exp = (long)mon->race->mexp * mon->level;

    /* Split experience between master and slaves */
    if (amount_exp && split) master_exp_gain(p, c, &amount_exp);

    /* Split experience if in a party */
    if (p->party)
    {
        /* Give experience to that party */
        party_exp_gain(p, p->party, amount_exp);
    }
    else
    {
        /* Give some experience */
        new_exp = amount_exp / p->lev;
        new_exp_frac = ((amount_exp % p->lev) * 0x10000L / p->lev) + p->exp_frac;

        /* Keep track of experience */
        if (new_exp_frac >= 0x10000L)
        {
            new_exp++;
            p->exp_frac = new_exp_frac - 0x10000L;
        }
        else
            p->exp_frac = new_exp_frac;

        /* Gain experience */
        player_exp_gain(p, new_exp);
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
static void monster_death(struct player *p, struct chunk *c, struct monster *mon)
{
    int dump_item = 0;
    int dump_gold = 0;
    bool visible = (mflag_has(p->mflag[mon->midx], MFLAG_VISIBLE) ||
        rf_has(mon->race->flags, RF_UNIQUE));
    int winners;

    /* Reward the player with experience */
    monster_give_xp(p, c, mon, false);

    /* Hack -- killing Morgoth marks some players as "winners" */
    winners = quest_check(p, c, mon);

    /* Drop objects being carried */
    monster_drop_carried(p, c, mon, winners, visible, &dump_item, &dump_gold);

    /* Take note of any dropped treasure */
    if (visible && (dump_item || dump_gold))
        lore_treasure(p, mon, dump_item, dump_gold);

    /* Process quest monsters */
    end_quest(p, c, mon);

    /* Drop a corpse */
    monster_drop_corpse(p, c, mon);
}


/*
 * Decreases a monster's hit points by `dam` and handle monster death.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * We announce monster death using an optional "death message" (`note`)
 * if given, and a otherwise a generic killed/destroyed message.
 *
 * Returns true if the monster has been killed (and deleted).
 */
bool mon_take_hit(struct player *p, struct chunk *c, struct monster *mon, int dam, bool *fear,
    int note)
{
    struct monster_lore *lore = get_lore(p, mon->race);
    char buf[MSG_LEN];
    char logbuf[MSG_LEN];
    int i;
    const char *title = get_title(p);
    bool cheeze;
    struct actor who_body;
    struct actor *who = &who_body;

    /* Killing an unique multiple times is cheezy! */
    /* Adding clones here to avoid cloning/breeding abuse */
    cheeze = ((rf_has(mon->race->flags, RF_UNIQUE) && lore->pkills) || mon->clone);

    /* Redraw (later) if needed */
    ACTOR_MONSTER(who, mon);
    update_health(who);

    /* Wake it up */
    mon_clear_timed(p, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, false);

    /* Become aware of its presence */
    if (mon->unaware) become_aware(p, c, mon);

    /* Hurt it */
    mon->hp -= dam;
    mflag_on(p->mflag[mon->midx], MFLAG_HURT);

    /* It is dead now */
    if (mon->hp < 0)
    {
        char m_name[NORMAL_WID];

        /* Assume normal death sound */
        int soundfx = MSG_KILL;

        /* Play a special sound if the monster was unique */
        if (rf_has(mon->race->flags, RF_UNIQUE))
        {
            if (mon->race->base == lookup_monster_base("Morgoth"))
                soundfx = MSG_KILL_KING;
            else
                soundfx = MSG_KILL_UNIQUE;
        }

        /* Extract monster name */
        monster_desc(p, m_name, sizeof(m_name), mon, MDESC_DEFAULT);

        /* Death message */
        switch (note)
        {
            /* Death by physical attack */
            case -2:
            {
                /* Make sure to flush any monster messages first */
                notice_stuff(p);

                /* Death by physical attack -- invisible monster */
                if (!mflag_has(p->mflag[mon->midx], MFLAG_VISIBLE))
                {
                    msg_format_near(p, MSG_GENERIC, " has killed %s.", m_name);
                    msgt(p, soundfx, "You have killed %s.", m_name);
                }

                /* Death by physical attack -- unusual monster */
                else if (monster_is_unusual(mon->race))
                {
                    msg_format_near(p, MSG_GENERIC, " has destroyed %s.", m_name);
                    msgt(p, soundfx, "You have destroyed %s.", m_name);
                }

                /* Death by physical attack -- normal monster */
                else
                {
                    msg_format_near(p, MSG_GENERIC, " has slain %s.", m_name);
                    msgt(p, soundfx, "You have slain %s.", m_name);
                }

                break;
            }

            /* Death by spell attack - messages handled by project_m() */
            case -1: break;

            /* Death by ranged attack */
            default:
            {
                char name[NORMAL_WID];

                monster_desc(p, name, sizeof(name), mon, MDESC_CAPITAL);
                switch (note)
                {
                    case MON_MSG_DESTROYED:
                        msg_format_complex_near(p, MSG_GENERIC, "%s is destroyed.", name);
                        break;
                    default:
                        msg_format_complex_near(p, MSG_GENERIC, "%s dies.", name);
                }
                add_monster_message(p, m_name, mon, note, true);
            }
        }

        /* Take note of the killer (only the first time!) */
        if (rf_has(mon->race->flags, RF_UNIQUE) && !lore->pkills)
        {
            /* Give credit to the killer */
            strnfmt(buf, sizeof(buf), "%s was slain by %s %s.", mon->race->name, title, p->name);

            /* Tell every player */
            msg_broadcast(p, buf, soundfx);

            /* Message for event history */
            strnfmt(logbuf, sizeof(logbuf), "Killed %s", mon->race->name);

            /* Record this kill in the event history */
            history_add_unique(p, logbuf, HIST_SLAY_UNIQUE);

            /* Give credit to party members who took part of the fight */
            for (i = 1; i <= NumPlayers; i++)
            {
                struct player *q = player_get(i);

                if (q == p) continue;

                /* Same party, same level, helped to kill */
                if (party_share_with(p, p->party, q) &&
                    mflag_has(q->mflag[mon->midx], MFLAG_HURT))
                {
                    /* Message for event history */
                    strnfmt(logbuf, sizeof(logbuf), "Helped to kill %s", mon->race->name);

                    /* Record this kill in the event history */
                    history_add(q, logbuf, HIST_HELP_UNIQUE, NULL);
                }
            }
        }

        /* Take note of the kill */
        if (lore->pkills < SHRT_MAX)
        {
            /* Remember */
            lore->pkills++;
        }

        /* Count kills in all lives */
        if (mon->race->lore.tkills < SHRT_MAX) mon->race->lore.tkills++;

        /* Update lore and tracking */
        lore_update(mon->race, lore);
        monster_race_track(p->upkeep, who);

        /* Should we absorb its soul? */
        if (p->timed[TMD_SOUL] && !monster_is_nonliving(mon->race))
        {
            int drain = 1 + (mon->level / 2) + p->lev * 4 / 5;
            if (drain > mon->maxhp) drain = mon->maxhp;
            msg(p, "You absorb the life of the dying soul.");
            hp_player_safe(p, 1 + drain / 2);
        }

        /* Cheezy kills give neither xp nor loot! */
        if (!cheeze) monster_death(p, c, mon);

        /* Redraw */
        p->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);

        /* Delete the monster */
        delete_monster_idx(c, mon->midx);

        /* Not afraid */
        (*fear) = false;

        /* Monster is dead */
        return true;
    }

    /* Hack -- pain cancels fear */
    if (!(*fear) && mon->m_timed[MON_TMD_FEAR] && (dam > 0))
    {
        int tmp = randint1(dam);

        /* Cure a little or all fear */
        if (tmp < mon->m_timed[MON_TMD_FEAR])
        {
            /* Reduce fear */
            mon_dec_timed(p, mon, MON_TMD_FEAR, tmp, MON_TMD_FLG_NOMESSAGE, false);
        }
        else
        {
            /* Cure fear */
            mon_clear_timed(p, mon, MON_TMD_FEAR, MON_TMD_FLG_NOMESSAGE, false);

            /* No more fear */
            (*fear) = false;
        }
    }

    /* Sometimes a monster gets scared by damage */
    if (!mon->m_timed[MON_TMD_FEAR] && !rf_has(mon->race->flags, RF_NO_FEAR))
    {
        int percentage;

        /* Percentage of fully healthy */
        percentage = (100L * mon->hp) / mon->maxhp;

        /*
         * Run (sometimes) if at 10% or less of max hit points,
         * or (usually) when hit for half its current hit points
         */
        if (((percentage <= 10) && CHANCE(percentage, 10)) || ((dam >= mon->hp) && magik(80)))
        {
            int timer = randint1(10) +
                (((dam >= mon->hp) && (percentage > 7))? 20: ((11 - percentage) * 5));

            /* Hack -- note fear */
            (*fear) = true;

            mon_inc_timed(p, mon, MON_TMD_FEAR, timer,
                MON_TMD_FLG_NOMESSAGE | MON_TMD_FLG_NOFAIL, false);
        }
    }

    /* Not dead yet */
    return false;
}


/*
 * Handle the "death" of a monster: drop carried objects
 */
void monster_drop_carried(struct player *p, struct chunk *c, struct monster *mon, int num,
    bool visible, int *dump_item, int *dump_gold)
{
    struct object *obj, *next;

    /* Create monster drops, if not yet created */
    if (mon->origin)
    {
        mon_create_drop(p, c, mon, mon->origin);
        mon->origin = ORIGIN_NONE;
    }

    /* Drop objects being carried */
    obj = mon->held_obj;
    while (obj)
    {
        next = obj->next;

        /* Object no longer held */
        obj->held_m_idx = 0;
        pile_excise(&mon->held_obj, obj);

        /* Count it and drop it - refactor once origin is a bitflag */
        if (dump_gold && tval_is_money(obj) && (obj->origin != ORIGIN_STOLEN))
            (*dump_gold)++;
        else if (dump_item && !tval_is_money(obj) && ((obj->origin == ORIGIN_DROP) ||
            (obj->origin == ORIGIN_DROP_PIT) || (obj->origin == ORIGIN_DROP_VAULT) ||
            (obj->origin == ORIGIN_DROP_SUMMON) || (obj->origin == ORIGIN_DROP_SPECIAL) ||
            (obj->origin == ORIGIN_DROP_BREED) || (obj->origin == ORIGIN_DROP_POLY)))
        {
            (*dump_item)++;
        }

        /* Change origin if monster is invisible */
        if (!visible) obj->origin = ORIGIN_DROP_UNKNOWN;

        /* Special handling of Grond/Morgoth */
        if (obj->artifact && kf_has(obj->kind->kind_flags, KF_QUEST_ART))
        {
            if (num > 0) obj->number = num;
            else
            {
                obj = next;
                continue;
            }
        }

        drop_near(p, c, obj, 0, mon->fy, mon->fx, true, DROP_FADE);
        obj = next;
    }

    /* Forget objects */
    mon->held_obj = NULL;
}


/*
 * Handle the "death" of a monster: drop corpse
 */
void monster_drop_corpse(struct player *p, struct chunk *c, struct monster *mon)
{
    int y, x;

    /* Get the location */
    y = mon->fy;
    x = mon->fx;

    /* Sometimes, a dead monster leaves a corpse */
    if (rf_has(mon->race->flags, RF_DROP_CORPSE) && one_in_(20))
    {
        struct object *corpse = object_new();
        s32b timeout;
        int sval;

        /* Is the monster humanoid? */
        bool human = is_humanoid(mon->race);

        /* Half chance to get a humanoid corpse from half-humanoids */
        if (is_half_humanoid(mon->race)) human = magik(50);

        /* Prepare to make the corpse */
        if (human) sval = lookup_sval(TV_CORPSE, "corpse (humanoid)");
        else sval = lookup_sval(TV_CORPSE, "corpse (other)");
        object_prep(p, corpse, lookup_kind(TV_CORPSE, sval), 0, MINIMISE);

        /* Remember the type of corpse */
        corpse->pval = mon->race->ridx;

        /* Calculate length of time before decay */
        timeout = 5 + 2 * mon->race->extra + randint0(2 * mon->race->extra);
        if (timeout > 32000) timeout = 32000;
        corpse->decay = corpse->timeout = timeout;

        /* Set weight */
        corpse->weight = (mon->race->extra + randint0(mon->race->extra) / 10) + 1;

        /* Set origin */
        set_origin(corpse, ORIGIN_DROP, mon->depth, mon->race->ridx);

        /* Drop it in the dungeon */
        drop_near(p, c, corpse, 0, y, x, true, DROP_FADE);
    }

    /* Sometimes, a dead monster leaves a skeleton */
    else if (rf_has(mon->race->flags, RF_DROP_SKELETON) && one_in_(mon->depth? 40: 200))
    {
        struct object *skeleton = object_new();
        int sval;

        /* Get the sval from monster race */
        if (mon->race->base == lookup_monster_base("canine"))
            sval = lookup_sval(TV_SKELETON, "Canine Skeleton");
        else if (mon->race->base == lookup_monster_base("rodent"))
            sval = lookup_sval(TV_SKELETON, "Rodent Skeleton");
        else if ((mon->race->base == lookup_monster_base("humanoid")) &&
            (strstr(mon->race->name, "elf") || strstr(mon->race->name, "elven")))
        {
            sval = lookup_sval(TV_SKELETON, "Elf Skeleton");
        }
        else if (mon->race->base == lookup_monster_base("kobold"))
            sval = lookup_sval(TV_SKELETON, "Kobold Skeleton");
        else if (mon->race->base == lookup_monster_base("orc"))
            sval = lookup_sval(TV_SKELETON, "Orc Skeleton");
        else if (mon->race->base == lookup_monster_base("person"))
            sval = lookup_sval(TV_SKELETON, "Human Skeleton");
        else if (streq(mon->race->name, "Ettin"))
            sval = lookup_sval(TV_SKELETON, "Ettin Skeleton");
        else if (mon->race->base == lookup_monster_base("troll"))
            sval = lookup_sval(TV_SKELETON, "Troll Skeleton");
        else if (mon->race->level >= 15)
            sval = lookup_sval(TV_SKELETON, "Skull");
        else if (one_in_(2))
            sval = lookup_sval(TV_SKELETON, "Broken Skull");
        else
            sval = lookup_sval(TV_SKELETON, "Broken Bone");

        /* Prepare to make the skeleton */
        object_prep(p, skeleton, lookup_kind(TV_SKELETON, sval), 0, MINIMISE);

        /* Set origin */
        set_origin(skeleton, ORIGIN_DROP, mon->depth, mon->race->ridx);

        /* Drop it in the dungeon */
        drop_near(p, c, skeleton, 0, y, x, true, DROP_FADE);
    }
}


struct init_module mon_make_module =
{
    "mon-make",
    init_race_allocs,
    cleanup_race_allocs
};
