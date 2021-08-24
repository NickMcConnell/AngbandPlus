/*
 * File: mon-summon.c
 * Purpose: Monster summoning
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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
 * The "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * The kin base for S_KIN
 */
struct monster_base *kin_base;


/*
 * The summon array
 */
struct summon *summons;


/*
 * Initialize monster summon types
 */


static enum parser_error parse_summon_name(struct parser *p)
{
    struct summon *h = parser_priv(p);
    struct summon *s = mem_zalloc(sizeof(*s));

    s->next = h;
    s->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, s);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_summon_message_type(struct parser *p)
{
    struct summon *s = parser_priv(p);
    int msg_index;
    const char *type;

    my_assert(s);

    type = parser_getsym(p, "type");

    msg_index = message_lookup_by_name(type);

    if (msg_index < 0)
        return PARSE_ERROR_INVALID_MESSAGE;

    s->message_type = msg_index;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_summon_unique(struct parser *p)
{
    struct summon *s = parser_priv(p);
    int unique;

    my_assert(s);

    unique = parser_getint(p, "allowed");

    if (unique) s->unique_allowed = true;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_summon_base(struct parser *p)
{
    struct summon *s = parser_priv(p);
    struct monster_base *base;
    struct monster_base_list *b;

    my_assert(s);

    base = lookup_monster_base(parser_getsym(p, "base"));
    if (base == NULL) return PARSE_ERROR_INVALID_MONSTER_BASE;

    b = mem_zalloc(sizeof(*b));
    b->base = base;
    b->next = s->bases;
    s->bases = b;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_summon_race_flag(struct parser *p)
{
    int flag;
    struct summon *s = parser_priv(p);

    my_assert(s);

    flag = lookup_flag(r_info_flags, parser_getsym(p, "flag"));

    if (flag == FLAG_END) return PARSE_ERROR_INVALID_FLAG;

    s->race_flag = flag;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_summon_fallback(struct parser *p)
{
    struct summon *s = parser_priv(p);

    my_assert(s);

    s->fallback_name = string_make(parser_getstr(p, "fallback"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_summon_desc(struct parser *p)
{
    struct summon *s = parser_priv(p);

    my_assert(s);

    s->desc = string_make(parser_getstr(p, "desc"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_summon(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_summon_name);
    parser_reg(p, "msgt sym type", parse_summon_message_type);
    parser_reg(p, "uniques int allowed", parse_summon_unique);
    parser_reg(p, "base sym base", parse_summon_base);
    parser_reg(p, "race-flag sym flag", parse_summon_race_flag);
    parser_reg(p, "fallback str fallback", parse_summon_fallback);
    parser_reg(p, "desc str desc", parse_summon_desc);

    return p;
}


static errr run_parse_summon(struct parser *p)
{
    return parse_file_quit_not_found(p, "summon");
}


static errr finish_parse_summon(struct parser *p)
{
    struct summon *s, *n;
    int index;

    /* Scan the list for the max id */
    z_info->summon_max = 0;
    s = parser_priv(p);
    while (s)
    {
        z_info->summon_max++;
        s = s->next;
    }

    /* Allocate the direct access list and copy the data to it */
    summons = mem_zalloc(z_info->summon_max * sizeof(*s));
    index = z_info->summon_max - 1;
    for (s = parser_priv(p); s; s = n, index--)
    {
        memcpy(&summons[index], s, sizeof(*s));
        n = s->next;
        summons[index].next = NULL;
        mem_free(s);
    }

    /* Add indices of fallback summons */
    for (index = 0; index < z_info->summon_max; index++)
    {
        char *name = summons[index].fallback_name;

        summons[index].fallback = summon_name_to_idx(name);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_summon(void)
{
    int i;

    /* Paranoia */
    if (!summons) return;

    for (i = 0; i < z_info->summon_max; i++)
    {
        struct summon *s = &summons[i];
        struct monster_base_list *b, *bn;

        b = s->bases;
        while (b)
        {
            bn = b->next;
            mem_free(b);
            b = bn;
        }

        string_free(s->desc);
        string_free(s->fallback_name);
        string_free(s->name);
    }
    mem_free(summons);
}


struct file_parser summon_parser =
{
    "summon",
    init_parse_summon,
    run_parse_summon,
    finish_parse_summon,
    cleanup_summon
};


/*
 * Lookup function to translate names of summons to indices
 */
int summon_name_to_idx(const char *name)
{
    int i;

    for (i = 0; i < z_info->summon_max; i++)
    {
        if (name && streq(name, summons[i].name)) return i;
    }

    return -1;
}


/*
 * The message type for a particular summon
 */
int summon_message_type(int summon_type)
{
    return summons[summon_type].message_type;
}


/*
 * The fallback type for a particular summon
 */
int summon_fallback_type(int summon_type)
{
    return summons[summon_type].fallback;
}


/*
 * The description for a particular summon
 */
const char *summon_desc(int type)
{
    if ((type < 0) || (type >= z_info->summon_max)) return NULL;
    return summons[type].desc;
}


/*
 * Decide if a monster race is "okay" to summon.
 *
 * Compares the given monster to the monster type specified by
 * summon_specific_type. Returns true if the monster is eligible to
 * be summoned, false otherwise.
 */
static bool summon_specific_okay(struct monster_race *race)
{
    struct summon *summon = &summons[summon_specific_type];
    struct monster_base_list *bases = summon->bases;
    bool unique = monster_is_unique(race);

    /* Forbid uniques? */
    if (!summon->unique_allowed && unique) return false;

    /* A valid base and no match means disallowed */
    while (bases)
    {
        if (race->base == bases->base) break;
        if (bases->next == NULL) return false;
        bases = bases->next;
    }

    /* A valid race flag and no match means disallowed */
    if (summon->race_flag && !rf_has(race->flags, summon->race_flag)) return false;

    /* Special case - summon kin */
    if (summon_specific_type == summon_name_to_idx("KIN"))
        return (!unique && (race->base == kin_base));

    /* If we made it here, we're fine */
    return true;
}


/*
 * Check to see if you can call the monster
 */
static bool can_call_monster(struct chunk *c, struct loc *grid, struct monster *mon)
{
    /* Skip dead monsters */
    if (!mon->race) return false;

    /* Only consider callable monsters */
    if (!summon_specific_okay(mon->race)) return false;

    /* Make sure the summoned monster is not in LOS of the summoner */
    if (los(c, grid, &mon->grid)) return false;

    /* Aquatic monsters suffocate if not in water */
    if (!square_iswater(c, grid) && rf_has(mon->race->flags, RF_AQUATIC)) return false;

    return true;
}


/*
 * Calls a monster from the level and moves it to the desired spot
 */
static int call_monster(struct chunk *c, struct loc *grid)
{
    int i, mon_count, choice;
    int *mon_indices;
    struct monster *mon;

    mon_count = 0;

    for (i = 1; i < cave_monster_max(c); i++)
    {
        mon = cave_monster(c, i);

        /* Figure out how many good monsters there are */
        if (can_call_monster(c, grid, mon)) mon_count++;
    }

    /* There were no good monsters on the level */
    if (mon_count == 0) return (0);

    /* Make the array */
    mon_indices = mem_zalloc(mon_count * sizeof(int));

    /* Reset mon_count */
    mon_count = 0;

    /* Now go through a second time and store the indices */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        mon = cave_monster(c, i);

        /* Save the values of the good monster */
        if (can_call_monster(c, grid, mon))
        {
            mon_indices[mon_count] = i;
            mon_count++;
        }
    }

    /* Pick one */
    choice = randint0(mon_count - 1);

    /* Get the lucky monster */
    mon = cave_monster(c, mon_indices[choice]);
    mem_free(mon_indices);

    /* Swap the monster */
    monster_swap(c, &mon->grid, grid);

    /* Wake it up, make it aware */
    monster_wake(NULL, mon, false, 100);
    mon_clear_timed(NULL, mon, MON_TMD_HOLD, MON_TMD_FLG_NOTIFY);

    /* Set it's energy to 0 */
    mon->energy = 0;

    return (mon->race->level);
}


/*
 * Places a monster (of the specified "type") near the given
 * location. Return the summoned monster's level if a monster was
 * actually summoned.
 *
 * We will attempt to place the monster up to 60 times before giving up.
 *
 * This function takes the "monster level"
 * of the summoning monster as a parameter, and use that, along with
 * the current dungeon level, to help determine the level of the
 * desired monster. Note that this is an upper bound, and also
 * tends to "prefer" monsters of that level. Currently, we use
 * the average of the dungeon and monster levels, and then add
 * five to allow slight increases in monster power.
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
int summon_specific(struct player *p, struct chunk *c, struct loc *grid, int lev, int type,
    bool delay, bool call, int chance, struct monster *summoner)
{
    struct monster *mon;
    struct monster_race *race;
    byte status = MSTATUS_HOSTILE, status_player = MSTATUS_SUMMONED;
    int summon_level = (monster_level(&p->wpos) + lev) / 2 + 5;
    struct loc nearby;
    struct monster_group_info info = {0, 0};

    /* Paranoia, make sure the level is allocated */
    if (!c) return 0;

    /* Forbid in towns */
    if (forbid_town(&c->wpos)) return 0;

    /* Look for a location, allow up to 4 squares away */
    if (!summon_location(c, &nearby, grid, 60)) return 0;

    /* Hack -- monster summoned by the player */
    if (chance) status = MSTATUS_SUMMONED;

    /* Hack -- try to "charm" the monster (friendly summon) */
    if (magik(chance))
    {
        /* A high level will help a lot */
        if (!CHANCE(MAX(summon_level - 5, 1), p->lev * 5)) status_player = 1 + randint1(2);

        /* A high wisdom will help a lot, and will always yield useful summons */
        if (can_charm_monster(p)) status_player = MSTATUS_ATTACK;
    }

    /* Save the "summon" type */
    summon_specific_type = type;

    /* Use the new calling scheme if requested */
    if (call && (type != summon_name_to_idx("UNIQUE")) && (type != summon_name_to_idx("WRAITH")))
        return call_monster(c, &nearby);

    /* Prepare allocation table */
    get_mon_num_prep(summon_specific_okay);

    /* Try to get a proper summon */
    while (1)
    {
        /* Pick a monster, using the level calculation */
        race = get_mon_num(c, summon_level, true);

        /* Handle failure */
        if (!race) break;

        /* Too enraged to be controlled */
        if (player_of_has(p, OF_AGGRAVATE))
        {
            status_player = MSTATUS_SUMMONED;
            break;
        }

        /* Uniques and breeders cannot be tamed */
        if ((status_player > MSTATUS_SUMMONED) &&
            (monster_is_unique(race) || rf_has(race->flags, RF_MULTIPLY)))
        {
            continue;
        }

        /* Useful summons should be "useful" (except specific summons) */
        if ((status_player == MSTATUS_ATTACK) && (type != summon_name_to_idx("JELLY")) &&
            (type != summon_name_to_idx("GOLEM")) && (type != summon_name_to_idx("VORTEX")) &&
            (type != summon_name_to_idx("HYDRA")) &&
            (rf_has(race->flags, RF_NEVER_BLOW) || rf_has(race->flags, RF_NEVER_MOVE) ||
            rf_has(race->flags, RF_RAND_25) || rf_has(race->flags, RF_RAND_50)))
        {
            continue;
        }

        /* Aquatic monsters suffocate if not in water */
        if (!square_iswater(c, &nearby) && rf_has(race->flags, RF_AQUATIC)) continue;

        /* Done */
        break;
    }

    /* Prepare allocation table */
    get_mon_num_prep(NULL);

    /* Handle failure */
    if (!race) return 0;

    /* Put summons in the group of any summoner */
    if (summoner)
    {
        struct monster_group *group = summon_group(c, summoner);

        info.index = group->index;
        info.role = MON_GROUP_SUMMON;
    }

    /* Attempt to place the monster (awake, don't allow groups) */
    if (!place_new_monster(p, c, &nearby, race, 0, &info, ORIGIN_DROP_SUMMON))
        return 0;

    /*
     * If delay, try to let the player act before the summoned monsters,
     * including holding faster monsters for the required number of turns
     */
    mon = square_monster(c, &nearby);
    if (delay)
    {
        int turns = (mon->mspeed + 9 - p->state.speed) / 10;

        mon->energy = 0;
        if (turns > 0)
        {
            /* Set timer directly to avoid resistance */
            mon->m_timed[MON_TMD_HOLD] = turns;
        }
    }

    /* Hack -- monster summoned by the player */
    if (status_player > MSTATUS_SUMMONED) monster_set_master(mon, p, status_player);
    else mon->status = status;

    /* Success, return the level of the monster */
    return mon->race->level;
}


/*
 * Summon a specific race near this location.
 * Summon until we can't find a location or we have summoned size...
 */
bool summon_specific_race_aux(struct player *p, struct chunk *c, struct loc *grid,
    struct monster_race *race, unsigned char size, bool pet)
{
    int n;
    struct monster_group_info info = {0, 0};

    /* Handle failure */
    if (!race) return false;

    /* Paranoia, make sure the level is allocated */
    if (!c) return false;

    /* Forbid in towns */
    if (forbid_town(&c->wpos)) return false;

    /* For each monster we are summoning */
    for (n = 0; n < size; n++)
    {
        struct loc new_grid;

        /* Look for a location */
        if (!summon_location(c, &new_grid, grid, 200)) return false;

        /* Aquatic monsters suffocate if not in water */
        if (!square_iswater(c, &new_grid) && rf_has(race->flags, RF_AQUATIC)) return false;

        /* Attempt to place the monster (awake, don't allow groups) */
        if (!place_new_monster(p, c, &new_grid, race, 0, &info, ORIGIN_DROP_SUMMON))
            return false;

        if (pet)
        {
            struct monster *mon = square_monster(c, &new_grid);

            monster_set_master(mon, p, MSTATUS_ATTACK);
        }
    }

    /* Success */
    return true;
}


bool summon_specific_race(struct player *p, struct chunk *c, struct loc *grid,
    struct monster_race *race, unsigned char size)
{
    return summon_specific_race_aux(p, c, grid, race, size, false);
}


/* Summon a specific race at a random location */
bool summon_specific_race_somewhere(struct player *p, struct chunk *c, struct monster_race *race,
    unsigned char size)
{
    struct loc grid;
    int tries = 50;

    /* Paranoia, make sure the level is allocated */
    if (!c) return false;

    /* Forbid in towns */
    if (forbid_town(&c->wpos)) return false;

    /* Find a legal, distant, unoccupied, space */
    while (--tries)
    {
        /* Pick a location */
        loc_init(&grid, randint0(c->width), randint0(c->height));

        /* Require "naked" floor grid */
        if (!square_isempty(c, &grid)) continue;

        /* We have a valid location */
        break;
    }

    /* Abort */
    if (!tries) return false;

    /* Attempt to place the monster */
    if (summon_specific_race(p, c, &grid, race, size)) return true;
    return false;
}


/*
 * This function is used when a group of monsters is summoned.
 */
int summon_monster_aux(struct player *p, struct chunk *c, struct loc *grid, int flag, int rlev,
    int max, int chance, struct monster *mon)
{
    int count = 0, val = 0, attempts = 0;
    int temp;
    int fallback_type = summon_fallback_type(flag);

    /* Continue summoning until we reach the current dungeon level */
    while ((val < p->wpos.depth * rlev) && (attempts < max))
    {
        /* Get a monster */
        temp = summon_specific(p, c, grid, rlev, flag, false, false, chance, mon);

        val += temp * temp;

        /* Increase the attempt, needed in case no monsters were available. */
        attempts++;

        /* Increase count of summoned monsters */
        if (val > 0) count++;
    }

    /* If the summon failed and there's a fallback type, use that */
    if ((count == 0) && (fallback_type >= 0))
        count = summon_monster_aux(p, c, grid, fallback_type, rlev, max, 0, mon);

    return count;
}


/*
 * Get a location for summoned monsters.
 */
bool summon_location(struct chunk *c, struct loc *place, struct loc *grid, int tries)
{
    int i;

    /* Look for a location */
    for (i = 0; i < tries; ++i)
    {
        /* Pick a distance */
        int d = (i / 15) + 1;

        /* Pick a location */
        if (!scatter(c, place, grid, d, true)) continue;

        /* Require "empty" floor grid */
        if (!square_isemptyfloor(c, place)) continue;

        /* No summon on glyphs */
        if (square_trap_flag(c, place, TRF_GLYPH)) continue;

        /* Okay */
        return true;
    }

    /* Failure */
    return false;
}


/*
 * Select a race for a monster shapechange from its possible summons
 */
struct monster_race *select_shape(struct player *p, struct monster *mon, int type)
{
    struct monster_race *race = NULL;
    struct chunk *c = chunk_get(&p->wpos);

    /* Save the "summon" type */
    summon_specific_type = type;

    /* Prepare allocation table */
    get_mon_num_prep(summon_specific_okay);

    /* Pick a monster */
    race = get_mon_num(c, p->wpos.depth + 5, true);

    /* Prepare allocation table */
    get_mon_num_prep(NULL);

    return race;
}
