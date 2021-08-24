/*
 * File: mon-summon.c
 * Purpose: Monster summoning
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


/*
 * The "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * The kin base for S_KIN
 */
struct monster_base *kin_base;


static struct summon_details
{
    const char *name;
    int message_type;
    bool unique_allowed;
    const char *base1;
    const char *base2;
    const char *base3;
    int race_flag;
    const char *description;
} summon_info[] =
{
    #define S(a, b, c, d, e, f, g, h) {#a, b, c, d, e, f, g, h},
    #include "list-summon-types.h"
    #undef S
    {"MAX", 0, false, NULL, NULL, NULL, 0, ""}
};


int summon_name_to_idx(const char *name)
{
    int i;

    for (i = 0; !streq(summon_info[i].name, "MAX"); i++)
    {
        if (streq(name, summon_info[i].name)) return i;
    }

    return -1;
}


const char *summon_desc(int type)
{
    if ((type < 0) || (type >= S_MAX)) return NULL;
    return summon_info[type].description;
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
    struct summon_details *info = &summon_info[summon_specific_type];
    bool unique = rf_has(race->flags, RF_UNIQUE);

    /* Forbid uniques? */
    if (!info->unique_allowed && unique) return false;

    /* A valid base and no match means disallowed */
    if (info->base1 && !match_monster_bases(race->base, info->base1, info->base2, info->base3, NULL))
        return false;

    /* A valid race flag and no match means disallowed */
    if (info->race_flag && !rf_has(race->flags, info->race_flag)) return false;

    /* Special case - summon kin */
    if (summon_specific_type == S_KIN)
        return (!unique && (race->base == kin_base));

    /* If we made it here, we're fine */
    return true;
}


/*
 * The message type for a particular summon
 */
int summon_message_type(int summon_type)
{
    return summon_info[summon_type].message_type;
}


/*
 * Check to see if you can call the monster
 */
static bool can_call_monster(struct chunk *c, int y, int x, struct monster *mon)
{
    int oy, ox;

    /* Skip dead monsters */
    if (!mon->race) return false;

    /* Only consider callable monsters */
    if (!summon_specific_okay(mon->race)) return false;

    /* Extract monster location */
    oy = mon->fy;
    ox = mon->fx;

    /* Make sure the summoned monster is not in LOS of the summoner */
    if (los(c, y, x, oy, ox)) return false;

    return true;
}


/*
 * Calls a monster from the level and moves it to the desired spot
 */
static int call_monster(struct chunk *c, int y, int x)
{
    int i, mon_count, choice;
    int oy, ox;
    int *mon_indices;
    struct monster *mon;

    mon_count = 0;

    for (i = 1; i < cave_monster_max(c); i++)
    {
        mon = cave_monster(c, i);

        /* Figure out how many good monsters there are */
        if (can_call_monster(c, y, x, mon)) mon_count++;
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
        if (can_call_monster(c, y, x, mon))
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

    /* Extract monster location */
    oy = mon->fy;
    ox = mon->fx;

    /* Swap the monster */
    monster_swap(c, oy, ox, y, x);

    /* Wake it up */
    mon_clear_timed(NULL, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, false);

    /* Set it's energy to 0 */
    mon->energy = 0;

    return (mon->race->level);
}


/*
 * Places a monster (of the specified "type") near the given
 * location. Return true if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 60 times before giving up.
 *
 * Note: S_UNIQUE and S_WRAITH will summon Uniques
 * Note: S_ANY, S_HI_UNDEAD, S_HI_DEMON and S_HI_DRAGON may summon Uniques
 * Note: None of the other summon codes will ever summon Uniques.
 *
 * This function has been changed. We now take the "monster level"
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
int summon_specific(struct player *p, struct chunk *c, int y1, int x1, int lev, int type,
    bool delay, bool call, int chance)
{
    int x = 0, y = 0;
    struct monster *mon;
    struct monster_race *race;
    byte status = MSTATUS_HOSTILE, status_player = MSTATUS_SUMMONED;
    int summon_level = (monster_level(p->depth) + monster_level(lev)) / 2 + 5;

    /* Paranoia, make sure the level is allocated */
    if (!c) return 0;

    /* Forbid in towns */
    if (forbid_town(c->depth)) return 0;

    /* Look for a location, allow up to 4 squares away */
    if (!summon_location(c, &y, &x, y1, x1, 60)) return 0;

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
    if (call && (type != S_UNIQUE) && (type != S_WRAITH))
        return call_monster(c, y, x);

    /* Prepare allocation table */
    get_mon_num_prep(summon_specific_okay);

    /* Try to get a proper summon */
    while (1)
    {
        /* Pick a monster, using the level calculation */
        race = get_mon_num(c, summon_level);

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
            (rf_has(race->flags, RF_UNIQUE) || rf_has(race->flags, RF_MULTIPLY)))
        {
            continue;
        }

        /* Useful summons should be "useful" (except specific summons) */
        if ((status_player == MSTATUS_ATTACK) && (type != S_JELLY) && (type != S_GOLEM) &&
            (type != S_VORTEX) && (type != S_HYDRA) &&
            (rf_has(race->flags, RF_NEVER_BLOW) || rf_has(race->flags, RF_NEVER_MOVE) ||
            rf_has(race->flags, RF_RAND_25) || rf_has(race->flags, RF_RAND_50)))
        {
            continue;
        }

        /* Done */
        break;
    }

    /* Prepare allocation table */
    get_mon_num_prep(NULL);

    /* Handle failure */
    if (!race) return 0;

    /* Attempt to place the monster (awake, don't allow groups) */
    if (!place_new_monster(p, c, y, x, race, 0, ORIGIN_DROP_SUMMON))
        return 0;

    /*
     * If delay, try to let the player act before the summoned monsters,
     * including slowing down faster monsters for one turn
     */
    mon = square_monster(c, y, x);
    if (delay)
    {
        mon->energy = 0;
        if (mon->mspeed > p->state.speed)
            mon_inc_timed(p, mon, MON_TMD_SLOW, 1, MON_TMD_FLG_NOMESSAGE, false);
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
bool summon_specific_race_aux(struct player *p, struct chunk *c, int y1, int x1,
    struct monster_race *race, unsigned char size, bool pet)
{
    int n, x, y;

    /* Handle failure */
    if (!race) return false;

    /* Paranoia, make sure the level is allocated */
    if (!c) return false;

    /* Forbid in towns */
    if (forbid_town(c->depth)) return false;

    /* For each monster we are summoning */
    for (n = 0; n < size; n++)
    {
        /* Look for a location */
        if (!summon_location(c, &y, &x, y1, x1, 200)) return false;

        /* Attempt to place the monster (awake, don't allow groups) */
        if (!place_new_monster(p, c, y, x, race, 0, ORIGIN_DROP_SUMMON))
            return false;

        if (pet)
        {
            struct monster *mon = square_monster(c, y, x);

            monster_set_master(mon, p, MSTATUS_ATTACK);
        }
    }

    /* Success */
    return true;
}


bool summon_specific_race(struct player *p, struct chunk *c, int y1, int x1,
    struct monster_race *race, unsigned char size)
{
    return summon_specific_race_aux(p, c, y1, x1, race, size, false);
}


/* Summon a specific race at a random location */
bool summon_specific_race_somewhere(struct player *p, struct chunk *c, struct monster_race *race,
    unsigned char size)
{
    int y, x;
    int tries = 50;

    /* Paranoia, make sure the level is allocated */
    if (!c) return false;

    /* Forbid in towns */
    if (forbid_town(c->depth)) return false;

    /* Find a legal, distant, unoccupied, space */
    while (--tries)
    {
        /* Pick a location */
        y = randint0(c->height);
        x = randint0(c->width);

        /* Require "naked" floor grid */
        if (!square_isempty(c, y, x)) continue;

        /* We have a valid location */
        break;
    }

    /* Abort */
    if (!tries) return false;

    /* Attempt to place the monster */
    if (summon_specific_race(p, c, y, x, race, size)) return true;
    return false;
}


/*
 * This function is used when a group of monsters is summoned.
 */
int summon_monster_aux(struct player *p, struct chunk *c, int y, int x, int flag, int rlev,
    int max, int chance)
{
    int count = 0, val = 0, attempts = 0;
    int temp;

    /* Continue summoning until we reach the current dungeon level */
    while ((val < p->depth * rlev) && (attempts < max))
    {
        /* Get a monster */
        temp = summon_specific(p, c, y, x, rlev, flag, false, false, chance);

        val += temp * temp;

        /* Increase the attempt, needed in case no monsters were available. */
        attempts++;

        /* Increase count of summoned monsters */
        if (val > 0) count++;
    }

    /*
     * In the special case that uniques or wraiths were summoned but all were
     * dead, S_HI_UNDEAD is used instead
     */
    if (!count && ((flag == S_WRAITH) || (flag == S_UNIQUE)))
        count = summon_monster_aux(p, c, y, x, S_HI_UNDEAD, rlev, max, 0);

    return count;
}


/*
 * Get a location for summoned monsters.
 */
bool summon_location(struct chunk *c, int *yp, int *xp, int y1, int x1, int tries)
{
    int i;

    /* Look for a location */
    for (i = 0; i < tries; ++i)
    {
        /* Pick a distance */
        int d = (i / 15) + 1;

        /* Pick a location */
        scatter(c, yp, xp, y1, x1, d, true);

        /* Require "empty" floor grid */
        if (!square_isemptyfloor(c, *yp, *xp)) continue;

        /* No summon on glyph of warding */
        if (square_iswarded(c, *yp, *xp)) continue;

        /* Okay */
        return true;
    }

    /* Failure */
    return false;
}
