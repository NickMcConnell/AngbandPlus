/*
 * File: chest.c
 * Purpose: Encapsulation of chest-related functions
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 Peter Denison
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
#include "../monster/mon-util.h"


/*
 * Each chest has a certain set of traps, determined by pval
 * Each chest has a "pval" from 1 to the chest level (max 55)
 * If the "pval" is negative then the trap has been disarmed
 * The "pval" of a chest determines the quality of its treasure
 * Note that disarming a trap on a chest also removes the lock.
 */
static const byte chest_traps[64] =
{
    0,                  /* 0 == empty */
    (CHEST_POISON),
    (CHEST_LOSE_STR),
    (CHEST_LOSE_CON),
    (CHEST_LOSE_STR),
    (CHEST_LOSE_CON),           /* 5 == best small wooden */
    0,
    (CHEST_POISON),
    (CHEST_POISON),
    (CHEST_LOSE_STR),
    (CHEST_LOSE_CON),
    (CHEST_POISON),
    (CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_SUMMON),         /* 15 == best large wooden */
    0,
    (CHEST_LOSE_STR),
    (CHEST_LOSE_CON),
    (CHEST_PARALYZE),
    (CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_SUMMON),
    (CHEST_PARALYZE),
    (CHEST_LOSE_STR),
    (CHEST_LOSE_CON),
    (CHEST_EXPLODE),            /* 25 == best small iron */
    0,
    (CHEST_POISON | CHEST_LOSE_STR),
    (CHEST_POISON | CHEST_LOSE_CON),
    (CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_PARALYZE),
    (CHEST_POISON | CHEST_SUMMON),
    (CHEST_SUMMON),
    (CHEST_EXPLODE),
    (CHEST_EXPLODE | CHEST_SUMMON), /* 35 == best large iron */
    0,
    (CHEST_SUMMON),
    (CHEST_EXPLODE),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_POISON | CHEST_PARALYZE),
    (CHEST_EXPLODE),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_POISON | CHEST_PARALYZE),    /* 45 == best small steel */
    0,
    (CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_STR),
    (CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_CON),
    (CHEST_POISON | CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_POISON | CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_STR | CHEST_LOSE_CON),
    (CHEST_POISON | CHEST_PARALYZE),
    (CHEST_POISON | CHEST_PARALYZE),    /* 55 == best large steel */
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
    (CHEST_EXPLODE | CHEST_SUMMON),
};


byte chest_trap_type(const object_type *o_ptr)
{
    s16b trap_value = o_ptr->pval[DEFAULT_PVAL];

    if (trap_value >= 0) return chest_traps[trap_value];
    return chest_traps[0 - trap_value];
}


/*
 * Determine if a chest is trapped
 */
bool is_trapped_chest(const object_type *o_ptr)
{
    if (o_ptr->tval != TV_CHEST) return FALSE;

    /* Disarmed or opened chests are not trapped */
    if (o_ptr->pval[DEFAULT_PVAL] <= 0) return FALSE;

    /* Some chests simply don't have traps */
    return (chest_traps[o_ptr->pval[DEFAULT_PVAL]] != 0);
}


/*
 * Determine if a chest is locked or trapped
 */
bool is_locked_chest(const object_type *o_ptr)
{
    if (o_ptr->tval != TV_CHEST) return FALSE;

    /* Disarmed or opened chests are not locked */
    return (o_ptr->pval[DEFAULT_PVAL] > 0);
}


/*
 * Unlock a chest
 */
void unlock_chest(object_type *o_ptr)
{
    o_ptr->pval[DEFAULT_PVAL] = (0 - o_ptr->pval[DEFAULT_PVAL]);
}


/*
 * Determine if a grid contains a chest matching the query type
 */
s16b chest_check(struct player *p, int y, int x, enum chest_query check_type)
{
    s16b this_o_idx, next_o_idx = 0;

    /* Scan all objects in the grid */
    for (this_o_idx = cave_get(p->depth)->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Check for chests */
        switch (check_type)
        {
            case CHEST_ANY:
                if (o_ptr->tval == TV_CHEST) return this_o_idx;
                break;
            case CHEST_OPENABLE:
                if ((o_ptr->tval == TV_CHEST) && (o_ptr->pval[DEFAULT_PVAL] != 0)) return this_o_idx;
                break;
            case CHEST_TRAPPED:
                if (is_trapped_chest(o_ptr) && object_is_known(p, o_ptr)) return this_o_idx;
                break;
        }
    }

    /* No chest */
    return (0);
}


/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
int count_chests(struct player *p, int *y, int *x, enum chest_query check_type)
{
    int d, count, o_idx;

    /* Count how many matches */
    count = 0;

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = p->py + ddy_ddd[d];
        int xx = p->px + ddx_ddd[d];

        /* No (visible) chest is there */
        if ((o_idx = chest_check(p, yy, xx, check_type)) == 0) continue;

        /* Count it */
        ++count;

        /* Remember the location of the last chest found */
        *y = yy;
        *x = xx;
    }

    /* All done */
    return count;
}


/*
 * Allocates objects upon opening a chest
 *
 * Disperse treasures from the chest "o_ptr", centered at (x,y).
 *
 * Small chests often contain "gold", while Large chests always contain
 * items.  Wooden chests contain 2 items, Iron chests contain 4 items,
 * and Steel chests contain 6 items.  The "value" of the items in a
 * chest is based on the level on which the chest is generated.
 */
static void chest_death(struct player *p, int y, int x, s16b o_idx)
{
    int number, value;
    bool tiny;
    object_type *o_ptr;
    object_type *i_ptr;
    object_type object_type_body;

    /* Get the chest */
    o_ptr = object_byid(o_idx);

    /* Small chests often hold "gold" */
    tiny = (o_ptr->sval < SV_CHEST_MIN_LARGE);

    /* Determine how much to drop (see above) */
    number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

    /* Zero pval means empty chest */
    if (!o_ptr->pval[DEFAULT_PVAL]) number = 0;

    /* Determine the "value" of the items */
    value = o_ptr->origin_depth - 10 + 2 * o_ptr->sval;
    if (value < 1) value = 1;

    /* Drop some objects (non-chests) */
    for (; number > 0; --number)
    {
        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        object_wipe(i_ptr);

        /* Small chests often drop gold */
        if (tiny && magik(75))
            make_gold(p, i_ptr, value, SV_GOLD_ANY);

        /* Otherwise drop an item */
        else
        {
            if (!make_object(p, cave_get(p->depth), i_ptr, value, FALSE, FALSE, NULL))
                continue;
            if (i_ptr->tval == TV_CHEST) continue;
        }

        /* Record origin */
        set_origin(i_ptr, ORIGIN_CHEST, o_ptr->origin_depth, 0);

        /* Drop it in the dungeon */
        drop_near(p, cave_get(p->depth), i_ptr, 0, y, x, TRUE);
    }

    /* Empty */
    o_ptr->pval[DEFAULT_PVAL] = 0;

    /* Known */
    object_notice_everything(p, o_ptr, TRUE);
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(struct player *p, int y, int x, s16b o_idx)
{
    int i, trap;
    object_type *o_ptr = object_byid(o_idx);
    const char *pself = player_self(p);

    /* Ignore disarmed chests */
    if (o_ptr->pval[DEFAULT_PVAL] <= 0) return;

    /* Obtain the traps */
    trap = chest_traps[o_ptr->pval[DEFAULT_PVAL]];

    /* Lose strength */
    if (trap & CHEST_LOSE_STR)
    {
        msg(p, "A small needle has pricked you!");
        strnfmt(p->died_flavor, sizeof(p->died_flavor), "pricked %s on a weakening needle", pself);
        if (!take_hit(p, damroll(1, 4), "a poison needle", FALSE)) do_dec_stat(p, A_STR, FALSE);
    }

    /* Lose constitution */
    if (trap & CHEST_LOSE_CON)
    {
        msg(p, "A small needle has pricked you!");
        strnfmt(p->died_flavor, sizeof(p->died_flavor), "pricked %s on an exhausting needle", pself);
        if (!take_hit(p, damroll(1, 4), "a poison needle", FALSE)) do_dec_stat(p, A_CON, FALSE);
    }

    /* Poison */
    if (trap & CHEST_POISON)
    {
        msg(p, "A puff of green gas surrounds you!");
        player_inc_timed(p, TMD_POISONED, 10 + randint1(20), TRUE, TRUE);
    }

    /* Paralyze */
    if (trap & CHEST_PARALYZE)
    {
        msg(p, "A puff of yellow gas surrounds you!");
        player_inc_timed(p, TMD_PARALYZED, 10 + randint1(20), TRUE, TRUE);
    }

    /* Summon monsters */
    if (trap & CHEST_SUMMON)
    {
        int num = 2 + randint1(3);

        msgt(p, MSG_SUM_MONSTER, "You are enveloped in a cloud of smoke!");
        if (check_antisummon(p, NULL)) num = 0;
        for (i = 0; i < num; i++)
            summon_specific(p, y, x, p->depth, 0, 1, 0);
    }

    /* Explode */
    if (trap & CHEST_EXPLODE)
    {
        msg(p, "There is a sudden explosion!");
        msg(p, "Everything inside the chest is destroyed!");
        o_ptr->pval[DEFAULT_PVAL] = 0;
        my_strcpy(p->died_flavor, "was torn apart by an exploding chest", sizeof(p->died_flavor));
        take_hit(p, damroll(5, 8), "an exploding chest", FALSE);
    }
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
bool do_cmd_open_chest(struct player *p, int y, int x, s16b o_idx)
{
    int i, j;
    bool flag = TRUE;
    bool more = FALSE;
    object_type *o_ptr = object_byid(o_idx);

    /* Attempt to unlock it */
    if (o_ptr->pval[DEFAULT_PVAL] > 0)
    {
        /* Assume locked, and thus not open */
        flag = FALSE;

        /* Get the "disarm" factor */
        i = p->state.skills[SKILL_DISARM];

        /* Penalize some conditions */
        if (p->timed[TMD_BLIND] || no_light(p)) i = i / 10;
        if (p->timed[TMD_CONFUSED] || p->timed[TMD_IMAGE]) i = i / 10;

        /* Extract the difficulty */
        j = i - o_ptr->pval[DEFAULT_PVAL];

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success -- May still have traps */
        if (magik(j))
        {
            msgt(p, MSG_LOCKPICK, "You have picked the lock.");
            player_exp_gain(p, 1);
            flag = TRUE;
        }

        /* Failure -- Keep trying */
        else
        {
            /* We may continue repeating */
            more = TRUE;
            msgt(p, MSG_LOCKPICK_FAIL, "You failed to pick the lock.");
        }
    }

    /* Allowed to open */
    if (flag)
    {
        /* Apply chest traps, if any */
        chest_trap(p, y, x, o_idx);

        /* Let the Chest drop items */
        chest_death(p, y, x, o_idx);

        /* Squelch chest if autosquelch calls for it */
        p->notice |= PN_SQUELCH;
    }

    /* Auto-squelch dead chests */
    if (o_ptr->pval[DEFAULT_PVAL] == 0) o_ptr->ignore = TRUE;

    /* Redraw chest, to be on the safe side (it may have been squelched) */
    cave_light_spot(cave_get(p->depth), y, x);

    /* Result */
    return (more);
}


/*
 * Attempt to disarm the chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
bool do_cmd_disarm_chest(struct player *p, int y, int x, s16b o_idx)
{
    int i, j;
    bool more = FALSE;
    object_type *o_ptr = object_byid(o_idx);

    /* Get the "disarm" factor */
    i = p->state.skills[SKILL_DISARM];

    /* Penalize some conditions */
    if (p->timed[TMD_BLIND] || no_light(p)) i = i / 10;
    if (p->timed[TMD_CONFUSED] || p->timed[TMD_IMAGE]) i = i / 10;

    /* Extract the difficulty */
    j = i - o_ptr->pval[DEFAULT_PVAL];

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Must find the trap first. */
    if (!object_is_known(p, o_ptr))
        msg(p, "I don't see any traps.");

    /* Already disarmed/unlocked or no traps */
    else if (!is_trapped_chest(o_ptr))
        msg(p, "The chest is not trapped.");

    /* Success (get a lot of experience) */
    else if (magik(j))
    {
        msgt(p, MSG_DISARM, "You have disarmed the chest.");
        player_exp_gain(p, o_ptr->pval[DEFAULT_PVAL]);
        o_ptr->pval[DEFAULT_PVAL] = (0 - o_ptr->pval[DEFAULT_PVAL]);
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && !CHANCE(5, i))
    {
        /* We may keep trying */
        more = TRUE;
        msg(p, "You failed to disarm the chest.");
    }

    /* Failure -- Set off the trap */
    else
    {
        msg(p, "You set off a trap!");
        chest_trap(p, y, x, o_idx);
    }

    /* Result */
    return (more);
}
