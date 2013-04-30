/*
 * File: trap.c
 * Purpose: Trap triggering, selection, and placement
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


#include "s-angband.h"
#include "attack.h"
#include "effects.h"
#include "trap.h"


/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, always hit 12% of the time.
 * Otherwise, match trap power against player armor.
 */
int trap_check_hit(struct player *p, int power)
{
    return test_hit(power, p->state.ac + p->state.to_a, TRUE);
}


/*
 * Hack -- instantiate a trap
 *
 * XXX XXX XXX This routine should be redone to reflect trap "level".
 * That is, it does not make sense to have spiked pits at 50 feet.
 * Actually, it is not this routine, but the "trap instantiation"
 * code, which should also check for "trap doors" on quest levels.
 */
void pick_trap(int depth, int y, int x)
{
    int feat;

    static const int min_level[] =
    {
        2,  /* Trap door */
        2,  /* Open pit */
        2,  /* Spiked pit */
        2,  /* Poison pit */
        3,  /* Summoning rune */
        1,  /* Teleport rune */
        2,  /* Fire rune */
        2,  /* Acid rune */
        2,  /* Slow rune */
        6,  /* Strength dart */
        6,  /* Dexterity dart */
        6,  /* Constitution dart */
        2,  /* Gas blind */
        1,  /* Gas confuse */
        2,  /* Gas poison */
        2   /* Gas sleep */
    };

    /* Paranoia */
    if (!cave_issecrettrap(cave_get(depth), y, x)) return;

    /* Pick a trap */
    while (1)
    {
        /* Hack -- pick a trap */
        feat = FEAT_TRAP_HEAD + randint0(16);

        /* Check against minimum depth */
        if (min_level[feat - FEAT_TRAP_HEAD] > depth) continue;

        /* Hack -- no trap doors on the deepest level */
        if ((feat == FEAT_TRAP_HEAD + 0x00) && (depth == MAX_DEPTH - 1)) continue;

        /* Done */
        break;
    }

    /* Activate the trap */
    cave_set_feat(cave_get(depth), y, x, feat);
}


/*
 * Places a trap. All traps are untyped until discovered.
 */
void place_trap(struct cave *c, int y, int x)
{
    my_assert(cave_in_bounds(c, y, x));
    my_assert(cave_isempty(c, y, x));

    /* Place an invisible trap */
    cave_set_feat(c, y, x, FEAT_INVIS);
}


/*
 * Create a trap during play. All traps are untyped until discovered.
 */
void create_trap(struct cave *c, int y, int x)
{
    my_assert(cave_in_bounds(c, y, x));
    my_assert(cave_isopen(c, y, x));

    /* Place an invisible trap */
    cave_set_feat(c, y, x, FEAT_INVIS);
}


/*
 * Handle player hitting a real trap
 */
void hit_trap(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    bool ident;
    struct feature *trap = &f_info[cave_get(p_ptr->depth)->feat[p_ptr->py][p_ptr->px]];

    /* Ghosts and rogues ignore traps */
    if (p_ptr->ghost || p_ptr->timed[TMD_TRAPS]) return;

    /* Disturb the player */
    disturb(p_ptr, 0, 0);

    /* Run the effect */
    effect_do(p_ptr, trap->effect, &ident, FALSE, 0, 0, 0);
}