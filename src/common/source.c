/*
 * File: source.c
 * Purpose: Type that allows various different origins for an effect
 *
 * Copyright (c) 2016 Andi Sidwell
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


#include "angband.h"


void source_trap(struct source *source, struct trap *trap)
{
    source->idx = 0;
    source->player = NULL;
    source->monster = NULL;
    source->trap = trap;
    source->target = NULL;
}


void source_monster(struct source *source, struct monster *monster)
{
    source->idx = monster->midx;
    source->player = NULL;
    source->monster = monster;
    source->trap = NULL;
    source->target = NULL;
}


void source_player(struct source *source, int idx, struct player *player)
{
    source->idx = idx;
    source->player = player;
    source->monster = NULL;
    source->trap = NULL;
    source->target = NULL;
}


void source_both(struct source *source, struct player *player, struct monster *monster)
{
    source->idx = 0;
    source->player = player;
    source->monster = monster;
    source->trap = NULL;
    source->target = NULL;
}


bool source_null(struct source *source)
{
    return ((source == NULL) ||
        ((source->player == NULL) && (source->monster == NULL) && (source->trap == NULL) &&
        (source->target == NULL)));
}


/*
 * Compares source1 and source2 strictly
 */
bool source_equal(struct source *source1, struct source *source2)
{
    return ((source1->player == source2->player) && (source1->monster == source2->monster) &&
        (source1->trap == source2->trap) && (source1->target == source2->target));
}


/*
 * Compares source1 and source2.
 *
 * source1 contains either a player or a monster
 * source2 can contain both a player and a monster
 */
bool source_equal_player_or_monster(struct source *source1, struct source *source2)
{
    if (source1->player && (source1->player == source2->player)) return true;
    if (source1->monster && (source1->monster == source2->monster)) return true;
    return false;
}
