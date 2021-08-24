/*
 * File: trap.c
 * Purpose: The trap layer - player traps, runes and door locks
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
 * Is there a specific kind of trap in this square?
 */
bool square_trap_specific(struct chunk *c, struct loc *grid, unsigned int tidx)
{
    struct trap *trap = square_trap(c, grid);
	
    /* First, check the trap marker */
    if (!square_istrap(c, grid)) return false;

    /* Scan the square trap list */
    while (trap)
    {
        /* We found a trap of the right kind */
        if (trap->kind->tidx == tidx) return true;
        trap = trap->next;
    }

    /* Report failure */
    return false;
}


/*
 * Is there a trap with a given flag in this square?
 */
bool square_trap_flag(struct chunk *c, struct loc *grid, int flag)
{
    struct trap *trap = square_trap(c, grid);

    /* First, check the trap marker */
    if (!square_istrap(c, grid)) return false;

    /* Scan the square trap list */
    while (trap)
    {
        /* We found a trap with the right flag */
        if (trf_has(trap->flags, flag)) return true;
        trap = trap->next;
    }

    /* Report failure */
    return false;
}


/*
 * Determine if a trap actually exists in this square.
 *
 * Called with vis = 0 to accept any trap, = 1 to accept only visible
 * traps, and = -1 to accept only invisible traps.
 *
 * Clear the SQUARE_TRAP flag if none exist.
 */
static bool square_verify_trap(struct chunk *c, struct loc *grid, int vis)
{
    bool trap_exists = false;
    struct trap *trap = square_trap(c, grid);

    /* Scan the square trap list */
    while (trap)
    {
		/* Accept any trap */
        if (!vis) return true;

        /* Accept traps that match visibility requirements */
        if ((vis == 1) && trf_has(trap->flags, TRF_VISIBLE))
            return true;

        if ((vis == -1) && !trf_has(trap->flags, TRF_VISIBLE))
            return true;

        /* Note that a trap does exist */
        trap_exists = true;
        trap = trap->next;
    }

    /* No traps in this location. */
    if (!trap_exists)
    {
		/* No traps */
		sqinfo_off(square(c, grid)->info, SQUARE_TRAP);

		/* Take note */
		square_note_spot(c, grid);
    }
    
    /* Report failure */
    return false;
}


/*
 * General trap routines
 */


/*
 * Free memory for all traps on a grid
 */
void square_free_trap(struct chunk *c, struct loc *grid)
{
    struct trap *next, *trap = square_trap(c, grid);

    while (trap)
    {
        next = trap->next;
        mem_free(trap);
        trap = next;
    }
}


/*
 * Remove all traps from a grid.
 *
 * Return true if traps were removed.
 */
bool square_remove_all_traps(struct chunk *c, struct loc *grid)
{
    struct trap *trap;
    bool were_there_traps;

    my_assert(square_in_bounds(c, grid));

    trap = square(c, grid)->trap;
    were_there_traps = ((trap == NULL)? false: true);

    while (trap)
    {
        struct trap *next_trap = trap->next;

        mem_free(trap);
        trap = next_trap;
    }

    square_set_trap(c, grid, NULL);

    /* Refresh grids that the character can see */
    square_light_spot(c, grid);

    square_verify_trap(c, grid, 0);

    return were_there_traps;
}


/*
 * Remove all traps with the given index.
 *
 * Return true if traps were removed.
 */
bool square_remove_trap(struct chunk *c, struct loc *grid, unsigned int t_idx_remove)
{
    bool removed = false;
    struct trap *prev_trap = NULL;
    struct trap *trap;

    /* Bounds check */
    my_assert(square_in_bounds(c, grid));

    /* Look at the traps in this grid */
    trap = square(c, grid)->trap;
    while (trap)
    {
        struct trap *next_trap = trap->next;

        if (t_idx_remove == trap->kind->tidx)
        {
            mem_free(trap);
            removed = true;

            if (prev_trap) prev_trap->next = next_trap;
            else square_set_trap(c, grid, next_trap);

            break;
        }

        prev_trap = trap;
        trap = next_trap;
    }

    /* Refresh grids that the character can see */
    square_light_spot(c, grid);

    square_verify_trap(c, grid, 0);

    return removed;
}


/*
 * Player traps
 */


/*
 * Determine if a cave grid is allowed to have player traps in it.
 */
bool square_player_trap_allowed(struct chunk *c, struct loc *grid)
{
    /*
     * We currently forbid multiple traps in a grid under normal conditions.
     * If this changes, various bits of code elsewhere will have to change too.
     */
    if (square_istrap(c, grid)) return false;

    /* We currently forbid traps in a grid with objects. */
    if (square_object(c, grid)) return false;

    /* Check it's a trappable square */
    return (square_istrappable(c, grid));
}


/*
 * Instantiate a player trap
 */
static int pick_trap(int feat, int trap_level, int max_depth)
{
    int i, pick;
    int *trap_probs = NULL;
    int trap_prob_max = 0;

    /* Paranoia */
    if (!feat_is_trap_holding(feat))
		return -1;

    /* Get trap probabilities */
    trap_probs = mem_zalloc(z_info->trap_max * sizeof(int));
    for (i = 0; i < z_info->trap_max; i++)
    {
        /* Get this trap */
        struct trap_kind *kind = &trap_info[i];

        trap_probs[i] = trap_prob_max;

        /* Ensure that this is a valid player trap */
        if (!kind->name) continue;
        if (!kind->rarity) continue;
        if (!trf_has(kind->flags, TRF_TRAP)) continue;

        /* Require that trap_level not be too low */
		if (kind->min_depth > trap_level) continue;

		/* Floor? */
		if (feat_is_floor(feat) && !trf_has(kind->flags, TRF_FLOOR))
			continue;

		/* Check legality of trapdoors. */
		if (trf_has(kind->flags, TRF_DOWN))
	    {
			/* No trap doors on the deepest level */
			if (trap_level == max_depth - 1) continue;
	    }

        /* Trap is okay, store the cumulative probability */
        trap_probs[i] += (100 / kind->rarity);
        trap_prob_max = trap_probs[i];
    }

    /* No valid trap */
    if (trap_prob_max == 0)
    {
        mem_free(trap_probs);
        return -1;
    }

    /* Pick at random. */
    pick = randint0(trap_prob_max);
    for (i = 0; i < z_info->trap_max; i++)
    {
        if (pick < trap_probs[i]) break;
    }

    mem_free(trap_probs);

    /* Return our chosen trap */
    return ((i < z_info->trap_max)? i: -1);
}


/*
 * Make a new trap of the given type. Return true if successful.
 *
 * We choose a player trap at random if the index is not legal.
 *
 * This should be the only function that places traps in the dungeon
 * except the savefile loading code.
 */
void place_trap(struct chunk *c, struct loc *grid, int tidx, int trap_level)
{
    struct trap *new_trap;

    /* We've been called with an illegal index; choose a random trap */
    if ((tidx <= 0) || (tidx >= z_info->trap_max))
    {
        /* Require the correct terrain */
        if (!square_player_trap_allowed(c, grid)) return;

        tidx = pick_trap(square(c, grid)->feat, trap_level,
            get_wt_info_at(&c->wpos.grid)->max_depth);
    }

    /* Failure */
    if (tidx < 0) return;

    /* Allocate a new trap for this grid (at the front of the list) */
    new_trap = mem_zalloc(sizeof(*new_trap));
    new_trap->next = square_trap(c, grid);
    square_set_trap(c, grid, new_trap);

    /* Set the details */
    new_trap->kind = &trap_info[tidx];
    loc_copy(&new_trap->grid, grid);
    new_trap->power = randcalc(new_trap->kind->power, trap_level, RANDOMISE);
    trf_copy(new_trap->flags, trap_info[tidx].flags);

    /* Toggle on the trap marker */
    sqinfo_on(square(c, grid)->info, SQUARE_TRAP);

    /* Redraw the grid */
    square_light_spot(c, grid);
}


/*
 * Reveal some of the player traps in a square
 */
bool square_reveal_trap(struct player *p, struct loc *grid, bool always, bool domsg)
{
    int found_trap = 0;
    struct chunk *c = chunk_get(&p->wpos);
    struct trap *trap = square_trap(c, grid);

    /* Check there is a player trap */
    if (!square_isplayertrap(c, grid)) return false;

    /* Scan the grid */
    while (trap)
    {
		/* Skip non-player traps */
        if (!trf_has(trap->flags, TRF_TRAP))
        {
            trap = trap->next;
            continue;
        }

        /* Skip traps the player doesn't notice */
        if (!always && (p->state.skills[SKILL_SEARCH] < trap->power))
        {
            trap = trap->next;
            continue;
        }

        /* See the trap */
        trf_on(trap->flags, TRF_VISIBLE);

        /* We found a trap */
        found_trap++;

        trap = trap->next;
    }

    /* We found at least one trap */
    if (found_trap)
    {
		/* We want to talk about it */
		if (domsg)
		{
			if (found_trap == 1) msg(p, "You have found a trap.");
			else msg(p, "You have found %d traps.", found_trap);
		}

		/* Memorize */
        square_memorize(p, c, grid);
        square_memorize_trap(p, c, grid);

		/* Redraw */
		square_light_spot(c, grid);
    }
    
    /* Return true if we found any traps */
    return (found_trap != 0);
}


/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, always hit 12% of the time.
 * Otherwise, match trap power against player armor.
 */
bool trap_check_hit(struct player *p, int power)
{
    return test_hit(power, p->state.ac + p->state.to_a, true);
}


void trap_msg_death(struct player *p, struct trap *trap, char *msg, int len)
{
    if (trap->kind->msg_death)
    {
        switch (trap->kind->msg_death_type)
        {
            case 0:
            {
                my_strcpy(msg, trap->kind->msg_death, len);
                break;
            }
            case 1:
            {
                const char *poss = player_poss(p);

                strnfmt(msg, len, trap->kind->msg_death, poss);
                break;
            }
            case 2:
            {
                const char *pself = player_self(p);

                strnfmt(msg, len, trap->kind->msg_death, pself);
                break;
            }
        }
    }
    else
    {
        char *article = (is_a_vowel(trap->kind->desc[0])? "an ": "a ");

        strnfmt(msg, len, "was killed by %s%s", article, trap->kind->desc);
    }
}


/*
 * Hit a trap
 */
void hit_trap(struct player *p, struct loc *grid, int delayed)
{
    bool ident = false;
    struct trap *trap;
    struct effect *effect;
    struct chunk *c = chunk_get(&p->wpos);
    int target_depth = dungeon_get_next_level(p, p->wpos.depth, 1);
    struct worldpos wpos;

    /* The player is safe from all traps */
    if (p->ghost || player_is_trapsafe(p)) return;

    wpos_init(&wpos, &p->wpos.grid, target_depth);
    
    /* Look at the traps in this grid */
    for (trap = square_trap(c, grid); trap; trap = trap->next)
    {
		int flag;
        bool saved = false, valid = true;

        /* Require that trap be capable of affecting the character */
        if (!trf_has(trap->kind->flags, TRF_TRAP)) continue;
        if (trap->timeout) continue;

        if ((delayed != trf_has(trap->kind->flags, TRF_DELAY)) && (delayed != -1)) continue;

        /* Disturb the player */
        disturb(p);

        if (trf_has(trap->kind->flags, TRF_DOWN))
        {
            /* Verify basic quests */
            if (is_quest_active(p, p->wpos.depth))
            {
                msg(p, "You feel quite certain something really awful just happened...");
                valid = false;
            }

            /* Hack -- DM redesigning the level */
            if (chunk_inhibit_players(&wpos))
            {
                msg(p, "You feel quite certain something really awful just happened...");
                valid = false;
            }
        }

        if (valid)
        {
            /* Give a message */
            if (trap->kind->msg) msg(p, trap->kind->msg);

            /* Test for save due to flag */
            for (flag = of_next(trap->kind->save_flags, FLAG_START); flag != FLAG_END;
                flag = of_next(trap->kind->save_flags, flag + 1))
            {
                equip_learn_flag(p, flag);
                if (player_of_has(p, flag)) saved = true;
            }

            /* Test for save due to armor */
            if (trf_has(trap->kind->flags, TRF_SAVE_ARMOR) && !trap_check_hit(p, 125))
                saved = true;

            /* Test for save due to saving throw */
            if (trf_has(trap->kind->flags, TRF_SAVE_THROW) && magik(p->state.skills[SKILL_SAVE]))
                saved = true;

            /* Save, or fire off the trap */
            if (saved)
            {
                if (trap->kind->msg_good) msg(p, trap->kind->msg_good);
            }
            else
            {
                struct source who_body;
                struct source *who = &who_body;

                source_player(who, get_player_index(get_connection(p->conn)), p);
                who->trap = trap;

                if (trap->kind->msg_bad) msg(p, trap->kind->msg_bad);

                effect = trap->kind->effect;
                effect_do(effect, who, &ident, false, 0, NULL, 0, 0, NULL);

                /* Trap may have gone */
                if (!square_trap(c, grid)) break;

                /* Do any extra effects */
                if (trap->kind->effect_xtra && one_in_(2))
                {
                    if (trap->kind->msg_xtra) msg(p, trap->kind->msg_xtra);
                    effect = trap->kind->effect_xtra;
                    effect_do(effect, who, &ident, false, 0, NULL, 0, 0, NULL);

                    /* Trap may have gone */
                    if (!square_trap(c, grid)) break;
                }
            }

            /* Some traps drop you a dungeon level */
            if (trf_has(trap->kind->flags, TRF_DOWN))
                dungeon_change_level(p, c, &wpos, LEVEL_RAND);

            /* Some traps drop you onto them */
            if (trf_has(trap->kind->flags, TRF_PIT))
                monster_swap(c, &p->grid, &trap->grid);

            /* Some traps disappear after activating, all have a chance to */
            if (trf_has(trap->kind->flags, TRF_ONETIME) || one_in_(3))
                square_destroy_trap(c, grid);
        }

        /* Trap may have gone */
        if (!square_trap(c, grid)) break;

        /* Trap becomes visible (always XXX) */
        trf_on(trap->flags, TRF_VISIBLE);
        square_memorize(p, c, grid);
        square_memorize_trap(p, c, grid);
    }

    /* Verify traps (remove marker if appropriate) */
    square_verify_trap(c, grid, 0);
}


/*
 * Disable a trap for the specified number of turns
 */
bool square_set_trap_timeout(struct player *p, struct chunk *c, struct loc *grid, bool domsg,
    unsigned int tidx, int time)
{
    bool trap_exists;
    struct trap *current_trap = NULL;

    /* Bounds check */
    my_assert(square_in_bounds(c, grid));

    /* Look at the traps in this grid */
    current_trap = square(c, grid)->trap;
    while (current_trap)
    {
        /* Get the next trap (may be NULL) */
        struct trap *next_trap = current_trap->next;

        /* If called with a specific index, skip others */
        if ((tidx > 0) && (tidx != current_trap->kind->tidx))
        {
            if (!next_trap) break;
            current_trap = next_trap;
            continue;
        }

        /* Set the timer */
        current_trap->timeout = time;

        /* Message if requested */
        if (p && domsg)
            msg(p, "You have disabled the %s.", current_trap->kind->name);

        /* Replace with the next trap */
        current_trap = next_trap;
    }

    /* Refresh grids that the character can see */
    square_light_spot(c, grid);

    /* Verify traps (remove marker if appropriate) */
    trap_exists = square_verify_trap(c, grid, 0);
    
    /* Report whether any traps exist in this grid */
    return (!trap_exists);
}


/*
 * Give the remaining time for a trap to be disabled; note it chooses the first
 * appropriate trap on the grid
 */
int square_trap_timeout(struct chunk *c, struct loc *grid, unsigned int tidx)
{
    struct trap *current_trap = square(c, grid)->trap;

    while (current_trap)
    {
        /* Get the next trap (may be NULL) */
        struct trap *next_trap = current_trap->next;

        /* If called with a specific index, skip others */
        if ((tidx > 0) && (tidx != current_trap->kind->tidx))
        {
            if (!next_trap) break;
            current_trap = next_trap;
            continue;
        }

        /* If the timer is set, return the value */
        if (current_trap->timeout) return current_trap->timeout;

        /* Replace with the next trap */
        current_trap = next_trap;
    }

    return 0;
}


/*
 * Door locks
 */


/*
 * Lock a closed door to a given power
 */
void square_set_door_lock(struct chunk *c, struct loc *grid, int power)
{
    struct trap_kind *lock = lookup_trap("door lock");
    struct trap *trap;

    /* Verify it's a closed door */
    if (!square_iscloseddoor(c, grid)) return;

    /* If there's no lock there, add one */
    if (!square_trap_specific(c, grid, lock->tidx))
        place_trap(c, grid, lock->tidx, 0);

    /* Set the power (of all locks - there should be only one) */
    trap = square_trap(c, grid);
    while (trap)
    {
        if (trap->kind == lock) trap->power = power;
        trap = trap->next;
    }
}


/*
 * Return the power of the lock on a door
 */
int square_door_power(struct chunk *c, struct loc *grid)
{
    struct trap_kind *lock = lookup_trap("door lock");
    struct trap *trap;

    /* Verify it's a closed door */
    if (!square_iscloseddoor(c, grid)) return 0;

    /* If there's no lock there, add one */
    if (!square_trap_specific(c, grid, lock->tidx)) return 0;

    /* Get the power and return it */
    trap = square_trap(c, grid);
    while (trap)
    {
        if (trap->kind == lock) return trap->power;
        trap = trap->next;
    }

    return 0;
}