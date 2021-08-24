/*
 * File: project-mon.c
 * Purpose: Projection effects on monsters
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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


#include "s-angband.h"


/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * Note that this function is one of the more "dangerous" ones...
 */
static struct monster_race *poly_race(struct chunk *c, struct monster_race *race)
{
    int i, minlvl, maxlvl, goal;

    my_assert(race && race->name);

    /* Uniques never polymorph */
    if (monster_is_unique(race)) return race;

    /* Allowable range of "levels" for resulting monster */
    goal = (c->wpos.depth + race->level) / 2 + 5;
    minlvl = MIN(race->level - 10, (race->level * 3) / 4);
    maxlvl = MAX(race->level + 10, (race->level * 5) / 4);

    /* Small chance to allow something really strong */
    if (one_in_(100)) maxlvl = 100;

    /* Try to pick a new, non-unique race within our level range */
    for (i = 0; i < 1000; i++)
    {
        struct monster_race *new_race = get_mon_num(c, goal, false);

        if (!new_race || (new_race == race)) continue;
        if (monster_is_unique(new_race)) continue;
        if ((new_race->level < minlvl) || (new_race->level > maxlvl)) continue;

        /* Avoid force-depth monsters, since it might cause a crash in project_m() */
        /* Paranoia: should never happen (see get_mon_num) */
        if (rf_has(new_race->flags, RF_FORCE_DEPTH) && (c->wpos.depth < new_race->level)) continue;

        return new_race;
    }

    /* If we get here, we weren't able to find a new race. */
    return race;
}


/*
 * Thrust the player or a monster away from the source of a projection.
 *
 * PWMAngband: "centre" contains the coordinates of the caster
 *
 * PWMAngband: completely rewritten using project_path() and stopping at the first monster,
 *             player or wall
 */
void thrust_away(struct chunk *c, struct source *origin, struct loc *centre, int grids_away)
{
    int i;
    int path_n;
    struct loc path_g[256];
    bool skip = true, moved = false;
    struct loc grid, target;

    /* Target */
    if (origin->monster)
        loc_copy(&target, &origin->monster->grid);
    else
    {
        struct trap *trap = origin->trap;

        loc_copy(&target, &origin->player->grid);

        /* Player gets pushed in a random direction if on the trap */
        if (trap)
        {
            int d = randint0(8);

            target.y += ddy_ddd[d];
            target.x += ddx_ddd[d];
        }
    }

    /* Nothing to do if the target is already in a wall */
    if (!square_isprojectable(c, &target))
    {
        sqinfo_off(square(c, &target)->info, SQUARE_PROJECT);
        return;
    }

    /* Start at the target grid. */
    loc_copy(&grid, &target);

    /* Calculate the path */
    path_n = project_path(NULL, path_g, z_info->max_range, c, centre, &target, PROJECT_THRU);

    /* Project along the path */
    for (i = 0; i < path_n; i++)
    {
        /* Skip target, start processing afterwards */
        if (loc_eq(&path_g[i], &target))
        {
            skip = false;
            continue;
        }

        /* Skip grids up to target */
        if (skip) continue;

        /* Stop before hitting unpassable terrain */
        if (!square_ispassable(c, &path_g[i])) break;
        if (!square_isprojectable(c, &path_g[i])) break;

        /* Stop before hitting a monster or player */
        if (square(c, &path_g[i])->mon) break;

        /* Jump to new location. */
        loc_copy(&grid, &path_g[i]);
        moved = true;

        /* We can't travel any more. */
        grids_away--;
        if (!grids_away) break;
    }

    /* Move target */
    if (moved)
    {
        monster_swap(c, &target, &grid);
        if (!origin->monster) player_know_floor(origin->player, c);
    }

    /* Some special messages or effects for player or monster. */
    if (square_isfiery(c, &grid))
    {
        if (!origin->monster && !player_passwall(origin->player))
            msg(origin->player, "You are thrown into molten lava!");
    }

    /* Clear the projection mark. */
    sqinfo_off(square(c, &grid)->info, SQUARE_PROJECT);
}


/*
 * Monster handlers
 */


typedef struct project_monster_handler_context_s
{
    struct source *origin;
    int r;
    struct chunk *cave;
    struct loc grid;
    int dam;
    int type;
    bool seen;
    struct monster *mon;
    struct monster_lore *lore;
    bool charm;
    bool do_gravity;
    bool do_delete;
    bool obvious;
    bool skipped;
    u16b flag;
    int do_poly;
    int teleport_distance;
    enum mon_messages hurt_msg;
    enum mon_messages die_msg;
    int mon_timed[MON_TMD_MAX];
} project_monster_handler_context_t;


typedef void (*project_monster_handler_f)(project_monster_handler_context_t *);


static int adjust_radius(project_monster_handler_context_t *context, int amount)
{
    return (amount + context->r) / (context->r + 1);
}


/*
 * Resist an attack if the monster has the given elemental flag.
 *
 * If the effect is seen, we learn that the monster has a given flag.
 * Resistance is divided by the factor.
 *
 * context is the project_m context.
 * flag is the RF_ flag that the monster must have.
 * factor is the divisor for the base damage.
 */
static void project_monster_resist_element(project_monster_handler_context_t *context, int flag,
    int factor)
{
    if (context->seen) rf_on(context->lore->flags, flag);
    if (rf_has(context->mon->race->flags, flag))
    {
        context->hurt_msg = MON_MSG_RESIST_A_LOT;
        context->dam /= factor;
    }
}


/*
 * Resist an attack if the monster has the given flag.
 *
 * If the effect is seen, we learn that the monster has a given flag.
 * Resistance is divided by the factor and reduced by a small random amount
 * (if reduce is set).
 *
 * context is the project_m context.
 * flag is the RF_ flag that the monster must have.
 * factor is the divisor for the base damage.
 * reduce should be true if the base damage * factor should be reduced.
 * msg is the message that should be displayed when the monster is hurt.
 */
static void project_monster_resist_other(project_monster_handler_context_t *context, int flag,
    int factor, bool reduce, enum mon_messages msg)
{
    if (context->seen) rf_on(context->lore->flags, flag);
    if (rf_has(context->mon->race->flags, flag))
    {
        context->hurt_msg = msg;
        context->dam *= factor;

        if (reduce)
            context->dam /= (randint1(6) + 6);
    }
}


/*
 * Resist an attack if the monster has the given flag or hurt the monster
 * more if it has another flag.
 *
 * If the effect is seen, we learn the status of both flags. Resistance is
 * divided by imm_factor while hurt is multiplied by hurt_factor.
 *
 * context is the project_m context.
 * hurt_flag is the RF_ flag that the monster must have to use the hurt factor.
 * imm_flag is the RF_ flag that the monster must have to use the resistance factor.
 * hurt_factor is the hurt multiplier for the base damage.
 * imm_factor is the resistance divisor for the base damage.
 * hurt_msg is the message that should be displayed when the monster is hurt.
 * die_msg is the message that should be displayed when the monster dies.
 */
static void project_monster_hurt_immune(project_monster_handler_context_t *context, int hurt_flag,
    int imm_flag, int hurt_factor, int imm_factor, enum mon_messages hurt_msg,
    enum mon_messages die_msg)
{
    if (context->seen)
    {
        rf_on(context->lore->flags, imm_flag);
        rf_on(context->lore->flags, hurt_flag);
    }
    if (rf_has(context->mon->race->flags, imm_flag))
    {
        context->hurt_msg = MON_MSG_RESIST_A_LOT;
        context->dam /= imm_factor;
    }
    else if (rf_has(context->mon->race->flags, hurt_flag))
    {
        context->hurt_msg = hurt_msg;
        context->die_msg = die_msg;
        context->dam *= hurt_factor;
    }
}


/*
 * Hurt the monster if it has a given flag or do no damage.
 *
 * If the effect is seen, we learn the status the flag. There is no damage
 * multiplier.
 *
 * context is the project_m context.
 * flag is the RF_ flag that the monster must have.
 * hurt_msg is the message that should be displayed when the monster is hurt.
 * die_msg is the message that should be displayed when the monster dies.
 */
static void project_monster_hurt_only(project_monster_handler_context_t *context, int flag,
    enum mon_messages hurt_msg, enum mon_messages die_msg)
{
    if (context->seen) rf_on(context->lore->flags, flag);

    if (rf_has(context->mon->race->flags, flag))
    {
        context->hurt_msg = hurt_msg;
        context->die_msg = die_msg;
    }
    else
        context->dam = 0;
}


/*
 * Resist an attack if the monster has the given spell flag.
 *
 * If the effect is seen, we learn that the monster has that spell (useful
 * for breaths). Resistance is multiplied by the factor and reduced by
 * a small random amount.
 *
 * context is the project_m context.
 * flag is the RSF_ flag that the monster must have.
 * factor is the multiplier for the base damage.
 */
static void project_monster_breath(project_monster_handler_context_t *context, int flag, int factor)
{
    if (rsf_has(context->mon->race->spell_flags, flag))
    {
        /* Learn about breathers through resistance */
        if (context->seen) rsf_on(context->lore->spell_flags, flag);

        context->hurt_msg = MON_MSG_RESIST;
        context->dam *= factor;
        context->dam /= (randint1(6) + 6);
    }
}


/*
 * Teleport away a monster that has a given flag.
 *
 * If the monster matches, it is teleported and the effect is obvious (if seen).
 * The player learns monster lore on whether or not the monster matches the
 * given flag if the effect is seen. Damage is not incurred by the monster.
 *
 * context is the project_m context.
 * flag is the RF_ flag that the monster must have.
 */
static void project_monster_teleport_away(project_monster_handler_context_t *context, int flag)
{
    if (context->seen) rf_on(context->lore->flags, flag);

    if (rf_has(context->mon->race->flags, flag))
        context->teleport_distance = context->dam;
    else
        context->skipped = true;

    context->obvious = true;
    context->dam = 0;
}


/*
 * Scare a monster that has a given flag.
 *
 * If the monster matches, fear is applied and the effect is obvious (if seen).
 * The player learns monster lore on whether or not the monster matches the
 * given flag if the effect is seen. Damage is not incurred by the monster.
 *
 * context is the project_m context.
 * flag is the RF_ flag that the monster must have.
 */
static void project_monster_scare(project_monster_handler_context_t *context, int flag)
{
    if (rf_has(context->mon->race->flags, flag))
        context->mon_timed[MON_TMD_FEAR] = context->dam;
    else
        context->skipped = true;

    context->obvious = true;
    context->dam = 0;
}


/*
 * Dispel a monster that has a given flag.
 *
 * If the monster matches, damage is applied and the effect is obvious
 * (if seen). Otherwise, no damage is applied and the effect is not obvious.
 * The player learns monster lore on whether or not the monster matches the
 * given flag if the effect is seen.
 *
 * context is the project_m context.
 * flag is the RF_ flag that the monster must have.
 */
static void project_monster_dispel(project_monster_handler_context_t *context, int flag)
{
    if (context->seen) rf_on(context->lore->flags, flag);

    if (rf_has(context->mon->race->flags, flag))
    {
        context->hurt_msg = MON_MSG_SHUDDER;
        context->die_msg = MON_MSG_DISSOLVE;
    }
    else
    {
        context->skipped = true;
        context->dam = 0;
    }

    context->obvious = true;
}


/*
 * Sleep a monster that has a given flag.
 *
 * If the monster matches, an attempt is made to put the monster to sleep
 * and the effect is obvious (if seen).
 * Otherwise, no damage is applied and the effect is not obvious.
 * The player learns monster lore on whether or not the monster matches the
 * given flag if the effect is seen.
 *
 * context is the project_m context.
 * flag is the RF_ flag that the monster must have.
 */
static void project_monster_sleep(project_monster_handler_context_t *context, int flag)
{
    int dam = sleep_value(context->mon->race);

    if (context->seen && flag) rf_on(context->lore->flags, flag);

    if (!flag || rf_has(context->mon->race->flags, flag))
    {
        if (context->charm && rf_has(context->mon->race->flags, RF_ANIMAL))
            dam += dam / 2;
        context->mon_timed[MON_TMD_SLEEP] = dam;
        context->dam = 0;
    }
    else
    {
        context->skipped = true;
        context->dam = 0;
    }

    context->obvious = true;
}


/* Acid */
static void project_monster_handler_ACID(project_monster_handler_context_t *context)
{
    project_monster_resist_element(context, RF_IM_ACID, 9);
}


/* Electricity */
static void project_monster_handler_ELEC(project_monster_handler_context_t *context)
{
    project_monster_resist_element(context, RF_IM_ELEC, 9);
}


/* Fire damage */
static void project_monster_handler_FIRE(project_monster_handler_context_t *context)
{
    project_monster_hurt_immune(context, RF_HURT_FIRE, RF_IM_FIRE, 2, 9, MON_MSG_CATCH_FIRE,
        MON_MSG_DISINTEGRATES);
}


/* Cold */
static void project_monster_handler_COLD(project_monster_handler_context_t *context)
{
    project_monster_hurt_immune(context, RF_HURT_COLD, RF_IM_COLD, 2, 9, MON_MSG_BADLY_FROZEN,
        MON_MSG_FREEZE_SHATTER);
}


/* Poison */
static void project_monster_handler_POIS(project_monster_handler_context_t *context)
{
    project_monster_resist_element(context, RF_IM_POIS, 9);

    /* Apply poison */
    context->mon_timed[MON_TMD_POIS] = adjust_radius(context, 5 + randint1(10));
}


/* Light -- opposite of Dark */
static void project_monster_handler_LIGHT(project_monster_handler_context_t *context)
{
    if (context->seen) rf_on(context->lore->flags, RF_HURT_LIGHT);

    if (rsf_has(context->mon->race->spell_flags, RSF_BR_LIGHT))
    {
        /* Learn about breathers through resistance */
        if (context->seen) rsf_on(context->lore->spell_flags, RSF_BR_LIGHT);

        context->hurt_msg = MON_MSG_RESIST;
        context->dam *= 2;
        context->dam /= (randint1(6) + 6);
    }
    else if (rf_has(context->mon->race->flags, RF_HURT_LIGHT))
    {
        context->hurt_msg = MON_MSG_CRINGE_LIGHT;
        context->die_msg = MON_MSG_SHRIVEL_LIGHT;
        context->dam *= 2;
    }
}


/* Dark -- opposite of Light */
static void project_monster_handler_DARK(project_monster_handler_context_t *context)
{
    project_monster_breath(context, RSF_BR_DARK, 2);
}


/* Sound -- sound breathers resist */
static void project_monster_handler_SOUND(project_monster_handler_context_t *context)
{
    project_monster_breath(context, RSF_BR_SOUN, 2);

    /* Apply stunning */
    context->mon_timed[MON_TMD_STUN] = adjust_radius(context, 5 + randint1(10));
}


/* Shards -- shard breathers resist */
static void project_monster_handler_SHARD(project_monster_handler_context_t *context)
{
    project_monster_breath(context, RSF_BR_SHAR, 3);

    /* Apply bleeding */
    context->mon_timed[MON_TMD_CUT] = adjust_radius(context, 5 + randint1(10));
}


/* Nexus */
static void project_monster_handler_NEXUS(project_monster_handler_context_t *context)
{
    project_monster_resist_other(context, RF_IM_NEXUS, 3, true, MON_MSG_RESIST);
}


/* Nether -- see above */
static void project_monster_handler_NETHER(project_monster_handler_context_t *context)
{
    /* Update the lore */
    if (context->seen)
    {
        /* Acquire knowledge of undead type and nether resistance */
        rf_on(context->lore->flags, RF_UNDEAD);
        rf_on(context->lore->flags, RF_IM_NETHER);

        /* If it isn't undead, acquire extra knowledge */
        if (!rf_has(context->mon->race->flags, RF_UNDEAD))
        {
            /* Learn this creature breathes nether if true */
            if (rsf_has(context->mon->race->spell_flags, RSF_BR_NETH))
                rsf_on(context->lore->spell_flags, RSF_BR_NETH);

            /* Otherwise learn about evil type */
            else
                rf_on(context->lore->flags, RF_EVIL);
        }
    }

    if (rf_has(context->mon->race->flags, RF_UNDEAD))
    {
        context->hurt_msg = MON_MSG_IMMUNE;
        context->dam = 0;
    }
    else if (rf_has(context->mon->race->flags, RF_IM_NETHER))
    {
        context->hurt_msg = MON_MSG_RESIST;
        context->dam *= 3;
        context->dam /= (randint1(6) + 6);
    }
    else if (monster_is_evil(context->mon))
    {
        context->dam /= 2;
        context->hurt_msg = MON_MSG_RESIST_SOMEWHAT;
    }
}


/* Chaos -- chaos breathers resist */
static void project_monster_handler_CHAOS(project_monster_handler_context_t *context)
{
    context->do_poly = 1;

    /* Forbid in the towns and on special levels */
    if (forbid_special(&context->cave->wpos)) context->do_poly = 0;

    /* Prevent polymorph on chaos breathers. */
    if (rsf_has(context->mon->race->spell_flags, RSF_BR_CHAO))
        context->do_poly = 0;

    /* Apply confusion */
    context->mon_timed[MON_TMD_CONF] = adjust_radius(context, 10 + randint1(10));

    project_monster_breath(context, RSF_BR_CHAO, 3);
}


/* Disenchantment */
static void project_monster_handler_DISEN(project_monster_handler_context_t *context)
{
    project_monster_resist_other(context, RF_IM_DISEN, 3, true, MON_MSG_RESIST);

    /* Affect monsters which don't resist, and have non-innate spells */
    if (!rf_has(context->mon->race->flags, RF_IM_DISEN) &&
        monster_has_non_innate_spells(context->mon))
    {
        context->mon_timed[MON_TMD_DISEN] = adjust_radius(context, 5 + randint1(10));
    }
}


/* Water damage */
static void project_monster_handler_WATER(project_monster_handler_context_t *context)
{
    /* Zero out the damage because this is an immunity flag. */
    project_monster_resist_other(context, RF_IM_WATER, 0, false, MON_MSG_IMMUNE);

    /* Immune to stunning and confusion */
    if (rf_has(context->mon->race->flags, RF_IM_WATER))
    {
        if (context->seen)
        {
            rf_on(context->lore->flags, RF_NO_STUN);
            rf_on(context->lore->flags, RF_NO_CONF);
        }
    }
    else
    {
        /* Apply stunning */
        context->mon_timed[MON_TMD_STUN] = adjust_radius(context, 5 + randint1(10));

        /* Apply confusion */
        context->mon_timed[MON_TMD_CONF] = adjust_radius(context, 10 + randint1(10));
    }
}


/* Ice -- cold + cuts + stun */
static void project_monster_handler_ICE(project_monster_handler_context_t *context)
{
    /* Message */
    add_monster_message(context->origin->player, context->mon, MON_MSG_ICE, false);

    /* Apply stunning */
    context->mon_timed[MON_TMD_STUN] = adjust_radius(context, 5 + randint1(10));

    /* Apply bleeding */
    context->mon_timed[MON_TMD_CUT] = adjust_radius(context, 5 + randint1(10));

    project_monster_hurt_immune(context, RF_HURT_COLD, RF_IM_COLD, 2, 9, MON_MSG_BADLY_FROZEN,
        MON_MSG_FREEZE_SHATTER);
}


/* Gravity -- breathers resist */
static void project_monster_handler_GRAVITY(project_monster_handler_context_t *context)
{
    context->teleport_distance = 10;

    /* Higher level monsters can resist the teleportation better */
    if (CHANCE(context->mon->level, z_info->max_depth)) context->teleport_distance = 0;

    /* Prevent displacement on gravity breathers. */
    if (rsf_has(context->mon->race->spell_flags, RSF_BR_GRAV))
        context->teleport_distance = 0;

    project_monster_breath(context, RSF_BR_GRAV, 3);

    /* Gravity effect */
    if (!rsf_has(context->mon->race->spell_flags, RSF_BR_GRAV))
        context->do_gravity = true;
}


/* Inertia -- breathers resist */
static void project_monster_handler_INERTIA(project_monster_handler_context_t *context)
{
    project_monster_breath(context, RSF_BR_INER, 3);

    /* Apply slowing */
    context->mon_timed[MON_TMD_SLOW] = adjust_radius(context, 10 + randint1(10));
}


/* Force */
static void project_monster_handler_FORCE(project_monster_handler_context_t *context)
{
    struct loc centre;
    struct source who_body;
    struct source *who = &who_body;

    /* Get location of caster (assumes index of caster is not zero) */
    origin_get_loc(&centre, context->origin);

    /* Apply stunning */
    context->mon_timed[MON_TMD_STUN] = adjust_radius(context, 5 + randint1(10));

    project_monster_breath(context, RSF_BR_WALL, 3);

    /* Prevent thrusting force breathers. */
    if (rsf_has(context->mon->race->spell_flags, RSF_BR_WALL)) return;

    /* Thrust monster away */
    source_monster(who, context->mon);
    who->player = context->origin->player;
    thrust_away(context->cave, who, &centre, 3 + context->dam / 20);

    /* Hack -- get new location */
    loc_copy(&context->grid, &context->mon->grid);
}


/* Time -- breathers resist */
static void project_monster_handler_TIME(project_monster_handler_context_t *context)
{
    project_monster_breath(context, RSF_BR_TIME, 3);
}


/* Plasma */
static void project_monster_handler_PLASMA(project_monster_handler_context_t *context)
{
    project_monster_resist_other(context, RF_IM_PLASMA, 3, true, MON_MSG_RESIST);

    /* Immune to stunning */
    if (rf_has(context->mon->race->flags, RF_IM_PLASMA))
    {
        if (context->seen) rf_on(context->lore->flags, RF_NO_STUN);
    }
    else
    {
        /* Apply stunning */
        context->mon_timed[MON_TMD_STUN] = adjust_radius(context, 5 + randint1(10));
    }
}


static void project_monster_handler_METEOR(project_monster_handler_context_t *context) {}
static void project_monster_handler_MISSILE(project_monster_handler_context_t *context) {}


/* Mana -- breathers resist */
static void project_monster_handler_MANA(project_monster_handler_context_t *context)
{
    project_monster_breath(context, RSF_BR_MANA, 3);
}


/* Holy Orb -- hurts evil */
static void project_monster_handler_HOLY_ORB(project_monster_handler_context_t *context)
{
    project_monster_resist_other(context, RF_EVIL, 2, false, MON_MSG_HIT_HARD);
}


static void project_monster_handler_ARROW_X(project_monster_handler_context_t *context) {}
static void project_monster_handler_ARROW_1(project_monster_handler_context_t *context) {}
static void project_monster_handler_ARROW_2(project_monster_handler_context_t *context) {}
static void project_monster_handler_ARROW_3(project_monster_handler_context_t *context) {}
static void project_monster_handler_ARROW_4(project_monster_handler_context_t *context) {}
static void project_monster_handler_BOULDER(project_monster_handler_context_t *context) {}


/* Light, but only hurts susceptible creatures */
static void project_monster_handler_LIGHT_WEAK(project_monster_handler_context_t *context)
{
    project_monster_hurt_only(context, RF_HURT_LIGHT, MON_MSG_CRINGE_LIGHT, MON_MSG_SHRIVEL_LIGHT);
}


static void project_monster_handler_DARK_WEAK(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


/* Stone to Mud */
static void project_monster_handler_KILL_WALL(project_monster_handler_context_t *context)
{
    project_monster_hurt_only(context, RF_HURT_ROCK, MON_MSG_LOSE_SKIN, MON_MSG_DISSOLVE);
}


static void project_monster_handler_KILL_DOOR(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


static void project_monster_handler_KILL_TRAP(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


static void project_monster_handler_MAKE_DOOR(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


static void project_monster_handler_MAKE_TRAP(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


static void project_monster_handler_STONE_WALL(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


static void project_monster_handler_RAISE(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


/* Teleport evil (Use "dam" as "power") */
static void project_monster_handler_AWAY_EVIL(project_monster_handler_context_t *context)
{
    project_monster_teleport_away(context, RF_EVIL);
}


/* Teleport spirits (Use "dam" as "power") */
static void project_monster_handler_AWAY_SPIRIT(project_monster_handler_context_t *context)
{
    project_monster_teleport_away(context, RF_SPIRIT);
}


/* Teleport monster (Use "dam" as "power") */
static void project_monster_handler_AWAY_ALL(project_monster_handler_context_t *context)
{
    /* Prepare to teleport */
    context->teleport_distance = context->dam;

    /* No "real" damage */
    context->dam = 0;
}


/* Turn undead (Use "dam" as "power") */
static void project_monster_handler_TURN_UNDEAD(project_monster_handler_context_t *context)
{
    project_monster_scare(context, RF_UNDEAD);
}


/* Turn living (Use "dam" as "power") */
static void project_monster_handler_TURN_LIVING(project_monster_handler_context_t *context)
{
    if (monster_is_living(context->mon))
        context->mon_timed[MON_TMD_FEAR] = context->dam;
    else
        context->skipped = true;

    context->obvious = true;
    context->dam = 0;
}


/* Turn monster (Use "dam" as "power") */
static void project_monster_handler_TURN_ALL(project_monster_handler_context_t *context)
{
    context->mon_timed[MON_TMD_FEAR] = context->dam;
    context->dam = 0;
}


/* Dispel undead */
static void project_monster_handler_DISP_UNDEAD(project_monster_handler_context_t *context)
{
    project_monster_dispel(context, RF_UNDEAD);
}


/* Dispel evil */
static void project_monster_handler_DISP_EVIL(project_monster_handler_context_t *context)
{
    project_monster_dispel(context, RF_EVIL);
}


/* Dispel monster */
static void project_monster_handler_DISP_ALL(project_monster_handler_context_t *context)
{
    context->hurt_msg = MON_MSG_SHUDDER;
    context->die_msg = MON_MSG_DISSOLVE;
}


/* Sleep (Ignore "dam") */
static void project_monster_handler_SLEEP_UNDEAD(project_monster_handler_context_t *context)
{
    project_monster_sleep(context, RF_UNDEAD);
}


/* Sleep (Ignore "dam") */
static void project_monster_handler_SLEEP_EVIL(project_monster_handler_context_t *context)
{
    project_monster_sleep(context, RF_EVIL);
}


/* Sleep (Ignore "dam") */
static void project_monster_handler_SLEEP_ALL(project_monster_handler_context_t *context)
{
    project_monster_sleep(context, RF_NONE);
}


/* Clone monsters (Ignore "dam") */
static void project_monster_handler_MON_CLONE(project_monster_handler_context_t *context)
{
    /* Heal fully */
    context->mon->hp = context->mon->maxhp;

    /* Speed up */
    mon_inc_timed(context->origin->player, context->mon, MON_TMD_FAST, 50, MON_TMD_FLG_NOTIFY);

    /* Attempt to clone */
    if (multiply_monster(context->origin->player, context->cave, context->mon))
        context->hurt_msg = MON_MSG_SPAWN;

    /* No "real" damage */
    context->dam = 0;
}


/* Polymorph monster (Use "dam" as "power") */
static void project_monster_handler_MON_POLY(project_monster_handler_context_t *context)
{
    if (context->charm && rf_has(context->mon->race->flags, RF_ANIMAL))
        context->dam += context->dam / 2;

    /* Polymorph later */
	context->do_poly = context->dam;

	/* No "real" damage */
	context->dam = 0;
}


/* Heal Monster (use "dam" as amount of healing) */
static void project_monster_handler_MON_HEAL(project_monster_handler_context_t *context)
{
    struct source mon_body;
    struct source *mon = &mon_body;

    /* Don't heal monsters with Forest Embrace */
    if (!context->mon)
    {
        /* Irrelevant */
        context->skipped = true;

        /* No damage */
        context->dam = 0;

        return;
    }

    source_monster(mon, context->mon);

    /* Wake up */
    mon_clear_timed(context->origin->player, context->mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);
    mon_clear_timed(context->origin->player, context->mon, MON_TMD_HOLD, MON_TMD_FLG_NOTIFY);

    /* Heal */
    context->mon->hp += context->dam;

    /* No overflow */
    if (context->mon->hp > context->mon->maxhp) context->mon->hp = context->mon->maxhp;

    /* Redraw (later) if needed */
    update_health(mon);

    /* Message */
    context->hurt_msg = MON_MSG_HEALTHIER;

    /* No "real" damage */
    context->dam = 0;
}


/* Speed Monster (Ignore "dam") */
static void project_monster_handler_MON_SPEED(project_monster_handler_context_t *context)
{
    context->mon_timed[MON_TMD_FAST] = context->dam;
    context->dam = 0;
}


/* Slow Monster (Use "dam" as "power") */
static void project_monster_handler_MON_SLOW(project_monster_handler_context_t *context)
{
    if (context->charm && rf_has(context->mon->race->flags, RF_ANIMAL))
        context->dam += context->dam / 2;
    context->mon_timed[MON_TMD_SLOW] = context->dam;
    context->dam = 0;
}


/* Confusion (Use "dam" as "power") */
static void project_monster_handler_MON_CONF(project_monster_handler_context_t *context)
{
    if (context->charm && rf_has(context->mon->race->flags, RF_ANIMAL))
        context->dam += context->dam / 2;
    context->mon_timed[MON_TMD_CONF] = context->dam;
    context->dam = 0;
}


/* Hold (Use "dam" as "power") */
static void project_monster_handler_MON_HOLD(project_monster_handler_context_t *context)
{
    if (context->charm && rf_has(context->mon->race->flags, RF_ANIMAL))
        context->dam += context->dam / 2;
    context->mon_timed[MON_TMD_HOLD] = context->dam;
    context->dam = 0;
}


/* Stun (Use "dam" as "power") */
static void project_monster_handler_MON_STUN(project_monster_handler_context_t *context)
{
    if (context->charm && rf_has(context->mon->race->flags, RF_ANIMAL))
        context->dam += context->dam / 2;
    context->mon_timed[MON_TMD_STUN] = context->dam;
    context->dam = 0;
}


/* Drain Life */
static void project_monster_handler_MON_DRAIN(project_monster_handler_context_t *context)
{
    if (context->seen)
        rf_on(context->lore->flags, RF_UNDEAD);
    if (monster_is_nonliving(context->mon->race))
    {
        context->hurt_msg = MON_MSG_UNAFFECTED;
        context->obvious = false;
        context->dam = 0;
    }
}


/* Crush */
static void project_monster_handler_MON_CRUSH(project_monster_handler_context_t *context)
{
    if (context->seen) context->obvious = true;
    if (context->mon->hp >= context->dam)
    {
        context->hurt_msg = MON_MSG_UNAFFECTED;
        context->dam = 0;
        context->obvious = false;
    }
}


/* Mind Blast */
static void project_monster_handler_PSI(project_monster_handler_context_t *context)
{
    context->die_msg = MON_MSG_DROP_DEAD;

    /* Mindless creatures are immune */
    if (rf_has(context->mon->race->flags, RF_EMPTY_MIND))
    {
        context->dam = 0;
        context->hurt_msg = MON_MSG_IMMUNE;
        context->obvious = false;
        return;
    }

    /* Weak-minded or strong creatures resist most of the time */
    if (monster_is_stupid(context->mon->race) ||
        rf_has(context->mon->race->flags, RF_WEIRD_MIND) ||
        rf_has(context->mon->race->flags, RF_ANIMAL) ||
        (context->mon->level > randint1(3 * context->dam)))
    {
        if (magik(66))
        {
            context->dam /= 3;
            context->hurt_msg = MON_MSG_RESIST;
            return;
        }
    }

    if (context->dam <= 0) return;

    /* Mind Blast (psi spell): random conf/stun/fear/sleep effect */
    if (context->type == PROJ_PSI)
    {
        switch (randint1(4))
        {
            case 1: context->mon_timed[MON_TMD_CONF] = 5 + randint1(5); break;
            case 2: context->mon_timed[MON_TMD_STUN] = 5 + randint1(5); break;
            case 3: context->mon_timed[MON_TMD_FEAR] = 10 + randint1(10); break;
            default: context->mon_timed[MON_TMD_SLEEP] = sleep_value(context->mon->race); break;
        }
    }

    /* Mind Blast (monster spell): conf effect */
    else if (context->type == PROJ_BLAST)
        context->mon_timed[MON_TMD_CONF] = 5 + randint1(5);

    /* Brain Smash: random blind/conf/sleep/slow effect */
    else
    {
        switch (randint1(4))
        {
            case 1: context->mon_timed[MON_TMD_BLIND] = 5 + randint1(5); break;
            case 2: context->mon_timed[MON_TMD_CONF] = 5 + randint1(5); break;
            case 3: context->mon_timed[MON_TMD_SLEEP] = sleep_value(context->mon->race); break;
            default: context->mon_timed[MON_TMD_SLOW] = 10 + randint1(10); break;
        }
    }
}


/* Drain Life + replenish SP */
static void project_monster_handler_PSI_DRAIN(project_monster_handler_context_t *context)
{
    if (context->seen)
        rf_on(context->lore->flags, RF_UNDEAD);
    if (monster_is_nonliving(context->mon->race))
    {
        context->hurt_msg = MON_MSG_UNAFFECTED;
        context->obvious = false;
        context->dam = 0;
    }
    else
    {
        int drain = context->dam;
        char dice[5];
        struct source who_body;
        struct source *who = &who_body;

        if (drain > context->mon->hp) drain = context->mon->hp;
        strnfmt(dice, sizeof(dice), "%d", 1 + 3 * drain / 4);
        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);
        effect_simple(EF_RESTORE_MANA, who, dice, 0, 0, 0, 0, 0, NULL);
    }
}

static void project_monster_handler_CURSE(project_monster_handler_context_t *context) {}


/* Heavy curse -- damage + cuts */
static void project_monster_handler_CURSE2(project_monster_handler_context_t *context)
{
    /* Apply bleeding */
    context->mon_timed[MON_TMD_CUT] = 5 + randint1(5);
}


/* Drain Life + replenish HP */
static void project_monster_handler_DRAIN(project_monster_handler_context_t *context)
{
    if (context->seen)
        rf_on(context->lore->flags, RF_UNDEAD);
    if (monster_is_nonliving(context->mon->race))
    {
        context->hurt_msg = MON_MSG_UNAFFECTED;
        context->obvious = false;
        context->dam = 0;
    }
    else
    {
        int drain = context->dam;

        if (drain > context->mon->hp) drain = context->mon->hp;
        hp_player_safe(context->origin->player, 1 + drain / 2);
    }
}


static void project_monster_handler_order(project_monster_handler_context_t *context, byte status)
{
    int id = (context->origin->player? context->origin->player->id: -1);
    bool controlled = ((context->mon->master == id)? true: false);

    if (context->seen) rf_on(context->lore->flags, RF_UNDEAD);

    /* Only if the undead monster is not already under the spell */
    if (context->origin->player && !(controlled && (context->mon->status == status)) &&
        rf_has(context->mon->race->flags, RF_UNDEAD))
    {
        /* Obvious */
        if (context->seen) context->obvious = true;

        /* Controlled monsters are more likely to react to new orders */
        if (controlled) context->dam *= 2;

        /* Attempt a saving throw */
        if (monster_is_unique(context->mon->race) ||
            CHANCE(context->mon->level - 10, (context->dam < 11)? 1: (context->dam - 10)))
        {
            /* No obvious effect */
            context->hurt_msg = MON_MSG_UNAFFECTED;
            context->obvious = false;
        }
        else if (player_of_has(context->origin->player, OF_AGGRAVATE))
        {
            /* Too enraged to be controlled */
            context->hurt_msg = MON_MSG_HATE;
            context->obvious = false;
        }
        else
        {
            /* Order monster */
            context->hurt_msg = MON_MSG_REACT;
            monster_set_master(context->mon, context->origin->player, status);
        }
    }

    /* No "real" damage */
    context->dam = 0;
}


/* Command undead monsters (Use "dam" as "power") */
static void project_monster_handler_COMMAND(project_monster_handler_context_t *context)
{
    /* Order undead monsters to attack */
    if (context->origin->player && magik(context->origin->player->lev * 9 / 5))
        project_monster_handler_order(context, MSTATUS_ATTACK);

    /* Order undead monsters to follow */
    else if (context->origin->player && magik(context->origin->player->lev * 9 / 5))
        project_monster_handler_order(context, MSTATUS_FOLLOW);

    /* Order undead monsters to stay still */
    else
        project_monster_handler_order(context, MSTATUS_GUARD);
}


/* Teleport monster to player */
static void project_monster_handler_TELE_TO(project_monster_handler_context_t *context)
{
    /* Teleport */
    if (!context->origin->player)
        context->obvious = false;
    else
    {
        struct loc grid;
        struct source who_body;
        struct source *who = &who_body;

        loc_copy(&grid, &context->mon->grid);

        source_player(who, get_player_index(get_connection(context->origin->player->conn)),
            context->origin->player);
        who->monster = context->mon;
        effect_simple(EF_TELEPORT_TO, who, "0", 0, 0, 0, context->origin->player->grid.y,
            context->origin->player->grid.x, NULL);
        if (!loc_eq(&context->mon->grid, &grid))
            context->hurt_msg = MON_MSG_RETURN;
        else
            context->obvious = false;

        /* Hack -- get new location */
        loc_copy(&context->grid, &context->mon->grid);
    }

    /* No "real" damage */
    context->dam = 0;
}


/* Teleport monster to another level */
static void project_monster_handler_TELE_LEVEL(project_monster_handler_context_t *context)
{
    /* Unique monsters resist */
    if (monster_is_unique(context->mon->race))
    {
        context->hurt_msg = MON_MSG_UNAFFECTED;
        context->obvious = false;
    }

    /* Normal monsters are banished */
    else
    {
        context->hurt_msg = MON_MSG_DISAPPEAR;
        context->do_delete = true;
    }

    /* No "real" damage */
    context->dam = 0;
}


/* Blindness (Use "dam" as "power") */
static void project_monster_handler_MON_BLIND(project_monster_handler_context_t *context)
{
    context->mon_timed[MON_TMD_BLIND] = context->dam;
    context->dam = 0;
}


/* Drain mana (Use "dam" as "power") */
static void project_monster_handler_DRAIN_MANA(project_monster_handler_context_t *context)
{
    /* Affects only casters */
    if (context->mon->race->freq_spell)
    {
        int r1;

        /* Attack power, capped vs monster level */
        r1 = (randint1(context->dam) / 2) + 1;
        if (r1 > (context->mon->level / 6) + 1) r1 = (context->mon->level / 6) + 1;

        /* Heal player */
        hp_player(context->origin->player, r1 * 6);
    }
    else
    {
        /* No obvious effect */
        context->hurt_msg = MON_MSG_UNAFFECTED;
        context->obvious = false;
    }

    /* No "real" damage */
    context->dam = 0;
}


static void project_monster_handler_FORGET(project_monster_handler_context_t *context)
{
    project_monster_handler_MON_CONF(context);
}


static void project_monster_handler_BLAST(project_monster_handler_context_t *context)
{
    project_monster_handler_PSI(context);
}


static void project_monster_handler_SMASH(project_monster_handler_context_t *context)
{
    project_monster_handler_PSI(context);
}


/* Order summoned monsters to attack */
static void project_monster_handler_CONTROL(project_monster_handler_context_t *context)
{
    /* Try to charm the monster */
    context->hurt_msg = charm_monster(context->mon, context->origin->player, context->dam);

    /* No obvious effect */
    if (context->hurt_msg != MON_MSG_REACT) context->obvious = false;

    /* No "real" damage */
    context->dam = 0;
}


static void project_monster_handler_PROJECT(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


static void project_monster_handler_TREES(project_monster_handler_context_t *context)
{
    /* Irrelevant */
    context->skipped = true;

    /* No damage */
    context->dam = 0;
}


/* Teleport animals (Use "dam" as "power") */
static void project_monster_handler_AWAY_ANIMAL(project_monster_handler_context_t *context)
{
    project_monster_teleport_away(context, RF_ANIMAL);
}


static const project_monster_handler_f monster_handlers[] =
{
    #define ELEM(a, b, c, d) project_monster_handler_##a,
    #include "../common/list-elements.h"
    #undef ELEM
    #define PROJ(a) project_monster_handler_##a,
    #include "../common/list-projections.h"
    #undef PROJ
    NULL
};


/*
 * Deal damage to a monster from another monster.
 *
 * Returns true if the monster has been killed (and deleted).
 */
bool project_m_monster_attack_aux(struct monster *attacker, struct chunk *c, struct monster *mon,
    int dam, byte note)
{
    struct source origin_body;
    struct source *origin = &origin_body;

    /* "Unique" monsters can only be "killed" by the player */
    if (monster_is_unique(mon->race))
    {
        /* Reduce monster hp to zero, but don't kill it. */
        if (dam > mon->hp) dam = mon->hp;
    }

    /* Redraw (later) if needed */
    source_monster(origin, mon);
    update_health(origin);

    /* Wake the monster up */
    mon_clear_timed(NULL, mon, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);
    mon_clear_timed(NULL, mon, MON_TMD_HOLD, MON_TMD_FLG_NOTIFY);

    /* Become aware of its presence */
    if (monster_is_camouflaged(mon)) become_aware(NULL, c, mon);

    /* Hurt the monster */
    mon->hp -= dam;

    /* Dead monster */
    if (mon->hp < 0)
    {
        int i;

        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *p = player_get(i);
            byte die_msg = note;
            bool seen = monster_is_visible(p, mon->midx);

            /* If he's not here, skip him */
            if (!wpos_eq(&p->wpos, &mon->wpos)) continue;

            /* Give detailed messages if destroyed */
            if (!seen)
                die_msg = MON_MSG_MORIA_DEATH;

            /* Death message */
            add_monster_message(p, mon, die_msg, false);

            /* Reward the master with some experience */
            if (attacker && (p->id == attacker->master)) monster_give_xp(p, c, mon, true);

            /* Redraw */
            p->upkeep->redraw |= (PR_MONLIST | PR_ITEMLIST);
        }

        /* Drop objects being carried */
        monster_drop_carried(NULL, c, mon, -1, false, NULL, NULL);

        /* Drop a corpse */
        monster_drop_corpse(NULL, c, mon);

        /* Delete the monster */
        delete_monster_idx(c, mon->midx);

        /* Monster is dead */
        return true;
    }

    /* Not dead yet */
    return false;
}


/*
 * Deal damage to a monster from another monster.
 *
 * This is a helper for project_m(). It is very similar to mon_take_hit(),
 * but eliminates the player-oriented stuff of that function. It isn't a type
 * handler, but we take a handler context since that has a lot of what we need.
 *
 * context is the project_m context.
 * return true if the monster died, false if it is still alive.
 */
static bool project_m_monster_attack(project_monster_handler_context_t *context)
{
    return project_m_monster_attack_aux(context->origin->monster, context->cave, context->mon,
        context->dam, context->die_msg);
}


/*
 * Deal damage to a monster from the player
 *
 * This is a helper for project_m(). It isn't a type handler, but we take a
 * handler context since that has a lot of what we need.
 *
 * context is the project_m context.
 * return true if the monster died, false if it is still alive.
 */
static bool project_m_player_attack(project_monster_handler_context_t *context)
{
    bool fear = false;
    bool mon_died = false;
    bool seen = context->seen;
    int dam = context->dam;
    enum mon_messages die_msg = context->die_msg;
    enum mon_messages hurt_msg = context->hurt_msg;
    struct monster *mon = context->mon;

    /*
     * No damage is now going to mean the monster is not hit - and hence
     * is not woken or released from holding.
     */
    if (!dam)
    {
        /* PWMAngband: cancel fire-till-kill if active */
        if (context->origin->player->firing_request > 1)
            context->origin->player->firing_request = 1;
        return false;
    }

    /*
     * The monster is going to be killed, so display a specific death message.
     * If the monster is not visible to the player, use a generic message.
     *
     * Note that mon_take_hit() below is passed a note of -1, which
     * ensures it doesn't print any death message and allows correct ordering
     * of messages.
     */
    if (dam > mon->hp)
    {
        if (!seen)
            die_msg = MON_MSG_MORIA_DEATH;
        add_monster_message(context->origin->player, mon, die_msg, false);
    }

    mon_died = mon_take_hit(context->origin->player, context->cave, mon, dam, &fear, -1);

    /*
     * If the monster didn't die, provide additional messages about how it was
     * hurt/damaged. If a specific message isn't provided, display a message
     * based on the amount of damage dealt. Also display a message
     * if the hit caused the monster to flee.
     */
    if (!mon_died)
    {
        if (seen && (hurt_msg != MON_MSG_NONE))
            add_monster_message(context->origin->player, mon, hurt_msg, false);
        else if (dam > 0)
            message_pain(context->origin->player, mon, dam);
        if (seen && fear)
            add_monster_message(context->origin->player, mon, MON_MSG_FLEE_IN_TERROR, true);
    }

    return mon_died;
}


/*
 * Deal damage to a monster from a trap.
 *
 * Returns true if the monster has been killed (and deleted).
 */
static bool project_m_trap_attack(project_monster_handler_context_t *context)
{
    return project_m_monster_attack_aux(NULL, context->cave, context->mon,
        context->dam, context->die_msg);
}


/*
 * Apply side effects from an attack onto a monster.
 *
 * This is a helper for project_m(). It isn't a type handler, but we take a
 * handler context since that has a lot of what we need.
 *
 * context is the project_m context.
 * m_idx is the cave monster index.
 * return true if a problem occured, false otherwise.
 */
static bool project_m_apply_side_effects(project_monster_handler_context_t *context, int *m_idx)
{
    int typ = context->type;

    /*
     * Handle side effects of an attack. First we check for polymorphing since
     * it may not make sense to apply status effects to a changed monster.
     * Right now, teleporting is also separate, but it could make sense in the
     * future to change it so that we can apply other effects AND teleport the
     * monster.
     */
    if (context->do_poly)
    {
        struct loc grid = context->grid;
        int savelvl = 0;
        struct monster_race *old_race;
        struct monster_race *new_race;

        /* Uniques cannot be polymorphed */
        if (monster_is_unique(context->mon->race))
        {
            if (typ == PROJ_MON_POLY)
                add_monster_message(context->origin->player, context->mon, MON_MSG_UNAFFECTED, false);
            return false;
        }

        if (context->seen) context->obvious = true;

        /* Saving throws depend on damage for direct poly, random for chaos */
        if (typ == PROJ_MON_POLY)
            savelvl = randint1(MAX(1, context->do_poly - 10)) + 10;
        else
            savelvl = randint1(90);
        if (context->mon->level > savelvl)
        {
            if (typ == PROJ_MON_POLY)
            {
                add_monster_message(context->origin->player, context->mon, MON_MSG_MAINTAIN_SHAPE,
                    false);
            }
            return false;
        }

        old_race = context->mon->race;
        new_race = poly_race(context->cave, old_race);

        /* Handle polymorph */
        if (new_race != old_race)
        {
            /* Report the polymorph before changing the monster */
            add_monster_message(context->origin->player, context->mon, MON_MSG_CHANGE, false);

            /* Delete the old monster, and return a new one */
            delete_monster_idx(context->cave, *m_idx);
            if (place_new_monster(context->origin->player, context->cave, &grid, new_race, 0,
                ORIGIN_DROP_POLY))
            {
                *m_idx = square(context->cave, &grid)->mon;
                context->mon = cave_monster(context->cave, *m_idx);
            }
            else
                return true;
        }
    }
    else if (context->do_gravity)
    {
        /* Message */
        add_monster_message(context->origin->player, context->mon, MON_MSG_TORN, false);

        /* Obvious */
        if (context->seen) context->obvious = true;

        /* Teleport */
        if (context->teleport_distance)
        {
            char dice[5];
            struct source who_body;
            struct source *who = &who_body;

            strnfmt(dice, sizeof(dice), "%d", context->teleport_distance);
            who->idx = (context->origin->player?
                get_player_index(get_connection(context->origin->player->conn)): 0);
            who->player = context->origin->player;
            who->monster = context->mon;
            who->trap = NULL;
            effect_simple(EF_TELEPORT, who, dice, 0, 0, 0, 0, 0, NULL);

            /* Hack -- get new location */
            loc_copy(&context->grid, &context->mon->grid);
        }

        /* Apply stunning */
        context->mon_timed[MON_TMD_STUN] = adjust_radius(context, 5 + randint1(10));
        if (context->mon_timed[MON_TMD_STUN])
        {
            mon_inc_timed(context->origin->player, context->mon, MON_TMD_STUN,
                context->mon_timed[MON_TMD_STUN], context->flag);
        }

        /* Apply slowing */
        context->mon_timed[MON_TMD_SLOW] = adjust_radius(context, 10 + randint1(10));
        if (context->mon_timed[MON_TMD_SLOW])
        {
            mon_inc_timed(context->origin->player, context->mon, MON_TMD_SLOW,
                context->mon_timed[MON_TMD_SLOW], context->flag);
        }
    }
    else if (context->teleport_distance)
    {
        char dice[5];
        int fy = context->mon->grid.y;
        int fx = context->mon->grid.x;
        struct source who_body;
        struct source *who = &who_body;

        if (context->seen) context->obvious = true;
        strnfmt(dice, sizeof(dice), "%d", context->teleport_distance);
        who->idx = (context->origin->player?
            get_player_index(get_connection(context->origin->player->conn)): 0);
        who->player = context->origin->player;
        who->monster = context->mon;
        who->trap = NULL;
        effect_simple(EF_TELEPORT, who, dice, 0, 0, 0, 0, 0, NULL);
        if ((context->mon->grid.y == fy) && (context->mon->grid.x == fx))
            context->obvious = false;

        /* Hack -- get new location */
        loc_copy(&context->grid, &context->mon->grid);
    }
    else
    {
        int i;

        for (i = 0; i < MON_TMD_MAX; i++)
        {
            if (context->mon_timed[i])
            {
                mon_inc_timed(context->origin->player, context->mon, i, context->mon_timed[i],
                    context->flag);
                context->obvious = true;
            }
        }
    }

    return false;
}


static bool project_m_is_threat(project_monster_handler_context_t *context)
{
    /* Paranoia */
    if ((context->type < 0) || (context->type >= PROJ_MAX)) return false;

    /* Always a threat */
    if (streq(projections[context->type].threat, "always")) return true;

    /* No threat if immune */
    if (streq(projections[context->type].threat, "immune"))
        return !rf_has(context->mon->race->flags, projections[context->type].threat_flag);

    /* A threat if vulnerable */
    if (streq(projections[context->type].threat, "vulnerable"))
        return rf_has(context->mon->race->flags, projections[context->type].threat_flag);

    /* Never a threat */
    if (streq(projections[context->type].threat, "never")) return false;

    /* A threat if living */
    if (streq(projections[context->type].threat, "living"))
        return !monster_is_nonliving(context->mon->race);

    return false;
}


/*
 * Called from project() to affect monsters
 *
 * Called for projections with the PROJECT_KILL flag set, which includes
 * bolt, beam, ball and breath effects.
 *
 * origin is the caster
 * r is the distance from the centre of the effect
 * c is the current cave
 * (y, x) the coordinates of the grid being handled
 * dam is the "damage" from the effect at distance r from the centre
 * typ is the projection (PROJ_) type
 * flg consists of any relevant PROJECT_ flags
 *
 * Returns whether the effects were obvious
 *
 * Note that this routine can handle "no damage" attacks (like teleport) by
 * taking a zero damage, and can even take parameters to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero. Note that actual damage should be already
 * adjusted for distance from the "epicenter" when passed in, but other effects
 * may be influenced by r.
 *
 * Note that "polymorph" is dangerous, since a failure in "place_monster()"'
 * may result in a dereference of an invalid pointer.  XXX XXX XXX
 *
 * Various messages are produced, and damage is applied.
 *
 * Just casting an element (e.g. plasma) does not make you immune, you must
 * actually be made of that substance, or breathe big balls of it.
 *
 * We assume that "Plasma" monsters, and "Plasma" breathers, are immune
 * to plasma.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less. If can breath nether, then it resists it as well.
 *
 * Damage reductions use the following formulas:
 *   Note that "dam = dam * 6 / (randint1(6) + 6);"
 *     gives avg damage of .655, ranging from .858 to .500
 *   Note that "dam = dam * 5 / (randint1(6) + 6);"
 *     gives avg damage of .544, ranging from .714 to .417
 *   Note that "dam = dam * 4 / (randint1(6) + 6);"
 *     gives avg damage of .444, ranging from .556 to .333
 *   Note that "dam = dam * 3 / (randint1(6) + 6);"
 *     gives avg damage of .327, ranging from .427 to .250
 *   Note that "dam = dam * 2 / (randint1(6) + 6);"
 *     gives something simple.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL. So,
 * to make a spell have no effect just set "note" to NULL. You should
 * also set "notice" to false, or the player will learn what the spell does.
 *
 * Note that this function determines if the player can see anything that
 * happens by taking into account: blindness, line-of-sight, and illumination.
 *
 * Hack -- effects on grids which are memorized but not in view are also seen.
 */
void project_m(struct source *origin, int r, struct chunk *c, struct loc *grid, int dam, int typ,
    int flg, bool *did_hit, bool *was_obvious, int *newy, int *newx)
{
    struct monster_race *race;
    bool mon_died = false;

    /* Is the effect obvious? */
    bool obvious = ((flg & PROJECT_AWARE)? true: false);

    /* Is the source an extra charming player? */
    bool charm = (origin->player? player_has(origin->player, PF_CHARM): false);

    int m_idx = square(c, grid)->mon;

    project_monster_handler_f monster_handler;
    project_monster_handler_context_t context;

    context.origin = origin;
    context.r = r;
    context.cave = c;
    loc_copy(&context.grid, grid);
    context.dam = dam;
    context.type = typ;
    context.seen = false;
    context.mon = NULL;
    context.lore = NULL;
    context.charm = charm;
    context.do_gravity = false;
    context.do_delete = false;
    context.obvious = obvious;
    context.skipped = false;
    context.flag = MON_TMD_FLG_NOTIFY;
    context.do_poly = 0;
    context.teleport_distance = 0;
    context.hurt_msg = MON_MSG_NONE;
    context.die_msg = MON_MSG_DIE;
    memset(context.mon_timed, 0, MON_TMD_MAX * sizeof(int));

    *did_hit = false;
    *was_obvious = false;
    *newy = grid->y;
    *newx = grid->x;

    /* Walls protect monsters */
    if (!square_ispassable(c, grid)) return;

    /* No monster here */
    if (m_idx <= 0) return;

    /* Obtain monster info */
    context.mon = cave_monster(c, m_idx);

    /* Never affect projector */
    if (context.mon == origin->monster) return;

    /* Obtain player info */
    if (origin->player)
    {
        context.lore = get_lore(origin->player, context.mon->race);
        context.seen = monster_is_visible(origin->player, m_idx);
    }

    /* Breathers may not blast members of the same race. */
    if (origin->monster && (flg & PROJECT_SAFE))
    {
        /* Skip monsters with the same race */
        if (origin->monster->race == context.mon->race) return;
    }

    /* Some monsters get "destroyed" */
    if (monster_is_destroyed(context.mon->race)) context.die_msg = MON_MSG_DESTROYED;

    /* The caster is a player */
    if (origin->player)
    {
        /* Check hostility for threatening spells */
        if (project_m_is_threat(&context) && !pvm_check(origin->player, context.mon)) return;
    }

    /* Force obviousness for certain types if seen. */
    if (projections[typ].obvious && context.seen) context.obvious = true;

    monster_handler = monster_handlers[typ];

    if (monster_handler != NULL)
        monster_handler(&context);
    else if (!projections[typ].obvious)
    {
        context.skipped = true;
        context.dam = 0;
    }

    /* Absolutely no effect */
    if (context.skipped) return;

    /* Apply damage to the monster, based on who did the damage. */
    if (origin->monster)
        mon_died = project_m_monster_attack(&context);
    else if (origin->trap)
        mon_died = project_m_trap_attack(&context);
    else if (origin->player)
        mon_died = project_m_player_attack(&context);

    /* Hack -- avoid a crash in case polymorph goes bad */
    if (!mon_died && project_m_apply_side_effects(&context, &m_idx))
    {
        *was_obvious = context.obvious;
        return;
    }

    /* Update locals again, since the project_m_* functions can change some values. */
    race = context.mon->race;
    obvious = context.obvious;

    /* Banish the monster */
    if (context.do_delete)
    {
        delete_monster_idx(c, m_idx);
        mon_died = true;
    }

    /* Update the monster */
    if (!mon_died) update_mon(context.mon, c, false);

    /* Redraw the (possibly new) monster grid */
    square_light_spot(c, &context.grid);

    /* Update monster recall window */
    if (origin->player)
    {
        struct actor_race *monster_race = &origin->player->upkeep->monster_race;

        /* Redraw */
        if (monster_race->race && (monster_race->race == race))
            origin->player->upkeep->redraw |= (PR_MONSTER);
    }

    /* Track it */
    *did_hit = true;
    *newy = context.grid.y;
    *newx = context.grid.x;

    /* Return "Anything seen?" */
    *was_obvious = obvious;
}


void monster_set_master(struct monster *mon, struct player *p, byte status)
{
    /* A high wisdom will allow more slaves to be controlled */
    if (p && (mon->status <= MSTATUS_SUMMONED))
    {
        int maxslaves = 1 + (1 + p->state.stat_ind[STAT_WIS]) / 4;

        if (p->slaves == maxslaves)
        {
            msg(p, "You cannot control that many monsters!");
            return;
        }
    }

    mon->master = (p? p->id: 0);
    mon->lifespan = (p? mon->level + 5 + randint1(5): 0);
    mon->resilient = 0;
    if (p && (mon->status <= MSTATUS_SUMMONED)) p->slaves++;
    mon->status = status;
}


/*
 * Stat Table (WIS) -- chance of getting a friendly summon
 */
static const byte summon_friendly[STAT_RANGE] =
{
    0   /* 3 */,
    4   /* 4 */,
    8   /* 5 */,
    12  /* 6 */,
    16  /* 7 */,
    20  /* 8 */,
    23  /* 9 */,
    26  /* 10 */,
    29  /* 11 */,
    32  /* 12 */,
    35  /* 13 */,
    38  /* 14 */,
    41  /* 15 */,
    44  /* 16 */,
    47  /* 17 */,
    50  /* 18/00-18/09 */,
    52  /* 18/10-18/19 */,
    54  /* 18/20-18/29 */,
    56  /* 18/30-18/39 */,
    58  /* 18/40-18/49 */,
    60  /* 18/50-18/59 */,
    62  /* 18/60-18/69 */,
    64  /* 18/70-18/79 */,
    66  /* 18/80-18/89 */,
    68  /* 18/90-18/99 */,
    70  /* 18/100-18/109 */,
    72  /* 18/110-18/119 */,
    74  /* 18/120-18/129 */,
    76  /* 18/130-18/139 */,
    78  /* 18/140-18/149 */,
    80  /* 18/150-18/159 */,
    83  /* 18/160-18/169 */,
    86  /* 18/170-18/179 */,
    89  /* 18/180-18/189 */,
    92  /* 18/190-18/199 */,
    95  /* 18/200-18/209 */,
    98  /* 18/210-18/219 */,
    100 /* 18/220+ */
};


bool can_charm_monster(struct player *p)
{
    return (magik(summon_friendly[p->state.stat_ind[STAT_WIS]]));
}


int charm_monster(struct monster *mon, struct player *p, byte status)
{
    bool charmed = false;

    /* Paranoia */
    if (!p) return MON_MSG_UNAFFECTED;

    /* Only if the monster has been summoned */
    if (mon->status == MSTATUS_HOSTILE) return MON_MSG_UNAFFECTED;

    /* Uniques are unaffected */
    if (monster_is_unique(mon->race)) return MON_MSG_UNAFFECTED;

    /* Too enraged to be controlled */
    if (player_of_has(p, OF_AGGRAVATE)) return MON_MSG_HATE;

    /* A high level will help a lot */
    if (!CHANCE(MAX(mon->level - 5, 1), p->lev * 5)) charmed = true;

    /* A high wisdom will help a lot, and will always yield useful summons */
    if (can_charm_monster(p))
    {
        charmed = true;
        status = MSTATUS_ATTACK;
    }

    /* Only if the monster is not already under the spell */
    if (mon->status >= status) return MON_MSG_UNAFFECTED;

    /* Monster is pacified */
    if (charmed)
    {
        monster_set_master(mon, p, status);
        return MON_MSG_REACT;
    }

    /* Monster stays hostile */
    return MON_MSG_UNAFFECTED;
}
