/*
 * File: project-player.c
 * Purpose: Projection effects on players
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
 * Is player susceptible to an attack?
 *
 * true means normal damage, false means no damage
 */
static bool is_susceptible(struct monster_race *race, int type)
{
    switch (type)
    {
        /* Check these attacks against vulnerability (polymorphed players) */
        case GF_LIGHT_WEAK: return (race && rf_has(race->flags, RF_HURT_LIGHT));
        case GF_KILL_WALL: return (race && rf_has(race->flags, RF_HURT_ROCK));
        case GF_DISP_EVIL: return (race && rf_has(race->flags, RF_EVIL));
        case GF_DISP_UNDEAD: return (race && rf_has(race->flags, RF_UNDEAD));

        /* Check these attacks against immunity (polymorphed players) */
        case GF_PSI_DRAIN:
        case GF_OLD_DRAIN:
        case GF_DRAIN: return !(race && monster_is_nonliving(race));

        /* Everything else will cause normal damage */
        default: return true;
    }
}


/*
 * Is player vulnerable to an attack?
 *
 * true means extra damage, false means normal damage
 */
static bool is_vulnerable(struct monster_race *race, int type)
{
    switch (type)
    {
        /* Check these attacks against vulnerability (polymorphed players) */
        case GF_FIRE: return (race && rf_has(race->flags, RF_HURT_FIRE));
        case GF_COLD:
        case GF_ICE: return (race && rf_has(race->flags, RF_HURT_COLD));
        case GF_LIGHT: return (race && rf_has(race->flags, RF_HURT_LIGHT));

        /* Everything else will cause normal damage */
        default: return false;
    }
}


/*
 * Adjust damage according to resistance or vulnerability.
 *
 * type is the attack type we are checking.
 * dam is the unadjusted damage.
 * dam_aspect is the calc we want (min, avg, max, random).
 * resist is the degree of resistance (-1 = vuln, 3 = immune).
 */
int adjust_dam(struct player *p, int type, int dam, aspect dam_aspect, int resist)
{
    int i, denom = 0;

    /* If an actual player exists, get their actual resist */
    if (p && (type < ELEM_MAX))
    {
        /* Hack -- ice damage checks against cold resistance */
        int res_type = ((type == GF_ICE)? GF_COLD: type);

        resist = p->state.el_info[res_type].res_level;

        /* Notice element stuff */
        equip_notice_element(p, res_type);
    }

    if (dam <= 0) return 0;

    /* Immune */
    if (resist == 3) return 0;

    /* Hack -- acid damage is halved by armour, holy orb is halved */
    if (((type == GF_ACID) && p && minus_ac(p)) || (type == GF_HOLY_ORB))
        dam = (dam + 1) / 2;

    /* Hack -- biofeedback halves "sharp" damage */
    if (p && p->timed[TMD_BIOFEEDBACK])
    {
        switch (type)
        {
            case GF_MISSILE:
            case GF_ARROW_X:
            case GF_ARROW_1:
            case GF_ARROW_2:
            case GF_ARROW_3:
            case GF_ARROW_4:
            case GF_BOULDER:
            case GF_SHARD:
            case GF_SOUND:
                dam = (dam + 1) / 2;
                break;
        }
    }

    /* Hack -- no damage from certain attacks unless vulnerable */
    if (p && !is_susceptible(p->poly_race, type)) dam = 0;

    /* Hack -- extra damage from certain attacks if vulnerable */
    if (p && is_vulnerable(p->poly_race, type)) dam = dam * 4 / 3;

    /* Vulnerable */
    if (resist == -1) return (dam * 4 / 3);

    /*
     * Variable resists vary the denominator, so we need to invert the logic
     * of dam_aspect. (m_bonus is unused)
     */
    switch (dam_aspect)
    {
        case MINIMISE:
            denom = randcalc(gf_denom(type), 0, MAXIMISE);
            break;
        case MAXIMISE:
            denom = randcalc(gf_denom(type), 0, MINIMISE);
            break;
        case AVERAGE:
        case RANDOMISE:
            denom = randcalc(gf_denom(type), 0, dam_aspect);
            break;
    }

    for (i = resist; i > 0; i--)
    {
        if (denom) dam = dam * gf_num(type) / denom;
    }

    return dam;
}


/*
 * Player handlers
 */


/*
 * Drain stats at random
 *
 * p is the affected player
 * num is the number of points to drain
 */
static void project_player_drain_stats(struct player *p, int num)
{
    int i, k = 0;
    const char *act = NULL;

    for (i = 0; i < num; i++)
    {
        switch (randint1(STAT_MAX))
        {
            case 1: k = STAT_STR; act = "strong"; break;
            case 2: k = STAT_INT; act = "bright"; break;
            case 3: k = STAT_WIS; act = "wise"; break;
            case 4: k = STAT_DEX; act = "agile"; break;
            case 5: k = STAT_CON; act = "hale"; break;
        }

        msg(p, "You're not as %s as you used to be...", act);
        player_stat_dec(p, k, false);
    }
}


/*
 * Swap a random pair of stats
 */
void project_player_swap_stats(struct player *p)
{
    int max1, cur1, max2, cur2, ii, jj;

    msg(p, "Your body starts to scramble...");

    /* Pick a pair of stats */
    ii = randint0(STAT_MAX);
    for (jj = ii; jj == ii; jj = randint0(STAT_MAX)) /* loop */;

    max1 = p->stat_max[ii];
    cur1 = p->stat_cur[ii];
    max2 = p->stat_max[jj];
    cur2 = p->stat_cur[jj];

    p->stat_max[ii] = max2;
    p->stat_cur[ii] = cur2;
    p->stat_max[jj] = max1;
    p->stat_cur[jj] = cur1;

    p->upkeep->update |= (PU_BONUS);
}


void project_player_time_effects(struct player *p, struct actor *who)
{
    /* Life draining */
    if (one_in_(2))
    {
        int drain = 100 + (p->exp / 100) * z_info->life_drain_percent;

        msg(p, "You feel your life force draining away!");
        player_exp_lose(p, drain, false);
    }

    /* Drain some stats */
    else if (!one_in_(5))
    {
        int num = 1;

        /* PWMAngband: time resistance prevents nastiest effect */
        if (!player_resists(p, ELEM_TIME)) num = 2;

        project_player_drain_stats(p, num);
    }

    /* Drain all stats */
    else
    {
        int perma = p->state.el_info[ELEM_TIME].res_level;

        if (p->timed[TMD_ANCHOR]) perma--;

        if (who->mon)
            update_smart_learn(who->mon, p, 0, 0, ELEM_TIME);

        /* PWMAngband: permanent time resistance prevents the effect completely */
        if (perma)
            msg(p, "You resist the effect!");

        /* PWMAngband: space/time anchor prevents nastiest effect */
        else if (p->timed[TMD_ANCHOR])
        {
            /* Life draining */
            if (randint1(9) < 6)
            {
                msg(p, "You feel your life force draining away!");
                player_exp_lose(p, 100 + (p->exp / 100) * z_info->life_drain_percent, false);
            }

            /* Drain one stat */
            else
                project_player_drain_stats(p, 1);
        }

        /* Normal case */
        else
        {
            int i;

            msg(p, "You're not as powerful as you used to be...");
            for (i = 0; i < STAT_MAX; i++) player_stat_dec(p, i, false);
        }
    }
}


typedef struct project_player_handler_context_s
{
    struct actor *who;
    int r;
    struct chunk *cave;
    int y;
    int x;
    int dam;
    int type;
    bool obvious;
} project_player_handler_context_t;


typedef void (*project_player_handler_f)(project_player_handler_context_t *);


static void project_player_handler_ACID(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, 0, 0, ELEM_ACID);
    if (player_is_immune(p, ELEM_ACID))
    {
        msg(p, "You resist the effect!");
        return;
    }

    inven_damage(p, GF_ACID, MIN(context->dam * 5, 300));
}


static void project_player_handler_ELEC(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, 0, 0, ELEM_ELEC);
    if (player_is_immune(p, ELEM_ELEC))
    {
        msg(p, "You resist the effect!");
        return;
    }

    inven_damage(p, GF_ELEC, MIN(context->dam * 5, 300));
}


static void project_player_handler_FIRE(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, 0, 0, ELEM_FIRE);
    if (player_is_immune(p, ELEM_FIRE))
    {
        msg(p, "You resist the effect!");
        return;
    }

    inven_damage(p, GF_FIRE, MIN(context->dam * 5, 300));
}


static void project_player_handler_COLD(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, 0, 0, ELEM_COLD);
    if (player_is_immune(p, ELEM_COLD))
    {
        msg(p, "You resist the effect!");
        return;
    }

    inven_damage(p, GF_COLD, MIN(context->dam * 5, 300));
}


static void project_player_handler_POIS(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (player_resists(p, ELEM_POIS))
    {
        msg(p, "You resist the effect!");
        return;
    }

    player_inc_timed(p, TMD_POISONED, 10 + randint1(context->dam), true, check);
}


static void project_player_handler_LIGHT(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, OF_PROT_BLIND, 0, -1);
    if (player_resists(p, ELEM_LIGHT) || player_of_has(p, OF_PROT_BLIND))
    {
        msg(p, "You resist the effect!");
        return;
    }

    player_inc_timed(p, TMD_BLIND, 2 + randint1(5), true, check);
}


static void project_player_handler_DARK(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, OF_PROT_BLIND, 0, -1);
    if (player_resists(p, ELEM_DARK) || player_of_has(p, OF_PROT_BLIND))
    {
        msg(p, "You resist the effect!");
        return;
    }

    player_inc_timed(p, TMD_BLIND, 2 + randint1(5), true, check);
}


static void project_player_handler_SOUND(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, OF_PROT_STUN, 0, -1);
    if (player_resists(p, ELEM_SOUND) || player_of_has(p, OF_PROT_STUN))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Stun */
    player_inc_timed(p, TMD_STUN, MIN(5 + randint1(context->dam / 3), 35), true, check);
}


static void project_player_handler_SHARD(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (player_resists(p, ELEM_SHARD))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Cuts */
    player_inc_timed(p, TMD_CUT, randint1(context->dam), true, check);
}


static void project_player_handler_NEXUS(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (player_resists(p, ELEM_NEXUS))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Stat swap */
    if (one_in_(7))
    {
        if (randint0(100) < p->state.skills[SKILL_SAVE])
        {
            msg(p, "You avoid the effect!");
            return;
        }
        project_player_swap_stats(p);
    }

    /* Teleport to */
    else if (one_in_(3))
    {
        if (context->who->mon)
            effect_simple(p, EF_TELEPORT_TO, "0", 0, 0, 0, NULL, context->who->mon);
        else
        {
            effect_simple(p, EF_TELEPORT_TO, "0", context->who->player->py,
                context->who->player->px, 0, NULL, NULL);
        }

        /* Hack -- get new location */
        context->y = p->py;
        context->x = p->px;
    }

    /* Teleport level */
    else if (one_in_(4))
    {
        if (randint0(100) < p->state.skills[SKILL_SAVE])
        {
            msg(p, "You avoid the effect!");
            return;
        }
        effect_simple(p, EF_TELEPORT_LEVEL, "0", 0, 0, 0, NULL, NULL);
    }

    /* Teleport */
    else
    {
        effect_simple(p, EF_TELEPORT, "200", 0, 0, 0, NULL, NULL);

        /* Hack -- get new location */
        context->y = p->py;
        context->x = p->px;
    }
}


static void project_player_handler_NETHER(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    int drain = 20 + (p->exp / 50) * z_info->life_drain_percent;

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, OF_HOLD_LIFE, 0, -1);
    if (player_resists(p, ELEM_NETHER) || player_of_has(p, OF_HOLD_LIFE))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Life draining */
    msg(p, "You feel your life force draining away!");
    player_exp_lose(p, drain, false);
}


static void project_player_handler_CHAOS(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (context->who->mon)
    {
        update_smart_learn(context->who->mon, p, OF_PROT_CONF, 0, -1);
        update_smart_learn(context->who->mon, p, OF_HOLD_LIFE, 0, -1);
    }
    if (player_resists(p, ELEM_CHAOS))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Hallucination */
    player_inc_timed(p, TMD_IMAGE, randint1(10), true, check);

    /* Confusion */
    if (player_of_has(p, OF_PROT_CONF))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_CONFUSED, 10 + randint0(20), true, check);

    /* Life draining */
    if (player_of_has(p, OF_HOLD_LIFE))
        msg(p, "You resist the effect!");
    else
    {
        int drain = 50 + (p->exp / 50) * z_info->life_drain_percent;

        msg(p, "You feel your life force draining away!");
        player_exp_lose(p, drain, false);
    }
}


static void project_player_handler_DISEN(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (player_resists(p, ELEM_DISEN))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Disenchant gear */
    effect_simple(p, EF_DISENCHANT, "0", 0, 0, 0, NULL, NULL);
}


static void project_player_handler_WATER(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (context->who->mon)
    {
        update_smart_learn(context->who->mon, p, OF_PROT_CONF, 0, -1);
        update_smart_learn(context->who->mon, p, OF_PROT_STUN, 0, -1);
    }

    /* Confusion */
    if (player_of_has(p, OF_PROT_CONF))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_CONFUSED, 5 + randint1(5), true, check);

    /* Stun */
    if (player_of_has(p, OF_PROT_STUN))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_STUN, randint1(40), true, check);
}


static void project_player_handler_ICE(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (context->who->mon)
    {
        update_smart_learn(context->who->mon, p, 0, 0, ELEM_SHARD);
        update_smart_learn(context->who->mon, p, OF_PROT_STUN, 0, -1);
        update_smart_learn(context->who->mon, p, 0, 0, ELEM_COLD);
    }

    if (player_is_immune(p, ELEM_COLD))
        msg(p, "You resist the effect!");
    else
        inven_damage(p, GF_COLD, MIN(context->dam * 5, 300));

    /* Cuts */
    if (player_resists(p, ELEM_SHARD))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_CUT, damroll(5, 8), true, check);

    /* Stun */
    if (player_of_has(p, OF_PROT_STUN))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_STUN, randint1(15), true, check);
}


static void project_player_handler_GRAVITY(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    msg(p, "Gravity warps around you.");

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, OF_PROT_STUN, 0, -1);

    /* Blink */
    if (randint1(127) > p->lev)
    {
        effect_simple(p, EF_TELEPORT, "5", 0, 0, 0, NULL, NULL);

        /* Hack -- get new location */
        context->y = p->py;
        context->x = p->px;
    }

    /* Slow */
    player_inc_timed(p, TMD_SLOW, 4 + randint0(4), true, check);

    /* Stun */
    if (player_of_has(p, OF_PROT_STUN))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_STUN, MIN(5 + randint1(context->dam / 3), 35), true, check);
}


static void project_player_handler_INERT(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    /* Slow */
    player_inc_timed(p, TMD_SLOW, 4 + randint0(4), true, check);
}


static void project_player_handler_FORCE(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);
    char grids_away[5];
    int c_y, c_x;

    /* Get location of caster (assumes index of caster is not zero) */
    if (context->who->mon)
    {
        c_y = context->who->mon->fy;
        c_x = context->who->mon->fx;
    }
    else
    {
        c_y = context->who->player->py;
        c_x = context->who->player->px;
    }

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, OF_PROT_STUN, 0, -1);
    if (player_of_has(p, OF_PROT_STUN))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Stun */
    player_inc_timed(p, TMD_STUN, randint1(20), true, check);

    /* Thrust player away. */
    strnfmt(grids_away, sizeof(grids_away), "%d", 3 + context->dam / 20);
    effect_simple(p, EF_THRUST_AWAY, grids_away, c_y, c_x, 0, NULL, NULL);

    /* Hack -- get new location */
    context->y = p->py;
    context->x = p->px;
}


static void project_player_handler_TIME(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    project_player_time_effects(p, context->who);
}


static void project_player_handler_PLASMA(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool check = (context->who->mon? false: true);

    if (context->who->mon)
        update_smart_learn(context->who->mon, p, OF_PROT_STUN, 0, -1);
    if (player_of_has(p, OF_PROT_STUN))
    {
        msg(p, "You resist the effect!");
        return;
    }

    /* Stun */
    player_inc_timed(p, TMD_STUN, MIN(5 + randint1(context->dam * 3 / 4), 35), true, check);
}


static void project_player_handler_METEOR(project_player_handler_context_t *context) {}
static void project_player_handler_MISSILE(project_player_handler_context_t *context) {}
static void project_player_handler_MANA(project_player_handler_context_t *context) {}
static void project_player_handler_HOLY_ORB(project_player_handler_context_t *context) {}
static void project_player_handler_ARROW_X(project_player_handler_context_t *context) {}
static void project_player_handler_ARROW_1(project_player_handler_context_t *context) {}
static void project_player_handler_ARROW_2(project_player_handler_context_t *context) {}
static void project_player_handler_ARROW_3(project_player_handler_context_t *context) {}
static void project_player_handler_ARROW_4(project_player_handler_context_t *context) {}
static void project_player_handler_BOULDER(project_player_handler_context_t *context) {}


/* PvP handlers */


static void project_player_handler_AWAY_EVIL(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Only evil players */
    if (p->poly_race && rf_has(p->poly_race->flags, RF_EVIL))
    {
        char dice[5];

        strnfmt(dice, sizeof(dice), "%d", context->dam);
        effect_simple(p, EF_TELEPORT, dice, 0, 0, 0, NULL, NULL);

        /* Hack -- get new location */
        context->y = p->py;
        context->x = p->px;
    }
    else
        msg(p, "You resist the effect!");
}


static void project_player_handler_AWAY_ALL(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    char dice[5];

    strnfmt(dice, sizeof(dice), "%d", context->dam);
    effect_simple(p, EF_TELEPORT, dice, 0, 0, 0, NULL, NULL);

    /* Hack -- get new location */
    context->y = p->py;
    context->x = p->px;
}


static void project_player_handler_TURN_UNDEAD(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Only undead players */
    if (p->poly_race && rf_has(p->poly_race->flags, RF_UNDEAD))
    {
        /* Fear */
        if (player_of_has(p, OF_PROT_FEAR))
            msg(p, "You resist the effect!");
        else
            player_inc_timed(p, TMD_AFRAID, 3 + randint1(4), true, true);
    }
    else
        msg(p, "You resist the effect!");
}


static void project_player_handler_TURN_ALL(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Fear */
    if (player_of_has(p, OF_PROT_FEAR))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_AFRAID, 3 + randint1(4), true, true);
}


static void project_player_handler_DISP_UNDEAD(project_player_handler_context_t *context) {}
static void project_player_handler_DISP_EVIL(project_player_handler_context_t *context) {}
static void project_player_handler_DISP_ALL(project_player_handler_context_t *context) {}


static void project_player_handler_OLD_CLONE(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Disable */
    msg(p, "You resist the effect!");
}


static void project_player_handler_OLD_POLY(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (player_resists(p, ELEM_NEXUS))
        msg(p, "You resist the effect!");

    /* Swap stats */
    else if (one_in_(2))
    {
        if (randint0(100) < p->state.skills[SKILL_SAVE])
            msg(p, "You avoid the effect!");
        else
            project_player_swap_stats(p);
    }

    /* Poly bat */
    else
    {
        char killer[NORMAL_WID];

        my_strcpy(killer, context->who->player->name, sizeof(killer));
        poly_bat(p, 10 + context->dam * 4, killer);
    }
}


static void project_player_handler_OLD_HEAL(project_player_handler_context_t *context)
{
    project_player_handler_OLD_CLONE(context);
}


static void project_player_handler_OLD_SPEED(project_player_handler_context_t *context)
{
    project_player_handler_OLD_CLONE(context);
}


static void project_player_handler_OLD_SLOW(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Slow */
    if (player_of_has(p, OF_FREE_ACT))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_SLOW, 3 + randint1(4), true, true);
}


static void project_player_handler_OLD_CONF(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Confusion */
    if (player_of_has(p, OF_PROT_CONF))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_CONFUSED, 3 + randint1(4), true, true);
}


static void project_player_handler_OLD_SLEEP(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Paralysis */
    if (player_of_has(p, OF_FREE_ACT))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_PARALYZED, 3 + randint1(4), true, true);
}


static void project_player_handler_OLD_DRAIN(project_player_handler_context_t *context) {}


static void project_player_handler_PSI(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    switch (randint1(4))
    {
        /* Confusion */
        case 1:
        {
            if (player_of_has(p, OF_PROT_CONF))
                msg(p, "You resist the effect!");
            else
                player_inc_timed(p, TMD_CONFUSED, 3 + randint1(4), true, true);
            break;
        }

        /* Stun */
        case 2:
        {
            if (player_of_has(p, OF_PROT_STUN))
                msg(p, "You resist the effect!");
            else
                player_inc_timed(p, TMD_STUN, 3 + randint1(4), true, true);
            break;
        }

        /* Fear */
        case 3:
        {
            if (player_of_has(p, OF_PROT_FEAR))
                msg(p, "You resist the effect!");
            else
                player_inc_timed(p, TMD_AFRAID, 3 + randint1(4), true, true);
            break;
        }

        /* Paralysis */
        default:
        {
            if (player_of_has(p, OF_FREE_ACT))
                msg(p, "You resist the effect!");
            else
                player_inc_timed(p, TMD_PARALYZED, 3 + randint1(4), true, true);
            break;
        }
    }
}


static void project_player_handler_OLD_STUN(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Stun */
    if (player_of_has(p, OF_PROT_STUN))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_STUN, 3 + randint1(4), true, true);
}


static void project_player_handler_DEATH(project_player_handler_context_t *context) {}
static void project_player_handler_PSI_DRAIN(project_player_handler_context_t *context) {}
static void project_player_handler_CURSE(project_player_handler_context_t *context) {}


static void project_player_handler_CURSE2(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Cuts */
    player_inc_timed(p, TMD_CUT, damroll(10, 10), true, true);
}


static void project_player_handler_DRAIN(project_player_handler_context_t *context) {}
static void project_player_handler_GUARD(project_player_handler_context_t *context) {}
static void project_player_handler_FOLLOW(project_player_handler_context_t *context) {}


static void project_player_handler_TELE_TO(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    effect_simple(p, EF_TELEPORT_TO, "0", context->who->player->py, context->who->player->px, 0,
        NULL, NULL);

    /* Hack -- get new location */
    context->y = p->py;
    context->x = p->px;
}


static void project_player_handler_TELE_LEVEL(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    if (player_resists(p, ELEM_NEXUS))
        msg(p, "You resist the effect!");
    else
        effect_simple(p, EF_TELEPORT_LEVEL, "0", 0, 0, 0, NULL, NULL);
}


static void project_player_handler_OLD_BLIND(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Blindness */
    if (player_of_has(p, OF_PROT_BLIND))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_BLIND, 11 + randint1(4), true, true);
}


static void project_player_handler_DRAIN_MANA(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);
    bool seen = (!p->timed[TMD_BLIND] &&
        mflag_has(p->pflag[context->who->idx], MFLAG_VISIBLE));

    drain_mana(p, context->who, (randint1(context->dam) / 2) + 1, seen);
}


static void project_player_handler_FORGET(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Amnesia */
    player_inc_timed(p, TMD_AMNESIA, 8, true, true);
}


static void project_player_handler_BLAST(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Confusion */
    if (player_of_has(p, OF_PROT_CONF))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_CONFUSED, 3 + randint1(4), true, true);

    /* Amnesia */
    player_inc_timed(p, TMD_AMNESIA, 4, true, true);
}


static void project_player_handler_SMASH(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    /* Slow */
    player_inc_timed(p, TMD_SLOW, 3 + randint1(4), true, true);

    /* Blindness */
    if (player_of_has(p, OF_PROT_BLIND))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_BLIND, 7 + randint1(8), true, true);

    /* Confusion */
    if (player_of_has(p, OF_PROT_CONF))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_CONFUSED, 3 + randint1(4), true, true);

    /* Paralysis */
    if (player_of_has(p, OF_FREE_ACT))
        msg(p, "You resist the effect!");
    else
        player_inc_timed(p, TMD_PARALYZED, 3 + randint1(4), true, true);

    /* Amnesia */
    player_inc_timed(p, TMD_AMNESIA, 4, true, true);
}


static void project_player_handler_ATTACK(project_player_handler_context_t *context) {}
static void project_player_handler_CONTROL(project_player_handler_context_t *context) {}


static void project_player_handler_PROJECT(project_player_handler_context_t *context)
{
    struct player *p = player_get(0 - context->cave->squares[context->y][context->x].mon);

    int cidx = context->who->player->clazz->cidx;

    if (context->who->player->ghost && !player_can_undead(context->who->player))
        cidx = CLASS_GHOST;

    /* "dam" is used as spell index */
    cast_spell_proj(p, cidx, context->dam, false);
}


static const project_player_handler_f player_handlers[] =
{
    #define ELEM(a, b, c, d, e, f, g, h, col, pvp) project_player_handler_##a,
    #include "../common/list-elements.h"
    #undef ELEM
    #define PROJ_ENV(a, b, obv, col, desc, pvp) NULL,
    #include "../common/list-project-environs.h"
    #undef PROJ_ENV
    #define PROJ_MON(a, b, obv, col, desc, pvp) project_player_handler_##a,
    #include "../common/list-project-monsters.h"
    #undef PROJ_MON
    NULL
};


static bool project_p_is_threat(int type)
{
    bool threat = false;

    /* Is this type of attack a threat? */
    switch (type)
    {
        case GF_AWAY_ALL:
        case GF_PROJECT:
        case GF_TELE_TO:
        case GF_TELE_LEVEL: break;
        default: threat = true; break;
    }

    return threat;
}


/*
 * Called from project() to affect the player
 *
 * Called for projections with the PROJECT_PLAY flag set, which includes
 * bolt, beam, ball and breath effects.
 *
 * who is the caster
 * r is the distance from the centre of the effect
 * c is the current cave
 * (y, x) the coordinates of the grid being handled
 * dam is the "damage" from the effect at distance r from the centre
 * typ is the projection (GF_) type
 * what is the message to other players if the target dies
 * did_hit is true if a player was hit
 * was_obvious is true if the effects were obvious
 *
 * If "r" is non-zero, then the blast was centered elsewhere; the damage
 * is reduced in project() before being passed in here. This can happen if a
 * monster breathes at the player and hits a wall instead.
 *
 * We assume the player is aware of some effect, and always return "true".
 */
void project_p(struct actor *who, int r, struct chunk *c, int y, int x, int dam, int typ,
    const char *what, bool *did_hit, bool *was_obvious, int *newy, int *newx)
{
    bool blind, seen;
    bool obvious = true;
    bool dead = false;

    /* Monster name (for attacks) */
    char m_name[NORMAL_WID];

    /* Monster name (for damage) */
    char killer[NORMAL_WID];

    project_player_handler_f player_handler;
    project_player_handler_context_t context;

    /* Projected spell */
    int index = dam;

    struct player *p = player_get(0 - c->squares[y][x].mon);

    *did_hit = false;
    *was_obvious = false;
    *newy = y;
    *newx = x;

    /* No player here */
    if (!p) return;

    /* Never affect projector (except when trying to polymorph self) */
    if ((who->player == p) && (typ != GF_OLD_POLY)) return;

    /* Obtain player info */
    blind = (p->timed[TMD_BLIND]? true: false);
    if (who->mon)
        seen = (!blind && mflag_has(p->mflag[who->idx], MFLAG_VISIBLE));
    else
        seen = (!blind && mflag_has(p->pflag[who->idx], MFLAG_VISIBLE));

    /* Hack -- polymorph self */
    if (who->player == p) {}

    /* The caster is a monster */
    else if (who->mon)
    {
        /* Get the monster name */
        monster_desc(p, m_name, sizeof(m_name), who->mon, MDESC_CAPITAL);

        /* Get the monster's real name */
        monster_desc(p, killer, sizeof(killer), who->mon, MDESC_DIED_FROM);

        /* Check hostility for threatening spells */
        if (!pvm_check(p, who->mon)) return;
    }

    /* The caster is a player */
    else if (who->player)
    {
        /* Check hostility for threatening spells */
        if (project_p_is_threat(typ))
        {
            struct actor target_body;
            struct actor *target = &target_body;
            int mode;

            ACTOR_PLAYER(target, 0, p);
            mode = (target_equals(who->player, target)? PVP_DIRECT: PVP_INDIRECT);

            if (!pvp_check(who->player, p, mode, true, c->squares[y][x].feat))
                return;
        }

        player_desc(p, m_name, sizeof(m_name), who->player, true);
        my_strcpy(killer, who->player->name, sizeof(killer));
    }

    /* Let player know what is going on */
    if (!seen && gf_blind_desc(typ)) msg(p, "You %s!", gf_blind_desc(typ));

    /* MvP */
    if (who->mon)
    {
        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was %s by %s", what, killer);

        /* Adjust damage for resistance, immunity or vulnerability, and apply it */
        dam = adjust_dam(p, typ, dam, RANDOMISE, 0);
        if (dam) dead = take_hit(p, dam, killer, true);
    }

    /* PvP */
    else
    {
        /* Try a saving throw if available */
        if ((gf_flags(typ) & ATT_SAVE) && magik(p->state.skills[SKILL_SAVE]))
        {
            msg(p, "You resist the effects!");

            /* Hack */
            dead = true;
        }
        else
        {
            bool non_physical = ((gf_flags(typ) & ATT_NON_PHYS)? true: false);

            strnfmt(p->died_flavor, sizeof(p->died_flavor), "was %s by %s", what, killer);

            /* Adjust damage for resistance, immunity or vulnerability, and apply it */
            dam = adjust_dam(p, typ, dam, RANDOMISE, 0);
            if (dam && (gf_flags(typ) & ATT_DAMAGE))
                dead = take_hit(p, dam, killer, non_physical);

            /* Give a message */
            if (dam && (gf_flags(typ) & ATT_DAMAGE) && !dead)
                player_pain(who->player, p, dam);

            /* Projected spell */
            if (gf_flags(typ) & ATT_RAW) dam = index;
        }
    }

    context.who = who;
    context.r = r;
    context.cave = c;
    context.y = y;
    context.x = x;
    context.dam = dam;
    context.type = typ;
    context.obvious = obvious;

    player_handler = player_handlers[typ];

    /* Handle side effects */
    if ((player_handler != NULL) && !dead) player_handler(&context);

    obvious = context.obvious;

    /* Disturb */
    disturb(p, 1);

    /* Track this player */
    *did_hit = true;
    *newy = context.y;
    *newx = context.x;

    /* Return "Anything seen?" */
    *was_obvious = obvious;
}