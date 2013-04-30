/*
 * File: effects.c
 * Purpose: Big switch statement for every effect in the game
 *
 * Copyright (c) 2007 Andrew Sidwell
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
#include "effects.h"
#include "monster/mon-spell.h"
#include "monster/mon-util.h"
#include "s-spells.h"


/*
 * Entries for effect descriptions
 */
typedef struct
{
    u16b index;         /* Effect index */
    bool aim;           /* Whether the effect requires aiming */
    u16b power;         /* Power rating for obj-power.c */
    const char *desc;   /* Effect description */
} info_entry;


/*
 * Useful things about effects.
 */
static const info_entry effects[] =
{
    #define EFFECT(x, y, r, z) {EF_##x, y, r, z},
    #include "list-effects.h"
    #undef EFFECT
    {0}
};


/*
 * Utility functions
 */
bool effect_aim(effect_type effect)
{
    if ((effect < 1) || (effect > EF_MAX)) return FALSE;
    return effects[effect].aim;
}


u16b effect_power(effect_type effect)
{
    if ((effect < 1) || (effect > EF_MAX)) return 0;
    return effects[effect].power;
}


const char *effect_desc(effect_type effect)
{
    if ((effect < 1) || (effect > EF_MAX)) return NULL;
    return effects[effect].desc;
}


bool effect_obvious(effect_type effect)
{
    if (effect == EF_IDENTIFY) return TRUE;
    return FALSE;
}


/*
 * The "wonder" effect.
 *
 * Returns TRUE if the effect is evident.
 */
bool effect_wonder(struct player *p, int dir, int die, int beam)
{
    /*
     * This spell should become more useful (more controlled) as the player
     * gains experience levels. Thus, add 1/5 of the player's level to the die roll.
     * This eliminates the worst effects later on, while keeping the results quite
     * random. It also allows some potent effects only at high level.
     */

    bool visible = FALSE;
    int py = p->py;
    int px = p->px;
    int plev = p->lev;

    if (die > 100) msg(p, "You feel a surge of power!");

    if (die < 8) visible = clone_monster(p, dir);
    else if (die < 14) visible = speed_monster(p, dir);
    else if (die < 26) visible = heal_monster(p, dir);
    else if (die < 31) visible = poly_monster(p, dir, FALSE);
    else if (die < 36)
    {
        msg_spell(p, "fires a magic missile.");
        visible = fire_bolt_or_beam(p, beam - 10, GF_MISSILE, dir, damroll(3 + (plev - 1) / 5, 4));
    }
    else if (die < 41) confuse_monster(p, dir, plev, FALSE);
    else if (die < 46)
    {
        msg_spell(p, " casts a noxious cloud.");
        visible = fire_ball(p, GF_POIS, dir, 20 + plev / 2, 3);
    }
    else if (die < 51)
    {
        msg(p, "A line of blue shimmering light appears.");
        visible = light_line(p, dir);
    }
    else if (die < 56)
    {
        msg_spell(p, " casts a lightning bolt.");
        visible = fire_beam(p, GF_ELEC, dir, damroll(3 + (plev - 5) / 6, 6));
    }
    else if (die < 61)
    {
        msg_spell(p, " casts a frost bolt.");
        visible = fire_bolt_or_beam(p, beam - 10, GF_COLD, dir, damroll(5 + (plev - 5) / 4, 8));
    }
    else if (die < 66)
    {
        msg_spell(p, " casts an acid bolt.");
        visible = fire_bolt_or_beam(p, beam, GF_ACID, dir, damroll(6 + (plev - 5) / 4, 8));
    }
    else if (die < 71)
    {
        msg_spell(p, " casts a fire bolt.");
        visible = fire_bolt_or_beam(p, beam, GF_FIRE, dir, damroll(8 + (plev - 5) / 4, 8));
    }
    else if (die < 76) visible = drain_life(p, dir, 75);
    else if (die < 81)
    {
        msg_spell(p, " casts a lightning ball.");
        visible = fire_ball(p, GF_ELEC, dir, 30 + plev / 2, 2);
    }
    else if (die < 86)
    {
        msg_spell(p, " casts an acid ball.");
        visible = fire_ball(p, GF_ACID, dir, 40 + plev, 2);
    }
    else if (die < 91)
    {
        msg_spell(p, " casts an ice ball.");
        visible = fire_ball(p, GF_ICE, dir, 70 + plev, 3);
    }
    else if (die < 96)
    {
        msg_spell(p, " casts a fire ball.");
        visible = fire_ball(p, GF_FIRE, dir, 80 + plev, 3);
    }
    else if (die < 101) visible = drain_life(p, dir, 100 + plev);
    else if (die < 104) visible = earthquake(p, p->depth, py, px, 12);
    else if (die < 106) visible = destroy_area(p, p->depth, py, px, 15, TRUE);
    else if (die < 108) visible = banishment(p);
    else if (die < 110) visible = dispel_monsters(p, 120);
    else /* RARE */
    {
        if (dispel_monsters(p, 150)) visible = TRUE;
        if (slow_monsters(p, FALSE)) visible = TRUE;
        if (sleep_monsters(p, FALSE)) visible = TRUE;
        if (hp_player(p, 300)) visible = TRUE;
    }

    return visible;
}


/*
 * Hack -- Activate the ring of power
 */
static void ring_of_power(struct player *p, int dir)
{
    /* Pick a random effect */
    switch (randint1(10))
    {
        case 1:
        case 2:
        {
            /* Message */
            msg(p, "You are surrounded by a malignant aura.");

            /* Decrease all stats (permanently) */
            player_stat_dec(p, A_STR, TRUE);
            player_stat_dec(p, A_INT, TRUE);
            player_stat_dec(p, A_WIS, TRUE);
            player_stat_dec(p, A_DEX, TRUE);
            player_stat_dec(p, A_CON, TRUE);
            player_stat_dec(p, A_CHR, TRUE);

            /* Lose some experience (permanently) */
            player_exp_lose(p, p->exp / 4, TRUE);

            break;
        }

        case 3:
        {
            /* Message */
            msg(p, "You are surrounded by a powerful aura.");

            /* Dispel monsters */
            dispel_monsters(p, 1000);

            break;
        }

        case 4:
        case 5:
        case 6:
        {
            /* Mana Ball */
            fire_ball(p, GF_MANA, dir, 300, 3);

            break;
        }

        case 7:
        case 8:
        case 9:
        case 10:
        {
            /* Mana Bolt */
            fire_bolt(p, GF_MANA, dir, 250);

            break;
        }
    }
}


/*** Helper functions ***/


static void do_curing(struct player *p, bool *ident)
{
    if (player_clear_timed(p, TMD_BLIND, TRUE)) *ident = TRUE;
    if (player_clear_timed(p, TMD_CONFUSED, TRUE)) *ident = TRUE;
    if (player_clear_timed(p, TMD_AMNESIA, TRUE)) *ident = TRUE;
    if (player_clear_timed(p, TMD_POISONED, TRUE)) *ident = TRUE;
    if (player_clear_timed(p, TMD_STUN, TRUE)) *ident = TRUE;
    if (player_clear_timed(p, TMD_CUT, TRUE)) *ident = TRUE;
}


static void do_healing(struct player *p, bool *ident, int v)
{
    if (hp_player(p, v)) *ident = TRUE;
    do_curing(p, ident);
}


static void do_resistance(struct player *p, bool *ident, int v)
{
    if (player_inc_timed(p, TMD_OPP_ACID, v, TRUE, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_OPP_ELEC, v, TRUE, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_OPP_FIRE, v, TRUE, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_OPP_COLD, v, TRUE, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_OPP_POIS, v, TRUE, TRUE)) *ident = TRUE;
}


static void do_restore(struct player *p, bool *ident)
{
    if (do_res_stat(p, A_STR)) *ident = TRUE;
    if (do_res_stat(p, A_CON)) *ident = TRUE;
    if (do_res_stat(p, A_DEX)) *ident = TRUE;
    if (do_res_stat(p, A_WIS)) *ident = TRUE;
    if (do_res_stat(p, A_INT)) *ident = TRUE;
    if (do_res_stat(p, A_CHR)) *ident = TRUE;
}


void effect_light(struct player *p, bool *ident)
{
    if (hp_player(p, 20)) *ident = TRUE;
    if (player_clear_timed(p, TMD_BLIND, TRUE)) *ident = TRUE;
    if (player_dec_timed(p, TMD_CUT, 20, TRUE)) *ident = TRUE;
    if (player_dec_timed(p, TMD_CONFUSED, 5, TRUE)) *ident = TRUE;
}


void effect_serious(struct player *p, bool *ident)
{
    if (hp_player(p, 40)) *ident = TRUE;
    if (player_clear_timed(p, TMD_BLIND, TRUE)) *ident = TRUE;
    if (player_clear_timed(p, TMD_CONFUSED, TRUE)) *ident = TRUE;
    if (player_clear_timed(p, TMD_CUT, TRUE)) *ident = TRUE;
}


void effect_critical(struct player *p, bool *ident)
{
    do_healing(p, ident, 60);
}


void effect_mortal(struct player *p, bool *ident)
{
    do_healing(p, ident, 80);
}


void effect_heal(struct player *p, bool *ident)
{
    int amt = (p->mhp * 35) / 100;

    if (amt < 300) amt = 300;
    do_healing(p, ident, amt);
}


void effect_hero(struct player *p, int dur, bool *ident)
{
    if (hp_player(p, 10)) *ident = TRUE;
    if (player_clear_timed(p, TMD_AFRAID, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_BOLD, dur, TRUE, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_HERO, dur, TRUE, TRUE)) *ident = TRUE;
}


void effect_berserk(struct player *p, int dur, bool *ident)
{
    if (hp_player(p, 30)) *ident = TRUE;
    if (player_clear_timed(p, TMD_AFRAID, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_BOLD, dur, TRUE, TRUE)) *ident = TRUE;
    if (player_inc_timed(p, TMD_SHERO, dur, TRUE, TRUE)) *ident = TRUE;
}


struct breath_data
{
    int msg;
    const char *desc;
    int typ;
    int what;
};


/*
 * Do an effect (usually from a given object).
 * Boost is the extent to which skill surpasses difficulty, used as % boost. It
 * ranges from 0 to 138.
 */
bool effect_do(struct player *p, effect_type effect, bool *ident, bool aware, int dir, int beam,
    int boost)
{
    int dam;

    if ((effect < 1) || (effect > EF_MAX))
    {
        msg(p, "Effect not handled (%d). Please report this bug.", effect);
        return FALSE;
    }

    switch (effect)
    {
        case EF_XXX: break;

        case EF_ACID_BALL120:
        {
            msg_misc(p, " fires an acid ball.");
            dam = 120 * (100 + boost) / 100;
            fire_ball(p, GF_ACID, dir, dam, 2);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ACID_BOLT:
        {
            msg_misc(p, " fires an acid bolt.");
            dam = damroll(10, 8) * (100 + boost) / 100;
            fire_bolt_or_beam(p, beam, GF_ACID, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ACID_BOLT2:
        {
            msg_misc(p, " fires an acid bolt.");
            dam = damroll(12, 8) * (100 + boost) / 100;
            fire_bolt_or_beam(p, beam, GF_ACID, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ACQUIRE:
        {
            acquirement(p, p->depth, p->py, p->px, 1, 0);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ACQUIRE2:
        {
            acquirement(p, p->depth, p->py, p->px, randint1(2) + 1, 0);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ANNOY_MON:
        {
            msg(p, "There is a high pitched humming noise.");
            aggravate_monsters(p, 0);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ARROW:
        {
            msg_misc(p, " fires a magical arrow.");
            dam = 150 * (100 + boost) / 100;
            fire_bolt(p, GF_ARROW_X, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_BANISHMENT:
        {
            if (banishment(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_BERSERKER:
        {
            effect_berserk(p, randint1(50) + 50, ident);
            return TRUE;
        }

        case EF_BIZARRE:
        {
            ring_of_power(p, dir);
            *ident = TRUE;
            return TRUE;
        }

        case EF_BLESSING:
        {
            if (player_inc_timed(p, TMD_BLESSED, randint1(12) + 6, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_BLESSING2:
        {
            if (player_inc_timed(p, TMD_BLESSED, randint1(24) + 12, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_BLESSING3:
        {
            if (player_inc_timed(p, TMD_BLESSED, randint1(48) + 24, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_BLIND:
        {
            player_inc_timed(p, TMD_BLIND, damroll(4, 25) + 75, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_BRAWN:
        case EF_INTELLECT:
        case EF_CONTEMPLATION:
        case EF_NIMBLENESS:
        case EF_TOUGHNESS:
        {
            int stat = effect - EF_BRAWN;
            int decstat = stat;

            /* Pick a random stat to decrease other than "stat" */
            while (decstat == stat) decstat = randint0(A_MAX);

            if (do_dec_stat(p, decstat, TRUE))
            {
                do_inc_stat(p, stat);
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_CLAIRVOYANCE:
        {
            wiz_light(p, FALSE);
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_COLD_BALL48:
        {
            msg_misc(p, " fires a frost ball.");
            dam = 48 * (100 + boost) / 100;
            fire_ball(p, GF_COLD, dir, dam, 2);
            *ident = TRUE;
            return TRUE;
        }

        case EF_COLD_BALL96:
        {
            msg_misc(p, " fires a frost ball.");
            dam = 96 * (100 + boost) / 100;
            fire_ball(p, GF_COLD, dir, dam, 2);
            *ident = TRUE;
            return TRUE;
        }

        case EF_COLD_BALL160:
        {
            msg_misc(p, " shoots dragon frost!");
            dam = 160 * (100 + boost) / 100;
            fire_ball(p, GF_COLD, dir, dam, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_COLD_BALL200:
        {
            msg_misc(p, " fires a frost ball.");
            dam = 200 * (100 + boost) / 100;
            fire_ball(p, GF_COLD, dir, dam, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_COLD_BOLT:
        {
            msg_misc(p, " fires a frost bolt.");
            dam = damroll(6, 8) * (100 + boost) / 100;
            fire_bolt_or_beam(p, beam, GF_COLD, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_COLD_BOLT2:
        {
            msg_misc(p, " fires a frost bolt.");
            dam = damroll(10, 8) * (100 + boost) / 100;
            fire_bolt_or_beam(p, beam, GF_COLD, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_CONFUSE:
        {
            player_inc_timed(p, TMD_CONFUSED, damroll(4, 5) + 10, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_CONFUSING:
        {
            if (!p->confusing)
            {
                msg(p, "Your hands begin to glow.");
                p->confusing = TRUE;
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_CREATE_HOUSE:
        {
            *ident = TRUE;
            return create_house(p);
        }

        case EF_CREATE_TRAP:
        {
            if (trap_creation(p, TRUE))
                msg(p, "You hear a low-pitched whistling sound.");
            *ident = TRUE;
            return TRUE;
        }

        case EF_CURE_BODY:
        {
            if (player_clear_timed(p, TMD_STUN, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_CUT, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_POISONED, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_BLIND, TRUE)) *ident = TRUE;
            return TRUE;
        }

        case EF_CURE_MIND:
        {
            if (player_clear_timed(p, TMD_CONFUSED, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_AFRAID, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_IMAGE, TRUE)) *ident = TRUE;
            if (!of_has(p->state.flags, OF_RES_CONFU) &&
                player_inc_timed(p, TMD_OPP_CONF, damroll(4, 10), TRUE, TRUE))
            {
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_CURE_PARANOIA:
        {
            if (player_clear_timed(p, TMD_AFRAID, TRUE)) *ident = TRUE;
            return TRUE;
        }

        case EF_CURE_POISON:
        {
            if (player_clear_timed(p, TMD_POISONED, TRUE)) *ident = TRUE;
            return TRUE;
        }

        case EF_CURE_LIGHT:
        {
            effect_light(p, ident);
            return TRUE;
        }

        case EF_CURE_SERIOUS:
        {
            effect_serious(p, ident);
            return TRUE;
        }

        case EF_CURE_CRITICAL:
        {
            effect_critical(p, ident);
            return TRUE;
        }

        case EF_CURE_FULL300:
        {
            effect_heal(p, ident);
            return TRUE;
        }

        case EF_CURE_FULL500:
        {
            do_healing(p, ident, 500);
            return TRUE;
        }

        case EF_CURE_FULL1000:
        {
            do_healing(p, ident, 1000);
            return TRUE;
        }

        case EF_CURE_FULL1200:
        {
            do_healing(p, ident, 1200);
            return TRUE;
        }

        case EF_CURE_NONORLYBIG:
        {
            if (!player_undead(p))
                msg(p, "You feel life flow through your body!");
            do_restore(p, ident);

            /* Recalculate max. hitpoints */
            p->update |= (PU_HP);
            update_stuff(p);

            do_healing(p, ident, 5000);
            player_clear_timed(p, TMD_IMAGE, TRUE);
            restore_level(p);
            *ident = TRUE;
            return TRUE;
        }

        case EF_CURE_TEMP:
        {
            do_curing(p, ident);
            return TRUE;
        }

        case EF_CURSE_ARMOR:
        {
            if (curse_armor(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_CURSE_WEAPON:
        {
            if (curse_weapon(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_DARKNESS:
        {
            wieldeds_notice_flag(p, OF_RES_DARK);
            if (!check_state(p, OF_RES_DARK))
                player_inc_timed(p, TMD_BLIND, 3 + randint1(5), TRUE, TRUE);
            unlight_area(p, 10, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_DEEP_DESCENT:
        {
            deep_descent(p, FALSE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_DESTROY_TDOORS:
        {
            if (destroy_doors_touch(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_DESTRUCTION2:
        {
            destroy_area(p, p->depth, p->py, p->px, 15, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_DETECT_ALL:
        {
            msg(p, "An image forms in your mind...");
            if (detect_all(p, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_DETECT_DOORSTAIR:
        {
            if (detect_doorstairs(p, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_DETECT_EVIL:
        {
            if (detect_monsters_evil(p, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_DETECT_INVIS:
        {
            if (detect_monsters_invis(p, TRUE, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_DETECT_TRAP:
        {
            if (detect_traps(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_DETECT_TREASURE:
        {
            if (detect_treasure(p, aware, FALSE)) *ident = TRUE;
            return TRUE;
        }

        case EF_DISARMING:
        {
            if (disarm_trap(p, dir)) *ident = TRUE;
            return TRUE;
        }

        case EF_DISPEL_ALL:
        {
            dam = 120 * (100 + boost) / 100;
            if (dispel_monsters(p, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DISPEL_EVIL:
        {
            dam = p->lev * 5 * (100 + boost) / 100;
            dispel_evil(p, dam, aware);
            *ident = TRUE;
            return TRUE;
        }

        case EF_DISPEL_EVIL60:
        {
            dam = 60 * (100 + boost) / 100;
            if (dispel_evil(p, dam, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_DISPEL_UNDEAD:
        {
            dam = 60 * (100 + boost) / 100;
            if (dispel_undead(p, dam, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_BLACK:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_ACID, FLAG_END);
            dam = 130 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_BLUE:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_ELEC, FLAG_END);
            dam = 100 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_WHITE:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_COLD, FLAG_END);
            dam = 110 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_RED:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_FIRE, FLAG_END);
            dam = 200 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_GREEN:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_POIS, FLAG_END);
            dam = 150 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_MULTIHUED:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_ELEC, RSF_BR_COLD, RSF_BR_ACID, RSF_BR_POIS,
                RSF_BR_FIRE, FLAG_END);
            dam = 250 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_SHADOW:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_LIGHT, RSF_BR_DARK, FLAG_END);
            dam = 200 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_LAW:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_SOUN, RSF_BR_SHAR, FLAG_END);
            dam = 230 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_GOLD:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_SOUN, FLAG_END);
            dam = 130 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_CHAOS:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_CHAO, RSF_BR_DISE, FLAG_END);
            dam = 220 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_BALANCE:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_CHAO, RSF_BR_DISE, RSF_BR_SOUN, RSF_BR_SHAR, FLAG_END);
            dam = 250 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_POWER:
        {
            bitflag f[RSF_SIZE];

            rsf_setall(f);
            set_spells(f, RST_BREATH);
            dam = 300 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_CRYSTAL:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_SHAR, FLAG_END);
            dam = 130 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_SILVER:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_COLD, RSF_BR_INER, FLAG_END);
            dam = 150 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_ETHEREAL:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_NETH, FLAG_END);
            dam = 200 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_DLISK:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_FIRE, RSF_BR_NEXU, FLAG_END);
            dam = 200 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAGON_WATER:
        {
            bitflag f[RSF_SIZE];

            flags_init(f, RSF_SIZE, RSF_BR_WATE, FLAG_END);
            dam = 200 * (100 + boost) / 100;
            if (fire_breath(p, f, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAIN_LIFE:
        {
            dam = 90 * (100 + boost) / 100;
            if (drain_life(p, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAIN_LIFE2:
        {
            dam = 120 * (100 + boost) / 100;
            if (drain_life(p, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAIN_LIFE3:
        {
            dam = 150 * (100 + boost) / 100;
            if (drain_life(p, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRAIN_LIFE4:
        {
            dam = 250 * (100 + boost) / 100;
            if (drain_life(p, dir, dam)) *ident = TRUE;
            return TRUE;
        }

        case EF_DRINK_BREATH:
        {
            /* Table of random ball effects and their damages */
            const struct breath_data breath_types[] =
            {
                {0, " breathes fire!", GF_FIRE, 80},
                {0, " breathes frost!", GF_COLD, 80}
            };

            /* Pick a random (type, damage) tuple in the table */
            int which = randint0(N_ELEMENTS(breath_types));

            msg_misc(p, breath_types[which].desc);
            fire_ball(p, breath_types[which].typ, dir, breath_types[which].what, 2);
            *ident = TRUE;
            return FALSE;
        }

        case EF_DRINK_GOOD:
        {
            msg(p, "You feel less thirsty.");
            *ident = TRUE;
            return TRUE;
        }

        case EF_DRINK_SALT:
        {
            msg(p, "The potion makes you vomit!");
            msg_misc(p, " vomits!");
            player_set_food(p, PY_FOOD_STARVE - 1);
            player_clear_timed(p, TMD_POISONED, TRUE);
            player_inc_timed(p, TMD_PARALYZED, 4, TRUE, FALSE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_EARTHQUAKES:
        {
            earthquake(p, p->depth, p->py, p->px, 10);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ELEC_BALL64:
        {
            msg_misc(p, " fires a lightning ball.");
            dam = 64 * (100 + boost) / 100;
            fire_ball(p, GF_ELEC, dir, dam, 2);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ELEC_BALL250:
        {
            msg_misc(p, " fires a lightning ball.");
            dam = 250 * (100 + boost) / 100;
            fire_ball(p, GF_ELEC, dir, dam, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ELEC_BOLT:
        {
            msg_misc(p, " fires a lightning bolt.");
            dam = damroll(6, 6) * (100 + boost) / 100;
            fire_beam(p, GF_ELEC, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ENCHANT_ARMOR:
        {
            if (p->current_value == ITEM_REQUEST)
                msg(p, "A black magical aura surrounds you...");
            *ident = TRUE;
            return enchant_spell(p, 0, 0, -1);
        }

        case EF_ENCHANT_ARMOR2:
        {
            if (p->current_value == ITEM_REQUEST)
                msg(p, "A black magical aura surrounds you...");
            *ident = TRUE;
            return enchant_spell(p, 0, 0, 0 - 2 - randint1(3));
        }

        case EF_ENCHANT_TODAM:
        {
            if (p->current_value == ITEM_REQUEST)
                msg(p, "A red magical aura surrounds you...");
            *ident = TRUE;
            return enchant_spell(p, 0, -1, 0);
        }

        case EF_ENCHANT_TOHIT:
        {
            if (p->current_value == ITEM_REQUEST)
                msg(p, "A blue magical aura surrounds you...");
            *ident = TRUE;
            return enchant_spell(p, -1, 0, 0);
        }

        case EF_ENCHANT_WEAPON:
        {
            if (p->current_value == ITEM_REQUEST)
                msg(p, "A purple magical aura surrounds you...");
            *ident = TRUE;
            return enchant_spell(p, 0 - randint1(3), 0 - randint1(3), 0);
        }

        case EF_ENLIGHTENMENT:
        {
            msg(p, "An image of your surroundings forms in your mind...");
            wiz_light(p, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ENLIGHTENMENT2:
        {
            msg(p, "You begin to feel more enlightened...");
            message_flush(p);
            wiz_light(p, TRUE);
            do_inc_stat(p, A_INT);
            do_inc_stat(p, A_WIS);
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            detect_treasure(p, TRUE, TRUE);
            detect_monsters_entire_level(p);
            identify_pack(p);
            *ident = TRUE;
            return TRUE;
        }

        case EF_FIRE_BALL72:
        {
            msg_misc(p, " fires a fire ball.");
            dam = 72 * (100 + boost) / 100;
            fire_ball(p, GF_FIRE, dir, dam, 2);
            *ident = TRUE;
            return TRUE;
        }

        case EF_FIRE_BALL120:
        {
            msg_misc(p, " fires a fire ball.");
            dam = 120 * (100 + boost) / 100;
            fire_ball(p, GF_FIRE, dir, dam, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_FIRE_BALL144:
        {
            msg_misc(p, " fires a fire ball.");
            dam = 144 * (100 + boost) / 100;
            fire_ball(p, GF_FIRE, dir, dam, 2);
            *ident = TRUE;
            return TRUE;
        }

        case EF_FIRE_BALL200:
        {
            msg_misc(p, " shoots dragon fire!");
            dam = 200 * (100 + boost) / 100;
            fire_ball(p, GF_FIRE, dir, dam, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_FIRE_BOLT:
        {
            msg_misc(p, " fires a fire bolt.");
            dam = damroll(12, 8) * (100 + boost) / 100;
            fire_bolt_or_beam(p, beam, GF_FIRE, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_FOOD_BAD:
        {
            if (!player_undead(p)) msg(p, "Yuk, that tastes awful!");
            *ident = TRUE;
            return TRUE;
        }

        case EF_FOOD_GOOD:
        {
            if (!player_undead(p)) msg(p, "That tastes good.");
            *ident = TRUE;
            return TRUE;
        }

        case EF_FOOD_WAYBREAD:
        {
            if (!player_undead(p)) msg(p, "That tastes good.");
            hp_player(p, damroll(4, 8));
            player_clear_timed(p, TMD_POISONED, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_GAIN_ALL:
        {
            if (do_inc_stat(p, A_STR)) *ident = TRUE;
            if (do_inc_stat(p, A_INT)) *ident = TRUE;
            if (do_inc_stat(p, A_WIS)) *ident = TRUE;
            if (do_inc_stat(p, A_DEX)) *ident = TRUE;
            if (do_inc_stat(p, A_CON)) *ident = TRUE;
            if (do_inc_stat(p, A_CHR)) *ident = TRUE;
            return TRUE;
        }

        case EF_GAIN_STR:
        case EF_GAIN_INT:
        case EF_GAIN_WIS:
        case EF_GAIN_DEX:
        case EF_GAIN_CON:
        case EF_GAIN_CHR:
        {
            int stat = effect - EF_GAIN_STR;

            if (do_inc_stat(p, stat)) *ident = TRUE;
            return TRUE;
        }

        case EF_GAIN_EXP:
        {
            if (restore_level(p)) *ident = TRUE;
            if (p->exp < PY_MAX_EXP)
            {
                msg(p, "You feel more experienced.");
                player_exp_gain(p, 100000L);
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_GAIN_EXP_LTD:
        {
            if (restore_level(p)) *ident = TRUE;
            if (p->exp < PY_MAX_EXP)
            {
                s32b ee = (p->exp / 2) + 10;
                if (ee > 100000L) ee = 100000L;
                msg(p, "You feel more experienced.");
                player_exp_gain(p, ee);
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_HASTE:
        {
            if (player_inc_timed_nostack(p, TMD_FAST, randint1(20) + 20, 5, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_HASTE2:
        {
            if (player_inc_timed_nostack(p, TMD_FAST, randint1(75) + 75, 5, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_HERO:
        {
            effect_hero(p, randint1(25) + 25, ident);
            return TRUE;
        }

        case EF_IDENTIFY:
        {
            if (p->current_value == ITEM_REQUEST)
                msg(p, "A grey magical aura surrounds you...");
            *ident = TRUE;
            return ident_spell(p);
        }

        case EF_IDENTIFY2:
        {
            msg(p, "A white magical aura surrounds you...");
            identify_pack(p);
            *ident = TRUE;
            return TRUE;
        }

        case EF_ILLUMINATION:
        {
            msg_misc(p, " calls light.");
            if (light_area(p, damroll(2, 15), 3)) *ident = TRUE;
            return TRUE;
        }

        case EF_LIGHT:
        {
            msg_misc(p, " calls light.");
            if (light_area(p, damroll(2, 8), 2)) *ident = TRUE;
            return TRUE;
        }

        case EF_LIGHT_LINE:
        {
            msg(p, "A line of blue shimmering light appears.");
            light_line(p, dir);
            *ident = TRUE;
            return TRUE;
        }

        case EF_LOSCONF:
        {
            if (confuse_monsters(p, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_LOSE_EXP:
        {
            *ident = TRUE;
            if (!p->exp) return TRUE;
            wieldeds_notice_flag(p, OF_HOLD_LIFE);
            if (!check_state(p, OF_HOLD_LIFE))
            {
                msg(p, "You feel your memories fade.");
                player_exp_lose(p, p->exp / 4, FALSE);
            }
            return TRUE;
        }

        case EF_LOSHASTE:
        {
            if (speed_monsters(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_LOSKILL:
        {
            if (mass_banishment(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_LOSSLEEP:
        {
            if (sleep_monsters(p, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_LOSSLOW:
        {
            if (slow_monsters(p, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_MANA_BOLT:
        {
            msg_misc(p, " fires a mana bolt.");
            dam = damroll(12, 8) * (100 + boost) / 100;
            fire_bolt(p, GF_MANA, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_MAPPING:
        {
            map_area(p);
            *ident = TRUE;
            return TRUE;
        }

        case EF_MISSILE:
        {
            msg_misc(p, " fires a magic missile.");
            dam = damroll(3, 4) * (100 + boost) / 100;
            fire_bolt_or_beam(p, beam, GF_MISSILE, dir, dam);
            *ident = TRUE;
            return TRUE;
        }

        case EF_MON_CLONE:
        {
            if (clone_monster(p, dir)) *ident = TRUE;
            return TRUE;
        }    

        case EF_MON_CONFUSE:
        {
            if (confuse_monster(p, dir, 10, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_MON_HASTE:
        {
            if (speed_monster(p, dir)) *ident = TRUE;
            return TRUE;
        }

        case EF_MON_HEAL:
        {
            if (heal_monster(p, dir)) *ident = TRUE;
            return TRUE;
        }

        case EF_MON_SCARE:
        {
            if (fear_monster(p, dir, 10, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_MON_SLEEP:
        {
            if (sleep_monster(p, dir, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_MON_SLOW:
        {
            if (slow_monster(p, dir, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_PARALYZE:
        {
            player_inc_timed(p, TMD_PARALYZED, randint1(5) + 5, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_POISON:
        {
            player_inc_timed(p, TMD_POISONED, damroll(2, 7) + 10, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_POLYMORPH:
        {
            if (poly_monster(p, dir, aware)) *ident = TRUE;
            return TRUE;
        }

        case EF_POLY_RACE:
        {
            if (poly_race(p, beam))
            {
                *ident = TRUE;
                return TRUE;
            }
            return FALSE;
        }

        case EF_PROBING:
        {
            if (probing(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_PROTEVIL:
        {
            if (player_inc_timed(p, TMD_PROTEVIL, randint1(25) + 3 * p->lev, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_RAGE_BLESS_RESIST:
        {
            effect_berserk(p, randint1(50) + 50, ident);
            player_inc_timed(p, TMD_BLESSED, randint1(50) + 50, TRUE, TRUE);
            do_resistance(p, ident, randint1(50) + 50);
            *ident = TRUE;
            return TRUE;
        }

        case EF_RECALL:
        {
            set_recall(p, beam, FALSE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_RECHARGE:
        {
            if (p->current_value == ITEM_REQUEST)
                msg(p, "A yellow magical aura surrounds you...");
            *ident = TRUE;
            return recharge(p, 60);
        }

        /*case EF_REMOVE_CURSE:
        {
            if (remove_curse(p))
            {
                if (!p->timed[TMD_BLIND])
                    msg(p, "The air around your body glows blue for a moment...");
                else
                    msg(p, "You feel as if someone is watching over you.");
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_REMOVE_CURSE2:
        {
            if (remove_all_curse(p))
            {
                if (!p->timed[TMD_BLIND])
                    msg(p, "The air around your body glows blue for a moment...");
                else
                    msg(p, "You feel as if someone is watching over you.");
                *ident = TRUE;
            }
            return TRUE;
        }*/

        case EF_REM_FEAR_POIS:
        {
            player_clear_timed(p, TMD_AFRAID, TRUE);
            player_clear_timed(p, TMD_POISONED, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_RESIST_ALL:
        {
            do_resistance(p, ident, randint1(20) + 20);
            return TRUE;
        }

        case EF_RESIST_ALL_LORDLY:
        {
            do_resistance(p, ident, randint1(40) + 40);
            return TRUE;
        }

        case EF_RESIST_ACID:
        {
            if (player_inc_timed(p, TMD_OPP_ACID, beam, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_RESIST_COLD:
        {
            if (player_inc_timed(p, TMD_OPP_COLD, randint1(10) + 10, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_RESIST_ELEC:
        {
            if (player_inc_timed(p, TMD_OPP_ELEC, beam, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_RESIST_FIRE:
        {
            if (player_inc_timed(p, TMD_OPP_FIRE, randint1(10) + 10, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_RESIST_POIS:
        {
            if (player_inc_timed(p, TMD_OPP_POIS, randint1(10) + 10, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_RESTORE_ALL:
        {
            do_restore(p, ident);
            return TRUE;
        }

        case EF_RESTORE_LIFE:
        {
            if (restore_level(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_RESTORE_MANA:
        {
            if (sp_player(p, 500)) *ident = TRUE;
            return TRUE;
        }

        case EF_RESTORE_MANA2:
        {
            if (p->csp < p->msp)
            {
                restore_sp(p);
                msg(p, "Your feel your head clear.");
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_RESTORE_ST_LEV:
        {
            if (restore_level(p)) *ident = TRUE;
            do_restore(p, ident);
            return TRUE;
        }

        case EF_RESURRECT:
        {
            if (do_scroll_life(p)) *ident = TRUE;
            return TRUE;
        }

        case EF_RING_ACID:
        {
            dam = 70 * (100 + boost) / 100;
            fire_ball(p, GF_ACID, dir, dam, 2);
            player_inc_timed(p, TMD_OPP_ACID, randint1(20) + 20, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_RING_FLAMES:
        {
            dam = 80 * (100 + boost) / 100;
            fire_ball(p, GF_FIRE, dir, dam, 2);
            player_inc_timed(p, TMD_OPP_FIRE, randint1(20) + 20, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_RING_ICE:
        {
            dam = 75 * (100 + boost) / 100;
            fire_ball(p, GF_COLD, dir, dam, 2);
            player_inc_timed(p, TMD_OPP_COLD, randint1(20) + 20, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_RING_LIGHTNING:
        {
            dam = 85 * (100 + boost) / 100;
            fire_ball(p, GF_ELEC, dir, dam, 2);
            player_inc_timed(p, TMD_OPP_ELEC, randint1(20) + 20, TRUE, TRUE);
            *ident = TRUE;
            return TRUE;
        }

        case EF_RUNE:
        {
            warding_glyph(p);
            *ident = TRUE;
            return FALSE;
        }

        case EF_SATISFY:
        {
            if (player_set_food(p, PY_FOOD_MAX - 1)) *ident = TRUE;
            return TRUE;
        }

        case EF_SHERO:
        {
            effect_berserk(p, randint1(25) + 25, ident);
            return TRUE;
        }

        case EF_SHROOM_DEBILITY:
        {
            int stat = (one_in_(2)? A_STR: A_CON);

            if (p->csp < p->msp)
            {
                restore_sp(p);
                msg(p, "Your feel your head clear.");
            }

            do_dec_stat(p, stat, FALSE);

            *ident = TRUE;
            return TRUE;
        }

        case EF_SHROOM_EMERGENCY:
        {
            player_set_timed(p, TMD_IMAGE, rand_spread(250, 50), TRUE);
            player_set_timed(p, TMD_OPP_FIRE, rand_spread(30, 10), TRUE);
            player_set_timed(p, TMD_OPP_COLD, rand_spread(30, 10), TRUE);
            hp_player(p, 200);
            *ident = TRUE;
            return TRUE;
        }

        case EF_SHROOM_PURGING:
        {
            if (player_set_food(p, PY_FOOD_FAINT - 1)) *ident = TRUE;
            if (do_res_stat(p, A_STR)) *ident = TRUE;
            if (do_res_stat(p, A_CON)) *ident = TRUE;
            if (player_clear_timed(p, TMD_POISONED, TRUE)) *ident = TRUE;
            return TRUE;
        }

        case EF_SHROOM_SPRINTING:
        {
            if (player_inc_timed(p, TMD_SPRINT, 100, TRUE, TRUE)) *ident = TRUE;
            return TRUE;
        }

        case EF_SHROOM_STONE:
        {
            if (player_set_timed(p, TMD_STONESKIN, rand_spread(80, 20), TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_SHROOM_TERROR:
        {
            if (player_set_timed(p, TMD_TERROR, rand_spread(100, 20), TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_SLEEPII:
        {
            sleep_monsters_touch(p, aware);
            *ident = TRUE;
            return TRUE;
        }

        case EF_SLOW:
        {
            if (player_inc_timed(p, TMD_SLOW, randint1(20) + 20, TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_STAFF_HOLY:
        {
            dam = 120 * (100 + boost) / 100;
            if (dispel_evil(p, dam, TRUE)) *ident = TRUE;
            if (hp_player(p, 50)) *ident = TRUE;
            if (player_inc_timed(p, TMD_PROTEVIL, randint1(25) + 3 * p->lev, TRUE, TRUE))
                *ident = TRUE;
            do_curing(p, ident);
            if (player_clear_timed(p, TMD_TERROR, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_AFRAID, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_SLOW, TRUE)) *ident = TRUE;
            if (player_clear_timed(p, TMD_IMAGE, TRUE)) *ident = TRUE;
            return TRUE;
        }

        case EF_STAFF_MAGI:
        {
            if (do_res_stat(p, A_INT)) *ident = TRUE;
            if (p->csp < p->msp)
            {
                restore_sp(p);
                msg(p, "Your feel your head clear.");
                *ident = TRUE;
            }
            return TRUE;
        }

        case EF_STAR_BALL:
        {
            int i;

            if (!p->timed[TMD_BLIND])
                msg(p, "Lightning shoots in all directions!");
            dam = 150 * (100 + boost) / 100;
            for (i = 0; i < 8; i++)
                fire_ball(p, GF_ELEC, ddd[i], dam, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_STARLIGHT:
        {
            int i;

            if (!p->timed[TMD_BLIND])
                msg(p, "Light shoots in all directions!");
            for (i = 0; i < 8; i++) light_line(p, ddd[i]);
            *ident = TRUE;
            return TRUE;
        }

        case EF_STARLIGHT2:
        {
            int i;

            if (!p->timed[TMD_BLIND])
                msg(p, "Light shoots in all directions!");
            for (i = 0; i < 8; i++) strong_light_line(p, ddd[i]);
            *ident = TRUE;
            return TRUE;
        }

        case EF_STINKING_CLOUD:
        {
            msg_misc(p, " fires a stinking cloud.");
            dam = 12 * (100 + boost) / 100;
            fire_ball(p, GF_POIS, dir, dam, 2);
            *ident = TRUE;
            return TRUE;
        }

        case EF_STONE_TO_MUD:
        {
            if (wall_to_mud(p, dir)) *ident = TRUE;
            return TRUE;
        }

        case EF_SUMMON_MON:
        {
            int i;
            int chance = 0, num = randint1(3);

            /* Summoners may get friendly summons */
            if (player_has(p, PF_SUMMON_SPELLS)) chance = 100;

            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) num = 0;
            for (i = 0; i < num; i++)
            {
                if (summon_specific(p, p->py, p->px, p->depth, 0, 1, chance))
                    *ident = TRUE;
            }
            return TRUE;
        }

        case EF_SUMMON_UNDEAD:
        {
            int i;
            int chance = 0, num = randint1(3);

            /* Summoners may get friendly summons */
            if (player_has(p, PF_SUMMON_SPELLS)) chance = 100;

            sound(p, MSG_SUM_UNDEAD);
            if (check_antisummon(p, NULL)) num = 0;
            for (i = 0; i < num; i++)
            {
                if (summon_specific(p, p->py, p->px, p->depth, S_UNDEAD, 1, chance))
                    *ident = TRUE;
            }
            return TRUE;
        }

        case EF_TDOOR_DEST:
        {
            if (destroy_door(p, dir)) *ident = TRUE;
            return TRUE;
        }

        case EF_TELE_LEVEL:
        {
            teleport_player_level(p);
            *ident = TRUE;
            return FALSE;
        }

        case EF_TELE_LEVEL2:
        {
            if (player_inc_timed(p, TMD_PROBTRAVEL, 20 + randint1(20), TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_TELE_LONG:
        {
            msg_misc(p, " teleports away.");
            teleport_player(p, 100);
            *ident = TRUE;
            return FALSE;
        }

        case EF_TELE_OTHER:
        {
            if (teleport_monster(p, dir)) *ident = TRUE;
            return TRUE;
        }

        case EF_TELE_PHASE:
        {
            teleport_player(p, 10);
            *ident = TRUE;
            return FALSE;
        }

        case EF_TMD_ESP:
        {
            if (player_clear_timed(p, TMD_BLIND, TRUE)) *ident = TRUE;
            if (player_inc_timed(p, TMD_ESP, 12 + damroll(6, 6), TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_TMD_INFRA:
        {
            if (player_inc_timed(p, TMD_SINFRA, 100 + damroll(4, 25), TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_TMD_SINVIS:
        {
            if (player_clear_timed(p, TMD_BLIND, TRUE)) *ident = TRUE;
            if (player_inc_timed(p, TMD_SINVIS, 12 + damroll(2, 6), TRUE, TRUE))
                *ident = TRUE;
            return TRUE;
        }

        case EF_TRAP_DOOR:
        {
            const char *poss = player_poss(p), *pself = player_self(p);

            /* Verify basic quests */
            if (!quest_done(p, p->depth))
            {
                msg(p, "You feel quite certain something really awful just happened...");
                return TRUE;
            }

            /* Hack -- DM redesigning the level */
            if (players_on_depth[p->depth + 1] == INHIBIT_DEPTH)
            {
                msg(p, "You feel quite certain something really awful just happened...");
                return TRUE;
            }

            msg(p, "You fall through a trap door!");
            wieldeds_notice_flag(p, OF_FEATHER);
            if (check_state(p, OF_FEATHER))
                msg(p, "You float gently down to the next level.");
            else
            {
                strnfmt(p->died_flavor, sizeof(p->died_flavor),
                    "broke %s neck after falling from 50ft high", poss);
                take_hit(p, damroll(2, 8), "a trap", FALSE);
            }

            dungeon_change_level(p, p->depth + 1, LEVEL_RAND);
            return TRUE;
        }

        case EF_TRAP_PIT:
        {
            const char *poss = player_poss(p);

            msg(p, "You fall into a pit!");
            wieldeds_notice_flag(p, OF_FEATHER);
            if (check_state(p, OF_FEATHER))
                msg(p, "You float gently to the bottom of the pit.");
            else
            {
                strnfmt(p->died_flavor, sizeof(p->died_flavor),
                    "broke %s neck after falling into a pit", poss);
                take_hit(p, damroll(2, 6), "a trap", FALSE);
            }

            return TRUE;
        }

        case EF_TRAP_PIT_SPIKES:
        {
            const char *pself = player_self(p);

            msg(p, "You fall into a spiked pit!");

            wieldeds_notice_flag(p, OF_FEATHER);
            if (check_state(p, OF_FEATHER))
            {
                msg(p, "You float gently to the floor of the pit.");
                msg(p, "You carefully avoid touching the spikes.");
            }
            else
            {
                dam = damroll(2, 6);

                /* Extra spike damage */
                if (one_in_(2))
                {
                    msg(p, "You are impaled!");
                    dam = dam * 2;
                    player_inc_timed(p, TMD_CUT, randint1(dam), TRUE, TRUE);
                }

                strnfmt(p->died_flavor, sizeof(p->died_flavor), "impaled %s on sharp spikes", pself);
                take_hit(p, dam, "a trap", FALSE);
            }

            return TRUE;
        }

        case EF_TRAP_PIT_POISON:
        {
            const char *pself = player_self(p);

            msg(p, "You fall into a spiked pit!");

            wieldeds_notice_flag(p, OF_FEATHER);
            if (check_state(p, OF_FEATHER))
            {
                msg(p, "You float gently to the floor of the pit.");
                msg(p, "You carefully avoid touching the spikes.");
            }
            else
            {
                dam = damroll(2, 6);

                /* Extra spike damage */
                if (one_in_(2))
                {
                    msg(p, "You are impaled on poisonous spikes!");
                    player_inc_timed(p, TMD_CUT, randint1(dam * 2), TRUE, TRUE);
                    player_inc_timed(p, TMD_POISONED, randint1(dam * 4), TRUE, TRUE);
                }

                strnfmt(p->died_flavor, sizeof(p->died_flavor), "impaled %s on poisonous spikes",
                    pself);
                take_hit(p, dam, "a trap", FALSE);
            }

            return TRUE;
        }

        case EF_TRAP_RUNE_SUMMON:
        {
            int i;
            int num = 2 + randint1(3);

            msgt(p, MSG_SUM_MONSTER, "You are enveloped in a cloud of smoke!");
            if (check_antisummon(p, NULL)) num = 0;

            /* Remove trap */
            forget_spot(p->depth, p->py, p->px);
            cave_set_feat(cave_get(p->depth), p->py, p->px, FEAT_FLOOR);

            for (i = 0; i < num; i++)
                summon_specific(p, p->py, p->px, p->depth, 0, 1, 0);

            return TRUE;
        }

        case EF_TRAP_RUNE_TELEPORT:
        {
            msg(p, "You hit a teleport trap!");
            teleport_player(p, 100);
            return TRUE;
        }

        case EF_TRAP_SPOT_FIRE:
        {
            msg(p, "You are enveloped in flames!");
            dam = damroll(4, 6);
            my_strcpy(p->died_flavor, "was fried by a fire trap", sizeof(p->died_flavor));
            dam = adjust_dam(p, GF_FIRE, dam, RANDOMISE, check_for_resist(p, GF_FIRE, TRUE));
            if (dam)
            {
                if (!take_hit(p, dam, "a fire trap", FALSE))
                    inven_damage(p, GF_FIRE, MIN(dam * 5, 300));
            }
            return TRUE;
        }

        case EF_TRAP_SPOT_ACID:
        {
            msg(p, "You are splashed with acid!");
            dam = damroll(4, 6);
            my_strcpy(p->died_flavor, "was dissolved by an acid trap", sizeof(p->died_flavor));
            dam = adjust_dam(p, GF_ACID, dam, RANDOMISE, check_for_resist(p, GF_ACID, TRUE));
            if (dam)
            {
                if (!take_hit(p, dam, "an acid trap", FALSE))
                    inven_damage(p, GF_ACID, MIN(dam * 5, 300));
            }
            return TRUE;
        }

        case EF_TRAP_DART_SLOW:
        {
            if (trap_check_hit(p, 125))
            {
                msg(p, "A small dart hits you!");
                my_strcpy(p->died_flavor, "was shot by a slowing dart", sizeof(p->died_flavor));
                if (take_hit(p, damroll(1, 4), "a trap", FALSE)) break;
                player_inc_timed(p, TMD_SLOW, randint0(20) + 20, TRUE, FALSE);
            }
            else
                msg(p, "A small dart barely misses you.");
            return TRUE;
        }

        case EF_TRAP_DART_LOSE_STR:
        {
            if (trap_check_hit(p, 125))
            {
                msg(p, "A small dart hits you!");
                my_strcpy(p->died_flavor, "was shot by a weakening dart", sizeof(p->died_flavor));
                if (take_hit(p, damroll(1, 4), "a trap", FALSE)) break;
                do_dec_stat(p, A_STR, FALSE);
            }
            else
                msg(p, "A small dart barely misses you.");
            return TRUE;
        }

        case EF_TRAP_DART_LOSE_DEX:
        {
            if (trap_check_hit(p, 125))
            {
                msg(p, "A small dart hits you!");
                my_strcpy(p->died_flavor, "was shot by a small dart", sizeof(p->died_flavor));
                if (take_hit(p, damroll(1, 4), "a trap", FALSE)) break;
                do_dec_stat(p, A_DEX, FALSE);
            }
            else
                msg(p, "A small dart barely misses you.");
            return TRUE;
        }

        case EF_TRAP_DART_LOSE_CON:
        {
            if (trap_check_hit(p, 125))
            {
                msg(p, "A small dart hits you!");
                my_strcpy(p->died_flavor, "was shot by an exhausting dart", sizeof(p->died_flavor));
                if (take_hit(p, damroll(1, 4), "a trap", FALSE)) break;
                do_dec_stat(p, A_CON, FALSE);
            }
            else
                msg(p, "A small dart barely misses you.");
            return TRUE;
        }

        case EF_TRAP_GAS_BLIND:
        {
            msg(p, "You are surrounded by a black gas!");
            player_inc_timed(p, TMD_BLIND, randint0(50) + 25, TRUE, TRUE);
            return TRUE;
        }

        case EF_TRAP_GAS_CONFUSE:
        {
            msg(p, "You are surrounded by a gas of scintillating colors!");
            player_inc_timed(p, TMD_CONFUSED, randint0(20) + 10, TRUE, TRUE);
            return TRUE;
        }

        case EF_TRAP_GAS_POISON:
        {
            msg(p, "You are surrounded by a pungent green gas!");
            player_inc_timed(p, TMD_POISONED, randint0(20) + 10, TRUE, TRUE);
            return TRUE;
        }

        case EF_TRAP_GAS_SLEEP:
        {
            msg(p, "You are surrounded by a strange white mist!");
            player_inc_timed(p, TMD_PARALYZED, randint0(10) + 5, TRUE, TRUE);
            return TRUE;
        }

        case EF_WAND_BREATH:
        {
            /* Table of random ball effects and their damages */
            const struct breath_data breath_types[] =
            {
                {0, " shoots dragon acid!", GF_ACID, 200},
                {0, " shoots dragon lightning!", GF_ELEC, 160},
                {0, " shoots dragon fire!", GF_FIRE, 200},
                {0, " shoots dragon frost!", GF_COLD, 160},
                {0, " shoots dragon poison!", GF_POIS, 120}
            };

            /* Pick a random (type, damage) tuple in the table */
            int which = randint0(N_ELEMENTS(breath_types));

            msg_misc(p, breath_types[which].desc);
            fire_ball(p, breath_types[which].typ, dir, breath_types[which].what, 3);
            *ident = TRUE;
            return TRUE;
        }

        case EF_WONDER:
        {
            if (effect_wonder(p, dir, randint1(100) + p->lev / 5, beam))
                *ident = TRUE;
            return TRUE;
        }

        case EF_MAX: break;
    }

    /* Not used */
    msg(p, "Effect not handled (%d). Please report this bug.", effect);
    return FALSE;
}
