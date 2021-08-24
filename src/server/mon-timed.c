/*
 * File: mon-timed.c
 * Purpose: Monster timed effects.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
 * The different ways increases can stack - see mon_inc_timed()
 */
enum stack_type
{
    STACK_NO,
    STACK_INCR,
    STACK_MAX
};


/*
 * Monster timed effects.
 */
static struct mon_timed_effect
{
    const char *name;
    bool gets_save;
    enum stack_type stacking;
    int flag_resist;
    int max_timer;
    int message_begin;
    int message_end;
    int message_increase;
} effects[] =
{
    #define MON_TMD(a, b, c, d, e, f, g, h) {#a, b, STACK_##c, d, e, f, g, h},
    #include "../common/list-mon-timed.h"
    #undef MON_TMD
    {"MAX", false, STACK_NO, 0, 0, 0, 0, 0}
};


/*
 * Find the timed monster effect with the name `name`.
 *
 * Returns -1 on failure.
 */
int mon_timed_name_to_idx(const char *name)
{
    int i;

    for (i = 0; !streq(effects[i].name, "MAX"); i++)
    {
        if (streq(name, effects[i].name)) return i;
    }

    return -1;
}


/*
 * Roll the saving throw for monsters resisting a timed effect.
 */
static bool saving_throw(struct player *p, const struct monster *mon, int timer, int flag)
{
    int resist_chance = MIN(90, mon->race->level + MAX(0, 25 - timer / 2));

    if (p && p->timed[TMD_DESPAIR]) resist_chance /= 2;

    /* Uniques are doubly hard to affect */
    if (monster_is_unique(mon->race) && magik(resist_chance)) return true;

    return magik(resist_chance);
}


/*
 * Determines whether the given monster successfully resists the given effect.
 */
static bool does_resist(struct player *p, const struct monster *mon, int effect_type, int timer,
    int flag)
{
    struct mon_timed_effect *effect;
    struct monster_lore *lore;
    bool visible;

    my_assert(mon != NULL);
    my_assert(effect_type >= 0);
    my_assert(effect_type < MON_TMD_MAX);

    effect = &effects[effect_type];
    lore = (p? get_lore(p, mon->race): NULL);
    visible = (p? monster_is_visible(p, mon->midx): false);

    /* Sometimes the game can override the monster's innate resistance */
    if (flag & MON_TMD_FLG_NOFAIL) return false;

    /* Check resistances from monster flags */
    if (rf_has(mon->race->flags, effect->flag_resist))
    {
        lore_learn_flag_if_visible(lore, visible, effect->flag_resist);
        return true;
    }

    /* Some effects get a saving throw; others don't */
    if (effect->gets_save) return saving_throw(p, mon, timer, flag);
    return false;
}


/*
 * Attempts to set the timer of the given monster effect to `timer`.
 *
 * Checks to see if the monster resists the effect, using does_resist().
 * If not, the effect is set to `timer` turns. If `timer` is 0, or if the
 * effect timer was 0, or if MON_TMD_FLG_NOTIFY is set in `flag`, then a
 * message is printed, unless MON_TMD_FLG_NOMESSAGE is set in `flag`.
 *
 * Give messages if the right flags are set.
 * Check if the monster is able to resist the spell. Mark the lore.
 *
 * Returns true if the monster was affected, false if not.
 */
static bool mon_set_timed(struct player *p, struct monster *mon, int effect_type, int timer,
    int flag)
{
    struct mon_timed_effect *effect;
    bool check_resist;
    bool resisted = false;
    int m_note = 0;
    int old_timer;
    bool visible = false;

    my_assert(mon != NULL);
    my_assert(mon->race != NULL);
    my_assert(effect_type >= 0);
    my_assert(effect_type < MON_TMD_MAX);
    my_assert(timer >= 0);

    effect = &effects[effect_type];
    old_timer = mon->m_timed[effect_type];
    if (p) visible = monster_is_obvious(p, mon->midx, mon);

    /* Limit time of effect */
    if (timer > effect->max_timer) timer = effect->max_timer;

    /* No change */
    if (old_timer == timer) return false;

    /* Turning off, usually mention */
    if (timer == 0)
    {
        m_note = effect->message_end;
        flag |= MON_TMD_FLG_NOTIFY;
        check_resist = false;
    }

    /* Turning on, usually mention */
    else if (old_timer == 0)
    {
        m_note = effect->message_begin;
        flag |= MON_TMD_FLG_NOTIFY;
        check_resist = true;
    }

    /* Different message for increases, but don't automatically mention. */
    else if (timer > old_timer)
    {
        m_note = effect->message_increase;
        check_resist = true;
    }

    /* Decreases don't get a message, but never resist them */
    else
        check_resist = false;

    /* Determine if the monster resisted or not, if appropriate */
    if (check_resist && does_resist(p, mon, effect_type, timer, flag))
    {
        resisted = true;
        m_note = MON_MSG_UNAFFECTED;
    }
    else
    {
        mon->m_timed[effect_type] = timer;

        if (visible)
        {
            struct source who_body;
            struct source *who = &who_body;

            source_monster(who, mon);
            update_health(who);
        }

        /* Update the visuals, as appropriate. */
        if (effect_type == MON_TMD_SLEEP) update_monlist(mon);
    }

    /*
     * Print a message if there is one, if the effect allows for it, and if
     * the monster is visible
     */
    if (m_note && !(flag & MON_TMD_FLG_NOMESSAGE) && (flag & MON_TMD_FLG_NOTIFY) && visible)
        add_monster_message(p, mon, m_note, true);

    return !resisted;
}


/* Minimum number of turns a new timed effect can last */
#define MON_INC_MIN_TURNS 2


/*
 * Increases the timed effect `effect_type` by `timer`.
 *
 * Calculates the new timer, then passes that to mon_set_timed().
 * Note that each effect has a maximum number of turns it can be active for.
 * If this function would put an effect timer over that cap, it sets it for
 * that cap instead.
 *
 * Returns true if the monster's timer changed.
 */
bool mon_inc_timed(struct player *p, struct monster *mon, int effect_type, int timer, int flag)
{
    struct mon_timed_effect *effect;
    int new_value = timer;

    my_assert(effect_type >= 0);
    my_assert(effect_type < MON_TMD_MAX);

    /* For negative amounts, we use mon_dec_timed instead */
    my_assert(timer > 0);

    if (p && p->timed[TMD_DESPAIR] && (effect_type == MON_TMD_FEAR)) timer *= 2;

    /* Make it last for a mimimum # of turns if it is a new effect */
    if (!mon->m_timed[effect_type] && (timer < MON_INC_MIN_TURNS)) timer = MON_INC_MIN_TURNS;

    /* Stack effects correctly */
    effect = &effects[effect_type];
    switch (effect->stacking)
    {
        case STACK_NO:
        {
            new_value = mon->m_timed[effect_type];
            if (new_value == 0) new_value = timer;
            break;
        }
        case STACK_MAX:
        {
            new_value = MAX(mon->m_timed[effect_type], timer);
            break;
        }
        case STACK_INCR:
        {
            /* New counter amount - prevent overflow */
            if (SHRT_MAX - timer < mon->m_timed[effect_type])
                new_value = SHRT_MAX;
            else
                new_value = mon->m_timed[effect_type] + timer;
            break;
        }
    }

    return mon_set_timed(p, mon, effect_type, new_value, flag);
}


/*
 * Decreases the timed effect `effect_type` by `timer`.
 *
 * Calculates the new timer, then passes that to mon_set_timed().
 * If a timer would be set to a negative number, it is set to 0 instead.
 * Note that decreasing a timed effect should never fail.
 *
 * Returns true if the monster's timer changed.
 */
bool mon_dec_timed(struct player *p, struct monster *mon, int effect_type, int timer, int flag)
{
    my_assert(effect_type >= 0);
    my_assert(effect_type < MON_TMD_MAX);

    /* For negative amounts, we use mon_inc_timed instead */
    my_assert(timer > 0);

    /* Decreasing is never resisted */
    flag |= MON_TMD_FLG_NOFAIL;

    /* New counter amount */
    timer = mon->m_timed[effect_type] - timer;
    if (timer < 0) timer = 0;

    return mon_set_timed(p, mon, effect_type, timer, flag);
}


/*
 * Clears the timed effect `effect_type`.
 *
 * Returns true if the monster's timer changed.
 */
bool mon_clear_timed(struct player *p, struct monster *mon, int effect_type, int flag)
{
    my_assert(effect_type >= 0);
    my_assert(effect_type < MON_TMD_MAX);

    if (!mon->m_timed[effect_type]) return false;

    /* Clearing never fails */
    flag |= MON_TMD_FLG_NOFAIL;

    return mon_set_timed(p, mon, effect_type, 0, flag);
}


/*
 * The level at which an effect is affecting a monster.
 * Levels range from 0 (unaffected) to 5 (maximum effect).
 */
int monster_effect_level(struct monster *mon, int effect_type)
{
    struct mon_timed_effect *effect = &effects[effect_type];
    int divisor = MAX(effect->max_timer / 5, 1);

    return MIN((mon->m_timed[effect_type] + divisor - 1) / divisor, 5);
}


int monster_effect_accuracy(struct monster *mon, int effect_type, int chance)
{
    int conf_level = monster_effect_level(mon, MON_TMD_CONF);
    int accuracy = 100;

    while (conf_level)
    {
        accuracy *= (100 - chance);
        accuracy /= 100;
        conf_level--;
    }

    return accuracy;
}
