/*
 * File: mon-timed.c
 * Purpose: Monster timed effects.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "mon-msg.h"
#include "mon-spell.h"
#include "mon-timed.h"
#include "mon-util.h"


typedef struct
{
    int message_begin;
    int message_end;
    int message_increase;
    u32b flag_resist;
    int max_timer;
} mon_timed_effect;


/*
 * Monster timed effects.
 * '0' means no message.
 */
static mon_timed_effect effects[] =
{
    {MON_MSG_FALL_ASLEEP, MON_MSG_WAKES_UP, FALSE, RF_NO_SLEEP, 10000},
    {MON_MSG_DAZED, MON_MSG_NOT_DAZED, MON_MSG_MORE_DAZED, RF_NO_STUN, 200},
    {MON_MSG_CONFUSED, MON_MSG_NOT_CONFUSED, MON_MSG_MORE_CONFUSED, RF_NO_CONF, 200},
    {MON_MSG_FLEE_IN_TERROR, MON_MSG_NOT_AFRAID, MON_MSG_MORE_AFRAID, RF_NO_FEAR, 10000},
    {MON_MSG_SLOWED, MON_MSG_NOT_SLOWED, MON_MSG_MORE_SLOWED, 0L, 50},
    {MON_MSG_HASTED, MON_MSG_NOT_HASTED, MON_MSG_MORE_HASTED, 0L, 50},
    {MON_MSG_POISONED, MON_MSG_NOT_POISONED, MON_MSG_MORE_POISONED, RF_IM_POIS, 10000},
    {MON_MSG_BLEED, MON_MSG_NOT_BLEEDING, MON_MSG_MORE_BLEEDING, 0L, 10000},
    {MON_MSG_BLIND, MON_MSG_NOT_BLIND, MON_MSG_MORE_BLIND, RF_NO_SLEEP, 200},
    {MON_MSG_PARALYZED, MON_MSG_NOT_PARALYZED, MON_MSG_MORE_PARALYZED, RF_NO_STUN, 200}
};


/*
 * Determines whether the given monster successfully resists the given effect.
 *
 * If MON_TMD_FLG_NOFAIL is set in `flag`, this returns FALSE.
 * Then we determine if the monster resists the effect for some racial
 * reason. For example, the monster might have the NO_SLEEP flag, in which
 * case it always resists sleep. Or if it breathes chaos, it always resists
 * confusion. If the given monster doesn't resist for any of these reasons,
 * then it makes a saving throw. If MON_TMD_MON_SOURCE is set in `flag`,
 * indicating that another monster caused this effect, then the chance of
 * success on the saving throw just depends on the monster's native depth.
 * Otherwise, the chance of success decreases as `timer` increases.
 *
 * Also marks the lore for any appropriate resists.
 */
static bool mon_resist_effect(struct player *p, const monster_type *m_ptr, int ef_idx, int timer,
    u16b flag)
{
    mon_timed_effect *effect;
    int resist_chance;
    const monster_race *r_ptr;
    monster_lore *l_ptr;
    bool visible;

    my_assert((ef_idx >= 0) && (ef_idx < MON_TMD_MAX));
    effect = &effects[ef_idx];

    my_assert(m_ptr);
    r_ptr = &r_info[m_ptr->r_idx];
    l_ptr = (p? &p->lore[m_ptr->r_idx]: NULL);
    visible = (p? p->mon_vis[m_ptr->midx]: FALSE);

    /* Hasting never fails */
    if (ef_idx == MON_TMD_FAST) return (FALSE);

    /* Some effects are marked to never fail */
    if (flag & MON_TMD_FLG_NOFAIL) return (FALSE);

    /* A sleeping monster resists further sleeping */
    if ((ef_idx == MON_TMD_SLEEP) && m_ptr->m_timed[ef_idx]) return (TRUE);

    /* If the monster resists innately, learn about it */
    if (rf_has(r_ptr->flags, effect->flag_resist))
    {
        if (visible) rf_on(l_ptr->flags, effect->flag_resist);

        return (TRUE);
    }

    /* Monsters with specific breaths resist stunning/paralysis */
    if (((ef_idx == MON_TMD_STUN) || (ef_idx == MON_TMD_HOLD)) &&
        (rsf_has(r_ptr->spell_flags, RSF_BR_SOUN) || rsf_has(r_ptr->spell_flags, RSF_BR_WALL)))
    {
        /* Add the lore */
        if (visible)
        {
            if (rsf_has(r_ptr->spell_flags, RSF_BR_SOUN))
                rsf_on(l_ptr->spell_flags, RSF_BR_SOUN);
            if (rsf_has(r_ptr->spell_flags, RSF_BR_WALL))
                rsf_on(l_ptr->spell_flags, RSF_BR_WALL);
        }

        return (TRUE);
    }

    /* Monsters with specific breaths resist confusion */
    if ((ef_idx == MON_TMD_CONF) && rsf_has(r_ptr->spell_flags, RSF_BR_CHAO))
    {
        /* Add the lore */
        if (visible)
        {
            if (rsf_has(r_ptr->spell_flags, RSF_BR_CHAO))
                rsf_on(l_ptr->spell_flags, RSF_BR_CHAO);
        }

        return (TRUE);
    }

    /* Monsters with specific breaths resist cut */
    if ((ef_idx == MON_TMD_CUT) && rsf_has(r_ptr->spell_flags, RSF_BR_SHAR))
    {
        /* Add the lore */
        if (visible)
        {
            if (rsf_has(r_ptr->spell_flags, RSF_BR_SHAR))
                rsf_on(l_ptr->spell_flags, RSF_BR_SHAR);
        }

        return (TRUE);
    }

    /* Inertia breathers resist slowing */
    if ((ef_idx == MON_TMD_SLOW) && rsf_has(r_ptr->spell_flags, RSF_BR_INER))
    {
        if (l_ptr) rsf_on(l_ptr->spell_flags, RSF_BR_INER);
        return (TRUE);
    }

    /* Hack -- Sleep uses much bigger numbers */
    if (ef_idx == MON_TMD_SLEEP) timer /= 25;

    /* Calculate the chance of the monster making its saving throw. */
    if (flag & MON_TMD_MON_SOURCE)
        resist_chance = r_ptr->level;
    else
        resist_chance = r_ptr->level + 40 - (timer / 2);

    if (randint0(100) < resist_chance) return (TRUE);

    /* Uniques are doubly hard to affect */
    if (rf_has(r_ptr->flags, RF_UNIQUE))
    {
        if (randint0(100) < resist_chance) return (TRUE);
    }

    return (FALSE);
}


/*
 * Attempts to set the timer of the given monster effect to `timer`.
 *
 * Checks to see if the monster resists the effect, using mon_resist_effect().
 * If not, the effect is set to `timer` turns. If `timer` is 0, or if the
 * effect timer was 0, or if MON_TMD_FLG_NOTIFY is set in `flag`, then a
 * message is printed, unless MON_TMD_FLG_NOMESSAGE is set in `flag`.
 *
 * Give messages if the right flags are set.
 * Check if the monster is able to resist the spell.  Mark the lore.
 * Returns TRUE if the monster was affected.
 * Return FALSE if the monster was unaffected.
 */
static bool mon_set_timed(struct player *p, monster_type *m_ptr, int ef_idx, int timer, u16b flag,
    bool id)
{
    mon_timed_effect *effect;
    int m_note = 0;
    int resisted;
    int old_timer;
    bool visible = FALSE;

    my_assert((ef_idx >= 0) && (ef_idx < MON_TMD_MAX));
    effect = &effects[ef_idx];

    my_assert(m_ptr);
    old_timer = m_ptr->m_timed[ef_idx];
    if (p) visible = (p->mon_vis[m_ptr->midx] && !m_ptr->unaware);

    /* Ignore dead monsters */
    if (!m_ptr->r_idx) return FALSE;

    /* No change */
    if (old_timer == timer) return FALSE;

    /* Turning off, usually mention */
    if (timer == 0)
    {
        m_note = effect->message_end;
        flag |= MON_TMD_FLG_NOTIFY;
    }

    /* Turning on, usually mention */
    else if (old_timer == 0)
    {
        flag |= MON_TMD_FLG_NOTIFY;
        m_note = effect->message_begin;
    }

    /* Different message for increases, but don't automatically mention. */
    else if (timer > old_timer)
        m_note = effect->message_increase;

    /* Determine if the monster resisted or not */
    resisted = mon_resist_effect(p, m_ptr, ef_idx, timer, flag);

    if (resisted)
        m_note = MON_MSG_UNAFFECTED;
    else
        m_ptr->m_timed[ef_idx] = timer;

    if (visible) update_health(m_ptr->midx);

    /* Update the visuals, as appropriate. */
    if (ef_idx == MON_TMD_SLEEP) update_monlist(m_ptr);

    /*
     * Print a message if there is one, if the effect allows for it, and if
     * either the monster is visible, or we're trying to ID something
     */
    if (m_note && (visible || id) && !(flag & MON_TMD_FLG_NOMESSAGE) && (flag & MON_TMD_FLG_NOTIFY))
    {
        char m_name[NORMAL_WID];

        monster_desc(p, m_name, sizeof(m_name), m_ptr, 0x04);
        add_monster_message(p, m_name, m_ptr, m_note, TRUE);
    }

    return !resisted;
}


/*
 * Increases the timed effect `ef_idx` by `timer`.
 *
 * Calculates the new timer, then passes that to mon_set_timed().
 * Note that each effect has a maximum number of turns it can be active for.
 * If this function would put an effect timer over that cap, it sets it for
 * that cap instead.
 *
 * Returns TRUE if the monster's timer changed.
 */
bool mon_inc_timed(struct player *p, struct monster *m_ptr, int ef_idx, int timer, u16b flag,
    bool id)
{
   mon_timed_effect *effect;

    my_assert((ef_idx >= 0) && (ef_idx < MON_TMD_MAX));
    effect = &effects[ef_idx];

    /* For negative amounts, we use mon_dec_timed instead */
    my_assert(timer > 0);

    /* Make it last for a mimimum # of turns if it is a new effect */
    if (!m_ptr->m_timed[ef_idx] && (timer < 2)) timer = 2;

    /* New counter amount - prevent overflow */
    if (MAX_SHORT - timer < m_ptr->m_timed[ef_idx])
        timer = MAX_SHORT;
    else
        timer += m_ptr->m_timed[ef_idx];

    /* Reduce to max_timer if necessary */
    if (timer > effect->max_timer) timer = effect->max_timer;

    return mon_set_timed(p, m_ptr, ef_idx, timer, flag, id);
}


/*
 * Decreases the timed effect `ef_idx` by `timer`.
 *
 * Calculates the new timer, then passes that to mon_set_timed().
 * If a timer would be set to a negative number, it is set to 0 instead.
 * Note that decreasing a timed effect should never fail.
 *
 * Returns TRUE if the monster's timer changed.
 */
bool mon_dec_timed(struct player *p, struct monster *m_ptr, int ef_idx, int timer, u16b flag, bool id)
{
    my_assert((ef_idx >= 0) && (ef_idx < MON_TMD_MAX));

    my_assert(timer > 0);

    /* Decreasing is never resisted */
    flag |= MON_TMD_FLG_NOFAIL;

    /* New counter amount */
    timer = m_ptr->m_timed[ef_idx] - timer;
    if (timer < 0) timer = 0;

    return mon_set_timed(p, m_ptr, ef_idx, timer, flag, id);
}


/*
 * Clears the timed effect `ef_idx`.
 *
 * Returns TRUE if the monster's timer changed.
 */
bool mon_clear_timed(struct player *p, struct monster *m_ptr, int ef_idx, u16b flag, bool id)
{
    my_assert((ef_idx >= 0) && (ef_idx < MON_TMD_MAX));

    if (!m_ptr->m_timed[ef_idx]) return FALSE;

    /* Clearing never fails */
    flag |= MON_TMD_FLG_NOFAIL;

    return mon_set_timed(p, m_ptr, ef_idx, 0, flag, id);
}