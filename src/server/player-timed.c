/*
 * File: player-timed.c
 * Purpose: Timed effects handling
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2007 A Sidwell <andi@takkaria.org>
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
 * The "stun" and "cut" statuses need to be handled by special functions of
 * their own, as they are more complex than the ones handled by the generic
 * code.
 * The "bow branding", "adrenaline" and "biofeedback" statuses are also handled
 * in a special function...
 */
static bool set_stun(struct player *p, int v);
static bool set_cut(struct player *p, int v);
static bool set_bow_brand(struct player *p, int v);
static bool set_adrenaline(struct player *p, int v);
static bool set_biofeedback(struct player *p, int v);
static bool set_harmony(struct player *p, int v);


struct timed_effect
{
  const char *description;
  const char *on_begin, *on_end;
  const char *on_increase, *on_decrease;
  const char *near_begin, *near_end;
  u32b flag_redraw, flag_update;
  u16b msg;
  int fail_code;
  int fail;
};


static struct timed_effect effects[] =
{
    #define TMD(a, b, c, d, e, f, g, h, i, j, k, l, m) {b, c, d, e, f, g, h, i, j, k, l, m},
    #include "../common/list-player-timed.h"
    #undef TMD
    {NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0, 0}
};


static const char *timed_name_list[] =
{
    #define TMD(a, b, c, d, e, f, g, h, i, j, k, l, m) #a,
    #include "../common/list-player-timed.h"
    #undef TMD
    NULL
};


int timed_name_to_idx(const char *name)
{
    int i;

    for (i = 0; timed_name_list[i]; i++)
    {
        if (!my_stricmp(name, timed_name_list[i])) return i;
    }

    return -1;
}


const char *timed_idx_to_name(int type)
{
    my_assert(type >= 0);
    my_assert(type < TMD_MAX);

    return timed_name_list[type];
}


const char *timed_idx_to_desc(int type)
{
    my_assert(type >= 0);
    my_assert(type < TMD_MAX);

    return effects[type].description;
}


int timed_protect_flag(int type)
{
    return effects[type].fail;
}


/*
 * Set a timed event.
 */
bool player_set_timed(struct player *p, int idx, int v, bool notify)
{
    struct timed_effect *effect;
    bool result;

    /* Hack -- force good values */
    v = ((v > 10000)? 10000: ((v < 0)? 0: v));
    if ((idx < 0) || (idx > TMD_MAX)) return false;

    /* No change */
    if (p->timed[idx] == v) return false;

    /* Hack -- call other functions, reveal unaware players if noticed */
    if (idx == TMD_STUN)
    {
        result = set_stun(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    else if (idx == TMD_CUT)
    {
        result = set_cut(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    else if (idx == TMD_BOWBRAND)
    {
        result = set_bow_brand(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    else if (idx == TMD_ADRENALINE)
    {
        result = set_adrenaline(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    else if (idx == TMD_BIOFEEDBACK)
    {
        result = set_biofeedback(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }
    else if (idx == TMD_HARMONY)
    {
        result = set_harmony(p, v);
        if (result && p->k_idx) aware_player(p, p);
        return result;
    }

    /* Don't mention some effects */
    if ((idx == TMD_OPP_ACID) && player_is_immune(p, ELEM_ACID)) notify = false;
    if ((idx == TMD_OPP_ELEC) && player_is_immune(p, ELEM_ELEC)) notify = false;
    if ((idx == TMD_OPP_FIRE) && player_is_immune(p, ELEM_FIRE)) notify = false;
    if ((idx == TMD_OPP_COLD) && player_is_immune(p, ELEM_COLD)) notify = false;
    if ((idx == TMD_OPP_CONF) && player_of_has(p, OF_PROT_CONF)) notify = false;

    /* Find the effect */
    effect = &effects[idx];

    /* Always mention start or finish, otherwise on request */
    if (v == 0)
    {
        msg_misc(p, effect->near_end);
        msgt(p, MSG_RECOVER, effect->on_end);
        notify = true;
    }
    else if (p->timed[idx] == 0)
    {
        msg_misc(p, effect->near_begin);
        msgt(p, effect->msg, effect->on_begin);
        notify = true;
    }
    else if (notify)
    {
        /* Decrementing */
        if ((p->timed[idx] > v) && effect->on_decrease)
            msgt(p, effect->msg, effect->on_decrease);

        /* Incrementing */
        else if ((v > p->timed[idx]) && effect->on_increase)
            msgt(p, effect->msg, effect->on_increase);
    }

    /* Use the value */
    p->timed[idx] = v;

    /* Sort out the sprint effect */
    if ((idx == TMD_SPRINT) && (v == 0)) player_inc_timed(p, TMD_SLOW, 100, true, false);

    /* Nothing to notice */
    if (!notify) return false;

    /* Disturb */
    disturb(p, 0);

    /* Reveal unaware players */
    if (p->k_idx) aware_player(p, p);

    /* Update the visuals, as appropriate. */
    p->upkeep->update |= effect->flag_update;
    p->upkeep->redraw |= (PR_STATUS | effect->flag_redraw);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set a timed event permanently.
 */
static bool player_set_timed_perma(struct player *p, int idx)
{
    struct timed_effect *effect;
    bool notify = true;

    /* No change */
    if (p->timed[idx] == -1) return false;

    /* Don't mention some effects */
    if ((idx == TMD_OPP_ACID) && player_is_immune(p, ELEM_ACID)) notify = false;
    if ((idx == TMD_OPP_ELEC) && player_is_immune(p, ELEM_ELEC)) notify = false;
    if ((idx == TMD_OPP_FIRE) && player_is_immune(p, ELEM_FIRE)) notify = false;
    if ((idx == TMD_OPP_COLD) && player_is_immune(p, ELEM_COLD)) notify = false;

    /* Find the effect */
    effect = &effects[idx];

    /* Turning on, always mention */
    if (p->timed[idx] == 0)
    {
        msg_misc(p, effect->near_begin);
        msgt(p, effect->msg, effect->on_begin);
        notify = true;
    }

    /* Use the value */
    p->timed[idx] = -1;

    /* Nothing to notice */
    if (!notify) return false;

    /* Disturb */
    disturb(p, 0);

    /* Reveal unaware players */
    if (p->k_idx) aware_player(p, p);

    /* Update the visuals, as appropriate. */
    p->upkeep->update |= effect->flag_update;
    p->upkeep->redraw |= (PR_STATUS | effect->flag_redraw);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Increase the timed effect `idx` by `v`.  Mention this if `notify` is true.
 * Check for resistance to the effect if `check` is true.
 */
bool player_inc_timed_aux(struct player *p, struct monster *mon, int idx, int v, bool notify,
    bool check)
{
    struct timed_effect *effect;

    /* Check we have a valid effect */
    if ((idx < 0) || (idx > TMD_MAX)) return false;

    /* Find the effect */
    effect = &effects[idx];

    /* Check that @ can be affected by this effect */
    if (check && effect->fail_code)
    {
        /* If the effect is from a monster action, extra stuff happens */

        /* Determine whether an effect can be prevented by a flag */
        if (effect->fail_code == TMD_FAIL_FLAG_OBJECT)
        {
            /* Effect is inhibited by an object flag */
            equip_notice_flag(p, effect->fail);
            if (mon) update_smart_learn(mon, p, effect->fail, 0, -1);
            if (player_of_has(p, effect->fail))
            {
                if (mon) msg(p, "You resist the effect!");
                return false;
            }
        }
        else if (effect->fail_code == TMD_FAIL_FLAG_RESIST)
        {
            /* Effect is inhibited by a resist -- always learn */
            equip_notice_element(p, effect->fail);
            if (p->state.el_info[effect->fail].res_level > 0) return false;
        }
        else if (effect->fail_code == TMD_FAIL_FLAG_VULN)
        {
            /* Effect is inhibited by a vulnerability -- don't learn if temporary resistance */
            if (p->state.el_info[effect->fail].res_level < 0)
            {
                equip_notice_element(p, effect->fail);
                return false;
            }
        }

        /* Special case */
        if ((idx == TMD_POISONED) && p->timed[TMD_OPP_POIS]) return false;
    }

    /* Paralysis should be non-cumulative */
    if ((idx == TMD_PARALYZED) && (p->timed[idx] > 0)) return false;

    /* Hack -- permanent effect */
    if (p->timed[idx] == -1) return false;

    /* Handle polymorphed players */
    if (p->poly_race && (idx == TMD_AMNESIA))
    {
        if (rf_has(p->poly_race->flags, RF_EMPTY_MIND)) v /= 2;
        if (rf_has(p->poly_race->flags, RF_WEIRD_MIND)) v = v * 3 / 4;
    }

    /* Set v */
    v = v + p->timed[idx];

    return player_set_timed(p, idx, v, notify);
}


/*
 * Increase the timed effect `idx` by `v`.  Mention this if `notify` is true.
 * Check for resistance to the effect if `check` is true.
 */
bool player_inc_timed(struct player *p, int idx, int v, bool notify, bool check)
{
    return player_inc_timed_aux(p, NULL, idx, v, notify, check);
}


/*
 * Decrease the timed effect `idx` by `v`.  Mention this if `notify` is true.
 */
bool player_dec_timed(struct player *p, int idx, int v, bool notify)
{
    /* Check we have a valid effect */
    if ((idx < 0) || (idx > TMD_MAX)) return false;

    /* Set v */
    v = p->timed[idx] - v;

    return player_set_timed(p, idx, v, notify);
}


/*
 * Clear the timed effect `idx`.  Mention this if `notify` is true.
 */
bool player_clear_timed(struct player *p, int idx, bool notify)
{
    return player_set_timed(p, idx, 0, notify);
}


/*
 * Set "p->timed[TMD_STUN]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_stun(struct player *p, int v)
{
    int old_aux, new_aux; 
    bool notice = false;

    /* Hack -- the DM can not be stunned */
    if (p->dm_flags & DM_INVULNERABLE) return true;

    /* Hack -- force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Old state */
    if (p->timed[TMD_STUN] > 100)
        old_aux = 3; /* Knocked out */
    else if (p->timed[TMD_STUN] > 50)
        old_aux = 2; /* Heavy stun */
    else if (p->timed[TMD_STUN] > 0)
        old_aux = 1; /* Stun */
    else
        old_aux = 0; /* None */

    /* New state */
    if (v > 100)
        new_aux = 3; /* Knocked out */
    else if (v > 50)
        new_aux = 2; /* Heavy stun */
    else if (v > 0)
        new_aux = 1; /* Stun */
    else
        new_aux = 0; /* None */

    /* Increase or decrease stun */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Stun */
            case 1:
            {
                msg_misc(p, " appears stunned.");
                msgt(p, MSG_STUN, "You have been stunned.");
                break;
            }

            /* Heavy stun */
            case 2:
            {
                msg_misc(p, " appears heavily stunned.");
                msgt(p, MSG_STUN, "You have been heavily stunned.");
                break;
            }

            /* Knocked out */
            case 3:
            {
                msg_misc(p, " has been knocked out.");
                msgt(p, MSG_STUN, "You have been knocked out.");
                break;
            }
        }

        /* Notice */
        notice = true;
    }
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                msg_misc(p, " is no longer stunned.");
                msgt(p, MSG_RECOVER, "You are no longer stunned.");
                disturb(p, 0);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Use the value */
    p->timed[TMD_STUN] = v;

    /* No change */
    if (!notice) return false;

    /* Disturb and update */
    disturb(p, 0);
    p->upkeep->update |= (PU_BONUS);
    p->upkeep->redraw |= (PR_STATUS | PR_SPELL);
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_CUT]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_cut(struct player *p, int v)
{
    int old_aux, new_aux;
    bool notice = false;

    /* Hack -- force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Ghosts cannot bleed */
    if (p->ghost && (v > 0)) return true;

    /* Old state */
    if (p->timed[TMD_CUT] > TMD_CUT_DEEP)
        old_aux = 7; /* Mortal wound */
    else if (p->timed[TMD_CUT] > TMD_CUT_SEVERE)
        old_aux = 6; /* Deep gash */
    else if (p->timed[TMD_CUT] > TMD_CUT_NASTY)
        old_aux = 5; /* Severe cut */
    else if (p->timed[TMD_CUT] > TMD_CUT_BAD)
        old_aux = 4; /* Nasty cut */
    else if (p->timed[TMD_CUT] > TMD_CUT_LIGHT)
        old_aux = 3; /* Bad cut */
    else if (p->timed[TMD_CUT] > TMD_CUT_GRAZE)
        old_aux = 2; /* Light cut */
    else if (p->timed[TMD_CUT] > TMD_CUT_NONE)
        old_aux = 1; /* Graze */
    else
        old_aux = 0; /* None */

    /* New state */
    if (v > 1000)
        new_aux = 7; /* Mortal wound */
    else if (v > 200)
        new_aux = 6; /* Deep gash */
    else if (v > 100)
        new_aux = 5; /* Severe cut */
    else if (v > 50)
        new_aux = 4; /* Nasty cut */
    else if (v > 25)
        new_aux = 3; /* Bad cut */
    else if (v > 10)
        new_aux = 2; /* Light cut */
    else if (v > 0)
        new_aux = 1; /* Graze */
    else
        new_aux = 0; /* None */

    /* Increase or decrease cut */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Graze */
            case 1:
            {
                msg_misc(p, " has been given a graze.");
                msgt(p, MSG_CUT, "You have been given a graze.");
                break;
            }

            /* Light cut */
            case 2:
            {
                msg_misc(p, " has been given a light cut.");
                msgt(p, MSG_CUT, "You have been given a light cut.");
                break;
            }

            /* Bad cut */
            case 3:
            {
                msg_misc(p, " has been given a bad cut.");
                msgt(p, MSG_CUT, "You have been given a bad cut.");
                break;
            }

            /* Nasty cut */
            case 4:
            {
                msg_misc(p, " has been given a nasty cut.");
                msgt(p, MSG_CUT, "You have been given a nasty cut.");
                break;
            }

            /* Severe cut */
            case 5:
            {
                msg_misc(p, " has been given a severe cut.");
                msgt(p, MSG_CUT, "You have been given a severe cut.");
                break;
            }

            /* Deep gash */
            case 6:
            {
                msg_misc(p, " has been given a deep gash.");
                msgt(p, MSG_CUT, "You have been given a deep gash.");
                break;
            }

            /* Mortal wound */
            case 7:
            {
                msg_misc(p, " has been given a mortal wound.");
                msgt(p, MSG_CUT, "You have been given a mortal wound.");
                break;
            }
        }

        /* Notice */
        notice = true;
    }
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                msg_misc(p, " is no longer bleeding.");
                msgt(p, MSG_RECOVER, "You are no longer bleeding.");
                disturb(p, 0);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Use the value */
    p->timed[TMD_CUT] = v;

    /* No change */
    if (!notice) return false;

    /* Disturb and update */
    disturb(p, 0);
    p->upkeep->redraw |= (PR_STATUS);
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_BOWBRAND]", notice observable changes
 */
static bool set_bow_brand(struct player *p, int v)
{
    bool notice = false;

    /* Hack -- force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v)
    {
        if (!p->timed[TMD_BOWBRAND])
        {
            switch (p->brand.type)
            {
                case GF_ELEC:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow deep blue.");
                        msg(p, "Your missiles glow deep blue!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with lightning.");
                        msg(p, "Your missiles are covered with lightning!");
                    }
                    break;
                }
                case GF_COLD:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow bright white.");
                        msg(p, "Your missiles glow bright white!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with frost.");
                        msg(p, "Your missiles are covered with frost!");
                    }
                    break;
                }
                case GF_FIRE:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow deep red.");
                        msg(p, "Your missiles glow deep red!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with fire.");
                        msg(p, "Your missiles are covered with fire!");
                    }
                    break;
                }
                case GF_ACID:
                {
                    if (p->brand.blast)
                    {
                        msg_misc(p, "'s missiles glow pitch black.");
                        msg(p, "Your missiles glow pitch black!");
                    }
                    else
                    {
                        msg_misc(p, "'s missiles are covered with acid.");
                        msg(p, "Your missiles are covered with acid!");
                    }
                    break;
                }
                case GF_OLD_CONF:
                    msg_misc(p, "'s missiles glow many colors.");
                    msg(p, "Your missiles glow many colors!");
                    break;
                case GF_POIS:
                    msg_misc(p, "'s missiles are covered with venom.");
                    msg(p, "Your missiles are covered with venom!");
                    break;
                case GF_ARROW_X:
                    msg_misc(p, "'s missiles sharpen.");
                    msg(p, "Your missiles sharpen!");
                    break;
                case GF_SHARD:
                    msg_misc(p, "'s missiles become explosive.");
                    msg(p, "Your missiles become explosive!");
                    break;
                case GF_MISSILE:
                    msg_misc(p, "'s missiles glow with power.");
                    msg(p, "Your missiles glow with power!");
                    break;
                case GF_SOUND:
                    msg_misc(p, "'s missiles vibrate in a strange way.");
                    msg(p, "Your missiles vibrate in a strange way!");
                    break;
            }
            notice = true;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_BOWBRAND])
        {
            msg_misc(p, "'s missiles seem normal again.");
            msg(p, "Your missiles seem normal again.");
            notice = true;
        }
    }

    /* Use the value */
    p->timed[TMD_BOWBRAND] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Disturb */
    disturb(p, 0);

    /* Redraw the "brand" */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_ADRENALINE]", notice observable changes
 * Note the interaction with biofeedback
 */
static bool set_adrenaline(struct player *p, int v)
{
    int old_aux = 0, new_aux = 0;
    bool notice = false;

    /* Hack -- force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Limit duration (100 turns / 20 turns at 5th stage) */
    if (v > 100)
    {
        v = 100;

        /* Too much adrenaline causes damage */
        msg(p, "Your body can't handle that much adrenaline!");
        my_strcpy(p->died_flavor, "had a heart attack due to too much adrenaline",
            sizeof(p->died_flavor));
        take_hit(p, damroll(2, v), "adrenaline poisoning", false);
        notice = true;
    }

    /* Different stages */
    if (p->timed[TMD_ADRENALINE] > 0) old_aux = 1 + (p->timed[TMD_ADRENALINE] - 1) / 20;
    if (v > 0) new_aux = 1 + (v - 1) / 20;

    /* Increase stage */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Berserk strength effect */
            case 1:
            {
                msg_misc(p, "'s veins are flooded with adrenaline.");
                msg(p, "Adrenaline surges through your veins!");
                hp_player(p, 30);
                player_clear_timed(p, TMD_AFRAID, true);
                player_set_timed_perma(p, TMD_BOLD);
                player_set_timed_perma(p, TMD_SHERO);

                /* Adrenaline doesn't work well when biofeedback is activated */
                if (p->timed[TMD_BIOFEEDBACK])
                {
                    player_clear_timed(p, TMD_BIOFEEDBACK, true);
                    my_strcpy(p->died_flavor, "had a heart attack due to too much adrenaline",
                        sizeof(p->died_flavor));
                    take_hit(p, damroll(2, v), "adrenaline poisoning", false);
                }
                break;
            }

            /* Increase Str/Dex/Con */
            case 2:
            {
                msg_misc(p, "feels powerful.");
                msg(p, "You feel powerful!");
                break;
            }

            /* Increase Str/Dex/Con + increase to-dam */
            case 3:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                msg_misc(p, "'s hands glow red.");
                msg(p, "Your hands glow red!");
                break;
            }

            /* Increase Str/Dex/Con + increase attack speed */
            case 4:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                msg_misc(p, "'s hands tingle.");
                msg(p, "Your hands tingle!");
                break;
            }

            /* Increase Str/Dex/Con + haste effect */
            case 5:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_FAST);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Decrease stage */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                msg_misc(p, "'s veins are drained of adrenaline.");
                msg(p, "The adrenaline drains out of your veins.");
                player_clear_timed(p, TMD_BOLD, true);
                player_clear_timed(p, TMD_SHERO, true);
                break;
            }

            /* Decrease Str/Dex/Con */
            case 1:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                break;
            }

            /* Decrease Str/Dex/Con + decrease to-dam */
            case 2:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                msg_misc(p, "'s hands stop glowing.");
                msg(p, "Your hands stop glowing.");
                break;
            }

            /* Decrease Str/Dex/Con + decrease attack speed */
            case 3:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                msg_misc(p, "'s hands ache.");
                msg(p, "Your hands ache.");
                break;
            }

            /* Decrease Str/Dex/Con + lose haste effect */
            case 4:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_FAST, true);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Use the value */
    p->timed[TMD_ADRENALINE] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Notice */
    p->upkeep->update |= (PU_BONUS);

    /* Disturb */
    disturb(p, 0);

    /* Redraw the "adrenaline" */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_BIOFEEDBACK]", notice observable changes
 * Note the interaction with adrenaline
 */
static bool set_biofeedback(struct player *p, int v)
{
    bool notice = false;

    /* Hack -- force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v)
    {
        if (!p->timed[TMD_BIOFEEDBACK])
        {
            msg_misc(p, "'s pulse slows down.");
            msg(p, "Your pulse slows down!");

            /* Biofeedback doesn't work well when adrenaline is activated */
            if (p->timed[TMD_ADRENALINE])
            {
                player_clear_timed(p, TMD_ADRENALINE, true);
                if (one_in_(8))
                {
                    msg(p, "You feel weak and tired!");
                    player_inc_timed(p, TMD_SLOW, randint0(4) + 4, true, false);
                    if (one_in_(5))
                        player_inc_timed(p, TMD_PARALYZED, randint0(4) + 4, true, false);
                    if (one_in_(3)) player_inc_timed(p, TMD_STUN, randint1(30), true, false);
                }
            }
            notice = true;
        }

        /* Biofeedback can't reach high values */
        if (v > 35 + p->lev)
        {
            msg(p, "You speed up your pulse to avoid fainting!");
            v = 35 + p->lev;
            notice = true;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_BIOFEEDBACK])
        {
            msg_misc(p, "'s pulse speeds up.");
            msg(p, "You lose control of your blood flow.");
            notice = true;
        }

    }

    /* Use the value */
    p->timed[TMD_BIOFEEDBACK] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Notice */
    p->upkeep->update |= (PU_BONUS);

    /* Disturb */
    disturb(p, 0);

    /* Redraw the "biofeedback" */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->timed[TMD_HARMONY]", notice observable changes
 */
static bool set_harmony(struct player *p, int v)
{
    int old_aux = 0, new_aux = 0;
    bool notice = false;

    /* Hack -- force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Limit duration (100 turns / 20 turns at 5th stage) */
    if (v > 100) v = 100;

    /* Different stages */
    if (p->timed[TMD_HARMONY] > 0) old_aux = 1 + (p->timed[TMD_HARMONY] - 1) / 20;
    if (v > 0) new_aux = 1 + (v - 1) / 20;

    /* Increase stage */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Bless effect */
            case 1:
            {
                msg_misc(p, "feels attuned to nature.");
                msg(p, "You feel attuned to nature!");
                player_set_timed_perma(p, TMD_BLESSED);
                break;
            }

            /* Increase Str/Dex/Con */
            case 2:
            {
                msg_misc(p, "feels powerful.");
                msg(p, "You feel powerful!");
                break;
            }

            /* Increase Str/Dex/Con + shield effect */
            case 3:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_SHIELD);
                break;
            }

            /* Increase Str/Dex/Con + resistance effect */
            case 4:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_OPP_ACID);
                player_set_timed_perma(p, TMD_OPP_ELEC);
                player_set_timed_perma(p, TMD_OPP_FIRE);
                player_set_timed_perma(p, TMD_OPP_COLD);
                player_set_timed_perma(p, TMD_OPP_POIS);
                break;
            }

            /* Increase Str/Dex/Con + haste effect */
            case 5:
            {
                msg_misc(p, "feels more powerful.");
                msg(p, "You feel more powerful!");
                player_set_timed_perma(p, TMD_FAST);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Decrease stage */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                msg_misc(p, "feels less attuned to nature.");
                msg(p, "You feel less attuned to nature.");
                player_clear_timed(p, TMD_BLESSED, true);
                break;
            }

            /* Decrease Str/Dex/Con */
            case 1:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                break;
            }

            /* Decrease Str/Dex/Con + lose shield effect */
            case 2:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_SHIELD, true);
                break;
            }

            /* Decrease Str/Dex/Con + lose resistance effect */
            case 3:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_OPP_ACID, true);
                player_clear_timed(p, TMD_OPP_ELEC, true);
                player_clear_timed(p, TMD_OPP_FIRE, true);
                player_clear_timed(p, TMD_OPP_COLD, true);
                player_clear_timed(p, TMD_OPP_POIS, true);
                break;
            }

            /* Decrease Str/Dex/Con + lose haste effect */
            case 4:
            {
                msg_misc(p, "feels less powerful.");
                msg(p, "You feel less powerful.");
                player_clear_timed(p, TMD_FAST, true);
                break;
            }
        }

        /* Notice */
        notice = true;
    }

    /* Use the value */
    p->timed[TMD_HARMONY] = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Notice */
    p->upkeep->update |= (PU_BONUS);

    /* Disturb */
    disturb(p, 0);

    /* Redraw the status */
    p->upkeep->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return true;
}


/*
 * Set "p->food", notice observable changes
 *
 * The "p->food" variable can get as large as 17000, allowing the
 * addition of the most "filling" item, Elvish Waybread, which adds
 * 7500 food units, without overflowing the 32767 maximum limit.
 *
 * Perhaps we should disturb the player with various messages,
 * especially messages about hunger status changes.  XXX XXX XXX
 *
 * Digestion of food is handled in "dungeon.c", in which, normally,
 * the player digests about 20 food units per 100 game turns, more
 * when "fast", more when "regenerating", less with "slow digestion".
 */
bool player_set_food(struct player *p, int v)
{
    int old_aux, new_aux;
    bool notice = false;

    p->starving = false;

    /* Hack -- force good values */
    v = MIN(v, PY_FOOD_MAX);
    v = MAX(v, 0);

    /* Current value */
    if (p->food < PY_FOOD_FAINT) old_aux = 0;
    else if (p->food < PY_FOOD_WEAK) old_aux = 1;
    else if (p->food < PY_FOOD_ALERT) old_aux = 2;
    else if (p->food < PY_FOOD_FULL) old_aux = 3;
    else old_aux = 4;

    /* New value */
    if (v < PY_FOOD_FAINT) new_aux = 0;
    else if (v < PY_FOOD_WEAK) new_aux = 1;
    else if (v < PY_FOOD_ALERT) new_aux = 2;
    else if (v < PY_FOOD_FULL) new_aux = 3;
    else new_aux = 4;

    /* Change */
    if (new_aux != old_aux) notice = true;

    /* Hack -- do not display message for ghosts */
    if (p->ghost) old_aux = new_aux;

    /* Food increase or decrease */
    if (new_aux > old_aux)
    {
        switch (new_aux)
        {
            case 1:
                msg(p, "You are still weak.");
                break;
            case 2:
                msg(p, "You are still hungry.");
                break;
            case 3:
                msg(p, "You are no longer hungry.");
                break;
            case 4:
                msg(p, "You are full!");
                break;
        }
    }
    else if (new_aux < old_aux)
    {
        switch (new_aux)
        {
            case 0:
            {
                msgt(p, MSG_NOTICE, "You are getting faint from hunger!");

                /*
                 * If the player is at full hit points,
                 * destroy his connection (this will hopefully prevent
                 * people from starving while afk)
                 */
                if (p->chp == p->mhp) p->starving = true;
                break;
            }
            case 1:
                msgt(p, MSG_NOTICE, "You are getting weak from hunger!");
                break;
            case 2:
                msgt(p, MSG_HUNGRY, "You are getting hungry.");
                break;
            case 3:
                msgt(p, MSG_NOTICE, "You are no longer full.");
                break;
        }
    }

    /* Use the value */
    p->food = v;

    /* Nothing to notice */
    if (!notice) return false;

    /* Disturb and update */
    disturb(p, 0);
    p->upkeep->update |= (PU_BONUS);
    p->upkeep->redraw |= (PR_STATUS);
    handle_stuff(p);

    /* Result */
    return true;
}
