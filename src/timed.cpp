
/*
 * File: timed.cpp
 * Purpose: Timed effects handling
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2007 A Sidwell <andi@takkaria.org>, Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "src/npp.h"

typedef struct
{
  QString on_begin;
  QString on_end;
  QString on_increase;
  QString on_decrease;
  u32b flag_redraw, flag_update, flag_window;
  int msg;
  bool disable_repeat;
} timed_effect;


/*
 * The "stun" and "cut" statuses need to be handled by special functions of
 * their own, as they are more complex than the ones handled by the generic
 * code.
 */

static timed_effect effects[] =
{
    /*TMD_FAST*/
    { "You feel yourself moving faster!", "You feel yourself slow down.",
            NULL, NULL,
            0, PU_BONUS, 0, MSG_SPEED, FALSE },
    /*TMD_SLOW*/
    { "You feel yourself moving slower!", "You feel yourself speed up.",
            NULL, NULL,
            0, PU_BONUS, 0, MSG_SLOW, FALSE  },
    /*TMD_BLIND*/
    { "You are blind.", "You blink and your eyes clear.",
            NULL, NULL,
            PR_MAP, PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS, 0, MSG_BLIND, TRUE },
    /*TMD_PARALYZED*/
    { "You are paralysed!", "You can move again.",
            NULL, NULL,
            PR_STATUSBAR, 0, 0, MSG_PARALYZED, TRUE },
    /*TMD_CONFUSED*/
    { "You are confused!", "You are no longer confused.",
            "You are more confused!", "You feel a little less confused.",
            0, 0, 0, MSG_CONFUSED, TRUE },
    /*TMD_AFRAID*/
    { "You are terrified!", "You feel bolder now.",
            "You are more scared!", "You feel a little less scared.",
            0, PU_BONUS, 0, MSG_AFRAID, FALSE },
    /*TMD_IMAGE*/
    { "You feel drugged!", "You can see clearly again.",
            "You feel more drugged!", "You feel less drugged.",
            (PR_MAP | PR_WIN_OBJLIST), 0 , 0, MSG_DRUGGED, TRUE },
    /*TMD_POISONED*/
    { "You are poisoned!", "You are no longer poisoned.",
            "You are more poisoned!", "You are less poisoned.",
            0, 0, 0, MSG_POISONED, FALSE  },
    /* TMD_CUT -- handled seperately */
    { NULL, NULL, NULL, NULL, 0, 0, 0 ,0, FALSE },
    /* TMD_STUN -- handled seperately */
    { NULL, NULL, NULL, NULL, 0, 0, 0 ,0, FALSE },
    /*TMD_PROTEVIL*/
    { "You feel safe from evil!", "You no longer feel safe from evil.",
            "You feel even safer from evil!", "You feel less safe from evil.",
            0, 0, 0, MSG_PROT_EVIL, FALSE  },
    /* TMD_INVULN */
    { "Your skin turns into steel!", "Your skin returns to normal.",
            NULL, NULL,
            0, PU_BONUS, 0, MSG_INVULN, FALSE  },
    /* TMD_HERO */
    { "You feel like a hero!", "You no longer feel heroic.",
            "You feel more like a hero!", "You feel less heroic.",
            0, PU_BONUS, 0, MSG_HERO, FALSE  },
    /* TMD_BERSERK */
    { "You feel like a killing machine!", "You no longer feel berserk.",
            "You feel even more berserk!", "You feel less berserk.",
            0, PU_BONUS, 0, MSG_BERSERK, FALSE  },
    /* TMD_SHIELD */
    { "A mystic shield forms around your body!", "Your mystic shield crumbles away.",
            "The mystic shield strengthens.", "The mystic shield weakens.",
            0, PU_BONUS, 0, MSG_SHIELD, FALSE  },
    /* TMD_BLESSED */
    { "You feel righteous!", "The prayer has expired.",
            "You feel more righteous!", "You feel less righteous.",
            0, PU_BONUS, 0, MSG_BLESSED, FALSE  },
    /* TMD_SINVIS */
    { "Your eyes feel very sensitive!", "Your eyes no longer feel so sensitive.",
            "Your eyes feel more sensitive!", "Your eyes feel less sensitive.",
            0, (PU_BONUS | PU_MONSTERS), 0, MSG_SEE_INVIS, FALSE  },
    /* TMD_SINFRA */
    { "Your eyes begin to tingle!", "Your eyes stop tingling.",
            "Your eyes' tingling intensifies.", "Your eyes tingle less.",
            0, (PU_BONUS | PU_MONSTERS), 0, MSG_INFRARED, FALSE  },
    /* TMD_OPP_ACID */
    { "You feel resistant to acid!", "You are no longer resistant to acid.",
            "You feel more resistant to acid!", "You feel less resistant to acid.",
            PR_STATUSBAR, 0, 0, MSG_RES_ACID, FALSE  },
    /* TMD_OPP_ELEC */
    { "You feel resistant to electricity!", "You are no longer resistant to electricity.",
            "You feel more resistant to electricity!", "You feel less resistant to electricity.",
            PR_STATUSBAR, 0, 0, MSG_RES_ELEC, FALSE  },
    /* TMD_OPP_FIRE */
    { "You feel resistant to fire!", "You are no longer resistant to fire.",
            "You feel more resistant to fire!", "You feel less resistant to fire.",
            PR_STATUSBAR, 0, 0, MSG_RES_FIRE, FALSE  },
    /* TMD_OPP_COLD */
    { "You feel resistant to cold!", "You are no longer resistant to cold.",
            "You feel more resistant to cold!", "You feel less resistant to cold.",
            PR_STATUSBAR, 0, 0, MSG_RES_COLD, FALSE  },
    /* TMD_OPP_POIS */
    { "You feel resistant to poison!", "You are no longer resistant to poison.",
            "You feel more resistant to poison!", "You feel less resistant to poison.",
            0, 0, 0, MSG_RES_POIS, FALSE  },
    /* TMD_FLYING */
    { "You take flight!", "Your mystic wings disappear.",
            NULL, NULL,
            (PR_STATUSBAR), PU_STEALTH, 0, 0, FALSE  },
    /* TMD_NAT_LAVA */
    { "You feel native to lava terrains!", "You no longer feel native to lava terrains.",
            NULL, NULL,
            0, PU_BONUS, 0, 0, FALSE  },
    /* TMD_NAT_OIL */
    { "You feel native to oil terrains!", "You no longer feel native to oil terrains.",
            NULL, NULL,
            0, PU_BONUS, 0, 0, FALSE  },
    /* TMD_NAT_SAND */
    { "You feel native to sandy terrains!", "You no longer feel native to sandy terrains.",
            NULL, NULL,
            0, PU_BONUS, 0, 0 , FALSE },
    /* TMD_NAT_TREE */
    { "You feel native to forest terrains!", "You no longer feel native to forest terrains.",
            NULL, NULL,
            0, PU_BONUS, 0, 0, FALSE  },
    /* TMD_NAT_WATER */
    { "You feel native to water terrains!", "You no longer feel native to water terrains.",
            NULL, NULL,
            0, PU_BONUS, 0, 0, FALSE  },
    /* TMD_NAT_MUD */
    { "You feel native to muddy terrains!", "You no longer feel native to muddy terrains.",
            NULL, NULL,
            0, PU_BONUS, 0, 0, FALSE  },
    /* TMD_SLAY_ELEM */
    { "Your weapon glows with many colors!", "Your weapon returns to normal.",
            NULL, NULL,
            PR_STATUSBAR, 0, 0, 0, FALSE  },
    /* TMD_CALL_HOURNS */
    { "You try to awake the trees around you!", "The trees are asleep now.",
            NULL, NULL,
            0, 0, 0, 0, FALSE  },
};


/* Returns true for a temporary effect if the player has permanent protection. */
bool redundant_timed_event(int idx)
{
    /* Don't mention some effects. */
    if (idx == TMD_OPP_ACID && p_ptr->state.immune_acid) return (TRUE);
    if (idx == TMD_OPP_ELEC && p_ptr->state.immune_elec) return (TRUE);
    if (idx == TMD_OPP_FIRE && p_ptr->state.immune_fire) return (TRUE);
    if (idx == TMD_OPP_COLD && p_ptr->state.immune_cold) return (TRUE);
    if (idx == TMD_OPP_POIS && p_ptr->state.immune_pois) return (TRUE);
    if (idx == TMD_NAT_LAVA && (p_ptr->state.p_flags_native_no_temp & P_NATIVE_LAVA)) return (TRUE);
    if (idx == TMD_NAT_OIL && (p_ptr->state.p_flags_native_no_temp & P_NATIVE_OIL)) return (TRUE);
    if (idx == TMD_NAT_SAND && (p_ptr->state.p_flags_native_no_temp & P_NATIVE_SAND)) return (TRUE);
    if (idx == TMD_NAT_TREE && (p_ptr->state.p_flags_native_no_temp & P_NATIVE_FOREST)) return (TRUE);
    if (idx == TMD_NAT_WATER && (p_ptr->state.p_flags_native_no_temp & P_NATIVE_WATER)) return (TRUE);
    if (idx == TMD_NAT_MUD && (p_ptr->state.p_flags_native_no_temp & P_NATIVE_MUD)) return (TRUE);
    if (idx == TMD_SINVIS && p_ptr->state.see_inv_perm) return (TRUE);

    return (FALSE);
}

/*
 * Set a timed event (except timed resists, cutting and stunning).
 */
bool set_timed(int idx, int v, bool notify)
{
    timed_effect *effect;
    bool override_notify = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;
    if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

    /* No change */
    if (p_ptr->timed[idx] == v) return FALSE;

    /* Hack -- call other functions */
    if (idx == TMD_STUN) return set_stun(v);
    else if (idx == TMD_CUT) return set_cut(v);

    /* Don't mention some effects if the player has permanent protection. */
    if (redundant_timed_event(idx))
    {
        override_notify = TRUE;
        notify = FALSE;
    }


    /* Find the effect */
    effect = &effects[idx];

    /* Turning off, always mention */
    if ((v == 0) && (!override_notify))
    {
        message(effect->on_end);
        notify = TRUE;
    }

    /* Turning on, always mention unless it is overridden by an immunity above */
    else if ((p_ptr->timed[idx] == 0)  && (!override_notify))
    {
        if (notify) message(effect->on_begin);
        notify = TRUE;
    }

    else if (notify)
    {
        /* Decrementing */
        if (p_ptr->timed[idx] > v && effect->on_decrease.length())
            message(effect->on_decrease);

        /* Incrementing */
        else if (v > p_ptr->timed[idx] && effect->on_decrease.length())
            message(effect->on_increase);
    }

    /* Use the value */
    p_ptr->timed[idx] = v;

    /* Don't allow the player to repeat the previous command */
    if (effect->disable_repeat) cmd_disable_repeat();

    /* Nothing to notice */
    if (!notify) return FALSE;

    /* Disturb */
    if (disturb_state)
    {
        disturb(TRUE, FALSE);
    }

    /* Update the visuals, as appropriate. */
    p_ptr->update |= effect->flag_update;
    p_ptr->redraw |= (effect->flag_redraw);
    p_ptr->redraw |= (PR_STATUSBAR);

    /* Handle stuff */
    handle_stuff();

    /* Result */
    return TRUE;
}

/**
 * Increase the timed effect `idx` by `v`.  Mention this if `notify` is TRUE.
 */
bool inc_timed(int idx, int v, bool notify)
{
    /* Check we have a valid effect */
    if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

    /* Set v */
    v = v + p_ptr->timed[idx];

    if (!p_ptr->timed[idx]) p_ptr->redraw |= PR_STATUSBAR;

    return set_timed(idx, v, notify);
}

/**
 * Decrease the timed effect `idx` by `v`.  Mention this if `notify` is TRUE.
 */
bool dec_timed(int idx, int v, bool notify)
{
    /* Check we have a valid effect */
    if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

    /* Set v */
    v = p_ptr->timed[idx] - v;

    if (p_ptr->timed[idx] && (v < 1)) p_ptr->redraw |= PR_STATUSBAR;

    return set_timed(idx, v, notify);
}

/**
 * Clear the timed effect `idx`.  Mention this if `notify` is TRUE.
 */
bool clear_timed(int idx, bool notify)
{
    if (p_ptr->timed[idx]) p_ptr->redraw |= PR_STATUSBAR;

    return set_timed(idx, 0, notify);
}

/* Check to see if the player is in a state to repeat commands */
bool player_can_repeat(void)
{
    int i;

    for (i = 0; i < TMD_MAX; i++)
    {
        timed_effect *effect = &effects[i];

        /* Player isn't in this time state */
        if (!p_ptr->timed[i]) continue;

        /* State prevent repeating commands, player can't repeat commands */
        if (effect->disable_repeat) return (TRUE);
    }

    return (TRUE);
}

/*
 * Set "p_ptr->timed[TMD_STUN]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_stun(int v)
{
    int old_aux, new_aux;

    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Knocked out */
    if (p_ptr->timed[TMD_STUN] >= STUN_KNOCKED_OUT)
    {
        old_aux = 3;
    }

    /* Heavy stun */
    else if (p_ptr->timed[TMD_STUN] > STUN_HEAVY)
    {
        old_aux = 2;
    }

    /* Stun */
    else if (p_ptr->timed[TMD_STUN] >= STUN_LIGHT)
    {
        old_aux = 1;
    }

    /* None */
    else
    {
        old_aux = 0;
    }

    /* Knocked out */
    if (v >= STUN_KNOCKED_OUT)
    {
        new_aux = 3;
    }

    /* Heavy stun */
    else if (v > STUN_HEAVY)
    {
        new_aux = 2;
    }

    /* Stun */
    else if (v >= STUN_LIGHT)
    {
        new_aux = 1;
    }

    /* None */
    else
    {
        new_aux = 0;
    }

    /* Increase cut */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Stun */
            case 1:
            {
                message(QString("You have been stunned."));
                break;
            }

            /* Heavy stun */
            case 2:
            {
                bell(QString("You have been heavily stunned."));
                break;
            }

            /* Knocked out */
            case 3:
            {
                bell(QString("You have been knocked out."));
                break;
            }
        }

        /* Notice */
        notice = TRUE;
    }

    /* Decrease cut */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                message(QString("You are no longer stunned."));
                if (disturb_state) disturb(TRUE, FALSE);
                break;
            }
        }

        /* Notice */
        notice = TRUE;
    }

    /* Use the value */
    p_ptr->timed[TMD_STUN] = v;

    /* No change */
    if (!notice) return (FALSE);

    /* Disturb */
    if (disturb_state) disturb(TRUE, FALSE);

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Redraw the "stun" */
    p_ptr->redraw |= (PR_SIDEBAR_PL | PR_STATUSBAR);

    /* Handle stuff */
    handle_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_CUT]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_cut(int v)
{
    int old_aux, new_aux;

    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Mortal wound */
    if (p_ptr->timed[TMD_CUT] > CUT_MORTAL_WOUND)
    {
        old_aux = 7;
    }

    /* Deep gash */
    else if (p_ptr->timed[TMD_CUT] > CUT_DEEP_GASH)
    {
        old_aux = 6;
    }

    /* Severe cut */
    else if (p_ptr->timed[TMD_CUT] > CUT_SEVERE)
    {
        old_aux = 5;
    }

    /* Nasty cut */
    else if (p_ptr->timed[TMD_CUT] > CUT_NASTY)
    {
        old_aux = 4;
    }

    /* Bad cut */
    else if (p_ptr->timed[TMD_CUT] > CUT_BAD)
    {
        old_aux = 3;
    }

    /* Light cut */
    else if (p_ptr->timed[TMD_CUT] > CUT_LIGHT)
    {
        old_aux = 2;
    }

    /* Graze */
    else if (p_ptr->timed[TMD_CUT] >= CUT_GRAZE)
    {
        old_aux = 1;
    }

    /* None */
    else
    {
        old_aux = 0;
    }

    /* Mortal wound */
    if (v > CUT_MORTAL_WOUND)
    {
        new_aux = 7;
    }

    /* Deep gash */
    else if (v > CUT_DEEP_GASH)
    {
        new_aux = 6;
    }

    /* Severe cut */
    else if (v > CUT_SEVERE)
    {
        new_aux = 5;
    }

    /* Nasty cut */
    else if (v > CUT_NASTY)
    {
        new_aux = 4;
    }

    /* Bad cut */
    else if (v > CUT_BAD)
    {
        new_aux = 3;
    }

    /* Light cut */
    else if (v > CUT_LIGHT)
    {
        new_aux = 2;
    }

    /* Graze */
    else if (v >= CUT_GRAZE)
    {
        new_aux = 1;
    }

    /* None */
    else
    {
        new_aux = 0;
    }

    /* Increase cut */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Graze */
            case 1:
            {
                message(QString("You have been given a graze."));
                break;
            }

            /* Light cut */
            case 2:
            {
                message(QString("You have been given a light cut."));
                break;
            }

            /* Bad cut */
            case 3:
            {
                message(QString("You have been given a bad cut."));
                break;
            }

            /* Nasty cut */
            case 4:
            {
                message(QString("You have been given a nasty cut."));
                break;
            }

            /* Severe cut */
            case 5:
            {
                message(QString("You have been given a severe cut."));
                break;
            }

            /* Deep gash */
            case 6:
            {
                message(QString("You have been given a deep gash."));
                break;
            }

            /* Mortal wound */
            case 7:
            {
                bell(QString("You have been given a mortal wound."));
                break;
            }
        }

        /* Notice */
        notice = TRUE;
    }

    /* Decrease cut */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* None */
            case 0:
            {
                message(QString("You are no longer bleeding."));
                if (disturb_state) disturb(TRUE, FALSE);
                break;
            }
        }

        /* Notice */
        notice = TRUE;
    }

    /* Use the value */
    p_ptr->timed[TMD_CUT] = v;

    /* No change */
    if (!notice) return (FALSE);

    /* Disturb */
    if (disturb_state) disturb(TRUE, FALSE);

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Redraw the "cut" */
    p_ptr->redraw |= (PR_SIDEBAR_PL | PR_STATUSBAR);

    /* Handle stuff */
    handle_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "p_ptr->food", notice observable changes
 *
 * The "p_ptr->food" variable can get as large as 20000, allowing the
 * addition of the most "filling" item, Elvish Waybread, which adds
 * 7500 food units, without overflowing the 32767 maximum limit.
 *
 * Perhaps we should disturb the player with various messages,
 * especially messages about hunger status changes.  XXX XXX XXX
 *
 * Digestion of food is handled in "dungeon.c", in which, normally,
 * the player digests about 20 food units per 100 game turns, more
 * when "fast", more when "regenerating", less with "slow digestion",
 * but when the player is "gorged", he digests 100 food units per 10
 * game turns, or a full 1000 food units per 100 game turns.
 *
 * Note that the player's speed is reduced by 10 units while gorged,
 * so if the player eats a single food ration (5000 food units) when
 * full (15000 food units), he will be gorged for (5000/100)*10 = 500
 * game turns, or 500/(100/5) = 25 player turns (if nothing else is
 * affecting the player speed).
 */
bool set_food(int v)
{
    int old_aux, new_aux;

    bool notice = FALSE;

    /* Hack -- Force good values */
    v = MIN(v, PY_FOOD_UPPER);
    v = MAX(v, 0);

    /* Fainting / Starving */
    if (p_ptr->food < PY_FOOD_FAINT)
    {
        old_aux = 0;
    }

    /* Weak */
    else if (p_ptr->food < PY_FOOD_WEAK)
    {
        old_aux = 1;
    }

    /* Hungry */
    else if (p_ptr->food < PY_FOOD_ALERT)
    {
        old_aux = 2;
    }

    /* Normal */
    else if (p_ptr->food < PY_FOOD_FULL)
    {
        old_aux = 3;
    }

    /* Full */
    else if (p_ptr->food < PY_FOOD_MAX)
    {
        old_aux = 4;
    }

    /* Gorged */
    else
    {
        old_aux = 5;
    }

    /* Fainting / Starving */
    if (v < PY_FOOD_FAINT)
    {
        new_aux = 0;
    }

    /* Weak */
    else if (v < PY_FOOD_WEAK)
    {
        new_aux = 1;
    }

    /* Hungry */
    else if (v < PY_FOOD_ALERT)
    {
        new_aux = 2;
    }

    /* Normal */
    else if (v < PY_FOOD_FULL)
    {
        new_aux = 3;
    }

    /* Full */
    else if (v < PY_FOOD_MAX)
    {
        new_aux = 4;
    }

    /* Gorged */
    else
    {
        new_aux = 5;
    }

    /* Food increase */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Weak */
            case 1:
            {
                bell(QString("You are still weak."));
                break;
            }

            /* Hungry */
            case 2:
            {
                message(QString("You are still hungry."));
                break;
            }

            /* Normal */
            case 3:
            {
                message(QString("You are no longer hungry."));
                break;
            }

            /* Full */
            case 4:
            {
                message(QString("You are full!"));
                break;
            }

            /* Bloated */
            case 5:
            {
                message(QString("You have gorged yourself!"));
                break;
            }
        }

        /* Change */
        notice = TRUE;
    }

    /* Food decrease */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Fainting / Starving */
            case 0:
            {
                bell(QString("You are getting faint from hunger!"));
                break;
            }

            /* Weak */
            case 1:
            {
                bell(QString("You are getting weak from hunger!"));
                break;
            }

            /* Hungry */
            case 2:
            {
                message(QString("You are getting hungry."));
                break;
            }

            /* Normal */
            case 3:
            {
                message(QString("You are no longer full."));
                break;
            }

            /* Full */
            case 4:
            {
                message(QString("You are no longer gorged."));
                break;
            }
        }

        /* Change */
        notice = TRUE;
    }

    /* Use the value */
    p_ptr->food = v;

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Disturb */
    if (disturb_state) disturb(TRUE, FALSE);

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Redraw hunger */
    p_ptr->redraw |= (PR_SIDEBAR_PL | PR_STATUSBAR);

    /* Handle stuff */
    handle_stuff();

    /* Result */
    return (TRUE);
}


