/*
 * File: timed.c
 * Purpose: Timed effects handling
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2007 A Sidwell <andi@takkaria.org>
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
#include "../s-spells.h"


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


typedef struct
{
  const char *on_begin, *on_end;
  const char *on_increase, *on_decrease;
  const char *near_begin, *near_end;
  u32b flag_redraw, flag_update;
  u16b msg;
  int resist;
} timed_effect;


static timed_effect effects[] =
{
    {"You feel yourself moving faster!", "You feel yourself slow down.",
        NULL, NULL,
        " starts moving faster.", " slows down.",
        0, PU_BONUS, MSG_SPEED, 0},
    {"You feel yourself moving slower!", "You feel yourself speed up.",
        NULL, NULL,
        " starts moving slower.", " speeds up.",
        0, PU_BONUS, MSG_SLOW, OF_FREE_ACT},
    {"You are blind!", "You blink and your eyes clear.",
        NULL, NULL,
        " gropes around blindly.", " can see again.",
        PR_MAP, (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS), MSG_BLIND, OF_RES_BLIND},
    {"You are paralyzed!", "You can move again.",
        NULL, NULL,
        " becomes rigid.", " can move again.",
        0, 0, MSG_PARALYZED, OF_FREE_ACT},
    {"You are confused!", "You are no longer confused.",
        "You are more confused!", "You feel a little less confused.",
        " appears confused.", " appears less confused.",
        0, 0, MSG_CONFUSED, OF_RES_CONFU},
    {"You are terrified!", "You feel bolder now.",
        "You are more scared!", "You feel a little less scared.",
        " cowers in fear.", " appears bolder now.",
        0, PU_BONUS, MSG_AFRAID, OF_RES_FEAR},
    {"You feel drugged!", "You can see clearly again.",
        "You feel more drugged!", "You feel less drugged.",
        " appears drugged.", " can see clearly again.",
        (PR_MAP | PR_MONLIST | PR_ITEMLIST), PU_MONSTERS, MSG_DRUGGED, OF_RES_CHAOS},
    {"You are poisoned!", "You are no longer poisoned.",
        "You are more poisoned!", "You are less poisoned.",
        " appears poisoned.", " appears less poisoned.",
        0, 0, MSG_POISONED, OF_RES_POIS},
    {NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0},   /* TMD_CUT -- handled separately */
    {NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, OF_RES_STUN}, /* TMD_STUN -- handled separately */
    {"You feel safe from evil!", "You no longer feel safe from evil.",
        "You feel even safer from evil!", "You feel less safe from evil.",
        " is surrounded by a holy aura.", "'s holy aura disappears.",
        0, 0, MSG_PROT_EVIL, 0},
    {"You feel invulnerable!", "You feel vulnerable once more.",
        NULL, NULL,
        " is surrounded by an aura of power.", "'s aura of power disappears.",
        PR_MAP, (PU_BONUS | PU_MONSTERS), MSG_INVULN, 0},
    {"You feel like a hero!", "You no longer feel heroic.",
        "You feel more like a hero!", "You feel less heroic.",
        " appears heroic.", " appears less heroic.",
        0, (PU_BONUS | PU_HP), MSG_HERO, 0},
    {"You feel like a killing machine!", "You no longer feel berserk.",
        "You feel even more berserk!", "You feel less berserk.",
        " enters a battle rage.", " calms down.",
        0, (PU_BONUS | PU_HP), MSG_BERSERK, 0},
    {"A mystic shield forms around your body!", "Your mystic shield crumbles away.",
        "The mystic shield strengthens.", "The mystic shield weakens.",
        " forms a mystic shield.", "'s mystic shield crumbles away.",
        0, PU_BONUS, MSG_SHIELD, 0},
    {"You feel righteous!", "The prayer has expired.",
        "You feel more righteous!", "You feel less righteous.",
        " is surrounded by a bright aura.", "'s bright aura disappears.",
        0, PU_BONUS, MSG_BLESSED, 0},
    {"Your eyes feel very sensitive!", "Your eyes no longer feel so sensitive.",
        "Your eyes feel more sensitive!", "Your eyes feel less sensitive.",
        "'s eyes glow brightly.", "'s eyes are back to normal.",
        0, (PU_BONUS | PU_MONSTERS), MSG_SEE_INVIS, 0},
    {"Your eyes begin to tingle!", "Your eyes stop tingling.",
        "Your eyes' tingling intensifies.", "Your eyes tingle less.",
        "'s eyes begin to tingle.", "'s eyes stop tingling.",
        0, (PU_BONUS | PU_MONSTERS), MSG_INFRARED, 0},
    {"You feel resistant to acid!", "You are no longer resistant to acid.",
        "You feel more resistant to acid!", "You feel less resistant to acid.",
        " is surrounded by an acidic aura.", "'s acidic aura disappears.",
        0, 0, MSG_RES_ACID, OF_VULN_ACID},
    {"You feel resistant to lightning!", "You are no longer resistant to lightning.",
        "You feel more resistant to lightning!", "You feel less resistant to lightning.",
        " is surrounded by an electric aura.", "'s electric aura disappears.",
        0, 0, MSG_RES_ELEC, OF_VULN_ELEC},
    {"You feel resistant to fire!", "You are no longer resistant to fire.",
        "You feel more resistant to fire!", "You feel less resistant to fire.",
        " is surrounded by a fiery aura.", "'s fiery aura disappears.",
        0, 0, MSG_RES_FIRE, OF_VULN_FIRE},
    {"You feel resistant to cold!", "You are no longer resistant to cold.",
        "You feel more resistant to cold!", "You feel less resistant to cold.",
        " is surrounded by a chilling aura.", "'s chilling aura disappears.",
        0, 0, MSG_RES_COLD, OF_VULN_COLD},
    {"You feel resistant to poison!", "You are no longer resistant to poison.",
        "You feel more resistant to poison!", "You feel less resistant to poison.",
        " is surrounded by a toxic aura.", "'s toxic aura disappears.",
        0, 0, MSG_RES_POIS, 0},
    {"You feel resistant to confusion!", "You are no longer resistant to confusion.",
        "You feel more resistant to confusion!", "You feel less resistant to confusion.",
        " is surrounded by a clear aura.", "'s clear aura disappears.",
        0, PU_BONUS, MSG_GENERIC, 0},
    {"You feel your memories fade!", "Your memories come flooding back.",
        NULL, NULL,
        " appears disoriented.", " appears less disoriented.",
        0, 0, MSG_GENERIC, 0},
    {"Your mind expands!", "Your mind retracts.",
        "Your mind expands further.", NULL,
        " appears receptive.", " appears less receptive.",
        0, (PU_BONUS | PU_MONSTERS), MSG_GENERIC, 0},
    {"Your skin turns to stone!", "A fleshy shade returns to your skin.",
        NULL, NULL,
        "'s skin turns to stone.", "'s skin turns back to flesh.",
        0, PU_BONUS, MSG_GENERIC, 0},
    {"You feel the need to run away, and fast!", "The urge to run dissipates.",
        NULL, NULL,
        " feels the need to run away, and fast!", "'s urge to run dissipates.",
        0, PU_BONUS, MSG_AFRAID, 0},
    {"You start sprinting!", "You suddenly stop sprinting.",
        NULL, NULL,
        " starts sprinting.", " suddenly stops sprinting.",
        0, PU_BONUS, MSG_SPEED, 0},
    {"You feel bold!", "You no longer feel bold.",
        "You feel even bolder!", "You feel less bold.",
        " feels bold.", " no longer feels bold.",
        0, PU_BONUS, MSG_BOLD, 0},
    {"You turn into a wraith!", "You lose your wraith powers.",
        NULL, NULL,
        " turns into a wraith.", " appears more solid.",
        0, 0, MSG_GENERIC, 0},
    {"You start a calm meditation!", "You stop your meditation.",
        NULL, NULL,
        " starts a calm meditation.", " stops meditating.",
        0, (PU_HP | PU_MANA), MSG_GENERIC, 0},
    {"You feel immortal!", "You feel less immortal.",
        NULL, NULL,
        " forms a mana shield.", "'s mana shield crumbles away.",
        PR_MAP, PU_MONSTERS, MSG_GENERIC, 0},
    {"You fade in the shadows!", "The shadows enveloping you dissipate.",
        NULL, NULL,
        " fades in the shadows.", " is visible once again.",
        0, PU_MONSTERS, MSG_GENERIC, 0},
    {"Your image changes!", "Your image is back to normality.",
        NULL, NULL,
        "'s image changes.", "'s image is back to normality.",
        0, PU_MONSTERS, MSG_GENERIC, 0},
    {"You can avoid all the traps!", "You should worry about traps again.",
        NULL, NULL,
        " appears stealthy.", " appears less stealthy.",
        0, 0, MSG_GENERIC, 0},
    {NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0},  /* TMD_BOWBRAND -- handled separately */
    {"The space/time continuum seems to solidify!", "The space/time continuum seems more flexible.",
        NULL, NULL,
        " tightens the space/time continuum.", " loosens the space/time continuum.",
        0, 0, MSG_GENERIC, 0},
    {"You feel instable!", "You feel more stable.",
        NULL, NULL,
        " appears instable.", " appears more stable.",
        0, 0, MSG_GENERIC, 0},
    {NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0},  /* TMD_ADRENALINE -- handled separately */
    {NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0},  /* TMD_BIOFEEDBACK -- handled separately */
    {"Your touch becomes vampiric!", "Your touch is back to normal.",
        NULL, NULL,
        "'s touch becomes vampiric.", "'s touch is back to normal.",
        0, 0, MSG_GENERIC, 0},
    {"You start absorbing the souls of your foes!", "You stop absorbing the souls of your foes.",
        NULL, NULL,
        " is surrounded by a dark aura.", "'s dark aura disappears.",
        0, 0, MSG_GENERIC, 0},
    {"Your hands begin to glow black!", "Your hands stop glowing black.",
        NULL, NULL,
        "'s hands begin to glow black.", "'s hands stop glowing black.",
        PR_MAP, PU_MONSTERS, MSG_GENERIC, 0},
    {"You feel attuned to the elements!", "You feel less attuned to the elements.",
        NULL, NULL,
        " appears more powerful.", " appears less powerful.",
        0, 0, MSG_GENERIC, 0},
    {"Your skin turns icy!", "Your skin is no longer icy.",
        NULL, NULL,
        " is surrounded by an icy aura.", "'s icy aura disappears.",
        0, 0, MSG_GENERIC, 0},
    {"Your hands are covered with lightning!", "Your hands are normal once again.",
        NULL, NULL,
        "'s hands are covered with lightning.", "'s hands are normal once again.",
        0, PU_BONUS, MSG_GENERIC, 0},
    {"Your sight expands!", "Your sight is back to normal.",
        "Your sight expands further!", NULL,
        "'s sight expands.", "'s sight is back to normal.",
        0, PU_BONUS, MSG_GENERIC, 0},
    {"Your sight expands!", "Your sight is back to normal.",
        "Your sight expands further!", NULL,
        "'s sight expands.", "'s sight is back to normal.",
        0, PU_BONUS, MSG_GENERIC, 0},
    {"You start to regenerate quickly!", "Your regeneration rate is back to normal.",
        NULL, NULL,
        " starts to regenerate quickly.", "'s regeneration rate is back to normal.",
        0, 0, MSG_GENERIC, 0},
    {NULL, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, 0},  /* TMD_HARMONY -- handled separately */
    {"You can prevent all summoning!", "You should worry about summons again.",
        NULL, NULL,
        " is surrounded by a black aura.", "'s black aura disappears.",
        0, 0, MSG_GENERIC, 0}
};


/*
 * Set a timed event.
 */
bool player_set_timed(struct player *p, int idx, int v, bool notify)
{
    timed_effect *effect;
    bool result;

    /* Hack -- Force good values */
    v = ((v > 10000)? 10000: ((v < 0)? 0: v));
    if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

    /* No change */
    if (p->timed[idx] == v) return FALSE;

    /* Hack -- Call other functions, reveal unaware players if noticed */
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
    if ((idx == TMD_OPP_ACID) && check_state(p, OF_IM_ACID)) notify = FALSE;
    if ((idx == TMD_OPP_ELEC) && check_state(p, OF_IM_ELEC)) notify = FALSE;
    if ((idx == TMD_OPP_FIRE) && check_state(p, OF_IM_FIRE)) notify = FALSE;
    if ((idx == TMD_OPP_COLD) && check_state(p, OF_IM_COLD)) notify = FALSE;
    if ((idx == TMD_OPP_CONF) && of_has(p->state.flags, OF_RES_CONFU)) notify = FALSE;

    /* Find the effect */
    effect = &effects[idx];

    /* Turning off, always mention */
    if (v == 0)
    {
        msg_misc(p, effect->near_end);
        msgt(p, MSG_RECOVER, effect->on_end);
        notify = TRUE;
    }

    /* Turning on, always mention */
    else if (p->timed[idx] == 0)
    {
        msg_misc(p, effect->near_begin);
        msgt(p, effect->msg, effect->on_begin);
        notify = TRUE;
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
    if ((idx == TMD_SPRINT) && (v == 0)) player_inc_timed(p, TMD_SLOW, 100, TRUE, FALSE);

    /* Nothing to notice */
    if (!notify) return FALSE;

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Reveal unaware players */
    if (p->k_idx) aware_player(p, p);

    /* Update the visuals, as appropriate. */
    p->update |= effect->flag_update;
    p->redraw |= (PR_STATUS | effect->flag_redraw);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return TRUE;
}


/*
 * Increase the timed effect `idx` by `v`.  Mention this if `notify` is TRUE.
 * Check for resistance to the effect if `check` is TRUE.
 */
bool player_inc_timed(struct player *p, int idx, int v, bool notify, bool check)
{
    timed_effect *effect;

    /* Check we have a valid effect */
    if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

    /* Find the effect */
    effect = &effects[idx];

    /* Check that @ can be affected by this effect */
    if (check)
    {
        wieldeds_notice_flag(p, effect->resist);
        if (check_state(p, effect->resist)) return FALSE;
    }

    /* Paralysis should be non-cumulative */
    if ((idx == TMD_PARALYZED) && (p->timed[idx] > 0)) return FALSE;

    /* Hack -- Permanent effect */
    if (p->timed[idx] == -1) return FALSE;

    /* Handle polymorphed players */
    if (p->r_idx && (idx == TMD_AMNESIA))
    {
        monster_race *r_ptr = &r_info[p->r_idx];

        if (rf_has(r_ptr->flags, RF_EMPTY_MIND)) v /= 2;
        if (rf_has(r_ptr->flags, RF_WEIRD_MIND)) v = v * 3 / 4;
    }

    /* Set v */
    v = v + p->timed[idx];

    return player_set_timed(p, idx, v, notify);
}


/*
 * Decrease the timed effect `idx` by `v`.  Mention this if `notify` is TRUE.
 */
bool player_dec_timed(struct player *p, int idx, int v, bool notify)
{
    /* Check we have a valid effect */
    if ((idx < 0) || (idx > TMD_MAX)) return FALSE;

    /* Set v */
    v = p->timed[idx] - v;

    return player_set_timed(p, idx, v, notify);
}


/*
 * Clear the timed effect `idx`.  Mention this if `notify` is TRUE.
 */
bool player_clear_timed(struct player *p, int idx, bool notify)
{
    return player_set_timed(p, idx, 0, notify);
}


bool player_inc_timed_nostack(struct player *p, int idx, int v, int iv, bool notify)
{
    if (!p->timed[idx])
        return player_set_timed(p, idx, v, notify);
    return player_inc_timed(p, idx, iv, notify, TRUE);
}


/*
 * Set "p_ptr->timed[TMD_STUN]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_stun(struct player *p, int v)
{
    int old_aux, new_aux; 
    bool notice = FALSE;

    /* Hack -- The DM can not be stunned */
    if (p->dm_flags & DM_INVULNERABLE) return TRUE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Knocked out */
    if (p->timed[TMD_STUN] > 100)
        old_aux = 3;

    /* Heavy stun */
    else if (p->timed[TMD_STUN] > 50)
        old_aux = 2;

    /* Stun */
    else if (p->timed[TMD_STUN] > 0)
        old_aux = 1;

    /* None */
    else
        old_aux = 0;

    /* Knocked out */
    if (v > 100)
        new_aux = 3;

    /* Heavy stun */
    else if (v > 50)
        new_aux = 2;

    /* Stun */
    else if (v > 0)
        new_aux = 1;

    /* None */
    else
        new_aux = 0;

    /* Increase cut */
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
                msg_misc(p, " is no longer stunned.");
                msgt(p, MSG_RECOVER, "You are no longer stunned.");
                if (OPT_P(p, disturb_state)) disturb(p, 0, 0);
                break;
            }
        }

        /* Notice */
        notice = TRUE;
    }

    /* Use the value */
    p->timed[TMD_STUN] = v;

    /* No change */
    if (!notice) return (FALSE);

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Recalculate bonuses */
    p->update |= (PU_BONUS);

    /* Redraw the "stun" */
    p->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_CUT]", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_cut(struct player *p, int v)
{
    int old_aux, new_aux;
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Ghosts cannot bleed */
    if (p->ghost && (v > 0)) return TRUE;

    /* Mortal wound */
    if (p->timed[TMD_CUT] > 1000)
        old_aux = 7;

    /* Deep gash */
    else if (p->timed[TMD_CUT] > 200)
        old_aux = 6;

    /* Severe cut */
    else if (p->timed[TMD_CUT] > 100)
        old_aux = 5;

    /* Nasty cut */
    else if (p->timed[TMD_CUT] > 50)
        old_aux = 4;

    /* Bad cut */
    else if (p->timed[TMD_CUT] > 25)
        old_aux = 3;

    /* Light cut */
    else if (p->timed[TMD_CUT] > 10)
        old_aux = 2;

    /* Graze */
    else if (p->timed[TMD_CUT] > 0)
        old_aux = 1;

    /* None */
    else
        old_aux = 0;

    /* Mortal wound */
    if (v > 1000)
        new_aux = 7;

    /* Deep gash */
    else if (v > 200)
        new_aux = 6;

    /* Severe cut */
    else if (v > 100)
        new_aux = 5;

    /* Nasty cut */
    else if (v > 50)
        new_aux = 4;

    /* Bad cut */
    else if (v > 25)
        new_aux = 3;

    /* Light cut */
    else if (v > 10)
        new_aux = 2;

    /* Graze */
    else if (v > 0)
        new_aux = 1;

    /* None */
    else
        new_aux = 0;

    /* Increase cut */
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
                msg_misc(p, " is no longer bleeding.");
                msgt(p, MSG_RECOVER, "You are no longer bleeding.");
                if (OPT_P(p, disturb_state)) disturb(p, 0, 0);
                break;
            }
        }

        /* Notice */
        notice = TRUE;
    }

    /* Use the value */
    p->timed[TMD_CUT] = v;

    /* No change */
    if (!notice) return (FALSE);

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Redraw the "cut" */
    p->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_BOWBRAND]", notice observable changes
 */
static bool set_bow_brand(struct player *p, int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v)
    {
        if (!p->timed[TMD_BOWBRAND])
        {
            switch (p->bow_brand_t)
            {
                case BOW_BRAND_ELEC:
                    msg_misc(p, "'s missiles are covered with lightning.");
                    msg(p, "Your missiles are covered with lightning!");
                    break;
                case BOW_BRAND_COLD:
                    msg_misc(p, "'s missiles are covered with frost.");
                    msg(p, "Your missiles are covered with frost!");
                    break;
                case BOW_BRAND_FIRE:
                    msg_misc(p, "'s missiles are covered with fire.");
                    msg(p, "Your missiles are covered with fire!");
                    break;
                case BOW_BRAND_ACID:
                    msg_misc(p, "'s missiles are covered with acid.");
                    msg(p, "Your missiles are covered with acid!");
                    break;
                case BOW_BRAND_THUNDER:
                    msg_misc(p, "'s missiles glow deep blue.");
                    msg(p, "Your missiles glow deep blue!");
                    break;
                case BOW_BRAND_CONF:
                    msg_misc(p, "'s missiles glow many colors.");
                    msg(p, "Your missiles glow many colors!");
                    break;
                case BOW_BRAND_POISON:
                    msg_misc(p, "'s missiles are covered with venom.");
                    msg(p, "Your missiles are covered with venom!");
                    break;
                case BOW_BRAND_PIERCE:
                    msg_misc(p, "'s missiles sharpen.");
                    msg(p, "Your missiles sharpen!");
                    break;
                case BOW_BRAND_SHARDS:
                    msg_misc(p, "'s missiles become explosive.");
                    msg(p, "Your missiles become explosive!");
                    break;
                case BOW_BRAND_ICE:
                    msg_misc(p, "'s missiles glow bright white.");
                    msg(p, "Your missiles glow bright white!");
                    break;
                case BOW_BRAND_FLAME:
                    msg_misc(p, "'s missiles glow deep red.");
                    msg(p, "Your missiles glow deep red!");
                    break;
                case BOW_BRAND_WATER:
                    msg_misc(p, "'s missiles glow pitch black.");
                    msg(p, "Your missiles glow pitch black!");
                    break;
                case BOW_BRAND_POWER:
                    msg_misc(p, "'s missiles glow with power.");
                    msg(p, "Your missiles glow with power!");
                    break;
                case BOW_BRAND_SONIC:
                    msg_misc(p, "'s missiles vibrate in a strange way.");
                    msg(p, "Your missiles vibrate in a strange way!");
                    break;
            }
            notice = TRUE;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_BOWBRAND])
        {
            msg_misc(p, "'s missiles seem normal again.");
            msg(p, "Your missiles seem normal again.");
            notice = TRUE;
        }
    }

    /* Use the value */
    p->timed[TMD_BOWBRAND] = v;

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Redraw the "brand" */
    p->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_ADRENALINE]", notice observable changes
 * Note the interaction with biofeedback
 */
static bool set_adrenaline(struct player *p, int v)
{
    bool notice = FALSE;
    int vmax;
    s16b old_fx, new_fx;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v)
    {
        if (!p->timed[TMD_ADRENALINE])
        {
            msg_misc(p, "'s veins are flooded with adrenaline.");
            msg(p, "Adrenaline surges through your veins!");

            /* Adrenaline doesn't work well when biofeedback is activated */
            if (p->timed[TMD_BIOFEEDBACK])
            {
                player_clear_timed(p, TMD_BIOFEEDBACK, TRUE);
                my_strcpy(p->died_flavor, "had a heart attack due to too much adrenaline",
                    sizeof(p->died_flavor));
                take_hit(p, damroll(2, v), "adrenaline poisoning", FALSE);
            }
            notice = TRUE;
        }

        /* Too much adrenaline causes damage */
        vmax = 20 + randint1(160);
        if (vmax > 100) vmax = 100;
        if (vmax < p->timed[TMD_ADRENALINE])
            vmax = p->timed[TMD_ADRENALINE];
        if (v > vmax)
        {
            msg(p, "Your body can't handle that much adrenaline!");
            my_strcpy(p->died_flavor, "had a heart attack due to too much adrenaline",
                sizeof(p->died_flavor));
            take_hit(p, damroll(3, vmax * 2), "adrenaline poisoning", FALSE);
            v = vmax;
            notice = TRUE;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_ADRENALINE])
        {
            msg_misc(p, "'s veins are drained of adrenaline.");
            msg(p, "The adrenaline drains out of your veins.");
            notice = TRUE;
        }
    }

    /* Different stages: notice */
    old_fx = (p->timed[TMD_ADRENALINE] - 1) / 20;
    new_fx = (v - 1) / 20;
    if (new_fx != old_fx) notice = TRUE;

    /* Use the value */
    p->timed[TMD_ADRENALINE] = v;

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Notice */
    p->update |= (PU_BONUS | PU_HP);

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Redraw the "adrenaline" */
    p->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_BIOFEEDBACK]", notice observable changes
 * Note the interaction with adrenaline
 */
static bool set_biofeedback(struct player *p, int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
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
                player_clear_timed(p, TMD_ADRENALINE, TRUE);
                if (one_in_(8))
                {
                    msg(p, "You feel weak and tired!");
                    player_inc_timed(p, TMD_SLOW, randint0(4) + 4, TRUE, FALSE);
                    if (one_in_(5))
                        player_inc_timed(p, TMD_PARALYZED, randint0(4) + 4, TRUE, FALSE);
                    if (one_in_(3)) player_inc_timed(p, TMD_STUN, randint1(30), TRUE, FALSE);
                }
            }
            notice = TRUE;
        }

        /* Biofeedback can't reach high values */
        if (v > 35 + p->lev)
        {
            msg(p, "You speed up your pulse to avoid fainting!");
            v = 35 + p->lev;
            notice = TRUE;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_BIOFEEDBACK])
        {
            msg_misc(p, "'s pulse speeds up.");
            msg(p, "You lose control of your blood flow.");
            notice = TRUE;
        }

    }

    /* Use the value */
    p->timed[TMD_BIOFEEDBACK] = v;

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Notice */
    p->update |= (PU_BONUS | PU_HP);

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Redraw the "biofeedback" */
    p->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return (TRUE);
}


/*
 * Set "p_ptr->timed[TMD_HARMONY]", notice observable changes
 */
static bool set_harmony(struct player *p, int v)
{
    bool notice = FALSE;
    s16b old_fx, new_fx;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v)
    {
        if (!p->timed[TMD_HARMONY])
        {
            msg_misc(p, "feels attuned to nature.");
            msg(p, "You feel attuned to nature!");
            notice = TRUE;
        }
    }

    /* Shut */
    else
    {
        if (p->timed[TMD_HARMONY])
        {
            msg_misc(p, "feels less attuned to nature.");
            msg(p, "You feel less attuned to nature.");
            notice = TRUE;
        }
    }

    /* Different stages: notice */
    old_fx = (p->timed[TMD_HARMONY] - 1) / 20;
    new_fx = (v - 1) / 20;
    if (new_fx != old_fx) notice = TRUE;

    /* Use the value */
    p->timed[TMD_HARMONY] = v;

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Notice */
    p->update |= (PU_BONUS | PU_HP);

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Redraw the status */
    p->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

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
bool player_set_food(struct player *p, int v)
{
    int old_aux, new_aux;
    bool notice = FALSE;

    p->starving = FALSE;

    /* Hack -- Force good values */
    v = MIN(v, PY_FOOD_UPPER);
    v = MAX(v, 0);

    /* Fainting / Starving */
    if (p->food < PY_FOOD_FAINT)
        old_aux = 0;

    /* Weak */
    else if (p->food < PY_FOOD_WEAK)
        old_aux = 1;

    /* Hungry */
    else if (p->food < PY_FOOD_ALERT)
        old_aux = 2;

    /* Normal */
    else if (p->food < PY_FOOD_FULL)
        old_aux = 3;

    /* Full */
    else if (p->food < PY_FOOD_MAX)
        old_aux = 4;

    /* Gorged */
    else
        old_aux = 5;

    /* Fainting / Starving */
    if (v < PY_FOOD_FAINT)
        new_aux = 0;

    /* Weak */
    else if (v < PY_FOOD_WEAK)
        new_aux = 1;

    /* Hungry */
    else if (v < PY_FOOD_ALERT)
        new_aux = 2;

    /* Normal */
    else if (v < PY_FOOD_FULL)
        new_aux = 3;

    /* Full */
    else if (v < PY_FOOD_MAX)
        new_aux = 4;

    /* Gorged */
    else
        new_aux = 5;

    /* Change */
    if (new_aux != old_aux) notice = TRUE;

    /* Hack -- Do not display message for ghosts */
    if (p->ghost) old_aux = new_aux;

    /* Food increase */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
            /* Weak */
            case 1:
                msg(p, "You are still weak.");
                break;

            /* Hungry */
            case 2:
                msg(p, "You are still hungry.");
                break;

            /* Normal */
            case 3:
                msg(p, "You are no longer hungry.");
                break;

            /* Full */
            case 4:
                msg(p, "You are full!");
                break;

            /* Bloated */
            case 5:
                msg(p, "You have gorged yourself!");
                break;
        }
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
                msgt(p, MSG_NOTICE, "You are getting faint from hunger!");

                /*
                 * If the player is at full hit points,
                 * destroy his connection (this will hopefully prevent
                 * people from starving while afk)
                 */
                if (p->chp == p->mhp) p->starving = TRUE;
                break;
            }

            /* Weak */
            case 1:
                msgt(p, MSG_NOTICE, "You are getting weak from hunger!");
                break;

            /* Hungry */
            case 2:
                msgt(p, MSG_HUNGRY, "You are getting hungry.");
                break;

            /* Normal */
            case 3:
                msgt(p, MSG_NOTICE, "You are no longer full.");
                break;

            /* Full */
            case 4:
                msgt(p, MSG_NOTICE, "You are no longer gorged.");
                break;
        }
    }

    /* Use the value */
    p->food = v;

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Disturb */
    if (OPT_P(p, disturb_state)) disturb(p, 0, 0);

    /* Recalculate bonuses */
    p->update |= (PU_BONUS);

    /* Redraw hunger */
    p->redraw |= (PR_STATUS);

    /* Handle stuff */
    handle_stuff(p);

    /* Result */
    return (TRUE);
}
