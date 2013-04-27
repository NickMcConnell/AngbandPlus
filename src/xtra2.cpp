// File: xtra2.cpp

// Purpose: effects of various "objects"

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Set blindness, notice observable changes
 *
 * Note the use of "PU_UN_LITE" and "PU_UN_VIEW", which is needed to
 * memorize any terrain features which suddenly become "visible".
 * Note that blindness is currently the only thing which can affect
 * can_see_bold().
 */
bool CPlayer::mod_blind(int v)
{
    bool notice = FALSE;

    // Hack -- Force good values
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    // Open
    if (v) {
        if (!GetBlind()) {
            msg_print("You are blind!");
            notice = TRUE;
        }
    }

    // Shut
    else {
        if (GetBlind()) {
            msg_print("You can see again.");
            notice = TRUE;
        }
    }

    // Use the value
    SetBlind(v);

    // Nothing to notice
    if (!notice) return FALSE;

    // Forget stuff
    set_update(get_update() | PU_UN_VIEW | PU_UN_LITE);

    // Update stuff
    set_update(get_update() | PU_VIEW | PU_LITE | PU_MONSTERS);

    // Update stuff
    update_stuff();

    // Result
    return TRUE;
}


/*
 * Set confused, notice observable changes
 */
bool CPlayer::mod_confused(int v)
{
    bool notice = FALSE;

    // Hack -- Force good values
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    // Open
    if (v) {
        if (!GetConfused()) {
            msg_print("You are confused!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetConfused()) {
            msg_print("You feel less confused now.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetConfused(v);

    /* Nothing to notice */
    if (!notice) return FALSE;

    /* Update stuff */
    update_stuff();

    /* Result */
    return TRUE;
}


/*
 * Set "poisoned", notice observable changes
 */
bool CPlayer::mod_poisoned(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetPoisoned()) {
            msg_print("You are poisoned!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetPoisoned()) {
            msg_print("You are no longer poisoned.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetPoisoned(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return TRUE;
}


/*
 * Set "afraid", notice observable changes
 */
bool CPlayer::mod_afraid(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetAfraid()) {
            msg_print("You are terrified!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetAfraid()) {
            msg_print("You feel bolder now.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetAfraid(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return TRUE;
}


/*
 * Set "paralyzed", notice observable changes
 */
bool CPlayer::mod_paralyzed(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetParalyzed()) {
            msg_print("You are paralyzed!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetParalyzed()) {
            msg_print("You can move again.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetParalyzed(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return TRUE;
}


/*
 * Set "fast", notice observable changes
 */
bool CPlayer::mod_fast(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetFast()) {
            msg_print("You feel yourself moving faster!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetFast()) {
            msg_print("You feel yourself slow down.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetFast(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return TRUE;
}


/*
 * Set "slow", notice observable changes
 */
bool CPlayer::mod_slow(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetSlow()) {
            msg_print("You feel yourself moving slower!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetSlow()) {
            msg_print("You feel yourself speed up.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetSlow(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "shield", notice observable changes
 */
bool CPlayer::mod_shield(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetShield()) {
            msg_print("A mystic shield forms around your body!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetShield()) {
            msg_print("Your mystic shield crumbles away.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetShield(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}



/*
 * Set "blessed", notice observable changes
 */
bool CPlayer::mod_blessed(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetBlessed()) {
            msg_print("You feel righteous!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetBlessed()) {
            msg_print("The prayer has expired.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetBlessed(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "hero", notice observable changes
 */
bool CPlayer::mod_hero(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetHero()) {
            msg_print("You feel like a hero!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetHero()) {
            msg_print("The heroism wears off.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetHero(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Recalculate bonuses and hit points*/
    set_update(get_update() | PU_BONUS | PU_HP);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "shero", notice observable changes
 */
bool CPlayer::mod_shero(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetSHero()) {
            msg_print("You feel like a killing machine!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetSHero()) {
            msg_print("You feel less Berserk.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetSHero(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Recalculate hitpoints */
    set_update(get_update() | PU_HP);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "protevil", notice observable changes
 */
bool CPlayer::mod_protevil(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v)
    {
        if (!GetProtevil()) {
            msg_print("You feel safe from evil!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetProtevil()) {
            msg_print("You no longer feel safe from evil.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetProtevil(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    // Update stuff
    update_stuff();

    // Result
    return (TRUE);
}


/*
 * Set "shadowform", notice observable changes
 */
bool CPlayer::mod_shadowform(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetShadowform()) {
            msg_print("You dissolve into the aether.");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetShadowform()) {
            msg_print("You feel corporeal once more.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetShadowform(v);

    /* Nothing to notice */
    if (!notice) return FALSE;

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "tim_invis", notice observable changes
 */
bool CPlayer::mod_tim_invis(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetTimInvis()) {
            msg_print("Your eyes feel very sensitive!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetTimInvis()) {
            msg_print("Your eyes feel less sensitive.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetTimInvis(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    // Recalculate bonuses and update monsters
    set_update(get_update() | PU_BONUS | PU_MONSTERS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return TRUE;
}


/*
 * Set "tim_infra", notice observable changes
 */
bool CPlayer::mod_tim_infra(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetTimInfra()) {
            msg_print("Your eyes begin to tingle!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetTimInfra()) {
            msg_print("Your eyes stop tingling.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetTimInfra(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update the monsters */
    set_update(get_update() | PU_MONSTERS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "oppose_acid", notice observable changes
 */
bool CPlayer::mod_oppose_acid(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetOpposeAcid()) {
            msg_print("You feel resistant to acid!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetOpposeAcid()) {
            msg_print("You feel less resistant to acid.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetOpposeAcid(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "oppose_elec", notice observable changes
 */
bool CPlayer::mod_oppose_elec(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetOpposeElec()) {
            msg_print("You feel resistant to electricity!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetOpposeElec()) {
            msg_print("You feel less resistant to electricity.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetOpposeElec(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "oppose_fire", notice observable changes
 */
bool CPlayer::mod_oppose_fire(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v)
    {
        if (!GetOpposeFire()) {
            msg_print("You feel resistant to fire!");
            notice = TRUE;
        }
    }

    /* Shut */
    else
    {
        if (GetOpposeFire())
        {
            msg_print("You feel less resistant to fire.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetOpposeFire(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "oppose_cold", notice observable changes
 */
bool CPlayer::mod_oppose_cold(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetOpposeCold()) {
            msg_print("You feel resistant to cold!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetOpposeCold()) {
            msg_print("You feel less resistant to cold.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetOpposeCold(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "oppose_pois", notice observable changes
 */
bool CPlayer::mod_oppose_pois(int v)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Open */
    if (v) {
        if (!GetOpposePois()) {
            msg_print("You feel resistant to poison!");
            notice = TRUE;
        }
    }

    /* Shut */
    else {
        if (GetOpposePois()) {
            msg_print("You feel less resistant to poison.");
            notice = TRUE;
        }
    }

    /* Use the value */
    SetOpposePois(v);

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "stun", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool CPlayer::mod_stun(int v)
{
    int old_aux, new_aux;
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    /* Knocked out */
    if (GetStun() > 100) {
        old_aux = 3;
    }

    /* Heavy stun */
    else if (GetStun() > 50) {
        old_aux = 2;
    }

    /* Stun */
    else if (GetStun() > 0) {
        old_aux = 1;
    }

    /* None */
    else {
        old_aux = 0;
    }

    /* Knocked out */
    if (v > 100) {
        new_aux = 3;
    }

    /* Heavy stun */
    else if (v > 50) {
        new_aux = 2;
    }

    /* Stun */
    else if (v > 0) {
        new_aux = 1;
    }

    /* None */
    else {
        new_aux = 0;
    }

    /* Increase cut */
    if (new_aux > old_aux) {
        /* Describe the state */
        switch (new_aux) {
            // Stun
            case 1: msg_print("You have been stunned."); break;

            /* Heavy stun */
            case 2: msg_print("You have been heavily stunned."); break;

            /* Knocked out */
            case 3: msg_print("You have been knocked out."); break;
        }

        /* Notice */
        notice = TRUE;
    }

    /* Decrease cut */
    else if (new_aux < old_aux) {
        /* Describe the state */
        switch (new_aux) {
            /* None */
            case 0:
                msg_print("You are no longer stunned.");
                break;
        }

        /* Notice */
        notice = TRUE;
    }

    /* Use the value */
    SetStun(v);

    /* No change */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "cut", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool CPlayer::mod_cut(int v)
{
    int old_aux, new_aux;
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    // Mortal wound
    if (GetCut() > 1000) {
        old_aux = 6;
    }

    // Severe cut
    else if (GetCut() > 200) {
        old_aux = 5;
    }

    // Nasty cut
    else if (GetCut() > 100) {
        old_aux = 4;
    }

    // Bad cut
    else if (GetCut() > 50) {
        old_aux = 3;
    }

    // Light cut
    else if (GetCut() > 10) {
        old_aux = 2;
    }

    // Graze
    else if (GetCut() > 0) {
        old_aux = 1;
    }

    // None
    else {
        old_aux = 0;
    }

    // Mortal wound
    if (v > 1000) {
        new_aux = 7;
    }

    // Severe cut
    else if (v > 200) {
        new_aux = 5;
    }

    // Nasty cut
    else if (v > 100) {
        new_aux = 4;
    }

    // Bad cut
    else if (v > 50) {
        new_aux = 3;
    }

    /* Light cut */
    else if (v > 10) {
        new_aux = 2;
    }

    /* Graze */
    else if (v > 0) {
        new_aux = 1;
    }

    // None
    else {
        new_aux = 0;
    }

    // Increase cut
    if (new_aux > old_aux) {
        /* Describe the state */
        switch (new_aux) {
            /* Graze */
            case 1:
                msg_print("You have been given a graze.");
                break;

            /* Light cut */
            case 2:
                msg_print("You have been given a light cut.");
                break;

            /* Bad cut */
            case 3:
                msg_print("You have been given a bad cut.");
                break;

            /* Nasty cut */
            case 4:
                msg_print("You have been given a nasty cut.");
                break;

            /* Severe cut */
            case 5:
                msg_print("You have been given a severe cut.");
                break;

            /* Mortal wound */
            case 6:
                msg_print("You have been given a mortal wound.");
                break;
        }

        /* Notice */
        notice = TRUE;
    }

    /* Decrease cut */
    else if (new_aux < old_aux) {
        /* Describe the state */
        switch (new_aux) {
            /* None */
            case 0:
                msg_print("You are no longer bleeding.");
                break;
        }

        /* Notice */
        notice = TRUE;
    }

    /* Use the value */
    SetCut(v);

    /* No change */
    if (!notice) return (FALSE);

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return (TRUE);
}


/*
 * Set "food", notice observable changes
 *
 * The "food" variable can get as large as 20000, allowing the
 * addition of the most "filling" item, Elvish Waybread, which adds
 * 7500 food units, without overflowing the 32767 maximum limit.
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
bool CPlayer::mod_food(int v)
{
    int old_aux, new_aux;
    bool notice = FALSE;

    // Hack -- Force good values
    v = (v > 20000) ? 20000 : (v < 0) ? 0 : v;

    /* Fainting / Starving */
    if (GetFood() < PY_FOOD_FAINT) {
        old_aux = 0;
    }

    /* Weak */
    else if (GetFood() < PY_FOOD_WEAK) {
        old_aux = 1;
    }

    /* Hungry */
    else if (GetFood() < PY_FOOD_ALERT) {
        old_aux = 2;
    }

    /* Normal */
    else if (GetFood() < PY_FOOD_FULL) {
        old_aux = 3;
    }

    /* Full */
    else if (GetFood() < PY_FOOD_MAX) {
        old_aux = 4;
    }

    /* Gorged */
    else {
        old_aux = 5;
    }

    /* Fainting / Starving */
    if (v < PY_FOOD_FAINT) {
        new_aux = 0;
    }

    /* Weak */
    else if (v < PY_FOOD_WEAK) {
        new_aux = 1;
    }

    /* Hungry */
    else if (v < PY_FOOD_ALERT) {
        new_aux = 2;
    }

    /* Normal */
    else if (v < PY_FOOD_FULL) {
        new_aux = 3;
    }

    /* Full */
    else if (v < PY_FOOD_MAX) {
        new_aux = 4;
    }

    /* Gorged */
    else {
        new_aux = 5;
    }

    /* Food increase */
    if (new_aux > old_aux) {
        /* Describe the state */
        switch (new_aux) {
            /* Weak */
            case 1:
                msg_print("You are still weak.");
                break;

            /* Hungry */
            case 2:
                msg_print("You are still hungry.");
                break;

            /* Normal */
            case 3:
                msg_print("You are no longer hungry.");
                break;

            /* Full */
            case 4:
                msg_print("You are full!");
                break;

            /* Bloated */
            case 5:
                msg_print("You have gorged yourself!");
                break;
        }

        /* Change */
        notice = TRUE;
    }

    /* Food decrease */
    else if (new_aux < old_aux) {
        /* Describe the state */
        switch (new_aux) {
            /* Fainting / Starving */
            case 0:
                msg_print("You are getting faint from hunger!");
                break;

            /* Weak */
            case 1:
                msg_print("You are getting weak from hunger!");
                break;

            /* Hungry */
            case 2:
                msg_print("You are getting hungry.");
                break;

            /* Normal */
            case 3:
                msg_print("You are no longer full.");
                break;

            /* Full */
            case 4:
                msg_print("You are no longer gorged.");
                break;
        }

        /* Change */
        notice = TRUE;
    }

    /* Use the value */
    SetFood(v);

    /* Nothing to notice */
    if (!notice) return FALSE;

    /* Recalculate bonuses */
    set_update(get_update() | PU_BONUS);

    /* Update stuff */
    update_stuff();

    /* Result */
    return TRUE;
}





/*
 * Advance experience levels and print experience
 */
void CPlayer::check_experience(void)
{
    int i;


    // Note current level
    i = GetLev();


    // Hack -- lower limits
    if (GetExp() < 0) SetExp(0);
    if (GetMaxExp() < 0) SetMaxExp(0);

    // Hack -- upper limits
    if (GetExp() > PY_MAX_EXP) SetExp(PY_MAX_EXP);
    if (GetMaxExp() > PY_MAX_EXP) SetMaxExp(PY_MAX_EXP);

    /* Hack -- maintain "max" experience */
    if (GetExp() > GetMaxExp()) {
        SetMaxExp(GetExp());
    }

    /* Update stuff */
    update_stuff();

    /* Lose levels while possible */
    while ((GetLev() > 1) &&
           (GetExp() < (player_exp[GetLev()-2] * GetExpFact() / 100L)))
    {
        /* Lose a level */
        SetLev(GetLev() - 1);

        //# Lose level

        /* Update some stuff */
        set_update(get_update() | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

        /* Update stuff */
        update_stuff();
    }

    /* Gain levels while possible */
    while ((GetLev() < PY_MAX_LEVEL) &&
           (GetExp() >= (player_exp[GetLev()-1] * GetExpFact() / 100L)))
    {
        /* Gain a level */
        SetLev(GetLev() + 1);

        /* Save the highest level */
        if (GetLev() > GetMaxPlv()) {
            SetMaxPlv(GetLev());
        }

        //# Level

        /* Message */
        msg_format("Welcome to level %d.", GetLev());

        /* Update some stuff */
        set_update(get_update() | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

        /* Update stuff */
        update_stuff();
    }
}


/*
 * Gain experience
 */
void CPlayer::gain_exp(s32b amount)
{
    /* Gain some experience */
    SetExp(GetExp() + amount);

    /* Slowly recover from experience drainage */
    if (GetExp() < GetMaxExp()) {
        /* Gain max experience (10%) */
        SetMaxExp(GetMaxExp() + amount/10);
    }

    /* Check Experience */
    check_experience();
}


/*
 * Lose experience
 */
void CPlayer::lose_exp(s32b amount)
{
    /* Never drop below zero experience */
    if (amount > GetExp()) amount = GetExp();

    /* Lose some experience */
    SetExp(GetExp() - amount);

    /* Check Experience */
    check_experience();
}




/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * XXX XXX XXX Note the use of actual "monster names"
 */
static int get_coin_type(CMonsterRace *r_ptr)
{
    char *name = r_name + r_ptr->name;

    /* Analyze creeping coins and golems */
    if ((r_ptr->type == TYPE_COINS) || (r_ptr->type == TYPE_GOLEM)) {
        /* Look for textual clues */
        if (strstr(name, "copper")) return (2);
        if (strstr(name, "silver")) return (5);
        if (strstr(name, "gold")) return (10);
        if (strstr(name, "mithril")) return (16);
        if (strstr(name, "adamantite")) return (17);

        /* Look for textual clues */
        if (strstr(name, "Copper")) return (2);
        if (strstr(name, "Silver")) return (5);
        if (strstr(name, "Gold")) return (10);
        if (strstr(name, "Mithril")) return (16);
        if (strstr(name, "Adamantite")) return (17);
    }

    /* Assume nothing */
    return (0);
}


/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that in a few, very rare, circumstances, killing Morgoth
 * may result in the Iron Crown of Morgoth crushing the Lead-Filled
 * Mace "Grond", since the Iron Crown is more important.
 */
void CMonster::monster_death(void)
{
    int i, j, y, x, ny, nx;
    int dump_item = 0, dump_gold = 0;
    int number = 0, total = 0;
    CGrid *g_ptr;
    CMonsterRace *r_ptr = get_r_ptr();

    bool visible = (is_visible() || (r_ptr->flags1 & RF1_UNIQUE));

    bool good = (r_ptr->flags1 & RF1_DROP_GOOD) ? TRUE : FALSE;
    bool great = (r_ptr->flags1 & RF1_DROP_GREAT) ? TRUE : FALSE;

    bool do_gold = (!(r_ptr->flags1 & RF1_ONLY_ITEM));
    bool do_item = (!(r_ptr->flags1 & RF1_ONLY_GOLD));

    int force_coin = get_coin_type(r_ptr);


    /* Get the location */
    x = GetX();
    y = GetY();

    // Determine how much we can drop
    if ((r_ptr->flags1 & RF1_DROP_60) && percent(60)) number++;
    if ((r_ptr->flags1 & RF1_DROP_90) && percent(90)) number++;
    if (r_ptr->flags1 & RF1_DROP_1D2) number += damroll(1, 2);
    if (r_ptr->flags1 & RF1_DROP_2D2) number += damroll(2, 2);
    if (r_ptr->flags1 & RF1_DROP_3D2) number += damroll(3, 2);
    if (r_ptr->flags1 & RF1_DROP_4D2) number += damroll(4, 2);
    if (get_spawned()) number = 0;

    // Drop the monster's inventory
    while (i_ptr) {
        CItem *next_i_ptr = i_ptr->next_i_ptr;

        // Drop it
        drop_near(i_ptr, -1, y, x);

        // Excise it
        delete i_ptr;

        // Next becomes first
        i_ptr = next_i_ptr;
    }

    /* Drop some objects */
    for (j = 0; j < number; j++) {
        // Try 20 times per item, increasing range
        for (i = 0; i < 20; ++i) {
            int d = (i + 14) / 15;

            // Pick a "correct" location
            scatter(&ny, &nx, y, x, d, 0);

            // Must be "clean" floor grid
            if (!clean_stackable_grid_bold(ny, nx)) continue;

            // Hack -- handle creeping coins
            coin_type = force_coin;

            // Place Gold
            if (do_gold && (!do_item || (rand_int(100) < 50))) {
                place_gold(nx, ny, rand_range(dun_level, r_ptr->level));
                if (p_ptr->can_see_bold(ny, nx)) dump_gold++;
            }

            // Place Item 
            else {
                place_object(ny, nx, good, great,
                    rand_range(dun_level, r_ptr->level));
                if (p_ptr->can_see_bold(ny, nx)) dump_item++;
            }

            /* Reset "coin" type */
            coin_type = 0;

            /* Notice */
            note_spot(ny, nx);

            /* Under the player */
            if (p_ptr->is_at(nx, ny)) {
                msg_print("You feel something roll beneath your feet.");
            }

            break;
        }
    }


    /* Take note of any dropped treasure */
    if (visible && (dump_item || dump_gold)) {
        /* Take notes on treasure */
        lore_treasure(dump_item, dump_gold);
    }


    /* Mega-Hack -- drop "winner" treasures */
    if (r_ptr->flags1 & RF1_DROP_CHOSEN) {
        CItem *prize;


        // Mega-Hack -- Prepare to make "Grond"
        prize = new CItem(TV_HAFTED, SV_GROND);

        // Mega-Hack -- Mark this item as "Grond"
        prize->SetName1(ART_GROND);

        // Mega-Hack -- Actually create "Grond"
        prize->apply_magic(-1, AM_ALLOW_ART | AM_FORCE_GOOD | AM_FORCE_GREAT);

        // Drop it in the dungeon
        drop_near(prize, -1, y, x);

        // Delete
        delete prize;


        // Mega-Hack -- Prepare to make "Morgoth"
        prize = new CItem(TV_CROWN, SV_MORGOTH);

        // Mega-Hack -- Mark this item as "Morgoth"
        prize->SetName1(ART_MORGOTH);

        // Mega-Hack -- Actually create "Morgoth"
        prize->apply_magic(-1, AM_ALLOW_ART | AM_FORCE_GOOD | AM_FORCE_GREAT);

        // Drop it in the dungeon
        drop_near(prize, -1, y, x);

        // Delete
        delete prize;
    }


    /* Only process "Quest Monsters" */
    if (!(r_ptr->flags1 & RF1_QUESTOR)) return;


    /* Hack -- Mark quests as complete */
    for (i = 0; i < MAX_Q_IDX; i++) {
        /* Hack -- note completed quests */
        if (q_list[i].level == r_ptr->level) q_list[i].level = 0;

        /* Count incomplete quests */
        if (q_list[i].level) total++;
    }


    /* Need some stairs */
    if (total) {
        /* Stagger around until we find a legal grid */
        while (!valid_grid(y, x)) {
            /* Pick a location */
            scatter(&ny, &nx, y, x, 1, 0);

            /* Stagger */
            y = ny; x = nx;
        }

        /* Delete any old objects XXX XXX XXX */
        delete_objects(y, x);

        /* Explain the stairway */
        msg_print("A magical stairway appears...");

        /* Access the grid */
        g_ptr = &cave[y][x];

        /* Create stairs down */
        //: Fix quest stairs
        //g_ptr->set_feat(CF_STAIRS_DOWN);

        /* Note the spot */
        note_spot(y, x);

        /* Remember to update everything */
        p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
    }


    /* Nothing left, game over... */
    else {
        /* Total winner */
        total_winner = TRUE;

        /* Congratulations */
        mini_message_box("Victory", "You have won the game!");
        msg_print("*** CONGRATULATIONS ***");
        msg_print("You have won the game!");
        msg_print("You may retire (commit suicide) when you are ready.");
    }
}




/*
 * Decreases monsters hit points, handling monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages.  Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Hack -- unseen monsters yield "You have killed it." message.
 *
 * Added fear (DGK) and check whether to print fear messages -CWS
 *
 * Genericized name, sex, and capitilization -BEN-
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * XXX XXX XXX Consider decreasing monster experience over time, say,
 * by using "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))"
 * instead of simply "(m_exp * m_lev) / (p_lev)", to make the first
 * monster worth more than subsequent monsters.  This would also need
 * to induce changes in the monster recall code.
 */
bool CMonster::mon_take_hit(int dam, bool *fear, char *note)
{
    CMonsterRace *r_ptr = get_r_ptr();
    s32b new_exp, new_exp_frac;


    // Wake it up
    set_csleep(0);

    // Hurt it
    SetCHP(GetCHP() - dam);

    // It is dead now
    if (GetCHP() < 0) {
        char m_name[80];

        // Extract monster name 
        get_desc(m_name, 0);

        //# Kill

        // Death by Missile/Spell attack
        if (note) {
            msg_format("%^s%s", m_name, note);
        }

        // Death by physical attack -- invisible monster
        else if (!is_visible()) {
            msg_format("You have killed %s.", m_name);
        }

        // Death by Physical attack -- non-living monster
        else if (r_ptr->isNonLiving() || (r_ptr->flags2 & RF2_STUPID)) {
            msg_format("You have destroyed %s.", m_name);
        }

        /* Death by Physical attack -- living monster */
        else {
            msg_format("You have slain %s.", m_name);
        }

        /* Give some experience */
        new_exp = ((long)r_ptr->mexp * r_ptr->level) / p_ptr->GetLev();
        new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % p_ptr->GetLev())
                        * 0x10000L / p_ptr->GetLev()) + p_ptr->GetExpFrac();

        /* Keep track of experience */
        if (new_exp_frac >= 0x10000L) {
            new_exp++;
            p_ptr->SetExpFrac(new_exp_frac - 0x10000);
        }
        else {
            p_ptr->SetExpFrac(new_exp_frac);
        }

        /* Gain experience */
        p_ptr->gain_exp(new_exp);

        /* Generate treasure */
        monster_death();

        /* When the player kills a Unique, it stays dead */
        if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 0;

        /* Recall even invisible uniques or winners */
        if (is_visible() || (r_ptr->flags1 & RF1_UNIQUE)) {
            /* Count kills this life */
            if (r_ptr->r_pkills < MAX_SHORT) r_ptr->r_pkills++;

            /* Count kills in all lives */
            if (r_ptr->r_tkills < MAX_SHORT) r_ptr->r_tkills++;
        }

        /* Not afraid */
        *fear = FALSE;

        /* Delete the monster */
        delete_monster(this);

        /* Monster is dead */
        return TRUE;
    }


    /* Mega-Hack -- Pain cancels fear */
    if (get_afraid() && (dam > 0)) {
        int tmp = randint(dam);

        /* Cure a little fear */
        if (tmp < get_afraid()) {
            /* Reduce fear */
            set_afraid(get_afraid() - tmp);
        }

        /* Cure all the fear */
        else {
            /* Cure fear */
            set_afraid(0);

            /* No more fear */
            *fear = FALSE;
        }
    }

    /* Sometimes a monster gets scared by damage */
    if (!get_afraid() && !(r_ptr->flags3 & RF3_NO_FEAR)) {
        int percentage;

        /* Percentage of fully healthy */
        percentage = (100L * GetCHP()) / GetMHP();

        /*
         * Run (sometimes) if at 10% or less of max hit points,
         * or (usually) when hit for half its current hit points
         */
        if (((percentage <= 10) && (rand_int(10) < percentage)) ||
            ((dam >= GetCHP()) && percent(80)))
        {
            /* Hack -- note fear */
            (*fear) = TRUE;

            /* XXX XXX XXX Hack -- Add some timed fear */
            set_afraid(randint(10) +
                (((dam >= GetCHP()) && (percentage > 7)) ?
                20 : ((11 - percentage) * 5)));
        }
    }

    /* Not dead yet */
    return FALSE;
}



/*
 * Utumno sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(vptr u, vptr v, int p, int q)
{
    int z, a, b;

    /* Done sort */
    if (p >= q) return;

    /* Pivot */
    z = p;

    /* Begin */
    a = p;
    b = q;

    /* Partition */
    while (TRUE) {
        /* Slide i2 */
        while (!(*ang_sort_comp)(u, v, b, z)) b--;

        /* Slide i1 */
        while (!(*ang_sort_comp)(u, v, z, a)) a++;

        /* Done partition */
        if (a >= b) break;

        /* Swap */
        (*ang_sort_swap)(u, v, a, b);

        /* Advance */
        a++, b--;
    }

    /* Recurse left side */
    ang_sort_aux(u, v, p, b);

    /* Recurse right side */
    ang_sort_aux(u, v, b+1, q);
}


/*
 * Utumno sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(vptr u, vptr v, int n)
{
    /* Sort the array */
    ang_sort_aux(u, v, 0, n-1);
}



/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
bool ang_sort_comp_distance(vptr u, vptr v, int a, int b)
{
    byte *x = (byte*)(u);
    byte *y = (byte*)(v);

    int da, db, kx, ky;

    /* Absolute distance components */
    kx = x[a]; kx -= p_ptr->GetX(); kx = ABS(kx);
    ky = y[a]; ky -= p_ptr->GetY(); ky = ABS(ky);

    /* Approximate Double Distance to the first point */
    da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

    /* Absolute distance components */
    kx = x[b]; kx -= p_ptr->GetX(); kx = ABS(kx);
    ky = y[b]; ky -= p_ptr->GetY(); ky = ABS(ky);

    /* Approximate Double Distance to the first point */
    db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

    /* Compare the distances */
    return (da <= db);
}


/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
void ang_sort_swap_distance(vptr u, vptr v, int a, int b)
{
    byte *x = (byte*)(u);
    byte *y = (byte*)(v);

    byte temp;

    /* Swap "x" */
    temp = x[a];
    x[a] = x[b];
    x[b] = temp;

    /* Swap "y" */
    temp = y[a];
    y[a] = y[b];
    y[b] = temp;
}

