/* File: effects.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: effects of various "objects" */

#include "angband.h"

#include <assert.h>

bool free_act_save_p(int ml)
{
    int i, skill = plr->skills.sav;
    for (i = 0; i < plr->free_act; i++)
    {
        if (randint0(50 + ml) < skill)
        {
            equip_learn_flag(OF_FREE_ACT);
            return TRUE;
        }
        skill = skill * 2 / 3;
    }
    return FALSE;
}

void set_action(int typ)
{
    int prev_typ = plr->action;

    if (typ == prev_typ)
        return;
    else
    {
        switch (prev_typ)
        {
        case ACTION_SEARCH:
            msg_print("You no longer walk carefully.");
            plr->redraw |= PR_EFFECTS;
            break;
        case ACTION_REST:
            resting = 0;
            break;
        case ACTION_LEARN:
            msg_print("You stop Learning");
            new_mane = FALSE;
            break;
        case ACTION_KAMAE:
            msg_print("You stop assuming the posture.");
            plr->special_defense &= ~(KAMAE_MASK);
            break;
        case ACTION_KATA:
            msg_print("You stop assuming the posture.");
            plr->special_defense &= ~(KATA_MASK);
            plr->update |= (PU_MONSTERS);
            plr->redraw |= (PR_STATUS);
            break;
        case ACTION_SING:
            msg_print("You stop singing.");
            break;
        case ACTION_QUICK_WALK:
            msg_print("You are no longer moving extremely fast.");
            break;
        case ACTION_SPELL:
            msg_print("You stop chanting.");
            break;
        case ACTION_STALK:
            msg_print("You no longer stalk your prey.");
            break;
        }
    }

    plr->action = typ;

    if (prev_typ == ACTION_SING) music_stop();

    switch (plr->action)
    {
    case ACTION_SEARCH:
        msg_print("You begin to walk carefully.");
        plr->redraw |= PR_EFFECTS;
        break;
    case ACTION_LEARN:
        msg_print("You begin Learning");
        break;
    case ACTION_QUICK_WALK:
        msg_print("You begin to move extremely fast.");
        break;
    case ACTION_STALK:
        msg_print("You begin to stalk your prey.");
        break;
    }

    plr->update |= (PU_BONUS);
    if (plr->action == ACTION_GLITTER || prev_typ == ACTION_GLITTER)
        plr->update |= PU_FLOW;
    plr->redraw |= (PR_STATE);
}

/* reset timed flags */
void reset_tim_flags(void)
{
    plr_tim_clear();

    plr->afraid = 0;          /* Timed -- Fear */
    plr->tim_mimic = 0;
    plr->mimic_form = MIMIC_NONE;

    plr->action = ACTION_NONE;
    
    plr->fasting = FALSE;

    plr->sutemi = FALSE;
    plr->counter = FALSE;
    plr->special_attack = 0;
    if (plr->special_defense & DEFENSE_SANCTUARY)
        plr_tim_unlock(T_INVULN);
    plr->special_defense = 0;

    while(plr->energy_need < 0) plr->energy_need += ENERGY_NEED();
    world_player = FALSE;

    if (plr->riding)
        mon_tim_clear(dun_mon(cave, plr->riding));

    if (plr->pclass == CLASS_BARD)
    {
        plr->magic_num1[0] = 0;
        plr->magic_num2[0] = 0;
    }
}


void dispel_player(void)
{
    plr_tim_dispel();
    
    /* Its important that doppelganger gets called correctly and not set_mimic()
       since we monkey with things like the experience factor! */
    if (plr->prace == RACE_DOPPELGANGER && plr->mimic_form != MIMIC_NONE && !plr->tim_mimic)
        mimic_race(MIMIC_NONE, NULL);
    else
        (void)set_mimic(0, 0, TRUE);
        
    set_sanctuary(FALSE);

    mimic_dispel_player();


    /* Cancel glowing hands */
    if (plr->special_attack & ATTACK_CONFUSE)
    {
        plr->special_attack &= ~(ATTACK_CONFUSE);
        msg_print("Your hands stop glowing.");
    }
}


/*
 * Set "plr->tim_mimic", and "plr->mimic_form",
 * notice observable changes
 */
bool set_mimic(int v, int p, bool do_dec)
{
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    if (plr->is_dead) return FALSE;

    /* Open */
    if (v)
    {
        if (plr->tim_mimic && (plr->mimic_form == p) && !do_dec)
        {
            if (plr->tim_mimic > v) return FALSE;
        }
        else if ((!plr->tim_mimic) || (plr->mimic_form != p))
        {
            msg_print("You feel that your body changes.");
            plr->mimic_form=p;
            notice = TRUE;
        }
    }

    /* Shut */
    else
    {
        if (plr->tim_mimic)
        {
            msg_print("You are no longer transformed.");
            plr->mimic_form= MIMIC_NONE;
            notice = TRUE;
            p = MIMIC_NONE;
        }
    }

    /* Use the value */
    plr->tim_mimic = v;
    equip_on_change_race();

    /* Nothing to notice */
    if (!notice)
        return (FALSE);

    /* Disturb */
    if (disturb_state)
        disturb(0, 0);

    /* Redraw title */
    plr->redraw |= (PR_BASIC | PR_STATUS | PR_MAP | PR_EQUIPPY | PR_EFFECTS);

    /* Recalculate bonuses */
    plr->update |= (PU_BONUS | PU_INNATE | PU_HP | PU_MANA | PU_TORCH);
    handle_stuff();

    /* Result */
    return (TRUE);
}

bool set_sanctuary(bool set)
{
    bool notice = FALSE;

    if (plr->is_dead) return FALSE;

    if (set)
    {
        if (!(plr->special_defense & DEFENSE_SANCTUARY))
        {
            msg_print("You claim Sanctuary!  Nothing can hurt you now!!");
            notice = TRUE;
            plr->special_defense |= DEFENSE_SANCTUARY;
            plr_tim_lock(T_INVULN);
        }
    }
    else
    {
        if (plr->special_defense & DEFENSE_SANCTUARY)
        {
            msg_print("You no longer feel safe.");
            notice = TRUE;
            plr->special_defense &= ~(DEFENSE_SANCTUARY);
            plr_tim_unlock(T_INVULN);
        }
    }

    if (!notice) return FALSE;
    plr->redraw |= (PR_STATUS);
    if (disturb_state) disturb(0, 0);
    return TRUE;
}

bool set_superstealth(bool set)
{
    bool notice = FALSE;

    if (plr->is_dead) return FALSE;

    /* Open */
    if (set)
    {
        if (!(plr->special_defense & NINJA_S_STEALTH))
        {
            int lite = plr_light(plr->pos);
            if (lite > 0)
            {
                if (disturb_minor)
                    msg_print("<color:D>You are mantled in weak shadow from ordinary eyes.</color>");
                plr->monlite = plr->old_monlite = TRUE;
            }
            else
            {
                if (disturb_minor)
                    msg_print("<color:D>You are mantled in shadow from ordinary eyes!</color>");
                plr->monlite = plr->old_monlite = FALSE;
            }

            notice = TRUE;

            /* Use the value */
            plr->special_defense |= NINJA_S_STEALTH;
        }
    }

    /* Shut */
    else
    {
        if (plr->special_defense & NINJA_S_STEALTH)
        {
            if (disturb_minor)
                msg_print("<color:y>You are exposed to common sight once more.</color>");

            notice = TRUE;

            /* Use the value */
            plr->special_defense &= ~(NINJA_S_STEALTH);
        }
    }

    /* Nothing to notice */
    if (!notice) return (FALSE);

    /* Redraw status bar */
    plr->redraw |= PR_EFFECTS;

    /* Disturb */
    if (disturb_state) disturb(0, 0);

    /* Result */
    return (TRUE);
}
bool set_invisible(bool set)
{
    bool notice = FALSE;

    if (plr->is_dead) return FALSE;

    if (set)
    {
        if (!(plr->special_defense & DEFENSE_INVISIBLE))
        {
            if (disturb_minor)
                msg_print("<color:D>You are invisible to ordinary eyes!</color>");

            notice = TRUE;
            plr->special_defense |= DEFENSE_INVISIBLE;
        }
    }
    else
    {
        if (plr->special_defense & DEFENSE_INVISIBLE)
        {
            if (disturb_minor)
                msg_print("<color:y>You have revealed yourself to common sight.</color>");

            notice = TRUE;
            plr->special_defense &= ~(DEFENSE_INVISIBLE);
        }
    }
    if (notice)
    {
        plr->redraw |= PR_EFFECTS;
        if (disturb_state) disturb(0, 0);
    }
    return notice;
}

/*
 * Set "plr->food", notice observable changes
 *
 * The "plr->food" variable can get as large as 20000, allowing the
 * addition of the most "filling" item, Elvish Waybread, which adds
 * 7500 food units, without overflowing the 32767 maximum limit.
 *
 * Perhaps we should disturb the player with various messages,
 * especially messages about hunger status changes. XXX XXX XXX
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
    int old_pct;
    int new_pct;

    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 20000) ? 20000 : (v < 0) ? 0 : v;

    /* CTK: I added a "food bar" to track hunger ... */
    old_pct = plr->food * 100 / PY_FOOD_FULL;
    new_pct = v * 100 / PY_FOOD_FULL;

    /* Fainting / Starving */
    if (plr->food < PY_FOOD_FAINT)
    {
        old_aux = 0;
    }

    /* Weak */
    else if (plr->food < PY_FOOD_WEAK)
    {
        old_aux = 1;
    }

    /* Hungry */
    else if (plr->food < PY_FOOD_ALERT)
    {
        old_aux = 2;
    }

    /* Normal */
    else if (plr->food < PY_FOOD_FULL)
    {
        old_aux = 3;
    }

    /* Full */
    else if (plr->food < PY_FOOD_MAX)
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

    if (old_aux < 1 && new_aux > 0)
        virtue_add(VIRTUE_PATIENCE, 2);
    else if (old_aux < 3 && (old_aux != new_aux))
        virtue_add(VIRTUE_PATIENCE, 1);
    if (old_aux == 2)
        virtue_add(VIRTUE_TEMPERANCE, 1);
    if (old_aux == 0)
        virtue_add(VIRTUE_TEMPERANCE, -1);

    if (display_food_bar && new_pct != old_pct)
        notice = TRUE;

    /* Food increase */
    if (new_aux > old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
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
            virtue_add(VIRTUE_HARMONY, -1);
            virtue_add(VIRTUE_PATIENCE, -1);
            virtue_add(VIRTUE_TEMPERANCE, -2);

            break;
        }

        /* Change */
        notice = TRUE;
    }

    if ((v > plr->food) && plr->fasting)
    {
        msg_print("You break your fast.");
        plr->redraw |= PR_EFFECTS;
        plr->fasting = FALSE;
    }

    /* Food decrease */
    else if (new_aux < old_aux)
    {
        /* Describe the state */
        switch (new_aux)
        {
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
    plr->food = v;

    /* Nothing to notice */
    if (!notice) return (FALSE);

    if (new_aux != old_aux)
    {
        /* Disturb */
        if (disturb_state) disturb(0, 0);

        /* Recalculate bonuses */
        plr->update |= (PU_BONUS);
    }

    /* Redraw hunger */
    plr->redraw |= PR_EFFECTS;
    if (display_food_bar)
        plr->redraw |= PR_HEALTH_BARS;

    /* Handle stuff */
    handle_stuff();

    /* Result */
    return (TRUE);
}

/*
 * Increases a stat by one randomized level             -RAK-
 *
 * Note that this function (used by stat potions) now restores
 * the stat BEFORE increasing it.
 */
bool inc_stat(int stat)
{
    int value, gain;

    /* Then augment the current/max stat */
    value = plr->stat_cur[stat];

    /* Cannot go above 18/100 */
    if (value < plr->stat_max_max[stat])
    {
        /* Gain one (sometimes two) points */
        if (value < 18)
        {
            gain = ((randint0(100) < 75) ? 1 : 2);
            value += gain;
        }
        else if (value < (plr->stat_max_max[stat]-2))
        {                                                  /* v--- Scale all calcs by 10 */
            int delta = (plr->stat_max_max[stat] - value) * 10;
            int pct = rand_range(200, 350);                /* Note: Old spread was about 14% to 40% */
            int max_value = plr->stat_max_max[stat] - 1; /* e.g. 18/99 if max is 18/100 */
            int gain;

            gain = delta * pct / 1000;
            gain = (gain + 5) / 10; /* round back to an integer */
            if (gain < 2)
                gain = 2;

            value += gain;
            if (value > max_value)
                value = max_value;
        }
        /* Gain one point at a time */
        else
        {
            value++;
        }

        /* Save the new value */
        plr->stat_cur[stat] = value;

        /* Bring up the maximum too */
        if (value > plr->stat_max[stat])
        {
            plr->stat_max[stat] = value;
        }

        /* Recalculate bonuses */
        plr->update |= (PU_BONUS);

        /* Success */
        return (TRUE);
    }

    /* Nothing to gain */
    return (FALSE);
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Amount could be a little higher in extreme cases to mangle very high
 * stats from massive assaults. -CWS
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent. This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount, int permanent)
{
    int cur, max, loss, same, res = FALSE;


    /* Acquire current value */
    cur = plr->stat_cur[stat];
    max = plr->stat_max[stat];

    /* Note when the values are identical */
    same = (cur == max);

    /* Damage "current" value */
    if (cur > 3)
    {
        /* Handle "low" values */
        if (cur <= 18)
        {
            if (amount > 90) cur--;
            if (amount > 50) cur--;
            if (amount > 20) cur--;
            cur--;
        }

        /* Handle "high" values */
        else
        {
            /* Hack -- Decrement by a random amount between one-quarter */
            /* and one-half of the stat bonus times the percentage, with a */
            /* minimum damage of half the percentage. -CWS */
            loss = (((cur-18) / 2 + 1) / 2 + 1);

            /* Paranoia */
            if (loss < 1) loss = 1;

            /* Randomize the loss */
            loss = ((randint1(loss) + loss) * amount) / 100;

            /* Maximal loss */
            if (loss < amount/2) loss = amount/2;

            /* Lose some points */
            cur = cur - loss;

            /* Hack -- Only reduce stat to 17 sometimes */
            if (cur < 18) cur = (amount <= 20) ? 18 : 17;
        }

        /* Prevent illegal values */
        if (cur < 3) cur = 3;

        /* Something happened */
        if (cur != plr->stat_cur[stat]) res = TRUE;
    }

    /* Damage "max" value */
    if (permanent && (max > 3))
    {
        virtue_add(VIRTUE_SACRIFICE, 1);
        if (stat == A_WIS || stat == A_INT)
            virtue_add(VIRTUE_ENLIGHTENMENT, -2);

        /* Handle "low" values */
        if (max <= 18)
        {
            if (amount > 90) max--;
            if (amount > 50) max--;
            if (amount > 20) max--;
            max--;
        }

        /* Handle "high" values */
        else
        {
            /* Hack -- Decrement by a random amount between one-quarter */
            /* and one-half of the stat bonus times the percentage, with a */
            /* minimum damage of half the percentage. -CWS */
            loss = (((max-18) / 2 + 1) / 2 + 1);
            loss = ((randint1(loss) + loss) * amount) / 100;
            if (loss < amount/2) loss = amount/2;

            /* Lose some points */
            max = max - loss;

            /* Hack -- Only reduce stat to 17 sometimes */
            if (max < 18) max = (amount <= 20) ? 18 : 17;
        }

        /* Hack -- keep it clean */
        if (same || (max < cur)) max = cur;

        /* Something happened */
        if (max != plr->stat_max[stat]) res = TRUE;
    }

    /* Apply changes */
    if (res)
    {
        /* Actually set the stat to its new value. */
        plr->stat_cur[stat] = cur;
        plr->stat_max[stat] = max;

        /* Redisplay the stats later */
        plr->redraw |= (PR_STATS);

        /* Recalculate bonuses */
        plr->update |= (PU_BONUS);
    }

    /* Done */
    return (res);
}


/*
 * Restore a stat. Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
    /* Restore if needed */
    if (plr->stat_cur[stat] != plr->stat_max[stat])
    {
        /* Restore */
        plr->stat_cur[stat] = plr->stat_max[stat];

        /* Recalculate bonuses */
        plr->update |= (PU_BONUS);

        /* Redisplay the stats later */
        plr->redraw |= (PR_STATS);

        /* Success */
        return (TRUE);
    }

    /* Nothing to restore */
    return (FALSE);
}


/*
 * Increase players hit points, notice effects
 */
bool hp_player(int num)
{
    if (plr->pclass == CLASS_BLOOD_KNIGHT)
    {
        num /= 2;        
        if (num == 0)
            return FALSE;
    }
    return hp_player_aux(num);
}

bool hp_player_aux(int num)
{
    int old_hp = plr->chp;

    num = num * (virtue_current(VIRTUE_VITALITY) + 1250) / 1250;

    if (mut_present(MUT_SACRED_VITALITY))
    {
        num += num/5;
    }

    /* Healing needed */
    if (plr->chp < plr->mhp)
    {
        if ((num > 0) && (plr->chp < (plr->mhp/3)))
            virtue_add(VIRTUE_TEMPERANCE, 1);

        /* XXX Handle device lore ... of course, we don't know if a device
         * is actually being used atm, but it won't hurt to set the variable anyway. */
        if (plr->chp + num <= plr->mhp)
            device_lore = TRUE;

        /* Gain hitpoints */
        plr->chp += num;

        /* Enforce maximum */
        if (plr->chp >= plr->mhp)
        {
            plr->chp = plr->mhp;
            plr->chp_frac = 0;

            if (weaponmaster_is_(WEAPONMASTER_STAVES))
                plr->update |= (PU_BONUS);
        }

        if (plr->pclass == CLASS_BLOOD_KNIGHT)
            plr->update |= PU_BONUS;

        /* Redraw */
        plr->redraw |= (PR_HP);

        fear_heal_p(old_hp, plr->chp);

        /* Notice */
        return (TRUE);
    }

    /* Ignore */
    return (FALSE);
}

/* Player Life
 * Drained by Undead GF_UNLIFE attacks. Drain below 0 turns the player into an undead!
 * Restored by vampiric weapons or powerful healing or !RestoreLife
 * Gained by player GF_UNLIFE attacks (Undead races only ... cf RACE_MON_LICH) */
bool plr_drain_life(int amt)
{
    bool notice = FALSE;
    int old_clp = plr->clp;
    assert(!(get_race()->flags & RACE_IS_NONLIVING));
    plr->clp -= amt;
    if (plr->clp <= 0)
    {
        if (plr->pclass != CLASS_MONSTER)
        {
            int which = RACE_SKELETON;
            switch (randint1(4))
            {
            case 1: which = RACE_VAMPIRE; break;
            case 2: which = RACE_SKELETON; break;
            case 3: which = RACE_ZOMBIE; break;
            case 4: which = RACE_SPECTRE; break;
            }

            msg_print("<color:v>Your life force is exhausted!</color>");
            change_race(which, "");
            plr->clp = 1000; /* full unlife */
            assert(get_race()->flags & RACE_IS_NONLIVING); /* no more life drain */
            notice = TRUE;
        }
        else
            plr->clp = 0; /* monsters can't change their race ... */
    }
    if (plr->clp != old_clp)
    {
        msg_print("<color:D>You feel your life draining away!</color>");
        plr->update |= PU_HP;
        plr->redraw |= PR_EFFECTS;
        notice = TRUE;
    }
    return notice;
}
bool plr_restore_life(int amt)
{
    bool notice = FALSE;
    int old_clp = plr->clp;
    if (plr->clp >= 1000) return FALSE; /* don't tank undead power bonus */
    plr->clp += amt;
    if (plr->clp > 1000) plr->clp = 1000;
    if (plr->clp != old_clp)
    {
        msg_print("<color:B>You feel your life returning.</color>");
        plr->update |= PU_HP;
        plr->redraw |= PR_EFFECTS;
        notice = TRUE;
    }
    return notice;
}
bool plr_gain_life(int amt)
{
    /* Only undead players can GF_UNLIFE to gain life. They can never
     * lose life. */
    assert(get_race()->flags & RACE_IS_UNDEAD);
    assert(plr->clp >= 1000);

    plr->clp += amt;
    msg_print("<color:R>You grow more powerful!</color>");
    plr->update |= PU_HP;
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}

/* vampiric drain goes first to recovering the player's life,
 * and then, if any is left over, to recovering hit points */
bool vamp_player(int num)
{
    if (plr->clp + num <= 1000)
        return plr_restore_life(num);
    else if (plr->clp < 1000)
    {
        int lp = 1000 - plr->clp;
        plr_restore_life(lp);
        num -= lp;
        assert(num > 0);
    }
    return hp_player_aux(num);
}

bool sp_player(int num)
{
    bool notice = FALSE;
    int old_csp = plr->csp;

    plr->csp += num;
    if (num > 0 && plr->csp > plr->msp) /* Mystics and Samurai super charge */
    {
        plr->csp = plr->msp;
        plr->csp_frac = 0;
    }
    if (plr->csp < 0)
    {
        plr->csp = 0;
        plr->csp_frac = 0;
    }
    if (plr->csp != old_csp)
    {
        plr->redraw |= PR_MANA;
        notice = TRUE;
    }
    return notice;
}

/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_pos[] =
{
    "strong",
    "smart",
    "wise",
    "dextrous",
    "healthy",
    "self-confident"
};


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
    "weak",
    "stupid",
    "naive",
    "clumsy",
    "sickly",
    "insecure"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
    bool sust = FALSE;

    /* Access the "sustain" */
    switch (stat)
    {
        case A_STR: if (plr->sustain_str) sust = TRUE; break;
        case A_INT: if (plr->sustain_int) sust = TRUE; break;
        case A_WIS: if (plr->sustain_wis) sust = TRUE; break;
        case A_DEX: if (plr->sustain_dex) sust = TRUE; break;
        case A_CON: if (plr->sustain_con) sust = TRUE; break;
        case A_CHR: if (plr->sustain_chr) sust = TRUE; break;
    }

    /* Sustain */
    if (sust && randint0(13))
    {
        if (disturb_minor)
            msg_format("You feel %s for a moment, but the feeling passes.", desc_stat_neg[stat]);

        equip_learn_flag(OF_SUST_STR + stat);
        return TRUE;
    }

    /* Attempt to reduce the stat */
    if (dec_stat(stat, 10, FALSE))
    {
        /* Message */
        msg_format("You feel very <color:r>%s</color>.", desc_stat_neg[stat]);


        /* Notice effect */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
    /* Attempt to increase */
    if (res_stat(stat))
    {
        /* Message */
        msg_format("You feel less %s.", desc_stat_neg[stat]);


        /* Notice */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}


/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
    bool res;

    /* Restore strength */
    res = res_stat(stat);

    /* Attempt to increase */
    if (inc_stat(stat))
    {
        if (stat == A_WIS)
        {
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            virtue_add(VIRTUE_FAITH, 1);
        }
        else if (stat == A_INT)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
        }
        else if (stat == A_CON)
            virtue_add(VIRTUE_VITALITY, 1);

        /* Message */
        msg_format("Wow! You feel very %s!", desc_stat_pos[stat]);


        /* Notice */
        return (TRUE);
    }

    /* Restoration worked */
    if (res)
    {
        /* Message */
        msg_format("You feel less %s.", desc_stat_neg[stat]);


        /* Notice */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}


/*
 * Restores any drained experience
 */
bool restore_level(void)
{
    s32b max_exp = plr->max_exp;

    /* Possessor Max Lvl is limited by their current form */
    if (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
    {
        s32b racial_max = possessor_max_exp();
        if (max_exp > racial_max)
            max_exp = racial_max;
    }

    if (plr->exp < max_exp)
    {
        msg_print("You feel your life energies returning.");
        plr->exp = max_exp;
        check_experience();
        return TRUE;
    }
    return FALSE;
}

/*
 * Forget everything
 */
static void _forget(obj_ptr obj)
{
    if (!obj_is_identified_fully(obj))
    {
        obj->feeling = FEEL_NONE;
        obj->ident &= ~(IDENT_EMPTY);
        obj->ident &= ~(IDENT_TRIED);
        obj->ident &= ~(IDENT_KNOWN);
        obj->ident &= ~(IDENT_SENSE);
    }
}

bool lose_all_info(void)
{
    virtue_add(VIRTUE_KNOWLEDGE, -5);
    virtue_add(VIRTUE_ENLIGHTENMENT, -5);

    if (!plr->auto_id)
    {
        pack_for_each(_forget);
        equip_for_each(_forget);
        quiver_for_each(_forget);

        plr->update |= PU_BONUS;
        plr->notice |= PN_OPTIMIZE_PACK;
        plr->window |= (PW_INVEN | PW_EQUIP | PW_OBJECT_LIST);
    }
    wiz_dark();

    return TRUE;
}

void do_poly_wounds(void)
{
    /* Changed to always provide at least _some_ healing */
    s16b wounds = plr_tim_amount(T_CUT);
    s16b hit_p = (plr->mhp - plr->chp);
    s16b change = damroll(plr->lev, 5);
    bool Nasty_effect = one_in_(5);

    if (!(wounds || hit_p || Nasty_effect)) return;

    msg_print("Your wounds are polymorphed into less serious ones.");

    hp_player(change);
    if (Nasty_effect)
    {
        msg_print("A new wound was created!");
        take_hit(DAMAGE_LOSELIFE, change / 2, "a polymorphed wound");

        if (!plr->no_cut) plr_tim_add(T_CUT, change);
    }
    else
    {
        plr_tim_subtract(T_CUT, change/2);
    }
}


/*
 * Change player race
 */
void change_race(int new_race, cptr effect_msg)
{
    cptr title = get_race_aux(new_race, 0)->name;
    int  old_race = plr->prace;
    static bool _lock = FALSE; /* This effect is not re-entrant! */

    if (_lock) return;
    if (get_race()->flags & RACE_IS_MONSTER) return;
    if (get_race_aux(new_race, 0)->flags & RACE_IS_MONSTER) return;
    if (old_race == RACE_ANDROID) return;
    if (old_race == RACE_DOPPELGANGER) return;
    if (new_race == old_race) return;

    _lock = TRUE;

    if (old_race == RACE_HUMAN || old_race == RACE_DEMIGOD)
    {
        int i, idx;
        for (i = 0; i < MAX_DEMIGOD_POWERS; i++)
        {
            idx = plr->demigod_power[i];
            if (idx >= 0)
            {
                mut_unlock(idx);
                mut_lose(idx);
                plr->demigod_power[i] = -1;
            }
        }
        plr->psubrace = 0;
    }
    if (old_race == RACE_DRACONIAN)
    {
        int idx = plr->draconian_power;
        if (idx >= 0)
        {
            mut_unlock(idx);
            mut_lose(idx);
            plr->draconian_power = -1;
            if (idx == MUT_DRACONIAN_METAMORPHOSIS)
                equip_on_change_race();
        }
        plr->psubrace = 0;
    }

    msg_format("You turn into %s %s%s!", (!effect_msg[0] && is_a_vowel(title[0]) ? "an" : "a"), effect_msg, title);

    virtue_add(VIRTUE_CHANCE, 2);

    if (plr->prace < 32)
    {
        plr->old_race1 |= 1L << plr->prace;
    }
    else
    {
        plr->old_race2 |= 1L << (plr->prace-32);
    }
    plr->prace = new_race;
    plr->psubrace = 0;

    /* Experience factor */
    plr->expfact = calc_exp_factor();

    do_cmd_rerate(FALSE);

    /* The experience level may be modified */
    check_experience();

    if (plr->prace == RACE_HUMAN || plr->prace == RACE_DEMIGOD || plr->prace == RACE_DRACONIAN)
    {
        race_t *race_ptr = get_true_race();
        if (race_ptr != NULL && race_ptr->hooks.gain_level != NULL)
            race_ptr->hooks.gain_level(plr->lev);    /* This is OK ... Just make sure we get to choose racial powers on poly */
    }

    plr->redraw |= (PR_BASIC);

    plr->update |= PU_BONUS | PU_INNATE;

    handle_stuff();

    /* Load an autopick preference file */
    if (old_race != plr->prace) autopick_load_pref(FALSE);

    /* Player's graphic tile may change */
    draw_pos(plr->pos);

    _lock = FALSE;
}


void do_poly_self(void)
{
    int power = plr->lev;

    msg_print("You feel a change coming over you...");

    virtue_add(VIRTUE_CHANCE, 1);

    if ((power > randint0(20)) && one_in_(3) && (plr->prace != RACE_ANDROID))
    {
        char effect_msg[80] = "";
        int new_race, expfact, goalexpfact;

        /* Some form of racial polymorph... */
        power -= 10;

        if ((power > randint0(5)) && one_in_(4))
        {
            /* sex change */
            power -= 2;

            if (plr->psex == SEX_MALE)
            {
                plr->psex = SEX_FEMALE;
                sprintf(effect_msg, "female ");

            }
            else
            {
                plr->psex = SEX_MALE;
                sprintf(effect_msg, "male ");

            }
        }

        if ((power > randint0(30)) && one_in_(5))
        {
            int tmp = 0;

            /* Harmful deformity */
            power -= 15;

            while (tmp < 6)
            {
                if (one_in_(2))
                {
                    (void)dec_stat(tmp, randint1(6) + 6, one_in_(3));
                    power -= 1;
                }
                tmp++;
            }

            /* Deformities are discriminated against! */
            (void)dec_stat(A_CHR, randint1(6), TRUE);

            if (effect_msg[0])
            {
                char tmp_msg[80];
                sprintf(tmp_msg,"%s ",effect_msg);
                sprintf(effect_msg,"deformed %s ",tmp_msg);

            }
            else
            {
                sprintf(effect_msg,"deformed ");

            }
        }

        while ((power > randint0(20)) && one_in_(10))
        {
            /* Polymorph into a less mutated form */
            power -= 10;

            if (!mut_lose_random(NULL))
                msg_print("You feel oddly normal.");

        }

        /*
         * Restrict the race choices by exp penalty so
         * weak polymorph always means weak race
         */
        if (power < 0)
            goalexpfact = 100;
        else
            goalexpfact = 100 + 3 * randint0(power);

        do
        {
            new_race = plr_race_polymorph();
            expfact = get_race_aux(new_race, 0)->exp;
        }
        while (expfact > goalexpfact);

        change_race(new_race, effect_msg);
    }

    if ((power > randint0(30)) && one_in_(6))
    {
        int tmp = 0;

        /* Abomination! */
        power -= 20;

        msg_print("Your internal organs are rearranged!");

        while (tmp < 6)
        {
            (void)dec_stat(tmp, randint1(6) + 6, one_in_(3));
            tmp++;
        }
        if (one_in_(6))
        {
            msg_print("You find living difficult in your present form!");
            take_hit(DAMAGE_LOSELIFE, damroll(randint1(10), plr->lev), "a lethal mutation");

            power -= 10;
        }
    }

    if ((power > randint0(20)) && one_in_(4))
    {
        power -= 10;

        get_max_stats();
        do_cmd_rerate(FALSE);
    }

    while ((power > randint0(15)) && one_in_(3))
    {
        power -= 7;
        mut_gain_random(NULL);
    }

    if (power > randint0(5))
    {
        power -= 5;
        do_poly_wounds();
    }

    /* Note: earlier deductions may have left power < 0 already. */
    while (power > 0)
    {
        mutate_player();
        power--;
    }
}

/*
 * Decreases players hit points and sets death flag if necessary
 *
 * XXX XXX XXX Invulnerability needs to be changed into a "shield"
 *
 * XXX XXX XXX Hack -- this function allows the user to save (or quit)
 * the game when he dies, since the "You die." message is shown before
 * setting the player to "dead".
 */

int take_hit(int damage_type, int damage, cptr hit_from)
{
    int old_chp = plr->chp;

    char death_message[1024];
    int warning = (plr->mhp * hitpoint_warn / 10);

    /* Paranoia */
    if (plr->is_dead) return 0;
    if (!damage) return 0;

    if (plr->sutemi) damage *= 2;
    if (plr->special_defense & KATA_IAI) damage += (damage + 4) / 5;
    if (check_foresight()) return 0;
    if (statistics_hack) return 0;

    if (damage_type != DAMAGE_USELIFE)
    {
        /* Disturb */
        disturb(1, 0);
    }

    /* Mega-Hack -- Apply "invulnerability" */
    if ( damage_type != DAMAGE_USELIFE
      && damage_type != DAMAGE_LOSELIFE )
    {
        int invuln = plr_tim_amount(T_INVULN);
        if (invuln)
        {
            if (damage_type == DAMAGE_FORCE)
            {
                msg_print("The attack cuts your shield of invulnerability open!");
            }
            else if (one_in_(PENETRATE_INVULNERABILITY))
            {
                msg_print("The attack penetrates your shield of invulnerability!");
            }
            else if (damage < invuln)
            {
                plr_tim_subtract(T_INVULN, damage);
                return 0;
            }
            else
            {
                damage -= invuln;
                plr_tim_remove(T_INVULN);
            }
        }

        if (damage_type != DAMAGE_NOESCAPE && CHECK_MULTISHADOW())
        {
            if (damage_type == DAMAGE_FORCE)
            {
                msg_print("The attack hits Shadow together with you!");
            }
            else if (damage_type == DAMAGE_ATTACK)
            {
                msg_print("The attack hits Shadow, you are unharmed!");
                return 0;
            }
        }

        if (plr_tim_find(T_WRAITH))
        {
            if (damage_type == DAMAGE_FORCE)
            {
                msg_print("The attack cuts through your ethereal body!");
            }
            else
            {
                damage /= 2;
                if ((damage == 0) && one_in_(2)) damage = 1;
            }
        }

        if (plr->special_defense & KATA_MUSOU)
        {
            damage /= 2;
            if ((damage == 0) && one_in_(2)) damage = 1;
        }
    } /* not if LOSELIFE USELIFE */

    if (weaponmaster_get_toggle() == TOGGLE_BULWARK && damage_type == DAMAGE_ATTACK)
        damage -= (damage + 2)/3;

    /* Tera-Hack: Duelist Nemesis */
    if ( plr->pclass == CLASS_DUELIST
      && mon_process_current() == who_mon(plr->duelist_target)
      && plr->lev >= 45
      && damage > plr->chp )
    {
        mon_attack_ptr x = mon_attack_current();
        if (x && !x->mon2)
            x->stop = STOP_PLR_SPECIAL;
        damage = 0;
        msg_print("Nemesis!!!!  You cannot be slain by your current target!");
        plr_tim_add(T_STUN, STUN_KNOCKED_OUT - 1); /* XXX bypass resist GF_STUN */
        msg_format("%^s is no longer your current target.", duelist_current_challenge());
        plr->duelist_target = who_create_null();
        plr->redraw |= PR_STATUS;
    }
    
    /* Rage Mage: "Rage Fueled" */
    if ( plr->pclass == CLASS_RAGE_MAGE
      && (!hit_from || strcmp(hit_from, "Rage") != 0))
    {
        rage_mage_rage_fueled(damage);
    }

    if (plr->wizard && damage > 0)
        msg_format("You take %d damage.", damage);

    plr->chp -= damage;
    if(damage_type == DAMAGE_GENO && plr->chp < 0)
    {
        damage += plr->chp;
        plr->chp = 0;
    }

    fear_hurt_p(old_chp, plr->chp);

    if (plr->prace == RACE_MON_POSSESSOR)
        possessor_on_take_hit();

    /* Display the hitpoints */
    plr->redraw |= (PR_HP);

    /* This might slow things down a bit ... 
       But, Blood Knight power varies with hp. */
    if (plr->pclass == CLASS_BLOOD_KNIGHT)
        plr->update |= (PU_BONUS);

    if (weaponmaster_is_(WEAPONMASTER_STAVES))
        plr->update |= (PU_BONUS);

    handle_stuff();

    if (damage_type != DAMAGE_GENO && plr->chp == 0)
    {
        virtue_add(VIRTUE_SACRIFICE, 1);
        virtue_add(VIRTUE_CHANCE, 2);
    }

    /* Dead player */
    if (plr->chp < 0)
    {
        bool android = (plr->prace == RACE_ANDROID ? TRUE : FALSE);

        /* Sound */
        sound(SOUND_DEATH);

        virtue_add(VIRTUE_SACRIFICE, 10);

        /* Leaving */
        plr->leaving = TRUE;

        /* Note death */
        plr->is_dead = TRUE;

        {
            bool seppuku = streq(hit_from, "Seppuku");
            bool winning_seppuku = plr->total_winner && seppuku;

            /* Note cause of death */
            if (seppuku)
            {
                strcpy(plr->died_from, hit_from);
            }
            else
            {
                char dummy[1024];
                sprintf(dummy, "%s%s", hit_from, !plr_tim_find(T_PARALYZED) ? "" : " while helpless");
                my_strcpy(plr->died_from, dummy, sizeof plr->died_from);
            }

            msg_add_tiny_screenshot(50, 24);

            plr->total_winner = FALSE;
            flush();

            if (get_check_strict("Dump the screen? ", CHECK_NO_HISTORY))
            {
                do_cmd_save_screen();
            }

            flush();

            /* Initialize "last message" buffer */
            if (plr->last_message) z_string_free(plr->last_message);
            plr->last_message = NULL;

            /* Hack -- Note death */
            if (!last_words)
            {
                msg_print(android ? "You are broken." : "You die.");
                msg_print(NULL);
            }
            else
            {
                if (winning_seppuku)
                {
                    get_rnd_line("seppuku.txt", 0, death_message);
                }
                else
                {
                    get_rnd_line("death.txt", 0, death_message);
                }

                do
                {
                    while (!get_string("Last word: ", death_message, 1024)) ;
                }
                while (winning_seppuku && !get_check_strict("Are you sure? ", CHECK_NO_HISTORY));

                if (death_message[0] == '\0')
                {
                    strcpy(death_message, android ? "You are broken." : "You die.");
                }
                else plr->last_message = z_string_make(death_message);
                
                msg_print(death_message);
            }
        }

        /* Dead */
        return damage;
    }

    /* Hitpoint warning */
    if (plr->chp < warning && !world_monster)
    {
        sound(SOUND_WARN);

        /* Hack -- stop the player on first crossing the threshold */
        if (old_chp >= warning) 
        {
            msg_prompt("<color:v>*** LOW HITPOINT WARNING! ***</color> Press <color:y>Space</color> to continue.", " ", PROMPT_FORCE_CHOICE);
        }
        else
        {
            cmsg_print(TERM_VIOLET, "*Ouch!*");
            flush();
        }
    }
    return damage;
}


/*
 * Gain experience
 */
void gain_exp_64(s32b amount, u32b amount_frac)
{
    if (plr->is_dead) return;

    if (plr->prace == RACE_ANDROID) return;

    if ( (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC) 
      && !possessor_can_gain_exp() )
    {
        return;
    }

    /* Gain some experience */
    s64b_add(&(plr->exp), &(plr->exp_frac), amount, amount_frac);

    /* Slowly recover from experience drainage */
    if (plr->exp < plr->max_exp)
    {
        /* Gain max experience (20%) (was 10%) */
        plr->max_exp += amount / 5;
    }

    /* Check Experience ... later. Definitely not during melee attacks.
     * However, stat runs can check now ... otherwise, they gain CL1 exp
     * from all kills until the stat run is finished! */
    if (statistics_hack)
        check_experience();
    else
        plr->notice |= PN_EXP;
}


/*
 * Gain experience
 */
void gain_exp(s32b amount)
{
    gain_exp_64(amount, 0L);
}

/*
 * Lose experience
 */
void lose_exp(s32b amount)
{
    if (plr->prace == RACE_ANDROID) return;

    /* Never drop below zero experience */
    if (amount > plr->exp) amount = plr->exp;

    /* Lose some experience */
    plr->exp -= amount;

    /* Check Experience */
    check_experience();
}


/*
 * Drain experience
 * Return amount drained.
 */
int drain_exp(s32b drain, s32b slip, int hold_life_prob)
{
    int i;

    /* Androids use construction points, not experience points */
    if (plr->prace == RACE_ANDROID) return 0;

    for (i = 0; i < plr->hold_life; i++)
    {
        if (plr->hold_life && (randint0(100) < hold_life_prob))
        {
            msg_print("You keep hold of your life force!");
            return 0;
        }
    }

    if (plr->hold_life)
    {
        msg_print("You feel your life slipping away!");
        lose_exp(slip);
        return slip;
    }
    msg_print("You feel your life draining away!");
    lose_exp(drain);
    return drain;
}




/*
 * Choose a warrior-mage elemental attack. -LM-
 */
bool choose_ele_attack(void)
{
    int num;

    char choice;

    /* Save screen */
    screen_save();

    num = (plr->lev - 20) / 5;

              c_prt(TERM_RED,    "        a) Fire Brand", 2, 14);

    if (num >= 2) c_prt(TERM_L_WHITE,"        b) Cold Brand", 3, 14);
    else prt("", 3, 14);

    if (num >= 3) c_prt(TERM_GREEN,  "        c) Poison Brand", 4, 14);
    else prt("", 4, 14);

    if (num >= 4) c_prt(TERM_L_DARK, "        d) Acid Brand", 5, 14);
    else prt("", 5, 14);

    if (num >= 5) c_prt(TERM_BLUE,   "        e) Elec Brand", 6, 14);
    else prt("", 6, 14);

    prt("", 7, 14);
    prt("", 8, 14);
    prt("", 9, 14);

    prt("", 1, 0);
    prt("        Choose a temporary elemental brand ", 1, 14);

    choice = inkey();

    if ((choice == 'a') || (choice == 'A')) 
        plr_tim_add(T_BRAND_FIRE, plr->lev/2 + randint1(plr->lev/2));
    else if (((choice == 'b') || (choice == 'B')) && (num >= 2))
        plr_tim_add(T_BRAND_COLD, plr->lev/2 + randint1(plr->lev/2));
    else if (((choice == 'c') || (choice == 'C')) && (num >= 3))
        plr_tim_add(T_BRAND_POIS, plr->lev/2 + randint1(plr->lev/2));
    else if (((choice == 'd') || (choice == 'D')) && (num >= 4))
        plr_tim_add(T_BRAND_ACID, plr->lev/2 + randint1(plr->lev/2));
    else if (((choice == 'e') || (choice == 'E')) && (num >= 5))
        plr_tim_add(T_BRAND_ELEC, plr->lev/2 + randint1(plr->lev/2));
    else
    {
        msg_print("You cancel the temporary branding.");
        screen_load();
        return FALSE;
    }
    /* Load screen */
    screen_load();
    return TRUE;
}


/*
 * Choose a elemental immune. -LM-
 */
bool choose_ele_immune(int turn)
{
    char choice;

    /* Save screen */
    screen_save();

    c_prt(TERM_RED,    "        a) Immune Fire", 2, 14);

    c_prt(TERM_L_WHITE,"        b) Immune Cold", 3, 14);

    c_prt(TERM_L_DARK, "        c) Immune Acid", 4, 14);

    c_prt(TERM_BLUE,   "        d) Immune Elec", 5, 14);


    prt("", 6, 14);
    prt("", 7, 14);
    prt("", 8, 14);
    prt("", 9, 14);

    prt("", 1, 0);
    prt("        Choose a temporary elemental immune ", 1, 14);

    choice = inkey();

    if ((choice == 'a') || (choice == 'A')) 
        plr_tim_add(T_IM_FIRE, turn);
    else if ((choice == 'b') || (choice == 'B'))
        plr_tim_add(T_IM_COLD, turn);
    else if ((choice == 'c') || (choice == 'C'))
        plr_tim_add(T_IM_ACID, turn);
    else if ((choice == 'd') || (choice == 'D'))
        plr_tim_add(T_IM_ELEC, turn);
    else
    {
        msg_print("You cancel the temporary immune.");
        screen_load();
        return FALSE;
    }
    /* Load screen */
    screen_load();
    return TRUE;
}

