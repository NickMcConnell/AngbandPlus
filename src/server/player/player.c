/*
 * File: player.c
 * Purpose: Player implementation
 *
 * Copyright (c) 2011 elly+angband@leptoquark.net. See COPYING.
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
#include "../history.h"
#include "../s-spells.h"


/*
 * Increases a stat
 */
bool player_stat_inc(struct player *p, int stat)
{
    int v = p->stat_cur[stat];

    /* Cannot go above 18/100 */
    if (v >= 18+100) return FALSE;

    /* Increase linearly */
    if (v < 18) p->stat_cur[stat]++;
    else if (v < 18+90)
    {
        int gain = (((18+100) - v) / 2 + 3) / 2;

        /* Paranoia */
        if (gain < 1) gain = 1;

        /* Apply the bonus */
        p->stat_cur[stat] += randint1(gain) + gain / 2;

        /* Maximal value */
        if (p->stat_cur[stat] > 18+99) p->stat_cur[stat] = 18+99;
    }
    else p->stat_cur[stat] = 18+100;

    /* Bring up the maximum too */
    if (p->stat_cur[stat] > p->stat_max[stat]) p->stat_max[stat] = p->stat_cur[stat];

    /* Recalculate bonuses */
    p->update |= (PU_BONUS);

    return TRUE;
}


/*
 * Decreases a stat
 */
bool player_stat_dec(struct player *p, int stat, bool permanent)
{
    int cur, max, res;

    cur = p->stat_cur[stat];
    max = p->stat_max[stat];

    /* Damage "current" value */
    if (cur > 18+10) cur -= 10;
    else if (cur > 18) cur = 18;
    else if (cur > 3) cur -= 1;

    res = (cur != p->stat_cur[stat]);

    /* Damage "max" value */
    if (permanent)
    {
        if (max > 18+10) max -= 10;
        else if (max > 18) max = 18;
        else if (max > 3) max -= 1;

        res = (max != p->stat_max[stat]);
    }

    /* Apply changes */
    if (res)
    {
        p->stat_cur[stat] = cur;
        p->stat_max[stat] = max;
        p->update |= (PU_BONUS);
        p->redraw |= (PR_STATS);
    }

    return res;
}


/*
 * Advance experience levels and print experience
 */
static void adjust_level(struct player *p)
{
    char buf[NORMAL_WID];
    bool redraw = FALSE;

    /* Hack -- lower limit */
    if (p->exp < 0) p->exp = 0;

    /* Hack -- lower limit */
    if (p->max_exp < 0) p->max_exp = 0;

    /* Hack -- upper limit */
    if (p->exp > PY_MAX_EXP) p->exp = PY_MAX_EXP;

    /* Hack -- upper limit */
    if (p->max_exp > PY_MAX_EXP) p->max_exp = PY_MAX_EXP;

    /* Hack -- maintain "max" experience */
    if (p->exp > p->max_exp) p->max_exp = p->exp;

    /* Redraw experience */
    p->redraw |= (PR_EXP);

    /* Handle stuff */
    handle_stuff(p);

    /* Lose levels while possible */
    while ((p->lev > 1) && (p->exp < adv_exp(p->lev - 1, p->expfact)))
    {
        /* Lose a level */
        p->lev--;

        /* Dragon */
        if (player_has(p, PF_DRAGON)) poly_dragon(p, TRUE);

        /* Redraw */
        redraw = TRUE;
    }

    /* Gain levels while possible */
    while ((p->lev < PY_MAX_LEVEL) && (p->exp >= adv_exp(p->lev, p->expfact)))
    {
        /* Gain a level */
        p->lev++;

        /* Dragon */
        if (player_has(p, PF_DRAGON)) poly_dragon(p, TRUE);

        /* Save the highest level */
        if (p->lev > p->max_lev)
        {
            p->max_lev = p->lev;

            /* Message */
            msgt(p, MSG_LEVEL, "Welcome to level %d.", p->lev);
            strnfmt(buf, sizeof(buf), "%s has attained level %d.", p->name, p->lev);
            msg_broadcast(p, buf);

            /* Add to social class */
            p->sc += randint1(2);
            if (p->sc > 150) p->sc = 150;

            /* Restore stats */
            do_res_stat(p, A_STR);
            do_res_stat(p, A_INT);
            do_res_stat(p, A_WIS);
            do_res_stat(p, A_DEX);
            do_res_stat(p, A_CON);
            do_res_stat(p, A_CHR);

            /* Record this event in the character history */
            if (!(p->lev % 5))
            {
                strnfmt(buf, sizeof(buf), "Reached level %d", p->lev);
                history_add_unique(p, buf, HISTORY_GAIN_LEVEL);
            }
        }

        /* Redraw */
        redraw = TRUE;
    }

    /* Redraw - Do it only once to avoid socket buffer overflow */
    if (redraw)
    {
        /* Update some stuff */
        p->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

        /* Redraw some stuff */
        p->redraw |= (PR_LEV | PR_TITLE | PR_EXP | PR_STATS | PR_EQUIP | PR_SPELL);
    }

    /* Handle stuff */
    handle_stuff(p);
}


/*
 * Gain experience
 */
void player_exp_gain(struct player *p, s32b amount)
{
    /* Gain some experience */
    p->exp += amount;

    /* Slowly recover from experience drainage */
    if (p->exp < p->max_exp)
    {
        /* Gain max experience (10%) */
        p->max_exp += amount / 10;
    }

    /* Adjust experience levels */
    adjust_level(p);
}


/*
 * Lose experience
 */
void player_exp_lose(struct player *p, s32b amount, bool permanent)
{
    /* Never drop below zero experience */
    if (amount > p->exp) amount = p->exp;

    /* Lose some experience */
    p->exp -= amount;
    if (permanent) p->max_exp -= amount;

    /* Adjust experience levels */
    adjust_level(p);
}


/*
 * An array for player structures
 *
 * Player index is in [1..NumPlayers]
 */
static player_type **Players;


void init_players(void)
{
    Players = C_ZNEW(MAX_PLAYERS, player_type*);
}


void free_players(void)
{
    mem_free(Players);
}


struct player *player_get(int Ind)
{
    return (((Ind > 0) && (Ind < MAX_PLAYERS))? Players[Ind]: NULL);
}


void player_set(int Ind, struct player *p)
{
    if ((Ind > 0) && (Ind < MAX_PLAYERS)) Players[Ind] = p;
}


/*
 * Record the original (pre-ghost) cause of death
 */
void player_death_info(struct player *p, const char *died_from)
{
    my_strcpy(p->death_info.title, get_title(p), sizeof(p->death_info.title));
    p->death_info.max_lev = p->max_lev;
    p->death_info.lev = p->lev;
    p->death_info.max_exp = p->max_exp;
    p->death_info.exp = p->exp;
    p->death_info.au = p->au;
    p->death_info.max_depth = p->max_depth;
    p->death_info.depth = p->depth;
    my_strcpy(p->death_info.died_from, died_from, sizeof(p->death_info.died_from));
    time(&p->death_info.time);
    my_strcpy(p->death_info.ctime, ctime(&p->death_info.time), sizeof(p->death_info.ctime));
}
