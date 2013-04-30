/*
 * File: xtra3.c
 * Purpose: Handles the setting up, updating, and cleaning up of the various
 *          things that are displayed by the game.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Antony Sidwell
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
#include "cmds.h"
#include "monster/mon-lore.h"
#include "monster/mon-spell.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "object/pval.h"
#include "s-spells.h"
#include "target.h"


/*** Sidebar display functions ***/


/*
 * Print character stat in given row, column
 */
static void prt_stat(struct player *p, int stat)
{
    Send_stat(p, stat, p->state.stat_top[stat], p->state.stat_use[stat], p->stat_max[stat],
        p->state.stat_add[stat], p->stat_cur[stat]);
}


/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(struct player *p)
{
    const char *title = get_title(p);

    /* Ghost */
    if (p->ghost) title = "Ghost";

    Send_title(p, title);
}


/*
 * Prints level
 */
static void prt_lvl(struct player *p)
{
    Send_lvl(p, p->lev, p->max_lev);
}     


/*
 * Display the experience
 */
static void prt_exp(struct player *p)
{
    Send_exp(p, p->max_exp, p->exp, p->expfact);
}


/*
 * Prints current gold
 */
static void prt_gold(struct player *p)
{
    Send_gold(p, p->au);
}


/*
 * Equippy chars
 */
static void prt_equippy(struct player *p)
{
    int i;
    byte a;
    char c;
    object_type *o_ptr;

    /* Dump equippy chars */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
    {
        /* Object */
        o_ptr = &p->inventory[i];

        if (o_ptr->kind)
        {
            /* Use ASCII symbol for distorted tiles */
            if (p->tile_distorted)
            {
                a = o_ptr->kind->d_attr;
                c = o_ptr->kind->d_char;

                if (o_ptr->kind->flavor && !(p->obj_aware[o_ptr->kind->kidx] && a && c))
                {
                    a = o_ptr->kind->flavor->d_attr;
                    c = o_ptr->kind->flavor->d_char;
                }
            }
            else
            {
                a = object_attr(p, o_ptr);
                c = object_char(p, o_ptr);
            }

            /* Multi-hued object */
            if (a == TERM_MULTI) a = TERM_VIOLET;
        }
        else
        {
            a = TERM_WHITE;
            c = ' ';
        }

        /* Dump proper character */
        p->hist_flags[0][i - INVEN_WIELD].a = a;
        p->hist_flags[0][i - INVEN_WIELD].c = c;
    }
}


/*
 * Prints current AC
 */
static void prt_ac(struct player *p)
{
    Send_ac(p, p->state.dis_ac, p->state.dis_to_a);
}


/*
 * Prints Cur hit points
 */
static void prt_hp(struct player *p)
{
    Send_hp(p, p->mhp, p->chp);
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(struct player *p)
{
    Send_sp(p, p->msp, p->csp);
}


/*
 * Redraw the "monster health bar"
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targeting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.  When nothing
 * is being tracked, we clear the health bar.  If the monster being
 * tracked is not currently visible, a special health bar is shown.
 */
static void prt_health(struct player *p)
{
    bool is_unseen, is_dead, is_afraid, is_confused, is_stunned, is_asleep;
    bool is_poisoned, is_bleeding;
    int pct;

    /* Not tracking */
    if (!p->health_who)
    {
        /* Erase the health bar */
        Send_monster_health(p, 0, 0);
        return;
    }

    /* Tracking a player */
    if (p->health_who < 0)
    {
        player_type *q_ptr;

        /* Make sure we have a valid index */
        if (0 - p->health_who > NumPlayers)
        {
            /* Invalid index -- erase the health bar */
            Send_monster_health(p, 0, 0);

            /* Reset the index */
            p->health_who = 0;
            return;
        }

        q_ptr = player_get(0 - p->health_who);

        /* Tracking a bad player (?) */
        if (!q_ptr)
        {
            /* Erase the health bar */
            Send_monster_health(p, 0, 0);
            return;
        }

        /* Extract various states */
        is_unseen = !p->play_vis[0 - p->health_who];
        is_dead = FALSE;
        is_afraid = (check_state(q_ptr, OF_AFRAID)? TRUE: FALSE);
        is_confused = (q_ptr->timed[TMD_CONFUSED]? TRUE: FALSE);
        is_stunned = (q_ptr->timed[TMD_PARALYZED]? TRUE: FALSE);
        is_asleep = (q_ptr->timed[TMD_PARALYZED]? TRUE: FALSE);
        is_poisoned = (q_ptr->timed[TMD_POISONED]? TRUE: FALSE);
        is_bleeding = (q_ptr->timed[TMD_CUT]? TRUE: FALSE);

        /* Extract the "percent" of health */
        pct = 100L * q_ptr->chp / q_ptr->mhp;
    }

    /* Tracking a monster */
    else
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), p->health_who);

        /* Tracking a bad monster (?) */
        if (!m_ptr->r_idx)
        {
            /* Erase the health bar */
            Send_monster_health(p, 0, 0);
            return;
        }

        /* Extract various states */
        is_unseen = !p->mon_vis[p->health_who];
        is_dead = ((m_ptr->hp < 0)? TRUE: FALSE);
        is_afraid = (m_ptr->m_timed[MON_TMD_FEAR]? TRUE: FALSE);
        is_confused = (m_ptr->m_timed[MON_TMD_CONF] || m_ptr->m_timed[MON_TMD_BLIND]);
        is_stunned = (m_ptr->m_timed[MON_TMD_STUN] || m_ptr->m_timed[MON_TMD_HOLD]);
        is_asleep = (m_ptr->m_timed[MON_TMD_SLEEP]? TRUE: FALSE);
        is_poisoned = (m_ptr->m_timed[MON_TMD_POIS]? TRUE: FALSE);
        is_bleeding = (m_ptr->m_timed[MON_TMD_CUT]? TRUE: FALSE);

        /* Extract the "percent" of health */
        pct = 100L * m_ptr->hp / m_ptr->maxhp;
    }

    /* Tracking an unseen monster */
    if (is_unseen)
    {
        /* Indicate that the monster health is "unknown" */
        Send_monster_health(p, 0, TERM_WHITE);
    }

    /* Tracking a hallucinatory monster */
    else if (p->timed[TMD_IMAGE])
    {
        /* Indicate that the monster health is "unknown" */
        Send_monster_health(p, 0, TERM_WHITE);
    }

    /* Tracking a dead monster (???) */
    else if (is_dead)
    {
        /* Indicate that the monster health is "unknown" */
        Send_monster_health(p, 0, TERM_WHITE);
    }

    /* Tracking a visible monster */
    else
    {
        int len;

        /* Default to almost dead */
        byte attr = TERM_RED;

        /* Badly wounded */
        if (pct >= 10) attr = TERM_L_RED;

        /* Wounded */
        if (pct >= 25) attr = TERM_ORANGE;

        /* Somewhat wounded */
        if (pct >= 60) attr = TERM_YELLOW;

        /* Healthy */
        if (pct >= 100) attr = TERM_L_GREEN;

        /* Afraid */
        if (is_afraid) attr = TERM_VIOLET;

        /* Poisoned */
        if (is_poisoned) attr = TERM_GREEN;

        /* Bleeding */
        if (is_bleeding) attr = TERM_L_UMBER;

        /* Confused */
        if (is_confused) attr = TERM_UMBER;

        /* Stunned */
        if (is_stunned) attr = TERM_L_BLUE;

        /* Asleep */
        if (is_asleep) attr = TERM_BLUE;

        /* Convert percent into "health" */
        len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

        /* Send the health */
        Send_monster_health(p, len, attr);
    }
}


/*
 * Prints the speed of a character.
 */
static void prt_speed(struct player *p)
{
    s16b speed = get_speed(p);

    Send_speed(p, speed);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(struct player *p)
{
    Send_depth(p, p->depth, p->max_depth);
}


/*** Status line display functions ***/


/*
 * Prints status of hunger
 */
static void prt_hunger(struct player *p)
{
    Send_food(p, p->food);
}


/*
 * Print all timed effects.
 */
static void prt_tmd(struct player *p)
{
    Send_status(p, p->timed);
}


/*
 * Prints all status line indicators
 */
static void prt_status(struct player *p)
{
    prt_hunger(p);
    prt_tmd(p);
}


/*
 * Prints all state effects
 */
static void prt_state(struct player *p)
{
    bool s, r, u;

    /* Searching */
    s = (p->searching? TRUE: FALSE);

    /* Resting */
    r = (p->resting? TRUE: FALSE);

    /* Unignoring */
    u = (p->unignoring? TRUE: FALSE);

    Send_state(p, s, r, u);

    /* Print recall status */
    Send_recall(p, p->word_recall);
}


/*
 * Prints trap detection status
 */
static void prt_dtrap(struct player *p)
{
    Send_dtrap(p, get_dtrap(p));
}


/*
 * Print whether a character is studying or not.
 */
static void prt_study(struct player *p)
{
    /* Does the player carry a book with a spell they can study? */
    p->can_study_book = player_can_study_book(p);

    Send_study(p, p->new_spells, p->can_study_book);
}


/*** Subwindow display functions ***/


/*
 * Display floor item in equipment sub-window
 */
static void prt_floor_item(struct player *p)
{
    int floor_list[MAX_FLOOR_STACK];
    int floor_num;

    /* Paranoia */
    if (!cave_get(p->depth)) return;

    floor_num = scan_floor(p, floor_list, N_ELEMENTS(floor_list), p->depth, p->py,
        p->px, 0x02);
    display_floor(p, floor_list, floor_num);
}


/*
 * Hack -- Fix spells
 */
static void fix_spell(struct player *p)
{
    int i;
    spell_flags flags;

    /* Ghosts get a different set */
    if (p->ghost && !player_can_undead(p))
    {
        show_ghost_spells(p);
        return;
    }

    /* Shapechangers get a different set */
    if (player_has(p, PF_MONSTER_SPELLS))
    {
        show_mimic_spells(p);
        return;
    }

    /* Hack -- Must be literate */
    if (!p->clazz->spell_book) return;

    flags.line_attr = TERM_WHITE;
    flags.flag = RSF_NONE;
    flags.dir_attr = 0;
    flags.proj_attr = 0;

    /* Wipe the spell array */
    Send_spell_info(p, 0, 0, "", &flags);

    /* Scan for appropriate books */
    for (i = 0; i < INVEN_WIELD; i++)
    {
        if (p->inventory[i].tval == p->clazz->spell_book)
            do_cmd_browse(p, i);
    }
}


/*
 * Hack -- display mini-map view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_map(struct player *p)
{
    display_map(p, TRUE);
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(struct player *p)
{
    int r_idx = p->monster_race_idx;

    /* Describe it */
    if (r_idx < 0) describe_player(p, 0 - r_idx);
    else describe_monster(p, r_idx);

    /* Notify player */
    notify_player_popup(p, "Monster Recall", NTERM_WIN_MONSTER, 0);
}


/*
 * Hack -- Display monsters in sub-windows
 */
static void fix_monlist(struct player *p)
{
    /* Display visible monsters */
    display_monlist(p, FALSE);

    /* Notify player */
    notify_player_popup(p, "Monster List", NTERM_WIN_MONLIST, 0);
}


/*
 * Hack -- Display objects in sub-windows
 */
static void fix_objlist(struct player *p)
{
    /* Display visible objects */
    display_itemlist(p, FALSE);

    /* Notify player */
    notify_player_popup(p, "Object List", NTERM_WIN_OBJLIST, 0);
}


/*** Update flag handler functions ***/


/*
 * Obtain innate "pval" player flags
 */
static void player_flags_extra(struct player *p, bitflag f[OF_SIZE])
{
    /* Ent: penalty to speed */
    if (player_has(p, PF_GIANT)) of_on(f, OF_SPEED);

    /* Unencumbered monks get nice abilities */
    if (monk_armor_ok(p, p->inventory))
    {
        /* Faster every 10 levels */
        if (p->lev >= 10) of_on(f, OF_SPEED);
    }

    /* Ghost */
    if (p->ghost) of_on(f, OF_INFRA);

    /* Rogues get speed bonus */
    if (player_has(p, PF_SPEED_BONUS) && (p->lev >= 5))
        of_on(f, OF_SPEED);

    /* Handle polymorphed players */
    if (p->r_idx)
    {
        monster_race *r_ptr = &r_info[p->r_idx];

        if (((r_ptr->speed - 110) / 2) != 0) of_on(f, OF_SPEED);
        if (rf_has(r_ptr->flags, RF_KILL_WALL)) of_on(f, OF_TUNNEL);
        if ((r_ptr->extra <= 100) || (r_ptr->extra > 150)) of_on(f, OF_STEALTH);
    }

    /* Elementalists get instant tunnel at level 50 */
    if (player_has(p, PF_ELEMENTAL_SPELLS) && (p->lev == 50))
        of_on(f, OF_TUNNEL);

    /* Monks get stealth bonus */
    if (player_has(p, PF_MARTIAL_ARTS) && !check_state(p, OF_AGGRAVATE))
        of_on(f, OF_STEALTH);

    /* If the race has innate infravision/digging, set the corresponding flag */
    if (p->race->infra) of_on(f, OF_INFRA);
    if (p->race->r_skills[SKILL_DIGGING] > 0) of_on(f, OF_TUNNEL);
}


/*
 * Obtain the "flags" for the player as if he was an item
 */
void player_flags(struct player *p, bitflag f[OF_SIZE], object_type *inven)
{
    /* Clear */
    of_wipe(f);

    /* Add racial flags */
    of_copy(f, p->race->flags);

    /* Some classes become immune to fear at a certain plevel */
    if (player_has(p, PF_BRAVERY_30) && (p->lev >= 30))
        of_on(f, OF_RES_FEAR);

    /* Ent */
    if (player_has(p, PF_GIANT))
    {
        if (p->lev >= 5) of_on(f, OF_SEE_INVIS);
        if (p->lev >= 10) of_on(f, OF_ESP_ANIMAL);
        if (p->lev >= 15) of_on(f, OF_ESP_ORC);
        if (p->lev >= 20) of_on(f, OF_ESP_TROLL);
        if (p->lev >= 25) of_on(f, OF_ESP_GIANT);
        if (p->lev >= 30) of_on(f, OF_ESP_DRAGON);
        if (p->lev >= 35) of_on(f, OF_ESP_DEMON);
        if (p->lev >= 40) of_on(f, OF_ESP_UNDEAD);
        if (p->lev >= 45) of_on(f, OF_ESP_EVIL);
        if (p->lev == PY_MAX_LEVEL) of_on(f, OF_ESP_RADIUS);
    }

    /* Thunderlord */
    if (player_has(p, PF_THUNDERLORD))
    {
        if (p->lev >= 5) of_on(f, OF_ESP_DRAGON);
        if (p->lev >= 10) of_on(f, OF_RES_FIRE);
        if (p->lev >= 15) of_on(f, OF_RES_COLD);
        if (p->lev >= 20) of_on(f, OF_RES_ACID);
        if (p->lev >= 25) of_on(f, OF_RES_ELEC);
    }

    /* Elementalist */
    if (player_has(p, PF_ELEMENTAL_SPELLS))
    {
        if (p->lev >= 10) of_on(f, OF_RES_FIRE);
        if (p->lev >= 15) of_on(f, OF_RES_COLD);
        if (p->lev >= 20) of_on(f, OF_RES_ACID);
        if (p->lev >= 25) of_on(f, OF_RES_ELEC);
    }

    /* Unencumbered monks get nice abilities */
    if (monk_armor_ok(p, inven))
    {
        /* Feather falling at level 10 */
        if (p->lev >= 10) of_on(f, OF_FEATHER);

        /* Fear resistance at level 15 */
        if (p->lev >= 15) of_on(f, OF_RES_FEAR);

        /* Confusion resistance at level 20 */
        if (p->lev >= 20) of_on(f, OF_RES_CONFU);

        /* Free action at level 25 */
        if (p->lev >= 25) of_on(f, OF_FREE_ACT);
    }

    /* Ghost */
    if (p->ghost)
    {
        of_on(f, OF_SEE_INVIS);
        of_on(f, OF_RES_NETHR);
        of_on(f, OF_HOLD_LIFE);
        of_on(f, OF_FREE_ACT);
        of_on(f, OF_RES_FEAR);

        /* PWMAngband */
        of_on(f, OF_RES_DARK);
        of_on(f, OF_RES_BLIND);
        of_on(f, OF_RES_POIS);
        of_on(f, OF_RES_COLD);
        of_on(f, OF_RES_CONFU);
        of_on(f, OF_RES_SHARD);
        of_on(f, OF_RES_STUN);
        of_on(f, OF_RES_TIME);
        of_on(f, OF_FEATHER);
        of_on(f, OF_SUST_STR);
        of_on(f, OF_SUST_INT);
        of_on(f, OF_SUST_WIS);
        of_on(f, OF_SUST_DEX);
        of_on(f, OF_SUST_CON);
        of_on(f, OF_SUST_CHR);
    }

    /* Handle polymorphed players */
    if (p->r_idx)
    {
        monster_race *r_ptr = &r_info[p->r_idx];
        int m;

        for (m = 0; m < MONSTER_BLOW_MAX; m++)
        {
            /* Skip non-attacks */
            if (!r_ptr->blow[m].method) continue;

            switch (r_ptr->blow[m].effect)
            {
                case RBE_EXP_10:
                case RBE_EXP_20:
                case RBE_EXP_40:
                case RBE_EXP_80: of_on(f, OF_HOLD_LIFE); break;
            }
        }

        if (rf_has(r_ptr->flags, RF_REGENERATE)) of_on(f, OF_REGEN);
        if (rf_has(r_ptr->flags, RF_HURT_ROCK)) of_on(f, OF_RES_SHARD);
        /*if (rf_has(r_ptr->flags, RF_HURT_FIRE)) of_on(f, OF_VULN_FIRE);*/
        /*if (rf_has(r_ptr->flags, RF_HURT_COLD)) of_on(f, OF_VULN_COLD);*/
        if (rf_has(r_ptr->flags, RF_IM_ACID)) of_on(f, OF_RES_ACID);
        if (rf_has(r_ptr->flags, RF_IM_ELEC)) of_on(f, OF_RES_ELEC);
        if (rf_has(r_ptr->flags, RF_IM_FIRE)) of_on(f, OF_RES_FIRE);
        if (rf_has(r_ptr->flags, RF_IM_COLD)) of_on(f, OF_RES_COLD);
        if (rf_has(r_ptr->flags, RF_IM_POIS)) of_on(f, OF_RES_POIS);
        if (rf_has(r_ptr->flags, RF_RES_NETH))
        {
            of_on(f, OF_RES_NETHR);
            of_on(f, OF_HOLD_LIFE);
        }
        if (rf_has(r_ptr->flags, RF_IM_WATER))
        {
            of_on(f, OF_RES_CONFU);
            of_on(f, OF_RES_STUN);
        }
        if (rf_has(r_ptr->flags, RF_RES_PLAS)) of_on(f, OF_RES_STUN);
        if (rf_has(r_ptr->flags, RF_RES_NEXUS)) of_on(f, OF_RES_NEXUS);
        if (rf_has(r_ptr->flags, RF_RES_DISE)) of_on(f, OF_RES_DISEN);
        if (rf_has(r_ptr->flags, RF_NO_FEAR)) of_on(f, OF_RES_FEAR);
        if (rf_has(r_ptr->flags, RF_NO_STUN)) of_on(f, OF_RES_STUN);
        if (rf_has(r_ptr->flags, RF_NO_CONF)) of_on(f, OF_RES_CONFU);
        if (rf_has(r_ptr->flags, RF_NO_SLEEP)) of_on(f, OF_FREE_ACT);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_ACID)) of_on(f, OF_RES_ACID);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_ELEC)) of_on(f, OF_RES_ELEC);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_FIRE)) of_on(f, OF_RES_FIRE);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_COLD)) of_on(f, OF_RES_COLD);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_POIS)) of_on(f, OF_RES_POIS);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_NETH))
        {
            of_on(f, OF_RES_NETHR);
            of_on(f, OF_HOLD_LIFE);
        }
        if (rsf_has(r_ptr->spell_flags, RSF_BR_LIGHT))
        {
            of_on(f, OF_RES_LIGHT);
            of_on(f, OF_RES_BLIND);
        }
        if (rsf_has(r_ptr->spell_flags, RSF_BR_DARK))
        {
            of_on(f, OF_RES_DARK);
            of_on(f, OF_RES_BLIND);
        }
        if (rsf_has(r_ptr->spell_flags, RSF_BR_SOUN))
        {
            of_on(f, OF_RES_SOUND);
            of_on(f, OF_RES_STUN);
        }
        if (rsf_has(r_ptr->spell_flags, RSF_BR_CHAO))
        {
            of_on(f, OF_RES_CHAOS);
            of_on(f, OF_RES_CONFU);
        }
        if (rsf_has(r_ptr->spell_flags, RSF_BR_DISE)) of_on(f, OF_RES_DISEN);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_NEXU)) of_on(f, OF_RES_NEXUS);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_TIME)) of_on(f, OF_RES_TIME);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_INER)) of_on(f, OF_FREE_ACT);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_GRAV))
        {
            of_on(f, OF_FEATHER);
            of_on(f, OF_RES_STUN);
        }
        if (rsf_has(r_ptr->spell_flags, RSF_BR_SHAR)) of_on(f, OF_RES_SHARD);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_PLAS)) of_on(f, OF_RES_STUN);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_WALL)) of_on(f, OF_RES_STUN);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_MANA)) of_on(f, OF_RES_MANA);
        if (rsf_has(r_ptr->spell_flags, RSF_BR_WATE))
        {
            of_on(f, OF_RES_CONFU);
            of_on(f, OF_RES_STUN);
        }
    }
}


/*** Generic "deal with" functions ***/


/*
 * Redraw the cursor
 *
 * This function must simply calculate correct offset for each player
 * and update his cursor location
 */
static void cursor_redraw(struct player *p)
{
    int vis, x, y = 0;

    /* Not tracking */
    if (!p->cursor_who)
    {
        /* Reset the cursor */
        vis = 0;
    }

    /* Tracking a player */
    else if (p->cursor_who < 0)
    {
        player_type *q_ptr;

        /* Make sure we have a valid index */
        if (0 - p->cursor_who > NumPlayers)
        {
            /* Invalid index -- Reset the cursor */
            Send_cursor(p, 0, 0, 0);

            /* Reset the index */
            p->cursor_who = 0;
            return;
        }

        q_ptr = player_get(0 - p->cursor_who);

        /* Tracking a bad player (?) */
        if (!q_ptr)
        {
            /* Reset the cursor */
            vis = 0;
        }

        /* Tracking an unseen player */
        else if (!p->play_vis[0 - p->cursor_who])
        {
            /* Should not be possible */
            vis = 0;
        }

        /* Tracking a visible player */
        else
        {
            vis = 1;
            x = q_ptr->px - p->offset_x;
            y = q_ptr->py - p->offset_y + 1;

            /* Hack -- Is there something targetable above our position? */
            if (in_bounds_fully(q_ptr->py - 1, q_ptr->px) &&
                target_set_interactive_accept(p, q_ptr->py - 1, q_ptr->px))
            {
                vis = 2;
            }
        }
    }

    /* Tracking a bad monster (?) */
    else if (!cave_monster(cave_get(p->depth), p->cursor_who)->r_idx)
    {
        /* Reset the cursor */
        vis = 0;
    }

    /* Tracking an unseen monster */
    else if (!p->mon_vis[p->cursor_who])
    {
        /* Reset cursor */
        vis = 0;
    }

    /* Tracking a dead monster (???) */
    else if (cave_monster(cave_get(p->depth), p->cursor_who)->hp < 0)
    {
        /* Reset cursor */
        vis = 0;
    }

    /* Tracking a visible monster */
    else
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), p->cursor_who);

        vis = 1;
        x = m_ptr->fx - p->offset_x;
        y = m_ptr->fy - p->offset_y + 1;

        /* Hack -- Is there something targetable above our position? */
        if (in_bounds_fully(m_ptr->fy - 1, m_ptr->fx) &&
            target_set_interactive_accept(p, m_ptr->fy - 1, m_ptr->fx))
        {
            vis = 2;
        }
    }

    if (vis)
        Send_cursor(p, vis, x, y);
    else
    {
        Send_cursor(p, 0, 0, 0);

        /* Cancel tracking */
        p->cursor_who = 0;
    }

    /* Redraw targeting path */
    if (p->path_drawn)
    {
        /* Remove old path */
        load_path(p, p->path_n, p->path_g);

        /* Redraw new targeting path */
        if (vis)
        {
            p->path_n = project_path(p->path_g, MAX_RANGE, p->depth, p->py, p->px,
                y + p->offset_y - 1, x + p->offset_x, PROJECT_THRU);
            p->path_drawn = draw_path(p, p->path_n, p->path_g, p->py, p->px);
        }
    }
}


static void prt_player_sust_info(struct player *p)
{
    int i, j, stat;
    object_type *o_ptr;
    bitflag f[OF_SIZE];
    int stat_flags[A_MAX];
    int sustain_flags[A_MAX];
    byte a;
    char c;
    int boost;
    bool aware;

    /* Build the stat flags tables */
    stat_flags[A_STR] = OF_STR;
    stat_flags[A_INT] = OF_INT;
    stat_flags[A_WIS] = OF_WIS;
    stat_flags[A_DEX] = OF_DEX;
    stat_flags[A_CON] = OF_CON;
    stat_flags[A_CHR] = OF_CHR;
    sustain_flags[A_STR] = OF_SUST_STR;
    sustain_flags[A_INT] = OF_SUST_INT;
    sustain_flags[A_WIS] = OF_SUST_WIS;
    sustain_flags[A_DEX] = OF_SUST_DEX;
    sustain_flags[A_CON] = OF_SUST_CON;
    sustain_flags[A_CHR] = OF_SUST_CHR;

    /* Process equipment */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
    {
        /* Get the object */
        o_ptr = &p->inventory[i];

        /* Get the "known" flags */
        aware = (o_ptr->kind? object_flavor_is_aware(p, o_ptr): FALSE);
        object_flags_known(o_ptr, f, aware);

        /* Initialize color based of sign of pval */
        for (stat = 0; stat < A_MAX; stat++)
        {
            /* Default */
            a = TERM_SLATE;
            c = '.';

            /* Hack -- Precalculate boost */
            boost = 0;
            if (of_has(f, stat_flags[stat]))
            {
                /* Work out which pval we're talking about */
                j = which_pval(o_ptr, stat_flags[stat]);

                boost = o_ptr->pval[j];
            }

            /* Boost */
            if (boost)
            {
                /* Default */
                c = '*';

                /* Good */
                if (boost > 0)
                {
                    /* Good */
                    a = TERM_L_GREEN;

                    /* Label boost */
                    if (boost < 10) c = I2D(boost);
                }

                /* Bad */
                if (boost < 0)
                {
                    /* Bad */
                    a = TERM_RED;

                    /* Label boost */
                    if (boost > -10) c = I2D(-(boost));
                }
            }

            /* Sustain */
            if (of_has(f, sustain_flags[stat]))
            {
                /* Dark green */
                a = TERM_GREEN;

                /* Convert '.' to 's' */
                if (c == '.') c = 's';
            }

            if ((c == '.') && o_ptr->kind &&
                !object_flag_is_known(p, o_ptr, sustain_flags[stat]))
                    c = '?';

            /* Dump proper character */
            p->hist_flags[stat + 1][i - INVEN_WIELD].a = a;
            p->hist_flags[stat + 1][i - INVEN_WIELD].c = c;
        }
    }

    /* Player flags */
    player_flags(p, f, p->inventory);

    /* Check stats */
    for (stat = 0; stat < A_MAX; ++stat)
    {
        /* Default */
        a = TERM_SLATE;
        c = '.';

        /* Sustain */
        if (of_has(f, sustain_flags[stat]))
        {
            /* Dark green "s" */
            a = TERM_GREEN;
            c = 's';
        }

        /* Dump */
        p->hist_flags[stat + 1][RES_COLS - 1].a = a;
        p->hist_flags[stat + 1][RES_COLS - 1].c = c;
    }

    /* Handle polymorphed players */
    if (p->r_idx)
    {
        monster_race *r_ptr = &r_info[p->r_idx];

        /* Default */
        a = p->hist_flags[A_INT + 1][RES_COLS - 1].a;
        c = p->hist_flags[A_INT + 1][RES_COLS - 1].c;
        boost = 0;

        if (rf_has(r_ptr->flags, RF_STUPID)) boost -= 2;
        if (rf_has(r_ptr->flags, RF_SMART)) boost += 2;
        if (r_ptr->freq_spell == 33) boost += 1;
        if (r_ptr->freq_spell == 50) boost += 3;
        if (r_ptr->freq_spell == 100) boost += 5;

        /* Boost */
        if (boost)
        {
            /* Default */
            c = '*';

            /* Good */
            if (boost)
            {
                /* Good */
                if (a == TERM_SLATE) a = TERM_L_GREEN;

                /* Label boost */
                if (boost < 10) c = I2D(boost);
            }

            /* Bad */
            if (boost < 0)
            {
                /* Bad */
                if (a == TERM_SLATE) a = TERM_RED;

                /* Label boost */
                if (boost > -10) c = I2D(-(boost));
            }
        }

        /* Dump */
        p->hist_flags[A_INT + 1][RES_COLS - 1].a = a;
        p->hist_flags[A_INT + 1][RES_COLS - 1].c = c;
    }
}


/*  
 * List of resistances and abilities to display
 */
typedef struct
{
    int res_flag;  /* Resistance flag bit */
    int im_flag;   /* Corresponding immunity bit, if any */
    int vuln_flag; /* Corresponding vulnerability flag, if any */
} player_flag_record;


static const player_flag_record player_flag_table[RES_PANELS * RES_ROWS] =
{
    {OF_RES_ACID, OF_IM_ACID, OF_VULN_ACID},
    {OF_RES_ELEC, OF_IM_ELEC, OF_VULN_ELEC},
    {OF_RES_FIRE, OF_IM_FIRE, OF_VULN_FIRE},
    {OF_RES_COLD, OF_IM_COLD, OF_VULN_COLD},
    {OF_RES_POIS, FLAG_END, FLAG_END},
    {OF_RES_LIGHT, FLAG_END, FLAG_END},
    {OF_RES_DARK, FLAG_END, FLAG_END},
    {OF_RES_SOUND, FLAG_END, FLAG_END},
    {OF_RES_SHARD, FLAG_END, FLAG_END},

    {OF_RES_NEXUS, FLAG_END, FLAG_END},
    {OF_RES_NETHR, FLAG_END, FLAG_END},
    {OF_RES_CHAOS, FLAG_END, FLAG_END},
    {OF_RES_DISEN, FLAG_END, FLAG_END},
    {OF_FEATHER, FLAG_END, FLAG_END},
    {OF_RES_FEAR, FLAG_END, FLAG_END},
    {OF_RES_BLIND, FLAG_END, FLAG_END},
    {OF_RES_CONFU, FLAG_END, FLAG_END},
    {OF_RES_STUN, FLAG_END, FLAG_END},

    {OF_LIGHT, FLAG_END, FLAG_END},
    {OF_REGEN, FLAG_END, FLAG_END},
    {OF_ESP_POWER, FLAG_END, FLAG_END},
    {OF_SEE_INVIS, FLAG_END, FLAG_END},
    {OF_FREE_ACT, FLAG_END, FLAG_END},
    {OF_HOLD_LIFE, FLAG_END, FLAG_END},
    {OF_STEALTH, FLAG_END, FLAG_END},
    {OF_SEARCH, FLAG_END, FLAG_END},
    {OF_INFRA, FLAG_END, FLAG_END},

    {OF_TUNNEL, FLAG_END, FLAG_END},
    {OF_SPEED, FLAG_END, FLAG_END},
    {OF_BLOWS, FLAG_END, FLAG_END},
    {OF_SHOTS, FLAG_END, FLAG_END},
    {OF_MIGHT, FLAG_END, FLAG_END},
    {OF_SLOW_DIGEST, FLAG_END, FLAG_END},
    {OF_IMPAIR_HP, FLAG_END, FLAG_END},
    {OF_AFRAID, FLAG_END, FLAG_END},
    {OF_AGGRAVATE, FLAG_END, FLAG_END}
};


static void prt_resistance_panel(struct player *p, int which, const player_flag_record *resists)
{
    size_t i, j;
    int off = 1 + A_MAX + RES_ROWS * which;

    for (i = 0; i < RES_ROWS; i++)
    {
        /* Repeated extraction of flags is inefficient but more natural */
        for (j = INVEN_WIELD; j <= INVEN_TOTAL; j++)
        {
            object_type *o_ptr = NULL;
            bitflag f[OF_SIZE];
            byte attr = (TERM_WHITE | (j % 2) * 8); /* Alternating columns */
            char sym = '.';
            bool res, imm, vuln;

            /* Wipe flagset */
            of_wipe(f);

            if (j < INVEN_TOTAL)
            {
                /* Object */
                o_ptr = &p->inventory[j];

                /* Fill in Known flags */
                if (o_ptr->kind)
                    object_flags_known(o_ptr, f, object_flavor_is_aware(p, o_ptr));
            }
            else
            {
                /* Player flags */
                player_flags(p, f, p->inventory);
                player_flags_extra(p, f);
            }

            res = of_has(f, resists[i].res_flag);
            imm = of_has(f, resists[i].im_flag);
            vuln = of_has(f, resists[i].vuln_flag);

            if (imm) sym = '*';
            else if (res && !vuln) sym = '+';
            else if (vuln && !res) sym = '-';
            else if (o_ptr && o_ptr->kind && !object_flag_is_known(p, o_ptr, resists[i].res_flag))
                sym = '?';

            /* Special case: ESP flags */
            if (resists[i].res_flag == OF_ESP_POWER)
            {
                bitflag f_esp[OF_SIZE], f2[OF_SIZE];

                of_copy(f_esp, f);
                create_mask(f2, FALSE, OFT_ESP, OFT_MAX);
                of_inter(f_esp, f2);

                sym = '.';
                if (of_has(f_esp, OF_ESP_ALL)) sym = '+';
                else if (of_has(f_esp, OF_ESP_RADIUS)) sym = 'R';
                else if (of_has(f_esp, OF_ESP_EVIL)) sym = 'E';
                else if (of_has_unique(f_esp, OF_ESP_ANIMAL)) sym = 'N';
                else if (of_has_unique(f_esp, OF_ESP_UNDEAD)) sym = 'G';
                else if (of_has_unique(f_esp, OF_ESP_DEMON)) sym = 'U';
                else if (of_has_unique(f_esp, OF_ESP_ORC)) sym = 'o';
                else if (of_has_unique(f_esp, OF_ESP_TROLL)) sym = 'T';
                else if (of_has_unique(f_esp, OF_ESP_GIANT)) sym = 'P';
                else if (of_has_unique(f_esp, OF_ESP_DRAGON)) sym = 'D';
                else if (!of_is_empty(f_esp)) sym = 'M'; /* Mixed */
                else if (o_ptr && o_ptr->kind && !object_flag_is_known(p, o_ptr, resists[i].res_flag))
                    sym = '?';
            }

            p->hist_flags[off + i][j - INVEN_WIELD].a = attr;
            p->hist_flags[off + i][j - INVEN_WIELD].c = sym;
        }
    }
}


static void prt_player_flag_info(struct player *p)
{
    int i;

    for (i = 0; i < RES_PANELS; i++)
        prt_resistance_panel(p, i, player_flag_table + i * RES_ROWS);
}


static void prt_flags(struct player *p)
{
    int i;

    prt_equippy(p);
    prt_player_sust_info(p);
    prt_player_flag_info(p);

    for (i = 0; i < N_HISTORY_FLAGS; i++) Send_objflags(p, i);
}


static void prt_plusses(struct player *p)
{
    int show_tofhit, show_tomhit, show_toshit;
    int show_tofdam, show_tomdam, show_tosdam;

    get_plusses(p, &show_tofhit, &show_tofdam, &show_tomhit, &show_tomdam,
        &show_toshit, &show_tosdam);

    Send_plusses(p, show_tofhit, show_tofdam, show_tomhit, show_tomdam,
        show_toshit, show_tosdam);
}


static void prt_misc(struct player *p)
{
    Send_char_info(p, p->race->ridx, p->clazz->cidx, p->psex);
}


static void prt_stats(struct player *p)
{
    int i;

    for (i = 0; i < A_MAX; i++) prt_stat(p, i);
}


static void prt_minimap(struct player *p)
{
    prt_map(p);
    if (p->window_flag & PW_MAP) fix_map(p);
}


static void prt_equip(struct player *p)
{
    display_equip(p);
    prt_flags(p);
    update_prevent_inscriptions(p);
}


static void prt_monster(struct player *p)
{
    if (p->window_flag & PW_MONSTER) fix_monster(p);
}


static void prt_monsterlist(struct player *p)
{
    /* Only if full refresh */
    if (!p->full_refresh) return;

    if (p->window_flag & PW_MONLIST) fix_monlist(p);
    p->redraw &= ~(PR_MONLIST);
}


static void prt_objectlist(struct player *p)
{
    /* Only if full refresh */
    if (!p->full_refresh) return;

    if (p->window_flag & PW_ITEMLIST) fix_objlist(p);
    p->redraw &= ~(PR_ITEMLIST);
}


typedef struct
{
    u32b flag;
    void (*hook)(struct player *);
    bool remove_flag;
} flag_event_trigger;


/*
 * Events triggered by the various flags.
 */
static const flag_event_trigger redraw_events[] =
{
    {PR_MISC, prt_misc, TRUE},
    {PR_TITLE, prt_title, TRUE},
    {PR_LEV, prt_lvl, TRUE},
    {PR_EXP, prt_exp, TRUE},
    {PR_STATS, prt_stats, TRUE},
    {PR_ARMOR, prt_ac, TRUE},
    {PR_HP, prt_hp, TRUE},
    {PR_MANA, prt_sp, TRUE},
    {PR_GOLD, prt_gold, TRUE},
    {PR_ITEMLIST, prt_objectlist, FALSE},
    {PR_HEALTH, prt_health, TRUE},
    {PR_SPEED, prt_speed, TRUE},
    {PR_STUDY, prt_study, TRUE},
    {PR_DEPTH, prt_depth, TRUE},
    {PR_STATUS, prt_status, TRUE},
    {PR_DTRAP, prt_dtrap, TRUE},
    {PR_STATE, prt_state, TRUE},
    {PR_MAP, prt_minimap, TRUE},
    {PR_INVEN, display_inven, TRUE},
    {PR_EQUIP, prt_equip, TRUE},
    {PR_MONSTER, prt_monster, TRUE},
    {PR_MONLIST, prt_monsterlist, FALSE},
    {PR_SPELL, fix_spell, TRUE},
    {PR_PLUSSES, prt_plusses, TRUE},
    {PR_CURSOR, cursor_redraw, TRUE},
    {PR_FLOOR, prt_floor_item, TRUE}
};


/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(struct player *p)
{
    size_t i;

    /* Nothing to do */
    if (!p->redraw) return;

    /* Character is not ready yet, no screen updates */
    if (!p->alive) return;

    /* For each listed flag, give an appropriate response */
    for (i = 0; i < N_ELEMENTS(redraw_events); i++)
    {
        const flag_event_trigger *hnd = &redraw_events[i];
        if (p->redraw & hnd->flag)
        {
            if (hnd->remove_flag)
                p->redraw &= ~(hnd->flag);
            hnd->hook(p);
        }
    }
}
