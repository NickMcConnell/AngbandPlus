/* File: melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Monster spells and movement */

/*
* This file has several additions to it by Keldon Jones (keldon@umr.edu)
* to improve the general quality of the AI (version 0.1.1).
*/

#include "angband.h"
#include <assert.h>

/*
 * Hack, based on mon_take_hit... perhaps all monster attacks on
 * other monsters should use this?
 */
void mon_take_hit_mon(mon_ptr mon, int dam, bool *fear, cptr note, mon_ptr who)
{
    char m_name[160];
    bool seen = mon_show_msg(mon);
    bool known = (mon->cdis <= MAX_SIGHT);

    monster_desc(m_name, mon, 0);

    if (mon->ml)
        check_mon_health_redraw(mon);

    mon_tim_delete(mon, MT_SLEEP);

    if (mon->id == plr->riding)
        disturb(1, 0);

    if (mon_tim_find(mon, T_INVULN) && randint0(PENETRATE_INVULNERABILITY))
    {
        if (seen)
            msg_format("%^s is unharmed.", m_name);
        return;
    }

    /* Hurt it */
    mon->hp -= dam;

    /* It is dead now... or is it? */
    if (mon->hp < 0)
    {
        if ( (mon_race_is_unique(mon->race) || mon_race_is_nazgul(mon->race) || (mon->mflag2 & MFLAG2_QUESTOR))
          && !prace_is_(RACE_MON_QUYLTHULG))
        {
            mon->hp = 1;
        }
        else
        {
            if (known)
            {
                monster_desc(m_name, mon, MD_TRUE_NAME);
                /* Unseen death by normal attack */
                if (!seen)
                {
                    mon_fight = TRUE;
                }
                /* Death by special attack */
                else if (note)
                {
                    msg_format("%^s%s", m_name, note);
                }
                /* Death by normal attack -- nonliving monster */
                else if (!mon_is_living(mon))
                {
                    msg_format("%^s is destroyed.", m_name);
                }
                /* Death by normal attack -- living monster */
                else
                {
                    msg_format("%^s is killed.", m_name);
                }
            }

            monster_gain_exp(who, mon->race);

            mon_check_kill_unique(mon);
            monster_death(mon, mon_is_pet(who));
            delete_monster(mon);

            (*fear) = FALSE;
            return;
        }
    }

#ifdef ALLOW_FEAR

    /* Mega-Hack -- Pain cancels fear */
    if (mon_tim_find(mon, T_FEAR) && dam > 0) mon->pain += dam;

    /* Sometimes a monster gets scared by damage */
    if ( !mon_tim_find(mon, T_FEAR)
      && !mon_tim_find(mon, T_BERSERK)
      && _1d(100) > mon_res_pct(mon, GF_FEAR) ) /* XXX check for IMMUNE(FEAR) */
    {
        /* Percentage of fully healthy */
        int percentage = (100L * mon->hp) / mon->maxhp;

        /*
        * Run (sometimes) if at 10% or less of max hit points,
        * or (usually) when hit for half its current hit points
         */
        if (((percentage <= 10) && (randint0(10) < percentage)) ||
            ((dam >= mon->hp) && (randint0(100) < 80)))
        {
            int amt = randint1(10);
            if (dam >= mon->hp && percentage > 7) amt += 20;
            else amt += (11 - percentage)*5;
            mon_tim_add(mon, T_FEAR, amt);
            (*fear) = TRUE;
        }
    }

#endif /* ALLOW_FEAR */

    #if 0
    XXX This routine is all but dead. Its being used by "bird drops" in mon_spell.c
    if ((dam > 0) && !mon_is_pet(mon) && !mon_is_friendly(mon) && (who != m_idx))
    {
        if (mon_is_pet(dun_mon(cave, who)) && !plr_at(mon->target))
        {
            mon_set_target(mon, dun_mon(cave, who)->pos);
        }
    }
    #endif

    if (mon->id == plr->riding && dam > 0)
    {
        char m_name[80];

        monster_desc(m_name, mon, 0);

        if (mon->hp > mon->maxhp/3) dam = (dam + 1) / 2;
        if (rakuba((dam > 200) ? 200 : dam, FALSE))
            msg_format("You have thrown off from %s!", m_name);
    }

    /* Not dead yet */
    return;
}


/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players. See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency. Like not using "mod" or "div" when possible. And
 * attempting to check the conditions in an optimal order. Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
int mon_will_run(mon_ptr m_ptr)
{
#ifdef ALLOW_TERROR

    monster_race *r_ptr = m_ptr->race;

    u16b p_lev, m_lev;
    u16b p_chp, p_mhp;
    u16b m_chp, m_mhp;
    u32b p_val, m_val;

#endif
    #ifdef DEVELOPER
    if (0 && plr->wizard) return TRUE;
    #endif

    /* Friends can be commanded to avoid the player */
    if (mon_is_pet(m_ptr) && m_ptr->dun->id == plr->dun_id)
    {
        /* Are we trying to avoid the player? */
        return ((plr->pet_follow_distance < 0) &&
                  (m_ptr->cdis <= (0 - plr->pet_follow_distance)));
    }

    /* Keep monsters from running too far away */
    if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

    /* All "afraid" monsters will run away */
    if (mon_tim_find(m_ptr, T_FEAR)) return (TRUE);

#ifdef ALLOW_TERROR

    /* Nearby monsters will not become terrified */
    if (m_ptr->cdis <= 5) return (FALSE);

    if (mon_is_friendly(m_ptr)) return FALSE;

    if ( plr->prace == RACE_MON_RING
      && !plr->riding
      && !is_aware(m_ptr)
      && mon_is_type(m_ptr->race, SUMMON_RING_BEARER) )
    {
        return FALSE;
    }

    /* Examine player power (level) */
    p_lev = plr->lev;

    /* Examine monster power (level plus morale) */
    m_lev = r_ptr->alloc.lvl + (m_ptr->id & 0x08) + 25;

    /* Optimize extreme cases below */
    if (m_lev > p_lev + 4) return (FALSE);
    if (m_lev + 4 <= p_lev) return (TRUE);

    /* Examine player health */
    p_chp = plr->chp;
    p_mhp = plr->mhp;

    /* Examine monster health */
    m_chp = m_ptr->hp;
    m_mhp = m_ptr->maxhp;

    /* Prepare to optimize the calculation */
    p_val = (p_lev * p_mhp) + (p_chp << 2); /* div p_mhp */
    m_val = (m_lev * m_mhp) + (m_chp << 2); /* div m_mhp */

    /* Strong players scare strong monsters */
    if (p_val * m_mhp > m_val * p_mhp) return (TRUE);

#endif

    /* Assume no terror */
    return (FALSE);
}

void dispel_monster_status(mon_ptr mon)
{
    mon_tim_dispel(mon);
}

bool process_the_world(int num, bool vs_player)
{
    monster_type *m_ptr = mon_process_current();  /* the world monster */

    if (world_monster) return (FALSE);

    if(vs_player)
    {
        char m_name[80];
        monster_desc(m_name, m_ptr, 0);

        if (mon_race_is_(m_ptr->race, "V.Dio"))
            msg_format("%s yells 'The World! Time has stopped!'", m_name);
        else if (mon_race_is_(m_ptr->race, "p.Wong"))
            msg_format("%s yells 'Time!'", m_name);
        else msg_print("hek!");

        if (plr->pclass == CLASS_TIME_LORD && !mon_save_p(m_ptr, A_WIS))
        {
            msg_format("You grin as time begins to flow once again!");
            return TRUE;
        }

        msg_print(NULL);
    }

    /* This monster cast spells */
    world_monster = m_ptr;

    if (vs_player) do_cmd_redraw();

    while(num--)
    {
        if(!mon_is_valid(m_ptr)) break;
        mon_process_aux(m_ptr);

        if (mon_is_hostile(m_ptr) && plr_project(m_ptr->pos))
            mon_clear_target(m_ptr); /* XXX one counterattack move, then back to plr */

        /* Notice stuff */
        if (plr->notice) notice_stuff();

        /* Update stuff */
        if (plr->update) update_stuff();

        /* Redraw stuff */
        if (plr->redraw) redraw_stuff();

        /* Redraw stuff */
        if (plr->window) window_stuff();

        /* Delay */
        if (vs_player) Term_xtra(TERM_XTRA_DELAY, 500);
    }

    /* Redraw map */
    plr->redraw |= (PR_MAP);

    /* Update monsters */
    plr->update |= (PU_MONSTERS);

    /* Window stuff */
    plr->window |= (PW_OVERHEAD | PW_DUNGEON);

    world_monster = NULL;
    if (vs_player || plr_view(m_ptr->pos))
    {
        msg_print("You feel time flowing around you once more.");
        msg_print(NULL);
    }

    handle_stuff();

    return (TRUE);
}

void mon_gain_exp(mon_ptr mon, int amt)
{
    if (!mon->race->evolution.exp) return;
    mon->exp += amt;
    if (0 || plr->wizard)
        msg_format("<color:D>mon_gain_exp %d (needs %d).</color>", mon->exp, mon->race->evolution.exp);

    if (mon->mflag2 & MFLAG2_CHAMELEON) return;

    if (mon->exp >= mon->race->evolution.exp)
    {
        char m_name[80];
        int old_hp = mon->hp;
        int old_maxhp = mon->max_maxhp;
        mon_race_ptr old_race = mon->race;
        mon_ptr parent = mon_parent(mon); 

        /* Hack -- Reduce the racial counter of previous monster */
        assert(mon->race->alloc.cur_num > 0);
        mon->race->alloc.cur_num--;

        monster_desc(m_name, mon, 0);
        mon->race = mon_race_lookup(mon->race->evolution.id);
        mon->apparent_race = mon->race;

        /* Count the monsters on the level */
        mon->race->alloc.cur_num++;

        mon->max_maxhp = dice_roll(mon->race->hp);
        mon->maxhp = mon->max_maxhp;
        mon->hp = old_hp * mon->maxhp / old_maxhp;

        /* Extract the monster base speed */
        mon->mspeed = get_mspeed(mon->race);

        /* alignment: maintain allegiance to master if possible */
        mon->align = mon->race->align;
        if (parent && !mon->align)
            mon->align = parent->align;
            
        mon->exp = 0;

        if (mon_is_pet(mon) || mon->ml)
        {
            if (!ignore_unview || plr_can_see(mon->pos))
            {
                if (plr_tim_find(T_HALLUCINATE))
                {
                    monster_race *hallu_race;

                    do
                    {
                        hallu_race = vec_random(mon_alloc_tbl);
                    }
                    while (mon_race_is_unique(hallu_race));

                    msg_format("%^s evolved into %s.", m_name, hallu_race->name);
                }
                else
                {
                    msg_format("%^s evolved into %s.", m_name, mon->race->name);
                }
            }

            if (!plr_tim_find(T_HALLUCINATE))
                old_race->lore.flags |= RFL_EVOLUTION;

            /* Now you feel very close to this pet. */
            mon_set_parent(mon, 0);
        }
        mon_lore_sighting(mon);
        update_mon(mon, FALSE);
        draw_pos(mon->pos);
    }
    if (mon->id == plr->riding) plr->update |= PU_BONUS;
}

void monster_gain_exp(mon_ptr mon, mon_race_ptr slain)
{
    int new_exp;

    if (!mon_is_valid(mon)) return;

    new_exp = slain->mexp * slain->alloc.lvl / (mon_lvl(mon) + 2);
    if (mon->id == plr->riding) new_exp = (new_exp + 1) / 2;
    if (cave->type->id == D_SURFACE) new_exp /= 5;

    /* Experimental: Share the xp with the player */
    if (mon_is_pet(mon))
    {
        int  div = 5;
        bool penalty = TRUE;
        int  exp;

        if ( prace_is_(RACE_MON_QUYLTHULG)
          || (prace_is_(RACE_MON_RING) && plr->riding == mon->id) )
        {
            div = 1;
            penalty = FALSE;
        }

        exp = new_exp / div;
        gain_exp(exp);
        if (penalty)
            new_exp -= exp;
        if (new_exp < 0) new_exp = 0;
    }

    mon_gain_exp(mon, new_exp);
}
