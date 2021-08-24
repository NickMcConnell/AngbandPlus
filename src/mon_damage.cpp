/* File: mon_damage.cpp */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include <QTime>
#include <QDate>




typedef struct
{
  int message_begin;
  int message_end;
  int message_increase;
  u32b flag_resist;
} mon_timed_effect;

/*
 * Monster timed effects.  Notice this code assumes the monster resist
 * is in the third set of flags.
 * '0' means no message.
 */

static mon_timed_effect effects[] =
{
    /*TMD_MON_SLEEP*/
    {MON_MSG_FALL_ASLEEP, MON_MSG_WAKES_UP, FALSE, RF3_NO_SLEEP},
    /*TMD_MON_STUN*/
    {MON_MSG_DAZED, MON_MSG_NOT_DAZED, MON_MSG_MORE_DAZED, RF3_NO_STUN },
    /*TMD_MON_CONF*/
    {MON_MSG_CONFUSED, MON_MSG_NOT_CONFUSED, MON_MSG_MORE_CONFUSED, RF3_NO_CONF },
    /*TMD_MON_FEAR*/
    {MON_MSG_FLEE_IN_TERROR, MON_MSG_NOT_AFRAID, MON_MSG_MORE_AFRAID, RF3_NO_FEAR },
    /*66*/
    {MON_MSG_SLOWED, MON_SNG_NOT_SLOWED, MON_MSG_MORE_SLOWED, RF3_NO_SLOW  },
    /*TMD_MON_FAST*/
    {MON_MSG_HASTED, MON_MSG_NOT_HASTED, MON_MSG_MORE_HASTED, 0L  },

};

static int charisma_adjustment(const monster_race *r_ptr)
{

    /*stupid or brainless monsters aren't affected by player charisma*/
    if (r_ptr->flags2 & (RF2_STUPID | RF2_EMPTY_MIND)) return (0);

    /*weird monsters are rarely affected by player charisma*/
    if ((r_ptr->flags2 & (RF2_WEIRD_MIND)) && (!one_in_(10))) return (0);

    /*charisma applies*/
    return (adj_chr_charm[p_ptr->state.stat_index[A_CHR]]);
}

#define ZERO_RESIST		0
#define HALF_RESIST		1
#define FULL_RESIST		2


/*
 * Helper function for mon_set_timed.  This determined if the monster
 * Successfully resisted the effect.  Also marks the lore for any
 * appropriate resists.
 */
static int mon_resist_effect(int m_idx, int idx, u16b flag)
{
    mon_timed_effect *effect = &effects[idx];
    int resisted = ZERO_RESIST;
    int resist_chance;
    monster_type *m_ptr = &mon_list[m_idx];
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    /* Hasting never fails */
    if (idx == MON_TMD_FAST) return (ZERO_RESIST);

    /* Some effects are marked to never fail */
    if (flag & (MON_TMD_FLG_NOFAIL)) return (ZERO_RESIST);

    /* Stupid, weird, or empty monsters aren't affected by some effects*/
    if (r_ptr->flags2 & (RF2_STUPID | RF2_EMPTY_MIND | RF2_WEIRD_MIND))
    {
        if (idx == MON_TMD_CONF) return (FULL_RESIST);
        if (idx == MON_TMD_SLEEP) return (FULL_RESIST);
    }

    /* Calculate the chance of the monster resisting. */
    if (flag & (MON_TMD_MON_SOURCE))
    {
        resist_chance = r_ptr->level;
    }
    else
    {
        resist_chance = r_ptr->level + 25 - p_ptr->lev / 5;
        resist_chance -= charisma_adjustment(r_ptr);
    }

    /* Monsters who resist get half the duration, at most */
    if (r_ptr->flags3 & (effect->flag_resist))
    {
        resisted = HALF_RESIST;

        /* Mark the lore */
        if (flag & MON_TMD_FLG_SEEN) l_ptr->r_l_flags3 |= effect->flag_resist;

        /* 2 changes to resist */
        if (randint0(100) < resist_chance) return (FULL_RESIST);
        if (randint0(100) < resist_chance) return (FULL_RESIST);

    }

    /* Uniques are doubly hard to affect */
    if (r_ptr->flags1 & RF1_UNIQUE)
    {
        resisted = HALF_RESIST;
        if (randint0(100) < resist_chance) return (FULL_RESIST);
    }

    /* Monsters with specific breaths and undead get an extra chance at resisting at stunning*/
    if ((idx == MON_TMD_STUN) &&
        ((r_ptr->flags4 & (RF4_BRTH_SOUND | RF4_BRTH_FORCE)) || (monster_nonliving(r_ptr))))
    {
        resisted = HALF_RESIST;

        if ((randint0(100) < resist_chance))
        {
            /* Add the lore */
            if (flag & MON_TMD_FLG_SEEN)
            {
                if (r_ptr->flags4 & (RF4_BRTH_SOUND))
                {
                    l_ptr->r_l_flags4 |= RF4_BRTH_SOUND;
                }
                if (r_ptr->flags4 & (RF4_BRTH_FORCE))
                {
                    l_ptr->r_l_flags4 |= RF4_BRTH_FORCE;
                }
            }

            return (FULL_RESIST);
        }
    }

    /* Monsters with specific breaths get an extra chance at resisting confusion*/
    if ((idx == MON_TMD_CONF) &&
        (r_ptr->flags4 & (RF4_BRTH_CONFU | RF4_BRTH_CHAOS)))
    {

        resisted = HALF_RESIST;

        if ((randint0(100) < resist_chance))
        {
            /* Add the lore */
            if (flag & MON_TMD_FLG_SEEN)
            {
                if (r_ptr->flags4 & (RF4_BRTH_CONFU))
                {
                    l_ptr->r_l_flags4 |= RF4_BRTH_CONFU;
                }
                if (r_ptr->flags4 & (RF4_BRTH_CHAOS))
                {
                    l_ptr->r_l_flags4 |= RF4_BRTH_CHAOS;
                }
            }
            return (FULL_RESIST);
        }
    }

    /* Very difficult to make non-living creatures sleep */
    if ((idx == MON_TMD_SLEEP) &&  (monster_nonliving(r_ptr)))
    {
        resisted = HALF_RESIST;

        if ((randint0(100) < resist_chance)) return (FULL_RESIST);
    }

    /* Inertia breathers are highly resistant to slowing*/
    if ((idx == MON_TMD_SLOW) && (r_ptr->flags4 & (RF4_BRTH_INER)))
    {
        resisted = HALF_RESIST;

        if ((randint0(100) < resist_chance))
        {
            /* Add the lore */
            if (flag & MON_TMD_FLG_SEEN)
            {
                l_ptr->r_l_flags4 |= RF4_BRTH_INER;
            }

            return (FULL_RESIST);

        }
    }

    return (resisted);
}

/*
 * Set a timed monster event to 'v'.  Give messages if the right flags are set.
 * Check if the monster is able to resist the spell.  Mark the lore
 * Note much this code assumes the monster resistances are in the
 * r_ptr>flags3 set.
 * Returns TRUE if the monster was affected
 * Return FALSE if the monster was unaffected.
 */
static bool mon_set_timed(int m_idx, int idx, int v, u16b flag)
{
    mon_timed_effect *effect = &effects[idx];
    monster_type *m_ptr = &mon_list[m_idx];

    QString m_name;
    int m_note = FALSE;

    int resisted;

    m_note = 0;

    /* Get monster name*/
    m_name = monster_desc(m_ptr, 0);

    bool old_sleep = FALSE;
    bool new_sleep = FALSE;
    if (m_ptr->m_timed[MON_TMD_SLEEP]) old_sleep = TRUE;

    /* No change */
    if (m_ptr->m_timed[idx] == v) return FALSE;

    /* Turning off, usually mention */
    if (v == 0)
    {
        m_note = effect->message_end;

        flag |= MON_TMD_FLG_NOTIFY;
    }

    /* Turning on, usually mention */
    else if (m_ptr->m_timed[idx] == 0)
    {

        flag |= MON_TMD_FLG_NOTIFY;

        m_note = effect->message_begin;
    }
    /* Different message for increases, but don't automatically mention. */
    else if (v > m_ptr->m_timed[idx])
    {
        m_note = effect->message_increase;
    }

    /* Determine if the monster resisted or not */
    resisted = mon_resist_effect(m_idx, idx, flag);

    if (resisted == FULL_RESIST)
    {
        m_note = MON_MSG_UNAFFECTED;
    }

    /* Cut the increase duration in half */
    else if (resisted == HALF_RESIST)
    {
        int change = v - m_ptr->m_timed[idx];

        m_note = MON_MSG_RESIST_SOMEWHAT;

        /* Paranoia - make sure it is an increase that can be cut in half */
        if (change > 1)
        {
            change /= 2;
            v = m_ptr->m_timed[idx] + change;
        }
    }

    /* set the JUST_SCARED flag */
    else if (idx == MON_TMD_FEAR)
    {
        if (v > m_ptr->m_timed[idx]) m_ptr->mflag |= (MFLAG_JUST_SCARED);
    }

    /* Apply the value, unless they fully resisted */
    if (resisted != FULL_RESIST)
    {
        m_ptr->m_timed[idx] = v;
    }

    if ((idx == MON_TMD_FAST) || (idx == MON_TMD_SLOW))
    {
         calc_monster_speed(m_ptr->fy, m_ptr->fx);
    }
    /* Just waking up, clear some of the other effects */
    else if ((idx != MON_TMD_SLEEP) && one_in_(2))
    {
        m_ptr->m_timed[MON_TMD_SLEEP] = 0;
    }
    if (m_ptr->m_timed[MON_TMD_SLEEP]) new_sleep = TRUE;

    /* Update the visuals, as appropriate. */
    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);
    if (m_ptr->ml)
    {
        if (old_sleep != new_sleep) p_ptr->redraw |= (PR_WIN_MONLIST);
    }

    /* Return result without any messages */
    if ((flag & (MON_TMD_FLG_NOMESSAGE)) || (!m_note) ||
        (!(flag & (MON_TMD_FLG_SEEN))) ||
        (!(flag & (MON_TMD_FLG_NOTIFY))))
    {
        /* Return a boolean result */
        if (resisted == FULL_RESIST) return FALSE;
        return (TRUE);
    }

    /* Finally, handle the message */
    add_monster_message(m_name, m_idx, m_note);

    /* Return a boolean result */
    if (resisted == FULL_RESIST) return FALSE;
    return (TRUE);
}

/*
 * Increase the timed effect `idx` by `v`.
 */
bool mon_inc_timed(int m_idx, int idx, int v, u16b flag)
{
    monster_type *m_ptr = &mon_list[m_idx];

    /* Ignore dead monsters */
    if (!m_ptr->r_idx) return FALSE;

    if (v < 0) return (FALSE);

    /* Check we have a valid effect */
    if ((idx < 0) || (idx > MON_TMD_MAX)) return FALSE;

    /* mark if seen */
    if (m_ptr->ml) flag |= MON_TMD_FLG_SEEN;

    /* Hasting never fails */
    if (idx == MON_TMD_FAST) flag |= MON_TMD_FLG_NOFAIL;

    /* Can't prolong sleep of sleeping monsters */
    if ((idx == MON_TMD_SLEEP) &&
        (m_ptr->m_timed[MON_TMD_SLEEP])) return FALSE;

    /* Make it last for a mimimum # of turns if it is a new effect */
    if ((!m_ptr->m_timed[idx]) && (v < 2)) v = 2;

    /* New counter amount */
    v = m_ptr->m_timed[idx] + v;

    /* Boundry Control */
    if (v > 10000) v = 10000;

    return mon_set_timed(m_idx, idx, v, flag);
}

/*
 * Decrease the timed effect `idx` by `v`.
 */
bool mon_dec_timed(int m_idx, int idx, int v, u16b flag)
{
    monster_type *m_ptr = &mon_list[m_idx];

    /* Ignore dead monsters */
    if (!m_ptr->r_idx) return FALSE;

    if (v < 0) return (FALSE);

    /* Check we have a valid effect */
    if ((idx < 0) || (idx > MON_TMD_MAX)) return FALSE;

    /* mark if seen */
    if (m_ptr->ml) flag |= MON_TMD_FLG_SEEN;

    /* Decreasing is never resisted */
    flag |= MON_TMD_FLG_NOFAIL;

    /* New counter amount */
    v = m_ptr->m_timed[idx] - v;

    /* Use clear function if appropriate */
    if (v < 0) return (mon_clear_timed(m_idx, idx, flag));

    return mon_set_timed(m_idx, idx, v, flag);
}

/**
 * Clear the timed effect `idx`.
 */
bool mon_clear_timed(int m_idx, int idx, u16b flag)
{
    monster_type *m_ptr = &mon_list[m_idx];

    /* Ignore dead monsters */
    if (!m_ptr->r_idx) return FALSE;

    if (!m_ptr->m_timed[idx]) return FALSE;

    /* mark if seen */
    if (m_ptr->ml) flag |= MON_TMD_FLG_SEEN;

    /* Monster is no longer desperate */
    if (idx == MON_TMD_FEAR)
    {
        m_ptr->mflag &= ~(MFLAG_DESPERATE);
    }

    /* Clearing never fails */
    flag |= MON_TMD_FLG_NOFAIL;

    /* Check we have a valid effect */
    if ((idx < 0) || (idx > MON_TMD_MAX)) return (FALSE);

    return mon_set_timed(m_idx, idx, 0, flag);
}

/* Helper function to wake monsters who are asleep */
void wake_monster_attack(monster_type *m_ptr, u16b flag)
{
    int m_idx = dungeon_info[m_ptr->fy][m_ptr->fx].monster_idx;

    /* Already Awake */
    if (!m_ptr->m_timed[MON_TMD_SLEEP]) return;

    /* Disturb the monster */
    mon_clear_timed(m_idx, MON_TMD_SLEEP, flag);

    /* Make the monster a little slow to wake up */
    if (m_ptr->m_energy > BASE_ENERGY_MOVE /2) m_ptr->m_energy = BASE_ENERGY_MOVE /2;
}

/* Helper function to sleep monsters who are awake */
bool sleep_monster_spell(monster_type *m_ptr, int v, u16b flag)
{
    s32b m_idx = m_ptr->get_mon_idx();
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Already Asleep */
    if (m_ptr->m_timed[MON_TMD_SLEEP]) return (FALSE);

    /* Monster is eligible for a full sleep */
    if (m_ptr->mon_fully_healthy())
    {
        int new_v = rand_range((r_ptr->sleep + 1) / 2, r_ptr->sleep);

        if (v < new_v) v = new_v;
    }

    /* Disturb the monster */
    return (mon_inc_timed(m_idx, MON_TMD_SLEEP, v, flag));
}




s32b get_experience_by_level(int level)
{
    if (game_mode == GAME_NPPMORIA)
    {
        return (player_exp_nppmoria[level]);
    }

    return (player_exp_nppangband[level]);
}


/*
 * Advance experience levels and print experience
 */
void check_experience(void)
{
    /* Hack -- lower limit */
    if (p_ptr->exp < 0) p_ptr->exp = 0;

    /* Hack -- lower limit */
    if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

    /* Hack -- upper limit */
    if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

    /* Hack -- upper limit */
    if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;

    /* Hack -- maintain "max" experience */
    if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

    /* Redraw experience and score */
    p_ptr->update |= (PU_PLAYER_SCORE);
    p_ptr->redraw |= (PR_SIDEBAR_PL);

    /* Handle stuff */
    handle_stuff();

    /* Lose levels while possible */
    while ((p_ptr->lev > 1) &&
           (p_ptr->exp < (get_experience_by_level(p_ptr->lev-2) * p_ptr->expfact / 100L)))
    {
        /* Lose a level */
        p_ptr->lev--;

        /* Update some stuff */
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

        /* Redraw some stuff */
        p_ptr->redraw |= (PR_TITLEBAR | PR_WIN_MON_RECALL);
    }

    /* Gain levels while possible */
    while ((p_ptr->lev < z_info->max_level) &&
           (p_ptr->exp >= (get_experience_by_level(p_ptr->lev-1) * p_ptr->expfact / 100L)))
    {
        /* Gain a level */
        p_ptr->lev++;

        /* Message */
        color_message(QString("Welcome to level %1.") .arg(p_ptr->lev), TERM_L_BLUE);

        /* Save the highest level*/
        if (p_ptr->lev > p_ptr->max_lev)
        {
            /* update the highest level*/
            p_ptr->max_lev = p_ptr->lev;

            /* If auto-note taking enabled, write a note to the file every 5th level. */
            if ((p_ptr->lev % 5) == 0)
            {
                   QString buf;

                   /* Build the message */
                   buf = (QString("Reached level %1") .arg(p_ptr->lev));

                   /* Write message */
                   write_note(buf,  p_ptr->depth);
            }
        }

        /* Update some stuff */
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

        /* Redraw some stuff */
        p_ptr->redraw |= (PR_SIDEBAR_PL | PR_WIN_MON_RECALL);
    }

    /* Gain max levels while possible */
    while ((p_ptr->max_lev < z_info->max_level) &&
           (p_ptr->max_exp >= (get_experience_by_level(p_ptr->max_lev-1) *
                               p_ptr->expfact / 100L)))
    {
        /* Gain max level */
        p_ptr->max_lev++;

        /* Update some stuff */
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

        /* Redraw some stuff */
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }

    /* Handle stuff */
    handle_stuff();

}


/*
 * Gain experience
 */
void gain_exp(s32b amount)
{

    /* Gain some experience */
    p_ptr->exp += amount;

    /* Slowly recover from experience drainage */
    if (p_ptr->exp < p_ptr->max_exp)
    {
        /* Gain max experience (10%) */
        p_ptr->max_exp += amount / 10;
    }

    /* Check Experience */
    check_experience();

}


/*
 * Lose experience
 */
void lose_exp(s32b amount)
{
    /* Never drop below zero experience */
    if (amount > p_ptr->exp) amount = p_ptr->exp;

    /* Lose some experience */
    p_ptr->exp -= amount;

    /* Check Experience */
    check_experience();
}


/* Helper function for monster_death - drop any objects the monster is holding */
static void mon_drop_held_objects(monster_type *m_ptr)
{
    s16b this_o_idx, next_o_idx = 0;
    object_type *i_ptr;
    object_type object_type_body;

    /* Drop objects being carried */
    for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /*Remove the mark to hide when monsters carry this object*/
        o_ptr->ident &= ~(IDENT_HIDE_CARRY);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Paranoia */
        o_ptr->held_m_idx = 0;

        /* Get local object */
        i_ptr = &object_type_body;

        /* Copy the object */
        i_ptr->object_copy(o_ptr);

        /* Delete the object */
        delete_object_idx(this_o_idx);

        /* Drop it */
        drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
    }

    /* Forget objects */
    m_ptr->hold_o_idx = 0;
}

/*
 * Helper function for monster_death -
 * Intended only to drop Morgoth's special artifacts
 */
static void mon_drop_chosen_objects(monster_type *m_ptr)
{
    object_type *i_ptr;
    object_type object_type_body;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Mega-Hack -- Prepare to make "Grond" */
    object_prep(i_ptr, lookup_kind(TV_HAFTED, SV_GROND));

    /* Mega-Hack -- Mark this item as "Grond" */
    i_ptr->art_num = ART_GROND;

    /* Mega-Hack -- Actually create "Grond" */
    apply_magic(i_ptr, -1, TRUE, TRUE, TRUE, FALSE);

    /* Remember history */
    object_history(i_ptr, ORIGIN_MORGOTH, 0);

    /* Drop it in the dungeon */
    drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);

    /* Get local object */
    i_ptr = &object_type_body;

    /* Mega-Hack -- Prepare to make "Morgoth's crown" */
    object_prep(i_ptr, lookup_kind(TV_CROWN, SV_MORGOTH));

    /* Mega-Hack -- Mark this item as "Morgoth" */
    i_ptr->art_num = ART_MORGOTH;

    /* Mega-Hack -- Actually create "Morgoth" */
    apply_magic(i_ptr, -1, TRUE, TRUE, TRUE, FALSE);

    /* Remember history */
    object_history(i_ptr, ORIGIN_MORGOTH, 0);

    /* Drop it in the dungeon */
    drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
}

/*
 * Helper function for monster_death -
 * Drop the monster's normal objects
 */
static void mon_drop_loot(int m_idx)
{
    monster_type *m_ptr = &mon_list[m_idx];

    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    int j;
    bool chest = (r_ptr->flags1 & (RF1_DROP_CHEST)) ? TRUE : FALSE;
    bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
    bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

    bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
    bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));
    bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

    int force_coin = get_coin_type(r_ptr);

    int dump_item = 0;
    int dump_gold = 0;

    int number_drops = 0;

    object_type *i_ptr;
    object_type object_type_body;

    /* Average dungeon and monster levels */
    s16b set_object_level = object_level = (p_ptr->depth + r_ptr->level) / 2;

    /* Determine how much we can drop */
    if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number_drops++;
    if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number_drops++;
    if (r_ptr->flags1 & (RF1_DROP_1D2)) number_drops += damroll(1, 2);
    if (r_ptr->flags1 & (RF1_DROP_2D2)) number_drops += damroll(2, 2);
    if (r_ptr->flags1 & (RF1_DROP_3D2)) number_drops += damroll(3, 2);
    if (r_ptr->flags1 & (RF1_DROP_4D2)) number_drops += damroll(4, 2);

    /* Hack -- handle creeping coins */
    coin_type = force_coin;

    /* Drop some objects */
    for (j = 0; j < number_drops; j++)
    {
        bool interesting = FALSE;

        /* Re-set the object level */
        object_level = set_object_level;

        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        i_ptr->object_wipe();

        /* work on the "too much junk" problem, large drops sometimes are less items with a "boost". */
        if ((randint(750) < (number_drops * number_drops)) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
        {
            interesting = TRUE;
            number_drops -= 5;
            object_level += 5;

            /*Boundry Control*/
            if (number_drops < 0) number_drops = 0;
            if (object_level > MAX_DEPTH) object_level = MAX_DEPTH;
        }

        /* Make Gold */
        if (do_gold && (!chest) && (!do_item || (rand_int(100) < 50)))
        {
            /* Make some gold */
            if (!make_gold(i_ptr)) continue;

            /* Assume seen XXX XXX XXX */
            dump_gold++;
        }

        /* Make Object */
        else
        {
            if (chest)
            {
                if (!make_object(i_ptr, good, great, DROP_TYPE_CHEST, FALSE)) continue;
            }

            /* Make an object */
            else if (!make_object(i_ptr, good, great, DROP_TYPE_UNTHEMED, interesting)) continue;

            /* Remember history */
            if (visible) object_history(i_ptr, ORIGIN_DROP_KNOWN, m_ptr->r_idx);
            else object_history(i_ptr, ORIGIN_DROP_UNKNOWN, 0);

            /* Assume seen XXX XXX XXX */
            dump_item++;
        }

        /* Drop it in the dungeon */
        drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
    }

    /* Re-set the object level */
    object_level = set_object_level;

    /*If marked for a bonus item, create it and drop it */
    if (m_ptr->mflag & (MFLAG_BONUS_ITEM))
    {
        bool this_good = good;
        bool this_great = great;
        bool this_chest = chest;
        bool interesting = FALSE;

        QString o_name;

        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        i_ptr->object_wipe();

        if (one_in_(50)) this_chest = TRUE;
        if (one_in_(15)) this_great = TRUE;
        if (one_in_(5)) this_good = TRUE;
        if ((!this_good) && (!this_great) && (!this_chest))
        {
            object_level += 5;
            if (object_level > MAX_DEPTH) object_level = MAX_DEPTH;
            interesting = TRUE;
        }

        if (this_chest)
        {
            while (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_CHEST, FALSE)) continue;
        }

        /* Make an object */
        else while (!make_object(i_ptr, this_good, this_good, DROP_TYPE_UNTHEMED, interesting)) continue;

        /* Remember history */
        if (visible) object_history(i_ptr, ORIGIN_DROP_KNOWN, m_ptr->r_idx);
        else object_history(i_ptr, ORIGIN_DROP_UNKNOWN, 0);

        o_name = object_desc(i_ptr, ODESC_PREFIX | ODESC_FULL);

        /* Drop it in the dungeon */
        drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);


    }

    /* Reset the object level */
    object_level = p_ptr->depth;

    /* Reset "coin" type */
    coin_type = 0;

    /* Take note of any dropped treasure */
    if (visible && (dump_item || dump_gold))
    {
        /* Take notes on treasure */
        lore_treasure(m_idx, dump_item, dump_gold);
    }

}

/*
 * Helper function for monster-death.
 * Process the death of a quest monster.
 */
static void process_quest_monster_death(int i, int m_idx, bool *writenote)
{
    quest_type *q_ptr = &q_info[i];
    monster_type *m_ptr = &mon_list[m_idx];

    /* Not the right monster race for certain quests */
    if (quest_single_r_idx(q_ptr) || quest_fixed(q_ptr))
    {
        if (q_ptr->mon_idx != m_ptr->r_idx) return;
    }

    /* Not a quest that counts monster deaths */
    else if (quest_multiple_r_idx(q_ptr))
    {
        if (!(m_ptr->mflag & (MFLAG_QUEST))) return;
    }

    else if (quest_timed(q_ptr))
    {
        if (m_ptr->mflag & (MFLAG_QUEST))
        {
            q_ptr->q_num_killed++;
            p_ptr->notice |= PN_QUEST_REMAIN;
        }
        return;
    }

    else return;

    /* Mark kills */
    q_ptr->q_num_killed++;

    /* Completed quest? */
    if (q_ptr->q_num_killed >= q_ptr->q_max_num)
    {
        /* Mark complete */
        quest_finished(q_ptr);

        /*
         * Make a note of the completed quest, but not for fixed quests.
         * That is a special note written later.
         */
        if (!quest_fixed(q_ptr))
        {
            write_quest_note(TRUE);
            *writenote = FALSE;
        }
    }

    /*not done yet*/
    if (!(q_ptr->q_flags & (QFLAG_COMPLETED)))
    {
        p_ptr->notice |= PN_QUEST_REMAIN;
    }

    /* Update the quest status */
    p_ptr->redraw |= (PR_SIDEBAR_PL);
}

/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques or quest monsters.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx, int who)
{
    int i;
    int total = 0;
    bool questlevel = FALSE;
    bool completed = FALSE;
    bool fixedquest = FALSE;
    bool writenote = TRUE;

    monster_type *m_ptr = &mon_list[m_idx];

    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Drop any objects the monster is carrying */
    if (m_ptr->hold_o_idx)
    {
        mon_drop_held_objects(m_ptr);
    }

    /* Mega-Hack -- drop "winner" treasures */
    if (r_ptr->flags1 & (RF1_DROP_CHOSEN))
    {
        mon_drop_chosen_objects(m_ptr);
    }

    /* Drop the monster's standard loot */
    mon_drop_loot(m_idx);

    /* Update monster list window */
    p_ptr->redraw |= (PR_WIN_MONLIST);

    /* Count incomplete quests */
    for (i = 0; i < z_info->q_max; i++)
    {
        quest_type *q_ptr = &q_info[i];

        /*
         * Hack - don't count if player didn't kill, or on a town level
         * This assumes only a player can kill quest monsters!!!!!
         */
        if (((who != SOURCE_PLAYER) && (who != SOURCE_TRAP)) || (!p_ptr->depth)) continue;

        /* Quest level? */
        if ((q_ptr->base_level == p_ptr->depth) && !is_quest_complete(i))
        {
            /* We are on a quest level */
            questlevel = TRUE;

            /* Mark fixed quests */
            if (quest_fixed(q_ptr)) fixedquest = TRUE;

            process_quest_monster_death(i, m_idx, &writenote);

            /* We just completed the quest */
            if (q_ptr->q_flags & (QFLAG_COMPLETED))
            {
                completed = TRUE;
            }
        }

        /* Count remaining permanent quests */
        if (quest_fixed(q_ptr))
        {
            if (!is_quest_complete(i)) total++;
        }
    }

    /* If the player kills a Unique, and the notes option is on, write a note.
     * If the unique is a guild questor, the note was already written */
    if ((r_ptr->flags1 & (RF1_UNIQUE)) && (writenote))
    {

        QString note2;
        QString real_name;

        /*write note for player ghosts*/
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
        {
            /*paranoia*/
            /* Check there is a name/ghost first */
            if (!player_ghost_name.length())
            {
                /*Make sure the name has been created*/
                prepare_ghost_name();
            }

            note2 = (QString("Destroyed %1") .arg(capitalize_first(player_ghost_name)));
        }

        /*All other uniques*/
        else
        {
            /* Get the monster's real name for the notes file */
            real_name = monster_desc_race(m_ptr->r_idx);

            /* Write note */
            if (monster_nonliving(r_ptr)) note2 = (QString("Destroyed %1") .arg(real_name));
            else note2 = (QString("Killed %1") .arg(real_name));
        }

        write_note(note2, p_ptr->depth);
    }

    /* Only process dungeon kills */
    if (!p_ptr->depth) return;

    /* Hack, check if the Balrog of Moria just died in NPPMoria */
    if (game_mode == GAME_NPPMORIA)
    {
        /* Only the Balrog of Moria should have this flag */
        if (!(r_ptr->flags1 & (RF1_QUESTOR))) return;

        /* Set the variables so the game knows we are done */
        questlevel = TRUE;
        completed = TRUE;
        fixedquest = TRUE;
        total = FALSE;
    }


    /* Require a quest level */
    if (!questlevel) return;

    /* Require all quests on this level to be completed */
    if (!completed) return;

    /* Check quest type */
    if (!fixedquest)
    {
        /* Give a message */
        message(QString("You have completed your quest - collect your reward at the guild!"));

        /* Turn on quest indicator */
        quest_indicator_timer = 50;
        quest_indicator_complete = TRUE;

        ui_animate_accomplishment(p_ptr->py, p_ptr->px, GF_DISP_ALL);

        /* Redraw the status */
        p_ptr->redraw |= (PR_SIDEBAR_PL);

        return;
    }

    /* Need some stairs */
    else if (total)
    {
        p_ptr->q_fame += 150;
        altered_inventory_counter += 50;

        ui_animate_accomplishment(p_ptr->py, p_ptr->px, GF_STATIC);
    }

    /* Nothing left, game over... */
    else
    {

        /* Total winner */
        p_ptr->total_winner = TRUE;

        /* Redraw the "title" */
        p_ptr->redraw |= (PR_TITLEBAR);

        p_ptr->q_fame += 500;
        altered_inventory_counter += 200;

        /* Congratulations */
        message(QString("*** CONGRATULATIONS ***"));
        message(QString("You have won the game!"));
        message(QString("You may retire (commit suicide) when you are ready."));
        ui_animate_accomplishment(p_ptr->py, p_ptr->px, GF_TIME);

        /* Write a note */
        QDate today = QDate::currentDate();
        QTime right_now = QTime::currentTime();

        /* Get time */
        QString long_day = QString("%1 at %2") .arg(today.toString()) .arg(right_now.toString());

        /* Write message */
        if (game_mode == GAME_NPPMORIA) write_note(QString("<b>%1 slew The Balrog of Moria on %2.</b>") .arg(op_ptr->full_name) .arg(long_day), p_ptr->depth);
        else write_note(QString("<b>%1 slew Morgoth on %2.</b>") .arg(op_ptr->full_name)  .arg(long_day), p_ptr->depth);
        write_note(QString("<b>Long live %1!!!!!!</b>") .arg(op_ptr->full_name) , p_ptr->depth);
        write_note(QString("<b>Long live %1!!!!!!</b>") .arg(op_ptr->full_name) , p_ptr->depth);

    }
}


/*Helper function to calculate the monster experience*/
static s32b calc_mon_exp(const monster_race *r_ptr)
{
    /*calculate the monster experience*/
    s32b new_exp = ((long)r_ptr->mexp * r_ptr->level) / p_ptr->lev;

    s16b new_level = p_ptr->max_lev;

    /*not a full point of experience to gain*/
    if (new_exp < 1) return (0);

    /*
     * Check to make sure player is at level 50, so no adjustmetn necessary,
     * also prevents next line from crashing the game
     */
    while (new_level < z_info->max_level)
    {
        s32b remaining_exp;
        s32b net_exp_gain;

        /*
         * Player is not gaining a new max level
         * (in the player_exp chart level 1 exp-to-gain-next-level is at slot 0)
         */
        if ((p_ptr->exp + new_exp) <= (get_experience_by_level(new_level-1)) * p_ptr->expfact / 100L) break;

        /*just checking this again*/
        if (new_exp < 1) break;

        /*figure out the remainder*/
        net_exp_gain = (p_ptr->exp + new_exp) - (get_experience_by_level(new_level-1) * p_ptr->expfact / 100L);

        /*add one level*/
        new_level++;

        /*player is going up a max level, adjust*/
        remaining_exp = ((long)net_exp_gain * (new_level - 1)) / new_level;

        /*slightly reduce new experience*/
        new_exp -= (net_exp_gain - remaining_exp);
    }

    return (new_exp);
}

/*
 * Decrease a monster's hit points, handle monster death.
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
 * Invisible monsters induce a special "You have killed it." message.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * Consider decreasing monster experience over time, say, by using
 * "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))" instead
 * of simply "(m_exp * m_lev) / (p_lev)", to make the first monster
 * worth more than subsequent monsters.  This would also need to
 * induce changes in the monster recall code.  XXX XXX XXX
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, QString note, int who, bool do_death_message)
{
    monster_type *m_ptr = &mon_list[m_idx];

    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    s32b new_exp, new_exp_frac;

    /* Redraw (later) if needed */
    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);
    if (p_ptr->monster_race_idx == m_ptr->r_idx) p_ptr->redraw |= (PR_WIN_MON_RECALL);

    if (!dam) return(FALSE);

    /* Allow the debugging of damage done. */
    if ((dam > 0) && (cheat_know))
    {
        if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
        {
            message(QString("You do %1 (out of %2) damage.") .arg(dam) .arg(m_ptr->hp));
        }
        else message(QString("%1 (out of %2) damage has been done.") .arg(dam) .arg(m_ptr->hp));

    }

    /* Wake it up */
    wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

    /* Hurt it */
    m_ptr->hp -= dam;

    /* It is dead now */
    if (m_ptr->hp < 0)
    {
        QString m_name;

        /* Extract monster name */
        m_name = monster_desc(m_ptr, 0);

        /* Increase the noise level slightly. */
        if (add_wakeup_chance <= 8000) add_wakeup_chance += 300;

        /* Death by Missile/Spell attack */
        if (!do_death_message)
        {
            // Suppress messages below
        }
        else if (note.length())
        {
                message(QString("%1%2") .arg(capitalize_first(m_name)) .arg(note));
        }

        /* Death by physical attack -- invisible monster */
        else if (!m_ptr->ml)
        {
            if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
            {
                message(QString("You have killed %1.") .arg(m_name));
            }
            else message(QString("%1 has been killed.") .arg(capitalize_first(m_name)));
        }

        /* Death by Physical attack -- non-living monster */
        else if (monster_nonliving(r_ptr))
        {
            if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
            {
                message(QString("You have destroyed %1.") .arg(m_name));
            }
            else message(QString("%1 has been destroyed") .arg(capitalize_first(m_name)));
        }

        /* Death by Physical attack -- living monster */
        else
        {
            if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
            {
                message(QString("You have slain %1.") .arg(m_name));
            }
            else message(QString("%1 has been slain") .arg(capitalize_first(m_name)));
        }

        /* Generate treasure */
        monster_death(m_idx, who);

        if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
        {

            /* Give some experience for the kill */
            new_exp = calc_mon_exp(r_ptr);

            /* Handle fractional experience */
            new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % p_ptr->lev)
                         * 0x10000L / p_ptr->lev) + p_ptr->exp_frac;

            /* Keep track of experience */
            if (new_exp_frac >= 0x10000L)
            {
                new_exp++;
                p_ptr->exp_frac = (u16b)(new_exp_frac - 0x10000L);
            }
            else
            {
                p_ptr->exp_frac = (u16b)new_exp_frac;
            }

            /* Gain experience */
            gain_exp(new_exp);
        }

        /* When the player kills a Unique, it stays dead */
        if (r_ptr->flags1 & (RF1_UNIQUE))
        {
            /* This is the "evil iggy" exception in Moria */
            if ((r_ptr->flags2 & (RF2_SPECIAL)) && !(r_ptr->flags1 & (RF1_QUESTOR)))
            {
                /* Do nothing.....yes, I know this is bad coding */
            }

            else r_ptr->max_num = 0;

            /* reputation bonus, except for the town unique */
            if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
            {
                if (r_ptr->level >= p_ptr->lev)p_ptr->q_fame += 5;
                altered_inventory_counter += 2;
            }
        }

        /* When the player kills a player ghost, the template needs to be deleted.
         */
        if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) &&
            ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP)))
        {
            /* fame boost*/
            p_ptr->q_fame += 7;
            altered_inventory_counter += 5;
            delete_player_ghost_entry();
        }

        /* Recall even invisible uniques or winners */
        if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
        {
            /* Count kills this life */
            if (l_ptr->pkills < SHRT_MAX) l_ptr->pkills++;

            /* Count kills in all lives */
            if (l_ptr->tkills < SHRT_MAX) l_ptr->tkills++;

            /* Hack -- Auto-recall */
            monster_race_track(m_ptr->r_idx);
        }

        /* Delete the monster */
        delete_monster_idx(m_idx);

        /* Not afraid */
        (*fear) = FALSE;

        /* Monster is dead */
        return (TRUE);
    }

    /* Sometimes a monster gets scared by damage */
    if (!m_ptr->m_timed[MON_TMD_FEAR] && ((r_ptr->flags3 & (RF3_NO_FEAR)) == 0) && (dam > 0))
    {
        int percentage;

        /* Percentage of fully healthy */
        percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

        /*
         * Run (sometimes) if at 10% or less of max hit points,
         * or (usually) when hit for half its current hit points
         */
        if ((randint(10) >= percentage) ||
            ((dam >= m_ptr->hp) && (!one_in_(5))))
        {
            int fear_amt;

            /* Hack -- note fear */
            (*fear) = TRUE;

            /* Hack -- Add some timed fear */
            fear_amt = rand_range(20, 30) + dam / 5;
            mon_inc_timed(m_idx, MON_TMD_FEAR, fear_amt, MON_TMD_FLG_NOMESSAGE);

            /*a monster can't be wary and afraid*/
            m_ptr->mflag &= ~(MFLAG_WARY);

        }
    }

    /* Monster will always go active */
    m_ptr->mflag |= (MFLAG_ACTV);

    /* Recalculate desired minimum range */
    if (dam > 0) m_ptr->min_range = 0;

    /* Not dead yet */
    return (FALSE);
}
