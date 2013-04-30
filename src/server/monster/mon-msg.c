/*
 * File: mon-msg.c
 * Purpose: Monster message code.
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
#include "mon-util.h"


/*
 * The NULL-terminated array of string actions used to format stacked messages.
 * Singular and plural modifiers are encoded in the same string. Example:
 * "[is|are] hurt" is expanded to "is hurt" if you request the singular form.
 * The string is expanded to "are hurt" if the plural form is requested.
 * The singular and plural parts are optional. Example:
 * "rear[s] up in anger" only includes a modifier for the singular form.
 * Any of these strings can start with "~", in which case we consider that
 * string as a whole message, not as a part of a larger message. This
 * is useful to display Moria-like death messages.
 */
static const char *msg_repository[MAX_MON_MSG + 1] =
{
    /* Dummy action */
    "[is|are] hurt.", /* MON_MSG_NONE */

    /* From project_m */
    "die[s].", /* MON_MSG_DIE */
    "[is|are] destroyed.", /* MON_MSG_DESTROYED */
    "resist[s] a lot.", /* MON_MSG_RESIST_A_LOT */
    "[is|are] hit hard.", /* MON_MSG_HIT_HARD */
    "resist[s].", /* MON_MSG_RESIST */
    "[is|are] immune.", /* MON_MSG_IMMUNE */
    "resist[s] somewhat.", /* MON_MSG_RESIST_SOMEWHAT */
    "[is|are] unaffected!", /* MON_MSG_UNAFFECTED */
    "spawn[s]!", /* MON_MSG_SPAWN */
    "look[s] healthier.", /* MON_MSG_HEALTHIER */
    "fall[s] asleep!", /* MON_MSG_FALL_ASLEEP */
    "wake[s] up.", /* MON_MSG_WAKES_UP */
    "cringe[s] from the light!", /* MON_MSG_CRINGE_LIGHT */
    "shrivel[s] away in the light!", /* MON_MSG_SHRIVEL_LIGHT */
    "lose[s] some skin!", /* MON_MSG_LOSE_SKIN */
    "dissolve[s]!", /* MON_MSG_DISSOLVE */
    "catch[es] fire!", /* MON_MSG_CATCH_FIRE */
    "[is|are] badly frozen.", /* MON_MSG_BADLY_FROZEN */
    "shudder[s].", /* MON_MSG_SHUDDER */
    "change[s]!", /* MON_MSG_CHANGE */
    "disappear[s]!", /* MON_MSG_DISAPPEAR */
    "[is|are] even more stunned.", /* MON_MSG_MORE_DAZED */
    "[is|are] stunned.", /* MON_MSG_DAZED */
    "[is|are] no longer stunned.", /* MON_MSG_NOT_DAZED */
    "look[s] more confused.", /* MON_MSG_MORE_CONFUSED */
    "look[s] confused.", /* MON_MSG_CONFUSED */
    "[is|are] no longer confused.", /* MON_MSG_NOT_CONFUSED */
    "look[s] more slowed.", /* MON_MSG_MORE_SLOWED */
    "look[s] slowed.", /* MON_MSG_SLOWED */
    "speed[s] up.", /* MON_MSG_NOT_SLOWED */
    "look[s] even faster!", /* MON_MSG_MORE_HASTED */
    "start[s] moving faster.", /* MON_MSG_HASTED */
    "slow[s] down.", /* MON_MSG_NOT_HASTED */
    "look[s] more terrified!", /* MON_MSG_MORE_AFRAID */
    "flee[s] in terror!", /* MON_MSG_FLEE_IN_TERROR */
    "[is|are] no longer afraid.", /* MON_MSG_NOT_AFRAID */
    "~You hear [a|several] scream[|s] of agony!", /* MON_MSG_MORIA_DEATH */
    "disintegrate[s]!", /* MON_MSG_DISENTEGRATES */
    "freez[es] and shatter[s]!", /* MON_MSG_FREEZE_SHATTER */
    "lose[s] some mana!", /* MON_MSG_MANA_DRAIN */
    "look[s] briefly puzzled.", /* MON_MSG_BRIEF_PUZZLE */
    "maintain[s] the same shape.", /* MON_MSG_MAINTAIN_SHAPE */

    /* From message_pain */
    "[is|are] unharmed.", /* MON_MSG_UNHARMED */

    /* Dummy messages for monster pain - we use edit file info instead. */
    "", /* MON_MSG_95 */
    "", /* MON_MSG_75 */
    "", /* MON_MSG_50 */
    "", /* MON_MSG_35 */
    "", /* MON_MSG_20 */
    "", /* MON_MSG_10 */
    "", /* MON_MSG_0 */

    /* PWMAngband */
    "bleed[s] from open wounds!",  /* MON_MSG_BLEED */
    "[is|are] no longer bleeding.", /* MON_MSG_NOT_BLEEDING */
    "bleed[s] even more.", /* MON_MSG_MORE_BLEEDING */
    "[is|are] poisoned!", /* MON_MSG_POISONED */
    "[is|are] no longer poisoned.", /* MON_MSG_NOT_POISONED */
    "look[s] more poisoned.", /* MON_MSG_MORE_POISONED */
    "grope[s] around blindly!",  /* MON_MSG_BLIND */
    "[is|are] no longer blind.", /* MON_MSG_NOT_BLIND */
    "look[s] more blind.", /* MON_MSG_MORE_BLIND */
    "[is|are] paralyzed!",  /* MON_MSG_PARALYZED */
    "[is|are] no longer paralyzed.", /* MON_MSG_NOT_PARALYZED */
    "look[s] more paralyzed.", /* MON_MSG_MORE_PARALYZED */
    "drop[s] dead.", /* MON_MSG_DROP_DEAD */
    "hate[s] you too much!", /* MON_MSG_HATE */
    "react[s] to your order.", /* MON_MSG_REACT */
    "return[s]!", /* MON_MSG_RETURN */
    "[is|are] torn apart by gravity.", /* MON_MSG_TORN */
    "[is|are] blasted by shards of ice.", /* MON_MSG_ICE */
    "[is|are] embedded in the rock!", /* MON_MSG_EMBEDDED */
    "wail[s] out in pain!", /* MON_MSG_WAIL */
    "croak[s] in agony.", /* MON_MSG_CROAK */

    NULL /* MAX_MON_MSG */
};


static char *get_mon_msg_action(byte msg_code, bool do_plural, struct monster_race *race);


/*
 * Displays a message describing a player's reaction to damage.
 */
void player_pain(int Ind, int who, int dam)
{
    player_type *p_ptr = player_get(Ind);
    long oldhp, newhp, tmp;
    int percentage;
    player_type *q_ptr = player_get(who);
    int msg_code = MON_MSG_UNHARMED;
    char m_name[NORMAL_WID];
    char *action;

    /* Paranoia */
    if (!who) return;

    /* Get the player name */
    player_desc(p_ptr, m_name, sizeof(m_name), q_ptr, TRUE);

    /* Notice non-damage */
    if (dam == 0)
    {
        action = get_mon_msg_action(msg_code, FALSE, &r_info[0]);
        msg(p_ptr, "%s %s", m_name, action);
        return;
    }

    /* Note -- Subtle fix */
    newhp = (long)(q_ptr->chp);
    oldhp = newhp + (long)(dam);
    tmp = ((oldhp > 0)? ((newhp * 100L) / oldhp): 0);
    percentage = (int)(tmp);

    if (percentage > 95) msg_code = MON_MSG_95;
    else if (percentage > 75) msg_code = MON_MSG_75;
    else if (percentage > 50) msg_code = MON_MSG_50;
    else if (percentage > 35) msg_code = MON_MSG_35;
    else if (percentage > 20) msg_code = MON_MSG_20;
    else if (percentage > 10) msg_code = MON_MSG_10;
    else msg_code = MON_MSG_0;

    action = get_mon_msg_action(msg_code, FALSE, &r_info[0]);
    msg(p_ptr, "%s %s", m_name, action);
}


/*
 * Adds to the message queue a message describing a monster's reaction
 * to damage.
 */
void message_pain(int Ind, struct monster *m_ptr, int dam)
{
    player_type *p_ptr = player_get(Ind);
    long oldhp, newhp, tmp;
    int percentage;
    int msg_code = MON_MSG_UNHARMED;
    char m_name[NORMAL_WID];

    /* Get the monster name */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

    /* Notice non-damage */
    if (dam == 0)
    {
        add_monster_message(p_ptr, m_name, m_ptr, msg_code, FALSE);
        return;
    }

    /* Note -- Subtle fix */
    newhp = (long)(m_ptr->hp);
    oldhp = newhp + (long)(dam);
    tmp = ((oldhp > 0)? ((newhp * 100L) / oldhp): 0);
    percentage = (int)(tmp);

    if (percentage > 95) msg_code = MON_MSG_95;
    else if (percentage > 75) msg_code = MON_MSG_75;
    else if (percentage > 50) msg_code = MON_MSG_50;
    else if (percentage > 35) msg_code = MON_MSG_35;
    else if (percentage > 20) msg_code = MON_MSG_20;
    else if (percentage > 10) msg_code = MON_MSG_10;
    else msg_code = MON_MSG_0;

    add_monster_message(p_ptr, m_name, m_ptr, msg_code, FALSE);
}


#define SINGULAR_MON    1
#define PLURAL_MON      2


/*
 * Returns a pointer to a statically allocatted string containing a formatted
 * message based on the given message code and the quantity flag.
 * The contents of the returned value will change with the next call
 * to this function
 */
static char *get_mon_msg_action(byte msg_code, bool do_plural, struct monster_race *race)
{
    static char buf[200];
    const char *action;
    u16b n = 0;

    /* Regular text */
    byte flag = 0;

    my_assert(msg_code < MAX_MON_MSG);
    action = msg_repository[msg_code];

    my_assert(race->base && race->base->pain);

    if (race->base && race->base->pain)
    {
        switch (msg_code)
        {
            case MON_MSG_95: action = race->base->pain->messages[0]; break;
            case MON_MSG_75: action = race->base->pain->messages[1]; break;
            case MON_MSG_50: action = race->base->pain->messages[2]; break;
            case MON_MSG_35: action = race->base->pain->messages[3]; break;
            case MON_MSG_20: action = race->base->pain->messages[4]; break;
            case MON_MSG_10: action = race->base->pain->messages[5]; break;
            case MON_MSG_0: action = race->base->pain->messages[6]; break;
        }
    }

    /* Put the message characters in the buffer */
    for (; *action; action++)
    {
        /* Check available space */
        if (n >= (sizeof(buf) - 1)) break;

        /* Are we parsing a quantity modifier? */
        if (flag)
        {
            /* Check the presence of the modifier's terminator */
            if (*action == ']')
            {
                /* Go back to parsing regular text */
                flag = 0;

                /* Skip the mark */
                continue;
            }

            /* Check if we have to parse the plural modifier */
            if (*action == '|')
            {
                /* Switch to plural modifier */
                flag = PLURAL_MON;

                /* Skip the mark */
                continue;
            }

            /* Ignore the character if we need the other part */
            if ((flag == PLURAL_MON) != do_plural) continue;
        }

        /* Do we need to parse a new quantity modifier? */
        else if (*action == '[')
        {
            /* Switch to singular modifier */
            flag = SINGULAR_MON;

            /* Skip the mark */
            continue;
        }

        /* Append the character to the buffer */
        buf[n++] = *action;
    }

    /* Terminate the buffer */
    buf[n] = '\0';

    /* Done */
    return (buf);
}


/*
 * Tracks which monster has had which pain message stored, so redundant messages
 * don't happen due to monster attacks hitting other monsters.
 * Returns TRUE if the message is redundant.
 */
static bool redundant_monster_message(struct player *p, struct monster *m_ptr, int msg_code)
{
    int i;

    my_assert(m_ptr);
    my_assert((msg_code >= 0) && (msg_code < MAX_MON_MSG));

    /* No messages yet */
    if (!p->size_mon_hist) return FALSE;

    for (i = 0; i < p->size_mon_hist; i++)
    {
        /* Not the same monster */
        if (m_ptr != p->mon_message_hist[i].mon) continue;

        /* Not the same code */
        if (msg_code != p->mon_message_hist[i].message_code) continue;

        /* We have a match. */
        return (TRUE);
    }

    return (FALSE);
}


/*
 * Stack a codified message for the given monster race. You must supply
 * the description of some monster of this race. You can also supply
 * different monster descriptions for the same race.
 * Return TRUE on success.
 */
bool add_monster_message(struct player *p, const char *mon_name, struct monster *m_ptr,
    int msg_code, bool delay)
{
    int i;
    byte mon_flags = 0;
    int r_idx = m_ptr->r_idx;

    /* Paranoia */
    if (!p) return (FALSE);

    my_assert((msg_code >= 0) && (msg_code < MAX_MON_MSG));

    if (redundant_monster_message(p, m_ptr, msg_code)) return (FALSE);

    /* Paranoia */
    if (!mon_name || !mon_name[0]) mon_name = "it";

    /* Save the "hidden" mark, if present */
    if (strstr(mon_name, "(hidden)")) mon_flags |= 0x01;

    /* Save the "offscreen" mark, if present */
    if (strstr(mon_name, "(offscreen)")) mon_flags |= 0x02;

    /* Monster is invisible or out of LOS */
    if (streq(mon_name, "it") || streq(mon_name, "something")) mon_flags |= 0x04;

    /* Query if the message is already stored */
    /*for (i = 0; i < p->size_mon_msg; i++)*/
    for (i = p->size_mon_msg - 1; i < p->size_mon_msg; i++)
    {
        /* We found the race and the message code */
        if ((p->mon_msg[i].mon_race == r_idx) &&
            (p->mon_msg[i].mon_flags == mon_flags) &&
            (p->mon_msg[i].msg_code == msg_code))
        {
            /* Can we increment the counter? */
            if (p->mon_msg[i].mon_count < MAX_UCHAR)
            {
                /* Stack the message */
                ++(p->mon_msg[i].mon_count);
            }

            /* Success */
            return (TRUE);
        }
    }

    /* The message isn't stored. Check free space */
    if (p->size_mon_msg >= MAX_STORED_MON_MSG) return (FALSE);

    /* Assign the message data to the free slot */
    p->mon_msg[i].mon_race = r_idx;
    p->mon_msg[i].mon_flags = mon_flags;
    p->mon_msg[i].msg_code = msg_code;
    p->mon_msg[i].delay = delay;

    /* Just this monster so far */
    p->mon_msg[i].mon_count = 1;

    /* One more entry */
    ++p->size_mon_msg;

    p->notice |= PN_MON_MESSAGE;

    /* Record which monster had this message stored */
    if (p->size_mon_hist >= MAX_STORED_MON_CODES) return (TRUE);
    p->mon_message_hist[p->size_mon_hist].mon = m_ptr;
    p->mon_message_hist[p->size_mon_hist].message_code = msg_code;
    p->size_mon_hist++;

    /* Success */
    return (TRUE);
}


/*
 * Show and delete the stacked monster messages.
 */
static void flush_monster_messages(struct player *p, bool delay)
{
    int i, r_idx, count;
    const monster_race *r_ptr;
    char buf[512];
    char *action;
    bool action_only;

    /* Show every message */
    for (i = 0; i < p->size_mon_msg; i++)
    {
        if (p->mon_msg[i].delay != delay) continue;

        /* Cache the monster count */
        count = p->mon_msg[i].mon_count;

        /* Paranoia */
        if (count < 1) continue;

        /* Start with an empty string */
        buf[0] = '\0';

        /* Cache the race index */
        r_idx = p->mon_msg[i].mon_race;

        /* Get the proper message action */
        action = get_mon_msg_action(p->mon_msg[i].msg_code, (count > 1), &r_info[r_idx]);

        /* Is it a regular race? */
        if (r_idx > 0)
        {
            /* Get the race */
            r_ptr = &r_info[r_idx];
        }

        /* It's the special mark for non-visible monsters */
        else
        {
            /* No race */
            r_ptr = NULL;
        }

        /* Monster is marked as invisible */
        if (p->mon_msg[i].mon_flags & 0x04) r_ptr = NULL;

        /* Special message? */
        action_only = (*action == '~');

        /* Format the proper message for visible monsters */
        if (r_ptr && !action_only)
        {
            char race_name[NORMAL_WID];

            /* Get the race name */
            my_strcpy(race_name, r_ptr->name, sizeof(race_name));

            /* Uniques */
            if (rf_has(r_ptr->flags, RF_UNIQUE))
            {
                /* Just copy the race name */
                my_strcpy(buf, r_ptr->name, sizeof(buf));
            }

            /* We have more than one monster */
            else if (count > 1)
            {
                /* Get the plural of the race name */
                plural_aux(race_name, sizeof(race_name));

                /* Put the count and the race name together */
                strnfmt(buf, sizeof(buf), "%d %s", count, race_name);
            }

            /* Normal lonely monsters */
            else
            {
                /* Just add a slight flavor */
                strnfmt(buf, sizeof(buf), "the %s", race_name);
            }
        }

        /* Format the message for non-viewable monsters if necessary */
        else if (!r_ptr && !action_only)
        {
            if (count > 1)
            {
                /* Show the counter */
                strnfmt(buf, sizeof(buf), "%d monsters", count);
            }
            else
            {
                /* Just one non-visible monster */
                my_strcpy(buf, "it", sizeof(buf));
            }
        }

        /* Special message. Nuke the mark */
        if (action_only) ++action;

        /* Regular message */
        else
        {
            /* Add special mark. Monster is offscreen */
            if (p->mon_msg[i].mon_flags & 0x02)
                my_strcat(buf, " (offscreen)", sizeof(buf));

            /* Add the separator */
            my_strcat(buf, " ", sizeof(buf));
        }

        /* Append the action to the message */
        my_strcat(buf, action, sizeof(buf));

        /* Capitalize the message */
        *buf = toupper((unsigned char)*buf);

        /* Hack -- Play sound for fear message */
        if (p->mon_msg[i].msg_code == MON_MSG_FLEE_IN_TERROR) sound(p, MSG_FLEE);

        /* Show the message */
        msg(p, buf);
    }
}


/*
 * Print and delete all stacked monster messages.
 */
void flush_all_monster_messages(struct player *p)
{
    /* Flush regular messages, then delayed messages */
    flush_monster_messages(p, FALSE);
    flush_monster_messages(p, TRUE);

    /* Delete all the stacked messages and history */
    p->size_mon_msg = 0;
    p->size_mon_hist = 0;
}