/*
 * File: mon-msg.c
 * Purpose: Monster message code.
 *
 * Copyright (c) 1997-2016 Jeff Greene, Andi Sidwell
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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
 * Maximum number of stacked monster messages
 */
#define MAX_STORED_MON_MSG      200
#define MAX_STORED_MON_CODES    400


/*
 * Flags for whether monsters are offscreen or invisible
 */
#define MON_MSG_FLAG_OFFSCREEN	0x01
#define MON_MSG_FLAG_INVISIBLE	0x02


/*
 * An array of monster messages in order of monster message type.
 *
 * Singular and plural modifiers are encoded in the same string. Example:
 * "[is|are] hurt" is expanded to "is hurt" if you request the singular form.
 * The string is expanded to "are hurt" if the plural form is requested.
 *
 * The singular and plural parts are optional. Example:
 * "rear[s] up in anger" only includes a modifier for the singular form.
 *
 * Any of these strings can start with "~", in which case we consider that
 * string as a whole message, not as a part of a larger message. This
 * is useful to display Moria-like death messages.
 */
static const struct
{
    const char *msg;
    bool omit_subject;
    int type;
} msg_repository[] =
{
    #define MON_MSG(x, t, o, s) {s, o, t},
    #include "../common/list-mon-message.h"
    #undef MON_MSG
    {NULL, MSG_GENERIC}
};


/*
 * Adds to the message queue a message describing a monster's reaction
 * to damage.
 */
void message_pain(struct player *p, struct monster *mon, int dam)
{
    int msg_code = MON_MSG_UNHARMED;

    /* Calculate damage levels */
    if (dam > 0)
    {
        /* Note -- subtle fix */
        long newhp = (long)(mon->hp);
        long oldhp = newhp + (long)(dam);
        long tmp = ((oldhp > 0)? ((newhp * 100L) / oldhp): 0);
        int percentage = (int)(tmp);

        if (percentage > 95) msg_code = MON_MSG_95;
        else if (percentage > 75) msg_code = MON_MSG_75;
        else if (percentage > 50) msg_code = MON_MSG_50;
        else if (percentage > 35) msg_code = MON_MSG_35;
        else if (percentage > 20) msg_code = MON_MSG_20;
        else if (percentage > 10) msg_code = MON_MSG_10;
        else msg_code = MON_MSG_0;
    }

    add_monster_message(p, mon, msg_code, false);
}


/*
 * Tracks which monster has had which pain message stored, so redundant messages
 * don't happen due to monster attacks hitting other monsters.
 * Returns true if the message is redundant.
 */
static bool redundant_monster_message(struct player *p, struct monster *mon, int msg_code)
{
    int i;

    my_assert(mon);
    my_assert(msg_code >= 0);
    my_assert(msg_code < MON_MSG_MAX);

    for (i = 0; i < p->size_mon_hist; i++)
    {
        /* Check for a matched monster & monster code */
        if ((mon == p->mon_message_hist[i].mon) && (msg_code == p->mon_message_hist[i].message_code))
            return true;
    }

    return false;
}


/*
 * Try to work out what flags a message should have from a monster name
 */
static int message_flags(struct player *p, const struct monster *mon)
{
    int flags = 0;

    if (!panel_contains(p, &((struct monster *)mon)->grid))
        flags |= MON_MSG_FLAG_OFFSCREEN;

    if (!monster_is_visible(p, mon->midx))
        flags |= MON_MSG_FLAG_INVISIBLE;

    return flags;
}


/*
 * Store the monster in the monster history for duplicate checking later
 */
static void store_monster(struct player *p, struct monster *mon, int msg_code)
{
    /* Record which monster had this message stored */
    if (p->size_mon_hist < MAX_STORED_MON_CODES)
    {
        p->mon_message_hist[p->size_mon_hist].mon = mon;
        p->mon_message_hist[p->size_mon_hist].message_code = msg_code;
        p->size_mon_hist++;
    }
}


/*
 * Try to stack a message on top of existing ones
 *
 * Returns true if successful, false if failed
 */
static bool stack_message(struct player *p, struct monster *mon, int msg_code, int flags)
{
    int i;

    for (i = 0; i < p->size_mon_msg; i++)
    {
        /* We found the race and the message code */
        if ((p->mon_msg[i].race == mon->race) &&
            (p->mon_msg[i].flags == flags) &&
            (p->mon_msg[i].msg_code == msg_code))
        {
            p->mon_msg[i].count++;
            store_monster(p, mon, msg_code);
            return true;
        }
    }

    return false;
}


static int what_delay(int msg_code, int delay)
{
    int type = msg_repository[msg_code].type;

    if (type == MSG_KILL) return 2;
    return (delay? 1: 0);
}


/*
 * Stack a codified message for the given monster race.
 *
 * Return true on success.
 */
bool add_monster_message(struct player *p, struct monster *mon, int msg_code, bool delay)
{
    int flags;

    /* Paranoia */
    if (!p) return false;

    my_assert(msg_code >= 0);
    my_assert(msg_code < MON_MSG_MAX);

    flags = message_flags(p, mon);

    /* Try to stack the message on top of older messages if it isn't redundant */
    if (redundant_monster_message(p, mon, msg_code)) return false;
    if (stack_message(p, mon, msg_code, flags)) return true;

    /* If not possible, check we have storage space for more messages and add */
    if (p->size_mon_msg < MAX_STORED_MON_MSG)
    {
        p->mon_msg[p->size_mon_msg].race = mon->race;
        p->mon_msg[p->size_mon_msg].flags = flags;
        p->mon_msg[p->size_mon_msg].msg_code = msg_code;
        p->mon_msg[p->size_mon_msg].count = 1;
        p->mon_msg[p->size_mon_msg].delay = what_delay(msg_code, delay);
        p->size_mon_msg++;

        store_monster(p, mon, msg_code);

        p->upkeep->notice |= PN_MON_MESSAGE;

        return true;
    }

    return false;
}


/*
 * Create the subject of the sentence for monster messages
 */
static void get_subject(char *buf, size_t buflen, struct monster_race *race, int count,
    bool invisible, bool offscreen)
{
    if (invisible)
    {
        if (count == 1)
            my_strcpy(buf, "It", buflen);
        else
            strnfmt(buf, buflen, "%d monsters", count);
    }
    else
    {
        /* Uniques, multiple monsters, or just one */
        if (monster_is_unique(race))
            my_strcpy(buf, race->name, buflen);
        else if (count == 1)
            strnfmt(buf, buflen, "The %s", race->name);
        else
        {
            /* Get the plural of the race name */
            if (race->plural != NULL)
                strnfmt(buf, buflen, "%d %s", count, race->plural);
            else
            {
                strnfmt(buf, buflen, "%d %s", count, race->name);
                plural_aux(buf, buflen);
            }
        }
    }

    if (offscreen)
        my_strcat(buf, " (offscreen)", buflen);

    /* Add a separator */
    my_strcat(buf, " ", buflen);
}


/* State machine constants for get_message_text() */
#define MSG_PARSE_NORMAL    0
#define MSG_PARSE_SINGLE    1
#define MSG_PARSE_PLURAL    2


/*
 * Formats a message based on the given message code and the plural flag.
 *
 * pos the position in buf to start writing the message into
 */
static void get_message_text(char *buf, size_t buflen, int msg_code,
    const struct monster_race *race, bool do_plural)
{
    const char *source;
    int state;
    size_t maxlen;
    size_t pos;
    size_t i;

    my_assert(msg_code < MON_MSG_MAX);
    my_assert(race != NULL);
    my_assert(race->base != NULL);
    my_assert(race->base->pain != NULL);

    /* Find the appropriate message */
    source = msg_repository[msg_code].msg;
    switch (msg_code)
    {
        case MON_MSG_95: source = race->base->pain->messages[0]; break;
        case MON_MSG_75: source = race->base->pain->messages[1]; break;
        case MON_MSG_50: source = race->base->pain->messages[2]; break;
        case MON_MSG_35: source = race->base->pain->messages[3]; break;
        case MON_MSG_20: source = race->base->pain->messages[4]; break;
        case MON_MSG_10: source = race->base->pain->messages[5]; break;
        case MON_MSG_0: source = race->base->pain->messages[6]; break;
    }

    state = MSG_PARSE_NORMAL;
    maxlen = strlen(source);
    pos = 0;

    /* Put the message characters in the buffer */
    for (i = 0; ((i < maxlen) && (pos < buflen - 1)); i++)
    {
        char cur = source[i];

        /*
         * The characters '[|]' switch parsing mode and are never output.
         * The syntax is [singular|plural]
         */
        if ((state == MSG_PARSE_NORMAL) && (cur == '['))
            state = MSG_PARSE_SINGLE;
        else if ((state == MSG_PARSE_SINGLE) && (cur == '|'))
            state = MSG_PARSE_PLURAL;
        else if ((state != MSG_PARSE_NORMAL) && (cur == ']'))
            state = MSG_PARSE_NORMAL;
        else if ((state == MSG_PARSE_NORMAL) ||
                ((state == MSG_PARSE_SINGLE) && (do_plural == false)) ||
                ((state == MSG_PARSE_PLURAL) && (do_plural == true)))
        {
            /* Copy the characters according to the mode */
            buf[pos++] = cur;
        }
    }

    /* We should always return to the normal state */
    my_assert(state == MSG_PARSE_NORMAL);

    /* Terminate the buffer */
    buf[pos] = 0;
}


#undef MSG_PARSE_NORMAL
#undef MSG_PARSE_SINGLE
#undef MSG_PARSE_PLURAL


/*
 * Accessor function - should we skip the monster name for this message type?
 */
static bool skip_subject(int msg_code)
{
    my_assert(msg_code >= 0);
    my_assert(msg_code < MON_MSG_MAX);

    return msg_repository[msg_code].omit_subject;
}


/*
 * Return a MSG_ type for the given message code (and monster)
 */
static int get_message_type(int msg_code, const struct monster_race *race)
{
    int type = msg_repository[msg_code].type;

    if (type == MSG_KILL)
    {
        /* Play a special sound if the monster was unique */
        if (monster_is_unique(race))
        {
            if (race->base == lookup_monster_base("Morgoth"))
                type = MSG_KILL_KING;
            else
                type = MSG_KILL_UNIQUE;
        }
    }

    return type;
}


/*
 * Show the given monster message.
 */
static void show_message(struct player *p, struct monster_race_message *msg)
{
    char subject[60] = "";
    char body[60];

    /* Some messages don't require a monster name */
    if (!skip_subject(msg->msg_code))
    {
        /* Get 'it ' or '3 monsters (offscreen) ' or '15000 snakes ' etc */
        get_subject(subject, sizeof(subject), msg->race, msg->count,
            (msg->flags & MON_MSG_FLAG_INVISIBLE), (msg->flags & MON_MSG_FLAG_OFFSCREEN));
    }

    /* Get the message proper, corrected for singular/plural etc. */
    get_message_text(body, sizeof(body), msg->msg_code, msg->race, (msg->count > 1));

    /* Show the message */
    msgt(p, get_message_type(msg->msg_code, msg->race), "%s%s", subject, body);
}


/*
 * Show and then clear all stacked monster messages.
 */
void show_monster_messages(struct player *p)
{
    int delay;

    for (delay = 0; delay < 3; delay++)
    {
        int i;

        for (i = 0; i < p->size_mon_msg; i++)
        {
            struct monster_race_message *msg = &p->mon_msg[i];

            /* Skip irrelevant entries */
            if (msg->delay == delay)
                show_message(p, msg);
        }
    }

    /* Delete all the stacked messages and history */
    p->size_mon_msg = p->size_mon_hist = 0;
}


/*
 * Displays a message describing a player's reaction to damage.
 */
void player_pain(struct player *p, struct player *who, int dam)
{
    int msg_code = MON_MSG_UNHARMED;
    char subject[NORMAL_WID];
    char body[60];

    /* Paranoia */
    if (!who) return;

    /* Get the player name */
    player_desc(p, subject, sizeof(subject), who, true);

    /* Calculate damage levels */
    if (dam > 0)
    {
        /* Note -- subtle fix */
        long newhp = (long)(who->chp);
        long oldhp = newhp + (long)(dam);
        long tmp = ((oldhp > 0)? ((newhp * 100L) / oldhp): 0);
        int percentage = (int)(tmp);

        if (percentage > 95) msg_code = MON_MSG_95;
        else if (percentage > 75) msg_code = MON_MSG_75;
        else if (percentage > 50) msg_code = MON_MSG_50;
        else if (percentage > 35) msg_code = MON_MSG_35;
        else if (percentage > 20) msg_code = MON_MSG_20;
        else if (percentage > 10) msg_code = MON_MSG_10;
        else msg_code = MON_MSG_0;
    }

    get_message_text(body, sizeof(body), msg_code, &r_info[0], false);
    msg(p, "%s %s", subject, body);
}


void monmsg_init(struct player *p)
{
    /* Array of stacked monster messages */
    p->mon_msg = mem_zalloc(MAX_STORED_MON_MSG * sizeof(*p->mon_msg));
    p->mon_message_hist = mem_zalloc(MAX_STORED_MON_CODES * sizeof(*p->mon_message_hist));
}


void monmsg_cleanup(struct player *p)
{
    /* Free the stacked monster messages */
    mem_free(p->mon_msg);
    mem_free(p->mon_message_hist);
}
