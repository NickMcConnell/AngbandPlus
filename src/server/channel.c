/*
 * File: channel.c
 * Purpose: Chat channels
 *
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


/* Chat channels */
int chan_audit;
int chan_debug;
int chan_cheat;


static const char *virt_channels[VIRTUAL_CHANNELS] = {NULL, "&say", NULL};


static int find_chat_target(const char *search, char *error, size_t maxlen)
{
    size_t len;
    int i, j, target = 0;
    const char *problem = "";
    struct player *q;
    bool party_trap = false;
    bool channel_trap = false;

    /* Acquire length of search string */
    len = strlen(search);

    /* Virtual channels? */
    if (len && search[0] == '&')
    {
        channel_trap = true;

        /* Find one */
        for (i = 1; i < VIRTUAL_CHANNELS; i++)
        {
            /* Done */
            if (!virt_channels[i]) break;

            /* Compare names */
            if (!my_strnicmp(virt_channels[i], search, len))
            {
                /* Set target if not set already or an exact match */
                if (!target || (len == strlen(virt_channels[i])))
                {
                    target = i;
                    problem = "";
                }
                else
                {
                    /* Matching too many */
                    /* Make sure we don't already have an exact match */
                    if (len != strlen(parties[0 - target].name))
                        problem = "channels";
                }
                break;
            }
        }
    }

    /* Look for a recipient who matches the search string */
    if (len && !channel_trap)
    {
        /* Check for party hinter */
        if (search[0] == '^')
        {
            party_trap = true;
            search = search + 1;
        }

        /* First check parties */
        for (i = 1; i < MAX_PARTIES; i++)
        {
            /* Skip if empty */
            if (!parties[i].num) continue;

            /* Check name */
            if (!my_strnicmp(parties[i].name, search, len))
            {
                /* Make sure one of the party members is actually logged on. */
                for (j = 1; j <= NumPlayers; j++)
                {
                    struct player *player = player_get(j);

                    /* Only connected players XXX */
                    if (player->conn == -1) continue;

                    /* Check this guy */
                    if (player_in_party(i, player))
                    {
                        /* Set target if not set already or an exact match */
                        if (!target || (len == strlen(parties[i].name)))
                        {
                            target = 0 - i;
                            problem = "";
                        }
                        else
                        {
                            /* Matching too many parties */
                            /* Make sure we don't already have an exact match */
                            if (len != strlen(parties[0 - target].name))
                                problem = "parties";
                        }
                        break;
                    }
                }
            }
        }

        /* Was hinting at party, Ignore players */
        if (!party_trap)
        {
            /* Then check players */
            for (i = 1; i <= NumPlayers; i++)
            {
                /* Check this one */
                q = player_get(i);

                /* Skip DM */
                if (q->dm_flags & DM_SECRET_PRESENCE) continue;

                /* Check name */
                if (!my_strnicmp(q->name, search, len))
                {
                    /* Set target if not set already or an exact match */
                    if (!target || (len == strlen(q->name)))
                    {
                        target = i;
                        problem = "";
                    }

                    /* Matching too many people */
                    else if (target > 0)
                    {
                        /* Make sure we don't already have an exact match */
                        if (len != strlen(player_get(target)->name))
                            problem = "players";
                    }
                    else
                        problem = "players or parties";
                }
            }
        }
    }

    /* Check for recipient set but no match found */
    if (len && !target)
    {
        /* Prepare an error message */
        strnfmt(error, maxlen, "Could not match name '%s'.", search);

        /* Give up */
        return 0;
    }

    /* Check for multiple recipients found */
    if (!STRZERO(problem))
    {
        /* Send an error message */
        strnfmt(error, maxlen, "'%s' matches too many %s.", search, problem);

        /* Give up */
        return 0;
    }

    /* Hack -- pack player targets and virtual channels together */
    if (target > 0 && !channel_trap) target += VIRTUAL_CHANNELS;

    return target;
}


static void assist_whisper(struct player *p, const char *search)
{
    int target;
    char error[NORMAL_WID];

    target = find_chat_target(search, error, sizeof(error));

    /* No match */
    if (!target)
    {
        /* Relay error */
        msg(p, error);
    }

    /* Virtual channel -- what he sent */
    else if (target > 0 && target < VIRTUAL_CHANNELS)
        Send_channel(p, 255, virt_channels[target]);

    /* A Player */
    else if (target > 0)
        Send_channel(p, 255, player_get(target - VIRTUAL_CHANNELS)->name);

    /* A Party */
    else if (target < 0)
        Send_channel(p, 255, parties[0 - target].name);
}


void channel_join(struct player *p, const char *channel, bool quiet)
{
    int i, last_free = 0;

    /* Find channel */
    for (i = 0; i < MAX_CHANNELS; i++)
    {
        if (!last_free && STRZERO(channels[i].name)) last_free = i;

        /* Name match */
        if (!strcmp(channels[i].name, channel))
        {
            /* Not present on this channel */
            if (!on_channel(p, i))
            {
                /* Hack -- can't join due to modes? */
                if (((channels[i].mode & CM_KEYLOCK) && !is_dm_p(p)) ||
                    (p->on_channel[i] & UCM_BAN))
                {
                    /* Hack -- route to "unable to join" message */
                    last_free = 0;
                    break;
                }

                /* Enter channel */
                channels[i].num++;
                p->on_channel[i] |= UCM_EAR;
                Send_channel(p, (byte)i, NULL);
                if (!quiet) msg(p, "Listening to channel %s", channel);
            }

            /* Select channel */
            else
            {
                p->main_channel = i;
                Send_channel(p, (byte)i, "");
                if (!quiet) msg(p, "Channel changed to %s", channel);
            }
            return;
        }
    }

    /* No such channel */

    /* We have free space */
    if (last_free)
    {
        /* Create channel */
        my_strcpy(channels[last_free].name, channel, sizeof(channels[0].name));
        channels[last_free].num = 1;
        p->on_channel[last_free] |= (UCM_EAR | UCM_OPER);
        Send_channel(p, (byte)last_free, false);
        if (!quiet) msg(p, "Listening to channel %s", channel);
    }

    /* All channel slots are used up */
    else
    {
        if (!quiet) msg(p, "Unable to join channel %s", channel);
    }
}


/* Actual code for leaving channels */
static void channel_leave_id(struct player *p, int i, bool quiet)
{
    if (!i || !(p->on_channel[i] & UCM_EAR)) return;

    channels[i].num--;
    if (!quiet) msg(p, "Left channel %s", channels[i].name);
    if (channels[i].num <= 0 && !(channels[i].mode & CM_SERVICE))
    {
        channels[i].name[0] = '\0';
        channels[i].id = 0;
    }
    if (p->main_channel == i)
        p->main_channel = 0;
    p->on_channel[i] &= ~(UCM_LEAVE);
    if (!quiet)
        Send_channel(p, (byte)i, "-");
}


/* Find channel by name and leave it */
void channel_leave(struct player *p, const char *channel)
{
    int i;

    for (i = 0; i < MAX_CHANNELS; i++)
    {
        if (!strcmp(channels[i].name, channel))
        {
            channel_leave_id(p, i, false);
            break;
        }
    }
}


/* Leave all channels */
void channels_leave(struct player *p)
{
    int i;

    for (i = 0; i < MAX_CHANNELS; i++)
    {
        if (p->on_channel[i] & UCM_EAR)
            channel_leave_id(p, i, true);
    }
}


static void msg_format_type(struct player *p, u16b type, const char *fmt, ...)
{
    va_list vp;
    char buf[MSG_LEN];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    vstrnfmt(buf, MSG_LEN, fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Display */
    msg_print(p, buf, type);
}


/*
 * A message prefixed by a player name is sent only to that player.
 * Otherwise, it is sent to everyone.
 */
static void player_talk_aux(struct player *p, const char *message)
{
    struct player *q;
    int i, target = 0;
    char search[NORMAL_WID], sender[NORMAL_WID], error[NORMAL_WID];
    char tmp_chan[MAX_CHAN_LEN];
    const char *colon, *chan_prefix;
    bool msg_off = false;
    int dest_chan;

    /* Get sender's name */
    if (p)
    {
        /* Get player name */
        my_strcpy(sender, p->name, sizeof(sender));
    }
    else
    {
        /* Default name */
        my_strcpy(sender, "", sizeof(sender));
    }

    /* Default to no search string */
    my_strcpy(search, "", sizeof(search));

    /* Default to #public channel if not originated by a player */
    dest_chan = 0;

    if (p)
    {
        /* Default to the senders main channel */
        dest_chan = p->main_channel;

        /* Set search string from senders secondary channel */
        my_strcpy(search, p->second_channel, sizeof(search));
    }

    /* Is the message destined for a particular channel? */
    if (strchr("#", *message))
    {
        /* Yes, examine in more detail */
        chan_prefix = strchr(message, ' ');
        if (!chan_prefix && strlen(message) < MAX_CHAN_LEN)
        {
            /* Channel name only?  Change the players default channel */
            if (p)
            {
                my_strcpy(tmp_chan, message, sizeof(tmp_chan));
                channel_join(p, tmp_chan, false);
                return;
            }
        }
        else if (!chan_prefix || (chan_prefix - message >= MAX_CHAN_LEN))
        {
            /* Invalid channel prefix?  Forget about the channel. */
        }
        else
        {
            /* Channel name followed by text? Extract the channel name */
            my_strcpy(tmp_chan, message, chan_prefix - message + 1);
            dest_chan = -1;
            for (i = 0; i < MAX_CHANNELS; i++)
            {
                if (!strcmp(channels[i].name, tmp_chan))
                {
                    dest_chan = i;
                    break;
                }
            }
            message += (chan_prefix - message) + 1;

            /* Forget about search string */
            msg_off = false;
            my_strcpy(search, "", sizeof(search));
        }
    }

    /* Look for a player's name followed by a colon */
    colon = strchr(message, ':');

    /* Pretend colon wasn't there */
    if (colon)
    {
        /* Messenger is undefined OR colon is last symbol OR colon is part of "smiley" */
        if (!p || !*(colon + 1) || strchr(")(-|\\/", *(colon + 1))) colon = NULL;
    }

    /* Form a search string if we found a colon */
    if (colon)
    {
        /* Copy everything up to the colon to the search string */
        my_strcpy(search, message, colon - message + 1);

        /* Move colon pointer forward to next word */
        while (*colon && (isspace(*colon) || *colon == ':')) colon++;

        /* Offset message */
        msg_off = true;
    }

    /* Find special target */
    if (strlen(search))
    {
        /* There's nothing else, prepare for whisper */
        if ((size_t)(colon - message) == strlen(message))
        {
            assist_whisper(p, search);
            return;
        }

        /* Hack -- empty 'party hinter' hints to own party */
        if ((search[0] == '^') && p && p->party && (search[1] == '\0'))
            my_strcpy(search, parties[p->party].name, sizeof(search));

        if (!(target = find_chat_target(search, error, sizeof(error))))
        {
            /* Error */
            if (p) msg(p, error);

            /* Done */
            return;
        }
    }

    /* No need to offset message */
    if (!msg_off) colon = message;

    /* Send to a virtual channel */
    if (target > 0)
    {
        /* Make sure it's a channel, not player */
        if (target < VIRTUAL_CHANNELS)
        {
            const char *verb = "say";
            char punct = '.';
            char mssg[60];

            my_strcpy(mssg, colon, sizeof(mssg));
            switch (target)
            {
                case 1: /* "&say" */
                {
                    for (i = strlen(mssg) - 1; i > 0; i--)
                    {
                        switch (mssg[i])
                        {
                            case ' ': continue;
                            case '?': verb = "ask";
                            case '!':
                            case '.': punct = mssg[i]; mssg[i] = '\0';
                            default: break;
                        }
                        break;
                    }

                    /* Send somewhere */
                    msg_format_type(p, MSG_TALK, "You %s, \"%s\"%c", verb, mssg, punct);
                    if (p)
                    {
                        msg_format_complex_near(p, MSG_TALK, "%s %ss, \"%s\"%c", sender, verb,
                            mssg, punct);
                    }

                    break;
                }
            }

            return;
        }

        /* It was a player */
        else target -= VIRTUAL_CHANNELS;
    }

    /* Send to appropriate player */
    if (target > 0)
    {
        /* Set target player */
        q = player_get(target);

        /* Send message to target */
        msg_format_type(q, MSG_WHISPER, "[%s:%s] %s", q->name, sender, colon);

        /* Also send back to sender */
        msg_format_type(p, MSG_WHISPER, "[%s:%s] %s", q->name, sender, colon);

        /* Done */
        return;
    }

    /* Send to appropriate party */
    if (target < 0)
    {
        /* Send message to target party */
        party_msg_format(0 - target, "[%s:%s] %s", parties[0 - target].name, sender, colon);

        /* Also send back to sender if the sender is not in the party being messaged. */
        if (p && (p->party != 0 - target))
            msg(p, "[%s:%s] %s", parties[0 - target].name, sender, colon);

        /* Done */
        return;
    }

    /* Total failure... */
    if (dest_chan == -1) return;
    if (p && !can_talk(p, dest_chan)) return;

    /* Send to everyone in this channel */
    for (i = 1; i <= NumPlayers; i++)
    {
        q = player_get(i);
        if (q->on_channel[dest_chan] & UCM_EAR)
        {
            /* Send message */
            if (p)
                msg_format_type(q, MSG_CHAT + dest_chan, "[%s] %s", sender, message);
            else
                msg_format_type(q, MSG_CHAT + dest_chan, "%s", message);
        }
    }

    /* Send to the console too */
    console_print(format("[%s] %s", sender, message), dest_chan);
}


/*
 * A player has sent a message to the rest of the world.
 *
 * Parse it and send to everyone or to only the person(s) he requested.
 *
 * Note that more than one message may get sent at once, separated by
 * tabs ('\t').  Thus, this function splits them and calls
 * "player_talk_aux" to do the dirty work.
 */
void do_cmd_message(struct player *p, char *message)
{
    char *cur, *next;

    /* Start at the beginning */
    cur = message;

    /* Process until out of messages */
    while (cur)
    {
        /* Find the next tab */
        next = strchr(cur, '\t');

        /* Stop out the tab */
        if (next)
        {
            /* Replace with \0 */
            *next = '\0';
        }

        /* Process this message */
        player_talk_aux(p, cur);

        /* Move to the next one */
        if (next)
        {
            /* One step past the \0 */
            cur = next + 1;
        }
        else
        {
            /* No more message */
            cur = NULL;
        }
    }
}


void do_cmd_chat(struct player *p, char *buf)
{
    if (buf[0] == '-')
        channel_leave(p, buf + 1);
    else if (buf[0] == '#')
    {
        channel_join(p, buf, true);

        /* Hack -- Secondary channel */
        p->second_channel[0] = '\0';
    }
    else
        my_strcpy(p->second_channel, buf, sizeof(p->second_channel));
}