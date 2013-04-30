/*
 * File: s-util.c
 * Purpose: Utility functions
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "monster/mon-util.h"
#include "netserver.h"


/*
 * For those systems that don't have "stricmp()"
 *
 * Compare the two strings "a" and "b" ala "strcmp()" ignoring case.
 */
int stricmp(const char *a, const char *b)
{
    const char *s1, *s2;
    char z1, z2;

    /* Scan the strings */
    for (s1 = a, s2 = b; TRUE; s1++, s2++)
    {
        z1 = FORCEUPPER(*s1);
        z2 = FORCEUPPER(*s2);
        if (z1 < z2) return (-1);
        if (z1 > z2) return (1);
        if (!z1) return (0);
    }
}


char color_attr_to_char(int a)
{
    if (a == TERM_MULTI) return 'v';
    if ((a < TERM_DARK) || (a > TERM_DEEP_L_BLUE)) return 'w';
    return color_table[a].index_char;
}


/*
 * Mega-Hack -- Make a (relevant?) sound
 */
void sound(struct player *p, int val)
{
    /* Don't repeat current sound */
    if (val == p->current_sound) return;

    /* Don't play too many sounds */
    if (p->current_sound == -3) return;

    /* When too many sounds, only play the first one */
    if (p->current_sound == -2) p->current_sound = -3;
    else p->current_sound = val;

    /* Make a sound */
    Send_sound(p, val);
}


/*
 * Allow user to "prevent" certain choices
 * "!*" prevents everything
 */
bool inscription_prevent(quark_t quark, char what, bool is_harmless)
{
    const char *str, *s;

    /* Get inscription */
    str = quark_str(quark);
    if (!str) return FALSE;

    /* Check for a "prevention" inscription */
    str = strchr(str, '!');
    if (!str) return FALSE;

    /* Allow user to "prevent" certain choices */
    for (s = str + 1; *s; s++)
    {
        /* Allow user to "prevent" all choices */
        if ((*s == '*') && !is_harmless) return TRUE;

        /* Exact match */
        if (*s == what) return TRUE;

        /* Stop at the first non-letter character */
        if (!isalpha(*s) && !strchr("{!}", *s)) return FALSE;
    }

    /* Allow it */
    return FALSE;
}


/*
 * Output a message to the top line of the screen.
 *
 * Break long messages into multiple pieces (40-72 chars).
 *
 * Allow multiple short messages to "share" the top line.
 *
 * Prompt the user to make sure he has a chance to read them.
 *
 * These messages are memorized for later reference (see above).
 *
 * We could do a "Term_fresh()" to provide "flicker" if needed.
 *
 * We must be very careful about using the "msg()" functions without
 * explicitly calling the special "msg(NULL)" function, since this may
 * result in the loss of information if the screen is cleared, or if anything
 * is displayed on the top line.
 *
 * Hack -- Note that "msg(NULL)" will clear the top line even if no
 * messages are pending.
 */
void msg_print_aux(struct player *p, const char *msg, u16b type)
{
    bool log = TRUE;
    bool add = FALSE;
    bool dup = FALSE;
    char multiplier[12];
    s16b ptr;

    /* Excludes all channels but #public from the log file */
    if (type > MSG_CHAT) log = FALSE;

    if (msg)
    {
        /* We don't need to log *everything* */
        if (strchr("[", *msg)) log = FALSE;

        /*
         * Log messages for each player, so we can dump last messages
         * in server-side character dumps
         */
        if (p && log)
        {
            add = TRUE;

            /* Ensure we know where the last message is */
            ptr = p->msg_hist_ptr - 1;
            if (ptr < 0) ptr = MAX_MSG_HIST - 1;

            /* If this message is already in the buffer, count it as a dupe */
            if (!strcmp(p->msg_log[ptr], msg))
            {
                p->msg_hist_dupe++;

                /* And don't add another copy to the buffer */
                add = FALSE;
                dup = TRUE;
            }

            /* This message is the end of a series of dupes */
            else if (p->msg_hist_dupe > 0)
            {
                /* Add the dupe counter to the end of the last message */
                strnfmt(multiplier, sizeof(multiplier), " (x%d)", p->msg_hist_dupe + 1);
                my_strcat(p->msg_log[ptr], multiplier, sizeof(p->msg_log[0]));
                p->msg_hist_dupe = 0;
            }

            if (add)
            {
                /* Standard, unique (for the moment) message */
                my_strcpy(p->msg_log[p->msg_hist_ptr], msg, NORMAL_WID - 1);
                p->msg_hist_ptr++;
            }

            /* Maintain a circular buffer */
            if (p->msg_hist_ptr == MAX_MSG_HIST)
                p->msg_hist_ptr = 0;

            /* Log the message */
            plog_fmt("%s: %s", p->name, msg);
        }
        else if (log)
        {
            /* Log the message */
            plog(msg);
        }
    }

    /* Hack -- Repeated message of the same type */
    if (dup && (type == p->msg_last_type))
    {
        /* Send a SPACE character instead */
        Send_message(p, " ", type);
        return;
    }

    /* Paranoia */
    if (!p) return;

    /* Last type sent */
    p->msg_last_type = type;

    /* Ahh, the beautiful simplicity of it... */
    Send_message(p, msg, type);
}


void msg_broadcast(struct player *p, const char *msg)
{
    int i;

    /* Tell every player */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Skip the specified player */
        if (p_ptr == p) continue;

        /* Tell this one */
        msg_print_aux(p_ptr, msg, MSG_CHAT);
    }

    /* Send to console */
    console_print((char*)msg, 0);
}


/*
 * Display a formatted message, using "vstrnfmt()" and "msg_print_aux()".
 */
void msg(struct player *p, const char *fmt, ...)
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
    msg_print_aux(p, buf, MSG_GENERIC);
}


/*
 * Display a message to everyone who is in sight of another player.
 *
 * This is mainly used to keep other players advised of actions done
 * by a player. The message is not sent to the player who performed
 * the action.
 */
void msg_print_complex_near(struct player *p, struct player *q, u16b type, const char *msg)
{
    int y, x, i;

    /* Extract player's location */
    y = p->py;
    x = p->px;

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Check this player */
        player_type *q_ptr = player_get(i);

        /* Don't send the message to the player who caused it */
        if (p == q_ptr) continue;

        /* Don't send the message to the second ignoree */
        if (q == q_ptr) continue;

        /* Make sure this player is at this depth */
        if (q_ptr->depth != p->depth) continue;

        /* Can he see this player? */
        if (player_has_los_bold(q_ptr, y, x))
        {
            /* Send the message */
            msg_print_aux(q_ptr, msg, type);
        }
    }
}


/*
 * Same as above, except send a formatted message.
 */
void msg_format_complex_near(struct player *p, u16b type, const char *fmt, ...)
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
    msg_print_complex_near(p, p, type, buf);
}


/*
 * Display a message to everyone who is in sight of another player.
 *
 * The content of the message will depend on whether or not the player is visible.
 * The function supposes that the message is in the form "(foo) does something..."
 */
void msg_print_near(struct player *p, u16b type, const char *msg)
{
    char p_name[NORMAL_WID], buf[NORMAL_WID];
    int y, x, i;

    /* Extract player's location */
    y = p->py;
    x = p->px;

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Check this player */
        player_type *q_ptr = player_get(i);

        /* Don't send the message to the player who caused it */
        if (p == q_ptr) continue;

        /* Make sure this player is at this depth */
        if (q_ptr->depth != p->depth) continue;

        /* Can he see this player? */
        if (player_has_los_bold(q_ptr, y, x))
        {
            player_desc(q_ptr, p_name, sizeof(p_name), p, TRUE);
            strnfmt(buf, sizeof(buf), "%s%s", p_name, msg);

            /* Send the message */
            msg_print_aux(q_ptr, buf, type);
        }
    }
}


/*
 * Same as above, except send a formatted message.
 */
void msg_format_near(struct player *p, u16b type, const char *fmt, ...)
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
    msg_print_near(p, type, buf);
}


static void msg_format_type(int Ind, u16b type, const char *fmt, ...)
{
    player_type *p_ptr = player_get(Ind);
    va_list vp;
    char buf[MSG_LEN];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    vstrnfmt(buf, MSG_LEN, fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Display */
    msg_print_aux(p_ptr, buf, type);
}


#define VIRTUAL_CHANNELS 8
const char *virt_channels[] = {NULL, "&say", NULL};

static int find_chat_target(const char *search, char *error, size_t maxlen)
{
    int i, j, len, target = 0;
    const char *problem = "";
    player_type *q_ptr;
    bool party_trap = FALSE;
    bool channel_trap = FALSE;

    /* Acquire length of search string */
    len = strlen(search);

    /* Virtual channels? */
    if (len && search[0] == '&')
    {
        channel_trap = TRUE;

        /* Find one */
        for (i = 1; i < VIRTUAL_CHANNELS; i++)
        {
            /* Done */
            if (!virt_channels[i]) break;

            /* Compare names */
            if (!strncasecmp(virt_channels[i], search, len))
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
            party_trap = TRUE;
            search = search + 1;
        }

        /* First check parties */
        for (i = 1; i < MAX_PARTIES; i++)
        {
            /* Skip if empty */
            if (!parties[i].num) continue;

            /* Check name */
            if (!strncasecmp(parties[i].name, search, len))
            {
                /* Make sure one of the party members is actually logged on. */
                for (j = 1; j <= NumPlayers; j++)
                {
                    player_type *j_ptr = player_get(j);

                    if (j_ptr->conn == NOT_CONNECTED) continue;

                    /* Check this guy */
                    if (player_in_party(i, j_ptr))
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
                q_ptr = player_get(i);

                /* Skip DM */
                if (q_ptr->dm_flags & DM_SECRET_PRESENCE) continue;

                /* Check name */
                if (!strncasecmp(q_ptr->name, search, len))
                {
                    /* Set target if not set already or an exact match */
                    if (!target || (len == strlen(q_ptr->name)))
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


static void assist_whisper(int Ind, const char *search)
{
    player_type *p_ptr = player_get(Ind);
    int target;
    char error[NORMAL_WID];

    target = find_chat_target(search, error, sizeof(error));

    /* No match */
    if (!target)
    {
        /* Relay error */
        msg(p_ptr, error);
    }

    /* Virtual channel -- what he sent */
    else if (target > 0 && target < VIRTUAL_CHANNELS)
        Send_channel(Ind, 255, virt_channels[target]);

    /* A Player */
    else if (target > 0)
        Send_channel(Ind, 255, player_get(target - VIRTUAL_CHANNELS)->name);

    /* A Party */
    else if (target < 0)
        Send_channel(Ind, 255, parties[0 - target].name);
}


void channel_join(int Ind, const char *channel, bool quiet)
{
    player_type *p_ptr = player_get(Ind);
    int i, last_free = 0;

    /* Find channel */
    for (i = 0; i < MAX_CHANNELS; i++)
    {
        if (!last_free && STRZERO(channels[i].name)) last_free = i;

        /* Name match */
        if (!strcmp(channels[i].name, channel))
        {
            /* Not present on this channel */
            if (!on_channel(p_ptr, i))
            {
                /* Hack -- can't join due to modes? */
                if (((channels[i].mode & CM_KEYLOCK) && !is_dm_p(p_ptr)) ||
                    (p_ptr->on_channel[i] & UCM_BAN))
                {
                    /* Hack -- route to "unable to join" message */
                    last_free = 0;
                    break;
                }

                /* Enter channel */
                channels[i].num++;
                p_ptr->on_channel[i] |= UCM_EAR;
                Send_channel(Ind, (byte)i, NULL);
                if (!quiet) msg(p_ptr, "Listening to channel %s", channel);
            }

            /* Select channel */
            else
            {
                p_ptr->main_channel = i;
                Send_channel(Ind, (byte)i, "");
                if (!quiet) msg(p_ptr, "Channel changed to %s", channel);
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
        p_ptr->on_channel[last_free] |= (UCM_EAR | UCM_OPER);
        Send_channel(Ind, (byte)last_free, FALSE);
        if (!quiet) msg(p_ptr, "Listening to channel %s", channel);
    }

    /* All channel slots are used up */
    else
    {
        if (!quiet) msg(p_ptr, "Unable to join channel %s", channel);
    }
}


/* Actual code for leaving channels */
static void channel_leave_id(int Ind, int i, bool quiet)
{
    player_type *p_ptr = player_get(Ind);

    if (!i || !(p_ptr->on_channel[i] & UCM_EAR)) return;

    channels[i].num--;
    if (!quiet) msg(p_ptr, "Left channel %s", channels[i].name);
    if (channels[i].num <= 0 && !(channels[i].mode & CM_SERVICE))
    {
        channels[i].name[0] = '\0';
        channels[i].id = 0;
    }
    if (p_ptr->main_channel == i)
        p_ptr->main_channel = 0;
    p_ptr->on_channel[i] &= ~(UCM_LEAVE);
    if (!quiet)
        Send_channel(Ind, (byte)i, "-");
}


/* Find channel by name and leave it */
void channel_leave(int Ind, const char *channel)
{
    int i;

    for (i = 0; i < MAX_CHANNELS; i++)
    {
        if (!strcmp(channels[i].name, channel))
        {
            channel_leave_id(Ind, i, FALSE);
            break;
        }
    }
}


/* Leave all channels */
void channels_leave(int Ind)
{
    int i;
    player_type *p_ptr = player_get(Ind);

    for (i = 0; i < MAX_CHANNELS; i++)
    {
        if (p_ptr->on_channel[i] & UCM_EAR)
            channel_leave_id(Ind, i, TRUE);
    }
}


/*
 * A message prefixed by a player name is sent only to that player.
 * Otherwise, it is sent to everyone.
 */
static void player_talk_aux(int Ind, const char *message)
{
    player_type *p_ptr = player_get(Ind), *q_ptr;
    int i, target = 0;
    char search[NORMAL_WID], sender[NORMAL_WID], error[NORMAL_WID];
    char tmp_chan[MAX_CHAN_LEN];
    const char *colon, *chan_prefix;
    bool msg_off = FALSE;
    int dest_chan;

    /* Get sender's name */
    if (p_ptr)
    {
        /* Get player name */
        my_strcpy(sender, p_ptr->name, sizeof(sender));
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

    if (p_ptr)
    {
        /* Default to the senders main channel */
        dest_chan = p_ptr->main_channel;

        /* Set search string from senders secondary channel */
        my_strcpy(search, p_ptr->second_channel, sizeof(search));
    }

    /* Is the message destined for a particular channel? */
    if (strchr("#", *message))
    {
        /* Yes, examine in more detail */
        chan_prefix = strchr(message, ' ');
        if (!chan_prefix && strlen(message) < MAX_CHAN_LEN)
        {
            /* Channel name only?  Change the players default channel */
            if (p_ptr)
            {
                my_strcpy(tmp_chan, message, sizeof(tmp_chan));
                channel_join(Ind, tmp_chan, FALSE);
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
            msg_off = FALSE;
            my_strcpy(search, "", sizeof(search));
        }
    }

    /* Look for a player's name followed by a colon */
    colon = strchr(message, ':');

    /* Pretend colon wasn't there */
    if (colon)
    {
        /* Messenger is undefined OR colon is last symbol OR colon is part of "smiley" */
        if (!p_ptr || !*(colon + 1) || strchr(")(-|\\/", *(colon + 1))) colon = NULL;
    }

    /* Form a search string if we found a colon */
    if (colon)
    {
        /* Copy everything up to the colon to the search string */
        my_strcpy(search, message, colon - message + 1);

        /* Move colon pointer forward to next word */
        while (*colon && (isspace(*colon) || *colon == ':')) colon++;

        /* Offset message */
        msg_off = TRUE;
    }

    /* Find special target */
    if (strlen(search))
    {
        /* There's nothing else, prepare for whisper */
        if (colon - message == strlen(message))
        {
            assist_whisper(Ind, search);
            return;
        }

        /* Hack -- empty 'party hinter' hints to own party */
        if ((search[0] == '^') && p_ptr && p_ptr->party && (search[1] == '\0'))
            my_strcpy(search, parties[p_ptr->party].name, sizeof(search));

        if (!(target = find_chat_target(search, error, sizeof(error))))
        {
            /* Error */
            if (p_ptr) msg(p_ptr, error);

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
                    msg_format_type(Ind, MSG_TALK, "You %s, \"%s\"%c", verb, mssg, punct);
                    if (p_ptr)
                    {
                        msg_format_complex_near(p_ptr, MSG_TALK, "%s %ss, \"%s\"%c", sender, verb,
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
        q_ptr = player_get(target);

        /* Send message to target */
        msg_format_type(target, MSG_WHISPER, "[%s:%s] %s", q_ptr->name, sender, colon);

        /* Also send back to sender */
        msg_format_type(Ind, MSG_WHISPER, "[%s:%s] %s", q_ptr->name, sender, colon);

        /* Done */
        return;
    }

    /* Send to appropriate party */
    if (target < 0)
    {
        /* Send message to target party */
        party_msg_format(0 - target, "[%s:%s] %s", parties[0 - target].name, sender, colon);

        /* Also send back to sender if the sender is not in the party being messaged. */
        if (p_ptr && (p_ptr->party != 0 - target))
            msg(p_ptr, "[%s:%s] %s", parties[0 - target].name, sender, colon);

        /* Done */
        return;
    }

    /* Total failure... */
    if (dest_chan == -1) return;
    if (p_ptr && !can_talk(p_ptr, dest_chan)) return;

    /* Send to everyone in this channel */
    for (i = 1; i <= NumPlayers; i++)
    {
        q_ptr = player_get(i);
        if (q_ptr->on_channel[dest_chan] & UCM_EAR)
        {
            /* Send message */
            if (Ind)
                msg_format_type(i, MSG_CHAT + dest_chan, "[%s] %s", sender, message);
            else
                msg_format_type(i, MSG_CHAT + dest_chan, "%s", message);
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
 * Note that more than one message may get sent at once, seperated by
 * tabs ('\t').  Thus, this function splits them and calls
 * "player_talk_aux" to do the dirty work.
 */
void do_cmd_message(int Ind, char *message)
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
        player_talk_aux(Ind, cur);

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


void text_out_init(struct player *p)
{
    p->info_x = 0;
    p->info_y = 0;
}


static void text_out_line_end(struct player *p)
{
    /* Fill the rest of the line with spaces */
    while (p->info_x < NORMAL_WID)
    {
        p->info[p->info_y][p->info_x].a = TERM_WHITE;
        p->info[p->info_y][p->info_x].c = ' ';
        p->info_x++;
    }
}


void text_out_done(struct player *p)
{
    /* Flush */
    text_out(p, "\n");
    if (p->info_y == MAX_TXT_INFO)
        p->last_info_line = MAX_TXT_INFO - 1;
    else
    {
        text_out_line_end(p);
        p->last_info_line = p->info_y;
    }
}


/*
 * Write text and apply line-wrapping.
 *
 * Long lines will be wrapped at column 75 or at a newline character.
 * Note that punctuation can sometimes be placed one column beyond the wrap limit.
 *
 * You must be careful to end all file output with a newline character
 * to "flush" the stored line position.
 */
static void text_out_aux(struct player *p, byte a, const char *str)
{
    const char *s;
    char buf[MSG_LEN];

    /* Check limit */
    if (p->info_y == MAX_TXT_INFO) return;

    /* Copy to a rewriteable string */
    my_strcpy(buf, str, MSG_LEN);

    /* Current location within "buf" */
    s = buf;

    /* Process the string */
    while (*s)
    {
        int n = 0;
        int len = NORMAL_WID - 5 - p->info_x;
        int l_space = -1;

        /* Paranoia */
        if (len < 0) len = 0;

        /* Find length of line up to next newline or end-of-string */
        while ((n < len) && !((s[n] == '\n') || (s[n] == '\0')))
        {
            /* Mark the most recent space in the string */
            if (s[n] == ' ') l_space = n;

            /* Increment */
            n++;
        }

        /* If we have encountered no spaces */
        if ((l_space == -1) && (n == len))
        {
            /* If we are at the start of a new line */
            if (p->info_x == 0) len = n;

            /* Hack - Output punctuation at the end of the line */
            else if ((s[0] == ' ') || (s[0] == ',') || (s[0] == '.')) len = 1;

            else
            {
                /* Begin a new line */
                text_out_line_end(p);
                p->info_y++;

                /* Reset */
                p->info_x = 0;

                /* Check limit */
                if (p->info_y == MAX_TXT_INFO) return;

                continue;
            }
        }
        else
        {
            /* Wrap at the newline */
            if ((s[n] == '\n') || (s[n] == '\0')) len = n;

            /* Wrap at the last space */
            else len = l_space;
        }

        /* Write that line to file */
        for (n = 0; n < len; n++)
        {
            /* Write out the character */
            p->info[p->info_y][p->info_x].a = a;
            p->info[p->info_y][p->info_x].c = s[n];

            /* Increment */
            p->info_x++;
        }

        /* Move 's' past the stuff we've written */
        s += len;

        /* If we are at the end of the string, end */
        if (*s == '\0') return;

        /* Skip newlines */
        if (*s == '\n') s++;

        /* Begin a new line */
        text_out_line_end(p);
        p->info_y++;

        /* Reset */
        p->info_x = 0;

        /* Check limit */
        if (p->info_y == MAX_TXT_INFO) return;

        /* Skip whitespace */
        while (*s == ' ') s++;
    }
}


void text_out(struct player *p, const char *fmt, ...)
{
    char buf[MSG_LEN];
    va_list vp;

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Do the va_arg fmt to the buffer */
    vstrnfmt(buf, sizeof(buf), fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Output now */
    text_out_aux(p, TERM_WHITE, buf);
}


void text_out_c(struct player *p, byte a, const char *fmt, ...)
{
    char buf[MSG_LEN];
    va_list vp;

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Do the va_arg fmt to the buffer */
    vstrnfmt(buf, sizeof(buf), fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Output now */
    text_out_aux(p, a, buf);
}


void do_cmd_chat(int Ind, char *buf)
{
    player_type *p_ptr = player_get(Ind);

    if (buf[0] == '-')
        channel_leave(Ind, buf + 1);
    else if (buf[0] == '#')
    {
        channel_join(Ind, buf, TRUE);

        /* Hack: Secondary channel */
        p_ptr->second_channel[0] = '\0';
    }
    else
        my_strcpy(p_ptr->second_channel, buf, sizeof(p_ptr->second_channel));
}


/*
 * Display a formatted message and play the associated sound.
 */
void msgt(struct player *p, unsigned int type, const char *fmt, ...)
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
    sound(p, type);
    msg_print_aux(p, buf, type);
}


/*
 * Print the queued messages.
 */
void message_flush(struct player *p)
{
    msg_print_aux(p, NULL, MSG_GENERIC);
}


void msg_channel(int chan, const char *msg)
{
    int i;

    /* Log to file */
    if (channels[chan].mode & CM_PLOG) plog(msg);

    /* Tell every player */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        if (p_ptr->on_channel[chan] & UCM_EAR)
            msg_print_aux(p_ptr, msg, MSG_CHAT + chan);
    }

    /* And every console */
    console_print((char*)msg, chan);
}


#define end_of_segment(A) ((A) == ' ' || (A) == '!' || (A) == '@' || (A) == '^')

/*
 * Parse item's inscriptions, extract "^abc" and "^a ^b ^c"
 * cases and cache them.
 */
void fill_prevent_inscription(bool *arr, quark_t quark)
{
    const char *ax;

    /* Init quark */
    ax = quark_str(quark);
    if (ax == NULL) return;

    /* Find start of segment */
    while ((ax = strchr(ax, '^')) != NULL)
    {
        /* Parse segment */
        while (ax++ != NULL)
        {
            /* Reached end of quark, stop */
            if (*ax == 0) break;

            /* Reached end of segment, stop */
            if (end_of_segment(*ax)) break;

            /* Found a "Preventing Inscription" */
            arr[MIN(127, (byte)(*ax))] = TRUE;
        }
    }
}


/*
 * Refresh combined list of player's preventive inscriptions
 * after an update to his equipment was made.
 */
void update_prevent_inscriptions(struct player *p)
{
    object_type *o_ptr;
    int i;

    /* Clear flags */
    for (i = 0; i < 128; i++) p->prevents[i] = FALSE;

    /* Scan equipment */
    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        o_ptr = &p->inventory[i];

        /* Item exists and has inscription */
        if (o_ptr->tval && o_ptr->note)
        {
            /* Fill */
            fill_prevent_inscription(p->prevents, o_ptr->note);
        }
    }
}


void alloc_info_icky(struct player *p)
{
    int i, j;

    if (p->info_icky) return;

    p->last_info_line_icky = p->last_info_line;
    p->info_icky = C_ZNEW(p->last_info_line_icky + 1, cave_view_type*);
    for (i = 0; i <= p->last_info_line_icky; i++)
    {
        p->info_icky[i] = C_ZNEW(NORMAL_WID, cave_view_type);
        for (j = 0; j < NORMAL_WID; j++)
        {
            p->info_icky[i][j].a = p->info[i][j].a;
            p->info_icky[i][j].c = p->info[i][j].c;
        }
    }
}


s16b get_last_info_line(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    if (p_ptr->info_icky) return p_ptr->last_info_line_icky;
    return p_ptr->last_info_line;
}


cave_view_type* get_info(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    if (p_ptr->info_icky) return &p_ptr->info_icky[y][x];
    return &p_ptr->info[y][x];
}


void free_info_icky(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    mem_nfree((void**)p_ptr->info_icky, p_ptr->last_info_line_icky + 1);
    p_ptr->info_icky = NULL;
}


void alloc_header_icky(struct player *p, const char *header)
{
    if (p->header_icky) return;

    p->header_icky = C_ZNEW(NORMAL_WID, char);
    my_strcpy(p->header_icky, header, NORMAL_WID);
}


const char* get_header(struct player *p, const char *header)
{
    if (p->header_icky) return p->header_icky;
    return header;
}


void free_header_icky(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    if (!p_ptr->header_icky) return;

    mem_free(p_ptr->header_icky);
    p_ptr->header_icky = NULL;
}


void set_ghost_flag(struct player *p, s16b flag, bool report)
{
    p->ghost = flag;
    if (flag)
    {
        p->timed[TMD_INVIS] = -1;
        p->update |= PU_MONSTERS;
        p->redraw |= PR_STATUS;
        if (report) handle_stuff(p);
    }
    else
        player_clear_timed(p, TMD_INVIS, TRUE);
}


void notify_player_popup(struct player *p, char *header, u16b term, u16b pop)
{
    int i;

    /* Use a colored, non browsable display (popup mode) */
    Send_term_info(p, NTERM_ACTIVATE, term);
    Send_term_info(p, NTERM_CLEAR, 0);

    for (i = 0; i < p->last_info_line; i++)
        Send_remote_line(p, i);

    Send_special_other(p, header, 0, FALSE);

    Send_term_info(p, NTERM_FRESH, pop);
    Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
}


void notify_player(int Ind, char *header, u16b term, bool symbol)
{
    player_type *p_ptr = player_get(Ind);

    /* Notify player */
    if (p_ptr->last_info_line >= p_ptr->max_hgt - 4)
    {
        int i, j;

        /* Hack -- Use special term */
        Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);

        for (i = 0; i < p_ptr->last_info_line; i++)
        {
            /* Hack -- We have a symbol as first character */
            if (symbol)
            {
                /* Shift data by 2 characters to the right */
                for (j = NORMAL_WID - 1; j > 2; j--) p_ptr->info[i][j].c = p_ptr->info[i][j - 2].c;

                /* Main color */
                p_ptr->info[i][2].c = (char)p_ptr->info[i][1].a;

                /* Add symbol */
                p_ptr->info[i][1].c = p_ptr->info[i][0].c;
                p_ptr->info[i][0].c = (char)p_ptr->info[i][0].a;

                /* Hack -- Special coloring */
                p_ptr->info[i][0].a = TERM_SYMBOL;

            }

            /* Remove color info */
            else p_ptr->info[i][0].a = TERM_WHITE;
        }

        /* Force fullon mode (non colored, browsable display) */
        Send_special_other(p_ptr, header, 1, TRUE);
    }
    else
        notify_player_popup(p_ptr, header, term, NTERM_POP);
}


const char *player_poss(struct player *p)
{
    switch (p->psex)
    {
        case SEX_FEMALE: return "her";
        case SEX_MALE: return "his";
    }
    return "its";
}


const char *player_self(struct player *p)
{
    switch (p->psex)
    {
        case SEX_FEMALE: return "herself";
        case SEX_MALE: return "himself";
    }
    return "itself";
}


/*** Player access functions ***/


player_type *get_player(int Ind)
{
    return player_get(Ind);
}


const char *get_title(struct player *p)
{
    /* Winner */
    if (p->total_winner) return p->sex->winner;

    /* Normal */
    return p->clazz->title[(p->lev - 1) / 5];
}


s16b get_speed(struct player *p)
{
    s16b speed = p->state.speed - 110;

    /* Hack -- Visually "undo" the Search Mode Slowdown */
    if (p->searching) speed += 10;

    return speed;
}


void get_plusses(struct player *p, int* pfhit, int* pfdam, int* pmhit, int* pmdam,
    int* pshit, int* psdam)
{
    object_type *o_ptr;

    /* Initialize with the known bonuses */
    *pfhit = *pmhit = *pshit = p->state.dis_to_h;
    *pfdam = *pmdam = p->state.dis_to_d;
    *psdam = 0;

    /* Get the wielded weapon */
    o_ptr = &p->inventory[INVEN_WIELD];
    if (o_ptr->kind)
    {
        /* If known, add the wielded weapon bonuses */
        if (object_attack_plusses_are_visible(p, o_ptr))
        {
            *pmhit += o_ptr->to_h;
            *pmdam += o_ptr->to_d;
        }
    }       

    /* Get the wielded bow */
    o_ptr = &p->inventory[INVEN_BOW];
    if (o_ptr->kind)
    {
        /* If known, add the wielded bow bonuses */
        if (object_attack_plusses_are_visible(p, o_ptr))
        {
            *pshit += o_ptr->to_h;
            *psdam += o_ptr->to_d;
        }
    }
}


byte get_dtrap(struct player *p)
{
    /* Only on random levels */
    if (!random_level(p->depth)) return 0;

    /* Edge of detected area */
    if (p->cave->info[p->py][p->px] & CAVE_DEDGE) return 2;

    /* Detected area (safe) */
    if (p->cave->info[p->py][p->px] & CAVE_DTRAP) return 1;

    /* Non-detected area (watch out) */
    return 0;
}
