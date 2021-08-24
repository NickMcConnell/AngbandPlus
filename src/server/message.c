/*
 * File: message.c
 * Purpose: Message handling
 *
 * Copyright (c) 2007 Elly, Andi Sidwell
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
 * Hack -- make a (relevant?) sound
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


void msg_broadcast(struct player *p, const char *msg, u16b type)
{
    int i;

    /* Tell every player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Skip the specified player */
        if (player == p) continue;

        /* Tell this one */
        msg_print(player, msg, type);
    }

    /* Send to console */
    console_print((char*)msg, 0);
}


void msg_all(struct player *p, const char *msg, u16b type)
{
    int i;

    /* Tell every player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Tell this one */
        msg_print(player, msg, type);
    }
}


/*
 * Display a formatted message.
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
    msg_print(p, buf, MSG_GENERIC);
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
    int i;

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Check this player */
        struct player *player = player_get(i);

        /* Don't send the message to the player who caused it */
        if (p == player) continue;

        /* Don't send the message to the second ignoree */
        if (q == player) continue;

        /* Make sure this player is on this level */
        if (!wpos_eq(&player->wpos, &p->wpos)) continue;

        /* Can he see this player? */
        if (square_isview(player, &p->grid))
        {
            /* Send the message */
            msg_print(player, msg, type);
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
 * Display a message to everyone who is on the same dungeon level.
 *
 * This serves two functions: a dungeon level-wide chat, and a way
 * to attract attention of other nearby players.
 */
void msg_format_complex_far(struct player *p, u16b type, const char *fmt, const char *sender, ...)
{
    va_list vp;
    int i;
    char buf[MSG_LEN];
    char buf_vis[MSG_LEN];
    char buf_invis[MSG_LEN];

    /* Begin the Varargs Stuff */
    va_start(vp, sender);

    /* Format the args, save the length */
    vstrnfmt(buf, MSG_LEN, fmt, vp);
    strnfmt(buf_vis, MSG_LEN, "%s %s", sender, buf);
    strnfmt(buf_invis, MSG_LEN, "%s %s", "Someone", buf);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Check this player */
        struct player *player = player_get(i);

        /* Don't send the message to the player who caused it */
        if (p == player) continue;

        /* Don't send the message to the second ignoree */
        /*if (q == player) continue;*/

        /* Make sure this player is on this level */
        if (!wpos_eq(&player->wpos, &p->wpos)) continue;

        /* Can he see this player? */
        if (square_isview(player, &p->grid))
        {
            /* Send the message */
            msg_print(player, buf_vis, type);

            /* Disturb player */
            disturb(player, 0);
        }
        else
        {
            /* Send "invisible" message (e.g. "Someone yells") */
            msg_print(player, buf_invis, type);
        }
    }
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
    int i;

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Check this player */
        struct player *q = player_get(i);

        /* Don't send the message to the player who caused it */
        if (p == q) continue;

        /* Make sure this player is at this depth */
        if (!wpos_eq(&q->wpos, &p->wpos)) continue;

        /* Can he see this player? */
        if (square_isview(q, &p->grid))
        {
            player_desc(q, p_name, sizeof(p_name), p, true);
            strnfmt(buf, sizeof(buf), "%s%s", p_name, msg);

            /* Send the message */
            msg_print(q, buf, type);
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


/*
 * Display a formatted message with a given type, making a sound
 * relevant to the message type.
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
    msg_print(p, buf, type);
}


/*
 * Print a simple message
 */
void msg_print(struct player *p, const char *msg, u16b type)
{
    struct message data;

    data.msg = msg;
    data.type = type;
    display_message(p, &data);
}


/*
 * Print the queued messages.
 */
void message_flush(struct player *p)
{
    msg_print(p, NULL, MSG_GENERIC);
}


void msg_channel(int chan, const char *msg)
{
    int i;

    /* Log to file */
    if (channels[chan].mode & CM_PLOG) plog(msg);

    /* Tell every player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        if (p->on_channel[chan] & UCM_EAR)
            msg_print(p, msg, MSG_CHAT + chan);
    }

    /* And every console */
    console_print((char*)msg, chan);
}