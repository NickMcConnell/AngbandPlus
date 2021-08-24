/*
 * File: ui-message.c
 * Purpose: Message handling
 *
 * Copyright (c) 2007 Elly, Andi Sidwell
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


#include "c-angband.h"


typedef struct _message_t
{
    char *str;
    struct _message_t *newer;
    struct _message_t *older;
    u16b type;
    u16b count;
} message_t;


typedef struct _msgcolor_t
{
    u16b type;
    byte color;
    struct _msgcolor_t *next;
} msgcolor_t;


typedef struct _msgqueue_t
{
    message_t *head;
    message_t *tail;
    msgcolor_t *colors;
    u32b count;
    u32b max;
} msgqueue_t;


static msgqueue_t *messages = NULL;


/*
 * Functions operating on the entire list
 */


/*
 * Initialize the messages package. Should be called before using any other
 * functions in the package.
 */
void messages_init(void)
{
    messages = mem_zalloc(sizeof(msgqueue_t));
    messages->max = 2048;
}


/*
 * Free the message package.
 */
void messages_free(void)
{
    msgcolor_t *c, *nextc;
    message_t *m, *nextm;

    /* Paranoia */
    if (!messages) return;

    c = messages->colors;
    m = messages->head;

    while (m)
    {
        nextm = m->older;
        string_free(m->str);
        mem_free(m);
        m = nextm;
    }

    while (c)
    {
        nextc = c->next;
        mem_free(c);
        c = nextc;
    }

    mem_free(messages);
}


/*
 * Return the current number of messages stored.
 */
u16b messages_num(void)
{
    return messages->count;
}


/*
 * Functions for individual messages
 */


/*
 * Save a new message into the memory buffer, with text `str` and type `type`.
 * The type should be one of the MSG_ constants defined above.
 *
 * The new message may not be saved if it is identical to the one saved before
 * it, in which case the "count" of the message will be increased instead.
 * This count can be fetched using the message_count() function.
 */
void message_add(const char *str, u16b type)
{
    message_t *m;

    /* Fail if messages not loaded */
    if (!messages) return;

    if (messages->head && messages->head->type == type && !strcmp(messages->head->str, str))
    {
        messages->head->count++;
        return;
    }

    m = mem_zalloc(sizeof(message_t));
    m->str = string_make(str);
    m->type = type;
    m->count = 1;
    m->older = messages->head;

    if (messages->head) messages->head->newer = m;

    messages->head = m;
    messages->count++;

    if (!messages->tail) messages->tail = m;

    if (messages->count > messages->max)
    {
        message_t *old_tail = messages->tail;

        messages->tail = old_tail->newer;
        messages->tail->older = NULL;
        string_free(old_tail->str);
        mem_free(old_tail);
        messages->count--;
    }
}


/*
 * Returns the message of age `age`.
 */
static message_t *message_get(u16b age)
{
    message_t *m = messages->head;

    while (m && age--) m = m->older;

    return m;
}


/*
 * Returns the text of the message of age `age`.  The age of the most recently
 * saved message is 0, the one before that is of age 1, etc.
 *
 * Returns the empty string if the no messages of the age specified are
 * available.
 */
const char *message_str(u16b age)
{
    message_t *m = message_get(age);

    return (m? m->str: "");
}


/*
 * Returns the number of times the message of age `age` was saved. The age of
 * the most recently saved message is 0, the one before that is of age 1, etc.
 *
 * In other words, if message_add() was called five times, one after the other,
 * with the message "The orc sets your hair on fire.", then the text will only
 * have one age (age = 0), but will have a count of 5.
 */
u16b message_count(u16b age)
{
    message_t *m = message_get(age);

    return (m? m->count: 0);
}


/*
 * Returns the type of the message of age `age`.  The age of the most recently
 * saved message is 0, the one before that is of age 1, etc.
 *
 * The type is one of the MSG_ constants, defined above.
 */
u16b message_type(u16b age)
{
    message_t *m = message_get(age);

    return (m? m->type: 0);
}


/*
 * Returns the display colour of the message memorised `age` messages ago.
 * (i.e. age = 0 represents the last memorised message, age = 1 is the one
 * before that, etc).
 */
byte message_color(u16b age)
{
    message_t *m = message_get(age);

    return (m? message_type_color(m->type): COLOUR_WHITE);
}


/*
 * Message-color functions
 */


/*
 * Defines the color `color` for the message type `type`.
 */
void message_color_define(u16b type, byte color)
{
    msgcolor_t *mc;

    if (!messages->colors)
    {
        messages->colors = mem_zalloc(sizeof(msgcolor_t));
        messages->colors->type = type;
        messages->colors->color = color;
    }

    mc = messages->colors;
    while (mc->next)
    {
        if (mc->type == type) mc->color = color;
        mc = mc->next;
    }

    mc->next = mem_zalloc(sizeof(msgcolor_t));
    mc->next->type = type;
    mc->next->color = color;
}


/*
 * Returns the colour for the message type `type`.
 */
byte message_type_color(u16b type)
{
    msgcolor_t *mc;
    byte color = COLOUR_WHITE;

    if (messages)
    {
        mc = messages->colors;

        while (mc && mc->type != type) mc = mc->next;

        if (mc && (mc->color != COLOUR_DARK)) color = mc->color;
    }

    return color;
}


/*
 * Return the MSG_ flag that matches the given sound event name.
 *
 * name is the sound name from sound.prf
 */
int message_lookup_by_sound_name(const char *name)
{
    static const char *sound_names[] =
    {
        #define MSG(x, s) s,
        #include "../common/list-message.h"
        #undef MSG
    };
    size_t i;

    for (i = 0; i < N_ELEMENTS(sound_names); i++)
    {
        if (my_stricmp(name, sound_names[i]) == 0)
            return (int)i;
    }

    return -1;
}


/*
 * Return the sound name for the given message.
 *
 * message is the MSG_ flag to find.
 */
const char *message_sound_name(int message)
{
    static const char *sound_names[] =
    {
        #define MSG(x, s) s,
        #include "../common/list-message.h"
        #undef MSG
    };

    if ((message < MSG_GENERIC) || (message >= MSG_MAX))
        return NULL;

    return sound_names[message];
}


/* Extra functions */


/* Get last non local message */
const char *message_last()
{
    message_t *m = messages->head;
    const char *s = "";

    /* Loop */
    while (m)
    {
        /* Get the message text */
        s = m->str;

        /* Make sure it's not "local" */
        if ((m->type != MSG_LOCAL) && (m->type != MSG_BELL)) break;

        /* Advance */
        m = m->older;
    }

    /* Return the message text */
    return (s);
}


/* Hide message */
void message_del(u16b age)
{
    message_t *m = message_get(age);

    /* Hide */
    if (m) m->type = MSG_GENERIC;
}


static message_t *m_iter = NULL;


static void message_fill(message_iter *iter)
{
    if (m_iter)
    {
        iter->str = m_iter->str;
        iter->type = m_iter->type;
        iter->count = m_iter->count;
        iter->color = message_type_color(m_iter->type);
    }
    else
    {
        iter->str = "";
        iter->type = 0;
        iter->count = 0;
        iter->color = COLOUR_WHITE;
    }
}


/* First message */
void message_first(message_iter *iter)
{
    /* Reset iterator */
    m_iter = messages->head;

    /* Fill */
    message_fill(iter);
}


/* Next message */
void message_next(message_iter *iter)
{
    /* Advance iterator */
    if (m_iter) m_iter = m_iter->older;

    /* Fill */
    message_fill(iter);
}


/*
 * Make a noise, without a message. Sound modules hook into this event.
 */
void sound(int type)
{
    /* No sound */
    if (!OPT(use_sound)) return;

    /* Dispatch */
    event_signal_type(EVENT_SOUND, type);
}


/*
 * Flush the screen, make a noise
 */
void bell(const char *reason)
{
    assert(reason);

    /* Add the message */
    message_add(reason, MSG_BELL);

    /* Send bell event */
    event_signal(EVENT_BELL);
}


/*
 * Output a message to the top line of the screen.
 *
 * Long messages are truncated.
 *
 * These messages are memorized for later reference (see above).
 *
 * We must be very careful about using the "msg()" functions without
 * explicitly calling the special "msg(NULL)" function, since this may
 * result in the loss of information if the screen is cleared, or if anything
 * is displayed on the top line.
 *
 * Hack -- note that "msg(NULL)" will clear the top line even if no
 * messages are pending.
 */
void c_msg_print_aux(const char *msg, u16b type)
{
    char buf[MSG_LEN];

    /* Hack -- reset */
    prt("", 0, 0);

    /* Redraw */
    if (player->upkeep) player->upkeep->redraw |= (PR_MESSAGE | PR_MESSAGE_CHAT);

    /* No message */
    if (!msg) return;

    /* Memorize the message */
    message_add(msg, type);

    /* Copy it */
    my_strcpy(buf, msg, sizeof(buf));

    /* Display the message */
    Term_putstr(0, 0, strlen(buf), COLOUR_WHITE, buf);
}


/*
 * Print a message in the default color (white)
 */
void c_msg_print(const char *msg)
{
    c_msg_print_aux(msg, MSG_LOCAL);
}