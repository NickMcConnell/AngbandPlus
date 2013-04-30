/*
 * File: z-msg.c
 * Purpose: Message handling
 *
 * Copyright (c) 2007 Elly, Andrew Sidwell
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


#include "angband.h"


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


/* Functions operating on the entire list */


errr messages_init(void)
{
    messages = ZNEW(msgqueue_t);
    messages->max = 2048;
    return 0;
}


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


u16b messages_num(void)
{
    return messages->count;
}


/* Functions for individual messages */


void message_add(const char *str, u16b type)
{
    message_t *m;

    if (messages->head && messages->head->type == type && !strcmp(messages->head->str, str))
    {
        messages->head->count++;
        return;
    }

    m = ZNEW(message_t);
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


static message_t *message_get(u16b age)
{
    message_t *m = messages->head;

    while (m && age--) m = m->older;

    return m;
}


const char *message_str(u16b age)
{
    message_t *m = message_get(age);

    return (m? m->str: "");
}


u16b message_count(u16b age)
{
    message_t *m = message_get(age);

    return (m? m->count: 0);
}


u16b message_type(u16b age)
{
    message_t *m = message_get(age);

    return (m? m->type: 0);
}


byte message_color(u16b age)
{
    message_t *m = message_get(age);

    return (m? message_type_color(m->type): TERM_WHITE);
}


/* Message-color functions */


void message_color_define(u16b type, byte color)
{
    msgcolor_t *mc;

    if (!messages->colors)
    {
        messages->colors = ZNEW(msgcolor_t);
        messages->colors->type = type;
        messages->colors->color = color;
    }

    mc = messages->colors;
    while (mc->next)
    {
        if (mc->type == type) mc->color = color;
        mc = mc->next;
    }

    mc->next = ZNEW(msgcolor_t);
    mc->next->type = type;
    mc->next->color = color;
}


byte message_type_color(u16b type)
{
    msgcolor_t *mc;
    byte color = TERM_WHITE;

    if (messages)
    {
        mc = messages->colors;

        while (mc && mc->type != type) mc = mc->next;

        if (mc && (mc->color != TERM_DARK)) color = mc->color;
    }

    return color;
}


/* Extra functions */


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
        iter->color = TERM_WHITE;
    }
}


void message_first(message_iter *iter)
{
    /* Reset iterator */
    m_iter = messages->head;

    /* Fill */
    message_fill(iter);
}


void message_next(message_iter *iter)
{
    /* Advance iterator */
    if (m_iter) m_iter = m_iter->older;

    /* Fill */
    message_fill(iter);
}
