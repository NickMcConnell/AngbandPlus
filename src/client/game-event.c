/*
 * File: game-event.c
 * Purpose: Allows the registering of handlers to be told about game events.
 *
 * Copyright (c) 2007 Antony Sidwell
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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


typedef struct _event_handler_entry
{
    struct _event_handler_entry *next;
    game_event_handler *fn;
    void *user;
} event_handler_entry;


static event_handler_entry *event_handlers[N_GAME_EVENTS];


static void game_event_dispatch(game_event_type type, game_event_data *data)
{
    event_handler_entry *this = event_handlers[type];

    /* Send the word out to all interested event handlers */
    while (this)
    {
        /* Call the handler with the relevant data */
        this->fn(type, data, this->user);
        this = this->next;
    }
}


void event_add_handler(game_event_type type, game_event_handler *fn, void *user)
{
    event_handler_entry *new_handler;

    assert(fn != NULL);

    /* Make a new entry */
    new_handler = (event_handler_entry *)mem_alloc(sizeof(event_handler_entry));
    new_handler->fn = fn;
    new_handler->user = user;

    /* Add it to the head of the appropriate list */
    new_handler->next = event_handlers[type];
    event_handlers[type] = new_handler;
}


void event_remove_handler(game_event_type type, game_event_handler *fn, void *user)
{
    event_handler_entry *prev = NULL;
    event_handler_entry *this_handler = event_handlers[type];

    /* Look for the entry in the list */
    while (this_handler)
    {
        /* Check if this is the entry we want to remove */
        if ((this_handler->fn == fn) && (this_handler->user == user))
        {
            if (!prev) event_handlers[type] = this_handler->next;
            else prev->next = this_handler->next;

            mem_free(this_handler);
            return;
        }

        prev = this_handler;
        this_handler = this_handler->next;
    }
}


void event_remove_all_handlers(void)
{
    int type;
    event_handler_entry *handler, *next;

    for (type = 0; type < N_GAME_EVENTS; type++)
    {
        handler = event_handlers[type];
        while (handler)
        {
            next = handler->next;
            mem_free(handler);
            handler = next;
        }
        event_handlers[type] = NULL;
    }
}


void event_add_handler_set(game_event_type *type, size_t n_types,
    game_event_handler *fn, void *user)
{
    size_t i;

    for (i = 0; i < n_types; i++) event_add_handler(type[i], fn, user);
}


void event_remove_handler_set(game_event_type *type, size_t n_types,
    game_event_handler *fn, void *user)
{
    size_t i;

    for (i = 0; i < n_types; i++) event_remove_handler(type[i], fn, user);
}


void event_signal(game_event_type type)
{
    game_event_dispatch(type, NULL);
}


void event_signal_point(game_event_type type, int x, int y)
{
    game_event_data data;

    memset(&data, 0, sizeof(data));

    data.point.x = x;
    data.point.y = y;

    game_event_dispatch(type, &data);
}


void event_signal_type(game_event_type type, int t)
{
    game_event_data data;

    memset(&data, 0, sizeof(data));

    data.type = t;

    game_event_dispatch(type, &data);
}
