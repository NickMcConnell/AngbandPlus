/*
 * File: ui-event.c
 * Purpose: Allows the registering of handlers to be told about ui "events",
 *          and the game to signal these events to the UI.
 *
 * Copyright (c) 2007 Antony Sidwell
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


#include <assert.h>
#include "z-virt.h"
#include "game-event.h"

#define  N_GAME_EVENTS EVENT_END+1

struct event_handler_entry;
struct event_handler_entry
{
	event_handler_entry *next;	
	game_event_handler *fn;
	void *user;
};

struct event_handler_entry* event_handlers[N_GAME_EVENTS];

void game_event_init(void)
{
	C_WIPE(event_handlers+0,N_GAME_EVENTS);
}

static void game_event_dispatch(game_event_type type, game_event_data *data)
{
	event_handler_entry* this_handler = event_handlers[type];

	/* 
	 * Send the word out to all interested event handlers.
	 */
	while (this_handler)
	{
		/* Call the handler with the relevant data */
		this_handler->fn(type, data, this_handler->user);
		this_handler = this_handler->next;
	}
}

void event_register(game_event_type type, game_event_handler *fn, void *user)
{
	event_handler_entry* new_event;

	assert(0 <= type && type < N_GAME_EVENTS && "precondition");
	assert(NULL != fn && "precondition");

	/* Make a new entry */
	new_event = reinterpret_cast<event_handler_entry*>(mem_alloc(sizeof(event_handler_entry)));
	new_event->fn = fn;
	new_event->user = user;

	/* Add it to the head of the appropriate list */
	new_event->next = event_handlers[type];
	event_handlers[type] = new_event;
}

void event_deregister(game_event_type type, game_event_handler *fn, void *user)
{
	event_handler_entry *prev = NULL;
	event_handler_entry *this_handler = event_handlers[type];

	/* Look for the entry in the list */
	while (this_handler)
	{
		/* Check if this is the entry we want to remove */
		if (this_handler->fn == fn && this_handler->user == user)
		{
			if (!prev)
			{
				event_handlers[type] = this_handler->next;
			}
			else
			{
				prev->next = this_handler->next;
			}

			mem_free(this_handler);
			return;
		}

		prev = this_handler;
		this_handler = this_handler->next;
	}
}

void event_register_set(game_event_type *type, size_t n_types, game_event_handler *fn, void *user)
{
	size_t i;

	for (i = 0; i < n_types; i++)
	{
		event_register(type[i], fn, user);
	}
}

void event_deregister_set(game_event_type *type, size_t n_types, game_event_handler *fn, void *user)
{
	size_t i;

	for (i = 0; i < n_types; i++)
	{
		event_deregister(type[i], fn, user);
	}
}




void event_signal(game_event_type type)
{
	game_event_dispatch(type, NULL);
}

void event_signal_point(game_event_type type, int x, int y)
{
	game_event_data data;
	data.point.x = x;
	data.point.y = y;

	game_event_dispatch(type, &data);
}
void event_signal_string(game_event_type type, const char *s)
{
	game_event_data data;
	data.string = s;

	game_event_dispatch(type, &data);
}

