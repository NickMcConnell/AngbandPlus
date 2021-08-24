/**
 * \file ui-store.h
 * \brief Store UI
 *
 * Copyright (c) 1997 Robert A. Koeneke, James E. Wilson, Ben Harrison
 * Copyright (c) 1998-2014 Angband developers
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

#ifndef INCLUDED_UI_STORE_H
#define INCLUDED_UI_STORE_H

#include "game-event.h"
#include "store.h"
#include "ui-menu.h"

/**
 * Easy names for the elements of the 'scr_places' arrays.
 */
enum
{
	LOC_PRICE = 0,
	LOC_OWNER,
	LOC_HEADER,
	LOC_MORE,
	LOC_HELP_CLEAR,
	LOC_HELP_PROMPT,
	LOC_AU,
	LOC_WEIGHT,

	LOC_MAX
};

struct store_context {
	struct menu menu;			/* Menu instance */
	struct store *store;	/* Pointer to store */
	struct object **list;	/* List of objects (unused) */
	int flags;				/* Display flags */
	bool inspect_only;		/* Only allow looking */

	/* Places for the various things displayed onscreen */
	unsigned int scr_places_x[LOC_MAX];
	unsigned int scr_places_y[LOC_MAX];
};


void textui_store_knowledge(int n);
void enter_store(game_event_type type, game_event_data *data, void *user);
void use_store(game_event_type type, game_event_data *data, void *user);
void leave_store(game_event_type type, game_event_data *data, void *user);
void store_your_name(struct store *store);
const char *random_rumor(s32b real);
int store_roundup(int);
bool store_get_long_check(struct store_context *ctx, const char *prompt);
void store_long_text(struct store_context *ctx, const char *text);

#endif /* INCLUDED_UI_STORE_H */
