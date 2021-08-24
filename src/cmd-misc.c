/**
 * \file cmd-misc.c
 * \brief Deal with miscellaneous commands.
 *
 * Copyright (c) 2010 Andi Sidwell
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
#include "buildid.h"
#include "cave.h"
#include "cmd-core.h"
#include "cmds.h"
#include "game-input.h"
#include "init.h"
#include "mon-lore.h"
#include "mon-util.h"
#include "player-calcs.h"
#include "player-history.h"
#include "player-util.h"
#include "obj-util.h"
#include "target.h"
#include "ui-input.h"
#include "ui-output.h"
#include "ui-term.h"


/**
 * Toggle wizard mode
 */
void do_cmd_wizard(void)
{
	/* Verify first time */
	if (!(player->noscore & NOSCORE_WIZARD)) {
		/* Mention effects */
		msg("You are about to enter 'wizard' mode for the very first time!");
		msg("This is a form of cheating, and your game will not be scored!");
		event_signal(EVENT_MESSAGE_FLUSH);

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? "))
			return;

		/* Mark savefile */
		player->noscore |= NOSCORE_WIZARD;
	}

	/* Toggle mode */
	if (player->wizard) {
		player->wizard = false;
		msg("Wizard mode off.");
	} else {
		player->wizard = true;
		msg("Wizard mode on.");
	}

	/* Update monsters */
	player->upkeep->update |= (PU_MONSTERS);

	/* Redraw "title" */
	player->upkeep->redraw |= (PR_TITLE);
}

/**
 * Commit suicide
 */
void do_cmd_suicide(struct command *cmd)
{
	/* Commit suicide */
	player->is_dead = true;

	/* Cause of death */
	my_strcpy(player->died_from, "Quitting", sizeof(player->died_from));
}

/**
 * Record the player's thoughts as a note.
 *
 * This both displays the note back to the player and adds it to the game log.
 * Two fancy note types are supported: notes beginning with "/say" will be
 * written as 'Frodo says: "____"', and notes beginning with "/me" will
 * be written as 'Frodo ____'.
 */
void do_cmd_note(void)
{
	/* Allocate/Initialize strings to get and format user input. */
	char tmp[70];
	char note[90];
	my_strcpy(tmp, "", sizeof(tmp));
	my_strcpy(note, "", sizeof(note));

	/* Read a line of input from the user */
	if (!get_string("Note: ", tmp, sizeof(tmp))) return;

	/* Ignore empty notes */
	if (!tmp[0] || (tmp[0] == ' ')) return;

	/* Format the note correctly, supporting some cute /me commands */
	if (strncmp(tmp, "/say ", 5) == 0)
		strnfmt(note, sizeof(note), "-- %s says: \"%s\"", player->full_name,
				&tmp[5]);
	else if (strncmp(tmp, "/me", 3) == 0)
		strnfmt(note, sizeof(note), "-- %s%s", player->full_name, &tmp[3]);
	else
		strnfmt(note, sizeof(note), "-- Note: %s", tmp);

	/* Display the note (omitting the "-- " prefix) */
	msg("%s", &note[3]);

	/* Add a history entry */
	history_add(player, note, HIST_USER_INPUT);
}

/**
 * Change class.
 * This can be set at any time, but will take effect only at the next level up.
 * Also display the history of classes so far.
 * Because of that display, it should still be present at maxed level (although not if turned off by birth option)
 */
void do_cmd_change_class(void)
{
	bool leaving = false;

	if (!OPT(player, birth_multi_class)) {
		msg("You cannot change class as you have disabled it by birth option.");
		return;
	}

	/* Extract names, etc. from classes */
	int n_classes = 0;
	const char **name = NULL;
	int *cidx = NULL;
	for (struct player_class *c = classes; c; c = c->next)
		if (c->name)
			n_classes++;
	name = mem_zalloc(n_classes * sizeof(*name));
	cidx = mem_zalloc(n_classes * sizeof(*cidx));
	n_classes = 0;
	int current = 0;
	for (struct player_class *c = classes; c; c = c->next) {
		if (c->name) {
			name[n_classes] = c->name;
			cidx[n_classes] = c->cidx;
			if (c->cidx == player->switch_class)
				current = n_classes;
			n_classes++;
		}
	}

	/* Find longest class name - this (+ 1 blank) is the column size */
	int column_width = 0;
	for (int i = 0; i < n_classes; i++) {
		if ((int)strlen(name[i]) > column_width)
			column_width = strlen(name[i]);
	}
	column_width++;

	/* Build the top message */
	const char *tops = "You can select a class and at the next time you level up, if you have selected a different class from your current one, you will change class. This is usually at the cost of that level's experience.";
	const char *tops_max = "You are at maximum level. As changes to your class only take effect at level up, your class cannot change.";
	if (player->max_lev == PY_MAX_LEVEL)
		tops = tops_max;

	Term_save(); 
	bool loading = true;

	int selected = 0;
	do {
		/* Clear the screen, print the unchanging parts */
		Term_clear();

		/* Format and output the top message */
		struct textblock *tb = textblock_new();
		textblock_append_c(tb, COLOUR_L_YELLOW, tops);
		size_t *line_starts = NULL;
		size_t *line_lengths = NULL;
		int w, h;
		Term_get_size(&w, &h);
		int top = textblock_calculate_lines(tb, &line_starts, &line_lengths, w) + 2;
		textui_textblock_place(tb, SCREEN_REGION, NULL);
		textblock_free(tb);

		/* Loop: print the changing parts, read the keyboard and act on it until exiting (ESC, return). */
		leaving = false;

		/* Find the number of columns */
		int columns = 1;
		columns = w / column_width;
		if (columns > n_classes)
			columns = n_classes;
		int last_row_length = n_classes % columns;
		if (last_row_length == 0)
			last_row_length = columns;

		/* Display grid */
		for (int i = 0; i < n_classes; i++) {
			int col = (i % columns) * column_width;
			int row = (i / columns) + top;
			int colour;
			if (i == current) {
				if (i == selected) {
					colour = COLOUR_WHITE;
				} else {
					colour = COLOUR_YELLOW;
				}
			} else {
				if (i == selected) {
					colour = COLOUR_L_YELLOW;
				} else {
					colour = COLOUR_GREEN;
				}
			}
			c_prt(colour, name[i], row, col);
		}

		/* Classes so far */
		tb = textblock_new();
		textblock_append_c(tb, COLOUR_YELLOW, "You started as a %s", get_class_by_idx(player->lev_class[1])->name);
		if (player->max_lev == 1)
			textblock_append_c(tb, COLOUR_YELLOW, ", and have yet to level up.");
		else {
			/* Display the list of changes so far */
			int changes = 0;
			int prev = player->lev_class[1];
			for(int i = 2; i <= player->max_lev; i++) {
				int next = player->lev_class[i];
				if (next != prev) {
					changes++;
					prev = next;
				}
			}
			prev = player->lev_class[1];
			if (changes) {
				int *levels = mem_zalloc(n_classes * sizeof(*levels));
				int *percent = mem_zalloc(n_classes * sizeof(*percent));

				const char *changed = " and changed ";
				for(int i = 1; i <= player->max_lev; i++) {
					int next = player->lev_class[i];
					if (next != prev) {
						textblock_append_c(tb, COLOUR_YELLOW, "%sto %s at level %d", changed, get_class_by_idx(next)->name, i);
						prev = next;
						changes--;
						changed = (changes > 1)  ? ", " : " and ";
					}
					levels[next]++; 
				}

				/* Convert to percent, trying to make it sum to 100% even if it wouldn't because rounding */
				int sum = 0;
				int high = 0;
				int high_i = 0;
				int match = 0;
				for(int i = 0; i < n_classes; i++) {
					percent[i] = (levels[i] * 100) / player->max_lev;
					sum += percent[i];
					if (percent[i] > high) {
						high = percent[i];
						high_i = i;
						match = 0;
					} else if (percent[i] == high) {
						match++;
					}
				}
				if (match == 1)
					percent[high_i] += (100 - sum);

				textblock_append_c(tb, COLOUR_YELLOW, ", making you ");
				bool first = true;
				for(int i = 0; i < n_classes; i++) {
					if (percent[i] > 0) {
						textblock_append_c(tb, COLOUR_YELLOW, "%s%d%% %s", first ? "" : ", ", percent[i], get_class_by_idx(i)->name);
						first = false;
					}
				}
				textblock_append_c(tb, COLOUR_YELLOW, ".");
				mem_free(levels);
				mem_free(percent);
			} else {
				textblock_append_c(tb, COLOUR_YELLOW, ", and have not changed class.");
			}
		}

		/* Display below the grid, or at the bottom of the screen */
		line_starts = NULL;
		line_lengths = NULL;
		int lines = textblock_calculate_lines(tb, &line_starts, &line_lengths, w);
		region bottom_region = SCREEN_REGION;
		bottom_region.row = h - lines;
		textui_textblock_place(tb, bottom_region, NULL);
		textblock_free(tb);

		/* Redraw */
		Term_redraw();

		/* Read a key */
		struct keypress ch = inkey();
		switch(ch.code) {
			/* Navigate around the grid */
			case '2':
			case KC_PGDOWN:
			case ARROW_DOWN:
			selected += columns;
			if (selected >= n_classes)
				selected %= columns;
			break;
			case '4':
			case ARROW_LEFT:
			selected--;
			if (selected < 0)
				selected = n_classes - 1;
			break;
			case '6':
			case ARROW_RIGHT:
			selected++;
			if (selected >= n_classes)
				selected = 0;
			break;
			case '8':
			case KC_PGUP:
			case ARROW_UP:
			selected -= columns;
			if (selected < 0)
				selected += n_classes;
			break;

			/* Select */
			case ' ':
			case KC_ENTER: {
				Term_load();
				loading = false;
				if (selected == current)
					msg("You will remain a %s.", name[selected]);
				else {
					bool levels = false;
					for(int i = 1; i <= player->max_lev; i++) {
						if (player->lev_class[i] == cidx[selected]) {
							levels = true;
							break;
						}
					}
					msg("You will %s training as a %s next level up.", levels ? "return to" : "begin", name[selected]);
				}
				player->switch_class = cidx[selected];
				leaving = true;
				break;
			}

			/* Leave */
			case ESCAPE:
			case 'Q':
			leaving = true;
			break;
		}

	} while (!leaving);

	if (loading)
		Term_load();
	if (name)
		mem_free(name);
	if (cidx)
		mem_free(cidx);
}
