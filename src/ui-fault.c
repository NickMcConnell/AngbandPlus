/**
 * \file ui-fault.c
 * \brief Fault selection menu
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2016 Nick McConnell
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
#include "init.h"
#include "obj-fault.h"
#include "obj-knowledge.h"
#include "ui-menu.h"
#include "ui-output.h"

static int selection;

struct fault_menu_data {
	int index;
	int power;
};

/**
 * Display an entry on the item menu
 */
void get_fault_display(struct menu *menu, int oid, bool cursor, int row,
					  int col, int width)
{
	struct fault_menu_data *choice = menu_priv(menu);
	int attr = cursor ? COLOUR_L_BLUE : COLOUR_WHITE;
	char buf[80];
	int power = choice[oid].power;
	char *name = faults[choice[oid].index].name;

	strnfmt(buf, sizeof(buf), "  %s (fault strength %d)", name, power);
	c_put_str(attr, buf, row, col);
}

/**
 * Deal with events on the get_item menu
 */
bool get_fault_action(struct menu *menu, const ui_event *event, int oid, bool *exit)
{
	struct fault_menu_data *choice = menu_priv(menu);
	if (event->type == EVT_SELECT) {
		selection = choice[oid].index;
	}

	return false;
}

/**
 * Show spell long description when browsing
 */
static void fault_menu_browser(int oid, void *data, const region *loc)
{
	struct fault_menu_data *choice = data;
	char buf[80];

	/* Redirect output to the screen */
	text_out_hook = text_out_to_screen;
	text_out_wrap = 0;
	text_out_indent = loc->col - 1;
	text_out_pad = 1;

	Term_gotoxy(loc->col, loc->row + loc->page_rows);
	my_strcpy(buf, faults[choice[oid].index].desc, sizeof(buf));
	my_strcap(buf);
	text_out(" %s.\n", buf);

	/* XXX */
	text_out_pad = 0;
	text_out_indent = 0;
}

/**
 * Display list of faults to choose from
 */
int fault_menu(struct object *obj, char *dice_string)
{
	menu_iter menu_f = { 0, 0, get_fault_display, get_fault_action, 0 };
	struct menu *m = menu_new(MN_SKIN_SCROLL, &menu_f);
	int row;
	unsigned int length = 0;
	int i, count = 0;
	size_t array_size = z_info->fault_max * sizeof(struct fault_menu_data);
	struct fault_menu_data *available = mem_zalloc(array_size);
	static region area = { 20, 1, -1, -2 };

	/* Count and then list the faults */
	for (i = 1; i < z_info->fault_max; i++) {
		if ((obj->known->faults[i].power > 0) &&
			(obj->known->faults[i].power < 100) &&
			player_knows_fault(player, i)) {
			available[count].index = i;
			available[count].power = obj->faults[i].power;
			length = MAX(length, strlen(faults[i].name) + 13);
			count++;
		}
	}
	if (!count) {
		mem_free(available);
		return 0;
	}

	/* Set up the menu */
	menu_setpriv(m, count, available);
	m->header = format(" Repair which fault (with ability %s)?", dice_string);
	m->selections = lower_case;
	m->flags = (MN_PVT_TAGS);
	m->browse_hook = fault_menu_browser;

	/* Set up the item list variables */
	selection = 0;

	/* Set up the menu region */
	area.page_rows = m->count + 2;
	area.row = 1;
	area.col = (Term->wid - 1 - length) / 2;
	if (area.col <= 3)
		area.col = 0;
	area.width = MAX(length + 1, strlen(m->header));

	for (row = area.row; row < area.row + area.page_rows; row++)
		prt("", row, MAX(0, area.col - 1));

	menu_layout(m, &area);

	/* Choose */
	menu_select(m, 0, true);

	/* Clean up */
	mem_free(available);
	mem_free(m);

	/* Result */
	return selection;
}

bool textui_get_fault(int *choice, struct object *obj, char *dice_string)
{
	int fault = fault_menu(obj, dice_string);
	if (fault) {
		*choice = fault;
		return true;
	}
	return false;
}
