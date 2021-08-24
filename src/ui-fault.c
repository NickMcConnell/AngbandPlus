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

static random_value fault_repair_value;
static struct object *fault_repair_obj;

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

	/* Chance of failing once */
	double fail_once;
	int min = randcalc(fault_repair_value, 0, MINIMISE);
	int max = randcalc(fault_repair_value, 0, MAXIMISE);
	if (min == max)
		max++;
	fail_once = (100 * (power - min));
	fail_once /= (max - min);
	if (fail_once < 0)
		fail_once = 0;
	if (fail_once > 100)
		fail_once = 100;

	/* Chance of failing given unlimited attempts.
	 * Failure on a non-fragile item will always make it fragile.
	 * Failure on a fragile item will destroy it 1 time in 4.
	 * So
	 * On a fragile item, you get 1 chance x (1/4), 2 x (3/4x1/4), 3 x (3/4x3x4x1/4) etc.
	 * 	- which converges to 3 tries.
	 * On a non-fragile item you get one more than that.
	 **/
	double fail_ever;
	fail_ever = (fail_once * fail_once * fail_once) / 10000;
	if (!of_has(fault_repair_obj->flags, OF_FRAGILE))
		fail_ever = (fail_ever * fail_once) / 100;

	/* Some messing about to avoid unnecessary precision but distinguish "impossible to fail" from "unlikely" (<1%).
	 * This isn't necessary for 100% (rounding down).
	 */
	int fail_once_per = fail_once;
	char *fail_once_less = "";
	if ((fail_once > 0.0) && (fail_once_per == 0)) {
		fail_once_less = "<";
		fail_once_per = 1;
	}
	int fail_ever_per = fail_ever;
	char *fail_ever_less = "";
	if ((fail_ever > 0.0) && (fail_ever_per == 0)) {
		fail_ever_less = "<";
		fail_ever_per = 1;
	}
	if (((fail_once_per == 100) && (fail_ever_per == 100)) || ((fail_once == 0.0) && (fail_ever == 0.0))) {
		strnfmt(buf, sizeof(buf), "  %s (%d%% fail)", name, (int)fail_once);
	} else {
		strnfmt(buf, sizeof(buf), "  %s (%s%d%% fail, %s%d%% if repeated)", name, fail_once_less, fail_once_per, fail_ever_less, fail_ever_per);
	}
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

static void format_dice(char *dice_string, int size, random_value value)
{
	/* Get the possible dice strings */
	if ((value.dice == 1) && value.base) {
		strnfmt(dice_string, size, "%d+d%d",
				value.base, value.sides);
	} else if (value.dice && value.base) {
		strnfmt(dice_string, size, "%d+%dd%d",
				value.base, value.dice, value.sides);
	} else if (value.dice == 1) {
		strnfmt(dice_string, size, "d%d", value.sides);
	} else if (value.dice) {
		strnfmt(dice_string, size, "%dd%d",
				value.dice, value.sides);
	} else {
		strnfmt(dice_string, size, "%d", value.base);
	}
}

/**
 * Display list of faults to choose from
 */
int fault_menu(struct object *obj, random_value value)
{
	menu_iter menu_f = { 0, 0, get_fault_display, get_fault_action, 0 };
	struct menu *m = menu_new(MN_SKIN_SCROLL, &menu_f);
	int row;
	unsigned int length = 0;
	int i, count = 0;
	size_t array_size = z_info->fault_max * sizeof(struct fault_menu_data);
	struct fault_menu_data *available = mem_zalloc(array_size);
	static region area = { 20, 1, -1, -2 };
	char dice_string[20];
	format_dice(dice_string, sizeof(dice_string), value);

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

	fault_repair_value = value;
	fault_repair_obj = obj;

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

bool textui_get_fault(int *choice, struct object *obj, random_value value)
{
	int fault = fault_menu(obj, value);
	if (fault) {
		*choice = fault;
		return true;
	}
	return false;
}
