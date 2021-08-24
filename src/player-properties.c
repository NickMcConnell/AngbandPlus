/**
 * \file player-properties.c 
 * \brief Class and race abilities
 *
 * Copyright (c) 1997-2020 Ben Harrison, James E. Wilson, Robert A. Koeneke,
 * Leon Marrick, Bahman Rabii, Nick McConnell
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
#include "player-ability.h"
#include "player-calcs.h"
#include "ui-input.h"
#include "ui-menu.h"

/**
 * ------------------------------------------------------------------------
 * Ability data structures and utilities
 * ------------------------------------------------------------------------ */
enum {
    PLAYER_FLAG_NONE,
    PLAYER_FLAG_SPECIAL,
    PLAYER_FLAG_RACE,
    PLAYER_FLAG_CLASS
};

bool class_has_ability(const struct player_class *class,
					   struct player_ability *ability)
{
	if (streq(ability->type, "player") &&
		pf_has(class->pflags, ability->index)) {
		return true;
	} else if (streq(ability->type, "object") &&
			   of_has(class->flags, ability->index)) {
		return true;
	}
	return false;
}

bool race_has_ability(const struct player_race *race,
					  struct player_ability *ability)
{
	if (streq(ability->type, "player") &&
		pf_has(race->pflags, ability->index)) {
		return true;
	} else if (streq(ability->type, "object") &&
			   of_has(race->flags, ability->index)) {
		return true;
	} else if (streq(ability->type, "element") &&
			   (race->el_info[ability->index].res_level == ability->value)) {
		return true;
	}

	return false;
}

/**
 * ------------------------------------------------------------------------
 * Code for viewing race and class abilities
 * ------------------------------------------------------------------------ */
int num_abilities;
struct player_ability ability_list[32];


static char view_ability_tag(struct menu *menu, int oid)
{
	return I2A(oid);
}

/**
 * Display an entry on the gain ability menu
 */
void view_ability_display(struct menu *menu, int oid, bool cursor, int row,
					   int col, int width)
{
	char buf[80];
	byte color;
	struct player_ability *choices = menu->menu_data;

	switch (choices[oid].group) {
	case PLAYER_FLAG_SPECIAL:
		{
			sprintf(buf, "Specialty Ability: %s", choices[oid].name);
			color = COLOUR_GREEN;
			break;
		}
	case PLAYER_FLAG_CLASS:
		{
			sprintf(buf, "Class: %s", choices[oid].name);
			color = COLOUR_UMBER;
			break;
		}
	case PLAYER_FLAG_RACE:
		{
			sprintf(buf, "Racial: %s", choices[oid].name);
			color = COLOUR_ORANGE;
			break;
		}
	default:
		{
			sprintf(buf, "Mysterious");
			color = COLOUR_PURPLE;
		}
	}

	/* Print it */
	c_put_str(cursor ? COLOUR_WHITE : color, buf, row, col);

}


/**
 * Show ability long description when browsing
 */
static void view_ability_menu_browser(int oid, void *data, const region *loc)
{
	struct player_ability *choices = data;

	/* Redirect output to the screen */
	text_out_hook = text_out_to_screen;
	text_out_wrap = 60;
	text_out_indent = loc->col - 1;
	text_out_pad = 1;

	clear_from(loc->row + loc->page_rows);
	Term_gotoxy(loc->col, loc->row + loc->page_rows);
	text_out_c(COLOUR_L_BLUE, "\n%s\n", (char *) choices[oid].desc);

	/* XXX */
	text_out_pad = 0;
	text_out_indent = 0;
	text_out_wrap = 0;
}

static bool view_ability_flip;

/**
 * Display a list of available specialties.
 */
bool view_ability_menu(void)
{
	struct menu menu;
	menu_iter menu_f = { view_ability_tag, 0, view_ability_display, 0, 0 };
	region loc = { 0, 0, 70, -99 };
	char buf[80];
	view_ability_flip = false;

	/* Prompt choices */
	sprintf(buf,
			"Race and class abilities (%c-%c, T-show talents/mutations, ESC=exit): ",
			I2A(0), I2A(num_abilities - 1));

	/* Set up the menu */
	if (num_abilities) {
		menu_init(&menu, MN_SKIN_SCROLL, &menu_f);
		menu.header = buf;
		menu_setpriv(&menu, num_abilities, ability_list);
		loc.page_rows = num_abilities + 1;
		menu.flags = MN_DBL_TAP;
		menu.browse_hook = view_ability_menu_browser;
		region_erase_bordered(&loc);
		menu_layout(&menu, &loc);
	} else {
		return true;
	}

	do {
		ui_event ev = menu_select(&menu, EVT_KBRD, false);
		if (ev.type == EVT_ESCAPE)
			break;
		if (ev.type == EVT_KBRD) {
			if ((ev.key.code == 'T') || (ev.key.code == 't')) {
				view_ability_flip = true;
			}
		}
	} while (!view_ability_flip);


	return view_ability_flip;
}

/**
 * Browse known abilities -BR-
 */
static bool view_abilities(void)
{
	struct player_ability *ability;
	bool flip = false;

	/* Count the number of class powers we have */
	for (ability = player_abilities; ability; ability = ability->next) {
		if (class_has_ability(player->class, ability)) {
			memcpy(&ability_list[num_abilities], ability,
				   sizeof(struct player_ability));
			ability_list[num_abilities++].group = PLAYER_FLAG_CLASS;
		}
	}

	/* Count the number of race powers we have */
	for (ability = player_abilities; ability; ability = ability->next) {
		if (race_has_ability(player->race, ability)) {
			memcpy(&ability_list[num_abilities], ability,
				   sizeof(struct player_ability));
			ability_list[num_abilities++].group = PLAYER_FLAG_RACE;
		}
	}

	/* Count the number of ext powers we have */
	for (ability = player_abilities; ability; ability = ability->next) {
		if (race_has_ability(player->extension, ability)) {
			memcpy(&ability_list[num_abilities], ability,
				   sizeof(struct player_ability));
			ability_list[num_abilities++].group = PLAYER_FLAG_RACE;
		}
	}

	/* View choices until user exits */
	flip = view_ability_menu();

	/* Exit */
	num_abilities = 0;
	return flip;
}


/**
 * Interact with abilities or talents -BR-
 */
void do_cmd_abilities(void)
{
	static bool show_talents = false;
	bool flip;

	/* Save the screen and clear it */
	screen_save();

	do {
		flip = false;
		if (show_talents) {
			cmd_abilities(player, false, 0, &flip);
		}
		else {
			/* View existing abilities */
			flip = view_abilities();
		}
		if (flip)
			show_talents = !show_talents;
	} while (flip);

	/* Redraw eveything */
	player->upkeep->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

	/* Load screen */
	screen_load();
	return;
}
