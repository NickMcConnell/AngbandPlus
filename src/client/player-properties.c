/*
 * File: player-properties.c
 * Purpose: Class and race abilities
 *
 * Copyright (c) 1997-2020 Ben Harrison, James E. Wilson, Robert A. Koeneke,
 * Leon Marrick, Bahman Rabii, Nick McConnell
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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


/*
 * Ability data structures and utilities
 */


enum {
    PLAYER_FLAG_NONE,
    PLAYER_FLAG_SPECIAL,
    PLAYER_FLAG_RACE,
    PLAYER_FLAG_CLASS
};


static bool class_has_ability(const struct player_class *c, struct player_ability *ability)
{
    if (!ability->name) return false;

    if (streq(ability->type, "object"))
    {
        if (!of_has(c->flags, ability->index)) return false;
        if (c->flvl[ability->index] > player->lev) return false;
    }
    else if (streq(ability->type, "player"))
    {
        if (!pf_has(c->pflags, ability->index)) return false;
    }
    else if (streq(ability->type, "element"))
    {
        if (c->el_info[ability->index].res_level != ability->value) return false;
        if (c->el_info[ability->index].lvl > player->lev) return false;
    }

	return true;
}


static bool race_has_ability(const struct player_race *r, struct player_ability *ability)
{
	if (!ability->name) return false;

    if (streq(ability->type, "object"))
    {
        if (!of_has(r->flags, ability->index)) return false;
        if (r->flvl[ability->index] > player->lev) return false;
    }
    else if (streq(ability->type, "player"))
    {
        if (!pf_has(r->pflags, ability->index)) return false;
    }
    else if (streq(ability->type, "element"))
    {
        if (r->el_info[ability->index].res_level != ability->value) return false;
        if (r->el_info[ability->index].lvl > player->lev) return false;
    }

	return true;
}


/*
 * Code for viewing race and class abilities
 */


int num_abilities;
struct player_ability ability_list[32];


static char view_ability_tag(struct menu *menu, int oid)
{
	return I2A(oid);
}


/*
 * Display an entry on the gain ability menu
 */
static void view_ability_display(struct menu *menu, int oid, bool cursor, int row, int col,
    int width)
{
	char buf[NORMAL_WID];
	byte color;
	struct player_ability *choices = menu->menu_data;

	switch (choices[oid].group)
    {
	    case PLAYER_FLAG_SPECIAL:
		{
			strnfmt(buf, sizeof(buf), "Specialty Ability: %s", choices[oid].name);
			color = COLOUR_GREEN;
			break;
		}
	    case PLAYER_FLAG_CLASS:
		{
			strnfmt(buf, sizeof(buf), "Class: %s", choices[oid].name);
			color = COLOUR_UMBER;
			break;
		}
	    case PLAYER_FLAG_RACE:
		{
			strnfmt(buf, sizeof(buf), "Racial: %s", choices[oid].name);
			color = COLOUR_ORANGE;
			break;
		}
	    default:
		{
			my_strcpy(buf, "Mysterious", sizeof(buf));
			color = COLOUR_PURPLE;
		}
	}

	/* Print it */
	c_put_str(cursor? COLOUR_WHITE: color, buf, row, col);
}


/*
 * Show ability long description when browsing
 */
static void view_ability_menu_browser(int oid, void *data, const region *loc)
{
	struct player_ability *choices = data;
    char desc[MSG_LEN];

	clear_from(loc->row + loc->page_rows);
	Term_gotoxy(loc->col, loc->row + loc->page_rows);
    strnfmt(desc, sizeof(desc), "\n%s\n", (char *) choices[oid].desc);
	text_out_to_screen(COLOUR_L_BLUE, desc);
}


/*
 * Display list available specialties.
 */
static void view_ability_menu(void)
{
	struct menu menu;
	menu_iter menu_f = {view_ability_tag, 0, view_ability_display, 0, 0};
	region loc = {0, 0, 70, -99};
	char buf[NORMAL_WID];

	/* Save the screen and clear it */
	screen_save();

	/* Prompt choices */
	strnfmt(buf, sizeof(buf), "Race and class abilities (%c-%c, ESC=exit): ", I2A(0),
        I2A(num_abilities - 1));

	/* Set up the menu */
	menu_init(&menu, MN_SKIN_SCROLL, &menu_f);
	menu.header = buf;
	menu_setpriv(&menu, num_abilities, ability_list);
	loc.page_rows = num_abilities + 1;
	menu.flags = MN_DBL_TAP;
	menu.browse_hook = view_ability_menu_browser;
	region_erase_bordered(&loc);
	menu_layout(&menu, &loc);

	menu_select(&menu, 0, false);

	/* Load screen */
	screen_load(true);
}


/*
 * Browse known abilities
 */
static void view_abilities(void)
{
	struct player_ability *ability;

	/* Count the number of class powers we have */
	for (ability = player_abilities; ability; ability = ability->next)
    {
		if (class_has_ability(player->clazz, ability))
        {
			memcpy(&ability_list[num_abilities], ability, sizeof(struct player_ability));
			ability_list[num_abilities++].group = PLAYER_FLAG_CLASS;
		}
	}

	/* Count the number of race powers we have */
	for (ability = player_abilities; ability; ability = ability->next)
    {
		if (race_has_ability(player->race, ability))
        {
			memcpy(&ability_list[num_abilities], ability, sizeof(struct player_ability));
			ability_list[num_abilities++].group = PLAYER_FLAG_RACE;
		}
	}

	/* View choices until user exits */
	view_ability_menu();

	/* Exit */
	num_abilities = 0;
}


/*
 * Interact with abilities
 */
void do_cmd_abilities(void)
{
	/* View existing abilities */
	view_abilities();
}