/**
 * \file ui-spell.c
 * \brief Spell UI handing
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
#include "cave.h"
#include "cmds.h"
#include "cmd-core.h"
#include "effects-info.h"
#include "game-input.h"
#include "obj-tval.h"
#include "obj-util.h"
#include "object.h"
#include "player-calcs.h"
#include "player-spell.h"
#include "ui-display.h"
#include "ui-menu.h"
#include "ui-output.h"


/**
 * Spell menu data struct
 */
struct spell_menu_data {
	int *spells;
	int n_spells;

	bool browse;
	bool (*is_valid)(int spell_index);
	bool show_description;

	int selected_spell;
};


/**
 * Is item oid valid?
 */
static int spell_menu_valid(struct menu *m, int oid)
{
	struct spell_menu_data *d = menu_priv(m);
	int *spells = d->spells;

	return d->is_valid ? d->is_valid(spells[oid]) : true;
}

/**
 * Display a row of the spell menu
 */
static void spell_menu_display(struct menu *m, int oid, bool cursor,
		int row, int col, int wid)
{
	struct spell_menu_data *d = menu_priv(m);
	int spell_index = d->spells[oid];
	const struct class_spell *spell = spell_by_index(spell_index);

	char help[30];
	char out[80];

	int attr;
	const char *illegible = NULL;
	const char *comment = NULL;

	if (!spell) return;

	if (player->spell_flags[spell_index] & PY_SPELL_WORKED) {
		/* Get extra info */
		get_spell_info(spell_index, help, sizeof(help));
		comment = help;
		attr = COLOUR_WHITE;
	} else {
		comment = " untried";
		attr = COLOUR_L_GREEN;
	}

	/* Dump the spell --(-- 
	 * 'Cost' - what was once 'Mana' - may be:
	 * 	- (No cost)
	 *  X HP (HP cost, where X may be a fixed number, dice, or normal ("~50") and may change with level)
	 *  X t (Cooldown - X is as above. If the spell can't be cast because cooldown, change the colour and
	 * 			display the number of turns remaining.)
	 * If it has both display the HP (as more urgent) if it is available, the turns remaining otherwise.
	 **/
	char randval[30];
	*randval = 0;
	const char *tag = "";
	/* Cooldown in progress? */
	if (player->cooldown[spell_index] > 0) {
		attr = COLOUR_RED;
		snprintf(randval, sizeof(randval), "%d", player->cooldown[spell_index]);
	} else {
		if (randcalc(spell->hp, 0, AVERAGE) == 0) {
			/* Display cooldown */
			append_random_value_string(randval, sizeof(randval), &spell->turns);
		} else if (randcalc(spell->turns, 0, AVERAGE) == 0) {
			tag = "-";
		} else {
			/* Display HP */
			append_random_value_string(randval, sizeof(randval), &spell->hp);
			tag = " HP";
		}
	}
	strcat(randval, tag);

	strnfmt(out, sizeof(out), "%-26s%s %2d %8s %3d%%%s", spell->name,
			stat_names[spell->stat], spell->slevel, randval, spell_chance(spell_index), comment);
	c_prt(attr, illegible ? illegible : out, row, col);
}

/**
 * Handle an event on a menu row.
 */
static bool spell_menu_handler(struct menu *m, const ui_event *e, int oid, bool *exit)
{
	struct spell_menu_data *d = menu_priv(m);

	if (e->type == EVT_SELECT) {
		d->selected_spell = d->spells[oid];
		return d->browse ? true : false;
	}
	else if (e->type == EVT_KBRD) {
		if (e->key.code == '?') {
			d->show_description = !d->show_description;
		}
	}

	return false;
}

/**
 * Show spell long description when browsing
 */
static void spell_menu_browser(int oid, void *data, const region *loc)
{
	struct spell_menu_data *d = data;
	int spell_index = d->spells[oid];
	const struct class_spell *spell = spell_by_index(spell_index);

	if (d->show_description) {
		/* Redirect output to the screen */
		text_out_hook = text_out_to_screen;
		text_out_wrap = 0;
		text_out_indent = loc->col - 1;
		text_out_pad = 1;

		Term_gotoxy(loc->col, loc->row + loc->page_rows);
		/* Spell description */
		text_out("\n%s", spell->text);

		/* To summarize average damage, count the damaging effects */
		int num_damaging = 0;
		for (struct effect *e = spell->effect; e != NULL; e = effect_next(e)) {
			if (effect_damages(e)) {
				num_damaging++;
			}
		}
		/* Now enumerate the effects' damage and type if not forgotten */
		if (num_damaging > 0
			&& (player->spell_flags[spell_index] & PY_SPELL_WORKED)
			&& !(player->spell_flags[spell_index] & PY_SPELL_FORGOTTEN)) {
			text_out("  Inflicts an average of");
			int i = 0;
			for (struct effect *e = spell->effect; e != NULL; e = effect_next(e)) {
				if (effect_damages(e)) {
					if (num_damaging > 2 && i > 0) {
						text_out(",");
					}
					if (num_damaging > 1 && i == num_damaging - 1) {
						text_out(" and");
					}
					text_out_c(COLOUR_L_GREEN, " %d", effect_avg_damage(e));
					const char *projection = effect_projection(e);
					if (strlen(projection) > 0) {
						text_out(" %s", projection);
					}
					i++;
				}
			}
			text_out(" damage.");
		}
		text_out("\n\n");

		/* XXX */
		text_out_pad = 0;
		text_out_indent = 0;
	}
}

static const menu_iter spell_menu_iter = {
	NULL,	/* get_tag = NULL, just use lowercase selections */
	spell_menu_valid,
	spell_menu_display,
	spell_menu_handler,
	NULL	/* no resize hook */
};

/**
 * Create and initialise a spell menu, given an object and a validity hook
 */
static struct menu *spell_menu_new(bool (*is_valid)(int spell_index))
{
	struct menu *m = menu_new(MN_SKIN_SCROLL, &spell_menu_iter);
	struct spell_menu_data *d = mem_alloc(sizeof *d);
	size_t width = MAX(0, MIN(Term->wid - 15, 80));

	region loc = { 0 - width, 1, width, -99 };

	/* collect spells from object */
	d->n_spells = spell_collect_from_book(&d->spells);
	if (d->n_spells == 0 || !spell_okay_list(is_valid, d->spells, d->n_spells)){
		mem_free(m);
		mem_free(d->spells);
		mem_free(d);
		return NULL;
	}

	/* Copy across private data */
	d->is_valid = is_valid;
	d->selected_spell = -1;
	d->browse = false;
	d->show_description = false;

	menu_setpriv(m, d->n_spells, d);

	/* Set flags */
	m->header = "Name                         Stat Lv    Cost Fail Info";
	m->flags = MN_CASELESS_TAGS;
	m->selections = lower_case;
	m->browse_hook = spell_menu_browser;
	m->cmd_keys = "?";

	/* Set size */
	loc.page_rows = d->n_spells + 1;
	menu_layout(m, &loc);

	return m;
}

/**
 * Clean up a spell menu instance
 */
static void spell_menu_destroy(struct menu *m)
{
	struct spell_menu_data *d = menu_priv(m);
	mem_free(d->spells);
	mem_free(d);
	mem_free(m);
}

/**
 * Run the spell menu to select a spell.
 */
static int spell_menu_select(struct menu *m, const char *noun, const char *verb)
{
	struct spell_menu_data *d = menu_priv(m);
	char buf[80];

	screen_save();
	region_erase_bordered(&m->active);

	/* Format, capitalise and display */
	strnfmt(buf, sizeof buf, "%s which %s? ('?' to toggle description)",
			verb, noun);
	my_strcap(buf);
	prt(buf, 0, 0);

	menu_select(m, 0, true);
	screen_load();

	return d->selected_spell;
}

/**
 * Run the spell menu, without selections.
 */
static void spell_menu_browse(struct menu *m, const char *noun)
{
	struct spell_menu_data *d = menu_priv(m);

	screen_save();

	region_erase_bordered(&m->active);
	prt(format("Browsing %ss. ('?' to toggle description)", noun), 0, 0);

	d->browse = true;
	menu_select(m, 0, true);

	screen_load();
}

/**
 * Browse techniques
 */
void textui_spell_browse(void)
{
	struct menu *m;

	m = spell_menu_new(spell_okay_to_browse);
	if (m) {
		spell_menu_browse(m, "technique");
		spell_menu_destroy(m);
	} else {
		msg("You know no techniques.");
	}
}

/**
 * Get a technique from specified book.
 */
int textui_get_spell_from_book(const char *verb,
							   const char *error,
							   bool (*spell_filter)(int spell_index))
{
	const char *noun = "technique";
	struct menu *m;

	handle_stuff(player);

	m = spell_menu_new(spell_filter);
	if (m) {
		int spell_index = spell_menu_select(m, noun, verb);
		spell_menu_destroy(m);
		return spell_index;
	}

	return -1;
}

/**
 * Get a technique from the player.
 */
int textui_get_spell(const char *verb,
					 cmd_code cmd, const char *error,
					 bool (*spell_filter)(int spell_index))
{
	return textui_get_spell_from_book(verb, error, spell_filter);
}
