/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
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
#include "cmds.h"
#include "ui-menu.h"
#include "game-event.h"

/* Adjustment to minimum failure rates for wisdom/intelligence in moria */
int spell_failure_min_moria(int stat)
{


	int value = p_ptr->state.stat_use[stat];

	if (value > 117) 		return(0);
	else if (value > 107)	return(1);
	else if (value > 87)	return(2);
	else if (value > 67)	return(3);
	else if (value > 17)	return(4);
	else if (value > 14)	return(7);
	else if (value > 7)		return(10);
	else	return(25);
}



/*
 * Returns chance of failure for a spell
 */
s16b spell_chance(int spell)
{
	int chance, minfail;

	const magic_type *s_ptr;


	/* Paranoia -- must be literate */
	if (!cp_ptr->spell_book) return (100);

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	/* Extract the minimum failure rate */
	if (game_mode == GAME_NPPMORIA)
	{
		chance -= 3 * (stat_adj_moria(MORIA_SPELL_STAT)-1);
	}
	else chance -= adj_mag_stat[SPELL_STAT_SLOT];

	/* Not enough mana to cast */
	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	if (game_mode == GAME_NPPMORIA) minfail = spell_failure_min_moria(MORIA_SPELL_STAT);
	else minfail = adj_mag_fail[SPELL_STAT_SLOT];

	/* Non mage/priest characters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if (p_ptr->state.icky_wield)
	{
		chance += 25;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder (after minfail) */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}


/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known)
{
	const magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is ironman */
	if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if (p_ptr->spell_flags[spell] & PY_SPELL_LEARNED)
	{

		/* Okay to cast, not to study */
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}


byte spells[SPELLS_PER_BOOK];
byte num_spells;
int spell_mode;
#define SPELL_DISP_ROW		0
#define SPELL_DISP_COL		6
#define SPELL_DISP_WIDTH	70
#define SPELL_DISP_HGT		15
#define SPELL_ERROR			-3
int spell_pick;

static byte update_spells(const object_type *o_ptr)
{

	int i;

	byte count = 0;

	/* Make sure it is the right spellcasting realm. */
	if (cp_ptr->spell_book != o_ptr->tval) return (0);

	/* Clear everything to start */
	for (i = 0;  i < SPELLS_PER_BOOK; i++)
	{
		spells[i] = -1;
	}

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		int spell = get_spell_index(o_ptr, i);

		/* Collect this spell */
		if (spell != -1) spells[count++] = spell;
	}


	return (count);
}


static void spell_menu_hook(int oid, void *db, const region *loc)
{
	int spell = spells[oid];
	cptr comment = cast_spell(MODE_SPELL_DESC, cp_ptr->spell_book, spell, 0);
	char out_val[80];

	/* Don't display if the menu is hidden */
	if (!loc->col) return;

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Indent output */
	text_out_indent = loc->col + 7;
	text_out_wrap = 65;
	/* Dump the spell --(-- */
	strnfmt(out_val, sizeof(out_val), " %s", comment);
	prt("", loc->row + loc->page_rows, loc->col);
	prt("", loc->row + loc->page_rows + 1, loc->col);

	/* No info until they have cast the spell */
	if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED)) return;

	Term_gotoxy(loc->col + 6, loc->row + loc->page_rows);
	text_out_c(TERM_L_BLUE, out_val);
	text_out_indent = 0;
}


/**
 * Display an entry on the gain specialty menu
 */
static void get_spell_display(menu_type *menu, int oid, bool cursor, int row,
			 int col, int width)
{
	/* Get the spell index */
	int spell = spells[oid];

	char tag = I2A(oid);

	char out_val[80];
	byte line_attr = TERM_WHITE;

	/* Get the spell info */
	const magic_type *s_ptr = &mp_ptr->info[spell];

	cptr name = cast_spell(MODE_SPELL_NAME, cp_ptr->spell_book, spell, 0);
	cptr comment = cast_spell(MODE_SPELL_DESC_SHORT, cp_ptr->spell_book, spell, 0);

	/* For known spells */
	if(cursor)
	{
			line_attr = TERM_L_BLUE;
	}

	/* Skip illegible spells */
	if (s_ptr->slevel >= 99)
	{
		my_strcpy(out_val, format("%-30s", "(illegible)"), sizeof(out_val));
		c_prt(TERM_L_DARK, out_val, row, col);
		return;
	}

	/* Analyze the spell */
	if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)
	{
		comment = "Ironman Spell";
		if(cursor) line_attr = TERM_UMBER;
		else line_attr = TERM_L_RED;
	}
	/* Analyze the spell */
	else if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
	{
		comment = "forgotten";
		if(cursor) line_attr = TERM_UMBER;
		else line_attr = TERM_YELLOW;
	}
	else if (!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))
	{
		if (s_ptr->slevel <= p_ptr->lev)
		{
			comment = "unknown";
			if(cursor) line_attr = TERM_UMBER;
			else line_attr = TERM_ORANGE;
		}
		else
		{
			comment = "difficult";
			if(cursor) line_attr = TERM_UMBER;
			else line_attr = TERM_RED;
		}
	}
	else if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
	{
		comment = "untried";
		if(cursor) line_attr = TERM_GREEN;
		else line_attr = TERM_L_GREEN;
	}

	strnfmt(out_val, sizeof(out_val), "%c)  %-30s%2d %4d %3d%% %s",
			       tag, name, s_ptr->slevel, s_ptr->smana, spell_chance(spell), comment);

	c_prt(line_attr, out_val, row, col);
}


static bool spell_cast_action(char cmd, void *db, int oid)
{
	int i = A2I(cmd);
	int spell= spells[oid];

	(void)db;

	/* Analyze */
	switch (cmd)
	{
		case '\r':
		case '\n':
		case 'M':
		{
			if (spell_okay(spell, TRUE))
			{
				spell_pick = spell;
			}
			else
			{
				bell("Illegal spell choice!");
				spell_pick = SPELL_ERROR;
			}
			break;
		}
		case '?':
		{
			screen_save();
			show_file("options.txt#squelch", NULL, 0, 0);
			screen_load();
			break;
		}
		default: /* keyboard */
		{
			/* Not a spell choice */
			if ((i >= 0) || (i < num_spells))
			{
				if (spell_okay(spells[i], TRUE)) spell_pick = spells[i];
				else spell_pick = SPELL_ERROR;
			}
			else bell("illegal choice!");
			break;
		}
	}

	return (TRUE);
}


static bool spell_study_action(char cmd, void *db, int oid)
{
	int i = A2I(cmd);
	int spell= spells[oid];

	(void)db;

	/* Analyze */
	switch (cmd)
	{
		case '\r':
		case '\n':
		case 'G':
		{
			if (spell_okay(spell, FALSE))
			{
				spell_pick = spell;
				break;
			}
			else
			{
				bell("Illegal spell choice!");
				spell_pick = SPELL_ERROR;
			}
			break;
		}
		case '?':
		{
			screen_save();
			show_file("options.txt#squelch", NULL, 0, 0);
			screen_load();
			break;
		}
		default:
		{
			/* Not a spell choice */
			if ((i >= 0) || (i < num_spells))
			{
				if (spell_okay(spells[i], FALSE)) spell_pick = spells[i];
				else spell_pick = SPELL_ERROR;
			}
			else bell("illegal choice!");
			break;
		}
	}

	return (TRUE);
}


static bool spell_browse_action(char cmd, void *db, int oid)
{
	(void) oid;
	(void) db;

	/* Analyze */
	if (cmd == '?')
	{
		screen_save();
		show_file("options.txt#squelch", NULL, 0, 0);
		screen_load();
	}

	return (TRUE);
}


static bool get_spell_action(char cmd, void *db, int oid)
{
	if (spell_mode == BOOK_CAST) return (spell_cast_action(cmd, db, oid));
	else if (spell_mode == BOOK_STUDY) return (spell_study_action(cmd, db, oid));
	/* MODE_BROWSE */
	else return (spell_browse_action(cmd, db, oid));
}


/**
 * Display list available spell possibilities.
 * Returns the actual spell to cast/study.
 * Returns -1 if the user hits escape.
 * Returns -2 if there are no legal choices.
 * Also handles browsing.
 * Assumes spellbook is appropriate for the spellcasting class
 */
int get_spell_menu(const object_type *o_ptr, int mode_dummy)
{

	char header[100];
	bool okay = (mode_dummy == BOOK_BROWSE);
	int i;
	menu_type menu;
	menu_iter menu_f = { NULL , NULL, get_spell_display, get_spell_action };
	ui_event_data evt = { EVT_NONE, 0, 0, 0, 0 };
	region area = { SPELL_DISP_COL, SPELL_DISP_ROW, -1, SPELL_DISP_HGT };
	int cursor = 0;
	cptr noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
	cptr verb = cast_spell(MODE_SPELL_VERB, cp_ptr->spell_book, 1, 0);
	int row_count;

	bool show_menu = TRUE;

	spell_mode = mode_dummy;
	spell_pick = -2;

	/* Get the right verb for studying */
	if (spell_mode == BOOK_STUDY) verb = "study";

	/* Always view if we are browsing*/
	if ((!auto_display_lists) && (spell_mode != BOOK_BROWSE)) show_menu = FALSE;

	/* Update the spell table */
	num_spells = update_spells(o_ptr);

	/*
	 * Check for "okay" spells, note browse mode is already true, so
	 * this is just for study and cast
	 * */
	if (!okay) for (i = 0; i < num_spells; i++)
	{
		bool check = (spell_mode == BOOK_CAST);

		/* Look for "okay" spells */
		if (!spell_okay(spells[i], check)) continue;
		okay = TRUE;
		break;
	}

	/* Return here if there are no objects or acceptable spells */
	if ((!num_spells) || (!okay))
	{
		return (-2);
	}

	/* Save the screen */
	screen_save();

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.flags = MN_DBL_TAP;
	menu.cmd_keys = "abcdefghi*MG? \n\r";
	menu.menu_data = spells;
	menu.count = num_spells;
	menu.browse_hook = spell_menu_hook;

	/* Select an entry */
	while ((spell_pick == -2) && (evt.key != ESCAPE))
	{
		bool old_cursor = cursor;
		int spell = spells[cursor];
		button_kill_all();
		button_add("[ESC]", ESCAPE);
		button_add("[HELP]", '?');

		/* Priest studying would never use this menu */
		if ((p_ptr->new_spells) && (cp_ptr->flags & (CF_CHOOSE_SPELLS)) && (spell_mode == BOOK_STUDY))
		{
			if (spell_okay(spell, FALSE)) button_add("[STUDY]", 'G');
		}

		/* See if we can cast the actual spell */
		if ((spell_mode == BOOK_CAST) && (spell_okay(spell, TRUE)))
		{
			button_add("[CAST]", 'M');
		}
		screen_load();
		screen_save();
		event_signal(EVENT_MOUSEBUTTONS);

		/* Update the spell table */
		if (spell_mode == BOOK_BROWSE) my_strcpy(header, "       Press ESCAPE to continue", sizeof(header));
		else my_strcpy(header, format("(%^ss %c-%c to %s,%s,ESC=exit,?=help) %^s which %s? ",
						noun, I2A(0), I2A(num_spells - 1), verb, (show_menu ? "*=Hide" : "*=List"),verb, noun), sizeof(header));

		/* Update the menus */
		if (show_menu)
		{
			row_count = num_spells;
			menu.prompt = header;
			menu.title = "        Name                          Lv Mana Fail Info  ";
			area.page_rows = row_count+3;
			area.col = SPELL_DISP_COL;
		}
		else
		{
			row_count = 1;
			menu.title = header;
			menu.prompt = NULL;
			area.page_rows = row_count;
			/* Note this value is used in spell_menu_hook to check if a spell desc should be shown*/
			area.col = 0;
		}
		menu.count = row_count;
		menu_init(&menu, MN_SKIN_SCROLL, &menu_f, &area);
		evt = menu_select(&menu, &cursor, EVT_MOVE | EVT_KBRD);

		/* Toggle menu display */
		if (evt.key == '*')
		{
			spell_pick = -2;
			show_menu = (!show_menu);
			continue;
		}

		/* Handle mouseclicks */
		if ((old_cursor == cursor) && (evt.key == DEFINED_XFF))
		{
			(void)get_spell_action(I2A(cursor), spells, cursor);
		}
		/* Handle buttons */
		else if (evt.type == EVT_BUTTON)
		{
			switch (evt.key)
			{
				case 'M':
				case '?':
				case 'G':
				{
					(void)get_spell_action(evt.key, spells, cursor);
					break;
				}
				default:  	break;
			}
		}

		/* hack - handle tag navigation browsing */
		else if ((spell_mode == BOOK_BROWSE) && (evt.type != EVT_MOVE))
		{
			int x = A2I(evt.key);

			if ((x >= 0) || (x < num_spells))
			{
				cursor= x;

			}
		}

		/* Handle illegal choices */
		if (spell_pick == SPELL_ERROR)
		{
			spell_pick = -2;
			msg_format("You may not %s that %s.", verb, noun);
			message_flush();
		}
	}

	screen_load();
	basic_buttons();
	event_signal(EVENT_MOUSEBUTTONS);

	if (evt.key == ESCAPE) return (-1);

	return (spell_pick);
}


/*
 * Is the player capable of casting a spell?
 */
bool player_can_cast(void)
{
	if (!cp_ptr->spell_book)
	{
		msg_print("You cannot cast spells!");
		return FALSE;
	}

	if (p_ptr->timed[TMD_BLIND] || no_light())
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return FALSE;
	}

	return TRUE;
}


/*
 * Is the player capable of studying?
 */
bool player_can_study(void)
{
	if (!player_can_cast())
		return FALSE;

	if (!p_ptr->new_spells)
	{
		cptr p = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
		msg_format("You cannot learn any new %ss!", p);
		return FALSE;
	}

	return TRUE;
}


/*
 * Check if the given spell is in the given book.
 */
static bool spell_in_book(int spell, int book)
{
	int i;
	object_type *o_ptr = object_from_item_idx(book);

	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		if (spell == get_spell_index(o_ptr, i)) return TRUE;
	}

	return FALSE;
}


/*
 * Gain a specific spell, specified by spell number (for mages).
 */
void do_cmd_study_spell(cmd_code code, cmd_arg args[])
{
	int spell = args[0].choice;

	int item_list[INVEN_TOTAL + MAX_FLOOR_STACK];
	int item_num;
	int i;

	/* Check the player can study at all atm */
	if (!player_can_study())
		return;

	/* Check that the player can actually learn the nominated spell. */
	item_tester_hook = obj_can_browse;
	item_num = scan_items(item_list, N_ELEMENTS(item_list), (USE_INVEN | USE_FLOOR));

	/* Check through all available books */
	for (i = 0; i < item_num; i++)
	{
		if (spell_in_book(spell, item_list[i]))
		{
			if (spell_okay(spell, FALSE))
			{
				/* Spell is in an available book, and player is capable. */
				spell_learn(spell);
				p_ptr->p_energy_use = BASE_ENERGY_MOVE;
			}
			else
			{
				/* Spell is present, but player incapable. */
				msg_format("You cannot learn that spell.");
			}

			return;
		}
	}
}


/*
 * See if we can cast or study from a book
 */
bool player_can_use_book(const object_type *o_ptr, bool known)
{
	int i;

	/* Check the player can study at all, and the book is the right type */
	if (!cp_ptr->spell_book) return FALSE;
	if (p_ptr->timed[TMD_BLIND] || no_light()) return FALSE;
	if (p_ptr->timed[TMD_CONFUSED]) return FALSE;
	if (o_ptr->tval != cp_ptr->spell_book) return (FALSE);

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		int s = get_spell_index(o_ptr, i);

		/* Skip non-OK spells */
		if (s == -1) continue;
		if (!spell_okay(s, known)) continue;

		/* We found a spell to study/cast */
		return (TRUE);
	}

	/* No suitable spells */
	return (FALSE);
}


/*
 * Gain a random spell from the given book (for priests)
 */
void do_cmd_study_book(cmd_code code, cmd_arg args[])
{
	int book = args[0].item;
	object_type *o_ptr = object_from_item_idx(book);

	int spell = -1;
	int i, k = 0;

	cptr p = ((cp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

	/* Check the player can study at all atm */
	if (!player_can_study())
		return;

	/* Check that the player has access to the nominated spell book. */
	if (!item_is_available(book, obj_can_browse, (USE_INVEN | USE_FLOOR)))
	{
		msg_format("That item is not within your reach.");
		return;
	}

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		int s = get_spell_index(o_ptr, i);

		/* Skip non-OK spells */
		if (s == -1) continue;
		if (!spell_okay(s, FALSE)) continue;

		/* Apply the randomizer */
		if ((++k > 1) && (randint0(k) != 0)) continue;

		/* Track it */
		spell = s;
	}

	if (spell < 0)
	{
		msg_format("You cannot learn any %ss in that book.", p);
	}
	else
	{
		/* Remember we have used this book */
		object_kind *k_ptr = &k_info[o_ptr->k_idx];
		k_ptr->tried = TRUE;

		spell_learn(spell);
		p_ptr->p_energy_use = BASE_ENERGY_MOVE;
	}
}


/*
 * Cast a spell from a book
 */
void do_cmd_cast(cmd_code code, cmd_arg args[])
{
	int spell = args[0].choice;
	int dir = args[1].direction;

	int chance;

	cptr noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
	cptr verb = cast_spell(MODE_SPELL_VERB, cp_ptr->spell_book, 1, 0);

	const magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Verify insufficient mana */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.", verb, noun);

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");
	}

	/* Process spell */
	else
	{
		/* Cast the spell */
		sound(MSG_SPELL);

		if (cast_spell(MODE_SPELL_CAST, cp_ptr->spell_book, spell, dir) == NULL) return;

		/* A spell was cast */
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->redraw |= (PR_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Sufficient mana */
	if (s_ptr->smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)inc_timed(TMD_PARALYZED, randint1(5 * oops + 1), TRUE);

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);
}


/*
 * Learn the specified spell.
 */
void spell_learn(int spell)
{
	int i;
	cptr p = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);

	/* Learn the spell */
	p_ptr->spell_flags[spell] |= PY_SPELL_LEARNED;

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i] = spell;

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	           p, get_spell_name(cp_ptr->spell_book, spell));

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more %s%s.",
		           p_ptr->new_spells, p, PLURAL(p_ptr->new_spells));
	}

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY | PR_OBJECT);
}

s16b get_spell_from_list(s16b book, s16b spell)
{
	int realm = get_player_spell_realm();

	if (game_mode == GAME_NPPMORIA)
	{
		/* Check bounds */
		if ((spell < 0) || (spell >= SPELLS_PER_BOOK)) return (-1);
		if ((book < 0) || (book >= BOOKS_PER_REALM_MORIA)) return (-1);

		if (realm == MAGE_REALM) return (spell_list_nppmoria_mage[book][spell]);
		if (realm == PRIEST_REALM) return (spell_list_nppmoria_priest[book][spell]);
	}
	else
	{
		/* Check bounds */
		if ((spell < 0) || (spell >= SPELLS_PER_BOOK)) return (-1);
		if ((book < 0) || (book >= BOOKS_PER_REALM_ANGBAND)) return (-1);

		if (realm == MAGE_REALM) return (spell_list_nppangband_mage[book][spell]);
		if (realm == PRIEST_REALM) return (spell_list_nppangband_priest[book][spell]);
		if (realm == DRUID_REALM) return (spell_list_nppangband_druid[book][spell]);
	}


	/* Whoops! */
	return (-1);
}



int get_spell_index(const object_type *o_ptr, int index)
{
	return get_spell_from_list(o_ptr->sval,index);
}




