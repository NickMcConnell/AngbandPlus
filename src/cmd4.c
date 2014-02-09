/* File: cmd4.c */

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


/*
 *  Header and footer marker string for pref file dumps
 */
static cptr dump_seperator = "#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#";

/* These are used to place elements in the grid */
#define COLOR_FIRST_Y	6
#define COLOR_FIRST_X	1
#define COLOR_X(idx) (((idx) / MAX_BASE_COLORS) * 5 + COLOR_FIRST_X)
#define COLOR_Y(idx) ((idx) % MAX_BASE_COLORS + COLOR_FIRST_Y)


/* Hack - Note the cast to "int" to prevent overflow */
#define IS_BLACK(idx) \
(((int)angband_color_table[idx][1] == 0) && \
 ((int)angband_color_table[idx][2] == 0) && \
 ((int)angband_color_table[idx][3] == 0))

/* String used to show a color sample */
#define COLOR_SAMPLE "###"

/* We show black as dots to see the shape of the grid */
#define BLACK_SAMPLE "..."

/*max length of note output*/
#define LINEWRAP	75


static void dump_pref_file(void (*dump)(ang_file *), const char *title, int row)
{
	char ftmp[80];
	char buf[1024];

	/* Prompt */
	prt(format("%s to a pref file", title), row, 0);

	/* Prompt */
	prt("File: ", row + 2, 0);

	/* Default filename */
	strnfmt(ftmp, sizeof ftmp, "%s.prf", op_ptr->base_name);

	/* Get a filename */
	if (!askfor_aux(ftmp, sizeof ftmp, NULL)) return;

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

	if (!prefs_save(buf, dump, title))
	{
		prt("", 0, 0);
		msg_print("Failed");
		return;
	}

	/* Message */
	prt("", 0, 0);
	msg_print(format("Dumped %s", strstr(title, " ")+1));
}



#define DUMP_COL	120
#define MOD_COL		121

/*
 * Hack -- redraw the screen
 *
 * This command performs various low level updates, clears all the "extra"
 * windows, does a total redraw of the main window, and requests all of the
 * interesting updates and redraws that I can think of.
 *
 * This command is also used to "instantiate" the results of the user
 * selecting various things, such as graphics mode, so it must call
 * the "TERM_XTRA_REACT" hook before redrawing the windows.
 */
void do_cmd_redraw(void)
{
	int j;

	term *old = Term;


	/* Low level flush */
	Term_flush();

	/* Reset "inkey()" */
	flush();

	if (character_dungeon)
		verify_panel();

	/* Hack -- React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);


	/* Combine and Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);


	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_NATIVE);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_INVEN | PR_EQUIP |
	                  PR_MESSAGE | PR_MONSTER | PR_OBJECT |
					  PR_MONLIST | PR_ITEMLIST | PR_FEATURE);

	/* Clear screen */
	Term_clear();

	/* Hack -- update */
	handle_stuff();

	/* Place the cursor on the player */
	if (0 != character_dungeon)
		move_cursor_relative(p_ptr->px, p_ptr->py);

	/* Redraw every window */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Dead window */
		if (!angband_term[j]) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Redraw */
		Term_redraw();

		/* Refresh */
		(void)Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- change name
 */
void do_cmd_change_name(void)
{
	ui_event_data ke;

	int mode = 0;

	cptr p;

	store_type *st_ptr = &store[STORE_HOME];

	/* Prompt */
	p = "[(c)hange name, (f)ile, (h/-)prev display mode, (l/+)next display mode, or ESC]";

	/* Save screen */
	screen_save();

	/* Save the buttons */
	button_kill_all();

	/* Forever */
	while (1)
	{

		bool done = FALSE;

		/* Menu buttons */
		button_kill_all();
		button_add("ESC", ESCAPE);
		button_add("|RENAME", 'c');
		button_add("|FILE", 'f');
		button_add("|NEXT", ARROW_RIGHT);
		button_add("|PREV", ARROW_LEFT);

		/* Display the player */
		display_player(mode, TRUE);

		event_signal(EVENT_MOUSEBUTTONS);

		/* Prompt */
		Term_putstr(1, (Term->hgt - 2), -1, TERM_WHITE, p);

		/* Query */
		ke = inkey_ex();

		switch (ke.key)
		{

			case ESCAPE:
			{
				done = TRUE;
				break;
			}

			case 'c':
			{
				char namebuf[32] = "";

				button_kill_all();
				button_add("ESC", ESCAPE);
				button_add("|RANDOM", '*');
				event_signal(EVENT_MOUSEBUTTONS);

				if (get_name(namebuf, sizeof namebuf))
				{
					/* Set player name */
					my_strcpy(op_ptr->full_name, namebuf,
						  sizeof(op_ptr->full_name));

					/* Don't change savefile name. */
					process_player_name(FALSE);
				}
				break;
			}

			/* File dump */
			case 'f':
			{
				char buf[1024];
				char fname[80];

				strnfmt(fname, sizeof fname, "%s.txt", op_ptr->base_name);

				button_kill_all();
				button_add("ESC", ESCAPE);
				event_signal(EVENT_MOUSEBUTTONS);

				if (get_file(fname, buf, sizeof buf))
				{
					if (file_character(buf, FALSE) != 0)
						msg_print("Character dump failed!");
					else
						msg_print("Character dump successful.");
				}
				break;
			}

			/* Toggle next */
			case '+':
			case 'h':
			case ARROW_LEFT:
			{
				mode += 1;

				/*loop through the four screens*/
				if (mode == 5) mode = 0;
				/*don't display home if empty*/
				else if ((!st_ptr->stock_num) && (mode == 2)) mode = 4;
				break;
			}

			/* Toggle prev */
			case 'l':
			case ARROW_RIGHT:
			case '-':
			{
				/*loop through the four screens*/
				if (mode == 0) mode = 4;

				else if (mode == 4)
				{

					/*don't display home if empty*/
					if (st_ptr->stock_num) mode = 3;
					else mode = 1;
				}

				else mode -= 1;
				break;
			}

			default:
			{
				bell("Illegal command for change name!");
			}
		}

		/* Flush messages */
		message_flush();

		if (done) break;
	}

	/* Load screen */
	screen_load();

	button_restore();
	event_signal(EVENT_MOUSEBUTTONS);
}





/*
 * Recall the most recent message
 */
void do_cmd_message_one(void)
{
	/* Recall one message XXX XXX XXX */
	c_prt(message_color(0), format( "> %s", message_str(0)), 0, 0);
}


/*
 * Show previous messages to the user
 *
 * The screen format uses line 0 and 23 for headers and prompts,
 * skips line 1 and 22, and uses line 2 thru 21 for old messages.
 *
 * This command shows you which commands you are viewing, and allows
 * you to "search" for strings in the recall.
 *
 * Note that messages may be longer than 80 characters, but they are
 * displayed using "infinite" length, with a special sub-command to
 * "slide" the virtual display to the left or right.
 *
 * Attempt to only highlight the matching portions of the string.
 */
void do_cmd_messages(void)
{
	ui_event_data ke;

	int i, j, n, q;
	int wid, hgt;

	char shower[80] = "";



	/* Total messages */
	n = messages_num();

	/* Start on first message */
	i = 0;

	/* Start at leftmost edge */
	q = 0;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Save screen */
	screen_save();

	/* Adjust the buttons */
	button_backup_all();
	button_kill_all();
	button_add("ESC", ESCAPE);
	button_add("|SHOW", '=');
	button_add("|FIND", '/');
	button_add("|UP20", 'p');
	button_add("|DOWN20", 'n');
	button_add("|->", '6');
	button_add("|<-|", '4');

	/* Process requests until done */
	while (1)
	{
		/* Update the buttons as appropriate */
		if (shower[0]) button_add("|NEXT", '-');
		else button_kill('-');

		/* Clear screen */
		Term_clear();

		/* Dump messages */
		for (j = 0; (j < hgt - 4) && (i + j < n); j++)
		{
			const char *msg;
			const char *str = message_str(i + j);
			byte attr = message_color(i + j);
			u16b count = message_count(i + j);

			if (count == 1)
				msg = str;
			else
				msg = format("%s <%dx>", str, count);

			/* Apply horizontal scroll */
			msg = ((int)strlen(msg) >= q) ? (msg + q) : "";

			/* Dump the messages, bottom to top */
			Term_putstr(0, hgt - 3 - j, -1, attr, msg);

			/* Highlight "shower" */
			if (shower[0])
			{
				str = msg;

				/* Display matches */
				while ((str = my_stristr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-msg, hgt - 3 - j, len, TERM_YELLOW, str);

					/* Advance */
					str += len;
				}
			}
		}

		/* Display header */
		prt(format("Message recall (%d-%d of %d), offset %d", i, i + j - 1, n, q), 0, 0);

		/* Display prompt (not very informative) */
		if (shower[0])
		    prt("[Movement keys to navigate, '-' for next, '=' to find]", hgt - 2, 0);
		else
			prt("[Movement keys to navigate, '=' to find, or ESCAPE to exit]", hgt - 2, 0);

		/* Make the mousebuttons current */
		event_signal(EVENT_MOUSEBUTTONS);

		/* Get a command */
		ke = inkey_ex();

		/* Exit on Escape */
		if (ke.key == ESCAPE)
		{
			break;
		}

		/* Find text */
		else if (ke.key == '=')
		{
			/* Get the string to find */
			prt("Find: ", hgt - 1, 0);
			if (!askfor_aux(shower, sizeof shower, NULL)) continue;

			/* Set to find */
			ke.key = '-';
		}

		/* Horizontal scroll */
		else if (ke.key == '4' || ke.key == ARROW_LEFT)
		{
			/* Scroll left */
			q = (q >= wid / 2) ? (q - wid / 2) : 0;

			/* Success */
			continue;
		}

		/* Horizontal scroll */
		else if (ke.key == '6'|| ke.key == ARROW_RIGHT)
		{
			/* Scroll right */
			q = q + wid / 2;

			/* Success */
			continue;
		}

		/* Recall 1 older message */
		else if (ke.key == '8' || ke.key == ARROW_UP)
		{
			/* Go older if legal */
			if (i + 1 < n) i += 1;
		}

		/* Recall 1 newer messages */
		else if (ke.key == '2' || ke.key == ARROW_DOWN || ke.key == '\r' || ke.key == '\n')
		{
			/* Go newer (if able) */
			i = (i >= 1) ? (i - 1) : 0;
		}

		/* Recall 20 older messages */
		else if ((ke.key == 'p') || (ke.key == KTRL('P')) || (ke.key == ' '))
		{
			/* Go older if legal */
			if (i + 20 < n) i += 20;
		}

		/* Recall 20 newer messages */
		else if ((ke.key == 'n') || (ke.key == KTRL('N')))
		{
			/* Go newer (if able) */
			i = (i >= 20) ? (i - 20) : 0;
		}

		/* Find the next item */
		else if (ke.key == '-' && shower[0])
		{
			s16b z;

			/* Scan messages */
			for (z = i + 1; z < n; z++)
			{
				/* Search for it */
				if (my_stristr(message_str(z), shower))
				{
					/* New location */
					i = z;

					/* Done */
					break;
				}
			}
		}

		/* Error time */
		else
		{
			bell(NULL);
		}
	}



	/* Load screen */
	screen_load();

	/* restore the buttons */
	button_restore();
	event_signal(EVENT_MOUSEBUTTONS);
}

/*** Options display and setting ***/

/* Accessor functions */
const char *option_name(int opt)
{
	if (opt >= OPT_MAX) return NULL;
	return options[opt].name;
}

const char *option_desc(int opt)
{
	if (opt >= OPT_MAX) return NULL;
	return options[opt].description;
}

/* Setup functions */
void option_set(int opt, bool on)
{
	op_ptr->opt[opt] = on;
}

void option_set_defaults(void)
{
	size_t opt;

	for (opt = 0; opt < OPT_MAX; opt++)
		op_ptr->opt[opt] = options[opt].normal;
}


/*
 * Displays an option entry.
 */
static void display_option(menu_type *menu, int oid,
							bool cursor, int row, int col, int width)
{
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];
	(void)menu;
	(void)width;

	c_prt(attr, format("%-45s: %s  (%s)", option_desc(oid),
	                   op_ptr->opt[oid] ? "yes" : "no ", option_name(oid)),
	                   row, col);
}

/*
 * Handle keypresses for an option entry.
 */
static bool update_option (char key, void *pgdb, int oid)
{
	(void)pgdb;

	/* Ignore arrow events */
	if (key == ARROW_LEFT || key == ARROW_RIGHT)
		return TRUE;

	switch (toupper((unsigned char) key))
	{
		case 'Y':
		{
			op_ptr->opt[oid] = TRUE;
			break;
		}

		case 'N':
		{
			op_ptr->opt[oid] = FALSE;
			break;
		}

		case '?':
		{
			if (game_mode == GAME_NPPMORIA) show_file(format("m_options.txt#%s", option_name(oid)), NULL, 0, 0);
			else show_file(format("options.txt#%s", option_name(oid)), NULL, 0, 0);
			break;
		}

		default:
		{
			op_ptr->opt[oid] = !op_ptr->opt[oid];
			break;
		}

	}

	return TRUE;
}

static const menu_iter options_toggle_iter =
{
	NULL,
	NULL,
	display_option,		/* label */
	update_option		/* updater */
};

static menu_type option_toggle_menu;


/*
 * Interact with the options
 */
void do_cmd_options_aux(void *vpage, cptr info)
{
	int page = (int)vpage;
	int opt[OPT_PAGE_PER];
	int i, n = 0;
	int cursor_pos = 0;
	region area = (mouse_buttons ? SCREEN_REGION_BUTTONS : SCREEN_REGION);

	menu_type *menu = &option_toggle_menu;
	WIPE(menu, menu_type);

	menu->title = info;

	screen_save();
	clear_from(0);

	/* Filter the options for this page */
	for (i = 0; i < OPT_PAGE_PER; i++)
	{
		if (game_mode == GAME_NPPMORIA)
		{
			if (option_page_nppmoria[page][i] != OPT_NONE)
			{
				opt[n++] = option_page_nppmoria[page][i];
			}

		}
		else
		{
			if (option_page_nppangband[page][i] != OPT_NONE)
			{
				opt[n++] = option_page_nppangband[page][i];
			}
		}

	}

	menu_set_filter(menu, opt, n);
	menu->menu_data = vpage;
	menu->cmd_keys = "?Yy\n\rNnTt\x8C";  /*\x8c = ARROW_RIGHT*/
	menu->selections = "abcdefghijklmopqrsuvwxz";
	menu->count = OPT_PAGE_PER;
	menu->flags = MN_DBL_TAP;
	menu_layout(menu, &area);

	menu_init(menu, MN_SKIN_SCROLL, &options_toggle_iter, &area);

	while (TRUE)
	{
		ui_event_data cx;

		/* re-draw the buttons, returning from the help menu messes them up */
		button_kill_all();
		button_add("[ESC]", ESCAPE);
		button_add("[HELP]", '?');
		button_add("[TOGGLE]", 'T');
		event_signal(EVENT_MOUSEBUTTONS);

		cx = menu_select(menu, &cursor_pos, EVT_MOVE);

		if (cx.key == ESCAPE)
			break;
		else if (cx.type == EVT_MOVE)
			cursor_pos = cx.index;
		else if (cx.type == EVT_SELECT && strchr("YN", toupper((unsigned char) cx.key)))
			cursor_pos++;

		cursor_pos = (cursor_pos+n) % n;

		/* Menu select doesn't handle this for some reason -JG */
		if (cx.type == EVT_BUTTON) update_option(cx.key, NULL, opt[cursor_pos]);
	}

	/* Hack -- Notice use of any "cheat" options */
	for (i = OPT_CHEAT; i < OPT_ADULT; i++)
	{
		if (op_ptr->opt[i])
		{
			/* Set score option */
			op_ptr->opt[OPT_SCORE + (i - OPT_CHEAT)] = TRUE;
		}
	}

	screen_load();

}




/*
 * Modify the "window" options
 */
static void do_cmd_options_win(void)
{
	int i, j, d;

	int y = 0;
	int x = 0;

	ui_event_data ke;

	u32b new_flags[ANGBAND_TERM_MAX];

 	/* Set new flags to the old values */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		new_flags[j] = op_ptr->window_flag[j];
	}


	/* Clear screen */
	clear_from(0);

	button_kill_all();
	button_add("[ESC]", ESCAPE);
	event_signal(EVENT_MOUSEBUTTONS);

	/* Interact */
	while (1)
	{
		/* Prompt */
		prt("Window flags (<dir> to move, 't' to toggle, or ESC)", 0, 0);

		/* Display the windows */
		for (j = 0; j < ANGBAND_TERM_MAX; j++)
		{
			byte a = TERM_WHITE;

			cptr s = angband_term_name[j];

			/* Use color */
			if (j == x) a = TERM_L_BLUE;

			/* Window name, staggered, centered */
			Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
		}

		/* Display the options */
		for (i = 0; i < PW_MAX_FLAGS; i++)
		{
			byte a = TERM_WHITE;

			cptr str = window_flag_desc[i];

			/* Use color */
			if (i == y) a = TERM_L_BLUE;

			/* Unused option */
			if (!str) str = "(Unused option)";

			/* Flag name */
			Term_putstr(0, i + 5, -1, a, str);

			/* Display the windows */
			for (j = 0; j < ANGBAND_TERM_MAX; j++)
			{
				char c = '.';

				a = TERM_WHITE;

				/* Use color */
				if ((i == y) && (j == x)) a = TERM_L_BLUE;

				/* Active flag */
				if (new_flags[j] & (1L << i)) c = 'X';

				/* Flag value */
				Term_putch(35 + j * 5, i + 5, a, c);
			}
		}

		/* Place Cursor */
		Term_gotoxy(35 + x * 5, y + 5);

		/* Get key */
		ke = inkey_ex();

		/* Allow escape */
		if ((ke.key == ESCAPE) || (ke.key == 'q')) break;

		/* Mouse interaction */
		if (ke.key == DEFINED_XFF)
		{
			int choicey = ke.mousey - 5;
			int choicex = (ke.mousex - 35)/5;

			if ((choicey >= 0) && (choicey < PW_MAX_FLAGS)
				&& (choicex > 0) && (choicex < ANGBAND_TERM_MAX)
				&& !(ke.mousex % 5))
			{
				y = choicey;
				x = (ke.mousex - 35)/5;
			}

			/* Toggle using mousebutton later */
			if (!ke.index) continue;
		}

		/* Toggle */
		if ((ke.key == '5') || (ke.key == 't') || (ke.key == '\n') || (ke.key == '\r') || ((ke.key == '\xff') && (ke.index)))
		{
			/* Hack -- ignore the main window */
			if (x == 0)
			{
				bell("Cannot set main window flags!");
			}

			/* Toggle flag (off) */
			else if (new_flags[x] & (1L << y))
			{
				new_flags[x] &= ~(1L << y);
			}

			/* Toggle flag (on) */
			else
			{
				new_flags[x] |= (1L << y);
			}

			/* Continue */
			continue;
		}


		/* Extract direction */
		d = target_dir(ke.key);

		/* Move */
		if (d != 0)
		{
			x = (x + ddx[d]) % ANGBAND_TERM_MAX;
			y = (y + ddy[d]) % PW_MAX_FLAGS;
		}

		/* Oops */
		else
		{
			bell("Illegal command for window options!");
		}
	}

	/* Notice changes */
	subwindows_set_flags(new_flags, ANGBAND_TERM_MAX);
	do_cmd_redraw();
}



/*
 * Remove old lines from pref files
 */
static void remove_old_dump(cptr orig_file, cptr mark)
{
	ang_file *tmp_fff, *orig_fff;

	char buf[1024];
	char temp_file[1024];
	bool between_marks = FALSE;
	bool changed = FALSE;
	char expected_line[1024];

	/* Open an old dump file in read-only mode */
	orig_fff = file_open(orig_file, MODE_READ, -1);

	/* If original file does not exist, nothing to do */
	if (!orig_fff) return;

	/* Open a new temporary file */
	path_build(temp_file, sizeof(temp_file), ANGBAND_DIR_SAVE, "temp_dump.tmp");
	tmp_fff = file_open(temp_file, MODE_WRITE, FTYPE_TEXT);

	if (!tmp_fff)
	{
	    msg_format("Failed to create temporary file %s.", temp_file);
	    msg_print(NULL);
	    return;
	}

	strnfmt(expected_line, sizeof(expected_line),
	        "%s begin %s", dump_seperator, mark);

	/* Loop for every line */
	while (TRUE)
	{
		/* Read a line */
		if (!file_getl(orig_fff, buf, sizeof(buf)))
		{
			/* End of file but no end marker */
			if (between_marks) changed = FALSE;

			break;
		}

		/* Is this line a header/footer? */
		if (strncmp(buf, dump_seperator, strlen(dump_seperator)) == 0)
		{
			/* Found the expected line? */
			if (strcmp(buf, expected_line) == 0)
			{
				if (!between_marks)
				{
					/* Expect the footer next */
					strnfmt(expected_line, sizeof(expected_line),
					        "%s end %s", dump_seperator, mark);

					between_marks = TRUE;

					/* There are some changes */
					changed = TRUE;
				}
				else
				{
					/* Expect a header next - XXX shouldn't happen */
					strnfmt(expected_line, sizeof(expected_line),
					        "%s begin %s", dump_seperator, mark);

					between_marks = FALSE;

					/* Next line */
					continue;
				}
			}
			/* Found a different line */
			else
			{
				/* Expected a footer and got something different? */
				if (between_marks)
				{
					/* Abort */
					changed = FALSE;
					break;
				}
			}
		}

		if (!between_marks)
		{
			/* Copy orginal line */
			file_putf(tmp_fff, "%s\n", buf);
		}
	}

	/* Close files */
	file_close(orig_fff);
	file_close(tmp_fff);

	/* If there are changes, overwrite the original file with the new one */
	if (changed)
	{
		/* Copy contents of temporary file */
		tmp_fff = file_open(temp_file, MODE_READ, -1);
		orig_fff = file_open(orig_file, MODE_WRITE, FTYPE_TEXT);

		while (file_getl(tmp_fff, buf, sizeof(buf)))
		{
			file_putf(orig_fff, "%s\n", buf);
		}

		file_close(orig_fff);
		file_close(tmp_fff);
	}

	/* Kill the temporary file */
	file_delete(temp_file);
}




/*
 * Output the header of a pref-file dump
 */
static void pref_header(ang_file *fff, cptr mark)
{
	/* Start of dump */
	file_putf(fff, "%s begin %s\n", dump_seperator, mark);

	file_putf(fff, "# *Warning!*  The lines below are an automatic dump.\n");
	file_putf(fff, "# Don't edit them; changes will be deleted and replaced automatically.\n");
}


/*
 * Output the footer of a pref-file dump
 */
static void pref_footer(ang_file *fff, cptr mark)
{
	file_putf(fff, "# *Warning!*  The lines above are an automatic dump.\n");
	file_putf(fff, "# Don't edit them; changes will be deleted and replaced automatically.\n");

	/* End of dump */
	file_putf(fff, "%s end %s\n", dump_seperator, mark);
}

/*
 * Ask for a "user pref line" and process it
 */
void do_cmd_pref(void)
{
	char tmp[80];

	/* Default */
	my_strcpy(tmp, "", sizeof(tmp));

	/* Ask for a "user pref command" */
	if (!get_string("Pref: ", tmp, 80)) return;

	/* Process that pref command */
	(void)process_pref_file_command(tmp);
}

/*
 * Ask for a "user pref file" and process it.
 *
 * This function should only be used by standard interaction commands,
 * in which a standard "Command:" prompt is present on the given row.
 *
 * Allow absolute file names?  XXX XXX XXX
 */
static void do_cmd_pref_file_hack(long row)
{
	char ftmp[80];

	/* Prompt */
	prt("Command: Load a user pref file", row, 0);

	/* Prompt */
	prt("File: ", row + 2, 0);

	/* Default filename */
	strnfmt(ftmp, sizeof ftmp, "%s.prf", op_ptr->base_name);

	/* Ask for a file (or cancel) */
	if (!askfor_aux(ftmp, sizeof ftmp, NULL)) return;

	/* Process the given filename */
	if (process_pref_file(ftmp))
	{
		/* Mention failure */
		prt("", 0, 0);
		msg_format("Failed to load '%s'!", ftmp);
	}
	else
	{
		/* Mention success */
		prt("", 0, 0);
		msg_format("Loaded '%s'.", ftmp);
	}
}



/*** Interact with macros and keymaps ***/

#ifdef ALLOW_MACROS

/*
 * Hack -- ask for a "trigger" (see below)
 *
 * Note the complex use of the "inkey()" function from "util.c".
 *
 * Note that both "flush()" calls are extremely important.  This may
 * no longer be true, since "util.c" is much simpler now.  XXX XXX XXX
 */
static void do_cmd_macro_aux(char *buf)
{
	ui_event_data e;

	int n = 0;
	int curs_x, curs_y;

	char tmp[1024] = "";

	/* Get cursor position */
	Term_locate(&curs_x, &curs_y);

	/* Flush */
	flush();

	/* Do not process macros */
	inkey_base = TRUE;

	/* First key */
	e = inkey_ex();

	/* Read the pattern */
	while (e.key != 0 && e.type != EVT_MOUSE)
	{
		/* Save the key */
		buf[n++] = e.key;
		buf[n] = 0;

		/* Get representation of the sequence so far */
		ascii_to_text(tmp, sizeof(tmp), buf);

		/* Echo it after the prompt */
		Term_erase(curs_x, curs_y, 80);
		Term_gotoxy(curs_x, curs_y);
		Term_addstr(-1, TERM_WHITE, tmp);

		/* Do not process macros */
		inkey_base = TRUE;

		/* Do not wait for keys */
		inkey_scan = SCAN_INSTANT;

		/* Attempt to read a key */
		e = inkey_ex();
	}

	/* Convert the trigger */
	ascii_to_text(tmp, sizeof(tmp), buf);
}


/*
 * Ask for, and display, a keymap trigger.
 *
 * Returns the trigger input.
 *
 * Note that both "flush()" calls are extremely important.  This may
 * no longer be true, since "util.c" is much simpler now.  XXX XXX XXX
 */
static char keymap_get_trigger(void)
{
	char tmp[80];
	char buf[2];

	/* Flush */
	flush();

	/* Get a key */
	buf[0] = inkey();
	buf[1] = '\0';

	/* Convert to ascii */
	ascii_to_text(tmp, sizeof(tmp), buf);

	/* Hack -- display the trigger */
	Term_addstr(-1, TERM_WHITE, tmp);

	/* Flush */
	flush();

	/* Return trigger */
	return buf[0];
}


/*
 * Macro menu action functions
 */

static void macro_pref_load(void *unused, const char *title)
{
	do_cmd_pref_file_hack(16);
}

static void macro_pref_append(void *unused, const char *title)
{
	(void)dump_pref_file(macro_dump, "Dump macros", 15);
}

static void macro_query(void *unused, const char *title)
{
	int k;
	char buf[1024];

	prt("Command: Query a macro", 16, 0);
	prt("Trigger: ", 18, 0);

	/* Get a macro trigger */
	do_cmd_macro_aux(buf);

	/* Get the action */
	k = macro_find_exact(buf);

	/* Nothing found */
	if (k < 0)
	{
		/* Prompt */
		prt("", 0, 0);
		prt("Found no macro.  Press any key to continue.", 19, 0);
	}

	/* Found one */
	else
	{
		/* Obtain the action */
		my_strcpy(macro_buffer, macro__act[k], sizeof(macro_buffer));

		/* Analyze the current action */
		ascii_to_text(buf, sizeof(buf), macro_buffer);

		/* Display the current action */
		prt(buf, 22, 0);

		/* Prompt */
		prt("", 0, 0);
		prt("Found a macro.  Press any key to continue.", 19, 0);
	}
	(void)inkey_ex();
}

static void macro_create(void *unused, const char *title)
{
	char pat[1024];
	char tmp[1024];

	prt("Command: Create a macro", 16, 0);
	prt("Trigger: ", 18, 0);

	/* Get a macro trigger */
	do_cmd_macro_aux(pat);

	/* Clear */
	clear_from(20);

	/* Prompt */
	prt("Action: ", 20, 0);

	/* Convert to text */
	ascii_to_text(tmp, sizeof(tmp), macro_buffer);

	/* Get an encoded action */
	if (askfor_aux(tmp, sizeof tmp, NULL))
	{
		/* Convert to ascii */
		text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);

		/* Link the macro */
		macro_add(pat, macro_buffer);

		/* Prompt */
		prt("", 0, 0);
		prt("Added a macro.  Press any key to continue.", 22, 0);
		(void)inkey_ex();

	}
}

static void macro_remove(void *unused, const char *title)
{
	char pat[1024];

	prt("Command: Remove a macro", 16, 0);
	prt("Trigger: ", 18, 0);

	/* Get a macro trigger */
	do_cmd_macro_aux(pat);

	/* Link the macro */
	macro_add(pat, pat);

	/* Prompt */
	prt("", 0, 0);
	prt("Removed a macro.  Press any key to continue.", 19, 0);
	(void)inkey_ex();

}

static void keymap_pref_append(void *unused, const char *title)
{
	(void)dump_pref_file(keymap_dump, "Dump keymaps", 13);
}

static void keymap_query(void *unused, const char *title)
{
	char tmp[1024];
	int mode = (rogue_like_commands ? KEYMAP_MODE_ROGUE : KEYMAP_MODE_ORIG);
	char c;
	const char *act;

	prt(title, 13, 0);
	prt("Key: ", 14, 0);

	/* Get a keymap trigger & mapping */
	c = keymap_get_trigger();
	act = keymap_act[mode][(byte) c];

	/* Nothing found */
	if (!act)
	{
		/* Prompt */
		prt("No keymap with that trigger.  Press any key to continue.", 17, 0);
		(void)inkey_ex();
	}

	/* Found one */
	else
	{
		/* Obtain the action */
		my_strcpy(macro_buffer, act, sizeof(macro_buffer));

		/* Analyze the current action */
		ascii_to_text(tmp, sizeof(tmp), macro_buffer);

		/* Display the current action */
		prt("Found: ", 15, 0);
		Term_addstr(-1, TERM_WHITE, tmp);

		prt("Press any key to continue.", 17, 0);
		(void)inkey_ex();
	}
}

static void keymap_create(void *unused, const char *title)
{
	char c;
	char tmp[1024];
	int mode = (rogue_like_commands ? KEYMAP_MODE_ROGUE : KEYMAP_MODE_ORIG);

	prt(title, 13, 0);
	prt("Key: ", 14, 0);

	c = keymap_get_trigger();

	prt("Action: ", 15, 0);

	/* Get an encoded action, with a default response */
	ascii_to_text(tmp, sizeof(tmp), macro_buffer);
	if (askfor_aux(tmp, sizeof tmp, NULL))
	{
		/* Convert to ascii */
		text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);

		/* Make new keymap */
		string_free(keymap_act[mode][(byte) c]);
		keymap_act[mode][(byte) c] = string_make(macro_buffer);

		/* Prompt */
		prt("", 0, 0);
		prt("Keymap added.  Press any key to continue.", 16, 0);
		(void)inkey_ex();
	}
}

static void keymap_remove(void *unused, const char *title)
{
	char c;
	int mode = (rogue_like_commands ? KEYMAP_MODE_ROGUE : KEYMAP_MODE_ORIG);

	prt(title, 13, 0);
	prt("Key: ", 14, 0);

	c = keymap_get_trigger();

	if (keymap_act[mode][(byte) c])
	{
		/* Free old keymap */
		string_free(keymap_act[mode][(byte) c]);
		keymap_act[mode][(byte) c] = NULL;

		/* Prompt */
		prt("Removed.  Press any key to continue.", 16, 0);
	}
	else
	{
		prt("No keymap to remove!", 16, 0);
	}

	/* Prompt */
	prt("", 0, 0);
	prt("Press any key to continue.", 17, 0);
	(void)inkey_ex();
}

static void macro_enter(void *unused, const char *title)
{
	char tmp[1024];

	prt(title, 16, 0);
	prt("Action: ", 17, 0);

	/* Get an action, with a default response */
	ascii_to_text(tmp, sizeof(tmp), macro_buffer);
	if (askfor_aux(tmp, sizeof tmp, NULL))
	{
		/* Save to global macro buffer */
		text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);
	}
}

static void macro_browse_hook(int oid, void *db, const region *loc)
{
	char tmp[1024];

	/* Show current action */
	prt("Current action (if any) shown below:", 13, 0);
	ascii_to_text(tmp, sizeof(tmp), macro_buffer);
	prt(tmp, 14, 0);
}

static menu_action macro_actions[] =
{
	{ 'l', "Load a user pref file",    	macro_pref_load, 	NULL },
	{ 's', "Save macros to a file",  	macro_pref_append, 	NULL },
	{ 'q', "Query a macro",            	macro_query,		NULL },
	{ 'c', "Create a macro",           	macro_create,		NULL },
	{ 'r', "Remove a macro",           	macro_remove,		NULL },
	{ 'a', "Append keymaps to a file", 	keymap_pref_append,	NULL },
	{ 'k', "Query a keymap",           	keymap_query,		NULL },
	{ 'm', "Create a keymap",          	keymap_create,		NULL },
	{ 'd', "Remove a keymap",          	keymap_remove,		NULL },
	{ 'e', "Enter a new action",       	macro_enter,		NULL },
};

/* Return the tag for a menu entry */
static char macro_menu_tag(menu_type *menu, int oid)
{
	(void)menu;
	return macro_actions[oid].id;
}

/* Display a menu entry */
static void macro_menu_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];
	(void)menu;
	(void)width;
	c_prt(attr, macro_actions[oid].name, row, col);
}

static const menu_iter macro_iter =
{
		macro_menu_tag,
		NULL,
		macro_menu_display,
		NULL
};

static void do_cmd_macros(void)
{
	char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };
	menu_type menu;

	int cursor = 0;
	ui_event_data evt = EVENT_EMPTY;

	region loc = {0, 0, 0, 12};

	/* macro menu */
	WIPE(&menu, menu);
	menu.menu_data = macro_actions;
	menu.title = "Interact with macros";
	menu.cmd_keys = cmd_keys;
	menu.menu_data = macro_actions;
	menu.count = N_ELEMENTS(macro_actions);
	menu_init(&menu, MN_SKIN_SCROLL, &macro_iter, &loc);
	menu.browse_hook = macro_browse_hook;

	screen_save();

	while (evt.key != ESCAPE)
	{
		/* Reset the buttons */
		button_kill_all();
		button_add("[ESC]", ESCAPE);
		button_add("[HELP]", '?');
		clear_from(0);
		event_signal(EVENT_MOUSEBUTTONS);

		evt = menu_select(&menu, &cursor, EVT_MOVE);
		if (evt.type == EVT_SELECT)
		{
			if ((size_t) cursor < N_ELEMENTS(macro_actions))
			{
				macro_actions[cursor].action(macro_actions[cursor].data,
							                              macro_actions[cursor].name);

			}
		}
		else if (evt.type == EVT_BUTTON)
		{
			if (evt.key == '?')	show_file("insc_macro.txt#macros", NULL, 0, 0);
		}
	}

	screen_load();
}

#endif /* ALLOW_MACROS */

/*** Interact with visuals ***/

static void visuals_pref_load(void *unused, const char *title)
{
	do_cmd_pref_file_hack(15);
}

#ifdef ALLOW_VISUALS

static void visuals_dump_monsters(void *unused, const char *title)
{
	dump_pref_file(dump_monsters, title, 15);
}

static void visuals_dump_objects(void *unused, const char *title)
{
	dump_pref_file(dump_objects, title, 15);
}

static void visuals_dump_terrain(void *unused, const char *title)
{
	dump_pref_file(dump_features, title, 15);
}

static void visuals_dump_flavors(void *unused, const char *title)
{
	dump_pref_file(dump_flavors, title, 15);
}

#endif /* ALLOW_VISUALS */

static void visuals_reset(void *unused, const char *title)
{
	/* Reset */
	reset_visuals(TRUE);

	/* Message */
	prt("", 0, 0);
	msg_print("Visual attr/char tables reset.  Press any key to continue.");
	(void)inkey_ex();
}

#ifdef ALLOW_VISUALS



static int find_next_race(int r_idx, int increment)
{
	int next_r_idx = r_idx;

	monster_race *r_ptr;

	while (TRUE)
	{
		next_r_idx += increment;

		/* boundry_control */
		if (next_r_idx < 0) next_r_idx = z_info->r_max - 1;
		else if (next_r_idx >= z_info->r_max) next_r_idx = 0;

		r_ptr = &r_info[next_r_idx];

		/* Oops - should never happen, but avoid infinite loops just in case */
		if (next_r_idx == r_idx) return (r_idx);

		/* Skip non-entries */
		if (!r_ptr->r_speed) continue;

		return (next_r_idx);
	}
}

static int find_next_object(int k_idx, int increment)
{
	int next_k_idx = k_idx;

	object_kind *k_ptr;

	while (TRUE)
	{
		next_k_idx += increment;

		/* boundry_control */
		if (next_k_idx < 0) next_k_idx = z_info->k_max - 1;
		else if (next_k_idx >= z_info->k_max) next_k_idx = 0;

		k_ptr = &k_info[k_idx];

		/* Oops - should never happen, but avoid infinite loops just in case */
		if (next_k_idx == k_idx) return (k_idx);

		/* Skip non-entries */
		if (!k_ptr->name) continue;

		return (next_k_idx);
	}
}

static int find_next_terrain(int f_idx, int increment)
{
	int next_f_idx = f_idx;

	feature_type *f_ptr;

	while (TRUE)
	{
		next_f_idx += increment;

		/* boundry_control */
		if (next_f_idx < 0) next_f_idx = z_info->f_max - 1;
		else if (next_f_idx >= z_info->f_max) next_f_idx = 0;

		f_ptr = &f_info[f_idx];

		/* Oops - should never happen, but avoid infinite loops just in case */
		if (next_f_idx == f_idx) return (f_idx);

		/* Skip non-entries */
		if (!f_ptr->name) continue;

		return (next_f_idx);
	}
}

static byte find_next_color(byte color, signed char increment)
{
	byte next_color = color;
	int i;
	int max_color = 1;

	/* Find the current max color*/
	for (i = 1; i < MAX_COLORS; i++)
	{
		if (IS_BLACK(i))continue;

		max_color = i;
	}

	while (TRUE)
	{
		next_color += increment;

		/* Oops - should never happen, but avoid infinite loops just in case */
		if (next_color == color) return (color);

		/* Boundry control */
		if (next_color > max_color)
		{
			if (increment > 0) next_color = TERM_DARK;
			/* (increment < 0)*/
			else next_color = max_color;
		}

		/* skip the blank colors, except TERM_DARK */
		if (IS_BLACK(next_color))
		{
			if (next_color == TERM_DARK) return (TERM_DARK);
			continue;
		}

		return (next_color);
	}
}

static u16b find_next_flavor(u16b flavor, s16b increment)
{
	u16b next_flavor = flavor;

	flavor_type *flavor_ptr;

	while (TRUE)
	{
		/* Since this is sn unsigned variable,
		 * make sure the value is 0 is handles properly */
		if (!next_flavor && (increment < 0)) next_flavor = z_info->flavor_max - 1;
		else next_flavor += increment;
		/* boundry_control */
		if (next_flavor >= z_info->flavor_max) next_flavor = 0;

		flavor_ptr = &flavor_info[next_flavor];

		/* Oops - should never happen, but avoid infinite loops just in case */
		if (next_flavor == flavor) return (flavor);

		/* Skip unknown flavors */
		if (!flavor_ptr->text) continue;

		return (next_flavor);
	}
}


static void change_monster_visuals(void *unused, const char *title)
{
	int r_idx = 0;
	ui_event_data cx;

	clear_from(0);

	/* Prompt */
	prt("Command: Change monster attr/chars", 15, 0);

	button_kill_all();
	button_add("ESC|", ESCAPE);
	button_add("PREV_RACE|", 	'N');
	button_add("NEXT_RACE|", 	'n');
	button_add("PREV_COLOR|", 	'A');
	button_add("NEXT_COLOR|", 	'a');
	button_add("PREV_CHAR|", 	'C');
	button_add("NEXT_CHAR", 	'c');
	button_add("RESTORE", 		'r');

	/* Hack -- query until done */
	while (TRUE)
	{
		monster_race *r_ptr = &r_info[r_idx];

		byte default_color = (byte)(r_ptr->d_attr);
		byte default_char = (byte)(r_ptr->d_char);
		byte custom_color = (byte)(r_ptr->x_attr);
		byte custom_char = (byte)(r_ptr->x_char);

		/* Label the object */
		Term_putstr(5, 2, -1, TERM_WHITE,
				format("Monster = %d, Name = %-40.40s", r_idx, r_ptr->name_full));

		/* Label the Default values */
		Term_putstr(10, 4, -1, TERM_WHITE,
		             format("Default attr/char = %3u / %3u", default_color, default_char));
		Term_putstr(40, 4, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 4, default_color, default_char);

		if (use_bigtile)
		{
			if (default_color & 0x80)
				Term_putch(44, 4, 255, -1);
			else
				Term_putch(44, 4, 0, ' ');
		}

		/* Label the Current values */
		Term_putstr(10, 5, -1, TERM_WHITE,
		            format("Current attr/char = %3u / %3u", custom_color , custom_char));
		Term_putstr(40, 5, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 5, custom_color , custom_char);

		if (use_bigtile)
		{
			if (custom_color & 0x80)
			{
				Term_putch(44, 5, 255, -1);
			}
			else
			{
				Term_putch(44, 5, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 7, -1, TERM_WHITE, 	"Commands:");
		Term_putstr(0, 8, -1, TERM_WHITE, 	"N:  Previous Monster Race");
		Term_putstr(0, 9, -1, TERM_WHITE, 	"n:  Next Monster Race");
		Term_putstr(0, 10, -1, TERM_WHITE, 	"C:  Previous Character");
		Term_putstr(0, 11, -1, TERM_WHITE, 	"c:  Next Character");
		Term_putstr(0, 12, -1, TERM_WHITE, 	"A:  Previous Color");
		Term_putstr(0, 13, -1, TERM_WHITE, 	"a:  Next Color");
		Term_putstr(0, 14, -1, TERM_WHITE, 	"r:  Restore");

		event_signal(EVENT_MOUSEBUTTONS);

		/* Get a command */
		cx = inkey_ex();

		/* All done */
		if (cx.key == ESCAPE) break;

		/* Analyze */
		switch (cx.key)
		{
			case 'n': 	{r_idx = find_next_race(r_idx,  1); break;}
			case 'N':	{r_idx = find_next_race(r_idx, -1); break;}
			case 'c': 	{r_ptr->x_char = (byte)(custom_char + 1); break;}
			case 'C':	{r_ptr->x_char = (byte)(custom_char - 1); break;}
			case 'a':   {r_ptr->x_attr = find_next_color(custom_color , 1); break;}
			case 'A':   {r_ptr->x_attr = find_next_color(custom_color , -1); break;}
			case 'r':
			{
				r_ptr->x_char = r_ptr->d_char;
				r_ptr->x_attr = r_ptr->d_attr;
				break;
			}
			default:  	break;
		}
	}
}

static void change_object_visuals(void *unused, const char *title)
{
	int k_idx = 0;
	ui_event_data cx;

	clear_from(0);

	/* Prompt */
	prt("Command: Change object attr/chars", 15, 0);

	button_kill_all();
	button_add("ESC|", ESCAPE);
	button_add("PREV_OBJECT|", 	'N');
	button_add("NEXT_OBJECT|", 	'n');
	button_add("PREV_COLOR|", 	'A');
	button_add("NEXT_COLOR|", 	'a');
	button_add("PREV_CHAR|", 	'C');
	button_add("NEXT_CHAR|", 	'c');
	button_add("RESTORE", 		'r');

	/* Hack -- query until done */
	while (TRUE)
	{
		object_kind *k_ptr = &k_info[k_idx];

		byte default_color = (byte)(k_ptr->d_attr);
		byte default_char = (byte)(k_ptr->d_char);
		byte custom_color = (byte)(k_ptr->x_attr);
		byte custom_char = (byte)(k_ptr->x_char);

		/* Label the object */
		Term_putstr(5, 2, -1, TERM_WHITE, format("Object = %d, Name = %-40.40s",
			    	k_idx, (k_name + k_ptr->name)));

		/* Label the Default values */
		Term_putstr(10, 4, -1, TERM_WHITE,
		             format("Default attr/char = %3u / %3u", default_color, default_char));
		Term_putstr(40, 4, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 4, default_color, default_char);

		if (use_bigtile)
		{
			if (default_color & 0x80)
				Term_putch(44, 4, 255, -1);
			else
				Term_putch(44, 4, 0, ' ');
		}

		/* Label the Current values */
		Term_putstr(10, 5, -1, TERM_WHITE,
		            format("Current attr/char = %3u / %3u", custom_color , custom_char));
		Term_putstr(40, 5, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 5, custom_color , custom_char);

		if (use_bigtile)
		{
			if (custom_color & 0x80)
			{
				Term_putch(44, 5, 255, -1);
			}
			else
			{
				Term_putch(44, 5, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 7, -1, TERM_WHITE, 	"Commands:");
		Term_putstr(0, 8, -1, TERM_WHITE, 	"N:  Previous Object Type");
		Term_putstr(0, 9, -1, TERM_WHITE, 	"n:  Next Object Type");
		Term_putstr(0, 10, -1, TERM_WHITE, 	"C:  Previous Character");
		Term_putstr(0, 11, -1, TERM_WHITE, 	"c:  Next Character");
		Term_putstr(0, 12, -1, TERM_WHITE, 	"A:  Previous Color");
		Term_putstr(0, 13, -1, TERM_WHITE, 	"a:  Next Color");
		Term_putstr(0, 14, -1, TERM_WHITE, 	"r:  Restore");

		event_signal(EVENT_MOUSEBUTTONS);

		/* Get a command */
		cx = inkey_ex();

		/* All done */
		if (cx.key == ESCAPE) break;

		/* Analyze */
		switch (cx.key)
		{
			case 'n': 	{k_idx = find_next_object(k_idx,  1); break;}
			case 'N':	{k_idx = find_next_object(k_idx, -1); break;}
			case 'c': 	{k_ptr->x_char = (byte)(custom_char + 1); break;}
			case 'C':	{k_ptr->x_char = (byte)(custom_char - 1); break;}
			case 'a':   {k_ptr->x_attr = find_next_color(custom_color , 1); break;}
			case 'A':   {k_ptr->x_attr = find_next_color(custom_color , -1); break;}
			case 'r':
			{
				k_ptr->x_char = k_ptr->d_char;
				k_ptr->x_attr = k_ptr->d_attr;
				break;
			}
			default:  	break;
		}
	}
}


static void change_terrain_visuals(void *unused, const char *title)
{
	int f_idx = 0;
	ui_event_data cx;

	clear_from(0);

	/* Prompt */
	prt("Command: Change terrain attr/chars", 15, 0);

	button_kill_all();
	button_add("ESC|", ESCAPE);
	button_add("PREV_TERRAIN|", 	'N');
	button_add("NEXT_TERRAIN|", 	'n');
	button_add("PREV_COLOR|", 	'A');
	button_add("NEXT_COLOR|", 	'a');
	button_add("PREV_CHAR|", 	'C');
	button_add("NEXT_CHAR|", 	'c');
	button_add("RESTORE", 		'r');

	/* Hack -- query until done */
	while (TRUE)
	{
		feature_type *f_ptr = &f_info[f_idx];
		char name[80];
		byte default_color = (byte)(f_ptr->d_attr);
		byte default_char = (byte)(f_ptr->d_char);
		byte custom_color = (byte)(f_ptr->x_attr);
		byte custom_char = (byte)(f_ptr->x_char);

		/* Get feature name */
		feature_desc(name, sizeof(name), f_idx, FALSE, FALSE);

		/* Label the object */
		Term_putstr(5, 2, -1, TERM_WHITE, format("Terrain = %d, Name = %-40.40s", f_idx, name));

		/* Label the Default values */
		Term_putstr(10, 4, -1, TERM_WHITE,
		             format("Default attr/char = %3u / %3u", default_color, default_char));
		Term_putstr(40, 4, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 4, default_color, default_char);

		if (use_bigtile)
		{
			if (default_color & 0x80)
				Term_putch(44, 4, 255, -1);
			else
				Term_putch(44, 4, 0, ' ');
		}

		/* Label the Current values */
		Term_putstr(10, 5, -1, TERM_WHITE,
		            format("Current attr/char = %3u / %3u", custom_color , custom_char));
		Term_putstr(40, 5, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 5, custom_color , custom_char);

		if (use_bigtile)
		{
			if (custom_color & 0x80)
			{
				Term_putch(44, 5, 255, -1);
			}
			else
			{
				Term_putch(44, 5, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 7, -1, TERM_WHITE, 	"Commands:");
		Term_putstr(0, 8, -1, TERM_WHITE, 	"N:  Previous Terrain Type");
		Term_putstr(0, 9, -1, TERM_WHITE, 	"n:  Next Terrain Type");
		Term_putstr(0, 10, -1, TERM_WHITE, 	"C:  Previous Character");
		Term_putstr(0, 11, -1, TERM_WHITE, 	"c:  Next Character");
		Term_putstr(0, 12, -1, TERM_WHITE, 	"A:  Previous Color");
		Term_putstr(0, 13, -1, TERM_WHITE, 	"a:  Next Color");
		Term_putstr(0, 14, -1, TERM_WHITE, 	"r:  Restore");

		event_signal(EVENT_MOUSEBUTTONS);

		/* Get a command */
		cx = inkey_ex();

		/* All done */
		if (cx.key == ESCAPE) break;

		/* Analyze */
		switch (cx.key)
		{
			case 'n': 	{f_idx = find_next_terrain(f_idx,  1); break;}
			case 'N':	{f_idx = find_next_terrain(f_idx, -1); break;}
			case 'c': 	{f_ptr->x_char = (byte)(custom_char + 1); break;}
			case 'C':	{f_ptr->x_char = (byte)(custom_char - 1); break;}
			case 'a':   {f_ptr->x_attr = find_next_color(custom_color , 1); break;}
			case 'A':   {f_ptr->x_attr = find_next_color(custom_color , -1); break;}
			case 'r':
			{
				f_ptr->x_char = f_ptr->d_char;
				f_ptr->x_attr = f_ptr->d_attr;
				break;
			}
			default:  	break;
		}
	}
}


static void change_flavor_visuals(void *unused, const char *title)
{
	u16b f = 1;
	ui_event_data cx;

	clear_from(0);

	/* Prompt */
	prt("Command: Change flavor attr/chars", 15, 0);

	button_kill_all();
	button_add("ESC|", ESCAPE);
	button_add("PREV_FLAVOR|", 	'N');
	button_add("NEXT_FLAVOR|", 	'n');
	button_add("PREV_COLOR|", 	'A');
	button_add("NEXT_COLOR|", 	'a');
	button_add("PREV_CHAR|", 	'C');
	button_add("NEXT_CHAR|", 	'c');
	button_add("RESTORE", 		'r');

	/* Hack -- query until done */
	while (TRUE)
	{
		flavor_type *flavor_ptr = &flavor_info[f];

		byte default_color = (byte)(flavor_ptr->d_attr);
		byte default_char = (byte)(flavor_ptr->d_char);
		byte custom_color = (byte)(flavor_ptr->x_attr);
		byte custom_char = (byte)(flavor_ptr->x_char);

		/* Label the object */
		Term_putstr(5, 2, -1, TERM_WHITE, format("Flavor = %d, Text = %-40.40s",
                f, (flavor_text + flavor_ptr->text)));

		/* Label the Default values */
		Term_putstr(10, 4, -1, TERM_WHITE,
		             format("Default attr/char = %3u / %3u", default_color, default_char));
		Term_putstr(40, 4, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 4, default_color, default_char);

		if (use_bigtile)
		{
			if (default_color & 0x80)
				Term_putch(44, 4, 255, -1);
			else
				Term_putch(44, 4, 0, ' ');
		}

		/* Label the Current values */
		Term_putstr(10, 5, -1, TERM_WHITE,
		            format("Current attr/char = %3u / %3u", custom_color , custom_char));
		Term_putstr(40, 5, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 5, custom_color , custom_char);

		if (use_bigtile)
		{
			if (custom_color & 0x80)
			{
				Term_putch(44, 5, 255, -1);
			}
			else
			{
				Term_putch(44, 5, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 7, -1, TERM_WHITE, 	"Commands:");
		Term_putstr(0, 8, -1, TERM_WHITE, 	"N:  Previous Flavor");
		Term_putstr(0, 9, -1, TERM_WHITE, 	"n:  Next Flavor");
		Term_putstr(0, 10, -1, TERM_WHITE, 	"C:  Previous Character");
		Term_putstr(0, 11, -1, TERM_WHITE, 	"c:  Next Character");
		Term_putstr(0, 12, -1, TERM_WHITE, 	"A:  Previous Color");
		Term_putstr(0, 13, -1, TERM_WHITE, 	"a:  Next Color");
		Term_putstr(0, 14, -1, TERM_WHITE, 	"r:  Restore");

		event_signal(EVENT_MOUSEBUTTONS);

		/* Get a command */
		cx = inkey_ex();

		/* All done */
		if (cx.key == ESCAPE) break;

		/* Analyze */
		switch (cx.key)
		{
			case 'n': 	{f = find_next_flavor(f,  1); break;}
			case 'N':	{f = find_next_flavor(f, -1); break;}
			case 'c': 	{flavor_ptr->x_char = (byte)(custom_char + 1); break;}
			case 'C':	{flavor_ptr->x_char = (byte)(custom_char - 1); break;}
			case 'a':   {flavor_ptr->x_attr = find_next_color(custom_color , 1); break;}
			case 'A':   {flavor_ptr->x_attr = find_next_color(custom_color , -1); break;}
			case 'r':
			{
				flavor_ptr->x_char = flavor_ptr->d_char;
				flavor_ptr->x_attr = flavor_ptr->d_attr;
				break;
			}
			default:  	break;
		}
	}
}

#endif /* ALLOW_VISUALS */



static menu_action visual_actions [] =
{
	{ 'l', 			"Load a user pref file", 		visuals_pref_load, 		NULL },
#ifdef ALLOW_VISUALS
	{ 'M',  		"Dump monster attr/chars", 		visuals_dump_monsters, 	NULL },
	{ 'O',  		"Dump object attr/chars", 		visuals_dump_objects,	NULL },
	{ 'T', 			"Dump terrain attr/chars", 		visuals_dump_terrain, 	NULL },
	{ 'F', 			"Dump flavor attr/chars", 		visuals_dump_flavors, 	NULL },
	{ 'm',  		"Change monster attr/chars", 	change_monster_visuals, NULL },
	{ 'o',  		"Change object attr/chars", 	change_object_visuals, 	NULL },
	{ 't', 			"Change terrain attr/chars", 	change_terrain_visuals, NULL },
	{ 'f', 			"Change flavor attr/chars", 	change_flavor_visuals, 	NULL },
#endif /* ALLOW_VISUALS */
	{ 'r', 			"Reset visuals", 				visuals_reset, 			NULL },
};

/* Return the tag for a menu entry */
static char visual_menu_tag(menu_type *menu, int oid)
{
	(void)menu;
	return visual_actions[oid].id;
}

/* Display a menu entry */
static void visual_menu_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];
	(void)menu;
	(void)width;
	c_prt(attr, visual_actions[oid].name, row, col);
}

static const menu_iter visual_iter =
{
		visual_menu_tag,
		NULL,
		visual_menu_display,
		NULL
};

/*
 * Interact with "visuals"
 */
void do_cmd_visuals(void)
{
	char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };
	menu_type menu;

	int cursor = 0;
	ui_event_data evt = EVENT_EMPTY;

	region area = (mouse_buttons ? SCREEN_REGION_BUTTONS : SCREEN_REGION);

	/* macro menu */
	WIPE(&menu, menu);
	menu.menu_data = visual_actions;
	menu.title = "Interact with visuals";
	menu.cmd_keys = cmd_keys;
	menu.menu_data = visual_actions;
	menu.count = N_ELEMENTS(visual_actions);
	menu_init(&menu, MN_SKIN_SCROLL, &visual_iter, &area);

	screen_save();

	while (evt.key != ESCAPE)
	{
		/* Reset the buttons */
		button_kill_all();
		button_add("[ESC]", ESCAPE);
		button_add("[HELP]", '?');
		clear_from(0);
		event_signal(EVENT_MOUSEBUTTONS);

		evt = menu_select(&menu, &cursor, EVT_MOVE);
		if (evt.type == EVT_SELECT)
		{
			if ((size_t) cursor < N_ELEMENTS(visual_actions))
			{
				visual_actions[cursor].action(visual_actions[cursor].data,
						visual_actions[cursor].name);

			}
		}
		else if (evt.type == EVT_BUTTON)
		{

			if (evt.key == '?')
			{
				if (game_mode == GAME_NPPMORIA) show_file("m_options.txt", NULL, 0, 0);
				else show_file("options.txt", NULL, 0, 0);
			}
		}
	}

	screen_load();
}




static void colors_pref_load(void *unused, const char *title)
{
	/* Ask for and load a user pref file */
	do_cmd_pref_file_hack(8);


	Term_xtra(TERM_XTRA_REACT, 0);
	Term_redraw();
}

#ifdef ALLOW_COLORS

static void colors_pref_dump(void *unused, const char *title)
{
	ang_file *fff;
	int i;
	char buf[1024];

	static cptr mark = "Colors";
	char ftmp[80];

	/* Prompt */
	prt("Command: Dump colors", 8, 0);

	/* Prompt */
	prt("File: ", 10, 0);

	/* Default filename */
	strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

	/* Get a filename */
	if (!askfor_aux(ftmp, sizeof(ftmp), NULL)) return;

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

	/* Remove old colors */
	remove_old_dump(buf, mark);

	/* Append to the file */
	fff = file_open(buf, MODE_APPEND, FTYPE_TEXT);

	/* Failure */
	if (!fff) return;

	/* Output header */
	pref_header(fff, mark);

	/* Skip some lines */
	file_putf(fff, "\n\n");

	/* Start dumping */
	file_putf(fff, "# Color redefinitions\n\n");

	/* Dump colors */
	for (i = 0; i < 256; i++)
	{
		int kv = angband_color_table[i][0];
		int rv = angband_color_table[i][1];
		int gv = angband_color_table[i][2];
		int bv = angband_color_table[i][3];

		cptr name = "unknown";

		/* Skip non-entries */
		if (!kv && !rv && !gv && !bv) continue;

		/* Extract the color name */
		if (i < 16) name = color_names[i];

		/* Dump a comment */
		file_putf(fff, "# Color '%s'\n", name);

		/* Dump the monster attr/char info */
		file_putf(fff, "V:%d:0x%02X:0x%02X:0x%02X:0x%02X\n\n",
		        i, kv, rv, gv, bv);
	}

	/* All done */
	file_putf(fff, "\n\n\n\n");

	/* Output footer */
	pref_footer(fff, mark);

	/* Close */
	file_close(fff);

	/* Message */
	msg_print("Dumped color redefinitions.");
}

/*
 * Asks to the user for specific color values.
 * Returns TRUE if the color was modified.
 */
static bool askfor_color_values(int idx)
{
  	char str[10];

  	int k, r, g, b;

  	/* Get the default value */
  	sprintf(str, "%d", angband_color_table[idx][1]);

  	/* Query, check for ESCAPE */
  	if (!get_string("Red (0-255) ", str, sizeof(str))) return FALSE;

  	/* Convert to number */
  	r = atoi(str);

  	/* Check bounds */
  	if (r < 0) r = 0;
  	if (r > 255) r = 255;

  	/* Get the default value */
  	sprintf(str, "%d", angband_color_table[idx][2]);

  	/* Query, check for ESCAPE */
  	if (!get_string("Green (0-255) ", str, sizeof(str))) return FALSE;

  	/* Convert to number */
  	g = atoi(str);

  	/* Check bounds */
  	if (g < 0) g = 0;
  	if (g > 255) g = 255;

  	/* Get the default value */
  	sprintf(str, "%d", angband_color_table[idx][3]);

 	/* Query, check for ESCAPE */
  	if (!get_string("Blue (0-255) ", str, sizeof(str))) return FALSE;

 	/* Convert to number */
  	b = atoi(str);

  	/* Check bounds */
  	if (b < 0) b = 0;
  	if (b > 255) b = 255;

  	/* Get the default value */
  	sprintf(str, "%d", angband_color_table[idx][0]);

  	/* Query, check for ESCAPE */
  	if (!get_string("Extra (0-255) ", str, sizeof(str))) return FALSE;

  	/* Convert to number */
  	k = atoi(str);

  	/* Check bounds */
  	if (k < 0) k = 0;
  	if (k > 255) k = 255;

  	/* Do nothing if the color is not modified */
  	if ((k == angband_color_table[idx][0]) &&
        (r == angband_color_table[idx][1]) &&
        (g == angband_color_table[idx][2]) &&
        (b == angband_color_table[idx][3])) return FALSE;

  	/* Modify the color table */
 	angband_color_table[idx][0] = k;
 	angband_color_table[idx][1] = r;
 	angband_color_table[idx][2] = g;
  	angband_color_table[idx][3] = b;

  	/* Notify the changes */
  	return TRUE;
}

/*
 * The screen used to modify the color table. Only 128 colors can be modified.
 * The remaining entries of the color table are reserved for graphic mode.
 */
static void colors_modify(void *unused, const char *title)
{
	int x, y, idx, old_idx;
	ui_event_data ch;
	char msg[100];

	/* Flags */
	bool do_move, do_update;

	/* Clear the screen */
	Term_clear();

	/* Draw the color table */
	for (idx = 0; idx < MAX_COLORS; idx++)
	{
	  	/* Get coordinates, the x value is adjusted to show a fake cursor */
	  	x = COLOR_X(idx) + 1;
	   	y = COLOR_Y(idx);

	   	/* Show a sample of the color */
	   	if (IS_BLACK(idx)) c_put_str(TERM_WHITE, BLACK_SAMPLE, y, x);
	   	else c_put_str(idx, COLOR_SAMPLE, y, x);
	}

	/* Show screen commands and help */
	y = 1;
	x = 42;
	c_put_str(TERM_WHITE, "Commands:", y, x);
	c_put_str(TERM_WHITE, "ESC: Return", y + 2, x);
	c_put_str(TERM_WHITE, "Arrows or mouse selection: Move to color", y + 3, x);
	c_put_str(TERM_WHITE, "k,K: Incr,Decr extra value", y + 4, x);
	c_put_str(TERM_WHITE, "r,R: Incr,Decr red value", y + 5, x);
	c_put_str(TERM_WHITE, "g,G: Incr,Decr green value", y + 6, x);
	c_put_str(TERM_WHITE, "b,B: Incr,Decr blue value", y + 7, x);
	c_put_str(TERM_WHITE, "c: Copy from color", y + 8, x);
	c_put_str(TERM_WHITE, "v (or double-click): Set specific values", y + 9, x);
	c_put_str(TERM_WHITE, "First column: base colors", y + 11, x);
	c_put_str(TERM_WHITE, "Second column: first shade, etc.", y + 12, x);

	c_put_str(TERM_WHITE, "Shades look like base colors in 16 color ports.",
	      			22, 0);

  	button_kill_all();
  	button_add("[ESC]", ESCAPE);
  	button_add("[Red+]", 'r');
  	button_add("[Red-]", 'R');
  	button_add("[Blue+]", 'b');
  	button_add("[Blue-]", 'B');
  	button_add("[Green+]", 'g');
  	button_add("[Green-]", 'H');
   	button_add("[X-]", 'k');
  	button_add("[X+]", 'K');
  	button_add("[c]", 'c');
  	button_add("[v]", 'v');
   	event_signal(EVENT_MOUSEBUTTONS);

  	/* Hack - We want to show the fake cursor */
  	do_move = TRUE;
	do_update = TRUE;

  	/* Start with the first color */
  	idx = 0;

  	/* Used to erase the old position of the fake cursor */
  	old_idx = -1;

  	while (1)
  	{
    	/* Movement request */
    	if (do_move)
    	{

      		/* Erase the old fake cursor */
      		if (old_idx >= 0)
      		{
				/* Get coordinates */
				x = COLOR_X(old_idx);
				y = COLOR_Y(old_idx);

				/* Draw spaces */
				c_put_str(TERM_WHITE, " ", y, x);
				c_put_str(TERM_WHITE, " ", y, x + 4);
      		}

      		/* Show the current fake cursor */
      		/* Get coordinates */
      		x = COLOR_X(idx);
      		y = COLOR_Y(idx);

      		/* Draw the cursor */
      		c_put_str(TERM_WHITE, ">", y, x);
      		c_put_str(TERM_WHITE, "<", y, x + 4);

      		/* Format the name of the color */
      		my_strcpy(msg, format("Color = %d (0x%02X), Name = %s", idx, idx,
	    	get_ext_color_name(idx)), sizeof(msg));

      		/* Show the name and some whitespace */
      		c_put_str(TERM_WHITE, format("%-40s", msg), 2, 0);
    	}

    	/* Color update request */
    	if (do_update)
    	{
      		/* Get coordinates, adjust x */
      		x = COLOR_X(idx) + 1;
      		y = COLOR_Y(idx);

      		/* Hack - Redraw the sample if needed */
      		if (IS_BLACK(idx)) c_put_str(TERM_WHITE, BLACK_SAMPLE, y, x);
      		else c_put_str(idx, COLOR_SAMPLE, y, x);

      		/* Notify the changes in the color table to the terminal */
      		Term_xtra(TERM_XTRA_REACT, 0);

      		/* The user is playing with white, redraw all */
      		if (idx == TERM_WHITE) Term_redraw();

      		/* Or reduce flickering by redrawing the changes only */
      		else Term_redraw_section(x, y, x + 2, y);
    	}

    	/* Common code, show the values in the color table */
    	if (do_move || do_update)
    	{
      		/* Format the view of the color values */
		  	my_strcpy(msg, format("K = %d / R,G,B = %d, %d, %d",
	    			angband_color_table[idx][0],
	    			angband_color_table[idx][1],
	    			angband_color_table[idx][2],
					angband_color_table[idx][3]), sizeof(msg));

			/* Show color values and some whitespace */
      		c_put_str(TERM_WHITE, format("%-40s", msg), 4, 0);

    	}

    	/* Reset flags */
    	do_move = FALSE;
    	do_update = FALSE;
    	old_idx = -1;

    	/* Get a command */
    	ch = inkey_ex();

    	if (ch.key == ESCAPE) break;

    	switch(ch.key)
    	{
      		/* Down */
      		case '2':
			{
	  			/* Check bounds */
	  			if (idx + 1 >= MAX_COLORS) break;

	  			/* Erase the old cursor */
	  			old_idx = idx;

	  			/* Get the new position */
	  			++idx;

	  			/* Request movement */
	  			do_move = TRUE;
	  			break;
			}

      		/* Up */
      		case '8':
			{

				/* Check bounds */
	  			if (idx - 1 < 0) break;

	  			/* Erase the old cursor */
	  			old_idx = idx;

	  			/* Get the new position */
	  			--idx;

	  			/* Request movement */
	  			do_move = TRUE;
	  			break;
			}

      		/* Left */
      		case '4':
			{
	  			/* Check bounds */
	  			if (idx - 16 < 0) break;

	  			/* Erase the old cursor */
	  			old_idx = idx;

	  			/* Get the new position */
	  			idx -= 16;

	  			/* Request movement */
	  			do_move = TRUE;
	  			break;
			}

	  		/* Right */
      		case '6':
			{
	  			/* Check bounds */
	  			if (idx + 16 >= MAX_COLORS) break;

	  			/* Erase the old cursor */
	  			old_idx = idx;

	  			/* Get the new position */
	  			idx += 16;

	  			/* Request movement */
	  			do_move = TRUE;
	  			break;
			}

			/* Copy from color */
      		case 'c':
			{
	  			char str[10];
	  			int src;

	  			/* Get the default value, the base color */
	  			sprintf(str, "%d", GET_BASE_COLOR(idx));

	  			/* Query, check for ESCAPE */
	  			if (!get_string(format("Copy from color (0-%d, def. base) ",
					MAX_COLORS - 1), str, sizeof(str))) break;

	  			/* Convert to number */
	  			src = atoi(str);

	  			/* Check bounds */
	  			if (src < 0) src = 0;
	  			if (src >= MAX_COLORS) src = MAX_COLORS - 1;

	  			/* Do nothing if the colors are the same */
	  			if (src == idx) break;

	  			/* Modify the color table */
	  			angband_color_table[idx][0] = angband_color_table[src][0];
	  			angband_color_table[idx][1] = angband_color_table[src][1];
	  			angband_color_table[idx][2] = angband_color_table[src][2];
	  			angband_color_table[idx][3] = angband_color_table[src][3];

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

      		/* Increase the extra value */
      		case 'k':
			{
	  			/* Get a pointer to the proper value */
	  			byte *k_ptr = &angband_color_table[idx][0];

	  			/* Modify the value */
	  			*k_ptr = (byte)(*k_ptr + 1);

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

      		/* Decrease the extra value */
      		case 'K':
			{

	  			/* Get a pointer to the proper value */
	  			byte *k_ptr = &angband_color_table[idx][0];

	  			/* Modify the value */
	  			*k_ptr = (byte)(*k_ptr - 1);

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

      		/* Increase the red value */
      		case 'r':
			{
	  			/* Get a pointer to the proper value */
	  			byte *r_ptr = &angband_color_table[idx][1];

	  			/* Modify the value */
	  			*r_ptr = (byte)(*r_ptr + 1);

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

      		/* Decrease the red value */
      		case 'R':
			{

	  			/* Get a pointer to the proper value */
	  			byte *r_ptr = &angband_color_table[idx][1];

	  			/* Modify the value */
	  			*r_ptr = (byte)(*r_ptr - 1);

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

	  		/* Increase the green value */
      		case 'g':
			{
	  			/* Get a pointer to the proper value */
	  			byte *g_ptr = &angband_color_table[idx][2];

	  			/* Modify the value */
	  			*g_ptr = (byte)(*g_ptr + 1);

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

	  		/* Decrease the green value */
      		case 'G':
			{
	  			/* Get a pointer to the proper value */
	  			byte *g_ptr = &angband_color_table[idx][2];

	  			/* Modify the value */
	  			*g_ptr = (byte)(*g_ptr - 1);

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

	  		/* Increase the blue value */
      		case 'b':
			{
	  			/* Get a pointer to the proper value */
	  			byte *b_ptr = &angband_color_table[idx][3];

	  			/* Modify the value */
	  			*b_ptr = (byte)(*b_ptr + 1);

				/* Request update */
	  			do_update = TRUE;
	  			break;
			}

      		/* Decrease the blue value */
      		case 'B':
			{
	  			/* Get a pointer to the proper value */
	  			byte *b_ptr = &angband_color_table[idx][3];

				/* Modify the value */
	  			*b_ptr = (byte)(*b_ptr - 1);

	  			/* Request update */
	  			do_update = TRUE;
	  			break;
			}

			/* Mouse interaction */
      		case DEFINED_XFF:
			{
				int choicey = ch.mousey;
				int choicex = ch.mousex;
				int this_idx;

				/* Ensure if we clicked in the color grid */
				if (choicey < COLOR_FIRST_Y) break;
				if (choicex < COLOR_FIRST_X) break;
				choicey -= COLOR_FIRST_Y;
				choicex /= 5;
				/* It's a 18x9 grid */
				if (choicey > 15) break;
				if (choicex > 7)  break;
				this_idx = (choicex * 16) + choicey;

				/* A new color has been selected */
				if (this_idx != idx)
				{
					/* Erase the old cursor */
					old_idx = idx;

					/* Get the new position */
					idx = this_idx;

					/* Request movement */
					do_move = TRUE;
				}
				/* do the 'v' command for double-clicks */
				else do_update = askfor_color_values(idx);
				break;
			}

	  		/* Ask for specific values */
      		case 'v':
			{
	  			do_update = askfor_color_values(idx);
	  			break;
			}
    	}
  	}
}

#endif /* ALLOW_COLORS */


static menu_action color_actions [] =
{
	{'l', "Load a user pref file", colors_pref_load, NULL},
#ifdef ALLOW_COLORS
	{'d', "Dump colors", colors_pref_dump, 0},
	{'m', "Modify colors", colors_modify, 0},
#endif /* ALLOW_COLORS */
};

/* Return the tag for a menu entry */
static char color_menu_tag(menu_type *menu, int oid)
{
	(void)menu;
	return color_actions[oid].id;
}

/* Display a menu entry */
static void color_menu_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];
	(void)menu;
	(void)width;
	c_prt(attr, color_actions[oid].name, row, col);
}

static const menu_iter color_iter =
{
		color_menu_tag,
		NULL,
		color_menu_display,
		NULL
};

/*
 * Interact with "colors"
 */
void do_cmd_colors(void)
{

	char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };
	menu_type menu;

	int cursor = 0;
	ui_event_data evt = EVENT_EMPTY;

	region area = (mouse_buttons ? SCREEN_REGION_BUTTONS : SCREEN_REGION);

	/* macro menu */
	WIPE(&menu, menu);
	menu.menu_data = color_actions;
	menu.title = "Interact with Colors";
	menu.cmd_keys = cmd_keys;
	menu.menu_data = color_actions;
	menu.count = N_ELEMENTS(color_actions);
	menu_init(&menu, MN_SKIN_SCROLL, &color_iter, &area);

	screen_save();

	while (evt.key != ESCAPE)
	{
		/* Reset the buttons */
		button_kill_all();
		button_add("[ESC]", ESCAPE);
		button_add("[HELP]", '?');
		clear_from(0);
		event_signal(EVENT_MOUSEBUTTONS);

		evt = menu_select(&menu, &cursor, EVT_MOVE);
		if (evt.type == EVT_SELECT)
		{
			if ((size_t) cursor < N_ELEMENTS(color_actions))
			{
				color_actions[cursor].action(color_actions[cursor].data,
							color_actions[cursor].name);

			}
		}
		else if (evt.type == EVT_BUTTON)
		{
			if (evt.key == '?')
			{
				if (game_mode == GAME_NPPMORIA) show_file("m_options.txt", NULL, 0, 0);
				else show_file("options.txt", NULL, 0, 0);
			}
		}
	}

	screen_load();
}


void do_cmd_dictate_note(void)
{
	char note[120];

	/* make the note blank */
	my_strcpy(note, "", sizeof(note));

	/* get the note */
	if (!get_string("Note: ", note, 70)) return;

	/* Record it*/
	do_cmd_note(note, p_ptr->depth);
}

/*
 * Set base delay factor
 */
static void do_cmd_delay(void)
{

	while (TRUE)
	{
		ui_event_data  ke;
		int msec = op_ptr->delay_factor * op_ptr->delay_factor;
		int delay = op_ptr->delay_factor;

		button_kill_all();
		button_add("[ESCAPE]", ESCAPE);
		button_add("[-]", '-');
		button_add("[+]", '+');
		button_add("[HELP]", '?');
		event_signal(EVENT_MOUSEBUTTONS);

		/* Prompt */
		prt(format("Current base delay factor: %d (%d msec)",
			   op_ptr->delay_factor, msec), 22, 0);
		prt("New base delay factor (0-15  +, - or ESC to accept): ", 21, 0);
		prt("Command: Base Delay Factor", 20, 0);

		ke = inkey_ex();
		if (ke.key == ESCAPE) break;
		if (ke.key == '?')
		{
			screen_save();

			show_file("options.txt#delay factor", NULL, 0, 0);

			screen_load();
			continue;
		}
		if (isdigit(ke.key)) delay = D2I(ke.key);
		if (ke.key == '+') delay++;
		else if (ke.key == '-') delay--;
		if (delay > 15) delay = 15;
		if (delay < 0) delay = 0;

		op_ptr->delay_factor = (byte)delay;
	}
}


/*
 * Set hitpoint warning level
 */
static void do_cmd_hp_warn(void)
{

	while (TRUE)
	{
		ui_event_data  ke;
		int warn = op_ptr->hitpoint_warn;

		button_kill_all();
		button_add("[ESCAPE]", ESCAPE);
		button_add("[-]", '-');
		button_add("[+]", '+');
		button_add("[HELP]", '?');
		event_signal(EVENT_MOUSEBUTTONS);

		/* Prompt */
		prt(format("Current hitpoint warning: %d (%d%%)",
					  op_ptr->hitpoint_warn, op_ptr->hitpoint_warn * 10), 22, 0);
		prt("New hitpoint warning (0-9): ", 21, 0);
		prt("Command: Hitpoint Warning", 20, 0);

		ke = inkey_ex();
		if (ke.key == ESCAPE) break;
		if (ke.key == '?')
		{
			screen_save();

			show_file("options.txt#hp warning", NULL, 0, 0);

			screen_load();
			continue;
		}
		if (isdigit(ke.key)) warn = D2I(ke.key);
		if (ke.key == '+') warn++;
		else if (ke.key == '-') warn--;
		if (warn > 9) warn = 9;
		if (warn < 0) warn = 0;

		op_ptr->hitpoint_warn = (byte)warn;
	}

}



/*
 * Set "lazy-movement" delay
 */
static void do_cmd_lazymove_delay(void)
{

	while (TRUE)
	{
		ui_event_data  ke;

		button_kill_all();
		button_add("[ESCAPE]", ESCAPE);
		button_add("[-]", '-');
		button_add("[+]", '+');
		button_add("[HELP]", '?');
		event_signal(EVENT_MOUSEBUTTONS);

		/* Prompt */
		prt(format("Current movement delay: %d (%d msec)",
					   lazymove_delay, lazymove_delay * 10), 22, 0);
		prt("New movement delay: ", 21, 0);
		prt("Command: Movement Delay Factor", 20, 0);

		ke = inkey_ex();
		if (ke.key == ESCAPE) break;
		if (ke.key == '?')
		{
			screen_save();

			show_file("options.txt#lazymove", NULL, 0, 0);

			screen_load();
			continue;
		}
		if (isdigit(ke.key)) lazymove_delay = D2I(ke.key);
		if (ke.key == '+') lazymove_delay++;
		else if (ke.key == '-') lazymove_delay--;
	}
}




/*
 * Write options to a file.
 */
static void do_dump_options(void *unused, const char *title)
{
	(void)unused;
	dump_pref_file(option_dump, "Dump options", 20);
}



/*** Main menu definitions and display ***/

/*
 * Definition of the options menu.
 *
 * XXX Too many entries.
 */

static menu_action option_actions [] =
{
	{'a', "Interface options", do_cmd_options_aux, (void*)0},
	{'b', "Display options", do_cmd_options_aux, (void*)1},
	{'e', "Warning and disturbance options", do_cmd_options_aux, (void*)2},
	{'f', "Birth (difficulty) options", do_cmd_options_aux, (void*)3},
	{'g', "Cheat options", do_cmd_options_aux, (void*)4},
	{0, 0, 0, 0}, /* Load and append */
	{'w', "Subwindow display settings", (action_f) do_cmd_options_win, 0},
	{'s', "Item squelch and Autoinscribe Menu", (action_f) do_cmd_squelch_autoinsc, 0},
	{'d', "Set base delay factor", (action_f) do_cmd_delay, 0},
	{'h', "Set hitpoint warning", (action_f) do_cmd_hp_warn, 0},
	{'i', "Set movement delay", (action_f) do_cmd_lazymove_delay, 0},
	{'l', "Load a user pref file", (action_f) do_cmd_pref_file_hack, (void*)20},
	{'o', "Save options", do_dump_options, 0},
	{0, 0, 0, 0}, /* Interact with */
	{'m', "Interact with macros (advanced)", (action_f) do_cmd_macros, 0},
	{'v', "Interact with visuals (advanced)", (action_f) do_cmd_visuals, 0},
	{'c', "Interact with colours (advanced)", (action_f) do_cmd_colors, 0},
};

static menu_type option_menu;

static char tag_opt_main(menu_type *menu, int oid)
{
	(void)menu;
	if (option_actions[oid].id)
		return option_actions[oid].id;

	return 0;
}

static int valid_opt_main(menu_type *menu, int oid)
{
	(void)menu;
	if (option_actions[oid].name)
		return 1;

	return 0;
}

static void display_opt_main(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];

	(void)menu;
	(void)width;
	if (option_actions[oid].name)
		c_prt(attr, option_actions[oid].name, row, col);
}


static const menu_iter options_iter =
{
	tag_opt_main,
	valid_opt_main,
	display_opt_main,
	NULL
};


/*
 * Display the options main menu.
 */
void do_cmd_options(void)
{
	char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };
	int cursor = 0;
	ui_event_data c = EVENT_EMPTY;

	region area = (mouse_buttons ? SCREEN_REGION_BUTTONS : SCREEN_REGION);

	/* options screen setup */
	menu_type *menu = &option_menu;
	WIPE(menu, menu_type);
	menu->title = "Options Menu";
	menu->menu_data = option_actions;
	menu->flags = MN_CASELESS_TAGS;
	menu->cmd_keys = cmd_keys;
	menu->count = N_ELEMENTS(option_actions);
	menu_init(menu, MN_SKIN_SCROLL, &options_iter, &area);

	screen_save();
	menu_layout(&option_menu, &area);


	while (c.key != ESCAPE)
	{
		clear_from(0);
		button_kill_all();
		button_add("[ESCAPE]", ESCAPE);
		button_add("[HELP]", '?');
		event_signal(EVENT_MOUSEBUTTONS);

		c = menu_select(&option_menu, &cursor, 0);
		if (c.type == EVT_SELECT && option_actions[cursor].action)
		{
			option_actions[cursor].action(option_actions[cursor].data,
			                              option_actions[cursor].name);
		}

		if (c.key == '?')
		{
			screen_save();

			if (game_mode == GAME_NPPMORIA) show_file("m_options.txt", NULL, 0, 0);
			else show_file("options.txt", NULL, 0, 0);

			screen_load();
			continue;
		}

		message_flush();
	}

	screen_load();

	/* Restore the statusline */
	button_kill_all();
	basic_buttons();
	do_cmd_redraw();
}




/*
 * Take notes.  There are two ways this can happen, either in the message recall or
 * a file.  The command should be passed a string, which will automatically be
 * written. -CK-
 */
void do_cmd_note(char *note, int what_depth)
{
	char buf[120];
	int length, length_info;
    char info_note[40];
 	char depths[10];

	/* Default */
	my_strcpy(buf, "", sizeof(buf));

	my_strcpy(buf, note, sizeof(buf));

 	/* Ignore empty notes */
 	if (!buf[0] || (buf[0] == ' ')) return;

	/* If the note taking option is off, write it to the message recall.
 	 */
 	if (!adult_take_notes)
	{
		msg_format("Note: %s", buf);

		/*Done*/
		return;
	}

	/*
	 *Write a note in the notes file
	 *Artifacts use depth artifact created.  All others
	 *use player depth.
	 */

	/*get depth for recording\
	 *mark if item is a quest reward or a chest item
	 */
 	if (what_depth == 0)
 	{
 		my_strcpy(depths, " Town", sizeof(depths));
 	}
 	else if (what_depth == CHEST_LEVEL)
 	{
 		strnfmt(depths, sizeof(depths), "Chest");
 	}
	else if (what_depth == QUEST_LEVEL)
 	{
 		strnfmt(depths, sizeof(depths), "Quest");
 	}
	else
 	{
 		strnfmt(depths, sizeof(depths), " %4d", what_depth * 50);
 	}

 	/* Make preliminary part of note */
    strnfmt(info_note, sizeof(info_note), "|%9lu| %s | %2d  | ", turn, depths, p_ptr->lev);

	/*write the info note*/
	file_putf(notes_file, info_note);

	/*get the length of the notes*/
	length_info = strlen(info_note);
	length = strlen(buf);

	/*break up long notes*/
	if((length + length_info) > LINEWRAP)
	{
		bool keep_going = TRUE;
		int startpoint = 0;
		int endpoint, n;

		while (keep_going)
		{

			/*don't print more than the set linewrap amount*/
			endpoint = startpoint + LINEWRAP - strlen(info_note) + 1;

			/*find a breaking point*/
			while (TRUE)
			{
				/*are we at the end of the line?*/
				if (endpoint >= length)
				{
					/*print to the end*/
					endpoint = length;
					keep_going = FALSE;
					break;
				}

				/* Mark the most recent space or dash in the string */
				else if ((buf[endpoint] == ' ') ||
			    		 (buf[endpoint] == '-')) break;

				/*no spaces in the line, so break in the middle of text*/
				else if (endpoint == startpoint)
				{
					endpoint = startpoint + LINEWRAP - strlen(info_note) + 1;
					break;
				}

				/* check previous char */
				endpoint--;
			}

			/*make a continued note if applicable*/
			if (startpoint) file_putf(notes_file, "|  continued...   |     |  ");

			/* Write that line to file */
			for (n = startpoint; n <= endpoint; n++)
			{
				char ch;

				/* Ensure the character is printable */
				ch = (isprint(buf[n]) ? buf[n] : ' ');

				/* Write out the character */
				file_putf(notes_file, "%c", ch);

			}

			/*break the line*/
			file_putf(notes_file, "\n");

			/*prepare for the next line*/
			startpoint = endpoint + 1;
		}

	}

 	/* Add note to buffer */
 	else
	{
		file_putf(notes_file, "%s", buf);

		/*break the line*/
		file_putf(notes_file, "\n");
	}
}


/*
 * Mention the current version
 */
void do_cmd_version(void)
{
	/* Silly message */
	msg_format("You are playing %s %s.  Type '?' for more info.",
			VERSION_MODE_NAME, VERSION_STRING);
}



/*
 * Array of feeling strings
 */
static cptr do_cmd_feeling_text[LEV_THEME_HEAD] =
{
	"Looks like any other level.",
	"You feel there is something special about this level.",
	"You have a superb feeling about this level.",
	"You have an excellent feeling...",
	"You have a very good feeling...",
	"You have a good feeling...",
	"You feel strangely lucky...",
	"You feel your luck is turning...",
	"You like the look of this place...",
	"This level can't be all bad...",
	"What a boring place..."
};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(void)
{
	bool is_quest_level = quest_check(p_ptr->depth);

	/* No sensing things in Moria */
	if (game_mode == GAME_NPPMORIA) return;

	/* No useful feeling in town */
	if (!p_ptr->depth)
	{
		msg_print("Looks like a typical town.");
		return;
	}

	/* No useful feelings until enough time has passed */
	if (!do_feeling)
	{
		msg_print("You are still uncertain about this level...");
		return;
	}

	if (p_ptr->dungeon_type == DUNGEON_TYPE_WILDERNESS)
	{
		if (is_quest_level) 	msg_print("You have entered a wilderness level on the verge of destruction!.");
		else 					msg_print("You have entered an area of near pristine wilderness.");
	}

	else if (p_ptr->dungeon_type == DUNGEON_TYPE_GREATER_VAULT)
	{
		msg_print("You have discovered a gigantic vault of great treasures guarded by dangerous creatures.");
	}

	else if (p_ptr->dungeon_type == DUNGEON_TYPE_LABYRINTH)
	{
		if (is_quest_level) msg_print("You have entered a tiny, closely guarded labyrinth.");
		else msg_print("You have entered a complex labyrinth of dungeon hallways.");
	}

	else if (p_ptr->dungeon_type == DUNGEON_TYPE_ARENA)
	{
		msg_print("You are in an arena fighting for your life.");
	}

	/* Verify the feeling */
	else if (feeling >= LEV_THEME_HEAD)
	{

		/*print out a message about a themed level*/
		char note[100];
		char mon_theme[80];

		my_strcpy(note, "You have entered ", sizeof(note));
		my_strcpy(mon_theme, feeling_themed_level[feeling - LEV_THEME_HEAD], sizeof(mon_theme));
		if (my_is_vowel(mon_theme[0])) my_strcat(note, "an ", sizeof(note));
		else my_strcat(note, "a ", sizeof(note));

		my_strcat(note, format("%s stronghold.", mon_theme), sizeof(note));

		msg_print(note);
	}

	/* Display the feeling */
	else msg_print(do_cmd_feeling_text[feeling]);

	/* Redraw the feeling indicator */
	p_ptr->redraw |= (PR_FEELING);
}




/*
 * Display the current quest (if any)
 */
void do_cmd_quest(void)
{
	char q_out[120];

	/* Check if you're on a quest */
	if (guild_quest_level() > 0)
	{
 		/* Completed quest */
		if (guild_quest_complete())
		{
			msg_print("Collect your reward at the guild!");
		}
		else
		{
			describe_quest(q_out, sizeof(q_out), guild_quest_level(), QMODE_FULL);

			/* Break into two lines if necessary */
			if (strlen(q_out) < 70) msg_print(q_out);
			else
			{
				describe_quest(q_out, sizeof(q_out), guild_quest_level(), QMODE_HALF_1);
				msg_format(q_out);
				describe_quest(q_out, sizeof(q_out), guild_quest_level(), QMODE_HALF_2);
				msg_format(q_out);
			}
		}
	}
	/* No quest at all */
	else msg_print("You are not currently undertaking a quest.");
}
/*
 * Encode the screen colors
 */
static const char hack[17] = "dwsorgbuDWvyRGBU";


/*
 * Hack -- load a screen dump from a file
 *
 * ToDo: Add support for loading/saving screen-dumps with graphics
 * and pseudo-graphics.  Allow the player to specify the filename
 * of the dump.
 */
void do_cmd_load_screen(void)
{
	int i, y, x;

	byte a = 0;
	char c = ' ';

	bool okay = TRUE;

	ang_file *fp;

	char buf[1024];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "dump.txt");

	/* Open the file */
	fp = file_open(buf, MODE_READ, -1);

	/* Oops */
	if (!fp) return;


	/* Save screen */
	screen_save();


	/* Clear the screen */
	Term_clear();


	/* Load the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (!file_getl(fp, buf, sizeof(buf))) okay = FALSE;


		/* Show each row */
		for (x = 0; x < 79; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Get the blank line */
	if (!file_getl(fp, buf, sizeof(buf))) okay = FALSE;


	/* Dump the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (!file_getl(fp, buf, sizeof(buf))) okay = FALSE;

		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Look up the attr */
			for (i = 0; i < 16; i++)
			{
				/* Use attr matches */
				if (hack[i] == buf[x]) a = i;
			}

			/* Put the attr/char */
			Term_draw(x, y, a, c);
		}
	}


	/* Close it */
	file_close(fp);


	/* Message */
	msg_print("Screen dump loaded.");
	message_flush();


	/* Load screen */
	screen_load();
}



/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
	char tmp_val[256];

	/* Ask for a file */
	my_strcpy(tmp_val, "dump.html", sizeof(tmp_val));
	if (!get_string("File: ", tmp_val, sizeof(tmp_val))) return;

	html_screenshot(tmp_val, 0);
	msg_print("Dump saved.");
}

/*
 * Create and return an empty file in writing mode to append notes.
 * It returns a copy of the file name in "path".
 * "max" must be the maximum size in bytes of "path".
 * Returns NULL on failure
 */
void create_notes_file(void)
{

	char filename[1024];

	/* Hack -- No base name yet */
	if (!op_ptr->base_name[0]) process_player_name(FALSE);

	/* Format the name of the notes file */
	strnfmt(filename, sizeof(filename), "%s.nte", op_ptr->base_name);

	/* Build the full path to the notes file */
	path_build(notes_fname, sizeof(notes_fname), ANGBAND_DIR_USER, filename);

	/* Create it */
	notes_file = file_open(notes_fname, MODE_WRITE, FTYPE_TEXT);

	return;
}

/*
 * Close and destroy the notes file. notes_file and notes_fname variables are cleared
 */
void delete_notes_file(void)
{
	/* Close the notes file */
	if (notes_file)
	{
		file_close(notes_file);

		notes_file = NULL;
	}

	/* Delete the notes file */
	if (notes_fname[0])
	{
		file_delete(notes_fname);

		notes_fname[0] = '\0';
	}
}






