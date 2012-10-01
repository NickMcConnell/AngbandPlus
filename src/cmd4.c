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



/* String used to show a color sample */
#define COLOR_SAMPLE "###"

/*max length of note output*/
#define LINEWRAP	75

/*
 *  Header and footer marker string for pref file dumps
 */
static cptr dump_seperator = "#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#";


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


/* Flag value for missing array entry */
#define MISSING -17

#define APP_MACRO	101
#define ASK_MACRO	103
#define DEL_MACRO	104
#define NEW_MACRO	105
#define APP_KEYMAP	106
#define ASK_KEYMAP	107
#define DEL_KEYMAP	108
#define NEW_KEYMAP	109
#define ENTER_ACT	110
#define LOAD_PREF	111
#define DUMP_MON	112
#define DUMP_OBJ	113
#define DUMP_FEAT	114
#define DUMP_FLAV	115
#define CHANGE_MON  116
#define CHANGE_OBJ  117
#define CHANGE_FEAT 118
#define CHANGE_FLAV 119
#define DUMP_COL	120
#define MOD_COL		121
#define RESET_VIS	122


#define INFO_SCREENS 2 /* Number of screens in character info mode */


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

	/* Forever */
	while (1)
	{
		/* Display the player */
		display_player(mode);

		/* Prompt */
		Term_putstr(1, 23, -1, TERM_WHITE, p);

		/* Query */
		ke = inkey_ex();

		/* Exit */
		if (ke.key == ESCAPE) break;

		/* Change name */
		if ((ke.key == 'c') || ((ke.key == DEFINED_XFF) && (ke.mousey == 2) && (ke.mousex < 26)))
		{
			char namebuf[32] = "";

			if (get_name(namebuf, sizeof namebuf))
			{
				/* Set player name */
				my_strcpy(op_ptr->full_name, namebuf,
						  sizeof(op_ptr->full_name));

				/* Don't change savefile name. */
				process_player_name(FALSE);
			}
		}

		/* File dump */
		else if (ke.key == 'f')
		{
			char buf[1024];
			char fname[80];

			strnfmt(fname, sizeof fname, "%s.txt", op_ptr->base_name);

			if (get_file(fname, buf, sizeof buf))
			{
				if (file_character(buf, FALSE) != 0)
					msg_print("Character dump failed!");
				else
					msg_print("Character dump successful.");
			}
		}

		/* Toggle mode */
		else if ((ke.key == '+') || (ke.key == DEFINED_XFF) || (ke.key == 'h') ||
		         (ke.key == ARROW_LEFT) || (ke.key == ' '))
		{
			mode += 1;

			/*loop through the four screens*/
			if (mode == 5) mode = 0;
			/*don't display home if empty*/
			else if ((!st_ptr->stock_num) && (mode == 2)) mode = 4;
		}

		/* Toggle mode */
		else if ((ke.key == 'l') || ke.key == ARROW_RIGHT || ke.key == '-')
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
		}

		/* Oops */
		else
		{
			bell("Illegal command for change name!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Load screen */
	screen_load();
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

	/* Process requests until done */
	while (1)
	{
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
		    prt("[Movement keys to navigate, '-' for next, '=' to find]", hgt - 1, 0);
		else
			prt("[Movement keys to navigate, '=' to find, or ESCAPE to exit]", hgt - 1, 0);


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

		/* Scroll forwards or backwards using mouse clicks */
		else if (ke.key == DEFINED_XFF)
		{
			if (ke.index)
			{
				if (ke.mousey <= hgt / 2)
				{
					/* Go older if legal */
					if (i + 20 < n) i += 20;
				}
				else
				{
					/* Go newer (if able) */
					i = (i >= 20) ? (i - 20) : 0;
				}
			}
		}

		/* Error time */
		else
		{
			bell(NULL);
		}


		/* Find the next item */
		if (ke.key == '-' && shower[0])
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
	}

	/* Load screen */
	screen_load();
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
static bool update_option(char key, void *pgdb, int oid)
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
			show_file(format("options.txt#%s", option_name(oid)), NULL, 0, 0);
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
 * Interact with some options
 */
static void do_cmd_options_aux(void *vpage, cptr info)
{
	int page = (int)vpage;
	int opt[OPT_PAGE_PER];
	int i, n = 0;
	int cursor_pos = 0;

	menu_type *menu = &option_toggle_menu;
	menu->title = info;
	menu_layout(menu, &SCREEN_REGION);

	screen_save();
	clear_from(0);

	/* Filter the options for this page */
	for (i = 0; i < OPT_PAGE_PER; i++)
	{
		if (option_page[page][i] != OPT_NONE)
			opt[n++] = option_page[page][i];
	}

	menu_set_filter(menu, opt, n);
	menu->menu_data = vpage;

	menu_layout(menu, &SCREEN_REGION);

	while (TRUE)
	{
		ui_event_data cx;

		cx = menu_select(menu, &cursor_pos, EVT_MOVE);

		if (cx.key == ESCAPE)
			break;
		else if (cx.type == EVT_MOVE)
			cursor_pos = cx.index;
		else if (cx.type == EVT_SELECT && strchr("YN", toupper((unsigned char) cx.key)))
			cursor_pos++;

		cursor_pos = (cursor_pos+n) % n;
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
			x = (x + ddx[d] + 8) % ANGBAND_TERM_MAX;
			y = (y + ddy[d] + 16) % PW_MAX_FLAGS;
		}

		/* Oops */
		else
		{
			bell("Illegal command for window options!");
		}
	}

	/* Notice changes */
	subwindows_set_flags(new_flags, ANGBAND_TERM_MAX);
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
 * Hack -- ask for a "trigger" (see below)
 *
 * Note the complex use of the "inkey()" function from "util.c".
 *
 * Note that both "flush()" calls are extremely important.  This may
 * no longer be true, since "util.c" is much simpler now.  XXX XXX XXX
 */
static void do_cmd_macro_aux(char *buf)
{
	char ch;

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
	ch = inkey();


	/* Read the pattern */
	while (ch != 0 && ch != DEFINED_XFF)
	{
		/* Save the key */
		buf[n++] = ch;
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
		ch = inkey();
	}

	/* Convert the trigger */
	ascii_to_text(tmp, sizeof(tmp), buf);
}





/*
 * Hack -- ask for a keymap "trigger" (see below)
 *
 * Note that both "flush()" calls are extremely important.  This may
 * no longer be true, since "util.c" is much simpler now.  XXX XXX XXX
 */
static void do_cmd_macro_aux_keymap(char *buf)
{
	char tmp[1024];


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



/*
 * Interact with "macros"
 *
 * Could use some helpful instructions on this page.  XXX XXX XXX
 * CLEANUP
 */
static menu_action macro_actions[] =
{
	{ LOAD_PREF,  "Load a user pref file",    0, 0 },
#ifdef ALLOW_MACROS
	{ APP_MACRO,  "Append macros to a file",  0, 0 },
	{ ASK_MACRO,  "Query a macro",            0, 0 },
	{ NEW_MACRO,  "Create a macro",           0, 0 },
	{ DEL_MACRO,  "Remove a macro",           0, 0 },
	{ APP_KEYMAP, "Append keymaps to a file", 0, 0 },
	{ ASK_KEYMAP, "Query a keymap",           0, 0 },
	{ NEW_KEYMAP, "Create a keymap",          0, 0 },
	{ DEL_KEYMAP, "Remove a keymap",          0, 0 },
	{ ENTER_ACT,  "Enter a new action",       0, 0 },
#endif /* ALLOW_MACROS */
};

static menu_type macro_menu;

void do_cmd_macros(void)
{

	char tmp[1024];

	char pat[1024];

	int mode;
	int cursor = 0;

	region loc = {0, 0, 0, 12};

	if (rogue_like_commands)
		mode = KEYMAP_MODE_ROGUE;
	else
		mode = KEYMAP_MODE_ORIG;


	screen_save();

	menu_layout(&macro_menu, &loc);

	/* Process requests until done */
	while (1)
	{
		ui_event_data c;
		int evt;

		/* Clear screen */
		clear_from(0);

		/* Describe current action */
		prt("Current action (if any) shown below:", 13, 0);

		/* Analyze the current action */
		ascii_to_text(tmp, sizeof(tmp), macro_buffer);

		/* Display the current action */
		prt(tmp, 14, 0);
		c = menu_select(&macro_menu, &cursor, EVT_CMD);


		if (ESCAPE == c.key) break;
		if (c.key == ARROW_LEFT || c.key == ARROW_RIGHT) continue;
		evt = macro_actions[cursor].id;

		switch(evt)
		{
		case LOAD_PREF:
		{
			do_cmd_pref_file_hack(16);
			break;
		}

#ifdef ALLOW_MACROS
		case APP_MACRO:
		{
			/* Dump the macros */
			(void)dump_pref_file(macro_dump, "Dump Macros", 15);

			break;
		}

		case ASK_MACRO:
		{
			int k;

			/* Prompt */
			prt("Command: Query a macro", 16, 0);

			/* Prompt */
			prt("Trigger: ", 18, 0);

			/* Get a macro trigger */
			do_cmd_macro_aux(pat);

			/* Get the action */
			k = macro_find_exact(pat);

			/* Nothing found */
			if (k < 0)
			{
				/* Prompt */
				prt("", 0, 0);
				msg_print("Found no macro.");
			}

			/* Found one */
			else
			{
				/* Obtain the action */
				my_strcpy(macro_buffer, macro__act[k], sizeof(macro_buffer));

				/* Analyze the current action */
				ascii_to_text(tmp, sizeof(tmp), macro_buffer);

				/* Display the current action */
				prt(tmp, 22, 0);

				/* Prompt */
				prt("", 0, 0);
				msg_print("Found a macro.");
			}
			break;
		}

		case NEW_MACRO:
		{
			/* Prompt */
			prt("Command: Create a macro", 16, 0);

			/* Prompt */
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
				msg_print("Added a macro.");
			}
			break;
		}

		case DEL_MACRO:
		{
			/* Prompt */
			prt("Command: Remove a macro", 16, 0);

			/* Prompt */
			prt("Trigger: ", 18, 0);

			/* Get a macro trigger */
			do_cmd_macro_aux(pat);

			/* Link the macro */
			macro_add(pat, pat);

			/* Prompt */
			prt("", 0, 0);
			msg_print("Removed a macro.");
			break;
		}
		case APP_KEYMAP:
		{
			/* Dump the keymaps */
			(void)dump_pref_file(keymap_dump, "Dump Keymaps", 15);
			break;
		}
		case ASK_KEYMAP:
		{
			cptr act;

			/* Prompt */
			prt("Command: Query a keymap", 16, 0);

			/* Prompt */
			prt("Keypress: ", 18, 0);

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(pat);

			/* Look up the keymap */
			act = keymap_act[mode][(byte)(pat[0])];

			/* Nothing found */
			if (!act)
			{
				/* Prompt */
				prt("", 0, 0);
				msg_print("Found no keymap.");
			}

			/* Found one */
			else
			{
				/* Obtain the action */
				my_strcpy(macro_buffer, act, sizeof(macro_buffer));

				/* Analyze the current action */
				ascii_to_text(tmp, sizeof(tmp), macro_buffer);

				/* Display the current action */
				prt(tmp, 22, 0);

				/* Prompt */
				prt("", 0, 0);
				msg_print("Found a keymap.");
			}
			break;
		}
		case NEW_KEYMAP:
		{
			/* Prompt */
			prt("Command: Create a keymap", 16, 0);

			/* Prompt */
			prt("Keypress: ", 18, 0);

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(pat);

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

				/* Free old keymap */
				string_free(keymap_act[mode][(byte)(pat[0])]);

				/* Make new keymap */
				keymap_act[mode][(byte)(pat[0])] = string_make(macro_buffer);

				/* Prompt */
				prt("", 0, 0);
				msg_print("Added a keymap.");
			}
			break;
		}
		case DEL_KEYMAP:
		{
			/* Prompt */
			prt("Command: Remove a keymap", 16, 0);

			/* Prompt */
			prt("Keypress: ", 18, 0);

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(pat);

			/* Free old keymap */
			string_free(keymap_act[mode][(byte)(pat[0])]);

			/* Make new keymap */
			keymap_act[mode][(byte)(pat[0])] = NULL;

			/* Prompt */
			prt("", 0, 0);
			msg_print("Removed a keymap.");
			break;
		}
		case ENTER_ACT: /* Enter a new action */
		{
			/* Prompt */
			prt("Command: Enter a new action", 16, 0);

			/* Go to the correct location */
			Term_gotoxy(0, 22);

			/* Analyze the current action */
			ascii_to_text(tmp, sizeof(tmp), macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, sizeof tmp, NULL))
			{
				/* Extract an action */
				text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);
			}
			break;
		}
#endif /* ALLOW_MACROS */
		}

		/* Flush messages */
		message_flush();
	}

	/* Load screen */
	screen_load();
}

#ifdef ALLOW_VISUALS

static void change_monster_visuals(void)
{
	static int r = 0;
	int cx;

	/* Prompt */
	prt("Command: Change monster attr/chars", 15, 0);

	/* Hack -- query until done */
	while (1)
	{
		monster_race *r_ptr = &r_info[r];

		byte da = (byte)(r_ptr->d_attr);
		byte dc = (byte)(r_ptr->d_char);
		byte ca = (byte)(r_ptr->x_attr);
		byte cc = (byte)(r_ptr->x_char);

		/* Label the object */
		Term_putstr(5, 17, -1, TERM_WHITE,
				format("Monster = %d, Name = %-40.40s", r, (r_name + r_ptr->name)));

		/* Label the Default values */
		Term_putstr(10, 19, -1, TERM_WHITE,
		             format("Default attr/char = %3u / %3u", da, dc));
		Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 19, da, dc);

		if (use_bigtile)
		{
			if (da & 0x80)
				Term_putch(44, 19, 255, -1);
			else
				Term_putch(44, 19, 0, ' ');
		}

		/* Label the Current values */
		Term_putstr(10, 20, -1, TERM_WHITE,
		            format("Current attr/char = %3u / %3u", ca, cc));
		Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 20, ca, cc);

		if (use_bigtile)
		{
			if (ca & 0x80)
			{
				Term_putch(44, 20, 255, -1);
			}
			else
			{
				Term_putch(44, 20, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 22, -1, TERM_WHITE, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		cx = inkey();

		/* All done */
		if (cx == ESCAPE) break;

		/* Analyze */
		if (cx == 'n') r = (r + z_info->r_max + 1) % z_info->r_max;
		if (cx == 'N') r = (r + z_info->r_max - 1) % z_info->r_max;
		if (cx == 'a') r_ptr->x_attr = (byte)(ca + 1);
		if (cx == 'A') r_ptr->x_attr = (byte)(ca - 1);
		if (cx == 'c') r_ptr->x_char = (byte)(cc + 1);
		if (cx == 'C') r_ptr->x_char = (byte)(cc - 1);
	}
}

static void change_object_visuals(void)
{
	static int k = 0;
	int cx;

	/* Prompt */
	prt("Command: Change object attr/chars", 15, 0);

	/* Hack -- query until done */
	while (1)
	{
		object_kind *k_ptr = &k_info[k];

		byte da = (byte)(k_ptr->d_attr);
		byte dc = (byte)(k_ptr->d_char);
		byte ca = (byte)(k_ptr->x_attr);
		byte cc = (byte)(k_ptr->x_char);

		/* Label the object */
		Term_putstr(5, 17, -1, TERM_WHITE, format("Object = %d, Name = %-40.40s",
					    k, (k_name + k_ptr->name)));

		/* Label the Default values */
		Term_putstr(10, 19, -1, TERM_WHITE, format("Default attr/char = %3d / %3d",
				da, dc));
		Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 19, da, dc);

		if (use_bigtile)
		{
			if (da & 0x80)
			{
				Term_putch(44, 19, 255, -1);
			}
			else
			{
				Term_putch(44, 19, 0, ' ');
			}
		}

		/* Label the Current values */
		Term_putstr(10, 20, -1, TERM_WHITE, format("Current attr/char = %3d / %3d",
			             ca, cc));
		Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 20, ca, cc);

		if (use_bigtile)
		{
			if (ca & 0x80)
			{
				Term_putch(44, 20, 255, -1);
			}
			else
			{
				Term_putch(44, 20, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 22, -1, TERM_WHITE, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		cx = inkey();

		/* All done */
		if (cx == ESCAPE) break;

		/* Analyze */
		if (cx == 'n') k = (k + z_info->k_max + 1) % z_info->k_max;
		if (cx == 'N') k = (k + z_info->k_max - 1) % z_info->k_max;
		if (cx == 'a') k_info[k].x_attr = (byte)(ca + 1);
		if (cx == 'A') k_info[k].x_attr = (byte)(ca - 1);
		if (cx == 'c') k_info[k].x_char = (byte)(cc + 1);
		if (cx == 'C') k_info[k].x_char = (byte)(cc - 1);
	}
}

static void change_feature_visuals(void)
{
	static int f = 0;
	int cx;

	/* Prompt */
	prt("Command: Change feature attr/chars", 15, 0);

	/* Hack -- query until done */
	while (1)
	{
		feature_type *f_ptr = &f_info[f];
		char name[80];

		byte da = (byte)(f_ptr->d_attr);
		byte dc = (byte)(f_ptr->d_char);
		byte ca = (byte)(f_ptr->x_attr);
		byte cc = (byte)(f_ptr->x_char);

		/* Get feature name */
		feature_desc(name, sizeof(name), f, FALSE, FALSE);

		/* Label the object */
		Term_putstr(5, 17, -1, TERM_WHITE,
			            format("Terrain = %d, Name = %-40.40s", f, name));

		/* Label the Default values */
		Term_putstr(10, 19, -1, TERM_WHITE,
		            format("Default attr/char = %3d / %3d", da, dc));
		Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 19, da, dc);

		if (use_bigtile)
		{
			if (da & 0x80)
			{
				Term_putch(44, 19, 255, -1);
			}
			else
			{
				Term_putch(44, 19, 0, ' ');
			}
		}

		/* Label the Current values */
		Term_putstr(10, 20, -1, TERM_WHITE,
		            format("Current attr/char = %3d / %3d", ca, cc));
		Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
					Term_putch(43, 20, ca, cc);

		if (use_bigtile)
		{
			if (ca & 0x80)
			{
				Term_putch(44, 20, 255, -1);
			}
			else
			{
				Term_putch(44, 20, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 22, -1, TERM_WHITE, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		cx = inkey();

		/* All done */
		if (cx == ESCAPE) break;

		/* Analyze */
		if (cx == 'n') f = (f + z_info->f_max + 1) % z_info->f_max;
		if (cx == 'N') f = (f + z_info->f_max - 1) % z_info->f_max;
		if (cx == 'a') f_info[f].x_attr = (byte)(ca + 1);
		if (cx == 'A') f_info[f].x_attr = (byte)(ca - 1);
		if (cx == 'c') f_info[f].x_char = (byte)(cc + 1);
		if (cx == 'C') f_info[f].x_char = (byte)(cc - 1);
	}
}

static void change_flavor_visuals(void)
{
	int cx;

	static int f = 0;

	/* Prompt */
	prt("Command: Change flavor attr/chars", 15, 0);

	/* Hack -- query until done */
	while (1)
	{
		flavor_type *flavor_ptr = &flavor_info[f];

		byte da = (byte)(flavor_ptr->d_attr);
		byte dc = (byte)(flavor_ptr->d_char);
		byte ca = (byte)(flavor_ptr->x_attr);
		byte cc = (byte)(flavor_ptr->x_char);

		/* Label the object */
		Term_putstr(5, 17, -1, TERM_WHITE,
		            format("Flavor = %d, Text = %-40.40s",
		                   f, (flavor_text + flavor_ptr->text)));

		/* Label the Default values */
		Term_putstr(10, 19, -1, TERM_WHITE,
			            format("Default attr/char = %3d / %3d", da, dc));
		Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 19, da, dc);
		Term_putch(43, 19, da, dc);

		if (use_bigtile)
		{
			if (da & 0x80)
			{
				Term_putch(44, 19, 255, -1);
			}
			else
			{
				Term_putch(44, 19, 0, ' ');
			}
		}

		/* Label the Current values */
		Term_putstr(10, 20, -1, TERM_WHITE,
			            format("Current attr/char = %3d / %3d", ca, cc));
		Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 20, ca, cc);

		if (use_bigtile)
		{
			if (ca & 0x80)
			{
				Term_putch(44, 20, 255, -1);
			}
			else
			{
				Term_putch(44, 20, 0, ' ');
			}
		}

		/* Prompt */
		Term_putstr(0, 22, -1, TERM_WHITE, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		cx = inkey();

		/* All done */
		if (cx == ESCAPE) break;

		/* Analyze */
		if (cx == 'n') f = (f + z_info->flavor_max + 1) % z_info->flavor_max;
		if (cx == 'N') f = (f + z_info->flavor_max - 1) % z_info->flavor_max;
		if (cx == 'a') flavor_info[f].x_attr = (byte)(ca + 1);
		if (cx == 'A') flavor_info[f].x_attr = (byte)(ca - 1);
		if (cx == 'c') flavor_info[f].x_char = (byte)(cc + 1);
		if (cx == 'C') flavor_info[f].x_char = (byte)(cc - 1);
	}
}


#endif /* ALLOW_VISUALS */

menu_action visual_menu_items [] =
{
	{ LOAD_PREF, 	"Load a user pref file", 0, 0},
	{ DUMP_MON,  	"Dump monster attr/chars", 0, 0},
	{ DUMP_OBJ,  	"Dump object attr/chars", 0, 0 },
	{ DUMP_FEAT, 	"Dump feature attr/chars", 0, 0 },
	{ DUMP_FLAV, 	"Dump flavor attr/chars", 0, 0 },
	{ CHANGE_MON,  	"Change monster attr/chars", 0, 0},
	{ CHANGE_OBJ,  	"Change object attr/chars", 0, 0 },
	{ CHANGE_FEAT, 	"Change feature attr/chars", 0, 0 },
	{ CHANGE_FLAV, 	"Change flavor attr/chars", 0, 0 },
	{ RESET_VIS, 	"Reset visuals", 0, 0 },
};

static menu_type visual_menu;

/*
 * Interact with "visuals"
 */
void do_cmd_visuals(void)
{
	int cursor = 0;

	/* Save screen */
	screen_save();

	menu_layout(&visual_menu, &SCREEN_REGION);

	/* Interact until done */
	while (1)
	{
		ui_event_data key;
		int evt = -1;
		clear_from(0);
		key = menu_select(&visual_menu, &cursor, EVT_CMD);
		if (key.key == ESCAPE)
			break;

		if (key.key == ARROW_LEFT || key.key == ARROW_RIGHT)
			continue;

		assert(cursor >= 0 && cursor < visual_menu.count);

		evt = visual_menu_items[cursor].id;

		if (evt == LOAD_PREF)
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(15);
		}

#ifdef ALLOW_VISUALS

		else if (evt == DUMP_MON)
		{
			dump_pref_file(dump_monsters, "Dump Monster attr/chars", 15);
		}

		else if (evt == DUMP_OBJ)
		{
			dump_pref_file(dump_objects, "Dump Object attr/chars", 15);
		}

		else if (evt == DUMP_FEAT)
		{
			dump_pref_file(dump_features, "Dump Feature attr/chars", 15);
		}

		/* Dump flavor attr/chars */
		else if (evt == DUMP_FLAV)
		{
			dump_pref_file(dump_flavors, "Dump Flavor attr/chars", 15);
		}

		else if (evt == CHANGE_MON)
		{
			change_monster_visuals();
		}

		else if (evt == CHANGE_OBJ)
		{
			change_object_visuals();
		}
		else if (evt == CHANGE_FEAT)
		{
			change_feature_visuals();
		}
		else if (evt == CHANGE_FLAV)
		{
			change_flavor_visuals();
		}

#endif /* ALLOW_VISUALS */

		/* Reset visuals */
		else if (evt == RESET_VIS)
		{
			/* Reset */
			reset_visuals(TRUE);

			/* Message */
			prt("", 0, 0);
			msg_print("Visual attr/char tables reset.");
		}

		message_flush();
	}

	/* Load screen */
	screen_load();
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


/* These two are used to place elements in the grid */
#define COLOR_X(idx) (((idx) / MAX_BASE_COLORS) * 5 + 1)
#define COLOR_Y(idx) ((idx) % MAX_BASE_COLORS + 6)


/* Hack - Note the cast to "int" to prevent overflow */
#define IS_BLACK(idx) \
((int)angband_color_table[idx][1] + (int)angband_color_table[idx][2] + \
 (int)angband_color_table[idx][3] == 0)

/* We show black as dots to see the shape of the grid */
#define BLACK_SAMPLE "..."

/*
 * The screen used to modify the color table. Only 128 colors can be modified.
 * The remaining entries of the color table are reserved for graphic mode.
 */
static void modify_colors(void)
{
	int x, y, idx, old_idx;
	char ch;
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
  	y = 2;
  	x = 42;
  	c_put_str(TERM_WHITE, "Commands:", y, x);
  	c_put_str(TERM_WHITE, "ESC: Return", y + 2, x);
  	c_put_str(TERM_WHITE, "Arrows: Move to color", y + 3, x);
  	c_put_str(TERM_WHITE, "k,K: Incr,Decr extra value", y + 4, x);
  	c_put_str(TERM_WHITE, "r,R: Incr,Decr red value", y + 5, x);
  	c_put_str(TERM_WHITE, "g,G: Incr,Decr green value", y + 6, x);
  	c_put_str(TERM_WHITE, "b,B: Incr,Decr blue value", y + 7, x);
  	c_put_str(TERM_WHITE, "c: Copy from color", y + 8, x);
  	c_put_str(TERM_WHITE, "v: Set specific values", y + 9, x);
  	c_put_str(TERM_WHITE, "First column: base colors", y + 11, x);
  	c_put_str(TERM_WHITE, "Second column: first shade, etc.", y + 12, x);

  	c_put_str(TERM_WHITE, "Shades look like base colors in 16 color ports.",
      			23, 0);

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
    	if (!get_com("Command: Modify colors ", &ch)) break;

    	switch(ch)
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

	  		/* Ask for specific values */
      		case 'v':
			{
	  			do_update = askfor_color_values(idx);
	  			break;
			}
    	}
  	}
}

static menu_action color_events [] =
{
	{LOAD_PREF, "Load a user pref file", 0, 0},
#ifdef ALLOW_COLORS
	{DUMP_COL, "Dump colors", 0, 0},
	{MOD_COL, "Modify colors", 0, 0}
#endif
};

static menu_type color_menu;


/*
 * Interact with "colors"
 */
void do_cmd_colors(void)
{
	int ch;

	int i;

	ang_file *fff;

	char buf[1024];

	/* Save screen */
	screen_save();

	/* Interact until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Interact with Colors", 2, 0);

		/* Give some choices */
		prt("(1) Load a user pref file", 4, 5);

		prt("(2) Dump colors", 5, 5);
		prt("(3) Modify colors", 6, 5);


		/* Prompt */
		prt("Command: ", 8, 0);

		/* Prompt */
		ch = inkey();

		/* Done */
		if (ch == ESCAPE) break;

		/* Load a user pref file */
		if (ch == '1')
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(8);

			/* Could skip the following if loading cancelled XXX XXX XXX */

			/* Mega-Hack -- React to color changes */
			Term_xtra(TERM_XTRA_REACT, 0);

			/* Mega-Hack -- Redraw physical windows */
			Term_redraw();
		}



		/* Dump colors */
		else if (ch == '2')
		{
			static cptr mark = "Colors";
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump colors", 8, 0);

			/* Prompt */
			prt("File: ", 10, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, sizeof(ftmp), NULL)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* Remove old colors */
			remove_old_dump(buf, mark);

			/* Append to the file */
			fff = file_open(buf, MODE_APPEND, FTYPE_TEXT);

			/* Failure */
			if (!fff) continue;

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

		/* Edit colors */
		else if (ch == '3')
		{
			modify_colors();
		}



		/* Unknown option */
		else
		{
			bell("Illegal command for colors!");
		}

		/* Flush messages */
		message_flush();
	}


	/* Load screen */
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

/*** Non-complex menu actions ***/

static bool askfor_aux_numbers(char *buf, size_t buflen, size_t *curs, size_t *len, char keypress, bool firsttime)
{
	switch (keypress)
	{
		case ESCAPE:
		case '\n':
		case '\r':
		case ARROW_LEFT:
		case ARROW_RIGHT:
		case 0x7F:
		case '\010':
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			return askfor_aux_keypress(buf, buflen, curs, len, keypress, firsttime);
	}

	return FALSE;
}


/*
 * Set base delay factor
 */
static void do_cmd_delay(void)
{
	bool res;
	char tmp[4] = "";
	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	strnfmt(tmp, sizeof(tmp), "%i", op_ptr->delay_factor);

	/* Prompt */
	prt("Command: Base Delay Factor", 20, 0);

	prt(format("Current base delay factor: %d (%d msec)",
			   op_ptr->delay_factor, msec), 22, 0);
	prt("New base delay factor (0-255): ", 21, 0);

	/* Ask the user for a string */
	res = askfor_aux(tmp, sizeof(tmp), askfor_aux_numbers);

	/* Process input */
	if (res)
	{
		op_ptr->delay_factor = (u16b) strtoul(tmp, NULL, 0);
	}
}


/*
 * Set hitpoint warning level
 */
static void do_cmd_hp_warn(void)
{
	bool res;
	char tmp[4] = "";
	u16b warn;

	strnfmt(tmp, sizeof(tmp), "%i", op_ptr->hitpoint_warn);

	/* Prompt */
	prt("Command: Hitpoint Warning", 20, 0);

	prt(format("Current hitpoint warning: %d (%d%%)",
			   op_ptr->hitpoint_warn, op_ptr->hitpoint_warn * 10), 22, 0);
	prt("New hitpoint warning (0-9): ", 21, 0);

	/* Ask the user for a string */
	res = askfor_aux(tmp, sizeof(tmp), askfor_aux_numbers);

	/* Process input */
	if (res)
	{
		warn = (u16b) strtoul(tmp, NULL, 0);

		/* Reset nonsensical warnings */
		if (warn > 9)
			warn = 0;

		op_ptr->hitpoint_warn = warn;
	}
}


/*
 * Set "lazy-movement" delay
 */
static void do_cmd_lazymove_delay(void)
{
	bool res;
	char tmp[4] = "";

	strnfmt(tmp, sizeof(tmp), "%i", lazymove_delay);

	/* Prompt */
	prt("Command: Movement Delay Factor", 20, 0);

	prt(format("Current movement delay: %d (%d msec)",
			   lazymove_delay, lazymove_delay * 10), 22, 0);
	prt("New movement delay: ", 21, 0);

	/* Ask the user for a string */
	res = askfor_aux(tmp, sizeof(tmp), askfor_aux_numbers);

	/* Process input */
	if (res)
	{
		lazymove_delay = (u16b) strtoul(tmp, NULL, 0);
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
	int cursor = 0;
	ui_event_data c = EVENT_EMPTY;

	screen_save();
	menu_layout(&option_menu, &SCREEN_REGION);

	while (c.key != ESCAPE)
	{
		clear_from(0);
		c = menu_select(&option_menu, &cursor, 0);
		if (c.type == EVT_SELECT && option_actions[cursor].action)
		{
			option_actions[cursor].action(option_actions[cursor].data,
			                              option_actions[cursor].name);
		}

		message_flush();
	}

	screen_load();
}




/*
 * Initialise all menus used here.
 */
void init_cmd4_c(void)
{
	/* some useful standard command keys */
	static const char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };

	/* Initialize the menus */
	menu_type *menu;

	/* options screen selection menu */
	menu = &option_menu;
	WIPE(menu, menu_type);
	menu->title = "Options Menu";
	menu->menu_data = option_actions;
	menu->flags = MN_CASELESS_TAGS;
	menu->cmd_keys = cmd_keys;
	menu->count = N_ELEMENTS(option_actions);
	menu_init(menu, MN_SKIN_SCROLL, &options_iter, &SCREEN_REGION);

	/* Initialize the options toggle menu */
	menu = &option_toggle_menu;
	WIPE(menu, menu_type);
	menu->prompt = "Set option (y/n/t), '?' for information";
	menu->cmd_keys = "?Yy\n\rNnTt\x8C"; /* \x8c = ARROW_RIGHT */
	menu->selections = "abcdefghijklmopqrsuvwxz";
	menu->count = OPT_PAGE_PER;
	menu->flags = MN_DBL_TAP;
	menu_init(menu, MN_SKIN_SCROLL, &options_toggle_iter, &SCREEN_REGION);

	/* macro menu */
	menu = &macro_menu;
	WIPE(menu, menu_type);
	menu->title = "Interact with macros";
	menu->cmd_keys = cmd_keys;
	menu->selections = lower_case;
	menu->menu_data = macro_actions;
	menu->count = N_ELEMENTS(macro_actions);
	menu_init(menu, MN_SKIN_SCROLL, find_menu_iter(MN_ITER_ACTIONS), &SCREEN_REGION);

	/* visuals menu */
	menu = &visual_menu;
	WIPE(menu, menu_type);
	menu->title = "Interact with visuals";
	menu->cmd_keys = cmd_keys;
	menu->selections = lower_case;
	menu->menu_data = visual_menu_items;
	menu->count = N_ELEMENTS(visual_menu_items);
	menu_init(menu, MN_SKIN_SCROLL, find_menu_iter(MN_ITER_ACTIONS), &SCREEN_REGION);

	/* colors menu */
	menu = &color_menu;
	WIPE(menu, menu_type);
	menu->title = "Interact with colors";
	menu->cmd_keys = cmd_keys;
	menu->selections = lower_case;
	menu->menu_data = color_events;
	menu->count = N_ELEMENTS(color_events);
	menu_init(menu, MN_SKIN_SCROLL, find_menu_iter(MN_ITER_ACTIONS), &SCREEN_REGION);

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
	           VERSION_NAME, VERSION_STRING);
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

	/* Verify the feeling */
	if (feeling >= LEV_THEME_HEAD)
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
 * Array of feeling strings
 */
static cptr do_cmd_challenge_text[14] =
{
	"challenges you from beyond the grave!",
	"thunders 'Prove worthy of your traditions - or die ashamed!'.",
	"desires to test your mettle!",
	"has risen from the dead to test you!",
	"roars 'Fight, or know yourself for a coward!'.",
	"summons you to a duel of life and death!",
	"desires you to know that you face a mighty champion of yore!",
	"demands that you prove your worthiness in combat!",
	"calls you unworthy of your ancestors!",
	"challenges you to a deathmatch!",
	"walks Middle-Earth once more!",
	"challenges you to demonstrate your prowess!",
	"demands you prove yourself here and now!",
	"asks 'Can ye face the best of those who came before?'."
};




/*
 * Personalize, randomize, and announce the challenge of a player ghost. -LM-
 */
void ghost_challenge(void)
{
	monster_race *r_ptr = &r_info[r_ghost];

	/*paranoia*/
	/* Check there is a name/ghost first */
	if (ghost_name[0] == '\0')
	{
		/*there wasn't a ghost*/
		bones_selector = 0;
		return;
	}

	msg_format("%^s, the %^s %s", ghost_name, r_name + r_ptr->name,
		do_cmd_challenge_text[rand_int(14)]);

	message_flush();
}


/*
 * Display the current quest (if any)
 */
void do_cmd_quest(void)
{
	char q_out[120];

	quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

	/* Check if you're on a quest */
	if (p_ptr->cur_quest > 0)
	{
		/* Completed quest */
		if (!q_ptr->active_level)
		{
			msg_print("Collect your reward at the guild!");
		}

		else
		{
			describe_quest(q_out, sizeof(q_out), p_ptr->cur_quest, QMODE_FULL);

			/* Break into two lines if necessary */
			if (strlen(q_out) < 70) msg_print(q_out);
			else
			{
				describe_quest(q_out, sizeof(q_out), p_ptr->cur_quest, QMODE_HALF_1);
				msg_format(q_out);
				describe_quest(q_out, sizeof(q_out), p_ptr->cur_quest, QMODE_HALF_2);
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






