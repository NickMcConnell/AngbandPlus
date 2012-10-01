/* File: cmd4.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* String used to show a color sample */
#define COLOR_SAMPLE "###"

/*max length of note output*/
#define LINEWRAP	75

/*used for knowledge display*/
#define BROWSER_ROWS			(Term->hgt - 8)


/*
 *  Header and footer marker string for pref file dumps
 */
static cptr dump_seperator = "#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#";

typedef struct monster_list_entry monster_list_entry;
/*
 * Structure for building monster "lists"
 */
struct monster_list_entry
{
	s16b r_idx;			/* Monster race index */

	byte amount;
};



/*
 * Remove old lines from pref files
 */
static void remove_old_dump(cptr orig_file, cptr mark)
{
	FILE *tmp_fff, *orig_fff;

	char tmp_file[1024];
	char buf[1024];
	bool between_marks = FALSE;
	bool changed = FALSE;
	char expected_line[1024];

	/* Open an old dump file in read-only mode */
	orig_fff = my_fopen(orig_file, "r");

	/* If original file does not exist, nothing to do */
	if (!orig_fff) return;

	/* Open a new temporary file */
	tmp_fff = my_fopen_temp(tmp_file, sizeof(tmp_file));

	if (!tmp_fff)
	{
	    msg_format("Failed to create temporary file %s.", tmp_file);
	    msg_print(NULL);
	    return;
	}

	strnfmt(expected_line, sizeof(expected_line),
	        "%s begin %s", dump_seperator, mark);

	/* Loop for every line */
	while (TRUE)
	{
		/* Read a line */
		if (my_fgets(orig_fff, buf, sizeof(buf)))
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
			fprintf(tmp_fff, "%s\n", buf);
		}
	}

	/* Close files */
	my_fclose(orig_fff);
	my_fclose(tmp_fff);

	/* If there are changes, overwrite the original file with the new one */
	if (changed)
	{
		/* Copy contents of temporary file */
		tmp_fff = my_fopen(tmp_file, "r");
		orig_fff = my_fopen(orig_file, "w");

		while (!my_fgets(tmp_fff, buf, sizeof(buf)))
		{
			fprintf(orig_fff, "%s\n", buf);
		}

		my_fclose(orig_fff);
		my_fclose(tmp_fff);
	}

	/* Kill the temporary file */
	fd_kill(tmp_file);
}


/*
 * Output the header of a pref-file dump
 */
static void pref_header(FILE *fff, cptr mark)
{
	/* Start of dump */
	fprintf(fff, "%s begin %s\n", dump_seperator, mark);

	fprintf(fff, "# *Warning!*  The lines below are an automatic dump.\n");
	fprintf(fff, "# Don't edit them; changes will be deleted and replaced automatically.\n");
}


/*
 * Output the footer of a pref-file dump
 */
static void pref_footer(FILE *fff, cptr mark)
{
	fprintf(fff, "# *Warning!*  The lines above are an automatic dump.\n");
	fprintf(fff, "# Don't edit them; changes will be deleted and replaced automatically.\n");

	/* End of dump */
	fprintf(fff, "%s end %s\n", dump_seperator, mark);
}


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


	/* Hack -- React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);


	/* Combine and Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);


	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_NATIVE);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1 |
	                  PW_MESSAGE | PW_OVERHEAD | PW_MONSTER | PW_OBJECT |
	                  PW_MAP | PW_MONLIST | PW_FEATURE);


	/* Clear screen */
	Term_clear();

	/* Hack -- update */
	handle_stuff();

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
	char ch;

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
		ch = inkey();

		/* Exit */
		if (ch == ESCAPE) break;

		/* Change name */
		if (ch == 'c')
		{
			get_name();
		}

		/* File dump */
		else if (ch == 'f')
		{
			char ftmp[80];

			strnfmt(ftmp, sizeof(ftmp), "%s.txt", op_ptr->base_name);

			if (get_string("File name: ", ftmp, sizeof(ftmp)))
			{
				if (ftmp[0] && (ftmp[0] != ' '))
				{
					if (file_character(ftmp, FALSE))
					{
						msg_print("Character dump failed!");
					}
					else
					{
						msg_print("Character dump successful.");
					}
				}
			}
		}

		/* Toggle mode */
		else if ((ch == '+') || (ch == 'l'))
		{
			mode += 1;

			/*loop through the four screens*/
			if (mode == 5) mode = 0;
			/*don't display home if empty*/
			else if ((!st_ptr->stock_num) && (mode == 2)) mode = 4;
		}

		/* Toggle mode */
		else if ((ch == '-') || (ch == 'h'))
		{
			/*loop through the four screens*/
			/*loop through the five screens*/
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
 * Attempt to only hilite the matching portions of the string.
 */
void do_cmd_messages(void)
{
	char ch;

	int i, j, n, q;
	int wid, hgt;

	char shower[80];
	char finder[80];


	/* Wipe finder */
	strcpy(finder, "");

	/* Wipe shower */
	strcpy(shower, "");


	/* Total messages */
	n = message_num();

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
			cptr msg = message_str((s16b)(i+j));
			byte attr = message_color((s16b)(i+j));

			/* Apply horizontal scroll */
			msg = ((int)strlen(msg) >= q) ? (msg + q) : "";

			/* Dump the messages, bottom to top */
			Term_putstr(0, hgt - 3 - j, -1, attr, msg);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = msg;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-msg, hgt - 3 - j, len, TERM_YELLOW, shower);

					/* Advance */
					str += len;
				}
			}
		}

		/* Display header XXX XXX XXX */
		prt(format("Message Recall (%d-%d of %d), Offset %d",
		           i, i + j - 1, n, q), 0, 0);

		/* Display prompt (not very informative) */
		prt("[Press 'p' for older, 'n' for newer, ..., or ESCAPE]", hgt - 1, 0);

		/* Get a command */
		ch = inkey();

		/* Exit on Escape */
		if (ch == ESCAPE) break;

		/* Hack -- Save the old index */
		j = i;

		/* Horizontal scroll */
		if (ch == '4')
		{
			/* Scroll left */
			q = (q >= wid / 2) ? (q - wid / 2) : 0;

			/* Success */
			continue;
		}

		/* Horizontal scroll */
		if (ch == '6')
		{
			/* Scroll right */
			q = q + wid / 2;

			/* Success */
			continue;
		}

		/* Hack -- handle show */
		if (ch == '=')
		{
			/* Prompt */
			prt("Show: ", hgt - 1, 0);

			/* Get a "shower" string, or continue */
			if (!askfor_aux(shower, sizeof(shower))) continue;

			/* Okay */
			continue;
		}

		/* Hack -- handle find */
		if (ch == '/')
		{
			s16b z;

			/* Prompt */
			prt("Find: ", hgt - 1, 0);

			/* Get a "finder" string, or continue */
			if (!askfor_aux(finder, sizeof(finder))) continue;

			/* Show it */
			my_strcpy(shower, finder, sizeof(shower));

			/* Scan messages */
			for (z = i + 1; z < n; z++)
			{
				cptr msg = message_str(z);

				/* Search for it */
				if (strstr(msg, finder))
				{
					/* New location */
					i = z;

					/* Done */
					break;
				}
			}
		}

		/* Recall 20 older messages */
		if ((ch == 'p') || (ch == KTRL('P')) || (ch == ' '))
		{
			/* Go older if legal */
			if (i + 20 < n) i += 20;
		}

		/* Recall 10 older messages */
		if (ch == '+')
		{
			/* Go older if legal */
			if (i + 10 < n) i += 10;
		}

		/* Recall 1 older message */
		if ((ch == '8') || (ch == '\n') || (ch == '\r'))
		{
			/* Go newer if legal */
			if (i + 1 < n) i += 1;
		}

		/* Recall 20 newer messages */
		if ((ch == 'n') || (ch == KTRL('N')))
		{
			/* Go newer (if able) */
			i = (i >= 20) ? (i - 20) : 0;
		}

		/* Recall 10 newer messages */
		if (ch == '-')
		{
			/* Go newer (if able) */
			i = (i >= 10) ? (i - 10) : 0;
		}

		/* Recall 1 newer messages */
		if (ch == '2')
		{
			/* Go newer (if able) */
			i = (i >= 1) ? (i - 1) : 0;
		}

		/* Hack -- Error of some kind */
		if (i == j) bell(NULL);
	}

	/* Load screen */
	screen_load();
}



/*
 * Ask for a "user pref line" and process it
 */
void do_cmd_pref(void)
{
	char tmp[80];

	/* Default */
	strcpy(tmp, "");

	/* Ask for a "user pref command" */
	if (!get_string("Pref: ", tmp, sizeof(tmp))) return;

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
static void do_cmd_pref_file_hack(int row)
{
	char ftmp[80];

	/* Prompt */
	prt("Command: Load a user pref file", row, 0);

	/* Prompt */
	prt("File: ", row + 2, 0);

	/* Default filename */
	strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

	/* Ask for a file (or cancel) */
	if (!askfor_aux(ftmp, sizeof(ftmp))) return;

	/* Process the given filename */
	if (process_pref_file(ftmp))
	{
		/* Mention failure */
		msg_format("Failed to load '%s'!", ftmp);
	}
	else
	{
		/* Mention success */
		msg_format("Loaded '%s'.", ftmp);
	}
}



/*
 * Interact with some options
 */
static void do_cmd_options_aux(int page, cptr info, bool *modified)
{
	char ch;

	int i, k = 0, n = 0;

	int opt[OPT_PAGE_PER];

	char buf[80];

	int dir;

	/* Scan the options */
	for (i = 0; i < OPT_PAGE_PER; i++)
	{
		/* Collect options on this "page" */
		if (option_page[page][i] != OPT_NONE)
		{
			opt[n++] = option_page[page][i];
		}
	}

	/* Clear screen */
	Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
		strnfmt(buf, sizeof(buf), "%s (RET to advance, y/n to set, ESC to accept, ? for help) ", info);
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Display the option text */
			strnfmt(buf, sizeof(buf), "%-48s: %s  (%s)",
			        options[opt[i]].desc,
			        op_ptr->opt[opt[i]] ? "yes" : "no ",
			        options[opt[i]].text);
			c_prt(a, buf, i + 2, 0);
		}

		/* Hilite current option */
		move_cursor(k + 2, 50);

		/* Get a key */
		ch = inkey();

		/*
		 * HACK - Try to translate the key into a direction
		 * to allow using the roguelike keys for navigation.
		 */
		dir = target_dir(ch);
		if ((dir == 2) || (dir == 4) || (dir == 6) || (dir == 8))
			ch = I2D(dir);

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				/* Hack -- Notice use of any "cheat" options */
				for (i = OPT_CHEAT; i < OPT_ADULT; i++)
				{
					if (op_ptr->opt[i])
					{
						/* Set score option */
						op_ptr->opt[OPT_SCORE + (i - OPT_CHEAT)] = TRUE;
					}
				}

				return;
			}

			case '-':
			case '8':
			{
				k = (n + k - 1) % n;
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			{
				k = (k + 1) % n;
				break;
			}

			case 't':
			case '5':
			{
				op_ptr->opt[opt[k]] = !op_ptr->opt[opt[k]];

				*modified = TRUE;
				break;
			}

			case 'y':
			case '6':
			{
				if (op_ptr->opt[opt[k]] == FALSE) *modified = TRUE;

				op_ptr->opt[opt[k]] = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case '4':
			{
				if (op_ptr->opt[opt[k]] == TRUE) *modified = TRUE;

				op_ptr->opt[opt[k]] = FALSE;
				k = (k + 1) % n;
				break;
			}

			case '?':
			{
				strnfmt(buf, sizeof(buf), "options.txt#%s", options[opt[k]].text);
				show_file(buf, NULL, 0, 0);
				Term_clear();
				break;
			}

			default:
			{
				bell("Illegal command for normal options!");
				break;
			}
		}
	}
}


/*
 * Modify the "window" options
 */
static void do_cmd_options_win(bool *modified)
{
	int i, j, d;

	int y = 0;
	int x = 0;

	char ch;

	u32b old_flag[ANGBAND_TERM_MAX];


	/* Memorize old flags */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		old_flag[j] = op_ptr->window_flag[j];
	}


	/* Clear screen */
	Term_clear();

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
		for (i = 0; i < MAX_DISPLAY_OPTIONS; i++)
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
				byte a = TERM_WHITE;

				char c = '.';

				/* Use color */
				if ((i == y) && (j == x)) a = TERM_L_BLUE;

				/* Active flag */
				if (op_ptr->window_flag[j] & (1L << i)) c = 'X';

				/* Flag value */
				Term_putch(35 + j * 5, i + 5, a, c);
			}
		}

		/* Place Cursor */
		Term_gotoxy(35 + x * 5, y + 5);

		/* Get key */
		ch = inkey();

		/* Allow escape */
		if ((ch == ESCAPE) || (ch == 'q')) break;

		/* Toggle */
		if ((ch == '5') || (ch == 't'))
		{
			/* Hack -- ignore the main window */
			if (x == 0)
			{
				bell("Cannot set main window flags!");
			}

			/* Toggle flag (off) */
			else if (op_ptr->window_flag[x] & (1L << y))
			{
				op_ptr->window_flag[x] &= ~(1L << y);
			}

			/* Toggle flag (on) */
			else
			{
				op_ptr->window_flag[x] |= (1L << y);
			}

			/* Continue */
			continue;
		}

		/* Extract direction */
		d = target_dir(ch);

		/* Move */
		if (d != 0)
		{
			x = (x + ddx[d] + ANGBAND_TERM_MAX) % ANGBAND_TERM_MAX;
			y = (y + ddy[d] + MAX_DISPLAY_OPTIONS) % MAX_DISPLAY_OPTIONS;
		}

		/* Oops */
		else
		{
			bell("Illegal command for window options!");
		}
	}

	/* Notice changes */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* Dead window */
		if (!angband_term[j]) continue;

		/* Ignore non-changes */
		if (op_ptr->window_flag[j] == old_flag[j]) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Erase */
		Term_clear();

		/* Refresh */
		(void)Term_fresh();

		/* Restore */
		Term_activate(old);

		/* Note change */
		*modified = TRUE;
	}
}


/*
 * Write all current options to the given preference file in the
 * lib/user directory. Modified from KAmband 1.8.
 */
static errr option_dump(cptr fname)
{
	static cptr mark = "Options Dump";

	int i, j;

	FILE *fff;

	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Remove old options */
	remove_old_dump(buf, mark);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);

	/* Output header */
	pref_header(fff, mark);

	/* Skip some lines */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic option dump\n\n");

	/* Dump options (skip cheat, adult, score) */
	for (i = 0; i < OPT_CHEAT; i++)
	{
		/* Require a real option */
		if (!options[i].text) continue;

		/* Comment */
		fprintf(fff, "# Option '%s'\n", options[i].desc);

		/* Dump the option */
		if (op_ptr->opt[i])
		{
			fprintf(fff, "Y:%s\n", options[i].text);
		}
		else
		{
			fprintf(fff, "X:%s\n", options[i].text);
		}

		/* Skip a line */
		fprintf(fff, "\n");
	}

	/* Dump window flags */
	for (i = 1; i < ANGBAND_TERM_MAX; i++)
	{
		/* Require a real window */
		if (!angband_term[i]) continue;

		/* Check each flag */
		for (j = 0; j < 32; j++)
		{
			/* Require a real flag */
			if (!window_flag_desc[j]) continue;

			/* Comment */
			fprintf(fff, "# Window '%s', Flag '%s'\n",
			        angband_term_name[i], window_flag_desc[j]);

			/* Dump the flag */
			if (op_ptr->window_flag[i] & (1L << j))
			{
				fprintf(fff, "W:%d:%d:1\n", i, j);
			}
			else
			{
				fprintf(fff, "W:%d:%d:0\n", i, j);
			}

			/* Skip a line */
			fprintf(fff, "\n");
		}
	}

	/* Output footer */
	pref_footer(fff, mark);

	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}


/*
 * Set or unset various options.
 *
 * After using this command, a complete redraw should be performed,
 * in case any visual options have been changed.
 */
void do_cmd_options(void)
{
	char ch;

	/* No options have been modified yet */
	bool modified = FALSE;

	/* Save screen */
	screen_save();

	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Why are we here */
		prt(format("%s options", VERSION_NAME), 2, 0);

		/* Give some choices */
		prt("(1) User Interface Options", 4, 5);
		prt("(2) Disturbance Options", 5, 5);
		prt("(3) Game-Play Options", 6, 5);
		prt("(4) Efficiency Options", 7, 5);
		prt("(5) Display Options", 8, 5);
		prt("(6) Birth Options", 9, 5);
		prt("(7) Cheat Options", 10, 5);

		/* Window flags */
		prt("(W) Window flags", 12, 5);

		/* Squelch menus */
		prt("(S) Item Squelch and Autoinscribe Menus", 13, 5);

		/* Load and Append */
		prt("(L) Load a user pref file", 14, 5);

		if (!modified)	prt("(A) Append options to a file", 15, 5);
		else c_prt(TERM_YELLOW ,"(A) Append option modifications to a file", 15, 5);

		/* Special choices */
		prt("(D) Base Delay Factor", 17, 5);
		prt("(H) Hitpoint Warning", 18, 5);
		prt("(P) Panel change offset", 19, 5);

		/* Prompt */
		prt("Command: ", 21, 0);

		/* Get command */
		ch = inkey();

		/* Exit */
		if (ch == ESCAPE) break;

		/* General Options */
		else if (ch == '1')
		{
			do_cmd_options_aux(0, "User Interface Options", &modified);
		}

		/* Disturbance Options */
		else if (ch == '2')
		{
			do_cmd_options_aux(1, "Disturbance Options", &modified);
		}

		/* Inventory Options */
		else if (ch == '3')
		{
			do_cmd_options_aux(2, "Game-Play Options", &modified);
		}

		/* Efficiency Options */
		else if (ch == '4')
		{
			do_cmd_options_aux(3, "Efficiency Options", &modified);
		}

		/* Display Options */
		else if (ch == '5')
		{
			do_cmd_options_aux(4, "Display Options", &modified);
		}

		/* Birth Options */
		else if (ch == '6')
		{
			do_cmd_options_aux(5, "Birth Options", &modified);
		}

		/* Cheating Options */
		else if (ch == '7')
		{
			do_cmd_options_aux(6, "Cheat Options", &modified);
		}

		/* Window flags */
		else if ((ch == 'W') || (ch == 'w'))
		{
			do_cmd_options_win(&modified);
		}

		/* Squelching and autoinscription menus */
		else if ((ch == 'S') || (ch == 's'))
		{
			do_cmd_squelch_autoinsc();
		}

		/* Load a user pref file */
		else if ((ch == 'L') || (ch == 'l'))
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(21);

			/* XXX XXX -- Assume no modifications remain */
			modified = FALSE;
		}

		/* Append options to a file */
		else if ((ch == 'A') || (ch == 'a'))
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Append options to a file", 21, 0);

			/* Prompt */
			prt("File: ", 22, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Dump the options */
			if (option_dump(ftmp))
			{
				/* Failure */
				msg_print("Failed!");
			}
			else
			{
				/* Success */
				msg_print("Done.");
			}

			/* All modified options have been saved */
			modified = FALSE;
		}

		/* Hack -- Base Delay Factor */
		else if ((ch == 'D') || (ch == 'd'))
		{
			/* Remember old value of delay factor */
			int old_delay_factor = op_ptr->delay_factor;

			/* Prompt */
			prt("Command: Base Delay Factor", 21, 0);

			/* Get a new value */
			while (1)
			{
				char cx;
				int msec = op_ptr->delay_factor * op_ptr->delay_factor;
				prt(format("Current base delay factor: %d (%d msec)",
				           op_ptr->delay_factor, msec), 23, 0);
				prt("New base delay factor (0-9 or ESC to accept): ", 22, 0);

				cx = inkey();
				if (cx == ESCAPE) break;
				if (isdigit((unsigned char)cx)) op_ptr->delay_factor = D2I(cx);
				else bell("Illegal delay factor!");
			}

			/* Note that a change was made */
			if (op_ptr->delay_factor != old_delay_factor) modified = TRUE;
		}

		/* Hack -- hitpoint warning factor */
		else if ((ch == 'H') || (ch == 'h'))
		{
			/* Prompt */
			prt("Command: Hitpoint Warning", 21, 0);

			/* Get a new value */
			while (1)
			{
				char cx;
				prt(format("Current hitpoint warning: %2d%%",
				           op_ptr->hitpoint_warn * 10), 23, 0);
				prt("New hitpoint warning (0-9 or ESC to accept): ", 22, 0);

				cx = inkey();
				if (cx == ESCAPE) break;
				if (isdigit((unsigned char)cx)) op_ptr->hitpoint_warn = D2I(cx);
				else bell("Illegal hitpoint warning!");
			}
		}

		else if ((ch == 'P') || (ch == 'p'))
		{
			/* Prompt */
			prt("Command: Panel change offset", 21, 0);

			/* Get a new value */
			while (1)
			{
				char buf[80];
				char cx;

				/* Show current settings */
				prt(format("Current offsets: top/bottom = %d, sides = %d",
				           (int)panel_change_offset_y, (int)panel_change_offset_x), 23, 0);
				/* Show options */
				prt("New offsets (y for top/bottom, x for sides or ESC to accept): ", 22, 0);

				/* Get a key */
				cx = inkey();
				/* Process operations */
				if (cx == ESCAPE) break;
				else if ((cx == 'Y') || (cx == 'y'))
				{
					/* Get a default value */
					strnfmt(buf, sizeof(buf), "%d", panel_change_offset_y);

					/* Show prompt/set cursor location */
					prt("New offset for top/bottom: ", 22, 0);

					/* Get the new value */
					if (askfor_aux(buf, sizeof(buf)))
					{
						/* Conver to number */
						int new_value = atoi(buf);

						/* Check bounds */
						if (new_value < MIN_PANEL_CHANGE_OFFSET_Y) bell("Illegal offset!");

						/* Assign */
						else panel_change_offset_y = new_value;
					}
				}
				else if ((cx == 'X') || (cx == 'x'))
				{
					/* Get a default value */
					strnfmt(buf, sizeof(buf), "%d", panel_change_offset_x);

					/* Show prompt/set cursor location */
					prt("New offset for sides: ", 22, 0);

					/* Get the new value */
					if (askfor_aux(buf, sizeof(buf)))
					{
						/* Conver to number */
						int new_value = atoi(buf);

						/* Check bounds */
						if (new_value < MIN_PANEL_CHANGE_OFFSET_X) bell("Illegal offset!");

						/* Assign */
						else panel_change_offset_x = new_value;
					}
				}
			}
		}

		/* Unknown option */
		else
		{
			/* Oops */
			bell("Illegal command for options!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Load screen */
	screen_load();

}





/*
 * Hack -- append all current macros to the given file
 */
static errr macro_dump(cptr fname)
{
	static cptr mark = "Macro Dump";

	int i;

	FILE *fff;

	char buf[1024];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Remove old macros */
	remove_old_dump(buf, mark);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);

	/* Output header */
	pref_header(fff, mark);

	/* Skip some lines */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic macro dump\n\n");

	/* Dump them */
	for (i = 0; i < macro__num; i++)
	{
		/* Start the macro */
		fprintf(fff, "# Macro '%d'\n\n", i);

		/* Extract the macro action */
		ascii_to_text(buf, sizeof(buf), macro__act[i]);

		/* Dump the macro action */
		fprintf(fff, "A:%s\n", buf);

		/* Extract the macro pattern */
		ascii_to_text(buf, sizeof(buf), macro__pat[i]);

		/* Dump the macro pattern */
		fprintf(fff, "P:%s\n", buf);

		/* End the macro */
		fprintf(fff, "\n\n");
	}

	/* Start dumping */
	fprintf(fff, "\n\n\n\n");

	/* Output footer */
	pref_footer(fff, mark);

	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
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

	char tmp[1024];

	/* Flush */
	flush();

	/* Do not process macros */
	inkey_base = TRUE;

	/* First key */
	ch = inkey();

	/* Read the pattern */
	while (ch != '\0')
	{
		/* Save the key */
		buf[n++] = ch;

		/* Do not process macros */
		inkey_base = TRUE;

		/* Do not wait for keys */
		inkey_scan = TRUE;

		/* Attempt to read a key */
		ch = inkey();
	}

	/* Terminate */
	buf[n] = '\0';

	/* Flush */
	flush();

	/* Convert the trigger */
	ascii_to_text(tmp, sizeof(tmp), buf);

	/* Hack -- display the trigger */
	Term_addstr(-1, TERM_WHITE, tmp);
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
 * Hack -- Append all keymaps to the given file.
 *
 * Hack -- We only append the keymaps for the "active" mode.
 */
static errr keymap_dump(cptr fname)
{
	static cptr mark = "Keymap Dump";

	int i;

	FILE *fff;

	char buf[1024];

	int mode;

	/* Roguelike */
	if (rogue_like_commands)
	{
		mode = KEYMAP_MODE_ROGUE;
	}

	/* Original */
	else
	{
		mode = KEYMAP_MODE_ORIG;
	}


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Remove old keymaps */
	remove_old_dump(buf, mark);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);

	/* Output header */
	pref_header(fff, mark);

	/* Skip some lines */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic keymap dump\n\n");

	/* Dump them */
	for (i = 0; i < (int)N_ELEMENTS(keymap_act[mode]); i++)
	{
		char key[2] = "?";

		cptr act;

		/* Loop up the keymap */
		act = keymap_act[mode][i];

		/* Skip empty keymaps */
		if (!act) continue;

		/* Encode the action */
		ascii_to_text(buf, sizeof(buf), act);

		/* Dump the keymap action */
		fprintf(fff, "A:%s\n", buf);

		/* Convert the key into a string */
		key[0] = i;

		/* Encode the key */
		ascii_to_text(buf, sizeof(buf), key);

		/* Dump the keymap pattern */
		fprintf(fff, "C:%d:%s\n", mode, buf);

		/* Skip a line */
		fprintf(fff, "\n");
	}

	/* Skip some lines */
	fprintf(fff, "\n\n\n");

	/* Output footer */
	pref_footer(fff, mark);

	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}





/*
 * Interact with "macros"
 *
 * Could use some helpful instructions on this page.  XXX XXX XXX
 */
void do_cmd_macros(void)
{
	char ch;

	char tmp[1024];

	char pat[1024];

	int mode;

	/* No options have been modified yet */
	bool modified_option = FALSE;

	/* No keymaps have been modified yet */
	bool modified_keymap = FALSE;

	/* Roguelike */
	if (rogue_like_commands)
	{
		mode = KEYMAP_MODE_ROGUE;
	}

	/* Original */
	else
	{
		mode = KEYMAP_MODE_ORIG;
	}

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Save screen */
	screen_save();

	/* Process requests until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Describe */
		prt("Interact with Macros", 2, 0);

		/* Describe that action */
		prt("Current action (if any) shown below:", 20, 0);

		/* Analyze the current action */
		ascii_to_text(tmp, sizeof(tmp), macro_buffer);

		/* Display the current action */
		prt(tmp, 22, 0);


		/* Selections */
		prt("(1) Load a user pref file", 4, 5);

		if (!modified_option)	prt("(2) Append macros to a file", 5, 5);
		else c_prt(TERM_YELLOW , "(2) Append macros to a file", 5, 5);

		prt("(3) Query a macro", 6, 5);
		prt("(4) Create a macro", 7, 5);
		prt("(5) Remove a macro", 8, 5);

		if (!modified_keymap)	prt("(6) Append keymaps to a file", 9, 5);
		else c_prt(TERM_YELLOW , "(6) Append keymaps to a file", 9, 5);

		prt("(7) Query a keymap", 10, 5);
		prt("(8) Create a keymap", 11, 5);
		prt("(9) Remove a keymap", 12, 5);
		prt("(0) Enter a new action", 13, 5);


		/* Prompt */
		prt("Command: ", 16, 0);

		/* Get a command */
		ch = inkey();

		/* Leave */
		if (ch == ESCAPE) break;

		/* Load a user pref file */
		if (ch == '1')
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(16);

			/*No current modifications*/
			modified_option = FALSE;
		}

		/* Save macros */
		else if (ch == '2')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Append macros to a file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Dump the macros */
			(void)macro_dump(ftmp);

			/*No current modifications*/
			modified_option = FALSE;

			/* Prompt */
			msg_print("Appended macros.");
		}

		/* Query a macro */
		else if (ch == '3')
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
				msg_print("Found a macro.");
			}
		}

		/* Create a macro */
		else if (ch == '4')
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
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);

				/* Link the macro */
				macro_add(pat, macro_buffer);

				/* Prompt */
				msg_print("Added a macro.");

				/*We have a modification*/
				modified_option = TRUE;
			}
		}

		/* Remove a macro */
		else if (ch == '5')
		{
			/* Prompt */
			prt("Command: Remove a macro", 16, 0);

			/* Prompt */
			prt("Trigger: ", 18, 0);

			/* Get a macro trigger */
			do_cmd_macro_aux(pat);

			/* Link the macro */
			macro_add(pat, pat);

			/*We have a modification*/
			modified_option = TRUE;

			/* Prompt */
			msg_print("Removed a macro.");
		}

		/* Save keymaps */
		else if (ch == '6')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Append keymaps to a file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Dump the macros */
			(void)keymap_dump(ftmp);

			/*No current keymaps*/
			modified_keymap = FALSE;

			/* Prompt */
			msg_print("Appended keymaps.");
		}

		/* Query a keymap */
		else if (ch == '7')
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
				msg_print("Found a keymap.");
			}
		}

		/* Create a keymap */
		else if (ch == '8')
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
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);

				/* Free old keymap */
				string_free(keymap_act[mode][(byte)(pat[0])]);

				/* Make new keymap */
				keymap_act[mode][(byte)(pat[0])] = string_make(macro_buffer);

				/*There is a current keymaps*/
				modified_keymap = TRUE;

				/* Prompt */
				msg_print("Added a keymap.");
			}
		}

		/* Remove a keymap */
		else if (ch == '9')
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

			/*There is a current keymaps*/
			modified_keymap = TRUE;

			/* Prompt */
			msg_print("Removed a keymap.");
		}

		/* Enter a new action */
		else if (ch == '0')
		{
			/* Prompt */
			prt("Command: Enter a new action", 16, 0);

			/* Go to the correct location */
			Term_gotoxy(0, 22);

			/* Analyze the current action */
			ascii_to_text(tmp, sizeof(tmp), macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
			{
				/* Extract an action */
				text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);

				/*There is a current keymaps*/
				modified_keymap = TRUE;
			}
		}



		/* Oops */
		else
		{
			/* Oops */
			bell("Illegal command for macros!");
		}

		/* Flush messages */
		message_flush();
	}


	/* Load screen */
	screen_load();
}


/*
 * Interact with "visuals"
 */
void do_cmd_visuals(void)
{
	int ch;
	int cx;

	int i;

	FILE *fff;

	char buf[1024];


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Save screen */
	screen_save();


	/* Interact until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Interact with Visuals", 2, 0);

		/* Give some choices */
		prt("(1) Load a user pref file", 4, 5);

		prt("(2) Dump monster attr/chars", 5, 5);
		prt("(3) Dump object attr/chars", 6, 5);
		prt("(4) Dump feature attr/chars", 7, 5);
		prt("(5) Dump flavor attr/chars", 8, 5);
		prt("(6) Change monster attr/chars", 9, 5);
		prt("(7) Change object attr/chars", 10, 5);
		prt("(8) Change feature attr/chars", 11, 5);
		prt("(9) Change flavor attr/chars", 12, 5);

		prt("(0) Reset visuals", 13, 5);

		/* Prompt */
		prt("Command: ", 15, 0);

		/* Prompt */
		ch = inkey();

		/* Done */
		if (ch == ESCAPE) break;

		/* Load a user pref file */
		if (ch == '1')
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(15);
		}



		/* Dump monster attr/chars */
		else if (ch == '2')
		{
			static cptr mark = "Monster attr/chars";
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump monster attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* Remove old attr/chars */
			remove_old_dump(buf, mark);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;

			/* Output header */
			pref_header(fff, mark);

			/* Skip some lines */
			fprintf(fff, "\n\n");

			/* Start dumping */
			fprintf(fff, "# Monster attr/char definitions\n\n");

			/* Dump monsters */
			for (i = 0; i < z_info->r_max; i++)
			{
				monster_race *r_ptr = &r_info[i];

				/* Skip non-entries */
				if (!r_ptr->name) continue;

				/* Dump a comment */
				fprintf(fff, "# %s\n", (r_name + r_ptr->name));

				/* Dump the monster attr/char info */
 				fprintf(fff, "R:%d:%d:%d\n\n", i,
  				        (byte)(r_ptr->x_attr), (byte)(r_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped monster attr/chars.");
		}

		/* Dump object attr/chars */
		else if (ch == '3')
		{
			static cptr mark = "Object attr/chars";
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump object attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* Remove old attr/chars */
			remove_old_dump(buf, mark);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;

			/* Output header */
			pref_header(fff, mark);

			/* Skip some lines */
			fprintf(fff, "\n\n");

			/* Start dumping */
			fprintf(fff, "# Object attr/char definitions\n\n");

			/* Dump objects */
			for (i = 0; i < z_info->k_max; i++)
			{
				object_kind *k_ptr = &k_info[i];

				/* Skip non-entries */
				if (!k_ptr->name) continue;

				/* Dump a comment */
				fprintf(fff, "# %s\n", (k_name + k_ptr->name));

				/* Dump the object attr/char info */
 				fprintf(fff, "K:%d:0x%02X:0x%02X\n\n", i,
  				        (byte)(k_ptr->x_attr), (byte)(k_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped object attr/chars.");
		}

		/* Dump feature attr/chars */
		else if (ch == '4')
		{
			static cptr mark = "Feature attr/chars";
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump feature attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* Remove old attr/chars */
			remove_old_dump(buf, mark);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;

			/* Output header */
			pref_header(fff, mark);

			/* Skip some lines */
			fprintf(fff, "\n\n");

			/* Start dumping */
			fprintf(fff, "# Feature attr/char definitions\n\n");

			/* Dump features */
			for (i = 0; i < z_info->f_max; i++)
			{
				feature_type *f_ptr = &f_info[i];
				char name[80];

				/* Skip non-entries */
				if (!f_ptr->name) continue;

				/* Get feature name */
				feature_desc(name, sizeof(name),
					f_ptr - f_info, FALSE, FALSE);

				/* Dump a comment */
				fprintf(fff, "# %s\n", name);

				/* Dump the feature attr/char info */
				fprintf(fff, "F:%d:%d:%d:%s\n\n", i,
					(byte)(f_ptr->x_attr), (byte)(f_ptr->x_char),
					(f_ptr->f_flags2 & (FF2_ATTR_LITE)) ? "YES" : "NO");
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped feature attr/chars.");
		}

		/* Dump flavor attr/chars */
		else if (ch == '5')
		{
			static cptr mark = "Flavor attr/chars";
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump flavor attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* Remove old attr/chars */
			remove_old_dump(buf, mark);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;

			/* Output header */
			pref_header(fff, mark);

			/* Skip some lines */
			fprintf(fff, "\n\n");

			/* Start dumping */
			fprintf(fff, "# Flavor attr/char definitions\n\n");

			/* Dump flavors */
			for (i = 0; i < z_info->flavor_max; i++)
			{
				flavor_type *flavor_ptr = &flavor_info[i];

				/* Dump a comment */
				fprintf(fff, "# %s\n", (flavor_text + flavor_ptr->text));

				/* Dump the flavor attr/char info */
				fprintf(fff, "L:%d:%d:%d\n\n", i,
				        (byte)(flavor_ptr->x_attr), (byte)(flavor_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped flavor attr/chars.");
		}

		/* Modify monster attr/chars */
		else if (ch == '6')
		{
			static int r = 0;

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
				            format("Monster = %d, Name = %-40.40s",
				                   r, (r_name + r_ptr->name)));

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
						Term_putch(44, 20, 255, -1);
					else
						Term_putch(44, 20, 0, ' ');
				}

				/* Prompt */
				Term_putstr(0, 22, -1, TERM_WHITE,
				            "Command (n/N/a/A/c/C): ");

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

		/* Modify object attr/chars */
		else if (ch == '7')
		{
			static int k = 0;

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
				Term_putstr(5, 17, -1, TERM_WHITE,
				            format("Object = %d, Name = %-40.40s",
				                   k, (k_name + k_ptr->name)));

				/* Label the Default values */
				Term_putstr(10, 19, -1, TERM_WHITE,
				            format("Default attr/char = %3d / %3d", da, dc));
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
				            format("Current attr/char = %3d / %3d", ca, cc));
				Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 20, ca, cc);

				if (use_bigtile)
				{
					if (ca & 0x80)
						Term_putch(44, 20, 255, -1);
					else
						Term_putch(44, 20, 0, ' ');
				}

				/* Prompt */
				Term_putstr(0, 22, -1, TERM_WHITE,
				            "Command (n/N/a/A/c/C): ");

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

		/* Modify feature attr/chars */
		else if (ch == '8')
		{
			static int f = 0;

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
						Term_putch(44, 19, 255, -1);
					else
						Term_putch(44, 19, 0, ' ');
				}

				/* Label the Current values */
				Term_putstr(10, 20, -1, TERM_WHITE,
				            format("Current attr/char = %3d / %3d", ca, cc));
				Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 20, ca, cc);

				if (use_bigtile)
				{
					if (ca & 0x80)
						Term_putch(44, 20, 255, -1);
					else
						Term_putch(44, 20, 0, ' ');
				}

				/* Prompt */
				Term_putstr(0, 22, -1, TERM_WHITE,
				            "Command (n/N/a/A/c/C): ");

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

		/* Modify flavor attr/chars */
		else if (ch == '9')
		{
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
						Term_putch(44, 19, 255, -1);
					else
						Term_putch(44, 19, 0, ' ');
				}

				/* Label the Current values */
				Term_putstr(10, 20, -1, TERM_WHITE,
				            format("Current attr/char = %3d / %3d", ca, cc));
				Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 20, ca, cc);

				if (use_bigtile)
				{
					if (ca & 0x80)
						Term_putch(44, 20, 255, -1);
					else
						Term_putch(44, 20, 0, ' ');
				}

				/* Prompt */
				Term_putstr(0, 22, -1, TERM_WHITE,
				            "Command (n/N/a/A/c/C): ");

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



		/* Reset visuals */
		else if (ch == '0')
		{
			/* Reset */
			reset_visuals(TRUE);

			/* Message */
			msg_print("Visual attr/char tables reset.");
		}

		/* Unknown option */
		else
		{
			bell("Illegal command for visuals!");
		}

		/* Flush messages */
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

/* We only can edit a portion of the color table */
#define MAX_COLORS 128

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


/*
 * Interact with "colors"
 */
void do_cmd_colors(void)
{
	int ch;

	int i;

	FILE *fff;

	char buf[1024];

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

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
			if (!askfor_aux(ftmp, sizeof(ftmp))) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* Remove old colors */
			remove_old_dump(buf, mark);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;

			/* Output header */
			pref_header(fff, mark);

			/* Skip some lines */
			fprintf(fff, "\n\n");

			/* Start dumping */
			fprintf(fff, "# Color redefinitions\n\n");

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
				fprintf(fff, "# Color '%s'\n", name);

				/* Dump the monster attr/char info */
				fprintf(fff, "V:%d:0x%02X:0x%02X:0x%02X:0x%02X\n\n",
				        i, kv, rv, gv, bv);
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

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


 /*
  * Take notes.  There are two ways this can happen, either in the message recall or
  * a file.  The command can also be passed a string, which will automatically be
  * written. -CK-
  */
 void do_cmd_note(char *note, int what_depth)
 {
 	char buf[120];

 	/* Default */
 	strcpy(buf, "");

 	/* If a note is passed, use that, otherwise accept user input. */
 	if (streq(note, ""))
	{

 	  	if (!get_string("Note: ", buf, 70)) return;

 	}

	else my_strcpy(buf, note, sizeof(buf));

 	/* Ignore empty notes */
 	if (!buf[0] || (buf[0] == ' ')) return;

	/* If the note taking option is on, write it to the file, otherwise write to
 	 * the message recall.
 	 */
 	if (adult_take_notes)
	{
		int length, length_info;
        char info_note[40];
 	  	char depths[10];

		/*Artifacts use depth artifact created.  All others
	 	 *use player depth.
	 	 */

		/*get depth for recording\
		 *mark if item is a quest reward or a chest item
		 */
 	    if (what_depth == 0)
 	    {
 			strcpy(depths, " Town");
 	    }
 	    else if (what_depth == CHEST_LEVEL)
 	    {
 			strnfmt(depths, sizeof(depths), "Chest");
 	    }
		else if (what_depth == QUEST_LEVEL)
 	    {
 			strnfmt(depths, sizeof(depths), "Quest");
 	    }
		else if (depth_in_feet)
 	    {
 			strnfmt(depths, sizeof(depths), " %4d", what_depth * 50);
 	    }
		else
 	    {
 		strnfmt(depths, sizeof(depths), " %4d", what_depth);
		}

 	  	/* Make preliminary part of note */
     	strnfmt(info_note, sizeof(info_note), "|%9lu| %s | %2d  | ", turn, depths, p_ptr->lev);

		/*write the info note*/
		fprintf(notes_file, info_note);

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
				if (startpoint) fprintf(notes_file, "|  continued...   |     |  ");

				/* Write that line to file */
				for (n = startpoint; n <= endpoint; n++)
				{
					char ch;

					/* Ensure the character is printable */
					ch = (isprint(buf[n]) ? buf[n] : ' ');

					/* Write out the character */
					fprintf(notes_file, "%c", ch);

				}

				/*break the line*/
				fprintf(notes_file, "\n");

				/*prepare for the next line*/
				startpoint = endpoint + 1;
			}

		}

 	 	/* Add note to buffer */
 	 	else
		{
			fprintf(notes_file, "%s", buf);

			/*break the line*/
			fprintf(notes_file, "\n");
		}


 	}

 	else msg_format("Note: %s", buf);

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

	FILE *fp;

	char buf[1024];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "dump.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

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
		if (my_fgets(fp, buf, sizeof(buf))) okay = FALSE;


		/* Show each row */
		for (x = 0; x < 79; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Get the blank line */
	if (my_fgets(fp, buf, sizeof(buf))) okay = FALSE;


	/* Dump the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (my_fgets(fp, buf, sizeof(buf))) okay = FALSE;

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
	my_fclose(fp);


	/* Message */
	msg_print("Screen dump loaded.");
	message_flush();


	/* Load screen */
	screen_load();
}


/*display the notes file*/
static void do_cmd_knowledge_notes(void)
{

	/*close the notes file for writing*/
	my_fclose(notes_file);

	show_file(notes_fname, "Notes", 0, 0);

	/*re-open for appending*/
	notes_file = my_fopen(notes_fname, "a");

}

/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
	char tmp_val[256];

	/* Ask for a file */
	strcpy(tmp_val, "dump.html");
	if (!get_string("File: ", tmp_val, sizeof(tmp_val))) return;

	html_screenshot(tmp_val);
	msg_print("Dump saved.");
}

/*
 * Description of each object group.
 */
static cptr object_group_text[] =
{
	"Mushrooms",
	"Potions",
	"Scrolls",
	"Rings",
	"Amulets",
	"Light Sources",
	"Wands",
	"Staves",
	"Rods",
	"Swords",
	"Hafted Weapons",
	"Polearms",
	"Diggers",
	"Bows",
	"Soft Armor",
	"Hard Armor",
	"Dragon Armor",
	"Dragon Shields",
	"Shields",
	"Cloaks",
	"Gloves",
	"Helms",
	"Crowns",
	"Boots",
	"Mage spell books",
	"Prayer books",
	"Druid spell books",
	"Shots",
	"Arrows",
	"Bolts",
	"Chests",
	NULL
};



/*
 * TVALs of items in each group
 */
static byte object_group_tval[] =
{
	TV_FOOD,
	TV_POTION,
	TV_SCROLL,
	TV_RING,
	TV_AMULET,
	TV_LITE,
	TV_WAND,
	TV_STAFF,
	TV_ROD,
	TV_SWORD,
	TV_HAFTED,
	TV_POLEARM,
	TV_DIGGING,
	TV_BOW,
	TV_SOFT_ARMOR,
	TV_HARD_ARMOR,
	TV_DRAG_ARMOR,
	TV_DRAG_SHIELD,
	TV_SHIELD,
	TV_CLOAK,
	TV_GLOVES,
	TV_HELM,
	TV_CROWN,
	TV_BOOTS,
	TV_MAGIC_BOOK,
	TV_PRAYER_BOOK,
	TV_DRUID_BOOK,
	TV_SHOT,
	TV_ARROW,
	TV_BOLT,
	TV_CHEST,
	0
};

/*
 * Build a list of objects indexes in the given group. Return the number
 * of objects in the group.
 */
static int collect_objects(int grp_cur, int object_idx[])
{
	int i, j, k, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval = object_group_tval[grp_cur];

	/* Check every object */
	for (i = 0; i < z_info->k_max; i++)
	{
		/* Access the object type */
		object_kind *k_ptr = &k_info[i];

		/*used to check for allocation*/
		k = 0;

		/* Skip empty objects */
		if (!k_ptr->name) continue;

		/* Skip items with no distribution (including special artifacts) */
		/* Scan allocation pairs */
		for (j = 0; j < 4; j++)
		{
			/*add the rarity, if there is one*/
			k += k_ptr->chance[j];
		}
		/*not in allocation table*/
		if (!(k))  continue;

		/* Require objects ever seen*/
		if (!(k_ptr->aware && k_ptr->everseen)) continue;

		/* Check for object in the group */
		if (k_ptr->tval == group_tval)
		{
			/* Add the race */
			object_idx[object_cnt++] = i;
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Return the number of races */
	return object_cnt;
}

/*
 * Check if an artifact is *identified*.  Assumes all lost or discarded
 * artifacts are *identified*.  Assumes it is a known artifact
 */
static bool is_artifact_fully_identified(byte art_num)
{
	int i, y;

	/* Process objects in the dungeon */
	for (i = 1; i < o_max; i++)
	{
		/*get the object*/
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Go to next one if this is not the artifact */
		if (o_ptr->art_num != art_num) continue;

		/* We have a match, return the status*/
		return (o_ptr->ident & (IDENT_MENTAL));
	}

	/*
	 * Scan the inventory for the artifact
	 * Notice we are doing the inventory and equipment in the same loop.
	 */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr;

		/* First, the item actually in the slot */
		o_ptr = &inventory[i];

		/*nothing there*/
		if (!(o_ptr->k_idx)) continue;

		/* Go to next one if this is not the artifact */
		if (o_ptr->art_num != art_num) continue;

		/* We have a match, return the status*/
		return (o_ptr->ident & (IDENT_MENTAL));
	}

	/* Check each store for the artifact*/
	for (y = 0; y < MAX_STORES; y++)
	{
		/* Get the store */
		store_type *st_ptr = &store[y];

		/*The guild doesn't carry stock*/
		if (y == STORE_GUILD) continue;

		/* Look for items in the home, if there is anything there */
		if (st_ptr->stock_num)
		{
			/*go through each item in the house*/
			for (i = 0; i < st_ptr->stock_num; i++)
			{
				object_type *o_ptr;

				/* Point to the item */
				o_ptr = &st_ptr->stock[i];

				/*nothing there*/
				if (!(o_ptr->k_idx)) continue;

				/* Go to next one if this is not the artifact */
				if (o_ptr->art_num != art_num) continue;

				/* We have a match, return the status*/
				return (o_ptr->ident & (IDENT_MENTAL));
			}
		}

	}

	/*
	 * Artifact isn't in the stores, on the ground, or in the inventory,
	 * so it must have been lost or discarded.  We then give the player
	 * full identification for their reference
     */
	return(TRUE);

}



/*
 * Build a list of artifact indexes in the given group. Return the number
 * of eligible artifacts in that group.
 */
static int collect_artifacts(int grp_cur, int object_idx[])
{
	int i, object_cnt = 0;
	bool *okay;

	store_type *st_ptr = &store[STORE_HOME];

	/* Get a list of x_char in this group */
	byte group_tval = object_group_tval[grp_cur];

	/*make a list of artifacts not found*/
	/* Allocate the "object_idx" array */
	C_MAKE(okay, z_info->art_max, bool);

	/* Default first,  */
	for (i = 0; i < z_info->art_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/*start with false*/
		okay[i] = FALSE;

		/* Skip "empty" artifacts */
		if (a_ptr->tval + a_ptr->sval == 0) continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->a_cur_num) continue;

		/*assume all created artifacts are good at this point*/
		okay[i] = TRUE;
	}

	/* Process objects in the dungeon */
	for (i = 1; i < o_max; i++)
	{
		/*get the object*/
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Ignore non-artifacts */
		if (!o_ptr->art_num) continue;

		/* Ignore known items */
		if (object_known_p(o_ptr)) continue;

		/* We found a created, unidentified artifact */
		okay[o_ptr->art_num] = FALSE;

	}

	/*
	 * Scan the inventory for unidentified artifacts
	 * Notice we are doing the inventory and equipment in the same loop.
	 */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		/* First, the item actually in the slot */
		object_type *o_ptr = &inventory[i];

		/*nothing there*/
		if (!(o_ptr->k_idx)) continue;

		/* Ignore non-artifacts */
		if (!o_ptr->art_num) continue;

		/* Ignore known items */
		if (object_known_p(o_ptr)) continue;

		/* We found a created, unidentified artifact */
		okay[o_ptr->art_num] = FALSE;
	}

	/* Look for items in the home, if there is anything there */
	if (st_ptr->stock_num)
	{
		/*go through each item in the house*/
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Point to the item */
			object_type *o_ptr = &st_ptr->stock[i];;

			/*nothing there*/
			if (!(o_ptr->k_idx)) continue;

			/* Ignore non-artifacts */
			if (!o_ptr->art_num) continue;

			/* Ignore known items */
			if (object_known_p(o_ptr)) continue;

			/* We found a created, unidentified artifact */
			okay[o_ptr->art_num] = FALSE;
		}

	}

	if (cheat_know)
	{
		for (i = 0; i < z_info->art_max; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			/* Skip "empty" artifacts */
			if (a_ptr->tval + a_ptr->sval == 0) continue;

			/*assume all created artifacts are good at this point*/
			okay[i] = TRUE;
		}
	}


	/* Finally, go through the list of artifacts and categorize the good ones */
	for (i = 0; i < z_info->art_max; i++)
	{
		/* Access the artifact */
		artifact_type *a_ptr = &a_info[i];

		/* Skip empty artifacts */
		if (a_ptr->tval + a_ptr->sval == 0) continue;

		/* Require artifacts ever seen*/
		if (okay[i] == FALSE) continue;

		/* Check for race in the group */
		if (a_ptr->tval == group_tval)
		{
			/* Add the race */
			object_idx[object_cnt++] = i;
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/*clear the array*/
	KILL(okay);

	/* Return the number of races */
	return object_cnt;

}


/*
 * Display the object groups.
 */
static void display_group_list(int col, int row, int wid, int per_page,
	int grp_idx[], cptr group_text[], int grp_cur, int grp_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && (grp_idx[i] >= 0); i++)
	{
		/* Get the group index */
		int grp = grp_idx[grp_top + i];

		/* Choose a color */
		byte attr = (grp_top + i == grp_cur) ? TERM_L_BLUE : TERM_WHITE;

		/* Erase the entire line */
		Term_erase(col, row + i, wid);

		/* Display the group label */
		c_put_str(attr, group_text[grp], row + i, col);
	}
}

/*
 * Move the cursor in a browser window
 */
static void browser_cursor(char ch, int *column, int *grp_cur, int grp_cnt,
						   int *list_cur, int list_cnt)
{
	int d;
	int col = *column;
	int grp = *grp_cur;
	int list = *list_cur;

	/* Extract direction */
	d = target_dir(ch);

	if (!d) return;

	/* Diagonals - hack */
	if ((ddx[d] > 0) && ddy[d])
	{
		/* Browse group list */
		if (!col)
		{
			int old_grp = grp;

			/* Move up or down */
			grp += ddy[d] * BROWSER_ROWS;

			/* Verify */
			if (grp >= grp_cnt)	grp = grp_cnt - 1;
			if (grp < 0) grp = 0;
			if (grp != old_grp)	list = 0;
		}

		/* Browse sub-list list */
		else
		{
			/* Move up or down */
			list += ddy[d] * BROWSER_ROWS;

			/* Verify */
			if (list >= list_cnt) list = list_cnt - 1;
			if (list < 0) list = 0;
		}

		(*grp_cur) = grp;
		(*list_cur) = list;

		return;
	}

	if (ddx[d])
	{
		col += ddx[d];
		if (col < 0) col = 0;
		if (col > 1) col = 1;

		(*column) = col;

		return;
	}

	/* Browse group list */
	if (!col)
	{
		int old_grp = grp;

		/* Move up or down */
		grp += ddy[d];

		/* Verify */
		if (grp >= grp_cnt)	grp = grp_cnt - 1;
		if (grp < 0) grp = 0;
		if (grp != old_grp)	list = 0;
	}

	/* Browse sub-list list */
	else
	{
		/* Move up or down */
		list += ddy[d];

		/* Verify */
		if (list >= list_cnt) list = list_cnt - 1;
		if (list < 0) list = 0;
	}

	(*grp_cur) = grp;
	(*list_cur) = list;
}

/*
 * Hack -- Create a "forged" artifact
 */
static bool prepare_fake_artifact(object_type *o_ptr, byte art_num)
{
	s16b i;

	artifact_type *a_ptr = &a_info[art_num];

	/* Ignore "empty" artifacts */
	if (a_ptr->tval + a_ptr->sval == 0) return FALSE;

	/* Get the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (FALSE);

	/* Create the artifact */
	object_prep(o_ptr, i);

	/* Save the name */
	o_ptr->art_num = art_num;

	/* Extract the fields */
	o_ptr->pval = a_ptr->pval;
	o_ptr->ac = a_ptr->ac;
	o_ptr->dd = a_ptr->dd;
	o_ptr->ds = a_ptr->ds;
	o_ptr->to_a = a_ptr->to_a;
	o_ptr->to_h = a_ptr->to_h;
	o_ptr->to_d = a_ptr->to_d;
	o_ptr->weight = a_ptr->weight;

	/*identify it*/
	object_known(o_ptr);

	/*make it a store item*/
	o_ptr->ident |= IDENT_STORE;

	/* Set the "known" flag, but only if the artifact is *identified* */
	if (is_artifact_fully_identified(art_num))
	{
		o_ptr->ident |= (IDENT_MENTAL);
	}

	/* Hack -- extract the "cursed" flag */
	if (a_ptr->a_flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

	/* Success */
	return (TRUE);
}


/*
 * Describe fake artifact
 */
void desc_art_fake(int a_idx)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Make fake artifact */
	prepare_fake_artifact(i_ptr, a_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	object_info_screen(i_ptr);
}

/*
 * Display the objects in a group.
 */
static void display_artifact_list(int col, int row, int per_page, int object_idx[],
	int object_cur, int object_top)
{
	int i;
	char o_name[80];
	object_type *i_ptr;
	object_type object_type_body;

	/* Display lines until done */
	for (i = 0; i < per_page && object_idx[i]; i++)
	{
		/* Get the object index */
		int a_idx = object_idx[object_top + i];

		/* Choose a color */
		byte attr = TERM_WHITE;
		byte cursor = TERM_L_BLUE;
		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make fake artifact */
		prepare_fake_artifact(i_ptr, a_idx);

		/* Get its name */
		object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 0);

		/* Display the name */
		c_prt(attr, o_name, row + i, col);


		if (cheat_know)
		{
			artifact_type *a_ptr = &a_info[a_idx];

			c_prt(attr, format ("%3d", a_idx), row + i, 68);
			c_prt(attr, format ("%3d", a_ptr->a_level), row + i, 72);
			c_prt(attr, format ("%3d", a_ptr->a_rarity), row + i, 76);

		}

	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}

/*
 * Display known artifacts
 */
static void do_cmd_knowledge_artifacts(void)
{
	int i, len, max;
	int grp_cur, grp_top;
	int artifact_old, artifact_cur, artifact_top;
	int grp_cnt, grp_idx[100];
	int artifact_cnt;
	int *artifact_idx;

	int column = 0;
	bool flag;
	bool redraw;

	/* Allocate the "artifact_idx" array */
	C_MAKE(artifact_idx, z_info->art_max, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; object_group_text[i] != NULL; i++)
	{
		/* Measure the label */
		len = strlen(object_group_text[i]);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if artifact are known */
		if (collect_artifacts(i, artifact_idx))
		{
			/* Build a list of groups with known artifacts */
			grp_idx[grp_cnt++] = i;
		}
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	grp_cur = grp_top = 0;
	artifact_cur = artifact_top = 0;
	artifact_old = -1;

	flag = FALSE;
	redraw = TRUE;

	while (!flag)
	{
		char ch;

		if (redraw)
		{
			clear_from(0);

			prt("Knowledge - artifacts", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);

			if (cheat_know)
			{
				prt("Idx", 4, 68);
				prt("Dep", 4, 72);
				prt("Rar", 4, 76);
			}

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < BROWSER_ROWS; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + BROWSER_ROWS) grp_top = grp_cur - BROWSER_ROWS + 1;

		/* Scroll artifact list */
		if (artifact_cur < artifact_top) artifact_top = artifact_cur;
		if (artifact_cur >= artifact_top + BROWSER_ROWS) artifact_top = artifact_cur - BROWSER_ROWS + 1;

		/* Display a list of object groups */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, object_group_text, grp_cur, grp_top);

		/* Get a list of objects in the current group */
		artifact_cnt = collect_artifacts(grp_idx[grp_cur], artifact_idx);

		/* Display a list of objects in the current group */
		display_artifact_list(max + 3, 6, BROWSER_ROWS, artifact_idx, artifact_cur, artifact_top);

		/* Prompt */
		prt("<dir>, 'r' to recall, ESC", TERM_LAST_ROW, 0);

		/* The "current" object changed */
		if (artifact_old != artifact_idx[artifact_cur])
		{
			/* Hack -- handle stuff */
			handle_stuff();

			/* Remember the "current" object */
			artifact_old = artifact_idx[artifact_cur];
		}

		if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (artifact_cur - artifact_top));
		}

		ch = inkey();

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'R':
			case 'r':
			{
				/* Recall on screen */
				desc_art_fake(artifact_idx[artifact_cur]);

				redraw = TRUE;
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &artifact_cur, artifact_cnt);
				break;
			}
		}
	}

	/* XXX XXX Free the "object_idx" array */
	KILL(artifact_idx);
}

/*
 * Description of each monster group.
 */
static cptr monster_group_text[] =
{
	"Uniques",						/*All uniques, all letters*/
	"Ants",  						/*'a'*/
	"Ainur/Maiar",					/*'A'*/
	"Bats",							/*'b'*/
	"Birds",						/*'B'*/
	"Centipedes",					/*'c'*/
	"Canines",						/*'C'*/
	"Dragons",						/*'d'*/
	"Ancient Dragons/Wyrms",		/*'D'*/
	"Floating Eyes",				/*'e'*/
	"Elementals",					/*'E'*/
	"Felines",						/*'f'*/
	"Dragon Flies",					/*'F'*/
	"Golems",						/*'g'*/
	"Ghosts",						/*'G'*/
	"Hobbits/Elves/Dwarves",		/*'h'*/
	"Hybrids",						/*'H'*/
	"Icky Things",					/*'i'*/
	"Insects",						/*'I'*/
	"Jellies",						/*'j'*/
	"Snakes",						/*'J'*/
	"Kobolds",						/*'k'*/
	"Killer Beetles",				/*'K'*/
	"Louses",						/*'l'*/
	"Lichs",						/*'L'*/
	"Molds",						/*'m'*/
	"Multi-Headed Reptiles",		/*'M'*/
	"Nagas",						/*'n'*/
	/*Unused*/						/*'N'*/
	"Orcs",							/*'o'*/
	"Ogres",						/*'O'*/
	"People/Humans",				/*'p'*/
	"Giant Humanoids",				/*'P'*/
	"Quadrupeds",					/*'q'*/
	"Quylthulgs",					/*'Q'*/
	"Rodents",						/*'r'*/
	"Reptiles/Amphibians",			/*'R'*/
	"Skeletons",					/*'s'*/
	"Spiders/Scorpions/Ticks",		/*'S'*/
	"Townpersons",					/*'t'*/
	"Trolls",						/*'T'*/
	"Minor Demons",					/*'u'*/
	"Major Demons",					/*'U'*/
	"Vortices",						/*'v'*/
	"Vampires",						/*'V'*/
	"Worms/Worm Masses",			/*'w'*/
	"Wight/Wraith/etc",				/*'W'*/
	/*Unused*/						/*'x'*/
	"Xorn/Xaren/etc",				/*'X'*/
	"Yeeks",						/*'y'*/
	"Yetis",						/*'Y'*/
	"Zombies/Mummies",				/*'z'*/
	"Zephyr Hounds",				/*'Z'*/
	"Mushroom Patches",				/*','*/
	"Mimics",						/*'$!?=._-*/
	NULL
};

/*
 * Symbols of monsters in each group. Note the "Uniques" group
 * is handled differently.
 */
static cptr monster_group_char[] =
{
	(char *) -1L,
	"a",
	"A",
	"b",
	"B",
	"c",
	"C",
	"d",
	"D",
	"e",
	"E",
	"f",
	"F",
	"g",
	"G",
	"h",
	"H",
	"i",
	"I",
	"j",
	"J",
	"k",
	"K",
	"l",
	"L",
	"m",
	"M",
	"n",
	/*"N",  Unused*/
	"o",
	"O",
	"p",
	"P",
	"q",
	"Q",
	"r",
	"R",
	"s",
	"S",
	"t",
	"T",
	"u",
	"U",
	"v",
	"V",
	"w",
	"W",
	/*"x", Unused*/
	"X",
	"y",
	"Y",
	"z",
	"Z",
	",",
	"$!?=._-~",  /*Mimics*/
	NULL
};

/*
 * Build a list of monster indexes in the given group. Return the number
 * of monsters in the group.
 */
static int collect_monsters(int grp_cur, monster_list_entry *mon_idx, int mode)
{
	int i, mon_count = 0;

	/* Get a list of x_char in this group */
	cptr group_char = monster_group_char[grp_cur];

	/* XXX Hack -- Check if this is the "Uniques" group */
	bool grp_unique = (monster_group_char[grp_cur] == (char *) -1L);

	/* Check every race */
	for (i = 0; i < z_info->r_max; i++)
	{
		/* Access the race */
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Is this a unique? */
		bool unique = (r_ptr->flags1 & (RF1_UNIQUE));

		/* Skip empty race */
		if (!r_ptr->name) continue;

		/* No Player Ghosts, unless active */
		if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && (r_ptr->cur_num == 0)) continue;

		if (grp_unique && !(unique)) continue;

		/* Require known monsters */
		if (!(mode & 0x02) && (!cheat_know) && (!(l_ptr->sights))) continue;

		/* Check for race in the group */
		if ((grp_unique) || (strchr(group_char, r_ptr->d_char)))
		{
			/* Add the race */
			mon_idx[mon_count++].r_idx = i;

			/* XXX Hack -- Just checking for non-empty group */
			if (mode & 0x01) break;
		}
	}

	/* Terminate the list */
	mon_idx[mon_count].r_idx = 0;

	/* Return the number of races */
	return (mon_count);
}

/*
 * Display the monsters in a group.
 */
static void display_monster_list(int col, int row, int per_page, monster_list_entry *mon_idx,
	int mon_cur, int mon_top, int grp_cur)
{
	int i;

	u32b known_uniques, dead_uniques, slay_count;

	/* Start with 0 kills*/
	known_uniques = dead_uniques = slay_count = 0;

	/* Count up monster kill counts */
	for (i = 1; i < z_info->r_max - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Require non-unique monsters */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			/*No active player ghosts*/
			if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && (r_ptr->cur_num == 0)) continue;

			/*Count if we have seen the unique*/
			if (l_ptr->sights)
			{
				known_uniques++;

				/*Count if the unique is dead*/
				if (r_ptr->max_num == 0)
				{
					dead_uniques++;
					slay_count++;
				}
			}

		}

		/* Collect "appropriate" monsters */
		else slay_count += l_ptr->pkills;
	}

	/* Display lines until done */
	for (i = 0; i < per_page && mon_idx[i].r_idx; i++)
	{
		byte attr;

		/* Get the race index */
		int r_idx = mon_idx[mon_top + i].r_idx;

		/* Access the race */
		monster_race *r_ptr = &r_info[r_idx];
		monster_lore *l_ptr = &l_list[r_idx];

		char race_name[80];

		/* Handle player chosts differently */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			char racial_name[80];

			/* Get the ghost name. */
			strcpy(race_name, ghost_name);

			/* Get the racial name. */
			strcpy(racial_name, r_name + r_ptr->name);

			/* Build the ghost name. */
			strcat(race_name, ", the ");
			strcat(race_name, racial_name);
		}

		/* Get the monster race name (singular)*/
		else monster_desc_race(race_name, sizeof(race_name), r_idx);

		/* Choose a color */
		attr = ((i + mon_top == mon_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Display the name */
		c_prt(attr, race_name, row + i, col);

		if (cheat_know)
		{
			c_prt(attr, format ("%d", r_idx), row + i, 60);
		}

		/* Display symbol */
		Term_putch(68, row + i, r_ptr->x_attr, r_ptr->x_char);

		/* Beware of bigtile mode */
		if (use_bigtile)
		{
			if (r_ptr->x_attr & 0x80)
			{
			  	Term_putch(69, row + i, 255, -1);
		    	}
			else
			{
			  	Term_putch(69, row + i, 0, ' ');
			}
		}

		/* Display kills */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/*use alive/dead for uniques*/
			put_str(format("%s", (r_ptr->max_num == 0) ? "dead" : "alive"),
			        row + i, 73);
		}
		else put_str(format("%5d", l_ptr->pkills), row + i, 73);

	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}

	/*Clear the monster count line*/
	Term_erase(0, TERM_LAST_ROW - 1, 255);

	if (monster_group_char[grp_cur] != (char *) -1L)
   	{
		c_put_str(TERM_L_BLUE, format("Total Creatures Slain: %8d.", slay_count), TERM_LAST_ROW - 1, col);
	}
	else
	{
		c_put_str(TERM_L_BLUE, format("Known Uniques: %3d, Slain Uniques: %3d.", known_uniques, dead_uniques),
						TERM_LAST_ROW - 1, col);
	}
}


/*
 * Display known monsters.
 */
static void do_cmd_knowledge_monsters(void)
{
	int i, len, max;
	int grp_cur, grp_top;
	int mon_cur, mon_top;
	int grp_cnt, grp_idx[100];
	monster_list_entry *mon_idx;
	int monster_count;

	int column = 0;
	bool flag;
	bool redraw;

	/* Allocate the "mon_idx" array */
	C_MAKE(mon_idx, z_info->r_max, monster_list_entry);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; monster_group_text[i] != NULL; i++)
	{

		/* Measure the label */
		len = strlen(monster_group_text[i]);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any monsters are known */
		if ((monster_group_char[i] == ((char *) -1L)) || collect_monsters(i, mon_idx, 0x01))
		{
			/* Build a list of groups with known monsters */
			grp_idx[grp_cnt++] = i;
		}

	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	grp_cur = grp_top = 0;
	mon_cur = mon_top = 0;

	flag = FALSE;
	redraw = TRUE;

	while (!flag)
	{
		char ch;

		if (redraw)
		{
			clear_from(0);

			prt("Knowledge - Monsters", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (cheat_know) prt("Idx", 4, 60);
			prt("Sym   Kills", 4, 67);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < BROWSER_ROWS; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + BROWSER_ROWS) grp_top = grp_cur - BROWSER_ROWS + 1;

		/* Scroll monster list */
		if (mon_cur < mon_top) mon_top = mon_cur;
		if (mon_cur >= mon_top + BROWSER_ROWS) mon_top = mon_cur - BROWSER_ROWS + 1;

		/* Display a list of monster groups */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, monster_group_text, grp_cur, grp_top);

		/* Get a list of monsters in the current group */
		monster_count = collect_monsters(grp_idx[grp_cur], mon_idx, 0x00);

		/* Display a list of monsters in the current group */
		display_monster_list(max + 3, 6, BROWSER_ROWS, mon_idx, mon_cur, mon_top, grp_cur);

		/* Track selected monster, to enable recall in sub-win*/
		p_ptr->monster_race_idx = mon_idx[mon_cur].r_idx;

		/* Prompt */
		prt("<dir>, 'r' to recall, ESC", TERM_LAST_ROW, 0);

		/* Hack -- handle stuff */
		handle_stuff();

		if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (mon_cur - mon_top));
		}

		ch = inkey();

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'R':
			case 'r':
			{
				/* Recall on screen */
				if (mon_idx[mon_cur].r_idx)
				{
					screen_roff(mon_idx[mon_cur].r_idx);

					(void) inkey();

					redraw = TRUE;
				}
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &mon_cur, monster_count);

				/*Update to a new monster*/
				p_ptr->window |= (PW_MONSTER);

				break;
			}
		}
	}

	/* XXX XXX Free the "mon_idx" array */
	KILL(mon_idx);
}

/*
 * Add a pval so the object descriptions don't look strange*
 */
void apply_magic_fake(object_type *o_ptr)
{
	/* Analyze type */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		{
			o_ptr->pval = 1;
			break;
		}

		/*many rings need a pval*/
		case TV_RING:
		{
			switch (o_ptr->sval)
			{
				case SV_RING_STR:
				case SV_RING_CON:
				case SV_RING_DEX:
				case SV_RING_INT:
				case SV_RING_SPEED:
				case SV_RING_SEARCHING:
				{
					o_ptr->pval = 1;
					break;
				}

				case SV_RING_AGGRAVATION:
				{
					o_ptr->ident |= (IDENT_CURSED);
					break;
				}
				case SV_RING_WEAKNESS:
				case SV_RING_STUPIDITY:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = -1;

					break;
				}
				/* WOE */
				case SV_RING_WOE:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->to_a = -1;
					o_ptr->pval = -1;

					break;
				}
				/* Ring that increase damage */
				case SV_RING_DAMAGE:
				{
					/* Bonus to damage */
					o_ptr->to_d = 1;

					break;
				}
				/* Ring that increase accuracy */
				case SV_RING_ACCURACY:
				{
					/* Bonus to hit */
					o_ptr->to_h = 1;

					break;
				}
				/* Rings that provide of Protection */
				case SV_RING_PROTECTION:
				case SV_RING_FLAMES:
				case SV_RING_ACID:
				case SV_RING_ICE:
				case SV_RING_LIGHTNING:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 1;

					break;
				}
				/*both to-hit and to-damage*/
				case SV_RING_SLAYING:
				{
					/* Bonus to damage and to hit */
					o_ptr->to_d = 1;
					o_ptr->to_h = 1;

					break;
				}
				default: break;

			}
			/*break for TVAL-Rings*/
			break;
		}

		case TV_AMULET:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Amulet of wisdom/charisma/infravision */
				case SV_AMULET_WISDOM:
				case SV_AMULET_CHARISMA:
				case SV_AMULET_INFRAVISION:
				case SV_AMULET_SEARCHING:
				case SV_AMULET_ESP:
				case SV_AMULET_DEVOTION:
				case SV_AMULET_TRICKERY:
				{
					/* Stat bonus */
					o_ptr->pval = 1;

					break;
				}

				/* Amulet of the Magi -- never cursed */
				case SV_AMULET_THE_MAGI:
				{
					o_ptr->pval = 1;
					o_ptr->to_a = 1;

					break;
				}

				/* Amulet of Weaponmastery -- never cursed */
				case SV_AMULET_WEAPONMASTERY:
				{
					o_ptr->to_h = 1;
					o_ptr->to_d = 1;
					o_ptr->pval = 1;

					break;
				}

				/* Amulet of Doom -- always cursed */
				case SV_AMULET_DOOM:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = -1;
					o_ptr->to_a = -1;

					break;
				}

				default: break;

			}
			/*break for TVAL-Amulets*/
			break;
		}

		case TV_LITE:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				case SV_LITE_TORCH:
				case SV_LITE_LANTERN:
				{
					o_ptr->timeout = 1;

					break;
				}

			}
			/*break for TVAL-Lites*/
			break;
		}

		/*give then one charge*/
		case TV_STAFF:
		case TV_WAND:
		{
			o_ptr->pval = 1;

			break;
		}

	}

}

/*
 * Describe fake object
 */
static void desc_obj_fake(int k_idx)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Create the object */
	object_prep(i_ptr, k_idx);

	/*add minimum bonuses so the descriptions don't look strange*/
	apply_magic_fake(i_ptr);

	/* It's fully known */
	i_ptr->ident |= IDENT_KNOWN;

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	object_info_screen(i_ptr);
}

/*
 * Display the objects in a group.
 */
static void display_object_list(int col, int row, int per_page, int object_idx[],
	int object_cur, int object_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && object_idx[i]; i++)
	{
		char buf[80];

		/* Get the object index */
		int k_idx = object_idx[object_top + i];

		/* Access the object */
		object_kind *k_ptr = &k_info[k_idx];

		/* Choose a color */
		byte attr = ((k_ptr->aware) ? TERM_WHITE : TERM_SLATE);
		byte cursor = ((k_ptr->aware) ? TERM_L_BLUE : TERM_BLUE);
		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Acquire the basic "name" of the object*/
	    	strip_name(buf, k_idx);

		/* Display the name */
		c_prt(attr, buf, row + i, col);

		if (cheat_know) c_prt(attr, format ("%d", k_idx), row + i, 70);

		if (k_ptr->aware)
		{
			/* Obtain attr/char */
			byte a = k_ptr->flavor ? (flavor_info[k_ptr->flavor].x_attr): k_ptr->x_attr;
			byte c = k_ptr->flavor ? (flavor_info[k_ptr->flavor].x_char): k_ptr->x_char;

			/* Display symbol */
			Term_putch(76, row + i, a, c);

			/* Beware of bigtile mode */
			if (use_bigtile)
			{
				if (a & 0x80)
				{
			  		Term_putch(77, row + i, 255, -1);
		    		}
				else
				{
			  		Term_putch(77, row + i, 0, ' ');
				}
			}
		}
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}

/*
 * The label of the group who collects miscellaneous features.
 */
#define MISC_GRP "Misc"


/*
 * Description of each feature group.
 */
static cptr feature_group_text[] =
{
	"Floors",
	"Player Traps",
	"Monster Traps",
	"Runes",
	"Doors",
	"Stairs",
	"Walls",
	"Stores",
	"Bridges",
	"Water",
	"Lava",
	"Ice",
	"Acid",
	"Oil",
	"Forest",
	"Sand/Mud",
	"Fire",
	MISC_GRP,
	NULL
};

/*
 * Flags of features in each group.
 * The misc. group must have all flags set to 0.
 */
static u32b feature_group_flag[] =
{
	FF1_FLOOR, 0, 0,
	0, FF2_TRAP_PASSIVE, 0,
	0, FF2_TRAP_MON, 0,
	0, FF2_TRAP_SMART, 0,
	FF1_DOOR, 0, 0,
	FF1_STAIRS, 0, 0,
	FF1_WALL, 0, 0,
	FF1_SHOP, 0, 0,
	0, FF2_BRIDGED, 0,
	0, 0, FF3_WATER,
	0, 0, FF3_LAVA,
	0, 0, FF3_ICE,
	0, 0, FF3_ACID,
	0, 0, FF3_OIL,
	0, 0, FF3_FOREST,
	0, 0, FF3_SAND | FF3_MUD,
	0, 0, FF3_FIRE,
	0, 0, 0,   /*Misc Group*/
	0, 0, 0
};

/*
 * Build a list of feature indexes in the given group. Return the number
 * of features in the group.
 */
static int collect_features(int grp_cur, int *feat_idx)
{
	int i, feat_cnt = 0;

	u32b flags1 = feature_group_flag[grp_cur*3+0];
	u32b flags2 = feature_group_flag[grp_cur*3+1];
	u32b flags3 = feature_group_flag[grp_cur*3+2];

	/*
 	 * We use the misc. group to collect features who don't match any
 	 * other group
 	 */
 	bool collecting_misc = streq(feature_group_text[grp_cur], MISC_GRP);

 	/* Special rules */
 	if (collecting_misc)
 	{
 		int grp;

 		/* We just collect the flags of the other groups here */
 		for (grp = 0; feature_group_text[grp]; grp++)
 		{
 			flags1 |= feature_group_flag[grp*3+0];
 			flags2 |= feature_group_flag[grp*3+1];
 			flags3 |= feature_group_flag[grp*3+2];
 		}
	}

	/*
	 * We ignore feature #0 to avoid a crash since #0 is used to mark the
	 * end of feat_idx
	 */
	for (i = 1; i < z_info->f_max; i++)
	{
		int idx, ii, tmp;

		/* Access the features */
		feature_type *f_ptr = &f_info[i];
		feature_lore *f_l_ptr = &f_l_list[i];

		/* Skip empty features */
		if (!f_ptr->name) continue;

		/* Skip features we never see */
		if (f_ptr->f_mimic != i) continue;

		/* Don't spoil terrain */
		if (!f_ptr->f_everseen && !f_l_ptr->f_l_sights && !cheat_know) continue;

		/* Check for any flags matching in the group */
		if (_feat_ff1_match(f_ptr, flags1) ||
 			_feat_ff2_match(f_ptr, flags2) ||
 			_feat_ff3_match(f_ptr, flags3))
		{
			/*
 			 * Reject interesting features when collecting the
 			 * misc. group
 			 */
 			if (collecting_misc) continue;
 		}
		/* No matching flags. Ignore boring feature */
		else
		{
			/* Unless when collecting the misc. group */
			if (!collecting_misc) continue;
		}

		for (ii = 0, idx = i; ii < feat_cnt; ii++)
		{
			char name1[80], name2[80];

			feature_desc(name1, sizeof(name1), idx,
				FALSE, FALSE);

			feature_desc(name2, sizeof(name2), feat_idx[ii],
				FALSE, FALSE);

			if (strcmp(name1, name2) <= 0)
			{
				tmp = idx;
				idx = feat_idx[ii];
				feat_idx[ii] = tmp;
			}
		}

		/* Add the feature */
		feat_idx[feat_cnt++] = idx;

	}

	/* Terminate the list */
	feat_idx[feat_cnt] = 0;

	/* Return the number of races */
	return feat_cnt;
}


/*
 * Display the features in a group.
 */
static void display_feature_list(int col, int row, int per_page, int *feat_idx,
	int feat_cur, int feat_top)
{
	int i;


/* This Unangband code doesn't fit in NPP, yet. -DG- */
#if 0


	int col2 = 70;
	int col3 = 72;

	/* Correct columns 1 and 3 */
	if (use_bigtile)
	{
		col2++; col3++; col3++;
	}

#endif /* 0 */

	/* Display lines until done */
	for (i = 0; i < per_page && feat_idx[i]; i++)
	{
		byte attr;
		char name[80];

		/* Get the feature index */
		int f_idx = feat_idx[feat_top + i];

		/* Access the feature */
		feature_type *f_ptr = &f_info[f_idx];

		/* Choose a color */
		attr = ((i + feat_top == feat_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Get feature name */
		feature_desc(name, sizeof(name), f_idx, FALSE, FALSE);

		/* Display the name */
		c_prt(attr, name, row + i, col);

		/* Hack -- visual_list mode */
		if (per_page == 1)
		{
			c_prt(attr, format(f_ptr->x_char >= 0 ? "%02x/%02x" :
				"%02x/%d", f_ptr->x_attr, f_ptr->x_char),
				row + i, 60);
		}
		else if (cheat_know)
		{
			c_prt(attr, format("%3d", f_idx), row + i, 60);
		}

		/* Display symbol */
		Term_putch(68, row + i, f_ptr->x_attr, f_ptr->x_char);

		if (use_bigtile)
		{
			if ((f_ptr->x_attr) & 0x80)
			{
				Term_putch(68 + 1, row + i, 255, -1);
			}
			else
			{
				Term_putch(68 + 1, row + i, 0, ' ');
			}
		}

/* This Unangband code doesn't fit in NPP, yet. -DG- */
#if 0


		/* Tile supports special lighting? */
		if (!(f_ptr->f_flags2 & (FF2_ATTR_LITE)))
		{
			prt("       ", row + i, col2);
			continue;
		}

		Term_putch(col2, row + i, TERM_SLATE, '(');

		Term_putch(col3, row + i, TERM_SLATE, '/');

		if (use_bigtile)
		{
			Term_putch(col3 + 3, row + i, TERM_SLATE, ')');
		}
		else
		{
			Term_putch(col3 + 2, row + i, TERM_SLATE, ')');
		}

		/* Display Character */
		Term_putch(col2+1, row + i, f_ptr->x_attr, f_ptr->x_char);

		if (use_bigtile)
		{
			if ((f_ptr->x_attr) & 0x80)
			{
				Term_putch(col2 + 2, row + i, 255, -1);
			}
			else
			{
				Term_putch(col2 + 2, row + i, 0, ' ');
			}
		}

#endif /* 0 */

	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}


/*
 * Interact with feature visuals.
 */
static void do_cmd_knowledge_features(void)
{
	int i, len, max;
	int grp_cur, grp_top;
	int feat_cur, feat_top;
	int grp_cnt, grp_idx[100];
	int feat_cnt;
	int *feat_idx;

	int column = 0;
	bool flag;
	bool redraw;

	/* Allocate the "feat_idx" array */
	C_MAKE(feat_idx, z_info->f_max, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; feature_group_text[i] != NULL; i++)
	{
		/* Measure the label */
		len = strlen(feature_group_text[i]);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any features are known */
		if (collect_features(i, feat_idx))
		{
			/* Build a list of groups with known features */
			grp_idx[grp_cnt++] = i;
		}
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	grp_cur = grp_top = 0;
	feat_cur = feat_top = 0;

	flag = FALSE;
	redraw = TRUE;

	while (!flag)
	{
		char ch;

		if (redraw)
		{
			clear_from(0);

			prt("Knowledge - features", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (cheat_know) prt("Idx", 4, 60);
			prt("Sym", 4, 67);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < BROWSER_ROWS; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + BROWSER_ROWS) grp_top = grp_cur - BROWSER_ROWS + 1;

		/* Scroll feature list */
		if (feat_cur < feat_top) feat_top = feat_cur;
		if (feat_cur >= feat_top + BROWSER_ROWS) feat_top = feat_cur - BROWSER_ROWS + 1;

		/* Display a list of feature groups */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, feature_group_text, grp_cur, grp_top);

		/* Get a list of features in the current group */
		feat_cnt = collect_features(grp_idx[grp_cur], feat_idx);

		/* Display a list of features in the current group */
		display_feature_list(max + 3, 6, BROWSER_ROWS, feat_idx, feat_cur, feat_top);

		/* Mega Hack -- track this feature */
		if (feat_cnt) feature_kind_track(feat_idx[feat_cur]);

		/* Prompt */
		prt("<dir>, 'r' to recall, ESC", TERM_LAST_ROW, 0);

		/* Hack -- handle stuff */
		handle_stuff();

		if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (feat_cur - feat_top));
		}

		ch = inkey();

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'R':
			case 'r':
			{
				/* Recall on screen */
				if (f_info[feat_idx[feat_cur]].name)
				{

					screen_feature_roff(feat_idx[feat_cur]);

					(void) inkey();

					redraw = TRUE;
				}
				break;
			}


			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &feat_cur, feat_cnt);
				break;
			}
		}
	}

	/* XXX XXX Free the "feat_idx" array */
	KILL(feat_idx);
}


/*
 * Display contents of the Home. Code taken from the player death interface
 * and the show known objects function. -LM-
 */
static void do_cmd_knowledge_home(void)
{
	int k;

	store_type *st_ptr = &store[STORE_HOME];

	FILE *fp;

	char o_name[120];

	char file_name[1024];

	/* Temporary file */
	fp = my_fopen_temp(file_name, sizeof(file_name));

	text_out_hook = text_out_to_file;
	text_out_file = fp;

	/* Failure */
	if (!fp)
	{
		msg_print("Could not open a temporary file to show the contents of your	home.");
		return;
	}


	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Display contents of the home */
		for (k = 0; k < st_ptr->stock_num; k++)
		{

			object_desc(o_name, sizeof(o_name), &st_ptr->stock[k], TRUE, 3);
			fprintf(fp, "%c) %s\n", I2A(k), o_name);

			/* Describe random object attributes*/
			identify_random_gen(&st_ptr->stock[k]);

		}
	}

	else fprintf(fp, "[Your Home Is Empty]\n\n");

	/* Close the file */
	my_fclose(fp);

	/* Display the file contents */
	show_file(file_name, "Contents of Your Home", 0, 2);

	/* Remove the file */
	fd_kill(file_name);
}



/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(void)
{
	int i, len, max;
	int grp_cur, grp_top;
	int object_old, object_cur, object_top;
	int grp_cnt, grp_idx[100];
	int object_cnt;
	int *object_idx;

	int column = 0;
	bool flag;
	bool redraw;

	/* Allocate the "object_idx" array */
	C_MAKE(object_idx, z_info->k_max, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; object_group_text[i] != NULL; i++)
	{
		/* Measure the label */
		len = strlen(object_group_text[i]);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any monsters are known */
		if (collect_objects(i, object_idx))
		{
			/* Build a list of groups with known monsters */
			grp_idx[grp_cnt++] = i;
		}
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	grp_cur = grp_top = 0;
	object_cur = object_top = 0;
	object_old = -1;

	flag = FALSE;
	redraw = TRUE;

	while (!flag)
	{
		char ch;

		if (redraw)
		{
			clear_from(0);

			prt("Knowledge - objects", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (cheat_know) prt("Idx", 4, 70);
			prt("Sym", 4, 75);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < BROWSER_ROWS; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + BROWSER_ROWS) grp_top = grp_cur - BROWSER_ROWS + 1;

		/* Scroll monster list */
		if (object_cur < object_top) object_top = object_cur;
		if (object_cur >= object_top + BROWSER_ROWS) object_top = object_cur - BROWSER_ROWS + 1;

		/* Display a list of object groups */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, object_group_text, grp_cur, grp_top);

		/* Get a list of objects in the current group */
		object_cnt = collect_objects(grp_idx[grp_cur], object_idx);

		/* Display a list of objects in the current group */
		display_object_list(max + 3, 6, BROWSER_ROWS, object_idx, object_cur, object_top);

		/* Prompt */
		prt("<dir>, 'r' to recall, ESC", TERM_LAST_ROW, 0);

		/* Mega Hack -- track this monster race */
		if (object_cnt) object_kind_track(object_idx[object_cur]);

		/* The "current" object changed */
		if (object_old != object_idx[object_cur])
		{
			/* Hack -- handle stuff */
			handle_stuff();

			/* Remember the "current" object */
			object_old = object_idx[object_cur];
		}

		if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (object_cur - object_top));
		}

		ch = inkey();

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'R':
			case 'r':
			{
				/* Recall on screen */
				desc_obj_fake(object_idx[object_cur]);

				redraw = TRUE;
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &object_cur, object_cnt);
				break;
			}
		}
	}

	/* XXX XXX Free the "object_idx" array */
	KILL(object_idx);
}


/*
 * Display kill counts
 */
static void do_cmd_knowledge_kills(void)
{
	int n, i;

	FILE *fff;

	char file_name[1024];

	u16b *who;
	u16b why = 4;


	/* Temporary file */
	fff = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fff) return;


	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Require non-unique monsters */
		if (r_ptr->flags1 & RF1_UNIQUE) continue;

		/* Collect "appropriate" monsters */
		if (l_ptr->pkills > 0) who[n++] = i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort by kills (and level) */
	ang_sort(who, &why, n);

	/* Print the monsters (highest kill counts first) */
	for (i = n - 1; i >= 0; i--)
	{
		monster_race *r_ptr = &r_info[who[i]];
		monster_lore *l_ptr = &l_list[who[i]];

		/* Print a message */
		fprintf(fff, "     %-40s  %5d\n",
		        (r_name + r_ptr->name), l_ptr->pkills);
	}

	/* Free the "who" array */
	FREE(who);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Kill counts", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Create and return an empty file in writing mode to append notes.
 * It returns a copy of the file name in "path".
 * "max" must be the maximum size in bytes of "path".
 * Returns NULL on failure
 */
FILE *create_notes_file(char path[], size_t max)
{
	FILE *fp;

#ifdef TEMPORARY_NOTES_FILES

	/* Create a temporary file */
	fp = my_fopen_temp(path, max);

#else

	/* Create the notes file in the lib/save folder */
	char name[80];

	/* Hack -- No base name yet */
	if (!op_ptr->base_name[0]) process_player_name(FALSE);

	/* Format the name of the notes file */
	strnfmt(name, sizeof(name), "%s.nte", op_ptr->base_name);

	/* Build the full path to the notes file */
	path_build(path, max, ANGBAND_DIR_SAVE, name);

	/* Create it */
	fp = my_fopen(path, "w");

#endif

	return (fp);
}

/*
 * Close and destroy the notes file. notes_file and notes_fname variables are cleared
 */
void delete_notes_file(void)
{
	/* Close the notes file */
	if (notes_file)
	{
		my_fclose(notes_file);

		notes_file = NULL;
	}

	/* Delete the notes file */
	if (notes_fname[0])
	{
		fd_kill(notes_fname);

		notes_fname[0] = '\0';
	}
}

/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
	char ch;

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Save screen */
	screen_save();

	/* Interact until done */
	while (1)
	{

		store_type *st_ptr = &store[STORE_HOME];

		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Display current knowledge", 2, 0);

		/* Give some choices */
		prt("(1) Display known artifacts", 4, 5);
		prt("(2) Display known monsters", 5, 5);
		prt("(3) Display known objects", 6, 5);
		prt("(4) Display known features", 7,5);
		prt("(5) Display hall of fame", 8, 5);
		prt("(6) Display kill counts", 9, 5);

		/*allow the player to see the notes taken if that option is selected*/
		c_put_str((adult_take_notes ? TERM_WHITE : TERM_SLATE) ,
					"(7) Display character notes file", 10, 5);

		/*give player option to see home inventory if there is anything in the house*/
		c_put_str((st_ptr->stock_num ? TERM_WHITE : TERM_SLATE) ,
				 	"(8) Display contents of your home", 11, 5);

		/* Prompt */
		prt("Command: ", 13, 0);

		/* Prompt */
		ch = inkey();

		/* Done */
		if (ch == ESCAPE) break;

		/* Artifacts */
		if (ch == '1')
		{
			do_cmd_knowledge_artifacts();
		}

		/* Uniques */
		else if (ch == '2')
		{
			do_cmd_knowledge_monsters();
		}

		/* Objects */
		else if (ch == '3')
		{
			do_cmd_knowledge_objects();
		}
		/* Objects */
		else if (ch == '4')
		{
			do_cmd_knowledge_features();
		}
		/* Scores */
		else if (ch == '5')
		{
			show_scores();
		}

		/* Scores */
		else if (ch == '6')
		{
			do_cmd_knowledge_kills();
		}

		/* Ntoes file, if one exists */
		else if ((ch == '7') && (adult_take_notes))
		{
			/* Spawn */
			do_cmd_knowledge_notes();
		}

		/* Home inventory, if there is anything in the house */
		else if ((ch == '8') && (st_ptr->stock_num))
		{
			/* Spawn */
			do_cmd_knowledge_home();
		}

		/* Unknown option */
		else
		{
			bell("Illegal command for knowledge!");
		}

		/* Flush messages */
		message_flush();
	}


	/* Load screen */
	screen_load();
}
