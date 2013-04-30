/* File: cmd4.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"



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
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_MONSTER | PW_OBJECT);

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
		Term_fresh();

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

	/* Prompt */
	p = "['c' to change name, 'f' to file, 'h' to change mode, or ESC]";

	/* Save screen */
	screen_save();

	/* Forever */
	while (1)
	{
		/* Display the player */
		display_player(mode);

		/* Prompt */
		Term_putstr(2, 23, -1, TERM_WHITE, p);

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

			sprintf(ftmp, "%s.txt", op_ptr->base_name);

			if (get_string("File name: ", ftmp, 80))
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
		else if (ch == 'h')
		{
			mode = !mode;
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
			if (!askfor_aux(shower, 80)) continue;

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
			if (!askfor_aux(finder, 80)) continue;

			/* Show it */
			strcpy(shower, finder);

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
static void do_cmd_pref_file_hack(int row)
{
	char ftmp[80];

	/* Prompt */
	prt("Command: Load a user pref file", row, 0);

	/* Prompt */
	prt("File: ", row + 2, 0);

	/* Default filename */
	sprintf(ftmp, "%s.prf", op_ptr->base_name);

	/* Ask for a file (or cancel) */
	if (!askfor_aux(ftmp, 80)) return;

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
static void do_cmd_options_aux(int page, cptr info)
{
	char ch;

	int i, k = 0, n = 0;

	int opt[OPT_PAGE_PER];

	char buf[80];


	/* Scan the options */
	for (i = 0; i < OPT_PAGE_PER; i++)
	{
		/* Collect options on this "page" */
		if (option_page[page][i] != 255)
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
		sprintf(buf, "%s (RET to advance, y/n to set, ESC to accept) ", info);
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%s)",
				option_desc[opt[i]],
				op_ptr->opt[opt[i]] ? "yes" : "no ",
				option_text[opt[i]]);
			c_prt(a, buf, i + 2, 0);
		}

		/* Hilite current option */
		move_cursor(k + 2, 50);

		/* Get a key */
		ch = inkey();

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
				break;
			}

			case 'y':
			case '6':
			{
				op_ptr->opt[opt[k]] = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case '4':
			{
				op_ptr->opt[opt[k]] = FALSE;
				k = (k + 1) % n;
				break;
			}

			case '?':
			{
				sprintf(buf, "option.txt#%s", option_text[opt[k]]);
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
static void do_cmd_options_win(void)
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
		for (i = 0; i < 16; i++)
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
			x = (x + ddx[d] + 8) % 8;
			y = (y + ddy[d] + 16) % 16;
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
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Write all current options to the given preference file in the
 * lib/user directory. Modified from KAmband 1.8.
 */
static errr option_dump(cptr fname)
{
	int i, j;

	FILE *fff;

	char buf[1024];

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);


	/* Skip some lines */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic option dump\n\n");

	/* Dump options (skip cheat, adult, score) */
	for (i = 0; i < OPT_CHEAT; i++)
	{
		/* Require a real option */
		if (!option_text[i]) continue;

		/* Comment */
		fprintf(fff, "# Option '%s'\n", option_desc[i]);

		/* Dump the option */
		if (op_ptr->opt[i])
		{
			fprintf(fff, "Y:%s\n", option_text[i]);
		}
		else
		{
			fprintf(fff, "X:%s\n", option_text[i]);
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

		/* Load and Append */
		prt("(L) Load a user pref file", 13, 5);
		prt("(A) Append options to a file", 14, 5);

		/* Special choices */
		prt("(D) Base Delay Factor", 16, 5);
		prt("(H) Hitpoint Warning", 17, 5);

		/* Prompt */
		prt("Command: ", 19, 0);

		/* Get command */
		ch = inkey();

		/* Exit */
		if (ch == ESCAPE) break;

		/* General Options */
		else if (ch == '1')
		{
			do_cmd_options_aux(0, "User Interface Options");
		}

		/* Disturbance Options */
		else if (ch == '2')
		{
			do_cmd_options_aux(1, "Disturbance Options");
		}

		/* Inventory Options */
		else if (ch == '3')
		{
			do_cmd_options_aux(2, "Game-Play Options");
		}

		/* Efficiency Options */
		else if (ch == '4')
		{
			do_cmd_options_aux(3, "Efficiency Options");
		}

		/* Display Options */
		else if (ch == '5')
		{
			do_cmd_options_aux(4, "Display Options");
		}

		/* Birth Options */
		else if (ch == '6')
		{
			do_cmd_options_aux(5, "Birth Options");
		}

		/* Cheating Options */
		else if (ch == '7')
		{
			do_cmd_options_aux(6, "Cheat Options");
		}
#if 0
		/* Variant Options */
		else if (ch == '8')
		{
			do_cmd_options_aux(7, "Variant Options");
		}

		/* Save-File Options */
		else if (ch == '9')
		{
			do_cmd_options_aux(8, "Save-File Options");
		}
#endif
		/* Window flags */
		else if ((ch == 'W') || (ch == 'w'))
		{
			do_cmd_options_win();
		}

		/* Load a user pref file */
		else if ((ch == 'L') || (ch == 'l'))
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(20);
		}

		/* Append options to a file */
		else if ((ch == 'A') || (ch == 'a'))
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Append options to a file", 20, 0);

			/* Prompt */
			prt("File: ", 21, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, 80)) continue;

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
		}

		/* Hack -- Base Delay Factor */
		else if ((ch == 'D') || (ch == 'd'))
		{
			/* Prompt */
			prt("Command: Base Delay Factor", 20, 0);

			/* Get a new value */
			while (1)
			{
				char cx;
				int msec = op_ptr->delay_factor * op_ptr->delay_factor;
				prt(format("Current base delay factor: %d (%d msec)",
					   op_ptr->delay_factor, msec), 22, 0);
				prt("New base delay factor (0-9 or ESC to accept): ", 21, 0);

				cx = inkey();
				if (cx == ESCAPE) break;
				if (isdigit(cx)) op_ptr->delay_factor = D2I(cx);
				else bell("Illegal delay factor!");
			}
		}

		/* Hack -- hitpoint warning factor */
		else if ((ch == 'H') || (ch == 'h'))
		{
			/* Prompt */
			prt("Command: Hitpoint Warning", 20, 0);

			/* Get a new value */
			while (1)
			{
				char cx;
				prt(format("Current hitpoint warning: %2d%%",
					   op_ptr->hitpoint_warn * 10), 22, 0);
				prt("New hitpoint warning (0-9 or ESC to accept): ", 21, 0);

				cx = inkey();
				if (cx == ESCAPE) break;
				if (isdigit(cx)) op_ptr->hitpoint_warn = D2I(cx);
				else bell("Illegal hitpoint warning!");
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



#ifdef ALLOW_MACROS

/*
 * Hack -- append all current macros to the given file
 */
static errr macro_dump(cptr fname)
{
	int i;

	FILE *fff;

	char buf[1024];


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);


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
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);


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


	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}


#endif


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
#ifdef ALLOW_MACROS
		prt("(2) Append macros to a file", 5, 5);
		prt("(3) Query a macro", 6, 5);
		prt("(4) Create a macro", 7, 5);
		prt("(5) Remove a macro", 8, 5);
		prt("(6) Append keymaps to a file", 9, 5);
		prt("(7) Query a keymap", 10, 5);
		prt("(8) Create a keymap", 11, 5);
		prt("(9) Remove a keymap", 12, 5);
		prt("(0) Enter a new action", 13, 5);
#endif /* ALLOW_MACROS */

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
		}

#ifdef ALLOW_MACROS

		/* Save macros */
		else if (ch == '2')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Append macros to a file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Dump the macros */
			(void)macro_dump(ftmp);

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
				strcpy(macro_buffer, macro__act[k]);

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
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Dump the macros */
			(void)keymap_dump(ftmp);

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
				strcpy(macro_buffer, act);

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
			}
		}

#endif /* ALLOW_MACROS */

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

	const char *empty_symbol = "<< ? >>";
	const char *empty_symbol2 = "\0";
	const char *empty_symbol3 = "\0";

	if (use_trptile && use_bigtile)
	{
		empty_symbol = "// ?????? \\\\";
		empty_symbol2 = "   ??????   ";
		empty_symbol3 = "\\\\ ?????? //";
	}
	else if (use_dbltile && use_bigtile)
	{
		empty_symbol = "// ???? \\\\";
		empty_symbol2 = "\\\\ ???? //";
	}
	else if (use_trptile)
	{
		empty_symbol = "// ??? \\\\";
		empty_symbol2 = "   ???   ";
		empty_symbol3 = "\\\\ ??? //";
	}
	else if (use_dbltile)
	{
		empty_symbol = "// ?? \\\\";
		empty_symbol2 = "\\\\ ?? //";
	}
	else if (use_bigtile) empty_symbol = "<< ?? >>";

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
#ifdef ALLOW_VISUALS
		prt("(2) Dump monster attr/chars", 5, 5);
		prt("(3) Dump object attr/chars", 6, 5);
		prt("(4) Dump feature attr/chars", 7, 5);
		prt("(5) Dump flavor attr/chars", 8, 5);
		prt("(6) Change monster attr/chars", 9, 5);
		prt("(7) Change object attr/chars", 10, 5);
		prt("(8) Change feature attr/chars", 11, 5);
		prt("(9) Change flavor attr/chars", 12, 5);
#endif
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

#ifdef ALLOW_VISUALS

		/* Dump monster attr/chars */
		else if (ch == '2')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump monster attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;


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
				fprintf(fff, "R:%d:0x%02X:0x%02X\n\n", i,
					(byte)(r_ptr->x_attr), (byte)(r_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped monster attr/chars.");
		}

		/* Dump object attr/chars */
		else if (ch == '3')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump object attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;


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

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped object attr/chars.");
		}

		/* Dump feature attr/chars */
		else if (ch == '4')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump feature attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;


			/* Skip some lines */
			fprintf(fff, "\n\n");

			/* Start dumping */
			fprintf(fff, "# Feature attr/char definitions\n\n");

			/* Dump features */
			for (i = 0; i < z_info->f_max; i++)
			{
				feature_type *f_ptr = &f_info[i];

				/* Skip non-entries */
				if (!f_ptr->name) continue;

				/* Dump a comment */
				fprintf(fff, "# %s\n", (f_name + f_ptr->name));

				/* Dump the feature attr/char info */
				fprintf(fff, "F:%d:0x%02X:0x%02X:%s\n\n", i,
					(byte)(f_ptr->x_attr), (byte)(f_ptr->x_char),(f_ptr->flags2 & (FF2_ATTR_LITE)) ? "YES" : "NO");
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped feature attr/chars.");
		}

		/* Dump flavor attr/chars */
		else if (ch == '5')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump flavor attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;


			/* Skip some lines */
			fprintf(fff, "\n\n");

			/* Start dumping */
			fprintf(fff, "# Flavor attr/char definitions\n\n");

			/* Dump flavors */
			for (i = 0; i < z_info->x_max; i++)
			{
				flavor_type *x_ptr = &x_info[i];

				/* Dump a comment */
				fprintf(fff, "# %s\n", (x_text + x_ptr->text));

				/* Dump the flavor attr/char info */
				fprintf(fff, "L:%d:0x%02X:0x%02X\n\n", i,
					(byte)(x_ptr->x_attr), (byte)(x_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

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

				int linec = (use_trptile ? 22: (use_dbltile ? 21 : 20));

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("Monster = %d, Name = %-40.40s",
						   r, (r_name + r_ptr->name)));

				/* Label the Default values */
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("Default attr/char = %3u / %3u", da, dc));
				Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol2);
				if (use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol3);

				Term_putch(43, 19, da, dc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, 19, da, dc);
				}

				/* Label the Current values */
				Term_putstr(10, linec, -1, TERM_WHITE,
					    format("Current attr/char = %3u / %3u", ca, cc));
				Term_putstr(40, linec, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, linec+1, -1, TERM_WHITE, empty_symbol2); 
				if (use_trptile) Term_putstr (40, linec+2, -1, TERM_WHITE, empty_symbol3); 
				Term_putch(43, linec, ca, cc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, linec++, ca, cc);
				}
				if (use_trptile) linec++;

				/* Prompt */
				Term_putstr(0, linec + 2, -1, TERM_WHITE,
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

				int linec = (use_trptile ? 22: (use_dbltile ? 21 : 20));

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("Object = %d, Name = %-40.40s",
						   k, (k_name + k_ptr->name)));

				/* Label the Default values */
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("Default attr/char = %3u / %3u", da, dc));
				Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol2);
				if (use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol3);

				Term_putch(43, 19, da, dc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, 19, da, dc);
				}

				/* Label the Current values */
				Term_putstr(10, linec, -1, TERM_WHITE,
					    format("Current attr/char = %3u / %3u", ca, cc));
				Term_putstr(40, linec, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, linec+1, -1, TERM_WHITE, empty_symbol2); 
				if (use_trptile) Term_putstr (40, linec+2, -1, TERM_WHITE, empty_symbol3); 
				Term_putch(43, linec, ca, cc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, linec++, ca, cc);
				}
				if (use_trptile) linec++;

				/* Prompt */
				Term_putstr(0, linec+2, -1, TERM_WHITE,
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

				byte da = (byte)(f_ptr->d_attr);
				byte dc = (byte)(f_ptr->d_char);
				byte ca = (byte)(f_ptr->x_attr);
				byte cc = (byte)(f_ptr->x_char);

				int linec = (use_trptile ? 22: (use_dbltile ? 21 : 20));

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("Terrain = %d, Name = %-40.40s",
						   f, (f_name + f_ptr->name)));

				/* Label the Default values */
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("Default attr/char = %3u / %3u", da, dc));
				Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol2);
				if (use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol3);

				Term_putch(43, 19, da, dc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, 19, da, dc);
				}

				/* Label the Current values */
				Term_putstr(10, linec, -1, TERM_WHITE,
					    format("Current attr/char = %3u / %3u", ca, cc));
				Term_putstr(40, linec, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, linec+1, -1, TERM_WHITE, empty_symbol2); 
				if (use_trptile) Term_putstr (40, linec+2, -1, TERM_WHITE, empty_symbol3); 
				Term_putch(43, linec, ca, cc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, linec++, ca, cc);
				}
				if (use_trptile) linec++;

				/* Prompt */
				Term_putstr(0, linec+2, -1, TERM_WHITE,
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
				flavor_type *x_ptr = &x_info[f];

				byte da = (byte)(x_ptr->d_attr);
				byte dc = (byte)(x_ptr->d_char);
				byte ca = (byte)(x_ptr->x_attr);
				byte cc = (byte)(x_ptr->x_char);

				int linec = (use_trptile ? 22: (use_dbltile ? 21 : 20));

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					    format("Flavor = %d, Text = %-40.40s",
						   f, (x_text + x_ptr->text)));

				/* Label the Default values */
				Term_putstr(10, 19, -1, TERM_WHITE,
					    format("Default attr/char = %3u / %3u", da, dc));
				Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol2);
				if (use_trptile) Term_putstr (40, 20, -1, TERM_WHITE, empty_symbol3);

				Term_putch(43, 19, da, dc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, 19, da, dc);
				}

				/* Label the Current values */
				Term_putstr(10, linec, -1, TERM_WHITE,
					    format("Current attr/char = %3u / %3u", ca, cc));
				Term_putstr(40, linec, -1, TERM_WHITE, empty_symbol);
				if (use_dbltile || use_trptile) Term_putstr (40, linec+1, -1, TERM_WHITE, empty_symbol2); 
				if (use_trptile) Term_putstr (40, linec+2, -1, TERM_WHITE, empty_symbol3); 
				Term_putch(43, linec, ca, cc);

				if (use_bigtile || use_dbltile || use_trptile)
				{
					big_putch(43, linec++, ca, cc);
				}
				if (use_trptile) linec++;

				/* Prompt */
				Term_putstr(0, linec+2, -1, TERM_WHITE,
					    "Command (n/N/a/A/c/C): ");

				/* Get a command */
				cx = inkey();

				/* All done */
				if (cx == ESCAPE) break;

				/* Analyze */
				if (cx == 'n') f = (f + z_info->x_max + 1) % z_info->x_max;
				if (cx == 'N') f = (f + z_info->x_max - 1) % z_info->x_max;
				if (cx == 'a') x_info[f].x_attr = (byte)(ca + 1);
				if (cx == 'A') x_info[f].x_attr = (byte)(ca - 1);
				if (cx == 'c') x_info[f].x_char = (byte)(cc + 1);
				if (cx == 'C') x_info[f].x_char = (byte)(cc - 1);
			}
		}

#endif /* ALLOW_VISUALS */

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
 * Interact with "colors"
 */
void do_cmd_colors(void)
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
		prt("Interact with Colors", 2, 0);

		/* Give some choices */
		prt("(1) Load a user pref file", 4, 5);
#ifdef ALLOW_COLORS
		prt("(2) Dump colors", 5, 5);
		prt("(3) Modify colors", 6, 5);
#endif /* ALLOW_COLORS */

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

#ifdef ALLOW_COLORS

		/* Dump colors */
		else if (ch == '2')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump colors", 8, 0);

			/* Prompt */
			prt("File: ", 10, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Failure */
			if (!fff) continue;


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

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped color redefinitions.");
		}

		/* Edit colors */
		else if (ch == '3')
		{
			static byte a = 0;

			/* Prompt */
			prt("Command: Modify colors", 8, 0);

			/* Hack -- query until done */
			while (1)
			{
				cptr name;

				/* Clear */
				clear_from(10);

				/* Exhibit the normal colors */
				for (i = 0; i < 16; i++)
				{
					/* Exhibit this color */
					Term_putstr(i*4, 20, -1, a, "###");

					/* Exhibit all colors */
					Term_putstr(i*4, 22, -1, (byte)i, format("%3d", i));
				}

				/* Describe the color */
				name = ((a < 16) ? color_names[a] : "undefined");

				/* Describe the color */
				Term_putstr(5, 10, -1, TERM_WHITE,
					    format("Color = %d, Name = %s", a, name));

				/* Label the Current values */
				Term_putstr(5, 12, -1, TERM_WHITE,
					    format("K = 0x%02x / R,G,B = 0x%02x,0x%02x,0x%02x",
						   angband_color_table[a][0],
						   angband_color_table[a][1],
						   angband_color_table[a][2],
						   angband_color_table[a][3]));

				/* Prompt */
				Term_putstr(0, 14, -1, TERM_WHITE,
					    "Command (n/N/k/K/r/R/g/G/b/B): ");

				/* Get a command */
				cx = inkey();

				/* All done */
				if (cx == ESCAPE) break;

				/* Analyze */
				if (cx == 'n') a = (byte)(a + 1);
				if (cx == 'N') a = (byte)(a - 1);
				if (cx == 'k') angband_color_table[a][0] = (byte)(angband_color_table[a][0] + 1);
				if (cx == 'K') angband_color_table[a][0] = (byte)(angband_color_table[a][0] - 1);
				if (cx == 'r') angband_color_table[a][1] = (byte)(angband_color_table[a][1] + 1);
				if (cx == 'R') angband_color_table[a][1] = (byte)(angband_color_table[a][1] - 1);
				if (cx == 'g') angband_color_table[a][2] = (byte)(angband_color_table[a][2] + 1);
				if (cx == 'G') angband_color_table[a][2] = (byte)(angband_color_table[a][2] - 1);
				if (cx == 'b') angband_color_table[a][3] = (byte)(angband_color_table[a][3] + 1);
				if (cx == 'B') angband_color_table[a][3] = (byte)(angband_color_table[a][3] - 1);

				/* Hack -- react to changes */
				Term_xtra(TERM_XTRA_REACT, 0);

				/* Hack -- redraw */
				Term_redraw();
			}
		}

#endif /* ALLOW_COLORS */

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
 * Hack -- append all current auto-inscriptions to the given file
 */
static errr autos_dump(cptr fname)
{
	int i;

	FILE *fff;

	char buf[1024];


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);

	/* Skip some lines */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic auto-inscription dump\n\n");

	/* Dump them */
	for (i = 0; i < z_info->k_max; i++)
	{
		if (k_info[i].note)
		{

			/* Start the macro */
			fprintf(fff, "# Kind '%s'\n\n", k_name + k_info[i].name);

			/* Dump the kind */
			fprintf(fff, "I:K:%d:%s\n", i, quark_str(k_info[i].note));

			/* End the inscription */
			fprintf(fff, "\n\n");

		}
	}

	/* Dump them */
	for (i = 0; i < z_info->e_max; i++)
	{
		if (e_info[i].note)
		{
			/* Start the macro */
			fprintf(fff, "# Ego item '%s'\n\n", e_name + e_info[i].name);

			/* Dump the kind */
			fprintf(fff, "I:E:%d:%s\n", i, quark_str(e_info[i].note));

			/* End the inscription */
			fprintf(fff, "\n\n");
		}
	}

#if 0
	/* Dump them */
	for (i = 0; i < z_info->r_max; i++)
	{
		if (r_info[i].note)
		{

			/* Start the macro */
			fprintf(fff, "# Monster race '%s'\n\n", r_name + r_info[i].name);

			/* Dump the kind */
			fprintf(fff, "I:R:%d:%s\n", i, quark_str(r_info[i].note));

			/* End the inscription */
			fprintf(fff, "\n\n");
		}
	}
#endif

	/* Start dumping */
	fprintf(fff, "\n\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}

/*
 * Strip an "object kind name" into a buffer
 */
static void strip_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);

	/* If not aware, use flavor */
	if (!k_ptr->aware && k_ptr->flavor) str = x_text + x_info[k_ptr->flavor].text;

	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&') || (*str == '#')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
		if (prefix(str,"# ")) str++; /* Skip following space */
		else if (*str != '~') *t++ = *str;
	}

	/* Terminate the new name */
	*t = '\0';
}

/*
 * Note something in the message recall
 */
void do_cmd_note(void)
{
	char tmp[80];

	/* Default */
	strcpy(tmp, "");

	/* Input */
	if (!get_string("Note: ", tmp, 80)) return;

	/* Ignore empty notes */
	if (!tmp[0] || (tmp[0] == ' ')) return;

	/* Add the note to the message recall */
	msg_format("Note: %s", tmp);
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
static cptr do_cmd_feeling_text[11] =
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
	/* Verify the feeling */
	if (feeling > 10) feeling = 10;

	/* No useful feeling in town */
	if (!p_ptr->depth)
	{
		msg_print("Looks like a typical town.");
		return;
	}

	/* Display the feeling */
	msg_print(do_cmd_feeling_text[feeling]);
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
	path_build(buf, 1024, ANGBAND_DIR_USER, "dump.txt");

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


/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
	int y, x;

	byte a = 0;
	char c = ' ';

	FILE *fff;

	char buf[1024];


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, "dump.txt");

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff) return;


	/* Save screen */
	screen_save();


	/* Dump the screen */
	for (y = 0; y < 24; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Skip a line */
	fprintf(fff, "\n");


	/* Dump the screen */
	for (y = 0; y < 24; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = hack[a&0x0F];
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Skip a line */
	fprintf(fff, "\n");


	/* Close it */
	my_fclose(fff);


	/* Message */
	msg_print("Screen dump saved.");
	message_flush();


	/* Load screen */
	screen_load();
}

/*
 * Hack -- save a screen dump to a file in html format
 */
void do_cmd_save_screen_html(void)
{
	int i;

	FILE *fff;

	char file_name[1024];


	/* Build the filename */
	path_build(file_name, 1024, ANGBAND_DIR_USER, "dump.prf");

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(file_name, "w");

	/* Oops */
	if (!fff) return;


	/* Extract default attr/char code for features */
	for (i = 0; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];

		if (!f_ptr->name) continue;

		/* Dump a comment */
		fprintf(fff, "# %s\n", (f_name + f_ptr->name));

		/* Dump the attr/char info */
		fprintf(fff, "F:%d:0x%02X:0x%02X:%s\n\n", i,
			(byte)(f_ptr->x_attr), (byte)(f_ptr->x_char), (f_ptr->flags2 & (FF2_ATTR_LITE) ? "YES" : "NO"));

		/* Assume we will use the underlying values */
		f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char = f_ptr->d_char;
	}

	/* Extract default attr/char code for objects */
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		if (!k_ptr->name) continue;

		/* Dump a comment */
		fprintf(fff, "# %s\n", (k_name + k_ptr->name));

		/* Dump the attr/char info */
		fprintf(fff, "K:%d:0x%02X:0x%02X\n\n", i,
			(byte)(k_ptr->x_attr), (byte)(k_ptr->x_char));

		/* Default attr/char */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract default attr/char code for flavors */
	for (i = 0; i < z_info->x_max; i++)
	{
		flavor_type *x_ptr = &x_info[i];

		if (!x_ptr->name) continue;

		/* Dump a comment */
		fprintf(fff, "# %s\n", (x_name + x_ptr->name));

		/* Dump the attr/char info */
		fprintf(fff, "L:%d:0x%02X:0x%02X\n\n", i,
			(byte)(x_ptr->x_attr), (byte)(x_ptr->x_char));

		/* Default attr/char */
		x_ptr->x_attr = x_ptr->d_attr;
		x_ptr->x_char = x_ptr->d_char;
	}


	/* Extract default attr/char code for monsters */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		if (!r_ptr->name) continue;

		/* Dump a comment */
		fprintf(fff, "# %s\n", (r_name + r_ptr->name));

		/* Dump the attr/char info */
		fprintf(fff, "R:%d:0x%02X:0x%02X\n\n", i,
			(byte)(r_ptr->x_attr), (byte)(r_ptr->x_char));

		/* Default attr/char */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;
	}

	/* Skip a line */
	fprintf(fff, "\n");


	/* Close it */
	my_fclose(fff);

	/* Refresh */
	do_cmd_redraw();

	/* Hack -- dump the graphics before loading font/graphics */
	dump_html();

	/* Graphic symbols */
	if (use_graphics)
	{
		/* Process "graf.prf" */
		process_pref_file("graf.prf");
	}

	/* Normal symbols */
	else
	{
		/* Process "font.prf" */
		process_pref_file("font.prf");
	}

	/* Process "dump.prf" */
	process_pref_file("dump.prf");


#ifdef ALLOW_BORG_GRAPHICS
	/* Initialize the translation table for the borg */
	init_translate_visuals();
#endif /* ALLOW_BORG_GRAPHICS */

	/* Message */
	msg_print("Html screen dump saved.");
	message_flush();

	/* Refresh */
	do_cmd_redraw();

	/* Delete the file */
	fd_kill(file_name);
}


/*
 * Description of each monster group.
 */
static cptr monster_group_text[] = 
{
	"Uniques",
	"Amphibians/Fish",
	"Ants",
	"Bats",
	"Birds",
	"Canines",
	"Centipedes",
	"Demons",
	"Dragons",
	"Elementals",
	"Elves",
	"Eyes/Beholders",
	"Felines",
	"Ghosts",
	"Giants",
	"Goblins",
	"Golems",
	"Harpies/Hybrids",
	"Hobbits/Dwarves",
	"Hydras",
	"Icky Things",
	"Insects",
	"Jellies",
	"Killer Beetles",
	"Lichs",
	"Maiar",
	"Mimics",
	"Molds",
	"Mushroom Patches",
	"Nagas",
	"Nightsbane",
	"Ogres",
	"Orcs",
	"People",
	"Quadrapeds",
	"Reptiles",
	"Rodents",
	"Scorpions/Spiders",
	"Skeletons",
	"Snakes",
	"Trolls",
	"Vampires",
	"Vortices",
	"Wights/Wraiths",
	"Worms/Worm Masses",
	"Xorns/Xarens",
	"Yeti",
	"Zephyr Hounds",
	"Zombies",
	NULL
};

/*
 * Symbols of monsters in each group. Note the "Uniques" group
 * is handled differently.
 */
static cptr monster_group_char[] = 
{
	(char *) -1L,
	"F",
	"a",
	"b",
	"B",
	"C",
	"c",
	"Uu",
	"ADd",
	"E",
	"l",
	"e",
	"f",
	"G",
	"P",
	"k",
	"g",
	"H",
	"h",
	"y",
	"i",
	"I",
	"j",
	"K",
	"L",
	"M",
	"$!?=.|~[]",
	"m",
	",",
	"n",
	"N",
	"O",
	"o",
	"pqt",
	"Q",
	"R",
	"r",
	"S",
	"s",
	"J",
	"T",
	"V",
	"v",
	"W",
	"w",
	"X",
	"Y",
	"Z",
	"z",
	NULL
};

/*
 * Build a list of monster indexes in the given group. Return the number
 * of monsters in the group.
 */
static int collect_monsters(int grp_cur, int *mon_idx, int mode)
{
	int i, mon_cnt = 0;

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
		bool unique = r_ptr->flags1 & (RF1_UNIQUE);

		/* Skip empty race */
		if (!r_ptr->name) continue;

		if (grp_unique && !(unique)) continue;

		/* Require known monsters */
		if (!(mode & 0x02) && !cheat_know && !(l_ptr->sights)) continue;

		/* Check for race in the group */
		if (grp_unique || strchr(group_char, r_ptr->d_char))
		{
			int idx, ii, tmp;

			for (ii = 0, idx = i;ii < mon_cnt; ii++)
			{
				if (strcmp(r_name + r_info[idx].name, r_name + r_info[mon_idx[ii]].name) <= 0)
				{
					tmp = idx;
					idx = mon_idx[ii];
					mon_idx[ii] = tmp;
				}

			}

			/* Add the race */
			mon_idx[mon_cnt++] = idx;

			/* XXX Hack -- Just checking for non-empty group */
			if (mode & 0x01) break;
		}
	}

	/* Terminate the list */
	mon_idx[mon_cnt] = 0;

	/* Return the number of races */
	return mon_cnt;
}


/*
 * Build a list of ego item indexes in the given group. Return the number
 * of ego items in the group.
 */
static int collect_ego_items(int grp_cur, int object_idx[])
{
	int i, j, k, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval = object_group_tval[grp_cur];

	/* Check every object */
	for (i = 0; i < z_info->e_max; i++)
	{
		/* Access the race */
		ego_item_type *e_ptr = &e_info[i];

		/* Skip empty objects */
		if (!e_ptr->name) continue;

		/* Require objects ever seen*/
		if (!(cheat_lore) && !(e_ptr->aware)) continue;

		/* Test if this is a legal ego-item type for this object */
		for (j = 0, k = 0; j < 3; j++) if (group_tval == e_ptr->tval[j]) k++;

		/* Check for race in the group */
		if (k)
		{
			int idx, ii, tmp;

			for (ii = 0, idx = i;ii < object_cnt; ii++)
			{
				/* XXX Need to remove leading ' of' or single-quote */

				if (strcmp(e_name + e_info[idx].name, e_name + e_info[object_idx[ii]].name) <= 0)
				{
					tmp = idx;
					idx = object_idx[ii];
					object_idx[ii] = tmp;
				}

			}

			/* Add the race */
			object_idx[object_cnt++] = idx;

		}

	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Return the number of races */
	return object_cnt;
}



/*
 * Build a list of object indexes in the given group. Return the number
 * of objects in the group.
 */
static int collect_objects(int grp_cur, int object_idx[], int mode)
{
	int i, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval = object_group_tval[grp_cur];

	/* Check every object */
	for (i = 0; i < z_info->k_max; i++)
	{
		/* Access the race */
		object_kind *k_ptr = &k_info[i];

		/* Skip empty objects */
		if (!k_ptr->name) continue;

		/* Skip special arts */
                if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

		/* Require objects ever seen*/
		if (!(mode & 0x02) && !(cheat_lore) && !(k_ptr->aware) && !(k_ptr->flavor)) continue;

		/* Check for race in the group */
		if (k_ptr->tval == group_tval)
		{
			int idx, ii, tmp;

			char name1[80];
			char name2[80];

			object_kind *i_ptr;

			for (ii = 0, idx = i;ii < object_cnt; ii++)
			{
				/* Get the object */
				i_ptr = &k_info[object_idx[ii]];

				/* If flavoured, list known items first */
				if (!k_ptr->aware && k_ptr->flavor && i_ptr->aware) continue;

				strip_name(name1,idx);
				strip_name(name2,object_idx[ii]);

				if ((k_ptr->aware && !i_ptr->aware && i_ptr->flavor) || (strcmp(name1,name2) <= 0))
				{
					tmp = idx;
					idx = object_idx[ii];
					object_idx[ii] = tmp;
				}
			}

			/* Add the race */
			object_idx[object_cnt++] = idx;
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Return the number of races */
	return object_cnt;
}

/*
 * Build a list of artifact indexes in the given group. Return the number
 * of artifacts in the group.
 */
static int collect_artifacts(int grp_cur, int object_idx[])
{
	int i, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval = object_group_tval[grp_cur];

	int y, x;

	bool *okay;

	/* Allocate the "okay" array */
	C_MAKE(okay, z_info->a_max, bool);

	/* Scan the artifacts */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Default */
		okay[i] = FALSE;

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Skip "uncreated" artifacts */
		if (!(cheat_lore) && !a_ptr->cur_num) continue;

		/* Assume okay */
		okay[i] = TRUE;
	}

	/* Check the dungeon */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			s16b this_o_idx, next_o_idx = 0;

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Ignore non-artifacts */
				if (!artifact_p(o_ptr)) continue;

				/* Ignore known items */
				if ((cheat_lore) || object_known_p(o_ptr)) continue;

				/* Note the artifact */
				okay[o_ptr->name1] = FALSE;
			}
		}
	}

	/* Check the inventory and equipment */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Ignore non-objects */
		if (!o_ptr->k_idx) continue;

		/* Ignore non-artifacts */
		if (!artifact_p(o_ptr)) continue;

		/* Ignore known items */
		if ((cheat_lore) || object_known_p(o_ptr)) continue;

		/* Note the artifact */
		okay[o_ptr->name1] = FALSE;
	}

	/* Scan the artifacts */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* List "dead" ones */
		if (!okay[i]) continue;

		/* Check for race in the group */
		if (a_ptr->tval == group_tval)
		{
			int idx, ii, tmp;

			for (ii = 0, idx = i;ii < object_cnt; ii++)
			{
				/* XXX Need to remove leading ' of' or single-quote */

				if (strcmp(a_name + a_info[idx].name, a_name + a_info[object_idx[ii]].name) <= 0)
				{
					tmp = idx;
					idx = object_idx[ii];
					object_idx[ii] = tmp;
				}

			}

			/* Add the race */
			object_idx[object_cnt++] = idx;
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Free the "okay" array */
	FREE(okay);

	/* Return the number of races */
	return object_cnt;
}

/*
 * Display the object groups.
 */
static void display_group_list(int col, int row, int wid, int per_page,
	const int grp_idx[], const cptr group_text[], int grp_cur, int grp_top)
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
		int browser_rows;
		int wid, hgt;

		/* Get size */
		Term_get_size(&wid, &hgt);

		browser_rows = hgt - 8;

		/* Browse group list */
		if (!col)
		{
			int old_grp = grp;

			/* Move up or down */
			grp += ddy[d] * browser_rows;

			/* Verify */
			if (grp >= grp_cnt)	grp = grp_cnt - 1;
			if (grp < 0) grp = 0;
			if (grp != old_grp)	list = 0;
		}

		/* Browse sub-list list */
		else
		{
			/* Move up or down */
			list += ddy[d] * browser_rows;

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
		if (grp < 0) grp = 0;
		if (grp >= grp_cnt)	grp = grp_cnt - 1;
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
 * Display the objects in a group.
 */
static void display_artifact_list(int col, int row, int per_page, int object_idx[],
	int object_cur, int object_top)
{
	int i;
	char o_name[80];
	object_type *o_ptr;
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
		o_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(o_ptr);

		/* Make fake artifact */
		make_fake_artifact(o_ptr, a_idx);

		/* Get its name */
		object_desc_spoil(o_name, sizeof(o_name), o_ptr, TRUE, 0);

		/* Display the name */
		c_prt(attr, o_name, row + i, col);

		if (p_ptr->wizard) 
		{
			c_prt(attr, format("%d", a_idx), row + i, 60);
		}

	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}

/*
 * Describe fake artifact 
 */
static void desc_art_fake(int a_idx)
{
	object_type *o_ptr;
	object_type object_type_body;

	/* Get local object */
	o_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Make fake artifact */
	make_fake_artifact(o_ptr, a_idx);

	/* Track the object */
	object_actual_track(o_ptr);

	/* Hack - mark as fake */
	term_obj_real = FALSE;

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save the screen */
	screen_save();

	/* Describe */
	screen_object(o_ptr);

	/* Load the screen */
	screen_load();

	(void)inkey();
}

/*
 * Display known objects
 */
static void do_cmd_knowledge_artifacts(void)
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

	int browser_rows;
	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

	/* Allocate the "object_idx" array */
	C_MAKE(object_idx, z_info->a_max, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; object_group_text[i] != NULL; i++)
	{
		/* Measure the label */
		len = strlen(object_group_text[i]);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any artifacts are known */
		if (collect_artifacts(i, object_idx))
		{
			/* Build a list of groups with known artifacts */
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

	while ((!flag) && (grp_cnt))
	{
		char ch;

		if (redraw)
		{
			clear_from(0);
		
			prt("Knowledge - artifacts", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < browser_rows; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

		/* Scroll artifact list */
		if (object_cur < object_top) object_top = object_cur;
		if (object_cur >= object_top + browser_rows) object_top = object_cur - browser_rows + 1;

		/* Display a list of object groups */
		display_group_list(0, 6, max, browser_rows, grp_idx, object_group_text, grp_cur, grp_top);

		/* Get a list of objects in the current group */
		object_cnt = collect_artifacts(grp_idx[grp_cur], object_idx);

		/* Display a list of objects in the current group */
		display_artifact_list(max + 3, 6, browser_rows, object_idx, object_cur, object_top);

		/* Prompt */
		prt("<dir>, ENTER to recall, ESC", hgt - 1, 0);

		/* Mega Hack -- track this artifact race */
		if (object_cnt) artifact_track(object_idx[object_cur]);

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
				desc_art_fake(object_idx[object_cur]);

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
	/* Prompt */
	if (!grp_cnt) msg_print("No artifacts known.");

	/* XXX XXX Free the "object_idx" array */
	FREE(object_idx);
}

/*
 * Display visuals.
 */
static void display_visual_list(int col, int row, int height, int width, byte attr_top, char char_left)
{
	int i, j;

	/* Clear the display lines */
	for (i = 0; i < height; i++)
	{
		Term_erase(col, row + i, width);
	}

	/* Triple tile uses double height and width width */
	if (use_trptile)
	{
		width /= (use_bigtile ? 6 : 3);
		height /= 3;
	}
	/* Double tile uses double height and width width */
	else if (use_dbltile)
	{
		width /= (use_bigtile ? 4 : 2);
		height /= 2;
	}

	/* Bigtile mode uses double width */
	else if (use_bigtile) width /= 2;

	/* Display lines until done */
	for (i = 0; i < height; i++)
	{
		/* Display columns until done */
		for (j = 0; j < width; j++)
		{
			byte a;
			char c;
			int x = col + j;
			int y = row + i;
			int ia, ic;

			/* Triple tile mode uses double width and double height */
			if (use_trptile)
			{
				y += 2 * i;
				x += (use_bigtile ? 5 : 2) * j;
			}
			/* Double tile mode uses double width and double height */
			else if (use_dbltile)
			{
				y += i;
				x += (use_bigtile ? 3 : 1) * j;
			}
			/* Bigtile mode uses double width */
			else if (use_bigtile) x += j;

			ia = attr_top + i;
			ic = char_left + j;

			a = (byte)ia;
			c = (char)ic;

			/* Display symbol */
			Term_putch(x, y, a, c);

			if (use_bigtile || use_dbltile || use_trptile)
			{
				big_putch(x, y, a, c);
			}
		}
	}
}


/*
 * Place the cursor at the collect position for visual mode
 */
static void place_visual_list_cursor(int col, int row, byte a, byte c, byte attr_top, byte char_left)
{
	int i = a - attr_top;
	int j = c - char_left;

	int x = col + j;
	int y = row + i;

	/* Triple tile mode uses double height and width */
	if (use_trptile)
	{
		y += 2 * i;
		x += (use_bigtile ? 5 : 2) * j;
	}
	/* Double tile mode uses double height and width */
	else if (use_dbltile)
	{
		y += i;
		x += (use_bigtile ? 3 : 1) * j;
	}
	/* Bigtile mode uses double width */
	else if (use_bigtile) x += j;

	/* Place the cursor */
	Term_gotoxy(x, y);
}


/*
 *  Clipboard variables for copy&paste in visual mode
 */
static byte attr_idx = 0;
static byte char_idx = 0;

/*
 *  Do visual mode command -- Change symbols
 */
static bool visual_mode_command(char ch, bool *visual_list_ptr, 
				int height, int width, 
				byte *attr_top_ptr, char *char_left_ptr, 
				byte *cur_attr_ptr, char *cur_char_ptr)
{
	static byte attr_old = 0;
	static char char_old = 0;

	int frame_left = 10;
	int frame_right = 10;
	int frame_top = 4;
	int frame_bottom = 4;

	if (use_trptile) frame_left /= (use_bigtile ? 6 : 3);
	else if (use_dbltile) frame_left /= (use_bigtile ? 4 : 2);
	else if (use_bigtile) frame_left /= 2;

	if (use_trptile) frame_right /= (use_bigtile ? 6 : 3);
	else if (use_dbltile) frame_right /= (use_bigtile ? 4 : 2);
	else if (use_bigtile) frame_right /= 2;

	if (use_trptile) frame_top /= 3;
	else if (use_dbltile) frame_top /= 2;

	if (use_trptile) frame_bottom /= 3;
	else if (use_dbltile) frame_bottom /= 2;

	switch (ch)
	{
	case ESCAPE:
		if (*visual_list_ptr)
		{
			/* Cancel change */
			*cur_attr_ptr = attr_old;
			*cur_char_ptr = char_old;
			*visual_list_ptr = FALSE;

			return TRUE;
		}

		break;

	case '\n':
	case '\r':
		if (*visual_list_ptr)
		{
			/* Accept change */
			*visual_list_ptr = FALSE;

			return TRUE;
		}
		break;

	case 'V':
	case 'v':
		if (!*visual_list_ptr)
		{
			*visual_list_ptr = TRUE;

			*attr_top_ptr = (byte)MAX(0, (int)*cur_attr_ptr - frame_top);
			*char_left_ptr = (char)MAX(-128, (int)*cur_char_ptr - frame_left);

			attr_old = *cur_attr_ptr;
			char_old = *cur_char_ptr;

			return TRUE;
		}
		break;

	case 'C':
	case 'c':
		/* Set the visual */
		attr_idx = *cur_attr_ptr;
		char_idx = *cur_char_ptr;

		return TRUE;

	case 'P':
	case 'p':
		if (attr_idx)
		{
			/* Set the char */
			*cur_attr_ptr = attr_idx;
			*attr_top_ptr = (byte)MAX(0, (int)*cur_attr_ptr - frame_top);
		}

		if (char_idx)
		{
			/* Set the char */
			*cur_char_ptr = char_idx;
			*char_left_ptr = (char)MAX(-128, (int)*cur_char_ptr - frame_left);
		}

		return TRUE;

	default:
		if (*visual_list_ptr)
		{
			int eff_width, eff_height;
			int d = target_dir(ch);
			byte a = *cur_attr_ptr;
			char c = *cur_char_ptr;

			if (use_trptile) eff_width = width / (use_bigtile ? 6 : 3);
			else if (use_dbltile) eff_width = width / (use_bigtile ? 4 : 2);
			else if (use_bigtile) eff_width = width / 2;
			else eff_width = width;

			if (use_trptile) eff_height = height / 3;
			else if (use_dbltile) eff_height = height / 2;
			else eff_height = height;
					
			/* Restrict direction */
			if ((a == 0) && (ddy[d] < 0)) d = 0;
			if ((c == -128) && (ddx[d] < 0)) d = 0;
			if ((a == 255) && (ddy[d] > 0)) d = 0;
			if ((c == 127) && (ddx[d] > 0)) d = 0;

			a += ddy[d];
			c += ddx[d];

			/* Set the visual */
			*cur_attr_ptr = a;
			*cur_char_ptr = c;


			/* Move the frame */
			if ((ddx[d] < 0) && *char_left_ptr > MAX(-128, (int)c - frame_left)) (*char_left_ptr)--;
			if ((ddx[d] > 0) && *char_left_ptr + eff_width < MIN(127, (int)c + frame_right)) (*char_left_ptr)++;
			if ((ddy[d] < 0) && *attr_top_ptr > MAX(0, (int)a - frame_top)) (*attr_top_ptr)--;
			if ((ddy[d] > 0) && *attr_top_ptr + eff_height < MIN(255, (int)a + frame_bottom)) (*attr_top_ptr)++;
			return TRUE;
		}
				
		break;
	}

	/* Visual mode command is not used */
	return FALSE;
}


/*
 * Display the monsters in a group.
 */
static void display_monster_list(int col, int row, int per_page, int *mon_idx,
	int mon_cur, int mon_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && mon_idx[i]; i++)
	{
		byte attr;
		byte a;
		char c;

		/* Get the race index */
		int r_idx = mon_idx[mon_top + i];

		/* Access the race */
		monster_race *r_ptr = &r_info[r_idx];
		monster_lore *l_ptr = &l_list[r_idx];

		/* Choose a color */
		attr = ((i + mon_top == mon_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Display the name */
		c_prt(attr, r_name + r_ptr->name, row + i, col);

		/* Hack -- visual_list mode */
		if (per_page == 1)
		{
			c_prt(attr, format(r_ptr->x_char >= 0 ? "%02x/%02x" : "%02x/%d", r_ptr->x_attr, r_ptr->x_char), row + i, 60);
		}
		else if (p_ptr->wizard) 
		{
			c_prt(attr, format("%d", r_idx), row + i, 60);
		}

		if (!use_dbltile && !use_trptile)
		{
			a = r_ptr->x_attr;
			c = r_ptr->x_char;

			/* Display symbol */
			Term_putch(70, row + i, a, c);

			if (use_bigtile)
			{
				if (a & 0x80)
					Term_putch(71, row + i, 255, -1);
				else
					Term_putch(71, row + i, TERM_WHITE, ' ');
			}
		}

		/* Display kills */
		if (r_ptr->flags1 & (RF1_UNIQUE)) put_str(format("%s", (r_ptr->max_num == 0)? "dead" : "alive"), row + i, 73);
		else put_str(format("%5d", l_ptr->pkills), row + i, 73);
	
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}


/*
 * Display known monsters.
 */
static void do_cmd_knowledge_monsters(void)
{
	int i, len, max;
	int grp_cur, grp_top, old_grp_cur;
	int mon_cur, mon_top;
	int grp_cnt, grp_idx[100];
	int mon_cnt;
	int *mon_idx;
	
	int column = 0;
	bool flag;
	bool redraw;

	bool visual_list = FALSE;
	byte attr_top = 0;
	char char_left = 0;

	int browser_rows;
	int wid, hgt;

	monster_race *r_ptr;

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

	/* Allocate the "mon_idx" array */
	C_MAKE(mon_idx, z_info->r_max, int);

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

	old_grp_cur = -1;
	grp_cur = grp_top = 0;
	mon_cur = mon_top = 0;
	mon_cnt = 0;

	flag = FALSE;
	redraw = TRUE;

	while ((!flag) && (grp_cnt))
	{
		char ch;

		if (redraw)
		{
			clear_from(0);
		
			prt("Knowledge - Monsters", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			prt("Sym   Kills", 4, 67);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < browser_rows; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

		/* Display a list of monster groups */
		display_group_list(0, 6, max, browser_rows, grp_idx, monster_group_text, grp_cur, grp_top);

		if (old_grp_cur != grp_cur)
		{
			old_grp_cur = grp_cur;

			/* Get a list of monsters in the current group */
			mon_cnt = collect_monsters(grp_idx[grp_cur], mon_idx, 0x00);
		}

		/* Scroll monster list */
		while (mon_cur < mon_top)
			mon_top = MAX(0, mon_top - browser_rows/2);
		while (mon_cur >= mon_top + browser_rows)
			mon_top = MIN(mon_cnt - browser_rows, mon_top + browser_rows/2);

		if (!visual_list)
		{
			/* Display a list of monsters in the current group */
			display_monster_list(max + 3, 6, browser_rows, mon_idx, mon_cur, mon_top);
		}
		else
		{
			mon_top = mon_cur;

			/* Display a monster name */
			display_monster_list(max + 3, 6, 1, mon_idx, mon_cur, mon_top);

			/* Display visual list below first monster */
			display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
		}

		/* Prompt */
		prt(format("<dir>, 'r' to recall%s%s, ESC",
			visual_list ? ", ENTER to accept" : ", 'v' for visuals",
			(attr_idx||char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy"), hgt - 1, 0);

		/* Get the current monster */
		r_ptr = &r_info[mon_idx[mon_cur]];

		/* Get the current monster */
		r_ptr = &r_info[mon_idx[mon_cur]];

		/* Mega Hack -- track this monster race */
		if (mon_cnt) monster_race_track(mon_idx[mon_cur]);

		/* Track this monster race */
		p_ptr->window |= (PW_MONSTER);

		/* Hack -- handle stuff */
		handle_stuff();

		if (visual_list)
		{
			place_visual_list_cursor(max + 3, 7, r_ptr->x_attr, r_ptr->x_char, attr_top, char_left);
		}
		else if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (mon_cur - mon_top));
		}
	
		ch = inkey();

		/* Do visual mode command if needed */
		if (visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, &r_ptr->x_attr, &r_ptr->x_char)) continue;

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
				if (mon_idx[mon_cur])
				{
					screen_roff(mon_idx[mon_cur]);

					(void)inkey();
	
					redraw = TRUE;
				}
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &mon_cur, mon_cnt);

				break;
			}
		}
	}

	/* Prompt */
	if (!grp_cnt) msg_print("No monsters known.");

	/* XXX XXX Free the "mon_idx" array */
	FREE(mon_idx);
}

/*
 * Display the objects in a group.
 */
static void display_ego_item_list(int col, int row, int per_page, int object_idx[],
	int object_cur, int object_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && object_idx[i]; i++)
	{
		/* Get the object index */
		int e_idx = object_idx[object_top + i];

		/* Access the object */
		ego_item_type *e_ptr = &e_info[e_idx];

		/* Choose a color */
		byte attr = ((e_ptr->aware) ? TERM_WHITE : TERM_SLATE);
		byte cursor = ((e_ptr->aware) ? TERM_L_BLUE : TERM_BLUE);

		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Display the name */
		c_prt(attr, e_name + e_ptr->name, row + i, col);

		if (p_ptr->wizard) 
		{
			c_prt(attr, format("%d", e_idx), row + i, 60);
		}

		if (e_ptr->note)
		{
			c_prt(TERM_YELLOW,quark_str(e_ptr->note), row+i, 65);
		}
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}

/*
 * Describe fake ego item
 */
static void desc_ego_fake(int e_idx)
{
	object_lore *n_ptr = &e_list[e_idx];

	/* Save screen */
	screen_save();

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* List can flags */
	list_object_flags(n_ptr->can_flags1,n_ptr->can_flags2,n_ptr->can_flags3,1);

	/* List may flags */
	list_object_flags(n_ptr->may_flags1,n_ptr->may_flags2,n_ptr->may_flags3,2);

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Dump the name */
	Term_addstr(-1, TERM_L_BLUE, format("Ego Item %s",e_name+e_info[e_idx].name));

	inkey();

	screen_load();
}


/*
 * Create a copy of an existing quark, except if the quark has '=x' in it, 
 * If an quark has '=x' in it remove it from the copied string, otherwise append it where 'x' is ch.
 * Return the new quark location.
 */
static int auto_note_modify(int note, char ch)
{
	char tmp[80];

	cptr s;

	/* Paranoia */
	if (!ch) return(note);

	/* Null length string to start */
	tmp[0] = '\0';

	/* Inscription */
	if (note)
	{

		/* Get the inscription */
		s = quark_str(note);

		/* Temporary copy */
		my_strcpy(tmp,s,80);

		/* Process inscription */
		while (s)
		{

			/* Auto-pickup on "=g" */
			if (s[1] == ch)
			{

				/* Truncate string */
				tmp[strlen(tmp)-strlen(s)] = '\0';

				/* Overwrite shorter string */
				my_strcat(tmp,s+2,80);
				
				/* Create quark */
				return(quark_add(tmp));
			}

			/* Find another '=' */
			s = strchr(s + 1, '=');
		}
	}

	/* Append note */
	my_strcat(tmp,format("=%c",ch),80);

	/* Create quark */
	return(quark_add(tmp));
}



/*
 * Display known ego_items
 */
static void do_cmd_knowledge_ego_items(void)
{
	int i, len, max;
	int grp_cur, grp_top;
	int object_cur, object_top;
	int grp_cnt, grp_idx[100];
	int object_cnt;
	int *object_idx;

	int column = 0;
	bool flag;
	bool redraw;

	int note_idx = 0;

	int browser_rows;
	int wid, hgt;

	ego_item_type *e_ptr;

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

	/* Allocate the "object_idx" array */
	C_MAKE(object_idx, z_info->e_max, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; object_group_text[i] != NULL; i++)
	{
		/* Measure the label */
		len = strlen(object_group_text[i]);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any ego items are known */
		if (collect_ego_items(i, object_idx))
		{
			/* Build a list of groups with known ego items */
			grp_idx[grp_cnt++] = i;
		}
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	grp_cur = grp_top = 0;
	object_cur = object_top = 0;

	flag = FALSE;
	redraw = TRUE;

	while ((!flag) && (grp_cnt))
	{
		char ch;

		if (redraw)
		{
			clear_from(0);
		
			prt("Knowledge - ego items", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			prt("Inscribed", 4, 65);
			prt("Sym", 4, 75);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < browser_rows; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}		

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

		/* Scroll ego item list */
		if (object_cur < object_top) object_top = object_cur;
		if (object_cur >= object_top + browser_rows) object_top = object_cur - browser_rows + 1;

		/* Display a list of object groups */
		display_group_list(0, 6, max, browser_rows, grp_idx, object_group_text, grp_cur, grp_top);

		/* Get a list of objects in the current group */
		object_cnt = collect_ego_items(grp_idx[grp_cur], object_idx);

		/* Display a list of objects in the current group */
		display_ego_item_list(max + 3, 6, browser_rows, object_idx, object_cur, object_top);

		/* Get the current ego item */
		e_ptr = &e_info[object_idx[object_cur]];

		/* Prompt */
		prt(format("<dir>, 'r' to recall, '{', '}'%s, ESC, 'k', 'g', ...",
			(note_idx) ? ", '\\' to re-inscribe" : (e_ptr->note) ? ", '\\', " : ""), hgt - 1, 0);

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
				desc_ego_fake(object_idx[object_cur]);

				redraw = TRUE;
				break;
			}

			case '{':
			{
				char note_text[80] = "";

				/* Prompt */
				prt("Inscribe with: ", hgt, 0);
	
				/* Default note */
				if (e_ptr->note)
				{
					sprintf(note_text, "%s", quark_str(e_ptr->note));
				}
	
				/* Get a filename */
				if (!askfor_aux(note_text, 80)) continue;
	
				/* Set the inscription */
				e_ptr->note = quark_add(note_text);

				/* Set the note */
				note_idx = e_ptr->note;

				/* Process objects */
				for (i = 1; i < o_max; i++)
				{
					/* Get the object */
					object_type *i_ptr = &o_list[i];
		
					/* Skip dead objects */
					if (!i_ptr->k_idx) continue;
		
					/* Not matching ego item */
					if (i_ptr->name2 != object_idx[object_cur]) continue;
		
					/* Already has note */
					if (i_ptr->note) continue;
		
					/* Auto-inscribe */
					if (object_known_p(i_ptr) || cheat_auto) i_ptr->note = e_ptr->note;

				}
				break;
			}

			case '}':
			{
				/* Set the inscription */
				e_ptr->note = 0;

				/* Process objects */
				for (i = 1; i < o_max; i++)
				{
					/* Get the object */
					object_type *i_ptr = &o_list[i];
		
					/* Skip dead objects */
					if (!i_ptr->k_idx) continue;
		
					/* Not matching ego item */
					if (i_ptr->name2 != object_idx[object_cur]) continue;
		
					/* Auto-inscribe */
					if (object_known_p(i_ptr) || cheat_auto) i_ptr->note = 0;

				}
				break;
			}

			case '\\':
			{
				if (note_idx)
				{
					/* Set the note */
					e_ptr->note = note_idx;
	
					/* Process objects */
					for (i = 1; i < o_max; i++)
					{
						/* Get the object */
						object_type *i_ptr = &o_list[i];
			
						/* Skip dead objects */
						if (!i_ptr->k_idx) continue;
			
						/* Not matching ego item */
						if (i_ptr->name2 != object_idx[object_cur]) continue;
			
						/* Already has note */
						if (i_ptr->note) continue;
			
						/* Auto-inscribe */
						if (object_known_p(i_ptr) || cheat_auto) i_ptr->note = note_idx;
	
					}
				}
				else if (e_ptr->note)
				{
					/* Set the note */
					note_idx = e_ptr->note;
				}
				break;
			}

			default:
			{
				if (target_dir(ch))
				{
					/* Move the cursor */
					browser_cursor(ch, &column, &grp_cur, grp_cnt, &object_cur, object_cnt);
				}
				else
				{
					note_idx = auto_note_modify(e_ptr->note,ch);
					e_ptr->note = note_idx;
				}
				break;
			}
		}
	}

	/* Prompt */
	if (!grp_cnt)  msg_print("No ego items known.");

	/* XXX XXX Free the "object_idx" array */
	FREE(object_idx);
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
		/* Get the object index */
		int k_idx = object_idx[object_top + i];

		/* Access the object */
		object_kind *k_ptr = &k_info[k_idx];

		char o_name[80];

		/* Choose a color */
		byte attr = ((k_ptr->aware) ? TERM_WHITE : TERM_SLATE);
		byte cursor = ((k_ptr->aware) ? TERM_L_BLUE : TERM_BLUE);

		byte a = k_ptr->flavor && (view_flavors || !k_ptr->aware) ? (x_info[k_ptr->flavor].x_attr) : (k_ptr->x_attr);
		char c = k_ptr->flavor && (view_flavors || !k_ptr->aware) ? (x_info[k_ptr->flavor].x_char) : (k_ptr->x_char);
	
		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Symbol is unknown */	
		if (!k_ptr->aware && !k_ptr->flavor && !p_ptr->wizard)
		{
			c = ' ';
			a = TERM_DARK;
		}

		/* Tidy name */
		strip_name(o_name,k_idx);

		/* Display the name */
		c_prt(attr, o_name, row + i, col);

		/* Hack -- visual_list mode */
		if (per_page == 1)
		{
			c_prt(attr, format(c >= 0 ? "%02x/%02x" : "%02x/%d", a, c), row + i, 60);
		}
		else if (p_ptr->wizard) 
		{
			c_prt(attr, format("%d", k_idx), row + i, 60);
		}

		if (k_ptr->note && (k_ptr->aware || !k_ptr->flavor))
		{
			c_prt(TERM_YELLOW,quark_str(k_ptr->note), row+i, 65);
		}

		/* Hack - don't use if double tile */
		if (!use_dbltile && !use_trptile)
		{

			/* Display symbol */
			Term_putch(76, row + i, a, c);

			if (use_bigtile)
			{
				if (a & 0x80)
					Term_putch(76 + 1, row + i, 255, -1);
				else
					Term_putch(76 + 1, row + i, 0, ' ');
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
 * Describe fake object
 */
static void desc_obj_fake(int k_idx)
{
	object_type *o_ptr;
	object_type object_type_body;

	/* Get local object */
	o_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Create the artifact */
	object_prep(o_ptr, k_idx);

	/* Hack -- its in the store */
	o_ptr->ident |= (IDENT_STORE);

	/* It's fully know */
	if (!k_info[k_idx].flavor) object_known(o_ptr);

	/* Track the object */
	object_actual_track(o_ptr);

	/* Hack - mark as fake */
	term_obj_real = FALSE;

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save the screen */
	screen_save();

	/* Describe */
	screen_object(o_ptr);

	/* Load the screen */
	screen_load();

	(void)inkey();
}


/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(void)
{
	int i, len, max;
	int grp_cur, grp_top, old_grp_cur;
	int object_old, object_cur, object_top;
	int grp_cnt, grp_idx[100];
	int object_cnt;
	int *object_idx;

	int column = 0;
	bool flag;
	bool redraw;

	bool visual_list = FALSE;
	byte attr_top = 0;
	char char_left = 0;

	int browser_rows;
	int wid, hgt;

	int note_idx = 0;

	object_kind *k_ptr;

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

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

		/* See if any objects are known */
		if (collect_objects(i, object_idx, 0x01))
		{
			/* Build a list of groups with known objects */
			grp_idx[grp_cnt++] = i;
		}
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	old_grp_cur = -1;
	grp_cur = grp_top = 0;
	object_cur = object_top = 0;
	object_old = -1;
	object_cnt = 0;

	flag = FALSE;
	redraw = TRUE;

	while ((!flag) && (grp_cnt))
	{
		char ch;

		if (redraw)
		{
			clear_from(0);
		
			prt("Knowledge - objects", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			prt("Inscribed", 4, 65);
			prt("Sym", 4, 75);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < browser_rows; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}
		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

		/* Display a list of object groups */
		display_group_list(0, 6, max, browser_rows, grp_idx, object_group_text, grp_cur, grp_top);

		if (old_grp_cur != grp_cur)
		{
			old_grp_cur = grp_cur;

			/* Get a list of objects in the current group */
			object_cnt = collect_objects(grp_idx[grp_cur], object_idx,0x01);
		}

		/* Scroll object list */
		while (object_cur < object_top)
			object_top = MAX(0, object_top - browser_rows/2);
		while (object_cur >= object_top + browser_rows)
			object_top = MIN(object_cnt - browser_rows, object_top + browser_rows/2);

		if (!visual_list)
		{
			/* Display a list of objects in the current group */
			display_object_list(max + 3, 6, browser_rows, object_idx, object_cur, object_top);
		}
		else
		{
			object_top = object_cur;

			/* Display a list of objects in the current group */
			display_object_list(max + 3, 6, 1, object_idx, object_cur, object_top);

			/* Display visual list below first object */
			display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
		}

		/* Get the current object */
		k_ptr = &k_info[object_idx[object_cur]];

		/* Mega Hack -- track this object */
		if (object_cnt) object_kind_track(object_idx[object_cur]);

		/* Prompt */
		prt(format("<dir>, 'r' to recall%s%s, '{', '}'%s, ESC, 'k', 'g', ...",
			(visual_list) ? ", ENTER to accept" : ", 'v' for visuals",
			(attr_idx||char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy",
			(note_idx) ? ", '\\' to re-inscribe" : (k_ptr->note) ? ", '\\'" : ""), hgt - 1, 0);

		/* The "current" object changed */
		if (object_old != object_idx[object_cur])
		{
			/* Hack -- handle stuff */
			handle_stuff();

			/* Remember the "current" object */
			object_old = object_idx[object_cur];
		}

		if (visual_list)
		{
			place_visual_list_cursor(max + 3, 7, k_ptr->flavor ? x_info[k_ptr->flavor].x_attr : k_ptr->x_attr, k_ptr->flavor ? x_info[k_ptr->flavor].x_char : k_ptr->x_char, attr_top, char_left);
		}
		else if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (object_cur - object_top));
		}
	
		ch = inkey();

		/* Do visual mode command if needed */
		if (visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, k_ptr->flavor ? &(x_info[k_ptr->flavor].x_attr) : &k_ptr->x_attr, k_ptr->flavor ? &(x_info[k_ptr->flavor].x_char) :&k_ptr->x_char)) continue;

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
				if (grp_cnt > 0)
					desc_obj_fake(object_idx[object_cur]);

				redraw = TRUE;
				break;
			}

			case '{':
			{
				char note_text[80] = "";

				if (k_ptr->aware || !k_ptr->flavor)
				{

					/* Prompt */
					prt("Inscribe with: ", hgt, 0);
	
					/* Default note */
					if (k_ptr->note)
					{
						sprintf(note_text, "%s", quark_str(k_ptr->note));
					}
	
					/* Get a filename */
					if (!askfor_aux(note_text, 80)) continue;
	
					/* Set the inscription */
					k_ptr->note = quark_add(note_text);

					/* Set the note */
					note_idx = k_ptr->note;

					/* Process objects */
					for (i = 1; i < o_max; i++)
					{
						/* Get the object */
						object_type *i_ptr = &o_list[i];

						/* Skip dead objects */
						if (!i_ptr->k_idx) continue;

						/* Not matching ego item */
						if (i_ptr->k_idx != object_idx[object_cur]) continue;

						/* Already has note */
						if (i_ptr->note) continue;
		
						/* Auto-inscribe */
						if (object_known_p(i_ptr) || cheat_auto) i_ptr->note = k_ptr->note;
					}
				}
				break;
			}

			case '}':
			{
				if (k_ptr->aware || !k_ptr->flavor)
				{
					/* Set the inscription */
					k_ptr->note = 0;

					/* Process objects */
					for (i = 1; i < o_max; i++)
					{
						/* Get the object */
						object_type *i_ptr = &o_list[i];
		
						/* Skip dead objects */
						if (!i_ptr->k_idx) continue;
		
						/* Not matching ego item */
						if (i_ptr->k_idx != object_idx[object_cur]) continue;
		
						/* Auto-inscribe */
						i_ptr->note = 0;
					}
				}
				break;
			}

			case '\\':
			{
				if (note_idx)
				{
					/* Set the note */
					k_ptr->note = note_idx;
	
					/* Process objects */
					for (i = 1; i < o_max; i++)
					{
						/* Get the object */
						object_type *i_ptr = &o_list[i];
			
						/* Skip dead objects */
						if (!i_ptr->k_idx) continue;
			
						/* Not matching ego item */
						if (i_ptr->k_idx != object_idx[object_cur]) continue;

						/* Already has note */
						if (i_ptr->note) continue;
			
						/* Auto-inscribe */
						if (object_known_p(i_ptr) || cheat_auto) i_ptr->note = note_idx;
					}
		
				}
				else if (k_ptr->note)
				{
					/* Set the note */
					note_idx = k_ptr->note;
				}
				break;
			}

			default:
			{
				if (target_dir(ch))
				{
					/* Move the cursor */
					browser_cursor(ch, &column, &grp_cur, grp_cnt, &object_cur, object_cnt);
				}
				else
				{
					note_idx = auto_note_modify(k_ptr->note,ch);
					k_ptr->note = note_idx;
				}

				break;
			}

		}
	}

	/* Prompt */
	if (!grp_cnt) prt("No object kinds known.", 14, 0);

	/* XXX XXX Free the "object_idx" array */
	FREE(object_idx);
}

/*
 * Description of each feature group.
 */
static cptr feature_group_text[] = 
{
	"Floors",
	"Traps",
	"Doors",
	"Stairs",
	"Walls",
	"Streamers",
	"Stores",
	"Chests",
	"Furnishings",
	"Bridges",
	"Water",
	"Lava",
	"Ice",
	"Acid",
	"Oil",
	"Chasms",
	"Sand/Earth",
	"Ground",
	"Trees/Plants",
	"Wilderness",
	NULL
};

/*
 * Flags of features in each group.
 */
static u32b feature_group_flag[] = 
{
	FF1_FLOOR, 0, 0,
	FF1_TRAP, 0, 0,
	FF1_DOOR, 0, 0,
	FF1_STAIRS, 0, 0,
	FF1_WALL, 0, 0,
	FF1_STREAMER, 0, 0,
	FF1_ENTER, 0, 0,
	0, 0, FF3_CHEST,
	0, 0, FF3_ALLOC,
	0, FF2_BRIDGED, 0,
	0, FF2_WATER, 0,
	0, FF2_LAVA, 0,
	0, FF2_ICE, 0,
	0, FF2_ACID, 0,
	0, FF2_OIL, 0,
	0, FF2_CHASM, 0,
	0, FF2_CAN_DIG, 0,
	0, 0, FF3_GROUND,
	0, 0, FF3_LIVING,
	0, 0, FF3_OUTSIDE,
	0,0,0
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

	/* Check every feature */
	for (i = 0; i < z_info->f_max; i++)
	{
		/* Access the race */
		feature_type *f_ptr = &f_info[i];

		/* Skip empty race */
		if (!f_ptr->name) continue;

		/* Skip features we never see */
		if (f_ptr->mimic != i) continue;

		/* Check for any flags matching in the group */
		if ((f_ptr->flags1 & (flags1)) || (f_ptr->flags2 & (flags2)) || (f_ptr->flags3 & (flags3)))
		{
			int idx, ii, tmp;

			for (ii = 0, idx = i;ii < feat_cnt; ii++)
			{
				if (strcmp(f_name + f_info[idx].name, f_name + f_info[feat_idx[ii]].name) <= 0)
				{
					tmp = idx;
					idx = feat_idx[ii];
					feat_idx[ii] = tmp;
				}

			}

			/* Add the race */
			feat_idx[feat_cnt++] = idx;
		}
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
	int col2 = 70;
	int col3 = 72;

	/* Correct columns 1 and 3 */
	if (use_bigtile)
	{
		col2++; col3++; col3++;
	}


	/* Display lines until done */
	for (i = 0; i < per_page && feat_idx[i]; i++)
	{
		byte attr;

		/* Get the race index */
		int f_idx = feat_idx[feat_top + i];

		/* Access the race */
		feature_type *f_ptr = &f_info[f_idx];

		/* Choose a color */
		attr = ((i + feat_top == feat_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Display the name */
		c_prt(attr, f_name + f_ptr->name, row + i, col);

		/* Hack -- visual_list mode */
		if (per_page == 1)
		{
			c_prt(attr, format(f_ptr->x_char >= 0 ? "%02x/%02x" : "%02x/%d", f_ptr->x_attr, f_ptr->x_char), row + i, 60);
		}
		else if (p_ptr->wizard) 
		{
			c_prt(attr, format("%d", f_idx), row + i, 60);
		}

		if (!use_dbltile && !use_trptile)
		{
			/* Display symbol */
			Term_putch(68, row + i, f_ptr->x_attr, f_ptr->x_char);

			if (use_bigtile)
			{
				if ((f_ptr->x_attr) & 0x80)
					Term_putch(68 + 1, row + i, 255, -1);
				else
					Term_putch(68 + 1, row + i, 0, ' ');
			}

			/* Tile supports special lighting? */
			if (!(f_ptr->flags2 & (FF2_ATTR_LITE)) ||
				((f_ptr->x_attr) && (arg_graphics = GRAPHICS_DAVID_GERVAIS_ISO)))
			{
				prt("       ", row + i, col2);
				continue;
			}

			Term_putch(col2, row + i, TERM_SLATE, '(');

			Term_putch(col3, row + i, TERM_SLATE, '/');

			if (use_bigtile)
				Term_putch(col3 + 3, row + i, TERM_SLATE, ')');
			else
				Term_putch(col3 + 2, row + i, TERM_SLATE, ')');

			/* Mega-hack */
			if (f_ptr->x_attr & 0x80)
			{
				/* Use a brightly lit tile */
				if ((arg_graphics == GRAPHICS_DAVID_GERVAIS) || (arg_graphics == GRAPHICS_DAVID_GERVAIS_ISO))
					Term_putch(col2+1, row + i, f_ptr->x_attr, f_ptr->x_char-1);
				else
					Term_putch(col2+1, row + i, f_ptr->x_attr, f_ptr->x_char+2);

				/* Use a dark tile */
				Term_putch(col3+1, row + i, f_ptr->x_attr, f_ptr->x_char+1);

			}
			else
			{
				/* Use "yellow" */
				Term_putch(col2+1, row + i, lite_attr[f_ptr->x_attr], f_ptr->x_char);

				/* Use "grey" */
				Term_putch(col3+1, row + i, dark_attr[f_ptr->x_attr], f_ptr->x_char);
			}

			if (use_bigtile)
			{
				if ((f_ptr->x_attr) & 0x80)
					Term_putch(col2 + 2, row + i, 255, -1);
				else
					Term_putch(col2 + 2, row + i, 0, ' ');

				if ((f_ptr->x_attr) & 0x80)
					Term_putch(col3 + 2, row + i, 255, -1);
				else
					Term_putch(col3 + 2, row + i, 0, ' ');
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
 * Interact with feature visuals.
 */
static void do_cmd_knowledge_features(void)
{
	int i, len, max;
	int grp_cur, grp_top, old_grp_cur;
	int feat_cur, feat_top;
	int grp_cnt, grp_idx[100];
	int feat_cnt;
	int *feat_idx;
	
	int column = 0;
	bool flag;
	bool redraw;

	bool visual_list = FALSE;
	byte attr_top = 0;
	char char_left = 0;

	int browser_rows;
	int wid, hgt;

	feature_type *f_ptr;

	/* Get size */
	Term_get_size(&wid, &hgt);

	browser_rows = hgt - 8;

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

	old_grp_cur = -1;
	grp_cur = grp_top = 0;
	feat_cur = feat_top = 0;
	feat_cnt = 0;

	flag = FALSE;
	redraw = TRUE;

	while ((!flag) && (grp_cnt))
	{
		char ch;

		if (redraw)
		{
			clear_from(0);
		
			prt("Visuals - features", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			prt("Sym", 4, 67);

			for (i = 0; i < 78; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < browser_rows; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top) grp_top = grp_cur;
		if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

		/* Display a list of feature groups */
		display_group_list(0, 6, max, browser_rows, grp_idx, feature_group_text, grp_cur, grp_top);

		if (old_grp_cur != grp_cur)
		{
			old_grp_cur = grp_cur;

			/* Get a list of features in the current group */
			feat_cnt = collect_features(grp_idx[grp_cur], feat_idx);
		}

		/* Scroll feature list */
		while (feat_cur < feat_top)
			feat_top = MAX(0, feat_top - browser_rows/2);
		while (feat_cur >= feat_top + browser_rows)
			feat_top = MIN(feat_cnt - browser_rows, feat_top + browser_rows/2);

		if (!visual_list)
		{
			/* Display a list of features in the current group */
			display_feature_list(max + 3, 6, browser_rows, feat_idx, feat_cur, feat_top);
		}
		else
		{
			feat_top = feat_cur;

			/* Display a list of features in the current group */
			display_feature_list(max + 3, 6, 1, feat_idx, feat_cur, feat_top);

			/* Display visual list below first feature */
			display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
		}

		/* Prompt */
		prt(format("<dir>%s%s, ESC", visual_list ? ", ENTER to accept" : ", 'v' for visuals, 'a' for lite", (attr_idx||char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy"), hgt - 1, 0);

		/* Get the current feature */
		f_ptr = &f_info[feat_idx[feat_cur]];

		if (visual_list)
		{
			place_visual_list_cursor(max + 3, 7, f_ptr->x_attr, f_ptr->x_char, attr_top, char_left);
		}
		else if (!column)
		{
			Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			Term_gotoxy(max + 3, 6 + (feat_cur - feat_top));
		}
	
		ch = inkey();

		/* Do visual mode command if needed */
		if (visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, &f_ptr->x_attr, &f_ptr->x_char)) continue;

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'a': case 'A':
			{
				if (f_ptr->flags2 & (FF2_ATTR_LITE))
				{
					f_ptr->flags2 &= ~(FF2_ATTR_LITE);
				}
				else
				{
					f_ptr->flags2 |= (FF2_ATTR_LITE);
				}
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &feat_cur, feat_cnt);
				break;
			}
		}
	}

	/* Prompt */
	if (!grp_cnt) msg_print("No features known.");

	/* XXX XXX Free the "feat_idx" array */
	FREE(feat_idx);
}



/*
 * Display contents of the Home. Code taken from the player death interface 
 * and the show known objects function. -LM-
 */
static void do_cmd_knowledge_home(void)
{
	int k;

	FILE *fff;

	object_type *o_ptr;
	char o_name[120];

	char file_name[1024];

	store_type *st_ptr = &store[STORE_HOME];

	/* Temporary file */
	fff = my_fopen_temp(file_name, 1024);
 
	/* Failure */
	if (!fff) return;

	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Display contents of the home */
		for (k = 0; k < st_ptr->stock_num; k++)
		{
			/* Acquire item */
			o_ptr = &st_ptr->stock[k];

			/* Acquire object description */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Print a message */
			fprintf(fff, "     %s\n", o_name);
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Contents of Your Home", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
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
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Display current knowledge", 2, 0);

		/* Give some choices */
		prt("(1) Display known artifacts", 4, 5);
		prt("(2) Display known monsters", 5, 5);
		prt("(3) Display known ego-items", 6, 5);
		prt("(4) Display known objects", 7, 5);
		prt("(5) Display known features", 8, 5);
		prt("(6) Display contents of your home", 9, 5);
		prt("(7) Load a user pref file", 10, 5);
		prt("(8) Dump auto-inscriptions", 11, 5);
		prt("(9) Interact with visuals", 12, 5);

		/* Prompt */
		prt("Command: ", 14, 0);

		/* Prompt */
		ch = inkey();

		/* Done */
		if (ch == ESCAPE) break;

		/* Artifacts */
		if (ch == '1')
		{
			/* Spawn */
			do_cmd_knowledge_artifacts();
		}

		/* Uniques */
		else if (ch == '2')
		{
			/* Spawn */
			do_cmd_knowledge_monsters();
		}

		/* Ego Items */
		else if (ch == '3')
		{
			/* Spawn */
			do_cmd_knowledge_ego_items();
		}

		/* Objects */
		else if (ch == '4')
		{
			/* Spawn */
			do_cmd_knowledge_objects();
		}

		/* Features */
		else if (ch == '5')
		{
			/* Spawn */
			do_cmd_knowledge_features();
		}

		/* Home */
		else if (ch == '6')
		{
			/* Spawn */
			do_cmd_knowledge_home();
		}

		/* Load a user pref file */
		else if (ch == '7')
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(13);

			/* Could skip the following if loading cancelled XXX XXX XXX */

			/* Mega-Hack -- Redraw physical windows */
			Term_redraw();
		}

		/* Dump colors */
		else if (ch == '8')
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump auto-inscriptions", 13, 0);

			/* Prompt */
			prt("File: ", 15, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Drop priv's */
			safe_setuid_drop();

			/* Dump the macros */
			(void)autos_dump(ftmp);

			/* Grab priv's */
			safe_setuid_grab();
   
			/* Message */
			msg_print("Appended auto-inscriptions.");
		}

		/* Visuals */
		else if (ch == '9')
		{
			/* Spawn */
			do_cmd_visuals();
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



