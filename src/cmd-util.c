/* File: cmd-util.c */
/* Purpose: Game interaction functions */

/*
 * The sheet of paper attached to the bottle reads as follows.
 * "Anyone who so much as takes a sip of this forbidden tonic will with
 * the barest thought be able to at will redraw the screen, change character
 * name, display previous messages. Interact with options.  Create code
 * to generate preference files.  Interact with macros, visuals, colors.  
 * Take notes, display level feeling and quests, save and load screen 
 * dumps.  Interact with the character knowledge menu (Display score, 
 * known objects, monsters, and artifacts, mutations, and pets).
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1 | PW_VISIBLE);

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
		if (mode > 2)
		{
			mode = 1;
		}
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
			mode++;
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
 * Toggle the autosave frequency
 */
static s16b autosave_freq_table[] =
{
	0, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, -1
};


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
		prt("(5) Birth Options", 8, 5);
		prt("(6) Cheat Options", 9, 5);

		/* Window flags */
		prt("(W) Window flags", 11, 5);

		/* Autosave */
		prt("(A) Autosave Frequency", 12, 5);
		
		/* Load and Append */
		prt("(L) Load a user pref file", 13, 5);
		prt("(S) Save options to a file", 14, 5);

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

		/* Birth Options */
		else if (ch == '5')
		{
			do_cmd_options_aux(4, "Birth Options");
		}

		/* Cheating Options */
		else if (ch == '6')
		{
			do_cmd_options_aux(5, "Cheat Options");
		}

		/* Window flags */
		else if ((ch == 'W') || (ch == 'w'))
		{
			do_cmd_options_win();
		}

		/* Load a user pref file */
		else if ((ch == 'L') || (ch == 'l'))
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(19);
		}

		/* Save options to a file */
		else if ((ch == 'S') || (ch == 's'))
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Append options to a file", 19, 0);

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
			prt("Command: Base Delay Factor", 19, 0);

			/* Get a new value */
			while (1)
			{
				char cx;
				int msec = op_ptr->delay_factor * op_ptr->delay_factor * 2;
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
			prt("Command: Hitpoint Warning", 19, 0);

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
		else if ((ch == 'a') || (ch == 'A'))
		{
			int i;
			
			/* Remember old value of autosave frequency */
			int old_autosave_freq = autosave_freq;

			/* Get the current autosave_freq table location */
			for (i = 0; autosave_freq_table[i] >= 0; i++)
			{
				if (autosave_freq_table[i] == autosave_freq) break;
			}

			/* Illegal autosave frequency -- cancel autosave */
			if (autosave_freq_table[i] < 0)
			{
				i = autosave_freq = 0;
			}

			/* Prompt */
			prt("Command: Autosave", 19, 0);

			/* Get a new value */
			while (TRUE)
			{
				char cx;

				if (!autosave_freq)
				{
					prt("Current autosave frequency:  Never", 21, 0);
				}
				else
				{
					prt(format("Current autosave frequency: every %d turns",
						autosave_freq), 21, 0);
				}
				prt("New autosave frequency (-/2 to lower, +/8 to raise, or ESC): ", 22, 0);

				cx = inkey();
				if (cx == ESCAPE) break;

				/* Go backwards */
				if (strchr("_-42jhJH", cx))
				{
					/* Go to the beginning of the table */
					if (i > 0)
					{
						/* Save the new value */
						autosave_freq = autosave_freq_table[--i];
					}
				}

				/* Go forwards */
				else
				{
					/* Go to the end of the table, stop when necessary */
					if (autosave_freq_table[++i] < 0) i--;

					/* Save the new value */
					autosave_freq = autosave_freq_table[i];
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
		ascii_to_text(buf, macro__act[i]);

		/* Dump the macro action */
		fprintf(fff, "A:%s\n", buf);

		/* Extract the macro pattern */
		ascii_to_text(buf, macro__pat[i]);

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
	ascii_to_text(tmp, buf);

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
	ascii_to_text(tmp, buf);

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
	for (i = 0; i < 256; i++)
	{
		char key[2] = "?";

		cptr act;

		/* Loop up the keymap */
		act = keymap_act[mode][i];

		/* Skip empty keymaps */
		if (!act) continue;

		/* Encode the action */
		ascii_to_text(buf, act);

		/* Dump the keymap action */
		fprintf(fff, "A:%s\n", buf);

		/* Convert the key into a string */
		key[0] = i;

		/* Encode the key */
		ascii_to_text(buf, key);

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
		ascii_to_text(tmp, macro_buffer);

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
				ascii_to_text(tmp, macro_buffer);

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
			ascii_to_text(tmp, macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, tmp);

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
				ascii_to_text(tmp, macro_buffer);

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
			ascii_to_text(tmp, macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, tmp);

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
			ascii_to_text(tmp, macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
			{
				/* Extract an action */
				text_to_ascii(macro_buffer, tmp);
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
		prt("(5) (unused)", 8, 5);
		prt("(6) Change monster attr/chars", 9, 5);
		prt("(7) Change object attr/chars", 10, 5);
		prt("(8) Change feature attr/chars", 11, 5);
		prt("(9) (unused)", 12, 5);
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
				fprintf(fff, "F:%d:0x%02X:0x%02X\n\n", i,
				        (byte)(f_ptr->x_attr), (byte)(f_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Close */
			my_fclose(fff);

			/* Message */
			msg_print("Dumped feature attr/chars.");
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

				/* Label the Current values */
				Term_putstr(10, 20, -1, TERM_WHITE,
				            format("Current attr/char = %3u / %3u", ca, cc));
				Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 20, ca, cc);

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
				char dc = (byte)(k_ptr->d_char);
				byte ca = (byte)(k_ptr->x_attr);
				char cc = (byte)(k_ptr->x_char);

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
				            format("Object = %d, Name = %-40.40s",
				                   k, (k_name + k_ptr->name)));

				/* Label the Default values */
				Term_putstr(10, 19, -1, TERM_WHITE,
				            format("Default attr/char = %3d / %3d", da, dc));
				Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 19, da, dc);

				/* Label the Current values */
				Term_putstr(10, 20, -1, TERM_WHITE,
				            format("Current attr/char = %3d / %3d", ca, cc));
				Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 20, ca, cc);

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

				byte da = (byte)(f_ptr->d_attr);
				char dc = (byte)(f_ptr->d_char);
				byte ca = (byte)(f_ptr->x_attr);
				char cc = (byte)(f_ptr->x_char);

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
				            format("Terrain = %d, Name = %-40.40s",
				                   f, (f_name + f_ptr->name)));

				/* Label the Default values */
				Term_putstr(10, 19, -1, TERM_WHITE,
				            format("Default attr/char = %3d / %3d", da, dc));
				Term_putstr(40, 19, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 19, da, dc);

				/* Label the Current values */
				Term_putstr(10, 20, -1, TERM_WHITE,
				            format("Current attr/char = %3d / %3d", ca, cc));
				Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
				Term_putch(43, 20, ca, cc);

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

#endif

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
	cptr quest_feel;
	
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
	
	/* Display the quest descpription for the current level (mode 3 = short)*/
	quest_feel = describe_quest(p_ptr->depth, 3);
	if (quest_check(p_ptr->depth)) message(MSG_GENERIC, -1, quest_feel);

}

/*
 * Display the current quest (if any)
 */
void do_cmd_quest(void)
{
	cptr q_out;

	quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

	/* Check if you're on a quest */
	if (p_ptr->cur_quest > 0)
	{
		/* Completed quest */
		if (!q_ptr->active_level)
		{
			msg_print("Collect your reward at the library!");
		}

		else
		{
			q_out = describe_quest(p_ptr->cur_quest, 4);

			/* Break into two lines if necessary */
			if (strlen(q_out) < 70) msg_print(q_out);
			else
			{
				q_out = describe_quest(p_ptr->cur_quest, 1);
				msg_format(q_out);
				q_out = describe_quest(p_ptr->cur_quest, 2);
				msg_format(q_out);
			}
		}
	}
	/* fixed quests */
	else if  (quest_check(p_ptr->depth) == QUEST_FIXED ||
		quest_check(p_ptr->depth) == QUEST_FIXED_U )
	{
			q_out = describe_quest(p_ptr->depth, 4);

			/* Break into two lines if necessary */
			if (strlen(q_out) < 70) msg_print(q_out);
			else
			{
				q_out = describe_quest(p_ptr->depth, 1);
				msg_format(q_out);
				q_out = describe_quest(p_ptr->depth, 2);
				msg_format(q_out);
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
		if (my_fgets(fp, buf, 1024)) okay = FALSE;


		/* Show each row */
		for (x = 0; x < 79; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Get the blank line */
	if (my_fgets(fp, buf, 1024)) okay = FALSE;


	/* Dump the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (my_fgets(fp, buf, 1024)) okay = FALSE;

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
* Hack -- load a screen dump from a file
* Hint: put shit in the 'file' directory using ANGBAND_DIR_FILE,
*/
void do_cmd_special_message(cptr path, cptr file)
{

	char buf[1024];
	
	path_build(buf, 1024, path, file);
	
	screen_save();
	
	show_file(buf, "Special Message", 0, 0);
	
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


#define BROWSER_ROWS	16


/*
 * Description of each monster group.
 */
static cptr monster_group_text[] =
{
	"Uniques",						/*All uniques, all letters*/
	"Automata",  					/*'a'*/
	"Alien",						/*'A'*/
	"Small Beastmen",				/*'b'*/
	"Large Beastmen",				/*'B'*/
	"Cards",						/*'c'*/
	"Constructs",					/*'C'*/
	"Small Dionsaurs",				/*'d'*/
	"Large Dinosaurs",				/*'D'*/
	"Equipment",					/*'e'*/
	"Elementals",					/*'E'*/
	"Felines",						/*'f'*/
	/*Unused*/						/*'F'*/
	"Golems",						/*'g'*/
	"Ghosts",						/*'G'*/
	"Humanoids",					/*'h'*/
	"Hybrids",						/*'H'*/
	"Birds",						/*'i'*/
	"Insects",						/*'I'*/
	/*Unused*/						/*'j'*/
	"Snakes",						/*'J'*/
	"Chickens",						/*'k'*/
	"Mechanical Chickens",			/*'K'*/
	"Lice",							/*'l'*/
	/*Unused*/						/*'L'*/
	"Molds",						/*'m'*/
	"Monkeys",						/*'M'*/
	"Plants",						/*'n'*/
	/*Unused*/						/*'N'*/
	"Objects",						/*'o'*/
	"Oz Residents",					/*'O'*/
	"People/Humans",				/*'p'*/
	"Giant Humanoids",				/*'P'*/
	"Quadrupeds",					/*'q'*/
	/*Unused*/						/*'Q'*/
	"Rodents",						/*'r'*/
	"Reptiles",						/*'R'*/
	"Skeletons",					/*'s'*/
	"Spiders",						/*'S'*/
	"Townpersons",					/*'t'*/
	"Trees",						/*'T'*/
	"Minor Demons",					/*'u'*/
	"Major Demons",					/*'U'*/
	"Vortices",						/*'v'*/
	"Vampires",						/*'V'*/
	"Worm Masses",				/*'w'*/
	"Wight/Wraith/etc",				/*'W'*/
	/*Unused*/						/*'x'*/
	/*Unused*/						/*'X'*/
	/*Unused*/							/*'y'*/
	/*Unused*/							/*'Y'*/
	"Zombies",						/*'z'*/
	"Hounds or Canines",			/*'Z'*/
	/*Unused*/						/*','*/
	/*Unused*/						/*'$!?=._-*/
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
	/*"F",Unused*/
	"g",
	"G",
	"h",
	"H",
	"i",
	"I",
	/*"j",Unused*/
	"J",
	"k",
	"K",
	"l",
	/*"L",Unused*/
	"m",
	"M",
	"n",
	/*"N",  Unused*/
	"o",
	"O",
	"p",
	"P",
	"q",
	/*"Q",  Unused*/
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
	/*"X", Unused*/
	/*"y", Unused*/
	/*"Y", Unused*/
	"z",
	"Z",
	/*",",Unused*/
	/*"$!?=._-",  Mimics*/
	NULL
};

#if 0
/*
 * Symbols and descriptions of monster groups.
 * Note that the "Uniques" group is handled differently.
 *
 * Most of the groups here should be in alphabetical order.
 */
static mon_struct monster_group[] =
{
	{ (char *) -1L, "uniques" },
	{ "A",  "aliens" },
	{ "a",  "automata" },
	{ "Bb", "beastmen" },
	{ "C",  "constructs" },
	{ "c",  "cards" },
	{ "&", "---" },
	{ "Dd", "dinosaurs" },
	{ "E",  "elementals" },
	{ "e",  "equipment" },
	{ "f",  "felines" },
	{ "F",  "---" },
	{ "G",  "ghosts" },
	{ "g",  "golems" },
	{ "p",  "humans" },
	{ "h",  "humanoids" },
	{ "H",  "hybrids" },
	{ "i",  "bird" },
	{ "I",  "insects" },
	{ "j",  "---" },
	{ "K",  "mechanical chickens" },
	{ "k",  "chickens" },
	{ "P",  "large humanoids" },
	{ "l",  "lice" },
	{ "L",  "---" },
	{ "$!?=\".`~^+<>()[]{}\\|/:", "---" },
	{ "m",  "molds" },
	{ "M",  "monkeys" },
	{ ",",  "mushroom patches" },
	{ "n",  "plants" },
	{ "@",  "non-player characters" },
	{ "O",  "oz residents" },
	{ "o",  "objects" },
	{ "q",  "quadrupeds" },
	{ "Q",  "---" },
	{ "R",  "reptiles" },
	{ "#%", "---" },
	{ "r",  "rodents" },
	{ "s",  "skeletons" },
	{ "J",  "snakes" },
	{ "S",  "spiders" },
	{ "t",  "townspeople" },
	{ "T",  "trees" },
	{ "Uu", "demons" },
	{ "V",  "vampires" },
	{ "v*", "vortexes and storms" },
	{ "W",  "wights/wraiths" },
	{ "w",  "worm masses" },
	{ "X",  "---" },
	{ "y",  "---" },
	{ "Y",  "---" },
	{ "Z",  "hounds or canines" },
	{ "z",  "zombies" },
	{ NULL, NULL }
};
#endif
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

		if (grp_unique && !(unique)) continue;

		/* Require known monsters */
		if (!(mode & 0x02) && (!cheat_know) && (!(l_ptr->r_sights))) continue;

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
 * Description of each object group.
 */
static cptr object_group_text[] =
{
	"Chests",
	"Pistol Ammo",
	"Rifle Bullets",
	"Shotgun Ammo",
	"Guns",
	"Diggers",
	"Hafted Weapons",
	"Polearms",
	"Swords",
	"Daggers",
	"Axes",
	"Blunt Weapons",
	"Boots",
	"Gloves",
	"Helms",
	"Trousers",
	"Cloaks",
	"Soft Armor",
	"Hard Armor",
	"Lights",
	"Amulets",
	"Rings",
	"Mecha Torsos",
	"Mecha Heads",
	"Mecha Arms",
	"Mecha Feet",
	"Tools",
	"Ray Guns",
	"Apparatuses",
	"Mechanisms",
	"Novels",
	"Books",
	"Tonics",
	"Food",
	"Magic/Engimatic",
	NULL
};

/*
 * TVALs of items in each group
 */
static byte object_group_tval[] =
{
	TV_CHEST,
	TV_AMMO,
	TV_BULLET,
	TV_SHOT,
	TV_GUN,
	TV_DIGGING,
	TV_HAFTED,
	TV_POLEARM,
	TV_SWORD,
	TV_DAGGER,
	TV_AXES,
	TV_BLUNT,
	TV_BOOTS,
	TV_GLOVES,
	TV_HELM,
	TV_LEG,
	TV_CLOAK,
	TV_SOFT_ARMOR,
	TV_HARD_ARMOR,
	TV_LITE,
	TV_AMULET,
	TV_RING,
	TV_MECHA_TORSO,
	TV_MECHA_HEAD,
	TV_MECHA_ARMS,
	TV_MECHA_FEET,
	TV_TOOL,
	TV_RAY,
	TV_APPARATUS,
	TV_MECHANISM,
	TV_TEXT,
	TV_BOOK,
	TV_TONIC,
	TV_FOOD,
	TV_MAGIC_BOOK,
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
		if (!k_ptr->aware) continue;

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
 * Move the cursor in a browser window.  -EZ-
 */
static void browser_cursor(char ch, int *column, int *grp_cur, int grp_cnt,
						   int *list_cur, int list_cnt)
{
	int d, dy, dx;
	int col = *column;
	int grp = *grp_cur;
	int list = *list_cur;

	/* Extract direction */
	d = target_dir(ch);

	/* Extract coordinates */
	dy = ddy[d];
	dx = ddx[d];

	/* Diagonals - hack */
	if ((dx > 0) && dy)
	{
		/* Browse group list */
		if (!col)
		{
			int old_grp = grp;

			/* Move up or down */
			grp += dy * BROWSER_ROWS;

			/* Verify */
			if (grp >= grp_cnt)	grp = grp_cnt - 1;
			if (grp < 0) grp = 0;
			if (grp != old_grp)	list = 0;
		}

		/* Browse sub-list list */
		else
		{
			/* Move up or down */
			list += dy * BROWSER_ROWS;

			/* Verify */
			if (list >= list_cnt) list = list_cnt - 1;
			if (list < 0) list = 0;
		}

		(*grp_cur) = grp;
		(*list_cur) = list;

		return;
	}

	if (dx)
	{
		col += dx;
		if (col < 0) col = 0;
		if (col > 1) col = 1;

		(*column) = col;

		return;
	}

	/* Advance one full page */
	if (ch == ' ')
	{
		dy = BROWSER_ROWS;
	}

	/* Back up one full page */
	if (ch == '-')
	{
		dy = 0 - BROWSER_ROWS;
	}

	/* Browse group list */
	if (!col)
	{
		int old_grp = grp;

		/* Move up or down */
		grp += dy;

		/* Verify */
		if (grp < 0) grp = 0;
		if (grp >= grp_cnt)	grp = grp_cnt - 1;
		if (grp != old_grp)	list = 0;
	}

	/* Browse sub-list list */
	else
	{
		/* Move up or down */
		list += dy;

		/* Verify */
		if (list >= list_cnt) list = list_cnt - 1;
		if (list < 0) list = 0;
	}

	(*grp_cur) = grp;
	(*list_cur) = list;
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
			/*Count if we have seen the unique*/
			if (l_ptr->r_sights)
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
		else slay_count += l_ptr->r_pkills;
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

		/* Get the monster race name (singular)*/
		monster_desc_race(race_name, sizeof(race_name), r_idx);

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

		/* Display kills */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/*use alive/dead for uniques*/
			put_str(format("%s", (r_ptr->max_num == 0) ? "dead" : "alive"),
			        row + i, 73);
		}
		else put_str(format("%5d", l_ptr->r_pkills), row + i, 73);

	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}

	/*Clear the monster count line*/
	Term_erase(0, 22, 255);

	if (monster_group_char[grp_cur] != (char *) -1L)
   	{


		c_put_str(TERM_L_BLUE, format("Total Creatures Slain: %8d.", slay_count), 22, col);
	}
	else
	{
		c_put_str(TERM_L_BLUE, format("Known Uniques: %3d, Slain Uniques: %3d.", known_uniques, dead_uniques),
						22, col);
	}
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
		if (o_ptr->name1 != art_num) continue;

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
		if (o_ptr->name1 != art_num) continue;

		/* We have a match, return the status*/
		return (o_ptr->ident & (IDENT_MENTAL));
	}

	/* Check each store for the artifact*/
	for (y = 0; y < MAX_STORES; y++)
	{
		/* Get the store */
		store_type *st_ptr = &store[y];

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
				if (o_ptr->name1 != art_num) continue;

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
 * Hack -- Create a "forged" artifact
 */
static bool prepare_fake_artifact(object_type *o_ptr, s16b name1)
{
	s16b i;

	artifact_type *a_ptr = &a_info[name1];

	/* Ignore "empty" artifacts */
	if (a_ptr->tval + a_ptr->sval == 0) return FALSE;

	/* Get the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (FALSE);

	/* Create the artifact */
	object_prep(o_ptr, i);

	/* Save the name */
	o_ptr->name1 = name1;

	/* Extract the fields */
	/* Extract pvals and related flags */
	o_ptr->pval = a_ptr->pval1;
	o_ptr->flags_pval1 = a_ptr->flags_pval1;
	o_ptr->pval2 = a_ptr->pval2;
	o_ptr->flags_pval2 = a_ptr->flags_pval2;
	o_ptr->pval3 = a_ptr->pval3;
	o_ptr->flags_pval3 = a_ptr->flags_pval3;

	o_ptr->flags1 = a_ptr->flags1;
	o_ptr->flags2 = a_ptr->flags2;
	o_ptr->flags3 = a_ptr->flags3;
	o_ptr->ac = a_ptr->ac;
	o_ptr->force = a_ptr->force;
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
	if (is_artifact_fully_identified(name1))
	{
		o_ptr->ident |= (IDENT_MENTAL);
	}

	/* Hack -- extract the "cursed" flag */
	if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

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

	do_cmd_observe(i_ptr, FALSE);
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
	C_MAKE(okay, z_info->a_max, bool);

	/* Default first,  */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/*start with false*/
		okay[i] = FALSE;

		/* Skip "empty" artifacts */
		if (a_ptr->tval + a_ptr->sval == 0) continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->cur_num) continue;

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
		if (!o_ptr->name1) continue;

		/* Ignore known items */
		if (object_known_p(o_ptr)) continue;

		/* We found a created, unidentified artifact */
		okay[o_ptr->name1] = FALSE;

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
		if (!o_ptr->name1) continue;

		/* Ignore known items */
		if (object_known_p(o_ptr)) continue;

		/* We found a created, unidentified artifact */
		okay[o_ptr->name1] = FALSE;
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
			if (!o_ptr->name1) continue;

			/* Ignore known items */
			if (object_known_p(o_ptr)) continue;

			/* We found a created, unidentified artifact */
			okay[o_ptr->name1] = FALSE;
		}

	}

	if (cheat_know)
	{
		for (i = 0; i < z_info->a_max; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			/* Skip "empty" artifacts */
			if (a_ptr->tval + a_ptr->sval == 0) continue;

			/*assume all created artifacts are good at this point*/
			okay[i] = TRUE;
		}
	}


	/* Finally, go through the list of artifacts and categorize the good ones */
	for (i = 0; i < z_info->a_max; i++)
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
	KILL(okay, bool);

	/* Return the number of races */
	return object_cnt;

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
		object_desc(o_name, i_ptr, TRUE, 0);

		/* Display the name */
		c_prt(attr, o_name, row + i, col);


		if (cheat_know)
		{
			artifact_type *a_ptr = &a_info[a_idx];

			c_prt(attr, format ("%3d", a_idx), row + i, 68);
			c_prt(attr, format ("%3d", a_ptr->level), row + i, 72);
			c_prt(attr, format ("%3d", a_ptr->rarity), row + i, 76);

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
	C_MAKE(artifact_idx, z_info->a_max, int);

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
		prt("<dir>, 'r' to recall, ESC", 23, 0);

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
	KILL(artifact_idx, int);
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

		/* Prompt */
		prt("<dir>, 'r' to recall, '<tab>' to switch format, ESC", 23, 0);

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
					while (TRUE) {
						char ch2;
						screen_roff(mon_idx[mon_cur].r_idx, p_ptr->monster_mem_fmt);

						ch2 = inkey();
						if (ch2 == '\t') {
							if (p_ptr->monster_mem_fmt == FALSE) p_ptr->monster_mem_fmt = TRUE;
							else p_ptr->monster_mem_fmt = FALSE;
							/* Update recall window */
							p_ptr->window |= (PW_MONSTER);
						}
						else break;

					}

					redraw = TRUE;
				}
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &mon_cur, monster_count);

				break;
			}
		}
	}

	/* XXX XXX Free the "mon_idx" array */
	KILL(mon_idx, monster_list_entry);
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
	/* apply_magic_fake(i_ptr); */

	/* It's fully known */
	i_ptr->ident |= IDENT_KNOWN;

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	do_cmd_observe(i_ptr, FALSE);
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
			byte a = k_ptr->d_attr; /* k_ptr->flavor ? (flavor_info[k_ptr->flavor].x_attr): k_ptr->d_attr; */
			byte c = k_ptr->d_char; /* k_ptr->flavor ? (flavor_info[k_ptr->flavor].x_char): k_ptr->d_char; */

			/* Display symbol */
			Term_putch(76, row + i, a, c);
		}
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
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
		prt("<dir>, 'r' to recall, ESC", 23, 0);

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
	KILL(object_idx, int);
}

/*
 * Display current pets
 */
static void do_cmd_knowledge_pets(void)
{
	int             i;
	FILE            *fff;
	monster_type    *m_ptr;
	int             t_friends = 0;
	int             t_levels = 0;
	int             show_upkeep = 0;
	char            file_name[1024];

	/* Temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return;

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for pets */
		if (is_pet(m_ptr))
		{	
			char buf[80];
			char pet_name[80];
			t_friends++;
			t_levels += (2 * (r_info[m_ptr->r_idx].level));
			monster_desc(pet_name, m_ptr, 0x88);
			fprintf(fff, "%s (%s)\n", pet_name, look_mon_desc(buf, i));
		}
	}

	if (t_friends > 1 + (p_ptr->lev / (cp_ptr->pet_upkeep_div)))
	{
		show_upkeep = t_levels;

		if (show_upkeep > 100) show_upkeep = 100;
		else if (show_upkeep < 10) show_upkeep = 10;
	}


	fprintf(fff, "----------------------------------------------\n");
	fprintf(fff, "   Total: %d pet%s.\n",
	        t_friends, (t_friends == 1 ? "" : "s"));
	fprintf(fff, "   Upkeep: %d%% mana.\n", show_upkeep);


	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Current Pets", 0, 0);

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
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Display current knowledge", 2, 0);

		/* Give some choices */
		prt("(1) Display known artifacts", 4, 5);
		prt("(2) Display known monsters", 5, 5);
		prt("(3) Display known objects", 6, 5);
		/* Mutations are in mutation.c */
		prt("(4) Display known mutations", 7, 5);
		/* Pets are in pets.c */
		prt("(5) Display pets", 8, 5);
		
		/* Prompt */
		prt("Command: ", 10, 0);

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

		/* Objects */
		else if (ch == '3')
		{
			/* Spawn */
			do_cmd_knowledge_objects();
		}
		/* Mutations */
		else if (ch == '4')
		{
			/* Spawn */
			do_cmd_knowledge_mutations();
		}
		/* Pets */
		else if (ch == '5')
		{
			/* Spawn */
			do_cmd_knowledge_pets();
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
