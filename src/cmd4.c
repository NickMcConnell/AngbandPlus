/* File: cmd4.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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


	/* Hack -- react to changes */
	Term_xtra(TERM_XTRA_REACT, 0);


	/* Combine and Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);


	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);

	/* Forget lite/view */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update lite/view */
	p_ptr->update |= (PU_VIEW | PU_LITE);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_MONSTER | PW_OBJECT);

	/* Hack -- update */
	handle_stuff();


	/* Redraw every window */
	for (j = 0; j < 8; j++)
	{
		/* Dead window */
		if (!angband_term[j])
			continue;

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
	char c;

	int mode = 0;

	char tmp[160];


	/* Save the screen */
	screen_save();

	/* Forever */
	while (1)
	{
		/* Display the player */
		display_player(mode);

		/* Prompt */
		Term_putstr(2, 23, -1, TERM_WHITE,
			"['c' to change name, 'f' to file, 'h' to change mode, or ESC]");

		/* Query */
		c = inkey();

		/* Exit */
		if (c == ESCAPE)
			break;

		/* Change name */
		if (c == 'c')
		{
			get_name();
		}

		/* File dump */
		else if (c == 'f')
		{
			sprintf(tmp, "%s.txt", op_ptr->base_name);
			if (get_string("File name: ", tmp, 80))
			{
				if (tmp[0] && (tmp[0] != ' '))
				{
					file_character(tmp);
				}
			}
		}

		/* Toggle mode */
		else if (c == 'h')
		{
			mode++;
		}

		/* Oops */
		else
		{
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}

	/* Restore the screen */
	screen_load();
}


/*
 * Recall the most recent message
 */
void do_cmd_message_one(void)
{
	/* Recall one message XXX XXX XXX */
	c_prt(message_prior(0), format("> %s", message_str(0)), 0, 0);
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
	int i, j, k, n, q;

	char shower[80];
	char finder[80];

	int per_screen = screen_y - 4;


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


	/* Save the screen */
	screen_save();

	/* Process requests until done */
	while (1)
	{
		/* Clear screen */
		clear_from(0);

		/* Dump per_screen lines of messages */
		for (j = 0; (j < per_screen) && (i + j < n); j++)
		{
			cptr msg = message_str(i + j);
			byte color = message_prior(i + j);

			/* Apply horizontal scroll */
			msg = (strlen(msg) >= q) ? (msg + q) : "";

			/* Dump the messages, bottom to top */
			Term_putstr(0, (screen_y - 3) - j, -1, color, msg);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = msg;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str - msg, (screen_y - 3) - j, len,
						TERM_YELLOW, shower);

					/* Advance */
					str += len;
				}
			}
		}

		/* Display header XXX XXX XXX */
		prt(format("Message Recall (%d-%d of %d), Offset %d", i, i + j - 1,
				n, q), 0, 0);

		/* Display prompt (not very informative) */
		prt("[Press 'p' for older, 'n' for newer, ..., or ESCAPE]",
			screen_y - 1, 0);

		/* Get a command */
		k = inkey();

		/* Exit on Escape */
		if (k == ESCAPE)
			break;

		/* Hack -- Save the old index */
		j = i;

		/* Horizontal scroll */
		if (k == '4')
		{
			/* Scroll left */
			q = (q >= 40) ? (q - 40) : 0;

			/* Success */
			continue;
		}

		/* Horizontal scroll */
		if (k == '6')
		{
			/* Scroll right */
			q = q + 40;

			/* Success */
			continue;
		}

		/* Hack -- handle show */
		if (k == '=')
		{
			/* Prompt */
			prt("Show: ", screen_y - 1, 0);

			/* Get a "shower" string, or continue */
			if (!askfor_aux(shower, 80, FALSE))
				continue;

			/* Okay */
			continue;
		}

		/* Hack -- handle find */
		if (k == '/')
		{
			int z;

			/* Prompt */
			prt("Find: ", screen_y - 1, 0);

			/* Get a "finder" string, or continue */
			if (!askfor_aux(finder, 80, FALSE))
				continue;

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

		/* Recall 1 older message */
		if ((k == '8') || (k == '\n') || (k == '\r'))
		{
			/* Go newer if legal */
			if (i + 1 < n)
				i += 1;
		}

		/* Recall 10 older messages */
		if (k == '+')
		{
			/* Go older if legal */
			if (i + 10 < n)
				i += 10;
		}

		/* Recall one screen of older messages */
		if ((k == 'p') || (k == KTRL('P')) || (k == ' '))
		{
			/* Go older if legal */
			if (i + per_screen < n)
				i += per_screen;
		}

		/* Recall one screen of newer messages */
		if ((k == 'n') || (k == KTRL('N')))
		{
			/* Go newer (if able) */
			i = (i >= per_screen) ? (i - per_screen) : 0;
		}

		/* Recall 10 newer messages */
		if (k == '-')
		{
			/* Go newer (if able) */
			i = (i >= 10) ? (i - 10) : 0;
		}

		/* Recall 1 newer messages */
		if (k == '2')
		{
			/* Go newer (if able) */
			i = (i >= 1) ? (i - 1) : 0;
		}

		/* Hack -- Error of some kind */
		if (i == j)
			bell();
	}

	/* Restore the screen */
	screen_load();
}


/*
 * Ask for a "user pref file" and process it.
 *
 * This function should only be used by standard interaction commands,
 * in which a standard "Command:" prompt is present on the given row.
 *
 * Allow absolute file names?  XXX XXX XXX
 */
static bool do_cmd_pref_file_hack(int row)
{
	char ftmp[80];

	/* Prompt */
	prt("Command: Load a user pref file", row, 0);

	/* Prompt */
	prt("File: ", row + 2, 0);

	/* Default filename */
	sprintf(ftmp, "%s.prf", op_ptr->base_name);

	/* Ask for a file (or cancel) */
	if (!askfor_aux(ftmp, 80, FALSE))
		return (FALSE);

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

	return (TRUE);
}


/*
 * Cheating options -- textual names
 */
cptr cheating_text[CHEAT_MAX] = {
	"cheat_peek",
	"cheat_hear",
	"cheat_room",
	"cheat_xtra",
	"cheat_know",
	"cheat_live"
};

/*
 * Cheating options -- descriptions
 */
cptr cheating_desc[CHEAT_MAX] = {
	"Peek into object creation",
	"Peek into monster creation",
	"Peek into dungeon creation",
	"Peek into something else",
	"Know complete monster info",
	"Allow player to avoid death"
};


/*
 * Hack -- write all current options to the given file
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

	/* Write to the file */
	fff = my_fopen(buf, "w");

	/* Failure */
	if (!fff)
		return (-1);

	/* Start dumping */
	fprintf(fff, "# Automatic option dump\n\n");

	/* Dump them */
	for (i = 0; i < OPT_MAX; i++)
	{
		/* Finished. */
		if (!option_text[i])
			break;

		fprintf(fff, "# Option '%s'\n", option_desc[i]);

		if (op_ptr->opt[i])
		{
			fprintf(fff, "Y:%s\n\n", option_text[i]);
		}
		else
		{
			fprintf(fff, "X:%s\n\n", option_text[i]);
		}
	}

	/* Dump window flags */
	for (i = 1; i < 8; i++)
	{
		/* Require a real window */
		if (!angband_term[i])
			continue;

		/* Check each flag */
		for (j = 0; j < 32; j++)
		{
			/* Require a real flag */
			if (!window_flag_desc[j])
				continue;

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
 * Interact with some options
 */
static void do_cmd_options_cheat(cptr info)
{
	char ch;

	int i, k = 0, n = CHEAT_MAX;

	char buf[80];


	/* Clear screen */
	Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
		sprintf(buf, "%s (RET to advance, y/n to set, ESC to accept) ",
			info);
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k)
				a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%s)", cheating_desc[i],
				p_ptr->cheat[i] ? "yes" : "no ", cheating_text[i]);
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
				if (p_ptr->cheat[k])
				{
					p_ptr->cheat[k] = FALSE;
				}

				else
				{
					/* Mark player as a cheater */
					p_ptr->noscore |= (0x0100 << k);

					p_ptr->cheat[k] = TRUE;
				}

				break;
			}

			case 'y':
			case '6':
			{
				/* Mark player as a cheater */
				p_ptr->noscore |= (0x0100 << k);

				p_ptr->cheat[k] = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case '4':
			{
				p_ptr->cheat[k] = FALSE;
				k = (k + 1) % n;
				break;
			}

			default:
			{
				bell();
				break;
			}
		}
	}

	do_cmd_redraw();
}


/*
 * Interact with some options
 */
static void do_cmd_options_aux(int page, cptr info)
{
	char ch;

	int i, k = 0, n = 0;

	int opt[22];

	char buf[80];


	/* Scan the options */
	for (i = 0; i < 22; i++)
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
		sprintf(buf, "%s (RET to advance, y/n to set, ESC to accept) ",
			info);
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k)
				a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%s)", option_desc[opt[i]],
				op_ptr->opt[opt[i]] ? "yes" : "no ", option_text[opt[i]]);
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

			default:
			{
				bell();
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
	int i, j;

	int y = 0;
	int x = 0;

	char ch;

	bool go = TRUE;

	u32b old_flag[8];


	/* Memorize old flags */
	for (j = 0; j < 8; j++)
	{
		/* Acquire current flags */
		old_flag[j] = op_ptr->window_flag[j];
	}


	/* Clear screen */
	Term_clear();

	/* Interact */
	while (go)
	{
		/* Prompt */
		prt("Window flags (<dir> to move, 't' to toggle, or ESC)", 0, 0);

		/* Display the windows */
		for (j = 0; j < 8; j++)
		{
			byte a = TERM_WHITE;

			cptr s = angband_term_name[j];

			/* Use color */
			if (j == x)
				a = TERM_L_BLUE;

			/* Window name, staggered, centered */
			Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
		}

		/* Display the options */
		for (i = 0; i < 16; i++)
		{
			byte a = TERM_WHITE;

			cptr str = window_flag_desc[i];

			/* Use color */
			if (i == y)
				a = TERM_L_BLUE;

			/* Unused option */
			if (!str)
				str = "(Unused option)";

			/* Flag name */
			Term_putstr(0, i + 5, -1, a, str);

			/* Display the windows */
			for (j = 0; j < 8; j++)
			{
				byte a = TERM_WHITE;

				char c = '.';

				/* Use color */
				if ((i == y) && (j == x))
					a = TERM_L_BLUE;

				/* Active flag */
				if (op_ptr->window_flag[j] & (1L << i))
					c = 'X';

				/* Flag value */
				Term_putch(35 + j * 5, i + 5, a, c);
			}
		}

		/* Place Cursor */
		Term_gotoxy(35 + x * 5, y + 5);

		/* Get key */
		ch = inkey();

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			case 'q':
			{
				go = FALSE;
				break;
			}

			case 't':
			case '.':
			case '5':
			case '0':
			{
				/* Hack -- ignore the main window */
				if (x == 0)
				{
					bell();
					break;
				}

				/* Toggle flag */
				if (op_ptr->window_flag[x] & (1L << y))
				{
					op_ptr->window_flag[x] &= ~(1L << y);
				}
				else
				{
					op_ptr->window_flag[x] |= (1L << y);
				}

				break;
			}

			default:
			{
				/* Extract direction */
				int d = target_dir(ch);

				/* Move */
				if (d != 0)
				{
					x = (x + ddx[d] + 8) % 8;
					y = (y + ddy[d] + 16) % 16;
				}

				/* Oops */
				else
				{
					bell();
				}

				break;
			}
		}
	}

	/* Notice changes */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* Dead window */
		if (!angband_term[j])
			continue;

		/* Ignore non-changes */
		if (op_ptr->window_flag[j] == old_flag[j])
			continue;

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
 * Set or unset various options.
 *
 * After using this command, a complete redraw is performed,
 * whether or not it is technically required.
 */
void do_cmd_options(void)
{
	int k;


	/* Save the screen */
	screen_save();


	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Why are we here */
		prt("Kamband options", 2, 0);

		/* Give some choices */
		prt("(1) User Interface Options", 4, 5);
		prt("(2) Disturbance Options", 5, 5);
		prt("(3) Game-Play Options", 6, 5);
		prt("(4) Efficiency Options", 7, 5);
		prt("(5) Kamband Options", 8, 5);

		/* Cheating */
		prt("(C) Cheating Options", 10, 5);

		/* Window flags */
		prt("(W) Window flags", 11, 5);

		/* Special choices */
		prt("(D) Base Delay Factor", 13, 5);
		prt("(H) Hitpoint Warning", 14, 5);

		/* Load and Save */
		prt("(R) Read options from a file", 4, 40);
		prt("(S) Write options to a file", 5, 40);

		/* Prompt */
		prt("Command: ", 18, 0);

		/* Get command */
		k = inkey();

		/* Exit */
		if (k == ESCAPE)
			break;

		/* Analyze */
		switch (k)
		{
				/* General Options */
			case '1':
			{
				/* Process the general options */
				do_cmd_options_aux(0, "User Interface Options");
				break;
			}

				/* Disturbance Options */
			case '2':
			{
				/* Spawn */
				do_cmd_options_aux(1, "Disturbance Options");
				break;
			}

				/* Inventory Options */
			case '3':
			{
				/* Spawn */
				do_cmd_options_aux(2, "Game-Play Options");
				break;
			}

				/* Efficiency Options */
			case '4':
			{
				/* Spawn */
				do_cmd_options_aux(3, "Efficiency Options");
				break;
			}

				/* Kamband Options */
			case '5':
			{
				/* Spawn */
				do_cmd_options_aux(4, "Kamband Options");
				break;
			}

				/* Cheating Options */
			case 'C':
			{
				/* Spawn */
				do_cmd_options_cheat("Cheaters never win (seriously!)");
				break;
			}

				/* Window flags */
			case 'W':
			case 'w':
			{
				/* Spawn */
				do_cmd_options_win();
				break;
			}

				/* Hack -- Delay Speed */
			case 'D':
			case 'd':
			{
				/* Prompt */
				prt("Command: Base Delay Factor", 18, 0);

				/* Get a new value */
				while (1)
				{
					int msec = op_ptr->delay_factor * op_ptr->delay_factor;
					prt(format("Current base delay factor: %d (%d msec)",
							op_ptr->delay_factor, msec), 22, 0);
					prt("Delay Factor (0-9 or ESC to accept): ", 20, 0);
					k = inkey();
					if (k == ESCAPE)
						break;
					if (isdigit(k))
						op_ptr->delay_factor = D2I(k);
					else
						bell();
				}

				break;
			}

				/* Hack -- hitpoint warning factor */
			case 'H':
			case 'h':
			{
				/* Prompt */
				prt("Command: Hitpoint Warning", 18, 0);

				/* Get a new value */
				while (1)
				{
					prt(format("Current hitpoint warning: %2d%%",
							op_ptr->hitpoint_warn * 10), 22, 0);
					prt("Hitpoint Warning (0-9 or ESC to accept): ", 20,
						0);
					k = inkey();
					if (k == ESCAPE)
						break;
					if (isdigit(k))
						op_ptr->hitpoint_warn = D2I(k);
					else
						bell();
				}

				break;
			}

			case 'R':
			case 'r':
			{
				/* Ask for and load a user pref file */
				(void) do_cmd_pref_file_hack(18);
				break;
			}

			case 'S':
			case 's':
			{
				char ftmp[80];

				/* Prepare the prompt */
				prt("Command: Write options to a file", 18, 0);

				/* Prompt */
				prt("File: ", 20, 0);

				/* Default filename */
				sprintf(ftmp, "%s.prf", op_ptr->base_name);

				/* Ask for a file */
				if (!askfor_aux(ftmp, 70, FALSE))
					break;

				/* Drop priviledges (?) */
				safe_setuid_drop();

				/* Dump the options */
				if (option_dump(ftmp))
				{
					/* Failure */
					mprint(MSG_TEMP, "Failed!");
				}
				else
				{
					/* Success */
					mprint(MSG_TEMP, "Done.");
				}

				/* Grab priviledges */
				safe_setuid_grab();

				break;
			}

				/* Unknown option */
			default:
			{
				/* Oops */
				bell();
				break;
			}
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Restore the screen */
	screen_load();
}



/*
 * Ask for a "user pref line" and process it
 *
 * XXX XXX XXX Allow absolute file names?
 */
void do_cmd_pref(void)
{
	char buf[80];

	/* Default */
	strcpy(buf, "");

	/* Ask for a "user pref command" */
	if (!get_string("Pref: ", buf, 80))
		return;

	/* Process that pref command */
	(void) process_pref_file_aux(buf);
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
	if (!fff)
		return (-1);


	/* Skip space */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic macro dump\n\n");

	/* Dump them */
	for (i = 0; i < macro__num; i++)
	{
		/* Start the macro */
		fprintf(fff, "# Macro '%d'\n\n", i);

		/* Extract the action */
		ascii_to_text(buf, macro__act[i]);

		/* Dump the macro */
		fprintf(fff, "A:%s\n", buf);

		/* Extract the action */
		ascii_to_text(buf, macro__pat[i]);

		/* Dump normal macros */
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
	int i, n = 0;

	char tmp[1024];


	/* Flush */
	flush();

	/* Do not process macros */
	inkey_base = TRUE;

	/* First key */
	i = inkey();

	/* Read the pattern */
	while (i)
	{
		/* Save the key */
		buf[n++] = i;

		/* Do not process macros */
		inkey_base = TRUE;

		/* Do not wait for keys */
		inkey_scan = TRUE;

		/* Attempt to read a key */
		i = inkey();
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
 * Hack -- append all keymaps to the given file
 */
static errr keymap_dump(cptr fname)
{
	int i;

	FILE *fff;

	char key[1024];
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
	if (!fff)
		return (-1);


	/* Skip space */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic keymap dump\n\n");

	/* Dump them */
	for (i = 0; i < 256; i++)
	{
		cptr act;

		/* Loop up the keymap */
		act = keymap_act[mode][i];

		/* Skip empty keymaps */
		if (!act)
			continue;

		/* Start the keymap */
		fprintf(fff, "# Keymap '%d'\n\n", i);

		/* Encode the action */
		ascii_to_text(buf, act);

		/* Dump the action */
		fprintf(fff, "A:%s\n", buf);

		/* Encode the key */
		buf[0] = i;
		buf[1] = '\0';
		ascii_to_text(key, buf);

		/* Dump the key */
		fprintf(fff, "C:%d:%s\n", mode, key);

		/* End the keymap */
		fprintf(fff, "\n\n");
	}

	/* Terminate */
	fprintf(fff, "\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}

#endif /* ALLOW_MACROS */


/*
 * Interact with "macros"
 *
 * Note that the macro "action" must be defined before the trigger.
 *
 * Could use some helpful instructions on this page.  XXX XXX XXX
 */
void do_cmd_macros(void)
{
	int i;

	char tmp[1024];

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
		ascii_to_text(buf, macro_buffer);

		/* Display the current action */
		prt(buf, 22, 0);


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
		i = inkey();

		/* Leave */
		if (i == ESCAPE)
			break;

		/* Load a 'macro' file */
		else if (i == '1')
		{
			/* Prompt */
			prt("Command: Load a user pref file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(tmp, 80, FALSE))
				continue;

			/* Process the given filename */
			if (0 != process_pref_file(tmp))
			{
				/* Prompt */
				mprint(MSG_TEMP, "Could not load file!");
			}
		}

#ifdef ALLOW_MACROS

		/* Save macros */
		else if (i == '2')
		{
			/* Prompt */
			prt("Command: Append macros to a file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(tmp, 80, FALSE))
				continue;

			/* Drop priv's */
			safe_setuid_drop();

			/* Dump the macros */
			(void) macro_dump(tmp);

			/* Grab priv's */
			safe_setuid_grab();

			/* Prompt */
			mprint(MSG_TEMP, "Appended macros.");
		}

		/* Query a macro */
		else if (i == '3')
		{
			int k;

			/* Prompt */
			prt("Command: Query a macro", 16, 0);

			/* Prompt */
			prt("Trigger: ", 18, 0);

			/* Get a macro trigger */
			do_cmd_macro_aux(buf);

			/* Acquire action */
			k = macro_find_exact(buf);

			/* Nothing found */
			if (k < 0)
			{
				/* Prompt */
				mprint(MSG_TEMP, "Found no macro.");
			}

			/* Found one */
			else
			{
				/* Obtain the action */
				strcpy(macro_buffer, macro__act[k]);

				/* Analyze the current action */
				ascii_to_text(buf, macro_buffer);

				/* Display the current action */
				prt(buf, 22, 0);

				/* Prompt */
				mprint(MSG_TEMP, "Found a macro.");
			}
		}

		/* Create a macro */
		else if (i == '4')
		{
			/* Prompt */
			prt("Command: Create a macro", 16, 0);

			/* Prompt */
			prt("Trigger: ", 18, 0);

			/* Get a macro trigger */
			do_cmd_macro_aux(buf);

			/* Clear */
			clear_from(20);

			/* Prompt */
			prt("Action: ", 20, 0);

			/* Convert to text */
			ascii_to_text(tmp, macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80, FALSE))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, tmp);

				/* Link the macro */
				macro_add(buf, macro_buffer);

				/* Prompt */
				mprint(MSG_TEMP, "Added a macro.");
			}
		}

		/* Remove a macro */
		else if (i == '5')
		{
			/* Prompt */
			prt("Command: Remove a macro", 16, 0);

			/* Prompt */
			prt("Trigger: ", 18, 0);

			/* Get a macro trigger */
			do_cmd_macro_aux(buf);

			/* Link the macro */
			macro_add(buf, buf);

			/* Prompt */
			mprint(MSG_TEMP, "Removed a macro.");
		}

		/* Save keymaps */
		else if (i == '6')
		{
			/* Prompt */
			prt("Command: Append keymaps to a file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(tmp, 80, FALSE))
				continue;

			/* Drop priv's */
			safe_setuid_drop();

			/* Dump the macros */
			(void) keymap_dump(tmp);

			/* Grab priv's */
			safe_setuid_grab();

			/* Prompt */
			mprint(MSG_TEMP, "Appended keymaps.");
		}

		/* Query a keymap */
		else if (i == '7')
		{
			cptr act;

			/* Prompt */
			prt("Command: Query a keymap", 16, 0);

			/* Prompt */
			prt("Keypress: ", 18, 0);

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(buf);

			/* Look up the keymap */
			act = keymap_act[mode][(byte) (buf[0])];

			/* Nothing found */
			if (!act)
			{
				/* Prompt */
				mprint(MSG_TEMP, "Found no keymap.");
			}

			/* Found one */
			else
			{
				/* Obtain the action */
				strcpy(macro_buffer, act);

				/* Analyze the current action */
				ascii_to_text(buf, macro_buffer);

				/* Display the current action */
				prt(buf, 22, 0);

				/* Prompt */
				mprint(MSG_TEMP, "Found a keymap.");
			}
		}

		/* Create a keymap */
		else if (i == '8')
		{
			/* Prompt */
			prt("Command: Create a keymap", 16, 0);

			/* Prompt */
			prt("Keypress: ", 18, 0);

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(buf);

			/* Clear */
			clear_from(20);

			/* Prompt */
			prt("Action: ", 20, 0);

			/* Convert to text */
			ascii_to_text(tmp, macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80, FALSE))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, tmp);

				/* Free old keymap */
				string_free(keymap_act[mode][(byte) (buf[0])]);

				/* Make new keymap */
				keymap_act[mode][(byte) (buf[0])] =
					string_make(macro_buffer);

				/* Prompt */
				mprint(MSG_TEMP, "Added a keymap.");

				/* XXX Hack -- See main-win.c */
				angband_keymap_flag = TRUE;
			}
		}

		/* Remove a keymap */
		else if (i == '9')
		{
			/* Prompt */
			prt("Command: Remove a keymap", 16, 0);

			/* Prompt */
			prt("Keypress: ", 18, 0);

			/* Get a keymap trigger */
			do_cmd_macro_aux_keymap(buf);

			/* Free old keymap */
			string_free(keymap_act[mode][(byte) (buf[0])]);

			/* Make new keymap */
			keymap_act[mode][(byte) (buf[0])] = NULL;

			/* Prompt */
			mprint(MSG_TEMP, "Removed a keymap.");

			/* XXX Hack -- See main-win.c */
			angband_keymap_flag = TRUE;
		}

		/* Enter a new action */
		else if (i == '0')
		{
			/* Prompt */
			prt("Command: Enter a new action", 16, 0);

			/* Go to the correct location */
			Term_gotoxy(0, 22);

			/* Hack -- limit the value */
			tmp[80] = '\0';

			/* Get an encoded action */
			if (!askfor_aux(buf, 80, FALSE))
				continue;

			/* Extract an action */
			text_to_ascii(macro_buffer, buf);
		}

#endif /* ALLOW_MACROS */

		/* Oops */
		else
		{
			/* Oops */
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Load screen */
	screen_load();
}



/*
 * Interact with "visuals"
 */
void do_cmd_visuals(void)
{
	int i;

	FILE *fff;

	char tmp[160];

	char buf[1024];


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Save the screen */
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
		i = inkey();

		/* Done */
		if (i == ESCAPE)
			break;

		/* Load a 'pref' file */
		else if (i == '1')
		{
			/* Prompt */
			prt("Command: Load a user pref file", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "user.prf");

			/* Query */
			if (!askfor_aux(tmp, 70, FALSE))
				continue;

			/* Process the given filename */
			(void) process_pref_file(tmp);
		}

#ifdef ALLOW_VISUALS

		/* Dump monster attr/chars */
		else if (i == '2')
		{
			/* Prompt */
			prt("Command: Dump monster attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "user.prf");

			/* Get a filename */
			if (!askfor_aux(tmp, 70, FALSE))
				continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff)
				continue;

			/* Start dumping */
			fprintf(fff, "\n\n");
			fprintf(fff, "# Monster attr/char definitions\n\n");

			/* Dump monsters */
			for (i = 0; i < MAX_R_IDX; i++)
			{
				monster_race *r_ptr = &r_info[i];

				/* Skip non-entries */
				if (!r_ptr->name)
					continue;

				/* Dump a comment */
				fprintf(fff, "# %s\n", (r_name + r_ptr->name));

				/* Dump the monster attr/char info */
				fprintf(fff, "R:%d:0x%02X:0x%02X\n\n", i,
					(byte) (r_ptr->x_attr), (byte) (r_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Close */
			my_fclose(fff);

			/* Message */
			mprint(MSG_TEMP, "Dumped monster attr/chars.");
		}

		/* Dump object attr/chars */
		else if (i == '3')
		{
			/* Prompt */
			prt("Command: Dump object attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "user.prf");

			/* Get a filename */
			if (!askfor_aux(tmp, 70, FALSE))
				continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff)
				continue;

			/* Start dumping */
			fprintf(fff, "\n\n");
			fprintf(fff, "# Object attr/char definitions\n\n");

			/* Dump objects */
			for (i = 0; i < MAX_K_IDX; i++)
			{
				object_kind *k_ptr = &k_info[i];

				/* Skip non-entries */
				if (!k_ptr->name)
					continue;

				/* Dump a comment */
				fprintf(fff, "# %s\n", (k_name + k_ptr->name));

				/* Dump the object attr/char info */
				fprintf(fff, "K:%d:0x%02X:0x%02X\n\n", i,
					(byte) (k_ptr->x_attr), (byte) (k_ptr->x_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Close */
			my_fclose(fff);

			/* Message */
			mprint(MSG_TEMP, "Dumped object attr/chars.");
		}

		/* Dump feature attr/chars */
		else if (i == '4')
		{
			/* Prompt */
			prt("Command: Dump feature attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "user.prf");

			/* Get a filename */
			if (!askfor_aux(tmp, 70, FALSE))
				continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff)
				continue;

			/* Start dumping */
			fprintf(fff, "\n\n");
			fprintf(fff, "# Feature attr/char definitions\n\n");

			/* Dump features */
			for (i = 0; i < MAX_F_IDX; i++)
			{
				feature_type *f_ptr = &f_info[i];

				/* Skip non-entries */
				if (!f_ptr->name)
					continue;

				/* Dump a comment */
				fprintf(fff, "# %s\n", (f_name + f_ptr->name));

				/* Dump the feature attr/char info */
				fprintf(fff, "F:%d:0x%02X:0x%02X\n\n", i,
					(byte) (f_ptr->z_attr), (byte) (f_ptr->z_char));
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Close */
			my_fclose(fff);

			/* Message */
			mprint(MSG_TEMP, "Dumped feature attr/chars.");
		}

		/* Modify monster attr/chars */
		else if (i == '6')
		{
			static int r = 0;

			/* Prompt */
			prt("Command: Change monster attr/chars", 15, 0);

			/* Hack -- query until done */
			while (1)
			{
				monster_race *r_ptr = &r_info[r];

				int da = (byte) (r_ptr->d_attr);
				int dc = (byte) (r_ptr->d_char);
				int ca = (byte) (r_ptr->x_attr);
				int cc = (byte) (r_ptr->x_char);

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					format("Monster = %d, Name = %-40.40s", r,
						(r_name + r_ptr->name)));

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
				i = inkey();

				/* All done */
				if (i == ESCAPE)
					break;

				/* Analyze */
				if (i == 'n')
					r = (r + MAX_R_IDX + 1) % MAX_R_IDX;
				if (i == 'N')
					r = (r + MAX_R_IDX - 1) % MAX_R_IDX;
				if (i == 'a')
					r_ptr->x_attr = (byte) (ca + 1);
				if (i == 'A')
					r_ptr->x_attr = (byte) (ca - 1);
				if (i == 'c')
					r_ptr->x_char = (byte) (cc + 1);
				if (i == 'C')
					r_ptr->x_char = (byte) (cc - 1);
			}
		}

		/* Modify object attr/chars */
		else if (i == '7')
		{
			static int k = 0;

			/* Prompt */
			prt("Command: Change object attr/chars", 15, 0);

			/* Hack -- query until done */
			while (1)
			{
				object_kind *k_ptr = &k_info[k];

				int da = (byte) k_ptr->k_attr;
				int dc = (byte) k_ptr->k_char;
				int ca = (byte) k_ptr->x_attr;
				int cc = (byte) k_ptr->x_char;

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					format("Object = %d, Name = %-40.40s", k,
						(k_name + k_ptr->name)));

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
				i = inkey();

				/* All done */
				if (i == ESCAPE)
					break;

				/* Analyze */
				if (i == 'n')
					k = (k + MAX_K_IDX + 1) % MAX_K_IDX;
				if (i == 'N')
					k = (k + MAX_K_IDX - 1) % MAX_K_IDX;
				if (i == 'a')
					k_info[k].x_attr = (byte) (ca + 1);
				if (i == 'A')
					k_info[k].x_attr = (byte) (ca - 1);
				if (i == 'c')
					k_info[k].x_char = (byte) (cc + 1);
				if (i == 'C')
					k_info[k].x_char = (byte) (cc - 1);
			}
		}

		/* Modify feature attr/chars */
		else if (i == '8')
		{
			static int f = 0;

			/* Prompt */
			prt("Command: Change feature attr/chars", 15, 0);

			/* Hack -- query until done */
			while (1)
			{
				feature_type *f_ptr = &f_info[f];

				int da = (byte) f_ptr->f_attr;
				int dc = (byte) f_ptr->f_char;
				int ca = (byte) f_ptr->z_attr;
				int cc = (byte) f_ptr->z_char;

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					format("Terrain = %d, Name = %-40.40s", f,
						(f_name + f_ptr->name)));

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
				i = inkey();

				/* All done */
				if (i == ESCAPE)
					break;

				/* Analyze */
				if (i == 'n')
					f = (f + MAX_F_IDX + 1) % MAX_F_IDX;
				if (i == 'N')
					f = (f + MAX_F_IDX - 1) % MAX_F_IDX;
				if (i == 'a')
					f_info[f].z_attr = (byte) (ca + 1);
				if (i == 'A')
					f_info[f].z_attr = (byte) (ca - 1);
				if (i == 'c')
					f_info[f].z_char = (byte) (cc + 1);
				if (i == 'C')
					f_info[f].z_char = (byte) (cc - 1);
			}
		}

#endif

		/* Reset visuals */
		else if (i == '0')
		{
			/* Reset */
			reset_visuals(TRUE);

			/* Message */
			mprint(MSG_TEMP, "Visual attr/char tables reset.");
		}

		/* Unknown option */
		else
		{
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Restore the screen */
	screen_load();
}


/*
 * Interact with "colors"
 */
void do_cmd_colors(void)
{
	int i;

	FILE *fff;

	char tmp[160];

	char buf[1024];


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Save the screen */
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
#endif

		/* Prompt */
		prt("Command: ", 8, 0);

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE)
			break;

		/* Load a 'pref' file */
		if (i == '1')
		{
			/* Prompt */
			prt("Command: Load a user pref file", 8, 0);

			/* Prompt */
			prt("File: ", 10, 0);

			/* Default file */
			sprintf(tmp, "user.prf");

			/* Query */
			if (!askfor_aux(tmp, 70, FALSE))
				continue;

			/* Process the given filename */
			(void) process_pref_file(tmp);

			/* Mega-Hack -- react to changes */
			Term_xtra(TERM_XTRA_REACT, 0);

			/* Mega-Hack -- redraw */
			Term_redraw();
		}

#ifdef ALLOW_COLORS

		/* Dump colors */
		else if (i == '2')
		{
			/* Prompt */
			prt("Command: Dump colors", 8, 0);

			/* Prompt */
			prt("File: ", 10, 0);

			/* Default filename */
			sprintf(tmp, "user.prf");

			/* Get a filename */
			if (!askfor_aux(tmp, 70, FALSE))
				continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff)
				continue;

			/* Start dumping */
			fprintf(fff, "\n\n");
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
				if (!kv && !rv && !gv && !bv)
					continue;

				/* Extract the color name */
				if (i < 16)
					name = color_names[i];

				/* Dump a comment */
				fprintf(fff, "# Color '%s'\n", name);

				/* Dump the monster attr/char info */
				fprintf(fff, "V:%d:0x%02X:0x%02X:0x%02X:0x%02X\n\n", i, kv,
					rv, gv, bv);
			}

			/* All done */
			fprintf(fff, "\n\n\n\n");

			/* Close */
			my_fclose(fff);

			/* Message */
			mprint(MSG_TEMP, "Dumped color redefinitions.");
		}

		/* Edit colors */
		else if (i == '3')
		{
			static int a = 0;

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
					Term_putstr(i * 4, 20, -1, a, "###");

					/* Exhibit all colors */
					Term_putstr(i * 4, 22, -1, i, format("%3d", i));
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
				i = inkey();

				/* All done */
				if (i == ESCAPE)
					break;

				/* Analyze */
				if (i == 'n')
					a = (byte) (a + 1);
				if (i == 'N')
					a = (byte) (a - 1);
				if (i == 'k')
					angband_color_table[a][0] =
						(byte) (angband_color_table[a][0] + 1);
				if (i == 'K')
					angband_color_table[a][0] =
						(byte) (angband_color_table[a][0] - 1);
				if (i == 'r')
					angband_color_table[a][1] =
						(byte) (angband_color_table[a][1] + 1);
				if (i == 'R')
					angband_color_table[a][1] =
						(byte) (angband_color_table[a][1] - 1);
				if (i == 'g')
					angband_color_table[a][2] =
						(byte) (angband_color_table[a][2] + 1);
				if (i == 'G')
					angband_color_table[a][2] =
						(byte) (angband_color_table[a][2] - 1);
				if (i == 'b')
					angband_color_table[a][3] =
						(byte) (angband_color_table[a][3] + 1);
				if (i == 'B')
					angband_color_table[a][3] =
						(byte) (angband_color_table[a][3] - 1);

				/* Hack -- react to changes */
				Term_xtra(TERM_XTRA_REACT, 0);

				/* Hack -- redraw */
				Term_redraw();
			}
		}

#endif

		/* Unknown option */
		else
		{
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Restore the screen */
	screen_load();
}


/*
 * Note something in the message recall
 */
void do_cmd_note(void)
{
	char buf[80];

	/* Default */
	strcpy(buf, "");

	/* Input */
	if (!get_string("Note: ", buf, 60))
		return;

	/* Ignore empty notes */
	if (!buf[0] || (buf[0] == ' '))
		return;

	/* Add the note to the message recall */
	mformat(MSG_WARNING, "Note: %s", buf);
}


/*
 * Mention the current version
 */
void do_cmd_version(void)
{
	/* Silly message */
	msg_format("You are playing Kamband %d.%d.  Type '?' for more info.",
		KAM_VERSION_MAJOR, KAM_VERSION_MINOR);
}



/*
 * Array of feeling strings
 */
static cptr do_cmd_feeling_text[11] = {
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
 * Array of pet feeling strings.
 */

static cptr do_cmd_pet_feeling_text[11] = {
	"You sense a small friendly aura.",
	"You sense a friendly aura.",
	"You have a feeling of friendship.",
	"You feel supported by your allies.",
	"You feel a sense of true friendship.",
	"You sense you are deeply respected.",
	"You feel the adoration your followers.",
	"You feel like a true leader.",
	"You feel that your followers will do anything for you.",
	"You feel as if you are in charge of this level."
};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(void)
{
	/* Verify the feeling */
	if (feeling < 0)
		feeling = 0;
	if (feeling > 10)
		feeling = 10;

	/* No useful feeling in town */
	if (!p_ptr->depth)
	{
		msg_print("Looks like a typical town.");
		return;
	}

	/* Display the feeling */
	msg_print(do_cmd_feeling_text[feeling]);

	if (pet_rating)
	{
		msg_print(do_cmd_pet_feeling_text[pet_rating - 1]);
	}
}


/*
 * Encode the screen colors
 */
static char hack[17] = "dwsorgbuDWvyRGBU";


/*
 * Hack -- load a screen dump from a file
 */
void do_cmd_load_screen(void)
{
	int i, y, x;

	byte a = 0;
	char c = ' ';

	bool okay = TRUE;

	FILE *fff;

	char tmp_val[80], buf[1024];

	int len;


	/* Default filename */
	(void) strcpy(tmp_val, "dump.txt");

	/* Ask for a file */
	if (!get_string("File: ", tmp_val, 80))
		return;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, tmp_val);

	/* Read the file */
	fff = my_fopen(buf, "r");

	/* Oops */
	if (!fff)
		return;

	/* Save screen */
	screen_save();

	/* Clear the screen */
	Term_clear();

	/* Load the screen */
	for (y = 0; okay; y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, 1024))
			okay = FALSE;

		/* Stop on blank line */
		if (!buf[0])
			break;

		/* Get the width */
		len = strlen(buf);

		/* XXX Restrict to current screen size */
		if (len >= Term->wid)
			len = Term->wid;

		/* Show each row */
		for (x = 0; x < len; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Dump the screen */
	for (y = 0; okay; y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, 1024))
			okay = FALSE;

		/* Stop on blank line */
		if (!buf[0])
			break;

		/* Get the width */
		len = strlen(buf);

		/* XXX Restrict to current screen size */
		if (len >= Term->wid)
			len = Term->wid;

		/* Dump each row */
		for (x = 0; x < len; x++)
		{
			/* Get the attr/char */
			(void) (Term_what(x, y, &a, &c));

			/* Look up the attr */
			for (i = 0; i < 16; i++)
			{
				/* Use attr matches */
				if (hack[i] == buf[x])
					a = i;
			}

			/* Put the attr/char */
			Term_draw(x, y, a, c);
		}
	}

	/* Close it */
	my_fclose(fff);

	/* Message */
	mprint(MSG_TEMP, "Screen dump loaded.");
	msg_print(NULL);

	/* Restore the screen */
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

	char tmp_val[80], buf[1024];


	/* Ask for a file (or cancel) */
	(void) strcpy(tmp_val, "dump.txt");

	/* Ask for a file */
	if (!get_string("File: ", tmp_val, 80))
		return;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, tmp_val);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Hack -- drop permissions */
	safe_setuid_drop();

	/* Append to the file */
	fff = my_fopen(buf, "w");

	/* Hack -- grab permissions */
	safe_setuid_grab();

	/* Oops */
	if (!fff)
		return;

	/* Save screen */
	screen_save();

	/* Dump the screen */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Dump each row */
		for (x = 0; x < Term->wid; x++)
		{
			/* Get the attr/char */
			(void) (Term_what(x, y, &a, &c));

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
	for (y = 0; y < Term->hgt; y++)
	{
		/* Dump each row */
		for (x = 0; x < Term->wid; x++)
		{
			/* Get the attr/char */
			(void) (Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = hack[a & 0x0F];
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
	mprint(MSG_TEMP, "Screen dump saved.");
	msg_print(NULL);

	/* Load screen */
	screen_load();
}


/*
 * Display known artifacts
 */
static void do_cmd_knowledge_artifacts(void)
{
	int k, z;

	FILE *fff;

	char file_name[1024];

	char base_name[80];

	bool okay[MAX_A_IDX];

	object_type *o_ptr;


	/* Temporary file */
	if (path_temp(file_name, 1024))
		return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	/* Scan the artifacts */
	for (k = 0; k < MAX_A_IDX; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* Default */
		okay[k] = FALSE;

		/* Skip "empty" artifacts */
		if (!a_ptr->name)
			continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->cur_num)
			continue;

		/* Assume okay */
		okay[k] = TRUE;
	}

	/* Check the dungeon */
	for (o_ptr = o_list; o_ptr != NULL; o_ptr = o_ptr->next_global)
	{

		/* Ignore random artifacts */
		if (o_ptr->tval == TV_RANDART)
			continue;

		/* Ignore non-artifacts */
		if (!artifact_p(o_ptr))
			continue;

		/* Ignore known items */
		if (object_known_p(o_ptr))
			continue;

		/* Note the artifact */
		okay[o_ptr->name1] = FALSE;
	}

	/* Scan the artifacts */
	for (k = 0; k < MAX_A_IDX; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* List "dead" ones */
		if (!okay[k])
			continue;

		/* Paranoia */
		strcpy(base_name, "Unknown Artifact");

		/* Obtain the base object type */
		z = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Real object */
		if (z)
		{
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Create fake object */
			object_prep(i_ptr, z);

			/* Make it an artifact */
			i_ptr->name1 = k;

			/* Describe the artifact */
			object_desc_store(base_name, i_ptr, FALSE, 0);
		}

		/* Hack -- Build the artifact name */
		fprintf(fff, "     The %s\n", base_name);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Known artifacts", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Display known uniques
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
static void do_cmd_knowledge_uniques(void)
{
	int k;

	FILE *fff;

	char file_name[1024];


	/* Temporary file */
	if (path_temp(file_name, 1024))
		return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	/* Scan the monster races */
	for (k = 1; k < MAX_R_IDX - 1; k++)
	{
		monster_race *r_ptr = &r_info[k];

		/* Only print Uniques */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			bool dead = (r_ptr->max_num == 0);

			/* Only display "known" uniques */
			if (dead || cheat_know || r_ptr->r_sights)
			{
				/* Print a message */
				fprintf(fff, "     %s is %s\n", (r_name + r_ptr->name),
					(dead ? "dead" : "alive"));
			}
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Known Uniques", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(void)
{
	int k;

	FILE *fff;

	char o_name[80];

	char file_name[1024];


	/* Temporary file */
	if (path_temp(file_name, 1024))
		return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	/* Scan the object kinds */
	for (k = 1; k < MAX_K_IDX; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Hack -- skip artifacts */
		if (k_ptr->flags3 & (TR3_INSTA_ART))
			continue;

		/* List known flavored objects */
		if (k_ptr->has_flavor && k_ptr->aware)
		{
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Create fake object */
			object_prep(i_ptr, k);

			/* Describe the object */
			object_desc_store(o_name, i_ptr, FALSE, 0);

			/* Print a message */
			fprintf(fff, "     %s\n", o_name);
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Known Objects", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Display known recipes.
 */
static void do_cmd_knowledge_recipes(void)
{
	int k, i;
	long mask;
	FILE *fff;
	char o_name[80];
	char ingr_names[80];
	char file_name[1024];
	object_type tmp_obj;

	if (path_temp(file_name, 1024))
		return;

	fff = my_fopen(file_name, "w");

	for (k = 0; k < MAX_RECIPES; k++)
	{
		/*
		 * for (i = 0; i < MAX_RECIPES; i++) {
		 * if (k != i) {
		 * if (recipe_info[k].ingrs == recipe_info[i].ingrs &&
		 * recipe_info[k].result_kind != 0) {
		 * printf("Illegal mismatch: %d vs %d\n", recipe_info[k].result_kind,
		 * recipe_info[i].result_kind);
		 * }
		 * }
		 * }
		 */

		if (recipe_recall[k])
		{
			mask = recipe_info[k].ingrs;

			if (recipe_recall[k] == 1)
			{
				/* Get resulting object description. */

				object_prep(&tmp_obj, recipe_info[k].result_kind);
				object_desc_store(o_name, &tmp_obj, FALSE, 0);
			}
			else
			{
				strcpy(o_name, "Something");
			}

			ingr_names[0] = '\0';

			for (i = 0; i < 16; i++)
			{
				if (mask & (1L << i))
				{
					strcat(ingr_names, ingr_short_names[i]);
					strcat(ingr_names, " ");
				}
			}

			fprintf(fff, "  %s -- %s\n", o_name, ingr_names);
		}
	}

	my_fclose(fff);

	show_file(file_name, "Known Formulae", 0, 0);

	fd_kill(file_name);
}




/*
 * Display your mutations.
 */
static void do_cmd_knowledge_mutations(void)
{
	int i;
	FILE *fff;
	char file_name[1024];

	if (path_temp(file_name, 1024))
		return;

	fff = my_fopen(file_name, "w");

	for (i = 0; i < 32; i++)
	{
		if (p_ptr->mutations1 & (1L << i))
		{
			fprintf(fff, "%s\n", mutation_names[i][0]);
		}

		if (p_ptr->mutations2 & (1L << i))
		{
			fprintf(fff, "%s\n", mutation_names[i + 32][0]);
		}

		if (p_ptr->mutations3 & (1L << i))
		{
			fprintf(fff, "%s\n", mutation_names[i + 64][0]);
		}
	}

	my_fclose(fff);

	show_file(file_name, "Your mutations", 0, 0);

	fd_kill(file_name);
}


/*
 * Display completed quests.
 */

static void do_cmd_knowledge_quests(void)
{
	int i, q_stat;
	FILE *fff;
	char file_name[1024];

	if (path_temp(file_name, 1024))
		return;

	fff = my_fopen(file_name, "w");

	for (i = 0; i < max_quests; i++)
	{
		q_stat = quest_status[i];

		if (q_stat == QUEST_COMPLETED)
		{
			fprintf(fff, "%30s: Completed\n", v_name + q_v_ptrs[i]->name);

		}
		else if (q_stat == QUEST_IN_PROGRESS)
		{
			fprintf(fff, "%30s: In progress\n",
				v_name + q_v_ptrs[i]->name);

		}
		else if (q_stat == QUEST_ASSIGNED)
		{
			fprintf(fff, "%30s: Assigned\n", v_name + q_v_ptrs[i]->name);
		}
	}

	my_fclose(fff);

	show_file(file_name, "Your completed quests", 0, 0);

	fd_kill(file_name);
}



/*
 * Display your spell-list, in a nice tabular form.
 */

static void do_cmd_knowledge_spells(void)
{
	int i;
	FILE *fff;
	char file_name[1024];
	spell *rspell;

	if (path_temp(file_name, 1024))
		return;

	fff = my_fopen(file_name, "w");

	for (i = 0; i < spell_num; i++)
	{
		rspell = &spells[i];

		if (rspell->unknown)
			continue;

		if (rspell->untried)
		{
			fprintf(fff, "%3d) %-30s (Spell Untried)\n", i + 1,
				rspell->name);

		}
		else
		{
			fprintf(fff, "%3d) %-30s %4d%% %3d (%s)\n", i + 1,
				rspell->name, spell_chance(rspell), rspell->mana,
				rspell->desc);
		}
	}

	my_fclose(fff);

	show_file(file_name, "Your spells", 0, 0);

	fd_kill(file_name);
}



/*
 * Display your character dump.
 */

static void do_cmd_knowledge_dump(void)
{
	char file_name[1024];

	if (path_temp(file_name, 1024))
		return;

	file_character(file_name);

	show_file(file_name, "Character dump", 0, 0);

	fd_kill(file_name);
}


/*
 * Description of each monster group.
 */
static char *monster_group_text[] = {
"Ancient Dragons",
"Angelic Beings",
"Aquatic",
"Birds",
"Canines",
"Creeping Coins",
"Demihumans",
"Dragons",
"Elementals",
"Energy",
"Eyes/Beholders",
"Felines",
"Ghosts",
"Giant Ants",
"Giant Bats",
"Giant Centipedes",
"Giant Dragon Flies",
"Giant Lice",
"Giants",
"Golems",
"Humans",
"Hybrids",
"Mummies",
"Icky Things",
"Jellies",
"Killer Beetles",
"Kobolds",
"Lichs",
"Major Demons",
"Minor Demons",
"Molds",
"Multiplying Insects",
"Mushroom Patches",
"Nagas",
"Ogres",
"Orcs",
"Quadropeds",
"Quylthulgs",
"Reptiles/Amphibians",
"Rodents",
"Scorpions/Spiders",
"Skeletons",
"Snakes",
"Townspeople",
"Tricksters",
"Trolls",
"Vampires",
"Vortices",
"Wights/Wraiths",
"Worms/Worm Masses",
"Xorns/Xarens",
"Yeeks",
"Yeti",
"Zephyr Hounds",
"Zombies/Mummies",
"Uniques",
NULL};

/*
 * Symbols of monsters in each group. Note the "Uniques" group
 * is handled differently.
 */
char *monster_group_char[] = {
"D",
"A",
"x",
"B",
"C",
"$",
"h",
"d",
"E",
"*",
"e",
"f",
"G",
"a",
"b",
"c",
"F",
"l",
"P",
"g",
"p",
"H",
"M",
"i",
"j",
"K",
"k",
"L",
"U",
"u",
"m",
"I",
",",
"n",
"O",
"o",
"q",
"Q",
"R",
"r",
"S",
"s",
"J",
"t",
"!?=.#",
"T",
"V",
"v",
"W",
"w",
"X",
"y",
"Y",
"Z",
"z",
(char *) -1L,
NULL
};


/*
 * Build a list of monster indexes in the given group. Return the number
 * of monsters in the group.
 */
static int collect_monsters(int grp_cur, int mon_idx[], int mode)
{
	int i, mon_cnt = 0;

	/* Get a list of x_char in this group */
	char *group_char = monster_group_char[grp_cur];

	/* XXX Hack -- Check if this is the "Uniques" group */
	bool grp_unique = (monster_group_char[grp_cur] == (char *) -1L);


	/* Check every race (skip ghost) */
	for (i = 0; i < MAX_R_IDX - 1; i++)
	{
		/* Access the race */
		monster_race *r_ptr = &r_info[i];

		/* Is this a unique? */
		bool unique = (r_ptr->flags1 & RF1_UNIQUE) != 0;

		/* Is the unique dead? */
		bool dead = unique && (r_ptr->max_num == 0);

		/* Skip empty race */
		if (!r_ptr->name) continue;

		/* Require matching "unique" status */
		if (unique != grp_unique) continue;

		/* Require "known" race */
		if (!((mode & 0x02) || cheat_know || dead || r_ptr->r_sights)) continue;

		/* Check for race in the group */
		if (unique || strchr(group_char, r_ptr->d_char))
		{
			/* Add the race */
			mon_idx[mon_cnt++] = i;

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
 * Display the monster groups.
 */
static void display_group_list(int col, int row, int wid, int per_page,
	int grp_idx[], int grp_cur, int grp_top)
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
		c_put_str(attr, monster_group_text[grp], row + i, col);
	}
}


/*
 * Display the monsters in a group.
 */
static void display_monster_list(int col, int row, int per_page, int mon_idx[],
	int mon_cur, int mon_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && mon_idx[i]; i++)
	{
		/* Get the race index */
		int r_idx = mon_idx[mon_top + i];

		/* Access the race */
		monster_race *r_ptr = &r_info[r_idx];

		/* Choose a color */
		byte attr = (i + mon_top == mon_cur) ? TERM_L_BLUE : TERM_WHITE;

		/* Display the name */
		c_prt(attr, r_name + r_ptr->name, row + i, col);

		/* Display symbol */
		Term_putch(screen_x - 9, row + i, r_ptr->x_attr, r_ptr->x_char);

		/* Display kills */
		put_str(format("%5d", r_ptr->r_pkills), row + i, screen_x - 5);
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}


/*
 * Search all the groups for a monster.
 */
static bool find_monster(int grp_cnt, int grp_idx[], int *grp_cur, int *mon_cur,
	int mode, long data)
{
	int i, j;
	int grp_s, grp_e, loop = 2;
	int mon_idx[MAX_R_IDX];

	/* Limits of search */
	grp_s = (*grp_cur);
	grp_e = grp_cnt;

	/* Search until done */
	while (loop--)
	{
		/* Check each group */
		for (i = grp_s; i < grp_e; i++)
		{
			int mon_s, mon_e;

			/* Get monsters in this group */
			int mon_cnt = collect_monsters(grp_idx[i], mon_idx, 0x00);

			/* Limits of search */
			mon_s = 0;
			mon_e = mon_cnt;

			/*
			 * The first time around, start after the current
			 * monster. The second time around, stop at the
			 * current monster (so it can be "found").
			 */
			if (i == (*grp_cur))
			{
				if (loop)
				{
					mon_s = (*mon_cur) + 1;
				}
				else
				{
					mon_e = (*mon_cur) + 1;
				}
			}

			/* Check each monster in this group */
			for (j = mon_s; j < mon_e; j++)
			{
				/* Access the monster race */
				monster_race *r_ptr = &r_info[mon_idx[j]];

				/* Look for char */
				if ((mode == 1) && (r_ptr->x_char == (char) data))
				{
					/* Set the current group */
					(*grp_cur) = i;

					/* Set the current monster */
					(*mon_cur) = j;

					/* Success */
					return (TRUE);
				}

				/* Look for name */
				if ((mode == 2) && strstr(r_name + r_ptr->name, (char *) data))
				{
					/* Set the current group */
					(*grp_cur) = i;

					/* Set the current monster */
					(*mon_cur) = j;

					/* Success */
					return (TRUE);
				}
			}
		}

		/* Limits of search */
		grp_s = 0;
		grp_e = (*grp_cur) + 1;
	}

	/* No match */
	return (FALSE);
}


/*
 * Sanity check: Make sure every monster is in a single group.
 */
static void knowledge_monsters_sanity(void)
{
	int i, j, r_idx;
	bool exists[MAX_R_IDX];
	int mon_idx[MAX_R_IDX];

	/* Assume each race isn't in a group */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		exists[i] = FALSE;
	}

	/* Check every group */
	for (i = 0; monster_group_text[i] != NULL; i++)
	{
		/* Get every monster in this group (force knowledge) */
		if (collect_monsters(i, mon_idx, 0x02))
		{
			/* Check every monster in the group */
			for (j = 0; mon_idx[j]; j++)
			{
				/* Get the race index */
				r_idx = mon_idx[j];

				/* Disallow more than one group */
				if (exists[r_idx])
					quit_fmt("Race %d in more than one group", r_idx);

				/* Note the race is in a group */
				exists[r_idx] = TRUE;
			}
		}
	}

	/* Check every race (skip ghost) */
	for (i = 1; i < MAX_R_IDX - 1; i++)
	{
		/* Skip empty race */
		if (!r_info[i].name) continue;

		/* Check existence */
		if (!exists[i])
			quit_fmt("Race %d not in any group", i);
	}
}


/*
 * Display known monsters.
 */
void do_cmd_knowledge_monsters(void)
{
	int i, len, max;
	int per_page;
	int grp_cur, grp_top;
	int mon_old, mon_cur, mon_top;
	int grp_cnt, grp_idx[100];
	int mon_cnt, mon_idx[MAX_R_IDX];
	int column = 0;
	bool flag;
	bool redraw;

	char find_ch = '\0', find_name[80] = "";
	int find_mode = 0;

	/* Sanity check */
	knowledge_monsters_sanity();

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; monster_group_text[i] != NULL; i++)
	{
		/* Measure the label */
		len = strlen(monster_group_text[i]);

		/* Save the maximum length */
		if (len > max)
		{
			max = len;
		}

		/* See if any monsters are known */
		if (collect_monsters(i, mon_idx, 0x01))
		{
			/* Build a list of groups with known monsters */
			grp_idx[grp_cnt++] = i;
		}
	}

	/* Terminate the list */
	grp_idx[grp_cnt] = -1;

	per_page = screen_y - 9;

	grp_cur = grp_top = 0;
	mon_cur = mon_top = 0;
	mon_old = -1;

	flag = FALSE;
	redraw = TRUE;

	while (!flag)
	{
		int ch;

		if (redraw)
		{
			clear_from(0);
		
			prt("Knowledge - Monsters", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			prt("Sym", 4, screen_x - 11);
			prt("Kills", 4, screen_x - 5);

			for (i = 0; i < screen_x; i++)
			{
				Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < per_page; i++)
			{
				Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		if (grp_cur < grp_top)
			grp_top = grp_cur;
		if (grp_cur >= grp_top + per_page)
			grp_top = grp_cur - per_page + 1;

		/* Scroll monster list */
		if (mon_cur < mon_top)
			mon_top = mon_cur;
		if (mon_cur >= mon_top + per_page)
			mon_top = mon_cur - per_page + 1;

		/* Display a list of monster groups */
		display_group_list(0, 6, max, per_page, grp_idx, grp_cur, grp_top);

		/* Get a list of monsters in the current group */
		mon_cnt = collect_monsters(grp_idx[grp_cur], mon_idx, 0x00);

		/* Display a list of monsters in the current group */
		display_monster_list(max + 3, 6, per_page, mon_idx, mon_cur, mon_top);

		/* Prompt */
		prt("<dir>, 's' symbol, 'n' name, 'g' repeat, 'r' to recall, ESC", screen_y - 1, 0);

		/* The "current" monster changed */
		if (mon_old != mon_idx[mon_cur])
		{
			/* Hack -- track this monster race */
			monster_race_track(mon_idx[mon_cur]);

			/* Hack -- handle stuff */
			handle_stuff();

			/* Remember the "current" monster */
			mon_old = mon_idx[mon_cur];
		}

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

			case 'S':
			case 's':
			{
				/* Prompt */
				prt("Monster symbol: ", screen_y - 1, 0);

				/* Get a key */
				find_ch = inkey();

				/* Cancel */
				if (find_ch == ESCAPE) break;

				/* Search */
				if (!find_monster(grp_cnt, grp_idx, &grp_cur, &mon_cur,
					1, (long) find_ch))
				{
					msg_print("No match.");
					msg_print(NULL);
					break;
				}

				/* Remember the search mode */
				find_mode = 1;

				/* Focus on monster list */
				column = 1;

				break;
			}

			case 'N':
			case 'n':
			{
				/* Prompt */
				prt("Monster name: ", screen_y - 1, 0);
	
				/* Get a string, or continue */
				if (!askfor_aux(find_name, 80, FALSE)) break;

				/* Search */
				if (!find_monster(grp_cnt, grp_idx, &grp_cur, &mon_cur,
					2, (long) find_name))
				{
					msg_print("No match.");
					msg_print(NULL);
					break;
				}

				/* Remember the search mode */
				find_mode = 2;

				/* Focus on monster list */
				column = 1;

				break;
			}

			case 'G':
			case 'g':
			{
				if (find_mode == 1)
				{
					/* Search */
					if (!find_monster(grp_cnt, grp_idx, &grp_cur, &mon_cur,
						1, (long) find_ch))
					{
						msg_print("No match.");
						msg_print(NULL);
						break;
					}

					/* Focus on monster list */
					column = 1;
				}

				if (find_mode == 2)
				{
					/* Search */
					if (!find_monster(grp_cnt, grp_idx, &grp_cur, &mon_cur,
						2, (long) find_name))
					{
						msg_print("No match.");
						msg_print(NULL);
						break;
					}

					/* Focus on monster list */
					column = 1;
				}

				break;
			}

			case 'R':
			case 'r':
			{
				/* Recall on screen */
				screen_roff(mon_idx[mon_cur], 0);

				(void) inkey();

				redraw = TRUE;
				break;
			}

			default:
			{
				int d;

				/* Extract direction */
				d = target_dir(ch);

				if (!d) break;

				if (ddx[d])
				{
					column += ddx[d];
					if (column < 0) column = 0;
					if (column > 1) column = 1;
					break;
				}

				/* Browse group list */
				if (!column)
				{
					int old_grp = grp_cur;

					/* Move up or down */
					grp_cur += ddy[d];

					/* Verify */
					if (grp_cur < 0)
						grp_cur = 0;
					if (grp_cur >= grp_cnt)
						grp_cur = grp_cnt - 1;

					if (grp_cur != old_grp)
						mon_cur = mon_top = 0;
				}

				/* Browse monster list */
				else
				{
					/* Move up or down */
					mon_cur += ddy[d];

					/* Verify */
					if (mon_cur < 0)
						mon_cur = 0;
					if (mon_cur >= mon_cnt)
						mon_cur = mon_cnt - 1;
				}
				
				break;
			}
		}
	}
}


/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
	int i;


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Save the screen */
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
		prt("(2) Display known uniques", 5, 5);
		prt("(3) Display known monsters", 6, 5);
		prt("(4) Display known objects", 7, 5);
		prt("(5) Display known formulae", 8, 5);
		prt("(6) Display your mutations", 9, 5);
		prt("(7) Display your quests", 10, 5);
		prt("(8) Display learned spells", 11, 5);
		prt("(9) Display character dump", 12, 5);

		/* Prompt */
		prt("Command: ", 14, 0);

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE)
			break;

		/* Artifacts */
		if (i == '1')
		{
			/* Spawn */
			do_cmd_knowledge_artifacts();
		}

		/* Uniques */
		else if (i == '2')
		{
			/* Spawn */
			do_cmd_knowledge_uniques();
		}

		/* Monsters */
		else if (i == '3')
		{
			/* Spawn */
			do_cmd_knowledge_monsters();
		}

		/* Objects */
		else if (i == '4')
		{
			/* Spawn */
			do_cmd_knowledge_objects();
		}

		/* Recipes */
		else if (i == '5')
		{
			/* Spawn */
			do_cmd_knowledge_recipes();
		}

		/* Mutations */
		else if (i == '6')
		{
			/* Spawn */
			do_cmd_knowledge_mutations();
		}

		/* Quests */
		else if (i == '7')
		{
			/* Spawn */
			do_cmd_knowledge_quests();
		}

		/* Spells */
		else if (i == '8')
		{
			/* Spawn */
			do_cmd_knowledge_spells();
		}

		/* Character dump */
		else if (i == '9')
		{
			/* Spawn */
			do_cmd_knowledge_dump();
		}

		/* Unknown option */
		else
		{
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Restore the screen */
	screen_load();
}

/*
 * Display the time and date
 */
void do_cmd_time(void)
{
	s32b len = 10L * TOWN_DAWN;
	s32b tick = turn % len + len / 4;

	int day = turn / len + 1;
	int hour = (24 * tick / len) % 24;
	int min = (1440 * tick / len) % 60;

	/* Message */
	mformat(MSG_TEMP, "This is day %d. The time is %d:%02d %s.", day,
		(hour % 12 == 0) ? 12 : (hour % 12), min,
		(hour < 12) ? "AM" : "PM");
}
