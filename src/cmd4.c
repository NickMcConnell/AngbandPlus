/* File: cmd4.c */

/* Screen refresh, change character name, message recall, interacting
 * with options (inc. text of cheat options), macros, visuals, also level
 * feelings, screen dumps/loading, and known Uniques/Artifacts/Objects/
 * recalling the contents of the home.
 *
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
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

	/* Forget view */
	p_ptr->update |= (PU_FORGET_VIEW);

	/* Update view */
	p_ptr->update |= (PU_UPDATE_VIEW);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

	/* Hack -- update */
	handle_stuff();


	/* Redraw every window */
	for (j = 0; j < TERM_WIN_MAX; j++)
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
 * Map resizing whenever the main term changes size
 */
void resize_map(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;

	/* Mega-Hack -- no panel yet */
	panel_row_min = 0;
	panel_row_max = 0;
	panel_col_min = 0;
	panel_col_max = 0;

	/* Reset the panels */
	map_panel_size();

	if (character_dungeon)
	{
		verify_panel();
	}

	/* Combine and Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

	/* Forget view */
	p_ptr->update |= (PU_FORGET_VIEW);

	/* Update view */
	p_ptr->update |= (PU_UPDATE_VIEW);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Hack -- update */
	handle_stuff();

	/* Redraw */
	Term_redraw();

	/* Refresh */
	Term_fresh();
}

/*
 * Redraw a term when it is resized
 */
void redraw_window(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;

	/* Hack - Activate term zero for the redraw */
	Term_activate(&term_screen[0]);

	/* Hack -- react to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

	/* Hack -- update */
	handle_stuff();

	/* Redraw */
	Term_redraw();

	/* Refresh */
	Term_fresh();
}

/*
 * Hack -- change name
 */
void do_cmd_change_name(void)
{
	char c;

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
		c = inkey();

		/* Exit */
		if (c == ESCAPE) break;

		/* Change name */
		if (c == 'c')
		{
			get_name();
		}

		/* File dump */
		else if (c == 'f')
		{
			char tmp[81];

			sprintf(tmp, "%s.txt", op_ptr->base_name);

			if (get_string("File name: ", tmp, 81))
			{
				if (tmp[0] && (tmp[0] != ' '))
				{
					file_character(tmp, FALSE);
				}
			}
		}

		/* Toggle mode.  Changed back to 2.8.2 format. */
		else if (c == 'h')
		{
			mode++;
		}

		/* Oops */
		else
		{
			bell("Illegal command for change name!");
		}

		/* Flush messages */
		msg_print(NULL);
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
 * Show previous messages to the user -BEN-
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
	s16b i, j, k, n;
	int q;

	char shower[80];
	char finder[80];

	int wid, hgt;

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

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Process requests until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Dump messages */
		for (j = 0; (j < hgt-4) && (i + j < n); j++)
		{
			cptr msg = message_str(i+j);
			byte attr = message_color(i+j);

			/* Hack -- fake monochrome */
			if (fake_monochrome) attr = TERM_WHITE;

			/* Apply horizontal scroll */
			msg = ((int)strlen(msg) >= q) ? (msg + q) : "";

			/* Dump the messages, bottom to top */
			Term_putstr(0, hgt-3 - j, -1, attr, msg);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = msg;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str - msg, hgt-3 - j, len, TERM_YELLOW, shower);

					/* Advance */
					str += len;
				}
			}
		}

		/* Display header XXX XXX XXX */
		prt(format("Message Recall (%d-%d of %d), Offset %d",
		    i, i + j - 1, n, q), 0, 0);

		/* Display prompt (not very informative) */
		prt("[Press 'p' for older, 'n' for newer, ..., or ESCAPE]", hgt-1, 0);

		/* Get a command */
		k = inkey();

		/* Exit on Escape */
		if (k == ESCAPE) break;

		/* Hack -- Save the old index */
		j = i;

		/* Horizontal scroll */
		if (k == '4')
		{
			/* Scroll left */
			q = (q >= wid / 2) ? (q - wid / 2) : 0;

			/* Success */
			continue;
		}

		/* Horizontal scroll */
		if (k == '6')
		{
			/* Scroll right */
			q = q + wid / 2;

			/* Success */
			continue;
		}

		/* Hack -- handle show */
		if (k == '=')
		{
			/* Prompt */
			prt("Show: ", hgt-1, 0);

			/* Get a "shower" string, or continue */
			if (!askfor_aux(shower, 80)) continue;

			/* Okay */
			continue;
		}

		/* Hack -- handle find */
		if (k == '/')
		{
			s16b z;

			/* Prompt */
			prt("Find: ", hgt-1, 0);

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

		/* Recall 1 older message */
		if ((k == '8') || (k == '\n') || (k == '\r'))
		{
			/* Go newer if legal */
			if (i + 1 < n) i += 1;
		}

		/* Recall 10 older messages */
		if (k == '+')
		{
			/* Go older if legal */
			if (i + 10 < n) i += 10;
		}

		/* Recall 20 older messages */
		if ((k == 'p') || (k == KTRL('P')) || (k == ' '))
		{
			/* Go older if legal */
			if (i + 20 < n) i += 20;
		}

		/* Recall 20 newer messages */
		if ((k == 'n') || (k == KTRL('N')))
		{
			/* Go newer (if able) */
			i = (i >= 20) ? (i - 20) : 0;
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
		if (i == j) bell(NULL);
	}

	/* Restore the screen */
	screen_load();
}



/*
 * Autosave options -- textual names
 */
static cptr autosave_text[1] =
{
	"autosave"
};

/*
 * Autosave options -- descriptions
 */
static cptr autosave_desc[1] =
{
	"Timed autosave"
};

s16b toggle_frequency(s16b current)
{
	if (current == 0) return 50;
	if (current == 50) return 100;
	if (current == 100) return 250;
	if (current == 250) return 500;
	if (current == 500) return 1000;
	if (current == 1000) return 2500;
	if (current == 2500) return 5000;
	if (current == 5000) return 10000;
	if (current == 10000) return 25000;

	else return 0;
}


/*
 * Interact with autosave options.  From Zangband.
 */
static void do_cmd_options_autosave(cptr info)
{
	char ch;

	int i, k = 0, n = 1;

	char buf[80];


	/* Clear screen */
	Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
		sprintf(buf, "%s (RET to advance, y/n to set, 'F' for frequency, ESC to accept) ", info);
		prt(buf, 0, 0);

		/* Display the options */
		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%s)",
				autosave_desc[i],
				autosave ? "yes" : "no ",
				autosave_text[i]);
			c_prt(a, buf, i + 2, 0);

			prt(format("Timed autosave frequency: every %d turns", autosave_freq), 5, 0);
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

			case 'y':
			case 'Y':
			case '6':
			{

				autosave = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				autosave = FALSE;
				k = (k + 1) % n;
				break;
			}

			case 'f':
			case 'F':
			{
				autosave_freq = toggle_frequency(autosave_freq);
				prt(format("Timed autosave frequency: every %d turns",
				       autosave_freq), 5, 0);
			}

			default:
			{
				bell("Illegal command for Autosave options!");
				break;
			}
		}
	}
}

/*
 * Ask for a "user pref line" and process it
 *
 * XXX XXX XXX Allow absolute file names?
 */
void do_cmd_pref(void)
{
	char tmp[81];

	/* Default */
	strcpy(tmp, "");

	/* Ask for a "user pref command" */
	if (!get_string("Pref: ", tmp, 81)) return;

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
	if (!askfor_aux(ftmp, 80)) return (FALSE);

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
				for (i = OPT_cheat_start; i < OPT_cheat_end+1; i++)
				{
					if (op_ptr->opt[i])
					{
						/* Set score option */
						op_ptr->opt[OPT_score_start + (i - OPT_cheat_start)] = TRUE;
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

	u32b old_flag[TERM_WIN_MAX];


	/* Memorize old flags */
	for (j = 0; j < TERM_WIN_MAX; j++)
	{
		/* Acquire current flags */
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
		for (j = 0; j < TERM_WIN_MAX; j++)
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
			for (j = 0; j < TERM_WIN_MAX; j++)
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
	for (j = 0; j < TERM_WIN_MAX; j++)
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
	for (i = 0; i < OPT_cheat_start; i++)
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
	for (i = 1; i < 8; i++)
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
 * After using this command, a complete redraw is performed,
 * whether or not it is technically required.
 */
void do_cmd_options(void)
{
	int k;


	/* Save screen */
	screen_save();


	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Why are we here */
		prt("Oangband options", 2, 0);

		/* Give some choices */
		prt("(1) User Interface Options", 4, 5);
		prt("(2) Disturbance Options", 5, 5);
		prt("(3) Game-Play Options", 6, 5);
		prt("(4) Efficiency Options", 7, 5);
		prt("(5) Birth Options - For Character Creation (Only)", 9, 5);

		/* Cheating */
		prt("(6) Cheat Options", 11, 5);

		/* Window flags */
		prt("(W) Window flags", 13, 5);

		/* Squelch Menus */
		prt("(I) Item squelching menus", 14, 5);

		/* Special choices */
		prt("(D) Base Delay Factor", 15, 5);
		prt("(H) Hitpoint Warning", 16, 5);
		prt("(A) Autosave Options", 17, 5);

		/* Load and Save */
		prt("(R) Load a user pref file", 4, 40);
		prt("(S) Append options to a file", 5, 40);

		/* Prompt */
		prt("Command: ", 19, 0);

		/* Get command */
		k = inkey();

		/* Exit */
		if (k == ESCAPE) break;

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

			/* Birth Options */
			case '5':
			{
				/* Spawn */
				do_cmd_options_aux(4, "Birth Options");
				break;
			}

			/* Cheating Options */
			case 'C':
			case '6':
			{
				/* Spawn */
				do_cmd_options_aux(5, "Cheaters never win (seriously!)");
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

			/* Squelching Menus */
			case 'I':
			case 'i':
			{
				/* Spawn */
				do_cmd_squelch();
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
					if (k == ESCAPE) break;
					if (isdigit(k)) op_ptr->delay_factor = D2I(k);
					else bell("Illegal delay factor!");
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
					prt("Hitpoint Warning (0-9 or ESC to accept): ", 20, 0);
					k = inkey();
					if (k == ESCAPE) break;
					if (isdigit(k)) op_ptr->hitpoint_warn = D2I(k);
					else bell("Illegal hitpoint warning!");
				}

				break;

			case 'a':
			case 'A':
			{
				(void) do_cmd_options_autosave("Autosave");
				break;
			}


			}

			case 'r':
			case 'R':
			{
				/* Ask for and load a user pref file */
				(void) do_cmd_pref_file_hack(18);
				break;
			}

			case 's':
			case 'S':
			{
				char ftmp[80];

				/* Prompt */
				prt("Command: Write options to a file", 18, 0);

				/* Prompt */
				prt("File: ", 20, 0);

				/* Default filename */
				sprintf(ftmp, "%s.prf", op_ptr->base_name);

				/* Ask for a file */
				if (!askfor_aux(ftmp, 80)) break;

				/* Drop priv's */
				safe_setuid_drop();

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

				/* Grab priv's */
				safe_setuid_grab();

				break;
			}

			/* Unknown option */
			default:
			{
				/* Oops */
				bell("Illegal command for options!");
				break;
			}
		}

		/* Flush messages */
		msg_print(NULL);
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
	if (!fff) return (-1);


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
		if (!act) continue;

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

	/* Start dumping */
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
		if (i == ESCAPE) break;

		/* Load a 'macro' file */
		else if (i == '1')
		{
#if 1
			/* Ask for and load a user pref file */
			(void) do_cmd_pref_file_hack(16);
#else
			/* Prompt */
			prt("Command: Load a user pref file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(tmp, 80)) continue;

			/* Process the given filename */
			if (0 != process_pref_file(tmp))
			{
				/* Prompt */
				msg_print("Could not load file!");
			}
#endif
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
			if (!askfor_aux(tmp, 80)) continue;

			/* Drop priv's */
			safe_setuid_drop();

			/* Dump the macros */
			(void)macro_dump(tmp);

			/* Grab priv's */
			safe_setuid_grab();

			/* Prompt */
			msg_print("Appended macros.");
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
				msg_print("Found no macro.");
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
				msg_print("Found a macro.");
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
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, tmp);

				/* Link the macro */
				macro_add(buf, macro_buffer);

				/* Prompt */
				msg_print("Added a macro.");
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
			msg_print("Removed a macro.");
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
			if (!askfor_aux(tmp, 80)) continue;

			/* Drop priv's */
			safe_setuid_drop();

			/* Dump the macros */
			(void)keymap_dump(tmp);

			/* Grab priv's */
			safe_setuid_grab();

			/* Prompt */
			msg_print("Appended keymaps.");
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
			act = keymap_act[mode][(byte)(buf[0])];

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
				ascii_to_text(buf, macro_buffer);

				/* Display the current action */
				prt(buf, 22, 0);

				/* Prompt */
				msg_print("Found a keymap.");
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
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, tmp);

				/* Free old keymap */
				string_free(keymap_act[mode][(byte)(buf[0])]);

				/* Make new keymap */
				keymap_act[mode][(byte)(buf[0])] = string_make(macro_buffer);

				/* Prompt */
				msg_print("Added a keymap.");

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
			string_free(keymap_act[mode][(byte)(buf[0])]);

			/* Make new keymap */
			keymap_act[mode][(byte)(buf[0])] = NULL;

			/* Prompt */
			msg_print("Removed a keymap.");

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
			if (!askfor_aux(buf, 80)) continue;

			/* Extract an action */
			text_to_ascii(macro_buffer, buf);
		}

#endif /* ALLOW_MACROS */

		/* Oops */
		else
		{
			/* Oops */
			bell("Illegal command for macros!");
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
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

		/* Load a 'pref' file */
		else if (i == '1')
		{
#if 1
			/* Ask for and load a user pref file */
			(void) do_cmd_pref_file_hack(15);
#else
			/* Prompt */
			prt("Command: Load a user pref file", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Query */
			if (!askfor_aux(tmp, 80)) continue;

			/* Process the given filename */
			(void)process_pref_file(tmp);
#endif
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
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(tmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff) continue;

			/* Start dumping */
			fprintf(fff, "\n\n");
			fprintf(fff, "# Monster attr/char definitions\n\n");

			/* Dump monsters */
			for (i = 0; i < MAX_R_IDX; i++)
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
		else if (i == '3')
		{
			/* Prompt */
			prt("Command: Dump object attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(tmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff) continue;

			/* Start dumping */
			fprintf(fff, "\n\n");
			fprintf(fff, "# Object attr/char definitions\n\n");

			/* Dump objects */
			for (i = 0; i < MAX_K_IDX; i++)
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
		else if (i == '4')
		{
			/* Prompt */
			prt("Command: Dump feature attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(tmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff) continue;

			/* Start dumping */
			fprintf(fff, "\n\n");
			fprintf(fff, "# Feature attr/char definitions\n\n");

			/* Dump features */
			for (i = 0; i < MAX_F_IDX; i++)
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
		else if (i == '6')
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
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				/* Analyze */
				if (i == 'n') r = (r + MAX_R_IDX + 1) % MAX_R_IDX;
				if (i == 'N') r = (r + MAX_R_IDX - 1) % MAX_R_IDX;
				if (i == 'a') r_ptr->x_attr = (byte)(ca + 1);
				if (i == 'A') r_ptr->x_attr = (byte)(ca - 1);
				if (i == 'c') r_ptr->x_char = (byte)(cc + 1);
				if (i == 'C') r_ptr->x_char = (byte)(cc - 1);
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

				byte da = (byte)k_ptr->d_attr;
				byte dc = (byte)k_ptr->d_char;
				byte ca = (byte)k_ptr->x_attr;
				byte cc = (byte)k_ptr->x_char;

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
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				/* Analyze */
				if (i == 'n') k = (k + MAX_K_IDX + 1) % MAX_K_IDX;
				if (i == 'N') k = (k + MAX_K_IDX - 1) % MAX_K_IDX;
				if (i == 'a') k_info[k].x_attr = (byte)(ca + 1);
				if (i == 'A') k_info[k].x_attr = (byte)(ca - 1);
				if (i == 'c') k_info[k].x_char = (byte)(cc + 1);
				if (i == 'C') k_info[k].x_char = (byte)(cc - 1);
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

				byte da = (byte)f_ptr->d_attr;
				byte dc = (byte)f_ptr->d_char;
				byte ca = (byte)f_ptr->x_attr;
				byte cc = (byte)f_ptr->x_char;

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
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				/* Analyze */
				if (i == 'n') f = (f + MAX_F_IDX + 1) % MAX_F_IDX;
				if (i == 'N') f = (f + MAX_F_IDX - 1) % MAX_F_IDX;
				if (i == 'a') f_info[f].x_attr = (byte)(ca + 1);
				if (i == 'A') f_info[f].x_attr = (byte)(ca - 1);
				if (i == 'c') f_info[f].x_char = (byte)(cc + 1);
				if (i == 'C') f_info[f].x_char = (byte)(cc - 1);
			}
		}

#endif

		/* Reset visuals */
		else if (i == '0')
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
		msg_print(NULL);
	}


	/* Load screen */
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
#endif

		/* Prompt */
		prt("Command: ", 8, 0);

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

		/* Load a 'pref' file */
		if (i == '1')
		{
#if 1
			/* Ask for and load a user pref file */
			if (!do_cmd_pref_file_hack(8)) break;

			/* Mega-Hack -- react to changes */
			Term_xtra(TERM_XTRA_REACT, 0);

			/* Mega-Hack -- redraw */
			Term_redraw();
#else
			/* Prompt */
			prt("Command: Load a user pref file", 8, 0);

			/* Prompt */
			prt("File: ", 10, 0);

			/* Default file */
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Query */
			if (!askfor_aux(tmp, 80)) continue;

			/* Process the given filename */
			(void)process_pref_file(tmp);

			/* Mega-Hack -- react to changes */
			Term_xtra(TERM_XTRA_REACT, 0);

			/* Mega-Hack -- redraw */
			Term_redraw();
#endif
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
			sprintf(tmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(tmp, 80)) continue;

			/* Build the filename */
			path_build(buf, 1024, ANGBAND_DIR_USER, tmp);

			/* Drop priv's */
			safe_setuid_drop();

			/* Append to the file */
			fff = my_fopen(buf, "a");

			/* Grab priv's */
			safe_setuid_grab();

			/* Failure */
			if (!fff) continue;

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
		else if (i == '3')
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
				i = inkey();

				/* All done */
				if (i == ESCAPE) break;

				/* Analyze */
				if (i == 'n') a = (byte)(a + 1);
				if (i == 'N') a = (byte)(a - 1);
				if (i == 'k') angband_color_table[a][0] = (byte)(angband_color_table[a][0] + 1);
				if (i == 'K') angband_color_table[a][0] = (byte)(angband_color_table[a][0] - 1);
				if (i == 'r') angband_color_table[a][1] = (byte)(angband_color_table[a][1] + 1);
				if (i == 'R') angband_color_table[a][1] = (byte)(angband_color_table[a][1] - 1);
				if (i == 'g') angband_color_table[a][2] = (byte)(angband_color_table[a][2] + 1);
				if (i == 'G') angband_color_table[a][2] = (byte)(angband_color_table[a][2] - 1);
				if (i == 'b') angband_color_table[a][3] = (byte)(angband_color_table[a][3] + 1);
				if (i == 'B') angband_color_table[a][3] = (byte)(angband_color_table[a][3] - 1);

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
			bell("Illegal command for colors!");
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Load screen */
	screen_load();
}



/*
 * Note something in the message recall
 */
void do_cmd_note(void)
{
	char tmp[81];

	/* Default */
	strcpy(tmp, "");

	/* Input */
	if (!get_string("Note: ", tmp, 81)) return;

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
	msg_format("You are playing Oangband %d.%d.%d.  Type '?' for more info.",
		   O_VERSION_MAJOR, O_VERSION_MINOR, O_VERSION_PATCH);
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
	"This seems a very quiet place..."
};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(void)
{
	/* Verify the feeling */
	if (feeling < 0) feeling = 0;
	if (feeling > 10) feeling = 10;

	/* No useful feeling in town */
	if (!p_ptr->depth)
	{
		msg_print("Looks like a typical town.");
		return;
	}

	/* Display the feeling */
	if (p_ptr->themed_level) msg_format("%s", themed_feeling);
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

	msg_format("%^s, the %^s %s", ghost_name, r_name + r_ptr->name,
		do_cmd_challenge_text[rand_int(14)]);
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


	/* Ask for a file (or cancel) */
	(void) strcpy(tmp_val, "dump.txt");

	/* Prompt */
	prt("File: ", 0, 0);

	/* Ask for a file */
	if (!askfor_aux(tmp_val, 80)) return;

	/* Hack -- Erase prompt */
	prt("", 0, 0);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, tmp_val);

	/* Append to the file */
	fff = my_fopen(buf, "r");

	/* Oops */
	if (!fff) return;


	/* Save screen */
	screen_save();


	/* Clear the screen */
	Term_clear();


	/* Load the screen */
	for (y = 0; okay; y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, 1024)) okay = FALSE;

		/* Stop on blank line */
		if (!buf[0]) break;

		/* Get the width */
		len = strlen(buf);

		/* XXX Restrict to current screen size */
		if (len >= Term->wid) len = Term->wid;

		/* Show each row */
		for (x = 0; x < len; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Get the blank line */
	/* if (my_fgets(fff, buf, 1024)) okay = FALSE; */


	/* Load the screen */
	for (y = 0; okay; y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, 1024)) okay = FALSE;

		/* Stop on blank line */
		if (!buf[0]) break;

		/* Get the width */
		len = strlen(buf);

		/* XXX Restrict to current screen size */
		if (len >= Term->wid) len = Term->wid;

		/* Show each row */
		for (x = 0; x < len; x++)
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

		/* End the row */
/*		fprintf(fff, "\n"); */
	}


	/* Get the blank line */
	/* if (my_fgets(fff, buf, 1024)) okay = FALSE; */


	/* Close it */
	my_fclose(fff);


	/* Message */
	msg_print("Screen dump loaded.");
	msg_print(NULL);


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

	char tmp_val[80], buf[1024];


	/* Ask for a file (or cancel) */
	(void) strcpy(tmp_val, "dump.txt");

	/* Prompt */
	prt("File: ", 0, 0);

	/* Ask for a file */
	if (!askfor_aux(tmp_val, 80)) return;

	/* Hack -- Erase prompt */
	prt("", 0, 0);

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
	if (!fff) return;


	/* Save screen */
	screen_save();


	/* Dump the screen */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Dump each row */
		for (x = 0; x < Term->wid; x++)
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
	for (y = 0; y < Term->hgt; y++)
	{
		/* Dump each row */
		for (x = 0; x < Term->wid; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

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
	msg_print("Screen dump saved.");
	msg_print(NULL);


	/* Load screen */
	screen_load();
}


/* used for knowledge display */
#define BROWSER_ROWS			16


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
 * Display the object groups.
 */
static void display_object_group_list(int col, int row, int wid, int per_page,
	int grp_idx[], grouper group_item_list[], int grp_cur, int grp_top)
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
		c_put_str(attr, group_item_list[new_group_index[grp]].name, row + i, col);
	}
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

	/* make a list of artifacts not found */
	/* Allocate the "object_idx" array */
	C_MAKE(okay, MAX_A_IDX, bool);

	/* Default first,  */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* start with false */
		okay[i] = FALSE;

		/* Skip "empty" artifacts */
		if (a_ptr->tval + a_ptr->sval == 0) continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->creat_stat) continue;

		/* assume all created artifacts are good at this point */
		okay[i] = TRUE;
	}

	/* Process objects in the dungeon */
	for (i = 1; i < o_max; i++)
	{
		/* get the object */
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

		/* Nothing there */
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
		/* go through each item in the house */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Point to the item */
			object_type *o_ptr = &st_ptr->stock[i];

			/* Nothing there */
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
		for (i = 0; i < MAX_A_IDX; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			/* Skip "empty" artifacts */
			if (a_ptr->tval + a_ptr->sval == 0) continue;

			/* assume all created artifacts are good at this point */
			okay[i] = TRUE;
		}
	}


	/* Finally, go through the list of artifacts and categorize the good ones */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		byte group_tval;
		int j;

		/* Access the artifact */
		artifact_type *a_ptr = &a_info[i];

		/* Skip empty artifacts */
		if (a_ptr->tval + a_ptr->sval == 0) continue;

		/* Require artifacts ever seen */
		if (okay[i] == FALSE) continue;

		/* Get a list of x_char in this group */
		/* Aggregate tvals in this group */
		for (j = new_group_index[grp_cur];
		     j < new_group_index[grp_cur + 1];
		     j++)
		{
			group_tval = group_item[j].tval;

			/* Check for artifact in the group */
			if (a_ptr->tval == group_tval)
			{
				/* Add the artifact */
				object_idx[object_cnt++] = i;
			}
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* clear the array */
	C_KILL(okay, MAX_A_IDX, bool);

	/* Return the number of races */
	return object_cnt;
}


/*
 * Display the objects in a group.
 */
static void display_artifact_list(int col, int row, int per_page, int object_idx[],
	int object_cur, int object_top)
{
	int i, z;
	char o_name[80];
	object_type *i_ptr;
	object_type object_type_body;

	/* Display lines until done */
	for (i = 0; i < per_page && object_idx[i]; i++)
	{
		/* Get the object index */
		int a_idx = object_idx[object_top + i];
		artifact_type *a_ptr = &a_info[a_idx];


		/* Choose a color */
		byte attr = TERM_WHITE;
		byte cursor = TERM_L_BLUE;
		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain the base object type */
		z = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Wipe the object */
		object_prep(i_ptr, z);

		/* Paranoia */
		strcpy(o_name, "Unknown Artifact");

		/* Make it an artifact */
		i_ptr->name1 = a_idx;

		/* Describe the artifact */
		object_desc_spoil(o_name, i_ptr, TRUE, 0);

		/* Display the name */
		c_prt(attr, o_name, row + i, col);

		if (cheat_know)
		{
			c_prt(attr, format("%3d", a_idx), row + i, 68);
			c_prt(attr, format("%3d", a_ptr->level), row + i, 72);
			c_prt(attr, format("%3d", a_ptr->rarity), row + i, 76);
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
	C_MAKE(artifact_idx, MAX_A_IDX, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; new_group_index[i] >= 0; i++)
	{
		/* Measure the label */
		len = strlen(group_item[new_group_index[i]].name);

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
		display_object_group_list(0, 6, max, BROWSER_ROWS, grp_idx, group_item, grp_cur, grp_top);

		/* Get a list of objects in the current group */
		artifact_cnt = collect_artifacts(grp_idx[grp_cur], artifact_idx);

		/* Display a list of objects in the current group */
		display_artifact_list(max + 3, 6, BROWSER_ROWS, artifact_idx, artifact_cur, artifact_top);

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

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &artifact_cur, artifact_cnt);
				break;
			}
		}
	}

	/* XXX XXX Free the "object_idx" array */
	C_KILL(artifact_idx, MAX_A_IDX, int);
}


typedef struct monster_list_entry monster_list_entry;
/*
 * Structure for building monster "lists"
 */
struct monster_list_entry
{
	s16b r_idx;			/* Monster race index */

	byte amount;
};


typedef struct monster_group_info monster_group_info;
/*
 * Structure for building monster "lists"
 */
struct monster_group_info
{
	cptr symbol;			/* Monster race symbol */
	cptr text;			/* Monster race text */
};


/*
 * Description of each monster group.
 */
static monster_group_info monster_group_array[] =
{
	{ (char *) -1L, "Uniques" },			/*All uniques, all letters*/
	{ "a", "Ants" },  						/*'a'*/
	/*Unused*/						/*'A'*/
	{ "b", "Bats" },							/*'b'*/
	{ "B", "Birds" },						/*'B'*/
	{ "c", "Centipedes" },					/*'c'*/
	{ "C", "Canines" },						/*'C'*/
	{ "d", "Dragons" },						/*'d'*/
	{ "D", "Ancient Dragons/Wyrms" },		/*'D'*/
	{ "e", "Floating Eyes" },				/*'e'*/
	{ "E", "Elementals" },					/*'E'*/
	{ "f", "Felines" },						/*'f'*/
	{ "F", "Dragon Flies" },					/*'F'*/
	{ "g", "Golems" },						/*'g'*/
	{ "G", "Ghosts" },						/*'G'*/
	{ "h", "Humanoids" },		/*'h'*/
	{ "H", "Hybrids" },						/*'H'*/
	{ "i", "Icky Things" },					/*'i'*/
	{ "I", "Insects" },						/*'I'*/
	{ "j", "Jellies" },						/*'j'*/
	{ "J", "Snakes" },						/*'J'*/
	{ "k", "Kobolds" },						/*'k'*/
	{ "K", "Killer Beetles" },				/*'K'*/
	{ "l", "Louses" },						/*'l'*/
	{ "L", "Lichs" },						/*'L'*/
	{ "m", "Molds" },						/*'m'*/
	{ "N", "Multi-Headed Reptiles" },		/*'M'*/
	{ "n", "Nagas" },						/*'n'*/
	/*Unused*/						/*'N'*/
	{ "o", "Orcs" },							/*'o'*/
	{ "O", "Ogres" },						/*'O'*/
	{ "p", "People/Humans" },				/*'p'*/
	{ "P", "Giant Humanoids" },				/*'P'*/
	{ "q", "Quadrupeds" },					/*'q'*/
	{ "Q", "Quylthulgs" },					/*'Q'*/
	{ "r", "Rodents" },						/*'r'*/
	{ "R", "Reptiles/Amphibians" },			/*'R'*/
	{ "s", "Skeletons" },					/*'s'*/
	{ "S", "Spiders/Scorpions/Ticks" },		/*'S'*/
	{ "t", "Townpersons" },					/*'t'*/
	{ "T", "Trolls" },						/*'T'*/
	{ "u", "Minor Demons" },					/*'u'*/
	{ "U", "Major Demons" },					/*'U'*/
	{ "v", "Vortices" },						/*'v'*/
	{ "V", "Vampires" },						/*'V'*/
	{ "w", "Worms/Worm Masses" },			/*'w'*/
	{ "W", "Wight/Wraith/etc" },				/*'W'*/
	/*Unused*/						/*'x'*/
	{ "X", "Xorn/Xaren/etc" },				/*'X'*/
	{ "y", "Yeeks" },						/*'y'*/
	{ "Y", "Yetis" },						/*'Y'*/
	{ "z", "Zombies/Mummies" },				/*'z'*/
	{ "X", "Zephyr Hounds" },				/*'Z'*/
	{ ",", "Mushroom Patches" },				/*','*/
	{ "$!?=~", "Mimics" },						/*'$!?=._-*/
	{ NULL, NULL }
};


/*
 * Display the monster groups.
 */
static void display_monster_group_list(int col, int row, int wid, int per_page,
	int grp_idx[], monster_group_info group_array[], int grp_cur, int grp_top)
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
		c_put_str(attr, group_array[grp].text, row + i, col);
	}
}


/*
 * Build a string describing a monster race, currently used for quests.
 *
 * Assumes a singular monster.  This may need to be run through the
 * plural_aux function in the quest.c file.  (Changes "wolf" to
 * wolves, etc.....)
 *
 * I am assuming that no monster name is more than 65 characters long,
 * so that "char desc[80];" is sufficiently large for any result, even
 * when the "offscreen" notation is added.
 *
 */
void monster_desc_race(char *desc, size_t max, int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	cptr name = (r_name + r_ptr->name);

	/* Write the name */
	my_strcpy(desc, name, max);
}


/*
 * Build a list of monster indexes in the given group. Return the number
 * of monsters in the group.
 */
static int collect_monsters(int grp_cur, monster_list_entry *mon_idx, int mode)
{
	int i, mon_count = 0;

	/* Get a list of x_char in this group */
	cptr group_char = monster_group_array[grp_cur].symbol;

	/* XXX Hack -- Check if this is the "Uniques" group */
	bool grp_unique = (monster_group_array[grp_cur].symbol == (char *) -1L);

	/* Check every race */
	for (i = 0; i < MAX_R_IDX; i++)
	{
		/* Access the race */
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];
		char race_name[80];

		/* race name */
		monster_desc_race(race_name, sizeof(race_name), i);

		/* Is this a unique? */
		bool unique = (r_ptr->flags1 & (RF1_UNIQUE));

		/* Skip empty race */
		if (!r_ptr->name) continue;
		if (strcmp(race_name, "()") == 0) continue;

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

	/* Start with 0 kills */
	known_uniques = dead_uniques = slay_count = 0;

	/* Count up monster kill counts */
	for (i = 1; i < MAX_R_IDX - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Require non-unique monsters */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			/* No active player ghosts */
			if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && (r_ptr->cur_num == 0)) continue;

			/* Count if we have seen the unique */
			if (l_ptr->sights)
			{
				known_uniques++;

				/* Count if the unique is dead */
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

		/* Handle player ghosts differently */
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

		/* Get the monster race name (singular) */
		else monster_desc_race(race_name, sizeof(race_name), r_idx);

		/* Choose a color */
		attr = ((i + mon_top == mon_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Display the name */
		c_prt(attr, race_name, row + i, col);

		if (cheat_know)
		{
			c_prt(attr, format("%d", r_idx), row + i, 60);
		}

		/* Display symbol */
		Term_putch(68, row + i, r_ptr->x_attr, r_ptr->x_char);

		/* Display kills */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* use alive/dead for uniques */
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

	/* Clear the monster count line */
	Term_erase(0, 22, 255);

	if (monster_group_array[grp_cur].symbol != (char *) -1L)
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
	C_MAKE(mon_idx, MAX_R_IDX, monster_list_entry);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; monster_group_array[i].text != NULL; i++)
	{
		/* Measure the label */
		len = strlen(monster_group_array[i].text);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any monsters are known */
		if ((monster_group_array[i].symbol == ((char *) -1L)) || collect_monsters(i, mon_idx, 0x01))
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
		display_monster_group_list(0, 6, max, BROWSER_ROWS, grp_idx, monster_group_array, grp_cur, grp_top);

		/* Get a list of monsters in the current group */
		monster_count = collect_monsters(grp_idx[grp_cur], mon_idx, 0x00);

		/* Display a list of monsters in the current group */
		display_monster_list(max + 3, 6, BROWSER_ROWS, mon_idx, mon_cur, mon_top, grp_cur);

		/* Track selected monster, to enable recall in sub-win */
		p_ptr->monster_race_idx = mon_idx[mon_cur].r_idx;

		/* Prompt */
		prt("<dir>, 'r' to recall, ESC", 23, 0);

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

				/* Update to a new monster */
				p_ptr->window |= (PW_MONSTER);

				break;
			}
		}
	}

	/* XXX XXX Free the "mon_idx" array */
	C_KILL(mon_idx, MAX_R_IDX, monster_list_entry);
}


/*
 * Build a list of objects indexes in the given group. Return the number
 * of objects in the group.
 */
static int collect_objects(int grp_cur, int object_idx[])
{
	int i, j, k, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval;

	/* Check every object */
	for (i = 0; i < MAX_K_IDX; i++)
	{
		/* Access the object type */
		object_kind *k_ptr = &k_info[i];

		/* used to check for allocation */
		k = 0;

		/* Skip empty objects */
		if (!k_ptr->name) continue;

		/* Skip items with no distribution (including special artifacts) */
		/* Scan allocation pairs */
		for (j = 0; j < 4; j++)
		{
			/* add the rarity, if there is one */
			k += k_ptr->chance[j];
		}

		/* not in allocation table */
		if (!(k)) continue;

		/* Require objects ever seen */
		if (!(k_ptr->aware || cheat_know)) continue;

		/* Get a list of x_char in this group */
		/* Aggregate tvals in this group */
		for (j = new_group_index[grp_cur];
		     j < new_group_index[grp_cur + 1];
		     j++)
		{
			group_tval = group_item[j].tval;

			/* Check for artifact in the group */
			if (k_ptr->tval == group_tval)
			{
				/* Add the artifact */
				object_idx[object_cnt++] = i;
			}
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Return the number of object types */
	return object_cnt;
}


/*
 * Display the objects in a group.
 */
static void display_object_list(int col, int row, int per_page, int object_idx[],
	int object_cur, int object_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && (object_idx[i] != 0); i++)
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

		/* Acquire the basic "name" of the object */
		strip_name(buf, k_idx);

		/* Display the name */
		c_prt(attr, buf, row + i, col);

		if (cheat_know) c_prt(attr, format("%d", k_idx), row + i, 70);

		if (k_ptr->aware)
		{
			/* Obtain attr/char */
			byte a = k_ptr->flavor ? k_ptr->flavor : k_ptr->d_attr;
			byte c = k_ptr->d_char;

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
	C_MAKE(object_idx, MAX_K_IDX, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; group_item[new_group_index[i]].name != NULL; i++)
	{
		/* Measure the label */
		len = strlen(group_item[new_group_index[i]].name);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any objects are known */
		if (collect_objects(i, object_idx))
		{
			/* Build a list of groups with known objects */
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

		/* Scroll object list */
		if (object_cur < object_top) object_top = object_cur;
		if (object_cur >= object_top + BROWSER_ROWS) object_top = object_cur - BROWSER_ROWS + 1;

		/* Display a list of object groups */
		display_object_group_list(0, 6, max, BROWSER_ROWS, grp_idx, group_item, grp_cur, grp_top);

		/* Get a list of objects in the current group */
		object_cnt = collect_objects(grp_idx[grp_cur], object_idx);

		/* Display a list of objects in the current group */
		display_object_list(max + 3, 6, BROWSER_ROWS, object_idx, object_cur, object_top);

		/* Mega Hack -- track this object type */
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

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &object_cur, object_cnt);
				break;
			}
		}
	}

	/* XXX XXX Free the "object_idx" array */
	C_KILL(object_idx, MAX_K_IDX, int);
}


/*
 * Display contents of the Home.
 *
 * Contents moved to store.c
 */
static void do_cmd_knowledge_home(void)
{
	display_home_inventory_remote();
}


/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
	int i;


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
		prt("(4) Display contents of your home", 7, 5);

		/* Prompt */
		prt("Command: ", 9, 0);

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

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
			do_cmd_knowledge_monsters();
		}

		/* Objects */
		else if (i == '3')
		{
			/* Spawn */
			do_cmd_knowledge_objects();
		}

		/* The Home. */
		else if (i == '4')
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
		msg_print(NULL);
	}


	/* Load screen */
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
	msg_format("This is day %d. The time is %d:%02d %s.", day,
		(hour % 12 == 0) ? 12 : (hour % 12), min,
		(hour < 12) ? "AM" : "PM");
}



