
/* File: cmd4.c */

/*
 * Redraw the screen, change character name, display previous messages.
 * Interact with options.  Code to generate preference files.  Interact
 * with macros, visuals, colors.  Take notes, display level feeling and
 * quests, save and load screen dumps.  Interact with the character
 * knowledge menu (Display score, known objects, monsters, and artifacts,
 * contents of the home, kill count, and quests).
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
	p_ptr->update |= (PU_BONUS | PU_SCORE | PU_HP | PU_MANA | PU_SPELLS);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1 |
		PW_M_LIST);

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

	int old_rows = screen_rows;

	cptr p;

	/* Prompt */
	p = "['c': change name, 'h': change mode, 'f': save a char dump, '?': help, or ESC]";


	/* Save screen */
	screen_save();

	/* Set to 25 screen rows */
	Term_rows(FALSE);

	/* Make sure everything is properly updated */
	p_ptr->update |= (PU_BONUS);
	update_stuff();


	/* Forever */
	while (TRUE)
	{
		/* Display the player */
		display_player(mode);

		/* Prompt */
		Term_putstr(0, 23, -1, TERM_WHITE, p);

		/* Query */
		ch = inkey();

		/* Exit */
		if (ch == ESCAPE) break;
		if (ch == '\n') break;
		if (ch == '\r') break;

		/* Change name */
		if (ch == 'c')
		{
			get_name();

			/* Redraw the name (later) */
			p_ptr->redraw |= (PR_NAME);
		}

		/* File dump */
		else if (ch == 'f')
		{
			char ftmp[80];

			sprintf(ftmp, "%s.txt", op_ptr->base_name);

			if (get_string("File name:", ftmp, 80))
			{
				if (ftmp[0] && (ftmp[0] != ' '))
				{
					if (file_character(ftmp, FALSE))
					{
						/* Clean up the player display */
						display_player(mode);

						msg_print("Character dump failed!");
					}
					else
					{
						/* Clean up the player display */
						display_player(mode);

						/* Prompt */
						msg_format("Character dump saved in the \"%s\" directory.",
								ANGBAND_DIR_USER);
					}
				}
			}
		}

		/* Toggle mode */
		else if (ch == 'h')
		{
			mode++;
			if (mode == 4) mode = 0;
		}

		/* Get help */
		else if (ch == '?')
		{
			/* Help depends on mode */
			if ((mode == 0) || (mode == 1))
				p_ptr->get_help_index = HELP_CHAR_SCREEN_MAIN;
			else if (mode == 2)
				p_ptr->get_help_index = HELP_CHAR_SCREEN_FLAGS;
			else if (mode == 3)
				p_ptr->get_help_index = HELP_CHAR_SCREEN_COMBAT;

			/* Help me. */
			do_cmd_help();
		}


		/* Oops */
		else
		{
			bell("Illegal command for character information screens!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
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
 * The screen format uses the top and bottom lines for headers and prompts,
 * skips the lines adjacent to them, and uses the rest to display messages.
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
	char ch;

	int old_rows = screen_rows;

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


	/* We want 50 rows */
	if (text_50_rows) Term_rows(TRUE);

	/* We don't */
	else Term_rows(FALSE);

	/* Note height of screen */
	hgt = screen_rows;


	/* Process requests until done */
	while (TRUE)
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

			/* Highlight "shower" */
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
		if ((ch == '4') || (ch == 'h'))
		{
			/* Scroll left */
			q = (q >= wid / 2) ? (q - wid / 2) : 0;

			/* Success */
			continue;
		}

		/* Horizontal scroll */
		if ((ch == '6') || (ch == 'l'))
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
		if ((ch == 'p') || (ch == KTRL('P')) || (ch == ' ') || (ch == '3'))
		{
			/* Go older if legal */
			if (i + 20 < n) i += 20;
		}

		/* Recall 10 older messages */
		if ((ch == '+') || (ch == '='))
		{
			/* Go older if legal */
			if (i + 10 < n) i += 10;
		}

		/* Recall 1 older message */
		if ((ch == '8') || (ch == '\n') || (ch == '\r') || (ch == 'k'))
		{
			/* Go newer if legal */
			if (i + 1 < n) i += 1;
		}

		/* Recall 20 newer messages */
		if ((ch == 'n') || (ch == KTRL('N')) || (ch == '9'))
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
		if ((ch == '2') || (ch == 'j'))
		{
			/* Go newer (if able) */
			i = (i >= 1) ? (i - 1) : 0;
		}

		/* Go to a specific line */
		if (ch == '#')
		{
			char tmp[80];
			prt("Goto Line: ", hgt - 1, 0);
			strcpy(tmp, "0");
			if (askfor_aux(tmp, 80))
			{
				i = atoi(tmp);
				if (i > n - hgt + 4) i = n - hgt + 4;
				if (i < 0) i = 0;
			}
		}

		/* Go to the top */
		if (ch == 'G')
		{
			i = 0;
		}

		/* Go to the bottom */
		if (ch == 'g')
		{
			i = n - hgt + 4;
		}

		/* Hack -- Error of some kind */
		if (i == j) bell(NULL);
	}


	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
	}

	/* Set to 25 rows, if we were showing 25 before */
	else
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(FALSE);
	}


	/* Load screen */
	screen_load();
}


/*
 * Print the navigation bar for screen options
 */
static void prt_options_screen_navigation(int row, int set)
{
	/* First set */
	c_prt((set == 0 ? TERM_YELLOW : TERM_SLATE), "Windows", row, 1);

	/* Second set */
	c_prt((set == 1 ? TERM_YELLOW : TERM_SLATE), "Layout", row, 11);

	/* Third set */
	c_prt((set == 2 ? TERM_YELLOW : TERM_SLATE), "Left Panel", row, 20);

	/* Instructions */
	c_prt(TERM_WHITE, "'<' for previous, '>' for next set of options", row, 33);
}

/*
 * Modify the "window" options
 *
 * Return 0 if we want to leave, 1 if we want to advance to the next
 * option set, and -1 if we want to retreat to the previous option set.
 */
static s16b do_cmd_options_win(bool *modified)
{
	int i, j, d;
	int action = 0;

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
	while (TRUE)
	{
		/* Prompt */
		prt("Window flags (<dir> to move, 't' to toggle, or ESC)", 0, 0);

		/* Print the navigation bar -- highlight set #0 */
		prt_options_screen_navigation(screen_rows - 1, 0);

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
				char c = '.';

				/* Use color */
				if ((i == y) && (j == x)) a = TERM_L_BLUE;
				else a = TERM_WHITE;

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
		if (ch == ESCAPE)
		{
			action = 0;
			break;
		}

		/* Switch to next option set */
		if ((ch == '\t') || (ch == '>'))
		{
			action = 1;
			break;
		}

		/* Switch to previous option set */
		if (ch == '<')
		{
			action = -1;
			break;
		}

		/* Help me. */
		if (ch == '?')
		{
			show_file("options.txt#window flags:", NULL, 0, 1);
			Term_clear();

			/* Continue */
			continue;
		}

		/* Determine if we've used a direction key */
		d = target_dir(ch);

		/* Move, if able */
		if (d != 0)
		{
			x = (x + ddx[d] +  8) %  8;
			y = (y + ddy[d] + 16) % 16;
		}

		/* Not a direction -- Toggle */
		else
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

		/* Note change */
		*modified = TRUE;
	}

	/* Return */
	return (action);
}



/*
 * Toggle amount of space used to display the map
 */
static void toggle_map_rows(bool increment)
{
	int block = BLOCK_HGT;

	/* Change map rows */
	if (increment) map_rows += BLOCK_HGT;
	else           map_rows -= BLOCK_HGT;

	/* Require that map rows be divisible by BLOCK_HGT (currently 11) */
	if (map_rows % (block)) map_rows = map_rows * BLOCK_HGT / BLOCK_HGT;

	/* Enforce limits */
	if (map_rows > 4 * BLOCK_HGT) map_rows = 4 * BLOCK_HGT;
	if (map_rows < 2 * BLOCK_HGT) map_rows = 2 * BLOCK_HGT;
}

/*
 * Modify the main screen layout
 *
 * Return 0 if we want to leave, 1 if we want to advance to the next
 * option set, and -1 if we want to retreat to the previous option set.
 */
static s16b do_cmd_options_layout(bool *modified, bool *screen_change)
{
	int action = 0;
	char ch;

	int i, k = 0, n = 5;

	char buf[80];



	/* Interact with the player */
	while (TRUE)
	{
		/* Get best minimum view distance */
		int max_view_y = (map_rows   - PANEL_HGT) / 2;
		int max_view_x = (SCREEN_WID - PANEL_WID) / 2;

		/* Automatically correct minimum view distance */
		if (clear_y > max_view_y) clear_y = max_view_y;
		if (clear_x > max_view_x) clear_x = max_view_x;

		/* Clear screen */
		Term_clear();

		/* Prompt XXX XXX XXX */
		prt("Screen Layout (RET to advance, y/n to set, '+/-' to adjust number, or ESC) ", 0, 0);

		/* Print the navigation bar -- highlight set #1 */
		prt_options_screen_navigation(screen_rows - 1, 1);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k)
			{
				a = TERM_L_BLUE;
			}

			if (i == 0)
			{
				/* Display the option text - force_25_rows */
				sprintf(buf, "%-46s: %s     (%s)",
					"Display the main window in 25 rows",
					force_25_rows ? "yes" : "no ", "force_25_rows");
				c_prt(a, buf, i + 2, 0);
			}

			if (i == 1)
			{
				/* Display the option text - text_in_50_rows */
				sprintf(buf, "%-46s: %s     (%s)",
					"Display more things (like text) in 50 rows",
					text_50_rows ? "yes" : "no ", "text_50_rows");
				c_prt(a, buf, i + 2, 0);
			}

			if (i == 2)
			{
				/* Display the option text - dungeon rows */
				sprintf(buf, "%-46s: %2d      (%s)",
					"Number of rows used to display the dungeon",
					map_rows, "map_rows");
				c_prt(a, buf, i + 2, 0);
			}

			if (i == 3)
			{
				/* Display the option text - vertical view */
				sprintf(buf, "%-46s: %2d      (%s)",
					"Minimum vertical view distance",
					clear_y, "clear_y");
				c_prt(a, buf, i + 2, 0);
			}

			if (i == 4)
			{
				/* Display the option text - horizontal view */
				sprintf(buf, "%-46s: %2d      (%s)",
					"Minimum horizontal view distance",
					clear_x, "clear_x");
				c_prt(a, buf, i + 2, 0);
			}

			/* Special explanatory text */
			if (i == k)
			{
				Term_gotoxy(10, 10);

				if (k == 1)
				{
				}

				if (k == 2)
				{
				}

				if (k == 3)
				{
					c_roff(a, format("The maximum vertical view for the way you have currently set up the screen is %d.\n\n     The traditional vertical view is 2, and with bigscreen about two or three below the maximum works well.  If you set the view to the maximum, the map will center around the player every move.", max_view_y), 5, 75);
				}

				if (k == 4)
				{
					c_roff(a, format("The maximum vertical view for the way you have currently set up the screen is %d.\n\n     The traditional horizontal view is 4, and with bigscreen about two or three below the maximum works well.  If you set the view to the maximum, the map will center around the player every move.", max_view_x), 5, 75);
				}
			}
		}

		/* Highlight current option */
		move_cursor(k + 2, 48);


		/* Get key */
		ch = inkey();

		/* Allow escape */
		if (ch == ESCAPE)
		{
			action = 0;
			break;
		}

		/* Switch to next option set */
		else if ((ch == '\t') || (ch == '>'))
		{
			action = 1;
			break;
		}

		/* Switch to previous option set */
		else if (ch == '<')
		{
			action = -1;
			break;
		}

		/* Help me. */
		else if (ch == '?')
		{
			show_file("options.txt#screen layout:", NULL, 0, 1);
			Term_clear();
		}

		/* Previous option */
		else if ((ch == '8') || (ch == 'k')) k = (n + k - 1) % n;

		/* Next option */
		else if ((ch == '2') || (ch == ' ') || (ch == '\n') ||
		         (ch == '\r') ||(ch == 'j'))
		{
			k = (n + k + 1) % n;
		}

		/* Turn on this option */
		else if ((ch == 'Y') || (ch == 'y') || (ch == '6') || (ch == 'l'))
		{
			if (k == 0)
			{
				/* Force 25_row mode, reset map_rows, advance */
				force_25_rows = TRUE;

				map_rows = 2 * BLOCK_HGT;

				*modified = TRUE;

				/* Reset clear_x and clear_y */
				if (clear_y > 5) clear_y = 2;
				if (clear_y > 8) clear_y = 4;

				k = (k + 1) % n;
			}

			else if (k == 1)
			{
				/* Toggle option, reset map_rows, advance */
				text_50_rows = TRUE;

				k = (k + 1) % n;
			}
			else continue;

			*modified = TRUE;
		}

		/* Turn off this option */
		else if ((ch == 'N') || (ch == 'n') || (ch == '4') || (ch == 'h'))
		{
			if (k == 0)
			{
				/* Turn off 25_row mode, advance */
				force_25_rows = FALSE;

				*modified = TRUE;

				k = (k + 1) % n;
			}

			else if (k == 1)
			{
				/* Toggle option, advance */
				text_50_rows = FALSE;

				k = (k + 1) % n;
			}
			else continue;

			*modified = TRUE;
		}

		/* Decrease this quantity */
		else if ((ch == '-') || (ch == '_'))
		{
			if (k == 0) continue;
			if (k == 1) continue;
			if (k == 2) toggle_map_rows(FALSE);
			if (k == 3)
			{
				clear_y--;
				if (clear_y < 2) clear_y = 2;
			}
			if (k == 4)
			{
				clear_x--;
				if (clear_x < 4) clear_x = 4;
			}
			*modified = TRUE;
		}

		/* Increase this quantity */
		else if ((ch == '=') || (ch == '+'))
		{
			if (k == 0) continue;
			if (k == 1) continue;
			if (k == 2)
			{
				if (!force_25_rows) toggle_map_rows(TRUE);
			}
			if (k == 3)
			{
				clear_y++;
				if (clear_y > max_view_y) clear_y = max_view_y;
			}
			if (k == 4)
			{
				clear_x++;
				if (clear_x > max_view_x) clear_x = max_view_x;
			}
			*modified = TRUE;
		}

		/* Error */
		else
		{
			bell("Illegal command for Screen options!");
		}
	}

	/* Changes here always require a screen update */
	if (*modified) *screen_change = TRUE;

	/* Return */
	return (action);
}





/*
 * Modify the side panel
 *
 * Return 0 if we want to leave, 1 if we want to advance to the next
 * option set, and -1 if we want to retreat to the previous option set.
 */
static s16b do_cmd_options_panel(bool *modified)
{
	s16b action = 0;

	char ch;

	int i, j, k, num;

	/* Indexes of available options, and whether they are displayed */
	int option_avail[80][2];

	/* Get the maximum number of left panel slots available */
	int slots_avail = map_rows - ROW_CUSTOM;

	/* Start out selecting the first option in the left column */
	bool left_column = TRUE;
	int selected = 0, was_selected = 0;


	/* Count the number of active panel options, store them in a list */
	for (num = 0, i = 0; TRUE; i++)
	{
		/* End of list marker */
		if (custom_display_text[i] == NULL) break;

		/* No name -- not active */
		if (streq(custom_display_text[i], "")) continue;

		/* Store this option's index */
		option_avail[num][0] = i;

		/* Assume not displayed */
		option_avail[num][1] = 0;

		/* Stored another option */
		num++;

		/* Hack -- allow many monster health bars and blank lines */
		if ((i == DISPLAY_HEALTH) || (i == DISPLAY_BLANK)) continue;

		/* Otherwise, determine whether this option is displayed */
		for (j = 0; j < slots_avail; j++)
		{
			/* Matching indexes -- option is already displayed */
			if (custom_display[j] == option_avail[num - 1][0])
			{
				option_avail[num - 1][1] = 1;
				break;
			}
		}
	}

	/* Clear screen */
	Term_clear();

	/* Set to 50 rows if necessary */
	if (num >= 22) Term_rows(TRUE);


	/* Prompt XXX XXX XXX */
	prt("Left Panel (<num> or <letter> to move, RET to set/unset, +/- to swap, !, or ESC)", 0, 0);

	/* Separator line */
	c_prt(TERM_SLATE, "==============================================================================", 1, 1);

	/* Print the headers */
	c_prt(TERM_L_BLUE, "Available Displays", 2, 11);
	c_prt(TERM_L_BLUE, "Displays Shown on Left Panel", 2, 46);


	/* Repeat until satisfied */
	while (TRUE)
	{
		/* Clear most everything */
		clear_from(3);

		/* Display the list of available options */
		for (i = 0; i <= num; i++)
		{
			if (i == num)
			{
				c_prt(TERM_SLATE, "======================================",
					3 + i, 1);
			}
			else if (option_avail[i][1])
			{
				c_prt(TERM_SLATE, custom_display_text[option_avail[i][0]],
					3 + i, 1);
			}
			else if ((left_column) && (selected == i))
			{
				c_prt(TERM_YELLOW, custom_display_text[option_avail[i][0]],
					3 + i, 1);
			}
			else
			{
				prt(custom_display_text[option_avail[i][0]], 3 + i, 1);
			}
		}

		/* Print a separator line */
		for (i = 2; i < screen_rows - 1; i++)
		{
			Term_putstr(39, i, 2, TERM_SLATE, "||");
		}

		/* Display the left panel layout */
		for (i = 0; i <= slots_avail; i++)
		{
			if (i == slots_avail)
			{
				c_prt(TERM_SLATE, "======================================",
					3 + i, 41);
			}
			else if ((!left_column) && (selected == i))
			{
				if (custom_display[i] == 0)
				{
					c_prt(TERM_YELLOW, "----", 3 + i, 41);
				}
				else
				{
					c_prt(TERM_YELLOW, custom_display_text[custom_display[i]],
						3 + i, 41);
				}
			}
			else
			{
				prt(custom_display_text[custom_display[i]], 3 + i, 41);
			}
		}

		/* Print the navigation bar -- highlight set #2 */
		prt_options_screen_navigation(screen_rows - 1, 2);

		/* Get key */
		ch = inkey();

		/* Allow escape */
		if (ch == ESCAPE)
		{
			action = 0;
			break;
		}

		/* Switch to next option set */
		else if ((ch == '\t') || (ch == '>'))
		{
			action = 1;
			break;
		}

		/* Switch to previous option set */
		else if (ch == '<')
		{
			action = -1;
			break;
		}

		/* Help me. */
		else if (ch == '?')
		{
			Term_rows(FALSE);
			show_file("options.txt#customized left panel rows:", NULL, 0, 1);
			Term_clear();
			Term_rows(TRUE);
		}

		/* Go to the left column */
		else if ((ch == '4') || ((rogue_like_commands) && (ch == 'h')))
		{
			if (!left_column)
			{
				/* Restore previous cursor position */
				i = selected;
				selected = was_selected;
				was_selected = i;

				/* Select the first available option */
				while ((option_avail[selected][1]) && (selected < num - 1))
				{
					selected++;
				}

				left_column = TRUE;
			}
		}

		/* Go to the right column */
		else if ((ch == '6') || ((rogue_like_commands) && (ch == 'l')))
		{
			if (left_column)
			{
				/* Restore previous cursor position */
				i = selected;
				selected = was_selected;
				was_selected = i;
				left_column = FALSE;
			}
		}

		/* Go up one option */
		else if ((ch == '8') || ((rogue_like_commands) && (ch == 'k')))
		{
			int old_selected = selected;

			/* Stay legal, accept only available options or slots */
			if (selected > 0)
			{
				selected--;

				if (left_column)
				{
					while ((selected >= 0) && (option_avail[selected][1]))
					{
						selected--;
					}
				}

				if (selected < 0) selected = old_selected;
			}
		}

		/* Go down one option */
		else if ((ch == '2') || ((rogue_like_commands) && (ch == 'j')))
		{
			int old_selected = selected;

			/* Stay legal, accept only available options or active slots */
			if (selected < (left_column ? num - 1 : slots_avail - 1))
			{
				selected++;

				if (left_column)
				{
					while ((selected < num) && (option_avail[selected][1]))
					{
						selected++;
					}
					if (selected >= num) selected = old_selected;
				}

				else if (selected >= slots_avail) selected = old_selected;
			}
		}

		/* Move this option down */
		else if ((ch == '+') || (ch == '='))
		{
			/* Right column, and we have space to move it down */
			if ((!left_column) && (selected < slots_avail - 1))
			{
				/* Get the option index */
				i = custom_display[selected];

				/* Swap this option with the one below it */
				custom_display[selected] = custom_display[selected + 1];
				custom_display[selected + 1] = i;

				/* Move the cursor */
				selected++;

				/* Note change */
				*modified = TRUE;
			}
		}

		/* Move this option up */
		else if (ch == '-')
		{
			/* Right column, and we have space to move it up */
			if ((!left_column) && (selected > 0))
			{
				/* Get the option index */
				i = custom_display[selected];

				/* Swap this option with the one above it */
				custom_display[selected] = custom_display[selected - 1];
				custom_display[selected - 1] = i;

				/* Move the cursor */
				selected--;

				/* Note change */
				*modified = TRUE;
			}
		}

		/* Remove empty slots */
		else if (ch == '!')
		{
			/* Scan all slots (even if not available) */
			for (i = 0; i < CUSTOM_DISPLAY_ROWS; i++)
			{
				/* Skip used slots */
				if (custom_display[i]) continue;

				/* Move display up to this slot */
				for (j = i + 1; j < CUSTOM_DISPLAY_ROWS; j++)
				{
					if (!custom_display[j]) continue;

					custom_display[i] = custom_display[j];
					custom_display[j] = 0;
					break;
				}
			}
		}


		/* Activate or cancel this option */
		else if ((ch == 'y') || (ch == 'n') || (ch == '\r') ||
		         (ch == '\n') ||(ch == ' ') || (ch == 't'))
		{
			/* We're activating new options */
			if (left_column)
			{
				bool is_free = FALSE;

				/* Get this option index */
				i = option_avail[selected][0];

				/* Is it available? */
				if (option_avail[selected][1]) continue;

				/* Option is available -- check space */
				for (j = 0; j < slots_avail; j++)
				{
					/* Slot is free */
					if (custom_display[j] == 0)
					{
						is_free = TRUE;
						break;
					}
				}

				/* Space available -- Insert the option into the slot found */
				if (is_free)
				{
					/* Make the change -- note it */
					custom_display[j] = i;
					*modified = TRUE;

					/* Hack -- you can display lots of monster health bars */
					if ((i != DISPLAY_HEALTH) && (i != DISPLAY_BLANK))
					{
						/* Remove the option from the list of available options */
						for (j = 0; j < num; j++)
						{
							/* Find the right option, and make it unavailable */
							if (option_avail[j][0] == i)
							{
								option_avail[j][1] = 1;
								break;
							}
						}
					}
				}
			}

			/* We're removing an option */
			else
			{
				/* Get the option index */
				i = custom_display[selected];

				/* Remove it from the display */
				custom_display[selected] = 0;

				/* Note change */
				*modified = TRUE;

				/* Add the option to the list of available options */
				for (j = 0; j < num; j++)
				{
					/* Find the right option, and make it available */
					if (option_avail[j][0] == i)
					{
						option_avail[j][1] = 0;
						break;
					}
				}
			}
		}

		/* Letter typed -- Jump to next option starting with that letter */
		else if ((isalpha(ch)) && (left_column))
		{
			/* Convert to uppercase */
			ch = FORCEUPPER(ch);

			/* Wrap around the list of available options */
			for (j = -1, i = selected; i < num + selected; i++)
			{
				/* Stay within list */
				j = i % num;

				/* Require availability */
				if (option_avail[j][1]) continue;

				/* Get index */
				k = option_avail[j][0];

				/* Look for the right first letter */
				if (custom_display_text[k][0] == ch)
				{
					/* Save "j" */
					break;
				}
			}

			/* If we found a legal option, select it */
			if (j >= 0) selected = j;
		}

		/* Error */
		else bell("Illegal command for Panel options!");
	}

	/* Set to 25 rows if we were showing 50 */
	Term_rows(FALSE);

	/* Return */
	return (action);
}


/*
 * Interact with screen options.
 */
static void do_cmd_options_screen(bool *modified, bool *screen_change)
{
	int tmp = 0;

	/* There are three option sets.  We start on the last one used. */
	int option_set = MIN(2, p_ptr->last_set_options_screen);
	int num_option_sets = 3;


	/* Repeat until user is satisfied */
	while (TRUE)
	{
		/* Terminal windows */
		if (option_set == 0)
		{
			/* Interact with terminal windows */
			tmp = do_cmd_options_win(modified);
		}

		/* Main screen layout */
		else if (option_set == 1)
		{
			tmp = do_cmd_options_layout(modified, screen_change);
		}

		/* Side panel customization */
		else
		{
			/* Interact with side panel */
			tmp = do_cmd_options_panel(modified);
		}


		/* Advance to next set of options */
		if (tmp > 0)
		{
			option_set += 1;
			if (option_set >= num_option_sets) option_set = 0;
		}

		/* Retreat to previous set of options */
		else if (tmp < 0)
		{
			option_set -= 1;
			if (option_set < 0) option_set = num_option_sets - 1;
		}

		/* Return */
		else
		{
			/* Save the last option set used */
			p_ptr->last_set_options_screen = option_set;

			return;
		}
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
void do_cmd_options_aux(int page, cptr info, bool *modified)
{
	char ch;

	int i, k = 0, n = 0;

	int opt[OPT_PAGE_PER];

	char buf[80];


	/* Scan the options */
	for (i = 0; i < OPT_PAGE_PER; i++)
	{
		/* Collect options on this "page" */
		if (option_page[page][i] < OPT_MAX)
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

			/* Special case - print a blank line or a header  XXX XXX */
			if (option_desc[opt[i]][0] == ' ')
			{
				sprintf(buf, "%-60s", option_desc[opt[i]]);

				prt(buf, i + 2, 0);

				/* Move the cursor down if necessary */
				if (i == k) k++;
			}

			/* Print the option normally */
			else
			{
				/* Build the option text */
				sprintf(buf, "%-46s: %s     (%s)",
					option_desc[opt[i]],
					op_ptr->opt[opt[i]] ? "yes" : "no ",
					option_text[opt[i]]);

				c_prt(a, buf, i + 2, 0);
			}
		}

		/* Highlight current option */
		move_cursor(k + 2, 48);

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
			case 'k':
			{
				k = (n + k - 1) % n;

				while (option_desc[opt[k]][0] == ' ') k = (n + k - 1) % n;
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			case 'j':
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
			case 'l':
			{
				op_ptr->opt[opt[k]] = TRUE;

				*modified = TRUE;

				k = (k + 1) % n;
				break;
			}

			case 'n':
			case '4':
			case 'h':
			{
				op_ptr->opt[opt[k]] = FALSE;

				*modified = TRUE;

				k = (k + 1) % n;
				break;
			}

			case '?':
			{
				sprintf(buf, "options.txt#%s", option_text[opt[k]]);
				show_file(buf, NULL, 0, 1);
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
 *  Header and footer marker string for pref file dumps
 */
static cptr dump_seperator = "### (Automatic deletion marker) ###";


/*
 * Remove old lines from pref files
 * -Mogami-
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
	/* Add marker, describe this dump */
	fprintf(fff, "%s begin %s\n#\n", dump_seperator, mark);

	/* Warning */
	fprintf(fff, "# If marked, changes below will be deleted and replaced automatically.\n");
}


/*
 * Output the footer of a pref-file dump
 */
static void pref_footer(FILE *fff, cptr mark)
{
	/* Warning */
	fprintf(fff, "# If marked, changes above will be deleted and replaced automatically.\n#\n");

	/* Add marker, describe this dump */
	fprintf(fff, "%s end %s\n", dump_seperator, mark);
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
	if (!get_string("Pref:", tmp, 80)) return;

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
 * Write all current options to the given preference file in the
 * lib/user directory. Modified from KAmband 1.8.
 */
static errr option_dump(cptr fname)
{
	static cptr mark = "Options dump";

	FILE *fff;
	char buf[1024];

	int i, j;


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

	/* Dump options (skip birth) */
	for (i = 0; i < OPT_BIRTH; i++)
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

	/* Dump delay */
	if (TRUE)
	{
		fprintf(fff, "# Delay\n");
		fprintf(fff, "D:%d\n\n", op_ptr->delay_factor);
	}

	/* Dump hit point warning */
	if (TRUE)
	{
		fprintf(fff, "# Hitpoint warning (x10%%)\n");
		fprintf(fff, "H:%d\n\n", op_ptr->hitpoint_warn);
	}

	/* Dump Autosave */
	if (TRUE)
	{
		fprintf(fff, "# Autosave  (autosave, frequency)\n");
		fprintf(fff, "a:%c:%d\n\n", (autosave_freq ? '1' : '0'),
			autosave_freq);
	}

	/* Dump screen options */
	if (TRUE)
	{
		fprintf(fff, "# Screen  (force_25_rows, text_50_rows, map rows, y-margin, x-margin)\n");
		fprintf(fff, "t:%c:%c:%d:%d:%d\n\n",
			(force_25_rows ? '1' : '0'), (text_50_rows ? '1' : '0'),
			map_rows, clear_y, clear_x);
	}

	/* Skip a line */
	fprintf(fff, "\n");

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
	int i;

	/* No options have been modified yet */
	bool modified = FALSE;

	/* No screen change */
	bool screen_change = FALSE;

	int old_rows = screen_rows;

	/* Save screen */
	screen_save();

	/* Set to 25 screen rows */
	Term_rows(FALSE);

	/* Interact */
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Why are we here */
		prt(format("%s options:", VERSION_NAME), 2, 0);

		/* Give some choices */
		prt("(1) Game Behavior", 4, 5);
		prt("(2) Disturbance and Warning", 5, 5);
		prt("(3) Visual Interface", 6, 5);
		prt("(4) Screen Display", 7, 5);
		prt("(5) Difficulty and Cheating", 8, 5);

		/* Blank space */

		/* Special choices */
		prt("(D) Base Delay Factor", 11, 5);
		prt("(H) Hitpoint Warning", 12, 5);
		prt("(A) Autosave Options", 13, 5);

		/* Load and Append */
		prt("(L) Load a user preference file", 15, 5);
		prt("(S) Save options to a file", 16, 5);

		/* Get help */
		prt("(?) Get help", 18, 5);

		/* Prompt */
		prt("Command: ", 20, 0);

		/* Get command */
		ch = inkey();

		/* Exit */
		if (ch == ESCAPE)
		{
			break;
		}

		/* Game Behavior Options */
		else if (ch == '1')
		{
			do_cmd_options_aux(0, "Game Behavior Options", &modified);
		}

		/* Disturbance Options */
		else if (ch == '2')
		{
			do_cmd_options_aux(1, "Disturbance and Warning Options", &modified);
		}

		/* Visual Interface Options */
		else if (ch == '3')
		{
			do_cmd_options_aux(2, "Visual Interface Options", &modified);
		}

		/* Screen Display Options */
		else if (ch == '4')
		{
			do_cmd_options_screen(&modified, &screen_change);
		}

		/* Difficulty and Cheating Options */
		else if ((ch == '5') || (ch == 'C') || (ch == 'c'))
		{
			do_cmd_options_aux(4, "Difficulty and Cheating Options", &modified);
		}

		/* Load a user pref file */
		else if ((ch == 'L') || (ch == 'l'))
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(20);

			/* XXX XXX -- Assume no modifications remain */
			modified = FALSE;
		}

		/* Append options to a file */
		else if ((ch == 'S') || (ch == 's'))
		{
			char ftmp[80];

			/* Prompt */
			prt("Command: Save options to a file", 20, 0);

			/* Prompt */
			prt("File: ", 22, 0);

			/* Default filename */
			sprintf(ftmp, "user.prf");

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
				/* Prompt */
				msg_format("Options saved in the \"%s\" directory.",
						ANGBAND_DIR_USER);
			}

			/* All modified options have been saved */
			modified = FALSE;
		}

		/* Autosave --- from Zangband */
		else if ((ch == 'A') || (ch == 'a'))
		{
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
			prt("Command: Autosave", 20, 0);

			/* Get a new value */
			while (TRUE)
			{
				char cx;

				if (!autosave_freq)
				{
					prt("Current autosave frequency:  Never", 23, 0);
				}
				else
				{
					prt(format("Current autosave frequency: every %d turns",
						autosave_freq), 23, 0);
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

			/* Note that a change was made */
			if (autosave_freq != old_autosave_freq) modified = TRUE;
		}

		/* Hack -- Base Delay Factor */
		else if ((ch == 'D') || (ch == 'd'))
		{
			/* Remember old value of delay factor */
			int old_delay_factor = op_ptr->delay_factor;

			/* Prompt */
			prt("Command: Base Delay Factor", 20, 0);

			/* Get a new value */
			while (TRUE)
			{
				char cx;
				int msec = op_ptr->delay_factor * op_ptr->delay_factor;
				prt(format("Current base delay factor: %d (%d msec)",
					op_ptr->delay_factor, msec), 23, 0);
				prt("New base delay factor (0-9 or ESC to accept): ", 22, 0);

				cx = inkey();
				if (cx == ESCAPE) break;
				if (isdigit(cx)) op_ptr->delay_factor = D2I(cx);
				else bell("Illegal delay factor!");
			}

			/* Note that a change was made */
			if (op_ptr->delay_factor != old_delay_factor) modified = TRUE;
		}

		/* Hack -- hitpoint warning factor */
		else if ((ch == 'H') || (ch == 'h'))
		{
			/* Remember old value of HP warning */
			int old_hitpoint_warn = op_ptr->hitpoint_warn;

			/* Prompt */
			prt("Command: Hitpoint Warning", 20, 0);

			/* Get a new value */
			while (TRUE)
			{
				char cx;
				prt(format("Current hitpoint warning: %2d%%",
					op_ptr->hitpoint_warn * 10), 23, 0);
				prt("New hitpoint warning (0-9 or ESC to accept): ", 22, 0);

				cx = inkey();
				if (cx == ESCAPE) break;
				if (isdigit(cx)) op_ptr->hitpoint_warn = D2I(cx);
				else bell("Illegal hitpoint warning!");
			}

			/* Note that a change was made */
			if (op_ptr->hitpoint_warn != old_hitpoint_warn) modified = TRUE;
		}

		/* Help me. */
		else if (ch == '?')
		{
			show_file("options.txt", NULL, 0, 1);
			Term_clear();
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

	/* Apply screen changes */
	if (screen_change)
	{
		/* Apply force_25_rows */
		if (force_25_rows) Term_rows(FALSE);

		/* Cancel force_25_rows */
		else Term_rows(TRUE);

		/* Adjust the panels if necessary */
		verify_panel(5, FALSE);
	}

	/* Otherwise, set to 50 screen rows if we were showing 50 before */
	else if (old_rows == 50) Term_rows(TRUE);

	/* Load screen */
	screen_load();

	/* Do a complete screen refresh  XXX XXX */
	if (modified) do_cmd_redraw();
}



#ifdef ALLOW_MACROS

/*
 * Hack -- append all current macros to the given file
 */
static errr macro_dump(cptr fname)
{
	static cptr mark = "Macro dump";

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

	/* Dump them */
	for (i = 0; i < macro__num; i++)
	{
		/* Extract the macro action */
		ascii_to_text(buf, sizeof(buf), macro__act[i]);

		/* Dump the macro action */
		fprintf(fff, "A:%s\n", buf);

		/* Extract the macro pattern */
		ascii_to_text(buf, sizeof(buf), macro__pat[i]);

		/* Dump the macro pattern */
		fprintf(fff, "P:%s\n", buf);

		/* End the macro */
		fprintf(fff, "\n");
	}

	/* Skip a line */
	fprintf(fff, "\n");

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
	static cptr mark = "Keymap dump";

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

	/* Skip a line */
	fprintf(fff, "\n");

	/* Output footer */
	pref_footer(fff, mark);

	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}


#endif


/*
 * Interact with "macros"
 */
void do_cmd_macros(void)
{
	char ch;

	char tmp[1024];

	char pat[1024];

	int mode;
	int attr;

	int old_rows = screen_rows;

	bool changed_macro = FALSE;
	bool changed_keymap = FALSE;


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

	/* Set to 25 screen rows */
	Term_rows(FALSE);


	/* Process requests until done */
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Describe */
		prt("Interact with Macros", 1, 0);


		/* Describe that action */
		prt("Current action (if any) shown below:", 20, 0);

		/* Analyze the current action */
		ascii_to_text(tmp, sizeof(tmp), macro_buffer);

		/* Display the current action */
		prt(tmp, 22, 0);


		/* Selections */
		prt("(1) Load a user preference file", 3, 5);
#ifdef ALLOW_MACROS

		attr = (changed_macro ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(2) Append macros to a file", 4, 5);
		prt("(3) Query a macro", 5, 5);
		prt("(4) Create a macro", 6, 5);
		prt("(5) Remove a macro", 7, 5);

		attr = (changed_keymap ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(6) Append keymaps to a file", 8, 5);
		prt("(7) Query a keymap", 9, 5);
		prt("(8) Create a keymap", 10, 5);
		prt("(9) Remove a keymap", 11, 5);
		prt("(0) Enter a new action", 12, 5);
		prt("(?) Get help", 13, 5);
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
			msg_format("Macros saved in the \"%s\" directory.",
					ANGBAND_DIR_USER);

			/* Saved */
			changed_macro = FALSE;
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

				/* A macro has been added */
				changed_macro = TRUE;
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

			/* A macro has been "removed" */
			changed_macro = TRUE;
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
			msg_format("Keymaps saved in the \"%s\" directory.",
					ANGBAND_DIR_USER);

			/* Saved */
			changed_keymap = FALSE;
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

				/* Keymap added */
				changed_keymap = TRUE;
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

			/* Keymap "removed" */
			changed_keymap = TRUE;
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

		/* Help me. */
		else if (ch == '?')
		{
			p_ptr->get_help_index = HELP_CMD_MACRO;
			do_cmd_help();
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

	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
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
	int attr;

	int i;

	FILE *fff;
	char buf[1024];

	int old_rows = screen_rows;

	static cptr mark;
	char ftmp[80];

	bool changed_monster = FALSE;
	bool changed_object = FALSE;
	bool changed_feature = FALSE;


	/* Save screen */
	screen_save();

	/* Set to 25 screen rows */
	Term_rows(FALSE);


	/* Interact until done */
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Interact with Visuals", 1, 0);

		/* Give some choices */
		prt("(1) Load a user pref file", 3, 5);
#ifdef ALLOW_VISUALS
		attr = (changed_monster ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(2) Dump monster attr/chars", 4, 5);
		attr = (changed_object ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(3) Dump object attr/chars", 5, 5);
		attr = (changed_feature ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(4) Dump feature attr/chars", 6, 5);
		prt("", 7, 5);
		prt("(6) Change monster attr/chars", 8, 5);
		prt("(7) Change object attr/chars", 9, 5);
		prt("(8) Change feature attr/chars", 10, 5);
		prt("", 11, 5);
#endif
		prt("(0) Reset visuals", 12, 5);
		prt("(?) Get help", 13, 5);

		/* Prompt */
		prt("Command: ", 15, 0);

		/* Prompt */
		ch = inkey();

		/* Leave */
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
			mark = "Monster attr/char definitions";

			/* Prompt */
			prt("Command: Dump monster attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

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

			/* Skip a line */
			fprintf(fff, "\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

			/* Changes saved */
			changed_monster = FALSE;

			/* Message */
			msg_print("Dumped monster attr/chars.");
		}

		/* Dump object attr/chars */
		else if (ch == '3')
		{
			mark = "Object attr/char definitions";

			/* Prompt */
			prt("Command: Dump object attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

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

			/* Dump objects */
			for (i = 0; i < z_info->k_max; i++)
			{
				char o_name[80];
				object_kind *k_ptr = &k_info[i];

				/* Skip non-entries */
				if (!k_ptr->name) continue;

				/* Tidy names */
				strip_name(o_name, i);

				/* Dump a comment */
				fprintf(fff, "# %s\n", o_name);

				/* Dump the object attr/char info */
				fprintf(fff, "K:%d:0x%02X:0x%02X\n\n", i,
					(byte)(k_ptr->x_attr), (byte)(k_ptr->x_char));
			}

			/* Skip a line */
			fprintf(fff, "\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

			/* Changes saved */
			changed_object = FALSE;

			/* Message */
			msg_print("Dumped object attr/chars.");
		}

		/* Dump feature attr/chars */
		else if (ch == '4')
		{
			mark = "Feature attr/char definitions";

			/* Prompt */
			prt("Command: Dump feature attr/chars", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

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

			/* Dump features */
			for (i = 0; i < z_info->f_max; i++)
			{
				char c;

				feature_type *f_ptr = &f_info[i];

				/* Skip non-entries */
				if (!f_ptr->name) continue;

				/* Skip features that mimic other features */
				if (f_ptr->mimic != i) continue;

				/* Dump a comment */
				fprintf(fff, "# %s\n", (f_name + f_ptr->name));

				/* We always dump the 25-row feature info */
				c = feat_x_char_25[i];

				/* Dump the standard (25-line) feature attr/char info */
				fprintf(fff, "F:%d:0x%02X:0x%02X\n", i,
					(byte)(f_ptr->x_attr), (byte)(c));

				/*
				 * If there is a good reason to, we also save the
				 * alternative char in 50-line mode.
				 */
				if (feat_x_char_50[i] != feat_x_char_25[i])
				{
					c = feat_x_char_50[i];
					fprintf(fff, "f:%d:0x%02X\n", i, (byte)(c));
				}

				/* Spacer line */
				fprintf(fff, "\n");
			}

			/* Skip a line */
			fprintf(fff, "\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			my_fclose(fff);

			/* Changes saved */
			changed_feature = FALSE;

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
			while (TRUE)
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

				/* Assume a change was made */
				if (strchr("aAcC", cx)) changed_monster = TRUE;

				/* Analyze */
				if (cx == 'n')
				{
					/* Scan to the next legal monster */
					do
					{
						r = (r + z_info->r_max + 1) % z_info->r_max;
					}
					while (!r_info[r].name);
				}
				if (cx == 'N')
				{
					/* Scan to the previous legal monster */
					do
					{
						r = (r + z_info->r_max - 1) % z_info->r_max;
					}
					while (!r_info[r].name);
				}
				if (cx == 'a')
				{
					ca = (ca + 1) % 16;
					r_ptr->x_attr = (byte)ca;
				}
				if (cx == 'A')
				{
					if (ca <= 0) ca = 16;
					ca = (ca - 1) % 16;
					r_ptr->x_attr = (byte)ca;
				}
				if (cx == 'c')
				{
					cc = (cc + 1) % 256;
					r_ptr->x_char = (char)cc;
				}
				if (cx == 'C')
				{
					if (cc == 0) cc = 255;
					else cc = (cc - 1) % 256;
					r_ptr->x_char = (char)cc;
				}
			}
		}

		/* Modify object attr/chars */
		else if (ch == '7')
		{
			static int k = 0;

			/* Prompt */
			prt("Command: Change object attr/chars", 15, 0);

			/* Hack -- query until done */
			while (TRUE)
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

				/* Assume a change was made */
				if (strchr("aAcC", cx)) changed_object = TRUE;

				/* Analyze */
				if (cx == 'n')
				{
					/* Scan to the next legal object */
					do
					{
						k = (k + z_info->k_max + 1) % z_info->k_max;
					}
					while (!k_info[k].name);
				}
				if (cx == 'N')
				{
					/* Scan to the previous legal object */
					do
					{
						k = (k + z_info->k_max - 1) % z_info->k_max;
					}
					while (!k_info[k].name);
				}
				if (cx == 'a')
				{
					ca = (ca + 1) % 16;
					k_info[k].x_attr = (byte)ca;
				}
				if (cx == 'A')
				{
					if (ca <= 0) ca = 16;
					ca = (ca - 1) % 16;
					k_info[k].x_attr = (byte)ca;
				}
				if (cx == 'c')
				{
					cc = (cc + 1) % 256;
					k_info[k].x_char = (char)cc;
				}
				if (cx == 'C')
				{
					if (cc == 0) cc = 255;
					else cc = (cc - 1) % 256;
					k_info[k].x_char = (char)cc;
				}
			}
		}

		/* Modify feature attr/chars */
		else if (ch == '8')
		{
			static int f = 0;

			/* Prompt */
			prt(format("Command: Change feature attr/chars   -- for %d-line mode",
				(force_25_rows ? 25 : 50)), 15, 0);

			/* Hack -- query until done */
			while (TRUE)
			{
				feature_type *f_ptr = &f_info[f];

				byte da = (byte)(f_ptr->d_attr);
				byte dc = (byte)(f_ptr->d_char);
				byte ca = (byte)(f_ptr->x_attr);
				byte cc = (byte)(f_ptr->x_char);

				/* Hack -- show alternate chars if necessary */
				if (!force_25_rows)
				{
					cc = (byte)(feat_x_char_50[f]);
				}

				/* Label the object */
				Term_putstr(5, 17, -1, TERM_WHITE,
					format("Terrain = %d, Name = %-40.40s",
					f, (f_name + f_ptr->name)));

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

				/* Assume a change was made */
				if (strchr("aAcC", cx)) changed_feature = TRUE;

				/* Analyze */
				if (cx == 'n')
				{
					/* Scan to the next legal, non-mimic feature */
					do
					{
						f = (f + z_info->f_max + 1) % z_info->f_max;
					}
					while ((f_info[f].mimic != f) || (!f_info[f].name));
				}
				if (cx == 'N')
				{
					/* Scan to the previous legal, non-mimic feature */
					do
					{
						f = (f + z_info->f_max - 1) % z_info->f_max;
					}
					while ((f_info[f].mimic != f) || (!f_info[f].name));
				}
				if (cx == 'a')
				{
					ca = (ca + 1) % 16;
					f_info[f].x_attr = (byte)ca;
				}
				if (cx == 'A')
				{
					if (ca <= 0) ca = 16;
					ca = (ca - 1) % 16;
					f_info[f].x_attr = (byte)ca;
				}
				if (cx == 'c')
				{
					cc = (cc + 1) % 256;

					/* Save and immediately update the visuals */
					if (force_25_rows)
						f_info[f].x_char = feat_x_char_25[f] = (char)cc;
					else
						f_info[f].x_char = feat_x_char_50[f] = (char)cc;
				}
				if (cx == 'C')
				{
					if (cc == 0) cc = 255;
					else cc = (cc - 1) % 256;

					/* Save and immediately update the visuals */
					if (force_25_rows)
						f_info[f].x_char = feat_x_char_25[f] = (char)cc;
					else
						f_info[f].x_char = feat_x_char_50[f] = (char)cc;
				}
			}
		}

#endif

		/* Reset visuals */
		else if (ch == '0')
		{
			/* Reset */
			reset_visuals();

			/* Message */
			msg_print("Visual attr/char tables reset.");
		}

		/* Help me. */
		else if (ch == '?')
		{
			p_ptr->get_help_index = HELP_CMD_VISUALS;
			do_cmd_help();
		}

		/* Unknown option */
		else
		{
			bell("Illegal command for visuals!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50) Term_rows(TRUE);

	/* Load screen */
	screen_load();

	/* Do a complete screen refresh  XXX XXX */
	do_cmd_redraw();
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

	int old_rows = screen_rows;

	/* Save screen */
	screen_save();

	/* Set to 25 screen rows */
	Term_rows(FALSE);


	/* Interact until done */
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Interact with Colors", 1, 0);

		/* Give some choices */
		prt("(1) Load a user pref file", 3, 5);
#ifdef ALLOW_COLORS
		prt("(2) Dump colors", 4, 5);
		prt("(3) Modify colors", 5, 5);
		prt("(?) Get help", 6, 5);
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
			static cptr mark = "Color redefinitions";
			char ftmp[80];

			/* Prompt */
			prt("Command: Dump colors", 8, 0);

			/* Prompt */
			prt("File: ", 11, 0);

			/* Default filename */
			sprintf(ftmp, "%s.prf", op_ptr->base_name);

			/* Get a filename */
			if (!askfor_aux(ftmp, 80)) continue;

			/* Build the filename */
			path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

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

			/* Skip a line */
			fprintf(fff, "\n");

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
			static byte a = 0;

			/* Prompt */
			prt("Command: Modify colors", 8, 0);

			/* Hack -- query until done */
			while (TRUE)
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
				if (cx == 'k') angband_color_table[a][0] =
				                  (byte)(angband_color_table[a][0] + 1);
				if (cx == 'K') angband_color_table[a][0] =
				                  (byte)(angband_color_table[a][0] - 1);
				if (cx == 'r') angband_color_table[a][1] =
				                  (byte)(angband_color_table[a][1] + 1);
				if (cx == 'R') angband_color_table[a][1] =
				                  (byte)(angband_color_table[a][1] - 1);
				if (cx == 'g') angband_color_table[a][2] =
				                  (byte)(angband_color_table[a][2] + 1);
				if (cx == 'G') angband_color_table[a][2] =
				                  (byte)(angband_color_table[a][2] - 1);
				if (cx == 'b') angband_color_table[a][3] =
				                  (byte)(angband_color_table[a][3] + 1);
				if (cx == 'B') angband_color_table[a][3] =
				                  (byte)(angband_color_table[a][3] - 1);

				/* Hack -- react to changes */
				Term_xtra(TERM_XTRA_REACT, 0);

				/* Hack -- redraw */
				Term_redraw();
			}
		}

		/* Help me. */
		else if (ch == '?')
		{
			p_ptr->get_help_index = HELP_CMD_COLORS;
			do_cmd_help();
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


	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50) Term_rows(TRUE);

	/* Load screen */
	screen_load();

	/* Do a complete screen refresh  XXX XXX */
	do_cmd_redraw();
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
	if (!get_string("Note:", tmp, 80)) return;

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
	"You are still uncertain about this level...",
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
	cptr quest_feel;

	/* Verify the feeling */
	if (feeling > 10) feeling = 10;

	/* No useful feeling in town */
	if (!p_ptr->depth)
	{
		msg_print("Looks like a typical town.");
		return;
	}

	/* No useful feelings until enough time has passed */
	if (no_feeling_yet)
	{
		msg_print("You are still uncertain about this level...");
		return;
	}

	/* Display the precognition messages  XXX XXX */
	precog_msg(NULL);

	/* Display the feeling */
	msg_print(do_cmd_feeling_text[feeling]);

	/* Display the quest description for the current level */
	quest_feel = describe_quest(p_ptr->depth, QMODE_SHORT);
	if (quest_feel != NULL) msg_print(quest_feel);
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
			msg_print("Collect your reward at the Inn!");
		}

		else
		{
			q_out = describe_quest(p_ptr->cur_quest, QMODE_FULL);

			/* Break into two lines if necessary */
			if (strlen(q_out) < 70) msg_print(q_out);
			else
			{
				q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_1);
				msg_print(q_out);
				q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_2);
				msg_print(q_out);
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

	/* Append to the file */
	fp = my_fopen(buf, "r");

	/* Oops */
	if (!fp) return;


	/* Save screen */
	screen_save();


	/* Clear the screen */
	Term_clear();


	/* Load the screen */
	for (y = 0; okay && (y < Term->hgt); y++)
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
	for (y = 0; okay && (y < Term->hgt); y++)
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

	FILE *fp;

	char buf[1024];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "dump.txt");

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fp = my_fopen(buf, "w");

	/* Oops */
	if (!fp) return;


	/* Save screen */
	screen_save();


	/* Dump the screen */
	for (y = 0; y < Term->hgt; y++)
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
		fprintf(fp, "%s\n", buf);
	}

	/* Skip a line */
	fprintf(fp, "\n");


	/* Dump the screen */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = hack[a & 0x0F];
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fp, "%s\n", buf);
	}

	/* Skip a line */
	fprintf(fp, "\n");


	/* Close it */
	my_fclose(fp);


	/* Message */
	msg_print("Screen dump saved.");
	message_flush();


	/* Load screen */
	screen_load();
}



/*
 * Display the current score
 */
static void do_cmd_knowledge_score(void)
{
	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Grab permissions */
	safe_setuid_grab();

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);

	/* Drop permissions */
	safe_setuid_drop();

	/* Display the score */
	predict_score();

	/* Shut the high score file */
	fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;
}


#define BROWSER_ROWS	16



typedef struct mon_struct mon_struct;

struct mon_struct
{
	cptr symbol;
	cptr text;
};

/*
 * Symbols and descriptions of monster groups.
 * Note that the "Uniques" group is handled differently.
 *
 * Most of the groups here should be in alphabetical order.
 */
static mon_struct monster_group[] =
{
	{ (char *) -1L, "uniques" },
	{ (char *) -2L, "the player ghost" },
	{ "A",  "angels" },
	{ "a",  "ants" },
	{ "b",  "bats" },
	{ "B",  "birds" },
	{ "C",  "canines" },
	{ "c",  "centipedes" },
	{ "&I", "demons" },
	{ "Dd", "dragons" },
	{ "E",  "elementals" },
	{ "f",  "felines" },
	{ "e",  "floating eyes" },
	{ "F",  "flying insects" },
	{ "G",  "ghosts" },
	{ "P",  "giants" },
	{ "g",  "golems" },
	{ "p",  "humans" },
	{ "h",  "humanoids" },
	{ "H",  "hybrids" },
	{ "i",  "icky things" },
	{ "j",  "jellies" },
	{ "K",  "killer beetles" },
	{ "k",  "kobolds" },
	{ "l",  "lice" },
	{ "L",  "liches" },
	{ "$!?=\".`~^+<>()[]{}\\|/:", "mimics" },
	{ "m",  "molds" },
	{ "M",  "mummies" },
	{ ",",  "mushroom patches" },
	{ "n",  "nagas" },
	{ "@",  "non-player characters" },
	{ "O",  "ogres" },
	{ "o",  "orcs" },
	{ "q",  "quadrupeds" },
	{ "Q",  "quylthulgs" },
	{ "R",  "reptiles" },
	{ "#%", "rock monsters" },
	{ "r",  "rodents" },
	{ "s",  "skeletons" },
	{ "J",  "snakes" },
	{ "S",  "spiders" },
	{ "t",  "townspeople" },
	{ "T",  "trolls" },
	{ "V",  "vampires" },
	{ "v*",  "vortexes and storms" },
	{ "W",  "wights/wraiths" },
	{ "w",  "worm masses" },
	{ "X",  "xorns/xaren" },
	{ "y",  "yeeks" },
	{ "Y",  "yetis" },
	{ "Z",  "zephyr hounds" },
	{ "z",  "zombies" },
	{ NULL, NULL }
};

/*
 * Build a list of monster indexes in the given group.  Return the number
 * of monsters in the group.  -EZ-
 */
static int collect_monsters(int grp_cur, int *mon_idx, int mode)
{
	int i, mon_cnt = 0;

	/* Get a list of racial chars in this group */
	cptr group_char = monster_group[grp_cur].symbol;

	/* Hack -- Check if this is the "Uniques" group  XXX */
	bool grp_unique = (monster_group[grp_cur].symbol == (char *) -1L);
	bool grp_player_ghost =
	                  (monster_group[grp_cur].symbol == (char *) -2L);

	/* Check every race */
	for (i = 0; i < z_info->r_max; i++)
	{
		/* Get the race */
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Skip empty race */
		if (!r_ptr->name) continue;
		if (!r_ptr->rarity) continue;

		/* Uniques */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Appear only in the special unique category */
			if ((!grp_unique) && (!grp_player_ghost)) continue;
		}
		else if (grp_unique) continue;

		/* Player ghosts */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			/* Appear only in the special player ghosts category */
			if (!grp_player_ghost) continue;

			/* Do not appear unless there is a ghost present */
			if (!r_ptr->cur_num) continue;
		}
		else if (grp_player_ghost) continue;

		/* Usually require known monsters */
		if (!(mode & 0x02) && !cheat_know && !(l_ptr->sights) &&
		    !grp_player_ghost) continue;

		/* Require correct racial char, or unique/player ghost status */
		if ((grp_unique) || (grp_player_ghost) ||
		    (strchr(group_char, r_ptr->d_char)))
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
	return (mon_cnt);
}


typedef struct obj_struct obj_struct;

struct obj_struct
{
	byte tval;
	cptr text;
};


/*
 * Tvals and descriptions of object groups.
 */
static obj_struct object_group[] =
{
	{ TV_FOOD,        "Mushrooms" },
	{ TV_POTION,      "Potions" },
	{ TV_SCROLL,      "Scrolls" },
	{ TV_RING,        "Rings" },
	{ TV_AMULET,      "Amulets" },
	{ TV_LITE,        "Light Sources" },
	{ TV_WAND,        "Wands" },
	{ TV_STAFF,       "Staves" },
	{ TV_ROD,         "Rods" },
	{ TV_SWORD,       "Swords" },
	{ TV_HAFTED,      "Hafted Weapons" },
	{ TV_POLEARM,     "Polearms" },
	{ TV_DIGGING,     "Diggers" },
	{ TV_BOW,         "Bows" },
	{ TV_BOOTS,       "Boots" },
	{ TV_GLOVES,      "Gloves" },
	{ TV_HELM,        "Helms" },
	{ TV_CROWN,       "Crowns" },
	{ TV_SHIELD,      "Shields" },
	{ TV_CLOAK,       "Cloaks" },
	{ TV_SOFT_ARMOR,  "Soft Body Armour" },
	{ TV_HARD_ARMOR,  "Hard Body Armour" },
	{ TV_DRAG_ARMOR,  "Dragon Scale Mail" },
	{ TV_MAGIC_BOOK,  "Books of Sorcery" },
	{ TV_PRAYER_BOOK, "Books of Piety" },
	{ TV_NATURE_BOOK, "Books of Nature" },
	{ TV_DARK_BOOK,   "Tomes of Necromancy" },
	{ TV_COMPONENT,   "Components" },
	{ TV_PARCHMENT,   "Parchments" },
	{ TV_BOTTLE,      "Bottles" },
	{ TV_ESSENCE,     "Essences" },
	{ 0,              "" }
};

/*
 * Build a list of object indexes in the given group. Return the number
 * of object in the group.  -EZ-
 */
static int collect_objects(int grp_cur, int object_idx[])
{
	int i, j, k, object_cnt = 0;

	/* Get this group's tval */
	byte group_tval = object_group[grp_cur].tval;

	/* Check every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		/* Get the object kind */
		object_kind *k_ptr = &k_info[i];

		/* Skip empty objects */
		if (!k_ptr->name) continue;

		/* Skip non-flavoured objects */
		if (!k_ptr->flavor) continue;

		/* Skip items with no distribution */
		for (j = 0, k = 0; j < 3; j++) k += k_ptr->chance[j];
		if (!k)  continue;

		/* Require that object be seen at least once, or be aware */
		if (!((k_ptr->special & (SPECIAL_EVER_SEEN)) ||
		      (k_ptr->special & (SPECIAL_AWARE)))) continue;

		/* Object is of the correct kind */
		if (k_ptr->tval == group_tval)
		{
			/* Add the object */
			object_idx[object_cnt++] = i;
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Return the number of objects in this group */
	return (object_cnt);
}

/*
 * Build a list of artifact indexes in the given group.  Return the number
 * of artifacts in the group.  -EZ-
 */
static int collect_artifacts(int grp_cur, int object_idx[])
{
	int i, object_cnt = 0;

	/* Get a list of x_char in this group */
	byte group_tval = object_group[grp_cur].tval;

	/* Check every object */
	for (i = 1; i < z_info->a_max; i++)
	{
		/* Get the artifact */
		artifact_type *a_ptr = &a_info[i];

		/* Skip empty artifacts */
		if (!a_ptr->name) continue;

		/* Skip artifacts that may appear again */
		if (a_ptr->cur_num == 0) continue;

		/* Artifact is of the correct kind */
		if (a_ptr->tval == group_tval)
		{
			/* Add the artifact */
			object_idx[object_cnt++] = i;
		}
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Return the number of artifacts */
	return (object_cnt);
}

/*
 * Display the object groups.  -EZ-
 */
static void display_group_list(int col, int row, int wid, int per_page,
	int grp_idx[], bool object, int grp_cur, int grp_top)
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
		c_put_str(attr, (object ? object_group[grp].text :
			format("%^s", monster_group[grp].text)), row + i, col);
	}
}

/*
 * Create and observe a fake object or artifact.
 */
static void observe_object_fake(int k_idx, int a_idx)
{
	object_type forge;

	/* Get local object */
	object_type *o_ptr = &forge;

	/* We want to look at an artifact */
	if (a_idx)
	{
		/* Make fake artifact */
		make_fake_artifact(o_ptr, a_idx);
	}

	/* We want to look at an object kind */
	else
	{
		/* Create the object */
		object_prep(o_ptr, k_idx);
	}

	/* Identify the object normally XXX */
	object_known(o_ptr);

	/* Observe the object or artifact */
	do_cmd_observe(o_ptr, FALSE);
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
 * Display the artifacts in a group.  -EZ-
 */
static void display_artifact_list(int col, int row, int per_page,
	int object_idx[], int object_cur, int object_top)
{
	int i;
	char o_name[80];
	object_type *o_ptr;
	object_type forge;

	/* Display lines until done */
	for (i = 0; i < per_page && object_idx[i]; i++)
	{
		/* Get the artifact index */
		int a_idx = object_idx[object_top + i];

		/* Choose a color */
		byte attr = TERM_WHITE;
		byte cursor = TERM_L_BLUE;

		/* Highlight selected artifact */
		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Get local object */
		o_ptr = &forge;

		/* Make fake artifact */
		make_fake_artifact(o_ptr, a_idx);

		/* Get its name */
		object_desc_store(o_name, o_ptr, TRUE, 0);

		/* Display the name */
		c_prt(attr, o_name, row + i, col);

		/* More information for wizards */
		if (p_ptr->noscore & (0x0002))
			c_prt(attr, format("%d", a_idx), row + i, 70);
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}



/*
 * Display known artifacts.  -EZ-
 */
static void do_cmd_knowledge_artifacts(void)
{
	int i, len, max;
	int grp_cur = 0;
	int grp_top = 0;
	int object_old = -1;
	int object_cur = 0;
	int object_top = 0;
	int grp_cnt, grp_idx[100];
	int object_cnt;
	int *object_idx;

	int column = 0;
	bool flag = FALSE;
	bool redraw = TRUE;

	/* Allocate the "object_idx" array */
	C_MAKE(object_idx, z_info->a_max, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; object_group[i].tval != 0; i++)
	{
		/* Measure the label */
		len = strlen(object_group[i].text);

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


	/* Interact until satisfied */
	while (!flag)
	{
		char ch;

		/* Handle a redraw request */
		if (redraw)
		{
			clear_from(0);

			/* Redraw */
			prt("Knowledge - artifacts", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (p_ptr->noscore & (0x0002)) prt("Idx", 4, 70);

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
		if (grp_cur >= grp_top + BROWSER_ROWS)
			grp_top = grp_cur - BROWSER_ROWS + 1;

		/* Scroll object list (artifact) */
		if (object_cur < object_top) object_top = object_cur;
		if (object_cur >= object_top + BROWSER_ROWS)
			object_top = object_cur - BROWSER_ROWS + 1;

		/* Display a list of object groups (artifact) */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, TRUE,
			grp_cur, grp_top);

		/* Get a list of artifacts in the current group */
		object_cnt = collect_artifacts(grp_idx[grp_cur], object_idx);

		/* Display a list of artifacts in the current group */
		display_artifact_list(max + 3, 6, BROWSER_ROWS, object_idx, object_cur,
			object_top);

		/* Prompt */
		prt("<dir>, 'I' to inspect, ESC", 23, 0);

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

		/* Handle commands */
		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case 'I':
			case 'i':
			{
				/* Recall on screen */
				if (object_idx[object_cur] > 0)
				{
					observe_object_fake(0, object_idx[object_cur]);
					redraw = TRUE;
				}
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
	FREE(object_idx);
}

/*
 * Display the monsters in a group.  -EZ-
 */
static void display_monster_list(int col, int row, int per_page,
	int *mon_idx, int mon_cur, int mon_top)
{
	int i;

	/* Display lines until done */
	for (i = 0; i < per_page && mon_idx[i]; i++)
	{
		byte attr;

		/* Get the race index */
		int r_idx = mon_idx[mon_top + i];

		/* Get the race */
		monster_race *r_ptr = &r_info[r_idx];
		monster_lore *l_ptr = &l_list[r_idx];

		/* Choose a color */
		attr = ((i + mon_top == mon_cur) ? TERM_L_BLUE : TERM_WHITE);

		/* Display the monster name */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			char name[80];
			sprintf(name, "%s, the %s", ghost_name, r_name + r_ptr->name);
			c_prt(attr, name, row + i, col);
		}
		else
		{
			c_prt(attr, r_name + r_ptr->name, row + i, col);
		}

		if (p_ptr->noscore & (0x0002))
		{
			c_prt(attr, format ("%d", r_idx), row + i, 60);
		}

		/* Display symbol */
		Term_putch(68, row + i, r_ptr->x_attr, r_ptr->x_char);

		/* Display kills - unique */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			put_str(format("%s", (r_ptr->max_num == 0 ? "dead" : "alive")),
				row + i, 73);
		}

		/* Display kills - non-unique */
		else put_str(format("%5d", l_ptr->pkills), row + i, 73);

	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		Term_erase(col, row + i, 255);
	}
}

/*
 * Display known monsters.  -EZ-
 */
static void do_cmd_knowledge_monsters(void)
{
	int i, len, max;
	int grp_cur, grp_top;
	int mon_cur, mon_top;
	int grp_cnt, grp_idx[100];
	int mon_cnt;
	int *mon_idx;

	int column = 0;
	bool flag;
	bool redraw;

	/* Allocate the "mon_idx" array */
	C_MAKE(mon_idx, z_info->r_max, int);

	max = 0;
	grp_cnt = 0;

	/* Check every group */
	for (i = 0; monster_group[i].text != NULL && *monster_group[i].text; i++)
	{
		/* Measure the label */
		len = strlen(monster_group[i].text);

		/* Save the maximum length */
		if (len > max) max = len;

		/* See if any monsters are known */
		if ((monster_group[i].symbol == ((char *) -1L)) ||
		    (collect_monsters(i, mon_idx, 0x01)))
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
			if (p_ptr->noscore & (0x0002)) prt("Idx", 4, 60);
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
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, FALSE,
			grp_cur, grp_top);

		/* Get a list of monsters in the current group */
		mon_cnt = collect_monsters(grp_idx[grp_cur], mon_idx, 0x00);

		/* Display a list of monsters in the current group */
		display_monster_list(max + 3, 6, BROWSER_ROWS, mon_idx, mon_cur, mon_top);

		/* Prompt */
		prt("<dir>, 'r' to recall, ESC", 23, 0);

		/* Mega Hack -- track this monster race */
		if (mon_cnt) monster_race_track(mon_idx[mon_cur]);

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
				if (mon_idx[mon_cur])
				{
					/* Recall on screen */
					screen_roff(mon_idx[mon_cur]);

					/* Wait for a keypress */
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

	/* XXX XXX Free the "mon_idx" array */
	FREE(mon_idx);
}


/*
 * Total kill count.  Idea and some code by -TY-.
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
static void do_cmd_knowledge_kill_count(void)
{
	FILE *fp;

	char file_name[1024];
	char buf[120];

	int old_rows = screen_rows;

	u32b kills = 0;
	u32b k;

	int why;
	int wid;

	int grp, i, n, count;

	/* Array of monsters */
	u16b *who;


	/* Temporary file */
	fp = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fp)
	{
		msg_print("Could not open a temporary file to show the kill count.");
		return;
	}


	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);


	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Skip blank monsters */
		if (!r_ptr->name) continue;

		/* No kills of this monster */
		if (l_ptr->pkills == 0) continue;

		/* We never tally up player ghosts  XXX XXX */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

		/* Collect "appropriate" monsters */
		who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* No monsters to recall */
		msg_print("No known monsters!");
		message_flush();

		/* XXX XXX Free the "who" array */
		FREE(who);

		return;
	}

	/* Set margins */
	text_out_wrap   = 77;
	text_out_indent = 3;

	/* Dump to the file */
	text_out_hook = text_out_to_file;
	text_out_file = fp;

	/* Get available width */
	wid = text_out_wrap - text_out_indent;


	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* We are sorting by level */
	why = 2;

	/* Sort the monster array */
	ang_sort(who, &why, n);


	/* Print the total number of kills */
	if (p_ptr->total_kills <= 0L)
	{
		text_out("You have defeated no enemies yet.\n");
	}
	else if (p_ptr->total_kills == 1L)
	{
		text_out("You have defeated one enemy.\n");
	}
	else
	{
		text_out(format("You have defeated %lu enemies.\n",
			p_ptr->total_kills));
	}

	/* Count the slain uniques */
	for (count = 0, i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		/* Tally up dead uniques */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			if (r_ptr->max_num == 0) count++;
		}
	}

	/* We have slain at least one unique */
	if (count)
	{
		/* Header -- uniques */
		strcpy(buf, "------------------ Uniques ------------------");
		text_out_c(TERM_L_BLUE, format("\n\n%s\n\n", buf));

		/* Display all the slain uniques */
		for (count = 0, i = 0; i < n; i++)
		{
			monster_race *r_ptr = &r_info[who[i]];

			/* Note dead uniques, add to kill count */
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				if (r_ptr->max_num == 0)
				{
					text_out(format("%c     %s\n", r_ptr->x_char,
						(r_name + r_ptr->name)));

					kills++;
				}
			}
		}
	}


	/* Count the slain non-uniques */
	for (count = 0, i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		/* Tally up dead non-uniques */
		if (!(r_ptr->flags1 & (RF1_UNIQUE))) count++;
	}

	/* We have slain at least one non-unique */
	if (count)
	{
		/* Header -- non-uniques */
		strcpy(buf, "---------------- Non-Uniques ----------------");
		text_out_c(TERM_L_BLUE, format("\n\n%s\n", buf));

		/* Scan down the list of monster groups */
		for (grp = 2; monster_group[grp].text != NULL &&
			*monster_group[grp].text; grp++)
		{
			/* Get a list of racial chars in this group */
			cptr group_char = monster_group[grp].symbol;

			/* Look for monsters in this group */
			for (count = 0, i = 0; i < n; i++)
			{
				monster_race *r_ptr = &r_info[who[i]];

				/* Must not be a unique */
				if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

				/* Must be in this group */
				if (strchr(group_char, r_ptr->d_char))
				{
					/* We have at least one match */
					count++;
					break;
				}
			}

			/* No monsters in this group */
			if (!count) continue;

			/* Format group header with uppercase first letter */
			strcpy(buf, format("%c%s",
				toupper(monster_group[grp].text[0]),
				monster_group[grp].text + 1));

			/* Display group header */
			text_out_c(TERM_L_BLUE, format("\n%s:\n", buf));

			/* Display monsters in this group */
			for (i = 0; i < n; i++)
			{
				monster_race *r_ptr = &r_info[who[i]];
				monster_lore *l_ptr = &l_list[who[i]];

				/* Must not be a unique */
				if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

				/* Must be in this group */
				if (!strchr(group_char, r_ptr->d_char)) continue;

				/* A single kill */
				if (l_ptr->pkills == 1)
				{
					/* Display name, handle coins specially */
					if (strstr(r_name + r_ptr->name, "coins"))
					{
						text_out(format("%c     1 pile of %s\n", r_ptr->x_char,
								(r_name + r_ptr->name)));
					}
					else
					{
						text_out(format("%c     1 %s\n", r_ptr->x_char,
								(r_name + r_ptr->name)));
					}
				}

				/* More than one kill */
				else if (l_ptr->pkills)
				{
					/* Get name of monster */
					strcpy(buf, (r_name + r_ptr->name));

					/* Pluralize it */
					plural_aux(buf);

					/* Print it */
					text_out(format("%c     %d %s\n", r_ptr->x_char,
						l_ptr->pkills, buf));
				}

				/* Tally up kills */
				kills += l_ptr->pkills;
			}
		}
	}

	/* Spacer line */
	strcpy(buf, "-------------------------------------------");
	text_out_c(TERM_L_BLUE, format("\n\n%s\n\n", buf));

	/* Note total kills */
	if (p_ptr->total_kills > 0)
	{
		text_out(format("   %lu creature%s killed",
			p_ptr->total_kills, (p_ptr->total_kills == 1 ? "" : "s")));

		/* Calculate the number of unseen kills */
		k = p_ptr->total_kills - kills;

		/* Have we killed any monsters we did not see? */
		if (k > 0L)
		{
			text_out(format(", of which %lu %s of unknown race (unseen kills)",
				k, (k == 1L ? "was" : "were")));
		}

		/* End the sentence */
		text_out(format(".\n"));
	}

	/* Free the "who" array */
	FREE(who);

	/* Close the file */
	my_fclose(fp);


	/* We want 50 rows */
	if (text_50_rows) Term_rows(TRUE);

	/* We don't */
	else Term_rows(FALSE);


	/* Display the file contents */
	(void)show_file(file_name, "Kill Count", 0, 2);


	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
	}

	/* Set to 25 rows, if we were showing 25 before */
	else
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(FALSE);
	}


	/* Remove the file */
	(void)fd_kill(file_name);
}



/*
 * Display the objects in a group.  -EZ-
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

		/* Choose a color */
		byte attr =   ((k_ptr->special & (SPECIAL_AWARE)) ?
			TERM_WHITE : TERM_SLATE);
		byte cursor = ((k_ptr->special & (SPECIAL_AWARE)) ?
			TERM_L_BLUE : TERM_BLUE);

		/* Highlight selected object */
		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Display the name */
		c_prt(attr, k_name + k_ptr->name, row + i, col);

		if (p_ptr->noscore & (0x0002))
			c_prt(attr, format ("%d", k_idx), row + i, 70);

		if (k_ptr->special & (SPECIAL_AWARE))
		{
			byte a = misc_to_attr[k_ptr->flavor];
			byte c = misc_to_char[k_ptr->flavor];

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
 * Display known objects.  -EZ-
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
	for (i = 0; object_group[i].tval != 0; i++)
	{
		/* Measure the label */
		len = strlen(object_group[i].text);

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
			if (p_ptr->noscore & (0x0002)) prt("Idx", 4, 70);
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
		if (object_cur >= object_top + BROWSER_ROWS)
			object_top = object_cur - BROWSER_ROWS + 1;

		/* Display a list of object groups */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, TRUE,
			grp_cur, grp_top);

		/* Get a list of objects in the current group */
		object_cnt = collect_objects(grp_idx[grp_cur], object_idx);

		/* Display a list of objects in the current group */
		display_object_list(max + 3, 6, BROWSER_ROWS, object_idx, object_cur,
			object_top);

		/* Prompt */
		prt("<dir>, 'I' to inspect, ESC", 23, 0);

		/* Mega Hack -- track this object kind */
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

			case 'I':
			case 'i':
			{
				/* Recall on screen */
				if (object_idx[object_cur] > 0)
				{
					observe_object_fake(object_idx[object_cur], 0);
					redraw = TRUE;
				}
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &object_cur,
					object_cnt);
				break;
			}
		}
	}

	/* XXX XXX Free the "object_idx" array */
	FREE(object_idx);
}


/*
 * Display contents of the Home. Code taken from the player death interface
 * and the show known objects function.  -LM-
 */
static void do_cmd_knowledge_home(void)
{
	int k;

	store_type *st_ptr = &store[STORE_HOME];

	FILE *fp;

	object_type *o_ptr;
	char o_name[120];

	char file_name[1024];

	/* Temporary file */
	fp = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fp)
	{
		msg_print("Could not open a temporary file to show the contents of your home.");
		return;
	}


	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Display contents of the home */
		for (k = 0; k < st_ptr->stock_num; k++)
		{
			/* Acquire item */
			o_ptr = &st_ptr->stock[k];

			/* Acquire object description */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Print a message */
			fprintf(fp, "     %s\n", o_name);
		}
	}

	/* Close the file */
	my_fclose(fp);

	/* Display the file contents */
	show_file(file_name, "Contents of Your Home", 0, 2);

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Display the current quest and, underneath it, all previous quests.
 */
static void do_cmd_knowledge_quests(void)
{
	char buf[100];
	int wid;
	int i, level;
	bool quests = FALSE;
	int attr;

	char name[80];
	char intro[80];
	char targets[80];
	char where[80];

	FILE *fp;
	char file_name[1024];


	/* Temporary file */
	fp = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fp)
	{
		msg_print("Could not open a temporary file to show your quests.");
		return;
	}


	/* Set margins */
	text_out_wrap   = 77;
	text_out_indent = 3;

	/* Dump to the file */
	text_out_hook = text_out_to_file;
	text_out_file = fp;

	/* Get available width */
	wid = text_out_wrap - text_out_indent;

	/* Winner */
	if (p_ptr->total_winner)
	{
		center_string(buf, "You have slain Morgoth, Lord of Darkness!", wid);
		text_out_c(TERM_L_BLUE, format("%s\n\n", buf));
	}

	/* Killed Sauron */
	else if (!r_info[MON_SAURON].max_num)
	{
		center_string(buf, "You have slain Sauron, the Sorcerer; only Morgoth now remains!", wid);
		text_out_c(TERM_L_BLUE, format("%s\n\n", buf));
	}

	/* Print the current quest, if any */
	if (p_ptr->cur_quest)
	{
		text_out_c(TERM_L_BLUE,
			format("%s\n\n", describe_quest(p_ptr->cur_quest, QMODE_FULL)));
	}


	/* Print fame */
	get_fame_desc(&attr, name);
	center_string(buf, format("Fame:  %s", name), wid);
	text_out_c(attr, format("%s\n\n", buf));


	/* Print previous quests */
	for (i = 0; i < MAX_QM_IDX; i++)
	{
		/* Non-empty record */
		if (p_ptr->quest_memory[i].type != 0)
		{
			monster_race *r_ptr = &r_info[p_ptr->quest_memory[i].r_idx];

			/* Monster name */
			strcpy(name, r_name + r_ptr->name);

			/* Start the list */
			if (!quests) text_out("Previous Quests:\n");

			/* We've displayed at least one quest */
			quests = TRUE;

			/* Introduction */
			if (p_ptr->quest_memory[i].succeeded)
			{
				strcpy(intro, "Defeated");
				attr = TERM_WHITE;
			}
			else
			{
				strcpy(intro, "Failed to defeat");
				attr = TERM_RED;
			}

			/* Monster name */
			if (p_ptr->quest_memory[i].max_num > 1)
			{
				plural_aux(name);
				my_strcpy(targets, format("%d %s",
					p_ptr->quest_memory[i].max_num, name), sizeof(targets));
			}

			/* One quest monster */
			else
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					my_strcpy(targets, format("%s", name), sizeof(targets));
				}

				else if (is_a_vowel(name[0]))
				     my_strcpy(targets, format("an %s", name), sizeof(targets));
				else my_strcpy(targets, format("a %s",  name), sizeof(targets));
			}

			/* The location of the quest */
			level = p_ptr->quest_memory[i].level;

			if (!depth_in_feet) strcpy(where, format("on level %d", level));
			else
			{
				if (!use_metric) strcpy(where, format("at %d feet",
					level * 50));
				else strcpy(where, format("at %d meters", level * 15));
			}

			/* Print this quest */
			text_out_c(attr, format("%s %s %s\n", intro, targets, where));
		}
	}

	/* No previous quests */
	if (!quests)
	{
		text_out("You have finished no quests yet.\n");
	}

	/* Reset "text_out()" vars */
	text_out_wrap   = 0;
	text_out_indent = 0;

	/* Close the file */
	my_fclose(fp);

	/* Display the file contents */
	show_file(file_name, "Current and Completed Quests", 0, 2);

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Interact with "knowledge"
 *
 * The code for artifacts, objects, and monsters is taken from Eytan Zweig's
 * EyAngband.  That for the monster killed count is taken from Zangband.
 */
void do_cmd_knowledge(void)
{
	char ch;

	int old_rows = screen_rows;

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Save screen */
	screen_save();

	/* Set to 25 rows */
	Term_rows(FALSE);

	/* Interact until done */
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Display current knowledge", 2, 0);

		/* Give some choices */
		prt("(1) Display score",                         4, 5);
		prt("(3) Display known monsters",                6, 5);
		prt("(4) Display kill count",                    7, 5);
		prt("(5) Display known objects",                 8, 5);
		prt("(6) Display contents of your home",         9, 5);
		prt("(7) Display current and completed quests", 10, 5);

		/* When in town, we can also check artifacts  XXX XXX */
		if (!p_ptr->depth)
		{
			prt("(2) Display known (or lost) artifacts",  5, 5);
		}
		else
		{
			c_prt(TERM_SLATE,
				"(2) Cannot check known artifacts while in the dungeon",
				5, 5);
		}

		/* Prompt */
		prt("Command: ", 11, 0);

		/* Prompt */
		ch = inkey();

		/* Done */
		if (ch == ESCAPE) break;

		/* Game Score */
		if (ch == '1')
		{
			do_cmd_knowledge_score();
		}

		/* Artifacts */
		else if (ch == '2')
		{
			if (!p_ptr->depth)
			{
				do_cmd_knowledge_artifacts();
			}
			else
			{
				msg_print("You may only check artifacts when in town.");
			}
		}

		/* Uniques */
		else if (ch == '3')
		{
			do_cmd_knowledge_monsters();
		}

		/* Monster killed count */
		else if (ch == '4')
		{
			do_cmd_knowledge_kill_count();
		}

		/* Objects */
		else if (ch == '5')
		{
			do_cmd_knowledge_objects();
		}

		/* The home */
		else if (ch == '6')
		{
			do_cmd_knowledge_home();
		}

		/* Quests */
		else if (ch == '7')
		{
			do_cmd_knowledge_quests();
		}

		/* Unknown option */
		else
		{
			bell("Illegal command for knowledge!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Set to 50 rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
	}

	/* Load screen */
	screen_load();
}

