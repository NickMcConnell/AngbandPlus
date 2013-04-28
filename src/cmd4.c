/* File: cmd4.c */

/*
 * Redraw the screen, change character name, display previous messages.
 * Interact with options.  Code to generate preference files.  Interact
 * with macros, visuals, colors.  Take notes, display level feeling and
 * quests, save and load screen shots.  Interact with the character
 * knowledge menu (Display score, known objects, monsters, and artifacts,
 * contents of the home, kill count, and quests).
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Hack -- redraw the screen
 *
 * This command performs various low level updates, clears all the "extra"
 * windows, does a total redraw of the main window, and requests all possible
 * updates and redraws.
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
	(void)Term_flush();

	/* Reset "inkey()" */
	flush();


	/* Hack -- React to changes */
	(void)Term_xtra(TERM_XTRA_REACT, 0);


	/* Update everything */
	p_ptr->update = 0xFFFFFFFF;

	/* Redraw everything */
	p_ptr->redraw = 0xFFFFFFFF;

	/* Refresh everything */
	p_ptr->window = 0xFFFFFFFF;

	/* Combine and Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);


	/* Clear screen */
	(void)Term_clear();

	/* Hack -- update */
	handle_stuff();


	/* Redraw every window (bottommost first) */
	for (j = 0; j < TERM_MAX; j++)
	{
		/* Get the z order of this window */
		int term_idx = term_z_order[j];

		/* No term, or term is unavailable */
		if ((!angband_term[term_idx]) || (!angband_term[term_idx]->mapped_flag)) continue;

		/* Activate */
		(void)Term_activate(angband_term[term_idx]);

		/* Redraw */
		(void)Term_redraw();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Hack -- change name
 */
void do_cmd_change_name(void)
{
	char ch;
	char buf[1024];

	int mode = 0;


	/* Make sure everything is properly updated */
	p_ptr->update |= (PU_BONUS);
	update_stuff();

	/* Save screen */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_NORM | DSP_CX, 80, 0);

	/* Display a prompt */
	center_string(buf, sizeof(buf),
		"['c': change name, 'h': change mode, 'f': save a char dump, '?': help, or ESC]", display_width());


	/* Interact until satisfied */
	while (TRUE)
	{
		/* Display the player */
		display_player(mode, TRUE);

		/* Prompt */
		prt(buf, 23, 0);

		/* Query */
		ch = inkey(FALSE);

		/* Move cursor to top row */
		move_cursor(0, 0);

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
			char ftmp[DESC_LEN];

			(void)strnfmt(ftmp, sizeof(ftmp), "%s.txt", op_ptr->base_name);

			if (get_string("File name:", ftmp, sizeof(ftmp)))
			{
				if (ftmp[0] && (ftmp[0] != ' '))
				{
					if (file_character(ftmp, FALSE))
					{
						/* Clean up the player display */
						display_player(mode, TRUE);

						msg_print("Character dump failed!");
					}
					else
					{
						/* Clean up the player display */
						display_player(mode, TRUE);

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

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);
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
 * you to search for strings in the recall.
 */
void do_cmd_messages(void)
{
	char ch;
	bool shift_key = FALSE;

	int i, j, n, add;
	int wid, hgt, num;

	char shower[256];
	char finder[256];


	/* Wipe finder */
	strcpy(finder, "");

	/* Wipe shower */
	strcpy(shower, "");


	/* Total messages */
	n = message_num();

	/* Start on first message */
	i = 0;

	/* Get size */
	(void)Term_get_size(&wid, &hgt);


	/* Save the screen and center it */
	if (more_tall_display)
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_CX, 80, 0);
	else
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_TALL | DSP_CX, 80, 0);


	/* Note height of screen */
	hgt = Term->rows;


	/* Process requests until done */
	while (TRUE)
	{
		/* Clear screen */
		(void)Term_clear();

		/* Dump messages */
		for (j = 0, add = 0, num = 0; (i + j < n); j++, num++)
		{
			cptr msg = message_str((s16b)(i+num));
			byte attr = message_color((s16b)(i+num));

			/* Handle multi-line messages (approximately) */
			if ((int)strlen(msg) >= wid) add += (strlen(msg) / (wid-5));

			/* Stop at the top */
			if (j + add >= hgt - 4) break;

			/* Dump the messages, bottom to top */
			(void)Term_gotoxy(0, hgt - 3 - j - add);
			text_out_wrap = wid;
			text_out_indent = 2;
			text_out_to_screen(attr, msg);
		}


		/* Highlight "shower" */
		if (shower[0])
		{
			char buf[256];
			int y, x;
			byte a;
			char c;
			cptr str;

			/* Read all the lines on the screen */
			for (y = 0; y < hgt; y++)
			{
				/* Assume no text on this line */
				strcpy(buf, "");

				/* Read all the characters in this line */
				for (x = 0; x < wid; x++)
				{
					/* Get the character (ignore the color) */
					(void)Term_what(x, y, &a, &c);

					/* Add it to "buf" */
					strcat(buf, format("%c", c));
				}

				/* Make the text lowercase */
				strlower(buf);

				/* Start at beginning of line */
				str = buf;

				/* Display matches in remaining text */
				while ((str = strstr(str, shower)) != NULL)
				{
					/* Get the length of the match */
					int len = strlen(shower);

					/* Initial x-position */
					x = str - buf;

					/* Move the cursor */
					(void)Term_gotoxy(x, y);

					/* Overwrite the text in yellow */
					for (j = 0; j < len; j++)
					{
						/* Get the character */
						(void)Term_what(x++, y, &a, &c);

						/* Reprint in yellow */
						(void)Term_addch(TERM_YELLOW, c);
					}

					/* Advance past the matching text */
					str += len;
				}
			}
		}


		/* Display header XXX XXX */
		prt(format("Message Recall (%d-%d of %d)",
			i, i + num - 1, n), 0, 0);

		/* Display prompt */
		prt("Press direction keys to move, '/' search, '&' show, or ESCAPE]", hgt - 1, 0);

		/* Wait for legal input */
		while (TRUE)
		{
			/* Get key */
			ch = inkey(ALLOW_CLICK);

			/* Allow orthogonal direction keys */
			get_ui_direction(&ch, UI_NODIAG, &shift_key);

			/* Accept anything except untranslated mouse actions */
			if (ch != MOUSEKEY) break;
		}


		/* Hack -- Save the old index */
		j = i;


		/* Exit on Escape */
		if (ch == ESCAPE) break;


		/* Recall the oldest message */
		else if ((ch == 'g') || (ch == '7'))
		{
			i = n - 1;
		}

		/* Recall a full page of older messages */
		else if ((ch == ' ') || (ch == '9'))
		{
			/* Go older if legal */
			if (i + (hgt - 4) < n) i += (hgt - 4);
		}

		/* Recall a half page of older messages */
		else if ((ch == '-') || (ch == '_') || (ch == '4'))
		{
			/* Go older if legal */
			if (i + (hgt - 4) / 2 < n) i += (hgt - 4) / 2;
		}

		/* Recall 1 older message */
		else if ((ch == '8') || (ch == '\n') || (ch == '\r'))
		{
			/* Go older (if able) */
			i += (shift_key ? 5 : 1);
			if (i >= n) i = n - 1;
		}

		/* Recall 1 newer messages */
		else if (ch == '2')
		{
			/* Go newer (if able) */
			i -= (shift_key ? 5 : 1);
			if (i < 0) i = 0;
		}

		/* Recall a half page of newer messages */
		else if ((ch == '+') || (ch == '=') || (ch == '6'))
		{
			/* Go newer (if able) */
			i = (i >= (hgt - 4) / 2) ? (i - (hgt - 4) / 2) : 0;
		}

		/* Recall a full page of newer messages */
		else if (ch == '3')
		{
			/* Go newer (if able) */
			i = (i >= (hgt - 4)) ? (i - (hgt - 4)) : 0;
		}

		/* Recall the newest message */
		else if ((ch == 'G') || (ch == '1'))
		{
			i = 0;
		}


		/* Hack -- handle show */
		else if (ch == '&')
		{
			/* Prompt */
			prt("Show: ", hgt - 1, 0);

			/* Get a "shower" string, or continue */
			if (!askfor_aux(shower, sizeof(shower), FALSE)) continue;

			/* Make "shower" lowercase */
			strlower(shower);

			/* Okay */
			continue;
		}

		/* Hack -- handle find */
		else if (ch == '/')
		{
			s16b z;

			/* Prompt */
			prt("Find: ", hgt - 1, 0);

			/* Get a "finder" string, or continue */
			if (!askfor_aux(finder, wid, FALSE)) continue;

			/* Show it (search in lowercase) */
			strcpy(shower, finder);
			strlower(shower);

			/* Scan messages */
			for (z = i + 1; z < n; z++)
			{
				/* Get the message */
				char msg[1024];

				strcpy(msg, message_str(z));

				/* Make it lowercase */
				strlower(msg);

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

		/* Go to a specific line */
		else if (ch == '#')
		{
			char tmp[DESC_LEN];

			prt("Goto Line: ", hgt - 1, 0);
			strcpy(tmp, "0");

			if (askfor_aux(tmp, sizeof(tmp), FALSE))
			{
				i = atoi(tmp);
				if (i > n - hgt + 4) i = n - hgt + 4;
				if (i < 0) i = 0;
			}
		}
	}

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);
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
 * Modify the "sub-window" options
 *
 * Return 0 if we want to leave, 1 if we want to advance to the next
 * option set, and -1 if we want to retreat to the previous option set.
 */
static s16b do_cmd_options_win(bool *modified)
{
	int i, j, d;
	int action = 0;
	int window_displays = TOTAL_WINDOW_DISPLAYS;

	int y = 0;
	int x = TERM_SUBWINDOW;

	term *old = Term;

	char ch;

	u32b old_flag[TERM_MAX];


	/* Memorize old flags */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		old_flag[j] = op_ptr->window_flag[j];
	}

	/* Use the standard display and center an 80 column view */
	display_change(DSP_REMEMBER | DSP_CLEAR | DSP_NORM | DSP_CX,
		80, 0);


	/* Interact */
	while (TRUE)
	{
		/* Prompt */
		prt("Window flags (<dir> to move, 't' to toggle, or ESC)", 0, 0);

		/* Print the navigation bar -- highlight set #0 */
		prt_options_screen_navigation(Term->rows - 1, 0);

		/* Display the sub-windows */
		for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
		{
			byte a = TERM_WHITE;

			cptr s = angband_term_name[j];

			/* Use color */
			if (j == x) a = TERM_L_BLUE;

			/* Window name, staggered, centered */
			(void)Term_putstr(35 + (j-TERM_SUBWINDOW) * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
		}

		/* Display the options */
		for (i = 0; i < TOTAL_WINDOW_DISPLAYS; i++)
		{
			byte a = TERM_WHITE;

			cptr str = window_flag_desc[i];

			/* Use color */
			if (i == y) a = TERM_L_BLUE;

			/* Unused option */
			if (!str)
			{
				if (i >= BASIC_WINDOW_DISPLAYS) break;
				str = "(Unused option)";
			}

			/* Remember the last option with a name */
			else
			{
				window_displays = i;
			}


			/* Flag name */
			(void)Term_putstr(0, i + 5, -1, a, str);

			/* Display the sub-windows */
			for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
			{
				char c = '.';

				/* Use color */
				if ((i == y) && (j == x)) a = TERM_L_BLUE;
				else a = TERM_WHITE;

				/* Active flag */
				if (op_ptr->window_flag[j] & (1L << i)) c = 'X';

				/* Flag value */
				(void)Term_putch(35 + (j-TERM_SUBWINDOW) * 5, i + 5, a, c);
			}
		}

		/* Place Cursor */
		(void)Term_gotoxy(35 + (x-TERM_SUBWINDOW) * 5, y + 5);

		/* Get key */
		ch = inkey(FALSE);

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
			(void)Term_clear();

			/* Continue */
			continue;
		}

		/* Determine if we've used a direction key */
		d = target_dir(ch);

		/* Move, if able */
		if (d != 0)
		{
			x = (x + ddx[d] +  TERM_MAX) %  TERM_MAX;
			if (x < TERM_SUBWINDOW) x = TERM_SUBWINDOW;
			y = (y + ddy[d] + (window_displays + 1)) % (window_displays + 1);
		}

		/* Not a direction -- Toggle */
		else
		{
			u32b new_value;

			new_value = 1L << y;

			/* Hack -- ignore the main window */
			if (x < TERM_SUBWINDOW)
			{
				bell("Cannot set main window flags!");
			}

			/* Toggle flag (off) */
			else if (op_ptr->window_flag[x] == new_value)
			{
				op_ptr->window_flag[x] = 0;
			}

			/* Toggle flag (on) */
			else
			{
				op_ptr->window_flag[x] = new_value;
			}
		}
	}

	/* Notice changes (sub-windows only) */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* Ignore non-changes */
		if (op_ptr->window_flag[j] == old_flag[j]) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Erase */
		(void)Term_clear();

		/* Refresh */
		(void)Term_fresh();

		/* Note change */
		*modified = TRUE;
	}

	/* Restore old term */
	(void)Term_activate(old);

	/* Restore previous display */
	display_change(DSP_RESTORE, 0, 0);

	/* Return */
	return (action);
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

	int i, k = 0, n = 4;

	char buf[DESC_LEN];


	/* Clear screen */
	(void)Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Get the map term */
		term *t = use_special_map ? term_map : term_main;

		/* Get best minimum view distance */
		int max_view_y = (t->rows - get_panel_hgt()) / 2;
		int max_view_x = (t->cols - get_panel_wid()) / 2;

		/* Automatically correct minimum view distance */
		if (clear_y > max_view_y) clear_y = max_view_y;
		if (clear_x > max_view_x) clear_x = max_view_x;

		/* Clear any description text */
		for (i = n + 5; i < n + 15; i++) clear_row(i);

		/* Prompt */
		prt("Screen Layout (RET to advance, y/n to set, '+/-' to adjust number, or ESC) ", 0, 0);

		/* Print the navigation bar -- highlight set #1 */
		prt_options_screen_navigation(Term->rows - 1, 1);

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
				/* Display the option text -- more_tall_display */
				(void)strnfmt(buf, sizeof(buf), "%-46s: %s   (%s)",
					"Show more things (like text) in tall display",
					more_tall_display ? "yes" : "no ", "more_tall_display");
				c_prt(a, buf, i + 2, 0);
			}

			if (i == 1)
			{
				/* Display the option text -- map_display_precise_fit */
				(void)strnfmt(buf, sizeof(buf), "%-46s: %s   (%s)",
					"Show the largest map the screen will allow",
					map_display_precise_fit ? "yes" : "no ", "map_display_precise_fit");
				c_prt(a, buf, i + 2, 0);
			}

			if (i == 2)
			{
				/* Display the option text -- vertical view */
				(void)strnfmt(buf, sizeof(buf), "%-46s: %2d    (%s)",
					"Minimum vertical view distance",
					clear_y, "clear_y");
				c_prt(a, buf, i + 2, 0);
			}

			if (i == 3)
			{
				/* Display the option text -- horizontal view */
				(void)strnfmt(buf, sizeof(buf), "%-46s: %2d    (%s)",
					"Minimum horizontal view distance",
					clear_x, "clear_x");
				c_prt(a, buf, i + 2, 0);
			}

			/* Special explanatory text */
			if (i == k)
			{
				(void)Term_gotoxy(n + 5, 10);

				if (k == 0)
				{
					c_roff(a, "Various interfaces, such as stores, lists, and help, appear in the tall (46+ row) display.  This option is most helpful to those with large screens.", 5, display_width() - 5);
				}

				if (k == 1)
				{
					c_roff(a, format("The dungeon is laid out in %dx%d blocks; detection and panel shifting work the same way.  If you choose to make the map fit precisely, you may (depending on your screen size) see more of the dungeon, but you may also notice a slightly less clean appearance and find it less easy to keep track of detected areas.", BLOCK_HGT, BLOCK_WID), 5, display_width() - 5);
				}

				if (k == 2)
				{
					c_roff(a, format("The maximum vertical view for the way you have currently set up the screen is %d.\n\n     The traditional vertical view is 2.  If you can display at least %d map rows, about three or four below the maximum works well.  If you set the view to the maximum, the map will center around the player every move.", max_view_y, 3 * BLOCK_HGT), 5, display_width() - 5);
				}

				if (k == 3)
				{
					c_roff(a, format("The maximum horizontal view for the way you have currently set up the screen is %d.\n\n     The traditional horizontal view is 4.  If you can display at least %d map columns, about three or four below the maximum works well.  If you set the view to the maximum, the map will center around the player every move.", max_view_x, 4 * BLOCK_HGT), 5, display_width() - 5);
				}
			}
		}

		/* Highlight current option */
		move_cursor(k + 2, 48);


		/* Get key */
		ch = inkey(FALSE);

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
			(void)Term_clear();
		}

		/* Previous option */
		else if ((ch == '8') || (ch == 'k')) k = (n + k - 1) % n;

		/* Next option */
		else if ((ch == '2') || (ch == ' ') || (ch == '\n') ||
		         (ch == '\r') || (ch == '\t') || (ch == 'j'))
		{
			k = (n + k + 1) % n;
		}

		/* Turn on this option */
		else if ((ch == 'Y') || (ch == 'y') || (ch == '6') || (ch == 'l'))
		{
			if (k == 0)
			{
				more_tall_display = TRUE;
				*modified = TRUE;

				k = (k + 1) % n;
			}

			else if (k == 1)
			{
				map_display_precise_fit = TRUE;
				*modified = TRUE;

				k = (k + 1) % n;
			}

			else continue;
		}

		/* Turn off this option */
		else if ((ch == 'N') || (ch == 'n') || (ch == '4') || (ch == 'h'))
		{
			if (k == 0)
			{
				more_tall_display = FALSE;
				*modified = TRUE;

				k = (k + 1) % n;
			}

			else if (k == 1)
			{
				map_display_precise_fit = FALSE;
				*modified = TRUE;

				k = (k + 1) % n;
			}

			else continue;
		}

		/* Decrease this quantity */
		else if ((ch == '-') || (ch == '_'))
		{
			if (k == 0) continue;
			else if (k == 1) continue;
			else if (k == 2)
			{
				clear_y--;
				if (clear_y < 2) clear_y = 2;
			}
			else if (k == 3)
			{
				clear_x--;
				if (clear_x < 2) clear_x = 2;
			}
			*modified = TRUE;
		}

		/* Increase this quantity */
		else if ((ch == '=') || (ch == '+'))
		{
			if (k == 0) continue;
			else if (k == 1) continue;
			else if (k == 2)
			{
				clear_y++;
				if (clear_y > max_view_y) clear_y = max_view_y;
			}
			else if (k == 3)
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
	int option_avail[DISPLAY_MAX][2];

	/* Get the maximum number of left panel slots available */
	int slots_avail = MIN(80, term_main->rows - ROW_CUSTOM - 2);

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

	/* Center an 80 column view, using the tall display only if necessary and allowed */
	if ((more_tall_display) && (num >= Term->rows - 3))
	{
		display_change(DSP_REMEMBER | DSP_CLEAR | DSP_TALL | DSP_CX, 80, 0);
	}
	else
	{
		display_change(DSP_REMEMBER | DSP_CLEAR | DSP_NORM | DSP_CX, 80, 0);
	}

	/* Prompt */
	prt("Left Panel (<dir> to move, Return to set/unset, +/- to swap, !, or ESC)", 0, 0);


	/* Print the headers */
	c_prt(TERM_WHITE, "Available Displays", 2, 1);
	c_prt(TERM_WHITE, "Displays Shown on Left Panel", 2, 41);

	/* Separator line */
	c_prt(TERM_L_WHITE, "==============================================================================", 3, 1);



	/* Repeat until satisfied */
	while (TRUE)
	{
		bool flg_avail = FALSE;

		/* Clear most everything */
		clear_from(4);

		/* Display the list of available options */
		for (i = 0; i < num; i++)
		{
			if (option_avail[i][1])
			{
				c_prt(TERM_SLATE, custom_display_text[option_avail[i][0]],
					4 + i, 1);
			}
			else if ((left_column) && (selected == i))
			{
				c_prt(TERM_L_BLUE, custom_display_text[option_avail[i][0]],
					4 + i, 1);
			}
			else
			{
				prt(custom_display_text[option_avail[i][0]], 4 + i, 1);
			}
		}

		/* Print a separator line */
		for (i = 4; i < Term->rows - 1; i++)
		{
			(void)Term_putstr(39, i, 2, TERM_L_WHITE, "|");
		}

		/* Display the left panel layout */
		for (i = 0; i <= slots_avail; i++)
		{
			/* Make it clear that we have only limited space */
			if (i == slots_avail)
			{
				c_prt(TERM_L_WHITE, " - - - - - - - - - - - - -",
					4 + i, 41);
			}

			/* We are in the right column, and this slot is selected */
			else if ((!left_column) && (selected == i))
			{
				if (custom_display[i] == 0)
				{
					c_prt(TERM_L_BLUE, " -----", 4 + i, 41);
				}
				else
				{
					c_prt(TERM_L_BLUE, custom_display_text[custom_display[i]],
						4 + i, 41);
				}
			}

			/* Notice where the next option will fit */
			else if ((left_column) && (custom_display[i] == 0) && (!flg_avail))
			{
				c_prt(TERM_L_BLUE, "  (+)", 4 + i, 41);
				flg_avail = TRUE;
			}

			/* Usual case -- display the current option, if any */
			else
			{
				prt(custom_display_text[custom_display[i]], 4 + i, 41);
			}
		}

		/* Print the navigation bar -- highlight set #2 */
		prt_options_screen_navigation(Term->rows - 1, 2);

		/* Get key */
		ch = inkey(FALSE);

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
			show_file("options.txt#Customized left panel rows :", NULL, 0, 1);
			(void)Term_clear();
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
			ch = toupper(ch);

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

	/* Restore previous display */
	display_change(DSP_RESTORE, 0, 0);

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
			/* Interact with layout options */
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
	bool dummy;

	int opt[OPT_PAGE_PER];

	char buf[DESC_LEN];


	/* Scan the options */
	for (i = 0; i < OPT_PAGE_PER; i++)
	{
		/* Skip some options when the character is not being born */
		if (character_generated)
		{
			/* Skip options in the "birth" group */
			if ((option_page[page][i] >= OPT_BIRTH) &&
			    (option_page[page][i] < OPT_MAX)) continue;
		}

		/* Collect options on this "page" */
		if (option_page[page][i] < OPT_MAX)
		{
			opt[n++] = option_page[page][i];
		}
	}

	/* Clear screen */
	(void)Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt */
		(void)strnfmt(buf, sizeof(buf), "%s (RET to advance, y/n to set, ESC to accept) ", info);
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
				(void)strnfmt(buf, sizeof(buf), "%-60s", option_desc[opt[i]]);

				prt(buf, i + 2, 0);

				/* Move the cursor down if necessary */
				if (i == k) k++;
			}

			/* Print the option normally */
			else
			{
				/* Build the option text */
				(void)strnfmt(buf, sizeof(buf), "%-46s: %s     (%s)",
					option_desc[opt[i]],
					op_ptr->opt[opt[i]] ? "yes" : "no ",
					option_text[opt[i]]);

				c_prt(a, buf, i + 2, 0);
			}
		}

		/* Highlight current option */
		move_cursor(k + 2, 48);

		/* Get a key */
		ch = inkey(FALSE);

		/* Allow direction and roguelike keys */
		get_ui_direction(&ch, UI_NODIAG, &dummy);

		/* Clear out any lingering messages */
		prt("", 24, 2);

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				return;
			}

			case '8':
			{
				k = (n + k - 1) % n;

				while (option_desc[opt[k]][0] == ' ') k = (n + k - 1) % n;
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '\t':
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

			case '=':
			case '+':
			case 'y':
			case '6':
			{
				op_ptr->opt[opt[k]] = TRUE;

				*modified = TRUE;

				k = (k + 1) % n;
				break;
			}

			case '-':
			case '_':
			case 'n':
			case '4':
			{
				if (opt[k] == OPT_no_skill_cap && calc_max_power() == 100)
				{
					prt("You cannot turn this option off once you have reached power 100.", 24, 2);
					break;
				}

				op_ptr->opt[opt[k]] = FALSE;

				*modified = TRUE;

				k = (k + 1) % n;
				break;
			}

			case '?':
			{
				(void)strnfmt(buf, sizeof(buf), "options.txt#[%s]", option_text[opt[k]]);
				show_file(buf, NULL, 0, 1);
				(void)Term_clear();
				break;
			}

			default:
			{
				bell("Illegal command for normal options!");
				break;
			}
		}

		/* Some options cannot be combined */
		if (ironman_play) op_ptr->opt[OPT_beginner_play] = FALSE;

		/* Adjust character type as requested */
		if (beginner_play)      p_ptr->character_type = PCHAR_BEGINNER;
		else if (ironman_play)  p_ptr->character_type = PCHAR_IRONMAN;
		else                    p_ptr->character_type = PCHAR_NORMAL;
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

	(void)strnfmt(expected_line, sizeof(expected_line),
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
					(void)strnfmt(expected_line, sizeof(expected_line),
					        "%s end %s", dump_seperator, mark);

					between_marks = TRUE;

					/* There are some changes */
					changed = TRUE;
				}
				else
				{
					/* Expect a header next - XXX shouldn't happen */
					(void)strnfmt(expected_line, sizeof(expected_line),
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
			/* Copy original line */
			fprintf(tmp_fff, "%s\n", buf);
		}
	}

	/* Close files */
	(void)my_fclose(orig_fff);
	(void)my_fclose(tmp_fff);

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

		(void)my_fclose(orig_fff);
		(void)my_fclose(tmp_fff);
	}

	/* Kill the temporary file */
	(void)fd_kill(tmp_file);
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
	char tmp[DESC_LEN];

	/* Default */
	strcpy(tmp, "");

	/* Ask for a "user pref command" */
	if (!get_string("Pref:", tmp, sizeof(tmp))) return;

	/* Process that pref command */
	(void)process_pref_file_command(tmp);
}


/*
 * Ask for a "user pref file" and process it.
 *
 * This function should only be used by standard interaction commands,
 * in which a standard "Command:" prompt is present on the given row.
 */
static void do_cmd_pref_file_hack(int row, cptr file_name)
{
	char ftmp[DESC_LEN];

	/* Prompt */
	prt("Command: Load a user pref file", row, 0);

	/* Prompt */
	prt("File: ", row + 2, 0);

	/* Default filename */
	if (!file_name) (void)strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

	/* Specific filename */
	else (void)strnfmt(ftmp, sizeof(ftmp), "%s.prf", file_name);

	/* Ask for a file (or cancel) */
	if (!askfor_aux(ftmp, sizeof(ftmp), TRUE)) return;

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
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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

	/* Dump sub-window flags */
	for (i = TERM_SUBWINDOW; i < TERM_MAX; i++)
	{
		bool window_flag = FALSE;

		/* No term data */
		if (!angband_term[i]) continue;

		/* Check each flag */
		for (j = 0; j < 32; j++)
		{
			/* Require a real flag */
			if (!window_flag_desc[j]) continue;

			/* Dump the flag -- only one flag affects a window */
			if (op_ptr->window_flag[i] & (1L << j))
			{
				/* Comment */
				fprintf(fff, "# Window '%s', Flag '%s'\n",
					angband_term_name[i], window_flag_desc[j]);

				/* Values */
				fprintf(fff, "W:%d:%d:1\n\n", i, j);

				/* Notice active display */
				window_flag = TRUE;
				break;
			}
		}

		/* This sub-window has no displays */
		if (!window_flag)
		{
			/* Comment */
			fprintf(fff, "# Window '%s', No display flags\n", angband_term_name[i]);

			/* Cancel ALL displays */
			fprintf(fff, "W:%d:-1:0\n\n", i);
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
		fprintf(fff, "# Screen  (zero, more_tall_display, map_display_precise_fit, y-margin, x-margin)\n");
		fprintf(fff, "t:%c:%c:%c:%d:%d\n\n",
			'0', (more_tall_display ? '1' : '0'),
			(map_display_precise_fit ? '1' : '0'), clear_y, clear_x);
	}

	/* Skip a line */
	fprintf(fff, "\n");

	/* Output footer */
	pref_footer(fff, mark);

	/* Close */
	(void)my_fclose(fff);

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
	bool change = FALSE;


	/* Save the screen, center it, and use the standard display */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_NORM | DSP_CX, 80, 0);


	/* Interact */
	while (TRUE)
	{
		/* Clear screen */
		(void)Term_clear();

		/* Why are we here */
		prt(format("%s options:", VERSION_NAME), 2, 0);

		/* Give some choices */
		prt("(1) Game Behavior", 4, 5);
		prt("(2) Disturbance and Warning", 5, 5);
		prt("(3) Visual Interface", 6, 5);
		prt("(4) Screen Display", 7, 5);
		prt("(5) Difficulty", 8, 5);

		/* Special choices */
		if (Term->user_hook) prt("(!) User Interface", 10, 5);
		prt("(D) Base Delay Factor", 11, 5);
		prt("(H) Hitpoint Warning", 12, 5);
		prt("(A) Autosave Options", 13, 5);

		/* Save and load */
		prt("(S) Save as default options", 15, 5);
		prt("(L) Load an options file", 16, 5);

		/* Get help */
		prt("(?) Get help", 18, 5);

		/* Prompt */
		prt("Command: ", 20, 0);

		/* Get command */
		ch = inkey(FALSE);

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
			do_cmd_options_screen(&modified, &change);
		}

		/* Difficulty Options */
		else if (ch == '5')
		{
			do_cmd_options_aux(4, "Difficulty Options", &modified);
		}

		/* User Interface Options */
		else if (ch == '!')
		{
			(void)Term_user(0);

			/* Re-calculate display centering */
			screen_center_x(80);
		}

		/* Load a user pref file */
		else if ((ch == 'L') || (ch == 'l'))
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(20, "user");

			/* XXX XXX -- Assume no modifications remain */
			modified = FALSE;
		}

		/* Save options to file */
		else if ((ch == 'S') || (ch == 's'))
		{
			char ftmp[DESC_LEN];


			/* Prompt */
			prt("Command: Save options to a file", 20, 0);

			/* Prompt */
			prt("File: ", 22, 0);

			/* Display some helpful text on recent changes */
			(void)Term_gotoxy(45, 15);

			c_roff(TERM_L_WHITE, "The current options are automatically stored in your savefile when you next save the game.  You only need to save options explicitly if you want to create a default set for new characters.  For more details, consult the online help.", 40, display_width());

			(void)Term_gotoxy(6, 22);

			/* Default filename */
			(void)strnfmt(ftmp, sizeof(ftmp), "user.prf");

			/* Ask for a file */
			if (!askfor_aux(ftmp, sizeof(ftmp), FALSE)) continue;

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

				cx = inkey(FALSE);
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
				int amt;
				int msec = op_ptr->delay_factor * op_ptr->delay_factor;
				char buf[DESC_LEN];

				/* Prompt */
				prt(format("Current base delay factor: %d (%d msec)",
					op_ptr->delay_factor, msec), 23, 0);
				prt("New base delay factor (0-30 or Return to accept): ", 22, 0);

				/* Build the default */
				(void)strnfmt(buf, sizeof(buf), "%d", op_ptr->delay_factor);

				/* Ask the user for a string, allow cancel */
				if (!askfor_aux(buf, 3, FALSE)) break;

				/* Extract a number */
				amt = atoi(buf);

				/* Enforce the maximum */
				if (amt > 30) amt = 30;

				/* Enforce the minimum */
				if (amt < 0) amt = 0;

				/* Note no change */
				if (amt == op_ptr->delay_factor) break;

				/* Set delay factor */
				op_ptr->delay_factor = amt;
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

				cx = inkey(FALSE);
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
			(void)Term_clear();
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
	if (change)
	{
		/* Adjust the panels if necessary */
		verify_panel(5, FALSE);
	}

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

	/* Do a complete screen refresh  XXX XXX */
	if (modified) do_cmd_redraw();
}



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
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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
	(void)my_fclose(fff);

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
	ch = inkey(FALSE);

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
		ch = inkey(FALSE);
	}

	/* Terminate */
	buf[n] = '\0';

	/* Flush */
	flush();


	/* Convert the trigger */
	ascii_to_text(tmp, sizeof(tmp), buf);

	/* Hack -- display the trigger */
	(void)Term_addstr(-1, TERM_WHITE, tmp);
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
	buf[0] = inkey(FALSE);
	buf[1] = '\0';


	/* Convert to ascii */
	ascii_to_text(tmp, sizeof(tmp), buf);

	/* Hack -- display the trigger */
	(void)Term_addstr(-1, TERM_WHITE, tmp);


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
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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
	(void)my_fclose(fff);

	/* Success */
	return (0);
}


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

	/* Save the screen, center it, and use the standard display */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX, 80, 0);


	/* Process requests until done */
	while (TRUE)
	{
		/* Clear screen */
		(void)Term_clear();

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

		/* Prompt */
		prt("Command: ", 16, 0);

		/* Get a command */
		ch = inkey(FALSE);

		/* Leave */
		if (ch == ESCAPE) break;

		/* Load a user pref file */
		if (ch == '1')
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(16, "user");
		}

		/* Save macros */
		else if (ch == '2')
		{
			char ftmp[DESC_LEN];

			/* Prompt */
			prt("Command: Append macros to a file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			(void)strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, 80, FALSE)) continue;

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
			if (askfor_aux(tmp, sizeof(tmp), FALSE))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);

				/* Link the macro */
				(void)macro_add(pat, macro_buffer);

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
			(void)macro_add(pat, pat);

			/* Prompt */
			msg_print("Removed a macro.");

			/* A macro has been "removed" */
			changed_macro = TRUE;
		}

		/* Save keymaps */
		else if (ch == '6')
		{
			char ftmp[DESC_LEN];

			/* Prompt */
			prt("Command: Append keymaps to a file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

			/* Default filename */
			(void)strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

			/* Ask for a file */
			if (!askfor_aux(ftmp, sizeof(tmp), FALSE)) continue;

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
			if (askfor_aux(tmp, sizeof(tmp), FALSE))
			{
				/* Convert to ascii */
				text_to_ascii(macro_buffer, sizeof(macro_buffer), tmp);

				/* Free old keymap */
				(void)string_free(keymap_act[mode][(byte)(pat[0])]);

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
			(void)string_free(keymap_act[mode][(byte)(pat[0])]);

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
			(void)Term_gotoxy(0, 22);

			/* Analyze the current action */
			ascii_to_text(tmp, sizeof(tmp), macro_buffer);

			/* Get an encoded action */
			if (askfor_aux(tmp, sizeof(tmp), FALSE))
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

		/* Oops */
		else
		{
			/* Oops */
			bell("Illegal command for macros!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);
}

/*
 * Display helpful information about characters and colors.
 */
static void do_cmd_visual_info(cptr command)
{
	int i;

	(void)Term_clear();


	/* Handle requests for special displays */
	if (command)
	{
		/* Help me edit features */
		if (strstr(command, "help-features"))
		{
			/* Set cursor to line 1 */
			move_cursor(1, 0);

			/* Print help */
			roff("     Each terrain has three lighting values:  Normal, bright, and dim.  If lighting effects are turned off (in game options), only normal is used.  Otherwise:\n- Bright is used when torch light is turned on, you can see the grid directly, and it is lit.   If \"lights up under torch light only\" is set, only torch light counts.  Otherwise, any light does.\n- Dim is used when you cannot see the grid directly.\n- Normal is used in all other cases.\n\n     Typing '!' toggles \"lights up under torch light only\".\n\n     Press any key to continue editing.", 0, display_width());
		}

		/* Done */
		return;
	}


	/* List all the available characters in the current font */
	for (i = 0; i < 256; i++)
	{
		if (i % 32 == 0) (void)Term_putstr(1, 2 + i / 32, 3, TERM_SLATE, format("%3d", i));

		(void)Term_putch(9 + (2 * (i % 32)), 2 + i / 32, TERM_WHITE, (char)i);

		if (i % 32 == 31) (void)Term_putstr(76, 2 + i / 32, 3, TERM_SLATE, format("%3d", i));
	}


	/* List some colors */
	for (i = 0; i < max_system_colors; i++)
	{
		/* Stop on first color without an index character */
		if (!color_table[i].index_char) break;

		if (i % 32 == 0) (void)Term_putstr(1, 11 + i / 32, 3, TERM_SLATE, format("%3d", i));

		(void)Term_putch(9 + (2 * (i % 32)), 11 + i / 32, i, '#');

		if (i % 32 == 31) (void)Term_putstr(76, 11 + i / 32, 3, TERM_SLATE, format("%3d", i));
	}
}


/*
 * Change visuals.
 */
static int do_cmd_visual_change(cptr str, int max_idx, int *cur_idx, int *move,
	byte *da, byte *dc, byte *xa, byte *xc)
{
	char ch;


	/* Save starting values */
	byte xa0 = *xa;
	byte xc0 = *xc;

	/* Limit to attr values (allow high bits only if using graphics) */
	byte max_attr = (use_graphics) ? 255 : max_system_colors - 1;

	/* Assume no movement */
	*move = 0;


	/* Label the default values */
	(void)Term_putstr(10, 20, -1, TERM_WHITE,
		format("Default attr/char = %3u / %3u", *da, *dc));
	(void)Term_putstr(40, 20, -1, TERM_WHITE, "<< ? >>");
	(void)Term_putch(43, 20, *da, *dc);

	/* Label the current values */
	(void)Term_putstr(10, 22, -1, TERM_WHITE,
		format("Current attr/char = %3u / %3u", *xa, *xc));
	(void)Term_putstr(40, 22, -1, TERM_WHITE, "<< ? >>");
	(void)Term_putch(43, 22, *xa, *xc);


	/* Prompt */
	(void)Term_putstr(0, 16, -1, TERM_WHITE,
		format("Edit: %s (index: n/N/^n, attr: a/A/^a, char: c/C/^c): ", str));

	/* Get a command */
	ch = inkey(FALSE);


	/* Accept changes */
	if ((ch == ESCAPE) || (ch == '\n') || (ch == '\r')) return (-1);

	/* Move to another item */
	if (ch == 'n')
	{
		if (*cur_idx == max_idx) *cur_idx = 0;
		else (*cur_idx)++;
		*move = 1;
	}
	else if (ch == 'N')
	{
		if (*cur_idx == 0) *cur_idx = max_idx;
		else (*cur_idx)--;
		*move = -1;
	}
	else if (ch == KTRL('N'))
	{
		/* Move cursor to row 16, column 0, and suggest the current value */
		move_cursor(16, 0);
		get_quantity_default = *cur_idx;

		/* Get a quantity */
		*cur_idx = get_quantity(format("Jump to which index (0 - %d)?",
			max_idx), 0, max_idx);

		/* Jump to this index exactly -- no movement */
	}

	/* Change attribute */
	else if (ch == 'a')
	{
		if (*xa == max_attr) *xa = 0;
		else (*xa)++;
	}
	else if (ch == 'A')
	{
		if (*xa == 0) *xa = max_attr;
		else (*xa)--;
	}
	else if (ch == KTRL('A'))
	{
		/* Move cursor to row 16, column 0, and suggest the current value */
		move_cursor(16, 0);
		get_quantity_default = *xa;

		/* Get a quantity */
		*xa = get_quantity(format("Assign which attribute (0 - %d)?",
			max_attr), 0, max_attr);
	}

	/* Change character */
	else if (ch == 'c')
	{
		if (*xc == 255) *xc = 0;
		else (*xc)++;
	}
	else if (ch == 'C')
	{
		if (*xc == 0) *xc = 255;
		else (*xc)--;
	}
	else if (ch == KTRL('C'))
	{
		/* Move cursor to row 16, column 0, and suggest the current value */
		move_cursor(16, 0);
		get_quantity_default = *xc;

		/* Get a quantity */
		*xc = get_quantity(format("Assign which character (0 - %d)?",
			255), 0, 255);
	}

	/* Notice changes */
	if ((*xa != xa0) || (*xc != xc0)) return (1);

	/* No changes */
	return (0);
}

static FILE *fff_visuals = NULL;


/*
 * Open a file, remove old preferences, and print a header.
 */
static bool do_cmd_visuals_file(cptr str, bool automatic, char *mark)
{
	char ftmp[DESC_LEN];
	char buf[1024];

	/* Handle automatic usage */
	if (automatic)
	{
		/* Get a filename */
		strcpy(ftmp, "scrn_tmp.prf");
	}

	/* Handle manual usage */
	else
	{
		/* Prompt */
		prt(format("Command: Dump %s attr/chars", str), 16, 0);

		/* Prompt */
		prt("File: ", 18, 0);

		/* Default filename */
		(void)strnfmt(ftmp, sizeof(ftmp), "%s.prf", op_ptr->base_name);

		/* Get a filename, allow cancel */
		if (!askfor_aux(ftmp, sizeof(ftmp), FALSE)) return (FALSE);
	}

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Build a marker */
	strcpy(mark, format("%^s attr/char definitions", str));

	/* Remove old attr/chars */
	remove_old_dump(buf, mark);

	/* Append to the file */
	fff_visuals = my_fopen(buf, "a");

	/* Failure */
	if (!fff_visuals) return (FALSE);

	/* Output header */
	pref_header(fff_visuals, mark);

	/* Skip some lines */
	fprintf(fff_visuals, "\n\n");

	/* Success */
	return (TRUE);
}


/*
 * Macro -- Get a spell attribute.
 */
#define get_ga \
(g_ptr->attr_rdiag) ? g_ptr->attr_rdiag : spell_color(index)


/*
 * Interact with "visuals"
 *
 * Visuals currently include monster, object, feature, flavor, and effect
 * attributes and characters.  -BEN-, -LM-
 *
 * We will want to use the map term (if present) for this...
 */
void do_cmd_visuals(char cmd)
{
	int ch;
	byte attr;
	int index, result, move = 0;

	int i;

	/* Allow automatic usage */
	bool automatic = (isdigit(cmd) > 0);

	char mark[DESC_LEN];

	/* Keep track of changes */
	bool changed_monster = FALSE;
	bool changed_object = FALSE;
	bool changed_feature = FALSE;
	bool changed_flavor = FALSE;
	bool changed_spell = FALSE;


	/* Save the screen and center it */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_CX, 80, 0);


	/* Interact until done */
	while (TRUE)
	{
		/* Clear screen */
		(void)Term_clear();

		/* Ask for a choice */
		prt("Interact with Visuals", 1, 0);

		/*
		 * Note:  If you change the numbers used for commands, be
		 * sure also to change the code in "do_cmd_save_screen()".
		 */

		/* Give some choices */
		prt("(%) Load a user pref file", 3, 5);

		prt("(1) Change monster attr/chars", 5, 5);
		prt("(2) Change object attr/chars", 6, 5);
		prt("(3) Change feature attr/chars", 7, 5);
		prt("(4) Change flavor attr/chars", 8, 5);
		prt("(5) Change spell attr/chars", 9, 5);

		attr = (changed_monster ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(6) Dump monster attr/chars", 5, 45);
		attr = (changed_object ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(7) Dump object attr/chars", 6, 45);
		attr = (changed_feature ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(8) Dump feature attr/chars", 7, 45);
		attr = (changed_flavor ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(9) Dump flavor attr/chars", 8, 45);
		attr = (changed_spell ? TERM_YELLOW : TERM_WHITE);
		c_prt(attr, "(0) Dump spell attr/chars", 9, 45);

		prt("(!) Reset visuals", 11, 5);
		prt("(?) Get help", 11, 45);

		/* Handle automatic usage */
		if (automatic)
		{
			/* Accept commands only if legal, otherwise do nothing */
			if (isdigit(cmd)) ch = cmd;
			else
			{
				break;
			}

			/* Issue automatic command only once */
			cmd = '\0';
		}

		/* Handle manual usage */
		else
		{
			/* Prompt */
			prt("Command: ", 16, 0);

			/* Prompt */
			ch = inkey(FALSE);
		}

		/* Leave */
		if (ch == ESCAPE) break;


		/* Load a user pref file */
		if (ch == '%')
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(16, "user");
		}


		/* Modify monster attr/chars */
		else if (ch == '1')
		{
			index = 0;

			/* Display various information */
			do_cmd_visual_info(NULL);

			/* Query until done */
			while (TRUE)
			{
				monster_race *r_ptr = &r_info[index];

				/* Label the entry */
				(void)Term_putstr(5, 18, -1, TERM_WHITE,
					format("Monster = %d, Name = %-40.40s",
					index, (r_name + r_ptr->name)));

				/* Edit the entry */
				result = do_cmd_visual_change("Monster attr/chars",
					z_info->r_max - 1, &index, &move,
					(byte *)&r_ptr->d_attr, (byte *)&r_ptr->d_char,
					(byte *)&r_ptr->x_attr, (byte *)&r_ptr->x_char);

				/* Allow accept */
				if (result < 0) break;

				/* Notice changes */
				if (result > 0) changed_monster = TRUE;


				/* Scan to the next legal monster */
				if (move > 0)
				{
					while (!r_info[index].name)
					{
						if (++index >= z_info->r_max) index = 0;
					}
				}

				/* Scan to the previous legal monster */
				else if (move < 0)
				{
					while (!r_info[index].name)
					{
						if (--index < 0) index = z_info->r_max - 1;
					}
				}
			}
		}


		/* Modify object attr/chars */
		else if (ch == '2')
		{
			index = 0;

			/* Display various information */
			do_cmd_visual_info(NULL);

			/* Query until done */
			while (TRUE)
			{
				object_kind *k_ptr = &k_info[index];

				/* Label the entry */
				(void)Term_putstr(5, 18, -1, TERM_WHITE,
					format("Object = %d, Name = %-40.40s",
					index, (k_name + k_ptr->name)));

				/* Edit the entry */
				result = do_cmd_visual_change("Object attr/chars",
					z_info->k_max - 1, &index, &move,
					(byte *)&k_ptr->d_attr, (byte *)&k_ptr->d_char,
					(byte *)&k_ptr->x_attr, (byte *)&k_ptr->x_char);

				/* Allow accept */
				if (result < 0) break;

				/* Notice changes */
				if (result > 0) changed_object = TRUE;


				/* Scan to the next legal, unflavored object */
				if (move > 0)
				{
					while ((!k_info[index].name) || (k_info[index].flavor))
					{
						if (++index >= z_info->k_max) index = 0;
					}
				}

				/* Scan to the previous legal, unflavored object */
				else if (move < 0)
				{
					while ((!k_info[index].name) || (k_info[index].flavor))
					{
						if (--index < 0) index = z_info->k_max - 1;
					}
				}
			}
		}


		/* Modify feature attr/chars */
		else if (ch == '3')
		{
			byte max_attr = (use_graphics) ? 255 : max_system_colors - 1;

			int max_pair = 2;
			int old_pair;

			int move;

			int pair = 0;
			char ch;
			byte fa = 0, fc = 0;

			bool dummy;

			/* Start at the first legal feature */
			index = 1;

			/* Display various information */
			do_cmd_visual_info(NULL);


			/* Query until done */
			while (TRUE)
			{
				feature_type *f_ptr = &f_info[index];

				/* No movement */
				move = 0;
				old_pair = pair;

				/* Label the entry */
				(void)Term_putstr(5, 18, -1, TERM_WHITE,
					format("Terrain = %d, Name = %-40.40s",
					index, (f_name + f_ptr->name)));

				/* Label the current values */
				(void)Term_putstr(10, 20, -1, TERM_WHITE,
					format("normal = %3u / %3u,  bright = %3u / %3u,  dim  = %3u / %3u",
					f_ptr->x_attr, (byte)f_ptr->x_char, f_ptr->x_attr_lit,
					(byte)f_ptr->x_char_lit, f_ptr->x_attr_dim,
					(byte)f_ptr->x_char_dim));

				/* Label the selected pair */
				if (pair == 0) (void)Term_putstr(10, 20, -1, TERM_L_BLUE, "normal");
				if (pair == 1) (void)Term_putstr(31, 20, -1, TERM_L_BLUE, "bright");
				if (pair == 2) (void)Term_putstr(52, 20, -1, TERM_L_BLUE, "dim");

				/* Display the current values */
				(void)Term_putstr(20, 22, -1, TERM_WHITE, "<< ? >>");
				(void)Term_putstr(41, 22, -1, TERM_WHITE, "<< ? >>");
				(void)Term_putstr(60, 22, -1, TERM_WHITE, "<< ? >>");
				(void)Term_putch(23, 22, f_ptr->x_attr, f_ptr->x_char);
				(void)Term_putch(44, 22, f_ptr->x_attr_lit, f_ptr->x_char_lit);
				(void)Term_putch(63, 22, f_ptr->x_attr_dim, f_ptr->x_char_dim);

				/* Note torch-only bright lighting */
				if (f_ptr->flags & (TF_TORCH_ONLY))
					(void)Term_putstr(27, 23, -1, TERM_L_WHITE,
					"(bright under torch light only)");
				else
					(void)Term_putstr(27, 23, -1, TERM_L_WHITE,
					"                               ");

				/* Prompt */
				(void)Term_putstr(0, 16, -1, TERM_WHITE,
					"Edit feature: (index: n/N/^n, attr: a/A/^a, char: c/C/^c, <dir>, '!', or '?'): ");

				/* Store current values */
				if (pair == 0)
				{
					fa = f_ptr->x_attr;
					fc = (byte)f_ptr->x_char;
				}
				else if (pair == 1)
				{
					fa = f_ptr->x_attr_lit;
					fc = (byte)f_ptr->x_char_lit;
				}
				else if (pair == 2)
				{
					fa = f_ptr->x_attr_dim;
					fc = (byte)f_ptr->x_char_dim;
				}

				/* Wait for legal input */
				while (TRUE)
				{
					/* Get key */
					ch = inkey(FALSE);

					/* Allow orthogonal direction keys */
					get_ui_direction(&ch, UI_NODIAG | UI_NOMOUSE, &dummy);

					/* Accept anything except untranslated mouse actions */
					if (ch != MOUSEKEY) break;
				}

				/* Accept changes */
				if ((ch == ESCAPE) || (ch == '\n') || (ch == '\r')) break;


				/* Move to another item */
				if (ch == 'n')
				{
					if (index >= z_info->f_max - 1) index = 0;
					else index++;

					while ((f_info[index].mimic != index) || (!f_info[index].name))
					{
						if (++index > z_info->f_max - 1) index = 0;
					}
				}
				else if (ch == 'N')
				{
					if (index <= 0) index = z_info->f_max - 1;
					else index--;

					while ((f_info[index].mimic != index) || (!f_info[index].name))
					{
						if (--index < 0) index = z_info->f_max - 1;
					}
				}
				else if (ch == KTRL('N'))
				{
					/* Move cursor, suggest the current value */
					move_cursor(16, 0);
					get_quantity_default = index;

					/* Get a quantity */
					index = get_quantity(format("Jump to which index (0 - %d)?",
						z_info->f_max - 1), 0, z_info->f_max - 1);

					/* Use this exact value */
				}

				/* Change pair */
				else if (ch == '4')
				{
					pair--;
					if (pair < 0) pair = max_pair;
				}
				else if (ch == '6')
				{
					pair++;
					if (pair > max_pair) pair = 0;
				}

				/* Change attribute */
				else if (ch == 'a')
				{
					if (fa == max_attr) fa = 0;
					else fa++;
				}
				else if (ch == 'A')
				{
					if (fa == 0) fa = max_attr;
					else fa--;
				}
				else if (ch == KTRL('A'))
				{
					/* Move cursor, suggest the current value */
					move_cursor(16, 0);
					get_quantity_default = fa;

					/* Get a quantity */
					fa = get_quantity(format("Assign which attribute (0 - %d)?",
						max_attr), 0, max_attr);
				}

				/* Change character */
				else if (ch == 'c')
				{
					if (fc == 255) fc = 0;
					else fc++;
				}
				else if (ch == 'C')
				{
					if (fc == 0) fc = 255;
					else fc--;
				}
				else if (ch == KTRL('C'))
				{
					/* Move cursor, suggest the current value */
					move_cursor(16, 0);
					get_quantity_default = fc;

					/* Get a quantity */
					fc = get_quantity(format("Assign which character (0 - %d)?",
						255), 0, 255);
				}

				/* Change special lighting settings */
				else if (ch == '!')
				{
					/* Toggle the "torch only" setting */
					if (f_ptr->flags & (TF_TORCH_ONLY))
						f_ptr->flags &= ~(TF_TORCH_ONLY);
					else
						f_ptr->flags |= (TF_TORCH_ONLY);
				}

				else if (ch == '?')
				{
					/* Display help on features */
					do_cmd_visual_info("help-features");

					/* Wait for it */
					(void)inkey(ALLOW_CLICK);

					/* Display standard information */
					do_cmd_visual_info(NULL);
				}

				/* Save modified values */
				if (pair == old_pair)
				{
					if (pair == 0)
					{
						if ((f_ptr->x_attr != fa) ||
						    (f_ptr->x_char != (char)fc)) changed_feature = TRUE;
						f_ptr->x_attr = fa;
						f_ptr->x_char = (char)fc;
					}
					else if (pair == 1)
					{
						if ((f_ptr->x_attr_lit != fa) ||
						    (f_ptr->x_char_lit != (char)fc)) changed_feature = TRUE;
						f_ptr->x_attr_lit = fa;
						f_ptr->x_char_lit = (char)fc;
					}
					else if (pair == 2)
					{
						if ((f_ptr->x_attr_dim != fa) ||
						    (f_ptr->x_char_dim != (char)fc)) changed_feature = TRUE;
						f_ptr->x_attr_dim = fa;
						f_ptr->x_char_dim = (char)fc;
					}
				}
			}
		}


		/* Modify flavor attr/chars */
		else if (ch == '4')
		{
			index = 0;

			/* Display various information */
			do_cmd_visual_info(NULL);

			/* Query until done */
			while (TRUE)
			{
				flavor_type *flavor_ptr = &flavor_info[index];

				/* Label the entry */
				(void)Term_putstr(5, 18, -1, TERM_WHITE,
					format("Flavor = %d, Name = %-40.40s",
					index, (flavor_text + flavor_ptr->text)));

				/* Edit the entry */
				result = do_cmd_visual_change("Flavor attr/chars",
					z_info->flavor_max - 1, &index, &move,
					(byte *)&flavor_ptr->d_attr, (byte *)&flavor_ptr->d_char,
					(byte *)&flavor_ptr->x_attr, (byte *)&flavor_ptr->x_char);

				/* Allow accept */
				if (result < 0) break;

				/* Notice changes */
				if (result > 0) changed_flavor = TRUE;


				/* Scan to the next legal flavor */
				if (move > 0)
				{
					while (!flavor_info[index].text)
					{
						if (++index >= z_info->flavor_max) index = 0;
					}
				}

				/* Scan to the previous legal flavor */
				else if (move < 0)
				{
					while (!flavor_info[index].text)
					{
						if (--index < 0) index = z_info->flavor_max - 1;
					}
				}
			}
		}

		/* Modify spell attr/chars */
		else if (ch == '5')
		{
			byte max_attr = (use_graphics) ? 255 : max_system_colors - 1;

			int max_pair = 4;
			int old_pair;

			int max_editable;

			int move;

			int pair = 0;
			char ch;
			byte ga = 0, gc;

			bool dummy;

			/* Start at the first legal spell */
			index = 1;

			/* Display various information */
			do_cmd_visual_info(NULL);

			/*
			 * For ease of use, only the more common projections can be
			 * edited in-game.  All of them can be edited in the pref
			 * files themselves.
			 */
			for (max_editable = 1; max_editable < 256; max_editable++)
			{
				if (projection_names[max_editable] == NULL) break;
			}
			max_editable--;

			/* Query until done */
			while (TRUE)
			{
				proj_graphics_type *g_ptr = &proj_graphics[index];

				/* No movement */
				move = 0;
				old_pair = pair;

				/* Label the entry */
				(void)Term_putstr(5, 18, -1, TERM_WHITE,
					format("Spell index = %d, Name = %-40.40s",
					index, projection_names[index]));

				/* Display one of four possible trails */
				gc = (pair == 0) ? g_ptr->char_vert : ' ';
				(void)Term_putch(74, 16, get_ga, gc);
				(void)Term_putch(74, 17, get_ga, gc);
				(void)Term_putch(74, 18, get_ga, gc);
				(void)Term_putch(74, 19, get_ga, gc);

				gc = (pair == 1) ? g_ptr->char_horiz : ' ';
				(void)Term_putch(68, 22, get_ga, gc);
				(void)Term_putch(69, 22, get_ga, gc);
				(void)Term_putch(70, 22, get_ga, gc);
				(void)Term_putch(71, 22, get_ga, gc);

				gc = (pair == 2) ? g_ptr->char_rdiag : ' ';
				(void)Term_putch(79, 17, get_ga, gc);
				(void)Term_putch(78, 18, get_ga, gc);
				(void)Term_putch(77, 19, get_ga, gc);
				(void)Term_putch(76, 20, get_ga, gc);

				gc = (pair == 3) ? g_ptr->char_ldiag : ' ';
				(void)Term_putch(69, 17, get_ga, gc);
				(void)Term_putch(70, 18, get_ga, gc);
				(void)Term_putch(71, 19, get_ga, gc);
				(void)Term_putch(72, 20, get_ga, gc);

				/* Display an explosion */
				gc = g_ptr->char_ball;
				for (i = 0; i < grids_in_radius[2]; i++)
				{
					int y = 22 + nearby_grids_y[i];
					int x = 74 + nearby_grids_x[i];
					(void)Term_putch(x, y, get_ga, gc);
				}

				/* Label the current values */
				(void)Term_putstr(10, 20, -1, TERM_WHITE,
					format("vert  = %3u / %3u,  horiz = %3u / %3u,  rdiag = %3u / %3u",
					g_ptr->attr_vert, (byte)g_ptr->char_vert, g_ptr->attr_horiz,
					(byte)g_ptr->char_horiz, g_ptr->attr_rdiag, (byte)g_ptr->char_rdiag));
				(void)Term_putstr(10, 21, -1, TERM_WHITE,
					format("ldiag = %3u / %3u,  ball  = %3u / %3u",
					g_ptr->attr_ldiag, (byte)g_ptr->char_ldiag,
					g_ptr->attr_ball, (byte)g_ptr->char_ball));

				/* Label the selected pair */
				if (pair == 0) (void)Term_putstr(10, 20, -1, TERM_L_BLUE, "vert");
				if (pair == 1) (void)Term_putstr(30, 20, -1, TERM_L_BLUE, "horiz");
				if (pair == 2) (void)Term_putstr(50, 20, -1, TERM_L_BLUE, "rdiag");
				if (pair == 3) (void)Term_putstr(10, 21, -1, TERM_L_BLUE, "ldiag");
				if (pair == 4) (void)Term_putstr(30, 21, -1, TERM_L_BLUE, "ball");

				/* Prompt */
				(void)Term_putstr(0, 16, -1, TERM_WHITE,
					"Edit spell: (index: n/N/^n, pair: <dir>, attr: a/A/^a, char: c/C/^c): ");

				/* Store current values */
				if (pair == 0)
				{
					ga = g_ptr->attr_vert;
					gc = (byte)g_ptr->char_vert;
				}
				else if (pair == 1)
				{
					ga = g_ptr->attr_horiz;
					gc = (byte)g_ptr->char_horiz;
				}
				else if (pair == 2)
				{
					ga = g_ptr->attr_rdiag;
					gc = (byte)g_ptr->char_rdiag;
				}
				else if (pair == 3)
				{
					ga = g_ptr->attr_ldiag;
					gc = (byte)g_ptr->char_ldiag;
				}
				else if (pair == 4)
				{
					ga = g_ptr->attr_ball;
					gc = (byte)g_ptr->char_ball;
				}


				/* Wait for legal input */
				while (TRUE)
				{
					/* Get key */
					ch = inkey(FALSE);

					/* Allow orthogonal direction keys */
					get_ui_direction(&ch, UI_NODIAG | UI_NOMOUSE, &dummy);

					/* Accept anything except untranslated mouse actions */
					if (ch != MOUSEKEY) break;
				}


				/* Accept changes */
				if ((ch == ESCAPE) || (ch == '\n') || (ch == '\r')) break;

				/* Move to another item */
				if (ch == 'n')
				{
					if (index >= max_editable) index = 1;
					else index++;

					while (!strlen(projection_names[index]))
					{
						if (++index > max_editable) index = 1;
					}
				}
				else if (ch == 'N')
				{
					if (index <= 1) index = max_editable;
					else index--;

					while (!strlen(projection_names[index]))
					{
						if (--index < 1) index = max_editable;
					}
				}
				else if (ch == KTRL('N'))
				{
					/* Move cursor, suggest the current value */
					move_cursor(16, 0);
					get_quantity_default = index;

					/* Get a quantity */
					index = get_quantity(format("Jump to which index (0 - %d)?",
						max_editable), 0, max_editable);

					/* Use this exact value */
				}

				/* Change pair */
				else if (ch == '4')
				{
					pair--;
					if (pair < 0) pair = max_pair;
				}
				else if (ch == '6')
				{
					pair++;
					if (pair > max_pair) pair = 0;
				}

				/* Change attribute */
				else if (ch == 'a')
				{
					if (ga == max_attr) ga = 0;
					else ga++;
				}
				else if (ch == 'A')
				{
					if (ga == 0) ga = max_attr;
					else ga--;
				}
				else if (ch == KTRL('A'))
				{
					/* Move cursor, suggest the current value */
					move_cursor(16, 0);
					get_quantity_default = ga;

					/* Get a quantity */
					ga = get_quantity(format("Assign which attribute (0 - %d)?",
						max_attr), 0, max_attr);
				}

				/* Change character */
				else if (ch == 'c')
				{
					if (gc == 255) gc = 0;
					else gc++;
				}
				else if (ch == 'C')
				{
					if (gc == 0) gc = 255;
					else gc--;
				}
				else if (ch == KTRL('C'))
				{
					/* Move cursor, suggest the current value */
					move_cursor(16, 0);
					get_quantity_default = gc;

					/* Get a quantity */
					gc = get_quantity(format("Assign which character (0 - %d)?",
						255), 0, 255);
				}

				/* Save modified values */
				if (pair == old_pair)
				{
					if (pair == 0)
					{
						if ((g_ptr->attr_vert != ga) ||
						    (g_ptr->char_vert != (char)gc)) changed_spell = TRUE;
						g_ptr->attr_vert = ga;
						g_ptr->char_vert = (char)gc;
					}
					else if (pair == 1)
					{
						if ((g_ptr->attr_horiz != ga) ||
						    (g_ptr->char_horiz != (char)gc)) changed_spell = TRUE;
						g_ptr->attr_horiz = ga;
						g_ptr->char_horiz = (char)gc;
					}
					else if (pair == 2)
					{
						if ((g_ptr->attr_rdiag != ga) ||
						    (g_ptr->char_rdiag != (char)gc)) changed_spell = TRUE;
						g_ptr->attr_rdiag = ga;
						g_ptr->char_rdiag = (char)gc;
					}
					else if (pair == 3)
					{
						if ((g_ptr->attr_ldiag != ga) ||
						    (g_ptr->char_ldiag != (char)gc)) changed_spell = TRUE;
						g_ptr->attr_ldiag = ga;
						g_ptr->char_ldiag = (char)gc;
					}
					else if (pair == 4)
					{
						if ((g_ptr->attr_ball != ga) ||
						    (g_ptr->char_ball != (char)gc)) changed_spell = TRUE;
						g_ptr->attr_ball = ga;
						g_ptr->char_ball = (char)gc;
					}
				}
			}
		}


		/* Dump monster attr/chars */
		else if (ch == '6')
		{
			/* Attempt to open, clean up, and print a header to a file. */
			if (!do_cmd_visuals_file("monster", automatic, mark))
			{
				/* Notice failure or cancel */
				continue;
			}

			/* Dump monsters */
			for (i = 0; i < z_info->r_max; i++)
			{
				monster_race *r_ptr = &r_info[i];

				/* Skip non-entries */
				if (!r_ptr->name) continue;

				/* Dump a comment */
				fprintf(fff_visuals, "# %s\n", (r_name + r_ptr->name));

				/* Dump the monster attr/char info */
				fprintf(fff_visuals, "R:%d:%c:%d\n\n", i,
					color_table[r_ptr->x_attr].index_char, (byte)(r_ptr->x_char));
			}

			/* Skip a line */
			fprintf(fff_visuals, "\n");

			/* Output footer */
			pref_footer(fff_visuals, mark);

			/* Close */
			(void)my_fclose(fff_visuals);

			/* Changes saved */
			changed_monster = FALSE;

			/* Message */
			if (!automatic) msg_print("Dumped monster attr/chars.");
		}

		/* Dump object attr/chars */
		else if (ch == '7')
		{
			/* Attempt to open, clean up, and print a header to a file. */
			if (!do_cmd_visuals_file("object", automatic, mark))
			{
				/* Notice failure or cancel */
				continue;
			}

			/* Dump objects */
			for (i = 0; i < z_info->k_max; i++)
			{
				char o_name[DESC_LEN];
				object_kind *k_ptr = &k_info[i];

				/* Skip non-entries */
				if (!k_ptr->name) continue;

				/* Tidy names */
				strip_name(o_name, i);

				/* Dump a comment */
				fprintf(fff_visuals, "# %s\n", o_name);

				/* Dump the object attr/char info */
				fprintf(fff_visuals, "K:%d:%c:%d\n\n", i,
					color_table[k_ptr->x_attr].index_char, (byte)(k_ptr->x_char));
			}

			/* Skip a line */
			fprintf(fff_visuals, "\n");

			/* Output footer */
			pref_footer(fff_visuals, mark);

			/* Close */
			(void)my_fclose(fff_visuals);

			/* Changes saved */
			changed_object = FALSE;

			/* Message */
			if (!automatic) msg_print("Dumped object attr/chars.");
		}

		/* Dump feature attr/chars */
		else if (ch == '8')
		{
			/* Attempt to open, clean up, and print a header to a file. */
			if (!do_cmd_visuals_file("feature", automatic, mark))
			{
				/* Notice failure or cancel */
				continue;
			}

			/* Dump features */
			for (i = 0; i < z_info->f_max; i++)
			{
				feature_type *f_ptr = &f_info[i];

				/* Skip non-entries */
				if (!f_ptr->name) continue;

				/* Skip features that mimic other features */
				if (f_ptr->mimic != i) continue;

				/* Dump a comment */
				fprintf(fff_visuals, "# %s\n", (f_name + f_ptr->name));

				/* Dump the feature attr/char info */
				fprintf(fff_visuals, "F:%d:%c:%d:%c:%d:%c:%d%s\n\n", i,
					color_table[f_ptr->x_attr].index_char, f_ptr->x_char,
					color_table[f_ptr->x_attr_lit].index_char, f_ptr->x_char_lit,
					color_table[f_ptr->x_attr_dim].index_char, f_ptr->x_char_dim,
					f_ptr->flags & (TF_TORCH_ONLY) ? ":torch_only" : "");
			}

			/* Skip a line */
			fprintf(fff_visuals, "\n");

			/* Output footer */
			pref_footer(fff_visuals, mark);

			/* Close */
			(void)my_fclose(fff_visuals);

			/* Changes saved */
			changed_feature = FALSE;

			/* Message */
			if (!automatic) msg_print("Dumped feature attr/chars.");
		}

		/* Dump flavor attr/chars */
		else if (ch == '9')
		{
			/* Attempt to open, clean up, and print a header to a file. */
			if (!do_cmd_visuals_file("flavor", automatic, mark))
			{
				/* Notice failure or cancel */
				continue;
			}

			/* Dump flavors */
			for (i = 0; i < z_info->flavor_max; i++)
			{
				/* Get this flavor */
				flavor_type *flavor_ptr = &flavor_info[i];

				/* Ignore unused */
				if (!flavor_ptr->text) continue;

				/* Dump a comment */
				fprintf(fff_visuals, "# %s\n", (flavor_text + flavor_ptr->text));

				/* Dump the flavor attr/char info */
				fprintf(fff_visuals, "L:%d:%d:%d\n\n", i,
				        (byte)(flavor_ptr->x_attr), (byte)(flavor_ptr->x_char));
			}

			/* Skip a line */
			fprintf(fff_visuals, "\n");

			/* Output footer */
			pref_footer(fff_visuals, mark);

			/* Close */
			(void)my_fclose(fff_visuals);

			/* Changes saved */
			changed_flavor = FALSE;

			/* Message */
			if (!automatic) msg_print("Dumped flavor attr/chars.");
		}

		/* Dump projection attr/chars */
		else if (ch == '0')
		{
			bool print_name = TRUE;

			/* Attempt to open, clean up, and print a header to a file. */
			if (!do_cmd_visuals_file("projection", automatic, mark))
			{
				/* Notice failure or cancel */
				continue;
			}

			/* Dump projections */
			for (i = 0; i < 256; i++)
			{
				proj_graphics_type *pg_ptr = &proj_graphics[i];

				/* Only dump changed projections */
				if ((i > 0) &&
				    (pg_ptr->attr_vert  == proj_graphics[0].attr_vert) &&
				    (pg_ptr->char_vert  == proj_graphics[0].char_vert) &&
				    (pg_ptr->attr_horiz == proj_graphics[0].attr_horiz) &&
				    (pg_ptr->char_horiz == proj_graphics[0].char_horiz) &&
				    (pg_ptr->attr_rdiag == proj_graphics[0].attr_rdiag) &&
				    (pg_ptr->char_rdiag == proj_graphics[0].char_rdiag) &&
				    (pg_ptr->attr_ldiag == proj_graphics[0].attr_ldiag) &&
				    (pg_ptr->char_ldiag == proj_graphics[0].char_ldiag) &&
				    (pg_ptr->attr_ball  == proj_graphics[0].attr_ball) &&
				    (pg_ptr->char_ball  == proj_graphics[0].char_ball) &&
				    (pg_ptr->flags      == proj_graphics[0].flags))
				{
					continue;
				}

				/* Print name, but only if one is available */
				if ((print_name) && ((i == 0) || (strlen(projection_names[i]))))
				{
					/* Label the default set of projection graphics */
					if (i == 0)
						fprintf(fff_visuals, "# Default set of projection graphics, used in all cases not specified below\n");

					/* End of names array -- stop print names */
					else if (projection_names[i] == NULL) print_name = FALSE;

					/* Dump a comment */
					else if (strlen(projection_names[i]))
						fprintf(fff_visuals, "# %s\n", projection_names[i]);

				}

				/* Dump the projection attr/char info */
				fprintf(fff_visuals, "S:%d:%d/%d:%d/%d:%d/%d:%d/%d:%d/%d\n\n", i,
				       pg_ptr->attr_vert,  (byte)pg_ptr->char_vert,
				       pg_ptr->attr_horiz, (byte)pg_ptr->char_horiz,
				       pg_ptr->attr_rdiag, (byte)pg_ptr->char_rdiag,
				       pg_ptr->attr_ldiag, (byte)pg_ptr->char_ldiag,
				       pg_ptr->attr_ball,  (byte)pg_ptr->char_ball);
			}

			/* Skip a line */
			fprintf(fff_visuals, "\n");

			/* Output footer */
			pref_footer(fff_visuals, mark);

			/* Close */
			(void)my_fclose(fff_visuals);

			/* Changes saved */
			changed_spell = FALSE;

			/* Message */
			if (!automatic) msg_print("Dumped projection attr/chars.");
		}

		/* Reset visuals */
		else if (ch == '!')
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


	/* Restore previous screen */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

	/* Do a complete screen refresh  XXX XXX */
	do_cmd_redraw();
}



/*
 * Asks the user for specific color values.
 * Returns TRUE if the color was modified.
 */
static bool askfor_color_values(int idx)
{
	char str[10];

	int k, r, g, b;

	/* Get the default value */
	(void)strnfmt(str, sizeof(str), "%d", color_table[idx].rv);

	/* Query, check for ESCAPE */
	if (!get_string("Red (0-255) ", str, sizeof(str))) return (FALSE);

	/* Convert to number */
	r = atoi(str);

	/* Check bounds */
	if (r < 0) r = 0;
	if (r > 255) r = 255;

	/* Get the default value */
	(void)strnfmt(str, sizeof(str), "%d", color_table[idx].gv);

	/* Query, check for ESCAPE */
	if (!get_string("Green (0-255) ", str, sizeof(str))) return (FALSE);

	/* Convert to number */
	g = atoi(str);

	/* Check bounds */
	if (g < 0) g = 0;
	if (g > 255) g = 255;

	/* Get the default value */
	(void)strnfmt(str, sizeof(str), "%d", color_table[idx].bv);

	/* Query, check for ESCAPE */
	if (!get_string("Blue (0-255) ", str, sizeof(str))) return (FALSE);

	/* Convert to number */
	b = atoi(str);

	/* Check bounds */
	if (b < 0) b = 0;
	if (b > 255) b = 255;

	/* Get the default value */
	(void)strnfmt(str, sizeof(str), "%d", color_table[idx].kv);

	/* Query, check for ESCAPE */
	if (!get_string("Extra (0-255) ", str, sizeof(str))) return (FALSE);

	/* Convert to number */
	k = atoi(str);

	/* Check bounds */
	if (k < 0) k = 0;
	if (k > 255) k = 255;

	/* Do nothing if the color is not modified */
	if ((k == color_table[idx].kv) &&
	    (r == color_table[idx].rv) &&
	    (g == color_table[idx].gv) &&
	    (b == color_table[idx].bv)) return (FALSE);

	/* Modify the color table */
	color_table[idx].kv = k;
	color_table[idx].rv = r;
	color_table[idx].gv = g;
	color_table[idx].bv = b;

	/* Success */
	return (TRUE);
}


/* String used to show a color sample */
#define COLOR_SAMPLE "###"

/* These two are used to place elements in the grid */
#define COLOR_X(idx) (((idx) / 16) * 5 + 1)
#define COLOR_Y(idx) ((idx) % 16 + 6)

/* We only can edit a portion of the color table */
#define MAX_COLORS_EDIT 128

/* Hack - Note the cast to "int" to prevent overflow */
#define IS_BLACK(idx) \
((int)color_table[idx].rv + (int)color_table[idx].gv + \
 (int)color_table[idx].bv == 0)

/* We show black as dots to see the shape of the grid */
#define BLACK_SAMPLE "..."

/*
 * The screen used to modify the color table. Only 128 colors can be modified.
 * The remaining entries of the color table are reserved for graphic mode.
 *
 * -DG-
 */
static void modify_colors(void)
{
	int x, y, idx, old_idx;
	char ch;
	char msg[DESC_LEN];

	/* Flags */
	bool do_move, do_update;
	bool do_redraw = TRUE;

	int max_legal_edit = MIN(MAX_COLORS_EDIT, max_system_colors);


	/* Hack - We want to show the fake cursor */
	do_move = TRUE;
	do_update = TRUE;

	/* Start with the first color */
	idx = 0;

	/* Used to erase the old position of the fake cursor */
	old_idx = -1;

	/* Interact until satisfied */
	while (TRUE)
	{
		/* Redraw request */
		if (do_redraw)
		{
			int tmp_idx = idx;

			/* Clear the screen */
			(void)Term_clear();

			/* Draw the color table */
			for (idx = 0; idx < max_legal_edit; idx++)
			{
				/* Get coordinates, the x value is adjusted to show a fake cursor */
				x = COLOR_X(idx) + 1;
				y = COLOR_Y(idx);

				/* Show a sample of the color */
				if (IS_BLACK(idx)) c_put_str(TERM_WHITE, BLACK_SAMPLE, y, x);
				else
				{
					byte attr = color_char_to_attr(color_table[idx].index_char);
					c_put_str(attr, COLOR_SAMPLE, y, x);
				}
			}

			/* Restore current color index */
			idx = tmp_idx;

			/* Show screen commands and help */
			y = 2;
			x = 42;
			put_str("Commands:", y, x);
			put_str("ESC: Return    ?: Help", y + 2, x);
			put_str("Arrows: Move to color", y + 3, x);
			put_str("n: Rename color", y + 4, x);
			put_str("i: Assign color an index character", y + 5, x);
			put_str("k,K: Incr,Decr extra value", y + 6, x);
			put_str("r,R: Incr,Decr red value", y + 7, x);
			put_str("g,G: Incr,Decr green value", y + 8, x);
			put_str("b,B: Incr,Decr blue value", y + 9, x);
			put_str("c: Copy from color", y + 10, x);
			put_str("v: Set specific values", y + 11, x);
			put_str("First column: base colors", y + 13, x);
			put_str("Other columns: extra colors.", y + 14, x);

			put_str("Please note: Colors may not display", y + 16, x);
			put_str("correctly until you assign them a ", y + 17, x);
			put_str("unique index character.", y + 18, x);

			/* Note that system colors are limited */
			if (max_system_colors < 128)
			{
				put_str(format("Colors after #%d are \"extended colors\";",
					max_system_colors - 1), y + 20, x);
				put_str("Your system will display them", y + 21, x);
				put_str("using the basic colors.", y + 22, x);
			}

			/* Done with the redraw; now perform an update */
			do_redraw = FALSE;
			do_move = do_update = TRUE;
		}

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
			(void)my_strcpy(msg, format("Color = %d:  %c : %s",
				idx, color_table[idx].index_char,
				color_table[idx].name), sizeof(msg));

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
			if (IS_BLACK(idx))
			{
				c_put_str(TERM_WHITE, BLACK_SAMPLE, y, x);
			}
			else
			{
				byte attr = color_char_to_attr(color_table[idx].index_char);
				c_put_str(attr, COLOR_SAMPLE, y, x);
			}

			/* Notify the changes in the color table to the terminal */
			(void)Term_xtra(TERM_XTRA_REACT, 0);

			/* The user is playing with white, redraw all */
			if (idx == TERM_WHITE) (void)Term_redraw();

			/* Or reduce flickering by redrawing the changes only */
			else (void)Term_redraw_section(x, y, x + 2, y);
		}

		/* Common code, show the values in the color table */
		if (do_move || do_update)
		{
			/* Format the view of the color values */
			(void)my_strcpy(msg, format("K = %d / R,G,B = %d, %d, %d",
					color_table[idx].kv,
					color_table[idx].rv,
					color_table[idx].gv,
					color_table[idx].bv), sizeof(msg));

			/* Show color values and some whitespace */
			c_put_str(TERM_WHITE, format("%-40s", msg), 4, 0);
		}

		/* Reset flags */
		do_move = FALSE;
		do_update = FALSE;
		old_idx = -1;

		/* Get a command */
		if (!get_com("Command: Modify colors ", &ch)) break;

		switch (ch)
		{
			/* Down */
			case '2':
			{
				/* Check bounds */
				if (idx + 1 >= max_legal_edit) break;

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
				if (idx + 16 >= max_legal_edit) break;

				/* Erase the old cursor */
				old_idx = idx;

				/* Get the new position */
				idx += 16;

				/* Request movement */
				do_move = TRUE;
				break;
			}

			/* Assign color an index character */
			case 'i':
			case 'I':
			{
				char ch2;
				int i;

				/* Continue until satisfied */
				while (TRUE)
				{
					/* Prompt */
					put_str("Use which letter as an index character (must not already be used)?", 0, 5);

					/* Get a character */
					ch2 = inkey(FALSE);

					/* Note escape */
					if (ch2 == ESCAPE) break;

					/* Clear second line */
					put_str("", 1, 10);

					/* Must be unique */
					for (i = 0; i < MAX_COLORS; i++)
					{
						if (color_table[i].index_char == ch2) break;
					}
					if (i != MAX_COLORS)
					{
						put_str("The character you use must be unique.", 1, 5);
						continue;
					}

					/* Save this character */
					color_table[idx].index_char = ch2;

					/* Request update and refresh color */
					do_update = TRUE;
					do_move = TRUE;

					break;
				}
				break;
			}

			/* Assign color a name */
			case 'n':
			case 'N':
			{
				char str[DESC_LEN];
				u16b max_length = sizeof(color_table[idx].name);

				/* Continue until satisfied */
				while (TRUE)
				{
					/* Clear string */
					strcpy(str, "");

					/* Get name, allow cancel */
					if (!get_string("Name of color: ", str, sizeof(str))) break;

					/* Clear second line */
					put_str("", 1, 5);

					/* We must be able to hold the name */
					if (strlen(str) >= max_length)
					{
						put_str(format("The name you use must be %d characters or less.", max_length), 1, 5);
						continue;
					}

					/* Save the name */
					(void)my_strcpy(color_table[idx].name, str, max_length);

					/* Refresh the color information */
					do_move = TRUE;

					break;
				}
				break;
			}

			/* Copy from color */
			case 'c':
			{
				char str[10];
				int src;

				/* Get the default value, the base color */
				strcpy(str, "1");

				/* Query, check for ESCAPE */
				if (!get_string(format("Copy from color (0-%d) ",
					max_legal_edit - 1), str, sizeof(str))) break;

				/* Convert to number */
				src = atoi(str);

				/* Check bounds */
				if (src < 0) src = 0;
				if (src >= max_legal_edit) src = max_legal_edit - 1;

				/* Do nothing if the colors are the same */
				if (src == idx) break;

				/* Modify the color table */
				color_table[idx].kv = color_table[src].kv;
				color_table[idx].rv = color_table[src].rv;
				color_table[idx].gv = color_table[src].gv;
				color_table[idx].bv = color_table[src].bv;

				/* Request update */
				do_update = TRUE;
				break;
			}

			/* Increase the extra value */
			case 'k':
			{
				/* Get a pointer to the proper value */
				byte *k_ptr = &color_table[idx].kv;

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
				byte *k_ptr = &color_table[idx].kv;

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
				byte *r_ptr = &color_table[idx].rv;

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
				byte *r_ptr = &color_table[idx].rv;

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
				byte *g_ptr = &color_table[idx].gv;

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
				byte *g_ptr = &color_table[idx].gv;

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
				byte *b_ptr = &color_table[idx].bv;

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
				byte *b_ptr = &color_table[idx].bv;

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

			/* Help me. */
			case '?':
			{
				p_ptr->get_help_index = HELP_CMD_COLORS;
				do_cmd_help();

				/* Request redraw */
				do_redraw = TRUE;

				break;
			}

			default:
			{
				break;
			}
		}
	}
}


/*
 * Interact with "colors".  -DG-
 */
void do_cmd_colors(void)
{
	int ch;

	int i;

	FILE *fff;
	char buf[1024];


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Save the screen and center it */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_CX, 80, 0);


	/* Interact until done */
	while (TRUE)
	{
		/* Clear screen */
		(void)Term_clear();

		/* Ask for a choice */
		prt("Interact with Colors", 2, 0);

		/* Give some choices */
		prt("(1) Load colors from file", 4, 5);
		prt("(2) Save colors to file", 5, 5);
		prt("(3) Modify colors", 6, 5);
		prt("(?) Get help", 7, 5);

		/* Prompt */
		prt("Command: ", 9, 0);

		/* Prompt */
		ch = inkey(FALSE);

		/* Done */
		if (ch == ESCAPE) break;

		/* Load a user pref file */
		if (ch == '1')
		{
			/* Ask for and load a user pref file */
			do_cmd_pref_file_hack(8, "color");

			/* Could skip the following if loading cancelled XXX XXX XXX */

			/* Mega-Hack -- React to color changes */
			(void)Term_xtra(TERM_XTRA_REACT, 0);

			/* Mega-Hack -- Redraw physical windows */
			(void)Term_redraw();
		}

		/* Dump colors */
		else if (ch == '2')
		{
			static cptr mark = "Color redefinitions";
			char ftmp[DESC_LEN];

			/* Required filename  XXX XXX */
			(void)strnfmt(ftmp, sizeof(ftmp), "color.prf");

			/* Build the filename */
			(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, ftmp);

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

			/* Print some comments so user knows what this new file is for */
			fprintf(fff, "# In order for colors to be stored in the user directory, and for them to \n");
			fprintf(fff, "# be nevertheless read before anything that needs to use them (edit files,\n");
			fprintf(fff, "# most other pref files), we must save them to a special file: colors.prf.\n\n");

			/* Dump colors */
			for (i = 0; i < MAX_COLORS_EDIT; i++)
			{
				char index_char = color_table[i].index_char;
				char name[DESC_LEN];

				int kv = color_table[i].kv;
				int rv = color_table[i].rv;
				int gv = color_table[i].gv;
				int bv = color_table[i].bv;

				/* Skip non-entries */
				if ((!index_char) || (!kv && !rv && !gv && !bv)) continue;

				/* Get name, add a colon */
				(void)my_strcpy(name, format("%s:", color_table[i].name), sizeof(name));


				/* Dump the color info */
				fprintf(fff, "V:%3d:%c:%-19s%3d:%3d:%3d:%3d:%d\n",
					i, index_char, name, kv, rv, gv, bv, translate_into_16_colors(i));
			}

			/* Skip a line */
			fprintf(fff, "\n");

			/* Output footer */
			pref_footer(fff, mark);

			/* Close */
			(void)my_fclose(fff);

			/* Message */
			msg_print("Dumped color redefinitions in \"lib/user/color.prf\".");
		}

		/* Edit colors */
		else if (ch == '3')
		{
			modify_colors();
		}

		/* Help me. */
		else if (ch == '?')
		{
			p_ptr->get_help_index = HELP_CMD_COLORS;
			do_cmd_help();
		}

		/* Unknown option */
		else
		{
			bell("Illegal command for colors!");
		}

		/* Flush messages */
		message_flush();
	}


	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

	/* Do a complete screen refresh  XXX XXX */
	do_cmd_redraw();
}



/*
 * Note something in the message recall.  -CK-
 *                       and the history -JM-
 */
void do_cmd_note(void)
{
	char tmp[1024];
	char buf[1024];

	/* Default */
	strcpy(tmp, "");

	/* Input */
	if (!get_string("Note:", tmp, sizeof(tmp))) return;

	/* Ignore empty notes */
	if (!tmp[0] || (tmp[0] == ' ')) return;

	/* Make message */
	strnfmt(buf, sizeof(tmp), "Note: %s", tmp);

	/* Add the note to the message recall */
	message(0, 0, buf);

	/* Add the note to history */
	history_add(buf, HISTORY_USER_INPUT, 0);
}


/*
 * Mention the current version
 */
void do_cmd_version(void)
{
	/* Version message */
	msg_format("You are playing %s %s.  Type '?' for more info.",
		VERSION_NAME, VERSION_STRING);
}


/*
 * Array of feeling strings
 *
 * The "negative" feelings idea is from Zangband.
 */
static cptr do_cmd_feeling_text[11] =
{
	"You are still uncertain about this place...",
	"You feel there is something special here...",
	"Premonitions of death appall you!  This place is murderous!",
	"This place feels terribly dangerous!",
	"You have a nasty feeling about this place.",
	"You have a bad feeling about this place.",
	"You feel nervous.",
	"You have an uneasy feeling.",
	"You have a faint uneasy feeling.",
	"This place seems reasonably safe.",
	"This seems a quiet, peaceful place."
};



/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(bool first_time)
{
	cptr quest_feel;

	/* Verify the feeling */
	if (feeling > 10) feeling = 10;

	/* No useful feeling in town */
	if (!p_ptr->depth)
	{
		msg_print("The town seems safe enough.");
		return;
	}

	/* Display the precognition messages  XXX XXX */
	if (!first_time) precog_msg(PRECOG_DISPLAY);

	/* No useful general feelings until enough time has passed */
	if (no_feeling_yet)
	{
		msg_print("You are still uncertain about this place...");
		return;
	}

	/* Display the feeling */
	if (TRUE)
	{
		int msg_attr = MSG_WHITE;

		/* Special colors for special feelings */
		if      (feeling == 0) msg_attr = MSG_SLATE;
		else if (feeling == 1) msg_attr = MSG_L_PURPLE;
		else if (feeling == 2) msg_attr = MSG_RED;
		else if (feeling <= 4) msg_attr = MSG_ORANGE;
		else if (feeling <= 6) msg_attr = MSG_YELLOW;

		/* Message */
		message(msg_attr, (first_time ? 250 : 0),
			do_cmd_feeling_text[feeling]);
	}

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
			if ((int)strlen(q_out) < Term->cols - 5) msg_print(q_out);
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
 * Write HTML escape characters.
 */
static void write_html_escape_char(FILE *htm, char c)
{
	switch (c)
	{
		case '<':
			fprintf(htm, "&lt;");
			break;
		case '>':
			fprintf(htm, "&gt;");
			break;
		case '&':
			fprintf(htm, "&amp;");
			break;
		default:
			fprintf(htm, "%c", c);
			break;
	}
}

/*
 * Take an html or vBulletin screenshot of both the main and the map term.
 * Used when "use_special_map" is ON.
 *
 * HACK -- Hard-coded handling of the status line below the map.  XXX XXX
 */
static void html_screenshot_aux_map(FILE *htm, int mode)
{
	int y, x, y1, x1;
	int wid, hgt, wid_main, hgt_main, wid_map, hgt_map;
	int status_line_y;

	byte a;
	byte oa = TERM_WHITE;
	char c = ' ';


	/* Retrieve current term size */
	(void)Term_get_size(&wid_main, &hgt_main);

	/* Activate the map term */
	(void)Term_activate(term_map);

	/* Retrieve current map term size */
	(void)Term_get_size(&wid_map, &hgt_map);

	/* Activate the main term again */
	(void)Term_activate(term_main);


	/* The total size must accomodate the map plus all main term displays */
	if (wid_main >= wid_map + COL_MAP)     wid = wid_main;
	else                                   wid = wid_map + COL_MAP;
	if (hgt_main >= hgt_map + ROW_MAP + 1) hgt = hgt_main;
	else                                   hgt = hgt_map + ROW_MAP + 1;

	/* Hack -- the status line always goes at the bottom */
	status_line_y = hgt - 1;


	/* Dump both main and map terms */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			/* Which term should we get data from? */
			if ((y >= ROW_MAP) && (y != status_line_y) && (x >= COL_MAP))
			{
				/* The map term is offset within the main term */
				(void)Term_activate(term_map);
				y1 = y - ROW_MAP;
				x1 = x - COL_MAP;
			}
			else
			{
				/* The main term is not offset */
				(void)Term_activate(term_main);
				y1 = y;
				x1 = x;

				/* ... except for the status line, which must come last  XXX XXX */
				if (y == status_line_y) y1 = ROW_STATUS;
			}

			/* Handle illegal space by making it blank */
			if ((y1 >= Term->rows) || (x1 >= Term->cols))
			{
				/* Use current color, if legal */
				a = ((oa != 255) ? oa : TERM_WHITE);

				/* Print spaces */
				c = ' ';
			}

			/* If legal, get the ASCII tile */
			else
			{
				a = Term->scr->a[y1][x1];
				c = Term->scr->c[y1][x1];
			}

			/* Color change -- only for non-space */
			if ((a != oa) && (c != ' '))
			{
				/* From the default white to another color */
				if (oa == TERM_WHITE)
				{
					if (mode == 1)
					{
						fprintf(htm, "[color=\"#%02X%02X%02X\"]",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
					else
					{
						fprintf(htm, "<font color=\"#%02X%02X%02X\">",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
				}

				/* From another color to the default white */
				else if (a == TERM_WHITE)
				{
					/* Close the <font> or [color] tag */
					if (mode == 1) fprintf(htm, "[/color]");
					else           fprintf(htm, "</font>");
				}

				/* Change colors */
				else
				{
					if (mode == 1)
					{
						fprintf(htm, "[/color][color=\"#%02X%02X%02X\"]",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
					else
					{
						fprintf(htm, "</font><font color=\"#%02X%02X%02X\">",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
				}

				/* Remember the last color */
				oa = a;
			}

			/* Write the character and escape special HTML characters */
			if (mode == 1) fprintf(htm, "%c", c);
			else           write_html_escape_char(htm, c);
		}

		/* End the row */
		fprintf(htm, "\n");
	}

	/* Close the last <font> or [color] tag */
	if (mode == 1) fprintf(htm, "[/color]");
	else           fprintf(htm, "</font>");

	/* Close out the "white" tag if necessary */
	if (oa != TERM_WHITE)
	{
		if (mode == 1) fprintf(htm, "[/color]");
		else           fprintf(htm, "</font>");
	}
}



/*
 * Take an html or vBulletin screenshot of the current term (only).
 * Used when "use_special_map" is OFF.
 */
void html_screenshot_aux_main(FILE *htm, int mode)
{
	int y, x;
	int wid, hgt;

	byte a;
	byte oa = TERM_WHITE;
	char c = ' ';

	/* Retrieve current screen size */
	(void)Term_get_size(&wid, &hgt);


	/* Dump the screen */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			/* Get the ASCII tile */
			a = Term->scr->a[y][x];
			c = Term->scr->c[y][x];

			/* Color change -- only for non-space */
			if ((a != oa) && (c != ' '))
			{
				/* From the default white to another color */
				if (oa == TERM_WHITE)
				{
					if (mode == 1)
					{
						fprintf(htm, "[color=\"#%02X%02X%02X\"]",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
					else
					{
						fprintf(htm, "<font color=\"#%02X%02X%02X\">",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
				}

				/* From another color to the default white */
				else if (a == TERM_WHITE)
				{
					/* Close the <font> or [color] tag */
					if (mode == 1) fprintf(htm, "[/color]");
					else           fprintf(htm, "</font>");
				}

				/* Change colors */
				else
				{
					if (mode == 1)
					{
						fprintf(htm, "[/color][color=\"#%02X%02X%02X\"]",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
					else
					{
						fprintf(htm, "</font><font color=\"#%02X%02X%02X\">",
							color_table[a].rv,
							color_table[a].gv,
							color_table[a].bv);
					}
				}

				/* Remember the last color */
				oa = a;
			}

			/* Write the character and escape special HTML characters */
			if (mode == 1) fprintf(htm, "%c", c);
			else           write_html_escape_char(htm, c);
		}

		/* End the row */
		fprintf(htm, "\n");
	}

	/* Close the last <font> or [color] tag */
	if (mode == 1) fprintf(htm, "[/color]");
	else           fprintf(htm, "</font>");

	/* Close out the "white" tag if necessary */
	if (oa != TERM_WHITE)
	{
		if (mode == 1) fprintf(htm, "[/color]");
		else           fprintf(htm, "</font>");
	}
}


/*
 * Take an html or vBulletin screenshot.
 *
 * Idea from ToME, code from Angband.
 */
static void html_screenshot(cptr name, int mode)
{
	FILE *htm;

	char buf[1024];

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	htm = my_fopen(buf, "w");

	/* Oops */
	if (!htm)
	{
		plog_fmt("Cannot write the '%s' file!", buf);
		return;
	}

	/* Write headers */
	if (mode == 1)
	{
		/* Write vBulletin headers */
		fprintf(htm, "[code][tt][bc=black][color=white]\n");
	}
	else
	{
		/* Write HTML headers */
		fprintf(htm, "<html>\n");
		fprintf(htm, "<head>\n");
		fprintf(htm, "<meta name=\"generator\" Content=\"%s %d.%d.%d\">\n",
			VERSION_NAME, VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
		fprintf(htm, "<title>%s</title>\n", name);
		fprintf(htm, "</head>\n\n");
		fprintf(htm, "<body bgcolor=\"#000000\"><font color=\"#FFFFFF\">\n");
		fprintf(htm, "<pre><tt>\n");
	}

	/* Save screen information to file */
	if (use_special_map)
	{
		html_screenshot_aux_map(htm, mode);
	}
	else
	{
		html_screenshot_aux_main(htm, mode);
	}

	/* Close out the codes */
	if (mode == 1)
	{
		/* Close out the vBulletin codes */
		fprintf(htm, "[/bc][/tt][/code]\n");
	}
	else
	{
		/* Close out the HTML codes */
		fprintf(htm, "</tt></pre>\n");
		fprintf(htm, "</body>\n");
		fprintf(htm, "</html>\n");
	}

	/* Close the file */
	(void)my_fclose(htm);
}


/*
 * Take a textual screenshot of both the main and the map term.  Used when
 * "use_special_map" is ON.
 *
 * HACK -- Hard-coded handling of the status line below the map.  XXX XXX
 */
static void text_screenshot_aux_map(FILE *fp)
{
	int y, x, y1, x1;
	int wid, hgt, wid_main, hgt_main, wid_map, hgt_map;
	int status_line_y;

	byte a = 0;
	char c = ' ';
	char buf[1024];


	/* Retrieve current term size */
	(void)Term_get_size(&wid_main, &hgt_main);

	/* Activate the map term */
	(void)Term_activate(term_map);

	/* Retrieve current map term size */
	(void)Term_get_size(&wid_map, &hgt_map);

	/* Activate the main term again */
	(void)Term_activate(term_main);


	/* The total size must accomodate the map plus all main term displays */
	if (wid_main >= wid_map + COL_MAP)     wid = wid_main;
	else                                   wid = wid_map + COL_MAP;
	if (hgt_main >= hgt_map + ROW_MAP + 1) hgt = hgt_main;
	else                                   hgt = hgt_map + ROW_MAP + 1;

	/* Hack -- the status line always goes at the bottom */
	status_line_y = hgt - 1;


	/* Dump the screen */
	for (y = 0; y < hgt; y++)
	{
		/* Dump each row */
		for (x = 0; x < wid; x++)
		{
			/* Which term should we get data from? */
			if ((y >= ROW_MAP) && (y != status_line_y) && (x >= COL_MAP))
			{
				/* The map term is offset within the main term */
				(void)Term_activate(term_map);
				y1 = y - ROW_MAP;
				x1 = x - COL_MAP;
			}
			else
			{
				/* The main term is not offset */
				(void)Term_activate(term_main);
				y1 = y;
				x1 = x;

				/* ... except for the status line, which must come last */
				if (y == status_line_y) y1 = ROW_STATUS;
			}

			/* Handle illegal space by making it blank */
			if ((y1 >= Term->rows) || (x1 >= Term->cols))
			{
				a = TERM_WHITE;
				c = ' ';
			}

			/* If legal, get the ASCII tile */
			else
			{
				/* Get the attr/char */
				(void)(Term_what(x1, y1, &a, &c));
			}

			/* Dump it */
			buf[x] = c;
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fp, "%s\n", buf);
	}
}



/*
 * Save character (but not color) information to file.
 */
static void text_screenshot(cptr name)
{
	int y, x;
	int wid, hgt;

	byte a = 0;
	char c = ' ';

	FILE *fp;

	char buf[1024];


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fp = my_fopen(buf, "w");

	/* Oops */
	if (!fp)
	{
		plog_fmt("Cannot write the '%s' file!", buf);
		return;
	}

	/* Different handling if a special map term exists */
	if (use_special_map)
	{
		text_screenshot_aux_map(fp);
	}

	/* Standard case */
	else
	{
		/* Retrieve current screen size */
		(void)Term_get_size(&wid, &hgt);

		/* Dump the screen */
		for (y = 0; y < hgt; y++)
		{
			/* Dump each row */
			for (x = 0; x < wid; x++)
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
	}

	/* End the file */
	fprintf(fp, "\n");

	/* Close it */
	(void)my_fclose(fp);
}


/*
 * Save a screen shot to file.
 */
void do_cmd_save_screen(void)
{
	char filename[80] = "";
	char c = ESCAPE;
	int i;
	int mode = 0;


	/* Choose */
	while (TRUE)
	{
		if (!get_com("Choose a screenshot:  ('t' for text, 'f' for forum, 'h' for html, '?' for help):", &c)) return;

		/* Pick a screenshot type */
		if (strchr("TtFfHh", c)) break;

		/* Get help */
		if (c == '?')
		{
			/* Save the screen */
			screen_save(TRUE);

			/* Show contextual help */
			p_ptr->get_help_index = HELP_SCREENSHOT;
			do_cmd_help();

			/* Load the screen */
			screen_load();
		}
	}

	/* Choose text or html */
	if ((c == 't') || (c == 'T'))
	{
		strcpy(filename, "dump.txt");
		mode = 0;
	}
	else if ((c == 'f') || (c == 'F'))
	{
		strcpy(filename, "dump-vB.txt");
		mode = 1;
	}
	else
	{
		strcpy(filename, "dump.html");
		mode = 2;
	}

	/* Ask for a file, allow cancel */
	if (!get_string("Save screenshot as: ", filename, sizeof(filename))) return;

	/* Save custom graphic preferences */
	do_cmd_visuals('6');
	do_cmd_visuals('7');
	do_cmd_visuals('8');
	do_cmd_visuals('9');
	do_cmd_visuals('0');


	/* Extract default attr/char code for features */
	for (i = 0; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->x_attr_lit = f_ptr->x_attr_dim = f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char_lit = f_ptr->x_char_dim = f_ptr->x_char = f_ptr->d_char;
	}

	/* Extract default attr/char code for objects */
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Default attr/char */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract default attr/char code for monsters */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Default attr/char */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;
	}

	/* Extract default attr/char code for flavors */
	for (i = 0; i < z_info->flavor_max; i++)
	{
		flavor_type *flavor_ptr = &flavor_info[i];

		/* Default attr/char */
		flavor_ptr->x_attr = flavor_ptr->d_attr;
		flavor_ptr->x_char = flavor_ptr->d_char;
	}

	/* Process the basic text-mode preferences (no extended characters) */
	(void)process_pref_file("font-xxx.prf");

	/* Do a complete screen refresh */
	do_cmd_redraw();

	/* Save screen */
	screen_save(FALSE);


	/* Take a screenshot */
	if      (mode == 0) text_screenshot(filename);
	else                html_screenshot(filename, mode);


	/* Reset the visuals, reload preferences */
	reset_visuals();

	/* Re-load custom graphic preferences */
	(void)process_pref_file("scrn_tmp.txt");

	/* Load screen */
	screen_load();

	/* Do a complete screen refresh */
	do_cmd_redraw();

	/* Message */
	msg_format("Screenshot saved in the \"lib/user\" directory as \"%s\".", filename);
}



/*
 * Display the current score
 */
static void do_cmd_knowledge_score(void)
{
	char buf[1024];

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Grab permissions */
	safe_setuid_grab();

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);

	/* Drop permissions */
	safe_setuid_drop();

	/* Display the score */
	(void)predict_score();

	/* Shut the high score file */
	(void)fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;
}


#define BROWSER_ROWS	 (Term->rows - 9)



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
	{ (char *) -2L, "ghosts of adventurers past" },
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
	{ "$!?=\"\'.`~^+<>()[]{}\\|/:", "mimics" },
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
	{ TV_SLING,       "Slings" },
	{ TV_BOW,         "Bows" },
	{ TV_CROSSBOW,    "Crossbows" },
	{ TV_SHOT,        "Missiles" },
	{ TV_BOOTS,       "Boots" },
	{ TV_GLOVES,      "Gloves" },
	{ TV_HELM,        "Helms" },
	{ TV_CROWN,       "Crowns" },
	{ TV_SHIELD,      "Shields" },
	{ TV_CLOAK,       "Cloaks" },
	{ TV_SOFT_ARMOR,  "Soft Body Armor" },
	{ TV_HARD_ARMOR,  "Hard Body Armor" },
	{ TV_DRAG_ARMOR,  "Dragon Scale Mail" },
	{ TV_MAGIC_BOOK,  "Books of Sorcery" },
	{ TV_PRAYER_BOOK, "Books of Piety" },
	{ TV_NATURE_BOOK, "Stones of Nature" },
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
 * of artifacts in the group.
 */
static int collect_artifacts(int grp_cur, int object_idx[])
{
	int i, y, x;
	int object_cnt = 0;

	bool *okay;

	/* Get a list of x_char in this group */
	byte group_tval = object_group[grp_cur].tval;


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
		if (!a_ptr->cur_num) continue;

		/* Assume okay */
		okay[i] = TRUE;
	}

	/* Check the dungeon */
	for (y = 0; y < dungeon_hgt; y++)
	{
		for (x = 0; x < dungeon_wid; x++)
		{
			object_type *o_ptr;

			/* Scan all objects in the grid */
			for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
			{
				/* Ignore non-artifacts */
				if (!artifact_p(o_ptr)) continue;

				/* Ignore known items */
				if (object_known_p(o_ptr)) continue;

				/* Note the artifact */
				okay[o_ptr->artifact_index] = FALSE;
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
		if (object_known_p(o_ptr)) continue;

		/* Note the artifact */
		okay[o_ptr->artifact_index] = FALSE;
	}


	/* Check every artifact */
	for (i = 1; i < z_info->a_max; i++)
	{
		/* Get the artifact */
		artifact_type *a_ptr = &a_info[i];

		/* Skip empty artifacts */
		if (!a_ptr->name) continue;

		/* List only the dead or known ones */
		if (!okay[i]) continue;

		/* Skip artifacts not of the correct kind */
		if (group_tval == TV_SHOT)
		{
			if (!is_missile(a_ptr)) continue;
		}
		else if (a_ptr->tval != group_tval) continue;

		/* Add the artifact */
		object_idx[object_cnt++] = i;
	}

	/* Terminate the list */
	object_idx[object_cnt] = 0;

	/* Free the "okay" array */
	FREE(okay);

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
		(void)Term_erase(col, row + i, wid);

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

		/* We must be aware of the object kind */
		if (!object_aware_p(o_ptr)) return;
	}

	/* We know basic information about this specific object */
	o_ptr->ident |= (IDENT_KNOWN);

	/* Observe the object or artifact */
	do_cmd_observe(o_ptr, FALSE);
}


/*
 * Move the cursor in a browser window.
 *
 * ch                 :  the command entered
 * Column             :  group or individual item lists
 * grp_cur, grp_cnt   :  position in group list, size of that list
 * list_cur, list_cnt :  position in individual item list, size of that list
 */
static void browser_cursor(char ch, int *column, int *grp_cur, int grp_cnt,
						   int *list_cur, int list_cnt)
{
	bool shift_key = FALSE;

	/* Allow all direction keys and shift-movement */
	get_ui_direction(&ch, 0x00, &shift_key);


	/* Move up one page */
	if ((ch == '7') || (ch == '9'))
	{
		/* Browsing group list */
		if (!*column) *grp_cur -= BROWSER_ROWS;

		/* Browsing individual item list */
		else *list_cur -= BROWSER_ROWS;
	}

	/* Move up one half page */
	else if ((ch == '-') || (ch == '_'))
	{
		/* Browsing group list */
		if (!*column) *grp_cur -= BROWSER_ROWS / 2;

		/* Browsing individual item list */
		else *list_cur -= BROWSER_ROWS / 2;
	}

	/* Move up one */
	else if (ch == '8')
	{
		/* Browsing group list */
		if (!*column) *grp_cur -= (shift_key ? 5 : 1);

		/* Browsing individual item list */
		else *list_cur -= (shift_key ? 5 : 1);
	}


	/* Move down one */
	else if ((ch == '2') || (ch == '\r') || (ch == '\n'))
	{
		/* Browsing group list */
		if (!*column) *grp_cur += (shift_key ? 5 : 1);

		/* Browsing individual item list */
		else *list_cur += (shift_key ? 5 : 1);
	}

	/* Move down one half page */
	else if ((ch == '+') || (ch == '='))
	{
		/* Browsing group list */
		if (!*column) *grp_cur += BROWSER_ROWS / 2;

		/* Browsing individual item list */
		else *list_cur += BROWSER_ROWS / 2;
	}

	/* Move down one page */
	else if ((ch == '1') || (ch == '3') || (ch == ' '))
	{
		/* Browsing group list */
		if (!*column) *grp_cur += BROWSER_ROWS;

		/* Browsing individual item list */
		else *list_cur += BROWSER_ROWS;
	}


	/* Shift columns */
	else if ((ch == '6') && (*column == 0))
	{
		*column = 1;
	}
	else if ((ch == '4') && (*column == 1))
	{
		*column = 0;
	}


	/* Check bounds */
	if (*list_cur >= list_cnt) *list_cur = list_cnt - 1;
	if (*list_cur < 0) *list_cur = 0;
	if (*grp_cur >= grp_cnt) *grp_cur = grp_cnt - 1;
	if (*grp_cur < 0) *grp_cur = 0;
}


/*
 * Display the artifacts in a group.  -EZ-
 */
static void display_artifact_list(int col, int row, int per_page,
	int object_idx[], int object_cur, int object_top)
{
	int i;
	char o_name[DESC_LEN];
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
		object_desc_store(o_name, sizeof(o_name), o_ptr, TRUE, 0);

		/* Display the name */
		c_prt(attr, o_name, row + i, col);

		/* More information for wizards */
		if (p_ptr->wizard)
			c_prt(attr, format("%d", a_idx), row + i, 70);
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		(void)Term_erase(col, row + i, 255);
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
			(void)Term_clear();

			/* Redraw */
			prt("Knowledge - artifacts", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (p_ptr->wizard) prt("Idx", 4, 70);

			for (i = 0; i < 78; i++)
			{
				(void)Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < BROWSER_ROWS; i++)
			{
				(void)Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		grp_top = MAX(0, grp_cur - BROWSER_ROWS + 1);

		/* Display a list of object groups (artifact) */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, TRUE,
			grp_cur, grp_top);

		/* Get a list of artifacts in the current group */
		object_cnt = collect_artifacts(grp_idx[grp_cur], object_idx);

		/* Never leave the list */
		if (object_cur >= object_cnt) object_cur = MAX(0, object_cnt - 1);

		/* Scroll object list (artifact) */
		object_top = MAX(0, object_cur - BROWSER_ROWS + 1);

		/* Display a list of artifacts in the current group */
		display_artifact_list(max + 3, 6, BROWSER_ROWS, object_idx, object_cur,
			object_top);


		/* Prompt */
		if (column) prt("<dir>, return to inspect, ESC", BROWSER_ROWS + 7, 0);
		else        prt("<dir>, ESC", BROWSER_ROWS + 7, 0);

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
			(void)Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			(void)Term_gotoxy(max + 3, 6 + (object_cur - object_top));
		}

		ch = inkey(FALSE);

		/* Handle commands */
		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case '\n':
			case '\r':
			{
				if (!column)
				{
					column = 1;
					break;
				}

				/* Fall through */
			}

			case 'R':
			case 'r':
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
			char name[DESC_LEN];
			(void)strnfmt(name, sizeof(name), "%s, the %s", ghost_name, r_name + r_ptr->name);
			c_prt(attr, name, row + i, col);
		}
		else
		{
			c_prt(attr, r_name + r_ptr->name, row + i, col);
		}

		if (p_ptr->wizard)
		{
			c_prt(attr, format ("%d", r_idx), row + i, 60);
		}

		/* Display symbol */
		(void)Term_putch(68, row + i, r_ptr->x_attr, r_ptr->x_char);

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
		(void)Term_erase(col, row + i, 255);
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
			(void)Term_clear();

			prt("Knowledge - Monsters", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (p_ptr->wizard) prt("Idx", 4, 60);
			prt("Sym   Kills", 4, 67);

			for (i = 0; i < 78; i++)
			{
				(void)Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < BROWSER_ROWS; i++)
			{
				(void)Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		grp_top = MAX(0, grp_cur - BROWSER_ROWS + 1);

		/* Display a list of monster groups */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, FALSE,
			grp_cur, grp_top);

		/* Get a list of monsters in the current group */
		mon_cnt = collect_monsters(grp_idx[grp_cur], mon_idx, 0x00);

		/* Never leave the list */
		if (mon_cur >= mon_cnt) mon_cur = MAX(0, mon_cnt - 1);

		/* Scroll monster list */
		mon_top = MAX(0, mon_cur - BROWSER_ROWS + 1);

		/* Display a list of monsters in the current group */
		display_monster_list(max + 3, 6, BROWSER_ROWS, mon_idx, mon_cur, mon_top);

		/* Track selected monster, to enable recall in sub-win */
		p_ptr->monster_race_idx = mon_idx[mon_cur];


		/* Prompt */
		if (column) prt("<dir>, return to recall, ESC", BROWSER_ROWS + 7, 0);
		else        prt("<dir>, ESC", BROWSER_ROWS + 7, 0);

		/* Mega Hack -- track this monster race */
		if (mon_cnt) monster_race_track(mon_idx[mon_cur]);

		/* Hack -- handle stuff */
		handle_stuff();

		if (!column)
		{
			(void)Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			(void)Term_gotoxy(max + 3, 6 + (mon_cur - mon_top));
		}

		ch = inkey(ALLOW_CLICK);

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case '\n':
			case '\r':
			{
				if (!column)
				{
					column = 1;
					break;
				}

				/* Fall through */
			}

			case 'R':
			case 'r':
			case 'I':
			case 'i':
			{
				/* Recall on screen */
				if (mon_idx[mon_cur])
				{
					/* Recall on screen */
					screen_roff(mon_idx[mon_cur]);

					/* Wait for a keypress */
					(void)inkey(ALLOW_CLICK);

					redraw = TRUE;
				}
				break;
			}

			default:
			{
				/* Move the cursor */
				browser_cursor(ch, &column, &grp_cur, grp_cnt, &mon_cur, mon_cnt);

				/* Display selected monster, if any */
				p_ptr->window |= (PW_MONSTER);

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
	char buf[DESC_LEN];

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
		/* No monsters killed */
		msg_print("You have defeated no foes.");
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
				my_toupper(monster_group[grp].text[0]),
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
	(void)my_fclose(fp);


	/* Use the standard or tall display and center an 80 column view */
	if (more_tall_display)
	{
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_TALL | DSP_CX,
			80, 0);
	}
	else
	{
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX,
			80, 0);
	}

	/* Display the file contents */
	(void)show_file(file_name, "Kill Count", 0, 2);

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);


	/* Reset "text_out()" vars */
	text_out_wrap   = 0;
	text_out_indent = 0;

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
	object_kind *k_ptr;
	byte attr, cursor;


	/* Display lines until done */
	for (i = 0; i < per_page; i++)
	{
		/* Get the object index */
		int k_idx = object_idx[object_top + i];

		/* Stop at end of list */
		if (k_idx <= 0) break;

		/* Access the object */
		k_ptr = &k_info[k_idx];

		/* Choose a color */
		attr =   ((k_ptr->special & (SPECIAL_AWARE)) ? TERM_WHITE : TERM_SLATE);
		cursor = ((k_ptr->special & (SPECIAL_AWARE)) ? TERM_L_BLUE : TERM_BLUE);

		/* Highlight selected object */
		attr = ((i + object_top == object_cur) ? cursor : attr);

		/* Display the name */
		c_prt(attr, k_name + k_ptr->name, row + i, col);

		/* Display object index if a wizard */
		if (p_ptr->wizard)
			c_prt(attr, format ("%d", k_idx), row + i, 70);

		/* If object is aware, show what it looks like */
		if (k_ptr->special & (SPECIAL_AWARE))
		{
			byte a = (k_ptr->flavor) ? (flavor_info[k_ptr->flavor].x_attr) :
				(k_ptr->x_attr);
			char c = (k_ptr->flavor) ? (flavor_info[k_ptr->flavor].x_char) :
				(k_ptr->x_char);

			/* Display symbol */
			(void)Term_putch(76, row + i, a, c);
		}
	}

	/* Clear remaining lines */
	for (; i < per_page; i++)
	{
		(void)Term_erase(col, row + i, 255);
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
			(void)Term_clear();

			prt("Knowledge - objects", 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, max + 3);
			if (p_ptr->wizard) prt("Idx", 4, 70);
			prt("Sym", 4, 75);

			for (i = 0; i < 78; i++)
			{
				(void)Term_putch(i, 5, TERM_WHITE, '=');
			}

			for (i = 0; i < BROWSER_ROWS; i++)
			{
				(void)Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
			}

			redraw = FALSE;
		}

		/* Scroll group list */
		grp_top = MAX(0, grp_cur - BROWSER_ROWS + 1);

		/* Display a list of object groups */
		display_group_list(0, 6, max, BROWSER_ROWS, grp_idx, TRUE,
			grp_cur, grp_top);

		/* Get a list of objects in the current group */
		object_cnt = collect_objects(grp_idx[grp_cur], object_idx);

		/* Never leave the list */
		if (object_cur >= object_cnt) object_cur = MAX(0, object_cnt - 1);

		/* Scroll object list */
		object_top = MAX(0, object_cur - BROWSER_ROWS + 1);

		/* Display a list of objects in the current group */
		display_object_list(max + 3, 6, BROWSER_ROWS, object_idx, object_cur,
			object_top);


		/* Prompt */
		if (column) prt("<dir>, return to inspect, ESC", BROWSER_ROWS + 7, 0);
		else        prt("<dir>, ESC", BROWSER_ROWS + 7, 0);

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
			(void)Term_gotoxy(0, 6 + (grp_cur - grp_top));
		}
		else
		{
			(void)Term_gotoxy(max + 3, 6 + (object_cur - object_top));
		}

		ch = inkey(ALLOW_CLICK);

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case '\n':
			case '\r':
			{
				if (!column)
				{
					column = 1;
					break;
				}

				/* Fall through */
			}

			case 'R':
			case 'r':
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
	char o_name[DESC_LEN];

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

			/* Get object description */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Print a message */
			fprintf(fp, "     %s\n", o_name);
		}
	}

	/* Close the file */
	(void)my_fclose(fp);

	/* Display the file contents */
	show_file(file_name, "Contents of Your Home", 0, 2);

	/* Remove the file */
	(void)fd_kill(file_name);
}


/*
 * Display the current quest and, underneath it, all previous quests.
 */
void do_cmd_knowledge_quests(FILE *fp)
{
	int wid;
	int i, level;
	bool quests = FALSE;
	bool temp_file = FALSE;
	int attr;

	quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

	char buf[DESC_LEN];
	char name[DESC_LEN];
	char intro[DESC_LEN];
	char targets[DESC_LEN];
	char where[DESC_LEN];

	char file_name[1024];


	/* We have not been given a file to write into */
	if (!fp)
	{
		/* Temporary file */
		fp = my_fopen_temp(file_name, sizeof(file_name));

		/* Notice failure */
		if (!fp)
		{
			msg_print("Could not open a temporary file to show your quests.");
			return;
		}

		temp_file = TRUE;
	}


	/* Set margins */
	text_out_wrap   = temp_file ? 77 : 70;
	text_out_indent = 3;

	/* Dump to the file */
	text_out_hook = text_out_to_file;
	text_out_file = fp;

	/* Get available width */
	wid = text_out_wrap - text_out_indent;

	/* Winner */
	if (p_ptr->total_winner)
	{
		center_string(buf, sizeof(buf), "You have slain Morgoth, Lord of Darkness!", wid);
		text_out_c(TERM_L_BLUE, format("%s\n\n", buf));
	}

	/* Killed Sauron */
	else if (!r_info[MON_SAURON].max_num)
	{
		center_string(buf, sizeof(buf), "You have slain Sauron, the Sorcerer; only Morgoth now remains!", wid);
		text_out_c(TERM_L_BLUE, format("%s\n\n", buf));
	}

	/* Print the current quest, if any */
	if ((p_ptr->cur_quest) && (q_ptr->active_level))
	{
		text_out_c(TERM_L_BLUE,
			format("%s\n\n", describe_quest(p_ptr->cur_quest, QMODE_FULL)));
	}


	/* Print fame */
	if (temp_file)
	{
		get_fame_desc(&attr, name);
		center_string(buf, sizeof(buf), format("Fame:  %s", name), wid);
		text_out_c(attr, format("%s\n\n", buf));
	}


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
			if (!quests && temp_file) text_out("Previous Quests:\n");

			/* We've displayed at least one quest */
			quests = TRUE;

			/* Introduction */
			if (p_ptr->quest_memory[i].succeeded)
			{
				strcpy(intro, "Defeated");
				if (p_ptr->cur_quest == p_ptr->quest_memory[i].level)
					attr = TERM_L_GREEN;
				else
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
				(void)my_strcpy(targets, format("%d %s",
					p_ptr->quest_memory[i].max_num, name), sizeof(targets));
			}

			/* One quest monster */
			else
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					(void)my_strcpy(targets, format("%s", name), sizeof(targets));
				}

				else if (my_is_vowel(name[0]))
					(void)my_strcpy(targets, format("an %s", name), sizeof(targets));
				else
					(void)my_strcpy(targets, format("a %s",  name), sizeof(targets));
			}

			/* The location of the quest */
			level = p_ptr->quest_memory[i].level;

			if (!depth_in_feet) strcpy(where, format("on level %d", level));
			else
			{
				if (!use_metric)
					(void)strcpy(where, format("at %d feet", level * 50));
				else
					(void)strcpy(where, format("at %d meters", level * 15));
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


	/* Using a temporary file */
	if (temp_file)
	{
		/* Close the file */
		(void)my_fclose(fp);

		/* Display the file contents */
		show_file(file_name, "Current and Completed Quests", 0, 2);

		/* Remove the file */
		(void)fd_kill(file_name);
	}
}

/*
 * Interact with "knowledge"
 *
 * The code for artifacts, objects, and monsters is taken from EyAngband.
 * That for the monster killed count is taken from Zangband.
 */
void do_cmd_knowledge(void)
{
	char ch;


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Save and center screen */
	display_change(DSP_REMEMBER | DSP_LOCK | DSP_CX, 80, 0);


	/* Interact until done */
	while (TRUE)
	{
		/* Clear screen and switch to standard view */
		display_change(DSP_CLEAR | DSP_NORM, 0, 0);

		/* Ask for a choice */
		prt("Display current knowledge", 2, 0);

		/* Give some choices */
		prt("(1) High scores",                        4, 5);
		prt("(2) Display known (or lost) artifacts",  5, 5);
		prt("(3) Known monsters",                     6, 5);
		prt("(4) Kill count",                         7, 5);
		prt("(5) Known objects",                      8, 5);
		prt("(6) Contents of your home",              9, 5);
		prt("(7) Current and completed quests",      10, 5);
		if (p_ptr->self_knowledge) prt("(8) Self Knowledge",      11, 5);

		/* Prompt */
		prt("Choice: ", (p_ptr->self_knowledge ? 13 : 12), 0);

		/* Prompt */
		ch = inkey(FALSE);

		/* Done */
		if (ch == ESCAPE) break;

		/* Game Score */
		if (ch == '1')
		{
			if (more_tall_display)
			{
				/* Clear screen and switch to large view */
				display_change(DSP_CLEAR | DSP_TALL, 0, 0);
			}
			do_cmd_knowledge_score();
		}

		/* Artifacts */
		else if (ch == '2')
		{
			if (more_tall_display)
			{
				display_change(DSP_CLEAR | DSP_TALL, 0, 0);
			}
			do_cmd_knowledge_artifacts();
		}

		/* Uniques */
		else if (ch == '3')
		{
			if (more_tall_display)
			{
				display_change(DSP_CLEAR | DSP_TALL, 0, 0);
			}
			do_cmd_knowledge_monsters();
		}

		/* Monster killed count */
		else if (ch == '4')
		{
			if (more_tall_display)
			{
				display_change(DSP_CLEAR | DSP_TALL, 0, 0);
			}
			do_cmd_knowledge_kill_count();
		}

		/* Objects */
		else if (ch == '5')
		{
			if (more_tall_display)
			{
				display_change(DSP_CLEAR | DSP_TALL, 0, 0);
			}
			do_cmd_knowledge_objects();
		}

		/* The home */
		else if (ch == '6')
		{
			if (more_tall_display)
			{
				display_change(DSP_CLEAR | DSP_TALL, 0, 0);
			}
			do_cmd_knowledge_home();
		}

		/* Quests */
		else if (ch == '7')
		{
			do_cmd_knowledge_quests(NULL);
		}

		/* Self knowledge  -LM- */
		else if ((ch == '8') && (p_ptr->self_knowledge))
		{
			/*
			 * We should allow self-inspection even when not magically
			 * self-aware, but haven't yet decided exactly what information
			 * should be given, and what not.
			 */
			self_knowledge(p_ptr->self_knowledge ? TRUE : FALSE);
		}

		/* Flush messages */
		message_flush();
	}

	/* Restore previous screen */
	display_change(DSP_RESTORE | DSP_UNLOCK, 0, 0);
}

