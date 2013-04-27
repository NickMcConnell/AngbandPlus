/* File: cmd4.c */

/* Purpose: Interface commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
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
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Forget view */
	p_ptr->update |= (PU_UN_VIEW);

	/* Update view */
	p_ptr->update |= (PU_VIEW);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

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
	char	c;

	int		mode = 0;

	char	tmp[160];


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Forever */
	while (1)
	{
		/* Display the player */
		display_player(mode);

		if (mode == 6)
		{
			mode = 0;
			display_player(mode);
		}

		/* Prompt */
		Term_putstr(2, 23, -1, TERM_WHITE,
			"['c' to change name, 'f' to file, 'h' to change mode, or ESC]");

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
			sprintf(tmp, "%s.txt", player_base);
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
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;


	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	handle_stuff();
}


/*
 * Recall the most recent message
 */
void do_cmd_message_one(void)
{
	/* Recall one message XXX XXX XXX */
	prt(format("> %s", message_str(0)), 0, 0);
}


/*
 * Show previous messages to the user	-BEN-
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


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Process requests until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Dump up to 20 lines of messages */
		for (j = 0; (j < 20) && (i + j < n); j++)
		{
			cptr msg = message_str(i+j);

			/* Apply horizontal scroll */
			msg = (strlen(msg) >= q) ? (msg + q) : "";

			/* Dump the messages, bottom to top */
			Term_putstr(0, 21-j, -1, TERM_WHITE, msg);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = msg;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-msg, 21-j, len, TERM_YELLOW, shower);

					/* Advance */
					str += len;
				}
			}
		}

		/* Display header XXX XXX XXX */
		prt(format("Message Recall (%d-%d of %d), Offset %d",
		    i, i+j-1, n, q), 0, 0);

		/* Display prompt (not very informative) */
		prt("[Press 'p' for older, 'n' for newer, ..., or ESCAPE]", 23, 0);

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
			prt("Show: ", 23, 0);

			/* Get a "shower" string, or continue */
			if (!askfor_aux(shower, 80)) continue;

			/* Okay */
			continue;
		}

		/* Hack -- handle find */
		if (k == '/')
		{
			int z;

			/* Prompt */
			prt("Find: ", 23, 0);

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
		if (i == j) bell();
	}

	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
}


#ifdef ALLOW_CHEATER
/*
 * Number of cheating options
 */
#define CHEAT_MAX 6

/*
 * Cheating options
 */
static option_type cheat_info[CHEAT_MAX] =
{
	{ &cheat_peek,		FALSE,	255,	0x01, 0x00,
	"cheat_peek",		"Peek into object creation" },

	{ &cheat_hear,		FALSE,	255,	0x02, 0x00,
	"cheat_hear",		"Peek into monster creation" },

	{ &cheat_room,		FALSE,	255,	0x04, 0x00,
	"cheat_room",		"Peek into dungeon creation" },

	{ &cheat_xtra,		FALSE,	255,	0x08, 0x00,
	"cheat_xtra",		"Peek into something else" },

	{ &cheat_know,		FALSE,	255,	0x10, 0x00,
	"cheat_know",		"Know complete monster info" },

	{ &cheat_live,		FALSE,	255,	0x20, 0x00,
	"cheat_live",		"Allow player to avoid death" }
};

/*
 * Interact with some options for cheating
 */
static void do_cmd_options_cheat(cptr info)
{
	char	ch;

	int		i, k = 0, n = CHEAT_MAX;

	char	buf[80];


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
			    cheat_info[i].o_desc,
			    (*cheat_info[i].o_var ? "yes" : "no "),
			    cheat_info[i].o_text);
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

			case 'y':
			case 'Y':
			case '6':
			{
				p_ptr->noscore |= (cheat_info[k].o_set * 256 + cheat_info[k].o_bit);
				(*cheat_info[k].o_var) = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				(*cheat_info[k].o_var) = FALSE;
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
#endif


static option_type autosave_info[2] =
{
	{ &autosave_l,      FALSE, 255, 0x01, 0x00,
	    "autosave_l",    "Autosave when entering new levels" },

	{ &autosave_t,      FALSE, 255, 0x02, 0x00,
	    "autosave_t",   "Timed autosave" },
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

	return 0;
}


/*
 * Interact with some options for cheating
 */
static void do_cmd_options_autosave(cptr info)
{
	char	ch;

	int     i, k = 0, n = 2;

	char	buf[80];


	/* Clear screen */
	Term_clear();

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
		sprintf(buf, "%s (RET to advance, y/n to set, 'F' for frequency, ESC to accept) ", info);
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			byte a = TERM_WHITE;

			/* Color current option */
			if (i == k) a = TERM_L_BLUE;

			/* Display the option text */
			sprintf(buf, "%-48s: %s  (%s)",
			    autosave_info[i].o_desc,
			    (*autosave_info[i].o_var ? "yes" : "no "),
			    autosave_info[i].o_text);
			c_prt(a, buf, i + 2, 0);
		}

		prt(format("Timed autosave frequency: every %d turns",  autosave_freq), 5, 0);


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

				(*autosave_info[k].o_var) = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				(*autosave_info[k].o_var) = FALSE;
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
				bell();
				break;
			}
		}
	}
}

/*
 * Interact with some options
 */
static void do_cmd_options_aux(int page, cptr info)
{
	char	ch;

	int	i, k = 0, n = 0;

	int	opt[24];

	char	buf[80];


	/* Lookup the options */
	for (i = 0; i < 24; i++) opt[i] = 0;

	/* Scan the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		/* Notice options on this "page" */
		if (option_info[i].o_page == page) opt[n++] = i;
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
			        option_info[opt[i]].o_desc,
			        (*option_info[opt[i]].o_var ? "yes" : "no "),
			        option_info[opt[i]].o_text);
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

			case 'y':
			case 'Y':
			case '6':
			{
				(*option_info[opt[k]].o_var) = TRUE;
				k = (k + 1) % n;
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				(*option_info[opt[k]].o_var) = FALSE;
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
	int i, j, d;

	int y = 0;
	int x = 0;

	char ch;

	bool go = TRUE;

	u32b old_flag[8];


	/* Memorize old flags */
	for (j = 0; j < 8; j++)
	{
		/* Acquire current flags */
		old_flag[j] = window_flag[j];
	}


	/* Clear screen */
	Term_clear();

	/* Interact */
	while (go)
	{
		/* Prompt XXX XXX XXX */
		prt("Window Flags (<dir>, t, y, n, ESC) ", 0, 0);

		/* Display the windows */
		for (j = 0; j < 8; j++)
		{
			byte a = TERM_WHITE;

			cptr s = angband_term_name[j];

			/* Use color */
			if (use_color && (j == x)) a = TERM_L_BLUE;

			/* Window name, staggered, centered */
			Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
		}

		/* Display the options */
		for (i = 0; i < 16; i++)
		{
			byte a = TERM_WHITE;

			cptr str = window_flag_desc[i];

			/* Use color */
			if (use_color && (i == y)) a = TERM_L_BLUE;

			/* Unused option */
			if (!str) str = "(Unused option)";

			/* Flag name */
			Term_putstr(0, i + 5, -1, a, str);

			/* Display the windows */
			for (j = 0; j < 8; j++)
			{
				byte a = TERM_WHITE;

				char c = '.';

				/* Use color */
				if (use_color && (i == y) && (j == x)) a = TERM_L_BLUE;

				/* Active flag */
				if (window_flag[j] & (1L << i)) c = 'X';

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
			{
				go = FALSE;
				break;
			}

			case 'T':
			case 't':
			{
				/* Clear windows */
				for (j = 0; j < 8; j++)
				{
					window_flag[j] &= ~(1L << y);
				}

				/* Clear flags */
				for (i = 0; i < 16; i++)
				{
					window_flag[x] &= ~(1L << i);
				}

				/* Fall through */
			}

			case 'y':
			case 'Y':
			{
				/* Ignore screen */
				if (x == 0) break;

				/* Set flag */
				window_flag[x] |= (1L << y);
				break;
			}

			case 'n':
			case 'N':
			{
				/* Clear flag */
				window_flag[x] &= ~(1L << y);
				break;
			}

			default:
			{
				d = get_keymap_dir(ch);

				x = (x + ddx[d] + 8) % 8;
				y = (y + ddy[d] + 16) % 16;

				if (!d) bell();
			}
		}
	}

	/* Notice changes */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* Dead window */
		if (!angband_term[j]) continue;

		/* Ignore non-changes */
		if (window_flag[j] == old_flag[j]) continue;

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
 * The user must use the "Ctrl-R" command to "adapt" to changes
 * in any options which control "visual" aspects of the game.
 */
void do_cmd_options(void)
{
	int k;


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();


	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Why are we here */
		prt("Angband options", 2, 0);

		/* Give some choices */
		prt("(1) User Interface Options", 4, 5);
		prt("(2) Disturbance Options", 5, 5);
		prt("(3) Game-Play Options", 6, 5);
		prt("(4) Efficiency Options", 7, 5);

		prt("(Z/5) Zangband Options", 9, 5);
		/* Special choices */
		prt("(D) Base Delay Factor", 10, 5);
		prt("(H) Hitpoint Warning", 11, 5);
		prt("(A) Autosave Options", 12, 5);


		/* Window flags */
		prt("(W) Window Flags", 13, 5);

#ifdef ALLOW_CHEATER
		/* Cheating */
		prt("(C) Cheating Options", 15, 5);
#endif

		/* Prompt */
		prt("Command: ", 18, 0);

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
				do_cmd_options_aux(1, "User Interface Options");
				break;
			}

			/* Disturbance Options */
			case '2':
			{
				/* Spawn */
				do_cmd_options_aux(2, "Disturbance Options");
				break;
			}

			/* Inventory Options */
			case '3':
			{
				/* Spawn */
				do_cmd_options_aux(3, "Game-Play Options");
				break;
			}

			/* Efficiency Options */
			case '4':
			{
				/* Spawn */
				do_cmd_options_aux(4, "Efficiency Options");
				break;
			}

			/* Zangband Options */
			case 'Z': case 'z': case '5':
			{
				do_cmd_options_aux(5, "Zangband Options");
				break;
			}

#ifdef ALLOW_CHEATER
			/* Cheating Options */
			case 'C':
			{
				do_cmd_options_cheat("Cheaters never win");
				break;
			}
#endif

			case 'a':
			case 'A':
			{
				do_cmd_options_autosave("Autosave");
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
					int msec = delay_factor * delay_factor * delay_factor;
					prt(format("Current base delay factor: %d (%d msec)",
					           delay_factor, msec), 22, 0);
					prt("Delay Factor (0-9 or ESC to accept): ", 20, 0);
					k = inkey();
					if (k == ESCAPE) break;
					if (isdigit(k)) delay_factor = D2I(k);
					else bell();
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
					prt(format("Current hitpoint warning: %d0%%",
					           hitpoint_warn), 22, 0);
					prt("Hitpoint Warning (0-9 or ESC to accept): ", 20, 0);
					k = inkey();
					if (k == ESCAPE) break;
					if (isdigit(k)) hitpoint_warn = D2I(k);
					else bell();
				}

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
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
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
	if (!get_string("Pref: ", buf, 80)) return;

	/* Process that pref command */
	(void)process_pref_file_aux(buf);
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
 * Note that both "flush()" calls are extremely important.
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

#endif


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

		/* Encode the key */
		buf[0] = i;
		buf[1] = '\0';
		ascii_to_text(key, buf);

		/* Encode the action */
		ascii_to_text(buf, act);

		/* Dump the macro */
		fprintf(fff, "A:%s\n", buf);
                fprintf(fff, "C:%d:%s\n", mode, key);
	}

	/* Start dumping */
	fprintf(fff, "\n\n\n");


	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}



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


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save screen */
	Term_save();


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
		ascii_to_text(buf, macro__buf);

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
			/* Prompt */
			prt("Command: Load a user pref file", 16, 0);

			/* Prompt */
			prt("File: ", 18, 0);

  			/* Default filename */
			sprintf(tmp, "%s.prf", player_name);

  			/* Ask for a file */
			if (!askfor_aux(tmp, 80)) continue;

  			/* Process the given filename */
			if (0 != process_pref_file(tmp))
			{
				/* Prompt */
				msg_print("Could not load file!");
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
			sprintf(tmp, "%s.prf", player_name);

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
				strcpy(macro__buf, macro__act[k]);

				/* Analyze the current action */
				ascii_to_text(buf, macro__buf);

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
			ascii_to_text(tmp, macro__buf);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro__buf, tmp);

				/* Link the macro */
				macro_add(buf, macro__buf);

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
			sprintf(tmp, "%s.prf", player_name);

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
				strcpy(macro__buf, act);

				/* Analyze the current action */
				ascii_to_text(buf, macro__buf);

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
			ascii_to_text(tmp, macro__buf);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				text_to_ascii(macro__buf, tmp);

				/* Free old keymap */
				string_free(keymap_act[mode][(byte)(buf[0])]);

				/* Make new keymap */
				keymap_act[mode][(byte)(buf[0])] = string_make(macro__buf);

				/* Prompt */
				msg_print("Added a keymap.");
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
			text_to_ascii(macro__buf, buf);
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
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
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


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();


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
			/* Prompt */
			prt("Command: Load a user pref file", 15, 0);

			/* Prompt */
			prt("File: ", 17, 0);

			/* Default filename */
			sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

			/* Query */
			if (!askfor_aux(tmp, 70)) continue;

			/* Process the given filename */
			(void)process_pref_file(tmp);
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
			sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor_aux(tmp, 70)) continue;

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
			sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor_aux(tmp, 70)) continue;

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
			sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor_aux(tmp, 70)) continue;

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

				int da = (byte)(r_ptr->d_attr);
				int dc = (byte)(r_ptr->d_char);
				int ca = (byte)(r_ptr->x_attr);
				int cc = (byte)(r_ptr->x_char);

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

				int da = (byte)k_ptr->d_attr;
				int dc = (byte)k_ptr->d_char;
				int ca = (byte)k_ptr->x_attr;
				int cc = (byte)k_ptr->x_char;

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

				int da = (byte)f_ptr->d_attr;
				int dc = (byte)f_ptr->d_char;
				int ca = (byte)f_ptr->x_attr;
				int cc = (byte)f_ptr->x_char;

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
			reset_visuals();

			/* Message */
			msg_print("Visual attr/char tables reset.");
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
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
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


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();


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
			/* Prompt */
			prt("Command: Load a user pref file", 8, 0);

			/* Prompt */
			prt("File: ", 10, 0);

			/* Default file */
			sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

			/* Query */
			if (!askfor_aux(tmp, 70)) continue;

			/* Process the given filename */
			(void)process_pref_file(tmp);

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
			sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

			/* Get a filename */
			if (!askfor_aux(tmp, 70)) continue;

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
					Term_putstr(i*4, 20, -1, a, "###");

					/* Exhibit all colors */
					Term_putstr(i*4, 22, -1, i, format("%3d", i));
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
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
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
	if (!get_string("Note: ", buf, 60)) return;

	/* Ignore empty notes */
	if (!buf[0] || (buf[0] == ' ')) return;

	/* Add the note to the message recall */
	msg_format("Note: %s", buf);
}


/*
 * Mention the current version
 */
void do_cmd_version(void)
{
    msg_format("You are playing Zceband %d.%d.%d.",
					VER_MAJOR, VER_MINOR, VER_PATCH);
	 msg_format("Current maintainer: %s", MAINTAINER);
}



/*
 * Array of feeling strings
 */
static cptr do_cmd_feeling_text[11] =
{
	"Looks like any other level.",
	"You feel there is something special about this level.",
	"You nearly faint as horrible visions of death fill your mind!",
	"This level looks very dangerous.",
	"You have a very bad feeling...",
	"You have a bad feeling...",
	"You feel nervous.",
	"You feel your luck is turning...",
	"You don't like the look of this place.",
	"This level looks reasonably safe.",
	"What a boring place..."
};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(void)
{
	int cur_size = cur_wid * cur_hgt;
	int max_size = DUN_WID * DUN_HGT;
	cptr t = "";

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
	msg_print(do_cmd_feeling_text[feeling]);

	if (cur_size > max_size) t = "It feels spacious.";
	if (cur_size > max_size * 2) t = "You feel very small.";
	if (cur_size == max_size) t = "It seems normal.";
	if ((cur_wid < DUN_WID) || (cur_hgt < DUN_HGT)) t = "It feels constricted.";
	if (cur_size < max_size) t = "It feels cramped.";
	if (cur_size < max_size / 2) t = "You feel claustrophobic.";

	msg_print(t);
}


/*
 * Hack -- replacement for "fgets()"
 *
 * Read a string, without a newline, to a file
 *
 * Do not process tabs or strip internal non-printables
 */
static errr my_fgets_full(FILE *fff, char *buf, huge n)
{
	huge i = 0;

	char *s;

	char tmp[1024];

	/* Read a line */
	if (fgets(tmp, 1024, fff))
	{
		/* Convert weirdness */
		for (s = tmp; *s; s++)
		{
			/* Handle newline */
			if (*s == '\n')
			{
				/* Terminate */
				buf[i] = '\0';

				/* Success */
				return (0);
			}

			/* Copy */
			buf[i++] = *s;

			/* Check length */
			if (i >= n) break;
		}
	}

	/* Nothing */
	buf[0] = '\0';

	/* Failure */
	return (1);
}


/*
 * Hack -- load a screen dump from a file
 */
void do_cmd_load_screen(void)
{
	int y, x;

	byte a = 0;
	char c = ' ';

	bool okay = TRUE;

	FILE *fff;

	char buf[1024];


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, "dump.txt");

	/* Append to the file */
	fff = my_fopen(buf, "r");

	/* Oops */
	if (!fff) return;


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Clear the screen */
	Term_clear();


	/* Load the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (my_fgets_full(fff, buf, 1024)) okay = FALSE;

		/* Show each row */
		for (x = 0; x < 79; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Get the blank line */
	if (my_fgets_full(fff, buf, 1024)) okay = FALSE;


	/* Dump the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (my_fgets_full(fff, buf, 1024)) okay = FALSE;

		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Hack -- fake monochrome */
			if (!use_color)
			{
				a = TERM_WHITE;
			}
			/* Look up the attr */
			else
			{
				a = strchr(color_char, buf[x]) - color_char;
			}

			/* Put the attr/char */
			Term_draw(x, y, a, c);
		}
	}

	/* Close it */
	my_fclose(fff);

	/* Message */
	msg_print("Screen dump loaded.");
	msg_print(NULL);


	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
}



/*
 * Redefinable "save_screen" action
 */
void (*screendump_aux)(void) = NULL;






/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
	/* Do we use a special screendump function ? */
	if (screendump_aux)
	{
		/* Dump the screen to a graphics file */
		(*screendump_aux)();
	}
	else /* Dump the screen as text */
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

		/* Hack -- drop permissions */
		safe_setuid_drop();

		/* Append to the file */
		fff = my_fopen(buf, "w");

		/* Hack -- grab permissions */
		safe_setuid_grab();

		/* Oops */
		if (!fff) return;


		/* Enter "icky" mode */
		character_icky = TRUE;

		/* Save the screen */
		Term_save();


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
				buf[x] = color_char[a&0x0F];
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


		/* Restore the screen */
		Term_load();

		/* Leave "icky" mode */
		character_icky = FALSE;
	}
}


/*
 * Check the status of "artifacts"
 */
void do_cmd_knowledge_artifacts(void)
{
	int i, k, z, x, y;

	FILE *fff;

	char file_name[1024];

	char base_name[80];

	bool okay[MAX_A_IDX];


	/* Temporary file */
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	/* Scan the artifacts */
	for (k = 0; k < MAX_A_IDX; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* Default */
		okay[k] = FALSE;

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->cur_num) continue;

		/* Assume okay */
		okay[k] = TRUE;
	}

	/* Check the dungeon */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			s16b this_o_idx, next_o_idx;

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Acquire object */
				o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Ignore non-artifacts */
				if (!artifact_p(o_ptr)) continue;

				/* Ignore known items */
				if (object_known_p(o_ptr)) continue;

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
		if (object_known_p(o_ptr)) continue;

		/* Note the artifact */
		okay[o_ptr->name1] = FALSE;
	}

	/* Scan the artifacts */
	for (k = 0; k < MAX_A_IDX; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* List "dead" ones */
		if (!okay[k]) continue;

		/* Paranoia */
		strcpy(base_name, "Unknown Artifact");

		/* Obtain the base object type */
		z = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Real object */
		if (z)
		{
			object_type forge;
			object_type *q_ptr;

			/* Get local object */
			q_ptr = &forge;

			/* Create fake object */
			object_prep(q_ptr, z);

			/* Make it an artifact */
			q_ptr->name1 = k;

			/* Describe the artifact */
			object_desc_store(base_name, q_ptr, FALSE, 0);
		}

		/* Hack -- Build the artifact name */
		fprintf(fff, "     The %s\n", base_name);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Artifacts Seen");

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
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	/* Scan the monster races */
	for (k = 1; k < MAX_R_IDX-1; k++)
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
				fprintf(fff, "     %s is %s\n",
						  (r_name + r_ptr->name),
						  (dead ? "dead" : "alive"));
			}
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Known Uniques");

	/* Remove the file */
	fd_kill(file_name);
}


typedef struct plur_rule plur_rule;

struct plur_rule
{
	cptr sing;
	cptr plur;
	bool (*func)(cptr name);
};


/*
 * Return TRUE if the given string may be pluralized using
 * the rule "y" -> "ies"
 */
static bool plur_rule_y(cptr name)
{
	/* Disallow vowels before the 'y' */
	switch(name[strlen(name) - 2])
	{
		case 'a':
		case 'e':
		case 'i':
		case 'o':
		case 'u':
		{
			/* A vowel */
			return FALSE;
		}
	}

	/* May use the rule */
	return TRUE;
}


/*
 * Array of possible prepositions
 */
static cptr name_prep[] =
{
	" that ",
	" of ",
	" to ",
	" with ",
	NULL
};


/*
 * Array of possible pluralizations
 */
static plur_rule name_plur[] =
{
	/* 'Manes', 'Skull druj', 'Dark young' - unchanged */
	{"Manes", NULL, NULL},
	{"druj", NULL, NULL},
	{"young", NULL, NULL},

	/*
	 * 'White harpy' -> 'White harpies'
	 * (but not 'Stairway' -> 'Stairwaies')
	 */
	{"y", "ies", plur_rule_y},

	/*
	 * 'Giant white mouse' -> 'Giant white mice'
	 * 'Giant white louse' -> 'Giant white lice'
	 */
	{"mouse", "mice", NULL},
	{"louse", "lice", NULL},

	/* 'Pukelman' -> 'Pukelmen' */
	{"man", "men", NULL},

	/* 'Aether vortex' -> 'Aether vortices' */
	{"ex", "ices", NULL},

	/* 'White wolf' -> 'White wolves' */
	{"olf", "olves", NULL},

	/*
	 * 'Uruk' -> 'Uruk-Hai'
	 * 'Olog' -> 'Olog-Hai'
	 */
	{"Uruk", "Uruk-Hai", NULL},
	{"Olog", "Olog-Hai", NULL},

	/* 'Homonculous' -> 'Homonculi' */
	{"ous", "i", NULL},

	/* 'Colossus' -> 'Colossi' */
	{"us", "i", NULL},

	/* '2-headed hydra' -> '2-headed hydrae' */
	{"hydra", "hydrae", NULL},

	/* 'Mumak' -> 'Mumakil' */
	{"Mumak", "Mumakil", NULL},

	/*
	 * 'Yellow mushroom patch' -> 'Yellow mushroom patches'
	 * 'White worm mass' -> 'White worm masses'
	 */
	{"ch", "ches", NULL},
	{"s", "ses", NULL},

	/* "Kobold" -> "Kobolds" */
	{"", "s", NULL},

	{NULL, NULL, NULL}
};


/*
 * Pluralize a monster name.
 */
void plural_aux(char *name)
{
	/* Acquire the string terminator */
	char *u = &name[strlen(name)];

	int i;

	/* Scan a name for prepositions */
	for (i = 0; name_prep[i]; i++)
	{
		cptr t = name_prep[i];

		if (strstr(name, t))
		{
			char buf[160];
			char *v;

			strcpy(buf, name);
			v = strstr(buf, t);

			/* Remove the preposition */
			*v = '\0';

			/* Try to pluralize this name */
			plural_aux(buf);

			/* Complete the pluralized name */
			strcat(buf, strstr(name, t));

			/* Write the new name */
			strcpy(name, buf);

			/* Done */
			return;
		}
	}

	/* 'Creeping copper coins' -> 'Piles of creeping copper coins' */
	if (strstr(name, "coins"))
	{
		char buf[160];
		strcpy(buf, "Piles of ");

		/* Fix the capitalization */
		name[0] = tolower(name[0]);

		strcat(buf, name);
		strcpy(name, buf);

		/* Done */
		return;
	}

	/* Check for suffixes that can be pluralized */
	for (i = 0; name_plur[i].sing; i++)
	{
		plur_rule *r_ptr = &name_plur[i];
		int s_len = strlen(r_ptr->sing);

		/* Require matching suffix */
		if (strcmp(u - s_len, r_ptr->sing)) continue;

		/* Verify the rule if necessary */
		if (r_ptr->func && !((r_ptr->func)(name))) continue;

		/* Some suffixes need no change */
		if (!r_ptr->plur) break;

		/* Copy the plural ending */
		strcpy(u - s_len, r_ptr->plur);

		/* Done */
		break;
	}
}


/*
 * Display current pets
 *
 */
static void do_cmd_knowledge_pets(void)
{
	int i;

	FILE *fff;

	monster_type * m_ptr;

	int t_friends = 0;
	int t_levels = 0;
	int show_upkeep = 0;
	int upkeep_divider = 20;

	char file_name[1024];


	/* Temporary file */
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	if (p_ptr->pclass == CLASS_MAGE) upkeep_divider = 15;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) upkeep_divider = 12;

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for friendly monsters */
		if (m_ptr->smart & (SM_FRIEND))
		{
			char pet_name[160];
			t_friends++;
			t_levels += r_info[m_ptr->r_idx].level;
			monster_desc(pet_name, m_ptr, 0x88);
			fprintf(fff, "%s (%s)\n", pet_name, look_mon_desc(i));
		}
	}

	if (t_friends > 1 + (p_ptr->level / (upkeep_divider)))
	{
		show_upkeep = (t_levels);

		if (show_upkeep > 100) show_upkeep = 100;
		else if (show_upkeep < 10) show_upkeep = 10;
	}


	fprintf(fff,"----------------------------------------------\n");
	fprintf(fff,"   Total: %d pet%s.\n", t_friends, (t_friends==1?"":"s"));
	fprintf(fff,"   Upkeep: %d%% mana.\n", show_upkeep);


	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Current Pets");

	/* Remove the file */
	fd_kill(file_name);
}



/*
 * Total kill count
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
static void do_cmd_knowledge_kill_count(void)
{
	int k;

	FILE *fff;

	char file_name[1024];

	u32b Total = 0;


	/* Temporary file */
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	{
		/* Monsters slain */
		int kk;

		for (kk = 1; kk < MAX_R_IDX-1; kk++)
		{
			monster_race *r_ptr = &r_info[kk];

			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				bool dead = (r_ptr->max_num == 0);

				if (dead)
				{
					Total++;
				}
			}
			else
			{
				s16b This = r_ptr->r_pkills;

				if (This > 0)
				{
					Total += This;
				}
			}
		}

		if (Total < 1)
			fprintf(fff,"You have defeated no enemies yet.\n\n");
		else if (Total == 1)
			fprintf(fff,"You have defeated one enemy.\n\n");
		else
			fprintf(fff,"You have defeated %lu enemies.\n\n", Total);
	}

	Total = 0;

	/* Scan the monster races */
	for (k = 1; k < MAX_R_IDX-1; k++)
	{
		monster_race *r_ptr = &r_info[k];

		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			bool dead = (r_ptr->max_num == 0);

			if (dead)
			{
				/* Print a message */
				fprintf(fff, "     %s\n",
				    (r_name + r_ptr->name));
				Total++;
			}
		}
		else
		{
			s16b This = r_ptr->r_pkills;

			if (This > 0)
			{
				if (This < 2)
				{
					if (strstr(r_name + r_ptr->name, "coins"))
					{
						char buf[160];

						strcpy(buf, r_name + r_ptr->name);
						buf[0] = tolower(buf[0]);

						fprintf(fff, "     1 Pile of %s\n", buf);
					}
					else
					{
						fprintf(fff, "     1 %s\n", (r_name + r_ptr->name));
					}
				}
				else
				{
					char buf[160];
					strcpy(buf, (r_name + r_ptr->name));
					plural_aux(buf);
					fprintf(fff, "     %d %s\n", This, buf);
				}

				Total += This;
			}
		}
	}

	fprintf(fff,"----------------------------------------------\n");
	fprintf(fff,"   Total: %lu creature%s killed.\n", Total, (Total==1?"":"s"));

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Kill Count");

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
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	/* Scan the object kinds */
	for (k = 1; k < MAX_K_IDX; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Hack -- skip artifacts */
		if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

		/* List known flavored objects */
		if (k_ptr->flavor && k_ptr->aware)
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
    show_file(file_name, "Known Objects");

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * List mutations we have...
 *
 */
void do_cmd_knowledge_mutations(void)
{

	FILE *fff;

	char file_name[1024];


	/* Temporary file */
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

	if (fff) dump_mutations(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Mutations");

	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Display the damage figure of an object
 * (used by compare_weapon_aux1)
 *
 * Only accurate for the current weapon, because it includes
 * the current +dam of the player.
 */
static void list_weapon_aux1(bool flag, cptr str, int mult,
									  int blows, int dd, int ds, int to_d,
									  int row, int col)
{
	char tmp_str[80];

	int min = blows * (mult * dd + to_d);
	int max = blows * (mult * dd * ds + to_d);
	int mid = (min + max) * 5;

	byte color = flag ? TERM_WHITE : TERM_SLATE;

	/* Print the intro text */
	c_put_str(color, str, row, col);

	/* Display damage only if required */
	if (!flag) return;

	/* Calculate the min and max damage figures */
	sprintf(tmp_str, "%6d%6d%9d.%d", min, max, mid / 10, mid % 10);

	/* Print the damage */
	put_str(tmp_str, row, col + 16);
}


/*
 * Display various types of damage done
 */
static void list_weapon_aux2(u32b f1, int blows, int dd, int ds, int to_d, int *row, int col)
{
	/* Start the table */
	put_str("Damage", *row, col);
	put_str("   Min   Max    Average", (*row)++, col + 16);
	put_str("---------------------------------------", (*row)++, col);

	/* Damage for one blow (if it hits) */
	list_weapon_aux1(TRUE, "Single", 1, 1, dd, ds, to_d, (*row)++, col);

	/* Damage for the complete attack (if all blows hit) */
	list_weapon_aux1(TRUE, "Total", 1, blows, dd, ds, to_d, (*row)++, col);

	/* No extra knowledge */
	if (!f1) return;

	/* Show minor slays */
	list_weapon_aux1((f1 & TR1_SLAY_ANIMAL) != 0, "Animals", 2, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_SLAY_EVIL) != 0, "Evil", 2, blows, dd, ds, to_d, (*row)++, col);

	/* Show major slays */
	list_weapon_aux1((f1 & TR1_SLAY_UNDEAD) != 0, "Undead", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_SLAY_DEMON) != 0, "Demons", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_SLAY_ORC) != 0, "Orcs", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_SLAY_TROLL) != 0, "Trolls", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_SLAY_GIANT) != 0, "Giants", 3, blows, dd, ds, to_d, (*row)++, col);

	/* Show dragon slays */
	if (f1 & TR1_KILL_DRAGON)
	{
		list_weapon_aux1(TRUE, "Dragons", 5, blows, dd, ds, to_d, (*row)++, col);
	}
	else
	{
		list_weapon_aux1((f1 & TR1_SLAY_DRAGON) != 0, "Dragons", 3, blows, dd, ds, to_d, (*row)++, col);
	}

	/* Show elemental brands */
	list_weapon_aux1((f1 & TR1_BRAND_ACID) != 0, "Acid", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_BRAND_ELEC) != 0, "Elec", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_BRAND_FIRE) != 0, "Fire", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_BRAND_COLD) != 0, "Cold", 3, blows, dd, ds, to_d, (*row)++, col);
	list_weapon_aux1((f1 & TR1_BRAND_POIS) != 0, "Poison", 3, blows, dd, ds, to_d, (*row)++, col);
}


/*
 * Displays (known) damage info about a melee weapon
 */
static void list_melee(object_type *o_ptr, int *row, int col)
{
	u32b f1, f2, f3;

	bool heavy_wield = (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10);

	int i;

	int to_h = p_ptr->dis_to_h, to_d = p_ptr->dis_to_d;
	int dd = o_ptr->dd, ds = o_ptr->ds;
	int blows = 1, extra_blows = 0;

	int speed = (o_ptr == &inventory[INVEN_WIELD]) ? p_ptr->pspeed : 110;
	int energy = extract_energy[MAX(0, MIN(199, speed))];

	char o_name[80];
	char tmp_str[80];

	for (i = INVEN_BOW; i < INVEN_TOTAL; i++)
	{
		object_type *i_ptr = &inventory[i];

		/* Extract some flags */
		object_flags_known(i_ptr, &f1, &f2, &f3);

		/* Add in the "bonus blows" */
		if (f1 & (TR1_BLOWS)) extra_blows += i_ptr->pval;
	}

	/* Extract to-hit and to-dam if known */
	if (object_known_p(o_ptr))
	{
		to_h += o_ptr->to_h;
		to_d += o_ptr->to_d;
	}

	/* Get the known flags */
	object_flags_known(o_ptr, &f1, &f2, &f3);

	/* Affect blows */
	if (!heavy_wield)
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div;

		/* Analyze the class */
		switch (p_ptr->pclass)
		{
			/* Warrior */
			case CLASS_WARRIOR:
				num = 6; wgt = 30; mul = 5; break;

			/* Mage */
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
				num = 4; wgt = 40; mul = 2; break;

			/* Priest, Mindcrafter */
			case CLASS_PRIEST:
			case CLASS_MINDCRAFTER:
				num = 5; wgt = 35; mul = 3; break;

			/* Rogue */
			case CLASS_ROGUE:
				num = 5; wgt = 30; mul = 3; break;

			/* Ranger */
			case CLASS_RANGER:
				num = 5; wgt = 35; mul = 4; break;

			/* Paladin */
			case CLASS_PALADIN:
				num = 5; wgt = 30; mul = 4; break;

			/* Warrior-Mage */
			case CLASS_WARRIOR_MAGE:
				num = 5; wgt = 35; mul = 3; break;

			/* Chaos Warrior */
			case CLASS_CHAOS_WARRIOR:
				num = 5; wgt = 30; mul = 4; break;

			/* Monk */
			case CLASS_MONK:
				num = (p_ptr->level<40?3:4); wgt = 40; mul = 4; break;
		}

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

		/* Access the strength vs weight */
		str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		blows = blows_table[str_index][dex_index];

		/* Maximal value */
		if (blows > num) blows = num;

		/* Add the bonus blows */
		if (f1 & (TR1_BLOWS)) blows += o_ptr->pval;
		blows += extra_blows;

		/* Level bonus for warriors (1-3) */
		if (p_ptr->pclass == CLASS_WARRIOR) blows += (p_ptr->level) / 15;

		/* Require at least one blow */
		if (blows < 1) blows = 1;
	}

	/* Account for speed */
	blows = blows * energy / 10;

	/* Print the weapon name */
	object_desc(o_name, o_ptr, TRUE, 2);
	put_str(o_name, (*row)++, col);

	sprintf(tmp_str, "To Hit: %d    To Damage: %d", to_h, to_d);
	put_str(tmp_str, (*row)++, col + 2);

	/* Print the player's number of blows */
	sprintf(tmp_str, "Attacks per round: %d", blows);
	put_str(tmp_str, (*row)++, col + 2);

	(*row)++;

	/* Display the extra knowledge */
	list_weapon_aux2(f1, blows, dd, ds, to_d, row, col);
}


/*
 * Displays (known) damage info about a bow and ammo
 */
static void list_bow(object_type *i_ptr, object_type *j_ptr, int *row, int col)
{
	u32b f1, f2, f3;

	bool heavy_shoot = (adj_str_hold[p_ptr->stat_ind[A_STR]] < i_ptr->weight / 10);

	int to_h = p_ptr->dis_to_h, to_d = 0;
	int dd = j_ptr->dd, ds = j_ptr->ds;
	int shots = 1, extra_shots = 0;

	int speed = (i_ptr == &inventory[INVEN_BOW]) ? p_ptr->pspeed : 110;
	int energy = extract_energy[MAX(0, MIN(199, speed))];

	char o_name[80];
	char tmp_str[80];

	/* Assume a base multiplier */
	int tmul = 1;

	/* Analyze the launcher */
	switch (i_ptr->sval)
	{
		/* Sling and ammo */
		case SV_SLING:
		{
			tmul = 2;
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			tmul = 2;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			tmul = 3;
			break;
		}

		/* Light Crossbow and Bolt */
		case SV_LIGHT_XBOW:
		{
			tmul = 3;
			break;
		}

		/* Heavy Crossbow and Bolt */
		case SV_HEAVY_XBOW:
		{
			tmul = 4;
			break;
		}
	}

	/* Extract to-hit and to-dam if known */
	if (object_known_p(i_ptr))
	{
		to_h += i_ptr->to_h;
		to_d += i_ptr->to_d;
	}

	/* Extract to-hit and to-dam if known */
	if (object_known_p(j_ptr))
	{
		to_h += j_ptr->to_h;
		to_d += j_ptr->to_d;
	}

	/* Extract launcher flags */
	object_flags_known(i_ptr, &f1, &f2, &f3);

	/* Get extra "power" from "extra might" */
	if (f3 & (TR3_XTRA_MIGHT)) tmul++;

	/* Add extra shots if known */
	if (f3 & (TR3_XTRA_SHOTS)) extra_shots++;

	/* Get the known flags */
	object_flags_known(j_ptr, &f1, &f2, &f3);

	/* Affect shots */
	if (!heavy_shoot)
	{
		/* Hack -- Reward High Level Rangers using Bows */
		if ((p_ptr->pclass == CLASS_RANGER) && ((i_ptr->sval == SV_SHORT_BOW) ||
			 (i_ptr->sval == SV_LONG_BOW)))
		{
			/* Extra shot at level 20 */
			if (p_ptr->level >= 20) shots++;

			/* Extra shot at level 40 */
			if (p_ptr->level >= 40) shots++;
		}

		/*
		 * Addendum -- also "Reward" high level warriors,
		 * with _any_ missile weapon -- TY
		 */
		if (p_ptr->pclass == CLASS_WARRIOR)
		{
			/* Extra shot at level 25 */
			if (p_ptr->level >= 25) shots++;

			/* Extra shot at level 50 */
			if (p_ptr->level >= 50) shots++;
		}

		/* Add in the "bonus shots" */
		shots += extra_shots;

		/* Require at least one shot */
		if (shots < 1) shots = 1;
	}

	/* Account for speed */
	shots = shots * energy / 10;

	/* Print the launcher name */
	object_desc(o_name, i_ptr, TRUE, 2);
	put_str(o_name, (*row)++, col);

	/* Print the ammo name */
	object_desc(o_name, j_ptr, TRUE, 2);
	put_str(o_name, (*row)++, col + 1);

	sprintf(tmp_str, "To Hit: %d    To Damage: %d", to_h, to_d);
	put_str(tmp_str, (*row)++, col + 2);

	/* Print the player's number of shots */
	sprintf(tmp_str, "Shots per round: %d", shots);
	put_str(tmp_str, (*row)++, col + 2);

	(*row)++;

	/* Display the extra knowledge */
	list_weapon_aux2(f1, shots, dd * tmul, ds, to_d * tmul, row, col);
}


/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "shots"
 */
static bool item_tester_hook_shot(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "arrows"
 */
static bool item_tester_hook_arrow(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_ARROW:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "bolts"
 */
static bool item_tester_hook_bolt(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_BOLT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Display abilities of weapon
 */
void do_cmd_knowledge_damage(void)
{
	int item, ammo;

	object_type object_type_body;

	object_type *i_ptr, *j_ptr = NULL;

	int row = 0;

	/* Get a weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Get the weapon */
	if (!get_item(&item, "View which weapon? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no weapons.");
		return;
	}

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		i_ptr = &inventory[item];
	}
	/* Get the item (on the floor) */
	else
	{
		i_ptr = &o_list[0 - item];
	}

	if (i_ptr->tval == TV_BOW)
	{
		int tval = 0, k_idx;

		switch(i_ptr->sval)
		{
			case SV_SLING:
			{
				tval = TV_SHOT;
				item_tester_hook = item_tester_hook_shot;
				break;
			}
			case SV_SHORT_BOW: case SV_LONG_BOW:
			{
				tval = TV_ARROW;
				item_tester_hook = item_tester_hook_arrow;
				break;
			}
			case SV_LIGHT_XBOW: case SV_HEAVY_XBOW:
			{
				tval = TV_BOLT;
				item_tester_hook = item_tester_hook_bolt;
				break;
			}
		}

		/* Get the ammo */
		if (!get_item(&ammo, "Use which ammo? ", TRUE, TRUE, TRUE))
		{
			/* Acquire the local object */
			j_ptr = &object_type_body;

			/* Wipe it */
			object_wipe(j_ptr);

			/* Find the basic ammo */
			k_idx = lookup_kind(tval, SV_AMMO_NORMAL);

			/* Oops */
			if (!k_idx) return;

			/* Make some normal ammo */
			object_prep(j_ptr, k_idx);
		}
		/* Get the item (in the pack) */
		else if (ammo >= 0)
		{
			j_ptr = &inventory[ammo];
		}
		/* Get the item (on the floor) */
		else
		{
			j_ptr = &o_list[0 - ammo];
		}
	}

	/* Save the screen */
	Term_save();

	/* Clear the screen */
	Term_clear();

	/* Display a melee weapon */
	if (i_ptr->tval != TV_BOW)
	{
		list_melee(i_ptr, &row, 0);
	}
	/* Display a missile weapon */
	else
	{
		list_bow(i_ptr, j_ptr, &row, 0);
	}

	/* Pause */
	prt("[Press any key to continue]", row + 1, 0);
	inkey();

	/* Load the screen */
	Term_load();
}


/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
	int i;


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();


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
		prt("(3) Display known objects", 6, 5);
		prt("(4) Display kill count", 7, 5);
		prt("(5) Display mutations", 8, 5);
		prt("(6) Display current pets", 9, 5);

		/* Prompt */
		prt("Command: ", 11, 0);

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
			do_cmd_knowledge_uniques();
		}


		/* Objects */
		else if (i == '3')
		{
			/* Spawn */
			do_cmd_knowledge_objects();
		}

		/* Kill count  */
		else if (i == '4')
		{
			/* Spawn */
			do_cmd_knowledge_kill_count();
		}

		/* Mutations */
		else if (i == '5')
		{
			/* Spawn */
			do_cmd_knowledge_mutations();
		}

		/* Pets */
		else if (i == '6')
		{
			do_cmd_knowledge_pets();
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
	Term_load();

	/*
	 * For some reason (probably incompatability with 2.8.1) the above
	 * does not seem to work here... hence the following line
	 */
	do_cmd_redraw();

	/* Leave "icky" mode */
	character_icky = FALSE;
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
	int full = hour * 100 + min;

	int start = 9999;
	int end = -9999;

	int num = 0;

	char desc[1024];

	char buf[1024];

	FILE *fff;

	strcpy(desc, "It is a strange time.");

	/* Message */
	msg_format("This is day %d. The time is %d:%02d %s.",
				  day, (hour % 12 == 0) ? 12 : (hour % 12),
				  min, (hour < 12) ? "AM" : "PM");

	/* Find the path */
	if (!rand_int(10) || p_ptr->image)
	{
		path_build(buf, 1024, ANGBAND_DIR_FILE, "timefun.txt");
	}
	else
	{
		path_build(buf, 1024, ANGBAND_DIR_FILE, "timenorm.txt");
	}

	/* Open this file */
	fff = my_fopen(buf, "rt");

	/* Oops */
	if (!fff) return;

	/* Find this time */
	while (!my_fgets(fff, buf, 1024))
	{
		/* Ignore comments */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Ignore invalid lines */
		if (buf[1] != ':') continue;

		/* Process 'Start' */
		if (buf[0] == 'S')
		{
			/* Extract the starting time */
			start = atoi(buf + 2);

			/* Assume valid for an hour */
			end = start + 59;

			/* Next... */
			continue;
		}

		/* Process 'End' */
		if (buf[0] == 'E')
		{
			/* Extract the ending time */
			end = atoi(buf + 2);

			/* Next... */
			continue;
		}

		/* Ignore incorrect range */
		if ((start > full) || (full > end)) continue;

		/* Process 'Description' */
		if (buf[0] == 'D')
		{
			num++;

			/* Apply the randomizer */
			if (!rand_int(num)) strcpy(desc, buf + 2);

			/* Next... */
			continue;
		}
	}

	/* Message */
	msg_print(desc);

	/* Close the file */
	my_fclose(fff);
}
