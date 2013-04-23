#define CMD4_C
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

	/* Forget lite/view */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update lite/view */
	p_ptr->update |= (PU_VIEW | PU_LITE);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= PR_ALL & ~PR_WIPE_0;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_SHOPS);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_MONSTER | PW_OBJECT | PW_OBJECT_DETAILS);

	/* Hack - recalculate the panel. */
	verify_panel(TRUE);

	/* Hack -- update */
	handle_stuff();

	/* Hack - reinitialise the help files. */
	init_help_files();

	/* Redraw every window */
	for (j = 0; j < 8; j++)
	{
		/* Dead window */
		if (!windows[j].term) continue;

		/* Activate */
		Term_activate(windows[j].term);

		/* Redraw */
		Term_redraw();

		/* Refresh */
		Term_fresh();
	}

	/* Restore */
	Term_activate(old);
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

		if (mode == 7)
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
			bell(0);
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
		const int max = Term->hgt-4, prompty = max+3;

		/* Clear screen */
		Term_clear();

		/* Dump up to 20 lines of messages */
		for (j = 0; (j < max) && (i + j < n); j++)
	{
			cptr msg = message_str((short)(i+j));

			/* Apply horizontal scroll */
			msg = (strlen(msg) >= (size_t)q) ? (msg + q) : "";

			/* Dump the messages, bottom to top */
			Term_putstr(0, max+1-j, -1, TERM_WHITE, msg);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = msg;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
		{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-msg, max+1-j, len, TERM_YELLOW, shower);

					/* Advance */
					str += len;
				}
			}
		}

		/* Display header XXX XXX XXX */
		prt(format("Message Recall (%d-%d of %d), Offset %d",
		    i, i+j-1, n, q), 0, 0);

		/* Display prompt (not very informative) */
		prt("[Press 'p' for older, 'n' for newer, ..., or ESCAPE]", prompty, 0);

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
			prt("Show: ", prompty, 0);

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
			prt("Find: ", prompty, 0);

			/* Get a "finder" string, or continue */
			if (!askfor_aux(finder, 80)) continue;

			/* Show it */
			strcpy(shower, finder);

			/* Scan messages */
			for (z = i + 1; z < n; z++)
			{
				cptr msg = message_str((short)z);

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
			if (i + max/2 < n) i += max/2;
		}

		/* Recall 20 older messages */
		if ((k == 'p') || (k == KTRL('P')) || (k == ' '))
		{
			/* Go older if legal */
			if (i + max < n) i += max;
		}

		/* Recall 20 newer messages */
		if ((k == 'n') || (k == KTRL('N')))
			{
			/* Go newer (if able) */
			i = (i >= max) ? (i - max) : 0;
			}

		/* Recall 10 newer messages */
		if (k == '-')
		{
			/* Go newer (if able) */
			i = (i >= max/2) ? (i - max/2) : 0;
		}

		/* Recall 1 newer messages */
		if (k == '2')
		{
			/* Go newer (if able) */
			i = (i >= 1) ? (i - 1) : 0;
	}

		/* Hack -- Error of some kind */
		if (i == j) bell(0);
	}

	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
}




static option_type autosave_info[3] =
{
	{ &autosave_l,      FALSE, 255, 0x01, 0x00,
	    "autosave_l",    "Autosave when entering new levels" },

	{ &autosave_t,      FALSE, 255, 0x02, 0x00,
	    "autosave_t",   "Timed autosave" },

	{ &autosave_q,      FALSE, 255, 0x04, 0x00,
	    "autosave_q",   "Quiet autosaves" },
	};
       
static s16b toggle_frequency(s16b current)
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

	int     i, k = 0, n = 3;

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

		/* Track this option. */
		help_track(autosave_info[k].o_text);

		/* Get a key */
		ch = inkey();

		/* Assume the help needed has changed. */
		help_track(NULL);

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
				break;
			}

			default:
		{
				bell(0);
				break;
		}
	}
	}
}

/*
 * Display help in the main window. Remove it at the next keypress.
 */
void display_help(cptr help)
{
	/* Save the screen somewhere. */
	int t = Term_save_aux();
	bool v;

	/* Display the help, if any. */
	if (help) help_track(help);

	/* Show the help. */
	clear_from(0);
	win_help_display();

	/* Ask for a keypress without a visible cursor. */
	Term_get_cursor(&v);
	Term_set_cursor(0);
	inkey();
	Term_set_cursor(v);

	/* Restore the screen. */
	Term_load_aux(t);
	Term_release(t);

	/* Forget the help, if any. */
	if (help) help_track(NULL);
}

/*
 * Display a text file on screen. Returns success or failure.
 */
static bool showfile(cptr name, byte col)
{
	int i = col;
	FILE *fp;
	char buf[1024];

	/* Check that the filename refers to a real file */
	if (!(fp = my_fopen_path(ANGBAND_DIR_FILE, name, "r"))) return FALSE;

	/* Dump the file to the screen */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Display and advance */
		Term_putstr(0, i++, -1, TERM_WHITE, buf);
	}

	/* Close */
	my_fclose(fp);

	/* Flush it */
	Term_fresh();

	/* Success */
	return TRUE;
}

/*
 * If option_info[i] has no effect because of other option settings,
 * treat it in a special way to make this clear.
 */
static bool opt_is_forced(int i)
	{
	bool *o_var = option_info[i].o_var;
	force_type *fs_ptr;

	for (fs_ptr = option_force; fs_ptr->forcing_opt; fs_ptr++)
		{
		if (o_var == fs_ptr->forced_opt &&
			*(fs_ptr->forcing_opt) == fs_ptr->forcing_value) return TRUE;
		}
	return FALSE;
	}

/*
 * Modify various globals when an option is set/unset.
 */
void opt_special_effect(const option_type * const op_ptr)
{
	if (op_ptr->o_page == OPTS_CHEAT)
	{
		noscore |= (1L<<op_ptr->o_bit);
	}
	if (op_ptr->o_var == &equippy_chars)
	{
		p_ptr->redraw |= PR_EQUIPPY;
	}
}


/*
 * Interact with some options
 */
void do_cmd_options_aux(int page, cptr info, cptr file)
{
	char	ch;

	int	i, k = 0, n;

	int	opt[MAX_OPTS_PER_PAGE];
	bool old_val[MAX_OPTS_PER_PAGE];

	char	buf[80];


	/* Lookup the options */
	for (i = 0; i < MAX_OPTS_PER_PAGE; i++) opt[i] = 0;

	/* Scan the options */
	for (i = n = 0; option_info[i].o_desc; i++)
	{
		/* Notice options on this "page" */
		if (option_info[i].o_page == page)
		{
			old_val[n] = *option_info[i].o_var;
			opt[n++] = i;
		}
	}


	/* Clear screen */
	Term_clear();

	/* Give a short explanation at the bottom of the "Spoiler" menu */
	if (file && !showfile(file, n+3))
	{
		c_prt(TERM_RED, format("ANGBAND_DIR_FILE/%s not found.", file), n+3, 0);
	}

	/* Interact with the player */
	while (TRUE)
	{
		/* Prompt XXX XXX XXX */
		sprintf(buf, "%s (RET to advance, y/n to set, ESC to accept, ? for help) ", info);
		prt(buf, 0, 0);

		/* Display the options */
		for (i = 0; i < n; i++)
		{
			int a;
			cptr state, effective;

			/* Color current option */
			if (i == k)
				a = TERM_L_BLUE;
			else
				a = TERM_WHITE;

			if (page != OPTS_BIRTH)
				effective = "";
			else if (option_info[opt[i]+1].o_page != OPTS_BIRTHR)
				effective = ".... ";
			else if (*option_info[opt[i]+1].o_var)
				effective = "(yes)";
			else
				effective = "(no) ";

			if (opt_is_forced(opt[i]))
				state = "N/A";
			else if (*option_info[opt[i]].o_var)
				state = "yes";
			else
				state = "no ";

			/* Display the option text */
			sprintf(buf, "%-48s: %s  %s(%s)", option_info[opt[i]].o_desc,
				state, effective, option_info[opt[i]].o_text);
			c_prt(a, buf, i + 2, 0);
		}

		/* Hilite current option */
		move_cursor(k + 2, 50);

		/* Track this option. */
		help_track(option_info[opt[k]].o_text);

		/* Get a key */
		ch = inkey();

		/* Assume the help needed has changed. */
		help_track(NULL);

		/* Analyze */
		switch (ch)
		{
			case ESCAPE:
			{
				for (i = 0; i < n; i++)
				{
					if (old_val[i] != *option_info[opt[i]].o_var)
						opt_special_effect(option_info+opt[i]);
				}
				return;
			}

			case '-':
			case '8':
			{
				do {
				k = (n + k - 1) % n;
				} while(opt_is_forced(opt[k]));
				break;
			}

			case ' ':
			case '\n':
			case '\r':
			case '2':
			{
				do {
				k = (k + 1) % n;
				} while(opt_is_forced(opt[k]));
				break;
			}

			case 'y':
			case 'Y':
			case '6':
			{
				(*option_info[opt[k]].o_var) = TRUE;
				do {
				k = (k + 1) % n;
				} while(opt_is_forced(opt[k]));
				break;
			}

			case 'n':
			case 'N':
			case '4':
			{
				(*option_info[opt[k]].o_var) = FALSE;
				do {
				k = (k + 1) % n;
				} while(opt_is_forced(opt[k]));
				break;
			}

			case 'x':
			case 'X':
			{
				(*option_info[opt[k]].o_var) ^= 1;
				do {
				k = (k + 1) % n;
				} while(opt_is_forced(opt[k]));
				break;
			}
			case '?':
			{
				/* Hack - show help on the main term. */
				display_help(option_info[opt[k]].o_text);
				break;
			}
			default:
			{
				bell(0);
				break;
			}
		}
	}
}

/*
 * Add lots of space. This is useful when writing to a line of the screen
 * which should be cleared.
 */
void clear_f0(char *buf, uint max, cptr UNUSED fmt, va_list UNUSED *vp)
{
	if (max > 256) max = 256;
	memset(buf, ' ', max-1);
	buf[max-1] = '\0';
}

#define PRI(window, display) (windows[window].pri[display])
#define REP(window, display) (windows[window].rep[display])

/*
 * Modify the "window" options
 */
static void do_cmd_options_win(void)
{
	int x, y;
	bool second;

	/* Clear screen */
	Term_clear();

	/* Interact */
	for (x = y = 0, second = FALSE;;)
	{
		int i,j;
		char ch, min;
		char buf[80] = "Display ";

		/* Choose a set of characters which has no other meaning in
		 * the layout */
		if (!rogue_like_commands)
			min = 'a'-1;
		else
			min = '0'-1;

		/* Prompt XXX XXX XXX */
		mc_put_fmt(0, 0, "Window Flags (<dir>, +, -, ., %c-%c, ESC) %v",
			min+1, min+10, clear_f0);

		/* Display the windows */
		for (j = 0; j < 8; j++)
		{
			cptr s = windows[j].name;

			/* Use colour to indicate the current selection. */
			char a = (j == x) ? 'B' : 'w';

			/* Window name, staggered, centered */
			mc_put_fmt(2+j%2, 35 + j * 5 - strlen(s) / 2, "$%c%s", a, s);
		}

		/* Display the options */
		for (i = 0; i < NUM_DISPLAY_FUNCS; i++)
		{
			/* Use colour to indicate the current selection. */
			char a = (i == y) ? 'B' : 'w';

			/* Write the flag name in. */
			mc_put_fmt(i+5, 0, "$%cDisplay %s", a, display_func[i].name);		

			/* Display the windows */
			for (j = 0; j < 8; j++)
			{
				char c1 = '.', c2 = ' ';

				/* Use colour to indicate the current selection. */
				char a = (i == y && j == x) ? 'B' : 'w';

				/* Active flags */
				if (REP(j,i)) c1 = min+REP(j,i);
				if (PRI(j,i)) c2 = min+PRI(j,i);

				/* Flag values */
				mc_put_fmt(i+5, 35+j*5, "$%c%c%c", a, c1, c2);
			}
		}

		/* Place Cursor */
		Term_gotoxy(35 + x * 5+((second) ? 1 : 0), y + 5);

		/* Copy the display name locally. */
		sprintf(buf, "%.71s", display_func[y].name);

		/* Track this option. */
		help_track(buf);
	
		/* Get key */
		ch = inkey();

		/* Assume the help needed has changed. */
		help_track(NULL);

		/* Analyze */
		switch (ch)
		{
			/* Finish, accepting the current settings. */
			case ESCAPE:
			{
				/* Redraw windows. */
				p_ptr->window |= PW_RETURN;
				return;
			}

			/* Increase both priority settings by 1. */
			case '+':
			{
				PRI(x,y)++;
				REP(x,y)++;
				second = FALSE;
				break;
			}

			/* Decrease both priority settings by 1. */
			case '-':
			{
				PRI(x,y)--;
				REP(x,y)--;
				second = FALSE;
				break;
			}

			/* Switch between PRI and REP. */
			case '\t':
			{
				second = !second;
				break;
			}

			/* Set to 0. */
			case '.':
			{
				/* A REP of 0 means that the display is never
				 * shown, so treat in a special way. A PRI of
				 * 0 is entirely normal. */
				if (!second) REP(x,y) = 0;
				PRI(x,y) = 0;
				second = FALSE;
				break;
			}

			default:
			{
				/* Set to a number between 1 and 10. */
				if (ch > min && ch <= min+10)
				{
					if (second)
					{
						PRI(x,y) = ch-min;
					}
					else
					{
						REP(x,y) = ch-min;
					}
					second = !second;
					break;
				}
				/* Move around the table. */
				else if ((i = get_keymap_dir(ch)))
				{
					x = (x + ddx[i] + ANGBAND_TERM_MAX) % ANGBAND_TERM_MAX;
					y = (y + ddy[i] + NUM_DISPLAY_FUNCS) % NUM_DISPLAY_FUNCS;
				}
				/* Invalid choice. */
				else
				{
					bell(0);
				}
			}
		}
	}
}

/*
 * Fill the screen with dots as a way of making PR_* displays show up
 * without forcing them to draw anything themselves by some more complex
 * mechanism.
 */
static void fill_screen(char attr)
{
	int y;
	for (y = 0; y < Term->hgt; y++) mc_put_fmt(y, 0, "$%c%v", attr,
		repeat_string_f2, ".", Term->wid);
}

static void do_cmd_options_redraw(void)
{
	bool clear;
	int n;
	char c;

	/* Allow "normal" screen update for now. */
	character_icky = FALSE;

	/* Add a special resize hook. */
	add_resize_hook(resize_inkey);

	for (c = KTRL('R'), n = 0, clear = FALSE; c != ESCAPE; c = inkey())
	{
		int inc = isupper(c) ? -1 : 1;
		co_ord *co_ptr = screen_coords+n;

		switch (c)
		{
			case KTRL('R'): case RESIZE_INKEY_KEY: case ESCAPE:
			{
				break;
			}
			case 'n': case 'N':
			{
				n += inc;
				n += N_ELEMENTS(screen_coords);
				n %= N_ELEMENTS(screen_coords);
				co_ptr = screen_coords+n;
				break;
			}
			case 'x': case 'X':
			{
				co_ptr->x += inc;
				break;
			}
			case 'y': case 'Y':
			{
				co_ptr->y += inc;
				break;
			}
			case '\t':
			{
				clear = !clear;
				break;
			}
			case '?':
			{
				/* Hack - show help on the main term. */
				display_help(NULL);
				break;
			}
			default:
			{
				bell(0);
			}
		}

		if (clear)
		{
			/* Clear screen */
			clear_from(0);
		}
		else
		{
			/* Fill the screen with dots. */
			fill_screen('D');
		}
		

		/* Redraw almost everything. */
		p_ptr->redraw |= PR_ALL & ~(PR_MAP | PR_WIPE);
		redraw_stuff();

		put_str("Place status messages", 2, COL_END+2);

		mc_put_fmt(5, COL_END+2, "Number = %2d, Name = %s", n, co_ptr->name);

		mc_put_fmt(7, COL_END+2, "x co-ord: %d, y co-ord: %d",
			co_ptr->x, co_ptr->y);

		put_str("Command (n/N/x/X/y/Y/Tab/?):", 10, COL_END+2);
	}

	/* Remove the resize hook. */
	delete_resize_hook(resize_inkey);

	/* Return to "icky" option menus. */
	character_icky = TRUE;
}

/*
 * Set the base delay factor.
 */
static void do_cmd_options_delay(void)
{
	/* Prompt */
	prt("Command: Base Delay Factor", 18, 0);

	/* Get a new value */
	while (1)
	{
		int k, msec = delay_factor * delay_factor * delay_factor;
		prt(format("Current base delay factor: %d (%d msec)",
		           delay_factor, msec), 22, 0);
		prt("Delay Factor (0-9 or ESC to accept): ", 20, 0);
		k = inkey();
		if (k == ESCAPE) break;
		if (isdigit(k)) delay_factor = D2I(k);
		else bell(0);
		}
	}

/*
 * Set the HP warning level.
 */
static void do_cmd_options_hp(void)
{
	/* Prompt */
	prt("Command: Hitpoint Warning", 18, 0);

	/* Get a new value */
	while (1)
	{
		int k;
		prt(format("Current hitpoint warning: %d0%%",
			hitpoint_warn), 22, 0);
		prt("Hitpoint Warning (0-9 or ESC to accept): ", 20, 0);
		k = inkey();
		if (k == ESCAPE) break;
		if (isdigit(k)) hitpoint_warn = D2I(k);
		else bell(0);
	}
}

/*
 * Dump a version string to an open file.
 */
void dump_version(FILE *fff)
{
	int i;

	assert(fff); /* Caller. */

	/* Header. */
	fprintf(fff, "\n# Current version\n");

	/* Start the line. */
	fprintf(fff, "O");

	/* Dump the version. */
	for (i = 0; i < MAX_SF_VAR; i++) fprintf(fff, ":%u", sf_flags_now[i]);

	/* Finish the line. */
	fprintf(fff, "\n");
}

/*
 * Actually dump options to an open file.
 *
 * This does not dump cheat options on the assumption that this is desired.
 * It does not dump autosave frequency, base delay or hitpoint warning level
 * as the game has no way of loading these.
 */
static errr option_dump_aux(cptr fname)
{
	const co_ord *co_ptr;
	uint i,j;

	FILE *fff = my_fopen_path(ANGBAND_DIR_USER, fname, "a");

	if (!fff) return -1;

	/* Skip some lines */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic option dump\n\n");

	for (i = 0; option_info[i].o_var; i++)
	{
		/* Paranoia - require a real option */
		if (!option_info[i].o_text) continue;

		/* Require a non-cheat option. */
		if (option_info[i].o_page == OPTS_CHEAT) continue;

		/* Comment */
		fprintf(fff, "# Option '%s'\n", option_info[i].o_desc);

		/* Dump the option */
		if ((*option_info[i].o_var))
		{
			fprintf(fff, "Y:%s\n\n", option_info[i].o_text);
		}
		else
		{
			fprintf(fff, "X:%s\n\n", option_info[i].o_text);
		}
	}

	/* Dump window flags */

	fprintf(fff, "# Reset window flags\n");
	fprintf(fff, "W:---reset---\n");

	for (i = 1; i < N_ELEMENTS(windows); i++)
	{
		/* Comment */
		fprintf(fff, "\n#Window '%s'\n", windows[i].name);

		for (j = 0; j < NUM_DISPLAY_FUNCS; j++)
		{
			cptr goodpri = ".abcdefghij";

			/* Not set. */
			if (!windows[i].rep[j] && !windows[i].pri[j]) continue;

			/* Dump the values. */
			fprintf(fff, "W:%s:%s:%c:%c\n", windows[i].name,
				display_func[j].name, goodpri[windows[i].rep[j]],
				goodpri[windows[i].pri[j]]);
		}
	}

	/* Dump redraw flags */

	fprintf(fff, "\n\n# Status description locations\n\n");
	for (co_ptr = screen_coords; co_ptr < END_PTR(screen_coords); co_ptr++)
	{
		fprintf(fff, "L:%s:%d:%d\n", co_ptr->name, co_ptr->x, co_ptr->y);
	}

	/* Dump the current version, as squelch settings use k_idx indices. */
	dump_version(fff);

	/* Save the squelch settings. */
	squelch_dump(fff);

	/* Mention options which can't be read. */
	fprintf(fff, "\n\n# Unparsable options\n\n");
	fprintf(fff, "# Base delay factor %d\n",
		delay_factor * delay_factor * delay_factor);
	fprintf(fff, "# Hitpoint warning %d\n", hitpoint_warn * 10);
	fprintf(fff, "# Autosave frequency %d\n\n", autosave_freq);

	fclose(fff);

	return SUCCESS;
}

#define DCO_ERROR_ABORT	1
#define DCO_ERROR_FILE	2

/*
 * Dump the options to a file.
 */
static errr option_dump(void)
{
	char ftmp[256];

	/* Prompt */
	prt("File: ", 21, 0);

	/* Default filename */
	sprintf(ftmp, "%.251s.prf", player_base);

	/* Handle abort. */
	if (!askfor_aux(ftmp, 256)) return DCO_ERROR_ABORT;

	if (option_dump_aux(ftmp)) return DCO_ERROR_FILE;

	return SUCCESS;
}


/*
 * Load the contents of a preference file.
 */
static errr option_load(void)
{
	char ftmp[256];

	prt("File: ", 21, 0);

	/* Default filename */
	sprintf(ftmp, "%.251s.prf", player_base);

	/* Handle abort. */
	if (!askfor_aux(ftmp, 256)) return DCO_ERROR_ABORT;

	/* Try to process the named file. */
	if (process_pref_file(ftmp)) return DCO_ERROR_FILE;

	return SUCCESS;
}

/*
 * Give some feedback for one of the above functions.
 * Note that a user abort passes unmentioned.
 */
static void dco_feedback(errr err)
{
	switch (err)
	{
		case DCO_ERROR_FILE:
		{
			msg_print("Failed!");
			break;
		}
		case SUCCESS:
		{
			msg_print("Done.");
			break;
		}
	}
}

typedef struct option_list option_list;
struct option_list
{
	cptr title;
	cptr text;
	int num;
	char ch;
	byte x;
	byte y;
};

static option_list opt_lists[] =
{
	{"User Interface Options", NULL, OPTS_UI, '1', 5, 4},
	{"Disturbance Options", NULL, OPTS_DISTURB, '2', 5, 5},
	{"Creature Options", NULL, OPTS_MON, '3', 5, 6},
	{"Object Options", NULL, OPTS_OBJ, '4', 5, 7},
	{"Performance Options", NULL, OPTS_PERF, '5', 5, 8},
	{"Miscellaneous Options", NULL, OPTS_MISC, '6', 5, 9},
	{"Birth Options", NULL, OPTS_BIRTH, 'B', 5, 11},
	{"Spoiler Options", "spoiler.txt", OPTS_SPOIL, 'S', 5, 12},
	{"Cheating Options", "o_cheat.txt", OPTS_CHEAT, 'C', 5, 13},
	{"Help", NULL, OPTS_HELP, '?', 5, 15},
	{"Autosave Options", NULL, OPTS_SAVE, 'A', 5, 16},
	{"Base Delay Factor", NULL, OPTS_DELAY, 'D', 43, 4},
	{"Hitpoint Warning", NULL, OPTS_HP, 'H', 43, 5},
	{"Window Options", NULL, OPTS_WINDOW, 'W', 43, 6},
	{"Redraw Options", NULL, OPTS_REDRAW, 'R', 43, 7},
	{"Interact with Macros", NULL, OPTS_MACRO, 'M', 43, 8},
	{"Interact with Visuals", NULL, OPTS_VISUAL, 'V', 43, 9},
	{"Interact with Colours", NULL, OPTS_COLOUR, 'K', 43, 10},
	{"Squelch Settings", NULL, OPTS_SQUELCH, 'Q', 43, 11},
	{"Save options", NULL, OPTS_TO_FILE, 'U', 43, 13},
	{"Save all preferences", NULL, OPTS_ALL_TO_FILE, 'P', 43, 14},
	{"Load preferences", NULL, OPTS_FROM_FILE, 'O', 43, 15},
};


/* Forward declare. */
static errr preference_dump(void);
static void do_cmd_macros(void);
static void do_cmd_visuals(void);
static void do_cmd_colors(void);

/*
 * Set or unset various options.
 *
 * The user must use the "Ctrl-R" command to "adapt" to changes
 * in any options which control "visual" aspects of the game.
 */
void do_cmd_options(void)
{
	int k, t;

	option_list *ol_ptr;

	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	t = Term_save_aux();

	/* Display the general options help */
	help_track("option_prompt");

	/* Interact */
	while (1)
	{
		char buf[] = "option= ";

		/* Clear screen */
		Term_clear();

		/* Why are we here */
		prt("Game Options", 2, 0);

		/* Give some choices */
		for (ol_ptr = opt_lists; ol_ptr < END_PTR(opt_lists); ol_ptr++)
		{
			prt(format("(%c) %s", ol_ptr->ch, ol_ptr->title), ol_ptr->y,
				ol_ptr->x);
		}

		/* Prompt */
		prt("Command: ", 18, 0);

		/* Get command */
		k = inkey();

		/* Exit */
		if (k == ESCAPE) break;

		/* Analyze */
		for (ol_ptr = opt_lists; ol_ptr < END_PTR(opt_lists); ol_ptr++)
		{
			if (FORCEUPPER(k) == ol_ptr->ch) goto good;
		}

		/* Failure. */
		bell(0);
		continue;

good:	/* Success */

		/* Track the help for this sub-menu (as e.g. option=1). */
		strchr(buf, '\0')[-1] = ol_ptr->ch;
		help_track(buf);

		/* Give a message (usually cleared immediately). */
		roff(ol_ptr->title);

		if (ol_ptr->num > -1)
		{
			do_cmd_options_aux(ol_ptr->num, ol_ptr->title, ol_ptr->text);
		}
		else switch (ol_ptr->num)
		{
			case OPTS_DELAY:
			{
				do_cmd_options_delay();
				break;
			}
			case OPTS_HP:
			{
				do_cmd_options_hp();
				break;
			}
			case OPTS_SAVE:
			{
				do_cmd_options_autosave("Autosave");
				break;
			}
			case OPTS_WINDOW:
			{
				do_cmd_options_win();
				break;
			}
			case OPTS_REDRAW:
			{
				do_cmd_options_redraw();
				break;
			}
			case OPTS_TO_FILE:
			{
				dco_feedback(option_dump());
				break;
			}
			case OPTS_ALL_TO_FILE:
			{
				dco_feedback(preference_dump());
				break;
			}
			case OPTS_FROM_FILE:
			{
				dco_feedback(option_load());
				break;
			}
			case OPTS_MACRO:
			{
				do_cmd_macros();
				break;
			}
			case OPTS_VISUAL:
			{
				do_cmd_visuals();
				break;
			}
			case OPTS_COLOUR:
			{
				do_cmd_colors();
				break;
			}
			case OPTS_SQUELCH:
			{
				do_cmd_options_squelch();
				break;
			}
			case OPTS_HELP:
			{
				do_cmd_help("custom.txt");
			}
		}

		/* Flush messages */
		msg_print(NULL);

		/* Flush help */
		help_track(NULL);
	}

	/* Remove the help */
	help_track(NULL);

	/* Restore the screen */
	Term_load_aux(t);

	/* Forget the restored screen. */
	Term_release(t);

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
	cptr err;
	u16b sf_flags[MAX_SF_VAR];
	current_flags(sf_flags);

	/* Default */
	strcpy(buf, "");

	/* Ask for a "user pref command" */
	if (!get_string("Pref: ", buf, 80)) return;

	/* Remember the request. */
	message_add(buf);

	/* Process that pref command */
	if ((err = process_pref_file_aux(buf, sf_flags)))
	{
		bell(err);
	}
}


#ifdef ALLOW_MACROS

/*
 * Hack -- append all current macros to the given file
 */
static errr macro_dump(cptr fname)
{
	int i;

	FILE *fff;


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen_path(ANGBAND_DIR_USER, fname, "a");

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

		/* Dump the action. */
		my_fprintf(fff, "A:%v\n", ascii_to_text_f1, macro__act[i]);

		/* Dump the trigger. */
		my_fprintf(fff, "P:%v\n\n", ascii_to_text_f1, macro__pat[i]);
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
static void do_cmd_macro_aux(char *buf, int len)
{
	int i, n = 0;

	/* Flush */
	flush();

	/* Do not process macros */
	inkey_base = TRUE;

	/* First key */
	i = inkey();

	/* Read the pattern */
	while (i && n < len-1)
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
 
 
	/* Hack -- display the trigger */
	Term_addstr(-1, TERM_WHITE, format("%v", ascii_to_text_f1, buf));
}



/*
 * Hack -- ask for a keymap "trigger" (see below)
 *
 * Note that both "flush()" calls are extremely important.  This may
 * no longer be true, since "util.c" is much simpler now.  XXX XXX XXX
 */
static void do_cmd_macro_aux_keymap(char *buf)
{
	cptr tmp;
	/* Flush */
	flush();

	/* Get a key */
	buf[0] = inkey();
	buf[1] = '\0';

	tmp = format("%v", ascii_to_text_f1, buf);

	/* Hack -- display the trigger */
	Term_addstr(-1, TERM_WHITE, tmp);
	
	/* Notice if the key isn't its own text representation. */
	if (strcmp(tmp, buf))
	{
		/* buf[0] could be anything, so output the character alone. */
		Term_addstr(-1, TERM_WHITE, " (");
		Term_addch(TERM_WHITE, buf[0]);
		Term_addch(TERM_WHITE, ')');
	}
				
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

	int mode = keymap_mode();


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen_path(ANGBAND_DIR_USER, fname, "a");

	/* Failure */
	if (!fff) return (-1);


	/* Skip space */
	fprintf(fff, "\n\n");

	/* Start dumping */
	fprintf(fff, "# Automatic keymap dump\n\n");

	/* Dump ---reset--- so that no extras are carried forward. */
	fprintf(fff, "C:---reset---\n");

	/* Dump them */
	for (i = 0; i < 256; i++)
	{
		cptr act;
		char buf[2];

		/* Loop up the keymap */
		act = keymap_act[mode][i];

		/* Skip empty keymaps */
		if (!act) continue;

		/* Encode the key */
		sprintf(buf, "%c", i);

		/* Dump the macro */
		my_fprintf(fff, "A:%v\nC:%d:%v\n", ascii_to_text_f1, act,
			mode, ascii_to_text_f1, buf);
	}

	/* Start dumping */
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
static void do_cmd_macros(void)
{
	int i;

	char tmp[1024];

	char buf[1024];

	int mode = keymap_mode();


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


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
		strnfmt(buf, sizeof(buf), "%v", ascii_to_text_f1, macro__buf);

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
				msg_print(format("Could not load file '%s'!", tmp));
			}
			else
{
				/* Prompt */
				msg_print(format("Loaded '%s'.", tmp));
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
			do_cmd_macro_aux(buf, sizeof(buf));

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
				strnfmt(buf, sizeof(buf), "%v", ascii_to_text_f1, macro__buf);

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
			do_cmd_macro_aux(buf, sizeof(buf));

			/* Clear */
			clear_from(20);

			/* Prompt */
			prt("Action: ", 20, 0);

			/* Convert to text */
			strnfmt(tmp, sizeof(tmp), "%v", ascii_to_text_f1, macro__buf);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
			{
				/* Convert to ascii */
				strnfmt(macro__buf, 1024, "%v", text_to_ascii_f1, tmp);

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
			do_cmd_macro_aux(buf, sizeof(buf));

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
				strnfmt(buf, sizeof(buf), "%v", ascii_to_text_f1, macro__buf);
	
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
			strnfmt(tmp, sizeof(tmp), "%v", ascii_to_text_f1, macro__buf);

			/* Get an encoded action */
			if (askfor_aux(tmp, 80))
{
				/* Convert to ascii */
				strnfmt(macro__buf, 1024, "%v", text_to_ascii_f1, tmp);

				/* Free old keymap */
				FREE(keymap_act[mode][(byte)(buf[0])]);

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
			FREE(keymap_act[mode][(byte)(buf[0])]);

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

			/* Get an encoded action */
			if (!askfor_aux(buf, 80)) continue;

			/* Extract an action */
			strnfmt(macro__buf, 1024, "%v", text_to_ascii_f1, buf);
		}

#endif /* ALLOW_MACROS */

		/* Oops */
		else
		{
			/* Oops */
			bell(0);
		}

		/* Flush messages */
		msg_print(NULL);
	}
}

#ifdef ALLOW_VISUALS

/*
 * Define a structure to store everything which distinguishes the way the
 * do_cmd_visuals() function deals with one type of thing with the way it
 * deals with another.
 */

typedef struct visual_type visual_type;

struct visual_type
{
	/* A text string which describes the type of preference. */
	cptr text;

	/*
	 * Get function.
	 * Obtain the name, the default character and colour, and pointers to the
	 * user-defined character and colour for R/W access.
	 */
	void (*get)(int, cptr *, byte *, char *, byte **, char **);

	/* 
	 * Dump functions.
	 * If pref_str is set, this determines the string dump_visual_stuff() uses
	 * for each pref string.
	 * If not, it runs dump once.
	 */
	void (*dump)(FILE *fff);
	void (*pref_str)(FILE *, int, char, byte, char);

	/*
	 * Reject function.
	 * If this is set, entries which this returns true for cannot be selected.
	 * If not, every legal entry can be selected.
	 */
	bool (*reject)(uint n);

	/* The text string printed before an option dump. */
	cptr initstring;

	/* The number of entries available (usually set during initialisation). */
	u16b max;

	/* The character which starts every preference line to indicate the type. */
	char startchar; 

	/* Indicate that colour has meaning for these visual prefs. */
	bool attr;

	/* Indicate that character has meaning for these visual prefs. */
	bool chars;
};

/*
 * Extract pointers to the following elements of something:
 * (cptr)(x_name+x_info[i].name)
 * (byte)(x_info[i].d_attr)
 * (char)(x_info[i].d_char)
 * (byte*)(&x_info[i].x_attr)
 * (char*)(&x_info[i].x_char)
 */
#define get_visuals(x_info) \
	((*da) = x_info[i].d_attr, \
	(*dc) = x_info[i].d_char, \
	(*xa) = &(x_info[i].x_attr), \
	(*xc) = &(x_info[i].x_char))



/*
 * The strings in f_name are processed before they are printed.
 */
static void get_visuals_feat(int i, cptr *name, byte *da, char *dc, byte **xa, char **xc)
{
	get_visuals(f_info);
	*name = format("%v", feature_desc_f2, i, 0);
}

/*
 * The strings in r_name are processed before they are printed.
 */
static void get_visuals_mon(int i, cptr *name, byte *da, char *dc,
	byte **xa, char **xc)
{
	get_visuals(r_info);
	*name = format("%v", monster_desc_aux_f3, r_info+i, 1, 0);
}

/*
 * The strings in k_name are processed before they are printed.
 */
static void get_visuals_obj(int i, cptr *name, byte *da, char *dc,
	byte **xa, char **xc)
{
	/* Get most of the visuals. */
	get_visuals(k_info);

	/* Place the name in a temporary buffer. */
	(*name) = format("%v", object_k_name_f1, i);
}

/*
 * Monster colours are special. They have no notion of character, and
 * the things they do have are stored in a different way to reflect the
 * fact that the defaults are set at compile-time rather than run-time.
 */
static void get_visuals_moncol(int i, cptr *name, byte *da, char UNUSED *dc,
	byte **xa, char UNUSED **xc)
{
	(*name) = moncol[i].name;
	(*da) = TERM_WHITE;
	(*xa) = &(moncol[i].attr);
}

/*
 * The strings in u_name are processed before they are printed (using a fake
 * object kind to allow object_desc() to be used).
 */
static void get_visuals_unident(int i, cptr *name, byte *da, char *dc,
	byte **xa, char **xc)
{
	/* Set everything up. k_info[1] is arbitrary, but it certainly exists. */
	object_kind hack_k, *k_ptr = &k_info[1];
	object_type o_ptr[1];

	/* Get most of the visuals (including a mangled name). */
	get_visuals(u_info);

	/* Put *k_ptr somewhere safe and clear it. */
	COPY(&hack_k, k_ptr, object_kind);
	WIPE(k_ptr, object_kind);

	/* Set up *k_ptr and *o_ptr. The tval is arbitrary. */
	k_ptr->tval = TV_FOOD;
	k_ptr->cost = 1;
	k_ptr->u_idx = i;
	object_prep(o_ptr, k_ptr-k_info);

	/* Place the name in a temporary buffer. */
	(*name) = format("%v", object_desc_f3, o_ptr, FALSE, 0);

	/* Replace the real *k_ptr */
	COPY(k_ptr, &hack_k, object_kind);
}

/*
 * Dump a normal visual pref string.
 */
static void pref_str_std(FILE *fff, int i, char startchar, byte a, char c)
{
	fprintf(fff, "%c:%d:0x%02X:0x%02X\n\n", startchar, i, a, (byte)c);
}

/*
 * Dump the visual pref string for an unidentified item.
 */
static void pref_str_unident(FILE *fff, int i, char startchar, byte a, char c)
{
	unident_type *u_ptr = u_info+i;

	fprintf(fff, "%c:%d:%d:0x%02X:0x%02X\n\n", startchar,
	u_ptr->p_id, u_ptr->s_id, a, (byte)c);
}

/*
 * Monster description colours are dumped in a special format because
 * of the low amount of information they use.
 */
static void visual_dump_moncol(FILE *fff)
{
	s16b n;
	char out[80] = "M";

	for (n = 0; n < MAX_MONCOL; n++)
	{
		moncol_type *mc_ptr = &moncol[n];
		char c1 = atchar[mc_ptr->attr/16];
		char c2 = atchar[mc_ptr->attr%16];

		/* A leading space looks neater than a leading d.*/
		if (c1 == 'd') c1 = ' ';
		strcat(out, format(":%c%c", c1, c2));
	}
	/* Start dumping */
	fprintf(fff, "\n\n");
	fprintf(fff, "# Monster memory attr definitions\n\n");
	fprintf(fff, out);
	fprintf(fff, "\n\n\n\n");
}


static bool visual_reject_feat(uint n)
{
	return (f_info[n].mimic != n);
}

/*
 * Non-objects are rejected, as are unseen objects other than (nothing).
 */
static bool visual_reject_obj(uint n)
{
	return (k_info[n].name == 0 || (n && !spoil_base && !k_info[n].seen));
}

/* The number of members of the visual[] array. */
#define VISUALS N_ELEMENTS(visual)

/* The line on which the command prompt appears. */
#define CMDLINE (7+2*VISUALS)

	/* Enter in the data for the various types of thing being altered. */
static visual_type visual[5] =
{
	{"monster attr/chars", get_visuals_mon, 0, pref_str_std,
		0, "Monster attr/char definitions", 0, 'R', TRUE, TRUE},
	{"object attr/chars", get_visuals_obj, 0, pref_str_std,
		visual_reject_obj, "Object attr/char definitions", 0, 'K', TRUE, TRUE},
	{"feature attr/chars", get_visuals_feat, 0, pref_str_std,
		visual_reject_feat, "Feature attr/char definitions", 0, 'F', TRUE, TRUE},
	{"monster memory attrs", get_visuals_moncol, visual_dump_moncol, 0,
		0, "Monster memory attr definitions", 0, 'M', TRUE, FALSE},
	{"unidentified object attr/chars", get_visuals_unident, 0, pref_str_unident,
		0, "Unidentified object attr/char definitions", 0, 'U', TRUE, TRUE},
};

static errr dump_visuals_aux(cptr tmp, visual_type *vs_ptr)
{
	int i;
	FILE *fff;

	/* Drop priv's */
	safe_setuid_drop();

	/* Append to the file */
	fff = my_fopen_path(ANGBAND_DIR_USER, tmp, "a");

	/* Grab priv's */
	safe_setuid_grab();

	/* Failure */
	if (!fff) return -1;

	/* This requires a normal pref file output. */
	if (vs_ptr->pref_str)
	{
		/* Start dumping */
		fprintf(fff, "\n\n");
		fprintf(fff, "# %s\n", vs_ptr->initstring);

		/* Print the version. */
		dump_version(fff);

		fprintf(fff, "\n%c:---reset---\n\n", vs_ptr->startchar);

		/* Dump entries */
		for (i = 0; i < vs_ptr->max; i++)
		{
			cptr name;
			byte da, *xa;
			char dc, *xc;
			(*vs_ptr->get)(i, &name, &da, &dc, &xa, &xc);

			/* Skip non-entries */
			if (vs_ptr->reject && (*vs_ptr->reject)(i)) continue;

			/* Skip default entries */
			if ((*xa == da) && (*xc == dc)) continue;

			/* Dump a comment */
			fprintf(fff, "# %s\n", name);

			/* Dump the attr/char info */
			(*vs_ptr->pref_str)(fff, i, vs_ptr->startchar, *xa, *xc);
		}

		/* All done */
		fprintf(fff, "\n\n\n\n");
	}
	/* An arbitrary dump function. */
	else
	{
		(*(vs_ptr->dump))(fff);
	}

	/* Close */
	my_fclose(fff);

	return SUCCESS;
}

static void dump_visuals(visual_type *vs_ptr)
{
	char tmp[71];

	/* Prompt */
	prt(format("Command: Dump %s", vs_ptr->text), CMDLINE, 0);

	/* Prompt */
	prt("File: ", CMDLINE+2, 0);

	/* Default filename */
	sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

	/* Get a filename */
	if (!askfor_aux(tmp, sizeof(tmp)-1)) return;

	if (dump_visuals_aux(tmp, vs_ptr))
	{
		msg_format("Failed to dump %s.", vs_ptr->text);
	}
	else
	{
		/* Message */
		msg_format("Dumped %s.", vs_ptr->text);
	}
}

static void modify_visuals(visual_type *vs_ptr)
{
	char i;
	int inc, max, num;
	uint r = vs_ptr->max-1, *out;

	/* Make a note of log(*vs_ptr->max-1)/log(10) */
	const uint numlen = strlen(format("%d", r));

	prt(format("Command: Change %s", vs_ptr->text), CMDLINE, 0);

	/* Start by finding the first valid selection. */
	Term_key_push('n');

	/* Hack -- query until done */
	while (1)
	{
		char dcl, *ccl;
		byte dal, *cal;

		uint da, dc, ca, cc = 50;
		cptr prompt, text;

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Hack - allow control codes to be entered separately. */
		if (i == '^')
		{
			if (get_com("Control: ", &i)) i = KTRL(i);
			else continue;
		}

		/* Split i into base character and modifier. */
		inc = (iscntrl(i)) ? 0 : (islower(i)) ? 1 : -1;
		i = 'a'+i-(iscntrl(i) ? KTRL('A') : (islower(i)) ? 'a' : 'A');

		switch (i)
		{
			case 'n':
				prompt = "Select number of desired entry: ";
				out = &r;
				max = vs_ptr->max;
				break;
			case 'a':
				if (!vs_ptr->attr) goto err;
				prompt = "Select number of desired colour: ";
				out = &ca;
				max = 256;
				break;
			case 'c':
				if (!vs_ptr->chars) goto err;
				prompt = "Select number of desired character: ";
				out = &cc;
				max = 256;
				break;
			default:
err:			/* Not a valid response, so ask again. */
				bell(0);
				continue;
		}

		/* Analyze */
		if (inc == 0)
		{
			char str[5] = "";

			inc = 0;

			if (get_string(prompt, str, sizeof(str)-1))
			{
				num = strtol(str, NULL, 0);

				/* Number out of bounds. */
				if (num < 0 || num >= max) continue;

				/* Invalid number in bounds. */
				if (i == 'n' && vs_ptr->reject &&
					(*vs_ptr->reject)(num)) continue;

				/* Everything as it should be. */
				(*out) = num;
			}
		}
		else
		{
			/* Handle rejection criteria if necessary. */
			do
			{
				(*out) = ((*out) + max + inc) % max;
			}
			while (i == 'n' && vs_ptr->reject && (*vs_ptr->reject)(r));
		}
		/* Save the information if it has changed.*/
		if (i == 'a' && cal) (*cal) = ca;
		if (i == 'c' && ccl) (*ccl) = cc;

		(*vs_ptr->get)(r, &text, &dal, &dcl, &cal, &ccl);

		da = dal;
		dc = (byte)dcl;
		if (cal) ca = *cal;
		if (ccl) cc = (byte)*ccl;

		/* Clear the line first. */
		Term_erase(0, CMDLINE+2, Term->wid);

		/* Write name first as it may be in the format buffer. */
		put_str(text, CMDLINE+2, 5+numlen+strlen("Number = , Name = "));

		/* Label the object */
		put_str(format("Number = %*d, Name = ", numlen, r),
			CMDLINE+2, 5);

		/* Display the default/current attr/char. */
		mc_put_fmt(CMDLINE+4, 40, "<< $%c%c $w>>", (vs_ptr->attr) ? atchar[da] : 'w', (vs_ptr->chars) ? dc : '#');
		mc_put_fmt(CMDLINE+5, 40, "<< $%c%c $w>>", (vs_ptr->attr) ? atchar[ca] : 'w', (vs_ptr->chars) ? cc : '#');

		if (vs_ptr->attr && vs_ptr->chars)
		{
			mc_put_fmt(CMDLINE+4, 10, "Default attr/char = %3u / %3u", da, dc);
			mc_put_fmt(CMDLINE+5, 10, "Current attr/char = %3u / %3u", ca, cc);
		}
		else if (vs_ptr->attr)
		{
			mc_put_fmt(CMDLINE+4, 10, "Default attr = %3u", da);
			mc_put_fmt(CMDLINE+5, 10, "Current attr = %3u", ca);
		}
		else if (vs_ptr->chars)
		{
			mc_put_fmt(CMDLINE+4, 10, "Default char = %3u", dc);
			mc_put_fmt(CMDLINE+5, 10, "Current char = %3u", cc);
		}
		else
		{
			mc_put_fmt(CMDLINE+4, 10, "A white #");
			mc_put_fmt(CMDLINE+5, 10, "A white #");
		}

		/* Prompt */
		mc_put_fmt(CMDLINE+7, 0, "Command (n/N%s%s): ", 
			(vs_ptr->attr) ? "/a/A" : "", (vs_ptr->chars) ? "/c/C" : "");
	}
}

#endif /* ALLOW_VISUALS */


/*
 * Interact with "visuals"
 */
static void do_cmd_visuals(void)
{
#ifdef ALLOW_VISUALS

#else /* ALLOW_VISUALS */

#define VISUALS 0

#endif /* ALLOW_VISUALS */

	uint i;

	/* Enter the maxima separately, as they are determined at run-time. */
	visual[0].max = MAX_R_IDX;
	visual[1].max = MAX_K_IDX;
	visual[2].max = MAX_F_IDX;
	visual[3].max = MAX_MONCOL;
	visual[4].max = MAX_U_IDX;

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Interact until done */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Interact with Visuals", 2, 0);

		prt("(a) Load a user pref file", 4, 5);
#ifdef ALLOW_VISUALS
		/* Give some choices */
		for (i = 0; i < VISUALS; i++)
		{
			visual_type *vs_ptr = &visual[i];
			prt(format("(%c) Dump %s", 'b'+i, vs_ptr->text), 5+i, 5);
			prt(format("(%c) Change %s", 'b'+i+VISUALS, vs_ptr->text), 5+i+VISUALS, 5);
		}
#endif
		prt(format("(%c) Reset visuals", 'b'+2*VISUALS), 5+2*VISUALS, 5);

		/* Prompt */
		prt("Command: ", CMDLINE, 0);

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

		/* Load a 'pref' file */
		else if (i == 'a')
		{
			char tmp[71];

			/* Prompt */
			prt("Command: Load a user pref file", CMDLINE, 0);

			/* Prompt */
			prt("File: ", CMDLINE+2, 0);

			/* Default filename */
			sprintf(tmp, "user-%s.prf", ANGBAND_SYS);

			/* Query */
			if (!askfor_aux(tmp, sizeof(tmp)-1)) continue;

			/* Process the given filename */
			(void)process_pref_file(tmp);
		}

#ifdef ALLOW_VISUALS

		/* Dump a visual file. */
		else if (i > 'a' && i < 'b'+VISUALS)
		{
			dump_visuals(visual+i-'b');
		}

		/* Modify a visual function. */
		else if (i > 'a'+VISUALS && i < 'b'+2*VISUALS)
		{
			modify_visuals(visual+i-'b'-VISUALS);
		}
#endif

		/* Reset visuals */
		else if (i == 'b'+2*VISUALS)
		{
			/* Reset */
			reset_visuals();

			/* Message */
			msg_print("Visual attr/char tables reset.");
		}

		/* Unknown option */
		else
		{
			bell(0);
		}

		/* Flush messages */
		msg_print(NULL);
	}
}

#ifdef ALLOW_COLORS
/*
 * Dump colour definitions to a named file.
 */
static errr dump_colours_aux(cptr file)
{
	FILE *fff;
	int i;

	/* Drop priv's */
	safe_setuid_drop();

	/* Append to the file */
	fff = my_fopen_path(ANGBAND_DIR_USER, file, "a");

	/* Grab priv's */
	safe_setuid_grab();

	/* Failure */
	if (!fff) return -1;

	/* Start dumping */
	fprintf(fff, "\n\n");
	fprintf(fff, "# Color redefinitions\n\n");

	/* Dump colors */
	for (i = 0; i < 256; i++)
	{
		uint kv = angband_color_table[i][0];
		uint rv = angband_color_table[i][1];
		uint gv = angband_color_table[i][2];
		uint bv = angband_color_table[i][3];

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

	return SUCCESS;
}
#endif /* ALLOW_COLORS */

/*
 * Interact with "colors"
 */
static void do_cmd_colors(void)
{
	int i;

	char tmp[160];


	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


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

			if (dump_colours_aux(tmp)) continue;

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
					Term_putstr(i*4, 20, -1, (byte)a, "###");

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
			bell(0);
		}

		/* Flush messages */
		msg_print(NULL);
	}
}


/*
 * Dump all of the above preferences to a file.
 */
static errr preference_dump(void)
{
	char ftmp[256];
	visual_type *vs_ptr;

	/* Prompt */
	prt("Command: Append options to a file", 21, 0);

	/* Prompt */
	prt("File: ", 21, 0);

	/* Default filename */
	sprintf(ftmp, "%.251s.prf", player_base);

	/* Handle abort. */
	if (!askfor_aux(ftmp, 256)) return DCO_ERROR_ABORT;

	/* Dump options. */
	if (option_dump_aux(ftmp)) return DCO_ERROR_FILE;

#ifdef ALLOW_MACROS
	/* Dump macros. */
	if (macro_dump(ftmp)) return DCO_ERROR_FILE;

	/* Dump keymaps. */
	if (keymap_dump(ftmp)) return DCO_ERROR_FILE;
#endif /* ALLOW_MACROS */

#ifdef ALLOW_VISUALS
	/* Dump visuals. */
	for (vs_ptr = visual; vs_ptr < END_PTR(visual); vs_ptr++)
	{
		if (dump_visuals_aux(ftmp, vs_ptr)) return DCO_ERROR_FILE;
	}
#endif /* ALLOW_VISUALS */

#ifdef ALLOW_COLORS
	/* Dump colours. */
	if (dump_colours_aux(ftmp)) return DCO_ERROR_FILE;
#endif /* ALLOW_COLORS */

	/* Messgage. */
	msg_print("Dumped preferences.");

	return SUCCESS;
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
   /* Silly message */
    msg_format("You are playing %s %s.", GAME_NAME, GAME_VERSION);
	/* These are ANSI standard constants so should work on any compiler */
#ifdef SHOW_COMPILE_TIME
	msg_format("(Compiled %s %s)", __TIME__, __DATE__);
#endif /* SHOW_COMPILE_TIME */
}



/*
 * Array of feeling strings
 */
static cptr do_cmd_feeling_text[11] =
{
	"You're not sure about this level yet.",
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
 * Display the feeling.
 * if ironman_feeling is set, this gives no useful information before the 2500th
 * turn.
 */
void do_cmd_feeling(bool FeelingOnly)
{
	/* Verify the feeling */
	if (feeling < 0) feeling = 0;
	if (feeling > 10) feeling = 10;

	/* No useful feeling in town, but print the town name */
	if (dun_level <= 0)
	{
	/* If you are in a town */
		if(!FeelingOnly)
		{
			if(is_town_p(wildy, wildx))
			{
				msg_format("You are in %s.",town_name+town_defs[cur_town].name);
			}
			else if(wild_grid[wildy][wildx].dungeon < MAX_CAVES)
			{
				msg_format("You are outside %s.",dun_name+dun_defs[wild_grid[wildy][wildx].dungeon].name);
			}
			else
			{
				msg_print("You are wandering around outside.");
			}
		}
		return;
	}
	
	/* You are in a dungeon, so... */
	if(!FeelingOnly)
	{
		msg_format("You are in %s.",dun_name+dun_defs[cur_dungeon].name);

		/* Show quest status */
		if (is_quest(dun_level))
		{
			print_quest_message();
		}
	}

	if (ironman_feeling && turn-old_turn < 2500)
	{
		/* Give an uninformative message if requested. */
		if (!FeelingOnly) msg_print(do_cmd_feeling_text[0]);
	}
	else
	{
		/* Display the feeling */
		msg_print(do_cmd_feeling_text[feeling]);
	}

	if ((cheat_wzrd) || (cheat_skll))
	{
		msg_format("Maximum %d for skills on this level",(dun_depth) * 3 / 2);
	}
}





/*
 * Hack -- load a screen dump from a file
 */
void do_cmd_load_screen(void)
{
	char buf[1024];
	int y, x, w, h;

	FILE *fff;


	/* Append to the file */
	fff = my_fopen_path(ANGBAND_DIR_USER, "dump.txt", "r");

	/* Oops */
	if (!fff) return;


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Clear the screen */
	Term_clear();

	Term_get_size(&w, &h);

	/* Load the screen */
	for (y = 0; y < h && !my_fgets(fff, buf, 1024) && *buf; y++)
	{
		/* Show each row */
		for (x = 0; x < w && buf[2*x]; x++)
		{
			byte a = (buf[2*x]-' ')%0x10;
			unsigned char c = (buf[2*x+1]-' ')+(((buf[2*x]-' ')/0x10)*0x40);

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
static void (*screendump_aux)(void) = NULL;






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

		byte a;
		unsigned char c;
		int w,h;

		FILE *fff;

		char buf[1024];


		/* File type is "TEXT" */
		FILE_TYPE(FILE_TYPE_TEXT);

		/* Hack -- drop permissions */
		safe_setuid_drop();

		/* Append to the file */
		fff = my_fopen_path(ANGBAND_DIR_USER, "dump.txt", "w");

		/* Hack -- grab permissions */
		safe_setuid_grab();

		/* Oops */
		if (!fff) return;

		Term_get_size(&w, &h);

		/* Dump the screen (reloadable version) */
		for (y = 0; y < h; y++)
		{
			/* Dump each row */
			for (x = 0; x < w; x++)
			{
				/* Get the attr/char */
				(void)(Term_what(x, y, &a, (char*)&c));

				/* c can take all sorts of strange values, so dump an
				 * abstract representation using characters in the range
				 * 32-95 only.
				 */

				/* Dump them */
				buf[2*x] = (a%0x10)+(c/0x40)*0x10+' ';
				buf[2*x+1] = (c%0x40)+' ';
			}

			/* Terminate */
			buf[2*x] = '\0';

			/* End the row */
			fprintf(fff, "%s\n", buf);
		}

		/* Skip a line */
		fprintf(fff, "\n");

		/* Dump the screen (original version, requires "printable" characters) */
		for (y = 0; y < 24; y++)
		{
			/* Dump each row */
			for (x = 0; x < 79; x++)
			{
				/* Get the attr/char */
				(void)(Term_what(x, y, &a, (char*)&c));

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
				char hack[17] = "dwsorgbuDWvyRGBU";

				/* Get the attr/char */
				(void)(Term_what(x, y, &a, (char*)&c));

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
		msg_print(NULL);
	}
}


/*
 * A simple function to include a coloured symbol in a text file for
 * show_file().
 *
 * Format:
 * "%v", get_symbol_f2, (byte)at, (char)ch
 */
void get_symbol_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	uint at = va_arg(*vp, uint);
	int ch = va_arg(*vp, int);

	/* Paranoia - max is not long enough. */
	if (max <= strlen("$c$$w")+2) return;

	/* Check formatting. */
	if (at != (byte)at || ch != (char)ch)
	{
		strcpy(buf, "(error)");
	}
	/* Double $ to print it if necessary. */
	else if (ch == '$')
	{
		sprintf(buf, "$%c$$$w", atchar[at]);
	}
	/* Normal string. */
	else
	{
		sprintf(buf, "$%c%c$w", atchar[at], ch);
	}
}

/*
 * A wrapper to find an appropriate attr/char combination from the *_info
 * array. The isprint() checks are necessary as show_file() reacts badly to
 * unprintable characters.
 */
#define get_symbol(x_ptr) \
	get_symbol_f2, (x_ptr)->x_attr, \
		isprint((x_ptr)->x_char) ? (x_ptr)->x_char : \
		isprint((x_ptr)->d_char) ? (x_ptr)->d_char : '#'

/*
 * Check the status of "artifacts"
 */
static void do_cmd_knowledge_artifacts(void)
{
	char file_name[1024];

	/* Open a new file */
	FILE *fff = my_fopen_temp(file_name, 1024);
	
	if (fff)
	{
	int i, k, x, y;

	/* Allocate the "okay" array */
	C_TNEW(okay, MAX_A_IDX, bool);

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
			cave_type *c_ptr = &cave[y][x];

			s16b this_o_idx, next_o_idx = 0;

			/* Scan all objects in the grid */
			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
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
		object_type q_ptr[1];

		/* List "dead" ones */
		if (!okay[k]) continue;

		/* Skip "empty" ones */
		if (!make_fake_artifact(q_ptr, k)) continue;

		/* Hack -- Build the artifact name */
		my_fprintf(fff, " %v   %v\n", get_symbol(&k_info[a_ptr->k_idx]),
			object_desc_f3, q_ptr, OD_ART | OD_SHOP, 0);
	}

	/* Free the "okay" array */
	TFREE(okay);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Artifacts Seen");

	/* Remove the file */
	fd_kill(file_name);
	}
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

	/* Open a new file */
	if (!((fff = my_fopen_temp(file_name, 1024)))) return;


	/* Scan the monster races */
	for (k = 1; k < MAX_R_IDX; k++)
	{
		monster_race *r_ptr = &r_info[k];

		/* Skip "fake" monsters. */
		if (is_fake_monster(r_ptr)) continue;

		/* Only print Uniques */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			bool dead = (r_ptr->max_num == 0);
			quest_type *q_ptr = cnv_monster_to_quest(r_ptr);
			char quest = (q_ptr && q_ptr->known) ? '!' : ' ';

			/* Only display "known" uniques */
			if (dead || spoil_mon || r_ptr->r_sights)
			{
				my_fprintf(fff, " %v %c $%c%v is %s\n", get_symbol(r_ptr),
					quest,	(dead) ? 'D' : 'w', monster_desc_aux_f3,
					r_ptr, 1, MDF_DEF, (dead) ? "dead" : "alive");
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
	int max_friends = 0;
	int show_upkeep = 0;
	int upkeep_divider = 20;

	char file_name[1024];


	/* Open a new file */
	if (!((fff = my_fopen_temp(file_name, 1024)))) return;

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for friendly monsters */
		if (m_ptr->smart & (SM_ALLY))
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			t_friends++;
			t_levels += r_ptr->level;
			my_fprintf(fff, "%v %v\n",	get_symbol(r_ptr),
				monster_desc_f2, m_ptr, 0x88);
		}
	}

	max_friends = 1 + (skill_set[SKILL_RACIAL].value / (upkeep_divider*2));
	if (t_friends > max_friends)
	{
		show_upkeep = (t_levels);

		if (show_upkeep > 100) show_upkeep = 100;
		else if (show_upkeep < 10) show_upkeep = 10;
	}


	fprintf(fff,"----------------------------------------------\n");
	fprintf(fff,"              Total: %d all%s.\n", t_friends, (t_friends==1?"y":"ies"));
	fprintf(fff," Max without upkeep: %d.\n",max_friends);
	fprintf(fff,"             Upkeep: %d%% mana.\n", show_upkeep);


	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Current Allies");

	/* Remove the file */
	fd_kill(file_name);
}



/*
 * Count the number of monsters killed. As we want to calculate the total before
 * displaying anything, we run this first without output, and then with.
 */
static long count_kills(FILE *fff, bool noisy)
{
	/* Monsters slain */
	int kk, Total = 0;

	for (kk = 1; kk < MAX_R_IDX; kk++)
	{
		monster_race *r_ptr = &r_info[kk];

		s16b This = r_ptr->r_pkills;

		/* Skip "fake" monsters. */
		if (is_fake_monster(r_ptr)) continue;

		Total += This;
		if (This && noisy)
		{
			byte flags = 0;
			if (This > 1) flags |= MDF_NUMBER;
			else if (r_ptr->flags1 & RF1_UNIQUE) flags |= MDF_DEF;
			else flags |= MDF_INDEF;
			
			my_fprintf(fff, " %v   %v\n", get_symbol(r_ptr),
				monster_desc_aux_f3, r_ptr, This, flags);
		}
	}
	return Total;
}

/*
 * Total kill count
 *
 * Note that the player ghosts are ignored.  XXX XXX XXX
 */
static void do_cmd_knowledge_kill_count(void)
{
	FILE *fff;

	char file_name[1024];

	long Total = 0;


	/* Open a new file */
	if (!((fff = my_fopen_temp(file_name, 1024)))) return;

	/* Count monsters slain */
	Total = count_kills(fff, FALSE);

		if (Total < 1)
			fprintf(fff,"You have defeated no enemies yet.\n\n");
		else if (Total == 1)
			fprintf(fff,"You have defeated one enemy.\n\n");
		else
		fprintf(fff,"You have defeated %ld enemies.\n\n", Total);

	/* Display the species-by-species breakdown. */
	(void)count_kills(fff, TRUE);

	fprintf(fff,"----------------------------------------------\n");
	fprintf(fff,"   Total: %ld creature%s killed.\n", Total, (Total==1?"":"s"));

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Kill Count");

	/* Remove the file */
	fd_kill(file_name);
}


/*
 * Sorting hook for do_cmd_knowledge_deaths().
 *
 * Sort from high number of kills to low, and then from low monster level to high.
 */
static bool ang_sort_comp_deaths(vptr u, vptr UNUSED v, int a, int b)
{
	s16b *races = (s16b*)u;
	bool dif = r_info[races[a]].r_deaths-r_info[races[b]].r_deaths;
	if (!dif) dif = r_info[races[b]].level-r_info[races[a]].level;
	return (dif >= 0);
}

/*
 * Swapping hook for do_cmd_knowledge_deaths()
 */
static void ang_sort_swap_deaths(vptr u, vptr UNUSED v, int a, int b)
{
	s16b *races = (s16b*)u;
	s16b c = races[a];
	races[a]=races[b];
	races[b]=c;
}
/*
 * Display those monsters who have directly killed players in the current
 * save file. Note that non-living causes (e.g. poison) aren't recorded.
 */
static void do_cmd_knowledge_deaths(void)
{
	char file_name[1024];

	/* Open a new file */
	FILE *fff = my_fopen_temp(file_name, 1024);
	
	if (fff)
	{

	typedef struct death_type death_type;
	
	C_TNEW(races, MAX_R_IDX, s16b);

	long i, Uniques = 0, Races = 0, Deaths = 0;

	/* Count the monsters who have killed some ancestors. */
	for (i = 0; i < MAX_R_IDX; i++)
	{
		if (r_info[i].r_deaths) races[Races++] = i;
	}
	
	/* Are there any? */
	if (!Races)
	{
		fprintf(fff, "None of your ancestors have been killed directly by a monster here.");
	}
	else
	{
		/* Sort the list by numbers of deaths. */
		ang_sort_comp = ang_sort_comp_deaths;
		ang_sort_swap = ang_sort_swap_deaths;
		ang_sort(races, 0, Races);
		fprintf(fff, "The following monster%s have claimed some of your ancestors' deaths here:\n", (Races == 1) ? "" : "s");

		/* Display the sorted list. */
		for (i = 0; i < Races; i++)
		{
			monster_race *r_ptr = &r_info[races[i]];
			int num = (r_ptr->flags1 & RF1_UNIQUE) ? 1 : r_ptr->r_deaths;
			byte flags;
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				flags = MDF_DEF;
			}
			else if (r_ptr->r_deaths == 1)
			{
				flags = MDF_INDEF;
			}
			else
			{
				flags = 0;
			}

			/* Format the string, including the monster's ASCII representation. */
			my_fprintf(fff, " %v   %d w%s killed by %v.\n",
				get_symbol(r_ptr), r_ptr->r_deaths, (r_ptr->r_deaths == 1) ?
				"as" : "ere", monster_desc_aux_f3, r_ptr, num, flags);

			/* Count the total. */
			Deaths+=r_ptr->r_deaths;
			if (r_ptr->flags1 & RF1_UNIQUE) Uniques+=r_ptr->r_deaths;
		}

		/* Display a summary at the bottom. */
		fprintf(fff,"----------------------------------------------\n");
		fprintf(fff,"Total: Killed %ld times by %ld different things (%ld times by uniques).\n", Deaths, Races, Uniques);
	}

	TFREE(races);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Causes of death");

	/* Remove the file */
	fd_kill(file_name);

	}
}

/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(void)
{
	int k;

	FILE *fff;

	char file_name[1024];


	/* Open a new file */
	if (!((fff = my_fopen_temp(file_name, 1024)))) return;

	/* Scan the object kinds */
	for (k = 1; k < MAX_K_IDX; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Hack -- skip items which only have special generation methods. */
		if (!kind_created_p(k_ptr)) continue;

		/* List known flavored objects */
		if (u_info[k_ptr->u_idx].s_id && k_ptr->aware)
		{
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Create fake object */
			object_prep(i_ptr, k);

			/* Print a message */
			my_fprintf(fff, " %v   %v\n", get_symbol_f2,
				object_attr(i_ptr), isprint(object_char(i_ptr)) ?
				object_char(i_ptr) : '#',
				object_desc_f3, i_ptr, OD_SHOP, 0);
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
 * List chaos features we have...
 *
 */
void do_cmd_knowledge_chaos_features(void)
{

	FILE *fff;

	char file_name[1024];


	/* Open a new file */
	if (!((fff = my_fopen_temp(file_name, 1024)))) return;

	if (fff) dump_chaos_features(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Chaos Features");

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Return the apparent chance (in percent) of failure of a hit against the
 * player by a monster with a given level using an attack with a given power.
 *
 * The same calculation is also used for traps.
 */
static int check_hit_percent(int power, int level)
{
	/* Calculate the player's apparent total AC. */
	int ac = p_ptr->dis_ac + p_ptr->dis_to_a;

	int num = MAX(1, ac * 3 / 4);

	int denom = MAX(num, power + level * 3);

	return 5 + 90 * num / denom;
}

/*
 * Output the effects derived from having your apparent AC to a file.
 */
static void do_cmd_knowledge_player_ac(FILE *fff)
{
	/* Calculate your current level, your recall level if in town. */
	int ac = p_ptr->dis_ac + p_ptr->dis_to_a;
	int lev;

	/* In the dungeon. */
	if (dun_level)
	{
		lev = dun_depth;
	}
	/* Previously in the dungeon. */
	else if (p_ptr->max_dlv)
	{
		lev = p_ptr->max_dlv + dun_defs[recall_dungeon].offset;
	}
	/* Still on the surface. */
	else
	{
		lev = 0;
	}

	/* See make_attack_normal(). */
	fprintf(fff,
		"You will take %d%% of the damage of attacks to hurt or to shatter.\n",
		ac < 150 ? 100-100*ac/250 : 40);

	/* See check_hit() in cmd1.c. */
	fprintf(fff, "A dart trap has a %d%% chance of missing you.\n",
		check_hit_percent(125, 0));

	/* See check_hit() in melee1.c and make_attack_normal(). */
	fprintf(fff, "A monster native to level %d which:\n", lev);
	fprintf(fff, "   attacks to hurt has a %d%% chance of missing you.\n",
		check_hit_percent(60, lev));
	fprintf(fff, "   attacks to shoot acid has a %d%% chance of missing you.\n",
		check_hit_percent(0, lev));
}

/*
 * Work out what the player's apparent saving throw is. In most cases, this
 * will be the actual saving throw, but an anti-magic shell can exist but be
 * unknown.
 */
static void do_cmd_knowledge_player_save(FILE *fff)
{
	int skill_sav;

	/* Anti-magic may be possessed unknowingly. */
	if (p_ptr->anti_magic && p_ptr->skill_sav == 95)
	{
		/* Look for an object known to grant an anti-magic field. */
		for (skill_sav = INVEN_WIELD; skill_sav <= INVEN_FEET; skill_sav++)
		{
			object_type o_ptr[1];
			object_info_known(o_ptr, inventory+skill_sav);
			if (o_ptr->flags3 & TR3_NO_MAGIC) break;
		}

		/* Found one. */
		if (skill_sav <= INVEN_FEET)
		{
			skill_sav = p_ptr->skill_sav;
		}
		/* Calculate what the saving throw would be otherwise (as in
		 * calc_bonuses()). */
		else
		{
			skill_sav = skill_set[SKILL_SAVE].value;

			if (p_ptr->muta3 & MUT3_MAGIC_RES)
			{
				skill_sav += (15 + ((skill_set[SKILL_RACIAL].value/2) / 5));
			}
			
			/* Affect Skill -- saving throw (WIS) */
			skill_sav += adj_wis_sav[p_ptr->stat_ind[A_WIS]];
		}
	}
	else
	{
		skill_sav = p_ptr->skill_sav;
	}

	/* Only values between 0 and 101 have an effect. */
	if (skill_sav < 0) skill_sav = 0;
	else if (skill_sav > 101) skill_sav = 101;

	fprintf(fff, "You have a %d%% saving throw.\n", skill_sav);
}

/*
 * Print out where the player is about to recall to, if anywhere.
 */
static bool do_cmd_knowledge_player_recall(FILE *fff)
{
	cptr to;
	if (!p_ptr->word_recall) return FALSE;
	if (dun_level)
	{
		to = town_name+town_defs[cur_town].name;
	}
	else
	{
		to = dun_name+dun_defs[recall_dungeon].name;
	}
	fprintf(fff, "You are waiting to recall to %s.\n", to);

	return TRUE;
}

/*
 * Print the player's chance of avoiding theft and falling.
 */
static void do_cmd_knowledge_player_theft(FILE *fff)
{
	int safe = adj_dex_safe[p_ptr->stat_ind[A_DEX]];
	int tough = skill_set[SKILL_TOUGH].value/2;
	int save = skill_set[SKILL_SAVE].value/2;
	fprintf(fff, "You have a %d%% chance of avoiding theft attacks.\n",
		safe + save);
	fprintf(fff,
		"You have a %d%% chance of bashing doors without being paralysed.\n",
		safe+tough);
}

/*
 * Print out various things about the player and his equipment.
 * This should include all of the messages printed by update_stuff() as
 * it is intended as a simple way to access that information.
 */
static void do_cmd_knowledge_player_misc(FILE *fff)
{
	if (p_ptr->ma_cumber_armour)
		fprintf(fff, "The weight of your armor disrupts your balance.\n");
	if (p_ptr->heavy_wield)
		fprintf(fff, "Your weapon is too heavy for you to wield properly.\n");
	if (p_ptr->heavy_shoot)
		fprintf(fff, "Your bow is too heavy for you to wield properly.\n");
	if (p_ptr->cumber_glove)
		fprintf(fff, "Your covered hands feel unsuitable for spellcasting.\n");
	if (p_ptr->cumber_armor)
		fprintf(fff, "The weight of your armor makes spellcasting difficult.\n");
	if (p_ptr->cumber_helm)
		fprintf(fff, "Your covered head feels unsuitable for mindcrafting.\n");
	if (p_ptr->new_spells)
		fprintf(fff, "You can learn %d more spell%s.\n", p_ptr->new_spells,
				(p_ptr->new_spells != 1) ? "s" : "");
}

/*
 * Output the current feeling to a file in the dungeon. Do nothing otherwise.
 */
static bool do_cmd_knowledge_player_feeling(FILE *fff)
{
	if (dun_level)
	{
		fprintf(fff, "%s\n", do_cmd_feeling_text[MIN(MAX(feeling, 0), 10)]);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/*
 * List various pieces of information about the player.
 */
static void do_cmd_knowledge_player(void)
{
	/* Open a new file. */
	char file_name[1024];
	FILE *fff = my_fopen_temp(file_name, 1024);
	if (!fff) return;

	/* Introduction - this is (of course) all based on known information. */
	fprintf(fff, "If your equipment has no unknown qualities:\n\n");

	/* Find out various things. */
	do_cmd_knowledge_player_ac(fff);
	fprintf(fff, "\n");
	do_cmd_knowledge_player_save(fff);
	fprintf(fff, "\n");
	do_cmd_knowledge_player_theft(fff);
	fprintf(fff, "\n");
	if (do_cmd_knowledge_player_feeling(fff))
		fprintf(fff, "\n");
	if (do_cmd_knowledge_player_recall(fff))
		fprintf(fff, "\n");
	do_cmd_knowledge_player_misc(fff);

	/* Close the file. */
	my_fclose(fff);

	/* Display the file contents. */
	show_file(file_name, "Character features");

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Notice if a given town has any real shops.
 */
bool shops_good(int town)
{
	/* The current town has space allocated for shops. */
	if (town_defs[town].numstores)
	{
	 	town_type *t_ptr = &town_defs[town];
		int i;
 		for (i = 0; i < t_ptr->numstores; i++)
		{
			/* Found a real store */
			if (t_ptr->store[i] != STORE_NONE) return TRUE;
		}
	}
	return FALSE;
}

/*
 * Displays a list of the shops in a given town.
 */
void shops_display(int town)
{
	const int offset = MAX_STORES_PER_TOWN*town;
	int i,j;
	town_type *t_ptr = &town_defs[town];

	clear_from(0);
	Term_putstr(0,0, Term->wid, TERM_WHITE,
		format("List of shops in %s",
			dun_name+dun_defs[town].shortname));

	for (i = 0, j = 1; i < t_ptr->numstores; i++)
	{
		feature_type *f_ptr;

		/* Only count real stores */
		if (t_ptr->store[i] == STORE_NONE) continue;
 
 		f_ptr = &f_info[FEAT_SHOP_HEAD+store[i+offset].type];
 
		/* Display the name of the store. */
		Term_putstr(0, ++j, Term->wid, TERM_WHITE, store_title(i+offset));

		/* Put the character used at the top. */
		Term_putch(0, j, f_ptr->x_attr, f_ptr->x_char);
	}
}	


/*
 * Allow the player to see which shops are in each town, what the maximum
 * price of each is and how greedy the shopkeepers are.
 *
 * Maybe it should work on the basis of the last thing the player knew, but
 * it seems like too much effort for little gain.
 */
static void do_cmd_knowledge_shops(void)
{
	/* I hope this isn't too silly... */
	int town = cur_town;
	
	/* Paranoia (?) - no town */
	if (!is_town_p(wildy, wildx)) town = 0;
	for (;;)
	{
		char c;
		shops_display(town);
		prt("[Press +,- or a town symbol to change displayed town, or press ESC to exit.]", 22, 0);
		Term_fresh();
		c = inkey();
		Term_putch(0,0,TERM_YELLOW,c);
		if (c == ESCAPE)
		{
			break;
		}
		else if (c == '+')
		{
			do
			{
				town=(town+1)%MAX_TOWNS;
				
			} while (!shops_good(town));
		}
		else if (c == '-')
		{
			do
			{
				town=(town+MAX_TOWNS-1)%MAX_TOWNS;
				
			} while (!shops_good(town));
		}
		else
		{
			int i;
			for (i = 0;; i++)
			{
				/* None found. */
				if (i == MAX_TOWNS)
				{
					bell(0);
					break;
				}

				if ((dun_defs[i].sym == c) &&
					(shops_good(i)))
				{
					town = i;
					break;
				}
			}
		}
	}
}

/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
	int i, t;

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	t = Term_save_aux();


	/* Interact until done */
	while (1)
	{
		typedef struct knowledge_type knowledge_type;
		struct knowledge_type {
		cptr text;
		void (*func)(void);
		};
		knowledge_type knowledge[] = {
		{"known artifacts", do_cmd_knowledge_artifacts},
		{"known uniques", do_cmd_knowledge_uniques},
		{"known objects", do_cmd_knowledge_objects},
		{"kill count", do_cmd_knowledge_kill_count},
		{"ancestral causes of death", do_cmd_knowledge_deaths},
		{"chaos features", do_cmd_knowledge_chaos_features},
		{"current allies", do_cmd_knowledge_pets},
		{"shop prices", do_cmd_knowledge_shops},
		{"extra character information", do_cmd_knowledge_player},
		};
		byte max_knowledge = sizeof(knowledge)/sizeof(knowledge_type);
		/* Clear screen */
		Term_clear();

		/* Ask for a choice */
		prt("Display current knowledge", 2, 0);

		/* Give some choices */
		for (i = 0; i < max_knowledge; i++)
		{
			knowledge_type *kn_ptr = &knowledge[i];
			prt(format("(%d) Display %s", i+1, kn_ptr->text), 4+i, 5);
		}

		/* Prompt */
		prt("Command: ", 5+i, 0);

		/* Prompt */
		i = inkey();

		/* Done */
		if (i == ESCAPE) break;

		/* Known options (see above) */
		else if (i > '0' && i < '1'+max_knowledge)
		{
			knowledge_type *kn_ptr = &knowledge[i-'1'];
			char help_str[] = "cmd=~0";
			help_str[5]=i;
			help_track(help_str);
			(*(kn_ptr->func))();
			help_track(NULL);
		}

		/* Unknown option */
		else
		{
			bell(0);
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Restore the screen */
	Term_load_aux(t);
	Term_release(t);

	/* Leave "icky" mode */
	character_icky = FALSE;
}

