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

	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Update view */
	p_ptr->update |= (PU_VIEW | PU_MON_LITE);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON |
					  PW_MONSTER | PW_VISIBLE | PW_OBJECT);

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
	}

	/* Restore */
	Term_activate(old);
}


/*
 * Map resizing whenever the main term changes size
 */
void resize_map(void)
{
	/* Only if the dungeon exists */
	if (!character_dungeon) return;

	/* Reset the panels */
	p_ptr->update |= (PU_MAP);

	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Update view */
	p_ptr->update |= (PU_VIEW | PU_MON_LITE);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Hack -- update */
	handle_stuff();

	/* Place the cursor on the player */
	if (!character_icky) move_cursor_relative(p_ptr->px, p_ptr->py);

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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON |
					  PW_MONSTER | PW_VISIBLE | PW_OBJECT);

	/* Hack -- update */
	handle_stuff();

	/* Refresh */
	Term_fresh();
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
	int i, j, k, n;
	int q;

	char shower[80];
	char finder[80];

	int wid, hgt;

	/* Wipe finder */
	finder[0] = 0;

	/* Wipe shower */
	shower[0] = 0;

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
		for (j = 0; (j < hgt - 4) && (i + j < n); j++)
		{
			cptr msg = message_str((s16b)(i + j));
			cptr attr = color_seq[message_color((s16b)(i + j))];

			/* Apply horizontal scroll */
			msg = ((int)strlen(msg) >= q) ? (msg + q) : "";

			/* Dump the messages, bottom to top */
			put_fstr(0, hgt - 3 - j, "%s%s", attr, msg);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = msg;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);
				
					/* Display the match */
					put_fstr(str - msg, hgt - 3 - j, CLR_YELLOW "%s", shower);

					/* Advance */
					str += len;
				}
			}
		}

		/* Display header XXX XXX XXX */
		prtf(0, 0, "Message Recall (%d-%d of %d), Offset %d",
				   i, i + j - 1, n, q);

		/* Display prompt (not very informative) */
		prtf(0, hgt - 1,
				"[Press 'p' for older, 'n' for newer, ..., or ESCAPE]");

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
			prtf(0, hgt - 1, "Show: ");

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
			prtf(0, hgt - 1, "Find: ");

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
 * Copy the indicated options out from the
 * option_info[] data structure into the
 * player, birth and server data structures as indicated.
 *
 * Options that should be unchanged are restored to the
 * current state.  (This stops the player changing the
 * birth options in the middle of the game.)
 */
void init_options(byte flags)
{
	int birth_counter = 0, server_counter = 0, player_counter = 0;
	int i;

	for (i = 0; i < OPT_MAX; i++)
	{
		/* A birth option? */
		if (i == birth_options[birth_counter])
		{
			/* Are we allowed to copy the data? */
			if (flags & OPT_FLAG_BIRTH)
			{
				/* Copy the state of the flag into p_ptr->birth[] */
				p_ptr->birth[birth_counter] = option_info[i].o_val;
			}
			else
			{
				/* Restore the option to its original value */
				option_info[i].o_val = p_ptr->birth[birth_counter];
			}

			/* Increment birth option counter */
			birth_counter++;
		}

		/* A server option? */
		else if (i == server_options[server_counter])
		{
			/* Are we allowed to copy the data? */
			if (flags & OPT_FLAG_SERVER)
			{
				/* Copy the state of the flag into svr_ptr->options[] */
				svr_ptr->options[server_counter] = option_info[i].o_val;
			}
			else
			{
				/* Restore the option to its original value */
				option_info[i].o_val = svr_ptr->options[server_counter];
			}

			/* Increment server option counter */
			server_counter++;
		}

		/* A player option */
		else
		{
			/* Are we allowed to copy the data? */
			if (flags & OPT_FLAG_PLAYER)
			{
				/* Copy the state of the flag into p_ptr->options[] */
				p_ptr->options[player_counter] = option_info[i].o_val;
			}
			else
			{
				/* Restore the option to its original value */
				option_info[i].o_val = p_ptr->options[player_counter];
			}

			/* Increment player option counter */
			player_counter++;
		}
	}

	/*
	 * XXX XXX XXX This is an absolutely evil hack.
	 * If ironman_downward is set - set vanilla town as well
	 */
	if (ironman_downward)
	{
		vanilla_town = TRUE;
	
		/* And here is the bit that shouldn't see the light of day. */
		option_info[192].o_val = TRUE;
	}
}



/*
 * Number of cheating options
 */
#define CHEAT_MAX 6

typedef struct cheat_option_type cheat_option_type;

struct cheat_option_type
{
	bool *o_var;

	u16b o_word;

	cptr o_text;
	cptr o_desc;
};


/*
 * Cheating options
 */
static const cheat_option_type cheat_info[CHEAT_MAX] =
{
	{&cheat_peek, 0x0001,
	 "cheat_peek", "Peek into object creation"},

	{&cheat_hear, 0x0002,
	 "cheat_hear", "Peek into monster creation"},

	{&cheat_room, 0x0004,
	 "cheat_room", "Peek into dungeon creation"},

	{&cheat_xtra, 0x0008,
	 "cheat_xtra", "Peek into something else"},

	{&cheat_know, 0x0010,
	 "cheat_know", "Know complete monster info"},

	{&cheat_live, 0x0020,
	 "cheat_live", "Allow player to avoid death"}
};

/* Forward declare */
extern menu_type cheat_menu[CHEAT_MAX + 1];

static bool do_cmd_options_cheat_aux(int option)
{
	char buf[1024];

	/* Toggle the option */
	(*cheat_info[option].o_var) = !(*cheat_info[option].o_var);
	
	if (*cheat_info[option].o_var)
	{
		/* Turn on the cheating flag */
		p_ptr->state.noscore |= cheat_info[option].o_word;
	}
	
	/* Change the option text */
	strnfmt(buf, 1024, "%-48s: %s  (%s)",
			cheat_info[option].o_desc,
			(*cheat_info[option].o_var ? "yes" : "no "),
			cheat_info[option].o_text);
	
	/* Delete old string */
	string_free(cheat_menu[option].text);

	/* Save new string */
	cheat_menu[option].text = string_make(buf);

	return (FALSE);
}

menu_type cheat_menu[CHEAT_MAX + 1] =
{
	{NULL, NULL, do_cmd_options_cheat_aux, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_cheat_aux, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_cheat_aux, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_cheat_aux, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_cheat_aux, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_cheat_aux, MN_ACTIVE | MN_SELECT},
	MENU_END
};

/*
 * Interact with some options for cheating
 */
static bool do_cmd_options_cheat(int dummy)
{
	char buf[1024];

	int i;
	
	/* Hack - ignore unused parameter */
	(void) dummy;
	
	/* Create the list of options */
	for (i = 0; i < CHEAT_MAX; i++)
	{
		/* Change the option text */
		strnfmt(buf, 1024, "%-48s: %s  (%s)",
				cheat_info[i].o_desc,
				(*cheat_info[i].o_var ? "yes" : "no "),
				cheat_info[i].o_text);
	
		/* Delete old string */
		string_free(cheat_menu[i].text);

		/* Save new string */
		cheat_menu[i].text = string_make(buf);
	}

	display_menu(cheat_menu, 0, TRUE, NULL, "Cheaters never win");

	return (FALSE);
}


static const cheat_option_type autosave_info[2] =
{
	{(bool *)(&autosave_l), 0x0001,
	 "autosave_l", "Autosave when entering new levels"},

	{(bool *)(&autosave_t), 0x0002,
	 "autosave_t", "Timed autosave"},
};


/* Forward declare */
extern menu_type autosave_menu[4];


static bool do_cmd_options_toggle_frequency(int option)
{
	s16b current = autosave_freq;
	
	char buf[1024];
	
	if (current == 0) autosave_freq = 50;
	if (current == 50) autosave_freq = 100;
	if (current == 100) autosave_freq = 250;
	if (current == 250) autosave_freq = 500;
	if (current == 500) autosave_freq = 1000;
	if (current == 1000) autosave_freq = 2500;
	if (current == 2500) autosave_freq = 5000;
	if (current == 5000) autosave_freq = 10000;
	if (current == 10000) autosave_freq = 25000;
	if (current == 25000) autosave_freq = 0;

	strnfmt(buf, 1024, "Timed autosave frequency: every %d turns", autosave_freq);
	
	/* Delete old string */
	string_free(autosave_menu[option].text);

	/* Save new string */
	autosave_menu[option].text = string_make(buf);

	return (FALSE);
}



static bool do_cmd_options_autosave_aux(int option)
{
	char buf[1024];
	
	/* Toggle the option */
	(*autosave_info[option].o_var) = !(*autosave_info[option].o_var);
		
	/* Change the option text */
	strnfmt(buf, 1024, "%-48s: %s  (%s)",
			autosave_info[option].o_desc,
			(*autosave_info[option].o_var ? "yes" : "no "),
			autosave_info[option].o_text);
	
	/* Delete old string */
	string_free(autosave_menu[option].text);

	/* Save new string */
	autosave_menu[option].text = string_make(buf);

	return (FALSE);
}


menu_type autosave_menu[4] =
{
	{NULL, NULL, do_cmd_options_autosave_aux, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_autosave_aux, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_toggle_frequency, MN_ACTIVE},
	MENU_END
};


/*
 * Interact with some options for cheating
 */
static bool do_cmd_options_autosave(int dummy)
{
	char buf[1024];

	int i;
	
	/* Hack - ignore unused parameter */
	(void) dummy;
	
	for (i = 0; i < 2; i++)
	{
		/* Change the option text */
		strnfmt(buf, 1024, "%-48s: %s  (%s)",
				autosave_info[i].o_desc,
				(*autosave_info[i].o_var ? "yes" : "no "),
				autosave_info[i].o_text);
	
		/* Delete old string */
		string_free(autosave_menu[i].text);

		/* Save new string */
		autosave_menu[i].text = string_make(buf);
	}
	
	/* Get string for autosave frequency */
	strnfmt(buf, 1024, "Timed autosave frequency: every %d turns", autosave_freq);
	
	/* Delete old string */
	string_free(autosave_menu[2].text);

	/* Save new string */
	autosave_menu[2].text = string_make(buf);
	
	display_menu(autosave_menu, 0, TRUE, NULL, "Autosave");
	
	return (FALSE);
}

/* Current option flags */
static byte option_flags;

static int option_page;

/* Forward declare */
extern menu_type options_aux_menu[25];

/*
 * Toggle the selected option
 */
static bool do_cmd_options_aux2(int option)
{
	int i, j = option;
	char buf[1024];

	/* Find the option to change */
	for (i = 0; option_info[i].o_desc; i++)
	{
		/* Notice options on this "page" */
		if ((option_info[i].o_page == option_page) &&
			(option_info[i].o_text))
		{
			if (!j)
			{
				option_info[i].o_val = !option_info[i].o_val;
				
				strnfmt(buf, 1024, "%-48s: %s  (%.23s)",
						option_info[i].o_desc,
						(option_info[i].o_val ? "yes" : "no "),
						option_info[i].o_text);
			
				/* Update the description */
				string_free(options_aux_menu[option].text);
				options_aux_menu[option].text = string_make(buf);
				
				/* Update the help */
				strnfmt(buf, 1024, "option.txt#%s", option_info[i].o_text);
				
				string_free(options_aux_menu[option].help);
				options_aux_menu[option].help = string_make(buf);
				
				/* Done */
				break;
			}
			else
			{
				/* Count down until we get to the required option */
				j--;
			}
		}
	}

	return (FALSE);
}


menu_type options_aux_menu[25] =
{
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, do_cmd_options_aux2, MN_ACTIVE | MN_SELECT},
	MENU_END
};


/*
 * Screen titles for each option sub-window
 */
static cptr option_window_title[8] =
{
	"User Interface Options",
	"Disturbance Options",
	"Game-Play Options",
	"Efficiency Options",
	"Display Options",
	"Birth Options",
	"Artificial Intelligence Options",
	"Testing Options"
};

/*
 * Interact with some options
 */
static bool do_cmd_options_aux(int page)
{
	int i, n = 0;
	
	char buf[1024];
	
	/* Save the current page */
	option_page = page + 1;

	/* Clear the options (24 options max + MENU_END) */
	for (i = 0; i < 25; i++)
	{
		string_free(options_aux_menu[i].text);
		options_aux_menu[i].text = NULL;
		
		string_free(options_aux_menu[i].help);
		options_aux_menu[i].help = NULL;
	}

	/* Scan the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		/* Notice options on this "page" */
		if (option_info[i].o_page == option_page)
		{
			/* Update the description */
			strnfmt(buf, 1024, "%-48s: %s  (%.23s)",
					option_info[i].o_desc,
					(option_info[i].o_val ? "yes" : "no "),
					option_info[i].o_text);
			
			options_aux_menu[n].text = string_make(buf);
			
			/* Update the help */
			strnfmt(buf, 1024, "option.txt#%s", option_info[i].o_text);
			options_aux_menu[n].help = string_make(buf);

			n++;
		}
	}


	/* Paranoia */
	if (n == 0)
	{
		/* There are no options */
		msgf("There are no available options there at the moment.");
		message_flush();

		/* Bail out */
		return (FALSE);
	}
	
	display_menu(options_aux_menu, 0, TRUE, NULL, option_window_title[page]);
	
	/* Save the changes */
	init_options(option_flags);

	return (FALSE);
}


/*
 * Modify the "window" options
 */
static bool do_cmd_options_win(int dummy)
{
	int i, j, d;

	int y = 0;
	int x = 0;

	char ch;

	bool go = TRUE;

	u32b old_flag[ANGBAND_TERM_MAX];

	/* Hack - ignore parameter */
	(void) dummy;


	/* Memorize old flags */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Acquire current flags */
		old_flag[j] = window_flag[j];
	}

	screen_save();

	/* Clear screen */
	Term_clear();
	
	/* Interact */
	while (go)
	{
		/* Prompt XXX XXX XXX */
		prtf(0, 0, "Window Flags (<dir>, t, y, n, ESC) ");

		/* Display the windows */
		for (j = 0; j < ANGBAND_TERM_MAX; j++)
		{
			/* Window name, staggered, centered */
			put_fstr(35 + j * 5 - strlen(angband_term_name[j]) / 2, 2 + j % 2,
						CLR_L_BLUE "%s", angband_term_name[j]);
		}

		/* Display the options */
		for (i = 0; i < WINDOW_CHOICE_MAX; i++)
		{
			cptr str = window_flag_desc[i];

			/* Unused option */
			if (!str) continue;

			/* Flag name */
			put_fstr(0, i + 5, CLR_L_BLUE "%s", str);

			/* Display the windows */
			for (j = 0; j < ANGBAND_TERM_MAX; j++)
			{
				char c = '.';
				byte a;

				a = TERM_WHITE;

				/* Use color */
				if (use_color && (i == y) && (j == x))
				{
					a = TERM_L_BLUE;
				}

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
				for (j = 0; j < ANGBAND_TERM_MAX; j++)
				{
					window_flag[j] &= ~(1L << y);
				}

				/* Clear flags */
				for (i = 0; i < WINDOW_CHOICE_MAX; i++)
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
				y = (y + ddy[d] + WINDOW_CHOICE_MAX) % WINDOW_CHOICE_MAX;

				if (!d) bell("Illegal command for window options!");
			}
		}
	}
	
	/* Hack - assume all windows will change */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL |
					  PW_PLAYER | PW_MESSAGE | PW_OVERHEAD |
					  PW_MONSTER | PW_OBJECT | PW_SNAPSHOT |
					  PW_BORG_1 | PW_BORG_2 | PW_DUNGEON);


	/* Notice changes */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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
	
	screen_load();
	
	return (FALSE);
}

/*
 * Modify the base delay factor
 */
static bool do_cmd_options_delay(int dummy)
{
	char k;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();

	/* Clear screen */
	Term_clear();

	/* Prompt */
	prtf(0, 18, "Command: Base Delay Factor");

	/* Get a new value */
	while (1)
	{
		int msec = delay_factor * delay_factor * delay_factor;
		prtf(0, 22, "Current base delay factor: %d (%d msec)",
				   delay_factor, msec);
		prtf(0, 20, "Delay Factor (0-9 or ESC to accept): ");

		k = inkey();

		if (k == ESCAPE) break;
		if (isdigit(k)) delay_factor = D2I(k);
		else
			bell("Illegal delay factor!");
	}

	screen_load();


	return (FALSE);
}

/*
 * Modify the hitpoint warning threshold
 */
static bool do_cmd_options_hitpoint(int dummy)
{
	char k;
	
	/* Hack - ignore parameter */
	(void) dummy;

	screen_save();

	/* Clear screen */
	Term_clear();

	/* Prompt */
	prtf(0, 18, "Command: Hitpoint Warning");

	/* Get a new value */
	while (1)
	{
		prtf(0, 22, "Current hitpoint warning: %d0%%",
				   hitpoint_warn);
		prtf(0, 20, "Hitpoint Warning (0-9 or ESC to accept): ");

		k = inkey();

		if (k == ESCAPE) break;
		if (isdigit(k)) hitpoint_warn = D2I(k);
		else
			bell("Illegal hitpoint warning!");
	}

	screen_load();

	return (FALSE);
}

static bool do_cmd_options_dump(int dummy)
{
	int i;
	FILE *fff;
	char buf[1024];
	
	/* Hack - ignore parameter */
	(void) dummy;

	screen_save();

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, "pref-opt.prf");

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Failed */
	if (!fff) return (FALSE);

	/* Header */
	froff(fff, "# File: pref-opt.prf\n\n");
	froff(fff, "# Allow user specification of various options\n\n");

	/* Scan the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		if ((option_info[i].o_text) &&
			(option_info[i].o_page != OPT_BIRTH_PAGE))
		{
			/* Dump the option */
			froff(fff, "%c:%s\n", (option_info[i].o_val ? 'Y' : 'X'),
					option_info[i].o_text);
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Clear top row */
	clear_msg();

	/* Success message */
	msgf("Saved default options.");

	screen_load();

	return (FALSE);
}

/* Number of things in the main options menu */
#define OPTION_MENU_MAX			18


/* The main options menu */
static menu_type options_menu[OPTION_MENU_MAX] =
{
	{"User Interface Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Disturbance Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Game-Play Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Efficiency Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Display Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Birth Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Artificial Intelligence Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Testing Options", NULL, do_cmd_options_aux, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	MENU_SEPERATOR,
	{"Cheating Options", NULL, do_cmd_options_cheat, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Base Delay Factor", NULL, do_cmd_options_delay, MN_ACTIVE | MN_SELECT},
	{"Hitpoint Warning", NULL, do_cmd_options_hitpoint, MN_ACTIVE | MN_SELECT},
	MENU_SEPERATOR,
	{"Autosave Options", NULL, do_cmd_options_autosave, MN_ACTIVE | MN_SELECT | MN_CLEAR},
	{"Window Flags", NULL, do_cmd_options_win, MN_ACTIVE | MN_SELECT},
	MENU_SEPERATOR,
	{"Dump Options to a Pref File", NULL, do_cmd_options_dump, MN_ACTIVE | MN_SELECT},
	MENU_END
};


/*
 * Set or unset various options.
 *
 * The user must use the "Ctrl-R" command to "adapt" to changes
 * in any options which control "visual" aspects of the game.
 */
void do_cmd_options(byte flags)
{
	/* Save option flags so menu functions can access them */
	option_flags = flags;

	display_menu(options_menu, 0, TRUE, NULL, VERSION_NAME " options");

	/* Hack - Redraw equippy chars */
	p_ptr->redraw |= (PR_EQUIPPY);
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
	buf[0] = 0;

	/* Ask for a "user pref command" */
	if (!get_string(buf, 80, "Pref: ")) return;

	/* Process that pref command */
	(void)process_pref_file_command(buf);
}


#ifdef ALLOW_MACROS

/*
 * Hack -- append all current macros to the given file
 */
errr macro_dump(cptr fname)
{
	int i;

	FILE *fff;

	char buf[1024];


	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);


	/* Skip space */
	froff(fff, "\n\n");

	/* Start dumping */
	froff(fff, "# Automatic macro dump\n\n");

	/* Dump them */
	for (i = 0; i < macro__num; i++)
	{
		/* Start the macro */
		froff(fff, "# Macro '%d'\n\n", i);

		/* Extract the action */
		ascii_to_text(buf, macro__act[i]);

		/* Dump the macro */
		froff(fff, "A:%s\n", buf);

		/* Extract the action */
		ascii_to_text(buf, macro__pat[i]);

		/* Dump normal macros */
		froff(fff, "P:%s\n", buf);

		/* End the macro */
		froff(fff, "\n\n");
	}

	/* Start dumping */
	froff(fff, "\n\n\n\n");


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
	p_ptr->cmd.inkey_base = TRUE;

	/* First key */
	i = inkey();

	/* Read the pattern */
	while (i)
	{
		/* Save the key */
		buf[n++] = i;

		/* Do not process macros */
		p_ptr->cmd.inkey_base = TRUE;

		/* Do not wait for keys */
		p_ptr->cmd.inkey_scan = TRUE;

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
	roff("%s", tmp);
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
	roff("%s", tmp);


	/* Flush */
	flush();
}


/*
 * Hack -- append all keymaps to the given file
 */
errr keymap_dump(cptr fname)
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
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return (-1);


	/* Skip space */
	froff(fff, "\n\n");

	/* Start dumping */
	froff(fff, "# Automatic keymap dump\n\n");

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
		froff(fff, "A:%s\n", buf);
		froff(fff, "C:%d:%s\n", mode, key);
	}

	/* Start dumping */
	froff(fff, "\n\n\n");


	/* Close */
	my_fclose(fff);

	/* Success */
	return (0);
}


/*
 * Load a user pref file
 */
static bool do_cmd_pref_key_load(int dummy)
{
	char tmp[1024];

	/* Hack - ignore parameter */
	(void) dummy;

	/* Prompt */
	prtf(0, 16, "Command: Load a user pref file\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "%s.prf", player_base);

	/* Ask for a file */
	if (!askfor_aux(tmp, 80))
	{
		return (FALSE);
	}
	
	/* Process the given filename */
	if (0 != process_pref_file("%s", tmp))
	{
		/* Prompt */
		msgf("Could not load file!");
	}

	return (FALSE);
}

#ifdef ALLOW_MACROS

static int display_cur_action(int dummy)
{
	char buf[1024];
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	Term_clear();

	/* Analyze the current action */
	ascii_to_text(buf, macro__buf);

	/* Describe + display that action */
	prtf(0, 20, "Current action (if any) shown below:\n\n%s", buf);
	
	/* No offset */
	return (0);
}


/*
 * Append macros to a file
 */
static bool do_cmd_macro_append(int dummy)
{
	char tmp[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Prompt */
	prtf(0, 16, "Command: Append macros to a file\n\n"
				"File: ");
	
	/* Default filename */
	strnfmt(tmp, 1024, "%s.prf", player_base);

	/* Ask for a file */
	if (!askfor_aux(tmp, 80))
	{
		return (FALSE);
	}
	
	/* Dump the macros */
	(void)macro_dump(tmp);

	/* Prompt */
	msgf("Appended macros.");
	
	return (FALSE);
}


/*
 * Query a macro
 */
static bool do_cmd_macro_query(int dummy)
{
	char tmp[1024];
	int k;

	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Prompt */
	prtf(0, 16, "Command: Query a macro\n\n"
				"Trigger: ");
	
	/* Get a macro trigger */
	do_cmd_macro_aux(tmp);

	/* Acquire action */
	k = macro_find_exact(tmp);

	/* Nothing found */
	if (k < 0)
	{
		/* Prompt */
		msgf("Found no macro.");
	}

	/* Found one */
	else
	{
		/* Obtain the action */
		strcpy(macro__buf, macro__act[k]);

		/* Analyze the current action */
		ascii_to_text(tmp, macro__buf);

		/* Display the current action */
		prtf(0, 22, tmp);

		/* Prompt */
		msgf("Found a macro.");
	}

	return (FALSE);
}


/*
 * Create a macro
 */
static bool do_cmd_macro_create(int dummy)
{
	char tmp[1024];
	char buf[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Prompt */
	prtf(0, 16, "Command: Create a macro\n\n"
				"Trigger: ");

	/* Get a macro trigger */
	do_cmd_macro_aux(tmp);
	
	/* Clear */
	clear_from(20);
	
	/* Prompt */
	prtf(0, 20, "Action: ");

	/* Convert to text */
	ascii_to_text(buf, macro__buf);

	/* Get an encoded action */
	if (askfor_aux(buf, 80))
	{
		/* Convert to ascii */
		text_to_ascii(macro__buf, buf);

		/* Link the macro */
		macro_add(tmp, macro__buf);

		/* Prompt */
		msgf("Added a macro.");
	}

	return (FALSE);
}


/*
 * Remove a macro
 */
static bool do_cmd_macro_remove(int dummy)
{
	char tmp[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Prompt */
	prtf(0, 16, "Command: Remove a macro\n\n"
				"Trigger: ");

	/* Get a macro trigger */
	do_cmd_macro_aux(tmp);

	/* Link the macro */
	macro_add(tmp, tmp);

	/* Prompt */
	msgf("Removed a macro.");

	return (FALSE);
}


/*
 * Append the keymaps to a file
 */
static bool do_cmd_keymap_append(int dummy)
{
	char tmp[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Prompt */
	prtf(0, 16, "Command: Append keymaps to a file\n\n"
				"File: ");
	
	/* Default filename */
	strnfmt(tmp, 1024, "%s.prf", player_base);

	/* Ask for a file */
	if (!askfor_aux(tmp, 80))
	{
		return (FALSE);
	}
	
	/* Dump the macros */
	(void)keymap_dump(tmp);

	/* Prompt */
	msgf("Appended keymaps.");
	
	return (FALSE);
}


/*
 * Query a keymap
 */
static bool do_cmd_keymap_query(int dummy)
{
	char buf[1024];
	
	cptr act;
	
	int mode;
	
	/* Hack - ignore parameter */
	(void) dummy;

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

	/* Prompt */
	prtf(0, 16, "Command: Query a keymap\n\n"
				"Keypress: ");
	
	/* Get a keymap trigger */
	do_cmd_macro_aux_keymap(buf);

	/* Look up the keymap */
	act = keymap_act[mode][(byte)(buf[0])];

	/* Nothing found */
	if (!act)
	{
		/* Prompt */
		msgf("Found no keymap.");
	}

	/* Found one */
	else
	{
		/* Obtain the action */
		strcpy(macro__buf, act);

		/* Analyze the current action */
		ascii_to_text(buf, macro__buf);

		/* Display the current action */
		prtf(0, 22, buf);

		/* Prompt */
		msgf("Found a keymap.");
	}
	
	return (FALSE);
}


/*
 * Create a keymap
 */
static bool do_cmd_keymap_create(int dummy)
{
	char tmp[1024];
	char buf[1024];

	int mode;
	
	/* Hack - ignore parameter */
	(void) dummy;

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
	
	/* Prompt */
	prtf(0, 16, "Command: Create a keymap\n\n"
				"Keypress: ");

	/* Get a keymap trigger */
	do_cmd_macro_aux_keymap(buf);
	
	/* Clear */
	clear_from(20);
	
	/* Prompt */
	prtf(0, 20, "Action: ");

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
		msgf("Added a keymap.");
	}

	return (FALSE);
}


/*
 * Remove a keymap
 */
static bool do_cmd_keymap_remove(int dummy)
{
	char buf[1024];

	int mode;
	
	/* Hack - ignore parameter */
	(void) dummy;

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

	/* Prompt */
	prtf(0, 16, "Command: Remove a keymap\n\n"
				"Keypress: ");
	
	/* Get a keymap trigger */
	do_cmd_macro_aux_keymap(buf);

	/* Free old keymap */
	string_free(keymap_act[mode][(byte)(buf[0])]);

	/* Make new keymap */
	keymap_act[mode][(byte)(buf[0])] = NULL;

	/* Prompt */
	msgf("Removed a keymap.");

	return (FALSE);
}

/*
 * Create a new action
 */
static bool do_cmd_action_create(int dummy)
{
	char buf[1024];
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Prompt */
	prtf(0, 16, "Command: Enter a new action");
	
	/* Go to the correct location */
	Term_gotoxy(0, 22);
	
	/* Get the current default action */
	ascii_to_text(buf, macro__buf);

	/* Hack -- limit the value */
	buf[80] = '\0';

	/* Get an encoded action */
	if (!askfor_aux(buf, 80))
	{
		return (FALSE);
	}
	
	/* Extract an action */
	text_to_ascii(macro__buf, buf);
	
	return (FALSE);
}

#endif /* ALLOW_MACROS */


#ifdef ALLOW_MACROS
#define MACRO_MENU_MAX		11
#else
#define MACRO_MENU_MAX		2
#endif /* ALLOW_MACROS */

/* The macro / keymap menu */
static menu_type macro_menu[MACRO_MENU_MAX] =
{
	{"Load a user pref file", NULL, do_cmd_pref_key_load, MN_ACTIVE},
#ifdef ALLOW_MACROS
	{"Append macros to a file", NULL, do_cmd_macro_append, MN_ACTIVE},
	{"Query a macro", NULL, do_cmd_macro_query, MN_ACTIVE},
	{"Create a macro", NULL, do_cmd_macro_create, MN_ACTIVE},
	{"Remove a macro", NULL, do_cmd_macro_remove, MN_ACTIVE},
	{"Append keymaps to a file", NULL, do_cmd_keymap_append, MN_ACTIVE},
	{"Query a keymap", NULL, do_cmd_keymap_query, MN_ACTIVE},
	{"Create a keymap", NULL, do_cmd_keymap_create, MN_ACTIVE},
	{"Remove a keymap", NULL, do_cmd_keymap_remove, MN_ACTIVE},
	{"Enter a new action", NULL, do_cmd_action_create, MN_ACTIVE},
#endif /* ALLOW_MACROS */
	MENU_END
};


/*
 * Interact with "macros"
 *
 * Note that the macro "action" must be defined before the trigger.
 *
 * Could use some helpful instructions on this page.  XXX XXX XXX
 */
void do_cmd_macros(void)
{
	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);
		
	display_menu(macro_menu, -1, FALSE, display_cur_action, "Interact with Macros");
}


/*
 * Load a user pref file
 */
static bool do_cmd_pref_vis_load(int dummy)
{
	char tmp[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();
	
	/* Prompt */
	prtf(0, 13, "Command: Load a user pref file\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Query */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}
	
	/* Process the given filename */
	if (0 != process_pref_file("%s", tmp))
	{
		/* Prompt */
		msgf("Could not load file!");
	}
	
	screen_load();
	return (FALSE);
}

#ifdef ALLOW_VISUALS

/* Dump monster attr/chars to pref file */
static bool do_cmd_dump_monster(int dummy)
{
	char tmp[1024], buf[1024];
	FILE *fff;
	
	int i;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();
	
	/* Prompt */
	prtf(0, 12, "Command: Dump monster attr/chars\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Get a filename */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}
	
	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, tmp);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff)
	{
		screen_load();
		return (FALSE);
	}
	
	/* Start dumping */
	froff(fff, "\n\n");
	froff(fff, "# Monster attr/char definitions\n\n");

	/* Dump monsters */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-entries */
		if (!r_ptr->name) continue;

		/* Dump a comment */
		froff(fff, "# %s\n", mon_race_name(r_ptr));

		/* Dump the monster attr/char info */
		froff(fff, "R:%d:0x%02X:0x%02X\n\n", i,
				(byte)(r_ptr->x_attr), (byte)(r_ptr->x_char));
	}

	/* All done */
	froff(fff, "\n\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Message */
	msgf("Dumped monster attr/chars.");

	screen_load();
	return (FALSE);
}


/* Dump object attr/chars to a pref file */
static bool do_cmd_dump_object(int dummy)
{
	char tmp[1024], buf[1024];
	FILE *fff;
	
	int i;

	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();
	
	/* Prompt */
	prtf(0, 12, "Command: Dump object attr/chars\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Get a filename */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}
	
	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, tmp);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff)
	{
		screen_load();
		return (FALSE);
	}
	
	/* Start dumping */
	froff(fff, "\n\n");
	froff(fff, "# Object attr/char definitions\n\n");

	/* Dump objects */
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip non-entries */
		if (!k_ptr->name) continue;

		/* Dump a comment */
		froff(fff, "# %s\n", (k_name + k_ptr->name));

		/* Dump the object attr/char info */
		froff(fff, "K:%d:0x%02X:0x%02X\n\n", i,
				(byte)(k_ptr->x_attr), (byte)(k_ptr->x_char));
	}

	/* All done */
	froff(fff, "\n\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Message */
	msgf("Dumped object attr/chars.");

	screen_load();

	return (FALSE);
}


/* Dump terrain feature attr/chars to a pref file */
static bool do_cmd_dump_feature(int dummy)
{
	char tmp[1024], buf[1024];
	FILE *fff;
	
	int i;

	/* Hack - ignore parameter */
	(void) dummy;

	screen_save();
	
	/* Prompt */
	prtf(0, 12, "Command: Dump feature attr/chars\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Get a filename */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}
	
	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, tmp);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff)
	{
		screen_load();
		return (FALSE);
	}
	
	/* Start dumping */
	froff(fff, "\n\n");
	froff(fff, "# Feature attr/char definitions\n\n");

	/* Dump features */
	for (i = 0; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Skip non-entries */
		if (!f_ptr->name) continue;

		/* Dump a comment */
		froff(fff, "# %s\n", (f_name + f_ptr->name));

		/* Dump the feature attr/char info */
		froff(fff, "F:%d:0x%02X:0x%02X\n\n", i,
				(byte)(f_ptr->x_attr), (byte)(f_ptr->x_char));
	}

	/* All done */
	froff(fff, "\n\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Message */
	msgf("Dumped feature attr/chars.");
	
	screen_load();
	
	return (FALSE);
}


/* Dump field attr/chars to a pref file */
static bool do_cmd_dump_field(int dummy)
{
	char tmp[1024], buf[1024];
	FILE *fff;
	
	int i;

	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();
	
	/* Prompt */
	prtf(0, 12, "Command: Dump field attr/chars\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Get a filename */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}
	
	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, tmp);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff)
	{
		screen_load();
		return (FALSE);
	}
	
	/* Start dumping */
	froff(fff, "\n\n");
	froff(fff, "# Field attr/char definitions\n\n");

	/* Dump features */
	for (i = 0; i < z_info->t_max; i++)
	{
		field_thaum *t_ptr = &t_info[i];

		/* Skip non-entries */
		if (!t_ptr->name) continue;

		/* Dump a comment */
		froff(fff, "# %s\n", t_ptr->name);

		/* Dump the field attr/char info */
		froff(fff, "F:%d:0x%02X:0x%02X\n\n", i,
				(byte)(t_ptr->f_attr), (byte)(t_ptr->f_char));
	}

	/* All done */
	froff(fff, "\n\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Message */
	msgf("Dumped field attr/chars.");

	screen_load();

	return (FALSE);
}


/* Modify monster attr/chars */
static bool do_cmd_change_monster(int dummy)
{
	static int r = 0;
	
	char i;

	/* Hack - ignore parameter */
	(void) dummy;

	screen_save();
	
	Term_clear();
	
	/* Prompt */
	prtf(0, 5, "Command: Change monster attr/chars");

	/* Hack -- query until done */
	while (1)
	{
		monster_race *r_ptr = &r_info[r];

		byte da = (r_ptr->d_attr);
		byte dc = (r_ptr->d_char);
		byte ca = (r_ptr->x_attr);
		byte cc = (r_ptr->x_char);

		/* Label the object */
		prtf(5, 7, "Monster = %d, Name = %-40.40s",
						   r, mon_race_name(r_ptr));

		/* Label the Default values */
		prtf(10, 9, "Default attr/char = %3u / %3u", da, dc);
		put_fstr(40, 9, "<< ? >>");
		Term_putch(43, 9, da, dc);

		/* Label the Current values */
		prtf(10, 10, "Current attr/char = %3u / %3u", ca, cc);
		put_fstr(40, 10, "<< ? >>");
		Term_putch(43, 10, ca, cc);

		/* Prompt */
		prtf(0, 12, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') r = (r + z_info->r_max + 1) % z_info->r_max;
		if (i == 'N') r = (r + z_info->r_max - 1) % z_info->r_max;
		if (i == 'a') r_ptr->x_attr = (byte)(ca + 1);
		if (i == 'A') r_ptr->x_attr = (byte)(ca - 1);
		if (i == 'c') r_ptr->x_char = (byte)(cc + 1);
		if (i == 'C') r_ptr->x_char = (byte)(cc - 1);
	}

	screen_load();

	return (FALSE);
}


/* Modify object attr/chars */
static bool do_cmd_change_object(int dummy)
{
	static int k = 0;
	
	char i;

	/* Hack - ignore parameters */
	(void) dummy;
	
	screen_save();
	
	Term_clear();
	
	/* Prompt */
	prtf(0, 5, "Command: Change object attr/chars");

	/* Hack -- query until done */
	while (1)
	{
		object_kind *k_ptr = &k_info[k];

		byte da = (byte)k_ptr->d_attr;
		byte dc = (byte)k_ptr->d_char;
		byte ca = (byte)k_ptr->x_attr;
		byte cc = (byte)k_ptr->x_char;

		/* Label the object */
		prtf(5, 7, "Object = %d, Name = %-40.40s",
						   k, (k_name + k_ptr->name));

		/* Label the Default values */
		prtf(10, 9, "Default attr/char = %3d / %3d", da, dc);
		put_fstr(40, 9, "<< ? >>");
		Term_putch(43, 9, da, dc);

		/* Label the Current values */
		prtf(10, 10, "Current attr/char = %3d / %3d", ca, cc);
		put_fstr(40, 10, "<< ? >>");
		Term_putch(43, 10, ca, cc);

		/* Prompt */
		prtf(0, 12, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') k = (k + z_info->k_max + 1) % z_info->k_max;
		if (i == 'N') k = (k + z_info->k_max - 1) % z_info->k_max;
		if (i == 'a') k_info[k].x_attr = (byte)(ca + 1);
		if (i == 'A') k_info[k].x_attr = (byte)(ca - 1);
		if (i == 'c') k_info[k].x_char = (byte)(cc + 1);
		if (i == 'C') k_info[k].x_char = (byte)(cc - 1);
	}	

	screen_load();
	
	return (FALSE);
}


/* Modify feature attr/chars */
static bool do_cmd_change_feature(int dummy)
{
	static int f = 0;
	
	char i;

	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();
	
	Term_clear();
	
	/* Prompt */
	prtf(0, 5, "Command: Change feature attr/chars");

	/* Hack -- query until done */
	while (1)
	{
		feature_type *f_ptr = &f_info[f];

		byte da = (byte)f_ptr->d_attr;
		byte dc = (byte)f_ptr->d_char;
		byte ca = (byte)f_ptr->x_attr;
		byte cc = (byte)f_ptr->x_char;

		/* Label the object */
		prtf(5, 7, "Terrain = %d, Name = %-40.40s",
						   f, (f_name + f_ptr->name));

		/* Label the Default values */
		prtf(10, 9, "Default attr/char = %3d / %3d", da, dc);
		put_fstr(40, 9, "<< ? >>");
		Term_putch(43, 9, da, dc);

		/* Label the Current values */
		prtf(10, 10, "Current attr/char = %3d / %3d", ca, cc);
		put_fstr(40, 10, "<< ? >>");
		Term_putch(43, 10, ca, cc);

		/* Prompt */
		prtf(0, 12, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') f = (f + z_info->f_max + 1) % z_info->f_max;
		if (i == 'N') f = (f + z_info->f_max - 1) % z_info->f_max;
		if (i == 'a') f_info[f].x_attr = (byte)(ca + 1);
		if (i == 'A') f_info[f].x_attr = (byte)(ca - 1);
		if (i == 'c') f_info[f].x_char = (byte)(cc + 1);
		if (i == 'C') f_info[f].x_char = (byte)(cc - 1);
	}

	screen_load();

	return (FALSE);
}


/* Modify field attr/chars */
static bool do_cmd_change_field(int dummy)
{
	static int f = 0;
	
	char i;
	
	/* Hack - ignore parameter */
	(void) dummy;

	screen_save();
	
	Term_clear();
	
	/* Prompt */
	prtf(0, 5, "Command: Change field attr/chars");

	/* Hack -- query until done */
	while (1)
	{
		field_thaum *t_ptr = &t_info[f];

		byte da = (byte)t_ptr->d_attr;
		byte dc = (byte)t_ptr->d_char;
		byte ca = (byte)t_ptr->f_attr;
		byte cc = (byte)t_ptr->f_char;

		/* Label the object */
		prtf(5, 7, "Field = %d, Name = %-40.40s", f, t_ptr->name);

		/* Label the Default values */
		prtf(10, 9, "Default attr/char = %3d / %3d", da, dc);
		put_fstr(40, 9, "<< ? >>");
		Term_putch(43, 9, da, dc);

		/* Label the Current values */
		prtf(10, 10, "Current attr/char = %3d / %3d", ca, cc);
		put_fstr(40, 10, "<< ? >>");
		Term_putch(43, 10, ca, cc);

		/* Prompt */
		prtf(0, 12, "Command (n/N/a/A/c/C): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') f = (f + z_info->t_max + 1) % z_info->t_max;
		if (i == 'N') f = (f + z_info->t_max - 1) % z_info->t_max;
		if (i == 'a') t_info[f].f_attr = (byte)(ca + 1);
		if (i == 'A') t_info[f].f_attr = (byte)(ca - 1);
		if (i == 'c') t_info[f].f_char = (byte)(cc + 1);
		if (i == 'C') t_info[f].f_char = (byte)(cc - 1);
	}

	screen_load();

	return (FALSE);
}

#endif /* ALLOW_VISUALS */


static bool do_cmd_reset_visuals(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Reset */
	reset_visuals();

	/* Message */
	msgf("Visual attr/char tables reset.");

	return (FALSE);
}


#ifdef ALLOW_VISUALS
#define VISUAL_MENU_MAX		11
#else
#define VISUAL_MENU MAX		3
#endif /* ALLOW_VISUALS */


/* The visuals menu */
static menu_type visuals_menu[VISUAL_MENU_MAX] =
{
	{"Load a user pref file", NULL, do_cmd_pref_vis_load, MN_ACTIVE},
#ifdef ALLOW_VISUALS
	{"Dump monster attr/chars", NULL, do_cmd_dump_monster, MN_ACTIVE},
	{"Dump object attr/chars", NULL, do_cmd_dump_object, MN_ACTIVE},
	{"Dump feature attr/chars", NULL, do_cmd_dump_feature, MN_ACTIVE},
	{"Dump field attr/chars", NULL, do_cmd_dump_field, MN_ACTIVE},
	{"Change monster attr/chars", NULL, do_cmd_change_monster, MN_ACTIVE | MN_CLEAR},
	{"Change object attr/chars", NULL, do_cmd_change_object, MN_ACTIVE | MN_CLEAR},
	{"Change feature attr/chars", NULL, do_cmd_change_feature, MN_ACTIVE | MN_CLEAR},
	{"Change field attr/chars", NULL, do_cmd_change_field, MN_ACTIVE | MN_CLEAR},
#endif /* ALLOW_VISUALS */
	{"Reset Visuals", NULL, do_cmd_reset_visuals, MN_ACTIVE},
	MENU_END
};


/*
 * Interact with "visuals"
 */
void do_cmd_visuals(void)
{
	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);
	
	display_menu(visuals_menu, -1, FALSE, NULL, "Interact with Visuals");
}


/*
 * Load a user pref file
 */
static bool do_cmd_pref_col_load(int dummy)
{
	char tmp[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();
	
	/* Prompt */
	prtf(0, 8, "Command: Load a user pref file\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Query */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}
	
	/* Process the given filename */
	if (0 != process_pref_file("%s", tmp))
	{
		/* Prompt */
		msgf("Could not load file!");
	}
	
	/* Mega-Hack -- react to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Mega-Hack -- redraw */
	Term_redraw();
	
	screen_load();
	return (FALSE);
}

#ifdef ALLOW_COLORS

static bool do_cmd_dump_colour(int dummy)
{
	int i;

	FILE *fff;

	char tmp[1024], buf[1024];

	/* Hack - ignore parameters */
	(void) dummy;
	
	screen_save();
	
	/* Prompt */
	prtf(0, 10, "Command: Dump colors\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Get a filename */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, tmp);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff)
	{
		screen_load();
		return (FALSE);
	}

	/* Start dumping */
	froff(fff, "\n\n");
	froff(fff, "# Color redefinitions\n\n");

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
		froff(fff, "# Color '%s'\n", name);

		/* Dump the monster attr/char info */
		froff(fff, "V:%d:0x%02X:0x%02X:0x%02X:0x%02X\n\n",
				i, (uint)kv, (uint)rv, (uint)gv, (uint)bv);
	}

	/* All done */
	froff(fff, "\n\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Message */
	msgf("Dumped color redefinitions.");

	screen_load();
	return (FALSE);
}


/*
 * Dump the message colours to a pref file
 */
static bool do_cmd_dump_message(int dummy)
{
	byte i;

	FILE *fff;

	char tmp[1024], buf[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();

	/* Prompt */
	prtf(0, 10, "Command: Dump message colors\n\n"
				"File: ");

	/* Default filename */
	strnfmt(tmp, 1024, "user-%s.prf", ANGBAND_SYS);

	/* Get a filename */
	if (!askfor_aux(tmp, 80))
	{
		screen_load();
		return (FALSE);
	}

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, tmp);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff)
	{
		screen_load();
		return (FALSE);
	}

	/* Start dumping */
	froff(fff, "\n\n");
	froff(fff, "# Message color definitions\n\n");

	/* Dump message colors */
	for (i = 0; i < MSG_MAX; i++)
	{
		byte color = get_msg_type_color(i);

		cptr name = "unknown";

		/* Extract the message type name */
		name = msg_names[i];

		/* Dump a comment */
		froff(fff, "# Message type: %s\n", name);

		/* Dump the message color */
		froff(fff, "M:%d:%c\n\n", i, color_char[color]);
	}

	/* All done */
	froff(fff, "\n\n\n\n");

	/* Close */
	my_fclose(fff);

	/* Message */
	msgf("Dumped message color definitions.");
	
	screen_load();
	return (FALSE);
}


static bool do_cmd_modify_colour(int dummy)
{
	static byte a = 0;
	
	int i;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	screen_save();
	
	clear_region(0, 9, 21);

	/* Prompt */
	prtf(5, 10, "Command: Modify colors");

	/* Hack -- query until done */
	while (1)
	{
		cptr name;
		byte j;

		/* Exhibit the normal colors */
		for (j = 0; j < 16; j++)
		{
			/* Exhibit this color */
			put_fstr(j * 4, 18, "%s###", color_seq[j]);

			/* Exhibit all colors */
			put_fstr(j * 4, 20, "%3d", j);
		}

		/* Describe the color */
		name = ((a < 16) ? color_names[a] : "undefined");

		/* Describe the color */
		prtf(5, 12, "Color = %d, Name = %s", a, name);

		/* Label the Current values */
		prtf(5, 14, "K = 0x%02x / R,G,B = 0x%02x,0x%02x,0x%02x",
						   angband_color_table[a][0],
						   angband_color_table[a][1],
						   angband_color_table[a][2],
						   angband_color_table[a][3]);

		/* Prompt */
		prtf(5, 16, "Command (n/N/k/K/r/R/g/G/b/B): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') a = (byte)(a + 1);
		if (i == 'N') a = (byte)(a - 1);
		if (i == 'k') angband_color_table[a][0] =
				(byte)(angband_color_table[a][0] + 1);
		if (i == 'K') angband_color_table[a][0] =
				(byte)(angband_color_table[a][0] - 1);
		if (i == 'r') angband_color_table[a][1] =
				(byte)(angband_color_table[a][1] + 1);
		if (i == 'R') angband_color_table[a][1] =
				(byte)(angband_color_table[a][1] - 1);
		if (i == 'g') angband_color_table[a][2] =
				(byte)(angband_color_table[a][2] + 1);
		if (i == 'G') angband_color_table[a][2] =
				(byte)(angband_color_table[a][2] - 1);
		if (i == 'b') angband_color_table[a][3] =
				(byte)(angband_color_table[a][3] + 1);
		if (i == 'B') angband_color_table[a][3] =
				(byte)(angband_color_table[a][3] - 1);

		/* Hack -- react to changes */
		Term_xtra(TERM_XTRA_REACT, 0);

		/* Hack -- redraw */
		Term_redraw();
	}
	
	screen_load();
	
	return (FALSE);
}


/*
 * Modify message colours
 */
static bool do_cmd_modify_message(int dummy)
{
	static byte a = 0;
	byte color;
	
	int i;

	/* Hack - ignore parameter */
	(void) dummy;

	screen_save();
	
	clear_region(0, 9, 17);
	
	/* Prompt */
	prtf(5, 10, "Command: Modify message colors");

	/* Hack -- query until done */
	while (1)
	{
		/* Describe the message */
		prtf(5, 12, "Message = %d, Type = %s", a, msg_names[a]);

		/* Show current color */
		color = get_msg_type_color(a);

		/* Paranoia */
		if (color >= 16) color = 0;

		prtf(5, 14, "Current color: %c / %s%s",
						color_char[color],
						color_seq[color],
						color_names[color]);

		/* Prompt */
		prtf(5, 16, "Command (n/N/c/C): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') a = (a + MSG_MAX + 1) % MSG_MAX;
		if (i == 'N') a = (a + MSG_MAX - 1) % MSG_MAX;
		if (i == 'c') message_color_define(a, (byte)(color + 1));
		if (i == 'C') message_color_define(a, (byte)(color - 1));
	}

	screen_load();

	return (FALSE);
}

#endif /* ALLOW_COLORS */


#ifdef ALLOW_COLORS
#define COLOR_MENU_MAX		6
#else
#define COLOR_MENU_MAX		2
#endif /* ALLOW_COLORS */


static menu_type color_menu[COLOR_MENU_MAX] =
{
	{"Load a user pref file", NULL, do_cmd_pref_col_load, MN_ACTIVE},
#ifdef ALLOW_COLORS
	{"Dump colours", NULL, do_cmd_dump_colour, MN_ACTIVE},
	{"Dump message colours", NULL, do_cmd_dump_message, MN_ACTIVE},
	{"Modify colours", NULL, do_cmd_modify_colour, MN_ACTIVE | MN_CLEAR},
	{"Modify message colours", NULL, do_cmd_modify_message, MN_ACTIVE | MN_CLEAR},
#endif /* ALLOW_COLORS */
	MENU_END
};

/*
 * Interact with "colors"
 */
void do_cmd_colors(void)
{
	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	display_menu(color_menu, -1, FALSE, NULL, "Interact with Colours");
}


/*
 * Take notes.
 */
void do_cmd_note(void)
{
	char buf[80];

	/* Default */
	buf[0] = 0;

	if (!get_string(buf, 60, "Note: ")) return;

	/* Ignore empty notes */
	if (!buf[0] || (buf[0] == ' ')) return;

	if (take_notes)
	{
		/* Add note to file */
		add_note(' ', buf);
	}
	else
	{
		/* Add note to message recall */
		msgf("Note: %s", buf);
	}
}


/*
 * Mention the current version
 */
void do_cmd_version(void)
{
	/* Silly message */
	msgf("You are playing " VERSION_NAME " " VERSION_STRING ".");
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
	/* Verify the feeling */
	if (p_ptr->state.feeling > 10) p_ptr->state.feeling = 10;

	if (p_ptr->place_num && !p_ptr->depth)
	{
		if (place[p_ptr->place_num].quest_num)
		{
			/* No useful feeling in a wilderness quest */
			msgf("Looks like a typical quest.");
		}
		else
		{
			/* No useful feeling in town */
			msgf("Looks like a typical town.");
		}

		return;
	}

	/* No useful feeling in the wilderness */
	if (!p_ptr->depth)
	{
		msgf("Looks like a typical wilderness.");
		return;
	}

	/* Display the feeling */
	if (turn - old_turn >= 1000)
	{
		msgf(do_cmd_feeling_text[p_ptr->state.feeling]);
	}
	else
	{
		msgf(do_cmd_feeling_text[0]);
	}
}


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

	char buf[1024];


	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, "dump.txt");

	/* Append to the file */
	fff = my_fopen(buf, "r");

	/* Oops */
	if (!fff) return;


	/* Save the screen */
	screen_save();

	/* Clear the screen */
	Term_clear();


	/* Load the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, 1024)) okay = FALSE;

		/* Show each row */
		for (x = 0; x < 79; x++)
		{
			/* Put the attr/char */
			Term_draw(x, y, TERM_WHITE, buf[x]);
		}
	}

	/* Get the blank line */
	if (my_fgets(fff, buf, 1024)) okay = FALSE;


	/* Dump the screen */
	for (y = 0; okay && (y < 24); y++)
	{
		/* Get a line of data */
		if (my_fgets(fff, buf, 1024)) okay = FALSE;

		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Look up the attr */
			for (i = 0; i < 16; i++)
			{
				/* Use attr matches */
				if (color_char[i] == buf[x]) a = i;
			}

			/* Hack -- fake monochrome */
			if (!use_color) a = TERM_WHITE;

			/* Put the attr/char */
			Term_draw(x, y, a, c);
		}

	}


	/* Get the blank line */
	if (my_fgets(fff, buf, 1024)) okay = FALSE;


	/* Close it */
	my_fclose(fff);


	/* Message */
	msgf("Screen dump loaded.");

	/* Restore the screen */
	screen_load();
}



/*
 * Redefinable "save_screen" action
 */
void (*screendump_aux) (void) = NULL;


/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
	/* Do we use a special screendump function ? */
	if (screendump_aux)
	{
		/* Dump the screen to a graphics file */
		(*screendump_aux) ();
	}
	else						/* Dump the screen as text */
	{
		int y, x;

		byte a = 0;
		char c = ' ';

		FILE *fff;

		char buf[1024];


		/* Build the filename */
		path_make(buf, ANGBAND_DIR_USER, "dump.txt");

		/* File type is "TEXT" */
		FILE_TYPE(FILE_TYPE_TEXT);

		/* Append to the file */
		fff = my_fopen(buf, "w");

		/* Oops */
		if (!fff) return;


		/* Save the screen */
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
			froff(fff, "%s\n", buf);
		}

		/* Skip a line */
		froff(fff, "\n");


		/* Dump the screen */
		for (y = 0; y < 24; y++)
		{
			/* Dump each row */
			for (x = 0; x < 79; x++)
			{
				/* Get the attr/char */
				(void)(Term_what(x, y, &a, &c));

				/* Dump it */
				buf[x] = color_char[a & 0x0F];
			}

			/* Terminate */
			buf[x] = '\0';

			/* End the row */
			froff(fff, "%s\n", buf);
		}

		/* Skip a line */
		froff(fff, "\n");


		/* Close it */
		my_fclose(fff);


		/* Message */
		msgf("Screen dump saved.");

		/* Restore the screen */
		screen_load();
	}
}


/*
 * Print a monster string, taking into account the strange
 * formatting of the '$' and '#' characters, as well as
 * doing colour correctly.
 */
static void print_monster_string(FILE *fff, byte a, char c, cptr name, int num)
{
	if (c == '$')
	{
		/* Hack - no unique coins */
		froff(fff, "  %s$$" CLR_WHITE "     %d pile of %s\n", color_seq[a], num, name);
	}
	else
	{
		if (num)
		{
			froff(fff, "  %s%c" CLR_WHITE "     %d %s\n", color_seq[a], c, num, name);
		}
		else
		{
			froff(fff, "  %s%c" CLR_WHITE "     %s\n", color_seq[a], c, name);
		}
	}
}


/*
 * Display known uniques
 */
static bool do_cmd_knowledge_uniques(int dummy)
{
	FILE *fff;

	char file_name[1024];

	int i, n, count_dead = 0;

	u16b why = 2;
	u16b *who;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights) continue;

		/* Require unique monsters if needed */
		if (!FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Collect "appropriate" monsters */
		who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* No monsters to recall */
		msgf("No known uniques.");

		/* XXX XXX Free the "who" array */
		KILL(who);

		return (FALSE);
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff)
	{
		/* XXX XXX Free the "who" array */
		KILL(who);

		return (FALSE);
	}

	/* Scan the monster races */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		if (r_ptr->max_num == 0)
		{
			/* Count the dead ones */
			count_dead++;

			/* Dead */
			print_monster_string(fff, r_ptr->d_attr, r_ptr->d_char,
				format(CLR_L_DARK "%s is dead.", mon_race_name(r_ptr)),
					 0);
		}
		else
		{
			/* Alive */
			print_monster_string(fff, r_ptr->d_attr, r_ptr->d_char, 
				format(CLR_L_BLUE "%s is alive.", mon_race_name(r_ptr)),
					0);
		}
	}

	/* Free the "who" array */
	KILL(who);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name,
					format("Known uniques, killed %d", count_dead),
					0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}

static const cptr plural_table[] =
{
	"ex", "ices",
	"ey", "eys",
	"y", "ies",
	"human", "humans",
	"shaman", "shamans",
	"man", "men",
	"ouse", "ice",
	"ff", "ffs",
	"f", "ves",
	"mage", "magi",
	"eraph", "eraphim",
	"umak", "umakil",
	"saurus", "saurs",
    "neko", "neko",
    "engu", "engu",
	"us", "i",
	"s", "ses",
	"x", "xes",
	"sh", "shes",
	"ch", "ches",
	/* This last entry must match everything */
	"", "s"
};

/*
 * Pluralize a monster name
 *
 * (Assume name[] is at least 80 chars long)
 */
void plural_aux(char *name)
{
	char *p;
	char buf[80];
	char tail[80];
	int len = strlen(name);
	int i;

	/* Don't overflow the buffer */
	if (len > 70) return;

	strcpy(buf, name);

	/* Total hack - handle Creeping coins */
	if (len >= 6 && streq(buf + len - 6, " coins"))
	{
		strnfmt(buf, 80, "piles of %s", name);
		strcpy(name, buf);
		return;
	}
	
	tail[0] = '\0';

	/* Find the trailing part we should ignore, if any */
	p = strstr(buf, " out ");
	if (!p) p = strstr(buf, " of ");
	if (!p) p = strstr(buf, " that ");
	if (!p) p = strstr(buf, " on ");
	if (!p) p = strstr(buf, " to ");
	if (p)
	{
		strcpy(tail, p);
		*p = '\0';
		len = strlen(buf);
	}

	/* Find the appropriate plural */
	for (i = 0;; i += 2)
	{
		if ((len >= (int)strlen(plural_table[i])) &&
			streq(buf + len - strlen(plural_table[i]), plural_table[i]))
		{
			/* Preterminate string */
			buf[len - strlen(plural_table[i])] = '\0';
			
			/* Pluralise */
			strnfmt(name, 80, "%s%s%s", buf, plural_table[i + 1], tail);
			return;
		}
	}
	
	/* Paranoia */
	quit("Failed to find matching plural in plural_aux()");
}


/*
 * Display current pets
 */
bool do_cmd_knowledge_pets(int dummy)
{
	int i;
	FILE *fff;
	monster_type *m_ptr;
	int t_friends = 0;
	int t_levels = 0;
	int show_upkeep = 0;
	char file_name[1024];

	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);
	
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
			t_friends++;
			t_levels += r_info[m_ptr->r_idx].level;
			froff(fff, "%v (%s)\n", MONSTER_FMT(m_ptr, 0x88), look_mon_desc(i));
		}
	}

	if (t_friends > 1 + (p_ptr->lev / (cp_ptr->pet_upkeep_div)))
	{
		show_upkeep = t_levels;

		if (show_upkeep > 95) show_upkeep = 95;
		else if (show_upkeep < 5) show_upkeep = 5;
	}


	froff(fff, "----------------------------------------------\n");
	froff(fff, "   Total: %d pet%s.\n",
			t_friends, (t_friends == 1 ? "" : "s"));
	froff(fff, "   Upkeep: %d%% mana.\n", show_upkeep);


	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Current Pets", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}


/*
 * Total kill count
 */
static bool do_cmd_knowledge_kill_count(int dummy)
{
	FILE *fff;

	char file_name[1024];

	u32b total = 0;
	u32b temp;

	int i, n;

	u16b why = 2;
	u16b *who;
	
	int kk;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights) continue;

		/* Collect "appropriate" monsters */
		who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* No monsters to recall */
		msgf("No known monsters!");

		/* XXX XXX Free the "who" array */
		KILL(who);

		return (FALSE);
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff)
	{
		/* XXX XXX Free the "who" array */
		KILL(who);

		return (FALSE);
	}


	/* Monsters slain */
	for (kk = 1; kk < z_info->r_max; kk++)
	{
		monster_race *r_ptr = &r_info[kk];

		if (FLAG(r_ptr, RF_UNIQUE))
		{
			bool dead = (r_ptr->max_num == 0);

			if (dead)
			{
				total++;
			}
		}
		else
		{
			s16b this = r_ptr->r_pkills;

			if (this > 0)
			{
				total += this;
			}
		}
	}

	if (total < 1)
		froff(fff, "You have defeated no enemies yet.\n\n");
	else if (total == 1)
		froff(fff, "You have defeated one enemy.\n\n");
	else
		froff(fff, "You have defeated %lu enemies.\n\n", total);

	/* Save total kills for later */
	temp = total;

	/* Zero out total so we can calculate kills of known monsters */
	total = 0;

	/* Scan the monster races */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		if (FLAG(r_ptr, RF_UNIQUE))
		{
			bool dead = (r_ptr->max_num == 0);

			if (dead)
			{
				print_monster_string(fff, r_ptr->d_attr, r_ptr->d_char, mon_race_name(r_ptr), 0);
				total++;
			}
		}
		else
		{
			s16b this = r_ptr->r_pkills;

			if (this > 0)
			{
				if (this < 2)
				{
					print_monster_string(fff, r_ptr->d_attr, r_ptr->d_char, mon_race_name(r_ptr), 1);
				}
				else
				{
					char ToPlural[80];
					strcpy(ToPlural, mon_race_name(r_ptr));
					plural_aux(ToPlural);
					print_monster_string(fff, r_ptr->d_attr, r_ptr->d_char, ToPlural, this);
				}

				total += this;
			}
		}
	}

	froff(fff, "----------------------------------------------\n");
	froff(fff, "   Total: %lu creature%s killed.\n",
			total, (total == 1 ? "" : "s"));

	/* Subtract off monsters you know you have killed */
	temp -= total;

	/* Have we killed any monsters we did not see? */
	if (temp)
	{
		froff(fff, "\n");
		froff(fff, " Unseen: %lu creature%s killed.\n",
				temp, (temp == 1 ? "" : "s"));
	}

	/* Free the "who" array */
	KILL(who);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Kill Count", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}


/*
 * Display known objects
 */
static bool do_cmd_knowledge_objects(int dummy)
{
	int k;

	FILE *fff;

	char file_name[1024];
	
	byte a;
	char c;
	cptr attr;
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);

	/* Scan the object kinds */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Hack -- skip artifacts */
		if (FLAG(k_ptr, TR_INSTA_ART)) continue;

		/* List known flavored objects */
		if (k_ptr->flavor && k_ptr->aware)
		{
			object_type *o_ptr;

			/* Create fake object */
			o_ptr = object_prep(k);
			
			attr = color_seq[tval_to_attr[o_ptr->tval % 128]];
			
			a = object_attr(o_ptr);
			c = object_char(o_ptr);
			
			/* Only add equippys if in ascii mode */
			if (!(a & 0x80) && !(c & 0x80))
			{
				if (c == '$')
				{
					/* Print a message ('$' needs to be escaped) */
					froff(fff, " %s$$%s  %v\n", color_seq[a], attr, OBJECT_STORE_FMT(o_ptr, FALSE, 0));
				}
				else
				{
					/* Print a message */
					froff(fff, " %s%c%s  %v\n", color_seq[a], c, attr, OBJECT_STORE_FMT(o_ptr, FALSE, 0));
				}
			}
			else
			{
				/* Print a message */
					froff(fff, "  %s  %v\n", attr, OBJECT_STORE_FMT(o_ptr, FALSE, 0));
			}
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Known Objects", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}


/*
 * List virtues & status
 */
static bool do_cmd_knowledge_virtues(int dummy)
{
	FILE *fff;

	char file_name[1024];
	
	/* Hack -ignore parameter */
	(void) dummy;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);
	
	dump_virtues(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Virtues", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}


/*
 * Print notes file
 */
static bool do_cmd_knowledge_notes(int dummy)
{
	char fname[1024];
	
	/* Hack - ignore parameter */
	(void) dummy;

	strncpy(fname, notes_file(), 1024);
	
	(void)show_file(fname, "Notes", 0, 0);
	
	return (FALSE);
}


/*
 * Dump info about a town to the given file
 */
void dump_town_info(FILE *fff, int town, bool ignore)
{
	int j;

	cptr build_name;
	
	char c;
	byte a;

	bool visited = FALSE;

	place_type *pl_ptr = &place[town];

	/* Is it a town? */
	if (!pl_ptr->quest_num)
	{
		/* Hack-- determine the town has been visited */
		visited = FALSE;

		for (j = 0; j < pl_ptr->numstores; j++)
		{
			/* Stores are not given coordinates until you visit a town */
			if ((pl_ptr->store[j].x != 0) && (pl_ptr->store[j].y != 0))
			{
				visited = TRUE;
				break;
			}
		}

		/* Build a buffer with the information (If visited, and if it is a town) */
		if (visited)
		{
			/* write stairs information to file */
			if (pl_ptr->dungeon)
			{
				froff(fff, "%s -- Stairs\n", pl_ptr->name);
			}
			else
			{
				froff(fff, "%s\n", pl_ptr->name);
			}

			/* Built stores information */
			for (j = 0; j < pl_ptr->numstores; j++)
			{
				build_name = building_name(pl_ptr->store[j].type);

				/* Make a string, but only if this is a real building */
				if (!streq(build_name, "Nothing"))
				{
					/* Get attr/char */
					building_char(pl_ptr->store[j].type, &a, &c);
					
					/* Only draw symbols in ascii mode */
					if (!(a & 0x80) && !(c & 0x80))
					{				
						/* Append information about store */
						froff(fff, "  %s%c" CLR_WHITE "   %s\n", color_seq[a], c, build_name);
					}
					else
					{
						/* Append information about store */
						froff(fff, "      %s\n", build_name);
					}
				}
			}

			/* Seperator */
			froff(fff, "\n");
		}

		/* Never been near the place */
		else
		{
			/* Give an empty message or no message */
			if (!ignore) froff(fff, "\nThis town has not been visited yet.\n");
		}
	}
}


/*
 * Dump info about a place if is has a dungeon to the given file
 */
static void dump_dungeon_info(FILE *fff, int town)
{
	int i;

	bool visited = FALSE;
	cptr place_name, place_dir;
	int depth;
	int x, y, count = 0;

	place_type *p2_ptr, *pl_ptr = &place[town];
	dun_type *d_ptr = pl_ptr->dungeon;
	wild_done_type *w_ptr;

	/* A place without a dungeon */
	if (!d_ptr) return;

	/* Get a shorthand */
	depth = d_ptr->recall_depth;

	/* Is it a town? */
	if (pl_ptr->numstores)
	{
		/* Hack-- determine the town has been visited */
		visited = FALSE;

		for (i = 0; i < pl_ptr->numstores; i++)
		{
			/* Stores are not given coordinates until you visit a town */
			if ((pl_ptr->store[i].x != 0) && (pl_ptr->store[i].y != 0))
			{
				visited = TRUE;
				break;
			}
		}

		/* Build a buffer with the information (If visited, and if it is a town) */
		if (visited)
		{
			/* Give the dungeon name and location*/
			froff(fff, "%s dungeon under %s",
				dungeon_type_name(d_ptr->habitat), pl_ptr->name);
		}
		else
		{
			/* Don't show this */
			return;
		}
	}
	/* So it is a dungeon */
	else
	{
		/* Fetch closest known town and direction */
		place_name = describe_quest_location(&place_dir,
						pl_ptr->x, pl_ptr->y, TRUE);

		/* Check a piece of the map */
		for (x = 0; x < 3; x++)
		{
			for (y = 0; y < 5; y++)
			{
				/* Pick up a spot on the map */
				w_ptr = &wild[pl_ptr->y + y][pl_ptr->x + x].done;

				/* Pick up the place associated with this spot */
				p2_ptr = (w_ptr->place) ? &place[w_ptr->place] : NULL;

				/* Does this spot contain a place? */
				if (!p2_ptr) continue;

				/* Has this spot been seen? */
				if (!(w_ptr->info & WILD_INFO_SEEN)) continue;
				
				/* Is this place the same as the one that we started with? */
				if (p2_ptr == pl_ptr) count++;
			}
		}

		/* Skip if the dungeon is unknown */
		if (!count) return;

		/* Give the dungeon name and location*/
		froff(fff, "%s dungeon %s of %s",
			dungeon_type_name(d_ptr->habitat), place_dir, place_name);

		/* Did the player go into the dungeon? */
		if (!depth)
		{
			/* It is still guarded by monsters */
			froff(fff, ", guarded");
		}
	}

	/* If the dungeon was attempted, show the depth */
	if (depth)
	{
		/* Show the depth reached */
		if (depth_in_feet)
		{
			if (depth == d_ptr->min_level)
				froff(fff, ", %d feet", depth * 50);
			else
				froff(fff, ", %d - %d feet", d_ptr->min_level * 50, depth * 50);
		}
		else
		{
			if (depth == d_ptr->min_level)
				froff(fff, ", level %d", depth);
			else
				froff(fff, ", level %d - %d", d_ptr->min_level, depth);
		}

		/* All the way down? */
		if (depth == d_ptr->max_level)
		{
			froff(fff, " (bottom)");
		}
	}

	/* Is the player in this dungeon? */
	if (p_ptr->place_num == town && p_ptr->depth)
	{
		froff(fff, ", current.\n\n\n");
	}
	else
	{
		froff(fff, ".\n\n");
	}
}


/*
 * Display information about wilderness areas
 */
static bool do_cmd_knowledge_wild(int dummy)
{
	int k;

	FILE *fff;

	char file_name[1024];
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);
	
	/* Cycle through the places */
	for (k = 1; k < place_count; k++)
	{
		dump_town_info(fff, k, TRUE);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Towns", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}


/*
 * Display information about wilderness areas
 */
static bool do_cmd_knowledge_dungeon(int dummy)
{
	int k;

	FILE *fff;

	char file_name[1024];
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);
	
	/* Cycle through the places */
	for (k = 1; k < place_count; k++)
	{
		dump_dungeon_info(fff, k);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Dungeons", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}


/* Some gaps for options that should not show up always */
static menu_type knowledge_menu[15] =
{
	{"Display known uniques", NULL, do_cmd_knowledge_uniques, MN_ACTIVE | MN_CLEAR},
	{"Display known objects", NULL, do_cmd_knowledge_objects, MN_ACTIVE | MN_CLEAR},
	{"Display kill count", NULL, do_cmd_knowledge_kill_count, MN_ACTIVE | MN_CLEAR},
	{"Display mutations", NULL, do_cmd_knowledge_mutations, MN_ACTIVE | MN_CLEAR},
	{"Display current pets", NULL, do_cmd_knowledge_pets, MN_ACTIVE | MN_CLEAR},
	{"Display current quests", NULL, do_cmd_knowledge_quests, MN_ACTIVE | MN_CLEAR},
	MENU_END,
	MENU_END,
	MENU_END,
	MENU_END,
	{"Display virtues", NULL, do_cmd_knowledge_virtues, MN_ACTIVE | MN_CLEAR},
	{"Display notes", NULL, do_cmd_knowledge_notes, MN_ACTIVE | MN_CLEAR},
	{"Display towns", NULL, do_cmd_knowledge_wild, MN_ACTIVE | MN_CLEAR},
	{"Display dungeons", NULL, do_cmd_knowledge_dungeon, MN_ACTIVE | MN_CLEAR},
	MENU_END
};


/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
	int nr, last_option = 6;

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* start at the first free spot */
	nr = last_option;

	/*
	 * Display virtues option is always left out
	 * if (use_virtues) knowledge_menu[nr++] = knowledge_menu[10];
	 */
	
	/* Copy in the display notes */
	if (take_notes) knowledge_menu[nr++] = knowledge_menu[11];

	/* Copy in the wilderness displays */
	if (!vanilla_town)
	{
		knowledge_menu[nr++] = knowledge_menu[12];
		knowledge_menu[nr++] = knowledge_menu[13];
	}
	
	/* Display the menu */
	display_menu(knowledge_menu, -1, FALSE, NULL, "Display current knowledge");

	/* Clear these options again */
	for (; nr >= last_option; nr--)
	{
		/* menu item 14 contains a MENU_END */
		knowledge_menu[nr] = knowledge_menu[14];
	}
}


/*
 * Check on the status of an active quest
 */
void do_cmd_checkquest(void)
{
	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Quest info */
	(void) do_cmd_knowledge_quests(0);
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
	msgf("This is day %d. The time is %d:%02d %s.",
			   day, (hour % 12 == 0) ? 12 : (hour % 12),
			   min, (hour < 12) ? "AM" : "PM");

	/* Find the path */
	if (one_in_(10) || p_ptr->tim.image)
	{
		path_make(buf, ANGBAND_DIR_FILE, "timefun.txt");
	}
	else
	{
		path_make(buf, ANGBAND_DIR_FILE, "timenorm.txt");
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
			if (one_in_(num)) strcpy(desc, buf + 2);

			/* Next... */
			continue;
		}
	}

	/* Message */
	msgf(desc);

	/* Close the file */
	my_fclose(fff);
}
