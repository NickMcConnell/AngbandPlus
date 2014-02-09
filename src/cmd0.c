/*
 * File: cmd0.c
 * Purpose: Deal with command processing.
 *
 * Copyright (c) 2007 Andrew Sidwell, Ben Harrison, Jeff Greene
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "cmds.h"
#include "ui-menu.h"
#include "game-cmd.h"

/*
 * This file contains (several) big lists of commands, so that they can be
 * easily manipulated for e.g. help displays, or if a port wants to provide a
 * native menu containing a command list.
 *
 * Consider a two-paned layout for the command menus. XXX
 *
 * This file still needs some clearing up. XXX
 */

/*** Big list of commands ***/

/* Useful typedef */
typedef void do_cmd_type(void);


/* Forward declare these, because they're really defined later */
static do_cmd_type do_cmd_wizard, do_cmd_try_debug,
			do_cmd_mouseclick, do_cmd_port,
			do_cmd_xxx_options, do_cmd_menu, do_cmd_monlist, do_cmd_itemlist;


/*
 * Holds a generic command - if cmd is set to other than CMD_NULL
 * it simply pushes that command to the game, otherwise the hook
 * function will be called.
 */
typedef struct
{
	const char *desc;
	unsigned char key;
	cmd_code cmd;
	do_cmd_type *hook;
} command_type;


/* Magic use */
static command_type cmd_magic[] =
{
	{ "Gain new spells, incantations, or prayers", 'G', CMD_NULL, textui_cmd_study },
	{ "Browse a book",              'b', CMD_NULL, do_cmd_browse },
	{ "Cast a spell",               'm', CMD_NULL, textui_cmd_cast },
	{ "Pray a prayer",              'p', CMD_NULL, textui_cmd_pray }
};

/* General actions */
static command_type cmd_action[] =
{
	{ "Search for traps/doors",     's', CMD_SEARCH, NULL },
	{ "Disarm a trap or chest",     'D', CMD_NULL, textui_cmd_disarm },
	{ "Rest for a while",           'R', CMD_NULL, textui_cmd_rest },
	{ "Look around",                'l', CMD_NULL, do_cmd_look },
	{ "Target monster or location", '*', CMD_NULL, do_cmd_target },
	{ "Target closest monster",     '\'', CMD_NULL, do_cmd_target_closest },
	{ "Dig a tunnel",               'T', CMD_NULL, textui_cmd_tunnel },
	{ "Go up staircase",            '<', CMD_GO_UP, NULL },
	{ "Go down staircase",          '>', CMD_GO_DOWN, NULL },
	{ "Toggle search mode",         'S', CMD_TOGGLE_SEARCH, NULL },
	{ "Open a door or a chest",     'o', CMD_NULL, textui_cmd_open },
	{ "Close a door",               'c', CMD_NULL, textui_cmd_close },
	{ "Jam a door shut",            'j', CMD_NULL, textui_cmd_spike },
	{ "Bash a monster or door",	    'B', CMD_NULL, textui_cmd_bash },
	{ "Make a monster trap",		'O', CMD_NULL, textui_cmd_make_trap},
	{ "Steal from a monster",		'P', CMD_NULL, textui_cmd_steal}
};


/* Item use commands */
static command_type cmd_item_use[] =
{
	{ "Read a scroll",            'r', CMD_NULL, textui_cmd_read_scroll },
	{ "Quaff a potion",           'q', CMD_NULL, textui_cmd_quaff_potion },
	{ "Use a staff",              'u', CMD_NULL, textui_cmd_use_staff },
	{ "Aim a wand",               'a', CMD_NULL, textui_cmd_aim_wand },
	{ "Zap a rod",                'z', CMD_NULL, textui_cmd_zap_rod },
	{ "Activate an object",       'A', CMD_NULL, textui_cmd_activate },
	{ "Eat some food",            'E', CMD_NULL, textui_cmd_eat_food },
	{ "Fuel your light source",   'F', CMD_NULL, textui_cmd_refill },
	{ "Fire your missile weapon", 'f', CMD_NULL, textui_cmd_fire },
	{ "Fire at nearest target",   'h', CMD_NULL, textui_cmd_fire_at_nearest },
	{ "Throw an item",            'v', CMD_NULL, textui_cmd_throw },
	{ "Use item",            	  '|', CMD_NULL, cmd_use_item  }
};

/* Item management commands */
static command_type cmd_item_manage[] =
{
	{ "Display equipment listing", 'e', CMD_NULL, do_cmd_equip },
	{ "Display inventory listing", 'i', CMD_NULL, do_cmd_inven },
	{ "Pick up objects",           'g', CMD_PICKUP, NULL },
	{ "Wear/wield an item",        'w', CMD_NULL, textui_cmd_wield },
	{ "Swap weapons",              'x', CMD_NULL, textui_cmd_swap_weapon },
	{ "Take/unwield off an item",  't', CMD_NULL, textui_cmd_takeoff },
	{ "Drop an item",              'd', CMD_NULL, textui_cmd_drop },
	{ "Destroy an item",           'k', CMD_NULL, textui_cmd_destroy },
	{ "Examine an item",           'I', CMD_NULL, do_cmd_observe },
	{ "Inscribe an object",        '{', CMD_NULL, textui_cmd_inscribe },
	{ "Uninscribe an object",      '}', CMD_NULL, textui_cmd_uninscribe },
	{ "Use item",            	   '|', CMD_NULL, cmd_use_item }

};

/* Information access commands */
static command_type cmd_info[] =
{
	{ "Full dungeon map",             'M', CMD_NULL, do_cmd_view_map },
	{ "Display visible item list",    ']', CMD_NULL, do_cmd_itemlist },
	{ "Display visible monster list", '[', CMD_NULL, do_cmd_monlist },
	{ "Locate player on map",         'L', CMD_NULL, do_cmd_locate },
	{ "Help",                         '?', CMD_NULL, do_cmd_help },
	{ "Identify symbol",              '/', CMD_NULL, do_cmd_query_symbol },
	{ "Character description",        'C', CMD_NULL, do_cmd_change_name },
	{ "Check knowledge",              '~', CMD_NULL, do_cmd_knowledge },
	{ "Check quest",   	        KTRL('Q'), CMD_NULL, do_cmd_quest },
	{ "Repeat level feeling",   KTRL('F'), CMD_NULL, do_cmd_feeling },
	{ "Show previous message",  KTRL('O'), CMD_NULL, do_cmd_message_one },
	{ "Show previous messages", KTRL('P'), CMD_NULL, do_cmd_messages }
};

/* Utility/assorted commands */
static command_type cmd_util[] =
{
	{ "Interact with options",        '=', CMD_NULL, do_cmd_xxx_options },
	{ "Port-specific preferences",    '!', CMD_NULL, do_cmd_port },

	{ "Save and don't quit",  KTRL('S'), CMD_SAVE, NULL },
	{ "Save and quit",        KTRL('X'), CMD_QUIT, NULL },
	{ "Quit (commit suicide)",      'Q', CMD_NULL, textui_cmd_suicide },
	{ "Redraw the screen",    KTRL('R'), CMD_NULL, do_cmd_redraw },

	{ "Load \"screen dump\"",       '(', CMD_NULL, do_cmd_load_screen },
	{ "Save \"screen dump\"",       ')', CMD_NULL, do_cmd_save_screen }
};


/* Commands that shouldn't be shown to the user */
static command_type cmd_hidden[] =
{
	{ "Take notes",               ':', CMD_NULL, do_cmd_dictate_note},
	{ "Version info",             'V', CMD_NULL, do_cmd_version },
	{ "Load a single pref line",  '"', CMD_NULL, do_cmd_pref },
	{ "Mouse click",      DEFINED_XFF, CMD_NULL, do_cmd_mouseclick },
	{ "Enter a store",            '_', CMD_ENTER_STORE, NULL },
	{ "Toggle windows",     KTRL('E'), CMD_NULL, toggle_inven_equip }, /* XXX */
	{ "Alter a grid",             '+', CMD_NULL, textui_cmd_alter },
	{ "Walk",                     ';', CMD_NULL, textui_cmd_walk },
	{ "Jump into a trap",         '-', CMD_NULL, textui_cmd_jump },
	{ "Start running",            '.', CMD_NULL, textui_cmd_run },
	{ "Stand still",              ',', CMD_HOLD, NULL },
	{ "Check knowledge",          '~', CMD_NULL, do_cmd_knowledge },
	{ "Display menu of actions", KTRL('H'), CMD_NULL, do_cmd_menu },
	{ "Center map",              KTRL('L'), CMD_NULL, do_cmd_center_map },

	{ "Toggle wizard mode",  KTRL('W'), CMD_NULL, do_cmd_wizard },
	{ "Repeat previous command",  'n', CMD_REPEAT, NULL },

#ifdef ALLOW_DEBUG
	{ "Debug mode commands", KTRL('A'), CMD_NULL, do_cmd_try_debug },
#endif
};


/*
 * A categorised list of all the command lists.
 */
typedef struct
{
	const char *name;
	command_type *list;
	size_t len;
} command_list;

static command_list cmds_all[] =
{
	{ "Use magic/Pray",  cmd_magic,       N_ELEMENTS(cmd_magic) },
	{ "Action commands", cmd_action,      N_ELEMENTS(cmd_action) },
	{ "Use item",        cmd_item_use,    N_ELEMENTS(cmd_item_use) },
	{ "Manage items",    cmd_item_manage, N_ELEMENTS(cmd_item_manage) },
	{ "Information",     cmd_info,        N_ELEMENTS(cmd_info) },
	{ "Utility",         cmd_util,        N_ELEMENTS(cmd_util) },
	{ "Hidden",          cmd_hidden,      N_ELEMENTS(cmd_hidden) }
};


/**
 * Menu functions
 */
char comm[22];
cptr comm_descr[22];
int poss;


/**
 * Item tag/command key
 */
static char show_tag(menu_type *menu, int oid)
{
	/* Caution - could be a problem here if KTRL commands were used */
	return comm[oid];
}


/**
 * Display an entry on a command menu
 */
static void show_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	/* Write the description */
	Term_putstr(col + 4, row, -1, attr, comm_descr[oid]);
}


/**
 * Handle user input from a command menu
 */
static bool show_action(char cmd, void *db, int oid)
{
	/* Handle enter and mouse */
	if (cmd == '\n' || cmd == '\r' || cmd == '\xff')
	{
		p_ptr->command_new = comm[oid];
	}
	return TRUE;
}


/**
 * Display a list of commands.
 */
static void show_cmd_menu(void)
{
	menu_type menu;
	menu_iter commands_menu = {show_tag, NULL, show_display, show_action };
	region area = { 0, 1, 20, -1};

	int cursor = 0;

	/* Size of menu */
	area.page_rows = poss + 1;

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = "\x8B\x8C\n\r";
	menu.count = poss;
	menu.menu_data = comm;
	menu_init(&menu, MN_SKIN_SCROLL, &commands_menu, &area);

	/* Select an entry */
	(void) menu_select(&menu, &cursor, 0);
}


/*
 * Figure out which row of the sidebar was clicked.
 * These are not constant as the function update_sidebar
 * will change what goes on each row depending on
 * the screen height.
 */
static int sidebar_click(int row)
{
	int i;

	/* Hack - special handling for the stat section */
	if ((row >= sidebar_details[SIDEBAR_STAT_MIN]) &&
		(row <= sidebar_details[SIDEBAR_STAT_MAX])) return SIDEBAR_STAT_MIN;

	/* Hack - special handling for the monster list section */
	if ((row >= sidebar_details[SIDEBAR_MON_MIN]) &&
			(row <= sidebar_details[SIDEBAR_MON_MAX])) return SIDEBAR_MON_MIN;

	for (i=0; i < SIDEBAR_MAX_TYPES; i++)
	{
		if (row == sidebar_details[i]) return (i);
	}

	/* Nothing on this row */
	return (MOUSE_NULL);
}


/**
 * Bring up player actions
 */
static void show_commands(void)
{
	int i;
	int py = p_ptr->py;
	int px = p_ptr->px;

	bool nearby_closed_door = FALSE;
	bool nearby_open_door = FALSE;
	bool nearby_trap = FALSE;
	bool nearby_player_trap = FALSE;
	bool nearby_monster = FALSE;
	bool nearby_chest = FALSE;
	bool nearby_trapped_chest = FALSE;
	bool can_steal = FALSE;
	bool can_set_trap = FALSE;
	bool can_tunnel = FALSE;
	bool can_bash = FALSE;
	bool can_spike = FALSE;

	/* Start with no commands */
	poss = 0;

	/* Count_chests looks at all nearby squares */
	if (count_chests(&py, &px, FALSE))
	{
		nearby_chest = TRUE;

		/* reset location (count_chests may have moved it */
		py = p_ptr->py;
		px = p_ptr->px;

		if (count_chests(&py, &px, TRUE)) nearby_trapped_chest = TRUE;

		/* reset location (count_chests may have moved it */
		py = p_ptr->py;
		px = p_ptr->px;
	}

	/* Check the sourrounding squares for their contents */
	for (i = 0; i < 8; i++)
	{
		int y = py + ddy_ddd[i];
		int x = px + ddx_ddd[i];

		if (!in_bounds_fully(y, x)) continue;

		if (cave_any_trap_bold(y, x))
		{
			nearby_trap = TRUE;
			if (cave_player_trap_bold(y, x)) 	nearby_player_trap = TRUE;
		}

		if (cave_known_closed_door(y, x))	nearby_closed_door = TRUE;
		if (cave_open_door(y, x))			nearby_open_door = TRUE;
		if (cave_ff1_match(y, x, (FF1_DOOR | FF1_CAN_TUNNEL)) == (FF1_CAN_TUNNEL))
		{
			can_tunnel = TRUE;
		}
		if (cave_m_idx[y][x] > 0)
		{
			nearby_monster = TRUE;
			can_bash = TRUE;
			if (cp_ptr->flags & CF_ROGUE_COMBAT) can_steal = TRUE;
		}
		else if (cave_passable_bold(y,x))
		{
			if (cp_ptr->flags & CF_ROGUE_COMBAT) can_set_trap = TRUE;
		}
		if (do_cmd_test(y, x, FS_BASH, FALSE)) can_bash = TRUE;
		if (do_cmd_test(y, x, FS_SPIKE, FALSE)) can_spike = TRUE;
	}

	/* Alter a grid */
	if (nearby_monster || can_tunnel || nearby_closed_door || nearby_trap)
	{
		comm[poss] = '+';
		comm_descr[poss++] = "Alter";
	}

	/* Dig a tunnel */
	if (can_tunnel)
	{
		comm[poss] = 'T';
		comm_descr[poss++] = "Tunnel";
	}
	/* Begin Running -- Arg is Max Distance */
	comm[poss] = '.';
	comm_descr[poss++] = "Run";

	/* Hold still for a turn.  Pickup objects if auto-pickup is true. */
	comm[poss] = ',';
	comm_descr[poss++] = "Stand still";

	comm[poss] = 'e';
	comm_descr[poss++] = "Equipment";

	comm[poss] = 'i';
	comm_descr[poss++] = "Inventory";

	/* Pick up objects. */
	if (cave_o_idx[p_ptr->py][p_ptr->px])
	{
		comm[poss] = 'g';
		comm_descr[poss++] = "Pick up";
	}

	/* Rest -- Arg is time */
	comm[poss] = 'R';
	comm_descr[poss++] = "Rest";

	/* Search for traps/doors */
	if (!p_ptr->searching)
	{
		comm[poss] = 's';

		comm_descr[poss++] = "Search";
	}

	/* Look around */
	comm[poss] = 'l';
	comm_descr[poss++] = "Look";

	/* Scroll the map */
	comm[poss] = 'L';
	comm_descr[poss++] = "Scroll map";

	/* Show the map */
	comm[poss] = 'M';
	comm_descr[poss++] = "Level map";

	/* Knowledge */
	comm[poss] = '~';
	comm_descr[poss++] = "Knowledge";

	/* Options */
	comm[poss] = '=';
	comm_descr[poss++] = "Options";

	/* Options */
	comm[poss] = ':';
	comm_descr[poss++] = "Take note";

	/* Toggle search mode */
	comm[poss] = 'S';
	comm_descr[poss++] = "Toggle searching";

	/* Go up staircase */
	if (cave_stair_bold(py, px))
	{
		if (cave_up_stairs(py, px))
		{
			comm[poss] = '<';
			if cave_shaft_bold(py, px)	comm_descr[poss++] = "Go up shaft";
			else comm_descr[poss++] = "Go up staircase";
		}
		else
		{
			comm[poss] = '>';
			if cave_shaft_bold(py, px)	comm_descr[poss++] = "Go down shaft";
			else comm_descr[poss++] = "Go down staircase";
		}
	}

	/* Disarm a trap or chest */
	if (nearby_trapped_chest || nearby_trap)
	{
		comm[poss] = 'D';
		comm_descr[poss++] = "Disarm";
	}

	/* Open a door or chest */
	if (nearby_closed_door || nearby_chest)
	{
		comm[poss] = 'o';
		comm_descr[poss++] = "Open";
	}

	/* Close a door */
	if (nearby_open_door)
	{
		comm[poss] = 'c';
		comm_descr[poss++] = "Close";
	}

	/* Jam a door with spikes,or bash it */
	if (can_bash)
	{
		comm[poss] = 'B';
		comm_descr[poss++] = "Bash";
	}

    /* Jam a door with spikes,or bash it */
	if (can_spike)
	{
		comm[poss] = 'j';
		comm_descr[poss++] = "Jam";
	}

	/* Jam a door with spikes,or bash it */
	if (can_set_trap)
	{
		comm[poss] = 'o';
		if ((nearby_player_trap) && (cp_ptr->flags & CF_ROGUE_COMBAT))
		{
			comm_descr[poss++] = "Set or Improve Trap";
		}
		else comm_descr[poss++] = "Set Trap";
	}
	if (can_steal)
	{
		comm[poss] = 'P';
		comm_descr[poss++] = "Steal From Monster";
	}

	if (cp_ptr->spell_book)
	{
		comm[poss] = 'm';
		comm_descr[poss++] = "Cast a spell";
	}

	/* Prompt */
	put_str("Choose a command, or ESC:", 0, 0);

	button_backup_all();

	/* Get a choice */
	show_cmd_menu();

	/* Load screen */
	button_restore();

	do_cmd_redraw();
}

/**
 * Divide up the screen into mousepress regions
 */
int click_area(ui_event_data ke)
{
	if ((ke.mousey) && (ke.mousex > COL_MAP) && (ke.mousey < (Term->hgt - 1)))
	{
		return MOUSE_MAP;
	}
	/* Status Line */
	else if (!ke.mousey) return MOUSE_MESSAGE;
	else if (ke.mousey == Term->hgt - 1) return MOUSE_STATUS_BAR;
	else if (ke.mousex <= COL_MAP) return (sidebar_click(ke.mousey));
	else return MOUSE_NULL;
}


/*
 * Toggle wizard mode
 */
static void do_cmd_wizard(void)
{
	/* Verify first time */
	if (!(p_ptr->noscore & NOSCORE_WIZARD))
	{
		/* Mention effects */
		msg_print("You are about to enter 'wizard' mode for the very first time!");
		msg_print("This is a form of cheating, and your game will not be scored!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? "))
			return;

		/* Mark savefile */
		p_ptr->noscore |= NOSCORE_WIZARD;
	}

	/* Toggle mode */
	if (p_ptr->wizard)
	{
		p_ptr->wizard = FALSE;
		msg_print("Wizard mode off.");
	}
	else
	{
		p_ptr->wizard = TRUE;
		msg_print("Wizard mode on.");
	}

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw "title" */
	p_ptr->redraw |= (PR_TITLE);
}


#ifdef ALLOW_DEBUG

/*
 * Verify use of "debug" mode
 */
static void do_cmd_try_debug(void)
{
	/* Ask first time */
	if (!(p_ptr->noscore & NOSCORE_DEBUG))
	{
		/* Mention effects */
		msg_print("You are about to use the dangerous, unsupported, debug commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use the debug commands? "))
			return;

		/* Mark savefile */
		p_ptr->noscore |= NOSCORE_DEBUG;
	}

	/* Okay */
	do_cmd_debug();
}

#endif /* ALLOW_DEBUG */

/*
 * Quit the game.
 */
void do_cmd_quit(cmd_code code, cmd_arg args[])
{
	/* Stop playing */
	p_ptr->playing = FALSE;

	/* Leaving */
	p_ptr->leaving = TRUE;
}

int find_sidebar_mon_idx(ui_event_data ke)
{
	int row = sidebar_details[SIDEBAR_MON_MIN];
	int count = 0;
	int mouse_y = ke.mousey;

	/* Paranoia */
	if (!sidebar_monsters[count]) return (0);

	while (row < (Term->hgt - 2))
	{
		monster_type *m_ptr = &mon_list[sidebar_monsters[count]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Figure out how many rows the monster entry is taking up */
		row += 3;
		if (r_ptr->mana) row++;

		/* Hack - Avoid nonsense clicks with monsters who didn't fit on the final row */
		if (row >= (Term->hgt - 2)) break;

		/* We found the monster the player clicked on */
		if (row >= mouse_y) return (sidebar_monsters[count]);

		/* Go to the next monster */
		count++;

		/* Paranoia */
		if (count >= SIDEBAR_MONSTER_MAX) break;
	}

	return (0);
}


/*
 * Handle a mouseclick, using the horrible hack that is '\xff'.
 */
static void do_cmd_mouseclick(void)
{
	int x, y, area;

	y = KEY_GRID_Y(p_ptr->command_cmd_ex);
	x = KEY_GRID_X(p_ptr->command_cmd_ex);

	/* Find out where we've clicked */
	area = click_area(p_ptr->command_cmd_ex);

	switch (area)
	{
		case MOUSE_MAP:
		{
			/* Go through various on-screen options below */
			break;
		}
		/* Display the character */
		case SIDEBAR_RACE:
		{
			char buf[80];
			if (game_mode == GAME_NPPMORIA) strnfmt(buf, sizeof(buf), "m_raceclas.txt#%s", p_name + rp_ptr->name);
			else strnfmt(buf, sizeof(buf), "raceclas.txt#%s", p_name + rp_ptr->name);
			screen_save();
			show_file(buf, NULL, 0, 0);
			screen_load();
			return;
		}
		case SIDEBAR_CLASS:
		{
			char buf[80];
			if (game_mode == GAME_NPPMORIA) strnfmt(buf, sizeof(buf), "m_raceclas.txt#%s", c_name + cp_ptr->name);
			else strnfmt(buf, sizeof(buf), "raceclas.txt#%s", c_name + cp_ptr->name);
			screen_save();
			show_file(buf, NULL, 0, 0);
			screen_load();
			return;
		}
		case SIDEBAR_LEVEL:
		case SIDEBAR_STAT_MIN:
		case SIDEBAR_STAT_MAX:
		case SIDEBAR_XP:
		case SIDEBAR_GOLD:
		{
			do_cmd_change_name();
			return;
		}
		case SIDEBAR_HP:
		{
			textui_cmd_rest();
			return;
		}
		case SIDEBAR_MANA:
		{
			textui_cmd_cast();
			return;
		}
		case SIDEBAR_MON_MIN:
		case SIDEBAR_MON_MAX:
		{
			if (sidebar_monsters[0])
			{
				monster_type *m_ptr;
				int m_idx = find_sidebar_mon_idx(p_ptr->command_cmd_ex);

				/* Paranoia */
				if (!m_idx) return;

				m_ptr = &mon_list[m_idx];

				if (m_ptr->r_idx)
				{

					/* Save screen */
					screen_save();

					screen_roff(m_ptr->r_idx);

					(void)anykey();

					screen_load();
				}
			}
			return;
		}
		case MOUSE_MESSAGE:
		{
			do_cmd_messages();
			return;
		}
		case SIDEBAR_FEELING:
		{
		    do_cmd_feeling();
		    return;
		}
		case SIDEBAR_QUEST:
		{
		    do_cmd_quest();
		    return;
		}
		case SIDEBAR_EQUIPPY:
		{
			cmd_use_item();
			return;
		}
		/* Do nothing */
		case SIDEBAR_SPEED:
		case SIDEBAR_DEPTH:
		case MOUSE_STATUS_BAR:
		case MOUSE_NULL:
		default:
		{
			return;
		}
	}

	/* Give the player a list of command to choose from */
	if ((p_ptr->py == y) && (p_ptr->px == x))
	{
		/* Display player */
		show_commands();

		return;
	}

	if (!mouse_movement) return;

	/*if (p_ptr->command_cmd_ex.mousebutton == 2)
	{
		target_set_location(y, x);
		msg_print("Target set.");
	}*/

	/* Mouseclick on map other than the player*/
	if (p_ptr->timed[TMD_CONFUSED])
	{
		cmd_insert(CMD_WALK, DIR_UNKNOWN);
	}
	else
	{
		cmd_insert(CMD_PATHFIND, y, x);
	}
}

/*
 * Port-specific options
 *
 * Should be moved to the options screen. XXX
 */
static void do_cmd_port(void)
{
	(void)Term_user(0);
}


/*
 * Display the options and redraw afterward.
 */
static void do_cmd_xxx_options(void)
{
	do_cmd_options();
	do_cmd_redraw();
}


/*
 * Display the main-screen monster list.
 */
static void do_cmd_monlist(void)
{
	/* Save the screen and display the list */
	screen_save();
	display_monlist();

	/* Wait */
	anykey();

	/* Return */
	screen_load();
}


/*
 * Display the main-screen item list.
 */
static void do_cmd_itemlist(void)
{
	/* Save the screen and display the list */
	screen_save();
	display_itemlist();

	/* Wait */
	anykey();

	/* Return */
	screen_load();
}


/*
 * Invoked when the command isn't recognised.
 */
static void do_cmd_unknown(void)
{
	prt("'?':help, mouseclick on player:command suggestions, CTRL-h:complete command list.", 0, 0);
}


/* List indexed by char */
struct {
	do_cmd_type *hook;
	cmd_code cmd;
} converted_list[UCHAR_MAX+1];


/*** Menu functions ***/

/* Display an entry on a command menu */
static void cmd_sub_entry(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);
	const command_type *commands = menu->menu_data;

	(void)width;

	/* Write the description */
	Term_putstr(col, row, -1, attr, commands[oid].desc);

	/* Include keypress */
	Term_addch(attr, ' ');
	Term_addch(attr, '(');

	/* KTRL()ing a control character does not alter it at all */
	if (KTRL(commands[oid].key) == commands[oid].key)
	{
		Term_addch(attr, '^');
		Term_addch(attr, UN_KTRL(commands[oid].key));
	}
	else
	{
		Term_addch(attr, commands[oid].key);
	}

	Term_addch(attr, ')');
}


/* Handle user input from a command menu */
static bool cmd_sub_action(char cmd, void *db, int oid)
{
	(void)db;
	(void)oid;

	/* Only handle enter */
	if (cmd == '\n' || cmd == '\r')
		return TRUE;
	else
		return FALSE;
}


/*
 * Display a list of commands.
 */
static bool cmd_menu(command_list *list, void *selection_p)
{
	menu_type menu;
	menu_iter commands_menu = { NULL, NULL, cmd_sub_entry, cmd_sub_action };
	region area = { 23, 4, 37, 13 };

	ui_event_data evt;
	int cursor = 0;
	command_type *selection = selection_p;

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = "\x8B\x8C\n\r";
	menu.count = list->len;
	menu.menu_data = list->list;
	menu_init(&menu, MN_SKIN_SCROLL, &commands_menu, &area);

	/* Set up the screen */
	screen_save();
	window_make(21, 3, 62, 17);

	/* Select an entry */
	evt = menu_select(&menu, &cursor, 0);

	/* Load the screen */
	screen_load();

	if (evt.type == EVT_SELECT)
	{
		*selection = list->list[evt.index];
	}

	if (evt.type == EVT_ESCAPE)
	{
		return FALSE;
	}
	else
	{
		return TRUE;
	}
}


static bool cmd_list_action(char cmd, void *db, int oid)
{
	if (cmd == '\n' || cmd == '\r' || cmd == DEFINED_XFF)
	{
		return cmd_menu(&cmds_all[oid], db);
	}
	else
	{
		return FALSE;
	}
}


static void cmd_list_entry(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);
	(void)menu;
	(void)width;
	Term_putstr(col, row, -1, attr, cmds_all[oid].name);
}


/*
 * Display a list of command types, allowing the user to select one.
 */
static void do_cmd_menu(void)
{
	menu_type menu;
	menu_iter commands_menu = { NULL, NULL, cmd_list_entry, cmd_list_action };
	region area = { 21, 5, 37, 6 };

	int cursor = 0;
	command_type chosen_command = {NULL, '\0', CMD_NULL, NULL};

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = "\x8B\x8C\n\r";
	menu.count = N_ELEMENTS(cmds_all) - 1;
	menu.menu_data = &chosen_command;
	menu_init(&menu, MN_SKIN_SCROLL, &commands_menu, &area);

	/* Set up the screen */
	screen_save();
	window_make(19, 4, 58, 11);

	/* Select an entry */
	(void)menu_select(&menu, &cursor, 0);

	/* Load de screen */
	screen_load();

	/* If a command was chosen, do it. */
	if (chosen_command.cmd != CMD_NULL)
	{
		cmd_insert(chosen_command.cmd);
	}
	else if (chosen_command.hook)
	{
		chosen_command.hook();
	}
}


/*** Exported functions ***/

/*
 * Initialise the command list.
 */
void cmd_init(void)
{
	size_t i, j;

	/* Go through all the lists of commands */
	for (j = 0; j < N_ELEMENTS(cmds_all); j++)
	{
		command_type *commands = cmds_all[j].list;

		/* Fill everything in */
		for (i = 0; i < cmds_all[j].len; i++)
		{
			unsigned char key = commands[i].key;

			/* Note: at present converted_list is UCHAR_MAX + 1
			   large, so 'key' is always a valid index. */
			converted_list[key].hook = commands[i].hook;
			converted_list[key].cmd = commands[i].cmd;
		}
	}

	/* Fill in the rest */
	for (i = 0; i < N_ELEMENTS(converted_list); i++)
	{
		switch (i)
		{
			/* Ignore */
			case ESCAPE:
			case ' ':
			case '\a':
			{
				break;
			}

			default:
			{
				if (!converted_list[i].hook && !converted_list[i].cmd)
				{
					converted_list[i].hook = do_cmd_unknown;
					converted_list[i].cmd = CMD_NULL;
				}

				break;
			}
		}
	}
}


/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
void textui_process_command(bool no_request)
{

	if (!no_request)
		request_command();

	/* Handle repeating the last command */
	/* repeat_check();*/

	/* Handle resize events XXX */
	if (p_ptr->command_cmd_ex.type == EVT_RESIZE)
	{
		do_cmd_redraw();
	}
	else
	{
		/* Within these boundaries, the cast to unsigned char will have the desired effect */
		assert(p_ptr->command_cmd >= CHAR_MIN && p_ptr->command_cmd <= CHAR_MAX);

		/* Execute the command */
		if (converted_list[(unsigned char) p_ptr->command_cmd].cmd != CMD_NULL)
			cmd_insert(converted_list[(unsigned char) p_ptr->command_cmd].cmd);

		else if (converted_list[(unsigned char) p_ptr->command_cmd].hook)
			converted_list[(unsigned char) p_ptr->command_cmd].hook();
	}
}
