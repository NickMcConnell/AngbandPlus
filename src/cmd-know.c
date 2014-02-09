/*
 * File: cmd-know.c
 * Purpose: Knowledge screen stuff.
 *
 * Copyright (c) 2000-2007 Eytan Zweig, Andrew Doull, Pete Mack.
 *                    Jeff Greene, Diego Gonzalez
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
#include "ui.h"
#include "ui-menu.h"
#include "game-event.h"



/* Flag value for missing array entry */
#define MISSING -17


typedef struct
{
	int maxnum;			/* Maximum possible item count for this class */
	bool easy_know;		/* Items don't need to be IDed to recognize membership */

	const char *(*name)(int gid);		/* Name of this group */

	/* Compare, in group and display order (optional if already sorted) */
	int (*gcomp)(const void *, const void *);	/* Compares gids of two oids */
	int (*group)(int oid);						/* Returns gid for an oid */

	/* Summary function for the "object" information. */
	void (*summary)(int gid, const int *object_list, int n, int top, int row, int col);
} group_funcs;


typedef struct
{
	/* Displays an entry at specified location, including kill-count and graphics */
	void (*display_member)(int col, int row, bool cursor, int oid);

	void (*lore)(int oid);		/* Displays lore for an oid */


	/* Required only for objects with modifiable display attributes */
	/* Unknown 'flavors' return flavor attributes */
	char *(*xchar)(int oid);	/* Get character attr for OID (by address) */
	byte *(*xattr)(int oid);	/* Get color attr for OID (by address) */

	const char *(*xtra_prompt)(int oid);	/* Returns optional extra prompt */
	void (*xtra_act)(char ch, int oid);		/* Handles optional extra actions */

	bool is_visual;							/* Does this kind have visual editing? */
} member_funcs;


/* Helper class for generating joins */
typedef struct join
{
		int oid;
		int gid;
} join_t;


/* A default group-by */
static join_t *default_join;

static int default_group(int oid) { return default_join[oid].gid; }


static int *obj_group_order;

/*
 * Description of each monster group.
 */
typedef struct monster_group monster_group;

struct monster_group
{
	cptr chars;
	cptr name;
};

static struct monster_group monster_group_nppmoria[] =
{
	{ (cptr)-1,	"Uniques" },
	{ "A",		"Ant Lions" },
	{ "a",		"Ants" },
	{ "B",		"Balrog" },
	{ "b",		"Bats" },
	{ "C",		"Gelatinous Cube" },
	{ "c",		"Centipedes" },
	{ "dD",		"Dragons" },
	{ "E",		"Elementals/Spirits" },
	{ "e",		"Floating Eyes" },
	{ "F",		"Flying Insects" },
	{ "f",		"Frogs" },
	{ "G",		"Ghosts" },
	{ "g",		"Golems" },
	{ "H",		"Hobgoblin" },
	{ "h",		"Harpies" },
	{ "i",		"Icky Things" },
	{ "J",		"Jellies" },
	{ "j",		"Jackals" },
	{ "K",		"Killer Beetles" },
	{ "k",		"Kobolds" },
	{ "L",		"Liches" },
	{ "l",		"Louse" },
	{ "M",		"Mummies" },
	{ "m",		"Molds" },
	{ "n",		"Nagas" },
/* Note some special handling of mimics in the code below since there are so many different symbols */
	{ "$!?",	"Mimics" },
	{ ",",		"Mushroom Patches" },
	{ "O",		"Oozes" },
	{ "o",		"Orcs" },
	{ "p",		"People/Humanoids" },
	{ "P",		"Giants" },
	{ "q",		"Quasits" },
	{ "Q",		"Quylthulgs" },
	{ "R",		"Snakes" },
	{ "r",		"Rodents" },
	{ "S",		"Scorpions" },
	{ "s",		"Skeletons" },
	{ "U",		"Umber Hulks" },
	{ "T",		"Trolls" },
	{ "t",		"Ticks" },
	{ "V",		"Vampires" },
	{ "W",		"Wights/Wraiths" },
	{ "w",		"Worms/Worm Masses" },
	{ "X",		"Xorns" },
	{ "y",		"Yeeks" },
	{ "Y",		"Yeti" },
	{ "z",		"Zombies" },
	{ NULL,       NULL }
};



static struct monster_group monster_group_nppangband[] =
{
	{ (cptr)-1,	"Uniques" },
	{ "A",		"Ainur/Maiar" },
	{ "a",		"Ants" },
	{ "b",		"Bats" },
	{ "B",		"Birds" },
	{ "C",		"Canines" },
	{ "c",		"Centipedes" },
	{ "uU",		"Demons" },
	{ "dD",		"Dragons" },
	{ "vE",		"Elementals/Vortices" },
	{ "e",		"Eyes/Beholders" },
	{ "f",		"Felines" },
	{ "G",		"Ghosts" },
	{ "OP",		"Giants/Ogres" },
	{ "g",		"Golems" },
	{ "H",		"Harpies/Hybrids" },
	{ "h",		"Hominids (Elves, Dwarves)" },
	{ "M",		"Hydras" },
	{ "i",		"Icky Things" },
	{ "lFI",	"Insects" },
	{ "j",		"Jellies" },
	{ "K",		"Killer Beetles" },
	{ "k",		"Kobolds" },
	{ "L",		"Lichs" },
	{ "tp",		"Men" },
/* Note some special handling of mimics in the code below since there are so many different symbols */
	{ "$!?",	"Mimics" },
	{ "m",		"Molds" },
	{ ",",		"Mushroom Patches" },
	{ "n",		"Nagas" },
	{ "o",		"Orcs" },
	{ "q",		"Quadrupeds" },
	{ "Q",		"Quylthulgs" },
	{ "R",		"Reptiles/Amphibians" },
	{ "r",		"Rodents" },
	{ "S",		"Scorpions/Spiders/Ticks" },
	{ "s",		"Skeletons/Drujs" },
	{ "J",		"Snakes" },
	{ "T",		"Trolls" },
	{ "V",		"Vampires" },
	{ "W",		"Wights/Wraiths" },
	{ "w",		"Worms/Worm Masses" },
	{ "X",		"Xorns/Xarens" },
	{ "y",		"Yeeks" },
	{ "Y",		"Yeti" },
	{ "Z",		"Zephyr Hounds" },
	{ "z",		"Zombies" },
	{ NULL,       NULL }
};


/*
 * The label of the group who collects miscellaneous features.
 */
#define MISC_GRP "Misc"
#define MAX_FEATURE_TYPES	18


/*
 * Description of each feature group.
 */
const char *feature_group_text[MAX_FEATURE_TYPES] =
{
	"Floors",
	"Player Traps",
	"Monster Traps",
	"Runes",
	"Doors",
	"Stairs",
	"Walls",
	"Stores",
	"Bridges",
	"Water",
	"Lava",
	"Ice",
	"Acid",
	"Oil",
	"Forest",
	"Sand/Mud",
	"Fire",
	MISC_GRP
};


/*
 * Flags of features in each group.
 * The misc. group must have all flags set to 0.
 */
static const u32b feature_flags[MAX_FEATURE_TYPES][3] =
{
	{FF1_FLOOR, 0, 0},
	{0, FF2_TRAP_PASSIVE, 0},
	{0, FF2_TRAP_MON, 0},
	{0, FF2_TRAP_SMART, 0},
	{FF1_DOOR, 0, 0},
	{FF1_STAIRS, 0, 0},
	{FF1_WALL, 0, 0},
	{FF1_SHOP, 0, 0},
	{0, FF2_BRIDGED, 0},
	{0, 0, FF3_WATER},
	{0, 0, FF3_LAVA},
	{0, 0, FF3_ICE},
	{0, 0, FF3_ACID},
	{0, 0, FF3_OIL},
	{0, 0, FF3_FOREST},
	{0, 0, FF3_SAND | FF3_MUD},
	{0, 0, FF3_FIRE},
	{0, 0, 0}   /*Misc Group*/
};


/* Useful method declarations */
static void display_visual_list(int col, int row, int height, int width,
				byte attr_top, byte char_left);

static bool visual_mode_command(ui_event_data ke, bool *visual_list_ptr,
				int height, int width,
				byte *attr_top_ptr, byte *char_left_ptr,
				byte *cur_attr_ptr, byte *cur_char_ptr,
				int col, int row, int *delay);

static void place_visual_list_cursor(int col, int row, byte a,
				byte c, byte attr_top, byte char_left);

/*
 * Clipboard variables for copy&paste in visual mode
 */
static byte attr_idx = 0;
static byte char_idx = 0;


/*
 * Return a specific ordering for the features.
 * Assumes the misc feature is the last one in feature_flags.
 */
static int feat_order(int feat)
{
	int i;

	/* Access the features */
	feature_type *f_ptr = &f_info[feat];

	for (i = 0; i < MAX_FEATURE_TYPES; i++)
	{
		/*
		 * We use the misc. group to collect features who don't match any
		 * other group
		 */
		bool collecting_misc = streq(feature_group_text[i], MISC_GRP);

		/* Check for any flags matching in the group */
		if (_feat_ff1_match(f_ptr, feature_flags[i][0]) ||
	 		_feat_ff2_match(f_ptr, feature_flags[i][1]) ||
	 		_feat_ff3_match(f_ptr, feature_flags[i][2]))
		{
			/*
	 		 * Reject interesting features when collecting the
	 		 * misc. group
	 		 */
	 		if (collecting_misc) continue;
		}

	 	/* No matching flags. Ignore boring feature */
		else
		{
			/* Unless when collecting the misc. group */
			if (!collecting_misc) continue;
		}

		/* We found the match */
		return (i);
	}

	/* No match found, return misc */
	return (i-1);
}


/*
 * Emit a 'graphical' symbol and a padding character if appropriate
 */
static void big_pad(int col, int row, byte a, byte c)
{
	Term_putch(col, row, a, c);
	if (!use_bigtile) return;

	if (a & 0x80)
		Term_putch(col + 1, row, 255, -1);
	else
		Term_putch(col + 1, row, 1, ' ');
}


/*
 * Return the actual width of a symbol
 */
static int actual_width(int width)
{
	if (use_bigtile) width *= 2;

	return width;
}


/*
 * Return the actual height of a symbol
 */
static int actual_height(int height)
{
	if (use_bigtile) height *= 2;

	return height;
}


/*
 * From an actual width, return the logical width
 */
static int logical_width(int width)
{
	int divider = 1;

	if (use_bigtile) divider *= 2;

	return width / divider;
}


/*
 * From an actual height, return the logical height
 */
static int logical_height(int height)
{
	int divider = 1;


	if (use_bigtile) divider *= 2;

	return height / divider;
}


static void display_group_member(menu_type *menu, int oid,
						bool cursor, int row, int col, int wid)
{
	const member_funcs *o_funcs = menu->menu_data;
	byte attr = curs_attrs[CURS_KNOWN][cursor == oid];

	(void)wid;

	/* Print the interesting part */
	o_funcs->display_member(col, row, cursor, oid);

	/* Do visual mode */
	if (o_funcs->is_visual && o_funcs->xattr)
	{
		byte c = *o_funcs->xchar(oid);
		byte a = *o_funcs->xattr(oid);

		c_put_str(attr, format((c & 0x80) ? "%02x/%02x" : "%02x/%d", a, c), row, 60);
	}
}


/*
 * Append the recall prompt
 */
static const char *recall_prompt(int oid)
{
	(void)oid;
	return ", 'r' to recall";
}


#define swap(a, b) (swapspace = (void*)(a)), ((a) = (b)), ((b) = swapspace)


/*
 * Interactive group by.
 * Recognises inscriptions, graphical symbols, lore
 */
/*
 * Interactive group by.
 * Recognises inscriptions, graphical symbols, lore
 */
static void display_knowledge(const char *title, int *obj_list, int o_count,
				group_funcs g_funcs, member_funcs o_funcs,
				const char *otherfields)
{
	/* maximum number of groups to display */
	int max_group = g_funcs.maxnum < o_count ? g_funcs.maxnum : o_count ;

	/* This could (should?) be (void **) */
	int *g_list, *g_offset;

	const char **g_names;

	int g_name_len = 8;  /* group name length, minimum is 8 */

	int grp_cnt = 0; /* total number groups */

	int g_cur = 0, grp_old = -1; /* group list positions */
	int o_cur = 0;				/* object list positions */
	int g_o_count = 0;			 /* object count for group */
	int oid = -1;				/* object identifiers */

	region title_area = { 0, 0, 0, 4 };
	region group_region = { 0, 6, MISSING, -2 };
	region object_region = { MISSING, 6, 0, -2 };

	/* display state variables */
	bool visual_list = FALSE;
	byte attr_top = 0;
	byte char_left = 0;

	int delay = 0;

	menu_type group_menu;
	menu_type object_menu;
	menu_iter object_iter;

	/* Panel state */
	/* These are swapped in parallel whenever the actively browsing " */
	/* changes */
	int *active_cursor = &g_cur, *inactive_cursor = &o_cur;
	menu_type *active_menu = &group_menu, *inactive_menu = &object_menu;
	int panel = 0;

	void *swapspace;
	bool do_swap = FALSE;

	bool flag = FALSE;
	bool redraw = TRUE;

	int browser_rows;
	int wid, hgt;
	int i;
	int prev_g = -1;

	int omode = rogue_like_commands;


	/* Get size */
	Term_get_size(&wid, &hgt);
	browser_rows = hgt - 8;

	/* Disable the roguelike commands for the duration */
	rogue_like_commands = FALSE;

	/* Do the group by. ang_sort only works on (void **) */
	/* Maybe should make this a precondition? */
	if (g_funcs.gcomp)
		qsort(obj_list, o_count, sizeof(*obj_list), g_funcs.gcomp);

	/* Sort everything into group order */
	g_list = C_ZNEW(max_group + 1, int);
	g_offset = C_ZNEW(max_group + 1, int);

	for (i = 0; i < o_count; i++)
	{
		if (prev_g != g_funcs.group(obj_list[i]))
		{
			prev_g = g_funcs.group(obj_list[i]);
			g_offset[grp_cnt] = i;
			g_list[grp_cnt++] = prev_g;
		}
	}

	g_offset[grp_cnt] = o_count;
	g_list[grp_cnt] = -1;


	/* The compact set of group names, in display order */
	g_names = C_ZNEW(grp_cnt, const char *);

	for (i = 0; i < grp_cnt; i++)
	{
		int len;
		g_names[i] = g_funcs.name(g_list[i]);
		len = strlen(g_names[i]);
		if (len > g_name_len) g_name_len = len;
	}

	/* Reasonable max group name len */
	if (g_name_len >= 20) g_name_len = 20;

	object_region.col = g_name_len + 3;
	group_region.width = g_name_len;


	/* Leave room for the group summary information */
	if (g_funcs.summary) object_region.page_rows = -3;


	/* Set up the two menus */
	WIPE(&group_menu, menu_type);
	group_menu.count = grp_cnt;
	group_menu.cmd_keys = "\n\r6\x8C";  /* Ignore these as menu commands */
	group_menu.menu_data = g_names;

	WIPE(&object_menu, menu_type);
	object_menu.menu_data = &o_funcs;
	WIPE(&object_iter, object_iter);
	object_iter.display_row = display_group_member;

	o_funcs.is_visual = FALSE;

	menu_init(&group_menu, MN_SKIN_SCROLL, find_menu_iter(MN_ITER_STRINGS), &group_region);
	menu_init(&object_menu, MN_SKIN_SCROLL, &object_iter, &object_region);

	/* Start with no buttons */
	button_kill_all();

	/* This is the event loop for a multi-region panel */
	/* Panels are -- text panels, two menus, and visual browser */
	/* with "pop-up menu" for lore */
	while ((!flag) && (grp_cnt))
	{
		ui_event_data ke, ke0;

		if (redraw)
		{
			/* Print the title bits */
			region_erase(&title_area);
			prt(format("Knowledge - %s", title), 2, 0);
			prt("Group", 4, 0);
			prt("Name", 4, g_name_len + 3);

			if (otherfields)
				prt(otherfields, 4, 46);

			/* Print dividers: horizontal and vertical */
			for (i = 0; i < 79; i++)
				Term_putch(i, 5, TERM_WHITE, '=');

			for (i = 0; i < browser_rows; i++)
				Term_putch(g_name_len + 1, 6 + i, TERM_WHITE, '|');

			/* Reset redraw flag */
			redraw = FALSE;
		}

		if (g_cur != grp_old)
		{
			grp_old = g_cur;
			o_cur = 0;
			g_o_count = g_offset[g_cur+1] - g_offset[g_cur];
			menu_set_filter(&object_menu, obj_list + g_offset[g_cur], g_o_count);
			group_menu.cursor = g_cur;
			object_menu.cursor = 0;
		}

		/* HACK ... */
		if (!visual_list)
		{
			/* ... The object menu may be browsing the entire group... */
			o_funcs.is_visual = FALSE;
			menu_set_filter(&object_menu, obj_list + g_offset[g_cur], g_o_count);
			object_menu.cursor = o_cur;
		}
		else
		{
			/* ... or just a single element in the group. */
			o_funcs.is_visual = TRUE;
			menu_set_filter(&object_menu, obj_list + o_cur + g_offset[g_cur], 1);
			object_menu.cursor = 0;
		}

		oid = obj_list[g_offset[g_cur]+o_cur];

		/* Print prompt */
		{
			const char *pedit = (!o_funcs.xattr) ? "" :
					(!(attr_idx|char_idx) ? ", 'c' to copy" : ", 'c', 'p' to paste");
			const char *xtra = o_funcs.xtra_prompt ? o_funcs.xtra_prompt(oid) : "";
			const char *pvs = "";

			button_add("|ESC", ESCAPE);
			button_add("|RECALL", 'r');

			/* Sort out the cut-and-paste buttons */
			if (o_funcs.xattr)
			{
				if (attr_idx|char_idx)
				{
					button_add("|PASTE", 'p');
					button_kill('c');
				}
				else
				{
					button_kill('p');
					button_add("|COPY", 'c');
				}
			}
			else
			{
				button_kill('p');
				button_kill('c');
			}

			/* Either ENTER or VISUALS */
			if (visual_list)
			{
				pvs = ", ENTER to accept";
				button_add("|ENTER", '\r');
				button_kill('v');
			}
			else if (o_funcs.xattr)
			{
				pvs = ", 'v' for visuals";
				button_add("|VISUALS", 'v');
				button_kill('\r');
			}

			prt(format("<dir>%s%s%s, ESC", pvs, pedit, xtra), hgt - 2, 0);
		}

		/* Update the buttons */
		event_signal(EVENT_MOUSEBUTTONS);

		if (do_swap)
		{
			do_swap = FALSE;
			swap(active_menu, inactive_menu);
			swap(active_cursor, inactive_cursor);
			panel = 1 - panel;
		}

		if (g_funcs.summary && !visual_list)
		{
			g_funcs.summary(g_cur, obj_list, g_o_count, g_offset[g_cur],
			                object_menu.boundary.row + object_menu.boundary.page_rows,
			                object_region.col);
		}

		menu_refresh(inactive_menu);
		menu_refresh(active_menu);

		handle_stuff();

		if (visual_list)
		{
			display_visual_list(g_name_len + 3, 7, browser_rows-1,
										wid - (g_name_len + 3), attr_top, char_left);
			place_visual_list_cursor(g_name_len + 3, 7, *o_funcs.xattr(oid),
										*o_funcs.xchar(oid), attr_top, char_left);
		}

		if (delay)
		{
			/* Force screen update */
			Term_fresh();

			/* Delay */
			Term_xtra(TERM_XTRA_DELAY, delay);

			delay = 0;
		}

		ke = inkey_ex();

		/* Do visual mode command if needed */
		if (o_funcs.xattr && o_funcs.xchar &&
					visual_mode_command(ke, &visual_list,
					browser_rows-1, wid - (g_name_len + 3),
					&attr_top, &char_left,
					o_funcs.xattr(oid), (byte*)
					o_funcs.xchar(oid),
					g_name_len + 3, 7, &delay))
		{
			continue;
		}

		if (ke.type == EVT_MOUSE)
		{
			/* Change active panels */
			if (region_inside(&inactive_menu->boundary, &ke))
			{
				swap(active_menu, inactive_menu);
				swap(active_cursor, inactive_cursor);
				panel = 1-panel;
			}
		}

		ke0 = run_event_loop(&active_menu->target, &ke);
		if (ke0.type != EVT_AGAIN) ke = ke0;

		switch (ke.type)
		{
			case EVT_KBRD:
			case EVT_BUTTON:
			{
				break;
			}

			case EVT_SELECT:
			case EVT_MOVE:
			{
				if (ke.type == EVT_SELECT)
				{
					if (panel == 1 && oid >= 0 && o_cur == active_menu->cursor)
					{
						o_funcs.lore(oid);
						redraw = TRUE;
					}
				}

				*active_cursor = active_menu->cursor;
				continue;
			}

			case EVT_BACK:
			{
				if (panel == 1)
					do_swap = TRUE;
				continue;
			}

			/* XXX Handle EVT_RESIZE */

			default:
			{
				continue;
			}
		}

		switch (ke.key)
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
				if (oid >= 0)
					o_funcs.lore(oid);

				redraw = TRUE;
				break;
			}

			/* Jump down a page */
			case '3':
			{
				*active_cursor += browser_rows;

				if (g_cur >= grp_cnt) g_cur = grp_cnt - 1;
				else if (o_cur >= g_o_count) o_cur = g_o_count - 1;

				break;
			}

			/* Jump up a page */
			case '9':
			{
				*active_cursor -= browser_rows;

				if (*active_cursor < 0) *active_cursor = 0;

				break;
			}

			default:
			{
				int d = target_dir(ke.key);

				/* Handle key-driven motion between panels */
				if (ddx[d] && ((ddx[d] < 0) == (panel == 1)))
				{
					/* Silly hack -- diagonal arithmetic */
					*inactive_cursor += ddy[d];
					if (*inactive_cursor < 0) *inactive_cursor = 0;
					else if (g_cur >= grp_cnt) g_cur = grp_cnt - 1;
					else if (o_cur >= g_o_count) o_cur = g_o_count - 1;
					do_swap = TRUE;
				}
				else if (o_funcs.xtra_act)
				{
					o_funcs.xtra_act(ke.key, oid);
				}

				break;
			}
		}
	}

	/* Restore roguelike option */
	rogue_like_commands = omode;

	/* Prompt */
	if (!grp_cnt)
		prt(format("No %s known.", title), 15, 0);

	FREE(g_names);
	FREE(g_offset);
	FREE(g_list);

	/* Restore the buttons */
	button_kill_all();
	button_restore();
}

/*
 * Display visuals.
 */
static void display_visual_list(int col, int row, int height, int width, byte attr_top, byte char_left)
{
	int i, j;

	/* Clear the display lines */
	for (i = 0; i < height; i++)
			Term_erase(col, row + i, width);

	width = logical_width(width);

	/* Display lines until done */
	for (i = 0; i < height; i++)
	{
		/* Display columns until done */
		for (j = 0; j < width; j++)
		{
			byte a;
			unsigned char c;
			int x = col + actual_width(j);
			int y = row + actual_width(i);
			int ia, ic;

			ia = attr_top + i;
			ic = char_left + j;

			a = (byte)ia;
			c = (unsigned char)ic;

			/* Display symbol */
			big_pad(x, y, a, c);
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

	int x = col + actual_width(j);
	int y = row + actual_height(i);

	/* Place the cursor */
	Term_gotoxy(x, y);
}


/*
 *  Do visual mode command -- Change symbols
 */
static bool visual_mode_command(ui_event_data ke, bool *visual_list_ptr,
				int height, int width,
				byte *attr_top_ptr, byte *char_left_ptr,
				byte *cur_attr_ptr, byte *cur_char_ptr,
				int col, int row, int *delay)
{
	static byte attr_old = 0;
	static char char_old = 0;

	/* These are the distance we want to maintain between the
	 * cursor and borders.
	 */
	int frame_left = logical_width(10);
	int frame_right = logical_width(10);
	int frame_top = logical_height(4);
	int frame_bottom = logical_height(4);

	switch (ke.key)
	{
		case ESCAPE:
		{
			if (*visual_list_ptr)
			{
				/* Cancel change */
				*cur_attr_ptr = attr_old;
				*cur_char_ptr = char_old;
				*visual_list_ptr = FALSE;

				return TRUE;
			}

			break;
		}

		case '\n':
		case '\r':
		{
			if (*visual_list_ptr)
			{
				/* Accept change */
				*visual_list_ptr = FALSE;
				return TRUE;
			}

			break;
		}

		case 'V':
		case 'v':
		{
			if (!*visual_list_ptr)
			{
				*visual_list_ptr = TRUE;

				*attr_top_ptr = (byte)MAX(0, (int)*cur_attr_ptr - frame_top);
				*char_left_ptr = (char)MAX(0, (int)*cur_char_ptr - frame_left);

				attr_old = *cur_attr_ptr;
				char_old = *cur_char_ptr;
			}
			else
			{
				/* Cancel change */
				*cur_attr_ptr = attr_old;
				*cur_char_ptr = char_old;
				*visual_list_ptr = FALSE;
			}

			return TRUE;
		}

		case 'C':
		case 'c':
		{
			/* Set the visual */
			attr_idx = *cur_attr_ptr;
			char_idx = *cur_char_ptr;

			return TRUE;
		}

		case 'P':
		case 'p':
		{
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
				*char_left_ptr = (char)MAX(0, (int)*cur_char_ptr - frame_left);
			}

			return TRUE;
		}

		default:
		{
			if (*visual_list_ptr)
			{
				int eff_width = actual_width(width);
				int eff_height = actual_height(height);
				int d = target_dir(ke.key);
				byte a = *cur_attr_ptr;
				byte c = *cur_char_ptr;

				/* Get mouse movement */
				if (ke.key == '\xff')
				{
					int my = ke.mousey - row;
					int mx = ke.mousex - col;

					my = logical_height(my);
					mx = logical_width(mx);

					if ((my >= 0) && (my < eff_height) && (mx >= 0) && (mx < eff_width)
						&& ((ke.index) || (a != *attr_top_ptr + my)
							|| (c != *char_left_ptr + mx)))
					{
						/* Set the visual */
						*cur_attr_ptr = a = *attr_top_ptr + my;
						*cur_char_ptr = c = *char_left_ptr + mx;

						/* Move the frame */
						if (*char_left_ptr > MAX(0, (int)c - frame_left))
							(*char_left_ptr)--;
						if (*char_left_ptr + eff_width <= MIN(255, (int)c + frame_right))
							(*char_left_ptr)++;
						if (*attr_top_ptr > MAX(0, (int)a - frame_top))
							(*attr_top_ptr)--;
						if (*attr_top_ptr + eff_height <= MIN(255, (int)a + frame_bottom))
							(*attr_top_ptr)++;

						/* Delay */
						*delay = 100;

						/* Accept change */
						if (ke.index) *visual_list_ptr = FALSE;

						return TRUE;
					}

					/* Cancel change */
					else if (ke.index)
					{
						*cur_attr_ptr = attr_old;
						*cur_char_ptr = char_old;
						*visual_list_ptr = FALSE;

						return TRUE;
					}
				}
				else
				{
					/* Restrict direction */
					if ((a == 0) && (ddy[d] < 0)) d = 0;
					if ((c == 0) && (ddx[d] < 0)) d = 0;
					if ((a == 255) && (ddy[d] > 0)) d = 0;
					if ((c == 255) && (ddx[d] > 0)) d = 0;

					a += ddy[d];
					c += ddx[d];

					/* Set the visual */
					*cur_attr_ptr = a;
					*cur_char_ptr = c;

					/* Move the frame */
					if ((ddx[d] < 0) && *char_left_ptr > MAX(0, (int)c - frame_left))
						(*char_left_ptr)--;
					if ((ddx[d] > 0) && *char_left_ptr + eff_width <=
														MIN(255, (int)c + frame_right))
					(*char_left_ptr)++;

					if ((ddy[d] < 0) && *attr_top_ptr > MAX(0, (int)a - frame_top))
						(*attr_top_ptr)--;
					if ((ddy[d] > 0) && *attr_top_ptr + eff_height <=
													MIN(255, (int)a + frame_bottom))
						(*attr_top_ptr)++;

					/* We need to always eat the input even if it is clipped,
					 * otherwise it will be interpreted as a change object
					 * selection command with messy results.
					 */
					return TRUE;
				}
			}

			break;
		}
	}

	/* Visual mode command is not used */
	return FALSE;
}


/* The following sections implement "subclasses" of the
 * abstract classes represented by member_funcs and group_funcs
 */

/* =================== MONSTERS ==================================== */
/* Many-to-many grouping - use default auxiliary join */

/*
 * Display a monster
 */
static void display_monster(int col, int row, bool cursor, int oid)
{
	/* HACK Get the race index. (Should be a wrapper function) */
	int r_idx = default_join[oid].oid;

	/* Access the race */
	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];

	/* Choose colors */
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];
	byte a = r_ptr->x_attr;
	byte c = r_ptr->x_char;

	/* Display the name */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		c_prt(attr, player_ghost_name, row, col);
	}
	else c_prt(attr, r_ptr->name_full, row, col);

	/* Display symbol */
	big_pad(66, row, a, c);

	/* Display kills */
	if (r_ptr->flags1 & (RF1_UNIQUE))
		put_str(format("%s", (r_ptr->max_num == 0)?  " dead" : "alive"), row, 70);
	else
		put_str(format("%5d", l_ptr->pkills), row, 70);
}


static int m_cmp_race(const void *a, const void *b)
{
	const monster_race *r_a = &r_info[default_join[*(const int *)a].oid];
	const monster_race *r_b = &r_info[default_join[*(const int *)b].oid];
	int gid = default_join[*(const int *)a].gid;

	/* Group by */
	int c = gid - default_join[*(const int *)b].gid;
	if (c) return c;

	/* Order results */
	c = r_a->d_char - r_b->d_char;
	if (c && gid != 0)
	{
		if (game_mode == GAME_NPPMORIA)
		{
			return strchr(monster_group_nppmoria[gid].chars, r_a->d_char)
						- strchr(monster_group_nppmoria[gid].chars, r_b->d_char);
		}

		/* UNIQUE group is ordered by level & name only */
		/* Others by order they appear in the group symbols */
		else return strchr(monster_group_nppangband[gid].chars, r_a->d_char)
			- strchr(monster_group_nppangband[gid].chars, r_b->d_char);
	}
	c = r_a->level - r_b->level;
	if (c) return c;

	return strcmp(r_a->name_full, r_b->name_full);
}

static char *m_xchar(int oid) { return &r_info[default_join[oid].oid].x_char; }
static byte *m_xattr(int oid) { return &r_info[default_join[oid].oid].x_attr; }
static const char *race_name(int gid)
{
	if (game_mode == GAME_NPPMORIA) return monster_group_nppmoria[gid].name;

	return monster_group_nppangband[gid].name;

}

static void mon_lore(int oid)
{
	/* Update the monster recall window */
	monster_race_track(default_join[oid].oid);
	handle_stuff();

	/* Save the screen */
	screen_save();

	/* Describe */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	roff_top(default_join[oid].oid);
	Term_gotoxy(0, 2);
	describe_monster(default_join[oid].oid, FALSE);

	text_out_c(TERM_L_BLUE, "\n[Press any key to continue]\n");
	(void)anykey();

	/* Load the screen */
	screen_load();
}

static void mon_summary(int gid, const int *object_list, int n, int top, int row, int col)
{
	int i;
	int kills = 0;

	/* Access the race */
	for (i = 0; i < n; i++)
	{
		int oid = default_join[object_list[i+top]].oid;
		kills += l_list[oid].pkills;
	}

	/* Different display for the first item if we've got uniques to show */
	if (gid == 0 && ((&r_info[default_join[object_list[0]].oid])->flags1 & (RF1_UNIQUE)))
	{
		c_prt(TERM_L_BLUE, format("%d known uniques, %d slain.", n, kills),
					row, col);
	}
	else
	{
		int tkills = 0;

		for (i = 0; i < z_info->r_max; i++)
			tkills += l_list[i].pkills;

		c_prt(TERM_L_BLUE, format("Creatures slain: %d/%d (in group/in total)", kills, tkills), row, col);
	}
}


static int count_known_monsters(void)
{
	int m_count = 0;
	int i;
	size_t j;

	for (i = 0; i < z_info->r_max; i++)
	{
		size_t counter = (game_mode == GAME_NPPMORIA ? N_ELEMENTS(monster_group_nppmoria) : N_ELEMENTS(monster_group_nppangband));

		monster_race *r_ptr = &r_info[i];
		if ((!cheat_know) && !l_list[i].sights) continue;
		if (!r_ptr->r_speed) continue;

		if (r_ptr->flags1 & RF1_UNIQUE) m_count++;

		/* Stop before the last one */
		counter--;

		for (j = 1; j < counter; j++)
		{
			const char *pat = (game_mode == GAME_NPPMORIA ? monster_group_nppmoria[j].chars : monster_group_nppangband[j].chars);
			if (strchr(pat, r_ptr->d_char)) m_count++;
			/* Special hack to count all of the mimics */
			else if (r_ptr->flags1 & (RF1_CHAR_MIMIC))
			{
				if (strchr(pat, '!')) m_count++;
			}
		}
	}

	return m_count;
}


/*
 * Display known monsters.
 */
static void do_cmd_knowledge_monsters(void *obj, const char *name)
{
	size_t counter = (game_mode == GAME_NPPMORIA ? N_ELEMENTS(monster_group_nppmoria) : N_ELEMENTS(monster_group_nppangband));

	group_funcs r_funcs_nppangband = {N_ELEMENTS(monster_group_nppangband), FALSE, race_name,
							m_cmp_race, default_group, mon_summary};

	group_funcs r_funcs_nppmoria = {N_ELEMENTS(monster_group_nppmoria), FALSE, race_name,
								m_cmp_race, default_group, mon_summary};

	member_funcs m_funcs = {display_monster, mon_lore, m_xchar, m_xattr, recall_prompt, 0, 0};

	int *monsters;
	int m_count = 0;
	int i;
	size_t j;

	(void)obj;
	(void)name;

	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		if ((!cheat_know) && !l_list[i].sights) continue;
		if (!r_ptr->r_speed) continue;

		if (r_ptr->flags1 & (RF1_UNIQUE)) m_count++;

		for (j = 1; j < (counter - 1); j++)
		{
			const char *pat = (game_mode == GAME_NPPMORIA ? monster_group_nppmoria[j].chars : monster_group_nppangband[j].chars);
			if (strchr(pat, r_ptr->d_char)) m_count++;

			/* Special hack to count all of the mimics */
			else if (r_ptr->flags1 & (RF1_CHAR_MIMIC))
			{
				if (strchr(pat, '!')) m_count++;
			}
		}
	}

	default_join = C_ZNEW(m_count, join_t);
	monsters = C_ZNEW(m_count, int);

	m_count = 0;
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		if ((!cheat_know) && !l_list[i].sights) continue;
		if (!r_ptr->r_speed) continue;

		for (j = 0; j < (counter-1); j++)
		{
			const char *pat = (game_mode == GAME_NPPMORIA ? monster_group_nppmoria[j].chars : monster_group_nppangband[j].chars);
			if (j == 0 && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

			else if (j > 0)
			{
				if (strchr(pat, '!'))
				{
					if (!(r_ptr->flags1 & (RF1_CHAR_MIMIC))) continue;
				}
				else if (!strchr(pat, r_ptr->d_char)) continue;
			}

			monsters[m_count] = m_count;
			default_join[m_count].oid = i;
			default_join[m_count++].gid = j;
		}
	}

	if (game_mode == GAME_NPPMORIA) display_knowledge("monsters", monsters, m_count, r_funcs_nppmoria, m_funcs, "                   Sym  Kills");
	else display_knowledge("monsters", monsters, m_count, r_funcs_nppangband, m_funcs, "                   Sym  Kills");
	FREE(default_join);
	FREE(monsters);
}


static const grouper object_text_order[] =
{
	{TV_RING,			"Rings"			},
	{TV_AMULET,			"Amulets"		},
	{TV_POTION,			"Potions"		},
	{TV_SCROLL,			"Scrolls"		},
	{TV_WAND,			"Wands"			},
	{TV_STAFF,			"Staves"		},
	{TV_ROD,			"Rods"			},
	{TV_FOOD,			"Food"			},
	{TV_PRAYER_BOOK,	"Priest Books"	},
	{TV_DRUID_BOOK,		"Druid Books"	},
	{TV_MAGIC_BOOK,		"Mage Books"	},
	{TV_LIGHT,			"Lights"		},
	{TV_FLASK,			"Flasks"		},
	{TV_SWORD,			"Swords"		},
	{TV_POLEARM,		"Polearms"		},
	{TV_HAFTED,			"Hafted Weapons"},
	{TV_BOW,			"Bows"			},
	{TV_ARROW,			"Ammunition"	},
	{TV_BOLT,			NULL			},
	{TV_SHOT,			NULL			},
	{TV_SHIELD,			"Shields"		},
	{TV_CROWN,			"Crowns"		},
	{TV_HELM,			"Helms"			},
	{TV_GLOVES,			"Gloves"		},
	{TV_BOOTS,			"Boots"			},
	{TV_CLOAK,			"Cloaks"		},
	{TV_DRAG_ARMOR,		"Dragon Scale Mail" },
	{TV_DRAG_SHIELD,	"Dragon Scale Shields" },
	{TV_HARD_ARMOR,		"Hard Armors"	},
	{TV_SOFT_ARMOR,		"Soft Armors"	},
	{TV_CHEST,          "Chests"		},
	{TV_SPIKE,			"Spikes"		},
	{TV_DIGGING,		"Diggers"		},
	{TV_JUNK,			"Junk"			},
	{0,					NULL			}
};


/* =================== ARTIFACTS ==================================== */
/* Many-to-one grouping */

static void get_artifact_display_name(char *o_name, size_t namelen, int a_idx)
{
	object_type object_type_body;
	object_type *o_ptr = &object_type_body;

	/* Make fake artifact */
	o_ptr = &object_type_body;
	object_wipe(o_ptr);
	make_fake_artifact(o_ptr, a_idx);
	object_aware(o_ptr);
	object_known(o_ptr);
	o_ptr->ident |= (IDENT_MENTAL);

	/* Get its name */
	object_desc(o_name, namelen, o_ptr, ODESC_PREFIX | ODESC_BASE | ODESC_SPOIL);
}


/*
 * Display an artifact label
 */
static void display_artifact(int col, int row, bool cursor, int oid)
{
	char o_name[80];

	/* Choose a color */
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];

	get_artifact_display_name(o_name, sizeof o_name, oid);

	/* Display the name */
	c_prt(attr, o_name, row, col);
}


/*
 * Show artifact lore
 */
void desc_art_fake(int a_idx)
{
	object_type *o_ptr;
	object_type object_type_body;
	bool lost = TRUE;
	int i, j;

	/* Get local object */
	o_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Look for the artifact, either in inventory, store or the object list */
	for (i = 0; i < z_info->o_max; i++)
	{
		if (o_list[i].art_num == a_idx)
		{
			o_ptr = &o_list[i];
			lost = FALSE;
			break;
		}
	}

	if (lost)
	{
		for (i = 0; i < INVEN_TOTAL; i++)
		{
			if (inventory[i].art_num == a_idx)
			{
				o_ptr = &inventory[i];
				lost = FALSE;
				break;
			}
		}
	}

	if (lost)
	{
		for (j = 0; j < MAX_STORES; j++)
		{
			for (i = 0; i < store[j].stock_size; i++)
			{
				if (store[j].stock[i].art_num == a_idx)
				{
					o_ptr = &store[j].stock[i];
					lost = FALSE;
					break;
				}
			}
			if (!lost) break;
		}
	}

	/* If it's been lost, make a fake artifact for it */
	if (lost)
	{
		make_fake_artifact(o_ptr, a_idx);
		object_aware(o_ptr);
		object_known(o_ptr);
		o_ptr->ident |= (IDENT_MENTAL);
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	text_out_hook = text_out_to_screen;

	/* Print the artifact information */
	Term_gotoxy(0, 0);
	object_info_screen(o_ptr);

}


static int a_cmp_tval(const void *a, const void *b)
{
	const artifact_type *a_a = &a_info[*(const int *)a];
	const artifact_type *a_b = &a_info[*(const int *)b];

	/*group by */
	int ta = obj_group_order[a_a->tval];
	int tb = obj_group_order[a_b->tval];
	int c = ta - tb;
	if (c) return c;

	/* order by */
	c = a_a->sval - a_b->sval;
	if (c) return c;
	return strcmp(a_a->name, a_b->name);
}


static const char *kind_name(int gid)
{
	return object_text_order[gid].name;
}


static int art2gid(int oid)
{
	return obj_group_order[a_info[oid].tval];
}


/*
 * Check if the given artifact idx is something we should "Know" about
 */
static bool artifact_is_known(int a_idx)
{
	int i;
	store_type *st_ptr = &store[STORE_GUILD];
	artifact_type *a_ptr = &a_info[a_idx];

	/* Artifact doesn't exist at all, we are in wizard mode, or not created yet */
	if ((a_ptr->tval + a_ptr->sval) == 0) return FALSE;
	if (p_ptr->wizard) return TRUE;
	if (a_ptr->a_cur_num == 0) return FALSE;

	/* Check all objects to see if it exists but hasn't been IDed */
	for (i = 0; i < z_info->o_max; i++)
	{
		int a = o_list[i].art_num;

		/* If we haven't actually identified the artifact yet */
		if (a && a == a_idx && !object_is_known(&o_list[i]))
		{
			return FALSE;
		}
	}

	/* Check inventory for the same */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Ignore non-objects */
		if (!o_ptr->k_idx) continue;

		if ((o_ptr->art_num == a_idx) && !object_is_known(o_ptr))
		{
			return FALSE;
		}
	}

	/* Check guild to see if it is waiting as a quest reward */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		object_type *o_ptr = &st_ptr->stock[i];

		if (o_ptr->art_num == a_idx) return (FALSE);
	}

	return TRUE;
}


/* If 'artifacts' is NULL, it counts the number of known artifacts, otherwise
   it collects the list of known artifacts into 'artifacts' as well. */
static int collect_known_artifacts(int *artifacts, size_t artifacts_len)
{
	int a_count = 0;
	int j;

	if (artifacts)
		assert(artifacts_len >= z_info->art_max);

	for (j = 0; j < z_info->art_max; j++)
	{
		/* Artifact doesn't exist */
		if (!a_info[j].name) continue;

		if ((cheat_xtra) || artifact_is_known(j))
		{
			if (artifacts)
				artifacts[a_count++] = j;
			else
				a_count++;
		}
	}

	return a_count;
}


/*
 * Display known artifacts
 */
static void do_cmd_knowledge_artifacts(void *obj, const char *name)
{
	/* HACK -- should be TV_MAX */
	group_funcs obj_f = {TV_GOLD, FALSE, kind_name, a_cmp_tval, art2gid, 0};
	member_funcs art_f = {display_artifact, desc_art_fake, 0, 0, recall_prompt, 0, 0};

	int *artifacts;
	int a_count = 0;

	(void)obj;
	(void)name;

	artifacts = C_ZNEW(z_info->art_max, int);

	/* Collect valid artifacts */
	a_count = collect_known_artifacts(artifacts, z_info->art_max);

	display_knowledge("artifacts", artifacts, a_count, obj_f, art_f, NULL);
	FREE(artifacts);
}


/* =================== EGO ITEMS  ==================================== */
/* Many-to-many grouping (uses default join) */

/* static u16b *e_note(int oid) {return &e_info[default_join[oid].oid].note;} */
static const char *ego_grp_name(int gid) { return object_text_order[gid].name; }

static void display_ego_item(int col, int row, bool cursor, int oid)
{
	/* HACK: Access the object */
	ego_item_type *e_ptr = &e_info[default_join[oid].oid];

	/* Choose a color */
	byte attr = curs_attrs[0 != (int)e_ptr->everseen][0 != (int)cursor];

	/* Display the name */
	c_prt(attr, e_name + e_ptr->name, row, col);
}


/*
 * Describe fake ego item "lore"
 */
static void desc_ego_fake(int oid)
{
	/* Hack: dereference the join */
	const char *cursed[] = { "permanently cursed", "heavily cursed", "cursed" };
	const char *xtra[] = { "sustains", "higher resistances", "abilities", "immunities", "stat increases",
							"slays", "*slays*", "elemental brands", "elemental resists", "native abilities"};
	u32b f3, i;

	int e_idx = default_join[oid].oid;
	ego_item_type *e_ptr = &e_info[e_idx];

	object_type dummy;
	WIPE(&dummy, dummy);

	/* Save screen */
	screen_save();

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Dump the name */
	c_prt(TERM_L_BLUE, format("%s %s", ego_grp_name(default_group(oid)),
	                                   e_name + e_ptr->name), 0, 0);

	/* Begin recall */
	Term_gotoxy(0, 1);
	text_out("\n");

	if (e_ptr->text)
	{
		int x, y;
		text_out("%s", e_text + e_ptr->text);
		Term_locate(&x, &y);
		Term_gotoxy(0, y+1);
	}

	/* List ego flags */
	dummy.ego_num = e_idx;
	dummy.tval = e_ptr->tval[0];
	object_info_out(&dummy, FALSE);

	if (e_ptr->xtra)
		text_out(format("It provides one or more random %s.", xtra[e_ptr->xtra - 1]));

	for (i = 0, f3 = TR3_PERMA_CURSE; i < 3 ; f3 >>= 1, i++)
	{
		if (e_ptr->flags3 & f3)
		{
			text_out_c(TERM_RED, "It is %s.", cursed[i]);
			break;
		}
	}

	text_out_c(TERM_L_BLUE, "\n\n[Press any key to continue]\n");
	(void)anykey();

	screen_load();
}


/* TODO? Currently ego items will order by e_idx */
static int e_cmp_tval(const void *a, const void *b)
{
	const ego_item_type *ea = &e_info[default_join[*(const int *)a].oid];
	const ego_item_type *eb = &e_info[default_join[*(const int *)b].oid];

	/* Group by */
	int c = default_join[*(const int *)a].gid - default_join[*(const int *)b].gid;
	if (c) return c;

	/* Order by */
	return strcmp(e_name + ea->name, e_name + eb->name);
}


/*
 * Display known ego_items
 */
static void do_cmd_knowledge_ego_items(void *obj, const char *name)
{
	group_funcs obj_f =
		{TV_GOLD, FALSE, ego_grp_name, e_cmp_tval, default_group, 0};

	member_funcs ego_f = {display_ego_item, desc_ego_fake, 0, 0, recall_prompt, 0, 0};

	int *egoitems;
	int e_count = 0;
	int i, j;

	(void)obj;
	(void)name;

	/* HACK: currently no more than 3 tvals for one ego type */
	egoitems = C_ZNEW(z_info->e_max * EGO_TVALS_MAX, int);
	default_join = C_ZNEW(z_info->e_max * EGO_TVALS_MAX, join_t);

	for (i = 0; i < z_info->e_max; i++)
	{
		if (e_info[i].everseen || (cheat_xtra))
		{
			for (j = 0; j < EGO_TVALS_MAX && e_info[i].tval[j]; j++)
			{
				int gid = obj_group_order[e_info[i].tval[j]];

				/* Ignore duplicate gids */
				if (j > 0 && gid == default_join[e_count - 1].gid) continue;

				egoitems[e_count] = e_count;
				default_join[e_count].oid = i;
				default_join[e_count++].gid = gid;
			}
		}
	}

	display_knowledge("ego items", egoitems, e_count, obj_f, ego_f, NULL);

	FREE(default_join);
	FREE(egoitems);
}


/* =================== ORDINARY OBJECTS  ==================================== */
/* Many-to-one grouping */

/*
 * Looks up an artifact idx given an object_kind *that's already known
 * to be an artifact*.  Behaviour is distinctly unfriendly if passed
 * flavours which don't correspond to an artifact.
 */
static int get_artifact_from_kind(object_kind *k_ptr)
{
	int i;

	assert(k_ptr->k_flags3 & TR3_INSTA_ART);

	/* Look for the corresponding artifact */
	for (i = 0; i < z_info->art_max; i++)
	{
		if (k_ptr->tval == a_info[i].tval &&
		    k_ptr->sval == a_info[i].sval)
		{
			break;
		}
	}

	assert(i < z_info->art_max);

	return i;
}


/*
 * Display the objects in a group.
 */
static void display_object(int col, int row, bool cursor, int oid)
{
	int k_idx = oid;

	object_kind *k_ptr = &k_info[k_idx];
	const char *inscrip = get_autoinscription(oid);
	byte item_squelch = get_squelch_status(oid);
	byte squelch_display_color = squelch_status_color[item_squelch];
	cptr squelch_text = squelch_status[item_squelch];

	char o_name[80];

	/* Choose a color */
	bool aware = (!k_ptr->flavor || k_ptr->aware);
	byte attr = curs_attrs[(int)aware][(int)cursor];

	/* Find graphics bits -- versions of the object_char and object_attr defines */
	bool use_flavour = (k_ptr->flavor) && !(aware && k_ptr->tval == TV_SCROLL);

	byte a = use_flavour ? flavor_info[k_ptr->flavor].x_attr : k_ptr->x_attr;
	byte c = use_flavour ? flavor_info[k_ptr->flavor].x_char : k_ptr->x_char;

	/* Display known artifacts differently */
	if ((k_ptr->k_flags3 & TR3_INSTA_ART) && artifact_is_known(get_artifact_from_kind(k_ptr)))
	{
		get_artifact_display_name(o_name, sizeof(o_name), get_artifact_from_kind(k_ptr));
	}
	else
	{
 		strip_name(o_name, k_idx);
	}

	/* If the type is "tried", display that */
	if (k_ptr->tried && !aware)
		my_strcat(o_name, " {tried}", sizeof(o_name));

	/* Display the name */
	c_prt(attr, o_name, row, col);

	/* Show squelch status */
	c_put_str(squelch_display_color, squelch_text, row, 46);

	/* Show autoinscription if around */
	if (aware && inscrip)
		c_put_str(TERM_YELLOW, inscrip, row, 62);

	/* Display symbol */
	big_pad(76, row, a, c);
}


/*
 * Add a pval so the object descriptions don't look strange*
 */
void apply_magic_fake(object_type *o_ptr)
{
	/* Analyze type */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		{
			o_ptr->pval = 1;
			break;
		}

		/*many rings need a pval*/
		case TV_RING:
		{
			switch (o_ptr->sval)
			{
				case SV_RING_STR:
				case SV_RING_CON:
				case SV_RING_DEX:
				case SV_RING_INT:
				case SV_RING_SPEED:
				case SV_RING_SEARCHING:
				{
					o_ptr->pval = 1;
					break;
				}

				case SV_RING_AGGRAVATION:
				{
					o_ptr->ident |= (IDENT_CURSED);
					break;
				}
				case SV_RING_WEAKNESS:
				case SV_RING_STUPIDITY:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = -1;

					break;
				}
				/* WOE */
				case SV_RING_WOE:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->to_a = -1;
					o_ptr->pval = -1;

					break;
				}
				/* Ring that increase damage */
				case SV_RING_DAMAGE:
				{
					/* Bonus to damage */
					o_ptr->to_d = 1;

					break;
				}
				/* Ring that increase accuracy */
				case SV_RING_ACCURACY:
				{
					/* Bonus to hit */
					o_ptr->to_h = 1;

					break;
				}
				/* Rings that provide of Protection */
				case SV_RING_PROTECTION:
				case SV_RING_FLAMES:
				case SV_RING_ACID:
				case SV_RING_ICE:
				case SV_RING_LIGHTNING:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 1;

					break;
				}
				/* Rings that provide of Protection */
				case SV_RING_LORD_PROT_ACID:
				case SV_RING_LORD_PROT_FIRE:
				case SV_RING_LORD_PROT_COLD:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 5;

					break;
				}
				/*both to-hit and to-damage*/
				case SV_RING_SLAYING:
				{
					/* Bonus to damage and to hit */
					o_ptr->to_d = 1;
					o_ptr->to_h = 1;

					break;
				}
				default: break;

			}
			/*break for TVAL-Rings*/
			break;
		}

		case TV_AMULET:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Amulet of wisdom/charisma/infravision */
				case SV_AMULET_WISDOM:
				case SV_AMULET_CHARISMA:
				case SV_AMULET_INFRAVISION:
				case SV_AMULET_SEARCHING:
				case SV_AMULET_ESP:
				case SV_AMULET_DEVOTION:
				case SV_AMULET_TRICKERY:
				{
					/* Stat bonus */
					o_ptr->pval = 1;

					break;
				}

				/* Amulet of the Magi -- never cursed */
				case SV_AMULET_THE_MAGI:
				{
					o_ptr->pval = 1;
					o_ptr->to_a = 1;

					break;
				}

				/* Amulet of Weaponmastery -- never cursed */
				case SV_AMULET_WEAPONMASTERY:
				{
					o_ptr->to_h = 1;
					o_ptr->to_d = 1;
					o_ptr->pval = 1;

					break;
				}

				/* Amulet of Doom -- always cursed */
				case SV_AMULET_DOOM:
				case SV_AMULET_WOE:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = -1;
					o_ptr->to_a = -1;

					break;
				}

				default: break;

			}
			/*break for TVAL-Amulets*/
			break;
		}

		case TV_LIGHT:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				case SV_LIGHT_TORCH:
				case SV_LIGHT_LANTERN:
				{
					o_ptr->timeout = 1;

					break;
				}

			}
			/*break for TVAL-Lites*/
			break;
		}

		/*give then one charge*/
		case TV_STAFF:
		case TV_WAND:
		{
			o_ptr->pval = 1;

			break;
		}
	}
}


/*
 * Describe fake object
 */
static void desc_obj_fake(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];
	object_type object_type_body;
	object_type *o_ptr = &object_type_body;

	/* Check for known artifacts, display them as artifacts */
	if ((k_ptr->k_flags3 & TR3_INSTA_ART) && artifact_is_known(get_artifact_from_kind(k_ptr)))
	{
		desc_art_fake(get_artifact_from_kind(k_ptr));
		return;
	}

	/* Update the object recall window */
	track_object_kind(k_idx);
	handle_stuff();

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Create the object */
	object_prep(o_ptr, k_idx);

	/* Add minimum bonuses so the descriptions don't look strange. */
	apply_magic_fake(o_ptr);

	/* Hack -- its in the store */
	if (k_info[k_idx].aware) o_ptr->ident |= (IDENT_STORE);

	/* It's fully know */
	if (!k_info[k_idx].flavor)
	{
		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Print the artifact information */
	Term_gotoxy(0, 0);
	object_info_screen(o_ptr);

}


static int o_cmp_tval(const void *a, const void *b)
{
	const object_kind *k_a = &k_info[*(const int *)a];
	const object_kind *k_b = &k_info[*(const int *)b];

	/* Group by */
	int ta = obj_group_order[k_a->tval];
	int tb = obj_group_order[k_b->tval];
	int c = ta - tb;
	if (c) return c;

	/* Order by */
	c = k_a->aware - k_b->aware;
	if (c) return -c; /* aware has low sort weight */

	switch (k_a->tval)
	{
		case TV_LIGHT:
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_DRUID_BOOK:
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
			/* leave sorted by sval */
			break;

		default:
			if (k_a->aware)
				return strcmp(k_name + k_a->name, k_name + k_b->name);

			/* Then in tried order */
			c = k_a->tried - k_b->tried;
			if (c) return -c;

			return strcmp(flavor_text + flavor_info[k_a->flavor].text,
			              flavor_text + flavor_info[k_b->flavor].text);
	}

	return k_a->sval - k_b->sval;
}


static int obj2gid(int oid)
{
	return obj_group_order[k_info[oid].tval];
}


static char *o_xchar(int oid)
{
	object_kind *k_ptr = &k_info[oid];

	if (!k_ptr->flavor || k_ptr->aware)
		return &k_ptr->x_char;
	else
		return &flavor_info[k_ptr->flavor].x_char;
}


static byte *o_xattr(int oid)
{
	object_kind *k_ptr = &k_info[oid];

	if (!k_ptr->flavor || k_ptr->aware)
		return &k_ptr->x_attr;
	else
		return &flavor_info[k_ptr->flavor].x_attr;
}


/*
 * Display special prompt for object inscription.
 */
static const char *o_xtra_prompt(int oid)
{
	object_kind *k_ptr = &k_info[oid];
	s16b idx = get_autoinscription_index(oid);

	const char *no_insc = ", 's' to toggle squelch, 'r'ecall, '{'";
	const char *with_insc = ", 's' to toggle squelch, 'r'ecall, '{', '}'";

	button_add("|TOGGLESQUELCH",'s');
	button_add("|INSCRIBE", '{');


	/* Forget it if we've never seen the thing */
	if (k_ptr->flavor && !k_ptr->aware)
		return "";

	/* If it's already inscribed */
	if (idx != -1)
	{	button_add("|UNINSCRIBE", '}');
		return with_insc;
	}
	/* Else */
	button_kill('}');

	return no_insc;
}


/*
 * Special key actions for object inscription.
 */
static void o_xtra_act(char ch, int oid)
{
	object_kind *k_ptr = &k_info[oid];
	s16b idx = get_autoinscription_index(oid);

	/* Increase squelch */
	if (ch == 's' || ch == 'S')
	{
		change_squelch_setting(oid, 1);
	}

	/* Forget it if we've never seen the thing */
	if (k_ptr->flavor && !k_ptr->aware)
		return;

	/* Uninscribe */
	if (ch == '}')
	{
		if (idx != -1) remove_autoinscription(oid);
		return;
	}

	/* Inscribe */
	else if (ch == '{')
	{
		char note_text[80] = "";

		/* Avoid the prompt getting in the way */
		screen_save();

		/* Prompt */
		prt("Inscribe with: ", 0, 0);

		/* Default note */
		if (idx != -1)
			strnfmt(note_text, sizeof(note_text), "%s", get_autoinscription(oid));

		/* Get an inscription */
		if (askfor_aux(note_text, sizeof(note_text), NULL))
		{
			/* Remove old inscription if existent */
			if (idx != -1)
				remove_autoinscription(oid);

			/* Add the autoinscription */
			add_autoinscription(oid, note_text);

			/* Notice stuff (later) */
			p_ptr->notice |= (PN_AUTOINSCRIBE);
			p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
		}

		/* Reload the screen */
		screen_load();
	}
}


/*
 * Display known objects
 */
void do_cmd_knowledge_objects(void *obj, const char *name)
{
	group_funcs kind_f = {TV_GOLD, FALSE, kind_name, o_cmp_tval, obj2gid, 0};
	member_funcs obj_f = {display_object, desc_obj_fake, o_xchar, o_xattr, o_xtra_prompt, o_xtra_act, 0};

	int *objects;
	int o_count = 0;
	int i, j, c;

	(void)obj;
	(void)name;

	objects = C_ZNEW(z_info->k_max, int);

	for (i = 0; i < z_info->k_max; i++)
	{
		/* Access the object type */
		object_kind *k_ptr = &k_info[i];

		/*used to check for allocation*/
		int k = 0;

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
		if (!(k_ptr->aware && k_ptr->everseen)) continue;

		c = obj_group_order[k_info[i].tval];
		if (c >= 0) objects[o_count++] = i;
	}

	display_knowledge("known objects", objects, o_count, kind_f, obj_f, "Squelch         Inscribed    Sym");

	FREE(objects);
}


/* =================== TERRAIN FEATURES ==================================== */
/* Many-to-one grouping */

static void display_feature_lore(int oid)
{
	/* Get the feature index */
	int f_idx = oid;

	/* Update the monster recall window */
	p_ptr->feature_kind_idx = f_idx;
	handle_stuff();

	/* Save the screen */
	screen_save();

	/* Describe */
	text_out_hook = text_out_to_screen;

	/* Describe feature */
	feature_roff_top(f_idx);

	Term_gotoxy(0, 2);

	/* Recall feature */
	describe_feature(f_idx, FALSE);

	text_out_c(TERM_L_BLUE, "\n[Press any key to continue]\n");
	(void)anykey();

	/* Load the screen */
	screen_load();
}


/*
 * Display the features in a group.
 */
static void display_feature(int col, int row, bool cursor, int oid )
{
	/* Get the feature index */
	int f_idx = oid;

	/* Access the feature */
	feature_type *f_ptr = &f_info[f_idx];

	/* Choose a color */
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];

	/* Display the name */
	c_prt(attr, f_name + f_ptr->name, row, col);

	/* Display symbol */
	big_pad(68, row, f_ptr->x_attr, f_ptr->x_char);

	/* ILLUMINATION AND DARKNESS GO HERE */

}


static int f_cmp_fkind(const void *a, const void *b)
{
	const feature_type *fa = &f_info[*(const int *)a];
	const feature_type *fb = &f_info[*(const int *)b];

	/* group by */
	int c = feat_order(*(const int *)a) - feat_order(*(const int *)b);
	if (c) return c;

	/* order by feature name */
	return strcmp(f_name + fa->name, f_name + fb->name);
}


static const char *fkind_name(int gid)
{
	return feature_group_text[gid];
}


static byte *f_xattr(int oid)
{
	return &f_info[oid].x_attr;
}


static char *f_xchar(int oid)
{
	return &f_info[oid].x_char;
}


/*
 * Interact with feature visuals.
 */
static void do_cmd_knowledge_features(void *obj, const char *name)
{
	group_funcs fkind_f = {N_ELEMENTS(feature_group_text), FALSE,
							fkind_name, f_cmp_fkind, feat_order, 0};

	member_funcs feat_f = {display_feature, display_feature_lore, f_xchar, f_xattr, recall_prompt, 0, 0};

	int *features;
	int f_count = 0;
	int i;

	(void)obj;
	(void)name;

	features = C_ZNEW(z_info->f_max, int);

	for (i = 0; i < z_info->f_max; i++)
	{
		/* Access the features */
		feature_type *f_ptr = &f_info[i];
		feature_lore *f_l_ptr = &f_l_list[i];

		/* Skip empty features */
		if (!f_ptr->name) continue;

		/* Skip features we never see */
		if (f_ptr->f_mimic != i) continue;

		/* Don't spoil terrain */
		if (!f_ptr->f_everseen && !f_l_ptr->f_l_sights && !cheat_know) continue;

		features[f_count++] = i;
	}

	display_knowledge("features", features, f_count, fkind_f, feat_f,
		"                    Sym");
	FREE(features);
}


/* =================== END JOIN DEFINITIONS ================================ */

/*
 * Display the high scores
 */
static void do_cmd_knowledge_scores(void *obj, const char *name)
{
	(void)obj;
	(void)name;
	show_scores();
}


/*
 * display the notes file
 */
void do_cmd_knowledge_notes(void)
{
	/*close the notes file for writing*/
	file_close(notes_file);

	show_file(notes_fname, "Notes", 0, 0);

	/*re-open for appending*/
	notes_file = file_open(notes_fname, MODE_APPEND, FTYPE_TEXT);

}


/*
 * display the notes file
 */
static void textui_knowledge_notes(void *obj, const char *name)
{
	(void)obj;
	(void)name;

	do_cmd_knowledge_notes();
}


/*
 * Display contents of the Home. Code taken from the player death interface
 * and the show known objects function. -LM-
 */
static void do_cmd_knowledge_home(void *obj, const char *name)
{
	int k;

	store_type *st_ptr = &store[STORE_HOME];

	ang_file *fff;

	char o_name[120];
	cptr which_set;
	char file_name[1024];

	(void)obj;
	(void)name;

	/* Temporary file */
	path_build(file_name, sizeof(file_name), ANGBAND_DIR_USER, "temp_kills.tmp");
	fff = file_open(file_name, MODE_WRITE, FTYPE_TEXT);

	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Failure */
	if (!fff) return;

	if (rogue_like_commands) which_set = roguelike_home_letters;
	else which_set = standard_home_letters;

	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Display contents of the home */
		for (k = 0; k < st_ptr->stock_num; k++)
		{
			object_desc(o_name, sizeof(o_name), &st_ptr->stock[k], ODESC_PREFIX | ODESC_FULL);
			file_putf(fff, "%c) %s\n", which_set[k], o_name);

			/* Describe random object attributes*/
			identify_random_gen(&st_ptr->stock[k]);
		}
	}

	else file_putf(fff, "[Your Home Is Empty]\n\n");

	/* Close the file */
	file_close(fff);

	/* Display the file contents */
	show_file(file_name, "Contents of Your Home", 0, 2);

	/* Remove the file */
	file_delete(file_name);
}


/*
 * Display kill counts
 */
static void do_cmd_knowledge_kills(void *obj, const char *name)
{
	int n, i;

	ang_file *fff;
	char file_name[1024];

	u16b *who;
	u16b why = 4;

	(void)obj;
	(void)name;

	/* Temporary file */
	path_build(file_name, sizeof(file_name), ANGBAND_DIR_USER, "temp_kills.tmp");
	fff = file_open(file_name, MODE_WRITE, FTYPE_TEXT);

	/* Failure */
	if (!fff) return;

	/* Allocate the "who" array */
	who = C_ZNEW(z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Require non-unique monsters */
		if (r_ptr->flags1 & RF1_UNIQUE) continue;

		/* Collect "appropriate" monsters */
		if (l_ptr->pkills > 0) who[n++] = i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort by kills (and level) */
	ang_sort(who, &why, n);

	/* Print the monsters (highest kill counts first) */
	for (i = n - 1; i >= 0; i--)
	{
		monster_race *r_ptr = &r_info[who[i]];
		monster_lore *l_ptr = &l_list[who[i]];

		/* Print a message */
		file_putf(fff, "     %-40s  %5d\n", r_ptr->name_full, l_ptr->pkills);
	}

	/* Free the "who" array */
	FREE(who);

	/* Close the file */
	file_close(fff);

	/* Display the file contents */
	show_file(file_name, "Kill counts", 0, 0);

	/* Remove the file */
	file_delete(file_name);
}


/*
 * Definition of the "player knowledge" menu.
 */
static menu_item knowledge_actions[] =
{
{ {0, "Display object knowledge",	do_cmd_knowledge_objects,	0}, 'a', 0 },
{ {0, "Display artifact knowledge",	do_cmd_knowledge_artifacts,	0}, 'b', 0 },
{ {0, "Display ego item knowledge",	do_cmd_knowledge_ego_items,	0}, 'c', 0 },
{ {0, "Display monster knowledge",	do_cmd_knowledge_monsters,	0}, 'd', 0 },
{ {0, "Display feature knowledge",	do_cmd_knowledge_features,	0}, 'e', 0 },
{ {0, "Display hall of fame",		do_cmd_knowledge_scores,	0}, 'f', 0 },
{ {0, "Display notes file",			textui_knowledge_notes,		0}, 'g', 0 },
{ {0, "Display home inventory",		do_cmd_knowledge_home,		0}, 'h', 0 },
{ {0, "Display kill counts",		do_cmd_knowledge_kills,		0}, 'k', 0 },
};

static menu_type knowledge_menu;


/*
 * Keep macro counts happy.
 */
static void cleanup_cmds(void)
{
	FREE(obj_group_order);
}


void init_cmd_know(void)
{
	/* Initialize the menus */
	menu_type *menu = &knowledge_menu;
	WIPE(menu, menu_type);
	menu->title = "Display current knowledge";
	menu->menu_data = knowledge_actions;
	menu->count = N_ELEMENTS(knowledge_actions),
	menu_init(menu, MN_SKIN_SCROLL, find_menu_iter(MN_ITER_ITEMS), &SCREEN_REGION);

	/* initialize other static variables */
	if (!obj_group_order)
	{
		int i;
		int gid = -1;

		obj_group_order = C_ZNEW(TV_GOLD + 1, int);
		atexit(cleanup_cmds);

		/* Allow for missing values */
		for (i = 0; i <= TV_GOLD; i++)
			obj_group_order[i] = -1;

		for (i = 0; 0 != object_text_order[i].tval; i++)
		{
			if (object_text_order[i].name) gid = i;
			obj_group_order[object_text_order[i].tval] = gid;
		}
	}
}


/*
 * Display the "player knowledge" menu.
 */
void do_cmd_knowledge(void)
{
	int cursor = 0;
	int i;
	ui_event_data c = EVENT_EMPTY;
	region knowledge_region = { 0, 0, -1, 11 };

	/* Grey out menu items that won't display anything */
	if (collect_known_artifacts(NULL, 0) > 0)
		knowledge_actions[1].flags = 0;
	else
		knowledge_actions[1].flags = MN_GREYED;

	knowledge_actions[2].flags = MN_GREYED;
	for (i = 0; i < z_info->e_max; i++)
	{
		if (e_info[i].everseen || (cheat_xtra))
		{
			knowledge_actions[2].flags = 0;
			break;
		}
	}

	if (count_known_monsters() > 0)
		knowledge_actions[3].flags = 0;
	else
		knowledge_actions[3].flags = MN_GREYED;

	screen_save();
	menu_layout(&knowledge_menu, &knowledge_region);

	/* Back up the buttons */
	button_backup_all();
	button_kill_all();

	while (c.key != ESCAPE)
	{
		clear_from(0);
		button_kill_all();
		button_add("ESC", ESCAPE);
		event_signal(EVENT_MOUSEBUTTONS);
		c = menu_select(&knowledge_menu, &cursor, 0);
	}

	screen_load();

	button_restore();
	event_signal(EVENT_MOUSEBUTTONS);

}

