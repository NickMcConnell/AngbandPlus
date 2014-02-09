
/* File: wizard2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
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


#ifdef ALLOW_DEBUG

/*
 * Show the extents of all flows
 */
static void do_cmd_wiz_show_flows(void)
{
	/* Some custom info for each flow */
	static struct
	{
		const char *name;
	} flow_info[MAX_FLOWS] =
	{
		{"NO DOORS"},
		{"PASS DOORS"},
		{"FLYING"},
		{"FLYING_NO_DOORS"},
		{"PASS WALLS"},
		{"LAVA"},
		{"ICE"},
		{"OIL"},
		{"FIRE"},
		{"SAND"},
		{"FOREST"},
		{"WATER"},
		{"ACID"},
		{"MUD"},
		{"LAVA_NO_DOORS"},
		{"ICE_NO_DOORS"},
		{"OIL_NO_DOORS"},
		{"FIRE_NO_DOORS"},
		{"SAND_NO_DOORS"},
		{"FOREST_NO_DOORS"},
		{"WATER_NO_DOORS"},
		{"ACID_NO_DOORS"},
		{"MUD_NO_DOORS"}
	};
	int flow, y, x;

	/* Scan the flows */
	for (flow = 0; flow < MAX_FLOWS; flow++)
	{
		bool flag = FALSE;

		/* Ignore empty flows */
		if (cost_at_center[flow] < 1) continue;

		/* Scan the visible grids */
		for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
		{
			for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
			{
				/* Default character/attribute */
				byte c = '#';
				byte a = TERM_RED;

				/* Ignore annoying grids */
				if (!in_bounds(y, x)) continue;

				/* The flow doesn't reach this grid */
				if (cave_cost[flow][y][x] < 1) continue;

				/* Found one */
				flag = TRUE;

				/* Special character/attribute for passable grids */
				if (cave_ff1_match(y, x, FF1_MOVE))
				{
					c = '*';
				       	a = TERM_YELLOW;
				}

				/* Special character for player */
				if (cave_m_idx[y][x] == -1)
				{
					c = '@';
				}
				/* Special character for monsters */
				else if (cave_m_idx[y][x] > 0)
				{
					c = 'X';
				}

				/* Mark the grid */
				print_rel(c, a, y, x);
			}
		}

		/* We couldn't find a grid in that flow */
		if (!flag) continue;

		/* Show a messsage */
		msg_format("Flow: %s.", flow_info[flow].name);

		/* Pause */
		message_flush();

		/* Redraw the map */
		prt_map();
	}
}


/*
 * * Debug scent trails and noise bursts.
 */

static void do_cmd_wiz_flow(void)
 {
	char cmd;

	int i, y, x, y2, x2;

	/* Get a "debug command" */
	if (!get_com("Press 'S' for scent, 'N' for noise info:", &cmd)) return;


	/* Analyze the command */
	switch (cmd)
 	{

		case 'S':
		case 's':
 		{
#ifdef MONSTER_SMELL
			/* Update map */
			for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
 			{
				for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
				{


					byte a;

					int age = get_scent(y, x);

					/* Must have scent */
					if (age == -1) continue;

					/* Pretty colors by age */
					if (age > SMELL_STRENGTH) a = TERM_L_DARK;

					else if (age < 10) a = TERM_BLUE;
					else if (age < 20) a = TERM_L_BLUE;
					else if (age < 30) a = TERM_GREEN;
					else if (age < 40) a = TERM_L_GREEN;
					else if (age < 50) a = TERM_YELLOW;
					else if (age < 60) a = TERM_ORANGE;
					else if (age < 70) a = TERM_L_RED;
					else a = TERM_RED;


					/* Display player/floors/walls */
					if ((y == py) && (x == px))
					{
						print_rel('@', a, y, x);
					}
					else
					{
						print_rel('0' + (age % 10), a, y, x);
					}
 				}
			}

			/* Prompt */
			prt("Scent ages", 0, 0);

			/* Wait for a keypress */
			(void)inkey();

			/* Redraw map */
			prt_map();

#else 	/*MONSTER_SMELL*/
			msg_print("Monster scent is not enabled in this copy of the game");
#endif /*MONSTER_SMELL*/

			break;

		}


		case 'N':
		case 'n':
		{

			/* Update map */
			for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
 			{
				for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
				{
					u16b lowest_cost = cave_cost[FLOW_NO_DOORS][y][x];
					int dir = -1;
					u16b cost;
					if (lowest_cost == 0) continue;

					for (i = 0; i < 8; i++)
					{
						/* Get the location */
						y2 = y + ddy_ddd[i];
						x2 = x + ddx_ddd[i];

						cost = cave_cost[FLOW_NO_DOORS][y2][x2];
						if (!cost) continue;

						/* If this grid's scent is younger, save it */
						if (lowest_cost > cost) lowest_cost = cost;

						/* If it isn't, look elsewhere */
						else continue;

						/* Save this direction */
						dir = i;
					}

					/* If we didn't find any younger scent, print a '5' */
					if (dir == -1) print_rel('5', TERM_YELLOW, y, x);

					/* Otherwise, convert to true direction and print */
					else
					{
						i = ddd[dir];
						print_rel('0' + i, TERM_L_BLUE, y, x);
					}
				}
 			}

			/* Prompt */
			prt("Directions given to advancing monsters using noise info", 0, 0);

			/* Wait for a keypress */
			(void)inkey();

			/* Redraw map */
			prt_map();

			break;

		}


		default:
		{
			break;
		}
 	}

 	/* Done */
	prt("", 0, 0);

	/* Redraw map */
	prt_map();

}



/*
 * Output a long int in binary format.
 */
static void prt_binary(u32b flags, int row, int col)
{
	int i;
	u32b bitmask;

	/* Scan the flags */
	for (i = bitmask = 1; i <= 32; i++, bitmask *= 2)
	{
		/* Dump set bits */
		if (flags & bitmask)
		{
			Term_putch(col++, row, TERM_BLUE, '*');
		}

		/* Dump unset bits */
		else
		{
			Term_putch(col++, row, TERM_WHITE, '-');
		}
	}
}


/*
 * Hack -- Teleport to the target
 */
static void do_cmd_wiz_bamf(void)
{
	/* Must have a target */
	if (target_okay())
	{
		/* Teleport to the target */
		teleport_player_to(p_ptr->target_row, p_ptr->target_col);
	}
}



/*
 * Aux function for "do_cmd_wiz_change()"
 */
static void do_cmd_wiz_change_aux(void)
{
	int i;

	int tmp_int;

	long tmp_long;

	char tmp_val[160];

	char ppp[80];


	/* Query the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Prompt */
		strnfmt(ppp, sizeof(ppp), "%s (3-118): ", stat_names[i]);

		/* Default */
		sprintf(tmp_val, "%d", p_ptr->stat_max[i]);

		/* Query */
		if (!get_string(ppp, tmp_val, 4)) return;

		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Verify */
		if (tmp_int > 18+100) tmp_int = 18+100;
		else if (tmp_int < 3) tmp_int = 3;

		/* Save it */
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = tmp_int;
	}


	/* Default */
	sprintf(tmp_val, "%ld", (long)(p_ptr->au));

	/* Query */
	if (!get_string("Gold: ", tmp_val, 10)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->au = tmp_long;


	/* Default */
	sprintf(tmp_val, "%ld", (long)(p_ptr->exp));

	/* Query */
	if (!get_string("Experience: ", tmp_val, 10)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->exp = tmp_long;

	/* Update */
	check_experience();

	/* Default */
	sprintf(tmp_val, "%ld", (long)(p_ptr->max_exp));

	/* Query */
	if (!get_string("Max Exp: ", tmp_val, 10)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->max_exp = tmp_long;

	/* Update */
	check_experience();

	/* Default */
	sprintf(tmp_val, "%ld", (long)(p_ptr->q_fame));

	/* Query */
	if (!get_string("Fame: ", tmp_val, 10)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->q_fame = tmp_long;

}


/*
 * Change various "permanent" player variables.
 */
static void do_cmd_wiz_change(void)
{
	/* Interact */
	do_cmd_wiz_change_aux();

	/* Redraw everything */
	do_cmd_redraw();
}



/*
 * Wizard routines for creating objects and modifying them
 *
 * This has been rewritten to make the whole procedure
 * of debugging objects much easier and more comfortable.
 *
 * Here are the low-level functions
 *
 * - wiz_display_item()
 *     display an item's debug-info
 * - wiz_create_itemtype()
 *     specify tval and sval (type and subtype of object)
 * - wiz_tweak_item()
 *     specify pval, +AC, +tohit, +todam
 *     Note that the wizard can leave this function anytime,
 *     thus accepting the default-values for the remaining values.
 *     pval comes first now, since it is most important.
 * - wiz_reroll_item()
 *     apply some magic to the item or turn it into an artifact.
 * - wiz_roll_item()
 *     Get some statistics about the rarity of an item:
 *     We create a lot of fake items and see if they are of the
 *     same type (tval and sval), then we compare pval and +AC.
 *     If the fake-item is better or equal it is counted.
 *     Note that cursed items that are better or equal (absolute values)
 *     are counted, too.
 *     HINT: This is *very* useful for balancing the game!
 * - wiz_quantity_item()
 *     change the quantity of an item, but be sane about it.
 *
 * And now the high-level functions
 * - do_cmd_wiz_play()
 *     play with an existing object
 * - wiz_create_item()
 *     create a new object
 *
 * Note -- You do not have to specify "pval" and other item-properties
 * directly. Just apply magic until you are satisfied with the item.
 *
 * Note -- For some items (such as wands, staffs, some rings, etc), you
 * must apply magic, or you will get "broken" or "uncharged" objects.
 *
 * Note -- Redefining artifacts via "do_cmd_wiz_play()" may destroy
 * the artifact.  Be careful.
 *
 * Hack -- this function will allow you to create multiple artifacts.
 * This "feature" may induce crashes or other nasty effects.
 */


/*
 * Display an item's properties
 */
static void wiz_display_item(const object_type *o_ptr)
{
	int j = 0;

	u32b f1, f2, f3, native;

	char buf[256];


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &native);

	/* Clear screen */
	Term_clear();

	/* Describe fully */
	object_desc_spoil(buf, sizeof(buf), o_ptr, TRUE, 3);

	prt(buf, 2, j);

	prt(format("kind = %-5d  level = %-4d  tval = %-5d  sval = %-5d",
	           o_ptr->k_idx, k_info[o_ptr->k_idx].k_level,
	           o_ptr->tval, o_ptr->sval), 4, j);

	prt(format("number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",
	           o_ptr->number, o_ptr->weight,
	           o_ptr->ac, o_ptr->dd, o_ptr->ds), 5, j);

	prt(format("pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",
	           o_ptr->pval, o_ptr->to_a, o_ptr->to_h, o_ptr->to_d), 6, j);

	prt(format("name1 = %-4d  name2 = %-4d  cost = %ld",
	           o_ptr->art_num, o_ptr->ego_num, (long)object_value(o_ptr)), 7, j);

	prt(format("ident = %04x  timeout = %-d",
	           o_ptr->ident, o_ptr->timeout), 8, j);

	prt("+------------FLAGS1------------+", 10, j);
	prt("AFFECT..........SLAY.......BRAND", 11, j);
	prt("                ae      xxxaefcp", 12, j);
	prt("siwdcc  ssitsasmnvudotgddduclioo", 13, j);
	prt("tnieoh  trnupthgiinmrrnrrmnierli", 14, j);
	prt("rtsxna..lcfndkttmldncltggnddceds", 15, j);
	prt_binary(f1, 16, j);

	prt("+------------FLAGS2------------+", 17, j);
	prt("SUST.......IMM..RESIST.........", 18, j);
	prt("           afecpaefcpfldbc s n  ", 19, j);
	prt("siwdcc     ciloocliooeialoshnecd", 20, j);
	prt("tnieoh     ireliierliatrnnnrethi", 21, j);
	prt("rtsxna.....decdsdcedsrekdfddxhss", 22, j);
	prt_binary(f2, 23, j);

	prt("+------------FLAGS3------------+", 10, j+32);
	prt("s   ts h     tadiiii   aiehs  hp", 11, j+32);
	prt("lf  eefoni   egrgggg  bcnaih  vr", 12, j+32);
	prt("we  lerler  ilgannnn  ltssdo  ym", 13, j+32);
	prt("da reiedvo  merirrrr  eityew ccc", 14, j+32);
	prt("itlepnelpn  ppanaefc  svaktm uuu", 15, j+32);
	prt("ghigavaiim  aoveclio  saanyo rrr", 16, j+32);
	prt("seteticfca  craxierl  etropd sss", 17, j+32);
	prt("trenhstekn  tttpdced  detwes eee", 18, j+32);
	prt_binary(f3, 19, j+32);

	prt("o_ptr->ident:", 20, j+34);
	prt(format("sense  %c  empty   %c  known   %c",
		(o_ptr->ident & IDENT_SENSE) ? '+' : ' ',
		(o_ptr->ident & IDENT_EMPTY) ? '+' : ' ',
		(o_ptr->ident & IDENT_KNOWN) ? '+' : ' '), 21, j+34);
	prt(format("store  %c  effect  %c  ",
		(o_ptr->ident & IDENT_STORE) ? '+' : ' ',
		(o_ptr->ident & IDENT_EFFECT) ? '+' : ' '), 22, j+34);
	prt(format("indest %c  ego    %c ",
		(o_ptr->ident & IDENT_INDESTRUCT) ? '+' : ' '), 23, j+34);
}


/*
 * A list of tvals and their textual names
 */
static const tval_desc tvals[] =
{
	{ TV_SWORD,             "Sword"                },
	{ TV_POLEARM,           "Polearm"              },
	{ TV_HAFTED,            "Hafted Weapon"        },
	{ TV_BOW,               "Bow"                  },
	{ TV_ARROW,             "Arrows"               },
	{ TV_BOLT,              "Bolts"                },
	{ TV_SHOT,              "Shots"                },
	{ TV_SHIELD,            "Shield"               },
	{ TV_CROWN,             "Crown"                },
	{ TV_HELM,              "Helm"                 },
	{ TV_GLOVES,            "Gloves"               },
	{ TV_BOOTS,             "Boots"                },
	{ TV_CLOAK,             "Cloak"                },
	{ TV_DRAG_ARMOR,        "Dragon Scale Mail"    },
	{ TV_DRAG_SHIELD,       "Dragon Scale Shield"  },
	{ TV_HARD_ARMOR,        "Hard Armor"           },
	{ TV_SOFT_ARMOR,        "Soft Armor"           },
	{ TV_RING,              "Ring"                 },
	{ TV_AMULET,            "Amulet"               },
	{ TV_LIGHT,              "Lite"                },
	{ TV_POTION,            "Potion"               },
	{ TV_SCROLL,            "Scroll"               },
	{ TV_WAND,              "Wand"                 },
	{ TV_STAFF,             "Staff"                },
	{ TV_ROD,               "Rod"                  },
	{ TV_PRAYER_BOOK,       "Priest Book"          },
	{ TV_MAGIC_BOOK,        "Magic Book"           },
	{ TV_DRUID_BOOK,        "Druid Book"           },
	{ TV_SPIKE,             "Spikes"               },
	{ TV_DIGGING,           "Digger"               },
	{ TV_CHEST,             "Chest"                },
	{ TV_FOOD,              "Food"                 },
	{ TV_FLASK,             "Flask"                },
	{ TV_SKELETON,          "Skeletons"            },
	{ TV_BOTTLE,            "Empty bottle"         },
	{ TV_JUNK,              "Junk"                 },
	{ 0,                    NULL                   }
};


/*
 * Get an object kind for creation (or zero)
 *
 * List up to 60 choices in three columns
 */
static int wiz_create_itemtype(void)
{
	int i, num, max_num;
	int col, row;
	int tval;

	cptr tval2_desc;
	char ch;

	int choice[60];
	static const char choice_name[] = "abcdefghijklmnopqrst"
	                                  "ABCDEFGHIJKLMNOPQRST"
	                                  "0123456789:;<=>?@%&*";
	const char *cp;

	char buf[160];


	/* Clear screen */
	Term_clear();

	/* Print all tval's and their descriptions */
	for (num = 0; (num < 60) && tvals[num].tval; num++)
	{
		row = 2 + (num % 20);
		col = 30 * (num / 20);
		ch  = choice_name[num];
		prt(format("[%c] %s", ch, tvals[num].desc), row, col);
	}


	/* We need to know the maximal possible tval_index */
	max_num = num;

	/* Choose! */
	if (!get_com("Get what type of object? ", &ch)) return (0);

	/* Analyze choice */
	num = -1;
	if ((cp = strchr(choice_name, ch)) != NULL)
		num = cp - choice_name;

	/* Bail out if choice is illegal */
	if ((num < 0) || (num >= max_num)) return (0);

	/* Base object type chosen, fill in tval */
	tval = tvals[num].tval;
	tval2_desc = tvals[num].desc;


	/*** And now we go for k_idx ***/

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 60) && (i < z_info->k_max); i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Analyze matching items */
		if (k_ptr->tval == tval)
		{
			/* Hack -- Skip instant artifacts */
			if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;

			/* Prepare it */
			row = 2 + (num % 20);
			col = 30 * (num / 20);
			ch  = choice_name[num];

			/* Get the "name" of object "i" */
			strip_name(buf, i);

			/* Print it */
			prt(format("[%c] %s", ch, buf), row, col);

			/* Remember the object index */
			choice[num++] = i;
		}
	}

	/* Me need to know the maximal possible remembered object_index */
	max_num = num;

	/* Choose! */
	if (!get_com(format("What Kind of %s? ", tval2_desc), &ch)) return (0);

	/* Analyze choice */
	num = -1;
	if ((cp = strchr(choice_name, ch)) != NULL)
		num = cp - choice_name;

	/* Bail out if choice is "illegal" */
	if ((num < 0) || (num >= max_num)) return (0);

	/* And return successful */
	return (choice[num]);
}


/*
 * Tweak an item
 */
static void wiz_tweak_item(object_type *o_ptr)
{
	cptr p;
	char tmp_val[80];


	/* Hack -- leave artifacts alone */
	if (artifact_p(o_ptr)) return;

	p = "Enter new 'pval' setting: ";
	sprintf(tmp_val, "%d", o_ptr->pval);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->pval = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_a' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_a);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->to_a = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_h' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_h);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->to_h = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_d' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_d);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->to_d = atoi(tmp_val);
	wiz_display_item(o_ptr);
}


/*
 * Apply magic to an item or turn it into an artifact. -Bernd-
 */
static void wiz_reroll_item(object_type *o_ptr)
{
	object_type *i_ptr;
	object_type object_type_body;

	char ch;

	bool changed = FALSE;


	/* Hack -- leave artifacts alone */
	if (artifact_p(o_ptr)) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the object */
	object_copy(i_ptr, o_ptr);


	/* Main loop. Ask for magification and artifactification */
	while (TRUE)
	{
		/* Display full item debug information */
		wiz_display_item(i_ptr);

		/* Ask wizard what to do. */
		if (!get_com("[a]ccept, [n]ormal, [g]ood, [e]xcellent? ", &ch))
			break;

		/* Create/change it! */
		if (ch == 'A' || ch == 'a')
		{
			changed = TRUE;
			break;
		}

		/* Apply normal magic, but first clear object */
		else if (ch == 'n' || ch == 'N')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, effective_depth(p_ptr->depth), FALSE, FALSE, FALSE, FALSE);
		}

		/* Apply good magic, but first clear object */
		else if (ch == 'g' || ch == 'g')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, effective_depth(p_ptr->depth), FALSE, TRUE, FALSE, FALSE);
		}

		/* Apply great magic, but first clear object */
		else if (ch == 'e' || ch == 'e')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, effective_depth(p_ptr->depth), FALSE, TRUE, TRUE, FALSE);
		}
	}


	/* Notice change */
	if (changed)
	{
		/* Remember */
		object_history(i_ptr, ORIGIN_CHEAT, 0);

		/* Restore the position information */
		i_ptr->iy = o_ptr->iy;
		i_ptr->ix = o_ptr->ix;
		i_ptr->next_o_idx = o_ptr->next_o_idx;
		i_ptr->marked = o_ptr->marked;

		/* Apply changes */
		object_copy(o_ptr, i_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS | PU_NATIVE);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	}
}



/*
 * Maximum number of rolls
 */
#define TEST_ROLL 100000


/*
 * Try to create an item again. Output some statistics.    -Bernd-
 *
 * The statistics are correct now.  We acquire a clean grid, and then
 * repeatedly place an object in this grid, copying it into an item
 * holder, and then deleting the object.  We fiddle with the artifact
 * counter flags to prevent weirdness.  We use the items to collect
 * statistics on item creation relative to the initial item.
 */
static void wiz_statistics(object_type *o_ptr)
{
	long i, matches, better, worse, other;

	char ch;
	cptr quality;

	bool good, great;

	object_type *i_ptr;
	object_type object_type_body;

	cptr q = "Rolls: %ld, Matches: %ld, Better: %ld, Worse: %ld, Other: %ld";


	/* Mega-Hack -- allow multiple artifacts XXX XXX XXX */
	if (artifact_p(o_ptr)) a_info[o_ptr->art_num].a_cur_num = 0;


	/* Interact */
	while (TRUE)
	{
		cptr pmt = "Roll for [n]ormal, [g]ood, or [e]xcellent treasure? ";

		/* Display item */
		wiz_display_item(o_ptr);

		/* Get choices */
		if (!get_com(pmt, &ch)) break;

		if (ch == 'n' || ch == 'N')
		{
			good = FALSE;
			great = FALSE;
			quality = "normal";
		}
		else if (ch == 'g' || ch == 'G')
		{
			good = TRUE;
			great = FALSE;
			quality = "good";
		}
		else if (ch == 'e' || ch == 'E')
		{
			good = TRUE;
			great = TRUE;
			quality = "excellent";
		}
		else
		{
#if 0 /* unused */
			good = FALSE;
			great = FALSE;
#endif /* unused */
			break;
		}

		/* Let us know what we are doing */
		msg_format("Creating a lot of %s items. Base level = %d.",
		           quality, effective_depth(p_ptr->depth));
		message_flush();

		/* Set counters to zero */
		matches = better = worse = other = 0;

		/* Let's rock and roll */
		for (i = 0; i <= TEST_ROLL; i++)
		{
			/* Output every few rolls */
			if ((i < 100) || (i % 100 == 0))
			{
				/* Do not wait */
				inkey_scan = TRUE;

				/* Allow interupt */
				if (inkey())
				{
					/* Flush */
					flush();

					/* Stop rolling */
					break;
				}

				/* Dump the stats */
				prt(format(q, i, matches, better, worse, other), 0, 0);
				(void)Term_fresh();
			}


			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Create an object */
			make_object(i_ptr, good, great, DROP_TYPE_UNTHEMED, FALSE);

			/* Mega-Hack -- allow multiple artifacts XXX XXX XXX */
			if (artifact_p(i_ptr)) a_info[i_ptr->art_num].a_cur_num = 0;

			/* Test for the same tval and sval. */
			if ((o_ptr->tval) != (i_ptr->tval)) continue;
			if ((o_ptr->sval) != (i_ptr->sval)) continue;

			/* Check for match */
			if ((i_ptr->pval == o_ptr->pval) &&
			    (i_ptr->to_a == o_ptr->to_a) &&
			    (i_ptr->to_h == o_ptr->to_h) &&
			    (i_ptr->to_d == o_ptr->to_d))
			{
				matches++;
			}

			/* Check for better */
			else if ((i_ptr->pval >= o_ptr->pval) &&
			         (i_ptr->to_a >= o_ptr->to_a) &&
			         (i_ptr->to_h >= o_ptr->to_h) &&
			         (i_ptr->to_d >= o_ptr->to_d))
			{
				better++;
			}

			/* Check for worse */
			else if ((i_ptr->pval <= o_ptr->pval) &&
			         (i_ptr->to_a <= o_ptr->to_a) &&
			         (i_ptr->to_h <= o_ptr->to_h) &&
			         (i_ptr->to_d <= o_ptr->to_d))
			{
				worse++;
			}

			/* Assume different */
			else
			{
				other++;
			}
		}

		/* Final dump */
		msg_format(q, i, matches, better, worse, other);
		message_flush();
	}


	/* Hack -- Normally only make a single artifact */
	if (artifact_p(o_ptr)) a_info[o_ptr->art_num].a_cur_num = 1;
}


/*
 * Change the quantity of a the item
 */
static void wiz_quantity_item(object_type *o_ptr, bool carried)
{
	int tmp_int;

	char tmp_val[3];


	/* Never duplicate artifacts */
	if (artifact_p(o_ptr)) return;


	/* Default */
	sprintf(tmp_val, "%d", o_ptr->number);

	/* Query */
	if (get_string("Quantity: ", tmp_val, 3))
	{
		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Paranoia */
		if (tmp_int < 1) tmp_int = 1;
		if (tmp_int > 99) tmp_int = 99;

		/* Adjust total weight being carried */
		if (carried) p_ptr->update |= (PU_BONUS);

		/* Adjust charge for rods */
		if (o_ptr->tval == TV_ROD)
		{
			o_ptr->pval = (o_ptr->pval / o_ptr->number) * tmp_int;
		}

		/* Accept modifications */
		o_ptr->number = tmp_int;
	}
}



/*
 * Play with an item. Options include:
 *   - Output statistics (via wiz_roll_item)
 *   - Reroll item (via wiz_reroll_item)
 *   - Change properties (via wiz_tweak_item)
 *   - Change the number of items (via wiz_quantity_item)
 */
static void do_cmd_wiz_play(void)
{
	int item;

	object_type *i_ptr;
	object_type object_type_body;

	object_type *o_ptr;

	char ch;

	cptr q, s;

	bool changed = FALSE;


	/* Get an item */
	q = "Play with which object? ";
	s = "You have nothing to play with.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Save screen */
	screen_save();


	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy object */
	object_copy(i_ptr, o_ptr);


	/* The main loop */
	while (TRUE)
	{
		/* Display the item */
		wiz_display_item(i_ptr);

		/* Get choice */
		if (!get_com("[a]ccept [s]tatistics [r]eroll [t]weak [q]uantity? ", &ch))
			break;

		if (ch == 'A' || ch == 'a')
		{
			changed = TRUE;
			break;
		}

		if (ch == 's' || ch == 'S')
		{
			wiz_statistics(i_ptr);
		}

		if (ch == 'r' || ch == 'r')
		{
			wiz_reroll_item(i_ptr);
		}

		if (ch == 't' || ch == 'T')
		{
			wiz_tweak_item(i_ptr);
		}

		if (ch == 'q' || ch == 'Q')
		{
			bool carried = (item >= 0) ? TRUE : FALSE;
			wiz_quantity_item(i_ptr, carried);
		}
	}


	/* Load screen */
	screen_load();


	/* Accept change */
	if (changed)
	{
		/* Message */
		msg_print("Changes accepted.");

		/* Change */
		object_copy(o_ptr, i_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS | PU_NATIVE);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

	}

	/* Ignore change */
	else
	{
		msg_print("Changes ignored.");
	}
}


/*
 * Wizard routine for creating objects
 *
 * Note that wizards cannot create objects on top of other objects.
 *
 * Hack -- this routine always makes a "dungeon object", and applies
 * magic to it, and attempts to decline cursed items. XXX XXX XXX
 */
static void wiz_create_item(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *i_ptr;
	object_type object_type_body;

	int k_idx;


	/* Save screen */
	screen_save();

	/* Get object base type */
	k_idx = wiz_create_itemtype();

	/* Load screen */
	screen_load();


	/* Return if failed */
	if (!k_idx) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create the item */
	object_prep(i_ptr, k_idx);

	/* Apply magic (no messages, no artifacts) */
	apply_magic(i_ptr, effective_depth(p_ptr->depth), FALSE, FALSE, FALSE, FALSE);

	/* Remember history */
	object_history(i_ptr, ORIGIN_CHEAT, 0);

	/* Drop the object from heaven */
	drop_near(i_ptr, -1, py, px);

	/* All done */
	msg_print("Allocated.");
}


/*
 * Create the artifact with the specified number
 */
static void wiz_create_artifact(void)
{
	object_type *i_ptr;
	object_type object_type_body;
	int k_idx, a_idx;

	cptr p;
	char tmp_val[80] = "";

	artifact_type *a_ptr;

	p = "Enter artifact number: ";

	/*Get the artifact Number*/
	if (!get_string(p, tmp_val, 6)) return;

	a_idx = atoi(tmp_val);

	/*Get teh artifact num*/
	a_ptr = &a_info[a_idx];

	/* Ignore "empty" artifacts */
	if (a_ptr->tval + a_ptr->sval == 0) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Acquire the "kind" index */
	k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!k_idx) return;

	/* Create the artifact */
	object_prep(i_ptr, k_idx);

	/* Save the name */
	i_ptr->art_num = a_idx;

	/* Extract the fields */
	i_ptr->pval = a_ptr->pval;
	i_ptr->ac = a_ptr->ac;
	i_ptr->dd = a_ptr->dd;
	i_ptr->ds = a_ptr->ds;
	i_ptr->to_a = a_ptr->to_a;
	i_ptr->to_h = a_ptr->to_h;
	i_ptr->to_d = a_ptr->to_d;
	i_ptr->weight = a_ptr->weight;

	/* Remember history */
	object_history(i_ptr, ORIGIN_CHEAT, 0);

	/* Drop the artifact from heaven */
	drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

	/* All done */
	msg_print("Allocated.");
}


/*
 * Cure everything instantly
 */
static void do_cmd_wiz_cure_all(void)
{
	/* Remove curses */
	(void)remove_all_curse();

	/* Restore stats */
	(void)res_stat(A_STR);
	(void)res_stat(A_INT);
	(void)res_stat(A_WIS);
	(void)res_stat(A_CON);
	(void)res_stat(A_DEX);
	(void)res_stat(A_CHR);

	/* Restore the level */
	(void)restore_level();

	/* Heal the player */
	p_ptr->chp = p_ptr->mhp;
	p_ptr->chp_frac = 0;

	/* Restore mana */
	p_ptr->csp = p_ptr->msp;
	p_ptr->csp_frac = 0;

	/* Cure stuff */
	(void)clear_timed(TMD_BLIND, TRUE);
	(void)clear_timed(TMD_CONFUSED, TRUE);
	(void)clear_timed(TMD_POISONED, TRUE);
	(void)clear_timed(TMD_AFRAID, TRUE);
	(void)clear_timed(TMD_PARALYZED, TRUE);
	(void)clear_timed(TMD_IMAGE, TRUE);
	(void)clear_timed(TMD_STUN, TRUE);
	(void)clear_timed(TMD_CUT, TRUE);
	(void)clear_timed(TMD_SLOW, TRUE);

	/* No longer hungry */
	(void)set_food(PY_FOOD_MAX - 1);

	/* Redraw everything */
	do_cmd_redraw();
}



/*
 * Go to any level
 */
static void do_cmd_wiz_jump(void)
{
	/* Ask for level */
	if (p_ptr->command_arg <= 0)
	{
		char ppp[80];

		char tmp_val[160];

		/* Prompt */
		sprintf(ppp, "Jump to level (0-%d): ", MAX_DEPTH-1);

		/* Default */
		sprintf(tmp_val, "%d", p_ptr->depth);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 11)) return;

		/* Extract request */
		p_ptr->command_arg = atoi(tmp_val);
	}

	/* Paranoia */
	if (p_ptr->command_arg < 0) p_ptr->command_arg = 0;

	/* Paranoia */
	if (p_ptr->command_arg > MAX_DEPTH - 1) p_ptr->command_arg = MAX_DEPTH - 1;

	/* Accept request */
	msg_format("You jump to dungeon level %d.", p_ptr->command_arg);

	/* New depth */
	dungeon_change_level(p_ptr->command_arg);

}


/*
 * Become aware of a lot of objects
 */
static void do_cmd_wiz_learn(void)
{
	int i;

	object_type *i_ptr;
	object_type object_type_body;

	/* Scan every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Induce awareness */
		if (k_ptr->k_level <= p_ptr->command_arg)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Prepare object */
			object_prep(i_ptr, i);

			/* Awareness */
			object_aware(i_ptr);
		}
	}
}

/*
 * Become aware of a lot of objects
 */
static void do_cmd_wiz_know_all(void)
{
	int i, j;

	/* Knowledge of every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		k_info[i].aware = TRUE;
		k_info[i].everseen = TRUE;
		k_info[i].tried = TRUE;

	}
	/* Knowledge of every ego-item */
	for (i = 1; i < z_info->e_max; i++)
	{
		e_info[i].everseen = TRUE;
	}
	/* Full knowledge of every monster */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Know all flags */
		l_ptr->r_l_flags1 = r_ptr->flags1;
		l_ptr->r_l_flags2 = r_ptr->flags2;
		l_ptr->r_l_flags3 = r_ptr->flags3;
		l_ptr->r_l_flags4 = r_ptr->flags4;
		l_ptr->r_l_flags5 = r_ptr->flags5;
		l_ptr->r_l_flags6 = r_ptr->flags6;
		l_ptr->r_l_flags7 = r_ptr->flags7;
		l_ptr->r_l_native = r_ptr->r_native;

		/* Know max sightings, sleeping habits, spellcasting, and combat blows. */
		l_ptr->sights = MAX_SHORT;
		l_ptr->ranged = MAX_UCHAR;
		for (j = 0; j < MONSTER_BLOW_MAX; j++)
		{
			l_ptr->blows[j] = MAX_UCHAR;
		}
		l_ptr->wake = l_ptr->ignore = MAX_UCHAR;

		/* know the treasure drops*/
		l_ptr->drop_gold = l_ptr->drop_item =
		(((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
		 ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

		/* But only "valid" treasure drops */
		if (r_ptr->flags1 & RF1_ONLY_GOLD) l_ptr->drop_item = 0;
		if (r_ptr->flags1 & RF1_ONLY_ITEM) l_ptr->drop_gold = 0;
	}

	/* Full knowledge of every feature */
	for (i = 1; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];
		feature_lore *f_l_ptr = &f_l_list[i];

		/* Know all flags and sites */
		f_l_ptr->f_l_flags1 = f_ptr->f_flags1;
		f_l_ptr->f_l_flags2 = f_ptr->f_flags2;
		f_l_ptr->f_l_flags3 = f_ptr->f_flags3;
		f_l_ptr->f_l_sights = MAX_UCHAR;

		/* Know all transitions */
		f_l_ptr->f_l_defaults = MAX_UCHAR;
		for (j = 0; j < MAX_FEAT_STATES; j++)
		{
			/*There isn't an action here*/
			if (f_ptr->state[j].fs_action == FS_FLAGS_END) continue;

			/* Hack -- we have seen this transition */
			f_l_ptr->f_l_state[j] = MAX_UCHAR;
		}
		/*Know movement, damage to non-native, and stealth.....*/
		f_l_ptr->f_l_dam_non_native = MAX_UCHAR;
		f_l_ptr->f_l_native_moves = MAX_UCHAR;
		f_l_ptr->f_l_non_native_moves = MAX_UCHAR;
		f_l_ptr->f_l_stealth_adj = MAX_UCHAR;
		f_l_ptr->f_l_native_to_hit_adj = MAX_UCHAR;
		f_l_ptr->f_l_non_native_to_hit_adj = MAX_UCHAR;
	}
}



/*
 * Hack -- Rerate Hitpoints
 */
static void do_cmd_rerate(void)
{
	int min_value, max_value, i, percent;

	min_value = (z_info->max_level * 3 * (p_ptr->hitdie - 1)) / 8;
	min_value += z_info->max_level;

	max_value = (z_info->max_level * 5 * (p_ptr->hitdie - 1)) / 8;
	max_value += z_info->max_level;

	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* Rerate */
	while (1)
	{
		/* Collect values */
		for (i = 1; i < z_info->max_level; i++)
		{
			p_ptr->player_hp[i] = randint(p_ptr->hitdie);
			p_ptr->player_hp[i] += p_ptr->player_hp[i - 1];
		}

		/* Legal values */
		if ((p_ptr->player_hp[z_info->max_level - 1] >= min_value) &&
		    (p_ptr->player_hp[z_info->max_level - 1] <= max_value)) break;
	}

	percent = (int)(((long)p_ptr->player_hp[z_info->max_level - 1] * 200L) /
	                (p_ptr->hitdie + ((z_info->max_level - 1) * p_ptr->hitdie)));

	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Handle stuff */
	handle_stuff();

	/* Message */
	msg_format("Current Life Rating is %d/100.", percent);
}


/*
 * Summon some creatures
 */
static void do_cmd_wiz_summon(int num)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i;

	for (i = 0; i < num; i++)
	{
		(void)summon_specific(py, px, p_ptr->depth, 0, 0L);
	}
}


/*
 * Summon a creature of the specified type
 *
 * This function is rather dangerous XXX XXX XXX
 */
static void do_cmd_wiz_named(int r_idx, bool slp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, x, y;

	/* Prepare the place_monster flags */
	byte mp_flags = (MPLACE_GROUP | MPLACE_OVERRIDE);
	if (slp) mp_flags |= MPLACE_SLEEP;

	/* Paranoia */
	if (!r_idx) return;
	if (r_idx >= z_info->r_max-1) return;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, py, px, d, 0);

		/* Require empty grids */
		if (!cave_empty_bold(y, x)) continue;

		/* Place it (allow groups) */
		if (place_monster_aux(y, x, r_idx, mp_flags)) break;
	}
}



/*
 * Hack -- Delete all nearby monsters
 */
static void do_cmd_wiz_zap(int d)
{
	int i;

	/* Banish everyone nearby */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > d) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Quest monsters can only be "killed" by the player */
		if (m_ptr->mflag & (MFLAG_QUEST)) continue;

		/* Delete the monster */
		delete_monster_idx(i);
	}

	/* Update monster list window */
	p_ptr->redraw |= PR_MONLIST;

}


/*
 * Un-hide all monsters
 */
static void do_cmd_wiz_unhide(int d)
{
	int i;

	/* Process monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > d) continue;

		/* Optimize -- Repair flags */
		repair_mflag_mark = TRUE;
		repair_mflag_show = TRUE;

		/* Detect the monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(i, FALSE);
	}
}


/*
 * Query the dungeon
 */
static void do_cmd_wiz_query(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	char cmd;

	u16b mask = 0x00;


	/* Get a "debug command" */
	if (!get_com("Debug Command Query: ", &cmd)) return;

	/* Extract a flag */
	switch (cmd)
	{
		case '0': mask = (1 << 0); break;
		case '1': mask = (1 << 1); break;
		case '2': mask = (1 << 2); break;
		case '3': mask = (1 << 3); break;
		case '4': mask = (1 << 4); break;
		case '5': mask = (1 << 5); break;
		case '6': mask = (1 << 6); break;
		case '7': mask = (1 << 7); break;

		case 'm': mask |= (CAVE_MARK); break;
		case 'g': mask |= (CAVE_GLOW); break;
		case 'r': mask |= (CAVE_ROOM); break;
		case 'i': mask |= (CAVE_ICKY); break;
		case 's': mask |= (CAVE_SEEN); break;
		case 'v': mask |= (CAVE_VIEW); break;
		case 't': mask |= (CAVE_TEMP); break;
		case 'l': mask |= (CAVE_LOS); break;
		case 'f': mask |= (CAVE_FIRE); break;
		case 'V': mask |= (CAVE_G_VAULT); break;
		case 'h': mask |= (CAVE_HALO); break;
		case 'p': mask |= (CAVE_PROJECT); break;
		case 'o': mask |= (CAVE_MOVE); break;
	}

	/* Scan map */
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			byte a = TERM_RED;

			if (!in_bounds_fully(y, x)) continue;

			/* Given mask, show only those grids */
			if (mask && !(cave_info[y][x] & mask)) continue;

			/* Given no mask, show unknown grids */
			if (!mask && (cave_info[y][x] & (CAVE_MARK))) continue;

			/* Color */
			if (cave_los_bold(y, x)) a = TERM_YELLOW;

			/* Display player/floors/walls */
			if ((y == py) && (x == px))
			{
				print_rel('@', a, y, x);
			}
			else if (cave_los_bold(y, x))
			{
				print_rel('*', a, y, x);
			}
			else
			{
				print_rel('#', a, y, x);
			}
		}
	}

	/* Get keypress */
	msg_print("Press any key.");
	message_flush();

	/* Redraw map */
	prt_map();
}

static void wiz_create_items(void)
{
	int i;
	object_type object_type_body;

	object_type *i_ptr;

	for(i=0; i < 25; i++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make a object (if possible) */
		if (!make_object(i_ptr, FALSE, FALSE, DROP_TYPE_UNTHEMED, FALSE)) continue;

		/* Drop the object */
		drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

	}
}

/* More informative than spoiler information */
static void do_cmd_wiz_know_quests(void)
{
	int i;

	msg_print(format("p_ptr->cur_quest is %d", guild_quest_level()));

	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		msg_print(format("quest #%d, name is %s, type is %d", i, q_name + q_ptr->name, q_ptr->q_type));
		msg_print(format("quest #%d, level is %d, mon_race is %d, theme is %d", i, q_ptr->base_level, q_ptr->mon_idx, q_ptr->q_theme));
		msg_print(format("quest #%d, max num is is %d, num killed is %d, ", i, q_ptr->q_max_num, q_ptr->q_num_killed));
		if (q_ptr->q_flags & (QFLAG_STARTED)) msg_print(format("quest #%d is started", i));
		if (q_ptr->q_flags & (QFLAG_COMPLETED)) msg_print(format("quest #%d is completed", i));
	}
}

/* Create a specific monster grid */
static void do_cmd_wiz_monster(void)
{

	int x, y;
	char buf[10];
	monster_race *r_ptr;
	int r_idx;

	int mon_num = 1;
	int attempts_left = 10000;

	/* Default monster */
	strnfmt(buf, sizeof(buf), "%d", mon_num);

	/* Ask for a monster number */
	if (!get_string("Create a specific monster?: ", buf, sizeof(buf)))
	{
		return;
	}

	/* Convert to number */
	r_idx = atoi(buf);

	/* Get the i'th race */
	r_ptr = &r_info[r_idx];

	/* Check sanity */
	if ((r_idx < 1) || (r_idx >= z_info->r_max) || (!r_ptr->r_speed))
	{
		msg_print("Invalid monster number");

		return;
	}

	/* Find a legal, distant, unoccupied, space */
	while (attempts_left)
	{
		--attempts_left;

		/* Pick a location */
		y = rand_int(p_ptr->cur_map_hgt);
		x = rand_int(p_ptr->cur_map_wid);

		/* Require a grid that all monsters can exist in. */
		if (!cave_empty_bold(y, x)) continue;

		/*Success*/
		break;

	}

	/* Place the monster */
	if(!place_monster_aux(y, x, r_idx, 0L))
	{
		msg_print("Monster placement failed");
	}
	else msg_print("Monster placement succeeded");

	p_ptr->redraw |= (PR_MAP | PR_MONLIST);
}


/* Create a specific terrain grid given its feature number or query a grid */
static void do_cmd_wiz_feature(void)
{
	char ch;
	int x, y;
	static int feat = FEAT_FLOOR;
	char buf[10];
	bool do_create = FALSE;

	/* Show the options */
	if (!get_com("Pick a terrain command: [c]reate [q]uery? ", &ch))
	{
		return;
	}

	/* Feature creation was chosen */
	if ((ch == 'c') || (ch == 'C'))
	{
		int tmp_feat;

		/* Default feature */
		strnfmt(buf, sizeof(buf), "%d", (int)feat);

		/* Ask for a feature number */
		if (!get_string("Enter a feature number: ", buf, sizeof(buf)))
		{
			return;
		}

		/* Convert to number */
		tmp_feat = atoi(buf);

		/* Check sanity */
		if ((tmp_feat < 1) || (tmp_feat >= z_info->f_max) ||
			(!f_info[tmp_feat].name))
		{
			msg_print("Invalid feature number");

			return;
		}

		/* Accept the feature */
		feat = tmp_feat;

		/* Ask for creation */
		do_create = TRUE;
	}
	/* Ignore all commands but "query" */
	else if ((ch != 'q') && (ch != 'Q'))
	{
		return;
	}

	/* Pick a location */
	if (!target_set_interactive(TARGET_GRID, -1, -1)) return;

	/* Paranoia */
	if (!p_ptr->target_set) return;

	y = p_ptr->target_row;
	x = p_ptr->target_col;

	/* Feature creation */
	if (do_create)
	{
		/* Paranoia */
		if (cave_o_idx[y][x] || cave_m_idx[y][x])
		{
			msg_print("Must be an empty grid");

			return;
		}

		/* Create the feature */
		/*build_terrain(y, x, feat);*/
		cave_set_feat(y, x, feat);

	}
	/* Query a dungeon grid */
	else
	{
		char name[80];

		/* Get the feature */
		feat = cave_feat[y][x];

		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, FALSE, FALSE);

		/* Show the name */
		msg_format("You see '%s' (%d)", name, feat);
	}
}

/*
 * Ask for and parse a "debug command"
 *
 * The "p_ptr->command_arg" may have been set.
 */
void do_cmd_debug(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	char cmd;


	/* Get a "debug command" */
	if (!get_com("Debug Command: ", &cmd)) return;

	/* Analyze the command */
	switch (cmd)
	{
		/* Ignore */
		case ESCAPE:
		case ' ':
		case '\n':
		case '\r':
		{
			break;
		}

#ifdef ALLOW_SPOILERS

		/* Hack -- Generate Spoilers */
		case '"':
		{
			do_cmd_spoilers();
			break;
		}

#endif


		/* Hack -- Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Cure all maladies */
		case 'a':
		{
			do_cmd_wiz_cure_all();
			break;
		}

		/* Teleport to target */
		case 'b':
		{
			do_cmd_wiz_bamf();
			break;
		}

		/* Create any object */
		case 'c':
		{
			wiz_create_item();
			break;
		}

		/* Create and drop objects */
		case 'M':
		{
			wiz_create_items();
			break;
		}

		/* Create an artifact */
		case 'C':
		{
			wiz_create_artifact();
			break;
		}

		/* Detect everything */
		case 'd':
		{
			wiz_light();
			(void)detect(DETECT_RADIUS, DETECT_ALL);
			break;
		}

		/* Edit character */
		case 'e':
		{
			do_cmd_wiz_change();
			break;
		}

		/* View item info */
		case 'f':
		{
			(void)identify_fully();
			break;
		}

		/* Good Objects */
		case 'g':
		{
			if (p_ptr->command_arg <= 0) p_ptr->command_arg = 1;
			acquirement(py, px, p_ptr->command_arg, FALSE);
			break;
		}

		/* Hitpoint rerating */
		case 'h':
		{
			do_cmd_rerate();
			break;
		}

		/* Identify */
		case 'i':
		{
			(void)mass_identify(3);
			break;
		}

		/* Go up or down in the dungeon */
		case 'j':
		{
			do_cmd_wiz_jump();
			break;
		}

		/* Self-Knowledge */
		case 'k':
		{
			self_knowledge();
			break;
		}

		case 'K':
		{
			do_cmd_wiz_know_all();
		}

		/* Learn about objects */
		case 'l':
		{
			do_cmd_wiz_learn();
			break;
		}

		/* Magic Mapping */
		case 'm':
		{
			detect(DETECT_RADIUS + 10, DETECT_MAP);
			break;
		}

		/* Summon Named Monster */
		case 'n':
		{
			do_cmd_wiz_named(p_ptr->command_arg, TRUE);
			break;
		}

		/* Object playing routines */
		case 'o':
		{
			do_cmd_wiz_play();
			break;
		}

		/* Phase Door */
		case 'p':
		{
			teleport_player(10, FALSE);
			break;
		}

		/* Query the dungeon */
		case 'Q':
		{
			do_cmd_wiz_know_quests();
			break;
		}

		/* Query the dungeon */
		case 'q':
		{
			do_cmd_wiz_query();
			break;
		}

		/* Summon Random Monster(s) */
		case 's':
		{
			if (p_ptr->command_arg <= 0) p_ptr->command_arg = 1;
			do_cmd_wiz_summon(p_ptr->command_arg);
			break;
		}

		/* Teleport */
		case 't':
		{
			teleport_player(100, FALSE);
			break;
		}

		/* Un-hide all monsters */
		case 'u':
		{
			if (p_ptr->command_arg <= 0) p_ptr->command_arg = 255;
			do_cmd_wiz_unhide(p_ptr->command_arg);
			break;
		}

		/* Very Good Objects */
		case 'v':
		{
			if (p_ptr->command_arg <= 0) p_ptr->command_arg = 1;
			acquirement(py, px, p_ptr->command_arg, TRUE);
			break;
		}

		/* Wizard Light the Level */
		case 'w':
		{
			wiz_light();
			break;
		}

		/* Increase Experience */
		case 'x':
		{
			if (p_ptr->command_arg)
			{
				gain_exp(p_ptr->command_arg);
			}
			else
			{
				gain_exp(p_ptr->exp + 1);
			}
			break;
		}

		/* Zap Monsters (Banishment) */
		case 'z':
		{
			if (p_ptr->command_arg <= 0) p_ptr->command_arg = MAX_SIGHT;
			do_cmd_wiz_zap(p_ptr->command_arg);
			break;
		}

		/* Hack */
		case '_':
		{
			do_cmd_wiz_flow();
			break;
		}

		/* Create terrain or query dungeon grid */
		case 'F':
		{
			do_cmd_wiz_feature();
			break;
		}

		/* Create a specific monster */
		case 'R':
		{
			do_cmd_wiz_monster();
			break;
		}

		/* Re-draw the dungeon*/
		case 'D':
		{
			/* Leaving */
			p_ptr->leaving = TRUE;
			p_ptr->autosave = TRUE;
			break;
		}

		/* Display flows */
		case '/':
		{
			do_cmd_wiz_show_flows();
			break;
		}


		/* Oops */
		default:
		{
			msg_print("That is not a valid debug command.");
			break;
		}
	}
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif


