/* File: wizard2.c */

/*
 * The wizard & debugging commands and their effects.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



#ifdef ALLOW_DEBUG

/*
 * Debug scent trails and noise bursts.
 */
static void do_cmd_wiz_flow(void)
{
	char cmd;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, y2, x2;

	/* Get a "debug command" */
	if (!get_com("Press 'S' for scent, 'N' for noise info:", &cmd)) return;


	/* Analyze the command */
	switch (cmd)
	{
		case 'S':
		case 's':
		{
			/* Update map */
			for (y = p_ptr->wy; y < p_ptr->wy + map_rows; y++)
			{
				for (x = p_ptr->wx; x < p_ptr->wx + map_cols; x++)
				{
					byte a;

					/* How old is the scent here? */
					int age = get_scent(y, x);

					/* Must have scent */
					if (age == -1) continue;

					/* Pretty colors by age */
					if (age > SMELL_STRENGTH) a = TERM_L_DARK;
					else if (age < 10)        a = TERM_BLUE;
					else if (age < 20)        a = TERM_L_BLUE;
					else if (age < 30)        a = TERM_GREEN;
					else if (age < 40)        a = TERM_L_GREEN;
					else if (age < 50)        a = TERM_YELLOW;
					else if (age < 60)        a = TERM_ORANGE;
					else if (age < 70)        a = TERM_L_RED;
					else                      a = TERM_RED;


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
			(void)inkey(ALLOW_CLICK);

			/* Redraw map */
			prt_map();

			break;

		}

		case 'N':
		case 'n':
		{

			/* Get a debug command */
			if (!get_com("Press 'D' for direction of flow, 'C' for actual cost values:", &cmd)) return;

			/* Indicate directions */
			if ((cmd == 'D') || (cmd == 'd'))
			{
				/* Update map */
				for (y = p_ptr->wy; y < p_ptr->wy + map_rows; y++)
				{
					for (x = p_ptr->wx; x < p_ptr->wx + map_cols; x++)
					{
						int lowest_cost = cave_cost[y][x];
						int dir = -1;
						int cost;

						if (lowest_cost == 0) continue;

						for (i = 0; i < 8; i++)
						{
							/* Get the location */
							y2 = y + ddy_ddd[i];
							x2 = x + ddx_ddd[i];

							cost = cave_cost[y2][x2];
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
				(void)inkey(ALLOW_CLICK);

				/* Redraw map */
				prt_map();
			}

			/* Actual cost values */
			else
			{
				int j;

				for (i = cost_at_center - 2; i <= 100 + NOISE_STRENGTH; ++i)
				{
					/* First show grids with no scent */
					if (i == cost_at_center - 2) j = 0;

					/* Then show specially marked grids (bug-checking) */
					else if (i == cost_at_center - 1) j = 255;

					/* Then show standard grids */
					else j = i;

					/* Update map */
					for (y = p_ptr->wy; y < p_ptr->wy + map_rows; y++)
					{
						for (x = p_ptr->wx; x < p_ptr->wx + map_cols; x++)
						{
							byte a = TERM_YELLOW;

							/* Display proper cost */
							if (cave_cost[y][x] != j) continue;

							/* Display player/floors/walls */
							if ((y == py) && (x == px))
							{
								print_rel('@', a, y, x);
							}
							else if (cave_passable_bold(y, x))
							{
								print_rel('*', a, y, x);
							}
							else
							{
								print_rel('#', a, y, x);
							}
						}
					}

					/* Prompt */
					if (j == 0)
					{
						prt("Grids with no scent", 0, 0);
					}
					else if (j == 255)
					{
						prt("Specially marked grids", 0, 0);
					}
					else
					{
						prt(format("Depth %d: ", j), 0, 0);
					}

					/* Get key */
					if (inkey(ALLOW_CLICK) == ESCAPE) break;

					/* Redraw map */
					prt_map();
				}
			}

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
 * Change various character variables
 */
static void do_cmd_wiz_change(void)
{
	int i;

	int tmp_int;

	long tmp_long;

	char tmp_val[32];

	char ppp[32];


	/* Query the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Prompt */
		(void)strnfmt(ppp, sizeof(ppp), "%s (3-118):", stat_names[i]);

		/* Default */
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", p_ptr->stat_max[i]);

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
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%ld", (long)(p_ptr->au));

	/* Query */
	if (!get_string("Gold:", tmp_val, 10)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->au = tmp_long;


	/* Default */
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%ld", (long)(p_ptr->exp));

	/* Query */
	if (!get_string("Experience:", tmp_val, 10)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->exp = tmp_long;

	/* Update */
	check_experience();

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
 * - wiz_statistics()
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
	int i, pval;
	int a = TERM_L_BLUE;
	cptr s = "           ";

	u32b f[4];

	char buf[256];

	int level;

	/* Extract the flags */
	object_flags(o_ptr, &f[1], &f[2], &f[3]);

	/* Get level -EB- */
	if (!artifact_p(o_ptr)) level = k_info[o_ptr->k_idx].level;
	else level = a_info[o_ptr->artifact_index].level;


	/* Clear screen */
	(void)Term_clear();

	/* Describe fully */
	object_desc_store(buf, sizeof(buf), o_ptr, TRUE, 3);
	prt(buf, 2, 0);

	/* Display the current generation level */
	center_string(buf, sizeof(buf), format("Generation depth:  %d", p_ptr->depth), 80);
	c_put_str(TERM_L_GREEN, buf, 3, 0);

	/* Print descriptions of various object fields */
	prt(format("k_idx   =%sdamage  =%sarmor =%s ident     =", s, s, s), 4, 0);
	prt(format("tval    =%sto_d    =%sto_a   =%s note      =", s, s, s), 5, 0);
	prt(format("sval    =%sto_h    =%slevel  =%s inscrip   =", s, s, s), 6, 0);

	prt(format("number  =%sweight  =%scost   =%s art_index =", s, s, s), 7, 0);
	prt(format("activat =%stimeout =%sadjust =%s ego_index =", s, s, s), 8, 0);

	prt(format("pval1   =%sflags   =%sflags1 =%s flavor    =", s, s, s), 9, 0);
	prt(format("pval2   =%sflags   =%sflags2 =", s, s), 10, 0);
	prt(format("pval3   =%sflags   =%sflags3 =", s, s), 11, 0);

	/* Print the values for these fields */
	c_put_str(a, format("%-5d", o_ptr->k_idx),              4, 10);
	c_put_str(a, format("%-5d", o_ptr->tval),               5, 10);
	c_put_str(a, format("%-5d", o_ptr->sval),               6, 10);
	c_put_str(a, format("%-5d", o_ptr->number),             7, 10);
	c_put_str(a, format("%-5d", o_ptr->activate),           8, 10);
	c_put_str(a, format("%-5d", o_ptr->pval),               9, 10);
	c_put_str(a, format("%-5d", o_ptr->pval2),             10, 10);
	c_put_str(a, format("%-5d", o_ptr->pval3),             11, 10);

	c_put_str(a, format("%dd%-3d", o_ptr->dd, o_ptr->ds),   4, 30);
	c_put_str(a, format("%-4d", o_ptr->to_d),               5, 30);
	c_put_str(a, format("%-4d", o_ptr->to_h),               6, 30);
	c_put_str(a, format("%-6d", o_ptr->weight),             7, 30);
	c_put_str(a, format("%-6d", o_ptr->timeout),            8, 30);
	c_put_str(a, format("%lx", o_ptr->flags_pval1),         9, 30);
	c_put_str(a, format("%lx", o_ptr->flags_pval2),        10, 30);
	c_put_str(a, format("%lx", o_ptr->flags_pval3),        11, 30);

	c_put_str(a, format("%-5d", o_ptr->ac),                 4, 49);
	c_put_str(a, format("%-5d", o_ptr->to_a),               5, 49);
	c_put_str(a, format("%-5d", level),                     6, 49);
	c_put_str(a, format("%-ld", (long)object_value_real(o_ptr)), 7, 49);
	c_put_str(a, format("%-5d", o_ptr->cost_adjust),        8, 49);
	c_put_str(a, format("%lx", o_ptr->flags1),              9, 49);
	c_put_str(a, format("%lx", o_ptr->flags2),             10, 49);
	c_put_str(a, format("%lx", o_ptr->flags3),             11, 49);

	c_put_str(a, format("%04x", o_ptr->ident),              4, 72);
	c_put_str(a, format("%-5d", o_ptr->note),               5, 72);
	c_put_str(a, format("%-5d", o_ptr->inscrip),            6, 72);
	c_put_str(a, format("%-5d", o_ptr->artifact_index),     7, 72);
	c_put_str(a, format("%-5d", o_ptr->ego_item_index),     8, 72);
	c_put_str(a, format("%-5d", k_info[o_ptr->k_idx].flavor),  9, 72);


	/* Print descriptions of object pvals and flags */
	for (pval = 0, i = 0; i < 128; i++)
	{
		/* Color the flag based on whether it is active, inactive, or unused */
		if (i < 32)
		{
			/* Get actual pval for this flag */
			pval = get_object_pval(o_ptr, 1L << (i % 32));

			if (pval) a = TERM_L_BLUE;
			else if (flag_creation_data[i].desc[0] == '(') a = TERM_DARK;
			else a = TERM_WHITE;
		}
		else
		{
			if (f[i / 32] & (1L << (i % 32))) a = TERM_L_BLUE;
			else if (flag_creation_data[i].desc[0] == '(') a = TERM_DARK;
			else a = TERM_WHITE;
		}

		/* Print this flag's description */
		c_prt(a, format("%s", flag_creation_data[i].desc), (i % 32) + 13, 20 * (i / 32));

		/* Display exact pval value */
		if ((i < 32) && (pval))
		{
			if (pval > 0) a = TERM_L_GREEN;
			else          a = TERM_RED;
			c_put_str(a, format("%4d", pval), (i % 32) + 13, 15);
		}
	}

	/* Move cursor to top */
	move_cursor(0, 0);
}


/*
 * A list of tvals, their textual names, and possibility for becoming an
 * artifact.
 */
tval_desc tvals[] =
{
	{ TV_SHOT,         "Shots",              TRUE  },
	{ TV_ARROW,        "Arrows",             TRUE  },
	{ TV_BOLT,         "Bolts",              TRUE  },
	{ TV_SLING,        "Sling",              TRUE  },
	{ TV_BOW,          "Bow",                TRUE  },
	{ TV_CROSSBOW,     "Crossbow",           TRUE  },
	{ -1,              " ",                  FALSE },
	{ TV_DIGGING,      "Digger",             TRUE  },
	{ TV_HAFTED,       "Hafted Weapon",      TRUE  },
	{ TV_POLEARM,      "Polearm",            TRUE  },
	{ TV_SWORD,        "Sword",              TRUE  },
	{ -1,              " ",                  FALSE },
	{ TV_BOOTS,        "Boots",              TRUE  },
	{ TV_GLOVES,       "Gloves",             TRUE  },
	{ TV_HELM,         "Helm",               TRUE  },
	{ TV_CROWN,        "Crown",              TRUE  },
	{ TV_SHIELD,       "Shield",             TRUE  },
	{ TV_CLOAK,        "Cloak",              TRUE  },
	{ TV_SOFT_ARMOR,   "Soft Armor",        TRUE  },
	{ TV_HARD_ARMOR,   "Hard Armor",        TRUE  },
	{ TV_DRAG_ARMOR,   "Dragon Scale Mail",  TRUE  },
	{ -1,              " ",                  FALSE },
	{ TV_LITE,         "Light Source",       TRUE  },
	{ TV_AMULET,       "Amulet",             TRUE  },
	{ TV_RING,         "Ring",               TRUE  },
	{ TV_STAFF,        "Staff",              FALSE },
	{ TV_WAND,         "Wand",               FALSE },
	{ TV_ROD,          "Rod",                FALSE },
	{ TV_SCROLL,       "Scroll",             FALSE },
	{ TV_POTION,       "Potion",             FALSE },
	{ -1,              " ",                  FALSE },
	{ TV_MAGIC_BOOK,   "Magic Book",         FALSE },
	{ TV_PRAYER_BOOK,  "Priest Book",        FALSE },
	{ TV_NATURE_BOOK,  "Druid Stone",        FALSE },
	{ TV_DARK_BOOK,    "Necromantic Tome",   FALSE },
	{ -1,              " ",                  FALSE },
	{ TV_COMPONENT,    "Component",          FALSE },
	{ TV_ESSENCE,      "Essence",            FALSE },
	{ TV_PARCHMENT,    "Parchment",          FALSE },
	{ TV_BOTTLE,       "Bottle",             FALSE },
	{ -1,              " ",                  FALSE },
	{ TV_FLASK,        "Flask",              FALSE },
	{ TV_FOOD,         "Food",               FALSE },
	{ TV_SPIKE,        "Spikes",             FALSE },
	{ TV_CHEST,        "Chest",              FALSE },
	{ TV_SKELETON,     "Skeletons",          FALSE },
	{ TV_JUNK,         "Junk",               FALSE },
	{ TV_GOLD,         "Treasure",           FALSE },
	{ 0,               NULL,                 FALSE }
};


/*
 * Build an "artifact name" and transfer it into a buffer.
 */
static void get_art_name(char *buf, int a_idx, size_t buflen)
{
	int i;
	object_type	forge;
	object_type *o_ptr;
	artifact_type *a_ptr = &a_info[a_idx];


	/* Get local object */
	o_ptr = &forge;

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return;

	/* Create the base object */
	object_prep(o_ptr, i);

	/* Mark it as an artifact */
	o_ptr->artifact_index = a_idx;

	/* Make it known to us */
	o_ptr->ident |= IDENT_KNOWN;

	/* Create the artifact description */
	object_desc(buf, buflen, o_ptr, FALSE, 0);
}


/*
 * Acquire an object or artifact for creation by selecting first a tval
 * and then a specific object or artifact from successive menus.
 *
 * Can list up to 60 choices in three columns.
 */
static int wiz_create_itemtype(bool artifact, bool wizard)
{
	int i, num, max_num;
	int col, row;
	int tval;

	cptr desc;
	char ch;

	int choice[60];

	char buf[DESC_LEN];


	/* Unused parameter */
	(void)wizard;

	/* Clear screen */
	(void)Term_clear();

	/* List all tval indexes and their descriptions */
	for (i = 0, num = 0; (num < 60) && tvals[i].tval; i++)
	{
		/* If we want artifacts, don't show tvals with none. */
		if (artifact && !(tvals[i].can_be_artifact)) continue;

		/* This is a legal tval */
		if (tvals[i].tval > 0)
		{
			row =  2 + (num % 20);
			col = 30 * (num / 20);
			ch  = index_chars[artifact ? num : i];
			prt(format("[%c] %s", ch, tvals[i].desc), row, col);
		}

		num++;
	}

	/* We need to know the maximal possible tval index */
	max_num = i;

	/* Choose! */
	if (artifact)
	{
		if (!get_com("Get what type of artifact?", &ch)) return (0);
	}
	else
	{
		if (!get_com("Get what type of object?", &ch)) return (0);
	}


	/* Analyze choice */
	num = get_index(ch, FALSE);

	/* Bail out if choice is not within the bounds of the list */
	if ((num < 0) || (num >= max_num)) return (0);

	/* Bail out if the tval is illegal */
	if (tvals[num].tval <= 0) return (0);

	/* Adjust and verify choice of tval. */
	for (i = 0; i <= num; i++)
	{
		/* Optionally -- Slide past all object types with no artifacts. */
		if (artifact)
		{
			if (!(tvals[i].can_be_artifact)) num++;
		}
	}

	/* Error -- we've reached the end of the list */
	if (i > max_num) return (0);

	/* Base object type chosen, fill in tval */
	tval = tvals[i-1].tval;
	desc = tvals[i-1].desc;


	/*** And now we go for k_idx ***/

	/* Clear screen */
	(void)Term_clear();

	/* If choosing an artifact... */
	if (artifact)
	{
		/* ...We have to search the whole  artifact list. */
		for (num = 0, i = 1; (num < 60) && (i < z_info->a_max); i++)
		{
			artifact_type *a_ptr = &a_info[i];

			/* Analyze matching items */
			if (a_ptr->tval == tval)
			{
				/* Prepare it */
				row = 2 + (num % 20);
				col = 30 * (num / 20);
				ch  = index_chars[num];

				/* Acquire the "name" of artifact with index "i" */
				get_art_name(buf, i, sizeof(buf));

				/* Print it */
				if (strlen(buf) > 1) prt(format("[%c] %s", ch, buf), row, col);

				/* Remember the artifact index */
				choice[num++] = i;
			}
		}
	}

	/* If choosing an object... */
	else
	{
		/* We have to search the whole item list. */
		for (num = 0, i = 1; (num < 60) && (i < z_info->k_max); i++)
		{
			object_kind *k_ptr = &k_info[i];

			/* Analyze matching items */
			if (k_ptr->tval == tval)
			{
				/* Hack -- Skip instant artifacts */
				if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

				/* Prepare it */
				row = 2 + (num % 20);
				col = 30 * (num / 20);
				ch  = index_chars[num];

				/* Acquire the "name" of object "i" */
				strip_name(buf, i);

				/* Print it */
				if (strlen(buf) > 1) prt(format("[%c] %s", ch, buf), row, col);

				/* Remember the object index */
				choice[num++] = i;
			}
		}
	}

	/* Remember the maximum legal index */
	max_num = num;

	/* Choose! */
	if (artifact)
	{
		if (!get_com(format("Which artifact %s?", desc), &ch))
			return (0);
	}
	else
	{
		if (!get_com(format("What kind of %s?", desc), &ch))
			return (0);
	}

	/* Analyze choice */
	num = get_index(ch, FALSE);

	/* Return on failure */
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
	char tmp_val[DESC_LEN];


	/* Hack -- leave artifacts alone */
	if (artifact_p(o_ptr)) return;

	p = "Enter new 'pval' setting:";
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->pval);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->pval = atoi(tmp_val);
	wiz_display_item(o_ptr);

	/* Only if second pval actually affects anything */
	if (o_ptr->flags_pval2)
	{
		p = "Enter new 'pval2' setting:";
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->pval2);
		if (!get_string(p, tmp_val, 6)) return;
		o_ptr->pval2 = atoi(tmp_val);
		wiz_display_item(o_ptr);
	}

	/* Only if third pval actually affects anything */
	if (o_ptr->flags_pval3)
	{
		p = "Enter new 'pval3' setting:";
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->pval3);
		if (!get_string(p, tmp_val, 6)) return;
		o_ptr->pval3 = atoi(tmp_val);
		wiz_display_item(o_ptr);
	}

	p = "Enter new 'to_a' setting:";
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->to_a);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->to_a = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_h' setting:";
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->to_h);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->to_h = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_d' setting:";
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->to_d);
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
	bool allow_specific_ego = TRUE;

	/* Save some old variables */
	byte old_iy = o_ptr->iy;
	byte old_ix = o_ptr->ix;
	byte old_ident = o_ptr->ident &
		(IDENT_SENSE | IDENT_KNOWN | IDENT_RUMOUR | IDENT_MENTAL);
	s16b old_next_o_idx = o_ptr->next_o_idx;
	s16b old_held_m_idx = o_ptr->held_m_idx;
	byte old_number = o_ptr->number;
	u16b old_note = o_ptr->note;
	s16b old_weight = o_ptr->weight;


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
		if (!get_com("[n]ormal, [g]ood, [e]xcellent, [s]pecial, or [a]ccept?", &ch))
		{
			break;
		}

		/* Apply normal magic, but first clear object */
		if (ch == 'N' || ch == 'n')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE);
			changed = TRUE;
		}

		/* Apply good magic, but first clear object */
		else if (ch == 'G' || ch == 'g')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, FALSE, TRUE, FALSE);
			changed = TRUE;
		}

		/* Apply great magic, but first clear object */
		else if (ch == 'E' || ch == 'e')
		{
			/* Allow forcing of ego items  -SKY- */
			if (!allow_specific_ego)
			{
				object_prep(i_ptr, o_ptr->k_idx);
				apply_magic(i_ptr, p_ptr->depth, FALSE, TRUE, TRUE);
				changed = TRUE;
				allow_specific_ego = FALSE;
			}
			else
			{
				char tmp_val[DESC_LEN];
				cptr p = "Enter new ego index (0 for random):";

				/* Query */
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->ego_item_index);
				if (!get_string(p, tmp_val, 6)) return;

				/* Random change */
				if (atoi(tmp_val) == 0)
				{
					object_prep(i_ptr, o_ptr->k_idx);
					apply_magic(i_ptr, p_ptr->depth, FALSE, TRUE, TRUE);
					changed = TRUE;
					allow_specific_ego = FALSE;
				}

				/* Forced change */
				else if ((atoi(tmp_val) > 0) && (atoi(tmp_val) < z_info->e_max))
				{
					/* Clear object, set ego-item index */
					object_prep(i_ptr, o_ptr->k_idx);
					i_ptr->ego_item_index = atoi(tmp_val);

					/* No not re-roll for ego/artifact */
					obj_gen_flags |= (OBJ_GEN_NO_ROLLS);

					/* Apply magic appropriate to this ego-item */
					apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE);
					obj_gen_flags = 0;
					changed = TRUE;
				}
			}
		}

		/* Apply wondrous magic, but first clear object */
		else if (ch == 'S' || ch == 's')
		{
			int i;

			/* Brute-force artifact-creation XXX XXX */
			for (i = 0; i < 10000; i++)
			{
				object_prep(i_ptr, o_ptr->k_idx);
				apply_magic(i_ptr, p_ptr->depth + (i / 100), TRUE, TRUE, TRUE);

				if (artifact_p(i_ptr)) break;
			}

			/* Report failure */
			if (!artifact_p(i_ptr))
			{
				prt("Attempt to turn this item into an artifact failed.  Press any key to continue.", 0, 0);
				(void)inkey(ALLOW_CLICK);
			}

			changed = TRUE;
		}

		/* Create/change it! */
		else if (ch == '\n' || ch == '\r' || ch == 'A' || ch == 'a')
		{
			break;
		}
	}

	/* Notice change */
	if (changed)
	{
		/* Apply changes */
		object_copy(o_ptr, i_ptr);

		/* Restore some old variables */
		o_ptr->iy = old_iy;
		o_ptr->ix = old_ix;
		o_ptr->ident |= ((IDENT_SENSE | IDENT_KNOWN |
			IDENT_RUMOUR | IDENT_MENTAL) & (old_ident));
		o_ptr->next_o_idx = old_next_o_idx;
		o_ptr->held_m_idx = old_held_m_idx;
		o_ptr->note = old_note;
		if (!artifact_p(o_ptr)) o_ptr->number = old_number;

		/* Recalculate burden  XXX */
		if (o_ptr->weight != old_weight)
		{
			int i;
			object_type *j_ptr;

			/* Add the weights of all real objects in inven/equip */
			for (i = 0, p_ptr->total_weight = 0; i < INVEN_TOTAL; i++)
			{
				j_ptr = &inventory[i];
				if (!j_ptr->k_idx) continue;
				p_ptr->total_weight += (j_ptr->number * j_ptr->weight);
			}
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}
}



/*
 * Maximum number of rolls
 */
#define TEST_ROLL 100000


/*
 * Output statistics on the rarity of an object, and on how its plusses compare
 * to those of similar items at the current generation depth.  -Bernd-
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
	if (artifact_p(o_ptr)) a_info[o_ptr->artifact_index].cur_num = 0;


	/* Interact */
	while (TRUE)
	{
		cptr pmt = "Roll for [n]ormal, [g]ood, or [e]xcellent treasure?";

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
			good = FALSE;
			great = FALSE;
			break;
		}

		/* Let us know what we are doing */
		msg_format("Creating a lot of %s items. Base level = %d.",
			   quality, p_ptr->depth);
		msg_print(NULL);

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

				/* Allow interrupt */
				if (inkey(ALLOW_CLICK))
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

			/* Create an object */
			make_object(i_ptr, good, great, FALSE);


			/* Mega-Hack -- allow multiple artifacts XXX XXX XXX */
			if (artifact_p(i_ptr)) a_info[i_ptr->artifact_index].cur_num = 0;


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
		msg_print(NULL);
	}


	/* Hack -- Normally only make a single artifact */
	if (artifact_p(o_ptr)) a_info[o_ptr->artifact_index].cur_num = 1;
}


/*
 * Change the quantity of an item
 */
static void wiz_quantity_item(object_type *o_ptr)
{
	int old_num = o_ptr->number;
	int num;

	char tmp_val[16];


	/* Never duplicate artifacts */
	if (artifact_p(o_ptr)) return;

	/* Default */
	(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", o_ptr->number);

	/* Query */
	num = (int)get_quantity(format("Quantity (currently %d):", o_ptr->number),
	                        0, MAX_STACK_SIZE - 1);

	/* Paranoia */
	if (num < 1) num = 1;

	/* Accept modifications */
	o_ptr->number = num;

	/* Hack -- Rod total timeouts increase with stack size.  XXX */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval = o_ptr->pval * o_ptr->number / old_num;
	}
}


/*
 * Play with an item. Options include:
 *   - Output statistics (via wiz_statistics)
 *   - Reroll item (via wiz_reroll_item)
 *   - Change properties (via wiz_tweak_item)
 *   - Change the number of items (via wiz_quantity_item)
 */
static void do_cmd_wiz_play(void)
{
	int item, i;

	object_type *i_ptr;
	object_type object_type_body;

	object_type *o_ptr;

	char ch;

	cptr q, s;

	bool changed;


	/* Get an item */
	q = "Play with which object?";
	s = "You have nothing to play with.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
		return;
	item_to_object(o_ptr, item);

	/* Assume not changed */
	changed = FALSE;


	/* Use the tall display and center an 80 column view */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_TALL | DSP_CX,
		80, 0);

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
		if (!get_com("[a]ccept [s]tatistics [r]eroll [t]weak [q]uantity?", &ch))
		{
			changed = FALSE;
			break;
		}

		if (ch == 'A' || ch == 'a')
		{
			changed = TRUE;
			break;
		}

		if (ch == 's' || ch == 'S')
		{
			wiz_statistics(i_ptr);
		}

		if (ch == 'r' || ch == 'R')
		{
			wiz_reroll_item(i_ptr);
		}

		if (ch == 't' || ch == 'T')
		{
			wiz_tweak_item(i_ptr);
		}

		if (ch == 'q' || ch == 'Q')
		{
			wiz_quantity_item(i_ptr);
		}
	}

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);


	/* Accept change */
	if (changed)
	{
		/* Message */
		msg_print("Changes accepted.");

		/* Change */
		object_copy(o_ptr, i_ptr);

		/* Item is visible XXX XXX */
		o_ptr->marked = TRUE;

		/* Hack -- see if magic devices are really "empty" */
		if (o_ptr->pval) o_ptr->ident &= ~(IDENT_EMPTY);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

		/* Recalculate burden (always) */
		for (i = 0, p_ptr->total_weight = 0; i < INVEN_TOTAL; i++)
		{
			object_type *j_ptr = &inventory[i];
			if (!j_ptr->k_idx) continue;
			p_ptr->total_weight += (j_ptr->number * j_ptr->weight);
		}
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
 * Hack -- this routine always makes a "dungeon object", and applies
 * magic to it.  XXX XXX XXX
 */
static void wiz_create_item(int k_idx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *i_ptr;
	object_type object_type_body;

	int num = 1;


	/* Get object index, unless a legal one is already chosen */
	if ((k_idx <= 0) || (k_idx >= z_info->k_max))
	{
		/* Use the standard display and center an 80 column view */
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX,
			80, 0);

		/* Get object base type */
		k_idx = wiz_create_itemtype(FALSE, TRUE);

		/* Restore previous display */
		display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

		/* Return if failed */
		if (!k_idx) return;
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create the item */
	object_prep(i_ptr, k_idx);

	/* Get quantity  -LM- */
	if ((i_ptr->tval != TV_GOLD) && (!i_ptr->artifact_index))
	{
		/* Move cursor */
		move_cursor(0, 0);

		num = (int)get_quantity("Make how many objects?", 1, 99);
	}

	/* Allow cancel */
	if (!num) return;

	/* Apply magic (no artifacts) */
	apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE);

	/* Set correct number */
	i_ptr->number = num;

	/*
	 * Hack -- Since treasure objects are not affected by apply_magic,
	 * they need special processing.
	 */
	if (i_ptr->tval == TV_GOLD)
	{
		/* Set coin type to the sval chosen */
		coin_type = i_ptr->sval;

		/* Process the treasure */
		make_gold(i_ptr);
	}

	/* Hack -- mark the object (necessary for essences) */
	i_ptr->marked = TRUE;

	/* Drop the object from heaven */
	drop_near(i_ptr, 0, py, px, DROP_HERE);

	/* All done */
	msg_print("Allocated.");
}

/*
 * Create an artifact
 */
static void wiz_create_artifact(int a_idx)
{
	object_type object_type_body;
	object_type *o_ptr;
	char o_name[DESC_LEN];


	/* Get artifact index, unless a legal one is already chosen */
	if ((a_idx <= 0) || (a_idx >= z_info->a_max))
	{
		/* Use the standard display and center an 80 column view */
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX,
			80, 0);

		/* Get an artifact index */
		a_idx = wiz_create_itemtype(TRUE, TRUE);

		/* Restore previous display */
		display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

		/* Return if failed */
		if (!a_idx) return;
	}

	/* Get local object */
	o_ptr = &object_type_body;

	/* Attempt to create the artifact */
	if (!make_fake_artifact(o_ptr, a_idx)) return;

	/* Identify it  XXX */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Drop the artifact from heaven */
	drop_near(o_ptr, 0, p_ptr->py, p_ptr->px, DROP_HERE);

	/* Describe the artifact */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

	/* All done */
	msg_format("You have created %s.", o_name);
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

	/* Update stuff (if needed) */
	if (p_ptr->update) update_stuff();

	/* Heal the player */
	p_ptr->chp = p_ptr->mhp;
	p_ptr->chp_frac = 0;

	/* Restore mana */
	p_ptr->csp = p_ptr->msp;
	p_ptr->csp_frac = 0;

	/* Cure stuff */
	(void)set_blind(0, NULL);
	(void)set_confused(0);
	(void)set_poisoned(0);
	(void)set_diseased(0, NULL);
	(void)set_afraid(0);
	(void)set_paralyzed(0);
	(void)set_image(0);
	(void)set_stun(0);
	(void)set_cut(0);
	(void)set_slow(0);
	p_ptr->black_breath = FALSE;

	/* No longer hungry */
	(void)set_food(p_ptr->food_bloated - 50);

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
		char ppp[DESC_LEN];

		char tmp_val[32];

		/* Prompt */
		(void)strnfmt(ppp, sizeof(ppp), "Jump to level (0-%d):", MAX_DEPTH-1);

		/* Default */
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%d", p_ptr->depth);

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
	p_ptr->depth = p_ptr->command_arg;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Become aware of a lot of objects
 */
static void do_cmd_wiz_learn(void)
{
	int i;
	bool forget = FALSE;

	object_type *i_ptr;
	object_type object_type_body;

	/* Choose a level */
	if (!p_ptr->command_arg)
	{
		p_ptr->command_arg = (s16b)
			get_quantity("Learn about object kinds up to which level?", 0, MAX_DEPTH);
	}

	/* Allow forgetting */
	if (get_check(format("Would you like to forget objects above level %d?",
		p_ptr->command_arg)))
	{
		forget = TRUE;
	}

	/* Scan every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Induce awareness */
		if (k_ptr->level <= p_ptr->command_arg)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Prepare object */
			object_prep(i_ptr, i);

			/* Awareness */
			object_aware(i_ptr);
		}

		/* Induce forgetfulness */
		else if (forget)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Prepare object */
			object_prep(i_ptr, i);

			/* No longer aware of the effects */
			k_info[i_ptr->k_idx].special &= ~(SPECIAL_AWARE);

			/* You have no longer seen it */
			k_info[i_ptr->k_idx].special &= ~(SPECIAL_EVER_SEEN);
		}
	}
}

/*
 * Summon some creatures
 */
static void do_cmd_wiz_summon(int num)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	(void)summon_specific(py, px, FALSE, p_ptr->depth, 0, num);
}


/*
 * Summon a creature of the specified type
 */
static void do_cmd_wiz_named(int r_idx, bool slp)
{
	monster_race *r_ptr = &r_info[r_idx];

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x;

	u16b grid[9];
	int grids = 0;


	/* Paranoia -- ignore empty or illegal monsters */
	if (!r_idx)
	{
		r_idx = (s16b)get_quantity("Which monster would you like to summon?",
		                     0, z_info->r_max);

		if (!r_idx) return;
	}

	if (r_idx >= z_info->r_max) return;
	if (!r_ptr->name) return;

	/* Scan the adjacent floor grids */
	for (i = 0; i < 9; i++)
	{
		y = py + ddy_ddd[i];
		x = px + ddx_ddd[i];

		/* Must be fully in bounds */
		if (!in_bounds_fully(y, x)) continue;

		/* This grid is OK for this monster (should monsters be able to dig?) */
		if (cave_exist_mon(r_ptr, y, x, FALSE, FALSE))
		{
			/* Save this grid */
			grid[grids++] = GRID(y, x);
		}
	}

	/* No grids available */
	if (!grids) return;

	/* Pick a grid at random */
	i = rand_int(grids);

	/* Get the coordinates */
	y = GRID_Y(grid[i]);
	x = GRID_X(grid[i]);

	/* Place it (allow groups) */
	if (place_monster_aux(y, x, r_idx, slp, TRUE))
	{
		char name[DESC_LEN];
		monster_type *m_ptr = NULL;

		/* Hack -- find the monster and point to it */
		for (i = 1; i < m_max; i++)
		{
			m_ptr = &m_list[i];

			if (m_ptr->r_idx == r_idx)
			{
				/* Get the monster name ("a kobold") */
				monster_desc(name, m_ptr, 0x88);

				/* Message */
				msg_format("You have summoned %s.", name);

				break;
			}
		}
	}
}


/*
 * Hack -- Delete all nearby monsters
 */
static void do_cmd_wiz_zap(int d)
{
	int i;

	/* Genocide everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > d) continue;

		/* Delete the monster */
		delete_monster_idx(i);
	}
}


/*
 * Un-hide all monsters
 */
static void do_cmd_wiz_unhide(int d)
{
	int i;

	/* Process monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > d) continue;

		/* Monster will stay fully visible until next turn */
		m_ptr->mflag |= (MFLAG_FULL | MFLAG_SHOW);

		/* Update the monster */
		(void)update_mon(i, FALSE);
	}
}


/*
 * Displays traps, doors, stairs, treasure, gold, objects, essences,
 * and monsters within a given distance from the character.  -LM-
 */
static void wiz_detect_all(int d)
{
	int top, bottom, right, left;

	bool detect;

	int i, y, x;


	/* Default detection range */
	if (d == 0) d = MAX_SIGHT;

	/* Determine bounds - top */
	top = p_ptr->py + d;
	if (top > dungeon_hgt) top = dungeon_hgt;

	/* Determine bounds - bottom */
	bottom = p_ptr->py - d;
	if (bottom < 0) bottom = 0;

	/* Determine bounds - right */
	right = p_ptr->px + d;
	if (right > dungeon_wid) right = dungeon_wid;

	/* Determine bounds - left */
	left = p_ptr->px - d;
	if (left < 0) left = 0;


	/* Scan within the calculated bounds */
	for (y = bottom; y <= top; y++)
	{
		for (x = left; x <= right; x++)
		{
			/* Paranoia -- stay legal */
			if (!in_bounds(y, x)) continue;

			/* Only detect nearby dungeon features */
			if (distance(y, x, p_ptr->py, p_ptr->px) > d) continue;

			/* Assume nothing here */
			detect = FALSE;

			/* Detect all traps and loose rocks */
			if (cave_info[y][x] & (CAVE_TRAP))
			{
				if (reveal_trap(y, x, 100, FALSE, TRUE))
				{
					detect = TRUE;
				}
			}

			/* Detect secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);
			}

			/* Detect doors and stairs */
			if ((cave_any_door(y, x)) || (cave_any_stairs(y, x)))
			{
				detect = TRUE;
			}

			/* Notice embedded gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_H) ||
				(cave_feat[y][x] == FEAT_QUARTZ_H))
			{
				/* Hack -- Expose the gold */
				cave_feat[y][x] += 0x02;
			}

			/* Magma/Quartz/Pillar + Known Gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_K) ||
				(cave_feat[y][x] == FEAT_QUARTZ_K) ||
				(cave_feat[y][x] == FEAT_PILLAR_GOLD))
			{
				detect = TRUE;
			}

			if (detect)
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (distance(y, x, p_ptr->py, p_ptr->px) > d) continue;

		/* Hack -- memorize it */
		o_ptr->marked = TRUE;

		/* Redraw */
		lite_spot(y, x);
	}

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (distance(y, x, p_ptr->py, p_ptr->px) > d) continue;

		/* Hack -- Reveal the monster */
		m_ptr->mflag &= ~(MFLAG_MIME);
		if (m_ptr->ml < ML_FULL) m_ptr->ml = ML_FULL;

		/* Monster will stay fully visible until next turn */
		m_ptr->mflag |= (MFLAG_FULL | MFLAG_SHOW);

		/* Update the monster */
		(void)update_mon(i, FALSE);
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

	term *old = Term;

	char cmd;
	cptr flag_desc;

	int panel_row_min = p_ptr->wy;
	int panel_row_max = p_ptr->wy + map_rows;
	int panel_col_min = p_ptr->wx;
	int panel_col_max = p_ptr->wx + map_cols;

	u16b mask = 0x0000;


	/* Get a "debug command" */
	if (!get_com("Cave_info flag query (m, g, G, r, i, s, v, t, w, f, l, p, o, e, ?):", &cmd)) return;

	/* Extract a flag */
	switch (cmd)
	{
		case 'm': mask |= (CAVE_MARK); flag_desc = "cave_mark"; break;
		case 'g': mask |= (CAVE_GLOW); flag_desc = "cave_glow"; break;
		case 'r': mask |= (CAVE_ROOM); flag_desc = "cave_room"; break;
		case 'i': mask |= (CAVE_ICKY); flag_desc = "cave_icky"; break;
		case 's': mask |= (CAVE_SEEN); flag_desc = "cave_seen"; break;
		case 'v': mask |= (CAVE_VIEW); flag_desc = "cave_view"; break;
		case 't': mask |= (CAVE_TEMP); flag_desc = "cave_temp"; break;
		case 'w': mask |= (CAVE_LOS); flag_desc = "LOS blocked"; break;

		case 'f': mask |= (CAVE_FIRE); flag_desc = "cave_fire"; break;
		case 'G': mask |= (CAVE_LITE); flag_desc = "cave_lite"; break;

		case 'o': mask |= (CAVE_TRAP);
			flag_desc = "trap markers"; break;
		case 'e': mask |= (CAVE_EFFT);
			flag_desc = "lingering effect"; break;

		/* Special functions */
		case 'p': flag_desc = "projectable"; break;
		case 'l': flag_desc = "line of sight as per \"los()\""; break;
		case '?': flag_desc = ""; break;

		default:
		{
			msg_print("No cave_info flag for this query.");
			return;
		}
	}


	/* We asked for help */
	if (cmd == '?')
	{
		msg_print("m = marked, g = permanently lit, r = in a room, i = vault (icky), ");
		msg_print("s = character can see, v = character can see if light exists, ");
		msg_print("t = temporary marking, w = LOS blocked, f = in line of fire, ");
		msg_print("p = clear shot/blocked shot/no shot, l = test of \"los()\", ");
		msg_print("o = flagged as trap, e = lingering effect, G = temporarily lit");
		msg_print(NULL);
		return;
	}


	/* Activate the dedicated map display (optionally) */
	if (use_special_map) (void)Term_activate(term_map);

	/* Scan map */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			byte a = TERM_RED;

			/* Given no mask, apply special code */
			if (!mask)
			{
				if (cmd == 'l')
				{
					if (los(py, px, y, x)) a = TERM_YELLOW;
				}

				if (cmd == 'p')
				{
					int path;
					path = projectable(y, x, py, px, PROJECT_CHCK);
					if (path != projectable(py, px, y, x, PROJECT_CHCK))
					{
						if (projectable(py, px, y, x, PROJECT_CHCK))
							a = TERM_L_RED;
						else
							a = TERM_RED;
					}
					else if (path == PROJECT_NO) a = TERM_SLATE;
					else if (path == PROJECT_NOT_CLEAR) a = TERM_WHITE;
					else if (path == PROJECT_CLEAR) a = TERM_L_GREEN;
					else a = TERM_PURPLE;
				}
			}

			/* Using a mask */
			else
			{
				/* Given mask, show only those grids */
				if (cmd == 'w')
				{
					if (cave_info[y][x] & mask) continue;
				}
				else
				{
					if (!(cave_info[y][x] & mask)) continue;
				}

				/* Color */
				if (cave_project_bold(y, x)) a = TERM_YELLOW;
			}

			/* Display player/floors/walls */
			if ((y == py) && (x == px))
			{
				print_rel('@', a, y, x);
			}
			else if (cave_project_bold(y, x))
			{
				print_rel('*', a, y, x);
			}
			else
			{
				print_rel('#', a, y, x);
			}
		}
	}

	/* Restore the previous Term (if necessary) */
	if (use_special_map) (void)Term_activate(old);

	/* Get keypress */
	msg_format("Showing %s.  Press any key to continue.", flag_desc);
	msg_print(NULL);

	/* Redraw map */
	prt_map();
}


/*
 * Query terrain flags
 */
static void do_cmd_wiz_query_terrain(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	char cmd;
	cptr flag_desc;

	int panel_row_min = p_ptr->wy;
	int panel_row_max = p_ptr->wy + map_rows;
	int panel_col_min = p_ptr->wx;
	int panel_col_max = p_ptr->wx + map_cols;

	u16b mask = 0x0000;


	/* Get a "debug command" */
	if (!get_com("terrain flag query (l, p, ~, i, X, -, o, t, b, f, w, r, R, d, D, s):", &cmd)) return;

	/* Extract a flag */
	switch (cmd)
	{
		case 'l': mask |= (TF_LOS); flag_desc = "tf_los (allows line of sight)"; break;
		case 'p': mask |= (TF_PROJECT); flag_desc = "tf_project (allows spells to cross)"; break;
		case '~': mask |= (TF_PASSABLE); flag_desc = "tf_passable (is passable by all)"; break;
		case 'i': mask |= (TF_INTERESTING); flag_desc = "tf_interesting (within interesting room)"; break;
		case 'X': mask |= (TF_PERMANENT); flag_desc = "tf_permanent (cannot be destroyed)"; break;
		case '-': mask |= (TF_NO_SCENT); flag_desc = "tf_no_scent (no scent allowed)"; break;
		case 'o': mask |= (TF_OBJECT); flag_desc = "tf_object (objects allowed)"; break;
		case 't': mask |= (TF_TRAP); flag_desc = "tf_trap (traps allowed)"; break;
		case 'b': mask |= (TF_TORCH_ONLY); flag_desc = "tf_torch_only (bright under torchlight)"; break;

		case 'f': mask |= (TF_FLOOR); flag_desc = "tf_floor (is a floor)"; break;
		case 'w': mask |= (TF_WALL); flag_desc = "tf_wall (is a wall)"; break;
		case 'r': mask |= (TF_ROCK); flag_desc = "tf_rock (any rock)"; break;
		case 'R': mask |= (TF_GRANITE); flag_desc = "tf_granite (granite)"; break;
		case 'd': mask |= (TF_DOOR_ANY); flag_desc = "tf_door_any (any door)"; break;
		case 'D': mask |= (TF_DOOR_CLOSED); flag_desc = "tf_door_closed (closed door)"; break;
		case 's': mask |= (TF_SHOP); flag_desc = "tf_shop (shop entrance)"; break;


		/* Special functions */
		case '?': flag_desc = ""; break;

		default:
		{
			msg_print("No terrain flag for this query.");
			return;
		}
	}


	/* We asked for help */
	if (cmd == '?')
	{
		msg_print("l = terrain allows LOS, p = projectable, ~ = passable, i = interesting, ");
		msg_print("X = permanent, - = no scent, o = object allowed, t = trap allowed, ");
		msg_print("b = torch bright only, ");
		msg_print("f = floor, w = wall, r = rock (any), R = granite, d = door (any), ");
		msg_print("D = closed door, s = shop");

		msg_print(NULL);
		return;
	}


	/* Scan map */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			byte a = TERM_L_BLUE;

			/* Given no mask, apply special code */
			if (!mask)
			{
				/* No special code yet */
			}

			/* Using a mask */
			else
			{
				/* Given mask, show only those grids */
				if (!(f_info[cave_feat[y][x]].flags & mask)) continue;
			}

			/* Display player/floors/walls */
			if ((y == py) && (x == px))
			{
				print_rel('@', a, y, x);
			}
			else if (cave_project_bold(y, x))
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
	msg_format("Showing %s.  Press any key to continue.", flag_desc);
	msg_print(NULL);

	/* Redraw map */
	prt_map();
}



#ifdef ALLOW_SPOILERS

/*
 * External function
 */
extern void do_cmd_spoilers(void);

#endif


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
	if (!get_com("Debug Command:", &cmd)) return;

	/* Analyze the command */
	switch (cmd)
	{
		/* Nothing */
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

		/* Cure all maladies */
		case 'a':
		{
			do_cmd_wiz_cure_all();
			break;
		}

		/* Select a target, and teleport to it. */
		case 'b':
		{
			(void)phase_warp(999, 0, TRUE);
			break;
		}

		/* Create any object */
		case 'c':
		{
			wiz_create_item(p_ptr->command_arg);
			break;
		}

		/* Create any artifact */
		case 'C':
		{
			wiz_create_artifact(p_ptr->command_arg);
			break;
		}

		/* Detect everything */
		case 'd':
		{
			wiz_detect_all(p_ptr->command_arg);
			break;
		}

		/* Edit character stats, gold, and unspent experience */
		case 'e':
		{
			do_cmd_wiz_change();
			break;
		}

		/* *Identify* */
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

		/* Identify */
		case 'i':
		{
			(void)ident_spell();
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
			set_self_knowledge(p_ptr->self_knowledge + 20,
				"You begin to know yourself a little better...");
			break;
		}

		/* Kill Knowledge */
		case 'K':
		{
			if (get_check("Are you certain you want to cast forget?"))
			{
				lose_all_info("You forget the map and your inventory.");
			}
			break;
		}

		/* Learn about objects */
		case 'l':
		{
			do_cmd_wiz_learn();
			break;
		}

		/* Probe monsters */
		case 'L':
		{
			probing();
			break;
		}

		/* Magic Mapping */
		case 'm':
		{
			map_area(0, 0, FALSE);
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
			teleport_player(10, TRUE, FALSE);
			break;
		}

		/* More info on objects, monsters, and rooms */
		case 'P':
		{
			if (!cheat_peek)
			{
				if (get_check("Get more information on objects generated?"))
					p_ptr->noscore |= (CHEAT_PEEK);
			}
			else
			{
				if (get_check("Stop getting more information on objects generated?")) p_ptr->noscore &= ~(CHEAT_PEEK);
			}

			if (!cheat_hear)
			{
				if (get_check("Get more information on monsters generated?"))
					p_ptr->noscore |= (CHEAT_HEAR);
			}
			else
			{
				if (get_check("Stop getting more information on monsters generated?")) p_ptr->noscore &= ~(CHEAT_HEAR);
			}

			if (!cheat_room)
			{
				if (get_check("Get more information on special rooms generated?"))
					p_ptr->noscore |= (CHEAT_ROOM);
			}
			else
			{
				if (get_check("Stop getting more information on special rooms generated?")) p_ptr->noscore &= ~(CHEAT_ROOM);
			}
			break;
		}

		/* Query the dungeon */
		case 'q':
		{
			do_cmd_wiz_query();
			break;
		}

		/* Query the terrain */
		case 'Q':
		{
			do_cmd_wiz_query_terrain();
			break;
		}

		/* Summon Random Monster(s) */
		case 's':
		{
			if (p_ptr->command_arg <= 0)
			{
				p_ptr->command_arg =
					(s16b)get_quantity("How many monsters would you like to summon?", 0, 99);
			}
			do_cmd_wiz_summon(p_ptr->command_arg);
			break;
		}

		/* Forget skills */
		case 'S':
		{
			int i;
			bool changed = FALSE;

			if (get_check("Forget all skills?"))
			{
				for (i = 0; i < NUM_SKILLS; i++)
				{
					p_ptr->pskills[i].cur = 0;
					p_ptr->pskills[i].max = 0;
				}

				/* No power */
				p_ptr->power = 1;

				changed = TRUE;
			}

			if (get_check("Cancel all oaths?"))
			{
				p_ptr->oath = 0;
				changed = TRUE;
			}

			if ((p_ptr->realm) && get_check("Clear magic realm and spells?"))
			{
				/* No realm */
				p_ptr->realm = NONE;

				/* No magic */
				mp_ptr = &magic_info[NONE];

				/* No spell information */
				for (i = 0; i < PY_MAX_SPELLS; i++)
				{
					p_ptr->spell_flags[i] = 0;
				}

				changed = TRUE;
			}

			if (changed)
			{
				/* Do a complete redraw */
				do_cmd_redraw();
			}

			break;
		}

		/* Teleport */
		case 't':
		{
			teleport_player(100, TRUE, FALSE);
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

		/* Fully wizard Light the Level */
		case 'w':
		{
			bool glow = get_check("Light up the level in addition to mapping it?");
			wiz_lite(TRUE, glow);
			break;
		}

		/* Zap Monsters (Genocide) */
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

		/* Help me. */
		case '?':
		default:
		{
			/* Use the standard display and center an 80 column view */
			display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX,
				80, 0);

			/* Header */
			move_cursor(1, 0);
			c_roff_centered(TERM_L_BLUE, "Listing of Wizard commands", 5, 75);

			/* Information */
			prt("'\"'  : create spoilers", 3, 0);
			prt("'a'  : cure all maladies", 4, 0);
			prt("'b'  : teleport to a target *", 5, 0);
			prt("'c'  : create an object", 6, 0);
			prt("'C'  : create an artifact", 7, 0);
			prt("'d'  : detect everything *", 8, 0);
			prt("'e'  : edit character", 9, 0);
			prt("'f'  : fully identify", 10, 0);
			prt("'g'  : acquirement (good) *", 11, 0);
			prt("'i'  : standard identify", 12, 0);
			prt("'j'  : jump to any level", 13, 0);
			prt("'k'  : self-knowledge", 14, 0);
			prt("'K'  : forget carried objects and map", 15, 0);
			prt("'l'  : learn (& forget) about objects", 16, 0);
			prt("'L'  : probing", 17, 0);

			prt("'m'  : magic mapping", 3, 40);
			prt("'n'  : summon specific monster *", 4, 40);
			prt("'o'  : tweak or reroll an object", 5, 40);
			prt("'p'  : phase door", 6, 40);
			prt("'P'  : more info on objs/monsters/rooms", 7, 40);
			prt("'q'  : query cave flags, projection", 8, 40);
			prt("'Q'  : query terrain flags", 9, 40);
			prt("'s'  : summon random monsters *", 10, 40);
			prt("'S'  : forget skills, cancel magic", 11, 40);
			prt("'t'  : teleport", 12, 40);
			prt("'u'  : reveal all monsters *", 13, 40);
			prt("'v'  : acquirement (great) *", 14, 40);
			prt("'w'  : wizard-light the level", 15, 40);
			prt("'z'  : zap monsters *", 16, 40);
			prt("'_'  : debug the flow code", 17, 40);


			/* Usage notes */
			move_cursor(20, 0);
			roff("     A '*' after a command description indicates that you can specify a target, a range, or a quantity before issuing the command.  In some cases, you have to do this for anything to happen.", 0, 80);

			/* The exit sign */
			move_cursor(24, 0);
			c_roff_centered(TERM_WHITE, "Press any key to continue.", 5, 75);

			/* Get any key */
			(void)inkey(ALLOW_CLICK);

			/* Restore previous display */
			display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

			break;
		}
	}
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif


