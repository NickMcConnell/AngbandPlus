/* File: wizard2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#ifdef ALLOW_DEBUG

/*
 * Listing of wizard commands 
 */
void do_cmd_wiz_help(void)
{
	/* Enter "icky" mode */
	character_icky++;

	/* Save the screen */
	Term_save();

	/* Flush */
	Term_fresh();

	/* Clear the screen */
	Term_clear();

	c_put_str(TERM_RED,"Wizard Commands",1,32);
	
	c_put_str(TERM_BLUE,"Character Editing",3,1);
	c_put_str(TERM_BLUE,"=================",4,1);
	put_str("a = Cure all",6,1);
	put_str("e = Edit stats",7,1);
	put_str("h = Reroll hitpoints",8,1);
	put_str("x = Gain experience",9,1);

	c_put_str(TERM_BLUE,"Movement",11,1);
	c_put_str(TERM_BLUE,"========",12,1);
	put_str("b = Teleport to target",14,1);
	put_str("j = Jump levels",15,1);
	put_str("p = Phase Door",16,1);
	put_str("P = Dimension Door",17,1);
	put_str("t = Teleport player",18,1);

	c_put_str(TERM_BLUE,"Monsters",3,24);
	c_put_str(TERM_BLUE,"========",4,24);
	put_str("(#)s = Summon monsters",6,24);
	put_str("#n   = Summon named mon.",7,24);
	put_str("#N   = Summon named unq.",8,24);
	put_str("(#)u = Unhide monsters",9,24);
	put_str("(#)z = Zap monsters",10,24);

	c_put_str(TERM_BLUE,"Dungeon Commands",12,24);
	c_put_str(TERM_BLUE,"================",13,24);
	put_str("d = Detect all",15,24);
	put_str("m = Map area",16,24);
	put_str("q = Query the dungeon ",17,24);
	put_str("w = Wizard light",18,24);
	put_str("_ = Show flow values",19,24);

	c_put_str(TERM_BLUE,"Object Commands",3,50);
	c_put_str(TERM_BLUE,"===============",4,50);
	put_str("c    = Create item",6,50);
	put_str("#C   = Create named artifact",7,50);
	put_str("f    = *Identify* item",8,50);
	put_str("(#)g = Generate good objects",9,50);
	put_str("i    = Identify item",10,50);
	put_str("(#)l = Learn about objects",11,50);
	put_str("o    = Object editor",12,50);
	put_str("(#)v = Generate great objects",13,50);
	put_str("X    = Generate a chest",14,50);

	c_put_str(TERM_BLUE,"General Commands",16,50);
	c_put_str(TERM_BLUE,"================",17,50);
	put_str("? = Wizard mode help",18,50); 
	put_str("\" = Generate spoilers",19,50);

	/* Wait for it */
	put_str("Hit any key to continue", 23, 23);

	/* Get any key */
	inkey();

	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky--;
}

/*
 * Hack -- quick debugging hook
 */
static void do_cmd_wiz_hack_ben(void)
{

#ifdef MONSTER_FLOW

	int i, y, x;

	for (i = 0; i < MONSTER_FLOW_DEPTH; ++i)
	{
		/* Update map */
		for (y = p_ptr->wy; y < p_ptr->wy + SCREEN_HGT; y++)
		{
			for (x = p_ptr->wx; x < p_ptr->wx + SCREEN_WID; x++)
			{
				byte a = TERM_RED;

				if (!in_bounds_fully(y, x)) continue;

				/* Display proper cost */
				if (cave_cost[y][x] != i) continue;

				/* Reliability in yellow */
				if (cave_when[y][x] == cave_when[p_ptr->py][p_ptr->px])
				{
					a = TERM_YELLOW;
				}

				/* Display player/floors/walls */
				if ((y == p_ptr->py) && (x == p_ptr->px))
				{
					print_rel('@', a, y, x);
				}
				else if (cave_floor_bold(y, x))
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
		prt(format("Depth %d: ", i), 0, 0);

		/* Get key */
		if (inkey() == ESCAPE) break;

		/* Redraw map */
		prt_map();
	}

	/* Done */
	prt("", 0, 0);

	/* Redraw map */
	prt_map();

#else /* MONSTER_FLOW */

	/* Oops */
	msg_print("Oops");

#endif /* MONSTER_FLOW */

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
 * Output a rarity graph for a type of object.
 */
static void prt_alloc(byte tval, byte sval, int row, int col)
{
	int i, j;

	int home, lev;

	u32b maxd = 1, maxr = 1, maxt = 1;

	u32b rarity[MAX_DEPTH];
	u32b total[MAX_DEPTH];
	u32b display[20];

	byte c = TERM_WHITE;
	cptr r = "+--common--+";

	object_kind *k_ptr;

	/* Get the entry */
	alloc_entry *table = alloc_kind_table;

	/* Wipe the tables */
	(void)C_WIPE(rarity, MAX_DEPTH, u32b);
	(void)C_WIPE(total, MAX_DEPTH, u32b);
	(void)C_WIPE(display, 20, u32b);

	/* Scan all entries */
	for (i = 0; i < MAX_DEPTH; i++)
	{
		/* Base level */
		lev = ((i * (GREAT_OBJ - 1)) + (1 + i * 5433L / 1000)) / GREAT_OBJ;

		for (j = 0; j < alloc_kind_size; j++)
		{
			/* Objects are sorted by depth */
			if (table[j].level > lev) break;

			/* Acquire this kind */
			k_ptr = &k_info[table[j].index];

			/* Accumulate probabilities */
			total[i] += table[j].prob1;

			/* Accumulate probabilities */
			if ((k_ptr->tval == tval) && (k_ptr->sval == sval))
			{
				home = k_ptr->level;
				rarity[i] += table[j].prob1;
			}
		}
	}

	/* Find maxima */
	for (i = 0; i < MAX_DEPTH; i++)
	{
		if (rarity[i] > maxr) maxr = rarity[i];
		if (total[i] > maxt) maxt = total[i];
	}

	/* Simulate a log graph */
	if (maxt / maxr > 32)
	{
		c = TERM_L_WHITE;
		r = "+-common-+";
	}
	if (maxt / maxr > 1024)
	{
		c = TERM_SLATE;
		r = "+--rare--+";
	}
	if (maxt / maxr > 32768L)
	{
		c = TERM_L_DARK;
		r = "+-v.rare-+";
	}

	/* Calculate probabilities for each range */
	for (i = 0; i < 20; i++)
	{
		/* Shift the values into view */
		for (j = i * MAX_DEPTH / 20; j < (i + 1) * MAX_DEPTH / 20; j++)
		{
			display[i] += rarity[j] * maxt * 10 / total[j];
		}

		/* Correct proportions */
		display[i] /= maxr;

		/* Track maximum */
		if (display[i] > maxd) maxd = display[i];
	}

	/* Normalize */
	for (i = 0; i < 20; i++)
	{
		display[i] = display[i] * 9 / maxd;
	}

	/* Graph the rarities */
	for (i = 0; i < 20; i++)
	{
		Term_putch(col, row + i + 1, TERM_WHITE,  '|');

		/* Note the level */
		if ((i * MAX_DEPTH / 20 <= home) && (home < (i + 1) * MAX_DEPTH / 20))
		{
			c_prt(TERM_RED, format("%.*s", display[i], "*********"), row + i + 1, col + 1);
		}
		else
		{
			c_prt(c, format("%.*s", display[i], "*********"), row + i + 1, col + 1);
		}
	}

	/* Make it look nice */
	prt(r, row, col);

	Term_putch(col, row + 8, TERM_WHITE,  'A');
	Term_putch(col, row + 9, TERM_WHITE,  'L');
	Term_putch(col, row + 10, TERM_WHITE, 'L');
	Term_putch(col, row + 11, TERM_WHITE, 'O');
	Term_putch(col, row + 12, TERM_WHITE, 'C');

	prt("+", row + 21, col);
}
 
/*
 * Output a long int in binary format.
 */
static void prt_resists(object_type *o_ptr, byte from, byte to, int row, int col)
{
	int i, j;
	byte attr;

	/* Scan the flags */
	for (i = from; i < to; i++)
	{
		j = object_resist(o_ptr, i);
		if (j) attr = TERM_L_BLUE;
		else attr = TERM_DARK;

		if (j >= 100) c_prt(TERM_L_GREEN, "**", row, col);
		else c_prt(attr, format("%d", j), row, col);
		col += 3;
	}
}

/*
 * Output a long int in binary format.
 */
static void prt_slays(object_type *o_ptr, byte from, byte to, int row, int col)
{
	int i, j;
	byte attr;
	byte slays[SL_MAX];

	weapon_slays(o_ptr, slays);

	/* Scan the flags */
	for (i = from; i < to; i++)
	{
		j = slays[i];
		if (j) attr = TERM_L_BLUE;
		else attr = TERM_DARK;

		c_prt(attr, format("%d", j), row, col);
		col += 3;
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
		sprintf(ppp, "%s (0-20): ", stat_names[i]);

		/* Default */
		sprintf(tmp_val, "%d", p_ptr->stat_max[i]);

		/* Query */
		if (!get_string(ppp, tmp_val, 3)) return;

		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Verify */
		if (tmp_int > 20) tmp_int = 20;
		else if (tmp_int < 0) tmp_int = 0;

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
	if (!get_string("Experience: ", tmp_val, 9)) return;

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
	if (!get_string("Max Exp: ", tmp_val, 9)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->max_exp = tmp_long;

	/* Update */
	check_experience();
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
static void wiz_display_item(object_type *o_ptr)
{
	int j = 0;

	u32b f1, f2, f3;

	char buf[256];

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Clear screen */
	Term_clear();

	/* Describe fully */
	object_desc_store(buf, o_ptr, TRUE, 3);
	prt(buf, 2, j);

	prt(format("kind = %-5d  level = %-4d  tval = %-5d  sval = %-5d",
	           o_ptr->k_idx, k_info[o_ptr->k_idx].level,
	           o_ptr->tval, o_ptr->sval), 4, j);

	prt(format("number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d  break = %d",
	           o_ptr->number, actual_weight(o_ptr), actual_ac(o_ptr), 
			   actual_dd(o_ptr), actual_ds(o_ptr), k_info[o_ptr->k_idx].breakage), 5, j);

	prt(format("pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d  timeout = %-d",
	           o_ptr->pval, o_ptr->to_a, actual_to_h(o_ptr), actual_to_d(o_ptr), o_ptr->timeout), 6, j);

	prt(format("a_idx = %-4d  e_idx = %-4d  prefix = %-4d ident = %04x  cost = %ld",
	           o_ptr->a_idx, o_ptr->e_idx, o_ptr->prefix_idx, o_ptr->ident, (long)object_value(o_ptr)) , 7, j);

	prt_binary(f1, 9, j+2);
	prt_binary(f3, 9, j+36);
	prt("f .....AFFECT................SUST.  sfrtsiglLITEIGN..ehs...tddtadlhp f", 10, j);
	prt("l siwdcc..ssidsbsmhm........siwdcc  leeeenlu1234end..zdh...ariegrccc l", 11, j);
	prt("a tnieoh..trniplhaei........tnieoh  daglevoc....lei..keo...inslgnuuu a", 12, j);
	prt("g rtsxna..lcfgeoonlg........rtsxna  gteeiiwk....els..ntm...nirererrr g", 13, j);
	prt("s ........t.r.ewtath..............  shnpns......mee..oyo...ttppaxsss s", 14, j);
	prt("1 ........h.a.dss.ht..............  tr..v........mn..wpd....mt.vpeee 3", 15, j);

	prt_binary(f2, 17, j+2);
	prt("f fhbNOEFCT...................bwti", 18, j);
	prt("l rlrbdspcc...................loem", 19, j);
	prt("a edalstouo...................eurp", 20, j);
	prt("g alvneuitn...................snra", 21, j);
	prt("s cfedsns.f...................sdoc", 22, j);
    prt("2 te..e.m.....................,,rt", 23, j);

	prt("Resists:", 16, j+36);
	prt_resists(o_ptr, 0, RS_MAX / 2, 16, j+44);
	prt("ac el fr cl wt po ds lt dk", 17, j+44);
	prt_resists(o_ptr, RS_MAX / 2, RS_MAX, 18, j+44);
	prt("cn sn sh nx nt cs dn tm mn", 19, j+44);

	prt("Slays:",   20, j+36);
	prt_slays(o_ptr, 0, SL_MAX / 2, 20, j+44);
	prt("ev cs an pl ud dm hm pl fr", 21, j+44);
	prt_slays(o_ptr, SL_MAX / 2, SL_MAX, 22, j+44);
	prt("dr ly ac el fr cl po lt dk", 23, j+44);

	prt_alloc(o_ptr->tval, o_ptr->sval, 2, 70); 
}

/*
 * A list of tvals and their textual names
 */
static tval_desc_type tvals[31] =
{
	{ TV_SWORD,			"Sword"				},
	{ TV_POLEARM,		"Polearm"			},
	{ TV_HAFTED,		"Hafted Weapon"		},
	{ TV_BOW,			"Bow"				},
	{ TV_ARROW,			"Arrows"			},
	{ TV_BOLT,			"Bolts"				},
	{ TV_SHOT,			"Shots"				},
	{ TV_SHIELD,		"Shield"			},
	{ TV_HEADGEAR,		"Head Gear"			},
	{ TV_GLOVES,		"Gloves"			},
	{ TV_BOOTS,			"Boots"				},
	{ TV_CLOAK,			"Cloak"				},
	{ TV_DRAG_ARMOR,	"Dragon Scale Mail"	},
	{ TV_BODY_ARMOR,	"Body Armor"		},
	{ TV_RING,			"Ring"				},
	{ TV_AMULET,		"Amulet"			},
	{ TV_LITE,			"Light Source"		},
/*	{ TV_LITE_SPECIAL,	"Permanent Lite"	}, No non-artifacts of this type */
	{ TV_POTION,		"Potion"			},
	{ TV_POWDER,		"Powder"			},
	{ TV_SCROLL,		"Scroll"			},
	{ TV_WAND,			"Wand"				},
	{ TV_STAFF,			"Staff"				},
	{ TV_ROD,			"Rod"				},
	{ TV_TALISMAN,		"Talisman"			},
	{ TV_MAGIC_BOOK,	"Spellbook"			},
	{ TV_MUSIC,			"Musical Instrument"},
	{ TV_DIGGING,		"Digger"			},
	{ TV_FOOD,			"Food"				},
	{ TV_FLASK,			"Flask"				},
	{ 0,				NULL				}
};

/*
 * Get an object kind for creation (or zero)
 *
 * List up to 63 choices in three columns
 */
static int wiz_create_itemtype(void)
{
	int i, num, max_num;
	int col, row;
	int tval;

	cptr tval_desc;
	char ch;

	int choice[64];
	static const char choice_name[] = "abcdefghijklmnopqrstu"
	                                  "ABCDEFGHIJKLMNOPQRSTU"
	                                  "0123456789:;<=>?@%&*(";
	const char *cp;

	char buf[160];

	/* Clear screen */
	Term_clear();

	/* Print all tval's and their descriptions */
	for (num = 0; (num < 63) && tvals[num].tval; num++)
	{
		row = 2 + (num % 21);
		col = 30 * (num / 21);
		ch  = choice_name[num];
		prt(format("[%c] %s", ch, tvals[num].desc), row, col);
	}

	/* We need to know the maximal possible tval_index */
	max_num = num;

	/* Choose! */
	if (!get_com("Get what type of object? ", &ch)) return (0);

	/* Analyze choice */
	num = -1;
	if ((cp = strchr(choice_name, ch)) != NULL)	num = cp - choice_name;

	/* Bail out if choice is illegal */
	if ((num < 0) || (num >= max_num)) return (0);

	/* Base object type chosen, fill in tval */
	tval = tvals[num].tval;
	tval_desc = tvals[num].desc;

	/*** And now we go for k_idx ***/

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 63) && (i < z_info->k_max); i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Analyze matching items */
		if (k_ptr->tval == tval)
		{
			int j, k;

			/* Skip items with no distribution (special artifacts) */
			for (j = 0, k = 0; j < MAX_OBJ_ALLOC; j++) k += k_ptr->chance[j];
			if (!(k))  continue; 

			/* Prepare it */
			row = 2 + (num % 21);
			col = 30 * (num / 21);
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
	if (!get_com(format("What Kind of %s? ", tval_desc), &ch)) return (0);

	/* Analyze choice */
	num = -1;
	if ((cp = strchr(choice_name, ch)) != NULL)	num = cp - choice_name;

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

	p = "Enter new 'timeout' setting: ";
	sprintf(tmp_val, "%d", o_ptr->timeout);
	if (!get_string(p, tmp_val, 6)) return;
	o_ptr->timeout = atoi(tmp_val);
	wiz_display_item(o_ptr);
}

/*
 * Tweak an item
 */
static void wiz_xtra_item(object_type *o_ptr)
{
	cptr p;
	char tmp_val[80];
	int max;

	/* Hack -- leave artifacts alone */
	if (artifact_p(o_ptr)) return;

	if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_HAFTED) ||(o_ptr->tval == TV_POLEARM) ||
		(o_ptr->tval == TV_BODY_ARMOR))
	{
		if (o_ptr->tval == TV_BODY_ARMOR)
		{
			max = z_info->apx_max;
		}
		else max = z_info->wpx_max;

		p = "Enter new prefix index: ";
		sprintf(tmp_val, "%d", o_ptr->prefix_idx);
		if (!get_string(p, tmp_val, 6)) return;

		if (atoi(tmp_val) < max)
		{
			o_ptr->prefix_idx = atoi(tmp_val);
			wiz_display_item(o_ptr);
		}
	}

	p = "Enter new ego index: ";
	sprintf(tmp_val, "%d", o_ptr->e_idx);
	if (!get_string(p, tmp_val, 6)) return;

	if (atoi(tmp_val) < z_info->e_max)
	{
		o_ptr->e_idx = atoi(tmp_val);
		wiz_display_item(o_ptr);
	}
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
			apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE, TRUE);
		}

		/* Apply good magic, but first clear object */
		else if (ch == 'g' || ch == 'g')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, FALSE, TRUE, FALSE, TRUE);
		}

		/* Apply great magic, but first clear object */
		else if (ch == 'e' || ch == 'e')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, FALSE, TRUE, TRUE, TRUE);
		}
	}

	/* Notice change */
	if (changed)
	{
		/* Note Object history */
		i_ptr->origin_nature = ORIGIN_CHEAT;

		/* Restore the position information */
		i_ptr->iy = o_ptr->iy;
		i_ptr->ix = o_ptr->ix;
		i_ptr->next_o_idx = o_ptr->next_o_idx;
		i_ptr->marked = o_ptr->marked;

		/* Apply changes */
		object_copy(o_ptr, i_ptr);

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
	char *quality;

	bool good, great;

	object_type *i_ptr;
	object_type object_type_body;

	cptr q = "Rolls: %ld, Matches: %ld, Better: %ld, Worse: %ld, Other: %ld";

	/* Mega-Hack -- allow multiple artifacts XXX XXX XXX */
	if (artifact_p(o_ptr)) a_info[o_ptr->a_idx].status &= ~A_STATUS_CREATED;

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
			break;
		}

		/* Let us know what we are doing */
		message_format(MSG_CHEAT, 0, "Creating a lot of %s items. Base level = %d.",
		           quality, p_ptr->depth);
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
				Term_fresh();
			}

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Create an object */
			make_object(i_ptr, good, great, TRUE);

			/* Mega-Hack -- allow multiple artifacts XXX XXX XXX */
			if (artifact_p(i_ptr)) a_info[i_ptr->a_idx].status &= ~A_STATUS_CREATED;

			/* Test for the same tval and sval. */
			if ((o_ptr->tval) != (i_ptr->tval)) continue;
			if ((o_ptr->sval) != (i_ptr->sval)) continue;

			/* Check for match */
			if ((i_ptr->pval == o_ptr->pval) &&
			    (i_ptr->to_a == o_ptr->to_a) &&
			    (i_ptr->to_h == o_ptr->to_h) &&
			    (i_ptr->to_d == o_ptr->to_d) &&
				(i_ptr->prefix_idx == i_ptr->prefix_idx))
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
		message_format(MSG_CHEAT, 0, q, i, matches, better, worse, other);
		message_flush();
	}

	/* Hack -- Normally only make a single artifact */
	if (artifact_p(o_ptr)) a_info[o_ptr->a_idx].status |= A_STATUS_CREATED;
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
		if (carried)
		{
			/* Remove the weight of the old number of objects */
			p_ptr->total_weight -= (o_ptr->number * actual_weight(o_ptr));

			/* Add the weight of the new number of objects */
			p_ptr->total_weight += (tmp_int * actual_weight(o_ptr));
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
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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
		if (!get_com("[a]ccept [s]tatistics [r]eroll [t]weak [x]tra [q]uantity? ", &ch))
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

		if (ch == 'x' || ch == 'X')
		{
			wiz_xtra_item(i_ptr);
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
		message(MSG_CHEAT, 0, "Changes accepted.");

		/* Change */
		object_copy(o_ptr, i_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Ignore change */
	else
	{
		message(MSG_CHEAT, 0, "Changes ignored.");
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
	apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE, TRUE);

	/* Mark origin */
	i_ptr->origin_nature = ORIGIN_CHEAT;

	/* Drop the object from heaven */
	drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

	/* All done */
	message(MSG_CHEAT, 0, "Allocated.");
}

/*
 * Create the artifact with the specified number
 */
static void wiz_create_artifact(int a_idx)
{
	object_type *i_ptr;
	object_type object_type_body;
	int k_idx;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return;

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
	i_ptr->a_idx = a_idx;

	/* Extract the fields */
	i_ptr->pval = a_ptr->pval;
	i_ptr->ac = a_ptr->ac;
	i_ptr->dd = a_ptr->dd;
	i_ptr->ds = a_ptr->ds;
	i_ptr->to_a = a_ptr->to_a;
	i_ptr->to_h = a_ptr->to_h;
	i_ptr->to_d = a_ptr->to_d;
	i_ptr->weight = a_ptr->weight;
	i_ptr->prefix_idx = a_ptr->prefix_idx;

	/* Note Object history */
	i_ptr->origin_nature = ORIGIN_CHEAT;

	/* Drop the artifact from heaven */
	drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

	/* All done */
	message(MSG_CHEAT, 0, "Allocated.");
}

/*
 * Cure everything instantly
 */
static void do_cmd_wiz_cure_all(void)
{
	/* Remove curses */
	(void)remove_all_curse();

	/* Restore stats */
	(void)do_res_stat(A_STR);
	(void)do_res_stat(A_INT);
	(void)do_res_stat(A_WIS);
	(void)do_res_stat(A_CON);
	(void)do_res_stat(A_DEX);
	(void)do_res_stat(A_CHR);

	/* Restore the level */
	(void)restore_exp();

	/* Heal the player */
	p_ptr->chp = p_ptr->mhp;
	p_ptr->chp_frac = 0;

	/* Restore mana */
	p_ptr->csp = p_ptr->msp;
	p_ptr->csp_frac = 0;

	/* Cure stuff */
	(void)set_blind(0);
	(void)set_confused(0);
	(void)set_poisoned(0);
	(void)set_diseased(0);
	(void)set_afraid(0);
	(void)set_paralyzed(0);
	(void)set_image(0);
	(void)set_stun(0);
	(void)set_cut(0);
	(void)set_slow(0);

	/* No longer hungry */
	(void)set_food(PY_FOOD_MAX - 1);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	handle_stuff();

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
		if (!get_string(ppp, tmp_val, 4)) return;

		/* Extract request */
		p_ptr->command_arg = atoi(tmp_val);
	}

	/* Paranoia */
	if (p_ptr->command_arg < 0) p_ptr->command_arg = 0;

	/* Paranoia */
	if (p_ptr->command_arg > MAX_DEPTH - 1) p_ptr->command_arg = MAX_DEPTH - 1;

	/* Accept request */
	message_format(MSG_CHEAT, 0, "You jump to dungeon level %d.", p_ptr->command_arg);

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

	object_type *i_ptr;
	object_type object_type_body;

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

			/* Know alchemy stuff too */
			potion_alch[i_ptr->sval].known1 = TRUE;
			potion_alch[i_ptr->sval].known2 = TRUE;
		}
	}
}

/*
 * Hack -- Rerate Hitpoints
 */
static void do_cmd_rerate(void)
{
	int min_value, max_value, i, percent;

	min_value = (PY_MAX_LEVEL * 3 * (p_ptr->hitdie - 1)) / 8;
	min_value += PY_MAX_LEVEL;

	max_value = (PY_MAX_LEVEL * 5 * (p_ptr->hitdie - 1)) / 8;
	max_value += PY_MAX_LEVEL;

	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* Rerate */
	while (TRUE)
	{
		/* Collect values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			p_ptr->player_hp[i] = randint(p_ptr->hitdie);
			p_ptr->player_hp[i] += p_ptr->player_hp[i - 1];
		}

		/* Legal values */
		if ((p_ptr->player_hp[PY_MAX_LEVEL - 1] >= min_value) &&
		    (p_ptr->player_hp[PY_MAX_LEVEL - 1] <= max_value)) break;
	}

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
	                (p_ptr->hitdie + ((PY_MAX_LEVEL - 1) * p_ptr->hitdie)));

	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Handle stuff */
	handle_stuff();

	/* Message */
	message_format(MSG_CHEAT, 0, "Current Life Rating is %d/100.", percent);
}

/*
 * Summon some creatures
 */
static void do_cmd_wiz_summon(int num)
{
	int i;

	for (i = 0; i < num; i++)
	{
		(void)summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, 0);
	}
}

/*
 * Summon a creature of the specified type
 *
 * This function is rather dangerous XXX XXX XXX
 */
static void do_cmd_wiz_named(int r_idx)
{
	int i, x, y;

	/* Paranoia */
	if (!r_idx) return;
	if (r_idx >= z_info->r_max) return;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		/* Pick a location */
		scatter(&y, &x, p_ptr->py, p_ptr->px, 1);

		/* Require empty grids */
		if (!cave_empty_bold(y, x)) continue;

		/* Place it (allow groups) */
		if (place_monster_aux(y, x, r_idx, TRUE, TRUE, 0)) break;
	}
}

/*
 * Summon a creature of the specified type
 *
 * This function is rather dangerous XXX XXX XXX
 */
static void do_cmd_wiz_named_unique(int u_idx)
{
	int i, r_idx, x, y;

	/* Paranoia */
	if (!u_idx) return;
	if (u_idx >= z_info->u_max) return;

	r_idx = u_info[u_idx].r_idx;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		/* Pick a location */
		scatter(&y, &x, p_ptr->py, p_ptr->px, 1);

		/* Require empty grids */
		if (!cave_empty_bold(y, x)) continue;

		/* Place it (allow groups) */
		if (place_monster_aux(y, x, r_idx, TRUE, TRUE, PLACE_UNIQUE)) break;
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

		/* Optimize -- Repair flags */
		repair_mflag_mark = repair_mflag_show = TRUE;

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
		case 'w': mask |= (CAVE_WALL); break;
	}

	/* Scan map */
	for (y = p_ptr->wy; y < p_ptr->wy + SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx + SCREEN_WID; x++)
		{
			byte a = TERM_RED;

			if (!in_bounds_fully(y, x)) continue;

			/* Given mask, show only those grids */
			if (mask && !(cave_info[y][x] & mask)) continue;

			/* Given no mask, show unknown grids */
			if (!mask && (cave_info[y][x] & (CAVE_MARK))) continue;

			/* Color */
			if (cave_floor_bold(y, x)) a = TERM_YELLOW;

			/* Display player/floors/walls */
			if ((y == p_ptr->py) && (x == p_ptr->px))
			{
				print_rel('@', a, y, x);
			}
			else if (cave_floor_bold(y, x))
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
	message(MSG_GENERIC, 0, "Press any key.");
	message_flush();

	/* Redraw map */
	prt_map();
}
#ifdef ALLOW_SPOILERS

/*
 * The spoiler file being created
 */
static FILE *fff = NULL;

/*
 * Extract a textual representation of an attribute
 */
static cptr attr_to_text(byte a)
{
	switch (a)
	{
		case TERM_DARK:    return ("xxx");
		case TERM_WHITE:   return ("White");
		case TERM_SLATE:   return ("Slate");
		case TERM_ORANGE:  return ("Orange");
		case TERM_RED:     return ("Red");
		case TERM_GREEN:   return ("Green");
		case TERM_BLUE:    return ("Blue");
		case TERM_UMBER:   return ("Umber");
		case TERM_L_DARK:  return ("L.Dark");
		case TERM_L_WHITE: return ("L.Slate");
		case TERM_VIOLET:  return ("Violet");
		case TERM_YELLOW:  return ("Yellow");
		case TERM_L_RED:   return ("L.Red");
		case TERM_L_GREEN: return ("L.Green");
		case TERM_L_BLUE:  return ("L.Blue");
		case TERM_L_UMBER: return ("L.Umber");
	}

	/* Oops */
	return ("Icky");
}

/*
 * A tval grouper
 */
typedef struct
{
	byte tval;
	cptr name;
} grouper;

/*
 * Item Spoilers by Ben Harrison (benh@phial.com)
 */

/*
 * The basic items categorized by type
 */
static grouper group_item[32] =
{
	{ TV_SHOT,		"Ammo" },
	{ TV_ARROW,		  NULL },
	{ TV_BOLT,		  NULL },

	{ TV_BOW,		"Shooters" },

	{ TV_SWORD,		"Weapons" },
	{ TV_POLEARM,	  NULL },
	{ TV_HAFTED,	  NULL },

	{ TV_DIGGING,	"Diggers" },

	{ TV_BODY_ARMOR,	"Armour (Body)" },
	{ TV_DRAG_ARMOR,	  NULL },

	{ TV_CLOAK,		"Armour (Misc)" },
	{ TV_SHIELD,	  NULL },
	{ TV_HEADGEAR,	  NULL },
	{ TV_GLOVES,	  NULL },
	{ TV_BOOTS,		  NULL },

	{ TV_AMULET,	"Amulets" },
	{ TV_RING,		"Rings" },

	{ TV_SCROLL,	"Scrolls" },
	{ TV_POTION,	"Potions" },
	{ TV_POWDER,	"Powders" },
	{ TV_FOOD,		"Food" },

	{ TV_ROD,		"Rods" },
	{ TV_TALISMAN,	"Talismans" },
	{ TV_WAND,		"Wands" },
	{ TV_STAFF,		"Staffs" },

	{ TV_MAGIC_BOOK,	"Spellbooks" },

	{ TV_FLASK,		"Various" },
	{ TV_LITE,		  NULL },
	{ TV_LITE_SPECIAL,NULL },

	{ TV_MUSIC,		"Musical Instruments" },

	{ 0, "" }
};

/*
 * Describe the kind
 */
static void kind_info(char *buf, char *dam, char *wgt, int *lev, s32b *val, int k)
{
	object_kind *k_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Prepare a fake item */
	object_prep(i_ptr, k);

	/* Obtain the "kind" info */
	k_ptr = &k_info[i_ptr->k_idx];

	/* It is known */
	i_ptr->ident |= (IDENT_KNOWN);

	/* Cancel bonuses */
	i_ptr->pval = 0;
	i_ptr->to_a = 0;
	i_ptr->to_h = 0;
	i_ptr->to_d = 0;

	/* Level */
	(*lev) = k_ptr->level;

	/* Value */
	(*val) = object_value(i_ptr);

	/* Hack */
	if (!buf || !dam || !wgt) return;

	/* Description (too brief) */
	object_desc_store(buf, i_ptr, FALSE, 0);

	/* Misc info */
	strcpy(dam, "");

	/* Damage */
	switch (i_ptr->tval)
	{
		/* Bows */
		case TV_BOW:
		{
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			sprintf(dam, "%dd%d", actual_dd(i_ptr), actual_ds(i_ptr));
			break;
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			sprintf(dam, "%dd%d", actual_dd(i_ptr), actual_ds(i_ptr));
			break;
		}

		/* Armour */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_HEADGEAR:
		case TV_SHIELD:
		case TV_BODY_ARMOR:
		case TV_DRAG_ARMOR:
		{
			sprintf(dam, "%d", i_ptr->ac);
			break;
		}
	}

	/* Weight */
	sprintf(wgt, "%3d.%d", i_ptr->weight / 10, i_ptr->weight % 10);
}

/*
 * Create a spoiler file for items
 */
static void spoil_obj_desc(cptr fname)
{
	int i, j, k, l, s, t, n = 0;

	u16b *who;

	char buf[1024];

	char wgt[80];
	char dam[80];

	cptr format = " %-54s%7s%6s%4s%9s\n";

	/* Allocate the "who" array */
	C_MAKE(who, z_info->k_max, u16b);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		message(MSG_FAIL, 0, "Cannot create spoiler file.");
		return;
	}

	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Header */
	fprintf(fff, "Spoiler File -- Basic Items for %s Version %s\n\n\n",
	        VERSION_NAME, VERSION_STRING);

	/* More Header */
	fprintf(fff, format, "Description", "Dam/AC", "Wgt", "Lev", "Cost");
	fprintf(fff, format, "-------------------------------------------",
 	        "------", "---", "---", "----");

	/* List the groups */
	for (i = 0; TRUE; i++)
	{
		/* Write out the group title */
		if (group_item[i].name)
		{
			/* Hack -- bubble-sort by cost and then level */
			for (s = 0; s < n - 1; s++)
			{
				for (t = 0; t < n - 1; t++)
				{
					int i1 = t;
					int i2 = t + 1;

					int e1;
					int e2;

					s32b t1;
					s32b t2;

					kind_info(NULL, NULL, NULL, &e1, &t1, who[i1]);
					kind_info(NULL, NULL, NULL, &e2, &t2, who[i2]);

					if ((t1 > t2) || ((t1 == t2) && (e1 > e2)))
					{
						int tmp = who[i1];
						who[i1] = who[i2];
						who[i2] = tmp;
					}
				}
			}

			/* Spoil each item */
			for (s = 0; s < n; s++)
			{
				int e;
				s32b v;

				/* Describe the kind */
				kind_info(buf, dam, wgt, &e, &v, who[s]);

				/* Dump it */
				fprintf(fff, " %-54s%7s%6s%4d%9ld\n", buf, dam, wgt, e, (long)(v));
			}

			/* Start a new set */
			n = 0;

			/* Notice the end */
			if (!group_item[i].tval) break;

			/* Start a new set */
			fprintf(fff, "\n\n%s\n\n", group_item[i].name);
		}

		/* Get legal item types */
		for (k = 1; k < z_info->k_max; k++)
		{
			object_kind *k_ptr = &k_info[k];

			/* Skip wrong tval's */
			if (k_ptr->tval != group_item[i].tval) continue;

			/* Skip items with no distribution (special artifacts) */
			for (j = 0, l = 0; j < MAX_OBJ_ALLOC; j++) l += k_ptr->chance[j];
			if (!(l))  continue; 

			/* Save the index */
			who[n++] = k;
		}
	}

	/* Free the "who" array */
	KILL(who);

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		message(MSG_FAIL, 0, "Cannot close spoiler file.");
		return;
	}

	/* Message */
	message(MSG_SUCCEED, 0, "Successfully created a spoiler file.");
}

/*
 * Artifact Spoilers by: randy@PICARD.tamu.edu (Randy Hutson)
 */

/*
 * The artifacts categorized by type
 */
static grouper group_artifact[19] =
{
	{ TV_SWORD,		"Edged Weapons" },
	{ TV_POLEARM,	"Polearms" },
	{ TV_HAFTED,	"Hafted Weapons" },
	{ TV_BOW,		"Shooters" },
	{ TV_DIGGING,	"Diggers" },

	{ TV_BODY_ARMOR,	"Body Armor" },
	{ TV_DRAG_ARMOR,	  NULL },

	{ TV_CLOAK,		"Cloaks" },
	{ TV_SHIELD,	"Shields" },
	{ TV_HEADGEAR,	"Head Gear" },
	{ TV_GLOVES,	"Gloves" },
	{ TV_BOOTS,		"Boots" },

	{ TV_LITE,		"Light Sources" },
	{ TV_LITE_SPECIAL, NULL},
	{ TV_AMULET,	"Amulets" },
	{ TV_RING,		"Rings" },
	{ TV_MAGIC_BOOK,"Spellbooks"},
	{ TV_MUSIC,		"Instruments"},

	{ 0, NULL }
};

/*
 * Write out `n' of the character `c' to the spoiler file
 */
static void spoiler_out_n_chars(int n, char c)
{
	while (--n >= 0) fputc(c, fff);
}

/*
 * Write out `n' blank lines to the spoiler file
 */
static void spoiler_blanklines(int n)
{
	spoiler_out_n_chars(n, '\n');
}

/*
 * Write a line to the spoiler file and then "underline" it with hypens
 */
static void spoiler_underline(cptr str)
{
	text_out(str);
	text_out("\n");
	spoiler_out_n_chars(strlen(str), '-');
	text_out("\n");
}

/*
 * Create a spoiler file for artifacts
 */
static void spoil_artifact(cptr fname)
{
	int i, j;

	object_type *i_ptr;
	object_type object_type_body;

	char buf[1024];

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		message(MSG_FAIL, 0, "Cannot create spoiler file.");
		return;
	}

	text_out_hook = text_out_to_file;
	text_out_file = fff;

	sprintf(buf, "Artifact Spoilers for %s Version %s",
	        VERSION_NAME, VERSION_STRING);
	spoiler_underline(buf);

	/* List the artifacts by tval */
	for (i = 0; group_artifact[i].tval; i++)
	{
		/* Write out the group title */
		if (group_artifact[i].name)
		{
			spoiler_blanklines(2);
			spoiler_underline(group_artifact[i].name);
			spoiler_blanklines(1);
		}

		/* Now search through all of the artifacts */
		for (j = 1; j < z_info->a_max; ++j)
		{
			artifact_type *a_ptr = &a_info[j];

			/* We only want objects in the current group */
			if (a_ptr->tval != group_artifact[i].tval) continue;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Attempt to "forge" the artifact */
			if (!make_fake_artifact(i_ptr, j)) continue;

			/* Artifact name */
			object_desc_store(buf, i_ptr, TRUE, 1);
			text_out("-- ");
			text_out(buf);
			text_out("\n");

			/* Write out the artifact description to the spoiler file */
			list_object(i_ptr, OBJECT_INFO_FULL);

			text_out(format("Level %u, Rarity %u, %d.%d lbs, %ld Gold.\n\n",
				a_ptr->level, a_ptr->rarity,
				a_ptr->weight / 10, a_ptr->weight % 10, (long)a_ptr->cost));
		}
	}

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		message(MSG_FAIL, 0, "Cannot close spoiler file.");
		return;
	}

	/* Message */
	message(MSG_SUCCEED, 0, "Successfully created a spoiler file.");
}

/*
 * Create a spoiler file for monsters
 */
static void spoil_mon_desc(cptr fname)
{
	int i, n = 0;

	char buf[1024];

	char nam[80];
	char lev[80];
	char rar[80];
	char spd[80];
	char ac[80];
	char hp[80];
	char exp[80];

	monster_list_entry *who;
	u16b why = 2;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		message(MSG_FAIL, 0, "Cannot create spoiler file.");
		return;
	}

	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Dump the header */
	fprintf(fff, "Monster Spoilers for %s Version %s\n",
	        VERSION_NAME, VERSION_STRING);
	fprintf(fff, "------------------------------------------\n\n");

	/* Dump the header */
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
	        "Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
	        "----", "---", "---", "---", "--", "--", "-----------");

	/* Allocate the "who" array */
	C_MAKE(who, M_LIST_ITEMS, monster_list_entry);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[n++].r_idx = i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_mon_sort_comp_hook;
	ang_sort_swap = ang_mon_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

	/* "Saturate" the monster list with all the appropriate uniques */
	saturate_mon_list(who, &n, TRUE, TRUE);

	/* Scan again */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = get_monster_fake(who[i].r_idx, 0, who[i].u_idx);

		cptr name = (monster_name_idx(who[i].r_idx, 0, who[i].u_idx));
		cptr attr_desc;

		/* Get the "name" */
		if (who[i].u_idx) 
		{
			sprintf(nam, "[U] %s", name);
		}
		else
		{
			sprintf(nam, "The %s", name);
		}

		/* Level */
		sprintf(lev, "%d", r_ptr->level);

		/* Rarity */
		sprintf(rar, "%d", r_ptr->rarity);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			sprintf(spd, "+%d", (r_ptr->speed - 110));
		}
		else
		{
			sprintf(spd, "-%d", (110 - r_ptr->speed));
		}

		/* Armor Class */
		sprintf(ac, "%d", r_ptr->ac);

		/* Hitpoints */
		if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
		{
			sprintf(hp, "%d", r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			sprintf(hp, "%dd%d", r_ptr->hdice, r_ptr->hside);
		}

		/* Experience */
		sprintf(exp, "%ld", (long)(r_ptr->mexp));

		if (r_ptr->flags1 & RF1_ATTR_MULTI) attr_desc = "M. hued";
		else if (r_ptr->flags1 & RF1_ATTR_MIMIC) attr_desc = "Mimic";
		else if (r_ptr->flags1 & RF1_ATTR_CLEAR) attr_desc = "Clear";
		else attr_desc = attr_to_text(r_ptr->d_attr);

		/* Hack -- use visual instead */
		sprintf(exp, "%s '%c'", attr_desc, r_ptr->d_char);

		/* Dump the info */
		fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		        nam, lev, rar, spd, hp, ac, exp);
	}

	/* End it */
	fprintf(fff, "\n");

	/* Free the "who" array */
	KILL(who);

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		message(MSG_FAIL, 0, "Cannot close spoiler file.");
		return;
	}

	/* Worked */
	message(MSG_SUCCEED, 0, "Successfully created a spoiler file.");
}

/*
 * Create a spoiler file for monsters (-SHAWN-)
 */
static void spoil_mon_info(cptr fname)
{
	char buf[1024];
	int i;
	u16b why = 2;
	monster_list_entry *who;
	int count = 0;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		message(MSG_FAIL, 0, "Cannot create spoiler file.");
		return;
	}

	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Dump the header */
	sprintf(buf, "Monster Spoilers for %s Version %s\n",
	        VERSION_NAME, VERSION_STRING);
	text_out(buf);
	text_out("------------------------------------------\n\n");

	/* Allocate the "who" array */
	C_MAKE(who, M_LIST_ITEMS, monster_list_entry);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[count++].r_idx = i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_mon_sort_comp_hook;
	ang_sort_swap = ang_mon_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, count);

	/* "Saturate" the monster list with all the appropriate uniques */
	saturate_mon_list(who, &count, TRUE, TRUE);

	/*
	 * List all monsters in order
	 */
	for (i = 0; i < count; i++)
	{
		monster_race *r_ptr = get_monster_fake(who[i].r_idx, 0, who[i].u_idx);

		/* Prefix */
		if (who[i].u_idx)
		{
			text_out("[U] ");
		}
		else
		{
			text_out("The ");
		}

		/* Name */
		sprintf(buf, "%s  (", (monster_name_idx(who[i].r_idx, 0, who[i].u_idx)));	/* ---)--- */
		text_out(buf);

		/* Color */
		if (r_ptr->flags1 & RF1_ATTR_MULTI) text_out("Multi-hued");
		else if (r_ptr->flags1 & RF1_ATTR_MIMIC) text_out("Mimic");
		else if (r_ptr->flags1 & RF1_ATTR_CLEAR) text_out("Clear");
		else text_out(attr_to_text(r_ptr->d_attr));

		/* Symbol --(-- */
		sprintf(buf, " '%c')\n", r_ptr->d_char);
		text_out(buf);

		/* Indent */
		sprintf(buf, "=== ");
		text_out(buf);

		/* Number */
		if (who[i].u_idx) sprintf(buf, "Num:%d/%d  ", who[i].r_idx, who[i].u_idx);
		else sprintf(buf, "Num:%d  ", who[i].r_idx);
		text_out(buf);

		/* Level */
		sprintf(buf, "Lev:%d  ", r_ptr->level);
		text_out(buf);

		/* Rarity */
		sprintf(buf, "Rar:%d  ", r_ptr->rarity);
		text_out(buf);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			sprintf(buf, "Spd:+%d  ", (r_ptr->speed - 110));
		}
		else
		{
			sprintf(buf, "Spd:-%d  ", (110 - r_ptr->speed));
		}
		text_out(buf);

		/* Hitpoints */
		if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
		{
			sprintf(buf, "Hp:%d  ", r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			sprintf(buf, "Hp:%dd%d  ", r_ptr->hdice, r_ptr->hside);
		}
		text_out(buf);

		/* Armor Class */
		sprintf(buf, "Ac:%d  ", r_ptr->ac);
		text_out(buf);

		/* Experience */
		sprintf(buf, "Exp:%ld\n", (long)(r_ptr->mexp));
		text_out(buf);

		/* Main body of lore */
		describe_monster(who[i].r_idx, who[i].u_idx, TRUE);

		/* Terminate the entry */
		text_out("\n");
	}

	/* Free the "who" array */
	KILL(who);

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		message(MSG_FAIL, 0, "Cannot close spoiler file.");
		return;
	}

	message(MSG_SUCCEED, 0, "Successfully created a spoiler file.");
}

/*
 * Do the calculations for a specific flag 
 */
static void spoil_mon_calc_aux(monster_list_entry *who, int n, int fset, u32b flag, bool full)
{
	int i, j, k, l, m;
	char nam[80];
	char title[7];
	cptr name;

	bool no_flag;

	u16b y_flag[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	u16b x_flag[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	u16b y_flag_u[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	u16b x_flag_u[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

	/* Check flag */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = get_monster_fake(who[i].r_idx, 0, who[i].u_idx);

		/* Ignore town monsters */
		if (!r_ptr->level) continue;

		l = r_ptr->level / 10;
		if (l > 9) l = 9;

		no_flag = FALSE;

		/* Hack - handle different flagsets */
		if (fset == 2) if (!(r_ptr->flags2 & flag)) no_flag = TRUE;
		if (fset == 3) if (!(r_ptr->flags3 & flag)) no_flag = TRUE;

		/* No resistance */
		if (no_flag)
		{
			if (who[i].u_idx)
			{
				x_flag_u[l]++;
				x_flag_u[10]++;
			}
			else
			{
				x_flag[l]++;
				x_flag[10]++;
			}

			continue;
		}
		
		if (full)
		{
			/* Get monster name */
			name = (monster_name_idx(who[i].r_idx, 0, who[i].u_idx));
		
			/* Prefix */
			if (who[i].u_idx) 
			{
				sprintf(nam, "[U] %s", name);
			}
			else
			{
				sprintf(nam, "The %s", name);
			}

			/* List monster */
			fprintf(fff, "%s\n", nam);
		}

		if (who[i].u_idx)
		{
			y_flag_u[l]++;
			y_flag_u[10]++;
		}
		else
		{
			y_flag[l]++;
			y_flag[10]++;
		}
	}

	fprintf(fff, "\n       |     Non-uniques     |        Uniques\n");
	fprintf(fff, "Levels | + | - |Distrib|Total| + | - |Distrib|Total\n");
	fprintf(fff, "-------+---+---+-------+-----+---+---+-------+-----\n");
	fprintf(fff, "");

	/* Print summary */
	for (i = 0; i < 11; i++)
	{
		j = y_flag[10];
		l = y_flag[i] + x_flag[i];
		k = y_flag_u[10];
		m = y_flag_u[i] + x_flag_u[i];

		if (i < 9) sprintf (title, "%3d-%3d", i * 10, i * 10 + 9);
		else if (i < 10) sprintf (title, "  90+  ");
		else sprintf(title, " TOTAL ");

		fprintf(fff, "%7s|%3d|%3d|  %3d%% |%3d%% |%3d|%3d|  %3d%% |%3d%%\n",
			title, y_flag[i], x_flag[i], 
			(j > 0 ? ((100 * y_flag[i]) / j) : 0), (l > 0 ? ((100 * y_flag[i]) / l) : 0),
			y_flag_u[i], x_flag_u[i], 
			(k > 0 ? ((100 * y_flag_u[i]) / k) : 0), (m > 0 ? ((100 * y_flag_u[i]) / m) : 0));
	}
}

/*
 * Create a spoiler file for monsters
 */
static void spoil_mon_calc(cptr fname)
{
	int i, n = 0;

	char buf[1024];
	bool full;

	monster_list_entry *who;
	u16b why = 2;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		message(MSG_FAIL, 0, "Cannot create spoiler file.");
		return;
	}

	text_out_hook = text_out_to_file;
	text_out_file = fff;

	full = get_check("Verbose list? ");

	/* Dump the header */
	fprintf(fff, "Monster Statistics for %s Version %s\n",
	        VERSION_NAME, VERSION_STRING);
	fprintf(fff, "------------------------------------------\n\n");

	/* Allocate the "who" array */
	C_MAKE(who, M_LIST_ITEMS, monster_list_entry);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[n++].r_idx = i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_mon_sort_comp_hook;
	ang_sort_swap = ang_mon_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

	/* "Saturate" the monster list with all the appropriate uniques */
	saturate_mon_list(who, &n, TRUE, TRUE);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Acid resistance\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_RES_ACID, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Acid vulnerability\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_HURT_ACID, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Elec resistance\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_RES_ELEC, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Elec vulnerability\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_HURT_ELEC, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Fire resistance\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_RES_FIRE, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Fire vulnerability\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_HURT_FIRE, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Cold resistance\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_RES_COLD, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Cold vulnerability\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_HURT_COLD, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Invisibility\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_INVISIBLE, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Kill Wall\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_KILL_WALL, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Pass Wall\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_PASS_WALL, full);
	
	fprintf(fff, "\n------------------\n");
	fprintf(fff, "No Blind\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_NO_BLIND, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "No Fear\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_NO_FEAR, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "Res Conf\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_RES_CONF, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "No Stun\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_NO_STUN, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "No Sleep\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_NO_SLEEP, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "No Cut\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 3, RF3_NO_CUT, full);

	fprintf(fff, "\n------------------\n");
	fprintf(fff, "See invisible\n");
	fprintf(fff, "------------------\n");

	spoil_mon_calc_aux(who, n, 2, RF2_SEE_INVIS, full);

	/* End it */
	fprintf(fff, "\n");

	/* Free the "who" array */
	KILL(who);

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		message(MSG_FAIL, 0, "Cannot close spoiler file.");
		return;
	}

	/* Worked */
	message(MSG_SUCCEED, 0, "Successfully created a spoiler file.");
}

/*
 * Create Spoiler files
 */
static void do_cmd_spoilers(void)
{
	char ch;

	/* Save screen */
	screen_save();

	/* Interact */
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Info */
		prt("Create a spoiler file.", 2, 0);

		/* Prompt for a file */
		prt("(1) Brief Object Info (obj-desc.spo)", 5, 5);
		prt("(2) Full Artifact Info (artifact.spo)", 6, 5);
		prt("(3) Brief Monster Info (mon-desc.spo)", 7, 5);
		prt("(4) Full Monster Info (mon-info.spo)", 8, 5);
		prt("(5) Monster Analysis (mon-data.spo)", 9, 5);

		/* Prompt */
		prt("Command: ", 13, 0);

		/* Get a choice */
		ch = inkey();

		/* Escape */
		if (ch == ESCAPE)
		{
			break;
		}

		/* Option (1) */
		else if (ch == '1')
		{
			spoil_obj_desc("obj-desc.spo");
		}

		/* Option (2) */
		else if (ch == '2')
		{
			spoil_artifact("artifact.spo");
		}

		/* Option (3) */
		else if (ch == '3')
		{
			spoil_mon_desc("mon-desc.spo");
		}

		/* Option (4) */
		else if (ch == '4')
		{
			spoil_mon_info("mon-info.spo");
		}

		/* Option (4) */
		else if (ch == '5')
		{
			spoil_mon_calc("mon-data.spo");
		}

		/* Oops */
		else
		{
			bell("Illegal command for spoilers!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Load screen */
	screen_load();
}

#endif /* ALLOW_SPOILERS */

/*
 * Ask for and parse a "debug command"
 *
 * The "p_ptr->command_arg" may have been set.
 */
void do_cmd_debug(void)
{
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

#endif /* ALLOW_SPOILERS */

		/* Hack -- Help */
		case '?':
		{
			do_cmd_wiz_help();
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

		/* Create an artifact */
		case 'C':
		{
			wiz_create_artifact(p_ptr->command_arg);
			break;
		}

		/* Detect everything */
		case 'd':
		{
			detect_all();
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
			acquirement(p_ptr->py, p_ptr->px, p_ptr->command_arg, FALSE, TRUE);
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
			(void)ident_spell();
			break;
		}

		/* Go up or down in the dungeon */
		case 'j':
		{
			do_cmd_wiz_jump();
			break;
		}

		/* Learn about objects */
		case 'l':
		{
			if (p_ptr->command_arg <= 0) p_ptr->command_arg = z_info->k_max-1;
			do_cmd_wiz_learn();
			break;
		}

		/* Magic Mapping */
		case 'm':
		{
			map_area();
			break;
		}

		/* Summon Named Monster */
		case 'n':
		{
			do_cmd_wiz_named(p_ptr->command_arg);
			break;
		}

		/* Summon Named Unique */
		case 'N':
		{
			do_cmd_wiz_named_unique(p_ptr->command_arg);
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
			teleport_player(10);
			break;
		}

		/* Dimension Door */
		case 'P':
		{
			dimen_door(25, 0);
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
			teleport_player(100);
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
			acquirement(p_ptr->py, p_ptr->px, p_ptr->command_arg, TRUE, TRUE);
			break;
		}

		/* Wizard Light the Level */
		case 'w':
		{
			wiz_lite();
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

		/* Create chest */
		case 'X':
		{
			int yy = p_ptr->py;
			int xx = p_ptr->px;

			/* Hack - boundary check */
			if cave_naked_bold(yy + 1, xx) place_chest(yy + 1, xx);
			else if cave_naked_bold(yy, xx + 1) place_chest(yy, xx + 1);
			else if cave_naked_bold(yy - 1, xx) place_chest(yy - 1, xx);
			else if cave_naked_bold(yy, xx - 1) place_chest(yy, xx - 1);
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
			do_cmd_wiz_hack_ben();
			break;
		}

		/* Oops */
		default:
		{
			message(MSG_FAIL, 0, "That is not a valid debug command.");
			break;
		}
	}
}

#else /* ALLOW_DEBUG */

#ifdef MACINTOSH
static int i = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_DEBUG */


