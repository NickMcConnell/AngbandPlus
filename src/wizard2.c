/* File: wizard2.c */

/* Purpose: Wizard commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



#ifdef ALLOW_WIZARD

/*
 * Dimension Door
 */
static bool wiz_dimension_door(void)
{
	int	x = 0, y = 0;
	cave_type *c_ptr;

	if (!tgt_pt(&x, &y, FALSE)) return FALSE;
	c_ptr = &cave[y][x];

	if (!teleportable_grid(c_ptr) && (c_ptr->feat != FEAT_AIR))
	{
#ifdef JP
		msg_print("精霊界から物質界に戻る時うまくいかなかった！");
#else
		msg_print("You fail to exit the astral plane correctly!");
#endif

		teleport_player(10);
	}
	else teleport_player_to(y, x, FALSE, FALSE);

	return (TRUE);
}


/*
 * Create the artifact of the specified number -- DAN
 *
 */
static void wiz_create_named_art(int a_idx)
{
	/* Create the artifact */
	create_named_art(a_idx, py, px);

	/* All done */
	msg_print("Allocated.");
}


/*
 * Hack -- quick debugging hook
 */
static void do_cmd_wiz_hack_ben(void)
{
	/* Oops */
	msg_print("Oops.");
	(void)probing();
}



#ifdef MONSTER_HORDES

/* Summon a horde of monsters */
static void do_cmd_summon_horde(void)
{
	int wy = py, wx = px;
	int attempts = 1000;

	while (--attempts)
	{
		scatter(&wy, &wx, py, px, 3, 0);
		if (cave_naked_bold(wy, wx)) break;
	}

	(void)alloc_horde(wy, wx);
}

#endif /* MONSTER_HORDES */


/*
 * Output a long int in binary format.
 */
static void prt_binary(u32b flags, int row, int col)
{
	int        	i;
	u32b        bitmask;

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


#define K_MAX_DEPTH 110

/*
 * Output a rarity graph for a type of object.
 */
static void prt_alloc(byte tval, byte sval, int row, int col)
{
	int i, j;
	int home = 0;
	u32b maxr = 1, maxt = 1, ratio;
	u32b rarity[K_MAX_DEPTH];
	u32b total[K_MAX_DEPTH];
	s32b maxd = 1, display[22];
	byte c = TERM_WHITE;
	cptr r = "+--common--+";
	object_kind *k_ptr;


	/* Get the entry */
	alloc_entry *table = alloc_kind_table;

	/* Wipe the tables */
	(void)C_WIPE(rarity, K_MAX_DEPTH, u32b);
	(void)C_WIPE(total, K_MAX_DEPTH, u32b);
	(void)C_WIPE(display, 22, s32b);

	/* Scan all entries */
	for (i = 0; i < K_MAX_DEPTH; i++)
	{
		int total_frac = 0;
		for (j = 0; j < alloc_kind_size; j++)
		{
			int prob = 0;

			if (table[j].level <= i)
			{
				prob = table[j].prob1 * GREAT_OBJ * K_MAX_DEPTH;
			}
			else if (table[j].level - 1 > 0)
			{
				prob = table[j].prob1 * i * K_MAX_DEPTH / (table[j].level - 1);
			}

			/* Acquire this kind */
			k_ptr = &k_info[table[j].index];

			/* Accumulate probabilities */
			total[i] += prob / (GREAT_OBJ * K_MAX_DEPTH);
			total_frac += prob % (GREAT_OBJ * K_MAX_DEPTH);

			/* Accumulate probabilities */
			if ((k_ptr->tval == tval) && (k_ptr->sval == sval))
			{
				home = k_ptr->level;
				rarity[i] += prob;
			}
		}
		total[i] += total_frac / (GREAT_OBJ * K_MAX_DEPTH);
	}

	/* Find maxima */
	for (i = 0; i < K_MAX_DEPTH; i++)
	{
		if (rarity[i] > maxr) maxr = rarity[i];
		if (total[i] > maxt) maxt = total[i];
	}

	if (maxr / (GREAT_OBJ * K_MAX_DEPTH) != 0)
		ratio = maxt / (maxr / (GREAT_OBJ * K_MAX_DEPTH));
	else
		ratio = 99999L;

	/* Simulate a log graph */
	if (ratio > 1000)
	{
		c = TERM_L_WHITE;
		r = "+-uncommon-+";
	}
	if (ratio > 3000)
	{
		c = TERM_SLATE;
		r = "+---rare---+";
	}
	if (ratio > 32768L)
	{
		c = TERM_L_DARK;
		r = "+-VeryRare-+";
	}

	/* Calculate probabilities for each range */
	for (i = 0; i < 22; i++)
	{
		/* Shift the values into view */

		int possibility = 0;
		for (j = i * K_MAX_DEPTH / 22; j < (i + 1) * K_MAX_DEPTH / 22; j++)
			possibility += rarity[j] * (100 * maxt / total[j]);

		possibility = possibility / maxr;

		/* display[i] = log_{sqrt(2)}(possibility) */
		display[i] = 0;
		while (possibility)
		{
			display[i]++;
			possibility = possibility * 1000 / 1414;
		}

		/* Track maximum */
		if (display[i] > maxd) maxd = display[i];
	}

	/* Normalize */
	if (maxd > 10) for (i = 0; i < 22; i++)
	{
		display[i] = display[i] - maxd + 10;
	}

	/* Graph the rarities */
	for (i = 0; i < 22; i++)
	{
		Term_putch(col, row + i + 1, TERM_WHITE,  '|');

		prt(format("%d", (i * K_MAX_DEPTH / 220) % 10), row + i + 1, col);

		if (display[i] <= 0) 
			continue;

		/* Note the level */
		if ((i * K_MAX_DEPTH / 22 <= home) && (home < (i + 1) * K_MAX_DEPTH / 22))
		{
			c_prt(TERM_RED, format("%.*s", display[i], "**********"), row + i + 1, col + 1);
		}
		else
		{
			c_prt(c, format("%.*s", display[i], "**********"), row + i + 1, col + 1);
		}
	}

	/* Make it look nice */
	prt(r, row, col);
}


/*
 * Hack -- Teleport to the target
 */
static void do_cmd_wiz_bamf(void)
{
	/* Must have a target */
	if (!target_who) return;

	/* Teleport to the target */
	teleport_player_to(target_row, target_col, FALSE, FALSE);
}


typedef struct
{
	char desc[80];
	s32b var;
	int  digit;
	s32b lower_limit;
	s32b upper_limit;
	bool changed;
}
change_menu_type;

#define CHANGE_MENU_ROW  4
#define CHANGE_MENU_COL  2

#define VAR_TYPE_S16B 0
#define VAR_TYPE_S32B 1

#define CHANGE_ITEM_STAT_BASE        0
#define CHANGE_ITEM_STAT_MAX         5
#define CHANGE_ITEM_GOLD_NOTE        6
#define CHANGE_ITEM_GOLD_MITHRIL     7
#define CHANGE_ITEM_GOLD_ADAMANTITE  8
#define CHANGE_ITEM_EXP_CLASS        9
#define CHANGE_ITEM_EXP_RACE        10
#define CHANGE_ITEM_LNC             11
#define CHANGE_ITEM_CF_BASE         12
#define CHANGE_ITEM_CF_MAX          16
#define MAX_CHANGE_ITEM             17

static void display_change_menu_line(int cur, int max_desc_len, change_menu_type *cur_item, byte attr)
{
	char buf[80];

	sprintf(buf, "%*s = %-*ld", max_desc_len, cur_item->desc, 11, cur_item->var);
	Term_putstr(CHANGE_MENU_COL, CHANGE_MENU_ROW + cur, -1, attr, buf);
}

static int get_digit(change_menu_type *cur_item)
{
	char lower_buf[80], upper_buf[80];
	int  lower_len, upper_len;

	sprintf(lower_buf, "%ld", cur_item->lower_limit);
	lower_len = strlen(lower_buf);

	sprintf(upper_buf, "%ld", cur_item->upper_limit);
	upper_len = strlen(upper_buf);

	return MAX(lower_len, upper_len);
}

static cptr range_string(change_menu_type *cur_item)
{
	static char buf[80];
	char        long_buf[80];

	buf[0] = '\0';

	sprintf(long_buf, (cur_item->lower_limit < 0) ? "((%ld)-" : "(%ld-", cur_item->lower_limit);
	strcat(buf, long_buf);

	sprintf(long_buf, (cur_item->upper_limit < 0) ? "(%ld))" : "%ld)", cur_item->upper_limit);
	strcat(buf, long_buf);

	return buf;
}

static void construct_change_menu(change_menu_type menu_list[MAX_CHANGE_ITEM], int *max_desc_len)
{
	change_menu_type *cur_item;
	int desc_len, i;

	*max_desc_len = 0;

	C_WIPE(menu_list, MAX_CHANGE_ITEM, change_menu_type);

	for (i = 0; i < A_MAX; i++)
	{
		char tmp_stat_names[10];

		cur_item = &menu_list[CHANGE_ITEM_STAT_BASE + i];
		cur_item->var = p_ptr->stat_max[i];
		cur_item->lower_limit = 3;
		cur_item->upper_limit = STAT_MAX_MAX;
		cur_item->digit = get_digit(cur_item);
		strcpy(tmp_stat_names, stat_names[i]);
		tmp_stat_names[4] = '\0';
		sprintf(cur_item->desc, "%s %s", tmp_stat_names, range_string(cur_item));
		desc_len = strlen(cur_item->desc);
		if (desc_len > *max_desc_len) *max_desc_len = desc_len;
	}

	cur_item = &menu_list[CHANGE_ITEM_GOLD_NOTE];
	cur_item->var = p_ptr->au[SV_GOLD_NOTE];
	cur_item->lower_limit = 0;
	cur_item->upper_limit = PY_MAX_GOLD;
	cur_item->digit = get_digit(cur_item);
	sprintf(cur_item->desc, "Gold (紙幣) %s", range_string(cur_item));
	desc_len = strlen(cur_item->desc);
	if (desc_len > *max_desc_len) *max_desc_len = desc_len;

	cur_item = &menu_list[CHANGE_ITEM_GOLD_MITHRIL];
	cur_item->var = p_ptr->au[SV_GOLD_MITHRIL];
	cur_item->lower_limit = 0;
	cur_item->upper_limit = PY_MAX_GOLD;
	cur_item->digit = get_digit(cur_item);
	sprintf(cur_item->desc, "Gold (ミスリル) %s", range_string(cur_item));
	desc_len = strlen(cur_item->desc);
	if (desc_len > *max_desc_len) *max_desc_len = desc_len;

	cur_item = &menu_list[CHANGE_ITEM_GOLD_ADAMANTITE];
	cur_item->var = p_ptr->au[SV_GOLD_ADAMANTITE];
	cur_item->lower_limit = 0;
	cur_item->upper_limit = PY_MAX_GOLD;
	cur_item->digit = get_digit(cur_item);
	sprintf(cur_item->desc, "Gold (アダマンタイト) %s", range_string(cur_item));
	desc_len = strlen(cur_item->desc);
	if (desc_len > *max_desc_len) *max_desc_len = desc_len;

	cur_item = &menu_list[CHANGE_ITEM_EXP_CLASS];
	cur_item->var = p_ptr->cexp_info[p_ptr->pclass].max_cexp;
	cur_item->lower_limit = 0;
	cur_item->upper_limit = PY_MAX_EXP;
	cur_item->digit = get_digit(cur_item);
	sprintf(cur_item->desc, "クラス経験値 (%s) %s", cp_ptr->title, range_string(cur_item));
	desc_len = strlen(cur_item->desc);
	if (desc_len > *max_desc_len) *max_desc_len = desc_len;

	cur_item = &menu_list[CHANGE_ITEM_EXP_RACE];
	cur_item->var = p_ptr->max_exp;
	cur_item->lower_limit = 0;
	cur_item->upper_limit = PY_MAX_EXP;
	cur_item->digit = get_digit(cur_item);
	sprintf(cur_item->desc, "種族経験値 %s", range_string(cur_item));
	desc_len = strlen(cur_item->desc);
	if (desc_len > *max_desc_len) *max_desc_len = desc_len;

	cur_item = &menu_list[CHANGE_ITEM_LNC];
	cur_item->var = p_ptr->align_self;
	cur_item->lower_limit = -300;
	cur_item->upper_limit = 300;
	cur_item->digit = get_digit(cur_item);
	sprintf(cur_item->desc, "LNCアラインメント %s", range_string(cur_item));
	desc_len = strlen(cur_item->desc);
	if (desc_len > *max_desc_len) *max_desc_len = desc_len;

	for (i = 0; i < ETHNICITY_NUM; i++)
	{
		cur_item = &menu_list[CHANGE_ITEM_CF_BASE + i];
		cur_item->var = chaos_frame[i];
		cur_item->lower_limit = CFRAME_LOWER_LIMIT;
		cur_item->upper_limit = CFRAME_UPPER_LIMIT;
		cur_item->digit = get_digit(cur_item);
		sprintf(cur_item->desc, "カオスフレーム (%s) %s", ethnicity_names[i], range_string(cur_item));
		desc_len = strlen(cur_item->desc);
		if (desc_len > *max_desc_len) *max_desc_len = desc_len;
	}
}

static void do_cmd_wiz_change_commit(change_menu_type menu_list[MAX_CHANGE_ITEM])
{
	change_menu_type *cur_item;
	int i;

	for (i = 0; i < A_MAX; i++)
	{
		cur_item = &menu_list[CHANGE_ITEM_STAT_BASE + i];
		if (cur_item->changed)
		{
			/* Save it */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i] = cur_item->var;
			p_ptr->update |= (PU_BONUS);
			update_stuff();
		}
	}

	cur_item = &menu_list[CHANGE_ITEM_GOLD_NOTE];
	if (cur_item->changed)
	{
		/* Save */
		p_ptr->au[SV_GOLD_NOTE] = cur_item->var;

		/* Update gold */
		p_ptr->update |= (PU_GOLD);
		update_stuff();
	}

	cur_item = &menu_list[CHANGE_ITEM_GOLD_MITHRIL];
	if (cur_item->changed)
	{
		/* Save */
		p_ptr->au[SV_GOLD_MITHRIL] = cur_item->var;

		/* Update gold */
		p_ptr->update |= (PU_GOLD);
		update_stuff();
	}

	cur_item = &menu_list[CHANGE_ITEM_GOLD_ADAMANTITE];
	if (cur_item->changed)
	{
		/* Save */
		p_ptr->au[SV_GOLD_ADAMANTITE] = cur_item->var;

		/* Update gold */
		p_ptr->update |= (PU_GOLD);
		update_stuff();
	}

	cur_item = &menu_list[CHANGE_ITEM_EXP_CLASS];
	if (cur_item->changed)
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

		/* Save */
		cexp_ptr->max_cexp = cur_item->var;
		cexp_ptr->cexp = cur_item->var;

		/* Update */
		check_class_experience();
	}

	cur_item = &menu_list[CHANGE_ITEM_EXP_RACE];
	if (cur_item->changed)
	{
		/* Save */
		p_ptr->max_exp = cur_item->var;
		p_ptr->exp = cur_item->var;

		/* Update */
		check_racial_experience();
	}

	cur_item = &menu_list[CHANGE_ITEM_LNC];
	if (cur_item->changed)
	{
		/* Save it */
		p_ptr->align_self = cur_item->var;
		p_ptr->update |= (PU_BONUS);
		update_stuff();
	}

	for (i = 0; i < ETHNICITY_NUM; i++)
	{
		cur_item = &menu_list[CHANGE_ITEM_CF_BASE + i];
		if (cur_item->changed)
		{
			/* Save it */
			chaos_frame[i] = cur_item->var;
			process_chaos_frame(i);
		}
	}
}

/*
 * Change various "permanent" player variables.
 */
static void do_cmd_wiz_change(void)
{
	char ch;
	char tmp_val[80];
	int  i;

	change_menu_type menu_list[MAX_CHANGE_ITEM];
	int cur, prev;
	change_menu_type *cur_item;

	int max_desc_len;

	/* Save the screen */
	screen_save();

	Term_clear();

	construct_change_menu(menu_list, &max_desc_len);

	prt("Wiz Change Menu - Move to 2/8/j/k, Modify to Enter, Exit to ESC", 2, CHANGE_MENU_COL);

	for (i = 0; i < MAX_CHANGE_ITEM; i++)
	{
		cur_item = &menu_list[i];
		display_change_menu_line(i, max_desc_len, cur_item, TERM_WHITE);
	}

	prev = cur = 0;
	while (1)
	{
		int dir;

		if (prev != cur)
			display_change_menu_line(prev, max_desc_len, &menu_list[prev], TERM_WHITE);

		cur_item = &menu_list[cur];
		display_change_menu_line(cur, max_desc_len, cur_item, TERM_L_BLUE);

		Term_gotoxy(CHANGE_MENU_COL + max_desc_len + 3, CHANGE_MENU_ROW + cur);

		prev = cur;

		ch = inkey();
		dir = get_keymap_dir(ch);
		if ((dir == 2) || (dir == 8)) ch = I2D(dir);

		switch (ch)
		{
		case ESCAPE:
			/* Restore the screen */
			screen_load();

			do_cmd_wiz_change_commit(menu_list);

			/* Redraw everything */
			do_cmd_redraw();
			return;

		case '\n':
		case '\r':
			sprintf(tmp_val, "%-*ld", cur_item->digit, cur_item->var);
			if (get_string("Enter new setting: ", tmp_val, cur_item->digit))
			{
				cur_item->var = atol(tmp_val);
				if (cur_item->var > cur_item->upper_limit) cur_item->var = cur_item->upper_limit;
				else if (cur_item->var < cur_item->lower_limit) cur_item->var = cur_item->lower_limit;
				cur_item->changed = TRUE;
			}
			break;

		case '8':
			for (cur--; !menu_list[cur].desc; cur--)
			{
				if (cur < 0) break;
			}

			if (cur < 0)
			{
				for (cur = MAX_CHANGE_ITEM - 1; !menu_list[cur].desc; cur--) /* Loop */;
			}
			break;

		case '2':
			for (cur++; !menu_list[cur].desc; cur++)
			{
				if (cur >= MAX_CHANGE_ITEM) break;
			}

			if (cur >= MAX_CHANGE_ITEM)
			{
				for (cur = 0; !menu_list[cur].desc; cur++) /* Loop */;
			}
			break;

		default:
			bell();
			break;
		}
	}
}


static void prt_object_bonus(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], int flgs_idx, int row, int col)
{
	int  i;
	byte a;
	char c;

	if (flgs_idx == 0)
	{
		/* Scan the flags */
		for (i = 0; i < A_MAX; i++)
		{
			if (have_flag(flgs, a_to_tr[i]))
			{
				int to_stat = o_ptr->to_stat[i];

				/* Default */
				a = TERM_SLATE;
				c = '*';

				/* Good */
				if (to_stat > 0)
				{
					a = TERM_L_GREEN;
					if (to_stat < 10) c = '0' + to_stat;
				}

				/* Bad */
				else if (to_stat < 0)
				{
					a = TERM_RED;
					if (to_stat > -10) c = '0' - to_stat;
				}

				/* 0 */
				else c = '0';

				Term_putch(col + a_to_tr[i], row, a, c);
			}
		}
	}

	for (i = 0; i < OB_MAX; i++)
	{
		if ((ob_to_tr[i] / 32) != flgs_idx) continue;

		if (have_flag(flgs, ob_to_tr[i]))
		{
			int to_misc = o_ptr->to_misc[i];

			/* Default */
			a = TERM_SLATE;
			c = '*';

			/* Good */
			if (to_misc > 0)
			{
				a = TERM_L_GREEN;
				if (to_misc < 10) c = '0' + to_misc;
			}

			/* Bad */
			else if (to_misc < 0)
			{
				a = TERM_RED;
				if (to_misc > -10) c = '0' - to_misc;
			}

			/* 0 */
			else c = '0';

			Term_putch(col + ob_to_tr[i] % 32, row, a, c);
		}
	}
}

/*
 * Wizard routines for creating objects		-RAK-
 * And for manipulating them!                   -Bernd-
 *
 * This has been rewritten to make the whole procedure
 * of debugging objects much easier and more comfortable.
 *
 * The following functions are meant to play with objects:
 * Create, modify, roll for them (for statistic purposes) and more.
 * The original functions were by RAK.
 * The function to show an item's debug information was written
 * by David Reeve Sward <sward+@CMU.EDU>.
 *                             Bernd (wiebelt@mathematik.hu-berlin.de)
 *
 * Here are the low-level functions
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
 * Just display an item's properties (debug-info)
 * Originally by David Reeve Sward <sward+@CMU.EDU>
 * Verbose item flags by -Bernd-
 */
static void wiz_display_item(object_type *o_ptr)
{
	int i, j = 13;
	u32b flgs[TR_FLAG_SIZE];
	char buf[256];

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	/* Clear the screen */
	for (i = 1; i <= 23; i++) prt("", i, j - 2);

	prt_alloc(o_ptr->tval, o_ptr->sval, 1, 0);

	/* Describe fully */
	object_desc_store(buf, o_ptr, TRUE, 3);

	prt(buf, 2, j);

	prt(format("kind = %-5d  level = %-4d  tval = %-5d  sval = %-5d",
	           o_ptr->k_idx, get_object_level(o_ptr),
	           o_ptr->tval, o_ptr->sval), 4, j);

	prt(format("number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",
	           o_ptr->number, o_ptr->weight,
	           o_ptr->ac, o_ptr->dd, o_ptr->ds), 5, j);

	prt(format("pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",
	           o_ptr->pval, o_ptr->to_a, o_ptr->to_h, o_ptr->to_d), 6, j);

	prt(format("name1 = %-4d  name2 = %-4d  cost = %ld",
	           o_ptr->name1, o_ptr->name2, (long)object_value(o_ptr)), 7, j);

	prt(format("ident = %04x  xtra1 = %-4d  xtra2 = %-4d  timeout = %-d",
	           o_ptr->ident, o_ptr->xtra1, o_ptr->xtra2, o_ptr->timeout), 8, j);

	prt(format("xtra3 = %-4d  xtra4 = %-4d  xtra5 = %-4d  cursed  = %-d",
	           o_ptr->xtra3, o_ptr->xtra4, o_ptr->xtra5, o_ptr->curse_flags), 9, j);

	prt("AFFECT........SLAY........BRAND.", 10, j);
	prt("      mf  i s      d tg  sq     ", 11, j);
	prt("      ao sn p cvae e ri xhupaefc", 12, j);
	prt("siwdccsrsrfdeahanvumooaddaaoclio", 13, j);
	prt("tnieohtctcriettmiinorlnrrrkiierl", 14, j);
	prt("rtsxnarelhagdkcpmldncltggpesdced", 15, j);
	prt_binary(flgs[0], 16, j);

	prt("SUST..  IMM.  RESIST............", 17, j);
	prt("                        b ss scd", 18, j);
	prt("      rgaefctrpsaefcpfldlcohnthi", 19, j);
	prt("siwdccioclioheatcliooeiaiouatoas", 20, j);
	prt("tnieohdoierlrfraierliatrnnnrhnoe", 21, j);
	prt("rtsxnaeddcedwlatdcedsrekdfddresn", 22, j);
	prt_binary(flgs[1], 23, j);

	prt("         u      d              w", 10, j+32);
	prt("fehcnn t n    stirmsiiii d aba r", 11, j+32);
	prt("aauaoomywhsllleegeihgggg rtglmea", 12, j+32);
	prt("uumutmacaohieieleggonnnnaaerefai", 13, j+32);
	prt("rrareanurlovvtiesehtrrrrcilaslst", 14, j+32);
	prt("aanalgarnyweienptntsaefctnevsdyh", 15, j+32);
	prt_binary(flgs[2], 16, j+32);

	prt("  brxrff                        ", 17, j+32);
	prt(" faeseee                        ", 18, j+32);
	prt(" nlshgamm                       ", 19, j+32);
	prt(" admaeraa                       ", 20, j+32);
	prt(" maarnfll                       ", 21, j+32);
	prt(" ergpmdee                       ", 22, j+32);
	prt_binary(flgs[3], 23, j+32);

	prt_object_bonus(o_ptr, flgs, 0, 16, j);
	prt_object_bonus(o_ptr, flgs, 2, 16, j+32);
}


/*
 * A structure to hold a tval and its description
 */
typedef struct tval_desc
{
	int        tval;
	cptr       desc;
} tval_desc;

/*
 * A list of tvals and their textual names
 */
static tval_desc tvals[] =
{
	{ TV_SWORD,             "Sword"               },
	{ TV_POLEARM,           "Polearm"             },
	{ TV_HAFTED,            "Hafted Weapon"       },
	{ TV_BOW,               "Bow"                 },
	{ TV_BULLET,            "Bullet"              },
	{ TV_ROUND,             "Rifle Round"         },
	{ TV_SHELL,             "Shot Shell"          },
	{ TV_ROCKET,            "Rocket"              },
	{ TV_ARROW,             "Arrows"              },
	{ TV_BOLT,              "Bolts"               },
	{ TV_SHIELD,            "Shield"              },
	{ TV_CROWN,             "Crown"               },
	{ TV_HELM,              "Helm"                },
	{ TV_GLOVES,            "Gloves"              },
	{ TV_BOOTS,             "Boots"               },
	{ TV_CLOAK,             "Cloak"               },
	{ TV_HARD_ARMOR,        "Hard Armor"          },
	{ TV_SOFT_ARMOR,        "Soft Armor"          },
	{ TV_RING,              "Ring"                },
	{ TV_AMULET,            "Amulet"              },
	{ TV_LITE,              "Lite"                },
	{ TV_POTION,            "Potion"              },
	{ TV_SCROLL,            "Scroll"              },
	{ TV_WAND,              "Wand"                },
	{ TV_STAFF,             "Staff"               },
	{ TV_ROD,               "Rod"                 },
	{ TV_MAGERY_BOOK,       "Magery Spellbook"    },
	{ TV_FIRE_BOOK,         "Fire Spellbook"      },
	{ TV_AQUA_BOOK,         "Aqua Spellbook"      },
	{ TV_EARTH_BOOK,        "Earth Spellbook"     },
	{ TV_WIND_BOOK,         "Wind Spellbook"      },
	{ TV_HOLY_BOOK,         "Holy Spellbook"      },
	{ TV_DEATH_BOOK,        "Death Spellbook"     },
	{ TV_SYMBIOTIC_BOOK,    "Symbiotic Spellbook" },
	{ TV_WITCH_BOOK,        "Witch Spellbook"     },
	{ TV_DRAKONITE_BOOK,    "Drakonite Spellbook" },
	{ TV_CRUSADE_BOOK,      "Crusade Spellbook"   },
	{ TV_SPIKE,             "Spikes"              },
	{ TV_STONE,             "Stone"               },
	{ TV_DIGGING,           "Digger"              },
	{ TV_CHEST,             "Chest"               },
	{ TV_CARD,              "Express Card"        },
	{ TV_TRUMP,             "Trump"               },
	{ TV_TAROT,             "Tarot Card"          },
	{ TV_SCRATCH_CARD,      "Scratch Card"        },
	{ TV_FIGURINE,          "Magical Figurine"    },
	{ TV_STATUE,            "Statue"              },
	{ TV_CORPSE,            "Corpse"              },
	{ TV_FOOD,              "Food"                },
	{ TV_FLASK,             "Flask"               },
	{ TV_JUNK,              "Junk"                },
	{ TV_SKELETON,          "Skeleton"            },
	{ 0,                    NULL                  }
};


/*
 * Strip an "object name" into a buffer
 */
void strip_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);


	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
#ifdef JP
		if (iskanji(*str)) {*t++ = *str++; *t++ = *str; continue;}
#endif
		if (*str != '~') *t++ = *str;
	}

	/* Terminate the new name */
	*t = '\0';
}


/*
 * Specify tval and sval (type and subtype of object) originally
 * by RAK, heavily modified by -Bernd-
 *
 * This function returns the k_idx of an object type, or zero if failed
 *
 * List up to 50 choices in three columns
 */
static int wiz_create_itemtype(void)
{
	int i, num, max_num;
	int col, row;
	int tval;

	cptr tval_desc;
	char ch;

	int choice[80];

	char buf[160];


	/* Clear screen */
	Term_clear();

	/* Print all tval's and their descriptions */
	for (num = 0; (num < 80) && tvals[num].tval; num++)
	{
		row = 2 + (num % 20);
		col = 20 * (num / 20);
		ch = listsym[num];
		prt(format("[%c] %s", ch, tvals[num].desc), row, col);
	}

	/* Me need to know the maximal possible tval_index */
	max_num = num;

	/* Choose! */
	if (!get_com("Get what type of object? ", &ch, FALSE)) return (0);

	/* Analyze choice */
	for (num = 0; num < max_num; num++)
	{
		if (listsym[num] == ch) break;
	}

	/* Bail out if choice is illegal */
	if ((num < 0) || (num >= max_num)) return (0);

	/* Base object type chosen, fill in tval */
	tval = tvals[num].tval;
	tval_desc = tvals[num].desc;


	/*** And now we go for k_idx ***/

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 80) && (i < max_k_idx); i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Analyze matching items */
		if (k_ptr->tval == tval)
		{
			/* Prepare it */
			row = 2 + (num % 20);
			col = 20 * (num / 20);
			ch = listsym[num];
			strcpy(buf,"                    ");

			/* Acquire the "name" of object "i" */
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
	if (!get_com(format("What Kind of %s? ", tval_desc), &ch, FALSE)) return (0);

	/* Analyze choice */
	for (num = 0; num < max_num; num++)
	{
		if (listsym[num] == ch) break;
	}

	/* Bail out if choice is "illegal" */
	if ((num < 0) || (num >= max_num)) return (0);

	/* And return successful */
	return (choice[num]);
}


/*
 * Tweak an item
 */
typedef struct
{
	cptr desc;
	vptr var;
	int  digit;
}
tweak_menu_type;

#define TWEAK_MENU_ROW  6
#define TWEAK_MENU_COL  2
#define TWEAK_VAR_DIGIT 6

static void display_tweak_menu_line(int cur, int menu_per_col, tweak_menu_type *cur_item, byte attr)
{
	char buf[80];
	int  tmp_int;

	if (cur_item->digit == 6) tmp_int = *((s16b *)cur_item->var);
	else if (cur_item->digit == 3) tmp_int = *((byte *)cur_item->var);
	sprintf(buf, "%s = %-*d", cur_item->desc, TWEAK_VAR_DIGIT, tmp_int);
	Term_putstr(TWEAK_MENU_COL + 17 * (cur / menu_per_col),
	            TWEAK_MENU_ROW + (cur % menu_per_col),
	            -1, attr, buf);
}

static void wiz_tweak_item(object_type *o_ptr)
{
	char ch;
	char tmp_val[80];
	char o_name[MAX_NLEN];
	int  i, tmp_int;

	tweak_menu_type menu_list[] =
	{
		/* Column 0 */
		{ "weight", NULL, 6 },
		{ "    ac", NULL, 6 },
		{ "    dd", NULL, 3 },
		{ "    ds", NULL, 3 },
		{     NULL, NULL, 0 },
		{ "  pval", NULL, 6 },
		{ "  to_h", NULL, 6 },
		{ "  to_d", NULL, 6 },
		{ "  to_a", NULL, 6 },
		{     NULL, NULL, 0 },
		{ " xtra1", NULL, 3 },
		{ " xtra2", NULL, 3 },
		{ " xtra3", NULL, 3 },
		{ " xtra4", NULL, 6 },
		{ " xtra5", NULL, 6 },

		/* Column 1 */
		{ "          to_STR", NULL, 6 },
		{ "          to_INT", NULL, 6 },
		{ "          to_WIS", NULL, 6 },
		{ "          to_DEX", NULL, 6 },
		{ "          to_CON", NULL, 6 },
		{ "          to_CHR", NULL, 6 },
		{               NULL, NULL, 0 },
		{ "to_MAGIC_MASTERY", NULL, 6 },
		{ "      to_STEALTH", NULL, 6 },
		{ "       to_SEARCH", NULL, 6 },
		{ "        to_INFRA", NULL, 6 },
		{ "       to_TUNNEL", NULL, 6 },
		{ "        to_SPEED", NULL, 6 },
		{ "        to_BLOWS", NULL, 6 },
		{ "   to_ANTI_MAGIC", NULL, 6 },
	};

	int menu_num = (sizeof menu_list) / (sizeof (tweak_menu_type));
	int menu_per_col = menu_num / 2;
	int cur, prev;
	tweak_menu_type *cur_item;

	/* Hack -- leave artifacts alone */
	if (artifact_p(o_ptr) || o_ptr->art_name) return;

	/* Column 0 */
	menu_list[0].var = &o_ptr->weight;
	menu_list[1].var = &o_ptr->ac;
	menu_list[2].var = &o_ptr->dd;
	menu_list[3].var = &o_ptr->ds;

	menu_list[5].var = &o_ptr->pval;
	menu_list[6].var = &o_ptr->to_h;
	menu_list[7].var = &o_ptr->to_d;
	menu_list[8].var = &o_ptr->to_a;

	menu_list[10].var = &o_ptr->xtra1;
	menu_list[11].var = &o_ptr->xtra2;
	menu_list[12].var = &o_ptr->xtra3;
	menu_list[13].var = &o_ptr->xtra4;
	menu_list[14].var = &o_ptr->xtra5;

	/* Column 1 */
	for (i = 0; i < A_MAX; i++)
		menu_list[15 + i].var = &o_ptr->to_stat[i];

	for (i = 0; i < OB_MAX; i++)
		menu_list[15 + A_MAX + 1 + i].var = &o_ptr->to_misc[i];

	Term_clear();

	prt("Tweak Menu - Move to 2/4/6/8/h/j/k/l, Modify to Enter, Exit to ESC", 2, TWEAK_MENU_COL);

	/* Describe fully */
	object_desc_store(o_name, o_ptr, TRUE, 3);
	prt(o_name, 4, TWEAK_MENU_COL);

	for (i = 0; i < menu_num; i++)
	{
		cur_item = &menu_list[i];
		if (cur_item->desc)
			display_tweak_menu_line(i, menu_per_col, cur_item, TERM_WHITE);
	}

	prev = cur = 0;
	while (1)
	{
		int dir;

		if (prev != cur)
			display_tweak_menu_line(prev, menu_per_col, &menu_list[prev], TERM_WHITE);

		cur_item = &menu_list[cur];
		display_tweak_menu_line(cur, menu_per_col, cur_item, TERM_L_BLUE);

		Term_gotoxy(TWEAK_MENU_COL + 17 * (cur / menu_per_col) + strlen(cur_item->desc) + 3,
		            TWEAK_MENU_ROW + (cur % menu_per_col));

		prev = cur;

		ch = inkey();
		dir = get_keymap_dir(ch);
		if ((dir == 2) || (dir == 4) || (dir == 6) || (dir == 8)) ch = I2D(dir);

		switch (ch)
		{
		case ESCAPE:
			wiz_display_item(o_ptr);
			return;

		case '\n':
		case '\r':
			if (cur_item->digit == 6) tmp_int = *((s16b *)cur_item->var);
			else if (cur_item->digit == 3) tmp_int = *((byte *)cur_item->var);
			sprintf(tmp_val, "%-*d", cur_item->digit, tmp_int);
			if (get_string("Enter new setting: ", tmp_val, cur_item->digit))
			{
				tmp_int = atoi(tmp_val);
				if (cur_item->digit == 6) *((s16b *)cur_item->var) = (s16b)tmp_int;
				else if (cur_item->digit == 3) *((byte *)cur_item->var) = (byte)tmp_int;
			}

			/* Describe fully */
			object_desc_store(o_name, o_ptr, TRUE, 3);
			prt(o_name, 4, 2);
			break;

		case '8':
			for (cur--; !menu_list[cur].desc; cur--)
			{
				if (cur < 0) break;
			}

			if (cur < 0)
			{
				for (cur = menu_num - 1; !menu_list[cur].desc; cur--) /* Loop */;
			}
			break;

		case '2':
			for (cur++; !menu_list[cur].desc; cur++)
			{
				if (cur >= menu_num) break;
			}

			if (cur >= menu_num)
			{
				for (cur = 0; !menu_list[cur].desc; cur++) /* Loop */;
			}
			break;

		case '6':
			cur += menu_per_col;
			if (cur >= menu_num) cur -= menu_num;
			if (!menu_list[cur].desc) cur = prev;
			break;

		case '4':
			cur -= menu_per_col;
			if (cur < 0) cur += menu_num;
			if (!menu_list[cur].desc) cur = prev;
			break;

		default:
			bell();
			break;
		}
	}
}


/*
 * Apply magic to an item or turn it into an artifact. -Bernd-
 */
static void wiz_reroll_item(object_type *o_ptr)
{
	object_type forge;
	object_type *q_ptr;

	char ch;

	bool changed = FALSE;


	/* Hack -- leave artifacts alone */
	if (artifact_p(o_ptr) || o_ptr->art_name) return;


	/* Get local object */
	q_ptr = &forge;

	/* Copy the object */
	object_copy(q_ptr, o_ptr);


	/* Main loop. Ask for magification and artifactification */
	while (TRUE)
	{
		/* Display full item debug information */
		wiz_display_item(q_ptr);

		/* Ask wizard what to do. */
		if (!get_com("[a]ccept, [w]orthless, [c]ursed, [n]ormal, [g]ood, [e]xcellent, [s]pecial? ", &ch, FALSE))
		{
			/* Preserve wizard-generated artifacts */
			if (artifact_p(q_ptr))
			{
				a_info[q_ptr->name1].cur_num = 0;
				q_ptr->name1 = 0;
			}

			changed = FALSE;
			break;
		}

		/* Create/change it! */
		if (ch == 'A' || ch == 'a')
		{
			changed = TRUE;
			break;
		}

		/* Preserve wizard-generated artifacts */
		if (artifact_p(q_ptr))
		{
			a_info[q_ptr->name1].cur_num = 0;
			q_ptr->name1 = 0;
		}

		switch(ch)
		{
			/* Apply bad magic, but first clear object */
			case 'w': case 'W':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AMF_GOOD | AMF_GREAT | AMF_CURSED);
				break;
			}
			/* Apply bad magic, but first clear object */
			case 'c': case 'C':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AMF_GOOD | AMF_CURSED);
				break;
			}
			/* Apply normal magic, but first clear object */
			case 'n': case 'N':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, 0L);
				break;
			}
			/* Apply good magic, but first clear object */
			case 'g': case 'G':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AMF_GOOD);
				break;
			}
			/* Apply great magic, but first clear object */
			case 'e': case 'E':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AMF_GOOD | AMF_GREAT);
				break;
			}
			case 's': case 'S':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AMF_OKAY | AMF_GOOD | AMF_GREAT | AMF_SPECIAL);

				/* Failed to create normal artifact; make a random one */
				if (!artifact_p(q_ptr) && !q_ptr->art_name) create_artifact(q_ptr, FALSE);
				break;
			}
		}
		q_ptr->iy = o_ptr->iy;
		q_ptr->ix = o_ptr->ix;
		q_ptr->next_o_idx = o_ptr->next_o_idx;
		q_ptr->marked = o_ptr->marked;
	}


	/* Notice change */
	if (changed)
	{
		/* Apply changes */
		object_copy(o_ptr, q_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	}
}



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
	u32b i, matches, better, worse, other, correct;

	u32b test_roll = 1000000;

	char ch;
	cptr quality;

	u32b am_flags;

	object_type forge;
	object_type	*q_ptr;

	cptr q = "Rolls: %ld  Correct: %ld  Matches: %ld  Better: %ld  Worse: %ld  Other: %ld";

	cptr p = "Enter number of items to roll: ";
	char tmp_val[80];

	bool bonus_match, bonus_better, bonus_worse;
	int k;


	/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
	if (artifact_p(o_ptr)) a_info[o_ptr->name1].cur_num = 0;


	/* Interact */
	while (TRUE)
	{
		cptr pmt = "Roll for [n]ormal, [g]ood, or [e]xcellent treasure? ";

		/* Display item */
		wiz_display_item(o_ptr);

		/* Get choices */
		if (!get_com(pmt, &ch, FALSE)) break;

		if (ch == 'n' || ch == 'N')
		{
			am_flags = AMF_OKAY;
			quality = "normal";
		}
		else if (ch == 'g' || ch == 'G')
		{
			am_flags = (AMF_OKAY | AMF_GOOD);
			quality = "good";
		}
		else if (ch == 'e' || ch == 'E')
		{
			am_flags = (AMF_OKAY | AMF_GOOD | AMF_GREAT);
			quality = "excellent";
		}
		else
		{
			am_flags = AMF_OKAY;
			break;
		}

		sprintf(tmp_val, "%ld", test_roll);
		if (get_string(p, tmp_val, 10)) test_roll = atol(tmp_val);
		test_roll = MAX(1, test_roll);

		/* Let us know what we are doing */
		msg_format("Creating a lot of %s items. Base level = %d.",
					  quality, dun_level);
		msg_print(NULL);

		/* Set counters to zero */
		correct = matches = better = worse = other = 0;

		/* Let's rock and roll */
		for (i = 0; i <= test_roll; i++)
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
				prt(format(q, i, correct, matches, better, worse, other), 0, 0);
				Term_fresh();
			}


			/* Get local object */
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Create an object */
			make_object(q_ptr, am_flags);


			/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
			if (artifact_p(q_ptr)) a_info[q_ptr->name1].cur_num = 0;


			/* Test for the same tval and sval. */
			if ((o_ptr->tval) != (q_ptr->tval)) continue;
			if ((o_ptr->sval) != (q_ptr->sval)) continue;

			/* One more correct item */
			correct++;

			/* Check bonuses */
			bonus_match = bonus_better = bonus_worse = TRUE;
			for (k = 0; k < A_MAX; k++)
			{
				if (q_ptr->to_stat[k] != o_ptr->to_stat[k]) bonus_match = FALSE;
				if (q_ptr->to_stat[k] < o_ptr->to_stat[k]) bonus_better = FALSE;
				if (q_ptr->to_stat[k] > o_ptr->to_stat[k]) bonus_worse = FALSE;
			}
			for (k = 0; k < OB_MAX; k++)
			{
				if (q_ptr->to_misc[k] != o_ptr->to_misc[k]) bonus_match = FALSE;
				if (q_ptr->to_misc[k] < o_ptr->to_misc[k]) bonus_better = FALSE;
				if (q_ptr->to_misc[k] > o_ptr->to_misc[k]) bonus_worse = FALSE;
			}

			/* Check for match */
			if (bonus_match &&
				 (q_ptr->pval == o_ptr->pval) &&
				 (q_ptr->to_a == o_ptr->to_a) &&
				 (q_ptr->to_h == o_ptr->to_h) &&
				 (q_ptr->to_d == o_ptr->to_d) &&
				 (q_ptr->name1 == o_ptr->name1))
			{
				matches++;
			}

			/* Check for better */
			else if (bonus_better &&
						(q_ptr->pval >= o_ptr->pval) &&
						(q_ptr->to_a >= o_ptr->to_a) &&
						(q_ptr->to_h >= o_ptr->to_h) &&
						(q_ptr->to_d >= o_ptr->to_d))
			{
				better++;
			}

			/* Check for worse */
			else if (bonus_worse &&
						(q_ptr->pval <= o_ptr->pval) &&
						(q_ptr->to_a <= o_ptr->to_a) &&
						(q_ptr->to_h <= o_ptr->to_h) &&
						(q_ptr->to_d <= o_ptr->to_d))
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
		msg_format(q, i, correct, matches, better, worse, other);
		msg_print(NULL);
	}


	/* Hack -- Normally only make a single artifact */
	if (artifact_p(o_ptr)) a_info[o_ptr->name1].cur_num = 1;
}


/*
 * Change the quantity of a the item
 */
static void wiz_quantity_item(object_type *o_ptr)
{
	int         tmp_int, tmp_qnt;

	char        tmp_val[100];


	/* Never duplicate artifacts */
	if (artifact_p(o_ptr) || o_ptr->art_name) return;

	/* Store old quantity. -LM- */
	tmp_qnt = o_ptr->number;

	/* Default */
	sprintf(tmp_val, "%d", o_ptr->number);

	/* Query */
	if (get_string("Quantity: ", tmp_val, 2))
	{
		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Paranoia */
		if (tmp_int < 1) tmp_int = 1;
		if (tmp_int > 99) tmp_int = 99;

		/* Accept modifications */
		o_ptr->number = tmp_int;
	}

	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval = o_ptr->pval * o_ptr->number / tmp_qnt;
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

	object_type	forge;
	object_type *q_ptr;

	object_type *o_ptr;

	char ch;

	bool changed;

	cptr q, s;

	item_tester_no_ryoute = TRUE;
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

	/* The item was not changed */
	changed = FALSE;

	/* Save the screen */
	screen_save();


	/* Get local object */
	q_ptr = &forge;

	/* Copy object */
	object_copy(q_ptr, o_ptr);

	if (item == INVEN_RARM)
	{
		if (mw_old_weight)
		{
			q_ptr->weight = mw_old_weight;
		}
		if (mw_diff_to_melee)
		{
			q_ptr->to_h -= mw_diff_to_melee;
			q_ptr->to_d -= mw_diff_to_melee;
		}
	}


	/* The main loop */
	while (TRUE)
	{
		/* Display the item */
		wiz_display_item(q_ptr);

		/* Get choice */
		if (!get_com("[a]ccept [s]tatistics [r]eroll [t]weak [q]uantity? ", &ch, FALSE))
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
			wiz_statistics(q_ptr);
		}

		if (ch == 'r' || ch == 'r')
		{
			wiz_reroll_item(q_ptr);
		}

		if (ch == 't' || ch == 'T')
		{
			wiz_tweak_item(q_ptr);
		}

		if (ch == 'q' || ch == 'Q')
		{
			wiz_quantity_item(q_ptr);
		}
	}


	/* Restore the screen */
	screen_load();


	/* Accept change */
	if (changed)
	{
		/* Message */
		msg_print("Changes accepted.");

		/* Recalcurate object's weight */
		if (item >= 0)
		{
			p_ptr->total_weight += (q_ptr->weight * q_ptr->number)
				- (o_ptr->weight * o_ptr->number);
		}

		/* Change */
		object_copy(o_ptr, q_ptr);

		if (item == INVEN_RARM)
		{
			if (mw_old_weight)
			{
				mw_old_weight = o_ptr->weight;
				p_ptr->total_weight -= (o_ptr->weight - 1) * o_ptr->number;
				o_ptr->weight = 1;
			}
			if (mw_diff_to_melee)
			{
				o_ptr->to_h += mw_diff_to_melee;
				o_ptr->to_d += mw_diff_to_melee;
			}
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	}

	/* Ignore change */
	else
	{
		msg_print("Changes ignored.");
	}
}


/*
 * Wizard routine for creating objects		-RAK-
 * Heavily modified to allow magification and artifactification  -Bernd-
 *
 * Note that wizards cannot create objects on top of other objects.
 *
 * Hack -- this routine always makes a "dungeon object", and applies
 * magic to it, and attempts to decline cursed items.
 */
static void wiz_create_item(void)
{
	object_type	forge;
	object_type *q_ptr;

	int k_idx;


	/* Save the screen */
	screen_save();

	/* Get object base type */
	k_idx = wiz_create_itemtype();

	/* Restore the screen */
	screen_load();


	/* Return if failed */
	if (!k_idx) return;

	if (k_info[k_idx].gen_flags & TRG_INSTA_ART)
	{
		int i;

		/* Artifactify */
		for (i = 1; i < max_a_idx; i++)
		{
			/* Ignore incorrect tval */
			if (a_info[i].tval != k_info[k_idx].tval) continue;

			/* Ignore incorrect sval */
			if (a_info[i].sval != k_info[k_idx].sval) continue;

			/* Create this artifact */
			create_named_art(i, py, px);

			/* All done */
			msg_print("Allocated(INSTA_ART).");

			return;
		}
	}

	/* Get local object */
	q_ptr = &forge;

	/* Create the item */
	object_prep(q_ptr, k_idx);

	/* Apply magic */
	apply_magic(q_ptr, dun_level, 0L);

	/* Drop the object from heaven */
	(void)drop_near(q_ptr, -1, py, px);

	/* All done */
	msg_print("Allocated.");
}


/*
 * Cure everything instantly
 */
static void do_cmd_wiz_cure_all(void)
{
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
	if (p_ptr->csp < p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	/* Cure stuff */
	(void)set_blind(0);
	(void)set_confused(0);
	(void)set_poisoned(0);
	(void)set_afraid(0);
	(void)set_paralyzed(0);
	(void)set_image(0);
	(void)set_stun(0);
	(void)set_cut(0);
	(void)set_slow(0, TRUE);
	(void)set_stoning(0);
	(void)set_opposite_pelem(0);
	(void)set_no_elem(0);

	/* No longer hungry */
	if (p_ptr->no_digest) p_ptr->food = PY_FOOD_FULL - 1;
	else (void)set_food(PY_FOOD_MAX - 1);

	/* Redraw everything */
	do_cmd_redraw();
}


/*
 * Go to any level
 */
static void do_cmd_wiz_jump(void)
{
	/* Ask for level */
	if (command_arg <= 0)
	{
		char	ppp[80];

		char	tmp_val[160];
		int		tmp_dungeon_type;

		/* Prompt */
		sprintf(ppp, "Jump which dungeon : ");

		/* Default */
		sprintf(tmp_val, "%d", dungeon_type);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 2)) return;

		tmp_dungeon_type = atoi(tmp_val);
		if (!d_info[tmp_dungeon_type].maxdepth || (tmp_dungeon_type > max_d_idx)) tmp_dungeon_type = DUNGEON_PALACE;

		/* Prompt */
		sprintf(ppp, "Jump to level (0, %d-%d): ", d_info[tmp_dungeon_type].mindepth, d_info[tmp_dungeon_type].maxdepth);

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		command_arg = atoi(tmp_val);

		dungeon_type = tmp_dungeon_type;
	}

	/* Paranoia */
	if (command_arg < d_info[dungeon_type].mindepth) command_arg = 0;

	/* Paranoia */
	if (command_arg > d_info[dungeon_type].maxdepth) command_arg = d_info[dungeon_type].maxdepth;

	/* Accept request */
	msg_format("You jump to dungeon level %d.", command_arg);

	if (autosave_l) do_cmd_save_game(TRUE);

	/* Change level */
	dun_level = command_arg;

	if (!IN_HEAVEN_GATE()) prepare_change_floor_mode(CFM_RAND_PLACE);

	if (!dun_level) dungeon_type = 0;
	p_ptr->inside_arena = FALSE;
	p_ptr->wild_mode = FALSE;

	leave_quest_check();

	if (record_stair) do_cmd_write_nikki(NIKKI_WIZ_TELE,0,NULL);

	p_ptr->inside_quest = 0;
	energy_use = 0;

	/* Prevent energy_need from being too lower than 0 */
	p_ptr->energy_need = 0;

	/*
	 * Clear all saved floors
	 * and create a first saved floor
	 */
	prepare_change_floor_mode(CFM_FIRST_FLOOR);

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Become aware of a lot of objects
 */
static void do_cmd_wiz_learn(void)
{
	int i;

	object_type forge;
	object_type *q_ptr;

	/* Scan every object */
	for (i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Induce awareness */
		if (k_ptr->level <= command_arg)
		{
			/* Get local object */
			q_ptr = &forge;

			/* Prepare object */
			object_prep(q_ptr, i);

			/* Awareness */
			object_aware(q_ptr);
		}
	}
}


/*
 * Summon some creatures
 */
static void do_cmd_wiz_summon(int num)
{
	int i;

	for (i = 0; i < num; i++)
	{
		(void)summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_IGNORE_AMGRID));
	}
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named(int r_idx)
{
	int i, x, y;

	/* Paranoia */
	/* if (!r_idx) return; */

	/* Prevent illegal monsters */
	if ((r_idx >= max_r_idx) && !monster_is_runeweapon(r_idx)) return;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, py, px, d, 0);

		/* Require empty grids */
		if (!cave_empty_bold(y, x)) continue;

		/* Place it (allow groups) */
		if (place_monster_aux(0, y, x, r_idx, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP | PM_IGNORE_AMGRID))) break;
	}
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named_friendly(int r_idx)
{
	(void) summon_named_creature(0, py, px, r_idx, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP | PM_FORCE_PET | PM_IGNORE_AMGRID));
}



/*
 * Hack -- Delete all nearby monsters
 */
static void do_cmd_wiz_zap(void)
{
	int i;


	/* Genocide everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Delete nearby monsters */
		if (m_ptr->cdis <= MAX_SIGHT)
		{
			if (i == p_ptr->riding)
			{
				rakuba(-1, FALSE);
				p_ptr->redraw |= (PR_EXTRA);
			}
			delete_monster_idx(i);
		}
	}
}


/*
 * Hack -- Delete all monsters
 */
static void do_cmd_wiz_zap_all(void)
{
	int i;

	/* Genocide everyone */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		if (i == p_ptr->riding)
		{
			rakuba(-1, FALSE);
			p_ptr->redraw |= (PR_EXTRA);
		}

		/* Delete this monster */
		delete_monster_idx(i);
	}
}

static void do_cmd_wiz_tele_town(void)
{
	int i, x, y;
	int num = 1;

	if (astral_mode || dun_level || p_ptr->inside_arena)
	{
#ifdef JP
		msg_print("このコマンドは現在は使えない！");
#else
		msg_print("This command cannot be used now!");
#endif
		return;
	}

	screen_save();
	for (i = 4; i <= 14; i++)
		prt("", i, 0);

	for (i=1;i<max_towns;i++)
	{
		char buf[80];

		if (i == p_ptr->town_num) continue;

		sprintf(buf,"%c) %-20s", I2A(i-1), town[i].name);
		prt(buf, 5+i, 5);
		num++;
	}

#ifdef JP
	prt("どこに行きますか:", 0, 0);
#else
	prt("Which town you go: ", 0, 0);
#endif
	while(1)
	{
		i = inkey();

		if (i == ESCAPE)
		{
			screen_load();
			return;
		}
		else if ((i < 'a') || (i > ('a'+max_towns-2))) continue;
		else if (((i-'a'+1) == p_ptr->town_num)) continue;
		break;
	}

	for (y = 0; y < max_wild_y; y++)
	{
		for (x = 0; x < max_wild_x; x++)
		{
			if(wilderness[y][x].town == (i-'a'+1))
			{
				p_ptr->wilderness_y = y;
				p_ptr->wilderness_x = x;
			}
		}
	}
	p_ptr->leaving = TRUE;
	p_ptr->teleport_town = TRUE;
	screen_load();
	return;
}


static void do_cmd_wiz_snap_dragon_info(void)
{
	char        tmp_val[80] = "1";

#ifdef JP
	msg_format("現在はスナップドラゴン武器は%d本存在します。", runeweapon_num);
#else
	msg_format("Current Snap Dragon weapon number is %d.", runeweapon_num);
#endif

	/* Query */
#ifdef JP
	if (get_string(format("スナップドラゴン武器のID (1-%d, 1が最新): ", MAX_RUNEWEAPON), tmp_val, 3))
#else
	if (get_string(format("Snap Dragon weapon ID (1-%d, 1 is latest): ", MAX_RUNEWEAPON), tmp_val, 3))
#endif
	{
		int             tmp_int;
		runeweapon_type *runeweapon;
		object_type     *o_ptr;
		monster_race    *r_ptr;
		char            o_name[MAX_NLEN];
		char            buf[160];

		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Paranoia */
		if (tmp_int < 1) tmp_int = 1;
		if (tmp_int > MAX_RUNEWEAPON) tmp_int = MAX_RUNEWEAPON;

		runeweapon = &runeweapon_list[tmp_int];
		o_ptr = &runeweapon->weapon;
		r_ptr = &r_info[runeweapon_r_idx_from(tmp_int)];

		if (!o_ptr->k_idx)
		{
#ifdef JP
			msg_format("ID: %dには武器はありません。", tmp_int);
#else
			msg_format("There is no Runeweapon (ID: %d).", tmp_int);
#endif
			return;
		}

#ifdef JP
		sprintf(buf, "(ID: %d, 先祖名: %s", tmp_int,
			runeweapon->ancestor[0] ? runeweapon->ancestor : "(ヴァレリアの覇者jnk)");
		if (runeweapon->status & RW_STATUS_FOUND) strcat(buf, ", 発見済み");
		else if (r_ptr->max_num == 0) strcat(buf, ", 戦闘勝利");
		else strcat(buf, ", 未発見");
		if (runeweapon->status & RW_STATUS_ILLEGAL) strcat(buf, ", 不正");
#else
		sprintf(buf, "(ID: %d, Ancestor: %s", tmp_int,
			runeweapon->ancestor[0] ? runeweapon->ancestor : "(jnk the Champion of Valeria)");
		if (runeweapon->status & RW_STATUS_FOUND) strcat(buf, ", Found");
		else if (r_ptr->max_num == 0) strcat(buf, ", Beaten");
		else strcat(buf, ", Not Found");
		if (runeweapon->status & RW_STATUS_ILLEGAL) strcat(buf, ", Illegal");
#endif
		strcat(buf, ")");
		msg_print(buf);

		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3);
#ifdef JP
		msg_format("%sを調べている...", o_name);
#else
		msg_format("Examining %s...", o_name);
#endif
#ifdef JP
		if (!screen_object(o_ptr, NULL, TRUE)) msg_print("特に変わったところはないようだ。");
#else
		if (!screen_object(o_ptr, NULL, TRUE)) msg_print("You see nothing special.");
#endif

		if ((runeweapon->status & RW_STATUS_ILLEGAL) && !p_ptr->wizard)
		{
#ifdef JP
			msg_print("不正な魔武器はウィザードモードでなければ設定できません。");
#else
			msg_print("You cannot configure illegal runeweapons unless wizard mode.");
#endif
			return;
		}

#ifdef JP
		if (get_check("この武器を生成しますか？"))
#else
		if (get_check("Really generate this weapon? "))
#endif
		{
			object_type forge;
			object_type *q_ptr;

			/* Get local object */
			q_ptr = &forge;

			object_copy(q_ptr, o_ptr);

			/* Drop the object from heaven */
			(void)drop_near(q_ptr, -1, py, px);
		}

		if (!astral_mode || (tmp_int != 1))
		{
#ifdef JP
			if (get_check("この武器の状態を変えますか？"))
#else
			if (get_check("Set status of this weapon?"))
#endif
			{
				char ch;

#ifdef JP
				get_com("[N]未発見, [B]戦闘勝利, [F]発見済み :", &ch, TRUE);
#else
				get_com("[N]one, [B]eaten, [F]ound :", &ch, TRUE);
#endif

				switch (ch)
				{
				case 'N':
				case 'n':
					runeweapon->status &= ~(RW_STATUS_FOUND);
					r_ptr->max_num = 1;
					break;

				case 'B':
				case 'b':
					runeweapon->status &= ~(RW_STATUS_FOUND);
					r_ptr->max_num = 0;
					break;

				case 'F':
				case 'f':
					runeweapon->status |= (RW_STATUS_FOUND);
					r_ptr->max_num = 0;
					break;
				}

				if (r_ptr->max_num == 0) remove_runeweapon(tmp_int);
			}
		}
		else
		{
#ifdef JP
			msg_print("この武器は先祖の肉体として構成されるため、状態は変えられません。");
#else
			msg_print("You cannot change status of this weapon because this is used as the Body of Ancestor.");
#endif
		}

		if (astral_mode && (tmp_int != 1))
		{
#ifdef JP
			msg_print("この魔武器モンスターはアストラルモードでは出現しません。");
#else
			msg_print("This Runeweapon do not appear in astral mode.");
#endif
			return;
		}

#ifdef JP
		if (get_check("この武器から構成されるモンスターを見ますか？"))
#else
		if (get_check("Really see the monster constructed from weapon? "))
#endif
		{
			/* Save the screen */
			screen_save();

			/* Recall on screen */
			screen_roff(runeweapon_r_idx_from(tmp_int), 0x01);

			/* Hack -- Auto-recall */
			monster_race_track(runeweapon_r_idx_from(tmp_int));

			/* Hack -- Handle stuff */
			handle_stuff();

			inkey();

			/* Restore */
			screen_load();
		}
	}
}


#ifdef ALLOW_SPOILERS

/*
 * External function
 */
extern void do_cmd_spoilers(void);

#endif /* ALLOW_SPOILERS */



/*
 * Hack -- declare external function
 */
extern void do_cmd_debug(void);



/*
 * Ask for and parse a "debug command"
 * The "command_arg" may have been set.
 */
void do_cmd_debug(void)
{
	int     x, y;
	char    cmd;


	/* Get a "debug command" */
	get_com("Debug Command: ", &cmd, FALSE);

	/* Analyze the command */
	switch (cmd)
	{
		/* Nothing */
		case ESCAPE:
		case ' ':
		case '\n':
		case '\r':
			break;


#ifdef ALLOW_SPOILERS

		/* Hack -- Generate Spoilers */
		case '"':
			do_cmd_spoilers();
			break;

#endif /* ALLOW_SPOILERS */


		/* Hack -- Help */
		case '?':
			do_cmd_help();
			break;


		/* Cure all maladies */
		case 'a':
			do_cmd_wiz_cure_all();
			break;

		/* Teleport to target */
		case 'b':
			do_cmd_wiz_bamf();
			break;

		/* Create any object */
		case 'c':
			wiz_create_item();
			break;

		/* Create a named artifact */
		case 'C':
			wiz_create_named_art(command_arg);
			break;

		/* Detect everything */
		case 'd':
			detect_all(DETECT_RAD_ALL*3);
			break;

		/* Dimension_door */
		case 'D':
			wiz_dimension_door();
			break;

		/* Edit character */
		case 'e':
			do_cmd_wiz_change();
			break;

		/* View item info */
		case 'f':
			identify_fully(FALSE);
			break;

		/* Good Objects */
		case 'g':
			if (command_arg <= 0) command_arg = 1;
			acquirement(py, px, command_arg, FALSE, TRUE);
			break;

#ifdef MONSTER_HORDES
		case 'H':
			do_cmd_summon_horde(); break;
#endif /* MONSTER_HORDES */

		/* Identify */
		case 'i':
			(void)ident_spell(FALSE);
			break;

		/* Go up or down in the dungeon */
		case 'j':
			do_cmd_wiz_jump();
			break;

		/* Self-Knowledge */
		case 'k':
			self_knowledge();
			break;

		/* Learn about objects */
		case 'l':
			do_cmd_wiz_learn();
			break;

		/* Magic Mapping */
		case 'm':
			map_area(DETECT_RAD_ALL);
			break;

		/* Mutation */
		case 'M':
			(void)gain_random_mutation(command_arg, TRUE);
			break;

		/* Specific reward */
		case 'r':
			(void)gain_level_reward(command_arg);
			break;

		/* Summon _friendly_ named monster */
		case 'N':
			do_cmd_wiz_named_friendly(command_arg);
			break;

		/* Summon Named Monster */
		case 'n':
			do_cmd_wiz_named(command_arg);
			break;

		/* Object playing routines */
		case 'o':
			do_cmd_wiz_play();
			break;

		/* Phase Door */
		case 'p':
			teleport_player(10);
			break;

#if 0
		/* Complete a Quest -KMW- */
		case 'q':
		{
			for (i = 0; i < max_quests; i++)
			{
				if (p_ptr->quest[i].status == QUEST_STATUS_TAKEN)
				{
					p_ptr->quest[i].status++;
					msg_print("Completed Quest");
					msg_print(NULL);
					break;
				}
			}
			if (i == max_quests)
			{
				msg_print("No current quest");
				msg_print(NULL);
			}
			break;
		}
#endif

		/* Make every dungeon square "known" to test streamers -KMW- */
		case 'u':
		{
			for(y = 0; y < cur_hgt; y++)
			{
				for(x = 0; x < cur_wid; x++)
				{
					cave[y][x].info |= (CAVE_GLOW | CAVE_MARK);
				}
			}
			wiz_lite(FALSE);
			break;
		}

		/* Summon Random Monster(s) */
		case 's':
			if (command_arg <= 0) command_arg = 1;
			do_cmd_wiz_summon(command_arg);
			break;

		/* "Snap Dragon" information */
		case 'S':
			do_cmd_wiz_snap_dragon_info();
			break;

		/* Teleport */
		case 't':
			teleport_player(100);
			break;


		/* Teleport Town */
		case 'T':
			do_cmd_wiz_tele_town();
			break;

		/* Very Good Objects */
		case 'v':
			if (command_arg <= 0) command_arg = 1;
			acquirement(py, px, command_arg, TRUE, TRUE);
			break;

		/* Wizard Light the Level */
		case 'w':
		wiz_lite((bool)(p_ptr->pclass == CLASS_NINJA));
		break;

		/* Wish */
		case 'W':
		wish_object(NULL);
		break;

		/* Increase Experience */
		case 'x':
		if (command_arg)
		{
			gain_exp(command_arg);
		}
		else
		{
			gain_class_exp(p_ptr->cexp_info[p_ptr->pclass].cexp + 1);
			gain_racial_exp(p_ptr->exp + 1);
		}
		break;

/* このままではつかえないので後で修正予定
		case 'y':
#ifdef JP
			msg_format("修正なしHP: %d+%d、修正なしMP: %d+%d。",
#else
			msg_format("Pure HP: %d+%d, Pure Mana: %d+%d.",
#endif
			            p_ptr->race_hp[p_ptr->lev - 1], p_ptr->player_ghp,
		    	        p_ptr->race_sp[p_ptr->lev - 1], p_ptr->player_gsp);
			break;
*/

		/* Zap Monsters (Genocide) */
		case 'z':
		do_cmd_wiz_zap();
		break;

		case 'Z':
		do_cmd_wiz_zap_all();
		break;

		/* Hack -- whatever I desire */
		case '_':
		do_cmd_wiz_hack_ben();
		break;

		/* Mundane object */
		case ':':
		mundane_spell(FALSE);
		break;

		/*  */
		case ';':
		ego_creation_scroll();
		break;

		/* Not a Wizard Command */
		default:
		msg_print("That is not a valid debug command.");
		break;
	}
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif

