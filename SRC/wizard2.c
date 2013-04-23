/* File: wizard2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



#ifdef ALLOW_WIZARD


/*
 * Hack -- quick debugging hook
 */
static void do_cmd_wiz_hack_ben(void)
{
   probing();

	/* Oops */
	msg_print("Oops.");
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
	if (!p_ptr->target_who) return;

	/* Teleport to the target */
	teleport_player_to(p_ptr->target_row, p_ptr->target_col);
}



/*
 * Aux function for "do_cmd_wiz_change()"
 */
static void do_cmd_wiz_change_aux(void)
{
	int i;

	int tmp_int;

	long tmp_long;

	char cmd;

	/* Query the stats */
	for (i = 0; i < 6; i++)
	{
		/* Prompt */
		prt(format("%s (3-118):", stat_names[i]), 0, 0);

		/* Query */
		tmp_int = get_number(p_ptr->stat_max[i], 18+100, 0, 15, &cmd);

		if (cmd == ESCAPE) return;

		/* Verify */
		if (tmp_int < 1) tmp_int = 1;

		/* Save it */
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = tmp_int;
	}

	/* Query */
	prt("Gold:", 0, 0);
	tmp_long = get_number(p_ptr->au, 999999999L, 0, 6, &cmd);

	if (cmd == ESCAPE) return;

	/* Save */
	p_ptr->au = tmp_long;

	/* Query */
	prt("Experience:", 0, 0);
	tmp_long = get_number(p_ptr->max_exp, 99999999L, 0, 12, &cmd);

	if (cmd == ESCAPE) return;

	/* Save */
	p_ptr->exp = p_ptr->max_exp = tmp_long;

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

	byte level;

	char buf[256];


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Clear screen */
	Term_clear();

	/* Describe fully */
	object_desc_store(buf, o_ptr, TRUE, 3);
	prt(buf, 2, j);

	if (!artifact_p(o_ptr)) level = k_info[o_ptr->k_idx].level;
	else level = a_info[o_ptr->name1].level;

	prt(format("kind = %-5d  level = %-4d  tval = %-5d  sval = %-5d",
				  o_ptr->k_idx, level, o_ptr->tval, o_ptr->sval), 4, j);

	prt(format("number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",
				  o_ptr->number, o_ptr->wt,
				  o_ptr->ac, o_ptr->dd, o_ptr->ds), 5, j);

	prt(format("pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",
				  o_ptr->pval, o_ptr->to_a, o_ptr->to_h, o_ptr->to_d), 6, j);

	prt(format("name1 = %-4d  name2 = %-4d  cost = %ld",
				  o_ptr->name1, o_ptr->name2, (long)object_value(o_ptr)), 7, j);

	prt(format("ident = %04x  timeout = %-d",
				  o_ptr->ident, o_ptr->timeout), 8, j);

	prt("+------------FLAGS1------------+", 10, j);
	prt("AFFECT..........SLAY......BRAND.", 11, j);
	prt("                ae      x aefc  ", 12, j);
	prt("siwdcc  ssidsasmnvudotgdd clio  ", 13, j);
	prt("tnieoh  trnipthgiinmrrnrr ierl  ", 14, j);
	prt("rtsxna..lcfgdkttmldncltgg.dced..", 15, j);
	prt_binary(f1, 16, j);

	prt("+------------FLAGS2------------+", 17, j);
	prt("SUST....IMMUN...RESIST..........", 18, j);
	prt("        aefc    aefcpfldbc s n  ", 19, j);
	prt("siwdcc  clio    cliooeialoshntcd", 20, j);
	prt("tnieoh  ierl    ierliatrnnnrehhi", 21, j);
	prt("rtsxna..dced....dcedsrekdfddxrss", 22, j);
	prt_binary(f2, 23, j);

	prt("+------------FLAGS3------------+", 10, j+32);
	prt("....ts.h.....tadiiii...aiehs..hp", 11, j+32);
	prt(" f  eefo     egrgggg  bcnaih  vr", 12, j+32);
	prt("de  lerl    ilrannnn  ltssdo cym", 13, j+32);
	prt("ia reieddh  meairrrr  eityew ucc", 14, j+32);
	prt("gtlepnelee  ppvnaefc  svaktm ruu", 15, j+32);
	prt("ehigavaica  aoaeclio  saanyo srr", 16, j+32);
	prt("seteticfav  crtxierl  etropd ess", 17, j+32);
	prt("trenhsteyy..ttepdced..detwes.dee", 18, j+32);
	prt_binary(f3, 19, j+32);
}


/*
 * A structure to hold a tval and its description
 */
typedef struct tval_desc
{
	int tval;
	cptr desc;
} tval_desc;

/*
 * A list of tvals and their textual names
 */
static tval_desc tvals[] =
{
	{ TV_SKELETON,				"Skeleton"				  },
	{ TV_BOTTLE,				"Bottle"					  },
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
	{ TV_HARD_ARMOR,        "Hard Armor"           },
	{ TV_SOFT_ARMOR,        "Soft Armor"           },
	{ TV_RING,              "Ring"                 },
	{ TV_AMULET,            "Amulet"               },
	{ TV_LITE,              "Lite"                 },
	{ TV_POTION,            "Potion"               },
	{ TV_SCROLL,            "Scroll"               },
	{ TV_WAND,              "Wand"                 },
	{ TV_STAFF,             "Staff"                },
	{ TV_ROD,               "Rod"                  },
	{ TV_PRAYER_BOOK,       "Priest Book"          },
	{ TV_MAGIC_BOOK,        "Magic Book"           },
	{ TV_SPIKE,             "Spikes"               },
	{ TV_DIGGING,           "Digger"               },
	{ TV_CHEST,             "Chest"                },
	{ TV_FOOD,              "Food"                 },
	{ TV_FLASK,             "Flask"                },
	{ 0,                    NULL                   }
};


/*
 * Strip an "object name" into a buffer
 */
static void strip_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);


	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
		if (*str != '~') *t++ = *str;
	}

	/* Terminate the new name */
	*t = '\0';
}

/*
 * Acquire an object kind for creation (or zero)
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

	int choice[60];

	char buf[160];


	/* Clear screen */
	Term_clear();

	/* Print all tval's and their descriptions */
	for (num = 0; (num < 60) && tvals[num].tval; num++)
	{
		row = 2 + (num % 20);
		col = 30 * (num / 20);
		ch = listsym[num];
		prt(format("[%c] %s", ch, tvals[num].desc), row, col);
	}

	/* Me need to know the maximal possible tval_index */
	max_num = num;

	/* Choose! */
	if (!get_com("Get what type of object? ", &ch)) return (0);

	/* Analyze choice */
	num = list_choice(ch);

	/* Bail out if choice is illegal */
	if ((num < 0) || (num >= max_num)) return (0);

	/* Base object type chosen, fill in tval */
	tval = tvals[num].tval;
	tval_desc = tvals[num].desc;


	/*** And now we go for k_idx ***/

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 60) && (i < MAX_K_IDX); i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Analyze matching items */
		if (k_ptr->tval == tval)
		{
			/* Prepare it */
			row = 2 + (num % 20);
			col = 30 * (num / 20);
			ch = listsym[num];

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
	if (!get_com(format("What Kind of %s? ", tval_desc), &ch)) return (0);

	/* Analyze choice */
	num = list_choice(ch);

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
	char cmd;

	/* Hack -- leave artifacts alone */
	if (artifact_p(o_ptr)) return;

	prt("Enter new 'pval' setting:", 0, 0);
	o_ptr->pval = get_number(o_ptr->pval, 99999, 0, 26, &cmd);

	if (cmd == ESCAPE) return;

	wiz_display_item(o_ptr);

	prt("Enter new 'to_a' setting:", 0, 0);
	o_ptr->to_a = get_number(o_ptr->to_a, 99999, 0, 26, &cmd);

	if (cmd == ESCAPE) return;

	wiz_display_item(o_ptr);

	prt("Enter new 'to_h' setting:", 0, 0);
	o_ptr->to_h = get_number(o_ptr->to_h, 99999, 0, 26, &cmd);

	if (cmd == ESCAPE) return;

	wiz_display_item(o_ptr);

	prt("Enter new 'to_d' setting:", 0, 0);
	o_ptr->to_d = get_number(o_ptr->to_d, 99999, 0, 26, &cmd);

	if (cmd == ESCAPE) return;

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


	/* Hack -- leave artifacts and corpses alone */
	if (artifact_p(o_ptr) || (o_ptr->tval == TV_SKELETON)) return;


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
		if (!get_com("[a]ccept, [n]ormal, [g]ood, [e]xcellent, [s]tupendous? ", &ch))
		{
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
		if (artifact_p(i_ptr))
		{
			a_info[i_ptr->name1].cur_num = 0;
			i_ptr->name1 = 0;

			compact_artifacts();
		}

		/* Apply normal magic, but first clear object */
		if (ch == 'N' || ch == 'n')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE);
		}

		/* Apply good magic, but first clear object */
		else if (ch == 'G' || ch == 'g')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, FALSE, TRUE, FALSE);
		}

		/* Apply great magic, but first clear object */
		else if (ch == 'E' || ch == 'e')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, FALSE, TRUE, TRUE);
		}

		/* Apply special magic, but first clear object */
		else if (ch == 'S' || ch == 's')
		{
			object_prep(i_ptr, o_ptr->k_idx);
			apply_magic(i_ptr, p_ptr->depth, TRUE, TRUE, TRUE);
		}
	}


	/* Notice change */
	if (changed)
	{
		/* Apply changes */
		object_copy(o_ptr, i_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
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


	/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
	if (artifact_p(o_ptr)) a_info[o_ptr->name1].cur_num = 0;


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
			make_object(i_ptr, good, great);


			/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
			if (artifact_p(i_ptr)) a_info[i_ptr->name1].cur_num = 0;


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
	if (artifact_p(o_ptr)) a_info[o_ptr->name1].cur_num = 1;
}


/*
 * Change the quantity of a the item
 */
static void wiz_quantity_item(object_type *o_ptr)
{
	int tmp_int;

	char cmd;


	/* Never duplicate artifacts */
	if (artifact_p(o_ptr)) return;

	prt("Quantity:", 0, 0);

	/* Query */
	tmp_int = get_number(o_ptr->number, 99, 0, 10, &cmd);

	if (cmd != ESCAPE)
	{
		/* Paranoia */
		if (tmp_int < 1) tmp_int = 1;
		if (tmp_int > 99) tmp_int = 99;

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

	bool changed;


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


	/* Icky */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();


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
			wiz_quantity_item(i_ptr);
		}
	}


	/* Restore the screen */
	Term_load();

	/* Not Icky */
	character_icky = FALSE;


	/* Accept change */
	if (changed)
	{
		/* Message */
		msg_print("Changes accepted.");

		/* Change */
		object_copy(o_ptr, i_ptr);

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


	/* Icky */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Get object base type */
	k_idx = wiz_create_itemtype();

	/* Restore the screen */
	Term_load();

	/* Not Icky */
	character_icky = FALSE;


	/* Return if failed */
	if (!k_idx) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create the item */
	object_prep(i_ptr, k_idx);

	/* Skeletons are a little special */
	if (i_ptr->tval == TV_SKELETON)
	{
		bool normal = FALSE;

		switch(i_ptr->sval)
		{
			case SV_SKULL:
			{
				i_ptr->wt = p_ptr->wt / 6;
				break;
			}
			case SV_HEAD:
			{
				i_ptr->wt = p_ptr->wt / 3;
				i_ptr->pval = i_ptr->wt + rand_int(i_ptr->wt);
				break;
			}
			case SV_SKELETON:
			{
				i_ptr->wt = p_ptr->wt * 10 / 4;
         	break;
         }
      	case SV_CORPSE:
         {
         	i_ptr->wt = p_ptr->wt * 10;
            i_ptr->pval = i_ptr->wt + rand_int(i_ptr->wt);
         	break;
         }
         case SV_RAW_MEAT:
         {
            i_ptr->pval = 100 + rand_int(100);
				break;
			}
			default:
			{
				normal = TRUE;
				break;
			}
		}

		if (!normal)
		{
			/* The player is unique, although its corpse might not be */
			object_aware(i_ptr);
			i_ptr->name1 = 1;
		}
	}

	if (k_info[k_idx].flags3 & TR3_INSTA_ART)
	{
		int i;

		/* Artifactify */
		for (i = 1; i < ART_MIN_NORMAL; i++)
		{
			if (a_info[i].sval == i_ptr->sval) i_ptr->name1 = i;
		}

		/* Apply magic */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);
	}
	else
	{
		/* Apply magic */
		apply_magic(i_ptr, p_ptr->depth, FALSE, FALSE, FALSE);
	}

	/* Drop the object from heaven */
	drop_near(i_ptr, -1, py, px);

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
	p_ptr->csp = p_ptr->msp;
	p_ptr->csp_frac = 0;

	/* Cure stuff */
	(void)set_blind(0);
	(void)set_confused(0);
	(void)set_poisoned(0);
	(void)set_afraid(0);
	(void)set_paralyzed(0);
	(void)set_image(0);
	(void)set_stun(0);
	(void)set_cut(0);
	(void)set_slow(0);

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
	int tmp_int;

	char cmd;

	/* Ask for level */
	if (p_ptr->command_arg <= 0)
	{
		/* Prompt */
		prt(format("Jump to level (0-%d):", MAX_DEPTH-1), 0, 0);

		/* Ask for a level */
		tmp_int = get_number(p_ptr->depth, MAX_DEPTH - 1, 0, 23, &cmd);

		if (cmd == ESCAPE)
		{
			prt("", 0, 0);
			return;
		}

		p_ptr->command_arg = tmp_int;
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

	/* Random quests are over */
	p_ptr->quest_max = 0;

	/* Repair the quest monster */
	r_info[p_ptr->quest_idx].flags1 &= ~(RF1_QUESTOR);
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
	for (i = 1; i < MAX_K_IDX; i++)
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
	while (1)
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
	p_ptr->window |= (PW_SPELL | PW_PLAYER);

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
		(void)summon_specific(py, px, p_ptr->depth, 0);
	}
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named(int r_idx, int slp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, x, y;

	/* Paranoia */
	if (!r_idx) return;
	if (r_idx >= MAX_R_IDX-1) return;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, py, px, d, 0);

		/* Require empty grids */
		if (!cave_empty_bold(y, x)) continue;

		/* Place it (allow groups) */
		if (place_monster_aux(y, x, r_idx, slp, TRUE)) break;
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
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > d) continue;

		/* Check for quest completion */
		if (r_ptr->flags1 & (RF1_QUESTOR)) check_quest(r_ptr, m_ptr, FALSE);

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



#ifdef ALLOW_SPOILERS

/*
 * External function
 */
extern void do_cmd_spoilers(void);

#endif



/*
 * Hack -- declare external function
 */
extern void do_cmd_debug(void);



/*
 * Ask for and parse a "debug command"
 * The "p_ptr->command_arg" may have been set.
 */
void do_cmd_debug(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	char cmd;


	/* Get a "debug command" */
	(void)(get_com("Debug Command: ", &cmd));

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
			acquirement(py, px, p_ptr->command_arg, FALSE);
			break;
		}

		/* Hitpoint rerating */
		case 'h':
		{
			do_cmd_rerate(); break;
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
			self_knowledge();
			break;
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
			map_area();
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
			teleport_player(10);
			break;
		}

		/* Remove curses */
		case 'r':
		{
			(void)remove_all_curse();
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
			acquirement(py, px, p_ptr->command_arg, TRUE);
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

