#define WIZARD2_C
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

/*
 * Hack -- Rerate Hitpoints
 */
void do_cmd_rerate(void)
{
	int i;
	int lastroll,j;

	/* Pre-calculate level 1 hitdice */
	player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */

	/* 'Roll' the hitpoint values */
	lastroll = p_ptr->hitdie;
	for (i = 1; i < 100; i++)
	{
		player_hp[i]=lastroll;
		lastroll--;
		if(lastroll<1) lastroll = p_ptr->hitdie;
	}
	/* Now shuffle them */
	for(i=1;i<100;i++)
	{
		j=randint(99);
		lastroll=player_hp[i];
		player_hp[i]=player_hp[j];
		player_hp[j]=lastroll;
	}
	/* Make each a cumulative score */
	for(i=1;i<100;i++)
	{
	player_hp[i] = player_hp[i-1] +player_hp[i];
	}

	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Handle stuff */
	handle_stuff();
}



#ifdef ALLOW_WIZARD

/*
 * Run a suitable function, return the screen to its appearance before the
 * function was run (if possible) and return the function's return code.
 */
static int choose_on_screen(int (*aux)(void))
{
	/* Save the screen */
	int i, t = Term_save_aux();

	/* Icky */
	character_icky = TRUE;

	/* Run the auxiliary function, remember its return. */
	i = (*aux)();

	/* Not Icky */
	character_icky = FALSE;

	/* Restore the screen */
	Term_load_aux(t);

	/* Forget the saved copy. */
	Term_release(t);

	/* Return the remembered value. */
	return i;
}
  

/*
 * Hack -- quick debugging hook
 */
void do_cmd_wiz_hack_ben(void)
{
	msg_print("No 'Wizard Hack' command coded.");
}



#ifdef MONSTER_HORDES
/* Summon a horde of monsters */
void do_cmd_summon_horde(void)
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
#endif

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


/*
 * Hack -- Teleport to the target
 */
void do_cmd_wiz_bamf(void)
{
	/* Must have a target */
	if (!target_who) return;

	/* Teleport to the target */
	teleport_player_to(target_row, target_col);
}

/*
 * Sorting (comparison) hook for skill furthest to the right, and then the
 * bottom.
 */
static bool ang_sort_comp_skills(vptr u, vptr UNUSED v, int a, int b)
{
	player_skill **s = (player_skill**)u;

	if (s[a]->x != s[b]->x) return (s[a]->x < s[b]->x);
	else return (s[a]->y <= s[b]->y);
}

/*
 * Swap two pointers to player_skills around.
 */
static void ang_sort_swap_skills(vptr u, vptr UNUSED v, int a, int b)
{
	player_skill **s = (player_skill**)u, *st;
	st = s[a];
	s[a] = s[b];
	s[b] = st;
}

/*
 * Aux function for "do_cmd_wiz_change()".	-RAK-
 */
static void do_cmd_wiz_change_aux(void)
{
	int			i,win;

	int			tmp_int;

	long		tmp_long;

	char		tmp_val[160];

	char		ppp[80];

	player_skill *skills[MAX_SKILLS];

	/* Query the stats */
	for (i = 0; i < 6; i++)
	{
		/* Prompt */
		sprintf(ppp, "%s (3-118): ", stat_names[i]);

		/* Default */
		sprintf(tmp_val, "%d", p_ptr->stat_max[i]);

		/* Query */
		if (!get_string(ppp, tmp_val, 3)) return;

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
	if (!get_string("Gold: ", tmp_val, 9)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->au = tmp_long;


	/* Initialise. */
	for (i = 0; i < MAX_SKILLS; i++)
	{
		skills[i] = skill_set+i;
	}

	/* Sort. */
	ang_sort_swap = ang_sort_swap_skills;
	ang_sort_comp = ang_sort_comp_skills;
	ang_sort(skills, 0, MAX_SKILLS);

	win = Term_save_aux();

	for(i=0;i<MAX_SKILLS;i++)
	{
		player_skill *sk_ptr = skills[i];

		/* Hack - pretend we're on the surface to avoid skills which are
		 * normally yellow. See skill_colour() for details.
		 */
		const s16b real_dun_level = dun_level;
		dun_level = 0;

		/* Display the current skill table. */
		display_player(2);

		dun_level = real_dun_level;

		/* Place the cursor by the skill in question.
		 * Hack - the 17 is from display_player_skills_aux().
		 */
		move_cursor(sk_ptr->y, sk_ptr->x+17);

		/* Default */
		sprintf(tmp_val, "%d%%", (int)(skills[i]->max_value));

		/* Query */
		if (!askfor_aux(tmp_val, 3)) return;

		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Verify */
		if (tmp_int < 0) tmp_int = 0L;
		if (tmp_int > 100) tmp_int = 100L;

		/* Save */
		skills[i]->max_value = (byte)tmp_int;
		skills[i]->value = (byte)tmp_int;

		/* Window stuff. */
		p_ptr->window |= PW_PLAYER_SKILLS;
	}

	Term_load_aux(win);
	Term_release(win);
}

/*
 * Change various "permanent" player variables.
 */
void do_cmd_wiz_change(void)
{
	/* Interact */
	do_cmd_wiz_change_aux();

	/* Redraw everything */
	do_cmd_redraw();
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
 *     specify tval (category) and object
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
 *     same type (k_idx), then we compare pval and +AC.
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
	int 	i, j = 13;

	u32b	f1, f2, f3;


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Clear the screen */
	for (i = 1; i <= 23; i++) prt("", i, j - 2);

	mc_put_fmt(2, j, "%v", object_desc_f3, o_ptr, OD_ART | OD_SHOP, 3);

	mc_put_fmt(4, j, "kind = %-5d  tval = %-5d  extra = %-5d",
	           o_ptr->k_idx, o_ptr->tval, k_info[o_ptr->k_idx].extra);

	mc_put_fmt(5, j, "number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",
	           o_ptr->number, o_ptr->weight, o_ptr->ac, o_ptr->dd, o_ptr->ds);

	mc_put_fmt(6, j, "pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",
	           o_ptr->pval, o_ptr->to_a, o_ptr->to_h, o_ptr->to_d);

	mc_put_fmt(7, j, "name1 = %-4d  name2 = %-4d  cost = %ld",
	           o_ptr->name1, o_ptr->name2, (long)object_value(o_ptr, TRUE));

	mc_put_fmt(8, j, "ident = %04x  timeout = %-d",
		o_ptr->ident, o_ptr->timeout);

	prt("+------------FLAGS1------------+", 10, j);
    prt("AFFECT........SLAY........BRAND.", 11, j);
    prt("              cvae      xsqpaefc", 12, j);
    prt("siwdcc  ssidsahanvudotgddhuoclio", 13, j);
    prt("tnieoh  trnipttmiinmrrnrrraiierl", 14, j);
    prt("rtsxna..lcfgdkcpmldncltggpksdced", 15, j);
	prt_binary(f1, 16, j);

	prt("+------------FLAGS2------------+", 17, j);
	prt("SUST....IMMUN.RESIST............", 18, j);
    prt("        aefcprpsaefcpfldbc sn   ", 19, j);
    prt("siwdcc  cliooeatcliooeialoshtncd", 20, j);
    prt("tnieoh  ierlifraierliatrnnnrhehi", 21, j);
    prt("rtsxna..dcedslatdcedsrekdfddrxss", 22, j);
	prt_binary(f2, 23, j);

	prt("+------------FLAGS3------------+", 10, j+32);
    prt("fe      ehsi  st    iiiiadta  hp", 11, j+32);
    prt("il   n taihnf ee    ggggcregb vr", 12, j+32);
    prt("re  nowysdose eld   nnnntalrl ym", 13, j+32);
    prt("ec  omrcyewta ieirmsrrrriieaeccc", 14, j+32);
    prt("aa  taauktmatlnpgeihaefcvnpvsuuu", 15, j+32);
    prt("uu  egirnyoahivaeggoclioaeoasrrr", 16, j+32);
    prt("rr  litsopdretitsehtierltxrtesss", 17, j+32);
    prt("aa  echewestreshtntsdcedeptedeee", 18, j+32);
	prt_binary(f3, 19, j+32);
}

/*
 * A list of tvals and their textual names
 */
static name_centry tval_names[] =
{
	{TV_SWORD,	"Sword"},
	{TV_POLEARM,	"Pole-arm"},
	{TV_HAFTED,	"Hafted Weapon"},
	{TV_BOW,	"Bow"},
	{TV_ARROW,	"Arrows"},
	{TV_BOLT,	"Bolts"},
	{TV_SHOT,	"Shots"},
	{TV_SHIELD,	"Shield"},
	{TV_CROWN,	"Crown"},
	{TV_HELM,	"Helm"},
	{TV_GLOVES,	"Gloves"},
	{TV_BOOTS,	"Boots"},
	{TV_CLOAK,	"Cloak"},
	{TV_DRAG_ARMOR,	"Dragon Scale Mail"},
	{TV_HARD_ARMOR,	"Hard Armour"},
	{TV_SOFT_ARMOR,	"Soft Armour"},
	{TV_RING,	"Ring"},
	{TV_AMULET,	"Amulet"},
	{TV_LITE,	"Lite"},
	{TV_POTION,	"Potion"},
	{TV_SCROLL,	"Scroll"},
	{TV_WAND,	"Wand"},
	{TV_STAFF,	"Staff"},
	{TV_ROD,	"Rod"},
	{TV_SORCERY_BOOK,	"Sorcery Book"},
	{TV_THAUMATURGY_BOOK,	"Thaumaturgy Book"},
	{TV_CONJURATION_BOOK,	"Conjuration Book"},
	{TV_NECROMANCY_BOOK,	"Necromancy Book"},
	{TV_CHARM,	"Charm"},
	{TV_SPIKE,	"Spike"},
	{TV_DIGGING,	"Digger"},
	{TV_CHEST,	"Chest"},
	{TV_FOOD,	"Food"},
	{TV_FLASK,	"Flask"},
	{0, NULL}
};

/*
 * Return whether k_info[i] can be generated and is part of the specified
 * category.
 */
static bool good_cat_object(int a, name_centry *cat)
{
	/* Hack -- skip items which only have special generation methods. */
	if (!kind_created_p(k_info+a)) return FALSE;

	/* Simply check the tval. */
	return (k_info[a].tval == cat->idx);
}

/*
 * Specify tval (category) and object. Originally
 * by RAK, heavily modified by -Bernd-
 *
 * This function returns the k_idx of an object type, or zero if failed
 *
 * List up to 50 choices in three columns
 */
static int wiz_create_itemtype(void)
{
	bool (*item_good)(int, name_centry *) = good_cat_object;
	void (*print_f1)(char *, uint, cptr, va_list *) = object_k_name_f1;

	int i, num;
	int col, row;
	uint len;
	name_centry *cat;

	cptr s;
	char ch;

	int	choice[60];

	/* A list of the valid options for this prompt. */
	cptr body =	option_chars;
	char sym[61], buf[80];

	C_TNEW(bufx, 61*ONAME_MAX, char);
	char *obuf[61];
	bool abort;

	WIPE(sym, sym);

	/* Choose a category until a valid response is given. */
	do
	{
		cat = choose_item_category(item_good, &abort, tval_names,
			"Select a category:", FALSE);
	}
	while (!cat && !abort);

	if (abort) return 0;

	/*** And now we go for k_idx ***/

	/* Turn obuf[] into a 2 dimensional array. */
	for (i = 0; i < 61; i++) obuf[i] = bufx+i*ONAME_MAX;

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 60) && (i < MAX_K_IDX); i++)
	{
		if (item_good(i, cat)) choice[num++] = i;
	}

	len = 80/((num+19)/20);

	/* The name of each option will be stored and trimmed to fit. */
	for (i = 0; i <= 60; i++) obuf[i] = bufx+i*ONAME_MAX;

	/* Get names for each valid object which can all fit on screen at once. */
	get_names(obuf, obuf[60], num, choice, len, print_f1);

	/* Print everything */
	for (i = 0; i < num; i++)
	{
		row = 2 + (i % 20);
		col = len * (i / 20);
		ch = body[i];
		
		/* Print it */
		mc_put_fmt(row, col, "$![%c] %.*^s", ch, len-4, obuf[i]);
	}

	/* Finished with the obuf[] array. */
	TFREE(bufx);

	/* Choose! */
	sprintf(buf, "What kind of %.60s? ", cat->str);
	if (!get_com(buf, &ch)) return (0);

	/* Analyze choice */
	s = strchr(body, ch);

	/* Bail out if choice is not recognised. */
	if (!s || s >= body+num) return 0;

	/* And return successful */
	return (choice[s - body]);
}

/*
 * Display a two-part prompt to select an item.
 * It uses a name_entry to describe the entries in the first part, which
 * should start at start.
 * type is the description used in the first prompt (e.g. "object").
 * max is the number of valid items.
 * If sym_from_cat is set, the int in each name_entry is used for the symbol
 * at the first prompt (the second never does so).
 * Otherwise, the ones in the body array are used.
 * item_good returns whether an item is something which can be generated and
 * belongs to the specified category.
 * print_f1 is a vstrnfmt_aux style function which copies the name of the item
 * specified by number into buf.
 */
static int choose_something(name_centry *start, cptr type, int max,
	bool sym_from_cat, bool (*item_good)(int, name_centry *),
	void (*print_f1)(char *, uint, cptr, va_list *))
{
	int i, num;
	int col, row;
	uint len;
	name_centry *cat;

	cptr s;
	char ch;

	int	choice[60];

	/* A list of the valid options for this prompt. */
	cptr body =	option_chars;
	char sym[61], buf[80];
	WIPE(sym, sym);

	/* Clear screen */
	Term_clear();

	/* Print all tval's and their descriptions */
	for (num = 0, cat = start; (num < 60) && cat->str; cat++)
	{
		for (i = 0; i < max; i++)
		{
			if ((*item_good)(i, cat)) break;
		}

		/* No good options exist in this category. */
		if (i == max) continue;

		row = 2 + (num % 20);
		col = 30 * (num / 20);
		if (sym_from_cat) sym[num] = cat->idx;
		else sym[num] = body[num];

		choice[num] = cat-start;
		mc_put_fmt(row, col, "$![%c] %.25s", sym[num++], cat->str);
	}

	/* Choose! */
	sprintf(buf, "Get what type of %.60s? ", type);
	if (!get_com(buf, &ch)) return (0);

	/* Analyze choice */
	s = strchr(sym, ch);

	/* Bail out if choice is not recognised. */
	if (!s) return (0);

	/* Base object type chosen, fill in tval */
	cat = &start[choice[s - sym]];


	/*** And now we go for a_idx ***/

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 60) && (i < max); i++)
	{
		/* Remember the object index */
		if (item_good(i, cat)) choice[num++] = i;
	}

	/* Paranoia - empty categories shouldn't have reached the list. */
	if (!num) return 0;

	len = 80/((num+19)/20);

	/* Print everything */
	for (i = 0; i < num; i++)
	{
		row = 2 + (i % 20);
		col = len * (i / 20);
		ch = body[i];
		
		/* Print it */
		mc_put_fmt(row, col, "$![%c] %.*^v", ch, len-4, print_f1, choice[i]);
	}

	/* Choose! */
	sprintf(buf, "What kind of %.60s? ", cat->str);
	if (!get_com(buf, &ch)) return (0);

	/* Analyze choice */
	s = strchr(body, ch);

	/* Bail out if choice is not recognised. */
	if (!s || s >= body+num) return 0;

	/* And return successful */
	return (choice[s - body]);
}

/*
 * Return whether a_info[i] can be generated and is part of the specified
 * category.
 */
static bool good_cat_artefact(int a, name_centry *cat)
{
	const artifact_type *a_ptr = a_info+a;

	/* Not a real artefact. */
	if (!a_ptr->name) return FALSE;

	/* Wrong symbol. */
	if (k_info[a_ptr->k_idx].tval != cat->idx) return FALSE;

	return TRUE;
}

/*
 * Return the name of an artefact specified by a_idx in buf.
 */
static void artefact_name_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int n = va_arg(*vp, int);
	object_type q_ptr[1];

	if (!make_fake_artifact(q_ptr, n)) return;

	strnfmt(buf, max, "%v", object_desc_f3, q_ptr, OD_SHOP, 0);
}

/*
 * Select an artefact from a list.
 */
static int choose_artefact(void)
{
	return choose_something(tval_names, "artifact", MAX_A_IDX,
		FALSE, good_cat_artefact, artefact_name_f1);
}

/*
 * Return whether r_info[i] can be generated and is part of the specified
 * category.
 */
static bool good_cat_monster(int r, name_centry *cat)
{
	const monster_race *r_ptr = &r_info[r];

	/* Not a valid monster. */
	if (is_fake_monster(r_ptr)) return FALSE;

	/* Not of this category. */
	if (r_ptr->d_char != cat->idx) return FALSE;

	return TRUE;
}

/*
 * Return the name of a monster specified by r_idx in buf.
 */
static void monster_name_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int n = va_arg(*vp, int);
	strnfmt(buf, max, "%v", monster_desc_aux_f3, r_info+n, 1, 0);
}

/*
 * Select a race of monster to create based first on the character used,
 * then on the race itself.
 *
 * Hack - this function relies on there being no more than 60 symbols which
 * are used to represent monsters, or 60 monsters with a symbol. 
 */
static int choose_monster_type(void)
{
	return choose_something(ident_info, "monster", MAX_R_IDX,
		FALSE, good_cat_monster, monster_name_f1);
}

/*
 * Create the artifact of the specified number -- DAN
 */
void wiz_create_named_art(int a_idx)
{
	object_type q_ptr[1];
	int i;

	artifact_type *a_ptr;

	/* Give a selection if needed. */
	if (!a_idx || !a_info[a_idx].name)
		a_idx = choose_on_screen(choose_artefact);

	/* Give up if nothing valid is selected. */
	if (a_idx < 0 || a_idx >= MAX_A_IDX || !a_info[a_idx].name) return;

	a_ptr = &a_info[a_idx];

	/* Wipe the object */
	object_wipe(q_ptr);

	/* Acquire the "kind" index */
	i = a_ptr->k_idx;

	/* Oops */
	if (i <= 0 || i >= MAX_K_IDX) return;

	/* Create the artifact */
	object_prep(q_ptr, i);

	/* Save the name */
	q_ptr->name1 = a_idx;

	apply_magic_2(q_ptr, dun_depth);

	/* Drop the artifact from heaven */
	drop_near(q_ptr, -1, py, px);

	/* All done */
	msg_print("Allocated.");
}



/*
 * Tweak an item
 */
static void wiz_tweak_item(object_type *o_ptr)
{
	cptr	p;
	char        tmp_val[80];


	/* Hack -- leave artifacts alone */
    if (allart_p(o_ptr)) return;

	p = "Enter new 'pval' setting: ";
	sprintf(tmp_val, "%d", o_ptr->pval);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->pval = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_a' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_a);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->to_a = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_h' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_h);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->to_h = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_d' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_d);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->to_d = atoi(tmp_val);
	wiz_display_item(o_ptr);
}


/*
 * Change an item fundamentally
 */
static void wiz_change_item(object_type *o_ptr)
{
	cptr	p;
	char        tmp_val[80];

	/* Hack -- leave artifacts alone */
	if (allart_p(o_ptr)) return;

	p = "Enter new 'k_idx' setting: ";
	sprintf(tmp_val, "%d", o_ptr->k_idx);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->k_idx = atoi(tmp_val);
	wiz_display_item(o_ptr);
	wiz_display_item(o_ptr);
	
	/* There's no easy way to detect impossible ego items, but restricting
	it to weapons and non-dragon armour is simple. */
	switch (o_ptr->tval)
	{
		case TV_SHOT: case TV_ARROW: case TV_BOLT: case TV_BOW:
		case TV_DIGGING: case TV_HAFTED: case TV_POLEARM: case TV_SWORD:
		case TV_BOOTS: case TV_GLOVES: case TV_HELM: case TV_CROWN:
		case TV_SHIELD: case TV_CLOAK: case TV_SOFT_ARMOR: case TV_HARD_ARMOR:
		break;
		default:
		return;
	}
	
	p = "Enter new 'ego' number: ";
	sprintf(tmp_val, "%d", o_ptr->name2);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->name2 = atoi(tmp_val);
	wiz_display_item(o_ptr);
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
    if (allart_p(o_ptr)) return;


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
		if (!get_com("[a]ccept, [n]ormal, [g]ood, [e]xcellent? ", &ch))
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

		/* Apply normal magic, but first clear object */
		else if (ch == 'n' || ch == 'N')
		{
			object_prep(q_ptr, o_ptr->k_idx);
			apply_magic(q_ptr, (dun_depth), FALSE, FALSE, FALSE);
		}

		/* Apply good magic, but first clear object */
		else if (ch == 'g' || ch == 'g')
		{
			object_prep(q_ptr, o_ptr->k_idx);
			apply_magic(q_ptr, (dun_depth), FALSE, TRUE, FALSE);
		}

		/* Apply great magic, but first clear object */
		else if (ch == 'e' || ch == 'e')
		{
			object_prep(q_ptr, o_ptr->k_idx);
			apply_magic(q_ptr, (dun_depth), FALSE, TRUE, TRUE);
		}
	}


	/* Notice change */
	if (changed)
	{
		/* Apply changes */
		object_copy(o_ptr, q_ptr);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr, 0);
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

	object_type forge;
	object_type	*q_ptr;
	
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
		           quality, (dun_depth));
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
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Create an object */
			make_object(q_ptr, good, great);


			/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
			if (artifact_p(q_ptr)) a_info[q_ptr->name1].cur_num = 0;


			/* Test for the same k_idx. */
			if ((o_ptr->k_idx) != (q_ptr->k_idx)) continue;

			/* Check for match */
			if ((q_ptr->pval == o_ptr->pval) &&
			    (q_ptr->to_a == o_ptr->to_a) &&
			    (q_ptr->to_h == o_ptr->to_h) &&
			    (q_ptr->to_d == o_ptr->to_d))
			{
				matches++;
			}

			/* Check for better */
			else if ((q_ptr->pval >= o_ptr->pval) &&
			         (q_ptr->to_a >= o_ptr->to_a) &&
			         (q_ptr->to_h >= o_ptr->to_h) &&
			         (q_ptr->to_d >= o_ptr->to_d))
			{
				better++;
			}

			/* Check for worse */
			else if ((q_ptr->pval <= o_ptr->pval) &&
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
	int         tmp_int;

	char        tmp_val[100];


	/* Never duplicate artifacts */
    if (allart_p(o_ptr)) return;


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
}



/*
 * Play with an item. Options include:
 *   - Output statistics (via wiz_roll_item)
 *   - Reroll item (via wiz_reroll_item)
 *   - Change properties (via wiz_tweak_item)
 *   - Change the number of items (via wiz_quantity_item)
 */
void do_cmd_wiz_play(void)
{
	errr err;

	object_type	forge;
	object_type *q_ptr;

	object_type *o_ptr;

	char ch;

	bool changed, finished = FALSE;


	/* Get an item (from equip or inven) */
	if (!((o_ptr = get_item(&err, "Play with which object? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to play with.");
		return;
	}


	/* The item was not changed */
	changed = FALSE;


	/* Icky */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();


	/* Get local object */
	q_ptr = &forge;
	
	/* Copy object */
	object_copy(q_ptr, o_ptr);


	/* The main loop */
	while (!finished)
	{
		/* Display the item */
		wiz_display_item(q_ptr);

		/* Get choice */
		(void)get_com("[a]ccept [s]tatistics [r]eroll [c]hange [t]weak [q]uantity? ", &ch);

		switch (FORCELOWER(ch))
		{
			case ESCAPE:
			changed = FALSE;
			finished = TRUE;
			break;

			case 'a':
			changed = TRUE;
			finished = TRUE;
			break;

			case 's':
			wiz_statistics(q_ptr);
			break;

			case 'r':
			wiz_reroll_item(q_ptr);
			break;

			case 't':
			wiz_tweak_item(q_ptr);
			break;

			case 'q':
			wiz_quantity_item(q_ptr);
			break;
			
			case 'c':
			wiz_change_item(q_ptr);
			break;
			
			default:
			bell(0);
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
		object_copy(o_ptr, q_ptr);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr, 0);
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
void wiz_create_item(int k_idx)
{
	object_type q_ptr[1];

	/* Ensure reasonable input */
	if (k_idx < 0 || k_idx >= MAX_K_IDX) k_idx = 0;
	else if (!k_info[k_idx].name) k_idx = 0;

	/* No meaningful input. */
	if (!k_idx) k_idx = choose_on_screen(wiz_create_itemtype);

	/* Return if failed */
	if (!k_idx) return;

	/* Create the item */
	object_prep(q_ptr, k_idx);

	/* Apply magic (no messages, no artifacts) */
	apply_magic(q_ptr, (dun_depth), FALSE, FALSE, FALSE);

	/* Drop the object from heaven */
	drop_near(q_ptr, -1, py, px);

	/* All done */
	msg_print("Allocated.");
}


/*
 * Cure everything instantly
 */
void do_cmd_wiz_cure_all(void)
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

	/* Restore chi */
	p_ptr->cchi = p_ptr->mchi;
	p_ptr->chi_frac = 0;

	/* Cure stuff */
	(void)set_flag(TIMED_BLIND, 0);
	(void)set_flag(TIMED_CONFUSED, 0);
	(void)set_flag(TIMED_POISONED, 0);
	(void)set_flag(TIMED_AFRAID, 0);
	(void)set_flag(TIMED_PARALYZED, 0);
	(void)set_flag(TIMED_IMAGE, 0);
	(void)set_flag(TIMED_STUN, 0);
	(void)set_flag(TIMED_CUT, 0);
	(void)set_flag(TIMED_SLOW, 0);

	/* No longer hungry */
	(void)set_flag(TIMED_FOOD, PY_FOOD_MAX - 1);

	/* Redraw everything */
	do_cmd_redraw();
}


/*
 * Go to any level
 */
void do_cmd_wiz_jump(void)
{
	/* Ask for level */
	if (command_arg <= 0)
	{
		char	ppp[80];

		char	tmp_val[160];

		/* Prompt */
		sprintf(ppp, "Jump to level (0-%d): ", dun_defs[cur_dungeon].max_level);

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		command_arg = atoi(tmp_val);
	}

	/* Paranoia */
	if (command_arg < 1) command_arg =1;

	/* Paranoia */
	if (command_arg > dun_defs[cur_dungeon].max_level) command_arg = dun_defs[cur_dungeon].max_level;

	/* Accept request */
	msg_format("You jump to dungeon level %d.", command_arg);

	change_level(command_arg, START_RANDOM);
}


/*
 * Become aware of a lot of objects
 */
void do_cmd_wiz_learn(void)
{
	int i;

	object_type forge;
	object_type *q_ptr;

	/* Scan every object */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Induce awareness */
		if (object_k_level(k_ptr) <= command_arg)
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
void do_cmd_wiz_summon(int num)
{
	int i;

	for (i = 0; i < num; i++)
	{
        (void)summon_specific(py, px, (dun_depth), SUMMON_ALL);
	}
}

/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named_aux(int r_idx, int slp, bool friend)
{
	int i, x, y;

	/* Request a choice if none supplied. */
	if (!r_idx) r_idx = choose_on_screen(choose_monster_type);

	/* Prevent illegal monsters */
	if (r_idx >= MAX_R_IDX || r_idx <= 0 || is_fake_monster(r_info+r_idx))
		return;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, py, px, d, 0);

		/* Require empty grids */
		if (!cave_empty_bold(y, x) || (cave[y][x].feat == FEAT_WATER)) continue;

		/* Place it (allow groups) */
        if (place_monster_aux(y, x, r_idx, slp != 0, TRUE, friend, TRUE)) break;
	}
}

/*
 * Make a hostile monster.
 */
void do_cmd_wiz_named(int r_idx, int slp)
{
	do_cmd_wiz_named_aux(r_idx, slp, FALSE);
}

/*
 * Make a friendly monster.
 */
void do_cmd_wiz_named_friendly(int r_idx, int slp)
{
	do_cmd_wiz_named_aux(r_idx, slp, TRUE);
}

/*
 * Hack -- Delete all nearby monsters
 */
void do_cmd_wiz_zap(void)
{
	int        i;

	/* Genocide everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Delete nearby monsters */
		if (m_ptr->cdis <= MAX_SIGHT) delete_monster_idx(i,TRUE);
	}
}

/*
 * Fire a magebolt at a creature that does 'enough' damage
 */
void do_cmd_magebolt(void)
{
	int dir;
	int tx, ty;
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Get a direction */
	if (!get_aim_dir(&dir)) return;

	/* Use the given direction */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	project(0, 0, ty, tx, 1000000, GF_MANA, flg);
}



/*
 * Ask for and parse a "debug command"
 * The "command_arg" may have been set.
 */
void do_cmd_debug(void)
{
	char		cmd;


	/* Get a "debug command" */
	(void)(get_com("Wizard Command: ", &cmd));

	command_new = CMD_DEBUG + cmd;
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif
