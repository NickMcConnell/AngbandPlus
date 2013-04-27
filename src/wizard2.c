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
void do_cmd_rerate(bool flag)
{
	int ave_value, min_value, max_value, i, j;

	ave_value = 2 * p_ptr->hitdie
	            + ((PY_MAX_LEVEL + 2) * (p_ptr->hitdie + 1));

	min_value = (ave_value * 95 / 200);
	max_value = (ave_value * 105 / 200);

	/* Rerate */
	while (1)
	{
		/* Pre-calculate level 1 hitdice */
		player_hp[0] = p_ptr->hitdie;

		/* Gain level 1 HP */
		for (i = 1; i < 4; i++)
		{
			player_hp[0] += randint1(p_ptr->hitdie);
		}

		/* Collect values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			/* Add in racial hit dice */
			j = randint1(p_ptr->hitdie);
			player_hp[i] = player_hp[i - 1] + j;
		}

		/* Legal values */
		if ((player_hp[PY_MAX_LEVEL - 1] >= min_value) &&
		    (player_hp[PY_MAX_LEVEL - 1] <= max_value)) break;
	}

	if (!flag) return;

	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Handle stuff */
	handle_stuff();
}


static cptr ltrim(cptr p)
{
	while(p[0] == ' ') p++;
	return p;
}

static void rtrim(char *p)
{
	int i = strlen(p) - 1;
	while(p[i] == ' ') p[i--] = '\0';
}

/* Reversive strncmp */
static int strrncmp(cptr s1, cptr s2, int len)
{
	int i;
	int l1 = strlen(s1);
	int l2 = strlen(s2);

	for (i = 1; i <= len; i++)
	{
		int p1 = l1 - i;
		int p2 = l2 - i;

		if (l1 != l2)
		{
			if (p1 < 0) return (-1);
			if (p2 < 0) return (1);
		}
		else
		{
			if (p1 < 0) return (0);
		}

		if (s1[p1] < s2[p2]) return (-1);
		if (s1[p1] > s2[p2]) return (-1);
	}

	return (0);
}


static int ego_slot(object_type *o_ptr)
{
	int slot = wield_slot(o_ptr);

	if (slot > -1) return slot;

	if ((o_ptr->tval == TV_SHOT) ||
		(o_ptr->tval == TV_ARROW) ||
		(o_ptr->tval == TV_BOLT))
		return (INVEN_AMMO);

	return (-1);
}

static void wishing_puff_of_smoke(void)
{
#ifdef JP
	msg_print("何かが足下に転がってきたが、煙のように消えてしまった。");
#else
	msg_print("You feel something roll beneath your feet, but it disappears in a puff of smoke!");
#endif
}

/*
 * XAngband: wishing
 * Make an wishing object, ego or artifact when it exists.
 * Return values:
 * 0 - canceled
 * 1 - normal objects
 * 2 - ego items
 * 3 - artifacts
 * -1 - failed
 */
s16b do_cmd_wishing(int prob, bool art, bool ego, bool confirm)
{
	int k, i;
	int a_id, a_num = 0;
	int e_id[10], e_num = 0;
	int k_id, k_num;
	char buf[MAX_NLEN];
	char o_name[MAX_NLEN];
	cptr str;
	object_type forge;
	object_type *q_ptr = &forge;
	object_kind *k_ptr;
	artifact_type *a_ptr = NULL;
	ego_item_type *e_ptr = NULL;

	/* initialize */
	bool wish_art = FALSE;
	bool randart = FALSE;
	bool wish_ego = FALSE;
	bool base = TRUE;
	bool ok = (randint0(100) < prob) ? TRUE : FALSE;
	bool ok2 = (randint0(100) < 50 + prob) ? TRUE : FALSE;
	bool must = (prob < 0) ? TRUE : FALSE;
	bool blessed = FALSE;
	bool fixed = FALSE;

	char *fixed_str[] = {
#ifdef JP
		"燃えない",
		"錆びない",
		"腐食しない",
		"安定した",
#else
		"rotproof",
		"fireproof",
		"rustproof",
		"erodeproof",
		"corrodeproof",
		"fixed",
#endif
		NULL,
	};

	buf[0] = '\0';
	str = buf;

	/* get wishing */
	while (1)
	{
#ifdef JP
		if (get_string("何をお望み？ ", buf, (MAX_NLEN - 1))) break;
		if (confirm)
		{
			if (!get_check("何も願いません。本当によろしいですか？")) continue;
		}
#else
		if (get_string("For what do you wish? ", buf, (MAX_NLEN - 1))) break;
		if (confirm)
		{
			if (!get_check("Do you wish nothing, really?")) continue;
		}
#endif
		return (0);
	}

#ifndef JP
	str_tolower(buf);

	/* remove 'a' */
	if (!strncmp(buf, "a ", 2)) str = ltrim(str + 1);
	else if (!strncmp(buf, "an ", 3)) str = ltrim(str + 2);
#endif

	/* remove surplus spaces */
	rtrim(buf);


	/*** evaluate header strings ****/

	/* wishing blessed object ? */
#ifdef JP
	if (!strncmp(str, "祝福された", 10))
	{
		str = ltrim(str+10);
		blessed = TRUE;
	}
#else
	if (!strncmp(str, "blessed", 7))
	{
		str = ltrim(str+7);
		blessed = TRUE;
	}
#endif

	/* wishing fixed object ? */
	for (i = 0; fixed_str[i] != NULL; i++)
	{
		if (!strncmp(str, fixed_str[i], strlen(fixed_str[i])))
		{
			str = ltrim(str+strlen(fixed_str[i]));
			fixed = TRUE;
			break;
		}
	}

#ifdef JP
	/* wishing preserve artifacts ? */
	if (!strncmp(str, "★", 2))
	{
		str = ltrim(str + 2);
		wish_art = TRUE;
		base = FALSE;
	}

	/* wishing random artifacts ? */
	else if (!strncmp(str, "☆", 2))
	{
		str = ltrim(str + 2);
		wish_art = TRUE;
		randart = TRUE;
	}

	/* wishing random ego ? */
	else if (!strncmp(str, "高級な", 6))
	{
		str = ltrim(str + 6);
		wish_ego = TRUE;
	}
#else
	/* wishing artifacts ? */
	if (!strncmp(str, "the ", 4))
	{
		str = ltrim(str + 4);
		wish_art = TRUE;
		randart = TRUE;
	}

	/* wishing random ego ? */
	else if (!strncmp(str, "excellent ", 9))
	{
		str = ltrim(str + 9);
		wish_ego = TRUE;
	}
#endif /* JP */

	/* No name */
	if (strlen(str) < 1)
	{
#ifdef JP
		msg_print("名前がない！");
#else
		msg_print("What?");
#endif
		return (0);
	}

	if ((!art) && (wish_art))
	{
#ifdef JP
		msg_print("アーティファクトは願えない！");
#else
		msg_print("You can not wish artifacts!");
#endif
		return (0);
	}

	if (cheat_xtra) msg_format("Wishing %s....", buf);


	/*** Search target object ***/

	/* search normal items */
	k_id = -1;
	k_num = 0;

	/* do when you do not wish fixed artifact directry */
	if (base)
	{
		int len;
		int mlen = 0;

		/* search base object */
		for (k = 1; k < max_k_idx; k++)
		{
			k_ptr = &k_info[k];

			/* Skip "empty" objects */
			if (!k_ptr->name) continue;

			/* make fake object */
			object_prep(q_ptr, k);

			/* get name */
			object_desc(o_name, q_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY | OD_STORE));
#ifndef JP
			str_tolower(o_name);
#endif

			if (cheat_xtra) msg_format("Matching object No.%d %s", k, o_name);

			/* matching */
			len = strlen(o_name);
#ifdef JP
			if (!strrncmp(str, o_name, len))
#else
			if (!strncmp(str, o_name, len))
#endif
			{
				/* Choice a object which matches longer */
				if (len > mlen)
				{
					k_id = k;
					k_num = 1;
					mlen = len;
				}
			}
		}

		/* if wishing base object exists, test for ego name */
		if ((ego) && (k_num == 1))
		{
			e_num = 0;

			/* make fake base object */
			object_prep(q_ptr, k_id);

			for (k = 1; k < max_e_idx; k++)
			{
				e_ptr = &e_info[k];

				/* Skip "empty" ego */
				if (!e_ptr->name) continue;

				/* get name */
				strcpy(o_name, (e_name + e_ptr->name));
#ifndef JP
				str_tolower(o_name);
#endif

				if (cheat_xtra) msg_format("Mathcing ego No.%d %s...", k, o_name);

				/* matching */
#ifdef JP
				if (!strncmp(str, o_name, strlen(o_name)))
#else
				if (!strrncmp(str, o_name, strlen(o_name)))
#endif
				{
					/* check slot */
					if (ego_slot(q_ptr) != e_ptr->slot) continue;

					/* memorize egos have same name */
					e_id[e_num++] = k;
				}
			}
		}
	}

	/* search artifacts */
	a_id = -1;
	a_num = 0;

	if (art)
	{
		char *astr;
		char aname[MAX_NLEN];

		for (k = 1; k < max_a_idx; k++)
		{
			object_type forge;

			q_ptr = &forge;
			a_ptr = &a_info[k];

			/* Skip "empty" artifacts */
			if (!a_ptr->name) continue;

			/* make fake artfact */
			i = lookup_kind(a_ptr->tval, a_ptr->sval);
			if (!i) continue;
			object_prep(q_ptr, i);
			q_ptr->name1 = k;

			/* get name */
			object_desc(o_name, q_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY | OD_STORE));
#ifndef JP
			str_tolower(o_name);
#endif

			/* get omitted name */
			astr = (a_name + a_ptr->name);

			/* remove full-name flag */
			if (astr[0] == '$') astr++;

#ifdef JP
			/* remove quotes */
			if (!strncmp(astr, "『", 2))
			{
				strcpy(aname, (astr + 2));
				astr = strstr(aname, "』");
				astr[0] = '\0';
			}
			/* remove 'of' */
			else
			{
				int l = strlen(astr);
				strcpy(aname, astr);
				if (!strrncmp(aname, "の", 2))
				{
					aname[l-2] = '\0';
				}
			}
#else
			/* remove quotes */
			if (astr[0] == '\'')
			{
				strcpy(aname, (astr + 1));
				astr = strchr(aname, '\'');
				astr[0] = '\0';
			}
			/* remove 'of ' */
			else if (!strncmp(astr, "of ", 3))
			{
				strcpy(aname, (astr + 3));
			}

			str_tolower(aname);
#endif

#ifdef JP
			if (cheat_xtra) msg_format("Matching artifact No.%d %s(%s)", k, aname, &o_name[2]);
#else
			if (cheat_xtra) msg_format("Matching artifact No.%d %s(%s)", k, aname, o_name);
#endif

			/* entire match */
#ifdef JP
			if (!strcmp(&o_name[2], str))
#else
			if (!strcmp(o_name, str))
#endif
			{
				a_id = k;
				a_num = 1;
				break;
			}

			/* partial match */
			else if (!strcmp(aname, str))
			{
				if (one_in_(a_num))
				{
					a_id = k;
					a_num++;
				}
			}
		}
	}


	/*** Create target object ***/

	/* Too many matches */
	if ((wizard) && ((a_num > 1) || (k_num > 1)))
	{
#ifdef JP
		msg_print("候補が多すぎる！");
#else
		msg_print("Too many matches!");
#endif
		return (-1);
	}

	/* wished artifact is found */
	else if ((a_id >= 0) && (a_num == 1))
	{
		if ((must) || (ok && !a_info[a_id].cur_num))
		{
			/* make target preserve artifact */
			create_named_art(a_id, py, px);
		}
		else
		{
			wishing_puff_of_smoke();
		}

		return (3);
	}

	/* wished object is found */
	else if ((!ego) && (wish_ego || e_num))
	{
#ifdef JP
		msg_print("エゴアイテムは願えない！");
#else
		msg_print("Can not wish ego items.");
#endif
		return (0);
	}

	else if ((k_id >= 0) && (k_num == 1))
	{
		byte retval = 1;
		int object_level;
		object_type forge;
		q_ptr = &forge;
		k_ptr = &k_info[k_id];
		object_level = k_ptr->level;

		/* Wish staff of wishing */
		if ((k_ptr->tval == TV_STAFF) && (k_ptr->sval == SV_STAFF_WISHING))
		{
#ifdef JP
			msg_print("その願いはすでにかなっている。");
#else
			msg_print("This wish has already been fulfilled.");
#endif
			return (-1);
		}

		/* Instant artifact objects */
		else if (k_ptr->gen_flags & (TRG_INSTA_ART))
		{
			for (k = 0; k < max_a_idx; k++)
			{
				a_ptr = &a_info[k];
				if (a_ptr->tval != k_ptr->tval) continue;
				if (a_ptr->sval != k_ptr->sval) continue;
				q_ptr->name1 = k;
				break;
			}

			if ((must) || (ok && !a_ptr->cur_num))
			{
				object_prep(q_ptr, k_id);
				apply_magic(q_ptr, -1, TRUE, TRUE, TRUE, FALSE);
			}
			else /* Not ok */
			{
				wishing_puff_of_smoke();
			}

			retval = 3;
		}

		/* Random Artifacts */
		else if (randart)
		{
			if (must || ok)
			{
				do
				{
					object_prep(q_ptr, k_id);
					apply_magic(q_ptr, object_level, FALSE, FALSE, FALSE, FALSE);
				}
				while (q_ptr->name1 || q_ptr->name2 || cursed_p(q_ptr));
				
				if (!q_ptr->art_name) (void)create_artifact(q_ptr, FALSE);
			}
			else /* Not ok */
			{
				wishing_puff_of_smoke();
			}

			retval = 3;
		}

		/* Ego items */
		else if ((ego) && (wish_ego || e_num))
		{
			if (must || ok2)
			{
				int max_roll = 1000;

				for (i = 0; i < max_roll; i++)
				{
					object_prep(q_ptr, k_id);
					(void)apply_magic(q_ptr, object_level, FALSE, TRUE, TRUE, FALSE);

					/* Paranoia */
					if (q_ptr->name1) continue;

					/* wishing a random ego */
					if (wish_ego) break;

					/* Match test */
					for (k = 0; k < e_num; k++)
					{
						if (q_ptr->name2 == e_id[k]) break;
					}

					/* Matched */
					if (k < e_num) break;
				}

				if (i == max_roll)
				{
#ifdef JP
					msg_print("失敗！もう一度願ってみてください。");
#else
					msg_print("Failed! Try again.");
#endif
					return (-1);
				}
			}
			else /* Not ok */
			{
				wishing_puff_of_smoke();
				return (2);
			}

			retval = 2;
		}

		/* Normal items */
		else
		{
			/* Try to make an uncursed object */
			for (i = 0; i < 100; i++)
			{
				object_prep(q_ptr, k_id);
				apply_magic(q_ptr, -1, FALSE, FALSE, FALSE, FALSE);
				if (!cursed_p(q_ptr)) break;
			}
		}

		/* Blessed */
		if (blessed && (wield_slot(q_ptr) != -1))
		{
			q_ptr->art_flags3 |= TR3_BLESSED;
		}

		/* Fixed */
		if (fixed && (wield_slot(q_ptr) != -1))
		{
			q_ptr->art_flags3 |= TR3_IGNORE_ACID;
			q_ptr->art_flags3 |= TR3_IGNORE_FIRE;
		}

		/* Drop it */
		(void)drop_near(q_ptr, -1, py, px);

		return (retval);
	}

	/* Nothing */
	else
	{
#ifdef JP
		msg_print("うーん、そんなものは存在しないようだ。");
#else
		msg_print("Ummmm, that is not existing.");
#endif
		return (-1);
	}

	return (1);
}


#ifdef ALLOW_WIZARD

/*
 * Dimension Door
 */
static bool wiz_dimension_door(void)
{
	int	x = 0, y = 0;

	if (!tgt_pt(&x, &y)) return FALSE;

	if (!cave_empty_bold(y, x))
	{
#ifdef JP
msg_print("精霊界から物質界に戻る時うまくいかなかった！");
#else
		msg_print("You fail to exit the astral plane correctly!");
#endif

		teleport_player(10);
	}
	else teleport_player_to(y, x);

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
	teleport_player_to(target_row, target_col);
}


/*
 * Aux function for "do_cmd_wiz_change()".	-RAK-
 */
static void do_cmd_wiz_change_aux(void)
{
	int i;
	int tmp_int;
	long tmp_long;
	char tmp_val[160];
	char ppp[80];


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


	/* Default */
	sprintf(tmp_val, "%ld", (long)(p_ptr->max_exp));

	/* Query */
	if (!get_string("Experience: ", tmp_val, 9)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->max_exp = tmp_long;
	p_ptr->exp = tmp_long;

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
 * Create a feature near the player.
 */
static void do_cmd_wiz_feature(int feat)
{
	int y, x, d = 3, attempts = 30;

	while (1)
	{
		/* Find a location */
		y = rand_spread(py, d);
		x = rand_spread(px, d);

		/* Reject illegal grids */
		if (!in_bounds(y, x)) continue;

		/* Reject the player */
		if ((y == py) && (x == px)) continue;

		attempts--;

		if (!attempts)
		{
			d++;
			attempts = 8 * d;
		}

		/* Try to place a new feature */
		if (cave[y][x].feat == feat) continue;

		/* Okay */
		break;
	}

	/* Nuke objects */
	delete_object_idx(cave[y][x].o_idx);

	/* Nuke monsters */
	delete_monster_idx(cave[y][x].m_idx);

	/* Forget this grid */
	cave[y][x].info &= ~(CAVE_MARK);

	/* Place the feature */
	cave_set_feat(y, x, feat);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE | PU_MONSTERS);
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
	u32b f1, f2, f3;
	char buf[256];

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Clear the screen */
	for (i = 1; i <= 23; i++) prt("", i, j - 2);

	prt_alloc(o_ptr->tval, o_ptr->sval, 1, 0);

	/* Describe fully */
	object_desc(buf, o_ptr, OD_STORE);

	prt(buf, 2, j);

	prt(format("kind = %-5d  level = %-4d  tval = %-5d  sval = %-5d",
		   o_ptr->k_idx, get_object_level(o_ptr),
		   o_ptr->tval, o_ptr->sval), 4, j);

	prt(format("number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",
		   o_ptr->number, o_ptr->weight,
		   o_ptr->ac, o_ptr->dd, o_ptr->ds), 5, j);

	prt(format("pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",
		   o_ptr->pval, o_ptr->to_a, o_ptr->to_h, o_ptr->to_d), 6, j);

	prt(format("name1 = %-4d  name2 = %-4d  cost = %-6ld ident = %04x",
		   o_ptr->name1, o_ptr->name2, (long)object_value(o_ptr), o_ptr->ident), 7, j);

	prt(format("xtra1 = %-4d  xtra2 = %-4d  xtra3 = %-4d  timeout = %-d",
		   o_ptr->xtra1, o_ptr->xtra2, o_ptr->xtra3, o_ptr->timeout), 8, j);

	prt("+------------FLAGS1------------+", 10, j);
	prt("AFFECT........SLAY........BRAND.", 11, j);
	prt("             cv ae      xsqpaefc", 12, j);
	prt("siwdccmssidsahahnvudotgddhuoclio", 13, j);
	prt("tnieohdtrnipttmmiinmrrnrrraiierl", 14, j);
	prt("rtsxnavlcfgdkcpnmldncltggpksdced", 15, j);
	prt_binary(f1, 16, j);

	prt("+------------FLAGS2------------+", 17, j);
	prt("SUST...IMMUN..RESIST............", 18, j);
	prt("        aefctrpsaefcpfldbc sn   ", 19, j);
	prt("siwdcc  clioheatcliooeialoshtncd", 20, j);
	prt("tnieoh  ierlrfraierliatrnnnrhehi", 21, j);
	prt("rtsxna..dcedwlatdcedsrekdfddrxss", 22, j);
	prt_binary(f2, 23, j);

	prt("+------------FLAGS3------------+", 10, j+32);
	prt("fe c     hs   st    iiiiadta  hp", 11, j+32);
	prt("il o n t ihdf ee    ggggcregb vr", 12, j+32);
	prt("re lnowy doee eld   nnnntalrl ym", 13, j+32);
	prt("ec domrc ewca ieirmsrrrriieaeccc", 14, j+32);
	prt("aa ataau tmmtlnpgeihaefcvnpvsuuu", 15, j+32);
	prt("uu uegir yoahivaeggoclioaeoasrrr", 16, j+32);
	prt("rr rlits pdnetitsehtierltxrtesss", 17, j+32);
	prt("aa aeche esareshtntsdcedeptedeee", 18, j+32);
	prt_binary(f3, 19, j+32);
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
#ifdef JP
	{ TV_SWORD,             "刀剣"                 },
	{ TV_POLEARM,           "竿状武器"             },
	{ TV_HAFTED,            "鈍器"                 },
	{ TV_BOW,               "射撃武器"             },
	{ TV_ARROW,             "矢"                   },
	{ TV_BOLT,              "ボルト"               },
	{ TV_SHOT,              "弾"                   },
	{ TV_SHIELD,            "盾"                   },
	{ TV_CROWN,             "冠"                   },
	{ TV_HELM,              "兜"                   },
	{ TV_GLOVES,            "グローブ"             },
	{ TV_BOOTS,             "靴"                   },
	{ TV_CLOAK,             "クローク"             },
	{ TV_DRAG_ARMOR,        "竜の鱗鎧"             },
	{ TV_HARD_ARMOR,        "硬質な鎧"             },
	{ TV_SOFT_ARMOR,        "軟質な鎧"             },
	{ TV_RING,              "指輪"                 },
	{ TV_AMULET,            "アミュレット"         },
	{ TV_LITE,              "光源"                 },
	{ TV_POTION,            "薬"                   },
	{ TV_SCROLL,            "巻物"                 },
	{ TV_WAND,              "ワンド"               },
	{ TV_STAFF,             "スタッフ"             },
	{ TV_ROD,               "ロッド"               },
	{ TV_LIFE_BOOK,         "魔法書(生命)"         },
	{ TV_SORCERY_BOOK,      "魔法書(仙術)"         },
	{ TV_SPIKE,             "くさび"               },
	{ TV_DIGGING,           "採掘道具"             },
	{ TV_CHEST,             "宝箱"                 },
	{ TV_FIGURINE,          "人形"                 },
	{ TV_STATUE,            "像"                   },
	{ TV_CORPSE,            "死体"                 },
	{ TV_FOOD,              "食料"                 },
	{ TV_FLASK,             "油つぼ"               },
	{ TV_JUNK,              "ガラクタ"             },
	{ TV_SKELETON,          "骨"                   },
	{ 0,                    NULL                   }
#else
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
	{ TV_LIFE_BOOK,         "Life Spellbook"       },
	{ TV_SORCERY_BOOK,      "Sorcery Spellbook"    },
	{ TV_SPIKE,             "Spikes"               },
	{ TV_DIGGING,           "Digger"               },
	{ TV_CHEST,             "Chest"                },
	{ TV_FIGURINE,          "Magical Figurine"     },
	{ TV_STATUE,            "Statue"               },
	{ TV_CORPSE,            "Corpse"               },
	{ TV_FOOD,              "Food"                 },
	{ TV_FLASK,             "Flask"                },
	{ TV_JUNK,              "Junk"                 },
	{ TV_SKELETON,          "Skeleton"             },
	{ 0,                    NULL                   }
#endif
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
	for (num = 0; num < max_num; num++)
	{
		if (listsym[num] == ch) break;
	}

	/* Bail out if choice is illegal */
	if (num >= max_num) return (0);

	/* Base object type chosen, fill in tval */
	tval = tvals[num].tval;
	tval_desc = tvals[num].desc;


	/*** And now we go for k_idx ***/

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 60) && (i < max_k_idx); i++)
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
#ifdef JP
	if (!get_com(format("どの種類の%s? ", tval_desc), &ch)) return (0);
#else
	if (!get_com(format("What Kind of %s? ", tval_desc), &ch)) return (0);
#endif

	/* Analyze choice */
	for (num = 0; num < max_num; num++)
	{
		if (listsym[num] == ch) break;
	}

	/* Bail out if choice is "illegal" */
	if (num >= max_num) return (0);

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


	p = "Enter new 'pval' setting: ";
	sprintf(tmp_val, "%d", (int)(o_ptr->pval));
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

	p = "Enter new 'xtra2' setting: ";
	sprintf(tmp_val, "%d", o_ptr->xtra2);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->xtra2 = atoi(tmp_val);
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

	bool changed;


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
		if (!get_com("[a]ccept, [w]orthless, [c]ursed, [n]ormal, [g]ood, [e]xcellent, [s]pecial? ", &ch))
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
				apply_magic(q_ptr, dun_level, FALSE, TRUE, TRUE, TRUE);
				break;
			}
			/* Apply bad magic, but first clear object */
			case 'c': case 'C':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, FALSE, TRUE, FALSE, TRUE);
				break;
			}
			/* Apply normal magic, but first clear object */
			case 'n': case 'N':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, FALSE, FALSE, FALSE, FALSE);
				break;
			}
			/* Apply good magic, but first clear object */
			case 'g': case 'G':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, FALSE, TRUE, FALSE, FALSE);
				break;
			}
			/* Apply great magic, but first clear object */
			case 'e': case 'E':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, FALSE, TRUE, TRUE, FALSE);
				break;
			}
			case 's': case 'S':
			{
				if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_WRAITH))
				{
					object_prep(q_ptr, o_ptr->k_idx);
					create_nazgul_ring(q_ptr);
				}
				else
				{
					while (1)
					{
						object_prep(q_ptr, o_ptr->k_idx);
						apply_magic(q_ptr, dun_level, TRUE, TRUE, TRUE, FALSE);

						if (!q_ptr->name2) break;
					}

					/* Failed to create normal artifact; make a random one */
					if (!(artifact_p(q_ptr) || q_ptr->art_name)) create_artifact(q_ptr, FALSE);
				}

				break;
			}
		}
	}


	/* Notice change */
	if (changed)
	{
		/* Restore the position information */
		q_ptr->iy = o_ptr->iy;
		q_ptr->ix = o_ptr->ix;
		q_ptr->next_o_idx = o_ptr->next_o_idx;
		q_ptr->marked = o_ptr->marked;

		/* Apply changes */
		object_copy(o_ptr, q_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_STATS);
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

	bool good, great;

	object_type forge;
	object_type	*q_ptr;

	cptr q = "Rolls: %ld  Correct: %ld  Matches: %ld  Better: %ld  Worse: %ld  Other: %ld";

	cptr p = "Enter number of items to roll: ";
	char tmp_val[80];


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
			make_object(q_ptr, good, great);


			/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
			if (artifact_p(q_ptr)) a_info[q_ptr->name1].cur_num = 0;


			/* Test for the same tval and sval. */
			if ((o_ptr->tval) != (q_ptr->tval)) continue;
			if ((o_ptr->sval) != (q_ptr->sval)) continue;

			/* One more correct item */
			correct++;

			/* Check for match */
			if ((q_ptr->pval == o_ptr->pval) &&
				 (q_ptr->to_a == o_ptr->to_a) &&
				 (q_ptr->to_h == o_ptr->to_h) &&
				 (q_ptr->to_d == o_ptr->to_d) &&
				 (q_ptr->name1 == o_ptr->name1))
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

		/* Hack -- rod pvals must change if the number in the stack does. -LM- */
		if (o_ptr->tval == TV_ROD)
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


	/* Save the screen */
	screen_save();


	/* Get local object */
	q_ptr = &forge;

	/* Copy object */
	object_copy(q_ptr, o_ptr);


	/* The main loop */
	while (TRUE)
	{
		/* Display the item */
		wiz_display_item(q_ptr);

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

		/* Change */
		object_copy(o_ptr, q_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_STATS);
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

	/* Get local object */
	q_ptr = &forge;

	/* Create the item */
	object_prep(q_ptr, k_idx);

	if (k_info[k_idx].gen_flags & TRG_INSTA_ART)
	{
		int i;

		/* Artifactify */
		for (i = 1; i < max_a_idx; i++)
		{
			/* Ignore incorrect tval */
			if (a_info[i].tval != q_ptr->tval) continue;

			/* Ignore incorrect sval */
			if (a_info[i].sval != q_ptr->sval) continue;

			/* Choose this artifact */
			q_ptr->name1 = i;
			break;
		}

		/* Apply magic */
		apply_magic(q_ptr, -1, TRUE, TRUE, TRUE, FALSE);
	}
	else
	{
		/* Apply magic */
		apply_magic(q_ptr, dun_level, FALSE, FALSE, FALSE, FALSE);
	}

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
	/* Ask for level */
	if (command_arg <= 0)
	{
		char	ppp[80];

		char	tmp_val[160];

		/* Prompt */
		sprintf(ppp, "Jump to level (0-%d): ", TINY_MAX_DEPTH-1);

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		command_arg = atoi(tmp_val);
	}

	/* Paranoia */
	if (command_arg < 0) command_arg = 0;

	/* Paranoia */
	if (command_arg > TINY_MAX_DEPTH - 1) command_arg = TINY_MAX_DEPTH - 1;

	/* Accept request */
	msg_format("You jump to dungeon level %d.", command_arg);

	if (autosave_l) do_cmd_save_game(TRUE);

	/* Change level */
	dun_level = command_arg;

	p_ptr->inside_arena = 0;

	leave_quest_check();

	p_ptr->inside_quest = 0;

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
		(void)summon_specific(0, py, px, dun_level, 0, TRUE, FALSE, FALSE);
	}
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named(int r_idx, bool slp)
{
	(void)summon_named_creature(py, px, r_idx, slp, TRUE, FALSE);
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named_friendly(int r_idx, bool slp)
{
	(void)summon_named_creature(py, px, r_idx, slp, TRUE, TRUE);
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
		if (m_ptr->cdis <= MAX_SIGHT) delete_monster_idx(i);
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

		/* Delete this monster */
		delete_monster_idx(i);
	}
}


#define NUM_O_SET 8
#define NUM_O_BIT 32

/*
 * Hack -- Dump option bits usage
 */
static void do_cmd_dump_options(void)
{
	int  i, j;
	FILE *fff;
	char buf[1024];
	int  **exist;

	/* Build the filename */
	path_build(buf, sizeof buf, ANGBAND_DIR_USER, "opt_info.txt");

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "a");

	/* Oops */
	if (!fff)
	{
#ifdef JP
		msg_format("ファイル %s を開けませんでした。", buf);
#else
		msg_format("Failed to open file %s.", buf);
#endif
		msg_print(NULL);
		return;
	}

	/* Allocate the "exist" array (2-dimension) */
	C_MAKE(exist, NUM_O_SET, int *);
	C_MAKE(*exist, NUM_O_BIT * NUM_O_SET, int);
	for (i = 1; i < NUM_O_SET; i++) exist[i] = *exist + i * NUM_O_BIT;

	/* Check for exist option bits */
	for (i = 0; option_info[i].o_desc; i++)
	{
		option_type *ot_ptr = &option_info[i];
		if (ot_ptr->o_var) exist[ot_ptr->o_set][ot_ptr->o_bit] = i + 1;
	}

	fprintf(fff, "[Option bits usage on XAngband %d.%d.%d]\n\n",
	        FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH);

	fputs("Set - Bit (Page) Option Name\n", fff);
	fputs("------------------------------------------------\n", fff);
	/* Dump option bits usage */
	for (i = 0; i < NUM_O_SET; i++)
	{
		for (j = 0; j < NUM_O_BIT; j++)
		{
			if (exist[i][j])
			{
				option_type *ot_ptr = &option_info[exist[i][j] - 1];
				fprintf(fff, "  %d -  %02d (%4d) %s\n",
				        i, j, ot_ptr->o_page, ot_ptr->o_text);
			}
			else
			{
				fprintf(fff, "  %d -  %02d\n", i, j);
			}
		}
		fputc('\n', fff);
	}

	/* Free the "exist" array (2-dimension) */
	C_KILL(*exist, NUM_O_BIT * NUM_O_SET, int);
	C_KILL(exist, NUM_O_SET, int *);

	/* Close it */
	my_fclose(fff);

#ifdef JP
	msg_format("オプションbit使用状況をファイル %s に書き出しました。", buf);
#else
	msg_format("Option bits usage dump saved to file %s.", buf);
#endif
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
	get_com("Debug Command: ", &cmd);

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
		screen_save();
#ifdef JP
		show_file("jwizard.txt", NULL, 0 , 0);
#else
		show_file("wizard.txt", NULL, 0 , 0);
#endif
		screen_load();
		break;

	/* Cure all maladies */
	case 'a':
		do_cmd_wiz_cure_all();
		break;

	/* Know alignment */
	case 'A':
		msg_format("Your alignment is %d.", p_ptr->align);
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
		detect_all(DETECT_RAD_ALL);
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
		(void)identify_fully();
		break;

	/* Create feature */
	case 'F':
		if (command_arg > 0) do_cmd_wiz_feature(command_arg);
		break;

	/* Good Objects */
	case 'g':
		if (command_arg <= 0) command_arg = 1;
		acquirement(py, px, command_arg, FALSE, TRUE);
		break;

	/* Hitpoint rerating */
	case 'h':
		do_cmd_rerate(TRUE);
		break;

#ifdef MONSTER_HORDES
	case 'H':
		do_cmd_summon_horde();
		break;
#endif /* MONSTER_HORDES */

	/* Identify */
	case 'i':
		(void)ident_spell();
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
		(void)gain_random_mutation(command_arg);
		break;

	/* Lose Mutation */
	case 'L':
		if (p_ptr->muta)
		{
			while (!lose_mutation(0));
		}
		break;

	/* Summon _friendly_ named monster */
	case 'N':
		do_cmd_wiz_named_friendly(command_arg, TRUE);
		break;

	/* Summon Named Monster */
	case 'n':
		do_cmd_wiz_named(command_arg, TRUE);
		break;

	/* Dump option bits usage */
	case 'O':
		do_cmd_dump_options();
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
#endif

	/* Make every dungeon square "known" to test streamers -KMW- */
	case 'u':
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				cave[y][x].info |= (CAVE_GLOW | CAVE_MARK);
			}
		}
		wiz_lite_aux(TRUE, TRUE);
		break;

	/* Summon Random Monster(s) */
	case 's':
		if (command_arg <= 0) command_arg = 1;
		do_cmd_wiz_summon(command_arg);
		break;

	/* Teleport */
	case 't':
		teleport_player(100);
		break;

	/* Very Good Objects */
	case 'v':
		if (command_arg <= 0) command_arg = 1;
		acquirement(py, px, command_arg, TRUE, TRUE);
		break;

	/* Wizard Light the Level */
	case 'w':
		wiz_lite();
		break;

	/* Increase Experience */
	case 'x':
		gain_exp(command_arg ? command_arg : (p_ptr->exp + 1));
		break;

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

	/* Hack: wishing */
	case 'W':
		do_cmd_wishing(-1, TRUE, TRUE, FALSE);
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
