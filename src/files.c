/*
 * File: files.c
 * Purpose: Various file-related activities, poorly organised
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "ui-menu.h"
#include "game-cmd.h"
#include "cmds.h"
#include "game-event.h"


#define MAX_PANEL 12


/*
 * Returns a "rating" of x depending on y, and sets "attr" to the
 * corresponding "attribute".
 */
static cptr likert(int x, int y, byte *attr)
{
	/* Paranoia */
	if (y <= 0) y = 1;

	/* Negative value */
	if (x < 0)
	{
		*attr = TERM_RED;
		return ("Very Bad");
	}

	/* Analyze the value */
	switch ((x / y))
	{
		case 0:
		case 1:
		{
			*attr = TERM_RED;
			return ("Bad");
		}
		case 2:
		{
			*attr = TERM_RED;
			return ("Poor");
		}
		case 3:
		case 4:
		{
			*attr = TERM_YELLOW;
			return ("Fair");
		}
		case 5:
		{
			*attr = TERM_YELLOW;
			return ("Good");
		}
		case 6:
		{
			*attr = TERM_YELLOW;
			return ("Very Good");
		}
		case 7:
		case 8:
		{
			*attr = TERM_L_GREEN;
			return ("Excellent");
		}
		case 9:
		case 10:
		case 11:
		case 12:
		case 13:
		{
			*attr = TERM_L_GREEN;
			return ("Superb");
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			*attr = TERM_L_GREEN;
			return ("Heroic");
		}
		default:
		{
			*attr = TERM_L_GREEN;
			return ("Legendary");
		}
	}
}


/*
 * Equippy chars
 */
static void display_player_equippy(int y, int x, bool onscreen)
{
	int i;

	byte a;
	char c;

	object_type *o_ptr;


	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
	{
		/* Object */
		o_ptr = &inventory[i];

		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;

		if (onscreen)
		{
			/* Get attr/char for display */
			a = object_attr(o_ptr);
			c = object_char(o_ptr);
		}
		else
		{
			/* Get attr/char for display */
			a = object_attr_default(o_ptr);
			c = object_char_default(o_ptr);
		}

		/* Dump */
		Term_putch(x+i-INVEN_WIELD, y, a, c);
	}
}


/*
 * Equippy chars
 */
static void display_home_inven_letters(int y, int x, bool onscreen)
{
	int i;

	cptr which_set;

	if (!onscreen) which_set = "abcdefghijklmnopqrstuvwxyz";
	else if (rogue_like_commands) which_set = roguelike_home_letters;
	else which_set = standard_home_letters;

	/* Dump equippy chars */
	for (i = 0; i < MAX_INVENTORY_HOME; ++i)
	{
		char c = which_set[i];

		/* Dump */
		Term_putch(x+i, y, TERM_WHITE, c);
	}
}


/*
 * Equippy chars
 */
static void display_home_equippy(int y, int x, bool onscreen)
{
	int i;

	byte a ;
	char c;

	object_type *o_ptr;

	store_type *st_ptr = &store[STORE_HOME];

	/* Dump equippy chars */
	for (i = 0; i < MAX_INVENTORY_HOME	; ++i)
	{
		/* Object */
		o_ptr = &st_ptr->stock[i];

		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;

		if (onscreen)
		{
			/* Get attr/char for display */
			a = object_attr(o_ptr);
			c = object_char(o_ptr);
		}
		else
		{
			/* Get attr/char for display */
			a = object_attr_default(o_ptr);
			c = object_char_default(o_ptr);
		}

		/* Dump */
		Term_putch(x+i, y, a, c);
	}
}


/*
 * Prints some "extra" information on the screen.
 *
 * Space includes rows 3-9 cols 24-79
 * Space includes rows 10-17 cols 1-79
 * Space includes rows 19-22 cols 1-79
 */
void display_player_xtra_info(void)
{
	int i;
	s32b tmpl;
	int row, col, col2;
	int hit, dam;
	int base, plus;
	int tmp;
	int xthn, xthb, xfos, xsrh;
	int xdis, xdev, xsav, xstl;
	byte likert_attr;

	object_type object_type_body;
	object_type *o_ptr;
	object_type *i_ptr;

	cptr desc;

	char buf[160];

	/* Upper middle */
	col = 23;
	col2 = col + 7;

	/* Age */
	row = 2;
	Term_putstr(col, row, -1, TERM_WHITE, "Age");
	Term_putstr(col2, row, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->age));

	/* Height */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Height");
	Term_putstr(col2, row, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->ht));

	/* Weight */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Weight");
	Term_putstr(col2, row, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->wt));

	/* Status */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Status");
	Term_putstr(col2, row, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->sc));

	/* Empty Space */
	++row;

	/* char level */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Level");
	Term_putstr(col2, row, -1, ((p_ptr->lev >= p_ptr->max_lev) ? TERM_L_BLUE : TERM_YELLOW),
			format("%4d", p_ptr->lev));


	/* Left */
	col = 1;
	col2 = col + 10;

	/* Game Turn */
	row = 9;
	Term_putstr(col, row, -1, TERM_WHITE, "GameTurn");
	Term_putstr(col2, row, -1, TERM_L_GREEN, format("%10ld", turn));

	/* Player Turn */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "PlayerTurn");
	Term_putstr(col2, row, -1, TERM_L_GREEN,
		format("%10ld", p_ptr->p_turn));

	/* Current Experience */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Cur Exp");

	if (p_ptr->exp >= p_ptr->max_exp)
	{
		Term_putstr(col2, row, -1, TERM_L_GREEN,
		            format("%10ld", p_ptr->exp));
	}
	else
	{
		Term_putstr(col2, row, -1, TERM_YELLOW,
		            format("%10ld", p_ptr->exp));
	}

	/* Maximum Experience */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Max Exp");
	Term_putstr(col2, row, -1, TERM_L_GREEN,
	            format("%10ld", p_ptr->max_exp));

	/* Advance Experience */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Adv Exp");
	if (p_ptr->lev < z_info->max_level)
	{
		s32b advance = (get_experience_by_level(p_ptr->lev-1) * p_ptr->expfact / 100L);

		/*some players want to see experience needed to gain next level*/
		advance -= p_ptr->exp;
		Term_putstr(col2, row, -1, TERM_L_GREEN,
		            format("%10ld", advance));
	}
	else
	{
		Term_putstr(col2, row, -1, TERM_L_GREEN,
		            format("%10s", "********"));
	}

	/* Max dungeon level */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "MaxDepth");

	/* Has he actually left the town? */
	if (p_ptr->max_depth)
	{
		/*express in feet */
		strnfmt(buf, sizeof(buf), "%5d ft", p_ptr->max_depth * 50);
	}
 	/*hasn't left town*/
	else strnfmt(buf, sizeof(buf), "    Town");

	Term_putstr(col2+2, row, -1, TERM_L_GREEN, buf);

	/* Gold */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Gold");
	Term_putstr(col2, row, -1, TERM_L_GREEN, format("%10ld", p_ptr->au));

	/* Burden (in pounds) */
	++row;

	Term_putstr(col, row, -1, TERM_WHITE, "Burden");

	/*calculate burden as a % of character's max burden*/
	strnfmt(buf, sizeof(buf), "%6ld lbs", p_ptr->total_weight / 10L);
	Term_putstr(col2, row, -1, TERM_L_GREEN, buf);

	/* Now print burden as a percentage of carrying capacity */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "% Burden");
	tmpl = ((p_ptr->total_weight * 10L) / adj_str_wgt[p_ptr->state.stat_ind[A_STR]]) / 10L;

	/*output, but leave a space for the %*/
	strnfmt(buf, sizeof(buf), format("%9ld", tmpl));
	Term_putstr(col2, row, -1, (tmpl < 100L) ? TERM_L_GREEN : TERM_YELLOW, buf);

	/*Hack - add the % at the end*/
	sprintf(buf, "%%");
	Term_putstr(col2+9, row, -1, (tmpl < 100L) ? TERM_L_GREEN : TERM_YELLOW, buf);

	/* Middle */
	col = 23;

	/* Armor */
	base = p_ptr->state.dis_ac;
	plus = p_ptr->state.dis_to_a;

	/* Total Armor */
	row = 9;
	strnfmt(buf, sizeof(buf), "[%d,%+d]", base, plus);
	Term_putstr(col, row, -1, TERM_WHITE, "Armor");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));


	/* Base skill */
	hit = p_ptr->state.dis_to_h;
	dam = p_ptr->state.dis_to_d;

	/* Basic fighting */
	++row;
	strnfmt(buf, sizeof(buf), "(%+d,%+d)", hit, dam);
	Term_putstr(col, row, -1, TERM_WHITE, "Fight");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));


	/* Melee weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Base skill */
	hit = p_ptr->state.dis_to_h;
	dam = p_ptr->state.dis_to_d;

	/* Apply weapon bonuses */
	if (object_known_p(o_ptr)) hit += o_ptr->to_h;
	if (object_known_p(o_ptr)) dam += o_ptr->to_d;

	/* Melee attacks */
	++row;
	strnfmt(buf, sizeof(buf), "(%+d,%+d)", hit, dam);
	Term_putstr(col, row, -1, TERM_WHITE, "Melee");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));


	/* Range weapon */
	if (adult_swap_weapons)
	{
		if (inventory[INVEN_MAIN_WEAPON].tval == TV_BOW) i_ptr = &inventory[INVEN_MAIN_WEAPON];

		/* A bow is not wielded, just set up a "dummy, blank" object and point to that */
		else
		{
			i_ptr = &object_type_body;
			object_wipe(i_ptr);
		}
	}
	else i_ptr = &inventory[INVEN_BOW];

	/* Base skill */
	hit = p_ptr->state.dis_to_h;
	dam = 0;

	/* Apply weapon bonuses */
	if (object_known_p(i_ptr)) hit += i_ptr->to_h;
	if (object_known_p(i_ptr)) dam += i_ptr->to_d;

	/* hack, rogues are deadly with slings*/
	if ((cp_ptr->flags & CF_ROGUE_COMBAT) && (p_ptr->state.ammo_tval == TV_SHOT))
	{
		hit += 3 + p_ptr->lev / 4;
		dam += p_ptr->lev * 2 / 3;
	}

	/* Range attacks */
	++row;
	strnfmt(buf, sizeof(buf), "(%+d,%+d)", hit, dam);
	Term_putstr(col, row, -1, TERM_WHITE, "Shoot");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));


	/* Blows */
	++row;
	strnfmt(buf, sizeof(buf), "%d/turn", p_ptr->state.num_blow);
	Term_putstr(col, row, -1, TERM_WHITE, "Blows");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));


	/* Shots */
	++row;
	strnfmt(buf, sizeof(buf), "%d/turn", p_ptr->state.num_fire);
	Term_putstr(col, row, -1, TERM_WHITE, "Shots");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));

	/*get the player's speed*/
	i = p_ptr->state.p_speed;

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += (game_mode == GAME_NPPMORIA ? 1 : 10);

	/* Hack -- Visually "undo" temp speed */
	if (p_ptr->timed[TMD_FAST]) i -= (game_mode == GAME_NPPMORIA ? 1 : 10);

	/* Hack -- Visually "undo" temp slowing */
	if (p_ptr->timed[TMD_SLOW]) i += (game_mode == GAME_NPPMORIA ? 1 : 10);

	/* Boundry Control */
	if (game_mode == GAME_NPPMORIA)
	{
		moria_speed_labels(buf, i, sizeof(buf));

	}

	/* Fast */
	else if (i > 110)
	{
		sprintf(buf, "+%d", (i - 110));
	}

	/* Slow */
	else if (i < 110)
	{
		sprintf(buf, "-%d", (110 - i));
	}

	else
	{
		 sprintf(buf, "Normal");
	}

	/* Speed */
	++row;
	Term_putstr(col, row, -1, TERM_WHITE, "Speed");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));

	/* Infra */
	++row;
	strnfmt(buf, sizeof(buf), "%d ft", p_ptr->state.see_infra * 10);
	Term_putstr(col, row, -1, TERM_WHITE, "Infra");
	Term_putstr(col+5, row, -1, TERM_L_BLUE, format("%11s", buf));

	/* Right */
	col = 40;

	/* Fighting Skill (with current weapon) */
	tmp = p_ptr->state.to_h + o_ptr->to_h;
	xthn = p_ptr->state.skills[SKILL_TO_HIT_MELEE] + (tmp * BTH_PLUS_ADJ);

	/* Shooting Skill (with current bow) */
	tmp = p_ptr->state.to_h + i_ptr->to_h;
	xthb = p_ptr->state.skills[SKILL_TO_HIT_BOW] + (tmp * BTH_PLUS_ADJ);

	/* Basic abilities */
	xdis = p_ptr->state.skills[SKILL_DISARM];
	xdev = p_ptr->state.skills[SKILL_DEVICE];
	xsav = p_ptr->state.skills[SKILL_SAVE];
	xstl = p_ptr->state.skills[SKILL_STEALTH];
	xsrh = p_ptr->state.skills[SKILL_SEARCH];
	xfos = p_ptr->state.skills[SKILL_SEARCH_FREQUENCY];

	row = 9;
	put_str("Saving Throw", row, col);
	if (xsav < 0) xsav = 0;
	else if (xsav > 100) xsav = 100;
	desc = likert(xsav, 6, &likert_attr);
	c_put_str(likert_attr, format("%8d%%", xsav), row, col+14);

	++row;
	put_str("Stealth", row, col);
	desc = likert(xstl, 1, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), row, col+14);

	++row;
	put_str("Fighting", row, col);
	desc = likert(xthn, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), row, col+14);

	++row;
	put_str("Shooting", row, col);
	desc = likert(xthb, 12, &likert_attr);
	c_put_str(likert_attr, format("%9s", desc), row, col+14);

	++row;
	put_str("Disarming", row, col);
	if (p_ptr->timed[TMD_BLIND] || no_light()) xdis = xdis / 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) xdis = xdis / 10;
	if (xdis < 0) xdis = 0;
	else if (xdis > 100) xdis = 100;
	desc = likert(xdis, 8, &likert_attr);
	c_put_str(likert_attr, format("%8d%%", xdis), row, col+14);

	++row;
	put_str("Magic Device", row, col);
	desc = likert(xdev, 6, &likert_attr);
	c_put_str(likert_attr, format("%9d", xdev), row, col+14);

	++row;
	put_str("Perception", row, col);
	desc = likert(xfos, 6, &likert_attr);
	if (xfos > 50) desc = "1 in 1";
	else desc = format("1 in %d", (50 - xfos));
	c_put_str(likert_attr, format("%9s", desc), row, col+14);

	++row;
	put_str("Searching", row, col);
	if (xsrh < 0) xsrh = 0;
	else if (xsrh > 100) xsrh = 100;
	desc = likert(xsrh, 6, &likert_attr);
	c_put_str(likert_attr, format("%8d%%", xsrh), row, col+14);

	/* Indent output by 1 character, and wrap at column 72 */
	text_out_wrap = 65;
	text_out_indent = 1;

	/* History */
	Term_gotoxy(text_out_indent, 19);
	text_out_to_screen(TERM_WHITE, p_ptr->history);

	/* Reset text_out() vars */
	text_out_wrap = 0;
	text_out_indent = 0;
}


/*
 * Obtain the "flags" for the player as if he was an item
 */
void player_flags(u32b *f1, u32b *f2, u32b *f3, u32b *fn)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = (*fn) = 0L;

	/* Add racial flags */
	(*f1) |= rp_ptr->pr_flags1;
	(*f2) |= rp_ptr->pr_flags2;
	(*f3) |= rp_ptr->pr_flags3;
	(*fn) |= rp_ptr->pr_native;
	(*fn) |= cp_ptr->c_native;

	if (cp_ptr->flags & CF_BRAVERY_30)
	{
		if (p_ptr->lev >= LEV_BRAVERY) (*f2) |= (TR2_RES_FEAR);
	}

	/* Brigand's poison resistance */
	if (cp_ptr->flags & CF_BRIGAND_COMBAT)
	{
		if (p_ptr->lev >= LEV_RES_POIS) (*f2) |= (TR2_RES_POIS);
	}
}


/*
 * List of resistances and abilities to display
 */
#define RES_ROWS 8

struct player_flag_record
{
	const char name[7];		/* Name of resistance/ability */
	byte set;				/* Which field this resistance is in { 1 2 3 } */
	u32b res_flag;			/* resistance flag bit */
	u32b im_flag;			/* corresponding immunity bit, if any */
	bool moria_flag;		/* Is it used in Moria? */
};

static const struct player_flag_record player_flag_table[RES_ROWS*4] =
{
	{ "rAcid",	2, TR2_RES_ACID,	TR2_IM_ACID, TRUE},
	{ "rElec",	2, TR2_RES_ELEC,	TR2_IM_ELEC, TRUE},
	{ "rFire",	2, TR2_RES_FIRE,	TR2_IM_FIRE, TRUE},
	{ "rCold",	2, TR2_RES_COLD,	TR2_IM_COLD, TRUE},
	{ "rPois",	2, TR2_RES_POIS,	TR2_IM_POIS, FALSE},
	{ "rFear",	2, TR2_RES_FEAR,	0, FALSE},
	{ "rLite",	2, TR2_RES_LIGHT,	0, FALSE},
	{ "rDark",	2, TR2_RES_DARK,	0, FALSE},

	{ "rBlnd",	2, TR2_RES_BLIND,	0, TRUE},
	{ "rConf",	2, TR2_RES_CONFU,	0, FALSE},
	{ "Sound",	2, TR2_RES_SOUND,	0, FALSE},
	{ "Shard",	2, TR2_RES_SHARD,	0, FALSE},
	{ "Nexus",	2, TR2_RES_NEXUS,	0, FALSE},
	{ "Nethr",	2, TR2_RES_NETHR,	0, FALSE},
	{ "Chaos",	2, TR2_RES_CHAOS,	0, FALSE},
	{ "Disen",	2, TR2_RES_DISEN,	0, FALSE},

	{ "S.Dig",	3, TR3_SLOW_DIGEST,	0, TRUE},
	{ "Feath",	3, TR3_FEATHER, 	0, TRUE},
	{ "PLite",	3, TR3_LIGHT, 		0, TRUE},
	{ "Regen",	3, TR3_REGEN, 		0, TRUE},
	{ "Telep",	3, TR3_TELEPATHY, 	0, TRUE},
	{ "Invis",	3, TR3_SEE_INVIS, 	0, TRUE},
	{ "FrAct",	3, TR3_FREE_ACT, 	0, TRUE},
	{ "HLife",	3, TR3_HOLD_LIFE, 	0, TRUE},

	{ "Stea.",	1, TR1_STEALTH,		0, TRUE},
	{ "Sear.",	1, TR1_SEARCH,		0, TRUE},
	{ "Infra",	1, TR1_INFRA,		0, TRUE},
	{ "Aggr",	3, TR3_AGGRAVATE,	0, TRUE},
	{ "Speed",	1, TR1_SPEED,		0, TRUE},
	{ "Blows",	1, TR1_BLOWS,		0, TRUE},
	{ "Shots",	1, TR1_SHOTS,		0, TRUE},
	{ "Might",	1, TR1_MIGHT,		0, TRUE}
};

/*
 * Special display, part 1
 */
static void display_player_flag_info(bool onscreen)
{
	int x, y, i, n;

	int row, col;

	u32b f1, f2, f3, fn;

	byte record = 0;

	/* Four columns */
	for (x = 0; x < 4; x++)
	{
		/* Reset */
		row = 11;
		col = 20 * x - 2;

		/* Header */
		c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col+8);

		/* Eight rows */
		for (y = 0; y < 8; y++)
		{
			byte name_attr = TERM_WHITE;
			const struct player_flag_record *pfr_ptr = &player_flag_table[record++];
			u32b flag_used;

			/* Don't display flags unused in Moria */
			if (game_mode == GAME_NPPMORIA)
			{
				if(!pfr_ptr->moria_flag)
				{
					row++;
					continue;
				}
			}

			/* Check equipment */
			for (n = 8, i = INVEN_WIELD; i < INVEN_TOTAL; ++i, ++n)
			{
				byte attr = TERM_SLATE;
				char o_name[80];
				object_type *o_ptr;

				/* Object */
				o_ptr = &inventory[i];

				if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON))
				{
					c_put_str(TERM_L_DARK, ".", row, col+n);
					continue;
				}

				object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

				/* Known flags */
				object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

				/* get the flag */
				if 		(pfr_ptr->set == 1) flag_used = f1;
				else if (pfr_ptr->set == 2) flag_used = f2;
				else if (pfr_ptr->set == 3) flag_used = f3;
				else/*(pfr_ptr->set == 4)*/ flag_used = fn;

				/* Color columns by parity */
				if (i % 2) attr = TERM_L_WHITE;

				/* Non-existent objects */
				if (!o_ptr->k_idx) attr = TERM_L_DARK;

				/* First check immunities */
				if ((pfr_ptr->im_flag) && (flag_used & (pfr_ptr->im_flag)))
				{
					c_put_str(TERM_L_GREEN, "*", row, col+n);
					name_attr = TERM_L_GREEN;
				}

				/* Check flags */
				else if (flag_used & (pfr_ptr->res_flag))
				{
					c_put_str(TERM_L_BLUE, "+", row, col+n);
					if (name_attr != TERM_L_GREEN) name_attr = TERM_L_BLUE;
				}

				/* Default */
				else
				{
					c_put_str(attr, ".", row, col+n);
				}
			}

			/* Player flags */
			player_flags(&f1, &f2, &f3, &fn);

			/* get the flag */
			if 		(pfr_ptr->set == 1) flag_used = f1;
			else if (pfr_ptr->set == 2) flag_used = f2;
			else if (pfr_ptr->set == 3) flag_used = f3;
			else /*(pfr_ptr->set == 4)*/flag_used = fn;

			/* Default */
			c_put_str(TERM_SLATE, ".", row, col+n);

			/* Hack -- Check immunities */
			if ((pfr_ptr->im_flag) && (flag_used & (pfr_ptr->im_flag)))
			{
				c_put_str(TERM_L_GREEN, "*", row, col+n);
				name_attr = TERM_L_GREEN;
			}

			/* Check flags */
			else if (flag_used & (pfr_ptr->res_flag))
			{
			    c_put_str(TERM_L_BLUE, "+", row, col+n);
				if (name_attr != TERM_L_GREEN) name_attr = TERM_L_BLUE;
			}

			/* Header */
			c_put_str(name_attr, pfr_ptr->name, row, col+2);

			/* Advance */
			row++;
		}

		/* Footer */
		c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col+8);

		/* Equippy */
		display_player_equippy(row++, col+8, onscreen);
	}
}


/*
 * Special display, part 2a
 */
static void display_player_misc_info(int row, int col)
{
	cptr p;

	char buf[80];

	int col2 = col + 7;

	/* Name */
	put_str("Name", row, col);
	c_put_str(TERM_L_BLUE, op_ptr->full_name, row, col2);

	/* Sex */
	++row;
	put_str("Sex", row, col);
	c_put_str(TERM_L_BLUE, sp_ptr->title, row, col2);


	/* Race */
	++row;
	put_str("Race", row, col);
	c_put_str(TERM_L_BLUE, p_name + rp_ptr->name, row, col2);


	/* Class */
	++row;
	put_str("Class", row, col);
	c_put_str(TERM_L_BLUE, c_name + cp_ptr->name, row, col2);


	/* Title */
	++row;
	put_str("Title", row, col);

	/* Wizard */
	if (p_ptr->wizard)
	{
		p = "[=-WIZARD-=]";
	}

	/* Winner */
	else if (p_ptr->total_winner || (p_ptr->lev > z_info->max_level))
	{
		p = "**WINNER**";
	}

	/* Normal */
	else
	{
		p = get_player_title();
	}

	/* Dump it */
	c_put_str(TERM_L_BLUE, p, row, col2);


	/* Hit Points */
	++row;
	put_str("HP", row, col);
	strnfmt(buf, sizeof(buf), "%d/%d", p_ptr->chp, p_ptr->mhp);
	c_put_str(TERM_L_BLUE, buf, row, col2);


	/* Spell Points */
	++row;
	put_str("SP", row, col);
	strnfmt(buf, sizeof(buf), "%d/%d", p_ptr->csp, p_ptr->msp);
	c_put_str(TERM_L_BLUE, buf, row, col2);
}


/*
 * Special display, part 2b
 */
void display_player_stat_info(int row, int col)
{
	int i;

	char buf[80];

	/* Print out the labels for the columns */
	c_put_str(TERM_WHITE, "  Self", row-1, col+5);
	/* Don't print stat modifiers stats aren't preserved */
	if (adult_maximize)
	{
		c_put_str(TERM_WHITE, " RB", row-1, col+11);
		c_put_str(TERM_WHITE, " CB", row-1, col+14);
	}
	c_put_str(TERM_WHITE, " EB", row-1, col+18);
	c_put_str(TERM_WHITE, "  Best", row-1, col+22);

	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Reduced */
		if (p_ptr->state.stat_use[i] < p_ptr->state.stat_top[i])
		{
			/* Use lowercase stat name */
			put_str(stat_names_reduced[i], row+i, col);
		}

		/* Normal */
		else
		{
			/* Assume uppercase stat name */
			put_str(stat_names[i], row+i, col);
		}

		/* Indicate natural maximum */
		if (p_ptr->stat_max[i] == 18+100)
		{
			put_str("!", row+i, col+3);
		}

		/* Internal "natural" maximum value */
		cnv_stat(p_ptr->stat_max[i], buf, sizeof(buf));
		c_put_str(TERM_L_GREEN, buf, row+i, col+5);

		/* Don't print stat modifiers stats aren't preserved */
		if (adult_maximize)
		{

			/* Race Bonus add in permanent stat bonus here */
			strnfmt(buf, sizeof(buf), "%+3d", (rp_ptr->r_adj[i] + p_ptr->stat_quest_add[i]));
			c_put_str(TERM_L_BLUE, buf, row+i, col+11);

			/* Class Bonus */
			strnfmt(buf, sizeof(buf), "%+3d", cp_ptr->c_adj[i]);
			c_put_str(TERM_L_BLUE, buf, row+i, col+14);
		}

		/* Equipment Bonus */
		strnfmt(buf, sizeof(buf), "%+3d", p_ptr->state.stat_add[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, col+18);

		/* Resulting "modified" maximum value */
		cnv_stat(p_ptr->state.stat_top[i], buf, sizeof(buf));
		c_put_str(TERM_L_GREEN, buf, row+i, col+22);

		/* Only display stat_use if not maximal */
		if (p_ptr->state.stat_use[i] < p_ptr->state.stat_top[i])
		{
			cnv_stat(p_ptr->state.stat_use[i], buf, sizeof(buf));
			c_put_str(TERM_YELLOW, buf, row+i, col+28);
		}
	}
}


/*
 * Special display, part 2c
 *
 * How to print out the modifications and sustains.
 * Positive mods with no sustain will be light green.
 * Positive mods with a sustain will be dark green.
 * Sustains (with no modification) will be a dark green 's'.
 * Negative mods (from a curse) will be red.
 * Huge mods (>9), like from MICoMorgoth, will be a '*'
 * No mod, no sustain, will be a slate '.'
 */
static void display_player_sust_info(bool onscreen)
{
	int i, row, col, stats;

	object_type *o_ptr;
	u32b f1, f2, f3, fn;
	u32b ignore_f2, ignore_f3, ignore_fn;

	byte a;
	char c;

	/* Row */
	row = 3;

	/* Column */
	col = 19;

	/* Header */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col);

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Get the "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		/* Hack -- assume stat modifiers are known */
		object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3, &ignore_fn);

		/* Initialize color based of sign of pval. */
		for (stats = 0; stats < A_MAX; stats++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';

			if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON))
			{
				Term_putch(col, row+stats, a, c);
				continue;
			}

			if (object_known_p(o_ptr))
			{

				/* Boost */
				if (f1 & (1<<stats))
				{
					/* Default */
					c = '*';

					/* Good */
					if (o_ptr->pval > 0)
					{
						/* Good */
						a = TERM_L_GREEN;

						/* Label boost */
						if (o_ptr->pval < 10) c = I2D(o_ptr->pval);
					}

					/* Bad */
					if (o_ptr->pval < 0)
					{
						/* Bad */
						a = TERM_RED;

						/* Label boost */
						if (o_ptr->pval > -10) c = I2D(-(o_ptr->pval));
					}
				}


				/* Sustain */
				if (f2 & (1<<stats))
				{
					/* Dark green */
					a = TERM_GREEN;

					/* Convert '.' to 's' */
					if (c == '.') c = 's';
				}
			}

			/* Dump proper character */
			Term_putch(col, row+stats, a, c);
		}

		/* Advance */
		col++;
	}

	/* Player flags */
	player_flags(&f1, &f2, &f3, &fn);

	/* Check stats */
	for (stats = 0; stats < A_MAX; ++stats)
	{
		/* Default */
		a = TERM_SLATE;
		c = '.';

		/* Sustain */
		if (f2 & (1<<stats))
		{
			/* Dark green "s" */
			a = TERM_GREEN;
			c = 's';
		}

		/* Dump */
		Term_putch(col, row+stats, a, c);
	}

	/* Column */
	col = 19;

	/* Footer */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row+6, col);

	/* Equippy */
	display_player_equippy(row+7, col, onscreen);
}

/*
 * Special display, part 3
 *
 * Dump all info for the home equipment
 * Positive mods with no sustain will be light green.
 * Positive mods with a sustain will be dark green.
 * Sustains (with no modification) will be a dark green 's'.
 * Negative mods (from a curse) will be red.
 * Huge mods (>9), like from MICoMorgoth, will be a '*'
 * No mod, no sustain, will be a slate '.'
 * This is followed by the home equipment
 *
 */
static void display_home_equipment_info(int mode, bool onscreen)
{
	int x, y, n, xmax, xmin;

	int i, row, col, stats;

	object_type *o_ptr;
	u32b f1, f2, f3, fn;
	u32b ignore_f2, ignore_f3, ignore_fn;

	byte a;
	char c;

	store_type *st_ptr = &store[STORE_HOME];

	/* Row */
	row = 3;

	/* Column */
	col = 7;

	/* Equippy */
	display_home_equippy(row-2, col, onscreen);

	/* Header */
	display_home_inven_letters(row-1, col, onscreen);

	/* Footer */
	display_home_inven_letters(row+6, col, onscreen);

	/* Process home stats */
	for (i = 0; i < MAX_INVENTORY_HOME; ++i)
	{
		/* Object */
		o_ptr = &st_ptr->stock[i];

		/* Get the "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		/* Hack -- assume stat modifiers are known */
		object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3, &ignore_fn);

		/* Initialize color based of sign of pval. */
		for (stats = 0; stats < A_MAX; stats++)
		{
			/* Assume uppercase stat name */
			c_put_str(TERM_WHITE, stat_names[stats], row+stats, 2);

			/* Default */
			a = TERM_SLATE;
			c = '.';

			if (object_known_p(o_ptr))
			{

				/* Boost */
				if (f1 & (1<<stats))
				{
					/* Default */
					c = '*';

					/* Good */
					if (o_ptr->pval > 0)
					{
						/* Good */
						a = TERM_L_GREEN;

						/* Label boost */
						if (o_ptr->pval < 10) c = I2D(o_ptr->pval);
					}

					/* Bad */
					if (o_ptr->pval < 0)
					{
						/* Bad */
						a = TERM_RED;

						/* Label boost */
						if (o_ptr->pval > -10) c = I2D(-(o_ptr->pval));
					}
				}

				/* Sustain */
				if (f2 & (1<<stats))
				{
					/* Dark green */
					a = TERM_GREEN;

					/* Convert '.' to 's' */
					if (c == '.') c = 's';
				}
			}

			/* Dump proper character */
			Term_putch(col, row+stats, a, c);
		}

		/* Advance */
		col++;
	}

	/*alternate between resists and abilities depending on the modes*/
	if (mode == 2)
	{
		xmin = 0;
		xmax = 2;
	}

	else if (mode == 3)
	{
		xmin = 2;
		xmax = 4;
	}

	/*paranoia*/
	else xmax = xmin = 0;

	/* Row */
	row = 10;

	/* Re-Set Column */
	col = 7;

	/* 2nd Header */
	display_home_inven_letters((row-1), (col + MAX_INVENTORY_HOME + 8), onscreen);

	/* Footer */
	display_home_inven_letters((row+8), col, onscreen);

	/* 2nd Footer */
	display_home_inven_letters((row+8), (col + MAX_INVENTORY_HOME + 8), onscreen);

	/* 3rd Equippy */
	display_home_equippy(row+9, col, onscreen);

	/* 4th Equippy */
	display_home_equippy(row+9,col+ MAX_INVENTORY_HOME + 8, onscreen);

	/* Two Rows, alternating depending upon the mode */
	for (x = 0; xmin < xmax; ++xmin, ++x)
	{
		/* Reset */
		col = 32 * x;
		row = 10;

		/* Eight rows */
		for (y = 0; y < 8; y++)
		{
			byte name_attr = TERM_WHITE;
			int z = (x * 8) + y + ((mode == 2) ? 0 : 16);
			const struct player_flag_record *pfr_ptr = &player_flag_table[z];
			u32b flag_used;

			/* Don't display flags unused in Moria */
			if (game_mode == GAME_NPPMORIA)
			{
				if(!pfr_ptr->moria_flag)
				{
					row++;
					continue;
				}
			}

			/* Check equipment */
			for (n = 7, i = 0; i < MAX_INVENTORY_HOME; ++i, ++n)
			{
				byte attr = TERM_SLATE;

				object_type *o_ptr;

				f1 = 0L;
				f2 = 0L;
				f3 = 0L;
				fn = 0L;

				/* Object */
				o_ptr = &st_ptr->stock[i];

				/* Known flags */
				object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

				/* get the flag */
				if 		(pfr_ptr->set == 1) flag_used = f1;
				else if (pfr_ptr->set == 2) flag_used = f2;
				else if (pfr_ptr->set == 3) flag_used = f3;
				else/*(pfr_ptr->set == 3)*/ flag_used = fn;

				/* Color columns by parity */
				if (i % 2) attr = TERM_L_WHITE;

				/* Non-existent objects */
				if (!o_ptr->k_idx) attr = TERM_L_DARK;

				/* First check immunities */
				if ((pfr_ptr->im_flag) && (flag_used & (pfr_ptr->im_flag)))
				{
					c_put_str(TERM_L_GREEN, "*", row, col+n);
					name_attr = TERM_L_GREEN;
				}

				/* Check flags */
				else if (flag_used & (pfr_ptr->res_flag))
				{
					c_put_str(TERM_L_BLUE, "+", row, col+n);
					if (name_attr != TERM_L_GREEN) name_attr = TERM_L_BLUE;
				}

				/* Default */
				else
				{
					c_put_str(attr, ".", row, col+n);
				}
			}

			/* Header */
			c_put_str(name_attr, pfr_ptr->name, row, col);

			/* Advance */
			row++;
		}
	}
}

/* Array of element pairs (name + feature flags) */
static struct
{
	const char *name;
	u32b flags;
} element_table[] =
{
	{"acid", ELEMENT_ACID},
	{"fire", ELEMENT_FIRE},
	{"forest", ELEMENT_FOREST},
	{"ice", ELEMENT_ICE},
	{"lava", ELEMENT_LAVA},
	{"mud", ELEMENT_MUD},
	{"oil", ELEMENT_OIL},
	{"sand", ELEMENT_SAND},
	{"water", ELEMENT_WATER},
	{"boiling water", ELEMENT_BWATER},
	{"boiling mud", ELEMENT_BMUD},
	{NULL, 0}
};


/*
 * Display the character on the screen (four different modes)
 * Shows a screen with the character's special abilities and temporary bonuses
 */
static void display_special_abilities(int row, int col)
{
	int i;
	const char *msg;
	int old_row;
	int col2;       /* The current column */
	const char *items[22];
	int n;

	/* Print the title */
	c_put_str(TERM_L_GREEN, "[Special abilities and temporary bonuses]", row, col);

	row += 2;

	/* Save this row for later */
	old_row = row;

	/* Reset item counter */
	n = 0;

	/* Collect nativity info */
	for (i = 0; element_table[i].name != NULL; i++)
	{
		u32b flags;

		if (element_table[i].name[0] == '\0') continue;

		flags = element_table[i].flags;

		if (!flags) continue;

		if ((p_ptr->p_native_known & flags) != flags) continue;

		items[n++] = element_table[i].name;
	}


	/* Print nativity info */
	if (n > 0)
	{
		col2 = col;

		msg = "You are native to ";

		put_str(msg, row, col2);

		col2 += strlen(msg);

		for (i = 0; i < n; i++)
		{
			/* Print separators */
			if (i > 0)
			{
				if (i < (n - 1))
				{
					put_str(", ", row, col2);

					col2 += 2;
				}
				else
				{
					put_str(" and ", row, col2);

					col2 += 5;
				}
			}

			msg = items[i];

			c_put_str(TERM_L_GREEN, msg, row, col2);

			col2 += strlen(msg);
		}
		/* Punctuation */
		put_str(".", row, col2);

		++row;
	}

	/* Print temporary bonus to infravision */
	if (p_ptr->timed[TMD_SINFRA] > 0)
	{
		msg = "Your magic enhances your ";

		put_str(msg, row, col);

		c_put_str(TERM_ORANGE, "infravision", row++, col + strlen(msg));
	}

	/* Print temporary "see invisible" spell */
	if (p_ptr->timed[TMD_SINVIS] > 0)
	{
		col2 = col;

		msg = "You can see ";

		put_str(msg, row, col2);

		col2 += strlen(msg);

		msg = "invisible";

		c_put_str(TERM_ORANGE, msg, row, col2);

		col2 += strlen(msg);

		put_str(" foes for some time", row++, col2);
	}

	/* Print temporary heroism */
	if (p_ptr->timed[TMD_HERO] > 0)
	{
		msg = "You feel ";

		put_str(msg, row, col);

		c_put_str(TERM_L_BLUE, "heroic", row++, col + strlen(msg));
	}

	/* Print temporary chant */
	if (p_ptr->timed[TMD_BLESSED] > 0)
	{
		col2 = col;

		msg = "You are ";

		put_str(msg, row, col2);

		col2 += strlen(msg);

		msg = "praying";

		c_put_str(TERM_L_BLUE, msg, row, col2);

		col2 += strlen(msg);

		put_str(" to your gods", row++, col2);
	}

	/* Print temporary berserk rage */
	if (p_ptr->timed[TMD_SHERO] > 0)
	{
		msg = "Your blood boils in ";

		put_str(msg, row, col);

		c_put_str(TERM_L_RED, "berserk rage", row++, col + strlen(msg));
	}

	/* Print temporary protection from evil */
	if (p_ptr->timed[TMD_PROTEVIL] > 0)
	{
		col2 = col;

		msg = "You feel protected against ";

		put_str(msg, row, col2);

		col2 += strlen(msg);

		msg = "evil";

		c_put_str(TERM_L_BLUE, msg, row, col2);

		col2 += strlen(msg);

		put_str(" forces", row++, col2);
	}

	/* Print flying */
	if (p_ptr->timed[TMD_FLYING] > 0)
	{
		msg = "You are ";

		put_str(msg, row, col);

	c_put_str(TERM_BLUE, "flying", row++, col + strlen(msg));
	}

	/* Print temporary shield protection */
	if (p_ptr->timed[TMD_SHIELD] > 0)
	{
		msg = "Your body is protected by a magical ";

		put_str(msg, row, col);

		c_put_str(TERM_YELLOW, "shield", row++, col + strlen(msg));
	}

	/* Print temporary elemental brand */
	if (p_ptr->timed[TMD_SLAY_ELEM] > 0)
	{
		msg = "Your weapon glows with ";

		put_str(msg, row, col);

		c_put_str(TERM_L_GREEN, "elemental brands", row++, col + strlen(msg));
	}

	/* Print temporary speed */
	if (p_ptr->timed[TMD_FAST] > 0)
	{
		msg = "Your feet are temporarily ";

		put_str(msg, row, col);

		c_put_str(TERM_VIOLET, "hasted", row++, col + strlen(msg));
	}

	/* Reset item counter */
	n = 0;

	/* Collect resistances */
	if (p_ptr->timed[TMD_OPP_ACID] > 0) items[n++] = "acid";
	if (p_ptr->timed[TMD_OPP_COLD] > 0) items[n++] = "cold";
	if (p_ptr->timed[TMD_OPP_ELEC] > 0) items[n++] = "electricity";
	if (p_ptr->timed[TMD_OPP_FIRE] > 0) items[n++] = "fire";
	if (p_ptr->timed[TMD_OPP_POIS] > 0) items[n++] = "poison";

	/* Print resistances */
	if (n > 0)
	{
		col2 = col;

		msg = "You have magical resistance to ";

		put_str(msg, row, col2);

		col2 += strlen(msg);

		for (i = 0; i < n; i++)
		{
			/* Print separators */
			if (i > 0)
			{
				if (i < (n - 1))
				{
					put_str(", ", row, col2);

					col2 += 2;
				}
				else
				{
					put_str(" and ", row, col2);

					col2 += 5;
				}
			}

			msg = items[i];

			c_put_str(TERM_YELLOW, msg, row, col2);

			col2 += strlen(msg);
		}

		++row;
	}

	/* No special abilities were shown. Display a message */
	if (row == old_row)
	{
		c_put_str(TERM_RED, "You don't have any", row, col);
	}
}


/*
 * Display the character on the screen (five different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank
 * in the first two modes.
 *
 * Mode 0 = standard display with skills/history
 * Mode 1 = special display with equipment flags
 * Mode 2 = Home equipment Stat Flags and 1st part of Resists
 * Mode 3 = Home equipment Stat Flags and 2st part of Resists
 * Mode 4 = Special abilities (nativity) and temporary bonuses
 *
 * The boolean onscreen specifies if this is to display onscreen
 * or to be written to a file.  To make sure the equippy is displayed properly
 * in a file while in tile mode.
 */
void display_player(int mode, bool onscreen)
{
	/* Erase screen */
	clear_from(0);

	/* All Modes Except #4 Use Stat info */
	if (mode != 4)
	{
		if (mode == 0)
		{
			display_player_stat_info(2, 35);
		}
		else
		{
			display_player_stat_info(3, 33);
		}
	}

	if ((mode) < 2)
	{

		/* Special */
		if (mode)
		{

			/* Misc info */
			display_player_misc_info(2, 1);

			/* Hack -- Level */
			put_str("Level", 9, 1);
			c_put_str(TERM_L_BLUE, format("%d", p_ptr->lev), 9, 8);

			/* Stat/Sustain flags */
			display_player_sust_info(onscreen);

			/* Other flags */
			display_player_flag_info(onscreen);
		}

		/* Standard */
		else
		{

			/* Misc info */
			display_player_misc_info(1, 1);

			/* Extra info */
			display_player_xtra_info();
		}
	}

	 /* Special abilities and temporary bonuses */
	else if (mode == 4)
	{
		display_special_abilities(1, 1);
	}

	else display_home_equipment_info(mode, onscreen);
}


static void dump_player_plus_minus(ang_file *fff)
{
	int i, stats, modifier;

	u32b f1, f2, f3, fn;

	object_type *o_ptr;

	/* Print it out */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
	{

		/* Object */
		o_ptr = &inventory[i];

		if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON))
		{
			file_putf(fff," ");
			continue;
		}

		/* Get the "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		modifier = FALSE;

		if (object_known_p(o_ptr))
		{

			/* check to see if there is an increase or decrease of a stat */
			for (stats = 0; stats < A_MAX; stats++)
			{
				/* Boost */
				if (f1 & (1<<stats)) modifier = TRUE;
			}
		}

		if (modifier)
		{
			/* positive pval */
			if (o_ptr->pval > 0)
			{
				/*Make it a space*/
				file_putf(fff,"+");

			}

			/* negative pval */
			else if (o_ptr->pval < 0)
			{
				/*Make it a space*/
				file_putf(fff,"-");

			}
		}

		/* Just a space */
		else file_putf(fff," ");
	}
}


static void dump_player_stat_info(ang_file *fff)
{
	int i, x, y, stats;

	u32b f1, f2, f3, fn;
	u32b ignore_f2, ignore_f3, ignore_fn;

	object_type *o_ptr;

	char c;

	char equippy[20];

	/* Build the equippy */
	for (x = 0,i = INVEN_WIELD; i < INVEN_TOTAL; ++i,++x)
	{

		/* Object */
		o_ptr = &inventory[i];

		if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON))
		{
			equippy[x] = ' ';
			continue;
		}

		/* empty objects */
		if (!o_ptr->k_idx)
		{
			/*Make it a space*/
			equippy[x] = ' ';

		}

		/* Get attr/char for display */
		else equippy[x] = object_char_default(o_ptr);
	}

	/*finish off the string*/
	equippy[x] = '\0';

	/*Hack - record spaces for the character*/

	file_putf(fff,"(+/-) ");

	dump_player_plus_minus(fff);

	file_putf(fff,"\n      %s               %s\n",equippy, equippy);

	file_putf(fff,"      abcdefghijkl@              abcdefghijkl@\n");

	for (stats = 0; stats < A_MAX; stats++)
	{
		file_putf(fff, "%6s", stat_names_reduced[stats]);

		/* Process equipment, show stat modifiers */
		for (x = 0, y = INVEN_WIELD; y < INVEN_TOTAL; ++y, ++x)
		{
			char c = '.';

			object_type *o_ptr;

			/* Get the object */
			o_ptr = &inventory[y];

			if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON))
			{
				/*dump the result*/
				file_putf(fff,".");
				continue;
			}

			/* Get the "known" flags */
			object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

			/* Hack -- assume stat modifiers are known */
			object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3, &ignore_fn);

			/* Boost */
			if (f1 & (1<<stats))
			{
				/* Default */
				c = '*';

				/* Good */
				if (o_ptr->pval > 0)
				{

					/* Label boost */
					if (o_ptr->pval < 10) c = I2D(o_ptr->pval);

				}

				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Label boost */
					if (o_ptr->pval > -10) c = I2D(-(o_ptr->pval));

				}
				if (o_ptr->pval == 0) c = '.';

			}
			/*dump the result*/
			file_putf(fff,"%c",c);
		}

		/*a couple spaces, then do the sustains*/
		file_putf(fff, ".      %7s ", stat_names_reduced[stats]);

		/* Process equipment, show stat modifiers */
		for (y = INVEN_WIELD; y < INVEN_TOTAL; ++y)
		{
			/* Get the object */
			object_type *o_ptr = &inventory[y];

			if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON))
			{
				file_putf(fff,".");
				continue;
			}

			/* Get the "known" flags */
			object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

			/* Hack -- assume stat modifiers are known */
			object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3, &ignore_fn);

			/* Sustain */
			if (f2 & (1<<stats))  c = 's';
			else c = '.';

			/*dump the result*/
			file_putf(fff,"%c",c);
		}

		/* Player flags */
		player_flags(&f1, &f2, &f3, &fn);

		/*default*/
		c = '.';

		/* Sustain */
		if (f2 & (1<<stats)) c = 's';

		/*dump the result*/
		file_putf(fff,"%c",c);

		file_putf(fff,"\n");
	}

}


static void dump_home_plus_minus(ang_file *fff)
{
	int i, stats, modifier;

	u32b f1, f2, f3, fn;

	object_type *o_ptr;
	store_type *st_ptr = &store[STORE_HOME];

	/* Print it out */
	for (i = 0; i < MAX_INVENTORY_HOME; ++i)
	{
		/* Object */
		o_ptr = &st_ptr->stock[i];

		/* Get the "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		modifier = FALSE;

		if (object_known_p(o_ptr))
		{

			/*check to see if there is an increase or decrease of a stat*/
			for (stats = 0; stats < A_MAX; stats++)
			{
				/* Boost */
				if (f1 & (1<<stats)) modifier = TRUE;
			}
		}

		if (modifier)
		{
			/* positive pval */
			if (o_ptr->pval > 0)
			{
				/*Make it a space*/
				file_putf(fff,"+");

			}

			/* negative pval */
			else if (o_ptr->pval < 0)
			{
				/*Make it a space*/
				file_putf(fff,"-");

			}
		}

		/* Just a space */
		else file_putf(fff," ");
	}
}


static void dump_home_stat_info(ang_file *fff)
{
	int i, stats;

	object_type *o_ptr;
	store_type *st_ptr = &store[STORE_HOME];
	u32b f1, f2, f3, fn;
	u32b ignore_f2, ignore_f3, ignore_fn;

	char c;
	char equippy[30];

	/* Print it out */
	for (i = 0; i < MAX_INVENTORY_HOME; ++i)
	{
		/* Object */
		o_ptr = &st_ptr->stock[i];

		/* empty objects */
		if (!o_ptr->k_idx)
		{
			/*Make it a space*/
			equippy[i] = ' ';
		}

		/* Get attr/char for display */
		else equippy[i] = object_char_default(o_ptr);
	}

	/*finish off the string*/
	equippy[i] = '\0';

	/*Hack - record spaces for the character*/

	file_putf(fff,"(+/-)  ");

	dump_home_plus_minus(fff);

	file_putf(fff,"\n       %s        %s\n", equippy, equippy);

	file_putf(fff,"       abcdefghijklmnopqrstuvwx        abcdefghijklmnopqrstuvwx\n");

	for (stats = 0; stats < A_MAX; stats++)
	{
		file_putf(fff, "%6s ", stat_names_reduced[stats]);

		/* Process home stats */
		for (i = 0; i < MAX_INVENTORY_HOME; ++i)
		{
			/* Object */
			o_ptr = &st_ptr->stock[i];

			/* Get the "known" flags */
			object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

			/* Hack -- assume stat modifiers are known */
			object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3, &ignore_fn);

			c = '.';

			/* Boost */
			if (f1 & (1<<stats))
			{
				/* Default */
				c = '*';

				/* Good */
				if (o_ptr->pval > 0)
				{
					/* Label boost */
					if (o_ptr->pval < 10) c = I2D(o_ptr->pval);
				}

				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Label boost */
					if (o_ptr->pval > -10) c = I2D(-(o_ptr->pval));
				}
				if (o_ptr->pval == 0) c = '.';

			}
			/*dump the result*/
			file_putf(fff,"%c",c);
		}

		/*a couple spaces, then do the sustains*/
		file_putf(fff, "  %5s ", stat_names_reduced[stats]);

		/* Process equipment, show stat modifiers */
		for (i = 0; i < MAX_INVENTORY_HOME; ++i)
		{
			/* Object */
			o_ptr = &st_ptr->stock[i];

			/* Get the "known" flags */
			object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

			/* Hack -- assume stat modifiers are known */
			object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3, &ignore_fn);

			/* Sustain */
			if (f2 & (1<<stats))  c = 's';
			else c = '.';

			/*dump the result*/
			file_putf(fff,"%c",c);
		}

		file_putf(fff, "\n");
	}
}


/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(const char *path, bool full)
{
	int i, w, x, y, z;

	byte a;
	char c;

	ang_file *fff;

	store_type *st_ptr = &store[STORE_HOME];

	byte (*old_xchar_hook)(byte c) = Term->xchar_hook;

	char o_name[80];

	char buf[1024];

	/* We use either ASCII or system-specific encoding */
 	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;

	/* Unused parameter */
	(void)full;

	/* Open the file for writing */
	fff = file_open(path, MODE_WRITE, FTYPE_TEXT);
	if (!fff) return (-1);


	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Display the requested encoding -- ASCII or system-specific */
 	if (!xchars_to_file) Term->xchar_hook = NULL;

	/* Begin dump */
	file_putf(fff, "  [%s %s Character Dump]\n\n",
			VERSION_MODE_NAME, VERSION_STRING);

	/* Display player */
	display_player(0, FALSE);

	/* Dump part of the screen */
	for (y = 1; y < 23; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Back up over spaces */
		while ((x > 0) && (buf[x-1] == ' ')) --x;

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		x_file_putf(fff, encoding, "%s\n", buf);
	}

	/* Skip some lines */
	file_putf(fff, "\n");

	/* If dead, dump last messages -- Prfnoff */
	if (p_ptr->is_dead)
	{
		i = messages_num();

		if (i > 15) i = 15;
		file_putf(fff, "  [Last Messages]\n\n");
		while (i-- > 0)
		{
			x_file_putf(fff, encoding, "> %s\n", message_str((s16b)i));
		}

		file_putf(fff, "\nKilled by %s.\n\n", p_ptr->died_from);
	}

	file_putf(fff, "  [Character Equipment Stat Modifiers, Sustains and Flags]\n\n");

	/*dump stat modifiers and sustains*/
	dump_player_stat_info(fff);

	/* Display player */
	display_player(1, FALSE);

	/* Dump flags, but in two separate rows */
	for (w = 0; w < 2; w ++)
	{
		for (y = (11 + w); y < (21 + w); y++)
		{
			/* Dump each row */
			for (z = 0, x = 0; x < 79; x++, z++)
			{
				/* Get the attr/char */
				(void)(Term_what(x, y, &a, &c));

				/*Go through the whole thing twice, printing
				 *two of the sets of resist flags each time.
				 */
				if ((!w) && (x < 40))
				{
					/*hack - space it out a bit*/
					if (x == 20) file_putf(fff, "       ");

					/* Dump it */
					file_putf(fff, "%c", c);
				}

				else if ((w) && (x > 39))
				{
					/*hack - space it out a bit*/
					if (x == 60) file_putf(fff, "       ");

					/* Dump it */
					file_putf(fff, "%c", c);
				}
			}

			/* End the row */
			file_putf(fff, "%s\n", buf);
		}
	}

	/* Dump the equipment */
	if (p_ptr->equip_cnt)
	{
		file_putf(fff, "\n  [Character Equipment]\n\n");

		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			object_desc(o_name, sizeof(o_name), &inventory[i], ODESC_PREFIX | ODESC_FULL);

			x_file_putf(fff, encoding, "%c) %s\n", index_to_label(i), o_name);

			/* Describe random object attributes */
			identify_random_gen(&inventory[i]);
		}

		file_putf(fff, "\n\n");
	}

	/* Dump the quiver */
	if (p_ptr->quiver_slots)
	{
		file_putf(fff, "  [Character Equipment -- Quiver]\n\n");

		for (i = QUIVER_START; i < QUIVER_END; i++)
		{
			/* Ignore empty slots */
			if (!inventory[i].k_idx) continue;

			object_desc(o_name, sizeof(o_name), &inventory[i], ODESC_PREFIX | ODESC_FULL);

			x_file_putf(fff, encoding, "%c) %s\n", index_to_label(i), o_name);

			/* Describe random object attributes */
			identify_random_gen(&inventory[i]);
		}

		file_putf(fff, "\n\n");
	}

	/* Dump the inventory */
	file_putf(fff, "  [Character Inventory]\n\n");
	for (i = 0; i < INVEN_PACK; i++)
	{
		if (!inventory[i].k_idx) break;

		object_desc(o_name, sizeof(o_name), &inventory[i], ODESC_PREFIX | ODESC_FULL);
		x_file_putf(fff, encoding, "%c) %s\n", index_to_label(i), o_name);

		/* Describe random object attributes */
		identify_random_gen(&inventory[i]);
	}
	file_putf(fff, "\n");

	/* Dump the Home Flags , then the inventory -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Header */
		file_putf(fff, "  [Home Inventory Stat Modifiers, Sustains and Flags]\n\n");

		/* Dump stat modifiers and sustains */
		dump_home_stat_info(fff);

		/* Dump the home resists and abilities display */
		for (i =2; i <4; ++i)
		{
			/* Display player */
			display_player(i, FALSE);

			/* Dump part of the screen */
			for (y = (i + 7); y < (i + 17); y++)
			{
				/* Dump each row */
				for (x = 0; x < 79; x++)
				{
					/* Get the attr/char */
					(void)(Term_what(x, y, &a, &c));

					/* Dump it */
					buf[x] = c;
				}

				/* Back up over spaces */
				while ((x > 0) && (buf[x-1] == ' ')) --x;

				/* Terminate */
				buf[x] = '\0';

				/* End the row */
				x_file_putf(fff, encoding, "%s\n", buf);
			}
		}

		/* Display player */
		display_player(0, FALSE);

		/* End the row */
		file_putf(fff, "\n");

		/*Now dump the inventory*/

		/* Header */
		file_putf(fff, "  [Home Inventory]\n\n");

		/* Dump all available items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			object_desc(o_name, sizeof(o_name), &st_ptr->stock[i], ODESC_PREFIX | ODESC_FULL);
			x_file_putf(fff, encoding, "%c) %s\n", I2A(i), o_name);

			/* Describe random object attributes */
			identify_random_gen(&st_ptr->stock[i]);
		}

		/* Add an empty line */
		file_putf(fff, "\n");
	}

	else file_putf(fff, "[Your Home Is Empty]\n\n");

	/* Check if in quest */
	if (guild_quest_level())
	{
		quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

		/* Skip completed quest */
		if (!(q_ptr->q_flags & (QFLAG_COMPLETED)))
		{
			char q_out[160];

			x_file_putf(fff, encoding, "  [Current Quest]\n\n");

			/* Get the quest description */
			describe_quest(q_out, sizeof(q_out), guild_quest_level(), QMODE_FULL);

			/* Describe quest */
			x_file_putf(fff, encoding, "%s\n", q_out);

			/* Add an empty line */
			x_file_putf(fff, encoding, "\n\n");
		}
	}

	/* dump notes to character file*/
	if (adult_take_notes)
	{
 		char line[1024];

		/* close the notes file for writing */
		file_close(notes_file);

		/* get the path for the notes file */
		notes_file = file_open(notes_fname, MODE_READ, -1);

		/* Write the contents of the notes file to the dump file line-by-line */
		while (file_getl(notes_file, line, sizeof(line)))
		{
			/* Replace escape sequences in template */
			fill_template(line, sizeof(line));

			/* Translate the note to the desired encoding */
			xstr_trans(line, encoding);

			/* Write the note */
			file_put(fff, line);

			/* Put a new line character */
			file_put(fff, "\n");
		}

		/* aesthetics */
		file_putf(fff, "============================================================\n");

		file_putf(fff, "\n\n");

		/*close it for reading*/
		file_close(notes_file);

		/*re-open for appending*/
		notes_file = file_open(notes_fname, MODE_APPEND, FTYPE_TEXT);
	}

	/* Dump options */
	file_putf(fff, "  [Options]\n");

	/* Dump options */
	for (i = 0; i < OPT_MAX; i++)
	{
		/* Hack - use game play options */
		if (i < OPT_GAME_PLAY) continue;
		if ((i >= OPT_EFFICIENCY) && (i < OPT_ADULT)) continue;

		/* Print the labels */
		if (i == OPT_GAME_PLAY) file_putf(fff, "\nGAME PLAY OPTIONS:\n\n");
		if (i == OPT_CHEAT) file_putf(fff, "\nCHEAT OPTIONS:\n\n");
		if (i == OPT_ADULT) file_putf(fff, "\nBIRTH OPTIONS:\n\n");
		if (i == OPT_SCORE) file_putf(fff, "\nCHEAT OPTIONS:\n\n");

		if (option_name(i))
		{
			x_file_putf(fff, encoding, "%-45s: %s (%s)\n",
					option_desc(i),
					op_ptr->opt[i] ? "yes" : "no ",
					option_name(i));
		}
	}

	/* Skip some lines */
	file_putf(fff, "\n\n");

	/* Return to standard display */
 	Term->xchar_hook = old_xchar_hook;

	/* Close it */
	file_close(fff);

	/* Success */
	return (0);
}


/*
 * Make a string lower case.
 */
void string_lower(char *buf)
{
	char *s;

	/* Lowercase the string */
	for (s = buf; *s != 0; s++) *s = my_tolower((unsigned char)*s);
}


/*
 * Recursive file perusal.
 *
 * Return FALSE on "?", otherwise TRUE.
 *
 * Process various special text in the input file, including the "menu"
 * structures used by the "help file" system.
 *
 * This function could be made much more efficient with the use of "seek"
 * functionality, especially when moving backwards through a file, or
 * forwards through a file by less than a page at a time.  XXX XXX XXX
 *
 * Consider using a temporary file, in which special lines do not appear,
 * and which could be pre-padded to 80 characters per line, to allow the
 * use of perfect seeking.  XXX XXX XXX
 *
 * Allow the user to "save" the current file.  XXX XXX XXX
 */
bool show_file(cptr name, cptr what, int line, int mode)
{
	int i, k, n;

	ui_event_data ke;

	/* Number of "real" lines passed by */
	int next = 0;

	/* Number of "real" lines in the file */
	int size;

	/* Backup value for "line" */
	int back = 0;

	/* This screen has sub-screens */
	bool menu = FALSE;

	/* Case sensitive search */
	bool case_sensitive = FALSE;

	/* Current help file */
	ang_file *fff = NULL;

	/* Find this string (if any) */
	char *find = NULL;

	/* Jump to this tag */
	cptr tag = NULL;

	/* Hold a string to find */
	char finder[80];

	/* Hold a string to show */
	char shower[80];

	/* Filename */
	char filename[1024];

	/* Describe this thing */
	char caption[128];

	/* Path buffer */
	char path[1024];

	/* General buffer */
	char buf[1024];

	/* Lower case version of the buffer, for searching */
	char lc_buf[1024];

	/* Sub-menu information */
	char hook[26][32];

	int wid, hgt;

	/* Wipe finder */
	my_strcpy(finder, "", sizeof(finder));

	/* Wipe shower */
	my_strcpy(shower, "", sizeof(shower));

	/* Wipe caption */
	my_strcpy(caption, "", sizeof(caption));

	/* Wipe the hooks */
	for (i = 0; i < 26; i++) hook[i][0] = '\0';

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Copy the filename */
	my_strcpy(filename, name, sizeof(filename));

	n = strlen(filename);

	/* Extract the tag from the filename */
	for (i = 0; i < n; i++)
	{
		if (filename[i] == '#')
		{
			filename[i] = '\0';
			tag = filename + i + 1;
			break;
		}
	}

	/* Redirect the name */
	name = filename;

	/* Hack XXX XXX XXX */
	if (what)
	{
		/* Caption */
		my_strcpy(caption, what, sizeof(caption));

		/* Get the filename */
		my_strcpy(path, name, sizeof(path));

		/* Open */
		fff = file_open(path, MODE_READ, -1);
	}

	/* Look in "help" */
	if (!fff)
	{
		/* Caption */
		strnfmt(caption, sizeof(caption), "Help file '%s'", name);

		/* Build the filename */
		path_build(path, sizeof(path), ANGBAND_DIR_HELP, name);

		/* Open the file */
		fff = file_open(path, MODE_READ, -1);
	}

	/* Look in "info" */
	if (!fff)
	{
		/* Caption */
		strnfmt(caption, sizeof(caption), "Info file '%s'", name);

		/* Build the filename */
		path_build(path, sizeof(path), ANGBAND_DIR_INFO, name);

		/* Open the file */
		fff = file_open(path, MODE_READ, -1);
	}

	/* Oops */
	if (!fff)
	{
		/* Message */
		msg_format("Cannot open '%s'.", name);
		message_flush();

		/* Oops */
		return (TRUE);
	}

	/* Pre-Parse the file */
	while (TRUE)
	{
		/* Read a line or stop */
		if (!file_getl(fff, buf, sizeof(buf))) break;

		/* XXX Parse "menu" items */
		if (prefix(buf, "***** "))
		{
			char b1 = '[', b2 = ']';

			/* Notice "menu" requests */
			if ((buf[6] == b1) && isalpha((unsigned char)buf[7]) &&
			    (buf[8] == b2) && (buf[9] == ' '))
			{
				/* This is a menu file */
				menu = TRUE;

				/* Extract the menu item */
				k = A2I(buf[7]);

				/* Store the menu item (if valid) */
				if ((k >= 0) && (k < 26))
					my_strcpy(hook[k], buf + 10, sizeof(hook[0]));
			}
			/* Notice "tag" requests */
			else if (buf[6] == '<')
			{
				if (tag)
				{
					/* Remove the closing '>' of the tag */
					buf[strlen(buf) - 1] = '\0';

					/* Compare with the requested tag */
					if (streq(buf + 7, tag))
					{
						/* Remember the tagged line */
						line = next;
					}
				}
			}

			/* Skip this */
			continue;
		}

		/* Count the "real" lines */
		next++;
	}

	/* Save the number of "real" lines */
	size = next;

	/* Display the file */
	while (TRUE)
	{
		char prompt[120];

		/* Clear screen */
		Term_clear();

		/* Restrict the visible range */
		if (line > (size - (hgt - 4))) line = size - (hgt - 4);
		if (line < 0) line = 0;

		/* Re-open the file if needed */
		if (next > line)
		{
			/* Close it */
			file_close(fff);

			/* Hack -- Re-Open the file */
			fff = file_open(path, MODE_READ, FTYPE_TEXT);

			/* Oops */
			if (!fff) return (TRUE);

			/* File has been restarted */
			next = 0;
		}

		/* Goto the selected line */
		while (next < line)
		{
			/* Get a line */
			if (!file_getl(fff, buf, sizeof(buf))) break;

			/* Skip tags/links */
			if (prefix(buf, "***** ")) continue;

			/* Count the lines */
			next++;
		}

		/* Dump the next lines of the file */
		for (i = 0; i < hgt - 4; )
		{
			/* Hack -- track the "first" line */
			if (!i) line = next;

			/* Get a line of the file or stop */
			if (!file_getl(fff, buf, sizeof(buf))) break;

			/* Hack -- skip "special" lines */
			if (prefix(buf, "***** ")) continue;

			/* Count the "real" lines */
			next++;

			/* Replace escape sequences in template */
			fill_template(buf, sizeof(buf));

			/* Make a copy of the current line for searching */
			my_strcpy(lc_buf, buf, sizeof(lc_buf));

			/* Make the line lower case */
			if (!case_sensitive) string_lower(lc_buf);

			/* Hack -- keep searching */
			if (find && !i && !strstr(lc_buf, find)) continue;

			/* Hack -- stop searching */
			find = NULL;

			/* Dump the line */
			Term_putstr(0, i+2, -1, TERM_WHITE, buf);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = lc_buf;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-lc_buf, i+2, len, TERM_YELLOW, &buf[str-lc_buf]);

					/* Advance */
					str += len;
				}
			}

			/* Count the printed lines */
			i++;
		}

		/* Hack -- failed search */
		if (find)
		{
			bell("Search string not found!");
			line = back;
			find = NULL;
			continue;
		}

		/* Show a general "title" */
		prt(format("[%s %s, %s, Line %d-%d/%d]", VERSION_MODE_NAME, VERSION_STRING,
				   caption, line, line + hgt - 4, size), 0, 0);

		/* Buttons */
		button_kill_all();
		button_add("ESC", ESCAPE);

		/* Prompt -- menu screen */
		if (menu)
		{
			/* Wait for it */
			my_strcpy(prompt,"[Press a letter, or ESC to exit.]", sizeof(prompt));
		}

		/* Prompt -- small files */
		else if (size <= hgt - 4)
		{
			/* Wait for it */
			my_strcpy(prompt,"[Press ESC to exit.]", sizeof(prompt));
		}

		/* Prompt -- large files */
		else
		{
			/* Wait for it */
			my_strcpy(prompt,"[Press Space to advance, or ESC to exit.]", sizeof(prompt));

			/* More buttons */
			if (!menu)
			{
				button_add("|UP", '-');
				button_add("|DOWN", ' ');
				button_add("|BOTTOM", '1');
				button_add("|TOP", '7');
			}
		}
		prt(prompt, hgt - 2, 0);
		if (!menu)
		{
			button_add("|FIND", '/');
			button_add("|GOTOLINE", '#');
		}
		else
		{
			button_add("|SHOW", '=');
			button_add("|GOTO_FILE", '%');
		}

		/* We are in icky_mode, so a call to redraw_stuff won't work */
		event_signal(EVENT_MOUSEBUTTONS);

		/* Get a keypress */
		ke = inkey_ex();

		if (menu && isalpha((unsigned char)ke.key))
		{
			/* Extract the requested menu item */
			k = A2I(ke.key);

			/* Verify the menu item */
			if ((k >= 0) && (k <= 25) && hook[k][0])
			{
				/* Recurse on that file */
				if (!show_file(hook[k], NULL, 0, mode)) ke.key = ESCAPE;
			}
		}

		/* Exit the help */
		if (ke.key == ESCAPE) break;

		/* Toggle case sensitive on/off */
		if (ke.key == '!')
		{
			case_sensitive = !case_sensitive;
		}

		/* Try showing */
		if (ke.key == '=')
		{
			/* Get "shower" */
			prt("Show: ", hgt - 1, 0);
			(void)askfor_aux(shower, sizeof(shower), NULL);

			/* Make the "shower" lowercase */
			if (!case_sensitive) string_lower(shower);
		}

		/* Try finding */
		if (ke.key == '/')
		{
			/* Get "finder" */
			prt("Find: ", hgt - 1, 0);
			if (askfor_aux(finder, sizeof(finder), NULL))
			{
				/* Find it */
				find = finder;
				back = line;
				line = line + 1;

				/* Make the "finder" lowercase */
				if (!case_sensitive) string_lower(finder);

				/* Show it */
				my_strcpy(shower, finder, sizeof(shower));
			}
		}

		/* Go to a specific line */
		if (ke.key == '#')
		{
			char tmp[80];
			prt("Goto Line: ", hgt - 1, 0);
			my_strcpy(tmp, "0", sizeof(tmp));
			if (askfor_aux(tmp, sizeof(tmp), NULL))
			{
				line = atoi(tmp);
			}
		}

		/* Go to a specific file */
		if (ke.key == '%')
		{
			char ftmp[80];
			prt("Goto File: ", hgt - 1, 0);
			if (game_mode == GAME_NPPMORIA) my_strcpy(ftmp, "m_help.hlp", sizeof(ftmp));
			else my_strcpy(ftmp, "help.hlp", sizeof(ftmp));
			if (askfor_aux(ftmp, sizeof(ftmp), NULL))
			{
				if (!show_file(ftmp, NULL, 0, mode)) ke.key = ESCAPE;
			}
		}

		/* Back up one line */
		if (ke.key == ARROW_UP || ke.key == '8')
		{
			line = line - 1;
			if (line < 0) line = 0;
		}

		/* Back up one half page */
		if (ke.key == '_')
		{
			line = line - ((hgt - 4) / 2);
		}

		/* Back up one full page */
		if ((ke.key == '-') || (ke.key == '9'))
		{
			line = line - (hgt - 4);
		}

		/* Back to the top */
		if (ke.key == '7')
		{
			line = 0;
 		}

		/* Advance one line */
		if ((ke.key == '\n') || (ke.key == '\r') || (ke.key == '2')
			    || (ke.key == ARROW_DOWN))
		{
			line = line + 1;
		}

		/* Advance one half page */
		if (ke.key == '+')
		{
			line = line + ((hgt - 4) / 2);
		}

		/* Advance one full page */
		if ((ke.key == ' ') || (ke.key == '3'))
		{
			line = line + (hgt - 4);
		}

		/* Advance to the bottom */
		if (ke.key == '1')
		{
			line = size;
		}

		/* Recurse on letters */
		if (menu && isdigit((int)ke.key) && hook[D2I(ke.key)][0])
		{
			/* Recurse on that file */
			if (!show_file(hook[D2I(ke.key)], NULL, 0, mode))
			ke.key = '?';
		}

		/* Exit on escape */
		if (ke.key == '?') break;
	}

	/* Close the file */
	file_close(fff);

	/* Done */
	return (ke.key != '?');
}


/*
 * Peruse the On-Line-Help
 */
void do_cmd_help(void)
{
	/* Save the buttons */
	button_backup_all();

	/* Save screen */
	screen_save();

	/* Peruse the main help file */
	if (game_mode == GAME_NPPMORIA) (void)show_file("m_help.hlp", NULL, 0, 0);
	else (void)show_file("help.hlp", NULL, 0, 0);

	/* Load screen */
	screen_load();

	/* Restore the buttons and status line */
	button_kill_all();
	button_restore();
	event_signal(EVENT_MOUSEBUTTONS);
}


/*
 * Process the player name and extract a clean "base name".
 *
 * If "sf" is TRUE, then we initialize "savefile" based on player name.
 *
 * Some platforms (Windows, Macintosh, Amiga) leave the "savefile" empty
 * when a new character is created, and then when the character is done
 * being created, they call this function to choose a new savefile name.
 *
 * This also now handles the turning on and off of the automatic
 * sequential numbering of character names with Roman numerals.
 */
void process_player_name(bool sf)
{
	int i;

	/* Process the player name */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		byte c = op_ptr->full_name[i];

		/* Translate to 7-bit ASCII */
 		if (c > 127) c = seven_bit_translation[c - 128];

		/* No control characters */
		if (iscntrl((unsigned char)c))
		{
			/* Illegal characters */
			quit_fmt("Illegal control char (0x%02X) in player name", c);
		}

		/* Convert all non-alphanumeric symbols */
		if (!isalpha((unsigned char)c) && !isdigit((unsigned char)c)) c = '_';

		/* Build "base_name" */
		op_ptr->base_name[i] = c;
	}

#if defined(WINDOWS)

	/* Max length */
	if (i > 8) i = 8;

#endif

	/* Terminate */
	op_ptr->base_name[i] = '\0';

	/* Require a "base" name */
	if (!op_ptr->base_name[0])
	{
		my_strcpy(op_ptr->base_name, "PLAYER", sizeof(op_ptr->base_name));
	}

	/* Pick savefile name if needed */
	if (sf)
	{
		char temp[128];

#if defined(SET_UID)
		/* Rename the savefile, using the player_uid and base_name */
		strnfmt(temp, sizeof(temp), "%d.%s", player_uid, op_ptr->base_name);
#else
		/* Rename the savefile, using the base name */
		strnfmt(temp, sizeof(temp), "%s", op_ptr->base_name);
#endif

		/* Build the filename */
		path_build(savefile, sizeof(savefile), ANGBAND_DIR_SAVE, temp);
	}
}


/*
 * Save the game
 */
void save_game(void)
{
	/* Disturb the player */
	disturb(1, 0);

	/* Clear messages */
	message_flush();

	/* Handle stuff */
	handle_stuff();

	/* Message */
	prt("Saving game...", 0, 0);

	/* Refresh */
	(void)Term_fresh();

	/* The player is not dead */
	my_strcpy(p_ptr->died_from, "(saved)", sizeof(p_ptr->died_from));

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Save the player */
	if (save_player())
	{
		prt("Saving game... done.", 0, 0);
	}

	/* Save failed (oops) */
	else
	{
		prt("Saving game... failed!", 0, 0);
	}

	/* Allow suspend again */
	signals_handle_tstp();

	/* Refresh */
	(void)Term_fresh();

	/* Note that the player is not dead */
	my_strcpy(p_ptr->died_from, "(alive and well)", sizeof(p_ptr->died_from));
}


/*
 * Close up the current game (player may or may not be dead)
 *
 * Note that the savefile is not saved until the tombstone is
 * actually displayed and the player has a chance to examine
 * the inventory and such.  This allows cheating if the game
 * is equipped with a "quit without save" method.  XXX XXX XXX
 */
void close_game(void)
{
	/* Handle stuff */
	handle_stuff();

	/* Flush the messages */
	message_flush();

	/* Flush the input */
	flush();

	/* No suspending now */
	signals_ignore_tstp();

	/* Hack -- Increase "icky" depth */
	character_icky++;

	/* Handle death */
	if (p_ptr->is_dead)
	{
		/* Auxiliary routine */
		death_screen();
	}

	/* Still alive */
	else
	{
		/* Save the game */
		save_game();

		if (Term->mapped_flag)
		{
			/* Prompt for scores XXX XXX XXX */
			prt("Press Return (or Escape).", 0, 40);

			/* Predict score (or ESCAPE) */
			if (inkey() != ESCAPE) predict_score();
		}
	}

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Allow suspending now */
	signals_handle_tstp();
}


/*
 * Handle abrupt death of the visual system
 *
 * This routine is called only in very rare situations, and only
 * by certain visual systems, when they experience fatal errors.
 *
 * XXX XXX Hack -- clear the death flag when creating a HANGUP
 * save file so that player can see tombstone when restart.
 */
void exit_game_panic(void)
{
	/* If nothing important has happened, just quit */
	if (!character_generated || character_saved) quit("panic");

	/* Mega-Hack -- see "msg_print()" */
	msg_flag = FALSE;

	/* Clear the top line */
	prt("", 0, 0);

	/* Hack -- turn off some things */
	disturb(1, 0);

	/* Hack -- Delay death XXX XXX XXX */
	if (p_ptr->chp < 0) p_ptr->is_dead = FALSE;

	/* Hardcode panic save */
	p_ptr->panic_save = 1;

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Indicate panic save */
	my_strcpy(p_ptr->died_from, "(panic save)", sizeof(p_ptr->died_from));

	/* Panic save, or get worried */
	if (!save_player()) quit("panic save failed!");


	/* Successful panic save */
	quit("panic save succeeded!");
}


static void write_html_escape_char(ang_file *htm, char c)
{
	switch (c)
	{
		/*
		case '#':
		{
			fprintf(htm, "&nbsp;");
			break;
		}*/
		case '<':
			file_putf(htm, "&lt;");
			break;
		case '>':
			file_putf(htm, "&gt;");
			break;
		case '&':
			file_putf(htm, "&amp;");
			break;
		default:
			file_putf(htm, "%c", c);
			break;
	}
}


/*
 * Get the default (ASCII) tile for a given screen location
 */
static void get_default_tile(int row, int col, byte *a_def, char *c_def)
{
	byte a, ta;
	char c, tc;

	int wid, hgt;
	int screen_wid, screen_hgt;

	int x;
	int y = row - ROW_MAP + Term->offset_y;

	/* Retrieve current screen size */
	Term_get_size(&wid, &hgt);

	/* Calculate the size of dungeon map area (ignoring bigscreen) */
	screen_wid = wid - (COL_MAP + 1);
	screen_hgt = hgt - (ROW_MAP + 1);

	/* Get the tile from the screen */
	a = Term->scr->a[row][col];
	c = Term->scr->c[row][col];

	/* Skip bigtile placeholders */
	if (use_bigtile && (a == 255) && (c == -1))
	{
		/* Replace with "white space" */
		a = TERM_WHITE;
		c = ' ';
	}
	/* Convert the map display to the default characters */
	else if (!character_icky &&
	    ((col - COL_MAP) >= 0) && ((col - COL_MAP) < screen_wid) &&
	    ((row - ROW_MAP) >= 0) && ((row - ROW_MAP) < screen_hgt))
	{
		/* Bigtile uses double-width tiles */
		if (use_bigtile)
			x = (col - COL_MAP) / 2 + Term->offset_x;
		else
			x = col - COL_MAP + Term->offset_x;

		/* Convert dungeon map into default attr/chars */
		if (in_bounds(y, x))
		{
			/* Retrieve default attr/char */
			map_info(y, x, &a, &c, &ta, &tc, TRUE);
		}
		else
		{
			/* "Out of bounds" is empty */
			a = TERM_WHITE;
			c = ' ';
		}

		if (c == '\0') c = ' ';
	}

	/* Filter out remaining graphics */
	if (a & 0x80)
	{
		/* Replace with "white space" */
		a = TERM_WHITE;
		c = ' ';
	}

	/* Return the default tile */
	*a_def = a;
	*c_def = c;
}


/* Take an html screenshot */
void html_screenshot(cptr name, int mode)
{
	int y, x;
	int wid, hgt;

	byte a = TERM_WHITE;
	byte oa = TERM_WHITE;
	char c = ' ';

	const char *new_color_fmt = (mode == 0) ?
					"<font color=\"#%02X%02X%02X\">"
				 	: "[COLOR=\"#%02X%02X%02X\"]";
	const char *change_color_fmt = (mode == 0) ?
					"</font><font color=\"#%02X%02X%02X\">"
					: "[/COLOR][COLOR=\"#%02X%02X%02X\"]";
	const char *close_color_fmt = mode ==  0 ? "</font>" : "[/COLOR]";

	ang_file *fp;
	char buf[1024];


	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);
	fp = file_open(buf, MODE_WRITE, FTYPE_TEXT);

	/* Oops */
	if (!fp)
	{
		plog_fmt("Cannot write the '%s' file!", buf);
		return;
	}

	/* Retrieve current screen size */
	Term_get_size(&wid, &hgt);

	if (mode == 0)
	{
		file_putf(fp, "<!DOCTYPE html><html><head>\n");
		file_putf(fp, "  <meta='generator' content='%s %s'>\n",
				VERSION_MODE_NAME, VERSION_STRING);
		file_putf(fp, "  <title>%s</title>\n", name);
		file_putf(fp, "</head>\n\n");
		file_putf(fp, "<body style='color: #fff; background: #000;'>\n");
		file_putf(fp, "<pre>\n");
	}
	else
	{
		file_putf(fp, "[CODE][TT][BC=black][COLOR=white]\n");
	}

	/* Dump the screen */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			/* Get the attr/char */
			get_default_tile(y, x, &a, &c);

			/* Color change */
			if (oa != a && c != ' ')
			{
				/* From the default white to another color */
				if (oa == TERM_WHITE)
				{
					file_putf(fp, new_color_fmt,
					        angband_color_table[a][1],
					        angband_color_table[a][2],
					        angband_color_table[a][3]);
				}

				/* From another color to the default white */
				else if (a == TERM_WHITE)
				{
					file_putf(fp, close_color_fmt);
				}

				/* Change colors */
				else
				{
					file_putf(fp, change_color_fmt,
					        angband_color_table[a][1],
					        angband_color_table[a][2],
					        angband_color_table[a][3]);
				}

				/* Remember the last color */
				oa = a;
			}

			/* Write the character and escape special HTML characters */
			if (mode == 0) write_html_escape_char(fp, c);
			else file_putf(fp, "%c", c);
		}

		/* End the row */
		file_putf(fp, "\n");
	}

	/* Close the last font-color tag if necessary */
	if (oa != TERM_WHITE) file_putf(fp, close_color_fmt);

	if (mode == 0)
	{
		file_putf(fp, "</pre>\n");
		file_putf(fp, "</body>\n");
		file_putf(fp, "</html>\n");
	}
	else
	{
		file_putf(fp, "[/COLOR][/BC][/TT][/CODE]\n");
	}

	/* Close it */
	file_close(fp);
}


/*
 * Finds text patterns in the given text and replaces them with some content determined
 * by the kind of pattern.
 * A pattern has this form: {{pattern_name}}
 * The text is replaced in place.
 * See the escapes array for the supported patterns.
 */
void fill_template(char buf[], int max_buf)
{
	char local[1024] = "";
	char *end_local = local;
	char *buf_ptr = buf;
	char *start;
	bool changed = FALSE;
	/* List of recognized patterns */
	static const char *escapes[] =
	{
		"{{full_character_name}}",
		NULL
	};

	/* Find occurrences of the patterns */
	/* First we look for the pattern's start */
	while ((start = strstr(buf_ptr, "{{")) != NULL)
	{
		int i = 0;
		int id = -1;

		/* Search the pattern */
		while (escapes[i] != NULL)
		{
			/* Found? */
			if (prefix(start, escapes[i]))
			{
				id = i;
				break;
			}
		}

		/* Found a pattern */
		if (id != -1)
		{
			/* Remember this */
			changed = TRUE;

			/* Copy the previous text */
			if (start > buf_ptr)
			{
				/* End the previous text */
				*start = '\0';

				/* Concat the previous text to the result text */
				end_local = my_fast_strcat(local, end_local, buf_ptr, sizeof(local));
			}

			/* Process pattern actions */
			switch (id)
			{
				/* Display current character name */
				case 0:
				{
					end_local = my_fast_strcat(local, end_local, op_ptr->full_name,
						sizeof(local));

					break;
				}
			}

			/* Start again at the end of the pattern */
			buf_ptr = start + strlen(escapes[id]);
		}
		/* Not found */
		else
		{
			/* Just ignore the pattern prefix */
			buf_ptr = start + 2;
		}
	}

	/* Something happened */
	if (changed)
	{
		/* Copy the remaining text if necessary */
		if (*buf_ptr)
		{
			end_local = my_fast_strcat(local, end_local, buf_ptr, sizeof(local));
		}

		/* Overwrite the original text */
		my_strcpy(buf, local, max_buf);
	}
}

