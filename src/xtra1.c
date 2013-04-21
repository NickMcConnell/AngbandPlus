/* File: xtra1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* Ugly, ugly hack to determine when to print god bonus messages */

static int old_badness = -1000;


/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 100)
		{
			sprintf(out_val, "18/%03d ", bonus);
		}
		else
		{
			sprintf(out_val, " 18/%02d ", bonus);
		}
	}

	/* From 3 to 18 */
	else
	{
		sprintf(out_val, "    %2d ", val);
	}
}



/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat_value(int value, int amount)
{
	int i;

	/* Reward */
	if (amount > 0)
	{
		/* Apply each point */
		for (i = 0; i < amount; i++)
		{
			/* One point at a time */
			if (value < 18)
				value++;

			/* Ten "points" at a time */
			else
				value += 10;
		}
	}

	/* Penalty */
	else if (amount < 0)
	{
		/* Apply each point */
		for (i = 0; i < (0 - amount); i++)
		{
			/* Ten points at a time */
			if (value >= 18 + 10)
				value -= 10;

			/* Hack -- prevent weirdness */
			else if (value > 18)
				value = 18;

			/* One point at a time */
			else if (value > 3)
				value--;
		}
	}

	/* Return new value */
	return (value);
}



/*
 * Print character info at given row, column in a 13 char field
 */
static void prt_field(cptr info, int row, int col)
{
	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", row, col);

	/* Dump the info itself */
	c_put_str(TERM_L_BLUE, info, row, col);
}




/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	char tmp[32];

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_YELLOW, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18 + 100)
	{
		put_str("!", ROW_STAT + stat, 3);
	}
}




/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	cptr p = "";

	/* This has been disabled to make way for a sanity display */
	return;

	/* Wizard */
	if (p_ptr->wizard)
	{
		p = "[=-WIZARD-=]";
	}

	/* Winner */
	else if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "***WINNER***";
	}

	/* Show current shape */

	else if (p_ptr->shape)
	{
		p = format("[%^s]", shape_info[p_ptr->shape - 1].name);
	}

	/* Normal */
	else
	{
		p = player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5];
	}

	prt_field(p, ROW_TITLE, COL_TITLE);
}


/*
 * Prints level
 */
static void prt_level(void)
{
	char tmp[32];

	sprintf(tmp, "%6d ", p_ptr->lev);

	if (p_ptr->lev >= p_ptr->max_lev)
	{
		put_str("LEVEL ", ROW_LEVEL, 0);
		c_put_str(TERM_L_GREEN, tmp, ROW_LEVEL, COL_LEVEL + 6);
	}
	else
	{
		put_str("Level ", ROW_LEVEL, 0);
		c_put_str(TERM_YELLOW, tmp, ROW_LEVEL, COL_LEVEL + 6);
	}
}


/*
 * Display the experience
 */
static void prt_exp(void)
{
	char out_val[32];

	sprintf(out_val, "%8ld ", (long) p_ptr->exp);

	if (p_ptr->exp >= p_ptr->max_exp)
	{
		put_str("EXP ", ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 4);
	}
	else
	{
		put_str("Exp ", ROW_EXP, 0);
		c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 4);
	}
}


/*
 * Prints current gold
 */
static void prt_gold(void)
{
	char tmp[32];

	put_str("AU ", ROW_GOLD, COL_GOLD);
	sprintf(tmp, "%9ld ", (long) p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD + 3);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
	char tmp[32];

	put_str("Cur AC ", ROW_AC, COL_AC);
	sprintf(tmp, "%5d ", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);
}


/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	char tmp[32];

	byte color;


	put_str("Max HP ", ROW_MAXHP, COL_MAXHP);

	sprintf(tmp, "%5d ", p_ptr->mhp);
	color = TERM_L_GREEN;

	c_put_str(color, tmp, ROW_MAXHP, COL_MAXHP + 7);


	put_str("Cur HP ", ROW_CURHP, COL_CURHP);

	sprintf(tmp, "%5d ", p_ptr->chp);

	if (p_ptr->chp >= p_ptr->mhp)
	{
		color = TERM_L_GREEN;
	}
	else if (p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	c_put_str(color, tmp, ROW_CURHP, COL_CURHP + 7);
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
	char tmp[32];
	byte color;


	/* Do not show mana unless it matters */

	if (spell_num == 0)
	{
		sprintf(tmp, "%12s", "");
		put_str(tmp, ROW_CURSP, COL_CURSP);
		put_str(tmp, ROW_MAXSP, COL_MAXSP);

		return;
	}


	put_str("Max SP ", ROW_MAXSP, COL_MAXSP);

	sprintf(tmp, "%5d ", p_ptr->msp);
	color = TERM_L_GREEN;

	c_put_str(color, tmp, ROW_MAXSP, COL_MAXSP + 7);


	put_str("Cur SP ", ROW_CURSP, COL_CURSP);

	sprintf(tmp, "%5d ", p_ptr->csp);

	if (p_ptr->csp >= p_ptr->msp)
	{
		color = TERM_L_GREEN;
	}
	else if (p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	/* Show mana */
	c_put_str(color, tmp, ROW_CURSP, COL_CURSP + 7);
}


/*
 * Prints the player's current sanity.
 * The format is different from HP/SP to save screen space.
 */
static void prt_sane(void)
{
	char tmp[5];
	byte color;
	int perc;

	if (p_ptr->msane == 0)
	{
		perc = 100;
	}
	else
	{
		perc = (100 * p_ptr->csane) / p_ptr->msane;
	}

	put_str("Sanity ", ROW_SANITY, COL_SANITY);

	sprintf(tmp, "%4d ", p_ptr->csane);

	if (perc >= 100)
	{
		color = TERM_L_GREEN;
	}
	else if (perc > (10 * op_ptr->hitpoint_warn))
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	c_put_str(color, tmp, ROW_SANITY, COL_SANITY + 8);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	char depths[32];

	if (p_ptr->inside_special == SPECIAL_ARENA || p_ptr->inside_special == SPECIAL_MAGIC_ARENA)	/* -KMW- */
	{
		strcpy(depths, "Arena");
	}
	else if (p_ptr->inside_special == SPECIAL_QUEST) /* -KMW- */
	{
		strcpy(depths, "Quest");
	}
	else if (!p_ptr->depth)
	{
		strcpy(depths, "Town");
	}
	else if (p_ptr->inside_special == SPECIAL_WILD)
	{
		strcpy(depths, "Wild");
	}
	else if (depth_in_feet)
	{
		sprintf(depths, "%d ft", p_ptr->depth * 50);
	}
	else
	{
		sprintf(depths, "Lev %d", p_ptr->depth);
	}

	/* Right-Adjust the "depth", and clear old values */
	prt(format("%7s", depths), ROW_DEPTH, COL_DEPTH);
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		c_put_str(TERM_RED, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		c_put_str(TERM_ORANGE, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		c_put_str(TERM_YELLOW, "Hungry", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		c_put_str(TERM_L_GREEN, "      ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		c_put_str(TERM_L_GREEN, "Full  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Gorged */
	else
	{
		c_put_str(TERM_GREEN, "Gorged", ROW_HUNGRY, COL_HUNGRY);
	}
}


/*
 * Prints Blind status
 */
static void prt_blind(void)
{
	if (p_ptr->blind)
	{
		c_put_str(TERM_ORANGE, "Blind", ROW_BLIND, COL_BLIND);
	}
	else
	{
		put_str("     ", ROW_BLIND, COL_BLIND);
	}
}


/*
 * Prints Confusion status
 */
static void prt_confused(void)
{
	if (p_ptr->confused)
	{
		c_put_str(TERM_ORANGE, "Confused", ROW_CONFUSED, COL_CONFUSED);
	}
	else
	{
		put_str("        ", ROW_CONFUSED, COL_CONFUSED);
	}
}


/*
 * Prints Fear status
 */
static void prt_afraid(void)
{
	if (p_ptr->afraid)
	{
		c_put_str(TERM_ORANGE, "Afraid", ROW_AFRAID, COL_AFRAID);
	}
	else
	{
		put_str("      ", ROW_AFRAID, COL_AFRAID);
	}
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(void)
{
	if (p_ptr->poisoned)
	{
		c_put_str(TERM_ORANGE, "Poisoned", ROW_POISONED, COL_POISONED);
	}
	else
	{
		put_str("        ", ROW_POISONED, COL_POISONED);
	}
}


/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
	byte attr = TERM_WHITE;

	char text[16];


	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		attr = TERM_RED;

		strcpy(text, "Paralyzed!");
	}

	/* Resting */
	else if (p_ptr->resting)
	{
		int i;
		int n = p_ptr->resting;

		/* Start with "Rest" */
		strcpy(text, "Rest      ");

		/* Extensive (timed) rest */
		if (n >= 1000)
		{
			i = n / 100;
			text[9] = '0';
			text[8] = '0';
			text[7] = '0' + (i % 10);
			if (i >= 10)
			{
				i = i / 10;
				text[6] = '0' + (i % 10);
				if (i >= 10)
				{
					text[5] = '0' + (i / 10);
				}
			}
		}

		/* Long (timed) rest */
		else if (n >= 100)
		{
			i = n;
			text[9] = '0' + (i % 10);
			i = i / 10;
			text[8] = '0' + (i % 10);
			text[7] = '0' + (i / 10);
		}

		/* Medium (timed) rest */
		else if (n >= 10)
		{
			i = n;
			text[9] = '0' + (i % 10);
			text[8] = '0' + (i / 10);
		}

		/* Short (timed) rest */
		else if (n > 0)
		{
			i = n;
			text[9] = '0' + (i);
		}

		/* Rest until healed */
		else if (n == -1)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '*';
		}

		/* Rest until done */
		else if (n == -2)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '&';
		}
	}

	/* Repeating */
	else if (p_ptr->command_rep)
	{
		if (p_ptr->command_rep > 999)
		{
			sprintf(text, "Rep. %3d00", p_ptr->command_rep / 100);
		}
		else
		{
			sprintf(text, "Repeat %3d", p_ptr->command_rep);
		}
	}

	/* Searching */
	else if (p_ptr->searching)
	{
		strcpy(text, "Searching ");
	}

	/* Nothing interesting */
	else
	{
		strcpy(text, "          ");
	}

	/* Display the info (or blanks) */
	c_put_str(attr, text, ROW_STATE, COL_STATE);
}


/*
 * Prints the speed of a character.			-CJS-
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed;

	int attr = TERM_WHITE;
	char buf[32] = "";

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching)
		i += 10;

	/* Fast */
	if (i > 110)
	{
		attr = TERM_L_GREEN;
		sprintf(buf, "Fast (+%d)", (i - 110));
	}

	/* Slow */
	else if (i < 110)
	{
		attr = TERM_L_UMBER;
		sprintf(buf, "Slow (-%d)", (110 - i));
	}

	/* Display the speed */
	c_put_str(attr, format("%-11s", buf), ROW_SPEED, COL_SPEED);
}


static void prt_study(void)
{
	if (p_ptr->new_spells)
	{
		put_str("Study", ROW_STUDY, 64);
	}
	else
	{
		put_str("     ", ROW_STUDY, COL_STUDY);
	}
}


static void prt_shout(void)
{
	if (p_ptr->pets_notice)
	{
		put_str("Pet", ROW_SHOUT, COL_SHOUT);
	}
	else
	{
		put_str("   ", ROW_SHOUT, COL_SHOUT);
	}
}


static void prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)
	{
		c_put_str(TERM_L_RED, "Mortal wound ", ROW_CUT, COL_CUT);
	}
	else if (c > 200)
	{
		c_put_str(TERM_RED, "Deep gash    ", ROW_CUT, COL_CUT);
	}
	else if (c > 100)
	{
		c_put_str(TERM_RED, "Severe cut   ", ROW_CUT, COL_CUT);
	}
	else if (c > 50)
	{
		c_put_str(TERM_ORANGE, "Nasty cut    ", ROW_CUT, COL_CUT);
	}
	else if (c > 25)
	{
		c_put_str(TERM_ORANGE, "Bad cut      ", ROW_CUT, COL_CUT);
	}
	else if (c > 10)
	{
		c_put_str(TERM_YELLOW, "Light cut    ", ROW_CUT, COL_CUT);
	}
	else if (c)
	{
		c_put_str(TERM_YELLOW, "Graze        ", ROW_CUT, COL_CUT);
	}
	else
	{
		put_str("             ", ROW_CUT, COL_CUT);
	}
}



static void prt_stun(void)
{
	int s = p_ptr->stun;

	if (s > 100)
	{
		c_put_str(TERM_RED, "Knocked out  ", ROW_STUN, COL_STUN);
	}
	else if (s > 50)
	{
		c_put_str(TERM_ORANGE, "Heavy stun   ", ROW_STUN, COL_STUN);
	}
	else if (s)
	{
		c_put_str(TERM_ORANGE, "Stun         ", ROW_STUN, COL_STUN);
	}
	else
	{
		put_str("             ", ROW_STUN, COL_STUN);
	}
}



/*
 * Redraw the "monster health bar"
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.  When nothing
 * is being tracked, we clear the health bar.  If the monster being
 * tracked is not currently visible, a special health bar is shown.
 */
static void health_redraw(void)
{
	/* Not tracking */
	if (!p_ptr->health_who)
	{
		/* Erase the health bar */
		Term_erase(COL_INFO, ROW_INFO, 13);
	}

	/* Tracking an unseen monster */
	else if (!m_list[p_ptr->health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 13, TERM_WHITE, "[----------] ");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 13, TERM_WHITE, "[----------] ");
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[p_ptr->health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 13, TERM_WHITE, "[----------] ");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;
		/* Customized health bar: letters for fear/sleep status.  From GJW -KMW- */
		const char hb_normal[] = "**********";
		const char hb_fearful[] = "FFFFFFFFFF";
		const char hb_sleeping[] = "SSSSSSSSSS";
		const char *hb_ptr = hb_normal;

		monster_type *m_ptr = &m_list[p_ptr->health_who];

		/* Default to almost dead */
		byte attr = TERM_RED;

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;

		/* Badly wounded */
		if (pct >= 10)
			attr = TERM_L_RED;

		/* Wounded */
		if (pct >= 25)
			attr = TERM_ORANGE;

		/* Somewhat Wounded */
		if (pct >= 60)
			attr = TERM_YELLOW;

		/* Healthy */
		if (pct >= 100)
			attr = TERM_L_GREEN;

		/* Afraid.  Modified by GJW -KMW- */
		if (m_ptr->monfear)
		{
			attr = TERM_VIOLET;
			hb_ptr = hb_fearful;
		}

		/* Asleep.  Modified by GJW -KMW- */
		if (m_ptr->csleep)
		{
			attr = TERM_BLUE;
			hb_ptr = hb_sleeping;
		}

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 13, TERM_WHITE, "[----------] ");

		/* Dump the current "health" (use appropriate symbols -KMW-) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, hb_ptr);
	}
}

/* Print race and class */

static void prt_misc(void)
{
	/* Race and Class */
	if (p_ptr->shape)
	{
		prt_field(format("[%^s]", shape_info[p_ptr->shape - 1].name),
			ROW_RACE, COL_RACE);
	}
	else
	{
		prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
	}

	prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);
}


/*
 * Print whitespace.
 */
static void prt_basic_whitespace(void)
{
	c_put_str(TERM_WHITE, "             ", ROW_GOLD + 1, COL_GOLD);
	c_put_str(TERM_WHITE, "             ", ROW_AC - 1, COL_AC);
}

/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;

	/* Race and class */
	prt_misc();

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();

	/* All Stats */
	for (i = 0; i < 6; i++)
		prt_stat(i);

	/* Armor */
	prt_ac();

	/* Hitpoints */
	prt_hp();

	/* Spellpoints */
	prt_sp();

	/* Gold */
	prt_gold();

	/* Current depth */
	prt_depth();

	/* Current sanity */
	prt_sane();

	/* Special */
	health_redraw();

	/* Print whitespace. */
	prt_basic_whitespace();
}


/*
 * Display extra info (mostly below map)
 */
static void prt_frame_extra(void)
{
	/* Cut/Stun */
	prt_cut();
	prt_stun();

	/* Food */
	prt_hunger();

	/* Various */
	prt_blind();
	prt_confused();
	prt_afraid();
	prt_poisoned();

	/* State */
	prt_state();

	/* Speed */
	prt_speed();

	/* Study spells */
	prt_study();

	prt_shout();
}


/*
 * Hack -- display inventory in sub-windows
 */
static void fix_inven(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_INVEN)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display inventory */
		show_stack(inventory, FALSE);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}



/*
 * Hack -- display equipment in sub-windows
 */
static void fix_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_EQUIP)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display equipment */
		show_equip();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display flags in sub-windows
 */
static void fix_pflags(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_SPELL)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display flags */
		display_player(2);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display character in sub-windows
 */
static void fix_player(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_PLAYER)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display player */
		display_player(0);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}



/*
 * Hack -- display recent messages in sub-windows
 *
 * XXX XXX XXX Adjust for width and split messages
 */
static void fix_message(void)
{
	int j, i;
	int w, h;
	int x, y;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MESSAGE)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Get size */
		Term_get_size(&w, &h);

		/* Dump messages */
		for (i = 0; i < h; i++)
		{
			/* Dump the message on the appropriate line */
			Term_putstr(0, (h - 1) - i, -1, message_prior(i),
				message_str(i));

			/* Cursor */
			Term_locate(&x, &y);

			/* Clear to end of line */
			Term_erase(x, y, 255);
		}

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display overhead view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_overhead(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_OVERHEAD)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Hack -- Hide player XXX XXX XXX */
		cave_m_idx[py][px] = 0;

		/* Redraw map */
		display_map(15);

		/* Hack -- Show player XXX XXX XXX */
		cave_m_idx[py][px] = -1;

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MONSTER)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */
		if (p_ptr->monster_race_idx)
			display_roff(p_ptr->monster_race_idx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j])
			continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_OBJECT)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */

		/* 
		 * Not used -- nothing to recall yet.
		 *
		 * if (p_ptr->object_kind_idx)
		 * display_koff(p_ptr->object_kind_idx);
		 */

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
	int j, k, levels, lfor;
	int num_allowed, num_known;

	spell *s_ptr;

	if (!cp_ptr->uses_magic)
		return;

	/* Hack -- wait for creation */
	if (!character_generated)
		return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra)
		return;

	/* Determine the number of spells allowed */
	levels = p_ptr->lev;

	/* Extract total allowed spells */
	num_allowed =
		(adj_mag_study[p_ptr->stat_ind[cp_ptr->spell_stat]] * levels / 2);

	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */

	for (j = 0; j < spell_num; j++)
	{
		if (!spells[j].unknown)
			num_known++;
	}

	/* 
	 * Handle automatically generated spells.
	 */

	if (cp_ptr->uses_magic && cp_ptr->spell_book == SV_SPELLBOOK_NONE)
	{

		/* Generate some Corrupted spells. */
		if (p_ptr->pclass == CLASS_CORRUPTED)
		{
			k = num_allowed - num_known;

			/* Don't forget known powers -- no cheating! */
			if (k < 1)
				return;

			for (j = 0; j < k; j++)
			{
				spell_generate_new(p_ptr->lev);
			}

			if (k == 1)
			{
				mprint(MSG_BONUS, "You have gained one new power.");
			}
			else
			{
				mformat(MSG_BONUS, "You have gained %d new powers.", k);
			}
		}

		return;
	}


	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;

	/* WARNING. 
	* (This can happen if we suddenly get catastrophically stupid,
	* for example during a shape shift.) */
	if (p_ptr->new_spells < 0) {
	  p_ptr->new_spells = 0;
	}

	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < spell_num; j++)
	{
		s_ptr = &spells[j];

		/* Skip spells we cannot remember */
		if (s_ptr->level > p_ptr->lev || !s_ptr->unknown)
			continue;

		/* Count it */
		k++;
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k)
		p_ptr->new_spells = k;


	/* Spell count has gone down -- forget some spells. */
	if (p_ptr->new_spells < p_ptr->old_spells)
	{
		lfor = p_ptr->old_spells - p_ptr->new_spells;

		/* Forget the spells last on the list. This is a hack, but
		 * that's OK, since forgetting the spell last learned is 
		 * not important. */

		for (j = spell_num - 1; j >= 0 && lfor; j--)
		{
			s_ptr = &spells[j];

			if (!s_ptr->unknown)
			{
				mformat(MSG_STUPID, "You have forgotten the power of %s.",
					s_ptr->name);
				s_ptr->unknown = TRUE;
				lfor--;
			}
		}

		/* Spell count has gone up -- show a message. */
	}
	else if (p_ptr->new_spells > p_ptr->old_spells)
	{

		mformat(MSG_BONUS, "You can learn %d more power%s.",
			p_ptr->new_spells, (p_ptr->new_spells != 1) ? "s" : "");

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;
}




static s16b calc_mana_aux(int i)
{
	object_type *o_ptr = equipment[i];

	if (o_ptr)
		return o_ptr->weight;
	else
		return 0;
}

/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
	int msp, levels, cur_wgt, max_wgt;

	object_type *o_ptr;


	/* Hack -- Must have spells to use. */
	if (spell_num == 0)
		return;


	/* Extract "effective" player level */
	levels = p_ptr->lev;

	/* Extract total mana */
	msp = adj_mag_mana[p_ptr->stat_ind[cp_ptr->spell_stat]] * levels / 2;

	/* Hack -- usually add one mana */
	if (msp)
		msp++;


	/* Only mages and illusionists (and bards) are affected -KMW- */
	if (cp_ptr->spell_book == SV_SPELLBOOK_MAGE ||
		cp_ptr->spell_book == SV_SPELLBOOK_ILLUSIONIST ||
		cp_ptr->spell_book == SV_SPELLBOOK_BARD || p_ptr->mega_spells)
	{
		u32b f1, f2, f3;

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = equipment[EQUIP_HANDS];

		/* Examine the gloves */
		if (o_ptr)
			object_flags(o_ptr, &f1, &f2, &f3);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr && o_ptr->k_idx && !(f3 & TR3_FREE_ACT) &&
			!(f1 & TR1_DEX) && o_ptr->pval > 0)
		{

			/* Encumbered */
			p_ptr->cumber_glove = TRUE;

			/* Reduce mana */
			/* Corrupted get huge reduction. */

			if (p_ptr->mega_spells)
			{
				msp = msp / 4;
			}
			else
			{
				msp = (3 * msp) / 4;
			}
		}
	}

	/* Assume player not encumbered by armor */
	p_ptr->cumber_armor = FALSE;

	/* Weigh the armor */
	cur_wgt = 0;

	cur_wgt += calc_mana_aux(EQUIP_BODY);
	cur_wgt += calc_mana_aux(EQUIP_HEAD);
	cur_wgt += calc_mana_aux(EQUIP_ARM);
	cur_wgt += calc_mana_aux(EQUIP_OUTER);
	cur_wgt += calc_mana_aux(EQUIP_HANDS);
	cur_wgt += calc_mana_aux(EQUIP_FEET);

	/* Determine the weight allowance */
	max_wgt = cp_ptr->spell_weight;

	/* Heavy armor penalizes mana */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Reduce mana */
		/* Corrupted get giant mana descrease. */
		if (p_ptr->mega_spells)
		{
			msp -= (cur_wgt - max_wgt);
		}
		else
		{
			msp -= ((cur_wgt - max_wgt) / 10);
		}
	}

	/* Mana can never be negative */
	if (msp < 0)
		msp = 0;


	/* Corrupted get double mana. XXX */
	if (p_ptr->mega_spells)
		msp *= 2;

	/* Warriors get half mana. */
	if (!cp_ptr->uses_magic)
		msp /= 2;

	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{
		/* Save new limit */
		p_ptr->msp = msp;

		/* Enforce new limit */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_SPELL | PW_PLAYER);
	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra)
		return;

	/* Take note when "glove state" changes */
	if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
			mprint(MSG_WARNING,
				"Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msg_print("Your hands feel more suitable for spellcasting.");
		}

		/* Save it */
		p_ptr->old_cumber_glove = p_ptr->cumber_glove;
	}


	/* Take note when "armor state" changes */
	if (p_ptr->old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (p_ptr->cumber_armor)
		{
			mprint(MSG_WARNING,
				"The weight of your armor encumbers your movement.");
		}
		else
		{
			msg_print("You feel able to move more freely.");
		}

		/* Save it */
		p_ptr->old_cumber_armor = p_ptr->cumber_armor;
	}
}



/*
 * Calculate the players (maximal) hit points
 *
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
	int bonus, mhp;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int) (adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128);

	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->lev - 1] + (bonus * p_ptr->lev / 2);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1)
		mhp = p_ptr->lev + 1;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		/* Save new limit */
		p_ptr->mhp = mhp;

		/* Enforce new limit */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_SPELL | PW_PLAYER);
	}
}

/*
 * Calculate the player's sanity
 */

static void calc_sanity(void)
{
	int bonus, msane;

	/* Hack -- use the con/hp table for sanity/wis */
	bonus = ((int) (adj_con_mhp[p_ptr->stat_ind[A_WIS]]) - 128);

	/* Hack -- assume 5 sanity points per level. */
	msane = 5 * (p_ptr->lev + 1) + (bonus * p_ptr->lev / 2);

	if (msane < p_ptr->lev + 1)
		msane = p_ptr->lev + 1;

	if (p_ptr->msane != msane)
	{

		/* Sanity carries over between levels. */
		p_ptr->csane += (msane - p_ptr->msane);

		p_ptr->msane = msane;

		if (p_ptr->csane >= msane)
		{
			p_ptr->csane = msane;
			p_ptr->csane_frac = 0;
		}

		p_ptr->redraw |= (PR_SANITY);
		p_ptr->window |= (PW_SPELL | PW_PLAYER);
	}
}

/*
 * Extract and set the current "lite radius"
 */
static void calc_torch(void)
{
	object_type *o_ptr = equipment[EQUIP_LITE];

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Player is glowing */
	/* Note: Made the radius much bigger */

	if (p_ptr->lite)
		p_ptr->cur_lite = 3;

	/* Examine actual lites */
	if (o_ptr && o_ptr->tval == TV_LITE)
	{
		/* Torches (with fuel) provide some lite */
		if ((o_ptr->sval == SV_LITE_TORCH) && (o_ptr->pval > 0))
		{
			p_ptr->cur_lite = 1;
		}

		/* Lanterns (with fuel) provide more lite */
		else if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->pval > 0))
		{
			p_ptr->cur_lite = 2;
		}

		/* Fake light sources provide no light. */
		else if (o_ptr->sval == SV_LITE_UNDEATH)
		{
			p_ptr->cur_lite = 0;
		}

		/* Artifact Lites provide permanent, bright, lite */
		else if (artifact_p(o_ptr))
			p_ptr->cur_lite = 3;
	}

	/* Reduce lite when running if requested */
	if (p_ptr->running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1)
			p_ptr->cur_lite = 1;
	}

	/* Notice changes in the "lite radius" */
	if (p_ptr->old_lite != p_ptr->cur_lite)
	{
		/* Update the lite */
		p_ptr->update |= (PU_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Remember the old lite */
		p_ptr->old_lite = p_ptr->cur_lite;
	}
}



/*
 * Computes current weight limit.
 */
static int weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
	i = adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100;

	/* Return the result */
	return (i);
}


/*
 * Give the player a bonus.
 */

static void god_bonus(int god, int goodness, bool do_print)
{

	if (!character_dungeon)
		do_print = FALSE;

	switch (god)
	{
		case 1: /* Night */
			p_ptr->resist_dark = TRUE;
			p_ptr->resist_cold = TRUE;
			if (do_print)
				mprint(MSG_BONUS, "You feel like a prowler of the night.");

			if (goodness > 1)
			{
				p_ptr->sustain_dex = TRUE;
				if (do_print)
					mprint(MSG_BONUS, "You feel strangely protected.");
			}

			if (goodness > 2)
			{
				p_ptr->telepathy = TRUE;
				p_ptr->lite = TRUE;
				p_ptr->aggravate = TRUE;
				if (do_print)
					mprint(MSG_BIG_BONUS,
						"You sense your prey around you.");
			}

			break;

		case 2: /* Day */
			p_ptr->resist_lite = TRUE;
			p_ptr->resist_confu = TRUE;
			if (do_print)
				mprint(MSG_BONUS,
					"You feel fortified against the forces of darkness.");

			if (goodness > 1)
			{
				p_ptr->lite = TRUE;
				if (do_print)
					mprint(MSG_BONUS, "You start to glow in a holy aura.");
			}

			break;

		case 3: /* Shadow */
			p_ptr->hold_life = TRUE;
			p_ptr->resist_blind = TRUE;
			p_ptr->resist_fear = TRUE;
			p_ptr->immaterial = TRUE;
			if (do_print)
				mprint(MSG_BIG_BONUS, "You feel ghostlike.");

			if (goodness > 1)
			{
				p_ptr->resist_dark = TRUE;
				if (do_print)
					mprint(MSG_BONUS,
						"You are protected against darkness.");
			}

			if (goodness > 2)
			{
				p_ptr->resist_cold = TRUE;
				p_ptr->regenerate = TRUE;
				if (do_print)
					mprint(MSG_BIG_BONUS, "You start to regenerate.");
			}
			break;

		case 4: /* Fire */
			p_ptr->resist_fire = TRUE;
			p_ptr->sustain_dex = TRUE;
			if (do_print)
				mprint(MSG_BONUS,
					"You feel the power of fire in your blood.");

			if (goodness > 1)
			{
				p_ptr->stat_add[A_DEX] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel more dextrous.");
			}

			if (goodness > 2)
			{
				p_ptr->immune_fire = TRUE;
				if (do_print)
					mprint(MSG_BIG_BONUS,
						"You feel one with the essence of fire.");
			}
			break;

		case 5: /* Blood */
			p_ptr->hold_life = TRUE;
			p_ptr->resist_pois = TRUE;
			if (do_print)
				mprint(MSG_BIG_BONUS,
					"You feel the blood course in your veins.");

			if (goodness > 1)
			{
				p_ptr->hero += 1000;
				if (do_print)
					mprint(MSG_BONUS, "You feel heroic.");
			}

			if (goodness > 2)
			{
				p_ptr->stat_add[A_DEX] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel more dextrous.");
			}
			break;


		case 6: /* Nature */
			p_ptr->telepathy = TRUE;
			if (do_print)
				mprint(MSG_BIG_BONUS,
					"You become more aware of the world around you.");

			if (goodness > 1)
			{
				p_ptr->see_inv = TRUE;
				if (do_print)
					mprint(MSG_BONUS, "You see through illusions.");
			}

			break;

		case 7: /* Water */
			p_ptr->slow_digest = TRUE;
			if (do_print)
				mprint(MSG_BONUS, "Your body changes subtly.");

			if (goodness > 2)
			{
				p_ptr->resist_pois = TRUE;
				if (do_print)
					mprint(MSG_BONUS, "You are protected from poisons.");
			}

			break;


		case 8: /* Stone */
			p_ptr->ffall = TRUE;
			if (do_print)
				mprint(MSG_BONUS, "Your body feels less heavy.");

			if (goodness > 2)
			{
				p_ptr->resist_shard = TRUE;
				if (do_print)
					mprint(MSG_BONUS, "You resist shards.");
			}

			break;


		case 9: /* Evil */
			p_ptr->aggravate = TRUE;
			p_ptr->impact = TRUE;
			p_ptr->sustain_str = TRUE;
			p_ptr->sustain_con = TRUE;
			p_ptr->hold_life = TRUE;
			if (do_print)
				mprint(MSG_BONUS, "You feel evil seep into your soul.");

			if (goodness > 1)
			{
				p_ptr->see_inv = TRUE;
				p_ptr->telepathy = TRUE;
				if (do_print)
					mprint(MSG_BIG_BONUS, "You sense your prey.");
			}

			if (goodness > 2)
			{
				p_ptr->resist_disen = TRUE;
				p_ptr->resist_chaos = TRUE;
				p_ptr->shero += 1000;
				if (do_print)
					mprint(MSG_BONUS, "You feel fortified.");
			}

			break;


		case 10: /* Good */
			p_ptr->see_inv = TRUE;
			if (do_print)
				mprint(MSG_BONUS, "You see through the guises of evil.");

			if (goodness > 2)
			{
				p_ptr->stat_add[A_WIS] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel wiser.");
			}

			break;

		case 11: /* Chaos */
			p_ptr->regenerate = TRUE;
			p_ptr->resist_chaos = TRUE;
			if (do_print)
				mprint(MSG_BONUS,
					"You feel fortified against forces of chaos.");

			if (goodness > 1)
			{
				p_ptr->stat_add[A_CON] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel healthier.");
			}

			if (goodness > 2)
			{
				p_ptr->hold_life = TRUE;
				if (do_print)
					mprint(MSG_BONUS,
						"You feel your lifeforce grow stronger.");
			}

			break;

		case 12: /* Sun */
			p_ptr->lite = TRUE;
			if (do_print)
				mprint(MSG_BONUS, "You are lit by a holy aura.");

			if (goodness > 1)
			{
				p_ptr->resist_lite = TRUE;
				if (do_print)
					mprint(MSG_BONUS, "You feel fortified against light.");
			}

			if (goodness > 2)
			{
				p_ptr->regenerate = TRUE;
				if (do_print)
					mprint(MSG_BIG_BONUS, "You start to regenerate.");
			}

			break;


		case 13: /* Rage */
			p_ptr->shero += 1000;
			if (do_print)
				mprint(MSG_BONUS, "You feel the rage boiling inside you.");

			if (goodness > 1)
			{
				p_ptr->stat_add[A_STR] += 1;
				p_ptr->stat_add[A_DEX] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel stronger and faster.");
			}

			if (goodness > 2)
			{
				p_ptr->free_act = TRUE;
				p_ptr->resist_blind = TRUE;
				p_ptr->resist_confu = TRUE;
				if (do_print)
					mprint(MSG_BONUS,
						"You resist the tricks of your enemies.");
			}

			break;


		case 14: /* Beast */
			p_ptr->telepathy = TRUE;
			p_ptr->free_act = TRUE;
			p_ptr->see_inv = TRUE;
			if (do_print)
				mprint(MSG_BIG_BONUS,
					"You find your hidden animal instincts.");

			if (goodness > 1)
			{
				p_ptr->regenerate = TRUE;
				if (do_print)
					mprint(MSG_BIG_BONUS, "You start regenerating.");
			}

			if (goodness > 2)
			{
				p_ptr->stat_add[A_STR] += 1;
				p_ptr->stat_add[A_DEX] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel stronger and faster.");
			}

			break;

		case 15: /* Winds */
			p_ptr->sustain_dex = TRUE;
			p_ptr->resist_sound = TRUE;
			p_ptr->free_act = TRUE;
			if (do_print)
				mprint(MSG_BONUS, "You feel one with the wind.");

			if (goodness > 1)
			{
				p_ptr->stat_add[A_DEX] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel more dextrous.");
			}

			if (goodness > 2)
			{
				p_ptr->pspeed += 10;
				if (do_print)
					mprint(MSG_BIG_BONUS, "You feel very fast.");
			}

			break;


		case 16: /* Spheres */
			p_ptr->sustain_int = TRUE;
			p_ptr->sustain_wis = TRUE;
			p_ptr->allseeing = TRUE;
			if (do_print)
				mprint(MSG_BIG_BONUS,
					"Your mind finds new analytical skills.");

			if (goodness > 1)
			{
				p_ptr->stat_add[A_INT] += 1;
				p_ptr->stat_add[A_WIS] += 1;
				if (do_print)
					mprint(MSG_BONUS, "You feel smarter and wiser.");
			}

			if (goodness > 2)
			{
				p_ptr->resist_blind = TRUE;
				p_ptr->resist_confu = TRUE;
				p_ptr->resist_fear = TRUE;
				if (do_print)
					mprint(MSG_BONUS, "You gain clarity of mind.");
			}

			break;


		case 17: /* Time. */
			p_ptr->pspeed += 10;
			if (do_print)
				mprint(MSG_BIG_BONUS, "You feel very fast.");

			if (goodness > 1)
				p_ptr->pspeed += 5;

			if (goodness > 2)
				p_ptr->pspeed += 2;
			break;

		case 18: /* Infinity. */
			p_ptr->stat_add[A_INT] += 1;
			p_ptr->stat_add[A_WIS] += 1;
			if (do_print)
				mprint(MSG_BONUS, "You feel smarter and wiser.");

			if (goodness > 1)
			{
				p_ptr->stat_add[A_INT] += 1;
				p_ptr->stat_add[A_WIS] += 1;
			}

			if (goodness > 2)
			{
				p_ptr->stat_add[A_INT] += 1;
				p_ptr->stat_add[A_WIS] += 1;
			}
			break;

		case 19: /* Change. */
			p_ptr->stat_add[A_INT] += randnor(0, 2);
			p_ptr->stat_add[A_WIS] += randnor(0, 2);
			p_ptr->stat_add[A_STR] += randnor(0, 2);
			p_ptr->stat_add[A_DEX] += randnor(0, 2);
			p_ptr->stat_add[A_CON] += randnor(0, 2);
			p_ptr->stat_add[A_CHR] += randnor(0, 2);

			if (do_print)
				mprint(MSG_BONUS,
					"Your body scrambles in an inperceptible way.");

			if (goodness > 1)
			{
				int x = randnor(0, 3);
				int y = randnor(0, 3);

				p_ptr->to_h += x;
				p_ptr->dis_to_h += x;
				p_ptr->to_d += y;
				p_ptr->dis_to_d += y;
			}

			if (goodness > 2)
			{
				int x = randnor(0, 3);

				p_ptr->to_a += x;
				p_ptr->dis_to_a += x;
			}
			break;

		case 20: /* Stasis. */
			p_ptr->sustain_str = TRUE;
			p_ptr->sustain_int = TRUE;
			p_ptr->sustain_wis = TRUE;
			p_ptr->sustain_con = TRUE;
			p_ptr->sustain_dex = TRUE;
			p_ptr->sustain_chr = TRUE;

			if (do_print)
				mprint(MSG_BIG_BONUS, "You feel strangely more solid.");

			if (goodness > 1)
			{
				p_ptr->resist_fire = TRUE;
				p_ptr->resist_cold = TRUE;
				p_ptr->resist_fear = TRUE;
			}

			if (goodness > 2)
			{
				p_ptr->resist_acid = TRUE;
				p_ptr->resist_elec = TRUE;
				p_ptr->resist_pois = TRUE;
			}

			break;

		case 21: /* Particles. */
		case 24: /* Matter. */
			p_ptr->resist_chaos = TRUE;
			p_ptr->resist_shard = TRUE;
			p_ptr->resist_acid = TRUE;
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_pois = TRUE;

			if (do_print)
				mprint(MSG_BIG_BONUS, "You feel protected in a big way.");
			break;

		case 22: /* Continuity. */
			p_ptr->resist_fear = TRUE;
			p_ptr->resist_confu = TRUE;
			p_ptr->resist_disen = TRUE;
			p_ptr->resist_nexus = TRUE;
			p_ptr->resist_nethr = TRUE;
			p_ptr->resist_blind = TRUE;

			if (do_print)
				mprint(MSG_BIG_BONUS, "You feel protected in a big way.");
			break;

		case 23: /* Energy. */
			p_ptr->resist_elec = TRUE;
			p_ptr->resist_fire = TRUE;
			p_ptr->resist_lite = TRUE;
			p_ptr->resist_dark = TRUE;
			p_ptr->resist_sound = TRUE;

			if (do_print)
				mprint(MSG_BIG_BONUS, "You feel protected in a big way.");
			break;

		case 25: /* Being. */
			p_ptr->stat_add[A_CHR] += 15;
			p_ptr->to_h -= 50;
			p_ptr->dis_to_d -= 50;
			p_ptr->to_h -= 50;
			p_ptr->dis_to_d -= 50;
			p_ptr->lite = TRUE;
			p_ptr->regenerate = TRUE;

			if (do_print)
				mprint(MSG_BONUS,
					"You have been branded with the Mark of "
					"Eternal Being.");
			break;

		case 26: /* Unbeing. */
			p_ptr->stat_add[A_CHR] -= 15;
			p_ptr->to_h += 30;
			p_ptr->dis_to_d += 30;
			p_ptr->to_h += 30;
			p_ptr->dis_to_d += 30;
			p_ptr->exp_drain = TRUE;
			p_ptr->teleport = TRUE;

			if (do_print)
				mprint(MSG_BONUS,
					"You have been branded with the Mark of "
					"Eternal Unbeing.");
			break;

	}
}


static void god_effect(int god, int badness)
{
	cptr name = deity_info[god - 1].name;
	bool do_print = FALSE;

	if (badness != old_badness)
	{
		do_print = TRUE;
	}

	if (badness > 7)
	{
		god_bonus(god, badness - 7, do_print);
		old_badness = badness;
		return;
	}

	if (p_ptr->depth == 0)
	{
		return;
	}

	switch (badness)
	{
		case 7:
			if (do_print)
				mformat(MSG_BONUS,
					"%s has noticed you. You feel enlightened.", name);
			break;
		case 6:
			if (do_print)
				mformat(MSG_WARNING, "You feel like %s is watching you.",
					name);
			break;
		case 5:
			mformat(MSG_URGENT, "%s is furious.", name);
			take_hit(5, "Godly wrath.");
			break;
		case 4:
			godly_wrath_blast(god - 1);
			break;
		case 3:
			mformat(MSG_URGENT,
				"You are struck by the malevolent will of %s!", name);
			p_ptr->paralyzed += 10;
			p_ptr->confused += 10;
			p_ptr->redraw |= PR_CONFUSED;
			p_ptr->redraw |= PR_STATE;
			handle_stuff();
			break;
		case 2:
			mprint(MSG_URGENT,
				"Your mind spins as horrible thoughts fill your mind.");
			p_ptr->paralyzed += 15;
			p_ptr->confused += 15;
			p_ptr->stun += 15;
			p_ptr->image += 15;
			p_ptr->redraw |= PR_CONFUSED;
			p_ptr->redraw |= PR_STUN;
			p_ptr->redraw |= PR_STATE;
			handle_stuff();
			break;
		case 1:
		{
			char killer[80];

			mformat(MSG_URGENT, "%s sends curses on you.", name);

			sprintf(killer, "Godly Wrath of %s", name);
			take_hit(damroll(5, 5), killer);
			break;
		}

		case 0:
			mformat(MSG_URGENT, "%s sends an unholy plague on you.", name);
			nasty_side_effect();
			break;
	}

	old_badness = badness;
}


/* Apply bonuses based on a mutation. */

static void process_mutation(int mut)
{
	switch (mut)
	{
		case MUT_PLUS_INT:
			p_ptr->stat_add[A_INT] += 5;
			break;

		case MUT_PLUS_STR:
			p_ptr->stat_add[A_STR] += 5;
			break;

		case MUT_PLUS_CON:
			p_ptr->stat_add[A_CON] += 5;
			break;

		case MUT_PLUS_WIS:
			p_ptr->stat_add[A_WIS] += 5;
			break;

		case MUT_PLUS_CHR:
			p_ptr->stat_add[A_CHR] += 5;
			break;

		case MUT_PLUS_DEX:
			p_ptr->stat_add[A_DEX] += 5;
			break;

		case MUT_PLUS_SPEED:
			p_ptr->pspeed += 10;
			break;

		case MUT_PLUS_STEALTH:
			p_ptr->skill_stl += 10;
			break;

		case MUT_PLUS_AC:
			p_ptr->to_a += 25;
			p_ptr->dis_to_a += 25;
			break;

		case MUT_ESP:
			p_ptr->telepathy = TRUE;
			break;

		case MUT_RES_FEAR:
			p_ptr->resist_fear = TRUE;
			break;

		case MUT_GLOW:
			p_ptr->lite = TRUE;
			break;

		case MUT_RES_CONF:
			p_ptr->resist_confu = TRUE;
			break;

		case MUT_IMMATERIAL:
			p_ptr->immaterial = TRUE;
			break;

		case MUT_MINUS_INT:
			p_ptr->stat_add[A_INT] -= 5;
			break;

		case MUT_MINUS_STR:
			p_ptr->stat_add[A_STR] -= 5;
			break;

		case MUT_MINUS_CON:
			p_ptr->stat_add[A_CON] -= 5;
			break;

		case MUT_MINUS_WIS:
			p_ptr->stat_add[A_WIS] -= 5;
			break;

		case MUT_MINUS_CHR:
			p_ptr->stat_add[A_CHR] -= 5;
			break;

		case MUT_MINUS_DEX:
			p_ptr->stat_add[A_DEX] -= 5;
			break;

		case MUT_MINUS_SPEED:
			p_ptr->pspeed -= 10;
			break;

		case MUT_AGGRAVATE:
			p_ptr->aggravate = TRUE;
			break;

		case MUT_MINUS_AC:
			p_ptr->to_a -= 25;
			p_ptr->dis_to_a -= 25;
			break;

		case MUT_BLIND:
			p_ptr->perma_blind = TRUE;
			break;

		case MUT_COWARD:
			if (randint(10) == 1 && !p_ptr->resist_fear &&
				character_generated)
			{
				set_afraid(p_ptr->afraid + damroll(5, 5));
			}
			break;

		case MUT_HALLUC:
			if (randint(10) == 1 && character_generated)
			{
				set_image(p_ptr->image + damroll(5, 5));
			}
			break;

		case MUT_CONFUSED:
			if (randint(10) == 1 && !p_ptr->resist_confu &&
				character_generated)
			{
				set_confused(p_ptr->confused + damroll(5, 5));
			}
			break;

		case MUT_LEGLESS:
			p_ptr->immovable = TRUE;
			break;

		case MUT_FLYING:
			p_ptr->flying = TRUE;
			break;

		case MUT_VAMPIRIC:
			p_ptr->vampiric = TRUE;
			break;

		case MUT_WEIRD_ATTACKS:
			p_ptr->weird_attack = TRUE;
			break;

		case MUT_MAPPING:
			p_ptr->allseeing = TRUE;
			break;

		case MUT_IMM_FIRE:
			p_ptr->immune_fire = TRUE;
			break;

		case MUT_IMM_ELEC:
			p_ptr->immune_elec = TRUE;
			break;

		case MUT_IMM_ACID:
			p_ptr->immune_acid = TRUE;
			break;

		case MUT_IMM_COLD:
			p_ptr->immune_cold = TRUE;
			break;

		case MUT_SEE_INVIS:
			p_ptr->see_inv = TRUE;
			break;

		case MUT_FREE_ACT:
			p_ptr->free_act = TRUE;
			break;

		case MUT_SLOW_DIGEST:
			p_ptr->slow_digest = TRUE;
			break;

		case MUT_REGENERATE:
			p_ptr->regenerate = TRUE;
			break;

		case MUT_FEATHER_FALL:
			p_ptr->ffall = TRUE;
			break;

		case MUT_HOLD_LIFE:
			p_ptr->hold_life = TRUE;
			break;

		case MUT_RES_BLIND:
			p_ptr->resist_blind = TRUE;
			break;

		case MUT_RES_POIS:
			p_ptr->resist_pois = TRUE;
			break;

		case MUT_RES_ACID:
			p_ptr->resist_acid = TRUE;
			break;

		case MUT_RES_ELEC:
			p_ptr->resist_elec = TRUE;
			break;

		case MUT_RES_FIRE:
			p_ptr->resist_fire = TRUE;
			break;

		case MUT_RES_LIGHT:
			p_ptr->resist_lite = TRUE;
			break;

		case MUT_RES_DARK:
			p_ptr->resist_dark = TRUE;
			break;

		case MUT_RES_SOUND:
			p_ptr->resist_sound = TRUE;
			break;

		case MUT_RES_CHAOS:
			p_ptr->resist_chaos = TRUE;
			break;

		case MUT_RES_DISEN:
			p_ptr->resist_disen = TRUE;
			break;

		case MUT_RES_SHARD:
			p_ptr->resist_shard = TRUE;
			break;

		case MUT_RES_NEXUS:
			p_ptr->resist_nexus = TRUE;
			break;

		case MUT_RES_NETHER:
			p_ptr->resist_nethr = TRUE;
			break;

		case MUT_POISONED:
			if (randint(10) == 1 && !p_ptr->resist_pois &&
				character_generated)
			{
				set_poisoned(p_ptr->poisoned + damroll(5, 5));
			}
			break;

		case MUT_PARALYZED:
			if (randint(10) == 1 && !p_ptr->free_act &&
				character_generated)
			{
				set_paralyzed(p_ptr->paralyzed + damroll(5, 5));
			}
			break;

		case MUT_BLEEDING:
			if (randint(10) == 1 && character_generated)
			{
				set_cut(p_ptr->cut + damroll(5, 5));
			}
			break;

		case MUT_PARASITES:
			if (randint(10) == 1 && character_generated)
			{
				set_food(p_ptr->food - damroll(25, 5));
			}
			break;

		case MUT_STUNNED:
			if (randint(10) == 1 && character_generated)
			{
				set_stun(p_ptr->stun + damroll(5, 5));
			}
			break;

		case MUT_TELEPORT:
			p_ptr->teleport = TRUE;
			break;

		case MUT_EXP_DRAIN:
			p_ptr->exp_drain = TRUE;
			break;

		case MUT_MINUS_DEVICES:
			p_ptr->skill_dev -= 10;
			break;

		case MUT_MINUS_SAVE:
			p_ptr->skill_sav -= 10;
			break;

		case MUT_MINUS_STEALTH:
			p_ptr->skill_stl -= 10;
			break;

		case MUT_MINUS_FIGHT:
			p_ptr->skill_thn -= 10;
			p_ptr->skill_thb -= 10;
			p_ptr->skill_tht -= 10;
			break;
	}
}

/* Apply bonuses based on current shape. */

static void process_shape_shift(int shape)
{

	switch (shape)
	{
		case SHAPE_ABOMINATION:
			p_ptr->stat_add[A_STR] -= 10;
			p_ptr->stat_add[A_DEX] -= 10;
			p_ptr->stat_add[A_INT] -= 10;
			p_ptr->stat_add[A_WIS] -= 10;
			p_ptr->stat_add[A_CON] -= 10;
			p_ptr->stat_add[A_CHR] -= 10;
			p_ptr->to_h -= 24;
			p_ptr->dis_to_h -= 24;
			p_ptr->to_a -= 24;
			p_ptr->dis_to_a -= 24;
			p_ptr->to_d -= 24;
			p_ptr->dis_to_d -= 24;
			break;

		case SHAPE_WOLF:
			p_ptr->stat_add[A_STR] += 10;
			p_ptr->stat_add[A_DEX] += 10;
			p_ptr->stat_add[A_INT] -= 20;
			p_ptr->stat_add[A_WIS] -= 20;
			p_ptr->aggravate = TRUE;
			p_ptr->resist_fear = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->to_h += 24;
			p_ptr->dis_to_h += 24;
			p_ptr->to_a -= 10;
			p_ptr->dis_to_a -= 10;
			break;

		case SHAPE_GHOST:
			p_ptr->immaterial = TRUE;
			p_ptr->telepathy = TRUE;
			p_ptr->stat_add[A_STR] -= 10;
			p_ptr->stat_add[A_CON] -= 10;
			p_ptr->stat_add[A_WIS] += 5;
			break;

		case SHAPE_INSECT:
			p_ptr->pspeed += 25;
			p_ptr->stat_add[A_STR] -= 10;
			p_ptr->stat_add[A_DEX] -= 10;
			p_ptr->stat_add[A_INT] -= 10;
			p_ptr->stat_add[A_WIS] -= 10;
			p_ptr->stat_add[A_CON] -= 10;
			p_ptr->stat_add[A_CHR] -= 10;
			p_ptr->to_h -= 24;
			p_ptr->dis_to_h -= 24;
			p_ptr->to_a += 60;
			p_ptr->dis_to_a += 60;
			p_ptr->to_d -= 60;
			p_ptr->dis_to_d -= 60;
			break;

		case SHAPE_GOAT:
			p_ptr->stat_add[A_INT] -= 20;
			p_ptr->stat_add[A_WIS] -= 10;
			p_ptr->pspeed += 5;
			p_ptr->resist_confu = TRUE;
			p_ptr->resist_fear = TRUE;
			p_ptr->to_d += 5;
			p_ptr->dis_to_d += 5;
			break;

		case SHAPE_APE:
			p_ptr->stat_add[A_INT] -= 5;
			p_ptr->stat_add[A_STR] += 7;
			p_ptr->pspeed -= 3;
			p_ptr->to_h += 15;
			p_ptr->to_d += 5;
			p_ptr->dis_to_h += 15;
			p_ptr->dis_to_d += 5;
			break;

		case SHAPE_STATUE:
			p_ptr->stat_add[A_DEX] -= 25;
			p_ptr->stat_add[A_CON] += 20;
			p_ptr->pspeed -= 40;
			p_ptr->to_h -= 50;
			p_ptr->dis_to_h -= 50;
			p_ptr->to_a += 100;
			p_ptr->dis_to_a += 100;
			break;

		case SHAPE_CHAOS_CLOUD:
			p_ptr->stat_add[A_STR] -= 25;
			p_ptr->stat_add[A_DEX] += 15;
			p_ptr->to_h += 10;
			p_ptr->dis_to_h += 10;
			p_ptr->to_d -= 15;
			p_ptr->dis_to_d -= 15;
			p_ptr->weird_attack = TRUE;
			p_ptr->resist_chaos = TRUE;
			p_ptr->engulfs = TRUE;
			break;

		case SHAPE_SPARROW:
			p_ptr->pspeed += 15;
			p_ptr->stat_add[A_STR] -= 10;
			p_ptr->stat_add[A_DEX] -= 10;
			p_ptr->stat_add[A_INT] -= 10;
			p_ptr->stat_add[A_WIS] -= 10;
			p_ptr->stat_add[A_CON] -= 10;
			p_ptr->stat_add[A_CHR] -= 10;
			p_ptr->to_h -= 24;
			p_ptr->dis_to_h -= 24;
			p_ptr->to_a += 60;
			p_ptr->dis_to_a += 60;
			p_ptr->to_d -= 60;
			p_ptr->dis_to_d -= 60;
			p_ptr->flying = TRUE;
			break;

		case SHAPE_KOBOLD:
			p_ptr->resist_pois = TRUE;
			break;

		case SHAPE_FIRE_CLOUD:
			p_ptr->stat_add[A_STR] -= 25;
			p_ptr->stat_add[A_DEX] += 15;
			p_ptr->to_h += 10;
			p_ptr->dis_to_h += 10;
			p_ptr->to_d -= 15;
			p_ptr->dis_to_d -= 15;
			p_ptr->immune_fire = TRUE;
			p_ptr->engulfs = TRUE;
			break;

		case SHAPE_COLD_CLOUD:
			p_ptr->stat_add[A_STR] -= 25;
			p_ptr->stat_add[A_DEX] += 15;
			p_ptr->to_h += 10;
			p_ptr->dis_to_h += 10;
			p_ptr->to_d -= 15;
			p_ptr->dis_to_d -= 15;
			p_ptr->immune_cold = TRUE;
			p_ptr->engulfs = TRUE;
			break;

		case SHAPE_DRAGON:
			p_ptr->stat_add[A_STR] += 25;
			p_ptr->to_d += 15;
			p_ptr->dis_to_d += 15;
			break;

		case SHAPE_DEMON:
			p_ptr->hold_life = TRUE;
			p_ptr->telepathy = TRUE;
			p_ptr->regenerate = TRUE;
			p_ptr->impact = TRUE;
			break;

		case SHAPE_HOUND:
			p_ptr->resist_nethr = TRUE;
			p_ptr->resist_nexus = TRUE;
			p_ptr->resist_disen = TRUE;
			p_ptr->resist_shard = TRUE;
			p_ptr->resist_sound = TRUE;
			break;

		case SHAPE_VAMPIRE:
			p_ptr->vampiric = TRUE;
			break;

		case SHAPE_QUYL:
			p_ptr->stat_add[A_STR] -= 25;
			p_ptr->stat_add[A_DEX] += 15;
			p_ptr->to_h += 10;
			p_ptr->dis_to_h += 10;
			p_ptr->to_d -= 15;
			p_ptr->dis_to_d -= 15;
			break;

		case SHAPE_ANGEL:
			p_ptr->resist_lite = TRUE;
			p_ptr->resist_dark = TRUE;
			p_ptr->resist_fear = TRUE;
			p_ptr->resist_confu = TRUE;
			p_ptr->resist_nexus = TRUE;
			p_ptr->resist_nethr = TRUE;
			p_ptr->resist_elec = TRUE;
			p_ptr->resist_acid = TRUE;
			p_ptr->resist_fire = TRUE;
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->resist_shard = TRUE;
			p_ptr->resist_disen = TRUE;
			p_ptr->resist_sound = TRUE;
			p_ptr->resist_blind = TRUE;
			break;

		case SHAPE_SERPENT:
			p_ptr->pspeed += 15;
			p_ptr->to_h += 30;
			p_ptr->dis_to_h += 30;
			p_ptr->resist_dark = TRUE;
			p_ptr->resist_pois = TRUE;
			break;

		case SHAPE_MANA_BALL:
			p_ptr->stat_add[A_STR] -= 25;
			p_ptr->stat_add[A_DEX] += 15;
			p_ptr->to_h += 10;
			p_ptr->dis_to_h += 10;
			p_ptr->to_d -= 15;
			p_ptr->dis_to_d -= 15;
			p_ptr->mega_spells = TRUE;
			p_ptr->engulfs = TRUE;
			break;

		case SHAPE_GIANT:
			p_ptr->impact = TRUE;
			p_ptr->stat_add[A_STR] += 50;
			p_ptr->to_d += 30;
			p_ptr->dis_to_d += 30;
			break;

		case SHAPE_SPIDER:
			p_ptr->pspeed += 20;
			p_ptr->resist_pois = TRUE;
			break;

		case SHAPE_MOLD:
			p_ptr->stat_add[A_STR] += 10;
			p_ptr->stat_add[A_DEX] += 10;
			p_ptr->stat_add[A_INT] += 10;
			p_ptr->stat_add[A_WIS] += 10;
			p_ptr->stat_add[A_CON] += 10;
			p_ptr->stat_add[A_CHR] += 10;
			p_ptr->immovable = TRUE;
			break;

		case SHAPE_ZOMBIE:
			p_ptr->stat_add[A_INT] -= 25;
			p_ptr->stat_add[A_WIS] -= 25;
			p_ptr->stat_add[A_CON] += 25;
			p_ptr->resist_nethr = TRUE;
			p_ptr->resist_nexus = TRUE;
			p_ptr->resist_disen = TRUE;
			p_ptr->resist_cold = TRUE;
			break;

		case SHAPE_WRAITH:
			p_ptr->stat_add[A_STR] += 30;
			p_ptr->stat_add[A_DEX] += 30;
			p_ptr->stat_add[A_INT] += 30;
			p_ptr->stat_add[A_WIS] += 30;
			p_ptr->stat_add[A_CON] += 30;
			p_ptr->stat_add[A_CHR] += 30;
			p_ptr->munchkin = TRUE;
			break;
	}
}

/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
static void calc_bonuses(void)
{
	int i, j, hold;

	int old_speed;

	int old_telepathy;
	int old_see_inv;
	int old_perma_blind;

	int old_dis_ac;
	int old_dis_to_a;

	int extra_blows;
	int extra_shots;
	int extra_might;

	int old_stat_top[6];
	int old_stat_use[6];
	int old_stat_ind[6];

	object_type *o_ptr;

	u32b f1, f2, f3;


	/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;
	old_perma_blind = p_ptr->perma_blind;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	/* Save the old stats */
	for (i = 0; i < 6; i++)
	{
		old_stat_top[i] = p_ptr->stat_top[i];
		old_stat_use[i] = p_ptr->stat_use[i];
		old_stat_ind[i] = p_ptr->stat_ind[i];
	}


	/*** Reset ***/

	/* Reset player speed */
	p_ptr->pspeed = 110;

	/* Reset "blow" info */
	p_ptr->num_blow = 1;
	extra_blows = 0;

	/* Reset "fire" info */
	p_ptr->num_fire = 0;
	p_ptr->ammo_mult = 0;
	p_ptr->ammo_tval = 0;
	extra_shots = 0;
	extra_might = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++)
		p_ptr->stat_add[i] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->lite = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_chr = FALSE;
	p_ptr->resist_acid = FALSE;
	p_ptr->resist_elec = FALSE;
	p_ptr->resist_fire = FALSE;
	p_ptr->resist_cold = FALSE;
	p_ptr->resist_pois = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->resist_lite = FALSE;
	p_ptr->resist_dark = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_confu = FALSE;
	p_ptr->resist_sound = FALSE;
	p_ptr->resist_chaos = FALSE;
	p_ptr->resist_disen = FALSE;
	p_ptr->resist_shard = FALSE;
	p_ptr->resist_nexus = FALSE;
	p_ptr->resist_nethr = FALSE;
	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;

	p_ptr->immaterial = FALSE;
	p_ptr->allseeing = FALSE;
	p_ptr->munchkin = FALSE;
	p_ptr->weird_attack = FALSE;
	p_ptr->immovable = FALSE;
	p_ptr->mega_spells = FALSE;
	p_ptr->vampiric = FALSE;
	p_ptr->flying = FALSE;
	p_ptr->perma_blind = FALSE;
	p_ptr->hates_light = FALSE;
	p_ptr->no_eating = FALSE;
	p_ptr->true_vampirism = FALSE;
	p_ptr->no_equip = FALSE;
	p_ptr->engulfs = FALSE;
	p_ptr->fated = FALSE;

	/*** Extract race/class info ***/

	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra;

	/* Base skill -- disarming */
	p_ptr->skill_dis = rp_ptr->r_dis + cp_ptr->c_dis;

	/* Base skill -- magic devices */
	p_ptr->skill_dev = rp_ptr->r_dev + cp_ptr->c_dev;

	/* Base skill -- saving throw */
	p_ptr->skill_sav = rp_ptr->r_sav + cp_ptr->c_sav;

	/* Base skill -- stealth */
	p_ptr->skill_stl = rp_ptr->r_stl + cp_ptr->c_stl;

	/* Base skill -- searching ability */
	p_ptr->skill_srh = rp_ptr->r_srh + cp_ptr->c_srh;

	/* Base skill -- searching frequency */
	p_ptr->skill_fos = rp_ptr->r_fos + cp_ptr->c_fos;

	/* Base skill -- combat (normal) */
	p_ptr->skill_thn = rp_ptr->r_thn + cp_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- digging */
	p_ptr->skill_dig = 0;

	/* Elf */
	if (p_ptr->prace == RACE_ELF)
		p_ptr->resist_lite = TRUE;

	/* Hobbit */
	if (p_ptr->prace == RACE_HOBBIT)
		p_ptr->sustain_dex = TRUE;

	/* Gnome */
	if (p_ptr->prace == RACE_GNOME)
		p_ptr->free_act = TRUE;

	/* Dwarf */
	if (p_ptr->prace == RACE_DWARF)
		p_ptr->resist_blind = TRUE;

	/* Half-Orc */
	if (p_ptr->prace == RACE_HALF_ORC)
		p_ptr->resist_dark = TRUE;

	/* Half-Troll */
	if (p_ptr->prace == RACE_HALF_TROLL)
		p_ptr->sustain_str = TRUE;

	/* Dunadan */
	if (p_ptr->prace == RACE_DUNADAN)
		p_ptr->sustain_con = TRUE;

	/* High Elf */
	if (p_ptr->prace == RACE_HIGH_ELF)
		p_ptr->resist_lite = TRUE;
	if (p_ptr->prace == RACE_HIGH_ELF)
		p_ptr->see_inv = TRUE;

	/* Kobold  From GJW -KMW- */
	if (p_ptr->prace == RACE_KOBOLD)
		p_ptr->resist_pois = TRUE;

	/* Mutant XXX */

	if (p_ptr->prace == RACE_MUTANT)
	{
		p_ptr->resist_chaos = TRUE;

		switch (p_ptr->prace_info)
		{
			case 1:
				p_ptr->resist_pois = TRUE;
				break;
			case 2:
				p_ptr->resist_blind = TRUE;
				break;
			case 3:
				p_ptr->resist_acid = TRUE;
				break;
			case 4:
				p_ptr->resist_elec = TRUE;
				break;
			case 5:
				p_ptr->resist_fear = TRUE;
				break;
			default:
				break;
		}

	}

	if (p_ptr->prace == RACE_GOLEM)
	{
		p_ptr->resist_fear = TRUE;
		p_ptr->resist_confu = TRUE;
		p_ptr->to_h += 25;
		p_ptr->to_d += 25;
		p_ptr->to_a += 25;
		p_ptr->dis_to_h += 25;
		p_ptr->dis_to_d += 25;
		p_ptr->dis_to_a += 25;
		p_ptr->pspeed -= 10;
	}

	if (p_ptr->prace == RACE_LEPRECHAUN)
	{
		p_ptr->sustain_dex = TRUE;
		p_ptr->resist_blind = TRUE;
		p_ptr->pspeed += 10;
		p_ptr->weird_attack = TRUE;
	}

	if (p_ptr->prace == RACE_MOLD)
	{
		p_ptr->immovable = TRUE;
		p_ptr->resist_nethr = TRUE;
		p_ptr->resist_nexus = TRUE;
		p_ptr->hold_life = TRUE;
	}

	if (p_ptr->prace == RACE_VORTEX)
	{
		p_ptr->immune_fire = TRUE;
		p_ptr->immune_acid = TRUE;
		p_ptr->immune_elec = TRUE;
		p_ptr->immune_cold = TRUE;
		p_ptr->lite = TRUE;
		p_ptr->no_equip = TRUE;
		p_ptr->engulfs = TRUE;

		p_ptr->pspeed += (p_ptr->lev / 2);
	}

	if (p_ptr->pclass == CLASS_ELEMENTAL)
	{
		p_ptr->no_equip = TRUE;

		/* Ugly hack -- give resistances with levels. */
		switch (p_ptr->lev / 2)
		{
			case 16:
				p_ptr->resist_disen = TRUE;
			case 15:
				p_ptr->resist_nexus = TRUE;
			case 14:
				p_ptr->resist_nethr = TRUE;
			case 13:
				p_ptr->resist_dark = TRUE;
			case 12:
				p_ptr->resist_lite = TRUE;
			case 11:
				p_ptr->resist_pois = TRUE;
			case 10:
				p_ptr->resist_chaos = TRUE;
			case 9:
				p_ptr->resist_sound = TRUE;
			case 8:
				p_ptr->resist_blind = TRUE;
			case 7:
				p_ptr->resist_shard = TRUE;
			case 6:
				p_ptr->resist_confu = TRUE;
			case 5:
				p_ptr->resist_fear = TRUE;
			case 4:
				p_ptr->resist_acid = TRUE;
			case 3:
				p_ptr->resist_elec = TRUE;
			case 2:
				p_ptr->resist_fire = TRUE;
			case 1:
				p_ptr->resist_cold = TRUE;
		}
	}

	/* Warrior */
	if (p_ptr->pclass == CLASS_WARRIOR)
	{
		if (p_ptr->lev >= 30)
			p_ptr->resist_fear = TRUE;
	}

	if (p_ptr->pclass == CLASS_CORRUPTED)
	{
		p_ptr->mega_spells = TRUE;
	}

	/* Note that the munchkinish powers also apply to ghosts. 
	 * (See below.) */
	if (p_ptr->prace == RACE_GHOST)
	{
		p_ptr->hold_life = TRUE;
		p_ptr->resist_nethr = TRUE;
	}

	if (p_ptr->pclass == CLASS_VAMPIRE)
	{
		p_ptr->hates_light = TRUE;
		p_ptr->no_eating = TRUE;
		p_ptr->true_vampirism = TRUE;
		p_ptr->resist_dark = TRUE;
	}

	if (p_ptr->pclass == CLASS_AVATAR) {
	  
	  p_ptr->fated = TRUE;
	}


	/* Shape-shift */

	if (p_ptr->shape)
	{
		process_shape_shift(p_ptr->shape);
	}

	/* Calculate bonuses due to gods. */
	if (p_ptr->pgod)
	{
		int badness = interpret_grace();

		god_effect(p_ptr->pgod, badness);
	}

	/*** Analyze equipment ***/

	/* Scan the equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = equipment[i];

		/* Skip non-objects */
		if (!o_ptr || !o_ptr->k_idx)
			continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stats */
		if (f1 & (TR1_STR))
			p_ptr->stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT))
			p_ptr->stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS))
			p_ptr->stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX))
			p_ptr->stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON))
			p_ptr->stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_CHR))
			p_ptr->stat_add[A_CHR] += o_ptr->pval;

		/* Affect stealth */
		if (f1 & (TR1_STEALTH))
			p_ptr->skill_stl += o_ptr->pval;

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_SEARCH))
			p_ptr->skill_srh += (o_ptr->pval * 5);

		/* Affect searching frequency (factor of five) */
		if (f1 & (TR1_SEARCH))
			p_ptr->skill_fos += (o_ptr->pval * 5);

		/* Affect infravision */
		if (f1 & (TR1_INFRA))
			p_ptr->see_infra += o_ptr->pval;

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL))
			p_ptr->skill_dig += (o_ptr->pval * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED))
			p_ptr->pspeed += o_ptr->pval;

		/* Affect blows */
		if (f1 & (TR1_BLOWS))
			extra_blows += o_ptr->pval;

		/* Affect shots */
		if (f1 & (TR1_SHOTS))
			extra_shots += o_ptr->pval;

		/* Affect Might */
		if (f1 & (TR1_MIGHT))
			extra_might += o_ptr->pval;

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST))
			p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_FEATHER))
			p_ptr->ffall = TRUE;
		if (f3 & (TR3_LITE))
			p_ptr->lite = TRUE;
		if (f3 & (TR3_REGEN))
			p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY))
			p_ptr->telepathy = TRUE;
		if (f3 & (TR3_SEE_INVIS))
			p_ptr->see_inv = TRUE;
		if (f3 & (TR3_FREE_ACT))
			p_ptr->free_act = TRUE;
		if (f3 & (TR3_HOLD_LIFE))
			p_ptr->hold_life = TRUE;

		/* Weird flags */
		if (f3 & (TR3_BLESSED))
			p_ptr->bless_blade = TRUE;
		if (f3 & (TR3_MUNCHKINISH))
			p_ptr->munchkin = TRUE;
		if (f3 & (TR3_WEIRD_ATTACK))
			p_ptr->weird_attack = TRUE;
		if (f3 & (TR3_FLYING))
			p_ptr->flying = TRUE;
		if (f3 & (TR3_VAMPIRIC))
			p_ptr->vampiric = TRUE;

		/* Bad flags */
		if (f3 & (TR3_IMPACT))
			p_ptr->impact = TRUE;
		if (f3 & (TR3_AGGRAVATE))
			p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT))
			p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP))
			p_ptr->exp_drain = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE))
			p_ptr->immune_fire = TRUE;
		if (f2 & (TR2_IM_ACID))
			p_ptr->immune_acid = TRUE;
		if (f2 & (TR2_IM_COLD))
			p_ptr->immune_cold = TRUE;
		if (f2 & (TR2_IM_ELEC))
			p_ptr->immune_elec = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID))
			p_ptr->resist_acid = TRUE;
		if (f2 & (TR2_RES_ELEC))
			p_ptr->resist_elec = TRUE;
		if (f2 & (TR2_RES_FIRE))
			p_ptr->resist_fire = TRUE;
		if (f2 & (TR2_RES_COLD))
			p_ptr->resist_cold = TRUE;
		if (f2 & (TR2_RES_POIS))
			p_ptr->resist_pois = TRUE;
		if (f2 & (TR2_RES_FEAR))
			p_ptr->resist_fear = TRUE;
		if (f2 & (TR2_RES_LITE))
			p_ptr->resist_lite = TRUE;
		if (f2 & (TR2_RES_DARK))
			p_ptr->resist_dark = TRUE;
		if (f2 & (TR2_RES_BLIND))
			p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_CONFU))
			p_ptr->resist_confu = TRUE;
		if (f2 & (TR2_RES_SOUND))
			p_ptr->resist_sound = TRUE;
		if (f2 & (TR2_RES_SHARD))
			p_ptr->resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS))
			p_ptr->resist_nexus = TRUE;
		if (f2 & (TR2_RES_NETHR))
			p_ptr->resist_nethr = TRUE;
		if (f2 & (TR2_RES_CHAOS))
			p_ptr->resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN))
			p_ptr->resist_disen = TRUE;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR))
			p_ptr->sustain_str = TRUE;
		if (f2 & (TR2_SUST_INT))
			p_ptr->sustain_int = TRUE;
		if (f2 & (TR2_SUST_WIS))
			p_ptr->sustain_wis = TRUE;
		if (f2 & (TR2_SUST_DEX))
			p_ptr->sustain_dex = TRUE;
		if (f2 & (TR2_SUST_CON))
			p_ptr->sustain_con = TRUE;
		if (f2 & (TR2_SUST_CHR))
			p_ptr->sustain_chr = TRUE;

		if (f2 & (TR2_PLUS_TO_AC))
		{
			if (object_known_p(o_ptr))
				p_ptr->dis_to_a += o_ptr->pval;

			p_ptr->to_a += o_ptr->pval;
		}

		if (f2 & (TR2_PLUS_TO_HIT))
		{
			if (object_known_p(o_ptr))
				p_ptr->dis_to_h += o_ptr->pval;

			p_ptr->to_h += o_ptr->pval;
		}

		if (f2 & (TR2_PLUS_TO_DAM))
		{
			if (object_known_p(o_ptr))
				p_ptr->dis_to_d += o_ptr->pval;

			p_ptr->to_d += o_ptr->pval;
		}

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr))
			p_ptr->dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == EQUIP_WIELD)
			continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == EQUIP_BOW)
			continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h += o_ptr->to_h;
		p_ptr->to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr))
			p_ptr->dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr))
			p_ptr->dis_to_d += o_ptr->to_d;
	}

	/*** Give munchkinish powers. ***/

	if ((p_ptr->prace == RACE_GHOST && !p_ptr->prace_info) ||
		p_ptr->prace == RACE_MUNCHKIN || p_ptr->munchkin)
	{
		p_ptr->immaterial = TRUE;
		p_ptr->allseeing = TRUE;
		p_ptr->lite = TRUE;
		p_ptr->telepathy = TRUE;
		p_ptr->pspeed += 10;
	}


	/* Analyze mutations */

	for (i = 0; i < 32; i++)
	{
		if (p_ptr->mutations1 & (1L << i))
		{
			process_mutation(i);
		}

		if (p_ptr->mutations2 & (1L << i))
		{
			process_mutation(i + 32);
		}

		if (p_ptr->mutations3 & (1L << i))
		{
			process_mutation(i + 64);
		}
	}

	/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < 6; i++)
	{
		int add, top, use, ind;

		/* Extract modifier */
		add = p_ptr->stat_add[i];

		/* Maximize mode */
		if (p_ptr->maximize)
		{
			/* Modify the stats for race/class */
			add += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
		}

		/* Extract the new "stat_top" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], add);

		/* Save the new value */
		p_ptr->stat_top[i] = top;

		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], add);

		/* Save the new value */
		p_ptr->stat_use[i] = use;

		/* Values: 3, 4, ..., 17 */
		if (use <= 18)
			ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18 + 219)
			ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else
			ind = (37);

		/* Save the new index */
		p_ptr->stat_ind[i] = ind;
	}


	/*** Temporary flags ***/

	/* Apply temporary "stun" */
	if (p_ptr->stun > 50)
	{
		p_ptr->to_h -= 20;
		p_ptr->dis_to_h -= 20;
		p_ptr->to_d -= 20;
		p_ptr->dis_to_d -= 20;
	}
	else if (p_ptr->stun)
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temprory shield */
	if (p_ptr->shield)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
	}

	/* Temporary "Beserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
	}

	/* Temporary "fast" */
	if (p_ptr->fast)
	{
		p_ptr->pspeed += 10;
	}

	/* Temporary "slow" */
	if (p_ptr->slow)
	{
		p_ptr->pspeed -= 10;
	}

	/* Temporary see invisible */
	if (p_ptr->tim_invis)
	{
		p_ptr->see_inv = TRUE;
	}

	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra++;
	}


	/*** Special flags ***/

	/* Hack -- Res chaos -> Res confu */
	if (p_ptr->resist_chaos)
	{
		p_ptr->resist_confu = TRUE;
	}

	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->hero || p_ptr->shero)
	{
		p_ptr->resist_fear = TRUE;
	}

	/*** Analyze weight ***/

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i / 2)
		p_ptr->pspeed -= ((j - (i / 2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX)
		p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching)
		p_ptr->pspeed -= 10;


	/*** Apply modifier bonuses ***/

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int) (adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_d += ((int) (adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h += ((int) (adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h += ((int) (adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int) (adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_d += ((int) (adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h += ((int) (adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h += ((int) (adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);


	/*** Modify skills ***/

	/* Affect Skill -- stealth (bonus one) */
	p_ptr->skill_stl += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skill_dis += adj_dex_dis[p_ptr->stat_ind[A_DEX]];
	p_ptr->skill_dis += adj_int_dis[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skill_dev += adj_int_dev[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skill_sav += adj_wis_sav[p_ptr->stat_ind[A_WIS]];

	/* Affect Skill -- digging (STR) */
	p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];

	/* Affect Skill -- disarming (Level, by Class) */
	p_ptr->skill_dis += (cp_ptr->x_dis * p_ptr->lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->skill_dev += (cp_ptr->x_dev * p_ptr->lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->skill_sav += (cp_ptr->x_sav * p_ptr->lev / 10);

	/* Affect Skill -- stealth (Level, by Class) */
	p_ptr->skill_stl += (cp_ptr->x_stl * p_ptr->lev / 10);

	/* Affect Skill -- search ability (Level, by Class) */
	p_ptr->skill_srh += (cp_ptr->x_srh * p_ptr->lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->skill_fos += (cp_ptr->x_fos * p_ptr->lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 10);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1)
		p_ptr->skill_dig = 1;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30)
		p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0)
		p_ptr->skill_stl = 0;

	/* Apply Skill -- Extract noise from stealth */
	p_ptr->noise = (1L << (30 - p_ptr->skill_stl));

	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];


	/*** Analyze current bow ***/

	/* Examine the "current bow" */
	o_ptr = equipment[EQUIP_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to carholdry a heavy bow */
	if (o_ptr && hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}

	/* Analyze launcher */
	if (o_ptr && o_ptr->k_idx)
	{
		/* Get to shoot */
		p_ptr->num_fire = 1;

		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
				/* Sling and ammo */
			case SV_SLING:
			{
				p_ptr->ammo_tval = TV_SHOT;
				p_ptr->ammo_mult = 2;
				break;
			}

				/* Short Bow and Arrow */
			case SV_SHORT_BOW:
			{
				p_ptr->ammo_tval = TV_ARROW;
				p_ptr->ammo_mult = 2;
				break;
			}

				/* Long Bow and Arrow */
			case SV_LONG_BOW:
			{
				p_ptr->ammo_tval = TV_ARROW;
				p_ptr->ammo_mult = 3;
				break;
			}

				/* Light Crossbow and Bolt */
			case SV_LIGHT_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;
				p_ptr->ammo_mult = 3;
				break;
			}

				/* Heavy Crossbow and Bolt */
			case SV_HEAVY_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;
				p_ptr->ammo_mult = 4;
				break;
			}
		}

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Extra might */
			p_ptr->ammo_mult += extra_might;

			/* Hack -- Rangers love Bows */
			if ((p_ptr->pclass == CLASS_RANGER) &&
				(p_ptr->ammo_tval == TV_ARROW))
			{
				/* Extra shot at level 20 */
				if (p_ptr->lev >= 20)
					p_ptr->num_fire++;

				/* Extra shot at level 40 */
				if (p_ptr->lev >= 40)
					p_ptr->num_fire++;
			}

			/* Hack -- Fighters */
			if ((p_ptr->pclass == CLASS_WARRIOR) &&
				(p_ptr->ammo_tval == TV_BOLT) && (p_ptr->lev >= 35))
			{
				p_ptr->num_fire++;
			}

		}

		/* Require at least one shot */
		if (p_ptr->num_fire < 1)
			p_ptr->num_fire = 1;
	}


	/*** Analyze weapon ***/

	/* Examine the "current weapon" */
	o_ptr = equipment[EQUIP_WIELD];

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* It is hard to hold a heavy weapon */
	if (o_ptr && hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy weapon */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	}

	/* Normal weapons */
	if (o_ptr && o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div = 0;

		/* Explanation: 
		 * num -- maximum number of blows.
		 * wgt -- minimum weight in tenth pounds.
		 * mul -- strength index multiplier.
		 */

		/* Analyze the class */
		switch (p_ptr->pclass)
		{

			case CLASS_LYCANTH:
			case CLASS_BEASTMASTER:
			case CLASS_ELEMENTAL:
			case CLASS_WARRIOR:
				num = 6;
				wgt = 30;
				mul = 5;
				break;

				/* Mage  Modified by GJW -KMW- */
			case CLASS_MAGE:
				num = 4;
				wgt = 35;
				mul = 3;
				break;

				/* Priest  Modified by GJW -KMW- */
			case CLASS_PRIEST:
				num = 5;
				wgt = 40;
				mul = 2;
				break;

			case CLASS_VAMPIRE:
			case CLASS_ROGUE:
				num = 5;
				wgt = 30;
				mul = 3;
				break;

			case CLASS_NECRO:
			case CLASS_MIMIC:
			case CLASS_RANGER:
				num = 5;
				wgt = 35;
				mul = 4;
				break;

				/* Paladin  Modified by GJW -KMW- */
		case CLASS_AVATAR:
		case CLASS_PALADIN:
		  num = 5;
		  wgt = 35;
		  mul = 4;
		  break;

			case CLASS_BARD:
			case CLASS_ILLUSIONIST:
				num = 4;
				wgt = 35;
				mul = 3;
				break;

			case CLASS_CORRUPTED:
				num = 2;
				wgt = 45;
				mul = 2;
				break;

		}

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

		/* Mages actually get mul = 3/2 not 3. */
		if (p_ptr->pclass == CLASS_MAGE ||
			p_ptr->pclass == CLASS_ILLUSIONIST ||
			p_ptr->pclass == CLASS_BARD ||
			p_ptr->pclass == CLASS_CORRUPTED) div *= 2;

		/* Access the strength vs weight */
		str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / div);

		/* Maximal value */
		if (str_index > 11)
			str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11)
			dex_index = 11;

		/* Use the blows table */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->num_blow > num)
			p_ptr->num_blow = num;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		/* Require at least one blow */
		if (p_ptr->num_blow < 1)
			p_ptr->num_blow = 1;

		/* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);
	}

	/* Assume okay */
	p_ptr->icky_wield = FALSE;

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((p_ptr->pclass == CLASS_PRIEST) && (!p_ptr->bless_blade) && o_ptr
		&& ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		/* Reduce the real bonuses */
		p_ptr->to_h -= 2;
		p_ptr->to_d -= 2;

		/* Reduce the mental bonuses */
		p_ptr->dis_to_h -= 2;
		p_ptr->dis_to_d -= 2;

		/* Icky weapon */
		p_ptr->icky_wield = TRUE;
	}

	/* Mage/Illusionist weapon penalty for non-applicable weapons -KMW- */
	if (((p_ptr->pclass == CLASS_MAGE) ||
			(p_ptr->pclass == CLASS_ILLUSIONIST)) && o_ptr &&
		((o_ptr->sval != SV_DAGGER) && (o_ptr->sval != SV_QUARTERSTAFF) &&
			(o_ptr->tval)))
	{
		/* Reduce the real bonuses */
		p_ptr->to_h -= 5;
		p_ptr->to_d -= 5;

		/* Reduce the mental bonuses */
		p_ptr->dis_to_h -= 5;
		p_ptr->dis_to_d -= 5;

		/* Icky weapon */
		p_ptr->icky_wield = TRUE;
	}

	/* XXX Corrupted shouldn't wield anything. */

	if ((p_ptr->mega_spells) && o_ptr)
	{
		p_ptr->to_h -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->dis_to_d -= 5;

		p_ptr->icky_wield = TRUE;
	}


	/*** Notice changes ***/

	/* Analyze stats */
	for (i = 0; i < 6; i++)
	{
		/* Notice changes */
		if (p_ptr->stat_top[i] != old_stat_top[i])
		{
			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_SPELL | PW_PLAYER);
		}

		/* Notice changes */
		if (p_ptr->stat_use[i] != old_stat_use[i])
		{
			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_SPELL | PW_PLAYER);
		}

		/* Notice changes */
		if (p_ptr->stat_ind[i] != old_stat_ind[i])
		{
			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in INT may affect Mana/Spells */
			else if (i == A_INT)
			{
				if (cp_ptr->spell_stat == A_INT)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_WIS)
			{
				if (cp_ptr->spell_stat == A_WIS)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS | PU_SANITY);
				}
			}
		}
	}

	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Permanent blindness */
	if (p_ptr->perma_blind != old_perma_blind)
	{

		/* Unblind */
		if (old_perma_blind)
		{
			p_ptr->blind = 0;

			/* Blind */
		}
		else
		{
			p_ptr->blind = 1000;
		}

		/* Forget stuff */
		p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Redraw the "blind" */
		p_ptr->redraw |= (PR_BLIND);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* Handle stuff */
		handle_stuff();
	}


	/* Redraw speed (if needed) */
	if (p_ptr->pspeed != old_speed)
	{
		/* Redraw speed */
		p_ptr->redraw |= (PR_SPEED);
	}

	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_SPELL | PW_PLAYER);
	}

	/* Hack -- handle "xtra" mode */
	if (character_xtra || !character_dungeon)
		return;

	/* Take note when "heavy bow" changes */
	if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			mprint(MSG_WARNING,
				"You have trouble wielding such a heavy bow.");

		}
		else if (equipment[EQUIP_BOW])
		{
			msg_print("You have no trouble wielding your bow.");

		}
		else
		{
			msg_print("You feel relieved to put down your heavy bow.");
		}

		/* Save it */
		p_ptr->old_heavy_shoot = p_ptr->heavy_shoot;
	}

	/* Take note when "heavy weapon" changes */
	if (p_ptr->old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
			mprint(MSG_WARNING,
				"You have trouble wielding such a heavy weapon.");
		}
		else if (equipment[EQUIP_WIELD])
		{
			msg_print("You have no trouble wielding your weapon.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy weapon.");
		}

		/* Save it */
		p_ptr->old_heavy_wield = p_ptr->heavy_wield;
	}

	/* Take note when "illegal weapon" changes */
	if (p_ptr->old_icky_wield != p_ptr->icky_wield)
	{
		/* Message */
		if (p_ptr->icky_wield)
		{
			mprint(MSG_WARNING,
				"You do not feel comfortable with your weapon.");
		}
		else if (equipment[EQUIP_WIELD])
		{
			msg_print("You feel comfortable with your weapon.");
		}
		else
		{
			msg_print
				("You feel more comfortable after removing your weapon.");
		}

		/* Save it */
		p_ptr->old_icky_wield = p_ptr->icky_wield;
	}

	if (p_ptr->old_munchkin != p_ptr->munchkin)
	{

		if (p_ptr->munchkin)
		{
			mprint(MSG_BIG_BONUS,
				"You feel otherworldly forces boiling inside you.");
		}
		else
		{
			mprint(MSG_WARNING, "You lose your ghostly powers.");
		}

		p_ptr->old_munchkin = p_ptr->munchkin;
	}
}


/*
 * Helper routine for clean_explosion().
 */
static void clean_explosion_aux(object_type *stack)
{
	object_type *o_ptr = stack;
	object_type *o_nxt;

	while (o_ptr)
	{
		/* Preload the next object. */
		o_nxt = o_ptr->next_global;

		/* Is it dead? */
		if (!o_ptr->k_idx)
		{
			int iy = o_ptr->iy;
			int ix = o_ptr->ix;
			byte stack = o_ptr->stack;

			remove_object(o_ptr);

			if (stack == STACK_FLOOR)
			{
				lite_spot(iy, ix);
			}
		}

		o_ptr = o_nxt;
	}
}


/* 
 * Delete all objects with k_idx == 0. This is used only for ``dead''
 * objects, after they've been exploded. See ``explode_object'' for detailed
 * explanation.
 *
 * Note: Rotted objects made of STUFF_FLESH are considered "exploded"
 * if they rot away. This can happen in the Home as well.
 */
static void clean_explosion(void)
{
	/* Clean exploded dungeon objects */
	clean_explosion_aux(o_list);

	/* In a vault store */
	if (p_ptr->inside_special == SPECIAL_STORE)
	{
		/* Not in the Home (see below) */
		if (p_ptr->s_idx != 7)
		{
			/* Clean exploded vault-store objects */
			clean_explosion_aux(store[p_ptr->s_idx].stock);
		}
	}

	/* Clean exploded Home objects */
	clean_explosion_aux(store[7].stock);
}


/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice)
		return;

	/* Clean exploded objects. */
	if (p_ptr->notice & PN_CLEAN_EXPLOSION)
	{
		p_ptr->notice &= ~(PN_CLEAN_EXPLOSION);
		clean_explosion();
	}

	/* Combine the pack */
	if (p_ptr->notice & (PN_COMBINE))
	{
		p_ptr->notice &= ~(PN_COMBINE);
		combine_pack();
	}

	/* Reorder the pack */
	if (p_ptr->notice & (PN_REORDER))
	{
		p_ptr->notice &= ~(PN_REORDER);
		reorder_pack();
	}
}


/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
	/* Update stuff */
	if (!p_ptr->update)
		return;


	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		calc_bonuses();
	}

	if (p_ptr->update & (PU_TORCH))
	{
		p_ptr->update &= ~(PU_TORCH);
		calc_torch();
	}

	if (p_ptr->update & (PU_HP))
	{
		p_ptr->update &= ~(PU_HP);
		calc_hitpoints();
	}

	if (p_ptr->update & (PU_MANA))
	{
		p_ptr->update &= ~(PU_MANA);
		calc_mana();
	}

	if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		calc_spells();
	}

	if (p_ptr->update & (PU_SANITY))
	{
		p_ptr->update &= ~(PU_SANITY);
		calc_sanity();
	}

	/* Character is not ready yet, no screen updates */
	if (!character_generated)
		return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky)
		return;


	if (p_ptr->update & (PU_UN_LITE))
	{
		p_ptr->update &= ~(PU_UN_LITE);
		forget_lite();
	}

	if (p_ptr->update & (PU_UN_VIEW))
	{
		p_ptr->update &= ~(PU_UN_VIEW);
		forget_view();
	}


	if (p_ptr->update & (PU_VIEW))
	{
		p_ptr->update &= ~(PU_VIEW);
		update_view();
	}

	if (p_ptr->update & (PU_LITE))
	{
		p_ptr->update &= ~(PU_LITE);
		update_lite();
	}


	if (p_ptr->update & (PU_FLOW))
	{
		p_ptr->update &= ~(PU_FLOW);
		update_flow();
	}


	if (p_ptr->update & (PU_DISTANCE))
	{
		p_ptr->update &= ~(PU_DISTANCE);
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(TRUE);
	}

	if (p_ptr->update & (PU_MONSTERS))
	{
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(FALSE);
	}
}


/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
	/* Redraw stuff */
	if (!p_ptr->redraw)
		return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated)
		return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky)
		return;



	/* Hack -- clear the screen */
	if (p_ptr->redraw & (PR_WIPE))
	{
		p_ptr->redraw &= ~(PR_WIPE);
		msg_print(NULL);
		Term_clear();
	}


	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
	}


	if (p_ptr->redraw & (PR_BASIC))
	{
		p_ptr->redraw &= ~(PR_BASIC);
		p_ptr->redraw &= ~(PR_MISC | PR_TITLE | PR_STATS);
		p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA | PR_SANITY);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		prt_frame_basic();
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		prt_misc();
	}

	if (p_ptr->redraw & (PR_TITLE))
	{
		p_ptr->redraw &= ~(PR_TITLE);
		prt_title();
	}

	if (p_ptr->redraw & (PR_LEV))
	{
		p_ptr->redraw &= ~(PR_LEV);
		prt_level();
	}

	if (p_ptr->redraw & (PR_EXP))
	{
		p_ptr->redraw &= ~(PR_EXP);
		prt_exp();
	}

	if (p_ptr->redraw & (PR_STATS))
	{
		p_ptr->redraw &= ~(PR_STATS);
		prt_stat(A_STR);
		prt_stat(A_INT);
		prt_stat(A_WIS);
		prt_stat(A_DEX);
		prt_stat(A_CON);
		prt_stat(A_CHR);
	}

	if (p_ptr->redraw & (PR_ARMOR))
	{
		p_ptr->redraw &= ~(PR_ARMOR);
		prt_ac();
	}

	if (p_ptr->redraw & (PR_HP))
	{
		p_ptr->redraw &= ~(PR_HP);
		prt_hp();
	}

	if (p_ptr->redraw & (PR_MANA))
	{
		p_ptr->redraw &= ~(PR_MANA);
		prt_sp();
	}

	if (p_ptr->redraw & (PR_SANITY))
	{
		p_ptr->redraw &= ~(PR_SANITY);
		prt_sane();
	}

	if (p_ptr->redraw & (PR_GOLD))
	{
		p_ptr->redraw &= ~(PR_GOLD);
		prt_gold();
	}

	if (p_ptr->redraw & (PR_DEPTH))
	{
		p_ptr->redraw &= ~(PR_DEPTH);
		prt_depth();
	}

	if (p_ptr->redraw & (PR_HEALTH))
	{
		p_ptr->redraw &= ~(PR_HEALTH);
		health_redraw();
	}


	if (p_ptr->redraw & (PR_EXTRA))
	{
		p_ptr->redraw &= ~(PR_EXTRA);
		p_ptr->redraw &= ~(PR_CUT | PR_STUN);
		p_ptr->redraw &= ~(PR_HUNGER);
		p_ptr->redraw &= ~(PR_BLIND | PR_CONFUSED);
		p_ptr->redraw &= ~(PR_AFRAID | PR_POISONED);
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STUDY | PR_SHOUT);
		prt_frame_extra();
	}

	if (p_ptr->redraw & (PR_CUT))
	{
		p_ptr->redraw &= ~(PR_CUT);
		prt_cut();
	}

	if (p_ptr->redraw & (PR_STUN))
	{
		p_ptr->redraw &= ~(PR_STUN);
		prt_stun();
	}

	if (p_ptr->redraw & (PR_HUNGER))
	{
		p_ptr->redraw &= ~(PR_HUNGER);
		prt_hunger();
	}

	if (p_ptr->redraw & (PR_BLIND))
	{
		p_ptr->redraw &= ~(PR_BLIND);
		prt_blind();
	}

	if (p_ptr->redraw & (PR_CONFUSED))
	{
		p_ptr->redraw &= ~(PR_CONFUSED);
		prt_confused();
	}

	if (p_ptr->redraw & (PR_AFRAID))
	{
		p_ptr->redraw &= ~(PR_AFRAID);
		prt_afraid();
	}

	if (p_ptr->redraw & (PR_POISONED))
	{
		p_ptr->redraw &= ~(PR_POISONED);
		prt_poisoned();
	}

	if (p_ptr->redraw & (PR_STATE))
	{
		p_ptr->redraw &= ~(PR_STATE);
		prt_state();
	}

	if (p_ptr->redraw & (PR_SPEED))
	{
		p_ptr->redraw &= ~(PR_SPEED);
		prt_speed();
	}

	if (p_ptr->redraw & (PR_STUDY))
	{
		p_ptr->redraw &= ~(PR_STUDY);
		prt_study();
	}

	if (p_ptr->redraw & (PR_SHOUT))
	{
		p_ptr->redraw &= ~(PR_SHOUT);
		prt_shout();
	}
}

/*
 * Handle "p_ptr->window"
 */
void window_stuff(void)
{
	int j;

	u32b mask = 0L;


	/* Nothing to do */
	if (!p_ptr->window)
		return;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		/* Save usable flags */
		if (angband_term[j])
		{
			/* Build the mask */
			mask |= op_ptr->window_flag[j];
		}
	}

	/* Apply usable flags */
	p_ptr->window &= (mask);

	/* Nothing to do */
	if (!p_ptr->window)
		return;


	/* Display inventory */
	if (p_ptr->window & (PW_INVEN))
	{
		p_ptr->window &= ~(PW_INVEN);
		fix_inven();
	}

	/* Display equipment */
	if (p_ptr->window & (PW_EQUIP))
	{
		p_ptr->window &= ~(PW_EQUIP);
		fix_equip();
	}

	/* Display pflags */
	if (p_ptr->window & (PW_SPELL))
	{
		p_ptr->window &= ~(PW_SPELL);
		fix_pflags();
	}

	/* Display player */
	if (p_ptr->window & (PW_PLAYER))
	{
		p_ptr->window &= ~(PW_PLAYER);
		fix_player();
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_MESSAGE))
	{
		p_ptr->window &= ~(PW_MESSAGE);
		fix_message();
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_OVERHEAD))
	{
		p_ptr->window &= ~(PW_OVERHEAD);
		fix_overhead();
	}

	/* Display monster recall */
	if (p_ptr->window & (PW_MONSTER))
	{
		p_ptr->window &= ~(PW_MONSTER);
		fix_monster();
	}

	/* Display object recall */
	if (p_ptr->window & (PW_OBJECT))
	{
		p_ptr->window &= ~(PW_OBJECT);
		fix_object();
	}
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{
	/* Update stuff */
	if (p_ptr->update)
		update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw)
		redraw_stuff();

	/* Window stuff */
	if (p_ptr->window)
		window_stuff();
}
