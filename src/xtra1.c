

/* File: xtra1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"




/*
 * Converts stat num into a six-char (right justified) string
 */
void
cnv_stat(int val, char *out_val)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 100)
		{
			sprintf(out_val, "18/%03d", bonus);
		}
		else
		{
			sprintf(out_val, " 18/%02d", bonus);
		}
	}

	/* From 3 to 18 */
	else
	{
		sprintf(out_val, "    %2d", val);
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
s16b
modify_stat_value(int value, int amount)
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
static void
prt_field(cptr info, int row, int col)
{
	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", row, col);

	/* Dump the info itself */
	c_put_str(TERM_L_BLUE, info, row, col);
}




/*
 * Print character stat in given row, column
 */
static void
prt_stat(int stat)
{
	char tmp[32];
	bool noval = FALSE;

	if (stat == A_LUC && !p_ptr->luck_known)
		noval = TRUE;

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat] && !noval)
	{
		put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_YELLOW, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], ROW_STAT + stat, 0);
		if (noval)
			c_put_str(TERM_L_GREEN, "???", ROW_STAT + stat, COL_STAT + 9);
		else
		{
			cnv_stat(p_ptr->stat_use[stat], tmp);
			c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
		}
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18 + 100)
	{
		put_str("!", ROW_STAT + stat, 3);
	}
}




/*
 * Prints "wizard" or "winner" titles if needed.
 */
static void
prt_title(void)
{
	cptr p = "";

	/* Wizard */
	if (p_ptr->wizard)
	{
		p = "[=-WIZARD-=]";
	}

	/* Winner */
	else if (p_ptr->total_winner)
	{
		p = "***WINNER***";
	}

	prt_field(p, ROW_TITLE, COL_TITLE);
}





/*
 * Display the experience
 */
static void
prt_exp(void)
{
	char out_val[32];
	bool skdrain = FALSE;
	int i;

	for(i = 0; i < NUM_SK_USED && !skdrain; i++)
	  if (p_ptr->pskills[i].cur < p_ptr->pskills[i].max)
		skdrain = TRUE;

	sprintf(out_val, "%8ld", (long)p_ptr->exp);

	if (p_ptr->exp >= p_ptr->max_exp || skdrain)
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
static void
prt_gold(void)
{
	char tmp[32];

	put_str("AU ", ROW_GOLD, COL_GOLD);
	sprintf(tmp, "%9ld", (long)p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD + 3);
}



/*
 * Prints current AC
 */
static void
prt_ac(void)
{
	char tmp[32];

	put_str("Cur AC ", ROW_AC, COL_AC);
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);
}


/*
 * Prints Cur/Max hit points
 */
static void
prt_hp(void)
{
	char tmp[32];

	byte color;


	put_str("Max HP ", ROW_MAXHP, COL_MAXHP);

	sprintf(tmp, "%5d", p_ptr->mhp);
	color = TERM_L_GREEN;

	c_put_str(color, tmp, ROW_MAXHP, COL_MAXHP + 7);


	put_str("Cur HP ", ROW_CURHP, COL_CURHP);

	sprintf(tmp, "%5d", p_ptr->chp);

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
static void
prt_sp(void)
{
	char tmp[32];
	byte color;


	/* Do not show mana unless it matters */
	if (!mp_ptr->spell_book)
		return;


	put_str("Max SP ", ROW_MAXSP, COL_MAXSP);

	sprintf(tmp, "%5d", p_ptr->msp);
	color = TERM_L_GREEN;

	c_put_str(color, tmp, ROW_MAXSP, COL_MAXSP + 7);


	put_str("Cur SP ", ROW_CURSP, COL_CURSP);

	sprintf(tmp, "%5d", p_ptr->csp);

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
 * Prints depth in stat area
 */
static void
prt_depth(void)
{
	char depths[32];

	if (!p_ptr->depth)
	{
		strcpy(depths, "Town");
	}
	else if (p_ptr->depth == -1)
	{
		strcpy(depths, "Quest");
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
	prt(format("%7s", depths), 23, COL_DEPTH);
}


/*
 * Prints status of hunger
 */
static void
prt_hunger(void)
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
static void
prt_blind(void)
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
static void
prt_confused(void)
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
static void
prt_afraid(void)
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
static void
prt_poisoned(void)
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
static void
prt_state(void)
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
			text[7] = I2D(i % 10);
			if (i >= 10)
			{
				i = i / 10;
				text[6] = I2D(i % 10);
				if (i >= 10)
				{
					text[5] = I2D(i / 10);
				}
			}
		}

		/* Long (timed) rest */
		else if (n >= 100)
		{
			i = n;
			text[9] = I2D(i % 10);
			i = i / 10;
			text[8] = I2D(i % 10);
			text[7] = I2D(i / 10);
		}

		/* Medium (timed) rest */
		else if (n >= 10)
		{
			i = n;
			text[9] = I2D(i % 10);
			text[8] = I2D(i / 10);
		}

		/* Short (timed) rest */
		else if (n > 0)
		{
			i = n;
			text[9] = I2D(i);
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
 * Prints the speed of a character.           -CJS-
 */
static void
prt_speed(void)
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
	c_put_str(attr, format("%-14s", buf), ROW_SPEED, COL_SPEED);
}


static void
prt_study(void)
{
	if (p_ptr->new_spells)
	{
		put_str("Study", ROW_STUDY, COL_STUDY);
	}
	else
	{
		put_str("     ", ROW_STUDY, COL_STUDY);
	}
}


static void
prt_invis()
{
	if (p_ptr->tim_invis)
	{
		put_str("Invisible", ROW_INVIS, COL_INVIS);
	}
	else
	{
		put_str("         ", ROW_INVIS, COL_INVIS);
	}
}


static void
prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)
	{
		c_put_str(TERM_L_RED, "Mortal wound", ROW_CUT, COL_CUT);
	}
	else if (c > 200)
	{
		c_put_str(TERM_RED, "Deep gash   ", ROW_CUT, COL_CUT);
	}
	else if (c > 100)
	{
		c_put_str(TERM_RED, "Severe cut  ", ROW_CUT, COL_CUT);
	}
	else if (c > 50)
	{
		c_put_str(TERM_ORANGE, "Nasty cut   ", ROW_CUT, COL_CUT);
	}
	else if (c > 25)
	{
		c_put_str(TERM_ORANGE, "Bad cut     ", ROW_CUT, COL_CUT);
	}
	else if (c > 10)
	{
		c_put_str(TERM_YELLOW, "Light cut   ", ROW_CUT, COL_CUT);
	}
	else if (c)
	{
		c_put_str(TERM_YELLOW, "Graze       ", ROW_CUT, COL_CUT);
	}
	else
	{
		put_str("            ", ROW_CUT, COL_CUT);
	}
}



static void
prt_stun(void)
{
	int s = p_ptr->stun;

	if (s > 100)
	{
		c_put_str(TERM_RED, "Knocked out ", ROW_STUN, COL_STUN);
	}
	else if (s > 50)
	{
		c_put_str(TERM_ORANGE, "Heavy stun  ", ROW_STUN, COL_STUN);
	}
	else if (s)
	{
		c_put_str(TERM_ORANGE, "Stun        ", ROW_STUN, COL_STUN);
	}
	else
	{
		put_str("            ", ROW_STUN, COL_STUN);
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
static void
health_redraw(void)
{
	/* Not tracking */
	if (!p_ptr->health_who)
	{
		/* Erase the health bar */
		Term_erase(COL_INFO, ROW_INFO, 12);
	}

	/* Tracking an unseen monster */
	else if (!m_list[p_ptr->health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[p_ptr->health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;

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
		/* Afraid */
		if (m_ptr->monfear)
			attr = TERM_VIOLET;
		/* Asleep */
		if (m_ptr->csleep)
			attr = TERM_BLUE;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
	}
}



/*
 * Display basic info (mostly left of map)
 */
static void
prt_frame_basic(void)
{
	int i;

	/* Race and Class */
	prt_field(rp_ptr->title, ROW_RACE, COL_RACE);

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_exp();

	/* All Stats */
	for (i = 0; i < 7; i++)
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
	/* Special */
	health_redraw();
}


/*
 * Display extra info (mostly below map)
 */
static void
prt_frame_extra(void)
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
}



/*
 * Hack -- display inventory in sub-windows
 */
static void
fix_inven(void)
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
		display_inven();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}
/*
 * Hack -- display equipment in sub-windows
 */
static void
fix_equip(void)
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
		display_equip();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Hack -- display player in sub-windows (mode 0)
 */
static void
fix_player_0(void)
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
		if (!(op_ptr->window_flag[j] & (PW_PLAYER_0)))
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
 * Hack -- display player in sub-windows (mode 1)
 */
static void
fix_player_1(void)
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
		if (!(op_ptr->window_flag[j] & (PW_PLAYER_1)))
			continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display flags */
		display_player(1);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display recent messages in sub-windows
 *
 * Adjust for width and split messages.  XXX XXX XXX
 */
static void
fix_message(void)
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
			Term_putstr(0, (h - 1) - i, -1, TERM_WHITE, message_str(i));

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
static void
fix_overhead(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int j;

	int cy, cx;

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
		display_map(&cy, &cx);

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
static void
fix_monster(void)
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
static void
fix_object(void)
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
		if (p_ptr->object_kind_idx)
			display_koff(p_ptr->object_kind_idx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Get the "name" of spells (ie, "spell", "prayer", or "technique")
 */
cptr
spell_type()
{
	switch (p_ptr->realm)
	{
	case PRIEST:
		return ("prayer");
		break;
	case DRUID:
		return ("technique");
		break;
	default:
		return ("spell");
		break;
	}
}


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void
calc_spells(void)
{
	int i, j, k, levels;
	int num_allowed, num_known;

	magic_type *s_ptr;

	cptr p;


	/* Hack -- must be literate */
	if (!p_ptr->realm)
		return;

	/* Hack -- wait for creation */
	if (!character_generated)
		return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra)
		return;

	p = spell_type();

	/* Determine the number of spells allowed */
	levels = get_skill(S_MAGIC);

	/* Hack -- no negative spells */
	if (levels < 0)
		levels = 0;

	/* Extract total allowed spells */
	num_allowed = (adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] *
				   levels / 2);

	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < 64; j++)
	{
		/* Count known spells */
		if ((j < 32) ?
			(p_ptr->spell_learned1 & (1L << j)) :
			(p_ptr->spell_learned2 & (1L << (j - 32))))
		{
			num_known++;
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;



	/* Forget spells which are too hard */
	for (i = 63; i >= 0; i--)
	{
		/* Efficiency -- all done */
		if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2)
			break;

		/* Access the spell */
		j = p_ptr->spell_order[i];

		/* Skip non-spells */
		if (j >= 99)
			continue;

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= get_skill(S_MAGIC))
			continue;

		/* Is it known? */
		if ((j < 32) ?
			(p_ptr->spell_learned1 & (1L << j)) :
			(p_ptr->spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten */
			if (j < 32)
			{
				p_ptr->spell_forgotten1 |= (1L << j);
			}
			else
			{
				p_ptr->spell_forgotten2 |= (1L << (j - 32));
			}

			/* No longer known */
			if (j < 32)
			{
				p_ptr->spell_learned1 &= ~(1L << j);
			}
			else
			{
				p_ptr->spell_learned2 &= ~(1L << (j - 32));
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
					   spell_names[p_ptr->realm - 1][j]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = 63; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0)
			break;

		/* Efficiency -- all done */
		if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2)
			break;

		/* Get the (i+1)th spell learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99)
			continue;

		/* Forget it (if learned) */
		if ((j < 32) ?
			(p_ptr->spell_learned1 & (1L << j)) :
			(p_ptr->spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten */
			if (j < 32)
			{
				p_ptr->spell_forgotten1 |= (1L << j);
			}
			else
			{
				p_ptr->spell_forgotten2 |= (1L << (j - 32));
			}

			/* No longer known */
			if (j < 32)
			{
				p_ptr->spell_learned1 &= ~(1L << j);
			}
			else
			{
				p_ptr->spell_learned2 &= ~(1L << (j - 32));
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
					   spell_names[p_ptr->realm - 1][j]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < 64; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0)
			break;

		/* Efficiency -- all done */
		if (!p_ptr->spell_forgotten1 && !p_ptr->spell_forgotten2)
			break;

		/* Get the next spell we learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99)
			break;

		/* Access the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > get_skill(S_MAGIC))
			continue;

		/* First set of spells */
		if ((j < 32) ?
			(p_ptr->spell_forgotten1 & (1L << j)) :
			(p_ptr->spell_forgotten2 & (1L << (j - 32))))
		{
			/* No longer forgotten */
			if (j < 32)
			{
				p_ptr->spell_forgotten1 &= ~(1L << j);
			}
			else
			{
				p_ptr->spell_forgotten2 &= ~(1L << (j - 32));
			}
			/* Known once more */
			if (j < 32)
			{
				p_ptr->spell_learned1 |= (1L << j);
			}
			else
			{
				p_ptr->spell_learned2 |= (1L << (j - 32));
			}

			/* Message */
			msg_format("You have remembered the %s of %s.",
					   p, spell_names[p_ptr->realm - 1][j]);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < 64; j++)
	{
		/* Access the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > get_skill(S_MAGIC))
			continue;

		/* Skip spells we already know */
		if ((j < 32) ?
			(p_ptr->spell_learned1 & (1L << j)) :
			(p_ptr->spell_learned2 & (1L << (j - 32))))
		{
			continue;
		}

		/* Count it */
		k++;
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k)
		p_ptr->new_spells = k;

	/* Spell count changed */
	if (p_ptr->old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
			msg_format("You can learn %d more %s%s.",
					   p_ptr->new_spells, p,
					   (p_ptr->new_spells != 1) ? "s" : "");
		}

		/* Save the new_spells value */
		p_ptr->old_spells = p_ptr->new_spells;

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void
calc_mana(void)
{
	int msp, levels, cur_wgt, max_wgt;

	object_type *o_ptr;


	/* Hack -- Must be literate */
	if (!p_ptr->realm)
		return;


	/* Extract "effective" player level */
	levels = get_skill(S_MPOWER);

	/* Hack -- no negative mana */
	if (levels < 0)
		levels = 0;

	/* Extract total mana */
	msp = adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 4;

	/* Hack -- usually add one mana */
	if (msp)
		msp++;


	/* Only mages are affected */
	if (p_ptr->realm == MAGE || p_ptr->realm == NECRO)
	{
		u32b f1, f2, f3;

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx &&
			!(f3 & (TR3_FREE_ACT)) &&
			!((f1 & (TR1_DEX)) && (o_ptr->pval > 0)))
		{
			/* Encumbered */
			p_ptr->cumber_glove = TRUE;
			/* Reduce mana */
			msp = (3 * msp) / 4;
		}
	}


	/* Assume player not encumbered by armor */
	p_ptr->cumber_armor = FALSE;

	/* Weigh the armor */
	cur_wgt = armor_weight();

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Reduce mana */
		msp -= ((cur_wgt - max_wgt) / 10);
	}


	/* Mana can never be negative */
	if (msp < 0)
		msp = 0;


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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
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
			msg_print("Your covered hands feel unsuitable for spellcasting.");
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
			msg_print("The weight of your armor encumbers your movement.");
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
static void
calc_hitpoints(void)
{
	int bonus, mhp, lev, halflev;

	/* Get player's "level" */
	lev = get_skill(S_ENDURANCE) / 2;
	halflev = get_skill(S_ENDURANCE) % 2;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128);

	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[lev - 1] + (bonus * lev / 2);

	/* add in "half-level" */
	if (halflev)
	{
		mhp += bonus / 4;
		mhp += (p_ptr->player_hp[lev] - p_ptr->player_hp[lev - 1]) / 2;
	}

	/* Always have at least one hitpoint per "level" */
	if (mhp < lev + 1)
		mhp = lev + 1;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero)
		mhp += 10;
	if (p_ptr->shero)
		mhp += 30;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		/* Add in extra hit points from heroism and such */
		if (p_ptr->extrahp)
		{
			p_ptr->chp += p_ptr->extrahp;
			p_ptr->extrahp = 0;
		}

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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}

/*
 * Extract and set the current "lite radius"
 */
static void
calc_torch(void)
{
	object_type *o_ptr = &inventory[INVEN_LITE];

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Player is glowing */
	if (p_ptr->lite)
		p_ptr->cur_lite = 1;

	/* Examine actual lites */
	if (o_ptr->tval == TV_LITE)
	{
		/* Torches (with fuel) provide some lite */
		if ((o_ptr->sval == SV_LITE_TORCH) && (o_ptr->pval > 0))
		{
			p_ptr->cur_lite = 1;
		}

		/* Lanterns (with fuel) provide more lite */
		if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->pval > 0))
		{
			p_ptr->cur_lite = 2;
		}

		/* Artifact Lites provide permanent, bright, lite */
		if (artifact_p(o_ptr))
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
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Remember the old lite */
		p_ptr->old_lite = p_ptr->cur_lite;
	}
}

/*
 * Computes current weight limit.
 */
static int
weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
	i = adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100;

	/* Return the result */
	return (i);
}

/* 
 * Compute a percentage modifier based on the Luck stat
 */
int
luck()
{
	int t, mod;

	t = p_ptr->stat_cur[A_LUC];
	if (t < 4)
		mod = -30;
	else if (t < 5)
		mod = -20;
	else if (t < 6)
		mod = -10;
	else if (t < 7)
		mod = -5;
	else if (t < 14)
		mod = 0;
	else if (t < 16)
		mod = 2;
	else if (t < 18)
		mod = 5;
	else if (t < 43)
		mod = t / 3;
	else
		mod = t / 5 + 7;		/* REALLY lucky dude! */
	return mod;
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
static void
calc_bonuses(void)
{
	int i, j, hold;

	int old_speed;

	int old_telepathy;
	int old_see_inv;

	int old_dis_ac;
	int old_dis_to_a;

	int old_spare_blows;
	int extra_blows, extra_blows_2;
	int extra_shots;
	int extra_might;

	int old_stat_top[7];
	int old_stat_use[7];
	int old_stat_ind[7];

	object_type *o_ptr, *o_ptr2;

	u32b f1, f2, f3;
	byte wskill, wsv;


/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;
	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	/* Save the old secondary weapon blows */
	old_spare_blows = p_ptr->num_blow2;

	/* Save the old stats */
	for (i = 0; i < 7; i++)
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
	p_ptr->num_blow2 = 0;
	extra_blows = extra_blows_2 = 0;

	/* Reset "fire" info */
	p_ptr->num_fire = 0;
	p_ptr->ammo_mult = 0;
	p_ptr->ammo_tval = 0;
	extra_shots = 0;
	extra_might = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < 7; i++)
		p_ptr->stat_add[i] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = get_skill(S_DODGING);

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
	p_ptr->invisible = 0;
	p_ptr->soulsteal = FALSE;
	p_ptr->nomagic = FALSE;
	p_ptr->twoweap = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_chr = FALSE;
	p_ptr->sustain_luc = FALSE;
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

/*** Extract race/skill info ***/

	/* Get weapon skill info */
	wskill = sweapon();
	wsv = get_raw_skill(wskill);

	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra;

	/* Base skill -- disarming */
	p_ptr->skill_dis = 20 + get_skill(S_DISARM);

	/* Base skill -- magic devices */
	p_ptr->skill_dev = 18 + get_skill(S_DEVICE);

	/* Base skill -- saving throw */
	p_ptr->skill_sav = 17 + get_skill(S_SAVE);

	/* Base skill -- stealth */
	p_ptr->skill_stl = 1 + get_skill(S_STEALTH);

	/* Base skill -- searching ability */
	p_ptr->skill_srh = 10 + get_raw_skill(S_PERCEPTION) / 2;

	/* Base skill -- searching frequency */
	p_ptr->skill_fos = 4 + get_skill(S_PERCEPTION);

	/* Base skill -- combat (normal) */
	p_ptr->skill_thn = rp_ptr->r_thn + 40 + (wsv * 5) / 8;

	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = rp_ptr->r_thb + 35 + get_raw_skill(S_ARCHERY) / 2;

	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = rp_ptr->r_thb + 35 + get_raw_skill(S_ARCHERY) / 2;

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
	{
		p_ptr->resist_lite = TRUE;
		p_ptr->see_inv = TRUE;
	}

	/* Dark Elf */
	if (p_ptr->prace == RACE_DARK_ELF)
	{
		p_ptr->sustain_int = TRUE;
		p_ptr->resist_chaos = TRUE;
	}

	/* Giant */
	if (p_ptr->prace == RACE_GIANT)
	{
		p_ptr->resist_fire = TRUE;
		p_ptr->resist_cold = TRUE;
	}

/*** Analyze equipment ***/

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx)
			continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stats */
		if (f1 & (TR1_STR))
			p_ptr->stat_add[A_STR] += cap_pval(o_ptr->pval, (MAX_TR1_STAT));
		if (f1 & (TR1_INT))
			p_ptr->stat_add[A_INT] += cap_pval(o_ptr->pval, (MAX_TR1_STAT));
		if (f1 & (TR1_WIS))
			p_ptr->stat_add[A_WIS] += cap_pval(o_ptr->pval, (MAX_TR1_STAT));
		if (f1 & (TR1_DEX))
			p_ptr->stat_add[A_DEX] += cap_pval(o_ptr->pval, (MAX_TR1_STAT));
		if (f1 & (TR1_CON))
			p_ptr->stat_add[A_CON] += cap_pval(o_ptr->pval, (MAX_TR1_STAT));
		if (f1 & (TR1_CHR))
			p_ptr->stat_add[A_CHR] += cap_pval(o_ptr->pval, (MAX_TR1_STAT));
		if (f1 & (TR1_LUC))
			p_ptr->stat_add[A_LUC] += cap_pval(o_ptr->pval, (MAX_TR1_STAT));

		/* Affect stealth */
		if (f1 & (TR1_STEALTH))
			p_ptr->skill_stl += cap_pval(o_ptr->pval, (MAX_TR1_STEALTH));

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_SEARCH))
			p_ptr->skill_srh += (cap_pval(o_ptr->pval, (MAX_TR1_SEARCH)) * 5);

		/* Affect searching frequency (factor of five) */
		if (f1 & (TR1_SEARCH))
			p_ptr->skill_fos += (cap_pval(o_ptr->pval, (MAX_TR1_SEARCH)) * 5);

		/* Affect infravision */
		if (f1 & (TR1_INFRA))
			p_ptr->see_infra += cap_pval(o_ptr->pval, (MAX_TR1_INFRA));

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL))
			p_ptr->skill_dig += (cap_pval(o_ptr->pval, (MAX_TR1_TUNNEL)) * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED))
			p_ptr->pspeed += cap_pval(o_ptr->pval, (MAX_TR1_SPEED));

		/* Affect invisibility */
		if (f1 & (TR1_INVIS))
			p_ptr->invisible += cap_pval(o_ptr->pval, (MAX_TR1_INVIS)) * 10;

		/* Affect blows */
		if (f1 & (TR1_BLOWS))
		{
			if (i == INVEN_ARM)
			{
				extra_blows_2 += cap_pval(o_ptr->pval, (MAX_TR1_BLOWS));
			}
			else
			{
				extra_blows += cap_pval(o_ptr->pval, (MAX_TR1_BLOWS));
			}
		}

		/* Affect shots */
		if (f1 & (TR1_SHOTS))
			extra_shots += cap_pval(o_ptr->pval, (MAX_TR1_SHOTS));

		/* Affect Might */
		if (f1 & (TR1_MIGHT))
			extra_might += cap_pval(o_ptr->pval, (MAX_TR1_MIGHT));

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

		/* Bad flags */
		if (f3 & (TR3_IMPACT))
			p_ptr->impact = TRUE;
		if (f3 & (TR3_AGGRAVATE))
			p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT))
			p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP))
			p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_SOULSTEAL))
			p_ptr->soulsteal = TRUE;
		if (f3 & (TR3_NOMAGIC))
			p_ptr->nomagic = TRUE;

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
		if (f2 & (TR2_SUST_LUC))
			p_ptr->sustain_luc = TRUE;

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
		if (i == INVEN_WIELD)
			continue;
		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW)
			continue;
		/* Hack -- do not apply "second weapon" bonuses */
		if (i == INVEN_ARM &&
			(o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED))
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

	/* Apply shapeshifting mods. */
	switch (p_ptr->schange)
	{
	case SHAPE_NORMAL:
		break;
	case SHAPE_SHEEP:
		p_ptr->see_infra += 1;
		p_ptr->skill_stl += 4;
		p_ptr->stat_add[A_LUC] += 1;
		p_ptr->aggravate = FALSE;
		p_ptr->resist_cold = TRUE;
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		break;
	case SHAPE_GOAT:
		p_ptr->see_infra += 1;
		p_ptr->stat_add[A_DEX] += 1;
		p_ptr->slow_digest = TRUE;
		p_ptr->to_a += 8;
		p_ptr->dis_to_a += 8;
		p_ptr->to_h += 3;
		p_ptr->dis_to_h += 3;
		break;
	case SHAPE_WOLVER:
		p_ptr->see_infra += 2;
		p_ptr->stat_add[A_CON] += 2;
		p_ptr->resist_fear = TRUE;
		p_ptr->to_d += 5;
		p_ptr->dis_to_d += 5;
		extra_blows += 1;
		extra_blows_2 += 1;
		break;
	case SHAPE_BEAR:
		p_ptr->see_infra += 1;
		p_ptr->stat_add[A_STR] += 3;
		p_ptr->stat_add[A_CON] += 3;
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
		p_ptr->to_d += 8;
		p_ptr->dis_to_d += 8;
		break;
	case SHAPE_LION:
		p_ptr->see_infra += 2;
		p_ptr->stat_add[A_STR] += 3;
		p_ptr->resist_fear = TRUE;
		p_ptr->to_a += 15;
		p_ptr->dis_to_a += 15;
		p_ptr->to_d += 10;
		p_ptr->dis_to_d += 10;
		break;
	case SHAPE_GAZELLE:
		p_ptr->see_infra += 1;
		p_ptr->stat_add[A_STR] -= 1;
		p_ptr->stat_add[A_DEX] += 2;
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 1;
		p_ptr->dis_to_h += 1;
		p_ptr->pspeed += 10;
		break;
	case SHAPE_CHEETAH:
		p_ptr->see_infra += 2;
		p_ptr->stat_add[A_DEX] += 2;
		extra_blows += 1;
		extra_blows_2 += 1;
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 2;
		p_ptr->dis_to_h += 2;
		p_ptr->to_d += 2;
		p_ptr->dis_to_d += 2;
		p_ptr->pspeed += 15;
		break;
	case SHAPE_DRAGON:
		p_ptr->see_infra += 3;
		p_ptr->stat_add[A_STR] += 2;
		p_ptr->stat_add[A_CON] += 2;
		p_ptr->aggravate = TRUE;
		p_ptr->see_inv = TRUE;
		p_ptr->resist_fire = TRUE;
		p_ptr->to_a += 25;
		p_ptr->dis_to_a += 25;
		p_ptr->to_h += 5;
		p_ptr->dis_to_h += 5;
		p_ptr->to_d += 10;
		p_ptr->dis_to_d += 10;
		p_ptr->pspeed += 10;
		break;
	}

/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < 7; i++)
	{
		int add, top, use, ind;

		/* Extract modifier */
		add = p_ptr->stat_add[i];

		/* Maximize mode */
		if (p_ptr->maximize)
		{
			/* Modify the stats for race, and apply the player's mods */
			add += (rp_ptr->r_adj[i] + p_ptr->stat_mod[i]);
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


	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temporary shield */
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

	/* Temporary "Berserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
		extra_blows += 1;
		extra_blows_2 += 1;
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
	if (p_ptr->detect_inv)
	{
		p_ptr->see_inv = TRUE;
	}
	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra++;
	}

	/* Temporary stealth boost */
	if (p_ptr->tim_stealth)
	{
		p_ptr->skill_stl += 5;
	}

	/* Temporary invisibility */
	if (p_ptr->tim_invis)
	{
		p_ptr->invisible += p_ptr->tim_inv_pow;
	}


/*** Special flags ***/

	/* Hack -- Res Chaos -> Res Conf */
	if (p_ptr->resist_chaos)
		p_ptr->resist_confu = TRUE;

	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->hero || p_ptr->shero)
		p_ptr->resist_fear = TRUE;

/*** Analyze weight ***/

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* Apply "encumbrance" from weight */
	if (j > i / 2)
		p_ptr->pspeed -= ((j - (i / 2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX)
		p_ptr->pspeed -= 10;
	/* Searching slows the player down */
	if (p_ptr->searching)
		p_ptr->pspeed -= 10;

	/* Maximum speed */
	if (p_ptr->pspeed > 195)
		p_ptr->pspeed = 195;

/*** Apply modifier bonuses ***/

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);


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

	/* Affect skill -- saving throw (LUC) */
	p_ptr->skill_sav += luck() / 2;

	/* Affect Skill -- digging (STR) */
	p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];

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
	o_ptr = &inventory[INVEN_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to hold a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}

	/* Analyze launcher */
	if (o_ptr->k_idx)
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

		/* Reward good archers using arrows */
		if (p_ptr->ammo_tval == TV_ARROW)
			extra_shots += get_raw_skill(S_ARCHERY) / 125;

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Extra might */
			p_ptr->ammo_mult += extra_might;
		}

		/* Require at least one shot */
		if (p_ptr->num_fire < 1)
			p_ptr->num_fire = 1;
	}


/*** Analyze weapons ***/

	/* Examine the "main weapon" */
	o_ptr = &inventory[INVEN_WIELD];

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* It is hard to hold a heavy weapon */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy weapon */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	}

	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div = 0;

		num = 1;
		if (wsv > 40) num++;
		if (wsv > 80) num++;
		if (wsv > 140) num++;
		if (wsv > 180) num++;
		mul = wsv / 80 + 2;
		wgt = 40 - wsv / 20;

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);
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


		/* Require at least one blow */
		if (p_ptr->num_blow < 1)
			p_ptr->num_blow = 1;

		/* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);
	}
	else if (!o_ptr->k_idx)
	{
		p_ptr->skill_dig += (wsv < 100) ? 0 : (wsv - 100) / 10;
		if (wskill == S_KARATE)
		{
			p_ptr->num_blow = adj_dex_kb[p_ptr->stat_ind[A_DEX]];
		}
		else
			p_ptr->num_blow = 1;	/* Only ONE blow/round! */
	}

	/* Assume okay */
	p_ptr->icky_wield = FALSE;

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((p_ptr->realm == PRIEST) && (!p_ptr->bless_blade) &&
		((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
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

	/* Weapon skill */
	p_ptr->wskill = sweapon();

	/* Examine the secondary weapon. */
	o_ptr2 = &inventory[INVEN_ARM];

	/* Is it a weapon? */
	if (o_ptr2->tval == TV_SWORD || o_ptr2->tval == TV_HAFTED)
	{
		p_ptr->twoweap = TRUE;
		/* If all the various and sundry conditions for dual wield are
		 * true, we can use it. */
		if (o_ptr2->tval == o_ptr->tval && !(p_ptr->heavy_wield) &&
			o_ptr2->weight < o_ptr->weight && o_ptr->weight < 160)
		{
			int str_index, dex_index;

			int num = 0, wgt = 0, mul = 0, div = 0;

			num = wsv / 50 + 1;
			mul = wsv / 60 + 1;
			wgt = 45 - wsv / 16;

			/* Enforce a minimum "weight" (tenth pounds) */
			div = ((o_ptr2->weight < wgt) ? wgt : o_ptr2->weight);
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
			p_ptr->num_blow2 = blows_table[str_index][dex_index];
			/* Maximal value */
			if (p_ptr->num_blow2 > num)
				p_ptr->num_blow2 = num;

			if (p_ptr->num_blow2 > get_skill(S_2HANDED))
				p_ptr->num_blow2 = get_skill(S_2HANDED);
		}
		else
			p_ptr->num_blow2 = 0;
	}

	/* If you're using a second weapon, you don't fight as well with
	 * the primary. */
	if (p_ptr->num_blow2 > 0)
	{
		p_ptr->num_blow *= 2;
		p_ptr->num_blow /= 3;
		if (p_ptr->num_blow < 1)
			p_ptr->num_blow = 1;
	}

	/* You never get more attacks with the secondary than the primary */
	if (p_ptr->twoweap && p_ptr->num_blow2 > p_ptr->num_blow)
		p_ptr->num_blow2 = p_ptr->num_blow;

	/* add in extra attacks */
	p_ptr->num_blow += extra_blows;
	if (p_ptr->num_blow2)
		p_ptr->num_blow2 += extra_blows_2;


/*** Notice changes ***/

	/* Analyze stats */
	for (i = 0; i < 7; i++)
	{
		/* Notice changes */
		if (p_ptr->stat_top[i] != old_stat_top[i])
		{
			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
		}

		/* Notice changes */
		if (p_ptr->stat_use[i] != old_stat_use[i])
		{
			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
		}

		/* Notice changes */
		if (p_ptr->stat_ind[i] != old_stat_ind[i])
		{
			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in spell stat affects Mana/Spells */
			else if (i == mp_ptr->spell_stat)
			{
				p_ptr->update |= (PU_MANA | PU_SPELLS);
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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra)
		return;

	/* Let the player know whether their secondary is any use. */
	if (p_ptr->twoweap && old_spare_blows > 0 && p_ptr->num_blow2 == 0)
		msg_print("You are unable to use your secondary weapon effectively.");

	/* Take note when "heavy bow" changes */
	if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msg_print("You have trouble wielding such a heavy bow.");
		}
		else if (inventory[INVEN_BOW].k_idx)
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
			msg_print("You have trouble wielding such a heavy weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
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
			msg_print("You do not feel comfortable with your weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You feel comfortable with your weapon.");
		}
		else
		{
			msg_print("You feel more comfortable after removing your weapon.");
		}
		/* Save it */
		p_ptr->old_icky_wield = p_ptr->icky_wield;
	}
}

/* Weigh the player's armor */
int
armor_weight(void)
{
	int cur_wgt = 0;

	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	if (!(p_ptr->twoweap))
		cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;
	return (cur_wgt);
}

/*
 * Handle "p_ptr->notice"
 */
void
notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice)
		return;


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
void
update_stuff(void)
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

	/* Character is not ready yet, no screen updates */
	if (!character_generated)
		return;

	/* Character is in "icky" mode, no screen updates */
	if (character_icky)
		return;


	if (p_ptr->update & (PU_FORGET_VIEW))
	{
		p_ptr->update &= ~(PU_FORGET_VIEW);
		forget_view();
	}

	if (p_ptr->update & (PU_UPDATE_VIEW))
	{
		p_ptr->update &= ~(PU_UPDATE_VIEW);
		update_view();
	}


	if (p_ptr->update & (PU_FORGET_FLOW))
	{
		p_ptr->update &= ~(PU_FORGET_FLOW);
		forget_flow();
	}

	if (p_ptr->update & (PU_UPDATE_FLOW))
	{
		p_ptr->update &= ~(PU_UPDATE_FLOW);
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


	if (p_ptr->update & (PU_PANEL))
	{
		p_ptr->update &= ~(PU_PANEL);
		verify_panel();
	}
}


/*
 * Handle "p_ptr->redraw"
 */
void
redraw_stuff(void)
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
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		prt_frame_basic();
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
	}

	if (p_ptr->redraw & (PR_TITLE))
	{
		p_ptr->redraw &= ~(PR_TITLE);
		prt_title();
	}

	if (p_ptr->redraw & (PR_EXP))
	{
		p_ptr->redraw &= ~(PR_EXP);
		prt_exp();
	}

	if (p_ptr->redraw & (PR_STATS))
	{
		int i;

		p_ptr->redraw &= ~(PR_STATS);
		for (i = 0; i < 7; i++)
			prt_stat(i);
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
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STUDY | PR_INVIS);
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

	if (p_ptr->redraw & PR_INVIS)
	{
		p_ptr->redraw &= ~(PR_INVIS);
		prt_invis();
	}
}


/*
 * Handle "p_ptr->window"
 */
void
window_stuff(void)
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

	/* Display player (mode 0) */
	if (p_ptr->window & (PW_PLAYER_0))
	{
		p_ptr->window &= ~(PW_PLAYER_0);
		fix_player_0();
	}

	/* Display player (mode 1) */
	if (p_ptr->window & (PW_PLAYER_1))
	{
		p_ptr->window &= ~(PW_PLAYER_1);
		fix_player_1();
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
void
handle_stuff(void)
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
