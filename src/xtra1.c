/* CVS: Last edit by $Author: sfuerst $ on $Date: 2000/07/19 13:51:45 $ */
/* File: misc.c */

/* Purpose: misc code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"




/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 220)
		{
			sprintf(out_val, "18/%3s", "***");
		}
		else if (bonus >= 100)
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
s16b modify_stat_value(int value, int amount)
{
	int    i;

	/* Reward */
	if (amount > 0)
	{
		/* Apply each point */
		for (i = 0; i < amount; i++)
		{
			/* One point at a time */
			if (value < 18) value++;

			/* Ten "points" at a time */
			else value += 10;
		}
	}

	/* Penalty */
	else if (amount < 0)
	{
		/* Apply each point */
		for (i = 0; i < (0 - amount); i++)
		{
			/* Ten points at a time */
			if (value >= 18+10) value -= 10;

			/* Hack -- prevent weirdness */
			else if (value > 18) value = 18;

			/* One point at a time */
			else if (value > 3) value--;
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
	if (p_ptr->stat_max[stat] == 18+100)
	{
		put_str("!", ROW_STAT + stat, 3);
	}
}

static int bar_count = 0;

static void clear_status_bar(void)
{
	Term_putstr(COL_STATBAR, ROW_STATBAR, 12, TERM_WHITE, "            ");
}


static void show_status_bar(cptr letter, byte *colour, int num)
{
	int i;

	if (num <= 12)
	{
		/* Reset everything */
		bar_count = 0;
		clear_status_bar();

		/* Display the flags */
		for (i = 0; i < num; i++)
		{
			Term_putch(COL_STATBAR + i, ROW_STATBAR, colour[i], letter[i]);
		}
	}
	else
	{
		/* increment the count (scroll the flags) */
		bar_count++;

		if (bar_count >= num) bar_count = 0;

		if (bar_count + 12 < num)
		{
			/* Simple case - all in a row */
			for (i = 0; i < 12; i++)
			{
				Term_putch(COL_STATBAR + i, ROW_STATBAR,
				           colour[i + bar_count], letter[i + bar_count]);
			}
		}
		else
		{
			/* Split over boundary */
			for (i = 0; i < num - bar_count; i++)
			{
				Term_putch(COL_STATBAR + i, ROW_STATBAR,
				           colour[i + bar_count], letter[i + bar_count]);
			}
			for (i = 0; i < 12 + bar_count - num; i++)
			{
				Term_putch(COL_STATBAR + i + num - bar_count, ROW_STATBAR,
					colour[i], letter[i]);
			}
		}
	}
}




/* Show status bar */

static void prt_status(void)
{
	int num = 0;
	char letter[30];
	byte colour[30];

	/* Collate active flags */

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		letter[num] = 'H';
		colour[num] = TERM_VIOLET;
		num++;
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		letter[num] = 'B';
		colour[num] = TERM_L_DARK;
		num++;
	}

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		letter[num] = 'I';
		colour[num] = TERM_L_BLUE;
		num++;
	}

	/* Timed esp */
	if (p_ptr->tim_esp)
	{
		letter[num] = 'E';
		colour[num] = TERM_ORANGE;
		num++;
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		letter[num] = 'I';
		colour[num] = TERM_L_RED;
		num++;
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		letter[num] = 'P';
		colour[num] = TERM_RED;
		num++;
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		letter[num] = 'C';
		colour[num] = TERM_VIOLET;
		num++;
	}

	/* Fast */
	if (p_ptr->fast)
	{
		letter[num] = 'S';
		colour[num] = TERM_GREEN;
		num++;
	}

	/* Slow */
	if (p_ptr->slow)
	{
		letter[num] = 'S';
		colour[num] = TERM_RED;
		num++;
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		letter[num] = 'E';
		colour[num] = TERM_L_DARK;
		num++;
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		letter[num] = 'I';
		colour[num] = TERM_YELLOW;
		num++;
	}

	/* Wraith form */
	if (p_ptr->wraith_form)
	{
		letter[num] = 'W';
		colour[num] = TERM_L_DARK;
		num++;
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		letter[num] = 'H';
		colour[num] = TERM_WHITE;
		num++;
	}

	/* Super Heroism / berserk */
	if (p_ptr->shero)
	{
		letter[num] = 'B';
		colour[num] = TERM_RED;
		num++;
	}

	/* Blessed */
	if (p_ptr->blessed)
	{
		letter[num] = 'B';
		colour[num] = TERM_WHITE;
		num++;
	}

	/* Shield */
	if (p_ptr->shield)
	{
		letter[num] = 'S';
		colour[num] = TERM_WHITE;
		num++;
	}

	/* Oppose Acid */
	if (p_ptr->oppose_acid)
	{
		letter[num] = 'A';
		colour[num] = TERM_GREEN;
		num++;
	}

	/* Oppose Lightning */
	if (p_ptr->oppose_elec)
	{
		letter[num] = 'E';
		colour[num] = TERM_BLUE;
		num++;
	}

	/* Oppose Fire */
	if (p_ptr->oppose_fire)
	{
		letter[num] = 'F';
		colour[num] = TERM_RED;
		num++;
	}

	/* Oppose Cold */
	if (p_ptr->oppose_cold)
	{
		letter[num] = 'C';
		colour[num] = TERM_WHITE;
		num++;
	}

	/* Oppose Poison */
	if (p_ptr->oppose_pois)
	{
		letter[num] = 'P';
		colour[num] = TERM_GREEN;
		num++;
	}

	/* Word of Recall */
	if (p_ptr->word_recall)
	{
		letter[num] = 'W';
		colour[num] = TERM_WHITE;
		num++;
	}

	/* Confusing Hands */
	if (p_ptr->confusing)
	{
		letter[num] = 'C';
		colour[num] = TERM_RED;
		num++;
	}

	if (num)
	{
		/* Display the status bar if there are flags set */
		show_status_bar(letter, colour, num);
	}
	else clear_status_bar();
}


/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	cptr p = "";

	/* Wizard */
	if (wizard)
	{
		p = "[=-WIZARD-=]";
	}

	/* Winner */
	else if (total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "***WINNER***";
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

	sprintf(tmp, "%6d", p_ptr->lev);

	if (p_ptr->lev >= p_ptr->max_plv)
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

	(void)sprintf(out_val, "%8ld", (long)p_ptr->exp);

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
	sprintf(tmp, "%9ld", (long)p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD + 3);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
	char tmp[32];

	put_str("Cur AC ", ROW_AC, COL_AC);
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
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

	sprintf(tmp, "%5d", p_ptr->mhp);
	color = TERM_L_GREEN;

	c_put_str(color, tmp, ROW_MAXHP, COL_MAXHP + 7);


	put_str("Cur HP ", ROW_CURHP, COL_CURHP);

	sprintf(tmp, "%5d", p_ptr->chp);

	if (p_ptr->chp >= p_ptr->mhp)
	{
		color = TERM_L_GREEN;
	}
	else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
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
	if (!mp_ptr->spell_book) return;


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
	else if (p_ptr->csp > (p_ptr->msp * hitpoint_warn) / 10)
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
static void prt_depth(void)
{
	char depths[32];

	if (p_ptr->inside_arena)
	{
		strcpy(depths, "Arena");
	}
	else if (p_ptr->inside_quest)
	{
		strcpy(depths, "Quest");
	}
	else if (!dun_level)
	{
		if (p_ptr->town_num)
			strcpy(depths, town[p_ptr->town_num].name);
		else
			strcpy(depths, "Wilderness");
	}
	else if (depth_in_feet)
	{
		(void)sprintf(depths, "%d ft", dun_level * 50);
	}
	else
	{
		(void)sprintf(depths, "Lev %d", dun_level);
	}

	/* Right-Adjust the "depth", and clear old values */
	prt(format("%7s", depths), 23, COL_DEPTH);
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
	else if (resting)
	{
		int i;

		/* Start with "Rest" */
		strcpy(text, "Rest      ");

		/* Extensive (timed) rest */
		if (resting >= 1000)
		{
			i = resting / 100;
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
		else if (resting >= 100)
		{
			i = resting;
			text[9] = '0' + (i % 10);
			i = i / 10;
			text[8] = '0' + (i % 10);
			text[7] = '0' + (i / 10);
		}

		/* Medium (timed) rest */
		else if (resting >= 10)
		{
			i = resting;
			text[9] = '0' + (i % 10);
			text[8] = '0' + (i / 10);
		}

		/* Short (timed) rest */
		else if (resting > 0)
		{
			i = resting;
			text[9] = '0' + (i);
		}

		/* Rest until healed */
		else if (resting == -1)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '*';
		}

		/* Rest until done */
		else if (resting == -2)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '&';
		}
	}

	/* Repeating */
	else if (command_rep)
	{
		if (command_rep > 999)
		{
			(void)sprintf(text, "Rep. %3d00", command_rep / 100);
		}
		else
		{
			(void)sprintf(text, "Repeat %3d", command_rep);
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

	byte attr = TERM_WHITE;
	char buf[32] = "";

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

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


static void prt_cut(void)
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



static void prt_stun(void)
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
 * Redraw the "monster health bar"	-DRS-
 * Rather extensive modifications by	-BEN-
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.
 *
 * Display the monster health bar (affectionately known as the
 * "health-o-meter").  Clear health bar if nothing is being tracked.
 * Auto-track current target monster when bored.  Note that the
 * health-bar stops tracking any monster that "disappears".
 */
static void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

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
		if (pct >= 10) attr = TERM_L_RED;

		/* Wounded */
		if (pct >= 25) attr = TERM_ORANGE;

		/* Somewhat Wounded */
		if (pct >= 60) attr = TERM_YELLOW;

		/* Healthy */
		if (pct >= 100) attr = TERM_L_GREEN;

		/* Afraid */
		if (m_ptr->monfear) attr = TERM_VIOLET;

		/* Asleep */
		if (m_ptr->csleep) attr = TERM_BLUE;

		/* Invulnerable */
		if (m_ptr->invulner) attr = TERM_WHITE;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
	}

#endif

}



/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;

	/* Race and Class */
	prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
	prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();

	/* All Stats */
	for (i = 0; i < 6; i++) prt_stat(i);

	/* Status Bar */
	prt_status();

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
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_INVEN))) continue;

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
static void fix_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_EQUIP))) continue;

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
 * Hack -- display equipment in sub-windows
 */
static void fix_spell(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_SPELL))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display spell list */
		display_spell_list();

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
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_PLAYER))) continue;

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
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_MESSAGE))) continue;

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
static void fix_overhead(void)
{
	int j;

	int cy, cx;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OVERHEAD))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Redraw map */
		display_map(&cy, &cx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display dungeon view in sub-windows
 */
static void fix_dungeon(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_DUNGEON))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Redraw dungeon view */
		display_dungeon();

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
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_MONSTER))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */
		if (p_ptr->monster_race_idx) display_roff(p_ptr->monster_race_idx);

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
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OBJECT))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */
		if (p_ptr->object_kind_idx) display_koff(p_ptr->object_kind_idx);

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
	int			i, j, k, levels;
	int			num_allowed, num_known;

	magic_type		*s_ptr;
	int use_realm1 = p_ptr->realm1 - 1;
	int use_realm2 = p_ptr->realm2 - 1;
	int which;


	cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" : "prayer");


	/* Hack -- must be literate */
	if (!mp_ptr->spell_book) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;


	/* Determine the number of spells allowed */
	levels = p_ptr->lev - mp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Extract total allowed spells */
	num_allowed = (adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2);


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < 64; j++)
	{
		/* Count known spells */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
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
		if (!spell_learned1 && !spell_learned2) break;

		/* Access the spell */
		j = spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;


		/* Get the spell */
		if (j < 32)
			s_ptr = &mp_ptr->info[use_realm1][j];
		else
			s_ptr = &mp_ptr->info[use_realm2][j%32];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

		/* Is it known? */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				spell_forgotten1 |= (1L << j);
				spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 |= (1L << (j - 32));
				spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
			spell_names[which][j%32]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = 63; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Efficiency -- all done */
		if (!spell_learned1 && !spell_learned2) break;

		/* Get the (i+1)th spell learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				spell_forgotten1 |= (1L << j);
				spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 |= (1L << (j - 32));
				spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
			           spell_names[which][j%32]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < 64; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Efficiency -- all done */
		if (!spell_forgotten1 && !spell_forgotten2) break;

		/* Get the next spell we learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) break;

		/* Access the spell */
		if (j < 32)
			s_ptr = &mp_ptr->info[use_realm1][j];
		else
			s_ptr = &mp_ptr->info[use_realm2][j % 32];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* First set of spells */
		if ((j < 32) ?
		    (spell_forgotten1 & (1L << j)) :
		    (spell_forgotten2 & (1L << (j - 32))))
		{
			/* No longer forgotten - known once more */
			if (j < 32)
			{
				spell_forgotten1 &= ~(1L << j);
				spell_learned1 |= (1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 &= ~(1L << (j - 32));
				spell_learned2 |= (1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msg_format("You have remembered the %s of %s.",
			           p, spell_names[which][j%32]);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < (p_ptr->realm2 != REALM_NONE ? 64 : 32); j++)
	{
		/* Access the spell */
		if (j < 32)
			s_ptr = &mp_ptr->info[use_realm1][j];
		else
			s_ptr = &mp_ptr->info[use_realm2][j % 32];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* Skip spells we already know */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
		{
			continue;
		}

		/* Count it */
		k++;
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

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
static void calc_mana(void)
{
	int		msp, levels, cur_wgt, max_wgt;

	object_type	*o_ptr;


	/* Hack -- Must be literate */
	if (!mp_ptr->spell_book) return;

	if (p_ptr->pclass == CLASS_MINDCRAFTER)
	{
		levels = p_ptr->lev;
	}
	else
	{
		/* Extract "effective" player level */
		levels = (p_ptr->lev - mp_ptr->spell_first) + 1;
	}


	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana */
	msp = adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2;

	/* Hack - the weak spellcasters get half as much mana (rounded up) in Oangband. */
	/* But this is not Oangband... */
#if 0
	switch (p_ptr->pclass)
	{
		case CLASS_ROGUE:
		case CLASS_RANGER:
		case CLASS_PALADIN:
		case CLASS_MONK:
		case CLASS_CHAOS_WARRIOR:
		{
			msp = (msp + 1) / 2;
			break;
		}
		default:
		{
			break;
		}
	}
#endif

	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Hack: High mages have a 25% mana bonus */
	if (msp && (p_ptr->pclass == CLASS_HIGH_MAGE)) msp += msp / 4;


	/* Only mages are affected */
	if (mp_ptr->spell_book == TV_SORCERY_BOOK)
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
			 !(f2 & (TR2_FREE_ACT)) &&
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
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana by a percentage.  -LM- */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Subtract a percentage of maximum mana. */
		switch (p_ptr->pclass)
		{
			/* For these classes, mana is halved if armour
			 * is 30 pounds over their weight limit. */
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
			{
				msp -= msp * (cur_wgt - max_wgt) / 600;
				break;
			}

			/* Mana halved if armour is 40 pounds over weight limit. */
			case CLASS_PRIEST:
			case CLASS_MINDCRAFTER:
			{
				msp -= msp * (cur_wgt - max_wgt) / 800;
				break;
			}

			/* Mana halved if armour is 50 pounds over weight limit. */
			case CLASS_ROGUE:
			case CLASS_RANGER:
			case CLASS_MONK:
			{
				msp -= msp * (cur_wgt - max_wgt) / 1000;
				break;
			}

			/* Mana halved if armour is 60 pounds over weight limit. */
			case CLASS_PALADIN:
			case CLASS_CHAOS_WARRIOR:
			case CLASS_WARRIOR_MAGE:
			{
				msp -= msp * (cur_wgt - max_wgt) / 1200;
				break;
			}

			/* For new classes created, but not yet added to this formula. */
			default:
			{
				msp -= msp * (cur_wgt - max_wgt) / 800;
				break;
			}
		}
	}


	/* Mana can never be negative */
	if (msp < 0) msp = 0;


	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{
		/* Enforce maximum */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

		/* Save new mana */
		p_ptr->msp = msp;

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

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
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
	int bonus, mhp;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128);

	/* Calculate hitpoints */
	mhp = player_hp[p_ptr->lev - 1] + (bonus * p_ptr->lev / 2);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero) mhp += 10;
	if (p_ptr->shero) mhp += 30;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		/* Enforce maximum */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

		/* Save the new max-hitpoints */
		p_ptr->mhp = mhp;

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}
}



/*
 * Extract and set the current "lite radius"
 *
 * SWD: Experimental modification: multiple light sources have additive effect.
 *
 */
static void calc_torch(void)
{
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Examine actual lites */
		if ((i == INVEN_LITE) && (o_ptr->k_idx) && (o_ptr->tval == TV_LITE))
		{
			/* Torches (with fuel) provide some lite */
			if ((o_ptr->sval == SV_LITE_TORCH) && (o_ptr->pval > 0))
			{
				p_ptr->cur_lite += 1;
				continue;
			}

			/* Lanterns (with fuel) provide more lite */
			if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->pval > 0))
			{
				p_ptr->cur_lite += 2;
				continue;
			}

			/* Artifact Lites provide permanent, bright, lite */
			if (artifact_p(o_ptr))
			{
				p_ptr->cur_lite += 3;
				continue;
			}

			/* notreached */
		}
		else
		{
			/* Skip empty slots */
			if (!o_ptr->k_idx) continue;

			/* Extract the flags */
			object_flags(o_ptr, &f1, &f2, &f3);

			/* does this item glow? */
			if (f3 & TR3_LITE) p_ptr->cur_lite++;
		}

	}

	/* max radius is 5 without rewriting other code -- */
	/* see cave.c:update_lite() and defines.h:LITE_MAX */
	if (p_ptr->cur_lite > 5) p_ptr->cur_lite = 5;

	/* check if the player doesn't have a lite source, */
	/* but does glow as an intrinsic.                  */
	if (p_ptr->cur_lite == 0 && p_ptr->lite) p_ptr->cur_lite = 1;

	/* end experimental mods */

	/* Reduce lite when running if requested */
	if (running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
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

/* Calculate all class-based bonuses and penalties to melee Skill.  Oangband
 * recognizes that it takes a great deal of training to get critical hits with
 * a large, heavy weapon - training that many classes simply do not have the
 * time or inclination for.  -LM-
 */
static sint add_special_melee_skill(byte pclass, s16b weight, object_type *o_ptr)
{
	int add_skill = 0;

	switch (pclass)
	{
		/* Warrior.  Can use 15 lb weapons without penalty at level 1, and 45 lb weapons without penalty at 50th level. */
		case CLASS_WARRIOR:
		{
			add_skill = 25 + p_ptr->lev - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;
			break;
		}

		/* Mage/High Mage.  Can use 6 lb weapons without penalty at level 1, and 16 lb weapons without penalty at 50th level. */
		case CLASS_MAGE:
		case CLASS_HIGH_MAGE:
		{
			add_skill = 20 + (2 * p_ptr->lev / 3) - (weight / 3);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -30) add_skill = -30;
			break;
		}

		/* Priest.  Can use 12 lb weapons without penalty at level 1, and 22 lb weapons without penalty at 50th level. */
		case CLASS_PRIEST:
		{
			add_skill = 30 + (1 * p_ptr->lev / 2) - (weight / 4);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -25) add_skill = -25;
			break;
		}

		/* Rogue.  Can use 10 lb weapons without penalty at level 1, and 20 lb
		* weapons without penalty at 50th level. Can get a bonus for using light
		* weapons.  */
		case CLASS_ROGUE:
		{
			if (!o_ptr->k_idx) add_skill = 0;

			else
			{
				add_skill = 33 + (2 * p_ptr->lev / 3) - (weight / 3);
				if (add_skill > 0) add_skill = add_skill / 2;
				if (add_skill > 15) add_skill = 15;
				if (add_skill < -25) add_skill = -25;
			}
			break;
		}

		/* Ranger.  Can use 12 lb weapons without penalty at level 1, and 25 lb
		*weapons without penalty at 50th level. */
		case CLASS_RANGER:
		{
			add_skill = 25 + (1 * p_ptr->lev / 2) - (weight / 5);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -20) add_skill = -20;
			break;
		}

		/* Paladin/Chaos warrior/Warrior mage.  Can use 15 lb weapons without
		* penalty at level 1, and 45 lb weapons without penalty at 50th level. */
		case CLASS_PALADIN:
		case CLASS_CHAOS_WARRIOR:
		case CLASS_WARRIOR_MAGE:
		{
			add_skill = 25 + p_ptr->lev - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;
			break;
		}

		/* Monk.  Can use 5 lb weapons without penalty at level 1, and
		 * slightly over 12 lb weapons without penalty at 50th level. Much
		 * prefers to use hands and feet.
		 */
		case CLASS_MONK:
		{
			if (!o_ptr->k_idx) add_skill = 14 + (p_ptr->lev);
			else
			{
				add_skill = 16 + (p_ptr->lev / 2) - (weight / 3);
				if (add_skill > 0) add_skill = 0;
				if (add_skill < -30) add_skill = -30;
			}
			break;
		}

		/*
		* MindCrafter.  Can use 6 lb weapons without penalty at level 1,
		* and 16 lb weapons without penalty at 50th level.
		*/
		case CLASS_MINDCRAFTER:
		{
			add_skill = 20 + (2 * p_ptr->lev / 3) - (weight / 3);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -30) add_skill = -30;
			break;
		}
	}

	return (add_skill);
}

/* Calculate all class and race-based bonuses and penalties to missile Skill -LM- */
static sint add_special_missile_skill(byte pclass, s16b weight, object_type *o_ptr)
{
	int add_skill = 0;

	switch (pclass)
	{

		/* Rogues are good with slings. */
		case CLASS_ROGUE:
		{
			if (p_ptr->tval_ammo == TV_SHOT)
			{
				add_skill = 3 + p_ptr->lev / 4;
			}
			break;
		}

		/* Rangers have a high missile skill, but they are not supposed to
		* be great with xbows and slings. */
		case CLASS_RANGER:
		{
			if (p_ptr->tval_ammo == TV_SHOT)
			{
				add_skill = 0 - p_ptr->lev / 7;
			}
			if (p_ptr->tval_ammo == TV_BOLT)
			{
				add_skill = 0 - p_ptr->lev / 7;
			}
			break;
		}

		/* Monks get a small bonus with slings. */
		case CLASS_MONK:
		{
			if (p_ptr->tval_ammo == TV_SHOT)
			{
				add_skill = p_ptr->lev / 7;
			}
		}
	}
	return (add_skill);
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
void calc_bonuses(void)
{
	int             i, j, hold;
	int             old_speed;
	int             old_telepathy;
	int             old_see_inv;
	int             old_dis_ac;
	int             old_dis_to_a;
	int             extra_blows;
	int             extra_shots;
	object_type     *o_ptr;
	u32b            f1, f2, f3;


	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;


	/* Clear extra blows/shots */
	extra_blows = extra_shots = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;


	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Start with "normal" speed */
	p_ptr->pspeed = 110;

	/* Start with a single blow per turn */
	p_ptr->num_blow = 1;

	/* Start with a single shot per turn */
	p_ptr->num_fire = 1;

	/* Reset the "xtra" tval */
	p_ptr->tval_xtra = 0;

	/* Reset the "ammo" tval */
	p_ptr->tval_ammo = 0;

	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->xtra_might = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->pass_wall = FALSE;
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
	p_ptr->resist_conf = FALSE;
	p_ptr->resist_sound = FALSE;
	p_ptr->resist_lite = FALSE;
	p_ptr->resist_dark = FALSE;
	p_ptr->resist_chaos = FALSE;
	p_ptr->resist_disen = FALSE;
	p_ptr->resist_shard = FALSE;
	p_ptr->resist_nexus = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_neth = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->reflect = FALSE;
	p_ptr->sh_fire = FALSE;
	p_ptr->sh_elec = FALSE;
	p_ptr->anti_magic = FALSE;
	p_ptr->anti_tele = FALSE;

	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;


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

#ifdef USE_SCRIPT

	if (!get_player_flags_callback())

#endif /* USE_SCRIPT */

	{
		switch (p_ptr->pclass)
		{
			case CLASS_WARRIOR:
				if (p_ptr->lev > 29) p_ptr->resist_fear = TRUE;
				break;
			case CLASS_PALADIN:
				if (p_ptr->lev > 39) p_ptr->resist_fear = TRUE;
				break;
			case CLASS_CHAOS_WARRIOR:
				if (p_ptr->lev > 29) p_ptr->resist_chaos = TRUE;
				if (p_ptr->lev > 39) p_ptr->resist_fear = TRUE;
				break;
			case CLASS_MINDCRAFTER:
				if (p_ptr->lev >  9) p_ptr->resist_fear = TRUE;
				if (p_ptr->lev > 19) p_ptr->sustain_wis = TRUE;
				if (p_ptr->lev > 29) p_ptr->resist_conf = TRUE;
				if (p_ptr->lev > 39) p_ptr->telepathy = TRUE;
				break;
			case CLASS_MONK:
				/* Unencumbered Monks become faster every 10 levels */
				if (!(monk_heavy_armor()))
#ifndef MONK_HACK
					if (!((p_ptr->prace == RACE_KLACKON) ||
						(p_ptr->prace == RACE_SPRITE)))
#endif /* MONK_HACK */
						p_ptr->pspeed += (p_ptr->lev) / 10;

				/* Free action if unencumbered at level 25 */
				if  ((p_ptr->lev > 24) && !monk_heavy_armor())
					p_ptr->free_act = TRUE;
				break;
		}

		/***** Races ****/
		switch (p_ptr->prace)
		{
			case RACE_ELF:
				p_ptr->resist_lite = TRUE;
				break;
			case RACE_HOBBIT:
				p_ptr->sustain_dex = TRUE;
				break;
			case RACE_GNOME:
				p_ptr->free_act = TRUE;
				break;
			case RACE_DWARF:
				p_ptr->resist_blind = TRUE;
				break;
			case RACE_HALF_ORC:
				p_ptr->resist_dark = TRUE;
				break;
			case RACE_HALF_TROLL:
				p_ptr->sustain_str = TRUE;

				if (p_ptr->lev > 14)
				{
					/* High level trolls heal fast... */
					p_ptr->regenerate = TRUE;

					if (p_ptr->pclass == CLASS_WARRIOR)
					{
						p_ptr->slow_digest = TRUE;
						/* Let's not make Regeneration
						 * a disadvantage for the poor warriors who can
						 * never learn a spell that satisfies hunger (actually
						 * neither can rogues, but half-trolls are not
						 * supposed to play rogues) */
					}
				}
				break;
			case RACE_AMBERITE:
				p_ptr->sustain_con = TRUE;
				p_ptr->regenerate = TRUE;  /* Amberites heal fast... */
				break;
			case RACE_HIGH_ELF:
				p_ptr->resist_lite = TRUE;
				p_ptr->see_inv = TRUE;
				break;
			case RACE_BARBARIAN:
				p_ptr->resist_fear = TRUE;
				break;
			case RACE_HALF_OGRE:
				p_ptr->resist_dark = TRUE;
				p_ptr->sustain_str = TRUE;
				break;
			case RACE_HALF_GIANT:
				p_ptr->sustain_str = TRUE;
				p_ptr->resist_shard = TRUE;
				break;
			case RACE_HALF_TITAN:
				p_ptr->resist_chaos = TRUE;
				break;
			case RACE_CYCLOPS:
				p_ptr->resist_sound = TRUE;
				break;
			case RACE_YEEK:
				p_ptr->resist_acid = TRUE;
				if (p_ptr->lev > 19) p_ptr->immune_acid = TRUE;
				break;
			case RACE_KLACKON:
				p_ptr->resist_conf = TRUE;
				p_ptr->resist_acid = TRUE;

				/* Klackons become faster */
				p_ptr->pspeed += (p_ptr->lev) / 10;
				break;
			case RACE_KOBOLD:
				p_ptr->resist_pois = TRUE;
				break;
			case RACE_NIBELUNG:
				p_ptr->resist_disen = TRUE;
				p_ptr->resist_dark = TRUE;
				break;
			case RACE_DARK_ELF:
				p_ptr->resist_dark = TRUE;
				if (p_ptr->lev > 19) p_ptr->see_inv = TRUE;
				break;
			case RACE_DRACONIAN:
				p_ptr->ffall = TRUE;
				if (p_ptr->lev >  4) p_ptr->resist_fire = TRUE;
				if (p_ptr->lev >  9) p_ptr->resist_cold = TRUE;
				if (p_ptr->lev > 14) p_ptr->resist_acid = TRUE;
				if (p_ptr->lev > 19) p_ptr->resist_elec = TRUE;
				if (p_ptr->lev > 34) p_ptr->resist_pois = TRUE;
				break;
			case RACE_MIND_FLAYER:
				p_ptr->sustain_int = TRUE;
				p_ptr->sustain_wis = TRUE;
				if (p_ptr->lev > 14) p_ptr->see_inv = TRUE;
				if (p_ptr->lev > 29) p_ptr->telepathy = TRUE;
				break;
			case RACE_IMP:
				p_ptr->resist_fire = TRUE;
				if (p_ptr->lev > 9) p_ptr->see_inv = TRUE;
				break;
			case RACE_GOLEM:
				p_ptr->slow_digest = TRUE;
				p_ptr->free_act = TRUE;
				p_ptr->see_inv = TRUE;
				p_ptr->resist_pois = TRUE;
				if (p_ptr->lev > 34) p_ptr->hold_life = TRUE;
				break;
			case RACE_SKELETON:
				p_ptr->resist_shard = TRUE;
				p_ptr->hold_life = TRUE;
				p_ptr->see_inv = TRUE;
				p_ptr->resist_pois = TRUE;
				if (p_ptr->lev > 9) p_ptr->resist_cold = TRUE;
				break;
			case RACE_ZOMBIE:
				p_ptr->resist_neth = TRUE;
				p_ptr->hold_life = TRUE;
				p_ptr->see_inv = TRUE;
				p_ptr->resist_pois = TRUE;
				p_ptr->slow_digest = TRUE;
				if (p_ptr->lev > 4) p_ptr->resist_cold = TRUE;
				break;
			case RACE_VAMPIRE:
				p_ptr->resist_dark = TRUE;
				p_ptr->hold_life = TRUE;
				p_ptr->resist_neth = TRUE;
				p_ptr->resist_cold = TRUE;
				p_ptr->resist_pois = TRUE;
				p_ptr->lite = TRUE;
				break;
			case RACE_SPECTRE:
				p_ptr->resist_neth = TRUE;
				p_ptr->hold_life = TRUE;
				p_ptr->see_inv = TRUE;
				p_ptr->resist_pois = TRUE;
				p_ptr->slow_digest = TRUE;
				p_ptr->resist_cold = TRUE;
				p_ptr->pass_wall = TRUE;
				if (p_ptr->lev > 34) p_ptr->telepathy = TRUE;
				break;
			case RACE_SPRITE:
				p_ptr->ffall = TRUE;
				p_ptr->resist_lite = TRUE;

				/* Sprites become faster */
				p_ptr->pspeed += p_ptr->lev / 10;
				break;
			case RACE_BEASTMAN:
				p_ptr->resist_conf = TRUE;
				p_ptr->resist_sound = TRUE;
				break;
		}
	}

	/* Hack -- apply racial/class stat maxes */
	if (maximize_mode)
	{
		/* Apply the racial modifiers */
		for (i = 0; i < 6; i++)
		{
			/* Modify the stats for "race" */
			p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
		}
	}


	/* I'm adding the mutations here for the lack of a better place... */
	if (p_ptr->muta3)
	{
		/* Hyper Strength */
		if (p_ptr->muta3 & MUT3_HYPER_STR)
		{
			p_ptr->stat_add[A_STR] += 4;
		}

		/* Puny */
		if (p_ptr->muta3 & MUT3_PUNY)
		{
			p_ptr->stat_add[A_STR] -= 4;
		}

		/* Living computer */
		if (p_ptr->muta3 & MUT3_HYPER_INT)
		{
			p_ptr->stat_add[A_INT] += 4;
			p_ptr->stat_add[A_WIS] += 4;
		}

		/* Moronic */
		if (p_ptr->muta3 & MUT3_MORONIC)
		{
			p_ptr->stat_add[A_INT] -= 4;
			p_ptr->stat_add[A_WIS] -= 4;
		}

		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
			p_ptr->stat_add[A_CON] += 4;
		}

		if (p_ptr->muta3 & MUT3_XTRA_FAT)
		{
			p_ptr->stat_add[A_CON] += 2;
			p_ptr->pspeed -= 2;
		}

		if (p_ptr->muta3 & MUT3_ALBINO)
		{
			p_ptr->stat_add[A_CON] -= 4;
		}

		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
			p_ptr->stat_add[A_CON] -= 2;
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->regenerate = FALSE;
			/* Cancel innate regeneration */
		}

		if (p_ptr->muta3 & MUT3_SILLY_VOI)
		{
			p_ptr->stat_add[A_CHR] -= 4;
		}

		if (p_ptr->muta3 & MUT3_BLANK_FAC)
		{
			p_ptr->stat_add[A_CHR] -= 1;
		}

		if (p_ptr->muta3 & MUT3_XTRA_EYES)
		{
			p_ptr->skill_fos += 15;
			p_ptr->skill_srh += 15;
		}

		if (p_ptr->muta3 & MUT3_MAGIC_RES)
		{
			p_ptr->skill_sav += (15 + (p_ptr->lev / 5));
		}

		if (p_ptr->muta3 & MUT3_XTRA_NOIS)
		{
			p_ptr->skill_stl -= 3;
		}

		if (p_ptr->muta3 & MUT3_INFRAVIS)
		{
			p_ptr->see_infra += 3;
		}

		if (p_ptr->muta3 & MUT3_XTRA_LEGS)
		{
			p_ptr->pspeed += 3;
		}

		if (p_ptr->muta3 & MUT3_SHORT_LEG)
		{
			p_ptr->pspeed -= 3;
		}

		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
		{
			p_ptr->sh_elec = TRUE;
		}

		if (p_ptr->muta3 & MUT3_FIRE_BODY)
		{
			p_ptr->sh_fire = TRUE;
			p_ptr->lite = TRUE;
		}

		if (p_ptr->muta3 & MUT3_WART_SKIN)
		{
			p_ptr->stat_add[A_CHR] -= 2;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
		}

		if (p_ptr->muta3 & MUT3_SCALES)
		{
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->to_a += 10;
			p_ptr->dis_to_a += 10;
		}

		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
			p_ptr->stat_add[A_DEX] -= 1;
			p_ptr->to_a += 25;
			p_ptr->dis_to_a += 25;
		}

		if (p_ptr->muta3 & MUT3_WINGS)
		{
			p_ptr->ffall = TRUE;
		}

		if (p_ptr->muta3 & MUT3_FEARLESS)
		{
			p_ptr->resist_fear = TRUE;
		}

		if (p_ptr->muta3 & MUT3_REGEN)
		{
			p_ptr->regenerate = TRUE;
		}

		if (p_ptr->muta3 & MUT3_ESP)
		{
			p_ptr->telepathy = TRUE;
		}

		if (p_ptr->muta3 & MUT3_LIMBER)
		{
			p_ptr->stat_add[A_DEX] += 3;
		}

		if (p_ptr->muta3 & MUT3_ARTHRITIS)
		{
			p_ptr->stat_add[A_DEX] -= 3;
		}

		if (p_ptr->muta3 & MUT3_MOTION)
		{
			p_ptr->free_act = TRUE;
			p_ptr->skill_stl += 1;
		}

#ifdef MUT3_SUS_STATS
		if (p_ptr->muta3 & MUT3_SUS_STATS)
		{
			p_ptr->sustain_con = TRUE;
			if (p_ptr->lev > 9)
				p_ptr->sustain_str = TRUE;
			if (p_ptr->lev > 19)
				p_ptr->sustain_dex = TRUE;
			if (p_ptr->lev > 29)
				p_ptr->sustain_wis = TRUE;
			if (p_ptr->lev > 39)
				p_ptr->sustain_int = TRUE;
			if (p_ptr->lev > 49)
				p_ptr->sustain_chr = TRUE;
		}
#endif /* MUT3_SUS_STATS */

		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
			p_ptr->stat_add[A_CHR] = 0;
		}
	}


	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stats */
		if (f1 & (TR1_STR)) p_ptr->stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT)) p_ptr->stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS)) p_ptr->stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX)) p_ptr->stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON)) p_ptr->stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_CHR)) p_ptr->stat_add[A_CHR] += o_ptr->pval;

		/* Affect stealth */
		if (f1 & (TR1_STEALTH)) p_ptr->skill_stl += o_ptr->pval;

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skill_srh += (o_ptr->pval * 5);

		/* Affect searching frequency (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skill_fos += (o_ptr->pval * 5);

		/* Affect infravision */
		if (f1 & (TR1_INFRA)) p_ptr->see_infra += o_ptr->pval;

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL)) p_ptr->skill_dig += (o_ptr->pval * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED)) p_ptr->pspeed += o_ptr->pval;

		/* Affect blows */
		if (f1 & (TR1_BLOWS)) extra_blows += o_ptr->pval;

		/* Hack -- cause earthquakes */
		if (f1 & (TR1_IMPACT)) p_ptr->impact = TRUE;

		/* Boost shots */
		if (f3 & (TR3_XTRA_SHOTS)) extra_shots++;

		/* Various flags */
		if (f3 & (TR3_AGGRAVATE))   p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT))    p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP))   p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_BLESSED))     p_ptr->bless_blade = TRUE;
		if (f3 & (TR3_XTRA_MIGHT))  p_ptr->xtra_might = TRUE;
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_REGEN))       p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY))   p_ptr->telepathy = TRUE;
		if (f3 & (TR3_LITE))        p_ptr->lite = TRUE;
		if (f3 & (TR3_SEE_INVIS))   p_ptr->see_inv = TRUE;
		if (f3 & (TR3_FEATHER))     p_ptr->ffall = TRUE;
		if (f2 & (TR2_FREE_ACT))    p_ptr->free_act = TRUE;
		if (f2 & (TR2_HOLD_LIFE))   p_ptr->hold_life = TRUE;
		if (f3 & (TR3_WRAITH))      set_wraith_form(20);

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE)) p_ptr->immune_fire = TRUE;
		if (f2 & (TR2_IM_ACID)) p_ptr->immune_acid = TRUE;
		if (f2 & (TR2_IM_COLD)) p_ptr->immune_cold = TRUE;
		if (f2 & (TR2_IM_ELEC)) p_ptr->immune_elec = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID))   p_ptr->resist_acid = TRUE;
		if (f2 & (TR2_RES_ELEC))   p_ptr->resist_elec = TRUE;
		if (f2 & (TR2_RES_FIRE))   p_ptr->resist_fire = TRUE;
		if (f2 & (TR2_RES_COLD))   p_ptr->resist_cold = TRUE;
		if (f2 & (TR2_RES_POIS))   p_ptr->resist_pois = TRUE;
		if (f2 & (TR2_RES_FEAR))   p_ptr->resist_fear = TRUE;
		if (f2 & (TR2_RES_CONF))   p_ptr->resist_conf = TRUE;
		if (f2 & (TR2_RES_SOUND))  p_ptr->resist_sound = TRUE;
		if (f2 & (TR2_RES_LITE))   p_ptr->resist_lite = TRUE;
		if (f2 & (TR2_RES_DARK))   p_ptr->resist_dark = TRUE;
		if (f2 & (TR2_RES_CHAOS))  p_ptr->resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN))  p_ptr->resist_disen = TRUE;
		if (f2 & (TR2_RES_SHARDS)) p_ptr->resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS))  p_ptr->resist_nexus = TRUE;
		if (f2 & (TR2_RES_BLIND))  p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_NETHER)) p_ptr->resist_neth = TRUE;

		if (f2 & (TR2_REFLECT))  p_ptr->reflect = TRUE;
		if (f3 & (TR3_SH_FIRE))  p_ptr->sh_fire = TRUE;
		if (f3 & (TR3_SH_ELEC))  p_ptr->sh_elec = TRUE;
		if (f3 & (TR3_NO_MAGIC)) p_ptr->anti_magic = TRUE;
		if (f3 & (TR3_NO_TELE))  p_ptr->anti_tele = TRUE;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
		if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
		if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
		if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
		if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
		if (f2 & (TR2_SUST_CHR)) p_ptr->sustain_chr = TRUE;

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h += o_ptr->to_h;
		p_ptr->to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr)) p_ptr->dis_to_d += o_ptr->to_d;
	}

	/* Monks get extra ac for armour _not worn_ */
	if ((p_ptr->pclass == CLASS_MONK) && !monk_heavy_armor())
	{
		if (!(inventory[INVEN_BODY].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev * 3) / 2;
			p_ptr->dis_to_a += (p_ptr->lev * 3) / 2;
		}
		if (!(inventory[INVEN_OUTER].k_idx) && (p_ptr->lev > 15))
		{
			p_ptr->to_a += ((p_ptr->lev - 13) / 3);
			p_ptr->dis_to_a += ((p_ptr->lev - 13) / 3);
		}
		if (!(inventory[INVEN_ARM].k_idx) && (p_ptr->lev > 10))
		{
			p_ptr->to_a += ((p_ptr->lev - 8) / 3);
			p_ptr->dis_to_a += ((p_ptr->lev - 8) / 3);
		}
		if (!(inventory[INVEN_HEAD].k_idx) && (p_ptr->lev > 4))
		{
			p_ptr->to_a += (p_ptr->lev - 2) / 3;
			p_ptr->dis_to_a += (p_ptr->lev -2) / 3;
		}
		if (!(inventory[INVEN_HANDS].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev / 2);
			p_ptr->dis_to_a += (p_ptr->lev / 2);
		}
		if (!(inventory[INVEN_FEET].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev / 3);
			p_ptr->dis_to_a += (p_ptr->lev / 3);
		}
	}

	/* Hack -- aura of fire also provides light */
	if (p_ptr->sh_fire) p_ptr->lite = TRUE;

	/* Golems also get an intrinsic AC bonus */
	if (p_ptr->prace == RACE_GOLEM)
	{
		p_ptr->to_a += 20 + (p_ptr->lev / 5);
		p_ptr->dis_to_a += 20 + (p_ptr->lev / 5);
	}

	/* Calculate stats */
	for (i = 0; i < 6; i++)
	{
		int top, use, ind;

		/* Extract the new "stat_use" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], p_ptr->stat_add[i]);

		/* Notice changes */
		if (p_ptr->stat_top[i] != top)
		{
			/* Save the new value */
			p_ptr->stat_top[i] = top;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}


		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->stat_add[i]);

		if ((i == A_CHR) && (p_ptr->muta3 & MUT3_ILL_NORM))
		{
			/* 10 to 18/90 charisma, guaranteed, based on level */
			if (use < 8 + 2 * p_ptr->lev)
			{
				use = 8 + 2 * p_ptr->lev;
			}
		}

		/* Notice changes */
		if (p_ptr->stat_use[i] != use)
		{
			/* Save the new value */
			p_ptr->stat_use[i] = use;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}


		/* Values: 3, 4, ..., 17 */
		if (use <= 18) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

		/* Notice changes */
		if (p_ptr->stat_ind[i] != ind)
		{
			/* Save the new index */
			p_ptr->stat_ind[i] = ind;

			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in INT may affect Mana/Spells */
			else if (i == A_INT)
			{
				if (mp_ptr->spell_stat == A_INT)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_WIS)
			{
				if (mp_ptr->spell_stat == A_WIS)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}
	}


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

	/* wraith_form */
	if (p_ptr->wraith_form)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
		p_ptr->reflect = TRUE;
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

	/* Temporary "telepathy" */
	if (p_ptr->tim_esp)
	{
		p_ptr->telepathy = TRUE;
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


	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->hero || p_ptr->shero)
	{
		p_ptr->resist_fear = TRUE;
	}


	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		p_ptr->update |= (PU_MONSTERS);
	}


	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);


	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);


	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}


	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];


	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];


	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to carry a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}


	/* Compute "extra shots" if needed */
	if (o_ptr->k_idx)
	{
		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
			case SV_SLING:
			{
				p_ptr->tval_ammo = TV_SHOT;
				break;
			}

			case SV_SHORT_BOW:
			case SV_LONG_BOW:
			{
				p_ptr->tval_ammo = TV_ARROW;
				break;
			}

			case SV_LIGHT_XBOW:
			case SV_HEAVY_XBOW:
			{
				p_ptr->tval_ammo = TV_BOLT;
				break;
			}
		}

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Hack -- Rangers love Bows */
			if ((p_ptr->pclass == CLASS_RANGER) &&
			    (p_ptr->tval_ammo == TV_ARROW))
			{
				/* Extra shot at level 20 */
				if (p_ptr->lev >= 20) p_ptr->num_fire++;

				/* Extra shot at level 40 */
				if (p_ptr->lev >= 40) p_ptr->num_fire++;
			}

			/* Hack -- Rangers can use XBows as well*/
			if ((p_ptr->pclass == CLASS_RANGER) &&
			    (p_ptr->tval_ammo == TV_BOLT))
			{
				/* Extra shot at level 20 */
				if (p_ptr->lev >= 30) p_ptr->num_fire++;
			}

			/* Hack -- Rogues love Slings */
			if ((p_ptr->pclass == CLASS_ROGUE) &&
			    (p_ptr->tval_ammo == TV_SHOT))
			{
				/* Extra shot at level 20 */
				if (p_ptr->lev >= 20) p_ptr->num_fire++;

				/* Extra shot at level 40 */
				if (p_ptr->lev >= 40) p_ptr->num_fire++;
			}

			/*
			 * Addendum -- also "Reward" high level warriors,
			 * with _any_ missile weapon -- TY
			 */
			if (p_ptr->pclass == CLASS_WARRIOR &&
			   (p_ptr->tval_ammo <= TV_BOLT) &&
			   (p_ptr->tval_ammo >= TV_SHOT))
			{
				/* Extra shot at level 40 */
				if (p_ptr->lev >= 40) p_ptr->num_fire++;

			}
		}
	}
	/* Add all class and race-specific adjustments to missile Skill. -LM- */
	p_ptr->skill_thb += add_special_missile_skill (p_ptr->pclass, o_ptr->weight,
	 o_ptr);


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

		/* The player gets to swing a heavy weapon only once. -LM- */
		p_ptr->num_blow = 1;
	}


	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div = 0;

		/* Analyze the class */
		switch (p_ptr->pclass)
		{
			/* Warrior */
			case CLASS_WARRIOR:
				num = 5; wgt = 30; mul = 5; break;

			/* Mage */
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
				num = 2; wgt = 40; mul = 2; break;

			/* Priest, Mindcrafter */
			case CLASS_PRIEST:
			case CLASS_MINDCRAFTER:
				num = 4; wgt = 35; mul = 3; break;

			/* Rogue */
			case CLASS_ROGUE:
				num = 4; wgt = 30; mul = 3; break;

			/* Ranger */
			case CLASS_RANGER:
				num = 4; wgt = 35; mul = 4; break;

			/* Paladin */
			case CLASS_PALADIN:
				num = 4; wgt = 30; mul = 4; break;

			/* Warrior-Mage */
			case CLASS_WARRIOR_MAGE:
				num = 4; wgt = 35; mul = 3; break;

			/* Chaos Warrior */
			case CLASS_CHAOS_WARRIOR:
				num = 4; wgt = 30; mul = 4; break;

			/* Monk */
			case CLASS_MONK:
				num = ((p_ptr->lev < 40) ? 2 : 3); wgt = 40; mul = 4; break;
		}

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

		/* Access the strength vs weight */
		str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->num_blow > num) p_ptr->num_blow = num;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		/* Require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;

		/* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);
	}


	/* Different calculation for monks with empty hands */
	else if (p_ptr->pclass == CLASS_MONK && monk_empty_hands())
	{
		p_ptr->num_blow = 2;

		if (p_ptr->lev > 9) p_ptr->num_blow++;
		if (p_ptr->lev > 14) p_ptr->num_blow++;
		if (p_ptr->lev > 24) p_ptr->num_blow++;
		if (p_ptr->lev > 34) p_ptr->num_blow++;
		if (p_ptr->lev > 44) p_ptr->num_blow++;
		if (p_ptr->lev > 49) p_ptr->num_blow++;

		if (monk_heavy_armor()) p_ptr->num_blow /= 2;

		p_ptr->num_blow += extra_blows;

		if (!monk_heavy_armor())
		{
			p_ptr->to_h += (p_ptr->lev / 3);
			p_ptr->to_d += (p_ptr->lev / 3);

			p_ptr->dis_to_h += (p_ptr->lev / 3);
			p_ptr->dis_to_d += (p_ptr->lev / 3);
		}
	}
	/* Everyone gets two blows if not wielding a weapon. -LM- */
	else if (!o_ptr->k_idx) p_ptr->num_blow = 2;

	/* Add all other class-specific adjustments to melee Skill. -LM- */
	p_ptr->skill_thn += add_special_melee_skill(p_ptr->pclass, o_ptr->weight, o_ptr);

	/* Assume okay */
	p_ptr->icky_wield = FALSE;
	monk_armour_aux = FALSE;

	/* Extra bonus for warriors... */
	if (p_ptr->pclass == CLASS_WARRIOR)
	{
		p_ptr->to_h += (p_ptr->lev / 5);
		p_ptr->to_d += (p_ptr->lev / 5);

		p_ptr->dis_to_h += (p_ptr->lev / 5);
		p_ptr->dis_to_d += (p_ptr->lev / 5);
	}

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((p_ptr->pclass == CLASS_PRIEST) && (!p_ptr->bless_blade) &&
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

	if (monk_heavy_armor())
	{
		monk_armour_aux = TRUE;
	}


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
	p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 50);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 50);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 50);


	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	if ((p_ptr->anti_magic) && (p_ptr->skill_sav < 95)) p_ptr->skill_sav = 95;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

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

			if (hack_mind)
				chg_virtue(V_FAITH, -1);
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

	if (p_ptr->pclass == CLASS_MONK && (monk_armour_aux != monk_notify_aux))
	{
		if (monk_heavy_armor())
		{
			msg_print("The weight of your armor disrupts your balance.");

			if (hack_mind)
			{
				chg_virtue(V_HARMONY, -1);
			}
		}
		else
			msg_print("You regain your balance.");

		monk_notify_aux = monk_armour_aux;
	}

	p_ptr->align = friend_align;
}



/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;


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
	if (!p_ptr->update) return;


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
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;


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
	if (!p_ptr->redraw) return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;



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
		p_ptr->redraw &= ~(PR_MISC | PR_TITLE | PR_STATS | PR_STATUS);
		p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		prt_frame_basic();
	}

	if (p_ptr->redraw & (PR_EQUIPPY))
	{
		p_ptr->redraw &= ~(PR_EQUIPPY);
		print_equippy(); /* To draw / delete equippy chars */
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
		prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);
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

	if (p_ptr->redraw & (PR_STATUS))
	{
		p_ptr->redraw &= ~(PR_STATUS);
		prt_status();
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
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STUDY);
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
}


/*
 * Handle "p_ptr->window"
 */
void window_stuff(void)
{
	int j;

	u32b mask = 0L;


	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		/* Save usable flags */
		if (angband_term[j]) mask |= window_flag[j];
	}

	/* Apply usable flags */
	p_ptr->window &= mask;

	/* Nothing to do */
	if (!p_ptr->window) return;


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

	/* Display spell list */
	if (p_ptr->window & (PW_SPELL))
	{
		p_ptr->window &= ~(PW_SPELL);
		fix_spell();
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

	/* Display overhead view */
	if (p_ptr->window & (PW_DUNGEON))
	{
		p_ptr->window &= ~(PW_DUNGEON);
		fix_dungeon();
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
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();

	/* Window stuff */
	if (p_ptr->window) window_stuff();
}


bool monk_empty_hands(void)
{
	if (p_ptr->pclass != CLASS_MONK) return FALSE;

	return !(inventory[INVEN_WIELD].k_idx);
}


bool monk_heavy_armor(void)
{
	u16b monk_arm_wgt = 0;

	if (p_ptr->pclass != CLASS_MONK) return FALSE;

	/* Weight the armor */
	monk_arm_wgt += inventory[INVEN_BODY].weight;
	monk_arm_wgt += inventory[INVEN_HEAD].weight;
	monk_arm_wgt += inventory[INVEN_ARM].weight;
	monk_arm_wgt += inventory[INVEN_OUTER].weight;
	monk_arm_wgt += inventory[INVEN_HANDS].weight;
	monk_arm_wgt += inventory[INVEN_FEET].weight;

	return (monk_arm_wgt > (100 + (p_ptr->lev * 4)));
}

/* Count the number of random quests chosen */
int number_of_quests(void)
{
	int i, j;

	/* Clear the counter */
	i = 0;

	for (j = MIN_RANDOM_QUEST; j < MAX_RANDOM_QUEST; j++)
	{
		if (quest[j].status != QUEST_STATUS_UNTAKEN)
		{
			/* Increment count of quests taken. */
			i++;
		}
	}

	/* Return the number of quests taken */
	return (i);
}

