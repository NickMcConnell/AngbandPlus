/* File: xtra1.c */

/* Purpose: misc code */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


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
    value += amount * 10;

    if (value < 30) value = 30;

	/* Return new value */
	return (value);
}


/*
 * Print character info at given row, column in a 13 char field
 */
static void prt_field(cptr info, int col, int row)
{
	/* Dump 13 spaces to clear */
	put_fstr(col, row, "             ");

	/* Dump the info itself */
	put_fstr(col, row, CLR_L_BLUE "%s", info);
}

/*
 * Returns a formatted string in the buffer of
 * the stat value which is the first parameter
 * in the va_list.
 */
void stat_format(char *buf, uint max, cptr fmt, va_list *vp)
{
    int arg;
	
	/* Unused parameter */
	(void)fmt;
	
	/* Get the argument */
	arg = va_arg(*vp, int);

	/* Format the number for the stat */
	if (arg >= 400)
        strnfmt(buf, max, "  40+ ");
    else
        strnfmt(buf, max, "  %2d.%d", arg / 10, arg % 10);
}


/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		put_fstr(COL_STAT, ROW_STAT + stat, "%5s" CLR_YELLOW " %v",
				 stat_names_reduced[stat],
				 stat_format, p_ptr->stat_use[stat]);
	}

	/* Display "healthy" stat */
	else
	{
		put_fstr(COL_STAT, ROW_STAT + stat, "%5s" CLR_L_GREEN " %v",
				 stat_names[stat],
				 stat_format, p_ptr->stat_use[stat]);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == stat_cap(stat))
	{
		put_fstr(COL_STAT + 3, ROW_STAT + stat, "!");
	}
}

static int bar_count = 0;

static void clear_status_bar(void)
{
	put_fstr(COL_STATBAR, ROW_STATBAR, "            ");
}


static void show_status_bar(cptr *letter, int num)
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
			put_fstr(COL_STATBAR + i, ROW_STATBAR, letter[i]);
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
				put_fstr(COL_STATBAR + i, ROW_STATBAR, letter[i + bar_count]);
			}
		}
		else
		{
			/* Split over boundary */
			for (i = 0; i < num - bar_count; i++)
			{
				put_fstr(COL_STATBAR + i, ROW_STATBAR, letter[i + bar_count]);
			}
			for (i = 0; i < 12 + bar_count - num; i++)
			{
				put_fstr(COL_STATBAR + i + num - bar_count, ROW_STATBAR,
						   letter[i]);
			}
		}
	}
}


/*
 * Show status bar
 */
static void prt_status(void)
{
	int num = 0;
	cptr letter[30];

	/* Collate active flags */

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		letter[num] = CLR_VIOLET "H";
		num++;
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		letter[num] = CLR_L_DARK "B";
		num++;
	}

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		letter[num] = CLR_L_BLUE "I";
		num++;
	}

	/* Timed esp */
	if (p_ptr->tim_esp)
	{
		letter[num] = CLR_ORANGE "E";
		num++;
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		letter[num] = CLR_L_RED "I";
		num++;
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		letter[num] = CLR_RED "P";
		num++;
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		letter[num] = CLR_VIOLET "C";
		num++;
	}

	/* Fast */
	if (p_ptr->fast)
	{
		letter[num] = CLR_GREEN "S";
		num++;
	}

	/* Slow */
	if (p_ptr->slow)
	{
		letter[num] = CLR_RED "S";
		num++;
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		letter[num] = CLR_L_DARK "E";
		num++;
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		letter[num] = CLR_YELLOW "I";
		num++;
	}

	/* Wraith form */
	if (p_ptr->wraith_form)
	{
		letter[num] = CLR_L_DARK "W";
		num++;
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		letter[num] = CLR_WHITE "H";
		num++;
	}

	/* Super Heroism / berserk */
	if (p_ptr->shero)
	{
		letter[num] = CLR_RED "B";
		num++;
	}

	/* Blessed */
	if (p_ptr->blessed)
	{
		letter[num] = CLR_WHITE "B";
		num++;
	}

	/* Shield */
	if (p_ptr->shield)
	{
		letter[num] = CLR_WHITE "S";
		num++;
	}

	/* Oppose Acid */
	if (p_ptr->oppose_acid)
	{
		letter[num] = CLR_GREEN "A";
		num++;
	}

	/* Oppose Lightning */
	if (p_ptr->oppose_elec)
	{
		letter[num] = CLR_BLUE "E";
		num++;
	}

	/* Oppose Fire */
	if (p_ptr->oppose_fire)
	{
		letter[num] = CLR_RED "F";
		num++;
	}

	/* Oppose Cold */
	if (p_ptr->oppose_cold)
	{
		letter[num] = CLR_WHITE "C";
		num++;
	}

	/* Oppose Poison */
	if (p_ptr->oppose_pois)
	{
		letter[num] = CLR_GREEN "P";
		num++;
	}

	/* Word of Recall */
	if (p_ptr->word_recall)
	{
		letter[num] = CLR_WHITE "W";
		num++;
	}

	/* Confusing Hands */
	if (p_ptr->confusing)
	{
		letter[num] = CLR_RED "C";
		num++;
	}

	if (num)
	{
		/* Display the status bar if there are flags set */
		show_status_bar(letter, num);
	}
	else
		clear_status_bar();
}


/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	cptr p;

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

	/* Normal */
	else
	{
		p = player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5];

	}

	prt_field(p, COL_TITLE, ROW_TITLE);
}


/*
 * Prints level
 */
static void prt_level(void)
{
	if (p_ptr->lev >= p_ptr->max_lev)
	{
		put_fstr(COL_LEVEL, ROW_LEVEL, "LEVEL " CLR_L_GREEN "%6d", p_ptr->lev);
	}
	else
	{
		put_fstr(COL_LEVEL, ROW_LEVEL, "Level " CLR_YELLOW "%6d", p_ptr->lev);
	}
}


/*
 * Display the experience
 */
static void prt_exp(void)
{
	cptr attr;

	if (p_ptr->exp >= p_ptr->max_exp)
	{
		attr = CLR_L_GREEN;
	}
	else
	{
		attr = CLR_YELLOW;
	}

	if (toggle_xp)
	{
		if (p_ptr->lev >= PY_MAX_LEVEL)
		{
			put_fstr(COL_EXP, ROW_EXP, "NEED%s********", attr);
		}
		else
		{
			/* Print the amount of experience to go until the next level */
			put_fstr(COL_EXP, ROW_EXP, "NEED%s%8ld", attr,
						  (long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact /
								 100L) - (long)p_ptr->exp);
		}
	}
	else
	{
		/* Use the 'old' experience display */
		put_fstr(COL_EXP, ROW_EXP, "EXP %s%8ld", attr, (long)p_ptr->exp);
	}
}


/*
 * Prints current gold
 */
static void prt_gold(void)
{
	put_fstr(COL_GOLD, ROW_GOLD, "AU " CLR_L_GREEN "%9ld", (long)p_ptr->au);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
	put_fstr(COL_AC, ROW_AC, "Cur AC " CLR_L_GREEN "%5d",
				p_ptr->dis_ac + p_ptr->dis_to_a);
}


/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	cptr color;
	byte color_player;

#ifndef VARIABLE_PLAYER_GRAPH

	monster_race *r_ptr = &r_info[0];
	byte old_attr = r_ptr->x_attr;

#endif /* !VARIABLE_PLAYER_GRAPH */

	put_fstr(COL_MAXHP, ROW_MAXHP, "Max HP " CLR_L_GREEN "%5d", p_ptr->mhp);

	color = CLR_L_GREEN;
	
	if (p_ptr->chp >= p_ptr->mhp)
	{
		color = CLR_L_GREEN;
		color_player = TERM_WHITE;
	}
	else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
	{
		color = CLR_YELLOW;
		color_player = TERM_ORANGE;
	}
	else
	{
		color = CLR_RED;
		color_player = TERM_RED;
	}

	put_fstr(COL_CURHP, ROW_CURHP, "Cur HP %s%5d", color, p_ptr->chp);

#ifndef VARIABLE_PLAYER_GRAPH


	/* Hack - only change the colour if in character mode */
	if (r_ptr->x_char != '@') return;

	/* Only change colour if asked */
	if (!view_player_colour)
	{
		/* Normal colour is white */
		color_player = TERM_WHITE;
	}

	/* Redraw the player ? */
	if (old_attr != color_player)
	{
		/* Change the player colour */
		r_ptr->x_attr = color_player;

		/* Show the change */
		if (character_dungeon) lite_spot(p_ptr->px, p_ptr->py);
	}
#endif /* !VARIABLE_PLAYER_GRAPH */
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
	cptr color;


	/* Do not show mana unless it matters */
	if (!mp_ptr->spell_book) return;

	put_fstr(COL_MAXSP, ROW_MAXSP, "Max SP " CLR_L_GREEN "%5d", p_ptr->msp);

	color = CLR_L_GREEN;

	if (p_ptr->csp >= p_ptr->msp)
	{
		color = CLR_L_GREEN;
	}
	else if (p_ptr->csp > (p_ptr->msp * hitpoint_warn) / 10)
	{
		color = CLR_YELLOW;
	}
	else
	{
		color = CLR_RED;
	}

	/* Show mana */
	put_fstr(COL_CURSP, ROW_CURSP, "Cur SP %s%5d", color, p_ptr->csp);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	if (!p_ptr->depth)
	{
		if (p_ptr->place_num)
		{
			if (place[p_ptr->place_num].quest_num)
			{
				prtf(COL_DEPTH, Term->hgt - 1, "Quest");
			}
			else
			{
				prtf(COL_DEPTH, Term->hgt - 1, "%17s", place[p_ptr->place_num].name);
			}
		}
		else
		{
			prtf(COL_DEPTH, Term->hgt - 1, "Wilderness");
		}
	}
	else if (depth_in_feet)
	{
		prtf(COL_DEPTH, Term->hgt - 1, "%d ft", p_ptr->depth * 50);
	}
	else
	{
		prtf(COL_DEPTH, Term->hgt - 1, "Lev %d", p_ptr->depth);
	}
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		put_fstr(COL_HUNGRY, Term->hgt - 1, CLR_RED "Weak  ");
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		put_fstr(COL_HUNGRY, Term->hgt - 1, CLR_ORANGE "Weak  ");
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		put_fstr(COL_HUNGRY, Term->hgt - 1, CLR_YELLOW "Hungry");
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		put_fstr(COL_HUNGRY, Term->hgt - 1, "      ");
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		put_fstr(COL_HUNGRY, Term->hgt - 1, CLR_L_GREEN "Full  ");
	}

	/* Gorged */
	else
	{
		put_fstr(COL_HUNGRY, Term->hgt - 1, CLR_GREEN "Gorged");
	}
}


/*
 * Prints Blind status
 */
static void prt_blind(void)
{
	if (p_ptr->blind)
	{
		put_fstr(COL_BLIND, Term->hgt - 1, CLR_ORANGE "Blind");
	}
	else
	{
		put_fstr(COL_BLIND, Term->hgt - 1, "     ");
	}
}


/*
 * Prints Confusion status
 */
static void prt_confused(void)
{
	if (p_ptr->confused)
	{
		put_fstr(COL_CONFUSED, Term->hgt - 1, CLR_ORANGE "Confused");
	}
	else
	{
		put_fstr(COL_CONFUSED, Term->hgt - 1, "        ");
	}
}


/*
 * Prints Fear status
 */
static void prt_afraid(void)
{
	if (p_ptr->afraid)
	{
		put_fstr(COL_AFRAID, Term->hgt - 1, CLR_ORANGE "Afraid");
	}
	else
	{
		put_fstr(COL_AFRAID, Term->hgt - 1, "      ");
	}
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(void)
{
	if (p_ptr->poisoned)
	{
		put_fstr(COL_POISONED, Term->hgt - 1, CLR_ORANGE "Poisoned");
	}
	else
	{
		put_fstr(COL_POISONED, Term->hgt - 1, "        ");
	}
}


/*
 * Prints Searching, Resting, or 'count' status
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
	char text[16];

	/* Resting */
	if (p_ptr->resting)
	{
		int i;

		/* Start with "Rest" */
		strcpy(text, "R     ");

		/* Extensive (timed) rest */
		if (p_ptr->resting >= 1000)
		{
			i = p_ptr->resting / 100;
			text[5] = '0';
			text[4] = '0';
			text[3] = '0' + (i % 10);
			if (i >= 10)
			{
				i = i / 10;
				text[2] = '0' + (i % 10);
				if (i >= 10)
				{
					text[1] = '0' + (i / 10);
				}
			}
		}

		/* Long (timed) rest */
		else if (p_ptr->resting >= 100)
		{
			i = p_ptr->resting;
			text[5] = '0' + (i % 10);
			i = i / 10;
			text[4] = '0' + (i % 10);
			text[3] = '0' + (i / 10);
		}

		/* Medium (timed) rest */
		else if (p_ptr->resting >= 10)
		{
			i = p_ptr->resting;
			text[5] = '0' + (i % 10);
			text[4] = '0' + (i / 10);
		}

		/* Short (timed) rest */
		else if (p_ptr->resting > 0)
		{
			i = p_ptr->resting;
			text[5] = '0' + (i);
		}

		/* Rest until healed */
		else if (p_ptr->resting == -1)
		{
			text[1] = text[2] = text[3] = text[4] = text[5] = '*';
		}

		/* Rest until done */
		else if (p_ptr->resting == -2)
		{
			text[1] = text[2] = text[3] = text[4] = text[5] = '&';
		}
		
		/* Display the info (or blanks) */
		put_fstr(COL_STATE, Term->hgt - 1, text);
	}

	/* Repeating */
	else if (p_ptr->command_rep)
	{
		if (p_ptr->command_rep > 999)
		{
			put_fstr(COL_STATE, Term->hgt - 1, "C%3d00", p_ptr->command_rep / 100);
		}
		else
		{
			put_fstr(COL_STATE, Term->hgt - 1, "C  %3d", p_ptr->command_rep);
		}
	}

	/* Searching */
	else if (p_ptr->searching)
	{
		put_fstr(COL_STATE, Term->hgt - 1, "Search");
	}

	/* Nothing interesting */
	else
	{
		put_fstr(COL_STATE, Term->hgt - 1, "      ");
	}
}


/*
 * Prints the speed or paralysis of a character.
 *
 * Note that the strings must be exactly 10 chars long.
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed;

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		put_fstr(COL_SPEED, Term->hgt - 1, CLR_RED "Paralyzed!");
	}

	/* Fast */
	else if (i > 110)
	{
		if (i <= 110 + 9)
		{
			/* One digit */
			put_fstr(COL_SPEED, Term->hgt - 1, CLR_L_GREEN "Fast (+%d) ", (i - 110));
		}
		else if (i <= 110 + 99)
		{
			/* Two digits */
			put_fstr(COL_SPEED, Term->hgt - 1, CLR_L_GREEN "Fast (+%d)", (i - 110));
		}
		else
		{
			/* Hack - save space */
			put_fstr(COL_SPEED, Term->hgt - 1, CLR_L_GREEN "Fast (***)");
		}
	}

	/* Slow */
	else if (i < 110)
	{
		if (i >= 110 - 9)
		{
			/* One digit */
			put_fstr(COL_SPEED, Term->hgt - 1, CLR_L_UMBER "Slow (-%d) ", (110 - i));
		}
		else if (i >= 110 - 99)
		{
			/* Two digits */
			put_fstr(COL_SPEED, Term->hgt - 1, CLR_L_UMBER "Slow (-%d)", (110 - i));
		}
		else
		{
			/* Hack - save space */
			put_fstr(COL_SPEED, Term->hgt - 1, CLR_L_UMBER "Slow (***)");
		}
	}
	else
	{
		/* Nothing to print */
		put_fstr(COL_SPEED, Term->hgt - 1, CLR_L_UMBER "          ");
	}
}


static void prt_study(void)
{
	if (p_ptr->new_spells)
	{
		put_fstr(COL_STUDY, Term->hgt - 1, "Study");
	}
	else
	{
		put_fstr(COL_STUDY, Term->hgt - 1, "     ");
	}
}


static void prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)
	{
		put_fstr(COL_CUT, ROW_CUT, CLR_L_RED "Mortal wound");
	}
	else if (c > 200)
	{
		put_fstr(COL_CUT, ROW_CUT, CLR_RED "Deep gash   ");
	}
	else if (c > 100)
	{
		put_fstr(COL_CUT, ROW_CUT, CLR_RED "Severe cut  ");
	}
	else if (c > 50)
	{
		put_fstr(COL_CUT, ROW_CUT, CLR_ORANGE "Nasty cut   ");
	}
	else if (c > 25)
	{
		put_fstr(COL_CUT, ROW_CUT, CLR_ORANGE "Bad cut     ");
	}
	else if (c > 10)
	{
		put_fstr(COL_CUT, ROW_CUT, CLR_YELLOW "Light cut   ");
	}
	else if (c)
	{
		put_fstr(COL_CUT, ROW_CUT, CLR_YELLOW "Graze       ");
	}
	else
	{
		put_fstr(COL_CUT, ROW_CUT, "            ");
	}
}


static void prt_stun(void)
{
	int s = p_ptr->stun;

	if (s > 100)
	{
		put_fstr(COL_STUN, ROW_STUN, CLR_RED "Knocked out ");
	}
	else if (s > 50)
	{
		put_fstr(COL_STUN, ROW_STUN, CLR_ORANGE "Heavy stun  ");
	}
	else if (s)
	{
		put_fstr(COL_STUN, ROW_STUN, CLR_ORANGE "Stun        ");
	}
	else
	{
		put_fstr(COL_STUN, ROW_STUN, "            ");
	}
}



/*
 * Redraw the "monster health bar"	-DRS-
 * Rather extensive modifications by	-BEN-
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targeting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.
 *
 * Display the monster health bar (affectionately known as the
 * "health-o-meter").  Clear health bar if nothing is being tracked.
 * Auto-track current target monster when bored.  Note that the
 * health-bar stops tracking any monster that "disappears".
 */
static void health_redraw(void)
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
		put_fstr(COL_INFO, ROW_INFO, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		put_fstr(COL_INFO, ROW_INFO, "[----------]");
	}

	/* Tracking a dead monster ??? */
	else if (!m_list[p_ptr->health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		put_fstr(COL_INFO, ROW_INFO, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;

		monster_type *m_ptr = &m_list[p_ptr->health_who];

		/* Default to almost dead */
		cptr attr = CLR_RED;

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;

		/* Badly wounded */
		if (pct >= 10) attr = CLR_L_RED;

		/* Wounded */
		if (pct >= 25) attr = CLR_ORANGE;

		/* Somewhat Wounded */
		if (pct >= 60) attr = CLR_YELLOW;

		/* Healthy */
		if (pct >= 100) attr = CLR_L_GREEN;

		/* Afraid */
		if (m_ptr->monfear) attr = CLR_VIOLET;

		/* Asleep */
		if (m_ptr->csleep) attr = CLR_BLUE;

		/* Invulnerable */
		if (m_ptr->invulner) attr = CLR_WHITE;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		put_fstr(COL_INFO, ROW_INFO, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		put_fstr(COL_INFO + 1, ROW_INFO, "%s%.*s", attr, len, "**********");
	}
}



/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;

	/* Race and Class */
	prt_field(rp_ptr->title, COL_RACE, ROW_RACE);
	prt_field(cp_ptr->title, COL_CLASS, ROW_CLASS);

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();

	/* All Stats */
	for (i = 0; i < A_MAX; i++) prt_stat(i);

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

	/* Update inventory information */
	Term_write_list(p_ptr->inventory, LIST_INVEN);

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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

	/* Update equipment information */
	Term_write_equipment();

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_PLAYER))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display player */
		display_player(DISPLAY_PLAYER_STANDARD);

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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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
			cptr attr = color_seq[message_color((s16b)i)];

			/* Dump the message on the appropriate line */
			put_fstr(0, (h - 1) - i, "%s%s", attr, message_str((s16b)i));

			/* Cursor */
			(void)Term_locate(&x, &y);

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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OVERHEAD))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* No offset from player */
		cx = 0;
		cy = 0;

		/* Redraw map */
		display_map(&cx, &cy);

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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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
 * Hack -- display visible monster list in sub-windows
 */
static void fix_visible(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_VISIBLE))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster list */
		display_visible();

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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
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
	int i, j, k, levels;
	int num_allowed, num_known;

	const magic_type *s_ptr;
	int use_realm1 = p_ptr->realm1 - 1;
	int use_realm2 = p_ptr->realm2 - 1;
	int which;

	/* Save the current number of spells to learn */
	s16b old_spells = p_ptr->new_spells;

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
	num_allowed =
		(adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 50);


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < PY_MAX_SPELLS; j++)
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
	for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
	{
		/* Efficiency -- all done */
		if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break;

		/* Access the spell */
		j = p_ptr->spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;


		/* Get the spell */
		if (j < 32)
			s_ptr = &mp_ptr->info[use_realm1][j];
		else
			s_ptr = &mp_ptr->info[use_realm2][j % 32];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

		/* Is it known? */
		if ((j < 32) ?
			(p_ptr->spell_learned1 & (1L << j)) :
			(p_ptr->spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				p_ptr->spell_forgotten1 |= (1L << j);
				p_ptr->spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				p_ptr->spell_forgotten2 |= (1L << (j - 32));
				p_ptr->spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msgf("You have forgotten the %s of %s.", p,
					   spell_names[which][j % 32]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Efficiency -- all done */
		if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break;

		/* Get the (i+1)th spell learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
		if ((j < 32) ?
			(p_ptr->spell_learned1 & (1L << j)) :
			(p_ptr->spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				p_ptr->spell_forgotten1 |= (1L << j);
				p_ptr->spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				p_ptr->spell_forgotten2 |= (1L << (j - 32));
				p_ptr->spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msgf("You have forgotten the %s of %s.", p,
					   spell_names[which][j % 32]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Efficiency -- all done */
		if (!p_ptr->spell_forgotten1 && !p_ptr->spell_forgotten2) break;

		/* Get the next spell we learned */
		j = p_ptr->spell_order[i];

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
			(p_ptr->spell_forgotten1 & (1L << j)) :
			(p_ptr->spell_forgotten2 & (1L << (j - 32))))
		{
			/* No longer forgotten - known once more */
			if (j < 32)
			{
				p_ptr->spell_forgotten1 &= ~(1L << j);
				p_ptr->spell_learned1 |= (1L << j);
				which = use_realm1;
			}
			else
			{
				p_ptr->spell_forgotten2 &= ~(1L << (j - 32));
				p_ptr->spell_learned2 |= (1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msgf("You have remembered the %s of %s.",
					   p, spell_names[which][j % 32]);

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
			(p_ptr->spell_learned1 & (1L << j)) :
			(p_ptr->spell_learned2 & (1L << (j - 32))))
		{
			continue;
		}

		/* Count it */
		k++;
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

	/* Spell count changed */
	if (old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
			msgf("You can learn %d more %s%s.",
					   p_ptr->new_spells, p,
					   (p_ptr->new_spells != 1) ? "s" : "");
		}

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
	int msp, levels, cur_wgt, max_wgt;

	object_type *o_ptr;

	bool old_cumber_glove = p_ptr->cumber_glove;
	bool old_cumber_armor = p_ptr->cumber_armor;


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
	msp = adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 25;

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
		o_ptr = &p_ptr->equipment[EQUIP_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx &&
			!(f2 & (TR2_FREE_ACT)) && !((f1 & (TR1_DEX)) && (o_ptr->pval > 0)))
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
	cur_wgt += p_ptr->equipment[EQUIP_BODY].weight;
	cur_wgt += p_ptr->equipment[EQUIP_HEAD].weight;
	cur_wgt += p_ptr->equipment[EQUIP_ARM].weight;
	cur_wgt += p_ptr->equipment[EQUIP_OUTER].weight;
	cur_wgt += p_ptr->equipment[EQUIP_HANDS].weight;
	cur_wgt += p_ptr->equipment[EQUIP_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana by a percentage.  -LM- */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/*
		 * Subtract a percentage of maximum mana.
		 * The addition of one is to make sure the
		 * mana total is decreased by some amount.
		 */
		switch (p_ptr->pclass)
		{
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
			{
				/*
				 * For these classes, mana is halved if armour
				 * is 30 pounds over their weight limit.
				 */
				msp -= msp * (cur_wgt - max_wgt) / 600 + 1;
				break;
			}

			case CLASS_PRIEST:
			case CLASS_MINDCRAFTER:
			{
				/* Mana halved if armour is 40 pounds over weight limit. */
				msp -= msp * (cur_wgt - max_wgt) / 800 + 1;
				break;
			}

			case CLASS_ROGUE:
			case CLASS_RANGER:
			case CLASS_MONK:
			{
				/* Mana halved if armour is 50 pounds over weight limit. */
				msp -= msp * (cur_wgt - max_wgt) / 1000 + 1;
				break;
			}

			case CLASS_PALADIN:
			case CLASS_CHAOS_WARRIOR:
			case CLASS_WARRIOR_MAGE:
			{
				/* Mana halved if armour is 60 pounds over weight limit. */
				msp -= msp * (cur_wgt - max_wgt) / 1200 + 1;
				break;
			}

			default:
			{
				/* For new classes, but not yet added to this formula. */
				msp -= msp * (cur_wgt - max_wgt) / 800 + 1;
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
	if (old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
			msgf("Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msgf("Your hands feel more suitable for spellcasting.");
		}
	}


	/* Take note when "armor state" changes */
	if (old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (p_ptr->cumber_armor)
		{
			msgf("The weight of your armor encumbers your movement.");
		}
		else
		{
			msgf("You feel able to move more freely.");
		}
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
	mhp = p_ptr->player_hp[p_ptr->lev - 1] + (bonus * p_ptr->lev / 2);

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
 */
static void calc_torch(void)
{
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3;

	s16b old_lite = p_ptr->cur_lite;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Examine actual lites */
		if ((i == EQUIP_LITE) && (o_ptr->k_idx) && (o_ptr->tval == TV_LITE))
		{
			/* Artifact Lites provide permanent, bright, lite */
			if (o_ptr->flags3 & TR3_INSTA_ART)
			{
				p_ptr->cur_lite += 3;
				continue;
			}

			/* Lanterns (with fuel) provide more lite */
			if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->timeout > 0))
			{
				p_ptr->cur_lite += 2;
				continue;
			}

			/* Torches (with fuel) provide some lite */
			if ((o_ptr->sval == SV_LITE_TORCH) && (o_ptr->timeout > 0))
			{
				p_ptr->cur_lite += 1;
				continue;
			}
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

	/*
	 * Check if the player doesn't have a lite source,
	 * but does glow as an intrinsic.
	 */
	if (p_ptr->cur_lite == 0 && p_ptr->lite) p_ptr->cur_lite = 1;

	/*
	 * Hack - blindness gives a torch radius of zero.
	 * This speeds up the map_info() function.
	 */
	if (p_ptr->blind)
	{
		/* No light */
		p_ptr->cur_lite = 0;
	}

	/* Notice changes in the "lite radius" */
	if (old_lite != p_ptr->cur_lite)
	{
		/* Update the view */
		p_ptr->update |= (PU_VIEW);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Update the monster lighting */
		p_ptr->update |= (PU_MON_LITE);

		/* Redraw the map */
		p_ptr->redraw |= (PR_MAP);
	}
}

/*
 * Recalculate the inventory and equipment weight
 */
static void calc_weight(void)
{
	object_type *o_ptr;

	int i;

	/* No weight yet */
	p_ptr->total_weight = 0;

	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Increase the weight */
		p_ptr->total_weight += (o_ptr->number * o_ptr->weight);
	}
	OBJ_ITT_END;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Need valid items */
		if (!o_ptr->k_idx) continue;

		/* Increase the weight */
		p_ptr->total_weight += o_ptr->weight;
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
static sint add_special_melee_skill(byte pclass, object_type *o_ptr)
{
	int add_skill = 0;
	s16b weight = o_ptr->weight;

	switch (pclass)
	{
		case CLASS_WARRIOR:
		{
			/*
			 * Warrior.
			 * Can use 15 lb weapons without penalty at level 1,
			 * and 45 lb weapons without penalty at 50th level.
			 */
			add_skill = 25 + p_ptr->lev - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;
			break;
		}

		case CLASS_MAGE:
		case CLASS_HIGH_MAGE:
		{
			/*
			 * Mage/High Mage.
			 * Can use 6 lb weapons without penalty at level 1,
			 * and 16 lb weapons without penalty at 50th level.
			 */
			add_skill = 20 + (2 * p_ptr->lev / 3) - (weight / 3);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -30) add_skill = -30;
			break;
		}

		case CLASS_PRIEST:
		{
			/*
			 * Priest.
			 * Can use 12 lb weapons without penalty at level 1,
			 * and 22 lb weapons without penalty at 50th level.
			 */
			add_skill = 30 + (1 * p_ptr->lev / 2) - (weight / 4);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -25) add_skill = -25;
			break;
		}

		case CLASS_ROGUE:
		{
			/*
			 * Rogue.
			 * Can use 10 lb weapons without penalty at level 1,
			 * and 20 lb weapons without penalty at 50th level.
			 * Can get a bonus for using light weapons.
			 */
			if (!o_ptr->k_idx)
			{
				add_skill = 0;
			}
			else
			{
				add_skill = 33 + (2 * p_ptr->lev / 3) - (weight / 3);
				if (add_skill > 0) add_skill = add_skill / 2;
				if (add_skill > 15) add_skill = 15;
				if (add_skill < -25) add_skill = -25;
			}
			break;
		}

		case CLASS_RANGER:
		{
			/*
			 * Ranger.
			 * Can use 12 lb weapons without penalty at level 1,
			 * and 25 lb weapons without penalty at 50th level.
			 */
			add_skill = 25 + (1 * p_ptr->lev / 2) - (weight / 5);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -20) add_skill = -20;
			break;
		}

		case CLASS_PALADIN:
		case CLASS_CHAOS_WARRIOR:
		case CLASS_WARRIOR_MAGE:
		{
			/*
			 * Paladin/Chaos warrior/Warrior mage.
			 * Can use 15 lb weapons without penalty at level 1,
			 * and 45 lb weapons without penalty at 50th level.
			 */
			add_skill = 25 + p_ptr->lev - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;
			break;
		}

		case CLASS_MONK:
		{
			/*
			 * Monk.
			 * Can use 5 lb weapons without penalty at level 1,
			 * and slightly over 12 lb weapons without penalty at 50th level.
			 * Much prefers to use hands and feet.
			 */
			if (!o_ptr->k_idx)
			{
				add_skill = 14 + (p_ptr->lev);
			}
			else
			{
				add_skill = 16 + (p_ptr->lev / 2) - (weight / 3);
				if (add_skill > 0) add_skill = 0;
				if (add_skill < -30) add_skill = -30;
			}
			break;
		}

		case CLASS_MINDCRAFTER:
		{
			/*
			 * Mindcrafter.
			 * Can use 6 lb weapons without penalty at level 1,
			 * and 16 lb weapons without penalty at 50th level.
			 */
			add_skill = 20 + (2 * p_ptr->lev / 3) - (weight / 3);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -30) add_skill = -30;
			break;
		}
	}

	return (add_skill);
}


/*
 * Calculate all class and race-based bonuses and penalties to missile skill
 *			 -LM-
 */
static sint add_special_missile_skill(byte pclass)
{
	int add_skill = 0;

	switch (pclass)
	{
		case CLASS_ROGUE:
		{
			/* Rogues are good with slings. */
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				add_skill = 3 + p_ptr->lev / 4;
			}
			break;
		}

		case CLASS_RANGER:
		{
			/*
			 * Rangers have a high missile skill,
			 * but they are not supposed to
			 * be great with xbows and slings.
			 */
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				add_skill = 0 - p_ptr->lev / 7;
			}
			if (p_ptr->ammo_tval == TV_BOLT)
			{
				add_skill = 0 - p_ptr->lev / 7;
			}
			break;
		}

		case CLASS_MONK:
		{
			/* Monks get a small bonus with slings. */
			if (p_ptr->ammo_tval == TV_SHOT)
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
static void calc_bonuses(void)
{
	int i, j, hold;
	int old_speed;
	int old_telepathy;
	int old_see_inv;
	int old_dis_ac;
	int old_dis_to_a;
	int extra_blows;
	int extra_shots;
	object_type *o_ptr;
	u32b f1, f2, f3;

	bool old_heavy_wield = p_ptr->heavy_wield;
	bool old_heavy_shoot = p_ptr->heavy_shoot;
	bool old_icky_wield = p_ptr->icky_wield;
	bool old_monk_armour = p_ptr->monk_armour_stat;

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

	/* Calculate monk armour status */
	if (p_ptr->pclass == CLASS_MONK)
	{
		u16b monk_arm_wgt = 0;

		/* Weigh the armor */
		monk_arm_wgt += p_ptr->equipment[EQUIP_BODY].weight;
		monk_arm_wgt += p_ptr->equipment[EQUIP_HEAD].weight;
		monk_arm_wgt += p_ptr->equipment[EQUIP_ARM].weight;
		monk_arm_wgt += p_ptr->equipment[EQUIP_OUTER].weight;
		monk_arm_wgt += p_ptr->equipment[EQUIP_HANDS].weight;
		monk_arm_wgt += p_ptr->equipment[EQUIP_FEET].weight;

		if (monk_arm_wgt > (100 + (p_ptr->lev * 4)))
		{
			/* Burdened */
			p_ptr->monk_armour_stat = TRUE;
		}
		else
		{
			/* Not burdened */
			p_ptr->monk_armour_stat = FALSE;
		}
	}

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] = 0;


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

	/* Reset the "ammo" tval */
	p_ptr->ammo_tval = 0;

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
	p_ptr->resist_confu = FALSE;
	p_ptr->resist_sound = FALSE;
	p_ptr->resist_lite = FALSE;
	p_ptr->resist_dark = FALSE;
	p_ptr->resist_chaos = FALSE;
	p_ptr->resist_disen = FALSE;
	p_ptr->resist_shard = FALSE;
	p_ptr->resist_nexus = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_nethr = FALSE;
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

	/* Base skill -- sensing ability */
	p_ptr->skill_sns = rp_ptr->r_sns + cp_ptr->c_sns;

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

	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR:
		{
			if (p_ptr->lev > 29) p_ptr->resist_fear = TRUE;
			break;
		}
		case CLASS_PALADIN:
		{
			if (p_ptr->lev > 39) p_ptr->resist_fear = TRUE;
			break;
		}
		case CLASS_CHAOS_WARRIOR:
		{
			if (p_ptr->lev > 29) p_ptr->resist_chaos = TRUE;
			if (p_ptr->lev > 39) p_ptr->resist_fear = TRUE;
			break;
		}
		case CLASS_MINDCRAFTER:
		{
			if (p_ptr->lev > 9) p_ptr->resist_fear = TRUE;
			if (p_ptr->lev > 19) p_ptr->sustain_wis = TRUE;
			if (p_ptr->lev > 29) p_ptr->resist_confu = TRUE;
			if (p_ptr->lev > 39) p_ptr->telepathy = TRUE;
			break;
		}
		case CLASS_MONK:
		{
			/* Unencumbered Monks become faster every 10 levels */
			if (!p_ptr->monk_armour_stat)
			{
#ifndef MONK_HACK
				if (!((p_ptr->prace == RACE_KLACKON) ||
					  (p_ptr->prace == RACE_SPRITE)))
#endif /* MONK_HACK */
				{
					p_ptr->pspeed += (p_ptr->lev) / 10;
				}

				/* Free action if unencumbered at level 25 */
				if (p_ptr->lev > 24) p_ptr->free_act = TRUE;
			}

			break;
		}
	}

	/***** Races ****/
	switch (p_ptr->prace)
	{
		case RACE_ELF:
		{
			p_ptr->resist_lite = TRUE;
			break;
		}
		case RACE_HOBBIT:
		{
			p_ptr->sustain_dex = TRUE;
			break;
		}
		case RACE_GNOME:
		{
			p_ptr->free_act = TRUE;
			break;
		}
		case RACE_DWARF:
		{
			p_ptr->resist_blind = TRUE;
			break;
		}
		case RACE_HALF_ORC:
		{
			p_ptr->resist_dark = TRUE;
			break;
		}
		case RACE_HALF_TROLL:
		{
			p_ptr->sustain_str = TRUE;

			if (p_ptr->lev > 14)
			{
				/* High level trolls heal fast... */
				p_ptr->regenerate = TRUE;

				if (p_ptr->pclass == CLASS_WARRIOR)
				{
					p_ptr->slow_digest = TRUE;
					/*
					 * Let's not make Regeneration
					 * a disadvantage for the poor warriors who can
					 * never learn a spell that satisfies hunger (actually
					 * neither can rogues, but half-trolls are not
					 * supposed to play rogues)
					 */
				}
			}
			break;
		}
		case RACE_AMBERITE:
		{
			p_ptr->sustain_con = TRUE;

			/* Amberites heal fast... */
			p_ptr->regenerate = TRUE;
			break;
		}
		case RACE_HIGH_ELF:
		{
			p_ptr->resist_lite = TRUE;
			p_ptr->see_inv = TRUE;
			break;
		}
		case RACE_BARBARIAN:
		{
			p_ptr->resist_fear = TRUE;
			break;
		}
		case RACE_HALF_OGRE:
		{
			p_ptr->resist_dark = TRUE;
			p_ptr->sustain_str = TRUE;
			break;
		}
		case RACE_HALF_GIANT:
		{
			p_ptr->sustain_str = TRUE;
			p_ptr->resist_shard = TRUE;
			break;
		}
		case RACE_HALF_TITAN:
		{
			p_ptr->resist_chaos = TRUE;
			break;
		}
		case RACE_CYCLOPS:
		{
			p_ptr->resist_sound = TRUE;
			break;
		}
		case RACE_YEEK:
		{
			p_ptr->resist_acid = TRUE;
			if (p_ptr->lev > 19) p_ptr->immune_acid = TRUE;
			break;
		}
		case RACE_KLACKON:
		{
			p_ptr->resist_confu = TRUE;
			p_ptr->resist_acid = TRUE;

			/* Klackons become faster */
			p_ptr->pspeed += (p_ptr->lev) / 10;
			break;
		}
		case RACE_KOBOLD:
		{
			p_ptr->resist_pois = TRUE;
			break;
		}
		case RACE_NIBELUNG:
		{
			p_ptr->resist_disen = TRUE;
			p_ptr->resist_dark = TRUE;
			break;
		}
		case RACE_DARK_ELF:
		{
			p_ptr->resist_dark = TRUE;
			if (p_ptr->lev > 19) p_ptr->see_inv = TRUE;
			break;
		}
		case RACE_DRACONIAN:
		{
			p_ptr->ffall = TRUE;
			if (p_ptr->lev > 4) p_ptr->resist_fire = TRUE;
			if (p_ptr->lev > 9) p_ptr->resist_cold = TRUE;
			if (p_ptr->lev > 14) p_ptr->resist_acid = TRUE;
			if (p_ptr->lev > 19) p_ptr->resist_elec = TRUE;
			if (p_ptr->lev > 34) p_ptr->resist_pois = TRUE;
			break;
		}
		case RACE_MIND_FLAYER:
		{
			p_ptr->sustain_int = TRUE;
			p_ptr->sustain_wis = TRUE;
			if (p_ptr->lev > 14) p_ptr->see_inv = TRUE;
			if (p_ptr->lev > 29) p_ptr->telepathy = TRUE;
			break;
		}
		case RACE_IMP:
		{
			p_ptr->resist_fire = TRUE;
			if (p_ptr->lev > 9) p_ptr->see_inv = TRUE;
			break;
		}
		case RACE_GOLEM:
		{
			p_ptr->slow_digest = TRUE;
			p_ptr->free_act = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->resist_pois = TRUE;
			if (p_ptr->lev > 34) p_ptr->hold_life = TRUE;
			break;
		}
		case RACE_SKELETON:
		{
			p_ptr->resist_shard = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->resist_pois = TRUE;
			if (p_ptr->lev > 9) p_ptr->resist_cold = TRUE;
			break;
		}
		case RACE_ZOMBIE:
		{
			p_ptr->resist_nethr = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->slow_digest = TRUE;
			if (p_ptr->lev > 4) p_ptr->resist_cold = TRUE;
			break;
		}
		case RACE_VAMPIRE:
		{
			p_ptr->resist_dark = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->resist_nethr = TRUE;
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->lite = TRUE;
			break;
		}
		case RACE_SPECTRE:
		{
			p_ptr->resist_nethr = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->slow_digest = TRUE;
			p_ptr->resist_cold = TRUE;
			p_ptr->pass_wall = TRUE;
			if (p_ptr->lev > 34) p_ptr->telepathy = TRUE;
			break;
		}
		case RACE_SPRITE:
		{
			p_ptr->ffall = TRUE;
			p_ptr->resist_lite = TRUE;

			/* Sprites become faster */
			p_ptr->pspeed += p_ptr->lev / 10;
			break;
		}
		case RACE_BEASTMAN:
		{
			p_ptr->resist_confu = TRUE;
			p_ptr->resist_sound = TRUE;
			break;
		}
		case RACE_GHOUL:
		{
			if (p_ptr->lev > 9) p_ptr->resist_dark = TRUE;
			p_ptr->hold_life = TRUE;
			if (p_ptr->lev > 19) p_ptr->resist_nethr = TRUE;
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_pois = TRUE;
			break;
		}
	}


	/* Effects of constantly acting mutations */
	if (p_ptr->muta3)
	{
		mutation_effect();
	}


	/* Remove flags that were not in Moria */
	if (ironman_moria)
	{
		p_ptr->reflect = FALSE;
		p_ptr->resist_pois = FALSE;
		p_ptr->resist_fear = FALSE;
		p_ptr->resist_lite = FALSE;
		p_ptr->resist_dark = FALSE;
		p_ptr->resist_confu = FALSE;
		p_ptr->resist_sound = FALSE;
		p_ptr->resist_shard = FALSE;
		p_ptr->resist_nethr = FALSE;
		p_ptr->resist_nexus = FALSE;
		p_ptr->resist_chaos = FALSE;
		p_ptr->resist_disen = FALSE;

		p_ptr->sh_fire = FALSE;
		p_ptr->sh_elec = FALSE;
		p_ptr->anti_tele = FALSE;
		p_ptr->anti_magic = FALSE;

		p_ptr->lite = FALSE;
	}

	/* Scan the usable inventory */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

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

		/* Affect sensing ability (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skill_sns += (o_ptr->pval * 5);

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
		if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_BLESSED)) p_ptr->bless_blade = TRUE;
		if (f3 & (TR3_XTRA_MIGHT)) p_ptr->xtra_might = TRUE;
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
		if (f3 & (TR3_LITE)) p_ptr->lite = TRUE;
		if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
		if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
		if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE)) p_ptr->immune_fire = TRUE;
		if (f2 & (TR2_IM_ACID)) p_ptr->immune_acid = TRUE;
		if (f2 & (TR2_IM_COLD)) p_ptr->immune_cold = TRUE;
		if (f2 & (TR2_IM_ELEC)) p_ptr->immune_elec = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID)) p_ptr->resist_acid = TRUE;
		if (f2 & (TR2_RES_ELEC)) p_ptr->resist_elec = TRUE;
		if (f2 & (TR2_RES_FIRE)) p_ptr->resist_fire = TRUE;
		if (f2 & (TR2_RES_COLD)) p_ptr->resist_cold = TRUE;
		if (f2 & (TR2_RES_POIS)) p_ptr->resist_pois = TRUE;
		if (f2 & (TR2_RES_FEAR)) p_ptr->resist_fear = TRUE;
		if (f2 & (TR2_RES_CONF)) p_ptr->resist_confu = TRUE;
		if (f2 & (TR2_RES_SOUND)) p_ptr->resist_sound = TRUE;
		if (f2 & (TR2_RES_LITE)) p_ptr->resist_lite = TRUE;
		if (f2 & (TR2_RES_DARK)) p_ptr->resist_dark = TRUE;
		if (f2 & (TR2_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN)) p_ptr->resist_disen = TRUE;
		if (f2 & (TR2_RES_SHARDS)) p_ptr->resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
		if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_NETHER)) p_ptr->resist_nethr = TRUE;

		if (f2 & (TR2_REFLECT)) p_ptr->reflect = TRUE;
		if (f3 & (TR3_SH_FIRE)) p_ptr->sh_fire = TRUE;
		if (f3 & (TR3_SH_ELEC)) p_ptr->sh_elec = TRUE;
		if (f3 & (TR3_NO_MAGIC)) p_ptr->anti_magic = TRUE;
		if (f3 & (TR3_NO_TELE)) p_ptr->anti_tele = TRUE;

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
		if (i == EQUIP_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == EQUIP_BOW) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h += o_ptr->to_h;
		p_ptr->to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr)) p_ptr->dis_to_d += o_ptr->to_d;
	}

	/* Monks get extra ac for armour _not worn_ */
	if ((p_ptr->pclass == CLASS_MONK) && (!p_ptr->monk_armour_stat))
	{
		if (!(p_ptr->equipment[EQUIP_BODY].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev * 3) / 2;
			p_ptr->dis_to_a += (p_ptr->lev * 3) / 2;
		}
		if (!(p_ptr->equipment[EQUIP_OUTER].k_idx) && (p_ptr->lev > 15))
		{
			p_ptr->to_a += ((p_ptr->lev - 13) / 3);
			p_ptr->dis_to_a += ((p_ptr->lev - 13) / 3);
		}
		if (!(p_ptr->equipment[EQUIP_ARM].k_idx) && (p_ptr->lev > 10))
		{
			p_ptr->to_a += ((p_ptr->lev - 8) / 3);
			p_ptr->dis_to_a += ((p_ptr->lev - 8) / 3);
		}
		if (!(p_ptr->equipment[EQUIP_HEAD].k_idx) && (p_ptr->lev > 4))
		{
			p_ptr->to_a += (p_ptr->lev - 2) / 3;
			p_ptr->dis_to_a += (p_ptr->lev - 2) / 3;
		}
		if (!(p_ptr->equipment[EQUIP_HANDS].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev / 2);
			p_ptr->dis_to_a += (p_ptr->lev / 2);
		}
		if (!(p_ptr->equipment[EQUIP_FEET].k_idx))
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
	for (i = 0; i < A_MAX; i++)
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
            int floor = 8 + 2 * p_ptr->lev;
            if (floor <= 18)
                floor *= 10;
            else
                floor += 180-18;

			/* 10 to 18/90 charisma, guaranteed, based on level */
			if (use < floor)
			{
				use = floor;
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


        if (use < 400)
            ind = use / 10 - 3;
        else
            ind = 37;

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
	if (j > i / 2) p_ptr->pspeed -= ((j - (i / 2)) / (i / 10));

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
	o_ptr = &p_ptr->equipment[EQUIP_BOW];


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
				p_ptr->ammo_tval = TV_SHOT;
				p_ptr->ammo_mult = 2;
				p_ptr->bow_energy = 50;
				break;
			}

			case SV_SHORT_BOW:
			{
				p_ptr->ammo_tval = TV_ARROW;
				p_ptr->ammo_mult = 2;
				p_ptr->bow_energy = 100;
				break;
			}
			case SV_LONG_BOW:
			{
				p_ptr->ammo_tval = TV_ARROW;

				if (p_ptr->stat_use[A_STR] >= 16)
				{
					p_ptr->ammo_mult = 3;
				}
				else
				{
					/* weak players cannot use a longbow well */
					p_ptr->ammo_mult = 2;
				}

				p_ptr->bow_energy = 100;
				break;
			}

			case SV_LIGHT_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;
				p_ptr->ammo_mult = 4;
				p_ptr->bow_energy = 120;
				break;
			}

			case SV_HEAVY_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;

				p_ptr->ammo_mult = 5;
				if (p_ptr->stat_use[A_DEX] >= 16)
				{
					p_ptr->bow_energy = 150;
				}
				else
				{
					/* players with low dex will take longer to load */
					p_ptr->bow_energy = 200;
				}
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
				(p_ptr->ammo_tval == TV_ARROW))
			{
				/* Extra shot at level 15 */
				if (p_ptr->lev >= 15) p_ptr->num_fire++;

				/* Extra shot at level 30 */
				if (p_ptr->lev >= 30) p_ptr->num_fire++;

				/* Extra shot at level 45 */
				if (p_ptr->lev >= 45) p_ptr->num_fire++;
			}

			/* Hack -- Rangers can use XBows as well */
			if ((p_ptr->pclass == CLASS_RANGER) &&
				(p_ptr->ammo_tval == TV_BOLT))
			{
				/* Extra shot at level 30 */
				if (p_ptr->lev >= 30) p_ptr->num_fire++;
			}

			/* Hack -- Rogues love Slings */
			if ((p_ptr->pclass == CLASS_ROGUE) && (p_ptr->ammo_tval == TV_SHOT))
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
				(p_ptr->ammo_tval <= TV_BOLT) && (p_ptr->ammo_tval >= TV_SHOT))
			{
				/* Extra shot at level 40 */
				if (p_ptr->lev >= 40) p_ptr->num_fire++;

			}
		}
	}
	/* Add all class and race-specific adjustments to missile Skill. -LM- */
	p_ptr->skill_thb += add_special_missile_skill(p_ptr->pclass);

	/* Examine the "main weapon" */
	o_ptr = &p_ptr->equipment[EQUIP_WIELD];

	/* Add all other class-specific adjustments to melee Skill. -LM- */
	p_ptr->skill_thn += add_special_melee_skill(p_ptr->pclass, o_ptr);

	/* Assume okay */
	p_ptr->icky_wield = FALSE;

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
		p_ptr->to_h -= (p_ptr->lev / 5);
		p_ptr->to_d -= (p_ptr->lev / 5);

		/* Reduce the mental bonuses */
		p_ptr->dis_to_h -= (p_ptr->lev / 5);
		p_ptr->dis_to_d -= (p_ptr->lev / 5);

		/* Icky weapon */
		p_ptr->icky_wield = TRUE;
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

	/* Affect Skill -- sensing ability (Level, by Class) */
	p_ptr->skill_sns += (cp_ptr->x_sns * p_ptr->lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->skill_fos += (cp_ptr->x_fos * p_ptr->lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 50);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 50);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 50);

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Apply Skill -- Extract noise from stealth */
	p_ptr->noise = (1L << (30 - p_ptr->skill_stl));

	if ((p_ptr->anti_magic) && (p_ptr->skill_sav < 95)) p_ptr->skill_sav = 95;

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* Are we using a weapon? */
	if (o_ptr->k_idx)
	{
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
		else
		{
			int str_index, dex_index;

			int effective_weight = 0, mul = 6;

			int skill;

			/* Enforce a minimum weight of three pounds. */
			effective_weight = (o_ptr->weight < 30 ? 30 : o_ptr->weight);

			/* Compare strength and weapon weight. */
			str_index = mul * adj_str_blow[p_ptr->stat_ind[A_STR]] /
				effective_weight;

			/* Maximal value */
			if (str_index > 11) str_index = 11;

			/* Index by dexterity */
			dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

			/* Maximal value */
			if (dex_index > 11) dex_index = 11;


			/* Use the blows table */
			p_ptr->num_blow = blows_table[str_index][dex_index];

			/* Get weapon skill */
			skill = p_ptr->skill_thn + (p_ptr->to_h * BTH_PLUS_ADJ);

			/* Require high skill to get large number of blows */
			if ((skill < 100) && (p_ptr->num_blow > 3))
			{
				p_ptr->num_blow = 3;
			}
			if ((skill < 150) && (p_ptr->num_blow > 4))
			{
				p_ptr->num_blow = 4;
			}
			if ((skill < 200) && (p_ptr->num_blow > 5))
			{
				p_ptr->num_blow = 5;
			}

			/* Paranoia - require at least one blow */
			if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;

			/* Add in extra blows */
			p_ptr->num_blow += extra_blows;


			/* Boost digging skill by weapon weight */
			p_ptr->skill_dig += (o_ptr->weight / 10);
		}
	}

	/* No weapon */
	else
	{
		/* Different calculation for monks with empty hands */
		if (p_ptr->pclass == CLASS_MONK)
		{
			p_ptr->num_blow = 2;

			if (p_ptr->lev > 9) p_ptr->num_blow++;
			if (p_ptr->lev > 14) p_ptr->num_blow++;
			if (p_ptr->lev > 24) p_ptr->num_blow++;
			if (p_ptr->lev > 34) p_ptr->num_blow++;
			if (p_ptr->lev > 44) p_ptr->num_blow++;
			if (p_ptr->lev > 49) p_ptr->num_blow++;

			if (p_ptr->monk_armour_stat)
			{
				p_ptr->num_blow /= 2;
			}
			else
			{
				p_ptr->to_h += (p_ptr->lev / 3);
				p_ptr->to_d += (p_ptr->lev / 3);

				p_ptr->dis_to_h += (p_ptr->lev / 3);
				p_ptr->dis_to_d += (p_ptr->lev / 3);
			}

			p_ptr->num_blow += extra_blows;
		}
		else
		{
			/* Everyone gets two blows if not wielding a weapon. -LM- */
			p_ptr->num_blow = 2;
		}
	}

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "heavy bow" changes */
	if (old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msgf("You have trouble wielding such a heavy bow.");
		}
		else if (p_ptr->equipment[EQUIP_BOW].k_idx)
		{
			msgf("You have no trouble wielding your bow.");
		}
		else
		{
			msgf("You feel relieved to put down your heavy bow.");
		}
	}


	/* Take note when "heavy weapon" changes */
	if (old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
			msgf("You have trouble wielding such a heavy weapon.");
		}
		else if (p_ptr->equipment[EQUIP_WIELD].k_idx)
		{
			msgf("You have no trouble wielding your weapon.");
		}
		else
		{
			msgf("You feel relieved to put down your heavy weapon.");
		}
	}


	/* Take note when "illegal weapon" changes */
	if (old_icky_wield != p_ptr->icky_wield)
	{
		/* Message */
		if (p_ptr->icky_wield)
		{
			msgf("You do not feel comfortable with your weapon.");
		}
		else if (p_ptr->equipment[EQUIP_WIELD].k_idx)
		{
			msgf("You feel comfortable with your weapon.");
		}
		else
		{
			msgf("You feel more comfortable after removing your weapon.");
		}
	}

	if (p_ptr->pclass == CLASS_MONK &&
		(p_ptr->monk_armour_stat != old_monk_armour))
	{
		if (p_ptr->monk_armour_stat)
		{
			msgf("The weight of your armor disrupts your balance.");
		}
		else
			msgf("You regain your balance.");
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

	if (p_ptr->update & (PU_WEIGHT))
	{
		p_ptr->update &= ~(PU_WEIGHT);
		calc_weight();
	}

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

	if (p_ptr->update & (PU_VIEW))
	{
		p_ptr->update &= ~(PU_VIEW);
		update_view();
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

	if ((p_ptr->update & (PU_MON_LITE)) && monster_light)
	{
		p_ptr->update &= ~(PU_MON_LITE);
		update_mon_lite();
	}
}


/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
	int i;

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
		message_flush();
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
		print_equippy();		/* To draw / delete equippy chars */
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		prt_field(rp_ptr->title, COL_RACE, ROW_RACE);
		prt_field(cp_ptr->title, COL_CLASS, ROW_CLASS);
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

		/* All Stats */
		for (i = 0; i < A_MAX; i++) prt_stat(i);
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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Save usable flags */
		if (angband_term[j]) mask |= window_flag[j];
	}

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

	/* Apply usable flags */
	p_ptr->window &= mask;

	/* Nothing to do */
	if (!p_ptr->window) return;

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

	/* Display monster list */
	if (p_ptr->window & (PW_VISIBLE))
	{
		p_ptr->window &= ~(PW_VISIBLE);
		fix_visible();
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
