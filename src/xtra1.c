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
#include "script.h"


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
	if (p_ptr->stat[stat].cur < p_ptr->stat[stat].max)
	{
		put_fstr(COL_STAT, ROW_STAT + stat, "%5s" CLR_YELLOW " %v",
				 stat_names_reduced[stat],
				 stat_format, p_ptr->stat[stat].use);
	}

	/* Display "healthy" stat */
	else
	{
		put_fstr(COL_STAT, ROW_STAT + stat, "%5s" CLR_L_GREEN " %v",
				 stat_names[stat],
				 stat_format, p_ptr->stat[stat].use);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat[stat].max == stat_cap(stat))
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
	if (p_ptr->tim.image)
	{
		letter[num] = CLR_VIOLET "H";
		num++;
	}

	/* Blindness */
	if (p_ptr->tim.blind)
	{
		letter[num] = CLR_L_DARK "B";
		num++;
	}

	/* Times see-invisible */
	if (p_ptr->tim.invis)
	{
		letter[num] = CLR_L_BLUE "I";
		num++;
	}

	/* Timed esp */
	if (p_ptr->tim.esp)
	{
		letter[num] = CLR_ORANGE "E";
		num++;
	}

	/* Timed infra-vision */
	if (p_ptr->tim.infra)
	{
		letter[num] = CLR_L_RED "I";
		num++;
	}

	/* Paralysis */
	if (p_ptr->tim.paralyzed)
	{
		letter[num] = CLR_RED "P";
		num++;
	}

	/* Confusion */
	if (p_ptr->tim.confused)
	{
		letter[num] = CLR_VIOLET "C";
		num++;
	}

	/* Fast */
	if (p_ptr->tim.fast)
	{
		letter[num] = CLR_GREEN "S";
		num++;
	}

	/* Slow */
	if (p_ptr->tim.slow)
	{
		letter[num] = CLR_RED "S";
		num++;
	}

	/* Protection from evil */
	if (p_ptr->tim.protevil)
	{
		letter[num] = CLR_L_DARK "E";
		num++;
	}

	/* Invulnerability */
	if (p_ptr->tim.invuln)
	{
		letter[num] = CLR_YELLOW "I";
		num++;
	}

	/* Wraith form */
	if (p_ptr->tim.wraith_form)
	{
		letter[num] = CLR_L_DARK "W";
		num++;
	}

	/* Heroism */
	if (p_ptr->tim.hero)
	{
		letter[num] = CLR_WHITE "H";
		num++;
	}

	/* Super Heroism / berserk */
	if (p_ptr->tim.shero)
	{
		letter[num] = CLR_RED "B";
		num++;
	}

	/* Blessed */
	if (p_ptr->tim.blessed)
	{
		letter[num] = CLR_WHITE "B";
		num++;
	}

	/* Shield */
	if (p_ptr->tim.shield)
	{
		letter[num] = CLR_WHITE "S";
		num++;
	}

	/* Oppose Acid */
	if (p_ptr->tim.oppose_acid)
	{
		letter[num] = CLR_GREEN "A";
		num++;
	}

	/* Oppose Lightning */
	if (p_ptr->tim.oppose_elec)
	{
		letter[num] = CLR_BLUE "E";
		num++;
	}

	/* Oppose Fire */
	if (p_ptr->tim.oppose_fire)
	{
		letter[num] = CLR_RED "F";
		num++;
	}

	/* Oppose Cold */
	if (p_ptr->tim.oppose_cold)
	{
		letter[num] = CLR_WHITE "C";
		num++;
	}

	/* Oppose Poison */
	if (p_ptr->tim.oppose_pois)
	{
		letter[num] = CLR_GREEN "P";
		num++;
	}

	/* Word of Recall */
	if (p_ptr->tim.word_recall)
	{
		letter[num] = CLR_WHITE "W";
		num++;
	}

	/* Confusing Hands */
	if (p_ptr->state.confusing)
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
	if (p_ptr->state.wizard)
	{
		p = "[=-WIZARD-=]";
	}

	/* Winner */
	else if (p_ptr->state.total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "***WINNER***";
	}

	/* Normal */
	else
	{
		p = player_title[p_ptr->rp.pclass][(p_ptr->lev - 1) / 5];

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
			put_fstr(COL_EXP, ROW_EXP, "NEED %s*******", attr);
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
			int q_num = place[p_ptr->place_num].quest_num;

			/* Is there a quest here? */
			if (q_num && q_num < z_info->q_max)
			{
				/* Is this a quest to find a ruin? */
				if (quest[q_num].type == QUEST_TYPE_FIND_PLACE)
				{
					prtf(COL_DEPTH, Term->hgt - 1, "Ruin");
				}
				/* then it is this a wilderness quest */
				else
				{
					prtf(COL_DEPTH, Term->hgt - 1, "Quest");
				}
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
	else if (p_ptr->depth == dungeon()->max_level)
	{
		prtf(COL_DEPTH, Term->hgt - 1, "Bottom");
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
	if (p_ptr->tim.blind)
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
	if (p_ptr->tim.confused)
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
	if (p_ptr->tim.afraid)
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
	if (p_ptr->tim.poisoned)
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
	if (p_ptr->state.resting)
	{
		int i;

		/* Start with "Rest" */
		strcpy(text, "R     ");

		/* Extensive (timed) rest */
		if (p_ptr->state.resting >= 1000)
		{
			i = p_ptr->state.resting / 100;
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
		else if (p_ptr->state.resting >= 100)
		{
			i = p_ptr->state.resting;
			text[5] = '0' + (i % 10);
			i = i / 10;
			text[4] = '0' + (i % 10);
			text[3] = '0' + (i / 10);
		}

		/* Medium (timed) rest */
		else if (p_ptr->state.resting >= 10)
		{
			i = p_ptr->state.resting;
			text[5] = '0' + (i % 10);
			text[4] = '0' + (i / 10);
		}

		/* Short (timed) rest */
		else if (p_ptr->state.resting > 0)
		{
			i = p_ptr->state.resting;
			text[5] = '0' + (i);
		}

		/* Rest until healed */
		else if (p_ptr->state.resting == -1)
		{
			text[1] = text[2] = text[3] = text[4] = text[5] = '*';
		}

		/* Rest until done */
		else if (p_ptr->state.resting == -2)
		{
			text[1] = text[2] = text[3] = text[4] = text[5] = '&';
		}
		
		/* Display the info (or blanks) */
		put_fstr(COL_STATE, Term->hgt - 1, text);
	}

	/* Repeating */
	else if (p_ptr->cmd.rep)
	{
		if (p_ptr->cmd.rep > 999)
		{
			put_fstr(COL_STATE, Term->hgt - 1, "C%3d00", p_ptr->cmd.rep / 100);
		}
		else
		{
			put_fstr(COL_STATE, Term->hgt - 1, "C  %3d", p_ptr->cmd.rep);
		}
	}

	/* Searching */
	else if (p_ptr->state.searching)
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
	if (p_ptr->state.searching) i += 10;

	/* Paralysis */
	if (p_ptr->tim.paralyzed)
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
	int c = p_ptr->tim.cut;

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
	int s = p_ptr->tim.stun;

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
	else if (p_ptr->tim.image)
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
		if (p_ptr->monster_race_idx) display_roff_mon(p_ptr->monster_race_idx);

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
	int use_realm1 = p_ptr->spell.r[0].realm - 1;
	int use_realm2 = p_ptr->spell.r[1].realm - 1;
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
		(adj_mag_study[p_ptr->stat[mp_ptr->spell_stat].ind] * levels / 50);


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < PY_MAX_SPELLS; j++)
	{
		/* Count known spells */
		if ((j < 32) ?
			(p_ptr->spell.r[0].learned & (1L << j)) :
			(p_ptr->spell.r[1].learned & (1L << (j - 32))))
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
		if (!p_ptr->spell.r[0].learned && !p_ptr->spell.r[1].learned) break;

		/* Access the spell */
		j = p_ptr->spell.order[i];

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
			(p_ptr->spell.r[0].learned & (1L << j)) :
			(p_ptr->spell.r[1].learned & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				p_ptr->spell.r[0].forgotten |= (1L << j);
				p_ptr->spell.r[0].learned &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				p_ptr->spell.r[1].forgotten |= (1L << (j - 32));
				p_ptr->spell.r[1].learned &= ~(1L << (j - 32));
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
		if (!p_ptr->spell.r[0].learned && !p_ptr->spell.r[1].learned) break;

		/* Get the (i+1)th spell learned */
		j = p_ptr->spell.order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
		if ((j < 32) ?
			(p_ptr->spell.r[0].learned & (1L << j)) :
			(p_ptr->spell.r[1].learned & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				p_ptr->spell.r[0].forgotten |= (1L << j);
				p_ptr->spell.r[0].learned &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				p_ptr->spell.r[1].forgotten |= (1L << (j - 32));
				p_ptr->spell.r[1].learned &= ~(1L << (j - 32));
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
		if (!p_ptr->spell.r[0].forgotten && !p_ptr->spell.r[1].forgotten) break;

		/* Get the next spell we learned */
		j = p_ptr->spell.order[i];

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
			(p_ptr->spell.r[0].forgotten & (1L << j)) :
			(p_ptr->spell.r[1].forgotten & (1L << (j - 32))))
		{
			/* No longer forgotten - known once more */
			if (j < 32)
			{
				p_ptr->spell.r[0].forgotten &= ~(1L << j);
				p_ptr->spell.r[0].learned |= (1L << j);
				which = use_realm1;
			}
			else
			{
				p_ptr->spell.r[1].forgotten &= ~(1L << (j - 32));
				p_ptr->spell.r[1].learned |= (1L << (j - 32));
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
	for (j = 0; j < (p_ptr->spell.r[1].realm != REALM_NONE ? 64 : 32); j++)
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
			(p_ptr->spell.r[0].learned & (1L << j)) :
			(p_ptr->spell.r[1].learned & (1L << (j - 32))))
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

	bool old_cumber_glove = p_ptr->state.cumber_glove;
	bool old_cumber_armor = p_ptr->state.cumber_armor;


	/* Hack -- Must be literate */
	if (!mp_ptr->spell_book) return;

	if (p_ptr->rp.pclass == CLASS_MINDCRAFTER)
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
	msp = adj_mag_mana[p_ptr->stat[mp_ptr->spell_stat].ind] * levels / 25;

	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Hack: High mages have a 25% mana bonus */
	if (msp && (p_ptr->rp.pclass == CLASS_HIGH_MAGE)) msp += msp / 4;


	/* Only mages are affected */
	if (mp_ptr->spell_book == TV_SORCERY_BOOK)
	{
		/* Assume player is not encumbered by gloves */
		p_ptr->state.cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &p_ptr->equipment[EQUIP_HANDS];

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx && (o_ptr->pval > 0) &&
			!((FLAG(o_ptr, TR_FREE_ACT)) || (FLAG(o_ptr, TR_DEX))))
		{
			/* Encumbered */
			p_ptr->state.cumber_glove = TRUE;

			/* Reduce mana */
			msp = (3 * msp) / 4;
		}
	}


	/* Assume player not encumbered by armor */
	p_ptr->state.cumber_armor = FALSE;

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
		p_ptr->state.cumber_armor = TRUE;

		/*
		 * Subtract a percentage of maximum mana.
		 * The addition of one is to make sure the
		 * mana total is decreased by some amount.
		 */
		switch (p_ptr->rp.pclass)
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


	/* Add bonus mana (not affected by encumberance or gloves) */
	msp += p_ptr->sp_bonus * levels;

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
	if (old_cumber_glove != p_ptr->state.cumber_glove)
	{
		/* Message */
		if (p_ptr->state.cumber_glove)
		{
			msgf("Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msgf("Your hands feel more suitable for spellcasting.");
		}
	}


	/* Take note when "armor state" changes */
	if (old_cumber_armor != p_ptr->state.cumber_armor)
	{
		/* Message */
		if (p_ptr->state.cumber_armor)
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
	bonus = ((int)(adj_con_mhp[p_ptr->stat[A_CON].ind]) - 128);

	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->lev - 1] + (bonus * p_ptr->lev / 2);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

	/* Factor in the hero / superhero settings */
	if (p_ptr->tim.hero) mhp += 10;
	if (p_ptr->tim.shero) mhp += 30;

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
			if (FLAG(o_ptr, TR_INSTA_ART))
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

			/* does this item glow? */
			if (FLAG(o_ptr, TR_LITE)) p_ptr->cur_lite++;
		}
	}

	/*
	 * Check if the player doesn't have a lite source,
	 * but does glow as an intrinsic.
	 */
	if ((p_ptr->cur_lite == 0) && (FLAG(p_ptr, TR_LITE)))
	{
		p_ptr->cur_lite = 1;
	}
	
	/*
	 * Hack - blindness gives a torch radius of zero.
	 * This speeds up the map_info() function.
	 */
	if (p_ptr->tim.blind)
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
	i = adj_str_wgt[p_ptr->stat[A_STR].ind] * 100;

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


void object_bonuses(const object_type *o_ptr, bonuses_type *b)
{
	/* Zero the bonuses */
	memset(b, 0, sizeof(bonuses_type));
	
	/* Affect stats */
	if (FLAG(o_ptr, TR_STR)) b->stat[A_STR] = o_ptr->pval;
	if (FLAG(o_ptr, TR_INT)) b->stat[A_INT] = o_ptr->pval;
	if (FLAG(o_ptr, TR_WIS)) b->stat[A_WIS] = o_ptr->pval;
	if (FLAG(o_ptr, TR_DEX)) b->stat[A_DEX] = o_ptr->pval;
	if (FLAG(o_ptr, TR_CON)) b->stat[A_CON] = o_ptr->pval;
	if (FLAG(o_ptr, TR_CHR)) b->stat[A_CHR] = o_ptr->pval;

	/* Affect mana */
	if (FLAG(o_ptr, TR_SP)) b->sp_bonus = o_ptr->pval;

	/* Affect stealth */
	if (FLAG(o_ptr, TR_STEALTH)) b->skills[SKILL_STL] = o_ptr->pval;

	/* Affect sensing ability (factor of five) */
	if (FLAG(o_ptr, TR_SEARCH)) b->skills[SKILL_SNS] = (o_ptr->pval * 5);

	/* Affect searching frequency (factor of five) */
	if (FLAG(o_ptr, TR_SEARCH)) b->skills[SKILL_FOS] = (o_ptr->pval * 5);

	/* Affect infravision */
	if (FLAG(o_ptr, TR_INFRA)) b->see_infra = o_ptr->pval;

	/* Affect digging (factor of 20) */
	if (FLAG(o_ptr, TR_TUNNEL)) b->skills[SKILL_DIG] = (o_ptr->pval * 20);

	/* Affect speed */
	if (FLAG(o_ptr, TR_SPEED)) b->pspeed = o_ptr->pval;

	/* Affect blows */
	if (FLAG(o_ptr, TR_BLOWS)) b->extra_blows = o_ptr->pval;

	/* Boost shots */
	if (FLAG(o_ptr, TR_XTRA_SHOTS)) b->extra_shots = 1;
	
	/* Boost saving throws */
	if (FLAG(o_ptr, TR_LUCK_10)) b->skills[SKILL_SAV] = 10;

	/* Apply special bonuses */
	apply_object_trigger(TRIGGER_BONUS, (object_type *)o_ptr, "p", "b", "bonuses_type", b);
}


void object_bonuses_known(const object_type *o_ptr, bonuses_type *b)
{
	/* Zero the bonuses */
	memset(b, 0, sizeof(bonuses_type));
	
	/* Affect stats */
	if (KN_FLAG(o_ptr, TR_STR)) b->stat[A_STR] = o_ptr->pval;
	if (KN_FLAG(o_ptr, TR_INT)) b->stat[A_INT] = o_ptr->pval;
	if (KN_FLAG(o_ptr, TR_WIS)) b->stat[A_WIS] = o_ptr->pval;
	if (KN_FLAG(o_ptr, TR_DEX)) b->stat[A_DEX] = o_ptr->pval;
	if (KN_FLAG(o_ptr, TR_CON)) b->stat[A_CON] = o_ptr->pval;
	if (KN_FLAG(o_ptr, TR_CHR)) b->stat[A_CHR] = o_ptr->pval;

	/* Affect mana */
	if (KN_FLAG(o_ptr, TR_SP)) b->sp_bonus = o_ptr->pval;

	/* Affect stealth */
	if (KN_FLAG(o_ptr, TR_STEALTH)) b->skills[SKILL_STL] = o_ptr->pval;

	/* Affect sensing ability (factor of five) */
	if (KN_FLAG(o_ptr, TR_SEARCH)) b->skills[SKILL_SNS] = (o_ptr->pval * 5);

	/* Affect searching frequency (factor of five) */
	if (KN_FLAG(o_ptr, TR_SEARCH)) b->skills[SKILL_FOS] = (o_ptr->pval * 5);

	/* Affect infravision */
	if (KN_FLAG(o_ptr, TR_INFRA)) b->see_infra = o_ptr->pval;

	/* Affect digging (factor of 20) */
	if (KN_FLAG(o_ptr, TR_TUNNEL)) b->skills[SKILL_DIG] = (o_ptr->pval * 20);

	/* Affect speed */
	if (KN_FLAG(o_ptr, TR_SPEED)) b->pspeed = o_ptr->pval;

	/* Affect blows */
	if (KN_FLAG(o_ptr, TR_BLOWS)) b->extra_blows = o_ptr->pval;

	/* Boost shots */
	if (KN_FLAG(o_ptr, TR_XTRA_SHOTS)) b->extra_shots = 1;
	
	/* Boost saving throws */
	if (KN_FLAG(o_ptr, TR_LUCK_10)) b->skills[SKILL_SAV] = 10;

	/* Apply special bonuses */
	if (object_known_full(o_ptr))
		apply_object_trigger(TRIGGER_BONUS, (object_type *)o_ptr, "p", "b", "bonuses_type", b);
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
	bool old_telepathy;
	bool old_see_inv;
	int old_dis_ac;
	int old_dis_to_a;
	int extra_blows;
	int extra_shots;
	object_type *o_ptr;

	bool old_heavy_wield = p_ptr->state.heavy_wield;
	bool old_heavy_shoot = p_ptr->state.heavy_shoot;
	bool old_icky_wield = p_ptr->state.icky_wield;
	bool old_monk_armour = p_ptr->state.monk_armour_stat;

	object_flags oflags;
	object_flags *of_ptr = &oflags;

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = FLAG(p_ptr, TR_TELEPATHY) ? TRUE : FALSE;
	old_see_inv = FLAG(p_ptr, TR_SEE_INVIS) ? TRUE : FALSE;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;


	/* Clear extra blows/shots */
	extra_blows = extra_shots = 0;

	/* Calculate monk armour status */
	if (p_ptr->rp.pclass == CLASS_MONK)
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
			p_ptr->state.monk_armour_stat = TRUE;
		}
		else
		{
			/* Not burdened */
			p_ptr->state.monk_armour_stat = FALSE;
		}
	}

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) p_ptr->stat[i].add = 0;


	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Start with "normal" speed */
	p_ptr->pspeed = 110;

	/* Start with "normal" mana */
	p_ptr->sp_bonus = 0;

	/* Start with a single blow per turn */
	p_ptr->num_blow = 1;

	/* Start with a single shot per turn */
	p_ptr->num_fire = 1;

	/* Reset the "ammo" tval */
	p_ptr->ammo_tval = 0;

	/* Clear all the flags */
	p_ptr->flags[0] = 0;
	p_ptr->flags[1] = 0;
	p_ptr->flags[2] = 0;
	p_ptr->flags[3] = 0;

	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra;


	/* Base skill -- disarming */
	p_ptr->skills[SKILL_DIS] = rp_ptr->r_dis + cp_ptr->c_dis;

	/* Base skill -- magic devices */
	p_ptr->skills[SKILL_DEV] = rp_ptr->r_dev + cp_ptr->c_dev;

	/* Base skill -- saving throw */
	p_ptr->skills[SKILL_SAV] = rp_ptr->r_sav + cp_ptr->c_sav;

	/* Base skill -- stealth */
	p_ptr->skills[SKILL_STL] = rp_ptr->r_stl + cp_ptr->c_stl;

	/* Base skill -- sensing ability */
	p_ptr->skills[SKILL_SNS] = rp_ptr->r_sns + cp_ptr->c_sns;

	/* Base skill -- searching frequency */
	p_ptr->skills[SKILL_FOS] = rp_ptr->r_fos + cp_ptr->c_fos;

	/* Base skill -- combat (normal) */
	p_ptr->skills[SKILL_THN] = rp_ptr->r_thn + cp_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	p_ptr->skills[SKILL_THB] = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	p_ptr->skills[SKILL_THT] = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- digging */
	p_ptr->skills[SKILL_DIG] = 0;

	/* Get the player racial/class flags (including some mutations) */
	player_flags(of_ptr);

	/* Hack - handle speed from monk/sprite/klackon here */
	if (FLAG(of_ptr, TR_SPEED))
	{
		p_ptr->pspeed += (p_ptr->lev) / 10;
		of_ptr->flags[0] &= ~(TR0_SPEED);
	}

	p_ptr->flags[0] |= of_ptr->flags[0];
	p_ptr->flags[1] |= of_ptr->flags[1];
	p_ptr->flags[2] |= of_ptr->flags[2];
	p_ptr->flags[3] |= of_ptr->flags[3];

	/* Effects of constantly acting mutations */
	if (p_ptr->muta3)
	{
		mutation_effect();
	}

	/* Scan the usable inventory */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		bonuses_type b;

		o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;
		
		p_ptr->flags[0] |= o_ptr->flags[0];
		p_ptr->flags[1] |= o_ptr->flags[1];
		p_ptr->flags[2] |= o_ptr->flags[2];
		p_ptr->flags[3] |= o_ptr->flags[3];

		/* Calculate bonuses from object */
		object_bonuses(o_ptr, &b);

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply bonuses to stats */
		for (j = 0; j < A_MAX; j++)
		{
			p_ptr->stat[j].add += b.stat[j];
		}
		
		p_ptr->sp_bonus += b.sp_bonus;
		p_ptr->see_infra += b.see_infra;
		p_ptr->pspeed += b.pspeed;
		extra_blows += b.extra_blows;
		extra_shots += b.extra_shots;

		/* Apply bonuses to skills */
		for (j = 0; j < MAX_SKILL; j++)
		{
			p_ptr->skills[j] += b.skills[j];
		}

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
	if ((p_ptr->rp.pclass == CLASS_MONK) && (!p_ptr->state.monk_armour_stat))
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
	if (FLAG(p_ptr, TR_SH_FIRE)) SET_FLAG(p_ptr, TR_LITE);

	/* Golems also get an intrinsic AC bonus */
	if (p_ptr->rp.prace == RACE_GOLEM)
	{
		p_ptr->to_a += 20 + (p_ptr->lev / 5);
		p_ptr->dis_to_a += 20 + (p_ptr->lev / 5);
	}

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int top, use, ind;

		/* Extract the new "stat_use" value for the stat */
		top = modify_stat_value(p_ptr->stat[i].max, p_ptr->stat[i].add);

		/* Notice changes */
		if (p_ptr->stat[i].top != top)
		{
			/* Save the new value */
			p_ptr->stat[i].top = top;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}


		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat[i].cur, p_ptr->stat[i].add);

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
		if (p_ptr->stat[i].use != use)
		{
			/* Save the new value */
			p_ptr->stat[i].use = use;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}

		/* Find index into various tables */
		if (use < 400)
			ind = use / 10 - 3;
		else
			ind = 37;

		/* Notice changes */
		if (p_ptr->stat[i].ind != ind)
		{
			/* Save the new index */
			p_ptr->stat[i].ind = ind;

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
	if (p_ptr->tim.stun > 50)
	{
		p_ptr->to_h -= 20;
		p_ptr->dis_to_h -= 20;
		p_ptr->to_d -= 20;
		p_ptr->dis_to_d -= 20;
	}
	else if (p_ptr->tim.stun)
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}

	/* Invulnerability */
	if (p_ptr->tim.invuln)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
	}

	/* wraith_form */
	if (p_ptr->tim.wraith_form)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
		SET_FLAG(p_ptr, TR_REFLECT);
	}

	/* Temporary blessing */
	if (p_ptr->tim.blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temporary shield */
	if (p_ptr->tim.shield)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary "Hero" */
	if (p_ptr->tim.hero)
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
	}

	/* Temporary "Beserk" */
	if (p_ptr->tim.shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
	}

	/* Temporary "fast" */
	if (p_ptr->tim.fast)
	{
		p_ptr->pspeed += 10;
	}

	/* Temporary "slow" */
	if (p_ptr->tim.slow)
	{
		p_ptr->pspeed -= 10;
	}

	/* Temporary "telepathy" */
	if (p_ptr->tim.esp)
	{
		SET_FLAG(p_ptr, TR_TELEPATHY);
	}

	/* Temporary see invisible */
	if (p_ptr->tim.invis)
	{
		SET_FLAG(p_ptr, TR_SEE_INVIS);
	}

	/* Temporary infravision boost */
	if (p_ptr->tim.infra)
	{
		p_ptr->see_infra += 3;
	}


	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->tim.hero || p_ptr->tim.shero)
	{
		SET_FLAG(p_ptr, TR_RES_FEAR);
	}

	/* Hack -- Telepathy Change */
	if (FLAG(p_ptr, TR_TELEPATHY) != old_telepathy)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (((FLAG(p_ptr, TR_SEE_INVIS)) ? TRUE : FALSE) != old_see_inv)
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
	if (p_ptr->state.searching) p_ptr->pspeed -= 10;

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);


	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_dex_ta[p_ptr->stat[A_DEX].ind]) - 128);
	p_ptr->to_d += ((int)(adj_str_td[p_ptr->stat[A_STR].ind]) - 128);
	p_ptr->to_h += ((int)(adj_dex_th[p_ptr->stat[A_DEX].ind]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat[A_DEX].ind]) - 128);
	p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat[A_STR].ind]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[p_ptr->stat[A_DEX].ind]) - 128);


	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}


	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat[A_STR].ind];


	/* Examine the "current bow" */
	o_ptr = &p_ptr->equipment[EQUIP_BOW];


	/* Assume not heavy */
	p_ptr->state.heavy_shoot = FALSE;

	/* It is hard to carry a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->state.heavy_shoot = TRUE;
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

				if (p_ptr->stat[A_STR].use >= 160)
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
				if (p_ptr->stat[A_DEX].use >= 160)
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
		if (o_ptr->k_idx && !p_ptr->state.heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Hack -- Rangers love Bows */
			if ((p_ptr->rp.pclass == CLASS_RANGER) &&
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
			if ((p_ptr->rp.pclass == CLASS_RANGER) &&
				(p_ptr->ammo_tval == TV_BOLT))
			{
				/* Extra shot at level 30 */
				if (p_ptr->lev >= 30) p_ptr->num_fire++;
			}

			/* Hack -- Rogues love Slings */
			if ((p_ptr->rp.pclass == CLASS_ROGUE) && (p_ptr->ammo_tval == TV_SHOT))
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
			if (p_ptr->rp.pclass == CLASS_WARRIOR &&
				(p_ptr->ammo_tval <= TV_BOLT) && (p_ptr->ammo_tval >= TV_SHOT))
			{
				/* Extra shot at level 40 */
				if (p_ptr->lev >= 40) p_ptr->num_fire++;

			}
		}
	}
	/* Add all class and race-specific adjustments to missile Skill. -LM- */
	p_ptr->skills[SKILL_THB] += add_special_missile_skill(p_ptr->rp.pclass);

	/* Examine the "main weapon" */
	o_ptr = &p_ptr->equipment[EQUIP_WIELD];

	/* Add all other class-specific adjustments to melee Skill. -LM- */
	p_ptr->skills[SKILL_THN] += add_special_melee_skill(p_ptr->rp.pclass, o_ptr);

	/* Assume okay */
	p_ptr->state.icky_wield = FALSE;

	/* Extra bonus for warriors... */
	if (p_ptr->rp.pclass == CLASS_WARRIOR)
	{
		p_ptr->to_h += (p_ptr->lev / 5);
		p_ptr->to_d += (p_ptr->lev / 5);

		p_ptr->dis_to_h += (p_ptr->lev / 5);
		p_ptr->dis_to_d += (p_ptr->lev / 5);
	}

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((p_ptr->rp.pclass == CLASS_PRIEST) &&
		(!(FLAG(p_ptr, TR_BLESSED))) &&
		((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		/* Reduce the real bonuses */
		p_ptr->to_h -= (p_ptr->lev / 5);
		p_ptr->to_d -= (p_ptr->lev / 5);

		/* Reduce the mental bonuses */
		p_ptr->dis_to_h -= (p_ptr->lev / 5);
		p_ptr->dis_to_d -= (p_ptr->lev / 5);

		/* Icky weapon */
		p_ptr->state.icky_wield = TRUE;
	}

	/* Affect Skill -- stealth (bonus one) */
	p_ptr->skills[SKILL_STL] += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skills[SKILL_DIS] += adj_dex_dis[p_ptr->stat[A_DEX].ind];
	p_ptr->skills[SKILL_DIS] += adj_int_dis[p_ptr->stat[A_INT].ind];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skills[SKILL_DEV] += adj_int_dev[p_ptr->stat[A_INT].ind];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skills[SKILL_SAV] += (adj_wis_sav[p_ptr->stat[A_WIS].ind] - 128);

	/* Affect Skill -- digging (STR) */
	p_ptr->skills[SKILL_DIG] += adj_str_dig[p_ptr->stat[A_STR].ind];

	/* Affect Skill -- disarming (Level, by Class) */
	p_ptr->skills[SKILL_DIS] += (cp_ptr->x_dis * p_ptr->lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->skills[SKILL_DEV] += (cp_ptr->x_dev * p_ptr->lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->skills[SKILL_SAV] += (cp_ptr->x_sav * p_ptr->lev / 10);

	/* Affect Skill -- stealth (Level, by Class) */
	p_ptr->skills[SKILL_STL] += (cp_ptr->x_stl * p_ptr->lev / 10);

	/* Affect Skill -- sensing ability (Level, by Class) */
	p_ptr->skills[SKILL_SNS] += (cp_ptr->x_sns * p_ptr->lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->skills[SKILL_FOS] += (cp_ptr->x_fos * p_ptr->lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skills[SKILL_THN] += (cp_ptr->x_thn * p_ptr->lev / 50);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skills[SKILL_THB] += (cp_ptr->x_thb * p_ptr->lev / 50);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skills[SKILL_THT] += (cp_ptr->x_thb * p_ptr->lev / 50);

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skills[SKILL_DIG] < 1) p_ptr->skills[SKILL_DIG] = 1;

	/* Limit Skill -- saving throw from 1 up */
	if (p_ptr->skills[SKILL_SAV] < 1) p_ptr->skills[SKILL_SAV] = 1;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skills[SKILL_STL] > 30) p_ptr->skills[SKILL_STL] = 30;
	if (p_ptr->skills[SKILL_STL] < 0) p_ptr->skills[SKILL_STL] = 0;

	/* Apply Skill -- Extract noise from stealth */
	p_ptr->noise = (1L << (30 - p_ptr->skills[SKILL_STL]));

	if ((FLAG(p_ptr, TR_NO_MAGIC)) && (p_ptr->skills[SKILL_SAV] < p_ptr->lev * 2 + 85))
		 p_ptr->skills[SKILL_SAV] = p_ptr->lev * 2 + 85;

	/* Assume not heavy */
	p_ptr->state.heavy_wield = FALSE;

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
			p_ptr->state.heavy_wield = TRUE;

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
			str_index = mul * adj_str_blow[p_ptr->stat[A_STR].ind] /
				effective_weight;

			/* Maximal value */
			if (str_index > 11) str_index = 11;

			/* Index by dexterity */
			dex_index = (adj_dex_blow[p_ptr->stat[A_DEX].ind]);

			/* Maximal value */
			if (dex_index > 11) dex_index = 11;


			/* Use the blows table */
			p_ptr->num_blow = blows_table[str_index][dex_index];

			/* Get weapon skill (not including magical enhancments) */
			skill = p_ptr->skills[SKILL_THN];

			/* Require high skill to get large number of blows */
			if (p_ptr->num_blow > 1 + skill / 15)
				p_ptr->num_blow = 1 + skill / 15;

			/* Paranoia - require at least one blow */
			if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;

			/* Add in extra blows */
			p_ptr->num_blow += extra_blows;


			/* Boost digging skill by weapon weight */
			p_ptr->skills[SKILL_DIG] += (o_ptr->weight / 10);
		}
	}

	/* No weapon */
	else
	{
		/* Different calculation for monks with empty hands */
		if (p_ptr->rp.pclass == CLASS_MONK)
		{
			p_ptr->num_blow = 2;

			if (p_ptr->lev > 9) p_ptr->num_blow++;
			if (p_ptr->lev > 14) p_ptr->num_blow++;
			if (p_ptr->lev > 24) p_ptr->num_blow++;
			if (p_ptr->lev > 34) p_ptr->num_blow++;
			if (p_ptr->lev > 44) p_ptr->num_blow++;
			if (p_ptr->lev > 49) p_ptr->num_blow++;

			if (p_ptr->state.monk_armour_stat)
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
	if (old_heavy_shoot != p_ptr->state.heavy_shoot)
	{
		/* Message */
		if (p_ptr->state.heavy_shoot)
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
	if (old_heavy_wield != p_ptr->state.heavy_wield)
	{
		/* Message */
		if (p_ptr->state.heavy_wield)
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
	if (old_icky_wield != p_ptr->state.icky_wield)
	{
		/* Message */
		if (p_ptr->state.icky_wield)
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

	if (p_ptr->rp.pclass == CLASS_MONK &&
		(p_ptr->state.monk_armour_stat != old_monk_armour))
	{
		if (p_ptr->state.monk_armour_stat)
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
	
	/* Do not update map, it doesn't exist */
	if (!character_dungeon) return;
	
	if (p_ptr->update & (PU_MAP))
	{
		p_ptr->update &= ~(PU_MAP);
		map_panel_size();
	}
	
	if ((p_ptr->update & (PU_MON_LITE)) && monster_light)
	{
		p_ptr->update &= ~(PU_MON_LITE);
		update_mon_lite();
		
		/*
		 * Hack - the odds are that since monsters moved, 
		 * we need to redraw the map.
		 */
		p_ptr->redraw |= (PR_MAP);
		p_ptr->update |= (PU_VIEW);
	}

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
	
	/* Do not update map it, doesn't exist */
	if (!character_dungeon) return;
	
	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
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

	/* Display object recall */
	if (p_ptr->window & (PW_OBJECT))
	{
		p_ptr->window &= ~(PW_OBJECT);
		fix_object();
	}
	
	/* Display monster recall */
	if (p_ptr->window & (PW_MONSTER))
	{
		p_ptr->window &= ~(PW_MONSTER);
		fix_monster();
	}

	/* Do not update map it, doesn't exist */
	if (!character_dungeon) return;

	/* Display monster list */
	if (p_ptr->window & (PW_VISIBLE))
	{
		p_ptr->window &= ~(PW_VISIBLE);
		fix_visible();
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

/*
 * Handle "p_ptr->change"
 */
void change_stuff(void)
{
	/* Nothing to do */
	if (!p_ptr->change) return;

	/* Redraw screen after a wiz_lite() */
	if (p_ptr->change & (PC_WIZ_LITE))
	{
		p_ptr->change &= ~(PC_WIZ_LITE);
	
		change_wiz_lite();
	}
	
	/* Shimmer monsters */
	if (p_ptr->change & (PC_SHIMMER))
	{
		p_ptr->change &= ~(PC_SHIMMER);
		
		change_shimmer();
	}
	
	/* Repair monsters */
	if (p_ptr->change & (PC_REPAIR))
	{
		p_ptr->change &= ~(PC_REPAIR);
		
		change_repair();
	}
	
	/* Give beastman mutation at birth */
	if (p_ptr->change & (PC_MUTATE))
	{
		p_ptr->change &= ~(PC_MUTATE);
		
		msgf("You feel different!");
		(void)gain_mutation(0);
	}
}

/*
 * Roll a saving throw for the player
 */
bool player_save(int power)
{
	return saving_throw(p_ptr->skills[SKILL_SAV] - power);
}
