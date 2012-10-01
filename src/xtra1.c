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
        sprintf(out_val, "    %3d", val);
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
                        value++;
		}
	}

	/* Penalty */
	else if (amount < 0)
	{
		/* Apply each point */
		for (i = 0; i < (0 - amount); i++)
		{
			/* One point at a time */
                        if (value > 0) value--;
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
 * Prints players max/cur tank points
 */
/* NEWANGBAND: OBSOLETE FUNCTION!! */
static void prt_tp(void)
{
        return;
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
		int colour;

		if (p_ptr->stat_cnt[stat])
			colour=TERM_ORANGE;
		else
			colour=TERM_YELLOW;

		put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		/* Formerly stat_use */
		cnv_stat(p_ptr->stat_ind[stat], tmp);
		c_put_str(colour, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], ROW_STAT + stat, 0);
		/* Formerly stat_use */
		cnv_stat(p_ptr->stat_ind[stat], tmp);
		c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Indicate natural maximum */
	/*if (p_ptr->stat_max[stat] == 18+100)*/
	/*{*/
	/*	put_str("!", ROW_STAT + stat, 3);*/
	/*}*/
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
                int gendervar = 0;
                if (p_ptr->psex == SEX_FEMALE) p = classes_def[p_ptr->pclass].ranksf[p_ptr->class_level[p_ptr->pclass] - 1];
                else p = classes_def[p_ptr->pclass].ranksm[p_ptr->class_level[p_ptr->pclass] - 1];
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

	put_str("AC ", ROW_AC, COL_AC);
	sprintf(tmp, "%9ld", (long)(p_ptr->dis_ac));

	/* Actually use the same column as gold. */
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_GOLD + 3);
}


/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	char tmp[32];

	byte color;


        
                put_str("HP ", ROW_HP, COL_HP);
		if (p_ptr->mhp > 99999)
		{
                	sprintf(tmp, "%10ld", p_ptr->chp);
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
                	c_put_str(color, tmp, ROW_HP, COL_HP + 3);
		}
		else
		{
                	sprintf(tmp, "%5ld/%5ld", p_ptr->chp, p_ptr->mhp);
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
                	c_put_str(color, tmp, ROW_HP, COL_HP + 2);
		}
}

/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
	char tmp[32];
	byte color;


        put_str("SP ", ROW_SP, COL_SP);

        sprintf(tmp, "%5ld/%5ld", p_ptr->csp, p_ptr->msp);
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
        c_put_str(color, tmp, ROW_SP, COL_SP + 2);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	char depths[32];

        if (special_flag)
	{
                strcpy(depths, "Special");
        }
	else if (p_ptr->inside_quest)
	{
		strcpy(depths, p_ptr->quest_name);
	}
	else if (!dun_level)
	{
		if (p_ptr->wild_mode)
		{
			(void)sprintf(depths, "(%d,%d)", p_ptr->wild_x, p_ptr->wild_y);
		}
		else strcpy(depths, p_ptr->town_name);
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
	/* Nothing */
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

/* Uses the same column as Stun. */
static void prt_powerattack(void)
{

	if (p_ptr->powerattack > 0)
	{
		if (p_ptr->powerlevel == 3)
		{
			c_put_str(TERM_L_GREEN, "Power Lvl. 3", ROW_STUN, COL_STUN);
		}
		else if (p_ptr->powerlevel == 2)
		{
			c_put_str(TERM_L_GREEN, "Power Lvl. 2", ROW_STUN, COL_STUN);
		}
		else
		{
			c_put_str(TERM_L_GREEN, "Power Lvl. 1", ROW_STUN, COL_STUN);
		}
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
	if (!health_who)
	{
		/* Erase the health bar */
                Term_erase(COL_INFO, ROW_MH, 12);
                Term_erase(COL_INFO, ROW_MH + 1, 12);
		Term_erase(COL_INFO, ROW_MH + 2, 12);
		Term_erase(COL_INFO, ROW_MH + 3, 12);
	}

	/* Tracking an unseen monster */
	else if (!m_list[health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
                Term_putstr(COL_INFO, ROW_MH, 12, TERM_WHITE, "[          ]");
                Term_erase(COL_INFO, ROW_MH + 1, 12);
		Term_erase(COL_INFO, ROW_MH + 2, 12);
		Term_erase(COL_INFO, ROW_MH + 3, 12);
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
                Term_putstr(COL_INFO, ROW_MH, 12, TERM_WHITE, "[          ]");
                Term_erase(COL_INFO, ROW_MH + 1, 12);
		Term_erase(COL_INFO, ROW_MH + 2, 12);
		Term_erase(COL_INFO, ROW_MH + 3, 12);
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
                Term_putstr(COL_INFO, ROW_MH, 12, TERM_WHITE, "[          ]");
                Term_erase(COL_INFO, ROW_MH + 1, 12);
		Term_erase(COL_INFO, ROW_MH + 2, 12);
		Term_erase(COL_INFO, ROW_MH + 3, 12);
	}

	/* Tracking a visible monster */
	else
	{
		int len;
		s32b pct;
                char thehealth[20], thelevel[20], thelives[20], mcr[20];

		monster_type *m_ptr = &m_list[health_who];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Default to almost dead */
		byte attr = TERM_RED;

		/* Extract the "percent" of health */
		if (m_ptr->maxhp > 10000)
		{
			pct = multiply_divide((m_ptr->hp / 100), 100, (m_ptr->maxhp / 100));
		}
		else
		{
			pct = multiply_divide(m_ptr->hp, 100, m_ptr->maxhp);
		}

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

                /* Elite */
                if (m_ptr->boss == 1) attr = 16;

                /* BOSS */
                if (m_ptr->boss == 2) attr = 17;

		/* Cursed monsters! */
		if (r_ptr->cursed > 0) attr = TERM_L_RED;

		/* Questors */
		if (r_ptr->flags1 & (RF1_QUESTOR)) attr = TERM_L_BLUE;

		/* Scaled monsters. */
		if (r_ptr->flags7 & (RF7_SCALED)) attr = TERM_INDIGO;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
                Term_putstr(COL_INFO, ROW_MH, 12, TERM_WHITE, "[          ]");
                Term_erase(COL_INFO, ROW_MH + 1, 12);
		Term_erase(COL_INFO, ROW_MH + 2, 12);

		/* Dump the current "health" (use '*' symbols) */
		if (r_ptr->flags1 & (RF1_QUESTOR) || r_ptr->flags7 & (RF7_SECRET_BOSS)) sprintf(thelevel, "??????????");
		else if (r_ptr->cursed > 0) sprintf(thelevel, "NIGHTMARE");
                else if (m_ptr->boss == 1) sprintf(thelevel, "ELITE");
                else if (m_ptr->boss == 2) sprintf(thelevel, "**BOSS**");
                else sprintf(thelevel, "Level %d", m_ptr->level);
                Term_putstr(COL_INFO + 1, ROW_MH, 12, attr, thelevel);
		if (m_ptr->maxhp >= 100000000) sprintf(thehealth, "%ld", m_ptr->hp);
                else sprintf(thehealth, "Hp: %ld", m_ptr->hp);

		sprintf(mcr, "CR: %d", r_ptr->cr);
		sprintf(thelives, "Lives: %ld", m_ptr->lives);

		Term_putstr(COL_INFO, ROW_MH + 2, 12, attr, mcr);
                /* Don't display the hp if it's a boss */
                if (m_ptr->boss != 2 && !(r_ptr->flags1 & (RF1_QUESTOR)) && !(r_ptr->flags7 & (RF7_SECRET_BOSS)) && (r_ptr->cursed == 0))
		{
			Term_putstr(COL_INFO, ROW_MH + 1, 12, attr, thehealth);
			if (m_ptr->lives > 0) Term_putstr(COL_INFO, ROW_MH + 3, 12, attr, thelives);
			else Term_putstr(COL_INFO, ROW_MH + 3, 12, attr, "            ");
		}
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
        if (p_ptr->pclass == CLASS_ELEM_LORD)
        {
                char s[80];
                char e[80];
                char f[80];
                if (p_ptr->psex == SEX_FEMALE) sprintf(s, "Lady");
                else sprintf(s, "Lord");
                sprintf(e, get_element_name(p_ptr->elemlord));
                sprintf(f, "%s %s", e, s);
                prt_field(f, ROW_CLASS, COL_CLASS);
        }
        else prt_field(classes_def[p_ptr->pclass].name, ROW_CLASS, COL_CLASS);

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();

	/* All Stats */
	for (i = 0; i < 6; i++) prt_stat(i);

	/* Armor */
	prt_ac();

	/* Hitpoints */
	prt_hp();

	/* Spellpoints */
	prt_sp();

        /* Tankerpoints */
        prt_tp();

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
	/*prt_cut();*/
	/*prt_stun();*/
	prt_powerattack();

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
	/*prt_study();*/
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
			Term_putstr(0, (h - 1) - i, -1, TERM_WHITE, message_str((s16b)i));

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
		if (monster_race_idx) display_roff(monster_race_idx);

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
		if (object_kind_idx) display_koff(object_kind_idx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
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
        u32b f1, f2, f3, f4;

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
			if (o_ptr->sval > SV_LITE_LANTERN)
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

			/* does this item glow? */
                        p_ptr->cur_lite += o_ptr->light;
                             
		}

	}

	/* max radius is 5 without rewriting other code -- */
	/* see cave.c:update_lite() and defines.h:LITE_MAX */
        if (p_ptr->cur_lite > 5) p_ptr->cur_lite = 5;

        /* check if the player doesn't have a lite source, */
	/* but does glow as an intrinsic.                  */
        if (p_ptr->cur_lite == 0 && p_ptr->lite) p_ptr->cur_lite = 2;
        /* Monsters see WELL in the dark! */
        /*if (p_ptr->prace == RACE_MONSTER) p_ptr->cur_lite = 5;*/
        if (p_ptr->body_monster) p_ptr->cur_lite = 5;

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
int weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
        i = max_carry() * 100;

	/* Return the result */
	return (i);
}

/*
 * Calc which body parts the player have, based on the
 * monster he incarnate, note that that's bnot a hack
 * since body parts of the player when in it's own body
 * are also defined in r_info(monster 0)
 */
void calc_body()
{
        monster_race *r_ptr = &r_info[p_ptr->body_monster];
        int i;
	int essenceslots = 13;

        for (i = 0; i < INVEN_TOTAL - INVEN_WIELD; i++)
                p_ptr->body_parts[i] = 0;

        /*for (i = 0; i < r_ptr->body_parts[BODY_WEAPON]; i++)
                p_ptr->body_parts[INVEN_WIELD - INVEN_WIELD + i] = INVEN_WIELD;*/
        if (r_ptr->body_parts[BODY_WEAPON])
	{
                /* p_ptr->body_parts[INVEN_BOW - INVEN_WIELD] = INVEN_BOW; */
		p_ptr->body_parts[INVEN_WIELD - INVEN_WIELD] = INVEN_WIELD;
		p_ptr->body_parts[INVEN_WIELD - INVEN_WIELD + 1] = INVEN_WIELD;

		essenceslots -= 2;
	}

        for (i = 0; i < r_ptr->body_parts[BODY_TORSO]; i++)
        {
                p_ptr->body_parts[INVEN_BODY - INVEN_WIELD + i] = INVEN_BODY;
                p_ptr->body_parts[INVEN_OUTER - INVEN_WIELD + i] = INVEN_OUTER;
                p_ptr->body_parts[INVEN_LITE - INVEN_WIELD + i] = INVEN_LITE;
                p_ptr->body_parts[INVEN_AMMO - INVEN_WIELD + i] = INVEN_AMMO;

		if (i == 0) essenceslots -= 4;                
        }

        for (i = 0; i < r_ptr->body_parts[BODY_FINGER]; i++)
	{
                p_ptr->body_parts[INVEN_RING - INVEN_WIELD + i] = INVEN_RING;
		if (i == 0 || i == 1) essenceslots -= 1;
	}

        for (i = 0; i < r_ptr->body_parts[BODY_HEAD]; i++)
        {
                p_ptr->body_parts[INVEN_HEAD - INVEN_WIELD + i] = INVEN_HEAD;
                p_ptr->body_parts[INVEN_NECK - INVEN_WIELD + i] = INVEN_NECK;

		if (i == 0) essenceslots -= 2;
        }

        for (i = 0; i < r_ptr->body_parts[BODY_ARMS]; i++)
        {
                p_ptr->body_parts[INVEN_ARM - INVEN_WIELD + i] = INVEN_ARM;
                p_ptr->body_parts[INVEN_HANDS - INVEN_WIELD + i] = INVEN_HANDS;

		if (i == 0) essenceslots -= 2;
        }
	if (r_ptr->body_parts[BODY_ARMS])
                p_ptr->body_parts[INVEN_TOOL - INVEN_WIELD] = INVEN_TOOL;

        for (i = 0; i < r_ptr->body_parts[BODY_LEGS]; i++)
	{
                p_ptr->body_parts[INVEN_FEET - INVEN_WIELD + i] = INVEN_FEET;
		if (i == 0) essenceslots -= 1;
	}

	/* Monster race without equipment slots gets Essence slots. */
	/* How much they get depends on the monster. */
	if (essenceslots > 0 && p_ptr->prace == RACE_MONSTER)
	{
		for (i = 0; i < essenceslots; i++)
		{
                	p_ptr->body_parts[INVEN_ESSENCE - INVEN_WIELD + i] = INVEN_ESSENCE;
		}
	}

        /* Ok now if the player lost a body part, he must drop the object he had on it */
        for (i = 0; i < INVEN_TOTAL - INVEN_WIELD; i++)
        {
                if ((!p_ptr->body_parts[i]) && (inventory[i + INVEN_WIELD].k_idx))
                {
                        /* Unequip it. */
                        inven_takeoff(i + INVEN_WIELD, 255, FALSE);
                }
        }
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


        if (p_ptr->update & (PU_BODY))
	{
                p_ptr->update &= ~(PU_BODY);
                calc_body();
	}

	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		call_lua("calc_bonuses", "()", "");
	}

	if (p_ptr->update & (PU_TORCH))
	{
		p_ptr->update &= ~(PU_TORCH);
		calc_torch();
	}

	if (p_ptr->update & (PU_HP))
	{
		p_ptr->update &= ~(PU_HP);
		call_lua("calc_hitpoints", "()", "");
	}

	if (p_ptr->update & (PU_MANA))
	{
		p_ptr->update &= ~(PU_MANA);
		call_lua("calc_mana", "()", "");
	}

	if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		/* Does nothing anymore. */
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
		p_ptr->redraw &= ~(PR_MISC | PR_TITLE | PR_STATS);
                p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
                p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA | PR_TANK);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		prt_frame_basic();
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
                if (p_ptr->pclass == CLASS_ELEM_LORD)
                {
                        char s[80];
                        char e[80];
                        char f[80];
                        if (p_ptr->psex == SEX_FEMALE) sprintf(s, "Lady");
                        else sprintf(s, "Lord");
                        sprintf(e, get_element_name(p_ptr->elemlord));
                        sprintf(f, "%s %s", s, e);
                        prt_field(f, ROW_CLASS, COL_CLASS);
                }
                else prt_field(classes_def[p_ptr->pclass].name, ROW_CLASS, COL_CLASS);                
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

        if (p_ptr->redraw & (PR_TANK))
	{
                p_ptr->redraw &= ~(PR_TANK);
                prt_tp();
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
		c_put_str(TERM_L_GREEN, "                          ", 23, 0);
		prt_frame_extra();
	}

	if (p_ptr->redraw & (PR_CUT))
	{
		p_ptr->redraw &= ~(PR_CUT);
		prt_cut();
	}

	/* Now used for Power Attack */
	if (p_ptr->redraw & (PR_STUN))
	{
		p_ptr->redraw &= ~(PR_STUN);
		/*prt_stun();*/
		prt_powerattack();
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

	/*if (p_ptr->redraw & (PR_STUDY))
	{
		p_ptr->redraw &= ~(PR_STUDY);
		prt_study();
	}*/

	if (damages_counter_duration > 0)
	{
		char total_damages[80];
		c_put_str(TERM_L_GREEN, "                          ", 23, 64);
		sprintf(total_damages, "Dam: %ld", damages_counter);
		if (damages_counter_player_damages) c_put_str(TERM_L_GREEN, total_damages, 23, 64);
		else c_put_str(TERM_L_RED, total_damages, 23, 64);
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


bool beastmaster_whip()
{
        return FALSE;
}

int get_artifact_idx(int level)
{
        int count = 0, i;
        bool OK = FALSE;

        while(count < 1000)
        {
                artifact_type *a_ptr; 

                count++;
                i = randint(max_a_idx-1);
                a_ptr = &a_info[i];

                if(!a_ptr->tval) continue;
                if(a_ptr->cur_num) continue;
                if(a_ptr->level > level) continue;

                OK = TRUE;
                break;
        }
        if(OK == FALSE) i = 1; /* The Phial */

        return i;
}

bool safety_check()
{
        u32b f1, f2, f3, f4;
        int i;
        object_type *o_ptr;

        i = 24;
        while (i < INVEN_TOTAL)
        {
                /* Get the item */
                o_ptr = &inventory[i];

                /* Examine the item */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);

                /* Check for the SAFETY flag */
                if (o_ptr->k_idx && (f4 & (TR4_SAFETY)))
                {
                        return (TRUE);
                }

                i++;
        }
        /* Default */
        return (FALSE);
}        

/* Set the skills, used by lua. */
/* Once a lua code, it caused major performance issues. This one's much faster. */
void calc_skills(int mode)
{	
	int i;
	switch (mode)
	{
		case 0:
		{
			for (i = 0; i < SKILL_MAX; i++)
			{
				p_ptr->skill_bonus[i] = 0;
			}
			break;
		}
		case 1:
		{
			for (i = 0; i < SKILL_MAX; i++)
			{
				p_ptr->skill_bonus[i] += ((p_ptr->skill_base[i] * classes_def[p_ptr->pclass].skills_bonus[i]) / 100);
			}
			break;
		}
		case 2:
		{
			for (i = 0; i < SKILL_MAX; i++)
			{
				p_ptr->skill[i] = p_ptr->skill_base[i] + p_ptr->skill_bonus[i];
			}
			break;
		}
	}
}

/* Again, once a lua script, it was too slow. */
void calc_stats(int mode)
{
	int i;
	switch (mode)
	{
		case 0:
		{
			for (i = 0; i < 6; i++)
			{
				p_ptr->stat_add[i] = 0;
			}
			break;
		}
		case 1:
		{
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

				/* Temporary stats! */
        			if (p_ptr->str_boost_dur > 0 && i == 1)
				{
                			p_ptr->stat_ind[A_STR] += p_ptr->str_boost;
				}
        			if (p_ptr->int_boost_dur > 0 && i == 2)
				{
                			p_ptr->stat_ind[A_INT] += p_ptr->int_boost;
				}
        			if (p_ptr->wis_boost_dur > 0 && i == 3)
				{
                			p_ptr->stat_ind[A_WIS] += p_ptr->wis_boost;
				}
        			if (p_ptr->dex_boost_dur > 0 && i == 4)
				{
                			p_ptr->stat_ind[A_DEX] += p_ptr->dex_boost;
				}
        			if (p_ptr->con_boost_dur > 0 && i == 5)
				{
                			p_ptr->stat_ind[A_CON] += p_ptr->con_boost;
				}
        			if (p_ptr->chr_boost_dur > 0 && i == 6)
				{
                			p_ptr->stat_ind[A_CHR] += p_ptr->chr_boost;
				}
				/* Shadow Stalkers gets a dex boost from invisibility! */
        			if (p_ptr->tim_invisible)
				{
                			if (p_ptr->abilities[(CLASS_SHADOW * 10) + 7] >= 1)
                			{
                        			int statbonus = p_ptr->skill[6] / 4;
                        			statbonus = statbonus + (statbonus * ((p_ptr->abilities[(CLASS_SHADOW * 10) + 7] * 10) / 100));
                        			p_ptr->stat_ind[A_DEX] += statbonus;
                			}                                
				}
				/* You may be affected by a permanent mutation! */
				/* ... until you find a potion, that is! ;) */
				p_ptr->stat_ind[A_STR] += p_ptr->stat_mut[A_STR];
				p_ptr->stat_ind[A_INT] += p_ptr->stat_mut[A_INT];
				p_ptr->stat_ind[A_WIS] += p_ptr->stat_mut[A_WIS];
				p_ptr->stat_ind[A_DEX] += p_ptr->stat_mut[A_DEX];
				p_ptr->stat_ind[A_CON] += p_ptr->stat_mut[A_CON];
				p_ptr->stat_ind[A_CHR] += p_ptr->stat_mut[A_CHR];

				/* Stats will not go below 1. */
				if (p_ptr->stat_ind[A_STR] < 1) p_ptr->stat_ind[A_STR] = 1;
				if (p_ptr->stat_ind[A_INT] < 1) p_ptr->stat_ind[A_INT] = 1;
				if (p_ptr->stat_ind[A_WIS] < 1) p_ptr->stat_ind[A_WIS] = 1;
				if (p_ptr->stat_ind[A_DEX] < 1) p_ptr->stat_ind[A_DEX] = 1;
				if (p_ptr->stat_ind[A_CON] < 1) p_ptr->stat_ind[A_CON] = 1;
				if (p_ptr->stat_ind[A_CHR] < 1) p_ptr->stat_ind[A_CHR] = 1;
		
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
		
                		ind = use;

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
						p_ptr->update |= (PU_MANA | PU_SPELLS);
					}

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER);
				}
			}
			break;
		}
	}
}

/* Slowness is rarely appreciated... */
void calc_equipment()
{
	int i, w;
	object_type *o_ptr;
	u32b f1, f2, f3, f4;

	/* Reset two global variables. */
	exblows = 0;
	exshots = 0;

	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{

		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip disabled objects. */
		if (o_ptr->disabled > 0 || o_ptr->disabled == -1) continue;

		/* Extract the item flags */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);
		
		/* Stats bonus/penalities. */
		p_ptr->stat_add[A_STR] += o_ptr->statsbonus[A_STR];
		p_ptr->stat_add[A_INT] += o_ptr->statsbonus[A_INT];
		p_ptr->stat_add[A_WIS] += o_ptr->statsbonus[A_WIS];
		p_ptr->stat_add[A_DEX] += o_ptr->statsbonus[A_DEX];
		p_ptr->stat_add[A_CON] += o_ptr->statsbonus[A_CON];
		p_ptr->stat_add[A_CHR] += o_ptr->statsbonus[A_CHR];

		/* Skills bonus/penalities. */
		for (w = 0; w < SKILL_MAX; w++)
		{
			p_ptr->skill_bonus[w] += o_ptr->skillsbonus[w];
		}

		/* Various bonus. */
		p_ptr->to_s += o_ptr->spellbonus;
		p_ptr->see_infra += o_ptr->infravision;
		p_ptr->pspeed += o_ptr->speedbonus;
		exblows += o_ptr->extrablows;
		p_ptr->invis += o_ptr->invisibility;
		exshots += o_ptr->extrashots;

		/* Various flags */
		if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_XTRA_MIGHT)) p_ptr->xtra_might = TRUE;
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
		if (f3 & (TR3_LITE)) p_ptr->lite = TRUE;
		if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
		if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
		if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;
		if (f3 & (TR3_WRAITH)) p_ptr->wraith_form = 20;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
		if (f4 & (TR4_FLY)) p_ptr->fly = TRUE;
		if (f4 & (TR4_CLIMB)) p_ptr->climb = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_FEAR)) p_ptr->resist_fear = TRUE;
		if (f2 & (TR2_RES_CONF)) p_ptr->resist_conf = TRUE;
		if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
		if (o_ptr->reflect > 0) p_ptr->reflect = TRUE;
		if (f3 & (TR3_SH_FIRE)) p_ptr->sh_fire = TRUE;
		if (f3 & (TR3_SH_ELEC)) p_ptr->sh_elec = TRUE;

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

		/* to_h and to_d */
		p_ptr->to_h += o_ptr->to_h;
		p_ptr->to_d += o_ptr->to_d;
		p_ptr->dis_to_h += o_ptr->to_h;
		p_ptr->dis_to_d += o_ptr->to_d;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class */
		p_ptr->dis_to_a += o_ptr->to_a;
	}

}

/* Calc resistances. */
void calc_resistances(int mode)
{
	int i;
	int j;

	object_type *o_ptr;
	monster_race *r_ptr;

	switch (mode)
	{
		case 0:
		{
			for (i = 0; i < MAX_RESIST; i++)
			{
				p_ptr->resistances[i] = 0;
			}
			break;
		}
		case 1:
		{
			/* Resistances from items. */
        		i = 24;
        		while (i < INVEN_TOTAL)
			{
				o_ptr = &inventory[i];

                		/* Augment resistances! */
				if (o_ptr->k_idx > 0)
				{
					for (j = 0; j < MAX_RESIST; j++)
					{
						p_ptr->resistances[j] += o_ptr->resistances[j];
					}
				}
                		i += 1;
        		}

			/* Resistances from monster body. */
			if (p_ptr->body_monster != 0)
			{
				r_ptr = &r_info[p_ptr->body_monster];
				for (j = 0; j < MAX_RESIST; j++)
				{
					p_ptr->resistances[j] += r_ptr->resistances[j];
				}
			}

			/* Cannot exceed 100. */
			for (j = 0; j < MAX_RESIST; j++)
			{
				if (p_ptr->resistances[j] > 100) p_ptr->resistances[j] = 100;
			}

			break;
		}
	}
}

/* Customized update function. */
void update_and_handle(void)
{
        cptr str;
	rp_ptr = &race_info[p_ptr->prace];
	str = rp_ptr->title;
	cp_ptr = &class_info[p_ptr->pclass];
	str = classes_def[p_ptr->pclass].name;
        p_ptr->update |= (PU_BONUS);
        p_ptr->update |= (PU_TORCH);
        p_ptr->update |= (PU_HP);
        p_ptr->update |= (PU_MANA);
        p_ptr->update |= (PU_SPELLS);
        p_ptr->update |= (PU_VIEW);
        p_ptr->update |= (PU_LITE);
        p_ptr->update |= (PU_FLOW);
        p_ptr->update |= (PU_BODY);
        p_ptr->redraw |= (PR_MANA);
        p_ptr->redraw |= (PR_HP);
        p_ptr->redraw |= (PR_GOLD);
        p_ptr->redraw |= (PR_STATS);
        p_ptr->redraw |= (PR_BASIC);
        p_ptr->redraw |= (PR_EXTRA);
	p_ptr->window |= (PW_PLAYER);
        handle_stuff();
        update_stuff();
        redraw_stuff();
}

/* Just how cursed you are? */
void calc_cursed()
{
	int i;
	object_type *o_ptr;

	p_ptr->cursed = 0;

	/* Scan the usable inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{

		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Adds the total curse. */
		p_ptr->cursed += o_ptr->cursed;
	}

}
