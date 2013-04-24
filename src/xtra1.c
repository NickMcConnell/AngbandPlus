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
void cnv_stat(int val, char *out_val)
{
	/* Above 18 */
	if (val > 999)
	{
		val = 999;
	
		sprintf(out_val, "   %3d", val);
	}
	/* From 3 to 18 */
	else
	{
		sprintf(out_val, "   %3d", val);
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
 /* Have to correct this function for new stat system. -CCC */
 /* I don't even know if these needs to be done at all, 
  * Its seems as though we could just get by by yanking 
  * the function completely - making all stats integers. 
  */
 
int modify_stat_value(int value, int amount)
{
	int i;

	/* Reward */
	if (amount > 0)
	{
		/* Apply each point */
		for (i = 0; i < amount; i++)
		{
			/* Ten "points" at a time */
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
			value -= 10;
		}
	}

	/* make sure no negative stats */
	if (value < 1) value = 1;

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
		if ((stat == A_EGO) || (stat == A_SCH))
		{
			if (p_ptr->tim_demonspell && p_ptr->tim_voorish)
				c_put_str(TERM_VIOLET, stat_names_reduced[stat], ROW_STAT + stat, 0);
			else if ((p_ptr->tim_demonspell) || (p_ptr->tim_voorish))
				c_put_str(TERM_RED, stat_names_reduced[stat], ROW_STAT + stat, 0);
			else put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		}
		if ((stat == A_MUS) || (stat == A_AGI) || (stat == A_VIG))
		{	
			if (p_ptr->tim_stygian)
				c_put_str(TERM_RED, stat_names_reduced[stat], ROW_STAT + stat, 0);
			else put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		}
		if (stat == A_CHR) put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_YELLOW, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Display "healthy" stat */
	else
	{
		if ((stat == A_EGO) || (stat == A_SCH))
		{
			if (p_ptr->tim_demonspell && p_ptr->tim_voorish)
				c_put_str(TERM_VIOLET, stat_names[stat], ROW_STAT + stat, 0);
			else if ((p_ptr->tim_demonspell) || (p_ptr->tim_voorish))
				c_put_str(TERM_RED, stat_names[stat], ROW_STAT + stat, 0);
			else put_str(stat_names[stat], ROW_STAT + stat, 0);
		}
		if ((stat == A_MUS) || (stat == A_AGI) || (stat == A_VIG))
		{	
			if (p_ptr->tim_stygian)
				c_put_str(TERM_RED, stat_names[stat], ROW_STAT + stat, 0);
			else put_str(stat_names[stat], ROW_STAT + stat, 0);
		}
		if (stat == A_CHR) put_str(stat_names[stat], ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 700)
	{
		put_str("!", ROW_STAT + stat, 3);
	}
}



/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_class(void)
{
	cptr p;

	/* Normal */
	/* Display Proper "Genderized" Class Titles" */
	if (p_ptr->psex == SEX_FEMALE)
	{
		p = c_text + cp_ptr->ftitle[(p_ptr->lev - 1) / 5];
	}
	else
	{
		p = c_text + cp_ptr->mtitle[(p_ptr->lev - 1) / 5];
	}

	prt_field(p, ROW_CLASS, COL_CLASS);
}

/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	char p[16];

	/* Wizard */
	if (p_ptr->wizard)
	{
		/* p = "[=-WIZARD-=]"; */
		/* Save the player name */
		strcpy(p, "[=-WIZARD-=]");
	}

	/* Winner */
	else if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		/* p = "***WINNER***"; */
		/* Save the player name */
		strcpy(p, "***WINNER***");
	}
	
	else 	
	{
		/* Save the player name */
		strcpy(p, op_ptr->full_name);
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

	if (!op_ptr->opt[OPT_exp_need])
	{
		(void)sprintf(out_val, "%8ld", (long)p_ptr->exp);
	}
	else
	{
		if (p_ptr->lev < PY_MAX_LEVEL)
		{
			long val = (long)(((player_exp[p_ptr->lev - 1] * p_ptr->expfact) / 100L) - p_ptr->exp);
	
			/* Boundary check */
			if (val < 0) val = 0;
	
			sprintf(out_val, "%7ld", val);
		}
		else
		{
			sprintf(out_val, "******* ");
		}
	}



	if (p_ptr->exp >= p_ptr->max_exp)
	{
                put_str((op_ptr->opt[OPT_exp_need] ? "NEED " : "EXP "), ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 4 + op_ptr->opt[OPT_exp_need]);
	}
	else
	{
                put_str((op_ptr->opt[OPT_exp_need] ? "Need " : "Exp "), ROW_EXP, 0);
		c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 4 + op_ptr->opt[OPT_exp_need]);
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
	
	/* This is temporary */
	if ((p_ptr->tim_wraith) && (p_ptr->invisiblity)) c_put_str(TERM_DARK, "Cur AC ", ROW_AC, COL_AC);
	else if (p_ptr->tim_wraith) c_put_str(TERM_SLATE, "Cur AC ", ROW_AC, COL_AC);
	else if (p_ptr->invisiblity) c_put_str(TERM_L_DARK, "Cur AC ", ROW_AC, COL_AC);
	else c_put_str(TERM_WHITE, "Cur AC ", ROW_AC, COL_AC);
	
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);
}


/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	char tmp[32];

	byte color, offset;

	if (p_ptr->tim_demonhealth) c_put_str(TERM_RED, "HP  ", ROW_HP, COL_HP);
	else put_str("HP  ", ROW_HP, COL_HP);
	sprintf(tmp, "%4d", p_ptr->chp);

	if (p_ptr->mhp > 999) offset = 3;
	else if (p_ptr->mhp > 99) offset = 4;
	else if (p_ptr->mhp > 9) offset = 5;
	else offset = 6;

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

	c_put_str(color, tmp, ROW_HP, COL_HP + offset);

	sprintf(tmp, "/%d", p_ptr->mhp);
	c_put_str(TERM_L_GREEN, tmp, ROW_HP, COL_HP + 4 + offset);
}

/*
 * Prints Cur/Max wound points
 */
static void prt_wp(void)
{
	char tmp[32];

	byte color, offset;

	if (p_ptr->tim_demonhealth)	c_put_str(TERM_RED, "WP  ", ROW_WP, COL_WP);
	else put_str("WP  ", ROW_WP, COL_WP);
	sprintf(tmp, "%4d", p_ptr->cwp);

	if (p_ptr->mwp > 999) offset = 3;
	else if (p_ptr->mwp > 99) offset = 4;
	else if (p_ptr->mwp > 9) offset = 5;
	else offset = 6;

	if (p_ptr->cwp >= p_ptr->mwp)
	{
		color = TERM_L_GREEN;
	}
	else if (p_ptr->cwp > (p_ptr->mwp * op_ptr->hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	c_put_str(color, tmp, ROW_WP, COL_WP + offset);

	sprintf(tmp, "/%d", p_ptr->mwp);
	c_put_str(TERM_L_GREEN, tmp, ROW_WP, COL_WP + 4 + offset);
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
	char tmp[32];
	byte color, offset;


	/* Do not show mana unless it matters */
	if (!cp_ptr->spell_book) return;

	
	if (p_ptr->tim_demonspell && p_ptr->tim_voorish)
		c_put_str(TERM_VIOLET, "SP  ", ROW_SP, COL_SP);
	else if ((p_ptr->tim_demonspell) || (p_ptr->tim_voorish)) 
		c_put_str(TERM_RED, "SP  ", ROW_SP, COL_SP);
	else put_str("SP  ", ROW_SP, COL_SP);
	sprintf(tmp, "%4d", p_ptr->csp);

	if (p_ptr->msp > 999) offset = 3;
	else if (p_ptr->msp > 99) offset = 4;
	else if (p_ptr->msp > 9) offset = 5;
	else offset = 6;

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

	c_put_str(color, tmp, ROW_SP, COL_SP + offset);

	sprintf(tmp, "/%d", p_ptr->msp);
	c_put_str(TERM_L_GREEN, tmp, ROW_SP, COL_SP + 4 + offset);
}

static void prt_effects(void)
{
	if (p_ptr->tim_wormsense) c_put_str(TERM_UMBER, "  Wormsense", ROW_GOLD+1, COL_GOLD);
	else put_str("            ", ROW_GOLD+1, COL_GOLD);

	if (p_ptr->blessed) c_put_str(TERM_BLUE, "Bls", ROW_EFFECT1, COL_EFFECT+1);
	else put_str("   ", ROW_EFFECT1, COL_EFFECT+1);
	if (p_ptr->hero) c_put_str(TERM_GREEN, "Hr", ROW_EFFECT1, COL_EFFECT+4);
	else put_str("  ", ROW_EFFECT1, COL_EFFECT+4);
	if (p_ptr->shero) c_put_str(TERM_L_BLUE, "Sh", ROW_EFFECT1, COL_EFFECT+6);
	else put_str("  ", ROW_EFFECT1, COL_EFFECT+6);
	if (p_ptr->protevil) c_put_str(TERM_RED, "PtE", ROW_EFFECT1, COL_EFFECT+8);
	else put_str("   ", ROW_EFFECT1, COL_EFFECT+8);
	
#if 0
	if (p_ptr->telepathy) c_put_str(TERM_VIOLET, "Esp", ROW_EFFECT2, COL_EFFECT+1);
	else put_str("   ", ROW_EFFECT2, COL_EFFECT+1);
	if (p_ptr->see_inv) c_put_str(TERM_L_BLUE, "SI", ROW_EFFECT2, COL_EFFECT+4);
	else put_str("  ", ROW_EFFECT2, COL_EFFECT+4);
	if (p_ptr->free_act) c_put_str(TERM_GREEN, "Fa", ROW_EFFECT2, COL_EFFECT+6);
	else put_str("  ", ROW_EFFECT2, COL_EFFECT+6);
	if (p_ptr->see_infra) c_put_str(TERM_RED, "Inf", ROW_EFFECT2, COL_EFFECT+8);
	else put_str("   ", ROW_EFFECT2, COL_EFFECT+8);
#endif
	
	if (p_ptr->no_teleport && p_ptr->anti_magic)
		c_put_str(TERM_L_DARK, " Null Zone  ", ROW_ANTI, COL_EFFECT);
	else if (p_ptr->no_teleport)
		c_put_str(TERM_SLATE,  " No Teleport", ROW_ANTI, COL_EFFECT);
	else if (p_ptr->anti_magic)
		c_put_str(TERM_SLATE,  " Anti-magic ", ROW_ANTI, COL_EFFECT);	
	else 
		put_str("            ", ROW_ANTI, COL_EFFECT);

}

/*
 * Prints depth in stat area - Rewritten for Steamband by DH 
 */
static void prt_depth(void)
{
	char depths[32];
	int attr = TERM_WHITE;
	
	if (!p_ptr->depth)
	{
		/* Town is center of earth- 5050ft or dlvl 100a - lvl "100" being the "1st" level */
		strcpy(depths, "Town");
	}
	else if (depth_in_feet)
	{
		/* should probably have a max dlvl check here */
		/* check_max_dlvl */
		if (p_ptr->depth > 50)
		{
			/* revert to incremental ft since lvl 101 is surface or 0ft */
			/* first check to see if at surface aka Fu's Lab gnd floor */
			if (p_ptr->depth == 51)
			{
				sprintf(depths,"Surface");
			}
			else
			{
				/*revert to incremental ft */ 
				/* Steam 50' is Angband 5100 or lvl 102 */
				sprintf(depths, "%d ft", (p_ptr->depth-51) * 50);
			}
		}
		else
		{
			sprintf(depths, "%d ft", 2550-(p_ptr->depth * 50));
		}
	}
	else
	{
		if (p_ptr->depth == 51)
		{
			sprintf(depths,"Surface");
		}
		else if (p_ptr->depth > 50)
		{
			/* revert to incremental lvls as lvl 101 is "surface" thus lvl 1 */
			sprintf(depths, "Lab Lv %d", p_ptr->depth-51);
		}
		else
		{
			sprintf(depths, "Lev %d", 51-p_ptr->depth);
		}
	}

	/* Get color of level based on feeling  -JSV- */
	if (p_ptr->depth)
	{
		if (feeling ==  1) attr = TERM_VIOLET;
		if (feeling ==  2) attr = TERM_RED;
		if (feeling ==  3) attr = TERM_L_RED;
		if (feeling ==  4) attr = TERM_ORANGE;
		if (feeling ==  5) attr = TERM_ORANGE;
		if (feeling ==  6) attr = TERM_YELLOW;
		if (feeling ==  7) attr = TERM_YELLOW;
		if (feeling ==  8) attr = TERM_WHITE;
		if (feeling ==  9) attr = TERM_WHITE;
		if (feeling == 10) attr = TERM_L_WHITE;
	}

	/* Right-Adjust the "depth", and clear old values */
	c_prt(attr, format("%7s", depths), ROW_DEPTH, COL_DEPTH);
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Hot!  ", ROW_HUNGRY, COL_HUNGRY);
		}
		else c_put_str(TERM_RED, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Hot!  ", ROW_HUNGRY, COL_HUNGRY);
		}
		else c_put_str(TERM_RED, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_YELLOW, "Warm  ", ROW_HUNGRY, COL_HUNGRY);
		}
		else c_put_str(TERM_YELLOW, "Hungry", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		c_put_str(TERM_L_GREEN, "      ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_L_GREEN, "Oiled ", ROW_HUNGRY, COL_HUNGRY);
		}
		else c_put_str(TERM_L_GREEN, "Full  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Gorged */
	else
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_GREEN, "Slick  ", ROW_HUNGRY, COL_HUNGRY);
		}
		else c_put_str(TERM_GREEN, "Gorged", ROW_HUNGRY, COL_HUNGRY);
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

/* now this will be the display that tells you you gained a level */
static void prt_level_g(void)
{
	byte attr = TERM_WHITE;
	if (p_ptr->free_sgain >= 1 || p_ptr->free_skpts >= 1)
	{
		attr = TERM_L_GREEN;
		c_put_str(attr, "(G) Lv", ROW_LEVEL_G, COL_LEVEL_G);
	}
	else
	{
		put_str("       ", ROW_LEVEL_G, COL_LEVEL_G);
	}
}


static void prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Core failure", ROW_CUT, COL_CUT);
		}
		else c_put_str(TERM_L_RED, "Mortal wound", ROW_CUT, COL_CUT);
	}
	else if (c > 200)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Core leak   ", ROW_CUT, COL_CUT);
		}
		else c_put_str(TERM_RED, "Deep gash   ", ROW_CUT, COL_CUT);
	}
	else if (c > 100)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Severe leak ", ROW_CUT, COL_CUT);
		}
		else c_put_str(TERM_RED, "Severe cut  ", ROW_CUT, COL_CUT);
	}
	else if (c > 50)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Nasty leak  ", ROW_CUT, COL_CUT);
		}
		else c_put_str(TERM_ORANGE, "Nasty cut   ", ROW_CUT, COL_CUT);
	}
	else if (c > 25)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Bad leak    ", ROW_CUT, COL_CUT);
		}
		else c_put_str(TERM_ORANGE, "Bad cut     ", ROW_CUT, COL_CUT);
	}
	else if (c > 10)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Leak        ", ROW_CUT, COL_CUT);
		}
		else c_put_str(TERM_YELLOW, "Light cut   ", ROW_CUT, COL_CUT);
	}
	else if (c)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
		{
			c_put_str(TERM_RED, "Drip        ", ROW_CUT, COL_CUT);
		}
		else c_put_str(TERM_YELLOW, "Graze       ", ROW_CUT, COL_CUT);
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

	/* Tracking a dead monster (?) */
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

		/* Confused */
		if (m_ptr->confused) attr = TERM_UMBER;

		/* Stunned */
		if (m_ptr->stunned) attr = TERM_L_BLUE;

		/* Asleep */
		if (m_ptr->csleep) attr = TERM_BLUE;

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
static void prt_frame_basic(void)
{
	int i;

	/* Race and Class */
	prt_field(p_name + rp_ptr->name, ROW_RACE, COL_RACE);

	/* Class */
	prt_class();

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();

	/* All Stats */
	for (i = 0; i < A_MAX; i++) prt_stat(i);

	/* Armor */
	prt_ac();

	/* Hitpoints */
	prt_hp();
	
	/* Wound points */
	prt_wp();

	/* Spellpoints */
	prt_sp();
	
	/* 2 rows of magic effects */
	prt_effects();

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
	
	/* Effects */
	prt_effects();

	/* can gain a level */
	prt_level_g();
}


/*
 * Hack -- display inventory in sub-windows
 */
static void fix_inven(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_INVEN))) continue;

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
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_EQUIP))) continue;

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
static void fix_player_0(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_PLAYER_0))) continue;

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
static void fix_player_1(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_PLAYER_1))) continue;

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
		if (!(op_ptr->window_flag[j] & (PW_MESSAGE))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Get size */
		Term_get_size(&w, &h);

		/* Dump messages */
		for (i = 0; i < h; i++)
		{
			byte color = message_color((s16b)i);

			/* Dump the message on the appropriate line */
			Term_putstr(0, (h - 1) - i, -1, color, message_str((s16b)i));

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
 * Hack -- display overhead view in sub-windows.
 *
 * This is most useful on a fast machine with the "center_player" option set,
 * which induces a call to this function every time the player moves.  With
 * the "center_player" option not set, this function is only called when the
 * panel changes.
 *
 * The "display_map()" function handles NULL arguments in a special manner.
 */
static void fix_overhead(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_OVERHEAD))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Redraw map */
		display_map(NULL, NULL);

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
		if (!(op_ptr->window_flag[j] & (PW_MONSTER))) continue;

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
                if (!(op_ptr->window_flag[j] & (PW_VISIBLE))) continue;

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
		if (!(op_ptr->window_flag[j] & (PW_OBJECT))) continue;

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
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
 
static void calc_mana(void)
{
	int msp, levels, cur_wgt, max_wgt;

	object_type *o_ptr;

	u32b pf1, pf2, pf3;
	player_flags(&pf1, &pf2, &pf3);

	levels = 0;

	/* Hack -- Must be literate */
	if (!cp_ptr->spell_book) return;

	levels = p_ptr->lev + 1;
	
	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana */
	/* @STAT@ */
	msp = ((p_ptr->stat_use[cp_ptr->spell_stat] / 80) * (levels));

	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Take mana bonus into account */
	msp += p_ptr->mana_bonus;
	
	/* Add in mana boosting skills */
	if (p_ptr->skills[SK_SPIRIT_BATTERY].skill_rank > 0)
		msp += ((p_ptr->skills[SK_SPIRIT_BATTERY].skill_rank * 3) / 2);
	if (p_ptr->skills[SK_EARTH_HEARTH].skill_rank > 0)
		msp += ((p_ptr->skills[SK_EARTH_HEARTH].skill_rank * 3) / 2);

	/* Process gloves for those disturbed by them */
	if (cp_ptr->flags & CF_CUMBER_GLOVE)
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
		    (get_object_pval(o_ptr, TR_PVAL_AGI) <= 0))
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
	if (!(pf3 & (TR3_AUTOMATA))) cur_wgt += inventory[INVEN_BODY].weight;
	if (!(pf3 & (TR3_AUTOMATA))) cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_LEG].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	if (!(pf3 & (TR3_AUTOMATA))) cur_wgt += inventory[INVEN_HANDS].weight;
	if (!(pf3 & (TR3_AUTOMATA))) cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = cp_ptr->spell_weight;

	/* Heavy armor penalizes mana */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Reduce mana */
		msp -= ((cur_wgt - max_wgt) / 10);
	}


	/* Mana can never be negative */
	if (msp < 0) msp = 0;


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
 *
 * Adjust current hitpoints if necessary
 * 
 * This function also handles adjusting wound points - it's not a
 * seperate function because woundpoints are checked at all the same 
 * times that hit points would be, and to insure no missed cases,
 * treat wp total as a second hit point total in the code. - CCC
 */
static void calc_hitpoints(void)
{
	int bonus, mhp;
	int woundbonus, mwp, racial;
	int toughness, toughnesswill, fortitude, ironbody, trailblazer, mentalresistance;
	int battleendurance;
	
	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = (p_ptr->stat_use[A_VIG] / 35);
	woundbonus = (p_ptr->stat_use[A_VIG] / 20);
	
	/* paranoia */
	toughness = mentalresistance = toughnesswill = fortitude = ironbody = trailblazer = 0;
	battleendurance = racial = 0;
	
	/* retrive skills */
	if (p_ptr->skills[SK_TOUGHNESS_WILL].skill_max > 0)
		toughnesswill = p_ptr->skills[SK_TOUGHNESS_WILL].skill_rank;
	if (p_ptr->skills[SK_MENTAL_RESISTANCE].skill_max > 0)
		mentalresistance = p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank;
	if (p_ptr->skills[SK_TOUGHNESS].skill_max > 0)
		toughness = p_ptr->skills[SK_TOUGHNESS].skill_rank;
	if (p_ptr->skills[SK_FORTITUDE].skill_max > 0)
		fortitude = p_ptr->skills[SK_FORTITUDE].skill_rank;
	if (p_ptr->skills[SK_IRON_BODY].skill_max > 0)
		ironbody = p_ptr->skills[SK_IRON_BODY].skill_rank;
	if (p_ptr->skills[SK_TRAILBLAZER].skill_max > 0)
		trailblazer = p_ptr->skills[SK_TRAILBLAZER].skill_rank;
	if (p_ptr->skills[SK_BATTLE_ENDURANCE].skill_max > 0)
		battleendurance = p_ptr->skills[SK_BATTLE_ENDURANCE].skill_rank;
		
	/* racial bonuses to wound points */
	if (p_ptr->prace == RACE_RUSSIAN) racial = p_ptr->lev / 5;
	if (p_ptr->prace == RACE_DWARF) racial = p_ptr->lev / 3;
	if (p_ptr->prace == RACE_AUTOMATA) racial = p_ptr->lev;
	if (p_ptr->prace == RACE_STEAM_MECHA) racial = p_ptr->lev * 3;
	if (p_ptr->prace == RACE_RAKSHASA) racial = p_ptr->lev;
	if (p_ptr->prace == RACE_GIANT) racial = p_ptr->lev * 2;
	if (p_ptr->prace == RACE_OGRE) racial = p_ptr->lev * 3 / 2;
	if (p_ptr->prace == RACE_OLD_ONE) racial = p_ptr->lev * 10;
	if (p_ptr->prace == RACE_GOBLIN) racial = -(p_ptr->stat_use[A_VIG] / 40);
	if (p_ptr->prace == RACE_GHOST) racial = -(p_ptr->stat_use[A_VIG] / 20);
	if ((p_ptr->prace == RACE_BROWNIE) ||
		(p_ptr->prace == RACE_SEELIE_FAE) ||
		(p_ptr->prace == RACE_UNSEELIE_FAE)) racial = -(p_ptr->stat_use[A_VIG] / 60);
	
	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 2);
	
	mwp = 1 + woundbonus + (p_ptr->lev / 2) + toughness + toughnesswill +
					(mentalresistance / 4) + fortitude + (ironbody / 2) + 
					((trailblazer + 1) / 2) + (battleendurance / 2) + racial;
	
	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;
	if (mwp < 0) mwp = 1;
	
	/* Take health bonus into account */
	mhp += p_ptr->health_bonus;
	mwp += p_ptr->health_bonus / 5;
	
	/* New maximum hitpoints */
	if ((p_ptr->mhp != mhp) || (p_ptr->mwp != mwp))
	{
		/* Save new limit */
		p_ptr->mhp = mhp;
		p_ptr->mwp = mwp;

		/* Enforce new limit */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}
		
		if (p_ptr->cwp >= mwp)
		{
			p_ptr->cwp = mwp;
			p_ptr->cwp_frac = 0;
		}

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
	
}

/*
 * Calculate and set the current light radius.
 *
 * In Sangband, a glowing character increases any light radius, as do
 * various special conditions.  The brightest wielded object counts as
 * the light source; they are not cumulative.  The maximum light radius
 * is 5 (see cave.c).
 */
static void calc_torch(void)
{
	int i, light;

	/* Save old light */
	s16b old_lite = p_ptr->cur_lite;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Character is glowing */
	if (p_ptr->lite) p_ptr->cur_lite += 1;

	/* seelie fae have intrinsic light */
	else if (p_ptr->prace == RACE_SEELIE_FAE)  p_ptr->cur_lite += 1;

#if 0
	/* Character has a very high piety */
	if (get_skill(S_PIETY, 0, 100) >= LEV_REQ_XTRA_LIGHT)
	{
		p_ptr->cur_lite += 1;
	}

	/* Character has an aura of holiness (not cumulative with piety) */
	else if (p_ptr->holy)
	{
		p_ptr->cur_lite += 1;
	}

	/* Character has reduced light */
	if (p_ptr->drain_light)
	{
		p_ptr->cur_lite -= 1;
	}
#endif
	/* Examine all wielded objects, use the brightest */
	/* Possibly allow a flag to add light? */
	for (light = 0, i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		int tmp = get_object_pval(o_ptr, TR_PVAL_LIGHT);

		u32b f1, f2, f3;

		/* Get object flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Light sources require fuel */
		if ((o_ptr->tval == TV_LITE) && (!(f3 & (TR3_NO_FUEL))))
		{
			/* No fuel */
			if (!o_ptr->pval) tmp = 0;

			/* Torches with low fuel burn more dimly */
			if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
			{
				if ((o_ptr->pval <= FUEL_LAMP_TWO) && (tmp > 1)) tmp -= 1;
				if ((o_ptr->pval <= FUEL_LAMP_ONE) && (tmp > 1)) tmp -= 1;
			}
			if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
			{
				if ((o_ptr->pval <= FUEL_TORCH_ONE) && (tmp > 1)) tmp -= 1;
			}
		}
		if (tmp > light) light = tmp;
	}

	/* Adjust light */
	p_ptr->cur_lite += light;

	/* Maximum light radius is 5 */
	if (p_ptr->cur_lite > 7) p_ptr->cur_lite = 7;

	/* Notice changes in the light radius */
	if (old_lite != p_ptr->cur_lite)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}
}


/*
 * Computes current weight limit.
 */
static int weight_limit(void)
{
	int i;
	int statval;

	/* Weight limit based only on strength */
	/* @STAT@ */
	statval = p_ptr->stat_use[A_MUS];
	
	/* Must be higher than 20, otherwise i becomes 0 */
	/* This should allow a character at minimum to carry at least 60 pounds */
	/* which is equivalent to an ey character with a str of 2. (if your strength is at */
	/* 80, you're a weakling anyway. You should be slowed by -1 or -2 for carrying */
	/* that much around*/
	if (statval < 25) statval = 25;
	
	i = 900 + ((statval / 25) * 100);

	/* Return the result */
	return (i);
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


	int old_dis_ac;
	int old_dis_to_a;
	
	int old_stealth;

	int extra_blows;
	int extra_shots;
	int extra_might;

	int old_stat_top[A_MAX];
	int old_stat_use[A_MAX];
	
	int res, dis_res;
	
	int old_telepathy;
	int old_see_inv;
	int old_wormsense;
	int old_free_act;
	int old_infra;
	int old_notele;
	int old_nomagic;
	int old_invisiblity;
	
	object_type *o_ptr;

	u32b f1, f2, f3;


	/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old stealth */
	old_stealth = p_ptr->skill_stl;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;
	old_wormsense = p_ptr->wormsense;
	old_free_act = p_ptr->free_act;
	old_infra = p_ptr->see_infra;
	old_notele = p_ptr->no_teleport;
	old_nomagic = p_ptr->anti_magic;
	old_invisiblity = p_ptr->invisiblity;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	/* Save the old stats */
	for (i = 0; i < A_MAX; i++)
	{
		old_stat_top[i] = p_ptr->stat_top[i];
		old_stat_use[i] = p_ptr->stat_use[i];
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
	for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Clear all the flags */
	p_ptr->wormsense = FALSE;
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->no_teleport = FALSE;
	p_ptr->no_magic = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->hp_drain = FALSE;
	p_ptr->sp_drain = FALSE;
	p_ptr->item_drain = FALSE;
	p_ptr->spell_disrupt = FALSE;
	p_ptr->anti_magic = FALSE;
	p_ptr->wraith_form = FALSE;
	p_ptr->invisiblity = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate_25 = FALSE;
	p_ptr->regenerate_50 = FALSE;
	p_ptr->regenerate_75 = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->sh_fire = FALSE;
	p_ptr->sh_elec = FALSE;
	p_ptr->sh_spine = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->lite = FALSE;
	p_ptr->sustain_mus = FALSE;
	p_ptr->sustain_agi = FALSE;
	p_ptr->sustain_vig = FALSE;
	p_ptr->sustain_sch = FALSE;
	p_ptr->sustain_ego = FALSE;
	p_ptr->sustain_chr = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_confu = FALSE;

	/* Clear the resists */
	for (i = 0; i < RS_MAX; ++i)
	{
		p_ptr->res[i] = 0;
		p_ptr->dis_res[i] = 0;
	}
	
	/* Clear the Mana bonus */
	p_ptr->mana_bonus = 0;

	/* Clear the Health bonus */
	p_ptr->health_bonus = 0;

	
	/*** Extract race/class info ***/

	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra;

	/* Base skill -- saving throw */
	p_ptr->skill_sav = p_ptr->lev/2;

	/* ANY SKILLS THAT ARE ALTERED BY MUTATIONS (or anywhere in the code) NEED */
	/* TO BE RESET HERE TO THEIR MAX, OTHERWISE THE INCREASE IS PERMANANT. */
	/* Note that increasing skills in mutations does not alter the skill display */
	/* Base skill -- disarming */
	p_ptr->skills[SK_DISARM_GOOD].skill_rank = p_ptr->skills[SK_DISARM_GOOD].skill_max;
	p_ptr->skills[SK_DISARM_NORM].skill_rank = p_ptr->skills[SK_DISARM_NORM].skill_max;
	p_ptr->skills[SK_DISARM_POOR].skill_rank = p_ptr->skills[SK_DISARM_POOR].skill_max;

	/* Base skill -- magic devices */
	p_ptr->skills[SK_DEVICE_GOOD].skill_rank = p_ptr->skills[SK_DEVICE_GOOD].skill_max;
	p_ptr->skills[SK_DEVICE_NORM].skill_rank = p_ptr->skills[SK_DEVICE_NORM].skill_max;
	p_ptr->skills[SK_DEVICE_POOR].skill_rank = p_ptr->skills[SK_DEVICE_POOR].skill_max;

	/* Base skill -- stealth */
	p_ptr->skills[SK_STEALTH_GOOD].skill_rank = p_ptr->skills[SK_STEALTH_GOOD].skill_max;
	p_ptr->skills[SK_STEALTH_NORM].skill_rank = p_ptr->skills[SK_STEALTH_NORM].skill_max;
	p_ptr->skills[SK_STEALTH_POOR].skill_rank = p_ptr->skills[SK_STEALTH_POOR].skill_max;

	/* Base skill -- searching ability */
	p_ptr->skills[SK_SEARCHING_GOOD].skill_rank = p_ptr->skills[SK_SEARCHING_GOOD].skill_max;
	p_ptr->skills[SK_SEARCHING_NORM].skill_rank = p_ptr->skills[SK_SEARCHING_NORM].skill_max;
	p_ptr->skills[SK_SEARCHING_POOR].skill_rank = p_ptr->skills[SK_SEARCHING_POOR].skill_max;

	/* Base skill -- combat (normal) */
	p_ptr->skills[SK_TOHIT].skill_rank = p_ptr->skills[SK_TOHIT].skill_max;

	/* Base skill -- combat (shooting) */
	p_ptr->skills[SK_TOHIT_SHOOTING].skill_rank = p_ptr->skills[SK_TOHIT_SHOOTING].skill_max;

	/* Base skill -- combat (throwing) */
	p_ptr->skills[SK_TOHIT_THROWING].skill_rank = p_ptr->skills[SK_TOHIT_THROWING].skill_max;

	/* Base skill -- digging */
	p_ptr->skill_dig = p_ptr->lev/2 + 1;	
	
	/* Base skill -- spell skills */
	p_ptr->skills[SK_LATIN].skill_rank = p_ptr->skills[SK_LATIN].skill_max;
	p_ptr->skills[SK_THAUMIC_ENERGY].skill_rank = p_ptr->skills[SK_THAUMIC_ENERGY].skill_max;
	p_ptr->skills[SK_LESSER_WARD].skill_rank = p_ptr->skills[SK_LESSER_WARD].skill_max;
	p_ptr->skills[SK_RITUAL_MAGIC].skill_rank = p_ptr->skills[SK_RITUAL_MAGIC].skill_max;
	p_ptr->skills[SK_TMPR_WILL].skill_rank = p_ptr->skills[SK_TMPR_WILL].skill_max;
	
	/* Have to Mirror this information in files.c */
	/*** Get base resistances ***/
	if (p_ptr->skills[SK_BASIC_SURVIVAL].skill_max > 0)
	{
		p_ptr->res[RS_FIR] = p_ptr->dis_res[RS_FIR] += p_ptr->skills[SK_BASIC_SURVIVAL].skill_rank * 2;
		p_ptr->res[RS_EAR] = p_ptr->dis_res[RS_EAR] += p_ptr->skills[SK_BASIC_SURVIVAL].skill_rank * 2;
		p_ptr->res[RS_AIR] = p_ptr->dis_res[RS_AIR] += p_ptr->skills[SK_BASIC_SURVIVAL].skill_rank * 2;
		p_ptr->res[RS_WTR] = p_ptr->dis_res[RS_WTR] += p_ptr->skills[SK_BASIC_SURVIVAL].skill_rank * 2;
	}
	if (p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_max > 0)
	{
		p_ptr->res[RS_FIR] = p_ptr->dis_res[RS_FIR] += (p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank + 1) / 2;
		p_ptr->res[RS_EAR] = p_ptr->dis_res[RS_EAR] += (p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank + 1) / 2;
		p_ptr->res[RS_AIR] = p_ptr->dis_res[RS_AIR] += (p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank + 1) / 2;
		p_ptr->res[RS_WTR] = p_ptr->dis_res[RS_WTR] += (p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank + 1) / 2;
		p_ptr->res[RS_ELC] = p_ptr->dis_res[RS_ELC] += p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank / 2;
		p_ptr->res[RS_ICE] = p_ptr->dis_res[RS_ICE] += p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank / 2;
		p_ptr->res[RS_ACD] = p_ptr->dis_res[RS_ACD] += p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank / 2;
		p_ptr->res[RS_PSN] = p_ptr->dis_res[RS_PSN] += p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank / 2;
		p_ptr->res[RS_TLK] = p_ptr->dis_res[RS_TLK] += p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank / 3;
	}
	if (p_ptr->skills[SK_MENTAL_RESISTANCE].skill_max > 0)
	{
		p_ptr->res[RS_PSI] = p_ptr->dis_res[RS_PSI] += (p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank) * 3;
	/*	p_ptr->res[RS_TLK] = p_ptr->dis_res[RS_TLK] += (p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank) * 3; */
		p_ptr->res[RS_SPI] = p_ptr->dis_res[RS_SPI] += (p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank) * 3;
		p_ptr->res[RS_TIM] = p_ptr->dis_res[RS_TIM] += p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank / 2;
		p_ptr->res[RS_ETH] = p_ptr->dis_res[RS_ETH] += p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank / 2;
		p_ptr->res[RS_SND] = p_ptr->dis_res[RS_SND] += p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank / 2;
		p_ptr->res[RS_NTH] = p_ptr->dis_res[RS_NTH] += p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank / 2;
		p_ptr->res[RS_LIT] = p_ptr->dis_res[RS_LIT] += p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank;
		p_ptr->res[RS_DRK] = p_ptr->dis_res[RS_DRK] += p_ptr->skills[SK_MENTAL_RESISTANCE].skill_rank;
	}
	if (p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_max > 0)
	{
		p_ptr->res[RS_FIR] = p_ptr->dis_res[RS_FIR] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank * 2;
		p_ptr->res[RS_EAR] = p_ptr->dis_res[RS_EAR] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank * 2;
		p_ptr->res[RS_AIR] = p_ptr->dis_res[RS_AIR] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank * 2;
		p_ptr->res[RS_WTR] = p_ptr->dis_res[RS_WTR] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank * 2;
		p_ptr->res[RS_ELC] = p_ptr->dis_res[RS_ELC] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank;
		p_ptr->res[RS_ICE] = p_ptr->dis_res[RS_ICE] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank;
		p_ptr->res[RS_ACD] = p_ptr->dis_res[RS_ACD] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank;
		p_ptr->res[RS_PSN] = p_ptr->dis_res[RS_PSN] += p_ptr->skills[SK_ELEMENTAL_RESISTANCE].skill_rank;
	}
	if (p_ptr->skills[SK_FIRE_MASTERY].skill_max > 0)
		p_ptr->res[RS_FIR] = p_ptr->dis_res[RS_FIR] += p_ptr->skills[SK_FIRE_MASTERY].skill_rank * 4;
	if (p_ptr->skills[SK_WIND_MASTERY].skill_max > 0)
		p_ptr->res[RS_AIR] = p_ptr->dis_res[RS_AIR] += p_ptr->skills[SK_WIND_MASTERY].skill_rank * 4;
	if (p_ptr->skills[SK_EARTH_MASTERY].skill_max > 0)
		p_ptr->res[RS_EAR] = p_ptr->dis_res[RS_EAR] += p_ptr->skills[SK_EARTH_MASTERY].skill_rank * 4;
	if (p_ptr->skills[SK_WATER_MASTERY].skill_max > 0)
		p_ptr->res[RS_WTR] = p_ptr->dis_res[RS_WTR] += p_ptr->skills[SK_WATER_MASTERY].skill_rank * 4;
	if ((p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0) &&
		(p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1))
	{
		p_ptr->res[RS_FIR] = p_ptr->dis_res[RS_FIR] -= p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2;
		p_ptr->res[RS_EAR] = p_ptr->dis_res[RS_EAR] -= p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2;
		p_ptr->res[RS_AIR] = p_ptr->dis_res[RS_AIR] -= p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2;
		p_ptr->res[RS_WTR] = p_ptr->dis_res[RS_WTR] -= p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2;
		p_ptr->res[RS_PSI] = p_ptr->dis_res[RS_PSI] -= p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2;
		p_ptr->res[RS_TLK] = p_ptr->dis_res[RS_TLK] -= p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2;
		p_ptr->res[RS_SPI] = p_ptr->dis_res[RS_SPI] -= p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2;
		
	}		
	
	/*** Analyze player ***/

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3);

	/* Good flags */
	if (f2 & (TR2_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
	if (f2 & (TR2_FEATHER)) p_ptr->ffall = TRUE;
	if (f2 & (TR2_TELEPATHY)) p_ptr->telepathy = TRUE;
	if (f2 & (TR2_SEE_INVIS)) p_ptr->see_inv = TRUE;
	if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
	if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;
	if (f2 & (TR2_REGEN_25)) p_ptr->regenerate_25 = TRUE;
	if (f2 & (TR2_REGEN_50)) p_ptr->regenerate_50 = TRUE;
	if (f2 & (TR2_REGEN_75)) p_ptr->regenerate_75 = TRUE;
	if (f2 & (TR2_RES_FEAR)) p_ptr->resist_fear = TRUE;
	if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
	if (f2 & (TR2_RES_CONFU)) p_ptr->resist_confu = TRUE;
	
	/* Weird flags */
	if (f2 & (TR2_BLESSED)) p_ptr->bless_blade = TRUE;
	if (f2 & (TR2_WRAITH)) p_ptr->wraith_form = TRUE;
	
	/* Bad flags */
	if (f3 & (TR3_NO_TELEPORT)) p_ptr->no_teleport = TRUE;
	if (f3 & (TR3_EARTHQUAKE)) p_ptr->impact = TRUE;
	if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
	if (f3 & (TR3_NO_MAGIC)) p_ptr->no_magic = TRUE;
	if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
	if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
	if (f3 & (TR3_DRAIN_HP)) p_ptr->hp_drain = TRUE;
	if (f3 & (TR3_DRAIN_SP)) p_ptr->sp_drain = TRUE;
	if (f3 & (TR3_DRAIN_ITEM)) p_ptr->item_drain = TRUE;
	if (f3 & (TR3_DISRUPT_SPELL)) p_ptr->spell_disrupt = TRUE;

	/* Sustain flags */
	if (f1 & (TR1_SUST_MUS)) p_ptr->sustain_mus = TRUE;
	if (f1 & (TR1_SUST_AGI)) p_ptr->sustain_agi = TRUE;
	if (f1 & (TR1_SUST_VIG)) p_ptr->sustain_vig = TRUE;
	if (f1 & (TR1_SUST_SCH)) p_ptr->sustain_sch = TRUE;
	if (f1 & (TR1_SUST_EGO)) p_ptr->sustain_ego = TRUE;
	if (f1 & (TR1_SUST_CHR)) p_ptr->sustain_chr = TRUE;

	/* add bonuses to character status due to skills */
	/* Some stuff should probably be moved here from below */
	/* Affect speed */
	if (p_ptr->skills[SK_MARTIAL_SPEED].skill_max > 0)
		p_ptr->pspeed += ((p_ptr->skills[SK_MARTIAL_SPEED].skill_rank) / 2);
	if (p_ptr->skills[SK_SPIRIT_SPEED].skill_max > 0)
		p_ptr->pspeed += ((p_ptr->skills[SK_SPIRIT_SPEED].skill_rank) / 2);
	if (p_ptr->skills[SK_WIND_LORE].skill_max > 1)
		p_ptr->pspeed += p_ptr->skills[SK_WIND_LORE].skill_rank / 4;
	if (p_ptr->skills[SK_WIND_MASTERY].skill_rank > 0)
		p_ptr->pspeed += p_ptr->skills[SK_WIND_MASTERY].skill_rank / 2;
	if (p_ptr->skills[SK_ATHLETICS].skill_rank > 9)
		p_ptr->pspeed += p_ptr->skills[SK_ATHLETICS].skill_rank / 10;
	
	/* Apply the bonuses to armor class */
	if (p_ptr->skills[SK_IRON_BODY].skill_max > 0)
		p_ptr->to_a += (p_ptr->skills[SK_IRON_BODY].skill_rank + 1) / 2;
	if (p_ptr->skills[SK_MARTIAL_DEFENSE].skill_max > 0)
		p_ptr->to_a += (p_ptr->skills[SK_MARTIAL_DEFENSE].skill_rank);
	if (p_ptr->skills[SK_SPIRIT_SHIELD].skill_max > 0)
		p_ptr->to_a += (p_ptr->skills[SK_SPIRIT_SHIELD].skill_rank * 2);
	if (p_ptr->skills[SK_EARTH_LORE].skill_max > 1)
		p_ptr->to_a += p_ptr->skills[SK_EARTH_LORE].skill_rank;
	if (p_ptr->skills[SK_EARTH_MASTERY].skill_max > 0)
		p_ptr->to_a += ((p_ptr->skills[SK_EARTH_MASTERY].skill_rank * 3) / 2);
	if (p_ptr->skills[SK_WATER_MASTERY].skill_max > 0)
		p_ptr->to_a += p_ptr->skills[SK_WATER_MASTERY].skill_rank;
	if (p_ptr->skills[SK_HOLD_THE_LINE].skill_max > 0)
		p_ptr->to_a += p_ptr->skills[SK_HOLD_THE_LINE].skill_rank;
	if (p_ptr->skills[SK_ACROBATICS].skill_max > 0)
		p_ptr->to_a += p_ptr->skills[SK_ACROBATICS].skill_rank;
	
	/* Apply the mental bonuses to armor class, if known */
	if (p_ptr->skills[SK_IRON_BODY].skill_max > 0)
		p_ptr->dis_to_a += (p_ptr->skills[SK_IRON_BODY].skill_rank + 1) / 2;
	if (p_ptr->skills[SK_MARTIAL_DEFENSE].skill_max > 0)
		p_ptr->dis_to_a += (p_ptr->skills[SK_MARTIAL_DEFENSE].skill_rank);
	if (p_ptr->skills[SK_SPIRIT_SHIELD].skill_max > 0)
		p_ptr->dis_to_a += (p_ptr->skills[SK_SPIRIT_SHIELD].skill_rank * 2);
	if (p_ptr->skills[SK_EARTH_LORE].skill_max > 1)
		p_ptr->dis_to_a += p_ptr->skills[SK_EARTH_LORE].skill_rank;
	if (p_ptr->skills[SK_EARTH_MASTERY].skill_max > 0)
		p_ptr->dis_to_a += ((p_ptr->skills[SK_EARTH_MASTERY].skill_rank * 3) / 2);
	if (p_ptr->skills[SK_WATER_MASTERY].skill_max > 0)
		p_ptr->dis_to_a += p_ptr->skills[SK_WATER_MASTERY].skill_rank;
	if (p_ptr->skills[SK_HOLD_THE_LINE].skill_max > 0)
		p_ptr->dis_to_a += p_ptr->skills[SK_HOLD_THE_LINE].skill_rank;
	if (p_ptr->skills[SK_ACROBATICS].skill_max > 0)
		p_ptr->dis_to_a += p_ptr->skills[SK_ACROBATICS].skill_rank;

	/* Apply Stat Bonuses from skills */
	/* mirroed in files.c */
	if (p_ptr->skills[SK_ERUDITE].skill_max > 0)
	{
		p_ptr->stat_add[A_MUS] -= p_ptr->skills[SK_ERUDITE].skill_rank;
		p_ptr->stat_add[A_EGO] += p_ptr->skills[SK_ERUDITE].skill_rank;
		p_ptr->stat_add[A_SCH] += p_ptr->skills[SK_ERUDITE].skill_rank;
	}	
	if (p_ptr->skills[SK_ATHLETICS].skill_max > 0)
	{
		p_ptr->stat_add[A_MUS] += (p_ptr->skills[SK_ATHLETICS].skill_rank + 2) / 4;
		p_ptr->stat_add[A_AGI] += (p_ptr->skills[SK_ATHLETICS].skill_rank + 3) / 4;
	}

	/* Apply *special* bonuses */
	/* Mediums get Telepathy after a certain point */
	if ((p_ptr->skills[SK_TELEPATHY].skill_max > 9) &&
		(p_ptr->skills[SK_CLAIRSENTIENCE].skill_max > 19))
		p_ptr->telepathy = TRUE;
	
	/* Naturalists get lore boosts */
	if (p_ptr->skills[SK_FIRE_LORE].skill_max > 1)
		p_ptr->see_infra += (p_ptr->skills[SK_FIRE_LORE].skill_max + 1) / 3;
	if (p_ptr->skills[SK_FIRE_MASTERY].skill_max > 9)
		p_ptr->sh_fire = TRUE;
	if (p_ptr->skills[SK_EARTH_MASTERY].skill_max > 9)
		p_ptr->sh_spine = TRUE;
	if (p_ptr->skills[SK_WIND_MASTERY].skill_max > 9)
		p_ptr->sh_elec = TRUE;
		
	/* Affect saving throw */

	if (p_ptr->skills[SK_SAVETH_BNUS].skill_max > 0)
		p_ptr->skill_sav += p_ptr->skills[SK_SAVETH_BNUS].skill_rank;

	if (p_ptr->skills[SK_STEALTH_GOOD].skill_max > 0)
		p_ptr->skill_stl = (p_ptr->skills[SK_STEALTH_GOOD].skill_rank) * 3 / 2;
	if (p_ptr->skills[SK_STEALTH_NORM].skill_max > 0)
		p_ptr->skill_stl = p_ptr->skills[SK_STEALTH_NORM].skill_rank;
	if (p_ptr->skills[SK_STEALTH_POOR].skill_max > 0)
		p_ptr->skill_stl = p_ptr->skills[SK_STEALTH_POOR].skill_rank / 2;

	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl <  0) p_ptr->skill_stl =  0;

	/* Calculate effects of mutations on stats (see mutation.c) - G */
	if (p_ptr->muta5 || p_ptr->muta6)
	{
		calc_mutations();
	}

	/*** Analyze equipment ***/

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stats */
		p_ptr->stat_add[A_MUS] += get_object_pval(o_ptr, TR_PVAL_MUS);
		p_ptr->stat_add[A_AGI] += get_object_pval(o_ptr, TR_PVAL_AGI);
		p_ptr->stat_add[A_VIG] += get_object_pval(o_ptr, TR_PVAL_VIG);
		p_ptr->stat_add[A_SCH] += get_object_pval(o_ptr, TR_PVAL_SCH);
		p_ptr->stat_add[A_EGO] += get_object_pval(o_ptr, TR_PVAL_EGO);
		p_ptr->stat_add[A_CHR] += get_object_pval(o_ptr, TR_PVAL_CHR);

		/* Affect stealth */
	 	p_ptr->skill_stl += get_object_pval(o_ptr, TR_PVAL_STEALTH);

		/* Affect saves */
		p_ptr->skill_sav += get_object_pval(o_ptr, TR_PVAL_SAVE);

		/* Affect searching ability (factor of five) */
		if (p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 0)
		{
		 	p_ptr->skills[SK_SEARCHING_GOOD].skill_rank += get_object_pval(o_ptr, TR_PVAL_SEARCH) * 5;
		}
		if (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 0)
		{
		 	p_ptr->skills[SK_SEARCHING_NORM].skill_rank += get_object_pval(o_ptr, TR_PVAL_SEARCH) * 5;
		}
		if (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 0)
		{
		 	p_ptr->skills[SK_SEARCHING_POOR].skill_rank += get_object_pval(o_ptr, TR_PVAL_SEARCH) * 5;
		}

		/* Affect digging (factor of 20) */
		p_ptr->skill_dig += get_object_pval(o_ptr, TR_PVAL_TUNNEL) * 20;

		/* Affect Magic ability */
		p_ptr->skills[SK_LATIN].skill_rank += get_object_pval(o_ptr, TR_PVAL_MAGIC_MASTER) * 2;
		p_ptr->skills[SK_TMPR_WILL].skill_rank += get_object_pval(o_ptr, TR_PVAL_MAGIC_MASTER) * 2;
		if (p_ptr->skills[SK_THAUMIC_ENERGY].skill_max > 0)
		{
		 	p_ptr->skills[SK_THAUMIC_ENERGY].skill_rank += get_object_pval(o_ptr, TR_PVAL_MAGIC_MASTER) * 2;
		}
		if (p_ptr->skills[SK_LESSER_WARD].skill_max > 0)
		{
		 	p_ptr->skills[SK_LESSER_WARD].skill_rank += get_object_pval(o_ptr, TR_PVAL_MAGIC_MASTER) * 2;
		}

		/* Affect infravision */
		p_ptr->see_infra += get_object_pval(o_ptr, TR_PVAL_INFRA);
		
		/* Affect mana (factor of 20) */
		p_ptr->mana_bonus += get_object_pval(o_ptr, TR_PVAL_MANA) * 20;
		
		/* Affect health (factor of 10) */
		p_ptr->health_bonus += get_object_pval(o_ptr, TR_PVAL_HEALTH) * 10;

		/* Affect speed */
		p_ptr->pspeed += get_object_pval(o_ptr, TR_PVAL_SPEED);

		/* Affect blows */
		extra_blows +=  get_object_pval(o_ptr, TR_PVAL_BLOWS);

		/* Affect shots */
		extra_shots += get_object_pval(o_ptr, TR_PVAL_SHOTS);

		/* Affect Might */
		extra_might += get_object_pval(o_ptr, TR_PVAL_MIGHT);

		/* Good flags */
		if (f2 & (TR2_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f2 & (TR2_FEATHER)) p_ptr->ffall = TRUE;
		if (f2 & (TR2_TELEPATHY)) p_ptr->telepathy = TRUE;
		if (f2 & (TR2_SEE_INVIS)) p_ptr->see_inv = TRUE;
		if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
		if (f2 & (TR2_INVISIBLE)) p_ptr->invisiblity = TRUE;
		if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;
		if (f3 & (TR3_SH_FIRE)) p_ptr->sh_fire = TRUE;
		if (f3 & (TR3_SH_ELEC)) p_ptr->sh_elec = TRUE;
		if (f3 & (TR3_SPINES)) p_ptr->sh_spine = TRUE;

		if (f2 & (TR2_REGEN_25)) p_ptr->regenerate_25 = TRUE;
		if (f2 & (TR2_REGEN_50)) p_ptr->regenerate_50 = TRUE;
		if (f2 & (TR2_REGEN_75)) p_ptr->regenerate_75 = TRUE;

		/* Weird flags */
		if (f2 & (TR2_BLESSED)) p_ptr->bless_blade = TRUE;
		if (f2 & (TR2_WRAITH)) p_ptr->wraith_form = TRUE;

		/* Bad flags */
		if (f3 & (TR3_NO_TELEPORT)) p_ptr->no_teleport = TRUE;
		if (f3 & (TR3_EARTHQUAKE)) p_ptr->impact = TRUE;
		if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
		if (f3 & (TR3_NO_MAGIC)) p_ptr->no_magic = TRUE;
		if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_DRAIN_HP)) p_ptr->hp_drain = TRUE;
		if (f3 & (TR3_DRAIN_SP)) p_ptr->sp_drain = TRUE;
		if (f3 & (TR3_DRAIN_ITEM)) p_ptr->item_drain = TRUE;
		if (f3 & (TR3_DISRUPT_SPELL)) p_ptr->spell_disrupt = TRUE;

		if (f2 & (TR2_RES_FEAR)) p_ptr->resist_fear = TRUE;
		if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_CONFU)) p_ptr->resist_confu = TRUE;

		/* Sustain flags */
		if (f1 & (TR1_SUST_MUS)) p_ptr->sustain_mus = TRUE;
		if (f1 & (TR1_SUST_AGI)) p_ptr->sustain_agi = TRUE;
		if (f1 & (TR1_SUST_VIG)) p_ptr->sustain_vig = TRUE;
		if (f1 & (TR1_SUST_SCH)) p_ptr->sustain_sch = TRUE;
		if (f1 & (TR1_SUST_EGO)) p_ptr->sustain_ego = TRUE;
		if (f1 & (TR1_SUST_CHR)) p_ptr->sustain_chr = TRUE;

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Extract resistances */
		for (j = 0 ; j < RS_MAX ; j++)
		{
			res = object_resist(o_ptr, j);
			dis_res = object_resist_known(o_ptr, j);
			
			/* Set current cap -- Not sure what the point of caps is */
			/* Base resistance = class + race */
			p_ptr->res[j] += res;

			/* Display */
			p_ptr->dis_res[j] += dis_res;
		}
		
		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_GUN) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h += o_ptr->to_h;
		p_ptr->to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr)) p_ptr->dis_to_d += o_ptr->to_d;
	}




	/*** Temporary flags ***/

	/* Temporary resistances */
	for (i = 0 ; i < RS_MAX ; i++)
	{
		/* Temporary resistance, add 33% */
		if (p_ptr->tim_res[i]) 
		{
				p_ptr->res[i] += TEMP_RES_BONUS;
				p_ptr->dis_res[i] += TEMP_RES_BONUS;
		}
	}

	/*** Special flags ***/
	
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
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}


	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
		p_ptr->to_h += 5;
		p_ptr->dis_to_h += 5;
	}

	/* Temprory shield */
	if (p_ptr->shield)
	{
		p_ptr->to_a += 40;
		p_ptr->dis_to_a += 40;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
		p_ptr->to_d += 5;
		p_ptr->dis_to_d += 5;
	}

	/* Temporary "Berserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_d += 30;
		p_ptr->dis_to_d += 30;
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
	
	/* Temporary Light */
	if (p_ptr->tim_light)
	{
		p_ptr->lite = TRUE;
	}
	
	/* Temporary esp */
	if (p_ptr->tim_esp)
	{
		p_ptr->telepathy = TRUE;
	}
	
	/* add a global counter that increases the risk of mutation */
	/* add mutations */
	/* Temporary Demonspell */
	if (p_ptr->tim_demonspell)
	{
		p_ptr->stat_add[A_EGO] += 4;
		p_ptr->stat_add[A_SCH] += 4;
		p_ptr->mana_bonus += p_ptr->lev * 10;
		p_ptr->skills[SK_LATIN].skill_rank += 20;
		if (p_ptr->skills[SK_THAUMIC_ENERGY].skill_max > 0)
			 	p_ptr->skills[SK_THAUMIC_ENERGY].skill_rank += 2;
		if (p_ptr->skills[SK_LESSER_WARD].skill_max > 0) 
				p_ptr->skills[SK_LESSER_WARD].skill_rank += 2;
	}
	

	/* Temporary Demonspell */
	if (p_ptr->tim_demonhealth)
	{
		/* Have to increase vigor to refresh hitpoints display */
		p_ptr->stat_add[A_VIG] += 4;
		p_ptr->health_bonus += p_ptr->lev * 10;
	}

	if (p_ptr->tim_wormsense)
	{
		p_ptr->see_inv = TRUE;
		p_ptr->wormsense = TRUE;
		if (p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 0)	p_ptr->skills[SK_SEARCHING_GOOD].skill_rank += p_ptr->lev * 5;
		if (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 0)	p_ptr->skills[SK_SEARCHING_NORM].skill_rank += p_ptr->lev * 5;
		if (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 0) p_ptr->skills[SK_SEARCHING_POOR].skill_rank += p_ptr->lev * 5;
		if (p_ptr->skills[SK_DISARM_GOOD].skill_max > 0) p_ptr->skills[SK_DISARM_GOOD].skill_rank += p_ptr->lev * 5;
		if (p_ptr->skills[SK_DISARM_NORM].skill_max > 0) p_ptr->skills[SK_DISARM_NORM].skill_rank += p_ptr->lev * 5;
		if (p_ptr->skills[SK_DISARM_POOR].skill_max > 0) p_ptr->skills[SK_DISARM_POOR].skill_rank += p_ptr->lev * 5;
		p_ptr->to_h += 14;
		p_ptr->dis_to_h += 14;
		/* nothing yet */
	}

	/* Temporary Demonspell */
	if (p_ptr->tim_voorish)
	{
		p_ptr->stat_add[A_EGO] += 1;
		p_ptr->stat_add[A_SCH] += 1;
		p_ptr->mana_bonus += p_ptr->lev * 5;
		p_ptr->skills[SK_LATIN].skill_rank += 5;
		p_ptr->skills[SK_TMPR_WILL].skill_rank += 10;
		p_ptr->skills[SK_RITUAL_MAGIC].skill_rank += 10;
	}

	/* Temporary stygian */
	if (p_ptr->tim_stygian)
	{
		p_ptr->stat_add[A_MUS] += 10;
		p_ptr->stat_add[A_VIG] += 10;
		p_ptr->stat_add[A_AGI] += 10;
		p_ptr->to_h += 30;
		p_ptr->dis_to_h += 30;
		p_ptr->to_d += 50;
		p_ptr->dis_to_d += 50;
		extra_blows += 2;
		p_ptr->health_bonus += p_ptr->lev * 5;
		/* Need to add demon attacks here */

	}

	if (p_ptr->tim_muscle)
	{
		p_ptr->stat_add[A_MUS] += 10;
	}

	if (p_ptr->tim_vigor)
	{
		p_ptr->stat_add[A_VIG] += 10;
	}

	if (p_ptr->tim_no_tele)
	{
		p_ptr->no_teleport = TRUE;
	}
	
	if (p_ptr->tim_free_act)
	{
		p_ptr->free_act = TRUE;
	}

	if (p_ptr->tim_anti_magic)
	{
		p_ptr->anti_magic = TRUE;
	}

	if (p_ptr->tim_evade)
	{
		p_ptr->pspeed += 5;
		p_ptr->to_h -= 40;
		p_ptr->dis_to_h -= 40;
		p_ptr->to_d -= 40;
		p_ptr->dis_to_d -= 40;
		p_ptr->to_a += 20;
		p_ptr->dis_to_a += 20;
	}

	/* Temporary wraithform */
	if (p_ptr->tim_wraith)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra++;
	}
	
	if (p_ptr->tim_harding)
	{
		p_ptr->to_a += (p_ptr->skills[SK_AEGIS_CYPHER].skill_rank * 5);
		p_ptr->dis_to_a += (p_ptr->skills[SK_AEGIS_CYPHER].skill_rank * 5);
	}

	if (p_ptr->tim_invisiblity)
	{
		p_ptr->invisiblity = TRUE;
	}
	
	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->hero || p_ptr->shero)
	{
		p_ptr->resist_fear = TRUE;
	}

	/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int add, top, use;

		/* Extract modifier */
		add = p_ptr->stat_add[i];

 		/* Extract the new "stat_top" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], add);

		/* Save the new value */
		p_ptr->stat_top[i] = top;

		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], add);

		/* Save the new value */
		p_ptr->stat_use[i] = use;
	}

	/*** Analyze weight ***/

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* Apply "encumbrance" from weight */
	if (j > i / 2) p_ptr->pspeed -= ((j - (i / 2)) / (i / 10));

	/* Wonderland Characters get speed boost to help survival */
	if (p_ptr->wonderland)
		p_ptr->pspeed +=10;

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

	/* Sanity check on extreme speeds */
	if (p_ptr->pspeed < 0) p_ptr->pspeed = 0;
	if (p_ptr->pspeed > 199) p_ptr->pspeed = 199;

	/*** Apply modifier bonuses ***/

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)((p_ptr->stat_use[A_AGI] / 60) - 3));
	p_ptr->to_d += ((int)((p_ptr->stat_use[A_MUS] / 40) - 3));
	p_ptr->to_h += ((int)((p_ptr->stat_use[A_AGI] / 60) - 3));
	p_ptr->to_h += ((int)((p_ptr->stat_use[A_MUS] / 60) - 3));

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)((p_ptr->stat_use[A_AGI] / 60) - 3));
	p_ptr->dis_to_d += ((int)((p_ptr->stat_use[A_MUS] / 40) - 3));
	p_ptr->dis_to_h += ((int)((p_ptr->stat_use[A_AGI] / 60) - 3));
	p_ptr->dis_to_h += ((int)((p_ptr->stat_use[A_MUS] / 60) - 3));


	/*** Modify skills ***/

	/* Affect Skill -- stealth (non-existant possible race bonus) */
	/* p_ptr->skill_stl += 1; */

	/* Affect Skill -- disarming (AGI and SCH) */
	if (p_ptr->skills[SK_DISARM_GOOD].skill_max >= 0)
	{
		p_ptr->skills[SK_DISARM_GOOD].skill_rank += (p_ptr->stat_use[A_AGI] / 99);
		p_ptr->skills[SK_DISARM_GOOD].skill_rank += (p_ptr->stat_use[A_SCH] / 50);
	}
	if (p_ptr->skills[SK_DISARM_NORM].skill_max >= 0)
	{
		p_ptr->skills[SK_DISARM_NORM].skill_rank += (p_ptr->stat_use[A_AGI] / 99);
		p_ptr->skills[SK_DISARM_NORM].skill_rank += (p_ptr->stat_use[A_SCH] / 50);
	}
	if (p_ptr->skills[SK_DISARM_POOR].skill_max >= 0)
	{
		p_ptr->skills[SK_DISARM_POOR].skill_rank += (p_ptr->stat_use[A_AGI] / 99);
		p_ptr->skills[SK_DISARM_POOR].skill_rank += (p_ptr->stat_use[A_SCH] / 50);
	}

	/* Affect Skill -- magic devices (SCH) */
	if (p_ptr->skills[SK_DEVICE_GOOD].skill_max >= 0)
	{
		p_ptr->skills[SK_DEVICE_GOOD].skill_rank += (p_ptr->stat_use[A_SCH] / 40);
	}
	if (p_ptr->skills[SK_DEVICE_NORM].skill_max >= 0)
	{
		p_ptr->skills[SK_DEVICE_NORM].skill_rank += (p_ptr->stat_use[A_SCH] / 40);
	}
	if (p_ptr->skills[SK_DEVICE_POOR].skill_max >= 0)
	{
		p_ptr->skills[SK_DEVICE_POOR].skill_rank += (p_ptr->stat_use[A_SCH] / 40);
	}	

	/* Affect Skill -- saving throw (EGO) */
	p_ptr->skill_sav += (p_ptr->stat_use[A_EGO] / 60);
	p_ptr->skill_sav += (p_ptr->stat_use[A_AGI] / 60);

	/* Affect Skill -- digging (MUS) */
	p_ptr->skill_dig += (p_ptr->stat_use[A_MUS] / 40);
	
	/* Recalculate stealth when needed */
	if ((p_ptr->skill_stl != old_stealth) || (!p_ptr->skill_stl))
	{
		/* Assume character is extremely noisy. */
		p_ptr->base_wakeup_chance = 100 * WAKEUP_ADJ;

		p_ptr->base_wakeup_chance -= (p_ptr->skill_stl * 64);

		/* Always make at least some innate noise */
		if (p_ptr->base_wakeup_chance < 100)
		{
			if (p_ptr->skills[SK_SUPERSTEALTH].skill_rank > 0) 
			{
				p_ptr->base_wakeup_chance = 100 - p_ptr->skills[SK_SUPERSTEALTH].skill_rank * 4;
				if (p_ptr->base_wakeup_chance < 20) p_ptr->base_wakeup_chance = 20;
			}
			else p_ptr->base_wakeup_chance = 100;
		}
	}

	/* Obtain the "hold" value */
	/* This is mirrored in the quest.c item function */
	hold = p_ptr->stat_use[A_MUS] / 9 + 5;


	/*** Analyze current bow ***/

	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_GUN];

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
		if (o_ptr->sval)
		{
			/* Get basic data */
			p_ptr->ammo_tval = o_ptr->ammo_tval;
			p_ptr->ammo_mult = o_ptr->ammo_mult;
			p_ptr->num_fire = o_ptr->num_fire;
		}
		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Extra might */
			p_ptr->ammo_mult += extra_might;

#if 0
			/* Hack -- Rangers love Bows */
			if ((
			    (p_ptr->ammo_tval == TV_BULLET) ||
			    (p_ptr->ammo_tval == TV_AMMO) ||
			    (p_ptr->ammo_tval == TV_SHOT)
			    ))
				{
					/* Extra Shot at level 4 */
					if (p_ptr->skills[SK_SWIFT_SHOT].skill_rank >= 4) p_ptr->num_fire++;

					/* Extra shot at level 8 */
					if (p_ptr->skills[SK_SWIFT_SHOT].skill_rank >= 8) p_ptr->num_fire++;
	
					/* Extra shot at level 12 */
					if (p_ptr->skills[SK_SWIFT_SHOT].skill_rank >= 12) p_ptr->num_fire++;

					/* Extra shot at level 16 */
					if (p_ptr->skills[SK_SWIFT_SHOT].skill_rank >= 16) p_ptr->num_fire++;

					/* Extra shot at level 20 */
					if (p_ptr->skills[SK_SWIFT_SHOT].skill_rank >= 20) p_ptr->num_fire++;
				}
#endif
		}

		/* Require at least one shot */
		if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
	}


	/*** Analyze weapon ***/

	/* Examine the "current weapon" */
	o_ptr = &inventory[INVEN_WIELD];

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* "My hands are deadly weapons!" Nick Cage in Con Air */
	if (!o_ptr->k_idx)
	{
		int str_index, dex_index;
		
		/* "Don't touch the bunny" */
		str_index = (p_ptr->stat_use[A_MUS] / 90);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* "I told you not to touch the bunny!" */
		dex_index = p_ptr->stat_use[A_AGI] / 90;

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* "He's got the whole world in his hands" - Steve Buscemi(sp?) same film */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->num_blow > cp_ptr->max_attacks) p_ptr->num_blow = cp_ptr->max_attacks;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		/* Require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;
	}
	
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
		int muscle_value, agility_value;

		int div;

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

		/* Stat value */
		muscle_value = p_ptr->stat_use[A_MUS];

		if (p_ptr->skills[SK_WEAPON_FINESSE].skill_rank > 0)
		{
			muscle_value += (p_ptr->skills[SK_WEAPON_FINESSE].skill_rank * 20);
		}

		str_index = ((muscle_value / 14) * cp_ptr->att_multiply / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Stat value */
		agility_value = p_ptr->stat_use[A_AGI];
		
		if (p_ptr->skills[SK_SWIFT_BLOW].skill_rank > 0)
		{
			agility_value += (p_ptr->skills[SK_SWIFT_BLOW].skill_rank *20);
		}

		/* Index by dexterity */
		dex_index = agility_value / 120;

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->num_blow > cp_ptr->max_attacks) p_ptr->num_blow = cp_ptr->max_attacks;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		/* Require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;

		/* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);

	}
	

	/* Assume okay */
	p_ptr->icky_wield = FALSE;

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (!p_ptr->bless_blade) &&
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


	/*** Notice changes ***/

	/* Analyze stats */
	for (i = 0; i < A_MAX; i++)
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
		if (p_ptr->stat_use[i] != old_stat_use[i])
		{
			/* Change in CON affects Hitpoints */
			if (i == A_VIG)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in a stat may affect Mana */
			if (cp_ptr->spell_stat == i)
			{
				p_ptr->update |= (PU_MANA);
			}
#if 0
			else if (i == A_SCH)
			{
				if (cp_ptr->spell_stat == A_SCH)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_EGO)
			{
				if (cp_ptr->spell_stat == A_EGO)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}
#endif
		}
	}

	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
		p_ptr->redraw |= (PR_EXTRA);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
		p_ptr->redraw |= (PR_EXTRA);
	}
	if (p_ptr->wormsense != old_wormsense) p_ptr->redraw |= (PR_EXTRA);
	if (p_ptr->free_act != old_free_act) p_ptr->redraw |= (PR_EXTRA);
	if (p_ptr->see_infra != old_infra) p_ptr->redraw |= (PR_EXTRA);
	if (p_ptr->no_teleport != old_notele) p_ptr->redraw |= (PR_EXTRA);
	if (p_ptr->anti_magic != old_nomagic) p_ptr->redraw |= (PR_EXTRA);
	if (p_ptr->invisiblity != old_invisiblity) p_ptr->redraw |= (PR_ARMOR);


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
	if (character_xtra) return;

	/* Take note when "heavy gun" changes */
	if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msg_print("You have trouble wielding such a heavy gun.");
		}
		else if (inventory[INVEN_GUN].k_idx)
		{
			msg_print("You have no trouble wielding your gun.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy gun.");
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
		/*calc_spells();*/
	}


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;


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
		/* This needs to go */
		p_ptr->update &= ~(PU_FORGET_FLOW);
		/* forget_flow(); */
	}

	if (p_ptr->update & (PU_UPDATE_FLOW))
	{
		/* This needs to go */
		p_ptr->update &= ~(PU_UPDATE_FLOW);
		/* update_flow(); */
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
void redraw_stuff(void)
{
	/* Redraw stuff */
	if (!p_ptr->redraw) return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;



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
		prt_field(p_name + rp_ptr->name, ROW_RACE, COL_RACE);
		prt_field(c_name + cp_ptr->name, ROW_CLASS, COL_CLASS);
	}

	if (p_ptr->redraw & (PR_TITLE))
	{
		p_ptr->redraw &= ~(PR_TITLE);
		prt_title();
	}

	if (p_ptr->redraw & PR_CLASS)
	{
		p_ptr->redraw &= ~PR_CLASS;
		prt_class();
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
		prt_stat(A_MUS);
		prt_stat(A_SCH);
		prt_stat(A_EGO);
		prt_stat(A_AGI);
		prt_stat(A_VIG);
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
		prt_wp();
		/*
		* hack:  DSV:  redraw player, since the player's color
		*	now indicates approximate health
		*/
		lite_spot(p_ptr->py, p_ptr->px);
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
		/*
		* hack:  DSV:  redraw player, since the player's color
		*	now indicates approximate health
		*/
		lite_spot(p_ptr->py, p_ptr->px);
	}


	if (p_ptr->redraw & (PR_EXTRA))
	{
		p_ptr->redraw &= ~(PR_EXTRA);
		p_ptr->redraw &= ~(PR_CUT | PR_STUN);
		p_ptr->redraw &= ~(PR_HUNGER);
		p_ptr->redraw &= ~(PR_BLIND | PR_CONFUSED);
		p_ptr->redraw &= ~(PR_AFRAID | PR_POISONED);
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_LEVEL);
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

	if (p_ptr->redraw & (PR_LEVEL))
	{
		p_ptr->redraw &= ~(PR_LEVEL);
		prt_level_g();
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
		if (angband_term[j])
		{
			/* Build the mask */
			mask |= op_ptr->window_flag[j];
		}
	}

	/* Apply usable flags */
	p_ptr->window &= (mask);

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

	/* Display message recall */
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


