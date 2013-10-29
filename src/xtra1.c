/* File: xtra1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights mayz also apply.
 */

#include "angband.h"
#include "script.h"



/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val, size_t out_len)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 100)
			strnfmt(out_val, out_len, "18/%03d", bonus);
		else
			strnfmt(out_val, out_len, " 18/%02d", bonus);
	}

	/* From 3 to 18 */
	else
	{
		strnfmt(out_val, out_len, "    %2d", val);
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




/*** Sidebar display functions ***/

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
static void prt_stat(int stat, int row, int col)
{
	char tmp[32];

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		put_str(stat_names_reduced[stat], row, col);
		cnv_stat(p_ptr->stat_use[stat], tmp, sizeof(tmp));
		c_put_str(TERM_YELLOW, tmp, row, col + 6);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], row, col);
		cnv_stat(p_ptr->stat_use[stat], tmp, sizeof(tmp));
		c_put_str(TERM_L_GREEN, tmp, row, col + 6);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18+100)
	{
		put_str("!", row, col + 3);
	}
}


/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(int row, int col)
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
		p = c_text + cp_ptr->title[(p_ptr->lev - 1) / 5];
	}

	prt_field(p, row, col);
}


/*
 * Prints level
 */
static void prt_level(int row, int col)
{
	char tmp[32];

	strnfmt(tmp, sizeof(tmp), "%6d", p_ptr->lev);

	if (p_ptr->lev >= p_ptr->max_lev)
	{
		put_str("LEVEL ", row, col);
		c_put_str(TERM_L_GREEN, tmp, row, col + 6);
	}
	else
	{
		put_str("Level ", row, col);
		c_put_str(TERM_YELLOW, tmp, row, col + 6);
	}
}


/*
 * Display the experience
 */
static void prt_exp(int row, int col)
{
	char out_val[32];
	bool lev50 = (p_ptr->lev == 50);

	long xp = (long)p_ptr->exp;


	/* Calculate XP for next level */
	if (!lev50)
		xp = (long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L) - p_ptr->exp;

	/* Format XP */
	strnfmt(out_val, sizeof(out_val), "%8ld", (long)xp);


	if (p_ptr->exp >= p_ptr->max_exp)
	{
		put_str((lev50 ? "EXP" : "NXT"), row, col);
		c_put_str(TERM_L_GREEN, out_val, row, col + 4);
	}
	else
	{
		put_str((lev50 ? "Exp" : "Nxt"), row, col);
		c_put_str(TERM_YELLOW, out_val, row, col + 4);
	}
}


/*
 * Prints current gold
 */
static void prt_gold(int row, int col)
{
	char tmp[32];

	put_str("AU ", row, col);
	strnfmt(tmp, sizeof(tmp), "%9ld", (long)p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, row, col + 3);
}


/*
 * Equippy chars
 */
static void prt_equippy(int row, int col)
{
	int i;

	byte a;
	char c;

	object_type *o_ptr;

	/* No equippy chars in bigtile mode */
	if (use_bigtile) return;

	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Object */
		o_ptr = &inventory[i];

		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Clear the part of the screen */
		if (!o_ptr->k_idx)
		{
			c = ' ';
			a = TERM_WHITE;
		}

		/* Dump */
		Term_putch(col + i - INVEN_WIELD, row, a, c);
	}
}


/*
 * Prints current AC
 */
static void prt_ac(int row, int col)
{
	char tmp[32];

	put_str("Cur AC ", row, col);
	strnfmt(tmp, sizeof(tmp), "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, row, col + 7);
}


/*
 * Prints Cur hit points
 */
static void prt_hp(int row, int col)
{
	char cur_hp[32], max_hp[32];
	byte color;

	put_str("HP ", row, col);

	strnfmt(max_hp, sizeof(max_hp), "%4d", p_ptr->mhp);
	strnfmt(cur_hp, sizeof(cur_hp), "%4d", p_ptr->chp);

	if (p_ptr->chp >= p_ptr->mhp)
		color = TERM_L_GREEN;
	else if (p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
		color = TERM_YELLOW;
	else
		color = TERM_RED;

	c_put_str(color, cur_hp, row, col + 3);
	c_put_str(TERM_WHITE, "/", row, col + 7);
	c_put_str(TERM_L_GREEN, max_hp, row, col + 8);
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(int row, int col)
{
	char cur_sp[32], max_sp[32];
	byte color;

	/* Do not show mana unless it matters */
	if (!cp_ptr->spell_book) return;
	/* if max spell points is 0 then mana doesn't matter yet */
	if (p_ptr->msp < 1) return;

	put_str("SP ", row, col);

	strnfmt(max_sp, sizeof(max_sp), "%4d", p_ptr->msp);
	strnfmt(cur_sp, sizeof(cur_sp), "%4d", p_ptr->csp);

	if (p_ptr->csp >= p_ptr->msp)
		color = TERM_L_GREEN;
	else if (p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
		color = TERM_YELLOW;
	else
		color = TERM_RED;

	/* Show mana */
	c_put_str(color, cur_sp, row, col + 3);
	c_put_str(TERM_WHITE, "/", row, col + 7);
	c_put_str(TERM_L_GREEN, max_sp, row, col + 8);
}

/*
 * Prints player's silver poison level
 */
static void prt_sil(int row, int col)
{
	char cur_silver[32];
	byte color;

	/* Do not show silver poison unless it matters */
	/* this doesn't seem to work -I can't see why, but it doesn't matter that much */
	if (p_ptr->silver == PY_SILVER_HEALTHY) 
    {
	                  put_str("      ", row, col);
                      c_put_str(TERM_L_GREEN, "     ", row, col + 8);
                      return;
    }

	put_str("Silver", row, col);
	
	/* what is strnfmt? let's see if copying it will be good enough */
	/* ..cool, it is */
	strnfmt(cur_silver, sizeof(cur_silver), "%4d", p_ptr->silver);

	if (p_ptr->silver >= PY_SILVER_VERYBAD)
		color = TERM_VIOLET;
	else if (p_ptr->silver >= PY_SILVER_LEVELTWO)
		color = TERM_RED;
	else if (p_ptr->silver >= PY_SILVER_LEVELONE)
		color = TERM_YELLOW;
	else
		color = TERM_L_GREEN;

	/* Show amount of silver poison */
	c_put_str(color, cur_silver, row, col + 8);
}

/*
 * Prints player's slime level
 */
static void prt_slime(int row, int col)
{
	char cur_slime[32];
	byte color;

	/* Do not show slime level unless it matters */
	/* this doesn't seem to work -I can't see why, but it doesn't matter that much */
	if (p_ptr->slime == PY_SLIME_HEALTHY)
    {
	                  put_str("      ", row, col);
                      c_put_str(TERM_L_GREEN, "     ", row, col + 8);
                      return;
    }
    
	put_str("Slime", row, col);

	/* what is strnfmt? let's see if copying it will be good enough */
	/* ..cool, it is */
	strnfmt(cur_slime, sizeof(cur_slime), "%4d", p_ptr->slime);

	if (p_ptr->slime >= PY_SLIME_LEVELTWO)
		color = TERM_RED;
	else if (p_ptr->slime >= PY_SLIME_LEVELONE)
		color = TERM_YELLOW;
	else
		color = TERM_L_GREEN;

	/* Show amount of silver poison */
	c_put_str(color, cur_slime, row, col + 8);
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
static void prt_health(int row, int col)
{
	/* Not tracking */
	if (!p_ptr->health_who)
	{
		/* Erase the health bar */
		Term_erase(col, row, 12);
	}

	/* Tracking an unseen monster */
	else if (!mon_list[p_ptr->health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->timed[TMD_IMAGE])
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a dead monster (?) */
	else if (!mon_list[p_ptr->health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;

		monster_type *m_ptr = &mon_list[p_ptr->health_who];

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
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(col + 1, row, len, attr, "**********");
	}
}


/*
 * Print cut indicator.
 */
static void prt_cut(int row, int col)
{
	int c = p_ptr->timed[TMD_CUT];

	if (c > 1000)
	{
		c_put_str(TERM_L_RED, "Mortal wound", row, col);
	}
	else if (c > 200)
	{
		c_put_str(TERM_RED, "Deep gash   ", row, col);
	}
	else if (c > 100)
	{
		c_put_str(TERM_RED, "Severe cut  ", row, col);
	}
	else if (c > 50)
	{
		c_put_str(TERM_ORANGE, "Nasty cut   ", row, col);
	}
	else if (c > 25)
	{
		c_put_str(TERM_ORANGE, "Bad cut     ", row, col);
	}
	else if (c > 10)
	{
		c_put_str(TERM_YELLOW, "Light cut   ", row, col);
	}
	else if (c)
	{
		c_put_str(TERM_YELLOW, "Graze       ", row, col);
	}
	else
	{
		put_str("            ", row, col);
	}
}


/*
 * Print stun indicator.
 */
static void prt_stun(int row, int col)
{
	int s = p_ptr->timed[TMD_STUN];

	if (s > 100)
	{
		c_put_str(TERM_RED, "Knocked out ", row, col);
	}
	else if (s > 50)
	{
		c_put_str(TERM_ORANGE, "Heavy stun  ", row, col);
	}
	else if (s)
	{
		c_put_str(TERM_ORANGE, "Stun        ", row, col);
	}
	else
	{
		put_str("            ", row, col);
	}
}




/* Some simple wrapper functions */
static void prt_str(int row, int col) { prt_stat(A_STR, row, col); }
static void prt_dex(int row, int col) { prt_stat(A_DEX, row, col); }
static void prt_wis(int row, int col) { prt_stat(A_WIS, row, col); }
static void prt_int(int row, int col) { prt_stat(A_INT, row, col); }
static void prt_con(int row, int col) { prt_stat(A_CON, row, col); }
static void prt_chr(int row, int col) { prt_stat(A_CHR, row, col); }
static void prt_race(int row, int col) { prt_field(p_name + rp_ptr->name, row, col); }
static void prt_class(int row, int col) { prt_field(c_name + cp_ptr->name, row, col); }


/*
 * Struct of sidebar handlers.
 */
static const struct side_handler_t
{
	void (*hook)(int, int);		/* int row, int col */
	int priority;				/* 1 is most important (always displayed) */
	u32b flag;					/* PR_* flag this corresponds to */
} side_handlers[] =
{
	{ prt_race,      20, PR_MISC },
	{ prt_class,     19, PR_MISC },
	{ prt_title,     18, PR_TITLE },
	{ prt_level,     10, PR_LEV },
	{ prt_exp,       15, PR_EXP },
	{ prt_gold,      11, PR_GOLD },
	{ prt_equippy,   17, PR_EQUIPPY },
	{ prt_str,        6, PR_STATS },
	{ prt_int,        5, PR_STATS },
	{ prt_wis,        4, PR_STATS },
	{ prt_dex,        3, PR_STATS },
	{ prt_con,        2, PR_STATS },
	{ prt_chr,        1, PR_STATS },
	{ NULL,          14, 0 },
	{ prt_ac,         7, PR_ARMOR },
	{ prt_hp,         8, PR_HP },
	{ prt_sp,         9, PR_MANA },
	{ prt_sil,       22, PR_SILVER },
	{ prt_slime,     21, PR_SLIME },
	{ prt_cut,       13, PR_CUT },
	{ prt_stun,      16, PR_STUN },
	{ prt_health,    12, PR_HEALTH }
};


/*
 * This prints the sidebar, using a clever method which means that it will only
 * print as much as can be displayed on <24-line screens.
 *
 * Each row is given a priority; the least important higher numbers and the most
 * important lower numbers.  As the screen gets smaller, the rows start to
 * disappear in the order of lowest to highest importance.
 */
static void display_sidebar(void)
{
	int x, y, row;
	int max_priority;
	size_t i;
	u32b to_clear = 0;


	Term_get_size(&x, &y);

	/* Keep the top and bottom lines clear. */
	max_priority = y - 2;

	/* Display list entries */
	for (i = 0, row = 1; i < N_ELEMENTS(side_handlers); i++)
	{
		const struct side_handler_t *hnd = &side_handlers[i];

		/* If this is high enough priority, display it */
		if (hnd->priority <= max_priority)
		{
			/* If the redraw flag it set, and there is a hook */
			if ((p_ptr->redraw & hnd->flag) && hnd->hook)
			{
				/* Mark flag for removal */
				to_clear |= hnd->flag;

				/* Display at the current row */
				hnd->hook(row, 0);
			}

			/* Increment for next time */
			row++;
		}
	}

	/* Clear flags */
	p_ptr->redraw &= ~(to_clear);
}




/*** Status line display functions ***/

/*
 * Prints depth in stat area
 */
static void prt_depth(int row, int col)
{
	char depths[32];

	if (!p_ptr->depth)
	{
		my_strcpy(depths, "Town", sizeof(depths));
	}
	else if (depth_in_feet)
	{
		strnfmt(depths, sizeof(depths), "%d ft", p_ptr->depth * 50);
	}
	else
	{
		strnfmt(depths, sizeof(depths), "Lev %d", p_ptr->depth);
	}

	/* Right-Adjust the "depth", and clear old values */
	prt(format("%7s", depths), row, col);
}


/*
 * Prints status of hunger
 */
static void prt_hunger(int row, int col)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		c_put_str(TERM_RED, "Weak  ", row, col);
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		c_put_str(TERM_ORANGE, "Weak  ", row, col);
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		c_put_str(TERM_YELLOW, "Hungry", row, col);
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		c_put_str(TERM_L_GREEN, "      ", row, col);
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		c_put_str(TERM_L_GREEN, "Full  ", row, col);
	}

	/* Gorged */
	else
	{
		c_put_str(TERM_GREEN, "Gorged", row, col);
	}
}


/*
 * Prints Blind status
 */
static void prt_blind(int row, int col)
{
	if ((p_ptr->timed[TMD_BLIND]) && (p_ptr->timed[TMD_BRAIL]))
	{
		c_put_str(TERM_YELLOW, "Brail", row, col);
	}
	else if (p_ptr->timed[TMD_BLIND])
	{
		c_put_str(TERM_ORANGE, "Blind", row, col);
	}
	else if (p_ptr->timed[TMD_ZAPPING])
	{
		c_put_str(TERM_BLUE, " ZAP ", row, col);
	}
	else
	{
		put_str("     ", row, col);
	}
}


/*
 * Prints Confusion status
 */
static void prt_confused(int row, int col)
{
	bool confused = p_ptr->timed[TMD_CONFUSED] ? TRUE : FALSE;
	bool forget   = p_ptr->timed[TMD_AMNESIA]  ? TRUE : FALSE;

	const char *text = "        ";

	if (confused && !forget)
		text = "Confused";
	else if (confused && forget)
		text = "Conf Amn";
	else if (!confused && forget)
		text = "Amnesiac";

	c_put_str(TERM_ORANGE, text, row, col);
}


/*
 * Prints Fear & Charm status
 */
static void prt_afraid(int row, int col)
{
	if (p_ptr->timed[TMD_TERROR])
	{
		c_put_str(TERM_RED, "Terror", row, col);
	}
	else if (p_ptr->timed[TMD_FRENZY])
	{
		c_put_str(TERM_ORANGE, "Frenzy", row, col);
	}
	else if (p_ptr->timed[TMD_BECOME_LICH])
	{
		c_put_str(TERM_SLATE, " Lich ", row, col);
	}
	else if (p_ptr->timed[TMD_AFRAID])
	{
		c_put_str(TERM_ORANGE, "Afraid", row, col);
	}
	else if (p_ptr->timed[TMD_CHARM])
	{
		c_put_str(TERM_YELLOW, "Charmd", row, col);
	}
	else
	{
		put_str("      ", row, col);
	}
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(int row, int col)
{
	if (p_ptr->timed[TMD_POISONED])
	{
		c_put_str(TERM_ORANGE, "Poisoned", row, col);
	}
	else
	{
		put_str("        ", row, col);
	}
}


/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(int row, int col)
{
	byte attr = TERM_WHITE;

	char text[16];


	/* Paralysis */
	if (p_ptr->timed[TMD_PARALYZED])
	{
		attr = TERM_RED;

		my_strcpy(text, "Paralyzed!", sizeof(text));
	}

	/* Resting */
	else if (p_ptr->resting)
	{
		int i;
		int n = p_ptr->resting;

		/* Start with "Rest" */
		my_strcpy(text, "Rest      ", sizeof(text));

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
			strnfmt(text, sizeof(text), "Rep. %3d00", p_ptr->command_rep / 100);
		else
			strnfmt(text, sizeof(text), "Repeat %3d", p_ptr->command_rep);
	}

	/* Searching */
	else if (p_ptr->searching)
	{
		my_strcpy(text, "Searching ", sizeof(text));
	}

	/* Nothing interesting */
	else
	{
		my_strcpy(text, "          ", sizeof(text));
	}

	/* Display the info (or blanks) */
	c_put_str(attr, text, row, col);
}


/*
 * Prints the speed of a character.
 */
static void prt_speed(int row, int col)
{
	int i = p_ptr->pspeed;

	byte attr = TERM_WHITE;
	const char *type = NULL;
	char buf[32] = "";

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	/* Fast */
	if (i > 110)
	{
		attr = TERM_L_GREEN;
		type = "Fast";
	}
    
	/* Slow */
	else if (i < 110)
	{
		attr = TERM_L_UMBER;
		type = "Slow";
	}

    /* sustained speed */
    else if (p_ptr->timed[TMD_SUST_SPEED])
    {
		attr = TERM_SLATE;
		type = "SSPD";
    }

	if (type)
		strnfmt(buf, sizeof(buf), "%s (%+d)", type, (i - 110));

	/* Display the speed */
	c_put_str(attr, format("%-14s", buf), row, col);
}


/*
 * Print whether a character is studying or not.
 */
static void prt_study(int row, int col)
{
	if (p_ptr->new_spells)
	{
		put_str("Study", row, col);
	}
	else
	{
		put_str("     ", row, col);
	}
}


static void prt_elements(int row, int col)
{
	int wid, n;

	/* Number of resists to display */
	int count = 5;

	/* XXX Ignore column setting */
	col = 80;
	wid = Term->wid - col;

	/* Print up to 5 letters of the resist */
	n = MIN(wid / count, 5);


	/* Check space */
	if (n <= 0) return;


	if (p_ptr->timed[TMD_OPP_ACID])
		Term_putstr(col, row, n, TERM_SLATE, "Acid ");
	else
		Term_putstr(col, row, n, TERM_SLATE, "     ");

	col += n;

	if (p_ptr->timed[TMD_OPP_ELEC])
		Term_putstr(col, row, n, TERM_BLUE, "Elec ");
	else
		Term_putstr(col, row, n, TERM_BLUE, "     ");

	col += n;

	if (p_ptr->timed[TMD_OPP_FIRE])
		Term_putstr(col, row, n, TERM_RED, "Fire ");
	else
		Term_putstr(col, row, n, TERM_RED, "     ");

	col += n;

	if (p_ptr->timed[TMD_OPP_COLD])
		Term_putstr(col, row, n, TERM_WHITE, "Cold ");
	else
		Term_putstr(col, row, n, TERM_WHITE, "     ");

	col += n;

	if (p_ptr->timed[TMD_OPP_POIS])
		Term_putstr(col, row, n, TERM_GREEN, "Pois ");
	else
		Term_putstr(col, row, n, TERM_GREEN, "     ");
}




/*
 * Struct of status line indicators.
 */
static const struct status_handler_t
{
	u32b flag;							/* p_ptr->redraw flag this entry is for */
	int column;							/* Column to display at */
	void (*hook)(int row, int col);		/* Display function */
} status_handlers[] =
{
	{ PR_HUNGER,    0, prt_hunger },   /* "Weak" / "Hungry" / "Full" / "Gorged" */
	{ PR_BLIND,     7, prt_blind },    /* "Blind" */
	{ PR_CONFUSED, 13, prt_confused }, /* "Confused" */
	{ PR_AFRAID,   22, prt_afraid },   /* "Afraid" or "Charmd" (can't have both at once) */
	{ PR_POISONED, 29, prt_poisoned }, /* "Poisoned" */
	{ PR_STATE,    38, prt_state },    /* <state> */
	{ PR_SPEED,    49, prt_speed },    /* "Slow (-NN)" or "Fast (+NN)" */
	{ PR_STUDY,    64, prt_study },    /* "Study" */
	{ PR_DEPTH,    70, prt_depth },    /* "Lev NNN" / "NNNN ft" */
	{ PR_OPPOSE_ELEMENTS, 80, prt_elements }, /* Acid Elec Fire Cold Pois */
};


/*
 * Print the status line.
 */
static void display_statusline(void)
{
	int row = Term->hgt - 1;
	size_t i;

	/* Display those which need redrawing */
	for (i = 0; i < N_ELEMENTS(status_handlers); i++)
	{
		const struct status_handler_t *hnd = &status_handlers[i];

		//if (p_ptr->redraw & hnd->flag)
		{
			p_ptr->redraw &= ~(hnd->flag);
			hnd->hook(row, hnd->column);
		}
	}

	return;
}




/*** Subwindow display functions ***/

/*
 * Hack -- display player in sub-windows (mode 0)
 */
static void fix_player_0(void)
{
	/* Display player */
	display_player(0);
}


/*
 * Hack -- display player in sub-windows (mode 1)
 */
static void fix_player_1(void)
{
	/* Display flags */
	display_player(1);
}


/*
 * Hack - Display the left-hand-side of the main term in a separate window
 */
static void fix_frame_compact(void)
{
	int row = 0;
	int col = 0;
	int i;

	/* Race and Class */
	prt_field(p_name + rp_ptr->name, row++, col);
	prt_field(c_name + cp_ptr->name, row++, col);

	/* Title */
	prt_title(row++, col);

	/* Level/Experience */
	prt_level(row++, col);
	prt_exp(row++, col);

	/* Gold */
	prt_gold(row++, col);

	/* Equippy chars */
	prt_equippy(row++, col);

	/* All Stats */
	for (i = 0; i < A_MAX; i++) prt_stat(i, row++, col);

	/* Empty row */
	row++;

	/* Armor */
	prt_ac(row++, col);

	/* Hitpoints */
	prt_hp(row++, col);

	/* Spellpoints */
	prt_sp(row++, col);

    /* silver poison */
    prt_sil(row++, col);

    /* slime level */
    prt_slime(row++, col);

	/* Cut */
	prt_cut(row++, col);

	/* Stun */
	prt_stun(row++, col);

	/* Monster health */
	prt_health(row++, col);
}


/*
 * Hack -- display recent messages in sub-windows
 *
 * Adjust for width and split messages.  XXX XXX XXX
 */
static void fix_message(void)
{
	int i;
	int w, h;
	int x, y;

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
	/* Redraw map */
	display_map(NULL, NULL);
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
	/* Display monster race info */
	if (p_ptr->monster_race_idx)
		display_roff(p_ptr->monster_race_idx);
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(void)
{
	/* Display object type info */
	if (p_ptr->object_kind_idx)
		display_koff(p_ptr->object_kind_idx);
}


/*
 * Print the status display subwindow
 */
static void fix_status(void)
{
	int row = 0;
	size_t i;

	for (i = 0; i < N_ELEMENTS(status_handlers) - 1; i++)
	{
		status_handlers[i].hook(row, 0);
		row++;
	}
}




/*
 * Struct of subwindow display handlers.
 */
static const struct win_handler_t
{
	u32b flag;
	void (*hook)(void);
} win_handlers[] =
{
	{ PW_INVEN, display_inven },		/* Display inventory */
	{ PW_EQUIP, display_equip },		/* Display equipment */
	{ PW_PLAYER_0, fix_player_0 },
	{ PW_PLAYER_1, fix_player_1 },
	{ PW_PLAYER_2, fix_frame_compact },
#if 0
	{ PW_MAP, fix_map },				/* The maps are always up-to-date */
#endif
	{ PW_MESSAGE, fix_message },
	{ PW_OVERHEAD, fix_overhead },
	{ PW_MONSTER, fix_monster },
	{ PW_OBJECT, fix_object },
	{ PW_MONLIST, display_monlist },	/* Display visible monsters */
	{ PW_STATUS, fix_status }		/* Display status lines */
};


/*
 * Handle "p_ptr->window"
 */
void window_stuff(void)
{
	size_t i, j;
	u32b mask = 0L;
	term *old = Term;

	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Scan windows */
	for (i = 0; i < ANGBAND_TERM_MAX; i++)
	{
		/* Ignore non-windows */
		if (!angband_term[i]) break;

		/* Save usable flags */
		mask |= op_ptr->window_flag[i];
	}

	/* Apply usable flags */
	p_ptr->window &= (mask);

	/* Nothing to do */
	if (!p_ptr->window) return;


	/* Walk through all the handlers */
	for (i = 0; i < N_ELEMENTS(win_handlers); i++)
	{
		const struct win_handler_t *hnd = &win_handlers[i];

		/* Don't bother if it has no flags set */
		if (!(p_ptr->window & hnd->flag)) continue;

		/* Iterate over all the terms */
		for (j = 0; j < ANGBAND_TERM_MAX; j++)
		{
			/* No window */
			if (!angband_term[j]) continue;

			/* No relevant flags */
			if (!(op_ptr->window_flag[j] & hnd->flag)) continue;

			/* Call the hook */
			Term_activate(angband_term[j]);
			hnd->hook();
			Term_fresh();
		}

		/* Forget the flag */
		p_ptr->window &= ~(hnd->flag);
	}

	/* Restore old terminal */
	Term_activate(old);
}




/*** Update flag handler functions ***/

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
	int percent_spells;

	const magic_type *s_ptr;

	s16b old_spells;

	cptr p = ((cp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");


	/* Hack -- must be literate */
	if (!cp_ptr->spell_book) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Save the new_spells value */
	old_spells = p_ptr->new_spells;


	/* Determine the number of spells allowed */
	levels = p_ptr->lev - cp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Number of 1/100 spells per level */
	percent_spells = adj_mag_study[p_ptr->stat_ind[cp_ptr->spell_stat]];

	/* Extract total allowed spells (rounded up) */
	num_allowed = (((percent_spells * levels) + 50) / 100);

	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < PY_MAX_SPELLS; j++)
	{
		/* Count known spells */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
		{
			num_known++;
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;



	/* Forget spells which are too hard */
	for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
	{
		/* Get the spell */
		j = p_ptr->spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

		/* Is it known? */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
		{
			/* Mark as forgotten */
			p_ptr->spell_flags[j] |= PY_SPELL_FORGOTTEN;

			/* No longer known */
			p_ptr->spell_flags[j] &= ~PY_SPELL_LEARNED;

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
			           get_spell_name(cp_ptr->spell_book, j));

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Get the (i+1)th spell learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
		{
			/* Mark as forgotten */
			p_ptr->spell_flags[j] |= PY_SPELL_FORGOTTEN;

			/* No longer known */
			p_ptr->spell_flags[j] &= ~PY_SPELL_LEARNED;

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
			           get_spell_name(cp_ptr->spell_book, j));

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Get the next spell we learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) break;

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* First set of spells */
		if (p_ptr->spell_flags[j] & PY_SPELL_FORGOTTEN)
		{
			/* No longer forgotten */
			p_ptr->spell_flags[j] &= ~PY_SPELL_FORGOTTEN;

			/* Known once more */
			p_ptr->spell_flags[j] |= PY_SPELL_LEARNED;

			/* Message */
			msg_format("You have remembered the %s of %s.",
			           p, get_spell_name(cp_ptr->spell_book, j));

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < PY_MAX_SPELLS; j++)
	{
		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* Skip spells we already know */
		if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
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
			msg_format("You can learn %d more %s%s.",
			           p_ptr->new_spells, p,
			           (p_ptr->new_spells != 1) ? "s" : "");
		}

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);

		/* Redraw object recall */
		p_ptr->window |= (PW_OBJECT);
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
	if (!cp_ptr->spell_book) return;


	/* Extract "effective" player level */
	levels = (p_ptr->lev - cp_ptr->spell_first) + 1;

	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana */
	msp = (long)adj_mag_mana[p_ptr->stat_ind[cp_ptr->spell_stat]] * levels / 100;
	
	/* war mages get 1.5x mana */
	if ((cp_ptr->flags & CF_POWER_SHIELD) && (cp_ptr->spell_book == TV_MAGIC_BOOK))
    {
       if (msp < 4) msp += 2;
       else msp = (msp*3)/2;
    }

	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Process gloves for those disturbed by them */
	if (cp_ptr->flags & CF_CUMBER_GLOVE)
	{
		u32b f1, f2, f3, f4;

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, &f1, &f2, &f3, &f4);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx &&
		    !(f3 & (TR3_FREE_ACT)) &&
		    !(f2 & (TR2_MAGIC_MASTERY)) &&
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
	if (old_cumber_glove != p_ptr->cumber_glove)
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
	}


	/* Take note when "armor state" changes */
	if (old_cumber_armor != p_ptr->cumber_armor)
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
	}
}


/*
 * Calculate the players (maximal) hit points
 *
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
	long bonus;
	int mhp;

	/* Get "1/100th hitpoint bonus per level" value */
	bonus = adj_con_mhp[p_ptr->stat_ind[A_CON]];

	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 100);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}


/*
 * Calculate and set the current light radius.
 *
 * The brightest wielded object counts as the light source; radii do not add
 * up anymore.
 *
 * Note that a cursed light source no longer emits light.
 */
static void calc_torch(void)
{
	int i;

	s16b old_lite = p_ptr->cur_lite;
	bool burn_light = TRUE;

	s16b new_lite = 0;
	int extra_lite = 0;



	/* Ascertain lightness if in the town */
	if (!p_ptr->depth && ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)))
		burn_light = FALSE;


	/* Examine all wielded objects, use the brightest */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3, f4;

		int amt = 0;
		object_type *o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3, &f4);

		/* Cursed objects emit no light */
		if (o_ptr->ident & IDENT_CURSED)
		{
			amt = 0;
		}

		/* Examine actual lites */
		else if (o_ptr->tval == TV_LITE)
		{
			int flag_inc = (f3 & TR3_LITE) ? 1 : 0;

			/* Artifact Lites provide permanent bright light */
			if (artifact_p(o_ptr))
			 	amt = 3 + flag_inc;

			/* Non-artifact lights and those without fuel provide no light */
			else if (!burn_light || o_ptr->timeout == 0)
				amt = 0;

			/* All lit lights provide at least radius 2 light */
			else
			{
				amt = 2 + flag_inc;

				/* Torches below 1500 fuel provide less light */
				if (o_ptr->sval == SV_LITE_TORCH && o_ptr->timeout < (FUEL_TORCH / 4))
				    amt--;
				
				/* Lights of darkvision give less light */
				if ((o_ptr->sval == SV_LITE_TORCH) || (o_ptr->sval == SV_LITE_LANTERN))
				{
                   if ((amt > 1) && (f3 & TR3_DARKVIS))
                   amt--;
                }
			}
		}

		else
		{
			/* LITE flag on a non-cursed non-light always increases radius */
			if (f3 & TR3_LITE) extra_lite++;
		}

		/* Alter p_ptr->cur_lite if reasonable */
		if (new_lite < amt)
		    new_lite = amt;
	}

	/* Add bonus from LITE flags */
	new_lite += extra_lite;
	
	/* Add bonus for True sight */
	if (p_ptr->timed[TMD_TSIGHT]) new_lite++;

	/* Limit light */
	new_lite = MIN(new_lite, 5);
	new_lite = MAX(new_lite, 0);

	/* Notice changes in the "lite radius" */
	if (old_lite != new_lite)
	{
		/* Update the visuals */
		p_ptr->cur_lite = new_lite;
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
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
	int extra_might;
	int heavy_bonus;

	int old_stat_top[A_MAX];
	int old_stat_use[A_MAX];
	int old_stat_ind[A_MAX];

	bool old_heavy_shoot;
	bool old_heavy_wield;
	bool old_icky_wield;

	object_type *o_ptr;

	u32b f1, f2, f3, f4;


	/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	/* Save the old stats */
	for (i = 0; i < A_MAX; i++)
	{
		old_stat_top[i] = p_ptr->stat_top[i];
		old_stat_use[i] = p_ptr->stat_use[i];
		old_stat_ind[i] = p_ptr->stat_ind[i];
	}

	old_heavy_shoot = p_ptr->heavy_shoot;
	old_heavy_wield = p_ptr->heavy_wield;
	old_icky_wield = p_ptr->icky_wield;


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
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->bless_blade = FALSE;
    p_ptr->stopregen = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->darkvis = FALSE;
	p_ptr->nice = FALSE;
	p_ptr->peace = FALSE;
	p_ptr->telepathy = FALSE;
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
	p_ptr->weakresist_pois = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->resist_charm = FALSE;
	p_ptr->resist_frenzy = FALSE;
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
    p_ptr->brand_cold = FALSE;
    p_ptr->brand_fire = FALSE;
    p_ptr->brand_acid = FALSE;
    p_ptr->brand_elec = FALSE;
    p_ptr->brand_pois = FALSE;
    p_ptr->accident = FALSE;
    goodweap = 0;
    badweap = 0;

	/*** Extract race/class info ***/

	/* Base alertness (racial still uses infra variable name) */
	/* alertness not infravision anymore */
 	p_ptr->see_infra = rp_ptr->infra + cp_ptr->calert;

	/* Base skill -- disarming */
	p_ptr->skills[SKILL_DIS] = rp_ptr->r_dis + cp_ptr->c_dis;

	/* Base skill -- magic devices */
	p_ptr->skills[SKILL_DEV] = rp_ptr->r_dev + cp_ptr->c_dev;

	/* Base skill -- saving throw */
	p_ptr->skills[SKILL_SAV] = rp_ptr->r_sav + cp_ptr->c_sav;

	/* Base skill -- stealth */
	p_ptr->skills[SKILL_STL] = rp_ptr->r_stl + cp_ptr->c_stl;

	/* Base skill -- searching ability */
	p_ptr->skills[SKILL_SRH] = rp_ptr->r_srh + cp_ptr->c_srh;

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

	/*** Analyze player ***/

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3, &f4);

	/* Good flags */
	if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
	if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
	if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
	if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
	if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
	if (f3 & (TR3_FREE_ACT)) p_ptr->free_act = TRUE;
	if (f3 & (TR3_HOLD_LIFE)) p_ptr->hold_life = TRUE;
	if (f3 & (TR3_DARKVIS)) p_ptr->darkvis = TRUE;
	if (f2 & (TR2_NICE)) p_ptr->nice = TRUE;
	if (f2 & (TR2_PEACE)) p_ptr->peace = TRUE;

	/* wierd flags */
	if (f3 & (TR3_BLESSED)) p_ptr->bless_blade = TRUE;

	/* Bad flags */
	if (f2 & (TR2_DANGER)) p_ptr->accident = TRUE;
	if (f3 & (TR3_IMPACT)) p_ptr->impact = TRUE;
	if (f3 & (TR3_AGGRAVATE))
    {
	   if (cp_ptr->flags & CF_CLASS_SPEED)
	   {
       p_ptr->skills[SKILL_STL] -= 4;
       }
       else
       {
       p_ptr->aggravate = TRUE;
       }
    }
	if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
	if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
	if (f3 & (TR3_STOPREGEN)) p_ptr->stopregen = TRUE;

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
	if (f4 & (TR4_RES_POIS)) p_ptr->resist_pois = TRUE;
	else if (f2 & (TR2_RES_POISB)) p_ptr->weakresist_pois = TRUE;
	if (f4 & (TR4_RES_FEAR)) p_ptr->resist_fear = TRUE;
	if (f4 & (TR4_RES_CHARM)) p_ptr->resist_charm = TRUE;
	if (f4 & (TR4_RES_LITE)) p_ptr->resist_lite = TRUE;
	if (f4 & (TR4_RES_DARK)) p_ptr->resist_dark = TRUE;
	if (f4 & (TR4_RES_BLIND)) p_ptr->resist_blind = TRUE;
	if (f4 & (TR4_RES_CONFU)) p_ptr->resist_confu = TRUE;
	if (f4 & (TR4_RES_SOUND)) p_ptr->resist_sound = TRUE;
	if (f4 & (TR4_RES_SHARD)) p_ptr->resist_shard = TRUE;
	if (f4 & (TR4_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
	if (f4 & (TR4_RES_NETHR)) p_ptr->resist_nethr = TRUE;
	if (f4 & (TR4_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
	if (f4 & (TR4_RES_DISEN)) p_ptr->resist_disen = TRUE;

	/* Sustain flags */
	if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
	if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
	if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
	if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
	if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
	if (f2 & (TR2_SUST_CHR)) p_ptr->sustain_chr = TRUE;


	/*** Analyze equipment ***/

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3, &f4);

		/* Affect stats */
		if (f1 & (TR1_STR)) p_ptr->stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT)) p_ptr->stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS)) p_ptr->stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX)) p_ptr->stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON)) p_ptr->stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_CHR)) p_ptr->stat_add[A_CHR] += o_ptr->pval;

		/* Affect stealth */
		if (f1 & (TR1_STEALTH)) p_ptr->skills[SKILL_STL] += o_ptr->pval;

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skills[SKILL_SRH] += (o_ptr->pval * 5);
		
		/* affect magic device mastery */
		if (f2 & (TR2_MAGIC_MASTERY))
		{
			if (p_ptr->skills[SKILL_DEV] < 22) p_ptr->skills[SKILL_DEV] += 15;
			else if (p_ptr->skills[SKILL_DEV] < 32) p_ptr->skills[SKILL_DEV] += 12;
			else if (p_ptr->skills[SKILL_DEV] < 42) p_ptr->skills[SKILL_DEV] += 9;
			else p_ptr->skills[SKILL_DEV] += 6;
        }

		/* Affect searching frequency (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skills[SKILL_FOS] += (o_ptr->pval * 5);

		/* Affect alertness */
		if (f1 & (TR1_INFRA)) p_ptr->see_infra += o_ptr->pval * 5;

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL)) p_ptr->skills[SKILL_DIG] += (o_ptr->pval * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED)) p_ptr->pspeed += o_ptr->pval;

		/* Affect blows */
		if (f1 & (TR1_BLOWS)) extra_blows += o_ptr->pval;

		/* Affect shots */
		if (f1 & (TR1_SHOTS)) extra_shots += o_ptr->pval;

		/* Affect Might */
		if (f1 & (TR1_MIGHT)) extra_might += o_ptr->pval;

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
		if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
		if (f3 & (TR3_FREE_ACT)) p_ptr->free_act = TRUE;
		if (f3 & (TR3_HOLD_LIFE)) p_ptr->hold_life = TRUE;
	    if (f3 & (TR3_DARKVIS)) p_ptr->darkvis = TRUE;
	    if (f2 & (TR2_NICE)) p_ptr->nice = TRUE;

        /* DJA: melee branding from elemental rings */	    
	    if (f1 & (TR1_BRAND_POIS)) p_ptr->brand_pois = TRUE;
	    if (f1 & (TR1_BRAND_FIRE)) p_ptr->brand_fire = TRUE;
	    if (f1 & (TR1_BRAND_ACID)) p_ptr->brand_acid = TRUE;
	    if (f1 & (TR1_BRAND_ELEC)) p_ptr->brand_elec = TRUE;
	    if (f1 & (TR1_BRAND_COLD)) p_ptr->brand_cold = TRUE;

	    /* Sentient Object & BLESSED flags */
	    if (f3 & (TR3_BLESSED)) p_ptr->bless_blade = TRUE;
	    if (f3 & (TR3_GOOD_WEAP)) goodweap += 1;
	    if (f3 & (TR3_BAD_WEAP)) badweap -= 1;

		/* Bad flags */
		if (f3 & (TR3_IMPACT)) p_ptr->impact = TRUE;
	    if (f3 & (TR3_AGGRAVATE))
        {
	      if (cp_ptr->flags & CF_CLASS_SPEED)
	      {
             p_ptr->skills[SKILL_STL] -= 5;
          }
          else
          {
             p_ptr->aggravate = TRUE;
          }
        }
		if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
  	    if (f3 & (TR3_STOPREGEN)) p_ptr->stopregen = TRUE;

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
		if (f4 & (TR4_RES_POIS)) p_ptr->resist_pois = TRUE;
	    else if (f2 & (TR2_RES_POISB)) p_ptr->weakresist_pois = TRUE;
		if (f4 & (TR4_RES_FEAR)) p_ptr->resist_fear = TRUE;
		if (f4 & (TR4_RES_CHARM)) p_ptr->resist_charm = TRUE;
		if (f4 & (TR4_RES_LITE)) p_ptr->resist_lite = TRUE;
		if (f4 & (TR4_RES_DARK)) p_ptr->resist_dark = TRUE;
		if (f4 & (TR4_RES_BLIND)) p_ptr->resist_blind = TRUE;
		if (f4 & (TR4_RES_CONFU)) p_ptr->resist_confu = TRUE;
		if (f4 & (TR4_RES_SOUND)) p_ptr->resist_sound = TRUE;
		if (f4 & (TR4_RES_SHARD)) p_ptr->resist_shard = TRUE;
		if (f4 & (TR4_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
		if (f4 & (TR4_RES_NETHR)) p_ptr->resist_nethr = TRUE;
		if (f4 & (TR4_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
		if (f4 & (TR4_RES_DISEN)) p_ptr->resist_disen = TRUE;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
		if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
		if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
		if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
		if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
		if (f2 & (TR2_SUST_CHR)) p_ptr->sustain_chr = TRUE;

	    /* check niceness */
	    if ((cp_ptr->spell_book == TV_DARK_BOOK) ||
	    (f3 & (TR3_BAD_WEAP)))
	    {
           p_ptr->nice = FALSE;
        }

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


    /*** track sentient equipment ***/    
    if (cp_ptr->spell_book == TV_DARK_BOOK)
    {
       if (goodweap > 0) magicmod = 2;
       if (badweap > 0) magicmod = 4;
       if ((goodweap > 0) && (badweap > 0))
       {
          if (goodweap == badweap) magicmod = 1;
          if (goodweap > badweap) magicmod = 3;
          if (goodweap < badweap) magicmod = 0;
       }
    }
	if (cp_ptr->spell_book == TV_PRAYER_BOOK)
	{
       if (goodweap > 0) magicmod = 6;
       if (badweap > 0) magicmod = 9;
       if ((goodweap > 0) && (badweap > 0))
       {
          if (goodweap == badweap) magicmod = 10;
          if (goodweap > badweap) magicmod = 7;
          if (goodweap < badweap) magicmod = 8;
       }
    }          
    if ((cp_ptr->spell_book != TV_PRAYER_BOOK) && (cp_ptr->spell_book != TV_DARK_BOOK))
	{
       if (badweap > 0) magicmod = 18;
       if (goodweap > 0) magicmod = 19;
       if ((goodweap > 0) && (badweap > 0))
       {
          if (goodweap == badweap) magicmod = 20;
          if (goodweap < badweap) magicmod = 21;
          if (goodweap > badweap) magicmod = 22;
       }
    }
    

	/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int add, top, use, ind;

		/* Extract modifier */
		add = p_ptr->stat_add[i];

		/* Maximize mode */
		if (adult_maximize)
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
		if (use <= 18) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

		/* Save the new index */
		p_ptr->stat_ind[i] = ind;
	}

	/*** Luck check ***/
	if (p_ptr->luck > PY_LUCKCHECK_MAX) p_ptr->luck = PY_LUCKCHECK_MAX;
	if (p_ptr->luck < PY_LUCKCHECK_MIN) p_ptr->luck = PY_LUCKCHECK_MIN;
	/* black magic users have a lower max luck */
    /* and those who pray don't rely on luck as much */
	if (((cp_ptr->spell_book == TV_DARK_BOOK) || (cp_ptr->spell_book == TV_PRAYER_BOOK)) && 
       (p_ptr->luck > PY_LUCKCHECK_MAX - 2))
	{
       p_ptr->luck = PY_LUCKCHECK_MAX - 2;
    }
    /* further hit for black magic users*/
    if ((cp_ptr->spell_book == TV_DARK_BOOK) && (p_ptr->luck > PY_LUCKCHECK_MAX - 4) &&
       (randint(1000) < 2))
	{
       p_ptr->luck -= 1;
    }

    /* reset goodluck and badluck */
    goodluck = 0;
    if (p_ptr->luck > 20) goodluck = p_ptr->luck - 20;
    badluck = 0;
    if (p_ptr->luck < 20) badluck = 20 - p_ptr->luck;


	/*** Temporary flags ***/

	/* Apply temporary "stun" */
	if (p_ptr->timed[TMD_STUN] > 50)
	{
		p_ptr->to_h -= 20;
		p_ptr->dis_to_h -= 20;
		p_ptr->to_d -= 20;
		p_ptr->dis_to_d -= 20;
		p_ptr->see_infra -= 4;
	}
	else if (p_ptr->timed[TMD_STUN])
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
		p_ptr->see_infra -= 2;
	}

	/* Invulnerability */
	if (p_ptr->timed[TMD_INVULN])
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
	}

	/* enhanced magic (most of its effects are elsewhere) */
	/* allows reading while blind and enhances spellcasting */
	/* less effect than magic mastery in the area of devices */
	if (p_ptr->timed[TMD_BRAIL])
	{
		if (p_ptr->skills[SKILL_DEV] < 22) p_ptr->skills[SKILL_DEV] += 10;
		else if (p_ptr->skills[SKILL_DEV] < 35) p_ptr->skills[SKILL_DEV] += 8;
		else if (p_ptr->skills[SKILL_DEV] < 45) p_ptr->skills[SKILL_DEV] += 6;
		else p_ptr->skills[SKILL_DEV] += 4;
    }

	/* Temporary blessing */
	if (p_ptr->timed[TMD_BLESSED])
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
        p_ptr->resist_frenzy = TRUE;
		p_ptr->see_infra += 2;
		goodluck += 1;
	}

	/* Temporary enhanced roguishness */
	if (p_ptr->timed[TMD_SUPER_ROGUE])
	{
        p_ptr->resist_frenzy = TRUE;
		p_ptr->to_a += 4;
		p_ptr->dis_to_a += 4;
		goodluck += 2;
        if (goodluck < 12)
        {
           p_ptr->pspeed += 2;
           p_ptr->skills[SKILL_STL] += 1;
		   p_ptr->see_infra += 10;
        }
        else
        {
           p_ptr->pspeed += goodluck / 4;
           p_ptr->skills[SKILL_STL] += goodluck / 6;
		   p_ptr->see_infra += goodluck - 2;
        }
	}

	/* Temporary shield */
	if (p_ptr->timed[TMD_SHIELD])
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}
	/* Temporary 'mana defence' (weaker shield) */
	else if (p_ptr->timed[TMD_WSHIELD])
	{
		p_ptr->to_a += 20;
		p_ptr->dis_to_a += 20;
	}
    /* ac doesn't stack with shield, but it always gives +1 stealth. */ 
	if (p_ptr->timed[TMD_WSHIELD])
    {
        p_ptr->skills[SKILL_STL] += 1;
    }

	/* Temporary "Hero" */
	if (p_ptr->timed[TMD_HERO])
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
	}

	/* Temporary "Berserk" */
	if (p_ptr->timed[TMD_SHERO])
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
	    p_ptr->see_infra -= 2;
	}

	/* Temporary "Slip into the Shadows" (extra stealth) */
	if (p_ptr->timed[TMD_SHADOW])
	{
       p_ptr->skills[SKILL_STL] = (p_ptr->skills[SKILL_STL] * 2);
	   p_ptr->to_a += 2;
	   p_ptr->dis_to_a += 2;
	   p_ptr->see_infra += 4;
	}

	/* Temporary "Frenzy" */
	if (p_ptr->timed[TMD_FRENZY])
	{
		p_ptr->to_d += 5;
		p_ptr->dis_to_d += 5;
		p_ptr->to_h -= 6;
		p_ptr->dis_to_h -= 6;
		p_ptr->to_a -= 12;
		p_ptr->dis_to_a -= 12;
	    p_ptr->see_infra -= 5;
        p_ptr->skills[SKILL_SAV] = p_ptr->skills[SKILL_SAV] - 4;
    }

	/* Temporary "fast" */
	if (p_ptr->timed[TMD_FAST])
	{
		p_ptr->pspeed += 10;
	}

	/* Temporary "slow" */
	if (p_ptr->timed[TMD_SLOW])
	{
		p_ptr->pspeed -= 10;
	}

	/* Temporary "stoneskin" */
	if (p_ptr->timed[TMD_STONESKIN])
	{
		p_ptr->pspeed -= 5;
		p_ptr->to_a += 30;
		p_ptr->dis_to_a += 30;
	}

    /* Temporary "desperate to escape" (cannot melee, shoot, or cast) */
	if (p_ptr->timed[TMD_TERROR])
	{
		p_ptr->pspeed += 11;
		p_ptr->to_a += 30;
		p_ptr->dis_to_a += 27;
	    p_ptr->see_infra -= 2;
	}

	/* Temporary speed adjustment (not always positive) */
	if (p_ptr->timed[TMD_ADJUST])
	{
		p_ptr->pspeed += spadjust;
	}

	/* sustain speed: resist slowing, inertia, and hasting */
	/* (undecided if it should affect TMD_ADJUST, it does for now */
	/* allows the other effects of TMD_TERROR */
	if (p_ptr->timed[TMD_SUST_SPEED])
	{
		if (p_ptr->timed[TMD_FAST]) (void)clear_timed(TMD_FAST);
		if (p_ptr->timed[TMD_SLOW]) (void)clear_timed(TMD_SLOW);
		if (p_ptr->timed[TMD_ADJUST]) (void)clear_timed(TMD_ADJUST);
		if (p_ptr->timed[TMD_TERROR]) p_ptr->pspeed -= 11;
	}

	/* Temporary see invisible */
	if (p_ptr->timed[TMD_SINVIS])
	{
		p_ptr->see_inv = TRUE;

		/* this part for 'see all foes' spell: */
		if ((cp_ptr->spell_book == TV_DARK_BOOK) && (p_ptr->lev > 32)) p_ptr->see_infra += 16;
	}

	/* Timed "True Sight" */
	/* also makes you resist halucenation and slightly lowers chaos dmg */
	if (p_ptr->timed[TMD_TSIGHT])
	{
		p_ptr->see_inv = TRUE;
        p_ptr->resist_blind = TRUE;
		p_ptr->to_h += 1;
		p_ptr->dis_to_h += 1;
	    p_ptr->see_infra += 20;
	}
	
	/* timed "sanctify for battle " */
	/* gives slay demon and slay evil (also slay undead in normal DJA) */
	if (p_ptr->timed[TMD_SANCTIFY])
	{
        p_ptr->resist_fear = TRUE;
        p_ptr->resist_charm = TRUE;
        p_ptr->resist_frenzy = TRUE;
	}
	
	/* spirit of the balrog, does extra fire and dark damage in melee */
	if (p_ptr->timed[TMD_BALROG])
	{
        p_ptr->resist_fear = TRUE;
        p_ptr->resist_charm = TRUE;
	}
	
	/* timed partial poison resistance */
	if (p_ptr->timed[TMD_WOPP_POIS])
	{
        p_ptr->weakresist_pois = TRUE;
	}
	
	/* timed nether resistance */
	if (p_ptr->timed[TMD_OPP_NETHR])
	{
        p_ptr->resist_nethr = TRUE;
    }
	
	/* timed dark resistance */
	if (p_ptr->timed[TMD_OPP_DARK])
	{
        p_ptr->resist_dark = TRUE;
    }
    
    /* beomce lich black magic spell */
    /* lichs are also immune to cold and resistant to dark and nether */
    /* and aggravate animals and creatures of light */
    /* and take extra damage from light attacks */
	if (p_ptr->timed[TMD_BECOME_LICH])
	{
		p_ptr->resist_fear = TRUE;
		p_ptr->resist_charm = TRUE;
	    p_ptr->skills[SKILL_SAV] += 3;
		p_ptr->to_a += 3;
		p_ptr->dis_to_a += 3;
	    p_ptr->see_infra += 6;
	    badluck += 1;
	    
	    if ((goodweap > 1) && (randint(3) == 1))
	    {
           msg_print("Some pieces of your equipment hate your current form.");
           take_hit(goodweap, "Wearing good sentient equipment while in an evil form");
        }
	    else if ((goodweap == 1) && (randint(4) == 1))
	    {
           msg_print("A piece of your equipment hates your current form.");
           take_hit(1, "Wearing good sentient equipment while in an evil form");
        }
       
	    /* lich cannot have temporary fire resistance */
        if (p_ptr->timed[TMD_OPP_FIRE])
        {
           msg_print("Temporary fire resistance doesn't work while in the form of an undead!");
           (void)clear_timed(TMD_OPP_FIRE);
        }
    }

	/* cannot have timed cold resistance and timed fire immunity */
	if (p_ptr->timed[TMD_OPP_COLD] && p_ptr->timed[TMD_IMM_FIRE])
	{
        msg_print("Temporary cold resistance doesn't work with temporary fire immunity!");
		(void)clear_timed(TMD_OPP_COLD);
	}
	
	/* timed hold life */
	if (p_ptr->timed[TMD_HOLDLIFE])
	{
        p_ptr->hold_life = TRUE;
    }

	/* timed telepathy */
	if (p_ptr->timed[TMD_ESP])
	{
        p_ptr->telepathy = TRUE;
	}

	/* timed second sight (telepathy only while blind) */
	if ((p_ptr->timed[TMD_MESP]) && (p_ptr->timed[TMD_BLIND]))
	{
        p_ptr->telepathy = TRUE;
	}

	/* "1st sight and 2nd thoughts" */
	/* (strengthens alertness and saves and inhibits magic and telepathy) */
	if (p_ptr->timed[TMD_2ND_THOUGHT])
	{
        (void)clear_timed(TMD_BLIND);
        p_ptr->resist_blind = TRUE;
	    p_ptr->see_infra += 15;
	    p_ptr->skills[SKILL_SAV] += 12;
        if ((p_ptr->timed[TMD_ESP]) || (p_ptr->timed[TMD_SINVIS]))
        {
           msg_print("The first sight blocks your second sight.");
           (void)clear_timed(TMD_ESP);
           (void)clear_timed(TMD_SINVIS);
        }
        (void)clear_timed(TMD_MESP);
        p_ptr->see_inv = FALSE;
        p_ptr->telepathy = FALSE;
	}

	/* Temporary alertness boost */
	if (p_ptr->timed[TMD_SINFRA])
	{
		p_ptr->see_infra += 15;
	}
	else if (p_ptr->timed[TMD_WSINFRA])
	{
		p_ptr->see_infra += 5;
	}
	
    /* Peace reduces number of blows by 1, makes criticals less likely */
    /* and prevents heroism, berserker, and frenzy */
    if (p_ptr->peace)
    {
	   (void)clear_timed(TMD_HERO);
	   (void)clear_timed(TMD_SHERO);
	   (void)clear_timed(TMD_FRENZY);
	   if (badluck > 0) badluck -= 1;
	   else goodluck += 1;
	   p_ptr->skills[SKILL_SAV] += 4;
       p_ptr->dis_to_h -= 2;
       p_ptr->to_h -= 2;
       if ((p_ptr->aggravate) && (goodluck > 6)) p_ptr->aggravate = FALSE;
       if (randint(12000) < goodluck * 2) inc_timed(TMD_SPHERE_CHARM, randint(10 + goodluck) + 15);
    }

	/*** Special flags ***/
	
    /* thieves' extra speed */
	if (cp_ptr->flags & CF_CLASS_SPEED)
	{
        p_ptr->pspeed += 2;
                      
		/* Extra speed at level 20 */
		if (p_ptr->lev >= 20) p_ptr->pspeed += 1;

		/* Extra speed at level 40 */
		if (p_ptr->lev >= 40) p_ptr->pspeed += 1;
	}

    /* Assassin gets extra stealth at L26 */
	if (cp_ptr->flags & CF_ASSASSIN)
	{
		if (p_ptr->lev >= 26) p_ptr->skills[SKILL_STL] += 1;
    }
    
	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->timed[TMD_HERO] || p_ptr->timed[TMD_SHERO])
	{
		p_ptr->resist_fear = TRUE;
	}
	
	/* Hack -- Berserk -> Res charm */
	if (p_ptr->timed[TMD_SHERO])
	{
		p_ptr->resist_charm = TRUE;
	}

    /* side effects of halucenation */
	if (p_ptr->timed[TMD_IMAGE])
    {
       p_ptr->see_infra -= 8; /* less alertness */
	   goodluck += 2; /* temporary luck*/
    }
   
    /* side effects of being charmed */
	if (p_ptr->timed[TMD_CHARM])
    {
       p_ptr->see_infra -= 3;
	   goodluck += 1;
    }

    /* possible side effects of some black magic */
	if (p_ptr->timed[TMD_WITCH])
    {
       p_ptr->see_infra -= 2;
	   badluck += 1;
    }

    /*** some Effects of Luck ***/
    /* this will cause these things to possibly change every time */
    /* this function is called */
/* (spellswitch9999 prevents skill numbers from changing on birth screen) */
    if (spellswitch != 9999)
    {
       p_ptr->to_h += randint(goodluck / 4);
       p_ptr->to_h -= randint(badluck / 4);
       if (randint(100) < 80) p_ptr->skills[SKILL_THT] += goodluck / 3;
       if (randint(100) < 50) p_ptr->skills[SKILL_THT] -= badluck / 3;
	   if ((goodluck > 10) || (badluck > 10) && (randint(100) < 66))
       {
          p_ptr->skills[SKILL_DEV] += 1 + goodluck / 6;
          p_ptr->skills[SKILL_DEV] -= 1 + badluck / 6;
       }
    }

    /*** Player alertness to monsters (palert) ***/
    /* how alert to monsters is the player? */
    if (p_ptr->timed[TMD_CONFUSED]) p_ptr->see_infra -= 4;
    if (p_ptr->searching) p_ptr->see_infra -= 2;
	
    if (p_ptr->see_infra < 1) p_ptr->see_infra = 1;
    palert = p_ptr->see_infra + randint(goodluck/2);
	if (p_ptr->silver >= PY_SILVER_LEVELTWO) palert -= 6;
	else if (p_ptr->silver >= PY_SILVER_LEVELONE) palert -= 3;
	if ((spellswitch > 90) && (spellswitch < 96)) palert += (spellswitch - 90) * 2;
	if (palert < 2) palert = 2;

	/*** Analyze weight ***/

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* Apply "encumbrance" from weight */
	if (j > i / 2) p_ptr->pspeed -= ((j - (i / 2)) / (i / 10));

	/* Bloating slows the player down */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

	/* Sanity check on extreme speeds */
	if (p_ptr->pspeed < 0) p_ptr->pspeed = 0;
	if (p_ptr->pspeed > 199) p_ptr->pspeed = 199;

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
	p_ptr->skills[SKILL_STL] += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skills[SKILL_DIS] += adj_dex_dis[p_ptr->stat_ind[A_DEX]];
	p_ptr->skills[SKILL_DIS] += adj_int_dis[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skills[SKILL_DEV] += adj_int_dev[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skills[SKILL_SAV] += adj_wis_sav[p_ptr->stat_ind[A_WIS]];

	/* Affect Skill -- digging (STR) */
	p_ptr->skills[SKILL_DIG] += adj_str_dig[p_ptr->stat_ind[A_STR]];

	/* Affect Skill -- disarming (Level, by Class) */
	p_ptr->skills[SKILL_DIS] += (cp_ptr->x_dis * p_ptr->lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->skills[SKILL_DEV] += (cp_ptr->x_dev * p_ptr->lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->skills[SKILL_SAV] += (cp_ptr->x_sav * p_ptr->lev / 10);

	/* Affect Skill -- stealth (Level, by Class) */
	p_ptr->skills[SKILL_STL] += (cp_ptr->x_stl * p_ptr->lev / 10);

	/* Affect Skill -- search ability (Level, by Class) */
	p_ptr->skills[SKILL_SRH] += (cp_ptr->x_srh * p_ptr->lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->skills[SKILL_FOS] += (cp_ptr->x_fos * p_ptr->lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skills[SKILL_THN] += (cp_ptr->x_thn * p_ptr->lev / 10);
	p_ptr->skills[SKILL_THN] += 2;

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skills[SKILL_THB] += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skills[SKILL_THT] += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skills[SKILL_DIG] < 1) p_ptr->skills[SKILL_DIG] = 1;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skills[SKILL_STL] > 30) p_ptr->skills[SKILL_STL] = 30;
	if (p_ptr->skills[SKILL_STL] < 0) p_ptr->skills[SKILL_STL] = 0;

	/* Apply Skill -- Extract noise from stealth */
	p_ptr->noise = (1L << (30 - p_ptr->skills[SKILL_STL]));

	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];
    
	/*** Analyze current bow ***/

	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

    if (!cp_ptr->flags & CF_HEAVY_BONUS)
    {
    	/* It is hard to carholdry a heavy bow */
	    if (hold < o_ptr->weight / 10)
	    {
		   /* Hard to wield a heavy bow */
		   p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		   p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		   /* Heavy Bow */
		   p_ptr->heavy_shoot = TRUE;
	    }
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
			
			case SV_HANDHELDC:
			{
				p_ptr->ammo_tval = TV_SHOT;
				p_ptr->ammo_mult = 3;
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
			
			case SV_GREAT_BOW:
			{
				p_ptr->ammo_tval = TV_ARROW;
				p_ptr->ammo_mult = 4;
				break;
			}

			/* Light Crossbow and Bolt */
			case SV_LIGHT_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;
				p_ptr->ammo_mult = 3;
				break;
			}
			
			case SV_MINI_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;
				p_ptr->ammo_mult = 2;
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

	/* certain classes are better at using certain types of weapons */
    if ((p_ptr->pclass == 4) && (p_ptr->ammo_tval == TV_ARROW))
    { /* rangers don't get extra shots but still good with bows */
	      p_ptr->to_h += 5;
	      p_ptr->dis_to_h += 5;
    }
    if ((p_ptr->pclass == 6) && (p_ptr->ammo_tval == TV_BOLT) || (p_ptr->ammo_tval == TV_SHOT))
    { /* archers not as good with slings or crossbows */
	      p_ptr->to_h -= 5;
	      p_ptr->dis_to_h -= 5;
    }
    if ((p_ptr->pclass == 5) || (p_ptr->pclass == 13) || (p_ptr->prace == 13) && (p_ptr->ammo_tval == TV_SHOT))
    { /* paladins and assassin don't like slings */
	      p_ptr->to_h -= 2;
	      p_ptr->dis_to_h -= 2;
    }
    if ((p_ptr->pclass == 8) || (p_ptr->pclass == 10) && (p_ptr->ammo_tval == TV_SHOT))
    { /* sling kindof is a priestly weapon */
	      p_ptr->to_h += 2;
	      p_ptr->dis_to_h += 2;
    }
    if ((p_ptr->pclass == 18) || (p_ptr->prace == 3) || (p_ptr->prace == 14) && (p_ptr->ammo_tval == TV_SHOT))
    { /* barbarians and hobbits like slings */
	      p_ptr->to_h += 4;
	      p_ptr->dis_to_h += 4;
    }
    if ((p_ptr->pclass == 13) && (p_ptr->ammo_tval == TV_BOLT))
    { /* assassins like crossbows */
	      p_ptr->to_h += 2;
	      p_ptr->dis_to_h += 2;
    }
    if ((p_ptr->prace == 9) || (p_ptr->prace == 12) || (p_ptr->pclass == 18) && (p_ptr->ammo_tval == TV_BOLT))
    { /* barbarians, high elves and fairies don't */
	      p_ptr->to_h -= 2;
	      p_ptr->dis_to_h -= 2;
    }
    if ((p_ptr->prace == 17) && (p_ptr->ammo_tval == TV_ARROW))
    { /* umber hulks don't use bows */
	      p_ptr->to_h -= 8;
	      p_ptr->dis_to_h -= 8;
    }

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Extra might */
			p_ptr->ammo_mult += extra_might;

			/* Hack -- Archers love Bows */
			if ((cp_ptr->flags & CF_EXTRA_SHOT) &&
			    (p_ptr->ammo_tval == TV_ARROW))
			{
				/* Extra shot at level 20 */
				if (p_ptr->lev >= 20) p_ptr->num_fire++;

				/* Extra shot at level 40 */
				if (p_ptr->lev >= 40) p_ptr->num_fire++;
			}
		}

		/* Require at least one shot */
		if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
	}


	/*** Analyze weapon ***/

	/* Examine the "current weapon" */
	o_ptr = &inventory[INVEN_WIELD];

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

    if (!cp_ptr->flags & CF_HEAVY_BONUS)
    {
	   /* It is hard to hold a heavy weapon */
	   if (hold < o_ptr->weight / 10)
	   {
		/* Hard to wield a heavy weapon */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	   }
    }

	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int div;

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

		/* Get the strength vs weight */
		str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * cp_ptr->att_multiply / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->num_blow > cp_ptr->max_attacks) p_ptr->num_blow = cp_ptr->max_attacks;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

        /* timed extra attack (from spell or mushroom only) */
		if (p_ptr->timed[TMD_XATTACK])
		{
		   p_ptr->num_blow += 1;
        }
		
		/* peace */
        if (p_ptr->peace) p_ptr->num_blow -= 1;

		/* Require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;

		/* Boost digging skill by weapon weight. DJA: blades don't dig well. */
		if (o_ptr->tval == TV_SWORD) p_ptr->skills[SKILL_DIG] += (o_ptr->weight / 25);
		else if (o_ptr->tval == TV_HAFTED) p_ptr->skills[SKILL_DIG] += (o_ptr->weight / 15);
		else p_ptr->skills[SKILL_DIG] += (o_ptr->weight / 10);
	}

	/* Assume okay */
	p_ptr->icky_wield = FALSE;
	
	/* certain classes are better at using certain types of weapons */
    if ((p_ptr->pclass == 6) && (o_ptr->tval == TV_POLEARM))
    { /* archer */
	      p_ptr->to_h -= 1;
	      p_ptr->dis_to_h -= 1;
    }
    if ((p_ptr->pclass == 8) && (o_ptr->tval == TV_SWORD))
    { /* priest: bless_weapon penalty is less than in V, so I added this */
	      p_ptr->to_h -= 1;
	      p_ptr->dis_to_h -= 1;
    }
    if ((p_ptr->pclass == 9) && (o_ptr->tval == TV_POLEARM))
    { /* healer */
	      p_ptr->to_h -= 1;
	      p_ptr->dis_to_h -= 1;
    }
    if (((p_ptr->pclass == 16) || (p_ptr->pclass == 10)) && (o_ptr->tval == TV_STAFF))
    { /* war mages and druids get bonus with magic staffs */
	      p_ptr->to_d += 1;
	      p_ptr->dis_to_d += 1;
	      p_ptr->to_h += 1;
	      p_ptr->dis_to_h += 1;
    }
    if (p_ptr->pclass == 8) /* barbarians */
    {
         /* barbarians fight with spears and axes (also bonus with tusks) */
         if (o_ptr->tval == TV_SWORD)
         {
            p_ptr->to_h -= 2;
	        p_ptr->dis_to_h -= 2;
         }
         if ((o_ptr->tval == TV_POLEARM) || (o_ptr->tval == TV_SKELETON))
         {
	        p_ptr->to_h += 2;
	        p_ptr->dis_to_h += 2;
         }
    }
    if ((p_ptr->pclass == 19) && (o_ptr->tval == TV_SKELETON))
    { /* chaos warrios */
	      p_ptr->to_h += 1;
	      p_ptr->dis_to_h += 1;
    }
    if ((p_ptr->pclass == 32) && (o_ptr->tval == TV_SWORD))
    { /* umber hulks not good with swords */
	      p_ptr->to_h -= 2;
	      p_ptr->dis_to_h -= 2;
    }
	
	/* Barbarians like heavy weapons */
    if (cp_ptr->flags & CF_HEAVY_BONUS)
    {
       if (o_ptr->weight > 99)
       {
          int hbonus = ((o_ptr->weight - 100) / 50) + 1;
	      p_ptr->to_d += hbonus;
	      p_ptr->dis_to_d += hbonus;
       }
       if (o_ptr->weight < 51) /* not as good with light weapons */
       {
	      p_ptr->to_d -= 1;
	      p_ptr->dis_to_d -= 1;
	      p_ptr->to_h -= 1;
	      p_ptr->dis_to_h -= 1;
       }
    }
    
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

        /* good edged weapon reduces combat penalty and prevents icky_wield */
		if (f3 & (TR3_GOOD_WEAP))
		{
             p_ptr->to_d += 1;
             p_ptr->dis_to_d += 1;
             if ((badweap > 0) && (goodweap > badweap)) magicmod = 11;
             if ((badweap > 0) && (goodweap < badweap)) magicmod = 13;
             if (badweap < 1) magicmod = 12;
        }
        else /* Icky weapon */
        {
             p_ptr->icky_wield = TRUE;
        }
	}
	/* Priest weapon penalty goes for any evil weapons also */
	/* (even if it's a hafted weapon) */
	/* It should never happen that an object has both BLESSED and BAD_WEAP */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (f3 & (TR3_BAD_WEAP)))
    {
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
					p_ptr->update |= (PU_MANA | PU_SPELLS);
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

	/* Take note when "heavy bow" changes */
	if (old_heavy_shoot != p_ptr->heavy_shoot)
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
	}

	/* Take note when "heavy weapon" changes */
	if (old_heavy_wield != p_ptr->heavy_wield)
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
	}

	/* Take note when "illegal weapon" changes */
	if (old_icky_wield != p_ptr->icky_wield)
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
	}
}




/*** Generic "deal with" functions ***/

/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;


	/* Deal with autoinscribe stuff */
	if (p_ptr->notice & PN_AUTOINSCRIBE)
	{
		p_ptr->notice &= ~(PN_AUTOINSCRIBE);
		autoinscribe_pack();
		autoinscribe_ground();
	}

	/* Deal with squelch stuff */
	if (p_ptr->notice & PN_SQUELCH)
	{
		p_ptr->notice &= ~(PN_SQUELCH);
		if (hide_squelchable) squelch_drop();
	}

	/* Combine the pack */
	if (p_ptr->notice & PN_COMBINE)
	{
		p_ptr->notice &= ~(PN_COMBINE);
		combine_pack();
	}

	/* Reorder the pack */
	if (p_ptr->notice & PN_REORDER)
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
void redraw_stuff(void)
{
	/* Redraw stuff */
	if (!p_ptr->redraw) return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;


	/* HACK - Redraw window "Display player (compact)" if necessary */
	if (p_ptr->redraw & (PR_MISC | PR_TITLE | PR_LEV | PR_EXP |
	                     PR_STATS | PR_ARMOR | PR_HP | PR_MANA |
	                     PR_GOLD | PR_HEALTH | PR_EQUIPPY | PR_CUT |
	                     PR_STUN | PR_SILVER | PR_SLIME))
	{
		p_ptr->window |= PW_PLAYER_2;
	}

	/* HACK - Redraw window "Display status" if necessary */
	if (p_ptr->redraw & (PR_HUNGER | PR_BLIND | PR_CONFUSED | PR_AFRAID |
	                     PR_POISONED | PR_STATE | PR_SPEED | PR_STUDY |
	                     PR_DEPTH))
	{
		p_ptr->window |= PW_STATUS;
	}

	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
	}

	if (p_ptr->redraw & (PR_HP))
	{
		/*
		 * hack:  redraw player, since the player's color
		 * now indicates approximate health.  Note that
		 * using this command when graphics mode is on
		 * causes the character to be a black square.
		 */
		if ((hp_changes_color) && (arg_graphics == GRAPHICS_NONE))
		{
			lite_spot(p_ptr->py, p_ptr->px);
		}
	}

	/* Redraw the sidebar */
	display_sidebar();

	/* Redraw the status line */
	display_statusline();

	return;
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

