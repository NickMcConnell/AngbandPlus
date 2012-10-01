/* File: xtra1.c */

/* Display of stats to the user from internal figures, char info shown on 
 * main screen and status displays, monster health bar, display various
 * things in sub-windows, spell management, calculation of max mana, max
 * HP, light radius, and weight limit.  Apply and display all modifiers,
 * attributes, etc. of the player, his gear, and temporary conditions to
 * the player.  Includes all racial and class attributes, effects of Bless
 * and the like, encumbrance, blows table inputs, and over-heavy weapons.
 *
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




/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	cptr p = "";

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
		p = player_title[p_ptr->pclass][(p_ptr->lev-1)/5];
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

	sprintf(out_val, "%8ld", (long)p_ptr->exp);

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
 * Prints current shape, if not normal.   -LM-
 */
static void prt_shape(void)
{
	char *shapedesc = "";

	switch (p_ptr->schange)
	{
		case SHAPE_MOUSE:
			shapedesc = "Mouse     ";
			break;
		case SHAPE_FERRET:
			shapedesc = "Ferret    ";
			break;
		case SHAPE_HOUND:
			shapedesc = "Hound     ";
			break;
		case SHAPE_GAZELLE:
			shapedesc = "Gazelle   ";
			break;
		case SHAPE_LION:
			shapedesc = "Lion      ";
			break;
		case SHAPE_ENT:
			shapedesc = "Ent       ";
			break;
		case SHAPE_BAT:
			shapedesc = "Bat       ";
			break;
		case SHAPE_WEREWOLF:
			shapedesc = "Werewolf  ";
			break;
		case SHAPE_VAMPIRE:
			shapedesc = "Vampire   ";
			break;
		case SHAPE_WYRM:
			shapedesc = "Wyrm      ";
			break;
		default:
			shapedesc = "          ";
			break;
	}

	/* Display (or write over) the shapechange with pretty colors. */
	if (mp_ptr->spell_book == TV_DRUID_BOOK) c_put_str(TERM_GREEN, shapedesc, 
		ROW_SHAPE, COL_SHAPE);
	else if (mp_ptr->spell_book == TV_NECRO_BOOK) c_put_str(TERM_VIOLET, shapedesc, 
		ROW_SHAPE, COL_SHAPE);
	else c_put_str(TERM_RED, shapedesc, ROW_SHAPE, COL_SHAPE);

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
static void prt_depth(void)
{
	char depths[32];

	if (!p_ptr->depth)
	{
		strcpy(depths, "Town");
	}
	else if (depth_in_feet)
	{
		if (use_metric) sprintf(depths, "%d m", p_ptr->depth * 15);
		else sprintf(depths, "%d ft", p_ptr->depth * 50);
	}
	else
	{
		sprintf(depths, "Lev %d", p_ptr->depth);
	}

	/* Right-Adjust the "depth", and clear old values */
	c_prt(TERM_L_BLUE, format("%7s", depths), Term->hgt - 1, Term->wid - 10);
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		c_put_str(TERM_RED, "Weak  ", Term->hgt - 1, COL_HUNGRY);
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		c_put_str(TERM_ORANGE, "Weak  ", Term->hgt - 1, COL_HUNGRY);
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		c_put_str(TERM_YELLOW, "Hungry", Term->hgt - 1, COL_HUNGRY);
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		c_put_str(TERM_L_GREEN, "      ", Term->hgt - 1, COL_HUNGRY);
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		c_put_str(TERM_L_GREEN, "Full  ", Term->hgt - 1, COL_HUNGRY);
	}

	/* Gorged */
	else
	{
		c_put_str(TERM_GREEN, "Gorged", Term->hgt - 1, COL_HUNGRY);
	}
}


/*
 * Prints Blind status
 */
static void prt_blind(void)
{
	if (p_ptr->blind)
	{
		c_put_str(TERM_ORANGE, "Blind", Term->hgt - 1, COL_BLIND);
	}
	else
	{
		put_str("     ", Term->hgt - 1, COL_BLIND);
	}
}


/*
 * Prints Confusion status
 */
static void prt_confused(void)
{
	if (p_ptr->confused)
	{
		c_put_str(TERM_ORANGE, "Confused", Term->hgt - 1, COL_CONFUSED);
	}
	else
	{
		put_str("        ", Term->hgt - 1, COL_CONFUSED);
	}
}


/*
 * Prints Fear status
 */
static void prt_afraid(void)
{
	if (p_ptr->afraid)
	{
		c_put_str(TERM_ORANGE, "Afraid", Term->hgt - 1, COL_AFRAID);
	}
	else
	{
		put_str("      ", Term->hgt - 1, COL_AFRAID);
	}
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(void)
{
	if (p_ptr->poisoned)
	{
		c_put_str(TERM_ORANGE, "Poisoned", Term->hgt - 1, COL_POISONED);
	}
	else
	{
		put_str("        ", Term->hgt - 1, COL_POISONED);
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
	c_put_str(attr, text, Term->hgt - 1, COL_STATE);
}


/*
 * Prints the speed of a character.  		-CJS-
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed;

	int attr = TERM_WHITE;
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
	c_put_str(attr, format("%-14s", buf), Term->hgt - 1, COL_SPEED);
}


static void prt_study(void)
{
	if (p_ptr->new_spells)
	{
		put_str("Study", Term->hgt - 1, 64);
	}
	else
	{
		put_str("     ", Term->hgt - 1, COL_STUDY);
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



static void prt_blank(void)
{
	int i,j;

	j = (panel_extra_rows ? 2 : 0);

	if (Term->hgt > (j + 24))
	{
		for (i=23; i < (Term->hgt - 1 - j); i++)
		{
			put_str("            ", i, 0);
		}
	}
}


/*
 * Redraw the "monster health bar"
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.   When nothing
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

		/* Black Breath */
		if (m_ptr->black_breath) attr = TERM_L_DARK;

		/* Stasis */
		if (m_ptr->stasis) attr = TERM_GREEN;


		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (handle monster stunning, confusion) */
		if (m_ptr->confused) 
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "cccccccccc");
		else if (m_ptr->stunned) 
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "ssssssssss");
		else
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
	}


	
}


/*
 * Constants for extra status messages
 */
enum {
	STATUS_BLESSED,
	STATUS_HERO,
	STATUS_SHERO,
	STATUS_OPPOSE_ACID,
	STATUS_OPPOSE_COLD,
	STATUS_OPPOSE_ELEC,
	STATUS_OPPOSE_FIRE,
	STATUS_OPPOSE_POIS,
	STATUS_PROTEVIL,
	STATUS_SHIELD,
	STATUS_FAST,
	STATUS_SLOW,
	STATUS_TIM_INFRA,
	STATUS_SEE_INVIS,
	STATUS_ESP,
	STATUS_IMAGE,
	STATUS_RECALL,
	STATUS_ELE_ATTACK,
	STATUS_MAGICDEF,
	STATUS_STEALTH,
	STATUS_MAX
};

/*
 * One of these exists for every extra status message.
 *
 * Col and row tell us where to draw.
 * Attr is the TERM_XXX color.
 * Width is the maximum field width.
 */
typedef struct {
	int col, row;
	byte attr;
	int width;
} status_type;

/*
 * Table of extra status message info.
 *
 * Order must match that of the STATUS_XXX constants.
 * Notice that col and row are initialized in init_status();
 * The attr field may be overridden in prt_status().
 */
status_type status_info[] = {
	{0, 0, TERM_L_WHITE, 7}, /* Blessed */
	{0, 0, TERM_L_WHITE, 4}, /* Hero */
	{0, 0, TERM_L_WHITE, 7}, /* Berserk */
	{0, 0, TERM_SLATE, 7}, /* ResAcid */
	{0, 0, TERM_WHITE, 7}, /* ResCold */
	{0, 0, TERM_BLUE, 7}, /* ResElec */
	{0, 0, TERM_RED, 7}, /* ResFire */
	{0, 0, TERM_GREEN, 7}, /* ResPois */
	{0, 0, TERM_L_BLUE, 8}, /* ProtEvil */
	{0, 0, TERM_L_BLUE, 6}, /* Shield */
	{0, 0, TERM_L_GREEN, 6}, /* Faster */
	{0, 0, TERM_L_UMBER, 6}, /* Slower */
	{0, 0, TERM_L_BLUE, 5}, /* Infra */
	{0, 0, TERM_L_BLUE, 8}, /* SeeInvis */
	{0, 0, TERM_L_GREEN, 3}, /* ESP */
	{0, 0, TERM_YELLOW, 6}, /* Halluc */
	{0, 0, TERM_L_BLUE, 6}, /* Recall */
	{0, 0, TERM_WHITE, 7}, /* Att1234 */
	{0, 0, TERM_WHITE, 8}, /* MagicDef */
	{0, 0, TERM_SLATE, 7}, /* Stealth */
};



/*
 * Initialize the extra status messages.
 */
static void init_status(void)
{
	int i, col, row;

	col = 0;
	row = Term->hgt - 3;

	/* Check each status message */
	for (i = 0; i < STATUS_MAX; i++)
	{
		/* Access the info */
		status_type *sp = &status_info[i];

		/* Save the column */
		sp->col = col;

		/* Save the row */
		sp->row = row;

		/* Move past this message */
		col += sp->width + 1;

		/* This is not the last message */
		if (i < STATUS_MAX - 1)
		{
			/* There isn't room for the next message on this line */
			if (col + status_info[i + 1].width >= 80)
			{
				/* Wrap */
				col = 0;
				row++;
			}
		}
	}
}


/*
 * Display all the extra status messages.
 */
static void prt_status(void)
{
	char *s = "                    ";

	int i;


	/* XXX Hack -- Always print messages (for debugging) */
	bool force = FALSE;

	/* XXX Check for room */
	if (!panel_extra_rows) return;

	/* Initialize */
	init_status();

	/* Check each status message */
	for (i = 0; i < STATUS_MAX; i++)
	{
		/* Access the info */
		status_type *sp = &status_info[i];

		/* Get the default attribute */
		byte attr = sp->attr;

		/* Assume empty display */
		char *t = s;

		/* Examine */
		switch (i)
		{
			case STATUS_BLESSED:
				if (force || p_ptr->blessed) t = "Blessed";
				break;

			case STATUS_HERO:
				if (force || p_ptr->hero) t = "Hero";
				break;

			case STATUS_SHERO:
				if (force || p_ptr->shero) t = "Berserk";
				break;

			case STATUS_OPPOSE_ACID:
				if (force || p_ptr->oppose_acid) t = "ResAcid";
				break;

			case STATUS_OPPOSE_COLD:
				if (force || p_ptr->oppose_cold) t = "ResCold";
				break;

			case STATUS_OPPOSE_ELEC:
				if (force || p_ptr->oppose_elec) t = "ResElec";
				break;

			case STATUS_OPPOSE_FIRE:
				if (force || p_ptr->oppose_fire) t = "ResFire";
				break;

			case STATUS_OPPOSE_POIS:
				if (force || p_ptr->oppose_pois) t = "ResPois";
				break;

			case STATUS_PROTEVIL:
				if (force || p_ptr->protevil) t = "ProtEvil";
				break;

			case STATUS_SHIELD:
				if (force || p_ptr->shield) t = "Shield";
				break;

			case STATUS_FAST:
				if (force || p_ptr->fast) t = "Faster";
				break;

			case STATUS_SLOW:
				if (force || p_ptr->slow) t = "Slower";
				break;

			case STATUS_TIM_INFRA:
				if (force || p_ptr->tim_infra) t = "Infra";
				break;

			case STATUS_SEE_INVIS:
				if (force || p_ptr->tim_invis) t = "SeeInvis";
				break;

			case STATUS_ESP:
				if (force || p_ptr->tim_esp) t = "ESP";
				break;

			case STATUS_RECALL:
				if (force || p_ptr->word_recall) t = "Recall";
				break;

			case STATUS_IMAGE:
				if (force || p_ptr->image) t = "Halluc";
				break;

			case STATUS_ELE_ATTACK:
				if (force || p_ptr->ele_attack)
				{
					if (force || p_ptr->special_attack & ATTACK_ACID)
					{
						attr = TERM_L_DARK;
						t = "AttAcid";
					}
					else if (p_ptr->special_attack & ATTACK_ELEC)
					{
						attr = TERM_BLUE;
						t = "AttElec";
					}
					else if (p_ptr->special_attack & ATTACK_FIRE)
					{
						attr = TERM_RED;
						t = "AttFire";
					}
					else if (p_ptr->special_attack & ATTACK_COLD)
					{
						attr = TERM_WHITE;
						t = "AttCold";
					}
					else if (p_ptr->special_attack & ATTACK_POIS)
					{
						attr = TERM_GREEN;
						t = "AttPois";
					}
				}
				break;

			case STATUS_MAGICDEF:
				if (force || p_ptr->magicdef) t = "MagicDef";
				break;

			case STATUS_STEALTH:
				if (force || p_ptr->superstealth) t = "Stealth";
				break;
		}

		/* XXX Hack -- Always show */
		if (force) attr = TERM_L_DARK;

		/* Display */
		Term_putstr(sp->col, sp->row, sp->width, attr, t);
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
	prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);

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

	/* Gold */
	prt_gold();

	/* Shape, if not normal. */
	prt_shape();

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

	/* Blank spaces in bigscreen mode */
	prt_blank();

	/* State */
	prt_state();

	/* Speed */
	prt_speed();

	/* Study spells */
	prt_study();

	/* Status */
	prt_status();
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
	for (j = 0; j < 8; j++)
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
	for (j = 0; j < 8; j++)
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
	for (j = 0; j < 8; j++)
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
 * Adjust for width and split messages.   XXX XXX XXX
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
			Term_putstr(0, (h - 1) - i, -1, color, message_str(i));

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

	int cy, cx;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_OVERHEAD))) continue;

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
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypassed until the character is created.
 */
static void calc_spells(void)
{
	int i, j, k, levels;
	int num_allowed, num_known;

	magic_type *s_ptr;

	cptr p = "";


	/* Hack -- must be literate */
	if (!mp_ptr->spell_book) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Determine magic description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) p = "spell";
	if (mp_ptr->spell_book == TV_PRAYER_BOOK) p = "prayer";
	if (mp_ptr->spell_book == TV_DRUID_BOOK) p = "druidic lore";
	if (mp_ptr->spell_book == TV_NECRO_BOOK) p = "ritual";

	/* Determine the number of spells allowed */
	levels = p_ptr->lev - mp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;


	/* Extract total allowed spells */
	num_allowed = (adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] *
		       levels / 2);

	/* Boundary control. */
	if (num_allowed > mp_ptr->spell_number) num_allowed = mp_ptr->spell_number;


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < mp_ptr->spell_number; j++)
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
		if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break;

		/* Access the spell */
		j = p_ptr->spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

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
				   spell_names[s_ptr->index]);

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
		if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2) break;

		/* Get the (i+1)th spell learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Get the spell */
		s_ptr = &mp_ptr->info[j];

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
				   spell_names[s_ptr->index]);

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
		if (!p_ptr->spell_forgotten1 && !p_ptr->spell_forgotten2) break;

		/* Get the next spell we learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) break;

		/* Access the spell */
		s_ptr = &mp_ptr->info[j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

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
				   p, spell_names[s_ptr->index]);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < mp_ptr->spell_number; j++)
	{
		/* Access the spell */
		s_ptr = &mp_ptr->info[j];

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
	if (p_ptr->old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
			msg_format("You can learn %d more %s%s.", p_ptr->new_spells, p, 
				((p_ptr->new_spells != 1) && 
				(mp_ptr->spell_book != TV_DRUID_BOOK)) ? "s" : "");
		}

		/* Save the new_spells value */
		p_ptr->old_spells = p_ptr->new_spells;

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor, and
 * by a shapeshift.
 *
 * This function induces status messages.
 *
 * New treatment of encumberance by LM
 */
static void calc_mana(void)
{
	int msp, levels, cur_wgt, max_wgt;

	object_type *o_ptr;


	/* Hack -- Must possess some magical realm. */
	if (!mp_ptr->spell_realm) return;

	/* Extract "effective" player level */
	levels = (p_ptr->lev - mp_ptr->spell_first) + 1;

	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana, using standard rounding. */
	msp = (adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels + 5) / 10;

	/* The weak spellcasters get half as much mana (rounded up) in Oangband. */
	switch (p_ptr->pclass)
	{
		case CLASS_ROGUE:
		case CLASS_RANGER:
		case CLASS_PALADIN:
		case CLASS_ASSASSIN:
		{
			msp = (msp + 1) / 2;
			break;
		}
		default:
		{
			break;
		}
	}

	/* Hack -- usually add one mana */
	if (msp) msp++;


	/* Only mage and Necromancer-type spellcasters are affected by gloves. */
	if ((mp_ptr->spell_book == TV_MAGIC_BOOK) || 
		(mp_ptr->spell_book == TV_NECRO_BOOK))
	{
		u32b f1, f2, f3;

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Normal gloves hurt mage or necro-type spells.  Now, only 
		 * Free Action or magic mastery stops this effect.
		 */
		if (o_ptr->k_idx &&
		    !(f3 & (TR3_FREE_ACT)) && !(f1 & (TR1_MAGIC_MASTERY)))
		{
			/* Encumbered */
			p_ptr->cumber_glove = TRUE;

			/* Reduce mana */
			msp = 3 * msp / 4;
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

	/* Heavy armor penalizes mana by a percentage. */
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
			case CLASS_NECRO:
			case CLASS_DRUID:
			{
				msp -= msp * (cur_wgt - max_wgt) / 600;
				break;
			}

			/* Mana halved if armour is 40 pounds over weight limit. */
			case CLASS_PRIEST:
			{
				msp -= msp * (cur_wgt - max_wgt) / 800;
				break;
			}

			/* Mana halved if armour is 50 pounds over weight limit. */
			case CLASS_ROGUE:
			case CLASS_RANGER:
			case CLASS_ASSASSIN:
			{
				msp -= msp * (cur_wgt - max_wgt) / 1000;
				break;
			}

			/* Mana halved if armour is 60 pounds over weight limit. */
			case CLASS_PALADIN:
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

	/* Any non-humaniod shape penalizes mana. */
	if (p_ptr->schange)
	{
		/* Chop mana to 2/3. */
		msp = 2 * msp / 3;
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
 */
static void calc_hitpoints(void)
{
	int bonus, mhp;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128);

	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 2);

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
 * Extract and set the current "lite radius"
 */
static void calc_torch(void)
{
	object_type *o_ptr = &inventory[INVEN_LITE];

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Player is glowing */
	if (p_ptr->lite) p_ptr->cur_lite += 1;

	/* Examine actual lites */
	if (o_ptr->tval == TV_LITE)
	{
		/* Torches (with fuel) provide some light */
		if ((o_ptr->sval == SV_LITE_TORCH) && (o_ptr->pval > 0))
		{
			p_ptr->cur_lite += 1;
		}

		/* Lanterns (with fuel) provide more light */
		if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->pval > 0))
		{
			p_ptr->cur_lite += 2;
		}

		/* Artifact Lites (but not the Stone of Lore) provide permanent, bright, light */
		if ((artifact_p(o_ptr)) && (o_ptr->k_idx != 477)) p_ptr->cur_lite += 3;
	}

	/* Priests and Paladins get a bonus to light radius at level 35 and
	 * 45, respectively.
	 */
	if ((p_ptr->pclass == CLASS_PRIEST) && (p_ptr->lev > 34))
		p_ptr->cur_lite += 1;
	if ((p_ptr->pclass == CLASS_PALADIN) && (p_ptr->lev > 44))
		p_ptr->cur_lite += 1;

	/* Reduce lite when running if requested */
	if (p_ptr->running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
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
sint add_special_melee_skill (byte pclass, s16b weight, object_type *o_ptr)
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

		/* Mage.  Can use 6 lb weapons without penalty at level 1, and 16 lb weapons without penalty at 50th level. */
		case CLASS_MAGE:
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

			/* Priest penalty for non-blessed edged weapons. */
			if (((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)) && ((!p_ptr->bless_blade)))
			{
				add_skill -= 10 + p_ptr->lev / 2;

				/* Icky weapon */
				p_ptr->icky_wield = TRUE;
			}
			break;
		}

		/* Rogue.  Can use 10 lb weapons without penalty at level 1, and 20 lb weapons without penalty at 50th level. Can get a bonus for using light weapons.  */
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

		/* Ranger.  Can use 12 lb weapons without penalty at level 1, and 25 lb weapons without penalty at 50th level. */
		case CLASS_RANGER:
		{
			add_skill = 25 + (1 * p_ptr->lev / 2) - (weight / 5);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -20) add_skill = -20;
			break;
		}

		/* Paladin.  Can use 15 lb weapons without penalty at level 1, and 45 lb weapons without penalty at 50th level. */
		case CLASS_PALADIN:
		{
			add_skill = 25 + p_ptr->lev - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;

			/* Paladin penalty for non-blessed edged weapons. */
			if (((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)) 
				&& ((!p_ptr->bless_blade)))
			{
				add_skill -= 10 + p_ptr->lev / 2;

				/* Icky weapon */
				p_ptr->icky_wield = TRUE;
			}
			break;
		}

		/* Druid.  Can use 5 lb weapons without penalty at level 1, and
		 * slightly over 12 lb weapons without penalty at 50th level. Much
		 * prefers to use hands and feet.
		 */
		case CLASS_DRUID:
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

		/* Necromancer.   Can use 6 lb weapons without penalty at level 1, and 16 lb weapons without penalty at 50th level. */
		case CLASS_NECRO:
		{
			add_skill = 20 + (2 * p_ptr->lev / 3) - (weight / 3);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -30) add_skill = -30;
			break;
		}

		/* Assassin.  Can use 10 lb weapons without penalty at level 1, and 20 
		 * lb weapons without penalty at 50th level. Can be quite dangerous with 
		 * light weapons.
		 */
		case CLASS_ASSASSIN:
		{
			if (!o_ptr->k_idx) add_skill = 0;

			else
			{
				add_skill = 33 + (2 * p_ptr->lev / 3) - (weight / 3);
				if (add_skill > 0) add_skill = (2 * add_skill / 3);
				if (add_skill > 20) add_skill = 20;
				if (add_skill < -25) add_skill = -25;
			}
			break;
		}
	}

	/* Now, special racial abilities and limitations are 
	 * considered.  Most modifiers are relatively small, to 
	 * keep options open to the player. */
	if (o_ptr->tval == TV_SWORD)
	{
		if ((rp_ptr->flags_special) & PS_SWORD_SKILL) add_skill += 3 + p_ptr->lev / 7;
		else if ((rp_ptr->flags_special) & PS_SWORD_UNSKILL) add_skill -= 3 + p_ptr->lev / 7;
	}

	else if (o_ptr->tval == TV_POLEARM)
	{
		if ((rp_ptr->flags_special) & PS_POLEARM_SKILL) add_skill += 3 + p_ptr->lev / 7;
		else if ((rp_ptr->flags_special) & PS_POLEARM_UNSKILL) add_skill -= 3 + p_ptr->lev / 7;
	}

	else if (o_ptr->tval == TV_HAFTED)
	{
		if ((rp_ptr->flags_special) & PS_HAFTED_SKILL) add_skill += 3 + p_ptr->lev / 7;
		else if ((rp_ptr->flags_special) & PS_HAFTED_UNSKILL) add_skill -= 3 + p_ptr->lev / 7;
	}

	return (add_skill);
}

/* Calculate all class and race-based bonuses and 
 * penalties to missile Skill 
 */

sint add_special_missile_skill (byte pclass, s16b weight, object_type *o_ptr)
{
	int add_skill = 0;

	switch (pclass)
	{

		/* Rogues are good with slings. */
		case CLASS_ROGUE:
		{
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				add_skill = 3 + p_ptr->lev / 4;
			}
			break;
		}

		/* Rangers have a high missile skill, but they are 
		 * not supposed to be great with xbows and slings. */
		case CLASS_RANGER:
		{
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

		/* Druids get a small bonus with slings. */
		case CLASS_DRUID:
		{
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				add_skill = p_ptr->lev / 7;
			}
		}
	}

	/* Now, special racial abilities and limitations 
	 * are considered.  The choice of race can be of 
	 * some significance.
	 */
	
	if (p_ptr->ammo_tval == TV_BOLT)
	{
		if ((rp_ptr->flags_special) & PS_XBOW_SKILL) add_skill += 3 + p_ptr->lev / 7;
		else if ((rp_ptr->flags_special) & PS_XBOW_UNSKILL) add_skill -= 3 + p_ptr->lev / 7;
	}
	else if (p_ptr->ammo_tval == TV_ARROW)
	{
		if ((rp_ptr->flags_special) & PS_BOW_SKILL) add_skill += 3 + p_ptr->lev / 7;
		else if ((rp_ptr->flags_special) & PS_BOW_UNSKILL) add_skill -= 3 + p_ptr->lev / 7;
	}
	else if (p_ptr->ammo_tval == TV_SHOT)
	{
		if ((rp_ptr->flags_special) & PS_SLING_SKILL) add_skill += 3 + p_ptr->lev / 7;
		else if ((rp_ptr->flags_special) & PS_SLING_UNSKILL) add_skill -= 3 + p_ptr->lev / 7;
	}
	return (add_skill);
}

/* Applies vital statistic changes from a shapeshift 
 * to the player.
*/
static void shape_change_stat(void)
{
	switch (p_ptr->schange)
	{
		case SHAPE_NORMAL:
			break;
		case SHAPE_MOUSE:
		{
			p_ptr->stat_add[A_STR] -= 2;
			p_ptr->stat_add[A_INT] -= 7;
			p_ptr->stat_add[A_CON] -= 1;
			p_ptr->stat_add[A_CHR] -= 5;
			break;
		}
		case SHAPE_FERRET:
		{
			p_ptr->stat_add[A_DEX] += 4;
			p_ptr->stat_add[A_CHR] -= 2;
			break;
		}
		case SHAPE_HOUND:
		{
			p_ptr->stat_add[A_CON] += 2;
			p_ptr->stat_add[A_INT] -= 2;
			p_ptr->stat_add[A_CHR] -= 2;
			break;
		}
		case SHAPE_GAZELLE:
		{
			p_ptr->stat_add[A_STR] -= 2;
			p_ptr->stat_add[A_DEX] += 2;
			p_ptr->stat_add[A_CON] -= 1;
			p_ptr->stat_add[A_WIS] -= 2;
			break;
		}
		case SHAPE_LION:
		{
			p_ptr->stat_add[A_STR] += 3;
			p_ptr->stat_add[A_CHR] -= 4;
			p_ptr->stat_add[A_WIS] -= 2;
			p_ptr->stat_add[A_INT] -= 2;
			break;
		}
		case SHAPE_ENT:
		{
			p_ptr->stat_add[A_STR] += 4;
			p_ptr->stat_add[A_WIS] += 1;
			p_ptr->stat_add[A_DEX] -= 5;
			p_ptr->stat_add[A_CON] += 4;
			p_ptr->stat_add[A_CHR] -= 1;
			break;
		}
		case SHAPE_BAT:
		{
			p_ptr->stat_add[A_STR] -= 1;
			p_ptr->stat_add[A_WIS] -= 2;
			p_ptr->stat_add[A_INT] -= 2;
			p_ptr->stat_add[A_CHR] -= 2;
			break;
		}
		case SHAPE_WEREWOLF:
		{
			p_ptr->stat_add[A_STR] += 2;
			p_ptr->stat_add[A_CHR] -= 5;
			p_ptr->stat_add[A_INT] -= 2;
			break;
		}
		case SHAPE_VAMPIRE:
		{
			p_ptr->stat_add[A_STR] += 2;
			p_ptr->stat_add[A_CON] += 1;
			p_ptr->stat_add[A_INT] += 2;
			p_ptr->stat_add[A_CHR] -= 3;
			break;
		}
		case SHAPE_WYRM:
		{
			p_ptr->stat_add[A_STR] += 2;
			p_ptr->stat_add[A_CON] += 1;
			p_ptr->stat_add[A_WIS] += 1;
			p_ptr->stat_add[A_INT] += 1;
			p_ptr->stat_add[A_DEX] -= 1;
			p_ptr->stat_add[A_CHR] -= 1;
			break;
		}
	}
}

/* A Sangband-derived function to apply all non-stat changes from a shapeshift 
 * to the player.  Any alterations also need to be added to the character screen 
 * (files.c, function "player_flags"), and all timed states (opposition to the 
 * elements for example) must be hacked into the timing of player states in 
 * dungeon.c.  -LM-
 */
static void shape_change_main(void)
{
	object_type *o_ptr;
	switch (p_ptr->schange)
	{
		case SHAPE_NORMAL:
			break;
		case SHAPE_MOUSE:
		{
			p_ptr->skill_stl = (30 + p_ptr->skill_stl) / 2;
			p_ptr->see_infra += 2;
			p_ptr->aggravate = FALSE;
			p_ptr->to_a -= 5;
			p_ptr->dis_to_a -= 5;
			p_ptr->to_h -= 15;
			p_ptr->dis_to_h -= 15;
			p_ptr->to_d -= 25;
			p_ptr->dis_to_d -= 25;
			p_ptr->skill_dev /= 4;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;
		}
		case SHAPE_FERRET:
		{
			p_ptr->see_infra += 2;
			p_ptr->regenerate = TRUE;
			p_ptr->to_d -= 10;
			p_ptr->dis_to_d -= 10;
			p_ptr->pspeed += 2;
			p_ptr->skill_fos += 10;
			p_ptr->skill_srh += 10;
			p_ptr->skill_dev /= 2;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;
		}
		case SHAPE_HOUND:
		{
			p_ptr->see_infra += 3;
			p_ptr->telepathy = TRUE;
			p_ptr->skill_dev /= 2;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;
		}
		case SHAPE_GAZELLE:
		{
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
			p_ptr->to_d -= 5;
			p_ptr->dis_to_d -= 5;
			p_ptr->pspeed += 6;
			p_ptr->skill_dev /= 2;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;
		}
		case SHAPE_LION:
		{
			p_ptr->resist_fear = TRUE;
			p_ptr->regenerate = TRUE;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
			p_ptr->to_h += 10;
			p_ptr->dis_to_h += 10;
			p_ptr->to_d += 15;
			p_ptr->dis_to_d += 15;
			p_ptr->pspeed += 1;
			p_ptr->skill_dev /= 2;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;
		}
		case SHAPE_ENT:
		{
			p_ptr->resist_cold = TRUE;
			p_ptr->immune_fire = FALSE;
			p_ptr->resist_fire = FALSE;
			p_ptr->resist_pois = TRUE;
			p_ptr->resist_fear = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->free_act = TRUE;
			p_ptr->ffall = FALSE;
			p_ptr->to_d += 10;
			p_ptr->dis_to_d += 10;
			p_ptr->skill_dig += 8;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;
		}
		case SHAPE_BAT:
		{
			p_ptr->see_infra += 6;
			p_ptr->resist_blind = TRUE;
			p_ptr->ffall = TRUE;
			p_ptr->to_h -= 5;
			p_ptr->dis_to_h -= 5;
			p_ptr->to_d -= 15;
			p_ptr->dis_to_d -= 15;
			p_ptr->pspeed += 5;
			p_ptr->skill_dev /= 4;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;
		}
		case SHAPE_WEREWOLF:
		{
			p_ptr->see_infra += 3;
			p_ptr->regenerate = TRUE;
			p_ptr->aggravate = TRUE;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
			p_ptr->to_h += 20;
			p_ptr->dis_to_h += 20;
			p_ptr->to_d += 20;
			p_ptr->dis_to_d += 20;
			p_ptr->skill_dev /= 2;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;
			break;	
		}
		case SHAPE_VAMPIRE:
		{
			p_ptr->see_infra += 3;
			if (p_ptr->cur_lite >= 3) p_ptr->cur_lite = 2;
			p_ptr->see_inv = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_lite = FALSE;
			p_ptr->oppose_fire = FALSE;
			p_ptr->regenerate = TRUE;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
			p_ptr->to_h += 5;
			p_ptr->dis_to_h += 5;
			p_ptr->to_d += 5;
			p_ptr->dis_to_d += 5;
			p_ptr->skill_stl += 1;
			p_ptr->skill_dev += 6;
			p_ptr->skill_thb -= 10;
			p_ptr->skill_tht -= 10;
			break;
		}
		case SHAPE_WYRM:
		{
			o_ptr = &inventory[INVEN_BODY];
			p_ptr->to_a += 10;
			p_ptr->dis_to_a += 10;
			p_ptr->to_d += 5;
			p_ptr->dis_to_d += 5;
			p_ptr->skill_stl -= 3;
			p_ptr->skill_dev += 4;
			p_ptr->skill_thb -= 30;
			p_ptr->skill_tht -= 30;

			/* 
			 * Apply an extra bonus power depending on the type
			 * of DSM when in WYRM form 
			 */
			if (o_ptr->tval == TV_DRAG_ARMOR)
			{
				/* Elemental DSM -> immunity */
				if (o_ptr->sval == SV_DRAGON_BLACK) 
					p_ptr->immune_acid = TRUE;
				else if (o_ptr->sval == SV_DRAGON_BLUE) 
					p_ptr->immune_elec = TRUE;
				else if (o_ptr->sval == SV_DRAGON_WHITE) 
					p_ptr->immune_cold = TRUE;
				else if (o_ptr->sval == SV_DRAGON_RED) 
					p_ptr->immune_fire = TRUE;

				/* Green DSM -> regen */
				else if (o_ptr->sval == SV_DRAGON_GREEN) 
					p_ptr->regenerate = TRUE;

				/* Shining DSM -> SI */
				else if (o_ptr->sval == SV_DRAGON_SHINING) 
					p_ptr->see_inv = TRUE;

				/* Law/Chaos DSM -> hold life */
				else if (o_ptr->sval == SV_DRAGON_LAW) 
					p_ptr->hold_life = TRUE;
				else if (o_ptr->sval == SV_DRAGON_CHAOS) 
					p_ptr->hold_life = TRUE;

				/* Bronze/Gold DSM -> FA */
				else if (o_ptr->sval == SV_DRAGON_BRONZE) 
					p_ptr->free_act = TRUE;
				else if (o_ptr->sval == SV_DRAGON_GOLD) 
					p_ptr->free_act = TRUE;

				/* Multihued, Balance and Power don't need any help */
			}
			break;
		}
	}
}



/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * This is the "kitchen sink" function!	 I may get around to 
 * segmenting it, simply to make it more readable...  -LM-
 *
 * I have added class-specific modifiers to Skill and enforced a max
 * of one blow/rnd for weapons too heavy to wield effectively. -LM-
 *
 * This function calls itself if the player's STR stat changes. -LM-
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

	int temp_armour;

	int old_dis_ac;
	int old_dis_to_a;

	int extra_shots;
	int extra_might;

	int old_stat_top[6];
	int old_stat_use[6];
	int old_stat_ind[6];

	int float_simulator;

	object_type *o_ptr;

	u32b f1, f2, f3;


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
	for (i = 0; i < 6; i++)
	{
		old_stat_top[i] = p_ptr->stat_top[i];
		old_stat_use[i] = p_ptr->stat_use[i];
		old_stat_ind[i] = p_ptr->stat_ind[i];
	}


	/* Hack - If the player's usage of his shield changes, we must
	 * recalculate various things.
	 */
	calc_again:


	/*** Reset ***/

	/* Reset player speed */
	p_ptr->pspeed = 110;

	/* Reset "blow" info */
	p_ptr->num_blow = 1;

	/* Reset "fire" info */
	p_ptr->num_fire = 0;
	p_ptr->ammo_mult = 0;
	p_ptr->ammo_tval = 0;
	extra_shots = 0;
	extra_might = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	/* p_ptr->black_breath = FALSE; */
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

	/* Base skill -- combat (melee) */
	p_ptr->skill_thn = rp_ptr->r_thn + cp_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- digging */
	p_ptr->skill_dig = 0;

	/*** Analyze player ***/

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3, FALSE);

	/* Good flags */
	if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
	if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
	if (f3 & (TR3_LITE)) p_ptr->lite = TRUE;
	if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
	if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
	if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
	if (f3 & (TR3_FREE_ACT)) p_ptr->free_act = TRUE;
	if (f3 & (TR3_HOLD_LIFE)) p_ptr->hold_life = TRUE;

	/* Weird flags */
	if (f3 & (TR3_BLESSED)) p_ptr->bless_blade = TRUE;

	/* Bad flags */
	if (f3 & (TR3_IMPACT)) p_ptr->impact = TRUE;
	if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
	if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
	if (f3 & (TR3_DRAIN_EXP)) p_ptr->black_breath = TRUE;

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
	if (f2 & (TR2_RES_LITE)) p_ptr->resist_lite = TRUE;
	if (f2 & (TR2_RES_DARK)) p_ptr->resist_dark = TRUE;
	if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
	if (f2 & (TR2_RES_CONFU)) p_ptr->resist_confu = TRUE;
	if (f2 & (TR2_RES_SOUND)) p_ptr->resist_sound = TRUE;
	if (f2 & (TR2_RES_SHARD)) p_ptr->resist_shard = TRUE;
	if (f2 & (TR2_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
	if (f2 & (TR2_RES_NETHR)) p_ptr->resist_nethr = TRUE;
	if (f2 & (TR2_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
	if (f2 & (TR2_RES_DISEN)) p_ptr->resist_disen = TRUE;

	/* Sustain flags */
	if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
	if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
	if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
	if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
	if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
	if (f2 & (TR2_SUST_CHR)) p_ptr->sustain_chr = TRUE;

	/* Special Stuff, not found in p_info.txt */

	/* Ent */
	if ((rp_ptr->flags_special) & PS_WOODEN) 
	{
		/* Ents dig like maniacs, but only with their hands. */
		if (!inventory[INVEN_WIELD].k_idx) 
			p_ptr->skill_dig += p_ptr->lev * 10;

		/* Ents get tougher and stronger as they age, but lose dexterity. */
		if (p_ptr->lev > 25) p_ptr->stat_add[A_STR]++;
		if (p_ptr->lev > 40) p_ptr->stat_add[A_STR]++;
		if (p_ptr->lev > 45) p_ptr->stat_add[A_STR]++;

		if (p_ptr->lev > 25) p_ptr->stat_add[A_DEX]--;
		if (p_ptr->lev > 40) p_ptr->stat_add[A_DEX]--;
		if (p_ptr->lev > 45) p_ptr->stat_add[A_DEX]--;

		if (p_ptr->lev > 25) p_ptr->stat_add[A_CON]++;
		if (p_ptr->lev > 40) p_ptr->stat_add[A_CON]++;
		if (p_ptr->lev > 45) p_ptr->stat_add[A_CON]++;
	}

	/*** Analyze equipment ***/

	/* Scan the equipment */
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

		if (f1 & (TR1_MAGIC_MASTERY))p_ptr->skill_dev += 10 * o_ptr->pval;

		/* Affect shots.  Altered in Oangband. */
		if (f1 & (TR1_SHOTS)) extra_shots += 1;

		/* Affect might.  Altered in Oangband. */
		if (f1 & (TR1_MIGHT1)) extra_might += 1;
		if (f1 & (TR1_MIGHT2)) extra_might += 2;

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
		if (f3 & (TR3_LITE)) p_ptr->lite = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
		if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
		if (f3 & (TR3_FREE_ACT)) p_ptr->free_act = TRUE;
		if (f3 & (TR3_HOLD_LIFE)) p_ptr->hold_life = TRUE;

		/* Weird flags */
		if (f3 & (TR3_BLESSED)) p_ptr->bless_blade = TRUE;

		/* Bad flags */
		if (f3 & (TR3_IMPACT)) p_ptr->impact = TRUE;
		if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;

		/* The new code implementing Tolkien's concept of "Black Breath"
		 * takes advantage of the existing drain_exp character flag, renamed
		 * "black_breath". This flag can also be set by a unlucky blow from
		 * an undead.  -LM- 
		 */
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->black_breath = TRUE;

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
		if (f2 & (TR2_RES_LITE)) p_ptr->resist_lite = TRUE;
		if (f2 & (TR2_RES_DARK)) p_ptr->resist_dark = TRUE;
		if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_CONFU)) p_ptr->resist_confu = TRUE;
		if (f2 & (TR2_RES_SOUND)) p_ptr->resist_sound = TRUE;
		if (f2 & (TR2_RES_SHARD)) p_ptr->resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
		if (f2 & (TR2_RES_NETHR)) p_ptr->resist_nethr = TRUE;
		if (f2 & (TR2_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN)) p_ptr->resist_disen = TRUE;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
		if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
		if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
		if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
		if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
		if (f2 & (TR2_SUST_CHR)) p_ptr->sustain_chr = TRUE;


		/* Modify the base armor class.   Shields worn on back are penalized. */
		if ((p_ptr->shield_on_back) && (i == INVEN_ARM)) 
			temp_armour = o_ptr->ac / 3;
		else temp_armour = o_ptr->ac;

		p_ptr->ac += temp_armour;

		/* The base armor class is always known */
		p_ptr->dis_ac += temp_armour;

		/* Apply the bonuses to armor class.  Shields worn on back are 
		 * penalized.
		 */
		if ((p_ptr->shield_on_back) && (i == INVEN_ARM)) 
			temp_armour = o_ptr->to_a / 2;
		else temp_armour = o_ptr->to_a;

		p_ptr->to_a += temp_armour;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += temp_armour;


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

	/* Hack -- clear a few flags for certain races. */

	/* The Shadow Fairy's saving grace */
	if (((rp_ptr->flags_special) & PS_SHADOW) && (p_ptr->aggravate)) 
	{
		p_ptr->skill_stl -= 3;
		p_ptr->aggravate = FALSE;
	}

	/* Nothing, but nothing, can make an Ent lightfooted. */
	if ((rp_ptr->flags_special) & PS_WOODEN) p_ptr->ffall = FALSE;


	/*** Analyze shapechanges - statistics only ***/
	shape_change_stat();


	/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < 6; i++)
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


	/*** Temporary flags ***/

	/* Apply temporary "stun".  */
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

	/* Heightened magical defenses.   Halves the difference between saving 
	 * throw and 100.  */
	if (p_ptr->magicdef)
	{
		p_ptr->to_a += 25;
		p_ptr->dis_to_a += 25;

		if (p_ptr->skill_sav <= 100) p_ptr->skill_sav += (100 - p_ptr->skill_sav) / 2;

		p_ptr->resist_blind = TRUE;
		p_ptr->resist_confu = TRUE;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temporary shield.  Added an exception for Necromancers to keep
	 * them in line.
	 */
	if ((p_ptr->shield) && (p_ptr->pclass == CLASS_NECRO))
	{
		p_ptr->to_a += 35;
		p_ptr->dis_to_a += 35;
	}
	else if (p_ptr->shield)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary "Hero".  Now also increases Deadliness. */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
		p_ptr->to_d += 5;
		p_ptr->dis_to_d += 5;
	}

	/* Temporary "Berserk".   Now also increases Deadliness. */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 5;
		p_ptr->dis_to_h += 5;
		p_ptr->to_d += 15;
		p_ptr->dis_to_d += 15;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;

		/* but berserkers make *lousy* archers. */
		p_ptr->skill_thb -= 20;
		p_ptr->skill_tht -= 20;
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

	/* Temporary infravision boost.   More useful now. */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra = p_ptr->see_infra + 3;
	}


	/*** Special flags ***/

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

	/* Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
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

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);


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

	/* Affect Skill -- disarming (Level, by Class and Race) */
	p_ptr->skill_dis += (cp_ptr->cx_dis * p_ptr->lev / 50);
	p_ptr->skill_dis += (rp_ptr->rx_dis * p_ptr->lev / 50);

	/* Affect Skill -- magic devices (Level, by Class and Race) */
	p_ptr->skill_dev += (cp_ptr->cx_dev * p_ptr->lev / 50);
	p_ptr->skill_dev += (rp_ptr->rx_dev * p_ptr->lev / 50);

	/* Affect Skill -- saving throw (Level, by Class and Race) */
	p_ptr->skill_sav += (cp_ptr->cx_sav * p_ptr->lev / 50);
	p_ptr->skill_sav += (rp_ptr->rx_sav * p_ptr->lev / 50);

	/* Affect Skill -- stealth (Level, by Class and Race) */
	p_ptr->skill_stl += (cp_ptr->cx_stl * p_ptr->lev / 50);
	p_ptr->skill_stl += (rp_ptr->rx_stl * p_ptr->lev / 50);

	/* Affect Skill -- search ability (Level, by Class and Race) */
	p_ptr->skill_srh += (cp_ptr->cx_srh * p_ptr->lev / 50);
	p_ptr->skill_srh += (rp_ptr->rx_srh * p_ptr->lev / 50);

	/* Affect Skill -- search frequency (Level, by Class and Race) */
	p_ptr->skill_fos += (cp_ptr->cx_fos * p_ptr->lev / 50);
	p_ptr->skill_fos += (rp_ptr->rx_fos * p_ptr->lev / 50);

	/* Affect Skill -- combat (melee) (Level, by Class and Race) */
	p_ptr->skill_thn += (cp_ptr->cx_thn * p_ptr->lev / 50);
	p_ptr->skill_thn += (rp_ptr->rx_thn * p_ptr->lev / 50);

	/* Affect Skill -- combat (shooting) (Level, by Class and Race) */
	p_ptr->skill_thb += (cp_ptr->cx_thb * p_ptr->lev / 50);
	p_ptr->skill_thb += (rp_ptr->rx_thb * p_ptr->lev / 50);

	/* Affect Skill -- combat (throwing) (Level, by Class and Race) */
	p_ptr->skill_tht += (cp_ptr->cx_thb * p_ptr->lev / 50);
	p_ptr->skill_tht += (rp_ptr->rx_thb * p_ptr->lev / 50);

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Apply Skill -- Extract the base wakeup chance (x100) from stealth.  
	 * Various activities can now directly affect the possibility that 
	 * monsters will be disturbed the next time they are processed.   So be 
	 * careful when bashing or fighting!  Also, since noise is created by 
	 * performing actions, not passing time, faster chars no longer have an 
	 * effective bonus to stealth.  To compensate, stealth is now 15% more 
	 * effective intrinsically.  See global variable "add_wakeup_chance".  -LM-
	 */
	float_simulator = 100;
	if (p_ptr->skill_stl % 3 == 1) float_simulator = 126;
	if (p_ptr->skill_stl % 3 == 2) float_simulator = 159;

	p_ptr->base_wakeup_chance = 85000L * 
		extract_energy[p_ptr->pspeed] / 
			( (1L << (p_ptr->skill_stl / 3)) * 
				float_simulator);


	/*** Analyze shapechanges - everything but statistics ***/
	shape_change_main();


	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];

	/*** Analyze current bow ***/

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

	/* Analyze launcher */
	if (o_ptr->k_idx)
	{
		/* Get to shoot */
		p_ptr->num_fire = 1;
		p_ptr->num_fire = 1;

		/* Launcher multiplier is now simply their damage dice. */
		p_ptr->ammo_mult = o_ptr->dd;


		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
			/* Sling and ammo */
			case SV_SLING:
			{
				p_ptr->ammo_tval = TV_SHOT;
				break;
			}

			/* Short Bow and Arrow */
			case SV_SHORT_BOW:
			{
				p_ptr->ammo_tval = TV_ARROW;
				break;
			}

			/* Long Bow and Arrow */
			case SV_LONG_BOW:
			{
				p_ptr->ammo_tval = TV_ARROW;
				break;
			}

			/* Light Crossbow and Bolt */
			case SV_LIGHT_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;
				break;
			}

			/* Heavy Crossbow and Bolt */
			case SV_HEAVY_XBOW:
			{
				p_ptr->ammo_tval = TV_BOLT;
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
				if (p_ptr->lev > 19) p_ptr->num_fire++;

				/* Extra shot at level 40 */
				if (p_ptr->lev > 39) p_ptr->num_fire++;
			}

			/* Hack -- Rangers are also decent with slings and xbows. */
			if ((p_ptr->pclass == CLASS_RANGER) &&
			    (p_ptr->ammo_tval == TV_SHOT))
			{
				/* Extra shot at level 25 */
				if (p_ptr->lev > 24) p_ptr->num_fire++;
			}
			if ((p_ptr->pclass == CLASS_RANGER) &&
			    (p_ptr->ammo_tval == TV_BOLT))
			{
				/* Extra shot at level 45 */
				if (p_ptr->lev > 44) p_ptr->num_fire++;
			}


			/* Hack -- Warriors can handle most missile weapons effectively. */
			if ((p_ptr->pclass == CLASS_WARRIOR) &&
			    (p_ptr->ammo_tval == TV_ARROW))
			{
				/* Extra shot at level 35 */
				if (p_ptr->lev > 34) p_ptr->num_fire++;
			}
			if ((p_ptr->pclass == CLASS_WARRIOR) &&
			    (p_ptr->ammo_tval == TV_BOLT))
			{
				/* Extra shot at level 40 */
				if (p_ptr->lev > 39) p_ptr->num_fire++;
			}


			/* Hack -- Rogues are great with slings. */
			if ((p_ptr->pclass == CLASS_ROGUE) &&
			    (p_ptr->ammo_tval == TV_SHOT))
			{
				/* Extra shot at level 20 */
				if (p_ptr->lev > 19) p_ptr->num_fire++;
			}
			if ((p_ptr->pclass == CLASS_ROGUE) &&
			    (p_ptr->ammo_tval == TV_SHOT))
			{
				/* Extra shot at level 40 */
				if (p_ptr->lev > 39) p_ptr->num_fire++;
			}

			/* See formula "do_cmd_fire" in "cmd2.c" for Assassin bonus
			 * to Deadliness. */
		}

		/* Require at least one shot */
		if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
	}

	/* Add all class and race-specific adjustments to missile Skill. */
	p_ptr->skill_thb += add_special_missile_skill (p_ptr->pclass, o_ptr->weight, o_ptr);


	/*** Analyze weapon ***/

	/* Examine the "current weapon" */
	o_ptr = &inventory[INVEN_WIELD];

	/* Assume that the player is not a Priest wielding an edged weapon. */
	p_ptr->icky_wield = FALSE;

	/* Assume the weapon is not too heavy */
	p_ptr->heavy_wield = FALSE;

	/* Inflict heavy weapon penalties. */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy weapon */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;

		/* The player gets to swing a heavy weapon only once. */
		p_ptr->num_blow = 1;
	}

	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int effective_weight = 0, mul = 6;

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

		/* Paranoia - require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;


		/* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);
	}

	/* Everyone gets two blows if not wielding a weapon. */
	else if (!o_ptr->k_idx) p_ptr->num_blow = 2;


	/* Add all other class and race-specific adjustments to melee Skill. */
	p_ptr->skill_thn += add_special_melee_skill(p_ptr->pclass, 
		o_ptr->weight, o_ptr);


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
			/* Change in STR may affect how shields are used. */
			if ((i == A_STR) && (inventory[INVEN_ARM].k_idx))
			{
				/* Access the wield slot */
				o_ptr = &inventory[INVEN_WIELD];

				/* Extract the flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* Analyze weapon for two-handed-use. */
				if (f3 & (TR3_TWO_HANDED_REQ) || 
					(f3 & (TR3_TWO_HANDED_DES) && 
					(p_ptr->stat_ind[A_STR] < 
					29 + (o_ptr->weight / 50 > 8 ? 
					8 : o_ptr->weight / 50))))
				{
					p_ptr->shield_on_back = TRUE;

				}
				else p_ptr->shield_on_back = FALSE;

				/* Hack - recalculate bonuses again. */
				if (p_ptr->old_shield_on_back != 
					p_ptr->shield_on_back)
				{
					/* do not check strength again */
					old_stat_ind[i] = p_ptr->stat_ind[i];
					goto calc_again;
				}
			}

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

	/* Take note when player moves his shield on and off his back. */
	if (p_ptr->old_shield_on_back != p_ptr->shield_on_back)
	{
		/* Messages */
		if (p_ptr->shield_on_back)
		{
			msg_print("You are carrying your shield on your back.");
		}
		else if (inventory[INVEN_ARM].k_idx)
		{
			msg_print("You are carrying your shield in your hand.");
		}

		/* No message for players no longer carrying a shield. */

		/* Save it */
		p_ptr->old_shield_on_back = p_ptr->shield_on_back;
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
		process_quiver(NULL, NULL);
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

	if (p_ptr->redraw & (PR_SHAPE))
	{
		p_ptr->redraw &= ~(PR_SHAPE);
		prt_shape();
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
		p_ptr->redraw &= ~(PR_STATUS);
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

	if (p_ptr->redraw & (PR_STATUS))
	{
		p_ptr->redraw &= ~(PR_STATUS);
		prt_status();
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


