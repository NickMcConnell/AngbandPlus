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
 * There is not a set of variables as to whether the player sustain is known or not.
 * This function checks that
 */
static bool known_sustain(int stat)
{
	u32b f1, f2, f3, fn;
	int i;

	/*First check if the stat is actually sustained*/
	switch (stat)
	{
		case A_STR: {if (!p_ptr->sustain_str) return (FALSE); break;}
		case A_INT:	{if (!p_ptr->sustain_int) return (FALSE); break;}
		case A_WIS:	{if (!p_ptr->sustain_wis) return (FALSE); break;}
		case A_DEX:	{if (!p_ptr->sustain_dex) return (FALSE); break;}
		case A_CON:	{if (!p_ptr->sustain_con) return (FALSE); break;}
		case A_AGI:	{if (!p_ptr->sustain_agi) return (FALSE); break;}
		case A_STE:	{if (!p_ptr->sustain_ste) return (FALSE); break;}
		case A_PER:	{if (!p_ptr->sustain_per) return (FALSE); break;}
		case A_LUC:	{if (!p_ptr->sustain_luc) return (FALSE); break;}
		default: break;
	}

	/* Process equipment */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; ++i)
	{
		/* Get the object */
		object_type *o_ptr = &inventory[i];

		/* Get the "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		if (object_known_p(o_ptr))
		{
			/* Known sustain */
			if (f2 & (1<<stat))
			{
				return (TRUE);
			}

		}
	}

	/* Player flags */
	player_flags(&f1, &f2, &f3, &fn);

	/* Sustain */
	if (f2 & (1<<stat))
	{
		return (TRUE);
	}

	/*Not known sustain*/
	return (FALSE);
}


/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat, int row, int col)
{
	char tmp[32];

	byte color = TERM_L_GREEN;

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		put_str(stat_names_reduced[stat], row, col);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_YELLOW, tmp, row, col + 6);
	}

	/* Display "healthy" stat */
	else
	{
		if (known_sustain(stat)) color = TERM_GREEN;

		put_str(stat_names[stat], row, col);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(color, tmp, row, col + 6);
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

	sprintf(tmp, "%6d", p_ptr->lev);

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
 * There is not a set of variables as to whether the player hold life is known or not.
 * This function returns the status of whether player hold life is known.
 */
static bool known_hold_life(void)
{
	u32b f1, f2, f3, fn;
	int i;

	/* First check if the player actually has hold life */
	if (!p_ptr->hold_life) return (FALSE);

	/* Process equipment */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; ++i)
	{
		/* Get the object */
		object_type *o_ptr = &inventory[i];

		/* Get the "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		if (f3 & (TR3_HOLD_LIFE))
		{
			return (TRUE);
		}

	}

	/* Player flags */
	player_flags(&f1, &f2, &f3, &fn);

	/* Sustain */
	if (f3 & (TR3_HOLD_LIFE))
	{
		return (TRUE);
	}

	/*Not known sustain*/
	return (FALSE);
}


/*
 * Display the experience
 */
static void prt_exp(int row, int col)
{
	char out_val[32];
	s32b exp_display;
	byte attr= TERM_L_GREEN;

	/*use different color if player's experience is drained*/
	if (p_ptr->exp >= p_ptr->max_exp)
	{
		if (known_hold_life()) attr = TERM_GREEN;
	}
	else
	{
		attr = TERM_YELLOW;
	}

	/*some players want to see how much they need to gain to get to the next level*/
	if ((toggle_xp) && (p_ptr->lev < PY_MAX_LEVEL))
	{
		exp_display = ((player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L) -
									p_ptr->exp);

		/*Print experience label*/
		put_str("NEXT ", row, col);
	}

	else
	{

		exp_display = p_ptr->exp;

		/*Print experience label*/
		put_str("EXP ", row, col);
	}

	sprintf(out_val, "%8ld", exp_display);

	c_put_str(attr, out_val, row, col + 4);

}


/*
 * Prints current gold
 */
static void prt_gold(int row, int col)
{
	char tmp[32];

	put_str("AU ", row, col);
	sprintf(tmp, "%9ld", (long)p_ptr->au);
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
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
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
 * Prints resistance flags
 */
static void prt_resistances(int row, int col)
{
  	byte color[50];
  	char chr[50], tmpstr[2];
  	int i, n = 0;

  	/* Temporary resistances */
   	{
		byte colors[] = { TERM_SLATE, TERM_L_BLUE, TERM_RED, TERM_WHITE,		TERM_GREEN, TERM_YELLOW };
		bool values[6];

  		const char *chars = "AEFCP?";
		values[0] = ((p_ptr->oppose_acid > 0) && (p_ptr->immune_acid < 1));
  		values[1] = ((p_ptr->oppose_elec > 0) && (p_ptr->immune_elec < 1)),
  		values[2] = ((p_ptr->oppose_fire > 0) && (p_ptr->immune_fire < 1)),
  		values[3] = ((p_ptr->oppose_cold > 0) && (p_ptr->immune_cold < 1)),
  		values[4] = ((p_ptr->oppose_pois > 0) && (p_ptr->immune_pois < 1));
  		values[5] = ((p_ptr->oppose_conf > 0));


		for(i = 0; i < 6; i++)
		{
			if(values[i])
			{
    			color[n] = colors[i];
    			chr[n++] = chars[i];
   			}

   		}

   	}


	/* Temporary nativities */
	{
		byte colors[] = { TERM_YELLOW, TERM_UMBER, TERM_BLUE, TERM_L_DARK,
							TERM_L_RED};
		bool values[5];

		const char *chars = "smwol";

       	values[0] = (p_ptr->temp_native_sand > 0);
        values[1] = (p_ptr->temp_native_mud > 0);
        values[2] = (p_ptr->temp_native_water > 0);
        values[3] = (p_ptr->temp_native_oil > 0);
        values[4] = (p_ptr->temp_native_lava > 0);

		for(i = 0; i < N_ELEMENTS(values); i++)
		{
			if(values[i])
			{
    			color[n] = colors[i];
    			chr[n++] = chars[i];
			}
		}
	}

	/* Temporary element brand for weapons */
	if (p_ptr->slay_elements)
	{
		/* No shimmering */
		if (avoid_other)
		{
			color[n] = TERM_VIOLET;
		}
		/* Pick one of the five elements */
		else
		{
			switch (rand_int(5))
			{
				case 0: color[n] = TERM_RED; break;
				case 1: color[n] = TERM_L_BLUE; break;
				case 2: color[n] = TERM_GREEN; break;
				case 3: color[n] = TERM_SLATE; break;
				default: color[n] = TERM_WHITE; break;
			}
		}

		chr[n++] = 'W';
	}

	/* Flying */
	if (p_ptr->flying)
	{
		color[n] = TERM_L_BLUE;
		chr[n++] = 'F';
	}

  	/* Clear the row */
  	put_str("           ", row, col);

  	/* Print up to 12 flags */
  	for (i = 0; (i < n) && (i < 12); i++)
  	{
    	tmpstr[0] = chr[i];
    	tmpstr[1] = '\0';
    	c_put_str(color[i], tmpstr, row, col + i);
  	}
}



/*
 * Prints current AC
 */
static void prt_ac(int row, int col)
{
	char tmp[32];

	put_str("AC ", row, col);
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, row, col + 7);
}


/*
 * Prints Cur/Max hit points
 */
static void prt_cur_hp(int row, int col)
{
	char tmp[32];
	int len;
	byte color;

	put_str("HP          ", row, col);

	len = sprintf(tmp, "%d:%d", p_ptr->chp, p_ptr->mhp);

	c_put_str(TERM_L_GREEN, tmp, row, col + 12 - len);

	/* Done? */
	if (p_ptr->chp >= p_ptr->mhp) return;

	if (p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	/* Show current hitpoints using another color */
	sprintf(tmp, "%d", p_ptr->chp);

	c_put_str(color, tmp, row, col + 12 - len);
}


/*
 * Prints players max/cur spell points
 */
static void prt_cur_sp(int row, int col)
{
	char tmp[32];
	byte color;
	int len;


	put_str("SP          ", row, col);

	len = sprintf(tmp, "%d:%d", p_ptr->csp, p_ptr->msp);

	c_put_str(TERM_L_GREEN, tmp, row, col + 12 - len);

	/* Done? */
	if (p_ptr->csp >= p_ptr->msp) return;

	if (p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}


	/* Show current mana using another color */
	sprintf(tmp, "%d", p_ptr->csp);

	c_put_str(color, tmp, row, col + 12 - len);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(int row, int col)
{
	char depths[32];
	byte attr = TERM_WHITE;

	if (!p_ptr->depth)
	{
		strcpy(depths, "Town");
	}
	else if (depth_in_feet)
	{
		sprintf(depths, "%d ft", p_ptr->depth * 50);
	}
	else
	{
		sprintf(depths, "Lev %d", p_ptr->depth);
	}

	/* Get color of level based on feeling  -JSV- */
	if ((p_ptr->depth) && (do_feeling))
	{
		if (feeling ==  1) attr = TERM_VIOLET;
		else if (feeling ==  2) attr = TERM_RED;
		else if (feeling ==  3) attr = TERM_L_RED;
		else if (feeling ==  4) attr = TERM_ORANGE;
		else if (feeling ==  5) attr = TERM_ORANGE;
		else if (feeling ==  6) attr = TERM_YELLOW;
		else if (feeling ==  7) attr = TERM_YELLOW;
		else if (feeling ==  8) attr = TERM_WHITE;
		else if (feeling ==  9) attr = TERM_WHITE;
		else if (feeling == 10) attr = TERM_L_WHITE;
		else if (feeling >= LEV_THEME_HEAD) attr = TERM_BLUE;
	}

	 /* Replace depth with the quest indicator */
	if (format_quest_indicator(depths, sizeof(depths)))
	{
		/* Change color if necessary */
		if ((attr == TERM_WHITE) || (attr == TERM_L_WHITE)) attr = TERM_L_GREEN;
	}

	/* Right-Adjust the "depth", and clear old values */
	c_prt(attr, format("%7s", depths), row, col);
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
	if (p_ptr->blind)
	{
		c_put_str(TERM_ORANGE, "Blind", row, col);
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
	if (p_ptr->confused)
	{
		c_put_str(TERM_ORANGE, "Confused", row, col);
	}
	else
	{
		put_str("        ", row, col);
	}
}


/*
 * Prints Fear status
 */
static void prt_afraid(int row, int col)
{
	if (p_ptr->afraid)
	{
		c_put_str(TERM_ORANGE, "Afraid", row, col);
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
	if (p_ptr->poisoned)
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
	c_put_str(attr, text, row, col);
}

/*
 * Hack - Modify the color based on speed bonuses. -DG-
 */
static byte analyze_speed_bonuses(byte default_attr)
{
	if (p_ptr->slow)	return (TERM_ORANGE);
	else if (p_ptr->fast)	return (TERM_VIOLET);
	else	return (default_attr);
}


/*
 * Prints the speed of a character.			-CJS-
 */
static void prt_speed(int row, int col)
{
	int i = p_ptr->pspeed;

	byte attr = TERM_WHITE;
	char buf[32] = "";

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	/* Fast */
	if (i > 110)
	{
		attr = analyze_speed_bonuses(TERM_L_GREEN);
		sprintf(buf, "Fast (+%d)", (i - 110));
	}

	/* Slow */
	else if (i < 110)
	{
		attr = analyze_speed_bonuses(TERM_L_UMBER);
		sprintf(buf, "Slow (-%d)", (110 - i));
	}

	/* Display the speed */
	c_put_str(attr, format("%-10s", buf), row, col);
}


static void prt_study(int row, int col)
{
	char buf[32] = "";

	if (p_ptr->new_spells)
	{
		sprintf(buf, "Study (%d)", p_ptr->new_spells);
	}
	put_str(format("%-10s", buf), row, col);
}


static void prt_cut(int row, int col)
{
	int c = p_ptr->cut;

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



static void prt_stun(int row, int col)
{
	int s = p_ptr->stun;

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
static void health_redraw(int row, int col)
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
	else if (p_ptr->image)
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

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (handle monster stunning, confusion) */
		if (m_ptr->confused)
			Term_putstr(col + 1, row, len, attr, "cccccccccc");
		else if (m_ptr->stunned)
			Term_putstr(col + 1, row, len, attr, "ssssssssss");
		else if (m_ptr->csleep)
			Term_putstr(col + 1, row, len, attr, "zzzzzzzzzz");
		else
			Term_putstr(col + 1, row, len, attr, "**********");

	}
}


/*
 * Redraw the "monster mana bar"
 *
 * The "monster mana bar" provides visual feedback on the "mana"
 * of the monster currently being "tracked".  It follows the lead of the monster
 * health bar for who to track.
 */
static void mana_redraw(int row, int col)
{

	/* Not tracking, or hiding a mimic */
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
	else if (p_ptr->image)
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
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Default to out of mana*/
		byte attr = TERM_RED;

		/*no mana, stop here*/
		if (!r_ptr->mana)
		{
			/* Erase the health bar */
			Term_erase(col, row, 12);

			return;
		}

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->mana / r_ptr->mana;

		/* almost no mana */
		if (pct >= 10) attr = TERM_L_RED;

		/* some mana */
		if (pct >= 25) attr = TERM_ORANGE;

		/* most mana */
		if (pct >= 60) attr = TERM_YELLOW;

		/* full mana */
		if (pct >= 100) attr = TERM_L_GREEN;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");

		/* Dump the current "mana"*/
		Term_putstr(col + 1, row, len, attr, "**********");

	}

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
		(void)Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Hack -- display monsters in sub-windows
 */
static void fix_monlist(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MONLIST))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display visible monsters */
		display_monlist();

		/* Fresh */
		(void)Term_fresh();

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
		(void)Term_fresh();

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
		(void)Term_fresh();

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
		(void)Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack - Display the left-hand-side of the main term in a separate window
 */
static void prt_frame_compact(void)
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

	/* Temp. resists */
	prt_resistances(row++, col);

	/* Armor */
	prt_ac(row++, col);

	/* Hitpoints */
	prt_cur_hp(row++, col);

	/* Spellpoints */
	prt_cur_sp(row++, col);

	/* Special */
	health_redraw(row++, col);

	/* Cut */
	prt_cut(row++, col);

	/* Stun */
	prt_stun(row++, col);
}

/*
 * Hack -- display player in sub-windows (compact)
 */
static void fix_player_compact(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_PLAYER_2))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display player */
		prt_frame_compact();

		/* Fresh */
		(void)Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Hack - Display the status line in a separate window
 */
static void prt_status_line(void)
{
	int row = 0;

	/* Hungry */
	prt_hunger(row, COL_HUNGRY);

	/* Blind */
	prt_blind(row, COL_BLIND);

	/* Confused */
	prt_confused(row, COL_CONFUSED);

	/* Afraid */
	prt_afraid(row, COL_AFRAID);

	/* Poisoned */
	prt_poisoned(row, COL_POISONED);

	/* State */
	prt_state(row, COL_STATE);

	/* Speed */
	prt_speed(row, COL_SPEED);

	/* Study */
	prt_study(row, COL_STUDY);

	/* Depth */
	prt_depth(row, COL_DEPTH);

	/* Temp. resists */
	prt_resistances(row, COL_OPPOSE_ELEMENTS);
}


/*
 * Hack -- display status in sub-windows
 */
static void fix_status(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_STATUS))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display status line */
		prt_status_line();

		/* Fresh */
		(void)Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display dungeon map view in sub-windows.
 */
static void fix_map(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MAP))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/*** The maps are always up-to-date ***/

		/* Fresh */
		(void)Term_fresh();

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
		(void)Term_fresh();

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
		(void)Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Hack -- display feature recall in sub-windows
 */
static void fix_feature(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_FEATURE))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display feature race info */
		if (p_ptr->feature_kind_idx) display_feature_roff(p_ptr->feature_kind_idx);

		/* Fresh */
		(void)Term_fresh();

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
		(void)Term_fresh();

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
		(void)Term_fresh();

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
	int percent_spells;

	const magic_type *s_ptr;

	s16b old_spells;

	cptr p = "spell";

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
	percent_spells = adj_mag_study[SPELL_STAT_SLOT];

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

		/* Don't count Ironman Spells. */
		if (p_ptr->spell_flags[j] & PY_SPELL_IRONMAN) continue;

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

	long bonus;

	/* Get "1/100th spellpoint bonus per level" value */
	bonus = adj_wis_msp[p_ptr->stat_ind[A_WIS]];

	/* Calculate spell points */
	msp = p_ptr->player_sp[p_ptr->lev-1] + (bonus * p_ptr->lev / 100);

	/* Process gloves for those disturbed by them */
	if (cp_ptr->flags & CF_CUMBER_GLOVE)
	{
		u32b f1, f2, f3, native;

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, &f1, &f2, &f3, &native);

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
		int i = 100;

		/* Get percentage of maximum hp */
		if (p_ptr->mhp) i = ((100 * p_ptr->chp) / p_ptr->mhp);

		/* Save new limit */
		p_ptr->mhp = mhp;

		/* Update current maximum hp */
		p_ptr->chp = ((i * p_ptr->mhp) / 100) + (((i * p_ptr->mhp) % 100 >= 50)	? 1 : 0);

		/* Hack - any change in max hitpoint resets frac */
		p_ptr->chp_frac = 0;

		/* Dihplay hp later */
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
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3, native;

	s16b old_lite = p_ptr->cur_lite;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Examine actual lites */
		if (o_ptr->tval == TV_LITE)
		{
			/* Artifact Lites provide permanent, bright, lite */
			if (artifact_p(o_ptr))
			{
				p_ptr->cur_lite += 3;
				continue;
			}

			if (o_ptr->sval == SV_LITE_GLOW1)
			{
				p_ptr->cur_lite += 1;
				continue;
			}

			if (o_ptr->sval == SV_LITE_GLOW2)
			{
				p_ptr->cur_lite += 2;
				continue;
			}

			if (o_ptr->sval == SV_LITE_GLOW3)
			{
				p_ptr->cur_lite += 3;
				continue;
			}

			/* Lanterns (with fuel) provide more lite */
			if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->timeout > 0))
			{
				object_flags(o_ptr, &f1, &f2, &f3, &native);

				/* Resist Dark means light radius of 3 */
				if ((f2 & TR2_RES_DARK) && (o_ptr->pval >= 0))
				{
				  p_ptr->cur_lite += 3;
				}
				/* Resist Light means light radius of 1 */
				/* The same with cursed lanterns */
				else if ((f2 & TR2_RES_LITE) || (o_ptr->pval < 0))
				{
				  p_ptr->cur_lite += 1;
				}
				else
				{
				  p_ptr->cur_lite += 2;
				}

				continue;
			}

			/* Torches (with fuel) provide some lite */
			if ((o_ptr->sval == SV_LITE_TORCH || o_ptr->sval == SV_LITE_MAGELIGHT) && (o_ptr->timeout > 0))
			{
				p_ptr->cur_lite += 1;
				continue;
			}
		}
		else
		{
			/* Extract the flags */
			object_flags(o_ptr, &f1, &f2, &f3, &native);

			/* does this item glow? */
			if (f3 & TR3_LITE) p_ptr->cur_lite++;
		}
	}


	/* Player is glowing */
	if (p_ptr->lite) p_ptr->cur_lite++;

	if (p_ptr->cur_lite){
		p_ptr->cur_lite += adj_per_lite[p_ptr->stat_ind[A_PER]];
	}

	/* Reduce lite when running if requested */
	if (p_ptr->running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
	}

	/* Notice changes in the "lite radius" */

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW);
	p_ptr->update |= (PU_MONSTERS);

	/* Notice changes in the "lite radius" */
	 if (old_lite != p_ptr->cur_lite)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}
}

/*
 * Extract and set the current nativity status
 */
static void calc_nativity(void)
{
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3, fn;

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3, &fn);

	/* Assume nothing */
	p_ptr->p_native = 0L;

	/* Assume nothing known */
	p_ptr->p_native_known = 0L;

	/*Class and Race Native flags*/
	p_ptr->p_native |= fn;
	p_ptr->p_native_known |= fn;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3, &fn);

		p_ptr->p_native |= fn;

		/* Don't know all the flags */
		if (!object_known_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags_known(o_ptr, &f1, &f2, &f3, &fn);

		p_ptr->p_native_known |= fn;

	}

	/*Manually add the temporary native flags.  Assume known*/
	if (p_ptr->temp_native_lava)
	{
		p_ptr->p_native |= P_NATIVE_LAVA;
		p_ptr->p_native_known |= P_NATIVE_LAVA;
	}
	if (p_ptr->temp_native_oil)
	{
		p_ptr->p_native |= P_NATIVE_OIL;
		p_ptr->p_native_known |= P_NATIVE_OIL;
	}
	if (p_ptr->temp_native_sand)
	{
		p_ptr->p_native |= P_NATIVE_SAND;
		p_ptr->p_native_known |= P_NATIVE_SAND;
	}
	if (p_ptr->temp_native_forest)
	{
		p_ptr->p_native |= P_NATIVE_FOREST;
		p_ptr->p_native_known |= P_NATIVE_FOREST;
	}
	if (p_ptr->temp_native_water)
	{
		p_ptr->p_native |= P_NATIVE_WATER;
		p_ptr->p_native_known |= P_NATIVE_WATER;
	}
	if (p_ptr->temp_native_mud)
	{
		p_ptr->p_native |= P_NATIVE_MUD;
		p_ptr->p_native_known |= P_NATIVE_MUD;
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


/*Re-calculate the player stealth*/
static void calc_stealth(void)
{
	int old_skill_stl, i;

	/* Save the old stealth */
	old_skill_stl = p_ptr->skill_stl;

	/* Base skill -- stealth */
	p_ptr->skill_stl = adj_ste_stealth[p_ptr->stat_ind[A_STE]];

	/* Very simple if flying */
	if (p_ptr->flying)
	{
		/*Very quiet*/
		p_ptr->skill_stl += 3;
	}

	/*Feature affects skill*/
	p_ptr->skill_stl += f_info[cave_feat[p_ptr->py][p_ptr->px]].f_stealth_adj;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Recalculate stealth when needed */
	if ((p_ptr->skill_stl != old_skill_stl) || (!p_ptr->skill_stl))
	{
		/* Assume character is extremely noisy. */
		p_ptr->base_wakeup_chance = 100 * WAKEUP_ADJ;

		/* For every increase in stealth past 0, multiply wakeup chance by 0.86. */
		for (i = 0; i < p_ptr->skill_stl; i++)
		{
			p_ptr->base_wakeup_chance = (86 * p_ptr->base_wakeup_chance) / 100;

			/* Always make at least some innate noise */
			if (p_ptr->base_wakeup_chance < 30)
			{
				p_ptr->base_wakeup_chance = 30;
				break;
			}
		}
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

	int old_telepathy, old_sr_telepathy_1, old_sr_telepathy_2;
	int old_see_inv;


	int old_dis_ac;
	int old_dis_to_a;

	int extra_blows;
	int extra_shots;
	int extra_might;

	int old_stat_top[A_MAX];
	int old_stat_use[A_MAX];
	int old_stat_ind[A_MAX];

	bool old_heavy_shoot;
	bool old_heavy_wield;
	bool old_icky_wield;

	object_type *o_ptr;

	u32b f1, f2, f3, fn;


	/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->pspeed;



	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_sr_telepathy_1 = p_ptr->sr_telepathy_1;
	old_sr_telepathy_2 = p_ptr->sr_telepathy_2;
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
	p_ptr->num_blows_times_ten = 10;
	extra_blows = 0;

	/* Reset "fire" info */
	p_ptr->num_fire_times_ten = 0;
	p_ptr->ammo_mult = 0;
	p_ptr->ammo_tval = 0;
	extra_shots = 0;
	extra_might = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h_melee = p_ptr->to_h_melee = 0;
	p_ptr->dis_to_d_melee = p_ptr->to_d_melee = 0;
	p_ptr->dis_to_h_missile = p_ptr->to_h_missile = 0;
	p_ptr->dis_to_d_missile = p_ptr->to_d_missile = 0;
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
	p_ptr->sr_telepathy_1 = FALSE;
	p_ptr->sr_telepathy_2 = FALSE;
	p_ptr->lite = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_agi = FALSE;
	p_ptr->sustain_ste = FALSE;
	p_ptr->sustain_per = FALSE;
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
	p_ptr->immune_pois = FALSE;

	p_ptr->n_woken = 0;

	/*** Extract race/class info ***/

	/* Base infravision */
	p_ptr->see_infra = rp_ptr->infra;

	/* Base skill -- disarming */
	p_ptr->skill_dis = rp_ptr->r_dis + cp_ptr->c_dis;

	/* Base skill -- magic devices */
	p_ptr->skill_dev = rp_ptr->r_dev + cp_ptr->c_dev;

	/* Base skill -- saving throw */
	p_ptr->skill_sav = rp_ptr->r_sav + cp_ptr->c_sav;

	/* Base skill -- searching ability */
	p_ptr->skill_srh = rp_ptr->r_srh + cp_ptr->c_srh + 2 * adj_per_search[p_ptr->stat_ind[A_PER]];
	if (p_ptr->skill_srh <= 2){
		p_ptr->skill_srh = 2;
	}

	/* Base skill -- searching frequency */
	p_ptr->skill_fos = rp_ptr->r_fos + cp_ptr->c_fos + 2 * adj_per_search[p_ptr->stat_ind[A_PER]];
	if (p_ptr->skill_fos <= 2){
		p_ptr->skill_fos = 2;
	}

	/* Base skill -- combat (normal) */
	p_ptr->skill_thn = rp_ptr->r_thn + cp_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- digging */
	p_ptr->skill_dig = 0;

	/*** Analyze player ***/

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3, &fn);

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
	if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;

	/* Immunity flags */
	if (f2 & (TR2_IM_FIRE)) p_ptr->immune_fire = TRUE;
	if (f2 & (TR2_IM_ACID)) p_ptr->immune_acid = TRUE;
	if (f2 & (TR2_IM_COLD)) p_ptr->immune_cold = TRUE;
	if (f2 & (TR2_IM_ELEC)) p_ptr->immune_elec = TRUE;
	if (f2 & (TR2_IM_POIS)) p_ptr->immune_pois = TRUE;

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
	if (f2 & (TR2_SUST_AGI)) p_ptr->sustain_agi = TRUE;
	if (f2 & (TR2_SUST_STE)) p_ptr->sustain_ste = TRUE;
	if (f2 & (TR2_SUST_PER)) p_ptr->sustain_per = TRUE;
	if (f2 & (TR2_SUST_LUC)) p_ptr->sustain_luc = TRUE;


	if (adj_con_slowdig[p_ptr->stat_ind[A_CON]]) p_ptr->slow_digest = TRUE;
	if (adj_ste_rdark[p_ptr->stat_ind[A_STE]])   p_ptr->resist_dark = TRUE;

	/*** Analyze equipment ***/

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3, &fn);

		/* Affect stats */
		if (f1 & (TR1_STR)) p_ptr->stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT)) p_ptr->stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS)) p_ptr->stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX)) p_ptr->stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON)) p_ptr->stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_AGI)) p_ptr->stat_add[A_AGI] += o_ptr->pval;
		if (f1 & (TR1_STE)) p_ptr->stat_add[A_STE] += o_ptr->pval;
		if (f1 & (TR1_PER)) p_ptr->stat_add[A_PER] += o_ptr->pval;
		if (f1 & (TR1_LUC)) p_ptr->stat_add[A_LUC] += o_ptr->pval;

		if (wield_slot(o_ptr)==(INVEN_BODY)) p_ptr->stat_add[A_STE] += o_ptr->to_h;
		if (f3 & (TR3_LO_STEALTH)) p_ptr->stat_add[A_STE] -= 2;
		if (f3 & (TR3_LO_PERCEPTION)) p_ptr->stat_add[A_PER] -= 2;

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

		/* Affect shots */
		if (f1 & (TR1_SHOTS)) extra_shots += o_ptr->pval;

		/* Affect Might */
		if (f1 & (TR1_MIGHT)) extra_might += o_ptr->pval;

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_VAMPIRE)) p_ptr->vampire = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
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
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE)) p_ptr->immune_fire = TRUE;
		if (f2 & (TR2_IM_ACID)) p_ptr->immune_acid = TRUE;
		if (f2 & (TR2_IM_COLD)) p_ptr->immune_cold = TRUE;
		if (f2 & (TR2_IM_ELEC)) p_ptr->immune_elec = TRUE;
		if (f2 & (TR2_IM_POIS)) p_ptr->immune_pois = TRUE;

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
		if (f2 & (TR2_SUST_AGI)) p_ptr->sustain_agi = TRUE;
		if (f2 & (TR2_SUST_STE)) p_ptr->sustain_ste = TRUE;
		if (f2 & (TR2_SUST_PER)) p_ptr->sustain_per = TRUE;
		if (f2 & (TR2_SUST_LUC)) p_ptr->sustain_luc = TRUE;

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr) || (o_ptr->ident & (IDENT_SENSE))) p_ptr->dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h_melee += o_ptr->to_h;
		p_ptr->to_h_missile += o_ptr->to_h;
		p_ptr->to_d_melee += o_ptr->to_d;
		p_ptr->to_d_missile += 0; /* slaying items don't help missile damage */

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr) || (o_ptr->ident & (IDENT_SENSE))) p_ptr->dis_to_h_melee += o_ptr->to_h;
		if (object_known_p(o_ptr) || (o_ptr->ident & (IDENT_SENSE))) p_ptr->dis_to_h_missile += o_ptr->to_h;
		if (object_known_p(o_ptr) || (o_ptr->ident & (IDENT_SENSE))) p_ptr->dis_to_d_melee += o_ptr->to_d;
		if (object_known_p(o_ptr) || (o_ptr->ident & (IDENT_SENSE))) p_ptr->dis_to_d_missile += 0; /* slaying items don't help missile damage */
	}

	/* Find cursed ammo in the quiver */
	p_ptr->cursed_quiver = FALSE;

	/* Scan the quiver */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Found cursed ammo */
		if (cursed_p(o_ptr))
		{
			/* Remember it */
			p_ptr->cursed_quiver = TRUE;

			/* Done */
			break;
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


	/*** Temporary flags ***/

	/* Apply temporary "stun" */
	if (p_ptr->stun > 50)
	{
		p_ptr->to_h_melee -= 20;
		p_ptr->to_h_missile -= 20;
		p_ptr->dis_to_h_melee -= 20;
		p_ptr->dis_to_h_missile -= 20;
		p_ptr->to_d_melee -= 20;
		p_ptr->dis_to_d_melee -= 20; /* but not to_d_missile or dis_to_d_missile */
	}
	else if (p_ptr->stun)
	{
		p_ptr->to_h_melee -= 5;
		p_ptr->to_h_missile -= 5;
		p_ptr->dis_to_h_melee -= 5;
		p_ptr->dis_to_h_missile -= 5;
		p_ptr->to_d_melee -= 5;
		p_ptr->dis_to_d_melee -= 5; /* but not to_d_missile or dis_to_d_missile */
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
		p_ptr->to_h_melee += 10;
		p_ptr->to_h_missile += 10;
		p_ptr->dis_to_h_melee += 10;
		p_ptr->dis_to_h_missile += 10;
	}

	/* Temporary shield */
	if (p_ptr->shield)
	{
		p_ptr->to_a += 30;
		p_ptr->dis_to_a += 30;
	}

	/* Temporary shield */
	if (p_ptr->megashield)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h_melee += 12;
		p_ptr->dis_to_h_melee += 12;
		p_ptr->to_h_missile += 12;
		p_ptr->dis_to_h_missile += 12;
	}

	/* Temporary "Berserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h_melee += 24;
		p_ptr->dis_to_h_melee += 24;
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
		p_ptr->see_infra += 5;
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
	if (j > i / 2) p_ptr->pspeed -= ((j - (i / 2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

	/* Sanity check on extreme speeds */
	if (p_ptr->pspeed < 0) p_ptr->pspeed = 0;
	if (p_ptr->pspeed > 199) p_ptr->pspeed = 199;

	/*** Apply modifier bonuses ***/

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_agi_ta[p_ptr->stat_ind[A_AGI]]) - 128);
	p_ptr->to_h_melee += ((int)(adj_dex_th_mel[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h_missile += ((int)(adj_per_th_mis[p_ptr->stat_ind[A_PER]]) - 128);
	p_ptr->to_d_melee += ((int)(adj_str_td_mel[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_d_missile += ((int)(adj_per_td_mis[p_ptr->stat_ind[A_PER]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_agi_ta[p_ptr->stat_ind[A_AGI]]) - 128);
	p_ptr->dis_to_h_melee += ((int)(adj_dex_th_mel[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h_missile += ((int)(adj_per_th_mis[p_ptr->stat_ind[A_PER]]) - 128);
	p_ptr->dis_to_d_melee += ((int)(adj_str_td_mel[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_d_missile += ((int)(adj_per_td_mis[p_ptr->stat_ind[A_PER]]) - 128);


	/*** Modify skills ***/

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skill_dis += adj_dex_dis[p_ptr->stat_ind[A_DEX]];
	p_ptr->skill_dis += adj_int_dis[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skill_dev += adj_int_dev[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skill_sav += adj_wis_sav[p_ptr->stat_ind[A_WIS]];
	if (p_ptr->skill_sav<0) p_ptr->skill_sav=0;

	/* Affect Skill -- digging (STR) */
	p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];

	/* Affect Skill -- disarming (Level, by Class) */
	p_ptr->skill_dis += (cp_ptr->x_dis * p_ptr->lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->skill_dev += (cp_ptr->x_dev * p_ptr->lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->skill_sav += (cp_ptr->x_sav * p_ptr->lev / 10);

	if (p_ptr->skill_sav >= 90){
		p_ptr->skill_sav = 90;
	}

	if (p_ptr->megashield)
	{
		p_ptr->skill_sav += 30;
	}

	if (p_ptr->bless_blade)
	{
		p_ptr->skill_sav += 10;
	}

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
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	p_ptr->pspeed = p_ptr->pspeed + adj_agi_speed[p_ptr->stat_ind[A_AGI]];
	p_ptr->see_infra = p_ptr->see_infra + adj_per_infra[p_ptr->stat_ind[A_PER]];
	if (adj_per_telepathy[p_ptr->stat_ind[A_PER]]== 1){
		p_ptr->sr_telepathy_1 = 1;
	} else if (adj_per_telepathy[p_ptr->stat_ind[A_PER]]== 2){
		p_ptr->sr_telepathy_2 = 1;
	}

	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];


	/*** Analyze current bow ***/

	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* Analyze launcher */
	if (o_ptr->k_idx)
	{
		/* Get to shoot */
		p_ptr->num_fire_times_ten = adj_dex_shots[p_ptr->stat_ind[A_DEX]];

		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
			/* Sling and ammo */
			case SV_SLING:
			{
				p_ptr->ammo_tval = TV_SHOT;
				p_ptr->ammo_mult = 2;

				/*Hack - Rogues get increased skill with slings*/
				if (cp_ptr->flags & CF_ROGUE_COMBAT)
				{
					p_ptr->skill_thb += 3 + p_ptr->lev / 4;
				}
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
			p_ptr->num_fire_times_ten += extra_shots * 10;

			/* Extra might */
			p_ptr->ammo_mult += extra_might;
		}

		/* Require at least one shot */
		if (p_ptr->num_fire_times_ten < 10) p_ptr->num_fire_times_ten = 10;
	}


	/*** Analyze weapon ***/

	/* Examine the "current weapon" */
	o_ptr = &inventory[INVEN_WIELD];

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* It is hard to hold a heavy weapon */
	if (hold < o_ptr->weight)
	{
		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	}

	/* Normal weapons */
	if (o_ptr->k_idx)
	{
		p_ptr->num_blows_times_ten = adj_dex_blows[p_ptr->stat_ind[A_DEX]];

		if (p_ptr->heavy_wield){
			p_ptr->num_blows_times_ten = p_ptr->num_blows_times_ten / 2;
		}

		/* Maximal value */
		if (p_ptr->num_blows_times_ten > 10*cp_ptr->max_attacks) p_ptr->num_blows_times_ten = 10*cp_ptr->max_attacks;

		/* Add in the "bonus blows" */
		p_ptr->num_blows_times_ten += 10 * extra_blows;

		/* Require at least 1 blow */
		if (p_ptr->num_blows_times_ten < 10) p_ptr->num_blows_times_ten = 10;

		/*add extra attack for those who have the flag*/
		if ((p_ptr->lev >= LEV_EXTRA_COMBAT) && (cp_ptr->flags & CF_EXTRA_ATTACK))
			p_ptr->num_blows_times_ten += 10;

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
		p_ptr->to_h_melee -= 2;
		p_ptr->to_d_melee -= 2;

		/* Reduce the mental bonuses */
		p_ptr->dis_to_h_melee -= 2;
		p_ptr->dis_to_d_melee -= 2;

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
			else if (i == A_INT || i==A_WIS)
			{

				if ((cp_ptr->spell_book == TV_MAGIC_BOOK) ||
					(cp_ptr->spell_book == TV_DRUID_BOOK))
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

		}
	}

	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy || p_ptr->sr_telepathy_1 != old_sr_telepathy_1 || p_ptr->sr_telepathy_2 != old_sr_telepathy_2)
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
		combine_quiver();
	}

	/* Reorder the pack */
	if (p_ptr->notice & (PN_REORDER))
	{
		p_ptr->notice &= ~(PN_REORDER);
		reorder_pack();
		(void)reorder_quiver(0);
	}

	if(p_ptr->notice & PN_AUTOINSCRIBE)
	{
		p_ptr->notice &= ~(PN_AUTOINSCRIBE);
		autoinscribe_pack();
		autoinscribe_ground();
	}


}


/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
	/* Update stuff */
	if (!p_ptr->update) return;

	if (p_ptr->update & (PU_TORCH))
	{
		p_ptr->update &= ~(PU_TORCH);
		calc_torch();
	}

	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		calc_bonuses();

		/*hack = always re-check stealth & nativity*/
		p_ptr->update |= (PU_STEALTH | PU_NATIVE);
	}

	if (p_ptr->update & (PU_NATIVE))
	{
		p_ptr->update &= ~(PU_NATIVE);
		calc_nativity();
		p_ptr->redraw |= PR_RESIST;

	}

	if (p_ptr->update & (PU_STEALTH))
	{
		p_ptr->update &= ~(PU_STEALTH);
		calc_stealth();
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

	/*if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		calc_spells();
	}*/


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

	/*Update the flows  */
	if (p_ptr->update & (PU_FLOW_DOORS | PU_FLOW_NO_DOORS))
	{
		update_flows(TRUE);
		p_ptr->update &= ~(PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
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
	                     PR_STATS | PR_RESIST | PR_ARMOR | PR_HP | PR_MANA |
	                     PR_GOLD | PR_HEALTH | PR_EQUIPPY | PR_CUT |
	                     PR_STUN | PR_EXP))
	{
		p_ptr->window |= PW_PLAYER_2;
	}

	/* HACK - Redraw window "Display status" if necessary */
	if (p_ptr->redraw & (PR_HUNGER | PR_BLIND | PR_CONFUSED | PR_AFRAID |
	                     PR_POISONED | PR_STATE | PR_SPEED | PR_STUDY |
	                     PR_DEPTH | PR_RESIST))
	{
		p_ptr->window |= PW_STATUS;
	}


	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
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
		prt_title(ROW_TITLE, COL_TITLE);
	}

	if (p_ptr->redraw & (PR_LEV))
	{
		p_ptr->redraw &= ~(PR_LEV);
		prt_level(ROW_LEVEL, COL_LEVEL);
	}

	if (p_ptr->redraw & (PR_EXP))
	{
		p_ptr->redraw &= ~(PR_EXP);
		prt_exp(ROW_EXP, COL_EXP);
	}

	if (p_ptr->redraw & (PR_STATS))
	{
		int i;

		for (i = 0; i < A_MAX; i++)
			prt_stat(i, ROW_STAT + i, COL_STAT);

		p_ptr->redraw &= ~(PR_STATS);
	}

	if (p_ptr->redraw & (PR_RESIST))
	{
	  p_ptr->redraw &= ~(PR_RESIST);
	  prt_resistances(ROW_RESIST, COL_RESIST);
	}

	if (p_ptr->redraw & (PR_ARMOR))
	{
		p_ptr->redraw &= ~(PR_ARMOR);
		prt_ac(ROW_AC, COL_AC);
	}

	if (p_ptr->redraw & (PR_HP))
	{
		p_ptr->redraw &= ~(PR_HP);
		prt_cur_hp(ROW_HP, COL_HP);

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

	if (p_ptr->redraw & (PR_MANA))
	{
		p_ptr->redraw &= ~(PR_MANA);
		prt_cur_sp(ROW_SP, COL_SP);
	}

	if (p_ptr->redraw & (PR_GOLD))
	{
		p_ptr->redraw &= ~(PR_GOLD);
		prt_gold(ROW_GOLD, COL_GOLD);
	}

	if (p_ptr->redraw & (PR_EQUIPPY))
	{
		p_ptr->redraw &= ~(PR_EQUIPPY);
		prt_equippy(ROW_EQUIPPY, COL_EQUIPPY);
	}

	if (p_ptr->redraw & (PR_DEPTH))
	{
		p_ptr->redraw &= ~(PR_DEPTH);
		prt_depth(ROW_DEPTH, COL_DEPTH);
	}

	if (p_ptr->redraw & (PR_HEALTH))
	{
		p_ptr->redraw &= ~(PR_HEALTH);
		health_redraw(ROW_INFO, COL_INFO);
	}

	if (p_ptr->redraw & (PR_MON_MANA))
	{
		p_ptr->redraw &= ~(PR_MON_MANA);
		mana_redraw(ROW_MON_MANA, COL_MON_MANA);
	}

	if (p_ptr->redraw & (PR_CUT))
	{
		p_ptr->redraw &= ~(PR_CUT);
		prt_cut(ROW_CUT, COL_CUT);
	}

	if (p_ptr->redraw & (PR_STUN))
	{
		p_ptr->redraw &= ~(PR_STUN);
		prt_stun(ROW_STUN, COL_STUN);
	}

	if (p_ptr->redraw & (PR_HUNGER))
	{
		p_ptr->redraw &= ~(PR_HUNGER);
		prt_hunger(ROW_HUNGRY, COL_HUNGRY);
	}

	if (p_ptr->redraw & (PR_BLIND))
	{
		p_ptr->redraw &= ~(PR_BLIND);
		prt_blind(ROW_BLIND, COL_BLIND);
	}

	if (p_ptr->redraw & (PR_CONFUSED))
	{
		p_ptr->redraw &= ~(PR_CONFUSED);
		prt_confused(ROW_CONFUSED, COL_CONFUSED);
	}

	if (p_ptr->redraw & (PR_AFRAID))
	{
		p_ptr->redraw &= ~(PR_AFRAID);
		prt_afraid(ROW_AFRAID, COL_AFRAID);
	}

	if (p_ptr->redraw & (PR_POISONED))
	{
		p_ptr->redraw &= ~(PR_POISONED);
		prt_poisoned(ROW_POISONED, COL_POISONED);
	}

	if (p_ptr->redraw & (PR_STATE))
	{
		p_ptr->redraw &= ~(PR_STATE);
		prt_state(ROW_STATE, COL_STATE);
	}

	if (p_ptr->redraw & (PR_SPEED))
	{
		p_ptr->redraw &= ~(PR_SPEED);
		prt_speed(ROW_SPEED, COL_SPEED);
	}

	if (p_ptr->redraw & (PR_STUDY))
	{
		p_ptr->redraw &= ~(PR_STUDY);
		prt_study(ROW_STUDY, COL_STUDY);
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

	/* Display monster list */
	if (p_ptr->window & (PW_MONLIST))
	{
		p_ptr->window &= ~(PW_MONLIST);
		fix_monlist();
	}

	/* Display status */
	if (p_ptr->window & (PW_STATUS))
	{
		p_ptr->window &= ~(PW_STATUS);
		fix_status();
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

	/* Display player (compact) */
	if (p_ptr->window & (PW_PLAYER_2))
	{
		p_ptr->window &= ~(PW_PLAYER_2);
		fix_player_compact();
	}

	/* Display map view */
	if (p_ptr->window & (PW_MAP))
	{
		p_ptr->window &= ~(PW_MAP);
		fix_map();
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

	/* Display object recall */
	if (p_ptr->window & (PW_OBJECT))
	{
		p_ptr->window &= ~(PW_OBJECT);
		fix_object();
	}
	/* Display feature recall */
	if (p_ptr->window & (PW_FEATURE))
	{
		p_ptr->window &= ~(PW_FEATURE);
		fix_feature();
	}
	/* Display monster recall */
	if (p_ptr->window & (PW_MONSTER))
	{
		p_ptr->window &= ~(PW_MONSTER);
		fix_monster();
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



