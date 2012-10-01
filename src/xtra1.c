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
 * Modify a stat value by a "modifier", return new value
 */
byte modify_stat_value(byte value, int amount)
{
	if (amount > 0) value += amount;

	if (amount < 0)
	{
		/* Use absolute value */
		amount *= -1; 
		if (amount >= value) value = 0;
		else value -= amount;
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
	byte a = TERM_L_GREEN;
	int offset = 0;

	/* Display "injured" stat */
	if (p_ptr->stat_use[stat] < p_ptr->stat_top[stat])
	{
		a = ((p_ptr->stat_use[stat] >0) && (p_ptr->stat_cur[stat] > 0)) ? 
			TERM_YELLOW : TERM_RED;
		put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], ROW_STAT + stat, 0);
	}

	/* Right-justify */
	if (p_ptr->stat_use[stat] < 10) offset++;
	if (p_ptr->stat_top[stat] < 10) offset++;

	sprintf(tmp, "  %d/%d", p_ptr->stat_use[stat], p_ptr->stat_top[stat]);
	c_put_str(a, tmp, ROW_STAT + stat, COL_STAT + 5 + offset);

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 20)
	{
		put_str("!", ROW_STAT + stat, 3);
	}
}

/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	cptr p;

	/* Winner */
	if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "***WINNER***";
	}

	/* Normal */
	else
	{
#ifndef PREVENT_LOAD_C_TEXT
		p = c_text+cp_ptr->title[(p_ptr->lev-1)/5];
#else /* PREVENT_LOAD_C_TEXT */
		p = " ";
#endif /* PREVENT_LOAD_C_TEXT */
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
	char out_val[8];

	if (p_ptr->lev < PY_MAX_LEVEL)
	{
		long val = (long)(((player_exp[p_ptr->lev - 1] * p_ptr->expfact) / 100L) - p_ptr->exp);

		/* Boundary check */
		if (val < 0) val = 0;

		sprintf(out_val, "%7ld", val);

		if (p_ptr->exp >= p_ptr->max_exp)
		{
			put_str("NEXT ", ROW_EXP, 0);
			c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 5);
		}
		else
		{
			put_str("Next ", ROW_EXP, 0);
			c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 5);
		}
	}
	else
	{
		put_str("NEXT ", ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, "******* ", ROW_EXP, COL_EXP + 5);
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

	byte color, offset;
	byte old_attr = r_info[0].x_attr;

	put_str("HP  ", ROW_HP, COL_HP);
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

	/* Hack - only change the colour if in character mode */
	if (r_info[0].x_char != '@') return;

	/* Only change colour if asked */
	if (!view_player_color)
	{
		/* Normal colour is white */
		color = TERM_WHITE;
	}
	else
	{
		/* Normal colour is white */
		if (color == TERM_L_GREEN) color = TERM_WHITE;

		/* Orange is better than yellow */
		if (color == TERM_YELLOW) color = TERM_ORANGE;
	}

	/* Redraw the player ? */
	if (old_attr != color)
	{
		/* Change the player color */
		r_info[0].x_attr = color;

		/* Show the change */
		if (character_dungeon) lite_spot(p_ptr->py, p_ptr->px);
	}
}

/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
	char tmp[32];
	byte color, offset;

	/* Do not show mana unless it matters */
	if (p_ptr->msp>0)
	{
		put_str("SP  ", ROW_SP, COL_SP);
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
	else
	{
		put_str("             ", ROW_SP, COL_SP);
	}
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
		put_str("      ", ROW_HUNGRY, COL_HUNGRY);
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
	if (p_ptr->confused > PY_CONF_INSANE)
	{
		c_put_str(TERM_L_RED, "Insane   ", ROW_CONFUSED, COL_CONFUSED);
	}
	else if (p_ptr->confused > PY_CONF_BEFUDDLE)
	{
		c_put_str(TERM_RED, "Befuddled", ROW_CONFUSED, COL_CONFUSED);
	}
	else if (p_ptr->confused > PY_CONF_CONFUSE)
	{
		c_put_str(TERM_ORANGE, "Confused ", ROW_CONFUSED, COL_CONFUSED);
	}
	else if (p_ptr->confused)
	{
		c_put_str(TERM_YELLOW, "Perplexed", ROW_CONFUSED, COL_CONFUSED);
	}
	else
	{
		put_str("         ", ROW_CONFUSED, COL_CONFUSED);
	}
}

/*
 * Prints Fear status
 */
static void prt_afraid(void)
{
	if (p_ptr->afraid > PY_FEAR_PANIC)
	{
		c_put_str(TERM_L_RED, "Panic ", ROW_AFRAID, COL_AFRAID);
	}
	else if (p_ptr->afraid > PY_FEAR_TERROR)
	{
		c_put_str(TERM_RED, "Terror", ROW_AFRAID, COL_AFRAID);
	}
	else if (p_ptr->afraid > PY_FEAR_AFRAID)
	{
		c_put_str(TERM_ORANGE, "Afraid", ROW_AFRAID, COL_AFRAID);
	}
	else if (p_ptr->afraid)
	{
		c_put_str(TERM_YELLOW, "Wary  ", ROW_AFRAID, COL_AFRAID);
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
 * Prints Diseased status
 */
static void prt_diseased(void)
{
	if (p_ptr->diseased)
	{
		c_put_str(TERM_ORANGE, "Diseased", ROW_DISEASED, COL_DISEASED);
	}
	else
	{
		put_str("        ", ROW_DISEASED, COL_DISEASED);
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

		strcpy(text, "Paralyzed");
	}

	/* Resting */
	else if (p_ptr->resting)
	{
		int i;
		int n = p_ptr->resting;

		/* Start with "Rest" */
		strcpy(text, "Rest     ");

		/* Extensive (timed) rest */
		if (n >= 1000)
		{
			i = n / 100;
			text[8] = '0';
			text[7] = '0';
			text[6] = I2D(i % 10);
			if (i >= 10)
			{
				i = i / 10;
				text[5] = I2D(i % 10);
				if (i >= 10)
				{
					text[4] = I2D(i / 10);
				}
			}
		}

		/* Long (timed) rest */
		else if (n >= 100)
		{
			i = n;
			text[8] = I2D(i % 10);
			i = i / 10;
			text[7] = I2D(i % 10);
			text[6] = I2D(i / 10);
		}

		/* Medium (timed) rest */
		else if (n >= 10)
		{
			i = n;
			text[8] = I2D(i % 10);
			text[7] = I2D(i / 10);
		}

		/* Short (timed) rest */
		else if (n > 0)
		{
			i = n;
			text[8] = I2D(i);
		}

		/* Rest until healed */
		else if (n == -1)
		{
			text[4] = text[5] = text[6] = text[7] = text[8] = '*';
		}

		/* Rest until done */
		else if (n == -2)
		{
			text[4] = text[5] = text[6] = text[7] = text[8] = '&';
		}

		 /* Rest for HPs */
		else if (n == -3)
		{
			text[7] = 'H';
			text[8] = 'P';
		}
	
		/* Rest for SPs */
		else if (n == -4)
		{
			text[7] = 'S';
			text[8] = 'P';
		}
	}

	/* Repeating */
	else if (p_ptr->command_rep)
	{
		if (p_ptr->command_rep > 999)
		{
			sprintf(text, "Rep %3d00", p_ptr->command_rep / 100);
		}
		else
		{
			sprintf(text, "Rep.  %3d", p_ptr->command_rep);
		}
	}

	/* Searching */
	else if (p_ptr->searching)
	{
		strcpy(text, "Searching");
	}

	/* Nothing interesting */
	else
	{
		strcpy(text, "         ");
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
	c_put_str(attr, format("%-10s", buf), ROW_SPEED, COL_SPEED);
}

static void prt_study(void)
{
	if (p_ptr->new_spells)
	{
		put_str(format("Study (%d) ",p_ptr->new_spells), ROW_STUDY, COL_STUDY);
	}
	else
	{
		put_str("          ", ROW_STUDY, COL_STUDY);
	}
}

static void prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > PY_CUT_MORTAL)
	{
		c_put_str(TERM_L_RED, "Mortal wound", ROW_CUT, COL_CUT);
	}
	else if (c > PY_CUT_DEEP)
	{
		c_put_str(TERM_RED, "Deep gash   ", ROW_CUT, COL_CUT);
	}
	else if (c > PY_CUT_SEVERE)
	{
		c_put_str(TERM_RED, "Severe cut  ", ROW_CUT, COL_CUT);
	}
	else if (c > PY_CUT_NASTY)
	{
		c_put_str(TERM_ORANGE, "Nasty cut   ", ROW_CUT, COL_CUT);
	}
	else if (c > PY_CUT_BAD)
	{
		c_put_str(TERM_ORANGE, "Bad cut     ", ROW_CUT, COL_CUT);
	}
	else if (c > PY_CUT_LIGHT)
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

	if (s > PY_STUN_KO)
	{
		c_put_str(TERM_RED, "Knocked out ", ROW_STUN, COL_STUN);
	}
	else if (s > PY_STUN_HEAVY)
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
 *
 * Copied from Oangband
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
	else if (!mon_list[p_ptr->health_who].ml)
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
	else if (!mon_list[p_ptr->health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
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

		/* calmed */
		if (m_ptr->calmed) attr = TERM_SLATE;

		/* Asleep */
		if (m_ptr->sleep) attr = TERM_BLUE;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (handle monster conditions) */
		if (m_ptr->confused) 
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "cccccccccc");
		else if (m_ptr->stunned) 
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "ssssssssss");
		else if (m_ptr->blinded) 
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "bbbbbbbbbb");
		else if (m_ptr->poisoned) 
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "pppppppppp");
		else if (m_ptr->bleeding) 
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "BBBBBBBBBB");
		else
			Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
	}
}

static void prt_equippy(void)
{
	display_player_equippy(ROW_EQUIPPY, COL_EQUIPPY);
}

/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;
	cptr s;

	/* Race and Class */
	if (!rp_ptr->special) s=p_name + rp_ptr->name;
		else s=rsp_ptr[(p_ptr->max_lev)/5]->name;
	prt_field(s, ROW_RACE, COL_RACE);
	prt_field(c_name + cp_ptr->name, ROW_CLASS, COL_CLASS);

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

	/* Spellpoints */
	prt_sp();

	/* Gold */
	prt_gold();

	/* Current depth */
	prt_depth();

	/* Equippy chars */
	prt_equippy(); 

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
	prt_diseased();

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
 * Track a new monster
 */
void health_track(int m_idx)
{
	/* Track a new guy */
	p_ptr->health_who = m_idx;

	/* Redraw (later) */
	p_ptr->redraw |= (PR_HEALTH);
}

/*
 * Hack -- track the given monster race
 */
void monster_track(int r_idx, int u_idx)
{
	/* Save this monster ID */
	term_mon_race_idx = r_idx;
	term_mon_unique_idx = u_idx;

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);
}

/*
 * Hack -- track the given object kind
 */
void artifact_track(int a_idx)
{
	object_type *o_ptr = &term_object;

	/* Make fake artifact */
	make_fake_artifact(o_ptr, a_idx);

	term_obj_real = FALSE;
	
	/* Window stuff */
	p_ptr->window |= (PW_OBJECT);
}

/*
 * Hack -- track the given object kind
 */
void object_kind_track(int k_idx)
{
	object_type *o_ptr = &term_object;

	/* Prepare the object */
	object_wipe(o_ptr);
	object_prep(o_ptr, k_idx);

	term_obj_real = FALSE;
	object_known(o_ptr);
	
	/* Window stuff */
	p_ptr->window |= (PW_OBJECT);
}

/*
 * Hack -- track the given object kind
 */
void object_actual_track(const object_type *j_ptr)
{
	object_type *o_ptr = &term_object;

	/* Prepare the object */
	object_wipe(o_ptr);
	object_copy(o_ptr, j_ptr);

	term_obj_real = TRUE;

	/* Window stuff */
	p_ptr->window |= (PW_OBJECT);
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
		display_player(CSCREEN_MAIN);

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
		display_player(CSCREEN_RESISTS);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Hack -- display player in sub-windows (mode 1)
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
		if (!(op_ptr->window_flag[j] & (PW_CONDITION))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display flags */
		display_player_status();

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
		if (term_mon_race_idx) display_roff(term_mon_race_idx, term_mon_unique_idx);

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
	object_type *o_ptr = &term_object;

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

		/* Display object info */
		display_koff(o_ptr);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Hack -- display room recall in sub-windows
 */
static void fix_room_info(void)
{
	int by = p_ptr->py / BLOCK_HGT;
	int bx = p_ptr->px / BLOCK_WID;
	int room = dun_room[by][bx];

	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_ROOM_INFO))) continue;	

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display room info */
		display_room_info(room);

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
	for (j = 0; j < 8; j++)
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
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
	int h, i, j, k, levels;
	int max_available, num_allowed, num_known;
	int stat_factor;

	magic_type *s_ptr;

	/* Hack -- must be literate */
	if (!literate()) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	levels=0;

	/* Determine the number of spells allowed - according to the lowest possible handicap */
	for (h = 0; h < SV_BOOK_MAX; h++)
	{
		/* Skip books we can't use */
		if (!cp_ptr->spell_book[h]) continue;

		if ((p_ptr->lev - cp_ptr->spell_handicap[h] + 1)>levels) levels=(p_ptr->lev - cp_ptr->spell_handicap[h] + 1);
	}

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Extract total allowed spells - Hack - Mystics get spells faster */
	stat_factor = (p_stat(cp_ptr->spell_stat1) + p_stat(cp_ptr->spell_stat2))/2;
	num_allowed = ((adj_mag_study[stat_factor] * 
		levels) / ((cp_ptr->flags & CF_EXTRA_SPELL) ? 33: 50));
		
	max_available = 0;

	for (h = 0; h < SV_BOOK_MAX; h++)
	{
		if (cp_ptr->spell_book[h]) max_available += count_spells(h);
	}

	/* Boundary control. -LM- */
	if (num_allowed > max_available) num_allowed = max_available;

	/* Assume none known */
	num_known = 0;

	for (h = 0; h < SV_BOOK_MAX; h++)
	{ 

		/* Skip books we can't use */
		if (!cp_ptr->spell_book[h]) continue;

		/* Count the number of spells we know */
		for (j = 0; j < MAX_BOOK_SPELLS; j++)
		{
			/* Count known spells */
			if (p_ptr->spell_learned[h] & (1L << j)) num_known++;
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;

	/* Forget spells that are too hard */
	for (i = (SV_BOOK_MAX * MAX_BOOK_SPELLS) - 1; i >=0 ; i--)
	{ 
		/* Access the book */
		h = p_ptr->spell_order[i][0];

		/* Efficiency -- all done */
		if (!p_ptr->spell_learned[h]) continue;

		/* Skip books we can't use */
		if (!cp_ptr->spell_book[h]) continue;

		/* Access the spell */
		j = p_ptr->spell_order[i][1];

		/* Skip non-spells */
		if (j >= 99) continue;

		/* Get the spell */
		s_ptr = &books[h].contents[j];

		/* Skip non-spells */
		if (s_ptr->index == 0) continue; 

		/* Skip spells we are allowed to know */
		if ((s_ptr->slevel+(cp_ptr->spell_handicap[h]-1)) <= p_ptr->lev) continue;

		/* Is it known? */
		if (p_ptr->spell_learned[h] & (1L << j))
		{
			/* Mark as forgotten */
			p_ptr->spell_forgotten[h] |= (1L << j);

			/* No longer known */
			p_ptr->spell_learned[h] &= ~(1L << j);

			/* Message */
			message_format(MSG_EFFECT, 0, "You have forgotten the spell of %s.", s_ptr->sname);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}

	/* Forget spells if we know too many spells */
	for (i = (SV_BOOK_MAX * MAX_BOOK_SPELLS) - 1; i >=0 ; i--)
	{ 
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Access the book */
		h = p_ptr->spell_order[i][0];

		/* Efficiency -- all done */
		if (!p_ptr->spell_learned[h]) continue;

		/* Skip books we can't use */
		if (!cp_ptr->spell_book[h]) continue;

		/* Access the spell */
		j = p_ptr->spell_order[i][1];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Get the spell */
		s_ptr = &books[h].contents[j];

		/* Skip non-spells */
		if (s_ptr->index == 0) continue; 

		/* Forget it (if learned) */
		if (p_ptr->spell_learned[h] & (1L << j))
		{
			/* Mark as forgotten */
			p_ptr->spell_forgotten[h] |= (1L << j);

			/* No longer known */
			p_ptr->spell_learned[h] &= ~(1L << j);

			/* Message */
			message_format(MSG_EFFECT, 0, "You have forgotten the spell of %s.", s_ptr->sname);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}

	/* Check for spells to remember */
	for (i = 0; i < (SV_BOOK_MAX * MAX_BOOK_SPELLS) ; i++)
	{ 
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Access the book */
		h = p_ptr->spell_order[i][0];

		/* Efficiency -- all done */
		if (!p_ptr->spell_forgotten[h]) continue;

		/* Skip books we can't use */
		if (!cp_ptr->spell_book[h]) continue;

		/* Access the spell */
		j = p_ptr->spell_order[i][1];

		/* Skip unknown spells */
		if (j >= 99) continue;
		
		/* Get the spell */
		s_ptr = &books[h].contents[j];

		/* Skip non-spells */
		if (s_ptr->index == 0) continue; 

		/* Skip spells we cannot remember */
		if ((s_ptr->slevel+(cp_ptr->spell_handicap[h]-1)) > p_ptr->lev) continue;

		/* Remember it (if forgotten) */
		if (p_ptr->spell_forgotten[h] & (1L << j))
		{
			/* No longer forgotten */
			p_ptr->spell_forgotten[h] &= ~(1L << j);

			/* Known once more */
			p_ptr->spell_learned[h] |= (1L << j);

			/* Message */
			message_format(MSG_EFFECT, 0, "You have remembered the spell of %s.", s_ptr->sname);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}

	/* Assume no spells available */
	k = 0;

	/* Count available spells */
	for (h = 0; h < SV_BOOK_MAX; h++)
	{ 

		/* Skip books we can't use */
		if (!cp_ptr->spell_book[h]) continue;

		/* Check for spells to remember */
		for (i = 0; i < MAX_BOOK_SPELLS; i++)
		{
			/* Get the spell */
			s_ptr = &books[h].contents[i];

			/* Skip non-spells */
			if (s_ptr->index == 0) continue; 

			/* Skip spells we cannot remember */
			if ((s_ptr->slevel+(cp_ptr->spell_handicap[h]-1)) > p_ptr->lev) continue;

			/* Skip spells we already know */
			if (p_ptr->spell_learned[h] & (1L << i)) continue;
		
			/* Count it */
			k++;
		}
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

	/* Spell count changed */
	if (p_ptr->old_spells != p_ptr->new_spells)
	{
		/* Save the new_spells value */
		p_ptr->old_spells = p_ptr->new_spells;

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
}

/*
 * Calculate maximum mana.  You do not need to know any spells.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
	int h, msp, levels, bonus;
	int stat_factor;

	/* Hack -- Must be literate */
	if (!spellcaster()) return;

	levels=0;

	/* Extract "effective" player level */
	if (cp_ptr->flags & CF_MUSIC) levels = p_ptr->lev;
	else for (h = 0; h < SV_BOOK_MAX; h++)
	{
		/* Skip books we can't use */
		if (!cp_ptr->spell_book[h]) continue;

		if ((p_ptr->lev - cp_ptr->spell_handicap[h] + 1)>levels) 
			levels = (p_ptr->lev - cp_ptr->spell_handicap[h] + 1);
	}

	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana */
	stat_factor = (p_stat(cp_ptr->spell_stat1) + p_stat(cp_ptr->spell_stat2))/2;
	if (cp_ptr->flags & CF_EXTRA_MANA) 
		msp = (adj_mag_mana[stat_factor] + adj_mag_extra_mana[stat_factor]) * levels / 25;
	else msp = adj_mag_mana[stat_factor] * levels / 25;
	
	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Mana bonuses */
	if (p_ptr->mana_add)
	{
		/* Each point of bonus = 5% of hitpoint (note - redefinition of bonus) */
		bonus = (p_ptr->mana_add * msp * 5) / 100;

		if (p_ptr->mana_add > 0)
		{
			/* If the bonus is too low, increase */
			if (bonus < (p_ptr->mana_add * 10)) bonus = p_ptr->mana_add * 10;
		}
		else
		{
			/* If the penalty is too low, increase */
			if (bonus > (p_ptr->mana_add * 10)) bonus = p_ptr->mana_add * 10;
		}

		msp += bonus;
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
}

/*
 * Calculate the players (maximal) hit points
 *
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
	int bonus, hp_lev, mhp;
	int bonus_sum = 0;
	int i;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_con_mhp[p_stat(A_CON)]) - 128);

	/* Make sure that you never get negative hp for any level */
	if (bonus >= -1) bonus_sum = bonus * p_ptr->lev;
	else for (i = 0; i < p_ptr->lev; i++)
	{
		if (i > 0) hp_lev = p_ptr->player_hp[i] - p_ptr->player_hp[i-1];
		else hp_lev = p_ptr->player_hp[0];

		if (bonus + hp_lev < 0) bonus_sum -= hp_lev;
		else bonus_sum += bonus;
	}

	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->lev-1] + (bonus_sum / 2);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

	/* HP bonuses */
	if (p_ptr->hp_add)
	{
		/* Each point of bonus = 5% of hitpoint (note - redefinition of bonus) */
		bonus = (p_ptr->hp_add * mhp * 5) / 100;

		if (p_ptr->hp_add > 0)
		{
			/* If the bonus is too low, increase */
			if (bonus < (p_ptr->hp_add * 10)) bonus = p_ptr->hp_add * 10;
		}
		else
		{
			/* If the penalty is too low, increase */
			if (bonus > (p_ptr->hp_add * 10)) bonus = p_ptr->hp_add * 10;
		}

		mhp += bonus;
	}

	/* Make sure you have at least one hp */
	if (mhp < 1) mhp = 1;

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
	u32b f1, f2, f3;

	object_type *o_ptr = &inventory[INVEN_LITE];

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Examine Refueling lites */
	if ((o_ptr->tval == TV_LITE_SPECIAL) || ((o_ptr->tval == TV_LITE) && (o_ptr->timeout > 0)))
	{
		if (f3 & (TR3_LITE4)) p_ptr->cur_lite = 4;
		else if (f3 & (TR3_LITE3)) p_ptr->cur_lite = 3;
		else if (f3 & (TR3_LITE2)) p_ptr->cur_lite = 2;
		else if (f3 & (TR3_LITE1)) p_ptr->cur_lite = 1;
		else p_ptr->cur_lite = 1; /* Hack - paranoia */
	}

	/* Reduce light gradually when running out of fuel */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->timeout < 300) && (o_ptr->timeout > 0))
	{
		p_ptr->cur_lite = ((p_ptr->cur_lite * o_ptr->timeout) / 300) + 1;
	}

	/* Reduce lite when running if requested */
	if (p_ptr->running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
	}

	/* Player is glowing */
	if (p_ptr->lite) p_ptr->cur_lite += 1;

	/* Never more than 4 radius */
	if (p_ptr->cur_lite > 4) p_ptr->cur_lite = 4;

	if (view_monster_lite)
    {
	    /* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW);
		p_ptr->update |= (PU_MONSTERS);
	}
    else
	{
		/* Notice changes in the "lite radius" */
		if (p_ptr->old_lite != p_ptr->cur_lite)
		{
			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Remember the old lite */
			p_ptr->old_lite = p_ptr->cur_lite;
		}
	}
}

/*
 * Calculates the amount of blows achieved with a certain (normally the current) weapon.
 */
byte calc_blows(const object_type *o_ptr, bool full)
{
	int i;
	int wgt_val = 0;

	int blows;
	u32b f1, f2, f3;

	object_type *i_ptr;

	/* Heavy weapons */
	if (adj_str_hold[p_stat(A_STR)] < object_weight(o_ptr) / 10)
	{
		return 1;
	}

	/* Enforce a minimum "weight" (tenth pounds) */
	if (o_ptr->k_idx) 
	{
		wgt_val = (object_weight(o_ptr) / 10);

		if (wgt_val > 29) wgt_val = 29;

		/* Hack - Boost digging skill by weapon weight */
		p_ptr->skill[SK_DIG] += (object_weight(o_ptr) / 10);

		/* Hack - Now modify the value according to class */
		if (cp_ptr->flags & CF_BETTER_BLOWS) wgt_val--;
		if (cp_ptr->flags & CF_WORSE_BLOWS) wgt_val++;

		if (wgt_val < 0) wgt_val = 0;
		if (wgt_val > 29) wgt_val = 29;
	}

	/* Calculate blows */
	blows = adj_dex_blows[p_stat(A_DEX)];

	/* Cap according to weapon weight */
	if (blows > weapon_wgt_blows[wgt_val]) blows = weapon_wgt_blows[wgt_val];

	/* Mega hack - calculate extra blows */
	if (full) object_flags(o_ptr, &f1, &f2, &f3);
	else object_flags_known(o_ptr, &f1, &f2, &f3);

	/* Affect blows */
	if (f1 & (TR1_BLOWS)) blows += o_ptr->pval;

	/* Scan the equipment */
	for (i = INVEN_BOW; i < INVEN_TOTAL; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Mega hack - calculate extra blows */
		if (full) object_flags(i_ptr, &f1, &f2, &f3);
		else object_flags_known(i_ptr, &f1, &f2, &f3);

		/* Affect blows */
		if (f1 & (TR1_BLOWS)) blows += i_ptr->pval;
	}

	/* If enraged, provide an extra blow */
	if (p_ptr->rage) blows++;

	/* Require at least one blow (Check again in case of negative extra blows) */
	if (blows < 1) blows = 1;

	return (byte)blows;
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
	int res, cur_cap;
	int dis_res, dis_cap;

	int old_speed;

	int old_telepathy;
	int old_see_inv;
	int old_invis;

	int old_dis_ac;
	int old_dis_to_a;

	int extra_shots;
	int extra_might;

	int old_stat_top[A_MAX];
	int old_stat_use[A_MAX];

	int old_mana_add, old_hp_add;

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

	/* Save the old health bonus */
	old_hp_add = p_ptr->hp_add;
	old_mana_add = p_ptr->mana_add;

	/* Save the old stats */
	for (i = 0; i < A_MAX; i++)
	{
		old_stat_top[i] = p_ptr->stat_top[i];
		old_stat_use[i] = p_ptr->stat_use[i];
	}

	/* Save some other stuff */
	old_invis = p_ptr->invis;
	
	/*** Reset ***/

	/* Reset player speed */
	p_ptr->pspeed = 110;

	/* Reset "blow" info */
	p_ptr->num_blow = 1;

	/* Reset "fire" info */
	p_ptr->num_fire = 0;
	extra_shots = 0;
	extra_might = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] = 0;
	p_ptr->mana_add = 0;
	p_ptr->hp_add = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Spell duration bonus */
	p_ptr->sp_dur = 0;
	p_ptr->sp_dam = 0;
	p_ptr->sp_inf = 0;

	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->taint_inv = FALSE;
	p_ptr->item_drain = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->disrupt = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->invis = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->lite = FALSE;
	p_ptr->bravery = FALSE;
	p_ptr->no_blind = FALSE;
	p_ptr->luck = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_chr = FALSE;

	p_ptr->tim_flag1 = 0L;
	p_ptr->tim_flag2 = 0L;
	p_ptr->tim_flag3 = 0L;

	/*** Extract race/class info ***/

	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra;

	/* Extract skills */
	for (i = 0 ; i < SK_MAX ; i++)
	{
		p_ptr->skill[i] = rp_ptr->r_skill[i] + cp_ptr->c_skill[i];
		if (rp_ptr->special) p_ptr->skill[i] += rsp_ptr[(p_ptr->max_lev) / 5]->r_skill[i];
	}

	/* Extract resistances */
	for (i = 0 ; i < RS_MAX ; i++)
	{
		/* Race can increase caps */
		cur_cap = (rp_ptr->res[i] > resist_caps[i].normal) ? 
			rp_ptr->res[i] : resist_caps[i].normal;

		/* Base resistance = class + race */
		p_ptr->res[i] = ((rp_ptr->res[i] + cp_ptr->res[i]) * p_ptr->lev) / 50;

		/* Boundary check */
		if (p_ptr->res[i] > cur_cap) p_ptr->res[i] = cur_cap;

		/* Display */
		p_ptr->dis_res[i] = p_ptr->res[i];
		dis_cap = cur_cap;
	}

	/*** Analyze player ***/

	/* Extract the player flags */
	player_flags(&f1, &f2, &f3);

	/* Good flags */
	if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
	if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
	if (f3 & (TR3_GLOW)) p_ptr->lite = TRUE;
	if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
	if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
	if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
	if (f3 & (TR3_INVIS)) p_ptr->invis = TRUE;
	if (f3 & (TR3_LUCK)) p_ptr->luck = TRUE;

	/* "semi-immunities" */
	if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
	if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;
	if (f2 & (TR2_BRAVERY)) p_ptr->bravery = TRUE;
	if (f2 & (TR2_NO_BLIND)) p_ptr->no_blind = TRUE;
	if (f2 & (TR2_NO_POISON)) p_ptr->no_poison = TRUE;
	if (f2 & (TR2_NO_DISEASE)) p_ptr->no_disease = TRUE;
	if (f2 & (TR2_NO_STUN)) p_ptr->no_stun = TRUE;
	if (f2 & (TR2_NO_CUT)) p_ptr->no_cut = TRUE;
	if (f2 & (TR2_NO_CONF)) p_ptr->no_confuse = TRUE;

	/* Weird flags */
	if (f2 & (TR2_BLESSED)) p_ptr->bless_blade = TRUE;

	/* Bad flags */
	if (f3 & (TR3_DISRUPT)) p_ptr->disrupt = TRUE;
	if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
	if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
	if (f3 & (TR3_TAINT)) p_ptr->taint_inv = TRUE;
	if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
	if (f3 & (TR3_DRAIN_ITEM)) p_ptr->item_drain = TRUE;

	/* Sustain flags */
	if (f1 & (TR1_SUST_STR)) p_ptr->sustain_str = TRUE;
	if (f1 & (TR1_SUST_INT)) p_ptr->sustain_int = TRUE;
	if (f1 & (TR1_SUST_WIS)) p_ptr->sustain_wis = TRUE;
	if (f1 & (TR1_SUST_DEX)) p_ptr->sustain_dex = TRUE;
	if (f1 & (TR1_SUST_CON)) p_ptr->sustain_con = TRUE;
	if (f1 & (TR1_SUST_CHR)) p_ptr->sustain_chr = TRUE;

	/*** Analyze equipment ***/

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack - skip empty lanterns */
		if ((o_ptr->tval == TV_LITE) && (!o_ptr->timeout)) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stats */
		if (f1 & (TR1_STR)) p_ptr->stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT)) p_ptr->stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS)) p_ptr->stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX)) p_ptr->stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON)) p_ptr->stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_CHR)) p_ptr->stat_add[A_CHR] += o_ptr->pval;

		/* Affect mana and health */
		if (f1 & (TR1_MANA)) p_ptr->mana_add += o_ptr->pval;
		if (f1 & (TR1_HEALTH)) p_ptr->hp_add += o_ptr->pval;

		/* Affect stealth */
		if (f1 & (TR1_STEALTH)) p_ptr->skill[SK_STL] += o_ptr->pval;

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_PERCEPTION)) p_ptr->skill[SK_PER] += (o_ptr->pval * 5);

		/* Affect infravision */
		if (f1 & (TR1_INFRA)) p_ptr->see_infra += o_ptr->pval;

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL)) p_ptr->skill[SK_DIG] += (o_ptr->pval * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED)) p_ptr->pspeed += o_ptr->pval;

		/* Affect shots */
		if (f1 & (TR1_SHOTS)) extra_shots += o_ptr->pval;

		/* Affect Might */
		if (f1 & (TR1_MIGHT)) extra_might += o_ptr->pval;

		/* Affect Duration */
		if (f1 & (TR1_SP_DUR)) p_ptr->sp_dur += o_ptr->pval;
		if (f1 & (TR1_SP_DAM)) p_ptr->sp_dam += o_ptr->pval;
		if (f1 & (TR1_SP_INF)) p_ptr->sp_inf += o_ptr->pval;

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
		if (f3 & (TR3_GLOW)) p_ptr->lite = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
		if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
		if (f3 & (TR3_INVIS)) p_ptr->invis = TRUE;
		if (f3 & (TR3_LUCK)) p_ptr->luck = TRUE;

		/* "semi-immunities" */
		if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
		if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;
		if (f2 & (TR2_BRAVERY)) p_ptr->bravery = TRUE;
		if (f2 & (TR2_NO_BLIND)) p_ptr->no_blind = TRUE;
		if (f2 & (TR2_NO_POISON)) p_ptr->no_poison = TRUE;
		if (f2 & (TR2_NO_DISEASE)) p_ptr->no_disease = TRUE;
		if (f2 & (TR2_NO_STUN)) p_ptr->no_stun = TRUE;
		if (f2 & (TR2_NO_CUT)) p_ptr->no_cut = TRUE;
		if (f2 & (TR2_NO_CONF)) p_ptr->no_confuse = TRUE;

		/* Weird flags */
		if (f2 & (TR2_BLESSED)) p_ptr->bless_blade = TRUE;

		/* Bad flags */
		if (f3 & (TR3_DISRUPT)) p_ptr->disrupt = TRUE;
		if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
		if (f3 & (TR3_TAINT)) p_ptr->taint_inv = TRUE;
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_DRAIN_ITEM)) p_ptr->item_drain = TRUE;

		/* Sustain flags */
		if (f1 & (TR1_SUST_STR)) p_ptr->sustain_str = TRUE;
		if (f1 & (TR1_SUST_INT)) p_ptr->sustain_int = TRUE;
		if (f1 & (TR1_SUST_WIS)) p_ptr->sustain_wis = TRUE;
		if (f1 & (TR1_SUST_DEX)) p_ptr->sustain_dex = TRUE;
		if (f1 & (TR1_SUST_CON)) p_ptr->sustain_con = TRUE;
		if (f1 & (TR1_SUST_CHR)) p_ptr->sustain_chr = TRUE;

		/* Modify the base armor class */
		p_ptr->ac += object_ac(o_ptr);

		/* The base armor class is always known */
		p_ptr->dis_ac += object_ac(o_ptr);

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Extract resistances */
		for (j = 0 ; j < RS_MAX ; j++)
		{
			res = object_resist(o_ptr, j);
			dis_res = object_resist_known(o_ptr, j);

			/* Set current cap */
			cur_cap = (p_ptr->res[j] > resist_caps[j].normal) ?
				p_ptr->res[j] : resist_caps[j].normal;
			dis_cap = (p_ptr->dis_res[j] > resist_caps[j].normal) ?
				p_ptr->dis_res[j] : resist_caps[j].normal;

			/* Base resistance = class + race */
			p_ptr->res[j] += res;

			/* Display */
			p_ptr->dis_res[j] += dis_res;

			if (res > cur_cap) cur_cap = res;
			if (dis_res > dis_cap) dis_cap = dis_res;

			/* Boundary check */
			if (p_ptr->res[j] > cur_cap) p_ptr->res[j] = cur_cap;
			if (p_ptr->dis_res[j] > dis_cap) p_ptr->dis_res[j] = dis_cap;
		}

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h += object_to_h(o_ptr);

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_h += object_to_h(o_ptr);
	}

	/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int add, top, use;

		/* Extract modifier */
		add = p_ptr->stat_add[i];

		/* Modify the stats for races */
		add += (rp_ptr->r_adj[i]);

		if (rp_ptr->special) add += rsp_ptr[(p_ptr->max_lev)/5]->r_adj[i];

		/* Extract the new "stat_top" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], add);

		/* Save the new value */
		p_ptr->stat_top[i] = top;

		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], add);

		/* Save the new value */
		p_ptr->stat_use[i] = use;
	}

	/*** Temporary flags ***/

	/* Temporary resistances */
	for (i = 0 ; i < RS_MAX ; i++)
	{
		/* Temporary resistance, add 33% */
		if (p_ptr->tim_res[i]) 
		{
			/* If already higher or equal to cap, ignore */
			if (p_ptr->res[i] < resist_caps[i].temp)
			{
				p_ptr->res[i] += TEMP_RES_BONUS;
				if (p_ptr->res[i] > resist_caps[i].temp) 
					p_ptr->res[i] = resist_caps[i].temp;
			}

			/* If already higher or equal to cap, ignore */
			if (p_ptr->dis_res[i] < resist_caps[i].temp)
			{
				p_ptr->dis_res[i] += TEMP_RES_BONUS;
				if (p_ptr->dis_res[i] > resist_caps[i].temp) 
					p_ptr->dis_res[i] = resist_caps[i].temp;
			}
		}
	}

	/* Apply temporary "stun" */
	if (p_ptr->stun > PY_STUN_HEAVY)
	{
		p_ptr->to_h -= 15;
		p_ptr->dis_to_h -= 15;
	}
	else if (p_ptr->stun)
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
	}

	/* Resilience */
	if (p_ptr->resilient)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 4;
		p_ptr->dis_to_h += 4;
	}

	/* Temprory shield */
	if (p_ptr->shield)
	{
		if (p_ptr->to_a < 10) p_ptr->to_a = 25;
		else p_ptr->to_a += 15;

		if (p_ptr->dis_to_a < 10) p_ptr->dis_to_a = 25;
		else p_ptr->dis_to_a += 15;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 8;
		p_ptr->dis_to_h += 8;
	}

	/* Temporary stability */
	if (p_ptr->stability)
	{
		p_ptr->no_stun = TRUE;
		p_ptr->no_confuse = TRUE;
	}

	/* Termporary bravery */
	if (p_ptr->tim_bravery)
	{
		p_ptr->bravery = TRUE;
		p_ptr->tim_flag2 |= TR2_BRAVERY;
	}

	/* Temporary "Beserk" */
	if (p_ptr->rage)
	{
		/* 
		 * Berserk also provides an extra blow, a penalty to magic item use,
		 * a penalty to saving throws, and a penalty to stealth - see below 
		 */
		p_ptr->to_h += 8;
		p_ptr->dis_to_h += 8;
		p_ptr->to_a -= 20;
		p_ptr->dis_to_a -= 20;
		p_ptr->disrupt = TRUE;
		p_ptr->tim_flag3 |= TR3_DISRUPT;
	}

	/* Temporary "fast" */
	if (p_ptr->fast)
	{
		p_ptr->pspeed += 10;
		p_ptr->tim_flag1 |= TR1_SPEED;
	}

	/* Temporary "slow" */
	if (p_ptr->slow)
	{
		p_ptr->pspeed -= 10;
		p_ptr->tim_flag1 |= TR1_SPEED;
	}

	/* Temporary see invisible */
	if (p_ptr->tim_see_invis)
	{
		p_ptr->see_inv = TRUE;
		p_ptr->tim_flag3 |= TR3_SEE_INVIS;
	}

	/* Temporary invisibility */
	if (p_ptr->tim_invis)
	{
		if (p_ptr->tim_invis > 0)
		{
			p_ptr->invis = TRUE;
			p_ptr->tim_flag3 |= TR3_INVIS;
		}
		else
		{
			/* Hack - temporary nullified invisiblity */
			p_ptr->invis = FALSE;
		}
	}

	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra += 3;
		p_ptr->tim_flag1 |= TR1_INFRA;
	}

	/* Temporary stealth boost */
	if (p_ptr->tim_stealth)
	{
		p_ptr->tim_flag1 |= TR1_STEALTH;
		/* Actual stealth affect appears later */
	}

	/* Temporary spell duration boost */
	if (p_ptr->tim_sp_dur)
	{
		p_ptr->tim_flag1 |= TR1_SP_DUR;
		p_ptr->sp_dur += 6;
	}

	/* Temporary spell damage boost */
	if (p_ptr->tim_sp_dam)
	{
		p_ptr->tim_flag1 |= TR1_SP_DAM;
		p_ptr->sp_dam += 3;
	}

	/* Temporary spell damage boost */
	if (p_ptr->tim_sp_inf)
	{
		p_ptr->tim_flag1 |= TR1_SP_INF;
		p_ptr->sp_inf += 3;
	}

	/* Temporary taint */
	if (p_ptr->taint)
	{
		p_ptr->tim_flag3 |= TR3_TAINT;
	}

	/* Fear */
	if (p_ptr->afraid)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		/* 
		 * Minuses to hit - they are only relevent if the fear is very low (wary)
		 * but are retained so that the display won't be weird
		 */
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
	}

	/*** Analyze weight ***/

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = adj_str_wgt[p_stat(A_STR)] * 100;

	/* Apply "encumbrance" from weight */
	if (j > i / 2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

	/* Sanity check on extreme speeds */
	if (p_ptr->pspeed < 0) p_ptr->pspeed = 0;
	if (p_ptr->pspeed > 199) p_ptr->pspeed = 199;

	/*** Apply modifier bonuses ***/

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_dex_ta[p_stat(A_DEX)]) - 128);
	p_ptr->to_h += ((int)(adj_dex_th[p_stat(A_DEX)]) - 128);
	p_ptr->to_h += ((int)(adj_str_th[p_stat(A_STR)]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_stat(A_DEX)]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[p_stat(A_DEX)]) - 128);
	p_ptr->dis_to_h += ((int)(adj_str_th[p_stat(A_STR)]) - 128);

	/*** Modify skills ***/

	/* Affect Skill -- stealth (bonus one) */
	p_ptr->skill[SK_STL] += 1;
	if (p_ptr->tim_stealth) p_ptr->skill[SK_STL] += 2;

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skill[SK_DIS] += adj_dex_dis[p_stat(A_DEX)];
	p_ptr->skill[SK_DIS] += adj_int_dis[p_stat(A_INT)];

	/* Affect Skill -- bypass (DEX) */
	p_ptr->skill[SK_BYP] += adj_dex_dis[p_stat(A_DEX)];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skill[SK_DEV] += adj_int_dev[p_stat(A_INT)];

	/* Affect Skill -- aclhemy (INT) */
	p_ptr->skill[SK_ALC] += adj_int_alc[p_stat(A_INT)];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skill[SK_SAV] += adj_wis_sav[p_stat(A_WIS)];

	/* Affect Skill -- digging (STR) */
	p_ptr->skill[SK_DIG] += adj_str_dig[p_stat(A_STR)];

	/* Affect Skill -- by level, class */
	for (i = 0; i < SK_MAX; i++) p_ptr->skill[i] += (cp_ptr->x_skill[i] * p_ptr->lev / 10);

	/* Affect Skill -- Berserk rage */
	if (p_ptr->rage)
	{
		p_ptr->skill[SK_DEV] = (p_ptr->skill[SK_DEV] * 8) / 10;
		p_ptr->skill[SK_SAV] = (p_ptr->skill[SK_SAV] * 8) / 10;
		p_ptr->skill[SK_STL] = (p_ptr->skill[SK_STL] * 8) / 10;
	}

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill[SK_DIG] < 1) p_ptr->skill[SK_DIG] = 1;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill[SK_STL] > 30) p_ptr->skill[SK_STL] = 30;
	if (p_ptr->skill[SK_STL] < 0) p_ptr->skill[SK_STL] = 0;

	/* Apply Skill -- Extract noise from stealth */
	p_ptr->noise = (1L << (30 - p_ptr->skill[SK_STL]));

	/* Obtain the "hold" value */
	hold = adj_str_hold[p_stat(A_STR)];

	/*** Analyze current bow ***/

	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to hold a heavy bow */
	if (hold < object_weight(o_ptr) / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - object_weight(o_ptr) / 10);
		p_ptr->dis_to_h += 2 * (hold - object_weight(o_ptr) / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}

	/* Analyze launcher */
	if (o_ptr->k_idx)
	{
		/* Get to shoot */
		p_ptr->num_fire = 1;

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Hack -- Rangers love Bows */
			if (cp_ptr->flags & CF_EXTRA_SHOT)
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

	/* Real weapon */
	if (o_ptr->k_idx)
	{
		/* Assume not heavy */
		p_ptr->heavy_wield = FALSE;

		/* Damage dice/sides */
		p_ptr->ds = object_ds(o_ptr);
		p_ptr->dd = object_dd(o_ptr);

		/* It is hard to hold a heavy weapon */
		if (hold < object_weight(o_ptr) / 10)
		{
			int penalty = (object_weight(o_ptr) / 10) - hold;

			/* Hard to wield a heavy weapon */
			p_ptr->to_h -= penalty;
			p_ptr->dis_to_h -= penalty;

			p_ptr->ds = (p_ptr->ds * 2) / (penalty + 2);
			p_ptr->dd = (p_ptr->dd * 2) / (penalty + 2);

			if (p_ptr->ds < 1) p_ptr->ds = 1;
			if (p_ptr->dd < 1) p_ptr->dd = 1;

			/* Heavy weapon */
			p_ptr->heavy_wield = TRUE;
		}

		/* Normal weapons or Unarmed combat */
		else 
		{
			/* Use the blows tables */
			p_ptr->num_blow = calc_blows(o_ptr, TRUE);
		}

		/* Assume okay */
		p_ptr->icky_wield = FALSE;

		/* Priest weapon penalty for non-blessed edged weapons */
		if ((cp_ptr->flags & CF_BLESS_WEAPON) && (!p_ptr->bless_blade) &&
			((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
		{
			/* Reduce the real bonuses */
			p_ptr->to_h -= 2;

			/* Reduce the mental bonuses */
			p_ptr->dis_to_h -= 2;

			/* Icky weapon */
			p_ptr->icky_wield = TRUE;
		}
	}
	/* Unarmed combat */
	else
	{
		/* Damage for unarmed combat */
		p_ptr->dd = adj_str_unarmed[p_stat(A_STR)];
		p_ptr->ds = 2;

		/* Use the blows tables (note that o_ptr here is always an empty object) */
		p_ptr->num_blow = calc_blows(o_ptr, TRUE);

		/* No weapon */
		p_ptr->heavy_wield = FALSE;
	}

	/*** Analyze armor ***/

	/* Examine the "current weapon" */
	o_ptr = &inventory[INVEN_BODY];

	p_ptr->cumber_armor_melee =
		((object_weight(o_ptr) / 10 > adj_str_armor[p_stat(A_STR)]) ? TRUE : FALSE);

	/* Take note when "glove state" changes */
	if (p_ptr->old_cumber_armor_melee != p_ptr->cumber_armor_melee)
	{
		/* Message */
		if (p_ptr->cumber_armor_melee)
		{
			message(MSG_EFFECT, 0, "You are not strong enough to fight well with this armor");
		}
		else
		{
			message(MSG_EFFECT, 0, "Your no longer feel constrained in your melee abilities");
		}

		/* Save it */
		p_ptr->old_cumber_armor_melee = p_ptr->cumber_armor_melee;
	}

	/* Reduce to-hit */
	if (p_ptr->cumber_armor_melee)
	{
		p_ptr->to_h -= 1 + ((object_weight(o_ptr) / 10) - adj_str_armor[p_stat(A_STR)]) / 4;
		p_ptr->dis_to_h -= 1 + ((object_weight(o_ptr) / 10) - adj_str_armor[p_stat(A_STR)]) / 4;
	}

	/*** Analyze stuff that disturbs spellcasting ***/

	/* Process gloves for those disturbed by them */
	if (cp_ptr->flags & CF_NO_GLOVE)
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
				!((o_ptr->pval > 0) && ((f1 & (TR1_DEX)) || (f1 & (TR1_MANA)))))
		{
			/* Encumbered */
			p_ptr->cumber_glove = TRUE;
		}

		/* Take note when "glove state" changes */
		if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
		{
			cptr act;

			if (!(cp_ptr->flags & CF_MUSIC)) act = "spellcasting";
			else act = "playing a musical instrument";

			/* Message */
			if (p_ptr->cumber_glove)
			{
				message_format(MSG_EFFECT, 0, "Your covered hands feel unsuitable for %s.", act);
			}
			else
			{
				message_format(MSG_EFFECT, 0, "Your hands feel more suitable for %s.",act);
			}

			/* Save it */
			p_ptr->old_cumber_glove = p_ptr->cumber_glove;
		}
	}

	if (cp_ptr->spell_weight)
	{
		int i;
		int cur_wgt;

		/* Assume player not encumbered by armor */
		p_ptr->cumber_armor_cast = 0;

		/* Weigh the armor */
		cur_wgt = 0;
		for (i = INVEN_BODY; i <= INVEN_FEET; i++)
		{
			object_type *o_ptr = &inventory[i];

			cur_wgt += object_weight(o_ptr);
		}

		/* Heavy armor penalty */
		if ((cur_wgt - cp_ptr->spell_weight) > 0)
			p_ptr->cumber_armor_cast = cur_wgt - cp_ptr->spell_weight;

		/* Take note when "armor state" changes */
		if (p_ptr->old_cumber_armor_cast != p_ptr->cumber_armor_cast)
		{
			/* Message */
			if (p_ptr->old_cumber_armor_cast == 0)
			{
				message(MSG_EFFECT, 0, "The weight of your armor encumbers your movement.");
			}
			else if (p_ptr->cumber_armor_cast > p_ptr->old_cumber_armor_cast)
			{
				message(MSG_EFFECT, 0, "The weight of your armor encumbers you further.");
			}
			else if (p_ptr->cumber_armor_cast == 0)
			{
				message(MSG_EFFECT, 0, "You feel able to move completely freely.");
			}
			else
			{
				message(MSG_EFFECT, 0, "You feel able to move more freely.");
			}

			/* Save it */
			p_ptr->old_cumber_armor_cast = p_ptr->cumber_armor_cast;
		}
	}

	/*** Chances to avoid effects ***/

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

			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in stats may affect Mana/Spells */
			if ((i == cp_ptr->spell_stat1) || (i == cp_ptr->spell_stat2))
			{
				p_ptr->update |= (PU_MANA | PU_SPELLS);
			}
		}
	}

	/* HP bonuses */
	if (p_ptr->hp_add != old_hp_add)
	{
		p_ptr->update |= (PU_HP);
	}

	/* Mana bonuses */
	if (p_ptr->mana_add != old_mana_add)
	{
		p_ptr->update |= (PU_MANA);
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

	/* --TM-- Invisibility Change */
	if (p_ptr->invis != old_invis)
	{
		if (p_ptr->invis)
		{
			message(MSG_EFFECT, 0, "You fade from sight...");
		}
		else
		{
			message(MSG_EFFECT, 0, "You become visible again.");
		}

		/* Window stuff */
		p_ptr->window |= (PW_CONDITION);
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
			message(MSG_EFFECT, 0, "You have trouble wielding such a heavy bow.");
		}
		else if (inventory[INVEN_BOW].k_idx)
		{
			message(MSG_EFFECT, 0, "You have no trouble wielding your bow.");
		}
		else
		{
			message(MSG_EFFECT, 0, "You feel relieved to put down your heavy bow.");
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
			message(MSG_EFFECT, 0, "You have trouble wielding such a heavy weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			message(MSG_EFFECT, 0, "You have no trouble wielding your weapon.");
		}
		else
		{
			message(MSG_EFFECT, 0, "You feel relieved to put down your heavy weapon.");
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
			message(MSG_EFFECT, 0, "You do not feel comfortable with your weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			message(MSG_EFFECT, 0, "You feel comfortable with your weapon.");
		}
		else
		{
			message(MSG_EFFECT, 0, "You feel more comfortable after removing your weapon.");
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

	/* Squelch items */
	if (p_ptr->notice & (PN_SQUELCH))
	{
		p_ptr->notice &= ~(PN_SQUELCH);
		if (auto_squelch) destroy_squelched_items();
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

	if (p_ptr->update & (PU_ROOM_INFO))
	{
		p_ptr->update &= ~(PU_ROOM_INFO);
		describe_room(FALSE);
	}
}

/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
	cptr s;
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
		p_ptr->redraw &= ~(PR_EQUIPPY);
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		prt_frame_basic();
	}

	if (p_ptr->redraw & (PR_EQUIPPY))
	{
		p_ptr->redraw &= ~(PR_EQUIPPY);
		prt_equippy(); /* To draw / delete equippy chars */
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		if (!rp_ptr->special) s=p_name + rp_ptr->name;
			else s=rsp_ptr[(p_ptr->max_lev)/5]->name;
		prt_field(s, ROW_RACE, COL_RACE);
		prt_field(c_name + cp_ptr->name, ROW_CLASS, COL_CLASS);
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
		p_ptr->redraw &= ~(PR_HUNGER | PR_DISEASED);
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

	if (p_ptr->redraw & (PR_DISEASED))
	{
		p_ptr->redraw &= ~(PR_DISEASED);
		prt_diseased();
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

	/* Display player (mode 1) */
	if (p_ptr->window & (PW_CONDITION))
	{
		p_ptr->window &= ~(PW_CONDITION);
		fix_status();
	}

	/* Display monster list */
	if (p_ptr->window & (PW_VISIBLE))
	{
		p_ptr->window &= ~(PW_VISIBLE);
		fix_visible();
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

	/* Display object recall */
	if (p_ptr->window & (PW_OBJECT))
	{
		p_ptr->window &= ~(PW_OBJECT);
		fix_object();
	}

	/* Display room info */
	if (p_ptr->window & (PW_ROOM_INFO))
	{
		p_ptr->window &= ~(PW_ROOM_INFO);
		fix_room_info();
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
