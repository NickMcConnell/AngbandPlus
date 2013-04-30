/* File: xtra1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
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
	c_put_str(TERM_WHITE, "	     ", row, col);

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
		p = c_text + cp_ptr->title[(p_ptr->lev-1)/5];
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
	if (c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL) return;


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

	town_type *t_ptr = &t_info[p_ptr->dungeon];
	dungeon_zone *zone = &t_ptr->zone[0];

	/* Get the zone */	
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	if (!zone->fill)
	{
		strcpy(depths, "Town");
	}
	else if ((p_ptr->depth == min_depth(p_ptr->dungeon)) && (zone->level < MAX_ZONE_WILDS))
	{
		strcpy(depths, "Ruins");
	}
	else if (p_ptr->depth == min_depth(p_ptr->dungeon))
	{
		strcpy(depths, "Wilds");
	}
	else if (depth_in_feet)
	{
		sprintf(depths, "%d ft", (p_ptr->depth-min_depth(p_ptr->dungeon)) * 50);
	}
	else
	{
		sprintf(depths, "Lev %d", (p_ptr->depth-min_depth(p_ptr->dungeon)));
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

	/* Fainting / Starving */
	else if (p_ptr->rest < PY_REST_FAINT)
	{
		attr = TERM_RED;
		strcpy(text, "Exhausted ");
	}

	/* Weak */
	else if (p_ptr->rest < PY_REST_WEAK)
	{
		attr = TERM_ORANGE;
		strcpy(text, "Exhausted ");
	}

	/* Tired */
	else if (p_ptr->rest < PY_REST_ALERT)
	{
		attr = TERM_YELLOW;
		strcpy(text, "Tired     ");
	}

	/* Nothing interesting */
	else
	{
                strcpy(text, "          ");
	}

	/* Hack -- handle some other stuff here. Don't change attr, so we inherit it from above. */
		if (p_ptr->searching) strcpy(text, "Searching ");
		if (p_ptr->held_song) strcpy(text, "Singing   ");


	/* Display the info (or blanks) */
	c_put_str(attr, text, ROW_STATE, COL_STATE);
}


/*
 * Prints the speed of a character.		     -CJS-
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



#ifdef USE_CLASS_PRETTY_NAMES


/*
 * Hack plus -- defines 'pretty' names for various class + style combinations.
 * Absolutely cosmetic.
 *
 * We make this a compile time option to save space on Angband lite varients.
 *
 * We use long_name to indicate there is less restriction of name length, and
 * short name to indicate that we want the name returned, rather than the style.
 *
 * We guarantee long names are distinct and try to make short names distinct.
 */
void lookup_prettyname(char name[60], int style, int sval, bool long_name, bool short_name)
{
	char temp[60];

	temp[0] = '\0';

	if (short_name) strcpy(temp,c_name+c_info[p_ptr->pclass].name);

	switch (p_ptr->pclass)
	{

		case 0:
			if (style == WS_UNARMED) strcpy(temp,"Martial Artist");
			if (style == WS_ONE_HANDED) strcpy(temp,"Swashbuckler");
			if (style == WS_TWO_HANDED) strcpy(temp,"Samurai");
			if (style == WS_TWO_WEAPON) strcpy(temp,"Gladiator");
			if (style == WS_WEAPON_SHIELD) strcpy(temp,"Knight");
			if (style == WS_RING) strcpy(temp,"Ringbearer");
			if ((style == WS_HAFTED) && (long_name))
			{
				strcpy(temp,"Weaponmaster (Hafted)");
			}
			else if ((style == WS_HAFTED) && (short_name))
			{
				strcpy(temp,"Weaponmaster");
			}
			if ((style == WS_POLEARM) && (long_name))
			{
				strcpy(temp,"Weaponmaster (Polearm)");
			}
			else if ((style == WS_POLEARM) && (short_name))
			{
				strcpy(temp,"Weaponmaster");
			}
			if (style == WS_SWORD) strcpy(temp,"Swordmaster");
			if (style == WS_THROWN) strcpy(temp,"Tribesman");
			if (style == WS_BOW) strcpy(temp,"Nomad");
			if (style == WS_SLAY_ORC) strcpy(temp,"Orckiller");
			if (style == WS_SLAY_TROLL) strcpy(temp,"Trollkiller");
			if (style == WS_SLAY_GIANT) strcpy(temp,"Giantkiller");
			if (style == WS_SLAY_DRAGON) strcpy(temp,"Dragonkiller");
			if (style == WS_RING) strcpy(temp,"Ringbearer");
			break;

		case 1:
			if (style == WS_POTION) strcpy(temp,"Herbalist");
			if (style == WS_SCROLL) strcpy(temp,"Sage");
			if (style == WS_WAND) strcpy(temp,"Magician");
			if (style == WS_STAFF) strcpy(temp,"Wizard");
			if (style == WS_AMULET) strcpy(temp,"Witch");
			if (style == WS_RING) strcpy(temp,"Ringwielder");
			if ((style == WS_MAGIC_BOOK) && (sval >= 0))
			{

				object_kind *k_ptr=&k_info[0];
				int i;
			
				/* Analyse books */
				for (i = 0;i<z_info->k_max;i++)
				{
					k_ptr = &k_info[i];

					if ((k_ptr->tval == TV_MAGIC_BOOK) && (k_ptr->sval == sval)) break;
				}

				if (short_name) strcpy(temp,"Magi");
				else sprintf(temp,"%s",k_name+k_ptr->name);
				if (long_name) sprintf(temp,"Magi %s",k_name+k_ptr->name);
				if (sval == 1) strcpy(temp,"Conjuror");
				if (sval == 2) strcpy(temp,"Invoker");
				if (sval == 3) strcpy(temp,"Sorcerer");
				if (sval == 6) strcpy(temp,"Archmage");
				if (sval == 7) strcpy(temp,"Warlock");
				if (sval == 8) strcpy(temp,"War mage");
				if (sval == 22) strcpy(temp,"Enchanter");
				if (sval == 24) strcpy(temp,"Healer");
				if (sval == 21) strcpy(temp,"Runesmith");
				if (sval == 18) strcpy(temp,"Geomancer");
				if (sval == 20) strcpy(temp,"Hydromancer");
				if (sval == 9) strcpy(temp,"Pyromancer");
				if (sval == 11) strcpy(temp,"Cyromancer");
				if (sval == 13) strcpy(temp,"Alchemist");
				if (sval == 14) strcpy(temp,"Necromancer");
				if (sval == 25) strcpy(temp,"Artificer");
				if (sval == 23) strcpy(temp,"Sage");
				if (sval == 15) strcpy(temp,"Beastmaster");
				if (sval == 16) strcpy(temp,"Illusionist");

			}
			break;

		case 2:
			if (style == WS_UNARMED) strcpy(temp,"Warrior Monk");
			if (style == WS_HAFTED) strcpy(temp,"Templar");
			if (style == WS_SLAY_EVIL) strcpy(temp,"Inquisitor");
			if ((style == WS_SLAY_DEMON) && (long_name))
			{
				strcpy(temp,"Exorcist (Ordo Maleficarum)");
			}
			else if ((style == WS_SLAY_DEMON) && (short_name))
			{
				strcpy(temp,"Exorcist");
			}
			if ((style == WS_SLAY_UNDEAD) && (long_name))
			{
				strcpy(temp,"Exorcist (Ordo Necros)");
			}
			else if ((style == WS_SLAY_UNDEAD) && (short_name))
			{
				strcpy(temp,"Exorcist");
			}
			break;

		case 3:
			if (style == WS_POTION) strcpy(temp,"Chemist");
			if (style == WS_SCROLL) strcpy(temp,"Scholar");
			if (style == WS_AMULET) strcpy(temp,"Gypsy");
			if (style == WS_RING) strcpy(temp,"Jeweler");
			break;
		case 4:
			if (style == WS_ONE_HANDED) strcpy(temp,"Outlaw");
			if (style == WS_TWO_WEAPON) strcpy(temp,"Bounty Hunter");
			if (style == WS_BOW) strcpy(temp,"Huntsman");
			if (style == WS_SLAY_ORC) strcpy(temp,"Orcslayer");
			if (style == WS_SLAY_TROLL) strcpy(temp,"Trollslayer");
			if (style == WS_SLAY_GIANT) strcpy(temp,"Giantslayer");
			if (style == WS_SLAY_ANIMAL) strcpy(temp,"Tracker");
			break;

		case 5:
			if (style == WS_ONE_HANDED) strcpy(temp,"Cavalier");
			if ((style == WS_WEAPON_SHIELD) && (long_name))
			{
				strcpy(temp,"Knight Defender");
			}
			else if (style == WS_WEAPON_SHIELD)
			{
				strcpy(temp,"Defender");
			}
			if (style == WS_SLAY_DRAGON) strcpy(temp,"Dragonslayer");
			if ((style == WS_PRAYER_BOOK) && (sval >= 0))
			{

				object_kind *k_ptr=&k_info[0];
				int i;
			
				/* Analyse books */
				for (i = 0;i<z_info->k_max;i++)
				{
					k_ptr = &k_info[i];

					if ((k_ptr->tval == TV_PRAYER_BOOK) && (k_ptr->sval == sval)) break;
				}

				if (short_name) strcpy(temp,"Knight Errant");
				else sprintf(temp,"%s",k_name+k_ptr->name);
				if (long_name) sprintf(temp,"Knight of the Order %s",k_name+k_ptr->name);
			}
			break;
		case 6:
			if (style == WS_UNARMED) strcpy(temp,"Acrobat");
			if (style == WS_ONE_HANDED) strcpy(temp,"Highwayman");
			if (style == WS_TWO_HANDED) strcpy(temp,"Ninja");
			if (style == WS_TWO_WEAPON) strcpy(temp,"Enforcer");
			if (style == WS_THROWN) strcpy(temp,"Juggler");
			if (style == WS_BACKSTAB) strcpy(temp,"Assassin");
			if (style == WS_POTION) strcpy(temp,"Apothecry");
			if (style == WS_SCROLL) strcpy(temp,"Archeologist");
			if (style == WS_AMULET) strcpy(temp,"Merchant");
			if (style == WS_RING) strcpy(temp,"Fence");
			if (style == WS_SLAY_UNDEAD) strcpy(temp,"Graverobber");
			if (style == WS_SLAY_DEMON) strcpy(temp,"Tombrobber");
			break;

		case 7:
			if (style == WS_SLING) strcpy(temp,"Slinger");
			if (style == WS_BOW) strcpy(temp,"Longbowman");
			if (style == WS_XBOW) strcpy(temp,"Crossbowman");
			break;
		case 8:
			if (style == WS_UNARMED) strcpy(temp,"Mystic");

			if (((style == WS_PRAYER_BOOK) || (style == WS_MAGIC_BOOK)) && (sval >= 0))
			{

				object_kind *k_ptr=&k_info[0];
				int i;

				int t = 0;

				switch(style)
				{
					case WS_MAGIC_BOOK:
						t = TV_MAGIC_BOOK;
						break;
					case WS_PRAYER_BOOK:
						t = TV_PRAYER_BOOK;
						break;
				}
			
				/* Analyse books */
				for (i = 0;i<z_info->k_max;i++)
				{
					k_ptr = &k_info[i];

					if ((k_ptr->tval == t) && (k_ptr->sval == sval)) break;
				}

				if (short_name) strcpy(temp,"Cultist");
				else sprintf(temp,"%s",k_name+k_ptr->name);
				if (long_name) sprintf(temp,"Cultist %s",
					k_name+k_ptr->name);
			}
			break;
		case 9:
			if (style == WS_UNARMED) strcpy(temp,"Dervish");
			if (style == WS_THROWN) strcpy(temp,"Jester");
			if (style == WS_INSTRUMENT) strcpy(temp,"Musician");

			if ((style == WS_SONG_BOOK) && (sval >= 0))
			{

				object_kind *k_ptr=&k_info[0];
				int i;
			
				/* Analyse books */
				for (i = 0;i<z_info->k_max;i++)
				{
					k_ptr = &k_info[i];

					if ((k_ptr->tval == TV_SONG_BOOK) && (k_ptr->sval == sval)) break;
				}

				if (short_name) strcpy(temp,"Spellsinger");
				else sprintf(temp,"%s",k_name+k_ptr->name);
				if (long_name) sprintf(temp,"Spellsinger (%s)",k_name+k_ptr->name);
			}
			break;
	}

	strcpy(name,temp);
}

#endif


/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;
#ifdef USE_CLASS_PRETTY_NAMES

	char name[60];

	lookup_prettyname(name,p_ptr->pstyle,p_ptr->psval,FALSE,TRUE);

#endif
	/* Race and Class */
	prt_field(p_name + rp_ptr->name, ROW_RACE, COL_RACE);

#ifdef USE_CLASS_PRETTY_NAMES
	if (strlen(name)) prt_field(name, ROW_CLASS, COL_CLASS);
	else prt_field(c_name + cp_ptr->name, ROW_CLASS, COL_CLASS);

#else
	prt_field(c_name + cp_ptr->name, ROW_CLASS, COL_CLASS);
#endif
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
 * Adjust for width and split messages.  XXX XXX XXX
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
	for (j = 0; j < 8; j++)
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
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypassed until the character is created.
 *
 * We detect characters with the old data structures for spells
 * and correct them here.
 * 
 * In the new structure, all learned bits must be set contiguous
 * from bit 0 up.
 */
static void calc_spells(void)
{
	int i, ii, j, k, levels;
	int num_allowed, num_known;
	bool spells_done = FALSE;
	bool spells_bad = FALSE;

	spell_type *s_ptr;
	spell_cast *sc_ptr = &(s_info[0].cast[0]);

	int max_spells = PY_MAX_SPELLS;
	cptr p;

	if (!variant_study_more) max_spells = 64;

	/* Hack --- We don't know which book it comes from */
	switch (c_info[p_ptr->pclass].spell_stat)
	{
		case A_WIS:
			p="prayer";
		break;

		case A_CHR:
			p = "song";
		break;

		default:
			p="spell";
		break;

	}

	if (c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Determine the number of spells allowed */
	levels = p_ptr->lev - c_info[p_ptr->pclass].spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Extract total allowed spells */
	num_allowed = (adj_mag_study[p_ptr->stat_ind[c_info[p_ptr->pclass].spell_stat]] *
		       levels / 2);

	/* Hack --- adjust num_allowed */
	if (num_allowed > max_spells) num_allowed = max_spells;

	/* Hack --- Assume no spells available */
	k = 0;

	/* Number of spells can be learnt */
	for (j = 0; j < z_info->s_max;j++)
	{
		/* Get the spell details */
		for (ii=0;ii<MAX_SPELL_CASTERS;ii++)
		{
			if (s_info[j].cast[ii].class == p_ptr->pclass)
			{
				k++;
			}
		}
	}

	/* Hack --- Assume cannot learn more than spells in spell books */
	if (num_allowed > k) num_allowed = k;


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < PY_MAX_SPELLS; j++)
	{
		/* Count known spells */
		if ((j < 32) ? (p_ptr->spell_learned1 & (1L << j)) :
		      ((j < 64) ? (p_ptr->spell_learned2 & (1L << (j - 32))) :
		      ((j < 96) ? (p_ptr->spell_learned3 & (1L << (j - 64))) :
		      (p_ptr->spell_learned4 & (1L << (j - 96))))))
		{
			num_known++;

		}
		else if (p_ptr->spell_order[j] == 99)
		{

			if (spells_done) spells_bad = TRUE;
			spells_done = TRUE;
		}
	}
	/* Fix spells, spell order and so forth for old characters. */
	if (spells_bad)
	{
                /* Hack -- forget all spells */
                p_ptr->spell_learned1 = 0;
                p_ptr->spell_learned2 = 0;
                p_ptr->spell_learned3 = 0;
                p_ptr->spell_learned4 = 0;

                /* Hack -- unlearn all spells */
                p_ptr->spell_forgotten1 = 0;
                p_ptr->spell_forgotten2 = 0;
                p_ptr->spell_forgotten3 = 0;
                p_ptr->spell_forgotten4 = 0;

                /* Hack -- unlearn all spells */
                for (j = 0; j < PY_MAX_SPELLS; j++) p_ptr->spell_order[j] = 0;

                /* Hack -- unlearn all spells */
                num_known = 0;
        }

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;

	/* Forget spells which are too hard */
	for (i = PY_MAX_SPELLS-1; i >= 0; i--)
	{
		/* Efficiency -- all done */
                if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2 && !p_ptr->spell_learned3 && !p_ptr->spell_learned4) break;

		/* Get the spell */
		j = p_ptr->spell_order[i];

		/* Skip non-spells */
		if (j == 0) continue;

		/* Get the spell */
		s_ptr = &s_info[j];

		/* Get the spell details */
		for (ii=0;ii<MAX_SPELL_CASTERS;ii++)
		{
			if (s_ptr->cast[ii].class == p_ptr->pclass)
			{
				sc_ptr=&(s_ptr->cast[ii]);
			}
		}

		/* Skip spells we are allowed to know */
		if (spell_level(j) <= p_ptr->lev) continue;

		/* Is it known? */
		if ((i < 32) ? (p_ptr->spell_learned1 & (1L << i)) :
		      ((i < 64) ? (p_ptr->spell_learned2 & (1L << (i - 32))) :
		      ((i < 96) ? (p_ptr->spell_learned3 & (1L << (i - 64))) :
		      (p_ptr->spell_learned4 & (1L << (i - 96))))))
		{
			/* Mark as forgotten */
			if (i < 32)
			{
				p_ptr->spell_forgotten1 |= (1L << i);
			}
			else if (i < 64)
			{
				p_ptr->spell_forgotten2 |= (1L << (i - 32));
			}
			else if (i < 96)
			{
				p_ptr->spell_forgotten3 |= (1L << (i - 64));
			}
			else
			{
				p_ptr->spell_forgotten4 |= (1L << (i - 96));
			}

			/* No longer known */
			if (i < 32)
			{
				p_ptr->spell_learned1 &= ~(1L << i);
			}
			else if (i < 64)
			{
				p_ptr->spell_learned2 &= ~(1L << (i - 32));
			}
			else if (i < 96)
			{
				p_ptr->spell_learned3 &= ~(1L << (i - 64));
			}
			else
			{
				p_ptr->spell_learned4 &= ~(1L << (i - 96));
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
				   s_name + s_ptr->name);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}

	/* Forget spells if we know too many spells */
	for (i = PY_MAX_SPELLS-1; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Efficiency -- all done */
                if (!p_ptr->spell_learned1 && !p_ptr->spell_learned2 && !p_ptr->spell_learned3 && !p_ptr->spell_learned4) break;

		/* Get the (i+1)th spell learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j == 0) continue;

		/* Get the spell */
		s_ptr = &s_info[j];

		/* Forget it (if learned) */
		if ((i < 32) ? (p_ptr->spell_learned1 & (1L << i)) :
		      ((i < 64) ? (p_ptr->spell_learned2 & (1L << (i - 32))) :
		      ((i < 96) ? (p_ptr->spell_learned3 & (1L << (i - 64))) :
		      (p_ptr->spell_learned4 & (1L << (i - 96))))))
		{
			/* Mark as forgotten */
			if (i < 32)
			{
				p_ptr->spell_forgotten1 |= (1L << i);
			}
			else if (i < 64)
			{
				p_ptr->spell_forgotten2 |= (1L << (i - 32));
			}
			else if (i < 96)
			{
				p_ptr->spell_forgotten3 |= (1L << (i - 64));
			}
			else
			{
				p_ptr->spell_forgotten4 |= (1L << (i - 96));
			}

			/* No longer known */
			if (i < 32)
			{
				p_ptr->spell_learned1 &= ~(1L << i);
			}
			else if (i < 64)
			{
				p_ptr->spell_learned2 &= ~(1L << (i - 32));
			}
			else if (i < 96)
			{
				p_ptr->spell_learned3 &= ~(1L << (i - 64));
			}
			else
			{
				p_ptr->spell_learned4 &= ~(1L << (i - 96));
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
				   s_name + s_ptr->name);

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
                if (!p_ptr->spell_forgotten1 && !p_ptr->spell_forgotten2 && !p_ptr->spell_forgotten3 && !p_ptr->spell_forgotten4) break;

		/* Get the next spell we learned */
		j = p_ptr->spell_order[i];

		/* Skip unknown spells */
		if (j == 0) break;

		/* Get the spell */
		s_ptr = &s_info[j];

		/* Get the spell details */
		for (ii=0;ii<MAX_SPELL_CASTERS;ii++)
		{
			if (s_ptr->cast[ii].class == p_ptr->pclass)
			{
				sc_ptr=&(s_ptr->cast[ii]);
			}
		}

		/* Skip spells we cannot remember */
		if (spell_level(j) > p_ptr->lev) continue;

		/* First set of spells */
		if ((i < 32) ? (p_ptr->spell_forgotten1 & (1L << i)) :
		      ((i < 64) ? (p_ptr->spell_forgotten2 & (1L << (i - 32))) :
		      ((i < 96) ? (p_ptr->spell_forgotten3 & (1L << (i - 64))) :
		      (p_ptr->spell_forgotten4 & (1L << (i - 96))))))
		{
			/* No longer forgotten */
			if (i < 32)
			{
				p_ptr->spell_forgotten1 &= ~(1L << i);
			}
			else if (i < 64)
			{
				p_ptr->spell_forgotten2 &= ~(1L << (i - 32));
			}
			else if (i < 96)
			{
				p_ptr->spell_forgotten3 &= ~(1L << (i - 64));
			}
			else
			{
				p_ptr->spell_forgotten4 &= ~(1L << (i - 96));
			}

			/* Mark as remembered */
			if (i < 32)
			{
				p_ptr->spell_learned1 |= (1L << i);
			}
			else if (i < 64)
			{
				p_ptr->spell_learned2 |= (1L << (i - 32));
			}
			else if (i < 96)
			{
				p_ptr->spell_learned3 |= (1L << (i - 64));
			}
			else
			{
				p_ptr->spell_learned4 |= (1L << (i - 96));
			}

			/* Message */
			msg_format("You have remembered the %s of %s.",
				   p, s_name + s_ptr->name);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}

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

	int i;

	player_class *pc_ptr = &(c_info[p_ptr->pclass]);

	object_type *o_ptr;

	bool icky_hands = FALSE;

	/* Do not show mana unless it matters */
	if (pc_ptr->spell_first > PY_MAX_LEVEL) return;

	/* Extract "effective" player level */
	levels = (p_ptr->lev - pc_ptr->spell_first) + 1;

	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana */
	msp = adj_mag_mana[p_ptr->stat_ind[pc_ptr->spell_stat]] * levels / 2;

	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Assume player is not encumbered by gloves */
	p_ptr->cumber_glove = FALSE;

	/* Check for icky_hands*/
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].benefit != WB_ICKY_HANDS) continue;

		/* Check for styles */
		if ((w_info[i].styles==0) || (w_info[i].styles & (p_ptr->cur_style & (1L << p_ptr->pstyle))))
		{
			icky_hands=TRUE;
		}

	}

	/* Only mages are affected */
	if (icky_hands)
	{
		u32b f1, f2, f3;

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
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = pc_ptr->spell_weight;

	/* Heavy armor penalizes mana */
	if ((((cur_wgt - max_wgt) / 10) > 0) && (msp))
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
 * Calculate the players available runes
 *
 * Uses carried runes, runes on ground and seasonal information.
 *
 */
static void calc_runes(void)
{
        int i;
        int this_o_idx,next_o_idx;

	/* Clear runes */
	p_ptr->cur_runes = 0;

	/* Check inventory */
	for (i = 0;i<INVEN_WIELD;i++)
	{
		if (inventory[i].k_idx)
		{
			if (inventory[i].tval == TV_RUNESTONE) p_ptr->cur_runes |= (2 << (inventory[i].sval-1));
		}
	}

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[p_ptr->py][p_ptr->px]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

                if (o_ptr->tval == TV_RUNESTONE) p_ptr->cur_runes |= (2 << inventory[i].sval);
	}

	/* Fun hack -- each rune has 1 day in lunar cycle */
	p_ptr->cur_runes |= 2 << ((turn / (10L * TOWN_DAWN)) % z_info->y_max);

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

	u32b f1;
	u32b f2;
	u32b f3;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Player is glowing */
	if (p_ptr->lite)
	{
#ifdef ALLOW_OBJECT_INFO_MORE
		equip_can_flags(0x0L,0x0L,TR3_LITE);
#endif
		p_ptr->cur_lite = 2;
	}

	/* Get the object flags */
	object_flags(o_ptr,&f1,&f2,&f3);

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
		{
			p_ptr->cur_lite = 3;

#ifdef ALLOW_OBJECT_INFO_MORE
			object_can_flags(o_ptr,0x0L,0x0L,TR3_INSTA_ART);
#endif

		}

	}
	/* Examine spells */
	else if (o_ptr->tval == TV_SPELL)
	{
		if (f3 & (TR3_LITE))
		{
			p_ptr->cur_lite = 3;

#ifdef ALLOW_OBJECT_INFO_MORE
			object_can_flags(o_ptr,0x0L,0x0L,TR3_LITE);
#endif
		}
		else
		{	     
			p_ptr->cur_lite = 1;
		}	     
	}

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


/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * Now also notices feature based slow downs from equipment being
 * weighed down. We need to add a swimming skill to allow players
 * to avoid the greater of two penalties. Currently, they always
 * succeed. This is probably not the best place for this function,
 * instead we should modify a p_ptr structure value elsewhere and
 * pass it in.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal goblin.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 *
 * Added in code to calculate blows, shots and might for weapon
 * styles, and most other benefits.
 * Note that bonuses for hit and damage for "weapons" and hit
 * bonuses for "bows" are not calculated here, nor are "power"
 * or "critical" benefits. 
 *
 * Also calculates what weapon styles the character is currently
 * using.
 *
 * Note that multiple weapon styles are possible in
 * conjunction, and the character is always using the WS_NONE
 * style. Also note that it is not possible to calculate some
 * styles here (WS_MAGIC_BOOK etc., WS_BACKSTAB) so that
 * these styles are limited in what flags they provide.
 *
 * In particular, "book" styles are only checked for the "power"
 * benefit and backstab styles are only checked for the weapon
 * hit and damage and critical benefit.
 */
static void calc_bonuses(void)
{
	int i, j, k, hold, weight;

	int old_speed;

	int old_telepathy;
	int old_see_inv;

	int old_esp_orc;
	int old_esp_giant;
	int old_esp_troll;
	int old_esp_dragon;

	int old_esp_demon;
	int old_esp_undead;
	int old_esp_nature;

	int old_dis_ac;
	int old_dis_to_a;

	int extra_blows;
	int extra_shots;
	int extra_might;

	int old_stat_top[A_MAX];
	int old_stat_use[A_MAX];
	int old_stat_ind[A_MAX];

	object_type *o_ptr;

	u32b f1, f2, f3;


	/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	old_esp_orc = p_ptr->esp_orc;
	old_esp_giant = p_ptr->esp_giant;
	old_esp_dragon = p_ptr->esp_dragon;
	old_esp_troll = p_ptr->esp_troll;

	old_esp_undead = p_ptr->esp_undead;
	old_esp_demon = p_ptr->esp_demon;
	old_esp_nature = p_ptr->esp_nature;

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


	/*** Reset ***/

	/* Reset player speed */
	p_ptr->pspeed = 110;

	/* Reset current style flags */
	p_ptr->cur_style = 0L;

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
	p_ptr->impact = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->esp_orc = FALSE;
	p_ptr->esp_giant = FALSE;
	p_ptr->esp_troll = FALSE;
	p_ptr->esp_dragon = FALSE;
	p_ptr->esp_undead = FALSE;
	p_ptr->esp_dragon = FALSE;
	p_ptr->esp_nature = FALSE;
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
	player_flags(&f1, &f2, &f3);

	/* Good flags */
	if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
	if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
	if (f3 & (TR3_LITE)) p_ptr->lite = TRUE;
	if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
	if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
	if (f3 & (TR3_ESP_ORC)) p_ptr->esp_orc = TRUE;
	if (f3 & (TR3_ESP_GIANT)) p_ptr->esp_giant = TRUE;
	if (f3 & (TR3_ESP_TROLL)) p_ptr->esp_troll = TRUE;
	if (f3 & (TR3_ESP_DRAGON)) p_ptr->esp_dragon = TRUE;
	if (f3 & (TR3_ESP_UNDEAD)) p_ptr->esp_undead = TRUE;
	if (f3 & (TR3_ESP_DEMON)) p_ptr->esp_demon = TRUE;
	if (f3 & (TR3_ESP_NATURE)) p_ptr->esp_nature = TRUE;
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
	if (f3 & (TR3_DRAIN_HP)) p_ptr->hp_drain = TRUE;
	if (f3 & (TR3_DRAIN_MANA)) p_ptr->mana_drain = TRUE;

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

		/* Affect blows */
		if (f1 & (TR1_BLOWS)) extra_blows += o_ptr->pval;

		/* Affect shots */
		if (f1 & (TR1_SHOTS)) extra_shots += o_ptr->pval;

		/* Affect Might */
		if (f1 & (TR1_MIGHT)) extra_might += o_ptr->pval;

		/* Good flags */
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
		if (f3 & (TR3_LITE)) p_ptr->lite = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
		if (f3 & (TR3_ESP_ORC)) p_ptr->esp_orc = TRUE;
		if (f3 & (TR3_ESP_GIANT)) p_ptr->esp_giant = TRUE;
		if (f3 & (TR3_ESP_TROLL)) p_ptr->esp_troll = TRUE;
		if (f3 & (TR3_ESP_DRAGON)) p_ptr->esp_dragon = TRUE;
			if (f3 & (TR3_ESP_NATURE)) p_ptr->esp_nature = TRUE;
		if (f3 & (TR3_ESP_UNDEAD)) p_ptr->esp_undead = TRUE;
		if (f3 & (TR3_ESP_DEMON)) p_ptr->esp_demon = TRUE;
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
		if (f3 & (TR3_DRAIN_HP)) p_ptr->hp_drain = TRUE;
		if (f3 & (TR3_DRAIN_MANA)) p_ptr->mana_drain = TRUE;

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

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_bonus_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h += o_ptr->to_h;
		p_ptr->to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_bonus_p(o_ptr)) p_ptr->dis_to_h += o_ptr->to_h;
		if (object_bonus_p(o_ptr)) p_ptr->dis_to_d += o_ptr->to_d;
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

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
		p_ptr->hold_life = TRUE;
	}

	/* Temprory shield */
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
		p_ptr->resist_fear = TRUE;
	}

	/* Temporary "Beserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
		p_ptr->resist_fear = TRUE;
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
		p_ptr->see_infra+=5;
	}


	/*** Analyze weight ***/

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* Base tire rate */
	k = 10 - adj_con_fix[p_ptr->stat_ind[A_CON]];

	/* Apply "tiredness" from weight */
	if (j > i/2) k += ((j - (i/2)) / (i / 10));

	/* ANDY - Modify the weight for terrain underneath */

	/* Ignore annoying locations */
	if (in_bounds_fully(p_ptr->py, p_ptr->px))
	{
		feature_type *f_ptr =&f_info[cave_feat[p_ptr->py][p_ptr->px]];

		bool can_swim = ((f_ptr->flags2 & (FF2_CAN_SWIM)) ? TRUE : FALSE);

		/* If both hands not free, we cannot swim */
		if ((inventory[INVEN_WIELD].k_idx) ||
			(inventory[INVEN_ARM].k_idx)) can_swim = FALSE;

		if (f_ptr->flags2 & (FF2_FILLED))
		{

			/* ANDY - Need to check for swimming skill XXX */
			if (f_ptr->flags2 & (FF2_CAN_SWIM))
			{
				j = j * 3;
				k = k * 3;
			}
			else
			{
				j = j * 4;
				k = k * 4;
			}
		}
		else if (f_ptr->flags2 & (FF2_DEEP))
		{
			/* ANDY - Need to check for swimming skill XXX */
			if (f_ptr->flags2 & (FF2_CAN_SWIM))
			{
				j = j * 2;
				k = k * 2;
			}
			else
			{
				j = j * 3;
				k = k * 3;
			}
		}
		else if (f_ptr->flags2 & (FF2_SHALLOW))
		{
			j = (j * 3)/2;
			k = (k * 3)/2;
		}
	}

	/* Set the rate of tiring */
	p_ptr->tiring = k;

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
	p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 10);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

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

	/* Hack -- we use 'firing' for throwing weapons */

	/* Always get to shoot */
	p_ptr->num_fire = 1;

	/* Always get to do some damage */
	p_ptr->ammo_mult = 1;

	/* Analyze launcher */
	if (o_ptr->k_idx)
	{
		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
			/* Sling and ammo */
			case SV_SLING:
			{
				/* Hack -- slings now act like 'throwers' */
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

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += extra_shots;

			/* Extra might */
			p_ptr->ammo_mult += extra_might;
	      
		}


		/* Require at least one shot */
		if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
	}



	/*** Analyze weapon ***/

	/* Examine the "current weapon" */
	o_ptr = &inventory[INVEN_WIELD];

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* Get the weight */
	if (o_ptr->k_idx) weight = o_ptr->weight;

	/* Hack -- no weapon */
	else weight = 0;

	/* Examine the "secondary weapon" */
	o_ptr = &inventory[INVEN_ARM];

	/* Add weight of secondary weapon */
	if (o_ptr->k_idx && (o_ptr->tval != TV_SHIELD)) weight += 2* o_ptr->weight;

	/* It is hard to hold a heavy weapon */
	if (hold < weight / 10)
	{
		/* Hard to wield a heavy weapon */
		p_ptr->to_h += 2 * (hold - weight / 10);
		p_ptr->dis_to_h += 2 * (hold - weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	}

	/* Normal weapons */
	if (weight && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = c_info[p_ptr->pclass].max_attacks;
		int wgt = c_info[p_ptr->pclass].min_weight;
		int mul = c_info[p_ptr->pclass].att_multiply;
		int div;

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((weight < wgt) ? wgt : weight);

		/* Get the strength vs weight */
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
		p_ptr->skill_dig += (weight / 10);
	}



	/*** Compute styles ***/

	/* Assume okay */
	p_ptr->icky_wield = FALSE;

	/* Hack --- always uses no style */
	p_ptr->cur_style |= (1L << WS_NONE);

	/* Check weapon preference styles */
	o_ptr = &inventory[INVEN_WIELD];

	if ((!o_ptr->k_idx) || (o_ptr->number > 1))
	{
		p_ptr->cur_style |= (1L << WS_UNARMED);
	}
	else 
	{
		/* One-handed if less than 20 lbs */
		if (o_ptr->weight < 200) p_ptr->cur_style |= (1L << WS_ONE_HANDED);

		/* Two-handed if greater than or equal to 10 lbs */
		if (o_ptr->weight >= 100) p_ptr->cur_style |= (1L << WS_TWO_HANDED);

		/* Set weapon preference styles */
		switch(o_ptr->tval)
		{
                        case TV_STAFF:
			case TV_HAFTED:
			{
				p_ptr->cur_style |= (1L << WS_HAFTED);
				break;
			}
			case TV_SWORD:
			{
				p_ptr->cur_style |= (1L << WS_SWORD);
				break;
			}
			case TV_POLEARM:
			{
				p_ptr->cur_style |= (1L << WS_POLEARM);
				break;
			}
			case TV_DIGGING:
				break;
			default:
			{
				p_ptr->cur_style |= (1L << WS_UNARMED);
				break;
			}
		}
	}

	/* Check shooting preference styles */
	o_ptr = &inventory[INVEN_BOW];

	if (!o_ptr->k_idx)
	{
		p_ptr->cur_style |= (1L << WS_THROWN);
	}
        else if (o_ptr->tval==TV_INSTRUMENT)
        {
                p_ptr->cur_style |= (1L << WS_INSTRUMENT);
        }
	else if (o_ptr->tval==TV_BOW)
	{
		/* Set shooting preference styles */
		switch(o_ptr->sval)
		{
			case SV_SLING:
			{
				p_ptr->cur_style |= (1L << WS_SLING);
				break;
			}
			case SV_SHORT_BOW:
			case SV_LONG_BOW:
			{
				p_ptr->cur_style |= (1L << WS_BOW);
				break;
			}
			case SV_LIGHT_XBOW:
			case SV_HEAVY_XBOW:
			{
				p_ptr->cur_style |= (1L << WS_XBOW);
				break;
			}
		}
	}

	
	/* Check fighting styles */
	o_ptr = &inventory[INVEN_ARM];

	/* Carrying a shield */
	if (o_ptr->k_idx)
	{
		/* Shield is not one-handed */
		p_ptr->cur_style &= ~(1L << WS_ONE_HANDED);

		/* Shield if not two-handed */
		p_ptr->cur_style &= ~(1L << WS_TWO_HANDED);

		/* Set fighting styles */
		switch(o_ptr->tval)
		{
			case TV_SHIELD:
			{
				p_ptr->cur_style |= (1L << WS_WEAPON_SHIELD);
				break;
			}
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_STAFF:
			{
				p_ptr->cur_style |= (1L << WS_TWO_WEAPON);
				break;
			}
		}
	}

	/*** Handle style benefits ***/
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		/* Check for styles */
		if ((w_info[i].styles==0) || (w_info[i].styles & (p_ptr->cur_style & (1L << p_ptr->pstyle))))
		{
			switch (w_info[i].benefit)
			{

				case WB_HIT:
				case WB_DAM:
				case WB_CRITICAL:
				case WB_POWER:
				case WB_ICKY_HANDS:
					/* Handled elsewhere */
				break;

				case WB_AC:
					if (!p_ptr->heavy_wield) p_ptr->to_a += (p_ptr->lev-w_info[i].level) /2;
					break;

				case WB_BLOW:
					if (!p_ptr->heavy_wield) p_ptr->num_blow++;
					break;

				case WB_SHOT:
					if (!p_ptr->heavy_shoot) p_ptr->num_fire++;
					break;

				case WB_MIGHT:
					if (!p_ptr->heavy_shoot) extra_might++;
					break;

				case WB_ICKY_WIELD:
					if ((inventory[INVEN_WIELD].k_idx) && (!p_ptr->bless_blade)) p_ptr->icky_wield = TRUE;
					break;

				case WB_BLESSED:
					p_ptr->bless_blade = TRUE;
					p_ptr->icky_wield = FALSE;
					break;

				case WB_RES_FEAR:
					p_ptr->resist_fear = TRUE;
					break;
			 }
		}

	}

	/* Don't like our weapon */
	if (p_ptr->icky_wield)
	{
		/* Reduce the real bonuses */
		p_ptr->to_h -= 2;
		p_ptr->to_d -= 2;

		/* Reduce the mental bonuses */
		p_ptr->dis_to_h -= 2;
		p_ptr->dis_to_d -= 2;

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
			else if ((i == c_info[p_ptr->pclass].spell_stat) && (c_info[p_ptr->pclass].spell_first <= PY_MAX_LEVEL))
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

	/* Hack -- ESP Change */
	if (p_ptr->esp_orc != old_esp_orc)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- ESP Change */
	if (p_ptr->esp_giant != old_esp_giant)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- ESP Change */
	if (p_ptr->esp_troll != old_esp_troll)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- ESP Change */
	if (p_ptr->esp_dragon != old_esp_dragon)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- ESP Change */
	if (p_ptr->esp_demon != old_esp_demon)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- ESP Change */
	if (p_ptr->esp_undead != old_esp_undead)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- ESP Change */
	if (p_ptr->esp_nature != old_esp_nature)
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

	if (p_ptr->update & (PU_RUNES))
	{
		p_ptr->update &= ~(PU_RUNES);
		calc_runes();
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
		describe_room();
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


