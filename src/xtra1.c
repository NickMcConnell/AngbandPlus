#define XTRA1_C
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
 * Converts a number (0 to 365) into a date
 */
void day_to_date(s16b day,char *date)
{
	cptr mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
	s16b days[13]={0,31,60,91, 121,152,182, 213,244,274,305,335,366};

	byte i;

	day = YEARDAY(day);

	for (i = 1; i < 13; i++)
	{
		if (day < days[i])
	{
			sprintf(date, "%2d %s ", day-days[i-1]+1, mon[i]);
		return;
	}
	}
	/* Something's gone horribly wrong */
	sprintf(date, "??????");
		return;
	}


/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
	if (val < 19)
	{
		sprintf(out_val, "    %2d", val);
		}
	else if (val < 8119)
		{
		sprintf(out_val, " %2d/%02d", (val-18)/100+18, (val-18)%100);
		}
	else
	{
		strcpy(out_val, " XX/XX");
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


#if 0
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
#endif



/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	char tmp[32];
	byte attr;
	cptr name;

	/* Choose a colour (white >18/219, yellow reduced, light green normal. */
	if (p_ptr->stat_use[stat] > 18+219) attr = TERM_WHITE;
	else if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat]) attr = TERM_YELLOW;
	else attr = TERM_L_GREEN;
	
	/* Choose a name style (reduced or normal) */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat]) name = stat_names_reduced[stat];
	else name = stat_names[stat];

	/* Display name */
	put_str(name, ROW_STAT + stat, 0);

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18+100)
	{
		put_str("!", ROW_STAT + stat, 3);
	}

	/* Display number */
	cnv_stat(p_ptr->stat_use[stat], tmp);
	c_put_str(attr, tmp, ROW_STAT + stat, COL_STAT + 6);
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
 * Prints current time
 */
static void prt_time(void)
{
	int minute = ((turn % ((10L * TOWN_DAWN)) * 1440) / ((10L * TOWN_DAWN)));
	int hour = ((minute/60)-6)%24; /* 0 to 23 */
	s16b day = 0;
	char date[20];

	/* Only keep loose minutes */
	minute = minute % 60;

	/* Work out day */
	if (turn <= 3*(10L * TOWN_DAWN)/4)
	{
		day = 1;
	}
	else
	{
		day = (turn - 3*(10L * TOWN_DAWN / 4)) / (10L * TOWN_DAWN) + 2;
	}

	hour = (hour+12) % 24;

	day_to_date((s16b)(day+p_ptr->startdate),date);
	put_str(format("%2d:%02d %s", hour, minute, date), ROW_TIME, COL_TIME);
}


/*
 * Prints current AC
 */
static void prt_ac(void)
{
	char tmp[32];

	put_str("AC:", ROW_AC, COL_AC);
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);
}

/*
 * Prints energy cost of last turn
 */
static void prt_energy(void)
{
	char tmp[32];
	put_str("LE:", ROW_ENERGY, COL_START);
	sprintf(tmp, "%4d", old_energy_use);
	c_put_str(TERM_L_GREEN, tmp, ROW_ENERGY, COL_END-4);
 }

/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	prt_nums("HP:", ROW_HP, COL_START, COL_END, p_ptr->chp, p_ptr->mhp);
}


/*
 * Prints players max/cur spell points and chi points
 */
static void prt_sp(void)
{
	prt_nums("SP:", ROW_SP, COL_START, COL_END, p_ptr->csp, p_ptr->msp);
	prt_nums("CH:", ROW_CHI, COL_START, COL_END, p_ptr->cchi, p_ptr->mchi);
}

/*
 * Displays spirit relationships
 *
 * Colour code:
 * Black = No relationship
 * Light Dark = Too powerful
 * Red = Furious or angry
 * Yellow = Annoyed or irriated
 * Green = Placated
 *
 */
  
static void prt_spirit(void)
{
	int	i, j, colour;
	char	il[2];
	byte plev = skill_set[SKILL_SHAMAN].value/2;
	spirit_type	*s_ptr;
	if(plev == 0) plev++;
	put_str("Life", ROW_LIFE, COL_START);
	put_str("Wild", ROW_WILD, COL_START);
	j=0;
	for (i=0;i<MAX_SPIRITS;i++) {
		s_ptr=&(spirits[i]);
		if(!(s_ptr->pact)) {
 			colour=0;
			sprintf(il,"#");
		}
   		else if (s_ptr->minskill > plev) {
 			colour=8;
 			sprintf(il,"#");
 		}
 		else if(s_ptr->annoyance > 8) {
 			colour=4;
 			sprintf(il, "%c", I2A(j));
 			j++;
 		}
 		else if(s_ptr->annoyance > 0) {
 			colour=11;
 			sprintf(il, "%c", I2A(j));
 			j++;
 		}
 		else {
 			colour=13;
 			sprintf(il, "%c", I2A(j));
 			j++;
 		}
 		if (i % 2 == 0)
 			c_put_str(colour, il, ROW_LIFE, COL_END-MAX_SPIRITS+1+i);
 		else
 			c_put_str(colour, il, ROW_WILD, COL_END-MAX_SPIRITS+i);
	}
}

/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	char depths[32];
	s16b level = dun_level + dun_offset;
	cptr descr[2][2] = {
	{"(Lev %d)", "%d ft"},
	{"/(Lev %d)\\", "/%d ft\\"}
	};
	if (depth_in_feet) level *= 50;


	if (dun_level)
	{
		(void)sprintf(depths, descr[(int)dun_defs[cur_dungeon].tower][(int)depth_in_feet], level);
	}
	
	else if (wild_grid[wildy][wildx].dungeon < MAX_CAVES)
		{
			(void)strcpy(depths, dun_defs[wild_grid[wildy][wildx].dungeon].shortname);
		}
		else
		{
			(void)sprintf(depths, "Wild (%d,%d)",wildx,wildy);
		}
	/* Right-Adjust the "depth", and clear old values */
	prt(format("%9s", depths), ROW_DEPTH, COL_DEPTH);
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

	/* Sneaking */
	else if (p_ptr->sneaking)
	{
		strcpy(text, "Sneaking  ");
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

	/* Hack -- Visually "undo" the Sneak Mode Slowdown */
	if (p_ptr->sneaking) i += 10;

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

/*
 * Displays temporary resistance in preference to studying ability as there's not
 * much call for libraries in the dungeon.
 */

static void prt_study(void)
{
	if (p_ptr->oppose_acid || p_ptr->oppose_elec || p_ptr->oppose_fire || p_ptr->oppose_cold || p_ptr->oppose_pois) 
	{
		put_str("     ", ROW_STUDY, COL_STUDY);
		Term_putch(COL_STUDY, ROW_STUDY, OPPOSE_COL(p_ptr->oppose_acid), 'A');
		Term_putch(COL_STUDY+1, ROW_STUDY, OPPOSE_COL(p_ptr->oppose_elec), 'E');
		Term_putch(COL_STUDY+2, ROW_STUDY, OPPOSE_COL(p_ptr->oppose_fire), 'F');
		Term_putch(COL_STUDY+3, ROW_STUDY, OPPOSE_COL(p_ptr->oppose_cold), 'C');
		Term_putch(COL_STUDY+4, ROW_STUDY, OPPOSE_COL(p_ptr->oppose_pois), 'P');
	}
	else if (p_ptr->new_spells)
	{
		put_str("Study", ROW_STUDY, COL_STUDY);
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
 *
 * Modified to give "character" as well as colour info by Dean Anderson
 */
static void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

	/* Not tracking */
	if (!health_who)
	{
		/* Erase the health bar */
		Term_erase(COL_INFO, ROW_INFO, 12);
	}

	/* Tracking an unseen monster */
	else if (!m_list[health_who].ml)
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
	else if (!m_list[health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;
		const char *smb;

		monster_type *m_ptr = &m_list[health_who];

		/* Default to almost dead */
		byte attr = TERM_RED;
		smb = "**********";

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
		if (m_ptr->monfear) {
			attr = TERM_VIOLET;
			smb = "AFRAID****";
		}
		/* Asleep */
		if (m_ptr->csleep) {
			attr = TERM_BLUE;
			smb = "SLEEPING**";
		}
		if (m_ptr->smart & SM_ALLY) {
			attr = TERM_L_UMBER;
			smb = "ALLY******";
		}
		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, smb);
	}

#endif

}



/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;

	/* All Stats */
	for (i = 0; i < 6; i++) prt_stat(i);

	/* Time */
	prt_time();
	/* Armor */
	prt_ac();

	/* Energy */
	prt_energy();
 
	/* Hitpoints */
	prt_hp();

	/* Spellpoints */
	prt_sp();
	
 	/* Spirit relationships */
 	prt_spirit();
 
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
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(bool quiet)
{
	int			i, j, k;
	int			num_allowed, num_known;
	int school;
	magic_type		*s_ptr;


    cptr p = "spell";


	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;


	/* Extract total allowed spells */
    num_allowed = ((adj_mag_study[p_ptr->stat_ind[A_INT]] *
                   skill_set[SKILL_MANA].value) / 4);

	/* Ensure at least one */
	num_allowed++;


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (i=0;i<MAX_SCHOOL;i++)
	{
		for (j = 0; j < 32; j++)
		{
			/* Count known spells */
			if (spell_learned[i] & (1L << j))
			{
				num_known++;
			}
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;

	/* Forget spells which are too hard */
	for (i = 127; i >= 0; i--)
	{
		/* Access the spell */
		j = spell_order[i];

		/* Skip non-spells */
		if (j >= 255) continue;



        /* Get the spell */
        if (j < 32)
			school = 0;
        else if (j < 64)
            school = 1;
		else if (j < 128)
			school = 2;
		else
			school = 3;

		j=(j%32);

		s_ptr = &mp_ptr->info[school][j];

		/* Skip spells we are allowed to know */
		if (s_ptr->minskill <= spell_skill(s_ptr)) continue;

		/* Is it known? */
		if (spell_learned[school] & (1L << j))
		{
			/* Mark as forgotten */
			spell_forgotten[school] |= (1L << j);

			/* No longer known */
			spell_learned[school] &= ~(1L << j);

			/* Message */
			if (!quiet) msg_format("You have forgotten the %s of %s.", p,
                       spell_names[school][j%32]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = 127; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;


		/* Get the (i+1)th spell learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 255) continue;

        /* Get the spell */
        if (j < 32)
			school = 0;
        else if (j < 64)
            school = 1;
		else if (j < 128)
			school = 2;
		else
			school = 3;

		j=(j%32);

		/* Forget it (if learned) */
		if (spell_learned[school] & (1L << j))
		{
			/* Mark as forgotten */
			spell_forgotten[school] |= (1L << j);

			/* No longer known */
				spell_learned[school] &= ~(1L << j);

			/* Message */
			if (!quiet) msg_format("You have forgotten the %s of %s.", p,
                       spell_names[school][j%32]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < 128; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Get the next spell we learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 255) break;

        /* Get the spell */
        if (j < 32)
			school = 0;
        else if (j < 64)
            school = 1;
		else if (j < 128)
			school = 2;
		else
			school = 3;

		j=(j%32);

		/* Access the spell */
           s_ptr = &mp_ptr->info[school][j];

		/* Skip spells we cannot remember */
		if (s_ptr->minskill > spell_skill(s_ptr)) continue;

		/* First set of spells */
		if (spell_forgotten[school] & (1L << j))
		{
			/* No longer forgotten */
			spell_forgotten[school] &= ~(1L << j);

			/* Known once more */
			spell_learned[school] |= (1L << j);

			/* Message */
			if (!quiet) msg_format("You have remembered the %s of %s.",
                       p, spell_names[school][j%32]);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < 128; j++)
	{

		/* Get the spell */
        if (j < 32)
			school = 0;
        else if (j < 64)
            school = 1;
		else if (j < 128)
			school = 2;
		else
			school = 3;

		/* Access the spell */
        s_ptr = &mp_ptr->info[school][j%32];

		/* Skip spells we cannot remember */
		if (s_ptr->minskill > spell_skill(s_ptr)) continue;

		/* Skip spells we already know */
		if (spell_learned[school] & (1L << (j % 32)))
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

		/* Message if needed and allowed. */
		if (p_ptr->new_spells && !quiet)
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

/* Calculated the AC bonus from a given empty armour slot. It does not check that
the player is eligible for such bonuses in the first place. */
static int mystic_armour(int slot)
{
	int i = 0;
	if (!inventory[slot].k_idx) {
		switch (slot) {
			case INVEN_BODY:
				i = (skill_set[SKILL_MA].value * 3) / 4;
				break;
			case INVEN_OUTER:
				i = ((skill_set[SKILL_MA].value - 26) / 6);
				break;
			case INVEN_ARM:
				i = ((skill_set[SKILL_MA].value - 16) / 6);
				break;
			case INVEN_HEAD:
				i = (skill_set[SKILL_MA].value - 4) / 6;
				break;
			case INVEN_HANDS:
				i = (skill_set[SKILL_MA].value / 4);
				break;
			case INVEN_FEET:
				i = (skill_set[SKILL_MA].value / 6);
				break;
			default:
				break;
		}
	}
	if (i > 0)
		return i;
	else
		return 0;
}

/*
 * Return whether a given object inhibits spellcasting.
 */
bool cumber_glove(object_type *o_ptr)
{
	u32b f[3];

	/* Only gloves can inhibit spellcasting. */
	if (wield_slot(o_ptr) != INVEN_HANDS) return FALSE;

	object_flags(o_ptr, f, f+1, f+2);

	/* Free action helps spellcasting. */
	if (f[1] & (TR2_FREE_ACT)) return FALSE;
	
	/* A dexterity bonus helps spellcasting. */
	if ((f[0] & (TR1_DEX)) && (o_ptr->pval > 0)) return FALSE;

	/* Other gloves harm spellcasting. */
	return TRUE;
}


/*
 * Return whether a given object inhibits mindcrafting.
 */
bool cumber_helm(object_type *o_ptr)
{
	u32b f[3];

	/* Only helmets can inhibit mindcrafting. */
	if (wield_slot(o_ptr) != INVEN_HEAD) return FALSE;

	object_flags(o_ptr, f, f+1, f+2);

	/* Telepathy helps mindcraft. */
	if (f[2] & (TR3_TELEPATHY)) return FALSE;
	
	/* A wisdom bonus helps mindcraft. */
	if ((f[0] & (TR1_WIS)) && (o_ptr->pval > 0)) return FALSE;

	/* Other helmets harm mindcraft. */
	return TRUE;
}

/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 *
 * This function also calculates maximum chi
 */
static void calc_mana(bool quiet)
{
	int	msp, levels, cur_wgt, max_wgt;
	int     mchi;

	object_type	*o_ptr;



	levels = skill_set[SKILL_MANA].value;


	/* Hack -- no negative mana */
	if (levels < 1) levels = 1;

	/* Extract total mana */
	msp = (adj_mag_mana[p_ptr->stat_ind[A_INT]]) * levels / 4;

	/* Hack -- add one mana */
	msp++;

	/* Assume player is not encumbered by gloves */
	p_ptr->cumber_glove = FALSE;

	/* Get the gloves */
	o_ptr = &inventory[INVEN_HANDS];

	/* Normal gloves hurt wizard spells */
	if (cumber_glove(o_ptr))
	{
		/* Encumbered */
		p_ptr->cumber_glove = TRUE;

		/* Reduce mana */
		msp = (3 * msp) / 4;
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

		/* XXX XXX XXX New mana maintenance */

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
		p_ptr->window |=(PW_SPELL);
	}

	/* Now repeat the whole process for chi */
	levels = skill_set[SKILL_CHI].value;


	/* Hack -- no negative chi */
	if (levels < 1) levels = 1;

	/* Extract total chi */
	mchi = (adj_mag_mana[p_ptr->stat_ind[A_WIS]]) * levels / 4;

	/* Hack -- add one chi */
	mchi++;

	/* Get the helmet */
	o_ptr = &inventory[INVEN_HEAD];

	/* Normal helms hurt mindcrafting */
	if (cumber_helm(o_ptr))
	{
		/* Encumbered */
		p_ptr->cumber_helm = TRUE;

		/* Reduce mana */
		mchi = (3 * mchi) / 4;
	}
	/* Special helms do not. */
	else
	{
		p_ptr->cumber_helm = FALSE;
	}
	

	/* Chi can never be negative */
	if (mchi < 0) mchi = 0;

	/* Maximum chi has changed */
	if (p_ptr->mchi != mchi)
	{

		/* XXX XXX XXX Chi maintenance */

		/* Enforce maximum */
		if (p_ptr->cchi >= mchi)
		{
			p_ptr->cchi = mchi;
			p_ptr->chi_frac = 0;
		}

		/* Save new chi */
		p_ptr->mchi = mchi;

		/* Display chi later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |=(PW_SPELL);
	}

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "glove state" changes */
	if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
	{
		/* No message */
		if (quiet);
		/* Message */
		else if (p_ptr->cumber_glove)
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

	/* Take note when "helm state" changes */
	if (p_ptr->old_cumber_helm != p_ptr->cumber_helm)
	{
		/* No message */
		if (quiet);
		/* Message */
		else if (p_ptr->cumber_helm)
		{
			msg_print("Your covered head feels unsuitable for mindcrafting.");
		}
		else
		{
			msg_print("Your head feels more suitable for mindcrafting.");
		}

		/* Save it */
		p_ptr->old_cumber_helm = p_ptr->cumber_helm;
	}

	/* Take note when "armor state" changes */
	if (p_ptr->old_cumber_armor != p_ptr->cumber_armor)
	{
		/* No message */
		if (quiet);
		/* Message */
		else if (p_ptr->cumber_armor)
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
	mhp = player_hp[skill_set[SKILL_TOUGH].value-1]/2 + (bonus*skill_set[SKILL_TOUGH].value/4);

	/* Always have at least one hitpoint per level */
	if (mhp < skill_set[SKILL_TOUGH].value) mhp = skill_set[SKILL_TOUGH].value;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero) mhp += 10;
	if (p_ptr->shero) mhp += 30;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{

#if 1

		/* XXX XXX XXX New hitpoint maintenance */

		/* Enforce maximum */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

#else

		s32b value;

		/* change current hit points proportionately to change of mhp */
		/* divide first to avoid overflow, little loss of accuracy */
		value = (((long)p_ptr->chp << 16) + p_ptr->chp_frac) / p_ptr->mhp;
		value = value * mhp;
		p_ptr->chp = (value >> 16);
		p_ptr->chp_frac = (value & 0xFFFF);

#endif

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

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Examine actual lites */
		if ((i == INVEN_LITE) && (o_ptr->k_idx) && (o_ptr->tval == TV_LITE)
			&& (allart_p(o_ptr) || o_ptr->pval > 0))
		{
			p_ptr->cur_lite += k_info[o_ptr->k_idx].extra;
		}

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Check for extra light. */
		if (f3 & TR3_LITE) p_ptr->cur_lite++;
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


/* Calculate the skill used by a certain weapon. */

int wield_skill(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_SWORD:
		{
			return k_info[o_ptr->k_idx].extra;
		}
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		{
			return SKILL_MISSILE;
		}
		/* An empty object will have a tval of 0. */
		case 0:
		{
			return SKILL_CLOSE;
		}
		default:
		{
			return 0;
		}
	}
}

/*
 * Test whether the worn armour weighs too much for effective martial arts.
 */
static bool ma_heavy_armor(void)
{

    u16b arm_wgt = 0;
    arm_wgt += inventory[INVEN_BODY].weight;
    arm_wgt += inventory[INVEN_HEAD].weight;
    arm_wgt += inventory[INVEN_ARM].weight;
    arm_wgt += inventory[INVEN_OUTER].weight;
    arm_wgt += inventory[INVEN_HANDS].weight;
    arm_wgt += inventory[INVEN_FEET].weight;

    return (arm_wgt > (u16b)((u16b)100 + (u16b)(skill_set[SKILL_MA].value * 2))) ;
}


/*
 * Calculate the martial arts AC bonus from empty slots, and set the AC for
 * each appropriately.
 */
static void calc_ma_armour(void)
{
	int i;
	for (i=INVEN_BODY; i<=INVEN_FEET; i++)
	{
		if (inventory[i].k_idx);
		
		else if ((ma_empty_hands()) && !(ma_heavy_armor()))
		{
			inventory[i].to_a = mystic_armour(i);
		}
		else
		{
			inventory[i].to_a = 0;
		}
	}
}


/*
 * Calculate the players current "state", taking into account
 * not only race intrinsics, but also objects being worn
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
 *
 * The "quiet" option disables all status change messages, used as
 * identify_fully_aux() may call this routine several times without
 * player intervention.
 */
static void calc_bonuses(bool quiet)
{
	int			i, j, hold;

	int			old_speed;

	int			old_telepathy;
	int			old_see_inv;

	int			old_dis_ac;
	int			old_dis_to_a;

	int			extra_blows;
	int			extra_shots;

	object_type		*o_ptr;

	u32b		f1, f2, f3;
	bool	mystic_armour_aux;


	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;


	/* Get weapon type */
	o_ptr = &inventory[INVEN_WIELD];

	/* Can we assign a weapon skill? */
	if (!(p_ptr->wield_skill=wield_skill(o_ptr)))
			{
			if (!quiet) msg_print("Unknown weapon tval wielded - defaulting to close combat skill.");
			p_ptr->wield_skill = SKILL_CLOSE;
	}


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


	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->xtra_might = FALSE;
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
	p_ptr->skill_dis = skill_set[SKILL_DISARM].value;
	/* Base skill -- magic devices */
	p_ptr->skill_dev = skill_set[SKILL_DEVICE].value;
	/* Base skill -- saving throw */
	p_ptr->skill_sav = skill_set[SKILL_SAVE].value;
	/* Base skill -- stealth */
	p_ptr->skill_stl = skill_set[SKILL_STEALTH].value/10;
	/* Base skill -- searching ability */
	p_ptr->skill_srh = skill_set[SKILL_SEARCH].value/2;
	/* Base skill -- searching frequency */
	p_ptr->skill_fos = skill_set[SKILL_PERCEPTION].value/2;
	/* Base skill -- combat (normal) */
	p_ptr->skill_thn = skill_set[p_ptr->wield_skill].value*3;
	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = skill_set[SKILL_MISSILE].value*3;
	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = skill_set[SKILL_MISSILE].value*3;
	/* Base skill -- digging */
	p_ptr->skill_dig = 0;


	/* Elf */
	if (p_ptr->prace == RACE_ELF) p_ptr->resist_lite = TRUE;

	/* Hobbit */
	if (p_ptr->prace == RACE_HOBBIT) p_ptr->sustain_dex = TRUE;

	/* Gnome */
	if (p_ptr->prace == RACE_GNOME) p_ptr->free_act = TRUE;

	/* Dwarf */
	if (p_ptr->prace == RACE_DWARF) p_ptr->resist_blind = TRUE;

	/* Half-Orc */
	if (p_ptr->prace == RACE_HALF_ORC) p_ptr->resist_dark = TRUE;

	/* Half-Troll */
    if (p_ptr->prace == RACE_HALF_TROLL)
    {
        p_ptr->sustain_str = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2)>14)
            {
                p_ptr->regenerate = TRUE;
                /* High level trolls heal fast... */
            }
            
    }

	/* Dunadan */
    if (p_ptr->prace == RACE_GREAT)
    {
            p_ptr->sustain_con = TRUE;
            p_ptr->regenerate = TRUE;  /* Great ones heal fast... */

    }

	/* High Elf */
	if (p_ptr->prace == RACE_HIGH_ELF) p_ptr->resist_lite = TRUE;
	if (p_ptr->prace == RACE_HIGH_ELF) p_ptr->see_inv = TRUE;

    if (p_ptr->prace == RACE_BARBARIAN) p_ptr->resist_fear = TRUE;
    else if (p_ptr->prace == RACE_HALF_OGRE)
    {   p_ptr->resist_dark = TRUE;
        p_ptr->sustain_str = TRUE;
    }
    else if (p_ptr->prace == RACE_HALF_GIANT)
    {
        p_ptr->sustain_str = TRUE;
        p_ptr->resist_shard = TRUE;
    }
    else if (p_ptr->prace == RACE_HALF_TITAN)
    {
        p_ptr->resist_chaos = TRUE;
    }
    else if (p_ptr->prace == RACE_CYCLOPS)
    {
        p_ptr->resist_sound = TRUE;
    }
    else if (p_ptr->prace == RACE_YEEK)
    {
        p_ptr->resist_acid = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 19)
        {
            p_ptr->immune_acid = TRUE;
        }
    }
    else if (p_ptr->prace == RACE_KLACKON)
    {
        p_ptr->resist_conf = TRUE;
        p_ptr->resist_acid = TRUE;
    }
    else if (p_ptr->prace == RACE_KOBOLD)
    {
        p_ptr->resist_pois = TRUE;
    }
    else if (p_ptr->prace == RACE_NIBELUNG)
    {
        p_ptr->resist_disen = TRUE;
        p_ptr->resist_dark = TRUE;
    }
    else if (p_ptr->prace == RACE_DARK_ELF)
    {
        p_ptr->resist_dark = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 19)
        {
            p_ptr->see_inv = TRUE;
        }
    }
    else if (p_ptr->prace == RACE_DRACONIAN)
    {
        p_ptr->ffall = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 4)
        {
            p_ptr->resist_fire = TRUE;
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 9)
        {
            p_ptr->resist_cold = TRUE;
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 14)
        {
            p_ptr->resist_acid = TRUE;
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 19)
        {
            p_ptr->resist_elec = TRUE;
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 34)
        {
            p_ptr->resist_pois = TRUE;
        }

    }
    else if (p_ptr->prace == RACE_MIND_FLAYER)
    {
        p_ptr->sustain_int = TRUE;
        p_ptr->sustain_wis = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 14)
        {
            p_ptr->see_inv = TRUE;
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 29)
        {
            p_ptr->telepathy = TRUE;
        }
    }
    else if (p_ptr->prace == RACE_IMP)
    {
        p_ptr->resist_fire = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 9)
        {
            p_ptr->see_inv = TRUE;
        }
    }
    else if (p_ptr->prace == RACE_GOLEM)
    {
        if ((skill_set[SKILL_RACIAL].value/2) > 34)
        {
            p_ptr->hold_life = TRUE;
        }
        p_ptr->slow_digest = TRUE;
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
        p_ptr->resist_pois = TRUE;
    }
    else if (p_ptr->prace == RACE_SKELETON)
    {
        p_ptr->resist_shard = TRUE;
        p_ptr->hold_life = TRUE;
        p_ptr->see_inv = TRUE;
        p_ptr->resist_pois = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 9)
        {
            p_ptr->resist_cold = TRUE;
        }
    }
    else if (p_ptr->prace == RACE_ZOMBIE)
    {
        p_ptr->resist_neth = TRUE;
        p_ptr->hold_life = TRUE;
        p_ptr->see_inv = TRUE;
        p_ptr->resist_pois = TRUE;
        p_ptr->slow_digest = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 4)
        {
            p_ptr->resist_cold = TRUE;
        }
    }
    else if (p_ptr->prace == RACE_VAMPIRE)
    {
        p_ptr->resist_dark = TRUE;
        p_ptr->hold_life = TRUE;
        p_ptr->resist_neth = TRUE;
        p_ptr->resist_cold = TRUE;
        p_ptr->resist_pois = TRUE;
        p_ptr->lite = TRUE;
    }
    else if (p_ptr->prace == RACE_SPECTRE)
    {
        p_ptr->resist_neth = TRUE;
        p_ptr->hold_life = TRUE;
        p_ptr->see_inv = TRUE;
        p_ptr->resist_pois = TRUE;
        p_ptr->slow_digest = TRUE;
        p_ptr->resist_cold = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2) > 34)
        {
            p_ptr->telepathy = TRUE;
        }
    }
    else if (p_ptr->prace == RACE_SPRITE)
    {
        p_ptr->ffall = TRUE;
        p_ptr->resist_lite = TRUE;
    }
    else if (p_ptr->prace == RACE_BROO)
    {
        p_ptr->resist_conf  = TRUE;
        p_ptr->resist_sound = TRUE;
    }

	/* Start with "normal" speed */
	p_ptr->pspeed = 110;

	/* Start with a single blow per turn */
	p_ptr->num_blow = 60;

	/* Start with a single shot per turn */
	p_ptr->num_fire = 60;

	/* Reset the "xtra" tval */
	p_ptr->tval_xtra = 0;

	/* Reset the "ammo" tval */
	p_ptr->tval_ammo = 0;


	/* Hack -- apply racial/template stat maxes */
	if (maximise_mode)
	{
		/* Apply the racial modifiers */
		for (i = 0; i < 6; i++)
		{
			/* Modify the stats for "race" */
			p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
		}
	}


    /* I'm adding the chaos features here for the lack of a better place... */
	if (p_ptr->muta3)
	{
		if (p_ptr->muta3 & MUT3_HYPER_STR)
		{
			p_ptr->stat_add[A_STR] += 4;
		}
		
		if (p_ptr->muta3 & MUT3_PUNY)
		{
			p_ptr->stat_add[A_STR] -= 4;
		}
		
		if (p_ptr->muta3 & MUT3_HYPER_INT)
		{
			p_ptr->stat_add[A_INT] += 4;
			p_ptr->stat_add[A_WIS] += 4;
		}
		
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
			p_ptr->skill_sav += (15 + ((skill_set[SKILL_RACIAL].value/2) / 5));
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
			p_ptr->telepathy =TRUE;
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
			p_ptr->free_act =TRUE;
			p_ptr->skill_stl += 1;
		}
		
		if (p_ptr->muta3 & MUT3_SUS_STATS)
		{
			p_ptr->sustain_con =TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 9)
				p_ptr->sustain_str = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 19)
				p_ptr->sustain_dex = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 29)
				p_ptr->sustain_wis = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 39)
				p_ptr->sustain_int = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 49)
				p_ptr->sustain_chr = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
			p_ptr->stat_add[A_CHR] = 0;
		}
	}

	/* Mystic get extra ac for armour _not worn_ */
	mystic_armour_aux = ma_heavy_armor();

	if (mystic_armour_aux || mystic_notify_aux) p_ptr->update |= PU_MA_ARMOUR;

	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx && !o_ptr->to_a) continue;

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
		if (f1 & (TR1_BLOWS)) extra_blows += (o_ptr->pval * 60);

		/* Hack -- cause earthquakes */
		if (f1 & (TR1_IMPACT)) p_ptr->impact = TRUE;

		/* Boost shots */
		if (f3 & (TR3_XTRA_SHOTS)) extra_shots+=60;

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
        if (f3 & (TR3_WRAITH)) p_ptr->wraith_form = MAX(p_ptr->wraith_form, 20);

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
		if (f2 & (TR2_RES_CONF)) p_ptr->resist_conf = TRUE;
		if (f2 & (TR2_RES_SOUND)) p_ptr->resist_sound = TRUE;
		if (f2 & (TR2_RES_LITE)) p_ptr->resist_lite = TRUE;
		if (f2 & (TR2_RES_DARK)) p_ptr->resist_dark = TRUE;
		if (f2 & (TR2_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN)) p_ptr->resist_disen = TRUE;
		if (f2 & (TR2_RES_SHARDS)) p_ptr->resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
		if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_NETHER)) p_ptr->resist_neth = TRUE;

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

    /* Hack -- aura of fire also provides light */
    if (p_ptr->sh_fire) p_ptr->lite = TRUE;

    if (p_ptr->prace == RACE_GOLEM) /* Golems also get an intrinsic AC bonus */
    {
        p_ptr->to_a += 20 + ((skill_set[SKILL_RACIAL].value/2) / 5);
        p_ptr->dis_to_a += 20 + ((skill_set[SKILL_RACIAL].value/2) / 5);
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
			if (use < 8 + 2 * (skill_set[SKILL_RACIAL].value/2))
			{
				use = 8 + 2 * (skill_set[SKILL_RACIAL].value/2);
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
				p_ptr->update |= (PU_MANA | PU_SPELLS);
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

    /* Klackons become faster... */
    if ((p_ptr->prace == RACE_KLACKON) || (p_ptr->prace == RACE_SPRITE))
    {
        p_ptr->pspeed += ((skill_set[SKILL_RACIAL].value/2)) / 10;
    }
	else if(!mystic_armour_aux) /* So do other people with martial arts... */
	{
		p_ptr->pspeed += (skill_set[SKILL_MA].value / 20);
	}

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


	/* Hack -- Res Chaos -> Res Conf */
	if (p_ptr->resist_chaos)
	{
		p_ptr->resist_conf = TRUE;
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
	j = total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Sneaking slows the player down */
	if (p_ptr->sneaking)
	{
		p_ptr->pspeed -= 15;
	}
	else /* But not sneaking cripples stealth skill */
	{
		p_ptr->skill_stl=p_ptr->skill_stl/2;
	}

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);


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

	/* It is hard to carholdry a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}


	/* Compute "extra shots" if needed */
	if (o_ptr->k_idx && !p_ptr->heavy_shoot)
	{
		/* Take note of required "tval" for missiles (should this rely
		 * on !p_ptr->heavy_shoot?). */
		p_ptr->tval_ammo = ammunition_type(o_ptr);

		/* Add additional shots for player skill, depending on weapon. */
		switch (o_ptr->k_idx)
		{
			case OBJ_SLING:
			{
				if (skill_set[SKILL_MISSILE].value==100)
					extra_shots += 120;
				else if (skill_set[SKILL_MISSILE].value>22)
					extra_shots += (skill_set[SKILL_MISSILE].value*4/3-30);
				break;
			}

			case OBJ_SHORT_BOW:
			case OBJ_LONG_BOW:
			{
				if (skill_set[SKILL_MISSILE].value==100)
					extra_shots += 180;
				if (skill_set[SKILL_MISSILE].value>15)
					extra_shots += (skill_set[SKILL_MISSILE].value*2-30);
				break;
			}

			case OBJ_LIGHT_CROSSBOW:
			case OBJ_HEAVY_CROSSBOW:
			{
				/* At 100 skill, a crossbow gives 1 1/6 extra shots anyway. */
				if (skill_set[SKILL_MISSILE].value>30)
					extra_shots += (skill_set[SKILL_MISSILE].value-30);
				break;
			}
		}


		/* Add in the "bonus shots" */
		p_ptr->num_fire += extra_shots;
		

		/* Require at least one shot */
		if (p_ptr->num_fire < 60) p_ptr->num_fire = 60;
	}



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
	}


	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = 5, wgt = 35, mul = 3, div = 0;

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

		/* Convert to blows per 60 turns */
		p_ptr->num_blow *= 60;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		/* Level bonus for warriors (0-3) */
		if (skill_set[p_ptr->wield_skill].value==100)
			p_ptr->num_blow += 180;
		else if (skill_set[p_ptr->wield_skill].value>15)
			p_ptr->num_blow += (skill_set[p_ptr->wield_skill].value*2-30);

		/* Require at least one blow */
		if (p_ptr->num_blow < 60) p_ptr->num_blow = 60;


        /* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);
	}


    /* Different calculation for mystics with empty hands */
    else if (ma_empty_hands())
    {

            p_ptr->num_blow = 0;

		if (skill_set[SKILL_MA].value == 100)
			p_ptr->num_blow = 420;
		else if (skill_set[SKILL_MA].value > 59)
			p_ptr->num_blow = skill_set[SKILL_MA].value * 6 - 210;
		else if (skill_set[SKILL_MA].value > 10)
			p_ptr->num_blow = skill_set[SKILL_MA].value * 3 - 30;

            if (mystic_armour_aux)
                p_ptr->num_blow /= 2;

            p_ptr->num_blow += 60 + extra_blows;

            if (!mystic_armour_aux)
            {
                p_ptr->to_h += (skill_set[SKILL_MA].value / 6);
                p_ptr->to_d += (skill_set[SKILL_MA].value / 6);

                p_ptr->dis_to_h += (skill_set[SKILL_MA].value / 6);
                p_ptr->dis_to_d += (skill_set[SKILL_MA].value / 6);
            }
    }
 
    p_ptr->to_h += (skill_set[p_ptr->wield_skill].value/10);
    p_ptr->to_d += (skill_set[p_ptr->wield_skill].value/10);

    p_ptr->dis_to_h += (skill_set[p_ptr->wield_skill].value/10);
    p_ptr->dis_to_d += (skill_set[p_ptr->wield_skill].value/10);


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

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	/* Limit Skill -- number of attacks from 60 to 600 per 60 rounds*/
	if (p_ptr->num_blow < 60) p_ptr->num_blow = 60;
	if (p_ptr->num_blow > 600) p_ptr->num_blow = 600;

	/* Limit Skill -- number of shots from 60 to 600 per 60 rounds*/
	if (p_ptr->num_fire < 60) p_ptr->num_fire = 60;
	if (p_ptr->num_fire > 600) p_ptr->num_fire = 600;

    if ((p_ptr->anti_magic) && (p_ptr->skill_sav < 95))
        p_ptr->skill_sav = 95;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "heavy bow" changes */
	if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* No message */
		if (quiet);
		/* Message */
		else if (p_ptr->heavy_shoot)
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
		/* No message */
		if (quiet);
		/* Message */
		else if (p_ptr->heavy_wield)
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

    if (mystic_armour_aux != mystic_notify_aux)
        {
		/* No message */
		if (quiet);
		/* Message */
		else if (mystic_armour_aux)
                msg_print("The weight of your armor disrupts your balance.");
            else
                msg_print("You regain your balance.");
            mystic_notify_aux = mystic_armour_aux;
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
 * Is cave[y][x] in a room?
 *
 * This actually looks for empty spaces connected to this square by right
 * angles.
 */
static bool is_room_func_p(int y, int x)
{
	uint i,adj;

	int xs[] = { 1, 1,-1,-1, 1,  1, 0,-1, 0, 1};
	int ys[] = { 1,-1,-1, 1, 1,  0, 1, 0,-1, 0};

	/* Hack - assume that ineligible non-floor squares are excluded elsewhere. */
	if (!cave_floor_bold(y,x)) return TRUE;

	for (i = adj = 0; i < N_ELEMENTS(xs); i++)
	{
		if (!cave_floor_bold(y+ys[i], x+xs[i]))
		{
			adj = 0;
		}
		/* (1,1) and (1,0) being floor squares does not make a room. */
		else if (adj++ && i != 5)
		{
			return TRUE;
		}
	}
	return FALSE;
}

/*
 * Check that every room square is next to another room square, to prevent
 * corners from making it easy to abuse the AI.
 */
static bool is_isolated_room_p(int y, int x)
{
	/* An array which rotates around 0,0 anticlockwise. */
	int xs[] = { 1, 1, 0,-1,-1,-1, 0, 1};
	int ys[] = { 0, 1, 1, 1, 0,-1,-1, 0};
	uint i;

	for (i = 0; i < N_ELEMENTS(xs); i++)
	{
		if (is_room_p(y+ys[i], x+xs[i])) return FALSE;
	}
	return TRUE;
}

/*
 * Set CAVE_ROOM as appropriate using the above routines.
 */
static void calc_rooms(void)
{
	int y,x;
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			if (is_room_func_p(y,x))
			{
				cave[y][x].info |= (CAVE_ROOM);
			}
			else
			{
				cave[y][x].info &= ~(CAVE_ROOM);
			}
		}
	}
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			if (is_isolated_room_p(y,x))
			{
				cave[y][x].info &= ~(CAVE_ROOM);
			}
		}
	}
}

/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
	bool quiet;

	/* Update stuff */
	if (!p_ptr->update) return;

	/*
	 * If quiet is set, this should calculate the changes but not comment
	 * on them. It should be included in any routines which print messages.
	 * The changes are still made, however.
	 */
	if (p_ptr->update & (PU_QUIET))
	{
		p_ptr->update &= ~(PU_QUIET);
		quiet = TRUE;
	}
	else
	{
		quiet = FALSE;
	}

	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		calc_bonuses(quiet);
	}

	if (p_ptr->update & (PU_MA_ARMOUR))
	{
		p_ptr->update &= ~(PU_MA_ARMOUR);
		calc_ma_armour();
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
		calc_mana(quiet);
	}

	if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		calc_spells(quiet);
	}


	/* Only making temporary changes, no screen updates */
	if (quiet) return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (screen_is_icky()) return;


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

	if (p_ptr->update & (PU_ROOM))
	{
		p_ptr->update &= ~(PU_ROOM);
		calc_rooms();
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
	if (screen_is_icky()) return;



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
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		p_ptr->redraw &= ~(PR_TIME);
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
	}

	if (p_ptr->redraw & (PR_TIME))
	{
		prt_time();
		p_ptr->redraw &= ~(PR_TIME);
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

	if (p_ptr->redraw & (PR_ENERGY))
	{
		p_ptr->redraw &= ~(PR_ENERGY);
		prt_energy();
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
	
 	if (p_ptr->redraw & (PR_SPIRIT))
 	{
 		p_ptr->redraw &= ~(PR_SPIRIT);
 		prt_spirit();
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
 * Return whether PW_INVEN is interesting
 */
static bool win_inven_good(void)
{
	int i;

	/* Boring without an inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* There is some inventory */
		if (inventory[i].k_idx) return TRUE;
	}

	/* There is no inventory */
	return FALSE;
}

/*
 * Return whether PW_EQUIP is interesting
 */
static bool win_equip_good(void)
{
	int i;

	/* Boring without equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* There is some equipment */
		if (inventory[i].k_idx) return TRUE;
	}
		
	/* There is no equipment */
	return FALSE;
}


/*
 * Return whether PW_SPELL is interesting
 */
static bool win_spell_good(void)
{
	/* Boring without any powers */
	return (mindcraft_powers[0].min_lev >
		skill_set[SKILL_MINDCRAFTING].value/2);
}




/*
 * Display the first player screen in a window.
 */
static void win_player_display(void)
{
	/* Display player */
	display_player(0);
}


/*
 * Display the third player screen in a window.
 */
static void win_player_skills_display(void)
{
	/* Display player */
	display_player(2);
}


/*
 * Return whether PW_MESSAGE is interesting
 */
static bool win_message_good(void)
{
	/* Boring without any messages */
	return (message_num() != 0);
}

/*
 * Display old messages in a window.
 */
static void win_message_display(void)
{
	int i;
	int w, h;
	int x, y;

	/* Get size */
	Term_get_size(&w, &h);

	/* Dump messages */
	for (i = 0; i < h; i++)
	{
		/* Dump the message on the appropriate line */
		Term_putstr(0, (h - 1) - i, -1, TERM_WHITE, message_str((short)i));

		/* Cursor */
		Term_locate(&x, &y);

		/* Clear to end of line */
		Term_erase(x, y, 255);
	}
}



/*
 * Return whether PW_OVERHEAD is interesting
 */
static bool win_overhead_good(void)
{
	/* The map's always different on the surface. */
	if (!dun_level) return TRUE;

	/* The map doesn't fit on the main display. */
	if (windows[0].term->wid < cur_wid) return TRUE;
	if (windows[0].term->hgt < cur_hgt) return TRUE;
	
	/* This is shown on the main display. */
	return FALSE;
}

/*
 * Display the overhead map in a window.
 */
static void win_overhead_display(void)
{
	int cy, cx;

	/* Redraw map */
	display_map(&cy, &cx, TRUE);
	
	/* Hack - also give the world map if the player is in a town. */
	if (!dun_level)
	{
		display_wild_map(cx+3);
	}
}


/*
 * PW_SHOPS is interesting if the player is in a town with shops.
 */
static bool win_shops_good(void)
{
	if (wild_grid[wildy][wildx].dungeon < MAX_TOWNS && !dun_level)
	{
		return shops_good(cur_town);
	}
	else
	{
		return FALSE;
	}
}

/*
 * Give a list of shops in the current town.
 */
static void win_shops_display(void)
{
	shops_display(cur_town);
}


/*
 * Return whether PW_MONSTER is interesting
 */
static bool win_monster_good(void)
{
	/* Boring without a selected monster */
	return (monster_race_idx != 0);
}

/*
 * Display monster recall in a window.
 */
static void win_monster_display(void)
{
	/* Display monster race info */
	if (monster_race_idx) display_roff(monster_race_idx);
}

/*
 * Return whether PW_OBJECT is interesting
 */
static bool win_object_good(void)
{
	/* Boring with a remembered object */
	return (object_kind_idx != 0);
}

/*
 * Display object kind recall in a window.
 */
static void win_object_display(void)
{
	/* Display object kind info */
	if (object_kind_idx) display_koff(object_kind_idx);
}



/*
 * Return whether PW_OBJECT_DETAILS is interesting.
 */
static bool win_object_details_good(void)
{
	object_type *o_ptr = cnv_idx_to_obj(object_idx);

	/* Non-objects are boring. */
	if (!o_ptr || !(o_ptr->k_idx)) return FALSE;

	/* Invisible floor objects are boring. */
	if (object_idx < 0 && !los(py, px, o_ptr->iy, o_ptr->ix)) return FALSE;

	/* Other objects are interesting. */
	return TRUE;
}	

static void win_object_details_display(void)
{
	object_type *o_ptr = cnv_idx_to_obj(object_idx);
	C_TNEW(o_name, ONAME_MAX, char);
	
	/* Never display non-objects. */
	if (!o_ptr || !(o_ptr->k_idx)) return;
	
	/* Never display invisible floor objects */
	if (object_idx < 0 && !los(py, px, o_ptr->iy, o_ptr->ix)) return;
	
	/* Describe fully. */
	identify_fully_aux(o_ptr, 2);
	
	/* Put the name at the top. */
	object_desc(o_name, o_ptr, TRUE, 3);
	Term_putstr(2, 0, Term->wid-2, TERM_WHITE, o_name);

	/* Put the character used at the top. */
	Term_putch(0, 0, object_attr(o_ptr), object_char(o_ptr));

	TFREE(o_name);
}

/* The option currently selected */
#define MAX_HELP_STRS	5
#define CUR_HELP_STR	help_str[help_strs]
static cptr help_str[MAX_HELP_STRS];
static int help_strs = 0;

/*
 * Remember or forget a help request. Only the latest one is
 * currently processed.
 */
void help_track(cptr str)
{
	/* Remove one. */
	if (!str)
	{
		if (help_strs) help_strs--;
	}
	else
	{
		/* Too many strings memorised. */
		if (help_strs++ == MAX_HELP_STRS-1)
		{
			int i;
			for (i = 1; i < MAX_HELP_STRS; i++)
			{
				help_str[i-1] = help_str[i];
			}
		}
	
		/* Set the current string */
		CUR_HELP_STR = str;
	}

	/* Window stuff */
	if (!is_keymap_or_macro()) p_ptr->window |= PW_HELP;
}

/*
 * Return whether PW_HELP is interesting.
 */
static bool win_help_good(void)
{
	/* There's a help hook present (should check for actual help). */
	return (help_strs != 0);
}

static cptr *help_files = NULL;

/*
 * Initialise the help_files[] array above.
 * Return false if the base help file was not found, true otherwise.
 */
static bool init_help_files(char *buf)
{
	int i;
	FILE *fff;
	cptr s,t;

	/* Open an index file. */
	path_build(buf, 1024, ANGBAND_DIR_HELP, syshelpfile);

	if (!((fff = my_fopen(buf, "r"))))
	{
		prt(format("Cannot open '%s'!", buf), Term->hgt/2, 0);
		return FALSE;
	}
		

	/* Count the file references. */
	for (i = 1; !my_fgets(fff, buf, 1024);)
	{
		for (s = buf; (s = strstr(s, "*****")); s += strlen("*****/a")) i++;
	}

	/* Create the help_files array. */
	help_files = C_NEW(i, cptr);

	/* Hack - The last element must be NULL. */
	help_files[--i] = NULL;

	/* Return to the start of the file. */
	fseek(fff, 0, SEEK_SET);

	/* Fill the help_files array. */
	while (!my_fgets(fff, buf, 1024))
	{
		for (s = buf; (s = strstr(s, "*****")); )
		{
			s += strlen("*****/a");
			t = strchr(s, '*');

			/* Paranoia. */
			if (!t) continue;

			/* Fill in the help_files array (backwards). */
			help_files[--i] = string_make(format("%.*s", t-s, s));
		}
	}

	my_fclose(fff);

	return TRUE;
}

void win_help_display(void)
{
	char buf[1024];
	FILE *fff;
	cptr *str;

	/* Nothing to show. */
	if (!help_str) return;

	/* Try to read the list of files at first. */
	if (!help_files && !init_help_files(buf)) return;

	/* Search every potentially relevant file (should use an index, but...) */
	for (str = help_files; *str; str++)
	{
		path_build(buf, 1024, ANGBAND_DIR_HELP, *str);
		
		/* No such file? */
		if (!((fff = my_fopen(buf, "r"))))
		{
			prt(format("Cannot open '%s'!", buf), Term->hgt/2, 0);
			return;
		}

		Term_gotoxy(0,0);

		while (!my_fgets(fff, buf, 1024))
		{
			/* Not an option heading. */
			if (strncmp(buf, CC_LINK_PREFIX, strlen(CC_LINK_PREFIX))) continue;

			/* Not this option heading. */
			if (!strstr(buf, format("<%s>", CUR_HELP_STR))) continue;

			while (!my_fgets(fff, buf, 1024) &&
				!prefix(buf, CC_LINK_PREFIX))
			{
				/* Print the line out in a possibly colourful way. */
				mc_roff(buf);
				
				/* Go to the next line. */
				roff("\n");
			}
			/* Only expect one match. */
			break;
		}
		
		my_fclose(fff);
	}
}

/*
 * Return whether PW_VISIBLE is interesting
 */
static bool win_visible_good(void)
{
	int i; 

	/* Hallucinating players find anything interesting. */
	if (p_ptr->image) return TRUE;

	/* Boring without any visible monsters. */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip unseen monsters */
		if (!m_ptr->ml) continue;

		/* A visible monster. */
		return TRUE;
	}

	/* No visible monsters. */
	return FALSE;
}

/* The following code to display visible monsters is taken from Eyangband 0.3.3
 * and uses a few strategic #defines to minimise the number of  changes needed
 * to do this.
 *
 * In actual fact, the only changes to the functions themselves are in the
 * definition of who[x].u_idx and the removal of the Term_clear().
 */

typedef struct monster_list_entry monster_list_entry;

struct monster_list_entry
{
	s16b r_idx;			/* Monster race index */
	s16b u_idx;			/* Unique index (for uniques) */

	byte amount;
};


#define get_lore_idx(idx, unused) (&(r_info[idx]))
#define get_monster_fake(idx, unused) (&(r_info[idx]))
#define monster_lore monster_race

/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
static bool ang_mon_sort_comp_hook(vptr u, vptr v, int a, int b)
{
	monster_list_entry *who = (monster_list_entry*)(u);

	u16b *why = (u16b*)(v);

	int r1 = who[a].r_idx;
	int r2 = who[b].r_idx;

	int z1, z2;

	/* Sort by player kills */
	if (*why >= 4)
	{
		/* Extract player kills */
		z1 = get_lore_idx(r1,who[a].u_idx)->r_pkills;
		z2 = get_lore_idx(r2,who[b].u_idx)->r_pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = get_lore_idx(r1,who[a].u_idx)->r_tkills;
		z2 = get_lore_idx(r2,who[b].u_idx)->r_tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = get_monster_fake(r1,who[a].u_idx)->level;
		z2 = get_monster_fake(r2,who[b].u_idx)->level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = get_monster_fake(r1,who[a].u_idx)->mexp;
		z2 = get_monster_fake(r2,who[b].u_idx)->mexp;

		/* Compare experience */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Compare indexes */
	return (r1 <= r2);
}

/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
static void ang_mon_sort_swap_hook(vptr u, vptr UNUSED v, int a, int b)
{
	monster_list_entry *who = (monster_list_entry*)(u);

	monster_list_entry holder;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}

/*
 * Display the visible monster list in a window.
 */
static void win_visible_display(void)
{
	int i, j;
	int c = 0;
	int items = 0;

	monster_list_entry *who;

	/* XXX Hallucination - no monster list */
	if (p_ptr->image)
	{		
		c_prt(TERM_WHITE,"You see a lot of pretty colours.",0,0);

		return;
	}

	/* Allocate the "who" array */
	C_MAKE(who, m_max, monster_list_entry);

	/* Count up the number visible in each race */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		bool found = FALSE;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip unseen monsters */
		if (!m_ptr->ml) continue;

		/* Increase for this race */
		if (items)
		{
			for (j = 0; j < items; j++)
			{
				if (who[j].r_idx == m_ptr->r_idx)
				{
					who[j].amount++;
					
					found = TRUE;

					break;
				}
			}
		}
		
		if (!found)
		{
			who[items].r_idx = m_ptr->r_idx;
			who[items].u_idx = !!(r_info[m_ptr->r_idx].flags1 & RF1_UNIQUE);
			who[items].amount = 1;

			items++;
		}

		/* Increase total Count */
		c++;
	}

	/* Are monsters visible? */
	if (items)
	{
		int w, h, num, len;
		u16b why = 1;
		cptr name;

		/* First, sort the monsters by expereince*/
		ang_sort_comp = ang_mon_sort_comp_hook;
		ang_sort_swap = ang_mon_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, items);

		/* Then, display them */
		(void)Term_get_size(&w, &h);

		/* Find the optimal width of one entry. */
		w = MAX(26, w/((items+h-2)/(h-1)));

		c_prt(TERM_WHITE,format("You can see %d monster%s", c, (c > 1 ? "s:" : ":")), 0, 0);

		/* Print the monsters in reverse order */
		for (i = items - 1, num = 0; i >= 0; i--, num++)
		{
			monster_lore *l_ptr = get_lore_idx(who[i].r_idx, who[i].u_idx);
			monster_race *r_ptr = get_monster_fake(who[i].r_idx, who[i].u_idx);

			/* Default Colour */
			byte attr = TERM_WHITE;

			/* Uniques */
			if (who[i].u_idx)
			{
				attr = TERM_L_RED;
			}

			/* Have we ever killed one? */
			if (l_ptr->r_tkills)
			{
				if (r_ptr->level > dun_depth)
				{
					attr = TERM_VIOLET;

					if (who[i].u_idx)
					{
						attr = TERM_RED;
					}
				}
			}
			else
			{
				if (!who[i].u_idx) attr = TERM_SLATE;
			}			
			
			/* Dump the monster character (not tracking shapechangers) */
			Term_putch((num / (h - 1)) * w, (num % (h - 1)) + 1, r_ptr->x_attr, r_ptr->x_char);


			/* Dump the monster name */
			if (who[i].amount == 1)
			{
				len = w-3;
			}
			else
			{
				len = w-3-strlen(format(" (x%d)", who[i].amount));
			}
			name = format("%.*s", len,
				monster_desc_aux(0, r_info+who[i].r_idx, who[i].amount, 0));

			if (who[i].amount != 1)
			{
				name = format("%s (x%d)", name, who[i].amount);
			}
			c_prt(attr, name, (num % (h - 1)) + 1, (num / (h - 1)) * w +2);
		}
	}
	else
	{
		c_prt(TERM_WHITE,"You see no monsters.",0,0);
	}

	/* XXX XXX Free the "who" array */
	FREE(who);
}

/* Allow window_stuff to be forced to prefer different choices rather than
 * stay the same. */
static bool window_stuff_rotate = FALSE;

#define PRI(a,b) (windows[a].pri[b])
#define REP(a,b) (windows[a].rep[b])

#define PRIORITY(a,b) ((old_window & 1<<(b)) ? REP(a,b) : PRI(a,b))

#define DISPLAY_NONE	(iilog(PW_NONE))
	
	typedef struct display_func_type display_func_type;

	struct display_func_type
	{
		bool (*good)(void);
		void (*display)(void);
	};
	
static display_func_type *display_func = NULL;

/*
 * Initialise the display_func array.
 */
static void init_window_stuff(void)
{
	int m;

	/* Paranoia - only run once */
	if (display_func) return;

	display_func = C_NEW(32, display_func_type);
	
	/* Set the default values. */
	for (m = 0; m < 32; m++)
	{
		display_func[m].good = func_false;
		display_func[m].display = func_nothing;
	}
	
	/*
	 * An array of display functions.
	 * "good" is true if there is something interesting to display.
	 * "display" actually displays the function in a clear window.
	 */
	/* Set for known display functions. */
	display_func[iilog(PW_INVEN)].good = win_inven_good;
	display_func[iilog(PW_INVEN)].display = display_inven;
	display_func[iilog(PW_EQUIP)].good = win_equip_good;
	display_func[iilog(PW_EQUIP)].display = display_equip;
	display_func[iilog(PW_SPELL)].good = win_spell_good;
	display_func[iilog(PW_SPELL)].display = display_spell_list;
	display_func[iilog(PW_PLAYER)].good = func_true;
	display_func[iilog(PW_PLAYER)].display = win_player_display;
	display_func[iilog(PW_PLAYER_SKILLS)].good = func_true;
	display_func[iilog(PW_PLAYER_SKILLS)].display = win_player_skills_display;
	display_func[iilog(PW_VISIBLE)].good = win_visible_good;
	display_func[iilog(PW_VISIBLE)].display = win_visible_display;
	display_func[iilog(PW_MESSAGE)].good = win_message_good;
	display_func[iilog(PW_MESSAGE)].display = win_message_display;
	display_func[iilog(PW_OVERHEAD)].good = win_overhead_good;
	display_func[iilog(PW_OVERHEAD)].display = win_overhead_display;
	display_func[iilog(PW_MONSTER)].good = win_monster_good;
	display_func[iilog(PW_MONSTER)].display = win_monster_display;
	display_func[iilog(PW_SHOPS)].good = win_shops_good;
	display_func[iilog(PW_SHOPS)].display = win_shops_display;
	display_func[iilog(PW_OBJECT)].good = win_object_good;
	display_func[iilog(PW_OBJECT)].display = win_object_display;
	display_func[iilog(PW_OBJECT_DETAILS)].good = win_object_details_good;
	display_func[iilog(PW_OBJECT_DETAILS)].display = win_object_details_display;
	display_func[iilog(PW_HELP)].good = win_help_good;
	display_func[iilog(PW_HELP)].display = win_help_display;
#if 0
	/* The following displays are defined but never used. */
	display_func[iilog(PW_SNAPSHOT)].good = func_false;
	display_func[iilog(PW_SNAPSHOT)].display = func_nothing;
	display_func[iilog(PW_BORG_1)].good = func_false;
	display_func[iilog(PW_BORG_1)].display = func_nothing;
	display_func[iilog(PW_BORG_2)].good = func_false;
	display_func[iilog(PW_BORG_2)].display = func_nothing;
#endif
}


/*
 * Handle "p_ptr->window"
 *
 * This makes the assumption that the main display will always be displayed
 * on term_screen. This is enforced elsewhere, but is not a necessary
 * assumption as the main display is not always of su
 */
void window_stuff(void)
{
	int m;
	
	u32b old_window = p_ptr->window & WINDOW_STUFF_MASK;
	
	/* Remember the original term (i.e. term_screen) */
	term *old = Term;

	/* Nothing to do */
	if (!old_window) return;

	/* Not initialised yet. */
	if (!display_func) init_window_stuff();

	/* Only process this display once. */
	p_ptr->window &= ~(old_window);

	/* Scan windows */
	for (m = 0; m < 8; m++)
	{
		window_type *w_ptr = &windows[m];

		/* Do nothing by default. */
		int n = DISPLAY_NONE;

		/* Skip non-existant windows */
		if (!w_ptr->term) continue;

		/* Hack - skip window containing main display */
		if (w_ptr->term == old) continue;

		/*
		 * If the "window_stuff_rotate" flag is set, find the first 
		 * "interesting" display after the current one which has a priority+
		 * greater than 0.
		 *
		 * Otherwise, find the "best" display according to several criteria,
		 * and display it. The criteria are:
		 *
		 * 1. Never show displays with a priority+ of 0
		 * 2. Prefer "interesting" displays to "boring" ones.
		 * 3. Prefer high priority ones to low priority ones.
		 * 4. Prefer changing windows to unchanging ones.
		 * 5. Prefer the current display, and others following it in order.
		 *
		 * + If a display is being changes at this invocation, the "rep"
		 * field is used to determine its priority. If not, the "pri" field
		 * is used.
		 */
		if (window_stuff_rotate)
		{
			n = w_ptr->current;
			for (n = (w_ptr->current+1)%32; n != w_ptr->current; n=(n+1)%32)
			{
				if (REP(m,n) == 0) continue; /* Don't stop at a priority of 0. */
				if ((*display_func[n].good)()) break; /* Stop at an interesting display. */
			}
			/* If there are no displays assigned to this window, do nothing. */
			if (REP(m,n) == 0) n = DISPLAY_NONE;
		}
		else
		{
			int i, n_good;
			
			/*
			 * Find an appropriate display by turning the qualities above
			 * which add to a display's goodness into numbers and comparing them.
			 * As n_good is initially less than 2, it can only fail to be replaced
			 * if the every display has a priority of 0.
			 */
			for (n_good = 0, i = w_ptr->current; i < w_ptr->current + 32; i++)
			{
				/* Decide whether display i is better than display n. */
				int i_good;
				
				i_good = 2*PRIORITY(m, i%32); /* 3 */

				if (!i_good) continue;	/* 1 (hack) */
				if (old_window & 1<<((i%32))) i_good++; /* 4 */
				if ((*display_func[i%32].good)()) i_good += 32; /* 2 */

				/* If display i is better, set n to i. */
				if (i_good > n_good)
				{
					n = i%32;
					n_good = i_good;
				}
			}
			/* If no positive priorities are found, do nothing. */
			if (!n_good) n = DISPLAY_NONE;
		}

		/* If different display is to be shown, show it. */
		if (n != w_ptr->current || old_window & 1<<n)
		{
			/* Set the current display. */
			if (n != w_ptr->current) w_ptr->current = n;

			/* Return to this routine next time. */
			if (n != DISPLAY_NONE) p_ptr->window |= PW_RETURN;

			/* Clear it. */
			Term_activate(w_ptr->term);
			clear_from(0);

			/* And draw it. */
			(*(display_func[w_ptr->current].display))();
			Term_fresh();
		}
	}

	/* Restore the original terminal. */
	Term_activate(old);

	/* Reset window_stuff_rotate. */
	window_stuff_rotate = FALSE;
}


/*
 * An entirely mis-named function. This causes all window flags with the same
 * window and non-boring quality to be rotated when window_stuff() is next
 * called.
 */
void toggle_inven_equip(void)
{
	/* Turn rotation on. */
	window_stuff_rotate = TRUE;

	/* Update everything. */
	p_ptr->window |= WINDOW_STUFF_MASK;
}

/*
 * Redraw the current term via a display function.
 */
void resize_window(void)
{
	int i;

	/* Hack - don't call during birth or level creation. */
	if (!character_dungeon) return;

	/* Paranoia - never call for term_screen */
	if (Term == term_screen) return;


	/* Hack -- react to changes (does this ever do anything?) */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Hack - find the window. */
	for (i = 0; i < ANGBAND_TERM_MAX; i++)
	{
		if (windows[i].term == Term)
		{
			/* Clear the window. */
			clear_from(0);

			/* Draw the window. */
			(*(display_func[windows[i].current].display))();

			/* Refresh */
			Term_fresh();

			return;
		}
	}
}



/*
 * Handle "p_ptr->update" and "p_ptr->redraw"
 */
void handle_stuff(void)
{
	/* Update stuff */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();
}


bool ma_empty_hands(void)
{
    return !(inventory[INVEN_WIELD].k_idx);
}

/*
 * Update the maxima for each skill, and possibly give chaos patron rewards
 */
static void update_skill_maxima(void)
{
	int i,chance;

	/* Broo might get a gift as they get better */
	if ((skill_set[SKILL_RACIAL].value > skill_set[SKILL_RACIAL].max_value) &&
		(p_ptr->prace == RACE_BROO))
	{
		chance = MAX(skill_set[SKILL_RACIAL].value,10);
		chance = MIN(chance,90);
		if (rand_int(100) < chance) gain_level_reward(0);
	}
	/* Anyone with a thaumaturgy skill has been calling out to chaos */
	if (skill_set[SKILL_THAUMATURGY].value > skill_set[SKILL_THAUMATURGY].max_value)
	{
		chance = MAX(skill_set[SKILL_THAUMATURGY].value,10);
		chance = MIN(chance,90);
		if (rand_int(100) < chance) gain_level_reward(0);
	}

	/* Now update all the maxima */
	for (i=0;i<MAX_SKILLS;i++)
	{
		if (skill_set[i].value > skill_set[i].max_value) skill_set[i].max_value = skill_set[i].value;
	}
}


/* Test whether a skill can be tested on the current level */
bool skill_check_possible(int index)
{
	return (dun_level && ((dun_depth) >= (((skill_set[index].value - skill_set[index].base) * 2) / 3)));
}

/* Give experience to a skill after usage */
void skill_exp(int index)
{
	/* No experience is gained on the surface */
	if (!dun_level) return;
	
	if ((cheat_wzrd) || (cheat_skll))
	{
		msg_format("Check %s skill, values %d/%d, exp %d/%d.",skill_set[index].name,
			skill_set[index].value, skill_set[index].base, skill_set[index].experience,
			skill_set[index].exp_to_raise);
	}

	if (!skill_check_possible(index))
	{
		if ((cheat_wzrd) || (cheat_skll))
		{
			msg_format("You are not tense enough to improve your %s skill.",skill_set[index].name);
		}
		return;
	}
	
	/* Debugging message */
	if ((cheat_wzrd) || (cheat_skll))
	{
		msg_format("Skill check for %s.",skill_set[index].name);
	}

	skill_set[index].experience+= 100; /* Means a player with an expfact of 100 has a 1-1 mapping */
	if(skill_set[index].experience >= skill_set[index].exp_to_raise * rp_ptr->r_exp)
	{

		/* Debugging message */
		if ((cheat_wzrd) || (cheat_skll))
		{
			msg_format("%s tested.",skill_set[index].name);
		}

		skill_set[index].experience-=skill_set[index].exp_to_raise * rp_ptr->r_exp;
		if(((byte)rand_int(100)>=(skill_set[index].value)) && (skill_set[index].value < 100))
		{
			skill_set[index].value++;
			calc_hitpoints(); /* The hit-points might have changed */
			calc_mana(FALSE); /* As might mana */
			calc_spells(FALSE); /* And spells */
			msg_format("%s %c%d%%->%d%%%c",skill_set[index].increase,
			(skill_check_possible(index) ? '(' : '['),skill_set[index].value-1,
			skill_set[index].value, (skill_check_possible(index) ? ')' : ']'));
			p_ptr->window |= PW_PLAYER_SKILLS; /* Window stuff */
			update_skill_maxima(); /* Update the maxima and possibly give rewards */
		}
	}
}

/* Give experience to spell skills for a spell */
void gain_spell_exp(magic_type *spell)
{
	bool check_mana = FALSE;
	int min_skill = spell->minskill * 2;
	switch(spell->sschool)
	{
	case SCH_THAUMATURGY:
		if (skill_set[SKILL_THAUMATURGY].value < min_skill + 50) {
			skill_exp(SKILL_THAUMATURGY);
			check_mana = TRUE;
		}
		break;
	case SCH_SORCERY:
		if (skill_set[SKILL_SORCERY].value < min_skill + 50) {
			skill_exp(SKILL_SORCERY);
			check_mana = TRUE;
		}
		break;
	case SCH_CONJURATION:
		if (skill_set[SKILL_CONJURATION].value < min_skill + 50) {
			skill_exp(SKILL_CONJURATION);
			check_mana = TRUE;
		}
		break;
	case SCH_NECROMANCY:
		if (skill_set[SKILL_NECROMANCY].value < min_skill + 50) {
			skill_exp(SKILL_NECROMANCY);
			check_mana = TRUE;
		}
	}
	switch(spell->stype)
	{
	case SP_CORPORIS:
		if (skill_set[SKILL_CORPORIS].value < min_skill + 50) {
			skill_exp(SKILL_CORPORIS);
			check_mana = TRUE;
		}
		break;
	case SP_NATURAE:
		if (skill_set[SKILL_NATURAE].value < min_skill + 50) {
			skill_exp(SKILL_NATURAE);
			check_mana = TRUE;
		}
		break;
	case SP_VIS:
		if (skill_set[SKILL_VIS].value < min_skill + 50) {
			skill_exp(SKILL_VIS);
			check_mana = TRUE;
		}
		break;
	case SP_ANIMAE:
		if (skill_set[SKILL_ANIMAE].value < min_skill + 50) {
			skill_exp(SKILL_ANIMAE);
			check_mana = TRUE;
		}
	}
	if (check_mana) skill_exp(SKILL_MANA);
}

/*
 * Return the energy used by casting a spell
 * This starts at 100 and then drops exponentially until
 * it reaches 10, then stops
 */
u16b spell_energy(u16b skill,u16b min)
{
	u32b en;
	
	/* Safety check to prevent overflows */
	if (min >= skill) 
	{
		/* Base calculation gives a square curve */
		en=TURN_ENERGY+((min-skill)*(min-skill)*TURN_ENERGY/100);
		if (en > 3*TURN_ENERGY) en = 3*TURN_ENERGY;
	}
	else
	{
		/* base calculation to give an inverse curve */
		en = 3*TURN_ENERGY/(skill-min);
		/* Force limits */
		if (en > TURN_ENERGY) en = TURN_ENERGY;
		if (en < TURN_ENERGY/10) en = TURN_ENERGY/10;
	}

	return (u16b)(en);
}

/*
 *
 * Combine the relevant skills for a given spell, then
 * divide the total by four to give an effective 'level'
 * of spellcasting
 *
 * This function always returns a minimum of 1
 * even if the skill levels are zero
 *
 */
byte spell_skill(magic_type *spell)
{
	byte total;
	switch(spell->sschool)
	{
	case SCH_THAUMATURGY:
		total = skill_set[SKILL_THAUMATURGY].value;
		break;
	case SCH_SORCERY:
		total = skill_set[SKILL_SORCERY].value;
		break;
	case SCH_CONJURATION:
		total = skill_set[SKILL_CONJURATION].value;
		break;
	case SCH_NECROMANCY:
		total = skill_set[SKILL_NECROMANCY].value;
		break;
	default:
		total = 0;
	}
	switch(spell->stype)
	{
	case SP_CORPORIS:
		total += skill_set[SKILL_CORPORIS].value;
		break;
	case SP_NATURAE:
		total += skill_set[SKILL_NATURAE].value;
		break;
	case SP_VIS:
		total += skill_set[SKILL_VIS].value;
		break;
	case SP_ANIMAE:
		total += skill_set[SKILL_ANIMAE].value;
		break;
	default:
		total = 0;
	}
	total= (total/4); /* This gives a total of 0-50 */
	if (total == 0) total++; /* So that we have a minimum of 1 */
	return (total);
}
