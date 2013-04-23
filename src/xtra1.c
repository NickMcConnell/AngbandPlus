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
static void day_to_date(int day,char *date)
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
 * A vstrnfmt_aux wrapper around day_to_date().
 */
void day_to_date_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int day = va_arg(*vp, int);
	if (max > strlen("10 Jan "))
	{
		day_to_date(day, buf);
	}
	else if (alert_failure)
	{
		msg_format("Only given %d characters for day_to_date(), %d needed!",
			max, strlen("10 Jan ")+1);
	}
}

/*
 * Converts stat num into a six-char (right justified) string
 */
static void cnv_stat(char *out_val, int val)
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

void cnv_stat_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int val = va_arg(*vp, int);

	/* Paranoia. */
	if (max < 7) return;

	cnv_stat(buf, val);
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
 * Translate a negative co-ordinate into one relative to the far edge of the
 * screen.
 */
static int get_y(const co_ord *t)
{
	if (t->y >= 0) return t->y;
	else return Term->hgt + t->y;
}

static int get_x(const co_ord *t)
{
	if (t->x >= 0) return t->x;
	else return Term->wid + t->x;
}

/* Shorten "put it where the table says it should go" for y,x functions. */
#define GET_YX(T) get_y(T), get_x(T)

static void prt_equippy(void)
{
    display_player_equippy(GET_YX(XY_EQUIPPY));
}

/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	char attr, colon;
	cptr name;

	/* Choose a colour (white >18/219, yellow reduced, light green normal. */
	if (p_ptr->stat_use[stat] > 18+219) attr = 'w';
	else if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat]) attr = 'y';
	else attr = 'G';
	
	/* Choose a name style (reduced or normal) */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
		name = stat_names_reduced[stat];
	else
		name = stat_names[stat];

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18+100)
	{
		colon = '!';
	}
	else
	{
		colon = ':';
	}

	/* Display name and number */
	mc_put_fmt(GET_YX(XY_STAT+stat), "%.3s%c  $%c%v",
		name, colon, attr, cnv_stat_f1, p_ptr->stat_use[stat]);
}

/*
 * Prints current gold
 */
static void prt_gold(void)
{
	mc_put_fmt(GET_YX(XY_GOLD), "AU $G%9ld", (long)p_ptr->au);
}

/*
 * Prints current time
 */
static void prt_time(void)
{
	int minute = ((turn % ((10L * TOWN_DAWN)) * 1440) / ((10L * TOWN_DAWN)));
	int hour = ((minute/60)+6)%24; /* 0 to 23 */
	int day = (turn + (10L * TOWN_DAWN / 4)) / (10L * TOWN_DAWN) + 1;

	/* Only keep loose minutes */
	minute = minute % 60;

	mc_put_fmt(GET_YX(XY_TIME), "%2d:%02d %v",
		hour, minute, day_to_date_f1, day+p_ptr->startdate);
}


/*
 * Prints current AC
 */
static void prt_ac(void)
{
	mc_put_fmt(GET_YX(XY_AC), "AC:    $G%5d", p_ptr->dis_ac+p_ptr->dis_to_a);
}

/*
 * Prints energy cost of last turn
 */
static void prt_energy(void)
{
	mc_put_fmt(GET_YX(XY_ENERGY), "LE:     $G%4d", old_energy_use);
}

/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	int x = get_x(XY_HP);
	int y = get_y(XY_HP);
	prt_nums("HP:", y, x, x+BORDER_WIDTH, p_ptr->chp, p_ptr->mhp);
}


/*
 * Prints players max/cur spell points and chi points
 */
static void prt_sp(void)
{
	int x = get_x(XY_SP);
	int y = get_y(XY_SP);
	prt_nums("SP:", y, x, x+BORDER_WIDTH, p_ptr->csp, p_ptr->msp);
	x = get_x(XY_CHI);
	y = get_y(XY_CHI);
	prt_nums("CH:", y, x, x+BORDER_WIDTH, p_ptr->cchi, p_ptr->mchi);
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
 */
static void prt_spirit(void)
{
	const int row_wild = get_y(XY_WILD_SPIRIT);
	const int col_wild = get_x(XY_WILD_SPIRIT);
	const int end_wild = col_wild+BORDER_WIDTH;
	const int row_life = get_y(XY_LIFE_SPIRIT);
	const int col_life = get_x(XY_LIFE_SPIRIT);
	const int end_life = col_life+BORDER_WIDTH;

	int	i, j;
	char	il[4];
	const int plev = MAX(1, skill_set[SKILL_SHAMAN].value/2);
	spirit_type	*s_ptr;
	put_str("Life", row_life, col_life);
	put_str("Wild", row_wild, col_wild);
	j=0;
	for (i=0;i<MAX_SPIRITS;i++)
	{
		s_ptr=&(spirits[i]);
		if(!(s_ptr->pact))
		{
			sprintf(il,"$d#");
		}
   		else if (s_ptr->minskill > plev)
		{
 			sprintf(il,"$D#");
 		}
 		else if(s_ptr->annoyance > 8)
		{
 			sprintf(il, "$r%c", I2A(j++));
 		}
 		else if(s_ptr->annoyance > 0)
		{
 			sprintf(il, "$y%c", I2A(j++));
 		}
 		else
		{
 			sprintf(il, "$G%c", I2A(j++));
 		}
		/* Should this check the sphere parameter? */
 		if (i % 2 == 0)
 			mc_put_str(row_life, end_life-MAX_SPIRITS+1+i, il);
 		else
 			mc_put_str(row_wild, end_wild-MAX_SPIRITS+i, il);
	}
}

/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	cptr depths;
	s16b level = dun_level + dun_offset;
	cptr descr[2][2] = {
	{"(Lev %d)", "%d ft"},
	{"/(Lev %d)\\", "/%d ft\\"}
	};
	if (depth_in_feet) level *= 50;

	if (dun_level)
	{
		depths = format(
			descr[!!(dun_defs[cur_dungeon].flags & DF_TOWER)][(int)depth_in_feet], level);
	}
	
	else if (wild_grid[wildy][wildx].dungeon < MAX_CAVES)
	{
		depths = dun_name+dun_defs[wild_grid[wildy][wildx].dungeon].shortname;
	}
	else
	{
		depths = format("Wild (%d,%d)",wildx,wildy);
	}
	/* Right-Adjust the "depth", and clear old values */
	mc_put_fmt(GET_YX(XY_DEPTH), "%9s", depths);
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	cptr str;
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT) str = "$rWeak";

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK) str = "$oWeak";

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT) str = "$yHungry";

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL) str = "";

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX) str = "$GFull";

	/* Gorged */
	else str = "$gGorged";

	mc_put_fmt(GET_YX(XY_HUNGRY), "%-8s", str);
}


/*
 * Prints Blind status
 */
static void prt_blind(void)
{
	if (p_ptr->blind)
	{
		c_put_str(TERM_ORANGE, "Blind", GET_YX(XY_BLIND));
	}
	else
	{
		put_str("     ", GET_YX(XY_BLIND));
	}
}


/*
 * Prints Confusion status
 */
static void prt_confused(void)
{
	if (p_ptr->confused)
	{
		c_put_str(TERM_ORANGE, "Confused", GET_YX(XY_CONFUSED));
	}
	else
	{
		put_str("        ", GET_YX(XY_CONFUSED));
	}
}


/*
 * Prints Fear status
 */
static void prt_afraid(void)
{
	if (p_ptr->afraid)
	{
		c_put_str(TERM_ORANGE, "Afraid", GET_YX(XY_AFRAID));
	}
	else
	{
		put_str("      ", GET_YX(XY_AFRAID));
	}
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(void)
{
	if (p_ptr->poisoned)
	{
		c_put_str(TERM_ORANGE, "Poisoned", GET_YX(XY_POISONED));
	}
	else
	{
		put_str("        ", GET_YX(XY_POISONED));
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
	c_put_str(attr, text, GET_YX(XY_STATE));
}


/*
 * Prints the speed of a character.			-CJS-
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed - 110;
	cptr change;

	/* Fast */
	if (i > 0) change = "$GFast";

	/* Slow */
	else if (i < 0) change = "$USlow";

	/* Normal" */
	else change = "$d";

	/* Display the speed */
	mc_put_fmt(GET_YX(XY_SPEED), "%-14v", vstrnfmt_fn, "%s (%+d)", change, i);
}

/*
 * Displays temporary resistance in preference to studying ability as there's not
 * much call for libraries in the dungeon.
 */

static void prt_study(void)
{
	if (p_ptr->oppose_acid || p_ptr->oppose_elec || p_ptr->oppose_fire || p_ptr->oppose_cold || p_ptr->oppose_pois) 
	{
		move_cursor(GET_YX(XY_STUDY));
		Term_addch(OPPOSE_COL(p_ptr->oppose_acid), 'A');
		Term_addch(OPPOSE_COL(p_ptr->oppose_elec), 'E');
		Term_addch(OPPOSE_COL(p_ptr->oppose_fire), 'F');
		Term_addch(OPPOSE_COL(p_ptr->oppose_cold), 'C');
		Term_addch(OPPOSE_COL(p_ptr->oppose_pois), 'P');
	}
	else if (p_ptr->new_spells)
	{
		put_str("Study", GET_YX(XY_STUDY));
	}
	else
	{
		put_str("     ", GET_YX(XY_STUDY));
	}
}


static void prt_cut(void)
{
	cptr str;
	int c = p_ptr->cut;

	if (c > 1000)
	{
		str = "$RMortal wound";
	}
	else if (c > 200)
	{
		str = "$rDeep gash   ";
	}
	else if (c > 100)
	{
		str = "$rSevere cut  ";
	}
	else if (c > 50)
	{
		str = "$oNasty cut   ";
	}
	else if (c > 25)
	{
		str = "$oBad cut     ";
	}
	else if (c > 10)
	{
		str = "$yLight cut   ";
	}
	else if (c)
	{
		str = "$yGraze       ";
	}
	else
	{
		str = "            ";
	}
	mc_put_str(GET_YX(XY_CUT), str);
}



static void prt_stun(void)
{
	cptr str;
	int s = p_ptr->stun;

	if (s > 100)
	{
		str = "$rKnocked out ";
	}
	else if (s > 50)
	{
		str = "$oHeavy stun  ";
	}
	else if (s)
	{
		str = "$oStun        ";
	}
	else
	{
		str = "            ";
	}
	mc_put_str(GET_YX(XY_STUN), str);
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

	cptr str, smb;
	int pct, len = 0;

	monster_type *m_ptr = &m_list[health_who];

	/* Default to almost dead */
	char attr = 'r';
	smb = "**********";

	/* Not tracking */
	if (!health_who)
	{
		/* Erase the health bar */
		str = "            ";
	}

	/* Tracking an unseen monster */
	else if (!m_list[health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		str = "[----------]";
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		str = "[----------]";
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		str = "[----------]";
	}

	/* Tracking a visible monster */
	else
	{
		str = "[----------]";

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;

		/* Badly wounded */
		if (pct >= 10) attr = 'R';

		/* Wounded */
		if (pct >= 25) attr = 'o';

		/* Somewhat Wounded */
		if (pct >= 60) attr = 'y';

		/* Healthy */
		if (pct >= 100) attr = 'G';

		/* Afraid */
		if (m_ptr->monfear) {
			attr = 'v';
			smb = "AFRAID****";
		}
		/* Asleep */
		if (m_ptr->csleep) {
			attr = 'B';
			smb = "SLEEPING**";
		}
		if (m_ptr->smart & SM_ALLY) {
			attr = 'U';
			smb = "ALLY******";
		}
		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;
	}

	/* Default to "unknown" */
	put_str(str, GET_YX(XY_INFO));

	/* Dump the current "health" (use '*' symbols) */
	mc_put_fmt(GET_YX(XY_INFO), "$%c%.*s", attr, len, smb);

#endif

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
	const s16b old_new_spells = p_ptr->new_spells;

	int			i, j, k;
	int			num_allowed, num_known;
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
	for (i=0; i<MAX_SCHOOL*MAX_SPELLS_PER_BOOK; i++)
	{
		/* Get the spell. */
		s_ptr = num_to_spell(i);

		/* Not a spell. */
		if (!s_ptr) continue;

		/* Count known spells */
		if (s_ptr->flags & MAGIC_LEARNED) num_known++;
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

		/* Get the spell. */
		s_ptr = num_to_spell(j);

		/* Skip spells we are allowed to know */
		if (s_ptr->min <= spell_skill(s_ptr)) continue;

		/* Forget it (if learned) */
		if (s_ptr->flags & MAGIC_LEARNED)
		{
			/* Mark as forgotten */
			s_ptr->flags |= MAGIC_FORGOT;

			/* No longer known */
			s_ptr->flags &= ~MAGIC_LEARNED;

			/* Message */
			if (!quiet)
				msg_format("You have forgotten the %s of %s.", p, s_ptr->name);

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
		s_ptr = num_to_spell(j);

		/* Skip unknown spells */
		if (j >= 255) continue;

		/* Get the spell. */
		s_ptr = num_to_spell(j);

		/* Forget it (if learned) */
		if (s_ptr->flags & MAGIC_LEARNED)
		{
			/* Mark as forgotten */
			s_ptr->flags |= MAGIC_FORGOT;

			/* No longer known */
			s_ptr->flags &= ~MAGIC_LEARNED;

			/* Message */
			if (!quiet)
				msg_format("You have forgotten the %s of %s.", p, s_ptr->name);

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

		/* Get the spell. */
		s_ptr = num_to_spell(j);

		/* Skip spells we cannot remember */
		if (s_ptr->min > spell_skill(s_ptr)) continue;

		/* First set of spells */
		if (s_ptr->flags & MAGIC_FORGOT)
		{
			/* No longer forgotten */
			s_ptr->flags &= ~MAGIC_FORGOT;

			/* Known once more */
			s_ptr->flags |= MAGIC_LEARNED;

			/* Message */
			if (!quiet)
				msg_format("You have remembered the %s of %s.", p, s_ptr->name);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Count spells that can be learned */
	for (j = k = 0; j < 128; j++)
	{
		/* Access the spell */
        s_ptr = num_to_spell(j);

		/* Skip spells we cannot remember */
		if (s_ptr->min > spell_skill(s_ptr)) continue;

		/* Skip spells we already know */
		if (s_ptr->flags & MAGIC_LEARNED) continue;

		/* Count it */
		k++;
	}


	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

    

	/* Spell count changed */
	if (old_new_spells != p_ptr->new_spells)
	{

		/* Message if needed and allowed. */
		if (p_ptr->new_spells && !quiet)
		{
			/* Message */
			msg_format("You can learn %d more %s%s.",
			           p_ptr->new_spells, p,
			           (p_ptr->new_spells != 1) ? "s" : "");
		}

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
bool PURE cumber_glove(object_ctype *o_ptr)
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
bool PURE cumber_helm(object_ctype *o_ptr)
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

#define MAX_SPELL_WEIGHT 300 /* Max weight for spellcasting */

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

	const bool old_cumber_armor = p_ptr->cumber_armor;
	const bool old_cumber_glove = p_ptr->cumber_glove;
	const bool old_cumber_helm = p_ptr->cumber_helm;

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
	max_wgt = MAX_SPELL_WEIGHT;

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
	if (old_cumber_glove != p_ptr->cumber_glove)
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
	}

	/* Take note when "helm state" changes */
	if (old_cumber_helm != p_ptr->cumber_helm)
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
	}

	/* Take note when "armor state" changes */
	if (old_cumber_armor != p_ptr->cumber_armor)
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
	const s16b old_cur_lite = p_ptr->cur_lite;

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
	if (old_cur_lite != p_ptr->cur_lite)
	{
		/* Update the lite */
		p_ptr->update |= (PU_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);
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

int PURE wield_skill(object_ctype *o_ptr)
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
		
		else if ((ma_empty_hands()) && !(p_ptr->ma_cumber_armour))
		{
			inventory[i].to_a = mystic_armour(i);
		}
		else
		{
			inventory[i].to_a = 0;
		}
	}

	/* Display the changes. */
	p_ptr->redraw |= PR_ARMOR;
	p_ptr->window |= PW_EQUIP | PW_PLAYER;
}


/*
 * Hack - determine if the player is immune to cuts.
 */
bool player_no_cut(void)
{
	if (p_ptr->prace == RACE_GOLEM) return TRUE;
	if (p_ptr->prace == RACE_SKELETON) return TRUE;
	if (p_ptr->prace == RACE_SPECTRE) return TRUE;
	if (p_ptr->prace == RACE_ZOMBIE && (skill_set[SKILL_RACIAL].value > 23))
		return TRUE;
	return FALSE;
}

/*
 * Hack - determine if the player is immune to stunning.
 */
bool player_no_stun(void)
{
	if (p_ptr->prace == RACE_GOLEM) return TRUE;
	return FALSE;
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

	const bool old_heavy_wield = p_ptr->heavy_wield;
	const bool old_heavy_shoot = p_ptr->heavy_shoot;

	object_type		*o_ptr;

	u32b		f1, f2, f3;

	const bool old_ma_cumber_armour = p_ptr->ma_cumber_armour;


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
	p_ptr->dis_to_a = 0;


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
	C_WIPE(p_ptr->sustain, A_MAX, bool);
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
	if (p_ptr->prace == RACE_HOBBIT) p_ptr->sustain[A_DEX] = TRUE;

	/* Gnome */
	if (p_ptr->prace == RACE_GNOME) p_ptr->free_act = TRUE;

	/* Dwarf */
	if (p_ptr->prace == RACE_DWARF) p_ptr->resist_blind = TRUE;

	/* Half-Orc */
	if (p_ptr->prace == RACE_HALF_ORC) p_ptr->resist_dark = TRUE;

	/* Half-Troll */
    if (p_ptr->prace == RACE_HALF_TROLL)
    {
        p_ptr->sustain[A_STR] = TRUE;
        if ((skill_set[SKILL_RACIAL].value/2)>14)
            {
                p_ptr->regenerate = TRUE;
                /* High level trolls heal fast... */
            }
            
    }

	/* Dunadan */
    if (p_ptr->prace == RACE_GREAT)
    {
            p_ptr->sustain[A_CON] = TRUE;
            p_ptr->regenerate = TRUE;  /* Great ones heal fast... */

    }

	/* High Elf */
	if (p_ptr->prace == RACE_HIGH_ELF) p_ptr->resist_lite = TRUE;
	if (p_ptr->prace == RACE_HIGH_ELF) p_ptr->see_inv = TRUE;

    if (p_ptr->prace == RACE_BARBARIAN) p_ptr->resist_fear = TRUE;
    else if (p_ptr->prace == RACE_HALF_OGRE)
    {   p_ptr->resist_dark = TRUE;
        p_ptr->sustain[A_STR] = TRUE;
    }
    else if (p_ptr->prace == RACE_HALF_GIANT)
    {
        p_ptr->sustain[A_STR] = TRUE;
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
        p_ptr->sustain[A_INT] = TRUE;
        p_ptr->sustain[A_WIS] = TRUE;
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

	/* Mystic get extra ac for armour _not worn_ */
	p_ptr->ma_cumber_armour = ma_heavy_armor();

	/* Calculate any martial arts AC now, so the effect can be counted below. */
	if (!p_ptr->ma_cumber_armour || !old_ma_cumber_armour)
	{
		calc_ma_armour();
		p_ptr->update &= ~(PU_MA_ARMOUR);
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
			p_ptr->ac += 5;
			p_ptr->dis_to_a += 5;
		}
		
		if (p_ptr->muta3 & MUT3_SCALES)
		{
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->ac += 10;
			p_ptr->dis_to_a += 10;
		}
		
		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
			p_ptr->stat_add[A_DEX] -= 1;
			p_ptr->ac += 25;
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
			p_ptr->sustain[A_CON] =TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 9)
				p_ptr->sustain[A_STR] = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 19)
				p_ptr->sustain[A_DEX] = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 29)
				p_ptr->sustain[A_WIS] = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 39)
				p_ptr->sustain[A_INT] = TRUE;
			if ((skill_set[SKILL_RACIAL].value/2) > 49)
				p_ptr->sustain[A_CHR] = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
			p_ptr->stat_add[A_CHR] = 0;
		}
	}

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
		if (f2 & (TR2_SUST_STR)) p_ptr->sustain[A_STR] = TRUE;
		if (f2 & (TR2_SUST_INT)) p_ptr->sustain[A_INT] = TRUE;
		if (f2 & (TR2_SUST_WIS)) p_ptr->sustain[A_WIS] = TRUE;
		if (f2 & (TR2_SUST_DEX)) p_ptr->sustain[A_DEX] = TRUE;
		if (f2 & (TR2_SUST_CON)) p_ptr->sustain[A_CON] = TRUE;
		if (f2 & (TR2_SUST_CHR)) p_ptr->sustain[A_CHR] = TRUE;

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->ac += o_ptr->to_a;

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
        p_ptr->ac += 20 + ((skill_set[SKILL_RACIAL].value/2) / 5);
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


		/* Calculate the index for the stat in use (see defines.h) */
		ind = ind_stat(use);

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
		p_ptr->ac += 100;
		p_ptr->dis_to_a += 100;
	}

    /* wraith_form */
    if (p_ptr->wraith_form)
	{
		p_ptr->ac += 100;
		p_ptr->dis_to_a += 100;
        p_ptr->reflect = TRUE;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->ac += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temprory shield */
	if (p_ptr->shield)
	{
		p_ptr->ac += 50;
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
		p_ptr->ac -= 10;
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
	else if(!p_ptr->ma_cumber_armour) /* So do other people with martial arts... */
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

	/* Negative infravision does not make sense. */
	if (p_ptr->see_infra < 0) p_ptr->see_infra = 0;

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
	p_ptr->ac += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
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
				else if (skill_set[SKILL_MISSILE].value>15)
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

            if (p_ptr->ma_cumber_armour)
                p_ptr->num_blow /= 2;

            p_ptr->num_blow += 60 + extra_blows;

            if (!p_ptr->ma_cumber_armour)
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
	if (old_heavy_shoot != p_ptr->heavy_shoot)
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
	}


	/* Take note when "heavy weapon" changes */
	if (old_heavy_wield != p_ptr->heavy_wield)
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
	}

	if (old_ma_cumber_armour != p_ptr->ma_cumber_armour)
	{
		/* No message */
		if (quiet);
		/* Message */
		else if (p_ptr->ma_cumber_armour)
			msg_print("The weight of your armor disrupts your balance.");
		else
			msg_print("You regain your balance.");
	}
}



/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;

	/* Squelch floor things. */
	if (p_ptr->notice & PN_FSQUELCH)
	{
		p_ptr->notice &= ~(PN_FSQUELCH);
		squelch_grid();
	}

	/* Squelch inventory things. */
	if (p_ptr->notice & PN_ISQUELCH)
	{
		p_ptr->notice &= ~(PN_ISQUELCH);
		squelch_inventory();
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

	/* Paranoia - avoid looking out of bounds. */
	if (!in_bounds(y, x)) return FALSE;

	for (i = 0; i < N_ELEMENTS(xs); i++)
	{
		if (cave[y+ys[i]][x+xs[i]].info & CAVE_ROOM) return FALSE;
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
	for (y = 1; y < cur_hgt-1; y++)
	{
		for (x = 1; x < cur_wid-1; x++)
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



	/* Hack -- clear the top line. */
	if (p_ptr->redraw & (PR_WIPE_0))
	{
		p_ptr->redraw &= ~(PR_WIPE_0);
		prt("", 0, 0);
	}

	/* Hack -- clear the rest of the screen */
	if (p_ptr->redraw & (PR_WIPE_1))
	{
		p_ptr->redraw &= ~(PR_WIPE_1);
		clear_from(1);
	}


	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
	}

    if (p_ptr->redraw & (PR_EQUIPPY))
    {
        p_ptr->redraw &= ~(PR_EQUIPPY);
        prt_equippy(); /* To draw / delete equippy chars */
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
	return (book_info[BK_MIND].info[0].min <=
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
	int i, h;

	/* Get size */
	Term_get_size(&i, &h);

	/* Dump messages */
	for (i = 0; i < h; i++)
	{
		/* Dump the message on the appropriate line */
		prt(message_str(i), (h-1)-i, 0);
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
	int x;

	/* Redraw map */
	display_map(0, 0, 0, &x);
	
	/* Hack - also give the world map if the player is in a town. */
	if (!dun_level)
	{
		display_wild_map(x+3);
	}
}


/*
 * PW_SHOPS is interesting if the player is in a town with shops.
 */
static bool win_shops_good(void)
{
	if (!dun_level && is_town_p(wildy,wildx))
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
	object_ctype *o_ptr = tracked_o_ptr;

	/* Non-objects are boring. */
	if (!o_ptr || !(o_ptr->k_idx)) return FALSE;

	/* Invisible floor objects are boring. */
	if (find_object(o_ptr) == OUP_FLOOR && !los(py, px, o_ptr->iy, o_ptr->ix))
	{
		return FALSE;
	}

	/* Other objects are interesting. */
	return TRUE;
}	

static void win_object_details_display(void)
{
	object_ctype *o_ptr = tracked_o_ptr;

	/* Never display non-objects. */
	if (!o_ptr || !(o_ptr->k_idx)) return;
	
	/* Never display invisible floor objects */
	if (find_object(o_ptr) == OUP_FLOOR && !los(py, px, o_ptr->iy, o_ptr->ix))
	{
		return;
	}
	
	/* Describe fully. */
	identify_fully_aux(o_ptr, 2);
	
	/* Put the name at the top. */
	mc_put_fmt(0, 0, "%v %v", get_symbol_f2, object_attr(o_ptr),
		object_char(o_ptr), object_desc_f3, o_ptr, TRUE, 3);
}

/* The option currently selected */
#define MAX_HELP_STRS	5
#define CUR_HELP_STR	help_str_list[help_strs]
static cptr help_str_list[MAX_HELP_STRS];
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
		if (help_strs == MAX_HELP_STRS-1)
		{
			int i;
			for (i = 1; i < MAX_HELP_STRS; i++)
			{
				help_str_list[i-1] = help_str_list[i];
			}
		}
		else
		{
			help_strs++;
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

/*
 * Display some help text. 
 */
void win_help_display(void)
{
	/* Nothing to show. */
	if (!help_strs) return;

	display_help_page(CUR_HELP_STR);
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

typedef struct monster_list_entry monster_list_entry;

struct monster_list_entry
{
	s16b r_idx;			/* Monster race index */
	byte amount;
};


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
		z1 = r_info[r1].r_pkills;
		z2 = r_info[r2].r_pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = r_info[r1].r_tkills;
		z2 = r_info[r2].r_tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = r_info[r1].level;
		z2 = r_info[r2].level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = r_info[r1].mexp;
		z2 = r_info[r2].mexp;

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
 * Dump a monster description to the screen.
 */
static void dump_race(int w, int h, int num, char attr, monster_list_entry *ptr)
{
	int x = num/(h-1)*w, y = num%(h-1)+1;

	monster_race *r_ptr = r_info+ptr->r_idx;
	int total = ptr->amount;
	byte flags = (total == 1) ? 0 : MDF_NUMBER;

	/* Dump the monster name. */
	mc_put_fmt(y, x, "%v $%c%.*v", get_symbol_f2, r_ptr->x_attr,
		r_ptr->x_char, attr, w-3, monster_desc_aux_f3, r_ptr, total, flags);
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
			who[items].amount = 1;

			items++;
		}

		/* Increase total Count */
		c++;
	}

	/* Are monsters visible? */
	if (items)
	{
		int w, h, num;
		u16b why = 1;

		/* First, sort the monsters by expereince*/
		ang_sort_comp = ang_mon_sort_comp_hook;
		ang_sort_swap = ang_mon_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, items);

		/* Then, display them */
		(void)Term_get_size(&w, &h);

		/* Find the optimal width of one entry. */
		w = MAX(26, w/((items+h-2)/(h-1)));

		mc_put_fmt(0, 0, "You can see %d monster%s.", c, (c > 1 ? "s:" : ":"));

		/* Print the monsters in reverse order */
		for (i = items - 1, num = 0; i >= 0; i--, num++)
		{
			monster_race *r_ptr = r_info+who[i].r_idx;

			/* Default Colour */
			char attr = 'w';

			/* Uniques */
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				attr = 'R';
			}

			/* Have we ever killed one? */
			if (r_ptr->r_tkills)
			{
				if (r_ptr->level > dun_depth)
				{
					attr = 'v';

					if (r_ptr->flags1 & RF1_UNIQUE)
					{
						attr = 'r';
					}
				}
			}
			else
			{
				if (!r_ptr->flags1 & RF1_UNIQUE) attr = 's';
			}			
			
			dump_race(w, h, num, attr, who+i);
		}
	}
	else
	{
		c_prt(TERM_WHITE,"You see no monsters.",0,0);
	}

	/* XXX XXX Free the "who" array */
	FREE(who);
}


/*
 * Check whether the floor display is "interesting".
 * True if the player character can see some floor at his feet.
 */
static bool win_floor_good(void)
{
	/* No floor to stand on. */
	if (!character_dungeon) return FALSE;

	/* An imaginary floor to stand on. */
	if (p_ptr->image) return TRUE;

	/* Not a real square. */
	if (!in_bounds2(tracked_co_ord.y, tracked_co_ord.x)) return FALSE;

	/* A visible wall. */
	return (cave[tracked_co_ord.y][tracked_co_ord.x].info & CAVE_MARK ||
		player_can_see_bold(tracked_co_ord.y, tracked_co_ord.x));
}


/*
 * Display a list of objects on the floor.
 *
 * The correct code here depends unpleasantly on that of target_set_aux().
 */
static void win_floor_display(void)
{
	cptr verb;

	if (tracked_co_ord.x == px && tracked_co_ord.y == py)
		verb = "are standing on";
	else
		verb = "see";

	if (p_ptr->image)
	{
		mc_put_fmt(0, 0, "You %s something strange.\n", verb);
	}
	else if (in_bounds2(tracked_co_ord.y, tracked_co_ord.x))
	{
		int y = 0;
		cave_type *c_ptr = &cave[tracked_co_ord.y][tracked_co_ord.x];
		monster_type *m_ptr = m_list+c_ptr->m_idx;
		monster_race *r_ptr = r_info+m_ptr->r_idx;
		object_type *o_ptr = o_list+c_ptr->o_idx;

		mc_put_fmt(y++, 0, "You %s %v.\n", verb, feature_desc_f2,
			c_ptr->feat, FDF_MIMIC | FDF_INDEF);

		if (c_ptr->m_idx && m_ptr->ml)
		{
			mc_put_fmt(y++, 0, "%v %v", get_symbol_f2, r_ptr->x_attr,
				r_ptr->x_char, monster_desc_f2, m_ptr, 0x0C);
		}

		for (; y < Term->hgt && o_ptr != o_list;
			o_ptr = o_list+o_ptr->next_o_idx)
		{
			if (!o_ptr->marked) continue;

			mc_put_fmt(y++, 0, "%v %v", get_symbol_f2, object_attr(o_ptr),
				object_char(o_ptr), object_desc_f3, o_ptr, TRUE, 3);
		}
	}
}


/*
 * The list of display functions.
 * This has an extra element to store PW_NONE, although it should not be set
 * by normal mechanisms.
 */
display_func_type display_func[NUM_DISPLAY_FUNCS+1] =
{
	{PW_INVEN, "inventory", win_inven_good, display_inven},
	{PW_EQUIP, "equipment", win_equip_good, display_equip},
	{PW_SPELL, "spell list", win_spell_good, display_spell_list},
	{PW_PLAYER, "character", func_true, win_player_display},
	{PW_PLAYER_SKILLS, "skills", func_true, win_player_skills_display},
	{PW_VISIBLE, "nearby monsters", win_visible_good, win_visible_display},
	{PW_MESSAGE, "messages", win_message_good, win_message_display},
	{PW_OVERHEAD, "overhead view", win_overhead_good, win_overhead_display},
	{PW_MONSTER, "monster recall", win_monster_good, win_monster_display},
	{PW_SHOPS, "shop names", win_shops_good, win_shops_display},
	{PW_OBJECT, "object recall", win_object_good, win_object_display},
	{PW_OBJECT_DETAILS, "object details", win_object_details_good,
		win_object_details_display},
	{PW_FLOOR, "floor information", win_floor_good, win_floor_display},
	{PW_HELP, "help", win_help_good, win_help_display},
	{PW_NONE, "", func_false, func_nothing},
};

/*
 * Choose what to display in a window after a "rotate" request, i.e.
 * the first "interesting" display after the current one which has a priority
 * greater than 0.
 *
 * If there are no suitable displays, it returns a blank display.
 */
static int window_rotate(const window_type *w_ptr)
{
	int c, n;
	for (c = 1; c < NUM_DISPLAY_FUNCS; c++)
	{
		n = (w_ptr->current+c)%NUM_DISPLAY_FUNCS;

		/* Don't stop at a priority of 0. */
		if (w_ptr->rep[n] == 0) continue; 

		/* Stop at any other interesting display. */
		if ((*display_func[n].good)()) return n;
	}
	return DISPLAY_NONE;
}

/*
 * Choose what to display in a window in a normal window_stuff() call according
 * to the following criteria:
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
static int window_best(const window_type *w_ptr, const u32b rep_mask)
{
	/* n is set if n_good != 0. */
	int i, UNREAD(n), i_good, n_good;

	/*
	 * Find an appropriate display by turning the qualities above
	 * which add to a display's goodness into numbers and comparing
	 * them. As n_good is initially less than 4, it can only fail to be
	 * replaced if the every display has a priority of 0.
	 */
	for (n_good = 0, i = 0; i < NUM_DISPLAY_FUNCS; i++)
	{
		display_func_type *d_ptr = display_func+i;

		/* Decide whether display i is better than display n. */
		if (rep_mask & d_ptr->flag)
			i_good = w_ptr->rep[i]*4;
		else
			i_good = w_ptr->pri[i]*4; /* 3 */

		if (!i_good) continue;	/* 1 (hack) */
		if (i >= w_ptr->current) i_good++; /* 5 */
		if (rep_mask & d_ptr->flag) i_good += 2; /* 4 */
		if ((*d_ptr->good)()) i_good += 64; /* 2 */

		/* If display i is better, set n to i. */
		if (i_good > n_good)
		{
			n = i;
			n_good = i_good;
		}
	}

	/* If no positive priorities are found, do nothing. */
	if (!n_good) n = DISPLAY_NONE;

	return n;
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
	int m, n;
	
	u32b old_window = p_ptr->window & WINDOW_STUFF_MASK;
	
	/* Remember the original term (i.e. term_screen) */
	term *old = Term;

	bool rotate = (old_window & PW_ROTATE) != 0;

	/* Nothing to do */
	if (!old_window) return;

	/* Only process this display once. */
	p_ptr->window &= ~(old_window);

	/* Scan windows */
	for (m = 0; m < 8; m++)
	{
		window_type *w_ptr = &windows[m];

		/* Skip non-existant windows */
		if (!w_ptr->term) continue;

		/* Hack - skip window containing main display */
		if (w_ptr->term == old) continue;

		if (rotate)
		{
			n = window_rotate(w_ptr);
		}
		else
		{
			n = window_best(w_ptr, old_window);
		}

		/* If different display is to be shown, show it. */
		if (n != w_ptr->current || old_window & display_func[n].flag)
		{
			display_func_type *d_ptr = display_func+n;

			/* Set the current display. */
			w_ptr->current = n;

			/* Return to this routine next time. */
			if (d_ptr->flag != PW_NONE) p_ptr->window |= PW_RETURN;

			/* Clear it. */
			Term_activate(w_ptr->term);
			clear_from(0);

			/* And draw it. */
			(*(d_ptr->display))();
			Term_fresh();
		}
	}

	/* Restore the original terminal. */
	Term_activate(old);
}


/*
 * An entirely mis-named function. This causes all window flags with the same
 * window and non-boring quality to be rotated when window_stuff() is next
 * called.
 */
void toggle_inven_equip(void)
{
	/* Rotate the displays. */
	p_ptr->window |= PW_ROTATE;
}

/*
 * Update various things in response to non-keypress events.
 */
void event_stuff(void)
{
	/* Call window_stuff() if the event affects it. */
	if (p_ptr->window & WINDOW_STUFF_MASK & ~(PW_RETURN))
	{
		window_stuff();
	}
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
	
	if (cheat_skll)
	{
		msg_format("Check %s skill, values %d/%d, exp %d/%d.",skill_set[index].name,
			skill_set[index].value, skill_set[index].base, skill_set[index].experience,
			skill_set[index].exp_to_raise);
	}

	if (!skill_check_possible(index))
	{
		if (cheat_skll)
		{
			msg_format("You are not tense enough to improve your %s skill.",skill_set[index].name);
		}
		return;
	}
	
	/* Debugging message */
	if (cheat_skll)
	{
		msg_format("Skill check for %s.",skill_set[index].name);
	}

	skill_set[index].experience+= 100; /* Means a player with an expfact of 100 has a 1-1 mapping */
	if(skill_set[index].experience >= skill_set[index].exp_to_raise * rp_ptr->r_exp)
	{

		/* Debugging message */
		if (cheat_skll)
		{
			msg_format("%s tested.",skill_set[index].name);
		}

		skill_set[index].experience-=skill_set[index].exp_to_raise * rp_ptr->r_exp;
		if(((byte)rand_int(100)>=(skill_set[index].value)) && (skill_set[index].value < 100))
		{
			skill_set[index].value++;

			/* Update other skill-related variables. */
			switch (index)
			{
				case SKILL_TOUGH: p_ptr->update |= PU_HP; break;
				case SKILL_MANA: p_ptr->update |= PU_SPELLS | PU_MANA; break;
				case SKILL_CHI: p_ptr->update |= PU_MANA; break;
				case SKILL_MA: p_ptr->update |= PU_MA_ARMOUR; break;
			}
			update_stuff();

			msg_format("%s %c%d%%->%d%%%c",skill_set[index].increase,
			(skill_check_possible(index) ? '(' : '['),skill_set[index].value-1,
			skill_set[index].value, (skill_check_possible(index) ? ')' : ']'));

			p_ptr->window |= PW_PLAYER_SKILLS; /* Window stuff */

			update_skill_maxima(); /* Update the maxima and possibly give rewards */
		}
	}
}

/*
 * Determine where an object is in terms of OUP_* flags.
 */
int find_object(object_ctype *o_ptr)
{
	int slot;
	/* Floor item. */
	if (o_ptr >= o_list && o_ptr < o_list+MAX_O_IDX)
	{
		return OUP_FLOOR;
	}
	/* Inventory item. */
	else if (o_ptr >= inventory && o_ptr < inventory+INVEN_TOTAL)
	{
		slot = o_ptr-inventory;
		if (slot <= INVEN_PACK) return OUP_INVEN;
		if (slot >= INVEN_POUCH_1 && slot <= INVEN_POUCH_6) return OUP_POUCH;
		if (slot >= INVEN_WIELD && slot <= INVEN_FEET) return OUP_EQUIP;
	}

	/* Unknown. */
	return 0;
}

/*
 * Update everything which needs to be updated after an object changes.
 */
void update_object(object_type *o_ptr, int where)
{
	/* Find the object if it is unknown. */
	if (!where && o_ptr) where = find_object(o_ptr);

	if (where & OUP_FLOOR)
	{
		/* Squelch the item if needed. */
		p_ptr->notice |= PN_FSQUELCH;

		/* Display the floor under the player, as the object may be there. */
		if (o_ptr && o_ptr->iy == py && o_ptr->ix == px) cave_track(py, px);
	}
	if (where & OUP_INVEN)
	{
		/* Put the object in the correct position. */
		p_ptr->notice |= PN_COMBINE | PN_REORDER | PN_ISQUELCH;

		/* Correct the speed, for if the weight has changed. */
		p_ptr->update |= PU_BONUS;

		/* Display the inventory window. */
		p_ptr->window |= PW_INVEN;
	}
	/* Pouches are . */
	if (where & OUP_POUCH)
	{
		/* Correct the speed, for if the weight has changed. */
		p_ptr->update |= PU_BONUS;

		/* Display the equipment window. */
		p_ptr->window |= PW_EQUIP;
	}
	if (where & OUP_EQUIP)
	{
		/* Update various item bonuses. */
		p_ptr->update |= PU_BONUS;

		/* Update separately for cumber_*() and armour weight. */
		p_ptr->update |= PU_MANA;

		/* Display the equipment window. */
		p_ptr->window |= PW_EQUIP;

		/* Display the player window, as some changes may not do this. (?) */
		p_ptr->window |= PW_PLAYER;
	}

	/* Hack - Give the changed object a valid stack number if appropriate.
	 * There may be a better place to put this. */
	if (o_ptr && !o_ptr->stack)
	{
		set_stack_number(o_ptr);
	}
}
