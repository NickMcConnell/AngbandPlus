/* File: birth.c */

/*
 * Character birth.
 *
 * Character histories, roll for stats and base hitpoints, determine
 * age/height/weight and starting gold.  Reset all character information.
 * Display birth options, quick-start, ask birth questions.  Roll and
 * auto-roll for stats.  Initialize and create a new character.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"

/*
 * Forward declare
 */
typedef struct birther birther;

/*
 * A structure to hold "rolled" information
 */
struct birther
{
	s16b age;
	s16b wt;
	s16b ht;
	s16b sc;

	s32b au;

	s16b stat[A_MAX];

	char history[250];
};


/*
 * Requested base stats
 */
static int stat_ask[A_MAX];

/*
 * Current stats (when rolling a character).
 */
static s16b stat_use[A_MAX];

/*
 * Order in which stats are assigned, from best to worst.
 */
static int stat_order[A_MAX];


/*
 * Forward declare
 */
typedef struct hist_type hist_type;

/*
 * Player background information
 */
struct hist_type
{
	cptr info;					/* Textual History */

	byte roll;					/* Frequency of this entry */
	byte chart;					/* Chart index */
	byte next;					/* Next chart index */
	byte bonus;					/* Social Class Bonus + 50 */
};


/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human/Dunadan/ -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *     Drúedain
 *   Elves         -->  7 -->  8 -->  9 --> 54 --> 55 --> 56
 *   Hobbit        --> 10 --> 11 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Gnome         --> 13 --> 14 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Dwarf         --> 16 --> 17 --> 18 --> 57 --> 58 --> 59 --> 60 --> 61
 *   Half-Orc      --> 19 --> 20 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Troll    --> 22 --> 23 --> 62 --> 63 --> 64 --> 65 --> 66
 *   Giant         --> 24 --> 67 --> 68 --> 69 --> 70 --> 71
 *   Ent           --> 80 --> 81 --> 82 -->83
 *
 * XXX XXX XXX This table *must* be correct or drastic errors may occur!
 */
static hist_type bg[] =
{
	{"You are the illegitimate and unacknowledged child ", 10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ", 20, 1, 2, 35},
	{"You are one of several children ", 95, 1, 2, 45},
	{"You are the first child ", 100, 1, 2, 50},

	{"of a Serf.  ", 40, 2, 3, 65},
	{"of a Yeoman.  ", 65, 2, 3, 80},
	{"of a Townsman.  ", 80, 2, 3, 90},
	{"of a Guildsman.  ", 90, 2, 3, 105},
	{"of a Landed Knight.  ", 96, 2, 3, 120},
	{"of a Titled Noble.  ", 99, 2, 3, 130},
	{"of a Royal Blood Line.  ", 100, 2, 3, 140},

	{"You are the black sheep of the family.  ", 20, 3, 50, 20},
	{"You are a credit to the family.  ", 80, 3, 50, 55},
	{"You were a well liked child.  ", 100, 3, 50, 60},

	{"Your mother was of the Teleri.  ", 40, 4, 1, 50},
	{"Your father was of the Teleri.  ", 75, 4, 1, 55},
	{"Your mother was of the Noldor.  ", 90, 4, 1, 55},
	{"Your father was of the Noldor.  ", 95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ", 98, 4, 1, 65},
	{"Your father was of the Vanyar.  ", 100, 4, 1, 70},

	{"You are one of several children ", 60, 7, 8, 50},
	{"You are the only child ", 100, 7, 8, 55},

	{"of a Telerin ", 75, 8, 9, 50},
	{"of a Noldo ", 95, 8, 9, 55},
	{"of a Vanya ", 100, 8, 9, 60},

	{"Ranger.  ", 40, 9, 54, 80},
	{"Archer.  ", 70, 9, 54, 90},
	{"Warrior.  ", 87, 9, 54, 110},
	{"Mage.  ", 95, 9, 54, 125},
	{"Prince.  ", 99, 9, 54, 140},
	{"King.  ", 100, 9, 54, 145},

	{"You are one of several children of a Hobbit ", 85, 10, 11, 45},
	{"You are the only child of a Hobbit ", 100, 10, 11, 55},

	{"Bum.  ", 20, 11, 3, 55},
	{"Tavern Owner.  ", 30, 11, 3, 80},
	{"Miller.  ", 40, 11, 3, 90},
	{"Home Owner.  ", 50, 11, 3, 100},
	{"Burglar.  ", 80, 11, 3, 110},
	{"Warrior.  ", 95, 11, 3, 115},
	{"Mage.  ", 99, 11, 3, 125},
	{"Clan Elder.  ", 100, 11, 3, 140},

	{"You are one of several children of a Gnome ", 85, 13, 14, 45},
	{"You are the only child of a Gnome ", 100, 13, 14, 55},

	{"Beggar.  ", 20, 14, 3, 55},
	{"Braggart.  ", 50, 14, 3, 70},
	{"Prankster.  ", 75, 14, 3, 85},
	{"Warrior.  ", 95, 14, 3, 100},
	{"Mage.  ", 100, 14, 3, 125},

	{"You are one of two children of a Dwarven ", 25, 16, 17, 40},
	{"You are the only child of a Dwarven ", 100, 16, 17, 50},

	{"Thief.  ", 10, 17, 18, 60},
	{"Prison Guard.  ", 25, 17, 18, 75},
	{"Miner.  ", 75, 17, 18, 90},
	{"Warrior.  ", 90, 17, 18, 110},
	{"Priest.  ", 99, 17, 18, 130},
	{"King.  ", 100, 17, 18, 150},

	{"You are the black sheep of the family.  ", 15, 18, 57, 10},
	{"You are a credit to the family.  ", 85, 18, 57, 50},
	{"You were a well liked child.  ", 100, 18, 57, 55},

	{"Your mother was an Orc, but it is unacknowledged.  ", 25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ", 100, 19, 20, 25},

	{"You are the adopted child ", 100, 20, 2, 50},
	{"Your mother was a Cave-Troll ", 30, 22, 23, 20},
	{"Your father was a Cave-Troll ", 60, 22, 23, 25},
	{"Your mother was a Hill-Troll ", 75, 22, 23, 30},
	{"Your father was a Hill-Troll ", 90, 22, 23, 35},
	{"Your mother was a Water-Troll ", 95, 22, 23, 40},
	{"Your father was a Water-Troll ", 100, 22, 23, 45},

	{"Cook.  ", 5, 23, 62, 60},
	{"Warrior.  ", 95, 23, 62, 55},
	{"Shaman.  ", 99, 23, 62, 65},
	{"Clan Chief.  ", 100, 23, 62, 80},

	{"You are the child of Hill Giant ", 30, 24, 67, 45},
	{"You are the child of Stone Giant ", 50, 24, 67, 50},
	{"You are the child of Fire Giant ", 80, 24, 67, 55},
	{"You are the child of Cloud Giant ", 96, 24, 67, 70},
	{"You are the child of Storm Giant ", 100, 24, 67, 85},

	{"You have dark brown eyes, ", 20, 50, 51, 50},
	{"You have brown eyes, ", 60, 50, 51, 50},
	{"You have hazel eyes, ", 70, 50, 51, 50},
	{"You have green eyes, ", 80, 50, 51, 50},
	{"You have blue eyes, ", 90, 50, 51, 50},
	{"You have blue-gray eyes, ", 100, 50, 51, 50},

	{"straight ", 70, 51, 52, 50},
	{"wavy ", 90, 51, 52, 50},
	{"curly ", 100, 51, 52, 50},

	{"black hair, ", 30, 52, 53, 50},
	{"brown hair, ", 70, 52, 53, 50},
	{"auburn hair, ", 80, 52, 53, 50},
	{"red hair, ", 90, 52, 53, 50},
	{"blond hair, ", 100, 52, 53, 50},

	{"and a very dark complexion.", 10, 53, 0, 50},
	{"and a dark complexion.", 30, 53, 0, 50},
	{"and an average complexion.", 80, 53, 0, 50},
	{"and a fair complexion.", 90, 53, 0, 50},
	{"and a very fair complexion.", 100, 53, 0, 50},

	{"You have light grey eyes, ", 85, 54, 55, 50},
	{"You have light blue eyes, ", 95, 54, 55, 50},
	{"You have light green eyes, ", 100, 54, 55, 50},

	{"straight ", 75, 55, 56, 50},
	{"wavy ", 100, 55, 56, 50},

	{"black hair, and a fair complexion.", 75, 56, 0, 50},
	{"brown hair, and a fair complexion.", 85, 56, 0, 50},
	{"blond hair, and a fair complexion.", 95, 56, 0, 50},
	{"silver hair, and a fair complexion.", 100, 56, 0, 50},

	{"You have dark brown eyes, ", 99, 57, 58, 50},
	{"You have glowing red eyes, ", 100, 57, 58, 60},

	{"straight ", 90, 58, 59, 50},
	{"wavy ", 100, 58, 59, 50},

	{"black hair, ", 75, 59, 60, 50},
	{"brown hair, ", 100, 59, 60, 50},

	{"a one foot beard, ", 25, 60, 61, 50},
	{"a two foot beard, ", 60, 60, 61, 51},
	{"a three foot beard, ", 90, 60, 61, 53},
	{"a four foot beard, ", 100, 60, 61, 55},

	{"and a dark complexion.", 100, 61, 0, 50},

	{"You have slime green eyes, ", 60, 62, 63, 50},
	{"You have puke yellow eyes, ", 85, 62, 63, 50},
	{"You have blue-bloodshot eyes, ", 99, 62, 63, 50},
	{"You have glowing red eyes, ", 100, 62, 63, 55},

	{"dirty ", 33, 63, 64, 50},
	{"mangy ", 66, 63, 64, 50},
	{"oily ", 100, 63, 64, 50},

	{"sea-weed green hair, ", 33, 64, 65, 50},
	{"bright red hair, ", 66, 64, 65, 50},
	{"dark purple hair, ", 100, 64, 65, 50},

	{"and green ", 25, 65, 66, 50},
	{"and blue ", 50, 65, 66, 50},
	{"and white ", 75, 65, 66, 50},
	{"and black ", 100, 65, 66, 50},

	{"ulcerous skin.", 33, 66, 0, 50},
	{"scabby skin.", 66, 66, 0, 50},
	{"leprous skin.", 100, 66, 0, 50},

	{"Outcasts.  ", 5, 67, 68, 30},
	{"Warriors.  ", 95, 67, 68, 60},
	{"War Leaders.  ", 99, 67, 68, 80},
	{"Clan Chiefs.  ", 100, 67, 68, 100},

	{"You have grey eyes, ", 30, 68, 69, 50},
	{"You have blue eyes, ", 60, 68, 69, 50},
	{"You have brown eyes, ", 90, 68, 69, 50},
	{"You have glowing eyes, ", 100, 68, 69, 55},

	{"long ", 50, 69, 70, 50},
	{"short ", 90, 69, 70, 45},
	{"wild ", 94, 69, 70, 50},
	{"flowing ", 98, 69, 70, 55},
	{"no hair, ", 100, 69, 71, 50},

	{"black hair, ", 60, 70, 71, 50},
	{"red hair, ", 70, 70, 71, 50},
	{"grey hair, ", 90, 70, 71, 50},
	{"silver hair, ", 100, 70, 71, 60},

	{"and a pale complexion.", 30, 71, 0, 50},
	{"and a fair complexion.", 85, 71, 0, 50},
	{"and a dark complexion.", 100, 71, 0, 50},

	{"Your father resembled an elm, ",   15, 80, 81, 50},
	{"Your father resembled a birch, ",  30, 80, 81, 50},
	{"Your father resembled an oak, ",   60, 80, 81, 50},
	{"Your father resembled a rowan, ",  70, 80, 81, 50},
	{"Your father resembled a yew, ",    85, 80, 81, 50},
	{"Your father resembled a cedar, ", 100, 80, 81, 50},

	{"and your mother's hair is the hue of corn.  ",  20, 81, 82, 50},
	{"and your mother tends gardens.  ",              60, 81, 82, 50},
	{"and you do not remember your mother.  ",       100, 81, 82, 50},

	{"Your bark is tough, ",        30, 82, 83, 50},
	{"Your branches are supple, ",  60, 82, 83, 50},
	{"Your leaves are green, ",    100, 82, 83, 50},

	{"and your decision to leave was considered hasty.",  60, 83, 0, 45},
	{"and you left after great deliberation.",           100, 83, 0, 55}
};



/*
 * Set up a stat priority table, or clear it
 */
static void prioritize_stats(bool prioritize)
{
	int i;

	/* DEBUG -- Turn off this code until we have more time to test it */
	prioritize = FALSE;

	/*
	 * When you turn on this code, also uncomment "p_ptr->birth_roll_requirement = "
	 * below.  Because it makes the autoroller (even) more effective, it would also
	 * be a good idea to re-balance manual and auto-rolling.  Perhaps a small maximum
	 * stat penalty for auto-rolled characters?  But then you need to tweak the
	 * rarity calcs...
	 */


	/* We want to set up the stat priority table */
	if (prioritize)
	{
		int j, stat, count;

		/* Randomize start and end points for the scan loop */
		int start = rand_int(A_MAX);
		int end = start + A_MAX;

		/* Scan all possible stat requests until the priority table is filled */
		for (i = 0, count = 0; count < A_MAX; i++)
		{
			/* Loop through the stats (random start to resolve ties) */
			for (j = start; j < end; j++)
			{
				/* Get stat */
				stat = j % A_MAX;

				/* Sort stats by increasing priority */
				if (stat_ask[stat] == i)
				{
					/* Save this stat, increment count */
					stat_order[A_MAX - 1 - (count++)] = stat;
				}
			}
		}
	}

	/* We want to clear it */
	else
	{
		for (i = 0; i < A_MAX; i++) stat_order[i] = -1;
	}
}


/*
 * Roll up character stats
 * Added ability to create an ordered list of stats. -CJN-
 *
 * The "priority" struct must contain a list of stat priorities (lower
 * is better).  If the struct contains ties, or values above A_MAX - 1,
 * these problems will be resolved by "calc_stat_rank"
 */
static void get_stats(void)
{
	int i, j, total;

	int stat, bonus;

	int dice[3 * A_MAX];
	int stats[A_MAX];

	int min = 7 * A_MAX;
	int max = 9 * A_MAX;


	/* Roll and verify some stats */
	while (TRUE)
	{
		/* Roll some dice */
		for (total = i = 0; i < 3 * A_MAX; i++)
		{
			/* Roll the dice */
			dice[i] = randint(3 + i % 3);

			/* Sum up totals */
			total += dice[i];
		}

		/* Require that totals fall within a set range */
		if ((total > min) && (total < max))
		{
			/* Each base stat ... */
			for (i = 0; i < A_MAX; i++)
			{
				/* ... is the sum of 5 + 1d3 + 1d4 + 1d5 */
				stats[i] = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];
			}

			/* Accept */
			break;
		}
	}

	/* We are using a stat priority array */
	if (stat_order[0] >= 0)
	{
		/* Sort the stats, highest to lowest */
		for (i = 0; i < A_MAX - 1; i++)
		{
			for (j = 0; j < A_MAX - 1; j++)
			{
				/* Bubble sort - descending values */
				if (stats[j] < stats[j + 1])
				{
					int tmp = stats[j];

					stats[j] = stats[j + 1];

					stats[j + 1] = tmp;
				}
			}
		}
	}

	/* Save and adjust stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Stats may or may not be prioritized */
		if (stat_order[0] >= 0) stat = stat_order[i];
		else                    stat = i;

		/* Save the base stat value */
		p_ptr->stat_max[stat] = stats[i];

		/* Get racial bonus to stat */
		bonus = rp_ptr->r_adj[stat];

		/* Start fully healed */
		p_ptr->stat_birth[stat] = p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];

		/* Apply the racial adjustment directly to maximum */
		stat_use[stat] = modify_stat(p_ptr->stat_max[stat], bonus);
	}
}

/*
 * Roll for some info that the auto-roller ignores.  -LM-
 *
 * No characters will have HPs at power level 100 that differ from the
 * average.
 * HPs will never differ from the average by more than 2 * character
 * hitdice on any level.
 */
void get_extra(void)
{
	int i;
	int min = 1, max = 1;

	/* Get hitdie */
	p_ptr->hitdie = rp_ptr->r_mhp;

	/* Initial hitpoints is the same as hitdie */
	p_ptr->player_hp[0] = p_ptr->mhp = p_ptr->hitdie;

	/* Final hitpoints = hitdie + (99 * hitdie / 4) */
	p_ptr->player_hp[PY_MAX_POWER - 1] =
		p_ptr->hitdie + ((PY_MAX_POWER-1) * p_ptr->hitdie / 4);


	/* Roll out the intervening hitpoints */
	for (i = 1; i < PY_MAX_POWER - 1; i++)
	{
		/* Expected previous level's HPs */
		int average = p_ptr->hitdie + ((i-1) * p_ptr->hitdie / 4);

		/* Difference between previous level's HPs and the average */
		int diff = p_ptr->player_hp[i-1] - average;

		/* Near the end, we allow less difference */
		if (i >= PY_MAX_POWER - 8) diff *= 100;

		/* Make adjustments near the end or where necessary */
		if ((diff) &&
		    ((i >= (PY_MAX_POWER - 8)) ||
		     (rand_int(p_ptr->hitdie * 2) < ABS(diff))))
		{
			/* Need to catch up (a lot) */
			if (diff <= -(p_ptr->hitdie))
			{
				min = div_round(p_ptr->hitdie, 2) - 1;
				max = div_round(p_ptr->hitdie, 2) - 1;
			}

			/* Need to catch up (a little) */
			else if (diff < 0)
			{
				min = 1 + div_round(ABS(diff), 3);
				max = div_round(p_ptr->hitdie, 2) - 1;
			}

			/* Need to slow down (a lot) */
			else if (diff >= p_ptr->hitdie)
			{
				min = 1;
				max = 1;
			}

			/* Need to slow down (a little) */
			else
			{
				min = 1;
				max = div_round(p_ptr->hitdie, 2) - 1 -
				      div_round(ABS(diff), 3);
			}
		}

		/* Allow less variance near the end */
		else if (i >= PY_MAX_POWER - 4)
		{
			min = 2;
			max = div_round(p_ptr->hitdie, 2) - 2;
			if (max < min) max = min = div_round(p_ptr->hitdie, 4);
		}

		/* Usual per-level gain */
		else
		{
			min = 1;
			max = div_round(p_ptr->hitdie, 2) - 1;
		}

		/* Add this level's HP gain to the previous level's HPs. */
		p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + rand_range(min, max);
	}
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int i, n, chart, roll, social_class;

	char *s;

	char buf[240];


	/* Clear the previous history string */
	p_ptr->history[0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint(4);

	/* Starting place */
	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
		case RACE_DUNADAN:
		case RACE_WOSES:
		case RACE_BEORNING:
		{
			chart = 1;
			break;
		}

		case RACE_ELF:
		case RACE_HIGH_ELF:
		case RACE_DARK_ELF:
		{
			chart = 7;
			break;
		}

		case RACE_HOBBIT:
		{
			chart = 10;
			break;
		}

		case RACE_GNOME:
		{
			chart = 13;
			break;
		}

		case RACE_DWARF:
		{
			chart = 16;
			break;
		}

		case RACE_HALF_ORC:
		{
			chart = 19;
			break;
		}

		case RACE_HALF_TROLL:
		{
			chart = 22;
			break;
		}

		case RACE_GIANT:
		{
			chart = 24;
			break;
		}

		case RACE_ENT:
		{
			chart = 80;
			break;
		}

		default:
		{
			chart = 0;
			break;
		}
	}

	/* Process the history */
	while (chart)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint(100);

		/* Access the proper entry in the table */
		while ((chart != bg[i].chart) || (roll > bg[i].roll)) i++;

		/* Acquire the textual history */
		(void)strcat(buf, bg[i].info);

		/* Add in the social class */
		social_class += (int)(bg[i].bonus) - 50;

		/* Enter the next chart */
		chart = bg[i].next;
	}



	/* Verify social class */
	if      (social_class > 100) social_class = 100;
	else if (social_class < 1)   social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;


	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */ ;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n - 1] == ' ')) s[--n] = '\0';


	/* Copy the history */
	(void)my_strcpy(p_ptr->history, buf, sizeof(p_ptr->history));
}


/*
 * Computes character's age, height, and weight.
 *
 * Weight is dependant on:
 *    Race/Gender-based "stockiness",
 *    height, and
 *    a random modifier ("chubbiness")
 */
static void get_ahw(void)
{
	int base_height = 0, base_weight = 0, mod_height = 0, mod_weight = 0;
	long tmp;

	/* Calculate the age */
	p_ptr->age = rand_range(rp_ptr->b_age, rp_ptr->b_age + rp_ptr->m_age);


	/* Calculate various height and weight values for males */
	if (p_ptr->psex == SEX_MALE)
	{
		base_height = rp_ptr->m_b_ht;
		base_weight = rp_ptr->m_b_wt;

		mod_height = rp_ptr->m_m_ht;
		mod_weight = rp_ptr->m_m_wt;
	}

	/* Calculate various height and weight values for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		base_height = rp_ptr->f_b_ht;
		base_weight = rp_ptr->f_b_wt;

		mod_height = rp_ptr->f_m_ht;
		mod_weight = rp_ptr->f_m_wt;
	}

	/* Calculate the actual height */
	p_ptr->ht = Rand_normal(base_height, mod_height);

	/* Calculate standard weight for the character's height. -LM- */
	tmp = (long)p_ptr->ht * p_ptr->ht * base_weight /
	      (long)(base_height * base_height);
	p_ptr->wt = (s16b)tmp;

	/* Apply random modifier. */
	p_ptr->wt = Rand_normal(p_ptr->wt, mod_weight);
}


/*
 * Get the player's starting money
 */
static void get_money(void)
{
	int i, gold;

	/* Social class determines starting gold */
	gold = (p_ptr->sc * 5) + rand_range(100, 150);

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* High charisma increases starting gold */
		if (i == A_CHR)
		{
			if (stat_use[A_CHR] > 18) gold += 20 * (stat_use[A_CHR] - 18);
		}

		/* High values for other stats decrease starting gold */
		else
		{
			if (stat_use[i] > 18) gold -= 20 * (stat_use[i] - 18);
		}
	}

	/* Minimum 75 gold */
	if (gold < 75) gold = 75;

	/* Save the gold */
	p_ptr->au_birth = p_ptr->au = gold;
}

/*
 * Clear all the global "character" data
 *
 * This function must be updated with everything, and is troublesome to
 * maintain. XXX XXX XXX
 */
void player_wipe(bool full)
{
	int i;
	int y, x;

	/* Hack -- no player in dungeon */
	if (full) cave_m_idx[p_ptr->py][p_ptr->px] = 0;

	/* Wipe the player */
	WIPE(p_ptr, player_type);

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}


	/* Start with no artifacts made yet */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		a_ptr->cur_num = 0;
	}

	/* Reset the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Reset level */
		if (q_ptr->type == QUEST_FIXED)
		{
			q_ptr->active_level = q_ptr->base_level;
			q_ptr->cur_num = 0;
		}
		else
		{
			q_ptr->type = 0;
			q_ptr->r_idx = 0;
			q_ptr->base_level = 0;
			q_ptr->active_level = 0;
			q_ptr->cur_num = 0;
			q_ptr->max_num = 0;
			q_ptr->reward = 0;
		}
	}

	/* Reset the "objects" */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset object kind lore flags */
		k_ptr->special = 0;
	}


	/* Reset the "monsters" */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 1;

		/* Clear monster memory */
		if (!character_existed) WIPE(l_ptr, monster_lore);
		else                    l_ptr->pkills = 0;
	}

	/* Hack -- normal luck */
	p_ptr->luck = 100;

	/* None of the spells have been cast yet */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		p_ptr->spell_flags[i] = 0;
	}

	/* No stealing */
	num_recent_thefts = 0;

	/* Character type is always normal to begin with -- except for ironman players */
    if (ironman_play) p_ptr->character_type = PCHAR_IRONMAN;
    else              p_ptr->character_type = PCHAR_NORMAL;

	/* And does not use multiple lives  XXX */
	op_ptr->opt[OPT_beginner_play] = FALSE;

	/* The below is only done when needed */
	if (!full) return;

	/* Clear flags and flow information. */
	for (y = 0; y < DUNGEON_HGT_MAX; y++)
	{
		for (x = 0; x < DUNGEON_WID_MAX; x++)
		{
			/* No flags */
			cave_info[y][x] = 0;

			cave_m_idx[y][x] = 0;
			cave_o_idx[y][x] = 0;

			/* No flow */
			cave_cost[y][x] = 0;
			cave_when[y][x] = 0;
		}
	}

	/* Hack -- seed for flavors */
	seed_flavor = rand_int(0x10000000);

	/* Hack -- seed for town layout */
	seed_town = rand_int(0x10000000);

	/* Wipe the objects */
	wipe_o_list();

	/* Wipe the monsters */
	wipe_m_list();
}



/*
 * Show descriptions of the current birth options on screen.
 *
 * They have to be quite terse - there's not much space...
 */
static void desc_birth_options(void)
{
	int i;
	byte a = TERM_L_BLUE;

	char buf[DESC_LEN];


	/* Clear from x = 50, between y = 20 and y = 24 */
	for (i = 19; i <= 24; i++) (void)Term_erase(50, i, 255);

	/* Display character type */
	c_prt(TERM_L_BLUE, character_type_name[p_ptr->character_type], 3, 10);


	/* Get a title, center it */
	center_string(buf, sizeof(buf), "Birth Options:", 26);

	/* Move cursor to start of first line */
	move_cursor(19, 50);

	/* Print out title */
	c_roff(TERM_WHITE, format("%s\n", buf), 50, display_width());

	/* Display character type details */
	c_roff(a, format("-%s\n", character_type_desc[p_ptr->character_type]), 50, display_width());

	/* Print out option descriptions */
	if (birth_autoroll) c_roff(a, "-Using the autoroller.\n", 50, display_width());
	else c_roff(a, "-Using manual rolling.\n", 50, display_width());
	if (birth_no_stores) c_roff(a, "-Cannot use stores.\n", 50, display_width());
	if (birth_no_artifacts) c_roff(a, "-No artifacts.\n", 50, display_width());
	if ((p_ptr->character_type != PCHAR_IRONMAN) && birth_no_return_stair)
		c_roff(a, "-No stairs back.\n", 50, display_width());
	if (birth_smart_cheat) c_roff(a, "-Monsters cheat-learn.\n", 50, display_width());
}


/*
 * Use existing character information.  -EZ-
 */
static bool player_birth_quick(void)
{
	int i, bonus;
	birther old_char;
	byte old_race;
	byte old_sex;
	byte old_hitdie;
	s16b old_hp[PY_MAX_POWER];
	s32b old_birth_roll_requirement;

	/* Check data */
	if ((!character_loaded) || (!p_ptr->hitdie))
	{
		/* Wipe the player */
		player_wipe(TRUE);

		/* Require a valid savefile */
		if (load_player(TRUE)) return (FALSE);
	}

	/* Store old data */
	old_sex = p_ptr->psex;
	old_race = p_ptr->prace;

	old_hitdie = p_ptr->hitdie;

	old_char.age = p_ptr->age;
	old_char.au = p_ptr->au_birth;
	old_char.ht = p_ptr->ht;
	old_char.wt = p_ptr->wt;
	old_char.sc = p_ptr->sc;

	old_birth_roll_requirement = p_ptr->birth_roll_requirement;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		old_char.stat[i] = p_ptr->stat_birth[i];
	}

	/* Save the history */
	strcpy(old_char.history, p_ptr->history);

	/* Save the hp */
	for (i = 0; i < PY_MAX_POWER; i++)
	{
		old_hp[i] = p_ptr->player_hp[i];
	}


	/* Wipe the player */
	player_wipe(TRUE);


	/* Load various data */
	p_ptr->psex = old_sex;
	p_ptr->prace = old_race;

	p_ptr->hitdie = old_hitdie;

	p_ptr->age = old_char.age;
	p_ptr->au_birth = p_ptr->au = old_char.au;
	p_ptr->ht = old_char.ht;
	p_ptr->wt = old_char.wt;
	p_ptr->sc = old_char.sc;

	p_ptr->birth_roll_requirement = old_birth_roll_requirement;


	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Restore stat */
		p_ptr->stat_birth[i] = old_char.stat[i];

		/* Start fully healed */
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = p_ptr->stat_birth[i];

		/* Get racial bonus to stat */
		bonus = rp_ptr->r_adj[i];

		/* Apply the racial bonuses */
		p_ptr->stat_use[i] = modify_stat(p_ptr->stat_max[i], bonus);
	}

	/* Start with some experience  XXX */
	p_ptr->exp = 3;


	/* Load the history */
	strcpy(p_ptr->history, old_char.history);

	p_ptr->power = 1;

	/* Load the hp */
	for (i = 0; i < PY_MAX_POWER; i++)
	{
		p_ptr->player_hp[i] = old_hp[i];
	}

	/* Hack -- seed for flavors */
	seed_flavor = rand_int(0x10000000);

	/* Hack -- seed for town layout */
	seed_town = rand_int(0x10000000);

	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Accept */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to interactively select a sex and race,
 * modify birth options, quick start, consult on-line help, undo choices and
 * start over, load another savefile, or quit the game.
 */
static int player_birth_aux_1(void)
{
	int k, i, question;
	int n;

	cptr str;

	char ch;
	byte a = TERM_WHITE;

	char p2 = ')';

	char buf[DESC_LEN];

	bool dummy;

	/* Allow quick starts if an ancestor (alive or dead) is available */
	bool can_quick_start = character_existed;

	/* Clear screen */
	(void)Term_clear();

	/* Get a title, center it */
	center_string(buf, sizeof(buf), format("Create a %s Character", VERSION_NAME), display_width());

	/* Print it out */
	prt(buf, 0, 0);

	/* Blank spacer line */

	/* Display type, gender, race */
	put_str("Type   :", 3, 1);
	put_str("Gender :", 4, 1);
	put_str("Race   :", 5, 1);

	/* Print out help text */
	if (can_quick_start)
		c_put_str(a, "  *) Quick-start based on previous character     ", 20, 1);
	c_put_str(a, "  =) Birth options    P) Play another character   ", 21, 1);
	c_put_str(a, "  S) Start over       Q) Quit the game           ", 22, 1);
	c_put_str(a, "ESC) Go back          ?) On-line help            ", 23, 1);


	/* No questions asked */
	question = 0;

	/* Repeat until all questions asked */
	while (TRUE)
	{
		/* Clear some lines */
		for (i = 6; i < 20; i++) clear_row(i);

		/* Print descriptions of the current birth options */
		desc_birth_options();

		/* Reset */
		k = 0;
		n = 0;

		/* Gender */
		if (question == 0)
		{
			/* Extra info */
			move_cursor(7, 0);
			c_roff_centered(TERM_WHITE, "Males weigh slightly more than females, but otherwise your character's gender has no significant gameplay effects.", 5, 75);

			/* Show genders */
			for (n = 0; n < MAX_SEXES; n++)
			{
				/* Analyze */
				p_ptr->psex = n;
				sp_ptr = &sex_info[p_ptr->psex];
				str = sp_ptr->title;

				/* Display */
				(void)strnfmt(buf, sizeof(buf), "%c%c %s", I2A(n), p2, str);
				put_str(buf, 10 + (n / 5), 2 + 15 * (n % 5));
			}

			/* Build a prompt */
			(void)strnfmt(buf, sizeof(buf), "%c-%c) Choose a gender", I2A(0), I2A(n-1));

			/* Fill in help text */
			if (!can_quick_start)
				c_put_str(a, format("%s  *) Choose at random        ", buf), 19, 1);
			else
				c_put_str(a, format("%s                             ", buf), 19, 1);


		}

		/* Race */
		else if (question == 1)
		{
			move_cursor(7, 0);
			c_roff_centered(TERM_WHITE, "Your race determines various intrinsic factors and bonuses.", 5, 75);

			/* Show races */
			for (n = 0; n < MAX_RACES; n++)
			{
				/* Analyze */
				p_ptr->prace = n;
				rp_ptr = &race_info[p_ptr->prace];
				str = rp_ptr->title;

				/* Display */
				(void)strnfmt(buf, sizeof(buf), "%c%c %s", I2A(n), p2, str);
				put_str(buf, 10 + (n / 5), 2 + 15 * (n % 5));
			}

			/* Build a prompt */
			(void)strnfmt(buf, sizeof(buf), "%c-%c) Choose a race  ", I2A(0), I2A(n-1));

			/* Fill in help text */
			c_put_str(a, format("%s  *) Choose at random        ", buf), 19, 1);


		}

		/* Get a command and process it */
		while (TRUE)
		{
			/* Move the cursor */
			if (question == 0)      move_cursor(4, 10);
			else if (question == 1) move_cursor(5, 10);


			ch = inkey(FALSE);

			/* Quit */
			if ((ch == 'Q') || (ch == 'q')) quit(NULL);

			/* Start over */
			else if ((ch == 'S') || (ch == 's')) return (0);

			/* Escape */
			else if (ch == ESCAPE)
			{
				/* Hack -- Go to previous question */
				if (question > 0) question--;

				/* Hack -- allow quick_starts if appropriate */
				if ((question == 0) && (character_loaded))
				{
					can_quick_start = TRUE;
					c_put_str(a, "  *) Quick-start based on previous character     ",
						20, 1);
				}

				/* Refresh */
				break;
			}

			/* Load another character, start over */
			else if ((ch == 'P') || (ch == 'p'))
			{
				/* Hack -- Cancel everything */
				return (-1);
			}

			/* Change options */
			else if (ch == '=')
			{
				/* Save screen */
				screen_save(TRUE);

				/* Interact with options -- birth options only */
				do_cmd_options_aux(4, "Birth/Difficulty Options", &dummy);

				/* Load screen */
				screen_load();

				/* Print descriptions of the current birth options */
				desc_birth_options();

				continue;
			}

			/* Help me. */
			else if (ch == '?')
			{
				if      (question == 0) p_ptr->get_help_index = HELP_BIRTH_GENDER;
				else if (question == 1) p_ptr->get_help_index = HELP_BIRTH_RACE;
				do_cmd_help();
			}


			/* Normal keypress */
			else
			{
				/* Random choice or quick-start */
				if (ch == '*')
				{
					if (can_quick_start) return ((int)player_birth_quick());

					if      (question == 0) k = rand_int(MAX_SEXES);
					else if (question == 1) k = rand_int(MAX_RACES);
					else                    k = 0;
				}
				else
				{
					/* Choose */
					k = (islower((unsigned char)ch) ? A2I(ch) : -1);
				}

				/* Accept */
				if ((k >= 0) && (k < n)) break;

				/* Complain */
				else
				{
					if (question == 0) bell("Illegal gender!");
					if (question == 1) bell("Illegal race!");
				}
			}
		}

		/* Escape */
		if (ch == ESCAPE) continue;


		/* Gender */
		if (question == 0)
		{
			/* Set gender */
			p_ptr->psex = k;
			sp_ptr = &sex_info[p_ptr->psex];

			/* Display gender */
			c_put_str(TERM_L_BLUE, format("%s", sp_ptr->title), 4, 10);
		}

		/* Race */
		else if (question == 1)
		{
			/* Set race */
			p_ptr->prace = k;
			rp_ptr = &race_info[p_ptr->prace];

			/* Display race */
			c_put_str(TERM_L_BLUE, format("%s", rp_ptr->title), 5, 10);
		}

		/* Question asked */
		question++;

		/* After the first question, cannot type '*' to quick-start */
		if (question > 0)
		{
			put_str("                                                 ", 20, 1);
			can_quick_start = FALSE;
		}

		/* Questions over */
		if (question >= 2) break;
	}

	/* Standard help */
	p_ptr->get_help_index = HELP_GENERAL;

	/* Done */
	return (2);
}



/*
 * Approximate chance out of 10000 that rolling 1d3 + 1d4 + 1d5 will yield
 * a number greater than or equal to 3 + table_element.
 */
static s16b stat_prob[10] =
{
	10000, 9820, 9315, 8325, 6823, 5000, 3177, 1675,  685,  180
};

/*
 * Amount (in tens of percentage points) to be added to rarity if total
 * of requested stats goes above a certain figure.
 */
static s16b add_for_total[12] =
{
	235, 108, 51, 29, 17, 11, 8, 6, 4, 3, 2, 1
};

/*
 * Calculate how frequently the requested stats will be rolled.  This code
 * only gives approximate figures.
 *
 * Todo:  A mathematically correct calculation.  XXX XXX
 */
static u32b calc_rarity(int total)
{
	u32b rarity;

	int i, j;
	int base, total2;

	/* Total too high */
	if (total >= 9 * A_MAX) return (999999999L);

	/* Calculate the rarity of this character */
	for (rarity = 1000L, i = 0; i < A_MAX; i++)
	{
		/* Get probability of rolling this number or higher */
		int roll_rarity = stat_prob[stat_ask[i] - 3];
		int whole       = 10000 / roll_rarity;
		int fraction    = 10000 % roll_rarity;
		u32b rar_tmp    = rarity;

		/* Sum up rarities */
		rarity *= whole;
		rarity += rar_tmp * fraction / roll_rarity;

		/* Rarity too high */
		if (rarity >= 100000000L) break;
	}

	/* Hack -- (partly) un-inflate the rarity */
	rarity /= 100;


	/*
	 * Specifying a large fraction of the amount stats can equal
	 * makes it especially hard to stay within the legal bounds.
	 */
	if ((9 * A_MAX) - total <= N_ELEMENTS(add_for_total))
	{
		i = add_for_total[(9 * A_MAX) - total - 1];

		/* Apply multiplier to rarity */
		rarity = rarity * (10L + i) / 10L;
	}

	/*
	 * Specifying multiple high stats makes it especially hard to
	 * stay within the legal bounds.
	 */
	for (j = 0, i = 0; i < A_MAX; i++)
	{
		if (stat_ask[i] < 10) j++;
		if (stat_ask[i] <  9) j++;
	}

	if      (j >= A_MAX * 2 - 2) base = 0;
	else if (j >= A_MAX * 2 - 4) base = 10;
	else if (j >= A_MAX * 2 - 6) base = 50;
	else if (j == A_MAX * 2 - 7) base = 150;
	else                         base = 400;

	for (total2 = base, i = 0; i < A_MAX; i++)
	{
		if      (stat_ask[i] >= 12) total2 += base * 4;
		else if (stat_ask[i] >=  7) total2 += base / (12 - stat_ask[i]);
	}

	/* Apply multiplier to rarity */
	rarity = rarity * (100L + total2) / 100L;


	/* Return approximate rarity */
	return (rarity / 10L);
}

/*
 * Helper function for 'player_birth()'.
 *
 * This function handles specifying and rolling for stats.
 */
static bool player_birth_aux_2(bool quick_start)
{
	int i, j, m, v;
	int total;
	u32b rarity;

	bool flag;
	bool prev = FALSE;

	char ch;
	char buf[DESC_LEN];

	s16b stat_limit[A_MAX];

	s32b stat_match[A_MAX];

	s32b auto_round = 0L;

	s32b last_round;


	/* Reset character rolls  (this can be abused...) */
	if (!quick_start) p_ptr->birth_roll_requirement = 0L;


	/*** Autoroll ***/

	/* Initialize */
	if (birth_autoroll && !quick_start)
	{
		int mval[A_MAX];
		char rtitle[DESC_LEN];

		char inp[DESC_LEN];

		/* Clear screen */
		(void)Term_clear();

		/* Store the race name */
		strcpy(rtitle, rp_ptr->title);
		strlower(rtitle);

		/* Print race and sex */
		put_str("Gender :", 3, 1);
		put_str("Race   :", 4, 1);
		c_put_str(TERM_L_BLUE, format("%s", sp_ptr->title), 3, 10);
		c_put_str(TERM_L_BLUE, format("%s", rp_ptr->title), 4, 10);

		/* Print instructions */
		move_cursor(7, 0);
		roff(format("     Please specify minimum values for your character's vital statistics.  When you are done, you will be told how unusual your desired adventurer is.  It is possible (indeed, easy) to ask for stats beyond what any %s can have.", rtitle), 5, display_width() - 5);

		/* Prompt for the minimum stats */
		put_str("Enter minimum value for: ", 12, 2);


		/* Interact until satisfied */
		while (TRUE)
		{
			/* Output the maximum stats */
			for (i = 0; i < A_MAX; i++)
			{
				/* Reset the "success" counter */
				stat_match[i] = 0;

				/* Race bonus */
				j = rp_ptr->r_adj[i];

				/* Obtain the "maximal" stat */
				m = modify_stat(17, j);

				/* Save the maximum */
				mval[i] = m;

				/* Above 18 */
				if (m > 18)
				{
					(void)strnfmt(inp, sizeof(inp), "(Max of 18/%02d):", (m - 18));
				}

				/* From 3 to 18 */
				else
				{
					(void)strnfmt(inp, sizeof(inp), "(Max of %2d):", m);
				}

				/* Prepare a prompt */
				(void)strnfmt(buf, sizeof(buf), "%-13s: %-20s",
					flag_creation_data[i].desc, inp);

				/* Dump the prompt */
				prt(buf, 13 + i, 5);
			}

			/* Input the minimum stats */
			for (i = 0; i < A_MAX; i++)
			{
				/* Get a minimum stat */
				while (TRUE)
				{
					char *s;

					/* Move the cursor */
					prt("", 13 + i, 36);

					/* Default */
					strcpy(inp, "");

					/* Context-specific help */
					p_ptr->get_help_index = HELP_STAT_STR + i;

					/* Get a response (or escape) */
					if (!askfor_aux(inp, 8, FALSE)) inp[0] = '0' + 3;

					/* Print "3" if we have specified no stat */
					if (!isdigit(inp[0])) prt("3", 13 + i, 36);

					/* Clear message line */
					prt("", 14 + A_MAX, 5);

					/* Hack -- add a fake slash */
					strcat(inp, "/");

					/* Hack -- look for the "slash" */
					s = strchr(inp, '/');

					/* Hack -- Nuke the slash */
					*s++ = '\0';

					/* Hack -- Extract an input */
					v = atoi(inp) + atoi(s);

					/* Break on valid input */
					if (v <= mval[i]) break;
				}

				/* Clear the rarity message line */
				clear_row(14 + A_MAX);

				/* Save the minimum stat */
				stat_limit[i] = (v > 0) ? v : 0;
			}

			/* General help */
			p_ptr->get_help_index = HELP_GENERAL;


			/* Calculate base stats (ranging from 3 to 12) and total them up */
			for (total = 0, i = 0; i < A_MAX; i++)
			{
				/* Convert stat */
				if (stat_limit[i] <= 18) j = stat_limit[i];
				else j = 18 + ((stat_limit[i] - 18) / 10);

				/* Un-apply racial modifier */
				j = j - rp_ptr->r_adj[i];

				/* Reduce stat by five */
				j -= 5;

				/* Minimum underlying stat is three */
				if (j < 3) j = 3;

				/* Maximum is twelve:  higher requests cannot be granted */
				if (j > 12) total = 9999;

				/* Save the base stat, add to total */
				total += stat_ask[i] = j;
			}

			/* Calculate rarity */
			rarity = calc_rarity(total);

			/* Refuse to attempt the (almost) impossible */
			if ((total >= 9 * A_MAX) || (rarity > 200000L))
			{
				c_prt(TERM_RED, "You ask for the impossible!", 14 + A_MAX, 2);
			}

			/* Print rarity */
			else
			{
				byte a = TERM_GREEN;
				if (rarity >     50L) a = TERM_L_GREEN;
				if (rarity >    500L) a = TERM_WHITE;
				if (rarity >   5000L) a = TERM_YELLOW;
				if (rarity >  25000L) a = TERM_ORANGE;
				if (rarity > 100000L) a = TERM_L_RED;

				if (rarity <= 2)
				{
					c_prt(a, "Most adventurers will meet your criteria.",
						14 + A_MAX, 2);
				}
				else
				{
					c_prt(a, format("Approximately one out of every %ld adventurers will meet your criteria.", rarity), 14 + A_MAX, 2);
				}

				/* Choice */
				if (get_check("Accept these odds, and roll up a character?"))
				{
					/*
					 * Because the new stat prioritizing code makes it faster to roll up
					 * good characters, we save the rarity here. ....  Or will, once we
					 * actually turn on said code!
					 */
					/* p_ptr->birth_roll_requirement = rarity; */


					break;
				}
				else
				{
					prt("", 0, 0);
				}
			}
		}
	}


	/* Clean up */
	clear_from(10);


	/*** Generate ***/

	/* Repeat until satisfied */
	while (TRUE)
	{
		/* Prioritize stats if autorolling, remove priorities otherwise */
		prioritize_stats(birth_autoroll);

		/* We've not loaded a character based on a previous one */
		if (!quick_start)
		{
			int col = 42;

			/* Autoroller feedback */
			if (birth_autoroll)
			{
				(void)Term_clear();

				/* Label */
				put_str(" Limit", 2, col+5);

				/* Label */
				put_str("  Freq", 2, col+13);

				/* Label */
				put_str("  Roll", 2, col+24);

				/* Put the minimal stats */
				for (i = 0; i < A_MAX; i++)
				{
					/* Label stats */
					put_str(stat_names[i], 3+i, col);

					/* Put the stat */
					cnv_stat(buf, sizeof(buf), stat_limit[i]);
					c_put_str(TERM_L_BLUE, buf, 3+i, col+5);
				}

				/* Note when we started */
				last_round = auto_round;

				/* Label count */
				put_str("Round:", 10, col+13);

				/* Indicate the state */
				put_str("(Hit ESC to stop)", 12, col+13);

				/* Auto-roll */
				while (TRUE)
				{
					bool accept = TRUE;

					/* Get a new character */
					get_stats();

					/* Advance the round */
					auto_round++;

					/* Hack -- Prevent overflow */
					if (auto_round >= 1000000L) break;

					/* Check and count acceptable stats */
					for (i = 0; i < A_MAX; i++)
					{
						/* This stat is okay */
						if (stat_use[i] >= stat_limit[i])
						{
							stat_match[i]++;
						}

						/* This stat is not okay */
						else
						{
							accept = FALSE;
						}
					}

					/* Break if "happy" */
					if (accept) break;

					/* Take note every 25 rolls */
					flag = (!(auto_round % 25L));

					/* Update display occasionally */
					if (flag || (auto_round < last_round + 100))
					{
						/* Put the stats (and percents) */
						for (i = 0; i < A_MAX; i++)
						{
							/* Put the stat */
							cnv_stat(buf, sizeof(buf), stat_use[i]);
							c_put_str(TERM_L_GREEN, buf, 3+i, col+24);

							/* Put the percent */
							if (stat_match[i])
							{
								int p = 1000L * stat_match[i] / auto_round;
								byte attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
								(void)strnfmt(buf, sizeof(buf), "%3d.%d%%", p/10, p%10);
								c_put_str(attr, buf, 3+i, col+13);
							}

							/* Never happened */
							else
							{
								c_put_str(TERM_RED, "(NONE)", 3+i, col+13);
							}
						}

						/* Dump round */
						put_str(format("%10ld", auto_round), 10, col+20);

						/* Make sure they see everything */
						(void)Term_fresh();

						/* Delay 1/40 second */
						if (flag) pause_for(25);

						/* Do not wait for a key */
						inkey_scan = TRUE;

						/* Check for a keypress */
						if (inkey(FALSE)) break;
					}
				}
			}

			/* Otherwise just get a character */
			else
			{
				/* Get a new character */
				get_stats();
			}

			/* Flush input */
			flush();

			/* Roll out hitpoints */
			get_extra();

			/* Roll for age/height/weight */
			get_ahw();

			/* Roll for social class */
			get_history();

			/* Roll for gold */
			get_money();

			/* Character starts out with a power level of 1 */
			p_ptr->power = 1;

			/* Get some starting experience  XXX */
			p_ptr->exp = 3;

			/* Redraw experience (later) */
			p_ptr->redraw |= (PR_EXP);
		}


		/* No magic yet */
		mp_ptr = &magic_info[NONE];

		/* Assume we're using karate */
		p_ptr->barehand = S_KARATE;

		/* Calculate the bonuses and hitpoints */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

		/* Hack -- Silence certain status messages */
		character_silent = 1;

		/* Update stuff */
		update_stuff();

		/* Messages are no longer silenced */
		character_silent = 0;

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;

		/* Fully rested */
		p_ptr->csp = p_ptr->msp;

		/* Input loop */
		while (TRUE)
		{
			/* Display the player */
			display_player(0, TRUE);

			/* Prepare a prompt */
			prt("['r' to reroll, 'S' to restart, or Return to accept]", 23, 0);

			/* Prompt and get a command */
			ch = inkey(FALSE);

			/* Quit */
			if ((ch == 'Q') || (ch == 'q')) quit(NULL);

			/* Start over */
			if ((ch == 'S') || (ch == 's')) return (FALSE);

			/* Escape accepts the roll */
			if (ch == ESCAPE) break;

			/* Return accepts the roll */
			if ((ch == '\r') || (ch == '\n') ||
			    ((ch == ' ') && (quick_start)))
			{
				break;
			}

			/* Reroll this character */
			if (((ch == ' ') && (!quick_start)) || (ch == 'r'))
			{
				/* Cancel quick_start (allow normal rolling) */
				if (quick_start)
				{
					birth_autoroll = FALSE;
					quick_start = FALSE;
				}
				break;
			}

			/* Help */
			if (ch == '?')
			{
				do_cmd_help();
				continue;
			}

			/* Warning */
			bell("Illegal command!");
		}

		/* Are we done? */
		if (ch == ESCAPE) break;
		if ((ch == '\r') || (ch == '\n')) break;

		/* Note that a previous roll exists */
		prev = TRUE;
	}

	/* Set character rolls  XXX */
	if (!quick_start) p_ptr->birth_roll_requirement = auto_round;


	/* Clear prompt */
	clear_from(23);

	/* Done */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * See "display_player" for screen layout code.
 */
static int player_birth_aux(void)
{
	char ch;
	char buf[1024];

	/*
	 * Ask questions.  Return -1 for cancel everything, 0 for repeat,
	 * 1 for quick-start, and 2 for normal character birth.
	 */
	int choice = player_birth_aux_1();

	/* Cancelled */
	if (choice <= 0) return (choice);

	/* Roll for stats, gold - handle quickstart.  Display character screen */
	if (!player_birth_aux_2(choice == 1)) return (0);

	/* Get a name, prepare savefile */
	get_name();

	/* Display the player (again) */
	display_player(0, TRUE);


	/* Center the prompt */
	center_string(buf, sizeof(buf),
		"['Q' to quit, 'S' to start over, or any other key to continue]", display_width());

	/* Prompt for it */
	prt(buf, 23, 0);

	/* Get a key */
	ch = inkey(FALSE);

	/* Quit */
	if ((ch == 'Q') || (ch == 'q')) quit(NULL);

	/* Start over */
	if ((ch == 'S') || (ch == 's')) return (0);

	/* Accept */
	return (1);
}

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i;

	object_type *o_ptr;
	object_type object_type_body;

	/* Get local object */
	o_ptr = &object_type_body;


	/* Hack -- Give the player some food */
	object_prep(o_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
	o_ptr->number = rand_range(3, 7);
	object_aware(o_ptr);
	object_known(o_ptr);
	(void)inven_carry(o_ptr);


	/* Get local object */
	o_ptr = &object_type_body;

	/* Hack -- Give the player some torches */
	object_prep(o_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
	apply_magic(o_ptr, 0, -2, FALSE, FALSE);
	o_ptr->number = rand_range(3, 7);
	o_ptr->pval = rand_range(4, 7) * 500;
	object_aware(o_ptr);
	object_known(o_ptr);
	(void)inven_carry(o_ptr);


	/* Set weather to normal, and allow random changes  XXX XXX */
	p_ptr->humid = 0;
	p_ptr->wind =  0;
	p_ptr->temp =  0;
	for (i = 0; i < 10; i++) change_weather(0, 0, 0);
}

/*
 * Some races can go longer between meals than others.
 */
static void init_stomach(void)
{
	bool large = FALSE;

	/* Character is a Half-Troll or a Giant or Ent */
	if ((p_ptr->prace == RACE_HALF_TROLL) ||
	    (p_ptr->prace == RACE_GIANT) ||
		(p_ptr->prace == RACE_ENT))
	{
		/* Trolls can go a long time between (huge!) meals */
		large = TRUE;
	}

	/* Set up the character's food values */
	p_ptr->food_bloated =  (!large ? PY_FOOD_MAX    : PY_FOOD_MAX    * 2);
	p_ptr->food_full =     (!large ? PY_FOOD_FULL   : PY_FOOD_FULL   * 2);
	p_ptr->food_hungry =   (!large ? PY_FOOD_ALERT  : PY_FOOD_ALERT  * 2);
	p_ptr->food_weak =     (!large ? PY_FOOD_WEAK   : PY_FOOD_WEAK   * 2);
	p_ptr->food_fainting = (!large ? PY_FOOD_FAINT  : PY_FOOD_FAINT  * 2);
	p_ptr->food_starving = (!large ? PY_FOOD_STARVE : PY_FOOD_STARVE * 2);


	/* Hack -- Well fed player */
	p_ptr->food = p_ptr->food_full - 1;
}

/*
 * Clear the cheating options
 */
static void cancel_cheat_options(void)
{
	int i;

	/* Paranoia -- Clear the "noscore" variable */
	p_ptr->noscore = 0;
	p_ptr->deaths = 0;

	/* Clear the cheating options */
	for (i = OPT_CHEAT_HEADER; i < 100; i++)
	{
		op_ptr->opt[i] = FALSE;
	}
}



/*
 * Create a new character.
 *
 * We may be called with "junk" left over in the player data structure,
 * so we must be sure to clear it first.
 */
bool player_birth(void)
{
	int n, choice;


	/* Use the standard display and center an 80 column view */
	display_change(DSP_REMEMBER | DSP_NORM | DSP_CX, 80, 0);

	/* Create a new character */
	while (TRUE)
	{
		/* Wipe the player */
		player_wipe(FALSE);

		/* Roll up a new character */
		choice = player_birth_aux();

		/* Accept */
		if (choice != 0) break;
	}

	/* Allow cancel */
	if (choice < 0)
	{
		/* Restore previous display */
		display_change(DSP_RESTORE, 0, 0);

		return (FALSE);
	}


	/* Note player birth in the message recall */
	message_add(" ", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add(format("========= %s the %s is born ===========",
		op_ptr->full_name, rp_ptr->title), MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add(" ", MSG_GENERIC);


	/* Hack -- outfit the player */
	player_outfit();

	/* Hack -- initialize character food values */
	init_stomach();

	/* Hack -- cancel all cheating options */
	cancel_cheat_options();

	/* Get or correct the inn name index */
	if ((p_ptr->inn_name <= 0) || (p_ptr->inn_name >= MAX_INN_NAMES))
		p_ptr->inn_name = randint(MAX_INN_NAMES-1);

	/* Shops */
	for (n = 0; n < MAX_STORES; n++)
	{
		/* Initialize */
		store_init(n);

		/* Maintain the shop (fully) */
		store_maint(n, TRUE);
	}

	p_ptr->specialty = SPECIALTY_NONE;

	/* Record a random beginning */
	history_clear();
	if (one_in_(4)) 		history_add("Started the adventure of a lifetime.", HISTORY_PLAYER_BIRTH, 0);
	else if (one_in_(3)) 	history_add("Swore a blood oath to defeat Morgoth.", HISTORY_PLAYER_BIRTH, 0);
	else if (one_in_(2))	history_add("Pops outside to see what all the fuss is about.", HISTORY_PLAYER_BIRTH, 0);
	else					history_add("Takes his last practice swing.", HISTORY_PLAYER_BIRTH, 0);


	/* Restore previous display */
	display_change(DSP_RESTORE, 0, 0);

	/* Success */
	return (TRUE);
}

