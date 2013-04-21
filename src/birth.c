/* File: birth.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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

	s16b stat[6];

	char history[4][60];
};



/*
 * Roll history - the player can choose one of these
 */
static birther rolls[10];



/*
 * Forward declare
 */
typedef struct hist_type hist_type;

/*
 * Player background information
 */
struct hist_type
{
	cptr info;			    /* Textual History */

	byte roll;			    /* Frequency of this entry */
	byte chart;			    /* Chart index */
	byte next;			    /* Next chart index */
	byte bonus;			    /* Social Class Bonus + 50 */
};


/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human/Dunadan -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Elf      -->  4 -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Elf/High-Elf  -->  7 -->  8 -->  9 --> 54 --> 55 --> 56
 *   Hobbit        --> 10 --> 11 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Gnome         --> 13 --> 14 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Dwarf         --> 16 --> 17 --> 18 --> 57 --> 58 --> 59 --> 60 --> 61
 *   Half-Orc      --> 19 --> 20 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Troll    --> 22 --> 23 --> 62 --> 63 --> 64 --> 65 --> 66
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
	{"You are a well liked child.  ", 100, 3, 50, 60},

	{"Your mother was of the Teleri.  ", 40, 4, 1, 50},
	{"Your father was of the Teleri.  ", 75, 4, 1, 55},
	{"Your mother was of the Noldor.  ", 90, 4, 1, 55},
	{"Your father was of the Noldor.  ", 95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ", 98, 4, 1, 65},
	{"Your father was of the Vanyar.  ", 100, 4, 1, 70},

	{"You are one of several children ", 60, 7, 8, 50},
	{"You are the only child ", 100, 7, 8, 55},

	{"of a Teleri ", 75, 8, 9, 50},
	{"of a Noldor ", 95, 8, 9, 55},
	{"of a Vanyar ", 100, 8, 9, 60},

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
	{"You are a well liked child.  ", 100, 18, 57, 55},

	{"Your mother was an Orc, but it is unacknowledged.  ", 25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ",	100, 19, 20, 25},

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
	{"leprous skin.", 100, 66, 0, 50}
};



/*
 * Current stats
 */
static s16b stat_use[6];



/*
 * Save the current data for later
 */
static void save_roll_data( int roll )
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
	rolls[ roll ].age = p_ptr->age;
	rolls[ roll ].wt = p_ptr->wt;
	rolls[ roll ].ht = p_ptr->ht;
	rolls[ roll ].sc = p_ptr->sc;
	rolls[ roll ].au = p_ptr->au;

	/* Save the stats */
	for ( i = 0; i < 6; i++ )
	{
		rolls[ roll ].stat[ i ] = p_ptr->stat_max[ i ];
	}

	/* Save the history */
	for ( i = 0; i < 4; i++ )
	{
		strcpy( rolls[ roll ].history[ i ], p_ptr->history[ i ] );
	}
}


/*
 * Load the roll data
 */
static void load_roll_data( int roll )
{
	int i;


	/* Load the data */
	p_ptr->age = rolls[ roll ].age;
	p_ptr->wt = rolls[ roll ].wt;
	p_ptr->ht = rolls[ roll ].ht;
	p_ptr->sc = rolls[ roll ].sc;
	p_ptr->au = rolls[ roll ].au;

	/* Load the stats */
	for ( i = 0; i < 6; i++ )
	{
		p_ptr->stat_max[ i ] = rolls[ roll ].stat[ i ];
		p_ptr->stat_cur[ i ] = rolls[ roll ].stat[ i ];
	}

	/* Load the history */
	for ( i = 0; i < 4; i++ )
	{
		strcpy( p_ptr->history[ i ], rolls[ roll ].history[ i ] );
	}
}




/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
	int i, j;

	int bonus;

	int dice[18];


	/* Roll and verify some stats */
	while (TRUE)
	{
		/* Roll some dice */
		for (j = i = 0; i < 18; i++)
		{
			/* Roll the dice */
			dice[i] = randint(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}

		/* Verify totals */
		if (j > 48) break;
	}

	/* Acquire the stats */
	for (i = 0; i < 6; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

		/* Save that value */
		p_ptr->stat_max[i] = j;

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Start fully healed */
		p_ptr->stat_cur[i] = p_ptr->stat_max[i];

		/* Efficiency -- Apply the racial/class bonuses */
		stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
	}
}


/*
 * Roll for some info that the auto-roller ignores
 */
void get_extra(void)
{
	int i, j, min_value, max_value;


	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Minimum hitpoints at highest level */
	min_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 3) / 8;
	min_value += PY_MAX_LEVEL;

	/* Maximum hitpoints at highest level */
	max_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 5) / 8;
	max_value += PY_MAX_LEVEL;

	/* Pre-calculate level 1 hitdice */
	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */
	while (TRUE)
	{
		/* Roll the hitpoint values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			j = randint(p_ptr->hitdie);
			p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + j;
		}

		/* XXX Could also require acceptable "mid-level" hitpoints */

		/* Require "valid" hitpoints at highest level */
		if (p_ptr->player_hp[PY_MAX_LEVEL-1] < min_value) continue;
		if (p_ptr->player_hp[PY_MAX_LEVEL-1] > max_value) continue;

		/* Acceptable */
		break;
	}
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int i, n, chart, roll, social_class;

	char *s, *t;

	char buf[240];



	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) p_ptr->history[i][0] = '\0';


	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint(4);

	/* Starting place */
	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
		case RACE_DUNADAN:
		{
			chart = 1;
			break;
		}

		case RACE_HALF_ELF:
		{
			chart = 4;
			break;
		}

		case RACE_ELF:
		case RACE_HIGH_ELF:
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
		strcat(buf, bg[i].info);

		/* Add in the social class */
		social_class += (int)(bg[i].bonus) - 50;

		/* Enter the next chart */
		chart = bg[i].next;
	}



	/* Verify social class */
	if (social_class > 100) social_class = 100;
	else if (social_class < 1) social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;


	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';


	/* Start at first line */
	i = 0;

	/* Collect the history */
	while (TRUE)
	{
		/* Extract remaining length */
		n = strlen(s);

		/* All done */
		if (n < 60)
		{
			/* Save one line of history */
			strcpy(p_ptr->history[i++], s);

			/* All done */
			break;
		}

		/* Find a reasonable break-point */
		for (n = 60; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

		/* Save next location */
		t = s + n;

		/* Wipe trailing spaces */
		while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

		/* Save one line of history */
		strcpy(p_ptr->history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++) /* loop */;
	}
}


/*
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = Rand_normal(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt = Rand_normal(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
	}
}




/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;


	/* Wipe the player */
	WIPE(p_ptr, player_type);


	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}


	/* Start with no artifacts made yet */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}


	/* Start with no quests */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		q_list[i].level = 0;
	}

	/* Add a special quest */
	q_list[0].level = 99;

	/* Add a second quest */
	q_list[1].level = 100;


	/* Reset the "objects" */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}


	/* Reset the "monsters" */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */ r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 1;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


	/* Hack -- no ghosts */
	r_info[MAX_R_IDX-1].max_num = 0;


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* None of the spells have been learned yet */
	for (i = 0; i < 64; i++) p_ptr->spell_order[i] = 99;
}


/*
 * Helper function for 'player_birth()'
 */
static bool player_birth_aux()
{
	int k, n, roll;

	cptr str;

	char c;

#if 0
	char p1 = '(';
#endif
	char p2 = ')';
	char b1 = '[';
	char b2 = ']';

	char buf[80];


	/*** Instructions ***/

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
	Term_putstr(5, 10, -1, TERM_WHITE,
		"Please answer the following questions.  Most of the questions");
	Term_putstr(5, 11, -1, TERM_WHITE,
		"display a set of standard answers, and many will also accept");
	Term_putstr(5, 12, -1, TERM_WHITE,
		"some special responses, including 'Q' to quit, 'S' to restart,");
	Term_putstr(5, 13, -1, TERM_WHITE,
		"and '?' for help.  Note that 'Q' and 'S' must be capitalized.");


	/*** Player sex ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Your 'sex' does not have any significant gameplay effects.");

	/* Prompt for "Sex" */
	for (n = 0; n < MAX_SEXES; n++)
	{
		/* Analyze */
		p_ptr->psex = n;
		sp_ptr = &sex_info[p_ptr->psex];
		str = sp_ptr->title;

		/* Display */
		sprintf(buf, "%c%c %s", I2A(n), p2, str);
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
	}

	/* Choose */
	while ( TRUE )
	{
		sprintf(buf, "Choose a sex (%c-%c): ", I2A(0), I2A(n-1));
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == ESCAPE) c = 'a';
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n)) break;
		if (c == '?') do_cmd_help();
		else bell("Illegal sex!");
	}

	/* Set sex */
	p_ptr->psex = k;
	sp_ptr = &sex_info[p_ptr->psex];

	/* Clean up */
	clear_from(15);


	/*** Player race ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Your 'race' determines various intrinsic factors and bonuses.");

	/* Dump races */
	for (n = 0; n < MAX_RACES; n++)
	{
		/* Analyze */
		p_ptr->prace = n;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;

		/* Display */
		sprintf(buf, "%c%c %s", I2A(n), p2, str);
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
	}

	/* Choose */
	while ( TRUE )
	{
		sprintf(buf, "Choose a race (%c-%c): ", I2A(0), I2A(n-1));
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == ESCAPE) c = 'a';
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n)) break;
		if (c == '?') do_cmd_help();
		else bell("Illegal race!");
	}

	/* Set race */
	p_ptr->prace = k;
	rp_ptr = &race_info[p_ptr->prace];

	/* Clean up */
	clear_from(15);


	/* Set class */
	p_ptr->pclass = MAX_CLASS-1;
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &magic_info[p_ptr->pclass];
	str = cp_ptr->title;

	/* Display */
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

	/* Clear */
	clear_from(20);


	/*** Generate ***/

	/* Do 10 rolls */
	for ( roll=0; roll<10; roll++ ) {

		/* Get a new character */
		get_stats();

		/* Flush input */
		flush();

		/* Roll for base hitpoints */
		get_extra();

		/* Roll for age/height/weight */
		get_ahw();

		/* Roll for social class */
		get_history();

		/* Roll for gold */
		p_ptr->au = 0;  /* Start with no money (it's useless in town) */

		save_roll_data( roll );
	}

	roll = 0;

	/* Input loop */
	while ( TRUE )
	{
		load_roll_data( roll );

		/* Calculate the bonuses and hitpoints */
		p_ptr->update |= (PU_BONUS | PU_HP);

		/* Update stuff */
		update_stuff();

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;

		/* Fully rested */
		p_ptr->csp = p_ptr->msp;

		/* Display the player */
		display_player(0);

		/* Prepare a prompt (must squeeze everything in) */
		Term_gotoxy(2, 23);
		Term_addch(TERM_WHITE, b1);
		if ( roll < 9 ) Term_addstr(-1, TERM_WHITE, "'n' for next");
		if ( roll > 0 ) Term_addstr(-1, TERM_WHITE,
			( roll < 9 ) ? ", 'p' for prev" : "'p' for prev");
		Term_addstr(-1, TERM_WHITE, ", or ESC to accept");
		Term_addch(TERM_WHITE, b2);

		/* Prompt and get a command */
		c = inkey();

		/* Quit */
		if (c == 'Q') quit(NULL);

		/* Start over */
		if (c == 'S') return (FALSE);

		/* Escape accepts the roll */
		if (c == ESCAPE) break;

		/* Previous character */
		if ( ( roll > 0 ) && ( c == 'p' ))
		{
			roll--;
			continue;
		}

		/* Next character */
		if ( ( roll < 9 ) && ( c == 'n' ))
		{
			roll++;
			continue;
		}

		/* Help */
		if (c == '?')
		{
			do_cmd_help();
			continue;
		}

		/* Warning */
		bell("Illegal command!");
	}

	/* Clear prompt */
	clear_from(23);


	/*** Finish up ***/

	/* Get a name, prepare savefile */
	get_name();

	/* Display the player */
	display_player(0);

	/* Prompt for it */
	prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

	/* Get a key */
	c = inkey();

	/* Quit */
	if (c == 'Q') quit(NULL);

	/* Start over */
	if (c == 'S') return (FALSE);

	/* Accept */
	return (TRUE);
}


/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		if (player_birth_aux()) break;
	}


	/* Note player birth in the message recall */
	message_add(" ");
	message_add("  ");
	message_add("====================");
	message_add("  ");
	message_add(" ");


	home_init();
}
