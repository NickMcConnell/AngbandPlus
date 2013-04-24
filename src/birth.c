/* File: birth.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/* Forward declare */
typedef struct birther birther;
typedef struct birth_menu birth_menu;

/* A structure to hold "rolled" information */
struct birther
{
	s16b age;
	s16b wt;
	s16b ht;
	s16b sc;

	s32b au;

	s16b stat[A_MAX];

	char history[4][60];
};

/* A Structure to hold the menus */
struct birth_menu
{
	bool ghost;
	cptr name;
};

/*
 * The last character displayed
 */
static birther prev;


/*
 * Current stats (when rolling a character).
 */
static s16b stat_use[A_MAX];



/*
 * Save the currently rolled data for later.
 */
static void save_prev_data(void)
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
	prev.age = p_ptr->age;
	prev.wt = p_ptr->wt;
	prev.ht = p_ptr->ht;
	prev.sc = p_ptr->sc;
	prev.au = p_ptr->au;

	/* Save the stats */
	/* This doesn't appear to need changing for the new stat system */
	for (i = 0; i < A_MAX; i++)
	{
		prev.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(prev.history[i], p_ptr->history[i]);
	}
}


/*
 * Load the previously rolled data.
 */
static void load_prev_data(void)
{
	int i;

	birther temp;


	/*** Save the current data ***/

	/* Save the data */
	temp.age = p_ptr->age;
	temp.wt = p_ptr->wt;
	temp.ht = p_ptr->ht;
	temp.sc = p_ptr->sc;
	temp.au = p_ptr->au;

	/* Save the stats */
	/* This stat exchange also doesn't appear to need changing. */
	for (i = 0; i < A_MAX; i++)
	{
		temp.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(temp.history[i], p_ptr->history[i]);
	}


	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->age = prev.age;
	p_ptr->wt = prev.wt;
	p_ptr->ht = prev.ht;
	p_ptr->sc = prev.sc;
	p_ptr->au = prev.au;

	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat_max[i] = prev.stat[i];
		p_ptr->stat_cur[i] = prev.stat[i];
		p_ptr->stat_birth[i] = prev.stat[i];
	}

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(p_ptr->history[i], prev.history[i]);
	}


	/*** Save the current data ***/

	/* Save the data */
	prev.age = temp.age;
	prev.wt = temp.wt;
	prev.ht = temp.ht;
	prev.sc = temp.sc;
	prev.au = temp.au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		prev.stat[i] = temp.stat[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(prev.history[i], temp.history[i]);
	}
}




/* This is the rewritten 'get_stats' function for the new stat system */
/* rp_ptr->r_adj[i] + cp_ptr->c_adj[i];*/
static void get_stats(void)
{
 	int i; 
 	int raceside, classside; 
 	int totalside;
 	int statroll;
 	int roll1, roll2, roll3;
 	
 	/* hmmmm. This should do it */
 	for (i = 0; i < A_MAX; i++)
 	{
 		/* assign the first number(racial dice sides) to raceside */
 		raceside = rp_ptr->r_adj[i];
 		
 		/* assign the second number (class die sides) to classside */
 		classside = cp_ptr->c_adj[i];
 		
 		/* total them */
 		totalside = raceside + classside;	

		/* reduce variance */
		roll1 = (randint(totalside) + randint(totalside)) / 2;

		/* reduce variance */
		roll2 = (randint(totalside) + randint(totalside)) / 2;

		/* reduce variance */
		roll3 = (randint(totalside) + randint(totalside)) / 2;
 		
 		/* Get a random integer between those two numbers three times */
 		statroll = (roll1 + roll2 + roll3) + (roll1 * 2) + (roll2 * 2) + (roll3 * 2);
 		statroll *= 14;
 		statroll /= 8;
 		
 		if (statroll > 700) statroll = 700;
 		
 		/* save that value */
 		p_ptr->stat_max[i] = statroll;
 		
 		/* Start fully healed */
 		stat_use[i] = p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i];
	}
}

/* Astral / Wonderland mode added by DH - stolen from gum*/
/* Darren Hart is awesome  */
static void astral_mode(void)
{
	char c;

	/*** Ghostly Status -- from Gumband ***/
	/* Extra info */
	Term_putstr(5, 13, -1, TERM_WHITE,
		"Starting in wonderland mode makes you begin the game on");
	Term_putstr(5, 14, -1, TERM_WHITE,
		"just below the surface.  You must make your way from there to the");
	Term_putstr(5, 15, -1, TERM_WHITE,
		"town on foot, where you will finally regain your corporeal");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"form.  You will then have to make your way back up to confront");
	Term_putstr(5, 17, -1, TERM_WHITE,
		"the source of evil to win the game.");

	while (1)
	{
		put_str("Start in Wonderland mode? (y/n) ", 20, 2);
		c = inkey();
		/*if (c == 'Q') quit(NULL);*/
		/*if (c == 'S') return (FALSE);*/
		if (c == ESCAPE) break;
		if ((c == 'y') || (c == 'n')) break;
		/*if (c == '?') do_cmd_help("help.hlp");*/
	}

	/* Set "ghost" mode */
	if (c == 'y')
	{
		p_ptr->astral = TRUE;
		p_ptr->was_astral = FALSE;
		p_ptr->astral_start = TRUE;
	}
	else
	{
		p_ptr->astral = FALSE;
		p_ptr->was_astral = FALSE;
		p_ptr->astral_start = FALSE;
	}

	/* Clear */
	clear_from(13);
}





/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
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
	chart = rp_ptr->hist;


	/* Process the history */
	while (chart)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint(100);

		/* Get the proper entry in the table */
		while ((chart != h_info[i].chart) || (roll > h_info[i].roll)) i++;

		/* Get the textual history */
		strcat(buf, (h_text + h_info[i].text));

		/* Add in the social class */
		social_class += (int)(h_info[i].bonus) - 50;

		/* Enter the next chart */
		chart = h_info[i].next;
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
 * Get the player's starting money
 */
static void get_money(void)
{
	int gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint(100) + 300;

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = gold;
}



/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;


	/* Wipe the player */
	(void)WIPE(p_ptr, player_type);


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
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
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
		if (r_ptr->flags1 & (RF1_UNIQUE)) 
		{
		r_ptr->max_num = 1;
		}
		
		/* Clear player kills */
		l_ptr->r_pkills = 0;
	}

	/* Default pet command settings */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
	p_ptr->pet_open_doors = FALSE;
	p_ptr->pet_pickup_items = FALSE;

	/* set skills to default */
	for (i = 0; i < N_SKILLS; i++)
	{
	p_ptr->skills[i].skill_rank = -2;
	p_ptr->skills[i].skill_max = -2;
	p_ptr->skills[i].skill_index = 0;
	}
	
	/* Hack -- no ghosts */
	r_info[z_info->r_max-1].max_num = 0;


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;
	
	/* Hack -- Give the player some inital skill points */
	p_ptr->free_skpts = rand_range(18, 22);

	/* None of the spells have been learned yet */
/*	for (i = 0; i < PY_MAX_SPELLS; i++) p_ptr->spell_order[i] = 99; */
}



/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i;
	int social_class_type;

	const start_item *e_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	/* Hack-Check Social Status to see if scum ,lower class, middle class, upper clas, noblity */
	/* scum start with -2 items, lower class -1 item, middle no change, upper class has +1 item */
	/* and nobility start with +2 items - -2 to +2 also affects start type & qty of food and torches */
	/* SOC 1: Scum = type 1 */
	/* SOC 2-10: Lower Class = type 2 */
	/* SOC 11-25: "upper" Lower Class = type 3 */
	/* SOC 26-75: Middle Class = type 4*/
	/* SOC 76-89: Lower Upper Class = type 5*/
	/* SOC 90-99: Upper Class = type 6*/
	/* SOC 100: Noblity = type 7*/

	social_class_type = 4; /* Set Middle Class as default type */

	if (p_ptr->sc < 2) social_class_type = 1; 
	else if ((p_ptr->sc > 1) && (p_ptr->sc < 11)) social_class_type = 2;
	else if ((p_ptr->sc > 10) && (p_ptr->sc < 26)) social_class_type = 3;
	/* note the missing social class type here */
	else if ((p_ptr->sc > 75) && (p_ptr->sc < 90)) social_class_type = 5;
	else if ((p_ptr->sc > 89) && (p_ptr->sc < 100)) social_class_type = 6;
	else if (p_ptr->sc > 99) social_class_type = 7;
	else social_class_type = 4;

	/* Astral (wonderland) Mode treats social_class_type as "middle" (type 4) */
	if (p_ptr->astral) social_class_type = 4;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some food */
	/* social class affect qty and type of food */
	switch (social_class_type)
	{
		case 1:
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
			}
			else object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_ONION));
			i_ptr->number = (byte)rand_range(1, 5);
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
			break; 
		case 2:
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
			}
			else object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_POTATO));
			i_ptr->number = (byte)rand_range(2, 5);
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
			break;
		case 3:
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
			}
			else object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_HADDOCKS));
			i_ptr->number = (byte)rand_range(2, 6);
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
			break;
		case 5:
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
			}
			else object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_CHEESE));
			i_ptr->number = (byte)rand_range(3, 7);
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
			break;
		case 6:
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
			}
			else object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_MEAT_PIE));
			i_ptr->number = (byte)rand_range(4, 7);
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
			break;
		case 7:
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
			}
			else object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_PIGEON_PIE));
			i_ptr->number = (byte)rand_range(4, 8);
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
			break;
		default: /*"Middle Class" type 4 is default */
			if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
			{
				object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
			}
			else object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
			i_ptr->number = (byte)rand_range(3, 6);
			if (p_ptr->astral) i_ptr->number += 5;
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
	}

	/* Start off with lots of ID scrolls if a ghost from gumband*/
	if (p_ptr->astral)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		object_prep(i_ptr, lookup_kind(TV_MECHANISM, SV_MECHANISM_IDENTIFY));
		i_ptr->number = 70;
		object_aware(i_ptr);
		object_known(i_ptr);
		(void)inven_carry(i_ptr);
	}


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some torches */
	/* social class affects qty and length of torches */
	/* astral characters don't need torches-they wiz lite */
	if (!p_ptr->astral)
	{ 	
		switch (social_class_type)
		{ 
			/*start switch */
			case 1:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TAPER));
				i_ptr->number = (byte)rand_range(1, 5);
				i_ptr->pval = rand_range(1,6) * 150;
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
				break; 
			case 2:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_CANDLE));
				i_ptr->number = (byte)rand_range(2, 5);
				i_ptr->pval = rand_range(1,6) * 300;
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
				break;
			case 3:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_CANDLE));
				i_ptr->number = (byte)rand_range(2, 6);
				i_ptr->pval = rand_range(2,7) * 400;
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
				break;
			case 5:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
				i_ptr->number = (byte)rand_range(3, 7);
				i_ptr->pval = rand_range(3,7) * 500;
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
				break;
			case 6:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_LANTERN));
				i_ptr->number = 1;
				i_ptr->pval = rand_range(1,7) * 400;
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
				break;
			case 7:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_LANTERN));
				i_ptr->number = 1;
				i_ptr->pval = rand_range(4,8) * 600;
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
				break;
			default: /*type 4 - Middle Class is default */
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
				i_ptr->number = (byte)rand_range(3, 6);
				i_ptr->pval = rand_range(3,7) * 500;
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
				break;
		} 
		/*end switch */
	}

	/* Hack -- Force Automata and Steam Meca to equip torsos (from Animeband) -DH */
	if ((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA))
	{
		object_prep(i_ptr, lookup_kind(TV_MECHA_TORSO, SV_MECHA_TORSO_TIN));
		i_ptr->number = 1;
		i_ptr->pval = 1;
		object_aware(i_ptr);
		object_known(i_ptr);
		object_copy(&inventory[INVEN_BODY], i_ptr);
	}
	
	/* Hack -- Give the player his equipment */
	for (i = 0; i < MAX_START_ITEMS; i++)
	{
		/* Access the item */
		e_ptr = &(cp_ptr->start_items[i]);

		/* Get local object */
		i_ptr = &object_type_body;

		/* Hack	-- Give the player an object */
		if (e_ptr->tval > 0)
		{
			/* Get the object_kind */
			int k_idx = lookup_kind(e_ptr->tval, e_ptr->sval);

			/* Valid item? */
			if (!k_idx) continue;

			/* Hack-check for Automata status and tonics */
			/* if true, replace tonics with Mechanicsm? */
			/* Automata's can't use tonics */
			if (((p_ptr->prace == RACE_AUTOMATA) || (p_ptr->prace == RACE_STEAM_MECHA)) && (e_ptr->tval == 75))
			{
				object_prep(i_ptr, lookup_kind(TV_MECHANISM, SV_MECHANISM_IDENTIFY));
			}
			else
			{
				/* Prepare the item */
				object_prep(i_ptr, k_idx);
			}

			/* Give the object charges ? */
			i_ptr->pval = e_ptr->pval;
			i_ptr->number = (byte)rand_range(e_ptr->min, e_ptr->max);

			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
		}
	}
}


/* Locations of the tables on the screen */

#define HEADER_ROW		1
#define INSTRUCT_ROW	3
#define QUESTION_ROW	7
#define TABLE_ROW		10

#define QUESTION_COL	2
#define SEX_COL			2
#define RACE_COL		14
#define RACE_AUX_COL    30
#define CLASS_COL		30
#define CLASS_AUX_COL   50

#define INVALID_CHOICE 255

/* Clear the previous question */
static void clear_question(void)
{
	int i;

	for (i = QUESTION_ROW; i < TABLE_ROW; i++)
	{
		/* Clear line, position cursor */
		Term_erase(0, i, 255);
	}
}


/*
 * Generic "get choice from menu" function
 */
static int get_player_choice(birth_menu *choices, int num, int col, int wid,
                             void (*hook)(birth_menu))
{
	int top = 0, cur = 0;
	int i, j, dir;
	char c;
	char buf[80];
	bool done = FALSE;
	int hgt;
	byte attr;
	

	/* Autoselect if able */
	if (num == 1) done = TRUE;

	/* Clear */
	for (i = TABLE_ROW; i < Term->hgt; i++)
	{
		/* Clear */
		Term_erase(col, i, Term->wid - wid);	
	}

	/* Choose */
	while (TRUE)
	{
		hgt = Term->hgt - TABLE_ROW - 1;

		/* Redraw the list */
		for (i = 0; ((i + top < num) && (i <= hgt)); i++)
		{
			if (i + top < 26)
			{
				sprintf(buf, "%c) %s", I2A(i + top), choices[i + top].name);
			}
			else
			{
				/* ToDo: Fix the ASCII dependency */
				sprintf(buf, "%c) %s", 'A' + (i + top - 26), choices[i + top].name);
			}

			/* Clear */
			Term_erase(col, i + TABLE_ROW, wid);

			/* Display */
			if (i == (cur - top))
			{
				/* Highlight the current selection */
				if (choices[i + top].ghost) attr = TERM_BLUE;
				else attr = TERM_L_BLUE;
			}
			else
			{
				if (choices[i + top].ghost) attr = TERM_SLATE;
				else attr = TERM_WHITE;
			}

			Term_putstr(col, i + TABLE_ROW, wid, attr, buf);
		}

		if (done) return (cur);

		/* Display auxiliary information if any is available. */
		if (hook) hook(choices[cur]);

		/* Move the cursor */
		put_str("", TABLE_ROW + cur - top, col);

		c = inkey();

		if (c == KTRL('X'))
		{
			quit(NULL);
		}
		if (c == ESCAPE)
		{
			/* Mega Hack - go back. */
			return (INVALID_CHOICE);
		}
		if (c == '*')
		{
			/* Select a legal choice at random */
			while (TRUE)
			{
				cur = rand_int(num);
				if (!choices[cur].ghost) break;
			}

			/* Move it onto the screen */
			if ((cur < top) || (cur > top + hgt))
			{
				top = cur;
			}

			/* Done */
			done = TRUE;
		}
		else if (c == '?')
		{
			do_cmd_help();
		}
		else if (c == '=')
		{
			do_cmd_options();
			
			/* Set adult options from birth options */
			for (j = OPT_BIRTH; j < OPT_CHEAT; j++)
			{
				op_ptr->opt[OPT_ADULT + (j - OPT_BIRTH)] = op_ptr->opt[j];
			}
		
			/* Reset score options from cheat options */
			for (j = OPT_CHEAT; j < OPT_ADULT; j++)
			{
				op_ptr->opt[OPT_SCORE + (j - OPT_CHEAT)] = op_ptr->opt[j];
			}

		}
		else if ((c == '\n') || (c == '\r'))
		{
			/* Done */
			return (cur);
		}
		else if (isdigit(c))
		{
			/* Get a direction from the key */
			dir = target_dir(c);

			/* Going up? */
			if (dir == 8)
			{
				if (cur != 0)
				{
					/* Move selection */
					cur--;
				}

				if ((top > 0) && ((cur - top) < 4))
				{
					/* Scroll up */
					top--;
				}
			}

			/* Going down? */
			if (dir == 2)
			{
				if (cur != (num - 1))
				{
					/* Move selection */
					cur++;
				}

				if ((top + hgt < (num - 1)) && ((top + hgt - cur) < 4))
				{
					/* Scroll down */
					top++;
				}
			}
		}
		else if (isalpha(c))
		{
			int choice;

			if (islower(c))
			{
				choice = A2I(c);
			}
			else
			{
				choice = c - 'A' + 26;
			}

			/* Validate input */
			if ((choice > -1) && (choice < num))
			{
				cur = choice;

				/* Move it onto the screen */
				if ((cur < top) || (cur > top + hgt))
				{
					top = cur;
				}

				/* Done */
				done = TRUE;
			}
			else
			{
				bell("Illegal response to question!");
			}
		}

		/* Invalid input */
		else bell("Illegal response to question!");
	}

	return (INVALID_CHOICE);
}


/*
 * Display additional information about each race during the selection.
 */
static void race_aux_hook(birth_menu r_str)
{
	int race, i;
	char s[50];

	/* Extract the proper race index from the string. */
	for (race = 0; race < z_info->p_max; race++)
	{
		if (!strcmp(r_str.name, p_name + p_info[race].name)) break;
	}

	if (race == z_info->p_max) return;

	/* Display relevant details. */
	for (i = 0; i < A_MAX; i++)
	{
		/* This is the name string, the code for the '+' sign, */
		/* and the adjustment for each stat. This will stay the same */
		/* except for it will be a dx. -CCC*/
		sprintf(s, "%s d%d   ", stat_names_reduced[i],
		p_info[race].r_adj[i]);
		Term_putstr(RACE_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}

	sprintf(s, "Hit die: %d ", p_info[race].r_mhp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX, -1, TERM_WHITE, s);
	sprintf(s, "Experience: %d%% ", p_info[race].r_exp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 1, -1, TERM_WHITE, s);
	sprintf(s, "Infravision: %d ft  ", p_info[race].infra * 10);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
}


/*
 * Player race
 */
static bool get_player_race()
{
	int i;
	birth_menu *races;

	C_MAKE(races, z_info->p_max, birth_menu);

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'race' determines various intrinsic factors and bonuses.");
	Term_putstr(QUESTION_COL, QUESTION_ROW + 1, -1, TERM_YELLOW,
		"Your combined dx stat value for race and class determines your stat growth.");

	/* Tabulate races */
	for (i = 0; i < z_info->p_max; i++)
	{
		races[i].name = p_name + p_info[i].name;
		races[i].ghost = FALSE;
	}

	p_ptr->prace = get_player_choice(races, z_info->p_max, RACE_COL, 15,
		race_aux_hook);

	/* No selection? */
	if (p_ptr->prace == INVALID_CHOICE)
	{
		p_ptr->prace = 0;
		return (FALSE);
	}

	/* Save the race pointer */
	rp_ptr = &p_info[p_ptr->prace];

	C_FREE(races, z_info->p_max, birth_menu);

	/* Success */
	return (TRUE);
}


/*
 * Display additional information about each class during the selection.
 */
static void class_aux_hook(birth_menu c_str)
{
	int class_idx, i;
	char s[128];

	/* Extract the proper class index from the string. */
	for (class_idx = 0; class_idx < z_info->c_max; class_idx++)
	{
		if (!strcmp(c_str.name, c_name + c_info[class_idx].name)) break;
	}

	if (class_idx == z_info->c_max) return;

	/* Display relevant details. */
	for (i = 0; i < A_MAX; i++)
	{
		/* See relevant section for race above, this should */
		/* For the most part remain unchanged */
		/* NOTE: NEED TO FIX THE ALIGNMENT OF STATGROWTH -FIXED */
		sprintf(s, "%s d%2d %sd%d   ", stat_names_reduced[i],
		c_info[class_idx].c_adj[i], "Statgrowth:", (c_info[class_idx].c_adj[i] + p_info[p_ptr->prace].r_adj[i]));
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}

	sprintf(s, "Hit die: %d ", c_info[class_idx].c_mhp);
	Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX, -1, TERM_WHITE, s);
	sprintf(s, "Experience: %d%% ", c_info[class_idx].c_exp);
	Term_putstr(CLASS_AUX_COL, TABLE_ROW + A_MAX + 1, -1, TERM_WHITE, s);
}

/*
 * Player class
 */
static bool get_player_class(void)
{
	int  i;
	birth_menu *classes;

	C_MAKE(classes, z_info->c_max, birth_menu);

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'class' determines various intrinsic abilities and bonuses.");
	Term_putstr(QUESTION_COL, QUESTION_ROW + 1, -1, TERM_YELLOW,
	    "Any greyed-out entries should only be used by advanced players.");

	/* Tabulate classes */
	for (i = 0; i < z_info->c_max; i++)
	{
		/* Analyze */
		if (!(rp_ptr->choice & (1L << i)))
		{
			classes[i].ghost = TRUE;
		}
		else
		{
			classes[i].ghost = FALSE;
		}

		/* Save the string */
		classes[i].name = c_name + c_info[i].name;
	}

	p_ptr->pclass = get_player_choice(classes, z_info->c_max, CLASS_COL, 20,
                                      class_aux_hook);

	/* No selection? */
	if (p_ptr->pclass == INVALID_CHOICE)
	{
		p_ptr->pclass = 0;

		return (FALSE);
	}

	/* Set class */
	cp_ptr = &c_info[p_ptr->pclass];
	
	/* Initialize the magic */
/*	mp_ptr = &cp_ptr->spells; */

	C_FREE(classes, z_info->c_max, birth_menu);

	return (TRUE);
}




/*
 * Player sex
 */
static bool get_player_sex(void)
{
	int i;
	birth_menu genders[MAX_SEXES];

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'sex' does not have any significant gameplay effects.");

	/* Tabulate genders */
	for (i = 0; i < MAX_SEXES; i++)
	{
		genders[i].name = sex_info[i].title;
		genders[i].ghost = FALSE;
	}

	p_ptr->psex = get_player_choice(genders, MAX_SEXES, SEX_COL, 15,
                                    NULL);

	/* No selection? */
	if (p_ptr->psex == INVALID_CHOICE)
	{
		p_ptr->psex = 0;
		return (FALSE);
	}

	/* Save the sex pointer */
	sp_ptr = &sex_info[p_ptr->psex];

	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * modify options (including the birth options).
 */
static bool player_birth_aux_1(void)
{
	/*** Instructions ***/
	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
	            "Please select your character from the menu below:");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW, -1, TERM_WHITE,
	            "Use the movement keys to scroll the menu, Enter to select the current");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW + 1, -1, TERM_WHITE,
	            "menu item, '*' for a random menu item, 'ESC' to restart the character");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW + 2, -1, TERM_WHITE,
	            "selection, '=' for (birth) options, '?' for help, or 'Ctrl-X' to quit.");

	/* Hack - highlight the key names */
	Term_putstr(QUESTION_COL + 8, INSTRUCT_ROW, - 1, TERM_L_GREEN, "movement keys");
	Term_putstr(QUESTION_COL + 42, INSTRUCT_ROW, - 1, TERM_L_GREEN, "Enter");
	Term_putstr(QUESTION_COL + 12, INSTRUCT_ROW + 1, - 1, TERM_L_GREEN, "*");
	Term_putstr(QUESTION_COL + 40, INSTRUCT_ROW + 1, - 1, TERM_L_GREEN, "ESC");
	Term_putstr(QUESTION_COL + 12, INSTRUCT_ROW + 2, - 1, TERM_L_GREEN, "=");
	Term_putstr(QUESTION_COL + 37, INSTRUCT_ROW + 2, - 1, TERM_L_GREEN, "?");
	Term_putstr(QUESTION_COL + 54, INSTRUCT_ROW + 2, - 1, TERM_L_GREEN, "Ctrl-X");

	/* Choose the player's sex */
	if (!get_player_sex()) return (FALSE);

	/* Clean up */
	clear_question();

	/* Choose the player's race */
	if (!get_player_race()) return (FALSE);

	/* Clean up */
	clear_question();

	/* Choose the player's class */
	if (!get_player_class()) return (FALSE);

	/* Clear */
	Term_clear();

	/* Done */
	return (TRUE);
}

/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "auto-rolling" and "random-rolling".
 *
 * The delay may be reduced, but is recommended to keep players
 * from continuously rolling up characters, which can be VERY
 * expensive CPU wise.  And it cuts down on player stupidity.
 */
static bool player_birth_aux_3(void)
{

	bool prev = FALSE;

	char ch;

	char b1 = '[';
	char b2 = ']';

	/*** Generate ***/

	/* do Astral Mode */
	astral_mode();

	/* Hack -- prepare the skills */
	skill_raceinit();
	skill_classinit();
	
	/* MEGA Hack -- Condense extra skills */
	skill_cleanup();

	/* Roll */
	while (TRUE)
	{
		int col = 42;
		
		/* Get a new character */
		get_stats();
		
		/* Flush input */
		flush();

		/*** Display ***/

		/* Roll for base hitpoints */
		get_extra();

		/* Roll for age/height/weight */
		get_ahw();

		/* Roll for social class */
		get_history();

		/* Roll for gold */
		get_money();

		/* Input loop */
		while (TRUE)
		{
			/* Calculate the bonuses and hitpoints */
			p_ptr->update |= (PU_BONUS | PU_HP);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;

			/* Display the player */
			display_player(99);

			/* Prepare a prompt (must squeeze everything in) */
			Term_gotoxy(2, 23);
			Term_addch(TERM_WHITE, b1);
			Term_addstr(-1, TERM_WHITE, "'r' to reroll");
			if (prev) Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
			Term_addstr(-1, TERM_WHITE, ", or Enter to accept");
			Term_addch(TERM_WHITE, b2);

			/* Prompt and get a command */
			ch = inkey();

			/* Quit */
			if (ch == KTRL('X')) quit(NULL);

			/* Start over */
			if (ch == ESCAPE) return (FALSE);

			/* Enter accepts the roll */
			if ((ch == '\r') || (ch == '\n')) break;

			/* Reroll this character */
			if ((ch == ' ') || (ch == 'r')) break;

			/* Previous character */
			if (prev && (ch == 'p'))
			{
				load_prev_data();
				continue;
			}

			/* Help */
			if (ch == '?')
			{
				do_cmd_help();
				continue;
			}

			/* Warning */
			bell("Illegal auto-roller command!");
		}

		/* Are we done? */
		if ((ch == '\r') || (ch == '\n')) break;

		/* Save this for the "previous" character */
		save_prev_data();

		/* Note that a previous roll exists */
		prev = TRUE;
	}

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
static bool player_birth_full(void)
{
	char ch;
	
	/* Wipe the player */
	player_wipe();

	/* Ask questions */
	if (!player_birth_aux_1()) return (FALSE);

	/* Point-based */
/*	if (adult_point_based)
 *	{
 */		/* Point based */
/*		if (!player_birth_aux_2()) return (FALSE);
 *	}
 *
 */	/* Random */
/*	else
 *	{
 */		/* Auto-roll */
/*	}  */
	if (!player_birth_aux_3()) return (FALSE);

	/* Get a name, prepare savefile */
	get_name();

	/* Display the player */
	display_player(99);

	/* Prompt for it */
	prt("['CTRL-X' to quit, 'ESC' to start over, or any other key to continue]", 23, 5);

	/* Get a key */
	ch = inkey();

	/* Quit */
	if (ch == KTRL('X')) quit(NULL);

	/* Start over */
	if (ch == ESCAPE) return (FALSE);

	/* Accept */
	return (TRUE);
}


static bool player_birth_quick(void)
{
	char ch;
	int i;
	birther old_char;
	byte old_class;
	byte old_race;
	byte old_sex;
	byte old_hitdie;
	u16b old_expfact;
	s16b old_hp[PY_MAX_LEVEL];

	old_sex = p_ptr->psex;
	old_class = p_ptr->pclass;
	old_race = p_ptr->prace;

	old_hitdie = p_ptr->hitdie;
	old_expfact = p_ptr->expfact;

	old_char.age = p_ptr->age;
	old_char.au = p_ptr->au_birth;
	old_char.ht = p_ptr->ht;
	old_char.wt = p_ptr->wt;
	old_char.sc = p_ptr->sc;
	
	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		old_char.stat[i] = p_ptr->stat_birth[i];
	}

	/* Save the history */
	for (i = 0; i < 5; i++)
	{
		strcpy(old_char.history[i], p_ptr->history[i]);
	}

	/* Save the hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		old_hp[i] = p_ptr->player_hp[i];
	}

	/* Wipe the player */
	player_wipe();

	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	p_ptr->psex = old_sex;
	p_ptr->pclass = old_class;
	p_ptr->prace = old_race;

	p_ptr->hitdie = old_hitdie;
	p_ptr->expfact = old_expfact;

	p_ptr->age = old_char.age;
	p_ptr->au_birth = p_ptr->au = old_char.au;
	p_ptr->ht = old_char.ht;
	p_ptr->wt = old_char.wt;
	p_ptr->sc = old_char.sc;
	
	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i] = old_char.stat[i];
	}

	/* Load the history */
	for (i = 0; i < 5; i++)
	{
		strcpy(p_ptr->history[i], old_char.history[i]);
	}

	/* Load the hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		p_ptr->player_hp[i] = old_hp[i];
	}
	
	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Update stuff */
	update_stuff();

	/* Fully healed */
	p_ptr->chp = p_ptr->mhp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;
	
	/* Hack -- Give the player some inital skill points */
	p_ptr->free_skpts = rand_range(14, 18);
	
	/* Hack -- prepare the skills */
	skill_raceinit();
	skill_classinit();
	
	/* MEGA Hack -- Condense extra skills */
	skill_cleanup();

	/* Roll for gold */
	get_money();
	
	/* do Astral Mode */
	astral_mode();

	/* Display the player */
	display_player(99);

	/* Get a name, prepare savefile */
	get_name();

	/* Prompt for it */
	prt("['CTRL-X' to quit, 'ESC' to start over, or any other key to continue]", 23, 5);

	/* Get a key */
	ch = inkey();

	/* Quit */
	if (ch == KTRL('X')) quit(NULL);

	/* Start over */
	if (ch == ESCAPE) return (FALSE);

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
	int i, n;
	bool do_full = TRUE;
	char buf[80];
	char ch;
	
	if (character_existed) while (TRUE)
	{
		/* Verify birth options */
		while (TRUE)
		{
			Term_clear();

			sprintf(buf, "Quick-start character based on previous one (y/n)? ");
			put_str(buf, 2, 2);
			ch = inkey();
			if (ch == KTRL('X')) quit(NULL);
			if ((ch == ESCAPE) || (ch == '\r') || (ch == '\n')) break;
			if (ch == 'y' || ch == 'n') break;
			if (ch == '?') do_cmd_help();
			else bell("Illegal answer!");
		}

		/* Verify */
		if (ch == 'y')
		{
			/* Quick creation */
			do_full = FALSE;
		}

		if (ch == 'n')
		{
			do_full = TRUE;
		}

		if (do_full) break;

		if (player_birth_quick()) break;
	}
		
	if (do_full) while (TRUE)
	{
		/* Roll up a new character */
		if (player_birth_full()) break;
	}

    /* Note player birth in the message recall */
	message_add(" ", MSG_GENERIC);
	message_add("You awake to find yourself in a strange dank place. You", MSG_GENERIC);
	message_add("are unsure of where you are, and cannot recall how you", MSG_GENERIC);
	message_add("came to find yourself in this nightmareish place.", MSG_GENERIC);
	message_add("There are several buildings nearby in the cavern and", MSG_GENERIC);
	message_add("there appear to be people milling about. Best to equip", MSG_GENERIC);
	message_add("yourself and look for a way out of this prison.", MSG_GENERIC);
	message_add("	As you step forward, a memory comes flashing back;", MSG_GENERIC);
	message_add("A memory of long, magnetic eyes with a true cat-green ", MSG_GENERIC);
	message_add("fire that burns from within. . . ", MSG_GENERIC);
	message_add("==================== ", MSG_GENERIC);
	message_add(" ", MSG_GENERIC);
	
	/* Hack -- outfit the player */
	/* Hack -- if race is Old One, skip starting equipment DH*/
	if (p_ptr->prace != RACE_OLD_ONE)
	{
		player_outfit();
	}
		
	/* Shops */
	for (n = 0; n < MAX_STORES; n++)
	{
		/* Initialize */
		store_init(n);

		/* Ignore home */
		if (n == STORE_HOME) continue;

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++) store_maint(n);
	}
}

