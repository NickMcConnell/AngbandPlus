/* File: birth.c */
/* Purpose: Character Generation Code */

/* 
 * birth.c contains information, processes, methods and data related to
 * creating and storing birth infomation (including, but not limited to
 * birth statistics, age, height, weight, social class, race, and class), 
 * setting wonderland mode for beginning characters, wiping all player 
 * data, handing out starting equipment to characters, selection of sex,
 * race, and class, and the player birth process.
 *
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

/* A structure "birther" to hold "rolled" information */
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

/* A Structure "birth_menu" to hold the menus */
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




/*
 * This is the rewritten 'get_stats' function for the new stat system. 
 * It basically works by taking a integer representing a rate of 
 * stat increase from the race and taking an integer representing
 * a rate of stat increase from the class, adding them together 
 * and using that as the rate of increase for the stat throughout 
 * the game. Every time the stat is raised, it is raised 1-X points 
 * where X is this value. 
 *
 * There are now several skills that increase this rate of
 * advancement. Obviously it helps to put points into those skills
 * early, but the benifited advancement also works for potions.
 *
 * The calculation in inc_stat from spells3.c is no longer identical
 * to the calculation below for totalside. It currently uses a 
 * straight average of randint(totalside) + randint(totalside) / 2
 *
 * (Possibly lower this value and allow the player to alter it
 * by the food they eat, among a variety of other things) XCCCX
 * 
 * rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
 *
 */
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
		roll1 = (Rand_normal(totalside, 2));

		/* reduce variance */
		roll2 = (Rand_normal(totalside, 2));

		/* reduce variance */
		roll3 = (Rand_normal(totalside, 2));
 		
 		/* Get a random integer between those two numbers three times */
 		statroll = (roll1 * 3) + (roll2 * 3) + (roll3 * 3);
 		
 		if (statroll > 700) statroll = 700;
 		
 		/* save that value */
 		p_ptr->stat_max[i] = statroll;
 		
 		/* Start fully healed */
 		stat_use[i] = p_ptr->stat_birth[i] = 
 			p_ptr->stat_cur[i] = p_ptr->stat_max[i];
	}
}

/* 
 * Astral / Wonderland mode added by DH - Stolen from gumband 2.2.2 
 * Darren Hart is awesome for writing/stealing this for Steamband   
 *
 * Wonderland mode starts the character on level 48 of the dungeon.
 * (Level 48 of the dungeon is 150', or 3 levels away from the 'surface'
 * not the 'town') No down stairs or pits can be created past level
 * 48 until the 'town' is reached. (at level 0 internally, level 51
 * to the player, or 2550 feet.)
 *
 * Wonderland characters get several bonuses including increased speed
 * and extra supplie, but _not_ including the ability to pass 
 * through walls. This deficit renders wonderland almost totally 
 * unplayable. The eventual goal for 0.5.0 is to change the dungeon /  
 * monster / item generation for wonderland characters to create
 * a unique game (of falling down the rabbit hole, as it were).
 *
 * I do not hesitate to remind my players that patience is a virtue.
 *
 */
static void wonderland_mode(void)
{
	char c;

	/*** Ghostly Status -- from Gumband ***/

	/* Extra info */
	Term_putstr(5, 13, -1, TERM_WHITE,
	 "Starting in wonderland mode makes you begin the game just below ");
	Term_putstr(5, 14, -1, TERM_WHITE,
	 "the surface.  You must make your way from there down to the town.");
	Term_putstr(5, 15, -1, TERM_WHITE,
	 "You will then have to make your way back up to the surface to");
	Term_putstr(5, 16, -1, TERM_WHITE,
	 "confront the source of evil to win the game.  NOTE: Wonderland");
	Term_putstr(5, 17, -1, TERM_WHITE,
	 " mode is currently broken. It is unlikely with current routines");
	Term_putstr(5, 18, -1, TERM_WHITE,
	 "that any monster will be generated that you can kill.");

	while (1)
	{
		put_str("Start in Wonderland mode? (y/n) ", 20, 2);
		c = inkey();
		if (c == ESCAPE) break;
		if ((c == 'y') || (c == 'n')) break;
	}

	/* Set "ghost" mode */
	if (c == 'y')
	{
		p_ptr->wonderland = TRUE;
		p_ptr->was_wonderland = FALSE;
		p_ptr->wonderland_start = TRUE;
	}
	else
	{
		p_ptr->wonderland = FALSE;
		p_ptr->was_wonderland = FALSE;
		p_ptr->wonderland_start = FALSE;
	}

	/* Clear */
	clear_from(13);
}


/*
 * Roll for some info that the auto-roller ignores.
 * Calculates your 'hit points' out over fifty levels
 * but doesn't calculate your 'wound points' out that 
 * way. I believe this was originally instituted to 
 * prevent 'cheating' from scumming a savefile.
 *
 * Woundpoints are instituited below and unlike
 * hit points are not random. This means they are 
 * calculated in mutiple places. Be careful to
 * rewrite the function everywhere it's calculated
 *
 * Currently the only things that affect woundpoints
 * differently are things that are available after birth -
 * skills, health amulets, stat increases, etc.
 * at which point the function in xtra1.c should have
 * taken over. 
 *
 * Wound points are only calculated in xtra1.c in the
 * function calc_hitpoints().
 *
 * Post to rgra regarding hit point storage XCCCX
 */
static void get_extra(void)
{
	int i, j, min_value, max_value;

	/* All stat mods come at the top of the function */
	int vigor_adjustment = p_ptr->stat_use[A_VIG] / 30;

	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Initial woundpoints -- Constant Value */
	p_ptr->mwp = 1 + vigor_adjustment + (p_ptr->lev / 2);

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
	social_class = randint(24);

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
 * Computes character's age, height, and weight.
 *
 * This function now should correctly calculate player weights 
 * and heights. It is mathmatically possible for values to 
 * be created that have negative values, but it is currently
 * extremely unlikely.
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

	/* Reset the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Reset level */
		if ((q_ptr->type == QUEST_FIXED) || (q_ptr->type == QUEST_FIXED_U))
		{
			q_ptr->active_level = q_ptr->base_level;
			q_ptr->cur_num = 0;
		}
		else
		{
			q_ptr->type = 0;
			q_ptr->mon_idx = 0;
			q_ptr->base_level = 0;
			q_ptr->active_level = 0;
			q_ptr->cur_num = 0;
			q_ptr->max_num = 0;
			q_ptr->reward = 0;
		}
	}

	/* No current quest */
	p_ptr->cur_quest = 0;

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

	/* set Skills to default */
	for (i = 0; i < N_SKILLS; i++)
	{
		p_ptr->skills[i].skill_rank = skills[i].skill_rank;
		p_ptr->skills[i].skill_max = skills[i].skill_max;
		p_ptr->skills[i].skill_raise = skills[i].skill_raise;
		p_ptr->skills[i].skill_index = skills[i].skill_index;
		p_ptr->skills[i].skill_type = skills[i].skill_type;
	}
	
	/* Set Steamware to default -- paranoia */
	p_ptr->eyes_research = 0;
	p_ptr->eyes_level = 0;
	p_ptr->reflex_research = 0;
	p_ptr->reflex_level = 0;
	p_ptr->plate_research = 0;
	p_ptr->plate_level = 0;
	p_ptr->core_research = 0;
	p_ptr->core_level = 0;
	p_ptr->spur_research = 0;
	p_ptr->spur_level = 0;
		
	/* Hack -- no ghosts */
	r_info[z_info->r_max-1].max_num = 0;

	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;
	
	/* Hack -- Give the player some inital skill points */
	p_ptr->free_skpts = rand_range(18, 22);
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

	bool automata;

	const start_item *e_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	/* calculate social class type */
	social_class_type = p_ptr->sc / 5;
	
	/* determine living state */
	if ((p_ptr->prace == RACE_AUTOMATA) || 
		(p_ptr->prace == RACE_STEAM_MECHA))
	{
		automata = TRUE;
	}
	else automata = FALSE;

	/* Wonderland Mode treats social_class_type as "middle" (type 4) */
	if (p_ptr->wonderland) social_class_type = 4;

	/* Get local object */
	i_ptr = &object_type_body;
	object_prep(i_ptr, lookup_kind(TV_FLASK, 0));

	/* Hack -- Give the player some food based on Social Class*/
	switch (social_class_type)
	{
		case 1:
			if (!automata) object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_ONION));
			i_ptr->number = (byte)rand_range(1, 5);
			break; 
		case 2: case 3:
			if (!automata) object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_POTATO));
			i_ptr->number = (byte)rand_range(2, 5);
			break;
		case 4: case 5: case 6: case 7:
			if (!automata) object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_HADDOCKS));
			i_ptr->number = (byte)rand_range(2, 6);
			break;
		case 17: case 16: case 15: case 14:
			if (!automata) object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_CHEESE));
			i_ptr->number = (byte)rand_range(8, 12);
			break;
		case 19: case 18:
			if (!automata) object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_PIGEON_PIE));
			i_ptr->number = (byte)rand_range(6, 9);
			break;
		case 20:
			if (!automata) object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_MEAT_PIE));
			i_ptr->number = (byte)rand_range(4, 8);
			break;
		default: /*"Middle Class" type 8-13 is default */
			if (!automata) object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_MEAT_PIE));
			i_ptr->number = (byte)rand_range(3, 6);
			break;
	}

	/* RMG - increase the number of food items character gets for wonderland*/
	if (p_ptr->wonderland) 
		i_ptr->number += 5;

	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);

	/* Start off with lots of ID scrolls if wonderland*/
	if (p_ptr->wonderland)
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

	/* Hack -- Give the player some light based off social class */
	if (!p_ptr->wonderland)
	{ 	
		switch (social_class_type)
		{ 
			/*start switch */
			case 1:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TAPER));
				apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);
				i_ptr->number = (byte)rand_range(1, 5);
				i_ptr->pval = rand_range(1, 6) * 150;
				break; 
			case 2: case 3:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_CANDLE_TALLOW));
				apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);
				i_ptr->number = (byte)rand_range(2, 5);
				i_ptr->pval = rand_range(1, 6) * 300;
				break;
			case 4: case 5: case 6: case 7:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_CANDLE_WAX));
				apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);
				i_ptr->number = (byte)rand_range(2, 6);
				i_ptr->pval = rand_range(2, 7) * 400;
				break;
			case 17: case 16: case 15: case 14:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
				apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);
				i_ptr->number = (byte)rand_range(3, 7);
				i_ptr->pval = rand_range(6, 9) * 500;
				break;
			case 19: case 18:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_LANTERN));
				apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);
				i_ptr->number = 1;
				i_ptr->pval = rand_range(1, 7) * 400;
				break;
			case 20:
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_LANTERN));
				apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);
				i_ptr->number = 1;
				i_ptr->pval = rand_range(4,8) * 600;
				break;
			default: /*type 8-13 - Middle Class is default */
				object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
				apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);
				i_ptr->number = (byte)rand_range(3, 6);
				i_ptr->pval = rand_range(5, 8) * 500;
				break;
		} 
		/*end switch */
	}

	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);
	
	/* Hack -- Force Automata and Steam Mecha to equip */
	/* torsos (from Animeband) -DH */
	if (automata)
	{
		/* Body */
		object_prep(i_ptr, lookup_kind(TV_MECHA_TORSO, SV_MECHA_TORSO_TIN));
		i_ptr->number = 1;
		i_ptr->pval = 1;
		object_aware(i_ptr);
		object_known(i_ptr);
		object_copy(&inventory[INVEN_BODY], i_ptr);
		
		/* Head */
		object_prep(i_ptr, lookup_kind(TV_MECHA_HEAD, SV_MECHA_HEAD_TIN));
		i_ptr->number = 1;
		i_ptr->pval = 1;
		object_aware(i_ptr);
		object_known(i_ptr);
		object_copy(&inventory[INVEN_HEAD], i_ptr);

		/* Arms */
		object_prep(i_ptr, lookup_kind(TV_MECHA_ARMS, SV_MECHA_ARMS_TIN));
		i_ptr->number = 1;
		i_ptr->pval = 1;
		object_aware(i_ptr);
		object_known(i_ptr);
		object_copy(&inventory[INVEN_HANDS], i_ptr);

		/* Legs */
		object_prep(i_ptr, lookup_kind(TV_MECHA_FEET, SV_MECHA_FEET_TIN));
		i_ptr->number = 1;
		i_ptr->pval = 1;
		object_aware(i_ptr);
		object_known(i_ptr);
		object_copy(&inventory[INVEN_FEET], i_ptr);

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

			/* Hack - Check for Automata status and tonics */
			if ((automata) && (e_ptr->tval == 75))
			{
				/* *Identify* should be a nicer than any tonic */
				object_prep(i_ptr, lookup_kind(TV_MECHANISM, 
									SV_MECHANISM_STAR_IDENTIFY));
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

			/* RMG - give some ammo for the gun */
			if (e_ptr->tval == TV_GUN)
			{
				if (i_ptr->ammo_tval == TV_AMMO)
				{
					object_prep(i_ptr, lookup_kind(TV_AMMO, SV_AMMO_LIGHT));
					i_ptr->number = (byte)rand_range(18, 20);
				}
				if (i_ptr->ammo_tval == TV_BULLET)
				{
					object_prep(i_ptr, lookup_kind(TV_BULLET, SV_AMMO_NORMAL));
					i_ptr->number = (byte)rand_range(16, 18);
				}
				if (i_ptr->ammo_tval == TV_SHOT)
				{
					object_prep(i_ptr, lookup_kind(TV_SHOT, SV_AMMO_BSHOT));
					i_ptr->number = (byte)rand_range(14, 16);
				}
				/* evil me, this ammo can't be sold, only used */
				/* TODO: actually there is a problem with this (and generated ammo) and memory loss */
				i_ptr->ident |= (IDENT_BROKEN);
				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
			}

		}
	}
	/* Hack -- Prevent allocation of mech items for non mecha characters. */
	if (!automata)
	{
	    /* Scan the items */
	    for (i = 1; i < z_info->k_max; i++)
	    {
			object_kind *k_ptr = &k_info[i];
			
	        /* Never generate mecha gen items if not mecha gen */
	        if (k_ptr->flags3 & (TR3_MECHA_GEN))
	        {
				permit_kind_table[i] = FALSE;
	        }	
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
                             cptr helpfile, void (*hook)(birth_menu))
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
			strnfmt(buf, sizeof(buf), "%s#%s", helpfile, choices[cur].name);

			screen_save();
			(void)show_file(buf, NULL, 0, 0);
			screen_load();
			/* do_cmd_help(); */
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
		/* and the adjustment for each stat. */
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
		"races.txt", race_aux_hook);

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
		sprintf(s, "%s d%2d %sd%d   ", stat_names_reduced[i],
		c_info[class_idx].c_adj[i], "Statgrowth:", (c_info[class_idx].c_adj[i] + p_info[p_ptr->prace].r_adj[i]));
		if (c_info[class_idx].spell_stat == i) Term_putstr(CLASS_AUX_COL, TABLE_ROW + i, -1, TERM_L_BLUE, s);
		else Term_putstr(CLASS_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
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
                                      "classes.txt", class_aux_hook);

	/* No selection? */
	if (p_ptr->pclass == INVALID_CHOICE)
	{
		p_ptr->pclass = 0;

		return (FALSE);
	}

	/* Set class */
	cp_ptr = &c_info[p_ptr->pclass];
	
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
                                    "birth.txt", NULL);

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

	/* do wonderland Mode */
	wonderland_mode();

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
			
			/* Fully Healed */
			p_ptr->cwp = p_ptr->mwp;

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
	for (i = 0; i < 4; i++)
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
	for (i = 0; i < 4; i++)
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

	p_ptr->cwp = p_ptr->mwp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;
	
	/* Hack -- Give the player some inital skill points */
	p_ptr->free_skpts = rand_range(18, 22);
	
	/* Hack -- prepare the skills */
	skill_raceinit();
	skill_classinit();
	
	/* MEGA Hack -- Condense extra skills */
	skill_cleanup();

	/* Roll for gold */
	get_money();
	
	/* do wonderland Mode */
	wonderland_mode();

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
	message_add("You awake to find yourself in a strange dank place. Unsure", MSG_GENERIC);
	message_add("of your location, you cannot recall how you came", MSG_GENERIC);
	message_add("to find yourself in this nightmareish cavern.", MSG_GENERIC);
	message_add("   There are several buildings nearby and there", MSG_GENERIC);
	message_add("appear to be people milling about. Best to equip", MSG_GENERIC);
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

