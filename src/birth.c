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
typedef struct birth_menu birth_menu;

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

	byte stat[A_MAX];

	char history[5][55];
};

/*
 * A structure to hold the menus
 */
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
static byte stat_use[A_MAX];

/*
 * Additional items in the "start kit"
 */
static start_item start_kit[5] =
{
	{TV_LITE, SV_LANTERN, 0, 1, 1},
	{TV_FLASK, SV_FLASK_LANTERN, 0, 4, 4},
	{TV_CLOAK, SV_CLOAK, 0, 1, 1},
	{TV_SCROLL, SV_SCROLL_PHASE_DOOR, 0, 1, 1},
	{TV_POTION, SV_POTION_CURE_LIGHT, 0, 1, 1}
};

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
	for (i = 0; i < 5; i++)
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
	for (i = 0; i < 5; i++)
	{
		strcpy(temp.history[i], p_ptr->history[i]);
	}

	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->age = prev.age;
	p_ptr->wt = prev.wt;
	p_ptr->ht = prev.ht;
	p_ptr->sc = prev.sc;
    p_ptr->au_birth = p_ptr->au = prev.au;

	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat_birth[i] = p_ptr->stat_max[i] = p_ptr->stat_cur[i] = prev.stat[i];
	}

	/* Load the history */
	for (i = 0; i < 5; i++)
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
	for (i = 0; i < 5; i++)
	{
		strcpy(prev.history[i], temp.history[i]);
	}
}

/*
 * Roll for a characters stats
 */
static void get_stats(void)
{
	int i;
	int j = 0;

	int dice[18];

	/* Roll and verify some stats */
	while ((j < 42) || (j > 54))
	{
		/* Roll some dice */
		for (j = i = 0; i < 18; i++)
		{
			/* Roll the dice */
			dice[i] = randint(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}
	}

	/* Roll the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Extract 2 + 1d3 + 1d4 + 1d5 */
		j = 2 + dice[3 * i] + dice[3 * i + 1] + dice[3 * i + 2];

		/* Apply the class bonus */
		stat_use[i] = modify_stat_value((byte)j, cp_ptr->c_adj[i]);
		
		/* Save the resulting stat maximum */
		p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_use[i];
	}
}

/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	int i;

	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Pre-calculate level 1 hitdice */
	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* 
	 * Hack - Get the hitpoints for non-random hp characters.  
	 * Each level provides exactly average hitpoints.
	 * If the average is a fraction, alternate.
	 */
	if (!adult_random_hp)
	{
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			p_ptr->player_hp[i] = p_ptr->player_hp[i - 1] + (p_ptr->hitdie + 1) / 2;
            if (((p_ptr->hitdie % 2) == 0) & ((i % 2) == 1)) p_ptr->player_hp[i]++;
		}

	}
	/* Hack, if the player wants, use old-style rolled hitpoints */
	else
	{
		int min_value, max_value;

		/* Minimum hitpoints at highest level */
		min_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 3) / 8;
		min_value += PY_MAX_LEVEL;

		/* Maximum hitpoints at highest level */
		max_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 5) / 8;
		max_value += PY_MAX_LEVEL;

		/* Roll out the hitpoints */
		while (TRUE)
		{
			/* Roll the hitpoint values */
			for (i = 1; i < PY_MAX_LEVEL; i++)
			{
				p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + randint(p_ptr->hitdie);
			}

			/* XXX Could also require acceptable "mid-level" hitpoints */
			
			/* Require "valid" hitpoints at highest level */
			if (p_ptr->player_hp[PY_MAX_LEVEL-1] < min_value) continue;
			if (p_ptr->player_hp[PY_MAX_LEVEL-1] > max_value) continue;

			/* Acceptable */
			break;
		}
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
	for (i = 0; i < 5; i++) p_ptr->history[i][0] = '\0';

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
		if (n < 55)
		{
			/* Save one line of history */
			strcpy(p_ptr->history[i++], s);

			/* All done */
			break;
		}

		/* Find a reasonable break-point */
		for (n = 55; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

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
	int i;

	int gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint(100) + ((adult_start_kit) ? 200 : 300);

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (stat_use[i] >= 19) gold -= 300;
		else if (stat_use[i] >= 17) gold -= 200;
		else if (stat_use[i] > 15) gold -= 150;
		else gold -= (stat_use[i] - 5) * 10;
	}

	/* Minimum 100 gold (or 0 if using start kit)*/
	if (!adult_start_kit)
	{
		if (gold < 100) gold = 100;
	}
	else if (gold < 0) gold = 0;

	/* 500 Bonus gold coins for easy_mode */
	if (adult_easy_mode) gold += 500;

	/* Save the gold */
	p_ptr->au_birth = p_ptr->au = gold;
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
	for (i = 0; i < INVEN_MAX; i++)
	{
		object_wipe(&inventory[i]);
	}

	/* Start with no artifacts made yet */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->status &= ~(A_STATUS_CREATED | A_STATUS_AWARE | A_STATUS_KNOWN | A_STATUS_LOST);
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

	/* Reset the "alchemy" knowledge */
	for (i = 0; i < SV_POTION_MAX ; i++)
	{
		potion_alch[i].known1 = FALSE;
		potion_alch[i].known2 = FALSE;
	}

	/* Reset the "monsters" */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *lr_ptr = &lr_list[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the unique counter */
		r_ptr->cur_unique = r_ptr->max_unique;

		/* Clear player kills */
		lr_ptr->r_pkills = 0;
	}

	/* Reset the "uniques" */
	for (i = 1; i < z_info->u_max; i++)
	{
		monster_unique *u_ptr = &u_info[i];
		monster_lore *lu_ptr = &lu_list[i];

		/* Hack -- Reset the depth */
		u_ptr->depth = -1;

		/* Hack -- Reset the living */
		u_ptr->dead = FALSE;

		/* Clear player kills */
		lu_ptr->r_pkills = 0;
	}

	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

	/* None of the spells have been learned yet */
	for (i = 0;i < (SV_BOOK_MAX * MAX_BOOK_SPELLS); i++)
	{
		p_ptr->spell_order[i][0] = 99;
		p_ptr->spell_order[i][1] = 99;
	}
}

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i;

	object_type *i_ptr;
	object_type object_type_body;
	const start_item *e_ptr;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some food */
	object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
	i_ptr->number = (byte)rand_range(3, 7);
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);

	/* Get local object */
	i_ptr = &object_type_body;

	if (adult_start_kit)
	{
		/* Hack -- Give the player the starting "kit" */
		for (i = 0; i < 5; i++)
		{
			/* Access the item */
			e_ptr = &start_kit[i];
		
			/* Get local object */
			i_ptr = &object_type_body;

			/* Hack	-- Give the player an object */
			if (e_ptr->tval > 0)
			{	
				object_prep(i_ptr, lookup_kind(e_ptr->tval, e_ptr->sval));

				/* Hack - make sure that lanterns have some oil in them */
				if ((e_ptr->tval == TV_LITE) && (e_ptr->sval == SV_LANTERN))
					i_ptr->timeout = 7500;

				i_ptr->number = rand_int(e_ptr->max - e_ptr->min) + e_ptr->min;

				object_aware(i_ptr);
				object_known(i_ptr);
				(void)inven_carry(i_ptr);
			}
		}
	}

	else
	{	
		/* Hack -- Give the player some torches */
		object_prep(i_ptr, lookup_kind(TV_LITE, SV_TORCH));
		i_ptr->number = (byte)rand_range(3, 7);
		i_ptr->timeout = rand_range(3, 7) * 500;
		object_aware(i_ptr);
		object_known(i_ptr);
		(void)inven_carry(i_ptr);
	}

	/* Hack -- Give the player his equipment */
	for (i = 0; i < MAX_START_ITEMS; i++)
	{
		/* Access the item */
		e_ptr = &cp_ptr->start_items[i];
		
		/* Get local object */
		i_ptr = &object_type_body;

		/* Hack	-- Give the player an object */
		if (e_ptr->tval > 0)
		{
			/* Get the object_kind */
			int k_idx = lookup_kind(e_ptr->tval, e_ptr->sval);

			/* Valid item? */
			if (!k_idx) continue;

			/* Prepare the item */
			object_prep(i_ptr, k_idx);
			i_ptr->number = (byte)rand_range(e_ptr->min, e_ptr->max);

			/* make ego item if necessary */
			if (e_ptr->ego) i_ptr->e_idx = e_ptr->ego;

			/* hack - give prefix if applicable */
			if ((rp_ptr->prefix) && ((i_ptr->tval == TV_SWORD) || (i_ptr->tval == TV_HAFTED) ||
				(i_ptr->tval == TV_POLEARM))) i_ptr->prefix_idx = rp_ptr->prefix;

			/* Mark History */
			i_ptr->origin_nature = ORIGIN_BIRTH;

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
#define RACE_AUX_COL    29
#define CLASS_COL		29
#define CLASS_AUX_COL   50

#define INVALID_CHOICE 255


/*
 * Clear the previous question
 */
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
	int i, dir;
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
			sprintf(buf, "%s#%s", helpfile, choices[cur].name);

			screen_save();
			(void)show_file(buf, NULL, 0, 0);
			screen_load();
		}
		else if (c == '=')
		{
			options_birth_menu(TRUE);
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
		sprintf(s, "%s%+d", stat_names_reduced[i],
		p_info[race].r_adj[i]);
		Term_putstr(RACE_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}

	sprintf(s, "Hit die: %d ", p_info[race].r_mhp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX, -1, TERM_WHITE, s);
	sprintf(s, "Experience: %d%% ", p_info[race].r_exp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 1, -1, TERM_WHITE, s);
	sprintf(s, "Infravision: %d ft ", p_info[race].infra * 10);
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
	for (i = 0; i < RACE_SPECIAL_LEVELS; i++)
		rsp_ptr[i] = &race_special_info[(rp_ptr->special) - 1][i];

	FREE(races);

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
		sprintf(s, "%s%+d", stat_names_reduced[i],
		c_info[class_idx].c_adj[i]);
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
                                      "classes.txt", class_aux_hook);

	/* No selection? */
	if (p_ptr->pclass == INVALID_CHOICE)
	{
		p_ptr->pclass = 0;

		return (FALSE);
	}

	/* Set class */
	cp_ptr = &c_info[p_ptr->pclass];

	FREE(classes);

	return (TRUE);
}

/*
 * Player sex
 */
static bool get_player_sex(void)
{
	int i;
	birth_menu genders[SEX_MAX];

	/* Extra info */
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'sex' does not have any significant gameplay effects.");

	/* Tabulate genders */
	for (i = 0; i < SEX_MAX; i++)
	{
		genders[i].name = sex_info[i].title;
		genders[i].ghost = FALSE;
	}

	p_ptr->psex = get_player_choice(genders, SEX_MAX, SEX_COL, 15,
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
	            "selection, '=' for the birth options, '?' for help, or 'Ctrl-X' to quit.");

	/* Hack - highlight the key names */
	Term_putstr(QUESTION_COL + 8, INSTRUCT_ROW, - 1, TERM_L_GREEN, "movement keys");
	Term_putstr(QUESTION_COL + 42, INSTRUCT_ROW, - 1, TERM_L_GREEN, "Enter");
	Term_putstr(QUESTION_COL + 12, INSTRUCT_ROW + 1, - 1, TERM_L_GREEN, "*");
	Term_putstr(QUESTION_COL + 40, INSTRUCT_ROW + 1, - 1, TERM_L_GREEN, "ESC");
	Term_putstr(QUESTION_COL + 12, INSTRUCT_ROW + 2, - 1, TERM_L_GREEN, "=");
	Term_putstr(QUESTION_COL + 39, INSTRUCT_ROW + 2, - 1, TERM_L_GREEN, "?");
	Term_putstr(QUESTION_COL + 56, INSTRUCT_ROW + 2, - 1, TERM_L_GREEN, "Ctrl-X");

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
 * Initial stat costs (initial stats always range from 6 to 15 inclusive).
 */
static int birth_stat_costs[(15 - 6) + 1] = { 0, 1, 2, 3, 5, 8, 12, 17, 23, 31 };

/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "point-based" character creation.
 *
 * The player selects, for each stat, a value from 6 to 15 (inclusive),
 * each costing a certain amount of points (as above), from a pool of 50
 * available points, to which race/class modifiers are then applied.
 *
 * Each unused point is converted into 50 gold pieces.
 */
#define POINTS		55
#define BASE_STAT	6

static bool player_birth_aux_2(void)
{
	int i;

	int row = 2;
	int col = 42;

	byte stat = 0;

	byte stats[A_MAX];

	int cost;

	char ch;

	char buf[80];

	/* Initialize stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Initial stats */
		stats[i] = BASE_STAT;
	}

	/* Roll for base hitpoints */
	get_extra();

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();

	/* Interact */
	while (TRUE)
	{
		/* Reset cost */
		cost = 0;

		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{
			p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i] =
					modify_stat_value(stats[i], cp_ptr->c_adj[i]);

			/* Total cost */
			cost += birth_stat_costs[stats[i] - BASE_STAT];
		}

		/* Restrict cost */
		if (cost > POINTS)
		{
			/* Warning */
			bell("Excessive stats!");

			/* Reduce stat */
			stats[stat]--;

			/* Recompute costs */
			continue;
		}

		/* Gold is inversely proportional to cost */
		p_ptr->au_birth = p_ptr->au = (25* (POINTS - cost)) + ((adult_start_kit) ? 0 : 100) 
			+ ((adult_easy_mode) ? 500 : 0);

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

		/* Display the costs header */
		put_str("Cost", row - 1, col + 32);

		/* Display the costs */
		for (i = 0; i < A_MAX; i++)
		{
			/* Display cost */
			sprintf(buf, "%4d", birth_stat_costs[stats[i] - BASE_STAT]);
			put_str(buf, row + i, col + 32);
		}

		/* Prompt XXX XXX XXX */
		sprintf(buf, "Total Cost %2d/%d.  Use 2/8 to move, 4/6 to modify, Enter to accept.", cost,POINTS);
		prt(buf, 0, 0);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(col + 36, row + stat);

		/* Get key */
		ch = inkey();

		/* Quit */
		if (ch == KTRL('X')) quit(NULL);

		/* Start over */
		if (ch == ESCAPE) return (FALSE);

		/* Done */
		if ((ch == '\r') || (ch == '\n')) break;

		/* Prev stat */
		if (ch == '8')
		{
			stat = (stat + A_MAX - 1) % A_MAX;;
		}

		/* Next stat */
		if (ch == '2')
		{
			stat = (stat + 1) % A_MAX;
		}

		/* Decrease stat */
		if ((ch == '4') && (stats[stat] > BASE_STAT))
		{
			stats[stat]--;
		}

		/* Increase stat */
		if ((ch == '6') && (stats[stat] < 15))
		{
			stats[stat]++;
		}
	}

	/* Done */
	return (TRUE);
}

/*
 * How often the autoroller will update the display and pause
 * to check for user interuptions.
 * Bigger values will make the autoroller faster, but slower
 * system may have problems because the user can't stop the
 * autoroller for this number of rolls.
 */
#define AUTOROLLER_STEP		25L

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
	int i, m, v;

	bool flag;
	bool prev = FALSE;

	char ch;

	char b1 = '[';
	char b2 = ']';

	char buf[80];

	s16b stat_auto[A_MAX];
	s32b stat_match[A_MAX];

	s32b auto_round = 0L;

	s32b last_round;

	/*** Autoroll ***/

	/* Initialize */
	if (adult_auto_roller)
	{
		byte mval[A_MAX];

		char inp[80];

		/* Extra info */
		Term_putstr(5, 10, -1, TERM_WHITE,
		            "The auto-roller will automatically ignore characters which do");
		Term_putstr(5, 11, -1, TERM_WHITE,
		            "not meet the minimum values for any stats specified below.");
		Term_putstr(5, 12, -1, TERM_WHITE,
		            "Note that stats are not independant, so it is not possible to");
		Term_putstr(5, 13, -1, TERM_WHITE,
		            "get perfect (or even high) values for all your stats.");

		/* Prompt for the minimum stats */
		put_str("Enter minimum value for: ", 15, 2);

		/* Output the maximum stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Reset the "success" counter */
			stat_match[i] = 0;

			/* Calculate race bonus */
			m = modify_stat_value(14, rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);

			/* Save the maximum */
			mval[i] = m;

			/* Extract a textual format */
			sprintf(inp, "(Max of %2d):", m);

			/* Prepare a prompt */
			sprintf(buf, "%-5s%-20s", stat_names[i], inp);

			/* Dump the prompt */
			put_str(buf, 16 + i, 5);
		}

		/* Input the minimum stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Get a minimum stat */
			while (TRUE)
			{
				/* Move the cursor */
				put_str("", 16 + i, 30);

				/* Default */
				strcpy(inp, "");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 9)) inp[0] = '\0';

				/* Hack -- Extract an input */
				v = atoi(inp);

				/* Break on valid input */
				if (v <= mval[i]) break;
			}

			/* Save the minimum stat - the stat as selected minus the race bonus*/
			stat_auto[i] = (v > 0) ? modify_stat_value((byte)v, -(rp_ptr->r_adj[i])) : 0;
		}
	}
	/* Initialize */
	else if (adult_weighted_roller)
	{
		char inp[80];

		/* Clean up */
		clear_from(10);

		/* Extra info */
		Term_putstr(5, 10, -1, TERM_WHITE,
					"The auto-roller will generate 500 characters and try to pick");
		Term_putstr(5, 11, -1, TERM_WHITE,
					"the one with the best stats, according to the weightings you");
		Term_putstr(5, 12, -1, TERM_WHITE,
					"choose below. Enter a value from 1-100 for each stat.");

		/* Prompt for the stat weights */
		put_str("Enter weight for: ", 15, 2);

		/* Output the prompts */
		for (i = 0; i < A_MAX; i++)
		{
			/* Reset the "success" counter */
			stat_match[i] = 0;

			/* Prepare a prompt */
			sprintf(buf, "%-5s", stat_names[i]);

			/* Dump the prompt */
			put_str(buf, 16 + i, 5);
		}

		/* Input the minimum stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* In the Antiband version this is dependent on class & stat */
			int def_weight = 50;

			/* Get a minimum stat */
			while (TRUE)
			{
				/* Move the cursor */
				put_str("", 16 + i, 10);

				/* Default */
				sprintf(inp, "%i", def_weight);

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 9)) inp[0] = '\0';

				/* Extract an input */
				v = atoi(inp);

				/* Break on valid input */
				if (v <= 100) break;
			}

			/* Save the weight */
			stat_auto[i] = (v > 0) ? v : def_weight;
		}
	}

	/* Clean up */
	clear_from(10);

	/*** Generate ***/

	/* Roll */
	while (TRUE)
	{
		int col = 42;

		/* Feedback */
		if (adult_auto_roller)
		{
			Term_clear();

			/* Label */
			put_str(" Limit", 2, col + 5);

			/* Label */
			put_str("  Freq", 2, col + 13);

			/* Label */
			put_str("  Roll", 2, col + 24);

			/* Put the minimal stats */
			for (i = 0; i < A_MAX; i++)
			{
				/* Label stats */
				put_str(stat_names[i], i + 3, col);

				/* Put the stat */
				sprintf(buf, "%2d", stat_auto[i]);
				c_put_str(TERM_L_BLUE, buf, i + 3, col + 9);
			}

			/* Note when we started */
			last_round = auto_round;

			/* Label count */
			put_str("Round:", 10, col + 13);

			/* Indicate the state */
			put_str("(Hit any key to stop)", 12, col + 13);

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
					if (stat_use[i] >= stat_auto[i])
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
				flag = (!(auto_round % AUTOROLLER_STEP));

				/* Update display occasionally */
				if (flag || (auto_round < last_round + 100))
				{
					/* Put the stats (and percents) */
					for (i = 0; i < A_MAX; i++)
					{
						/* Put the stat */
						sprintf(buf, "%2d", stat_use[i]);
						c_put_str(TERM_L_GREEN, buf, i + 3, col + 28);

						/* Put the percent */
						if (stat_match[i])
						{
							int p = 1000L * stat_match[i] / auto_round;
							byte attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
							sprintf(buf, "%3d.%d%%", p / 10, p % 10);
							c_put_str(attr, buf, i + 3, col + 13);
						}

						/* Never happened */
						else
						{
							c_put_str(TERM_RED, "(NONE)", i + 3, col + 13);
						}
					}

					/* Dump round */
					put_str(format("%10ld", auto_round), 10, col + 20);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					if (flag) Term_xtra(TERM_XTRA_DELAY, 100);

					/* Do not wait for a key */
					inkey_scan = TRUE;

					/* Check for a keypress */
					if (inkey()) break;
				}
			}
		}

		/* Weighted auto-roller */
		else if (adult_weighted_roller)
		{
			s32b best_score;
			s32b cur_score;
			byte stat_save[A_MAX];

			Term_clear();

			/* Label */
			put_str("Weight", 2, col + 5);

			/* Label */
			put_str("Roll", 2, col + 13);

			/* Put the stat weights */
			for (i = 0; i < A_MAX; i++)
			{
				/* Label stats */
				put_str(stat_names[i], i + 3, col);

				/* Put the weight */
				sprintf(buf, "%6d", stat_auto[i]);
				c_put_str(TERM_L_BLUE, buf, i + 3, col + 3);
			}

			/* Note when we started */
			last_round = auto_round;

			/* Label count */
			put_str("Round:", 10, col + 13);

			/* Indicate the state */
			put_str("(Hit ESC to stop)", 12, col + 13);

			best_score = -1;
			for (i = 0; i < A_MAX; i++)
			{
				stat_save[i] = 0;
			}

			/* Auto-roll */
			while (TRUE)
			{
				/* Get a new character */
				get_stats();

				/* Advance the round */
				auto_round++;

				/* Hack -- Prevent overflow */
				if (auto_round >= 1000000L) break;

				/* Calculate a score for the rolled stats */
				cur_score = 0;
				for (i = 0; i < A_MAX; i++)
				{
					cur_score += p_ptr->stat_cur[i] * stat_auto[i];
				}

				/* Compare current score against saved stats */
				if (cur_score > best_score)
				{
					best_score = cur_score;
					for (i = 0; i < A_MAX; i++)
					{
						stat_save[i] = p_ptr->stat_cur[i];
					}
				}

				/* Break after 500 rolls */
				if (auto_round >= last_round + 500) break;

				/* Take note every x rolls */
				flag = (!(auto_round % AUTOROLLER_STEP));

				/* Update display occasionally */
				if (flag || (auto_round < last_round + 100))
				{
					/* Put the stats (and percents) */
					for (i = 0; i < A_MAX; i++)
					{
						/* Put the stat */
						sprintf(buf, "%2d", stat_use[i]);
						c_put_str(TERM_L_GREEN, buf, i + 3, col + 14);
					}

					/* Dump round */
					put_str(format("%10ld", auto_round), 10, col + 20);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					if (flag) Term_xtra(TERM_XTRA_DELAY, 100);

					/* Do not wait for a key */
					inkey_scan = TRUE;

					/* Check for a keypress */
					if (inkey()) break;
				}
			}

			/* Load best stat set rolled */
			for (i = 0; i < A_MAX; i++)
			{
				p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_save[i];
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

		/*** Display ***/

		/* Roll for base hitpoints */
		get_extra();

		/* Roll for age/height/weight */
		get_ahw();

		/* Roll for social class */
		get_history();

		/* Roll for gold */
		get_money();

		/* Clear fame */
		p_ptr->fame = 0;

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
			bell("Illegal generation command!");
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
	int i;

	/* Wipe the player */
	player_wipe();

	/* Ask questions */
	if (!player_birth_aux_1()) return (FALSE);

	/* Set adult options from birth options */
	for (i = 0; i < OPT_BIRTH; i++)
	{
		op_ptr->opt_adult[i] = op_ptr->opt_birth[i];
	}

	/* Hack - ensure no more than one birth generation method */
	if (adult_point_based + adult_auto_roller + adult_weighted_roller > 1)
	{
		bell("Conflicting generation options!");
		Term_putstr(5, 10, -1, TERM_L_RED,
		            "Conflicting character generation options!");
		Term_putstr(5, 12, -1, TERM_WHITE,
		            "Change the options and try again!");

		prt("[Press any key to continue]", 23, 5);
		(void)inkey();
		return (FALSE);
	}

	/* Point-based */
	else if (adult_point_based)
	{
		/* Point based */
		if (!player_birth_aux_2()) return (FALSE);
	}

	/* Random */
	else
	{
		/* Auto-roll */
		if (!player_birth_aux_3()) return (FALSE);
	}

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

	/* Reset score options and cheat options */
	for (i = 0; i < OPT_CHEAT; i++)
	{
		op_ptr->opt_cheat[i] = op_ptr->opt_score[i] = FALSE;
	}

	/* Unless otherwise instructed, reset squelch bits */
	if (!adult_retain_squelch)
	{
		for (i = 0; i < OPT_SQUELCH; i++)
		{
			op_ptr->opt_squelch[i] = options_squelch[i].norm;
		}
		for (i = 0; i < z_info->k_max; i++)
		{
			k_info[i].squelch = FALSE;
		}
		for (i = 0; i < MAX_SQ_TYPES; i++)
		{
			op_ptr->squelch_level[i] = 0;
		}
	}

	/* Accept */
	return (TRUE);
}

/*
 * Helper function for 'player_birth_quick()'.
 *
 * See "display_player" for screen layout code.
 */
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

	/* Set adult options from birth options */
	for (i = 0; i < OPT_BIRTH; i++)
	{
		op_ptr->opt_adult[i] = op_ptr->opt_birth[i];
	}

	/* Reset score options and cheat options */
	for (i = 0; i < OPT_CHEAT; i++)
	{
		op_ptr->opt_cheat[i] = op_ptr->opt_score[i] = FALSE;
	}

	/* Unless otherwise instructed, reset squelch bits */
	if (!adult_retain_squelch)
	{
		for (i = 0; i < OPT_SQUELCH; i++)
		{
			op_ptr->opt_squelch[i] = options_squelch[i].norm;
		}
		for (i = 0; i < z_info->k_max; i++)
		{
			k_info[i].squelch = FALSE;
		}
		for (i = 0; i < MAX_SQ_TYPES; i++)
		{
			op_ptr->squelch_level[i] = 0;
		}
	}

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
	message_add("  ", MSG_GENERIC);
	message_add("====================", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add(" ", MSG_GENERIC);

	/* Hack -- outfit the player */
	player_outfit();

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
