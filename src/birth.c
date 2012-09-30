/* File: birth.c */

/* Purpose: create a player character */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * How often the autoroller will update the display and pause
 * to check for user interuptions.
 * Bigger values will make the autoroller faster, but slower
 * system may have problems because the user can't stop the
 * autoroller for this number of rolls.
 */
#define AUTOROLLER_STEP		25L

/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100


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

	s16b patron;

	s16b hp[PY_MAX_LEVEL];
};



/*
 * The last character displayed
 */
static birther prev;


/*
 * Current stats
 */
static s16b stat_use[A_MAX];


/*
 * Save the current data for later
 */
static void save_prev_data(void)
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
	prev.age = p_ptr->rp.age;
	prev.wt = p_ptr->rp.wt;
	prev.ht = p_ptr->rp.ht;
	prev.sc = p_ptr->rp.sc;
	prev.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		prev.stat[i] = p_ptr->stat[i].max;
	}

	/* Save the patron */
	prev.patron = p_ptr->chaos_patron;

	/* Save the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		prev.hp[i] = p_ptr->player_hp[i];
	}
}


/*
 * Load the previous data
 */
static void load_prev_data(void)
{
	int i;

	birther temp;


	/*** Save the current data ***/

	/* Save the data */
	temp.age = p_ptr->rp.age;
	temp.wt = p_ptr->rp.wt;
	temp.ht = p_ptr->rp.ht;
	temp.sc = p_ptr->rp.sc;
	temp.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		temp.stat[i] = p_ptr->stat[i].max;
	}

	/* Save the patron */
	temp.patron = p_ptr->chaos_patron;

	/* Save the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		temp.hp[i] = p_ptr->player_hp[i];
	}


	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->rp.age = prev.age;
	p_ptr->rp.wt = prev.wt;
	p_ptr->rp.ht = prev.ht;
	p_ptr->rp.sc = prev.sc;
	p_ptr->au = prev.au;

	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat[i].max = prev.stat[i];
		p_ptr->stat[i].cur = prev.stat[i];
	}

	/* Load the patron */
	p_ptr->chaos_patron = prev.patron;

	/* Load the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		p_ptr->player_hp[i] = prev.hp[i];
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

	/* Save the patron */
	prev.patron = temp.patron;

	/* Save the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		prev.hp[i] = temp.hp[i];
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
			dice[i] = randint1(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}

		/* Verify totals */
		if ((j > 42) && (j < 57)) break;
		/* 57 was 54... I hate 'magic numbers' :< TY */
	}

	/* Acquire the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3 * i] + dice[3 * i + 1] + dice[3 * i + 2];

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Apply the bonus to the stat (somewhat randomly) */
		stat_use[i] = adjust_stat(i, j * 10, bonus);

		/* Start fully healed */
		p_ptr->stat[i].cur = p_ptr->stat[i].max = stat_use[i];
	}
}


/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	int i, j, min_value, max_value;

#ifdef SHOW_LIFE_RATE
	int percent;
#endif /* SHOW_LIFE_RATE */

	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->rp.hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->rp.hitdie;

	/* Minimum hitpoints at highest level */
	min_value = (PY_MAX_LEVEL * (p_ptr->rp.hitdie - 1) * 3) / 8;
	min_value += PY_MAX_LEVEL;

	/* Maximum hitpoints at highest level */
	max_value = (PY_MAX_LEVEL * (p_ptr->rp.hitdie - 1) * 5) / 8;
	max_value += PY_MAX_LEVEL;

	/* Pre-calculate level 1 hitdice */
	p_ptr->player_hp[0] = p_ptr->rp.hitdie;

	/* Roll out the hitpoints */
	while (TRUE)
	{
		/* Roll the hitpoint values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			/* Add in racial hit dice */
			j = randint1(rp_ptr->r_mhp);
			p_ptr->player_hp[i] = p_ptr->player_hp[i - 1] + j;

			/* If class hit dice is non zero - add it on */
			if (cp_ptr->c_mhp)
			{
				p_ptr->player_hp[i] += randint1(cp_ptr->c_mhp);
			}
		}

		/* XXX Could also require acceptable "mid-level" hitpoints */

		/* Require "valid" hitpoints at highest level */
		if (p_ptr->player_hp[PY_MAX_LEVEL - 1] < min_value) continue;
		if (p_ptr->player_hp[PY_MAX_LEVEL - 1] > max_value) continue;

		/* Acceptable */
		break;
	}

#ifdef SHOW_LIFE_RATE

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
					(2 * p_ptr->rp.hitdie +
					 ((PY_MAX_LEVEL - 1) * (p_ptr->rp.hitdie + 1))));

	msgf("Current Life Rating is %d/100.", percent);
	message_flush();

#endif /* SHOW_LIFE_RATE */

}


/*
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	int h_percent;

	/* Calculate the age */
	p_ptr->rp.age = rp_ptr->b_age + randint1(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->rp.psex == SEX_MALE)
	{
		p_ptr->rp.ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		h_percent = (int)(p_ptr->rp.ht) * 100 / (int)(rp_ptr->m_b_ht);
		p_ptr->rp.wt = Rand_normal((int)(rp_ptr->m_b_wt) * h_percent / 100,
								(int)(rp_ptr->m_m_wt) * h_percent / 300);
	}
	/* Calculate the height/weight for females */
	else if (p_ptr->rp.psex == SEX_FEMALE)
	{
		p_ptr->rp.ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);

		h_percent = (int)(p_ptr->rp.ht) * 100 / (int)(rp_ptr->f_b_ht);
		p_ptr->rp.wt = Rand_normal((int)(rp_ptr->f_b_wt) * h_percent / 100,
								(int)(rp_ptr->f_m_wt) * h_percent / 300);
	}
}


/*
 * Get the player's starting money
 */
static void get_money(void)
{
	int i, gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->rp.sc * 6) + rand_range(300, 400);

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (stat_use[i] >= 180 + 50) gold -= 300;
		else if (stat_use[i] >= 180 + 20) gold -= 200;
		else if (stat_use[i] > 180) gold -= 150;
		else
			gold -= (stat_use[i] - 80);
	}

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

	bool options[OPT_PLAYER];
	bool birth[OPT_BIRTH];
	pcave_type *pcave[MAX_HGT];
	pblk_ptr **pwild;

	/* Hack -- save these allocated arrays */
	C_COPY(options, p_ptr->options, OPT_PLAYER, bool);
	C_COPY(birth, p_ptr->birth, OPT_BIRTH, bool);

	/* Hack -- save the cave and wilderness arrays */
	C_COPY(pcave, p_ptr->pcave, MAX_HGT, pcave_type *);
	pwild = p_ptr->pwild;

	/*
	 * Delete the carried objects
	 */
	if (p_ptr->inventory) delete_object_list(&p_ptr->inventory);

	/* Hack -- zero the struct */
	(void)WIPE(p_ptr, player_type);

	/* Hack -- Restore the cave and wilderness arrays */
	C_COPY(p_ptr->pcave, pcave, MAX_HGT, pcave_type *);
	p_ptr->pwild = pwild;

	/* Hack -- Restore the option arrays */
	C_COPY(p_ptr->options, options, OPT_PLAYER, bool);
	C_COPY(p_ptr->birth, birth, OPT_BIRTH, bool);

	/* Start with no artifacts made yet */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}

	/* Reset the objects */
	k_info_reset();

	/* Reset the "monsters" */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (FLAG(r_ptr, RF_UNIQUE)) r_ptr->max_num = 1;
		if (FLAG(r_ptr, RF_UNIQUE_7)) r_ptr->max_num = 7;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* None of the spells have been learned yet */
	for (i = 0; i < PY_MAX_SPELLS; i++) p_ptr->spell.order[i] = 99;

	/* Clear "cheat" options */
	cheat_peek = FALSE;
	cheat_hear = FALSE;
	cheat_room = FALSE;
	cheat_xtra = FALSE;
	cheat_know = FALSE;
	cheat_live = FALSE;

	/* Default pet command settings */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
}


/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
static const byte player_init[MAX_CLASS][3][2] =
{
	{
	 /* Warrior */
	 {TV_RING, SV_RING_RES_FEAR},	/* Warriors need it! */
	 {TV_SWORD, SV_BROAD_SWORD},
	 {TV_HARD_ARMOR, SV_CHAIN_MAIL}
	 },

	{
	 /* Mage */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for r[0].realm book */
	 {TV_SWORD, SV_DAGGER},
	 {TV_DEATH_BOOK, 0}			/* Hack: for r[1].realm book */
	 },

	{
	 /* Priest */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for Life / Death book */
	 {TV_HAFTED, SV_MACE},
	 {TV_DEATH_BOOK, 0}			/* Hack: for r[1].realm book */
	 },

	{
	 /* Rogue */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for r[0].realm book */
	 {TV_SWORD, SV_DAGGER},
	 {TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR}
	 },

	{
	 /* Ranger */
	 {TV_NATURE_BOOK, 0},
	 {TV_SWORD, SV_DAGGER},
	 {TV_DEATH_BOOK, 0}			/* Hack: for r[1].realm book */
	 },

	{
	 /* Paladin */
	 {TV_SORCERY_BOOK, 0},
	 {TV_SWORD, SV_BROAD_SWORD},
	 {TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL}
	 },

	{
	 /* Warrior-Mage */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for r[0].realm book */
	 {TV_SWORD, SV_SHORT_SWORD},
	 {TV_DEATH_BOOK, 0}			/* Hack: for r[1].realm book */
	 },

	{
	 /* Chaos Warrior */
	 {TV_SORCERY_BOOK, 0},		/* Hack: For r[0].realm book */
	 {TV_SWORD, SV_BROAD_SWORD},
	 {TV_HARD_ARMOR, SV_METAL_SCALE_MAIL}
	 },

	{
	 /* Monk */
	 {TV_SORCERY_BOOK, 0},
	 {TV_POTION, SV_POTION_HEALING},
	 {TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
	 },

	{
	 /* Mindcrafter */
	 {TV_SWORD, SV_DAGGER},
	 {TV_POTION, SV_POTION_RES_WIS},
	 {TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
	 },

	{
	 /* High Mage */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for r[0].realm book */
	 {TV_SWORD, SV_DAGGER},
	 {TV_RING, SV_RING_SUSTAIN_INT}
	 },
};


/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i, tv, sv;

	object_type *q_ptr;

	/* Give the player some food */
	switch (p_ptr->rp.prace)
	{
		case RACE_GOLEM:
		case RACE_SKELETON:
		case RACE_ZOMBIE:
		case RACE_VAMPIRE:
		case RACE_SPECTRE:
		case RACE_GHOUL:
		{
			/* Scrolls of satisfy hunger */
			q_ptr =
				object_prep(lookup_kind(TV_SCROLL, SV_SCROLL_SATISFY_HUNGER));
			q_ptr->number = (byte)rand_range(2, 5);
			object_aware(q_ptr);
			object_known(q_ptr);

			/* These objects give no score */
			q_ptr->info |= OB_NO_EXP;

			(void)inven_carry(q_ptr);

			break;
		}
		default:
		{
			/* Food rations */
			q_ptr = object_prep(lookup_kind(TV_FOOD, SV_FOOD_RATION));
			q_ptr->number = (byte)rand_range(3, 7);
			object_aware(q_ptr);
			object_known(q_ptr);

			(void)inven_carry(q_ptr);
		}
	}

	if (p_ptr->rp.prace == RACE_VAMPIRE)
	{
		/* Hack -- Give the player scrolls of DARKNESS! */
		q_ptr = object_prep(lookup_kind(TV_SCROLL, SV_SCROLL_DARKNESS));

		q_ptr->number = (byte)rand_range(2, 5);

		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects give no score */
		q_ptr->info |= OB_NO_EXP;

		(void)inven_carry(q_ptr);
	}
	else
	{
		/* Hack -- Give the player some torches */
		q_ptr = object_prep(lookup_kind(TV_LITE, SV_LITE_TORCH));
		q_ptr->number = (byte)rand_range(3, 7);
		q_ptr->timeout = rand_range(3, 7) * 500;
		q_ptr->pval = 0;
		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}

	if (p_ptr->rp.pclass == CLASS_RANGER)
	{
		/* Hack -- Give the player some arrows */
		q_ptr = object_prep(lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
		q_ptr->number = (byte)rand_range(15, 20);

		/* These objects give no score */
		q_ptr->info |= OB_NO_EXP;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);

		/* Hack -- Give the player a bow */
		q_ptr = object_prep(lookup_kind(TV_BOW, SV_SHORT_BOW));

		/* These objects give no score */
		q_ptr->info |= OB_NO_EXP;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE)
	{
		/* Hack -- Give the player a wand of magic missile */
		q_ptr = object_prep(lookup_kind(TV_WAND, SV_WAND_MAGIC_MISSILE));
		q_ptr->number = 1;
		q_ptr->pval = (byte)rand_range(25, 30);

		/* These objects give no score */
		q_ptr->info |= OB_NO_EXP;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->rp.pclass][i][0];
		sv = player_init[p_ptr->rp.pclass][i][1];

		/* Hack to initialize spellbooks */
		if (tv == TV_SORCERY_BOOK) tv = TV_LIFE_BOOK + p_ptr->spell.r[0].realm - 1;
		else if (tv == TV_DEATH_BOOK) tv = TV_LIFE_BOOK + p_ptr->spell.r[1].realm - 1;

		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
				 p_ptr->rp.prace == RACE_BARBARIAN)
		{
			/* Barbarians do not need a ring of resist fear */
			sv = SV_RING_SUSTAIN_STR;
		}

		/* Hack -- Give the player an object */
		q_ptr = object_prep(lookup_kind(tv, sv));

		/* Assassins begin the game with a poisoned dagger */
		if (tv == TV_SWORD && p_ptr->rp.pclass == CLASS_ROGUE &&
			p_ptr->spell.r[0].realm == REALM_DEATH)
		{
			add_ego_flags(q_ptr, EGO_BRAND_POIS);
		}

		/* These objects give no score */
		q_ptr->info |= OB_NO_EXP;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}
}

/* Locations of the tables on the screen */
#define QUESTION_COL	3
#define SEX_COL			0
#define RACE_COL		12
#define RACE_AUX_COL    27
#define CLASS_COL		27
#define CLASS_AUX_COL   48
#define REALM1_COL		48
#define REALM2_COL		60


/*
 * Player sex
 */
static bool get_player_sex(void)
{
	int i;
	cptr genders[MAX_SEXES];

	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Your 'sex' does not have any significant gameplay effects.");

	/* Tabulate genders */
	for (i = 0; i < MAX_SEXES; i++)
	{
		genders[i] = sex_info[i].title;
	}

	p_ptr->rp.psex = get_player_choice(genders, MAX_SEXES, SEX_COL, 15,
									"charattr.txt#TheSexes", NULL);

	/* No selection? */
	if (p_ptr->rp.psex == INVALID_CHOICE)
	{
		p_ptr->rp.psex = 0;
		return (FALSE);
	}

	/* Save the sex pointer */
	sp_ptr = &sex_info[p_ptr->rp.psex];

	return (TRUE);
}


/*
 * Display additional information about each race during the selection.
 */
static void race_aux_hook(cptr r_str)
{
	int race;

	/* Extract the proper race index from the string. */
	for (race = 0; race < MAX_RACES; race++)
	{
		if (streq(r_str, race_info[race].title)) break;
	}

	if (race == MAX_RACES) return;

	/* Display relevant details. */
	put_fstr(RACE_AUX_COL, TABLE_ROW,
    			"%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
				"Hit die: %d \n"
				"Experience: %2d%% \n"
				"Infravision: %d ft",
                stat_names_reduced[0], race_info[race].r_adj[0],
                stat_names_reduced[1], race_info[race].r_adj[1],
                stat_names_reduced[2], race_info[race].r_adj[2],
                stat_names_reduced[3], race_info[race].r_adj[3],
                stat_names_reduced[4], race_info[race].r_adj[4],
                stat_names_reduced[5], race_info[race].r_adj[5],
				race_info[race].r_mhp,
				race_info[race].r_exp,
				race_info[race].infra * 10);
}


/*
 * Player race
 */
static bool get_player_race(void)
{
	int i;
	cptr races[MAX_RACES];

	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Your 'race' determines various intrinsic factors and bonuses.");

	/* Tabulate races */
	for (i = 0; i < MAX_RACES; i++)
	{
		races[i] = race_info[i].title;
	}

	p_ptr->rp.prace = get_player_sort_choice(races, MAX_RACES, RACE_COL, 15,
										  "charattr.txt#TheRaces",
										  race_aux_hook);

	/* No selection? */
	if (p_ptr->rp.prace == INVALID_CHOICE)
	{
		p_ptr->rp.prace = 0;
		return (FALSE);
	}

	/* Give beastman a mutation at character birth */
	if (p_ptr->rp.prace == RACE_BEASTMAN)
	{
		p_ptr->change |= (PC_MUTATE);
	}

	/* Save the race pointer */
	rp_ptr = &race_info[p_ptr->rp.prace];

	/* Success */
	return (TRUE);
}


/*
 * Display additional information about each class during the selection.
 */
static void class_aux_hook(cptr c_str)
{
	int class_idx;
	char s[128];

	/* Extract the proper class index from the string. */
	for (class_idx = 0; class_idx < MAX_CLASS; class_idx++)
	{
		if (streq(c_str, class_info[class_idx].title)) break;

		/* Also test for titles in parentheses */
		strnfmt(s, 128, "(%s)", class_info[class_idx].title);
		if (streq(c_str, s)) break;
	}

	if (class_idx == MAX_CLASS) return;

	/* Display relevant details. */
	put_fstr(CLASS_AUX_COL, TABLE_ROW,
    			"%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
				"Hit die: %d \n"
				"Experience: %2d%%",
                stat_names_reduced[0], class_info[class_idx].c_adj[0],
                stat_names_reduced[1], class_info[class_idx].c_adj[1],
                stat_names_reduced[2], class_info[class_idx].c_adj[2],
                stat_names_reduced[3], class_info[class_idx].c_adj[3],
                stat_names_reduced[4], class_info[class_idx].c_adj[4],
                stat_names_reduced[5], class_info[class_idx].c_adj[5],
			 	class_info[class_idx].c_mhp,
				class_info[class_idx].c_exp);
}


/*
 * Player class
 */
static bool get_player_class(void)
{
	int i;
	cptr classes[MAX_CLASS];


	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Your 'class' determines various intrinsic abilities and bonuses.\n"
				"Any entries in parentheses should only be used by advanced players.");

	/* Tabulate classes */
	for (i = 0; i < MAX_CLASS; i++)
	{
		classes[i] = class_info[i].title;
	}

	p_ptr->rp.pclass = get_player_choice(classes, MAX_CLASS, CLASS_COL, 20,
									  "charattr.txt#TheClasses",
									  class_aux_hook);

	/* No selection? */
	if (p_ptr->rp.pclass == INVALID_CHOICE)
	{
		p_ptr->rp.pclass = 0;

		return (FALSE);
	}

	/* Set class */
	cp_ptr = &class_info[p_ptr->rp.pclass];
	mp_ptr = &magic_info[p_ptr->rp.pclass];

	return (TRUE);
}


/*
 * Choose the magical realms
 */
static bool get_player_realms(void)
{
	int i;
	int count = 0;
	cptr realms[MAX_REALM];
	int select[MAX_REALM];
	int choose;

	/* No realms at all? */
	select[0] = REALM_NONE;

	/* Get choices */
	for (i = 1; i < MAX_REALM; i++)
	{
		/* Can we use this realm? */
		if (realm_choices1[p_ptr->rp.pclass] & (1 << (i - 1)))
		{
			/* Save the information */
			select[count] = i;
			realms[count] = realm_names[i];

			/* Count them */
			count++;
		}
	}

	/* No magic? */
	if (!count) return (TRUE);

	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Life and Sorcery are protective, Chaos and Death are destructive.\n"
				"Nature has both defensive and offensive spells.");

	choose = get_player_choice(realms, count, REALM1_COL, 10,
							   "magic.txt#MagicRealms", NULL);

	/* No selection? */
	if (choose == INVALID_CHOICE) return (FALSE);

	/* Save the choice */
	p_ptr->spell.r[0].realm = select[choose];

	/* Paranoia - No realms at all? */
	select[0] = REALM_NONE;

	/* Reset counter */
	count = 0;

	/* Get choices */
	for (i = 1; i < MAX_REALM; i++)
	{
		/* Can we use this realm? */
		if ((realm_choices2[p_ptr->rp.pclass] & (1 << (i - 1)))
			&& (i != p_ptr->spell.r[0].realm))
		{
			/* Save the information */
			select[count] = i;
			realms[count] = realm_names[i];

			/* Increment counter */
			count++;
		}
	}

	/* No second realm? */
	if (!count) return (TRUE);

	choose = get_player_choice(realms, count, REALM2_COL, 10,
							   "magic.txt#MagicRealms", NULL);

	/* No selection? */
	if (choose == INVALID_CHOICE) return (FALSE);

	/* Save the choice */
	p_ptr->spell.r[1].realm = select[choose];

	/* Done */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * modify options (including the birth options).
 *
 * Taken from V 2.9.0
 */
static bool player_birth_aux_1(void)
{
	/*** Instructions ***/

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
	put_fstr(QUESTION_COL, HEADER_ROW,
			"Please select your character from the menu below.\n"
			"Use the movement keys to scroll the menu, 'enter' to select the current\n"
			"menu item, '*' for a random menu item, 'ESC' to restart the character\n"
			"selection, '=' for the birth options, '?' for help, or 'Ctrl-X' to quit.");

	if (!get_player_sex()) return (FALSE);

	/* Clean up */
    clear_region(0, QUESTION_ROW, TABLE_ROW - 1);

	/* Choose the players race */
	if (!get_player_race()) return (FALSE);

	/* Clean up */
	clear_region(0, QUESTION_ROW, TABLE_ROW - 1);

	/* Choose the players class */
	if (!get_player_class()) return (FALSE);

	/* Clean up */
	clear_region(0, QUESTION_ROW, TABLE_ROW - 1);

	/* Choose the magic realms */
	if (!get_player_realms()) return (FALSE);

	/* Clear */
	Term_clear();

	/* Display the information so far. */
	/* Name, Sex, Race, Class */
	put_fstr(0, 2,
				"Name     : " CLR_L_BLUE "%s\n" CLR_WHITE
				"Sex      : " CLR_L_BLUE "%s\n" CLR_WHITE
				"Race     : " CLR_L_BLUE "%s\n" CLR_WHITE
				"Class    : " CLR_L_BLUE "%s\n",
				player_name, sp_ptr->title, rp_ptr->title, cp_ptr->title);

	if (p_ptr->spell.r[0].realm || p_ptr->spell.r[1].realm)
	{
		put_fstr(0, 6, "Magic    : " CLR_L_BLUE "%s", realm_names[p_ptr->spell.r[0].realm]);
	}

	if (p_ptr->spell.r[1].realm)
	{
		put_fstr(11, 7, CLR_L_BLUE "%s", realm_names[p_ptr->spell.r[1].realm]);
	}

	/* And finally, initialize the starting quests */
	init_player_quests();

	/* Clear */
	clear_from(15);

	/* Done */
	return (TRUE);
}


/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
static const int birth_stat_costs[(18 - 10) + 1] =
{ 0, 1, 2, 4, 7, 11, 16, 22, 30 };


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "point-based" character creation.
 *
 * The player selects, for each stat, a value from 10 to 18 (inclusive),
 * each costing a certain amount of points (as above), from a pool of 48
 * available points, to which race/class modifiers are then applied.
 *
 * Each unused point is converted into 100 gold pieces, with a maximum of
 * 600 gp at birth.
 *
 * Taken from V 2.9.0
 */
static bool player_birth_aux_2(void)
{
	int i;

	int row = 3;
	int col = 42;

	int stat = 0;

	int stats[A_MAX];

	int cost;

	char ch;

	int mode = DISPLAY_PLAYER_STANDARD;


	/* Initialize stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Initial stats */
		stats[i] = 10;
	}


	/* Roll for base hitpoints */
	get_extra();

	/* Roll for age/height/weight */
	get_ahw();
	
	/* Dodgy "social class" */
	p_ptr->rp.sc = randint1(100);

	/* Hack -- get a chaos patron even if you are not a chaos warrior */
	p_ptr->chaos_patron = (s16b)randint0(MAX_PATRON);

	p_ptr->muta1 = 0;
	p_ptr->muta2 = 0;
	p_ptr->muta3 = 0;

	/* Interact */
	while (1)
	{
		/* Reset cost */
		cost = 0;

		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{
			int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

			/* Reset stats */
			p_ptr->stat[i].cur = adjust_stat(i, stats[i] * 10, bonus);
			p_ptr->stat[i].max = p_ptr->stat[i].cur;

			/* Total cost */
			cost += birth_stat_costs[stats[i] - 10];
		}

		/* Restrict cost */
		if (cost > 48)
		{
			/* Warning */
			bell("Excessive stats!");

			/* Reduce stat */
			stats[stat]--;

			/* Recompute costs */
			continue;
		}

		/* Gold is inversely proportional to cost */
		p_ptr->au = (100 * (48 - cost)) + 100;

		/* Maximum of 600 gold */
		if (p_ptr->au > 600) p_ptr->au = 600;

		/* Calculate the bonuses and hitpoints */
		p_ptr->update |= (PU_BONUS | PU_HP);

		/* Update stuff */
		update_stuff();

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;

		/* Fully rested */
		p_ptr->csp = p_ptr->msp;

		/* Display the player */
		display_player(mode);

		/* Display the costs header */
		put_fstr(col + 32, row - 2, "Cost");

		/* Display the costs */
		for (i = 0; i < A_MAX; i++)
		{
			/* Display cost */
			put_fstr(col + 32, row + (i - 1), "%4d", birth_stat_costs[stats[i] - 10]);
		}


		/* Prompt XXX XXX XXX */
		prtf(0, 0, "Total Cost %2d/48.  Use 2/8 to move, 4/6 to modify, Enter to accept.",
				cost);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(col + 36, row + stat - 1);

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
			stat = (stat + A_MAX - 1) % A_MAX;
		}

		/* Next stat */
		if (ch == '2')
		{
			stat = (stat + 1) % A_MAX;
		}

		/* Decrease stat */
		if ((ch == '4') && (stats[stat] > 10))
		{
			stats[stat]--;
		}

		/* Increase stat */
		if ((ch == '6') && (stats[stat] < 18))
		{
			stats[stat]++;
		}
	}

	/* Process stats */
	for (i = 0; i < A_MAX; i++)
	{
		int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Apply some randomness */
        p_ptr->stat[i].cur = adjust_stat(i, stats[i] * 10, bonus);
		p_ptr->stat[i].max = p_ptr->stat[i].cur;
	}

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Update stuff */
	update_stuff();

	/* Display the player */
	display_player(mode);

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
	int i, v;

	bool flag;
	bool previous = FALSE;

	char ch;

	int mode = DISPLAY_PLAYER_STANDARD;

#ifdef ALLOW_AUTOROLLER

	s16b stat_weight[A_MAX];
	s16b stat_save[A_MAX];

	s32b stat_match[A_MAX];

	s32b auto_round = 0L;

	s32b last_round;


	/*** Autoroll ***/

	/* Initialize */
	if (autoroller)
	{
		char inp[80];

		/* Clean up */
		clear_from(10);

		/* Extra info */
		put_fstr(5, 10,
					"The auto-roller will generate 500 characters and try to pick\n"
					"the one with the best stats, according to the weightings you\n"
					"choose below. Enter a value from 1-100 for each stat.");

		/* Prompt for the stat weights */
		put_fstr(2, 15, "Enter weight for: ");

		/* Output the prompts */
		for (i = 0; i < A_MAX; i++)
		{
			/* Reset the "success" counter */
			stat_match[i] = 0;

			/* Dump the prompt */
			put_fstr(5, 16 + i, "%-5s", stat_names[i]);
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
				Term_gotoxy(10, 16 + i);

				/* Default */
				strnfmt(inp, 80, "%i", def_weight);

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 9)) inp[0] = '\0';

				/* Extract an input */
				v = atoi(inp);

				/* Break on valid input */
				if (v <= 100) break;
			}

			/* Save the weight */
			stat_weight[i] = (v > 0) ? v : def_weight;
		}
	}

#endif /* ALLOW_AUTOROLLER */

	/* Clean up */
	clear_from(10);


	/*** Generate ***/

	/* Roll */
	while (TRUE)
	{
		int col = 42;

		/* Feedback */
		if (autoroller)
		{
			s32b best_score;
			s32b cur_score;

			Term_clear();

			/* Label */
			put_fstr(col + 5, 2, "Weight");

			/* Label */
			put_fstr(col + 13, 2, "  Roll");

			/* Put the stat weights */
			for (i = 0; i < A_MAX; i++)
			{
				/* Label stats */
				put_fstr(col, i + 3, stat_names[i]);

				/* Put the weight */
				put_fstr(col + 5, i + 3, CLR_L_BLUE "%6i", stat_weight[i]);
			}

			/* Note when we started */
			last_round = auto_round;

			/* Label count */
			put_fstr(col + 13, 10, "Round:");

			/* Indicate the state */
			put_fstr(col + 13, 12, "(Hit ESC to stop)");

			best_score = -1;
			for (i = 0; i < A_MAX; i++)
			{
				stat_save[i] = 3;
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
		   			cur_score += (p_ptr->stat[i].cur) * stat_weight[i];
				}

				/* Compare current score against saved stats */
				if (cur_score > best_score)
				{
					best_score = cur_score;
					for (i = 0; i < A_MAX; i++)
					{
						stat_save[i] = p_ptr->stat[i].cur;
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
						put_fstr(col + 13, 3 + i, CLR_L_GREEN "%v",
								 stat_format, stat_use[i]);
					}

					/* Dump round */
					put_fstr(col + 20, 10, "%10ld", auto_round);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					if (flag) Term_xtra(TERM_XTRA_DELAY, 100);

					/* Do not wait for a key */
					p_ptr->cmd.inkey_scan = TRUE;

					/* Check for a keypress */
					if (inkey()) break;
				}
			}

			/* Load best stat set rolled */
			for (i = 0; i < A_MAX; i++)
			{
				p_ptr->stat[i].cur = p_ptr->stat[i].max = stat_save[i];
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
		
		/* Dodgy "social class" */
		p_ptr->rp.sc = randint1(100);

		/* Roll for gold */
		get_money();

		/* Hack -- get a chaos patron even if you are not a chaos warrior */
		p_ptr->chaos_patron = (s16b)randint0(MAX_PATRON);

		p_ptr->muta1 = 0;
		p_ptr->muta2 = 0;
		p_ptr->muta3 = 0;

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
			display_player(mode);

			/* Prepare a prompt (must squeeze everything in) */
			prtf(2, 23, "['r' to reroll%s, 'n' for next screen, or Enter to accept]",
				previous ? ", 'p' for prev": "");

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
			if (previous && (ch == 'p'))
			{
				load_prev_data();
				continue;
			}

			/* Increase mode */
			if (ch == 'n')
			{
				mode = (mode + 1) % DISPLAY_PLAYER_MAX;
			}

			/* Help */
			if (ch == '?')
			{
				(void)show_file("birth.txt#CharDisplay", NULL, 0, 0);
				continue;
			}
			else if (ch == '=')
			{
				do_cmd_options(OPT_FLAG_BIRTH | OPT_FLAG_SERVER |
							   OPT_FLAG_PLAYER);
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
		previous = TRUE;
	}

	/* Clear prompt */
	clear_from(23);

	/* Done */
	return (TRUE);
}


static bool player_birth_aux(void)
{
    char ch;
    int i;

	/* Ask questions */
	if (!player_birth_aux_1()) return FALSE;

	/* Point based */
	if (point_based)
	{
		if (!player_birth_aux_2()) return FALSE;
	}

	/* Auto-roll */
	else
	{
		if (!player_birth_aux_3()) return FALSE;
    }

    /* Apply some randomness */
    for (i = 0; i < A_MAX; i++)
    {
        p_ptr->stat[i].cur += (s16b) randint0(10);
        p_ptr->stat[i].max = p_ptr->stat[i].cur;
    }

    /* Calculate the bonuses and hitpoints */
    p_ptr->update |= (PU_BONUS | PU_HP);

    /* Update stuff */
    update_stuff();

	/* Get a name, prepare savefile */
	get_character_name();

	/* Initialize the virtues */
	get_virtues();

	/* Display the player */
	display_player(DISPLAY_PLAYER_STANDARD);

	/* Prompt for it */
	prtf(10, 23,
		"['Ctrl-X' to suicide, 'Del' to start over, or Enter to continue]");

	/* Get a key */
	ch = inkey();

	/* Quit */
	if (ch == KTRL('X')) quit(NULL);

	/* Start over */
	if ((ch == 0x7F) || (ch == '.') || (ch == KTRL('H'))) return (FALSE);

	/* Accepted */

	/* Done */
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

	/* Create a note file if that option is set */
	if (take_notes)
	{
		add_note_type(NOTE_BIRTH);
	}

	/* Note player birth in the message recall */
	message_add(" ", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add("====================", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add(" ", MSG_GENERIC);

	/* Hack -- outfit the player */
	player_outfit();

	/* Set the message window flag as default */
	if (!window_flag[1])
		window_flag[1] |= PW_MESSAGE;

	/* Set the inv/equip window flag as default */
	if (!window_flag[2])
		window_flag[2] |= PW_INVEN;
}

