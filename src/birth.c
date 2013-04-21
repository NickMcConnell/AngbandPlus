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

	s16b stat[A_MAX];

	char history[4][60];
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
	p_ptr->wt_birth = p_ptr->wt = prev.wt;
	p_ptr->ht_birth = p_ptr->ht = prev.ht;
	p_ptr->sc_birth = p_ptr->sc = prev.sc;
	p_ptr->au_birth = p_ptr->au = prev.au;

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
 * Adjust a stat by an amount.
 *
 * This just uses "modify_stat_value()" unless "maximize" mode is false,
 * and a positive bonus is being applied, in which case, a special hack
 * is used, with the "auto_roll" flag affecting the result.
 *
 * The "auto_roll" flag selects "maximal" changes for use with the
 * auto-roller initialization code.  Otherwise, if "maximize" mode
 * is being used, the changes are fixed.  Otherwise, semi-random
 * changes will occur, with larger changes at lower values.
 */
static int adjust_stat(int value, int amount, int auto_roll)
{
	/* Negative amounts or maximize mode */
	if ((amount < 0) || adult_maximize)
	{
		return (modify_stat_value(value, amount));
	}

	/* Special hack */
	else
	{
		int i;

		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			if (value < 18)
			{
				value++;
			}
			else if (value < 18+70)
			{
				value += ((auto_roll ? 15 : randint(15)) + 5);
			}
			else if (value < 18+90)
			{
				value += ((auto_roll ? 6 : randint(6)) + 2);
			}
			else if (value < 18+100)
			{
				value++;
			}
		}
	}

	/* Return the result */
	return (value);
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
		if ((j > 42) && (j < 54)) break;
	}

	/* Roll the stats */
	for (i = 0; i < A_MAX; i++)
	{
	        int n;

		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

		/* Save that value */
		p_ptr->stat_max[i] = j;

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i];
		for (n = 0; n < p_ptr->available_classes; n++)
		  bonus += cp_ptr[p_ptr->pclass[n]]->c_adj[i];

		/* Variable stat maxes */
		if (adult_maximize)
		{
			/* Start fully healed */
			p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i];

			/* Efficiency -- Apply the racial/class bonuses */
			stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
		}

		/* Fixed stat maxes */
		else
		{
			/* Apply the bonus to the stat (somewhat randomly) */
			stat_use[i] = adjust_stat(p_ptr->stat_max[i], bonus, FALSE);

			/* Save the resulting stat maximum */
			p_ptr->stat_birth[i] = p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_use[i];
		}
	}
}


/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	int classHD = 0, i, j, min_value, max_value;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp;

	/* Class-based HD is an average */
	for (i = 0; i < p_ptr->available_classes; i++)
	  classHD += cp_ptr[p_ptr->pclass[i]]->c_mhp;
	classHD /= (p_ptr->available_classes + 1);

	p_ptr->hitdie += classHD;

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
		p_ptr->ht_birth = p_ptr->ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt_birth = p_ptr->wt = Rand_normal(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht_birth = p_ptr->ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt_birth = p_ptr->wt = Rand_normal(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
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
	gold = (p_ptr->sc * 6) + randint(100) + 300;

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
	     /* Mega-Hack -- reduce gold for high stats */
	     if (stat_use[i] >= 18+50) gold -= 300;
	     else if (stat_use[i] >= 18+20) gold -= 200;
	     else if (stat_use[i] > 18) gold -= 150;
	     else gold -= (stat_use[i] - 8) * 10;
	}

	/* Extra gold */
	if (adult_easy) gold += 500;
	if (adult_easy) gold *= 10;

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

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
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 1;

		/* Clear player kills */
		l_ptr->r_pkills = 0;
	}


	/* Hack -- no ghosts */
	r_info[z_info->r_max-1].max_num = 0;


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

	/* None of the spells have been learned yet */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	  {
	    p_ptr->spell_order[0][i] = 99;
	    p_ptr->spell_order[1][i] = 99;
	  }

	/* No crusader powers active */
	p_ptr->power_active = -1;
	p_ptr->power_passive = -1;

	/* Shapeshifter form */
	p_ptr->shapeshift = 0;

	p_ptr->current_class = 0; /* First class is active */
	p_ptr->available_classes = 1; /* One class only at start */

	p_ptr->realm_magery = 0;
	p_ptr->realm_priest = 0;

	/* No effects have been used */
	for (i = 0; i < MAX_EFFECTS; i++) effects[i] = 0;

	/* Clear class info */
	for (i = 0; i < MAX_CLASS; i++)
	  {
	    p_ptr->pclass[i] = 0;
	    p_ptr->expfact[i] = 100;
	    p_ptr->max_lev[i] = p_ptr->lev[i] = 1;
	    p_ptr->max_exp[i] = p_ptr->exp[i] = p_ptr->exp_frac[i] = 0;
	  }
	
}


/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
static const byte player_init[MAX_CLASS][2] =
{
  { /* Warrior */
    TV_HARD_ARMOR, SV_CHAIN_MAIL
  },
  { /* Mage */
    TV_HELM, SV_WIZARD_HAT
  },
  { /* Priest */
    TV_POTION, SV_POTION_CURE_CRITICAL
  },
  { /* Thief */
    TV_SCROLL, SV_SCROLL_TELEPORT
  },
  { /* Illusionist */
    TV_HELM, SV_WIZARD_HAT
  },
  { /* Archer */
    TV_POTION, SV_POTION_SPEED
  },
  { /* Death Priest */
    TV_POTION, SV_POTION_CURE_CRITICAL
  },
  { /* Berserker */
    TV_POTION, SV_POTION_BESERK_STRENGTH
  },
  { /* Monk */
    TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS
  },
  { /* Runecaster */
    TV_AMULET, SV_AMULET_SPELL /* pval 0 = Magic Missile */
  },
  { /* Crusader */
    TV_POTION, SV_POTION_DETECT_INVIS
  },
  { /* Shifter */
    TV_FOOD, SV_FOOD_RESTORING
  },
  { /* Slayer */
    TV_POTION, SV_POTION_CURE_CRITICAL
  },
  { /* Sorceror */
    TV_HELM, SV_WIZARD_HAT
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

	object_type *i_ptr;
	object_type object_type_body;

	/* Hack -- Give the player some food */
	i_ptr = &object_type_body;
	object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
	i_ptr->number = (byte)rand_range(3, 7);
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);

	if (p_ptr->astral) /* 30 MASS-ID scrolls */
	{
	    i_ptr = &object_type_body;
	    object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_MASS_IDENTIFY));
	    i_ptr->number = 30;
	    object_aware(i_ptr);
	    object_known(i_ptr);
	    (void)inven_carry(i_ptr);
	}

	if (adult_easy) /* Lantern and some flasks */
	{
	    i_ptr = &object_type_body;
	    object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_LANTERN));
	    i_ptr->pval = rand_range(3, 7) * 500;
	    object_aware(i_ptr);
	    object_known(i_ptr);
	    (void)inven_carry(i_ptr);
	    
	    i_ptr = &object_type_body;
	    object_prep(i_ptr, lookup_kind(TV_FLASK, 0));
	    i_ptr->number = (byte)rand_range(2, 5);
	    object_aware(i_ptr);
	    object_known(i_ptr);
	    (void)inven_carry(i_ptr);
	}
	else /* Hack -- Give the player some torches */
	{
	    i_ptr = &object_type_body;
	    object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
	    i_ptr->number = (byte)rand_range(3, 7);
	    i_ptr->pval = rand_range(3, 7) * 500;
	    object_aware(i_ptr);
	    object_known(i_ptr);
	    (void)inven_carry(i_ptr);
	}

	/* Give spellbooks */
	for (i = 0; i < p_ptr->available_classes; i++)
	{
	     tv = mp_ptr[p_ptr->pclass[i]]->spell_book;
	     if (tv != 0)
	     {
		  i_ptr = &object_type_body;
		  object_prep(i_ptr, lookup_kind(tv, 0));
		  object_aware(i_ptr);
		  object_known(i_ptr);
		  (void)inven_carry(i_ptr);		
	     }
	}
	
	/* Grant a weapon to non-monks */
	if (!player_has_class(CLASS_MONK, 0))
	{
	     /* Do not overwrite a 'better' weapon */
	     bool got_weapon = FALSE;
	     
	     /* Warrior/Crusader gets better weapon than Berserker */
	     if (player_has_class(CLASS_WARRIOR, 0))
	     {
		  /* Blunt weapon if character has a priest class */
		  if (player_has_class(CLASS_PRIEST, 0) ||
		      player_has_class(CLASS_DEATH_PRIEST, 0))
		  {
		       tv = TV_HAFTED; sv = SV_MACE;
		  }
		  else
		  {
		       tv = TV_SWORD; sv = SV_LONG_SWORD;
		  }
		  got_weapon = TRUE;
	     }	       

	     if (player_has_class(CLASS_CRUSADER, 0))
	     {
		  tv = TV_HAFTED; sv = SV_LUCERN_HAMMER;
		  got_weapon = TRUE;
	     }	       
	     
	     if (player_has_class(CLASS_SLAYER, 0))
	     {
		  tv = TV_SWORD; sv = SV_SCIMITAR;
		  got_weapon = TRUE;
	     }	       
	     
	     /* Only grant berserker weapon if no other has been granted */
	     if (player_has_class(CLASS_BERSERKER, 0) && (!got_weapon))
	     {
		  tv = TV_HAFTED; sv = SV_SPIKED_CLUB;
		  got_weapon = TRUE;
	     }

	     /* Default weapon, blunt if character has a priest class */
	     if (!got_weapon) 
	     {
		  if (player_has_class(CLASS_PRIEST, 0) ||
		      player_has_class(CLASS_DEATH_PRIEST, 0))
		  {
		       tv = TV_HAFTED; sv = SV_QUARTERSTAFF;
		  }
		  else
		  {
		       tv = TV_SWORD; sv = SV_KNIFE;
		  }	       
		  got_weapon = TRUE;
	     }

	     /* Grant the weapon */
	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(tv, sv));
	     if (adult_easy)
	     {
		  /* Get a list of ego-items to choose from */
		  int num = 0;
		  int random_ego[8];
		  byte ego_type;

		  /* Only small blades and quarterstaffs can gain these flags */
		  if (((i_ptr->tval == 23) && (i_ptr->sval <= 19)) ||
		      ((i_ptr->tval == 21) && (i_ptr->sval == 3)))
		  {
		       if (p_ptr->realm_magery == REALM_MAGIC+1) {
			    random_ego[num] = EGO_R_MAGERY; num++; }
		       if (p_ptr->realm_magery == REALM_ILLUSION+1) {
			    random_ego[num] = EGO_R_ILLUSION; num++; }
		  }

		  /* Only hafted weapons can gain these flags */
		  if (i_ptr->tval == 21)
		  {
		       if (p_ptr->realm_priest == REALM_PRAYER+1) {
			    random_ego[num] = EGO_R_HOLY; num++; }
		       if (p_ptr->realm_priest == REALM_DEATH+1) {
			    random_ego[num] = EGO_R_DEATH; num++; }
		  }
		  
		  /* Slaying weapons */
		  if (player_has_class(CLASS_WARRIOR, 0)) {
		       random_ego[num] = EGO_SLAY_ORC; num++; }
		  if (player_has_class(CLASS_PRIEST, 0)) {
		       random_ego[num] = EGO_SLAY_EVIL; num++; }
		  if (player_has_class(CLASS_CRUSADER, 0)) {
		       random_ego[num] = EGO_SLAY_UNDEAD; num++; 
		       random_ego[num] = EGO_SLAY_DEMON; num++; 
		       random_ego[num] = EGO_SLAY_EVIL; num++; 
		  }
		  if (player_has_class(CLASS_SLAYER, 0)) {
		       random_ego[num] = EGO_SLAY_ANIMAL; num++; }

		  /* Anyone can gain these ego-items */
		  random_ego[num] = EGO_BRAND_ELEC; num++;
		  random_ego[num] = EGO_BRAND_FIRE; num++;
		  random_ego[num] = EGO_BRAND_COLD; num++;

		  /* Get a random ego-item */
		  ego_type = random_ego[rand_int(num)];
		  i_ptr->name2 = ego_type;

		  /* If successfull, add bonuses etc */
		  if (i_ptr->name2)
		  {
		       ego_item_type *e_ptr = &e_info[i_ptr->name2];
		       if (e_ptr->max_to_h > 0) i_ptr->to_h = randint(e_ptr->max_to_h);
		       if (e_ptr->max_to_d > 0) i_ptr->to_d = randint(e_ptr->max_to_d);
		       if (e_ptr->max_to_a > 0) i_ptr->to_a = randint(e_ptr->max_to_a);

		       /* Get pval and bonuses */
		       if (e_ptr->max_pval > 0)
		       { 
			    i_ptr->pval = randint(e_ptr->max_pval);
			    i_ptr->to_h += rand_int(5); i_ptr->to_d += rand_int(5);
		       }
		       else /* Larger bonuses */
		       {
			    i_ptr->to_h += 5 + rand_int(5); i_ptr->to_d += 5 + rand_int(5);
		       }
		       i_ptr->discount = 90;
		  }

	     }
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);
	}

	/* Give archers a bow and ammo, depending on race */
	if (player_has_class(CLASS_ARCHER, 0))
	{ 
	     int tv_ammo, sv_bow;

	     switch (rp_ptr->archer_type == 0 ? randint(3) : rp_ptr->archer_type)
	     {
	     case 2: /* Bows */
		  tv_ammo = TV_ARROW; 
		  if (rand_int(4) == 1)
		      sv_bow = SV_LONG_BOW; 
		  else /* 3 in 4 */
		       sv_bow = SV_SHORT_BOW;
		  break;
	     case 3: /* Crossbows */
		  tv_ammo = TV_BOLT; 
		  if (rand_int(5) == 1)
		       sv_bow = SV_HEAVY_XBOW; 
		  else /* 4 in 5 */
		       sv_bow = SV_LIGHT_XBOW; 
		  break;
	     case 1: /* Slings */
	     default:
		  tv_ammo = TV_SHOT; sv_bow = SV_SLING; break;
	     }
	
	     /* Create the missile launcher */
	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(TV_BOW, sv_bow));
	     if (adult_easy)
	     {
		  apply_magic(i_ptr, 20, TRUE, TRUE, TRUE);
		  i_ptr->discount = 90;
	     }
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);
	     
	     /* Create the ammo */
	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(tv_ammo, SV_AMMO_NORMAL));
	     if (adult_easy)
	     {
		  apply_magic(i_ptr, 20, TRUE, TRUE, TRUE);
		  i_ptr->discount = 90;
	     }
	     i_ptr->number = (byte)rand_range(20, 40);
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);
	}	  

	/* Hack -- Give runecasters some charms */
	if (player_has_class(CLASS_RUNECASTER, 0) && adult_easy)
	{
	     int spell1, spell2;

	     switch (rand_int(6))
	     {
	     case 0: spell1 = SPELL_DETECT_MONSTERS; spell2 = SPELL_PHASE_DOOR; break;
	     case 1: spell1 = SPELL_FIND_TRAPS_DOORS; spell2 = SPELL_LIGHT_AREA; break;
	     case 2: spell1 = SPELL_LIGHT_AREA; spell2 = SPELL_DETECT_MONSTERS; break;
	     case 3: spell1 = SPELL_PHASE_DOOR; spell2 = SPELL_LIGHT_AREA; break;
	     case 4: spell1 = SPELL_PHASE_DOOR; spell2 = SPELL_FIND_TRAPS_DOORS; break;
	     case 5: spell1 = SPELL_FIND_TRAPS_DOORS; spell2 = SPELL_DETECT_MONSTERS; break;
	     }

	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(TV_RING, SV_RING_SPELL));
	     i_ptr->pval = spell1; 
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);	     
		  
	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(TV_RING, SV_RING_SPELL));
	     i_ptr->pval = spell2; 
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);	     
	}

	/* Hack -- Give sorcerors some spell scrolls */
	if (player_has_class(CLASS_SORCEROR, 0))
	{
	     int spell1, spell2;

	     switch (rand_int(6))
	     {
	     case 0: spell1 = SPELL_DETECT_MONSTERS; spell2 = SPELL_PHASE_DOOR; break;
	     case 1: spell1 = SPELL_FIND_TRAPS_DOORS; spell2 = SPELL_LIGHT_AREA; break;
	     case 2: spell1 = SPELL_LIGHT_AREA; spell2 = SPELL_DETECT_MONSTERS; break;
	     case 3: spell1 = SPELL_PHASE_DOOR; spell2 = SPELL_LIGHT_AREA; break;
	     case 4: spell1 = SPELL_PHASE_DOOR; spell2 = SPELL_FIND_TRAPS_DOORS; break;
	     case 5: spell1 = SPELL_FIND_TRAPS_DOORS; spell2 = SPELL_DETECT_MONSTERS; break;
	     }

	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_SPELL));
	     i_ptr->pval = spell1; 
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);	     
		  
	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_SPELL));
	     i_ptr->pval = spell2; 
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);	     

	     /* And one of magic missile */
	     i_ptr = &object_type_body;
	     object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_SPELL));
	     i_ptr->pval = SPELL_MAGIC_MISSILE; 
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);	     
	}

	/* Hack -- Give the player some objects */
	for (i = 0; i < MAX_CLASS; i++)
	{
	    if (player_has_class(i, 0))
	    {
		tv = player_init[i][0];
		sv = player_init[i][1];	     
		i_ptr = &object_type_body;
		object_prep(i_ptr, lookup_kind(tv, sv));
		if (adult_easy && tv <= TV_HARD_ARMOR)
		{
		    apply_magic(i_ptr, 20, TRUE, TRUE, TRUE);
		    i_ptr->discount = 90;
		}
		/* Boots of Kicking for Monks */
		if (adult_easy && tv == TV_BOOTS && player_has_class(CLASS_MONK, 0))
		{
		     i_ptr->name2 = EGO_KICKING;
		     {
			  ego_item_type *e_ptr = &e_info[i_ptr->name2];
			  i_ptr->to_d = randint(e_ptr->max_to_d);
		     }
		}
		object_aware(i_ptr);
		object_known(i_ptr);
		(void)inven_carry(i_ptr);	  
	    }
	}

	/* Starting equipment */
	if (adult_easy)
	{
	    i_ptr = &object_type_body;
	    object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_PHASE_DOOR)); 
	    i_ptr->number = randint(3);
	    object_aware(i_ptr);
	    object_known(i_ptr);
	    (void)inven_carry(i_ptr);	  

	    if (!p_ptr->astral)
	    {
		 i_ptr = &object_type_body;
		 object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_IDENTIFY)); 
		 i_ptr->number = randint(3);
		 object_aware(i_ptr);
		 object_known(i_ptr);
		 (void)inven_carry(i_ptr);

		 i_ptr = &object_type_body;
		 object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL)); 
		 object_aware(i_ptr);
		 object_known(i_ptr);
		 (void)inven_carry(i_ptr);	  
	    }	  

	    i_ptr = &object_type_body;
	    object_prep(i_ptr, lookup_kind(TV_POTION, SV_POTION_CURE_SERIOUS)); 
	    i_ptr->number = randint(3);
	    object_aware(i_ptr);
	    object_known(i_ptr);
	    (void)inven_carry(i_ptr);	  
	}
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * modify options (including the birth options).
 */
static bool player_birth_aux_1(void)
{
	int k, n, i;

	cptr str;

	char ch;

#if 0
	char p1 = '(';
#endif
	char p2 = ')';

	char buf[80];

	int choice;

	/*** Instructions ***/

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
	Term_putstr(5, 7, -1, TERM_WHITE,
	            "Please answer the following questions.  Most of the questions");
	Term_putstr(5, 8, -1, TERM_WHITE,
	            "display a set of standard answers, and many will also accept");
	Term_putstr(5, 9, -1, TERM_WHITE,
	            "some special responses, including 'Q' to quit, 'S' to restart,");
	Term_putstr(5, 10, -1, TERM_WHITE,
	            "and '?' for help.  Note that 'Q' and 'S' must be capitalized.");


	/*** Player sex ***/

	/* Extra info */
	Term_putstr(5, 12, -1, TERM_WHITE,
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
		put_str(buf, 18 + (n/5), 2 + 15 * (n%5));
	}

	/* Choose */
	while (1)
	{
		sprintf(buf, "Choose a sex (%c-%c, or * for random): ",
		        I2A(0), I2A(n-1));
		put_str(buf, 17, 2);
		ch = inkey();
		if (ch == 'Q') quit(NULL);
		if (ch == 'S') return (FALSE);
		k = (islower(ch) ? A2I(ch) : -1);
		if (ch == ESCAPE) ch = '*';
		if (ch == '*') k = rand_int(MAX_SEXES);
		if ((k >= 0) && (k < n)) break;
		if (ch == '?') do_cmd_help();
		else bell("Illegal sex!");
	}

	/* Set sex */
	p_ptr->psex = k;
	sp_ptr = &sex_info[p_ptr->psex];

	/* Sex */
	put_str("Sex", 3, 1);
	c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 8);

	/* Clean up */
	clear_from(12);


	/*** Player race ***/

	/* Extra info */
	Term_putstr(5, 12, -1, TERM_WHITE,
	            "Your 'race' determines various intrinsic factors and bonuses.");

	/* Dump races */
	for (n = 0; n < z_info->p_max; n++)
	{
		/* Analyze */
		p_ptr->prace = n;
		rp_ptr = &p_info[p_ptr->prace];
		str = p_name + rp_ptr->name;

		/* Display */
		sprintf(buf, "%c%c %s", I2A(n), p2, str);
		put_str(buf, 18 + (n/5), 2 + 15 * (n%5));
	}

	/* Choose */
	while (1)
	{
		sprintf(buf, "Choose a race (%c-%c, or * for random): ",
		        I2A(0), I2A(n-1));
		put_str(buf, 17, 2);
		ch = inkey();
		if (ch == 'Q') quit(NULL);
		if (ch == 'S') return (FALSE);
		k = (islower(ch) ? A2I(ch) : -1);
		if (ch == ESCAPE) ch = '*';
		if (ch == '*') k = rand_int(z_info->p_max);
		if ((k >= 0) && (k < n)) break;
		if (ch == '?') do_cmd_help();
		else bell("Illegal race!");
	}

	/* Set race */
	p_ptr->prace = k;
	rp_ptr = &p_info[p_ptr->prace];

	/* Race */
	put_str("Race", 4, 1);
	c_put_str(TERM_L_BLUE, p_name + rp_ptr->name, 4, 8);

	/* Clean up */
	clear_from(12);


	/*** Player class ***/

	/* Extra info */
	Term_putstr(5, 12, -1, TERM_WHITE,
	            "Your 'classes' determine various intrinsic abilities and bonuses.");
	Term_putstr(5, 13, -1, TERM_WHITE,
	            "Any entries with a (*) should only be used by advanced players.");
	Term_putstr(5, 14, -1, TERM_WHITE,
        	    "You may choose more than one class. To stop choosing classes, hit");
	Term_putstr(5, 15, -1, TERM_WHITE,
		    "ESCAPE instead of selecting a class.");

	p_ptr->available_classes = 0;

	/* Get classes, up to racial limit */
	for (choice = 0; choice < rp_ptr->max_multi; choice++)
	{
 	    bool disallowed;
	    int list[MAX_CLASS], listnum = 0;

	    /* Get list of allowed classes */
	    for (n = 0; n < MAX_CLASS; n++)
	    {
	        int i;

		disallowed = FALSE; /* Default is OK */

		/* Check for dis-allowed combinations */
		for (i = 0; i < p_ptr->available_classes; i++)
		{
		    /* Check table of disallowed combinations */
		    if (not_allowed[p_ptr->pclass[i]][n]) disallowed = TRUE; 
		}

		/* Non-recommended combinations cannot be second+ class */
		if (!(rp_ptr->choice & (1L << list[n])) && 
		    p_ptr->available_classes > 0)
		  disallowed = TRUE;

		/* Add to list of allowed choices */
		if (!disallowed)
		{
		    list[listnum] = n;
		    listnum++;	      
		}
	    }

	    /* Hack : No more classes after choosing non-recommended class */
	    if (!(rp_ptr->choice & (1L << p_ptr->pclass[0])))
	      break;

	    /* If there are allowed choices */
	    if (listnum > 0)
	    {
	        /* Display allowed choices */
	        for (n = 0; n < listnum; n++)
		{
		    cptr mod = "";
		  
		    /* Analyze */		    
		    cp_ptr[list[n]] = &class_info[list[n]];
		    str = cp_ptr[list[n]]->title;
		
		    /* Verify legality */
		    if (!(rp_ptr->choice & (1L << list[n]))) mod = " (*)";

		    /* Display */
		    sprintf(buf, "%c%c %s%s", I2A(n), p2, str, mod);
		    put_str(buf, 18 + (n/3), 2 + 20 * (n%3));    
		}
	  
		/* Get a class */
		while (1)
		{
		    sprintf(buf, "Choose a class (%c-%c, or * for random): ",
			    I2A(0), I2A(listnum-1));
		    put_str(buf, 17, 2);
		    ch = inkey();
		    if (ch == 'Q') quit(NULL);
		    if (ch == 'S') return (FALSE);
		    k = (islower(ch) ? A2I(ch) : -1);
		    if (ch == ESCAPE && p_ptr->available_classes > 0) break;
		    if (ch == '*')
		    {
			 if (p_ptr->available_classes > 0)
			 {
			      k = rand_int(listnum + 1);
			      /* Also has a chance of cancelling next choice */
			      if (k == listnum) { ch = ESCAPE; break; }
			 }
			 else k = rand_int(listnum);
		    }
		    if ((k >= 0) && (k < n)) break;
		    if (ch == '?') do_cmd_help();
		    else bell("Illegal class!");
		}

		/* Stop choosing classes (if at least one has been chosen) */
		if (ch == ESCAPE && (p_ptr->available_classes > 0)) {
		  /* Clean up */
		  clear_from(12);
		  break;
		}

		/* Set class */
		p_ptr->pclass[choice] = list[k];
		cp_ptr[p_ptr->pclass[choice]] = &class_info[list[k]];
		mp_ptr[p_ptr->pclass[choice]] = &magic_info[list[k]];

		if (mp_ptr[p_ptr->pclass[choice]]->spell_book != 0)
		{
		  int temp = mp_ptr[p_ptr->pclass[choice]]->spell_book - TV_FIRST_BOOK;
		  switch (temp)
		    {
		    case REALM_MAGIC:
		      p_ptr->realm_magery = REALM_MAGIC + 1;
		      break;
		    case REALM_PRAYER:
		      p_ptr->realm_priest = REALM_PRAYER + 1;
		      break;
		    case REALM_ILLUSION:
		      p_ptr->realm_magery = REALM_ILLUSION + 1;
		      break;
		    case REALM_DEATH:
		      p_ptr->realm_priest = REALM_DEATH + 1;
		      break;
		    }
		}

		/* Clear sorceror spells */
		for (i = 0; i < MAX_SORCEROR_SPELL; i++)
		{
		     sorceror_spell[i] = -1;
		}

		/* Level one */
		p_ptr->max_lev[choice] = 
		  p_ptr->lev[choice] = 1;
		
		/* Start with no experience */
		p_ptr->max_exp[choice] = 
		  p_ptr->exp[choice] = 0;

		/* Experience factor */
		{
		     /* (race penalty + class penalty) * choice */
		     int temp = (rp_ptr->r_exp + cp_ptr[p_ptr->pclass[choice]]->c_exp) * (choice+1);
		     p_ptr->expfact[choice] = temp;
		}

		/* Increment number of classes */
		p_ptr->available_classes++;
		
		/* Clean up */
		clear_from(16);
	    }
	    else		
		/* Clean up */
		clear_from(12);
	}

	/* Extra info */
	Term_putstr(5, 12, -1, TERM_WHITE,
		"Starting as an astral being makes you begin the game on");
	Term_putstr(5, 13, -1, TERM_WHITE,
		"dungeon level 95.  You must make your way from there to the");
	Term_putstr(5, 14, -1, TERM_WHITE,
		"town on foot, where you will finally regain your corporeal");
	Term_putstr(5, 15, -1, TERM_WHITE,
	        "form.  You will then have to make your way back down to win");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"the game.");

	while (1)
	{
		put_str("Start as an astral being? (y/n) ", 18, 2);
		ch = inkey();
		if (ch == 'Q') quit(NULL);
		if (ch == 'S') return (FALSE);
		if (ch == ESCAPE) break;
		if ((ch == 'y') || (ch == 'n')) break;
		if (ch == '?') do_cmd_help();
		else bell("Illegal answer!");
	}

	/* Set "astral" mode */
	if (ch == 'y')
	{
		p_ptr->astral = TRUE;
		p_ptr->astral_start = TRUE;
		p_ptr->astral_birth = TRUE;
	}
	else
	{
	        p_ptr->astral = FALSE;
		p_ptr->astral_start = FALSE;
		p_ptr->astral_birth = FALSE;
	}

	/* Clear */
	clear_from(12);

	/*** Birth options ***/

	/* Extra info */
	Term_putstr(5, 12, -1, TERM_WHITE,
	            "You can change your options at any time, but the 'Birth' options");
	Term_putstr(5, 13, -1, TERM_WHITE,
	            "must be changed now to affect the birth of this character.");

	/* Verify birth options */
	while (1)
	{
		sprintf(buf, "Modify options (y/n)? ");
		put_str(buf, 18, 2);
		ch = inkey();
		if (ch == 'Q') quit(NULL);
		if (ch == 'S') return (FALSE);
		if (ch == ESCAPE) break;
		if (ch == 'y' || ch == 'n') break;
		if (ch == '?') do_cmd_help();
		else bell("Illegal answer!");
	}

	/* Verify */
	if (ch == 'y')
	{
		/* Interact with options */
		do_cmd_options();
	}

	/* Set adult options from birth options */
	for (i = OPT_BIRTH; i < OPT_CHEAT; i++)
	{
		op_ptr->opt[OPT_ADULT + (i - OPT_BIRTH)] = op_ptr->opt[i];
	}

	/* Reset score options from cheat options */
	for (i = OPT_CHEAT; i < OPT_ADULT; i++)
	{
		op_ptr->opt[OPT_SCORE + (i - OPT_CHEAT)] = op_ptr->opt[i];
	}

	/* Clean up */
	clear_from(9);

	/* Done */
	return (TRUE);
}


/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
static const int birth_stat_costs[(18-10)+1] = { 0, 1, 2, 4, 7, 11, 16, 22, 30 };


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "point-based" character creation.
 *
 * The player selects, for each stat, a value from 10 to 18 (inclusive),
 * each costing a certain amount of points (as above), from a pool of 48
 * available points, to which race/class modifiers are then applied.
 *
 * Each unused point is converted into 100 gold pieces.
 */
static bool player_birth_aux_2(void)
{
	int i;

	int row = 2;
	int col = 42;

	int stat = 0;

	int stats[A_MAX];

	int cost;

	char ch;

	char buf[80];


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

	/* Roll for social class */
	get_history();


	/* Interact */
	while (1)
	{
		/* Reset cost */
		cost = 0;

		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{

		    int bonus = rp_ptr->r_adj[i];
		    int n;

		    /* Obtain a "bonus" for "race" and "class" */
		    for (n = 0; n < p_ptr->available_classes; n++)
		      bonus += cp_ptr[p_ptr->pclass[n]]->c_adj[i];
		  
		    /* Apply the racial/class bonuses */
		    p_ptr->stat_cur[i] = modify_stat_value(stats[i], bonus);
		    p_ptr->stat_birth[i] = p_ptr->stat_max[i] = p_ptr->stat_cur[i];

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
		p_ptr->au_birth = p_ptr->au = (100 * (48 - cost)) + 100;

		/* Easy mode gives more gold, but not more points */
		if (adult_easy) { p_ptr->au += 500; p_ptr->au *= 10; }

		/* Calculate the bonuses and hitpoints */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

		/* Update stuff */
		update_stuff();

		/* HACK - Give crusader/slayer some piety, based on wisdom */
		if (player_has_class(CLASS_CRUSADER, 0) || player_has_class(CLASS_SLAYER, 0) )
		{
		     /* +1 = 2-3, +2 = 3-6, +3 = 4-9, d4 = 5-12 */
		     p_ptr->mpp = randint(adj_mag_mana[p_ptr->stat_ind[A_WIS]] * 2) + 
			  adj_mag_mana[p_ptr->stat_ind[A_WIS]] + 1;
		     p_ptr->cpp = p_ptr->mpp;
		}

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;

		/* Fully rested */
		p_ptr->csp = p_ptr->msp; 
		p_ptr->cpp = p_ptr->mpp; 

		/* Display the player */
		display_player(0);

		/* Display the costs header */
		put_str("Cost", row - 1, col + 32);

		/* Display the costs */
		for (i = 0; i < A_MAX; i++)
		{
			/* Display cost */
			sprintf(buf, "%4d", birth_stat_costs[stats[i] - 10]);
			put_str(buf, row + i, col + 32);
		}


		/* Prompt XXX XXX XXX */
		sprintf(buf, "Total Cost %2d/48.  Use 2/8 to move, 4/6 to modify, ESC to accept.", cost);
		prt(buf, 0, 0);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(col + 36, row + stat);

		/* Get key */
		ch = inkey();

		/* Quit */
		if (ch == 'Q') quit(NULL);

		/* Start over */
		if (ch == 'S') return (FALSE);

		/* Done */
		if (ch == ESCAPE) break;

		/* Prev stat */
		if (ch == '8')
		{
			stat = (stat + 5) % 6;
		}

		/* Next stat */
		if (ch == '2')
		{
			stat = (stat + 1) % 6;
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


	/* Done */
	return (TRUE);
}

void cnv_stat_desc(int val, char *out_val)
{
     if (val < 6)
	  sprintf(out_val, "Max of (bad)      :");
     else if (val < 9)
	  sprintf(out_val, "Max of (poor)     :");
     else if (val < 12)
	  sprintf(out_val, "Max of (average)  :");
     else if (val < 15)
	  sprintf(out_val, "Max of (fair)     :");
     else if (val < 18)
	  sprintf(out_val, "Max of (good)     :");
     else if (val < 48)
	  sprintf(out_val, "Max of (very good):");
     else if (val < 78)
	  sprintf(out_val, "Max of (excellent):");
     else if (val < 118)
	  sprintf(out_val, "Max of (superb)   :");
     else
	  sprintf(out_val, "Max of (legendary):");
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
	int i, j, m, v;

	bool flag;
	bool prev = FALSE;

	char ch;

	char b1 = '[';
	char b2 = ']';

	char buf[80];


#ifdef ALLOW_AUTOROLLER

	s16b stat_limit[A_MAX];

	s32b stat_match[A_MAX];

	s32b auto_round = 0L;

	s32b last_round;


	/*** Autoroll ***/

	/* Initialize */
	if (adult_auto_roller)
	{
		int mval[A_MAX];

		char inp[80];


		/* Extra info */
		Term_putstr(5, 7, -1, TERM_WHITE,
		            "The auto-roller will automatically ignore characters which do");
		Term_putstr(5, 8, -1, TERM_WHITE,
		            "not meet the minimum values for any stats specified below.");
		Term_putstr(5, 9, -1, TERM_WHITE,
		            "Note that stats are not independant, so it is not possible to");
		Term_putstr(5, 10, -1, TERM_WHITE,
		            "get perfect (or even high) values for all your stats.");

		if (adult_hidden)
		{
		     Term_putstr(5, 11, -1, TERM_WHITE,
				 "In hidden mode, type one of these letters - b)ad, p)oor, a)verage, ");
		     Term_putstr(5, 12, -1, TERM_WHITE, 
				 "f)air, g)ood, v)ery good, e)xellent, s)uperb, or l)egendary to ");
		     Term_putstr(5, 13, -1, TERM_WHITE, 
				 "choose your minimum stat.");
		}

		/* Prompt for the minimum stats */
		put_str("Enter minimum value for: ", 15, 5);

		/* Output the maximum stats */
		for (i = 0; i < A_MAX; i++)
		{
		        int n;

			/* Reset the "success" counter */
			stat_match[i] = 0;

			/* Race/Class bonus */
			j = rp_ptr->r_adj[i];
			for (n = 0; n < p_ptr->available_classes; n++)
			  j += cp_ptr[p_ptr->pclass[n]]->c_adj[i];

			/* Obtain the "maximal" stat */
			m = adjust_stat(17, j, TRUE);

			/* Save the maximum */
			mval[i] = m;

			/* Extract a textual format */
			if (adult_hidden) cnv_stat_desc(m, inp);

			/* Above 18 */
			if (m > 18)
			{
			     if (!adult_hidden)
				  sprintf(inp, "(Max of 18/%02d):", (m - 18));
			}

			/* From 3 to 18 */
			else
			{
			     if (!adult_hidden)
				  sprintf(inp, "(Max of %2d):", m);
			}

			/* Prepare a prompt */
			sprintf(buf, "%-5s%-20s", stat_names[i], inp);

			/* Dump the prompt */
			put_str(buf, 16 + i, 5);
		}

		/* Input the minimum stats */
		if (adult_hidden) 
		{ for (i = 0; i < A_MAX; i++)
		{
		     /* Get a minimum stat */
		     while (TRUE)
		     {
			  /* Move the cursor */
			  put_str("", 16 + i, 30);

			  /* Default */
			  strcpy(inp, "");

			  /* Get a response (or escape) */
			  if (!askfor_aux(inp, 8)) inp[0] = '\0';

			  /* Extract a value */
			  switch (inp[0])
			  {
			  case 'b': /* Bad */
			       v = 0; break;
			  case 'p': /* Poor */
			       v = 6; break;
			  case 'a': /* Average */
			       v = 9; break;
			  case 'f': /* Fair */
			       v = 12; break;
			  case 'g': /* Good */
			       v = 15; break;
			  case 'v': /* Very Good */
			       v = 18; break;
			  case 'e': /* Excellent */
			       v = 48; break;
			  case 's': /* Superb */
			       v = 78; break;
			  case 'l': /* Legendary */
			       v = 118; break;
			  default:
			       v = 0; break;
			  }

			  /* Break on valid input */
			  if (v <= mval[i]) break;
		     }

		     /* Save the minimum stat */
		     stat_limit[i] = v;
		}
		}
		else 
		{ for (i = 0; i < A_MAX; i++)
		{
			/* Get a minimum stat */
			while (TRUE)
			{
				char *s;

				/* Move the cursor */
				put_str("", 16 + i, 30);

				/* Default */
				strcpy(inp, "");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 8)) inp[0] = '\0';

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

			/* Save the minimum stat */
			stat_limit[i] = (v > 0) ? v : 0;
		}
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
		if (adult_auto_roller)
		{
			Term_clear();

			/* Label */
			if (!adult_hidden) put_str(" Limit", 1, col+5);

			/* Label */
			put_str("  Freq", 1, col+13);

			/* Label */
			put_str("  Roll", 1, col+24);

			/* Put the minimal stats */
			for (i = 0; i < A_MAX; i++)
			{
				/* Label stats */
				put_str(stat_names[i], 2+i, col);

				/* Put the stat */
				if (!adult_hidden)
				{
				     cnv_stat(stat_limit[i], buf);
				     c_put_str(TERM_L_BLUE, buf, 2+i, col+5);
				}
			}

			/* Note when we started */
			last_round = auto_round;

			/* Label count */
			put_str("Round:", 9, col+13);

			/* Indicate the state */
			put_str("(Hit ESC to stop)", 11, col+13);

			/* Auto-roll */
			while (1)
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
						cnv_stat(stat_use[i], buf);
						c_put_str(TERM_L_GREEN, buf, 2+i, col+24);

						/* Put the percent */
						if (stat_match[i])
						{
							int p = 1000L * stat_match[i] / auto_round;
							byte attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
							sprintf(buf, "%3d.%d%%", p/10, p%10);
							c_put_str(attr, buf, 2+i, col+13);
						}

						/* Never happened */
						else
						{
							c_put_str(TERM_RED, "(NONE)", 2+i, col+13);
						}
					}

					/* Dump round */
					put_str(format("%10ld", auto_round), 9, col+20);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					/* if (flag) Term_xtra(TERM_XTRA_DELAY, 100); */

					/* Do not wait for a key */
					inkey_scan = TRUE;

					/* Check for a keypress */
					if (inkey()) break;
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
			p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

			/* Update stuff */
			update_stuff();

			/* HACK - Give crusader/slayer some piety, based on wisdom */
			if (player_has_class(CLASS_CRUSADER, 0) || player_has_class(CLASS_SLAYER, 0))
			{
			     /* +1 = 2-3, +2 = 3-6, +3 = 4-9, d4 = 5-12 */
			     p_ptr->mpp = randint(adj_mag_mana[p_ptr->stat_ind[A_WIS]] * 2) + 
				  adj_mag_mana[p_ptr->stat_ind[A_WIS]] + 1;
			     p_ptr->cpp = p_ptr->mpp;
			}

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;
			p_ptr->cpp = p_ptr->mpp;

			/* Display the player */
			display_player(0);

			/* Prepare a prompt (must squeeze everything in) */
			Term_gotoxy(2, 23);
			Term_addch(TERM_WHITE, b1);
			Term_addstr(-1, TERM_WHITE, "'r' to reroll");
			if (prev) Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
			Term_addstr(-1, TERM_WHITE, ", or ESC to accept");
			Term_addch(TERM_WHITE, b2);

			/* Prompt and get a command */
			ch = inkey();

			/* Quit */
			if (ch == 'Q') quit(NULL);

			/* Start over */
			if (ch == 'S') return (FALSE);

			/* Escape accepts the roll */
			if (ch == ESCAPE) break;

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
		if (ch == ESCAPE) break;

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
static bool player_birth_aux(void)
{
	char ch;

	/* Ask questions */
	if (!player_birth_aux_1()) return (FALSE);

	/* Point-based */
	if (adult_point_based)
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
	display_player(0);

	/* Prompt for it */
	prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

	/* Get a key */
	ch = inkey();

	/* Quit */
	if (ch == 'Q') quit(NULL);

	/* Start over */
	if (ch == 'S') return (FALSE);

	/* Accept */
	return (TRUE);
}

/* Create a character from previous character, code was originally
   from EyAngband */
static bool player_birth_quick()
{
	char ch;
	int i;
	birther old_char;
	byte old_astral;
	byte old_class[MAX_CLASS];
	byte old_current_class;
	byte old_available_classes;
	byte old_race;
	byte old_sex;
	byte old_hitdie;
	u16b old_expfact[MAX_CLASS];
	s16b old_hp[PY_MAX_LEVEL];

	old_astral = p_ptr->astral_birth;
	old_sex = p_ptr->psex;
	for (i = 0; i < MAX_CLASS; i++)
	  old_class[i] = p_ptr->pclass[i];
	old_current_class = p_ptr->current_class;
	old_available_classes = p_ptr->available_classes;
	old_race = p_ptr->prace;

	old_hitdie = p_ptr->hitdie;
	for (i = 0; i < MAX_CLASS; i++)
	  old_expfact[i] = p_ptr->expfact[i];

	old_char.age = p_ptr->age;
	old_char.au = p_ptr->au_birth;
	old_char.ht = p_ptr->ht_birth;
	old_char.wt = p_ptr->wt_birth;
	old_char.sc = p_ptr->sc_birth;
	
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

	p_ptr->astral_birth = p_ptr->astral_start = p_ptr->astral = old_astral;

	p_ptr->current_class = old_current_class;
	p_ptr->available_classes = old_available_classes;

	/* Level one */
	for (i = 0; i < p_ptr->available_classes; i++)
	  p_ptr->max_lev[i] = p_ptr->lev[i] = 1;

	p_ptr->psex = old_sex;

	for (i = 0; i < p_ptr->available_classes; i++)
	{
	     p_ptr->pclass[i] = old_class[i];

	     /* Magic info */
	     cp_ptr[p_ptr->pclass[i]] = &class_info[p_ptr->pclass[i]];
	     mp_ptr[p_ptr->pclass[i]] = &magic_info[p_ptr->pclass[i]];

	     if (mp_ptr[p_ptr->pclass[i]]->spell_book != 0)
	     {
		  int temp = mp_ptr[p_ptr->pclass[i]]->spell_book - TV_FIRST_BOOK;
		  
		  switch (temp)
		  {
		  case REALM_MAGIC:
		       p_ptr->realm_magery = REALM_MAGIC + 1;
		       break;
		  case REALM_PRAYER:
		       p_ptr->realm_priest = REALM_PRAYER + 1;
		       break;
		  case REALM_ILLUSION:
		       p_ptr->realm_magery = REALM_ILLUSION + 1;
		       break;
		  case REALM_DEATH:
		       p_ptr->realm_priest = REALM_DEATH + 1;
		       break;
		  }
	     }
	}

	p_ptr->prace = old_race;

	p_ptr->hitdie = old_hitdie;
	for (i = 0; i < p_ptr->available_classes; i++)
	  p_ptr->expfact[i] = old_expfact[i];

	p_ptr->age = old_char.age;
	p_ptr->au_birth = p_ptr->au = old_char.au;
	p_ptr->ht_birth = p_ptr->ht = old_char.ht;
	p_ptr->wt_birth = p_ptr->wt = old_char.wt;
	p_ptr->sc_birth = p_ptr->sc = old_char.sc;
	
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

	/* Clear sorceror spells */
	for (i = 0; i < MAX_SORCEROR_SPELL; i++)
	{
	     sorceror_spell[i] = -1;
	}

	/* Set birth options from adult options */
	for (i = 0; i < 32; i++)
	{
		op_ptr->opt[OPT_BIRTH + i] = op_ptr->opt[OPT_ADULT + i];
	}

	/* Reset score options */
	for (i = 0; i < 32; i++)
	{
		op_ptr->opt[OPT_SCORE + i] = FALSE;
		op_ptr->opt[OPT_CHEAT + i] = FALSE;
	}

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

	/* Update stuff */
	update_stuff();

	/* HACK - Give crusader/slayer some piety, based on wisdom */
	if (player_has_class(CLASS_CRUSADER, 0) || player_has_class(CLASS_SLAYER, 0) )
	{
	    /* +1 = 2-3, +2 = 3-6, +3 = 4-9, d4 = 5-12 */
	    p_ptr->mpp = randint(adj_mag_mana[p_ptr->stat_ind[A_WIS]] * 2) + 
	      adj_mag_mana[p_ptr->stat_ind[A_WIS]] + 1;
	    p_ptr->cpp = p_ptr->mpp;
	}
	
	/* Fully healed */
	p_ptr->chp = p_ptr->mhp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;
	p_ptr->cpp = p_ptr->mpp;

	/* Get a name, prepare savefile */
	get_name();

	/* Display the player */
	display_player(0);

	/* Prompt for it */
	prt("['Q' to suicide, 'S' to start over or ESC to continue]", 23, 10);

	/* Get a key */
	ch = inkey();

	/* Quit */
	if (ch == 'Q') quit(NULL);

	/* Start over */
	if (ch == 'S') return (FALSE);

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
	bool done = FALSE;

	/* Quickstart if previous character existed on the savefile */
	if (character_existed)
	{
	     char ch;
	     char buf[80];

	     while (TRUE)
	     {
		  Term_clear();

		  sprintf(buf, "Quickstart character based on previous one (y/n) ? ");
		  put_str(buf, 2, 2);
		  ch = inkey();
		  if (ch == 'Q') quit (NULL);
		  if (ch == ESCAPE) break;
		  if (strchr("YyNn", ch)) break;
		  if (ch == '?') { do_cmd_help(); continue; }
		  bell("Illegal answer!");
	     }

	     if (ch == 'Y' || ch == 'y')
	     {
		  if (player_birth_quick()) done = TRUE;
	     }
	}

	/* Create a new character */
	while (!done)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		done = (player_birth_aux());
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
