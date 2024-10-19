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
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = 200;

	/* Initial hitpoints */
	p_ptr->mhp = cp_ptr->c_mhp;
}


/*
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	/* Calculate the age */
         p_ptr->age = rand_range(15, 30);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = Rand_normal(72, 6);
		p_ptr->wt = Rand_normal(180, 25);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = Rand_normal(66, 4);
		p_ptr->wt = Rand_normal(150, 20);
	}
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


	/* Quests */
	{
	     /* Levels that contain a quest monster */
	     int quest_level[MAX_Q_IDX] = 
	     { 
		  12, 61, 85, /* Andariel */
		  23, 65, 88, /* The Summoner */
		  24, 66, 89, /* Duriel */
		  36, 71, 93, /* Mephisto */
		  43, 74, 95, /* Hephasto */
		  44, 75, 96, /* Diablo */
		  56, 81, 100 /* Baal */
	     };
	     
	     cptr quest_desc[MAX_Q_IDX / 3] =
	     {
		  "Andariel",
		  "The Summoner",
		  "Duriel",
		  "Mephisto",
		  "Hephasto the Smith",
		  "Diablo",
		  "Baal"
	     };
			 
	     for (i = 0; i < MAX_Q_IDX; i++)
	     {
		  /* Set the quest levels */
		  q_list[i].level = quest_level[i];

		  /* Set the quest descriptions */
		  q_list[i].desc = quest_desc[i / 3];
	     }
	}

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


	/* No skills learnt, no stats increased, no auras, charge-ups or
	 * shapeshifts active 
	 */
	for (i = 0; i < MAX_SKILLS; i++)
	     p_ptr->skill[i] = 0;
	p_ptr->skill_points = 1;
	for (i = 0; i < A_MAX; i++)
	     p_ptr->stat_bonus[i] = 0;
	p_ptr->stat_points = 0;
	p_ptr->aura = 0;
	for (i = 0; i < MAX_CU_SKILLS; i++)
	     p_ptr->charge_up[i] = 0;
	p_ptr->shapeshift = 0;
	p_ptr->tim_shapeshift = 0;
	p_ptr->frenzy = 0;
}




/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */

static const byte player_init[MAX_CLASS][3][2] =
{
	{
		/* Warrior */
		{ TV_AXE, SV_HATCHET },
		{ TV_SWORD, SV_KNIFE },
		{ TV_HARD_ARMOR, SV_HAUBERK },
	},

	{
		/* Mage */
		{ TV_SOFT_ARMOR, SV_ROBE },
		{ TV_HAFTED, SV_MAGE_STAFF },
		{ TV_POTION, SV_POTION_MANA_LIGHT },
	},

	{
		/* Priest */
		{ TV_HAFTED, SV_WOODEN_CLUB },
		{ TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR },
		{ TV_POTION, SV_POTION_MANA_LIGHT },
	},

	{
		/* Rogue */
		{ TV_CLAW, SV_CLAW },
		{ TV_CLAW, SV_CLAW },
		{ TV_CLOAK, SV_FUR_CLOAK },
	},

	{
		/* Ranger */
	        { TV_POLEARM, SV_SPEAR },
		{ TV_BOW, SV_LONG_BOW },
		{ TV_SOFT_ARMOR, SV_SOFT_STUDDED_LEATHER },
	},

	{
		/* Paladin */
		{ TV_HARD_ARMOR, SV_RING_MAIL },
		{ TV_SWORD, SV_LONG_SWORD },
		{ TV_SHIELD, SV_LARGE_LEATHER_SHIELD },
	},

	{
	        /* Necromancer */
	        { TV_SWORD, SV_DAGGER },
		{ TV_SOFT_ARMOR, SV_ROBE },
		{ TV_POTION, SV_POTION_MANA_LIGHT },
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

	/* Hack -- Give the player some torches */
	object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
	i_ptr->number = (byte)rand_range(3, 7);
	i_ptr->pval = rand_range(3, 7) * 500;
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some cure light wounds */
	object_prep(i_ptr, lookup_kind(TV_POTION, SV_POTION_CURE_LIGHT));
	i_ptr->number = 4;
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player an identify scroll */
	object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_IDENTIFY));
	i_ptr->number = 1;
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player a word of recall scroll */
	object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL));
	i_ptr->number = 1;
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);


	/* Give amazons some arrows */
	if (p_ptr->pclass == CLASS_RANGER)
	{
	     /* Get local object */
	     i_ptr = &object_type_body;

	     /* Hack -- Give arrows */
	     object_prep(i_ptr, lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
	     i_ptr->number = rand_range(15, 25);
	     object_aware(i_ptr);
	     object_known(i_ptr);
	     (void)inven_carry(i_ptr);
	}


	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Get local object */
		i_ptr = &object_type_body;

		/* Hack -- Give the player an object */
		object_prep(i_ptr, lookup_kind(tv, sv));
		object_aware(i_ptr);
		object_known(i_ptr);
		(void)inven_carry(i_ptr);
	}
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a class, and
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


	/*** Player class ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
	            "Your 'class' determines various intrinsic abilities and bonuses.");

	/* Dump classes */
	for (n = 0; n < MAX_CLASS; n++)
	{
		cptr mod = "";

		/* Analyze */
		p_ptr->pclass = n;
		cp_ptr = &class_info[p_ptr->pclass];
		str = cp_ptr->title;

		/* Display */
		sprintf(buf, "%c%c %s%s", I2A(n), p2, str, mod);
		put_str(buf, 18 + (n/3), 2 + 20 * (n%3));
	}

	/* Get a class */
	while (1)
	{
		sprintf(buf, "Choose a class (%c-%c, or * for random): ",
		        I2A(0), I2A(n-1));
		put_str(buf, 17, 2);
		ch = inkey();
		if (ch == 'Q') quit(NULL);
		if (ch == 'S') return (FALSE);
		k = (islower(ch) ? A2I(ch) : -1);
		if (ch == ESCAPE) ch = '*';
		if (ch == '*')
		{
			while (1)
			{
				k = rand_int(MAX_CLASS);

				break;
			}
		}
		if ((k >= 0) && (k < n)) break;
		if (ch == '?') do_cmd_help();
		else bell("Illegal class!");
	}

	/* Set class */
	p_ptr->pclass = k;
	cp_ptr = &class_info[p_ptr->pclass];

	/* Set sex based on class */
	switch (p_ptr->pclass)
	{
	case CLASS_WARRIOR:
	case CLASS_PRIEST:
	case CLASS_PALADIN:
	case CLASS_NECRO:
	     p_ptr->psex = 1; /* Male */
	     break;
	case CLASS_MAGE:
	case CLASS_ROGUE:
	case CLASS_RANGER:
	     p_ptr->psex = 0; /* Female */
	     break;
	}
	sp_ptr = &sex_info[p_ptr->psex];

	/* Clean up */
	clear_from(15);


	/*** Birth options ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
	            "You can change your options at any time, but the 'Birth' options");
	Term_putstr(5, 16, -1, TERM_WHITE,
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
	clear_from(10);

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
static bool player_birth_aux_2(void)
{
     int i;

     /* Get a new character */

     /* Get attributes from class */
     for (i = 0; i < A_MAX; i++)
     {
	  p_ptr->stat_cur[i] = p_ptr->stat_max[i] = cp_ptr->c_adj[i];
     }

     /* Roll for base hitpoints */
     get_extra();

     /* Roll for age/height/weight */
     get_ahw();

     /* Set amount of gold */
     p_ptr->au = 0;

     /* Calculate the bonuses and hitpoints */
     p_ptr->update |= (PU_BONUS | PU_HP);

     /* Update stuff */
     update_stuff();
     
     /* Fully healed */
     p_ptr->chp = p_ptr->mhp;
     p_ptr->cure_hp = 0;
     p_ptr->time_hp = 0;

     /* Fully rested */
     p_ptr->csp = p_ptr->msp;
     p_ptr->cure_sp = 0;
     p_ptr->time_hp = 0;

     /* Display the player */
     display_player(0);

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

	/* Auto-roll */
	if (!player_birth_aux_2()) return (FALSE);

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


/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
	int i, n;


	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		if (player_birth_aux()) break;
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



