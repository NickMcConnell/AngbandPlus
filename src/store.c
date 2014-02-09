/* File: store.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"
#include "ui-menu.h"
#include "game-event.h"



/*** Constants and definitions ***/

/* Easy names for the elements of the 'scr_places' arrays. */
enum
{
	LOC_PRICE = 0,
	LOC_OWNER,
	LOC_HEADER,
	LOC_ITEMS_START,
	LOC_ITEMS_END,
	LOC_MORE,
	LOC_HELP_CLEAR,
	LOC_HELP_PROMPT,
	LOC_AU,
	LOC_WEIGHT,
	LOC_CUR_QUEST1,
	LOC_CUR_QUEST2,
	LOC_GUILD_REP,

	LOC_MAX
};

/* Places for the various things displayed onscreen */
static unsigned int scr_places_x[LOC_MAX];
static unsigned int scr_places_y[LOC_MAX];


/* State flags */
#define STORE_GOLD_CHANGE      0x01
#define STORE_FRAME_CHANGE     0x02

#define STORE_SHOW_HELP        0x04

#define STORE_MAX_ITEM			99


/* Compound flag for the initial display of a store */
#define STORE_INIT_CHANGE		(STORE_FRAME_CHANGE | STORE_GOLD_CHANGE)


/** Variables to maintain state ***/

/* Flags for the display */
static u16b store_flags;

static int services_min;
static int services_max;
static int quests_min;
static int quests_max;




/*List of various store services allowed*/
/*
 * Timed effects
 */
enum
{
	SERVICE_ENCHANT_ARMOR	= 0,
	SERVICE_ENCHANT_TO_HIT,
	SERVICE_ENCHANT_TO_DAM,
	SERVICE_ELEM_BRAND_WEAP,
	SERVICE_ELEM_BRAND_AMMO,
	SERVICE_RECHARGING,
	SERVICE_IDENTIFY,
	SERVICE_IDENTIFY_FULLY,
	SERVICE_CURE_CRITICAL,
	SERVICE_RESTORE_LIFE_LEVELS,
	SERVICE_REMOVE_CURSE,
	SERVICE_REMOVE_HEAVY_CURSE,
	SERVICE_RESTORE_STAT,
	SERVICE_INCREASE_STAT,
	SERVICE_CREATE_RANDART,
	SERVICE_PROBE_QUEST_MON,
	SERVICE_BUY_HEALING_POTION,
	SERVICE_BUY_LIFE_POTION,
	SERVICE_BUY_SCROLL_BANISHMENT,
	SERVICE_FIREPROOF_BOOK,
	SERVICE_QUEST_DEFER_REWARD,
	SERVICE_ABANDON_QUEST,
	SERVICE_QUEST_REWARD_RANDART,
	SERVICE_QUEST_REWARD_INC_HP,
	SERVICE_QUEST_REWARD_INC_STAT,
	SERVICE_QUEST_REWARD_AUGMENTATION,


	STORE_SERVICE_MAX
};

#define QUEST_REWARD_HEAD	SERVICE_QUEST_DEFER_REWARD
#define QUEST_REWARD_TAIL	SERVICE_QUEST_REWARD_AUGMENTATION

/* Indicates which store offers the service*/
static byte service_store[STORE_SERVICE_MAX] =
{
	STORE_ARMOR,		/*  SERVICE_ENCHANT_ARMOR   	*/
	STORE_WEAPON,		/*  SERVICE_ENCHANT_TO_HIT   	*/
	STORE_WEAPON,		/*  SERVICE_ENCHANT_TO_DAM   	*/
	STORE_WEAPON,		/*  SERVICE_ELEM_BRAND_WEAP   	*/
	STORE_WEAPON,		/*  SERVICE_ELEM_BRAND_AMMO   	*/
	STORE_MAGIC, 		/*  SERVICE_RECHARGING   		*/
	STORE_MAGIC, 		/*  SERVICE_IDENTIFY   			*/
	STORE_MAGIC, 		/*  SERVICE_IDENTIFY_FULLY		*/
	STORE_TEMPLE, 		/*  SERVICE_CURE_CRITICAL  		*/
	STORE_TEMPLE, 		/*  SERVICE_RESTORE_LIFE_LEVELS	*/
	STORE_TEMPLE, 		/*  SERVICE_REMOVE_CURSE   		*/
	STORE_TEMPLE, 		/*  SERVICE_REMOVE_HEAVY_CURSE	*/
	STORE_ALCHEMY, 		/*  SERVICE_RESTORE_STAT   		*/
	STORE_ALCHEMY, 		/*  SERVICE_INCREASE_STAT   	*/
	STORE_GUILD,		/*  SERVICE_CREATE_RANDART   	*/
	STORE_GUILD,		/*	SERVICE_PROBE_QUEST_MON		*/
	STORE_TEMPLE,		/*	SERVICE_BUY_HEALING_POTION	*/
	STORE_TEMPLE,		/*	SERVICE_BUY_LIFE_POTION		*/
	STORE_MAGIC,		/*	SERVICE_BUY_SCROLL_BANISHMENT	*/
	STORE_BOOKSHOP,		/*	SERVICE_FIREPROOF_BOOK		*/
	STORE_GUILD,		/*	SERVICE_QUEST_DEFER_REWARD	*/
	STORE_GUILD,		/*	SERVICE_ABANDON_QUEST		*/
	STORE_GUILD,		/*	SERVICE_QUEST_REWARD_RANDART	*/
	STORE_GUILD,		/*	SERVICE_QUEST_REWARD_INC_HP		*/
	STORE_GUILD,		/*	SERVICE_QUEST_REWARD_INC_STAT	*/
	STORE_GUILD			/*	SERVICE_QUEST_REWARD_AUGMENTATION	*/

};

/* Indicates the base price of the service*/
static u32b service_price[STORE_SERVICE_MAX] =
{
	125,				/*  SERVICE_ENCHANT_ARMOR   	*/
	125,				/*  SERVICE_ENCHANT_TO_HIT   	*/
	125,				/*  SERVICE_ENCHANT_TO_DAM   	*/
	35000,				/*  SERVICE_ELEM_BRAND_WEAP   	*/
	17500,				/*  SERVICE_ELEM_BRAND_AMMO   	*/
	175, 				/*  SERVICE_RECHARGING   		*/
	75, 				/*  SERVICE_IDENTIFY   			*/
	4500,		 		/*  SERVICE_IDENTIFY_FULLY		*/
	75, 				/*  SERVICE_CURE_CRITICAL  		*/
	1000, 				/*  SERVICE_RESTORE_LIFE_LEVELS	*/
	300, 				/*  SERVICE_REMOVE_CURSE   		*/
	15000, 				/*  SERVICE_REMOVE_HEAVY_CURSE	*/
	700, 				/*  SERVICE_RESTORE_STAT   		*/
	37500L, 			/*  SERVICE_INCREASE_STAT   	*/
	750000L,			/*  SERVICE_CREATE_RANDART   	*/
	150,				/*	SERVICE_PROBE_QUEST_MON		*/
	20000L,				/*	SERVICE_BUY_HEALING_POTION	*/
	125000L,			/*	SERVICE_BUY_LIFE_POTION		*/
	125000L,			/*	SERVICE_BUY_SCROLL_BANISHMENT	*/
	100000L,			/*  SERVICE_FIREPROOF_BOOK 		*/
	0,					/*	SERVICE_QUEST_DEFER_REWARD	*/
	0,					/*  SERVICE_ABANDON_QUEST 		*/
	0,					/*	SERVICE_QUEST_REWARD_RANDART	*/
	0,					/*	SERVICE_QUEST_REWARD_INC_HP		*/
	0,					/*	SERVICE_QUEST_REWARD_INC_STAT	*/
	0					/*	SERVICE_QUEST_REWARD_AUGMENTATION	*/

};

/*
 * Indicates the base price of the service.  [v] means the price varies depending on the item
 */
static cptr service_names[STORE_SERVICE_MAX] =
{
	"Enchant armor [price varies]",				/*  SERVICE_ENCHANT_ARMOR   	*/
	"Enchant weapon to-hit [price varies]",			/*  SERVICE_ENCHANT_TO_HIT   	*/
	"Enchant weapon to-dam [price varies]",			/*  SERVICE_ENCHANT_TO_DAM   	*/
	"Elemental Brand a weapon [price varies]",	/*  SERVICE_ELEM_BRAND_WEAP   	*/
	"Elemental brand some ammunition[price varies]",	/*  SERVICE_ELEM_BRAND_AMMO */
	"Recharge item [price varies]",				/*  SERVICE_RECHARGING   		*/
	"Identify item",							/*  SERVICE_IDENTIFY   			*/
	"*Identify* item",							/*  SERVICE_IDENTIFY_FULLY		*/
	"Cure Critical Wounds",	 					/*  SERVICE_CURE_CRITICAL  		*/
	"Restore Life Levels",						/*  SERVICE_RESTORE_LIFE_LEVELS	*/
	"Remove curse", 							/*  SERVICE_REMOVE_CURSE   		*/
	"Remove *curse*", 							/*  SERVICE_REMOVE_HEAVY_CURSE	*/
	"Restore stat", 							/*  SERVICE_RESTORE_STAT   		*/
	"Increase stat", 							/*  SERVICE_INCREASE_STAT   	*/
	"Create Artifact[price varies]",			/*  SERVICE_CREATE_RANDART   	*/
	"Probe a Quest Monster[price varies]",		/*	SERVICE_PROBE_QUEST_MON		*/
	"Purchase Potion of Healing",				/*	SERVICE_BUY_HEALING_POTION	*/
	"Purchase Potion of Life",					/*	SERVICE_BUY_LIFE_POTION		*/
	"Purchase Scroll of Mass Banishment",		/*	SERVICE_BUY_SCROLL_BANISHMENT	*/
	"Make Spell Book Fireproof[price varies]",	/*  SERVICE_FIREPROOF_BOOK */
	"Defer Quest Reward",						/*	SERVICE_QUEST_DEFER_REWARD	*/
	"Abandon Your Quest",						/*  SERVICE_ABANDON_QUEST 		*/
	"Create Artifact Quest Reward",				/*	SERVICE_QUEST_REWARD_RANDART	*/
	"Permanent Hit Point Increase Reward",		/*	SERVICE_QUEST_REWARD_INC_HP		*/
	"Permanent Stat Increase Reward",			/*	SERVICE_QUEST_REWARD_INC_STAT	*/
	"Permanent Stats Augmentation Reward"		/*	SERVICE_QUEST_REWARD_AUGMENTATION	*/
};


static byte services_offered[STORE_SERVICE_MAX];
static byte quests_offered[QUEST_SLOT_MAX];

/* Quest Titles*/
static cptr quest_title[QUEST_SLOT_MAX] =
{
	"Monster or Unique Quest",	/* QUEST_MONSTER*/
  	"Guardian Quest",			/* QUEST_GUARDIAN */
	"Pit or Nest Quest",		/* QUEST_PIT*/
	"Wilderness Quest",			/* QUEST_WILDERNESS */
  	"Level Quest",				/* QUEST_THEMED_LEVEL*/
	"Vault Quest",				/* QUEST_VAULT*/
	"Arena Quest",				/* QUEST_ARENA_LEVEL */
	"Labyrinth Quest",			/* QUEST_LABYRINTH_LEVEL */
	"Greater Vault Quest"		/* QUEST_SLOT_GREATER_VAULT */
};



/*** Utilities ***/

/*
 * Return the owner struct for the given store.
 */
static owner_type *store_owner(int st)
{
	store_type *st_ptr = &store[st];
	return &b_info[(st * z_info->b_max) + st_ptr->owner];
}


/* Randomly select one of the entries in an array */
#define ONE_OF(x)	x[randint0(N_ELEMENTS(x))]


/*
 * Shopkeeper welcome messages.
 *
 * The shopkeeper's name must come first, then the character's name.
 */
static const char *comment_welcome[] =
{
	"",
	"%s nods to you.",
	"%s says hello.",
	"%s: \"See anything you like, adventurer?\"",
	"%s: \"How may I help you, %s?\"",
	"%s: \"Welcome back, %s.\"",
	"%s: \"A pleasure to see you again, %s.\"",
	"%s: \"How may I be of assistance, good %s?\"",
	"%s: \"You do honour to my humble store, noble %s.\"",
	"%s: \"I and my family are entirely at your service, glorious %s.\""
};

/*
 * Messages for reacting to purchase prices.
 */
static const char *comment_worthless[] =
{
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!",
	"The shopkeeper wails in anguish!",
	"The shopkeeper mutters in disgust."
};

static const char *comment_bad[] =
{
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you."
};

static const char *comment_accept[] =
{
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!"
};

static const char *comment_good[] =
{
	"Cool!",
	"You've made my day!",
	"The shopkeeper sniggers.",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly."
};

static const char *comment_great[] =
{
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully.",
	"Wow.  I'm going to name my new villa in your honour."
};

int stats[A_MAX];
#define STAT_ESCAPE		A_MAX
#define STAT_NO_CHOICE	A_MAX + 1

/**
 * Item tag/command key
 */
static char stats_tag(menu_type *menu, int oid)
{
	/* Caution - could be a problem here if KTRL commands were used */
	return I2A(oid);
}

/**
 * Display an entry on a command menu
 */
static void stat_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	char buf[80];
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);
	int pr_stat = stats[oid];

	/* Write the description */
	c_prt(attr, stat_names[pr_stat], row, col);

	/* Note if the stat is maxed */
	if (p_ptr->stat_max[pr_stat] == 18+100) put_str("!", row, col+3);

	/* Internal "natural" maximum value */
	cnv_stat(p_ptr->stat_max[pr_stat], buf, sizeof(buf));
	c_put_str(TERM_L_GREEN, buf, row, col+5);

	if (!adult_maximize)
	{
		/* Race Bonus */
		strnfmt(buf, sizeof(buf), "%+3d", (rp_ptr->r_adj[pr_stat] + p_ptr->stat_quest_add[pr_stat]));
		c_put_str(TERM_L_BLUE, buf, row, col+11);

		/* Class Bonus */
		strnfmt(buf, sizeof(buf), "%+3d", cp_ptr->c_adj[pr_stat]);
		c_put_str(TERM_L_BLUE, buf, row, col+14);
	}

	/* Equipment Bonus */
	strnfmt(buf, sizeof(buf), "%+3d", p_ptr->state.stat_add[pr_stat]);
	c_put_str(TERM_L_BLUE, buf, row, col+18);

	/* Resulting "modified" maximum value */
	cnv_stat(p_ptr->state.stat_top[pr_stat], buf, sizeof(buf));
	c_put_str(TERM_L_GREEN, buf, row, col+22);

	/* Only display stat_use if not maximal */
	if (p_ptr->state.stat_use[pr_stat] < p_ptr->state.stat_top[pr_stat])
	{
		cnv_stat(p_ptr->state.stat_use[pr_stat], buf, sizeof(buf));
		c_put_str(TERM_YELLOW, buf, row, col+29);
	}
}

/**
 * Handle user input from a command menu
 */
static bool stat_action(char cmd, void *db, int oid)
{
	return TRUE;
}
/*
 * Pick a stat.
 */
static int stats_menu(int service)
{
	int i;
	int count = 0;
	char title[120];
	menu_type menu;
	menu_iter menu_f = { stats_tag, NULL, stat_display, stat_action };
	ui_event_data evt = { EVT_NONE, 0, 0, 0, 0 };
	region area = { 0, 1, -1, -1 };
	int cursor = 0;
	int return_value = STAT_NO_CHOICE;

	/* Count up the stats that require the service */
	for (i = 0; i < A_MAX; i++)
	{
		/* Add the stat that need to be restored */
		if (service == SERVICE_RESTORE_STAT)
		{
			if (p_ptr->stat_cur[i] < p_ptr->stat_max[i]) stats[count++] = i;
		}
		else if (service == SERVICE_INCREASE_STAT)
		{
			if (p_ptr->stat_max[i] < 18+100) stats[count++] = i;
		}
		/* must be SERVICE_QUEST_REWARD_INC_STAT*/
		else
		{
			if (p_ptr->stat_quest_add[i] <= 2) stats[count++] = i;
		}

	}

	/* None of the player stats need servicing */
	if (!count) return (STAT_NO_CHOICE);

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.count = count;
	if (!adult_maximize) menu.title = "              Self        EB   Best";
	else menu.title = "              Self RB CB  EB   Best";
	menu.menu_data = stats;
	if (service == SERVICE_RESTORE_STAT)
	{
		my_strcpy(title, " Please select a stat to restore.", sizeof(title));
	}
	else if (service == SERVICE_INCREASE_STAT)
	{
		my_strcpy(title, " Please select a stat to increase.", sizeof(title));
	}
	/* must be SERVICE_QUEST_REWARD_INC_STAT*/
	else
	{
		my_strcpy(title, " Please select a stat to permanently increase.", sizeof(title));
	}

	menu.prompt = title;
	area.page_rows = count + 4;
	area.width = MAX(strlen(menu.title), strlen(menu.prompt)) + 7;

	menu_init(&menu, MN_SKIN_SCROLL, &menu_f, &area);

	/* Make some buttons */
	button_backup_all();
	button_kill_all();
	button_add("[ESC]", ESCAPE);

	/* Add buttons for all stats that require the service */
	for (i = 0; i < count; i++)
	{
		/* Only use the first three digits */
		char stat_name[4];

		/*
		 * Very important to use a string function that handles buffer overflow,
		 * since a string 5 characters long is being put into a variable 3 spaces long.
		 */
		my_strcpy(stat_name, stat_names[stats[i]], sizeof(stat_name));

		button_add(format("[%s]", stat_name),I2A(i));
	}

	event_signal(EVENT_MOUSEBUTTONS);

	/* Select an entry */
	while (return_value == STAT_NO_CHOICE)
	{
		evt = menu_select(&menu, &cursor, EVT_MOVE);

		if (evt.key == ESCAPE)
		{
			return_value = STAT_ESCAPE;
			continue;
		}

		else if (evt.type == EVT_BUTTON)
		{
			switch (evt.key)
			{
				case 'a':
				case 'b':
				case 'c':
				case 'd':
				case 'e':
				case 'f':
				{
					return_value = stats[A2I(evt.key)];
					continue;
				}
					default:  	break;
			}
		}
		else if (evt.type == EVT_SELECT)
		{
			return_value = stats[cursor];
			continue;
		}
	}

	/* Load screen */
	button_kill_all();
	button_restore();
	return (return_value);
}



/*
 * The greeting a shopkeeper gives the character says a lot about his
 * general attitude.
 *
 * Taken and modified from Sangband 1.0.
 */
static void prt_welcome(const owner_type *ot_ptr)
{
	char short_name[20];
	cptr player_name;
	const char *owner_name = &b_name[ot_ptr->owner_name];

	/* We go from level 1 - 50  */
	size_t i = ((unsigned)p_ptr->lev - 1) / 5;

	/* Sanity check in case we increase the max level carelessly */
	i = MIN(i, N_ELEMENTS(comment_welcome) - 1);

	/* Only show the message one in four times to stop it being irritating. */
	if (!one_in_(4)) return;

	/* Welcome the character */
	if (i)
	{
		int j;

		/* Extract the first name of the store owner (stop before the first space) */
		for (j = 0; owner_name[j] && owner_name[j] != ' '; j++)
			short_name[j] = owner_name[j];

		/* Truncate the name */
		short_name[j] = '\0';


		/* Get a title for the character */
		if ((i % 2) && randint0(2)) player_name = get_player_title();
		else if (randint0(2))       player_name = op_ptr->full_name;
		else                        player_name = (p_ptr->psex == SEX_MALE ? "sir" : "lady");

		/* Balthazar says "Welcome" */
		prt(format(comment_welcome[i], short_name, player_name), 0, 0);
	}
}

/*
 * Marks everything to re-draw, update, and notice when an item
 * or service is bought or sold in a store.  Note you can sell
 * directly from your equipment, so everything needs to be updated.
 */
static void store_updates(void)
{
	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_INVEN | PR_EQUIP | PR_MESSAGE);

	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_ITEMLIST);

}

/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(s32b price, s32b value, s32b guess)
{
	/* Item was worthless, but we bought it */
	if ((value <= 0) && (price > value))
		message(MSG_STORE1, 0, ONE_OF(comment_worthless));

	/* Item was cheaper than we thought, and we paid more than necessary */
	else if ((value < guess) && (price > value))
		message(MSG_STORE2, 0, ONE_OF(comment_bad));

	/* Item was a good bargain, and we got away with it */
	else if ((value > guess) && (value < (4 * guess)) && (price < value))
		message(MSG_STORE3, 0, ONE_OF(comment_good));

	/* Item was a great bargain, and we got away with it */
	else if ((value > guess) && (price < value))
		message(MSG_STORE4, 0, ONE_OF(comment_great));
}


/*
 * We store the current "store pointer" here so everyone can access it
 */
static store_type *st_ptr = NULL;

/*
 * We store the current "owner type" here so everyone can access it
 */
static owner_type *ot_ptr = NULL;

#define STORE_NONE -1

/* Get the current store number, or STORE_NONE if not in a store */
static int current_store(void)
{
	if (cave_shop_bold(p_ptr->py,p_ptr->px))

		return (f_info[cave_feat[p_ptr->py][p_ptr->px]].f_power);

	return STORE_NONE;
}


/*  Store Services */

/*return false if the player doesn't have enough gold*/
static bool check_gold(s32b price)
{
	if (price > p_ptr->au)
	{
		msg_format("It would cost you %d gold.  You don't have enough.", price);

		return (FALSE);
	}

	return (TRUE);
}

/* Percent decrease or increase in price of goods		 */
int moria_chr_adj()
{
	int charisma  = p_ptr->state.stat_use[A_CHR];

	if (charisma > 117) 		return(90);
	else if (charisma > 107) 	return(92);
	else if (charisma > 87)		return(94);
	else if (charisma > 67)		return(96);
	else if (charisma > 18)		return(98);
	else switch(charisma)
    {
		case 18:	return(100);
		case 17:	return(101);
		case 16:	return(102);
		case 15:	return(103);
		case 14:	return(104);
		case 13:	return(106);
		case 12:	return(108);
		case 11:	return(110);
		case 10:	return(112);
		case 9:  return(114);
		case 8:  return(116);
		case 7:  return(118);
		case 6:  return(120);
		case 5:  return(122);
		case 4:  return(125);
		case 3:  return(130);
		default: return(100);
    }
}


static void init_services_and_quests(int store_num)
{
	int i;
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Wipe all the old information. */
	services_min = 0;
 	services_max = 0;
	quests_min = 0;
	quests_max = 0;
	for (i = 0; i < STORE_SERVICE_MAX; i++) services_offered[i] = -1;
	for (i = 0; i < QUEST_SLOT_MAX; i++) quests_offered[i] = -1;

	/*Nothing in the store*/
	if (store_num == STORE_HOME) return;

	/*First, initialize the services*/
	/* Get the store services for the current store*/
	for (i = 0; i < STORE_SERVICE_MAX; i++)
	{

		/* Check if the services option is disabled */
		if (adult_no_store_services) break;

		/* Services are store-specific */
		if (service_store[i] != store_num) continue;

		/*
		 * The guild only offers certain services
		 * depending on the active quest.
		 */

		/* Offer this service only if there is a quest to abandon. */
		if (i == SERVICE_ABANDON_QUEST)
		{
			/*We finished the quest, why abandon it?*/
			if (guild_quest_complete()) continue;

			/* No current guild quest */
			if (!q_ptr->q_type) continue;

			if (!guild_quest_level()) continue;
		}

		else if ((i >= QUEST_REWARD_HEAD) &&
				 (i <= QUEST_REWARD_TAIL))
		{
			/* Not currently offering a reward */
			if (!guild_quest_complete()) continue;

			if (i == SERVICE_QUEST_REWARD_INC_HP)
			{
				if (!(q_ptr->q_reward & (REWARD_INC_HP)))
				{
					/* This service should not be offered */
					continue;
				}
			}
			if (i == SERVICE_QUEST_REWARD_RANDART)
			{
				if (!(q_ptr->q_reward & (REWARD_RANDART)))
				{
					/* This service should not be offered */
					continue;
				}
			}
			else if (i == SERVICE_QUEST_REWARD_INC_STAT)
			{
				if (!(q_ptr->q_reward & (REWARD_INC_STAT)))
				{
					/* This service should not be offered */
					continue;
				}
			}
			else if (i == SERVICE_QUEST_REWARD_AUGMENTATION)
			{
				if (!(q_ptr->q_reward & (REWARD_AUGMENTATION)))
				{
					/* This service should not be offered */
					continue;
				}
			}
		}

		/* Filter out quest-specific services when appropriate. */
		else if (i == SERVICE_PROBE_QUEST_MON)
		{
			if (!guild_quest_level()) continue;
			if (guild_quest_complete()) continue;

			if (q_ptr->q_type == QUEST_VAULT) continue;
			if (q_ptr->q_type == QUEST_GREATER_VAULT) continue;
			if (quest_type_collection(q_ptr)) continue;
			if (quest_multiple_r_idx(q_ptr)) continue;
		}

		/* Offer this service. */
		services_offered[services_max++] = i;
	}

	quests_min = services_max;
	quests_max = quests_min;

	/*
	 * Now, initialize the quests,
	 * but only if the player is in the guild are active.
	 */
	if (store_num != STORE_GUILD) return;

	/* No quest options if they currently have an active one. */
	if (guild_quest_level()) return;

	/*Honor the no quests option*/
	if (!can_quest_at_level()) return;

	/* Check if the no_quests option is on */
	if (adult_no_quests) return;

	/*get a list of allowable quests*/
	for (i = 0;i < QUEST_SLOT_MAX; i++)
	{
		if (quest_allowed(i))
		{
			/*Allow*/
			quests_offered[quests_max - quests_min] = i;
			quests_max++;
		}
	}
}

/*
 * Calculate the service price.  The guild has no race preferences, so we
 * use the player fame instead.
 */
static u32b price_services(int store_num, int choice)
{

	/* get the service price*/
	u32b price = service_price[choice];

	/*adjust price, but not for the guild*/
	if (store_num != STORE_GUILD)
	{
		/* Extract the "minimum" price */
		if (game_mode == GAME_NPPMORIA)
		{
			price = ((price * moria_chr_adj()) / 100L);
		}
		else price = ((price * adj_chr_gold[p_ptr->state.stat_ind[A_CHR]]) / 100L);
	}

	/*Guild price factoring*/
	else
	{
		if (p_ptr->q_fame < 1000) price += price * (1000 - p_ptr->q_fame) / 1000;
	}

	return(price);
}


/*
 * Process the chosen service from a store
 */
static bool store_service_aux(int store_num, s16b choice)
{
	object_type *o_ptr;
	object_kind *k_ptr;
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	char o_name[80];
	char title[80];

	byte lev;

	cptr q, s;

	int item;

	char prompt[160];

	u32b price = price_services(store_num, choice);
	get_title(title, sizeof(title));

	switch (choice)
	{
		case SERVICE_ENCHANT_ARMOR:
		case SERVICE_ENCHANT_TO_HIT:
		case SERVICE_ENCHANT_TO_DAM:
		{
			s16b add_to;
			s16b counter = 1;

			u32b f1, f2, f3, fn;

			/* Enchant armor if requested */
			if (choice == SERVICE_ENCHANT_ARMOR)
			{
				item_tester_hook = item_tester_hook_ided_armour;
			}
			else item_tester_hook = item_tester_hook_ided_weapon;

			/* Get an item */
			q = "Enchant which item? ";
			s = "You have nothing to enchant.";
			if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_QUIVER))) return (FALSE);

			/*Got the item*/
			o_ptr = &inventory[item];
			k_ptr = &k_info[o_ptr->k_idx];

			/* Extract the flags */
			object_flags(o_ptr, &f1, &f2, &f3, &fn);

			if (choice == SERVICE_ENCHANT_ARMOR) add_to = o_ptr->to_a;
			else if (choice == SERVICE_ENCHANT_TO_HIT) add_to = o_ptr->to_h;
			/* to-damage*/
			else add_to = o_ptr->to_d;

			/* Description, shorten it for artifacts */
			if (o_ptr->art_num) object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);
			else object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

			/*
			 * We will eventually run into the u32 variable max number, so
			 * cut off the + allowed
			 */
			if (add_to >= 15)
			{
				msg_format("%s %s cannot be enchanted any further",
	           ((item >= 0) ? "Your" : "The"), o_name);

			 	return (FALSE);
			}

			/* Missiles are easier to enchant */
			if ((o_ptr->tval == TV_BOLT) ||
	    		(o_ptr->tval == TV_ARROW) ||
	    		(o_ptr->tval == TV_SHOT))
			{
				price = price / 20;
			}

			/* Greater to-hit and to-dam makes things more expensive*/
			while (add_to >= counter)
			{
				price += (price * 8) / 10;

				counter ++;
			}

			/*multiply for quantity*/
			price *= o_ptr->number;

			/*artifacts are double*/
			if (o_ptr->art_num) price *= 2;

			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			strnfmt(prompt, sizeof(prompt), "Spend %d gold to enchant %s? ",
							price, o_name);
			if (!get_check(prompt)) return (FALSE);

			/*reduce the gold*/
			p_ptr->au -= price;

			/* Description */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

			/* Describe */
			msg_format("%s %s glow%s brightly!",
	           ((item >= 0) ? "Your" : "The"), o_name,
	           ((o_ptr->number > 1) ? "" : "s"));

			if (choice == SERVICE_ENCHANT_ARMOR) o_ptr->to_a ++;
			else if (choice == SERVICE_ENCHANT_TO_HIT) o_ptr->to_h ++;
			/* to-damage*/
			else o_ptr->to_d++;

			/* Break curse */
			if (cursed_p(o_ptr) &&
				(!(k_ptr->k_flags3 & (TR3_PERMA_CURSE))) &&
				 (add_to >= 0) && (rand_int(100) < 25))
			{
				msg_print("The curse is broken!");

				/* Uncurse the object */
				uncurse_object(o_ptr);

			}

			return (TRUE);
		}

		case SERVICE_ELEM_BRAND_WEAP:
		case SERVICE_ELEM_BRAND_AMMO:
		{
			byte brand_type;

			/* Enchant weapon if requested */
			if (choice == SERVICE_ELEM_BRAND_WEAP)
			{
				item_tester_hook = item_tester_hook_wieldable_ided_weapon;
			}
			/*Ammo*/
			else item_tester_hook = item_tester_hook_ided_ammo;

			/* Get an item */
			q = "Brand which item? ";
			s = "You have nothing to Brand.";
			if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_QUIVER))) return (FALSE);

			/*Got the item*/
			o_ptr = &inventory[item];

			/* Description, shorten it for artifacts */
			if (o_ptr->art_num) object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);
			else object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

			/*If artifact, or ego item, don't bother*/
			if ((o_ptr->art_num) || (o_ptr->ego_num))
			{
				msg_format("%^s cannot be branded!", o_name);

				return (FALSE);
			}

			/* Missiles are easier to enchant */
			if ((o_ptr->tval == TV_BOLT) ||
	    		(o_ptr->tval == TV_ARROW) ||
	    		(o_ptr->tval == TV_SHOT))
			{
				price = price / 20;
			}

			/*multiply for quantity*/
			price *= o_ptr->number;

			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			strnfmt(prompt, sizeof(prompt), "Spend %d gold to brand %s? ",
							price, o_name);
			if (!get_check(prompt)) return (FALSE);

			if (choice == SERVICE_ELEM_BRAND_WEAP)
			{
				if (one_in_(2)) brand_type = BRAND_OFFSET_FLAME;
				else brand_type = BRAND_OFFSET_FROST;

			}
			/*ammo*/
			else
			{
				/* Select the brand */
				if (one_in_(3))
					brand_type = EGO_AMMO_FLAME;
				else if (one_in_(2))
					brand_type = EGO_AMMO_FROST;
				else brand_type = EGO_AMMO_VENOM;
			}

			/*Brand*/
			if (brand_object(o_ptr, brand_type, FALSE))
			{
				p_ptr->au -= price;
				return (TRUE);
			}
			msg_format("Branding failed.");
			return (FALSE);
		}

		case SERVICE_RECHARGING:
		{

			/* Only accept legal items, which are wands and staffs */
			item_tester_hook = item_tester_hook_recharge;

			/* Get an item */
			q = "Recharge which item? ";
			s = "You have nothing to recharge.";
			if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

			/*Got the item*/
			o_ptr = &inventory[item];

			/* Description */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

			/* Extract the object "level" */
			lev = k_info[o_ptr->k_idx].k_level;

			/*base price on level*/
			price += price * (lev / 2);

			/*get the price for rods*/
			if (o_ptr->tval == TV_ROD)
			{
				if (!o_ptr->timeout)
				{
					/* Describe */
					msg_format("The %s %s not require re-charging!",
	          			o_name, (o_ptr->number > 1 ? "do" : "does"));

					return (FALSE);
				}
				else
				{
					price += (price * o_ptr->timeout) / 20;
				}
			}

			/*Wands, and Staffs*/
			else
			{
				price += o_ptr->pval * price;

				/*Bigger charage for a stack of staffs or wands*/
				if (o_ptr->number > 1) price += (o_ptr->number - 1) * price;
			}

			/*Too expensive*/
			if (!check_gold(price)) return(FALSE);

			strnfmt(prompt, sizeof(prompt), "Spend %d gold to recharge %s?  ",
							price, o_name);
			if (!get_check(prompt)) return(FALSE);

			/*re-charge the rods*/
			if (o_ptr->tval == TV_ROD)
			{
				o_ptr->timeout = 0;
			}
			/*Wands and staffs*/
			else
			{
				recharge_staff_wand(o_ptr, 75);
			}

			/*We re-charged an item*/
			p_ptr->au -= price;

			return (TRUE);
		}
		case SERVICE_IDENTIFY:
		{
			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			/*We identified an item*/
			if (ident_spell())
			{
				p_ptr->au -= price;
				return (TRUE);
			}
			return (FALSE);
		}
		case SERVICE_IDENTIFY_FULLY:
		{
			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			/*We identified an item*/
			if (identify_fully())
			{
				p_ptr->au -= price;
				return (TRUE);
			}
			return (FALSE);
		}
		case SERVICE_CURE_CRITICAL:
		{
			bool healed = FALSE;

			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			strnfmt(prompt, sizeof(prompt), "Spend %d gold to cure critical wounds? ",
							price);
			if (!get_check(prompt)) return (FALSE);

			/*Heal the player, note if they actually need healing*/
			if (hp_player(damroll(8, 10))) healed = TRUE;
			if (clear_timed(TMD_BLIND, TRUE)) healed = TRUE;
			if (clear_timed(TMD_CONFUSED, TRUE)) healed = TRUE;
			if (clear_timed(TMD_POISONED, TRUE)) healed = TRUE;
			if (set_stun(0)) healed = TRUE;
			if (set_cut(0)) healed = TRUE;

			/*We identified an item*/
			if (healed)
			{
				p_ptr->au -= price;
				return (TRUE);
			}
			msg_format("You do not require any healing services.");

			return (FALSE);
		}
		case SERVICE_RESTORE_LIFE_LEVELS:
		{
			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			strnfmt(prompt, sizeof(prompt), "Spend %d gold to restore life levels? ",
							price);
			if (!get_check(prompt)) return (FALSE);

			/*We restored the player*/
			if (restore_level())
			{
				p_ptr->au -= price;
				return (TRUE);
			}
			/* Not needed*/
			msg_format("Your life levels do not require restoring.");
			return (FALSE);
		}
		case SERVICE_REMOVE_CURSE:
		case SERVICE_REMOVE_HEAVY_CURSE:
		{
			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			/*We removed a curse an item, charge the player*/
			if (remove_curse(choice == SERVICE_REMOVE_HEAVY_CURSE ? TRUE : FALSE))
			{
				p_ptr->au -= price;
				return (TRUE);
			}

			else msg_format("No items had a curse removed.");
			return (FALSE);

		}
		case SERVICE_RESTORE_STAT:
		case SERVICE_INCREASE_STAT:
		case SERVICE_QUEST_REWARD_INC_STAT:
		{

			int result;

			/*Too expensive*/
			if (choice != SERVICE_QUEST_REWARD_INC_STAT)
			{
				if (!check_gold(price)) return (FALSE);
			}
			else
			{
				/* Ask confirmation */
				if (!get_check(format("Choose a stat to permanently increase, %s?", title))) return (FALSE);
			}

			screen_save();

			/* returning false??*/
			result = stats_menu(choice);

			if (result == STAT_NO_CHOICE)
			{

				if (choice == SERVICE_RESTORE_STAT)
				{
					screen_load();
					msg_format("None of your stats need restoring.");
				}
				else if (choice == SERVICE_INCREASE_STAT)
				{
					screen_load();
					msg_format("Your stats cannot be increased any further.");
				}
				/* must be SERVICE_QUEST_REWARD_INC_STAT*/
				else
				{
					screen_load();
					msg_format("Your stats cannot be permanently increased any further.");
				}
				return (FALSE);
			}

			/*player chose escape - do nothing */
			if (result == STAT_ESCAPE)
			{
				screen_load();
				return (FALSE);
			}

			/*restore the stat*/
			if (choice == SERVICE_RESTORE_STAT)
			{
				/*charge it*/
				if (do_res_stat(result)) p_ptr->au -= price;
				else msg_format("Your %s does not need restoring.",
										stat_names_full[result]);

			}
			else if (choice == SERVICE_INCREASE_STAT)
			{
				if (do_inc_stat(result)) p_ptr->au -= price;
				else msg_format("Your %s cannot be increased any further.",
									stat_names_full[result]);
			}
			/* must be SERVICE_QUEST_REWARD_INC_STAT*/
			else
			{
				Term_gotoxy(0, 0);
				do_perm_stat_boost(result);
				guild_quest_wipe(TRUE);
			}
			screen_load();
			return (TRUE);
		}

		case SERVICE_CREATE_RANDART:
		{
			s32b o_value;

			if ((adult_no_artifacts) || (adult_no_xtra_artifacts))
			{
				msg_print("Nothing happens.");
				return (FALSE);
			}

			/* Only accept legal items */
			item_tester_hook = item_tester_hook_randart;

			/* Get an item */
			q = format("Choose an item to be made into an artifact, %s.", title);
			s = format("You have no eligible item, %s.", title);
			if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

			/*Got the item*/
			o_ptr = &inventory[item];

			/*Got the object kind*/
			k_ptr = &k_info[o_ptr->k_idx];

			/* Description */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

			/* Get the "value" of the item */
			o_value = k_ptr->cost * 50;

			/*Get the price for the item*/
			price = price + o_value;

			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			strnfmt(prompt, sizeof(prompt), "Spend %d gold to make %s into an artifact? ",
							price, o_name);
			if (!get_check(prompt)) return (FALSE);

			/*re-use the o_value variable for a completely different purpose*/
			/*extra power bonus for expensive items and high player fame*/
			o_value = p_ptr->q_fame / 20 + MAX((k_ptr->cost / 2000), p_ptr->q_fame / 50);

			/*Hack - add in any to-hit and to-value, since they will be erased*/
			o_value += (o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) / 2;

			/*actually create the Randart, or handle failure*/
			if (make_one_randart(o_ptr, o_value, TRUE))
			{
				p_ptr->au -= price;

				/* Identify it fully */
				object_aware(o_ptr);
				object_known(o_ptr);

				/* Mark the history */
				o_ptr->origin_nature = ORIGIN_ACQUIRE;
				o_ptr->origin_r_idx = 0;
				o_ptr->origin_dlvl = 0;



				/* Mark the item as fully known */
				o_ptr->ident |= (IDENT_MENTAL);

				/*Let the player know what they just got*/
				object_info_screen(o_ptr);

				return (TRUE);
			}

			msg_print("The attempt at making an artifact has failed");
			return (FALSE);
		}

		case SERVICE_QUEST_REWARD_RANDART:
		{
			int rand_power;

			/* Paranoia - should never happen */
			if ((adult_no_artifacts) || (adult_no_xtra_artifacts))
			{
				msg_print("Nothing happens.");
				return (FALSE);
			}

			/* Only accept legal items */
			item_tester_hook = item_tester_hook_randart;

			/* Get an item */
			q = format("Choose an item to be made into an artifact, %s. ", title);
			s = format("You have no eligible item, %s. ", title);
			if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

			/*Got the item*/
			o_ptr = &inventory[item];

			/*Got the object kind*/
			k_ptr = &k_info[o_ptr->k_idx];

			/* Description */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

			strnfmt(prompt, sizeof(prompt), "Make %s into an artifact? ", o_name);

			if (!get_check(prompt)) return (FALSE);

			/* extra power bonus for expensive items and high player fame*/
			rand_power = (p_ptr->q_fame + p_ptr->deferred_rewards) / 20 + MAX((k_ptr->cost / 2000), p_ptr->q_fame / 50);

			/*Hack - add in any to-hit and to-value, since they will be erased*/
			rand_power += (o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) / 2;

			/*actually create the Randart, or handle failure*/
			if (make_one_randart(o_ptr, rand_power, TRUE))
			{
				/* Identify it fully */
				object_aware(o_ptr);
				object_known(o_ptr);

				/* Mark the history */
				o_ptr->origin_nature = ORIGIN_REWARD;
				o_ptr->origin_r_idx = 0;
				o_ptr->origin_dlvl = q_ptr->base_level;

				/* Mark the item as fully known */
				o_ptr->ident |= (IDENT_MENTAL);

				/*Let the player know what they just got*/
				object_info_screen(o_ptr);

				guild_quest_wipe(TRUE);

				return (TRUE);
			}
			msg_print("The attempt at making an artifact has failed");
			return (FALSE);
		}

		case SERVICE_PROBE_QUEST_MON:
		{
			char race_name[80];

			monster_race *r_ptr = &r_info[q_ptr->mon_idx];
			monster_lore *l_ptr = &l_list[q_ptr->mon_idx];

			if ((!quest_single_r_idx(q_ptr)) || (q_ptr->mon_idx == 0))
			{
				msg_print("You are not currently questing for a specific creature.");
				return (FALSE);
			}

			/* Not a vault quest, so get the monster race name (singular)*/
			monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

			/* Make it plural if necessary*/
			if (q_ptr->q_max_num > 1) plural_aux(race_name, sizeof(race_name));

			price += r_ptr->level * 100;

			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			/*confirm*/
			strnfmt(prompt, sizeof(prompt), "Spend %d gold to probe %s? ",
							price, race_name);
			if (!get_check(prompt)) return (FALSE);

			/*charge the player*/
			p_ptr->au -= price;

			/*learn something about the monster*/
			lore_probe_monster_aux(q_ptr->mon_idx);

			/* Hack -- Increse the sightings, and ranged attacks around 50% of the time */
			l_ptr->sights = MAX_SHORT;
			l_ptr->ranged = MAX_UCHAR;

			/* Know "race" flags */
			l_ptr->r_l_flags3 |= (r_ptr->flags3 & RF3_RACE_MASK);
			/* Know "forced" flags */
			l_ptr->r_l_flags1 |= (r_ptr->flags1 & (RF1_FORCE_DEPTH | RF1_FORCE_MAXHP));

			/* Save screen */
			screen_save();

			/* Begin recall */
			Term_gotoxy(0, 1);

			/* Output to the screen */
			text_out_hook = text_out_to_screen;

			/* Recall monster */
			describe_monster(q_ptr->mon_idx, FALSE);

			/* Describe monster */
			roff_top(q_ptr->mon_idx);

			/*give the player a look at the updated monster info*/
			put_str("Press any key to continue.  ", 0, 40);

			inkey();

			/* Load screen */
			screen_load();
			return (TRUE);
		}
		case SERVICE_BUY_HEALING_POTION:
		case SERVICE_BUY_LIFE_POTION:
		case SERVICE_BUY_SCROLL_BANISHMENT:
		{
			char o_name[80];
			int k_idx;

			object_type *i_ptr;
			object_type object_type_body;

			i_ptr = &object_type_body;

			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			/*get the healing potion index*/
			if (choice == SERVICE_BUY_HEALING_POTION)
			{
				k_idx = lookup_kind(TV_POTION, SV_POTION_HEALING);
			}
			/*get the potion of life index*/
			else if (choice == SERVICE_BUY_LIFE_POTION)
			{
				k_idx = lookup_kind(TV_POTION, SV_POTION_LIFE);
			}

			/* SERVICE_BUY_SCROLL_BANISHMENT */
			else k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_MASS_BANISHMENT);

			/*get the book kind*/
			k_ptr = &k_info[k_idx];

			/*Too expensive*/
			if (!check_gold(price)) return (FALSE);

			strnfmt(prompt, sizeof(prompt), "Spend %d gold to purchase a potion of %s? ",
							price, (k_name + k_ptr->name));
			if (!get_check(prompt)) return (FALSE);

			/*charge the player*/
			p_ptr->au -= price;

			/* Make the potion */
			object_prep(i_ptr, k_idx);

			/* Identify it */
			k_info[k_idx].aware = TRUE;

			/* Describe the result */
			object_desc(o_name, sizeof(o_name), i_ptr, ODESC_FULL);

			/* Remember history */
			object_history(i_ptr, ORIGIN_STORE, 0);

			/* Note that the pack is too full */
			if (!inven_carry_okay(i_ptr))
			{
				msg_format("You have no room in your backpack.");

				/* Drop the object */
				drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

				/* Inform the player */
				msg_format("Your %s is waiting outside!", o_name);

			}

			/* Give it to the player */
			else
			{
				int item_new;

				/* Give it to the player */
				item_new = inven_carry(i_ptr);

				/* Describe just the result */
				object_desc(o_name, sizeof(o_name), &inventory[item_new], ODESC_PREFIX | ODESC_FULL);

				/* Message */
				msg_format("You have (%c) %s.", index_to_label(item_new), o_name);
			}

			return (TRUE);
		}
		case SERVICE_ABANDON_QUEST:
		{
			/* Check for current quest */
			if (!guild_quest_level())
			{
		    	msg_format("You don't have a current quest, %s.", title);
		    	return (FALSE);
			}

			/* Ask confirmation */
			if (!get_check(format("Abandon your quest, %s?", title))) return (FALSE);

			/* Remove the current quest */
			quest_fail();

			/*Get the new title, and give a message*/
			get_title(title, sizeof(title));
			msg_print(format("The guild is disappointed in you, %s.", title));
			message_flush();

			return (TRUE);
		}
		case SERVICE_FIREPROOF_BOOK:
		{

		  	int i;

		  	/*Too expensive*/
		  	if (!check_gold(price)) return (FALSE);

			/* Restrict choices to spell books */
			item_tester_tval = cp_ptr->spell_book;

			/* Only accept legal items, which are burnable books */
			item_tester_hook = item_tester_hook_flammable_book;

			/* Get an item */
			q = "Fireproof which book? ";
			if (cp_ptr->spell_book == TV_PRAYER_BOOK) s = "You have no flammable prayer books!";
			else s = "You have no flammable spell books!";
			if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

		  	/*Got the item*/
			o_ptr = &inventory[item];
			k_ptr = &k_info[o_ptr->k_idx];

			/*Adjust the price for the book and the number of books*/
		  	price += (k_ptr->cost * 6);
			price *= o_ptr->number;

		  	/*Too expensive*/
		  	if (!check_gold(price)) return (FALSE);

			/* Description */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

		  	/*confirm*/
		  	strnfmt(prompt, sizeof(prompt),
		      	"Spend %d gold to fireproof %s? ", 	price, o_name);
		  	if (!get_check(prompt)) return (FALSE);

			/*find the ego-item*/
			for (i = 0; i < z_info->e_max; i++)
			{
				ego_item_type *e_ptr = &e_info[i];

				if (strstr((e_name + e_ptr->name), "Fireproof"))
				{
					int j;
					bool right_type = FALSE;

					/*Confirm the right tval*/
					for (j = 0; j < EGO_TVALS_MAX; j++)
					{
						if (e_ptr->tval[j] == cp_ptr->spell_book) right_type = TRUE;
					}

					/*We found it*/
					if (right_type)
					{
						/*charge the player*/
		  				p_ptr->au -= price;

						o_ptr->ego_num = i;

						/* Description */
						object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

						/*Confirm it worked*/
						msg_format("You have %s", o_name);

						return (TRUE);
					}
				}
			}

 		  	return (FALSE);
 		}
		case SERVICE_QUEST_DEFER_REWARD:
		{
			/* Check for current quest */
			if (!guild_quest_level())
			{
			   	msg_format("You don't have a current quest, %s.", title);
			   	return (FALSE);
			}

			/* Ask confirmation */
			if (!get_check(format("Really defer your reward, %s?", title))) return (FALSE);

			p_ptr->deferred_rewards += (q_ptr->q_fame_inc * 3) / 2;

			guild_quest_wipe(FALSE);

			return (TRUE);

		}
		case SERVICE_QUEST_REWARD_INC_HP:
		{
			/* Check for current quest */
			if (!guild_quest_level())
			{
			   	msg_format("You don't have a current quest, %s.", title);
			   	return (FALSE);
			}

			/* Ask confirmation */
			if (!get_check(format("Do you wish to permanently increase your hit points, %s?", title))) return (FALSE);

			grant_reward_hp();

			/* Inform the player */
			msg_print(format("You now have an increased vitality, %s!", title));

			guild_quest_wipe(TRUE);

			return (TRUE);
		}
		case SERVICE_QUEST_REWARD_AUGMENTATION:
		{
			int i;

			/* Check for current quest */
			if (!guild_quest_level())
			{
			   	msg_format("You don't have a current quest, %s.", title);
			   	return (FALSE);
			}

			/* Ask confirmation */
			if (!get_check(format("Do you wish to permanently increase your stats, %s?", title))) return (FALSE);

			/* Boost all six stats */
			for (i = 0; i < A_MAX; i++) do_perm_stat_boost(i);

			/* The quest is over */
			guild_quest_wipe(TRUE);
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Buy a service from a store
 */
static bool service_purchase(int this_store, int choice)
{

	/*player chose excape*/
	if ((choice == -1) || (choice >= STORE_SERVICE_MAX)) return(FALSE);

	/*give the player the service*/
	if (!store_service_aux(this_store, choice))
	{
		return (FALSE);
	}

	store_updates();

	/* Done */
	return (TRUE);
}



/*
 * Determine the price of an object (qty one) in a store.
 *  store_buying == TRUE  means the shop is buying, player selling
 *               == FALSE means the shop is selling, player buying
 *
 * This function takes into account the player's charisma, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * object, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- the black market always charges twice as much as it should.
 *
 */
s32b price_item(const object_type *o_ptr, bool store_buying)
{
	int adjust;
	int this_store = current_store();
	s32b price;
	owner_type *ot_ptr;

	if (this_store == STORE_NONE) return 0L;

	ot_ptr = store_owner(this_store);

	/* Get the value of one of the items */
	price = object_value(o_ptr);

	/* Worthless items */
	if (price <= 0) return (0L);

	/* Add in the charisma factor */
	if (this_store == STORE_B_MARKET) adjust = 175;
	else if (game_mode == GAME_NPPMORIA)
	{
		adjust = moria_chr_adj();
	}
	else adjust = adj_chr_gold[p_ptr->state.stat_ind[A_CHR]];


	/* Shop is buying */
	if (store_buying)
	{
		/* Check for no_selling option */
		if (adult_no_selling) return (0L);

		/* Set the factor */
		adjust = 185 - adjust;

		/* Boundry control */
		if (adjust > 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (this_store == STORE_B_MARKET) price = price / 2;
	}

	/* Shop is selling */
	else
	{
		adjust = 25 + adjust;

		/* Boundry control */
		if (adjust < 100) adjust = 100;

		/* Mega-Hack -- Black market sucks */
		if (this_store == STORE_B_MARKET) price = price * 2;
	}

	/* Compute the final price (with rounding) */
	price = (price * adjust + 50L) / 100L;

	/* Now limit the price to the purse limit */
	if (store_buying && (price > ot_ptr->max_cost))
		price = ot_ptr->max_cost;

	/* Note -- Never become "free" */
	if (price <= 0L) return (1L);

	/* Return the price */
	return (price);
}



/*
 * Certain "cheap" objects should be created in "piles".
 *
 * Some objects can be sold at a "discount" (in smaller piles).
 *
 * Standard percentage discounts include 10, 25, 50, 75, and 90.
 */
static void mass_produce(object_type *o_ptr, int store)
{
	int size = 1;

	int discount = 0;

	s32b cost = object_value(o_ptr);

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Food, Flasks, and Lites */
		case TV_FOOD:
		case TV_FLASK:
		case TV_LIGHT:
		{
			if (cost <= 5L) size += damroll(2, 5);
			if (cost <= 20L) size += damroll(2, 5);
			break;
		}

		case TV_POTION:
		case TV_SCROLL:
		{
			if (cost <= 60L) size += damroll(3, 5);
			if (cost <= 240L) size += damroll(1, 5);
			break;
		}

		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_DRUID_BOOK:
		{
			if (cost <= 50L) size += damroll(2, 3);
			if (cost <= 500L) size += damroll(1, 3);
			break;
		}

		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SHIELD:
		case TV_GLOVES:
		case TV_BOOTS:
		case TV_CLOAK:
		case TV_HELM:
		case TV_CROWN:
		case TV_SWORD:
		case TV_POLEARM:
		case TV_HAFTED:
		case TV_DIGGING:
		case TV_BOW:
		{
			if (o_ptr->ego_num) break;
			if (cost <= 10L) size += damroll(2, 2);
			if (cost <= 100L) size += damroll(2, 2);
			break;
		}
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_SPIKE:
		{
			if (o_ptr->ego_num) size += damroll(3, 5);
			else if ((o_ptr->to_h > 0) || (o_ptr->to_d > 0)) size += damroll(4, 6);
			else size += damroll(8, 7);
			break;
		}

	}

	/* Pick a discount */
	if (cost < 5L)
	{
		discount = 0;
	}
	else if ((one_in_(25)) && (cost > 10L))
	{
		discount = 10;
	}
	else if (one_in_(50))
	{
		discount = 25;
	}
	else if (one_in_(150))
	{
		discount = 50;
	}
	else if (one_in_(300))
	{
		discount = 75;
	}
	else if (one_in_(500))
	{
		discount = 90;
	}

	/* Save the discount */
	o_ptr->discount = discount;

	/* Save the total pile size */
	o_ptr->number = MAX(size - (size * discount / 100), 1);

	/* Hack -- rods need to increase PVAL if stacked */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval = o_ptr->number * k_info[o_ptr->k_idx].pval;
	}
}



/*
 * Determine if a store object can "absorb" another object.
 *
 * See "object_similar()" for the same function for the "player".
 *
 * This function can ignore many of the checks done for the player,
 * since stores (but not the home) only get objects under certain
 * restricted circumstances.
 */
static bool store_object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
	u32b f1, f2, f3, fn;
	u32b j1, j2, j3, jn;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	object_flags(j_ptr, &j1, &j2, &j3, &jn);

	/* Hack -- Identical items cannot be stacked */
	if (o_ptr == j_ptr) return (0);

	/* Different objects cannot be stacked */
	if (o_ptr->k_idx != j_ptr->k_idx) return (0);

	/* Different charges (etc) cannot be stacked, except for staves, wands and rods. */
	if ((o_ptr->pval != j_ptr->pval) &&
		(o_ptr->tval != TV_WAND) &&
		(o_ptr->tval != TV_ROD) &&
		(o_ptr->tval != TV_STAFF)) return (0);

	/* Require many identical values */
	if (o_ptr->to_h != j_ptr->to_h) return (0);
	if (o_ptr->to_d != j_ptr->to_d) return (0);
	if (o_ptr->to_a != j_ptr->to_a) return (0);

	/* Require identical "artifact" names */
	if (o_ptr->art_num != j_ptr->art_num) return (0);

	/* Require identical "ego-item" names */
	if (o_ptr->ego_num != j_ptr->ego_num) return (0);

	/* Hack -- Never stack "powerful" items */
	if (o_ptr->xtra1 || j_ptr->xtra1) return (0);

	/* Mega-Hack -- Handle lites */
	if (fuelable_light_p(o_ptr))
	{
		if (o_ptr->timeout != j_ptr->timeout) return 0;
	}

	/* Hack -- Never stack recharging items */
	else if (o_ptr->timeout || j_ptr->timeout) return (0);

	/* Require many identical values */
	if (o_ptr->ac != j_ptr->ac) return (0);
	if (o_ptr->dd != j_ptr->dd) return (0);
	if (o_ptr->ds != j_ptr->ds) return (0);

	/* Hack -- Never stack chests */
	if (o_ptr->tval == TV_CHEST) return (0);

	/* Require matching "discount" fields */
	if (o_ptr->discount != j_ptr->discount) return (0);

	/*Allow well balanced items to stack only with other
			 *well balanced items*/
	if ((o_ptr->ident & IDENT_PERFECT_BALANCE) !=
        (o_ptr->ident & IDENT_PERFECT_BALANCE)) return (FALSE);

	/* Different flags */
	if ((f1 != j1) || (f2 != j2) || \
		(f3 != j3) || (fn != jn)) return(FALSE);

	/* They match, so they must be similar */
	return (TRUE);
}


/*
 * Allow a store object to absorb another object
 */
static void store_object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Combine quantity, lose excess items */
	o_ptr->number = (total > STORE_MAX_ITEM) ? STORE_MAX_ITEM : total;

	/*
	 *Hack -- if rods are stacking, add the pvals (maximum timeouts)
	 * and any charging timeouts together.
	 */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval += j_ptr->pval;
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Hack -- if wands/staves are stacking, combine the charges. */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		o_ptr->pval += j_ptr->pval;
	}

	/* Combine the histories */
	stack_histories(o_ptr, j_ptr);
}


/*
 * Check to see if the shop will be carrying too many objects
 *
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" objects this way, by
 * adding them to a pile which was already full.
 */
static bool store_check_num(int st, const object_type *o_ptr)
{
	int i;
	object_type *j_ptr;

	store_type *st_ptr = &store[st];

	/* Free space is always usable */
	if (st_ptr->stock_num < st_ptr->stock_size) return TRUE;

	/* The "home" acts like the player */
	if (st == STORE_HOME)
	{
		/* Check all the objects */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Get the existing object */
			j_ptr = &st_ptr->stock[i];

			/* Can the new object be combined with the old one? */
			if (object_similar(j_ptr, o_ptr)) return (TRUE);
		}
	}

	/* Normal stores do special stuff */
	else
	{
		/* Check all the objects */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Get the existing object */
			j_ptr = &st_ptr->stock[i];

			/* Can the new object be combined with the old one? */
			if (store_object_similar(j_ptr, o_ptr)) return (TRUE);
		}
	}

	/* But there was no room at the inn... */
	return (FALSE);
}


/*
 * Determine if the current store will purchase the given object
 *
 * Note that a shop-keeper must refuse to buy "worthless" objects
 */
static bool store_will_buy(int store_num, const object_type *o_ptr)
{
	/* Hack -- The Home and guild are simple */
	if (store_num == STORE_HOME) return (TRUE);
	if (store_num == STORE_GUILD) return (FALSE);

	/* Some results are slightly different for Moria */
	if (game_mode == GAME_NPPMORIA)
	{
		if ((store_num == STORE_TEMPLE) && (o_ptr->tval == TV_PRAYER_BOOK)) return (TRUE);
		if ((store_num == STORE_MAGIC) &&  (o_ptr->tval == TV_MAGIC_BOOK)) 	return (TRUE);
	}

	/* Switch on the store */
	switch (store_num)
	{
		/* General Store */
		case STORE_GENERAL:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_FOOD:
				case TV_LIGHT:
				case TV_FLASK:
				case TV_SPIKE:
				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				case TV_DIGGING:
				case TV_CLOAK:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Armoury */
		case STORE_ARMOR:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_BOOTS:
				case TV_GLOVES:
				case TV_CROWN:
				case TV_HELM:
				case TV_SHIELD:
				case TV_CLOAK:
				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
				case TV_DRAG_ARMOR:
				case TV_DRAG_SHIELD:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Weapon Shop */
		case STORE_WEAPON:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_SHOT:
				case TV_BOLT:
				case TV_ARROW:
				case TV_BOW:
				case TV_DIGGING:
				case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Temple */
		case STORE_TEMPLE:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{

				case TV_SCROLL:
				case TV_POTION:
				case TV_HAFTED:
			break;
				case TV_POLEARM:
				case TV_SWORD:
				{
					/* Known blessed blades are accepted too */
					if (is_blessed(o_ptr) && object_known_p(o_ptr)) break;
				}
				default:
				return (FALSE);
			}
			break;
		}

		/* Alchemist */
		case STORE_ALCHEMY:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{

				case TV_SCROLL:
				case TV_POTION:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		/* Magic Shop */
		case STORE_MAGIC:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_AMULET:
				case TV_RING:
				case TV_STAFF:
				case TV_WAND:
				case TV_ROD:
				case TV_SCROLL:
				case TV_POTION:
				break;
				default:
				return (FALSE);
			}
			break;
		}

		case STORE_BOOKSHOP:
		{
			/* Analyze the type */
			switch (o_ptr->tval)
			{
				case TV_MAGIC_BOOK:
				case TV_DRUID_BOOK:
				case TV_PRAYER_BOOK:
				break;
				default:
				return (FALSE);
			}
		}
	}

	/* Ignore "worthless" items XXX XXX XXX */
	if (object_value(o_ptr) <= 0) return (FALSE);

	/* Assume okay */
	return (TRUE);
}



/*
 * Add an object to the inventory of the Home.
 *
 * In all cases, return the slot (or -1) where the object was placed.
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" becoming
 * known: the player may have to pick stuff up and drop it again.
 */
static int home_carry(object_type *o_ptr)
{
	int i, slot;
	u32b value, j_value;
	object_type *j_ptr;

	store_type *st_ptr = &store[STORE_HOME];

	/* Check each existing object (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing object */
		j_ptr = &st_ptr->stock[slot];

		if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

		/* The home acts just like the player */
		if (object_similar(j_ptr, o_ptr))
		{
			/* Save the new number of items */
			object_absorb(j_ptr, o_ptr);

			/* All done */
			return (slot);
		}
	}

	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


	/* Determine the "value" of the object */
	value = object_value(o_ptr);

	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that object */
		j_ptr = &st_ptr->stock[slot];

		/* Hack -- readable books always come first */
		if ((o_ptr->tval == cp_ptr->spell_book) &&
		    (j_ptr->tval != cp_ptr->spell_book)) break;
		if ((j_ptr->tval == cp_ptr->spell_book) &&
		    (o_ptr->tval != cp_ptr->spell_book)) continue;

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Can happen in the home */
		if (!object_flavor_is_aware(o_ptr)) continue;
		if (!object_flavor_is_aware(j_ptr)) break;

		/* Objects sort by increasing sval */
		if (o_ptr->sval < j_ptr->sval) break;
		if (o_ptr->sval > j_ptr->sval) continue;

		/* Objects in the home can be unknown */
		if (!object_is_known(o_ptr)) continue;
		if (!object_is_known(j_ptr)) break;

		/* Objects sort by decreasing value */
		j_value = object_value(j_ptr);
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		/* Hack -- slide the objects */
		object_copy(&st_ptr->stock[i], &st_ptr->stock[i-1]);
	}

	/* More stuff now */
	st_ptr->stock_num++;

	/* Hack -- Insert the new object */
	object_copy(&st_ptr->stock[slot], o_ptr);

	/* Return the location */
	return (slot);
}


/*
 * Add an object to a real stores inventory.
 *
 * If the object is "worthless", it is thrown away (except in the home).
 *
 * If the object cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.  Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an object to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static int store_carry(int st, object_type *o_ptr)
{
	int i, slot;
	u32b value, j_value;
	object_type *j_ptr;

	store_type *st_ptr = &store[st];

	/* Evaluate the object */
	value = object_value(o_ptr);

	/* Cursed/Worthless items "disappear" when sold */
	if (value <= 0) return (-1);

	/* Erase the inscription & pseudo-ID bit */
	o_ptr->obj_note = 0;

	/* Some item types require maintenance */
	switch (o_ptr->tval)
	{
		/* Refuel lights to the standard amount */
		case TV_LIGHT:
		{
			if (o_ptr->sval == SV_LIGHT_TORCH)
				o_ptr->timeout = DEFAULT_TORCH;

			else if (o_ptr->sval == SV_LIGHT_LANTERN)
					o_ptr->timeout = DEFAULT_LAMP;

			break;
		}

		/* Recharge rods */
		case TV_ROD:
		{
			o_ptr->timeout = 0;
			break;
		}

		/* recharge wands and staves */
		case TV_STAFF:
		case TV_WAND:
		{
			int charges = o_ptr->pval;

			o_ptr->pval = 0;

			/* Calculate the recharged number of charges */
			for (i = 0; i < o_ptr->number; i++) recharge_staff_wand(o_ptr, 60);

			/* Use recharged value only if greater */
			if (charges > o_ptr->pval) o_ptr->pval = charges;
		}
	}


	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;


	/* Check each existing object (try to combine) */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get the existing object */
		j_ptr = &st_ptr->stock[slot];

		/* Can the existing items be incremented? */
		if (store_object_similar(j_ptr, o_ptr))
		{
			/* Absorb (some of) the object */
			store_object_absorb(j_ptr, o_ptr);

			/* All done */
			return (slot);
		}
	}

	/* No space? */
	if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


	/* Check existing slots to see if we must "slide" */
	for (slot = 0; slot < st_ptr->stock_num; slot++)
	{
		/* Get that object */
		j_ptr = &st_ptr->stock[slot];

		/* Hack -- readable books always come first */
		if ((o_ptr->tval == cp_ptr->spell_book) &&
		    (j_ptr->tval != cp_ptr->spell_book)) break;
		if ((j_ptr->tval == cp_ptr->spell_book) &&
		    (o_ptr->tval != cp_ptr->spell_book)) continue;

		/* Objects sort by decreasing type */
		if (o_ptr->tval > j_ptr->tval) break;
		if (o_ptr->tval < j_ptr->tval) continue;

		/* Objects sort by increasing sval */
		if (o_ptr->sval < j_ptr->sval) break;
		if (o_ptr->sval > j_ptr->sval) continue;

		/* Evaluate that slot */
		j_value = object_value(j_ptr);

		/* Objects sort by decreasing value */
		if (value > j_value) break;
		if (value < j_value) continue;
	}

	/* Slide the others up */
	for (i = st_ptr->stock_num; i > slot; i--)
	{
		/* Hack -- slide the objects */
		object_copy(&st_ptr->stock[i], &st_ptr->stock[i-1]);
	}

	/* More stuff now */
	st_ptr->stock_num++;

	/* Hack -- Insert the new object */
	object_copy(&st_ptr->stock[slot], o_ptr);

	/* Return the location */
	return (slot);
}

/*
 * Increase, by a 'num', the number of an item 'item' in store 'st'.
 * This can result in zero items.
 */
void store_item_increase(int st, int item, int num)
{
	int cnt;
	object_type *o_ptr;

	store_type *st_ptr = &store[st];

	/* Get the object */
	o_ptr = &st_ptr->stock[item];

	/* Verify the number */
	cnt = o_ptr->number + num;
	if (cnt > STORE_MAX_ITEM) cnt = STORE_MAX_ITEM;
	else if (cnt < 0) cnt = 0;
	num = cnt - o_ptr->number;

	/* Save the new number */
	o_ptr->number += num;

	/* Hack - don't let the store be bought out of items that are always in stock run out */
	if (keep_in_stock(o_ptr, st))
	{
		if (st != STORE_HOME) o_ptr->number = STORE_MAX_ITEM;
	}
}


/*
 * Remove a slot if it is empty, in store 'st'.
 */
void store_item_optimize(int st, int item)
{
	int j;
	object_type *o_ptr;

	store_type *st_ptr = &store[st];

	/* Get the object */
	o_ptr = &st_ptr->stock[item];

	/* Must exist */
	if (!o_ptr->k_idx) return;

	/* Must have no items */
	if (o_ptr->number) return;

	/* One less object */
	st_ptr->stock_num--;

	/* Slide everyone */
	for (j = item; j < st_ptr->stock_num; j++)
	{
		st_ptr->stock[j] = st_ptr->stock[j + 1];
	}

	/* Nuke the final slot */
	object_wipe(&st_ptr->stock[j]);
}


/*
 * This makes sure that the black market doesn't stock any object that other
 * stores have, unless it is an ego-item or has various bonuses.
 *
 * Based on a suggestion by Lee Vogt <lvogt@cig.mcel.mot.com>.
 */
static bool black_market_ok(const object_type *o_ptr)
{
	int i, j;

	/* Ego items are always fine */
	if (ego_item_p(o_ptr)) return (TRUE);

	/* Good items are normally fine */
	if (o_ptr->to_a > 2) return (TRUE);
	if (o_ptr->to_h > 1) return (TRUE);
	if (o_ptr->to_d > 2) return (TRUE);


	/* No cheap items */
	if (object_value(o_ptr) < 10) return (FALSE);

	/* Check the other stores */
	for (i = 0; i < MAX_STORES; i++)
	{
		/* Skip home, the guild, and the black market */
		if (i == STORE_B_MARKET || i == STORE_HOME || i == STORE_GUILD)
			continue;

		/* Check every object in the store */
		for (j = 0; j < store[i].stock_num; j++)
		{
			object_type *j_ptr = &store[i].stock[j];

			/* Compare object kinds */
			if (o_ptr->k_idx == j_ptr->k_idx)
				return (FALSE);
		}
	}

	/* Otherwise fine */
	return (TRUE);
}

/*
 * Keep certain objects (undiscounted only).
 *
 * Note if this list is greatly expanded, the store_maint function
 * could get caught in an eternal loop.  Be mindful of the fixed
 * variable STORE_MAX_KEEP and STORE_MIN_KEEP when making this list.
 */
bool keep_in_stock(const object_type *o_ptr, int which)
{

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	if (game_mode == GAME_NPPMORIA) return (FALSE);

	/*Discounted items, ego items, or artifacts don't stay in stock*/
	if (o_ptr->discount) return (FALSE);
	if (o_ptr->art_num) return (FALSE);
	if (o_ptr->ego_num) return (FALSE);

	/* Never in the home */
	if (which == STORE_HOME) return (FALSE);

	/* Analyze the item type */
	switch (k_ptr->tval)
	{
		/* Certain kinds of food is sold there*/
		case TV_FOOD:
		{
			/*only keep in the general store*/
			if (which != STORE_GENERAL) return (FALSE);
			if (k_ptr->sval == SV_FOOD_RATION) return (TRUE);
			return (FALSE);
		}

		/* Non artifact Lite Sources should be kept */
		case TV_LIGHT:
		{
			/*only keep in the general store*/
			if (which != STORE_GENERAL) return (FALSE);
			if (k_ptr->sval == SV_LIGHT_TORCH &&
			    o_ptr->timeout > 0) return (TRUE);
			return (FALSE);
		}
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/*only keep in the general store or weaponsmith*/
			if ((which != STORE_GENERAL) && (which != STORE_WEAPON)) return (FALSE);
			if (k_ptr->sval == SV_AMMO_NORMAL)
			{
				/*only normal ammo that isn't enchanted*/
				if ((o_ptr->to_h > 0) || (o_ptr->to_d > 0)) return FALSE;
				return (TRUE);
			}
			return (FALSE);
		}
		case TV_POTION:
		{
			/*only keep in the temple*/
			if (which != STORE_TEMPLE) return (FALSE);
			if (k_ptr->sval == SV_POTION_CURE_CRITICAL) return (TRUE);
			if (k_ptr->sval == SV_POTION_RESTORE_EXP) return (TRUE);
			return (FALSE);
		}
		case TV_SCROLL:
		{
			/*only keep in the alchemy shop*/
			if (which != STORE_ALCHEMY) return (FALSE);
			if (k_ptr->sval == SV_SCROLL_PHASE_DOOR) return (TRUE);
			if (k_ptr->sval == SV_SCROLL_SATISFY_HUNGER) return (TRUE);
			if (k_ptr->sval == SV_SCROLL_IDENTIFY) return (TRUE);
			if (k_ptr->sval == SV_SCROLL_WORD_OF_RECALL) return (TRUE);
			return (FALSE);

		}
		/* Flasks should be kept */
		case TV_FLASK:
		{
			if (which != STORE_GENERAL) return (FALSE);
			return (TRUE);
		}
		case TV_PRAYER_BOOK:
		case TV_MAGIC_BOOK:
		case TV_DRUID_BOOK:
		{
			if (which != STORE_BOOKSHOP) return (FALSE);
			if (k_ptr->sval < SV_BOOK_MIN_GOOD) return (TRUE);
			return (FALSE);
		}

	}
	return (FALSE);
}

/*
 * Return true if all items in store are standard stock items.
 * False if standard items.
 * Used to determine if the store should shuffle inventory or
 * if the shopkeeper should retire.
 */
static int count_nonstandard_inven(int which)
{
	store_type *st_ptr = &store[which];
	int i;
	int counter = 0;

	/* Discount all the items */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &st_ptr->stock[i];

		/* Don't count essential stock items*/
		if (keep_in_stock(o_ptr, which)) continue;

		/* We have some non-standard inventory */
		counter++;
	}

	return (counter);
}


/*
 * Delete an object from store 'st', or, if it is a stack, perhaps only
 * partially delete it.
 */
void store_delete_index(int st, int what)
{
	int num;
	object_type *o_ptr;

	store_type *st_ptr = &store[st];

	/* Paranoia */
	if (st_ptr->stock_num <= 0) return;

	/* keep certain items */

	/* Get the object */
	o_ptr = &st_ptr->stock[what];

	/* Determine how many objects are in the slot */
	num = o_ptr->number;

	/* Some stores keep large amounts of certain objects in stock objects*/
	if ((st != STORE_B_MARKET) && (keep_in_stock(o_ptr, st)))
	{
		if (o_ptr->number > 60) num = num / 2;
		return;
	}

	/* Deal with stacks */
	if (num > 1)
	{
		/* Special behaviour for arrows, bolts &tc. */
		switch (o_ptr->tval)
		{
			case TV_SPIKE:
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			{
				/* 50% of the time, destroy the entire stack */
				if (randint0(100) < 50 || num < 10)
					num = o_ptr->number;

				/* 50% of the time, reduce the size to a multiple of 5 */
				else
					num = randint1(num / 5) * 5 + (num % 5);

				break;
			}

			default:
			{
				/* 50% of the time, destroy a single object */
				if (randint0(100) < 50) num = 1;

				/* 25% of the time, destroy half the objects */
				else if (randint0(100) < 50) num = (num + 1) / 2;

				/* 25% of the time, destroy all objects */
				else num = o_ptr->number;

				/* Hack -- decrement the total charges of staves and wands. */
				if (o_ptr->tval == TV_STAFF || o_ptr->tval == TV_WAND)
				{
					o_ptr->pval -= num * o_ptr->pval / o_ptr->number;
				}
			}
		}

	}

	/*Wipe the randart if necessary*/
	if (o_ptr->art_num) artifact_wipe(o_ptr->art_num, FALSE);

	/* Delete the item */
	store_item_increase(st, what, -num);
	store_item_optimize(st, what);
}


/*
 * Delete a random object from store 'st', or, if it is a stack, perhaps only
 * partially delete it.
 *
 * This function is used when store maintenance occurs, and is designed to
 * imitate non-PC purchasers making purchases from the store.
 */
static void store_delete_random(int st)
{
	int what;
	store_type *st_ptr = &store[st];
	object_type *o_ptr;

	/* Paranoia */
	if (st_ptr->stock_num <= 0) return;

	/* Pick a random slot */
	what = randint0(st_ptr->stock_num);

	/* Get the object */
	o_ptr = &st_ptr->stock[what];

	if (keep_in_stock(o_ptr, st)) return;

	store_delete_index(st, what);
}

/*
 * Creates a random object and gives it to a store
 * This algorithm needs to be rethought.  A lot.
 *
 */
static void store_create_random(int which)
{
	int tries, k_idx;

	object_type *i_ptr;
	object_type object_type_body;

	/* Activate that store */
	store_type *st_ptr = &store[which];

	/* Paranoia -- no room left */
	if (st_ptr->stock_num >= st_ptr->stock_size) return;

	/* Hack -- consider up to ten items */
	for (tries = 0; tries < 10; tries++)
	{

		s16b magic_level;

		/* Pick a level for object/creation */
		magic_level = 15 + (p_ptr->lev / 2) + rand_int(p_ptr->lev);

		/* Get local object */
		i_ptr = &object_type_body;

		/*wipe the object*/
		object_wipe(i_ptr);

		/*
		 * Get the object level.  The object level of 100 is a hack
		 * to ensure that all items that are part of the regular store
		 * inventory are not out of depth.
		 */
		if ((which == STORE_B_MARKET) || (allow_altered_inventory))
		{
		 	object_level = magic_level;
		}
		else object_level = 100;

		/* Pick a random object */
		k_idx = get_obj_num(object_level);

		/* Handle failure - but this should never happen*/
		if (!k_idx) continue;

		/* Prepare the object */
		object_prep(i_ptr, k_idx);

		/* Apply magic (dis-allow artifacts) */
		apply_magic(i_ptr, magic_level, FALSE, FALSE, FALSE, FALSE);

		/* Hack -- Charge lite's */
		if (i_ptr->tval == TV_LIGHT)
		{
			if (i_ptr->sval == SV_LIGHT_TORCH) i_ptr->timeout = FUEL_TORCH / 2;
			if (i_ptr->sval == SV_LIGHT_LANTERN) i_ptr->timeout = FUEL_LAMP / 2;
		}

		/* The object is "fully known" */
		object_known(i_ptr);
		i_ptr->ident |= (IDENT_MENTAL);

		/* Item belongs to a store */
		i_ptr->ident |= IDENT_STORE;

		/* Remember history */
		object_history(i_ptr, ORIGIN_STORE, 0);

		/* Black markets have expensive tastes */
		if ((which == STORE_B_MARKET) && !black_market_ok(i_ptr))
			continue;

		/* No "worthless" items */
		if (object_value(i_ptr) <= 0) continue;


		/* Mass produce and/or Apply discount */
		mass_produce(i_ptr, which);

		/* Attempt to carry the (known) object */
		(void)store_carry(which, i_ptr);

		/* Definitely done */
		break;
	}

	/* Reset the object level */
	object_level = p_ptr->depth;

}



/*
 * Maintain the inventory at the stores.
 */
void store_maint(int which)
{
	int j;
	int alt_min = 0;

	int old_rating = rating;

	store_type *st_ptr;

	/* Ignore home and guild */
	if ((which == STORE_HOME) || (which == STORE_GUILD)) return;

	/* Activate that store */
	st_ptr = &store[which];

	/* Activate the owner */
	ot_ptr = &b_info[(which * z_info->b_max) + st_ptr->owner];

	/* XXX Prune the black market */
	if (which == STORE_B_MARKET)
	{
		/* Destroy crappy black market items */
		for (j = st_ptr->stock_num - 1; j >= 0; j--)
		{
			object_type *o_ptr = &st_ptr->stock[j];

			/* Destroy crappy items */
			if (!black_market_ok(o_ptr))
			{
				/*Wipe a randart if necessary*/
				if (o_ptr->art_num) artifact_wipe(o_ptr->art_num, FALSE);

				/* Destroy the object */
				store_item_increase(which, j, 0 - o_ptr->number);
				store_item_optimize(which, j);
			}
		}
	}

	/* Choose the number of slots to keep */
	j = st_ptr->stock_num;

	/* Sell a few items */
	if (game_mode == GAME_NPPMORIA)
	{
		/* Sell a few items */
		j = j - randint(STORE_TURNOVER_NPPMORIA);

		/* Never keep more than "STORE_MAX_KEEP" slots */
		if (j > STORE_MAX_KEEP_NPPMORIA) j = STORE_MAX_KEEP_NPPMORIA;

		/* Always "keep" at least "STORE_MIN_KEEP" items */
		if (j < STORE_MIN_KEEP_NPPMORIA) j = STORE_MIN_KEEP_NPPMORIA;
	}
	else
	{
		/* Sell a few items */
		j = j - randint(STORE_TURNOVER_NPPANGBAND);

		/* Never keep more than "STORE_MAX_KEEP" slots */
		if (j > STORE_MAX_KEEP_NPPANGBAND) j = STORE_MAX_KEEP_NPPANGBAND;

		/* Always "keep" at least "STORE_MIN_KEEP" items */
		if (j < STORE_MIN_KEEP_NPPANGBAND) j = STORE_MIN_KEEP_NPPANGBAND;
	}



	/* Count how many items must be kept*/
	alt_min = st_ptr->stock_num - count_nonstandard_inven(which);

	/* Paranoia */
	if (alt_min < 0) alt_min = 0;

	/*
	 * Paranoia - the while loop below will lock up game if j
	 * is less than the # of "must-keep" items in store
	 */
	if (j < alt_min) j = alt_min;

	/* Destroy objects until only "j" slots are left */
	while (st_ptr->stock_num > j) store_delete_random(which);

	/* Choose the number of slots to fill */
	j = st_ptr->stock_num;

	/* Sell a few items */
	if (game_mode == GAME_NPPMORIA)
	{
		/* Buy some more items */
			j = j + randint(STORE_TURNOVER_NPPMORIA);

			/* Never keep more than "STORE_MAX_KEEP" slots */
			if (j > STORE_MAX_KEEP_NPPMORIA) j = STORE_MAX_KEEP_NPPMORIA;

			/* Always "keep" at least "STORE_MIN_KEEP" items */
			if (j < STORE_MIN_KEEP_NPPMORIA) j = STORE_MIN_KEEP_NPPMORIA;
	}
	else
	{
		/* Buy some more items */
			j = j + randint(STORE_TURNOVER_NPPANGBAND);

			/* Never keep more than "STORE_MAX_KEEP" slots */
			if (j > STORE_MAX_KEEP_NPPANGBAND) j = STORE_MAX_KEEP_NPPANGBAND;

			/* Always "keep" at least "STORE_MIN_KEEP" items */
			if (j < STORE_MIN_KEEP_NPPANGBAND) j = STORE_MIN_KEEP_NPPANGBAND;
	}

	/*
	 * Paranoia - should never happen unless the items in
	 * the keep_in_stock function is greatly enlarged.
	 */
	if (j < alt_min) j = alt_min;

	/* Hack -- prevent "overflow" */
	if (j >= st_ptr->stock_size) j = st_ptr->stock_size - 1;

	/* Calculate if the player gets altered inventory */
	if (randint1(500) < (p_ptr->lev + (p_ptr->max_depth / 2) + altered_inventory_counter))
	{
		allow_altered_inventory = TRUE;

		/* Reduce teh altered inventory counter */
		if (which == STORE_GENERAL)
		{
			if (altered_inventory_counter <= 10) altered_inventory_counter = 0;
			else altered_inventory_counter -= 10;
		}
		else
		{
			if (altered_inventory_counter <= 25) altered_inventory_counter = 0;
			else altered_inventory_counter -= 25;
		}

	}

	/*
	 * Paranoia:
	 * This should never be false unless a new store isn't set up properly.
	 * Note this function sets the allocation table, which must be undone at the bottom.
	 */
	if (prep_store_object(which))
	{
		/* Create some new items */
		while (st_ptr->stock_num < j) store_create_random(which);
	}

	/* Re-set all object generation settings and level rating. */
	get_obj_num_hook = NULL;
	get_obj_num_prep();
	object_generation_mode = OB_GEN_MODE_NORMAL;
	rating = old_rating;
	allow_altered_inventory = FALSE;

}


/*
 * Initialize the stores
 */
void store_init(int which)
{
	int k;

	/* Activate that store */
	st_ptr = &store[which];

	/* Pick an owner */
	st_ptr->owner = (byte)rand_int(z_info->b_max);

	/* Activate the new owner */
	ot_ptr = &b_info[(which * z_info->b_max) + st_ptr->owner];

	/* Nothing in stock */
	st_ptr->stock_num = 0;

	/* Clear any old items */
	for (k = 0; k < st_ptr->stock_size; k++)
	{
		object_wipe(&st_ptr->stock[k]);
	}

}


/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
	int i;

	/* Activate that store */
	store_type *st_ptr = &store[which];

	/* Ignore home & guild*/
	if ((which == STORE_HOME) || (which == STORE_GUILD)) return;

	/* Pick a new owner */
	i = st_ptr->owner;

	while (i == st_ptr->owner)
	    i = randint0(z_info->b_max);

	st_ptr->owner = i;

	/* Discount all the items */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &st_ptr->stock[i];

		/*don't discount the essential stock items*/
		if (keep_in_stock(o_ptr, which)) continue;

		/* Discount non-discounted items by 40 percent */
		if (o_ptr->discount == 0) o_ptr->discount = 40;

	}
}



/*** Display code ***/


/*
 * This function sets up screen locations based on the current term size.
 *
 * Current screen layout:
 *  line 0: reserved for messages
 *  line 1: shopkeeper and their purse / item buying price
 *  line 2: empty
 *  line 3: table headers
 *
 *  line 4: Start of items
 *
 * If help is turned off, then the rest of the display goes as:
 *
 *  line (height - 4): end of items
 *  line (height - 3): "more" prompt
 *  line (height - 2): empty
 *  line (height - 1): Help prompt and remaining gold
 *
 * If help is turned on, then the rest of the display goes as:
 *
 *  line (height - 7): end of items
 *  line (height - 6): "more" prompt
 *  line (height - 4): gold remaining
 *  line (height - 3): command help
 *
 *  Notice there is a slightly different layout for the guild.
 */
static void store_display_recalc(int this_store)
{
	int wid, hgt;
	int hgt_max;

	store_type *st_ptr = &store[this_store];

	Term_get_size(&wid, &hgt);

	/* Clip the width at a maximum of 104 (enough room for an 80-char item name) */
	if (wid > 104) wid = 104;

	/* Clip the text_out function at two smaller than the screen width */
	text_out_wrap = wid - 2;

	/* Put a reasonable limit on height for extremely large screens. */
	if ((this_store == STORE_GUILD) && !guild_quest_complete()) hgt_max = quests_max + 20;
	else hgt_max = st_ptr->stock_num + quests_max + 20;
	if (hgt > hgt_max) hgt = hgt_max;

	/* X co-ords first */
	scr_places_x[LOC_PRICE] = wid - 14;
	scr_places_x[LOC_AU] = wid - 26;
	scr_places_x[LOC_OWNER] = wid - 2;
	scr_places_x[LOC_WEIGHT] = wid - 14;

	/* Add space for for prices */
	if (current_store() != STORE_HOME)
		scr_places_x[LOC_WEIGHT] -= 10;

	/* Then Y */
	scr_places_y[LOC_OWNER] = 1;
	scr_places_y[LOC_HEADER] = 3;
	scr_places_y[LOC_ITEMS_START] = 4;

	/* If we are displaying help, make the height smaller */
	if (store_flags & (STORE_SHOW_HELP))
		hgt -= 3;

	scr_places_y[LOC_ITEMS_END] = hgt - 4;
	scr_places_y[LOC_MORE] = hgt - 3;
	scr_places_y[LOC_AU] = hgt - 2;

	/*Some guild-specific layout items*/
	if (this_store == STORE_GUILD)
	{
			scr_places_y[LOC_ITEMS_END] = hgt - 11;
			scr_places_y[LOC_MORE] = hgt - 10;
			scr_places_y[LOC_CUR_QUEST1] = hgt - 5;
			scr_places_y[LOC_CUR_QUEST2] = hgt - 4;
			scr_places_y[LOC_GUILD_REP] = hgt - 8;
	}


	/* If we're displaying the help, then put it with a line of padding */
	if (!(store_flags & (STORE_SHOW_HELP)))
	{
		hgt -= 2;
	}

	scr_places_y[LOC_HELP_CLEAR] = hgt - 1;
	scr_places_y[LOC_HELP_PROMPT] = hgt;
}

/* for entry_type */
#define ENTRY_SERVICE	0
#define ENTRY_QUEST		1
#define ENTRY_OBJECT	2


/*Gets the type of entry as well as the number*/
static int find_entry_type(int *entry_type, int oid)
{
	int entry_num;

	/* Find out what type of entry this is and get the entry_num. */
	if ((oid >= services_min) && (oid < services_max))
	{
		*entry_type = ENTRY_SERVICE;
		entry_num = oid;
	}
	else if ((oid >= quests_min) && (oid < quests_max))
	{
		*entry_type = ENTRY_QUEST;
		entry_num = oid - quests_min;
	}
	else  /*object*/
	{
		*entry_type = ENTRY_OBJECT;
		entry_num = oid - quests_max;
	}

	return (entry_num);
}


/*
 * Helper function for store_display_entry.
 * Returns TRUE if the identify status of the store object
 * has changed as a result of the item being displayed.
 */
static bool object_ident_changed(object_type *o_ptr)
{
	bool ident_changed = FALSE;

	/* Object will now be id'ed instead of sensed, and
	 * wands/staves/rods will be re-charged.
	 */
	o_ptr->ident &= ~(IDENT_SENSE | IDENT_EMPTY);

	/*
	 * Player is now aware of this object kind, including flavors.
	 * All objects *identified*.
	 */

	if (!k_info[o_ptr->k_idx].everseen)
	{
		ident_changed = TRUE;
		k_info[o_ptr->k_idx].everseen = TRUE;
	}
	if (!k_info[o_ptr->k_idx].aware)
	{
		ident_changed = TRUE;
		k_info[o_ptr->k_idx].aware = TRUE;
	}
	if (!(o_ptr->ident & (IDENT_KNOWN)))
	{
		ident_changed = TRUE;
		o_ptr->ident |= (IDENT_KNOWN);
	}
	if ((!(o_ptr->ident & (IDENT_MENTAL))) &&
		((o_ptr->ego_num) || (o_ptr->art_num)))
	{
		ident_changed = TRUE;
		o_ptr->ident |= (IDENT_MENTAL);
	}

	return (ident_changed);
}

/*
 * Redisplay a single store entry
 */
static void store_display_entry(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	s32b x;
	byte desc = ODESC_PREFIX;
	int entry_type;
   	int entry_num;

	char i_name[80];
	char out_val[160];
	byte colour = TERM_WHITE;

	int this_store = current_store();
	store_type *st_ptr = &store[this_store];
	(void)menu;
	(void)cursor;
	(void)width;

	entry_num = find_entry_type(&entry_type, oid);

	/* Display the entry name and, if object, weight*/
	if (entry_type == ENTRY_SERVICE)
	{
		colour = TERM_L_GREEN;
		my_strcpy(i_name, service_names[services_offered[entry_num]], sizeof(i_name));
	}
	else if (entry_type == ENTRY_QUEST)
	{
		my_strcpy(i_name, quest_title[quests_offered[entry_num]], sizeof(i_name));
	}
	else /*object*/
	{

		/* Get the object */
		object_type *o_ptr = &st_ptr->stock[entry_num];

		colour = tval_to_attr[o_ptr->tval & 0x7F];

		/* Don't display the quest reward inventory until the quest is complete */
		if (this_store == STORE_GUILD)
		{
			if (!guild_quest_complete()) return;
		}

		if (this_store !=STORE_HOME)
		{
			if (object_ident_changed(o_ptr)) store_updates();
		}

		/* Describe the object - preserving insriptions in the home */
		if ((this_store == STORE_HOME) || (this_store == STORE_GUILD)) desc = ODESC_FULL;
		else desc = ODESC_FULL | ODESC_STORE;
		object_desc(i_name, sizeof(i_name), o_ptr, ODESC_PREFIX | desc);
	}

	/*Always use the same color for highlighted items */
	if (cursor) colour = curs_attrs[CURS_KNOWN][(int)cursor];

	/* Print the entry */
	c_put_str(colour, i_name, row, col);

	/* Show weights for objects */
    if (entry_type == ENTRY_OBJECT)
	{
		/* Redundant, but it avoids compiler warnings */
		object_type *o_ptr = &st_ptr->stock[entry_num];

		/* Make sure long inscriptions aren't mixed up in the weight */
		Term_erase(row, scr_places_x[LOC_WEIGHT]-1, 20);

		strnfmt(out_val, sizeof out_val, "%3d.%d lb", o_ptr->weight / 10, o_ptr->weight % 10);
		c_put_str(curs_attrs[CURS_KNOWN][(int)cursor], out_val, row, scr_places_x[LOC_WEIGHT]);
	}

	/* Get the price if appropriate*/
	if (this_store == STORE_HOME) return;
	/* No prices for quest rewards */
	if (this_store == STORE_GUILD)
	{
		if (guild_quest_complete()) return;
	}

	/* Display the entry price */
	if (entry_type == ENTRY_SERVICE)
	{
		x = price_services(this_store, services_offered[entry_num]);

		if (x) strnfmt(out_val, sizeof out_val, "%9ld", (long)x);
		else strnfmt(out_val, sizeof out_val, "     Free");

		/* Display the service name */
		c_put_str(colour, i_name, row, col);
	}
	else if (entry_type == ENTRY_QUEST)
	{
		/* No prices for quests*/
		return;

	}
	else /*object*/
	{
		/* Redundant, but it avoids compiler warnings */
		object_type *o_ptr = &st_ptr->stock[entry_num];

		/* Extract the "minimum" price */
		x = price_item(o_ptr, FALSE);

		if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)) &&
		    (o_ptr->number > 1))
			strnfmt(out_val, sizeof out_val, "%9ld avg", (long)x);
		else
			strnfmt(out_val, sizeof out_val, "%9ld    ", (long)x);
	}

	/* Make sure the player can afford it */
	if ((int) p_ptr->au < (int) x)
	{
		colour = curs_attrs[CURS_UNKNOWN][(int)cursor];
	}

	/* Make sure long inscriptions aren't mixed up in the price */
	Term_erase(row, scr_places_x[LOC_PRICE]-1, 20);

	/* Actually draw the price */
	c_put_str(colour, out_val, row, scr_places_x[LOC_PRICE]);

}

/*
 * Display store (after clearing screen)
 */
static void store_display_frame(void)
{
	char buf[80];
	int this_store = current_store();

	owner_type *ot_ptr = store_owner(this_store);

	/* Clear screen, except for status bar */
	clear_from(1);

	/* The "Home" is special */
	if (this_store == STORE_HOME)
	{
		/* Put the owner name */
		put_str("Your Home", scr_places_y[LOC_OWNER], 1);

		/* Label the object descriptions */
		put_str("Home Inventory", scr_places_y[LOC_HEADER], 1);

		/* Show weight header */
		put_str("Weight", 5, scr_places_x[LOC_WEIGHT] + 2);
	}

	/*The Guild is also special*/
	else if (this_store == STORE_GUILD)
	{
		/* Put the owner name */
		put_str("The Adventurer's Guild", scr_places_y[LOC_OWNER], 1);

		/* Label the object descriptions */
		if (guild_quest_complete())
		{
			put_str("Guild Services and Quest Rewards",	scr_places_y[LOC_HEADER], 1);
		}
		else if (guild_quest_level())
		{
			char q_out[120];

			put_str("Guild Services", scr_places_y[LOC_HEADER], 1);

			/* Print out the quest on 2 different lines. */
			c_put_str(TERM_BLUE, "Your current quest:", scr_places_y[LOC_CUR_QUEST1]-1, 1);
			describe_quest(q_out, sizeof(q_out), guild_quest_level(), QMODE_HALF_1);
			put_str(q_out, scr_places_y[LOC_CUR_QUEST1], 1);

			/* Put the monster symbol at the end if necessary */
			show_quest_mon(scr_places_y[LOC_CUR_QUEST1], 1 + strlen(q_out));
			describe_quest(q_out, sizeof(q_out), guild_quest_level(), QMODE_HALF_2);
			put_str(q_out, scr_places_y[LOC_CUR_QUEST2], 1);
		}
		else
		{
			put_str("Guild Services and Available Quests",
							scr_places_y[LOC_HEADER], 1);
		}

		/* Print the standard greeting and reputation. */
		prt_rep_guild(scr_places_y[LOC_GUILD_REP], 3);
	}

	/* Normal stores */
	else
	{
		const char *store_name = (f_name + f_info[cave_feat[p_ptr->py][p_ptr->px]].name);
		const char *owner_name = &b_name[ot_ptr->owner_name];

		/* Put the owner name */
		put_str(owner_name, scr_places_y[LOC_OWNER], 1);

		/* Show the max price in the store (above prices) */
		strnfmt(buf, sizeof(buf), "%s (%ld)", store_name, (long)(ot_ptr->max_cost));
		prt(buf, scr_places_y[LOC_OWNER], scr_places_x[LOC_OWNER] - strlen(buf));


		/* Label the object descriptions */
		if (services_max)
		{
			put_str("Store Inventory and Services", scr_places_y[LOC_HEADER], 1);
		}
		else put_str("Store Inventory", scr_places_y[LOC_HEADER], 1);

		/* Showing weight label */
		put_str("Weight", scr_places_y[LOC_HEADER], scr_places_x[LOC_WEIGHT] + 2);

		/* Label the asking price (in stores) */
		put_str("Price", scr_places_y[LOC_HEADER], scr_places_x[LOC_PRICE] + 4);
	}
}



/*
 * Display help.
 */
static void store_display_help(void)
{
	int help_loc = scr_places_y[LOC_HELP_PROMPT];

	/* Clear */
	clear_from(scr_places_y[LOC_HELP_CLEAR]);

	/* Prepare help hooks */
	text_out_hook = text_out_to_screen;
	text_out_indent = 1;
	Term_gotoxy(1, help_loc);

	text_out("Use the ");
	text_out_c(TERM_L_GREEN, "movement keys");
	text_out(" or ");
	text_out_c(TERM_L_GREEN, "a mouseclick");
	text_out(" to navigate, or ");
	text_out_c(TERM_L_GREEN, "Space");
	text_out(" to advance to the next page. '");

	if (rogue_like_commands)
		text_out_c(TERM_L_GREEN, "x");
	else
		text_out_c(TERM_L_GREEN, "l");

	text_out("' examines and '");
	text_out_c(TERM_L_GREEN, "p");
	text_out("' or'");
	text_out_c(TERM_L_GREEN, "g");

	if (current_store() == STORE_HOME) text_out("' picks up");
	else text_out("' purchases");

	text_out(" the selected item. '");

	text_out_c(TERM_L_GREEN, "s");
	text_out("' or'");
	text_out_c(TERM_L_GREEN, "d");
	if (current_store() == STORE_HOME) text_out("' drops");
	else text_out("' sells");

	text_out(" an item from your inventory. ");

	text_out_c(TERM_L_GREEN, "ESC");
	text_out(" exits the building.");

	text_out_indent = 0;
}


/*
 * Decides what parts of the store display to redraw.  Called on terminal
 * resizings and the redraw command.
 */
static void store_redraw(void)
{
	if (store_flags & (STORE_FRAME_CHANGE))
	{
		store_display_frame();

		if (store_flags & STORE_SHOW_HELP)
			store_display_help();
		else
			prt("Press '?' for help.", scr_places_y[LOC_HELP_PROMPT], 1);

		store_flags &= ~(STORE_FRAME_CHANGE);
		event_signal(EVENT_MOUSEBUTTONS);
	}

	if (store_flags & (STORE_GOLD_CHANGE))
	{
		prt(format("Gold Remaining: %9ld", (long)p_ptr->au),
		    scr_places_y[LOC_AU], scr_places_x[LOC_AU]);
		store_flags &= ~(STORE_GOLD_CHANGE);
	}
}


/*** Higher-level code ***/


static bool store_get_check(const char *prompt)
{
	ui_event_data ch;
	bool return_v = FALSE;

	/* Prompt for it */
	prt(prompt, 0, 0);

	/* Make some buttons */
	button_backup_all();
	button_kill_all();
	button_add("[YES]", 'y');
	button_add("[NO]", 'n');
	event_signal(EVENT_MOUSEBUTTONS);

	while (TRUE)
	{
		/* Get an answer */
		ch = inkey_ex();

		if ((strchr("Nn", ch.key)) || (ch.key == ESCAPE)) break;
		if ((strchr("Yy", ch.key)) || (ch.key == '\r') || (ch.key == '\r') || (ch.key == '\xff'))
		{
			return_v = TRUE;
			break;
		}

	}

	/* Kill the buttons */
	/* Restore the old buttons */
	button_restore();
	event_signal(EVENT_MOUSEBUTTONS);

	/* Erase the prompt */
	prt("", 0, 0);

	/* Success */
	return (return_v);
}


/*
 * Return the quantity of a given item in the pack (include quiver).
 */
static int find_inven(const object_type *o_ptr)
{
	int j;
	int num = 0;

	u32b f1, f2, f3, fn;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Similar slot? */
	for (j = 0; j < QUIVER_END; j++)
	{
		object_type *j_ptr = &inventory[j];

		u32b j1, j2, j3, jn;

		/* Check only the inventory and the quiver */
		if (j >= INVEN_WIELD && j < QUIVER_START) continue;

		/* Require identical object types */
		if (!j_ptr->k_idx || o_ptr->k_idx != j_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(j_ptr, &j1, &j2, &j3, &jn);


		/* Different flags */
		if (f1 != j1 ||
			f2 != j2 ||
			f3 != j3 ||
			fn != jn)
			continue;

		/* Analyze the items */
		switch (o_ptr->tval)
		{
			/* Chests */
			case TV_CHEST:
			{
				/* Never okay */
				return 0;
			}

			/* Food and Potions and Scrolls */
			case TV_FOOD:
			case TV_POTION:
			case TV_SCROLL:
			{
				/* Assume okay */
				break;
			}

			/* Staves and Wands */
			case TV_STAFF:
			case TV_WAND:
			{
				/* Assume okay */
				break;
			}

			/* Rods */
			case TV_ROD:
			{
				/* Assume okay */
				break;
			}

			/* Weapons and Armor */
			case TV_BOW:
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				/* Fall through */
			}

			/* Rings, Amulets, Lights */
			case TV_RING:
			case TV_AMULET:
			case TV_LIGHT:
			{
				/* Require both items to be known */
				if (!object_is_known(o_ptr) || !object_is_known(j_ptr)) continue;

				/* Fall through */
			}

			/* Missiles */
			case TV_BOLT:
			case TV_ARROW:
			case TV_SHOT:
			{
				/* Require identical knowledge of both items */
				if (object_is_known(o_ptr) != object_is_known(j_ptr)) continue;

				/* Require identical "bonuses" */
				if (o_ptr->to_h != j_ptr->to_h) continue;
				if (o_ptr->to_d != j_ptr->to_d) continue;
				if (o_ptr->to_a != j_ptr->to_a) continue;

				/* Require identical "pval" code */
				if (o_ptr->pval != j_ptr->pval) continue;

				/* Require identical "artifact" names */
				if (o_ptr->art_num != j_ptr->art_num) continue;

				/* Require identical "ego-item" names */
				if (o_ptr->ego_num != j_ptr->ego_num) continue;

				/* Lights must have same amount of fuel */
				else if (o_ptr->timeout != j_ptr->timeout && o_ptr->tval == TV_LIGHT)
					continue;

				/* Require identical "values" */
				if (o_ptr->ac != j_ptr->ac) continue;
				if (o_ptr->dd != j_ptr->dd) continue;
				if (o_ptr->ds != j_ptr->ds) continue;

				/* Probably okay */
				break;
			}

			/* Various */
			default:
			{
				/* Require knowledge */
				if (!object_is_known(o_ptr) || !object_is_known(j_ptr)) continue;

				/* Probably okay */
				break;
			}
		}

		/* Different flags */
		if ((f1 != j1) || (f2 != j2) || \
			(f3 != j3) || (fn != jn)) continue;

		/* They match, so add up */
		num += j_ptr->number;
	}

	return num;
}

/*
 * Buy the item with the given index from the current store's inventory.
 */
void do_cmd_buy(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;
	int amt = args[1].number;

	object_type *o_ptr;
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;

	char o_name[80];
	int price, item_new;

	store_type *st_ptr;
	int this_store = current_store();

	if (this_store == STORE_NONE)
	{
		msg_print("You cannot purchase items when not in a store.");
		return;
	}

	st_ptr = &store[this_store];

	/* Get the actual object */
	o_ptr = &st_ptr->stock[item];

	/* Get desired object */
	object_copy_amt(i_ptr, o_ptr, amt);

	/* Ensure we have room */
	if (!inven_carry_okay(i_ptr))
	{
		msg_print("You cannot carry that many items.");
		return;
	}

	/* Describe the object (fully) */
	object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Extract the price for the entire stack */
	price = price_item(i_ptr, FALSE) * i_ptr->number;

	if (price > p_ptr->au)
	{
		msg_print("You cannot afford that purchase.");
		return;
	}

	/* Spend the money */
	p_ptr->au -= price;

	/* Update the display */
	store_flags |= (STORE_GOLD_CHANGE | STORE_FRAME_CHANGE);

	/* ID objects on buy */
	identify_object(i_ptr, TRUE);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* The object no longer belongs to the store */
	i_ptr->ident &= ~(IDENT_STORE);

	/* Clear the top line */
	clear_message_line();

	/* Message */
	if (one_in_(3)) message(MSG_STORE5, 0, ONE_OF(comment_accept));
	msg_format("You bought %s for %ld gold.", o_name, (long)price);

	/* Erase the inscription */
	i_ptr->obj_note = 0;

	/* Give it to the player */
	item_new = inven_carry(i_ptr);

	/* Message */
	object_desc(o_name, sizeof(o_name), &inventory[item_new], ODESC_PREFIX | ODESC_FULL);

	msg_format("You have %s (%c).", o_name, index_to_label(item_new));

	/* Hack - Reduce the number of charges in the original stack */
	if (o_ptr->tval == TV_WAND || o_ptr->tval == TV_STAFF)
	{
		o_ptr->pval -= i_ptr->pval;
	}

	/* Handle stuff */
	handle_stuff();

	/* Remove the bought objects from the store */
	store_item_increase(this_store, item, -amt);
	store_item_optimize(this_store, item);

	/* Store is empty */
	if ((st_ptr->stock_num == 0) || (count_nonstandard_inven(this_store) == 0))
	{
		int i;

		/* Shuffle */
		if (one_in_(STORE_SHUFFLE))
		{
			/* Message */
			msg_print("The shopkeeper retires.");

			/* Shuffle the store */
			store_shuffle(this_store);
			store_maint(this_store);
			store_flags |= STORE_FRAME_CHANGE;
		}

		/* Maintain */
		else
		{
			/* Message */
			msg_print("The shopkeeper brings out some new stock.");
		}

		/* New inventory */
		for (i = 0; i < 10; ++i)
		{
			/* Maintain the store */
			store_maint(this_store);
		}
	}
}

/*
 * Retrieve the item with the given index from the home's inventory.
 */
void do_cmd_reward(cmd_code code, cmd_arg args[])
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	int item = args[0].item;
	int amt = args[1].number;
	char title[40];
	object_type *o_ptr;
	object_kind *k_ptr;
	object_type picked_item;
	char o_name[80];
	int item_new;

	store_type *st_ptr;

	/*Get the current title*/
	get_title(title, sizeof(title));

	/* Paranoia */
	if (current_store() != STORE_GUILD)
	{
		msg_print(format("You are not currently in the guild, %s.", title));
		return;
	}
	if (!guild_quest_complete())
	{
		msg_print(format("You are not currently eligible for a quest reward, %s.", title));
		return;
	}

	st_ptr = &store[STORE_GUILD];

	/* Get the actual object */
	o_ptr = &st_ptr->stock[item];
	k_ptr = &k_info[o_ptr->k_idx];

	/* Mark the history */
	o_ptr->origin_nature = ORIGIN_REWARD;
	o_ptr->origin_r_idx = 0;
	o_ptr->origin_dlvl = q_ptr->base_level;

	/* Get desired object */
	object_copy_amt(&picked_item, o_ptr, amt);

	/* Ensure we have room */
	if ((!inven_carry_okay(&picked_item)) && (o_ptr->tval != TV_GOLD))
	{
		msg_print(format("You cannot carry that many items, %s.", title));
		return;
	}

	/* Give it to the player, with gold handled differently than objects */
	if (o_ptr->tval == TV_GOLD)
	{
		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

		p_ptr->au += o_ptr->pval;
		msg_format("You have been rewarded with %s, %s.", o_name, title);
	}
	else
	{
		item_new = inven_carry(&picked_item);

		/* Describe just the result */
		object_desc(o_name, sizeof(o_name), &inventory[item_new], ODESC_PREFIX | ODESC_FULL);

		/* Message */
		msg_format("You have been rewarded with %s (%c), %s.", o_name, index_to_label(item_new), title);
	}

	/*It's an ironman spellbook, so make the spells available. */
	if ((k_ptr->k_flags3 & (TR3_IRONMAN_ONLY)) && (cp_ptr->spell_book == k_ptr->tval))
	{
		byte j;

		/* Extract spells */
		for (j = 0; j < SPELLS_PER_BOOK; j++)
		{
			s16b spell = get_spell_from_list(k_ptr->sval, j);

			/*skip blank spell slots*/
			if (spell == -1) continue;

			/* Remove the Ironman Restriction. */
			p_ptr->spell_flags[spell] &= ~(PY_SPELL_IRONMAN);
		}

		/* Update the spells. */
		p_ptr->update |= PU_SPELLS;
	}

	/* Handle Artifacts */
	if (o_ptr->art_num)
	{
		/*
		 * Artifact might not yet be marked as created (if it was chosen from tailored
		 * rewards), so now it's the right time to mark it.
		 */
		a_info[o_ptr->art_num].a_cur_num = 1;

		/* If the item was an artifact, and if the auto-note is selected, write a message. */
		if (adult_take_notes)
		{
			int artifact_depth;
			char note[120];
			char shorter_desc[100];

			/* Get a shorter description to fit the notes file */
			object_desc(shorter_desc, sizeof(shorter_desc), o_ptr, ODESC_BASE);

			/* Build note and write */
			sprintf(note, "Quest Reward: %s", shorter_desc);

			/*record the depth where the artifact was created */
			artifact_depth = o_ptr->xtra1;

			do_cmd_note(note, artifact_depth);

			/*mark item creation depth as 0, which will indicate the artifact
			 *has been previously identified.  This prevents an artifact from showing
			 *up on the notes list twice if it has been previously identified.  JG */
			o_ptr->xtra1 = 0;
		}

		/* Process artifact lore */
		if (ARTIFACT_EASY_MENTAL(o_ptr))
		{
			/* Get the lore entry */
			artifact_lore *a_l_ptr = &a_l_list[o_ptr->art_num];

			/* Remember this artifact from now on */
			a_l_ptr->was_fully_identified = TRUE;
		}
	}

	/* Handle stuff */
	handle_stuff();

	/* Remove the item from the guild before we wipe everything */
	store_item_increase(STORE_GUILD, item, -amt);
	store_item_optimize(STORE_GUILD, item);

	/* The quest is over */
	guild_quest_wipe(TRUE);
	init_services_and_quests(STORE_GUILD);
	p_ptr->redraw |= (PR_QUEST_ST);
	store_flags |= (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE);
}


/*
 * Retrieve the item with the given index from the home's inventory.
 */
void do_cmd_retrieve(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;
	int amt = args[1].number;

	object_type *o_ptr;
	object_type picked_item;
	char o_name[80];
	int item_new;

	store_type *st_ptr;

	if (current_store() != STORE_HOME)
	{
		msg_print("You are not currently at home.");
		return;
	}

	st_ptr = &store[STORE_HOME];

	/* Get the actual object */
	o_ptr = &st_ptr->stock[item];

	/* Get desired object */
	object_copy_amt(&picked_item, o_ptr, amt);

	/* Ensure we have room */
	if (!inven_carry_okay(&picked_item))
	{
		msg_print("You cannot carry that many items.");
		return;
	}

	/* Distribute charges of wands, staves, or rods */
	distribute_charges(o_ptr, &picked_item, amt);

	/* Give it to the player */
	item_new = inven_carry(&picked_item);

	/* Describe just the result */
	object_desc(o_name, sizeof(o_name), &inventory[item_new],
					ODESC_PREFIX | ODESC_FULL);

	/* Message */
	msg_format("You have %s (%c).", o_name, index_to_label(item_new));

	/* Handle stuff */
	handle_stuff();

	/* Remove the items from the home */
	store_item_increase(STORE_HOME, item, -amt);
	store_item_optimize(STORE_HOME, item);

}


/*
 * The "randart" tester
 */
bool item_tester_hook_randart(const object_type *o_ptr)
{
	/*Hack - don't allow cursed items*/
	if(o_ptr->ident & (IDENT_CURSED)) return (FALSE);

	/*Hack - don't allow broken items*/
	if(o_ptr->ident & (IDENT_BROKEN)) return (FALSE);

	/*Hack - don't allow unidentified items*/
	if(!(o_ptr->ident & (IDENT_KNOWN))) return (FALSE);

	/* Don't use current artifacts */
	if (o_ptr->art_num) return (FALSE);

	if (can_be_randart(o_ptr))
	{
		/*We don't use ego-items, unless dragon armor*/
		if ((o_ptr->tval != TV_DRAG_ARMOR) && (o_ptr->tval != TV_DRAG_SHIELD))
		{
			if (o_ptr->ego_num) return(FALSE);
		}

		/*don't make artifacts out of stacks of items*/
		if (o_ptr->number > 1) return (FALSE);

		/*eligible to be a randart*/
		return (TRUE);
	}

	/* Assume cannot be a randart */
	return (FALSE);
}

/*
 * The flammable book tester
 */
bool item_tester_hook_flammable_book(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;

	if 	((o_ptr->tval != TV_PRAYER_BOOK) && (o_ptr->tval != TV_DRUID_BOOK) &&
	 	 (o_ptr->tval != TV_MAGIC_BOOK)) return (FALSE);

	/* Get the "known" flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/*already flammable*/
	if (f3 & TR3_IGNORE_FIRE) return (FALSE);

	/* Immune to lava, so they should resist fire. */
	if (fn & ELEMENT_LAVA) return (FALSE);

	/* Flammable spellbook */
	return (TRUE);
}




#define DISPLAY_STAT_ROW		8
#define DISPLAY_STAT_COL		10

/*
 * Buy an object or service from a store, or get a quest from the guild.
 */
static bool store_purchase(int oid)
{
	int max_amount, amount_purchased, num, entry_num, entry_type;

	object_type *o_ptr;

	object_type object_type_body;
	object_type *i_ptr = &object_type_body;

	char o_name[80];

	s32b price;

	int this_store = current_store();

	store_type *st_ptr;

	if (this_store == STORE_NONE)
	{
		msg_print("You cannot purchase items when not in a store.");
		return FALSE;
	}

	st_ptr = &store[this_store];

	entry_num = find_entry_type(&entry_type, oid);

	/* Handle services and quests differently. */
	if (entry_type == ENTRY_SERVICE)
	{
		bool success = service_purchase(this_store, services_offered[entry_num]);

		if ((services_offered[entry_num] >= QUEST_REWARD_HEAD) &&
			(services_offered[entry_num] <= QUEST_REWARD_TAIL))
		{
			if (success)
			{
				init_services_and_quests(this_store);

			}
		}

		p_ptr->redraw |= (PR_QUEST_ST);

		store_flags |= (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE);

		return (success);
	}
	else if (entry_type == ENTRY_QUEST)
	{
		bool success = guild_purchase(quests_offered[entry_num]);

		/*
		 * We must now re-do the entry list and store layout
		 * since the number of entries has changed
		 */
		if (success)
		{
			init_services_and_quests(this_store);
		}

		p_ptr->redraw |= (PR_QUEST_ST);

		store_flags |= (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE);

		return (success);

	}
	/* else an object */

	/* Get the actual object */
	o_ptr = &st_ptr->stock[entry_num];
	if (entry_num < 0) return FALSE;

	if ((this_store == STORE_HOME) || (this_store == STORE_GUILD))
	{
		max_amount = o_ptr->number;
	}
	else
	{
		/* Price of one */
		price = price_item(o_ptr, FALSE);

		/* Check if the player can afford any at all */
		if ((u32b)p_ptr->au < (u32b)price)
		{
			/* Tell the user */
			msg_print("You do not have enough gold for this item.");

			/* Abort now */
			return FALSE;
		}

		/* Work out how many the player can afford */
		max_amount = p_ptr->au / price;
		if (max_amount > o_ptr->number) max_amount = o_ptr->number;
	}

	/* Find the number of this item in the inventory */
	if (!object_flavor_is_aware(o_ptr))
		num = 0;
	else
		num = find_inven(o_ptr);

	if (this_store == STORE_GUILD)
	{
		amount_purchased = o_ptr->number;
	}
	else
	{
		strnfmt(o_name, sizeof o_name, "%s how many%s? (max %d) ",
	        (this_store == STORE_HOME) ? "Take" : "Buy",
	        num ? format(" (you have %d)", num) : "", max_amount);

		/* Get a quantity */
		amount_purchased = get_quantity(o_name, max_amount);
	}

	/* Allow user abort */
	if (amount_purchased <= 0) return FALSE;

	/* Get desired object */
	object_copy_amt(i_ptr, o_ptr, amount_purchased);

	/* Ensure we have room */
	if ((!inven_carry_okay(i_ptr)) && (i_ptr->tval != TV_GOLD))
	{
		msg_print("You cannot carry that many items.");
		return FALSE;
	}

	/* Attempt to buy it */
	/* Home is much easier */
	if (this_store == STORE_HOME)
	{
		cmd_insert(CMD_RETRIEVE, entry_num, amount_purchased);
	}
	else if (this_store == STORE_GUILD)
	{
		cmd_insert(CMD_REWARD, entry_num, amount_purchased);
	}
	else
	{
		u32b price;
		bool response;

		/* Extract the price for the entire stack */
		price = price_item(i_ptr, FALSE) * i_ptr->number;


		/* Describe the object (fully) */
		object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

		screen_save();

		/* Show price */
		prt(format("Price: %d", price), 1, 0);

		/* Confirm purchase */
		response = store_get_check(format("Buy %s? (y/n)", o_name));
		screen_load();

		/* Negative response, so give up */
		if (!response)
		{
			/*re-distribute charges*/
			o_ptr->timeout += i_ptr->timeout;

			return FALSE;
		}

		cmd_insert(CMD_BUY, entry_num, amount_purchased);
	}

	store_updates();


	return TRUE;
}



/*
 * Determine if the current store will purchase the given object
 */
static bool store_will_buy_tester(const object_type *o_ptr)
{
	int this_store = current_store();

	if (this_store == STORE_NONE) return FALSE;

	return store_will_buy(this_store, o_ptr);
}

/*
 * Sell an item to the current store.
 */
void do_cmd_sell(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;
	int amt = args[1].number;
	object_type sold_item;
	int price, dummy, value;
	char o_name[120];

	/* Get the item */
	object_type *o_ptr = object_from_item_idx(item);

	/* Cannot remove cursed objects */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		msg_print("Hmmm, it seems to be cursed.");
		return;
	}

	/* Check we are somewhere we can sell the items. */
	if (current_store() == STORE_NONE)
	{
		msg_print("You cannot sell items when not in a store.");
		return;
	}

	/* Check the store wants the items being sold */
	if (!store_will_buy(current_store(), o_ptr))
	{
		msg_print("I do not wish to purchase this item.");
		return;
	}

	/* Get a copy of the object representing the number being sold */
	object_copy_amt(&sold_item, o_ptr, amt);


	/* Check if the store has space for the items */
	if (!store_check_num(current_store(), &sold_item))
	{
		msg_print("I have not the room in my store to keep it.");
		return;
	}

	price = price_item(&sold_item, TRUE) * amt;

	/* Get some money */
	p_ptr->au += price;

	/* Update the display */
	store_flags |= (STORE_GOLD_CHANGE | STORE_FRAME_CHANGE);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_RESIST | PR_EXP |
			  PR_STATS | PU_NATIVE | PR_ITEMLIST);

	/* Get the "apparent" value */
	dummy = object_value(&sold_item) * amt;

	/* Identify original object */
	identify_object(o_ptr, TRUE);

	/* Take a new copy of the now known-about object. */
	object_copy_amt(&sold_item, o_ptr, amt);

	/* The item belongs to the store now */
	sold_item.ident |= IDENT_STORE;
	o_ptr->ident &= ~(IDENT_QUIVER);

	/*
	 * Hack -- Allocate charges between those wands, staves, or rods
	 * sold and retained, unless all are being sold.
	 */
	distribute_charges(o_ptr, &sold_item, amt);

	/* Get the "actual" value */
	value = object_value(&sold_item) * amt;

	/* Get the description all over again */
	object_desc(o_name, sizeof(o_name), &sold_item, ODESC_PREFIX | ODESC_FULL);

	/* Describe the result (in message buffer) */
	msg_format("You sold %s (%c) for %ld gold.",
			   o_name, index_to_label(item), (long)price);

	/* Analyze the prices (and comment verbally) */
	purchase_analyze(price, value, dummy);

	/* Take the object from the player */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

	/* Handle stuff */
	handle_stuff();

	/* The store gets that (known) object */
	store_carry(current_store(), &sold_item);

}

/*
 * Stash an item in the home.
 */
void do_cmd_stash(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;
	int amt = args[1].number;
	object_type dropped_item;
	object_type *o_ptr = object_from_item_idx(item);
	char o_name[120];

	/* Check we are somewhere we can stash items. */
	if (current_store() != STORE_HOME)
	{
		msg_print("You are not in your home.");
		return;
	}

	/* Cannot remove cursed objects */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		msg_print("Hmmm, it seems to be cursed.");
		return;
	}

	/* Get a copy of the object representing the number being sold */
	object_copy_amt(&dropped_item, o_ptr, amt);

	if (!store_check_num(STORE_HOME, &dropped_item))
	{
		msg_print("Your home is full.");
		return;
	}

	/* Distribute charges of wands/staves/rods */
	distribute_charges(o_ptr, &dropped_item, amt);

	/* Describe */
	object_desc(o_name, sizeof(o_name), &dropped_item, ODESC_PREFIX | ODESC_FULL);

	/* Message */
	msg_format("You drop %s (%c).", o_name, index_to_label(item));

	/* Take it from the players inventory */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

	/* Handle stuff */
	handle_stuff();

	/* Let the home carry it */
	home_carry(&dropped_item);

}

/*
 * Sell an object, or drop if it we're in the home.
 */
static bool store_sell(void)
{
	int amt;
	int item;
	int get_mode = (USE_EQUIP | USE_INVEN | USE_FLOOR  | USE_QUIVER);

	object_type *o_ptr;
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;

	char o_name[120];


	const char *reject = "You have nothing that I want. ";
	const char *prompt = "Sell which item? ";

	int this_store = current_store();

	if (this_store == STORE_NONE)
	{
		msg_print("You cannot sell items when not in a store.");
		return (FALSE);
	}

	if (this_store == STORE_GUILD)
	{
		prt("", 0, 0);
		msg_print("The Guild does not purchase items.");
		return (FALSE);
	}

	/* Clear all current messages */
	msg_flag = FALSE;
	prt("", 0, 0);

	if (this_store == STORE_HOME)
		prompt = "Drop which item? ";
	else
	{
		item_tester_hook = store_will_buy_tester;
	}

	/* Get an item */
	p_ptr->command_wrk = USE_INVEN;
	p_ptr->command_cmd = 'd';
	if (!get_item(&item, prompt, reject, get_mode))
	{
		return (FALSE);
	}

	/* Get the item */
	o_ptr = object_from_item_idx(item);

	/* Hack -- Cannot remove cursed objects */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return (FALSE);
	}

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return (FALSE);

	/* Get a copy of the object representing the number being sold */
	object_copy_amt(i_ptr, object_from_item_idx(item), amt);

	if (!store_check_num(this_store, i_ptr))
	{

		if (this_store == STORE_HOME)
			msg_print("Your home is full.");

		else
			msg_print("I have not the room in my store to keep it.");

		return (FALSE);
	}

	/* Get a full description */
	object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Real store */
	if (this_store != STORE_HOME)
	{
		/* Extract the value of the items */
		u32b price = price_item(i_ptr, TRUE) * amt;

		screen_save();

		/* Show price */
		prt(format("Price: %d", price), 1, 0);

		/* Confirm sale */
		if (!store_get_check(format("Sell %s? (y/n)", o_name)))
		{
			screen_load();
			return (FALSE);
		}

		screen_load();

		cmd_insert(CMD_SELL, item, amt);

	}

	/* Player is at home */
	else
	{
		cmd_insert(CMD_STASH, item, amt);
	}

	store_updates();

	return (TRUE);

}

/*
 * Examine an item in a store
 */
static void store_examine(int oid)
{
	store_type *st_ptr = &store[current_store()];
	object_type *o_ptr;
	char file_name[80];
	char service_name[120];

	int entry_num, entry_type;

	if (oid < 0) return;

	entry_num = find_entry_type(&entry_type, oid);

	screen_save();

	/* Clear the screen */
	Term_erase(0, 0, 255);
	Term_gotoxy(0, 0);

	/* Display the entry name and, if object, weight*/
	if (entry_type == ENTRY_SERVICE)
	{
		strnfmt(file_name, sizeof(file_name), "town.txt");
		strnfmt(service_name, sizeof(service_name), service_names[services_offered[entry_num]]);
		show_file(format("%s#%s", file_name, service_name), NULL,  0, 0);
		screen_load();
		return;
	}
	else if (entry_type == ENTRY_QUEST)
	{
		strnfmt(file_name, sizeof(file_name), "quests.txt");
		strnfmt(service_name, sizeof(service_name), quest_title[quests_offered[entry_num]]);
		show_file(format("%s#%s", file_name, service_name), NULL,  0, 0);
		screen_load();
		return;
	}

	/* else (entry type == ENTRY_OBJECT) */

	/* Get the actual object */
	o_ptr = &st_ptr->stock[entry_num];

	/* Show full info in most stores, but normal info in player home */
	object_info_screen(o_ptr);

	screen_load();

	/* Process artifact lore */
	if (ARTIFACT_EASY_MENTAL(o_ptr))
	{
		/* Get the lore entry */
		artifact_lore *a_l_ptr = &a_l_list[o_ptr->art_num];

		/* Remember this artifact from now on */
		a_l_ptr->was_fully_identified = TRUE;
	}

	/* Hack -- Browse book, then prompt for a command */
	if (o_ptr->tval == cp_ptr->spell_book)
	{
		/* Call the aux function */
		get_spell_menu(o_ptr, BOOK_BROWSE);
	}
}



/*
 * Flee the store when it overflows.
 */
static bool store_overflow(void)
{
	int item = INVEN_PACK;

	object_type *o_ptr = &inventory[item];

	p_ptr->redraw |= (PR_ITEMLIST);

	/* Flee from the store */
	if (current_store() != STORE_HOME)
	{
		/* Leave */
		msg_print("Your pack is so full that you flee the store...");
		return TRUE;
	}

	/* Flee from the home */
	else if (!store_check_num(current_store(), o_ptr))
	{
		/* Leave */
		msg_print("Your pack is so full that you flee your home...");
		return TRUE;
	}

	/* Drop items into the home */
	else
	{
		object_type *i_ptr;
		object_type object_type_body;

		char o_name[80];


		/* Give a message */
		msg_print("Your pack overflows!");

		/* Get local object */
		i_ptr = &object_type_body;

		/* Grab a copy of the object */
		object_copy(i_ptr, o_ptr);

		/* Describe it */
		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

		/* Message */
		msg_format("You drop %s (%c).", o_name, index_to_label(item));

		/* Remove it from the players inventory */
		inven_item_increase(item, -255);
		inven_item_describe(item);
		inven_item_optimize(item);

		/* Handle stuff */
		handle_stuff();

		/* Let the home carry it */
		home_carry(i_ptr);
	}

	return FALSE;
}

/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands in the stores
 * which are not allowed in the dungeon, and we must disable some commands
 * which are allowed in the dungeon but not in the stores, to prevent chaos.
 */
static bool store_process_command(char cmd, void *db, int oid)
{
	bool equip_toggle = FALSE;
	bool redraw = FALSE;
	bool command_processed = FALSE;

	/* Parse the command */
	switch (cmd)
	{
		/* Leave */
		case ESCAPE:
		{
			command_processed = TRUE;
			break;
		}

		/* Sell */
		case 's':
		case 'd':
		{

			command_processed = store_sell();
			if (command_processed)
			{
				redraw = TRUE;
				/* Changing the inventory usually changes the frame. */
				store_flags |= (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE);
			}

			break;
		}

		/* Buy */
		case 'p':
		case 'g':
		{
			/* On successful purchase, redraw */
			command_processed = store_purchase(oid);
			if (command_processed)
			{
				redraw = TRUE;
				/* Changing the inventory usually changes the frame. */
				store_flags |= (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE);

			}
			break;
		}

		/* Examine */
		case 'l':
		case 'x':
		{
			store_examine(oid);
			break;
		}

		/* Redraw */
		case KTRL('R'):
		{
			Term_clear();
			store_flags |= (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE);
			command_processed = TRUE;
			break;
		}


		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			textui_cmd_wield();
			redraw = TRUE;
			command_processed = TRUE;

			break;
		}

		/* Take off equipment */
		case 'T':
		case 't':
		{
			textui_cmd_takeoff();
			redraw = TRUE;
			command_processed = TRUE;

			break;
		}

		/* Destroy an item */
		case KTRL('D'):
		case 'k':
		{
			textui_cmd_destroy();
			redraw = TRUE;
			command_processed = TRUE;

			break;
		}

		/* Equipment and inventory list */
		case 'e':
		case 'i':
		{
			/* Handle equipment command */
			if (cmd == 'e') equip_toggle = TRUE;

			/* Display the right thing until the user escapes */
			do
			{
				if (equip_toggle) do_cmd_equip();
				else do_cmd_inven();

				/* Toggle the toggle */
				equip_toggle = !equip_toggle;

			} while (p_ptr->command_new == '/' || p_ptr->command_new == 'e' ||
			         p_ptr->command_new == 'i');

			/* Legal inventory commands are drop, inspect */
			if (!strchr("dsI", p_ptr->command_new))
				p_ptr->command_new = 0;

			break;
		}


		/*** Various commands ***/

		/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}



		/*** Use various objects ***/

		/* Browse a book */
		case 'P':
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Inscribe an object */
		case '{':
		{
			textui_cmd_inscribe();
			redraw = TRUE;
			command_processed = TRUE;

			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			textui_cmd_uninscribe();
			redraw = TRUE;
			command_processed = TRUE;

			break;
		}


		/*** Help and Such ***/

		/* Character description */
		case 'C':
		{
			do_cmd_change_name();
			break;
		}

		case '?':
		{
			/* Toggle help */
			if (store_flags & STORE_SHOW_HELP)
				store_flags &= ~(STORE_SHOW_HELP);
			else
				store_flags |= STORE_SHOW_HELP;

			/* Redisplay */
			store_flags |= STORE_INIT_CHANGE;
			redraw = TRUE;
			command_processed = TRUE;
			break;
		}

		/*** System Commands ***/

		/* Interact with options */
		case '=':
		{
			do_cmd_options();
			redraw = TRUE;

			break;
		}


		/*** Misc Commands ***/

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

		/* Check knowledge */
		case '~':
		{
			do_cmd_knowledge();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}
	}

	/* Let the game handle any core commands (equipping, etc) */
	process_command(CMD_STORE, TRUE);

	if (redraw)
	{
		command_processed = TRUE;

		store_updates();
		notice_stuff();
		handle_stuff();

		event_signal(EVENT_INVENTORY);
		event_signal(EVENT_EQUIPMENT);

	}

	return command_processed;
}


/*
 * Enter a store, and interact with it.
 */
void do_cmd_store(cmd_code code, cmd_arg args[])
{
	bool leave = FALSE;

	/* Check for outstanding rewards */
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Take note of the store number from the terrain feature */
	int this_store = current_store();

	/* Verify that there is a store */
	if (this_store == STORE_NONE)
	{
		msg_print("You see no store here.");
		return;
	}

	/* Check if we can enter the store */
	if (adult_no_stores)
	{
		msg_print("The doors are locked.");
		return;
	}

	/* See if we are holding a quest item */
	if (this_store == STORE_GUILD)
	{


		if ((q_ptr->q_type == QUEST_VAULT) && (!guild_quest_complete()))
		{
			/* The artifact has been returned, the quest is a success */
			if (quest_item_slot() > -1) quest_finished(q_ptr);
		}

		if ((quest_type_collection(q_ptr)) && (!guild_quest_complete()))
		{
			if (quest_item_count() >= quest_collection_num(q_ptr)) quest_finished(q_ptr);
		}
	}

	/*
	 * Quests and services are re-counted
	 * each time a person enters the store
	 * and worked into the store's inventory
	 */
	init_services_and_quests(this_store);

	/*
	 * Shut down the normal game view - it won't be updated - and start
	 * up the store state.
	 */
	event_signal(EVENT_LEAVE_GAME);
	event_signal(EVENT_ENTER_STORE);
	event_signal(EVENT_INIT_STATUSLINE);

	/* Forget the view */
	forget_view();

	/* Reset the command variables */
	p_ptr->command_arg = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_new = 0;

	/*** Display ***/

	/* Save current screen (ie. dungeon) */
	screen_save();

	/*** Inventory display ***/
	{

		static region items_region = { 1, 4, -1, -1 };
		static const menu_iter store_menu = { NULL, NULL, store_display_entry, store_process_command };
		const menu_iter *cur_menu = &store_menu;

		menu_type menu;
		ui_event_data evt = EVENT_EMPTY;
		int cursor = 0;

		store_type *st_ptr = &store[this_store];

		/* Wipe the menu and set it up */
		WIPE(&menu, menu);
		menu.flags = MN_DBL_TAP;

		/* Calculate the positions of things and redraw */
		store_flags = STORE_INIT_CHANGE;
		store_display_recalc(this_store);
		store_redraw();

		/* Objects in inventory may have been re-drawn when entering the store */
		notice_stuff();
		handle_stuff();

		/* Say a friendly hello. */
		if (this_store != STORE_HOME)
		{
			if (this_store == STORE_GUILD)
			{
				prt_welcome_guild();
			}
			else prt_welcome(store_owner(this_store));
		}

		/* Loop */
		while (!leave)
		{
			/* add the store buttons */
			button_kill_all();
			button_add("[HELP]", '?');
			if (this_store == STORE_HOME)
			{
				button_add("[GET]", 'p');
				button_add("[DROP]", 's');
			}
			else if (this_store == STORE_GUILD)
			{
				button_add("[SELECT]", 'p');
			}
			else
			{
				button_add("[BUY]", 'p');
				button_add("[SELL]", 's');
			}
			button_add("[EXAMINE]", 'x');
			button_add("[LEAVE]", ESCAPE);
			event_signal(EVENT_MOUSEBUTTONS);

			/* As many rows in the menus as there are items in the store */
			if ((this_store == STORE_GUILD) && (!guild_quest_complete())) menu.count = quests_max;
			else menu.count = st_ptr->stock_num + quests_max;

			/* Roguelike */
			if (rogue_like_commands)
			{
				/* These two can't intersect! */
				menu.cmd_keys = "\n\x04\x10\r?={}~CEIPTdegilpswx\x8B\x8C"; /* \x10 = ^p , \x04 = ^D */
				menu.selections = "abcfmnoqrtuvyz13456790ABDFGHJKLMNO";
			}

			/* Original */
			else
			{
				/* These two can't intersect! */
				menu.cmd_keys = "\n\x010\r?={}~CEIbdegiklpstw\x8B\x8C"; /* \x10 = ^p */
				menu.selections = "acfhmnoqruvyz13456790ABDFGHJKLMNO";
			}

			/* Keep the cursor in range of the stock */
			if (cursor < 0 || cursor >= menu.count)
				cursor = menu.count - 1;

			items_region.page_rows = scr_places_y[LOC_MORE] - scr_places_y[LOC_ITEMS_START];

			/* Init the menu structure */
			menu_init(&menu, MN_SKIN_SCROLL, cur_menu, &items_region);

			if (menu.count > items_region.page_rows)
				menu.prompt = "  -more-";
			else
				menu.prompt = NULL;

			menu_layout(&menu, &menu.boundary);

			evt.type = EVT_MOVE;

			/* Get a selection/action */
			while (evt.type == EVT_MOVE)
			{
				evt = menu_select(&menu, &cursor, EVT_MOVE);
			}
			if (evt.key == ESCAPE || evt.type == EVT_BACK)
			{
				leave = TRUE;
			}
			/* Handle buttons */
			else if (evt.type == EVT_BUTTON)
			{
				store_process_command(evt.key, FALSE, cursor);

				/* Display the store */
				store_display_recalc(this_store);
				store_redraw();
			}
			else if (evt.type == EVT_RESIZE)
			{
				/* Resize event */
				store_display_recalc(this_store);
				store_redraw();
			}
			else
			{

				/* Display the store */
				store_display_recalc(this_store);
				store_redraw();

				/* Notice and handle stuff */
				notice_stuff();
				handle_stuff();

				/* XXX Pack Overflow */
				if (inventory[INVEN_MAX_PACK].k_idx)
					leave = store_overflow();
			}

			/* Clear all current messages */
			msg_flag = FALSE;
		}

	}

	/* Switch back to the normal game view. */
	event_signal(EVENT_REMOVE_STATUSLINE);
	event_signal(EVENT_LEAVE_STORE);
	event_signal(EVENT_ENTER_GAME);

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Hack -- Cancel automatic command */
	p_ptr->command_new = 0;

	/* Flush messages XXX XXX XXX */
	message_flush();

	/* Load the screen */
	screen_load();

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* restore the buttons */
	basic_buttons();
}
