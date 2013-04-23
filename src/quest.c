/* File: quest.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * This file contains functions used for handling quests by Eytan Zweig
 */

#include "angband.h"

#define QUEST_SLOT_WIELD	0
#define QUEST_SLOT_BOW		1
#define QUEST_SLOT_RING		2
#define QUEST_SLOT_AMULET	3
#define QUEST_SLOT_LITE		4
#define QUEST_SLOT_BODY		5
#define QUEST_SLOT_CLOAK	6
#define QUEST_SLOT_SHIELD	7
#define QUEST_SLOT_HEAD		8
#define QUEST_SLOT_HANDS	9
#define QUEST_SLOT_BOOTS	10
#define QUEST_SLOT_NUM 		11
#define INVEN_NOT_EQUIPMENT QUEST_SLOT_NUM + 1


/*slots for the following tables*/
/*The code below assumes QUEST_SLOT_MONSTER is first and always available to the player*/
#define QUEST_SLOT_MONSTER		0
#define QUEST_SLOT_PIT_NEST		1
#define QUEST_SLOT_LEVEL		2
#define QUEST_SLOT_VAULT		3
#define QUEST_SLOT_MAX			4

/*Monsters only appear so deep*/
#define MAX_MIN_DEPTH			76

/* Quest Titles*/
static cptr quest_title[QUEST_SLOT_MAX] =
{
	"Monster or Unique Quest",	/*QUEST_MONSTER*/
	"Pit or Nest Quest",		/*QUEST_PIT*/
  	"Level Quest",				/*QUEST_LEVEL*/
	"Vault Quest"				/*QUEST_VAULT*/
};

/* Quest Titles*/
static byte quest_title_color[QUEST_SLOT_MAX] =
{
 	TERM_BLUE,			/*QUEST_MONSTER*/
 	TERM_VIOLET,		/*QUEST_VAULT*/
 	TERM_ORANGE,		/*QUEST_PIT*/
 	TERM_L_RED			/*QUEST_CLEAR_THEMED_LEVEL*/
};

/*
 * Determine which classification a quest item is.
 * This is slightly different than inven_wield because the two ring slots count as one.
 */
static s16b quest_equip_slot(const object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (QUEST_SLOT_WIELD);
		}

		case TV_BOW:
		{
			return (QUEST_SLOT_BOW);
		}

		case TV_RING:
		{
			return (QUEST_SLOT_RING);
		}

		case TV_AMULET:
		{
			return (QUEST_SLOT_AMULET);
		}

		case TV_LITE:
		{
			return (QUEST_SLOT_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			return (QUEST_SLOT_BODY);
		}

		case TV_CLOAK:
		{
			return (QUEST_SLOT_CLOAK);
		}

		case TV_SHIELD:
		case TV_DRAG_SHIELD:
		{
			return (QUEST_SLOT_SHIELD);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			return (QUEST_SLOT_HEAD);
		}

		case TV_GLOVES:
		{
			return (QUEST_SLOT_HANDS);
		}

		case TV_BOOTS:
		{
			return (QUEST_SLOT_BOOTS);
		}

		/*not a wieldable item*/
		default:
		{
			return (INVEN_NOT_EQUIPMENT);
		}
	}
}

/*
 * Mega-hack - Fix plural names of monsters
 *
 * Taken from PernAngband via EY, modified to fit NPP monster list
 */
void plural_aux(char *name, size_t max)
{
	int name_len = strlen(name);

	if (strstr(name, " of "))
	{
		cptr aider = strstr(name, " of ");
		char dummy[80];
		int i = 0;
		cptr ctr = name;

		while (ctr < aider)
		{
			dummy[i] = *ctr;
			ctr++;
			i++;
		}

		if (dummy[i - 1] == 's')
		{
			strcpy (&(dummy[i]), "es");
			i++;
		}
		else
		{
			strcpy (&(dummy[i]), "s");
		}

		strcpy(&(dummy[i + 1]), aider);
		my_strcpy(name, dummy, max);
	}
	else if ((strstr(name, "coins")) || (strstr(name, "gems")))
	{
		char dummy[80];
		strcpy (dummy, "Piles of c");
		my_strcat (dummy, &(name[1]), sizeof(dummy));
		my_strcpy (name, dummy, max);
		return;
	}
	else if ((strstr(name, "Manes")) || (name[name_len-1] == 'u') || (strstr(name, "Yeti")) ||
		(streq(&(name[name_len-2]), "ua")) || (streq(&(name[name_len-3]), "nee")) ||
		(streq(&(name[name_len-4]), "idhe")))
	{
		return;
	}
	else if (name[name_len-1] == 'y')
	{
		strcpy(&(name[name_len - 1]), "ies");
	}
	else if (streq(&(name[name_len - 4]), "ouse"))
	{
		strcpy (&(name[name_len - 4]), "ice");
	}
	else if (streq(&(name[name_len - 4]), "lung"))
	{
		strcpy (&(name[name_len - 4]), "lungen");
	}
	else if (streq(&(name[name_len - 3]), "sus"))
	{
		strcpy (&(name[name_len - 3]), "si");
	}
	else if (streq(&(name[name_len - 4]), "star"))
	{
		strcpy (&(name[name_len - 4]), "stari");
	}
	else if (streq(&(name[name_len - 3]), "aia"))
	{
		strcpy (&(name[name_len - 3]), "aiar");
	}
	else if (streq(&(name[name_len - 3]), "inu"))
	{
		strcpy (&(name[name_len - 3]), "inur");
	}
	else if (streq(&(name[name_len - 5]), "culus"))
	{
		strcpy (&(name[name_len - 5]), "culi");
	}
	else if (streq(&(name[name_len - 4]), "sman"))
	{
		strcpy (&(name[name_len - 4]), "smen");
	}
	else if (streq(&(name[name_len - 4]), "lman"))
	{
		strcpy (&(name[name_len - 4]), "lmen");
	}
	else if (streq(&(name[name_len - 2]), "ex"))
	{
		strcpy (&(name[name_len - 2]), "ices");
	}
	else if ((name[name_len - 1] == 'f') && (!streq(&(name[name_len - 2]), "ff")))
	{
		strcpy (&(name[name_len - 1]), "ves");
	}
	else if ((streq(&(name[name_len - 2]), "ch")) || (name[name_len - 1] == 's'))
	{
		strcpy (&(name[name_len]), "es");
	}
	else
	{
		strcpy (&(name[name_len]), "s");
	}
}

/*
 * Provide a description of the quest
 */
void describe_quest(char *buf, size_t max, s16b level, int mode)
{
	int q_idx = quest_num(level);
	char name[80];
	char intro[120];
	char targets[80];
	char where[80];
	char race_name[80];
	char what[25];

	quest_type *q_ptr = &q_info[q_idx];
	monster_race *r_ptr = &r_info[q_ptr->mon_idx];

	/*Paranoia*/
	if (!p_ptr->cur_quest > 0)
	{
		my_strcpy(buf, "None", max);

		return;
	}

	/* Vault quests */
	if (q_ptr->type == QUEST_VAULT)
	{
		object_type *i_ptr;
		object_type object_type_body;
		char o_name[80];

		/* Get local object */
		i_ptr = &object_type_body;

		/*create a "mock" quest artifact*/
		create_quest_artifact(i_ptr);

		/* Get a shorter description to fit the notes file */
		object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

		my_strcpy(intro, format("Retrieve the %s from a vault", o_name),
					sizeof(intro));

		/* The location of the quest */
		if (!depth_in_feet) my_strcpy(where, format("on dungeon level %d and return to the Guild.", level), sizeof(where));
		else my_strcpy(where, format("at a depth of %d feet and return to the Guild.", level * 50), sizeof(where));

		if (mode == QMODE_SHORT) my_strcpy(buf, format("%s.", intro), max);
		else if (mode == QMODE_FULL) my_strcpy(buf, format("%s %s", intro, where), max);
		else if (mode == QMODE_HALF_1) my_strcpy(buf, format("%s", intro), max);
		else if (mode == QMODE_HALF_2) my_strcpy(buf, format("%s", where), max);

		/*we are done*/
		return;
	}

	/* Vault, pit or nest quests */
	if  ((q_ptr->type == QUEST_THEMED_LEVEL) ||
		 (q_ptr->type == QUEST_PIT) ||
		 (q_ptr->type == QUEST_NEST))
	{
		/*print out a message about a themed level*/
		char mon_theme[80];

		my_strcpy(intro, "Clear out ", sizeof(intro));

		/*If the middle of a quest, give the number remaining*/
		if (q_ptr->started)
		{
		    int remaining = q_ptr->max_num - q_ptr->cur_num;

			if (remaining > 1)
			{
				my_strcpy(intro, format("%d remaining creatures from ", remaining),
								sizeof(intro));
			}
			else my_strcpy(intro, "There is one remaining creature from ", sizeof(intro));
		}

		my_strcpy(mon_theme, feeling_themed_level[q_ptr->theme], sizeof(mon_theme));
		if (is_a_vowel(mon_theme[0])) my_strcat(intro, "an ", sizeof(intro));
		else my_strcat(intro, "a ", sizeof(intro));

		if (q_ptr->type ==  QUEST_THEMED_LEVEL)
		{
			my_strcat(intro, format("%s stronghold", mon_theme), sizeof(intro));
		}
		else if (q_ptr->type == QUEST_PIT)
		{
			my_strcat(intro, format("%s pit", mon_theme), sizeof(intro));
		}
		else if (q_ptr->type == QUEST_NEST)
		{
			my_strcat(intro, format("%s nest", mon_theme), sizeof(intro));
		}

		/* The location of the quest */
		if (!depth_in_feet) my_strcpy(where, format("on dungeon level %d.", level), sizeof(where));
		else my_strcpy(where, format("at a depth of %d feet.", level * 50), sizeof(where));

		if (mode == QMODE_SHORT) my_strcpy(buf, format("%s.", intro), max);
		else if (mode == QMODE_FULL) my_strcpy(buf, format("%s %s", intro, where), max);
		else if (mode == QMODE_HALF_1) my_strcpy(buf, format("%s", intro), max);
		else if (mode == QMODE_HALF_2) my_strcpy(buf, format("%s", where), max);

		/*we are done*/
		return;
	}

	/* Not a vault quest, so get the monster race name (singular)*/
	monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

	if ((q_ptr->type == QUEST_UNIQUE) || (q_ptr->type == QUEST_FIXED_U))
	{

		/* Monster quests */
		my_strcpy(targets, race_name, sizeof(targets));
		my_strcpy(intro, "To fulfill your task, you must", sizeof(intro));
	}
	else
	{
		/* Monster quests */
		my_strcpy(name, race_name, sizeof(name));

		/* Multiple quest monsters */
		if ((q_ptr->max_num - q_ptr->cur_num) > 1)
		{
			plural_aux(name, sizeof(name));
			my_strcpy(targets,
				format("%d %s",(q_ptr->max_num - q_ptr->cur_num), name), sizeof(targets));
		}

		/* One quest monster */
		else
		{
			if (is_a_vowel(name[0])) my_strcpy(targets, format("an %s", name), sizeof(targets));
			else my_strcpy(targets, format("a %s", name), sizeof(targets));
		}
	}

	if (monster_nonliving(r_ptr)) my_strcpy(what, "destroy", sizeof(what));
	else my_strcpy(what, "kill", sizeof(what));

	/* The type of the quest */
	if (q_ptr->type == QUEST_FIXED) my_strcpy(intro, "For eternal glory, you must", sizeof(intro));
	else if (q_ptr->type == QUEST_FIXED_U) my_strcpy(intro, "For eternal glory, you must", sizeof(intro));
	else if (q_ptr->type == QUEST_MONSTER) my_strcpy(intro, "To fulfill your task, you must", sizeof(intro));

	/* The location of the quest */
	if (!depth_in_feet) my_strcpy(where, format("on dungeon level %d.", level), sizeof(where));
	else my_strcpy(where, format("at a depth of %d feet.", level * 50), sizeof(where));

	/* Output */
	if (mode == QMODE_SHORT) my_strcpy(buf, format("%s %s %s.", intro, what, targets), max);
	else if (mode == QMODE_FULL) my_strcpy(buf, format("%s %s %s %s", intro, what, targets, where), max);
	else if (mode == QMODE_HALF_1) my_strcpy(buf, format("%s %s %s", intro, what, targets), max);
	else if (mode == QMODE_HALF_2) my_strcpy(buf, format("%s", where), max);

	/* We are done */
	return;
}

/*
 * Give a reward to the player
 *
 * Hack - The entire "tailored item" routine makes quite a few assumptions
 * as to what's a good reward, plus it relies a lot of item price to make
 * deductions. It is basically a list of special cases.
 */
static void grant_reward_gold(void)
{
	int i;

	char o_name[80];

	int repeats;

	object_type *i_ptr;
 	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/*ugly, but effective hack - make extra gold, prevent chests & allow artifacts*/
	object_generation_mode = OB_GEN_MODE_QUEST;

    /*base amount of gold on fame*/
	repeats = p_ptr->fame / 4;

	/*but put in a minimum*/
	if (repeats < 3) repeats = 4;

	/* Give a good gold type for the level */
	for (i = 0; i < repeats; i++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make some gold */
		while (!make_gold(i_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

		/* Message */
		msg_format("You have been rewarded with %ld gold pieces.",
			           (long)i_ptr->pval);
		message_flush();

		/* Collect the gold */
		p_ptr->au += i_ptr->pval;

	}

	/* Redraw gold */
	p_ptr->redraw |= (PR_GOLD);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/*undo special item level creation*/
	object_generation_mode = OB_GEN_MODE_NORMAL;

	return;
}

/*
 * Choose a reward theme.  The lite slot is never returned.  The amulet slot
 * is never returned either, because that is the same theme as a ring.
 */

static int get_reward_theme(void)
{
	while (TRUE)
	{
		/*pick an inventory slot at random*/
		int i = rand_int(QUEST_SLOT_NUM);

		/* Pick a theme  */
		switch (i)
		{
			case QUEST_SLOT_WIELD: 	return(DROP_TYPE_WEAPON);
			case QUEST_SLOT_BOW: 	return(DROP_TYPE_BOW);
			case QUEST_SLOT_RING:	return(DROP_TYPE_JEWELRY);
			case QUEST_SLOT_BODY:	return(DROP_TYPE_ARMOR);
			case QUEST_SLOT_CLOAK:	return(DROP_TYPE_CLOAK);
			case QUEST_SLOT_SHIELD:	return(DROP_TYPE_SHIELD);
			case QUEST_SLOT_HEAD:	return(DROP_TYPE_HEADGEAR);
			case QUEST_SLOT_HANDS:	return(DROP_TYPE_GLOVES);
			case QUEST_SLOT_BOOTS:	return(DROP_TYPE_BOOTS);

			/*amulets (covered by jewelry), or lite source aren't used*/
			default: continue;
		}
	}

	/*This line will never be reached, but the processors give warnings if it isn't here*/
	return(0);
}

/*Get the player title, based on the current player fame*/
void get_title(char *buf, size_t max)
{

	/* Player's title */
	if (p_ptr->fame > 110) my_strcpy(buf, "oh glorious one", max);
	else if (p_ptr->fame > 80) my_strcpy(buf, "oh great one", max);
	else if (p_ptr->fame > 50)
	{
		if (p_ptr->psex == SEX_MALE) my_strcpy(buf, "my lord", max);
		else my_strcpy(buf, "my lady", max);
	}
	else if (p_ptr->fame > 25)
	{
		if (p_ptr->psex == SEX_MALE) my_strcpy(buf, "sir", max);
		else my_strcpy(buf, "madam", max);
	}
	else if (p_ptr->fame > 10)
	{
		if (!op_ptr->full_name[0])
		{
			my_strcpy(buf, (c_name + cp_ptr->name), max);
		}
		else my_strcpy(buf, op_ptr->full_name, max);
	}
	else my_strcpy(buf, (p_name + rp_ptr->name), max);

	return;
}

/*
 * Give a reward to the player
 *
 * Hack - The entire "tailored item" routine makes quite a few assumptions
 * as to what's a good reward, plus it relies a lot of item price to make
 * deductions. It is basically a list of special cases.
 */
static void grant_reward_object(byte type)
{
	int i, x;

	char o_name[80];

	bool got_item = FALSE;

	int repeats;

	byte artifact_marker = 0;

	/* Check for outstanding rewards */
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	object_type *i_ptr;
   	object_type *j_ptr;
 	object_type object_type_body;
	store_type *st_ptr = &store[STORE_HOME];

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Paranoia */
	if (!type) return;

	/*ugly, but effective hack - make extra gold, prevent chests & allow artifacts*/
	object_generation_mode = OB_GEN_MODE_QUEST;

	if ((type == REWARD_RANDART) && (!adult_no_xtra_artifacts))
	{
		s16b k_idx = 0;
		char title[40];

		/*Just make sure we aren't making a ring, amulet, or shovel*/
		while (!k_idx)
		{

			object_kind *k_ptr;
			int theme = get_reward_theme();

			k_idx = 0;

			if (theme == DROP_TYPE_DIGGING) continue;
			if (theme == DROP_TYPE_JEWELRY) continue;

			/*prepare the object generation level for a specific theme*/
			prep_object_theme(theme);

			k_idx = get_obj_num(object_level);

			/*make sure it is a weapon and not too heavy*/
			k_ptr = &k_info[k_idx];

			switch (k_ptr->tval)
			{
				case TV_DIGGING:
				case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:
				{
					if (adj_str_hold[p_ptr->stat_ind[A_STR]] < k_ptr->weight / 10)
					{
						/* Clear restriction */
						get_obj_num_hook = NULL;

						/* Un-do the object theme */
						get_obj_num_prep();

						k_idx = 0;
						continue;
					}
					break;
				}
				default: break;
			}

			/*prepare the object template*/
			object_wipe(i_ptr);

			/* Prepare the object */
			object_prep(i_ptr, k_idx);

			/* Clear restriction */
			get_obj_num_hook = NULL;

			/* Un-do the object theme */
			get_obj_num_prep();

			if (!can_be_randart(i_ptr))
			{

				/*prepare the object template*/
				object_wipe(i_ptr);

				k_idx = 0;
				continue;
			}
		}

		get_title(title, sizeof(title));

		/*Let the player know it is a Randart reward*/
		msg_format("We will attempt to create an artifact in your honor, %s", title);
		message_flush();

		/*actually create the Randart, mark it if so, if not, make a tailored reward*/
		if (make_one_randart(i_ptr, ((p_ptr->fame + q_ptr->base_level) / 2), TRUE))
		{
			got_item = TRUE;
		}
		else
		{
			msg_format("We were unable to create this artifact, %s", title);
			type = REWARD_TAILORED;

			/*prepare the object template*/
			object_wipe(i_ptr);
		}

	}

	/* Try a couple things with a tailored reward */
	if (type == REWARD_TAILORED)
	{

		/*
		 * First, maybe we want to give a spellbook to a spellcaster.
		 * Hack - greater chance if a "pure" spellcaster.
		 */
		if ((cp_ptr->spell_book) &&
			(rand_int(100) < (cp_ptr->flags & CF_ZERO_FAIL ? 40 : 25)))
		{
			/*200 tries at a book*/
			for (i = 0; i < 200; i++)
			{
				bool already_own = FALSE;

				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				/* Valid item exists? */
				if (cp_ptr->spell_book == TV_MAGIC_BOOK)
				{
					/*try to make a spellbook, frequently returns nothing*/
					if (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_DUNGEON_MAGIC_BOOK)) continue;

				}

				if (cp_ptr->spell_book == TV_PRAYER_BOOK)
				{
					/*try to make a spellbook, frequently returns nothing*/
					if (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_DUNGEON_PRAYER_BOOK)) continue;
				}

				/* Was this already a reward (marked tried) */
				if (k_info[i_ptr->k_idx].tried) continue;

				/* Look for item in the pack */
				for (x = 0; x < INVEN_PACK; x++)
				{
					/* Get the item */
					j_ptr = &inventory[x];

					/* Nothing here */
					if (!j_ptr->k_idx) continue;

					/*Is it the same spellbook?*/
					if ((j_ptr->tval == i_ptr->tval) && (j_ptr->sval == i_ptr->sval))
					{
						already_own = TRUE;
					}
				}

				/* Look for item in home */
				if (st_ptr->stock_num)
				{
					for (x = 0; x < st_ptr->stock_num; x++)
					{
						j_ptr = &st_ptr->stock[x];

						/* Nothing here */
						if (!j_ptr->k_idx) continue;

						if ((j_ptr->tval == i_ptr->tval) && (j_ptr->sval == i_ptr->sval))
							already_own = TRUE;
					}
 				}

				/* Hack - Mark as tried */
				k_info[i_ptr->k_idx].tried = TRUE;

				/*we already have a book*/
				if (already_own) continue;

				/*we found a book we can use as a reward*/
				got_item = TRUE;

				break;
			}
		}

		/* Maybe we want to give a potion of augmentation */
		if (!got_item)
		{
			/*clear the counter*/
			x = 0;

			/* Add up the stats indexes over 13*/
			for (i = 0; i < A_MAX; i++)
			{
				x += (MAX(0, p_ptr->stat_ind[i] - 10));
			}

			/*We only want to give potion if stats are low.*/
			if ((rand_int(36) + 36) > x)
			{
				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				/* Make a potion of augmentation */
				object_prep(i_ptr, lookup_kind(TV_POTION, SV_POTION_AUGMENTATION));

				got_item = TRUE;
			}
		}
	}

	/* We didn't find anything else, so lets find something to wear */
	if (!got_item)
	{
		s32b price[QUEST_SLOT_NUM];
		s32b val[QUEST_SLOT_NUM];
		s32b diff = 1;
		s32b object_price;
		s16b inven_quest_slot;
		s32b biggest_price = 0;

		st_ptr = &store[STORE_HOME];

		/*first, start everything at 0*/
		for (i = 0; i < QUEST_SLOT_NUM; i++)
		{
			price[i] = 0;
			val[i] = 0;
		}

		/*
		 * If a tailored reward, factor in the quality of equipment
		 * the player already has.
		 */
		if (type == REWARD_TAILORED)
		{

			/* First, figure out the best price for each slot
			 * Notice we are doing the inventory and equipment in the same loop.
			 */
			for (i = 0; i < INVEN_TOTAL; i++)
			{

				/* First, the item actually in the slot */
				j_ptr = &inventory[i];

				/*nothing there*/
				if (!(j_ptr->k_idx)) continue;

				/*get the quest category of equipment/inventory*/
				inven_quest_slot = quest_equip_slot(j_ptr);

				/*not a piece of equipment*/
				if (inven_quest_slot == INVEN_NOT_EQUIPMENT) continue;

				/*get the value*/
				object_price = object_value(j_ptr);

				/* Compare prices */
				if (object_price > price[inven_quest_slot])
				{
					price[inven_quest_slot] = object_price;
				}
			}

			/* Look for items in the home, if there is anything there */
			if (st_ptr->stock_num)
			{
				/*go through each item in the house*/
				for (i = 0; i < st_ptr->stock_num; i++)
				{

					/* Point to the item */
					j_ptr = &st_ptr->stock[i];;

					/*nothing there*/
					if (!(j_ptr->k_idx)) continue;

					/*get the quest category of equipment/inventory*/
					inven_quest_slot = quest_equip_slot(j_ptr);

					/*not a piece of equipment*/
					if (inven_quest_slot == INVEN_NOT_EQUIPMENT) continue;

					/*get the value*/
					object_price = object_value(j_ptr);

					/* Compare prices */
					if (object_price > price[inven_quest_slot])
					{
						price[inven_quest_slot] = object_price;
					}
				}
			}

			/*
			 * Process objects on the floor.
			 * This is so the palyer can't leave a bunch of items
			 * outside on the floor to force a certain theme.
			 */
			for (i = 1; i < o_max; i++)
			{
				/* Point to the item */
				j_ptr = &o_list[i];

				/*nothing there*/
				if (!(j_ptr->k_idx)) continue;

				/* Ignore un-ID'ed items */
				if (!object_known_p(j_ptr)) continue;

				/*get the quest category of equipment/inventory*/
				inven_quest_slot = quest_equip_slot(j_ptr);

				/*not a piece of equipment*/
				if (inven_quest_slot == INVEN_NOT_EQUIPMENT) continue;

				/*get the value*/
				object_price = object_value(j_ptr);

				/* Compare prices */
				if (object_price > price[inven_quest_slot])
				{
					price[inven_quest_slot] = object_price;
				}
			}

		}

		/*
		 * Base number for loops on fame and reward quest type,
		 * but put in a minimum for each one.
		 */
		switch (type)
		{
			case REWARD_TAILORED:
			{
				repeats = p_ptr->fame / 7;
				if (repeats < 3) repeats = 3;
				break;
			}
			case REWARD_GREAT_ITEM:
			{
				repeats = p_ptr->fame / 9;
				if (repeats < 2) repeats = 2;
				break;
			}

			/*good reward*/
			default:
			{
				repeats = p_ptr->fame / 11;
				if (repeats < 1) repeats = 1;
				break;
			}
		}

		/*
		 * X is only increased after certain conditions are met
		 * Do pre-determined # of attempts at an item
	     */
		for (x = 0; x <= repeats;)
		{
			bool do_great = FALSE;
			s16b droptype;

			bool count_object = TRUE;

			/*create the items in the guild....*/
			store_type *st_ptr = &store[STORE_GUILD];

			/* Get the existing object */
			j_ptr = &st_ptr->stock[1];

			/*wipe a randart if it wasn't used*/
			if (artifact_marker)
			{
				/*Don't wipe any normal artifacts!*/
				if (artifact_marker >= z_info->art_norm_max)
				{
					/*make sure it isn't being used before wiping*/
					if (i_ptr->name1 != artifact_marker) artifact_wipe(artifact_marker, FALSE);
				}

				/*clear the artifact marker*/
				artifact_marker = 0;
			}

			/* Now, produce items */
			droptype = get_reward_theme();

			/* Wipe the object */
			object_wipe(j_ptr);

			/*sometimes make good counter true to give 4 artifact rarity rolls*/
			if (type == REWARD_TAILORED)
			{
				if (randint(100) < p_ptr->fame) do_great = TRUE;
			}
			else if (type == REWARD_GREAT_ITEM) do_great = TRUE;

			/* Valid item exists?  If not, don't count it*/
			if (!make_object(j_ptr, do_great, TRUE, droptype)) continue;

			/* Hack -- in case of artifact, mark it as not created yet */
			if (j_ptr->name1)
		 	{
				a_info[j_ptr->name1].cur_num = 0;

				/*marker in case a randart needs to be erased later*/
				artifact_marker = j_ptr->name1;
			}

			/*get the quest category of equipment/inventory*/
			inven_quest_slot = quest_equip_slot(j_ptr);

			/*paranoia*/
			if (inven_quest_slot == INVEN_NOT_EQUIPMENT) continue;

			/* Make sure weapon isn't too heavy */
			if ((inven_quest_slot == QUEST_SLOT_WIELD) &&
				(adj_str_hold[p_ptr->stat_ind[A_STR]] < j_ptr->weight / 10))
		   	{
			    continue;
			}

			/*blessed weapons only for priests*/
			if ((inven_quest_slot == QUEST_SLOT_WIELD) && (cp_ptr->flags & CF_BLESS_WEAPON) &&
				((j_ptr->tval == TV_SWORD) || (j_ptr->tval == TV_POLEARM)) &&
				(!is_blessed(j_ptr)))
			{
				continue;
			}

			/* Make sure gloves won't ruin spellcasting */
			if ((inven_quest_slot == QUEST_SLOT_HANDS) && (cp_ptr->flags & CF_CUMBER_GLOVE))
			{
				u32b f1, f2, f3;

				object_flags(j_ptr, &f1, &f2, &f3);

				/* Limit to legal glove types */
				if (!((f3 & (TR3_FREE_ACT)) || (f1 & (TR1_DEX))))
				{
					continue;
				}
			}

			/* Hack - mark as identified */
			object_known(j_ptr);

			/*get the value*/
			object_price = object_value(j_ptr);

			if (type == REWARD_TAILORED)
			{
				/*
				 * Find the difference between the best item they have for that slot
				 * and the item now.
				 */
				val[inven_quest_slot] = (object_price - price[inven_quest_slot]);

				/* Tailored reward try hard to find something valuable*/
				if (val[inven_quest_slot] <= 0)
				{
					{
						/*don't count it 50% of the time if cheaper*/
						if (!(one_in_(2))) count_object = FALSE;
					}
				}
			}

			/*Good and great rewards just go off straight price*/
			else
			{
				if (object_price < biggest_price)
				{
					x++;
					continue;
				}
			}

			/*We have an item to count */
			if (count_object) x++;

			/*
			 * Best item yet, or coin flip if it is a tie.
			 * This check will be true for tailored rewards only.
			 */
			if ((val[inven_quest_slot] > diff) ||
				((val[inven_quest_slot] == diff) && (one_in_(2))))
			{
				diff = val[inven_quest_slot];

				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				/* Structure Copy */
				object_copy(i_ptr, j_ptr);

				got_item = TRUE;

			}

			/*keep the most expensive item*/
			if (object_price < biggest_price) continue;

			/*
			 * This check is always for a good or great reward, or a tailored
			 * reward where an item hasn't been found more valuable than what
			 * the player already has AND is the most valuable one object the check above.
			 */
			if (diff == 1)
			{
				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				/* Structure Copy */
				object_copy(i_ptr, j_ptr);

				biggest_price = object_price;

				got_item = TRUE;
			}

		}
	}

	/*
	 * Catch_all if we never found anything.  Should never happen.
	 */
	if ((!i_ptr->k_idx) || (got_item == FALSE))
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Valid item exists?  If not, don't count it*/
		while (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_UNTHEMED)) continue;
	}

	/*Don't wipe any normal artifacts!*/
	if (artifact_marker >= z_info->art_norm_max)
	{
		/*make sure it isn't being used before wiping*/
		if (i_ptr->name1 != artifact_marker) artifact_wipe(artifact_marker, FALSE);
	}

	/* Identify it fully */
	object_aware(i_ptr);
	object_known(i_ptr);

	/* Mark the item as fully known */
	i_ptr->ident |= (IDENT_MENTAL);

	/* Handle Artifacts */
	if (i_ptr->name1)
	{
		/*
		 * Artifact might not yet be marked as created (if it was chosen from tailored
		 * rewards), so now it's the right time to mark it
		 */
		a_info[i_ptr->name1].cur_num = 1;

		/* If the item was an artifact, and if the auto-note is selected, write a message. */
   		 if (adult_take_notes)
		{
			int artifact_depth;
    	    char note[120];
			char shorter_desc[100];

			/* Get a shorter description to fit the notes file */
			object_desc(shorter_desc, sizeof(shorter_desc), i_ptr, TRUE, 0);

			/* Build note and write */
    	    sprintf(note, "Reward: %s", shorter_desc);

			/*record the depth where the artifact was created*/
			artifact_depth = i_ptr->xtra1;

    	    do_cmd_note(note, artifact_depth);

			/*mark item creation depth as 0, which will indicate the artifact
			 *has been previously identified.  This prevents an artifact from showing
			 *up on the notes list twice ifit has been previously identified.  JG
			 */
			i_ptr->xtra1 = 0;
		}
	}

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Note that the pack is too full */
	if (!inven_carry_okay(i_ptr))
	{
		msg_format("You have no room in your backpack for %s.", o_name);

		/* Drop the object */
		drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

		/* Inform the player */
		msg_print("Your reward is waiting outside!");

	}

	/* Give it to the player */
	else
	{
		int item_new;

		/* Give it to the player */
		item_new = inven_carry(i_ptr);

		/* Message */
		msg_format("Your reward is %s (%c).", o_name, index_to_label(item_new));
	}

	message_flush();

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/*undo special item level creation*/
	object_generation_mode = FALSE;

}

#define LEV_BOOST 3

/*
 * Actually give the character a quest.
 */
static bool place_mon_quest(int lev)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	int i, chance;

	monster_race *r_ptr;

	int r_idx;

	alloc_entry *table = alloc_race_table;

	long value;

	int total = 0;

	int max_depth, min_depth;
	u32b max_diff, min_diff, max_diff_unique, min_diff_unique;

	/* Monsters can be up to 3 levels out of depth with difficulty 0 */
	max_depth = LEV_BOOST + lev;

	/*assumes lev is always greater than 0*/
	min_depth = lev;

	/*don't make it too easy if the player isn't diving very fast*/
	if (lev < p_ptr->max_lev)
	{
		max_depth += ((p_ptr->max_lev - lev) / 2);
		min_depth += ((p_ptr->max_lev - lev) / 2);
	}

	/*boundry control*/
	if (max_depth > (MAX_DEPTH - 15)) max_depth = MAX_DEPTH - 15;
	if (min_depth < 1) min_depth = 1;
	if (min_depth > MAX_MIN_DEPTH) min_depth = MAX_MIN_DEPTH;

	/*get the average ddifficulty spanning 5 levele for monsters*/
	max_diff = min_diff = min_diff_unique = max_diff_unique = 0;

	/*first get the total of the 5 levels*/
	for (i = 0; i < 5; i++)
	{
		min_diff += mon_power_ave[i + min_depth][CREATURE_NON_UNIQUE];
		min_diff_unique += mon_power_ave[i + min_depth][CREATURE_UNIQUE];

		/*put some boundry control on the highest level*/
		max_diff += mon_power_ave[i + min_depth + LEV_BOOST][CREATURE_NON_UNIQUE];
		max_diff_unique += mon_power_ave[i + min_depth + LEV_BOOST][CREATURE_UNIQUE];
	}

	/*now get the average*/
	min_diff /= 5;
	max_diff /= 5;
	min_diff_unique /= 5;
	max_diff_unique /= 5;

	/*hack - make sure there is a suitable range*/
	while ((min_diff * 10 / 9) > max_diff)
	{
		min_diff = min_diff * 19 / 20;
		max_diff = max_diff * 21 / 20;
	}
	while ((min_diff_unique * 10 / 9) > max_diff_unique)
	{
		min_diff_unique = min_diff_unique * 19 / 20;
		max_diff_unique = max_diff_unique * 21 / 20;
	}

	/*
	 * Process the probabilities
	 */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Default */
		table[i].prob3 = 0;

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/*A little bit of level control to keep out extreme monsters*/

		/*enforce a maximum depth*/
		if (r_ptr->level > max_depth + 5) continue;
		/*enforce a minimum depth*/
		if (r_ptr->level < min_depth - 5) continue;

		/*no player ghosts*/
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

		/* Uniques only for unique quests*/
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/*get the right difficulty*/
			if (r_ptr->mon_power < min_diff_unique) continue;
			if (r_ptr->mon_power > max_diff_unique) continue;

			/* no dead ones*/
			if (r_ptr->cur_num >= r_ptr->max_num) continue;
		}
		/*no uniques for monster quests*/
		else
		{
			/*get the right difficulty*/
			if (r_ptr->mon_power < min_diff) continue;
			if (r_ptr->mon_power > max_diff) continue;

			/*no quests for certain kinds of animals the guild wouldn't care about*/
			/*lets see how this works without this for now
			if ((strchr("BFIJKSabcejlrmw,", r_ptr->d_char)) &&
			(!(r_ptr->flags2 & (RF2_SMART)))) continue;*/

			/* Check if a immobile monster can still hurt the player from a distance */
			if (r_ptr->flags1 & RF1_NEVER_MOVE)
			{
				bool okay = FALSE;

				/* Allow if the monster has at least one summon spell*/
				if (r_ptr->flags7)	okay = TRUE;

				/* Allow if the monster can attack */
				if ((r_ptr->flags4 & (RF4_ATTACK_MASK)) ||
					(r_ptr->flags5 & (RF5_ATTACK_MASK)) ||
					(r_ptr->flags6 & (RF6_ATTACK_MASK)))
					okay = TRUE;

				/* Allow if the monster can bring the player to itself */
				if  (r_ptr->flags6 & (RF6_TELE_TO))
						okay = TRUE;

				if (!okay) continue;
			}

		}

		/* Depth Monsters never appear in quests*/
		if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Total */
		total += table[i].prob3;
	}

	/*no eligible creatures - should never happen*/
	if (!total)
	{
    	/* No monsters - no quest */
		msg_print("There are no eligible creatures to quest for");
		return (FALSE);

	}

	/* Select the monster from eligible monsters */

	/* Pick a monster */
	value = rand_int(total);

	/* Find the monster */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}

	/* Set up the quest */
	q_ptr->mon_idx = table[i].index;
	q_ptr->cur_num = 0;
	q_ptr->started = FALSE;
	q_ptr->base_level = lev;
	q_ptr->active_level = lev;

	/* Get the actual race */
	r_ptr = &r_info[q_ptr->mon_idx];

	/*
	 * This info slightly different depending on if
	 * this is a unique quest or monster quest
	 */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		q_ptr->max_num = 1;
		q_ptr->type	= QUEST_UNIQUE;
	}
	else
	{
		s16b num;

		/*the number of quest monsters is based around 10, depending on monster power*/
		num = ((min_diff + max_diff) * 5) / r_ptr->mon_power;

		/*higher number of monsters based on player fame*/
		num += p_ptr->fame / 10;

		/*boundry control*/
		if (num < 4) num = 4;
		if (num > 60) num = 60;

		/*just a touch of randomness*/
		num += randint(3);

		/*assign the number*/
		q_ptr->max_num = num;

		/*assign the quest type*/
		q_ptr->type	= QUEST_MONSTER;

	}

	/* Set current quest */
	p_ptr->cur_quest = lev;

	/* Chance of gold reward */
	chance = 75 - (p_ptr->fame * 2) - (lev *  2);

	/* Better rewards for unique quests */
	if (r_ptr->flags1 & (RF1_UNIQUE)) chance -= 10;

	if (rand_int(100) < chance) q_ptr->reward = REWARD_GOLD;

	else
	{
		/* Chance of good item reward */
		chance = 95 - (p_ptr->fame * 2) - (lev * 2 / 3);

		/* Better rewards for unique quests */
		if (r_ptr->flags1 & (RF1_UNIQUE)) chance -= 10;

		if (chance < 0) chance = 0;

		if (rand_int(100) < chance) q_ptr->reward = REWARD_GOOD_ITEM;

		/* First roll for tailored award */
		else if (((!adult_no_artifacts) || (!adult_no_xtra_artifacts)) &&
		((65 + damroll(6,15) < p_ptr->fame)) && (one_in_(3)))
			q_ptr->reward = REWARD_RANDART;

		/* First roll for tailored award */
		else if (15 + damroll(6,15) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;

		/* Default - a great item reward */
		else q_ptr->reward = REWARD_GREAT_ITEM;
	}

	/*success*/
	return (TRUE);
}

/*
 * Make sure a quest will be a challenge by making sure we aren't assigning a
 * monster theme where all the monsters are already in depth.
 */
static bool check_pit_nest_depth(int lev, byte theme)
{
	bool return_value = FALSE;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	int i, max_depth;

	alloc_entry *table = alloc_race_table;

	/*set the hook*/
	get_mon_hook(theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	max_depth = lev + PIT_NEST_QUEST_BOOST;

	/*Monsters only go so high*/
	if (lev > MAX_MIN_DEPTH) lev = MAX_MIN_DEPTH;

	/* Go through each one */
	for (i = 0; i < alloc_race_size; i++)
	{
		monster_race *r_ptr;

		int r_idx;

		/* Hack -- No town monsters are acceptable */
		if (table[i].level <= 0) continue;

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/*No player ghosts*/
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

		/*Pits or nests do not allow uniques*/
		if ((q_ptr->type == QUEST_NEST) || (q_ptr->type == QUEST_PIT))
		{
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;
		}

		/* Make sure there is at least one monster out of depth. */
		if ((table[i].level > lev) && (table[i].level < max_depth))
		{
			return_value = TRUE;

			/*No need to continue, we only need one*/
			break;
		}

	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	return (return_value);

}



/*
 * Make sure a quest will be a challenge by making sure we aren't assigning a
 * monster theme where all the monsters are already in depth.
 */
static bool check_theme_depth(int lev, byte theme)
{
	bool return_value = FALSE;

	int i;
	int max_depth, min_depth;
	u32b max_diff, max_diff_unique;

	alloc_entry *table = alloc_race_table;

	/*set the hook*/
	get_mon_hook(theme);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Factor in the boost */
	max_depth = lev + THEMED_LEVEL_QUEST_BOOST;
	min_depth = lev;

	/*don't make it too easy if the player isn't diving very fast*/
	if (p_ptr->depth < p_ptr->max_lev)
	{
		min_depth += ((p_ptr->max_lev - p_ptr->depth) / 2);
		max_depth += ((p_ptr->max_lev - p_ptr->depth) / 2);
	}

	/*boundry control*/
	if (max_depth > (MAX_DEPTH - 15)) max_depth = MAX_DEPTH - 15;
	if (min_depth > MAX_MIN_DEPTH) min_depth = MAX_MIN_DEPTH;

	/*get the average difficulty spanning 5 levels for monsters*/
	max_diff = max_diff_unique = 0;

	/*first get the total of the 5 levels*/
	for (i = 0; i < 5; i++)
	{
		/*put some boundry control on the highest level*/
		max_diff += mon_power_ave[max_depth + i][CREATURE_NON_UNIQUE];
		max_diff_unique += mon_power_ave[max_depth + i][CREATURE_UNIQUE];
	}

	/*now get the average*/
	max_diff /= 5;
	max_diff_unique /= 5;

	/* Go through each one */
	for (i = 0; i < alloc_race_size; i++)
	{
		monster_race *r_ptr;

		int r_idx;

		/* Hack -- No town monsters are acceptable */
		if (table[i].level <= 0) continue;

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/*enforce a maximum depth*/
		if (r_ptr->level > max_depth) continue;

		/*No player ghosts*/
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

		/* Uniques only for unique quests*/
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/*get the right difficulty*/
			if (r_ptr->mon_power > max_diff_unique) continue;
			if (r_ptr->mon_power < mon_power_ave[min_depth][CREATURE_UNIQUE]) continue;

			/* no dead ones*/
			if (r_ptr->cur_num >= r_ptr->max_num) continue;

			/*Make sure it is reasonably hard*/

		}
		/*other monsters based on difficulty*/
		else
		{
			/*get the right difficulty*/
			if (r_ptr->mon_power > max_diff) continue;
			if (r_ptr->mon_power < mon_power_ave[min_depth][CREATURE_NON_UNIQUE]) continue;

		}

		/*Found one*/
		return_value = TRUE;

		/*No need to continue, we only need one*/
		break;

	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	return (return_value);

}


/*
 * Actually give the character a vault quest
 */
static bool place_pit_nest_quest(int lev)
{

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	int tries = 0;
	int i;

	bool checked_theme_yet[LEV_THEME_HEAD];

	/*start with false, meaning we haven't checked it yet*/
	for (i = 0; i < LEV_THEME_HEAD; i++) checked_theme_yet[i] = FALSE;

	/*50% chance of a pit or nest*/
	if one_in_(2) q_ptr->type = QUEST_PIT;
	else q_ptr->type = QUEST_NEST;

	q_ptr->base_level = lev;
	q_ptr->active_level = lev;
	if (((!adult_no_artifacts) || (!adult_no_xtra_artifacts)) &&
		((35 + damroll(5,20) < p_ptr->fame)) && (one_in_(3)))
			q_ptr->reward = REWARD_RANDART;
	else if (25 + damroll(3,20) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;
	else q_ptr->reward = REWARD_GREAT_ITEM;

	while (TRUE)
	{
		/*
		 * We get 5000 tries at this, but it should only fail
		 * if somebody wasn't careful while editing around the monster list.
		 */
		if ((tries++) > 5000) return (FALSE);

		/*Get the actual theme for either the pit or nest*/
		if (q_ptr->type == QUEST_PIT) q_ptr->theme = get_pit_theme(lev + 3);
		else q_ptr->theme = get_nest_theme(lev + 3);

		/*hack - never do jelly quests*/
		if (q_ptr->theme == LEV_THEME_JELLY) continue;

		/*We don't want to run through check_theme_depth a bunch of times*/
		if (checked_theme_yet[q_ptr->theme] == TRUE) continue;

		/*make sure there are hard enough monsters*/
		if (!check_pit_nest_depth(q_ptr->base_level, q_ptr->theme))
		{
			/*mark it as checked*/
			checked_theme_yet[q_ptr->theme] = TRUE;

			/*next try*/
			continue;
		}

		/*found a good theme*/
		break;
	}

	/* Set current quest */
	p_ptr->cur_quest = lev;

	q_ptr->started = FALSE;

	/*success*/
	return (TRUE);
}

/*
 * Actually give the character a vault quest
 */
static bool place_level_quest(int lev)
{

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	int i;
	int tries = 0;

	bool checked_theme_yet[LEV_THEME_HEAD];

	/*start with false, meaning we haven't checked it yet*/
	for (i = 0; i < LEV_THEME_HEAD; i++) checked_theme_yet[i] = FALSE;

	/* Actually write the quest */
	q_ptr->type = QUEST_THEMED_LEVEL;
	q_ptr->base_level = lev;
	q_ptr->active_level = lev;
	if (((!adult_no_artifacts) || (!adult_no_xtra_artifacts)) &&
	    (30 + damroll(5,20) < p_ptr->fame))  q_ptr->reward = REWARD_RANDART;
	else if (20 + damroll(3,10) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;
	else q_ptr->reward = REWARD_GREAT_ITEM;

	while (TRUE)
	{
		/*
		 * We get 5000 tries at this, but it should only fail
		 * if somebody wasn't careful while editing around the monster list.
		 */
		if ((tries++) > 5000) return (FALSE);

		/*Get the actual theme*/
		q_ptr->theme = get_level_theme(lev + THEMED_LEVEL_QUEST_BOOST / 2);

		/*hack - never do jelly quests*/
		if (q_ptr->theme == LEV_THEME_JELLY) continue;

		/*We don't want to run through check_tmeme_depth a bunch of times*/
		if (checked_theme_yet[q_ptr->theme] == TRUE) continue;

		/*make sure there are hard enough monsters*/
		if (!check_theme_depth(lev, q_ptr->theme))
		{
			/*mark it as checked*/
			checked_theme_yet[q_ptr->theme] = TRUE;

			/*next try*/
			continue;
		}

		/*found a good theme*/
		break;

	}

	/* Set current quest */
	p_ptr->cur_quest = lev;

	q_ptr->started = FALSE;

	/*success*/
	return (TRUE);
}

/*
 * Actually give the character a vault quest
 */
static bool place_vault_quest(int lev)
{

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Actually write the quest */
	q_ptr->type = QUEST_VAULT;
	q_ptr->base_level = lev;
	q_ptr->active_level = lev;
	if ((!adult_no_artifacts) &&  (!adult_no_xtra_artifacts) &&
		((50 + damroll(5,20) < p_ptr->fame)) && (one_in_(3)))
			q_ptr->reward = REWARD_RANDART;
	else if (20 + damroll(3,20) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;
	else q_ptr->reward = REWARD_GREAT_ITEM;

	/*generate a quest artifact*/
	make_quest_artifact(lev);

	/* Set current quest */
	p_ptr->cur_quest = lev;

	q_ptr->started = FALSE;

	/*success*/
	return (TRUE);
}

static bool quest_allowed(byte j)
{
	/*not a valid quest type*/
	if (j >= QUEST_SLOT_MAX) return (FALSE);

	/*quest vaults not always offered*/
	if (j == QUEST_SLOT_VAULT)
	{
		if ((p_ptr->fame < 10) || (p_ptr->fame % 5 != 3)) return (FALSE);
	}
	else if (j == QUEST_SLOT_PIT_NEST)
	{
		if (p_ptr->max_depth < 5) return (FALSE);
	}
	else if (j == QUEST_SLOT_LEVEL)
	{
		if (p_ptr->max_depth < 10) return (FALSE);
	}

	/*allowable*/
	return (TRUE);

}

/*
 * Display the "contents" of the adventurer's guild
 */
void display_guild(void)
{
	int j;
	byte attr;
	char q_out[120];
	char title[40];

	bool do_reward;

	/* Check for outstanding rewards */
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	do_reward = FALSE;

	if ((q_ptr->type == QUEST_MONSTER) || (q_ptr->type == QUEST_UNIQUE))
	{
		/*We owe the player a reward*/
		if ((!q_ptr->active_level) && (q_ptr->reward))
		{

			/* Grant fame bonus */
			p_ptr->fame += randint(2);

			do_reward = TRUE;

			/*The note has already been written*/
		}
	}

	else if (q_ptr->type == QUEST_VAULT)
	{
		char note[120];

		object_type *o_ptr;

		/* Find quest item in the inventory*/
		j = quest_item_slot();

		/* We have the quest reward */
		if (j > -1)
		{

			o_ptr = &inventory[j];

			/* Found it */
			do_reward = TRUE;

			/* Grant fame bonus */
			p_ptr->fame += 3;

			/*if using notes file, make a note*/
			if (adult_take_notes)
			{
				char o_name[80];

				/* Get a shorter description to fit the notes file */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

				/*Create the note*/
				sprintf(note, "Returned %s to the Adventurer's Guild.", o_name);

				/*write note*/
 		  		do_cmd_note(note, q_ptr->active_level);
			}

			artifact_wipe(o_ptr->name1, TRUE);

			/* Destroy the quest item in the pack */
			inven_item_increase(j, -255);
			inven_item_optimize(j);
		}
	}

	else if  ((q_ptr->type ==  QUEST_THEMED_LEVEL) || (q_ptr->type == QUEST_PIT) ||
			  (q_ptr->type == QUEST_NEST))
	{
		/*We owe the player a reward*/
		if ((!q_ptr->active_level) && (q_ptr->reward))
		{
			if (q_ptr->type == QUEST_THEMED_LEVEL)	p_ptr->fame += 2;
			else if (q_ptr->type == QUEST_PIT) 		p_ptr->fame += 1;
			else if (q_ptr->type == QUEST_NEST) 	p_ptr->fame += 1;

			/* Slightly random fame bonus for these harder quests*/
			p_ptr->fame += randint(2);

			do_reward = TRUE;

			/*The note has already been written*/
		}
  	}

	/* Reward the player,and wipe the quest */
	if (do_reward)
	{
		/* Generate object at quest level */
		object_level = q_ptr->base_level;

		/* Create the reward*/
		if (q_ptr->reward == REWARD_GOLD) grant_reward_gold();
		else grant_reward_object(q_ptr->reward);

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();

		/* Reset object level */
		object_level = (challenge() * effective_depth(p_ptr->depth)) / 10;

		/* Wipe the quest */
		guild_quest_wipe();

		/* Clear the screen */
		Term_clear();

	}

	/* Describe the guild */
	put_str("The Adventurer's Guild", 3, 30);

	/*Get the title*/
	get_title(title, sizeof(title));

	/* Introduction */
	put_str(format("Welcome to the Adventurer's Guild, %s.", title), 5, 3);

	/* Player's reputation */
	switch (p_ptr->fame / 5)
	{
		case 0:
		{
			attr = TERM_RED;
			my_strcpy(title, "poor", sizeof(title));
			break;
		}
		case 1:
		case 2:
		case 3:
		{
			attr = TERM_YELLOW;
			my_strcpy(title, "fair", sizeof(title));
			break;
		}
		case 4:
		case 5:
		case 6:
		{
			attr = TERM_YELLOW;
			my_strcpy(title, "good", sizeof(title));
			break;
		}
		case 7:
		case 8:
		{
			attr = TERM_YELLOW;
			my_strcpy(title, "very good", sizeof(title));
			break;
		}
		case 9:
		case 10:
		{
			attr = TERM_L_GREEN;
			my_strcpy(title, "excellent", sizeof(title));
			break;
		}
		case 11:
		case 12:
		case 13:
		{
			attr = TERM_L_GREEN;
			my_strcpy(title, "superb", sizeof(title));
			break;
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			attr = TERM_L_GREEN;
			my_strcpy(title, "heroic", sizeof(title));
			break;
		}
		default:
		{
			attr = TERM_L_GREEN;
			my_strcpy(title, "legendary", sizeof(title));
			break;
		}
	}

	/* Quest count */
	put_str("You might be interested to know that your current reputation is", 6, 3);
	c_put_str(attr, title, 6, 67);

	/* Label the quest descriptions */
	if (!p_ptr->cur_quest)
	{
		byte num_quests = 0;

		/* Not Currently in a quest */
		put_str("Available Quests:", 8, 3);

		/*print out the menu of quests*/
		for (j = 0; j < QUEST_SLOT_MAX; j++)
		{
			/*check if the quest is allowable*/
			if (!quest_allowed(j)) continue;

			/*display it*/
			put_str(format("%c)", I2A(num_quests)), 10 + num_quests, 0);
			c_put_str(quest_title_color[j], quest_title[j], 10 + num_quests, 4);
			num_quests++;
		}
	}

	else
	{

		put_str("Your current quest:", 8, 3);

		/* Reset the cursor */
		Term_gotoxy(3, 10);

		/*break quest description into two lines*/
		describe_quest(q_out, sizeof(q_out), p_ptr->cur_quest, QMODE_HALF_1);
		Term_addstr(-1, TERM_WHITE, q_out);

		/*display the monster character if applicable*/
		if ((quest_check(p_ptr->cur_quest) == QUEST_MONSTER) ||
			(quest_check(p_ptr->cur_quest) == QUEST_UNIQUE))
		{
			quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];
			monster_race *r_ptr = &r_info[q_ptr->mon_idx];

			/* Get the char */
			byte a1 = r_ptr->d_attr;
			/* Get the attr */
			char c1 = r_ptr->d_char;

			/* Append the "standard" attr/char info */
			Term_addstr(-1, TERM_WHITE, " ('");
			Term_addch(a1, c1);
			Term_addstr(-1, TERM_WHITE, "')");
		}

		/* Reset the cursor */
		Term_gotoxy(3, 11);
		describe_quest(q_out, sizeof(q_out), p_ptr->cur_quest, QMODE_HALF_2);
		Term_addstr(-1, TERM_WHITE, q_out);
	}
}


/*
 * "Purchase" a quest from the guild
 */
void guild_purchase(void)
{
	int i, qlev;
	byte j, item;
	char prompt[80];
	byte max_quest_choice[QUEST_SLOT_MAX];
	byte num_quests = 0;

	if (p_ptr->cur_quest)
	{
		msg_print("Finish your current quest first!");
		return;
	}

	/*Honor the no quests option*/
	if (adult_no_quests)
	{
		msg_print("Nothing happens!");
		return;
	}

	/* Get level for quest - if never been in dungeon at 50', otherwise 2-3 levels deeper */
	if (!p_ptr->max_depth) qlev = 1;
	else qlev = p_ptr->max_depth + 1 + randint(2);

	/*nowhere to quest*/
	if (qlev >= MAX_DEPTH)
	{
		msg_print("You have completely conquered the dungeon!");
		return;
	}

	/* Make sure there is no fixed quest on the same level of quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* check fixed quests to see that they're not on the same level*/
		if ((q_info[i].type == QUEST_FIXED) || (q_info[i].type == QUEST_FIXED_U))
		{
			if (q_info[i].base_level == qlev)
			{
				msg_print("A greater task lies before you!");
				return;
			}
		}

	}

	/*get a list of allowable quests*/
	for (j = 0;j < QUEST_SLOT_MAX; j++)
	{
		if (quest_allowed(j))
		{

			/*Allow*/
			max_quest_choice[num_quests] = j;
			num_quests++;
		}
	}

	/* Copy the string over */
	my_strcpy(prompt, "Please choose a quest (ESC to cancel):",
				sizeof(prompt));

	/* Get the quest number if a choice is needed, then translate that to the quest type */
	if (num_quests > 1)
	{
		s16b choice;

		/*Prompt the player for a quest choice*/
		choice = get_menu_choice(num_quests, prompt);

		/* Quit if no quest chosen */
		if (choice == -1) return;

		/* Get a choice*/
		item = max_quest_choice[choice];
	}

	/*hack - automatically a monster quest*/
	else item = 0;

	/*place a monster quest*/
	if (item == QUEST_SLOT_MONSTER)
	{
		if (!place_mon_quest(qlev))
		{
			guild_quest_wipe();
			return;
		}
	}
	/*Place a vault quest*/
	else if (item == QUEST_SLOT_VAULT)
	{
		if (!place_vault_quest(qlev))
		{
			guild_quest_wipe();
			return;
		}
	}
	else if (item == QUEST_SLOT_LEVEL)
	{
		if (!place_level_quest(qlev))
		{
			guild_quest_wipe();
			return;
		}
	}
	/*Nest or Pit quests*/
	else if (item == QUEST_SLOT_PIT_NEST)
	{
		if (!place_pit_nest_quest(qlev))
		{
			guild_quest_wipe();
			return;
		}
	}

	/* Clear screen */
	Term_clear();

	display_guild();

	return;
}

/*
 * Hack -- Check if a level is a "quest" level - returns quest type
 */
byte quest_check(int lev)
{
	int i;

	/* Town is never a quest */
	if (!lev) return 0;

	/* Check quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Check for quest */
		if (q_info[i].active_level == lev) return (q_info[i].type);
	}

	/* Nope */
	return 0;
}

/*
 * Return the index of the quest for current level
 */
int quest_num(int lev)
{
	int i;

	/* Town is never a quest */
	if (!lev) return 0;

	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Quest level? */
		if (q_info[i].active_level == lev) return i;
	}

	/* No quest */
	return 0;
}

/*
 * Return the slot of a quest item in the inventory
 */
int quest_item_slot(void)
{
	int i;
	object_type *o_ptr;

	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		if (o_ptr->ident & IDENT_QUEST) return (i);
	}

	/* No quest item */
	return -1;
}

/*
 * Wipe an artifact clean.  Rebuild the names string to delete the name.
 * Check first to make sure it is a randart
 */
void guild_quest_wipe(void)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Wipe the structure */
	(void)WIPE(q_ptr, quest_type);

	p_ptr->cur_quest = 0;

}

/*
 * Fail your quest
 */
void quest_fail(void)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	monster_race *r_ptr = &r_info[q_ptr->mon_idx];
	byte quest;

	/* Message */
	msg_print("You have failed in your quest!");

	/*find out the type of quest*/
	quest = quest_check(q_ptr->base_level);

	/* Reputation penalty */
	if (p_ptr->fame)
	{
		/*special quests cut the player fame in half*/
		if (quest == QUEST_VAULT)	p_ptr->fame /= 2;

		else if ((quest == QUEST_NEST) ||
			     (quest == QUEST_PIT) ||
				 (quest == QUEST_THEMED_LEVEL))	p_ptr->fame = (p_ptr->fame * 6 / 10);

		else
		{
			/* Monster quests */
			p_ptr->fame = (p_ptr->fame * 4 / 10);
		}
	}

	/*make a note of the failed quest */
	if (adult_take_notes)
	{

		char note[120];

		if (quest == QUEST_VAULT)
		{
			object_type *i_ptr;
			object_type object_type_body;
			char o_name[80];

			/* Get local object */
			i_ptr = &object_type_body;

			/*create a "mock" quest artifact*/
			create_quest_artifact(i_ptr);

			/* Get a shorter description to fit the notes file */
			object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

			/*create the note*/
			sprintf(note, "Quest: Failed to return %s to the Guild", o_name);

			/*clear out the artifact*/
			artifact_wipe(QUEST_ART_SLOT, TRUE);
		}

		else if ((quest == QUEST_THEMED_LEVEL) ||
			 	 (quest == QUEST_PIT) ||
				 (quest == QUEST_NEST))
		{
			char mon_theme[80];

			/*Get the theme*/
			my_strcpy(mon_theme, feeling_themed_level[q_ptr->theme], sizeof(mon_theme));

			my_strcpy(note, "Quest: Failed to clear out ", sizeof(note));

			/*make the grammar proper*/
			if (is_a_vowel(mon_theme[0])) my_strcat(note, "an ", sizeof(note));
			else my_strcat(note, "a ", sizeof(note));

			/*dump the monster theme*/
			my_strcat(note, mon_theme, sizeof(note));

			/*Finish off the line*/
			if  (quest == QUEST_THEMED_LEVEL) 	my_strcat(note, " stronghold.", sizeof(note));
			else if (quest == QUEST_PIT)	my_strcat(note, " pit.", sizeof(note));
			else if (quest == QUEST_NEST)	my_strcat(note, " nest.", sizeof(note));

   		}

		else
		{
			char race_name[80];

			/* Get the monster race name (singular)*/
			monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

			/* Multiple quest monsters */
			if (q_ptr->max_num > 1)
			{
				plural_aux(race_name, sizeof(race_name));
			}

			if (q_ptr->type == QUEST_UNIQUE)
			{
				/*write note*/
				if monster_nonliving(r_ptr)
				sprintf(note, "Quest: Failed to destroy %s", race_name);
				else sprintf(note, "Quest: Failed to kill %s", race_name);
			}

			else
			{
				/* Write note */
				if monster_nonliving(r_ptr)
            		sprintf(note, "Failed a quest to destroy %d %s", q_ptr->max_num, race_name);
				else sprintf(note, "Failed a quest to kill %d %s", q_ptr->max_num, race_name);
			}

 		}

		/*write the note*/
		do_cmd_note(note, q_ptr->base_level);

	}

	/*wipe the quest*/
	guild_quest_wipe();

	/* Disturb */
	if (disturb_minor) disturb(0,0);
}
