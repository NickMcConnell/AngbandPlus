/* File: quest.c */

/*
 * Copyright (c) 1997 Eytan Zweig, Jeff Greene, Diego Gonzalez
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
#define QUEST_SLOT_LIGHT	4
#define QUEST_SLOT_BODY		5
#define QUEST_SLOT_CLOAK	6
#define QUEST_SLOT_SHIELD	7
#define QUEST_SLOT_HEAD		8
#define QUEST_SLOT_HANDS	9
#define QUEST_SLOT_BOOTS	10
#define QUEST_SLOT_NUM 		11
#define INVEN_NOT_EQUIPMENT QUEST_SLOT_NUM + 1




/*Monsters only appear so deep*/
#define MAX_MIN_DEPTH			76



/*
 * Return the next guild quest level.
 * Note this function does not handle overflows such as
 * the max_depth of the dungeon.
 */
static int guild_quest_level(void)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	int depth = 0;

	if (p_ptr->cur_quest) return (p_ptr->cur_quest);

	/*
	 * If the player hasn't left town yet, first quest
	 * is at 50 feet..
	 */
	if (!p_ptr->quest_depth) return (2);

	depth = p_ptr->quest_depth + QUEST_LEVEL_BOOST;

	/* Consistently apply either 2 or three levels to the max depth */
	if (q_ptr->q_flags & (QFLAG_EXTRA_LEVEL)) depth ++;

	if (depth > MAX_DEPTH) depth = MAX_DEPTH;

	return (depth);
}

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

		case TV_LIGHT:
		{
			return (QUEST_SLOT_LIGHT);
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

	else if (strstr(name, "Greater Servant of"))
	{
		char dummy[80];
		strcpy (dummy, "Greater Servants of ");
		my_strcat (dummy, &(name[1]), sizeof(dummy));
		my_strcpy (name, dummy, max);
		return;
	}
	else if (strstr(name, "Lesser Servant of"))
	{
		char dummy[80];
		strcpy (dummy, "Greater Servants of ");
		my_strcat (dummy, &(name[1]), sizeof(dummy));
		my_strcpy (name, dummy, max);
		return;
	}
	else if (strstr(name, "Servant of"))
	{
		char dummy[80];
		strcpy (dummy, "Servants of ");
		my_strcat (dummy, &(name[1]), sizeof(dummy));
		my_strcpy (name, dummy, max);
		return;
	}
	else if (strstr(name, "Great Wyrm"))
	{
		char dummy[80];
		strcpy (dummy, "Great Wyrms ");
		my_strcat (dummy, &(name[1]), sizeof(dummy));
		my_strcpy (name, dummy, max);
		return;
	}
	else if (strstr(name, "Spawn of"))
	{
		char dummy[80];
		strcpy (dummy, "Spawn of ");
		my_strcat (dummy, &(name[1]), sizeof(dummy));
		my_strcpy (name, dummy, max);
		return;
	}
	else if (strstr(name, "Descendant of"))
	{
		char dummy[80];
		strcpy (dummy, "Descendant of ");
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
	else if (((streq(&(name[name_len - 2]), "ch")) || (name[name_len - 1] == 's')) &&
			(!streq(&(name[name_len - 5]), "iarch")))
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
	if (p_ptr->cur_quest == 0)
	{
		my_strcpy(buf, "None", max);

		return;
	}

	/* Vault quests */
	if (q_ptr->q_type == QUEST_VAULT)
	{
		object_type *i_ptr;
		object_type object_type_body;
		char o_name[80];

		/* Get local object */
		i_ptr = &object_type_body;

		/*create a "mock" quest artifact*/
		create_quest_artifact(i_ptr);

		/* Get a shorter description to fit the notes file */
		object_desc(o_name, sizeof(o_name), i_ptr, ODESC_BASE);

		my_strcpy(intro, format("Retrieve the %s from a vault", o_name),
					sizeof(intro));

		/* The location of the quest */
		my_strcpy(where, format("at a depth of %d feet and return to the Guild.", level * 50), sizeof(where));

		if (mode == QMODE_SHORT) my_strcpy(buf, format("%s.", intro), max);
		else if (mode == QMODE_FULL) my_strcpy(buf, format("%s %s", intro, where), max);
		else if (mode == QMODE_HALF_1) my_strcpy(buf, format("%s", intro), max);
		else if (mode == QMODE_HALF_2) my_strcpy(buf, format("%s", where), max);

		/*we are done*/
		return;
	}

	/* Vault, pit or nest quests */
	if  ((q_ptr->q_type == QUEST_THEMED_LEVEL) ||
		 (q_ptr->q_type == QUEST_PIT) ||
		 (q_ptr->q_type == QUEST_NEST))
	{
		/*print out a message about a themed level*/
		char mon_theme[80];

		my_strcpy(intro, "Clear out ", sizeof(intro));

		/*If the middle of a quest, give the number remaining*/
		if (q_ptr->q_flags & (QFLAG_STARTED))
		{
			/* Completed quest */
			if (!q_ptr->active_level)
			{
				my_strcpy(buf, "Collect your reward!", max);
				return;
			}
			/* Still active */
			else
			{
		    	int remaining = q_ptr->max_num - q_ptr->cur_num;

				if (remaining > 1)
				{
					my_strcpy(intro, format("%d remaining creatures from ", remaining),
									sizeof(intro));
				}
				else my_strcpy(intro, "There is 1 remaining creature from ", sizeof(intro));
			}
		}

		my_strcpy(mon_theme, feeling_themed_level[q_ptr->theme], sizeof(mon_theme));
		if (my_is_vowel(mon_theme[0])) my_strcat(intro, "an ", sizeof(intro));
		else my_strcat(intro, "a ", sizeof(intro));

		if (q_ptr->q_type ==  QUEST_THEMED_LEVEL)
		{
			my_strcat(intro, format("%s stronghold", mon_theme), sizeof(intro));
		}
		else if (q_ptr->q_type == QUEST_PIT)
		{
			my_strcat(intro, format("%s pit", mon_theme), sizeof(intro));
		}
		else if (q_ptr->q_type == QUEST_NEST)
		{
			my_strcat(intro, format("%s nest", mon_theme), sizeof(intro));
		}

		/* The location of the quest */
		my_strcpy(where, format("at a depth of %d feet.", level * 50), sizeof(where));

		if (mode == QMODE_SHORT) my_strcpy(buf, format("%s.", intro), max);
		else if (mode == QMODE_FULL) my_strcpy(buf, format("%s %s", intro, where), max);
		else if (mode == QMODE_HALF_1) my_strcpy(buf, format("%s", intro), max);
		else if (mode == QMODE_HALF_2) my_strcpy(buf, format("%s", where), max);

		/*we are done*/
		return;
	}

	/* Completed quest */
	if (!q_ptr->active_level)
	{
		my_strcpy(buf, "Collect your reward!", max);
		return;
	}

	/* Not a vault quest, so get the monster race name (singular)*/
	monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

	if ((q_ptr->q_type == QUEST_UNIQUE) || (q_ptr->q_type == QUEST_FIXED_U))
	{

		/* Monster quests */
		my_strcpy(targets, race_name, sizeof(targets));
		my_strcpy(intro, "To fulfill your quest, ", sizeof(intro));
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
			if (my_is_vowel(name[0])) my_strcpy(targets, format("an %s", name), sizeof(targets));
			else my_strcpy(targets, format("a %s", name), sizeof(targets));
		}
	}

	if (monster_nonliving(r_ptr)) my_strcpy(what, "destroy", sizeof(what));
	else my_strcpy(what, "kill", sizeof(what));

	/* The type of the quest */
	if (q_ptr->q_type == QUEST_FIXED) my_strcpy(intro, "For eternal glory, ", sizeof(intro));
	else if (q_ptr->q_type == QUEST_FIXED_U) my_strcpy(intro, "For eternal glory, ", sizeof(intro));
	else if ((q_ptr->q_type == QUEST_MONSTER) || (q_ptr->q_type == QUEST_FIXED_MON))
	{
		my_strcpy(intro, "To complete your ", sizeof(intro));
		if (q_ptr->q_type == QUEST_FIXED_MON)
		{
			my_strcat(intro, "fixed ", sizeof(intro));
		}
		my_strcat(intro, "quest.", sizeof(intro));
	}

	/* The location of the quest */
	my_strcpy(where, format("at a depth of %d feet.", level * 50), sizeof(where));

	/* Output */
	if (mode == QMODE_SHORT) my_strcpy(buf, format("%s %s %s.", intro, what, targets), max);
	else if (mode == QMODE_FULL) my_strcpy(buf, format("%s %s %s %s", intro, what, targets, where), max);
	else if (mode == QMODE_HALF_1) my_strcpy(buf, format("%s %s %s", intro, what, targets), max);
	else if (mode == QMODE_HALF_2) my_strcpy(buf, format("%s", where), max);

	/* We are done */
	return;
}

void show_quest_mon(int y, int x)
{

	int i = quest_check(p_ptr->cur_quest);

	/* Reset the cursor */
	Term_gotoxy(x, y);

	/*display the monster character if applicable*/
	if ((i == QUEST_MONSTER) || (i == QUEST_UNIQUE) ||
		(i == QUEST_FIXED)   || (i == QUEST_FIXED_U) ||
		(i == QUEST_FIXED_MON))
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
	if (repeats < 3) repeats = 3;

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
		object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

		/* Message */
		msg_format("You have been rewarded with %ld gold pieces.",
			           (long)i_ptr->pval);

		/* Collect the gold */
		p_ptr->au += i_ptr->pval;

	}

	/* Redraw gold */
	p_ptr->redraw |= (PR_GOLD);

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
	if (p_ptr->fame > 150) my_strcpy(buf, "oh glorious one", max);
	else if (p_ptr->fame > 110) my_strcpy(buf, "oh great one", max);
	else if (p_ptr->fame > 80)
	{
		if (p_ptr->psex == SEX_MALE) my_strcpy(buf, "my lord", max);
		else my_strcpy(buf, "my lady", max);
	}
	else if (p_ptr->fame > 50)
	{
		if (p_ptr->psex == SEX_MALE) my_strcpy(buf, "sir", max);
		else my_strcpy(buf, "madam", max);
	}
	else if (p_ptr->fame > 20)
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



void prt_rep_guild(int rep_y, int rep_x)
{
	byte attr;

	char message[120];
	char title[40];

	my_strcpy(message, "You might be interested to know that your current reputation is ", sizeof(message));

	put_str(message, rep_y, rep_x);

	/* Prepare to print the reputation after the previous message. */
	rep_x += strlen(message);

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
	c_put_str(attr, title, rep_y, rep_x);
}

void prt_welcome_guild(void)
{
	char title[40];

	/*Get the current title*/
	get_title(title, sizeof(title));

	/* Introduction */
	put_str(format("Welcome to the Adventurer's Guild, %s.", title), 0, 0);

}

static void grant_reward_hp(void)
{

	int max = p_ptr->player_hp[0];
	s16b new_hp[PY_MAX_LEVEL];

	int max_increase = MIN((max * 5 / 2), (PY_MAX_LEVEL / 2));
	int i, this_increase;

	char title[40];

	get_title(title, sizeof(title));\

	/* Get level 1 hitdice */
	new_hp[0] = max;

	/*
     * Get the max increases for each level from the player_hp seed.
	 * It's easier to work with the data this way.  Also find the current smallest increase.
	 */
	for (i = 1; i < PY_MAX_LEVEL; i++)
	{
		new_hp[i] = (p_ptr->player_hp[i] - p_ptr->player_hp[i - 1]);
	}

	/*
	 * Max out HP at the lowest possible levels until we are out.
	 * The reward system does not allow this reward if the player
	 * is anywhere close to max HP in all levels.
	 * for i = 1 because we know 0 is already maxed.
	 */
	for (i = 1; i < PY_MAX_LEVEL; i++)
	{

		/* Already Maxed */
		if (new_hp[i] == max) continue;

		this_increase = max - new_hp[i];

		/*
		 * Add HP, but only up until the max increase.
		 * Then we are done.
		 */
		if (this_increase < max_increase)
		{
			new_hp[i] = max;
			max_increase -= this_increase;
			continue;
		}
		/* Adding the final allotment of HP */
		else
		{
			new_hp[i] = new_hp[i] + max_increase;

			/* We are done. */
			break;
		}


	}

	/*
     * Re-calc the HP seed with the increases.
	 */
	for (i = 1; i < PY_MAX_LEVEL; i++)
	{
		p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + new_hp[i];

	}

	/* Inform the player */
	msg_format("The adventurer's guild grants to you a newfound vitality, %s!", title);

	/* Update the hitpoints. */
	p_ptr->update |= PU_HP;
}


/*
 * Give a reward to the player
 *
 * Hack - The entire "tailored item" routine makes quite a few assumptions
 * as to what's a good reward, plus it relies a lot of item price to make
 * deductions. It is basically a list of special cases.
 */
static void grant_reward_object(byte depth, byte type)
{
	int i, x;

	char o_name[80];

	bool got_item = FALSE;

	int repeats;

	char title[40];

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

	get_title(title, sizeof(title));

	if ((type == REWARD_RANDART) && (!adult_no_xtra_artifacts))
	{
		s16b k_idx = 0;

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
					if (adj_str_hold[p_ptr->state.stat_ind[A_STR]] < k_ptr->weight / 10)
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

		/*Let the player know it is a Randart reward*/
		msg_format("We will attempt to create an artifact in your honor, %s", title);
		message_flush();

		/*actually create the Randart, mark it if so, if not, make a tailored reward*/
		if (make_one_randart(i_ptr, ((((p_ptr->fame * 8) / 10)  + q_ptr->base_level) / 2), TRUE))
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

				bool interesting = one_in_(2);

				/* Get local object */
				i_ptr = &object_type_body;

				/* Wipe the object */
				object_wipe(i_ptr);

				/* Valid item exists? */
				if (cp_ptr->spell_book == TV_MAGIC_BOOK)
				{
					/*try to make a spellbook, frequently returns nothing*/
					if (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_DUNGEON_MAGIC_BOOK, interesting)) continue;
				}

				if (cp_ptr->spell_book == TV_PRAYER_BOOK)
				{
					/*try to make a spellbook, frequently returns nothing*/
					if (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_DUNGEON_PRAYER_BOOK, interesting)) continue;
				}

				if (cp_ptr->spell_book == TV_DRUID_BOOK)
				{
					/*try to make a spellbook, frequently returns nothing*/
					if (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_DUNGEON_DRUID_BOOK, interesting)) continue;
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

			/* Maybe give the ironman book as a reward? */
			if ((!got_item) && ((randint(100) + 100) < p_ptr->fame))
			{

				/* Now, make the spells accessable. */
				byte realm, j;

				/* Search for a suitable ironman spellbook */
				for (i = 1; i < z_info->k_max; i++)
				{
					object_kind *k_ptr = &k_info[i];

					/*Not the right kind of spellbook*/
					if (cp_ptr->spell_book != k_ptr->tval) continue;

					/*Not an ironman book*/
					if (!(k_ptr->k_flags3 & (TR3_IRONMAN_ONLY))) continue;

					/* Book has already been "tried" */
					if (k_ptr->tried) continue;

					/* We now have a suitable spellbook. */

					/* Wipe the object. */
					object_wipe(i_ptr);

					/* Prepare the Spellbook. */
					object_prep(i_ptr, i);

					/*We ahve an item*/
					got_item = TRUE;

					/* Hack - Mark as tried */
					k_info[i_ptr->k_idx].tried = TRUE;

					realm = get_player_spell_realm();

					/* Extract spells */
					for (j = 0; j < SPELLS_PER_BOOK; j++)
					{

						byte sval = k_ptr->sval;

						s16b spell = spell_list[realm][sval][j];

						/*skip blank spell slots*/
						if (spell == -1) continue;

						/* Remove the Ironman Restriction. */
						p_ptr->spell_flags[spell] &= ~(PY_SPELL_IRONMAN);
					}

					/* Update the spells. */
					p_ptr->update |= PU_SPELLS;

					break;
				}
			}
		}
	}

	/* Try a couple things with a tailored reward, but only below 1000' */
	if ((type == REWARD_GOOD_ITEM) && (randint(20) >= q_ptr->base_level))
	{

		int stats[A_MAX];

		int choice = 0;

		/*clear the counter*/
		x = 0;

		/* Add up the stats that aren't maxed*/
		for (i = 0; i < A_MAX; i++)
		{
			/*Count stats that aren't maxed*/
			if (p_ptr->stat_max[i] == 18+100) continue;

			/*Record this stat*/
			stats[x] = i;

			/*Increase the counter*/
			x++;
		}

		/*Don't do if charisma if it is the only choice*/
		if (stats[0] == A_CHR) x = 0;

		/*Display the stats*/
		if (x > 1)
		{
			int row = 10;
			int col = 23;
			int y;
			bool done = FALSE;

			window_make(13, 8, 62, 17);

			msg_format("Your reward is a stat potion of your choice, %s", title);
			message_flush();

			/* Save screen */
			screen_save();

			prt("   Please choose which stat you would like to augment:", (row-8), col);

			/* Print out the labels for the columns */
			c_put_str(TERM_WHITE, "  Self", row-1, col+5);
			c_put_str(TERM_WHITE, " RB", row-1, col+11);
			c_put_str(TERM_WHITE, " CB", row-1, col+14);
			c_put_str(TERM_WHITE, " EB", row-1, col+18);
			c_put_str(TERM_WHITE, "  Best", row-1, col+22);

			for (y = 0; y < x; y++)
			{
				char buf[80];
				char tmp_val[5];

				/* Start with an empty "index" */
				tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

				/* Prepare an "index" */
				tmp_val[0] = index_to_label(y);

				/* Bracket the "index" --(-- */
				tmp_val[1] = ')';

				/*Terminate*/
				tmp_val[3] = '\0';

				i = stats[y];

				/* Display the index (or blank space) */
				put_str(tmp_val, row+y, col - 8);

				/* Reduced */
				if (p_ptr->state.stat_use[i] < p_ptr->state.stat_top[i])
				{
					/* Use lowercase stat name */
					put_str(stat_names_reduced[i], row+y, col);
				}

				/* Normal */
				else
				{
					/* Assume uppercase stat name */
					put_str(stat_names[i], row+y, col - 4);
				}

				/* Internal "natural" maximum value */
				cnv_stat(p_ptr->stat_max[i], buf, sizeof(buf));
				c_put_str(TERM_L_GREEN, buf, row+y, col+5);

				/* Race Bonus */
				strnfmt(buf, sizeof(buf), "%+3d", rp_ptr->r_adj[i]);
				c_put_str(TERM_L_BLUE, buf, row+y, col+11);

				/* Class Bonus */
				strnfmt(buf, sizeof(buf), "%+3d", cp_ptr->c_adj[i]);
				c_put_str(TERM_L_BLUE, buf, row+y, col+14);

				/* Equipment Bonus */
				strnfmt(buf, sizeof(buf), "%+3d", p_ptr->state.stat_add[i]);
				c_put_str(TERM_L_BLUE, buf, row+y, col+18);

				/* Resulting "modified" maximum value */
				cnv_stat(p_ptr->state.stat_top[i], buf, sizeof(buf));
				c_put_str(TERM_L_GREEN, buf, row+y, col+22);

				/* Only display stat_use if not maximal */
				if (p_ptr->state.stat_use[i] < p_ptr->state.stat_top[i])
				{
					cnv_stat(p_ptr->state.stat_use[i], buf, sizeof(buf));
					c_put_str(TERM_YELLOW, buf, row+y, col+28);
				}

			}

			while (!done)
			{
				char c = inkey();

				/* Letters are used for selection */
				if (isalpha(c))
				{
					if (islower(c))
					{
						choice = A2I(c);
					}
					else
					{
						choice = c - 'A' + 26;
					}

					/* Validate input */
					if ((choice > -1) && (choice < x))
					{
						done = TRUE;
					}

					else
					{
						bell("Illegal response to question!");
					}
				}
			}
		}

		if (x > 0)
		{
			int sval;

			/*Get the proper sval*/
  			switch (stats[choice])
			{
				case A_STR: {sval = SV_POTION_INC_STR; break;}
				case A_INT: {sval = SV_POTION_INC_INT; break;}
				case A_WIS: {sval = SV_POTION_INC_WIS; break;}
				case A_DEX: {sval = SV_POTION_INC_DEX; break;}
				case A_CON: {sval = SV_POTION_INC_CON; break;}
				default: sval = SV_POTION_INC_CHR;
			}

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make the potion */
			object_prep(i_ptr, lookup_kind(TV_POTION, sval));

			/* Since charisma is a cheap potion. */
			if (sval == SV_POTION_INC_CHR) i_ptr->number = 3;

			got_item = TRUE;

			screen_load();

		}
	}

	/* Maybe we want to give a potion of augmentation */
	if ((!got_item) && ((type == REWARD_GOOD_ITEM) || (type == REWARD_GOOD_ITEM)))
	{
		/*clear the counter*/
		x = 0;

		/* Add up the stats indexes over 13*/
		for (i = 0; i < A_MAX; i++)
		{
			x += (MAX(0, p_ptr->state.stat_ind[i] - 10));
		}

		/*We only want to give potion if stats are low.*/
		if ((rand_int(30) + 18) > x)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make the spellbook */
			object_prep(i_ptr, lookup_kind(TV_POTION, SV_POTION_AUGMENTATION));

			got_item = TRUE;
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
				repeats = p_ptr->fame / 8;
				if (repeats < 3) repeats = 3;
				break;
			}
			case REWARD_GREAT_ITEM:
			{
				repeats = p_ptr->fame / 10;
				if (repeats < 2) repeats = 2;
				break;
			}

			/*good reward*/
			default:
			{
				repeats = p_ptr->fame / 13;
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
					if (i_ptr->art_num != artifact_marker) artifact_wipe(artifact_marker, FALSE);
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
				if (randint(125) < p_ptr->fame) do_great = TRUE;
			}
			else if (type == REWARD_GREAT_ITEM) do_great = TRUE;

			/* Valid item exists?  If not, don't count it*/
			if (!make_object(j_ptr, do_great, TRUE, droptype, FALSE)) continue;

			/* Hack -- in case of artifact, mark it as not created yet */
			if (j_ptr->art_num)
		 	{
				a_info[j_ptr->art_num].a_cur_num = 0;

				/*marker in case a randart needs to be erased later*/
				artifact_marker = j_ptr->art_num;
			}

			/*get the quest category of equipment/inventory*/
			inven_quest_slot = quest_equip_slot(j_ptr);

			/*paranoia*/
			if (inven_quest_slot == INVEN_NOT_EQUIPMENT) continue;

			/* Make sure weapon isn't too heavy */
			if ((inven_quest_slot == QUEST_SLOT_WIELD) &&
				(adj_str_hold[p_ptr->state.stat_ind[A_STR]] < j_ptr->weight / 10))
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
				u32b f1, f2, f3, fn;

				object_flags(j_ptr, &f1, &f2, &f3, &fn);

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
		while (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_UNTHEMED, FALSE)) continue;
	}

	/*Don't wipe any normal artifacts!*/
	if (artifact_marker >= z_info->art_norm_max)
	{
		/*make sure it isn't being used before wiping*/
		if (i_ptr->art_num != artifact_marker) artifact_wipe(artifact_marker, FALSE);
	}

	/* Identify it fully */
	object_aware(i_ptr);
	object_known(i_ptr);

	/* Mark the item as fully known */
	i_ptr->ident |= (IDENT_MENTAL);

	/* Handle Artifacts */
	if (i_ptr->art_num)
	{
		/*
		 * Artifact might not yet be marked as created (if it was chosen from tailored
		 * rewards), so now it's the right time to mark it
		 */
		a_info[i_ptr->art_num].a_cur_num = 1;

		/* If the item was an artifact, and if the auto-note is selected, write a message. */
		if (adult_take_notes)
		{
			int artifact_depth;
			char note[120];
			char shorter_desc[100];

			/* Get a shorter description to fit the notes file */
			object_desc(shorter_desc, sizeof(shorter_desc), i_ptr, ODESC_BASE);

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

		/* Process artifact lore */
		if (ARTIFACT_EASY_MENTAL(i_ptr))
		{
			/* Get the lore entry */
			artifact_lore *a_l_ptr = &a_l_list[i_ptr->art_num];

			/* Remember this artifact from now on */
			a_l_ptr->was_fully_identified = TRUE;
		}
	}

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Remember history */
	object_history(i_ptr, ORIGIN_REWARD, 0);
	/* Hack */
	i_ptr->origin_dlvl = depth;

	/* Note that the pack is too full */
	if (!inven_carry_okay(i_ptr))
	{
		msg_format("You have no room in your backpack for %s.", o_name);

		/* Inform the player */
		msg_print("Your reward is waiting outside!");

		/* Drop the object */
		drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
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
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_QUEST_ST);

	/*undo special item level creation*/
	object_generation_mode = FALSE;

}

#define LEV_BOOST 3

/* Helper function to decide if the player should have a quest reward */
static bool extra_hp_reward(int chance)
{
	int cur_hp = p_ptr->player_hp[PY_MAX_LEVEL-1];
	int max_hp = ((PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 5) / 8) + PY_MAX_LEVEL + (p_ptr->hitdie * 3);
	int hp_left = max_hp - cur_hp;

	/* Boundry Control before randing check.*/
	if (hp_left < p_ptr->hitdie) return (FALSE);

	if (randint(chance) > p_ptr->fame) return (FALSE);

	if (one_in_(2)) return (FALSE);

	if (randint1(hp_left) < p_ptr->hitdie) return (FALSE);

	return (TRUE);
}

/* Helper function to decide if the player should have a custom quest reward */
static bool custom_randart_reward(int chance, int dice)
{
	if (!adult_no_artifacts) return FALSE;
	if (!adult_no_xtra_artifacts) return FALSE;

	if (chance + damroll(dice,25) > p_ptr->fame) return (FALSE);
	if (!one_in_(3)) return (FALSE);

	return (TRUE);
}


/*
 * Actually give the character a fixed or monster quest.
 * If bool fixed it true, a fixed quest is placed.
 */
static bool place_mon_quest(int lev, bool fixed)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	monster_lore *l_ptr;

	int i;

	monster_race *r_ptr;

	int r_idx;

	alloc_entry *table = alloc_race_table;

	long value;

	int total = 0;

	int max_depth, min_depth;
	u32b max_diff, min_diff, max_diff_unique, min_diff_unique;

	/* Monsters can be up to 3 levels out of depth */
	max_depth = LEV_BOOST + lev;

	/*assumes lev is always greater than 0*/
	min_depth = lev;

	/* Fixed quests use deeper monsters */
	if (fixed)
	{
		max_depth += 6;
		min_depth += 6;
	}

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

	/*get the average difficulty spanning 5 levele for monsters*/
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

			/* Not for fixed quests */
			if (fixed) continue;
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

			/* Check if an immobile monster can still hurt the player from a distance */
			if (r_ptr->flags1 & RF1_NEVER_MOVE)
			{
				bool okay = FALSE;

				/* Allow if the monster has at least one summon spell*/
				if (r_ptr->flags7)	okay = TRUE;

				/* Allow if the monster can attack */
				if ((r_ptr->flags4 & (RF4_ATTACK_MASK | RF4_BREATH_MASK)) ||
					(r_ptr->flags5 & (RF5_ATTACK_MASK | RF5_BREATH_MASK)) ||
					(r_ptr->flags5 & (RF6_ATTACK_MASK | RF6_BREATH_MASK)) ||
					(r_ptr->flags6 & (RF7_ATTACK_MASK | RF7_BREATH_MASK)))
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
	q_ptr->base_level = lev;
	q_ptr->active_level = lev;

	/* Get the actual race */
	r_ptr = &r_info[q_ptr->mon_idx];
	l_ptr = &l_list[q_ptr->mon_idx];

	/* Mark it as seen at least once */
	if (l_ptr->sights < 1 ) l_ptr->sights = 1;

	/*
	 * This info slightly different depending on if
	 * this is a unique quest or monster quest
	 */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		q_ptr->max_num = 1;
		q_ptr->q_type	= QUEST_UNIQUE;
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

		/* Fixed have more monsters */
		if (fixed) num += 3 + randint1(2) + randint1(2);

		/*assign the number*/
		q_ptr->max_num = num;

		/*assign the quest type*/
		if (fixed) q_ptr->q_type	= QUEST_FIXED_MON;
		else q_ptr->q_type	= QUEST_MONSTER;

	}

	/* First roll for tailored award */
	if (custom_randart_reward(75, 8))	q_ptr->reward = REWARD_RANDART;

	/* Next, a chance to simply increase the player's hp lifeline*/
	else if (extra_hp_reward(50)) q_ptr->reward = REWARD_INC_HP;

	/* Then, a try for a tailored award */
	else if (25 + damroll(5,20) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;

	else
	{
		/* Chance of good item reward */
		int chance = 95 - (p_ptr->fame * 2) - (lev *  2);

		/* Better rewards for unique or fixed quests */
		if (r_ptr->flags1 & (RF1_UNIQUE)) chance -= 10;
		if (fixed) chance -= 10;

		if (rand_int(100) > chance)
		{
			q_ptr->reward = REWARD_GREAT_ITEM;
		}

		else if (one_in_(2)) q_ptr->reward = REWARD_GOOD_ITEM;

		else q_ptr->reward = REWARD_GOLD;

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
		if ((q_ptr->q_type == QUEST_NEST) || (q_ptr->q_type == QUEST_PIT))
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
	bool out_of_depth_mon = FALSE;

	int potential_monsters = 0;
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

		/*Count the number of monsters on the level*/
		potential_monsters += max_themed_monsters(r_ptr, max_diff);

		/*There is at least one monster of the right depth*/
		out_of_depth_mon = TRUE;

	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	if (!out_of_depth_mon) return(FALSE);
	if (potential_monsters < (QUEST_THEMED_LEVEL_NUM / 3)) return (FALSE);
	return (TRUE);

}


/*
 * Actually give the character a vault quest
 */
static bool place_pit_nest_quest(int lev)
{

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	int tries = 0;
	int i;

	bool checked_theme_yet[LEV_THEME_TAIL];

	/*start with false, meaning we haven't checked it yet*/
	for (i = 0; i < LEV_THEME_TAIL; i++) checked_theme_yet[i] = FALSE;

	/*50% chance of a pit or nest*/
	if one_in_(2) q_ptr->q_type = QUEST_PIT;
	else q_ptr->q_type = QUEST_NEST;

	q_ptr->base_level = lev;
	q_ptr->active_level = lev;
	if (custom_randart_reward(75, 6)) q_ptr->reward = REWARD_RANDART;

	/* A chance to simply increase the player's hp lifeline*/
	else if (extra_hp_reward(40)) q_ptr->reward = REWARD_INC_HP;

	else if (30 + damroll(3,25) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;

	else q_ptr->reward = REWARD_GREAT_ITEM;

	while (TRUE)
	{
		/*
		 * We get 5000 tries at this, but it should only fail
		 * if somebody wasn't careful while editing around the monster list.
		 */
		if ((tries++) > 5000) return (FALSE);

		/*Get the actual theme for either the pit or nest*/
		if (q_ptr->q_type == QUEST_PIT) q_ptr->theme = get_pit_theme(lev + 3, TRUE);
		else q_ptr->theme = get_nest_theme(lev + 3, TRUE);

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

	/*success*/
	return (TRUE);
}

/* Helper function to see if a level has a good level quest. */
static int check_level_quest(void)
{
	int i;

	for (i = 0; i < LEV_THEME_TAIL; i++)
	{

		/*hack - never do jelly quests, or the single element breathing dragons. */
		if (i == LEV_THEME_JELLY) 		continue;
		if (i == LEV_THEME_DRAGON_ACID) continue;
		if (i == LEV_THEME_DRAGON_FIRE) continue;
		if (i == LEV_THEME_DRAGON_ELEC) continue;
		if (i == LEV_THEME_DRAGON_COLD) continue;
		if (i == LEV_THEME_DRAGON_POIS) continue;

		/* There is at least one good theme. */
		if (check_theme_depth(guild_quest_level(), i)) return (TRUE);
	}

	return (FALSE);
}

/*
 * Actually give the character a vault quest
 */
static bool place_level_quest(int lev)
{

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	int i;
	int tries = 0;

	bool checked_theme_yet[LEV_THEME_TAIL];

	/*start with false, meaning we haven't checked it yet*/
	for (i = 0; i < LEV_THEME_TAIL; i++) checked_theme_yet[i] = FALSE;

	/* Actually write the quest */
	q_ptr->q_type = QUEST_THEMED_LEVEL;
	q_ptr->base_level = lev;
	q_ptr->active_level = lev;
	if (custom_randart_reward(60, 6))  q_ptr->reward = REWARD_RANDART;

	/* A chance to simply increase the player's hp lifeline*/
	else if (extra_hp_reward(30)) q_ptr->reward = REWARD_INC_HP;

	else if (25 + damroll(3,25) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;

	else q_ptr->reward = REWARD_GREAT_ITEM;

	while (TRUE)
	{
		/*
		 * We get 5000 tries at this, but it should only fail
		 * if somebody wasn't careful while editing around the monster list.
		 */
		if ((tries++) > 5000) return (FALSE);

		/*Get the actual theme*/
		q_ptr->theme = get_level_theme(lev + THEMED_LEVEL_QUEST_BOOST / 2, TRUE);

		/*hack - never do jelly quests*/
		if (q_ptr->theme == LEV_THEME_JELLY) continue;

		/*We don't want to run through check_theme_depth a bunch of times*/
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
	q_ptr->q_type = QUEST_VAULT;
	q_ptr->base_level = lev;
	q_ptr->active_level = lev;
	if (custom_randart_reward(60, 5)) q_ptr->reward = REWARD_RANDART;

	/* A chance to simply increase the player's hp lifeline*/
	else if (extra_hp_reward(15)) q_ptr->reward = REWARD_INC_HP;

	else if (20 + damroll(3,25) < p_ptr->fame) q_ptr->reward = REWARD_TAILORED;

	else q_ptr->reward = REWARD_GREAT_ITEM;

	/*generate a quest artifact*/
	make_quest_artifact(lev);

	/*success*/
	return (TRUE);
}

bool quest_allowed(byte j)
{
	/*not a valid quest type*/
	if (j >= QUEST_SLOT_MAX) return (FALSE);

	/*quest vaults not always offered*/
	if (j == QUEST_SLOT_VAULT)
	{
		if (p_ptr->fame < 10)return (FALSE);
		if (!(q_info[GUILD_QUEST_SLOT].q_flags & (QFLAG_VAULT_QUEST))) return (FALSE);
	}
	else if (j == QUEST_SLOT_PIT_NEST)
	{
		if (p_ptr->max_depth < 6) return (FALSE);
	}
	else if (j == QUEST_SLOT_LEVEL)
	{
		if (p_ptr->max_depth < 14) return (FALSE);
		if (!check_level_quest()) return (FALSE);
	}
	else if (j == QUEST_SLOT_FIXED)
	{
		if (p_ptr->max_depth < 3) return (FALSE);
		if (p_ptr->quest_depth > MAX_MIN_DEPTH) return (FALSE);
	}

	/* Allowable */
	return (TRUE);

}

/*Determine if we can do a quest, based on players current max depth */
bool can_quest_at_level(void)
{
	int i;

	/*Honor the no quests option*/
	if (adult_no_quests) return (FALSE);

	/* No more quests if they are at the bottom of the dungeon. */
	if ((p_ptr->quest_depth + 2) > MAX_DEPTH) return (FALSE);

	/* Make sure there is no fixed quest on the same level of quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* check fixed quests to see that they're not on the same level*/
		if ((q_info[i].q_type == QUEST_FIXED) || (q_info[i].q_type == QUEST_FIXED_U))
		{
			if (q_info[i].base_level == guild_quest_level())
			{
				return (FALSE);
			}
		}
	}

	/* Allowable */
	return (TRUE);
}

/* Check if the player is eligible for a quest reward. */
bool check_reward(void)
{

	/* Check for outstanding rewards */
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	if ((q_ptr->q_type == QUEST_MONSTER) || (q_ptr->q_type == QUEST_UNIQUE) ||
		(q_ptr->q_type == QUEST_FIXED_MON))
	{
		/*We owe the player a reward*/
		if ((!q_ptr->active_level) && (q_ptr->reward))
		{
			return (TRUE);
		}
	}

	else if (q_ptr->q_type == QUEST_VAULT)
	{
		/* Find quest item in the inventory*/
		if (quest_item_slot() > -1) return (TRUE);
		else return (FALSE);

	}

	else if  ((q_ptr->q_type ==  QUEST_THEMED_LEVEL) ||
			  (q_ptr->q_type == QUEST_PIT) ||
			  (q_ptr->q_type == QUEST_NEST))
	{
		/*We owe the player a reward*/
		if ((!q_ptr->active_level) && (q_ptr->reward))
		{
			return (TRUE);
		}
  	}

	return (FALSE);
}

/* Actually give the quest reward*/
void do_reward(void)
{
	int j;

	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* Sanity check. */
	if (!check_reward()) return;

	if ((q_ptr->q_type == QUEST_MONSTER) || (q_ptr->q_type == QUEST_UNIQUE) ||
		(q_ptr->q_type == QUEST_FIXED_MON))
	{
		/* Grant fame bonus */
		p_ptr->fame += 1;

		/* Fixed quests sometimes get a better fame boost */
		if (q_ptr->q_type == QUEST_FIXED_MON)
		{
			if (one_in_(2)) p_ptr->fame += 1;
		}
		altered_inventory_counter += 3;

		/* The note has already been written */
	}

	else if (q_ptr->q_type == QUEST_VAULT)
	{
		char note[120];

		object_type *o_ptr;

		/* Find quest item in the inventory*/
		j = quest_item_slot();

		o_ptr = &inventory[j];

		/* Grant fame bonus */
		p_ptr->fame += 5;
		altered_inventory_counter += 5;

		/*if using notes file, make a note*/
		if (adult_take_notes)
		{
			char o_name[80];

			/* Get a shorter description to fit the notes file */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);

			/*Create the note*/
			sprintf(note, "Returned %s to the Adventurer's Guild.", o_name);

			/*write note*/
 			do_cmd_note(note, q_ptr->active_level);
		}

		artifact_wipe(o_ptr->art_num, TRUE);

		/* Destroy the quest item in the pack */
		inven_item_increase(j, -255);
		inven_item_optimize(j);
	}

	else if  ((q_ptr->q_type ==  QUEST_THEMED_LEVEL) || (q_ptr->q_type == QUEST_PIT) ||
			  (q_ptr->q_type == QUEST_NEST))
	{
		/*We owe the player a reward*/
		if ((!q_ptr->active_level) && (q_ptr->reward))
		{
			if (q_ptr->q_type == QUEST_THEMED_LEVEL)	p_ptr->fame += 1;

			/* Slightly random fame bonus for these harder quests*/
			p_ptr->fame += randint(2);
			altered_inventory_counter += 4;

			/*The note has already been written*/
		}
  	}

	/* Generate object at quest level */
	object_level = q_ptr->base_level;

	/* Create the reward*/
	if (q_ptr->reward == REWARD_GOLD) grant_reward_gold();
	else if (q_ptr->reward == REWARD_INC_HP) grant_reward_hp();
	else grant_reward_object(q_ptr->base_level, q_ptr->reward);

	/* Handle and handle stuff */
	notice_stuff();
	handle_stuff();

	/* Reset object level */
	object_level = p_ptr->depth;

	/* Wipe the quest */
	guild_quest_wipe();

}

/*
 * Display the "contents" of the adventurer's guild
 */
void display_quest(void)
{
	char q_out[120];


	put_str("Your current quest:", 8, 3);

	/* Reset the cursor */
	Term_gotoxy(3, 10);

	/*break quest description into two lines*/
	describe_quest(q_out, sizeof(q_out), p_ptr->cur_quest, QMODE_HALF_1);
	Term_addstr(-1, TERM_WHITE, q_out);

	/*display the monster character if applicable*/
	if ((quest_check(p_ptr->cur_quest) == QUEST_MONSTER) ||
		(quest_check(p_ptr->cur_quest) == QUEST_UNIQUE) ||
		(quest_check(p_ptr->cur_quest) == QUEST_FIXED_MON))
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


/*
 * "Purchase" a quest from the guild
 */
bool guild_purchase(int choice)
{
	int i;
	int qlev = guild_quest_level();
	bool quest_placed = TRUE;

	/* Make sure there is no fixed quest on the same level of quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* check fixed quests to see that they're not on the same level*/
		if ((q_info[i].q_type == QUEST_FIXED) || (q_info[i].q_type == QUEST_FIXED_U))
		{
			if (q_info[i].base_level == qlev)
			{
				msg_print("A greater task lies before you!");
				return (FALSE);
			}
		}

	}

	/*place a monster quest*/
	if (choice == QUEST_SLOT_MONSTER)
	{
		if (!place_mon_quest(qlev, FALSE))
		{
			guild_quest_wipe();
			quest_placed = FALSE;
		}
	}
	/*Place a vault quest*/
	else if (choice == QUEST_SLOT_VAULT)
	{
		if (!place_vault_quest(qlev))
		{
			guild_quest_wipe();
			quest_placed = FALSE;
		}
	}
	else if (choice == QUEST_SLOT_LEVEL)
	{
		if (!place_level_quest(qlev))
		{
			guild_quest_wipe();
			quest_placed = FALSE;
		}
	}
	/*Nest or Pit quests*/
	else if (choice == QUEST_SLOT_PIT_NEST)
	{
		if (!place_pit_nest_quest(qlev))
		{
			guild_quest_wipe();
			quest_placed = FALSE;
		}
	}
	/*place a monster quest*/
	else if (choice == QUEST_SLOT_FIXED)
	{
		if (!place_mon_quest(qlev, TRUE))
		{
			guild_quest_wipe();
			quest_placed = FALSE;
		}
	}

	/* Set current quest */
	if (quest_placed)
	{
		p_ptr->quest_depth = qlev;
		p_ptr->cur_quest = qlev;
		q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_STARTED);
	}


	/*
	 * Have the next quest be either two or three levels above
	 * this one.
	 */
	if (one_in_(2)) q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_EXTRA_LEVEL);
	else q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_EXTRA_LEVEL);

	/* Vault quest allowed 1/3 of the time */
	if (one_in_(3)) q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_VAULT_QUEST);
	else q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_VAULT_QUEST);

	return (TRUE);
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
		if (q_info[i].active_level == lev) return (q_info[i].q_type);
	}

	p_ptr->redraw |= (PR_QUEST_ST);

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
 * Wipe the quest clean.
 */
void guild_quest_wipe(void)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
	bool extra_level = FALSE;
	bool vault_quest = FALSE;


	/* Remember if there should be an extra level added to the next quest depth */
	if (q_ptr->q_flags & (QFLAG_EXTRA_LEVEL)) extra_level = TRUE;
	/* Remember if we should allow a vault quest */
	if (q_ptr->q_flags & (QFLAG_VAULT_QUEST)) vault_quest = TRUE;

	/* Wipe the structure */
	(void)WIPE(q_ptr, quest_type);

	if (extra_level) q_ptr->q_flags |= QFLAG_EXTRA_LEVEL;
	if (vault_quest) q_ptr->q_flags |= QFLAG_VAULT_QUEST;

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

	/* Show a special mark for some player turns. Redraw if necessary */
	quest_indicator_timer = 50;
	quest_indicator_complete = FALSE;
	if (!character_icky) p_ptr->redraw |= (PR_QUEST_ST);

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
			object_desc(o_name, sizeof(o_name), i_ptr, ODESC_BASE);

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
			if (my_is_vowel(mon_theme[0])) my_strcat(note, "an ", sizeof(note));
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

			if (q_ptr->q_type == QUEST_UNIQUE)
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
            		sprintf(note, "Quest: Failed to destroy %d %s", q_ptr->max_num, race_name);
				else sprintf(note, "Quest: Failed to kill %d %s", q_ptr->max_num, race_name);
			}

 		}

		/*write the note*/
		do_cmd_note(note, q_ptr->base_level);

	}

	/*wipe the quest*/
	guild_quest_wipe();

	p_ptr->redraw |= (PR_QUEST_ST);

	/* Disturb */
	disturb(0,0);
}


/*
 * Create the quest indicator string and put it on "dest"
 * Return TRUE if the quest indicator is active
 */
void format_quest_indicator(char dest[], int max, byte *attr)
{
	/* Get the current quest, if any */
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	/* No quest */
	if (!q_ptr->q_type)
	{
		my_strcpy(dest, "Qst: None", max);
		return;
	}

	/* Completed Quest! */
	if (!q_ptr->active_level)
	{
		my_strcpy(dest, "Q:Reward!", max);
		*attr = TERM_GREEN;
		return;
	}

 	/* Special case. Vault quests */
	if (q_ptr->q_type == QUEST_VAULT)
	{
		/* Get the artifact template */
		artifact_type *a_ptr = &a_info[QUEST_ART_SLOT];

		/* Default character */
		char chr = ']';

		/* Player has the quest item */
		if (q_ptr->q_flags & (QFLAG_STARTED))
		{
			if (quest_item_slot() > -1)
			{
				my_strcpy(dest, "Q:GoTo Guild!", max);
				*attr = TERM_GREEN;
				return;
			}

		}

		else if (q_ptr->base_level != p_ptr->depth)
		{
			strnfmt(dest, max, "Q:Goto %d'", q_ptr->base_level * 50);
			*attr = TERM_BLUE;
			return;
		}

		/* Get the symbol of the artifact */
		if (a_ptr->name[0])
		{
			/* Get the object kind */
			s16b k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

			/* Get the default character (graphics mode not supported) */
			if (k_idx) chr = k_info[k_idx].d_char;
		}

		/* Format */
		strnfmt(dest, max, "Qst:%c", chr);
		*attr = TERM_L_RED;
	}

	else if (q_ptr->base_level != p_ptr->depth)
	{
		strnfmt(dest, max, "Q:Goto %d'", q_ptr->base_level * 50);

		if (quest_indicator_timer > 0) *attr = TERM_RED;
		else *attr = TERM_BLUE;
		return;
	}

	/* Monster, pit/nest or themed level quests */
	else
	{
		/* Show the remaining number of monsters */
		strnfmt(dest, max, "Qst:%d" , q_ptr->max_num - q_ptr->cur_num);
		*attr = TERM_L_RED;
	}

	/* Success */
	return;
}

void quest_monster_update(void)
{
	quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];
	char race_name[80];
	char note[120];

	int remaining = q_ptr->max_num - q_ptr->cur_num;

	/*nothing left, notification already printed in monster_death */
	if (!remaining) return;

	if (((q_ptr->q_type ==  QUEST_THEMED_LEVEL) ||
         (q_ptr->q_type == QUEST_PIT) ||
		 (q_ptr->q_type == QUEST_NEST)))
	{
		if (remaining > 1)
		{
			my_strcpy(note, format("There are %d creatures remaining ", remaining), sizeof(note));
		}
		else my_strcpy(note, "There is one creature remaining ", sizeof(note));
		my_strcat(note, format("from the %s", feeling_themed_level[q_ptr->theme]),
						sizeof(note));
		if  (q_ptr->q_type ==  QUEST_THEMED_LEVEL) 	my_strcat(note, " stronghold.", sizeof(note));
		else if (q_ptr->q_type == QUEST_PIT)	my_strcat(note, " pit.", sizeof(note));
		else if (q_ptr->q_type == QUEST_NEST)	my_strcat(note, " nest.", sizeof(note));

		/*dump the final note*/
		msg_format(note);
	}
	/* Monster and fixed monster quests */
	else
	{
		/* Get the monster race name (singular)*/
		monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

		/* Multiple quest monsters */
		if (remaining > 1)
		{
			plural_aux(race_name, sizeof(race_name));
		}

		my_strcpy(note, format("You have %d %s remaining to complete your ",
				remaining, race_name), sizeof(note));

		if (q_ptr->q_type == QUEST_FIXED_MON)
		{
			my_strcat(note, "fixed ", sizeof(note));
		}

		my_strcat(note, "quest.", sizeof(note));

		/*dump the final note*/
		msg_format(note);


	}
}


