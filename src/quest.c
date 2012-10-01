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
   
#define QMODE_HALF_1 1
#define QMODE_HALF_2 2
#define QMODE_SHORT  3
#define QMODE_FULL   4

static int avail_quest;

/*
 * The Adventurer's guild's selection
 */
static s16b guild[GUILD_QUESTS] = 
{
	0,	2,	5,	8,
};

/*
 * Mega-hack - Fix plural names of monsters
 *
 * Taken from PernAngband, modified to fit Ey monster list 
 */
static void plural_aux(char * Name)
{
	int NameLen = strlen(Name);

	if (strstr(Name, " of "))
	{
	        cptr aider = strstr(Name, " of ");
	        char dummy[80];
	        int i = 0;
	        cptr ctr = Name;

	        while (ctr < aider)
	        {
	            dummy[i] = *ctr;
	            ctr++; i++;
	        }

		if (dummy[i-1] == 's')
		{
			strcpy (&(dummy[i]), "es");
			i++;
		}
		else
		{
			strcpy (&(dummy[i]), "s");
		}

		strcpy(&(dummy[i+1]), aider);
		strcpy(Name, dummy);
	}
	else if ((strstr(Name, "coins")) || (strstr(Name, "gems")))
	{
		char dummy[80];
		strcpy (dummy, "Piles of c");
		strcat (dummy, &(Name[1]));
		strcpy (Name, dummy);
		return;
	}
	else if ((strstr(Name, "Manes")) || (Name[NameLen-1]=='u') || (strstr(Name, "Yeti"))
		|| (streq(&(Name[NameLen-2]), "ua")) || (streq(&(Name[NameLen-3]), "nee")))
	{
		return;
	}
	else if (Name[NameLen-1]=='y')
	{
		strcpy(&(Name[NameLen-1]), "ies");
	}
	else if (streq(&(Name[NameLen-4]), "ouse"))
	{
		strcpy (&(Name[NameLen-4]), "ice");
	}
	else if (streq(&(Name[NameLen-4]), "lung"))
	{
		strcpy (&(Name[NameLen-4]), "lungen");
	}
	else if (streq(&(Name[NameLen-3]), "sus"))
	{
		strcpy (&(Name[NameLen-3]), "si");
	}	
	else if (streq(&(Name[NameLen-3]), "ous"))
	{
		strcpy (&(Name[NameLen-3]), "i");
	}	
	else if ((streq(&(Name[NameLen-4]), "lman")) || (streq(&(Name[NameLen-4]), "sman")))
	{
		strcpy (&(Name[NameLen-3]), "men");
	}
	else if (streq(&(Name[NameLen-2]), "ex"))
	{
		strcpy (&(Name[NameLen-2]), "ices");
	}
	else if ((Name[NameLen-1]=='f') && (!streq(&(Name[NameLen-2]), "ff")))
	{
		strcpy (&(Name[NameLen-1]), "ves");
	}
	else if ((streq(&(Name[NameLen-2]), "ch")) || (Name[NameLen-1] == 's'))
	{
		strcpy (&(Name[NameLen]), "es");
	}
	else
	{
		strcpy (&(Name[NameLen]), "s");
	}
}

/*
 * Provide a description of the quest
 */
cptr describe_quest(s16b level, int mode)
{
	int q_idx = quest_num(level);
	char name[80];
	char intro[80];
	char targets[80];
	char where[80];

	quest_type *q_ptr = &q_info[q_idx];

	if (!q_idx) return NULL;

	strcpy(name, (monster_name_race(q_ptr->r_idx)));

	/* Multiple quest monsters */
	if ((q_ptr->max_num - q_ptr->cur_num) > 1)
	{
		plural_aux(name);
		strcpy(targets, format("%d %s",(q_ptr->max_num - q_ptr->cur_num), name));
	}

	/* One quest monster */
	else
	{
		if (is_a_vowel(name[0])) strcpy(targets, format("an %s", name));
		else strcpy(targets, format("a %s", name));
	}

	/* The type of the quest */
	if (q_ptr->type == QUEST_FIXED) strcpy(intro, "For eternal glory, you must");
	else if (q_ptr->type == QUEST_GUILD) strcpy(intro, "To fulfill your task, you must");

	/* Paranoia */
	else strcpy(intro, "For some bizzare reason, you should");
			
	/* The location of the quest */
	if (!depth_in_feet) strcpy(where, format("on dungeon level %d.", level));
	else strcpy(where, format("at a depth of %d feet.", level * 50));

	/* Output */
	if (mode == QMODE_SHORT) return (format("%s kill %s.", intro, targets));
	else if (mode == QMODE_FULL) return (format("%s kill %s %s", intro, targets, where));
	else if (mode == QMODE_HALF_1) return (format("%s kill %s", intro, targets));
	else if (mode == QMODE_HALF_2) return (format("%s", where));
	
	/* Paranoia */
	return NULL;
}

/*
 * Give a reward to the player 
 */
static void grant_reward(byte reward_level, byte type)
{
	int i;
	bool great = ((type == REWARD_GREAT_ITEM) ? TRUE : FALSE);

	object_type *i_ptr;
	object_type object_type_body;

	/* Generate object at quest level */
	p_ptr->obj_depth  = reward_level;

	if (type == REWARD_GOLD) 
	{
		for (i = 0; i < 5; i++)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make some gold */
			if (!make_gold(i_ptr, 0)) continue;

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
		}
	}

	else acquirement(p_ptr->py, p_ptr->px, 1, great, FALSE);

	/* Reset object level */
	p_ptr->obj_depth  = p_ptr->depth;
}

/*
 * Actually give the character a quest
 */
static bool place_quest(int q, int lev, int number, int difficulty)
{
	int i, mcount, midx;
	int chance;
	sint lev_diff;
	monster_race *r_ptr;

	mcount = 0;

	/* Monsters can be up to 3 levels out of depth with difficulty 0 */
	lev_diff = 3 + difficulty;

	while (mcount == 0)
	{
		if ((lev_diff<(-50)) || (lev+lev_diff < 1))
		{
			message(MSG_FAIL, 0, "There are no elligable monsters to quest for");
			return FALSE;
		}

		/* There's a chance of climbing up */
		if ((lev_diff < (difficulty - 3)) || lev+lev_diff <= 1) chance = 0;
		else if (lev_diff > difficulty) chance = 80;
		else chance = 20;

		if (rand_int(100) < chance) 
		{
			if (lev_diff <= difficulty) number += (damroll(2,2) - 1);
			lev_diff--;
			continue;
		}

		/* Paranoia */
		if (lev + lev_diff <= 0) lev_diff = 1-lev;

		/* Count possible monsters */
		for (i = 0; i < z_info->r_max; i++)
		{
			r_ptr = &r_info[i];
			/* Never any monster with force depth */
			if (r_ptr->flags1 & RF1_FORCE_DEPTH) continue;

			/* Never any monster that multiplies */
			if (r_ptr->flags2 & RF2_MULTIPLY) continue;

			/* Never any monster that can't move (that would be too easy) */
			if (r_ptr->flags1 & RF1_NEVER_MOVE) continue;

			/* Count an appropriate monster */
			if (r_ptr->flags1 & RF1_UNIQUE) continue;

			if (r_ptr->level == (lev + lev_diff)) mcount++;
		}

		/* Climb up until you find a suitable monster type */
		if (mcount == 0)
		{
			/* Add some monsters to balance difficulty (sort-of) */
			number += (damroll(2,2) - 1);
			lev_diff--;
		}
	}

	/* choose random monster */
	midx = randint(mcount);

	for (i = 0, mcount = 0; mcount < midx; i++)
	{
		r_ptr = &r_info[i];

		/* Never any monster with force depth */
		if ((r_ptr->flags1 & RF1_FORCE_DEPTH)) continue;
		
		/* Never any monster that multiplies */
		if (r_ptr->flags2 & RF2_MULTIPLY) continue;

		/* Never any monster that can't move (that would be too easy) */
		if (r_ptr->flags1 & RF1_NEVER_MOVE) continue;

		/* Count appropriate monsters */
		if ((r_ptr->flags1 & RF1_UNIQUE)) continue;
		if (r_ptr->level == (lev+lev_diff)) mcount++;
	}

	if (i <= 1) 
	{
		message(MSG_FAIL, 0, "Something is wrong");
		return FALSE;
	}

	/* Paranoia */
	if (number <= 0) number = 1;
	if (number > 63) number = 63;

	if (adult_easy_mode) number = (number + 1) / 2;

	/* Actually write the quest */
	q_info[q].type = QUEST_GUILD;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;
	q_info[q].r_idx = i - 1;
	q_info[q].max_num = number;
	q_info[q].started = FALSE;

	/* Set current quest */
	p_ptr->cur_quest = lev;

	/* Decide on reward type */

	/* Chance of gold reward */
	chance = 90 - ((difficulty - 1) * 20) - (lev * 2); 
	
	/* No negative chances */
	if (chance < 0) chance = 0;

	if (rand_int(100) < chance) q_info[q].reward = REWARD_GOLD;
	else
	{
		/* Chance of good item reward */
		chance = 95 - ((difficulty - 1) * 10) - (lev - 1); 

		if (chance < 0) chance = 0;

		if (rand_int(100) < chance) q_info[q].reward = REWARD_GOOD_ITEM;

		/* Otherwise - a great item reward */
		else q_info[q].reward = REWARD_GREAT_ITEM;
	}

	/*success*/
	return TRUE;
}

/*
 * Display the "contents" of the adventurer's guild 
 */
void display_guild(void)
{
	int i, j;
	cptr q_out;

	/* Describe the guild */
	put_str("The Adventurer's Guild", 3, 30);

	/* Check for outstanding rewards */
	for (i = 0; i < z_info->q_max ; i++)
	{
		if (q_info[i].type == QUEST_GUILD)
		{
			/* Skip incomplete quests */
			if (q_info[i].active_level) continue;

			/* Check to see if there's a reward */
			if (q_info[i].reward == 0) continue;
			else
			{
				/* Create the reward */
				grant_reward(q_info[i].base_level, q_info[i].reward);

				/* Reset the reward */
				q_info[i].reward = 0;

				/* Reset the quest */
				p_ptr->cur_quest = 0;

				/* Clear the screen */
				Term_clear();

				/* Inform the player */
				message(MSG_STORE, 0, "A reward for your efforts is waiting outside!");
			}
		}
	}

	/* Label the quest descriptions */
	if (!p_ptr->cur_quest)
	{
		/* Not Currently in a quest */
		put_str("Available Quests:", 5, 3);

		avail_quest = 0;

		for (j = 0;j < GUILD_QUESTS; j++)
		{
	
			byte attr;
			cptr difficulty;

			if (guild[j] == 99)
			{
				attr = TERM_L_RED;
				difficulty = "A bounty";
			}
			else if (guild[j] <= 1)
			{
				attr = TERM_L_GREEN;
				difficulty = "An easy";
			}
			else if (guild[j] <= 3)
			{
				attr = TERM_GREEN;
				difficulty = "A moderate";
			}
			else if (guild[j] <= 5)
			{
				attr = TERM_YELLOW;
				difficulty = "A difficult";
			}
			else 
			{
				attr = TERM_ORANGE;
				difficulty = "A challenging";
			}

			put_str(format("%c)", I2A(avail_quest)), 7+avail_quest, 0);
			c_put_str(attr, format ("%s quest.",difficulty), 7+avail_quest, 4);
			avail_quest++;
		}
	}

	else 
	{
		put_str("Your Quest:", 5, 3);
		q_out = describe_quest(p_ptr->cur_quest, QMODE_FULL);

		/* Break into two lines if necessary */
		if (strlen(q_out) < 70) put_str(q_out, 7, 3);
		else 
		{
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_1);
			put_str(q_out, 7, 3);
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_2);
			put_str(q_out, 8, 3);
		}
	}
}

/*
 * Choose a quest from the list
 */
static int get_quest(void)
{
	int item;
	char which;
	char buf[160];

	/* Build the prompt */
	sprintf(buf, "(Items %c-%c, ESC to exit)", I2A(0), I2A(avail_quest - 1));

	/* Ask until done */
	while (TRUE)
	{
		/* Escape */
		if (!get_com(buf, &which)) return (-1);

		/* Lowercase */
		which = tolower(which);

		/* Convert response to item */
		item = A2I(which);

		/* Oops */
		if ((item < 0) || (item > avail_quest-1 ))
		{
			/* Oops */
			bell("Illegal guild quest choice!");
			continue;
		}

		break;
	}

	/* Success */
	return (item);
}

/*
 * "Purchase" a quest from the guild
 */
void guild_purchase(void)
{
	int item;
	int i, slot, qlev, num;
	bool found = FALSE;

	/* In a current quest */
	if (p_ptr->cur_quest)
	{
		message(MSG_FAIL, 0, "Finish your current quest first!");
		return;
	}

	/* Get the quest number */
	item = get_quest();
		
	/* Quit if no quest chosen */
	if (item == -1) return;
	
	/* Get level for quest - most likely on the next level but can be deeper */
	qlev = p_ptr->max_depth+1+rand_int(5)-2;
	if (qlev < p_ptr->max_depth+1) qlev = p_ptr->max_depth+1;

	/* Check list of quests */
	for (i = 0; i < z_info->q_max ; i++)
	{
		/* check fixed quests to see that they're not on the same level*/
		if (q_info[i].type == QUEST_FIXED)
		{
			if (q_info[i].active_level == qlev)
			{
				message(MSG_FAIL, 0, "A greater task lies before you!");
				return;
			}
			/* if not problem, skip */
			continue;
		}

		if (q_info[i].type == QUEST_GUILD)
		{
			/* skip completed random quests */
			if (!q_info[i].active_level) continue;
			else 
			{
				message(MSG_FAIL, 0, "You already have an assigned quest!");
				return;
			}
		}

		slot = i;
		found = TRUE;
		
		break;
	}

	if (found)
	{
		/* How many monsters? */
		num = damroll(5,3)+2;
		if (!place_quest(slot, qlev, num, guild[item])) return;
	}

	else message(MSG_FAIL, 0, "You can't accept any more quests!");

	/* Clear screen */
	Term_clear();

	display_guild();

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