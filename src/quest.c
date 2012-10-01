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

static s16b current;
static int avail_quest;

/*
 * The Adventurer's guild's selection
 */
static s16b guild[GUILD_QUESTS] = 
{
	{0},
	{2},
	{5},
	{8},
};

/*
 * Fix plural names of monsters
 *
 * Taken from PernAngband 
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
		|| (streq(&(Name[NameLen-2]), "ua")))
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
	int i;
	char name[80];
	char intro[80];
	char targets[80];
	char where[80];

	/* Check quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Check for quest */
		if (q_ptr->active_level == level)
		{
			monster_race *r_ptr = &r_info[q_ptr->r_idx];

			strcpy(name, (r_name + r_ptr->name));

			/* Unique quest monster */
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				strcpy(targets,name);
			}

			/* Multiple quest monsters */
			else if ((q_ptr->max_num - q_ptr->cur_num) > 1)
			{
				plural_aux(name);
				strcpy(targets,format("%d %s",(q_ptr->max_num - q_ptr->cur_num), name));
			}

			/* One quest monster */
			else
			{
				if (is_a_vowel(name[0])) strcpy(targets,format("an %s", name));
				else strcpy(targets,format("a %s", name));
			}

			/* The type of the quest */
			if (q_ptr->type == QUEST_FIXED) strcpy(intro,"For eternal glory, you must");
			else if (q_ptr->type == QUEST_GUILD) strcpy(intro,"To fulfill your task, you must");

			/* Paranoia */
			else strcpy(intro,"For some unexplainable reason, you should");
			
			/* The location of the quest */
			if (!depth_in_feet) strcpy(where,format("on dungeon level %d.", level));
			else strcpy(where,format("at a depth of %d feet.", level*50));

			if (mode == QMODE_SHORT) return (format("%s kill %s.",intro,targets));
			else if (mode == QMODE_FULL) return (format("%s kill %s %s", intro, targets, where));
			else if (mode == QMODE_HALF_1) return (format("%s kill %s",intro,targets));
			else if (mode == QMODE_HALF_2) return (format("%s", where));
		}
	}
	
	/* No quest */
	return NULL;
}

/*
 * Give a reward to the player 
 */
static void grant_reward(byte reward_level, byte type)
{
	int oldlevel,i;

	object_type *i_ptr;
	object_type object_type_body;

	oldlevel = object_level;
	object_level = reward_level;

	if (type == REWARD_GOLD) 
	{
		for (i = 0;i < 5;i++)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make some gold */
			if (!make_gold(i_ptr)) continue;

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
		}
	}

	else acquirement(p_ptr->py,p_ptr->px,1,(bool)(type == REWARD_GREAT_ITEM));

	object_level = oldlevel;
}

/*
 * Actually give the character a quest
 */
static bool place_quest(int q, int lev, int number, int difficulty, bool unique)
{
	int i, mcount, midx;
	int chance;
	sint lev_diff;
	monster_race *r_ptr;
	monster_lore *l_ptr;

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
		if ((lev_diff<(difficulty-3)) || lev+lev_diff<=1) chance = 0;
		else if (lev_diff>difficulty) chance = 80;
		else chance = 20;

		if (rand_int(100) < chance) 
		{
			if (lev_diff<=difficulty) number += (damroll(2,2)-1);
			lev_diff--;
			continue;
		}

		/* Paranoia */
		if (lev+lev_diff <= 0) lev_diff = 1-lev;

		/* Count possible monsters */
		for (i = 0; i < z_info->r_max;i++)
		{
			r_ptr = &r_info[i];
			l_ptr = &l_list[i];

			/* Never any monster with force depth */
			if (r_ptr->flags1 & RF1_FORCE_DEPTH) continue;

			/* Never any monster that multiplies */
			if (r_ptr->flags2 & RF2_MULTIPLY) continue;

			/* Never any monster that can't move (that would be too easy) */
			if (r_ptr->flags1 & RF1_NEVER_MOVE) continue;

			/* Count an appropriate monster */
			if (unique)
			{
				if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;
				if (l_ptr->r_pkills) continue;

				if (r_ptr->level == (lev+lev_diff)) mcount++;
			}
			else 
			{
				if (r_ptr->flags1 & RF1_UNIQUE) continue;

				if (r_ptr->level == (lev+lev_diff)) mcount++;
			}
		}

		/* Climb up until you find a suitable monster type */
		if (mcount == 0)
		{
			/* Add some monsters to balance difficulty (sort-of) */
			number += (damroll(2,2)-1);
			lev_diff--;
		}
	}

	/* choose random monster */
	midx = randint(mcount);

	for (i = 0, mcount = 0; mcount < midx; i++)
	{
		r_ptr = &r_info[i];
		l_ptr = &l_list[i];

		/* Never any monster with force depth */
		if ((r_ptr->flags1 & RF1_FORCE_DEPTH)) continue;
		
		/* Never any monster that multiplies */
		if (r_ptr->flags2 & RF2_MULTIPLY) continue;

		/* Never any monster that can't move (that would be too easy) */
		if (r_ptr->flags1 & RF1_NEVER_MOVE) continue;

		/* Count appropriate monsters */
		if (unique)
		{
			if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;
			if (l_ptr->r_pkills) continue;
			if (r_ptr->level == (lev+lev_diff)) mcount++;
		}
		else 
		{
			if ((r_ptr->flags1 & RF1_UNIQUE)) continue;
			if (r_ptr->level == (lev+lev_diff)) mcount++;
		}
	}

	if (i <= 1) 
	{
		message(MSG_FAIL, 0, "Something is wrong");
		return FALSE;
	}

	/* Paranoia */
	if (number <= 0) number = 1;
	if (number > 63) number = 63;

	/* Actually write the quest */
	q_info[q].type = QUEST_GUILD;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;
	q_info[q].r_idx = i-1;
	q_info[q].max_num = (unique) ? 1 : number;

	/* Decide on reward type */

	/* 100% for gold reward, modified by difficulty and dlev*/
	chance = 100 - (difficulty*15) - (lev/3); 
	
	/* No negative chances */
	if (chance < 0) chance = 0;

	if (rand_int(100)<chance) q_info[q].reward = REWARD_GOLD;
	else
	{
		/* 85% for good item reward, modified by difficulty and dlev*/
		chance = 85 - (difficulty*5) - (lev/5); 

		if (chance < 0) chance = 0;

		if (rand_int(100)<chance) q_info[q].reward = REWARD_GOOD_ITEM;

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
	cptr curquest;
	bool legal = TRUE;

	/* Describe the guild */
	put_str("The Adventurer's Guild", 3, 30);

	current = FALSE;

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
				/* Inform the player */
				message(MSG_STORE, 0, "A reward for your efforts is waiting outside!");
				
				/* Create the reward */
				grant_reward(q_info[i].base_level,q_info[i].reward);

				/* Reset the reward */
				q_info[i].reward = 0;
			}
		}
	}

	/* Check if there are no current quests */
	for (i = 0; i < z_info->q_max ; i++)
	{
		if (q_info[i].type == QUEST_GUILD)
		{
			/* skip completed random quests */
			if (!q_info[i].active_level) continue;
			else 
			{
				current = q_info[i].active_level;
				break;
			}
		}
	}

	/* Label the object descriptions */
	if (!current) 
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
				difficulty = "A challanging";
			}

			put_str(format("%c)", I2A(avail_quest)), 7+avail_quest, 0);
			c_put_str(attr, format ("%s quest.",difficulty), 7+avail_quest, 4);
			avail_quest++;
		}
	}

	else 
	{
		put_str("Your Quest:", 5, 3);
		curquest = describe_quest(current,QMODE_FULL);

		/* Break into two lines if necessary */
		if (strlen(curquest) < 70) put_str(curquest, 7, 3);
		else 
		{
			curquest = describe_quest(current,QMODE_HALF_1);
			put_str(curquest, 7, 3);
			curquest = describe_quest(current,QMODE_HALF_2);
			put_str(curquest, 8, 3);
		}
	}
}

/*
 * Choose a quest from the list
 */
static bool get_quest(int *com_val)
{
	int item;
	char which;
	char buf[160];

#ifdef ALLOW_REPEAT

	/* Get the item index */
	if (repeat_pull(com_val))
	{
		/* Verify the item */
		if ((*com_val >= 0) && (*com_val <= (avail_quest - 1)))
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	/* Assume failure */
	*com_val = (-1);

	/* Build the prompt */
	sprintf(buf, "(Items %c-%c, ESC to exit)", I2A(0), I2A(avail_quest - 1));

	/* Ask until done */
	while (TRUE)
	{
		bool verify;

		/* Escape */
		if (!get_com(buf, &which)) return (FALSE);

		/* Note verify */
		verify = (isupper(which) ? TRUE : FALSE);

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

		/* No verification */
		if (!verify) break;
	}

	/* Save item */
	(*com_val) = item;

#ifdef ALLOW_REPEAT

	repeat_push(*com_val);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
}

/*
 * "Purchase" a quest from the guild
 */
void guild_purchase(void)
{
	int item;
	int i, slot, qlev, num;
	bool unique;
	bool found = FALSE;

	/* In a current quest */
	if (current)
	{
		message(MSG_FAIL, 0, "Finish your current quest first!");
		return;
	}

	/* Get the object number to be bought */
	if (!get_quest(&item)) return;
	
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

		if (!found) slot = i;
		found = TRUE;
	}

	if (found)
	{
		unique = FALSE;
		if (guild[item] == 99) unique = TRUE;
		
		/* How many monsters? */

		if (unique) num = 1;
		else num = damroll(4,4)+2;
		if (!place_quest(slot,qlev,num,(unique) ? 0 : guild[item],unique)) return;
	}

	else message(MSG_FAIL, 0, "You can't accept any more quests!");

	/* Clear screen */
	Term_clear();

	display_guild();

}

/*
 * Hack -- Check if a level is a "quest" level - returns quest type
 */
byte quest_check(int level)
{
	int i;

	/* Town is never a quest */
	if (!level) return (FALSE);

	/* Check quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Check for quest */
		if (q_info[i].active_level == level) return (q_info[i].type);
	}

	/* Nope */
	return 0;
}
