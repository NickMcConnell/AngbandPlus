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

/* 
 * Actually place a random quest in a given slot 
 */
/*
 * Fix plural names of monsters
 *
 * Taken from PernAngband 
 */

s16b current;
int avail_quest;

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
	else if ((strstr(Name, "Manes")) || (strstr(Name, "Pikachu")) || (strstr(Name, "Yeti")))
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
	else if (streq(&(Name[NameLen-4]), "lman"))
	{
		strcpy (&(Name[NameLen-4]), "lmen");
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

cptr quest_feeling(s16b level, bool full)
{
	int i;
	char name[80];
	char part1[80];
	char part2[80];

	/* Check quests */
	for (i = 0; i < z_info->q_max; i++)
	{		
		quest *q_ptr = &q_info[i];
	
		/* Check for quest */
		if (q_ptr->active_level == level)
		{
			monster_race *r_ptr = &r_info[q_ptr->r_idx];

			strcpy(name, (r_name + r_ptr->name));

			/* Unique quest monster */
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				strcpy(part2,name);
			}

			/* Multiple quest monsters */
			else if ((q_ptr->max_num - q_ptr->cur_num) > 1)
			{
				plural_aux(name);
				strcpy(part2,format("%d %s",(q_ptr->max_num - q_ptr->cur_num), name));
			}

			/* One quest monster */
			else
			{
				if (is_a_vowel(name[0])) strcpy(part2,format("an %s", name));
				else strcpy(part2,format("a %s", name));
			}

			if (q_ptr->type == QUEST_FIXED) strcpy(part1,"For eternal glory, you must");
			else if (q_ptr->type == QUEST_GUILD) strcpy(part1,"To fulfill your task, you must");
			else strcpy(part1,"For some wierd reason, you should");

			if (!full) return (format("%s kill %s.",part1,part2));
			else return (format("%s kill %s on level %d.", part1, part2, level));
		}
	}

	return NULL;
}

static bool place_quest(int q, int lev, int number, int difficulty, bool unique)
{
	int i, mcount, midx;
	int lev_diff, chance;
	monster_race *r_ptr;
	monster_lore *l_ptr;


	mcount = 0;

	/* Monsters can be up to 5 levels out of depth */
	lev_diff = 5 + difficulty;

	while (mcount == 0)
	{
		if ((lev_diff<(-50)) || (lev+lev_diff < 1))
		{
			msg_print("There are no elligable monsters to quest for");
			return FALSE;
		}

		/* There's a chance of climbing up */
		if ((lev_diff<(difficulty-5)) || lev+lev_diff==1) chance = 0;
		else if (lev_diff>difficulty) chance = 85;
		else chance = 20;

		if (rand_int(100) < chance) 
		{
			if (lev_diff<=difficulty) number++;
			lev_diff--;
			continue;
		}

		/* Count possible monsters */
		for (i = 0; i < z_info->r_max;i++)
		{
			r_ptr = &r_info[i];
			l_ptr = &l_list[i];

			/* Never any monster with force depth */
			if ((r_ptr->flags1 & RF1_FORCE_DEPTH)) continue;
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

		/*Climb up until you find a suitable monster type */
		if (mcount == 0)
		{
			/* Add a monster to balance difficulty (sort-of) */
			number++;
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
		msg_print("Something is wrong");
		return FALSE;
	}

	/* Actually write the quest */
	q_info[q].type = QUEST_GUILD;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;
	q_info[q].r_idx = i-1;
	q_info[q].max_num = (unique) ? 1 : number;

	/*success*/
	return TRUE;
}


void display_guild(void)
{
	int i, j;
	bool legal = TRUE;

	/* Put the owner name */
	put_str("The Adventurer's Guild", 3, 30);

	current = FALSE;

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
		put_str(quest_feeling(current,TRUE), 7, 3);
	}

}

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
		if (item < 0)
		{
			/* Oops */
			bell("Illegal store object choice!");

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
 * Buy an object from a store
 */
void guild_purchase(void)
{
	int item;
	int i, slot, qlev;
	bool unique;
	bool found = FALSE;

	/* In a current quest */
	if (current)
	{
		msg_print("Finish your current quest first!");
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
				msg_print("A greater task lies before you!");
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
				msg_print("You already have an assigned quest!");
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
		if (!place_quest(slot,qlev,(unique) ? 1 : randint(3),
			(unique) ? 0 : guild[item],unique)) return;
	}

	msg_print("You can't accept any more quests!");

	/* Clear screen */
	Term_clear();

	display_guild();

}