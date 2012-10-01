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

#define GUILD_QUESTS	3	/* Number of quests offered at the guild at any given time */

#define GUILD_FAME_SPECIAL	12

static int avail_quest;

/*
 * The Adventurer's guild's selection
 */
static s16b guild[GUILD_QUESTS] = 
{
	2,	5,	8
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

	/* Vault quests */
	if (q_ptr->type == QUEST_VAULT)
	{
		strcpy(intro, "To fulfill your task, you must retrieve the fabled treasure");

		/* The location of the quest */
		if (!depth_in_feet) strcpy(where, format("on dungeon level %d.", level));
		else strcpy(where, format("at a depth of %d feet.", level * 50));

		if (mode == QMODE_SHORT) return (format("%s.", intro));
		else if (mode == QMODE_FULL) return (format("%s %s", intro, where));
		else if (mode == QMODE_HALF_1) return (format("%s", intro));
		else if (mode == QMODE_HALF_2) return (format("%s", where));
	}

	/* Monster quests */
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
	else strcpy(intro, "For some bizarre reason, you should");
			
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
	int i, j;
	bool great = ((type == REWARD_GREAT_ITEM) ? TRUE : FALSE);
	u32b f1, f2, f3;

	object_type *i_ptr, *j_ptr;
 	object_type object_type_body;
	store_type *st_ptr = &store[STORE_HOME];

	/* Generate object at quest level */
	p_ptr->obj_depth = reward_level;

	/* Get local object */
	i_ptr = &object_type_body;
	
	/* Create a gold reward */
	if (type == REWARD_GOLD) 
	{
		for (i = 0; i < 5; i++)
		{
			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make some gold */
			if (!make_gold(i_ptr, 0)) continue;

			/* Drop the object */
			drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
		}
	}
	/* Create an item reward */
	else
	{
		s32b price_threshold = ((p_ptr->au * (10 + p_ptr->fame)) / 100);
		int force_item = rand_int(p_ptr->fame);
		int x_fame;

		/* Boundary check */
		if (price_threshold < p_ptr->au) 
			price_threshold = (p_ptr->au < 10000 ? 10000 : p_ptr->au);

		if (price_threshold > 250000) price_threshold = 250000;

		/* 100 attempts at finding a decent item */
		for (i = 0; i < 100; i++)
		{
			/* Temporary fame */
			x_fame = ((p_ptr->fame - i) < 1) ? 0 : (p_ptr->fame - i);

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make a good (or great) object (if possible) */
			if (!make_object(i_ptr, TRUE, great, FALSE)) continue;

			/* It is identified */
			object_known(i_ptr);

			/* Sometimes give inappropriate rewards */
			if (i > p_ptr->fame) force_item = rand_int(p_ptr->fame);

			if (force_item < 2) break;

			/* Relatively expensive items are always appropriate */
			if ((i_ptr->number * object_value(i_ptr)) > price_threshold) break;

			/* Check for appropriateness */
			if (i_ptr->tval == TV_MAGIC_BOOK)
			{
				bool already_own_book = FALSE;

				/* Spellbooks - first, check if you can use them */
				if (!cp_ptr->spell_book[i_ptr->sval]) continue;

				/* Sometimes, check if we already have them */
				if (rand_int(p_ptr->fame) < 15)	break;

				/* Look for item in the pack */
				for (j = 0; j < INVEN_PACK; j++)
				{
					/* Get the item */
					j_ptr = &inventory[j];

					if ((j_ptr->tval == i_ptr->tval) && (j_ptr->sval == i_ptr->sval)) 
						already_own_book = TRUE;
				}

				/* Look for item in home */
				if (st_ptr->stock_num)
				{
					for (j = 0; j < st_ptr->stock_num; j++)
					{
						j_ptr = &st_ptr->stock[j];
						if ((j_ptr->tval == i_ptr->tval) && (j_ptr->sval == i_ptr->sval)) 
							already_own_book = TRUE;
					}
 				}

				if (already_own_book) continue;

				break;
			}
			else if (wearable_p(i_ptr) && (i_ptr->tval != TV_RING))
			{
				/* Wearable items - compare to item already in slot */
				j = wield_slot(i_ptr);

				j_ptr = &inventory[j];

				/* Compare value of the item with the old item */
				if (j_ptr->k_idx)
				{
					if (object_value(i_ptr) <= ((object_value(j_ptr) * (90 + x_fame)) / 100))
						continue;
				}
				
				/* Sometimes, more sophisticated checks */
				if (rand_int(p_ptr->fame) < 7) break;

				/* Weapons - additional checks */
				if (j == INVEN_WIELD)
				{
					object_flags(i_ptr, &f1, &f2, &f3);

					if (cp_ptr->flags & CF_BLESS_WEAPON)
					{
						/* Limit to legal weapon types */
						if ((i_ptr->tval != TV_HAFTED) && !(f2 & TR2_BLESSED)) continue;
					}
					
					/* Too heavy */
					if (adj_str_hold[p_stat(A_STR)] < actual_weight(i_ptr) / 10) continue;
				}

				/* Gloves - additional checks */
				if (j == INVEN_HANDS)
				{
					object_flags(i_ptr, &f1, &f2, &f3);

					if (cp_ptr->flags & CF_NO_GLOVE)
					{
						/* Limit to legal glove types */
						if (!(f2 & (TR2_FREE_ACT)) && !((i_ptr->pval > 0) && 
							((f1 & (TR1_DEX)) || (f1 & (TR1_MANA)))))							
							continue;
					}
				}

				break;
			}
			else if ((i_ptr->tval == TV_RING))
			{
				/* Rings - compare to cheapest of worn rings */
				j = (object_value(&inventory[INVEN_LEFT]) > object_value(&inventory[INVEN_RIGHT]))
					? INVEN_LEFT : INVEN_RIGHT;

				j_ptr = &inventory[j];

				if (!j_ptr->k_idx) break;

				/* Compare value of the item with the old item */
				if (object_value(i_ptr) <= ((object_value(j_ptr) * (90 + x_fame)) / 100))
					continue;
				
				break;
			}
			else if (ammo_p(i_ptr))
			{
				/* If you can't use the ammo, inappropriate */
				if (p_ptr->ammo_tval != i_ptr->tval) continue;

				/* For high fame, ammo rewards are rare */
				if (randint(p_ptr->fame) > 30) continue;

				/* If you can, appropriate */
				break;
			}

			/* Other item types are always appropriate */
			break;
		}

		/* Identify it */
		object_aware(i_ptr);

		/* Mark history */
		i_ptr->origin_nature = ORIGIN_REWARD;
		i_ptr->origin_dlvl = reward_level;

		/* Handle Artifacts */
		if (artifact_p(i_ptr))
		{
			artifact_type *a_ptr = &a_info[i_ptr->a_idx];

			/* Mark the item as fully known */
			if (cp_ptr->flags & CF_LORE) artifact_known(&a_info[i_ptr->a_idx]);

			/* Mark the artifact as "aware" */
			artifact_aware(&a_info[i_ptr->a_idx]);

			/* Describe it fully */
			if artifact_known_p(a_ptr) 
			{
				/* Track the object */
				object_actual_track(i_ptr);

				/* Hack -- Handle stuff */
				handle_stuff();
	
				screen_object(i_ptr, TRUE);
			}
		}

		/* Drop the object */
		drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
	}

	/* Reset object level */
	p_ptr->obj_depth  = p_ptr->depth;
}

/*
 * Actually give the character a quest
 */
static bool place_mon_quest(int q, int lev, int number, int difficulty)
{
	int i, chance;
	int mcount = 0;
	sint lev_diff;
	monster_race *r_ptr;
	int *monster_idx;

	/* Allocate the "monster_idx" array */
	C_MAKE(monster_idx, z_info->r_max, int);

	/* Monsters can be up to 3 levels out of depth with difficulty 0 */
	lev_diff = 3 + difficulty;

	while (mcount < 5)
	{
		/* After trying for a while, give up */
		if ((lev_diff < (-50)) || (lev + lev_diff < 1)) break;

		/* There's a chance of climbing up */
		if ((lev_diff < (difficulty - 3)) || (lev + lev_diff <= 1)) chance = 0;
		else if (lev_diff > difficulty) chance = 80;
		else chance = 20;

		if (rand_int(100) < chance) 
		{
			if (lev_diff <= difficulty) number += (damroll(2,2) - 1);
			lev_diff--;
			continue;
		}

		/* Paranoia */
		if (lev + lev_diff <= 0) lev_diff = 1 - lev;

		/* Count possible monsters */
		for (i = 0; i < z_info->r_max; i++)
		{
			r_ptr = &r_info[i];

			/* Check for appropriate level */
			if (r_ptr->level != (lev + lev_diff)) continue;
			
			/* Never any monster that multiplies */
			if (r_ptr->flags1 & RF1_MULTIPLY) continue;

			/* Check if a monster that can't move can still hurt the player from a distance */
			if (r_ptr->flags2 & RF2_NEVER_MOVE)
			{
				bool okay = FALSE;

				/* Allow if the monster can summon */
				if ((r_ptr->s_flags1 & (SRF1_SUMMON_MASK)) ||
					(r_ptr->s_flags2 & (SRF2_SUMMON_MASK)) || 
					(r_ptr->s_flags3 & (SRF3_SUMMON_MASK)))
					okay = TRUE;
				/* Allow if the monster can attack */
				if ((r_ptr->s_flags1 & (SRF1_ATTACK_MASK)) ||
					(r_ptr->s_flags2 & (SRF2_ATTACK_MASK)) || 
					(r_ptr->s_flags3 & (SRF3_ATTACK_MASK)))
					okay = TRUE;
				/* Allow if the monster can bring the player to itself */
				if ((r_ptr->s_flags1 & (SRF1_TACTIC_FAR_MASK)) ||
					(r_ptr->s_flags2 & (SRF2_TACTIC_FAR_MASK)) || 
					(r_ptr->s_flags3 & (SRF3_TACTIC_FAR_MASK)))
					okay = TRUE;

				if (!okay) continue;
			}

			/* No uniques */
			if (r_ptr->flags1 & RF1_UNIQUE) continue;

			/* Allow monster */
			monster_idx[mcount++] = i;
		}

		/* Climb up until you find a suitable monster type */
		if (mcount < 5)
		{
			/* Add some monsters to balance difficulty (sort-of) */
			if (mcount == 0) number += (damroll(2,2) - 1);

			lev_diff--;
		}
	}

	/* Paranoia */
	if (mcount == 0) 
	{
		/* No monsters - no quest */
		message(MSG_FAIL, 0, "There are no elligable monsters to quest for");

		/* XXX XXX Free the "monster_idx" array */
		KILL(monster_idx);

		return FALSE;
	}

	/* choose random monster */
	i = rand_int(mcount);

	/* Paranoia */
	if (number <= 0) number = 1;
	if (number > 63) number = 63;

	if (adult_easy_mode) number = (number + 1) / 2;

	/* Actually write the quest */
	q_info[q].type = QUEST_GUILD;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;
	q_info[q].r_idx = monster_idx[i];
	q_info[q].max_num = number;
	q_info[q].cur_num = 0;
	q_info[q].started = FALSE;

	/* Set current quest */
	p_ptr->cur_quest = lev;

	/* Decide on reward type */

	/* Chance of gold reward */
	chance = 90 - ((difficulty - 1) * 20) - (lev * 2); 
	
	/* No negative chances */
	if (chance < 0) chance = 0;

	/* First roll for gold award */
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

	/* XXX XXX Free the "monster_idx" array */
	KILL(monster_idx);

	/*success*/
	return TRUE;
}

/*
 * Actually give the character a quest
 */
static bool place_vault_quest(int q, int lev)
{
	/* Actually write the quest */
	q_info[q].type = QUEST_VAULT;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;
	q_info[q].reward = REWARD_GREAT_ITEM;
	q_info[q].cur_num = q_info[q].max_num = 0;
	q_info[q].started = 0;

	/* Set current quest */
	p_ptr->cur_quest = lev;

	/*success*/
	return TRUE;
}

/*
 * Display the "contents" of the adventurer's guild 
 */
void display_guild(void)
{
	int i, j;
	byte attr;
	cptr p, q_out;

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

				/* Grant fame bonus */
				p_ptr->fame += randint(q_info[i].reward + 1);

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

		if (q_info[i].type == QUEST_VAULT)
		{
			/* Check to see if there's a reward */
			if (q_info[i].reward == 0) continue;
			else
			{
				int j = quest_item_slot();

				if (j == -1) continue;

				/* Create the reward */
				grant_reward(q_info[i].base_level, q_info[i].reward);

				/* Grant fame bonus */
				p_ptr->fame += 5;

				/* Finish the quest */
				q_info[i].active_level = 0;

				/* Reset the reward */
				q_info[i].reward = 0;

				/* Reset the quest */
				p_ptr->cur_quest = 0;

				/* Clear the screen */
				Term_clear();
			
				/* Destroy a potion in the pack */
				inven_item_increase(j, -1);
				inven_item_optimize(j);

				/* Inform the player */
				message(MSG_STORE, 0, "A reward for your efforts is waiting outside!");
			}
		}
	}

	/* Player's title */
	if (p_ptr->fame > 110) p = "oh glorious one";
	else if (p_ptr->fame > 80) p = "oh great one";
	else if (p_ptr->fame > 50)
	{
		if (p_ptr->psex == SEX_MALE) p = "my lord";
		else p = "my lady";
	}
	else if (p_ptr->fame > 25)
	{
		if (p_ptr->psex == SEX_MALE) p = "sir";
		else p = "madam";
	}
	else if (p_ptr->fame > 10)
	{
		if (!op_ptr->full_name[0])
		{
			p = c_name + cp_ptr->name;
		}
		else p = op_ptr->full_name;
	}
	else p = p_name + rp_ptr->name;

	/* Introduction */
	put_str(format("Welcome to the Adventurer's Guild, %s.", p), 5, 3);

	/* Player's reputation */
	switch (p_ptr->fame / 5)
	{
		case 0:
		{
			attr = TERM_RED;
			p = "poor";
			break;
		}
		case 1:
		case 2:
		case 3:
		{
			attr = TERM_YELLOW;
			p = "fair";
			break;
		}
		case 4:
		case 5:
		case 6:
		{
			attr = TERM_YELLOW;
			p = "good";
			break;
		}
		case 7:
		case 8:
		{
			attr = TERM_YELLOW;
			p = "very good";
			break;
		}
		case 9:
		case 10:
		{
			attr = TERM_L_GREEN;
			p = "excellent";
			break;
		}
		case 11:
		case 12:
		case 13:
		{
			attr = TERM_L_GREEN;
			p = "superb";
			break;
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			attr = TERM_L_GREEN;
			p = "heroic";
			break;
		}
		default:
		{
			attr = TERM_L_GREEN;
			p = "legendary";
			break;
		}
	}

	/* Quest count */
	put_str("You might be interested to know that your current reputation is", 6, 3);
	c_put_str(attr, p, 6, 67);

	/* Label the quest descriptions */
	if (!p_ptr->cur_quest)
	{
		/* Not Currently in a quest */
		put_str("Available Quests:", 8, 3);

		avail_quest = 0;

		for (j = 0;j < GUILD_QUESTS; j++)
		{
			if (guild[j] <= 3)
			{
				attr = TERM_L_GREEN;
				p = "An easy";
			}
			else if (guild[j] <= 7)
			{
				attr = TERM_YELLOW;
				p = "A moderate";
			}
			else
			{
				attr = TERM_ORANGE;
				p = "A difficult";
			}

			put_str(format("%c)", I2A(avail_quest)), 10 + avail_quest, 0);
			c_put_str(attr, format ("%s quest.", p), 10 + avail_quest, 4);
			avail_quest++;
		}

		if (p_ptr->fame % GUILD_FAME_SPECIAL == 7)
		{
			put_str(format("%c)", I2A(avail_quest)), 10 + avail_quest, 0);
			c_put_str(TERM_VIOLET, "A special quest.", 10 + avail_quest, 4);
			avail_quest++;
		}
	}

	else 
	{
		put_str("Your current quest:", 8, 3);
		q_out = describe_quest(p_ptr->cur_quest, QMODE_FULL);

		/* Break into two lines if necessary */
		if (strlen(q_out) < 70) put_str(q_out, 10, 3);
		else 
		{
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_1);
			put_str(q_out, 10, 3);
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_2);
			put_str(q_out, 11, 3);
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
		if ((item < 0) || (item > avail_quest - 1 ))
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
	if (!p_ptr->max_depth) qlev = 1;
	else qlev = p_ptr->max_depth + 1 + randint(2);

	if (qlev >= MAX_DEPTH) 
	{
		message(MSG_FAIL, 0, "You have reached the lower depths of the dungeon!");
		return;
	}

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

		slot = i;
		found = TRUE;
		
		break;
	}

	if (found)
	{
		if (item < GUILD_QUESTS)
		{
			/* How many monsters? */
			num = damroll(4,4) + 1 + (p_ptr->fame / 30);
			if (!place_mon_quest(slot, qlev, num, guild[item])) return;
		}
		else if (!place_vault_quest(slot, qlev)) return;
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

		if (o_ptr->tval == TV_QUEST) return (i);
	}

	/* No quest item */
	return -1;
}

/*
 * Fail your quest
 */
void quest_fail(void)
{
	quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

	/* Mark quest as completed */
	q_ptr->active_level = 0;
	p_ptr->cur_quest = 0;
				
	/* No reward for failed quest */
	q_ptr->reward = 0;

	/* Message */
	message(MSG_QUEST_FAIL, 0, "You have failed in your quest!");

	if (p_ptr->fame) p_ptr->fame /= 2;
	
	/* Disturb */
	if (disturb_minor) disturb(0);
}
