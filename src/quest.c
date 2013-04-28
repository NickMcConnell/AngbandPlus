/* File: quest.c */

/*
 * Handle random quests, as supplied by the Inn.
 *
 * Copyright (c) 2002
 * Eytan Zweig, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * This file comes from EyAngband 0.5.0.
 */

#include "angband.h"

/* Number of quests offered at the Inn at any given time */
#define GUILD_QUESTS    3

static int avail_quest;

/*
 * The Inn's quest selection
 */
static s16b inn_quests[GUILD_QUESTS] =
{
	2,	5,	8
};



/*
 * Pluralize a monster name.  From Zangband, etc.
 */
void plural_aux(char *name)
{
	int name_len = strlen(name);

	if (strstr(name, "Mimic ("))
	{
		cptr aider = strstr(name, " (");
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
			strcpy(&(dummy[i]), "es");
			i++;
		}
		else
		{
			strcpy(&(dummy[i]), "s");
		}

		strcpy(&(dummy[i + 1]), aider);
		strcpy(name, dummy);
	}

	else if (strstr(name, " of "))
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
			strcpy(&(dummy[i]), "es");
			i++;
		}
		else
		{
			strcpy(&(dummy[i]), "s");
		}

		strcpy(&(dummy[i + 1]), aider);
		strcpy(name, dummy);
	}
	else if ((strstr(name, "coins")) || (strstr(name, "gems")))
	{
		char dummy[80];
		strcpy(dummy, "piles of ");
		strcat(dummy, name);
		strcpy(name, dummy);
		return;
	}
	else if ((strstr(name, "Manes")) || (name[name_len-1] == 'u') ||
		(streq(&(name[name_len-2]), "ua")) ||
		(streq(&(name[name_len-3]), "nee")) ||
		(streq(&(name[name_len-4]), "idhe")))
	{
		return;
	}
	else if (streq(&(name[name_len - 2]), "ey"))
	{
		strcpy(&(name[name_len - 2]), "eys");
	}
	else if (name[name_len - 1] == 'y')
	{
		strcpy(&(name[name_len - 1]), "ies");
	}
	else if (streq(&(name[name_len - 4]), "ouse"))
	{
		strcpy(&(name[name_len - 4]), "ice");
	}
	else if (streq(&(name[name_len - 4]), "lung"))
	{
		strcpy (&(name[name_len - 4]), "lungen");
	}
	else if (streq(&(name[name_len - 3]), "sus"))
	{
		strcpy (&(name[name_len - 3]), "si");
	}
	else if (streq(&(name[name_len - 2]), "us"))
	{
		strcpy(&(name[name_len - 2]), "i");
	}
	else if (streq(&(name[name_len - 6]), "kelman"))
	{
		strcpy(&(name[name_len - 6]), "kelmen");
	}
	else if (streq(&(name[name_len - 8]), "wordsman"))
	{
		strcpy(&(name[name_len - 8]), "wordsmen");
	}
	else if (streq(&(name[name_len - 7]), "oodsman"))
	{
		strcpy(&(name[name_len - 7]), "oodsmen");
	}
	else if (streq(&(name[name_len - 7]), "eastman"))
	{
		strcpy(&(name[name_len - 7]), "eastmen");
	}
	else if (streq(&(name[name_len - 8]), "izardman"))
	{
		strcpy(&(name[name_len - 8]), "izardmen");
	}
	else if (streq(&(name[name_len - 5]), "geist"))
	{
		strcpy(&(name[name_len - 5]), "geister");
	}
	else if (streq(&(name[name_len - 2]), "ex"))
	{
		strcpy(&(name[name_len - 2]), "ices");
	}
	else if ((name[name_len - 1] == 'f') && (!streq(&(name[name_len - 2]), "ff")))
	{
		strcpy (&(name[name_len - 1]), "ves");
	}
	else if (suffix(name, "ch") ||
			 suffix(name, "sh") ||
			 suffix(name, "nx") || suffix(name, "s") || suffix(name, "o"))
	{
		strcpy(&(name[name_len]), "es");
	}
	else
	{
		strcpy(&(name[name_len]), "s");
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

	cptr s_kill = "kill";

	quest_type *q_ptr = &q_info[q_idx];
	monster_race *r_ptr = &r_info[q_ptr->r_idx];

	if (!q_idx) return (NULL);

	/* Monster name */
	strcpy(name, r_name + r_ptr->name);


	/* Multiple quest monsters */
	if (((q_ptr->max_num - q_ptr->cur_num) > 1) ||
	    ((!q_ptr->active_level) && (q_ptr->max_num > 1)))
	{
		int num = (q_ptr->active_level ?
			q_ptr->max_num - q_ptr->cur_num : q_ptr->max_num);

		plural_aux(name);
		my_strcpy(targets, format("%d %s", num, name), sizeof(targets));
	}

	/* One (remaining) quest monster */
	else
	{
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			my_strcpy(targets, format("%s", name), sizeof(targets));
			s_kill = "defeat";
		}

		else if (is_a_vowel(name[0]))
		     my_strcpy(targets, format("an %s", name), sizeof(targets));
		else my_strcpy(targets, format("a %s", name), sizeof(targets));
	}

	/* The type of the quest */
	if (q_ptr->type == QUEST_FIXED)
	     strcpy(intro, "For eternal glory, you must");
	else strcpy(intro, "To fulfill your task, you must");

	/* The location of the quest */
	if (!depth_in_feet) strcpy(where, format("on dungeon level %d.", level));
	else
	{
		if (!use_metric) strcpy(where, format("at a depth of %d feet.",
			level * 50));
		else strcpy(where, format("at a depth of %d meters.", level * 15));
	}


	/* Quest is complete */
	if (!q_ptr->active_level)
	{
		return (format("You have slain %s %s.", targets, where));
	}

	/* Output */
	if (mode == QMODE_SHORT) return (format("%s %s %s.", intro, s_kill, targets));
	else if (mode == QMODE_FULL) return (format("%s %s %s %s", intro, s_kill,
		targets, where));
	else if (mode == QMODE_HALF_1) return (format("%s %s %s", intro, s_kill, targets));
	else if (mode == QMODE_HALF_2) return (format("%s", where));

	/* Paranoia */
	return (NULL);
}


/*
 * Make sure that quest monsters are present.  Because of earthquakes,
 * monsters fighting each other, etc., there is no alternative but to
 * check this constantly.
 *
 * This code is paranoid because it has to be.
 */
void insure_quest_monsters(void)
{
	int q_idx = quest_num(p_ptr->cur_quest);
	quest_type *q_ptr = &q_info[q_idx];
	monster_race *r_ptr = &r_info[q_ptr->r_idx];
	int y, x;

	/* This is an active random quest */
	if ((q_ptr->active_level) && (q_ptr->type == QUEST_RANDOM))
	{
		/* There are not enough remaining monsters */
		if ((!(r_ptr->flags1 & (RF1_UNIQUE))) &&
			 ((q_ptr->max_num - q_ptr->cur_num) > r_ptr->cur_num))
		{
			/* Place some more monsters */
			while (TRUE)
			{
				/* Find a legal, distant, unoccupied, space */
				while (TRUE)
				{
					/* Pick a location */
					y = rand_int(dungeon_hgt);
					x = rand_int(dungeon_wid);

					/* Require a grid that the monster can exist in. */
					if (!cave_exist_mon(r_ptr, y, x, FALSE, FALSE)) continue;

					/* Accept only far away grids */
					if (distance(y, x, p_ptr->py, p_ptr->px) > MAX_SIGHT) break;
				}

				/* Place another monster -- compact monsters when necessary */
				if (!place_monster_aux(y, x, q_ptr->r_idx, FALSE, FALSE))
				{
					compact_monsters(64);
				}

				/* Stop when we have enough monsters */
				if (r_ptr->cur_num >= q_ptr->max_num - q_ptr->cur_num) break;
			}
		}
	}
}


/*
 * Give a reward to the player
 *
 * Hack - The entire "tailored item" routine makes quite a few assumptions
 * as to what's a good reward, plus it relies a lot of item price to make
 * deductions. It is basically a list of special cases.
 */
static void grant_reward(byte reward_level, byte type)
{
	int i, j;
	bool great = ((type == REWARD_GREAT_ITEM) ? TRUE : FALSE);
	u32b f1, f2, f3;

	object_type *i_ptr, *j_ptr;
	object_type forge;
	store_type *st_ptr = &store[STORE_HOME];

	/* Generate object at quest level */
	object_level = reward_level;

	/* Get local object */
	i_ptr = &forge;

	/* Create a gold reward */
	if (type == REWARD_GOLD)
	{
		/* Make the gold a little more interesting */
		object_level += 10;

		for (i = 0; i < 5; i++)
		{
			/* Make some gold */
			if (!make_gold(i_ptr)) continue;

			/* Drop it */
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

		if (price_threshold > 250000L) price_threshold = 250000L;

		/* 100 attempts at finding a decent item */
		for (i = 0; i < 100; i++)
		{
			/* Temporary fame */
			x_fame = ((p_ptr->fame - i) < 1) ? 0 : (p_ptr->fame - i);

			/* Make a good (or great) object (if possible) */
			if (!make_object(i_ptr, TRUE, great, FALSE)) continue;

			/* It is identified */
			object_known(i_ptr);

			/* Sometimes give inappropriate rewards */
			if (i > p_ptr->fame) force_item = rand_int(p_ptr->fame);

			if (force_item < 4) break;

			/* Relatively expensive items are always appropriate */
			if ((i_ptr->number * object_value(i_ptr)) > price_threshold) break;

			/* Check spellbooks */
			if ((i_ptr->tval == TV_MAGIC_BOOK) ||
			    (i_ptr->tval == TV_PRAYER_BOOK) ||
			    (i_ptr->tval == TV_NATURE_BOOK) ||
			    (i_ptr->tval == TV_DARK_BOOK))
			{
				bool already_own_book = FALSE;

				/* Check if you can use them */
				if (i_ptr->tval != mp_ptr->spell_book) continue;

				/* Sometimes, check if we already have them */
				if (rand_int(p_ptr->fame) < 15) break;

				/* Look for item in the pack */
				for (j = 0; j < INVEN_PACK; j++)
				{
					/* Get the item */
					j_ptr = &inventory[j];

					if (!j_ptr->k_idx) continue;

					if ((j_ptr->tval == i_ptr->tval) && (j_ptr->sval == i_ptr->sval))
						already_own_book = TRUE;
				}

				/* Look for item in home */
				if (st_ptr->stock_num)
				{
					for (j = 0; j < st_ptr->stock_num; j++)
					{
						j_ptr = &st_ptr->stock[j];
						if ((j_ptr->tval == i_ptr->tval) &&
						    (j_ptr->sval == i_ptr->sval))
						{
							already_own_book = TRUE;
						}
					}
				}

				/* Already have that book */
				if (already_own_book) continue;

				break;
			}
			else if ((wield_slot(i_ptr) != -1) && (i_ptr->tval != TV_RING))
			{
				/* Wearable items - compare to item already in slot */
				j = wield_slot(i_ptr);

				j_ptr = &inventory[j];

				/* Compare value of the item with the old item */
				if (j_ptr->k_idx)
				{
					if ((object_value_real(i_ptr)) <= ((object_value_real(j_ptr) *
					    (90 + x_fame)) / 100))
					{
						continue;
					}
				}

				/* Sometimes, more sophisticated checks */
				if (rand_int(p_ptr->fame) < 7) break;

				/* Weapons - additional checks */
				if (j == INVEN_WIELD)
				{
					object_flags(i_ptr, &f1, &f2, &f3);

					if (p_ptr->realm == PRIEST)
					{
						/* Limit to legal weapon types */
						if ((i_ptr->tval != TV_HAFTED) && !(f3 & (TR3_BLESSED)))
							continue;
					}

					/* Too heavy */
					if (adj_str_hold[p_ptr->stat_ind[A_STR]] < (i_ptr->weight / 10))
						continue;
				}

				/* Gloves - additional checks */
				if (j == INVEN_HANDS)
				{
					object_flags(i_ptr, &f1, &f2, &f3);

					/* Player is only comfortable with certain gloves */
					if (p_ptr->oath & (OATH_OF_SORCERY) ||
					    p_ptr->oath & (BLACK_MYSTERY))
					{
						/* Limit to legal glove types */
						if (!(f3 & (TR3_FREE_ACT)) &&
						    (get_object_pval(i_ptr, TR_PVAL_DEVICE) <= 0) &&
						    (get_object_pval(i_ptr, TR_PVAL_DEX) <= 0))
						{
							continue;
						}
					}
				}

				break;
			}
			else if ((i_ptr->tval == TV_RING))
			{
				/* Rings - compare to cheaper of worn rings */
				j = (object_value_real(&inventory[INVEN_LEFT]) >
				     object_value_real(&inventory[INVEN_RIGHT]))
				     ? INVEN_LEFT : INVEN_RIGHT;

				j_ptr = &inventory[j];

				if (!j_ptr->k_idx) break;

				/* Compare value of the item with the old item */
				if ((object_value_real(i_ptr)) <= ((object_value_real(j_ptr) *
				    (90 + x_fame)) / 100))
				{
					continue;
				}

				break;
			}
			else if (is_missile(i_ptr))
			{
				/* If you can't use the ammo, inappropriate */
				if (p_ptr->ammo_tval != i_ptr->tval) continue;

				/* For high fame, ammo rewards are rare */
				if (randint(p_ptr->fame) > 30) continue;

				/* Assume appropriate */
				break;
			}

			/* Other item types are always appropriate */
			break;
		}

		/* Identify it fully */
		object_aware(i_ptr);
		object_mental(i_ptr);

		/* Drop the object */
		drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
	}

	/* Reset object level */
	object_level = p_ptr->depth;
}

/*
 * Actually give the character a quest
 */
static bool place_mon_quest(int q, int lev, int number, int difficulty)
{
	int i, chance;
	int mcount = 0;
	int lev_diff;
	monster_race *r_ptr;
	int *monster_idx;

	/* Allocate the "monster_idx" array */
	C_MAKE(monster_idx, z_info->r_max, int);

	/* Monsters are at least 1 level out of depth with difficulty 0 */
	lev_diff = 1 + difficulty + lev / 30;

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
			if (lev_diff <= difficulty) number += (damroll(2, 2) - 1);
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
			if (r_ptr->flags2 & (RF2_MULTIPLY)) continue;

			/* Never fixed-depth monsters */
			if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) continue;

			/* Monster can't move - check ranged attacks */
			if (r_ptr->flags1 & (RF1_NEVER_MOVE))
			{
				bool okay = FALSE;

				/* Allow if the monster can summon */
				if (r_ptr->flags7) okay = TRUE;

				/* Allow if the monster has attack spells */
				if ((r_ptr->flags4 & (RF4_ATTACK_MASK)) ||
				    (r_ptr->flags5 & (RF5_ATTACK_MASK)) ||
				    (r_ptr->flags6 & (RF6_ATTACK_MASK)))
				{
					okay = TRUE;
				}

				if (!okay) continue;
			}

			/* No uniques */
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

			/* Allow monster */
			monster_idx[mcount++] = i;
		}

		/* Climb up until you find a suitable monster type */
		if (mcount < 5)
		{
			/* Add some monsters to balance difficulty (sort-of) */
			if (mcount == 0) number += (damroll(2, 2) - 1);

			lev_diff--;
		}
	}

	/* Paranoia */
	if (mcount == 0)
	{
		/* No monsters - no quest */
		msg_print("There are no eligible monsters to quest for");

		/* XXX XXX Free the "monster_idx" array */
		FREE(monster_idx);

		return (FALSE);
	}

	/* choose random monster */
	i = rand_int(mcount);

	/* Get monster */
	r_ptr = &r_info[monster_idx[i]];

	/* You must quest for more monsters if they come in groups */
	if      (r_ptr->flags1 & (RF1_FRIENDS)) number = 3 * mcount / 2;
	else if (r_ptr->flags1 & (RF1_FRIEND))  number = 4 * mcount / 3;

	/* Paranoia */
	if (number <= 0) number = 1;
	if (number > 63) number = 63;


	/* Actually write the quest */
	q_info[q].type = QUEST_RANDOM;
	q_info[q].base_level = lev;
	q_info[q].active_level = lev;
	q_info[q].r_idx = monster_idx[i];
	q_info[q].max_num = number;
	q_info[q].cur_num = 0;
	q_info[q].started = FALSE;

	/* Fail the quest on the third quest_fail check */
	q_info[q].slack = 3;

	/* Set current quest */
	p_ptr->cur_quest = lev;


	/* Chance of gold reward */
	chance = 90 - ((difficulty - 2) * 20) - (lev * 5);

	/* First roll for gold award */
	if (rand_int(100) < chance)
	{
		q_info[q].reward = REWARD_GOLD;
	}

	/* Item reward */
	else
	{
		/* Chance of good item reward */
		chance = 105 - ((difficulty - 2) * 10) - (lev);

		/* Roll for good or great item */
		if (rand_int(100) < chance)
		{
			q_info[q].reward = REWARD_GOOD_ITEM;
		}
		else
		{
			q_info[q].reward = REWARD_GREAT_ITEM;
		}
	}

	/* XXX XXX Free the "monster_idx" array */
	FREE(monster_idx);

	/* Take note of quest */
	left_panel_display(DISPLAY_QUEST, 0);

	/* Success */
	return (TRUE);
}


/*
 * Various possible names for the Inn
 */
cptr inn_names[11] =
{
	NULL,
	"The Green Dragon",
	"The Prancing Stallion",
	"The Trollish Arms",
	"The Goblin's Head",
	"Firewater Spring",
	"The Inn at Bywater",
	"The Black Rose",
	"Gestolfo's Pub",
	"Journey's End",
	"The Red Bull",
};


/*
 * Display the Inn
 */
void display_inn(void)
{
	int i, j;
	byte attr;
	cptr p, q, q_out;


	/* Get or correct the inn name index */
	if ((p_ptr->inn_name <= 0) || (p_ptr->inn_name >= 11))
		p_ptr->inn_name = randint(10);

	/* Describe the inn */
	put_str(format("%s", inn_names[p_ptr->inn_name]), 2, 30);


	/* Check for outstanding rewards */
	for (i = 0; i < z_info->q_max ; i++)
	{
		if (q_info[i].type == QUEST_RANDOM)
		{
			/* Skip incomplete quests */
			if (q_info[i].active_level) continue;

			/* Check to see if there's a reward */
			if (q_info[i].reward)
			{
				/* Create the reward */
				grant_reward(q_info[i].base_level, q_info[i].reward);

				/* Grant fame bonus */
				p_ptr->fame += rand_range(4, 6);

				/* Reset the reward */
				q_info[i].reward = 0;

				/* Remember this quest */
				for (j = 0; j < MAX_QM_IDX; j++)
				{
					/* Found an empty record */
					if (p_ptr->quest_memory[j].type == 0)
					{
						p_ptr->quest_memory[j].type = q_info[i].type;
						p_ptr->quest_memory[j].level = p_ptr->cur_quest;
						p_ptr->quest_memory[j].r_idx = q_info[i].r_idx;
						p_ptr->quest_memory[j].max_num = q_info[i].max_num;
						p_ptr->quest_memory[j].succeeded = 1;
						break;
					}
				}

				/* Reset the quest */
				p_ptr->cur_quest = 0;

				/* Inform the player */
				msg_print("A reward for your efforts is waiting outside!");
			}
		}
	}

	/* Player's title */
	if      (p_ptr->fame >= 85) p = "O glorious ";
	else if (p_ptr->fame >= 65) p = "O great ";
	else if (p_ptr->fame >= 45) p = "noble ";
	else if (p_ptr->fame >= 25)
	{
		if (p_ptr->psex == SEX_MALE)   p = "sir ";
		else if (p_ptr->psex == SEX_FEMALE) p = "lady ";
		else p = "O ";
	}
	else
	{
		p = "";
	}

	if (p_ptr->fame >= 5)
	{
		if (!op_ptr->full_name[0])
		{
			q = "adventurer";
		}
		else q = op_ptr->full_name;
	}
	else
	{
		q = race_info[p_ptr->prace].title;
	}

	/* Introduction */
	put_str(format("Welcome, %s%s.", p, q), 4, 3);


	/* Get description of fame */
	if (p_ptr->fame < -10)
	{
		attr = TERM_RED;
		p = "name is held in contempt";
	}
	else if (p_ptr->fame < 0)
	{
		attr = TERM_YELLOW;
		p = "name is scorned";
	}
	else if (p_ptr->fame == 0)
	{
		attr = TERM_WHITE;
		p = "name is unknown";
	}
	else if (p_ptr->fame < 10)
	{
		attr = TERM_WHITE;
		p = "name has been mentioned once or twice";
	}
	else if (p_ptr->fame < 20)
	{
		attr = TERM_L_GREEN;
		p = "deeds have earned you respect";
	}
	else if (p_ptr->fame < 35)
	{
		attr = TERM_L_GREEN;
		p = "name is spoken with praise";
	}
	else if (p_ptr->fame < 45)
	{
		attr = TERM_GREEN;
		p = "name is well-known";
	}
	else if (p_ptr->fame < 60)
	{
		attr = TERM_GREEN;
		p = "name commands respect";
	}
	else if (p_ptr->fame < 75)
	{
		attr = TERM_L_BLUE;
		p = "victories are far-famed";
	}
	else if (p_ptr->fame < 90)
	{
		attr = TERM_L_BLUE;
		p = "accomplishments are of legendary stature";
	}
	else
	{
		attr = TERM_BLUE;
		p = "name and your deeds are the stuff of song and story";
	}

	/* Fame display */
	if (p_ptr->fame != 0)
	{
		c_put_str(attr, format("Word has it that your %s.", p), 5, 3);
	}

	/* Not Currently in a quest */
	if (!p_ptr->cur_quest)
	{
		/* Label the quest descriptions */
		put_str("Available Quests:", 7, 3);

		/* We always offer easy quests */
		avail_quest = 1;

		/* We offer harder quests too, for characters of some prowess */
		if (p_ptr->fame >=  5) avail_quest = 2;
		if (p_ptr->fame >= 10) avail_quest = 3;

		for (i = 0; i < avail_quest; i++)
		{
			if (inn_quests[i] <= 3)
			{
				attr = TERM_L_GREEN;
				p = "An easy";
			}
			else if (inn_quests[i] <= 5)
			{
				attr = TERM_YELLOW;
				p = "A moderate";
			}
			else
			{
				attr = TERM_ORANGE;
				p = "A difficult";
			}

			put_str(format("%c)", '1' + i), 9 + i, 3);
			c_put_str(attr, format ("%s quest.", p), 9 + i, 7);
		}


		prt(format("%c-%c) Get a quest.", '1', '1' + (avail_quest - 1)), 22, 31);
	}

	else
	{
		put_str("Your current quest:", 7, 3);
		q_out = describe_quest(p_ptr->cur_quest, QMODE_FULL);

		/* Break into two lines if necessary */
		if (strlen(q_out) < 70) put_str(q_out, 9, 3);
		else
		{
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_1);
			put_str(q_out, 9, 3);
			q_out = describe_quest(p_ptr->cur_quest, QMODE_HALF_2);
			put_str(q_out, 10, 3);
		}
	}

	prt("?) Get help.", 22, 60);

	/* We have a quest, and have the gold to get information */
	if ((p_ptr->cur_quest) &&
	    (p_ptr->au >= (p_ptr->power + p_ptr->max_depth) * 10))
	{
		prt(format("r) Learn about quest monster (price %d gold).",
			(1 + p_ptr->power + p_ptr->max_depth) * 25), 23, 3);
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
	sprintf(buf, "(Items %c-%c, ESC to exit)", '1', '1' + (avail_quest - 1));

	/* Ask until done */
	while (TRUE)
	{
		/* Escape */
		if (!get_com(buf, &which)) return (-1);

		/* Convert response to item */
		item = which - '1';

		/* Oops */
		if ((item < 0) || (item > avail_quest - 1))
		{
			/* Oops */
			bell("Illegal quest choice!");
			continue;
		}

		break;
	}

	/* Success */
	return (item);
}

/*
 * "Purchase" a quest from the Inn
 */
void inn_purchase(int item)
{
	int slot = 0;
	int i, qlev, num;
	bool found = FALSE;

	/* We always offer easy quests */
	avail_quest = 1;

	/* We offer harder quests too, for characters of some prowess */
	if (p_ptr->fame >=  5) avail_quest = 2;
	if (p_ptr->fame >= 10) avail_quest = 3;

	/* In a current quest */
	if (p_ptr->cur_quest)
	{
		msg_print("Finish your current quest first!");
		return;
	}

	/* Quit if we don't have that quest available */
	if (item > avail_quest)
	{
		msg_print("Unknown quest.");
		return;
	}

	/* Get the quest number  (Hack -- adjust non-zero inputs) */
	if (!item) item = get_quest();
	else item -= 1;

	/* Quit if no quest chosen */
	if (item == -1) return;

	/* Get level for quest  (beta code) */
	qlev = MAX(1, p_ptr->max_depth) + rand_range(2, 3);
	if (p_ptr->power > qlev) qlev += (p_ptr->power - qlev) / 3;


	/* We've run out of OOD monsters.  XXX */
	if (qlev >= 88)
	{
		msg_print("Alas, we have no quests that are worthy of you.");
		return;
	}

	/* Check list of quests.  Never use the first quest. */
	for (i = 1; i < z_info->q_max; i++)
	{
		/* Check fixed quests to see that they're not on the same level */
		if (q_info[i].type == QUEST_FIXED)
		{
			if (q_info[i].active_level == qlev)
			{
				msg_print("A greater task lies before you!");
				return;
			}

			/* If not problem, skip */
			continue;
		}

		slot = i;
		found = TRUE;

		break;
	}

	if (found)
	{
		if (item < 3)
		{
			/* How many monsters? */
			num = damroll(5, 3) + (p_ptr->fame / 30);
			if (!place_mon_quest(slot, qlev, num, inn_quests[item])) return;
		}
		else return;
	}

	else msg_print("You can't accept any more quests!");

	/* Clear screen */
	Term_clear();

	display_inn();

}

/*
 * Hack -- Check if a level is a "quest" level - returns quest type
 */
byte quest_check(int lev)
{
	int i;

	/* Town is never a quest */
	if (!lev) return (0);

	/* Check quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Check for quest */
		if (q_info[i].active_level == lev) return (q_info[i].type);
	}

	/* Nope */
	return (0);
}

/*
 * Return the index of the quest for current level
 */
int quest_num(int lev)
{
	int i;

	/* Town is never a quest */
	if (!lev) return (0);

	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Quest level? */
		if (q_info[i].active_level == lev) return (i);
	}

	/* No quest */
	return (0);
}

/*
 * Fail your quest
 */
static void quest_fail(void)
{
	int q_idx = quest_num(p_ptr->cur_quest);
	int j;
	quest_type *q_ptr = &q_info[q_idx];

	/* Remember this quest */
	for (j = 0; j < MAX_QM_IDX; j++)
	{
		/* Found an empty record */
		if (p_ptr->quest_memory[j].type == 0)
		{
			p_ptr->quest_memory[j].type = q_info[q_idx].type;
			p_ptr->quest_memory[j].level = p_ptr->cur_quest;
			p_ptr->quest_memory[j].r_idx = q_info[q_idx].r_idx;
			p_ptr->quest_memory[j].max_num = q_info[q_idx].max_num;
			p_ptr->quest_memory[j].succeeded = 0;
			break;
		}
	}

	/* Mark quest as completed */
	q_ptr->active_level = 0;
	p_ptr->cur_quest = 0;

	/* No reward for failed quest */
	q_ptr->reward = 0;

	/* Bell */
	bell(NULL);

	/* Message */
	message(MSG_ORANGE, 500, "You have failed in your quest!");

	/* Lose some fame */
	p_ptr->fame -= (5 + div_round(p_ptr->fame, 10));

	/* Take note of fame */
	left_panel_display(DISPLAY_FAME, 0);

	/* Take note of quest */
	left_panel_display(DISPLAY_QUEST, 0);

	/* Disturb */
	if (disturb_minor) disturb(0, 0);
}


/*
 * Quest failure checks.  Allow some "slack" before failing a quest.
 */
void check_quest_failure(int mode)
{
	/* We are on a quest */
	if (p_ptr->cur_quest)
	{
		quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

		/* We have started a random quest */
		if ((q_ptr->type == QUEST_RANDOM) &&
			 (q_ptr->started) && (q_ptr->active_level))
		{
			/* We have left the quest level */
			if (p_ptr->cur_quest != p_ptr->depth)
			{
				int chance = 0;

				/* Get chances against failing the quest */
				if      (mode == 1) chance = 300;
				else if (mode == 2) chance =   3;

				/* Get closer to failure every so often */
				if (one_in_(chance))
				{
					if (q_ptr->slack) q_ptr->slack--;

					/* Fail the quest, or warn */
					if (!q_ptr->slack)
					{
						quest_fail();
					}
					else
					{
						message(MSG_YELLOW, 500,
							"You are in danger of failing in your quest!");
					}
				}
			}
		}
	}
}

