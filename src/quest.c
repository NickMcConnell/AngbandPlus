/* File: quest.c */

/*
 * Copyright (c) 1999 Eric Bock
 * Inspired by the quest code of Heino Vander Sanden and Jimmy de Laet
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Print the number and monster to kill in this level
 */
void print_quest_message(void)
{
	int q_idx = level_quest[p_ptr->depth];

	quest_type *q_ptr = &q_list[q_idx];

	int remain = q_ptr->max_num - q_ptr->cur_num;

	monster_race *r_ptr = &r_info[q_ptr->r_idx];

	char name[80];

	/* Extract the name */
	strcpy(name, (r_name + r_ptr->name));

	/* Note uniques */
	if (q_ptr->max_num == 1)
	{
		msg_format("You still have to kill %s.", name);
	}
	else
	{
		/* More than one monster remaining */
		if (remain > 1)
		{
			plural_aux(name);
			msg_format("You still have to kill %d %s.", remain, name);
		}
		/* One left */
		else
		{
			msg_format("You still have to kill 1 %s.", name);
		}
	}
}

#define MAX_QUEST_DESC	6

/* Array of places to find an inscription */
static cptr quest_desc[MAX_QUEST_DESC] =
{
	"You find the following inscription in the floor:",
	"You see a message inscribed in the wall:",
	"There is a sign saying",
	"Something is written on the staircase:",
	"You find a scroll with the following message:",
	"Fiery runes blaze before you:",
};


/*
 * Discover quest
 */
void quest_discovery(void)
{
	int q_idx = level_quest[p_ptr->depth];

	quest_type *q_ptr = &q_list[q_idx];

	monster_race *r_ptr = &r_info[q_ptr->r_idx];

	char name[80];

	/* Extract the name */
	strcpy(name, (r_name + r_ptr->name));

	/* Note the message */
	msg_print(quest_desc[rand_int(MAX_QUEST_DESC)]);
	msg_print(NULL);

	/* Note uniques */
	if (q_ptr->max_num == 1)
	{
		msg_format("Beware, this level is protected by %s!", name);
	}
	/* Multiple monsters */
	else
	{
		plural_aux(name);
		msg_format("Be warned, this level is guarded by %d %s!", q_ptr->max_num, name);
	}
}


/*
 * Initialise the matrix of quests
 * and add the 2 obligatory ones ( Oberon and the Serpent of Chaos )
 */
void initialise_quests()
{
	int i;

	/* Reset all quests */
	C_WIPE(q_list, MAX_Q_IDX, quest_type);

	for (i = 0; i < MAX_DEPTH; i++)
	{
		level_quest[i] = -1;
	}

	/* Add a special quest: Oberon */
	q_list[0].level = 99;
	level_quest[99] = 0;

	/* Find Oberon */
	for (i = 0; i < MAX_R_IDX; i++)
	{
		if (strstr(r_name + r_info[i].name, "Oberon") &&
					  (r_info[i].flags1 & (RF1_QUESTOR)))
		{
			break;
		}
	}

	q_list[0].r_idx = i;
	q_list[0].max_num = 1;

	/* Add a second quest: Serpent of Chaos */
	q_list[1].level = 100;
	level_quest[100] = 1;

	/* Find the Serpent of Chaos */
	for (i = 0; i < MAX_R_IDX; i++)
	{
		if (strstr(r_name + r_info[i].name, "Serpent") &&
					  (r_info[i].flags1 & (RF1_QUESTOR)))
		{
			break;
		}
	}

	q_list[1].r_idx = i;
	q_list[1].max_num = 1;
}


/*
 * Helper function for random quest generation
 */
static bool quest_aux(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Require non-quest monster */
	if (r_ptr->flags1 & RF1_QUESTOR) return (FALSE);

	/* Questors must be evil */
	if (!(r_ptr->flags3 & RF3_EVIL)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for random quest generation
 */
static bool nasty_quest_aux(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	bool summoner = (r_ptr->flags4 & RF4_SUMMON_MASK) ||
						 (r_ptr->flags5 & RF5_SUMMON_MASK) ||
						 (r_ptr->flags6 & RF6_SUMMON_MASK);

	/* Require non-quest monster */
	if (r_ptr->flags1 & RF1_QUESTOR) return (FALSE);

	/* Reject weak or non-summoners (usually) */
	if ((!summoner || (r_ptr->freq_spell < 25)) &&
		 (rand_int(100) > NASTY_QUEST))
	{
		return (FALSE);
	}

	/* Questors must be evil */
	if (!(r_ptr->flags3 & RF3_EVIL)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Generate the additional quests
 */
void player_birth_quests(void)
{
	int i;
	int level;

	bool used_level[MAX_DEPTH];
	bool used_monst[MAX_R_IDX];

	bool nasty;

	int depth = p_ptr->depth;

	quest_type *q_ptr;

	C_WIPE(used_level, MAX_DEPTH, bool);
	C_WIPE(used_monst, MAX_R_IDX, bool);

	/* Note the default quests */
	for (i = 0; i < DEFAULT_QUESTS; i++)
	{
		q_ptr = &q_list[i];

		/* This level is used */
		used_level[q_ptr->level] = TRUE;

		/* Unique monster are used */
		if (r_info[q_ptr->r_idx].flags1 & (RF1_UNIQUE)) used_monst[q_ptr->r_idx] = TRUE;
	}

	/* Generate some random additional quests */
	for (i = DEFAULT_QUESTS; (i < total_quests) && (i < MAX_DEPTH - 1); i++)
	{
		q_ptr = &q_list[i];

		while (1)
		{
			/* Choose a level */
			level = randint(MAX_DEPTH - 1);

			/* Skip previous levels */
			if (used_level[level]) continue;

			/* Hack -- fake dungeon level */
			p_ptr->depth = level;

			/* Occasionally try to make a difficult quest */
			if (!rand_int(NASTY_QUEST))
			{
				get_mon_num_hook = nasty_quest_aux;
				nasty = TRUE;
			}
			/* Normal quest */
			else
			{
				get_mon_num_hook = quest_aux;
				nasty = FALSE;
			}

			/* Prepare the allocation table */
			get_mon_num_prep();

			/* Choose a hard monster  */
			q_ptr->r_idx = get_mon_num(level + rand_range(15, 30));

			/* Skip unavailable uniques */
			if (used_monst[q_ptr->r_idx]) continue;

			/* Okay */
			break;
		}

		/* Set the quest level */
		q_ptr->level = level;

		/* Note it */
		level_quest[level] = i;

		/* This level is used */
		used_level[level] = TRUE;

		if (r_info[q_ptr->r_idx].flags1 & (RF1_UNIQUE))
		{
			/* Uniques are unavailable */
			used_monst[q_ptr->r_idx] = TRUE;

			/* Uniques can only be killed once */
			q_ptr->max_num = 1;
		}
		else
		{
			/* Some monsters come in groups */
			if (r_info[q_ptr->r_idx].flags1 & (RF1_FRIENDS))
			{
				q_ptr->max_num = 10;
			}
			else
			{
				q_ptr->max_num = 5;
			}

			/* Randomize the number a bit */
			q_ptr->max_num += rand_int(level / 3 + 5);

			/* Nasty quest levels may have more monsters */
			if (nasty) q_ptr->max_num += rand_int(level / 10 + 10);
		}
	}

	/* Remove the monster allocation hook */
	get_mon_num_hook = NULL;

	/* Reset the allocation table */
	get_mon_num_prep();

	/* Restore dungeon level */
	p_ptr->depth = depth;
}


/*
 * Put Quest monster in dungeon
 */
void put_quest_monster(int r_idx)
{
	int y, x;

	/* Try to place a monster */
	while (1)
	{
		/* Find a legal unoccupied space */
		while (1)
		{
			/* Pick a location */
			y = rand_int(cur_hgt);
			x = rand_int(cur_wid);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;

			/* At least 15 grids away */
			if (distance(y, x, p_ptr->py, p_ptr->px) > 15) break;
		}

		/* Oops */
		if (!place_monster_aux(y, x, r_idx, FALSE, FALSE, FALSE)) break;
	}
}


/*
 * Is this level a quest level?
 */
bool is_quest(int level)
{
	/* Extract this quest */
	int q_idx = level_quest[level];

	quest_type *q_ptr = &q_list[q_idx];

	monster_race *r_ptr;

	/* No quest here */
	if (q_idx < 0) return (FALSE);

	r_ptr = &r_info[q_ptr->r_idx];

	/* Dead uniques are not quests */
	if (!r_ptr->max_num) return (FALSE);

	/* A quest */
	return (TRUE);
}
