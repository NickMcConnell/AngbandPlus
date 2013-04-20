/* File: quest.c */

/* Purpose: functions used by the random quest source
 * maybe some of these function are better placed somewhere else
 */

/*
 * Copyright (c) 1989 James E. Wilson, Christopher J. Stuart
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 *
 * Heino Vander Sanden
 * Heino.VanderSanden@AdValvas.Be
 *
 */

 #include "angband.h"

/*
 * Search quests for correct monster
 */
int get_quest_monster(void)
{
	int i;

	for (i = 0; i < MAX_Q_IDX; i++)
	{
		if (q_list[i].level == dun_level) return (q_list[i].r_idx);
	}
	return 0;
}

/*
 * Search quests for number of monsters
 */
int get_max_monster(void)
{
	int i;

	for (i = 0; i < MAX_Q_IDX; i++)
	{
		if (q_list[i].level == dun_level) return (q_list[i].max_num);
	}
	return 0;
}

/*
 * Get quest number
 */
int get_quest_number(void)
{
	int i;

	for (i=0; i < MAX_Q_IDX; i++)
	{
                if (q_list[i].level == dun_level) return (i);
	}
	return 0;
}

/*
 * Print the number and monster to kill in this level
 */
void print_quest_message(void)
{
	int             q_idx = get_quest_number();
	monster_race	*r_ptr = &r_info[q_list[q_idx].r_idx];
	int             q_num = q_list[q_idx].max_num - q_list[q_idx].cur_num;
	char		name[80];

	strcpy(name, (r_name + r_ptr->name));

	if (q_list[q_idx].max_num == 1)
		msg_format("You still have to kill %s.", name);
	else
	{
		if (q_num > 1)
		{
			plural_aux(name);
			msg_format("You still have to kill %d %s.", q_num, name);
		}
		else
		{
			msg_format("You still have to kill 1 %s.", name);
		}
	}
}

/* Array of places to find an inscription */
static cptr find_quest[5] =
{
	"You find the following inscription in the floor",
	"You see a message inscribed in the wall",
	"There is a sign saying",
	"Something is writen on the staircase",
        "You find a scroll with the following message",
};


/*
 * Discover quest
 */
void quest_discovery(void)
{
	int 	        q_idx = get_quest_number();
	monster_race	*r_ptr = &r_info[q_list[q_idx].r_idx];
	int             q_num = q_list[q_idx].max_num;
	char		name[80];

	strcpy(name, (r_name + r_ptr->name));

        msg_print (find_quest[rand_range(0,4)]);
	msg_print (NULL);

	if (q_num == 1)
	{
		msg_format("Beware, this level is protected by %s!", name);
	}
	else
	{
		plural_aux(name);
		msg_format("Be warned, this level is guarded by %d %s!", q_num, name);
	}
}


/*
 * Search the next quest level
 */
int next_quest_level(void)
{
	int i;

	for(i = p_ptr->max_dlv; i < 127; i++)
	{
		if (is_quest(i, FALSE))
			return i;
	}
	return 127;
}

/*
 * Initialise the matrix of quests
 * and add the 3 obligatory ones (Arioch, Xiombarg and Mabelrode)
 * Heino Vander Sanden, Jimmy De Laet
 * Edited for ZAngband by Robert Ruehlmann
 * Edited for Gumband by Gumby
 */
void initialise_quests()
{
	int i;

	/* Start with no quests */
	for (i = 0; i < MAX_QUESTS + DEFAULT_QUESTS; i++)
	{
		q_list[i].level = 0;
		q_list[i].r_idx = 0;
		q_list[i].cur_num = 0;
		q_list[i].max_num = 0;
	}

	/* Add a special quest: Arioch */
	q_list[0].level = 98;
	q_list[0].r_idx = 636;
	q_list[0].max_num = 1;

	/* Add a special quest: Xiombarg */
	q_list[1].level = 99;
	q_list[1].r_idx = 637;
	q_list[1].max_num = 1;

	/* Add a second quest: Mabelrode */
	q_list[2].level = 100;
	q_list[2].r_idx = 638;
	q_list[2].max_num = 1;
}


/*
 * Get number of quest monsters for quest i
 * Heino Vander Sanden
 */
int get_number_monster(int i)
{
	int num;

	if (r_info[q_list[i].r_idx].flags1 & (RF1_UNIQUE))
	{
		return (1);
	}
	else
	{
		num = 5 + rand_range(1, (q_list[i].level / 3) + 5);
		return (num);
	}
}

/*
 * Get random monster
 * Heino Vander Sanden
 * Edited for ZAngband by Robert Ruehlmann
 * Then edited for Gumband by Gumby :)
 */
int get_rnd_q_monster(int q_idx)
{
	int r_idx,j,tmp;

	tmp = rand_range(1,10);
	/* first level 10 monster (146), last monster (635) */
	switch (tmp)
	{
		case 1 : r_idx = rand_range(146,251); break; /* 10-19 */
		case 2 : r_idx = rand_range(252,346); break; /* 20-29 */
		case 3 : r_idx = rand_range(347,458); break; /* 30-39 */
		case 4 : r_idx = rand_range(459,518); break; /* 40-49 */
		case 5 : r_idx = rand_range(519,559); break; /* 50-59 */
		case 6 : r_idx = rand_range(560,584); break; /* 60-69 */
		case 7 : r_idx = rand_range(585,609); break; /* 70-79 */
		case 8 : r_idx = rand_range(610,622); break; /* 80-89 */
		case 9 : r_idx = rand_range(623,635); break; /* 90-97 */
		default : r_idx = rand_range (146,635);
	}

		/*
		 * Prevent monsters with friends or escorts, and breeders,
		 * from being selected for quests. Such monsters are almost
		 * always too weak individually. -- Gumby
		 */
		if (r_info[r_idx].flags1 & (RF1_FRIENDS))  return (0);
		if (r_info[r_idx].flags1 & (RF1_ESCORT))   return (0);
		if (r_info[r_idx].flags1 & (RF1_ESCORTS))  return (0);
		if (r_info[r_idx].flags2 & (RF2_MULTIPLY)) return (0);

	for (j = 2; j < q_idx; j++)
		if ((q_list[j].r_idx == r_idx) && r_info[r_idx].flags1 & (RF1_UNIQUE))
			return (0);

	return (r_idx);
}


/*
 * Generate the additional quests
 * Heino Vander Sanden, Jimmy De Laet, and Robert Ruehlmann
 */
void player_birth_quests(void)
{
	int i,j;
	bool same_level;

	/* Generate MAX_Q_IDX - DEFAULT_QUESTS random additional quests */
	for (i = DEFAULT_QUESTS; i < MAX_Q_IDX; i++)
	{
		do
		{
			same_level = FALSE;

			/* Get a random monster */
			do
			{
				q_list[i].r_idx = get_rnd_q_monster(i);
			}
			while (!q_list[i].r_idx);

			/* Set the quest level to the level of the monster */
			q_list[i].level = r_info[q_list[i].r_idx].level;

			/* Quest monster at least 2 levels out of depth */
			q_list[i].level -= rand_range(2, 3 + (q_list[i].level / 6));

			/* No 2 quests on the same level */
			for (j = 2; j < i; j++)
			{
				if (q_list[i].level == q_list[j].level)
				{
					same_level = TRUE;
					break;
				}
			}
		}
		while (same_level);

		q_list[i].max_num = get_number_monster(i);
	}
}


/*
 * Put Quest monster in dungeon
 * Heino Vander Sanden
 */
void put_quest_monster(int r_idx)
{
	int	y, x;

	do
	{
		/* Find a legal unoccupied space */
		while (1)
		{
			/* Pick a location */
			y = rand_int(MAX_HGT);
			x = rand_int(MAX_WID);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;
			{
				/* At least 15 grids away */
				if (distance(y, x, py, px) > 15) break;
			}
		}
	}
	while (!place_monster_one(y, x, r_idx, 0,0));
}


/*
 * Hack -- Check if a level is a "quest" level
 */
bool is_quest(int level, bool kill)
{
	int i;

	/* Town is never a quest */
	if (!level) return (FALSE);

	/* Check quests */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		/* Check for quest */
		/* rr9: Also check if a unique quest monster is already killed */
		if (q_list[i].level == level)
		{
			monster_race *r_ptr = &r_info[get_quest_monster()];

			/* "Unique" monsters must be "unique" */
			if (!kill && (r_ptr->flags1 & (RF1_UNIQUE)) &&
			    (r_ptr->max_num == 0))
			{
				return (FALSE);
			}

			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}

