#define QUEST_C
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
 * Get the current quest, if any.
 */
quest_type *get_quest(void)
{
	quest_type *q_ptr;

	for (q_ptr = q_list; q_ptr < q_list+MAX_Q_IDX; q_ptr++)
	{
		if ((q_ptr->level == dun_level) && (q_ptr->dungeon == cur_dungeon))
		{
			return q_ptr;
		}
	}
	return NULL;
}

/*
 * Search quests for correct monster
 */
int get_quest_monster(void)
{
	quest_type *q_ptr = get_quest();
	if (q_ptr)
	{
		return q_ptr->r_idx;
	}
	else
	{
		return 0;
	}
}

/*
 * Print the number and monster to kill in this level
 */
void print_quest_message(void)
{
	quest_type *q_ptr = get_quest();
	monster_race *r_ptr = &r_info[q_ptr->r_idx];
	int q_num = q_ptr->max_num - q_ptr->cur_num_known;
	byte flag = (q_ptr->max_num == 1) ? MDF_DEF : MDF_NUMBER;

	msg_format("You still have to kill %v.", monster_desc_aux_f3, r_ptr,
		q_num, flag);
}

/* Array of places to find an inscription. Should the game check that the
 * player can see the mentioned feature?
 */
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
 *
 * Set new if the player is encountering this level for the first time
 */
void quest_discovery(bool new)
{
	quest_type *q_ptr = get_quest();
	monster_race *r_ptr = &r_info[q_ptr->r_idx];
	int q_num = q_ptr->max_num;

	/* Get a properly formatted name. Note that no monster will actually
	be given an article as only uniques are currently allowed to be
	solitary quest monsters. */
	byte flags = 0;
	if (q_num > 1)
		flags = MDF_NUMBER;
	else if (~r_ptr->flags1 & RF1_UNIQUE)
		flags = MDF_DEF;
	else
		flags = MDF_INDEF;

	/* There isn't an inscription everywhere the player saves the game... */
	if (new)
	{
		msg_print (find_quest[rand_range(0,4)]);
		msg_print (NULL);
	}
	msg_format("Beware, this level is protected by %v!", monster_desc_aux_f3,
		r_ptr, q_num, flags);

	/* Once discovered, always known. */
	q_ptr->known = TRUE;
}

/*
 * Mark quest monsters out as being special.
 */
void set_guardians(void)
{
	monster_race *r_ptr;
	quest_type *q_ptr;

	/* Remove any previous quest monster status. */
	for (r_ptr = r_info; r_ptr < r_info+MAX_R_IDX; r_ptr++)
	{
		r_ptr->flags1 &= ~(RF1_GUARDIAN);
	}

	/* Set quest monster status for the current quest and any unique quests. */
	for (q_ptr = q_list; q_ptr < q_list+MAX_Q_IDX; q_ptr++)
	{
		r_ptr = r_info+q_ptr->r_idx;

		if ((get_quest_monster() == q_ptr->r_idx) ||
			(r_ptr->flags1 & RF1_UNIQUE))
		{
			r_ptr->flags1 |= RF1_GUARDIAN;
		}
	}
}

quest_type *cnv_monster_to_quest(monster_race *r_ptr)
{
	quest_type *q_ptr;

	/* Set quest monster status for the current quest and any unique quests. */
	for (q_ptr = q_list; q_ptr < q_list+MAX_Q_IDX; q_ptr++)
	{
		if (r_ptr-r_info == q_ptr->r_idx) return q_ptr;
	}
	return NULL;
}
