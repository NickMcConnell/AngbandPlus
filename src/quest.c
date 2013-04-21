/* File: quest.c */

/* Purpose: functions used by the random quest source
* maybe some of these function are better placed somewhere else
*/

/*
 * Copyright (c) 1989 James E. Wilson, Christopher J. Stuart
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * James E. Wilson and Christopher J. Stuart released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
 */

/*
*
* Heino Vander Sanden
* Heino.VanderSanden@AdValvas.Be
* 
* Heino Vander Sanden released all changes to the Angband code under the terms of the GNU General Public License (version 2),
* as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
* or under the terms of the traditional Angband license.  
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
		if (q_list[i].level == (unsigned int)abs(dun_level)) return (q_list[i].r_idx);
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
		if (q_list[i].level ==  (unsigned int)abs(dun_level)) return (q_list[i].max_num);
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
		if (q_list[i].level ==  (unsigned int)abs(dun_level))
		{
			return (i);
		}
	}
	return -1;
}

/*
* Print the number and monster to kill in this level
*/
void print_quest_message(void)
{
	int q_idx = get_quest_number();
	monster_race	*r_ptr = &r_info[q_list[q_idx].r_idx];
	cptr name = (r_name + r_ptr->name);
	int q_num = q_list[q_idx].max_num - q_list[q_idx].cur_num;

	if (q_list[q_idx].max_num == 1)
		msg_format("You still have to kill %s.", name);
	else
		if (q_num > 1)
			msg_format("You still have to kill %d %ss.", q_num, name);
		else
			msg_format("You still have to kill 1 %s.", name);
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
	int 	q_idx = get_quest_number();
	monster_race	*r_ptr = &r_info[q_list[q_idx].r_idx];
	cptr  name = (r_name + r_ptr->name);
	int q_num = q_list[q_idx].max_num;

	msg_print (find_quest[rand_range(0,4)]);
	msg_print (NULL);
	if (q_num == 1)
		msg_format("Beware, this level is protected by %s!", name);
	else
		msg_format("Be warned, this level is guarded by %d %ss!", q_num, name);
}

/*
* Search the next quest level
*/

int next_quest_level(void)
{
	int i;

	for(i = p_ptr->max_dun_level; i < 127; i++)
	{
		if (is_quest(i))
			return i;
	}
	return 127;
}
