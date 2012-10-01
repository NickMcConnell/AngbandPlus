/* File: quest.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Check on the status of an active quest -KMW-
 * TODO: Spill out status when not a simple kill # monster.
 */
void do_cmd_checkquest(void)
{
	int i2, i,j;
	char tmp_str[80];
	char tmp_str2[80];
	monster_race *r_ptr;
	cptr name;

	for (i2 = QUEST_REWARD_HEAD; i2 < QUEST_REWARD_TAIL; i2++)
	{
		i = i2 - QUEST_DIFF;
		if (!p_ptr->rewards[i])
		{
			sprintf(tmp_str,"No current quest");
			break;
		} 
		else if (p_ptr->rewards[i] == QUEST_ACTIVE)
		{
			j = i2 - QUEST_OFFSET1;
			if ((q_list[j].quest_type == 1) ||
			    (q_list[j].quest_type == 2))
			{
				r_ptr = &r_info[q_list[j].r_idx];
				name = r_name + r_ptr->name;
				sprintf(tmp_str,"Quest Status: Kill %d %s, have killed %d",
				    q_list[j].max_num, name, q_list[j].cur_num);
				break;
			} else {
				strcpy(tmp_str2,q_list[j].qname);
				if (q_list[j].quest_type == QUEST_OBJ_FIND_OBJECT)
					sprintf(tmp_str,"%s - find object",tmp_str2);
				else if (q_list[j].quest_type == QUEST_OBJ_FIND_EXIT)
					sprintf(tmp_str,"%s - find exit",tmp_str2);
				else if (q_list[j].quest_type == QUEST_OBJ_KILL_MONSTERS)
					sprintf(tmp_str,"%s: Kill %d creatures, have killed %d",
					    tmp_str2, q_list[j].num_mon, q_list[j].cur_num);
				else if (q_list[j].quest_type == QUEST_OBJ_KILL_ALL_MONSTERS)
					sprintf(tmp_str,"%s: Kill all monsters", tmp_str2);
				break;
			}
		} else if (p_ptr->rewards[i] == QUEST_COMPLETED) 
				{
			sprintf(tmp_str,"Current Quest Complete - Unrewarded");
			break;
		}
	}
	msg_print(tmp_str);
	msg_print(NULL);
}
