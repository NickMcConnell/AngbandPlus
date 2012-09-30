/* File: quest.c */

/* Purpose: Quest code */

/*
 * Copyright (c) 1989, 2003 James E. Wilson, Robert A. Koeneke,
 *                          Robert Ruehlmann, Steven Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#include "wild.h"

/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100


/*
 * Wipe a quest
 */
static void quest_wipe(int i)
{
	quest_type *q_ptr = &quest[i];

	q_ptr->status = QUEST_STATUS_UNTAKEN;
	q_ptr->flags = 0x00;
	q_ptr->type = QUEST_TYPE_NONE;

	/* No artificial quest item */
	q_ptr->item = 0;

	/* No quest-giver */
	q_ptr->place = 0;
	q_ptr->shop = 0;

	/* No reward */
	q_ptr->reward = 0;

	/* Types of creation and trigger hooks */
	q_ptr->c_type = QC_NONE;
	q_ptr->x_type = QX_NONE;

	/* Timeout */
	q_ptr->timeout = 0;

	/* No name */
	q_ptr->name[0] = '\0';

	/*
	 * Do not need to clear the extra data
	 * - it is ignored since q_ptr->type = QUEST_TYPE_NONE
	 */
}

/* Current location in scan for completed quests */
static s16b q_cnt = 0;

/*
 * Acquires and returns the index of a "free" quest.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
u16b q_pop(void)
{
	int i;
	
	/* Initial allocation */
	if (q_max < z_info->q_max)
	{
		/* Get next space */
		i = q_max;

		/* Count quests */
		q_max++;
		
		/* Use this quest */
		return (i);
	}
	
	/* Recycle finished quests */
	for (i = 1; i < q_max; i++)
	{
		quest_type *q_ptr;
		
		/* Make sure we have a linear algorithm */
		q_cnt++;
		
		/* loop back to start */
		if (q_cnt >= q_max) q_cnt = 0;

		/* Acquire quest */
		q_ptr = &quest[q_cnt];

		/* Skip live quests */
		if (q_ptr->status != QUEST_STATUS_FINISHED) continue;
		
		/* Clear the old data */
		quest_wipe(q_cnt);

		/* Use this field */
		return (q_cnt);
	}

	/* Warn the player */
	msgf("Too many quests!");

	/* Oops */
	return (0);
}


/*
 * Make a quest for killing n monsters of a certain type on a certain level
 */
u16b insert_dungeon_monster_quest(u16b r_idx, u16b num, u16b level)
{
	int q_num;
	quest_type *q_ptr;
	monster_race *r_ptr = &r_info[r_idx];

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (0);

	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_DUNGEON;

	/* We need to place the monster(s) when the dungeon is made */
	q_ptr->c_type = QC_DUN_MONST;

	/* We need to trigger when the monsters are killed */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		q_ptr->x_type = QX_KILL_UNIQUE;
	}
	else
	{
		q_ptr->x_type = QX_KILL_MONST;
	}

	if (num != 1)
	{
		char buf[80];
		strcpy(buf, r_name + r_ptr->name);
		plural_aux(buf);

		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %d %s.", (int)num, buf);
	}
	else
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %s.", r_name + r_ptr->name);
	}

	/* Save the quest data */
	q_ptr->data.dun.r_idx = r_idx;
	q_ptr->data.dun.level = level;
	q_ptr->data.dun.cur_num = 0;
	q_ptr->data.dun.max_num = num;
	q_ptr->data.dun.num_mon = 0;

	/* Return number of quest */
	return (q_num);
}


/*
 * Create the quests for the Serpent and Oberon
 */
static void insert_winner_quest(u16b r_idx, u16b num, u16b level)
{
	/* Normal monster quest */
	u16b q_idx = insert_dungeon_monster_quest(r_idx, num, level);
	
	/* Winner result of quest */
	quest[q_idx].x_type = QX_KILL_WINNER;
	quest[q_idx].flags |= QUEST_FLAG_KNOWN;
	quest[q_idx].status = QUEST_STATUS_TAKEN;
}


/*
 * Look for an appropriate dungeon for a given level
 */
static u16b find_good_dungeon(int level, int dist)
{
	int i;
	
	int score, best_score = 0;
	
	int best_place = 0;
	
	place_type *pl_ptr;
	
	for (i = 0; i < place_count; i++)
	{
		pl_ptr = &place[i];
		
		/* Want dungeons */
		if (pl_ptr->type != TOWN_DUNGEON) continue;
		
		/* Constrain distance */
		if (distance(pl_ptr->x, pl_ptr->y, p_ptr->px / 16, p_ptr->py / 16) < dist)
		{
			continue;
		}
	
		/* Get difference in levels */
		score = abs(pl_ptr->dungeon->max_level - level);
		
		/* The bigger the difference, the less likely a high score is */
		score = randint1(127 - score); 
	
		if (score > best_score)
		{
			best_score = score;
			best_place = i;
		}
	}
	
	/* Best match to reward level */
	return (best_place);
}

/*
 * Look for an appropriate town with a distance appropriate
 * for the given level.
 */
static u16b find_good_town(int *dist)
{
	int i;
	
	int score, best_score = 0;
	
	int best_place = 0;
	
	place_type *pl_ptr;
	
	for (i = 0; i < place_count; i++)
	{
		/* Not current town */
		if (i == p_ptr->place_num) continue;
	
		pl_ptr = &place[i];
		
		/* Want towns with buildings */
		if (!pl_ptr->numstores) continue;
		
		/* Get difference of distance in wilderness blocks and difficulty level */
		score = abs(distance(pl_ptr->x, pl_ptr->y, p_ptr->px / 16, p_ptr->py / 16) - *dist);
		
		/* The bigger the difference, the less likely a high score is */
		score = randint1(WILD_SIZE - score); 
	
		if (score > best_score)
		{
			best_score = score;
			best_place = i;
		}
	}
	
	/* Save distance to best town */
	pl_ptr = &place[best_place];
	*dist = distance(pl_ptr->x, pl_ptr->y, p_ptr->px / 16, p_ptr->py / 16);
	
	/* Best match to reward level */
	return (best_place);
}


static cptr describe_quest_location(cptr * dirn, int x, int y)
{
	int i;
	
	int dx, dy;
	
	/* Find the nearest town */
	int best_dist = 99999;
	int best_town = 0;
	
	for (i = 0; i < place_count; i++)
	{
		int d;

		/* Only real towns */
		if (place[i].type != TOWN_FRACT)
			continue;

		/* Find closest town */
		d = distance(x, y, place[i].x, place[i].y);
		if (d < best_dist)
		{
			best_dist = d;
			best_town = i;
		}
	}

	
	dx = x - place[best_town].x;
	dy = y - place[best_town].y;

	if (ABS(dy) > ABS(dx) * 3)
	{
		if (dy > 0)
			*dirn = "south";
		else
			*dirn = "north";
	}
	else if (ABS(dx) > ABS(dy) * 3)
	{
		if (dx > 0)
			*dirn = "east";
		else
			*dirn = "west";
	}
	else if (dx > 0)
	{
		if (dy > 0)
			*dirn = "south-east";
		else
			*dirn = "north-east";
	}
	else
	{
		if (dy > 0)
			*dirn = "south-west";
		else
			*dirn = "north-west";
	}

	return (place[best_town].name);
}


/*
 * Initialise the quests
 */
errr init_quests(void)
{
	int i;

	/* Make the quest array */
	C_MAKE(quest, z_info->q_max, quest_type);

	/* Reset number of quests */
	q_max = 1;

	/* Wipe the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_wipe(i);
	}

	return (0);
}



/*
 * Quests
 *
 */
void init_player_quests(void)
{
	int i;

	/* Reset number of quests */
	q_max = 1;

	/* Clear all the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_wipe(i);
	}

	/* Add the winner quests */

	/* Hack XXX XXX Oberon, hard coded */
	insert_winner_quest(QW_OBERON, 1, 99);

	/* Hack XXX XXX Serpent, hard coded */
	insert_winner_quest(QW_SERPENT, 1, 100);
}


/* Array of places to find an inscription */
static cptr find_quest[] =
{
	"You find the following inscription in the floor",
	"You see a message inscribed in the wall",
	"There is a sign saying",
	"Something is written on the staircase",
	"You find a scroll with the following message",
	"You hear",
};


/*
 * Discover quests on this level
 */
void quest_discovery(void)
{
	int i;

	quest_type *q_ptr;

	char name[80];

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Quest needs to be taken. */
		if (q_ptr->status != QUEST_STATUS_TAKEN) continue;

		/* Is the quest active? */
		if (!(q_ptr->flags & QUEST_FLAG_ACTIVE)) continue;

		/* Is the quest known already? */
		if (q_ptr->flags & QUEST_FLAG_KNOWN) continue;

		/* Hack - The quest is now known */
		q_ptr->flags |= QUEST_FLAG_KNOWN;

		/* See what type of quest it is */
		switch (q_ptr->type)
		{
			case QUEST_TYPE_NONE:
			{
				/* Paranoia */
				continue;
			}

			case QUEST_TYPE_BOUNTY:
			{

				/* Paranoia */
				continue;
			}

			case QUEST_TYPE_DUNGEON:
			{
				monster_race *r_ptr = &r_info[q_ptr->data.dun.r_idx];
				int q_num = q_ptr->data.dun.max_num - q_ptr->data.dun.cur_num;

				/* Assume the quest is a 'kill n monsters quest' for now. */
				strcpy(name, (r_name + r_ptr->name));

				if (FLAG(r_ptr, RF_UNIQUE))
				{
					/* Unique */
					msgf("%s: Beware, this level is protected by %s!",
							   find_quest[rand_range(0, 5)], name);
				}
				else
				{
					/* Normal monsters */
					if (q_num > 1) plural_aux(name);

					msgf("%s: Be warned, this level is guarded by %d %s!",
							   find_quest[rand_range(0, 5)], q_num, name);
				}

				/* Disturb */
				disturb(FALSE);

				continue;
			}

			case QUEST_TYPE_WILD:
			{
				msgf("You discover something unusual in the wilderness.");

				/* Disturb */
				disturb(FALSE);

				/* Paranoia */
				continue;
			}
			
			case QUEST_TYPE_FIND_ITEM:
			{
				msgf("You feel there is something interesting about this level.");

				/* Disturb */
				disturb(FALSE);

				/* Paranoia */
				continue;
			
			}

			default:
			{
				/* Paranoia */
				continue;
			}
		}
	}
}


/*
 * Is this dungeon level a special (winner) quest level? */
bool is_special_level(int level)
{
	int i;

	quest_type *q_ptr;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Must be dungeon quest */
		if (q_ptr->type != QUEST_TYPE_DUNGEON) continue;
		
		/* Must be winner quest */
		if (q_ptr->x_type != QX_KILL_WINNER) continue;
		
		/* Is the quest still there? */
		if (q_ptr->status > QUEST_STATUS_TAKEN) continue;

		/* Does the level match? */
		if (q_ptr->data.dun.level == level) return (TRUE);
	}

	return (FALSE);
}

/*
 * Activate quests valid for this level
 */
void activate_quests(int level)
{
	int i;

	quest_type *q_ptr;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];
		
		/* Assume no longer active */
		q_ptr->flags &= ~(QUEST_FLAG_ACTIVE);

		/* Is the quest relevant? */
		switch (q_ptr->type)
		{
			case QUEST_TYPE_DUNGEON:
			{
				/* Correct dungeon level? */
				if (q_ptr->data.dun.level != level) break;
				
				/* Follow through */
			}
			

			case QUEST_TYPE_BOUNTY:
			{
				q_ptr->flags |= QUEST_FLAG_ACTIVE;

				/* Hack - toggle QUESTOR flag */
				SET_FLAG(&r_info[q_ptr->data.dun.r_idx], RF_QUESTOR);
				
				break;
			}
			
			case QUEST_TYPE_WILD:
			{
				/* In Wilderness? */
				if (!level) q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			}

			
			case QUEST_TYPE_FIND_ITEM:
			{
				int place_num = q_ptr->data.fit.place;
				
				place_type *pl_ptr;
				
				/* Not correct place? */
				if (place_num != p_ptr->place_num) break;
				
				pl_ptr = &place[place_num];
				
				/* Need to be in the dungeon */
				if (!pl_ptr->dungeon) break;
			
				/* Correct dungeon level? */
				if (level != pl_ptr->dungeon->max_level) break;
				
				q_ptr->flags |= QUEST_FLAG_ACTIVE;
			}
			
			case QUEST_TYPE_MESSAGE:
			{
				int place_num = q_ptr->data.msg.place;
				
				place_type *pl_ptr;
				
				/* Not correct place? */
				if (place_num != p_ptr->place_num) break;
				
				pl_ptr = &place[place_num];
				
				/* Need to be on the surface */
				if (p_ptr->depth) break;
				
				q_ptr->flags |= QUEST_FLAG_ACTIVE;
			}
			
			case QUEST_TYPE_FIND_PLACE:
			{
				int place_num = q_ptr->data.fpl.place;
				
				place_type *pl_ptr;
				
				/* Not correct place? */
				if (place_num != p_ptr->place_num) break;
				
				pl_ptr = &place[place_num];
				
				/* Need to be on the surface */
				if (p_ptr->depth) break;
				
				q_ptr->flags |= QUEST_FLAG_ACTIVE;
			}
		}
	}
}


/*
 * Create a magical staircase
 */
static void create_stairs(int x, int y)
{
	int i = 0;

	int ny, nx;

	cave_type *c_ptr = area(x, y);
	
	/* Paranoia - not on deepest dungeon level */
	if (p_ptr->depth == dungeon()->max_level) return;

	/* Stagger around */
	while ((cave_perma_grid(c_ptr) || c_ptr->o_idx) && !(i > 100))
	{
		/* Pick a location */
		scatter(&nx, &ny, x, y, 1);

		/* Stagger */
		y = ny;
		x = nx;

		/* paranoia - increment counter */
		i++;

		/* paranoia */
		if (!in_bounds2(x, y)) continue;

		c_ptr = area(x, y);
	}

	/* Explain the staircase */
	msgf("A magical staircase appears...");
	
	/* Destroy the fields on the square */
	delete_field(x, y);

	/* Create stairs down */
	cave_set_feat(x, y, FEAT_MORE);
}


static void display_monster_quest(quest_type *q_ptr)
{
	int j, k;
	cave_type *c_ptr;
	
	int x, y;

	monster_race *r_ptr = &r_info[q_ptr->data.dun.r_idx];

	/* Hack -- "unique" monsters must be "unique" */
	if ((FLAG(r_ptr, RF_UNIQUE)) &&
		(r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Hack - the unique is already dead */
		q_ptr->status = QUEST_STATUS_FINISHED;
	}
	else
	{
		bool group;

		for (j = 0; j < (q_ptr->data.dun.max_num
							- q_ptr->data.dun.cur_num); j++)
		{
			for (k = 0; k < 5000; k++)
			{
				/* Find an empty grid */
				while (TRUE)
				{
					y = rand_range(p_ptr->min_hgt + 1,
								   p_ptr->max_hgt - 2);
					x = rand_range(p_ptr->min_wid + 1,
								   p_ptr->max_wid - 2);

					/* Access the grid */
					c_ptr = area(x, y);

					if (!cave_naked_grid(c_ptr)) continue;
					if (distance(x, y, p_ptr->px, p_ptr->py) <
						10) continue;
					else
						break;
				}

				if (FLAG(r_ptr, RF_FRIENDS))
					group = FALSE;
				else
					group = TRUE;

				/* Try to place the monster */
				if (place_monster_aux
					(x, y, q_ptr->data.dun.r_idx, FALSE, group,
					 FALSE, FALSE, TRUE))
				{
					/* Success */
					break;
				}
				else
				{
					/* Failure - Try again */
					continue;
				}
			}
		}
	}
}

static void display_artifact_quest(quest_type *q_ptr)
{
	int	x = rand_range(p_ptr->min_wid + 1, p_ptr->max_wid - 2);
	int y = rand_range(p_ptr->min_hgt + 1, p_ptr->max_hgt - 2);
	
	/* Drop artifact in dungeon */
	create_named_art(q_ptr->data.fit.a_idx, x, y);
}


/*
 * Test each quest to see which ones are created
 */
void trigger_quest_create(byte c_type, vptr data)
{
	int i;
	quest_type *q_ptr;

	/* Ignore data - it may be unused */
	(void)data;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Quest must be chosen */
		if (q_ptr->status != QUEST_STATUS_TAKEN) continue;

		/* Quest must be active */
		if (!(q_ptr->flags & QUEST_FLAG_ACTIVE)) continue;

		/* Must be relevant */
		if (q_ptr->c_type != c_type) continue;

		/* Handle the trigger */
		switch (c_type)
		{
			case QC_NONE:
			{
				/* Paranoia */
				continue;
			}

			case QC_DUN_MONST:
			{
				display_monster_quest(q_ptr);
				continue;
			}
			
			case QC_DUN_ARTIFACT:
			{
				display_artifact_quest(q_ptr);
				continue;
			}
		}
	}
}


/*
 * Test each quest to see if they are completed
 */
void trigger_quest_complete(byte x_type, vptr data)
{
	int i;
	quest_type *q_ptr;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Quest must be chosen */
		if (q_ptr->status != QUEST_STATUS_TAKEN) continue;

		/* Quest must be active */
		if (!(q_ptr->flags & QUEST_FLAG_ACTIVE)) continue;

		/* Must be relevant */
		if (q_ptr->x_type != x_type) continue;

		/* Handle the trigger */
		switch (x_type)
		{
			case QX_NONE:
			{
				/* Paranoia */
				break;
			}

			case QX_KILL_MONST:
			case QX_KILL_UNIQUE:
			{
				monster_type *m_ptr = ((monster_type *)data);

				if (q_ptr->data.bnt.r_idx == m_ptr->r_idx)
				{
					/* Don't count clones */
					if (m_ptr->smart & SM_CLONED) break;

					/* Increment number killed */
					q_ptr->data.bnt.cur_num++;

					if (q_ptr->data.bnt.cur_num >= q_ptr->data.bnt.max_num)
					{
						/* Complete the quest */
						q_ptr->status = QUEST_STATUS_COMPLETED;

						/* Monster is no longer 'QUESTOR' */
						r_info[q_ptr->data.bnt.r_idx].flags[0] &= ~(RF0_QUESTOR);
					}
				}

				break;
			}

			case QX_KILL_WINNER:
			{
				monster_type *m_ptr = ((monster_type *)data);
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				if (q_ptr->data.dun.r_idx == m_ptr->r_idx)
				{
					/* Winner? */
					if (strstr((r_name + r_ptr->name), "Serpent of Chaos"))
					{
						/* Total winner */
						p_ptr->state.total_winner = TRUE;

						/* Redraw the "title" */
						p_ptr->redraw |= (PR_TITLE);

						/* Congratulations */
						msgf("*** CONGRATULATIONS ***");
						msgf("You have won the game!");
						msgf
							("You may retire (commit suicide) when you are ready.");
					}
					else
					{
						/* Oberon */

						/* A message */
						msgf("Well done!");
						msgf("You have beaten Oberon.");
						msgf
							("You now can meet the final challenge of the Serpent of Chaos.");
					}

					/* Complete the quest */
					q_ptr->status = QUEST_STATUS_COMPLETED;

					/* Mega-hack */
					create_stairs(m_ptr->fx, m_ptr->fy);
				}

				break;
			}

			case QX_WILD_ENTER:
			{
				place_type *pl_ptr;
			
				/* Only trigger for the correct quest */
				if (q_ptr != data) continue;
				
				pl_ptr = &place[p_ptr->place_num];
				
				/* Wilderness quests turn off the monsters */
				if (q_ptr->type == QUEST_TYPE_WILD)
				{
				
					/* Unlink location from wilderness */
					int x = ((u16b)p_ptr->wilderness_x / WILD_BLOCK_SIZE);
					int y = ((u16b)p_ptr->wilderness_y / WILD_BLOCK_SIZE);

					wild_done_type *w_ptr = &wild[y][x].done;
					
					/* No more place here */
					w_ptr->place = 0;
					
					/* Decrement active block counter */
					pl_ptr->data--;
					
					/* Are we done yet? */
					if (pl_ptr->data) continue;
				}
				/* Else always completed on first entrance */

				/* Complete the quest */
				q_ptr->status = QUEST_STATUS_COMPLETED;

				break;
			}
			
			case QX_KNOW_ARTIFACT:
			{
				if (*((int *) data) == q_ptr->data.fit.a_idx)
				{
					/* Complete the quest */
					q_ptr->status = QUEST_STATUS_COMPLETED;
				}
			}
			
			case QX_FIND_SHOP:
			{
				place_type *pl_ptr;
			
				/* Towns must match */
				if (p_ptr->place_num != q_ptr->data.msg.place) continue;
				
				pl_ptr = &place[p_ptr->place_num];
				
				/* Do the stores match? */
				if ((store_type *) data != &pl_ptr->store[q_ptr->data.msg.shop]) continue;
				
				/* Complete the quest */
				q_ptr->status = QUEST_STATUS_COMPLETED;
				
				msgf("You have found the place you were looking for and you deliver the message!");
				message_flush();
			}
		}

		/* Finished the quest? */
		if ((q_ptr->status == QUEST_STATUS_FINISHED) ||
			(q_ptr->status == QUEST_STATUS_COMPLETED))
		{
			msgf("You just completed your quest!");
		}
	}
}


/*
 * Look up a quest that corresponds to the given building.
 *
 * If there is none, return NULL.
 *
 * (This assumes one quest per building.)
 */
quest_type *lookup_quest_building(const store_type *b_ptr)
{
	int i;
	quest_type *q_ptr;
	
	place_type *pl_ptr = &place[p_ptr->place_num];
	
	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];
		
		if (q_ptr->place != p_ptr->place_num) continue; 
		
		/* Bounds checking */
		if (q_ptr->shop >= pl_ptr->numstores) continue;
		
		/* A match? */
		if (&pl_ptr->store[q_ptr->shop] == b_ptr)
		{
		
			return (q_ptr);
		}
	}

	/* No match */
	return (NULL);
}


void reward_quest(quest_type *q_ptr)
{
	switch (q_ptr->type)
	{
		case QUEST_TYPE_FIND_ITEM:
		{
			if (q_ptr->status == QUEST_STATUS_COMPLETED)
			{
				msgf("You can keep it if you like.");
				
				/* Allow another quest to be selected */
				q_ptr->place = 0;
				q_ptr->shop = 0;
				
				/* Allow this quest to be deleted if needed */
				q_ptr->status = QUEST_STATUS_FINISHED;
				
				/* Take note */
				if (auto_notes)
				{
					add_note('Q', "Finished quest: %s", q_ptr->name);
				}
			}
			else
			{
				msgf("Still looking?");
			}
			
			break;
		}
		
		case QUEST_TYPE_BOUNTY:
		{
			if (q_ptr->status == QUEST_STATUS_COMPLETED)
			{
				/* Work out reward */
				long reward = q_ptr->reward;
				reward *= reward * 100;
				
				/* Give to player */
				p_ptr->au += reward;
				
				msgf("You are given %ld gold pieces for your efforts.", reward);
				
				/* Allow another quest to be selected */
				q_ptr->place = 0;
				q_ptr->shop = 0;
				
				/* Allow this quest to be deleted if needed */
				q_ptr->status = QUEST_STATUS_FINISHED;
				
				/* Take note */
				if (auto_notes)
				{
					add_note('Q', "Finished quest: %s", q_ptr->name);
				}
			}
			else
			{
				msgf("Still looking for them?");
			}
			
			break;
		}
		
		case QUEST_TYPE_MESSAGE:
		{
			if (q_ptr->status == QUEST_STATUS_COMPLETED)
			{
				/* Work out reward */
				long reward = q_ptr->reward * 100;
				
				/* Give to player */
				p_ptr->au += reward;
				
				msgf("You are given %ld gold pieces for your efforts.", reward);
				
				/* Allow another quest to be selected */
				q_ptr->place = 0;
				q_ptr->shop = 0;
				
				/* Allow this quest to be deleted if needed */
				q_ptr->status = QUEST_STATUS_FINISHED;
				
				/* Take note */
				if (auto_notes)
				{
					add_note('Q', "Finished quest: %s", q_ptr->name);
				}
			}
			else
			{
				msgf("Please deliver it as soon as possible!");
			}
			
			break;
		}
		
		case QUEST_TYPE_FIND_PLACE:
		{
			if (q_ptr->status == QUEST_STATUS_COMPLETED)
			{
				/* Work out reward */
				long reward = q_ptr->reward * 100;
				
				/* Give to player */
				p_ptr->au += reward;
				
				msgf("You are given %ld gold pieces for your efforts.", reward);
				
				/* Allow another quest to be selected */
				q_ptr->place = 0;
				q_ptr->shop = 0;
				
				/* Allow this quest to be deleted if needed */
				q_ptr->status = QUEST_STATUS_FINISHED;
				
				/* Take note */
				if (auto_notes)
				{
					add_note('Q', "Finished quest: %s", q_ptr->name);
				}
			}
			else
			{
				msgf("Please find it as soon as possible!");
			}
			
			break;
		}

		
		default:
		{
		
		}
	}

	message_flush();
}

static const store_type *curr_build;
static int curr_scale;

/* Save the quest giver (current town + building) */
static void set_quest_giver(quest_type *q_ptr)
{
	place_type *pl_ptr = &place[p_ptr->place_num];

	/* Remember quest giver for later */
	q_ptr->place = p_ptr->place_num;
	q_ptr->shop = GET_ARRAY_INDEX(pl_ptr->store, curr_build);
	
	/* We know of this quest */
	q_ptr->flags |= QUEST_FLAG_KNOWN;
}


static quest_type *insert_artifact_quest(u16b a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	
	place_type *pl_ptr;

	cptr town_name, town_dir;
	
	quest_type *q_ptr;

	int q_num;
	
	/* Skip "empty" artifacts */
	if (!a_ptr->name) return (NULL);
		
	/* Cannot make an artifact twice */
	if (a_ptr->cur_num) return (NULL);

	/* No quest items */
	if (FLAG(a_ptr, TR_QUESTITEM)) return (NULL);
	
	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (NULL);

	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_FIND_ITEM;
	
	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	/* We need to place the artifact in the dungeon */
	q_ptr->c_type = QC_DUN_ARTIFACT;
	
	/* Finished when the player identifies it */
	q_ptr->x_type = QX_KNOW_ARTIFACT;
	
	/* Find an available dungeon to place it in */
	
	/* Save the quest data */
	q_ptr->data.fit.a_idx = a_idx;
	q_ptr->data.fit.place = find_good_dungeon(a_ptr->level, curr_scale * 2);
	
	/* Where is it? */
	pl_ptr = &place[q_ptr->data.fit.place];
	
	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, pl_ptr->x, pl_ptr->y);
				
	/* XXX XXX Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Find the relic %s, which is hidden %s of %s.", a_name + a_ptr->name, town_dir, town_name);
	
	/* Artifact is now a quest item */
	SET_FLAG(a_ptr, TR_QUESTITEM);
	
	/* Done */
	return (q_ptr);
}


static bool bad_item_quest = FALSE;
static const store_type *bad_item_quest_giver = NULL;

static bool request_find_item(int dummy)
{	
	quest_type *q_ptr;

	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Hack - if we have tried before, break out early */
	if (bad_item_quest && (bad_item_quest_giver == curr_build))
	{
		msgf("I still don't know of any lost relics!");
	
		message_flush();
		
		/* No available quests. */
		return (FALSE);
	}
	
	/* Try to find a artifact to quest for */
	q_ptr = insert_artifact_quest(randint1(z_info->a_max));
	
	if (!q_ptr)
	{
		msgf("Sorry, I don't know of any missing relics to find right now.");
	
		message_flush();
		
		bad_item_quest = TRUE;
		bad_item_quest_giver = curr_build;
	
		/* No available quests, unfortunately. */
		return (FALSE);
	}
				
	/* Display a helpful message. */
	msgf("%s", q_ptr->name);
				  
	message_flush();
			
	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);
			
	/* Exit */
	return (TRUE);
}


static bool monster_quest(const monster_race *r_ptr)
{
	/* No bounty quests for multiplying monsters */
	if (FLAG(r_ptr, RF_MULTIPLY)) return FALSE;

	/* No bounty to kill friendly monsters */
	if (FLAG(r_ptr, RF_FRIENDLY)) return FALSE;
	
	/* Only "hard" monsters for quests */
	if (FLAG(r_ptr, RF_NEVER_MOVE) || FLAG(r_ptr, RF_FRIENDS)) return FALSE;
	
	/* No uniques that are already dead */
	if ((FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
			&& (r_ptr->cur_num >= r_ptr->max_num))
		{
			return FALSE;
		}

	return TRUE;
}


static quest_type *insert_bounty_quest(u16b r_idx, u16b num)
{
	quest_type *q_ptr;

	int q_num;
	
	monster_race *r_ptr = &r_info[r_idx];
	
	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (NULL);

	q_ptr = &quest[q_num];

	/* Bounty quest */
	q_ptr->type = QUEST_TYPE_BOUNTY;
	
	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	if (num != 1)
	{
		char buf[80];
		strcpy(buf, r_name + r_ptr->name);
		plural_aux(buf);

		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %d %s.", (int)num, buf);
	}
	else
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %s.", r_name + r_ptr->name);
	}
	
	/* No need to specially create anything */
	q_ptr->c_type = QC_NONE;

	/* We need to trigger when the monsters are killed */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		q_ptr->x_type = QX_KILL_UNIQUE;
	}
	else
	{
		q_ptr->x_type = QX_KILL_MONST;
	}
	
	/* Save the quest data */
	q_ptr->data.bnt.r_idx = r_idx;
	q_ptr->data.bnt.cur_num = 0;
	q_ptr->data.bnt.max_num = num;
	q_ptr->reward = r_ptr->level;

	/* Done */
	return (q_ptr);
}

static bool request_bounty(int dummy)
{
	int i;
	
	u16b num;
	u16b best_r_idx = 1;
	int best_level = 1;

	int r_idx;
	monster_race *r_ptr;
	
	quest_type *q_ptr;
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Get monster */
	for (i = 0; i < MAX_TRIES; i++)
	{
		/*
		 * Random monster out of depth
		 * (depending on level + number of quests)
		 */
		r_idx = get_mon_num(p_ptr->max_lev + curr_scale);

		r_ptr = &r_info[r_idx];
		
		/* Look at the monster - only "hard" monsters for quests */
		if (!monster_quest(r_ptr)) continue;

		/* Save the index if the monster is deeper than current monster */
		if (!best_r_idx || (r_info[r_idx].level > best_level))
		{
			best_r_idx = r_idx;
			best_level = r_info[r_idx].level;
		}

		/* Accept monsters that are a few levels out of depth */
		if (best_level > p_ptr->max_lev * 2) break;
	}

	r_ptr = &r_info[best_r_idx];

	/* Get the number of monsters */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		num = 1;
	}
	else if (FLAG(r_ptr, RF_UNIQUE_7))
	{
		num = randint1(r_ptr->max_num - r_ptr->cur_num);
	}
	else
	{
		num = 5 + randint0(p_ptr->max_lev / 2) / r_ptr->rarity;
	}
	
	/* Generate the quest */
	q_ptr = insert_bounty_quest(best_r_idx, num);

	if (!q_ptr)
	{
		msgf("Sorry, I don't need any bounties today.");
	
		message_flush();
	
		/* No available quests, unfortunately. */
		return (FALSE);
	}
		
	/* Show it on the screen? */
			
			
	/* Display a helpful message. */
	msgf("My bounty is: %s.", q_ptr->name);
				  
	message_flush();
			
	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);
			
	/* Exit */
	return (TRUE);
}


static quest_type *insert_message_quest(int dist)
{
	place_type *pl_ptr;
	
	quest_type *q_ptr;
	store_type *st_ptr;
	
	cptr owner;

	int store;
	byte owner_num;
	u16b place_num;

	/* Get a new quest */
	int q_num = q_pop();
		
	/* Paranoia */
	if (!q_num) return (NULL);
	
	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_MESSAGE;
	
	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	/* We don't need any special creation operation */
	q_ptr->c_type = QC_NONE;
	
	/* Finished when the player finds it */
	q_ptr->x_type = QX_FIND_SHOP;
	
	/* Find a town that is roughly dist wilderness blocks away */
	place_num = find_good_town(&dist);
	
	/* Get the place */
	pl_ptr = &place[place_num];
	
	/* Find a store at that town */
	while (TRUE)
	{
		store = randint0(pl_ptr->numstores);
		
		st_ptr = &pl_ptr->store[store];
		
		/* Want a store with an owner */
		if (st_ptr->owner)
		{
			int field_num = wild_build[st_ptr->type].field;
			
			/* Hack XXX XXX - not homes */
			if (field_num == FT_STORE_HOME) continue;
			
			owner_num = t_info[field_num].data_init[0];
			
			/* Store or building? */
			if (build_is_store(wild_build[st_ptr->type].type))
			{
				/* Store */
				const owner_type *ot_ptr = &owners[owner_num][st_ptr->owner];
				
				owner = ot_ptr->owner_name;
			}
			else
			{
				/* Building */
				const b_own_type *bo_ptr = &b_owners[owner_num][st_ptr->owner];
				
				owner = bo_ptr->owner_name;
			}
		
			break;
		}
	}
		
	/* XXX XXX Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Carry a message to %s in %s.", owner, pl_ptr->name);
	
	
	/* Save the quest data */
	q_ptr->data.msg.shop = store;
	q_ptr->data.msg.place = place_num;
	
	/* Set the reward level */
	q_ptr->reward = dist;
	
	/* Done */
	return (q_ptr);
}

static bool request_message(int dummy)
{
	quest_type *q_ptr;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	/*
	 * Generate a quest to send a message to a town
	 * roughly 20 to 50 wilderness squares away.
	 */
	q_ptr = insert_message_quest(curr_scale * 2);

	if (!q_ptr)
	{
		msgf("Sorry, I don't need any to send any messages today.");
	
		message_flush();
	
		/* No available quests, unfortunately. */
		return (FALSE);
	}
		
	/* Show it on the screen? */
			
	/* Display a helpful message. */
	msgf("%s.", q_ptr->name);
				  
	message_flush();
			
	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);
			
	/* Exit */
	return (TRUE);
}

static quest_type *insert_find_place_quest(int dist)
{
	place_type *pl_ptr;
	
	quest_type *q_ptr;

	cptr town_name, town_dir;

	u16b place_num;

	/* Get a new quest */
	int q_num = q_pop();
		
	/* Paranoia */
	if (!q_num) return (NULL);
	
	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_FIND_PLACE;
	
	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	/* We don't need any special creation operation */
	q_ptr->c_type = QC_NONE;
	
	/* Finished when the player finds it */
	q_ptr->x_type = QX_WILD_ENTER;
	
	/* Find a dungeon that is roughly dist wilderness blocks away */
	place_num = find_good_dungeon(curr_scale * 2, dist);
	
	/* Get the place */
	pl_ptr = &place[place_num];
	
	pl_ptr->quest_num = q_num;
	
	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, pl_ptr->x, pl_ptr->y);
				
	/* XXX XXX Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Find a certain lost ruin, which is hidden %s of %s.", town_dir, town_name);

	q_ptr->data.fpl.place = place_num;
	
	/* Set the reward level */
	q_ptr->reward = dist + curr_scale * 2;
	
	/* Done */
	return (q_ptr);
}

static bool request_find_place(int dummy)
{
	quest_type *q_ptr;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	/*
	 * Generate a quest to find a dungeon
	 * roughly 75 wilderness blocks away
	 */
	q_ptr = insert_find_place_quest(75);

	if (!q_ptr)
	{
		msgf("Sorry, I'm not looking for any lost ruins.");
	
		message_flush();
	
		/* No available quests, unfortunately. */
		return (FALSE);
	}
		
	/* Show it on the screen? */
			
	/* Display a helpful message. */
	msgf("%s.", q_ptr->name);
				  
	message_flush();
			
	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);
			
	/* Exit */
	return (TRUE);
}



#define QUEST_MENU_MAX		5
#define QUEST_MENU_RELIC	3

/* The quest selection menu */
static menu_type quest_menu[QUEST_MENU_MAX] =
{
	{"To hunt down a bounty of monsters", NULL, request_bounty, MN_ACTIVE},
	{"To send a message to someone far away", NULL, request_message, MN_ACTIVE},
	{"To find a lost ruin", NULL, request_find_place, MN_ACTIVE},
	{"To find a lost relic", NULL, request_find_item, MN_ACTIVE},
	MENU_END
};

void request_quest(const store_type *b_ptr, int scale)
{
	/* Save building so we can remember the quest giver */
	curr_build = b_ptr;
	
	/* Save scale so we can work out how hard to make the quest */
	curr_scale = scale;
	
	/* Only allow artifact quests from large castles */
	if (scale < 20)
	{
		quest_menu[QUEST_MENU_RELIC].flags &= ~(MN_ACTIVE);
	}
	else
	{
		quest_menu[QUEST_MENU_RELIC].flags |= MN_ACTIVE;
	}

	display_menu(quest_menu, -1, FALSE, NULL, "What type of quest would you like?");
}



/*
 * Print quest status of all active quests
 */
bool do_cmd_knowledge_quests(int dummy)
{
	FILE *fff;
	char file_name[1024];
	char name[80];
	char tmp_str[256];

	quest_type *q_ptr;
	int i;
	
	bool taken;
	bool finished;
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Do we know about it? */
		if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;
		
		taken = (q_ptr->status == QUEST_STATUS_TAKEN);
		finished = (q_ptr->status == QUEST_STATUS_FINISHED);

		/* See what type of quest it is */
		switch (q_ptr->type)
		{
			case QUEST_TYPE_NONE:
			{
				/* Paranoia */
				continue;
			}

			case QUEST_TYPE_BOUNTY:
			{
				if (taken)
				{
					/* Hack - this is simple */
					strnfmt(tmp_str, 256, "%s\n\n", q_ptr->name);
				}
				else
				{
					/* Hack - this is simple */
					strnfmt(tmp_str, 256, "%s (Killed)\n", q_ptr->name);
				}

				break;
			}

			case QUEST_TYPE_DUNGEON:
			{
				monster_race *r_ptr = &r_info[q_ptr->data.dun.r_idx];

				char level[20];

				strncpy(name, r_name + r_ptr->name, 80);

				/* In feet, or in levels */
				if (depth_in_feet)
				{
					strnfmt(level, 20, "%4dft",
							(int)q_ptr->data.dun.level * 50);
				}
				else
				{
					strnfmt(level, 20, "%3d", (int)q_ptr->data.dun.level);
				}

				if (taken)
				{
					/* Hack - assume kill n monsters of type m */
					if (q_ptr->data.dun.max_num > 1)
					{
						plural_aux(name);

						strnfmt(tmp_str, 256,
								"%s (Dungeon level: %s)\n\n  Kill %d %s, have killed %d.\n\n",
								q_ptr->name, level,
								(int)q_ptr->data.dun.max_num, name,
								(int)q_ptr->data.dun.cur_num);
					}
					else
					{
						strnfmt(tmp_str, 256, "%s (Dungeon level: %s)\n\n",
								q_ptr->name, level);
					}
				}
				else
				{
					/* Assume we've completed it for now */
					strnfmt(tmp_str, 256,
							"%s (Completed on dungeon level %s). \n",
							q_ptr->name, level);
				}

				break;
			}
			
			case QUEST_TYPE_FIND_ITEM:
			case QUEST_TYPE_FIND_PLACE:
			{
				if (taken)
				{
					/* Hack - this is simple */
					strnfmt(tmp_str, 256, "%s\n\n", q_ptr->name);
				}
				else if (finished)
				{
					/* Hack - this is simple */
					strnfmt(tmp_str, 256, "%s (Completed)\n", q_ptr->name);
				}
				else
				{
					/* Hack - this is simple */
					strnfmt(tmp_str, 256, "%s (Found)\n", q_ptr->name);
				}
				break;
			}

			case QUEST_TYPE_WILD:
			case QUEST_TYPE_MESSAGE:
			{
				if (taken)
				{
					/* Hack - this is simple */
					strnfmt(tmp_str, 256, "%s\n\n", q_ptr->name);
				}
				else
				{
					/* Hack - this is simple */
					strnfmt(tmp_str, 256, "%s (Completed)\n", q_ptr->name);
				}

				break;
			}

			default:
			{
				/* Paranoia */
				strnfmt(tmp_str, 256, "Invalid quest type!");
			}
		}

		/* Copy to the file */
		froff(fff, "%s", tmp_str);
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Quest status", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}


/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a quest of the given type.
 *
 * The general selections are not allowed to include "unique" monsters.
 */


/*
 * Hack - Monster validation macro
 *
 * Line 1 -- forbid town monsters
 * Line 2 -- forbid uniques
 * Line 3 -- forbid aquatic monsters
 */
#define quest_monster_okay(I) \
	(!FLAG(&r_info[I], RF_WILD_TOWN) && \
	 !FLAG(&r_info[I], RF_UNIQUE) && \
	 !FLAG(&r_info[I], RF_AQUATIC))


#ifdef UNUSED_FUNC
/*
 * Helper monster selection function
 */
static bool quest_aux_simple(int r_idx)
{
	/* Okay */
	return (quest_monster_okay(r_idx));
}

#endif /* UNUSED_FUNC */


/*
 * Helper function for selecting undead
 */
static bool quest_aux_undead(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require Undead */
	if (!FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for selecting orcs
 */
static bool quest_aux_orc(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require orc */
	if (!(FLAG(r_ptr, RF_ORC))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for selecting trolls
 */
static bool quest_aux_troll(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require troll */
	if (!(FLAG(r_ptr, RF_TROLL))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for selecting giants
 */
static bool quest_aux_giant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require giant */
	if (!(FLAG(r_ptr, RF_GIANT))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for selecting dragons
 */
static bool quest_aux_dragon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require dragon */
	if (!(FLAG(r_ptr, RF_DRAGON))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}

static int pick_quest_type(quest_aux_type *l_ptr, int level)
{
	int tmp, total;

	quest_aux_type *n_ptr;

	int i;

	/* Calculate the total possibilities */
	for (i = 0, total = 0; TRUE; i++)
	{
		n_ptr = &l_ptr[i];

		/* Note end */
		if (!n_ptr->hook_func) break;

		/* Ignore excessive depth */
		if (n_ptr->level > level) continue;

		/* Count this possibility */
		total += n_ptr->chance * MAX_DEPTH / (level - n_ptr->level + 5);
	}

	if (!total) return (-1);

	/* Pick a random type */
	tmp = randint0(total);

	/* Find this type */
	for (i = 0, total = 0; TRUE; i++)
	{
		n_ptr = &l_ptr[i];

		/* Note end */
		if (!n_ptr->hook_func) break;

		/* Ignore excessive depth */
		if (n_ptr->level > level) continue;

		/* Count this possibility */
		total += n_ptr->chance * MAX_DEPTH / (level - n_ptr->level + 5);

		/* Found the type */
		if (tmp < total) break;
	}

	return (i);
}



static quest_aux_type camp_types[] =
{
	{quest_aux_undead, 10, 1, "undead"},
	{quest_aux_orc, 20, 2, "orc"},
	{quest_aux_troll, 40, 4, "troll"},
	{quest_aux_giant, 60, 1, "giant"},
	{quest_aux_dragon, 80, 1, "dragon"},
	{NULL, 0, 0, NULL},
};



/*
 * Pick a quest to use
 */
void pick_wild_quest(int *xsize, int *ysize, byte *flags)
{
	/* Hack - don't worry too much now, we only have one type of quest */

	/* Random size */
	*xsize = randint1(5);
	*ysize = randint1(5);

	/* On normal terrain */
	*flags = Q_GEN_PICKY;
}

/*
 * Look to see if a wilderness block is able to have
 * a quest overlayed on top.
 */
bool quest_blank(int x, int y, int xsize, int ysize, int place_num, byte flags)
{
	int i, j;
	wild_gen2_type *w_ptr;
	place_type *pl_ptr = &place[place_num];

	/* Hack - Population check */
	if (randint0(256) > wild[y][x].trans.pop_map) return (FALSE);

	for (i = x - 1; i < x + xsize + 2; i++)
	{
		for (j = y - 1; j < y + ysize + 2; j++)
		{
			/* Hack - Not next to boundary */
			if ((i <= 0) || (i >= max_wild - 1) ||
				(j <= 0) || (j >= max_wild - 1))
			{
				return (FALSE);
			}

			w_ptr = &wild[j][i].trans;

			/* No place already */
			if (w_ptr->place) return (FALSE);

			/* Picky quests require "normal terrain" */
			if (flags & Q_GEN_PICKY)
			{
				/* No water or lava or acid */
				if (w_ptr->
					info & (WILD_INFO_WATER | WILD_INFO_LAVA | WILD_INFO_ACID))
				{
					return (FALSE);
				}
			}

			/* Ocean quests must be on water */
			if (flags & Q_GEN_OCEAN)
			{
				/* Not on Ocean? */
				if (w_ptr->hgt_map >= (256 / SEA_FRACTION)) return (FALSE);
			}
			else
			{
				/* Otherwise, Ocean is not allowed */
				if (w_ptr->hgt_map < (256 / SEA_FRACTION)) return (FALSE);
			}
		}
	}

	/* Look to see if another town / quest is too close */
	for (i = 1; i < place_num; i++)
	{
		if (distance(place[i].x, place[i].y, x, y) < MIN_DIST_QUEST)
		{
			/* Too close? */
			return (FALSE);
		}
	}

	/* Save size */
	pl_ptr->xsize = xsize;
	pl_ptr->ysize = ysize;

	/* Ok then */
	return (TRUE);
}


/*
 * Create a quest in the wilderness
 */
bool create_quest(int x, int y, int place_num)
{
	int i, j;
	int q_num, qtype;
	cptr town_name, town_dir;

	wild_type *w_ptr = &wild[y][x];

	place_type *pl_ptr = &place[place_num];

	quest_type *q_ptr;

	/* Select type of monster to place in the camp */
	qtype = pick_quest_type(camp_types, (255 - w_ptr->trans.law_map) / 3);

	/* Is the area too easy for the quests? */
	if (qtype == -1) return (FALSE);

	/* Get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (FALSE);

	/* Get a random seed for later */
	pl_ptr->seed = randint0(0x10000000);

	/* Quest */
	pl_ptr->type = TOWN_QUEST;
	pl_ptr->monst_type = TOWN_MONST_MONST;
	pl_ptr->x = x;
	pl_ptr->y = y;
	pl_ptr->quest_num = q_num;

	/* Data value is used as a counter of "active" blocks */
	pl_ptr->data = 0;

	if ((!pl_ptr->xsize) || (!pl_ptr->ysize)) quit("Zero quest size");

	/* Link wilderness to quest */
	for (i = 0; i < pl_ptr->xsize; i++)
	{
		for (j = 0; j < pl_ptr->ysize; j++)
		{
			w_ptr = &wild[y + j][x + i];

			/*
			 * Add quest to wilderness
			 * Note: only 255 can be stored currently.
			 */
			w_ptr->trans.place = (byte)place_num;

			/* Increment "active block" counter */
			pl_ptr->data++;
		}
	}

	/* Set up quest */
	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_WILD;

	/* We don't need a special generator */
	q_ptr->c_type = QC_NONE;

	/* We need to trigger when the player enters the wilderness block */
	q_ptr->x_type = QX_WILD_ENTER;
	
	/* Get name and direction of closest town to quest */
	town_name = describe_quest_location(&town_dir, pl_ptr->x, pl_ptr->y);

	/* Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Defeat the %s camp %s of %s.",
				  camp_types[qtype].name, town_dir, town_name);

	/* Save the quest data */
	q_ptr->data.wld.place = place_num;
	q_ptr->data.wld.data = qtype;
	/* q_ptr->data.wld.depth = (255 - w_ptr->trans.law_map) / 3; */

	return (TRUE);
}


/*
 * Draw the quest onto its region
 */
void draw_quest(place_type *pl_ptr)
{
	int x, y, n;
	int i, j;

	wild_type *w_ptr = &wild[pl_ptr->y][pl_ptr->x];

	quest_type *q_ptr = &quest[pl_ptr->quest_num];

	cave_type *c_ptr;

	/* Object theme */
	obj_theme theme;

	int depth = w_ptr->done.mon_gen;

	/* Paranoia */
	if (pl_ptr->region) quit("Quest already has region during creation.");

	/* Get region */
	create_region(pl_ptr, pl_ptr->xsize * WILD_BLOCK_SIZE,
						 pl_ptr->ysize * WILD_BLOCK_SIZE,
						 REGION_OVER);

	/* Hack - do not increment refcount here - let allocate_block do that */

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant quest layout */
	Rand_value = pl_ptr->seed;

	/* Apply the monster restriction */
	get_mon_num_prep(camp_types[q_ptr->data.wld.data].hook_func);

	/* Set theme for weapons / armour */
	theme.treasure = 0;
	theme.combat = 100;
	theme.magic = 0;
	theme.tools = 0;

	init_match_theme(theme);

	/* Prepare allocation table */
	get_obj_num_prep(kind_is_theme);

	/* Pick number random spots within region */
	n = (pl_ptr->xsize * pl_ptr->ysize) / 4;

	while (n != 0)
	{
		/* Decrement counter */
		n--;

		/* Get spot */
		x = randint0(pl_ptr->xsize * 2);
		y = randint0(pl_ptr->ysize * 2);

		/* Place ground */
		for (i = 0; i < 8; i++)
		{
			for (j = 0; j < 8; j++)
			{
				/* Get location */
				c_ptr = cave_p(x * 8 + i, y * 8 + j);

				/* Draw a roughly circular blob */
				if (randint0(distance(0, 0, i, j)) < 4)
				{
					if (one_in_(3))
					{
						c_ptr->feat = FEAT_PEBBLES;
					}
					else
					{
						c_ptr->feat = FEAT_DIRT;
					}

					/* Place monsters on spots */
					if (one_in_(QUEST_CAMP_MON))
					{
						/* Pick a race to clone */
						c_ptr->m_idx = get_mon_num(depth);
					}

					/* Place weapons + armour around the spots */
					if (one_in_(QUEST_CAMP_OBJ))
					{
						c_ptr->o_idx = get_obj_num(depth, depth / 3);
					}
				}
			}
		}
	}


	/* Set theme for junk */
	theme.treasure = 5;
	theme.combat = 0;
	theme.magic = 0;
	theme.tools = 5;

	init_match_theme(theme);

	/* Clear allocation table */
	get_obj_num_prep(kind_is_theme);

	/* Scatter stuff over the region */
	for (i = 0; i < pl_ptr->xsize * WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < pl_ptr->ysize * WILD_BLOCK_SIZE; j++)
		{
			/* Only on some squares */
			if (!one_in_(QUEST_CAMP_SCATTER)) continue;

			/* Get location */
			c_ptr = cave_p(i, j);

			/* Not on allocated squares */
			if (c_ptr->feat) continue;

			if (one_in_(3))
			{
				c_ptr->feat = FEAT_PEBBLES;
			}
			else
			{
				c_ptr->feat = FEAT_DIRT;
			}

			/* Place monsters on spots */
			if (one_in_(QUEST_CAMP_MON))
			{
				/* Pick a race to clone */
				c_ptr->m_idx = get_mon_num(depth);

				/* Place junk under monsters */
				if (one_in_(QUEST_CAMP_OBJ))
				{
					c_ptr->o_idx = get_obj_num(depth, 0);
				}
			}
		}
	}

	/* Activate quest + we know about the quest */
	q_ptr->flags |= (QUEST_FLAG_ACTIVE);

	/* Hack - we now take this quest */
	if (q_ptr->status == QUEST_STATUS_UNTAKEN)
	{
		q_ptr->status = QUEST_STATUS_TAKEN;
	}

	/* Mega-hack Give a message if we "discover" it */
	quest_discovery();

	/* We know about it now */
	q_ptr->flags |= QUEST_FLAG_KNOWN;

	/* Hack XXX XXX (No quest-giving store yet) */

	/* Hack -- use the "complex" RNG */
	Rand_quick = FALSE;

	/* Remove the monster restriction */
	get_mon_num_prep(NULL);
}
