/* File: quest.c */

/* Purpose: Quest code */

/*
 * Copyright (c) 1989, 1999 James E. Wilson, Robert A. Koeneke,
 * Robert Ruehlmann
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

	/* Warn the player */
	msgf("Too many quests!");

	/* Oops */
	return (0);
}

/*
 * Wipe a quest
 */
static void quest_wipe(int q_idx)
{
	quest_type *q_ptr = &quest[q_idx];

	q_ptr->status = QUEST_STATUS_UNTAKEN;
	q_ptr->flags = 0x00;
	q_ptr->type = QUEST_TYPE_UNKNOWN;

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
	 * - it is ignored since q_ptr->type = UNKNOWN
	 */
}

/* The quest list will be dynamic soon... */
#if 0

/*
 * Copy a quest from one slot to another
 */
static void quest_copy(int src, int dst)
{
	/* Copy the structure */
	COPY(&quest[src], &quest[dst], quest_type);

	/* Insert other needed copy code here - ie fixing info / item array */
}


/*
 * Remove a quest from the list
 */
static void quest_remove(int q_idx)
{
	int i;

	/* Paranoia */
	if ((q_idx < 0) || (q_idx >= q_max)) return;

	/* Shift quests down */
	for (i = q_idx + 1; i < q_max; i++)
	{
		quest_copy(i, i - 1);
	}

	/* Paranoia */
	if (!q_max) return;

	/* Wipe empty slot */
	quest_wipe(q_max);

	/* Decrease total number of quests */
	q_max--;
}


/*
 * Clean the list of quests, removing 'completed' ones
 */
static void prune_quests(void)
{
	int i;

	quest_type *q_ptr;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/*
		 * Remove 'completed' quests.
		 *
		 * Set a quest to be 'finished' if you want to
		 * keep the player memory of it.
		 */
		if (q_ptr->status == QUEST_STATUS_COMPLETED)
		{
			quest_remove(i);
		}
	}
}

#endif /* 0 */

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
	if (r_ptr->flags1 & RF1_UNIQUE)
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
		(void)strnfmt(q_ptr->name, 60, "Kill %d %s.", (int)num, buf);
	}
	else
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 60, "Kill %s.", r_name + r_ptr->name);
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

static bool monster_quest(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Random quests are in the dungeon */
	if (!(r_ptr->flags8 & RF8_DUNGEON)) return FALSE;

	/* No random quests for aquatic monsters */
	if (r_ptr->flags7 & RF7_AQUATIC) return FALSE;

	/* No random quests for multiplying monsters */
	if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;

	/* No quests to kill friendly monsters */
	if (r_ptr->flags7 & RF7_FRIENDLY) return FALSE;
	
	/* Only "hard" monsters for quests */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE | RF1_FRIENDS)) return FALSE;

	return TRUE;
}

/*
 * Quests
 *
 * q_num is the number of random quests to use.
 * If it is -1, then we ask how many to use.
 */
void get_player_quests(int q_num)
{
	char inp[80];

	monster_race *r_ptr;

	int r_idx;
	int i, j, v, level;

	int best_level, best_r_idx;
	int num;
	int q_idx;

	/* Reset number of quests */
	q_max = 1;

	/* Clear all the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_wipe(i);
	}
	
	if (q_num == -1)
	{
		/* Extra info */
		put_fstr(5, 15,
				"You can enter the number of quests you'd like to perform in addition\n"
				"to the two obligatory ones ( Oberon and the Serpent of Chaos )\n"
				"In case you do not want any additional quests, just enter 0\n"
				"If you want a random number of random quests, just enter *");

		/* Ask the number of additional quests */
		while (TRUE)
		{
			put_fstr(2, 20, "Number of additional quests? (<50) ");

			/* Get a the number of additional quest */
			while (TRUE)
			{
				/* Move the cursor */
				Term_gotoxy(37, 20);

				/* Default */
				strcpy(inp, "20");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 3)) inp[0] = '\0';

				/* Check for random number of quests */
				if (inp[0] == '*')
				{
					/* 0 to 49 random quests */
					v = randint0(50);
				}
				else
				{
					v = atoi(inp);
				}

				/* Break on valid input */
				if ((v < 50) && (v >= 0)) break;
			}
			break;
		}
	}
	else
	{
		v = q_num;
	}
#if 0

	/*
	 * Hard-code number of dungeon quests until
	 * we get the quest-giver stores working.
	 */
	v = 20;

#endif /* 0 */

	/* Prepare monster list */
	get_mon_num_prep(monster_quest);	

	/* Generate quests */
	for (i = 0; i < v; i++)
	{
		level = (i * 96 + 48) / v + 1;

		best_r_idx = 1;
		best_level = 1;

		/* Get monster */
		for (j = 0; j < MAX_TRIES; j++)
		{
			int depth;
			int min_depth;

			if (!ironman_deep_quests)
			{
				depth = level + 6 +
					randint1(level * v / 200 + 1) +
					randint1(level * v / 200 + 1);
				min_depth = level + (level / 20) + 1;
			}
			else
			{
				depth = level + rand_range(15, 30);
				min_depth = level + v / 10 + 10;
			}

			/*
			 * Random monster out of depth
			 * (depending on level + number of quests)
			 */
			r_idx = get_mon_num(depth);

			r_ptr = &r_info[r_idx];

			/* Save the index if the monster is deeper than current monster */
			if (!best_r_idx || (r_info[r_idx].level > best_level))
			{
				best_r_idx = r_idx;
				best_level = r_info[r_idx].level;
			}

			/* Accept monsters that are a few levels out of depth */
			if (best_level > min_depth) break;
		}

		r_ptr = &r_info[best_r_idx];

		/* Get the number of monsters */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			/* Mark uniques */
			r_ptr->flags1 |= RF1_QUESTOR;

			num = 1;
		}
		else if (r_ptr->flags3 & RF3_UNIQUE_7)
		{
			/* Mark uniques */
			r_ptr->flags1 |= RF1_QUESTOR;

			num = randint1(r_ptr->max_num);
		}
		else
		{
			num = 5 + (s16b)randint0(level / 3 + 5) / r_ptr->rarity;
		}

		/* Create the quest */
		insert_dungeon_monster_quest(best_r_idx, num, level);
	}
	
	/* Restore allocation table */
	get_mon_num_prep(NULL);

	/* Add the winner quests */

	/* Hack XXX XXX Oberon, hard coded */
	q_idx = insert_dungeon_monster_quest(QW_OBERON, 1, 99);
	quest[q_idx].x_type = QX_KILL_WINNER;
	quest[q_idx].flags |= QUEST_FLAG_KNOWN;
	quest[q_idx].status = QUEST_STATUS_TAKEN;

	/* Hack XXX XXX Serpent, hard coded */
	q_idx = insert_dungeon_monster_quest(QW_SERPENT, 1, 100);
	quest[q_idx].x_type = QX_KILL_WINNER;
	quest[q_idx].flags |= QUEST_FLAG_KNOWN;
	quest[q_idx].status = QUEST_STATUS_TAKEN;
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
			case QUEST_TYPE_UNKNOWN:
			{
				/* Paranoia */
				continue;
			}

			case QUEST_TYPE_GENERAL:
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

				if (r_ptr->flags1 & RF1_UNIQUE)
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

			default:
			{
				/* Paranoia */
				continue;
			}
		}
	}
}


/*
 * Is this dungeon level a quest level? */
bool is_quest_level(int level)
{
	int i;

	quest_type *q_ptr;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Must be dungeon quest */
		if (q_ptr->type != QUEST_TYPE_DUNGEON) continue;

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

		/* Is the quest relevant? */
		if ((q_ptr->type == QUEST_TYPE_GENERAL)
			|| ((q_ptr->type == QUEST_TYPE_DUNGEON)
				&& (q_ptr->data.dun.level == level)))
		{
			q_ptr->flags |= QUEST_FLAG_ACTIVE;

			/* Hack - toggle QUESTOR flag */
			if (q_ptr->type == QUEST_TYPE_DUNGEON)
			{
				r_info[q_ptr->data.dun.r_idx].flags1 |= RF1_QUESTOR;

				/* Hack - we notice the dungeon quest */
				if (q_ptr->status == QUEST_STATUS_UNTAKEN)
				{
					q_ptr->status = QUEST_STATUS_TAKEN;
				}
			}
		}
		else
		{
			/* No longer created or active */
			q_ptr->flags &= ~(QUEST_FLAG_ACTIVE | QUEST_FLAG_CREATED);
		}
	}
}


/*
 * Count the number of quests chosen so far
 */
int number_of_quests(void)
{
	int i;

	int count = 0;

	for (i = 0; i < q_max; i++)
	{
		if (quest[i].status != QUEST_STATUS_UNTAKEN)
		{
			/* Increment count of quests taken. */
			count++;
		}
	}

	/* Return the number of quests taken */
	return (count);
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
	if (p_ptr->depth == max_dun_level()) return;

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

	/* Create stairs down */
	cave_set_feat(x, y, FEAT_MORE);
}

/*
 * Give the reward for the indicated quest
 */
static void quest_reward(int num, int x, int y)
{
	object_type *o_ptr;
	
	dun_type *dundata = place[p_ptr->place_num].dungeon;
	
	obj_theme *o_theme = &dundata->theme;

	/* Ignore num for now */
	(void)num;

	while (TRUE)
	{
		/* Average of 20 great objects per game */
		if (randint0(number_of_quests()) < 20)
		{
			/* Make a great object */
			o_ptr = make_object(base_level(), 30, o_theme);
		}
		else
		{
			/* Make a good object */
			o_ptr = make_object(base_level(), 15, o_theme);
		}

		if (!o_ptr) continue;

		/* We need a 'good' item - so check the price */
		if (object_value_real(o_ptr) > 100 * p_ptr->depth) break;
	}

	/* Drop it in the dungeon */
	drop_near(o_ptr, -1, x, y);
}


/*
 * Test each quest to see which ones are created
 */
void trigger_quest_create(byte c_type, vptr data)
{
	int i, j, k;
	quest_type *q_ptr;
	cave_type *c_ptr;

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
				int x, y;

				monster_race *r_ptr = &r_info[q_ptr->data.dun.r_idx];

				/* Hack -- "unique" monsters must be "unique" */
				if ((r_ptr->flags1 & RF1_UNIQUE) &&
					(r_ptr->cur_num >= r_ptr->max_num))
				{
					/* The unique is already dead */
					q_ptr->status = QUEST_STATUS_FINISHED;
				}
				else
				{
					bool group;

					/* Hard quests -> revive all monsters */
					if (ironman_hard_quests)
					{
						q_ptr->data.dun.cur_num = 0;
					}

					for (j = 0;
						 j <
						 (q_ptr->data.dun.max_num - q_ptr->data.dun.cur_num);
						 j++)
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

							if (r_ptr->flags1 & RF1_FRIENDS)
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
		}

		/* The quest is created */
		q_ptr->flags |= QUEST_FLAG_CREATED;
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

				if (q_ptr->data.dun.r_idx == m_ptr->r_idx)
				{
					/* Don't count clones */
					if (m_ptr->smart & SM_CLONED) break;

					/* Increment number killed */
					q_ptr->data.dun.cur_num++;

					if (q_ptr->data.dun.cur_num >= q_ptr->data.dun.max_num)
					{
						/* Complete the quest */
						q_ptr->status = QUEST_STATUS_COMPLETED;

						/* Create some stairs */
						create_stairs(m_ptr->fx, m_ptr->fy);

						/* Drop the reward */
						quest_reward(q_ptr->reward, m_ptr->fx, m_ptr->fy);

						/* Monster is no longer 'QUESTOR' */
						r_info[q_ptr->data.dun.r_idx].flags1 &= ~(RF1_QUESTOR);
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
					q_ptr->status = QUEST_STATUS_FINISHED;

					/* Mega-hack */
					create_stairs(m_ptr->fx, m_ptr->fy);
				}

				break;
			}

			case QX_WILD_ENTER:
			{
				/* Only trigger for the correct quest */
				if (q_ptr != data) continue;

				/* Complete the quest */
				q_ptr->status = QUEST_STATUS_FINISHED;

				break;
			}
		}

		/*
		 * Hack - Give a message for 'completed' quests.
		 * XXX XXX XXX (Not yet - no quest-givers)
		 */
		/* if (q_ptr->status == QUEST_STATUS_COMPLETED) */
		if (q_ptr->status == QUEST_STATUS_FINISHED)
		{
#if 0
			/* Take note */
			if (auto_notes)
			{
				add_note('Q', "Finished quest: %d %s",
						quest[i].max_num,
						(r_name + r_info[quest[i].r_idx].name));
			}
#endif /* 0 */

			msgf("You just completed your quest!");
		}

	}
}

#if 0
/*
 * Display quest information
 */
static void get_questinfo(int questnum)
{
	int i;
	int old_quest;
	char tmp_str[80];


	/* Clear the text */
	for (i = 0; i < 10; i++)
	{
		quest_text[i][0] = '\0';
	}


	/* Print the quest info */
	prtf(0, 5, "Quest Information (Danger level: %d)",
			quest[questnum].level);

	prtf(0, 7, quest[questnum].name);

	for (i = 0; i < 10; i++)
	{
		put_fstr(0, i + 8, CLR_YELLOW "%s", quest_text[i]);
	}
}


/*
 * Request a quest from the Lord.
 */
static void castle_quest(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int q_index = 0;
	monster_race *r_ptr;
	quest_type *q_ptr;
	cptr name;


	clear_bldg(7, 18);

	/* Current quest of the building */
	q_index = get_qindex();

	/* Is there a quest available at the building? */
	if (!q_index)
	{
		put_fstr(0, 8, "I don't have a quest for you at the moment.");
		return;
	}

	q_ptr = &quest[q_index];

	/* Quest is completed */
	if (q_ptr->status == QUEST_STATUS_COMPLETED)
	{
		/* Rewarded quest */
		q_ptr->status = QUEST_STATUS_REWARDED;

		get_questinfo(q_index);
	}
	/* Failed quest */
	else if (q_ptr->status == QUEST_STATUS_FAILED)
	{
		get_questinfo(q_index);

		/* Mark quest as done (but failed) */
		q_ptr->status = QUEST_STATUS_FAILED_DONE;
	}
	/* Quest is still unfinished */
	else if (q_ptr->status == QUEST_STATUS_TAKEN)
	{
		put_fstr(0, 8, "You have not completed your current quest yet!");
		put_fstr(0, 9, "Use CTRL-Q to check the status of your quest.");
		put_fstr(0, 12, "Return when you have completed your quest.");
	}
	/* No quest yet */
	else if (q_ptr->status == QUEST_STATUS_UNTAKEN)
	{
		q_ptr->status = QUEST_STATUS_TAKEN;

		/* Assign a new quest */
		if (q_ptr->type == QUEST_TYPE_KILL_ANY_LEVEL)
		{
			if (q_ptr->r_idx == 0)
			{
				/* Random monster at least 5 - 10 levels out of deep */
				q_ptr->r_idx = get_mon_num(q_ptr->level + rand_range(5, 10));
			}

			r_ptr = &r_info[q_ptr->r_idx];

			while ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->rarity != 1))
			{
				q_ptr->r_idx = get_mon_num(q_ptr->level) + rand_range(5, 10);
				r_ptr = &r_info[q_ptr->r_idx];
			}

			if (q_ptr->max_num == 0)
			{
				/* Random monster number */
				if (randint1(10) > 7)
					q_ptr->max_num = 1;
				else
					q_ptr->max_num = rand_range(2, 4);
			}

			q_ptr->cur_num = 0;
			name = (r_name + r_ptr->name);
			msgf("Your quest: kill %d %s", q_ptr->max_num, name);
			message_flush();
		}
		else
		{
			get_questinfo(q_index);
		}
	}
}

#endif /* 0 */


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

		/* See what type of quest it is */
		switch (q_ptr->type)
		{
			case QUEST_TYPE_UNKNOWN:
			{
				/* Paranoia */
				continue;
			}

			case QUEST_TYPE_GENERAL:
			{

				/* Paranoia */
				continue;
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

				if (q_ptr->status == QUEST_STATUS_TAKEN)
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

			case QUEST_TYPE_WILD:
			{
				if (q_ptr->status == QUEST_STATUS_TAKEN)
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
	(!(r_info[I].flags8 & RF8_WILD_TOWN) && \
	 !(r_info[I].flags1 & RF1_UNIQUE) && \
	 !(r_info[I].flags7 & RF7_AQUATIC))


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
	if (!(r_ptr->flags3 & (RF3_UNDEAD))) return (FALSE);

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
	if (!(r_ptr->flags3 & RF3_ORC)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

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
	if (!(r_ptr->flags3 & RF3_TROLL)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

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
	if (!(r_ptr->flags3 & RF3_GIANT)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

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
	if (!(r_ptr->flags3 & RF3_DRAGON)) return (FALSE);

	/* Decline undead */
	if (r_ptr->flags3 & RF3_UNDEAD) return (FALSE);

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
	int best_town, best_dist;
	int dx, dy;
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

	/* Find the nearest town */
	best_dist = 99999;
	best_town = 0;
	for (i = 0; i < place_count; i++)
	{
		int d;

		/* Only real towns */
		if (place[i].type != TOWN_FRACT)
			continue;

		/* Find closest town */
		d = distance(pl_ptr->x, pl_ptr->y, place[i].x, place[i].y);
		if (d < best_dist)
		{
			best_dist = d;
			best_town = i;
		}
	}

	town_name = place[best_town].name;
	dx = pl_ptr->x - place[best_town].x;
	dy = pl_ptr->y - place[best_town].y;

	if (ABS(dy) > ABS(dx) * 3)
	{
		if (dy > 0)
			town_dir = "south";
		else
			town_dir = "north";
	}
	else if (ABS(dx) > ABS(dy) * 3)
	{
		if (dx > 0)
			town_dir = "east";
		else
			town_dir = "west";
	}
	else if (dx > 0)
	{
		if (dy > 0)
			town_dir = "south-east";
		else
			town_dir = "north-east";
	}
	else
	{
		if (dy > 0)
			town_dir = "south-west";
		else
			town_dir = "north-west";
	}

	/* Create quest name */
	(void)strnfmt(q_ptr->name, 60, "Defeat the %s camp %s of %s.",
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
void draw_quest(u16b place_num)
{
	int x, y, n;
	int i, j;

	place_type *pl_ptr = &place[place_num];

	wild_type *w_ptr = &wild[pl_ptr->y][pl_ptr->x];

	quest_type *q_ptr = &quest[pl_ptr->quest_num];

	cave_type *c_ptr;

	/* Object theme */
	obj_theme theme;

	int depth = w_ptr->done.mon_gen;

	/* Paranoia */
	if (pl_ptr->region) quit("Quest already has region during creation.");

	/* Get region */
	pl_ptr->region = (s16b)create_region(pl_ptr->xsize * WILD_BLOCK_SIZE,
										 pl_ptr->ysize * WILD_BLOCK_SIZE,
										 REGION_NULL);

	/* Hack - do not increment refcount here - let allocate_block do that */

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant quest layout */
	Rand_value = place[place_num].seed;

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

	/* Activate quest + create quest + we know about the quest */
	q_ptr->flags |= (QUEST_FLAG_ACTIVE | QUEST_FLAG_CREATED);

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
