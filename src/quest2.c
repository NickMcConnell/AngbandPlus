/* File: quest1.c */

/* Purpose: Quest code */

/*
 * Copyright (c) 1989, 2003 James E. Wilson, Robert A. Koeneke,
 *                          Robert Ruehlmann, Steven Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 * Changes copyright (c) 2008 Mangojuice, licenced under the GPL.
 */

#include "angband.h"

#include "wild.h"

#define MAX_TRIES 30
extern const store_type *curr_build;

int in_quest(void)
{
	if (current_quest == NULL) return (0);
	else return (1);
}

static int curr_scale;

/* See if this quest is a wild quest and activate it if necessary */
void discover_wild_quest(int q_num)
{
	/* Is there a quest here? */
	if (!q_num) return;

	/* Is it a wild quest */
	if (quest[q_num].type != QUEST_TYPE_WILD) return;

	/* Was this a taken quest? */
	if (quest[q_num].status == QUEST_STATUS_UNTAKEN)
	{
		/* Now we take it */
		quest[q_num].status = QUEST_STATUS_TAKEN;

		/* Hack -- make it active to make the discovery */
		quest[q_num].flags |= QUEST_FLAG_ACTIVE;

		/* Announce */
		quest_discovery();
	}
}

/*
 * This function returns the closest name of the closest town and the
 * direction to that town.  If known == TRUE then the player must have
 * seen the closest town too, in order not to give away clues to the map.
 */
cptr describe_quest_location(cptr * dirn, int x, int y, bool known)
{
	int i;
	int dx, dy;

	/* Find the nearest town */
	int best_dist = 99999;
	int best_town = 0;

	for (i = 0; i < place_count; i++)
	{
		bool visit = FALSE;
		int d;

		wild_type *w_ptr;

		/* Only real towns */
		if (place[i].type != PL_TOWN_FRACT) continue;

		/* Should this be a known town? */
		if (known)
		{
			for (dx = 0; dx < 8 && !visit; dx++)
			{
				for (dy = 0; dy < 8 && !visit; dy++)
				{
					/* Get wilderness square where the town could be */
					w_ptr = &wild[place[i].y + dy][place[i].x + dx];

					/* Is this a town square */
					if (w_ptr->done.place != i) continue;

					/* Has the player visited this square? */
					visit |= (w_ptr->done.info & WILD_INFO_SEEN);
				}
			}

			/* Unmapped town */
			if (!visit) continue;
		}

		/* Find closest town */
		d = distance(x, y, place[i].x+4, place[i].y+4);

		/* Keep track of the best town */
		if (d < best_dist)
		{
			best_dist = d;
			best_town = i;
		}
	}

	/* Just in case */
	if (!best_town)
	{
		*dirn = "in the middle";
		return ("nowhere");
	}

	/* Approximates the center of the city */
	dx = x - (place[best_town].x + 4);
	dy = y - (place[best_town].y + 4);

	if (distance(x, y, place[best_town].x + 4, place[best_town].y + 4) < 6)
	{
		*dirn = "near";
	}
	else if (ABS(dy) > ABS(dx) * 3)
	{
		if (dy > 0)
			*dirn = "south of";
		else
			*dirn = "north of";
	}
	else if (ABS(dx) > ABS(dy) * 3)
	{
		if (dx > 0)
			*dirn = "east of";
		else
			*dirn = "west of";
	}
	else if (dx > 0)
	{
		if (dy > 0)
			*dirn = "south-east of";
		else
			*dirn = "north-east of";
	}
	else
	{
		if (dy > 0)
			*dirn = "south-west of";
		else
			*dirn = "north-west of";
	}

	return (place[best_town].name);
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
			case QUEST_TYPE_DEFEND:
			{

				/* Paranoia */
				continue;
			}

			case QUEST_TYPE_DUNGEON:
			{
				monster_race *r_ptr = &r_info[q_ptr->data.dun.r_idx];
				int q_num = q_ptr->data.dun.max_num - q_ptr->data.dun.cur_num;

				/* Assume the quest is a 'kill n monsters quest' for now. */
				strcpy(name, mon_race_name(r_ptr));

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
 * Is this dungeon level a special (no-stairs) quest level? */
bool is_special_level(int level)
{
	int i;
	quest_type *q_ptr;

	/* The winner quest on this level would be a problem */
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

int active_level(quest_type *q_ptr)
{
	int max_dead, all_dead;

	/* Paranoia */
	if (!q_ptr) return(0);

	max_dead = q_ptr->data.fix.min_level;
	all_dead = max_dead;

	switch (q_ptr->type)
	{
		case QUEST_TYPE_FIXED_KILL:
		case QUEST_TYPE_FIXED_BOSS:
			/* Never more than one level */
			return(FALSE);
		case QUEST_TYPE_FIXED_DEN:
			max_dead += q_ptr->data.fix.data.den.cleared;
			all_dead += q_ptr->data.fix.data.den.levels;
			break;
		case QUEST_TYPE_FIXED_CLEAROUT:
			max_dead += q_ptr->data.fix.data.clearout.cleared;
			all_dead += q_ptr->data.fix.data.clearout.levels;
			break;
		default:
			/* Shouldn't happen... */
			return (0);
	}

	/* If quest is dead, no active level anymore. */
	if (current_quest->status > QUEST_STATUS_TAKEN ||
		current_quest->data.fix.attempts == 0)
	{
		return (all_dead);
	}

	return(max_dead);
}


/*
 * This function is called in change_level.
 *
 * Its purpose is to handle transitional stuff about quests:
 * maintain current_quest, check about entering or leaving levels in fixed quests,
 * "turn on" bounty / artifact quests, et cetera.
 */
void activate_quests(int level)
{
	int i;
	monster_race *r_ptr;
	quest_type *q_ptr;

	/* First, deal with moving in or out of quest dungeons. */
	if (current_quest && current_quest->status == QUEST_STATUS_TAKEN)
	{
		/* Did we just leave the first active level? */
		if (level == 0 || level == active_level(current_quest)-1)
		{
			/* We just left a quest dungeon that isn't completed.  Decrease attempts. */
			current_quest->data.fix.attempts--;
			if (current_quest->data.fix.attempts == 0)
			{
				msgf ("The dungeon collapses behind you!");
				msgf ("You failed to complete your quest!");
				current_quest->status = QUEST_STATUS_FAILED;
			}
			else if (current_quest->data.fix.attempts == 1)
				/* Warn */
				msgf ("The stairs start to crumble...");
		}
	}

	/* Set current quest */
	current_quest = (level && place[p_ptr->place_num].quest_num ? &quest[place[p_ptr->place_num].quest_num] : NULL);

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Is the quest still there? */
		if (q_ptr->status > QUEST_STATUS_TAKEN) continue;

		/* Assume no longer active */
		q_ptr->flags &= ~(QUEST_FLAG_ACTIVE);

		/* Is the quest relevant? */
		switch (q_ptr->type)
		{
			case QUEST_TYPE_DUNGEON:
			{
				/* Set QUESTOR flag if the target is unique. */
				if (FLAG(&r_info[q_ptr->data.dun.r_idx], RF_UNIQUE))
					SET_FLAG(&r_info[q_ptr->data.dun.r_idx], RF_QUESTOR);

				/* Correct dungeon level? */
				if (q_ptr->data.dun.level != level) break;

				/* Current quest? */
				if (q_ptr->status != QUEST_STATUS_TAKEN ||
					!(q_ptr->flags & QUEST_FLAG_KNOWN))
					break;

				/* Hack - toggle QUESTOR flag */
				SET_FLAG(&r_info[q_ptr->data.dun.r_idx], RF_QUESTOR);

				/* Activate the quest */
				q_ptr->flags |= QUEST_FLAG_ACTIVE;

				break;
			}

			case QUEST_TYPE_BOUNTY:
			{
				/* Always active.  Previously this would have led to the quest monster
				   being placed on the level all the time.  Now, we will do the necessary
				   checks when we attempt to create the quest monsters.
				   This way, quest monsters can come up at random.
				*/
				r_ptr = &r_info[q_ptr->data.bnt.r_idx];

				/* Mark boss as questor if unique */
				if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
					SET_FLAG(r_ptr, RF_QUESTOR);

				/* Current quest? */
				if (q_ptr->status != QUEST_STATUS_TAKEN ||
					!(q_ptr->flags & QUEST_FLAG_KNOWN))
					break;

				/* Hack - toggle QUESTOR flag */
				SET_FLAG(r_ptr, RF_QUESTOR);

				q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			}

			case QUEST_TYPE_WILD:
			{
				/* In Wilderness? */
				if (!level)
					q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			}

			case QUEST_TYPE_FIND_ITEM:
			{
				/* Always active until the relic has been id'd */
				q_ptr->flags |= QUEST_FLAG_ACTIVE;
			}

			case QUEST_TYPE_MESSAGE:
			{
				/* Need to be on the surface */
				if (!p_ptr->depth)
					q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			}

			case QUEST_TYPE_FIND_PLACE:
			{
				/* Need to be on the surface */
				if (!p_ptr->depth)
					q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			}
			case QUEST_TYPE_DEFEND:
			{
				/* Always active, while quest isn't overdue */
				SET_FLAG(&r_info[q_ptr->data.bnt.r_idx], RF_QUESTOR);

				q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			}
			case QUEST_TYPE_LOAN:
				/* Always active */
				q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			case QUEST_TYPE_FIXED_CLEAROUT:
				if (q_ptr == current_quest)
					q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;
			case QUEST_TYPE_FIXED_BOSS:
				if (q_ptr == current_quest)
					q_ptr->flags |= QUEST_FLAG_ACTIVE;

				r_ptr = &r_info[q_ptr->data.fix.data.boss.r_idx];

				/* Mark boss as questor if unique */
				if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
					SET_FLAG(r_ptr, RF_QUESTOR);

				/* Mark boss as questor if we're in the level */
				else if (q_ptr == current_quest)
					SET_FLAG(r_ptr, RF_QUESTOR);

				/* Otherwise, *unset* the questor flag. */
				else
					r_ptr->flags[0] &= ~RF0_QUESTOR;

				break;
			case QUEST_TYPE_FIXED_KILL:
				if (q_ptr == current_quest)
					q_ptr->flags |= QUEST_FLAG_ACTIVE;

				r_ptr = &r_info[q_ptr->data.fix.data.kill.r_idx];

				/* Mark boss as questor if unique */
				if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
					SET_FLAG(r_ptr, RF_QUESTOR);

				/* Mark boss as questor if we're in the level */
				else if (q_ptr == current_quest)
					SET_FLAG(r_ptr, RF_QUESTOR);

				/* Otherwise, *unset* the questor flag. */
				else
					r_ptr->flags[0] &= ~RF0_QUESTOR;

				break;
			case QUEST_TYPE_FIXED_DEN:
				if (q_ptr == current_quest)
					q_ptr->flags |= QUEST_FLAG_ACTIVE;
				break;

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
		int number, r_idx = 0;
		int ymax, ymin, xmax, xmin;

		ymax = p_ptr->max_hgt - 2;
		ymin = p_ptr->min_hgt + 1;
		xmax = p_ptr->max_wid - 2;
		xmin = p_ptr->min_wid + 1;

		/* Create a number of monsters depending on quest_type */
		if (q_ptr->type == QUEST_TYPE_BOUNTY)
		{
			/* One at a time */
			number = 1;

			/* Which monster? */
			r_idx = q_ptr->data.bnt.r_idx;
		}
		else if (q_ptr->type == QUEST_TYPE_DUNGEON)
		{
			/* All remaining at once */
			number = q_ptr->data.dun.max_num - q_ptr->data.dun.cur_num;

			/* Which monster? */
			r_idx = q_ptr->data.dun.r_idx;
		}
		else if (q_ptr->type == QUEST_TYPE_FIXED_KILL || q_ptr->type == QUEST_TYPE_FIXED_BOSS)
		{
			/* Don't do anything; handled in cave_gen currently */
			return;
		}
		else /* (q_ptr->type == QUEST_TYPE_DEFEND) */
		{

			/* All at once */
			number = q_ptr->data.bnt.max_num - q_ptr->data.bnt.cur_num;

			/* Which monster? */
			r_idx = q_ptr->data.bnt.r_idx;

			ymax = MIN(ymax, p_ptr->py + 30);
			ymin = MAX(ymin, p_ptr->py - 30);
			xmax = MIN(xmax, p_ptr->px + 30);
			xmin = MAX(xmin, p_ptr->px - 30);
		}

		for (j = 0; j < number; j++)
		{
			for (k = 0; k < 5000; k++)
			{
				/* Find an empty grid */
				while (TRUE)
				{
					y = rand_range(ymin, ymax);
					x = rand_range(xmin, xmax);

					/* Access the grid */
					c_ptr = area(x, y);

					if (!cave_naked_grid(c_ptr)) continue;
					if (distance(x, y, p_ptr->px, p_ptr->py) < 10)
						continue;
					else
						break;
				}

				if (FLAG(r_ptr, RF_FRIENDS))
					group = FALSE;
				else
					group = TRUE;

				/* Try to place the monster */
				if (place_monster_aux
					(x, y, r_idx, FALSE, group, FALSE, FALSE, TRUE))
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

	/* Warn the player, maybe */
	if (!preserve_mode) msgf ("You feel there is something special about this level...");
}

/*
 * Test each quest to see which ones are created
 */
void trigger_quest_create(byte c_type, vptr data)
{
	int i;
	quest_type *q_ptr;
	int level = p_ptr->depth;

	/* Ignore data - it may be unused */
	(void)data;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Mega-hack!  Only consider the current quest if there is one. */
		if (current_quest) {
			q_ptr = current_quest;
			/* For efficiency... */
			i = q_max;
		}

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
				dun_type *d_ptr = dungeon();
				monster_race *r_ptr;

				if (q_ptr->type == QUEST_TYPE_DUNGEON)
				{
					if (level == q_ptr->data.dun.level)
						display_monster_quest(q_ptr);
				}
				else if (q_ptr->type == QUEST_TYPE_BOUNTY)
				{
					r_ptr = &r_info[q_ptr->data.bnt.r_idx];

					/* Is the player inside the right sort of dungeon? */
					if (level && (r_ptr->flags[7] & d_ptr->habitat || !monster_theme))
					{
						/* Frequent when the monster would not be that much out of depth. */
						if ((level >= r_ptr->level-5) && (one_in_(2)))
						{
							/* Activate the quest */
							display_monster_quest(q_ptr);
						}
					}
				}
				else if (q_ptr->type == QUEST_TYPE_FIXED_KILL || q_ptr->type == QUEST_TYPE_FIXED_BOSS)
				{
					display_monster_quest(q_ptr);
				}
				continue;
			}

			case QC_DUN_ARTIFACT:
			{
				int place_num = q_ptr->data.fit.place;

				place_type *pl_ptr;

				/* Not correct place? */
				if (place_num != p_ptr->place_num) continue;

				pl_ptr = &place[place_num];

				/* Need to be in the dungeon, and low enough */
				if (p_ptr->depth < a_info[q_ptr->data.fit.a_idx].level) continue;

				display_artifact_quest(q_ptr);

				continue;
			}
		}
	}
}

static void trigger_quest_complete_clear(void)
{
	int i;
	quest_type *q_ptr;

	/* Just in case ... */
	if (!current_quest) return;

	q_ptr = current_quest;

	/* Count monsters */
	for (i = 0; i < z_info->m_max; i++)
	{
		if (m_list[i].r_idx)
		{
			/* Found a non-dead monster. */
			if (i > 20) compact_monsters(0);
			return;
		}
	}


	switch (q_ptr->type)
	{
		case QUEST_TYPE_FIXED_KILL:
			/* Do nothing.  It is only the target that matters here. */
			return;
		case QUEST_TYPE_FIXED_CLEAROUT:
			/* Only counts if this is the first uncleared level. */
			if (p_ptr->depth == q_ptr->data.fix.min_level + q_ptr->data.fix.data.clearout.cleared)
			{
				q_ptr->data.fix.data.clearout.cleared++;
				/* If we've cleared everything, quest is completed. */
				if (q_ptr->data.fix.data.clearout.cleared == q_ptr->data.fix.data.clearout.levels)
					q_ptr->status = QUEST_STATUS_COMPLETED;

				/* Otherwise, create stairs down. */
				else
				{
					msgf ("You have made progress in your quest!");
					create_stairs(p_ptr->px, p_ptr->py);
				}
			}
			break;
		case QUEST_TYPE_FIXED_BOSS:
			/* Clears the quest, only if the boss is not a unique.  */
			if (q_ptr->x_type != QX_CLEAR_LEVEL) return;
			else q_ptr->status = QUEST_STATUS_COMPLETED;
			break;
		case QUEST_TYPE_FIXED_DEN:
			/* Only counts if this is the first uncleared level. */
			if (p_ptr->depth == q_ptr->data.fix.min_level + q_ptr->data.fix.data.den.cleared)
			{
				q_ptr->data.fix.data.den.cleared++;
				/* If we've cleared everything, quest is completed. */
				if (q_ptr->data.fix.data.den.cleared == q_ptr->data.fix.data.den.levels)
					q_ptr->status = QUEST_STATUS_COMPLETED;
					/* Otherwise, create stairs down. */
				else
				{
					msgf ("You have made progress in your quest!");
					create_stairs(p_ptr->px, p_ptr->py);
				}
			}
			break;
		default:
			break;
	}

	if ((q_ptr->status == QUEST_STATUS_FINISHED) ||
		(q_ptr->status == QUEST_STATUS_COMPLETED))
	{
		msgf("You just completed your quest!");
	}
}

static void trigger_quest_complete_kill(monster_type * m_ptr)
{
	int i;
	quest_type *q_ptr;
	int r_idx;
	int test_r_idx = m_ptr->r_idx;
	s16b max, *cur, c;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Quest must be chosen */
		if (q_ptr->status != QUEST_STATUS_TAKEN) continue;

		/* Quest must be active */
		if (!(q_ptr->flags & QUEST_FLAG_ACTIVE)) continue;

		/* Quest must be known */
		if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;

		/* Must be relevant */
		if (q_ptr->x_type != QX_KILL_MONST &&
			q_ptr->x_type != QX_KILL_UNIQUE) continue;

		/* Determine key information about this quest */
		if (q_ptr->type == QUEST_TYPE_FIXED_KILL)
		{
			r_idx = q_ptr->data.fix.data.kill.r_idx;
			max = 1;
			cur = &c;
			c = 0;
		}
		else if (q_ptr->type == QUEST_TYPE_FIXED_BOSS)
		{
			r_idx = q_ptr->data.fix.data.boss.r_idx;
			max = 1;
			cur = &c;
			c = 0;
		}
		else if (q_ptr->type == QUEST_TYPE_DEFEND ||
				 q_ptr->type == QUEST_TYPE_BOUNTY)
		{
			r_idx = q_ptr->data.bnt.r_idx;
			max = q_ptr->data.bnt.max_num;
			cur = (u16b *)&q_ptr->data.bnt.cur_num;
		}
		else if (q_ptr->type == QUEST_TYPE_DUNGEON)
		{
			r_idx = q_ptr->data.dun.r_idx;
			max = q_ptr->data.dun.max_num;
			cur = &q_ptr->data.dun.cur_num;
		}
		else
		{
			/* Shouldn't happen... */
			continue;
		}

		if (r_idx == test_r_idx)
		{
			/* Don't count clones */
			if (m_ptr->smart & SM_CLONED) break;

			/* Increment number killed */
			(*cur)++;

			if (*cur >= max)
			{
				/* Complete the quest */
				q_ptr->status = QUEST_STATUS_COMPLETED;

				/* Monster is no longer 'QUESTOR' */
				r_info[r_idx].flags[0] &= ~(RF0_QUESTOR);
			}
		}

		if ((q_ptr->status == QUEST_STATUS_FINISHED) ||
			(q_ptr->status == QUEST_STATUS_COMPLETED))
		{
			msgf("You just completed your quest!");
		}
	}
}

static void trigger_quest_complete_winner(monster_type * m_ptr)
{
	int i;
	quest_type *q_ptr;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Quest must be chosen */
		if (q_ptr->status != QUEST_STATUS_TAKEN) continue;

		/* Quest must be active */
		if (!(q_ptr->flags & QUEST_FLAG_ACTIVE)) continue;

		/* Quest must be known */
		if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;

		/* Must be relevant */
		if (q_ptr->x_type != QX_KILL_WINNER) continue;

		if (q_ptr->data.dun.r_idx == m_ptr->r_idx)
		{
			/* Winner? */
			if (mon_name_cont(r_ptr, "Serpent of Chaos") ||
				(mon_name_cont(r_ptr, "Morgoth,") && !amber_monsters))
			{
				/* Total winner */
				p_ptr->state.total_winner = TRUE;

				/* Redraw the "title" */
				p_ptr->redraw |= (PR_TITLE);

				/* Congratulations */
				msgf("*** CONGRATULATIONS ***");
				msgf("You have won the game!");
				msgf("You may retire (commit suicide) when you are ready.");
			}
			else
			{
				/* Oberon */

				/* A message */
				msgf("Well done!");
				msgf((amber_monsters ? "You have beaten Oberon." : "You have beaten Sauron."));
				msgf((amber_monsters ? "You now can meet the final challenge of the Serpent of Chaos."
									: "You can now meet the final challenge of Morgoth."));
			}

			/* Complete the quest */
			q_ptr->status = QUEST_STATUS_COMPLETED;

			/* Mega-hack */
			create_stairs(m_ptr->fx, m_ptr->fy);
		}
	}
}

static void trigger_quest_complete_wild(quest_type * q_ptr)
{
	place_type *pl_ptr;

	/* Quest must be chosen */
	if (q_ptr->status != QUEST_STATUS_TAKEN) return;

	/* Quest must be active */
	if (!(q_ptr->flags & QUEST_FLAG_ACTIVE)) return;

	/* Quest must be known */
	if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) return;

	/* Must be relevant */
	if (q_ptr->x_type != QX_WILD_ENTER) return;

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
		if (pl_ptr->data) return	;

		/* Finish the quest */
		q_ptr->status = QUEST_STATUS_FINISHED;
	}

	if (q_ptr->type == QUEST_TYPE_FIND_PLACE)
	{
		msgf("You find the ruin you were looking for.");

		/* Complete the quest */
		q_ptr->status = QUEST_STATUS_COMPLETED;
	}

	if ((q_ptr->status == QUEST_STATUS_FINISHED) ||
		(q_ptr->status == QUEST_STATUS_COMPLETED))
	{
		msgf("You just completed your quest!");
	}
}

static void trigger_quest_complete_artifact(int a_idx)
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

		/* Quest must be known */
		if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;

		/* Must be relevant */
		if (q_ptr->x_type != QX_KNOW_ARTIFACT) continue;

		if (a_idx == q_ptr->data.fit.a_idx)
		{
			msgf("You find the relic you were looking for.");

			/* Complete the quest */
			q_ptr->status = QUEST_STATUS_COMPLETED;
		}
		else continue;

		if ((q_ptr->status == QUEST_STATUS_FINISHED) ||
			(q_ptr->status == QUEST_STATUS_COMPLETED))
		{
			msgf("You just completed your quest!");
		}
	}
}

static void trigger_quest_complete_message(store_type * st_ptr)
{
	int i;
	quest_type *q_ptr;
	place_type *pl_ptr;

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		/* Quest must be chosen */
		if (q_ptr->status != QUEST_STATUS_TAKEN) continue;

		/* Quest must be active */
		if (!(q_ptr->flags & QUEST_FLAG_ACTIVE)) continue;

		/* Quest must be known */
		if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;

		/* Must be relevant */
		if (q_ptr->x_type != QX_FIND_SHOP) continue;

		/* Towns must match */
		if (p_ptr->place_num != q_ptr->data.msg.place) continue;

		pl_ptr = &place[p_ptr->place_num];

		/* Do the stores match? */
		if (st_ptr != &pl_ptr->store[q_ptr->data.msg.shop]) continue;

		/* Complete the quest */
		q_ptr->status = QUEST_STATUS_COMPLETED;

		msgf("You have found the place you were looking for and you deliver the message!");
		message_flush();
	}
}

static void trigger_quest_complete_loan(void)
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

		/* Quest must be known */
		if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;

		/* Must be relevant */
		if (q_ptr->x_type != QX_LOAN) continue;

		/* Quest is finished */
		q_ptr->status = QUEST_STATUS_FINISHED;

		/* Hack: We don't want repaid loans showing up in the list of quests. */
		q_ptr->flags &= ~QUEST_FLAG_KNOWN;
	}
}

/*
 * Test each quest to see if they are completed
 */
void trigger_quest_complete(byte x_type, vptr data)
{
	switch(x_type)
	{
		case QX_CLEAR_LEVEL:
			trigger_quest_complete_clear();
			break;
		case QX_KILL_MONST:
		case QX_KILL_UNIQUE:
			trigger_quest_complete_kill((monster_type *)data);
			break;
		case QX_KILL_WINNER:
			trigger_quest_complete_winner((monster_type *)data);
			break;
		case QX_WILD_ENTER:
		   	trigger_quest_complete_wild((quest_type *)data);
		   	break;
		case QX_KNOW_ARTIFACT:
			trigger_quest_complete_artifact(*((int *) data));
			break;
		case QX_FIND_SHOP:
			trigger_quest_complete_message((store_type *) data);
			break;
		case QX_LOAN:
			trigger_quest_complete_loan();
			break;
	}
}

/*
 * Do whatever should be done when quest number num has
 * run out of time.
 *
 * XXX not very sophisticated yet.
 */
void trigger_quest_fail(u16b num)
{
	if (quest[num].type == QUEST_TYPE_LOAN) return;  /* Doesn't really timeout. */
	if (quest[num].status != QUEST_STATUS_TAKEN) return;  /* Irrelevant */

	msgf ("You failed to complete your quest in time.");
	quest[num].status = QUEST_STATUS_FAILED;

	/* No reward for overdue quests */
	quest[num].reward = 0;
}

/*
 * Look up an (taken) quest that corresponds to the given building.
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

		/* Disregard untaken quests */
		if (q_ptr->status == QUEST_STATUS_UNTAKEN) continue;

		/* Disregard finished quests */
		if (q_ptr->status == QUEST_STATUS_FINISHED) continue;

		if (q_ptr->status == QUEST_STATUS_FINISHED_FAILED) continue;

		/* A match? */
		if (&pl_ptr->store[q_ptr->shop] == b_ptr)
		{
			return (q_ptr);
		}
	}

	/* No match */
	return (NULL);
}

/*
 * Look up the next quest that corresponds to the given building.
 *
 * If there is none, return NULL.
 *
 * (This assumes one current quest per building.)
 */
int lookup_quest_building_next(const store_type *b_ptr)
{
	int i, best = -1;
	quest_type *q_ptr;

	place_type *pl_ptr = &place[p_ptr->place_num];

	for (i = 0; i < q_max; i++)
	{
		q_ptr = &quest[i];

		if (q_ptr->place != p_ptr->place_num) continue;

		/* Bounds checking */
		if (q_ptr->shop >= pl_ptr->numstores) continue;

		/* Only consider quests from this building */
		if (&pl_ptr->store[q_ptr->shop] != b_ptr) continue;

		/* If there's a quest that's taken, it is the right one */
		if (q_ptr->status == QUEST_STATUS_TAKEN) return(i);

		/* Reject quests that can't be taken */
		if (q_ptr->status != QUEST_STATUS_UNTAKEN) continue;

		/* Found a first one? */
		if (best == -1)  best = i;

		/* Found a better one? */
		else if (q_ptr->level < quest[best].level)  best = i;
	}

	/* Never found it */
	if (best == -1)
	{
		/* No further quests. */
		if (b_ptr->type == BUILD_CASTLE0 || b_ptr->type == BUILD_CASTLE1 ||
			b_ptr->type == BUILD_CASTLE2)
		{
			/* Find the first winner quest */
			for (i = 0; i < q_max; i++)
			{
				q_ptr = &quest[i];
				/* Disregard quests we can't start now */
				if (q_ptr->status != QUEST_STATUS_UNTAKEN) continue;

				/* Ignore non-winner quests */
				if (q_ptr->x_type != QX_KILL_WINNER) continue;

				/* Found it */
				return (i);
			}

			/* If we get here, there was none */
			return (-1);
		}
		else
		{
			/* Don't give out the winner quests in non-castles */
			return (-1);
		}
	}
	else
	{
		return (best);
	}
}

/*
 * Grant appropriate rewards for finishing the quest
 */
static void give_reward(store_type * st_ptr, quest_type * q_ptr)
{
	int stat = -1;
	bool get_gold = FALSE;
	bool get_xp = FALSE;
	bool get_item = FALSE;
	int gp = 0;
	int xp = 0;
	obj_theme theme;

	int r = q_ptr->reward;

	theme.treasure = 0;
	theme.tools = 0;
	theme.magic = 50;
	theme.combat = 50;

	/* Based on the building, decide on reward type */
	switch(st_ptr->type)
	{
		/* Much gold, gain DEX or CHR */
		case BUILD_THIEVES_GUILD:
			get_gold = TRUE;
			gp = r;
			stat = (one_in_(2) ? A_DEX : A_CHR);
			break;
		/* Any stat to gain, equally likely */
		/* Experience and gold */
		case BUILD_CASTLE0:
		case BUILD_CASTLE1:
		case BUILD_CASTLE2:
			get_gold = TRUE;
			gp = (r/2);
			get_xp = TRUE;
			xp = (r/5);
			stat = randint0(6);
			break;
		/* Some xp, combat item, gain str / int / dex*/
		case BUILD_RANGER_GUILD:
			get_xp = TRUE;
			xp = (r/5);
			stat = (one_in_(3) ? A_STR : (one_in_(2) ? A_DEX : A_INT));
			get_item = TRUE;
			theme.combat = 100;
			theme.magic = 0;
			break;
		/* Some gold, combat item, gain str or con */
		case BUILD_WARRIOR_GUILD:
			get_gold = TRUE;
			gp = r/2;
			stat = (one_in_(2) ? A_STR : A_CON);
			get_item = TRUE;
			theme.combat = 100;
			theme.magic = 0;
			break;
		/* Some xp, magic item, gain wis or con */
		case BUILD_CATHEDRAL:
			get_xp = TRUE;
			xp = (r/5);
			stat = (one_in_(2) ? A_WIS : A_CON);
			get_item = TRUE;
			theme.combat = 0;
			theme.magic = 100;
			break;
		/* Some gold, magic item, gain int or wis */
		case BUILD_MAGE_GUILD:
			get_gold = TRUE;
			gp = r/2;
			stat = (one_in_(2) ? A_INT : A_WIS);
			get_item = TRUE;
			theme.combat = 0;
			theme.magic = 75;
			theme.treasure = 25;
			break;
		/* Small amount of gold and XP */
		case BUILD_FARM:
			get_gold = TRUE;
			gp = r/10;
			get_xp = TRUE;
			xp = r/20;
			break;
		/* Just gold */
		case BUILD_COURIER:
		default:
			get_gold = TRUE;
			gp = r;
			break;
	}

	if (get_gold)
	{
		/* Give some gold */
		p_ptr->au += gp;

		/* Tell the player */
		msgf ("You receive %d gold for your services.", gp);
	}

	if (get_xp)
	{
		/* Gain some experience */
		gain_exp(xp);

		/* Tell the player */
		msgf ("You receive experience!");
	}

	if (stat != -1 && q_ptr->level >= 5)
	{
		int amt = 5 + (q_ptr->level / 10);

		int cap = stat_cap(stat);

		/* Don't get too close to maxing out */
		amt = MIN(amt, MAX((cap - p_ptr->stat[stat].cur)/2, 1));

		do_inc_stat_fixed(stat, amt);
	}

	if (get_item)
	{
		/* Give an OOD object */
		msgf ("A reward awaits you outside.");

		/* Prepare for object memory */
		current_object_source.type = OM_QUEST;
		current_object_source.place_num = p_ptr->place_num;
		current_object_source.depth = 0;
		current_object_source.data = q_ptr->type;

		/* Usually gives one object, but if r > 4000 it goes up */
		/* Must include q_ptr->level because we are currently in town. */
		/* Max out at 5 items */
		semi_acquirement(MIN(MAX(1, (r/2000)-3),5),
						q_ptr->level + MIN(15, (r/150)),
						&theme);
	}
}

/*
 * "Cash in" a quest, or be reminded that it's not done.
 */
void reward_quest(quest_type *q_ptr)
{
	bool completed = FALSE;
	bool reward = FALSE;
	store_type *st_ptr;

	if (q_ptr->status == QUEST_STATUS_FAILED)
	{
		msgf ("Thanks anyway.");

		/* Allow the quest to be deleted */
		q_ptr->status = QUEST_STATUS_FINISHED_FAILED;

		/* Take note */
		if (auto_notes)
		{
			add_note('Q', "Failed quest: %s", q_ptr->name);
		}
	}

	else
	{
		switch (q_ptr->type)
		{
			case QUEST_TYPE_FIND_ITEM:
			{
				if (q_ptr->status == QUEST_STATUS_COMPLETED)
				{
					msgf("You can keep it if you like.");

					/* Allow this quest to be deleted if needed */
					q_ptr->status = QUEST_STATUS_FINISHED;

					/* Take note */
					if (auto_notes)
					{
						add_note('Q', "Finished quest: %s", q_ptr->name);
					}
					completed=TRUE;
				}
				else
				{
					msgf("%s", q_ptr->name);
					msgf("Still looking?");
				}
				break;
			}
			case QUEST_TYPE_BOUNTY:
			case QUEST_TYPE_DEFEND:
			{
				if (q_ptr->status == QUEST_STATUS_COMPLETED)
				{
					/* Allow this quest to be deleted if needed */
					q_ptr->status = QUEST_STATUS_FINISHED;

					/* Take note */
					if (auto_notes)
					{
						add_note('Q', "Finished quest: %s", q_ptr->name);
					}
					completed=TRUE;
					reward = TRUE;
				}
				else  /* Still looking */
				{
					/* Remind what the quest is */
					msgf("%s", q_ptr->name);

					/* If you have killed all but one monster */
					if (q_ptr->data.bnt.max_num - q_ptr->data.bnt.cur_num == 1)
					{
						monster_race *r_ptr = &r_info[q_ptr->data.bnt.r_idx];

						if (FLAG(r_ptr, RF_MALE))
						{
							/* Male reference */
							msgf("Still looking for him?");
						}
						else if (FLAG(r_ptr, RF_FEMALE))
						{
							/* Female reference */
							msgf("Still looking for her?");
						}
						else
						{
							/* Neuter reference */
							msgf("Still looking for it?");
						}
					}
					else
					{
						/* More than one monster */
						msgf("Still looking for them?");
					}
				}
				break;
			}
			case QUEST_TYPE_MESSAGE:
			{
				if (q_ptr->status == QUEST_STATUS_COMPLETED)
				{
					/* Allow this quest to be deleted if needed */
					q_ptr->status = QUEST_STATUS_FINISHED;

					/* Take note */
					if (auto_notes)
					{
						add_note('Q', "Finished quest: %s", q_ptr->name);
					}
					completed=TRUE;
					reward=TRUE;
				}
				else
				{
					/* Tell the player what he was trying */
					msgf("%s", q_ptr->name);
					msgf("Please deliver the message as soon as possible!");
				}
				break;
			}
			case QUEST_TYPE_FIND_PLACE:
			{
				if (q_ptr->status == QUEST_STATUS_COMPLETED)
				{
					/* Break the link between place and quest */
					place[q_ptr->data.fpl.place].quest_num = z_info->q_max;

					/* Allow this quest to be deleted if needed */
					q_ptr->status = QUEST_STATUS_FINISHED;

					/* Take note */
					if (auto_notes)
					{
						add_note('Q', "Finished quest: %s", q_ptr->name);
					}
					completed=TRUE;
					reward=TRUE;
				}
				else
				{
					/* Remind the player what he was doing */
					msgf("%s", q_ptr->name);
					msgf("Please find the ruin as soon as possible!");
				}
				break;
			}
			default:
			{
				if (q_ptr->status == QUEST_STATUS_COMPLETED)
				{
					q_ptr->status = QUEST_STATUS_FINISHED;

					if (auto_notes)
					{
						add_note('Q', "Finished quest: %s", q_ptr->name);
					}
					completed = TRUE;
					reward = TRUE;
				}
				else
				{
					msgf ("%s", q_ptr->name);
				}
				break;
			}
		}
	}

	message_flush();

	st_ptr = get_current_store();

	/* Check for anything else special we need to do */
	if (completed)
	{
		/* Set the next quest level. */
		st_ptr->data = 0;
		build_set_qlevel();

		/* If this was a Thieves' Guild, give "low" membership. */
		if (st_ptr->type == BUILD_THIEVES_GUILD && st_ptr->max_cost == GS_NONMEMBER)
		{
			msgf ("Welcome to the guild.");
			st_ptr->max_cost = GS_LOW_MEMBER;
			message_flush();
			/* Membership was your reward.  (Hack) */
			reward = FALSE;
		}
	}

	if (reward)
	{
		give_reward(st_ptr, q_ptr);
	}

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

		/* No farms */
		if (pl_ptr->type == PL_FARM) continue;

		/* Get difference of distance in wilderness blocks and difficulty level */
		score = abs(distance(pl_ptr->x, pl_ptr->y, p_ptr->px / 16, p_ptr->py / 16) - *dist);

		/* The bigger the difference, the less likely a high score is */
		score = randint1(MAX(WILD_SIZE - score, 0));

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

static quest_type *insert_message_quest(int dist)
{
	place_type *pl_ptr;

	quest_type *q_ptr;
	store_type *st_ptr;

	int store;
	u16b place_num;

	int q_num;

	/* Get a new quest */
	q_num = q_pop();

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
	do
	{
		store = randint0(pl_ptr->numstores);

		st_ptr = &pl_ptr->store[store];

		/* Want a store with an owner */
	}
	while (st_ptr->type == BUILD_NONE ||
		   st_ptr->type == BUILD_STAIRS ||
		   st_ptr->type == BUILD_BLANK ||
		   st_ptr->type == BUILD_STORE_HOME);

	/* XXX XXX Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Carry a message to %s in %s.",
					quark_str(st_ptr->owner_name),
					pl_ptr->name);


	/* Save the quest data */
	q_ptr->data.msg.shop = store;
	q_ptr->data.msg.place = place_num;

	/* Set the reward level */
	q_ptr->reward = dist * 100;

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
	 * roughly 20 to 70 wilderness squares away.
	 */
	q_ptr = insert_message_quest(curr_scale);

	if (!q_ptr)
	{
		msgf("Sorry, I don't need any to send any messages today.");

		message_flush();

		/* No available quests, unfortunately. */
		return (FALSE);
	}

	/* Show it on the screen? */

	/* Display a helpful message. */
	msgf("%s", q_ptr->name);

	message_flush();

	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);

	/* Exit */
	return (TRUE);
}


static quest_type *insert_defender_quest(u16b r_idx, u16b num)
{
	quest_type *q_ptr;

	int q_num;

	monster_race *r_ptr = &r_info[r_idx];

	char buf2[80];

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (NULL);

	q_ptr = &quest[q_num];

	/* Bounty quest */
	q_ptr->type = QUEST_TYPE_DEFEND;

	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	/* 1 hour */
	q_ptr->timeout = turn+TOWN_HOUR;

	desc_time(q_ptr->timeout, buf2);

	if (num != 1)
	{
		char buf[80];
		strcpy(buf, mon_race_name(r_ptr));
		plural_aux(buf);

		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %d %s by %s", num, buf, buf2);
	}
	else
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %s by %s", mon_race_name(r_ptr), buf2);
	}

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

	/* Save the quest data */
	q_ptr->data.bnt.r_idx = r_idx;
	q_ptr->data.bnt.cur_num = 0;
	q_ptr->data.bnt.max_num = num;
	q_ptr->reward = r_ptr->level * r_ptr->level * num * 2;


	/* bonus reward for uniques */
	if (num == 1) q_ptr->reward *= 10;

	/* Set Questor flag */
	SET_FLAG(&r_info[r_idx], RF_QUESTOR);

	/* Activate right away */
	q_ptr->flags |= QUEST_FLAG_ACTIVE;

	/* Summon the monsters */
	display_monster_quest(q_ptr);

	/* Done */
	return (q_ptr);
}


static bool request_defender(int dummy)
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
		 * Random monster of specified depth.
		 */
		r_idx = get_filter_mon_num(curr_scale, &monster_quest);

		r_ptr = &r_info[r_idx];

		/* Require the monster to match the guild's theme */
		if (!monster_quest_guild_theme(r_ptr)) continue;


		/* Save the index if the monster is deeper than current monster */
		if (!best_r_idx || (r_info[r_idx].level > best_level))
		{
			best_r_idx = r_idx;
			best_level = r_info[r_idx].level;
		}

		/* Accept monsters that are a few levels out of depth */
		if (best_level > curr_scale + 5) break;
	}

	if (best_r_idx == 1 && curr_scale >= 100)
	{
		msgf("Sorry, I don't need defending right now.");
		message_flush();
		return(FALSE);
	} else if (best_r_idx == 1) {
		/* We must be trying a scale that's too low.  Try again at a harder scale. */
		curr_scale += 5;
		return(request_defender(dummy));
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
		num = (5 + randint0(p_ptr->max_lev / 2)) / r_ptr->rarity;
	}

	/* Generate the quest */
	q_ptr = insert_defender_quest(best_r_idx, num);

	if (!q_ptr)
	{
		msgf("Sorry, I don't need defending right now.");

		message_flush();

		/* No available quests, unfortunately. */
		return (FALSE);
	}

	/* Show it on the screen? */


	/* Display a helpful message. */
	msgf("%s", q_ptr->name);

	message_flush();

	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);

	/* Exit */
	return (TRUE);
}



void request_quest(const store_type *b_ptr, int scale)
{
	quest_type *q_ptr;
	int q_num;

	/* Save building so we can remember the quest giver */
	curr_build = b_ptr;

	/* Save scale so we can work out how hard to make the quest */
	curr_scale = scale;

	/* For buildings with only one quest type, just request the quest. */
	switch(b_ptr->type)
	{
		case BUILD_COURIER:
			/* Hack: quest difficulty should be random */
			curr_scale = rand_range(20,70);
			request_message(0);
			return;
		case BUILD_FARM:
			/* Hack: quests should never be that hard */
			curr_scale = MIN(curr_scale, 30);
			request_defender(0);
			return;
	}

	/* What's the next quest? */
	q_num = lookup_quest_building_next(b_ptr);

	if (q_num == -1)
	{
		/* No quests available */
		msgf ("Sorry, we don't have any errands for you.");
		return;
	}
	/* Available quest, but player level not sufficient */
	else
	{
		q_ptr = &quest[q_num];
		if (p_ptr->max_lev * 3 < q_ptr->level - (q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT ? 5 : 12))
		{
			msgf ("Come back when you are more powerful.");
			return;
		}

		/* Explain the quest */
		msgf ("%s", q_ptr->name);

		/* Give the player q_ptr as a quest. */
		q_ptr->flags |= QUEST_FLAG_KNOWN;
		q_ptr->status = QUEST_STATUS_TAKEN;

		/* If necessary, link the quest place to the wilderness */
		if (q_ptr->type == QUEST_TYPE_FIXED_DEN ||
			q_ptr->type == QUEST_TYPE_FIXED_KILL ||
			q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT ||
			q_ptr->type == QUEST_TYPE_FIXED_BOSS)
		{
			int i;
			place_type *pl_ptr = NULL;

			/* Find the place */
			for (i = 0; i < place_count; i++)
			{
				pl_ptr = &place[i];

				if (pl_ptr->quest_num == q_num)
					break;
			}

			/* Paranoia */
			if (i == place_count)
			{
				msgf ("Warning: Couldn't find quest place to activate it.");
			}

			/* Redraw, there should be stairs now */
			refresh_quest_stair(pl_ptr);
		}
	}
}


/* Show the quest status as a string */
static cptr quest_status_string(quest_type *q_ptr)
{
	int monst_num = 0;
	int max_num = 0;

	/* Just checking */
	if (!q_ptr) return (NULL);

	/* Surely you jest? */
	if (q_ptr->type == QUEST_TYPE_NONE) return (NULL);

	/* Check the various statuses */
	switch (q_ptr->status)
	{
		/* Unknown quest */
		case QUEST_STATUS_UNTAKEN: return (NULL);

		/* Underway */
		case QUEST_STATUS_TAKEN:
		{
			max_num = -1;

			/* For multi-level fixed quests, say how many levels are cleared. */
			if (q_ptr->type == QUEST_TYPE_FIXED_DEN || q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT)
			{
				int lvls = (q_ptr->type == QUEST_TYPE_FIXED_DEN ? q_ptr->data.fix.data.den.levels :
																  q_ptr->data.fix.data.clearout.levels);
				int cleared = (q_ptr->type == QUEST_TYPE_FIXED_DEN ? q_ptr->data.fix.data.den.cleared :
																	 q_ptr->data.fix.data.clearout.cleared);

				if (lvls == 1 || cleared == 0) return ("");

				else return (format("You have cleared %d level%s.", cleared, (cleared == 1 ? "" : "s")));
			}

			/* Count the bounty monsters */
			if (q_ptr->type == QUEST_TYPE_BOUNTY || q_ptr->type == QUEST_TYPE_DEFEND)
			{
				monst_num = q_ptr->data.bnt.cur_num;
				max_num = q_ptr->data.bnt.max_num;
			}

			/* Count the dungeon monsters */
			if (q_ptr->type == QUEST_TYPE_DUNGEON)
			{
				monst_num = q_ptr->data.dun.cur_num;
				max_num = q_ptr->data.dun.max_num;
			}

			/* Don't show the count for zero or one monsters */
			if (max_num == 0) return ("");

			/* Tell the world */
			else if (max_num > 1) return (format("You have killed %d.", monst_num));

			else return ("");
		}

		/* Report back to quest_giver */
		case QUEST_STATUS_COMPLETED:
		{
			/* All done killing */
			if (q_ptr->type == QUEST_TYPE_BOUNTY) return ("(Killed)");
			if (q_ptr->type == QUEST_TYPE_DEFEND) return ("(Killed)");

			/* All done killing */
			if (q_ptr->type == QUEST_TYPE_DUNGEON) return ("(Killed)");
			if (q_ptr->type == QUEST_TYPE_FIXED_KILL) return ("(Killed)");

			/* All done defeating */
			if (q_ptr->type == QUEST_TYPE_WILD) return ("(Defeated)");
			if (q_ptr->type == QUEST_TYPE_FIXED_BOSS) return ("(Defeated)");

			/* All done delivering */
			if (q_ptr->type == QUEST_TYPE_MESSAGE) return ("(Delivered)");

			/* All done finding */
			if (q_ptr->type == QUEST_TYPE_FIND_ITEM) return ("(Found)");

			/* All done finding */
			if (q_ptr->type == QUEST_TYPE_FIND_PLACE) return ("(Found)");

			/* Repaid */
			if (q_ptr->type == QUEST_TYPE_LOAN) return ("(Paid)");

			/* Cleared out */
			if (q_ptr->type == QUEST_TYPE_FIXED_DEN) return ("(Cleared)");
			if (q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT) return ("(Cleared)");
		}

		/* Finnished */
		case QUEST_STATUS_FINISHED: return ("(Finished)");

		case QUEST_STATUS_FAILED: return ("(Failed)");

		case QUEST_STATUS_FINISHED_FAILED: return ("(Finished, Failed)");

		default: return ("(BUG!)\n");
	}
}


/*
 * Print quest status of all active quests
 */
bool do_cmd_knowledge_quests_aux(int place_num, FILE *fff)
{
	char tmp_str[256];

	quest_type *q_ptr;
	int i, j, k, sz, w, pass, lvl, cnt;
	char * s;

	(void)Term_get_size(&w, &j);
	sz = w;

	/* Arrange quests by general status */
	for (pass = 0; pass < 3; pass++)
	{
		cnt = 0;

		/* Sort quests by level */
		for (lvl = 0; lvl <= 100; lvl++)
		{

			for (i = 0; i < q_max; i++)
			{
				q_ptr = &quest[i];

				/* Appropriate level only */
				if (q_ptr->level != lvl) continue;

				/* Quests that are untaken never show up. */
				if (q_ptr->status == QUEST_STATUS_UNTAKEN) continue;

				/* Do we know about it? */
				if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;

				/* Paranoia */
				if (q_ptr->type == QUEST_TYPE_NONE) continue;

				/* First time through, only current quests */
				if (pass == 0 && q_ptr->status != QUEST_STATUS_TAKEN) continue;

				/* Second time through, only completed quests */
				if (pass == 1 && (q_ptr->status == QUEST_STATUS_TAKEN ||
								  q_ptr->status == QUEST_STATUS_FINISHED ||
								  q_ptr->status == QUEST_STATUS_FINISHED_FAILED)) continue;

				/* Third time through, only finished quests */
				if (pass == 2 && (q_ptr->status != QUEST_STATUS_FINISHED &&
								  q_ptr->status != QUEST_STATUS_FINISHED_FAILED)) continue;

				/* If we are restricted to one place, restrict to one place. */
				if (place_num != 0 && q_ptr->place != place_num) continue;

				if (!cnt)
				{
					cnt++;
					switch (pass)
					{
						case 0:
							froff (fff, "====Active quests==== \n\n");
							break;
						case 1:
							froff (fff, "====Completed/failed quests==== \n\n");
							break;
						case 2:
							froff (fff, "====Finished quests==== \n\n");
							break;
					}
				}

				if (q_ptr->type != QUEST_TYPE_WILD && pass != 2)
				{
					place_type *pl_ptr = &place[q_ptr->place];

					if (pl_ptr->type != PL_FARM && q_ptr->place)
					{
						if (place_num == 0)
						{
							froff(fff, "From a %s in %s (Danger level %i):\n   ",
									building_name(pl_ptr->store[q_ptr->shop].type),
									pl_ptr->name, q_ptr->level);
						}
						else
						{
							froff(fff, "From the %s (Danger level %i):\n   ",
									building_name(pl_ptr->store[q_ptr->shop].type), q_ptr->level);
						}
						sz -= 3;
					}
				}
				else if (pass == 2 && q_ptr->level)
				{
					froff (fff, "(Danger level %i):  ", q_ptr->level);
					sz -= 15;
				}

				/* See what type of quest it is */
				switch (q_ptr->type)
				{
					case QUEST_TYPE_DUNGEON:
					{
						/* In feet, or in levels */
						if (depth_in_feet)
						{
							strnfmt(tmp_str, 256, "%s (%4dft) %s",
									q_ptr->name, (int)q_ptr->data.dun.level * 50, quest_status_string(q_ptr));
						}
						else
						{
							strnfmt(tmp_str, 256, "%s %s",
									q_ptr->name, quest_status_string(q_ptr));
						}

						break;
					}
					case QUEST_TYPE_BOUNTY:
					case QUEST_TYPE_MESSAGE:
					case QUEST_TYPE_FIND_ITEM:
					case QUEST_TYPE_FIND_PLACE:
					case QUEST_TYPE_WILD:
					case QUEST_TYPE_DEFEND:
					case QUEST_TYPE_FIXED_DEN:
					case QUEST_TYPE_FIXED_CLEAROUT:
					case QUEST_TYPE_FIXED_KILL:
					case QUEST_TYPE_FIXED_BOSS:
					{
						/* Hack - this is simple */
						strnfmt(tmp_str, 256, "%s  %s",
							q_ptr->name, quest_status_string(q_ptr));

						break;
					}
					case QUEST_TYPE_LOAN:
						if (q_ptr->status != QUEST_STATUS_FINISHED)
								strnfmt(tmp_str, 256, "%s  %s",
							q_ptr->name, quest_status_string(q_ptr));
						break;
					default:
					{
						/* Paranoia */
						strnfmt(tmp_str, 256, "Invalid quest type!");
					}
				}

				if (q_ptr == current_quest)
				{
					strnfmt(tmp_str, 256, "%s (Current)", tmp_str);
				}
				else
					strnfmt(tmp_str, 256, "%s", tmp_str);



				/* Copy to the file */
				s = tmp_str;
				j = strlen(tmp_str);

				/* Break up tmp_str so that it displays nicely */
				do
				{
					if (j <= sz)
					{
						froff(fff, "%s", s);
						break;
					}
					for (k = sz; k >= sz-14; k--)
					{
						if (s[k] == ' ')
						{
							s[k] = 0;
							froff(fff, "%s\n", s);
							s[k] = ' ';
							s = &s[k+1];
							j -= k+1;
							break;
						}
					}

					if (k == sz-15)
					{
						char c_tmp = s[sz];
						s[sz] = 0;
						froff(fff, "%s\n", s);
						s[sz] = c_tmp;
						s = &s[sz+1];
						j -= sz+1;
					}

					sz = w;
				} while (TRUE);

				/* Line breaks */
				froff(fff, "%s", pass == 3 ? "\n" : "\n\n");
			}
		}
	}

	return (FALSE);
}

bool do_cmd_knowledge_quests(int dummy)
{
	char file_name[1024];
	FILE *fff;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, 1024);

	/* Failure */
	if (!fff) return (FALSE);

	/* Hack: Ignore parameter */
	(void)dummy;

	do_cmd_knowledge_quests_aux(0, fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Quest status", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);

	return (FALSE);
}



/* Dump the quests related to this town into fff, only when display is set */
void dump_castle_info(FILE *fff, int town)
{
	int i;
	bool quest_in_town = FALSE;

	quest_type *q_ptr;

	/* Loop through the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Find a quest */
		q_ptr = &quest[i];

		/* Is it from this town? */
		if (town != q_ptr->place) continue;

		/* It is ready to be seen? */
		if (q_ptr->status < QUEST_STATUS_TAKEN) continue;
		if (!(q_ptr->flags & QUEST_FLAG_KNOWN)) continue;

		/* There is a quest */
		quest_in_town = TRUE;
	}

	/* If no quest was issued  */
	if (!quest_in_town)
	{
		/* Say so */
		froff(fff, "No quest was issued in this town.\n");
	}
	else
	{
		/* Print out quest information */
		do_cmd_knowledge_quests_aux(town, fff);
	}
}

/*
 * Returns "true" if the monster listed matches this rule.
 * Ignores inclusion/exclusion and negation.
 */
static bool hook_rule_tester(int r_idx, hook_rule_type *rule)
{
	monster_race *r_ptr = &r_info[r_idx];
	int j, m;

	/* Paranoia */
	if (!rule) return (TRUE);

	switch (rule->type)
	{
		/* Shouldn't happen.  Default to FALSE, since an
		   empty rule should be an "exclude". */
		case HRT_NONE:
			return (FALSE);

		case HRT_GRAPHIC:
			/* See if the monster's default character is in the
			   data string. */
			m = strlen(rule->data);
			for (j = 0; j < m; j++)
			{
				if (r_ptr->d_char == rule->data[j])
				{
					return (TRUE);
				}
			}
			break;
		case HRT_FLAG:
		{
			/* Use the data string to put together a list of flag
			   values.  See if any match any of the monster's flags */
			u32b f = 0;
			u32b v = 0;

			/* Extract the flags from the string */
			for (j = 0; j < 9; j++)
			{
				f = (u32b)rule->data[(4*j)];
				f += ((u32b)(1 << 8) * rule->data[(4*j)+1]);
				f += ((u32b)(1 << 16) * rule->data[(4*j)+2]);
				f += ((u32b)(1 << 24) * rule->data[(4*j)+3]);

				/* Accumulate from this batch of flags */
				v |= (f & r_ptr->flags[j]);
			}

			/* Did we match any? */
			if (v)  return (TRUE);
			break;
		}
		case HRT_NAME:
		{
			/* Parse the data string into strings separated by colons. */
			cptr s;
			char buf[128];
			char buf2[128];

			strncpy(buf, rule->data, 128);
			strncpy(buf2, mon_race_name(r_ptr), 128);

			m = strlen(buf2);
			/* De-capitalize monster name */

			for (j = 0; j < m; j++)
			{
				buf2[j] = (char)tolower(buf2[j]);
			}

			m = strlen(buf);
			/* De-capitalize the data string, so we are case-insensitive */
			for (j = 0; j < m; j++)
			{
				buf[j] = (char)tolower(buf[j]);
			}

			/* Try each string */
			s = &buf[0];

			for (j = 0; j < m; j++)
			{
				if (buf[j] == ':')
				{
					buf[j] = 0;

					if (strstr(buf2, s))
						return (TRUE);
					s = &buf[j+1];
				}
			}
			if (strstr(buf2, s))
				return (TRUE);
			else
				return (FALSE);
			break;
		}

		case HRT_SPELL_FREQ:
		{
			/* Compare monster's spellcasting frequency to number in data */
			int f = atoi(rule->data);

			if (!f)
			{
				msgf ("Cannot test spell frequency 1 in 0");
				break;
			}

			if (r_ptr->freq_spell >= (100 / f)) return (TRUE);
			break;
		}
		case HRT_RIDX:
		{
			/* Compare monster's index to list of colon-separated numbers */
			int idx = 0;
			cptr s = rule->data;

			m = strlen(rule->data);

			for (j = 0; j < m; j++)
			{
				if (rule->data[j] != ':') continue;

				rule->data[j] = 0;

				if (!sscanf(s, "%i", &idx)) break;

				rule->data[j] = ':';

				if (idx == r_idx) return (TRUE);

				s = &(rule->data[j+1]);
			}

			if (!sscanf(s, "%i", &idx)) break;

			if (idx == r_idx) return (TRUE);

			break;
		}
		case HRT_BLOW:
		{
			/* Compare each of monster's blows to list of methods / effects */
			int k;

			/* Loop two characters at a time */
			for (j = 0;  j < 128 && rule->data[j]; j+= 2)
			{
				/* Method */
				if (rule->data[j] == 'M')
				{
					for (k = 0; k < 4; k++)
					{
						if (r_ptr->blow[k].method ==
								(byte)rule->data[j+1])
						{
							return(TRUE);
						}
					}
				}
				/* Effect */
				else if (rule->data[j] == 'E')
				{
					for (k = 0; k < 4; k++)
					{
						if (r_ptr->blow[k].effect ==
								(byte)rule->data[j+1])
						{
							return(TRUE);
						}
					}
				}
				else
				{
					msgf ("Error in rule internals: expected M or E");
				}
			}
			break;
		}
		case HRT_HIT_DICE:
		{
			/* Compare monster's hit dice to number in data */
			int f = atoi(rule->data);

			if (r_ptr->hdice >= f) return (TRUE);
			break;
		}
		case HRT_SPEED:
		{
			/* Compare monster's speed to number in data */
			int f = atoi(rule->data);

			if (r_ptr->speed >= f) return (TRUE);
			break;
		}
		case HRT_DEPTH:
		{
			/* Compare monster's level to number in data */
			int f = atoi(rule->data);

			if (r_ptr->level >= f) return (TRUE);
			break;
		}
		case HRT_RARITY:
		{
			/* Compare monster's rarity to number in data */
			int f = atoi(rule->data);

			if (r_ptr->rarity >= f) return (TRUE);
			break;
		}
		case HRT_ARMOR:
		{
			/* Compare monster's ac to number in data */
			int f = atoi(rule->data);

			if (r_ptr->ac >= f) return (TRUE);
			break;
		}
		default:
		{
			msgf ("Unknown hook type");
			break;
		}
	}

	/* Must not have passed */
	return (FALSE);
}

bool monster_group_test(int r_idx, monster_group_type * mg_ptr)
{
	bool ok, test;
	int i;
	hook_rule_type * rule;

	/* Paranoia */
	if (!mg_ptr) return (TRUE);

	rule = &mg_ptr->rule[0];

	/* Paranoia */
	if (rule->type == HRT_NONE) return (TRUE);

	/* Start out with ok = FALSE, unless 1st rule is an exclude */
	ok = (rule->include ? FALSE : TRUE);

	for (i = 0; i < 6; i++)
	{
		rule = &mg_ptr->rule[i];

		if (rule->type == HRT_NONE) break;

		test = hook_rule_tester(r_idx, rule);

		/* Invert the test result if we're supposed to. */
		if (rule->neg)
			test = (test ? FALSE : TRUE);

		/* If result is TRUE, obey the include instruction */
		if (test && rule->include)
			ok = TRUE;

		/* Or the exclude */
		if (test && !rule->include)
			ok = FALSE;

	}

	return (ok);
}

/*
 * The general "quest theme hook"
 *   Returns TRUE if the given r_idx is a monster we can generate
 *   in the current quest.
 */
bool quest_theme_hook(int r_idx)
{
	monster_race * r_ptr = &r_info[r_idx];

	/* Disallow questors */
	if (FLAG(r_ptr, RF_QUESTOR)) return (FALSE);

	/* Hack: disallow townsfolk */
	if (!(r_ptr->flags[7] & RF7_DUN)) return (FALSE);

	/* Hack: disallow friendly monsters */
	if (FLAG(r_ptr, RF_FRIENDLY)) return (FALSE);

	if (!current_quest) return (TRUE);

	if (current_quest->type == QUEST_TYPE_FIXED_DEN)
	{
		if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7)) return (FALSE);
		return (monster_group_test(r_idx, &mg_info[current_quest->data.fix.data.den.mg_idx]));
	}

	if (current_quest->type == QUEST_TYPE_FIXED_CLEAROUT)
		return (TRUE);

	if (current_quest->type == QUEST_TYPE_FIXED_KILL)
	{
		/* For this type, we will just wipe the monsters later. */
		return (TRUE);
	}

	/* XXX WRITE THIS */
	/* Currently, just works like "summon kin" */
	if (current_quest->type == QUEST_TYPE_FIXED_BOSS)
	{
		if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7)) return (FALSE);

		/* Never summon the boss.  We will deliberately place him. */
		if (r_idx == current_quest->data.fix.data.kill.r_idx && !hero_okay(r_idx))
			return (FALSE);
		else if (r_info[r_idx].d_char == r_info[current_quest->data.fix.data.boss.r_idx].d_char &&
				 r_info[r_idx].level <= r_info[current_quest->data.fix.data.boss.r_idx].level)
			return (TRUE);
		else
			return (FALSE);
	}

	return (TRUE);
}


static quest_type * find_loan (void)
{
	int i;
	quest_type * q_ptr;

	for (i = 0; i < q_max; i++) {
		q_ptr = &quest[i];

		if (q_ptr->type != QUEST_TYPE_LOAN) continue;
		if (q_ptr->status != QUEST_STATUS_TAKEN) continue;
		return (q_ptr);
	}

	/* Didn't have one */
	return (NULL);
}


/*
 * Determine if the player has a loan, and if so, which bank is the lender.
 */
store_type * get_loaner(void)
{
	quest_type * q_ptr = find_loan();

	if (q_ptr == NULL) return NULL;

	return (&(place[q_ptr->data.msg.place].store[q_ptr->data.msg.shop]));
}

/*
 * Find the current amount of the player's loan.
 */
s32b get_loan_amount(void)
{
	quest_type * q_ptr = find_loan();

	if (q_ptr == NULL) return 0;

	return (q_ptr->timeout);
}

/*
 * Set up a new loan quest
 */
quest_type * insert_loan(s32b amt)
{
	place_type *pl_ptr = &place[p_ptr->place_num];

	quest_type *q_ptr;
	store_type *st_ptr = get_current_store();
	int store, i;
	int q_num;

	/* Determine which store this is */
	store = -1;

	for (i = 0; i < pl_ptr->numstores; i++) {
		if (st_ptr == &pl_ptr->store[i])
		{
			store = i;
			break;
		}
	}

	/* Paranoia */
	if (store == -1) return (NULL);

	/* Get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (NULL);

	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_LOAN;

	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	/* We don't need any special creation operation */
	q_ptr->c_type = QC_NONE;

	/* Finished when the player pays it off */
	q_ptr->x_type = QX_LOAN;

	/* XXX XXX Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Repay a loan of %i gold to the bank in %s.",
					amt, pl_ptr->name);


	/* Save the quest data */
	q_ptr->data.msg.shop = store;
	q_ptr->data.msg.place = p_ptr->place_num;

	/* Set the reward level, which stores the amount of the loan */
	q_ptr->timeout = amt;

	q_ptr->flags = QUEST_FLAG_ACTIVE;

	/* Done */
	return (q_ptr);

}

/*
 * Returns TRUE if there should be stairs available.
 */
bool quest_stairs_active(int p_num)
{
	place_type *pl_ptr = &place[p_num];
	quest_type *q_ptr = &quest[pl_ptr->quest_num];

	if (q_ptr->type != QUEST_TYPE_FIXED_DEN &&
		q_ptr->type != QUEST_TYPE_FIXED_BOSS &&
		q_ptr->type != QUEST_TYPE_FIXED_CLEAROUT &&
		q_ptr->type != QUEST_TYPE_FIXED_KILL)
			return (FALSE);

	if (q_ptr->status != QUEST_STATUS_TAKEN)
		return (FALSE);

	if (q_ptr->data.fix.attempts == 0)
		return (FALSE);

	return (TRUE);
}

