/* File: cmd2.c */

/* Purpose: Movement commands (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	bool go_up = FALSE;
	cave_type *c_ptr;

	/* Player grid */
	c_ptr = area(py,px);
#if 0
	/* Quest up stairs */
	if (c_ptr->feat == FEAT_QUEST_UP)
	{
		/* Success */
		msg_print("You enter the up staircase.");

		leaving_quest = p_ptr->inside_quest;
		p_ptr->inside_quest = c_ptr->special;

		/* Leaving an 'only once' quest marks it as failed */
		if (leaving_quest &&
			(quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
			(quest[leaving_quest].status == QUEST_STATUS_TAKEN))
		{
			quest[leaving_quest].status = QUEST_STATUS_FAILED;
		}

#ifdef USE_SCRIPT
		if (cmd_go_up_callback()) return;
#endif /* USE_SCRIPT */

		/* Activate the quest */
		if (!quest[p_ptr->inside_quest].status)
		{
			quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
		}

		/* Leaving a quest */
		if (!p_ptr->inside_quest)
		{
			dun_level = 0;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;
		p_ptr->leftbldg = TRUE;

		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;
	}

	/* Normal up stairs */
	else if (c_ptr->feat == FEAT_LESS)
#else /* 0 */
	if (c_ptr->feat == FEAT_LESS)

#endif /* 0 */
	{
		if (!dun_level)
		{
			go_up = TRUE;
		}
		else
		{
			if (confirm_stairs)
			{
				if (get_check("Really leave the level? "))
					go_up = TRUE;
			}
			else
			{
				go_up = TRUE;
			}
		}

		if (go_up)
		{
	/*
	 * I'm experimenting without this... otherwise the monsters get to
	 * act first when we go up stairs, theoretically resulting in a possible
	 * insta-death.
	 */
			energy_use = 0;

			/* Success */
			msg_print("You enter a maze of up staircases.");

			if (autosave_l) do_cmd_save_game(TRUE);

#ifdef USE_SCRIPT
			if (cmd_go_up_callback()) return;
#endif /* USE_SCRIPT */

#if 0

			if (p_ptr->inside_quest)
			{
				dun_level = 1;
				leaving_quest = p_ptr->inside_quest;

				/* Leaving an 'only once' quest marks it as failed */
				if (leaving_quest &&
					(quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
					(quest[leaving_quest].status == QUEST_STATUS_TAKEN))
				{
					quest[leaving_quest].status = QUEST_STATUS_FAILED;
				}

				p_ptr->inside_quest = c_ptr->special;
			}
#endif /* 0 */

			/* Create a way back */
			create_down_stair = TRUE;

			/* New depth */
			dun_level--;

			/* Leaving the dungeon to town */
			if (!dun_level && p_ptr->town_num && !leaving_quest)
				p_ptr->leaving_dungeon = TRUE;

			/* Leaving */
			p_ptr->leaving = TRUE;
		}
	}
	else
	{
		msg_print("I see no up staircase here.");
		return;
	}
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	cave_type *c_ptr;
	bool go_down = FALSE;

	/* Player grid */
	c_ptr = area(py,px);

#if 0

	/* Quest down stairs */
	if (c_ptr->feat == FEAT_QUEST_DOWN)
	{
		msg_print("You enter the down staircase.");

		leaving_quest = p_ptr->inside_quest;

		/* Leaving an 'only once' quest marks it as failed */
		if (leaving_quest &&
			(quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
			(quest[leaving_quest].status == QUEST_STATUS_TAKEN))
		{
			quest[leaving_quest].status = QUEST_STATUS_FAILED;
		}

		p_ptr->inside_quest = c_ptr->special;

		/* Activate the quest */
		if (!quest[p_ptr->inside_quest].status)
		{
			quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
		}

		/* Leaving a quest */
		if (!p_ptr->inside_quest)
		{
			dun_level = 0;
		}

		/* Leaving */
		p_ptr->leaving = TRUE;
		p_ptr->leftbldg = TRUE;

		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;
	}
	/* Verify stairs */
	else if (c_ptr->feat != FEAT_MORE)
#else /* 0 */
	if (c_ptr->feat != FEAT_MORE)
#endif /* 0 */

	{
		msg_print("I see no down staircase here.");
		return;
	}
	else
	{
		if (!dun_level)
		{
			go_down = TRUE;

			/* Save old player position */
			p_ptr->oldpx = px;
			p_ptr->oldpy = py;
		}
		else
		{
			if (confirm_stairs)
			{
				if (get_check("Really leave the level? "))
					go_down = TRUE;
			}
			else
			{
				go_down = TRUE;
			}
		}

		if (go_down)
		{
			energy_use = 0;

			/* Success */
			msg_print("You enter a maze of down staircases.");

			if (autosave_l) do_cmd_save_game(TRUE);

			/* Go down */
			dun_level++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			/* Create a way back */
			create_up_stair = TRUE;
		}
	}
}



/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Take a turn */
	energy_use = 100;

#ifdef USE_SCRIPT
	if (cmd_search_callback(py, px))
	{
		/* Disturb */
		disturb(0, 0);

		return;
	}
#endif /* USE_SCRIPT */

	/* Search */
	search();
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{
	/* Stop searching */
	if (p_ptr->searching)
	{
		/* Clear the searching flag */
		p_ptr->searching = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start searching */
	else
	{
		/* Set the searching flag */
		p_ptr->searching = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
	}
}



/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x)
{
	cave_type *c_ptr = area(y, x);

	s16b this_o_idx, next_o_idx;


	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip unknown chests XXX XXX */
		/* if (!o_ptr->marked) continue; */

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}


/*
 * Allocates objects upon opening a chest    -BEN-
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * Small chests often contain "gold", while Large chests always contain
 * items.  Wooden chests contain 2 items, Iron chests contain 4 items,
 * and Steel chests contain 6 items.  The "value" of the items in a
 * chest is based on the "power" of the chest, which is in turn based
 * on the level on which the chest is generated.
 */
static void chest_death(int y, int x, s16b o_idx)
{
	int number;

	bool small;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr = &o_list[o_idx];


	/* Small chests often hold "gold" */
	small = (o_ptr->sval < SV_CHEST_MIN_LARGE);

	/* Determine how much to drop (see above) */
	number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 10;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Small chests often drop gold */
		if (small && (rand_int(100) < 25))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make a good object */
			if (!make_object(q_ptr, TRUE, FALSE)) continue;
		}

#ifdef USE_SCRIPT
		q_ptr->python = object_create_callback(q_ptr);
#endif /* USE_SCRIPT */

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = base_level;

	/* No longer opening a chest */
	opening_chest = FALSE;

	/* Empty */
	o_ptr->pval = 0;

	/* Known */
	object_known(o_ptr);
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, s16b o_idx)
{
	int  i, trap;

	object_type *o_ptr = &o_list[o_idx];


	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msg_print("A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void)set_poisoned(p_ptr->poisoned + 10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");

		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + 10 + randint(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		msg_print("You are enveloped in a cloud of smoke!");

		for (i = 0; i < num; i++)
		{
			if (randint(100) < dun_level)
				(void)activate_hi_summon();
			else
				(void)summon_specific(0, y, x, dun_level, 0, TRUE, FALSE, FALSE);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		sound(SOUND_EXPLODE);
		take_hit(damroll(5, 8), "an exploding chest");
	}
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
	int i, j;

	bool flag = TRUE;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Take a turn */
	energy_use = 100;

	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success -- May still have traps */
		if (rand_int(100) < j)
		{
			msg_print("You have picked the lock.");
			gain_exp(1);
			flag = TRUE;
		}

		/* Failure -- Keep trying */
		else
		{
			/* We may continue repeating */
			more = TRUE;
			if (flush_failure) flush();
			msg_print("You failed to pick the lock.");
		}
	}

	/* Allowed to open */
	if (flag)
	{
		/* Apply chest traps, if any */
		chest_trap(y, x, o_idx);

		/* Let the Chest drop items */
		chest_death(y, x, o_idx);
	}

	/* Result */
	return (more);
}


/*
 * Return TRUE if the given feature is an open door
 */
static bool is_open(int feat)
{
	return (feat == FEAT_OPEN);
}

/*
 * Return TRUE if the given feature is a closed door
 */
static bool is_closed(int feat)
{
	return (feat == FEAT_CLOSED);
}

/*
 * Return the number of traps around (or under) the character.
 */
int count_traps(int *y, int *x, bool under)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* if not searching under player continue */
		if ((d == 8) && !under) continue;

		/* Extract adjacent (legal) location */
		yy = py + ddy_ddd[d];
		xx = px + ddx_ddd[d];

		/* paranoia */
		if (!in_bounds2(yy, xx)) continue;

		/* Not looking for this feature */
		if (!is_visible_trap(area(yy, xx))) continue;

		/* OK */
		++count;

		/* Remember the location. Only useful if only one match */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}

/*
 * Return the number of doors around (or under) the character.
 */
static int count_doors(int *y, int *x, bool (*test)(int feat), bool under)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* if not searching under player continue */
		if ((d == 8) && !under) continue;

		/* Extract adjacent (legal) location */
		yy = py + ddy_ddd[d];
		xx = px + ddx_ddd[d];

		/* paranoia */
		if (!in_bounds2(yy, xx)) continue;

		/* Must have knowledge */
		if (!(area(yy,xx)->info & (CAVE_MARK))) continue;

		/* Not looking for this feature */
		if (!((*test)(area(yy, xx)->feat))) continue;

		/* OK */
		++count;

		/* Remember the location. Only useful if only one match */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}




/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
static int count_chests(int *y, int *x, bool trapped)
{
	int d, count, o_idx;

	object_type *o_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = py + ddy_ddd[d];
		int xx = px + ddx_ddd[d];

		/* No (visible) chest is there */
		if ((o_idx = chest_check(yy, xx)) == 0) continue;

		/* Grab the object */
		o_ptr = &o_list[o_idx];

		/* Already open */
		if (o_ptr->pval == 0) continue;

		/* No (known) traps here */
		if (trapped && (!object_known_p(o_ptr) ||
			!chest_traps[o_ptr->pval])) continue;

		/* OK */
		++count;

		/* Remember the location. Only useful if only one match */
		*y = yy;
		*x = xx;
	}

	/* All done */
	return count;
}


/*
 * Convert an adjacent location to a direction.
 */
static int coords_to_dir(int y, int x)
{
	int d[3][3] = { {7, 4, 1}, {8, 5, 2}, {9, 6, 3} };
	int dy, dx;

	dy = y - py;
	dx = x - px;

	/* Paranoia */
	if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

	return d[dx + 1][dy + 1];
}

/*
 * Perform the basic "open" command on doors
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
bool do_cmd_open_aux(int y, int x)
{
	int i;

	cave_type *c_ptr;
	
	s16b *fld_ptr;

	/* Take a turn */
	energy_use = 100;

	/* Get requested grid */
	c_ptr = area(y,x);
	
	/* Must be a closed door */
	if (c_ptr->feat != FEAT_CLOSED)
	{
		/* Nope */
		return (FALSE);
	}
	
	/* Get fields */
	fld_ptr = field_is_type(&c_ptr->fld_idx, FTYPE_DOOR);
	
	/* If the door is locked / jammed */
	if (*fld_ptr)
	{
		/* Get the "disarm" factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Success? */
		if (!field_hook_single(fld_ptr, FIELD_ACT_INTERACT, (void *) &i))
		{
			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_MONSTERS | PU_MON_LITE);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Gain experience, but not for the locked doors in town */
			if (dun_level) gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* We may keep trying */
			return (TRUE);
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_MONSTERS | PU_MON_LITE);

		/* Sound */
		sound(SOUND_OPENDOOR);
	}

	/* Done - no more to try. */
	return (FALSE);
}


/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
	int y, x, dir;

	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_open)
	{
		int num_doors, num_chests;

		/* Count closed doors */
		num_doors = count_doors(&y, &x, is_closed, FALSE);

		/* Count chests (locked) */
		num_chests = count_chests(&y, &x, FALSE);

		/* See if only one target */
		if ((num_doors + num_chests) == 1)
		{
			command_dir = coords_to_dir(y, x);
		}
	}

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

#ifdef USE_SCRIPT
		if (cmd_open_callback(y, x))
		{
			/* Don't repeat the action */
			disturb(0, 0);
			return;
		}
#endif /* USE_SCRIPT */

		/* paranoia */
		if (!in_bounds2(y, x)) return;

		/* Get requested grid */
		c_ptr = area(y,x);

		/* Check for chest */
		o_idx = chest_check(y, x);

		/* Nothing useful */
		if (!((c_ptr->feat == FEAT_CLOSED) || o_idx))
		{
			/* Message */
			msg_print("You see nothing there to open.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Handle chests */
		else if (o_idx)
		{
			/* Open the chest */
			more = do_cmd_open_chest(y, x, o_idx);
		}

		/* Handle doors */
		else
		{
			/* Open the door */
			more = do_cmd_open_aux(y, x);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Perform the basic "close" command
 *
 * Assume destination is an open/broken door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = area(y,x);

	/* Broken door */
	if (c_ptr->feat == FEAT_BROKEN)
	{
		/* Message */
		msg_print("The door appears to be broken.");
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_CLOSED);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_MONSTERS | PU_MON_LITE);

		/* Sound */
		sound(SOUND_SHUTDOOR);
	}

	/* Result */
	return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(void)
{
	int y, x, dir;

	cave_type *c_ptr;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Count open doors */
		if (count_doors(&y, &x, is_open, FALSE) == 1)
		{
			command_dir = coords_to_dir(y, x);
		}
	}

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x))
		{
			/* Message */
			msg_print("You see nothing there to close.");

			disturb(0, 0);
			return;
		}

		/* Get grid and contents */
		c_ptr = area(y,x);

		/* Require open/broken door */
		if ((c_ptr->feat != FEAT_OPEN) && (c_ptr->feat != FEAT_BROKEN))
		{
			/* Message */
			msg_print("You see nothing there to close.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Close the door */
		else
		{
			/* Close the door */
			more = do_cmd_close_aux(y, x);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}






/*
 * Tunnel through wall.  Assumes valid location.
 */
static bool twall(int y, int x, byte feat)
{
	cave_type	*c_ptr = area(y,x);

	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_grid(c_ptr)) return (FALSE);

	/* Forget the wall */
	c_ptr->info &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, feat);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

	/* Result */
	return (TRUE);
}



/*
 * Perform the basic "tunnel" command
 *
 * Assumes that the destination is a wall, a vein, a secret
 * door, or rubble.
 *
 * Assumes that no monster is blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	bool more = FALSE;
	
	cave_type *c_ptr = area(y, x);
	
	int action;
	
	int dig = p_ptr->skill_dig;
	
	s16b *fld_ptr = field_hook_find(&c_ptr->fld_idx,
			 FIELD_ACT_INTERACT_TEST, (void *) &action);
			 
	/* Take a turn */
	energy_use = 100;
	
	/* Sound */
	sound(SOUND_DIG);

	/* Must have knowledge */
	if (!(c_ptr->info & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	if (*fld_ptr && (action == 0))
	{
		if (!field_hook_single(fld_ptr, FIELD_ACT_INTERACT, (void *) &dig))
		{
			/* Notice new floor grids */
			if (!cave_floor_grid(c_ptr))
			{
				/* Update some things */
				p_ptr->update |= (PU_VIEW | PU_FLOW |
					 PU_MONSTERS | PU_MON_LITE);
			}
			
			/* Finished tunneling */
			return(FALSE);
		}
		
		/* Keep on tunneling */
		return (TRUE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_grid(c_ptr))
	{
		/* Message */
		msg_print("You see nothing there to tunnel.");

		/* Nope */
		return (FALSE);
	}

	/* Titanium */
	if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
	    (c_ptr->feat <= FEAT_PERM_SOLID))
	{
		msg_print("This seems to be permanent rock.");
	}

	else if ((c_ptr->feat == FEAT_TREES) ||
		(c_ptr->feat == FEAT_PINE_TREE) ||
		(c_ptr->feat == FEAT_SNOW_TREE))
	{
		/* Chop Down */
		if ((p_ptr->skill_dig > 10 + rand_int(400)) && twall(y, x, FEAT_SNOW))
		{
			msg_print("You have cleared away the trees.");

			chg_virtue(V_DILIGENCE, 1);
			chg_virtue(V_NATURE, -1);
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
			msg_print("You chop away at the tree.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (rand_int(100) < 25) search();
		}
	}

	/* Jungle */
	else if (c_ptr->feat == FEAT_JUNGLE)
	{
		/* Chop Down */
		if ((p_ptr->skill_dig > 10 + rand_int(800)) && twall(y, x, FEAT_BUSH))
		{
			msg_print("You have cleared away the jungle.");

			chg_virtue(V_DILIGENCE, 1);
			chg_virtue(V_NATURE, -2);
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
			msg_print("You chop away at the undergrowth.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (rand_int(100) < 25) search();
		}
	}


	/* Granite + mountain side */
	else if (((c_ptr->feat >= FEAT_WALL_EXTRA) &&
	         (c_ptr->feat <= FEAT_WALL_SOLID)) ||
		 (c_ptr->feat == FEAT_MOUNTAIN) ||
		 (c_ptr->feat == FEAT_SNOW_MOUNTAIN))
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 40 + rand_int(1600)) && twall(y, x, FEAT_FLOOR))
		{
			msg_print("You have finished the tunnel.");

			chg_virtue(V_DILIGENCE, 1);
			chg_virtue(V_NATURE, -1);
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;
		}
	}


	/* Quartz / Magma */
	else if ((c_ptr->feat >= FEAT_MAGMA) &&
	    (c_ptr->feat <= FEAT_QUARTZ_K))
	{
		bool okay;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (c_ptr->feat >= FEAT_MAGMA_H) gold = TRUE;

		/* Extract "quartz" flag XXX XXX XXX */
		if ((c_ptr->feat - FEAT_MAGMA) & 0x01) hard = TRUE;

		/* Quartz */
		if (hard)
		{
			okay = (p_ptr->skill_dig > 20 + rand_int(800));
		}

		/* Magma */
		else
		{
			okay = (p_ptr->skill_dig > 10 + rand_int(400));
		}

		/* Success */
		if (okay && twall(y, x, FEAT_FLOOR))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(y, x);

				/* Message */
				msg_print("You have found something!");
			}

			/* Found nothing */
			else
			{
				/* Message */
				msg_print("You have finished the tunnel.");

				chg_virtue(V_DILIGENCE, 1);
				chg_virtue(V_NATURE, -1);
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
			msg_print("You tunnel into the quartz vein.");
			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
			msg_print("You tunnel into the magma vein.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (c_ptr->feat == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skill_dig > rand_int(200)) && twall(y, x, FEAT_FLOOR))
		{
			/* Message */
			msg_print("You have removed the rubble.");

			/* Hack -- place an object */
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE);

				/* Observe new object */
				if (player_can_see_bold(y, x))
				{
					msg_print("You have found something!");
				}
			}
		}

		else
		{
			/* Message, keep digging */
			msg_print("You dig in the rubble.");
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (c_ptr->feat >= FEAT_SECRET)
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 30 + rand_int(1200)) && twall(y, x, FEAT_FLOOR))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (rand_int(100) < 25) search();
		}
	}

	/* Notice new floor grids */
	if (!cave_floor_grid(c_ptr))
	{
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
	}

	/* Result */
	return (more);
}


/*
 * Tunnels through "walls" (including rubble and closed doors)
 *
 * Note that you must tunnel in order to hit invisible monsters
 * in walls, though moving into walls still takes a turn anyway.
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int			y, x, dir;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction to tunnel, or Abort */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Cannot escape the wilderness by tunneling */
		if (!in_bounds2(y, x))
		{
			/* Message */
			msg_print("You cannot tunnel outside the wilderness.");

			/* Do not repeat */
			disturb(0, 0);

			/* exit */
			return;
		}

		/* Get grid */
		c_ptr = area(y,x);

		/* No tunnelling through doors */
		if ((c_ptr->feat == FEAT_CLOSED) ||
		((c_ptr->feat >= FEAT_SHOP_HEAD) && (c_ptr->feat <= FEAT_SHOP_TAIL)))
		{
			/* Message */
			msg_print("You cannot tunnel through doors.");
		}

		/* No tunnelling through air */
		else if (cave_floor_grid(c_ptr) || ((c_ptr->feat >= FEAT_PATTERN_START) &&
		    (c_ptr->feat <= FEAT_PATTERN_XTRA2)))
		{
			/* Message */
			msg_print("You cannot tunnel through air.");
		}

		/* No tunneling through obelisks */
		else if (c_ptr->feat == FEAT_OBELISK)
		{
			/* Message */
			msg_print("You cannot tunnel through that.");
		}

		/* A monster is in the way */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Try digging */
		else
		{
			/* Tunnel through walls */
			more = do_cmd_tunnel_aux(y, x);
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(int y, int x, s16b o_idx)
{
	int i, j;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Take a turn */
	energy_use = 100;

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Extract the difficulty */
	j = i - o_ptr->pval;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Must find the trap first. */
	if (!object_known_p(o_ptr))
	{
		msg_print("I don't see any traps.");
	}

	/* Already disarmed/unlocked */
	else if (o_ptr->pval <= 0)
	{
		msg_print("The chest is not trapped.");
	}

	/* No traps to find. */
	else if (!chest_traps[o_ptr->pval])
	{
		msg_print("The chest is not trapped.");
	}

	/* Success (get a lot of experience) */
	else if (rand_int(100) < j)
	{
		msg_print("You have disarmed the chest.");
		gain_exp(o_ptr->pval);
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* We may keep trying */
		more = TRUE;
		if (flush_failure) flush();
		msg_print("You failed to disarm the chest.");
	}

	/* Failure -- Set off the trap */
	else
	{
		msg_print("You set off a trap!");
		sound(SOUND_FAIL);
		chest_trap(y, x, o_idx);
	}

	/* Result */
	return (more);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */

bool do_cmd_disarm_aux(cave_type *c_ptr, int dir)
{
	int i;

	field_type *f_ptr;
	field_thaum *t_ptr;
	s16b *fld_ptr;

	bool more = FALSE;
	
	int xp;
	
	/* Get trap */
	fld_ptr = field_first_known(&c_ptr->fld_idx, FTYPE_TRAP);
	
	/* This should never happen - no trap here to disarm */
	if (!(*fld_ptr)) return (FALSE);
	
	/* Take a turn */
	energy_use = 100;
	
	/* Point to field */
	f_ptr = &fld_list[*fld_ptr];
	
	/* Get amount of xp for a successful disarm */
	xp = f_ptr->data[0] * f_ptr->data[0];
	
	/* Get type of trap */
	t_ptr = &t_info[f_ptr->t_idx];
	 
	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Success */
	if (!field_hook_single(fld_ptr, FIELD_ACT_INTERACT, (void *) &i))
	{
		/* Message */
		msg_format("You have disarmed the %s.", t_ptr->name);

		/* Reward */
		gain_exp(xp);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		msg_format("You failed to disarm the %s.", t_ptr->name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		msg_format("You set off the %s!", t_ptr->name);

		/* Move the player onto the trap */
		move_player(dir, easy_disarm);
	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(void)
{
	int y, x, dir;

	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

	/* Option: Pick a direction */
	if (easy_disarm)
	{
		int num_traps, num_chests;

		/* Count visible traps */
		num_traps = count_traps(&y, &x, TRUE);

		/* Count chests (trapped) */
		num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if (num_traps || num_chests)
		{
			bool too_many = (num_traps && num_chests) || (num_traps > 1) ||
			    (num_chests > 1);
			if (!too_many) command_dir = coords_to_dir(y, x);
		}
	}

	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction (or abort) */
	if (get_rep_dir(&dir,TRUE))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x))
		{
			/* Message */
			msg_print("You see nothing there to disarm.");

			disturb(0, 0);
			return;
		}

		/* Get grid and contents */
		c_ptr = area(y,x);

		/* Check for chests */
		o_idx = chest_check(y, x);

		/* Disarm a trap */
		if (!is_visible_trap(c_ptr) && !o_idx)
		{
			/* Message */
			msg_print("You see nothing there to disarm.");
		}

		/* Monster in the way */
		else if (c_ptr->m_idx)
		{
			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Disarm chest */
		else if (o_idx)
		{
			/* Disarm the chest */
			more = do_cmd_disarm_chest(y, x, o_idx);
		}

		/* Disarm trap */
		else
		{
			/* Disarm the trap */
			more = do_cmd_disarm_aux(c_ptr, dir);
		}
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * Consider confusion XXX XXX XXX
 *
 * This command must always take a turn, to prevent free detection
 * of invisible monsters.
 */
void do_cmd_alter(void)
{
	int			y, x, dir;
	int			action;

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a direction */
	if (get_rep_dir(&dir,TRUE))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x))
		{
			/* Oops */
			msg_print("You attack the empty air.");

			disturb(0, 0);
			return;
		}

		/* Get grid */
		c_ptr = area(y,x);

		/* Take a turn */
		energy_use = 100;

		/* Attack monsters */
		if (c_ptr->m_idx)
		{
			/* Attack */
			py_attack(y, x);
		}

		else if (*field_hook_find(&c_ptr->fld_idx, FIELD_ACT_INTERACT_TEST,
					 (void *) &action))
		{
			switch (action)
			{
				case 0:
				{
					/* Tunnel */
					more = do_cmd_tunnel_aux(y, x);
					break;
				}
				
				case 1:
				{
					/* Disarm */
					more = do_cmd_disarm_aux(c_ptr, dir);
					break;
				}
				
				case 2:
				{
					/* Unlock / open */
					more = do_cmd_open_aux(y, x);
					break;
				}
			}
		}

		/* Tunnel through walls */
		else if (((c_ptr->feat >= FEAT_SECRET) &&
		          (c_ptr->feat <= FEAT_PERM_SOLID)) ||
		         ((c_ptr->feat == FEAT_TREES) ||
		          (c_ptr->feat == FEAT_MOUNTAIN) ||
			  (c_ptr->feat == FEAT_SNOW_MOUNTAIN) ||
			  (c_ptr->feat == FEAT_PINE_TREE) ||
			  (c_ptr->feat == FEAT_SNOW_TREE) ||
			  (c_ptr->feat == FEAT_JUNGLE)))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x);
		}

		/* Open closed doors */
		else if (c_ptr->feat == FEAT_CLOSED)
		{
			/* open */
			more = do_cmd_open_aux(y, x);
		}

		/* Close open doors */
		else if ((c_ptr->feat == FEAT_OPEN) ||
		         (c_ptr->feat == FEAT_BROKEN))
		{
			/* close */
			more = do_cmd_close_aux(y, x);
		}

		/* Oops */
		else
		{
			/* Oops */
			msg_print("You attack the empty air.");
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
static bool get_spike(int *ip)
{
	int i;

	/* Check every item in the pack */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Check the "tval" code */
		if (o_ptr->tval == TV_SPIKE)
		{
			/* Save the spike index */
			(*ip) = i;

			/* Success */
			return (TRUE);
		}
	}

	/* Oops */
	return (FALSE);
}


/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int dir, item;
	s16b y, x;

	cave_type *c_ptr;


	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x))
		{
			/* Message */
			msg_print("You see nothing there to spike.");

			disturb(0, 0);
			return;

		}

		/* Get grid and contents */
		c_ptr = area(y,x);

		/* Require closed door */
		if (c_ptr->feat != FEAT_CLOSED)
		{
			/* Message */
			msg_print("You see nothing there to spike.");
		}

		/* Get a spike */
		else if (!get_spike(&item))
		{
			/* Message */
			msg_print("You have no spikes!");
		}

		/* Is a monster in the way? */
		else if (c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = 100;

			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Go for it */
		else
		{
			/* Take a turn */
			energy_use = 100;

			/* Successful jamming */
			msg_print("You jam the door with a spike.");
			
			/* Make a jammed door on the square */
			make_lockjam_door(y, x, 1, TRUE);

			/* Use up, and describe, a single spike, from the bottom */
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
	}
}



/*
 * Support code for the "Walk" and "Jump" commands
 */
void do_cmd_walk(int pickup)
{
	int dir;

	bool more = FALSE;


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Take a turn */
		energy_use = 100;

		/* Actually move the character */
		move_player(dir, pickup);

		/* Allow more walking */
		more = TRUE;
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Start running.
 */
void do_cmd_run(void)
{
	int dir;

	/* Hack -- no running when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir,FALSE))
	{
		/* Hack -- Set the run counter */
		running = (command_arg ? command_arg : 1000);

		/* First step */
		run_step(dir);
	}
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_stay(int pickup)
{
	cave_type *c_ptr = area(py,px);


	/* Allow repeated command */
	if (command_arg)
	{
		/* Set repeat count */
		command_rep = command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		command_arg = 0;
	}


	/* Take a turn */
	energy_use = 100;


	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == rand_int(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->searching)
	{
		search();
	}


	/* Handle "objects" */
	carry(pickup);


	/* Hack -- enter a store if we are on one */
	if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
	    (c_ptr->feat <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		command_new = '_';
	}
#if 0
	/* Hack -- enter a building if we are on one -KMW- */
	else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
	    (c_ptr->feat <= FEAT_BLDG_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter building */
		command_new = ']';
	}
#endif
#if 0

	/* Exit a quest if reach the quest exit */
	else if (c_ptr->feat == FEAT_QUEST_EXIT)
	{
		int q_index = p_ptr->inside_quest;

		/* Was quest completed? */
		if (quest[q_index].type == QUEST_TYPE_FIND_EXIT)
		{
			quest[q_index].status = QUEST_STATUS_COMPLETED;
			msg_print("You accomplished your quest!");
			msg_print(NULL);
		}

		leaving_quest = p_ptr->inside_quest;

		/* Leaving an 'only once' quest marks it as failed */
		if (leaving_quest &&
			(quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
			(quest[leaving_quest].status == QUEST_STATUS_TAKEN))
		{
			quest[leaving_quest].status = QUEST_STATUS_FAILED;
		}

		p_ptr->inside_quest = area(py,px)->special;
		dun_level = 0;
		p_ptr->oldpx = 0;
		p_ptr->oldpy = 0;
		p_ptr->leaving = TRUE;
	}
#endif /* 0 */
}


/*
 * Resting allows a player to safely restore his hp	-RAK-
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (command_arg <= 0)
	{
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";

		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 4)) return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			command_arg = (-1);
		}

		/* Rest some */
		else
		{
			command_arg = atoi(out_val);
			if (command_arg <= 0) return;
		}
	}


	/* Paranoia */
	if (command_arg > 9999) command_arg = 9999;

	/* The sin of sloth */
	if (command_arg > 100)
		chg_virtue(V_DILIGENCE, -1);

	/* Why are you sleeping when there's no need?  WAKE UP!*/
	if ((p_ptr->chp == p_ptr->mhp) &&
		(p_ptr->csp == p_ptr->msp) &&
		!p_ptr->blind && !p_ptr->confused &&
		!p_ptr->poisoned && !p_ptr->afraid &&
		!p_ptr->stun && !p_ptr->cut &&
		!p_ptr->slow && !p_ptr->paralyzed &&
		!p_ptr->image && !p_ptr->word_recall)
			chg_virtue(V_DILIGENCE, -1);

	/* Take a turn XXX XXX XXX (?) */
	energy_use = 100;

	/* Save the rest code */
	resting = command_arg;

	/* Cancel searching */
	p_ptr->searching = FALSE;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh */
	Term_fresh();
}


/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(object_type *o_ptr)
{
	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
			return (100);

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_SKELETON:
			return (50);

		/* Sometimes break */
		case TV_WAND:
		case TV_SPIKE:
		case TV_ARROW:
			return (25);

		/* Rarely break */
		case TV_SHOT:
		case TV_BOLT:
		default:
			return (10);
	}
}


/*
 * Calculation of critical hits for objects fired or thrown by the player. -LM-
 */
static sint critical_shot(int chance, int sleeping_bonus,
	char o_name[], char m_name[], int visible)
{
	int i, k;
	int mult_a_crit;

	if (!visible)
	{
		msg_format("The %s finds a mark.", o_name);
	}

	/* Extract missile power. */
	i = (chance + sleeping_bonus);

	/* Test for critical hit. */
	if (randint(i + 200) <= i)
	{
		/* Encourage the player to throw weapons at sleeping
		 * monsters. -LM-
		 */
		if (sleeping_bonus && visible)
		{
			msg_print("You rudely awaken the monster!");
		}

		/* Determine level of critical hit */
		k = randint(i) + randint(100);

		/* This portion of the function determines the level of critical hit,
		 * then adjusts the damage dice multiplier and displays an appropriate
		 * combat message.
		 * A distinction is made between visible and invisible monsters.
		 */
		if (k < 125)
		{
			if (visible)
			{
				msg_format("The %s strikes %s.", o_name, m_name);
			}

			mult_a_crit = 15;
		}
		else if (k < 215)
		{
			if (visible)
			{
				msg_format("The %s penetrates %s.", o_name, m_name);
			}

			mult_a_crit = 21;
		}
		else if (k < 275)
		{
			if (visible)
			{
				msg_format("The %s drives into %s!", o_name, m_name);
			}

			mult_a_crit = 28;
		}
		else
		{
			if (visible)
			{
				msg_format("The %s transpierces %s!", o_name, m_name);
			}

			mult_a_crit = 35;
		}
	}
	/* If the shot is not a critical hit, then the default message is shown. */
	else
	{
		if (visible)
		{
			msg_format("The %s hits %s.", o_name, m_name);
		}

		mult_a_crit = 10;
	}

	return (mult_a_crit);
}



/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void do_cmd_fire_aux(int item, object_type *j_ptr)
{
	int dir;
	int j, y, x, ny, nx, ty, tx;

	int armour, bonus, chance, total_deadliness;

	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	long tdam;
	int tdam_remainder, tdam_whole;

	/* Assume no weapon of velocity or accuracy bonus. */
	int special_dam = 0;
	int special_hit = 0;

	object_type *o_ptr;

	int tdis, thits, tmul;
	int cur_dis, visible;

	int chance2;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	char o_name[80];
	char m_name[80];

	int msec = delay_factor * delay_factor * delay_factor;

	cave_type *c_ptr;

	/* Missile launchers of Velocity and Accuracy sometimes "supercharge" */
	if ((j_ptr->name2 == EGO_VELOCITY) || (j_ptr->name2 == EGO_ACCURACY))
	{
		/* Occasional boost to shot. */
		if (randint(16) == 1)
		{
			if (j_ptr->name2 == EGO_VELOCITY) special_dam = TRUE;
			else if (j_ptr->name2 == EGO_ACCURACY) special_hit = TRUE;

			/* Describe the object */
			object_desc(o_name, j_ptr, FALSE, 0);

			/* Let player know that weapon is activated. */
			msg_format("You feel your %s tremble in your hand.", o_name);
		}
	}

	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* sum all the applicable additions to Deadliness. */
	total_deadliness = p_ptr->to_d + i_ptr->to_d + j_ptr->to_d;

	/* Single object */
	i_ptr->number = 1;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}

	/* Sound */
	sound(SOUND_SHOOT);

	/* Describe the object */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Actually "fire" the object. */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = 1;

	/* Analyze the launcher */
	switch (j_ptr->sval)
	{
		/* Sling and ammo */
		case SV_SLING:
		{
			tmul = 2;
			energy_use = 50;
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			tmul = 2;
			energy_use = 100;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			if (p_ptr->stat_use[A_STR] >= 16)
			{
				tmul = 3;
			}
			else
			{
				/* weak players cannot use a longbow well */
				tmul = 2;
			}
			energy_use = 100;
			break;
		}

		/* Light Crossbow and Bolt */
		case SV_LIGHT_XBOW:
		{
			tmul = 4;
			energy_use = 120;
			break;
		}

		/* Heavy Crossbow and Bolt */
		case SV_HEAVY_XBOW:
		{
			tmul = 5;
			if (p_ptr->stat_use[A_DEX] >= 16)
			{
				energy_use = 150;
			}
			else
			{
				/* players with low dex will take longer to load */
				energy_use = 200;
			}
			break;
		}
	}

	/* Get extra "power" from "extra might" */
	if (p_ptr->xtra_might) tmul++;

	/* Base range */
	tdis = 5 + 5 * tmul;

	/* Take a (partial) turn - note strange formula. */
	
	/* The real number of shots per round is (1 + n)/2 */
	energy_use = (2 * energy_use / (1 + thits));

	/* Fire ammo of backbiting, and it will turn on you. -LM- */
	if (i_ptr->name2 == EGO_BACKBITING)
	{
		/* Message. */
		msg_print("Your missile turns in midair and strikes you!");

		/* Calculate damage. */
		tdam = damroll(tmul * 4, i_ptr->ds);

		/* Inflict both normal and wound damage. */
		take_hit(tdam, "ammo of backbiting.");
		set_cut(randint(tdam * 3));

		/* That ends that shot! */
		return;
	}

	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}


	/* Hack -- Handle stuff */
	handle_stuff();


	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis; )
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stopped by wilderness boundary */
		if (!in_bounds2(ny, nx)) break;

		/* Stopped by walls/doors */
		c_ptr = area(ny, nx);
		if (!cave_floor_grid(c_ptr)) break;

		/* Advance the distance */
		cur_dis++;


		/* The player can see the (on screen) missile */
		if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
		{
			char c = object_char(i_ptr);
			byte a = object_attr(i_ptr);

			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(c, a, ny, nx);
			move_cursor_relative(ny, nx);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(ny, nx);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Save the new location */
		x = nx;
		y = ny;


		/* Monster here, Try to hit it */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			chance2 = chance - cur_dis;

			/* Note the collision */
			hit_body = TRUE;

			/* Sleeping, visible monsters are easier to hit. -LM- */
			if ((m_ptr->csleep) && (visible))
				sleeping_bonus = 5 + p_ptr->lev / 5;

			/* Monsters in rubble can take advantage of cover. -LM- */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/*
			 * Monsters in trees can take advantage of cover,
			 * except from rangers.
			 */
			else if ((c_ptr->feat == FEAT_TREES) &&
					 (p_ptr->pclass == CLASS_RANGER))
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. -LM- */
			else if (c_ptr->feat == FEAT_DEEP_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}

			/* Get effective armour class of monster. */
			armour = r_ptr->ac + terrain_bonus;

			/* Weapons of velocity sometimes almost negate monster armour. */
			if (special_hit) armour /= 3;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2 + sleeping_bonus, armour, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if (!monster_living(r_ptr))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Get "the monster" or "it" */
				monster_desc(m_name, m_ptr, 0);

				if (visible)
				{
					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* The basic damage-determination formula is the same in
				 * archery as it is in melee (apart from the launcher mul-
				 * tiplier).  See formula "py_attack" in "cmd1.c" for more
				 * details. -LM-
				 */

				/* Base damage dice. */
				tdam = i_ptr->dd;

				/* Multiply by the missile weapon multiplier. */
				tdam *= tmul;


				/* multiply by slays or brands. (10x inflation) */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);

				/* multiply by critical shot. (10x inflation) + level damage bonus */
				tdam *= critical_shot(chance2, sleeping_bonus,
					o_name, m_name, visible);

				/*
				 * Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					tdam *= (100 +
					deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					tdam *= (100 -
					deadliness_conversion[ABS(total_deadliness)]);
				else
					tdam = 0;

				/* Get the whole number of dice by deflating the result. */
				tdam_whole = tdam / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				tdam_remainder = tdam % 10000;

				/*
				 * Calculate and combine the damages of the whole and
				 * fractional dice.
				 */
				tdam = damroll(tdam_whole, o_ptr->ds) +
					(tdam_remainder * damroll(1, o_ptr->ds) / 10000);

				/* If a weapon of velocity activates, increase damage. */
				if (special_dam)
				{
					tdam += 15;
				}

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Modify the damage */
				tdam = mon_damage_mod(m_ptr, tdam, 0);

				/* Complex message */
				if (wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(c_ptr->m_idx, tdam);

					/* Anger the monster */
					if (tdam > 0) anger_monster(m_ptr);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Sound */
						sound(SOUND_FLEE);

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						msg_format("%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	(void)drop_near(i_ptr, j, y, x);
}


void do_cmd_fire(void)
{
	int item;
	object_type *j_ptr;
	cptr q, s;

	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

	/* Require a launcher */
	if (!j_ptr->tval)
	{
		msg_print("You have nothing to fire with.");
		return;
	}


	/* Require proper missile */
	item_tester_tval = p_ptr->tval_ammo;

	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Fire the item */
	do_cmd_fire_aux(item, j_ptr);
}


/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
void do_cmd_throw_aux(int mult)
{
	int dir, item;
	int j, y, x, ny, nx, ty, tx;
	int chance, chance2, tdis;
	int mul, div;
	int cur_dis, visible;

	long tdam;
	int tdam_remainder, tdam_whole;

	int total_deadliness;
	int sleeping_bonus = 0;
	int terrain_bonus = 0;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	bool hit_body = FALSE;
	bool hit_wall = FALSE;

	char o_name[80];
	char m_name[80];

	int msec = delay_factor * delay_factor * delay_factor;

	u32b f1, f2, f3;
	cptr q, s;

	cave_type *c_ptr;


	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		/* Nope */
		return;
	}

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Extract the thrown object's flags. */
	object_flags(q_ptr, &f1, &f2, &f3);

	/* Distribute the charges of rods/wands between the stacks */
	distribute_charges(o_ptr, q_ptr, 1);

	/* Single object */
	q_ptr->number = 1;

	/* Reduce and describe inventory */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}


	/* Description */
	object_desc(o_name, q_ptr, FALSE, 3);

	/* Extract a "distance multiplier" */
	/* Changed for 'launcher' mutation */
	mul = 10 + 2 * (mult - 1);

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10-18 */
	if (tdis > mul) tdis = mul;

	/* Chance of hitting.  Other thrown objects are easier to use, but
	 * only throwing weapons take advantage of bonuses to Skill from
	 * other items. -LM-
	 */
	if (f2 & (TR2_THROW)) chance = ((p_ptr->skill_tht) +
		((p_ptr->to_h + q_ptr->to_h) * BTH_PLUS_ADJ));
	else chance = ((3 * p_ptr->skill_tht / 2) +
		(q_ptr->to_h * BTH_PLUS_ADJ));


	/* Take a turn */
	energy_use = 100;


	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}


	/* Hack -- Handle stuff */
	handle_stuff();


	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis; )
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stopped by wilderness boundary */
		if (!in_bounds2(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}

		/* Stopped by walls/doors */
		c_ptr = area(ny, nx);
		if (!cave_floor_grid(c_ptr))
		{
			hit_wall = TRUE;
			break;
		}

		/* Advance the distance */
		cur_dis++;

		/* The player can see the (on screen) missile */
		if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
		{
			char c = object_char(q_ptr);
			byte a = object_attr(q_ptr);

			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(c, a, ny, nx);
			move_cursor_relative(ny, nx);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(ny, nx);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Save the new location */
		x = nx;
		y = ny;


		/* Monster here, Try to hit it */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Calculate the projectile accuracy, modified by distance. */
			chance2 = chance - distance(py, px, y, x);

			/* Monsters in rubble can take advantage of cover. -LM- */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/*
			 * Monsters in trees can take advantage of cover,
			 * except from rangers.
			 */
			else if ((c_ptr->feat == FEAT_TREES) &&
			         (p_ptr->pclass == CLASS_RANGER))
			{
				terrain_bonus = r_ptr->ac / 5 + 5;
			}
			/* Monsters in water are vulnerable. -LM- */
			else if (c_ptr->feat == FEAT_DEEP_WATER)
			{
				terrain_bonus -= r_ptr->ac / 4;
			}

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance - cur_dis, r_ptr->ac + terrain_bonus, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if (!monster_living(r_ptr))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}

				/* Get "the monster" or "it" */
				monster_desc(m_name, m_ptr, 0);

				/* Handle visible monster */
				if (visible)
				{
					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* sum all the applicable additions to Deadliness. */
				total_deadliness = p_ptr->to_d + q_ptr->to_d;


				/* The basic damage-determination formula is the same in
				 * throwing as it is in melee (apart from the thrown weapon
				 * multiplier, and the ignoring of non-object bonuses to
				 * Deadliness for objects that are not thrown weapons).  See
				 * formula "py_attack" in "cmd1.c" for more details. -LM-
				 */

				tdam = q_ptr->dd;

				/* Multiply the number of damage dice by the throwing weapon
				 * multiplier, if applicable.  This is not the prettiest
				 * equation, but it does at least try to keep throwing
				 * weapons competitive.
				 */
				if (f2 & (TR2_THROW))
				{
					tdam *= 4 + p_ptr->lev / 6;
				}

				/* multiply by slays or brands. (10x inflation) */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr);

				/* Only allow critical hits if the object is a throwing
				 * weapon.  Otherwise, grant the default multiplier.
				 * (10x inflation)
				 */
				if (f2 & (TR2_THROW)) tdam *= critical_shot
					(chance2, sleeping_bonus, o_name, m_name, visible);
				else tdam *= 10;

				/* Convert total or object-only Deadliness into a percen-
				 * tage, and apply it as a bonus or penalty (100x inflation)
				 */
				if (f2 & (TR2_THROW))
				{
					if (total_deadliness > 0)
						tdam *= (100 +
						    deadliness_conversion[total_deadliness]);
					else if (total_deadliness > -31)
						tdam *= (100 -
						    deadliness_conversion[ABS(total_deadliness)]);
					else
						tdam = 0;
				}

				else
				{
					if (q_ptr->to_d > 0)
						tdam *= (100 +
						    deadliness_conversion[q_ptr->to_d]);
					else if (q_ptr->to_d > -31)
						tdam *= (100 -
						     deadliness_conversion[ABS(q_ptr->to_d)]);
					else
						tdam = 0;
				}

				/* Get the whole number of dice by deflating the result. */
				tdam_whole = tdam / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				tdam_remainder = tdam % 10000;


				/* Calculate and combine the damages of the whole and
				 * fractional dice.
				 */
				tdam = damroll(tdam_whole, q_ptr->ds) +
					(tdam_remainder * damroll(1, q_ptr->ds) / 10000);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Modify the damage */
				tdam = mon_damage_mod(m_ptr, tdam, 0);

				/* Complex message */
				if (wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(c_ptr->m_idx, tdam);

					/* Anger the monster */
					if ((tdam > 0) && !object_is_potion(q_ptr))
						anger_monster(m_ptr);

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Sound */
						sound(SOUND_FLEE);

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						msg_format("%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(q_ptr) : 0);

	/* Figurines transform */
	if ((q_ptr->tval == TV_FIGURINE) && !(p_ptr->inside_arena))
	{
		j = 100;

		if (!(summon_named_creature(y, x, q_ptr->pval, FALSE, FALSE,
				(bool)!(q_ptr->ident & IDENT_CURSED))))
			msg_print("The Figurine writhes and then shatters.");
		else if (q_ptr->ident & IDENT_CURSED)
			msg_print("You have a bad feeling about this.");
	}

	/* Potions smash open */
	if (object_is_potion(q_ptr))
	{
		if (hit_body || hit_wall || (randint(100) < j))
		{
			/* Message */
			msg_format("The %s shatters!", o_name);

			if (potion_smash_effect(0, y, x, q_ptr->k_idx))
			{
				monster_type *m_ptr = &m_list[area(y, x)->m_idx];

				/* ToDo (Robert): fix the invulnerability */
				if (area(y, x)->m_idx &&
				    !is_hostile(&m_list[area(y, x)->m_idx]) &&
				    !(m_ptr->invulner))
				{
					char m_name[80];
					monster_desc(m_name, &m_list[area(y, x)->m_idx], 0);
					msg_format("%^s gets angry!", m_name);
					set_hostile(&m_list[area(y, x)->m_idx]);
				}
			}

			return;
		}
		else
		{
			j = 0;
		}
	}

	/* Drop (or break) near that location */
	(void)drop_near(q_ptr, j, y, x);

	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Throw an object from the pack or floor.
 */
void do_cmd_throw(void)
{
	do_cmd_throw_aux(1);
}
