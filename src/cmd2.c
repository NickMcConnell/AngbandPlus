/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Verify stairs */
	if (cave_feat[py][px] != FEAT_LESS)
	{
		mprint(MSG_TEMP, "I see no up staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	msg_print("You enter a maze of up staircases.");

	if (p_ptr->inside_special == SPECIAL_QUEST)
	{
		p_ptr->depth = 0;
		p_ptr->inside_special = 0;


	} else if (p_ptr->inside_special == SPECIAL_WILD) {
	  mprint(MSG_STUPID, "That is not the way to reach Heaven.");
	  return;

	} else
	{
		/* Create a way back */
		p_ptr->create_down_stair = TRUE;

		/* New depth */
		p_ptr->depth--;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Verify stairs */
	if (cave_feat[py][px] != FEAT_MORE && cave_feat[py][px] != FEAT_SHAFT)
	{
		mprint(MSG_TEMP, "I see no down staircase here.");
		return;
	}

	if (p_ptr->inside_special == SPECIAL_WILD)
	{
	  /* HACK: Save the current location. */
	  p_ptr->wilderness_px = p_ptr->px;
	  p_ptr->wilderness_py = p_ptr->py;
	  p_ptr->wilderness_depth = p_ptr->depth;

	  p_ptr->inside_special = 0;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	msg_print("You enter a maze of down staircases.");

	/* Create a way back */
	p_ptr->create_up_stair = TRUE;

	/* New level */
	if (cave_feat[py][px] == FEAT_SHAFT) {
	  p_ptr->depth += randint(3);

	} else {
	  p_ptr->depth++;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}



/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

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
static object_type *chest_check(int y, int x)
{

	object_type *o_ptr;

	/* Scan all objects in the grid */
	for (o_ptr = cave_o_idx[y][x]; o_ptr != NULL; o_ptr = o_ptr->next)
	{

		/* Skip unknown chests XXX XXX */
		/* if (!o_ptr->marked) continue; */

		/* Check for chest */
		if (o_ptr->tval == TV_CHEST)
			return (o_ptr);
	}

	/* No chest */
	return (NULL);
}


/*
 * Allocate objects upon opening a chest
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * Small chests often contain "gold", while Large chests always contain
 * items.  Wooden chests contain 2 items, Iron chests contain 4 items,
 * and Steel chests contain 6 items.  The "value" of the items in a
 * chest is based on the "power" of the chest, which is in turn based
 * on the level on which the chest is generated.
 */
static void chest_death(int y, int x, object_type * o_ptr)
{
	int number;

	bool tiny;

	object_type *i_ptr;

	/* Small chests often hold "gold" */
	tiny = (o_ptr->sval < SV_CHEST_MIN_LARGE);

	/* Determine how much to drop (see above) */
	number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

	/* Zero pval means empty chest */
	if (!o_ptr->pval)
		number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 10;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		i_ptr = new_object();

		/* Make an object */
		if (!make_object(i_ptr, FALSE, FALSE))
			continue;

		/* Drop it in the dungeon */
		drop_near(i_ptr, FALSE, y, x);
	}

	/* Reset the object level */
	object_level = p_ptr->depth;

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
static void chest_trap(int y, int x, object_type * o_ptr)
{
	int i, trap;


	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0)
		return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
		mprint(MSG_STUPID, "A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void) do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		mprint(MSG_STUPID, "A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void) do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		mprint(MSG_STUPID, "A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void) set_poisoned(p_ptr->poisoned + 10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		mprint(MSG_STUPID, "A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void) set_paralyzed(p_ptr->paralyzed + 10 + randint(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		mprint(MSG_STUPID, "You are enveloped in a cloud of smoke!");
		for (i = 0; i < num; i++)
		{
			(void) summon_specific(y, x, p_ptr->depth, 0);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		mprint(MSG_STUPID, "There is a sudden explosion!");
		mprint(MSG_STUPID, "Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
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
static bool do_cmd_open_chest(int y, int x, object_type * o_ptr)
{
	int i, j;

	bool flag = TRUE;

	bool more = FALSE;

	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite())
			i = i / 10;
		if (p_ptr->confused || p_ptr->image)
			i = i / 10;

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2)
			j = 2;

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
			if (flush_failure)
				flush();
			mprint(MSG_TEMP, "You failed to pick the lock.");
		}
	}

	/* Allowed to open */
	if (flag)
	{
		/* Apply chest traps, if any */
		chest_trap(y, x, o_ptr);

		/* Let the Chest drop items */
		chest_death(y, x, o_ptr);
	}

	/* Result */
	return (more);
}


/*
 * Attempt to disarm the chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(int y, int x, object_type * o_ptr)
{
	int i, j;

	bool more = FALSE;


	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite())
		i = i / 10;
	if (p_ptr->confused || p_ptr->image)
		i = i / 10;

	/* Extract the difficulty */
	j = i - o_ptr->pval;

	/* Always have a small chance of success */
	if (j < 2)
		j = 2;

	/* Must find the trap first. */
	if (!object_known_p(o_ptr))
	{
		mprint(MSG_TEMP, "I don't see any traps.");
	}

	/* Already disarmed/unlocked */
	else if (o_ptr->pval <= 0)
	{
		mprint(MSG_TEMP, "The chest is not trapped.");
	}

	/* No traps to find. */
	else if (!chest_traps[o_ptr->pval])
	{
		mprint(MSG_TEMP, "The chest is not trapped.");
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
		if (flush_failure)
			flush();
		mprint(MSG_TEMP, "You failed to disarm the chest.");
	}

	/* Failure -- Set off the trap */
	else
	{
		mprint(MSG_STUPID, "You set off a trap!");
		chest_trap(y, x, o_ptr);
	}

	/* Result */
	return (more);
}


/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_feats(int *y, int *x, byte f1, byte f2)
{
	int d, count;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave_info[yy][xx] & (CAVE_MARK)))
			continue;

		/* Not looking for this feature */
		if (cave_feat[yy][xx] < f1)
			continue;
		if (cave_feat[yy][xx] > f2)
			continue;

		/* Count it */
		++count;

		/* Remember the location of the last feature found */
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
	int d, count;

	object_type *o_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* No (visible) chest is there */
		if ((o_ptr = chest_check(yy, xx)) == NULL)
			continue;

		/* Already open */
		if (o_ptr->pval == 0)
			continue;

		/* No (known) traps here */
		if (trapped && (!object_known_p(o_ptr) || (o_ptr->pval < 0) ||
				!chest_traps[o_ptr->pval]))
			continue;

		/* Count it */
		++count;

		/* Remember the location of the last chest found */
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
	return (motion_dir(p_ptr->py, p_ptr->px, y, x));
}


/*
 * Determine if a given grid may be "opened"
 */
static bool do_cmd_open_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
			(cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there to open.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
	int i, j;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_open_test(y, x))
		return (FALSE);


	/* Jammed door */
	if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked door */
	else if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite())
			i = i / 10;
		if (p_ptr->confused || p_ptr->image)
			i = i / 10;

		/* Extract the lock power */
		j = cave_feat[y][x] - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2)
			j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			msg_print("You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure)
				flush();

			/* Message */
			mprint(MSG_TEMP, "You failed to pick the lock.");

			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

		/* Sound */
		sound(SOUND_OPENDOOR);
	}

	/* Result */
	return (more);
}



/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	object_type *o_ptr;

	bool more = FALSE;


	/* Easy Open */
	if (easy_open)
	{
		/* Handle a single closed door or locked chest */
		if ((count_feats(&y, &x, FEAT_DOOR_HEAD,
					FEAT_DOOR_TAIL) + count_chests(&y, &x, FALSE)) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_ptr = chest_check(y, x);


	/* Verify legality */
	if (!o_ptr && !do_cmd_open_test(y, x))
		return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chest */
		o_ptr = chest_check(y, x);
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Chest */
	else if (o_ptr)
	{
		/* Open the chest */
		more = do_cmd_open_chest(y, x, o_ptr);
	}

	/* Door */
	else
	{
		/* Open the door */
		more = do_cmd_open_aux(y, x);
	}

	/* Cancel repeat unless we may continue */
	if (!more)
		disturb(0, 0);
}


/*
 * Determine if a given grid may be "closed"
 */
static bool do_cmd_close_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require open/broken door */
	if ((cave_feat[y][x] != FEAT_OPEN) && (cave_feat[y][x] != FEAT_BROKEN))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there to close.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "close" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_close_test(y, x))
		return (FALSE);


	/* Broken door */
	if (cave_feat[y][x] == FEAT_BROKEN)
	{
		/* Message */
		msg_print("The door appears to be broken.");
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


	/* Easy Close */
	if (easy_open)
	{
		/* Handle a single open door */
		if (count_feats(&y, &x, FEAT_OPEN, FEAT_OPEN) == 1)
		{
			/* Don't close door player is on */
			if ((y != py) || (x != px))
			{
				p_ptr->command_dir = coords_to_dir(y, x);
			}
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_close_test(y, x))
		return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Door */
	else
	{
		/* Close door */
		more = do_cmd_close_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more)
		disturb(0, 0);
}



/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	if (cave_feat[y][x] == FEAT_CHAOS_FOG)
	{
		msg_print("The fog is too insubstansial to tunnel.");
		return FALSE;
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(y, x))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there to tunnel.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * Attempting to do so will produce floor grids which are not part
 * of the room, and whose "illumination" status do not change with
 * the rest of the room.
 */
static bool twall(int y, int x)
{
	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x))
		return (FALSE);

	/* Sound */
	sound(SOUND_DIG);

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

	/* Result */
	return (TRUE);
}


/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x))
		return (FALSE);


	/* Sound XXX XXX XXX */
	/* sound(SOUND_DIG); */

	/* Titanium */
	if ((cave_feat[y][x] >= FEAT_PERM_EXTRA) &&
		(cave_feat[y][x] <= FEAT_PERM_SOLID))
	{
		msg_print("This seems to be permanent rock.");
	}

	/* water, lava, & trees -KMW- */
	else if (((cave_feat[y][x] >= FEAT_DEEP_WATER) &&
			(cave_feat[y][x] <= FEAT_SHAL_LAVA)) ||
		(cave_feat[y][x] == FEAT_MOUNTAIN) ||
		((cave_feat[y][x] >= FEAT_QUEST_ENTER) &&
			(cave_feat[y][x] <= FEAT_QUEST_EXIT)))
	{
		msg_print("You can't tunnel through that!");
	}

	else if (cave_feat[y][x] == FEAT_TREES)	/* -KMW- */
	{
		/* Chop Down */
		if ((p_ptr->skill_dig > 10 + rand_int(400)) && twall(y, x))
		{
			msg_print("You have cleared away the trees.");
		}

		/* Keep trying */
		else
		{
			/* We may continue chopping */
			mprint(MSG_TEMP, "You chop away at the tree.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (rand_int(100) < 25)
				search();
		}
	}

	/* Granite */
	else if ((cave_feat[y][x] >= FEAT_WALL_EXTRA) &&
		(cave_feat[y][x] <= FEAT_WALL_SOLID))
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 40 + rand_int(1600)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			mprint(MSG_TEMP, "You tunnel into the granite wall.");
			more = TRUE;
		}
	}

	/* Quartz / Magma */
	else if ((cave_feat[y][x] >= FEAT_MAGMA) &&
		(cave_feat[y][x] <= FEAT_QUARTZ_K))
	{
		bool okay = FALSE;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (cave_feat[y][x] >= FEAT_MAGMA_H)
			gold = TRUE;

		/* Extract "quartz" flag XXX XXX XXX */
		if ((cave_feat[y][x] - FEAT_MAGMA) & 0x01)
			hard = TRUE;

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
		if (okay && twall(y, x))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_object(y, x, FALSE, FALSE);

				/* Message */
				mprint(MSG_BONUS, "You have found something!");
			}

			/* Found nothing */
			else
			{
				/* Message */
				msg_print("You have finished the tunnel.");
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
			mprint(MSG_TEMP, "You tunnel into the quartz vein.");
			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
			mprint(MSG_TEMP, "You tunnel into the magma vein.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skill_dig > rand_int(200)) && twall(y, x))
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
					mprint(MSG_BONUS, "You have found something!");
				}
			}
		}

		else
		{
			/* Message, keep digging */
			mprint(MSG_TEMP, "You dig in the rubble.");
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (cave_feat[y][x] >= FEAT_SECRET)
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 30 + rand_int(1200)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			mprint(MSG_TEMP, "You tunnel into the granite wall.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (rand_int(100) < 25)
				search();
		}
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 30 + rand_int(1200)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			mprint(MSG_TEMP, "You tunnel into the door.");
			more = TRUE;
		}
	}

	/* Notice new floor grids */
	if (!cave_floor_bold(y, x))
	{
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
	}

	/* Result */
	return (more);
}


/*
 * Tunnel through "walls" (including rubble and secret doors)
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Oops */
	if (!do_cmd_tunnel_test(y, x))
		return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Walls */
	else
	{
		/* Tunnel through walls */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Cancel repetition unless we can continue */
	if (!more)
		disturb(0, 0);
}


/*
 * Determine if a given grid may be "disarmed"
 */
static bool do_cmd_disarm_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require an actual trap */
	if (!((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
			(cave_feat[y][x] <= FEAT_TRAP_TAIL)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there to disarm.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_aux(int y, int x)
{
	int i, j, power;

	cptr name;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_disarm_test(y, x))
		return (FALSE);


	/* Access trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite())
		i = i / 10;
	if (p_ptr->confused || p_ptr->image)
		i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power" */
	power = 5;

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2)
		j = 2;

	/* Success */
	if (rand_int(100) < j)
	{
		/* Message */
		msg_format("You have disarmed the %s.", name);

		/* Reward */
		gain_exp(power);

		/* Forget the trap */
		cave_info[y][x] &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(y, x, FEAT_FLOOR);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* Failure */
		if (flush_failure)
			flush();

		/* Message */
		mformat(MSG_TEMP, "You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		mformat(MSG_STUPID, "You set off the %s!", name);

		/* Hit the trap */
		hit_trap(y, x);
	}

	/* Result */
	return (more);
}


/*
 * Disarms a trap, or a chest
 */
void do_cmd_disarm(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	object_type *o_ptr;

	bool more = FALSE;


	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Handle a single visible trap or trapped chest */
		if ((count_feats(&y, &x, FEAT_TRAP_HEAD,
					FEAT_TRAP_TAIL) + count_chests(&y, &x, TRUE)) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_ptr = chest_check(y, x);


	/* Verify legality */
	if (!o_ptr && !do_cmd_disarm_test(y, x))
		return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chests */
		o_ptr = chest_check(y, x);
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Chest */
	else if (o_ptr)
	{
		/* Disarm the chest */
		more = do_cmd_disarm_chest(y, x, o_ptr);
	}

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_cmd_disarm_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more)
		disturb(0, 0);
}


/*
 * Determine if a given grid may be "bashed"
 */
static bool do_cmd_bash_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door or altar */
	if ((cave_feat[y][x] < FEAT_DOOR_HEAD ||
			cave_feat[y][x] > FEAT_DOOR_TAIL) &&
		(cave_feat[y][x] < FEAT_ALTAR_HEAD ||
			cave_feat[y][x] > FEAT_ALTAR_TAIL))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there to bash.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/* Try to bash an altar. */

static bool do_cmd_bash_altar(int y, int x)
{
	bool more = FALSE;
	int pgod = p_ptr->pgod;
	int agod = cave_feat[y][x] - FEAT_ALTAR_HEAD + 1;

	msg_print("You smash into the altar!");

	/* Player doesn't worship anyone -- create an explosion. */
	if (pgod == 0)
	{
		godly_wrath_blast(agod - 1);

	}
	else if (pgod == agod)
	{
		/* Supreme blasphemy! */
		mformat(MSG_DEADLY,
			"%s thunders: ``Enjoy your eternity in Hell, mortal!",
			deity_info[pgod - 1].name);
		set_grace(-200000);
	}
	else
	{

		/* Has the player asked for a favor recently? */
		if (p_ptr->god_favor > 0)
		{
			mformat(MSG_URGENT, "%s thunders: ``Know thy place, mortal!''",
				deity_info[pgod - 1].name);

			godly_wrath_blast(pgod - 1);

			set_grace(p_ptr->grace - 1000);

		}
		else
		{
			mprint(MSG_BIG_BONUS,
				"The old altar crumbles into dust, and a new "
				"one materializes in its place.");
			mformat(MSG_BIG_BONUS, "%s is very pleased!",
				deity_info[pgod - 1].name);

			set_grace(p_ptr->grace + 1000);
			p_ptr->god_favor += 3000;

			/* Forget the altar */
			cave_info[y][x] &= ~(CAVE_MARK);
			/* Change it. */
			cave_set_feat(y, x, FEAT_ALTAR_HEAD + (pgod - 1));
		}
	}

	return more;
}

/*
 * Perform the basic "bash" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x)
{
	int bash, temp;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_bash_test(y, x))
		return (FALSE);


	/* Message */
	msg_print("You smash into the door!");

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = ((cave_feat[y][x] - FEAT_DOOR_HEAD) & 0x07);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1)
		temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Message */
		msg_print("The door crashes open!");

		/* Break down the door */
		if (rand_int(100) < 50)
		{
			cave_set_feat(y, x, FEAT_BROKEN);
		}

		/* Open the door */
		else
		{
			cave_set_feat(y, x, FEAT_OPEN);
		}

		/* Sound */
		sound(SOUND_OPENDOOR);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE);
		p_ptr->update |= (PU_DISTANCE);
	}

	/* Saving throw against stun */
	else if (rand_int(100) <
		adj_dex_safe[p_ptr->stat_ind[A_DEX]] + p_ptr->lev)
	{
		/* Message */
		mprint(MSG_TEMP, "The door holds firm.");

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		mprint(MSG_WARNING, "You are off-balance.");

		/* Hack -- Lose balance ala paralysis */
		(void) set_paralyzed(p_ptr->paralyzed + 2 + rand_int(2));
	}

	/* Result */
	return (more);
}


/*
 * Bash open a door, success based on character strength
 *
 * For a closed door, pval is positive if locked; negative if stuck.
 *
 * For an open door, pval is positive for a broken door.
 *
 * A closed door can be opened - harder if locked. Any door might be
 * bashed open (and thereby broken). Bashing a door is (potentially)
 * faster! You move into the door way. To open a stuck door, it must
 * be bashed. A closed door can be jammed (see do_cmd_spike()).
 *
 * Creatures can also open or bash doors, see elsewhere.
 */
void do_cmd_bash(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_bash_test(y, x))
		return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);

		/* Done */
		return;

	}
	else if (cave_feat[y][x] >= FEAT_ALTAR_HEAD &&
		cave_feat[y][x] <= FEAT_ALTAR_TAIL)
	{
		more = do_cmd_bash_altar(y, x);
	}

	/* Door */
	else
	{
		/* Bash the door */
		more = do_cmd_bash_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more)
		disturb(0, 0);
}



/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * This command must always take a turn, to prevent free detection
 * of invisible monsters.
 *
 * The "semantics" of this command must be chosen before the player
 * is confused, and it must be verified against the new grid.
 */
void do_cmd_alter(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	int feat;

	bool more = FALSE;


	/* Get a direction */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Original feature */
	feat = cave_feat[y][x];

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK)))
		feat = FEAT_NONE;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
	}

	/* Tunnel through walls */
	else if (feat >= FEAT_SECRET)
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Bash jammed doors */
	else if (feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Tunnel */
		more = do_cmd_bash_aux(y, x);
	}

	/* Open closed doors */
	else if (feat >= FEAT_DOOR_HEAD)
	{
		/* Tunnel */
		more = do_cmd_open_aux(y, x);
	}

	/* Disarm traps */
	else if (feat >= FEAT_TRAP_HEAD)
	{
		/* Tunnel */
		more = do_cmd_disarm_aux(y, x);
	}

	/* Oops */
	else
	{
		/* Oops */
		mprint(MSG_TEMP, "You spin around.");
	}

	/* Cancel repetition unless we can continue */
	if (!more)
		disturb(0, 0);
}



/*
 * Determine if a given grid may be "spiked"
 */
static bool do_cmd_spike_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
			(cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		mprint(MSG_TEMP, "You see nothing there to spike.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	object_type *o_ptr;

	item_tester_tval = TV_SPIKE;
	o_ptr =
		get_item("Use which spike", "You have no spikes!", py, px,
		(USE_INVEN | USE_FLOOR | USE_JUST_ONE | USE_REMOVE));

	if (!o_ptr)
		return;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_spike_test(y, x))
		return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}


	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Go for it */
	else
	{
		/* Verify legality */
		if (!do_cmd_spike_test(y, x))
			return;

		/* Successful jamming */
		msg_print("You jam the door with a spike.");

		/* Convert "locked" to "stuck" XXX XXX XXX */
		if (cave_feat[y][x] < FEAT_DOOR_HEAD + 0x08)
			cave_feat[y][x] += 0x08;

		/* Add one spike to the door */
		if (cave_feat[y][x] < FEAT_DOOR_TAIL)
			cave_feat[y][x]++;

		remove_object(o_ptr);
	}
}



/*
 * Determine if a given grid may be "walked"
 */
bool do_cmd_walk_test(int y, int x)
{
	bool flat = cave_floor_bold(y, x);

	/* Never ever allow someone to walk off the edge of the dungeon! */
	if (!in_bounds_fully(y, x))
	{

		/* Hack -- scroll the wilderness. */
		if (p_ptr->inside_special == SPECIAL_WILD)
		{
			disturb(0, 0);
			p_ptr->leaving = TRUE;
			return FALSE;

		}
		else
		{
			msg_print("There's a solid titanium wall in your way.");
			return FALSE;
		}
	}


	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & CAVE_MARK))
	{
		note_spot(y, x);
		lite_spot(y, x);
	}


	/* if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE); */

	/* Allow pass-thru-wall */

	if (p_ptr->immaterial && cave_feat[y][x] >= FEAT_DOOR_HEAD &&
		cave_feat[y][x] <= FEAT_WALL_SOLID)
	{
		mprint(MSG_TEMP, "You move through the solid wall.");

		return TRUE;
	}

	if (cave_feat[y][x] == FEAT_DEEP_WATER)
	{
		int weight = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100);

		if (p_ptr->flying)
			return TRUE;

		if (p_ptr->total_weight < weight)
		{
			return TRUE;
		}
		else
		{
			mprint(MSG_TEMP, "You can't swim with that much weight.");
			return FALSE;
		}

	}
	else if (cave_feat[y][x] == FEAT_SHAL_LAVA)
	{
		if (p_ptr->flying)
		{

			return TRUE;

		}
		else if (p_ptr->resist_fire || p_ptr->immune_fire ||
			p_ptr->oppose_fire)
		{

			mprint(MSG_TEMP, "You walk through the lava.");
			return TRUE;

		}
		else
		{

			mprint(MSG_TEMP, "You aren't able to resist the heat!");
			return FALSE;

		}

	}
	else if (cave_feat[y][x] == FEAT_DEEP_LAVA ||
		cave_feat[y][x] == FEAT_MOUNTAIN)
	{

		if (p_ptr->flying)
			return TRUE;

		mprint(MSG_TEMP, "You can't move through that!");
		return FALSE;

	}
	else if (flat)
	{

		return TRUE;

	}
	else if (cave_feat[y][x] == FEAT_CHAOS_FOG)
	{
		if (randint(10) == 1)
		{
			mprint(MSG_TEMP, "The fog shifts to block your movement.");

			/* Hack -- consume a turn standing still */
			p_ptr->energy_use = 100;
			return FALSE;
		}

		return TRUE;

	}
	else if (cave_feat[y][x] == FEAT_TREES)
	{

		return TRUE;

	}
	else if ((cave_feat[y][x] >= FEAT_QUEST_ENTER &&
			cave_feat[y][x] <= FEAT_QUEST_EXIT) ||
		cave_feat[y][x] == FEAT_STORE_EXIT)
	{
		return TRUE;

	}
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		mprint(MSG_TEMP, "There is a pile of rubble in the way.");
		return FALSE;

	}
	else if (cave_feat[y][x] < FEAT_SECRET)
	{
		/* Option: Open doors by walking into them */
		if (easy_alter)
			return (TRUE);

		mprint(MSG_TEMP, "There is a door blocking your way.");
		return FALSE;

	}
	else
	{
		mprint(MSG_TEMP, "There is a wall blocking your way.");
		return FALSE;

	}
}

/*
 * Helper function for the "walk" and "jump" commands
 */
static void do_cmd_walk_or_jump(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool did_conf_move = FALSE;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Take a turn */
		p_ptr->energy_use = 100;

		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		did_conf_move = TRUE;
	}

	/* Verify legality */
	if (!do_cmd_walk_test(y, x))
	{
		disturb(0, 0);
		return;
	}

	/* Take a turn if it wasn't done already. */
	if (!did_conf_move)
		p_ptr->energy_use = 100;


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Move the player */
	move_player(dir, pickup);
}


/*
 * Walk into a grid (usually pickup)
 */
void do_cmd_walk(void)
{
	/* Move (usually pickup) */

	if (p_ptr->immovable)
	{
		do_cmd_unwalk();
	}
	else
	{
		do_cmd_walk_or_jump(FALSE);
	}
}

/*
 * Jump into a grid (usually do not pickup)
 */
void do_cmd_jump(void)
{
	/* Move (usually do not pickup) */

	if (p_ptr->immovable)
	{
		do_cmd_unwalk();
	}
	else
	{
		do_cmd_walk_or_jump(TRUE);
	}
}

/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	if (p_ptr->immovable)
	{
		do_cmd_immovable_special();
		return;
	}

	/* Hack XXX XXX XXX */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir))
		return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x))
	{
		disturb(0, 0);
		return;
	}

	/* Start run */
	run_step(dir);
}


void check_store_entering(s16b py, s16b px)
{
	/* Hack -- enter a store if we are on one */
	if ((cave_feat[py][px] >= FEAT_SHOP_HEAD &&
			cave_feat[py][px] <= FEAT_SHOP_TAIL) ||
		cave_feat[py][px] == FEAT_STORE_EXIT)
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';
	}

	/* Hack -- enter a building if we are on one -KMW- */
	if ((cave_feat[py][px] >= FEAT_BLDG_HEAD) &&
		(cave_feat[py][px] <= FEAT_BLDG_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter building */
		p_ptr->command_new = ']';
	}

	/* Exit a quest if reach the quest exit -KMW */
	if (cave_feat[py][px] == FEAT_QUEST_EXIT)
	{
		exit_quest();
	}
}

/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;


	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

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
	py_pickup(pickup);

	check_store_entering(py, px);

}


/*
 * Hold still (usually pickup)
 */
void do_cmd_hold(void)
{
	/* Hold still (usually pickup) */
	if (p_ptr->immovable)
	{
		do_cmd_unwalk();
	}
	else
	{
		do_cmd_hold_or_stay(always_pickup);
	}
}

/*
 * Stay still (usually do not pickup)
 */
void do_cmd_stay(void)
{
	/* Stay still (usually do not pickup) */

	if (p_ptr->immovable)
	{
		do_cmd_hold_or_stay(TRUE);
	}
	else
	{
		do_cmd_hold_or_stay(!always_pickup);
	}
}

/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";

		char out_val[80];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, 4))
			return;

		/* Rest until done */
		if (out_val[0] == '&')
		{
			p_ptr->command_arg = (-2);
		}

		/* Rest a lot */
		else if (out_val[0] == '*')
		{
			p_ptr->command_arg = (-1);
		}

		/* Rest some */
		else
		{
			p_ptr->command_arg = atoi(out_val);
			if (p_ptr->command_arg <= 0)
				return;
		}
	}


	/* Paranoia */
	if (p_ptr->command_arg > 9999)
		p_ptr->command_arg = 9999;


	/* Take a turn XXX XXX XXX (?) */
	p_ptr->energy_use = 100;

	/* Save the rest code. */
	p_ptr->resting = p_ptr->command_arg;

	/* Save the turn counter. */
	old_resting_turn = turn;

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
void do_cmd_fire(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;
	int y, x, ny, nx, ty, tx;
	int tdam, tdis, thits, tmul;
	int bonus, chance;
	int cur_dis, visible;

	object_type *o_ptr;
	object_type *j_ptr;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Inside arena */
	if (p_ptr->inside_special == SPECIAL_ARENA)
	{
		msg_print("You're in the arena now. This is hand-to-hand!");
		msg_print(NULL);
		return;
	}

	/* Magical Arena. */
	if (p_ptr->inside_special == SPECIAL_MAGIC_ARENA)
	{
		mprint(MSG_TEMP,
			"You're in the magical arena, archery is forbidden!");
		msg_print(NULL);
		return;
	}

	/* Get the "bow" (if any) */
	j_ptr = equipment[EQUIP_BOW];

	/* Require a usable launcher */
	if (!j_ptr || !p_ptr->ammo_tval)
	{
		mprint(MSG_TEMP, "You have nothing to fire with.");
		return;
	}


	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	o_ptr =
		get_item("Fire which item", "You have nothing to fire.", p_ptr->py,
		p_ptr->px, (USE_INVEN | USE_FLOOR | USE_JUST_ONE | USE_REMOVE));


	if (!o_ptr)
		return;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir))
	{
		inven_carry(o_ptr);
		return;
	}

	/* Sound */
	sound(SOUND_SHOOT);


	/* Describe the object */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(o_ptr);
	missile_char = object_char(o_ptr);


	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Use a base distance */
	tdis = 10;

	/* Base damage from thrown object plus launcher bonus */
	tdam = damroll(o_ptr->dd, o_ptr->ds) + o_ptr->to_d + j_ptr->to_d;

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h + o_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range */
	tdis = 10 + 5 * tmul;

	/* Take a (partial) turn */
	p_ptr->energy_use = (100 / thits);


	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}


	/* Hack -- Handle stuff */
	handle_stuff();


	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis;)
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx))
			break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stopped by walls/doors */
		if (!cave_floor_bold(ny, nx))
			break;

		/* Advance the distance */
		cur_dis++;

		/* Save the new location */
		x = nx;
		y = ny;


		/* The player can see the (on screen) missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}


		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance - cur_dis, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
					(r_ptr->flags3 & (RF3_UNDEAD)) ||
					(r_ptr->flags2 & (RF2_STUPID)) ||
					(strchr("Evg", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}


				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml)
						monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml)
						health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(o_ptr, tdam, m_ptr);
				tdam = critical_shot(o_ptr->weight, o_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0)
					tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.", tdam,
						m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies,
						TRUE, FALSE))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

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

					if (m_ptr->is_pet)
					{
						hostile_monsters(cave_m_idx[y][x]);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Drop (or break) near that location */
	drop_near(o_ptr, TRUE, y, x);
}


static bool item_tester_hook_throw(object_type * o_ptr)
{
	int i;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i] == o_ptr && (cursed_p(o_ptr) ||
				show_inven_equip == FALSE))
			return FALSE;
	}

	return TRUE;
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
void do_cmd_throw(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;
	int y, x, ny, nx, ty, tx;
	int chance, tdam, tdis;
	int mul, div;
	int cur_dis, visible;

	object_type *o_ptr;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Some objects are very easy to throw -- namely potions/flasks */
	bool easy = FALSE;

	/* Inside arena */
	if (p_ptr->inside_special == SPECIAL_ARENA)
	{
		mprint(MSG_TEMP, "You're in the arena now. This is hand-to-hand!");
		msg_print(NULL);
		return;
	}

	/* Magical Arena. */
	if (p_ptr->inside_special == SPECIAL_MAGIC_ARENA)
	{
		mprint(MSG_TEMP,
			"You're in the magical arena, throwing is forbidden!");
		msg_print(NULL);
		return;
	}

	item_tester_hook = item_tester_hook_throw;

	/* Get an item */
	o_ptr =
		get_item("Throw which item", "You have nothing to throw",
		p_ptr->py, p_ptr->px,
		(USE_INVEN | USE_FLOOR | USE_JUST_ONE | USE_REMOVE));


	if (!o_ptr)
		return;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir))
	{
		inven_carry(o_ptr);
		return;
	}

	/* Set the ``easy'' flag. */
	if (o_ptr->tval == TV_POTION || o_ptr->tval == TV_FLASK)
	{
		easy = TRUE;
	}

	/* Description */
	object_desc(o_name, o_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(o_ptr);
	missile_char = object_char(o_ptr);


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((o_ptr->weight > 10) ? o_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10)
		tdis = 10;

	/* Easy objects can always be thrown relatively far. */
	if (easy && tdis < 3)
		tdis = 3;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(o_ptr->dd, o_ptr->ds) + o_ptr->to_d;

	/* Chance of hitting */
	if (easy)
	{
		chance = 100;
	}
	else
	{
		chance = (p_ptr->skill_tht + (p_ptr->to_h * BTH_PLUS_ADJ));
	}


	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Start at the player */
	y = py;
	x = px;

	/* Predict the "target" location */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}


	/* Hack -- Handle stuff */
	handle_stuff();


	/* Travel until stopped */
	for (cur_dis = 0; cur_dis <= tdis;)
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx))
			break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stopped by walls/doors */
		if (!cave_floor_bold(ny, nx))
			break;

		/* Advance the distance */
		cur_dis++;

		/* Save the new location */
		x = nx;
		y = ny;


		/* The player can see the (on screen) missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}


		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{

			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* ``Easy'' missiles don't do any damage. */
			if (easy)
				break;

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance - cur_dis, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
					(r_ptr->flags3 & (RF3_UNDEAD)) ||
					(r_ptr->flags2 & (RF2_STUPID)) ||
					(strchr("Evg", r_ptr->d_char)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}


				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					msg_format("The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml)
						monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml)
						health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(o_ptr, tdam, m_ptr);
				tdam = critical_shot(o_ptr->weight, o_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0)
					tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.", tdam,
						m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies,
						TRUE, FALSE))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);

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


					if (m_ptr->is_pet)
					{
						hostile_monsters(cave_m_idx[y][x]);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Drop (or break) near that location */
	drop_near(o_ptr, TRUE, y, x);
}


/*
 * Try to ``walk'' using phase door.
 */

void do_cmd_unwalk(void)
{
	int dir, y, x, feat;
	int py = p_ptr->py;
	int px = p_ptr->px;

	bool more = FALSE;

	if (!get_rep_dir(&dir))
		return;

	y = py + ddy[dir];
	x = px + ddx[dir];

	feat = cave_feat[y][x];

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK)))
		feat = FEAT_NONE;

	/* Take a turn */
	p_ptr->energy_use = 100;


	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}

	/* Allow repeated command */
	if (p_ptr->command_arg)
	{
		/* Set repeat count */
		p_ptr->command_rep = p_ptr->command_arg - 1;

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);

		/* Cancel the arg */
		p_ptr->command_arg = 0;
	}


	/* Attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
	}

	/* Hack -- Ignore weird terrain types. */
	else if (feat >= FEAT_PERM_EXTRA && feat != FEAT_TREES)
	{
		teleport_player(10);
	}

	/* Tunnel through walls */
	else if (feat >= FEAT_SECRET)
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Bash jammed doors */
	else if (feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Tunnel */
		more = do_cmd_bash_aux(y, x);
	}

	/* Open closed doors */
	else if (feat >= FEAT_DOOR_HEAD)
	{
		/* Tunnel */
		more = do_cmd_open_aux(y, x);
	}

	/* Disarm traps */
	else if (feat >= FEAT_TRAP_HEAD)
	{
		/* Tunnel */
		more = do_cmd_disarm_aux(y, x);
	}

	/* Walking semantics */
	else
	{
		teleport_player_directed(10, dir);
	}

	/* Cancel repetition unless we can continue */
	if (!more)
		disturb(0, 0);
}
