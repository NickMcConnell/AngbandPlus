/* File: cmd-misc.c */

/*
 * Commands that didn't fit anywhere else.
 *
 * Copyright (c) 1999 Leon Marrick, Ben Harrison, James E. Wilson, 
 * Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set_interactive(TARGET_KILL))
	{
		message(MSG_GENERIC, 0, "Target Selected.");
	}

	/* Target aborted */
	else
	{
		message(MSG_GENERIC, 0, "Target Aborted.");
	}
}

/*
 * Look command
 */
void do_cmd_look(void)
{
	/* Look around */
	if (target_set_interactive(TARGET_LOOK))
	{
		message(MSG_GENERIC, 0, "Target Selected.");
	}
}

/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
	int dir, y1, x1, y2, x2;

	char tmp_val[80];

	char out_val[160];

	/* Start at current panel */
	y2 = y1 = p_ptr->wy;
	x2 = x1 = p_ptr->wx;

	/* Show panels until done */
	while (TRUE)
	{
		/* Describe the location */
		if ((y2 == y1) && (x2 == x1))
		{
			tmp_val[0] = '\0';
		}
		else
		{
			sprintf(tmp_val, "%s%s of",
			        ((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
			        ((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
		}

		/* Prepare to ask which way to look */
		sprintf(out_val,
		        "Map sector [%d,%d], which is%s your sector.  Direction?",
		        (y2 / PANEL_HGT), (x2 / PANEL_WID), tmp_val);

		/* More detail */
		if (center_player)
		{
			sprintf(out_val,
		        	"Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
		        	(y2 / PANEL_HGT), (y2 % PANEL_HGT),
		        	(x2 / PANEL_WID), (x2 % PANEL_WID), tmp_val);
		}

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command)) break;

			/* Extract direction */
			dir = target_dir(command);

			/* Error */
			if (!dir) bell("Illegal direction for locate!");
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		y2 += (ddy[dir] * PANEL_HGT);
		x2 += (ddx[dir] * PANEL_WID);

		/* Verify the row */
		if (y2 < 0) y2 = 0;
		if (y2 > p_ptr->cur_hgt - SCREEN_HGT) y2 = p_ptr->cur_hgt - SCREEN_HGT;

		/* Verify the col */
		if (x2 < 0) x2 = 0;
		if (x2 > p_ptr->cur_wid - SCREEN_WID) x2 = p_ptr->cur_wid - SCREEN_WID;

		/* Handle "changes" */
		if ((p_ptr->wy != y2) || (p_ptr->wx != x2))
		{
			/* Update panel */
			p_ptr->wy = y2;
			p_ptr->wx = x2;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Handle stuff */
			handle_stuff();
		}
	}

	/* Verify panel */
	p_ptr->update |= (PU_PANEL);

	/* Handle stuff */
	handle_stuff();
}

/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
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
static void chest_death(int y, int x, s16b o_idx)
{
	int number;

	bool tiny;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	/* Get the chest */
	o_ptr = &o_list[o_idx];

	/* Small chests often hold "gold" */
	tiny = (o_ptr->sval < SV_CHEST_MIN_LARGE);

	/* Determine how much to drop (see above) */
	number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Determine the "value" of the items */
	p_ptr->obj_depth = ABS(o_ptr->pval) + 10;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Small chests often drop gold */
		if (tiny && (rand_int(100) < 75))
		{
			/* Make some gold */
			if (!make_gold(i_ptr, 0)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, FALSE, FALSE, TRUE)) continue;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Reset the object level */
	p_ptr->obj_depth  = p_ptr->depth;

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
	int i, trap;

	object_type *o_ptr = &o_list[o_idx];

	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the traps */
	trap = chest_traps[o_ptr->pval];

	/* Lose strength */
	if (trap & (CHEST_LOSE_STR))
	{
		message(MSG_TRAP, (s16b)trap, "A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_STR, 10, FALSE, TRUE);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		message(MSG_TRAP, (s16b)trap, "A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_CON, 10, FALSE, TRUE);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		message(MSG_TRAP, (s16b)trap, "A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void)set_poisoned(p_ptr->poisoned + 10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		message(MSG_TRAP, (s16b)trap, "A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + 10 + randint(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		message(MSG_TRAP, (s16b)trap, "You are enveloped in a cloud of smoke!");
		for (i = 0; i < num; i++)
		{
			(void)summon_specific(y, x, p_ptr->depth, 0);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		message(MSG_TRAP, (s16b)trap, "There is a sudden explosion!");
		message(MSG_TRAP, -1, "Everything inside the chest is destroyed!");
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
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
	int i, j;

	bool flag = TRUE;
	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];

	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->skill[SK_DIS];

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
			message(MSG_LOCKPICK_SUCCEED, 0, "You have picked the lock.");
			gain_exp(1);
			flag = TRUE;
		}

		/* Failure -- Keep trying */
		else
		{
			/* We may continue repeating */
			more = TRUE;
			if (flush_failure) flush();
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");
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
 * Attempt to disarm the chest at the given location
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

	/* Get the "disarm" factor */
	i = p_ptr->skill[SK_DIS];

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
		message(MSG_FAIL, 0, "I don't see any traps.");
	}

	/* Already disarmed/unlocked */
	else if (o_ptr->pval <= 0)
	{
		message(MSG_FAIL, 0, "The chest is not trapped.");
	}

	/* No traps to find. */
	else if (!chest_traps[o_ptr->pval])
	{
		message(MSG_FAIL, 0, "The chest is not trapped.");
	}

	/* Success (get a lot of experience) */
	else if (rand_int(100) < j)
	{
		message(MSG_LOCKPICK_SUCCEED, 0, "You have disarmed the chest.");
		gain_exp(o_ptr->pval);
		o_ptr->pval = (0 - o_ptr->pval);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* We may keep trying */
		more = TRUE;
		if (flush_failure) flush();
		message(MSG_LOCKPICK_FAIL, 0, "You failed to disarm the chest.");
	}

	/* Failure -- Set off the trap */
	else
	{
		message(MSG_LOCKPICK_FAIL, 0, "You set off a trap!");
		chest_trap(y, x, o_idx);
	}

	/* Result */
	return (more);
}

/*
 * Return TRUE if the given feature is an open (or broken) door
 */
static bool is_open(int feat)
 {
	return ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN));
}

/*
 * Return TRUE if the given feature is a closed door
 */
static bool is_closed(int feat)
{
	return ((feat >= FEAT_DOOR_HEAD) &&
	        (feat <= FEAT_DOOR_TAIL));
}

/*
 * Return TRUE if the given feature is a trap
 */
static bool is_trap(int feat)
{
	return ((feat >= FEAT_TRAP_HEAD) &&
	        (feat <= FEAT_TRAP_TAIL));
}

/*
 * Return the number of doors/traps around (or under) the character.
 */
static int count_feats(int *y, int *x, bool (*test)(int feat), bool under)
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
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;
 
 		/* Must have knowledge */
 		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;
 
 		/* Not looking for this feature */
		if (!((*test)(cave_feat[yy][xx]))) continue;
 
 		/* Count it */
 		++count;

		/* Remember the location of the last door found */
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
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* No (visible) chest is there */
		if ((o_idx = chest_check(yy, xx)) == 0) continue;

		/* Grab the object */
		o_ptr = &o_list[o_idx];

		/* Already open */
		if (o_ptr->pval == 0) continue;

		/* No (known) traps here */
		if (trapped && (!object_known_p(o_ptr) ||
		     (o_ptr->pval < 0) || !chest_traps[o_ptr->pval]))
		{
			continue;
		}

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
 * Extract a "direction" which will move one step from the player location
 * towards the given "target" location (or "5" if no motion necessary).
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to open.");

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
	if (!do_cmd_open_test(y, x)) return (FALSE);

	/* Jammed door */
	if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		message(MSG_FAIL, 0, "The door appears to be stuck.");
	}

	/* Locked door */
	else if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill[SK_DIS];

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = cave_feat[y][x] - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			message(MSG_OPENDOOR, 0, "You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");

			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		cave_set_feat(y, x, FEAT_OPEN);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_OPENDOOR);
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
	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	/* Easy Open */
	if (easy_direction)
	{
		int num_doors, num_chests;

		/* Count closed doors */
		num_doors = count_feats(&y, &x, is_closed, FALSE);

		/* Count chests (locked) */
		num_chests = count_chests(&y, &x, FALSE);

		/* See if only one target */
		if ((num_doors + num_chests) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);

	/* Verify legality */
	if (!o_idx && !do_cmd_open_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* Check for chest */
		o_idx = chest_check(y, x);
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
		message(MSG_FAIL, 0, "There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Chest */
	else if (o_idx)
	{
		/* Open the chest */
		more = do_cmd_open_chest(y, x, o_idx);
	}

	/* Door */
	else
	{
		/* Open the door */
		more = do_cmd_open_aux(y, x);
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0);
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

 	/* Require open/broken door */
	if ((cave_feat[y][x] != FEAT_OPEN) &&
	    (cave_feat[y][x] != FEAT_BROKEN))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to close.");

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
	if (!do_cmd_close_test(y, x)) return (FALSE);

	/* Broken door */
	if (cave_feat[y][x] == FEAT_BROKEN)
	{
		/* Message */
		message(MSG_FAIL, 0, "The door appears to be broken.");
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_SHUTDOOR);
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

	bool more = FALSE;

	/* Easy Close */
	if (easy_direction)
	{
		/* Count open doors */
		if (count_feats(&y, &x, is_open, FALSE) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_close_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
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
		message(MSG_FAIL, 0, "There is a monster in the way!");

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
	if (!more) disturb(0);
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(y, x))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to tunnel.");

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
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Sound */
	sound(MSG_DIG);

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Result */
	return (TRUE);
}

/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Uses "twall" (above) to do all "terrain feature changing".
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	bool more = FALSE;

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Titanium */
	if (cave_feat[y][x] >= FEAT_PERM_EXTRA)
	{
		message(MSG_FAIL, 0, "This seems to be permanent rock.");
	}

	/* Granite */
	else if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
	{
		/* Tunnel */
		if ((p_ptr->skill[SK_DIG] > 40 + rand_int(1600)) && twall(y, x))
		{
			message(MSG_DIG, 0, "You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			message(MSG_DIG, 0, "You tunnel into the granite wall.");
			more = TRUE;
		}
	}

	/* Quartz / Magma */
	else if (cave_feat[y][x] >= FEAT_MAGMA)
	{
		bool okay = FALSE;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (cave_feat[y][x] >= FEAT_MAGMA_H)
		{
			gold = TRUE;
		}

		/* Extract "quartz" flag XXX XXX XXX */
		if ((cave_feat[y][x] - FEAT_MAGMA) & 0x01)
		{
			hard = TRUE;
		}

		/* Quartz */
		if (hard)
		{
			okay = (p_ptr->skill[SK_DIG] > 20 + rand_int(800));
		}

		/* Magma */
		else
		{
			okay = (p_ptr->skill[SK_DIG] > 10 + rand_int(400));
		}

		/* Success */
		if (okay && twall(y, x))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(y, x);

				/* Message */
				message(MSG_DIG, 0, "You have found something!");
			}

			/* Found nothing */
			else
			{
				/* Message */
				message(MSG_DIG, 0, "You have finished the tunnel.");
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
			message(MSG_DIG, 0, "You tunnel into the quartz vein.");
			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
			message(MSG_DIG, 0, "You tunnel into the magma vein.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skill[SK_DIG] > rand_int(200)) && twall(y, x))
		{
			/* Message */
			message(MSG_DIG, 0, "You have removed the rubble.");

			/* Hack -- place an object */
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE);

				/* Observe new object */
				if (player_can_see_bold(y, x))
				{
					message(MSG_DIG, 0, "You have found something!");
				}
			}
		}

		else
		{
			/* Message, keep digging */
			message(MSG_DIG, 0, "You dig in the rubble.");
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (cave_feat[y][x] >= FEAT_SECRET)
	{
		/* Tunnel */
		if ((p_ptr->skill[SK_DIG] > 30 + rand_int(1200)) && twall(y, x))
		{
			message(MSG_DIG, 0, "You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			message(MSG_DIG, 0, "You tunnel into the granite wall.");
			more = TRUE;

			/* Occasional Search XXX XXX */
			if (rand_int(100) < 25) search();
		}
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if ((p_ptr->skill[SK_DIG] > 30 + rand_int(1200)) && twall(y, x))
		{
			message(MSG_DIG, 0, "You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			message(MSG_DIG, 0, "You tunnel into the door.");
			more = TRUE;
		}
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
	int y, x, dir;

	bool more = FALSE;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Oops */
	if (!do_cmd_tunnel_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
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
		message(MSG_FAIL, 0, "There is a monster in the way!");

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
	if (!more) disturb(0);
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require an actual trap */
	if (!((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
	      (cave_feat[y][x] <= FEAT_TRAP_TAIL)))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to disarm.");

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
	if (!do_cmd_disarm_test(y, x)) return (FALSE);

	/* Get the trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill[SK_DIS];

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power" */
	power = 5;

	/* Extract the difficulty */
	j = i - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Success */
	if (rand_int(100) < j)
	{
		/* Message */
		message_format(MSG_LOCKPICK_SUCCEED, 0, "You have disarmed the %s.", name);

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
		if (flush_failure) flush();

		/* Message */
		message_format(MSG_LOCKPICK_FAIL, 0, "You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		message_format(MSG_LOCKPICK_FAIL, 0, "You set off the %s!", name);

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
	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

	/* Easy Disarm */
	if (easy_direction)
	{
		int num_traps, num_chests;

		/* Count visible traps */
		num_traps = count_feats(&y, &x, is_trap, TRUE);

		/* Count chests (trapped) */
		num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if (num_traps || num_chests)
 		{
			if (num_traps + num_chests <= 1)
				p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);

	/* Verify legality */
	if (!o_idx && !do_cmd_disarm_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* Check for chests */
		o_idx = chest_check(y, x);
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
		message(MSG_FAIL, 0, "There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Chest */
	else if (o_idx)
	{
		/* Disarm the chest */
		more = do_cmd_disarm_chest(y, x, o_idx);
	}

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_cmd_disarm_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0);
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to bash.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
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
	if (!do_cmd_bash_test(y, x)) return (FALSE);

	/* Message */
	message(MSG_BASH, 0, "You smash into the door!");

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = ((cave_feat[y][x] - FEAT_DOOR_HEAD) & 0x07);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
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

		/* Message */
		message(MSG_OPENDOOR, 0, "The door crashes open!");

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
	         p_ptr->lev)
	{
		/* Message */
		message(MSG_FAIL, 0, "The door holds firm.");

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		message(MSG_EFFECT, 0, "You are off-balance.");

		/* Hack -- Lose balance ala paralysis */
		(void)set_paralyzed(p_ptr->paralyzed + 2 + rand_int(2));
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
	int y, x, dir;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Verify legality */
	if (!do_cmd_bash_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
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
		message(MSG_FAIL, 0, "There is a monster in the way!");

		/* Attack */
		py_attack(y, x);
	}

	/* Door */
	else
	{
		/* Bash the door */
		if (!do_cmd_bash_aux(y, x))
		{
			/* Cancel repeat */
			disturb(0);
		}
	}
}

/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 *
 * The "semantics" of this command must be chosen before the player
 * is confused, and it must be verified against the new grid.
 */
void do_cmd_alter(void)
{
	int y, x, dir;

	int feat;

	bool more = FALSE;

	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];

	/* Original feature */
	feat = cave_feat[y][x];

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) feat = FEAT_NONE;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
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
		message(MSG_FAIL, 0, "You spin around.");
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0);
}

/*
 * Use a racial ability
 */
void do_cmd_use_racial(void)
{
	int dir;

	if ((!rp_ptr->special) || (rsp_ptr[p_ptr->max_lev/5]->power == 0))
	{
		message(MSG_FAIL, 0, "You have no racial powers!");
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
		message(MSG_FAIL, 0, "You are too confused!");
		return;
	}

	/* Not if still recharging */
	if (p_ptr->racial_power)
	{
		message(MSG_FAIL, 0, "You have to collect your energy");
		return;
	}

	switch (rsp_ptr[p_ptr->max_lev/5]->power)
	{
		/*Angel Powers*/
		case 1: /* Cherub - Detect Evil */
		{
			(void)detect_monsters_evil();
			p_ptr->racial_power=50;
			break;
		}			

		case 2: /* Seraph - Light area */
		{
			(void)lite_area(damroll(2,2),2);
			p_ptr->racial_power=50;
			break;
		}			

		case 3: /* Deva/Planeter - Spear of Light */
		{
			if (!get_aim_dir(&dir)) return;
			message(MSG_EFFECT, 0, "A line of blue shimmering light appears.");
			lite_line(dir,damroll(6,8));
			p_ptr->racial_power=25;
			break;
		}			

		case 4: /* Archon - Orb of Draining */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_HOLY_ORB, dir, damroll(3, 6)+50, 3);
			p_ptr->racial_power=15;
			break;
		}			

		case 5: /* Angel/Archangel - Protection From Evil */
		{
			(void)set_protevil(p_ptr->protevil + randint(25) + 30);
			p_ptr->racial_power=150;
			break;
		}	
		
		/*Demon Powers*/

		case 6: /* Tengu - Blink */
		{
			teleport_player(10);
			p_ptr->racial_power=20;
			break;
		}

		case 7: /* Bodak/Vrock/Hezrou - Fire Bolt */
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(0, GF_FIRE, dir, damroll(10, 8));
			p_ptr->racial_power=20;
			break;
		}			

		case 8: /* Glabrezu/Nalfeshnee/Pit Fiend - Fire Ball */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir, 85, 2);
			p_ptr->racial_power=20;
			break;
		}			

		case 9: /* Balrog - Plasma ball */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_PLASMA, dir, damroll(4, 7)+80, 3);
			p_ptr->racial_power=20;
			break;
		}			

	}
}

/*
 * Display a "small-scale" map of the dungeon.
 *
 * Note that the "player" is always displayed on the map.
 */
void do_cmd_view_map(void)
{
	int cy, cx;

	/* Save screen */
	screen_save();

	/* Note */
	prt("Please wait...", 0, 0);

	/* Flush */
	Term_fresh();

	/* Clear the screen */
	Term_clear();

	/* Display the map */
	display_map(&cy, &cx);

	/* Wait for it */
	put_str("Hit any key to continue", 23, 23);

	/* Hilite the player */
	Term_gotoxy(cx, cy);

	/* Get any key */
	(void)inkey();

	/* Load screen */
	screen_load();
}
