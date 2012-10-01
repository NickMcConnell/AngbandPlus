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
	return (feat == FEAT_CLOSED);
}

/*
 * Return the number of doors around (or under) the character.
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
	if (cave_feat[y][x] != FEAT_CLOSED)
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
	bool more = FALSE;

	/* Verify legality */
	if (!do_cmd_open_test(y, x)) return (FALSE);

	/* Locked door */
	if (trap_lock(y, x) && trap_player(y, x)) 
	{
		int i, j;

		trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

		/* Disarm factor */
		i = p_ptr->skill[SK_DIS];

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = t_ptr->charges;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			message(MSG_OPENDOOR, 0, "You have picked the lock.");

			/* Delete the lock */
			delete_trap(y, x);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			message(MSG_DISARM_FAIL, 0, "You failed to pick the lock.");

			/* Can keep trying */
			return (TRUE);
		}
	}
	/* Non-player locks */
	else if (trap_lock(y, x))
	{
		/* Delete the lock */
		delete_trap(y, x);
	}

	/* Open the door */
	cave_set_feat(y, x, FEAT_OPEN);

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Sound */
	sound(MSG_OPENDOOR);

	/* Result */
	return (more);
}

/*
 * Open a closed/locked/jammed door 
 *
 * Unlocking a locked door is worth one experience point.
 */
void do_cmd_open(void)
{
	int y, x, dir;

	bool more = FALSE;

	/* Easy Open */
	if (easy_direction)
	{
		int num_doors;

		/* Count closed doors */
		num_doors = count_feats(&y, &x, is_closed, FALSE);

		/* See if only one target */
		if (num_doors == 1)
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
	if (!do_cmd_open_test(y, x)) return;

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
		cave_set_feat(y, x, FEAT_CLOSED);

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

	/* If there's a lock, delete it */
	if (trap_lock(y, x)) delete_trap(y, x);

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
	else if (cave_feat[y][x] == FEAT_SECRET)
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
	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* No visible trap */
	if (!trap_disarmable(y, x))
	{
		message(MSG_FAIL, 0, "There's nothing there to disarm!");
		return (FALSE);
	}

	/* Legal */
	return (TRUE);
}

/*
 * Return the number of doors around (or under) the character.
 */
static int count_traps(int *y, int *x)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */
 
 	/* Check around (and under) the character */
 	for (d = 0; d < 9; d++)
 	{
 		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;
 
 		/* Must have knowledge */
 		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;
 
 		/* Not a disarmable trap */
		if (!trap_disarmable(yy, xx)) continue;
 
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
 * Disarms a trap
 */
void do_cmd_disarm(void)
{
	int y, x, dir;

	bool more = FALSE;

	/* Easy Disarm */
	if (easy_direction)
	{
		int num_traps;

		/* Count closed doors */
		num_traps = count_traps(&y, &x);

		/* See if only one target */
		if (num_traps == 1)
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
	if (!do_cmd_disarm_test(y, x)) return;

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

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_disarm_trap(y, x);
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
	if (cave_feat[y][x] != FEAT_CLOSED) 
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
	bash = adj_str_blow[p_stat(A_STR)];

	/* Extract door power !!!!! */
	temp = 1;

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

		/* Remove the lock */
		delete_trap(y, x);

		/* Message */
		message(MSG_OPENDOOR, 0, "The door crashes open!");

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_stat(A_DEX)] + p_ptr->lev)
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
	else if ((feat >= FEAT_SECRET) && (feat <= FEAT_PERM_SOLID))
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Open closed doors */
	else if (feat == FEAT_CLOSED)
	{
		/* Tunnel */
		more = do_cmd_open_aux(y, x);
	}

	/* Disarm traps */
	else if (trap_disarmable(y, x))
	{
		more = do_disarm_trap(y, x);
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
		message(MSG_FAIL, 0, "You have to collect your energy.");
		return;
	}

	switch (rsp_ptr[p_ptr->max_lev/5]->power)
	{
		/*Angel Powers*/
		case 1: /* Cherub - Detect Evil */
		{
			(void)detect_monsters_evil();
			p_ptr->racial_power = 50;
			break;
		}			

		case 2: /* Seraph - Light area */
		{
			lite_area(damroll(2, 2), 2);
			p_ptr->racial_power = 50;
			break;
		}			

		case 3: /* Deva/Planeter - Spear of Light */
		{
			if (!get_aim_dir(&dir)) return;
			message(MSG_EFFECT, 0, "A line of blue shimmering light appears.");
			lite_line(dir, damroll(6, 8));
			p_ptr->racial_power = 25;
			break;
		}			

		case 4: /* Archon - Orb of Draining */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_HOLY_ORB, dir, damroll(3, 6) + 50, 3);
			p_ptr->racial_power = 15;
			break;
		}			

		case 5: /* Angel/Archangel - Protection From Evil */
		{
			(void)set_protevil(p_ptr->protevil + randint(25) + 30);
			p_ptr->racial_power = 150;
			break;
		}	
		
		/*Demon Powers*/

		case 6: /* Tengu - Blink */
		{
			teleport_player(10);
			p_ptr->racial_power = 20;
			break;
		}

		case 7: /* Bodak/Vrock/Hezrou - Fire Bolt */
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt(GF_FIRE, dir, damroll(10, 8));
			p_ptr->racial_power = 20;
			break;
		}			

		case 8: /* Glabrezu/Nalfeshnee/Pit Fiend - Fire Ball */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir, 85, 2);
			p_ptr->racial_power = 20;
			break;
		}			

		case 9: /* Balrog - Plasma ball */
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_PLASMA, dir, damroll(4, 7) + 80, 3);
			p_ptr->racial_power = 20;
			break;
		}			
	}

	/* Take a turn */
	p_ptr->energy_use = 100;
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

/*
 * Place trap
 */
void do_cmd_place_trap(void)
{
	if (!(cp_ptr->flags & CF_TRAP_PLACE))
	{
		message (MSG_FAIL, 0, "You lack the knowledge to place traps!");
		return;
	}

	place_trap_player(p_ptr->py, p_ptr->px);

	/* Take a turn */
	p_ptr->energy_use = 100;
}
