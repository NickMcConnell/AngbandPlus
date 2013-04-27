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
 * Go up one level					-RAK-
 */
void do_cmd_go_up(void)
{
	bool go_up = FALSE;
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Verify stairs */
	if (cave_feat[py][px] != FEAT_LESS)
	{
		msg_print("There is no up staircase here.");
		return;
	}

	else
	{
		if (!p_ptr->depth)
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
			energy_use = 0;

			/* Success */
			msg_print("You enter a maze of up staircases.");

			if (autosave_l)
			{
				is_autosave = TRUE;
				msg_print("Autosaving the game...");
				do_cmd_save_game();
				is_autosave = FALSE;
			}

			/* Go up the stairs */
			p_ptr->depth--;
			new_level_flag = TRUE;

			/* Create a way back */
			create_down_stair = TRUE;
		}
	}
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	bool go_down = FALSE;
	bool fall_trap = FALSE;
	int py = p_ptr->py;
	int px = p_ptr->px;

	if (cave_feat[py][px] == (FEAT_TRAP_HEAD + 0x00)) fall_trap = TRUE;


	/* Verify stairs */
	if ((cave_feat[py][px] != FEAT_MORE) && !(fall_trap))
	{
		msg_print("There is no down staircase here.");
		return;
	}
	else
	{
		if (!p_ptr->depth)
		{
			go_down = TRUE;
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

			if (fall_trap)
				msg_print("You deliberately jump through the trap door.");
			else
				/* Success */
				msg_print("You enter a maze of down staircases.");

			if (autosave_l)
			{
				is_autosave = TRUE;
				msg_print("Autosaving the game...");
				do_cmd_save_game();
				is_autosave = FALSE;
			}

			/* Go down */
			p_ptr->depth++;
			new_level_flag = TRUE;

			if (!fall_trap)
			{
				/* Create a way back */
				create_up_stair = TRUE;
			}
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
	s16b this_o_idx, next_o_idx;


	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
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
		if (small && (rand_int(100) < 75))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(q_ptr, FALSE, FALSE)) continue;
		}

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, y, x);
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
			if (randint(100)<p_ptr->depth)
			{
				activate_hi_summon();
			}
			else
			{
				(void)summon_specific(y, x, p_ptr->depth + 5, SUMMON_MONSTER, TRUE, FALSE);
			}
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
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
		if (p_ptr->blind || player_has_no_lite) i = i / 10;
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
 * Perform the basic "open" command on doors
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x, int dir)
{
	int i, j;
	byte feat = cave_feat[y][x];

	bool more = FALSE;

	/* Take a turn */
	energy_use = 100;

	/* Jammed door */
	if (feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked door */
	else if (feat >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || player_has_no_lite) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = feat - FEAT_DOOR_HEAD;

		/* Extract the difficulty XXX XXX XXX */
		j = i - (j * 4);

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success */
		if (rand_int(100) < j)
		{
			/* Message */
			msg_print("You have picked the lock.");

			/* Open the door */
			draw_grid_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			msg_print("You failed to pick the lock.");

			/* We may keep trying */
			more = TRUE;
		}
	}

	/* Closed door */
	else
	{
		/* Open the door */
		draw_grid_feat(y, x, FEAT_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_MONSTERS);

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
	int y, x, dir;
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b o_idx;

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
	if (get_rep_dir(&dir))
	{
		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chest */
		o_idx = chest_check(y, x);

		/* Nothing useful */
		if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
				(cave_feat[y][x] <= FEAT_DOOR_TAIL)) &&
			 !o_idx)
		{
			/* Message */
			msg_print("You see nothing there to open.");
		}

		/* Monster in the way */
		else if (cave_monster_bold(y, x))
		{
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
			more = do_cmd_open_aux(y, x, dir);
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
static bool do_cmd_close_aux(int y, int x, int dir)
{
	bool		more = FALSE;

	/* Take a turn */
	energy_use = 100;

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
		draw_grid_feat(y, x, FEAT_DOOR_HEAD + 0x00);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_MONSTERS);

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
	int py = p_ptr->py;
	int px = p_ptr->px;

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
	if (get_rep_dir(&dir))
	{
		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Require open/broken door */
		if ((cave_feat[y][x] != FEAT_OPEN) && (cave_feat[y][x] != FEAT_BROKEN))
		{
			/* Message */
			msg_print("You see nothing there to close.");
		}

		/* Monster in the way */
		else if (cave_monster_bold(y, x))
		{
			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Close the door */
		else
		{
			/* Close the door */
			more = do_cmd_close_aux(y, x, dir);
		}
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}



/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * This will, however, produce grids which are NOT illuminated
 * (or darkened) along with the rest of the room.
 */
static bool twall(int y, int x)
{
	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	draw_grid_feat(y, x, FEAT_FLOOR);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS);

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
static bool do_cmd_tunnel_aux(int y, int x, int dir)
{
	bool more = FALSE;

	byte feat = cave_feat[y][x];

	/* Take a turn */
	energy_use = 100;

	/* Sound */
	sound(SOUND_DIG);

	if (feat == FEAT_BARREL_GOO)
	{
		if (get_check("Really hit the barrel?"))
		{
			msg_print("You hit the barrel.");

			if (p_ptr->skill_dig > 10 + rand_int(10))
			{
				/* Oops */
				msg_print("The barrel explodes!");

				/* Kill it */
				draw_grid_feat(y, x, FEAT_FLOOR);

				/* Boom! */
				project(0, 2, y, x, 150, GF_ROCKET, PROJECT_JUMP | PROJECT_GRID |
																 PROJECT_ITEM | PROJECT_KILL);

				/* Update stuff */
				p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);
			}
		}
	}

	/* Titanium */
	else if (feat >= FEAT_PERM_EXTRA)
	{
		msg_print("This seems to be permanent rock.");
	}

	/* Granite */
	else if ((feat >= FEAT_WALL_EXTRA) || (feat == FEAT_SECRET))
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 40 + rand_int(1600)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunnelling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;
		}
	}

	/* Quartz / Magma */
	else if (feat >= FEAT_MAGMA)
	{
		bool okay;
		bool gold = FALSE;
		bool hard = FALSE;

		/* Found gold */
		if (feat >= FEAT_MAGMA_H) gold = TRUE;

		/* Extract "quartz" flag XXX XXX XXX */
		if ((feat - FEAT_MAGMA) & 0x01) hard = TRUE;

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
				place_gold(y, x);

				/* Message */
				msg_print("You have found something!");
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
	else if (feat == FEAT_RUBBLE)
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

	/* Default to secret doors */
	else
	{
		/* Message, keep digging */
		msg_print("You tunnel into the granite wall.");
		more = TRUE;
	}

	/* Notice new floor grids */
	if (!cave_floor_bold(y, x))
	{
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS);
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
	int py = p_ptr->py;
	int px = p_ptr->px;

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
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Oops */
		if (cave_floor_bold(y, x) || ((cave_feat[y][x] >= FEAT_MINOR_GLYPH) &&
			 (cave_feat[y][x] <= FEAT_PATTERN_XTRA2)))
		{
			/* Message */
			msg_print("You cannot tunnel through air.");
		}

		/* Open doors */
		else if (cave_feat[y][x] < FEAT_SECRET)
		{
			do_cmd_open_aux(y, x, dir);
		}

		/* A monster is in the way */
		else if (cave_monster_bold(y, x))
		{
			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Try digging */
		else
		{
			/* Tunnel through walls */
			more = do_cmd_tunnel_aux(y, x, dir);
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
	if (p_ptr->blind || player_has_no_lite) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Extract the difficulty */
	j = i - o_ptr->pval;

	/* Always have a small chance of success */
	if (j < 2) j = 2;

	/* Must find the trap first. */
	if (!object_known_p(o_ptr))
	{
		msg_print("You see no traps.");
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
static bool do_cmd_disarm_aux(int y, int x, int dir)
{
	int i, j, power;

	cptr name;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Access trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || player_has_no_lite) i = i / 10;
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
		msg_format("You have disarmed the %s.", name);

		/* Reward */
		gain_exp(power);

		/* Forget the trap */
		cave_info[y][x] &= ~(CAVE_MARK);

		/* Remove the trap */
		draw_grid_feat(y, x, FEAT_FLOOR);

		/* move the player onto the trap grid */
		move_player(dir, FALSE);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* Failure */
		if (flush_failure) flush();

		/* Message */
		msg_format("You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		msg_format("You set off the %s!", name);

		/* Move the player onto the trap */
		move_player(dir, FALSE);
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b o_idx;

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

	/* Get a direction (or abort) */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Check for chests */
		o_idx = chest_check(y, x);

		/* Disarm a trap */
		if ((f_info[cave_feat[y][x]].d_char != '^') && !o_idx)
		{
			/* Message */
			msg_print("You see nothing there to disarm.");
		}

		/* Monster in the way */
		else if (cave_monster_bold(y, x))
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
			more = do_cmd_disarm_aux(y, x, dir);
		}
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/*
 * Perform the basic "bash" command
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x, int dir)
{
	int			bash, temp;

	bool		more = FALSE;


	/* Take a turn */
	energy_use = 100;

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
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Message */
		msg_print("The door crashes open!");

		/* Break down the door */
		if (rand_int(100) < 50)
		{
			draw_grid_feat(y, x, FEAT_BROKEN);
		}

		/* Open the door */
		else
		{
			draw_grid_feat(y, x, FEAT_OPEN);
		}

		/* Sound */
		sound(SOUND_OPENDOOR);

		/* Hack -- Fall through the door */
		move_player(dir, FALSE);

		/* Update some things */
		p_ptr->update |= (PU_VIEW);
		p_ptr->update |= (PU_DISTANCE);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
				p_ptr->level)
	{
		/* Message */
		msg_print("The door holds firm.");

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		msg_print("You are off-balance.");

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
	int			y, x, dir;
	int py = p_ptr->py;
	int px = p_ptr->px;

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

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Bash location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Nothing useful */
		if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
				(cave_feat[y][x] <= FEAT_DOOR_TAIL)))
		{
			/* Message */
			msg_print("You see nothing there to bash.");
		}

		/* Monster in the way */
		else if (cave_monster_bold(y, x))
		{
			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Bash a closed door */
		else
		{
			/* Bash the door */
			more = do_cmd_bash_aux(y, x, dir);
		}
	}

	/* Unless valid action taken, cancel bash */
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	byte feat;

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
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		feat = cave_feat[y][x];

		/* Take a turn */
		energy_use = 100;

		/* Attack monsters */
		if (cave_monster_bold(y, x))
		{
			/* Attack */
			py_attack(y, x);
		}

		/* Tunnel through walls */
		else if ((feat >= FEAT_SECRET) && (feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x, dir);
		}

		/* Bash jammed doors */
		else if ((feat >= FEAT_DOOR_HEAD + 0x08) && (feat < FEAT_MINOR_GLYPH))
		{
			/* Bash */
			more = do_cmd_bash_aux(y, x, dir);
		}

		/* Open closed doors */
		else if ((feat >= FEAT_DOOR_HEAD) && (feat < FEAT_MINOR_GLYPH))
		{
			/* Open */
			more = do_cmd_open_aux(y, x, dir);
		}

		/* Disarm traps */
		else if (f_info[feat].d_char == '^')
		{
			/* Disarm */
			more = do_cmd_disarm_aux(y, x, dir);
		}

		/* Oops */
		else
		{
			/* Oops */
			msg_print("You spin around.");
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
	int y, x, dir, item;
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Require closed door */
		if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
				(cave_feat[y][x] <= FEAT_DOOR_TAIL)))
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
		else if (cave_monster_bold(y, x))
		{
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

			/* Convert "locked" to "stuck" XXX XXX XXX */
			if (cave_feat[y][x] < FEAT_DOOR_HEAD + 0x08) cave_feat[y][x] += 0x08;

			/* Add one spike to the door */
			if (cave_feat[y][x] < FEAT_DOOR_TAIL) cave_feat[y][x]++;

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
	if (get_rep_dir(&dir))
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
	if (get_rep_dir(&dir))
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
	int py = p_ptr->py;
	int px = p_ptr->px;

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
	if ((cave_feat[py][px] >= FEAT_SHOP_HEAD) &&
		 (cave_feat[py][px] <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		command_new = '_';
	}
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
		case TV_BOTTLE:
		case TV_JUNK:
		case TV_POTION:
		case TV_FLASK:
		case TV_FOOD:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		{
			return (50);
		}

		/* Sometimes break */
		case TV_SKELETON:
		case TV_CHEST:
		case TV_ARROW:
		case TV_AMULET:
		case TV_STAFF:
		{
			return (33);
		}

		/* Occasionally break */
		case TV_BOW:
		case TV_POLEARM:
		{
			return (25);
		}

		/* Seldom break */
		case TV_BOLT:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_WAND:
		{
			return (10);
		}

		/* Never break */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_RING:
		case TV_ROD:
		{
			return (0);
		}

		/* Rarely break */
		default:
		{
			return (5);
		}
	}
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
	int dir, item;
	int j, y, x, ny, nx, ty, tx;
	int tdam, tdis, thits, tmul;
	int bonus, chance;
	int cur_dis, visible;
	int py = p_ptr->py;
	int px = p_ptr->px;
	int m_idx;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;
	object_type *j_ptr;

	byte missile_attr;

	char o_name[80];

	int msec = delay_factor * delay_factor * delay_factor;


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

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Fire which item? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to fire.");
		return;
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
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

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


	/* Sound */
	sound(SOUND_SHOOT);


	/* Describe the object */
	object_desc(o_name, q_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(q_ptr);

	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from thrown object plus launcher bonus */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d + j_ptr->to_d;

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h + q_ptr->to_h + j_ptr->to_h);
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
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			tmul = 2;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			tmul = 3;
			break;
		}

		/* Light Crossbow and Bolt */
		case SV_LIGHT_XBOW:
		{
			tmul = 3;
			break;
		}

		/* Heavy Crossbow and Bolt */
		case SV_HEAVY_XBOW:
		{
			tmul = 4;
			break;
		}
	}

	/* Get extra "power" from "extra might" */
	if (p_ptr->xtra_might) tmul++;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range */
	tdis = 10 + 5 * tmul;


	/* Take a (partial) turn */
	energy_use = (100 / thits);


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

		/* Stopped by walls/doors */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance the distance */
		cur_dis++;


		/* The player can see the (on screen) missile */
		if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
		{
			u16b p;

			/* Obtain the bolt pict */
			p = bolt_pict(y, x, ny, nx, GF_MISSILE);

			/* Visual effects */
			print_rel(PICT_C(p), missile_attr, ny, nx);
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

		m_idx = cave_m_idx[y][x];

		/* Monster here, Try to hit it */
		if (m_idx)
		{
			monster_type *m_ptr = &m_list[m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

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
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(m_idx);

					/* Anger friends */
					if (m_ptr->smart & SM_FRIEND)
					{
						char m_name[80];
						monster_desc(m_name, m_ptr, 0);
						msg_format("%s gets angry!", m_name);
						m_ptr->smart &= ~SM_FRIEND;
					}
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr);
				tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(m_idx, tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(m_idx, tdam);

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
	j = breakage_chance(q_ptr);

	/* Drop (or break) near that location */
	drop_near(q_ptr, j, y, x);
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
	int dir, item;
	int j, y, x, ny, nx, ty, tx;
	int chance, tdam, tdis;
	int mul, div;
	int cur_dis, visible;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int m_idx = 0;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	byte missile_attr;

	char o_name[80];

	int msec = delay_factor * delay_factor * delay_factor;


	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Throw which item? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to throw.");
		return;
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
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

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

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(q_ptr);

	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d;

	/* Chance of hitting */
	chance = (p_ptr->skill_tht + (p_ptr->to_h * BTH_PLUS_ADJ));


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

		/* Stopped by walls/doors */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance the distance */
		cur_dis++;

		/* The player can see the (on screen) missile */
		if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
		{
			u16b p;

			/* Obtain the bolt pict */
			p = bolt_pict(y, x, ny, nx, GF_MISSILE);

			/* Visual effects */
			print_rel(PICT_C(p), missile_attr, ny, nx);
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

		m_idx = cave_m_idx[y][x];

		/* Monster here, Try to hit it */
		if (m_idx)
		{
			monster_type *m_ptr = &m_list[m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

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
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(m_idx);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr);
				tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(m_idx, tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(m_idx, tdam);

					/* Anger friends */
					if ((m_ptr->smart & SM_FRIEND) &&
					    (!(k_info[q_ptr->k_idx].tval == TV_POTION)))
					{
						char m_name[80];
						monster_desc(m_name, m_ptr, 0);
						msg_format("%s gets angry!", m_name);
						m_ptr->smart &= ~SM_FRIEND;
					}

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
	j = breakage_chance(q_ptr);

	/* Potions smash open */
	if (k_info[q_ptr->k_idx].tval == TV_POTION)
	{
		if (randint(100) < j)
		{
			/* Message */
			msg_format("The %s shatters!", o_name);

			if (potion_smash_effect(y, x, q_ptr->sval))
			{
				if (m_idx && (m_list[m_idx].smart & SM_FRIEND))
				{
					char m_name[80];
					monster_desc(m_name, &m_list[m_idx], 0);
					msg_format("%s gets angry!", m_name);
					m_list[m_idx].smart &= ~SM_FRIEND;
				}
			}

			return;
		}
		else
		{
			j = 0;
		}
	}

	/* Rings and shots bounce and roll */
	if ((k_info[q_ptr->k_idx].tval == TV_RING) || (k_info[q_ptr->k_idx].tval == TV_SHOT))
	{
		int speed = tdis - cur_dis;

		for (; speed > 0; speed /= 3)
		{
			int oy = y;
			int ox = x;

			/* Find a location to roll to */
			scatter(&ty, &tx, oy, ox, speed, 0);

			while(speed)
			{
				/* Hack -- Stop at the target */
				if ((y == ty) && (x == tx)) break;

				/* Calculate the new location (see "project()") */
				ny = y;
				nx = x;
				mmove2(&ny, &nx, oy, ox, ty, tx);

				/* Stopped by walls/doors */
				if (!cave_floor_bold(ny, nx)) break;

				/* Lose some speed */
				speed--;

				/* The player can see the (on screen) missile */
				if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
				{
					u16b p;

					/* Obtain the bolt pict */
					p = bolt_pict(y, x, ny, nx, GF_MISSILE);

					/* Visual effects */
					print_rel(PICT_C(p), missile_attr, ny, nx);
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
			}
		}
	}

	/* Drop (or break) near that location */
	drop_near(q_ptr, j, y, x);
}


/*
 * Returns the chance to activate a racial power/mutation
 */
static int racial_chance(s16b min_level, int use_stat, int difficulty)
{
	int i;
	int val;
	int sum = 0;
	int stat = p_ptr->stat_cur[use_stat];

	/* No chance for success */
	if ((p_ptr->level < min_level) || p_ptr->confused)
	{
		return (0);
	}

	/* Calculate difficulty */
	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->level > min_level)
	{
		int lev_adj = ((p_ptr->level - min_level) / 3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* We only need halfs of the difficulty */
	difficulty = difficulty / 2;

	for (i = 1; i <= stat; i++)
	{
		val = i - difficulty;
		if (val > 0)
			sum += (val <= difficulty) ? val : difficulty;
	}

	if (difficulty == 0)
		return (100);
	else
		return (((sum * 100) / difficulty) / stat);
}


/* Note: return value indicates that we have succesfully used the power */

bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
	bool use_hp = FALSE;

	/* Not enough mana - use hp */
	if (p_ptr->csp < cost) use_hp = TRUE;

	/* Power is not available yet */
	if (p_ptr->level < min_level)
	{
		msg_format("You need to attain level %d to use this power.", min_level);
		energy_use = 0;
		return FALSE;
	}

	/* Too confused */
	else if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		energy_use = 0;
		return FALSE;
	}

	/* Risk death? */
	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!get_check("Really use the power in your weakened state? "))
		{
			energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->level > min_level)
	{
		int lev_adj = ((p_ptr->level - min_level) / 3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	energy_use = 100;

	if (use_hp)
	{
		take_hit((cost / 2) + randint(cost / 2),
			"concentrating too hard");
	}
	else
	{
		p_ptr->csp -= (cost / 2) + randint(cost / 2);
	}


	/* Redraw mana and hp */
	p_ptr->redraw |= (PR_HP | PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_SPELL);

	/* Success? */
	if (randint(p_ptr->stat_cur[use_stat]) >=
		 ((difficulty / 2) + randint(difficulty / 2)))
	{
		return TRUE;
	}

	msg_print("You've failed to concentrate hard enough.");
	return FALSE;
}


static void cmd_racial_power_aux(s32b command)
{
	s16b        plev = p_ptr->level;
	int         dir = 0;

	int py = p_ptr->py;
	int px = p_ptr->px;

	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			if (racial_aux(5, 5, A_WIS, 12))
			{
				msg_print("You examine your surroundings.");
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}
			break;

		case RACE_HOBBIT:
			if (racial_aux(15, 10, A_INT, 10))
			{
				object_type *q_ptr;
				object_type forge;

				/* Get local object */
				q_ptr = &forge;

				/* Create the food ration */
				object_prep(q_ptr, 21);

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, py, px);
				msg_print("You cook some food.");
			}
			break;

		case RACE_GNOME:
			if (racial_aux(5, (5 + (plev / 5)), A_INT, 12))
			{
				msg_print("Blink!");
				teleport_player(10 + plev);
			}
			break;

		case RACE_HALF_ORC:
			if (racial_aux(3, 5, A_WIS,
				 (p_ptr->pclass == CLASS_WARRIOR ? 5 : 10)))
			{
				msg_print("You play tough.");
				(void)set_afraid(0);
			}
			break;

		case RACE_HALF_TROLL:
			if (racial_aux(10, 12, A_WIS,
				 (p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("RAAAGH!");
				(void)set_afraid(0);

				(void)set_shero(p_ptr->shero + 10 + randint(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_AMBERITE:
			if (command == -2)
			{
				if (racial_aux(40, 75, A_WIS, 50))
				{
					msg_print("You picture the Pattern in your mind and walk it...");
					(void)set_poisoned(0);
					(void)set_image(0);
					(void)set_stun(0);
					(void)set_cut(0);
					(void)set_blind(0);
					(void)set_afraid(0);
					(void)do_res_stat(A_STR);
					(void)do_res_stat(A_INT);
					(void)do_res_stat(A_WIS);
					(void)do_res_stat(A_DEX);
					(void)do_res_stat(A_CON);
					(void)do_res_stat(A_CHR);
					(void)restore_level();
				}
			}

			else if (command == -1)
			{
				if (racial_aux(30, 50, A_INT, 50))
				{
					msg_print("You start walking around. Your surroundings change.");

					if (autosave_l) do_cmd_save_game();

					/* Leaving */
					new_level_flag = TRUE;
				}
			}
			break;

		case RACE_BARBARIAN:
			if (racial_aux(8, 10, A_WIS, (p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("Raaagh!");
				(void)set_afraid(0);

				(void)set_shero(p_ptr->shero + 10 + randint(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_HALF_OGRE:
			if (racial_aux(25, 35, A_INT, 15))
			{
				msg_print("You carefully set an explosive rune...");
				explosive_rune();
			}
			break;

		case RACE_HALF_GIANT:
			if (racial_aux(20, 10, A_STR, 12))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You bash at a stone wall.");
				(void)wall_to_mud(dir);
			}
			break;

		case RACE_HALF_TITAN:
			if (racial_aux(35, 20, A_INT, 12))
			{
				msg_print("You examine your foes...");
				probing();
			}
			break;

		case RACE_CYCLOPS:
			if (racial_aux(20, 15, A_STR, 12))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You throw a huge boulder.");
				fire_bolt(GF_MISSILE, dir, (3 * plev) / 2);
			}
			break;

		case RACE_YEEK:
			if (racial_aux(15, 15, A_WIS, 10))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You make a horrible scream!");
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_KLACKON:
			if (racial_aux(9, 9, A_DEX, 14))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You spit acid.");
				if (plev < 25)
				{
					fire_bolt(GF_ACID, dir, damroll(7, 8) + (plev / 3));
				}
				else
				{
					fire_ball(GF_ACID, dir, randint(plev * 3) + 15, 2);
				}
			}
			break;

		case RACE_KOBOLD:
			if (racial_aux(12, 8, A_DEX, 14))
			{
				if(!get_aim_dir(&dir)) break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, damroll(3, 8) + (plev / 3));
			}
			break;

		case RACE_NIBELUNG:
			if (racial_aux(10, 5, A_WIS, 10))
			{
				msg_print("You examine your surroundings.");
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}
			break;

		case RACE_DARK_ELF:
			if (racial_aux(2, 2, A_INT, 9))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You cast a magic missile.");
				fire_bolt_or_beam(10, GF_MISSILE, dir,
					 damroll(3 + ((plev - 1) / 5), 4));
			}
			break;

		case RACE_DRACONIAN:
			if (racial_aux(1, plev, A_CON, 12))
			{
				int  Type = ((randint(3) == 1) ? GF_COLD : GF_FIRE);
				cptr Type_desc = ((Type == GF_COLD) ? "cold" : "fire");

				if (randint(100) < plev)
				{
					switch (p_ptr->pclass)
					{
						case CLASS_WARRIOR:
						case CLASS_RANGER:
							if (randint(3) == 1)
							{
								Type = GF_MISSILE;
								Type_desc = "the elements";
							}
							else
							{
								Type = GF_SHARDS;
								Type_desc = "shards";
							}
							break;
						case CLASS_MAGE:
						case CLASS_WARRIOR_MAGE:
						case CLASS_HIGH_MAGE:
							if (randint(3) == 1)
							{
								Type = GF_MANA;
								Type_desc = "mana";
							}
							else
							{
								Type = GF_DISENCHANT;
								Type_desc = "disenchantment";
							}
							break;
						case CLASS_CHAOS_WARRIOR:
							if (randint(3) != 1)
							{
								Type = GF_CONFUSION;
								Type_desc = "confusion";
							}
							else
							{
								Type = GF_CHAOS;
								Type_desc = "chaos";
							}
							break;
						case CLASS_MONK:
							if (randint(3) != 1)
							{
								Type = GF_CONFUSION;
								Type_desc = "confusion";
							}
							else
							{
								Type = GF_SOUND;
								Type_desc = "sound";
							}
							break;
						case CLASS_MINDCRAFTER:
							if (randint(3) != 1)
							{
								Type = GF_CONFUSION;
								Type_desc = "confusion";
							}
							else
							{
								Type = GF_PSI;
								Type_desc = "mental energy";
							}
							break;
						case CLASS_PRIEST:
						case CLASS_PALADIN:
							if (randint(3) == 1)
							{
								Type = GF_HELL_FIRE;
								Type_desc = "hellfire";
							}
							else
							{
								Type = GF_HOLY_FIRE;
								Type_desc = "holy fire";
							}
							break;
						case CLASS_ROGUE:
							if (randint(3) == 1)
							{
								Type = GF_DARK;
								Type_desc = "darkness";
							}
							else
							{
								Type = GF_POIS;
								Type_desc = "poison";
							}
							break;
					}
				}

				if (!get_aim_dir(&dir)) break;
				msg_format("You breathe %s.", Type_desc);
				fire_ball(Type, dir, p_ptr->chp / 3, (plev / 15) + 1);
			}
			break;

		case RACE_MIND_FLAYER:
			if (racial_aux(15, 12, A_INT, 14))
			{
				if (!get_aim_dir(&dir)) break;
				else
				{
					msg_print("You concentrate and your eyes glow red...");
					fire_bolt(GF_PSI, dir, damroll(8, 8));
				}
			}
			break;

		case RACE_IMP:
			if (racial_aux(9, 15, A_CON, 15))
			{
				if (!get_aim_dir(&dir)) break;
				if (plev >= 30)
				{
					msg_print("You cast a ball of fire.");
					fire_ball(GF_FIRE, dir, randint(plev * 7 / 2) + 10, 2);
				}
				else
				{
					msg_print("You cast a bolt of fire.");
					fire_bolt(GF_FIRE, dir, damroll(9, 8) + (plev / 3));
				}
			}
			break;

		case RACE_GOLEM:
			if (racial_aux(20, 15, A_CON, 8))
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
			}
			break;

		case RACE_SKELETON:
		case RACE_ZOMBIE:
			if (racial_aux(30, 30, A_WIS, 18))
			{
				msg_print("You attempt to restore your lost energies.");
				(void)restore_level();
			}
			break;

		case RACE_VAMPIRE:
			if (racial_aux(2, (1 + (plev / 3)), A_CON, 9))
			{
				int y, x, dummy;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir)) break;   /* was get_aim_dir */
				y = py + ddy[dir];
				x = px + ddx[dir];

				if (cave_dead_bold(y, x))
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");
				dummy = plev + damroll(MAX(1, plev / 10), plev);   /* Dmg */
				if (drain_life(dir, dummy))
				{
					if (p_ptr->food < PY_FOOD_FULL)
						/* No heal if we are "full" */
						(void)hp_player(dummy);
					else
						msg_print("You were not hungry.");
						/* Gain nutritional sustenance: 150/hp drained */
						/* A Food ration gives 5000 food points (by contrast) */
						/* Don't ever get more than "Full" this way */
						/* But if we ARE Gorged,  it won't cure us */
						dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
						(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
				}
				else
					msg_print("Yechh. That tastes foul.");
			}
			break;

		case RACE_SPECTRE:
			if (racial_aux(4, 6, A_INT, 3))
			{
				msg_print("You emit an eldritch howl!");
				if (!get_aim_dir(&dir)) break;
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_SPRITE:
			if (racial_aux(12, 12, A_INT, 15))
			{
				msg_print("You throw some magic dust...");
				if (plev < 25)
					sleep_monsters_touch();
				else
					(void)sleep_monsters();
			}
			break;

		case RACE_FIEND:
		{
			if (racial_aux(5, 15, A_CON, 12))
			{
				/* Actual grids in the "path" */
				u16b path_g[512];

				int path_n = 0;

				int ty, tx;
				int ey, ex;

				int m_idx = 0;

				msg_print("You crouch for a leap...");

				if (!get_aim_dir(&dir)) break;

				/* Use the given direction */
				tx = px + ddx[dir] * MAX_RANGE;
				ty = py + ddy[dir] * MAX_RANGE;

				/* Hack -- Use an actual "target" */
				if ((dir == 5) && target_okay())
				{
					tx = target_col;
					ty = target_row;
				}

				/* Find a path to the target */
				path_n = project_path(path_g, adj_str_hold[p_ptr->stat_ind[A_STR]] / 2,
											 py, px, ty, tx, PROJECT_STOP);

				/* No movement */
				if (!path_n) break;

				/* Extract the final grid */
				ty = ey = GRID_Y(path_g[path_n - 1]);
				tx = ex = GRID_X(path_g[path_n - 1]);

				/* Note monsters */
				m_idx = cave_m_idx[ty][tx];

				/* Extract the previous grid if necessary */
				if (!cave_empty_bold(ty, tx))
				{
					if (path_n == 1)
					{
						if (m_idx) py_attack(ty, tx);
						break;
					}

					ey = GRID_Y(path_g[path_n - 2]);
					ex = GRID_X(path_g[path_n - 2]);
				}

				/* Move player */
				monster_swap(py, px, ey, ex);

				/* Verify the panel */
				verify_panel();

				/* Handle stuff XXX XXX XXX */
				handle_stuff();

				/* Attack the monster at the target */
				if (m_idx) py_attack(ty, tx);
			}

			break;
		}

		default:
			msg_print("This race has no bonus power.");
			energy_use = 0;
	}

	/* Redraw mana and hp */
	p_ptr->redraw |= (PR_HP | PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_SPELL);
}


void mutation_power_aux(int power)
{
	int     dir = 0;
	int     lvl = p_ptr->level;

	int py = p_ptr->py;
	int px = p_ptr->px;

	switch (power)
	{
		case MUT1_SPIT_ACID:
			if (racial_aux(9, 9, A_DEX, 15))
			{
				msg_print("You spit acid...");
				if (get_aim_dir(&dir))
					fire_ball(GF_ACID, dir, randint(lvl * 3) + 15, 1 + (lvl / 30));
			}
			break;

		case MUT1_BR_FIRE:
			if (racial_aux(20, lvl, A_CON, 18))
			{
				msg_print("You breathe fire...");
				if (get_aim_dir(&dir))
					fire_ball(GF_FIRE, dir, randint(lvl * 7 / 2) + 10, 1 + (lvl / 20));
			}
			break;

		case MUT1_HYPN_GAZE:
			if (racial_aux(12, 12, A_CHR, 18))
			{
				msg_print("Your eyes look mesmerizing...");
				if (get_aim_dir(&dir))
					(void)charm_monster(dir, lvl);
			}
			break;

		case MUT1_TELEKINES:
			if (racial_aux(9, 9, A_WIS, 14))
			{
				msg_print("You concentrate...");
				if (get_aim_dir(&dir))
					fetch(dir, lvl * 10, TRUE);
			}
			break;

		case MUT1_VTELEPORT:
			if (racial_aux(7, 7, A_WIS, 15))
			{
				msg_print("You concentrate...");
				teleport_player(10 + 4 * lvl);
			}
			break;

		case MUT1_MIND_BLST:
			if (racial_aux(5, 3, A_WIS, 15))
			{
				msg_print("You concentrate...");
				if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_PSI, dir, damroll(8, 8));
			}
			break;

		case MUT1_RADIATION:
			if (racial_aux(15, 15, A_CON, 14))
			{
				msg_print("Radiation flows from your body!");
				fire_ball(GF_NUKE, 0, lvl * 7 / 3, 3 + (lvl / 20));
			}
			break;

		case MUT1_VAMPIRISM:
			if (racial_aux(2, (1 + (lvl / 3)), A_CON, 9))
			{
				int x, y, dummy;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir)) break;
				y = py + ddy[dir];
				x = px + ddx[dir];

				if (cave_dead_bold(y, x))
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");

				dummy = lvl + randint(lvl);

				if (drain_life(dir, dummy))
				{
					if (p_ptr->food < PY_FOOD_FULL)
						/* No heal if we are "full" */
						(void)hp_player(dummy);
					else
						msg_print("You were not hungry.");
						/* Gain nutritional sustenance: 150/hp drained */
						/* A Food ration gives 5000 food points (by contrast) */
						/* Don't ever get more than "Full" this way */
						/* But if we ARE Gorged,  it won't cure us */
						dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
						(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
				}
				else
					msg_print("Yechh. That tastes foul.");
			}
			break;

		case MUT1_BLINK:
			if (racial_aux(3, 3, A_WIS, 12))
			{
				teleport_player(10);
			}
			break;

		case MUT1_EAT_ROCK:
			if (racial_aux(8, 12, A_CON, 18))
			{
				int x, y, ox, oy;

				if (!get_rep_dir(&dir)) break;
				y = py + ddy[dir];
				x = px + ddx[dir];

				if (cave_floor_bold(y, x))
				{
					msg_print("You bite into thin air!");
					break;
				}
				else if ((cave_feat[y][x] >= FEAT_PERM_EXTRA) &&
					(cave_feat[y][x] <= FEAT_PERM_SOLID))
				{
					msg_print("Ouch!  This wall is harder than your teeth!");
					break;
				}
				else if (cave_monster_bold(y, x))
				{
					msg_print("There's something in the way!");
					break;
				}
				else
				{
					if ((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
						(cave_feat[y][x] <= FEAT_RUBBLE))
					{
						(void)set_food(p_ptr->food + 3000);
					}
					else if ((cave_feat[y][x] >= FEAT_MAGMA) &&
						(cave_feat[y][x] <= FEAT_QUARTZ_K))
					{
						(void)set_food(p_ptr->food + 5000);
					}
					else
					{
						msg_print("This granite is very filling!");
						(void)set_food(p_ptr->food + 10000);
					}
				}
				(void)wall_to_mud(dir);

				oy = py;
				ox = px;

				py = y;
				px = x;

				lite_spot(py, px);
				lite_spot(oy, ox);

				verify_panel();

				p_ptr->update |= (PU_VIEW | PU_FLOW);
				p_ptr->update |= (PU_DISTANCE);
				p_ptr->window |= (PW_OVERHEAD);
			}
			break;

		case MUT1_SHRIEK:
			if (racial_aux(20, 14, A_CON, 16))
			{
				(void)fire_ball(GF_SOUND, 0, lvl * 5 / 3, 8);
				(void)aggravate_monsters(0);
			}
			break;

		case MUT1_ILLUMINE:
			if (racial_aux(3, 2, A_INT, 10))
			{
				(void)lite_area(damroll(2, (lvl / 2)), (lvl / 10) + 1);
			}
			break;

		case MUT1_DET_CURSE:
			if (racial_aux(7, 14, A_WIS, 14))
			{
				int i;

				for (i = 0; i < INVEN_TOTAL; i++)
				{
					object_type *o_ptr = &inventory[i];

					if (!o_ptr->k_idx) continue;
					if (!cursed_p(o_ptr)) continue;

					o_ptr->note = quark_add("cursed");
				}
			}
			break;

		case MUT1_BERSERK:
			if (racial_aux(8, 8, A_STR, 14))
			{
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)hp_player(30);
				(void)set_afraid(0);
			}
			break;

		case MUT1_POLYMORPH:
			if (racial_aux(18, 20, A_CON, 18))
			{
				do_poly_self();
			}
			break;

		case MUT1_MIDAS_TCH:
			if (racial_aux(10, 5, A_INT, 12))
			{
				(void)alchemy();
			}
			break;

		/* Summon pet monsters around the player */
		case MUT1_SUMMON_M:
		if (racial_aux(10, p_ptr->level / 2, A_CON, 10))
		{
			summon_specific(py, px, (lvl * 3) / 2, SUMMON_NO_UNIQUES, TRUE, TRUE);
		}
		break;

		/* Summon pet molds around the player */
		case MUT1_GROW_MOLD:
			if (racial_aux(1, 6, A_CON, 14))
			{
				int i;
				for (i = 0; i < 8; i++)
				{
					summon_specific(py, px, (lvl * 3) / 2, SUMMON_BIZARRE1, TRUE, TRUE);
				}
			}
			break;

		case MUT1_RESIST:
			if (racial_aux(10, 12, A_CON, 12))
			{
				int num = lvl / 10;
				int dur = randint(20) + 20;

				if (rand_int(5) < num)
				{
					(void)set_oppose_acid(p_ptr->oppose_acid + dur);
					num--;
				}
				if (rand_int(4) < num)
				{
					(void)set_oppose_elec(p_ptr->oppose_elec + dur);
					num--;
				}
				if (rand_int(3) < num)
				{
					(void)set_oppose_fire(p_ptr->oppose_fire + dur);
					num--;
				}
				if (rand_int(2) < num)
				{
					(void)set_oppose_cold(p_ptr->oppose_cold + dur);
					num--;
				}
				if (num)
				{
					(void)set_oppose_pois(p_ptr->oppose_pois + dur);
				}
			}
			break;

		case MUT1_EARTHQUAKE:
			if (racial_aux(12, 12, A_STR, 16))
			{
				earthquake(py, px, 10);
			}
			break;

		case MUT1_DAZZLE:
			if (racial_aux(7, 15, A_CHR, 8))
			{
				stun_monsters(lvl * 4);
				confuse_monsters(lvl * 4);
				turn_monsters(lvl * 4);
			}
			break;

		case MUT1_RECALL:
			if (racial_aux(17, 50, A_INT, 16))
			{
				if (p_ptr->depth && (p_ptr->max_depth > p_ptr->depth))
				{
					if (get_check("Reset recall depth? "))
						p_ptr->max_depth = p_ptr->depth;
				}
				if (!p_ptr->word_recall)
				{
					p_ptr->word_recall = rand_int(21) + 15;
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
			}
			break;

		case MUT1_BANISH:
			if (racial_aux(25, 25, A_WIS, 18))
			{
				int x,y;
				monster_type *m_ptr;
				monster_race *r_ptr;

				if (!get_rep_dir(&dir)) return;
				y = py + ddy[dir];
				x = px + ddx[dir];

				if (cave_dead_bold(y, x))
				{
					msg_print("You sense no evil there!");
					break;
				}

				m_ptr = &m_list[cave_m_idx[y][x]];
				r_ptr = &r_info[m_ptr->r_idx];

				if ((r_ptr->flags3 & RF3_EVIL) &&
					 !(r_ptr->flags1 & RF1_QUESTOR) &&
					 !(r_ptr->flags1 & RF1_UNIQUE))
				{
					/* Delete the monster, rather than killing it. */
					delete_monster_idx(cave_m_idx[y][x]);
					msg_print("The evil creature vanishes in a puff of sulfurous smoke!");
				}
				else
				{
					msg_print("Your invocation is ineffectual!");
				}
			}
			break;

		case MUT1_COLD_TOUCH:
			if (racial_aux(2, 2, A_CON, 11))
			{
				int x,y;

				if (!get_rep_dir(&dir)) return;
				y = py + ddy[dir];
				x = px + ddx[dir];

				if (cave_dead_bold(y, x))
				{
					msg_print("You wave your hands in the air.");
					break;
				}
				fire_bolt(GF_COLD, dir, damroll(6, 8) + (lvl / 3));
			}
			break;

		case MUT1_SHARD_BOLT:
			if (racial_aux(3, 2, A_CON, 7))
			{
				msg_print("You cast a stinging shard...");
				if (get_aim_dir(&dir))
					fire_bolt(GF_SHARDS, dir, damroll(3 + ((lvl - 1) / 10), 5));
			}
			break;

		case MUT1_SHARD_BLAST:
			if (racial_aux(7, 4, A_CON, 10))
			{
				msg_print("You cast a volley of shards...");
				if (get_aim_dir(&dir))
					fire_blast(GF_SHARDS, dir, 2 + ((lvl - 1) / 10), 4, 5, 3);
			}
			break;

		case MUT1_DSHARD_BLAST:
			if (racial_aux(14, 8, A_CON, 12))
			{
				msg_print("You cast a volley of shards...");
				if (get_aim_dir(&dir))
					fire_blast(GF_SHARDS, dir, 2 + ((lvl - 1) / 10), 4, 10, 4);
			}
			break;

		case MUT1_CHAIN_SHARDS:
			if (racial_aux(17, 10, A_STR, 16))
			{
				msg_print("You launch a barrage of shards...");
				if (get_aim_dir(&dir))
					fire_blast(GF_SHARDS, dir, 3 + ((lvl - 1) / 10), 5, 10, 2);
			}
			break;

		case MUT1_ROCKET:
			if (racial_aux(21, 15, A_STR, 18))
			{
				msg_print("You fire a rocket...");
				if (get_aim_dir(&dir))
					fire_ball(GF_ROCKET, dir, lvl * 3, 2);
			}
			break;

		case MUT1_PLAS_BOLT:
			if (racial_aux(27, 14, A_INT, 16))
			{
				msg_print("You cast a burst of plasma...");
				if (get_aim_dir(&dir))
					fire_blast(GF_PLASMA, dir, lvl / 5, 6, 3, 0);
			}
			break;

		case MUT1_BFG:
			if (racial_aux(40, 50, A_CON, 18))
			{
				msg_print("You invoke the BFG...");
				if (get_aim_dir(&dir))
				{
					fire_ball(GF_BFG, dir, 200, 1);
					fire_blast(GF_BFG, dir, 50, 5, damroll(5, 2), 10);
				}
			}
			break;

		default:
			energy_use = 0;
			msg_format("Power %s not implemented. Oops.", power);
	}
}



typedef struct power_desc_type power_desc_type;

struct power_desc_type
{
	cptr name;
	int level;
	int cost;
	int fail;
	char desc[40];
	u32b number;
};


/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	power_desc_type power_desc[36];

	int num, ask;
	int i = 0;
	int lvl = p_ptr->level;
	int page = 0;

	bool flag, redraw;
	bool warrior = ((p_ptr->pclass == CLASS_WARRIOR) ? TRUE : FALSE);
	bool has_racial = FALSE;

	char choice;
	char out_val[160];

	/* Wipe it */
	C_WIPE(power_desc, 36, power_desc_type);

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to use any powers!");
		energy_use = 0;
		return;
	}

	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			power_desc[0].name = "detect doors and traps";
			power_desc[0].level = 5;
			power_desc[0].cost = 5;
			power_desc[0].fail = 100 - racial_chance(5, A_WIS, 12);
			has_racial = TRUE;
			break;
		case RACE_NIBELUNG:
			power_desc[0].name = "detect doors and traps";
			power_desc[0].level = 10;
			power_desc[0].cost = 5;
			power_desc[0].fail = 100 - racial_chance(10, A_WIS, 10);
			has_racial = TRUE;
			break;
		case RACE_HOBBIT:
			power_desc[0].name = "create food";
			power_desc[0].level = 15;
			power_desc[0].cost = 10;
			power_desc[0].fail = 100 - racial_chance(15, A_INT, 10);
			has_racial = TRUE;
			break;
		case RACE_GNOME:
			power_desc[0].name = "teleport";
			power_desc[0].level = 5;
			power_desc[0].cost = 5 + (lvl / 5);
			power_desc[0].fail = 100 - racial_chance(5, A_INT, 12);
			sprintf(power_desc[0].desc, "range %d", 10 + lvl);
			has_racial = TRUE;
			break;
		case RACE_HALF_ORC:
			power_desc[0].name = "remove fear";
			power_desc[0].level = 3;
			power_desc[0].cost = 5;
			power_desc[0].fail = 100 - racial_chance(3, A_WIS, (warrior ? 5 : 10));
			has_racial = TRUE;
			break;
		case RACE_HALF_TROLL:
			power_desc[0].name = "berserk";
			power_desc[0].level = 10;
			power_desc[0].cost = 12;
			power_desc[0].fail = 100 - racial_chance(10, A_WIS, (warrior ? 6 : 12));
			sprintf(power_desc[0].desc, "dur d%d + 10", lvl);
			has_racial = TRUE;
			break;
		case RACE_BARBARIAN:
			power_desc[0].name = "berserk";
			power_desc[0].level = 8;
			power_desc[0].cost = 10;
			power_desc[0].fail = 100 - racial_chance(8, A_WIS, (warrior ? 6 : 12));
			sprintf(power_desc[0].desc, "dur d%d + 10", lvl);
			has_racial = TRUE;
			break;
		case RACE_AMBERITE:
			power_desc[0].name = "shadow shifting";
			power_desc[0].level = 30;
			power_desc[0].cost = 50;
			power_desc[0].fail = 100 - racial_chance(30, A_INT, 50);

			power_desc[1].name = "pattern mindwalking";
			power_desc[1].level = 40;
			power_desc[1].cost = 75;
			power_desc[1].fail = 100 - racial_chance(40, A_WIS, 50);
			power_desc[1].number = -2;
			num++;
			has_racial = TRUE;
			break;
		case RACE_HALF_OGRE:
			power_desc[0].name = "explosive rune";
			power_desc[0].level = 25;
			power_desc[0].cost = 35;
			power_desc[0].fail = 100 - racial_chance(25, A_INT, 15);
			has_racial = TRUE;
			break;
		case RACE_HALF_GIANT:
			power_desc[0].name = "stone to mud";
			power_desc[0].level = 20;
			power_desc[0].cost = 10;
			power_desc[0].fail = 100 - racial_chance(20, A_STR, 12);
			has_racial = TRUE;
			break;
		case RACE_HALF_TITAN:
			power_desc[0].name = "probing";
			power_desc[0].level = 35;
			power_desc[0].cost = 20;
			power_desc[0].fail = 100 - racial_chance(35, A_INT, 12);
			has_racial = TRUE;
			break;
		case RACE_CYCLOPS:
			power_desc[0].name = "throw boulder";
			power_desc[0].level = 20;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(20, A_STR, 12);
			sprintf(power_desc[0].desc, "dam %d", (3 * lvl) / 2);
			has_racial = TRUE;
			break;
		case RACE_YEEK:
			power_desc[0].name = "scare monster";
			power_desc[0].level = 15;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(15, A_WIS, 10);
			has_racial = TRUE;
			break;
		case RACE_SPECTRE:
			power_desc[0].name = "scare monster";
			power_desc[0].level = 4;
			power_desc[0].cost = 6;
			power_desc[0].fail = 100 - racial_chance(4, A_INT, 3);
			has_racial = TRUE;
			break;
		case RACE_KLACKON:
			power_desc[0].name = "spit acid";
			power_desc[0].level = 9;
			power_desc[0].cost = 9;
			power_desc[0].fail = 100 - racial_chance(9, A_DEX, 14);

			if (lvl < 25)
			{
				sprintf(power_desc[0].desc, "dam 7d8 + %d", lvl / 3);
			}
			else
			{
				sprintf(power_desc[0].desc, "dam d%d + 15", lvl * 3);
			}

			has_racial = TRUE;
			break;
		case RACE_KOBOLD:
			power_desc[0].name = "poison dart";
			power_desc[0].level = 12;
			power_desc[0].cost = 8;
			power_desc[0].fail = 100 - racial_chance(12, A_DEX, 14);
			sprintf(power_desc[0].desc, "dam 3d8 + %d", lvl / 3);
			has_racial = TRUE;
			break;
		case RACE_DARK_ELF:
			power_desc[0].name = "magic missile";
			power_desc[0].level = 2;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_chance(2, A_INT, 9);
			sprintf(power_desc[0].desc, "dam %dd%d", 3 + ((lvl - 1) / 5), 4);
			has_racial = TRUE;
			break;
		case RACE_DRACONIAN:
			power_desc[0].name = "breath weapon";
			power_desc[0].level = 1;
			power_desc[0].cost = lvl;
			power_desc[0].fail = 100 - racial_chance(1, A_CON, 12);
			sprintf(power_desc[0].desc, "dam %d", p_ptr->chp / 3);
			has_racial = TRUE;
			break;
		case RACE_MIND_FLAYER:
			power_desc[0].name = "mind blast";
			power_desc[0].level = 15;
			power_desc[0].cost = 12;
			power_desc[0].fail = 100 - racial_chance(15, A_INT, 14);
			sprintf(power_desc[0].desc, "dam 8d8");
			has_racial = TRUE;
			break;
		case RACE_IMP:
			power_desc[0].level = 9;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(9, A_CON, 15);

			if (lvl < 30)
			{
				power_desc[0].name = "fire bolt";
				sprintf(power_desc[0].desc, "dam 9d8 + %d", lvl / 3);
			}
			else
			{
				power_desc[0].name = "fire ball";
				sprintf(power_desc[0].desc, "dam d%d + 10", lvl * 7 / 2);
			}

			has_racial = TRUE;
			break;
		case RACE_GOLEM:
			power_desc[0].name = "stone skin";
			power_desc[0].level = 20;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(20, A_CON, 8);
			sprintf(power_desc[0].desc, "dur d20 + 30");
			has_racial = TRUE;
			break;
		case RACE_SKELETON:
		case RACE_ZOMBIE:
			power_desc[0].name = "restore life";
			power_desc[0].level = 30;
			power_desc[0].cost = 30;
			power_desc[0].fail = 100 - racial_chance(30, A_WIS, 18);
			has_racial = TRUE;
			break;
		case RACE_VAMPIRE:
			power_desc[0].name = "drain life";
			power_desc[0].level = 2;
			power_desc[0].cost = 1 + (lvl / 3);
			power_desc[0].fail = 100 - racial_chance(2, A_CON, 9);
			sprintf(power_desc[0].desc, "dam %d + %dd%d", lvl, MAX(1, lvl / 10), lvl);
			has_racial = TRUE;
			break;
		case RACE_SPRITE:
			power_desc[0].name = "sleeping dust";
			power_desc[0].level = 12;
			power_desc[0].cost = 12;
			power_desc[0].fail = 100 - racial_chance(12, A_INT, 15);
			has_racial = TRUE;
			break;
		case RACE_FIEND:
			power_desc[0].name = "leap";
			power_desc[0].level = 5;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(5, A_CON, 12);
			has_racial = TRUE;
			break;
		default:
			power_desc[0].name = "(none)";
	}

	if (!has_racial && !p_ptr->muta1)
	{
		msg_print("You have no powers to activate.");
		energy_use = 0;
		return;
	}

	if (has_racial)
	{
		power_desc[0].number = 0;
		num++;
	}

	if (p_ptr->muta1)
	{
		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			power_desc[num].name = "spit acid";
			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].fail = 100 - racial_chance(9, A_DEX, 15);
			sprintf(power_desc[num].desc, "dam d%d + 15", lvl * 3);
			power_desc[num++].number = MUT1_SPIT_ACID;
		}

		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			power_desc[num].name = "breathe fire";
			power_desc[num].level = 20;
			power_desc[num].cost = lvl;
			power_desc[num].fail = 100 - racial_chance(20, A_CON, 18);
			sprintf(power_desc[num].desc, "dam d%d + 10", lvl * 7 / 2);
			power_desc[num++].number = MUT1_BR_FIRE;
		}

		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			power_desc[num].name = "hypnotic gaze";
			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(12, A_CHR, 18);
			power_desc[num++].number = MUT1_HYPN_GAZE;
		}

		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
			power_desc[num].name = "telekinesis";
			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].fail = 100 - racial_chance(9, A_WIS, 14);
			power_desc[num++].number = MUT1_TELEKINES;
		}

		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			power_desc[num].name = "teleport";
			power_desc[num].level = 7;
			power_desc[num].cost = 7;
			power_desc[num].fail = 100 - racial_chance(7, A_WIS, 15);
			sprintf(power_desc[num].desc, "range %d", 10 + lvl * 4);
			power_desc[num++].number = MUT1_VTELEPORT;
		}

		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			power_desc[num].name = "mind blast";
			power_desc[num].level = 5;
			power_desc[num].cost = 3;
			power_desc[num].fail = 100 - racial_chance(5, A_WIS, 15);
			sprintf(power_desc[num].desc, "dam 8d8");
			power_desc[num++].number = MUT1_MIND_BLST;
		}

		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			power_desc[num].name = "emit radiation";
			power_desc[num].level = 15;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(15, A_CON, 14);
			sprintf(power_desc[num].desc, "dam d%d", lvl * 7 / 3);
			power_desc[num++].number = MUT1_RADIATION;
		}

		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			power_desc[num].name = "vampiric drain";
			power_desc[num].level = 2;
			power_desc[num].cost = (1 + (lvl / 3));
			power_desc[num].fail = 100 - racial_chance(2, A_CON, 9);
			sprintf(power_desc[num].desc, "dam d%d + %d", lvl, lvl);
			power_desc[num++].number = MUT1_VAMPIRISM;
		}


		if (p_ptr->muta1 & MUT1_BLINK)
		{
			power_desc[num].name = "blink";
			power_desc[num].level = 3;
			power_desc[num].cost = 3;
			power_desc[num].fail = 100 - racial_chance(3, A_WIS, 12);
			sprintf(power_desc[num].desc, "range 10");
			power_desc[num++].number = MUT1_BLINK;
		}

		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			power_desc[num].name = "eat rock";
			power_desc[num].level = 8;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(8, A_CON, 18);
			power_desc[num++].number = MUT1_EAT_ROCK;
		}

		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			power_desc[num].name = "shriek";
			power_desc[num].level = 20;
			power_desc[num].cost = 14;
			power_desc[num].fail = 100 - racial_chance(20, A_CON, 16);
			sprintf(power_desc[num].desc, "dam %d", lvl * 5 / 3);
			power_desc[num++].number = MUT1_SHRIEK;
		}

		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			power_desc[num].name = "illuminate";
			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(3, A_INT, 10);
			power_desc[num++].number = MUT1_ILLUMINE;
		}

		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			power_desc[num].name = "detect curses";
			power_desc[num].level = 7;
			power_desc[num].cost = 14;
			power_desc[num].fail = 100 - racial_chance(7, A_WIS, 14);
			power_desc[num++].number = MUT1_DET_CURSE;
		}

		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			power_desc[num].name = "berserk";
			power_desc[num].level = 8;
			power_desc[num].cost = 8;
			power_desc[num].fail = 100 - racial_chance(8, A_STR, 14);
			sprintf(power_desc[num].desc, "dur d25 + 25");
			power_desc[num++].number = MUT1_BERSERK;
		}

		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			power_desc[num].name = "polymorph";
			power_desc[num].level = 18;
			power_desc[num].cost = 20;
			power_desc[num].fail = 100 - racial_chance(18, A_CON, 18);
			power_desc[num++].number = MUT1_POLYMORPH;
		}

		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			power_desc[num].name = "midas touch";
			power_desc[num].level = 10;
			power_desc[num].cost = 5;
			power_desc[num].fail = 100 - racial_chance(10, A_INT, 12);
			power_desc[num++].number = MUT1_MIDAS_TCH;
		}

		if (p_ptr->muta1 & MUT1_SUMMON_M)
		{
			power_desc[num].name = "summon monsters";
			power_desc[num].level = 10;
			power_desc[num].cost = p_ptr->level / 2;
			power_desc[num].fail = 100 - racial_chance(10, A_CON, 10);
			power_desc[num++].number = MUT1_SUMMON_M;
		}

		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			power_desc[num].name = "grow mold";
			power_desc[num].level = 1;
			power_desc[num].cost = 6;
			power_desc[num].fail = 100 - racial_chance(1, A_CON, 14);
			power_desc[num++].number = MUT1_GROW_MOLD;
		}

		if (p_ptr->muta1 & MUT1_RESIST)
		{
			power_desc[num].name = "resist elements";
			power_desc[num].level = 10;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(10, A_CON, 12);
			sprintf(power_desc[num].desc, "dur d20 + 20");
			power_desc[num++].number = MUT1_RESIST;
		}

		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			power_desc[num].name = "earthquake";
			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(12, A_STR, 16);
			power_desc[num++].number = MUT1_EARTHQUAKE;
		}

		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			power_desc[num].name = "dazzle";
			power_desc[num].level = 7;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(7, A_CHR, 8);
			power_desc[num++].number = MUT1_DAZZLE;
		}

		if (p_ptr->muta1 & MUT1_RECALL)
		{
			power_desc[num].name = "recall";
			power_desc[num].level = 17;
			power_desc[num].cost = 50;
			power_desc[num].fail = 100 - racial_chance(17, A_INT, 16);
			power_desc[num++].number = MUT1_RECALL;
		}

		if (p_ptr->muta1 & MUT1_BANISH)
		{
			power_desc[num].name = "banish evil";
			power_desc[num].level = 25;
			power_desc[num].cost = 25;
			power_desc[num].fail = 100 - racial_chance(25, A_WIS, 18);
			power_desc[num++].number = MUT1_BANISH;
		}

		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			power_desc[num].name = "cold touch";
			power_desc[num].level = 2;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(2, A_CON, 11);
			sprintf(power_desc[num].desc, "dam 6d8 + %d", lvl / 3);
			power_desc[num++].number = MUT1_COLD_TOUCH;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BOLT)
		{
			power_desc[num].name = "shard bolt";
			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(3, A_CON, 7);
			sprintf(power_desc[num].desc, "dam %dd%d", 3 + ((lvl - 1) / 10), 5);
			power_desc[num++].number = MUT1_SHARD_BOLT;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BLAST)
		{
			power_desc[num].name = "shard blast";
			power_desc[num].level = 7;
			power_desc[num].cost = 4;
			power_desc[num].fail = 100 - racial_chance(7, A_CON, 10);
			sprintf(power_desc[num].desc, "dam %dd(%dd%d)", 5, 2 + ((lvl - 1) / 10), 4);
			power_desc[num++].number = MUT1_SHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BLAST)
		{
			power_desc[num].name = "large shard blast";
			power_desc[num].level = 14;
			power_desc[num].cost = 8;
			power_desc[num].fail = 100 - racial_chance(14, A_CON, 12);
			sprintf(power_desc[num].desc, "dam %dd(%dd%d)", 10, 2 + ((lvl - 1) / 10), 4);
			power_desc[num++].number = MUT1_DSHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
		{
			power_desc[num].name = "chainshards";
			power_desc[num].level = 17;
			power_desc[num].cost = 10;
			power_desc[num].fail = 100 - racial_chance(17, A_STR, 16);
			sprintf(power_desc[num].desc, "dam %dd(%dd%d)", 10, 3 + ((lvl - 1) / 10), 5);
			power_desc[num++].number = MUT1_CHAIN_SHARDS;
		}

		if (p_ptr->muta1 & MUT1_ROCKET)
		{
			power_desc[num].name = "rocket";
			power_desc[num].level = 21;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(21, A_STR, 18);
			sprintf(power_desc[num].desc, "dam %d", lvl * 3);
			power_desc[num++].number = MUT1_ROCKET;
		}

		if (p_ptr->muta1 & MUT1_PLAS_BOLT)
		{
			power_desc[num].name = "plasma bolt";
			power_desc[num].level = 27;
			power_desc[num].cost = 14;
			power_desc[num].fail = 100 - racial_chance(27, A_INT, 16);
			sprintf(power_desc[num].desc, "dam %dd(%dd%d)", 3, lvl / 5, 6);
			power_desc[num++].number = MUT1_PLAS_BOLT;
		}

		if (p_ptr->muta1 & MUT1_BFG)
		{
			power_desc[num].name = "BFG";
			power_desc[num].level = 40;
			power_desc[num].cost = 50;
			power_desc[num].fail = 100 - racial_chance(40, A_CON, 18);
			sprintf(power_desc[num].desc, "dam %d + (%dd%d)d(%dd%d)", 200, 5, 2, 50, 5);
			power_desc[num++].number = MUT1_BFG;
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
		I2A(0), (num <= 26) ? I2A(num - 1) : '0' + num - 27);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Advance the page if requested */
		if ((choice == ' ') && redraw && (num > 20)) page = (page + 1) % 2;

		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int max = MIN(page * 20 + 20, num);
				int cur = page * 20;
				char dummy[80];
				char letter;

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				/* Print header(s) */
				prt("                            Lv Cost Fail Desc", y++, x);

				/* Print list */
				while (cur < max)
				{
					/* letter/number for power selection */
					if (cur < 26)
					{
						letter = I2A(cur);
					}
					else
					{
						letter = '0' + cur - 26;
					}

					sprintf(dummy, " %c) %-23.23s %2d %4d %3d%% %s", letter,
							  power_desc[cur].name, power_desc[cur].level,
							  power_desc[cur].cost, power_desc[cur].fail, power_desc[cur].desc);

					prt(dummy, y + (cur % 20), x);
					cur++;
				}
			}

			/* Hide the list */
			else if (choice != ' ')
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag)
	{
		energy_use = 0;
		return;
	}

	if (!power_desc[i].number)
	{
		cmd_racial_power_aux(power_desc[i].number);
	}
	else
	{
		mutation_power_aux(power_desc[i].number);
	}

	/* Success */
	return;
}
