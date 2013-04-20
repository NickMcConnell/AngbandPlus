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
	cave_type *c_ptr;

	/* Player grid */
	c_ptr = &cave[py][px];

	/* Verify stairs */
	if (c_ptr->feat != FEAT_LESS)
	{
		msg_print("I see no up staircase here.");
		return;
	}

	else
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

#if 0
/* I'm experimenting without this... otherwise the monsters get to
act first when we go up stairs, theoretically resulting in a possible
insta-death. */
	/* Hack -- take a turn */
	energy_use = 100;
#else
	energy_use = 0;
#endif


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
			dun_level--;
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
	cave_type *c_ptr;
	bool go_down = FALSE;
	bool fall_trap = FALSE;

	/* Player grid */
	c_ptr = &cave[py][px];

	if (c_ptr->feat == (FEAT_TRAP_HEAD + 0x00)) fall_trap = TRUE;


	/* Verify stairs */
	if ((c_ptr->feat != FEAT_MORE) && !(fall_trap))
	{
		msg_print("I see no down staircase here.");
		return;
	}
	else
	{
		if (!dun_level)
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

#if 0
			/* Hack -- take a turn */
			energy_use = 100;
#else
			energy_use = 0;
#endif

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
			dun_level++;
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
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;


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
		if (small && (rand_int(100) < 75))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;
		}
		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(q_ptr, TRUE, FALSE)) continue;
		}

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = dun_level;

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
			if (randint(100)<dun_level)
				activate_hi_summon();
			else
				(void)summon_specific(y, x, dun_level, 0);
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
			/* Only Rogues get XP -- Gumby */
			if (p_ptr->pclass == CLASS_ROGUE) gain_exp(1);
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

	cave_type *c_ptr;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get requested grid */
	c_ptr = &cave[y][x];

	/* Jammed door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked door */
	else if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = c_ptr->feat - FEAT_DOOR_HEAD;

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
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Only Rogues gain XP */
			if (p_ptr->pclass == CLASS_ROGUE) gain_exp(1);
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
	int y, x, dir;
	
	s16b o_idx;

	cave_type *c_ptr;

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

		/* Get requested grid */
		c_ptr = &cave[y][x];

		/* Check for chest */
		o_idx = chest_check(y, x);

		/* Nothing useful */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_DOOR_TAIL)) &&
		    !o_idx)
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
	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

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
	int y, x, dir;

	cave_type *c_ptr;

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

		/* Get grid and contents */
		c_ptr = &cave[y][x];

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
	cave_type	*c_ptr = &cave[y][x];

	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Forget the wall */
	c_ptr->info &= ~(CAVE_MARK);

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
 * Assumes that the destination is a wall, a vein, a secret
 * door, or rubble.
 *
 * Assumes that no monster is blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x, int dir)
{
	cave_type *c_ptr;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Sound */
	sound(SOUND_DIG);

	/* Titanium */
	if (c_ptr->feat >= FEAT_PERM_EXTRA)
	{
		msg_print("This seems to be permanent rock.");
	}

	/* Granite */
	else if (c_ptr->feat >= FEAT_WALL_EXTRA)
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
			msg_print("You tunnel into the granite wall.");
			more = TRUE;
		}
	}

	/* Quartz / Magma */
	else if (c_ptr->feat >= FEAT_MAGMA)
	{
		bool okay = FALSE;
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
	else if (c_ptr->feat == FEAT_RUBBLE)
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
	else /* if (c_ptr->feat == FEAT_SECRET) */
	{
		/* Message, keep digging */
		msg_print("You tunnel into the granite wall.");
		more = TRUE;

		/* Hack -- Search */
		search();
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
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Oops */
		if (cave_floor_grid(c_ptr) || ((c_ptr->feat >= FEAT_MINOR_GLYPH) &&
		    (c_ptr->feat <= FEAT_PATTERN_XTRA2)))
		{
			/* Message */
			msg_print("You cannot tunnel through air.");
		}

		/* No tunnelling through doors */
		else if (c_ptr->feat < FEAT_SECRET)
		{
			/* Message */
			msg_print("You cannot tunnel through doors.");
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

		if(p_ptr->pclass == CLASS_ROGUE) gain_exp(o_ptr->pval);
		else gain_exp(o_ptr->pval / 2);

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

	cave_type *c_ptr;

	cptr name;

	bool more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Access trap name */
	name = (f_name + f_info[c_ptr->feat].name);

	/* Get the "disarm" factor */
	i = p_ptr->skill_dis;

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
		msg_format("You have disarmed the %s.", name);

		/* Reward */
		if(p_ptr->pclass == CLASS_ROGUE) gain_exp(power);
		else gain_exp(power / 2);

		/* Forget the trap */
		c_ptr->info &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(y, x, FEAT_FLOOR);

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
	
	s16b o_idx;

	cave_type *c_ptr;

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

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Check for chests */
		o_idx = chest_check(y, x);

		/* Disarm a trap */
		if (!((c_ptr->feat >= FEAT_TRAP_HEAD) &&
		    (c_ptr->feat <= FEAT_TRAP_TAIL)) &&
		    !o_idx)
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

	cave_type	*c_ptr;

	bool		more = FALSE;


	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Message */
	msg_print("You smash into the door!");

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = ((c_ptr->feat - FEAT_DOOR_HEAD) & 0x07);

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
			cave_set_feat(y, x, FEAT_BROKEN);
		}

		/* Open the door */
		else
		{
			cave_set_feat(y, x, FEAT_OPEN);
		}

		/* Sound */
		sound(SOUND_OPENDOOR);

		/* Hack -- Fall through the door */
		move_player(dir, FALSE);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE);
		p_ptr->update |= (PU_DISTANCE);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
	         p_ptr->lev)
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

	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Bash location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Nothing useful */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_DOOR_TAIL)))
		{
			/* Message */
			msg_print("You see nothing there to bash.");
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
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* Take a turn */
		energy_use = 100;
		
		/* Attack monsters */
		if (c_ptr->m_idx)
		{
			/* Attack */
			py_attack(y, x);
		}

		/* Tunnel through walls */
		else if ((c_ptr->feat >= FEAT_SECRET) &&
		    (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x, dir);
		}

		/* Bash jammed doors */
		else if ((c_ptr->feat >= FEAT_DOOR_HEAD + 0x08) &&
		    (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_bash_aux(y, x, dir);
		}

		/* Open closed doors */
		else if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		    (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_open_aux(y, x, dir);
		}

		/* Disarm traps */
		else if ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
		    (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_disarm_aux(y, x, dir);
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
	int y, x, dir, item;

	cave_type *c_ptr;


	/* Get a "repeated" direction */
	if (get_rep_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Require closed door */
		if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_DOOR_TAIL)))
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

			/* Convert "locked" to "stuck" XXX XXX XXX */
			if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

			/* Add one spike to the door */
			if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;

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
	cave_type *c_ptr = &cave[py][px];


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
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		{
			if (!(o_ptr->sval == SV_LITE_FEANORAN_LAMP))
			return (50);
		}

		case TV_SCROLL:
		case TV_ARROW:
		case TV_SKELETON:
		{
			return (50);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		case TV_SPIKE:
		{
			return (25);
		}
	}

	/* Rarely break */
	return (10);
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

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;
	object_type *j_ptr;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

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
	missile_char = object_char(q_ptr);


	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Use a base distance */
	tdis = 10;

	/* Base damage from thrown object plus launcher bonus */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d + j_ptr->to_d;

	/* Actually "fire" the object */
	/*
	 * Must adjust so that Weaponmasters don't get their level bonus and
	 * Priests don't get penalized for icky_wield.
	 *							-- Gumby
	 */
	if ((p_ptr->pclass == CLASS_WEAPONMASTER) &&
	    (inventory[INVEN_WIELD].tval == p_ptr->wm_choice))
		bonus = ((p_ptr->to_h - p_ptr->lev) + q_ptr->to_h + j_ptr->to_h);
	else if ((p_ptr->pclass == CLASS_PRIEST) && (p_ptr->icky_wield))
		bonus = (p_ptr->to_h + q_ptr->to_h + j_ptr->to_h + 15);
	else
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


		/* Monster here, Try to hit it */
		if (cave[y][x].m_idx)
		{
			cave_type *c_ptr = &cave[y][x];

			monster_type *m_ptr = &m_list[c_ptr->m_idx];
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
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);

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

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

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
	missile_char = object_char(q_ptr);


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d;

	/* Chance of hitting - adjusted for Weaponmasters -- Gumby */
	if ((p_ptr->pclass == CLASS_WEAPONMASTER) &&
	    (inventory[INVEN_WIELD].tval == p_ptr->wm_choice))
		chance = (p_ptr->skill_tht + ((p_ptr->to_h - p_ptr->lev) * BTH_PLUS_ADJ));
	else
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


		/* Monster here, Try to hit it */
		if (cave[y][x].m_idx)
		{
			cave_type *c_ptr = &cave[y][x];

			monster_type *m_ptr = &m_list[c_ptr->m_idx];
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
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr);
				tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

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
	j = (hit_body ? breakage_chance(q_ptr) : 0);

         /* Potions smash open */
	if (k_info[q_ptr->k_idx].tval == TV_POTION)
	{
		if ((hit_body) || (!cave_floor_bold(ny, nx)) || (randint(100) < j))
		{
			/* Message */
			msg_format("The %s shatters!", o_name);

			if (potion_smash_effect(1, y, x, q_ptr->sval))
			{
				if (cave[y][x].m_idx && (m_list[cave[y][x].m_idx].smart & SM_FRIEND))
				{
					char m_name[80];
					monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
					msg_format("%s gets angry!", m_name);
					m_list[cave[y][x].m_idx].smart &= ~SM_FRIEND;
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
	drop_near(q_ptr, j, y, x);
}


/* Note: return value indicates that we have succesfully used the power */

bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
	bool use_hp = FALSE;

	/* Use hit points when you don't have enough spell points */
	if (p_ptr->csp < cost) use_hp = TRUE;

	if (p_ptr->lev < min_level)
	{
		msg_format("You need to attain level %d to use this power.", min_level);
		energy_use = 0;
		return FALSE;
	}

	else if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		energy_use = 0;
		return FALSE;
	}

	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!(get_check("Really use the power in your weakened state? ")))
		{
			energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->stun) difficulty += p_ptr -> stun;
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level)/3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	energy_use = 100;
	if (use_hp) take_hit (((cost / 2) + (randint(cost / 2))),
	    "concentrating too hard");
	else p_ptr->csp -= (cost / 2 ) + (randint(cost / 2));

	p_ptr->redraw |= (PR_HP);

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);


	/* Success? */
	if (randint(p_ptr->stat_cur[use_stat]) >=
	    ((difficulty / 2) + randint(difficulty / 2)))
	return TRUE;

	msg_print("You've failed to concentrate hard enough.");
	return FALSE;
}

static void cmd_racial_power_aux (void)
{
	s16b plev = p_ptr->lev;
	int dir = 0;
	int Type = (randint(3)==1?GF_COLD:GF_FIRE);
	cptr Type_desc = (Type == GF_COLD?"cold":"fire");
	object_type *q_ptr;
	object_type forge;
	int dummy = 0;
	cave_type *c_ptr;
	int y = 0, x = 0;
    
    
	switch(p_ptr->prace)
	{
		case RACE_HUMAN:
			if (racial_aux(15, 10, A_WIS, 10))
			{
				msg_print("You take stock of your abilities.");
				(void)self_knowledge();
			}
			break;

		case RACE_HALF_ELF:
			if (racial_aux(15, 5, A_WIS, 10))
			{
				msg_print("You look for your animal friends.");
				(void)detect_monsters_xxx(RF3_ANIMAL);
			}
			break;

		case RACE_ELF:
			if (racial_aux(10, 5, A_WIS, 10))
			{
				msg_print("You look for your animal friends.");
				(void)detect_monsters_xxx(RF3_ANIMAL);
			}
			break;

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
			if (racial_aux(15,10,A_INT,10))
			{
				/* Get local object */
				q_ptr = &forge;

				/* Create the item */
				object_prep(q_ptr, 21);

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, py, px);
				msg_print("You cook some food.");
			}
			break;

		case RACE_GNOME:
			if (racial_aux(5, (5+(plev/5)), A_INT, 12))
			{
				msg_print("Blink!");
				teleport_player(10 + (plev));
			}
			break;

		case RACE_HALF_ORC:
			if (racial_aux(3, 5, A_WIS,
			(p_ptr->pclass == CLASS_WARRIOR?5:10)))
			{
				msg_print("You play tough.");
				(void)set_afraid(0);
			}
			break;

		case RACE_HALF_TROLL:
			if (racial_aux(10, 12, A_WIS,
			(p_ptr->pclass == CLASS_WARRIOR?6:12)))
			{
				msg_print("RAAAGH!");
				(void)set_afraid(0);

				(void)set_shero(p_ptr->shero + 10 + randint(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_GAMBOLT:
			if (racial_aux(20, (p_ptr->lev), A_CHR, 15))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You act charming.");
				(void)charm_monster(dir, plev);
			}
			break;

		case RACE_BARBARIAN:
			if (racial_aux(8, 10, A_WIS, (p_ptr->pclass == CLASS_WARRIOR?6:12)))
			{
				msg_print("RAAARGH!");
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
				int x,y;
				cave_type *c_ptr;

				if (!get_rep_dir(&dir)) return;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				      (c_ptr->feat <= FEAT_WALL_SOLID)))

				{
					msg_print("You wave your fists in the air.");
					break;
				}
				msg_print("You smash your fist into the wall!");
				wall_to_mud(dir);
			}
			break;

		case RACE_HALF_TITAN:
			if (racial_aux(25, 20, A_INT, 12))
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
				fire_bolt(GF_MISSILE, dir, (3 * p_ptr->lev));
			}
			break;

		case RACE_YEEK:
			if (racial_aux(5, 10, A_WIS, 10))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You make a horrible scream!");
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_KLACKON:
			if (racial_aux(9, 9, A_DEX, 14))
			{
				if (!(get_aim_dir(&dir))) break;
				msg_print("You spit acid.");
				if (p_ptr->lev < 25)
					fire_bolt(GF_ACID, dir, plev * 2);
				else
					fire_ball(GF_ACID, dir, plev * 2, 2);
			}
			break;

		case RACE_KOBOLD:
			if (racial_aux(12, 8, A_DEX, 14))
			{
				if(!get_aim_dir(&dir)) break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev * 2);
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
				    damroll(3 + ((plev - 1) / 3), 4));
			}
			break;

		case RACE_DRACONIAN:
			if (randint(100)<p_ptr->lev)
			{
				switch (p_ptr->pclass)
				{
					case CLASS_WARRIOR:
					case CLASS_RANGER:
					case CLASS_WEAPONMASTER:
						if (randint(3)==1)
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
						if (randint(3)==1)
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
						if (randint(3)!=1)
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
						if (randint(3)!=1)
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
						if (randint(3)!=1)
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
						if (p_ptr->realm1 == REALM_DEATH)
						{
							Type = GF_HELL_FIRE;
							Type_desc = "hellfire";
						}
						else if (p_ptr->realm1 == REALM_LIFE)
						{
							Type = GF_HOLY_FIRE;
							Type_desc = "holy fire";
						}
						break;
					case CLASS_ROGUE:
						if (p_ptr->realm1 == REALM_DEATH)
						{
							Type = GF_POIS;
							Type_desc = "poison";
						}
						else if (p_ptr->realm1 == REALM_TRUMP)
						{
							Type = GF_NEXUS;
							Type_desc = "nexus";
						}
						else if (p_ptr->realm1 == REALM_SORCERY)
						{
							Type = GF_CONFUSION;
							Type_desc = "confusion";
						}
						break;
				}
			}
			if (racial_aux(1, p_ptr->lev, A_CON, 16))
			{
				if (!get_aim_dir(&dir)) break;
				msg_format("You breathe %s.", Type_desc);
				fire_ball(Type, dir, (p_ptr->lev)*3,
				    ((p_ptr->lev)/15) + 1);
			}
			break;

		case RACE_MIND_FLAYER:
			if (racial_aux(15, 12, A_INT, 14))
			{
				if (!get_aim_dir(&dir)) break;
				else
				{
					msg_print("You concentrate and your eyes glow red...");
					fire_bolt(GF_PSI, dir, plev * 2);
				}
			}
			break;

		case RACE_IMP:
			if (racial_aux(9, 15, A_DEX, 15))
			{
				if (!get_aim_dir(&dir)) break;
				if (p_ptr->lev >= 30)
				{
					msg_print("You cast a ball of fire.");
					fire_ball(GF_FIRE, dir, plev * 2, 2);
				}
				else
				{
					msg_print("You cast a bolt of fire.");
					fire_bolt_or_beam(plev * 2, GF_FIRE,
							dir, plev * 2);
				}
			}
			break;

		case RACE_GOLEM:
			if (racial_aux(20, 15, A_CON, 8))
			{
				msg_print("You feel hard.");
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
			if (racial_aux(2, (1+(plev/3)), A_CON, 9))
			{
				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir)) break;   /* was get_aim_dir */
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!(c_ptr->m_idx))
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");
				dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
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

		case RACE_SPECTRE:
			if (racial_aux(4, 6, A_INT, 3))
			{
				msg_print("You emit an eldritch howl!");
				if (!get_aim_dir(&dir)) break;
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_SPRITE:
			if (racial_aux(8, 12, A_INT, 15))
			{
				msg_print("You throw some magic dust...");
				if (p_ptr->lev < 25)
					sleep_monsters_touch();
				else
					(void)sleep_monsters();
			}
			break;

		default:
			msg_print("This race has no bonus power.");
			energy_use = 0;
	}

	p_ptr->redraw |= (PR_HP | PR_MANA);
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}



/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	int                     i = 0;

	int                     Power = -1;
	int                     num = 0, dir = 0;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
	int             ask;

	char            choice;

	char            out_val[160];

	int lvl = p_ptr->lev;
	bool warrior = ((p_ptr->pclass == CLASS_WARRIOR)?TRUE:FALSE);

	int pets = 0, pet_ctr = 0;
	bool all_pets = FALSE;
	monster_type * m_ptr;

	bool has_racial = FALSE;

	cptr racial_power = "(none)";

	for (num = 0; num < 36; num++)
	{
		powers[num] = 0;
		strcpy (power_desc[num], "");
	}

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to use any powers!");
		energy_use = 0;
		return;
	}

	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
			if (lvl < 15)
				racial_power = "self knowledge (racial, lvl 15, cost 10)";
			else
				racial_power = "self knowledge (racial, cost 10, WIS 10@15)";
			has_racial = TRUE;
			break;

		case RACE_HALF_ELF:
			if (lvl < 15)
				racial_power = "detect animals (racial, lvl 15, cost 5)";
			else
				racial_power = "detect animals (racial, cost 5, WIS 10@15)";
			has_racial = TRUE;
			break;

		case RACE_ELF:
			if (lvl < 10)
				racial_power = "detect animals (racial, lvl 10, cost 5)";
			else
				racial_power = "detect animals (racial, cost 5, WIS 10@10)";
			has_racial = TRUE;
			break;

		case RACE_DWARF:
			if (lvl < 5)
				racial_power = "detect doors+traps (racial, lvl 5, cost 5)";
			else
				racial_power = "detect doors+traps (racial, cost 5, WIS 12@5)";
			has_racial = TRUE;
			break;

		case RACE_NIBELUNG:
			if (lvl < 10)
				racial_power = "detect doors+traps (racial, lvl 10, cost 5)";
			else
				racial_power = "detect doors+traps (racial, cost 5, WIS 10@10)";
			has_racial = TRUE;
			break;

		case RACE_HOBBIT:
			if (lvl < 15)
				racial_power = "create food        (racial, lvl 15, cost 10)";
			else
				racial_power = "create food        (racial, cost 10, INT 10@15)";
			has_racial = TRUE;
			break;

		case RACE_GNOME:
			if (lvl < 5)
				racial_power = "teleport           (racial, lvl 5, cost 5 + lvl/5)";
			else
				racial_power = "teleport           (racial, cost 5 + lvl/5, INT 12@5)";
			has_racial = TRUE;
			break;

		case RACE_HALF_ORC:
			if (lvl < 3)
				racial_power = "remove fear        (racial, lvl 3, cost 5)";
			else if (warrior)
				racial_power = "remove fear        (racial, cost 5, WIS 5@3)";
			else
				racial_power = "remove fear        (racial, cost 5, WIS 10@3)";
			has_racial = TRUE;
			break;

		case RACE_HALF_TROLL:
			if (lvl < 10)
				racial_power = "berserk            (racial, lvl 10, cost 12)";
			else if (warrior)
				racial_power = "berserk            (racial, cost 12, WIS 6@10)";
			else
				racial_power = "berserk            (racial, cost 12, WIS 12@10)";
			has_racial = TRUE;
			break;

		case RACE_BARBARIAN:
			if (lvl < 8)
				racial_power = "berserk            (racial, lvl 8, cost 10)";
			else if (warrior)
				racial_power = "berserk            (racial, cost 10, WIS 6@8)";
			else
				racial_power = "berserk            (racial, cost 10, WIS 12@8)";
			has_racial = TRUE;
			break;

		case RACE_GAMBOLT:
			if (lvl < 20)
				racial_power = "charm              (racial, lvl 20, cost lvl)";
			else
				racial_power = "charm              (racial, cost lvl, CHR, 15@20)";
			has_racial = TRUE;
			break;

		case RACE_HALF_OGRE:
			if (lvl < 25)
				racial_power = "explosive rune     (racial, lvl 25, cost 35)";
			else
				racial_power = "explosive rune     (racial, cost 35, INT 15@25)";
			has_racial = TRUE;
			break;

		case RACE_HALF_GIANT:
			if (lvl < 20)
				racial_power = "smash down a wall  (racial, lvl 20, cost 10)";
			else
				racial_power = "smash down a wall  (racial, cost 10, STR 12@20)";
			has_racial = TRUE;
			break;

		case RACE_HALF_TITAN:
			if (lvl < 35)
				racial_power = "probing            (racial, lvl 25, cost 20)";
			else
				racial_power = "probing            (racial, cost 20, INT 12@25)";
			has_racial = TRUE;
			break;

		case RACE_CYCLOPS:
			if (lvl < 20)
				racial_power = "throw boulder      (racial, lvl 20, cost 15, dam 3*lvl)";
			else
				racial_power = "throw boulder      (racial, cost 15, dam 3*lvl, STR 12@20)";
			has_racial = TRUE;
			break;

		case RACE_YEEK:
			if (lvl < 5)
				racial_power = "scare monster      (racial, lvl 5, cost 10)";
			else
				racial_power = "scare monster      (racial, cost 10, WIS 10@5)";
			has_racial = TRUE;
			break;

		case RACE_SPECTRE:
			if (lvl < 4)
				racial_power = "scare monster      (racial, lvl 4, cost 3)";
			else
				racial_power = "scare monster      (racial, cost 3, INT 3@5)";
			has_racial = TRUE;
			break;

		case RACE_KLACKON:
			if (lvl < 9)
				racial_power = "spit acid          (racial, lvl 9, cost 9, dam lvl*2)";
			else
				racial_power = "spit acid          (racial, cost 9, dam lvl*2, DEX 14@9)";
			has_racial = TRUE;
			break;

		case RACE_KOBOLD:
			if (lvl < 12)
				racial_power = "poison dart        (racial, lvl 12, cost 8, dam lvl*2)";
			else
				racial_power = "poison dart        (racial, cost 8, dam lvl*2, DEX 14@12)";
			has_racial = TRUE;
			break;

		case RACE_DARK_ELF:
			if (lvl < 2)
				racial_power = "magic missile      (racial, lvl 2, cost 2)";
			else
				racial_power = "magic missile      (racial, cost 2, INT 9@2)";
			has_racial = TRUE;
			break;

		case RACE_DRACONIAN:
			racial_power = "breath weapon      (racial, cost lvl, dam 3*lvl, CON 16@1)";
			has_racial = TRUE;
			break;

		case RACE_MIND_FLAYER:
			if (lvl < 15)
				racial_power = "mind blast         (racial, lvl 15, cost 12, dam lvl*2)";
			else
				racial_power = "mind blast         (racial, cost 12, dam lvl*2, INT 14@15)";
			has_racial = TRUE;
			break;

		case RACE_IMP:
			if (lvl < 9)
				racial_power = "fire bolt/ball     (racial, lvl 9/30, cost 15, dam lvl*2)";
			else
				racial_power = "fire bolt/ball(30) (racial, cost 15, dam lvl*2, DEX 15@9)";
			has_racial = TRUE;
			break;

		case RACE_GOLEM:
			if (lvl < 20)
				racial_power = "stone skin         (racial, lvl 20, cost 15, dur 30+d20)";
			else
				racial_power = "stone skin         (racial, cost 15, dur 30+d20, CON 8@20)";
			has_racial = TRUE;
			break;

		case RACE_SKELETON:
		case RACE_ZOMBIE:
			if (lvl < 30)
				racial_power = "restore life       (racial, lvl 30, cost 30)";
			else
				racial_power = "restore life       (racial, cost 30, WIS 18@30)";
			has_racial = TRUE;
			break;

		case RACE_VAMPIRE:
			if (lvl < 2)
				racial_power = "drain life         (racial, lvl 2, cost 1 + lvl/3) ";
			else
				racial_power = "drain life         (racial, cost 1 + lvl/3, CON 9@2)";
			has_racial = TRUE;
			break;

		case RACE_SPRITE:
			if (lvl < 8)
				racial_power = "sleeping dust      (racial, lvl 8, cost 12)";
			else
				racial_power = "sleeping dust      (racial, cost 12, INT 15@8)";
			has_racial = TRUE;
			break;
	}

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (m_ptr->smart & (SM_FRIEND)) pets++;
	}

	if (pets > 0)
	{
		strcpy(power_desc[num], "dismiss pets");
		powers[num++] = -2;
	}


	if (!(has_racial) && !(p_ptr->muta1) && !(pets > 0))
	{
		msg_print("You have no powers to activate.");
		energy_use = 0;
		return;
	}

	if (has_racial)
	{
		powers[0] = -1;
		strcpy(power_desc[0], racial_power);
		num++;
	}

	if (p_ptr->muta1)
	{
		int lvl = p_ptr->lev;

		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			if (lvl < 9)
				strcpy(power_desc[num],"spit acid          (mutation, lvl 9, cost 9, dam lvl*2)");
			else
				strcpy(power_desc[num],"spit acid          (mutation, cost 9, dam lvl*2, DEX 15@9)");
			powers[num++] = MUT1_SPIT_ACID;
		}

		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"fire breath        (mutation, lvl 20, cost lvl, dam lvl*3)");
			else
				strcpy(power_desc[num],"fire breath        (mutation, cost lvl, dam lvl*3, CON 18@20)");
			powers[num++] = MUT1_BR_FIRE;
		}

		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"hypnotic gaze      (mutation, lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"hypnotic gaze      (mutation, cost 12, CHR 18@12)");
			powers[num++] = MUT1_HYPN_GAZE;
		}

		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
			if (lvl < 9)
				strcpy(power_desc[num],"telekinesis        (mutation, lvl 9, cost 9)");
			else
				strcpy(power_desc[num],"telekinesis        (mutation, cost 9, WIS 14@9)");
			powers[num++] = MUT1_TELEKINES;
		}

		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"teleport           (mutation, lvl 7, cost 7)");
			else
				strcpy(power_desc[num],"teleport           (mutation, cost 7, WIS 15@7)");
			powers[num++] = MUT1_VTELEPORT;
		}

		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			if (lvl < 5)
				strcpy(power_desc[num],"mind blast         (mutation, lvl 5, cost 3)");
			else
				strcpy(power_desc[num],"mind blast         (mutation, cost 3, WIS 15@7)");
			powers[num++] = MUT1_MIND_BLST;
		}

		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			if (lvl < 15)
				strcpy(power_desc[num],"emit radiation     (mutation, lvl 15, cost 15, dam lvl*3)");
			else
				strcpy(power_desc[num],"emit radiation     (mutation, cost 15, dam lvl*3 CON 14@15)");
			powers[num++] = MUT1_RADIATION;
		}

		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			if (lvl < 13)
				strcpy(power_desc[num],"vampiric drain     (mutation, lvl 13, cost lvl)");
			else
				strcpy(power_desc[num],"vampiric drain     (mutation, cost lvl, CON 14@13)");
			powers[num++] = MUT1_VAMPIRISM;
		}

		if (p_ptr->muta1 & MUT1_BLINK)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"blink              (mutation, lvl 3, cost 3)");
			else
				strcpy(power_desc[num],"blink              (mutation, cost 3, WIS 12@3)");
			powers[num++] = MUT1_BLINK;
		}

		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"eat rock           (mutation, lvl 8, cost 12)");
			else
				strcpy(power_desc[num],"eat rock           (mutation, cost 12, CON 18@8)");
			powers[num++] = MUT1_EAT_ROCK;
		}

		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"shriek             (mutation, lvl 20, cost 14, dam lvl*3)");
			else
				strcpy(power_desc[num],"shriek             (mutation, cost 14, dam lvl*3, CON 16@20)");
			powers[num++] = MUT1_SHRIEK;
		}

		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"illuminate         (mutation, lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"illuminate         (mutation, cost 2, INT 10@3)");
			powers[num++] = MUT1_ILLUMINE;
		}

		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"detect curses      (mutation, lvl 7, cost 14)");
			else
				strcpy(power_desc[num],"detect curses      (mutation, cost 14, WIS 14@7)");
			powers[num++] = MUT1_DET_CURSE;
		}

		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"berserk            (mutation, lvl 8, cost 8)");
			else
				strcpy(power_desc[num],"berserk            (mutation, cost 8, STR 14@8)");
			powers[num++] = MUT1_BERSERK;
		}

		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			if (lvl < 18)
				strcpy(power_desc[num],"polymorph          (mutation, lvl 18, cost 20)");
			else
				strcpy(power_desc[num],"polymorph          (mutation, cost 20, CON 18@18)");
			powers[num++] = MUT1_POLYMORPH;
		}

		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"Midas touch        (mutation, lvl 20, cost 15)");
			else
				strcpy(power_desc[num],"Midas touch        (mutation, cost 15, INT 12@20)");
			powers[num++] = MUT1_MIDAS_TCH;
		}

		if (p_ptr->muta1 & MUT1_SUMMON_M)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"summon monsters    (mutation, lvl 10, cost lvl / 2)");
			else
				strcpy(power_desc[num],"summon monsters    (mutation, cost lvl / 2, CON 10@10)");
			powers[num++] = MUT1_SUMMON_M;
		}

		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			strcpy(power_desc[num],"grow molds         (mutation, cost 6, CON 14@1)");
			powers[num++] = MUT1_GROW_MOLD;
		}

		if (p_ptr->muta1 & MUT1_RESIST)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"resist elements    (mutation, lvl 10, cost 12)");
			else
				strcpy(power_desc[num],"resist elements    (mutation, cost 12, CON 12@10)");
			powers[num++] = MUT1_RESIST;
		}

		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			if (lvl < 12)
				strcpy(power_desc[num],"earthquake         (mutation, lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"earthquake         (mutation, cost 12, STR 16@12)");
			powers[num++] = MUT1_EARTHQUAKE;
		}

		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"dazzle             (mutation, lvl 7, cost 15)");
			else
				strcpy(power_desc[num],"dazzle             (mutation, cost 15, CHR 8@7)");
			powers[num++] = MUT1_DAZZLE;
		}

		if (p_ptr->muta1 & MUT1_RECALL)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"word of recall     (mutation, lvl 17, cost 50)");
			else
				strcpy(power_desc[num],"word of recall     (mutation, cost 50, INT 16@17)");
			powers[num++] = MUT1_RECALL;
		}

		if (p_ptr->muta1 & MUT1_BANISH)
		{
			if (lvl < 25)
				strcpy(power_desc[num],"banish evil        (mutation, lvl 25, cost 25)");
			else
				strcpy(power_desc[num],"banish evil        (mutation, cost 25, WIS 18@25)");
			powers[num++] = MUT1_BANISH;
		}

		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			if (lvl < 2)
				strcpy(power_desc[num],"cold touch         (mutation, lvl 2, cost 2, dam lvl*3)");
			else
				strcpy(power_desc[num],"cold touch         (mutation, cost 2, dam lvl*3, CON 11@2)");
			powers[num++] = MUT1_COLD_TOUCH;
		}

		if (p_ptr->muta1 & MUT1_MISSILE)
		{
			strcpy(power_desc[num],"magic missile      (mutation, cost 1, CON 5@1)");
			powers[num++] = MUT1_MISSILE;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BOLT)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"shard bolt         (mutation, lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"shard bolt         (mutation, cost 2, CON 7@3)");
			powers[num++] = MUT1_SHARD_BOLT;
		}

		if (p_ptr->muta1 & MUT1_SHARD_BLAST)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"shard blast        (mutation, lvl 7, cost 4)");
			else
				strcpy(power_desc[num],"shard blast        (mutation, cost 4, STR 10@7)");
			powers[num++] = MUT1_SHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_DSHARD_BLAST)
		{
			if (lvl < 14)
				strcpy(power_desc[num],"large shard blast  (mutation, lvl 14, cost 8)");
			else
				strcpy(power_desc[num],"large shard blast  (mutation, cost 8, CON 12@14)");
			powers[num++] = MUT1_DSHARD_BLAST;
		}

		if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"rapid shards       (mutation, lvl 17, cost 10)");
			else
				strcpy(power_desc[num],"rapid shards       (mutation, cost 10, STR 16@17)");
			powers[num++] = MUT1_CHAIN_SHARDS;
		}

		if (p_ptr->muta1 & MUT1_ROCKET)
		{
			if (lvl < 21)
				strcpy(power_desc[num],"rocket             (mutation, lvl 21, cost 15, dam lvl*4)");
			else
				strcpy(power_desc[num],"rocket             (mutation, cost 15, dam lvl*4, STR 18@21)");
			powers[num++] = MUT1_ROCKET;
		}
	}


	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;


	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
	    I2A(0), I2A(num - 1));

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 13;
				int ctr = 0;
				char dummy[80];

				strcpy (dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}

				prt ("", y + ctr, x);
			}

			/* Hide the list */
			else
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

		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

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

	if (powers[i]<=0)
	{
		if (powers[i] == -1) cmd_racial_power_aux();
		else
		{
			int Dismissed = 0;

			if (get_check("Dismiss all pets? ")) all_pets = TRUE;

			/* Process the monsters (backwards) */
			for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
			{
				/* Access the monster */
				m_ptr = &m_list[pet_ctr];
				if (m_ptr->smart & (SM_FRIEND)) /* Get rid of it! */
				{
					bool delete_this = FALSE;

					if (all_pets)
						delete_this = TRUE;
					else
					{
						char friend_name[80], check_friend[80];
						monster_desc(friend_name, m_ptr, 0x80);
						sprintf(check_friend, "Dismiss %s? ", friend_name);

						if (get_check(check_friend))
							delete_this = TRUE;
					}

					if (delete_this)
					{
						delete_monster_idx(pet_ctr);
						Dismissed++;
					}
				}
			}

			msg_format("You have dismissed %d pet%s.", Dismissed,
			    (Dismissed==1?"":"s"));
		}
	}
	else
	{
		energy_use = 100;

		switch (powers[i])
		{
			case MUT1_SPIT_ACID:
				if (racial_aux(9, 9, A_DEX, 15))
				{
					msg_print("You spit acid...");
					if (get_aim_dir(&dir))
						fire_ball(GF_ACID, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/30));
				}
				break;

			case MUT1_BR_FIRE:
				if (racial_aux(20, p_ptr->lev, A_CON, 18))
				{
					msg_print("You breathe fire...");
					if (get_aim_dir(&dir))
						fire_ball(GF_FIRE, dir, p_ptr->lev * 3, 1 + (p_ptr->lev/20));
				}
				break;

			case MUT1_HYPN_GAZE:
				if (racial_aux(12, 12, A_CHR, 18))
				{
					msg_print("Your eyes look mesmerizing...");
					if (get_aim_dir(&dir))
						(void) charm_monster(dir, p_ptr->lev);
				}
				break;

			case MUT1_TELEKINES:
				if (racial_aux(9, 9, A_WIS, 14))
				{
					msg_print("You concentrate...");
					if (get_aim_dir(&dir))
						fetch(dir, p_ptr->lev * 10, TRUE);
				}
				break;

			case MUT1_VTELEPORT:
				if (racial_aux(7, 7, A_WIS, 15))
				{
					msg_print("Blink!");
					teleport_player(10 + (p_ptr->lev));
				}
				break;

			case MUT1_MIND_BLST:
				if (racial_aux(5, 3, A_WIS, 15))
				{
					msg_print("You concentrate...");
					if (!get_aim_dir(&dir)) return;
						fire_bolt(GF_PSI, dir, damroll(3 + ((p_ptr->lev - 1) / 3), 4));
				}
				break;

			case MUT1_RADIATION:
				if (racial_aux(15, 15, A_CON, 14))
				{
					msg_print("Radiation flows from your body!");
					fire_ball(GF_NUKE, 0, (p_ptr->lev * 3), 3 + (p_ptr->lev / 20));
				}
				break;

			case MUT1_VAMPIRISM:
				if (racial_aux(13, p_ptr->lev, A_CON, 14))
				{
					if (!get_aim_dir(&dir)) return;
					if (drain_life(dir, (p_ptr->lev * 2)))
						hp_player(p_ptr->lev + randint(p_ptr->lev));
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
					cave_type *c_ptr;

					if (!get_rep_dir(&dir)) break;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];
					if (cave_floor_bold(y, x))
					{
						msg_print("You bite into thin air!");
						break;
					}
					else if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
						(c_ptr->feat <= FEAT_PERM_SOLID))
					{
						msg_print("Ouch!  This wall is harder than your teeth!");
						break;
					}
					else if (c_ptr->m_idx)
					{
						msg_print("There's something in the way!");
						break;
					}
					else
					{
						if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
							(c_ptr->feat <= FEAT_RUBBLE))
						{
							msg_print("It could use some salt.");
							(void)set_food(p_ptr->food + 500);
						}
						else if ((c_ptr->feat >= FEAT_MAGMA) &&
							(c_ptr->feat <= FEAT_QUARTZ_K))
						{
							msg_print("This stuff's quite tasty.");
							(void)set_food(p_ptr->food + 1500);
						}
						else
						{
							msg_print("*MUNCH*  *MUNCH*  *MUNCH*  Yummy!");
							(void)set_food(p_ptr->food + 3000);
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

					p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);
					p_ptr->update |= (PU_DISTANCE);
					p_ptr->window |= (PW_OVERHEAD);
				}
				break;

			case MUT1_SHRIEK:
				if (racial_aux(20, 14, A_CON, 16))
				{
					(void)fire_ball(GF_SOUND, 0, 3 * lvl, 8);
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
				if (racial_aux(20, 15, A_INT, 12))
				{
					(void)alchemy();
				}
				break;

			/* Summon pet monsters around the player */
			case MUT1_SUMMON_M:
				if (racial_aux(10, p_ptr->lev / 2, A_CON, 10))
				{
					summon_specific_friendly(py, px, lvl, SUMMON_NO_UNIQUES, TRUE);
				}
				break;

			/* Summon pet molds around the player */
			case MUT1_GROW_MOLD:
				if (racial_aux(1, 6, A_CON, 14))
				{
					int i;
					for (i = 0; i < 8; i++)
					{
						summon_specific_friendly(py, px, lvl, SUMMON_BIZARRE1, FALSE);
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
						num--;
					}
				}
				break;

			case MUT1_EARTHQUAKE:
				if (racial_aux(12, 12, A_STR, 16))
				{
					msg_print("You put your foot down... Hard!");
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
					if (dun_level && (p_ptr->max_dlv > dun_level))
					{
						if (get_check("Reset recall depth? "))
							p_ptr->max_dlv = dun_level;
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
					cave_type *c_ptr;
					monster_type *m_ptr;
					monster_race *r_ptr;

					if (!get_rep_dir(&dir)) return;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];

					if (!c_ptr->m_idx)
					{
						msg_print("You sense no evil there!");
						break;
					}

					m_ptr = &m_list[c_ptr->m_idx];
					r_ptr = &r_info[m_ptr->r_idx];

					if ((r_ptr->flags3 & RF3_EVIL) &&
					    !(r_ptr->flags1 & RF1_QUESTOR) &&
					    !(r_ptr->flags1 & RF1_UNIQUE))
					{
						/* Delete the monster, rather than killing it. */
						delete_monster_idx(c_ptr->m_idx);
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
					cave_type *c_ptr;

					if (!get_rep_dir(&dir)) return;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];

					if (!c_ptr->m_idx)
					{
						msg_print("You wave your hands in the air.");
						break;
					}
					fire_bolt(GF_COLD, dir, 3 * lvl);
				}
				break;

			case MUT1_MISSILE:
				if (racial_aux(1, 1, A_CON, 5))
				{
					msg_print("You cast a magic missile...");
					if (get_aim_dir(&dir))
						fire_bolt(GF_MISSILE, dir, damroll(3 + ((lvl - 1) / 3), 4));
				}
				break;

			case MUT1_SHARD_BOLT:
				if (racial_aux(3, 2, A_CON, 7))
				{
					msg_print("You cast a stinging shard...");
					if (get_aim_dir(&dir))
						fire_bolt(GF_SHARDS, dir, damroll(3 + (lvl / 5), 5));
				}
				break;

			case MUT1_SHARD_BLAST:
				if (racial_aux(7, 4, A_CON, 10))
				{
					msg_print("You cast a volley of shards...");
					if (get_aim_dir(&dir))
						fire_blast(GF_SHARDS, dir, 2 + (lvl / 5), 4, 5, 3);
				}
				break;

			case MUT1_DSHARD_BLAST:
				if (racial_aux(14, 8, A_CON, 12))
				{
					msg_print("You cast a volley of shards...");
					if (get_aim_dir(&dir))
						fire_blast(GF_SHARDS, dir, 2 + (lvl / 5), 4, 10, 4);
				}
				break;

			case MUT1_CHAIN_SHARDS:
				if (racial_aux(17, 10, A_STR, 16))
				{
					msg_print("You launch a barrage of shards...");
					if (get_aim_dir(&dir))
						fire_blast(GF_SHARDS, dir, 3 + (lvl / 5), 5, 10, 2);
				}
				break;

			case MUT1_ROCKET:
				if (racial_aux(21, 15, A_STR, 18))
				{
					msg_print("You fire a rocket...");
					if (get_aim_dir(&dir))
						fire_ball(GF_SHARDS, dir, lvl * 4, 2);
				}
				break;

			default:
				energy_use = 0;
				msg_format("Power %s not implemented. Oops.", powers[i]);
		}
	}

	/* Success */
	return;
}

