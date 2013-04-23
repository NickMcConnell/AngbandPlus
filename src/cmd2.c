/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#if defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM)

static int chest_check(int y, int x);
void hit_trap(int y, int x);

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_dt(int *y, int *x, byte f1, byte f2)
{
	int d, count;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++) {

		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;

		/* Not looking for this feature */
		if (cave_feat[yy][xx] < f1) continue;
		if (cave_feat[yy][xx] > f2) continue;

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
	int py = p_ptr->py;
	int px = p_ptr->px;
	
	int d, count, o_idx;

	object_type *o_ptr;

	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++) {

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

	int py = p_ptr->py;
	int px = p_ptr->px;
	
	dy = y - py;
	dx = x - px;

	/* Paranoia */
	if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

	return d[dx + 1][dy + 1];
}

#endif /* defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) -- TNB */

/*
 * Move up or down via stairs.
 * deeper stores whether we are moving to a deeper or a shallower level.
 * stairs stores whether we want stairs at the player's feet or not.
 */
static void use_stairs(cptr dir, bool deeper, int start)
{
	int i, levels = (multi_stair) ? randint(5) : 1;

	if (dun_level && confirm_stairs)
	{
		if (!get_check("Really leave the level? ")) return;
	}

	/* Take some time... */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Hack - assume that only trap doors yield random movement. */
	if (start == START_RANDOM)
	{
		msg_print("You deliberately jump through the trap door.");
	}
	else if (!dun_level)
	{
                /* Reset recall depth on entering a new dungeon. */
                /* RM: Only allow reset if entering a different dungeon.  */
                if (p_ptr->cur_dungeon != wild_grid[p_ptr->wildy][p_ptr->wildx].dungeon)
                {
    	                p_ptr->cur_dungeon = wild_grid[p_ptr->wildy][p_ptr->wildx].dungeon;
                        p_ptr->max_dlv = 1;
                }
		msg_format("You enter %s",dun_name+dun_defs[p_ptr->cur_dungeon].name);
	}
	else
	{
		msg_format("You enter a maze of %s staircases.", dir);
	}

	/* Never go very far at shallow levels. */
	if (levels > dun_level/2+1) levels = 1;

	/* The above already forces the action to be legal. */
	if (!deeper)
	{
		/* So just make it go shallower. */
		levels *= -1;
	}
	else
	{
		/* Prevent the player from passing the end of the dungeon. */
		int k = dun_defs[p_ptr->cur_dungeon].max_level - dun_level;
		if (k > 0 && k < levels) levels = k;

		/* Prevent the player from passing a quest. */
		for (i = 0; i < MAX_Q_IDX; i++)
		{
			if (q_list[i].dungeon == p_ptr->cur_dungeon)
			{
				int k = q_list[i].level - dun_level;

				if (k > 0 && k < levels) levels = k;
			}
		}
	}

	/* Prevent connected stairs in the dungeon, if requested. */
	if (dun_level+levels && !dungeon_stair) start = START_RANDOM;

	change_level(dun_level+levels, start);

	/* Check for leaving dungeon */
	if(!dun_level)
	{
		p_ptr->recall_dungeon = p_ptr->cur_dungeon;
		p_ptr->wildx=dun_defs[p_ptr->cur_dungeon].x;
		p_ptr->wildy=dun_defs[p_ptr->cur_dungeon].y;
	}
}

/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	/* Player grid */
	bool deeper;

	int py = p_ptr->py;
	int px = p_ptr->px;
	
	/* Verify stairs */
	if (cave_feat[py][px] != FEAT_LESS)
	{
		msg_print("I see no up staircase here.");
	}
	else
	{
		deeper = (!dun_level || dun_defs[p_ptr->cur_dungeon].flags & DF_TOWER);
		use_stairs("up", deeper, START_DOWN_STAIRS);
	}
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	/* Player grid */
	bool deeper;
	int start;

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Verify stairs */
	if (cave_feat[py][px] == FEAT_MORE)
	{
		start = START_UP_STAIRS;
	}
	else if (cave_feat[py][px] == FEAT_TRAP_DOOR)
	{
		start = START_RANDOM;
	}
	else
	{
		msg_print("I see no down staircase here.");
		return;
	}

	deeper = (!dun_level || ~dun_defs[p_ptr->cur_dungeon].flags & DF_TOWER);

	use_stairs("down", deeper, start);
}


/*
 * Search for hidden things
 */
static void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	
	int y, x, chance;

	s16b this_o_idx, next_o_idx = 0;

	/* Start with base search ability */
	chance = p_ptr->skill_srh;

	/* Penalize various conditions */
	if (p_ptr->blind || no_lite()) chance = chance / 10;
	if (p_ptr->confused || p_ptr->image) chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Invisible trap */
				if (cave_feat[y][x] == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Gain exp */
					skill_exp(SKILL_SEARCH);
					skill_exp(SKILL_PERCEPTION);

					/* Disturb */
					disturb(0);
				}

				/* Secret door */
				if (cave_feat[y][x] == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");
					gain_exp(1);

					/* Gain exp */
					skill_exp(SKILL_SEARCH);
					skill_exp(SKILL_PERCEPTION);

					/* Pick a door XXX XXX XXX */
					replace_secret_door(y, x);

					/* Disturb */
					disturb(0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
				{
					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval]) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Gain exp */
						skill_exp(SKILL_SEARCH);
						skill_exp(SKILL_PERCEPTION);

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0);
					}
				}
			}
		}
	}
}


/*
 * If command_arg is set, set command_rep to it (less 1 for this iteration).
 * Otherwise, do nothing.
 */
void cnv_arg_to_rep(void)
{
	if (!p_ptr->command_arg) return;

	/* Set repeat count */
	p_ptr->command_rep = p_ptr->command_arg - 1;

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Cancel the arg */
	p_ptr->command_arg = 0;
}


/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Take a turn */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Search */
	search();
}


/*
 * Hack -- toggle sneak mode
 */
void do_cmd_toggle_sneak(void)
{
	/* Toggle sneaking */
	p_ptr->sneaking = !p_ptr->sneaking;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE | PR_SPEED);
}



/*
 * Determine if a grid contains a chest
 */
static int chest_check(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;


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
 * Allocates objects upon opening a chest
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

	object_kind *k_ptr;

	/* Access chest */
	o_ptr = &o_list[o_idx];

	/* Access chest kind */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Small chests often hold "gold" */
	tiny = (k_ptr->extra / 10) == XT_CHEST_SMALL;

	/* Determine how much to drop (see above) */
	number = chest_number(k_ptr);

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 10;

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
			if (!make_gold(i_ptr, FOUND_CHEST, o_ptr->k_idx, 0)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, FALSE, FALSE, FOUND_CHEST, o_ptr->k_idx))
				continue;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = dun_depth;

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
		take_hit(damroll(1, 4), "a poison needle", MON_TRAP);
		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle", MON_TRAP);
		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msg_print("A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
		{
			(void)add_flag(TIMED_POISONED, 10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void)add_flag(TIMED_PARALYZED, 10 + randint(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		msg_print("You are enveloped in a cloud of smoke!");
		for (i = 0; i < num; i++)
		{
			if (randint(80)+10<(dun_depth))
				activate_hi_summon();
			else
				(void)summon_specific(y, x, dun_depth, 0);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		take_hit(damroll(5, 8), "an exploding chest", MON_TRAP);
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
			skill_exp(SKILL_DISARM);
			gain_exp(1);
			flag = TRUE;
		}

		/* Failure -- Keep trying */
		else if ((i > 4) && (randint(i) > 4))
		{
			/* We may continue repeating */
			more = TRUE;
			if (flush_failure) flush();
			msg_print("You failed to pick the lock.");
		}


	        /* Failure -- Set off the trap */
	        else
	        {
		        msg_print("You set off a trap!");
		        chest_trap(y, x, o_idx);
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


	/* Take a turn */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

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
		skill_exp(SKILL_DISARM);
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
 * Determine if a given grid may be "opened"
 */
static bool do_cmd_open_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		msg_print("You see nothing there to open.");

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
		msg_print("The door appears to be stuck.");
	}

	/* Locked door */
	else if (cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
		i = p_ptr->skill_dis;

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
			msg_print("You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			skill_exp(SKILL_DISARM);
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
	int y, x;

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Count closed doors (locked or jammed) */
		int num_doors = count_dt(&y, &x, FEAT_DOOR_HEAD, FEAT_DOOR_TAIL);

		/* Count chests (locked) */
		int num_chests = count_chests(&y, &x, FALSE);

		/* Set if only one target */
		if (num_doors + num_chests == 1) p_ptr->command_dir = coords_to_dir(y, x);
	}

#endif /* ALLOW_EASY_OPEN -- TNB */

	if (!get_rep_target(&x, &y)) return;

	/* Check for chests */
	o_idx = chest_check(y, x);

	/* Verify legality */
	if (!o_idx && !do_cmd_open_test(y, x)) return;
	
	/* Set repeat if requested. */

	/* Take a turn */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Apply confusion */
	confuse_target(&x, &y);

	/* Allow repeated command */
	cnv_arg_to_rep();

	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

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
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

 	/* Require open/broken door */
	if ((cave_feat[y][x] != FEAT_OPEN) &&
	    (cave_feat[y][x] != FEAT_BROKEN))
	{
		/* Message */
		msg_print("You see nothing there to close.");

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
	int y, x;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Select an open door if allowed. */
	if (easy_open && count_dt(&y, &x, FEAT_OPEN, FEAT_OPEN) == 1)
			p_ptr->command_dir = coords_to_dir(y, x);

#endif /* ALLOW_EASY_OPEN -- TNB */

	/* Get a direction (or abort) */
	if (!get_rep_target(&x, &y)) return;


	/* Verify legality */
	if (!do_cmd_close_test(y, x)) return;

	/* Apply confusion */
	confuse_target(&x, &y);

	/* Allow repeated command */
	cnv_arg_to_rep();

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
	if (!more) disturb(0);
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
	sound(SOUND_DIG);

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);

	return TRUE;
}



/* Special wall features for do_cmd_tunnel_aux(). */
#define WALL_GOLD 1 /* Contains gold. */
#define WALL_OBJ 2 /* May contain an object */
#define WALL_DOOR 3 /* Is really a secret door. */
#define WALL_NO_DIG 4 /* Undiggable. */

/* Messages given by a particular type of wall. */
#define WMSG_TREE 0 /* Tree, Bush */
#define WMSG_WATER 1 /* Water */
#define WMSG_WALL 2 /* Walls (all types), Door */
#define WMSG_SECRET 3 /* Walls (all types), Door */
#define WMSG_GOLD 4 /* Walls with gold in them (no success message) */
#define WMSG_RUBBLE 5 /* Rubble */
#define WMSG_MAX 6


typedef struct wall_type wall_type;
typedef struct wall_message_type wall_message_type;

struct wall_type
{
	byte feature; /* Name of the feature being dug. */
	s16b sk_min; /* Minimum p_ptr->skill_dig to dig through the wall. */
	s16b sk_max; /* Minimum skill to dig through the wall at once. */
	byte special; /* WALL_* entry for special cases. */
	byte msg; /* The set of messages to use (see wall_message) */
	cptr name; /* Name of the wall type. Derived from f_name if none. */
};

struct wall_message_type
{
	cptr cont;
	cptr fail;
	cptr succeed;
};

static wall_type wall_info[] =
{
	{FEAT_SECRET, 36, 1435, WALL_DOOR, WMSG_SECRET, NULL},
	{FEAT_RUBBLE, 0, 200, WALL_OBJ, WMSG_RUBBLE, "rubble"},
	{FEAT_MAGMA, 11, 410, 0, WMSG_WALL, NULL},
	{FEAT_QUARTZ, 21, 820, 0, WMSG_WALL, NULL},
	{FEAT_MAGMA_H, 11, 410, WALL_GOLD, WMSG_GOLD, NULL},
	{FEAT_QUARTZ_H, 21, 820, WALL_GOLD, WMSG_GOLD, NULL},
	{FEAT_MAGMA_K, 11, 410, WALL_GOLD, WMSG_GOLD, "magma vein"},
	{FEAT_QUARTZ_K, 21, 820, WALL_GOLD, WMSG_GOLD, "quartz vein"},
	{FEAT_WALL_EXTRA, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_WALL_INNER, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_WALL_OUTER, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_WALL_SOLID, 41, 1640, 0, WMSG_WALL, NULL},
	{FEAT_PERM_BUILDING, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_PERM_INNER, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_PERM_OUTER, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_PERM_SOLID, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
	{FEAT_WATER, 0, 0, WALL_NO_DIG, WMSG_WATER, NULL},
	{FEAT_TREE, 41, 140, 0, WMSG_TREE, NULL},
	{FEAT_BUSH, 0, 0, 0, WMSG_TREE, NULL},
	{FEAT_NONE, 31, 1230, 0, WMSG_WALL, NULL},

};

static const wall_message_type wall_message[WMSG_MAX] =
{
	{"You hack away at the %s.", "You leave no mark on the %s.", "You have chopped down the %s."},
	{"You empty out the %s.", "The %s fills up your tunnel as quickly as you dig!", "You have removed the %s."},
	{"You tunnel into the %s.", "You make no impression on the %s.", "You have finished the tunnel."},
	{"You tunnel into the %s.", "You make no impression on the %s.", "You break through a secret door!"},
	{"You tunnel into the %s.", "You make no impression on the %s.", "You have found something!"},
	{"You dig in the %s.", "You make no impression on the %s.", "You have removed the %s."},
};

/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_feat[y][x] == FEAT_NONE || cave_floor_bold(y, x))
	{
		/* Message */
		msg_print("You see nothing there to tunnel.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
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
	wall_type *w_ptr;
	const wall_message_type *wm_ptr;
	cptr w_name;

	bool more = FALSE;

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Sound */
	/* sound(SOUND_DIG); */

	for (w_ptr = wall_info;w_ptr->feature!= FEAT_NONE; w_ptr++)
	{
		if (w_ptr->feature == cave_feat[y][x]) break;

		/* Paranoia - not a valid wall (should not reach here). */
		if (w_ptr == wall_info+N_ELEMENTS(wall_info))
		{
			if (alert_failure) msg_print("Strange wall type found!");
			return FALSE;
		}
	}

	/* If a name is given here, use it. */
	/* If no name has been set, read it from f_name. */
	if (w_ptr->name)
	{
		w_name = w_ptr->name;
	}
	else
	{
		w_name = format("%v", feature_desc_f2, w_ptr->feature, 0);
	}

	/* Find the message set. */
	wm_ptr = &wall_message[w_ptr->msg];

	/* Certain failure */
	if (w_ptr->special == WALL_NO_DIG || p_ptr->skill_dig < w_ptr->sk_min)
	{
		msg_format(wm_ptr->fail, w_name);
	}
	/* Normal failure */
	else if (p_ptr->skill_dig < rand_range(w_ptr->sk_min, w_ptr->sk_max))
	{
		msg_format(wm_ptr->cont, w_name);
		more = TRUE;

		/* Occasional Search XXX XXX */
		if (w_ptr->special == WALL_DOOR && rand_int(100) < 25) search();
	}
	/* Success */
	else
	{
		/* Actually tunnel through wall. */
		twall(y, x);

		/* Message */
		msg_format(wm_ptr->succeed, w_name);

		/* Handle special cases. */
		switch (w_ptr->special)
		{
			case WALL_GOLD:
			{
				place_gold(y,x, FOUND_DIG, cave_feat[y][x]);
				break;
			}
			case WALL_OBJ:
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE, FOUND_DIG, cave_feat[y][x]);

				/* Observe new object */
				if (player_can_see_bold(y, x)) 
					msg_print("You have found something!");
				break;
			}
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	bool more = FALSE;

	/* Get a direction (or abort) */
	if (!get_rep_target(&x, &y)) return;

	/* Oops */
	if (!do_cmd_tunnel_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Apply confusion */
	confuse_target(&x, &y);

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
	if (!more) disturb(0);
}

#ifdef ALLOW_EASY_OPEN

/*
 * easy_open_door --
 *
 * If there is a jammed/closed/locked door at the given location,
 * then attempt to unlock/open it. Return TRUE if an attempt was
 * made (successful or not), otherwise return FALSE.
 *
 * The code here should be nearly identical to that in
 * do_cmd_open_test() and do_cmd_open_aux().
 */

static bool easy_open_door(int y, int x)
{
	int i, j;

        /* RM: Do not enter function if easy_open is off. */
        if (!easy_open)
        {
            return FALSE;
        }

	/* Must be a closed door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) && (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Nope */
		return (FALSE);
	}

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
			msg_print("You have picked the lock.");

			/* Open the door */
			cave_set_feat(y, x, FEAT_OPEN);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

			/* Sound */
			sound(SOUND_OPENDOOR);

			/* Experience */
			skill_exp(SKILL_DISARM);
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			msg_print("You failed to pick the lock.");
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
	return (TRUE);
}

#endif /* ALLOW_EASY_OPEN -- TNB */


/* Forward declaration. */
static void move_player(int y, int x, int do_pickup);

/*
 * Determine if a given grid may be "disarmed"
 */
static bool do_cmd_disarm_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require an actual trap */
	if (!((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
	      (cave_feat[y][x] <= FEAT_TRAP_TAIL)))
	{
		/* Message */
		msg_print("You see nothing there to disarm.");

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

	/* Access trap name */
	name = format("%v", feature_desc_f2, cave_feat[y][x], 0);

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
		gain_exp(power);
		skill_exp(SKILL_DISARM);

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
		msg_format("You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		msg_format("You set off the %s!", name);

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
	int y, x;

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_DISARM

	/* Option: Pick a direction */
	if (easy_disarm)
	{
		/* Count visible traps */
		int num_traps = count_dt(&y, &x, FEAT_TRAP_HEAD, FEAT_TRAP_TAIL);

		/* Count chests (trapped) */
		int num_chests = count_chests(&y, &x, TRUE);

		/* See if only one target */
		if (num_traps + num_chests == 1) p_ptr->command_dir = coords_to_dir(y, x);
	}

#endif /* ALLOW_EASY_DISARM -- TNB */

	if (!get_rep_target(&x, &y)) return;

	/* Check for chests */
	o_idx = chest_check(y, x);

	/* Verify legality */
	if (!o_idx && !do_cmd_disarm_test(y, x)) return;

	/* Take a turn */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Apply confusion */
	if (confuse_target(&x, &y))
	{
		/* Check for chests */
		o_idx = chest_check(y, x);
	}

	/* Allow repeated command */
	cnv_arg_to_rep();

	/* Disarm a trap */
	if (cave_m_idx[y][x])
	{
		/* Message */
		msg_print("There is a monster in the way!");

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
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		msg_print("You see nothing there to bash.");

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
		/* move_player(y, x, FALSE); */

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE);
		p_ptr->update |= (PU_DISTANCE);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
			skill_set[SKILL_TOUGH].value/2)
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
		(void)add_flag(TIMED_PARALYZED, 2 + rand_int(2));
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
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_bash_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}



	/* Set repeat if requested. */
	cnv_arg_to_rep();

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

	/* Door */
	else
	{
		/* Bash the door */
		more = do_cmd_bash_aux(y, x);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0);
}



/*
 * Attack a monster at a given location, return FALSE if it dies or disappears.
 *
 * This only aborts the repetition when the player is unable to continue
 * attacking. Any abort desired based on the monster's actions should be
 * handled elsewhere.
 */
static bool alter_monster(int y, int x)
{
	monster_type *m_ptr = m_list+cave_m_idx[y][x];

	/* Paranoia */
	if (!cave_m_idx[y][x] || !m_ptr->r_idx) return FALSE;

	/* Attack it. */
	py_attack(y, x);

	/* Dead monster. */
	if (!m_ptr->r_idx) return FALSE;

	/* Vanished monster. */
	if (!m_ptr->ml) return FALSE;

	/* Still there to hit again. */
	return TRUE;
}

/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * Consider confusion XXX XXX XXX
 *
 * This command must always take a turn for the initial request to prevent free
 * detection of invisible monsters.
 */
void do_cmd_alter(void)
{
	/* Track the command actually being executed by do_cmd_alter().
	 * This is necessary to enable the "fight" command to be repeated without
	 * the command continuing after the fight has finished. */
	static char alter_cmd = 0;

	int y, x;

	bool more = FALSE;


	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a direction */
	if (get_rep_target(&x, &y))
	{
		/* Take a turn */
		p_ptr->energy_use = extract_energy[p_ptr->pspeed];

		/* Attack monsters */
		if (cave_m_idx[y][x])
		{
			/* Attack */
			more = alter_monster(y, x);
			alter_cmd = 'H';
		}

		/* Avoid continuing to fight after the monster has gone. */
		else if (alter_cmd == 'H')
		{
			p_ptr->energy_use = 0;
		}

		/* Tunnel through walls */
		else if (((cave_feat[y][x] >= FEAT_SECRET)
			&& (cave_feat[y][x] < FEAT_MINOR_GLYPH)) ||
				(cave_feat[y][x] == FEAT_WATER) ||
				(cave_feat[y][x] == FEAT_TREE) ||
				(cave_feat[y][x] == FEAT_BUSH))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x);
			alter_cmd = 'T';
		}

		/* Bash jammed doors */
		else if ((cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x08)
			&& (cave_feat[y][x] < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_bash_aux(y, x);
			alter_cmd = 'B';
		}

		/* Open closed doors */
		else if ((cave_feat[y][x] >= FEAT_DOOR_HEAD)
			&& (cave_feat[y][x] < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_open_aux(y, x);
			alter_cmd = 'o';
		}

		/* Disarm traps */
		else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD)
			&& (cave_feat[y][x] < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_disarm_aux(y, x);
			alter_cmd = 'D';
		}

		/* Oops */
		else
		{
			/* Oops */
			msg_print("You attack the empty air.");
			alter_cmd = ',';
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more)
	{
		disturb(0);
		alter_cmd = 0;
	}
}


/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
static object_type *get_spike(void)
{
	object_type *o_ptr;

	/* Check every item in the pack */
	for (o_ptr = inventory; o_ptr < inventory+INVEN_PACK; o_ptr++)
	{
		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Check the "tval" code */
		if (o_ptr->tval == TV_SPIKE)
		{
			/* Return the spike index */
			return o_ptr;
		}
	}

	/* Oops */
	return NULL;
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
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		msg_print("You see nothing there to spike.");

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
	object_type *o_ptr;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;


	/* Get a spike */
	if (!(o_ptr = get_spike()))
	{
		/* Message */
		msg_print("You have no spikes!");

		/* Done */
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_target(&x, &y)) return;

	/* Verify legality */
	if (!do_cmd_spike_test(y, x)) return;


	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	
	/* Confuse direction */
	confuse_target(&x, &y);


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
		if (!do_cmd_spike_test(y, x)) return;

		/* Successful jamming */
		msg_print("You jam the door with a spike.");

		/* Convert "locked" to "stuck" XXX XXX XXX */
		if (cave_feat[y][x] < FEAT_DOOR_HEAD + 0x08) cave_feat[y][x] += 0x08;

		/* Add one spike to the door */
		if (cave_feat[y][x] < FEAT_DOOR_TAIL) cave_feat[y][x]++;

		/* Use up, and describe, a single spike, from the bottom */
		item_increase(o_ptr, -1);
		item_describe(o_ptr);
		item_optimize(o_ptr);
	}
}




/*
 * Modified version of get_check() to give a [y/n/q] prompt, returning
 * information available. Returns 'y', 'n' or 'q' as appropriate.
 */
static char get_check_ynq(cptr prompt)
{
	char rc;

	/* Help */
	help_track("ynq_prompt");

	rc = get_check_aux(prompt, "$!%.*s[y/n/q]%s%v", "nN\033yY\rQq", "nnnyyyqq");

	/* Leave a message. */
	message_add(format(0));

	/* Done with help */
	help_track(NULL);

	return rc;
}


/*
 * Make the player carry everything in a grid
 *
 * If "pickup" is FALSE then only gold will be picked up
 */
static void py_pickup(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b this_o_idx, next_o_idx = 0;

	bool gold_only = FALSE;
	
	/* First check the pile for squelching opportunities. */
	p_ptr->notice |= PN_FSQUELCH;
	notice_stuff();

	/* Then pick everything up. */

	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
			msg_format("You collect %ld gold pieces worth of %v.",
					(long)o_ptr->pval, object_desc_f3, o_ptr, FALSE, 0);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER | PW_SPELL);

			/* Delete the gold */
			delete_dun_object(o_ptr);

			/* Encourage further skill checks... */
			if (object_skill_count) object_skill_count--;

		}

		/* gold_only cancels the collection of objects, but gold is picked up
		 * automatically. */
		else if (gold_only);

		/* Pick up objects */
		else
		{
			bool pickup_this = FALSE;

			/* Notice previously untouched items. */
			if (~o_ptr->ident & IDENT_TOUCHED)
			{
                		/* RM: Allow experience gain, but not too quickly.  ... */
                		if (object_skill_count>1) 
					object_skill_count = object_skill_count / 2 - 1;
				else
					object_skill_count=0;

				/* Only do this once per item. */
				object_touch(o_ptr);
			}

			/* Hack - ignore hidden items altogether. */
			if (hidden_p(o_ptr)) continue;

			/* Display description if needed. */
			object_track(o_ptr);

			/* Note that the pack is too full */
			if (!inven_carry_okay(o_ptr))
			{
				if (always_pickup)
				{
					msg_format("You have no room for %v.",
						object_desc_f3, o_ptr, TRUE, 3);
					continue;
				}
			}
			else if (strstr(get_inscription(o_ptr), "=g"))
			{
				pickup_this = TRUE;
			}
			else if (!pickup)
			{
				pickup_this = FALSE;
			}
			else if (carry_query_flag)
			{
				char c = get_check_ynq(format("Pick up %.*v? ",
					Term->wid-strlen("Pick up ? "),
					object_desc_f3, o_ptr, TRUE, 3));

				/* Pick up no more objects. */
				gold_only = (c == 'q');

				/* Pick up this object. */
				if (c == 'y') pickup_this = TRUE;

				/* Say nothing more. */
				else continue;
			}
			else
			{
				pickup_this = TRUE;
			}

			if (pickup_this)
			{
				/* Carry the item */
				object_type *j_ptr = inven_carry(o_ptr);

				/* Message */
				msg_format("You have %v (%c).",
					object_desc_f3, j_ptr, TRUE, 3, index_to_label(j_ptr));

				/* Remember the object */
				object_track(j_ptr);

				/* Delete the object */
				delete_dun_object(o_ptr);
			}
			else
			{
				/* Give a message. */
				msg_format("You see %v.", object_desc_f3, o_ptr, TRUE, 3);
			}
		}
	}
}






/*
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(int y, int x)
{
	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Require open space */
	if (!cave_floor_bold(y, x))
	{
		/* Rubble */
		if (cave_feat[y][x] == FEAT_RUBBLE)
		{
			/* Message */
			msg_print("There is a pile of rubble in the way!");
		}

		/* Door */
		else if (cave_feat[y][x] < FEAT_SECRET)
		{
			/* Message */
			msg_print("There is a door in the way!");
		}

		/* Wall */
		else
		{
			/* Message */
			msg_print("There is a wall in the way!");
		}

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for the "walk" and "jump" commands
 */
static void do_cmd_walk_or_jump(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];
	}

	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;

	/* Allow repeated command */
	cnv_arg_to_rep();

	/* Move the player */
	move_player(y, x, pickup);

}

/*
 * Walk into a grid (usually pickup)
 */
void do_cmd_walk(void)
{
	/* Move (usually pickup) */
	do_cmd_walk_or_jump(always_pickup);
}


/*
 * Jump into a grid (usually do not pickup)
 */
void do_cmd_jump(void)
{
	/* Move (usually do not pickup) */
	do_cmd_walk_or_jump(!always_pickup);
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
	cnv_arg_to_rep();


	/* Take a turn */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];


	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == rand_int(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Sneaking allows continuous search */
	if (p_ptr->sneaking)
	{
		search();
	}

	/* Handle "objects" */
	py_pickup(pickup);


	/* Hack -- enter a store if we are on one */
	if ((cave_feat[py][px] >= FEAT_SHOP_HEAD) &&
	    (cave_feat[py][px] <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0);

		/* Hack -- enter store */
		p_ptr->command_new = KTRL('E');
	}
}


/*
 * Hold still (usually pickup)
 */
void do_cmd_hold(void)
{
	/* Hold still (usually pickup) */
	do_cmd_hold_or_stay(always_pickup);
}


/*
 * Stay still (usually do not pickup)
 */
void do_cmd_stay(void)
{
	/* Stay still (usually do not pickup) */
	do_cmd_hold_or_stay(!always_pickup);
}


/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Don't actually do anything if already resting. */
	if (p_ptr->command_rep)
	{
		/* Take a turn XXX XXX XXX (?) */
		p_ptr->energy_use = extract_energy[p_ptr->pspeed];

		return;
	}

	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
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
			if (p_ptr->command_arg <= 0) return;
		}
	}


	/* Paranoia */
	if (p_ptr->command_arg > 9999) p_ptr->command_arg = 9999;


	/* Take a turn XXX XXX XXX (?) */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Save the rest code */
	//p_ptr->resting = p_ptr->command_arg;
	cnv_arg_to_rep();

	/* Cancel sneaking */
	p_ptr->sneaking = FALSE;

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
 * Handle player hitting a real trap
 */
void hit_trap(int y, int x)
{
	int                     i, num, dam;

	cptr            name = "a trap";

	/* Disturb the player */
	disturb(0);

	/* Analyze XXX XXX XXX */
	switch (cave_feat[y][x])
	{
		case FEAT_TRAP_HEAD + 0x00:
		{

			if (p_ptr->ffall)
			{
				msg_print("You fly over a trap door.");
			}
			else
			{
				msg_print("You fall through a trap door!");
				dam = damroll(2, 8);
				name = "a trap door";
				if (dun_defs[p_ptr->cur_dungeon].flags & DF_TOWER)
				{
					change_level(dun_level-1, START_RANDOM);
				}
				else
				{
					change_level(dun_level+1, START_RANDOM);
				}
				take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{

			if (p_ptr->ffall)
			{
				msg_print("You fly over a pit trap.");
			}
			else
			{
				msg_print("You fall into a pit!");
				dam = damroll(2, 6);
				name = "a pit trap";
				take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x02:
		{


			if (p_ptr->ffall)
			{
				msg_print("You fly over a spiked pit.");
			}

			else
			{
				msg_print("You fall into a spiked pit!");
				/* Base damage */
				name = "a pit trap";
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

					name = "a spiked pit";
					dam = dam * 2;
					(void)add_flag(TIMED_CUT, randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name, MON_TRAP);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{


			if (p_ptr->ffall)
			{
				msg_print("You fly over a spiked pit.");
			}

			else
			{
				msg_print("You fall into a spiked pit!");
				/* Base damage */
				dam = damroll(2, 6);

				name = "a pit trap";

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

					name = "a poisonous spiked pit";

					dam = dam * 2;
					(void)add_flag(TIMED_CUT, randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						msg_print("The poison does not affect you!");
					}

					else
					{
						dam = dam * 2;
						(void)add_flag(TIMED_POISONED, randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name, MON_TRAP);
			}

			break;
		}

		case FEAT_TRAP_HEAD + 0x04:
		{
			msg_print("There is a flash of shimmering light!");
			cave_info[y][x]  &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);
			num = 2 + randint(3);
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(y, x, dun_depth, 0);
			}
			if ((dun_depth)>randint(100)) /* No nasty effect for low levels */
						{ do { activate_ty_curse(); } while (randint(6)==1); }
			break;
		}

		case FEAT_TRAP_HEAD + 0x05:
		{
			msg_print("You hit a teleport trap!");
			teleport_player(100);
			break;
		}

		case FEAT_TRAP_HEAD + 0x06:
		{
			msg_print("You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, "a fire trap", MON_TRAP);
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap", MON_TRAP);
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name, MON_TRAP);
				(void)add_flag(TIMED_SLOW, rand_int(20) + 20);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x09:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
				(void)do_dec_stat(A_STR);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0A:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
				(void)do_dec_stat(A_DEX);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0B:
		{
			if (check_hit(125, 0))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
		take_hit(dam, "a dart trap", MON_TRAP);
				(void)do_dec_stat(A_CON);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("You are surrounded by a black gas!");
			if (!p_ptr->resist_blind)
			{
				(void)add_flag(TIMED_BLIND, rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("You are surrounded by a gas of scintillating colors!");
			if (!p_ptr->resist_confu)
			{
				(void)add_flag(TIMED_CONFUSED, rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("You are surrounded by a pungent green gas!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)add_flag(TIMED_POISONED, rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("You are surrounded by a strange white mist!");
			if (!p_ptr->free_act)
			{
				(void)add_flag(TIMED_PARALYZED, rand_int(10) + 5);
			}
			break;
		}
	}
}


static bool pattern_tile(byte y, byte x)
{
	return
	((cave_feat[y][x] <= FEAT_PATTERN_XTRA2) &&
	(cave_feat[y][x] >= FEAT_PATTERN_START));

}

static bool pattern_seq(byte c_y, byte c_x, byte n_y,byte  n_x)
{
	if (!(pattern_tile(c_y, c_x)) && !(pattern_tile(n_y, n_x)))
			return TRUE;

	if (cave_feat[n_y][n_x] == FEAT_PATTERN_START)
	{
		if ((!(pattern_tile(c_y, c_x)))
				&& !(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
			if (get_check("If you start walking the Pattern, you must walk the whole way. Ok? "))
				return TRUE;
			else
				return FALSE;
		}
		else
			return TRUE;
	}
	else if ((cave_feat[n_y][n_x] == FEAT_PATTERN_OLD) ||
			(cave_feat[n_y][n_x] == FEAT_PATTERN_END) ||
			(cave_feat[n_y][n_x] == FEAT_PATTERN_XTRA2))
	{
		if (pattern_tile(c_y, c_x))
		{
			return TRUE;
		}
		else
		{
			msg_print("You must start walking the Pattern from the startpoint.");
			return FALSE;
		}
	}
	else if ((cave_feat[n_y][n_x] == FEAT_PATTERN_XTRA1)||
			(cave_feat[c_y][c_x] == FEAT_PATTERN_XTRA1))
	{
		return TRUE;
	}
	else if (cave_feat[c_y][c_x] == FEAT_PATTERN_START)
	{
		if (pattern_tile(n_y, n_x))
			return TRUE;
		else
			{
				msg_print("You must walk the Pattern in correct order.");
				return FALSE;
			}
	}
	else if ((cave_feat[c_y][c_x] == FEAT_PATTERN_OLD) ||
			(cave_feat[c_y][c_x] == FEAT_PATTERN_END) ||
			(cave_feat[c_y][c_x] == FEAT_PATTERN_XTRA2))
	{
		if (!pattern_tile(n_y, n_x))
		{
			msg_print("You may not step off from the Pattern.");
			return FALSE;
		}
		else
		{
			return TRUE;
		}
	}
	else
	{
		if (!pattern_tile(c_y, c_x))
		{
			msg_print("You must start walking the Pattern from the startpoint.");
			return FALSE;
		}
		else
		{
			byte ok_move = FEAT_PATTERN_START;
			switch (cave_feat[c_y][c_x])
			{
				case FEAT_PATTERN_1:
					ok_move = FEAT_PATTERN_2;
					break;
				case FEAT_PATTERN_2:
					ok_move = FEAT_PATTERN_3;
					break;
				case FEAT_PATTERN_3:
					ok_move = FEAT_PATTERN_4;
					break;
				case FEAT_PATTERN_4:
					ok_move = FEAT_PATTERN_1;
					break;
				default:
					if (cheat_wzrd)
						msg_format("Funny Pattern walking, %d.", cave_feat[c_y][c_x]);
					return TRUE; /* Goof-up */
			}
			if ((cave_feat[n_y][n_x] == ok_move) ||
				(cave_feat[n_y][n_x] == cave_feat[c_y][c_x]))
				return TRUE;
			else
			{
				if (!pattern_tile(n_y, n_x))
					msg_print("You may not step off from the Pattern.");
				else
					msg_print("You must walk the Pattern in correct order.");

				return FALSE;
			}
		}
	}

}


/*
 * Actually move the player to a given location.
 */
void move_to(s16b y, s16b x)
{
    	monster_swap(p_ptr->py, p_ptr->px, y, x);

	return;
}

/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid.  Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
static void move_player(int y, int x, int do_pickup)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	monster_type    *m_ptr;

	C_TNEW(m_name, MNAME_MAX, char);

	bool p_can_pass_walls = FALSE;
	bool wall_is_perma = FALSE;
	bool stormbringer = FALSE;

	
	/* Get the monster */
	m_ptr = &m_list[cave_m_idx[y][x]];


	if (inventory[INVEN_WIELD].name1 == ART_STORMBRINGER)
	{
		stormbringer = TRUE;
	}

	/* Player can not walk through "permanent walls"... */
	/* unless in Shadow Form */
	if (p_ptr->wraith_form || p_ptr->weak_wraith)
	{
		p_can_pass_walls = TRUE;
		if ((cave_feat[y][x] >= FEAT_PERM_BUILDING) && (cave_feat[y][x] <= FEAT_PERM_SOLID))
		{
			wall_is_perma = TRUE;
			p_can_pass_walls = FALSE;
		}
	}

	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] && (m_ptr->ml || cave_floor_bold(y,x) || p_can_pass_walls))
	{
		/* Attack -- only if we can see it OR it is not in a wall */
		if ((m_ptr->smart & SM_ALLY) &&
			!(p_ptr->confused || p_ptr->image || !(m_ptr->ml) || p_ptr->stun
			|| ((p_has_mutation(MUT_BERS_RAGE)) && p_ptr->shero))
			&& (pattern_seq((byte)py,(byte)px,(byte)y,(byte)x)) &&
			((cave_floor_bold(y, x)) || (p_can_pass_walls)))
		{
			m_ptr->csleep = 0;
			/* Extract monster name (or "it") */
			strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);
			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
			/* Track a new monster */
			if (m_ptr->ml) health_track(cave_m_idx[y][x]);
			/* displace? */
			if ( stormbringer && (randint(1000)>666))
			{
				py_attack(y,x);
			}
			else if (cave_floor_bold(py, px) ||
				(r_info[m_ptr->r_idx].flags2 & RF2_PASS_WALL))
			{
				msg_format("You push past %s.", m_name);
				m_ptr->fy = (byte)py;
				m_ptr->fx = (byte)px;			
				cave_m_idx[py][px] = cave_m_idx[y][x];
				cave_m_idx[y][x] = 0;
				update_mon(cave_m_idx[py][px], TRUE);
			}
			else
			{
				msg_format("%^s is in your way!", m_name);
				p_ptr->energy_use = 0;
				TFREE(m_name);
				return;
			}
			/* now continue on to 'movement' */
		}
		else
		{
			py_attack(y, x);
			TFREE(m_name);
			return;
		}
	}
#ifdef ALLOW_EASY_DISARM

	/* Disarm a visible trap */
	if ((do_pickup != easy_disarm) &&
		(cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		(cave_feat[y][x] <= FEAT_TRAP_TAIL))
	{
		(void) do_cmd_disarm_aux(y, x);
		TFREE(m_name);
		return;
	}

#endif /* ALLOW_EASY_DISARM -- TNB */


	/* Player can not walk through "walls" unless in wraith form...*/
	if (((!cave_floor_bold(y, x)) && (cave_feat[y][x] != FEAT_BUSH) && ((!p_can_pass_walls) ||
		(cave_feat[y][x] == FEAT_WILD_BORDER) || (cave_feat[y][x] == FEAT_PATH_BORDER))))
	{
		/* Disturb the player */
		disturb(0);

		/* Notice things in the dark */
		if (!(cave_info[y][x] & (CAVE_MARK)) &&
			(p_ptr->blind || !(cave_info[y][x] & (CAVE_LITE))))
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				msg_print("You feel some rubble blocking your way.");
				mark_spot(y, x);
				lite_spot(y, x);
			}

			/* Tree */
			else if (cave_feat[y][x] == FEAT_TREE)
			{
				msg_print("You feel a tree blocking your way.");
				mark_spot(y, x);
				lite_spot(y,x);
			}
			/* Water */
			else if (cave_feat[y][x] == FEAT_WATER)
			{
				msg_print("Your way seems to be blocked by water.");
				mark_spot(y, x);
				lite_spot(y,x);
			}

			else if ((cave_feat[y][x] == FEAT_WILD_BORDER) || (cave_feat[y][x] == FEAT_PATH_BORDER))
			{

				/* Leave town */
				if(is_town_p(p_ptr->wildy, p_ptr->wildx))
				{
					p_ptr->cur_town = wild_grid[p_ptr->wildy][p_ptr->wildx].dungeon;
					msg_format("You stumble out of %s.",town_name+town_defs[p_ptr->cur_town].name);
				}
				/* Hack - Test which border has been crossed and move the player directly. */
				if(y==0)
				{
					p_ptr->py=cur_hgt-2;
					p_ptr->wildy--;
				}
				if(y==cur_hgt-1)
				{
					p_ptr->py=1;
					p_ptr->wildy++;
				}
				if(x==0)
				{
					p_ptr->px=cur_wid-2;
					p_ptr->wildx--;
				}
				if(x==cur_wid-1)
				{
					p_ptr->px=1;
					p_ptr->wildx++;
				}
				px = p_ptr->px;
				py = p_ptr->py;
				if(is_town_p(p_ptr->wildy, p_ptr->wildx))
				{
					p_ptr->cur_town = wild_grid[p_ptr->wildy][p_ptr->wildx].dungeon;
					msg_format("You stumble into %s.",town_name+town_defs[p_ptr->cur_town].name);
				}
				change_level(0, START_WALK);
			}

			/* Closed door */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
				msg_print("You feel a closed door blocking your way.");
				mark_spot(y, x);
				lite_spot(y, x);
			}

			/* Wall (or secret door) */
			else
			{
				msg_print("You feel a wall blocking your way.");
				mark_spot(y, x);
				lite_spot(y, x);
			}
		}

		/* Notice things */
		else
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				msg_print("There is rubble blocking your way.");
				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					p_ptr->energy_use = 0;

				/* Well, it makes sense that you lose time bumping into
					a wall _if_ you are confused, stunned or blind; but
					typing mistakes should not cost you a turn... */
			}

			/* Tree */
			else if (cave_feat[y][x] == FEAT_TREE)
			{
				msg_print("There is a tree blocking your way.");
				mark_spot(y, x);
				lite_spot(y,x);
			        /* Assume that the player didn't really want to do that. */
				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					p_ptr->energy_use = 0;
			}

			/* Water */
			else if (cave_feat[y][x] == FEAT_WATER)
			{
				msg_print("You cannot swim.");
				mark_spot(y, x);
				lite_spot(y,x);
			        /* Assume that the player didn't really want to do that. */
				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					p_ptr->energy_use = 0;
			}

			else if ((cave_feat[y][x] == FEAT_WILD_BORDER) || (cave_feat[y][x] == FEAT_PATH_BORDER))
			{

				/* Leave town */
				if(is_town_p(p_ptr->wildy, p_ptr->wildx))
				{
					p_ptr->cur_town = wild_grid[p_ptr->wildy][p_ptr->wildx].dungeon;
					msg_format("You leave %s.",town_name+town_defs[p_ptr->cur_town].name);
				}
				/* Hack - Test which border has been crossed and move the player directly. */
				if(y==0)
				{
					p_ptr->py=cur_hgt-2;
					p_ptr->wildy--;
				}
				if(y==cur_hgt-1)
				{
					p_ptr->py=1;
					p_ptr->wildy++;
				}
				if(x==0)
				{
					p_ptr->px=cur_wid-2;
					p_ptr->wildx--;
				}
				if(x==cur_wid-1)
				{
					p_ptr->px=1;
					p_ptr->wildx++;
				}
				px = p_ptr->px;
				py = p_ptr->py;
				if(is_town_p(p_ptr->wildy, p_ptr->wildx))
				{
					p_ptr->cur_town = wild_grid[p_ptr->wildy][p_ptr->wildx].dungeon;
					msg_format("You enter %s.",town_name+town_defs[p_ptr->cur_town].name);
				}
				change_level(0, START_WALK);
			}

			/* Closed doors */
			else if (cave_feat[y][x] < FEAT_SECRET)
			{
#ifdef ALLOW_EASY_OPEN

				if (easy_open_door(y, x))
				{
					TFREE(m_name);
					return;
				}

#endif /* ALLOW_EASY_OPEN -- TNB */

				msg_print("There is a closed door blocking your way.");

				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					p_ptr->energy_use = 0;

			}

			/* Wall (or secret door) */
			else
			{
				msg_print("There is a wall blocking your way.");
				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					p_ptr->energy_use = 0;
			}
		}

		/* Sound */
		sound(SOUND_HITWALL);
		TFREE(m_name);
		return;
	}

	/* Movement on pattern */
	if (!pattern_seq((byte)py,(byte)px,(byte)y,(byte)x))
	{
		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
			p_ptr->energy_use = 0;
		}
		disturb(0); /* To avoid a loop with running */
		TFREE(m_name);
		return;
	}
/*    else */
	/* Normal Movement */
	
	move_to(y,x);

	/* New location */
	y = py = p_ptr->py;
	x = px = p_ptr->px;

	/* Sound XXX XXX XXX */
	/* sound(SOUND_WALK); */

	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) ||
		(0 == rand_int(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->sneaking)
	{
		search();
	}

	/* Handle "objects" */
#ifdef ALLOW_EASY_DISARM

	py_pickup(do_pickup != always_pickup);

#else /* ALLOW_EASY_DISARM -- TNB */

	py_pickup(do_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */


	/* Handle "store doors" */
	if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		(cave_feat[y][x] <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0);

		/* Hack -- Enter store */
		p_ptr->command_new = KTRL('E');
	}

	/* Discover invisible traps */
	else if (cave_feat[y][x] == FEAT_INVIS)
	{
		/* Disturb */
		disturb(0);

		/* Message */
		msg_print("You found a trap!");

		/* Pick a trap */
		pick_trap(p_ptr->py, p_ptr->px);

		/* Hit the trap */
		hit_trap(y, x);
	}

	/* Set off an visible trap */
	else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		(cave_feat[y][x] <= FEAT_TRAP_TAIL))
	{
		/* Disturb */
		disturb(0);

		/* Hit the trap */
		hit_trap(y, x);
	}
	TFREE(m_name);
}


/*
 * Hack -- Check for a "motion blocker" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are blank */
	if (!in_bounds(y, x)) return (FALSE);

	/* Must be a motion blocker */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Must be known to the player */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (FALSE);

	/* Default */
	return (TRUE);
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are unknown */
	if (!in_bounds(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave_info[y][x] & (CAVE_MARK)) return (FALSE);

	/* Non-floor grids are unknown */
	if (!cave_floor_bold(y, x)) return (TRUE);

	/* Viewable door/wall grids are known */
	if (player_can_see_bold(y, x)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm  -CJS-
 *
 * Basically, once you start running, you keep moving until something
 * interesting happens.  In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.  This means that we
 * must be careful when dealing with "illegal" grids.
 *
 * No assumptions are made about the layout of the dungeon, so this
 * algorithm works in hallways, rooms, town, destroyed areas, etc.
 *
 * In the diagrams below, the player has just arrived in the grid
 * marked as '@', and he has just come from a grid marked as 'o',
 * and he is about to enter the grid marked as 'x'.
 *
 * Running while confused is not allowed, and so running into a wall
 * is only possible when the wall is not seen by the player.  This
 * will take a turn and stop the running.
 *
 * Several conditions are tracked by the running variables.
 *
 *   p_ptr->run_open_area (in the open on at least one side)
 *   p_ptr->run_break_left (wall on the left, stop if it opens)
 *   p_ptr->run_break_right (wall on the right, stop if it opens)
 *
 * When running begins, these conditions are initialized by examining
 * the grids adjacent to the requested destination grid (marked 'x'),
 * two on each side (marked 'L' and 'R').  If either one of the two
 * grids on a given side is a wall, then that side is considered to
 * be "closed".  Both sides enclosed yields a hallway.
 *
 *    LL                     @L
 *    @x      (normal)       RxL   (diagonal)
 *    RR      (east)          R    (south-east)
 *
 * In the diagram below, in which the player is running east along a
 * hallway, he will stop as indicated before attempting to enter the
 * intersection (marked 'x').  Starting a new run in any direction
 * will begin a new hallway run.
 *
 * #.#
 * ##.##
 * o@x..
 * ##.##
 * #.#
 *
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * In the diagram below, the player is running east down a hallway,
 * and will stop in the grid (marked '1') before the intersection.
 * Continuing the run to the south-east would result in a long run
 * stopping at the end of the hallway (marked '2').
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After each step, the surroundings are examined to determine if
 * the running should stop, and to determine if the running should
 * change direction.  We examine the new current player location
 * (at which the runner has just arrived) and the direction from
 * which the runner is considered to have come.
 *
 * Moving one grid in some direction places you adjacent to three
 * or five new grids (for straight and diagonal moves respectively)
 * to which you were not previously adjacent (marked as '!').
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * If any of the newly adjacent grids are "interesting" (monsters,
 * objects, some terrain features) then running stops.
 *
 * If any of the newly adjacent grids seem to be open, and you are
 * looking for a break on that side, then running stops.
 *
 * If any of the newly adjacent grids do not seem to be open, and
 * you are in an open area, and the non-open side was previously
 * entirely open, then running stops.
 *
 * If you are in a hallway, then the algorithm must determine if
 * the running should continue, turn, or stop.  If only one of the
 * newly adjacent grids appears to be open, then running continues
 * in that direction, turning if necessary.  If there are more than
 * two possible choices, then running stops.  If there are exactly
 * two possible choices, separated by a grid which does not seem
 * to be open, then running stops.  Otherwise, as shown below, the
 * player has probably reached a "corner".
 *
 *    ###             o##
 *    o@x  (normal)   #@!   (diagonal)
 *    ##!  (east)     ##x   (south east)
 *
 * In this situation, there will be two newly adjacent open grids,
 * one touching the player on a diagonal, and one directly adjacent.
 * We must consider the two "option" grids further out (marked '?').
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid.  For some unknown reason, we assign "check_dir" to
 * the grid marked 's', which may be incorrectly labelled.
 *
 *    ###s
 *    o@x?   (may be incorrect diagram!)
 *    ##!?
 *
 * If both "option" grids are closed, then there is no reason to enter
 * the corner, and so we can cut the corner, by moving into the other
 * grid (diagonally).  If we choose not to cut the corner, then we may
 * go straight, but we pretend that we got there by moving diagonally.
 * Below, we avoid the obvious grid (marked 'x') and cut the corner
 * instead (marked 'n').
 *
 *    ###:               o##
 *    o@x#   (normal)    #@n    (maybe?)
 *    ##n#   (east)      ##x#
 *                       ####
 *
 * If one of the "option" grids is open, then we may have a choice, so
 * we check to see whether it is a potential corner or an intersection
 * (or room entrance).  If the grid two spaces straight ahead, and the
 * space marked with 's' are both open, then it is a potential corner
 * and we enter it if requested.  Otherwise, we stop, because it is
 * not a corner, and is instead an intersection or a room entrance.
 *
 *    ###
 *    o@x
 *    ##!#
 *
 * I do not think this documentation is correct.
 */



/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/*
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int             row, col, deepleft, deepright;
	int             i, shortleft, shortright;


	/* Save the direction */
	find_current = dir;

	/* Assume running straight */
	find_prevdir = dir;

	/* Assume looking for open area */
	find_openarea = TRUE;

	/* Assume not looking for breaks */
	find_breakright = find_breakleft = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for walls */
	if (see_wall(cycle[i+1], py, px))
	{
		find_breakleft = TRUE;
		shortleft = TRUE;
	}
	else if (see_wall(cycle[i+1], row, col))
	{
		find_breakleft = TRUE;
		deepleft = TRUE;
	}

	/* Check for walls */
	if (see_wall(cycle[i-1], py, px))
	{
		find_breakright = TRUE;
		shortright = TRUE;
	}
	else if (see_wall(cycle[i-1], row, col))
	{
		find_breakright = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (find_breakleft && find_breakright)
	{
		/* Not looking for open area */
		find_openarea = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				find_prevdir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				find_prevdir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				find_prevdir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				find_prevdir = cycle[i + 2];
			}
		}
	}
}


static s16b ignm_idx;

/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int                     prev_dir, new_dir, check_dir = 0;

	int                     row, col;
	int                     i, max, inv;
	int                     option, option2;


	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = find_prevdir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];

		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[row][col]];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (this_o_idx = cave_o_idx[row][col]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}


		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (cave_info[row][col] & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (cave_feat[row][col])
			{
				/* Floors */
				case FEAT_FLOOR:

				/* Invis traps */
				case FEAT_INVIS:

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_BUILDING:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:

				/* Surface Features */
				case FEAT_TREE:
				case FEAT_BUSH:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (find_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (find_ignore_stairs) notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (find_openarea)
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (find_openarea)
			{
				if (i < 0)
				{
					/* Break to the right */
					find_breakright = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					find_breakleft = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (find_openarea)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_bold(row, col)) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_bold(row, col)) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* At least one option which isn't in the current direction */
		else if (stop_corner && (option2 || option != prev_dir))
		{
			return TRUE;
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			find_current = option;

			/* No other options */
			find_prevdir = option;
		}

		/* Two options, examining corners */
		else if (find_examine && !find_cut)
		{
			/* Primary option */
			find_current = option;

			/* Hack -- allow curving */
			find_prevdir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = py + ddy[option];
			col = px + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
				!see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (find_examine &&
					see_nothing(option, row, col) &&
					see_nothing(option2, row, col))
				{
					find_current = option;
					find_prevdir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (find_cut)
			{
				find_current = option2;
				find_prevdir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				find_current = option;
				find_prevdir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(find_current, py, px))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}




/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
static void run_step(int dir)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	/* Start running */
	if (dir)
	{
		/* Hack -- do not start silly run */
		if (see_wall(dir, py, px))
		{
			/* Message */
			msg_print("You cannot run in that direction.");

			/* Disturb */
			disturb(0);

			/* Done */
			return;
		}

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Initialize */
		run_init(dir);

		/* Hack - attempting to run into a monster turns the run command
		 * into a command which attempts to fight the monster until the
		 * player is disturbed or the monster dies. */
		ignm_idx = cave_m_idx[py+ddy[find_current]][px+ddx[find_current]];
		if (!m_list[ignm_idx].r_idx || !m_list[ignm_idx].ml) ignm_idx = 0;
	}

	/* Keep fighting */
	else if (ignm_idx)
	{
                /* RM:Is this reachable anymore? */

		/* If the expected monster isn't seen to be there, stop running. */
		if ((cave_m_idx[py+ddy[find_current]][px+ddx[find_current]] != ignm_idx)
			|| !(m_list[ignm_idx].r_idx) || !(m_list[ignm_idx].ml))
		{
			/* Disturb */
			disturb(0);

			/* Done */
			return;
		}
	}
	/* Keep running */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Disturb */
			disturb(0);

			/* Done */
			return;
		}
	}

	/* Take time */
	p_ptr->energy_use = extract_energy[p_ptr->pspeed];

	/* Move the player, using the "pickup" flag */
#ifdef ALLOW_EASY_DISARM

	move_player(py + ddy[find_current], px + ddx[find_current], FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

	move_player(py + ddy[find_current], px + ddx[find_current], always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

        /* RM: Prevent a message from disappearing if a monster is somehow killed. */
	if (ignm_idx)
	{
		/* If the expected monster isn't seen to be there, stop running. */
		if ((cave_m_idx[py+ddy[find_current]][px+ddx[find_current]] != ignm_idx)
			|| !(m_list[ignm_idx].r_idx) || !(m_list[ignm_idx].ml))
		{
			/* Disturb */
			disturb(0);

			/* Done */
			return;
		}
	}

}


/*
 * Run around.
 */
void do_cmd_run(void)
{
	int dir;

	/* Hack -- no running when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
	}

	/* Continue running. */
	else if (p_ptr->command_rep)
	{
		run_step(0);
	}

	/* Get a "repeated" direction */
	else if (get_rep_dir(&dir))
	{
		/* Run until stopped unless told otherwise. */
		if (!p_ptr->command_arg) p_ptr->command_arg = -1;

		/* Hack -- Set the run counter */
		cnv_arg_to_rep();

		/* First step */
		run_step(dir);
	}
}
