/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	/* Verify stairs */
	if (cave_feat[p_ptr->py][p_ptr->px] != FEAT_LESS)
	{
		msg_print("I see no up staircase here.");
		return;
	}

	/* Ironman */
	if (adult_ironman)
	{
		msg_print("Nothing happens!");
		return;
	}

	/* held by a monster */
	if ((p_ptr->timed[TMD_BEAR_HOLD]) && (p_ptr->held_m_idx))
	{
		monster_type *m_ptr = &mon_list[p_ptr->held_m_idx];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		char m_name[80];
		int wrestle;
		int holdfast = r_ptr->level * 3;
		if (r_ptr->flags1 & (RF1_UNIQUE)) holdfast += r_ptr->level / 2;
		wrestle = adj_str_wgt[p_ptr->stat_ind[A_STR]] + goodluck/2;
		wrestle += adj_str_wgt[p_ptr->stat_ind[A_DEX]];
		if (p_ptr->free_act) wrestle += 25;
		monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

		/* attempt to pull free */
		if (randint(wrestle) > holdfast)
		{
			p_ptr->held_m_idx = 0;
			clear_timed(TMD_BEAR_HOLD);
		}
		else
		{
			msg_format("You are being held by %^s!", m_name);
			return;
		}
	}

	/* Hack -- take a turn (was 100) */
	p_ptr->energy_use = 10;

	/* Success */
	message(MSG_STAIRS_UP, 0, "You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_down_stair = TRUE;

	/* New depth */
	p_ptr->depth--;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
	/* Verify stairs */
	if (cave_feat[p_ptr->py][p_ptr->px] != FEAT_MORE)
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* held by a monster */
	if ((p_ptr->timed[TMD_BEAR_HOLD]) && (p_ptr->held_m_idx))
	{
		monster_type *m_ptr = &mon_list[p_ptr->held_m_idx];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		char m_name[80];
		int wrestle;
		int holdfast = (r_ptr->level * 5) / 2; /* (x2.5) */
		if (r_ptr->flags1 & (RF1_UNIQUE)) holdfast += r_ptr->level / 2;
		wrestle = adj_str_wgt[p_ptr->stat_ind[A_STR]] + goodluck/2;
		wrestle += adj_str_wgt[p_ptr->stat_ind[A_DEX]];
		if (p_ptr->free_act) wrestle += 25;
		monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

		/* attempt to pull free */
		if (randint(wrestle) > holdfast)
		{
			p_ptr->held_m_idx = 0;
			clear_timed(TMD_BEAR_HOLD);
		}
		else
		{
			msg_format("You are being held by %^s!", m_name);
			return;
		}
	}

	/* Hack -- take a turn (was 100) */
	p_ptr->energy_use = 10;

	/* Success */
	message(MSG_STAIRS_DOWN, 0, "You enter a maze of down staircases.");

	/* Create a way back */
	p_ptr->create_up_stair = TRUE;

	/* New level */
	p_ptr->depth++;

	/* For find vault spell */
	if ((p_ptr->find_vault) && (p_ptr->depth > 4)) p_ptr->seek_vault = TRUE;

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
	/* odds for extra items in a special chest */
	int die = randint(100 + goodluck - badluck);

	object_type *o_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	/* Get the chest */
	o_ptr = &o_list[o_idx];

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Small chests often hold gold */
	tiny = (o_ptr->sval < SV_CHEST_MIN_LARGE);

	/* Determine how much to drop (see above) */
	number = (o_ptr->sval % SV_CHEST_MIN_LARGE) * 2;

	/* special chests (number isn't set by sval) */
	if (o_ptr->sval == SV_SP_GOLD_CHEST)
	{
		number = 5 + rand_int(2);
		if (die > 100) number += 1;
		else if ((die > 80) && (number < 6)) number = 6;
	}
	else if (o_ptr->sval == SV_SP_SILVER_CHEST)
	{
		number = 3 + randint(2);
		if (die > 100) number += 1;
		else if ((die > 80) && (number < 5)) number += 1;
	}

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 10;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* reset good and great */
		bool good = FALSE;
		bool great = FALSE;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* odds for good / great objects in a special chest */
		die = randint(100 + ((goodluck+1)/2) - badluck);

		/* special chests */
		if (o_ptr->sval == SV_SP_GOLD_CHEST)
		{
			if (randint(100-badluck) < 12) tiny = TRUE; /* may drop gold instead of items */
			else tiny = FALSE;
			if (die > 12) good = TRUE;
			if (die > 70) great = TRUE;
		}
		else if (o_ptr->sval == SV_SP_SILVER_CHEST)
		{
			if (randint(100-badluck) < 20) tiny = TRUE; /* may drop gold instead of items */
			else tiny = FALSE;
			if (die > 25) good = TRUE;
			if (die > 100) great = TRUE;
		}

		/* Small chests often drop gold */
		if (tiny && (rand_int(100) < 75))
		{
			/* Make some gold */
			if ((good) && (randint(100) < 70))
			{
				if (!make_gold(i_ptr, 1)) continue;
			}
			else if (!make_gold(i_ptr, 0)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, good, great)) continue;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
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
	int i, trap;

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
		(void)do_dec_stat(A_STR, 0);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		msg_print("A small needle has pricked you!");
		take_hit(damroll(1, 4), "a poison needle");
		(void)do_dec_stat(A_CON, 0);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msg_print("A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
		{
            (void)inc_timed(TMD_POISONED, 10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void)inc_timed(TMD_PARALYZED, 10 + randint(20));
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		msg_print("You are enveloped in a cloud of smoke!");
		sound(MSG_SUM_MONSTER);
		for (i = 0; i < num; i++)
		{
			(void)summon_specific(y, x, p_ptr->depth, 0);
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


	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Get the "disarm" factor */
		i = p_ptr->skills[SKILL_DIS];

		/* Penalize some conditions */
		if (p_ptr->timed[TMD_BLIND] || no_lite()) i = i / 10;
		if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

		/* Extract the difficulty */
		j = i - o_ptr->pval;

		/* Always have a small chance of success */
		if (j < 2) j = 2;

		/* Success -- May still have traps */
		if (rand_int(100) < j)
		{
			message(MSG_LOCKPICK, 0, "You have picked the lock.");
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

		/* Squelch chest if autosquelch calls for it */
		p_ptr->notice |= PN_SQUELCH;
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
	i = p_ptr->skills[SKILL_DIS];

	/* Penalize some conditions */
	if (p_ptr->timed[TMD_BLIND] || no_lite()) i = i / 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

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
		message(MSG_DISARM, 0, "You have disarmed the chest.");
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
		if (trapped &&
		    (!object_known_p(o_ptr) ||
		     (o_ptr->pval < 0) ||
		     !chest_traps[o_ptr->pval]))
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
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if (!((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
	      (cave_feat[y][x] <= FEAT_DOOR_TAIL)))
	{
		/* Message */
		message(MSG_NOTHING_TO_OPEN, 0, "You see nothing there to open.");

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
		i = p_ptr->skills[SKILL_DIS];

		/* Penalize some conditions */
		if (p_ptr->timed[TMD_BLIND] || no_lite()) i = i / 8;
		if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 5;

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
			message(MSG_LOCKPICK, 0, "You have picked the lock.");

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
	if (easy_open)
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
	if (!more) disturb(0, 0);
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
	if (easy_open)
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
	if (!more) disturb(0, 0);
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
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(y, x))
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
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * Attempting to do so will produce floor grids which are not part
 * of the room, and whose "illumination" status do not change with
 * the rest of the room.
 */
bool twall(int y, int x)
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
	object_type *o_ptr;

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);


	/* Sound XXX XXX XXX */
	/* sound(MSG_DIG); */

	/* umber hulk can dig through permanent vault walls (but not easily) */
	if ((cave_feat[y][x] == FEAT_PERM_INNER) && (cp_ptr->flags & CF_HULK_CONF))
	{
	    /* Tunnel */
	    if ((p_ptr->skills[SKILL_DIG] > 50 + rand_int(2000)) && twall(y, x))
	    {
		   msg_print("You have finished the tunnel.");
	    }

		/* Keep trying */
	    else
	    {
		    /* We may continue tunelling */
		    msg_print("You tunnel into the titanium wall.");
		    more = TRUE;
	    }
	}

	/* Titanium */
	else if (cave_feat[y][x] >= FEAT_PERM_EXTRA)
	{
		msg_print("This seems to be permanent rock.");
	}

	/* Granite */
	else if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
	{
        /* umber hulk never fails when digging */
		if ((cp_ptr->flags & CF_HULK_CONF) && twall(y, x))
        {
            msg_print("You easily bash through the wall.");
        }
        else
        {
		    /* Tunnel */
		    if ((p_ptr->skills[SKILL_DIG] > 40 + rand_int(1600)) && twall(y, x))
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

        /* umber hulk never fails when digging */
		if (cp_ptr->flags & CF_HULK_CONF)
        {
			okay = ((p_ptr->skills[SKILL_DIG] + 21) > 1);
        }
        else
        {    
		   /* Quartz */
           if (hard)
		   {
			  okay = (p_ptr->skills[SKILL_DIG] > 20 + rand_int(800));
		   }

		   /* Magma */
		   else
		   {
			   okay = (p_ptr->skills[SKILL_DIG] > 10 + rand_int(400));
		   }
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
		        if (cp_ptr->flags & CF_HULK_CONF) msg_print("You easily bash through the wall.");
				else msg_print("You have finished the tunnel.");
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
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		/* Remove the rubble (slightly harder than it used to be) */
		if ((p_ptr->skills[SKILL_DIG] > 4 + rand_int(200)) && twall(y, x))
		{
			/* Message */
			msg_print("You have removed the rubble.");

#if old
			/* Hack -- place an object */
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE);

				/* Observe the new object */
				if (!squelch_hide_item(&o_list[cave_o_idx[y][x]]) &&
				    player_can_see_bold(y, x))
				{
					msg_print("You have found something!");
				}
			}
#else
#endif
		}

		else
		{
			/* Message, keep digging */
			msg_print("You dig in the rubble.");
			more = TRUE;
		}
	}

	/* Secret doors */
	else if (cave_feat[y][x] >= FEAT_SECRET)
	{
        /* umber hulk never fails when digging */
		if (cp_ptr->flags & CF_HULK_CONF)
        {
            twall(y, x);
            msg_print("You easily bash through the wall.");
        }
        else
        {
		    /* Tunnel */
		    if ((p_ptr->skills[SKILL_DIG] > 30 + rand_int(1200)) && twall(y, x))
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
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if ((p_ptr->skills[SKILL_DIG] > 30 + rand_int(1200)) && twall(y, x))
		{
			msg_print("You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			msg_print("You tunnel into the door.");
			more = TRUE;
		}
	}

	/* Scan all objects in the grid */
	/* (can't use &o_list[cave_o_idx[y][x]] because */
	/* there's more than one object with the big rocks) */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* if there is a buried object there, unhide it */
		if (o_ptr->hidden)
		{
			o_ptr->hidden = 0;
			/* sometimes remains hidden for squelch */
			if ((!squelch_hide_item(o_ptr)) && player_can_see_bold(y, x))
			{
				msg_print("You have found something!");
				/* Notice & redraw */
				note_spot(y, x);
				lite_spot(y, x);
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
	if (!more) disturb(0, 0);
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


	/* Get the trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

	/* Get the "disarm" factor */
	i = p_ptr->skills[SKILL_DIS];

	/* Penalize some conditions */
	if (p_ptr->timed[TMD_BLIND] || no_lite()) i = i / 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

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
		message_format(MSG_DISARM, 0, "You have disarmed the %s.", name);

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
	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;


	/* Easy Disarm */
	if (easy_open)
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
	if (!more) disturb(0, 0);
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

	/* always succeed with mighty_hurl */
	if (p_ptr->timed[TMD_MIGHTY_HURL]) temp = 101;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Break down the door */
		if ((rand_int(100) < 50) || (p_ptr->timed[TMD_MIGHTY_HURL]))
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
		(void)inc_timed(TMD_PARALYZED, 2 + rand_int(2));
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
		msg_print("There is a monster in the way!");

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
			disturb(0, 0);
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
#if 0
	/* Bash jammed doors */
	else if (feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Tunnel */
		more = do_cmd_bash_aux(y, x);
	}
#endif /* 0 */
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

#if 0

	/* Close open doors */
	else if (feat == FEAT_OPEN)
	{
		/* Close */
		more = do_cmd_close_aux(y, x);
	}

#endif

	/* Oops */
	else
	{
		/* Oops */
		msg_print("You spin around.");
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
	int y, x, dir, item = 0;


	/* Get a spike */
	if (!get_spike(&item))
	{
		/* Message */
		msg_print("You have no spikes!");

		/* Done */
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_spike_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
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
		if (!do_cmd_spike_test(y, x)) return;

		/* Successful jamming */
		msg_print("You jam the door with a spike.");

		/* Convert "locked" to "stuck" XXX XXX XXX */
		if (cave_feat[y][x] < FEAT_DOOR_HEAD + 0x08)
		{
			cave_feat[y][x] += 0x08;
		}

		/* Add one spike to the door */
		if (cave_feat[y][x] < FEAT_DOOR_TAIL)
		{
			cave_feat[y][x] += 0x01;
		}

		/* Use up, and describe, a single spike, from the bottom */
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
}



/*
 * Determine if a given grid may be "walked"
 */
bool do_cmd_walk_test(int y, int x)
{
	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Allow attack on visible monsters */
	if ((cave_m_idx[y][x] > 0) && (mon_list[cave_m_idx[y][x]].ml))
	{
		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		/* atttack ordinary trees only when you can see them */
		if ((m_ptr->r_idx == 834) && (!m_ptr->ml))
		{
			message(MSG_HITWALL, 0, "You feel a tree in the way!");
			return (FALSE);
		}
		return TRUE;
	}
	/* allow attack on invisible monster you just heard */
	else if ((cave_m_idx[y][x] > 0) && (mon_list[cave_m_idx[y][x]].heard))
	{
		return TRUE;
	}

	/* held by a monster */
	if ((p_ptr->timed[TMD_BEAR_HOLD]) && (p_ptr->held_m_idx))
	{
		monster_type *m_ptr = &mon_list[p_ptr->held_m_idx];
		char m_name[80];
		/* int curd = distance(p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx); */
		/* int newd = distance(y, x, m_ptr->fy, m_ptr->fx); */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
		if (distance(y, x, m_ptr->fy, m_ptr->fx) > m_ptr->cdis)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			int wrestle, holdfast, rlev;
			rlev = ((r_ptr->level >= 6) ? r_ptr->level : 6);

			if (r_ptr->flags1 & (RF1_UNIQUE)) holdfast = rlev * 3;
			else holdfast = (rlev * 5) / 2 + randint(rlev/2); /* (x2.5 +dx0.5) */
			wrestle = adj_str_wgt[p_ptr->stat_ind[A_STR]] + goodluck/2;
			wrestle += adj_str_wgt[p_ptr->stat_ind[A_DEX]];
			if (p_ptr->free_act) wrestle += 20;
	
			/* attempt to pull free */
			if (randint(wrestle) > holdfast)
			{
				p_ptr->held_m_idx = 0;
				clear_timed(TMD_BEAR_HOLD);
			}
			else
			{
				msg_format("You are being held by %^s!", m_name);
				return (FALSE);
			}
		}
	}

	/* Require open space */
	if ((!cave_floor_bold(y, x)) || (cave_feat[y][x] == FEAT_OPEN_PIT))
	{
		/* Rubble */
		if (cave_feat[y][x] == FEAT_RUBBLE)
		{
			/* rubble can be climbed over now */
            return (TRUE);
            /* Message *
			message(MSG_HITWALL, 0, "There is a pile of rubble in the way!"); */
		}

		/* open pit (not a trap) */
		else if (cave_feat[y][x] == FEAT_OPEN_PIT)
		{
			return (TRUE);
		}

		/* Door */
		else if (cave_feat[y][x] <= FEAT_DOOR_TAIL)
		{
			/* Hack -- Handle "easy_alter" */
			if (easy_alter) return (TRUE);

			/* Message */
			message(MSG_HITWALL, 0, "There is a door in the way!");
		}

		/* Wall */
		else
		{
			/* Message */
			message(MSG_HITWALL, 0, "There is a wall in the way!");
		}

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for the "walk" command.
 */
void do_cmd_walk(void)
{
	int y, x, dir;

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];
	}


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


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
	move_player(dir);
}

/*
 * Jump into a trap, turn off pickup.
 *
 * What a horrible function.
 */
void do_cmd_jump(void)
{
	bool old_easy_alter;

	/* Picking up NOT okay, so whatever you heard is obviously wrong */
	p_ptr->auto_pickup_okay = FALSE;

	/* easy_alter can be turned off (don't disarm traps) */
	old_easy_alter = easy_alter;
	easy_alter = FALSE;

	/* Now actually do this silly walk */
	do_cmd_walk();

	/* Restore easy_alter */
	easy_alter = old_easy_alter;
}



/*
 * Start running.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_run(void)
{
	int y, x, dir;


	/* Hack XXX XXX XXX */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Start run */
	run_step(dir);
}

/*
 * Start running with pathfinder.
 *
 * Note that running while confused is not allowed.
 */
void do_cmd_pathfind(int y, int x)
{
	/* Hack XXX XXX XXX */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}

	if (findpath(y, x))
	{
		p_ptr->running = 1000;
		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
		p_ptr->running_withpathfind = TRUE;
		run_step(0);
	}
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_hold(void)
{
	int trapalert;
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

	trapalert = p_ptr->skills[SKILL_FOS];
	/* mainly to help dwarves who have darkvision but horrible alertness */
	if (p_ptr->darkvis) trapalert += 6;

	/* Spontaneous Searching */
	if ((trapalert >= 50) || (0 == rand_int(50 - trapalert)))
	{
		search();
	}

	/* Continuous Searching */
	if (p_ptr->searching)
	{
		search();
	}

	/* Handle objects now.  XXX XXX XXX */
	p_ptr->energy_use += py_pickup(0) * 10;

	/* Hack -- enter a store if we are on one */
	if ((cave_feat[p_ptr->py][p_ptr->px] >= FEAT_SHOP_HEAD) &&
	    (cave_feat[p_ptr->py][p_ptr->px] <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';

		/* Free turn XXX XXX XXX */
		p_ptr->energy_use = 0;
	}
}



/*
 * Pick up objects on the floor beneath you.  -LM-
 */
void do_cmd_pickup(void)
{
	int energy_cost;

	/* Pick up floor objects, forcing a menu for multiple objects. */
	energy_cost = py_pickup(1) * 10;

	/* Maximum time expenditure is a full turn. */
	if (energy_cost > 100) energy_cost = 100;

	/* Charge this amount of energy. */
	p_ptr->energy_use = energy_cost;
}

/*
 * DJA: Telekinesis: Pick up objects on the floor from a distance.
 * spellswitch = 24
 */
bool do_telekinesis(void)
{
	int energy_cost = 0;
	int dir;

	/* spellswitch = 24 allows picking up objects from a distance (uses target) */
	/* also prevent using old target & changes the prompt for get_aim_dir */
	spellswitch = 24;

    /* get a target (never use old target) */
	if (!get_aim_dir(&dir))
	{
		/* reset spellswitch */
		spellswitch = 0;

		return FALSE;
	}

	/* Pick up floor objects, forcing a menu for multiple objects. */
	energy_cost = py_pickup(2) * 10;
	
	/* (using the spell already uses energy) */

	/* did we pick anything up? */
    if (energy_cost >= 10)
	{
		/* chance of waking up monsters in path of the floating object */
		if (randint(100) < 9 + (badluck*2) - ((goodluck+1)/2))
		{
			/* 0 damage beam to wake up monsters in path */
			fire_beam(GF_THROW, dir, 0);
		}
	}

	 /* reset target */
	 /* (would usually not want to re-use a telekinesis target) */
	 p_ptr->target_row = 0;
	 p_ptr->target_col = 0;

	/* reset spellswitch */
	spellswitch = 0;

	 return TRUE;
}



/*
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
#ifdef EFG
		/* EFGchange rest '|' to match rest '&' */
		cptr p = "Rest (0-9999, '*' HP and SP, '|' HP or SP, '&' as needed): ";
#else
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";
#endif

		char out_val[5] = "& ";

		/* Ask for duration */
		if (!get_string(p, out_val, sizeof(out_val))) return;

#ifdef EFG
		/* EFGchange rest '|' to match rest '&' */
		switch(out_val[0])
		{
			case '&':
				p_ptr->resting = REST_FULL;
				break;
			case '*':
				p_ptr->resting = REST_BOTH;
				break;
			case '|':
				p_ptr->resting = REST_EITHER;
				break;
			default:
				p_ptr->resting = atoi(out_val);
				if (p_ptr->resting < 0)
					p_ptr->resting = 0;
				if (p_ptr->resting > 9999)
					p_ptr->resting = 9999;
		}
	}
#else
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
	p_ptr->energy_use = 100;

	/* Save the rest code */
	p_ptr->resting = p_ptr->command_arg;
#endif

	/* Cancel the arg */
	p_ptr->command_arg = 0;

	/* Cancel searching */
	p_ptr->searching = FALSE;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Refresh XXX XXX XXX */
	Term_fresh();
}






/*
 * Determines the odds of an object breaking when thrown at a monster
 * (Hitwall means there was no collision with a monster but still chance to break)
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(const object_type *o_ptr, bool hitwall)
{
	int defolt;
	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		{
			if (hitwall) return (75);
			else return (100);
		}

		/* Usually breaks */
		case TV_FOOD:
		{
			if (hitwall) return (0);
			else return (75);
		}

		/* Often break */
		case TV_LITE:
		case TV_JUNK:
		{
			if (hitwall) return (0);
			else return (50);
		}
		
		/* Sometimes break */
		case TV_SCROLL: /* reduced chance */
		{
			if (hitwall) return (0);
			else return (40);
		}

		case TV_ARROW:
		{
			if (hitwall) return (0);
			else return (35);
		}

		/* tusks as spears */
        case TV_SKELETON:
		{
			if (hitwall) return (0);
			else return (30);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		{
			if (hitwall) return (0);
			else return (25);
		}
	}
	
	if (hitwall) defolt = 0;
	else
	{
		defolt = 10;
		/* egos don't break as easily */
		if (o_ptr->name2) defolt -= 4;
	}

	/* Rarely break */
	return (defolt);
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
 * (but there are now weapons which are meant for throwing)
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Archers (with Bows) and Anyone (with "Extra Shots") get extra shots.
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
	int i, j, y, x, ty, tx, dy, dx;
	int tdam, tdis, thits, tmul, noslip;
	int bonus, chance, excrit;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;
	bool hitwall = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];
	u32b f1, f2, f3, f4;

	int path_n;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get the "bow" (if any) */
	j_ptr = &inventory[INVEN_BOW];

	/* Require a usable launcher */
	if (!j_ptr->tval || !p_ptr->ammo_tval)
	{
		msg_print("You have nothing to fire with.");
		return;
	}


	/* Require proper missile */
	item_tester_tval = p_ptr->ammo_tval;

	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_QUIVER | USE_INVEN | USE_FLOOR))) return;
	
		
	/* Handle player charm */
	if (p_ptr->timed[TMD_CHARM])
	{
		/* Message */
		msg_print("You are too good a mood to try to hurt anything!");

		/* Done */
		return;
	}

	/* Handle terror */
	if (p_ptr->timed[TMD_TERROR])
	{
		/* Message */
		msg_print("You are too desperate to escape to fire anything!");

		/* Done */
		return;
	}

	/* Get the object */
	if (item >= 0)
	{
		o_ptr = &inventory[item];

		/* A cursed quiver disables the use of non-cursed ammo */
		if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver && !cursed_p(o_ptr))
		{
			msg_print("Your quiver is cursed!");
			return;
		}
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

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

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
	sound(MSG_SHOOT);

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from ammo plus launcher bonus */
	tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d + j_ptr->to_d;

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
	chance = (p_ptr->skills[SKILL_THB] + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage (but not by as much as before) */
	/* multiplier is now (tmul * .875) based on x2 to x1.75 */
	if (tmul == 2) tdam = ((tdam * 7) / 4);       /* (x1.75) */
	else if (tmul == 3) tdam = ((tdam * 21) / 8); /* (x2.625) */
	else if (tmul == 4) tdam = ((tdam * 7) / 2);  /* (x3.5) */
	else if (tmul == 5) tdam = ((tdam * 35) / 8); /* (x4.375) */
	else if (tmul > 5) tdam = ((tdam * 21) / 4);  /* (x5.25) */
    /* tdam *= tmul; */

    excrit = o_ptr->crc - 5;
	if (f2 & TR2_EXTRA_CRIT) excrit += 14; /* was 11 */
	if ((bonus + p_ptr->skills[SKILL_THB])-1 > 12) excrit += ((bonus + p_ptr->skills[SKILL_THB])-1)/12;

	/* Base range XXX XXX (DJA: 2 less than it was) */
	tdis = 8 + 5 * tmul;

	/* Take a (partial) turn */
	p_ptr->energy_use = (100 / thits);

	
	/* cursed ammo misfires */
	if (cursed_p(o_ptr))
	{
		/* changed: misfire was too common for PCs with low bow skill */
		/* noslip = 125 - p_ptr->skills[SKILL_THB]; */
		noslip = 75 - (p_ptr->skills[SKILL_THB] / 2);
		if (noslip < 8) noslip = 8;

		/* random direction & less likely to hit */
		if (randint(100) < noslip)
		{
			dir = ddd[rand_int(8)];
			msg_format("The %s misfires.", o_name);
			chance -= 5;
		}
	}


	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* DJXXX can shoot at monsters who are partly in a wall */
		if ((!cave_floor_bold(ny, nx)) && (cave_m_idx[ny][nx] > 0))
		{
			/* save grid in front of the wall for drop_near */;
			dy = y;
			dx = x;
			hitwall = TRUE;

			/* hard to hit monsters which are part way in a wall */
			chance = (chance * 3) / 4;
		}
		/* Hack -- Stop before hitting walls */
		else if (!cave_floor_bold(ny, nx))
		{
			/* chance of breakage when hitting a wall */
			if (randint(100) < 40 - goodluck) hit_body = TRUE;
			break;
		}

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);

			Term_fresh();
			if (p_ptr->window) window_stuff();

			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);

			Term_fresh();
			if (p_ptr->window) window_stuff();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			char m_name[80];
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			int visible = m_ptr->ml;

			/* hard to hit an invisible (or very stealthy) monster */
			if ((!visible) && (player_can_see_bold(y, x))) 
				chance2 = (chance2 * 3) / 4;
			/* slightly hard to hit a monster outside of your light range */
			else if (!visible) chance2 = (chance2 * 9) / 10;

			/* Get "the monster" or "it" */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize distance travelled) */
			if (test_hit(chance2, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
				    (r_ptr->flags3 & (RF3_NON_LIVING)) ||
				    (r_ptr->flags2 & (RF2_STUPID)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}


				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					message_format(MSG_SHOOT_HIT, 0, "The %s finds a mark.", o_name);
				}

				/* Handle visible monster */
				else
				{
					/* Message */
					message_format(MSG_SHOOT_HIT, 0, "The %s hits %s.", o_name, m_name);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}
				
				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, 1, m_ptr);
				tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam, 0, excrit);
				
                /* DJA: for races / classes who like slings, */
				/* slings get strength bonus to damage */
				/* (rogues, druids, barbarians, hobbits, and ghouls) */
				if ((p_ptr->ammo_tval == TV_SHOT) && (tmul < 3))
				{
					if (p_ptr->timed[TMD_MIGHTY_HURL])
					{
                       tdam += (((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) * 3) / 2;
					}
					else if ((p_ptr->pclass == 3) || (p_ptr->pclass == 10) || (p_ptr->pclass == 18) ||
						(p_ptr->prace == 3) || (p_ptr->prace == 14))
					{
                       tdam += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
					}
                }

				

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (p_ptr->wizard)
				{
					msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(cave_m_idx[y][x], tdam);
	
					/* end truce */
					if (m_ptr->truce)
					{
						m_ptr->truce = 0;
						msg_format("%^s cancells your truce.", m_name);
					}

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx,
						               "%^s flees in terror!", m_name);
					}
				}
			}
			else /* miss */
			{
				if (hitwall)
				{
					msg_format("The %s hits the wall.", o_name);
					/* less likely to break when hitting the wall */
					if (randint(100) < 40) hit_body = FALSE;
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr, FALSE) : 0);

	/* Drop (or break) near that location */
	if (hitwall) drop_near(i_ptr, j, dy, dx);
	else drop_near(i_ptr, j, y, x);
	
    /* apply exp drain */
    if (p_ptr->exp_drain) rxp_drain(45);
}


/*
 * Figure number of thrown weapons of the chosen item's weight
 * that you can throw in one turn.
 * (also called by obj_info.c)
 *
 * Very similar to number of blows with a melee weapon except
 * that the class min weight doesn't apply and the
 * multiplier is determined by throwing skill.
 * Also, it is applied like extra shots with a bow (by taking a partial turn)
 */
int thits_thrown(int weight)
{
	int str_index, dex_index;
	int throws, mult, maxt;
	int eskill = adj_dex_blow[p_ptr->stat_ind[A_DEX]] + p_ptr->skills[SKILL_THT];
	/* minimum weight (the weight of a throwing dagger) */
	if (weight < 12) weight = 12;

	/* determine multiplier from throwing skill and DEX */
	if (eskill < 60) mult = 1;
	else if (eskill < 90) mult = 2;
	else if (eskill < 120) mult = 3;
	else if (eskill < 152) mult = 4;
	else if (eskill < 198) mult = 5;
	else mult = 6;

	/* Get the strength vs weight */
	str_index = ((adj_str_blow[p_ptr->stat_ind[A_STR]] * mult) / (weight * 3));

	/* Maximal value */
	if (str_index > 11) str_index = 11;

	/* Index by dexterity */
	dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

	/* Maximal value */
	if (dex_index > 11) dex_index = 11;

	/* Use the blows table */
	throws = throw_blows_table[str_index][dex_index];

	/* Maximal value (possibly make it max 4 for everyone) */
	if (cp_ptr->max_attacks >= 5) maxt = 5;
	else maxt = 4;
	if (throws > maxt) throws = maxt;

	return throws;
}

/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?
 */
void do_cmd_throw(void)
{
	int dir, item;
	int i, j, y, x, ty, tx, dy, dx;
	int chance, tdam, tdis, noslip, thits;
	int mul, div;
	bool comeback, throwok, tooheavy, throwglove, hitwall = FALSE;
	int comechance, excrit, bonus;

	object_type *o_ptr;
	object_type *g_ptr; /* g_ptr to check gloves */
	u32b f1, f2, f3, f4;

	object_type *i_ptr;
	object_type object_type_body;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int path_n;
	u16b path_g[256];

	cptr q, s;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;


	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_QUIVER | USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	if (item >= 0)
	{
		o_ptr = &inventory[item];

		/* A cursed quiver disables the use of non-cursed ammo */
		if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver && !cursed_p(o_ptr))
		{
			msg_print("Your quiver is cursed!");
			return;
		}
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	tooheavy = FALSE;
	if ((!cp_ptr->flags & CF_HEAVY_BONUS) && (o_ptr->weight >= 200))
		tooheavy = TRUE;
	if ((o_ptr->tval == TV_SKELETON) && (o_ptr->sval == SV_BIG_ROCK))
		tooheavy = TRUE;
	if ((p_ptr->timed[TMD_MIGHTY_HURL]) || ((cp_ptr->flags & CF_HEAVY_BONUS) && (((int)(adj_con_fix[p_ptr->stat_ind[A_STR]]) - 128) > 7)))
		tooheavy = FALSE;

	if (tooheavy)
	{
		if (!get_check("This item is too heavy to throw effectively, do it anyway? "))
		{
			return;
		}
	}
	if ((o_ptr->tval == TV_SKELETON) && (o_ptr->sval == SV_BIG_ROCK))
		tooheavy = TRUE;

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/*** Hacky: Check for to_dam bonus from gauntlets of throwing ***/
	g_ptr = &inventory[INVEN_HANDS];
	if (g_ptr->k_idx)
	{
		/* Extract the gloves' flags */
		object_flags(g_ptr, &f1, &f2, &f3, &f4);

		/* add to_dam from gloves if appropriate (later) */
		if (f3 & (TR3_THROWMULT)) throwglove = TRUE;

		/* reset flags before getting thrown object flags */
		f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;
	}
	else throwglove = FALSE;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Extract the thrown object flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	
	comeback = FALSE;
	comechance = p_ptr->skills[SKILL_THT] / 5;
	if (f2 & TR2_RTURN)
	{
	   if (randint(120 + badluck) < 68 + comechance) comeback = TRUE;
    }

    /* Distribute the charges of rods/wands/staves between the stacks */
    /* (these should never have RTURN) */
    distribute_charges(o_ptr, i_ptr, 1);

    /* Single object */
    i_ptr->number = 1;

    /* returns? ..ok, it never actually leaves, but the player doesn't know that */
    if (comeback == FALSE)
    {
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
    }


	/* Description */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* limit distance for very heavy stuff */
	if ((!cp_ptr->flags & CF_HEAVY_BONUS) && (!p_ptr->timed[TMD_MIGHTY_HURL]) && 
		(o_ptr->weight >= 200))
	{
		if (tdis > 2) tdis = 2;
	}
	else if ((p_ptr->timed[TMD_MIGHTY_HURL]) && (cp_ptr->flags & CF_HEAVY_BONUS))
	{
		/* recycles the CON regeneration table */
		tdis += (adj_con_fix[p_ptr->stat_ind[A_STR]]);
		if (((o_ptr->weight >= 200) || (tooheavy)) && (tdis > 10)) tdis = 10;
	}
	/* HEAVY_BONUS class bonus to distance */
	else if ((cp_ptr->flags & CF_HEAVY_BONUS) || (p_ptr->timed[TMD_MIGHTY_HURL]))
	{
		/* recycles the CON regeneration table */
		tdis += (adj_con_fix[p_ptr->stat_ind[A_STR]]) / 2;
		if (((o_ptr->weight >= 200) || (tooheavy)) && (tdis > 8)) tdis = 8;
	}

	/* Max distance of 10 (or 12 if meant for throwing) */
	if ((p_ptr->timed[TMD_MIGHTY_HURL]) && (cp_ptr->flags & CF_HEAVY_BONUS))
	{
		if (tdis > 14) tdis = 14;
		if ((o_ptr->weight >= 150) && (tdis > 12)) tdis = 12;
	}
	else if (((f2 & TR2_THROWN) || (cp_ptr->flags & CF_HEAVY_BONUS) || 
		(p_ptr->timed[TMD_MIGHTY_HURL])) && (o_ptr->weight < 150))
	{
		if (tdis > 12) tdis = 12;
	}
	else if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d;

	/* add to_dam from gloves if appropriate */
	if (throwglove) tdam += g_ptr->to_d;

	/* determine whether to boost the damage */
	if (f2 & TR2_THROWN) throwok = TRUE;
	else throwok = FALSE;
	/* can be too heavy to throw effectively even if it's meant for throwing */
	if ((!cp_ptr->flags & CF_HEAVY_BONUS) && (o_ptr->weight >= 200) && 
		(!p_ptr->timed[TMD_MIGHTY_HURL]))
	{
		/* (if STR is less than 18/150) */
		if (((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) < 10) throwok = FALSE;
	}

	/* Boost the damage if appropriate */
	if (throwok)
	{
		/* gauntlets of throwing (or something else with the THROWMULT power) */
		if (p_ptr->throwmult == 2) tdam = ((tdam * 7) / 4);       /* (x1.75) */
		else if (p_ptr->throwmult == 3) tdam = ((tdam * 21) / 8); /* (x2.625) */
		else if (p_ptr->throwmult >= 4) tdam = ((tdam * 7) / 2);  /* (x3.5) */
		/* normal throwing weapon multiplier:  x1.4 */
		else tdam = (tdam * 7) / 5;
    }
    /* certain classes are strong enough to throw more effectively (x1.2) */
    /* (warrior, barbarian, and the knights) */
    else if ((cp_ptr->flags & CF_HEAVY_BONUS) || (p_ptr->pclass == 0) ||
         (cp_ptr->flags & CF_KNIGHT) || (p_ptr->timed[TMD_MIGHTY_HURL]))
    {
       tdam = (tdam * 6) / 5;
    }
	/* too heavy */
	else if (o_ptr->weight >= 200)
	{
		tdam = tdam / 2;
	}

	/* Chance of hitting */
	chance = (p_ptr->skills[SKILL_THT] + (p_ptr->to_h * BTH_PLUS_ADJ));

	/* Weapons meant for throwing get weapon to-hit bonus */
	if (f2 & TR2_THROWN) chance += i_ptr->to_h;
	/* otherwise still get partial to-hit bonus from object */
	else chance += (i_ptr->to_h + 1) / 3;

    excrit = i_ptr->crc - 5;
    bonus = p_ptr->skills[SKILL_THT] + p_ptr->to_h;
	if (f2 & TR2_THROWN) bonus += i_ptr->to_h;
	if (f2 & TR2_EXTRA_CRIT) excrit += 12;
	if (bonus-1 > 12) excrit += (bonus-1)/12;

	/* number of throws (assuming thrown items are the same weight) */
	if (f2 & TR2_THROWN) thits = thits_thrown(div);
	else thits = 1;

	/* Take a (partial) turn */
	p_ptr->energy_use = 100 / thits;


	/* cursed thrown weapons slip (limit to weapons meant for */
	/* throwing or else curses become too easy to recognise) */
	if ((cursed_p(i_ptr)) && (f2 & TR2_THROWN))
	{
		/* (side effect: lower skill will be able to recognise curses easier) */
		/* changed: misfire was too common for PCs with low skill */
		/* noslip = 130 - (p_ptr->skills[SKILL_THT]); */
		noslip = 80 - (p_ptr->skills[SKILL_THT] / 2);
		if (noslip < 10) noslip = 10;

		/* random direction */
		if (randint(100) < noslip)
		{
			dir = ddd[rand_int(8)];
			msg_format("The %s slips as you throw it.", o_name);
			chance -= 5;
		}
	}
	else if (cursed_p(i_ptr)) chance -= 5;


	/* Start at the player */
	y = p_ptr->py;
	x = p_ptr->px;

	/* Predict the "target" location */
	ty = p_ptr->py + 99 * ddy[dir];
	tx = p_ptr->px + 99 * ddx[dir];

	/* Check for "target request" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Calculate the path */
	path_n = project_path(path_g, tdis, p_ptr->py, p_ptr->px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* DJXXX can shoot at monsters who are partly in a wall */
		if ((!cave_floor_bold(ny, nx)) && (cave_m_idx[ny][nx] > 0))
		{
			/* save grid in front of the wall for drop_near */;
			dy = y;
			dx = x;
			hitwall = TRUE;

			/* hard to hit monsters which are part way in a wall */
			chance = (chance * 3) / 4;
		}
		/* Hack -- Stop before hitting walls */
		else if (!cave_floor_bold(ny, nx))
		{
			/* chance of breakage when hitting a wall */
			if (randint(100) < 50 - goodluck) hitwall = TRUE;
			break;
		}

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);

			Term_fresh();
			if (p_ptr->window) window_stuff();

			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);

			Term_fresh();
			if (p_ptr->window) window_stuff();
		}

		/* Delay anyway for consistency */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Handle monster */
		if (cave_m_idx[y][x] > 0)
		{
			int chance2, visible;
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			int num, tblows;

			/* Get "the monster" or "it" */
			char m_name[80];
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);

			chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;
			
			/* Get number of blows */
			num = 0;
	        tblows = 1;
            if (f1 & TR1_BLOWS)
            {
               tblows += i_ptr->pval;
            }
            
	        /* Attack once for each legal blow (for dancing weapons) */
	        while (num++ < tblows)
	        {
			    /* don't penalize range for extra hits of a dancing weapon */
				if (num > 1) chance2 = chance;

				/* Did we hit it (penalize range) */
			    if (test_hit(chance2, r_ptr->ac, m_ptr->ml))
			    {
				   bool fear = FALSE;

				   /* Assume a default death */
				   cptr note_dies = " dies.";

				   /* Some monsters get "destroyed" */
				   if ((r_ptr->flags3 & (RF3_DEMON)) ||
				      (r_ptr->flags3 & (RF3_NON_LIVING)) ||
				      (r_ptr->flags2 & (RF2_STUPID)))
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
					   /* Message */
					   msg_format("The %s hits %s.", o_name, m_name);

					   /* Hack -- Track this monster race */
					   if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					   /* Hack -- Track this monster */
					   if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				   }

                   /* DJA: add (partial) strength bonus for certain classes */
                   /* class 0 will always be warrior so no need for a flag in that case */
                   /* weapons meant for throwing always get STR bonus */
                   if ((cp_ptr->flags & CF_HEAVY_BONUS) || (cp_ptr->flags & CF_KNIGHT) ||
                      (p_ptr->pclass == 0) || (f2 & TR2_THROWN) || (p_ptr->timed[TMD_MIGHTY_HURL]))
                   {
                      int strb;
					  int eweight = o_ptr->weight;
                      if (!(f2 & TR2_THROWN)) eweight = eweight / 2; /* less if not meant for throwing */
                      /* complex strength bonus by weight (different than melee) */
                      strb = 10 * ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
				      if (eweight < 25) strb = strb / 2;
				      else if (eweight < 40) strb = (strb * 2) / 3;
				      else if (eweight < 50) strb = (strb * 3) / 4;
				      else if (eweight < 60) strb = (strb * 5) / 6;
				      strb = strb / 10;
					  if (p_ptr->timed[TMD_MIGHTY_HURL]) strb = strb * 2;
                      tdam += strb;
                   }

				   /* Apply special damage XXX XXX XXX */
				   /* moved to after STRbonus because throwing weapons were too weak */
				   tdam = tot_dam_aux(i_ptr, tdam, 0, m_ptr);

                   /* criticals less likely when weapon is thrown */
				   /* unless weapon is meant for throwing */
				   if (f2 & TR2_THROWN)
                   {
                       tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam, 0, excrit);
                   }
                   /* weapons not meant for throwing get to-hit bonus halved for crit chance */
				   else
                   {
				       bool weapon = FALSE;
                       if (wield_slot(o_ptr) == INVEN_WIELD) weapon = TRUE;
                       
                       if ((o_ptr->tval == 16) || (o_ptr->tval == 17) || (o_ptr->tval == 18) || (o_ptr->tval == 66))
                          weapon = TRUE;

                       /* when throwing something that isn't a weapon, critical is very unlikely */
                       if (weapon) tdam = critical_shot(i_ptr->weight, (i_ptr->to_h)/2, tdam, 1, excrit);
                       else tdam = critical_shot(i_ptr->weight, (i_ptr->to_h)/2, tdam, 2, excrit);
                   }

				   /* No negative damage */
				   if (tdam < 0) tdam = 0;

				   /* Complex message */
				   if (p_ptr->wizard)
				   {
					  msg_format("You do %d (out of %d) damage.",
					           tdam, m_ptr->hp);
				   }

				   /* Hit the monster, check for death */
				   if (mon_take_hit(cave_m_idx[y][x], tdam, &fear, note_dies))
				   {
					  /* Dead monster */
					  tblows = 0;
				   }

				   /* No death */
				   else
				   {
					   /* Message */
					   message_pain(cave_m_idx[y][x], tdam);

						/* end truce */
						if ((m_ptr->truce) && (tdam > 2))
						{
							m_ptr->truce = 0;
							msg_format("%^s cancells your truce.", m_name);
						}

					   /* Take note */
					   if (fear && m_ptr->ml)
					   {
						  char m_name[80];

						  /* Get the monster name (or "it") */
						  monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						  /* Message */
						  message_format(MSG_FLEE, m_ptr->r_idx,
						               "%^s flees in terror!", m_name);
					   }
				   }
			    }
			    /* miss message */
			    else if (m_ptr->ml)
                {
					if (hitwall)
					{
						msg_format("The %s hits the wall.", o_name);
						/* much less likely to break when hitting the wall */
						if (randint(100) < 60) hit_body = FALSE;
					}
					else msg_format("The %s misses %s.", o_name, m_name);
			    }
            }

			/* Stop looking */
			break;
		}
	}

    if ((f2 & TR2_RTURN) && (comeback == FALSE) && (randint(120 + badluck/2) < 50 + comechance))
    {
	   /* Chance of breakage (during attacks) */
	   /* Throwing weapons very rarely break */
	   if (hit_body)
	   {
	      if (f2 & TR2_THROWN) j = 0;
	      else j = breakage_chance(i_ptr, FALSE);
          /* j = (hit_body ? breakage_chance(i_ptr) : 0); */
	      if (cursed_p(o_ptr)) j += 3;
	      if (badluck > 9) j += (badluck-6) / 4;
	      if ((goodluck > 5) && (j > 0)) j -= (goodluck/5);
       }
	   /* some objects have a chance to break when hitting a wall without a monster */
       else j = 0;

       /* chance of hitting yourself with a dangerous weapon */
       /* (usually happens when the weapon returns and you don't catch it) */
	   if ((f2 & TR2_DANGER) && (randint((comechance*5)-badluck) < 75) && ((comechance*5) < 175))
	   {
          int ouch = damroll(o_ptr->dd, o_ptr->ds);
          ouch = self_dam_aux(o_ptr, ouch);

          /* object damage bonus after semi-multipliers */
          if (o_ptr->k_idx) ouch += o_ptr->to_d;

          msg_format("You are hit by the %s as it returns to you!", o_name);
          take_hit(ouch, "your own weapon");
       }
       else msg_format("The %s comes back to you, but you fail to catch it.", o_name);
	   
	   /* Drop (or break) near the player */
	   drop_near(i_ptr, j, p_ptr->py, p_ptr->px);
    }
    else if (comeback == FALSE)
    {
	   /* Chance of breakage (during attacks) */
	   /* Throwing weapons very rarely break */
	   if (hit_body)
	   {
	      if (f2 & TR2_THROWN) j = 1;
	      else j = breakage_chance(i_ptr, FALSE);
          /* j = (hit_body ? breakage_chance(i_ptr) : 0); */
	      if (cursed_p(o_ptr)) j += 3;
	      if (badluck > 9) j += (badluck-6) / 4;
	      if ((goodluck > 5) && (j > 0)) j -= (goodluck/5);
       }
	   /* some objects have a chance to break when hitting a wall without a monster */
	   else if ((hitwall) && (!f2 & TR2_THROWN)) j = breakage_chance(i_ptr, TRUE);
       else j = 0;

	   /* Drop (or break) near that location */
	   if ((hitwall) && (dx)) drop_near(i_ptr, j, dy, dx);
	   else drop_near(i_ptr, j, y, x);
    }
    else msg_format("You catch the %s as it comes back to you.", o_name);

    /* exp drain only kicks in if you were trying to hurt a monster */
    if ((p_ptr->exp_drain) && (hit_body))
    {
        rxp_drain(33);
    }
}

/*
 * See if one can squelch a given kind of item.
 */
static bool squelchable_hook(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

#ifdef EFG
        /* EFGchange allow squelching unaware objects */
        /* EFGchange code cleaning */
	if (squelch_item_ok(o_ptr)) return FALSE;
#else
	/* No point in double-squelching things */
	if (k_ptr->squelch) return FALSE;
#endif

	/* Don't squelch bad tvals */
	if (!squelch_tval(o_ptr->tval)) return FALSE;

#ifdef EFG
        /* EFGchange allow squelching unaware objects */
	return TRUE;
#else
	/* Only allow if aware */
	return object_aware_p(o_ptr);
#endif
}



/*
 * Mark item as "squelch".
 */
void do_cmd_mark_squelch()
{
	const char *q = "Squelch which item kind? ";
	const char *s = "You have nothing you can squelch.";

	object_type *o_ptr;
#ifdef EFG
	/* */
#else
	object_kind *k_ptr;
#endif
	int item;

	/* Get an item */
	item_tester_hook = squelchable_hook;
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
	if (item >= 0)
		o_ptr = &inventory[item];
	else
		o_ptr = &o_list[0 - item];

#ifdef EFG
        /* EFGchange allow squelching unaware objects */
        /* there should be no references to [].squelch outside of squelch.c */
	squelch_kind(o_ptr->k_idx, object_aware_p(o_ptr));

	/* EFGchange bugfix */
	p_ptr->notice |= PN_SQUELCH;
#else
	/* Get object kind */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Set squelch flag */
	k_ptr->squelch = TRUE;
#endif
}
