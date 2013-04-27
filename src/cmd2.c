/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Andrew Sidwell
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "option.h"
#include "tvalsval.h"

#include "keypad.h"

/**
 * chance to disarm, after difficulty
 */
int
player_type::disarm_skill() const
{
	/* Get the "disarm" factor */
	int i = p_ptr->skills[SKILL_DISARM];

	/* Penalize some conditions */
	if (p_ptr->timed[TMD_BLIND]  || no_lite()) i /= 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i /= 10;

	return i;
}

bool
player_type::disarm_trap(int power) const
{
	int j = disarm_skill()-power;

	/* Always have a small chance of success */
	if (j<2) j = 2;

	/* try it */
	return (rand_int(100) < j);
}


/**
 * Go up one level
 */
void do_cmd_go_up(void)
{
	/* Verify stairs */
	if (cave_feat[p_ptr->loc.y][p_ptr->loc.x] != FEAT_LESS)
	{
		msg_print("I see no up staircase here.");
		return;
	}

	/* Ironman */
	if (OPTION(adult_ironman))
	{
		msg_print("Nothing happens!");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS_UP, 0, "You enter a maze of up staircases.");

	/* Create a way back */
	p_ptr->create_down_stair = TRUE;

	/* New depth */
	p_ptr->depth--;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/**
 * Go down one level
 */
void do_cmd_go_down(void)
{
	/* Verify stairs */
	if (cave_feat[p_ptr->loc.y][p_ptr->loc.x] != FEAT_MORE)
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS_DOWN, 0, "You enter a maze of down staircases.");

	/* Create a way back */
	p_ptr->create_up_stair = TRUE;

	/* New level */
	p_ptr->depth++;

	/* Leaving */
	p_ptr->leaving = TRUE;
}



/**
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


/**
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



/**
 * Determine if a grid contains a chest
 */
static s16b chest_check(coord g)
{
	s16b this_o_idx, next_o_idx = 0;


	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[g.y][g.x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Skip unknown chests XXX XXX */
		/* if (!o_ptr->marked) continue; */

		/* Check for chest */
		if (o_ptr->obj_id.tval == TV_CHEST) return (this_o_idx);
	}

	/* No chest */
	return (0);
}


/**
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
static void chest_death(coord t, s16b o_idx)
{
	object_type *o_ptr = &o_list[o_idx];	/* Get the chest */
	object_type object_type_body;
	object_type *i_ptr = &object_type_body; /* Get local object */
	bool tiny = (o_ptr->obj_id.sval < SV_CHEST_MIN_LARGE);	/* Small chests often hold "gold" */
	int number = (o_ptr->obj_id.sval % SV_CHEST_MIN_LARGE) * 2;	/* Determine how much to drop (see above) */

	/* Zero pval means empty chest */
	if (!o_ptr->pval) number = 0;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Determine the "value" of the items */
	object_level = ABS(o_ptr->pval) + 10;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Wipe the object */
		WIPE(i_ptr);

		/* Small chests often drop gold */
		if (tiny && !one_in_(4))
		{
			/* Make some gold */
			make_gold(i_ptr, SV_CASH);
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, FALSE, FALSE)) continue;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, t);
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


/**
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(coord g, s16b o_idx)
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
		take_hit(NdS(1, 4), "a poison needle");
		(void)do_dec_stat(A_STR);
	}

	/* Lose constitution */
	if (trap & (CHEST_LOSE_CON))
	{
		msg_print("A small needle has pricked you!");
		take_hit(NdS(1, 4), "a poison needle");
		(void)do_dec_stat(A_CON);
	}

	/* Poison */
	if (trap & (CHEST_POISON))
	{
		msg_print("A puff of green gas surrounds you!");
		if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
		{
			(void)p_ptr->inc_timed<TMD_POISONED>(10 + randint(20));
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void)p_ptr->inc_timed<TMD_PARALYZED>(10 + randint(20));
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
			(void)summon_specific(g, p_ptr->depth, 0);
		}
	}

	/* Explode */
	if (trap & (CHEST_EXPLODE))
	{
		msg_print("There is a sudden explosion!");
		msg_print("Everything inside the chest is destroyed!");
		o_ptr->pval = 0;
		take_hit(NdS(5, 8), "an exploding chest");
	}
}


/**
 * Attempt to open the given chest at the given location
 *
 * \pre There is no monster blocking the destination
 *
 * \return TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(coord g, s16b o_idx)
{
	bool flag = TRUE;
	bool more = FALSE;
	object_type *o_ptr = &o_list[o_idx];


	/* Attempt to unlock it */
	if (o_ptr->pval > 0)
	{
		/* Assume locked, and thus not open */
		flag = FALSE;

		/* Success -- May still have traps */
		if (p_ptr->disarm_trap(o_ptr->pval))
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
			if (OPTION(flush_failure)) flush();
			message(MSG_LOCKPICK_FAIL, 0, "You failed to pick the lock.");
		}
	}

	/* Allowed to open */
	if (flag)
	{
		/* Apply chest traps, if any */
		chest_trap(g, o_idx);

		/* Let the Chest drop items */
		chest_death(g, o_idx);
	}

	/* Result */
	return (more);
}


/**
 * Attempt to disarm the chest at the given location
 *
 * \pre There is no monster blocking the destination
 *
 * \return TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(coord g, s16b o_idx)
{
	int i;

	bool more = FALSE;

	object_type *o_ptr = &o_list[o_idx];


	/* Get the "disarm" factor */
	i = p_ptr->disarm_skill();

	/* Must find the trap first. */
	if (!o_ptr->known())
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
	else if (p_ptr->disarm_trap(o_ptr->pval))
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
		if (OPTION(flush_failure)) flush();
		msg_print("You failed to disarm the chest.");
	}

	/* Failure -- Set off the trap */
	else
	{
		msg_print("You set off a trap!");
		chest_trap(g, o_idx);
	}

	/* Result */
	return (more);
}


/**
 * \return TRUE if the given feature is an open door
 */
static bool is_open(byte feat)
{
	return (feat == FEAT_OPEN);
}


/**
 * \return TRUE if the given feature is a closed door
 */
static bool is_closed(byte feat)
{
	return ((feat >= FEAT_DOOR_HEAD) &&
	        (feat <= FEAT_DOOR_TAIL));
}


/**
 * \return the number of doors/traps around (or under) the character.
 */
static int count_feats(coord& g, bool (*test)(byte feat), bool under)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < KEYPAD_DIR_MAX; d++)
	{
		/* Extract adjacent (legal) location */
		yy = p_ptr->loc.y + ddy_ddd[d];
		xx = p_ptr->loc.x + ddx_ddd[d];

		if (	in_bounds_fully(yy, xx)				/* Paranoia */
			&& 	(cave_info[yy][xx] & (CAVE_MARK))	/* Must have knowledge */
			&& 	(*test)(cave_feat[yy][xx]))			/* Looking for this feature */
			{	/* Count it */
			++count;

			/* Remember the location of the last door found */
			g.y = yy;
			g.x = xx;
			}

		/* Count it */
		++count;

		/* Remember the location of the last door found */
		g.y = yy;
		g.x = xx;
	}

	if (under)
	{	/* also look under the player */
		/* Extract adjacent (legal) location */
		yy = p_ptr->loc.y;
		xx = p_ptr->loc.x;

		if (	in_bounds_fully(yy, xx)				/* Paranoia */
			&& 	(cave_info[yy][xx] & (CAVE_MARK))
			&& 	(*test)(cave_feat[yy][xx]))
			{	/* Count it */
			++count;

			/* Remember the location of the last door found */
			g.y = yy;
			g.x = xx;
			}
	}

	/* All done */
	return count;
}


/*
 * \return the number of chests around (or under) the character.
 *
 * If requested, count only trapped chests.
 */
static int count_chests(coord& g, bool trapped)
{
	int d, o_idx;
	object_type *o_ptr;
	int count = 0;	/* Count how many matches */

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		coord t = p_ptr->loc + dd_coord[d];

		/* No (visible) chest is there */
		if ((o_idx = chest_check(t)) == 0) continue;

		/* Grab the object */
		o_ptr = &o_list[o_idx];

		/* Already open */
		if (o_ptr->pval == 0) continue;

		/* No (known) traps here */
		if (trapped &&
		    (!o_ptr->known() ||
		     (o_ptr->pval < 0) ||
		     !chest_traps[o_ptr->pval]))
		{
			continue;
		}

		/* Count it */
		++count;

		/* Remember the location of the last chest found */
		g = t;
	}

	/* All done */
	return count;
}


/**
 * Extract a "direction" which will move one step from the player location
 * towards the given "target" location (or "5" if no motion necessary).
 */
static int coords_to_dir(coord g)
{
	return (motion_dir(p_ptr->loc.y, p_ptr->loc.x, g.y, g.x));
}


/**
 * Determine if a given grid may be "opened"
 */
static bool do_cmd_open_test(coord g)
{
	/* Must have knowledge */
	if (!(cave_info[g.y][g.x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if (!cave_feat_in_range(g.y,g.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL))
	{
		/* Message */
		message(MSG_NOTHING_TO_OPEN, 0, "You see nothing there to open.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/**
 * Perform the basic "open" command on doors
 *
 * \pre There is no monster blocking the destination
 *
 * \return TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(coord g)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_open_test(g)) return (FALSE);


	/* Jammed door */
	if (cave_feat[g.y][g.x] >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked door */
	else if (cave_feat[g.y][g.x] >= FEAT_DOOR_HEAD + 0x01)
	{
		/* Success */
		if (p_ptr->disarm_trap(4*(cave_feat[g.y][g.x] - FEAT_DOOR_HEAD)))
		{
			/* Message */
			message(MSG_LOCKPICK, 0, "You have picked the lock.");

			/* Open the door */
			cave_set_feat(g.y, g.x, FEAT_OPEN);

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			/* Experience */
			gain_exp(1);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (OPTION(flush_failure)) flush();

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
		cave_set_feat(g.y, g.x, FEAT_OPEN);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_OPENDOOR);
	}

	/* Result */
	return (more);
}



/**
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
	coord g;
	int dir;

	s16b o_idx;

	bool more = FALSE;


	/* Easy Open */
	if (OPTION(easy_open))
	{
		int num_doors, num_chests;

		/* Count closed doors */
		num_doors = count_feats(g, is_closed, FALSE);

		/* Count chests (locked) */
		num_chests = count_chests(g, FALSE);

		/* See if only one target */
		if ((num_doors + num_chests) == 1)
		{
			p_ptr->command_dir = coords_to_dir(g);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	g = p_ptr->loc + dd_coord[dir];

	/* Check for chests */
	o_idx = chest_check(g);


	/* Verify legality */
	if (!o_idx && !do_cmd_open_test(g)) return;

	if ( o_idx && o_list[o_idx].pval>0 &&
		(o_list[o_idx].pval & (CHEST_LOSE_STR | CHEST_LOSE_CON | CHEST_POISON | CHEST_PARALYZE | CHEST_SUMMON | CHEST_EXPLODE))
		 && !p_ptr->allow_moron())
		{
		msg_print("You ignore the suggestion to open the undisarmed chest.");
		return;
		}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		g = p_ptr->loc + dd_coord[dir];

		/* Check for chest */
		o_idx = chest_check(g);
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
	if (cave_m_idx[g.y][g.x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(g);
	}

	/* Chest */
	else if (o_idx)
	{
		/* Open the chest */
		more = do_cmd_open_chest(g, o_idx);
	}

	/* Door */
	else
	{
		/* Open the door */
		more = do_cmd_open_aux(g);
	}

	/* Cancel repeat unless we may continue */
	if (!more) disturb(0, 0);
}


/**
 * Determine if a given grid may be "closed"
 */
static bool do_cmd_close_test(coord g)
{
	/* Must have knowledge */
	if (!(cave_info[g.y][g.x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

 	/* Require open/broken door */
	if ((cave_feat[g.y][g.x] != FEAT_OPEN) &&
	    (cave_feat[g.y][g.x] != FEAT_BROKEN))
	{
		/* Message */
		msg_print("You see nothing there to close.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/**
 * Perform the basic "close" command
 *
 * \pre There is no monster blocking the destination
 *
 * \return TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(coord g)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_close_test(g)) return (FALSE);


	/* Broken door */
	if (cave_feat[g.y][g.x] == FEAT_BROKEN)
	{
		/* Message */
		msg_print("The door appears to be broken.");
	}

	/* Open door */
	else
	{
		/* Close the door */
		cave_set_feat(g.y, g.x, FEAT_DOOR_HEAD + 0x00);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(MSG_SHUTDOOR);
	}

	/* Result */
	return (more);
}


/**
 * Close an open door.
 */
void do_cmd_close(void)
{
	coord g;
	int dir;

	bool more = FALSE;


	/* Easy Close */
	if (OPTION(easy_open))
	{
		/* Count open doors */
		if (count_feats(g, is_open, FALSE) == 1)
		{
			p_ptr->command_dir = coords_to_dir(g);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	g = p_ptr->loc + dd_coord[dir];

	/* Verify legality */
	if (!do_cmd_close_test(g)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		g = p_ptr->loc + dd_coord[dir];
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
	if (cave_m_idx[g.y][g.x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(g);
	}

	/* Door */
	else
	{
		/* Close door */
		more = do_cmd_close_aux(g);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}



/**
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(coord g)
{
	/* Must have knowledge */
	if (!(cave_info[g.y][g.x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(g.y, g.x))
	{
		/* Message */
		msg_print("You see nothing there to tunnel.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/**
 * Tunnel through wall.
 *
 * \param g valid location
 *
 * \note It is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * \note Attempting to do so will produce floor grids which are not part
 * of the room, and whose "illumination" status do not change with
 * the rest of the room.
 */
static bool twall(coord g)
{
	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(g.y, g.x)) return (FALSE);

	/* Sound */
	sound(MSG_DIG);

	/* Forget the wall */
	cave_info[g.y][g.x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(g.y, g.x, FEAT_FLOOR);

	/* Update the visuals; fully update the flow  */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS | PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Result */
	return (TRUE);
}


/**
 * Perform the basic "tunnel" command
 *
 * \pre No monster is blocking the destination
 *
 * Uses "twall" (above) to do all "terrain feature changing".
 *
 * \return TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(coord g)
{
	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_tunnel_test(g)) return (FALSE);


	/* Sound XXX XXX XXX */
	/* sound(MSG_DIG); */

	/* Titanium */
	if (cave_feat[g.y][g.x] >= FEAT_PERM_EXTRA)
	{
		msg_print("This seems to be permanent rock.");
	}

	/* Granite */
	else if (cave_feat[g.y][g.x] >= FEAT_WALL_EXTRA)
	{
		if (p_ptr->skills[SKILL_DIGGING]<41 && !p_ptr->allow_moron())
		{	/* subconscious kicks in, stops the zugzwang */
			msg_print("You realize that you cannot tunnel through granite.");
			return (FALSE);
		}
		/* Tunnel */
		else if ((p_ptr->skills[SKILL_DIGGING] > 40 + rand_int(1600)) && twall(g))
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
	else if (cave_feat[g.y][g.x] >= FEAT_MAGMA)
	{
		bool okay = FALSE;
		bool gold = FEAT_MAGMA_H <= cave_feat[g.y][g.x];	/* Found gold */
		bool hard = ((cave_feat[g.y][g.x] - FEAT_MAGMA) & 0x01);	/* Extract "quartz" flag XXX XXX XXX */

		if (hard)
		{	/* Quartz */
			if (p_ptr->skills[SKILL_DIGGING]<21 && !p_ptr->allow_moron())
			{	/* subconscious kicks in, stops the zugzwang */
				msg_print("You realize that you cannot tunnel through the quartz vein.");
				return (FALSE);
			}
			else
			{
				okay = (p_ptr->skills[SKILL_DIGGING] > 20 + rand_int(800));
			}
		}
		else
		{	/* Magma */
			if (p_ptr->skills[SKILL_DIGGING]<11 && !p_ptr->allow_moron())
			{	/* subconscious kicks in, stops the zugzwang */
				msg_print("You realize that you cannot tunnel through the magma vein.");
				return (FALSE);
			}
			else
			{
				okay = (p_ptr->skills[SKILL_DIGGING] > 10 + rand_int(400));
			}
		}

		/* Success */
		if (okay && twall(g))
		{
			/* Found treasure */
			if (gold)
			{
				/* Place some gold */
				place_gold(g.y, g.x);

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
	else if (cave_feat[g.y][g.x] == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->skills[SKILL_DIGGING] > rand_int(200)) && twall(g))
		{
			/* Message */
			msg_print("You have removed the rubble.");

			/* Hack -- place an object */
			if (one_in_(10))
			{
				/* Create a simple object */
				place_object(g.y, g.x, FALSE, FALSE);

				/* Observe new object */
				if (player_can_see_bold(g.y, g.x))
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
	else if (cave_feat[g.y][g.x] >= FEAT_SECRET)
	{
		/* Tunnel */
		if (p_ptr->skills[SKILL_DIGGING]<41 && !p_ptr->allow_moron())
		{	/* subconscious kicks in, stops the zugzwang...or at least thinks it does. */
			msg_print("You realize that you cannot tunnel through granite.");
			return (FALSE);
		}
		else if ((p_ptr->skills[SKILL_DIGGING] > 30 + rand_int(1200)) && twall(g))
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
			if (one_in_(4)) search();
		}
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if (p_ptr->skills[SKILL_DIGGING]<31 && !p_ptr->allow_moron())
		{	/* subconscious kicks in, stops the zugzwang. */
			msg_print("You realize that you cannot tunnel through the door.");
			return (FALSE);
		}
		else if ((p_ptr->skills[SKILL_DIGGING] > 30 + rand_int(1200)) && twall(g))
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

	/* Result */
	return (more);
}


/**
 * Tunnel through "walls" (including rubble and secret doors)
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
	int dir;
	coord t;

	bool more = FALSE;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	t = p_ptr->loc + dd_coord[dir];

	/* Oops */
	if (!do_cmd_tunnel_test(t)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		t = p_ptr->loc + dd_coord[dir];
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
	if (cave_m_idx[t.y][t.x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(t);
	}

	/* Walls */
	else
	{
		/* Tunnel through walls */
		more = do_cmd_tunnel_aux(t);
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0, 0);
}


/**
 * Determine if a given grid may be "disarmed"
 */
static bool do_cmd_disarm_test(coord g)
{
	/* Must have knowledge */
	if (!(cave_info[g.y][g.x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require an actual trap */
	if (!cave_feat_in_range(g.y,g.x,FEAT_TRAP_HEAD,FEAT_TRAP_TAIL))
	{
		/* Message */
		msg_print("You see nothing there to disarm.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/**
 * Perform the basic "disarm" command
 *
 * \pre There is no monster blocking the destination
 *
 * \return TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_aux(coord g)
{
	bool more = FALSE;
	/* Verify legality */
	if (!do_cmd_disarm_test(g)) return (FALSE);
	{	/* start scope block */
	const char* const name = feature_type::f_info[cave_feat[g.y][g.x]].name();	/* Get the trap name */
	int i = p_ptr->disarm_skill(); 	/* Get the "disarm" factor */
	int power = 5;					/* XXX XXX XXX Variable power? */

	/* Success */
	if (p_ptr->disarm_trap(power))
	{
		/* Message */
		message_format(MSG_DISARM, 0, "You have disarmed the %s.", name);

		/* Reward */
		gain_exp(power);

		/* Forget the trap */
		cave_info[g.y][g.x] &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(g.y, g.x, FEAT_FLOOR);
	}

	/* Failure -- Keep trying */
	else if ((i > 5) && (randint(i) > 5))
	{
		/* Failure */
		if (OPTION(flush_failure)) flush();

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
		hit_trap(g);
	}
	}	/* end scope block */
	/* Result */
	return (more);
}


/**
 * Disarms a trap, or a chest
 */
void do_cmd_disarm(void)
{
	coord g;
	int dir;

	s16b o_idx;

	bool more = FALSE;


	/* Easy Disarm */
	if (OPTION(easy_open))
	{
		int num_traps = count_feats(g, is_trap, TRUE);	/* Count visible traps */
		int num_chests = count_chests(g, TRUE);			/* Count chests (trapped) */

		/* See if only one target */
		if (num_traps || num_chests)
		{
			if (num_traps + num_chests <= 1)
				p_ptr->command_dir = coords_to_dir(g);
		}
	}

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	g = p_ptr->loc + dd_coord[dir];

	/* Check for chests */
	o_idx = chest_check(g);


	/* Verify legality */
	if (!o_idx && !do_cmd_disarm_test(g)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		g = p_ptr->loc + dd_coord[dir];

		/* Check for chests */
		o_idx = chest_check(g);
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
	if (cave_m_idx[g.y][g.x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(g);
	}

	/* Chest */
	else if (o_idx)
	{
		/* Disarm the chest */
		more = do_cmd_disarm_chest(g, o_idx);
	}

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_cmd_disarm_aux(g);
	}

	/* Cancel repeat unless told not to */
	if (!more) disturb(0, 0);
}


/**
 * Determine if a given grid may be "bashed"
 */
static bool do_cmd_bash_test(coord g)
{
	/* Must have knowledge */
	if (!(cave_info[g.y][g.x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (!cave_feat_in_range(g.y,g.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL))
	{
		/* Message */
		msg_print("You see nothing there to bash.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/**
 * Perform the basic "bash" command
 *
 * \pre There is no monster blocking the destination
 *
 * \return TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(coord g)
{
	int bash, temp;

	bool more = FALSE;


	/* Verify legality */
	if (!do_cmd_bash_test(g)) return (FALSE);


	/* Message */
	msg_print("You smash into the door!");

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
	temp = ((cave_feat[g.y][g.x] - FEAT_DOOR_HEAD) & 0x07);

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		cave_set_feat(g.y, g.x, (one_in_(2)	? FEAT_BROKEN	/* Break down the door */
											: FEAT_OPEN));	/* Open the door */
		
		/* Message */
		message(MSG_OPENDOOR, 0, "The door crashes open!");

		apply_noise(g, 10);		/* Bashing open doors is noisy */

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
	         p_ptr->lev)
	{
		/* Message */
		msg_print("The door holds firm.");

		apply_noise(g, 2);		/* Failing to bash open doors is noisy */

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		msg_print("You are off-balance.");

		apply_noise(g, 2);		/* Failing to bash open doors is noisy */

		/* Hack -- Lose balance ala paralysis */
		p_ptr->inc_timed<TMD_PARALYZED>(2 + rand_int(2));
	}

	/* Result */
	return (more);
}


/**
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
	int dir;
	coord t;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	t = p_ptr->loc + dd_coord[dir];


	/* Verify legality */
	if (!do_cmd_bash_test(t)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		t = p_ptr->loc + dd_coord[dir];
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
	if (cave_m_idx[t.y][t.x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(t);
	}

	/* Door */
	else
	{
		/* Bash the door */
		if (!do_cmd_bash_aux(t))
		{
			/* Cancel repeat */
			disturb(0, 0);
		}
	}
}



/**
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
	coord g;
	int dir;

	int feat;

	bool more = FALSE;


	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	g.y = p_ptr->loc.y + ddy[dir];
	g.x = p_ptr->loc.x + ddx[dir];


	/* Original feature */
	feat = cave_feat[g.y][g.x];

	/* Must have knowledge to know feature XXX XXX */
	if (!(cave_info[g.y][g.x] & (CAVE_MARK))) feat = FEAT_NONE;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		g.y = p_ptr->loc.y + ddy[dir];
		g.x = p_ptr->loc.x + ddx[dir];
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
	if (cave_m_idx[g.y][g.x] > 0)
	{
		/* Attack */
		py_attack(g);
	}

	/* Tunnel through walls */
	else if (feat >= FEAT_SECRET)
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(g);
	}
#if 0
	/* Bash jammed doors */
	else if (feat >= FEAT_DOOR_HEAD + 0x08)
	{
		/* Tunnel */
		more = do_cmd_bash_aux(g.y, g.x);
	}
#endif /* 0 */
	/* Open closed doors */
	else if (feat >= FEAT_DOOR_HEAD)
	{
		/* Tunnel */
		more = do_cmd_open_aux(g);
	}

	/* Disarm traps */
	else if (feat >= FEAT_TRAP_HEAD)
	{
		/* Tunnel */
		more = do_cmd_disarm_aux(g);
	}

#if 0

	/* Close open doors */
	else if (feat == FEAT_OPEN)
	{
		/* Close */
		more = do_cmd_close_aux(g.y, g.x);
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


/**
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
static bool get_spike(int& ip)
{
	int i;

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Check every item in the pack */
	for (i = 0; i < p_ptr->inven_cnt; ++i)
	{
		const object_type* const o_ptr = &p_ptr->inventory[i];

		/* Check the "tval" code */
		if (o_ptr->obj_id.tval == TV_SPIKE)
		{
			/* Save the spike index */
			ip = i;

			/* Success */
			return TRUE;
		}
	}

	/* Oops */
	return FALSE;
}


/**
 * Determine if a given grid may be "spiked"
 */
static bool do_cmd_spike_test(coord g)
{
	/* Must have knowledge */
	if (!(cave_info[g.y][g.x] & (CAVE_MARK)))
	{
		/* Message */
		msg_print("You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (!cave_feat_in_range(g.y,g.x,FEAT_DOOR_HEAD,FEAT_DOOR_TAIL))
	{
		/* Message */
		msg_print("You see nothing there to spike.");

		/* Nope */
		return (FALSE);
	}

	/* Okay */
	return (TRUE);
}


/**
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int dir, item;
	coord g;


	/* Get a spike */
	if (!get_spike(item))
	{
		/* Message */
		msg_print("You have no spikes!");

		/* Done */
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	g = p_ptr->loc + dd_coord[dir];


	/* Verify legality */
	if (!do_cmd_spike_test(g)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		g = p_ptr->loc + dd_coord[dir];
	}


	/* Monster */
	if (cave_m_idx[g.y][g.x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");

		/* Attack */
		py_attack(g);
	}

	/* Go for it */
	else
	{
		/* Verify legality */
		if (!do_cmd_spike_test(g)) return;

		/* Successful jamming */
		msg_print("You jam the door with a spike.");

		/* Convert "locked" to "stuck" XXX XXX XXX */
		if (cave_feat[g.y][g.x] < FEAT_DOOR_HEAD + 0x08)
		{
			cave_feat[g.y][g.x] += 0x08;
		}

		/* Add one spike to the door */
		if (cave_feat[g.y][g.x] < FEAT_DOOR_TAIL)
		{
			cave_feat[g.y][g.x] += 0x01;
		}

		/* Use up, and describe, a single spike, from the bottom */
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
}



/**
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(int y, int x)
{
	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Allow attack on visible monsters */
	if ((cave_m_idx[y][x] > 0) && (mon_list[cave_m_idx[y][x]].ml))
	{
		return TRUE;
	}

	/* Require open space */
	if (!cave_floor_bold(y, x))
	{
		/* Rubble */
		if (cave_feat[y][x] == FEAT_RUBBLE)
		{
			/* Message */
			message(MSG_HITWALL, 0, "There is a pile of rubble in the way!");
		}

		/* Door */
		else if (cave_feat[y][x] < FEAT_SECRET)
		{
			/* Hack -- Handle "easy_alter" */
			if (OPTION(easy_alter)) return (TRUE);

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


/**
 * Helper function for the "walk" and "jump" commands.
 */
static void do_cmd_walk_or_jump(int jumping)
{
	int y, x, dir;


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = p_ptr->loc.y + ddy[dir];
	x = p_ptr->loc.x + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Confuse direction */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = p_ptr->loc.y + ddy[dir];
		x = p_ptr->loc.x + ddx[dir];
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
	move_player(dir, jumping);
}


/**
 * Walk into a grid.
 */
void do_cmd_walk(void)
{
	/* Move (normal) */
	do_cmd_walk_or_jump(FALSE);
}


/**
 * Jump into a grid.
 */
void do_cmd_jump(void)
{
	/* Move (jump) */
	do_cmd_walk_or_jump(TRUE);
}


/**
 * Start running.
 *
 * \note Running while confused is not allowed.
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
	y = p_ptr->loc.y + ddy[dir];
	x = p_ptr->loc.x + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Start run */
	run_step(dir);
}



/**
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
static void do_cmd_hold_or_stay(int pickup)
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

	/* Spontaneous Searching */
	if (	(p_ptr->skills[SKILL_SEARCH_FREQUENCY] >= 50)
		|| 	one_in_(50 - p_ptr->skills[SKILL_SEARCH_FREQUENCY]))
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

	/* Hack -- enter a store if we are on one */
	if (cave_feat_in_range(p_ptr->loc.y,p_ptr->loc.x,FEAT_SHOP_HEAD,FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter store */
		p_ptr->command_new = '_';

		/* Free turn XXX XXX XXX */
		p_ptr->energy_use = 0;
	}
}


/**
 * Hold still (usually pickup)
 */
void do_cmd_hold(void)
{
	/* Hold still (usually pickup) */
	do_cmd_hold_or_stay(OPTION(always_pickup));
}


/**
 * Stay still (usually do not pickup)
 */
void do_cmd_stay(void)
{
	/* Stay still (usually do not pickup) */
	do_cmd_hold_or_stay(!OPTION(always_pickup));
}


/**
 * Rest (restores hit points and mana and such)
 */
void do_cmd_rest(void)
{
	/* Prompt for time if needed */
	if (p_ptr->command_arg <= 0)
	{
		const char* const p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";

		char out_val[5];

		/* Default */
		strcpy(out_val, "&");

		/* Ask for duration */
		if (!get_string(p, out_val, sizeof(out_val))) return;

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

