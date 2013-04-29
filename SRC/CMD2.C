/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"

/*
 * Check if action permissable here.
 */

bool do_cmd_test(int y, int x, int action)
{
	u32b bitzero = 0x01;
	u32b flag;

	cptr act;

	cptr here = (((p_ptr->px == x ) && (p_ptr->py == y))?"here":"there");

	feature_type *f_ptr;
	int feat;

	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		msg_format("You see nothing %s.",here);

		/* Nope */
		return (FALSE);
	}

	/* Get memorised feature */
	feat = f_info[cave_feat[y][x]].mimic;

	f_ptr = &f_info[feat];

	act=NULL;

	switch (action)
	{
		case FS_SECRET: break;
		case FS_OPEN: act=" to open"; break;    
		case FS_CLOSE: act=" to close"; break;
		case FS_BASH: act=" to bash"; break;
		case FS_DISARM: act=" to disarm"; break;
		case FS_SPIKE: act=" to spike"; break;
		case FS_ENTER: act=" to enter"; break;
		case FS_TUNNEL: act=" to tunnel"; break;
		case FS_LESS: act=" to climb up"; break;
		case FS_MORE: act=" to climb down"; break;
		case FS_RUN: act=" to run on"; break;
		case FS_KILL_MOVE: act=" to disturb"; break;
		default: break;
	}


	if (action < FS_FLAGS2)
	{
		flag = bitzero << (action - FS_FLAGS1);
		if (!(f_ptr->flags1 & flag))
		{
		 msg_format("You see nothing %s%s.",here,act);
		 return (FALSE);
		}
	}

	else if (action < FS_FLAGS3)
	{       
		flag = bitzero << (action - FS_FLAGS2);
		if (!(f_ptr->flags2 & flag))
		{
		 msg_format("You see nothing %s%s.",here,act);
		 return (FALSE);
		}
	}
	
	else if (action < FS_FLAGS_END)
	{       
		flag = bitzero << (action - FS_FLAGS3);
		if (!(f_ptr->flags2 & flag))
		{
		 msg_format("You see nothing %s%s.",here,act);
		 return (FALSE);
		}
	}

	return (TRUE);  

}




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
		msg_print("I see no up staircase here.");
		return;
	}

	/* Ironman */
	if (adult_ironman)
	{
		msg_print("Nothing happens!");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of up staircases.");

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Verify stairs */
	if (cave_feat[py][px] != FEAT_MORE)
	{
		msg_print("I see no down staircase here.");
		return;
	}

	/* Hack -- take a turn */
	p_ptr->energy_use = 100;

	/* Success */
	message(MSG_STAIRS, 0, "You enter a maze of down staircases.");

	/* Create a way back */
	p_ptr->create_up_stair = TRUE;

	/* New level */
	p_ptr->depth++;

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
			if (!make_gold(i_ptr)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, FALSE, FALSE)) continue;
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
#ifdef ALLOW_OBJECT_INFO
                        /* Always notice */
			if(!(p_ptr->resist_pois)) equip_not_flags(0x0L,TR2_RES_POIS,0x0L);
#endif
		}
		else if (p_ptr->resist_pois)
		{
#ifdef ALLOW_OBJECT_INFO
                        /* Sometimes notice */
			equip_can_flags(0x0L,TR2_RES_POIS,0x0L);
#endif
		}
	}

	/* Paralyze */
	if (trap & (CHEST_PARALYZE))
	{
		msg_print("A puff of yellow gas surrounds you!");
		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + 10 + randint(20));
#ifdef ALLOW_OBJECT_INFO
                        /* Always notice */
			equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
		}
		else
		{
#ifdef ALLOW_OBJECT_INFO
                        /* Always notice */
			equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
		}
	}

	/* Summon monsters */
	if (trap & (CHEST_SUMMON))
	{
		int num = 2 + randint(3);
		msg_print("You are enveloped in a cloud of smoke!");
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
		chest_trap(y, x, o_idx);
	}

	/* Result */
	return (more);
}


#if defined(ALLOW_EASY_OPEN)

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 * ANDY - Counts features that allow action.
 */
static int count_feats(int *y, int *x, int action)
{
	int d, count;

	int feat;

	feature_type *f_ptr;

	u32b flag, bitzero = 0x000000001L;


	/* Count how many matches */
	count = 0;

	/* Check around (and under) the character */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent (legal) location */
		int yy = p_ptr->py + ddy_ddd[d];
		int xx = p_ptr->px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;

		/* Get the feature */
		feat = cave_feat[yy][xx];

		/* Get the mimiced feature */
		feat = f_info[feat].mimic;

		f_ptr = &f_info[feat];

		if (action < FS_FLAGS2)
		{
			flag = bitzero << (action - FS_FLAGS1);
			if (!(f_ptr->flags1 & flag)) continue;                  
		}

                else if (action < FS_FLAGS_END)
		{       
			flag = bitzero << (action - FS_FLAGS2);
			if (!(f_ptr->flags2 & flag)) continue;                  
		}
	
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

#endif /* ALLOW_EASY_OPEN */


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
	if (!do_cmd_test(y, x,FS_OPEN)) return (FALSE);


	/* Verify legality */
	if (!do_cmd_test(y, x, FS_OPEN)) return (FALSE);

	/* Trapped door */
        if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

                /* Update the visuals */
                p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}


	/* Secrets on door/permanent doors */
        else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
		(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
	{

		/* Stuck */
                find_secret(y,x);

                /* Update the visuals */
                p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Locked door */
	else if ((f_info[cave_feat[y][x]].flags1 & (FF1_OPEN)) && (f_info[cave_feat[y][x]].power >0))
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
			message(MSG_OPENDOOR, 0, "You have picked the lock.");

			/* Open the door */
			cave_alter_feat(y, x, FS_OPEN);

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
		cave_alter_feat(y, x, FS_OPEN);

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Open */
	if (easy_open)
	{
		/* Handle a single closed door or locked chest */
                if ((count_feats(&y, &x, FS_OPEN) +
		     count_chests(&y, &x, FALSE)) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);


	/* Verify legality */
	if (!o_idx && !do_cmd_test(y, x,FS_OPEN)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

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
	if (!do_cmd_test(y, x,FS_CLOSE)) return (FALSE);

	/* Trapped door */
        if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Secrets on door/permanent doors */
        else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
		(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
	{
		/* Stuck */
                find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Close door */
	else
	{
		/* Close the door */
		cave_alter_feat(y, x, FS_CLOSE);

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Close */
	if (easy_open)
	{
		/* Handle a single open door */
                if (count_feats(&y, &x, FS_CLOSE) == 1)
		{
			/* Don't close door player is on */
			if ((y != py) || (x != px))
			{
				p_ptr->command_dir = coords_to_dir(y, x);
			}
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_test(y, x, FS_CLOSE)) return;


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
	if (!more) disturb(0, 0);
}






/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Do not use twall anymore --- ANDY
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
	bool more = FALSE;

	int i,j;

	cptr name;

	byte feat;
	
	feat = cave_feat[y][x];

	/* Verify legality */
	if (!do_cmd_test(y, x, FS_TUNNEL)) return (FALSE);

	i = p_ptr->skill_dig;

	j = f_info[cave_feat[y][x]].power;      

	/* Hack - bump up power for doors */
	if (f_info[cave_feat[y][x]].flags1 & (FF1_DOOR)) j = 30;

	/* Trapped door */
        if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Permanent doors/rock */
        else if (f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT))

	{
		/* Stuck */
                find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Dig or tunnel */
	else if (f_info[cave_feat[y][x]].flags2 & (FF2_CAN_DIG))
	{

		/* Dig */
		if (p_ptr->skill_dig > rand_int(200 * j))
		{
			sound(SOUND_DIG);

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* Give the message */                  
			msg_format("You have removed the %s.",name);
			
			cave_alter_feat(y,x,FS_TUNNEL);

                        /* Update the visuals */
                        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Keep trying */
		else
		{

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* We may continue tunelling */
			msg_format("You dig into the %s.",name);
			more = TRUE;
		}

	}

	else
	{

		/* Tunnel -- much harder */
		if (p_ptr->skill_dig > (j + rand_int(400 * j)))
		{
			sound(SOUND_DIG);

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* Give the message */                  
			msg_print("You have finished the tunnel.");
			
			cave_alter_feat(y,x,FS_TUNNEL);

                        /* Update the visuals */
                        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Keep trying */
		else
		{

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Get the name */
			name = (f_name + f_info[feat].name);

			/* We may continue tunelling */
			msg_format("You tunnel into the %s.",name);
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Tunnel */
	if (easy_open)
	{
		/* Handle a single open door */
		if (count_feats(&y, &x, FS_TUNNEL) == 1)
		{
			/* Don't close door player is on */
			if ((y != py) || (x != px))
			{
				p_ptr->command_dir = coords_to_dir(y, x);
			}
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Oops */
	if (!do_cmd_test(y, x, FS_TUNNEL)) return;

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
	if (!more) disturb(0, 0);
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
	if (!do_cmd_test(y, x, FS_DISARM)) return (FALSE);


	/* Get the trap name */
	name = (f_name + f_info[cave_feat[y][x]].name);

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

		/* Remove the trap */
		cave_alter_feat(y, x, FS_DISARM);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

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

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

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

	s16b o_idx;

	bool more = FALSE;

#ifdef ALLOW_EASY_OPEN

	/* Easy Disarm */
	if (easy_open)
	{
		/* Handle a single visible trap or trapped chest */
		if ((count_feats(&y, &x, FS_DISARM) +
		     count_chests(&y, &x, TRUE)) == 1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Check for chests */
	o_idx = chest_check(y, x);


	/* Verify legality */
	if (!o_idx && !do_cmd_test(y, x, FS_DISARM)) return;


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Apply confusion */
	if (confuse_dir(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

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
 * Perform the basic "bash" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x)
{
	int bash, temp;

	int feat;

	cptr name;

	bool more = FALSE;

	/* Verify legality */
	if (!do_cmd_test(y, x,FS_BASH)) return (FALSE);

	/* Get mimiced feature */
	feat = f_info[cave_feat[y][x]].mimic;

	/* Get the name */
	name = (f_name + f_info[feat].name);

	/* Message */
	msg_format("You smash into the %s!",name);

	/* Trapped door */
        if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
	{
		hit_trap(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}


	/* Secrets on door/permanent doors */
        else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
		(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
	{
		/* Stuck */
                find_secret(y,x);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	}

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Extract door power */
        temp = f_info[cave_feat[y][x]].power;

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Message */
		msg_format("The %s crashes open!",name);

		/* Break down the door */
		if (rand_int(100) < 50)
		{
			cave_alter_feat(y, x, FS_BASH);
		}

		/* Open the door */
		else
		{
			cave_alter_feat(y, x, FS_OPEN);
		}

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		/* Sound */
		sound(SOUND_OPENDOOR);

	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
		 p_ptr->lev)
	{
		/* Message */
		msg_format("The %s holds firm.",name);

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

#ifdef ALLOW_EASY_OPEN

	/* Easy Bash */
	if (easy_open)
	{
		/* Handle a single visible trap or trapped chest */
                if (count_feats(&y, &x, FS_BASH)==1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_test(y, x, FS_BASH)) return;


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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir;

	int feat;

	bool more = FALSE;


	/* Get a direction */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


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

	/* Disarm traps */
	else if (f_info[feat].flags1 & (FF1_DISARM))
	{
		/* Tunnel */
		more = do_cmd_disarm_aux(y, x);
	}

	/* Open closed doors */
	else if (f_info[feat].flags1 & (FF1_OPEN))
	{
		/* Tunnel */
		more = do_cmd_open_aux(y, x);
	}
#if 0
	/* Bash jammed doors */
	else if (f_info[feat].flags1 & (FF1_BASH))
	{
		/* Tunnel */
		more = do_cmd_bash_aux(y, x);
	}
#endif
	/* Tunnel through walls */
	else if (f_info[feat].flags1 & (FF1_TUNNEL))
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

#if 0

	/* Close open doors */
	else if (f_info[feat].flags1 & (FF1_CLOSE))
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
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dir, item;


	/* Get a spike */
	if (!get_spike(&item))
	{
		/* Message */
		msg_print("You have no spikes!");

		/* Done */
		return;
	}

#ifdef ALLOW_EASY_OPEN

	/* Easy Bash */
	if (easy_open)
	{
		/* Handle a single visible trap or trapped chest */
                if (count_feats(&y, &x, FS_SPIKE)==1)
		{
			p_ptr->command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN */

	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_test(y, x, FS_SPIKE)) return;


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
		if (!do_cmd_test(y, x, FS_SPIKE)) return;


		/* Trapped door */
                if (f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP))
		{
			hit_trap(y,x);

                        /* Update the visuals */
                        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}


		/* Secrets on door/permanent doors */
                else if ((f_info[cave_feat[y][x]].flags1 & (FF1_SECRET)) ||
			(f_info[cave_feat[y][x]].flags1 & (FF1_PERMANENT)))
		{
			/* Stuck */
                        find_secret(y,x);

                        /* Update the visuals */
                        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}

		/* Spike the door */
		else 
		{
			cave_alter_feat(y,x,FS_SPIKE);

                        /* Update the visuals */
                        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

		}


		/* Use up, and describe, a single spike, from the bottom */
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
}


static bool do_cmd_walk_test(int y, int x)
{
	byte feat;

	cptr name; 

	/* Get feature */
	feat = cave_feat[y][x];

	/* Get mimiced feature */
	feat = f_info[feat].mimic;

	/* Get the name */
	name = (f_name + f_info[feat].name);

	/* Hack -- walking obtains knowledge XXX XXX */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (TRUE);

	/* Hack -- walking allows attacking XXX XXX */
	if (cave_m_idx[y][x] >0) return (TRUE);


	/* Require open space */
	if (!(f_info[cave_feat[y][x]].flags1 & (FF1_MOVE)) ||
		(f_info[cave_feat[y][x]].flags2 & (FF2_FILLED)))
	{
#ifdef ALLOW_EASY_ALTER

		if (easy_alter) {

			if (f_info[feat].flags1 & (FF1_BASH)) return(TRUE);
			if (f_info[feat].flags1 & (FF1_OPEN)) return(TRUE);
		}

#endif /* ALLOW_EASY_ALTER */

		/* Message */
		msg_format("There is a %s in the way.",name);

		/* Nope */
		return (FALSE);

	}

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for the "walk" and "jump" commands.
 */
static void do_cmd_walk_or_jump(int jumping)
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


	/* Take a turn */
	p_ptr->energy_use = 100;

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


/*
 * Walk into a grid.
 */
void do_cmd_walk(void)
{
	/* Move (normal) */
	do_cmd_walk_or_jump(FALSE);
}


/*
 * Jump into a grid.
 */
void do_cmd_jump(void)
{
	/* Move (jump) */
	do_cmd_walk_or_jump(TRUE);
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


	/* Hack XXX XXX XXX */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;

	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Verify legality */
	if (!do_cmd_walk_test(y, x)) return;


	/* Start run */
	run_step(dir);
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

	/* Hack -- enter a store if we are on one */
	if (f_info[cave_feat[py][px]].flags1 & (FF1_ENTER))
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
	if (fresh_before) Term_fresh();
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
		case TV_SPELL:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_SKELETON:
		{
			return (50);
		}

		/* Sometimes break */
		case TV_ARROW:
		{
			return (35);
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
 * Note styles now benefit player.
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

	int dir, item;
	int i, j, y, x, ty, tx;
	int tdam, tdis, thits, tmul;
	int bonus, chance;

        int style_hit=0;
        int style_dam=0;
        int style_crit=0;
	u32b shoot_style;

	object_type *o_ptr;
	object_type *j_ptr;

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
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
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

#ifdef ALLOW_OBJECT_INFO
	/* Check usage */
	object_usage(INVEN_BOW);;
#endif

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

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
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);

	/* Check shooting styles only */
	shoot_style = p_ptr->cur_style & (WS_SHOOT_FLAGS);

	/*** Handle styles ***/
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		/* Check for styles */
                if ((w_info[i].styles==0) || (w_info[i].styles & (shoot_style & (1L << p_ptr->pstyle))))
		{
			switch (w_info[i].benefit)
			{

				case WB_HIT:
                                        style_hit += (p_ptr->lev - w_info[i].level) /2;
					break;

				case WB_DAM:
                                        style_dam += (p_ptr->lev - w_info[i].level) /2;
					break;

				case WB_CRITICAL:
					style_crit++;
					break;
                        }
		}

	}



	/* Use the proper number of shots */
	thits = p_ptr->num_fire;

	/* Base damage from thrown object */
	tdam = damroll(i_ptr->dd, i_ptr->ds);

	/* Actually "fire" the object */
	bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h + style_hit);
	chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

	/* Assume a base multiplier */
	tmul = p_ptr->ammo_mult;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range XXX XXX */
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

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
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
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int chance2 = chance - distance(py, px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize distance travelled) */
			if (test_hit_fire(chance2, r_ptr->ac, m_ptr->ml))
			{

#ifdef ALLOW_OBJECT_INFO
				u32b k1 = i_ptr->i_object.can_flags1;
				u32b k2 = i_ptr->i_object.can_flags2;
				u32b k3 = i_ptr->i_object.can_flags3;

                                u32b n1 = 0x0L;
                                u32b n2 = 0x0L;
                                u32b n3 = 0x0L;
#endif
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
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);

				/* Apply critical damage */
				tdam = critical_shot(i_ptr->weight, (i_ptr->to_h + style_crit *30), tdam);

				/* Apply launcher and missile bonus */
				tdam += i_ptr->to_d + j_ptr->to_d + style_dam;

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

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx,
							       "%^s flees in terror!", m_name);
					}
				}

#ifdef ALLOW_OBJECT_INFO
				/* Check flags */
				n1 = o_ptr->i_object.can_flags1 & ~(k1);
				n2 = o_ptr->i_object.can_flags2 & ~(k2);
				n3 = o_ptr->i_object.can_flags3 & ~(k3);

				/* Update the object */
				update_slot_flags(item,n1,n2,n3);

				/* Check usage */
				object_usage(item);
#endif


			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
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

	int dir, item;
	int i, j, y, x, ty, tx;
	int chance, tdam, tdis;
	int mul, div;

	object_type *o_ptr;

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
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the object */
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


	/* Description */
	object_desc(o_name, i_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(i_ptr);
	missile_char = object_char(i_ptr);


	/* Extract a "distance multiplier" */
	mul = 10;

	/* Enforce a minimum "weight" of one pound */
	div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(i_ptr->dd, i_ptr->ds);

	/* Chance of hitting */
	chance = (p_ptr->skill_tht + (p_ptr->to_h * BTH_PLUS_ADJ));


	/* Take a turn */
	p_ptr->energy_use = 100;


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

	/* Calculate the path */
	path_n = project_path(path_g, tdis, py, px, ty, tx, 0);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Stop before hitting walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		x = nx;
		y = ny;

		/* Only do visuals if the player can "see" the missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Visual effects */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			if (fresh_before) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(y, x);
			if (fresh_before) Term_fresh();
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
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			int chance2 = chance - distance(py, px, y, x);

			int visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit_fire(chance2, r_ptr->ac, m_ptr->ml))
			{


#ifdef ALLOW_OBJECT_INFO
				u32b k1 = i_ptr->i_object.can_flags1;
				u32b k2 = i_ptr->i_object.can_flags2;
				u32b k3 = i_ptr->i_object.can_flags3;

                                u32b n1 = 0x0L;
                                u32b n2 = 0x0L;
                                u32b n3 = 0x0L;
#endif
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
					if (m_ptr->ml) health_track(cave_m_idx[y][x]);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(i_ptr, tdam, m_ptr);

				/* Apply critical damage */
				tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam);

				/* Apply launcher and missile bonus */
				tdam += i_ptr->to_d;

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

					/* Take note */
					if (fear && m_ptr->ml)
					{
						char m_name[80];

						/* Get the monster name (or "it") */
						monster_desc(m_name, m_ptr, 0);

						/* Message */
						message_format(MSG_FLEE, m_ptr->r_idx,
							       "%^s flees in terror!", m_name);
					}
				}


#ifdef ALLOW_OBJECT_INFO
				/* Check flags */
				n1 = o_ptr->i_object.can_flags1 & ~(k1);
				n2 = o_ptr->i_object.can_flags2 & ~(k2);
				n3 = o_ptr->i_object.can_flags3 & ~(k3);

				/* Update the object */
				update_slot_flags(item,n1,n2,n3);

				/* Check usage */
				object_usage(item);
#endif

			}

			/* Stop looking */
			break;
		}
	}

	/* Chance of breakage (during attacks) */
	j = (hit_body ? breakage_chance(i_ptr) : 0);

	/* Drop (or break) near that location */
	drop_near(i_ptr, j, y, x);
}


