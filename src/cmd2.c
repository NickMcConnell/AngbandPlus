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


/* Try to bash an altar. */
static bool do_cmd_bash_altar(int y, int x) {

  cave_type       *c_ptr;

  bool more = FALSE;
  int pgod = p_ptr->pgod;
  int agod = cave[y][x].feat-FEAT_ALTAR_HEAD+1;
  int tmp = randint(100);
  c_ptr = &cave[y][x];

  msg_print("You smash into the altar!");

  /* Player doesn't worship anyone -- create an explosion. */
  if (pgod == 0) {
    /* 5% to explode. */
    if (randint(20) == 10) {
      godly_wrath_blast();
    } else {
      more = TRUE;
    }
  } else if (pgod == agod) {
    /* Supreme blasphemy! */
    msg_format("%s thunders: ``Enjoy your eternity in hell, mortal!",
               deity_info[pgod-1].name);
    set_grace(-200000);
    godly_wrath_blast();
  } else {
    /* 50% chance of nothing happening,
     * 40% of explosion,
     * 10% of consecration. */

    if (tmp < 50) {
      more = TRUE;
    }
    else if (tmp < 90) {
      msg_format("%s thunders: ``Know thy place, mortal!''",
                 deity_info[agod-1].name);
      godly_wrath_blast();
      set_grace(p_ptr->grace - 1000);
    } else {
      msg_print("The old altar crumbles into dust, and a new one materializes in its place.");
      msg_format("%s is pleased!",
                 deity_info[pgod-1].name);
      set_grace(p_ptr->grace + 1000);
      great_side_effect();

      /* Forget the altar */
      c_ptr->feat &= ~(CAVE_MARK);
      /* Change it. */
      cave_set_feat(y, x, FEAT_ALTAR_HEAD+(pgod-1));
    }
  }

  return more;
}


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
	bool go_up = FALSE;
	cave_type *c_ptr;
        char i;

#ifdef USE_PYTHON
        if(perform_event(EVENT_GO_UP, Py_BuildValue("(ii)", dun_level, p_ptr->inside_quest))) return;
#endif

	/* Player grid */
	c_ptr = &cave[py][px];

	/* test if on special level */
	if (special_flag)
	{
		prt("Leave this unique level forever (y/n) ? ",0,0);
		flush();
		i=inkey();
		prt("",0,0);
		if (i != 'y') return;
	}

	/* Quest up stairs */
	if (c_ptr->feat == FEAT_QUEST_UP)
	{
		/* Success */
		msg_print("You enter the up staircase.");

		leaving_quest = p_ptr->inside_quest;
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
	/* Normal up stairs */
	else if (c_ptr->feat == FEAT_LESS)
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
	/*
	 * I'm experimenting without this... otherwise the monsters get to
	 * act first when we go up stairs, theoretically resulting in a possible
	 * insta-death.
	 */
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

			if (p_ptr->inside_quest)
			{
				dun_level = 1;
				leaving_quest = p_ptr->inside_quest;
				p_ptr->inside_quest = c_ptr->special;
			}

			/* Create a way back */
			create_down_stair = TRUE;

			/* New depth */
			dun_level--;

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
	bool fall_trap = FALSE;
        char i;

#ifdef USE_PYTHON
        if(perform_event(EVENT_GO_DOWN, Py_BuildValue("(ii)", dun_level, p_ptr->inside_quest))) return;
#endif

	/* Player grid */
	c_ptr = &cave[py][px];

	if (c_ptr->feat == (FEAT_TRAP_HEAD + 0x00)) fall_trap = TRUE;

	/* test if on special level */
	if (special_flag)
	{
		prt("Leave this unique level forever (y/n) ? ",0,0);
		flush();
		i=inkey();
		prt("",0,0);
		if (i != 'y') return;
	}

	/* Quest down stairs */
	if (c_ptr->feat == FEAT_QUEST_DOWN)
	{
		msg_print("You enter the down staircase.");

		leaving_quest = p_ptr->inside_quest;
		p_ptr->inside_quest = c_ptr->special;

		/* Activate the quest */
		if (!quest[p_ptr->inside_quest].status)
		{
			quest[p_ptr->inside_quest].status = 1;
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
	else if ((c_ptr->feat != FEAT_MORE) && !(fall_trap))
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

                        if(c_ptr->special)
                        {
                                dungeon_type = c_ptr->special;
                                dun_level = dungeon_info[dungeon_type].mindepth;
                        }

			/* Leaving */
			p_ptr->leaving = TRUE;

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
			if (!make_object(q_ptr, FALSE, FALSE)) continue;
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


#if defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) /* TNB */

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
		int yy = py + ddy_ddd[d];
		int xx = px + ddx_ddd[d];

		/* Must have knowledge */
		if (!(cave[yy][xx].info & (CAVE_MARK))) continue;
				
		/* Not looking for this feature */
		if (cave[yy][xx].feat < f1) continue;
		if (cave[yy][xx].feat > f2) continue;
			
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
    
    dy = y - py;
    dx = x - px;
    
    /* Paranoia */
    if (ABS(dx) > 1 || ABS(dy) > 1) return (0);
    
    return d[dx + 1][dy + 1];
}

#endif /* defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) -- TNB */


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

#ifdef ALLOW_EASY_OPEN /* TNB */

	/* Option: Pick a direction */
	if (easy_open)
	{
	    int num_doors, num_chests;
	
	    /* Count closed doors (locked or jammed) */
	    num_doors = count_dt(&y, &x, FEAT_DOOR_HEAD, FEAT_DOOR_TAIL);
	    
	    /* Count chests (locked) */
	    num_chests = count_chests(&y, &x, FALSE);
	    
	    /* See if only one target */
	    if (num_doors || num_chests)
		{
	        bool too_many = (num_doors && num_chests) || (num_doors > 1) ||
				(num_chests > 1);
	        if (!too_many) command_dir = coords_to_dir(y, x);
	    }
	}

#endif /* ALLOW_EASY_OPEN -- TNB */

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

#ifdef ALLOW_EASY_OPEN /* TNB */

	/* Option: Pick a direction */
	if (easy_open)
	{
		/* Count open doors */
		if (count_dt(&y, &x, FEAT_OPEN, FEAT_OPEN) == 1)
		{
			command_dir = coords_to_dir(y, x);
		}
	}

#endif /* ALLOW_EASY_OPEN -- TNB */

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
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave[y][x].info & (CAVE_MARK)))
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
 * This will, however, produce grids which are NOT illuminated
 * (or darkened) along with the rest of the room.
 */
static bool twall(int y, int x, byte feat)
{
	cave_type	*c_ptr = &cave[y][x];

	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Forget the wall */
	c_ptr->info &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, feat);

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

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Sound */
	sound(SOUND_DIG);

	/* Titanium */
	if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
	    (c_ptr->feat <= FEAT_PERM_SOLID))
	{
		msg_print("This seems to be permanent rock.");
	}

	/* No tunnelling through mountains */
	else if (c_ptr->feat == FEAT_MOUNTAIN)
	{
		msg_print("You can't tunnel through that!");
	}

	else if (c_ptr->feat == FEAT_TREES) /* -KMW- */
	{
		/* Chop Down */
		if ((p_ptr->skill_dig > 10 + rand_int(400)) && twall(y, x, FEAT_GRASS))
		{
			msg_print("You have cleared away the trees.");
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


	/* Granite */
	else if ((c_ptr->feat >= FEAT_WALL_EXTRA) &&
	         (c_ptr->feat <= FEAT_WALL_SOLID))
	{
		/* Tunnel */
		if ((p_ptr->skill_dig > 40 + rand_int(1600)) && twall(y, x, FEAT_FLOOR))
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
	else if ((c_ptr->feat >= FEAT_MAGMA) &&
	    (c_ptr->feat <= FEAT_QUARTZ_K))
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

	/* Doors */
	else
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
			msg_print("You tunnel into the door.");
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

		/* No tunnelling through doors */
		if (((c_ptr->feat >= FEAT_DOOR_HEAD) && (c_ptr->feat <= FEAT_DOOR_TAIL)) ||
		    ((c_ptr->feat >= FEAT_BLDG_HEAD) && (c_ptr->feat <= FEAT_BLDG_TAIL)) ||
		    ((c_ptr->feat >= FEAT_SHOP_HEAD) && (c_ptr->feat <= FEAT_SHOP_TAIL)))
		{
			/* Message */
			msg_print("You cannot tunnel through doors.");
		}

		/* No tunnelling through air */
		else if (cave_floor_grid(c_ptr) || ((c_ptr->feat >= FEAT_MINOR_GLYPH) &&
		    (c_ptr->feat <= FEAT_PATTERN_XTRA2)))
		{
			/* Message */
			msg_print("You cannot tunnel through air.");
		}

		/* No tunnelling through mountains */
		else if (c_ptr->feat == FEAT_MOUNTAIN)
		{
			msg_print("You can't tunnel through that!");
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


#ifdef ALLOW_EASY_OPEN /* TNB */

/*
 * easy_open_door --
 *
 *	If there is a jammed/closed/locked door at the given location,
 *	then attempt to unlock/open it. Return TRUE if an attempt was
 *	made (successful or not), otherwise return FALSE.
 *
 *	The code here should be nearly identical to that in
 *	do_cmd_open_test() and do_cmd_open_aux().
 */

bool easy_open_door(int y, int x)
{
	int i, j;

	cave_type *c_ptr = &cave[y][x];

	/* Must be a closed door */
	if (!((c_ptr->feat >= FEAT_DOOR_HEAD) &&
	      (c_ptr->feat <= FEAT_DOOR_TAIL)))
	{
		/* Nope */
		return (FALSE);
	}

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
#ifdef ALLOW_EASY_DISARM /* TNB */

bool do_cmd_disarm_aux(int y, int x, int dir)

#else /* ALLOW_EASY_DISARM -- TNB */

static bool do_cmd_disarm_aux(int y, int x, int dir)

#endif /* ALLOW_EASY_DISARM -- TNB */
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
		gain_exp(power);

		/* Forget the trap */
		c_ptr->info &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(y, x, FEAT_FLOOR);

#ifdef ALLOW_EASY_DISARM /* TNB */

		/* Move the player onto the trap */
		move_player(dir, easy_disarm);

#else /* ALLOW_EASY_DISARM -- TNB */

		/* move the player onto the trap grid */
		move_player(dir, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
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

#ifdef ALLOW_EASY_DISARM /* TNB */

		/* Move the player onto the trap */
		move_player(dir, easy_disarm);

#else /* ALLOW_EASY_DISARM -- TNB */

		/* Move the player onto the trap */
		move_player(dir, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
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

#ifdef ALLOW_EASY_DISARM /* TNB */

	/* Option: Pick a direction */
	if (easy_disarm)
	{
	    int num_traps, num_chests;
	
	    /* Count visible traps */
	    num_traps = count_dt(&y, &x, FEAT_TRAP_HEAD, FEAT_TRAP_TAIL);
	    
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

#endif /* ALLOW_EASY_DISARM -- TNB */

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
                if  ((c_ptr->feat < FEAT_DOOR_HEAD ||
                      c_ptr->feat > FEAT_DOOR_TAIL) &&
                      (c_ptr->feat < FEAT_ALTAR_HEAD ||
                       c_ptr->feat > FEAT_ALTAR_TAIL))
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

                else if (c_ptr->feat >= FEAT_ALTAR_HEAD &&
                         c_ptr->feat <= FEAT_ALTAR_TAIL)
                {
                        more = do_cmd_bash_altar(y, x);
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
		else if (((c_ptr->feat >= FEAT_SECRET) &&
		    (c_ptr->feat < FEAT_MINOR_GLYPH)) ||
			((c_ptr->feat == FEAT_TREES) ||
			(c_ptr->feat == FEAT_MOUNTAIN)))
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


void do_cmd_walk_jump(int pickup)
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
 * Support code for the "Walk" and "Jump" commands
 */
void do_cmd_walk(int pickup)
{
        /* Move (usually pickup) */
  
        if (p_ptr->immovable) {
                do_cmd_unwalk();
        } else {
                do_cmd_walk_jump(pickup);
        }
}


void do_cmd_run_run()
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
 * Start running.
 */
void do_cmd_run(void)
{
	if (p_ptr->immovable) {
                do_cmd_immovable_special();
                return;
        }else
                do_cmd_run_run();
}



/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_stay_stay(int pickup)
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

	/* Hack -- enter a building if we are on one -KMW- */
	else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
	    (c_ptr->feat <= FEAT_BLDG_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- enter building */
		command_new = ']';
	}

	/* Exit a quest if reach the quest exit */
	else if (c_ptr->feat == FEAT_QUEST_EXIT)
	{
		int q_index = p_ptr->inside_quest;

		/* Was quest completed? */
		if (quest[q_index].type == 4)
		{
			quest[q_index].status = 2;
			msg_print("You accomplished your quest!");
			msg_print(NULL);
		}
		
		leaving_quest = p_ptr->inside_quest;
		p_ptr->inside_quest = cave[py][px].special;
		dun_level = 0;
		p_ptr->leaving = TRUE;
	}
}

void do_cmd_stay(int pickup)
{
        /* Hold still (usually pickup) */
//        if (p_ptr->immovable) {
//                do_cmd_unwalk();
//        }else{
                do_cmd_stay_stay(pickup);
//        }
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
                case TV_POTION2:
		case TV_BOTTLE:
		case TV_FOOD:
                case TV_FIRESTONE:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
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

#if 0   /* Old code without the quiver slot */
	/* Require proper missile */
	item_tester_tval = p_ptr->tval_ammo;

	/* Get an item */
	q = "Fire which item? ";
	s = "You have nothing to fire.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;


	/* Access the item (if in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}
#else   /* New code with the quiver slot */

        /* Get the "ammo" (if any) */
        o_ptr = &inventory[INVEN_AMMO];

        item = INVEN_AMMO;

        if(!o_ptr->k_idx)
	{
		msg_print("You have nothing to fire with.");
		return;
	}

        if(p_ptr->tval_ammo != o_ptr->tval)
	{
		msg_print("You have nothing to fire with.");
		return;
	}
#endif

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
					if (is_pet(m_ptr))
					{
						char m_name[80];
						monster_desc(m_name, m_ptr, 0);
						msg_format("%s gets angry!", m_name);
						set_pet(m_ptr, FALSE);
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


static int throw_mult = 1;

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
	bool hit_wall = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int msec = delay_factor * delay_factor * delay_factor;

	cptr q, s;


	/* Get an item */
	q = "Throw which item? ";
	s = "You have nothing to throw.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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
	/* Changed for 'launcher' mutation */
	mul = 10 + 2 * (throw_mult - 1);

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10-18 */
	if (tdis > mul) tdis = mul;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d;
	tdam *= throw_mult;

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
		if (!cave_floor_bold(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}

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
					if (is_pet(m_ptr) &&
					    (!(k_info[q_ptr->k_idx].tval == TV_POTION)))
					{
						char m_name[80];
						monster_desc(m_name, m_ptr, 0);
						msg_format("%s gets angry!", m_name);
						set_pet(m_ptr, FALSE);
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
		if ((hit_body) || (hit_wall) || (randint(100) < j))
		{
			/* Message */
			msg_format("The %s shatters!", o_name);

			if (potion_smash_effect(1, y, x, q_ptr->sval))
			{
				if (cave[y][x].m_idx && is_pet(&m_list[cave[y][x].m_idx]))
				{
					char m_name[80];
					monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
					msg_format("%s gets angry!", m_name);
					set_pet(&m_list[cave[y][x].m_idx], FALSE);
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

/*
 * Throw a boomerang object from the equipement(bow).
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
void do_cmd_boomerang(void)
{
        int dir;
        int j, y, x, ny, nx, ty, tx;
	int chance, tdam, tdis;
	int mul, div;
	int cur_dis, visible;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	bool hit_body = FALSE;
	bool hit_wall = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

	int msec = delay_factor * delay_factor * delay_factor;


	/* Get the "bow" (if any) */
        o_ptr = &inventory[INVEN_BOW];


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Single object */
	q_ptr->number = 1;

	/* Description */
	object_desc(o_name, q_ptr, FALSE, 3);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(q_ptr);
	missile_char = object_char(q_ptr);

	/* Extract a "distance multiplier" */
	/* Changed for 'launcher' mutation */
	mul = 10 + 2 * (throw_mult - 1);

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10-18 */
	if (tdis > mul) tdis = mul;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d;
	tdam *= throw_mult;

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
		if (!cave_floor_bold(ny, nx))
		{
			hit_wall = TRUE;
			break;
		}

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
					if (is_pet(m_ptr) &&
					    (!(k_info[q_ptr->k_idx].tval == TV_POTION)))
					{
						char m_name[80];
						monster_desc(m_name, m_ptr, 0);
						msg_format("%s gets angry!", m_name);
						set_pet(m_ptr, FALSE);
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

                                /* Chance of breakage (during attacks) */
                                j = (hit_body ? breakage_chance(o_ptr) : 0);

                                /* Break the boomerang */
                                if (!(o_ptr->art_name || artifact_p(o_ptr)) && (rand_int(100) < j))
                                {
                                        msg_print(format("Your %s is destroyed.",o_name));
                                        inven_item_increase(INVEN_BOW, -1);
                                        inven_item_optimize(INVEN_BOW);
                                }
                        }

			/* Stop looking */
			break;
                }
	}

        /* Travel back to the player */
	for (cur_dis = 0; cur_dis <= tdis; )
	{
		/* Hack -- Stop at the target */
                if ((y == py) && (x == px)) break;

		/* Calculate the new location (see "project()") */
		ny = y;
		nx = x;
                mmove2(&ny, &nx, ty, tx, py, px);

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
	}
}


/* Note: return value indicates that we have succesfully used the power */

bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
	bool use_hp = FALSE;

	/* Not enough mana - use hp */
	if (p_ptr->csp < cost) use_hp = TRUE;

	/* Power is not available yet */
	if (p_ptr->lev < min_level)
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
		if (!(get_check("Really use the power in your weakened state? ")))
		{
			energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->stun)
	{
		difficulty += p_ptr -> stun;
	}
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level)/3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	energy_use = 100;
	if (use_hp)
	{
		take_hit(((cost / 2) + (randint(cost / 2))),
			"concentrating too hard");
	}
	else
	{
		p_ptr->csp -= (cost / 2 ) + (randint(cost / 2));
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
bool racial_aux_tank(s16b min_level, int cost , int use_stat, int difficulty)
{
        if (p_ptr->ctp < cost)
	{
                return FALSE;
	}

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
        p_ptr->ctp -= (cost / 2 ) + (randint(cost / 2));

        p_ptr->redraw |= (PR_TANK);

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
	char ch = 0;
	int amber_power = 0;
	int dir = 0;
	object_type *q_ptr;
	object_type forge;
	int dummy = 0;
	cave_type *c_ptr;
	int y = 0, x = 0;
	int	ii = 0, ij = 0;
        char out_val[80];
        cptr p = "Power of the flame: ";
    

        if((!p_ptr->tim_mimic)&&(!p_ptr->body_monster))
        switch(p_ptr->prace)
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

		case RACE_BARBARIAN:
			if (racial_aux(8, 10, A_WIS, (p_ptr->pclass == CLASS_WARRIOR?6:12)))
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

                case RACE_RKNIGHT:
			/* Select power to use */
			while (TRUE)
			{
                                if (!get_com("Use [F]lash aura or [L]ight speed jump  ?", &ch))
				{
					amber_power = 0;
					break;
				}

                                if (ch == 'F' || ch == 'f')
				{
					amber_power = 1;
					break;
				}

                                if (ch == 'L' || ch == 'l')
				{
					amber_power = 2;
					break;
				}
			}

			if (amber_power == 1)
			{
                                if (racial_aux(1, 9, A_CHR, 7))
                                {
                                        if (!(get_aim_dir(&dir))) break;
                                        msg_print("You flash a bright aura.");
                                        if (p_ptr->lev < 10)
                                                fire_bolt(GF_CONFUSION, dir, plev*2);
                                        else
                                                fire_ball(GF_CONFUSION, dir, plev*2, 2);
                                }
                        }
                        if (amber_power == 2)
			{
                                if (racial_aux(30, 30, A_WIS, 7))
                                {
                                        (void)set_light_speed(p_ptr->lightspeed + 1 + randint(plev/25));
                                }
                        }
			break;


		case RACE_KOBOLD:
			if (racial_aux(12, 8, A_DEX, 14))
			{
				if(!get_aim_dir(&dir)) break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev);
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

                case RACE_DRAGONRIDER:

			/* Select power to use */
			while (TRUE)
			{
                                if (!get_com("Use [F]lame breathing , Flame [D]amage , [G]o between , come [B]ack in town ?", &ch))
				{
					amber_power = 0;
					break;
				}

                                if (ch == 'F' || ch == 'f')
				{
					amber_power = 1;
					break;
				}

                                if (ch == 'G' || ch == 'g')
				{
					amber_power = 2;
					break;
				}

                                if (ch == 'B' || ch == 'b')
				{
                                        amber_power = 3;
					break;
				}

                                if (ch == 'D' || ch == 'd')
				{
                                        amber_power = 4;
					break;
				}

			}

			if (amber_power == 1)
			{
                                if (racial_aux_tank(0, p_ptr->tp_aux1, A_STR, 6))
                                {
                                        if (!get_aim_dir(&dir)) break;
                                        msg_format("You breathe an big flame.");
                                        if(p_ptr->race_extra1==0)
                                                fire_bolt(GF_METEOR, dir, p_ptr->tp_aux1*4);
                                        if(p_ptr->race_extra1==1)
                                                fire_ball(GF_METEOR, dir, p_ptr->tp_aux1*2,((p_ptr->lev)/10) + 1);
                                        if(p_ptr->race_extra1==2)
                                                fire_beam(GF_METEOR, dir, p_ptr->tp_aux1*3);
                                        p_ptr->energy -= 100;
                                }
                        }
                        if (amber_power == 2)
			{
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                                if (racial_aux(3, 4, A_CON, 6)){
                             msg_print("You go between. Show the destination to your Dragon.");
                             if (!tgt_pt(&ii,&ij)) return;
                             p_ptr->energy -= 60 - plev;
                             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                             (distance(ij,ii,py,px) > plev*20 + 2) || !(cave[ij][ii].info & CAVE_MARK))
                             {
                                 msg_print("You fail to show the destination correctly!");
                                 p_ptr->energy -= 100;
                                 teleport_player(10);
                             }
                             else teleport_player_to(ij,ii);
                             }

                        }
                        if (amber_power == 3)
			{
                                if (special_flag)
                                {
                                msg_print("No recall on special levels..");
                                break;
                                }
                                if (racial_aux(7, 11, A_CON, 6)){
                                if(dun_level==0)
                                        msg_print("You are stupid , you are already in town !");
                                else{
                                        msg_print("You go between and show your Dragon the Town");
                                        p_ptr->energy -= 100;
                                        p_ptr->word_recall=1;
                                }
                                }
                        }
                        if (amber_power == 4)
			{
                                /* Ask for power */
                                if (!get_com("Type of the flame , [B]olt ,B[a]ll or B[e]am ?", &ch)) return;
                                if((ch=='B')||(ch=='b'))p_ptr->race_extra1=0;
                                if((ch=='A')||(ch=='a'))p_ptr->race_extra1=1;
                                if((ch=='E')||(ch=='e'))p_ptr->race_extra1=2;

                                sprintf(out_val, "%d", p_ptr->tp_aux1);

                                /* Ask for power */
                                if (!get_string(p, out_val, 4)) return;

                                p_ptr->tp_aux1 = atoi(out_val);                                
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


                case RACE_ENT:
                        if (racial_aux(2, 6, A_CON, 3))
			{
                                msg_print("You made the trees grow!");
                                grow_trees((plev/8<1)?1:plev/8);
			}
			break;

		default:
			msg_print("This race has no bonus power.");
			energy_use = 0;
	}
        else if (p_ptr->mimic_form)
        switch(p_ptr->mimic_form)
        {
                case MIMIC_ENT:
                        if (racial_aux(0, 6, A_CON, 3))
			{
                                msg_print("You made the trees grow!");
                                grow_trees((plev/8<1)?1:plev/8);
			}
			break;
                case MIMIC_MANA_BALL:
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        if (racial_aux(1, (5+(plev/5)), A_INT, 12))
			{
				teleport_player(10 + (plev));
			}
                        break;
                case MIMIC_VAMPIRE:
                                if (racial_aux(1, p_ptr->lev, A_CON, 14))
				{
					if (!get_aim_dir(&dir)) return;
					if (drain_life(dir, (p_ptr->lev * 2)))
						hp_player(p_ptr->lev + randint(p_ptr->lev));
				}
                        break;
                case MIMIC_FIRE_CLOUD:
                                if (racial_aux(1, p_ptr->lev, A_CON, 12))
				{
					msg_print("You breathe fire...");
					if (get_aim_dir(&dir))
						fire_ball(GF_FIRE, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/20));
				}
                        break;
                case MIMIC_COLD_CLOUD:
                                if (racial_aux(1, p_ptr->lev, A_CON, 12))
				{
                                        msg_print("You breathe cold...");
					if (get_aim_dir(&dir))
                                                fire_ball(GF_COLD, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/20));
				}
                        break;
                case MIMIC_CHAOS_CLOUD:
                                if (racial_aux(1, p_ptr->lev, A_CON, 12))
				{
                                        msg_print("You breathe chaos...");
					if (get_aim_dir(&dir))
                                                fire_ball(GF_CHAOS, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/20));
				}
                        break;
                case MIMIC_KOBOLD:
                        if (racial_aux(1, 8, A_DEX, 14))
			{
				if(!get_aim_dir(&dir)) break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev);
			}
			break;
                case MIMIC_DEMON:
                        if (racial_aux(1, 15, A_WIS, 15))
			{
				if (!get_aim_dir(&dir)) break;
				if (p_ptr->lev >= 30)
				{
					msg_print("You cast a ball of fire.");
					fire_ball(GF_FIRE, dir, plev, 2);
				}
				else
				{
					msg_print("You cast a bolt of fire.");
					fire_bolt(GF_FIRE, dir, plev);
				}
			}
			break;
                case MIMIC_DRAGON:
			if (racial_aux(1, p_ptr->lev, A_CON, 12))
			{
				if (!get_aim_dir(&dir)) break;
                                msg_format("You breathe the elements.");
                                fire_ball(GF_MISSILE, dir, (p_ptr->lev)*2,
				    ((p_ptr->lev)/15) + 1);
			}
			break;
                case MIMIC_QUYLTHULG:
                        if (racial_aux(1, 10, A_CHR, 6))
			{
                                do_cmd_beastmaster();
                        }
			break;
                case MIMIC_VALAR:
                        if (racial_aux(1, 30, A_CHR, 6))
			{
                        msg_print("The power of Eru Iluvatar flows trought you!");
                        msg_print("The world change!");
                if (autosave_l)
                {
                    is_autosave = TRUE;
                    msg_print("Autosaving the game...");
                    do_cmd_save_game();
                    is_autosave = FALSE;
                }
			/* Leaving */
			p_ptr->leaving = TRUE;
                        }
			break;
        }

	p_ptr->redraw |= (PR_HP | PR_MANA);
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Allow user to choose a power (racial / mutation / mimic) to activate
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
	monster_type * m_ptr;

	bool has_racial = FALSE;

	cptr racial_power = "(none)";

	cptr q, s;

	for (num = 0; num < 36; num++)
	{
		powers[num] = 0;
		strcpy(power_desc[num], "");
	}

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to use any powers!");
		energy_use = 0;
		return;
	}

        if((!p_ptr->tim_mimic)&&(!p_ptr->body_monster))
	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			if (lvl < 5)
				racial_power = "detect doors+traps (racial, lvl 5, cost 5)";
			else
				racial_power = "detect doors+traps (racial, cost 5, WIS 12@5)";
			has_racial = TRUE;
			break;

		case RACE_NIBELUNG:
			if (lvl < 5)
				racial_power = "detect doors+traps (racial, lvl 5, cost 5)";
			else
				racial_power = "detect doors+traps (racial, cost 5, WIS 10@5)";
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

		case RACE_HALF_OGRE:
			if (lvl < 25)
				racial_power = "explosive rune     (racial, lvl 25, cost 35)";
			else
				racial_power = "explosive rune     (racial, cost 35, INT 15@25)";
			has_racial = TRUE;
			break;

		case RACE_HALF_GIANT:
			if (lvl < 20)
				racial_power = "stone to mud       (racial, lvl 20, cost 10)";
			else
				racial_power = "stone to mud       (racial, cost 10, STR 12@20)";
			has_racial = TRUE;
			break;

		case RACE_SPECTRE:
			if (lvl < 4)
				racial_power = "scare monster      (racial, lvl 4, cost 3)";
			else
				racial_power = "scare monster      (racial, cost 3, INT 3@5)";
			has_racial = TRUE;
			break;

		case RACE_KOBOLD:
			if (lvl < 12)
				racial_power = "poison dart        (racial, lvl 12, cost 8, dam lvl)";
			else
				racial_power = "poison dart        (racial, cost 8, dam lvl, DEX 14@12)";
			has_racial = TRUE;
			break;

		case RACE_DARK_ELF:
			if (lvl < 2)
				racial_power = "magic missile      (racial, lvl 2, cost 2)";
			else
				racial_power = "magic missile      (racial, cost 2, INT 9@2)";
			has_racial = TRUE;
			break;


		case RACE_VAMPIRE:
			if (lvl < 2)
				racial_power = "drain life         (racial, lvl 2, cost 1 + lvl/3) ";
			else
				racial_power = "drain life         (racial, cost 1 + lvl/3, CON 9@2)";
			has_racial = TRUE;
			break;

                case RACE_RKNIGHT:
                        racial_power = "Rohan's Knight's Powers";
			has_racial = TRUE;
			break;
                case RACE_DRAGONRIDER:
                        racial_power = "Dragon's Powers";
			has_racial = TRUE;
			break;
                case RACE_ENT:
			if (lvl < 2)
                                racial_power = "Grow trees (need level 2)";
                        else
                                racial_power = "Grow trees (cost 6)";
			has_racial = TRUE;
			break;
	}
        else
        switch(p_ptr->mimic_form)
        {
                case MIMIC_ENT:
                        racial_power = "Grow trees (cost 6)";
			has_racial = TRUE;
                        break;
                case MIMIC_VAMPIRE:
                        racial_power = "drain life         (mimic, cost 1 + lvl/3, CON 9@2)";
			has_racial = TRUE;
                        break;
                case MIMIC_MANA_BALL:
                        racial_power = "teleport           (mimic, cost 5 + lvl/5, INT 12@5)";
			has_racial = TRUE;
                        break;
                case MIMIC_FIRE_CLOUD:
                        racial_power = "breath fire        (mimic, cost lvl, dam 2*lvl, CON 12@1)";
			has_racial = TRUE;
                        break;
                case MIMIC_COLD_CLOUD:
                        racial_power = "breath cold        (mimic, cost lvl, dam 2*lvl, CON 12@1)";
			has_racial = TRUE;
                        break;
                case MIMIC_CHAOS_CLOUD:
                        racial_power = "breath chaos       (mimic, cost lvl, dam 2*lvl, CON 12@1)";
			has_racial = TRUE;
                        break;
                case MIMIC_GOST:
                        racial_power = "scare monster      (mimic, cost 3, INT 3@5)";
			has_racial = TRUE;
                        break;
                case MIMIC_KOBOLD:
                        racial_power = "poison dart        (mimic, cost 8, dam lvl, DEX 14@12)";
			has_racial = TRUE;
                        break;
                case MIMIC_DRAGON:
                        racial_power = "breath weapon      (mimic, cost lvl, dam 2*lvl, CON 12@1)";
			has_racial = TRUE;
                        break;
                case MIMIC_DEMON:
                        racial_power = "fire bolt/ball(30) (mimic, cost 15, dam lvl, WIS 15@9)";
			has_racial = TRUE;
                        break;
                case MIMIC_QUYLTHULG:
                        racial_power = "summon monster     (mimic, cost 10, CHR 6@5)";
			has_racial = TRUE;
                        break;
                case MIMIC_VALAR:
                        racial_power = "remake the world   (mimic, cost 30, INT 12@5)";
			has_racial = TRUE;
                        break;
        }

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr)) pets++;
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
				strcpy(power_desc[num],"spit acid        (lvl 9, cost 9, dam lvl)");
			else
				strcpy(power_desc[num],"spit acid        (cost 9, dam lvl, DEX 15@9)");
			powers[num++] = MUT1_SPIT_ACID;
		}
		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"fire breath      (lvl 20, cost lvl, dam lvl * 2)");
			else
				strcpy(power_desc[num],"fire breath      (cost lvl, dam lvl * 2, CON 18@20)");
			powers[num++] = MUT1_BR_FIRE;
		}
		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"hypnotic gaze    (lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"hypnotic gaze    (cost 12, CHR 18@12)");
			powers[num++] = MUT1_HYPN_GAZE;
		}
		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
			if (lvl < 9)
				strcpy(power_desc[num],"telekinesis      (lvl 9, cost 9)");
			else
				strcpy(power_desc[num],"telekinesis      (cost 9, WIS 14@9)");
			powers[num++] = MUT1_TELEKINES;
		}
		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"teleport         (lvl 7, cost 7)");
			else
				strcpy(power_desc[num],"teleport         (cost 7, WIS 15@7)");
			powers[num++] = MUT1_VTELEPORT;
		}
		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			if (lvl < 5)
				strcpy(power_desc[num],"mind blast       (lvl 5, cost 3)");
			else
				strcpy(power_desc[num],"mind blast       (cost 3, WIS 15@7)");
			powers[num++] = MUT1_MIND_BLST;
		}
		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			if (lvl < 15)
				strcpy(power_desc[num],"emit radiation   (lvl 15, cost 15)");
			else
				strcpy(power_desc[num],"emit radiation   (cost 15, CON 14@15)");
			powers[num++] = MUT1_RADIATION;
		}
		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			if (lvl < 13)
				strcpy(power_desc[num],"vampiric drain   (lvl 13, cost lvl)");
			else
				strcpy(power_desc[num],"vampiric drain   (cost lvl, CON 14@13)");
			powers[num++] = MUT1_VAMPIRISM;
		}
		if (p_ptr->muta1 & MUT1_SMELL_MET)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"smell metal      (lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"smell metal      (cost 2, INT 12@3)");
			powers[num++] = MUT1_SMELL_MET;
		}
		if (p_ptr->muta1 & MUT1_SMELL_MON)
		{
			if (lvl < 5)
				strcpy(power_desc[num],"smell monsters   (lvl 5, cost 4)");
			else
				strcpy(power_desc[num],"smell monsters   (cost 4, INT 15@5)");
			powers[num++] = MUT1_SMELL_MON;
		}
		if (p_ptr->muta1 & MUT1_BLINK)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"blink            (lvl 3, cost 3)");
			else
				strcpy(power_desc[num],"blink            (cost 3, WIS 12@3)");
			powers[num++] = MUT1_BLINK;
		}
		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"eat rock         (lvl 8, cost 12)");
			else
				strcpy(power_desc[num],"eat rock         (cost 12, CON 18@8)");
			powers[num++] = MUT1_EAT_ROCK;
		}
		if (p_ptr->muta1 & MUT1_SWAP_POS)
		{
			if (lvl < 15)
				strcpy(power_desc[num],"swap position    (lvl 15, cost 12)");
			else
				strcpy(power_desc[num],"swap position    (cost 12, DEX 16@15)");
			powers[num++] = MUT1_SWAP_POS;
		}
		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			if (lvl < 4)
				strcpy(power_desc[num],"shriek           (lvl 4, cost 4)");
			else
				strcpy(power_desc[num],"shriek           (cost 4, CON 6@4)");
			powers[num++] = MUT1_SHRIEK;
		}
		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"illuminate       (lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"illuminate       (cost 2, INT 10@3)");
			powers[num++] = MUT1_ILLUMINE;
		}
		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"detect curses    (lvl 7, cost 14)");
			else
				strcpy(power_desc[num],"detect curses    (cost 14, WIS 14@7)");
			powers[num++] = MUT1_DET_CURSE;
		}
		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"berserk          (lvl 8, cost 8)");
			else
				strcpy(power_desc[num],"berserk          (cost 8, STR 14@8)");
			powers[num++] = MUT1_BERSERK;
		}
		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			if (lvl < 18)
				strcpy(power_desc[num],"polymorph        (lvl 18, cost 20)");
			else
				strcpy(power_desc[num],"polymorph        (cost 20, CON 18@18)");
			powers[num++] = MUT1_POLYMORPH;
		}
		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"midas touch      (lvl 10, cost 5)");
			else
				strcpy(power_desc[num],"midas touch      (cost 5, INT 12@10)");
			powers[num++] = MUT1_MIDAS_TCH;
		}
		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			if (lvl < 1)
				strcpy(power_desc[num],"grow mold        (lvl 1, cost 6)");
			else
				strcpy(power_desc[num],"grow mold        (cost 6, CON 14@1)");
			powers[num++] = MUT1_GROW_MOLD;
		}
		if (p_ptr->muta1 & MUT1_RESIST)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"resist elements  (lvl 10, cost 12)");
			else
				strcpy(power_desc[num],"resist elements  (cost 12, CON 12@10)");
			powers[num++] = MUT1_RESIST;
		}
		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			if (lvl < 12)
				strcpy(power_desc[num],"earthquake       (lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"earthquake       (cost 12, STR 16@12)");
			powers[num++] = MUT1_EARTHQUAKE;
		}
		if (p_ptr->muta1 & MUT1_EAT_MAGIC)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"eat magic        (lvl 17, cost 1)");
			else
				strcpy(power_desc[num],"eat magic        (cost 1, WIS 15@17)");
			powers[num++] = MUT1_EAT_MAGIC;
		}
		if (p_ptr->muta1 & MUT1_WEIGH_MAG)
		{
			if (lvl < 6)
				strcpy(power_desc[num],"weigh magic      (lvl 6, cost 6)");
			else
				strcpy(power_desc[num],"weigh magic      (cost 6, INT 10@6)");
			powers[num++] = MUT1_WEIGH_MAG;
		}
		if (p_ptr->muta1 & MUT1_STERILITY)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"sterilize        (lvl 20, cost 40)");
			else
				strcpy(power_desc[num],"sterilize        (cost 40, CHR 18@20)");
			powers[num++] = MUT1_STERILITY;
		}
		if (p_ptr->muta1 & MUT1_PANIC_HIT)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"panic hit        (lvl 10, cost 12)");
			else
				strcpy(power_desc[num],"panic hit        (cost 12, DEX 14@10)");
			powers[num++] = MUT1_PANIC_HIT;
		}
		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"dazzle           (lvl 7, cost 15)");
			else
				strcpy(power_desc[num],"dazzle           (cost 15, CHR 8@7)");
			powers[num++] = MUT1_DAZZLE;
		}
		if (p_ptr->muta1 & MUT1_LASER_EYE)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"laser eye        (lvl 7, cost 10)");
			else
				strcpy(power_desc[num],"laser eye        (cost 10, WIS 9@7)");
			powers[num++] = MUT1_LASER_EYE;
		}
		if (p_ptr->muta1 & MUT1_RECALL)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"recall           (lvl 17, cost 50)");
			else
				strcpy(power_desc[num],"recall           (cost 50, INT 16@17)");
			powers[num++] = MUT1_RECALL;
		}
		if (p_ptr->muta1 & MUT1_BANISH)
		{
			if (lvl < 25)
				strcpy(power_desc[num],"bansih evil      (lvl 25, cost 25)");
			else
				strcpy(power_desc[num],"banish evil      (cost 25, WIS 18@25)");
			powers[num++] = MUT1_BANISH;
		}
		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			if (lvl < 2)
				strcpy(power_desc[num],"cold touch       (lvl 2, cost 2)");
			else
				strcpy(power_desc[num],"cold touch       (cost 2, CON 11@2)");
			powers[num++] = MUT1_COLD_TOUCH;
		}
		if (p_ptr->muta1 & MUT1_LAUNCHER)
		{
			strcpy(power_desc[num],    "throw object     (cost lev, STR 6@1)");
			/* XXX_XXX_XXX Hack! MUT1_LAUNCHER counts as negative... */
			powers[num++] = 3;
		}
        }

	if (pets > 0)
	{
                strcpy(power_desc[num], "hypnotize a pet");
                powers[num++] = -3;
	}

        strcpy(power_desc[num], "Awake an hypnotized pet");
        powers[num++] = -4;

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
		strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
		strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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

#ifdef USE_PYTHON
        if(perform_event(EVENT_XTRA_POWER, Py_BuildValue("(i)", i))) return;
#endif

	if (powers[i]<0)
	{
		if (powers[i] == -1) cmd_racial_power_aux();
                else if (powers[i] == -3)
                {
                        int dir,x,y;
                        cave_type *c_ptr;
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        object_type     *q_ptr;
                        object_type     forge;

                        msg_print("Hypnotize which pet?");
                        if (!get_rep_dir(&dir)) return;
                        y = py + ddy[dir];
                        x = px + ddx[dir];
                        c_ptr = &cave[y][x];
                        if(c_ptr->m_idx)
                        {
                                m_ptr = &m_list[c_ptr->m_idx];
                                r_ptr = &r_info[m_ptr->r_idx];

                                if(r_ptr->flags1 & RF1_NEVER_MOVE)
                                {
                                        q_ptr=&forge;
                                        object_prep(q_ptr, lookup_kind(TV_HYPNOS, 1));
                                        q_ptr->number = 1;
                                        q_ptr->pval = m_ptr->r_idx;
                                        q_ptr->pval2 = m_ptr->hp;
                                        object_aware(q_ptr);
                                        object_known(q_ptr);

                                        q_ptr->ident |= IDENT_STOREB;
                        
                                        drop_near(q_ptr, 0,y,x);

                                        delete_monster(y,x);
                                        health_who = 0;
                                }
                                else
                                        msg_print("You can only hypnotize the monsters that can't move.");
                        }
                        else msg_print("There is no pet here !");
                }
                else if (powers[i] == -4)
                {
                        monster_type *m_ptr;
                        int m_idx;
                        int item,x,y;
                        object_type *o_ptr;

                        cptr q, s;

                        /* Restrict choices to monsters */
                        item_tester_tval = TV_HYPNOS;

                        /* Get an item */
                        q = "Awake which monster? ";
                        s = "You have no monster to awake.";
                        if (!get_item(&item, q, s, (USE_FLOOR))) return;

                        o_ptr = &o_list[0 - item];

                        x=px;
                        y=py;
                        get_pos_player(100, &y, &x);

                        if((m_idx=place_monster_one_return(y, x, o_ptr->pval, FALSE, TRUE))==0) return;

                        m_ptr = &m_list[m_idx];
                        m_ptr->hp = o_ptr->pval2;

                        floor_item_increase(0 - item, -1);
                        floor_item_describe(0 - item);
                        floor_item_optimize(0 - item);
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
						fire_ball(GF_ACID, dir, p_ptr->lev, 1 + (p_ptr->lev/30));
				}
				break;

			case MUT1_BR_FIRE:
				if (racial_aux(20, p_ptr->lev, A_CON, 18))
				{
					msg_print("You breathe fire...");
					if (get_aim_dir(&dir))
						fire_ball(GF_FIRE, dir, p_ptr->lev * 2, 1 + (p_ptr->lev/20));
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
					msg_print("You concentrate...");
					teleport_player(10 + 4*(p_ptr->lev));
				}
				break;

			case MUT1_MIND_BLST:
				if (racial_aux(5, 3, A_WIS, 15))
				{
					msg_print("You concentrate...");
					if (!get_aim_dir(&dir)) return;
						fire_bolt(GF_PSI, dir, damroll(3 + ((p_ptr->lev - 1) / 5), 3));
				}
				break;

			case MUT1_RADIATION:
				if (racial_aux(15, 15, A_CON, 14))
				{
					msg_print("Radiation flows from your body!");
					fire_ball(GF_NUKE, 0, (p_ptr->lev * 2), 3 + (p_ptr->lev / 20));
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

			case MUT1_SMELL_MET:
				if (racial_aux(3, 2, A_INT, 12))
				{
					(void)detect_treasure();
				}
				break;

			case MUT1_SMELL_MON:
				if (racial_aux(5, 4, A_INT, 15))
				{
					(void)detect_monsters_normal();
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
					int x,y, ox,oy;
					cave_type *c_ptr;
					
					if (!get_rep_dir(&dir)) break;
					y = py + ddy[dir];
					x = px + ddx[dir];
					c_ptr = &cave[y][x];
					if (cave_floor_bold(y,x))
					{
						msg_print("You bite into thin air!");
						break;
					}
					else if (((c_ptr->feat >= FEAT_PERM_EXTRA) &&
						(c_ptr->feat <= FEAT_PERM_SOLID)) ||
						(c_ptr->feat == FEAT_MOUNTAIN))
					{
						msg_print("Ouch!  This wall is harder than your teeth!");
						break;
					}
					else if (c_ptr->m_idx)
					{
						msg_print("There's something in the way!");
						break;
					}
					else if (c_ptr->feat == FEAT_TREES)
					{
						msg_print("You don't like the woody taste!");
						break;
					}
					else
					{
						if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
							(c_ptr->feat <= FEAT_RUBBLE))
						{
							(void)set_food(p_ptr->food + 3000);
						}
						else if ((c_ptr->feat >= FEAT_MAGMA) &&
							(c_ptr->feat <= FEAT_QUARTZ_K))
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

					p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);
					p_ptr->update |= (PU_DISTANCE);
					p_ptr->window |= (PW_OVERHEAD);
				}
				break;

			case MUT1_SWAP_POS:
				if (racial_aux(15, 12, A_DEX, 16))
				{
					if (!get_aim_dir(&dir)) return;
					(void)teleport_swap(dir);
				}
				break;

			case MUT1_SHRIEK:
				if (racial_aux(4, 4, A_CON, 6))
				{
					(void)fire_ball(GF_SOUND, 0, 4 * p_ptr->lev, 8);
					(void)aggravate_monsters(0);
				}
				break;

			case MUT1_ILLUMINE:
				if (racial_aux(3, 2, A_INT, 10))
				{
					(void)lite_area(damroll(2, (p_ptr->lev / 2)), (p_ptr->lev / 10) + 1);
				}
				break;

			case MUT1_DET_CURSE:
				if (racial_aux(7, 14, A_WIS, 14))
				{
					int i;
					
					for (i=0; i < INVEN_TOTAL; i++)
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

			case MUT1_GROW_MOLD:
				if (racial_aux(1, 6, A_CON, 14))
				{
					int i;
					for (i=0; i < 8; i++)
					{
						summon_specific_friendly(py, px, p_ptr->lev, SUMMON_BIZARRE1, FALSE);
					}
				}
				break;

			case MUT1_RESIST:
				if (racial_aux(10, 12, A_CON, 12))
				{
					int num = p_ptr->lev/10;
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
					/* Prevent destruction of quest levels and town */
					if (!is_quest(dun_level) && dun_level)
						earthquake(py, px, 10);
				}
				break;

			case MUT1_EAT_MAGIC:
				if (racial_aux(17, 1, A_WIS, 15))
				{
					object_type * o_ptr;
					int lev, item;

					item_tester_hook = item_tester_hook_recharge;

					/* Get an item */
					q = "Drain which item? ";
					s = "You have nothing to drain.";
					if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) break;

					if (item >= 0)
					{
						o_ptr = &inventory[item];
					}
					else
					{
						o_ptr = &o_list[0 - item];
					}
					
					lev = k_info[o_ptr->k_idx].level;
					
					if (o_ptr->tval == TV_ROD)
					{
						if (o_ptr->pval > 0)
						{
							msg_print("You can't absorb energy from a discharged rod.");
						}
						else
						{
							p_ptr->csp += 2*lev;
							o_ptr->pval = 500;
						}
					}
					else
					{
						if (o_ptr->pval > 0)
						{
							p_ptr->csp += o_ptr->pval * lev;
							o_ptr->pval = 0;
						}
						else
						{
							msg_print("There's no energy there to absorb!");
						}
						o_ptr->ident |= IDENT_EMPTY;
					}
					
					if (p_ptr->csp > p_ptr->msp)
					{
						p_ptr->csp = p_ptr->msp;
					}
					
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);
					p_ptr->window |= (PW_INVEN);
				}
				break;
				
			case MUT1_WEIGH_MAG:
				if (racial_aux(6, 6, A_INT, 10))
				{
					report_magics();
				}
				break;
				
			case MUT1_STERILITY:
				if (racial_aux(20, 40, A_CHR, 18))
				{
					/* Fake a population explosion. */
					msg_print("You suddenly have a headache!");
					take_hit(randint(30) + 30, "the strain of forcing abstinence");
					num_repro += MAX_REPRO;
				}
				break;

			case MUT1_PANIC_HIT:
				if (racial_aux(10, 12, A_DEX, 14))
				{
					int x,y;
					
					if (!get_rep_dir(&dir)) return;
					y = py + ddy[dir];
					x = px + ddx[dir];
					if (cave[y][x].m_idx)
					{
						py_attack(y, x);
						teleport_player(30);
					}
					else
					{
						msg_print("You don't see any monster in this direction");
						msg_print(NULL);
					}
				}
				break;

			case MUT1_DAZZLE:
				if (racial_aux(7, 15, A_CHR, 8))
				{
					stun_monsters(p_ptr->lev * 4);
					confuse_monsters(p_ptr->lev * 4);
					turn_monsters(p_ptr->lev * 4);
				}
				break;

			case MUT1_LASER_EYE:
				if (racial_aux(7, 10, A_WIS, 9))
				{
					if (!get_aim_dir(&dir)) return;
					fire_beam(GF_LITE, dir, 2*p_ptr->lev);
				}
				break;

			case MUT1_RECALL:
				if (racial_aux(17, 50, A_INT, 16))
				{
                                        if (dun_level && (p_ptr->max_dlv[dungeon_type] > dun_level))
					{
						if (get_check("Reset recall depth? "))
                                                        p_ptr->max_dlv[dungeon_type] = dun_level;
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
					if (!(c_ptr->m_idx))
					{
						msg_print("You sense no evil there!");
						break;
					}

					m_ptr = &m_list[c_ptr->m_idx];
					r_ptr = &r_info[m_ptr->r_idx];
					
					if (r_ptr->flags3 & RF3_EVIL)
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
					if (!(c_ptr->m_idx))
					{
						msg_print("You wave your hands in the air.");
						break;
					}
					fire_bolt(GF_COLD, dir, 2 * (p_ptr->lev));
				}
				break;

			/* XXX_XXX_XXX Hack!  MUT1_LAUNCHER is negative, see above */
			case 3: /* MUT1_LAUNCHER */
				if (racial_aux(1, p_ptr->lev, A_STR, 6))
				{
					/* Gives a multiplier of 2 at first, up to 5 at 48th */
					throw_mult = 2 + (p_ptr->lev)/16;
					do_cmd_throw();
					throw_mult = 1;
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

/*
 * Try to ``walk'' using phase door.
 */

void do_cmd_unwalk() {
  int dir, y, x, feat;
  cave_type *c_ptr;

  bool more = FALSE;

  if (!get_rep_dir(&dir)) return;

  y = py + ddy[dir];
  x = px + ddx[dir];

  c_ptr = &cave[y][x];
  feat = c_ptr->feat;

  /* Must have knowledge to know feature XXX XXX */
  if (!(c_ptr->info & (CAVE_MARK))) feat = FEAT_NONE;

  /* Take a turn */
  p_ptr->energy -= 100;


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

  
  /* Attack monsters */
  if (c_ptr->m_idx > 0) {
    /* Attack */
    py_attack(y, x);
  }

  /* Hack -- Ignore weird terrain types. */
  else if (feat >= FEAT_PERM_EXTRA && feat != FEAT_TREES) {
    teleport_player(10);
  }
    
  /* Tunnel through walls */
  else if (feat >= FEAT_SECRET) {
    /* Tunnel */
    more = do_cmd_tunnel_aux(y, x, dir);
  }

  /* Bash jammed doors */
  else if (feat >= FEAT_DOOR_HEAD + 0x08) {
    /* Tunnel */
    more = do_cmd_bash_aux(y, x, dir);
  }
  
  /* Open closed doors */
  else if (feat >= FEAT_DOOR_HEAD) {
    /* Tunnel */
    more = do_cmd_open_aux(y, x, dir);
  }

  /* Disarm traps */
  else if (feat >= FEAT_TRAP_HEAD) {
    /* Tunnel */
    more = do_cmd_disarm_aux(y, x, dir);
  }

  /* Walking semantics */
  else {
    teleport_player_directed(10, dir);
  }

  /* Cancel repetition unless we can continue */
  if (!more) disturb(0, 0);
}

static bool tport_vertically(bool how) {
  if ((p_ptr->inside_arena)||(p_ptr->inside_quest)) { /* arena or quest -KMW- */
    msg_print("There is no effect.");
    return FALSE;
  }

  /* Go down */

  if (how) {
    if (dun_level >= MAX_DEPTH-1) {
      msg_print("The floor is impermeable.");
      return FALSE;
    }

    msg_print("You sink through the floor.");
    dun_level++;
    p_ptr->leaving = TRUE;
  } else {
    if (!dun_level) {
      msg_print("The only thing above you is air.");
      return FALSE;
    }

    msg_print("You rise through the ceiling.");
    dun_level--;
    p_ptr->leaving = TRUE;
  }
  return TRUE;
}


/*
 * Do a special ``movement'' action. Meant to be used for ``immovable''
 * characters.
 */
void do_cmd_immovable_special(void) {
  int i,ii,ij,dir;
  int foo = p_ptr->immov_cntr;
  int lose_sp = 0;
  int lose_hp = 0;
  bool did_act = FALSE;
  bool did_load = FALSE;

  if (foo > 1) {
    if (p_ptr->csp > foo/2) {

      msg_format("This will drain %d mana points!", foo/2);
      if (!get_check("Proceed? "))
	return;

      lose_sp = foo/2;

    } else if (p_ptr->chp > foo/2) {

      msg_format("Warning: This will drain %d hit points!", foo/2);
      if (!get_check("Proceed? "))
	return;

      lose_hp = foo/2;

    } else {
      msg_print("You can't use your powers yet.");
      return;
    }
  }

  /* Enter "icky" mode */
  character_icky = TRUE;

  /* Save the screen */
  Term_save();


  /* Interact until done */
  while (1) {
    /* Clear screen */
    Term_clear();

    /* Ask for a choice */
    prt("Do what special action:", 2, 0);

    /* Give some choices */
    prt("(1) Teleport to a specific place.", 4, 5);
    prt("(2) Fetch an item.", 5, 5);
    prt("(3) Go up 50'", 6, 5);
    prt("(4) Go down 50'", 7, 5);

    /* Prompt */
    prt("Command: ", 9, 0);

    /* Prompt */
    i = inkey();

    /* Done */
    if (i == ESCAPE) break;

    /* Tele-to */
    if (i == '1') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!tgt_pt(&ii,&ij)) break;

      /* Teleport to the target */
      teleport_player_to(ij,ii); 

      did_act = TRUE;
      break;
    }

    /* Fetch item */
    else if (i == '2') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!get_aim_dir(&dir)) return;
      fetch(dir, p_ptr->lev*15, FALSE);

      did_act = TRUE;
      break;
    }

    /* Move up */
    else if (i == '3') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!tport_vertically(FALSE)) return;

      did_act = TRUE;
      break;
    }

    /* Move down */
    else if (i == '4') {
      Term_load();
      character_icky = FALSE;
      did_load = TRUE;

      if (!tport_vertically(TRUE)) return;

      did_act = TRUE;
      break;
    }

    /* Unknown option */
    else {
      bell();
    }

  }

  /* Check if screen was restored before */
  if (!did_load) {
    /* Restore the screen */
    Term_load();

    /* Leave "icky" mode */
    character_icky = FALSE;
  }

  /* Apply stat losses if something was done */
  if (did_act) {
    p_ptr->immov_cntr += 101-(p_ptr->lev*2);

    if (lose_sp) {
      p_ptr->csp -= lose_sp;
      p_ptr->redraw |= (PR_MANA);
    }

    if (lose_hp) {
      p_ptr->chp -= lose_hp;
      p_ptr->redraw |= (PR_HP);
    }
  }
}

static bool item_tester_hook_sacrifice(object_type* o_ptr) {
  if (object_value(o_ptr) * o_ptr->number > 0) return TRUE;

  return FALSE;
}

/*
 * Handle sacrifices.
 * Grace is increased by value of sacrifice.
 */
void do_cmd_sacrifice(void) {

  object_type *o_ptr;

  cptr q, s;

  int val;

  byte on_what = cave[py][px].feat;
  byte what_god;

  int item = 1;

  /* Check valididty */

  if (on_what < FEAT_ALTAR_HEAD || on_what > FEAT_ALTAR_TAIL) {
    show_god_info(FALSE);
    return;
  }

  what_god = on_what-FEAT_ALTAR_HEAD+1;

  item_tester_hook = item_tester_hook_sacrifice;

  /* Get sacrifice */
  q = "Sacrifice what? ";
  s = "You have nothing to sacrifice.";
  if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

  o_ptr = &inventory[item];

  energy_use = 100;

  val = object_value(o_ptr) * o_ptr->number;

  /* Modify grace */

  if (p_ptr->pgod == 0) {
    p_ptr->pgod = what_god;
    set_grace(p_ptr->grace + val);
    p_ptr->god_favor = -60000;

  } else if (p_ptr->pgod != what_god) {
    msg_format("%s thunders in outrage at your blasphemy!",
            deity_info[p_ptr->pgod-1].name);

    set_grace(p_ptr->grace - val*10);
    (void) do_dec_stat(A_WIS);
    p_ptr->update |= PU_BONUS;

    if (val > 2500) {
      msg_format("You feel %s abandon you.",
              deity_info[p_ptr->pgod-1].name);

      p_ptr->pgod = what_god;
      set_grace(val);
      p_ptr->god_favor = -60000;
    }

  } else {
    set_grace(p_ptr->grace + val*5);
  }

        /* Eliminate the item (from the pack) */
        if (item >= 0)
        {
                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
        }
}

s32b rune_combine = 0;

/*
 * Hook to determine if an object is "rune-able"
 */
static bool item_tester_hook_runeable1(object_type *o_ptr)
{
        if(o_ptr->tval != TV_RUNE1) return FALSE;

        /* Assume yes */
        return (TRUE);
}
/*
 * Hook to determine if an object is "rune-able"
 */
static bool item_tester_hook_runeable2(object_type *o_ptr)
{
        if(o_ptr->tval != TV_RUNE2) return FALSE;

        if (rune_combine & (1<<o_ptr->sval)) return (FALSE);

        /* Assume yes */
        return (TRUE);
}

/*
 * Combine the Runes
 */
void do_cmd_rune(void)
{
        int item, dir, power, rune2 = 0, power_rune = 0;
        int chance, minfail, powerdiv;

        int rad = 0, ty = -1, tx = -1, dam = 0, type = 0, flg = 0;

	object_type     *o_ptr;

	cptr q, s;

        bool OK = FALSE;

        rune_combine = 0;        

        /* Require some mana */
        if(!p_ptr->csp)
        {
                msg_print("You have no mana!");
                return;
        }
	
	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}
	
	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

        /* Restrict choices to unused runes */
        item_tester_hook = item_tester_hook_runeable1;

	/* Get an item */
        q = "Use which rune? ";
        s = "You have no rune to use.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
        type = o_ptr->sval;

        do
        {
                /* Restrict choices to unused secondary runes */
                item_tester_hook = item_tester_hook_runeable2;

                OK = !get_item(&item, q, s, (USE_INVEN | USE_FLOOR));

                /* Get the item (in the pack) */
                if (item >= 0)
                {
                        o_ptr = &inventory[item];
                }

                /* Get the item (on the floor) */
                else
                {
                        o_ptr = &o_list[0 - item];
                }
                rune_combine |= 1 << o_ptr->sval;
                rune2 |= 1 << o_ptr->sval;
        }while(!OK);

        if(!rune2)
        {
                msg_print("You have not chose a second rune!");
                return;
        }

        if(rune2 & RUNE_POWER_SURGE)
                power_rune += 3;
        if(rune2 & RUNE_ARMAGEDDON)
                power_rune += 2;
        if(rune2 & RUNE_SPHERE)
                power_rune += 2;
        if(rune2 & RUNE_RAY)
                power_rune += 1;

        power = get_quantity("Which amount of Mana?", p_ptr->csp - (power_rune * (p_ptr->lev / 5)));

        p_ptr->csp -= power + (power_rune * (p_ptr->lev / 5));

        /* Not too weak power */
        power = (power < 10)?10:power;

        /* To reduce the high level powr, while increasing the low levels */
        powerdiv = power / (3 + (p_ptr->lev / 10));

        dam = damroll((powerdiv < 2)?powerdiv:2,power);

        /* Extract the base spell failure rate */
        chance = (10 * power_rune) + (power / 100);

        /* Reduce failure rate by INT/WIS adjustment */
        chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

        /* Extract the minimum failure rate */
        minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

        /* Minimum failure rate */
        if (chance < minfail) chance = minfail;
  
        /* Stunning makes spells harder */
        if (p_ptr->stun > 50) chance += 25;
        else if (p_ptr->stun) chance += 15;

        /* Always a 5 percent chance of working */
        if (chance > 95) chance = 95;

        /* Failure ? */
	if (rand_int(100) < chance)
	{
                char sfail[80];

		if (flush_failure) flush();

                get_rnd_line("sfail.txt",sfail);

                msg_format("A cloud of %s appears above you.", sfail);
		sound(SOUND_FAIL);

                energy_use = 100;

                /* Window stuff */
                p_ptr->window |= (PW_PLAYER);
                p_ptr->redraw |= (PR_MANA);
                return;
	}

        if(rune2 & RUNE_POWER_SURGE)
        {
                flg |= PROJECT_VIEWABLE;
                ty = py;
                tx = px;
        }
        if(rune2 & RUNE_ARMAGEDDON)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_KILL;
                flg |= PROJECT_ITEM;
                flg |= PROJECT_GRID;
                flg |= PROJECT_METEOR_SHOWER;
                rad = (power / 8 == 0)?1:power / 8;
                rad = (rad > 10)?10:rad;
                ty = py;
                tx = px;
        }
        if(rune2 & RUNE_SPHERE)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_KILL;
                flg |= PROJECT_ITEM;
                flg |= PROJECT_GRID;
                rad = (power / 8 == 0)?1:power / 8;
                rad = (rad > 10)?10:rad;
                ty = py;
                tx = px;
        }
        if(rune2 & RUNE_RAY)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_STOP;
                flg |= PROJECT_KILL;
                flg |= PROJECT_BEAM;
                ty = -1;
                tx = -1;
        }
        if(rune2 & RUNE_ARROW)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_STOP;
                flg |= PROJECT_KILL;
                ty = -1;
                tx = -1;
        }
        if(rune2 & RUNE_SELF)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_STOP;
                flg |= PROJECT_KILL;
                ty = py;
                tx = px;
                unsafe = TRUE;
        }

        if((ty == -1)&&(tx == -1))
        {
                if (!get_aim_dir(&dir)) return;

                /* Use the given direction */
                tx = px + ddx[dir];
                ty = py + ddy[dir];

                /* Hack -- Use an actual "target" */
                if ((dir == 5) && target_okay())
                {
                        tx = target_col;
                        ty = target_row;
                }
        }

        if(flg & PROJECT_VIEWABLE)
        {
                project_hack(type, dam);
        }
        else if(flg & PROJECT_METEOR_SHOWER)
        {
                project_meteor(rad, type, dam, flg);
        }
        else project(0, rad, ty, tx, dam, type, flg);

        if(unsafe) unsafe = FALSE;

	/* Take a turn */
	energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);
}

/*
 * Set of variables and functions to create an artifact
 */
typedef struct flag flag;
struct flag {
        int flag;
        int level;
        char desc[50];
};

flag flags1_level[28]=
{
        {TR1_STR,40,"Strength"},
        {TR1_INT,43,"Intelligence"},
        {TR1_WIS,38,"Wisdom"},
        {TR1_DEX,46,"Dexterity"},
        {TR1_CON,42,"Constitution"},
        {TR1_CHR,30,"Charisma"},
        {TR1_STEALTH,32,"Stealth"},
        {TR1_SEARCH,29,"Search"},
        {TR1_INFRA,6,"Infravision"},
        {TR1_TUNNEL,3,"Tunnel"},
        {TR1_SPEED,49,"Speed"},
        {TR1_BLOWS,38,"Blows"},
        {TR1_VAMPIRIC,26,"Vampiric"},
        {TR1_SLAY_ANIMAL,16,"Slay Annimal"},
        {TR1_SLAY_EVIL,25,"Slay Evil"},
        {TR1_SLAY_UNDEAD,30,"Slay Undead"},
        {TR1_SLAY_DEMON,40,"Slay Demon"},
        {TR1_SLAY_ORC,10,"Slay Orc"},
        {TR1_SLAY_TROLL,16,"Slay Troll"},
        {TR1_SLAY_GIANT,25,"Slay Giant"},
        {TR1_SLAY_DRAGON,33,"Slay Dragon"},
        {TR1_KILL_DRAGON,41,"*Slay* Dragon"},
        {TR1_VORPAL,36,"Vorpal"},
        {TR1_BRAND_POIS,3,"Brand Poison"},
        {TR1_BRAND_ACID,12,"Brand Acid"},
        {TR1_BRAND_ELEC,10,"Brand Lightning"},
        {TR1_BRAND_FIRE,6,"Brand Fire"},
        {TR1_BRAND_COLD,8,"Brand Cold"},
};

flag flags2_level[31]=
{
        {TR2_SUST_STR,32,"Sustain Strength"},
        {TR2_SUST_INT,34,"Sustain Intelligence"},
        {TR2_SUST_WIS,28,"Sustain Wisdom"},
        {TR2_SUST_DEX,36,"Sustain Dexterity"},
        {TR2_SUST_CON,36,"Sustain Constitution"},
        {TR2_SUST_CHR,25,"Sustain Charisma"},
        {TR2_INVIS,20,"Invisibility"},
        {TR2_LIFE,50,"Extra Life"},
        {TR2_IM_ACID,49,"Immune Acid"},
        {TR2_IM_ELEC,50,"Immune Ligthning"},
        {TR2_IM_FIRE,49,"Immune Fire"},
        {TR2_IM_COLD,50,"Immune Cold"},
        {TR2_REFLECT,38,"Reflection"},
        {TR2_FREE_ACT,20,"Free Action"},
        {TR2_HOLD_LIFE,30,"Hold Life"},
        {TR2_RES_ACID,12,"Resist Acid"},
        {TR2_RES_ELEC,15,"Resist Lightning"},
        {TR2_RES_FIRE,13,"Resist Fire"},
        {TR2_RES_COLD,14,"Resist Cold"},
        {TR2_RES_POIS,25,"Resist Poison"},
        {TR2_RES_FEAR,26,"Resist Fear"},
        {TR2_RES_LITE,31,"Resist Lite"},
        {TR2_RES_DARK,33,"Resist Darkness"},
        {TR2_RES_BLIND,30,"Resist Blindness"},
        {TR2_RES_CONF,36,"Resist Confusion"},
        {TR2_RES_SOUND,38,"Resist Sound"},
        {TR2_RES_SHARDS,42,"Resist Shards"},
        {TR2_RES_NETHER,39,"Resist Nether"},
        {TR2_RES_NEXUS,46,"Resist Nexus"},
        {TR2_RES_CHAOS,39,"Resist Chaos"},
        {TR2_RES_DISEN,47,"Resist Disenchantment"},
};

flag flags3_level[17]=
{
        {TR3_SH_FIRE,20,"Aura Fire"},
        {TR3_SH_ELEC,25,"Aura Ligthning"},
        {TR3_NO_TELE,29,"Anti Teleportaton"},
        {TR3_NO_MAGIC,34,"Anti Magic"},
        {TR3_WRAITH,50,"Wraith Form"},
        {TR3_TY_CURSE,1,"Ancient Curse"},
        {TR3_FEATHER,15,"Levitation"},
        {TR3_LITE,8,"Lite"},
        {TR3_SEE_INVIS,20,"See Invisible"},
        {TR3_TELEPATHY,36,"Telepathy"},
        {TR3_REGEN,32,"Regeneration"},
        {TR3_DRAIN_EXP,1,"Drain Experience"},
        {TR3_TELEPORT,12,"Teleport"},
        {TR3_AGGRAVATE,1,"Aggravate"},
        {TR3_CURSED,1,"Cursed"},
        {TR3_HEAVY_CURSE,1,"Heavy Cursed"},
        {TR3_PERMA_CURSE,1,"Permanently Cursed"},
};

bool flags1_select[28];
bool flags2_select[31];
bool flags3_select[17];

int show_flags(byte flag)
{
        int max = 0, i, x;

        char ttt[80];

        if (flag == 1) max = 28;
        if (flag == 2) max = 31;
        if (flag == 3) max = 17;

	Term_clear();

        for(i = 0; i < max; i++)
        {
                if(i < 22) x = 5; else x = 45;
                sprintf(ttt, " ");
                if((flag == 1)&&!flags1_select[i]) sprintf(ttt, "%c) %s(level %d)", (i < 26)?I2A(i):('0' + i - 26), flags1_level[i].desc, flags1_level[i].level);
                if((flag == 2)&&!flags2_select[i]) sprintf(ttt, "%c) %s(level %d)", (i < 26)?I2A(i):('0' + i - 26), flags2_level[i].desc, flags2_level[i].level);
                if((flag == 3)&&!flags3_select[i]) sprintf(ttt, "%c) %s(level %d)", (i < 26)?I2A(i):('0' + i - 26), flags3_level[i].desc, flags3_level[i].level);
                if(ttt) prt(ttt, ((i < 22)?i:i - 22) + 2, x);
        }
        return max;
}

void do_cmd_create_artifact()
{
        int max, i = 0, pval = 0;

        u32b cur_cost = 0;

        char out_val[160], ttt[80];

        bool flag = FALSE, okay = FALSE;

        char choice = 0;

        cptr q, s;

        int item;

        object_type forge;
        object_type *q_ptr = &forge;
        
        char o_name[80];

        s32b max_lev = (p_ptr->lev * 5000) + randint(p_ptr->skill_dev * 100) + (p_ptr->skill_dev * 100);

        energy_use = 100;

        /* Restrict choices to artifactable items */
        item_tester_hook = item_tester_hook_artifactable;

	/* Get an item */
        q = "Use which item? ";
        s = "You have nothing to use.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
                q_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
                q_ptr = &o_list[0 - item];
	}

	/* Description */
        object_desc(o_name, q_ptr, FALSE, 0);

        if (q_ptr->name1 || q_ptr->art_name)
	{
		msg_format("The %s %s already %s!",
                    o_name, ((q_ptr->number > 1) ? "are" : "is"),
                    ((q_ptr->number > 1) ? "artifacts" : "an artifact"));
		okay = FALSE;
	}

        else if (q_ptr->name2)
	{
		msg_format("The %s %s already %s!",
                    o_name, ((q_ptr->number > 1) ? "are" : "is"),
                    ((q_ptr->number > 1) ? "ego items" : "an ego item"));
		okay = FALSE;
	}

	else
	{
                if (q_ptr->number > 1)
		{
			msg_print("Not enough enough energy to enchant more than one object!");
                        msg_format("%d of your %s %s destroyed!",(q_ptr->number)-1, o_name, (q_ptr->number>2?"were":"was"));
                        q_ptr->number = 1;
		}
                okay = TRUE;
	}

        if(!okay) return;

        pval = get_quantity("Which bonus(1-5) ?", 5);

        /* Save the screen */
        Term_save();

        for(i = 0; i < 28; i++)
                flags1_select[i] = FALSE;
        for(i = 0; i < 31; i++)
                flags2_select[i] = FALSE;
        for(i = 0; i < 17; i++)
                flags3_select[i] = FALSE;

        /* Chose the flags1 */
        max = show_flags(1);
        prt("Total cost(the higher the cost is, the harder the creation is): 0 gp", 1, 0);

        /* Build a prompt (accept all flags) */
        if (max <= 26)
	{
                /* Build a prompt (accept all flags) */
                strnfmt(out_val, 78, "(Flags %c-%c, ESC=next set of flags) Add which flag to your artifact? ",
                        I2A(0), I2A(max - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Flags %c-%c, ESC=next set of flags) Add which flag to your artifact? ",
                        I2A(0), '0' + max - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		if (isalpha(choice))
		{
			/* Lowercase */
                        if (isupper(choice)) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			i = choice - '0' + 26;
		}

		/* Totally Illegal */
                if ((i < 0) || (i >= max))
		{
			bell();
			continue;
                }
                else if(p_ptr->lev < flags1_level[i].level)
		{
			bell();
			continue;
                }
                else
                {
                        object_type *o_ptr, forge;
                        int j;
                        char ttt[80];

                        flags1_select[i] = TRUE;
                        show_flags(1);

                        o_ptr = &forge;
                        object_wipe(o_ptr);
                        o_ptr->tval = TV_BOTTLE;
                        o_ptr->sval = 1;
                        o_ptr->art_flags1 = 0;
                        o_ptr->art_flags2 = 0;
                        o_ptr->art_flags3 = 0;
                        for(j = 0; j < 28; j++)
                                if(flags1_select[j]) o_ptr->art_flags1 |= flags1_level[j].flag;

                        cur_cost = (flag_cost(o_ptr, pval) < 1)?1:flag_cost(o_ptr, pval);
                        sprintf(ttt, "Total cost(the higher the cost is, the harder the creation is): %ld gp", cur_cost);
                        prt(ttt, 1, 0);
                }

                if(choice == '\e') flag = TRUE;
        }

        /* Chose the flags2 */
        max = show_flags(2);
        sprintf(ttt, "Total cost(the higher the cost is, the harder the creation is): %ld gp", cur_cost);
        prt(ttt, 1, 0);

        /* Build a prompt (accept all flags) */
        if (max <= 26)
	{
                /* Build a prompt (accept all flags) */
                strnfmt(out_val, 78, "(Flags %c-%c, ESC=next set of flags) Add which flag to your artifact? ",
                        I2A(0), I2A(max - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Flags %c-%c, ESC=next set of flags) Add which flag to your artifact? ",
                        I2A(0), '0' + max - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		if (isalpha(choice))
		{
			/* Lowercase */
                        if (isupper(choice)) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			i = choice - '0' + 26;
		}

		/* Totally Illegal */
                if ((i < 0) || (i >= max))
		{
			bell();
			continue;
                }
                else if(p_ptr->lev < flags2_level[i].level)
		{
			bell();
			continue;
                }
                else
                {
                        object_type *o_ptr, forge;
                        int j;
                        char ttt[80];

                        flags2_select[i] = TRUE;
                        show_flags(2);

                        o_ptr = &forge;
                        object_wipe(o_ptr);
                        o_ptr->tval = TV_BOTTLE;
                        o_ptr->sval = 1;
                        o_ptr->art_flags1 = 0;
                        o_ptr->art_flags2 = 0;
                        o_ptr->art_flags3 = 0;
                        for(j = 0; j < 28; j++)
                                if(flags1_select[j]) o_ptr->art_flags1 |= flags1_level[j].flag;
                        for(j = 0; j < 31; j++)
                                if(flags2_select[j]) o_ptr->art_flags2 |= flags2_level[j].flag;

                        cur_cost = (flag_cost(o_ptr, pval) < 1)?1:flag_cost(o_ptr, pval);
                        sprintf(ttt, "Total cost(the higher the cost is, the harder the creation is): %ld gp", cur_cost);
                        prt(ttt, 1, 0);
                }

                if(choice == '\e') flag = TRUE;
        }

        /* Chose the flags3 */
        max = show_flags(3);
        sprintf(ttt, "Total cost(the higher the cost is, the harder the creation is): %ld gp", cur_cost);
        prt(ttt, 1, 0);

        /* Build a prompt (accept all flags) */
        if (max <= 26)
	{
                /* Build a prompt (accept all flags) */
                strnfmt(out_val, 78, "(Flags %c-%c, ESC=create the artifact) Add which flag to your artifact? ",
                        I2A(0), I2A(max - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Flags %c-%c, ESC=create the artifact) Add which flag to your artifact? ",
                        I2A(0), '0' + max - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		if (isalpha(choice))
		{
			/* Lowercase */
                        if (isupper(choice)) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			i = choice - '0' + 26;
		}

		/* Totally Illegal */
                if ((i < 0) || (i >= max))
		{
			bell();
			continue;
                }
                else if(p_ptr->lev < flags3_level[i].level)
		{
			bell();
			continue;
                }
                else
                {
                        object_type *o_ptr, forge;
                        int j;
                        char ttt[80];

                        flags3_select[i] = TRUE;
                        show_flags(3);

                        o_ptr = &forge;
                        object_wipe(o_ptr);
                        o_ptr->tval = TV_BOTTLE;
                        o_ptr->sval = 1;
                        o_ptr->art_flags1 = 0;
                        o_ptr->art_flags2 = 0;
                        o_ptr->art_flags3 = 0;
                        for(j = 0; j < 28; j++)
                                if(flags1_select[j]) o_ptr->art_flags1 |= flags1_level[j].flag;
                        for(j = 0; j < 31; j++)
                                if(flags2_select[j]) o_ptr->art_flags2 |= flags2_level[j].flag;
                        for(j = 0; j < 17; j++)
                                if(flags3_select[j]) o_ptr->art_flags3 |= flags3_level[j].flag;

                        cur_cost = (flag_cost(o_ptr, pval) < 1)?1:flag_cost(o_ptr, pval);
                        sprintf(ttt, "Total cost(the higher the cost is, the harder the creation is): %ld gp", cur_cost);
                        prt(ttt, 1, 0);
                }

                if(choice == '\e') flag = TRUE;
        }

        /* Restore the screen */
        Term_load();

        /* Failure ? */
        if(cur_cost > rand_int(max_lev))
        {
                msg_print("You fail to create the artifact, the powerful force of it are released!");
                msg_print("Your body is altered by them!");
                dec_stat(A_STR, 45, TRUE);
                dec_stat(A_INT, 45, TRUE);
                dec_stat(A_WIS, 45, TRUE);
                dec_stat(A_DEX, 45, TRUE);
                dec_stat(A_CON, 45, TRUE);
                dec_stat(A_CHR, 45, TRUE);

                curse_equipment(98, 70);
                curse_equipment(98, 70);
                curse_equipment(98, 70);
                curse_equipment(98, 70);

                /* Window stuff */
                p_ptr->window |= (PW_INVEN | PW_EQUIP);

                /* Redraw stuff */
                p_ptr->redraw |= PR_STATS;
                return;
        }

        /* Actually create the artifact */
        q_ptr->pval = pval;

        /* Just to be sure */
        q_ptr->art_flags3 |= ( TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
	                       TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

        /* Apply the flags */
        for(i = 0; i < 28; i++)
                if(flags1_select[i]) q_ptr->art_flags1 |= flags1_level[i].flag;
        for(i = 0; i < 31; i++)
                if(flags2_select[i]) q_ptr->art_flags2 |= flags2_level[i].flag;
        for(i = 0; i < 17; i++)
                if(flags3_select[i]) q_ptr->art_flags3 |= flags3_level[i].flag;


	{
		char dummy_name[80];
                char new_name[80];

		strcpy(dummy_name, "");
                identify_fully_aux(q_ptr);
                q_ptr->ident |= IDENT_STOREB; /* This will be used later on... */
		if (!(get_string("What do you want to call the artifact? ", dummy_name, 80)))
                        strcpy(new_name,"of an Alchemist");
		else
		{
			strcpy(new_name,"called '");
			strcat(new_name,dummy_name);
			strcat(new_name,"'");
		}
		/* Identify it fully */
                object_aware(q_ptr);
                object_known(q_ptr);

		/* Mark the item as fully known */
                q_ptr->ident |= (IDENT_MENTAL);

                /* Save the inscription */
                q_ptr->art_name = quark_add(new_name);
	}

        /* Bad side effect */
        curse_equipment(98, 90);
        curse_equipment(98, 90);
        curse_equipment(98, 90);

	/* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
}
