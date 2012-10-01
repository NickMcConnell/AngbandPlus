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

void do_cmd_immovable_special(void);

/* Try to bash an altar. */
static bool do_cmd_bash_altar(int y, int x) {

  cave_type       *c_ptr;

  bool more = FALSE;
       int pgod = p_ptr->pgod;
       int agod = cave[y][x].feat-FEAT_ALTAR_HEAD+1;
       int tmp = randint(100);
       monster_race *r_ptr = &r_info[p_ptr->body_monster];

       c_ptr = &cave[y][x];
  
       if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_BASH_DOOR))
       {
               msg_print("You cannot do that.");
        
               return(FALSE);
       }

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
    msg_format("%s thunders: ``Enjoy your eternity in hell, mortal!``",
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
      msg_format("%s thunders: ``Know thy place, mortal!``",
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
        bool go_up = FALSE, go_up_many = FALSE;
        cave_type *c_ptr;
        char i;
        int oldl = dun_level;
  
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
  
                /* Leaving an 'only once' quest marks it as failed */
                if (leaving_quest &&
                        (quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
                        (quest[leaving_quest].status == QUEST_STATUS_TAKEN))
                {
                        quest[leaving_quest].status = QUEST_STATUS_FAILED;
                }
  
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
  
        }
        /* Normal up stairs */
        else if (c_ptr->feat == FEAT_SHAFT_UP)
        {
                if (dun_level == 1)
                {
                        go_up = TRUE;
                }
                else
                {
                        if (confirm_stairs)
                        {
                                if (get_check("Really leave the level? "))
                                        go_up_many = TRUE;
                        }
                        else
                        {
                                go_up_many = TRUE;
                        }
                }
        }
        else
        {
                msg_print("I see no up staircase here.");
                return;
        }
  
        if (go_up || go_up_many)
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
  
                        /* Leaving an 'only once' quest marks it as failed */
                        if (leaving_quest &&
                            (quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
                            (quest[leaving_quest].status == QUEST_STATUS_TAKEN))
                        {
                                quest[leaving_quest].status = QUEST_STATUS_FAILED;
                        }
  
                        p_ptr->inside_quest = c_ptr->special;
                }
  
                /* Create a way back */
                if (go_up_many)
                        create_down_shaft = TRUE;
                else
                create_down_stair = TRUE;
  
                /* New depth */
                if (go_up)
                        dun_level--;
                else
                {
                        dun_level -= randint(3) + 1;
                        if (dun_level <= 0) dun_level = 0;
                }
  
                if(c_ptr->special)
                {
                        dun_level = oldl;
                        dun_level = get_flevel();
                        dungeon_type = c_ptr->special;
                        dun_level += d_info[dungeon_type].mindepth;
                }

                /* Leaving */
                p_ptr->leaving = TRUE;
        }
}

/* Returns TRUE if we are in the Between... */
static bool between_effect(void)
{
        byte bx,by;

        if (cave[py][px].feat == FEAT_BETWEEN)
        {
#if 0 /* The Between is out of the space-time continuum anyway */
                if(p_ptr->resist_continuum) {msg_print("The space-time continuum can't be disrupted."); return TRUE;}
#endif

                bx = cave[py][px].special & 255;
                by = cave[py][px].special >> 8;

                msg_print("You fall in the between.");

                swap_position(by, bx);

                /* To avoid being teleported back */
                energy_use = 0;

                return TRUE;
        }
        else
                return FALSE;
}

/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
        cave_type *c_ptr;
        bool go_down = FALSE, go_down_many = FALSE;
        bool fall_trap = FALSE;
        char i;
        int old_dun = dun_level;
  
#ifdef USE_PYTHON
        if(perform_event(EVENT_GO_DOWN, Py_BuildValue("(ii)", dun_level, p_ptr->inside_quest))) return;
#endif

        /* Between Gates MUST be actived now */
        if (between_effect()) return;
  
        /* Player grid */
        c_ptr = &cave[py][px];
  
	if (c_ptr->t_idx == TRAP_OF_SINKING) fall_trap = TRUE;
  
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
  
                /* Leaving an 'only once' quest marks it as failed */
                if (leaving_quest &&
                        (quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
                        (quest[leaving_quest].status == QUEST_STATUS_TAKEN))
                {
                        quest[leaving_quest].status = QUEST_STATUS_FAILED;
                }
  
  
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
        /* Normal up stairs */
        else if (c_ptr->feat == FEAT_SHAFT_DOWN)
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
                                        go_down_many = TRUE;
                        }
                        else
                        {
                                go_down_many = TRUE;
                        }
                }
        }
        /* Normal stairs */
        else if (c_ptr->feat == FEAT_MORE)
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
        }
   
        else if (!(fall_trap))
        {
                msg_print("I see no down staircase here.");
                return;
        }
  
        if (go_down || go_down_many)
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
                if (go_down)
                        dun_level++;
                else if (go_down_many)
                {
                        int i = randint(3) + 1, j;

                        for(j = 1; j < i; j++)
                        {
                                dun_level++;
                                if(is_quest(dun_level + i - 1)) break;
                        }
                }

                if(c_ptr->special)
                {
                        if(d_info[c_ptr->special].min_plev <= p_ptr->lev)
                        {
                                dungeon_type = c_ptr->special;
                                dun_level = d_info[dungeon_type].mindepth;
                                msg_format("You go into %s", d_text + d_info[dungeon_type].text);
                        }
                        else
                        {
                                msg_print("You don't feel yourself experienced enough to go there...");
                                dun_level = old_dun;
                                return;
                        }
                }
  
                /* Leaving */
                p_ptr->leaving = TRUE;

                if (!fall_trap)
                {
                        /* Create a way back */
                        if (go_down_many)
                                create_up_shaft = TRUE;
                        else
                        create_up_stair = TRUE;
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
        int  trap;

	object_type *o_ptr = &o_list[o_idx];

	bool ident = FALSE;

	/* Ignore disarmed chests */
	if (o_ptr->pval <= 0) return;

	/* Obtain the trap */
	trap = o_ptr->pval;
	
	/* Message */
	msg_print("You found a trap!");
	
	/* Set off trap */
	ident = player_activate_trap_type(y, x, o_ptr, o_idx);
	if (ident)
	{
		t_info[o_ptr->pval].ident = TRUE;
		msg_format("You identified the trap as %s.",
			   t_name + t_info[trap].name);
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
                i = p_ptr->stat_ind[A_DEX];

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
 * Return the number of traps around (or under) the character.
 */
static int count_traps(int *y, int *x)
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
		if (cave[yy][xx].t_idx == 0) continue;
			
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
			!o_ptr->pval)) continue;

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
                i = p_ptr->stat_ind[A_DEX];

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
		
			/* Set off trap */
			if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

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
		/* Set off trap */
		if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

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
                        py_attack(y, x, -1);
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

        monster_race *r_ptr = &r_info[p_ptr->body_monster];
   
        if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_OPEN_DOOR))
        {
                msg_print("You cannot close doors.");
         
                return(FALSE);
        }

	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Set off trap */
	if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

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
                        py_attack(y, x, -1);
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
        /* Must have knowledge(execpt on "forget" levels) */
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
bool do_cmd_tunnel_aux(int y, int x, int dir)
{
	cave_type *c_ptr;

	bool more = FALSE;
       /* This rule was stupid, so it has been removed. */
        /* Must be have something to dig with */
        /*if(!inventory[INVEN_TOOL].k_idx || (inventory[INVEN_TOOL].tval != TV_DIGGING))
       * {
       *         msg_print("You have to use a shovel or a pick in your tool slot.");
       *
       *         return FALSE;
       * }
       */

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Sound */
	sound(SOUND_DIG);

	/* Titanium */
        if (f_info[c_ptr->feat].flags1 & FF1_PERMANENT)
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
                if ((randint(digging_ability()) > rand_int(200)) && twall(y, x, FEAT_GRASS))
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
                if ((randint(digging_ability()) > rand_int(4000)) && twall(y, x, FEAT_FLOOR))
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
                        okay = (randint(digging_ability()) > rand_int(600));
		}

		/* Magma */
		else
		{
                        okay = (randint(digging_ability()) > rand_int(450));
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
                if ((randint(digging_ability()) > rand_int(100)) && twall(y, x, d_info[dungeon_type].floor1))
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
                if ((randint(digging_ability()) > rand_int(600)) && twall(y, x, FEAT_FLOOR))
		{
			msg_print("You have finished the tunnel.");

			/* Set off trap */
			if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);
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
                if ((randint(digging_ability()) > rand_int(500)) && twall(y, x, FEAT_FLOOR))
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
                        py_attack(y, x, -1);
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
                i = p_ptr->stat_ind[A_DEX];

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

			/* Set off trap */
			if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

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
		/* Set off trap */
		if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

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
	trap_type *t_ptr;

        t_ptr = &t_info[o_ptr->pval];

	/* Take a turn */
	energy_use = 100;

	/* Get the "disarm" factor */
        i = p_ptr->stat_ind[A_DEX];

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Extract the difficulty */
	j = i - t_ptr->difficulty * 3;

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

	/* Success (get a lot of experience) */
	else if (rand_int(100) < j)
	{
		msg_print("You have disarmed the chest.");
		gain_exp(t_ptr->difficulty * 3);
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
	if (t_info[c_ptr->t_idx].ident)
		name = (t_name + t_info[c_ptr->t_idx].name);
	else
		name = "unknown trap";

	/* Get the "disarm" factor */
        i = p_ptr->stat_ind[A_DEX];

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* XXX XXX XXX Variable power? */

	/* Extract trap "power" */
	power = t_info[c_ptr->t_idx].difficulty;

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
		c_ptr->t_idx = 0;

#ifdef ALLOW_EASY_DISARM /* TNB */

		/* Move the player onto the trap */
		if (!(f_info[c_ptr->feat].flags1 & FF1_DOOR))
                        move_player(dir, easy_disarm);

#else /* ALLOW_EASY_DISARM -- TNB */

		/* move the player onto the trap grid */
		if (!(f_info[c_ptr->feat].flags1 & FF1_DOOR))
                        move_player(dir, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */

		/* Remove trap attr from grid */
		note_spot(y, x);
		lite_spot(y, x);
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
		if (!(f_info[c_ptr->feat].flags1 & FF1_DOOR))
		move_player(dir, easy_disarm);

#else /* ALLOW_EASY_DISARM -- TNB */

		/* Move the player onto the trap */
		if (!(f_info[c_ptr->feat].flags1 & FF1_DOOR))
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
	    num_traps = count_traps(&y, &x);
	    
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
		if ((c_ptr->t_idx == 0) &&
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
                        py_attack(y, x, -1);
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

        monster_race *r_ptr = &r_info[p_ptr->body_monster];
   
        if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_BASH_DOOR))
        {
                msg_print("You cannot do that.");
         
                return(FALSE);
        }

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
			/* Set off trap */
			if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

			cave_set_feat(y, x, FEAT_BROKEN);
		}

		/* Open the door */
		else
		{
			/* Set off trap */
			if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

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

        monster_race *r_ptr = &r_info[p_ptr->body_monster];
   
        if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_BASH_DOOR))
        {
                msg_print("You cannot do that.");
         
                return;
        }
 
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
                        py_attack(y, x, -1);
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
                        py_attack(y, x, -1);
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
		else if (c_ptr->t_idx != 0)
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
                        py_attack(y, x, -1);
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

/* Know about your Dark Aura(Dark Lords) */
void do_cmd_dark_aura_info(void)
{
     if (p_ptr->pclass == CLASS_DARK_LORD)
     {
     msg_format("Your aura can inflict maximal damages of %d on a radius of %d", 5 * p_ptr->lev * 2, p_ptr->lev /8 + 3);
     }
     else if (p_ptr->pclass == CLASS_VALKYRIE)
     {
     msg_format("Your aura can inflict maximal damages of %d on a radius of %d", 5 * p_ptr->lev, 2);
     }
     else msg_print("This command is reserved to Dark Lords and Valkyries!");
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
                if (p_ptr->body_monster == 1101)
                {
                        msg_print("You can't move while in rock shape!");
                }
                else move_player(dir, pickup);

                /* Dark Lords generate Darkness Aura which can hurt monsters! */
                if (p_ptr->pclass == CLASS_DARK_LORD) dark_lord_aura(rand_int(5) * p_ptr->lev * 2, p_ptr->lev /8 + 3);
                /* Valkyries generate Thunder Aura(but the truth is that is use GF_LITE, not GF_ELEC!) */
                /* which can hurt monsters! */
                if (p_ptr->pclass == CLASS_VALKYRIE) valkyrie_aura(rand_int(5) * p_ptr->lev, 2);

                /* The Paladin's Aura Of Life! :) */
                /*if (p_ptr->abilities[(CLASS_PALADIN * 10) + 2] >= 1) aura_of_life();*/

                /* Ranger's Wilderness Lore! */
                if (p_ptr->abilities[(CLASS_RANGER * 10)] >= 1) wilderness_lore();

                /* Elemental Lord's aura! */
                /*if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1) elem_lord_aura((p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] * 5) * p_ptr->lev, 2 + (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] / 30));*/

                /* Justice Warrior's Aura Of Evil Repulsing! */
                /*if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 2] >= 1) aura_repulse_evil(2 + (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 2] / 20));*/

                /* Wearing the Amulet Of Flooding? */
                do_cmd_check_flooding();

		/* Allow more walking */
		more = TRUE;
	}

        /* Hack -- In small scale wilderness it takes MUCH more time to move */
        energy_use *= (p_ptr->wild_mode)?((MAX_HGT + MAX_WID) / 2):1;

        /* Hack again -- Is there a special encounter ??? */
        if(p_ptr->wild_mode && magik(wf_info[wild_map[p_ptr->wilderness_y][p_ptr->wilderness_x].feat].level - (p_ptr->lev * 2 / 3)))
        {
                /* Go into large wilderness view */
                p_ptr->wilderness_x = px;
                p_ptr->wilderness_y = py;
                energy_use = 100;
                change_wild_mode();

                /* HACk -- set the encouter flag for the wilderness generation */
                generate_encounter = TRUE;

                /* Inform the player of his horrible fate :=) */
                msg_print("You are ambushed !");
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
        if (p_ptr->body_monster == 1101) return;
       
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
        p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Start running.
 */
void do_cmd_run(void)
{
	if (p_ptr->immovable) {
                return;
        }else
                do_cmd_run_run();
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
        if (randint(100) < ((p_ptr->stat_ind[A_INT] / 5) + (p_ptr->stat_ind[A_DEX] / 5)))
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

		/* Leaving an 'only once' quest marks it as failed */
		if (leaving_quest &&
			(quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
			(quest[leaving_quest].status == QUEST_STATUS_TAKEN))
		{
			quest[leaving_quest].status = QUEST_STATUS_FAILED;
		}

		p_ptr->inside_quest = cave[py][px].special;
		dun_level = 0;
		p_ptr->leaving = TRUE;
	}
}

/*
 * Resting allows a player to safely restore his hp	-RAK-
 */
void do_cmd_rest(void)
{
        /* Can't rest on a Between Gate -- too dangerous */
        if (cave[py][px].feat == FEAT_BETWEEN)
        {
                msg_print("Resting on a between gate is too dangerous!");
                return;
        }

        /* Can't rest while undead, it would mean dying */
        if (p_ptr->class_extra6 & CLASS_UNDEAD)
        {
                msg_print("Resting is impossible while undead!");
                return;
        }

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
        int reducer = 1 + ((p_ptr->pclass == CLASS_ARCHER)?(p_ptr->lev / 10):0);

	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
                case TV_POTION2:
		case TV_BOTTLE:
		case TV_FOOD:
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

		case TV_ARROW:
		{
                        return (50 / reducer);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SPIKE:
		{
			return (25);
		}

		case TV_SHOT:
		case TV_BOLT:
		{
                        return (25 / reducer);
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
void do_cmd_fire(int calledshot, bool multishot)
{
	int dir, item;
        int j, y, x, ny, nx, ty, tx, by, bx;
        int tdis, thits, tmul;
        s32b tdam;
	int cur_dis, visible;
        int breakage = -1, num_ricochet = 0;
        int hitbonus = 0;
        int hitpenality = 0;
        int ammoqty = 1;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;
	object_type *j_ptr;

	bool hit_body = FALSE;

	byte missile_attr;
	char missile_char;

	char o_name[80];

        cptr q, s;

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

        /* If nothing correct try to choose from the backpack */
        if ((p_ptr->tval_ammo != o_ptr->tval) || (!o_ptr->k_idx))
	{
		msg_print("You have nothing to fire with.");

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
	}
#endif

	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Single object */
        if (p_ptr->abilities[(CLASS_ARCHER * 10) + 6] >= 1 && o_ptr->tval == TV_ARROW && multishot)
        {
                char tmpstring[80];
                int maxarrows = (p_ptr->abilities[(CLASS_ARCHER * 10) + 6] / 5) + 2;
                if (maxarrows > o_ptr->number) maxarrows = o_ptr->number;
                sprintf(tmpstring, "Shoot how many arrows?(Max: %d) ", maxarrows);
                ammoqty = get_quantity(tmpstring, maxarrows);
                if (ammoqty <= 0) ammoqty = 1;
        }
        q_ptr->number = ammoqty;
        
	/* Reduce and describe inventory */
        if (item >= 0 && !(q_ptr->art_flags4 & (TR4_RETURNING)) && (p_ptr->skill_shooting < 70 || ammoqty > 1))
	{
                inven_item_increase(item, -ammoqty);
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
                /* Elven Bow and Arrow */
                case SV_ELVEN_BOW:
		{
                        tmul = 4;
			break;
		}
                /* High Elf Bow and Arrow */
                case SV_HIELF_BOW:
		{
                        tmul = 5;
			break;
		}
                /* Hunter's Bow and Arrow */
                case 16:
		{
                        tmul = 6;
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

        /* tmul is now increased by your shooting skill! */
        tmul += p_ptr->skill_shooting;

	/* Boost the damage */
	tdam *= tmul;

        /* Haha, the evil Shooter Multiplier!!! */
        tdam *= p_ptr->smultiplier;

        /* Now...you used the Multiple Arrows? Then, prepare for some */
        /* immense amount of damages! ;) */
        /* tdam *= ammoqty; */

	/* Base range */
	tdis = 10 + 5 * tmul;

        /* No negative dams */
        if (tdam < 0) tdam = 0;

	/* Take a (partial) turn */
	energy_use = (100 / thits);
        
        /* Ricochets ? */
        if(p_ptr->pclass == CLASS_ARCHER)
        {
                num_ricochet = (p_ptr->lev / 10) - 1;
                num_ricochet = (num_ricochet < 0)?0:num_ricochet;
        }
        
	/* Start at the player */
        by = py;
        bx = px;
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

        while(TRUE)
        {
                /* Travel until stopped */
                for (cur_dis = 0; cur_dis <= tdis; )
                {
                        /* Hack -- Stop at the target */
                        if ((y == ty) && (x == tx)) break;

                        /* Calculate the new location (see "project()") */
                        ny = y;
                        nx = x;
                        mmove2(&ny, &nx, by, bx, ty, tx);

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
                                int penalitypercent = 76 - p_ptr->abilities[(CLASS_ARCHER * 10) + 1];
                                cave_type *c_ptr = &cave[y][x];

                                monster_type *m_ptr = &m_list[c_ptr->m_idx];
                                monster_race *r_ptr = &r_info[m_ptr->r_idx];

                                /* Check the visibility */
                                visible = m_ptr->ml;

                                /* Note the collision */
                                hit_body = TRUE;

                                /* Accurate Shots ability */
                                hitbonus += (p_ptr->abilities[(CLASS_ARCHER)] * 20);

                                /* Called shots actually LOWERS hit rate! */
                                if (penalitypercent < 0) penalitypercent = 0;
                                if (calledshot > 0) hitpenality = ((p_ptr->to_h + hitbonus) * penalitypercent) / 100;

                                /* Did we hit it (penalize range) */
                                if (player_hit_monster(m_ptr, j_ptr->to_h + hitbonus - hitpenality))
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
                                        /* Check for the Piercing Shots ability! */
                                        if (p_ptr->abilities[(CLASS_ARCHER * 10) + 2] >= 1 || (q_ptr->art_flags4 & (TR4_LOWER_DEF)))
                                        {
                                                int defamount = 0;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                if (q_ptr->art_flags4 & (TR4_LOWER_DEF))
                                                {
                                                        defamount = (damroll(q_ptr->dd, q_ptr->ds) * p_ptr->smultiplier) / 10;
                                                }
                                                defamount += (p_ptr->abilities[(CLASS_ARCHER * 10) + 2] * 2);
                                                msg_format("%s loses %d defense!", m_name, defamount);
                                                m_ptr->defense -= defamount;
                                                if (m_ptr->defense <= 0) m_ptr->defense = 0;
                                        }
                                        /* Lower hit rate? */
                                        if ((q_ptr->art_flags4 & (TR4_LOWER_HIT)))
                                        {
                                                int hitamount;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                hitamount = (damroll(q_ptr->dd, q_ptr->ds) * p_ptr->smultiplier) / 10;
                                                if (m_ptr->hitrate <= 0) hitamount = 0;
                                                msg_format("%s loses %d hit rate!", m_name, hitamount);
                                                m_ptr->hitrate -= hitamount;
                                                if (m_ptr->hitrate <= 0) m_ptr->hitrate = 0;
                                        }
                                        /* Called shot! */
                                        if (calledshot == CALLED_LEGS)
                                        {
                                                int hitamount;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                hitamount = (p_ptr->skill_shooting / 3) + ((p_ptr->skill_shooting / 3) * (p_ptr->abilities[(CLASS_ARCHER * 10) + 1] / 4) / 100);
                                                if (hitamount > 50) hitamount = 50;

                                                if (m_ptr->mspeed == 0) hitamount = 0;
                                                if (hitamount > m_ptr->mspeed)
                                                {
                                                        m_ptr->mspeed = 0;
                                                }
                                                else m_ptr->mspeed -= hitamount;
                                                msg_format("%s loses %d speed!", m_name, hitamount);
                                        }
                                        if (calledshot == CALLED_ARMS)
                                        {
                                                int hitamount;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                hitamount = (p_ptr->skill_shooting / 4) * (p_ptr->abilities[(CLASS_ARCHER * 10) + 1]);

                                                m_ptr->hitrate -= hitamount;
                                                if (m_ptr->hitrate < 0) m_ptr->hitrate = 0;
                                                msg_format("%s loses %d hit rate!", m_name, hitamount);
                                        }
                                        if (q_ptr->tval == TV_SHOT && p_ptr->abilities[(CLASS_ARCHER * 10) + 8] >= 1)
                                        {
                                                int confpower = p_ptr->skill_shooting + (p_ptr->abilities[(CLASS_ARCHER * 10) + 8] * 2);
                                                int mresconf = m_ptr->level * 3;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                if ((r_ptr->flags1 & (RF1_UNIQUE)) || m_ptr->boss >= 1) mresconf *= 5;
                                                if (randint(confpower) >= randint(mresconf))
                                                {
                                                        if (!(r_ptr->flags1 & (RF1_QUESTOR)) && !(m_ptr->abilities & (BOSS_IMMUNE_WEAPONS)))
                                                        {
                                                                msg_format("%s is confused!", m_name);
                                                                m_ptr->confused = 5;
                                                        }
                                                }
                                        }
                                        
                                        /* Apply special damage XXX XXX XXX */                                        
                                        tdam = tot_dam_aux(q_ptr, tdam, m_ptr);
                                        tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);

                                        /* Head called shot */
                                        if (calledshot == CALLED_HEAD) tdam *= 5;

                                        /* No negative damage */
                                        if (tdam < 0) tdam = 0;

                                        /* Archers shoots multiple arrows at once! */
                                        tdam *= ammoqty;
                                        
                                        /* Bad news: Damage resistance apply for bows now... */
                                        tdam = monster_damage_reduction(tdam, m_ptr, FALSE);

                                        /* Another bad new: Elites/Bosses can be immune... */
                                        if (m_ptr->abilities & (BOSS_HALVE_DAMAGES))
                                        {
                                                tdam = tdam / 2;
                                        }
                                        if (m_ptr->abilities & (BOSS_IMMUNE_WEAPONS))
                                        {
                                                msg_print("The monster is immune!");
                                                tdam = 0;
                                        }

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

                                                /* Burning shots ability! :) */
                                                if (p_ptr->abilities[(CLASS_ARCHER * 10) + 4] >= 1)
                                                {
                                                        no_magic_return = TRUE;
                                                        corpse_explode(((p_ptr->abilities[(CLASS_ARCHER * 10) + 4] * 20) * p_ptr->lev), m_ptr->fx, m_ptr->fy, 0, GF_FIRE);
                                                        no_magic_return = FALSE;
                                                }
                                                /* Venomous shots ability! :) */
                                                /* Basically the same as burning shots */
                                                if (p_ptr->abilities[(CLASS_ARCHER * 10) + 5] >= 1)
                                                {
                                                        no_magic_return = TRUE;
                                                        corpse_explode(((p_ptr->abilities[(CLASS_ARCHER * 10) + 5] * 20) * p_ptr->lev), m_ptr->fx, m_ptr->fy, 0, GF_POIS);
                                                        no_magic_return = FALSE;
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

                /* Break ? */
                if(rand_int(100) < j)
                {
                        breakage = 100;
                        break;
                }

                /* If no break and if Archer, the ammo can ricochets */
                if((num_ricochet) && (hit_body) && (magik(45 + p_ptr->lev)))
                {
                        byte d;

                        num_ricochet--;
                        hit_body = FALSE;

                        /* New base location */
                        by = y;
                        bx = x;

                        /* New target location */
                        while(TRUE)
                        {
                                d = rand_int(10);
                                if(d != 5) break;
                        }
                        tx = px + 99 * ddx[d];
                        ty = py + 99 * ddy[d];

                        msg_format("The %s ricochets!", o_name);
                }
                else
                        break;
        }

	/* Drop (or break) near that location */
        if (p_ptr->skill_shooting < 70 && ammoqty <= 1)
        {
                drop_near(q_ptr, breakage, y, x);
        }
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
        int tdis;
        s32b tdam;
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

	/*
	 * Hack -- If rods or wands are thrown, the total maximum timeout or
	 * charges need to be allocated between the two stacks.
	 */
        if (o_ptr->tval == TV_WAND)
	{
		q_ptr->pval = o_ptr->pval / o_ptr->number;

		if (o_ptr->number > 1) o_ptr->pval -= q_ptr->pval;

		/* Hack -- Rods also need to have their timeouts distributed.  The
		 * thrown rod will accept all time remaining to charge up to its
		 * maximum.
	         */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			if (q_ptr->pval > o_ptr->timeout) q_ptr->timeout = o_ptr->timeout;
			else q_ptr->timeout = q_ptr->pval;

			if (o_ptr->number > 1) o_ptr->timeout -= q_ptr->timeout;
		}
	}

	/* Single object */
	q_ptr->number = 1;

	/* Reduce and describe inventory */
        if (item >= 0 && !(q_ptr->art_flags4 & (TR4_RETURNING)))
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Reduce and describe floor item */
        else if (!(q_ptr->art_flags4 & (TR4_RETURNING)))
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
        tdis = (max_carry() + 20) * mul / div;

	/* Max distance of 10-18 */
	if (tdis > mul) tdis = mul;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d;
        tdam *= (p_ptr->skill_throwing + 1);

        /* Throwing an ammo!?? This is ridiculous... */
        if (is_ammo(q_ptr)) tdam = tdam / 10;

        /* Evil and powerful multiplier... ;) */
        if (p_ptr->skill_throwing >= 25) tdam *= (p_ptr->multiplier + 1);
        else tdam *= p_ptr->multiplier;

	/* Take a turn */
        if (p_ptr->skill_throwing >= 50) energy_use = 50;
        else energy_use = 100;

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
                        if (player_hit_monster(m_ptr, 0))
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

                                        /* Throwing skill high enough to reduce defense? */
                                        if (p_ptr->skill_throwing >= 70)
                                        {
                                                m_ptr->defense = m_ptr->defense / 2;
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
                        else
                        {
                                char m_name[80];

                                /* Get the monster name (or "it") */
                                monster_desc(m_name, m_ptr, 0);
                                
                                msg_format("You miss %^s.", m_name);
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
        int y, x, ny, nx, ty, tx;
        int tdis;
        s32b tdam;
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
        tdis = (max_carry() + 20) * mul / div;

	/* Max distance of 10-18 */
	if (tdis > mul) tdis = mul;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d;
        tdam *= (p_ptr->skill_throwing + 1);

        /* Evil and powerful multiplier... ;) */
        if (p_ptr->skill_throwing >= 25) tdam *= (p_ptr->multiplier + 1);
        else tdam *= p_ptr->multiplier;

	/* Take a turn */
        if (p_ptr->skill_throwing >= 50) energy_use = 50;
        else energy_use = 100;


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
                        if (player_hit_monster(m_ptr, q_ptr->to_h))
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

                                        /* Throwing skill high enough to reduce defense? */
                                        if (p_ptr->skill_throwing >= 70)
                                        {
                                                m_ptr->defense = m_ptr->defense / 2;
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
                        else
                        {
                                char m_name[80];

                                /* Get the monster name (or "it") */
                                monster_desc(m_name, m_ptr, 0);
                                
                                msg_format("You miss %^s.", m_name);
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
        /* Must be removed... */
        return (FALSE);
}

static void cmd_racial_power_aux (void)
{
	s16b plev = p_ptr->lev;
	char ch = 0;
	int amber_power = 0;
	int dir = 0;    

        if((!p_ptr->tim_mimic)&&(!p_ptr->body_monster))
        switch(p_ptr->prace)
	{


		case RACE_VAMPIRE:
                        if (!get_rep_dir(&dir)) break;
                        vampire_drain(dir);
			break;


		    case RACE_DEMON:

			/* Select power to use */
			while (TRUE)
			{
                                if (!get_com("Use which power? [F]ire Breath, [D]arkness Breath ", &ch))
				{
					amber_power = 0;
					break;
				}

                                if (ch == 'F' || ch == 'f')
				{
					amber_power = 1;
					break;
				}

                                if (ch == 'D' || ch == 'd')
				{
					amber_power = 2;
					break;
				}


			}

			if (amber_power == 1)
			{
                 
                                if (p_ptr->lev >= 15)
				{
                                        if (!get_aim_dir(&dir)) break;
                                        msg_print("You breath fire.");
                                        fire_ball(GF_FIRE, dir, p_ptr->lev * 20, 2);
                                        energy_use = 100;
				}
				else
				{
                                        msg_print("You must reach Level 15 to use this power.");
               
				}
                        }

                        if (amber_power == 2)
			{
                               
                                if (p_ptr->lev >= 22)
                                {
                                        if (!get_aim_dir(&dir)) break;
                                        msg_print("You breath darkness.");
                                        fire_ball(GF_DARK, dir, p_ptr->lev * 40, 2);
                                        energy_use = 100;
                                }
                                else
                                {
                                        msg_print("You must reach level 22 to use this power.");
                                }
                        }


                        break;
                    case RACE_CELESTIAL:
                              {
                                if (!get_aim_dir(&dir)) break;
                                fire_ball(GF_LITE, dir, p_ptr->lev * 30, 2);
                                energy_use = 100;
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
                        if (!get_rep_dir(&dir)) break;
                        vampire_drain(dir);
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
                        msg_print("The power of Eru Iluvatar flows through you!");
                        msg_print("The world changes!");
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
void do_cmd_racial_power(bool combat_feat)
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

        monster_type *m_ptr;
	int pets = 0, pet_ctr = 0;

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
		case RACE_VAMPIRE:
                        racial_power = "Drain Life (30 damages/level, physical damages, heals and feed you.)";
			has_racial = TRUE;
			break;
                case RACE_DEMON:
                        racial_power = "Balrog's Powers";
			has_racial = TRUE;
                        break;

                case RACE_CELESTIAL:
                        racial_power = "Light Ball (Damages: 30/level  Radius: 2)";
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

        if (has_racial && !combat_feat)
	{
		powers[0] = -1;
		strcpy(power_desc[0], racial_power);
		num++;
	}
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1 && !(p_ptr->auraon) && !combat_feat)
        {
                powers[num] = -995;
                strcpy(power_desc[num], "Turn on your aura.");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1 && p_ptr->auraon && !combat_feat)
        {
                powers[num] = -996;
                strcpy(power_desc[num], "Turn off your aura.");
                num++;
        }        


        if (p_ptr->abilities[(CLASS_WARRIOR * 10)] >= 1)
	{
                powers[num] = -6;
                strcpy(power_desc[num], "Spin Attack");
		num++;
	}
        if (p_ptr->abilities[(CLASS_WARRIOR * 10) + 4] >= 1)
	{
                powers[num] = -7;
                strcpy(power_desc[num], "Accurate Strike");
		num++;
	}
        if (p_ptr->abilities[(CLASS_WARRIOR * 10) + 6] >= 1 && !combat_feat)
	{
                powers[num] = -8;
                strcpy(power_desc[num], "War Cry");
		num++;
	}
        if (p_ptr->abilities[(CLASS_WARRIOR * 10) + 8] >= 1)
	{
                powers[num] = -9;
                strcpy(power_desc[num], "Leaping Spin");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 1] >= 1 && !combat_feat)
	{
                powers[num] = -10;
                strcpy(power_desc[num], "Force Field");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 2] >= 1 && !combat_feat)
	{
                powers[num] = -11;
                strcpy(power_desc[num], "Magic Missile");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 4] >= 1 && !combat_feat)
	{
                powers[num] = -12;
                strcpy(power_desc[num], "Slow Down");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 5] >= 1 && !combat_feat)
	{
                powers[num] = -13;
                strcpy(power_desc[num], "Mirror Images");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 6] >= 1 && !combat_feat)
	{
                powers[num] = -14;
                strcpy(power_desc[num], "Damages Curse");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 7] >= 1 && !combat_feat)
	{
                powers[num] = -15;
                strcpy(power_desc[num], "Drain Object");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 8] >= 1 && !combat_feat)
	{
                powers[num] = -16;
                strcpy(power_desc[num], "Stone To Gold");
		num++;
	}
        if (p_ptr->abilities[(CLASS_MAGE * 10) + 9] >= 1 && !combat_feat)
	{
                powers[num] = -17;
                strcpy(power_desc[num], "Animated Knight");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10)] >= 1 && !combat_feat)
	{
                powers[num] = -18;
                strcpy(power_desc[num], "Heal");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 6] >= 1 && !combat_feat)
	{
                powers[num] = -19;
                strcpy(power_desc[num], "Harm");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 3] >= 1 && !combat_feat)
	{
                powers[num] = -20;
                strcpy(power_desc[num], "Mace Of Heaven");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 4] >= 1 && !combat_feat)
	{
                powers[num] = -21;
                strcpy(power_desc[num], "Dark Prayer");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 5] >= 1 && !combat_feat)
	{
                powers[num] = -22;
                strcpy(power_desc[num], "Holy Might");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 1] >= 1 && !combat_feat)
	{
                powers[num] = -23;
                strcpy(power_desc[num], "Turn Undeads");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] >= 1 && !combat_feat)
	{
                powers[num] = -24;
                strcpy(power_desc[num], "Forbidden Ritual");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 8] >= 1 && !combat_feat)
	{
                powers[num] = -25;
                strcpy(power_desc[num], "Divine Armor");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PRIEST * 10) + 9] >= 1 && !combat_feat)
	{
                powers[num] = -26;
                strcpy(power_desc[num], "Godly Wrath");
		num++;
	}
        if (p_ptr->abilities[(CLASS_ROGUE * 10)] >= 1 && !combat_feat)
	{
                powers[num] = -27;
                strcpy(power_desc[num], "Hide In Shadows");
		num++;
	}
        if (p_ptr->abilities[(CLASS_ROGUE * 10) + 4] >= 1 && !combat_feat)
	{
                powers[num] = -28;
                strcpy(power_desc[num], "Spike Trap");
		num++;
	}
        if (p_ptr->abilities[(CLASS_ROGUE * 10) + 5] >= 1 && !combat_feat)
	{
                powers[num] = -29;
                strcpy(power_desc[num], "Poison Weapon");
		num++;
	}
        if (p_ptr->abilities[(CLASS_ROGUE * 10) + 6] >= 1 && !combat_feat)
	{
                powers[num] = -30;
                strcpy(power_desc[num], "Gas Trap");
		num++;
	}
        if (p_ptr->abilities[(CLASS_ROGUE * 10) + 7] >= 1 && !combat_feat)
	{
                powers[num] = -31;
                strcpy(power_desc[num], "Poison Trap");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 2] >= 1 && !combat_feat)
	{
                powers[num] = -32;
                strcpy(power_desc[num], "Entangle");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 3] >= 1 && !combat_feat)
	{
                powers[num] = -33;
                strcpy(power_desc[num], "Animal Empathy");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 4] >= 1 && !combat_feat)
	{
                powers[num] = -34;
                strcpy(power_desc[num], "Call Anmimal");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 5] >= 1 && !combat_feat)
	{
                powers[num] = -35;
                strcpy(power_desc[num], "Warp On Trees");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 6] >= 1 && !combat_feat)
	{
                powers[num] = -36;
                strcpy(power_desc[num], "Summon Nymph");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 7] >= 1 && !combat_feat)
	{
                powers[num] = -37;
                strcpy(power_desc[num], "Thorned Vines");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 8] >= 1 && !combat_feat)
	{
                powers[num] = -38;
                strcpy(power_desc[num], "Sleep Pollen");
		num++;
	}
        if (p_ptr->abilities[(CLASS_RANGER * 10) + 9] >= 1 && !combat_feat)
	{
                powers[num] = -39;
                strcpy(power_desc[num], "Force Of Nature");
		num++;
	}
        if (p_ptr->abilities[(CLASS_PALADIN * 10)] >= 1 && !combat_feat)
        {
                powers[num] = -40;
                strcpy(power_desc[num], "Divine Strength");
                num++;
        }
        if (p_ptr->abilities[(CLASS_PALADIN * 10) + 1] >= 1 && !combat_feat)
        {
                powers[num] = -41;
                strcpy(power_desc[num], "Holy Bolt");
                num++;
        }
        if (p_ptr->abilities[(CLASS_PALADIN * 10) + 3] >= 1)
        {
                powers[num] = -42;
                strcpy(power_desc[num], "Smite Evil");
                num++;
        }
        if (p_ptr->abilities[(CLASS_PALADIN * 10) + 4] >= 1 && !combat_feat)
        {
                powers[num] = -43;
                strcpy(power_desc[num], "Blade Of Purity");
                num++;
        }
        if (p_ptr->abilities[(CLASS_PALADIN * 10) + 6] >= 1 && !combat_feat)
        {
                powers[num] = -44;
                strcpy(power_desc[num], "Feat Of Faith");
                num++;
        }
        if (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] >= 1 && !combat_feat)
        {
                powers[num] = -45;
                strcpy(power_desc[num], "Retrograde Darkness");
                num++;
        }
        if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 1 && !combat_feat)
        {
                powers[num] = -46;
                strcpy(power_desc[num], "Shining Armor");
                num++;
        }
        if (p_ptr->abilities[(CLASS_PALADIN * 10) + 9] >= 1 && !combat_feat)
        {
                powers[num] = -47;
                strcpy(power_desc[num], "Word Of Peace");
                num++;
        }
        if (p_ptr->abilities[(CLASS_MONK * 10) + 1] >= 1)
        {
                powers[num] = -48;
                strcpy(power_desc[num], "Spin Kick");
                num++;
        }
        if (p_ptr->abilities[(CLASS_MONK * 10) + 2] >= 1)
        {
                powers[num] = -49;
                strcpy(power_desc[num], "Hard Kick");
                num++;
        }
        if (p_ptr->abilities[(CLASS_MONK * 10) + 5] >= 1)
        {
                powers[num] = -50;
                strcpy(power_desc[num], "Ki Punch");
                num++;
        }
        if (p_ptr->abilities[(CLASS_MONK * 10) + 8] >= 1)
        {
                powers[num] = -51;
                strcpy(power_desc[num], "High Somersault");
                num++;
        }
        if (p_ptr->abilities[(CLASS_ARCHER * 10) + 1] >= 1 && !combat_feat)
        {
                powers[num] = -52;
                strcpy(power_desc[num], "Called Shot");
                num++;
        }
        if (p_ptr->abilities[(CLASS_ARCHER * 10) + 3] >= 1 && !combat_feat)
        {
                powers[num] = -53;
                strcpy(power_desc[num], "Chain Shot");
                num++;
        }
        if (p_ptr->abilities[(CLASS_ARCHER * 10) + 6] >= 1 && !combat_feat)
        {
                powers[num] = -54;
                strcpy(power_desc[num], "Multiple Arrows");
                num++;
        }
        if (p_ptr->abilities[(CLASS_ARCHER * 10) + 7] >= 1 && !combat_feat)
        {
                powers[num] = -55;
                strcpy(power_desc[num], "Charged Bolt");
                num++;
        }
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10)] >= 1 && !combat_feat)
        {
                char powername[80];
                powers[num] = -56;
                sprintf(powername, "%s Ball", get_element_name(p_ptr->elemlord));
                strcpy(power_desc[num], powername);
                num++;
        }
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 1] >= 1)
        {
                char powername[80];
                powers[num] = -57;
                sprintf(powername, "%s Strike", get_element_name(p_ptr->elemlord));
                strcpy(power_desc[num], powername);
                num++;
        }
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 2] >= 1 && !combat_feat)
        {
                char powername[80];
                powers[num] = -58;
                sprintf(powername, "Shield Of %s", get_element_name(p_ptr->elemlord));
                strcpy(power_desc[num], powername);
                num++;
        }
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 3] >= 1)
        {
                char powername[80];
                powers[num] = -59;
                sprintf(powername, "Fist Of %s", get_element_name(p_ptr->elemlord));
                strcpy(power_desc[num], powername);
                num++;
        }
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 6] >= 1 && !combat_feat)
        {
                powers[num] = -60;
                strcpy(power_desc[num], "Explosive Throw");
                num++;
        }
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 7] >= 1 && !combat_feat)
        {
                char powername[80];
                powers[num] = -61;
                sprintf(powername, "Wave Of %s", get_element_name(p_ptr->elemlord));
                strcpy(power_desc[num], powername);
                num++;
        }
        if (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] >= 1 && !combat_feat)
        {
                powers[num] = -62;
                strcpy(power_desc[num], "Memorize");
                num++;
        }
        if (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] >= 1 && p_ptr->body_monster == 0 && !combat_feat)
        {
                monster_race *r_ptr = &r_info[p_ptr->memorized];
                powers[num] = -63;
                sprintf(power_desc[num], "Morph: %s", (r_name + r_ptr->name));
                num++;
        }
        if (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] >= 1 && p_ptr->body_monster != 0 && !combat_feat)
        {
                powers[num] = -64;
                strcpy(power_desc[num], "Revert to normal shape");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 5] >= 1 && !combat_feat)
        {
                powers[num] = -65;
                strcpy(power_desc[num], "Dominate Monster");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 7] >= 1 && p_ptr->body_monster != 0 && !combat_feat)
        {
                powers[num] = -66;
                strcpy(power_desc[num], "Monstrous Wave");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 2] >= 1 && shield_has())
        {
                powers[num] = -67;
                strcpy(power_desc[num], "Shield Bash");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 7] >= 1)
        {
                powers[num] = -68;
                strcpy(power_desc[num], "Defensive Strike");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 8] >= 1 && shield_has() && !combat_feat)
        {
                powers[num] = -69;
                strcpy(power_desc[num], "Boomerang Shield");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10)] >= 1 && !combat_feat)
        {
                powers[num] = -70;
                strcpy(power_desc[num], "Shatter Evil");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 1] >= 1 && !combat_feat)
        {
                powers[num] = -71;
                strcpy(power_desc[num], "Angelic Voice");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 1 && !combat_feat)
        {
                powers[num] = -72;
                strcpy(power_desc[num], "Bless Weapon");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 5] >= 1 && !combat_feat)
        {
                powers[num] = -73;
                strcpy(power_desc[num], "Sacred Light");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 6] >= 1 && !combat_feat)
        {
                powers[num] = -74;
                strcpy(power_desc[num], "Slay Evil");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 7] >= 1 && !combat_feat)
        {
                powers[num] = -75;
                strcpy(power_desc[num], "Angelic Call");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 9] >= 1 && !combat_feat)
        {
                powers[num] = -76;
                strcpy(power_desc[num], "Light Of Life");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ZELAR * 10)] >= 1 && !combat_feat)
        {
                powers[num] = -77;
                strcpy(power_desc[num], "Energy Spin");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ZELAR * 10) + 1] >= 1 && !combat_feat)
        {
                powers[num] = -78;
                strcpy(power_desc[num], "Wave Kick");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ZELAR * 10) + 3] >= 1)
        {
                powers[num] = -79;
                strcpy(power_desc[num], "Energy Punch");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ZELAR * 10) + 4] >= 1)
        {
                powers[num] = -80;
                strcpy(power_desc[num], "Legs Breaking Throw");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ZELAR * 10) + 6] >= 1 && !combat_feat)
        {
                powers[num] = -81;
                strcpy(power_desc[num], "Energize Self");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ZELAR * 10) + 7] >= 1 && !combat_feat)
        {
                powers[num] = -82;
                strcpy(power_desc[num], "Dual Wave Fist");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ZELAR * 10) + 8] >= 1 && unarmed() && !combat_feat)
        {
                powers[num] = -83;
                strcpy(power_desc[num], "Gather Power");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 9] >= 1 && !combat_feat)
        {
                powers[num] = -84;
                strcpy(power_desc[num], "Capture Soul");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] >= 1 && !combat_feat)
        {
                powers[num] = -85;
                strcpy(power_desc[num], "Soul Power");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 1] >= 1 && !combat_feat)
        {
                powers[num] = -86;
                strcpy(power_desc[num], "Simulacrum");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 2] >= 1 && !combat_feat)
        {
                powers[num] = -87;
                strcpy(power_desc[num], "Soul Bind");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 3] >= 1 && !combat_feat)
        {
                powers[num] = -88;
                strcpy(power_desc[num], "Sealing Light");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 4] >= 1 && !combat_feat)
        {
                powers[num] = -89;
                strcpy(power_desc[num], "Soul Energize");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 6] >= 1)
        {
                powers[num] = -90;
                strcpy(power_desc[num], "Soul Strike");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7] >= 1 && !combat_feat)
        {
                powers[num] = -91;
                strcpy(power_desc[num], "Soul Shield");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 9] >= 1 && !combat_feat)
        {
                powers[num] = -92;
                strcpy(power_desc[num], "Wrath Of Souls");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SHADOW * 10)] >= 1)
        {
                powers[num] = -93;
                strcpy(power_desc[num], "Stealth Attack");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SHADOW * 10) + 2] >= 1 && !combat_feat)
        {
                powers[num] = -94;
                strcpy(power_desc[num], "Shadow Cloak");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SHADOW * 10) + 3] >= 1 && !combat_feat)
        {
                powers[num] = -95;
                strcpy(power_desc[num], "Shadow Ball");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SHADOW * 10) + 5] >= 1 && !combat_feat)
        {
                powers[num] = -96;
                strcpy(power_desc[num], "Shadow Phase");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SHADOW * 10) + 6] >= 1 && !combat_feat)
        {
                powers[num] = -97;
                strcpy(power_desc[num], "Dark Mist");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_SHADOW * 10) + 9] >= 1 && !combat_feat)
        {
                powers[num] = -98;
                strcpy(power_desc[num], "Storm Of Shadow Edges");
                num++;
        }        

        if (!combat_feat) {
        strcpy(power_desc[num], "Hire/Befriend(See newbies guide.)");
        powers[num] = -2;
        num++;
        }

	if (p_ptr->muta1)
	{
		int lvl = p_ptr->lev;

                if (p_ptr->muta1 & MUT1_SPIT_ACID && !combat_feat)
		{
			if (lvl < 9)
				strcpy(power_desc[num],"spit acid        (lvl 9, cost 9, dam lvl)");
			else
				strcpy(power_desc[num],"spit acid        (cost 9, dam lvl, DEX 15@9)");
			powers[num++] = MUT1_SPIT_ACID;
		}
                if (p_ptr->muta1 & MUT1_BR_FIRE && !combat_feat)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"fire breath      (lvl 20, cost lvl, dam lvl * 2)");
			else
				strcpy(power_desc[num],"fire breath      (cost lvl, dam lvl * 2, CON 18@20)");
			powers[num++] = MUT1_BR_FIRE;
		}
                if (p_ptr->muta1 & MUT1_HYPN_GAZE && !combat_feat)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"hypnotic gaze    (lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"hypnotic gaze    (cost 12, CHR 18@12)");
			powers[num++] = MUT1_HYPN_GAZE;
		}
                if (p_ptr->muta1 & MUT1_TELEKINES && !combat_feat)
		{
			if (lvl < 9)
				strcpy(power_desc[num],"telekinesis      (lvl 9, cost 9)");
			else
				strcpy(power_desc[num],"telekinesis      (cost 9, WIS 14@9)");
			powers[num++] = MUT1_TELEKINES;
		}
                if (p_ptr->muta1 & MUT1_VTELEPORT && !combat_feat)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"teleport         (lvl 7, cost 7)");
			else
				strcpy(power_desc[num],"teleport         (cost 7, WIS 15@7)");
			powers[num++] = MUT1_VTELEPORT;
		}
                if (p_ptr->muta1 & MUT1_MIND_BLST && !combat_feat)
		{
			if (lvl < 5)
				strcpy(power_desc[num],"mind blast       (lvl 5, cost 3)");
			else
				strcpy(power_desc[num],"mind blast       (cost 3, WIS 15@7)");
			powers[num++] = MUT1_MIND_BLST;
		}
                if (p_ptr->muta1 & MUT1_RADIATION && !combat_feat)
		{
			if (lvl < 15)
				strcpy(power_desc[num],"emit radiation   (lvl 15, cost 15)");
			else
				strcpy(power_desc[num],"emit radiation   (cost 15, CON 14@15)");
			powers[num++] = MUT1_RADIATION;
		}
                if (p_ptr->muta1 & MUT1_VAMPIRISM && !combat_feat)
		{
			if (lvl < 13)
				strcpy(power_desc[num],"vampiric drain   (lvl 13, cost lvl)");
			else
				strcpy(power_desc[num],"vampiric drain   (cost lvl, CON 14@13)");
			powers[num++] = MUT1_VAMPIRISM;
		}
                if (p_ptr->muta1 & MUT1_SMELL_MET && !combat_feat)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"smell metal      (lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"smell metal      (cost 2, INT 12@3)");
			powers[num++] = MUT1_SMELL_MET;
		}
                if (p_ptr->muta1 & MUT1_SMELL_MON && !combat_feat)
		{
			if (lvl < 5)
				strcpy(power_desc[num],"smell monsters   (lvl 5, cost 4)");
			else
				strcpy(power_desc[num],"smell monsters   (cost 4, INT 15@5)");
			powers[num++] = MUT1_SMELL_MON;
		}
                if (p_ptr->muta1 & MUT1_BLINK && !combat_feat)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"blink            (lvl 3, cost 3)");
			else
				strcpy(power_desc[num],"blink            (cost 3, WIS 12@3)");
			powers[num++] = MUT1_BLINK;
		}
                if (p_ptr->muta1 & MUT1_EAT_ROCK && !combat_feat)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"eat rock         (lvl 8, cost 12)");
			else
				strcpy(power_desc[num],"eat rock         (cost 12, CON 18@8)");
			powers[num++] = MUT1_EAT_ROCK;
		}
                if (p_ptr->muta1 & MUT1_SWAP_POS && !combat_feat)
		{
			if (lvl < 15)
				strcpy(power_desc[num],"swap position    (lvl 15, cost 12)");
			else
				strcpy(power_desc[num],"swap position    (cost 12, DEX 16@15)");
			powers[num++] = MUT1_SWAP_POS;
		}
                if (p_ptr->muta1 & MUT1_SHRIEK && !combat_feat)
		{
			if (lvl < 4)
				strcpy(power_desc[num],"shriek           (lvl 4, cost 4)");
			else
				strcpy(power_desc[num],"shriek           (cost 4, CON 6@4)");
			powers[num++] = MUT1_SHRIEK;
		}
                if (p_ptr->muta1 & MUT1_ILLUMINE && !combat_feat)
		{
			if (lvl < 3)
				strcpy(power_desc[num],"illuminate       (lvl 3, cost 2)");
			else
				strcpy(power_desc[num],"illuminate       (cost 2, INT 10@3)");
			powers[num++] = MUT1_ILLUMINE;
		}
                if (p_ptr->muta1 & MUT1_DET_CURSE && !combat_feat)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"detect curses    (lvl 7, cost 14)");
			else
				strcpy(power_desc[num],"detect curses    (cost 14, WIS 14@7)");
			powers[num++] = MUT1_DET_CURSE;
		}
                if (p_ptr->muta1 & MUT1_BERSERK && !combat_feat)
		{
			if (lvl < 8)
				strcpy(power_desc[num],"berserk          (lvl 8, cost 8)");
			else
				strcpy(power_desc[num],"berserk          (cost 8, STR 14@8)");
			powers[num++] = MUT1_BERSERK;
		}
                if (p_ptr->muta1 & MUT1_POLYMORPH && !combat_feat)
		{
			if (lvl < 18)
				strcpy(power_desc[num],"polymorph        (lvl 18, cost 20)");
			else
				strcpy(power_desc[num],"polymorph        (cost 20, CON 18@18)");
			powers[num++] = MUT1_POLYMORPH;
		}
                if (p_ptr->muta1 & MUT1_MIDAS_TCH && !combat_feat)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"midas touch      (lvl 10, cost 5)");
			else
				strcpy(power_desc[num],"midas touch      (cost 5, INT 12@10)");
			powers[num++] = MUT1_MIDAS_TCH;
		}
                if (p_ptr->muta1 & MUT1_GROW_MOLD && !combat_feat)
		{
			if (lvl < 1)
				strcpy(power_desc[num],"grow mold        (lvl 1, cost 6)");
			else
				strcpy(power_desc[num],"grow mold        (cost 6, CON 14@1)");
			powers[num++] = MUT1_GROW_MOLD;
		}
                if (p_ptr->muta1 & MUT1_RESIST && !combat_feat)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"resist elements  (lvl 10, cost 12)");
			else
				strcpy(power_desc[num],"resist elements  (cost 12, CON 12@10)");
			powers[num++] = MUT1_RESIST;
		}
                if (p_ptr->muta1 & MUT1_EARTHQUAKE && !combat_feat)
		{
			if (lvl < 12)
				strcpy(power_desc[num],"earthquake       (lvl 12, cost 12)");
			else
				strcpy(power_desc[num],"earthquake       (cost 12, STR 16@12)");
			powers[num++] = MUT1_EARTHQUAKE;
		}
                if (p_ptr->muta1 & MUT1_EAT_MAGIC && !combat_feat)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"eat magic        (lvl 17, cost 1)");
			else
				strcpy(power_desc[num],"eat magic        (cost 1, WIS 15@17)");
			powers[num++] = MUT1_EAT_MAGIC;
		}
                if (p_ptr->muta1 & MUT1_WEIGH_MAG && !combat_feat)
		{
			if (lvl < 6)
				strcpy(power_desc[num],"weigh magic      (lvl 6, cost 6)");
			else
				strcpy(power_desc[num],"weigh magic      (cost 6, INT 10@6)");
			powers[num++] = MUT1_WEIGH_MAG;
		}
                if (p_ptr->muta1 & MUT1_STERILITY && !combat_feat)
		{
			if (lvl < 20)
				strcpy(power_desc[num],"sterilize        (lvl 20, cost 40)");
			else
				strcpy(power_desc[num],"sterilize        (cost 40, CHR 18@20)");
			powers[num++] = MUT1_STERILITY;
		}
                if (p_ptr->muta1 & MUT1_PANIC_HIT && !combat_feat)
		{
			if (lvl < 10)
				strcpy(power_desc[num],"panic hit        (lvl 10, cost 12)");
			else
				strcpy(power_desc[num],"panic hit        (cost 12, DEX 14@10)");
			powers[num++] = MUT1_PANIC_HIT;
		}
                if (p_ptr->muta1 & MUT1_DAZZLE && !combat_feat)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"dazzle           (lvl 7, cost 15)");
			else
				strcpy(power_desc[num],"dazzle           (cost 15, CHR 8@7)");
			powers[num++] = MUT1_DAZZLE;
		}
                if (p_ptr->muta1 & MUT1_LASER_EYE && !combat_feat)
		{
			if (lvl < 7)
				strcpy(power_desc[num],"laser eye        (lvl 7, cost 10)");
			else
				strcpy(power_desc[num],"laser eye        (cost 10, WIS 9@7)");
			powers[num++] = MUT1_LASER_EYE;
		}
                if (p_ptr->muta1 & MUT1_RECALL && !combat_feat)
		{
			if (lvl < 17)
				strcpy(power_desc[num],"recall           (lvl 17, cost 50)");
			else
				strcpy(power_desc[num],"recall           (cost 50, INT 16@17)");
			powers[num++] = MUT1_RECALL;
		}
                if (p_ptr->muta1 & MUT1_BANISH && !combat_feat)
		{
			if (lvl < 25)
                                strcpy(power_desc[num],"banish evil      (lvl 25, cost 25)");
			else
				strcpy(power_desc[num],"banish evil      (cost 25, WIS 18@25)");
			powers[num++] = MUT1_BANISH;
		}
                if (p_ptr->muta1 & MUT1_COLD_TOUCH && !combat_feat)
		{
			if (lvl < 2)
				strcpy(power_desc[num],"cold touch       (lvl 2, cost 2)");
			else
				strcpy(power_desc[num],"cold touch       (cost 2, CON 11@2)");
			powers[num++] = MUT1_COLD_TOUCH;
		}
                if (p_ptr->muta1 & MUT1_LAUNCHER && !combat_feat)
		{
			strcpy(power_desc[num],    "throw object     (cost lev, STR 6@1)");
			/* XXX_XXX_XXX Hack! MUT1_LAUNCHER counts as negative... */
			powers[num++] = 3;
		}
        }

        if ((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] >= 1 || p_ptr->pclass == CLASS_MONSTER_MAGE) && !combat_feat)
        {
                strcpy(power_desc[num], "switch magic/monster magics");
                powers[num++] = -3;                
        }

        /*if (pets > 0)
	{
                strcpy(power_desc[num], "hypnotize a pet");
                powers[num++] = -3;
        }*/

        /*if (!combat_feat) {
        strcpy(power_desc[num], "Release pet from crystal");
        powers[num++] = -4;
        }*/

        if (pets > 0 && !combat_feat)
	{
                strcpy(power_desc[num], "Unsummon(destroy friendly creature)");
                powers[num++] = -5;
	}

        /* Here comes our feats abilities! */
        if (p_ptr->skill_swords >= 20 && sword_has())
        {
                strcpy(power_desc[num], "Sword Spin");
                powers[num++] = 100;
        }
        if (p_ptr->skill_hafted >= 15 && hafted_has())
        {
                strcpy(power_desc[num], "Smash");
                powers[num++] = 101;
        }
        if (p_ptr->skill_hafted >= 40 && hafted_has())
        {
                strcpy(power_desc[num], "Dizzy Smash");
                powers[num++] = 102;
        }
        if (p_ptr->skill_hafted >= 70 && hafted_has())
        {
                strcpy(power_desc[num], "Shattering Blow");
                powers[num++] = 103;
        }
        if (p_ptr->skill_polearms >= 5 && polearm_has())
        {
                strcpy(power_desc[num], "Thrust");
                powers[num++] = 104;
        }
        if (p_ptr->skill_shooting >= 20 && !combat_feat)
        {
                strcpy(power_desc[num], "Sharpen Ammos");
                powers[num++] = 105;
        }
        if (p_ptr->skill_marts >= 15 && unarmed())
        {
                strcpy(power_desc[num], "Power Punch");
                powers[num++] = 106;
        }
        if (p_ptr->skill_marts >= 80 && unarmed() && !combat_feat)
        {
                strcpy(power_desc[num], "Stunning Blow");
                powers[num++] = 107;
        }
        if (p_ptr->skill_agility >= 1)
        {
                strcpy(power_desc[num], "Jump");
                powers[num++] = 108;
        }
        if (p_ptr->skill_stealth >= 10 && !combat_feat)
        {
                strcpy(power_desc[num], "Hide");
                powers[num++] = 109;
        }
        if (p_ptr->skill_stealth >= 20 && !combat_feat)
        {
                strcpy(power_desc[num], "Run Away");
                powers[num++] = 110;
        }
        if (p_ptr->skill_spellcraft >= 10 && !combat_feat)
        {
                strcpy(power_desc[num], "Enchanted Blood");
                powers[num++] = 111;
        }
        if (p_ptr->skill_spellcraft >= 40 && !combat_feat)
        {
                strcpy(power_desc[num], "Mana Shield");
                powers[num++] = 112;
        }
        if (p_ptr->skill_daggers >= 30 && dagger_check() == TRUE)
        {
                strcpy(power_desc[num], "Eye Stab");
                powers[num++] = 113;
        }
        if (p_ptr->skill_daggers >= 80 && dagger_check() == TRUE && !combat_feat)
        {
                strcpy(power_desc[num], "Fatal Stab");
                powers[num++] = 114;
        }
        if (p_ptr->skill_axes >= 5 && axe_check() == TRUE)
        {
                strcpy(power_desc[num], "Chop");
                powers[num++] = 115;
        }
        if (p_ptr->skill_axes >= 40 && axe_check() == TRUE)
        {
                strcpy(power_desc[num], "Mutilate Legs");
                powers[num++] = 116;
        }
        if (p_ptr->skill_axes >= 70 && axe_check() == TRUE)
        {
                strcpy(power_desc[num], "Mutilate Arms");
                powers[num++] = 117;
        }
        if (p_ptr->skill_leadership >= 5 && !combat_feat)
	{
                strcpy(power_desc[num], "Heal Troops");
                powers[num++] = 118;
	}
        if (p_ptr->skill_leadership >= 20 && !combat_feat)
	{
                strcpy(power_desc[num], "Morale Boost");
                powers[num++] = 119;
	}
        if ((p_ptr->skill_alchemy >= 1 || p_ptr->skill_crafting >= 1) && !combat_feat)
	{
                strcpy(power_desc[num], "Combine Items");
                powers[num++] = 120;
	}
        if ((p_ptr->skill_alchemy >= 1 || p_ptr->skill_crafting >= 1) && !combat_feat)
	{
                strcpy(power_desc[num], "Decompose Item");
                powers[num++] = 121;
	}
                        

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
                        if (p_ptr->magic_mode == 1)
                        {
                                p_ptr->magic_mode = 0;
                                msg_print("You are now in Magic mode.");
                        }
                        else
                        {
                                p_ptr->magic_mode = 1;
                                msg_print("You are now in Monster Magics mode.");
                        }
                }
                /*else if (powers[i] == -4)
                {
                        int m_idx;
                        int item,x,y;
                        object_type *o_ptr;

                        cptr q, s;

                        item_tester_tval = TV_HYPNOS;

                        q = "Release which monster? ";
                        s = "You have no monster to release.";
                        if (!get_item(&item, q, s, (USE_INVEN))) return;

                        o_ptr = &inventory[item]; 
                        
                        msg_print("Place where?");
                        if (!tgt_pt(&x,&y)) return;

                        if((m_idx=place_monster_one_return_no_boss(y, x, o_ptr->pval, FALSE, TRUE, o_ptr->pval3, o_ptr->pval2, o_ptr->xtra1))==0) return;


                        inven_item_increase(item, -1);
                        inven_item_describe(item);
                        inven_item_optimize(item);
                }*/
                else if (powers[i] == -5)
                {
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_UNSUMMON, dir, 0);
                        energy_use = 100;                        
                }
                /* Want that dragon to follow you? ;) */
                else if (powers[i] == -2)
                {
                        if (!get_aim_dir(&dir)) return;
                        hire_befriend(dir);
                        energy_use = 100;
                }
                else if (powers[i] == -6)
                {
                        spin_attack();
                        energy_use = 100;                        
                }

                else if (powers[i] == -7)
                {
                        if (!get_rep_dir(&dir)) return;
                        accurate_strike(dir);
                        energy_use = 100;
                }

                else if (powers[i] == -8)
                {
                        s32b duration;
                        duration = p_ptr->abilities[(CLASS_WARRIOR * 10) + 6] + 4;
                        attack_aura(GF_FEAR_CURSE, duration, 3);
                        update_and_handle();
                }

                else if (powers[i] == -9)
                {
                        leaping_spin();
                        energy_use = 100;
                }

                else if (powers[i] == -10)
                {
                        p_ptr->pres = 25 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] / 5);
                        p_ptr->mres = 25 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] / 5);
                        (void)set_pres((5 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] * 2)));
                        (void)set_mres((5 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] * 2)));
                        energy_use = 100;
                }

                else if (powers[i] == -11)
                {
                        s32b dam;
                        dam = p_ptr->abilities[(CLASS_MAGE * 10) + 2] * 15;
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_MISSILE, dir, (p_ptr->lev * dam));
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -12)
                {
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_SLOW_DOWN, dir, 0, (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 20));
                        energy_use = 100;
                }
                else if (powers[i] == -13)
                {
                        int a = 0;
                        msg_print("You create illusions of yourself!");
                        for (a = 0; a < ((p_ptr->abilities[(CLASS_MAGE * 10) + 5] / 5) + 3); a++)
                        {                        
                                summon_specific_friendly(py, px, 127, SUMMON_ILLUSION, FALSE);
                        }
                        energy_use = 100;
                }
                else if (powers[i] == -14)
                {
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DAMAGES_CURSE, dir, 0, (p_ptr->abilities[(CLASS_MAGE * 10) + 6] / 20));
                        energy_use = 100;
                }
                else if (powers[i] == -15)
                {
                        drain_object();
                        energy_use = 100;
                }
                else if (powers[i] == -16)
                {
                        stone_to_gold();
                        energy_use = 100;
                }
                else if (powers[i] == -17)
                {
                        animate_knight();
                        energy_use = 100;
                }
                else if (powers[i] == -18)
                {
                        msg_print("You heal yourself!");
                        p_ptr->chp += ((10 + (p_ptr->abilities[(CLASS_PRIEST * 10)] / 2)) * p_ptr->lev);
                        if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                        if (p_ptr->abilities[(CLASS_PRIEST * 10)] >= 15)
                        {
                                (void)set_cut(0);
                                (void)set_poisoned(0);
                        }
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -19)
                {
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, (p_ptr->abilities[(CLASS_PRIEST * 10) + 6] * 20) * p_ptr->lev, 0, 1);
                        energy_use = 100;
                }
                else if (powers[i] == -20)
                {
                        mace_of_heaven();
                        energy_use = 100;
                }
                else if (powers[i] == -21)
                {
                        msg_print("You offer your life as a sacrifice...");
                        p_ptr->mhp -= p_ptr->mhp / 2;                        
                        p_ptr->str_boost = ((p_ptr->stat_cur[A_STR] * (p_ptr->abilities[(CLASS_PRIEST * 10) + 4] * 5)) / 100);
                        p_ptr->int_boost = ((p_ptr->stat_cur[A_INT] * (p_ptr->abilities[(CLASS_PRIEST * 10) + 4] * 5)) / 100);
                        p_ptr->dex_boost = ((p_ptr->stat_cur[A_DEX] * (p_ptr->abilities[(CLASS_PRIEST * 10) + 4] * 5)) / 100);
                        p_ptr->con_boost = ((p_ptr->stat_cur[A_CON] * (p_ptr->abilities[(CLASS_PRIEST * 10) + 4] * 5)) / 100);
                        p_ptr->chr_boost = ((p_ptr->stat_cur[A_CHR] * (p_ptr->abilities[(CLASS_PRIEST * 10) + 4] * 5)) / 100);
                        (void)set_str_boost(5 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 4]));
                        (void)set_int_boost(5 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 4]));
                        (void)set_dex_boost(5 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 4]));
                        (void)set_con_boost(5 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 4]));
                        (void)set_chr_boost(5 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 4]));
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -22)
                {
                        msg_print("You feel empowered...");
                        (void)set_fast(4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5]));
                        (void)set_blessed(4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5]));
                        p_ptr->pres = 20 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5] / 3);
                        (void)set_pres(4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5]));
                        energy_use = 100;
                }
                else if (powers[i] == -23)
                {
                        s32b turn_damages;
                        turn_damages = (p_ptr->lev * 40) * p_ptr->abilities[(CLASS_PRIEST * 10) + 1];
                        attack_aura(GF_TURN_UNDEAD, turn_damages, 10);
                        energy_use = 100;
                }
                else if (powers[i] == -24)
                {
                        int x;
                        if (p_ptr->csp >= 200)
                        {
                                if (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] < 50)
                                {
                                        for (x = 0; x < 20; x++)
                                        {                          
                                                summon_specific_friendly(py, px, p_ptr->abilities[(CLASS_PRIEST * 10) + 7], 838, FALSE);
                                        }
                                }
                                else
                                {
                                        for (x = 0; x < 20; x++)
                                        {                          
                                                summon_specific_friendly(py, px, p_ptr->abilities[(CLASS_PRIEST * 10) + 7], SUMMON_DEMON, FALSE);
                                        }
                                }        
                                p_ptr->csp = 0;
                                update_and_handle();
                        }
                        else msg_print("You need at least 200 mana!");
                        energy_use = 100;
                }
                else if (powers[i] == -25)
                {
                        p_ptr->ac_boost = p_ptr->abilities[(CLASS_PRIEST * 10) + 8] * 30;
                        (void)set_ac_boost(p_ptr->abilities[(CLASS_PRIEST * 10) + 8] + 5);
                        energy_use = 100;
                }

                else if (powers[i] == -26)
                {
                        godly_wrath();
                        energy_use = 100;
                }
                else if (powers[i] == -27)
                {
                        int invpower = p_ptr->abilities[(CLASS_ROGUE * 10)] + 10;
                        (void)set_invis(invpower, invpower);
                        p_ptr->ac_boost = p_ptr->abilities[(CLASS_ROGUE * 10)] * 10;
                        (void)set_ac_boost(invpower);
                        energy_use = 100;
                }
                else if (powers[i] == -28)
                {
                        set_spike_trap();
                        energy_use = 100;
                }
                else if (powers[i] == -29)
                {
                        assassin_poison_weapon();
                        energy_use = 100;
                }
                else if (powers[i] == -30)
                {
                        set_gas_trap();
                        energy_use = 100;
                }
                else if (powers[i] == -31)
                {
                        set_poison_trap();
                        energy_use = 100;
                }
                else if (powers[i] == -32)
                {
                        ranger_entangle();
                        energy_use = 100;
                }
                else if (powers[i] == -33)
                {
                        attack_aura(GF_ANIMAL_EMPATHY, 0, (p_ptr->abilities[(CLASS_RANGER * 10) + 3] / 10) + 5);
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -34)
                {
                        msg_print("You call an animal to your aid!");
                        summon_specific_friendly(py, px, p_ptr->abilities[(CLASS_RANGER * 10) + 4], SUMMON_ANIMAL, FALSE);
                        energy_use = 100;
                }
                else if (powers[i] == -35)
                {
                        warp_on_trees();
                        energy_use = 100;
                }
                else if (powers[i] == -36)
                {
                        call_dryad();
                        energy_use = 100;
                }
                else if (powers[i] == -37)
                {
                        ranger_thorned_vines();
                        energy_use = 100;
                }
                else if (powers[i] == -38)
                {
                        attack_aura(GF_SLEEP_POLLEN, 0, 3 + (p_ptr->abilities[(CLASS_RANGER * 10) + 8] / 10));
                        energy_use = 100;
                }
                else if (powers[i] == -39)
                {
                        ranger_force_of_nature();
                        energy_use = 100;
                }
                else if (powers[i] == -40)
                {
                        p_ptr->str_boost = (p_ptr->abilities[(CLASS_PALADIN * 10)] * 2);
                        (void)set_str_boost(5 + p_ptr->abilities[(CLASS_PALADIN * 10)]);
                        energy_use = 100;
                }
                else if (powers[i] == -41)
                {
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_LITE, dir, (p_ptr->abilities[(CLASS_PALADIN * 10) + 1] * 5) * p_ptr->lev);
                        energy_use = 100;                        
                }
                else if (powers[i] == -42)
                {
                        if (!get_rep_dir(&dir)) return;
                        smite_evil(dir);
                        energy_use = 100;
                }
                else if (powers[i] == -43)
                {
                        blade_of_purity();
                        energy_use = 100;
                }
                else if (powers[i] == -44)
                {
                        p_ptr->str_boost = (p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 5);
                        p_ptr->dex_boost = (p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 5);
                        p_ptr->ac_boost = p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 100;
                        (void)set_ac_boost(2);
                        (void)set_blessed(2);
                        (void)set_str_boost(2);
                        (void)set_dex_boost(2);
                        energy_use = 100;
                }
                else if (powers[i] == -45)
                {
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_RETROGRADE_DARKNESS, dir, 0, (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] / 10));                        
                        energy_use = 100;
                }
                else if (powers[i] == -46)
                {
                        paladin_shining_armor();
                        energy_use = 100;
                }
                else if (powers[i] == -47)
                {
                        word_of_peace();
                        energy_use = 100;
                }
                else if (powers[i] == -48)
                {
                        spin_kick(p_ptr->dis_to_d * p_ptr->abilities[(CLASS_MONK * 10) + 1], 1);
                        energy_use = 100;
                }
                else if (powers[i] == -49)
                {
                        if (!get_rep_dir(&dir)) return;
                        hard_kick(dir, p_ptr->dis_to_d * ((p_ptr->abilities[(CLASS_MONK * 10) + 2] /3)+1), 3 + (p_ptr->abilities[(CLASS_MONK * 10) + 2] / 10));
                        energy_use = 100;
                }
                else if (powers[i] == -50)
                {
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MISSILE, p_ptr->dis_to_d * ((p_ptr->abilities[(CLASS_MONK * 10) + 5] / 2)+1), 0, 1);
                        energy_use = 100;
                }
                else if (powers[i] == -51)
                {
                        int x, y;
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, (p_ptr->dis_to_d * p_ptr->abilities[(CLASS_MONK * 10) + 8]), 0, 1);
                        msg_print("Jump where? ");
                        if (!tgt_pt(&x,&y)) return;
                        if (!cave_empty_bold(y,x) || (cave[y][x].info & CAVE_ICKY) || (distance(y,x,py,px) > 3))
                        {
                                msg_print("You can't jump there...");
                        }
                        else
                        {
                                if (!(cave[y][x].info & CAVE_MARK))
                                {
                                        if (cave[y][x].info & CAVE_LITE) teleport_player_to(y,x);
                                        else msg_print("You can't jump there...");
                                }
                                else teleport_player_to(y,x);
                        }
                        energy_use = 100;
                }
                else if (powers[i] == -52)
                {
                        called_shots();
                }
                else if (powers[i] == -53)
                {
                        s32b dam;
                        object_type *o_ptr = &inventory[INVEN_AMMO];
                        object_type *q_ptr = &inventory[INVEN_BOW];
                        if (!q_ptr) {msg_print("You need a shooter!"); return;}
                        if (!o_ptr) {msg_print("You need ammos!"); return;}
                        dam = damroll(o_ptr->dd, o_ptr->ds) * p_ptr->skill_shooting;
                        dam += q_ptr->to_d + o_ptr->to_d;
                        dam *= p_ptr->smultiplier;
                        dam *= (p_ptr->abilities[(CLASS_ARCHER * 10) + 3] / 4)+1;
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, (p_ptr->abilities[(CLASS_ARCHER * 10) + 3] + 4));
                        inven_item_increase(INVEN_AMMO, -1);
                        inven_item_describe(INVEN_AMMO);
                        inven_item_optimize(INVEN_AMMO);
                        energy_use = 100;
                }
                else if (powers[i] == -54)
                {
                        do_cmd_fire(0, TRUE);
                }
                else if (powers[i] == -55)
                {
                        s32b dam;
                        object_type *o_ptr = &inventory[INVEN_AMMO];
                        object_type *q_ptr = &inventory[INVEN_BOW];
                        if (!q_ptr || (q_ptr->sval != SV_LIGHT_XBOW && q_ptr->sval != SV_HEAVY_XBOW)) {msg_print("You need a crossbow!"); return;}
                        if (o_ptr->tval != TV_BOLT) {msg_print("You need bolts!"); return;}
                        dam = damroll(o_ptr->dd, o_ptr->ds) * p_ptr->skill_shooting;
                        dam += q_ptr->to_d + o_ptr->to_d;
                        dam *= p_ptr->smultiplier;
                        dam *= (p_ptr->abilities[(CLASS_ARCHER * 10) + 7] / 2) + 1;
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir, dam, (p_ptr->abilities[(CLASS_ARCHER * 10) + 7] / 30)+1);
                        inven_item_increase(INVEN_AMMO, -1);
                        inven_item_describe(INVEN_AMMO);
                        inven_item_optimize(INVEN_AMMO);
                        energy_use = 100;
                }
                else if (powers[i] == -56)
                {
                        s32b dam = (p_ptr->abilities[(CLASS_ELEM_LORD * 10)] * 10) * p_ptr->lev;
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(p_ptr->elemlord, dir, dam, (p_ptr->abilities[(CLASS_ELEM_LORD * 10)] / 20)+1);                                                
                        energy_use = 100;
                }
                else if (powers[i] == -57)
                {
                        if (!get_rep_dir(&dir)) return;
                        element_strike(dir);
                        energy_use = 100;
                }
                else if (powers[i] == -58)
                {
                        (void)set_elem_shield(p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 2] + 9);
                        energy_use = 100;
                }
                else if (powers[i] == -59)
                {
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, p_ptr->elemlord, (p_ptr->to_d / 2) * ((p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 3])), p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 3] / 40, 1);
                        energy_use = 100;
                }
                else if (powers[i] == -60)
                {
                        explosive_throw();
                        energy_use = 100;
                }
                else if (powers[i] == -61)
                {
                        elem_wave();
                        energy_use = 100;
                }
                else if (powers[i] == -62)
                {
                        morph_memorize();
                        energy_use = 100;
                }
                else if (powers[i] == -63)
                {
                        morph_into_memorized();
                        lite_spot(py, px);
                        energy_use = 100;
                }
                else if (powers[i] == -64)
                {
                        p_ptr->body_monster = 0;
                        lite_spot(py, px);
                        msg_print("You return to your normal shape!");
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -65)
                {
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_DOMINATE_MONSTER, dir, 0);
                        energy_use = 100;
                }
                else if (powers[i] == -66)
                {
                        monstrous_wave();
                }
                else if (powers[i] == -67)
                {
                        s32b dam;
                        int dis;
                        object_type *o_ptr = &inventory[INVEN_ARM];
                        dam = ((o_ptr->ac * p_ptr->abilities[(CLASS_DEFENDER * 10) + 2]) * p_ptr->lev);
                        dam *= p_ptr->multiplier;
                        dis = 2 + (p_ptr->abilities[(CLASS_DEFENDER * 10) + 2] / 20);
                        if (!get_rep_dir(&dir)) return;
                        smash(dir, dam, dis);
                        energy_use = 100;
                }
                else if (powers[i] == -68)
                {
                        s32b dam;
                        dam = ((p_ptr->ac + p_ptr->to_a) * 100) * p_ptr->abilities[(CLASS_DEFENDER * 10) + 7];
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, 1);
                        energy_use = 100;
                }
                else if (powers[i] == -69)
                {
                        s32b dam;
                        object_type *o_ptr = &inventory[INVEN_ARM];
                        dam = ((o_ptr->ac + o_ptr->to_a) * p_ptr->skill_throwing) * p_ptr->abilities[(CLASS_DEFENDER * 10) + 8];
                        dam *= p_ptr->multiplier;
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_PHYSICAL, dir, dam);
                        energy_use = 100;
                }
                else if (powers[i] == -70)
                {
                        s32b dam = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10)] * 20) * p_ptr->lev;
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_SHATTER_EVIL, dir, dam, (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10)] / 30)+2);                                                
                        energy_use = 100;
                }
                else if (powers[i] == -71)
                {
                        attack_aura(GF_ANGELIC_VOICE, 0, 15);
                        energy_use = 100;
                }
                else if (powers[i] == -72)
                {
                        justice_bless_weapon();
                        energy_use = 100;
                }
                else if (powers[i] == -73)
                {
                        s32b dam;
                        dam = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 5] * 10) * p_ptr->lev;
                        attack_aura(GF_LITE, dam, 3 + (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 5] / 20));
                        energy_use = 100;
                }
                else if (powers[i] == -74)
                {
                        s32b dam;
                        dam = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 6] * 30) * p_ptr->lev;
                        if (!get_aim_dir(&dir)) return;
                        no_magic_return = TRUE;
                        fire_bolt(GF_SLAY_EVIL, dir, dam);
                        no_magic_return = FALSE;
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -75)
                {
                        int x;
                        msg_print("You call angels to your aid!");
                        for (x = 0; x < 15; x++)
                        {
                                summon_specific_friendly(py, px, p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 7], SUMMON_ANGEL, FALSE);
                        }
                        energy_use = 100;
                }
                else if (powers[i] == -76)
                {
                        s32b dam;
                        dam = (p_ptr->chp * p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 9]);
                        attack_aura(GF_LITE, dam, 5 + (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 9] / 30));
                        energy_use = 100;
                }
                                                          
                else if (powers[i] == -77)
                {
                        s32b dam;
                        dam = ((p_ptr->dis_to_d / 2) * p_ptr->abilities[(CLASS_ZELAR * 10)]);
                        attack_aura(GF_MANA, dam, 1 + (p_ptr->abilities[(CLASS_ZELAR * 10)] / 30));
                        energy_use = 100;
                }
                else if (powers[i] == -78)
                {
                        s32b dam;
                        int range = 10 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 1] / 2);
                        dam = ((p_ptr->dis_to_d) * p_ptr->abilities[(CLASS_ZELAR * 10) + 1]);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, dam, 0, range);
                        energy_use = 100;
                }
                else if (powers[i] == -79)
                {
                        s32b dam;
                        dam = ((p_ptr->dis_to_d) * p_ptr->abilities[(CLASS_ZELAR * 10) + 3]);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, dam, 0, 1);
                        energy_use = 100;
                }
                else if (powers[i] == -80)
                {
                        zelar_leg_throw();
                        energy_use = 100;
                }
                else if (powers[i] == -81)
                {
                        msg_print("You energize yourself!");
                        p_ptr->chp += ((10 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 6]*2)) * p_ptr->lev);
                        p_ptr->csp += ((5 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 6]) / 2) * p_ptr->lev);
                        if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                        if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
                        (void)set_cut(0);
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -82)
                {
                        {
                                s32b dam;
                                dam = ((p_ptr->dis_to_d / 2) * (p_ptr->abilities[(CLASS_ZELAR * 10) + 7]));
                                dir = get_a_dir();
                                chain_attack(dir, GF_MANA, dam, 0, 10);
                        }
                        {
                                s32b dam;
                                dam = ((p_ptr->dis_to_d / 2) * (p_ptr->abilities[(CLASS_ZELAR * 10) + 7]));
                                dir = get_a_dir();
                                chain_attack(dir, GF_MANA, dam, 0, 10);
                        }
                        energy_use = 100;
                }
                else if (powers[i] == -83)
                {
                        p_ptr->str_boost = (p_ptr->abilities[(CLASS_ZELAR * 10) + 8] * 20);
                        (void)set_str_boost(2);
                        energy_use = 100;
                }
                else if (powers[i] == -84)
                {
                        int x, y;
                        if (!tgt_pt(&x,&y)) return;
                        capture_soul(x, y);
                        energy_use = 100;
                }
                else if (powers[i] == -85)
                {
                        use_soul_power();
                }
                else if (powers[i] == -86)
                {
                        simulacrum();
                }
                else if (powers[i] == -87)
                {
                        soul_bind();
                }
                else if (powers[i] == -88)
                {
                        sealing_light();
                }
                else if (powers[i] == -89)
                {
                        soul_energize();
                }
                else if (powers[i] == -90)
                {
                        monster_race *r_ptr;
                        int m_idx;
                        s32b monhp;
                        s32b dam;
                        int item;
                        object_type *o_ptr;

                        cptr q, s;

                        /* Restrict choices to monsters */
                        item_tester_tval = TV_SOUL;

                        /* Get an item */
                        q = "Use which soul? ";
                        s = "You have no souls!.";
                        if (!get_item(&item, q, s, (USE_INVEN))) return;

                        o_ptr = &inventory[item]; 

                        /* Get the monster */
                        r_ptr = &r_info[o_ptr->pval];

                        monhp = maxroll(r_ptr->hdice, r_ptr->hside);
                        monhp = monhp + ((monhp * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 6] * 10)) / 100);

                        dam = (weapon_damages() / 2) + monhp;
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, dam, 0, 1);
                        energy_use = 100;
                        
                }
                else if (powers[i] == -91)
                {
                        monster_race *r_ptr;
                        int m_idx;
                        int item;
                        object_type *o_ptr;

                        cptr q, s;

                        /* Restrict choices to monsters */
                        item_tester_tval = TV_SOUL;

                        /* Get an item */
                        q = "Use which soul? ";
                        s = "You have no souls!.";
                        if (!get_item(&item, q, s, (USE_INVEN))) return;

                        o_ptr = &inventory[item]; 

                        if (o_ptr->timeout >= 1)
                        {
                                msg_print("This soul still need to recharge.");
                                return;
                        }

                        /* Get the monster */
                        r_ptr = &r_info[o_ptr->pval];

                        p_ptr->ac_boost = (r_ptr->ac / 2) * p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7];
                        p_ptr->mres = r_ptr->level + ((r_ptr->level * p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7]) / 100);
                        if (p_ptr->mres > 75) p_ptr->mres = 75;
                        (void)set_mres(p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7] + 10);
                        (void)set_ac_boost(p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7] + 10);

                        o_ptr->timeout = p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7] + 10;
                        energy_use = 100;
                }
                else if (powers[i] == -92)
                {
                        monster_race *r_ptr;
                        int m_idx;
                        s32b monhp;
                        s32b dam;
                        int rad;
                        int item;
                        object_type *o_ptr;

                        cptr q, s;

                        /* Restrict choices to monsters */
                        item_tester_tval = TV_SOUL;

                        /* Get an item */
                        q = "Use which soul? ";
                        s = "You have no souls!.";
                        if (!get_item(&item, q, s, (USE_INVEN))) return;

                        o_ptr = &inventory[item]; 

                        /* Get the monster */
                        r_ptr = &r_info[o_ptr->pval];

                        monhp = maxroll(r_ptr->hdice, r_ptr->hside);

                        dam = monhp;
                        dam *= p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 9];
                        rad = 2 + p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 9] / 30; 
                        if (!get_rep_dir(&dir)) return;
                        no_magic_return = TRUE;
                        chain_attack(dir, GF_MANA, dam, rad, 30);
                        no_magic_return = FALSE;

			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
                        energy_use = 100;                        
                }
                else if (powers[i] == -93)
                {
                        if (p_ptr->tim_invisible >= 1)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= 5;
                                dam *= (p_ptr->skill_stealth + 1);
                                dam += o_ptr->to_d;
                                dam *= p_ptr->multiplier;
                                dam *= p_ptr->abilities[(CLASS_SHADOW * 10)];
                                if (!get_rep_dir(&dir)) return;
                                chain_attack(dir, GF_STEALTH_ATTACK, dam, 0, 1);
                        }
                        else msg_print("You can only use this if you are using a temporary invisibility ability(such as Hide in Shadows or Shadow Cloak)");
                        energy_use = 100;
                }
                else if (powers[i] == -94)
                {
                        int invpower = (p_ptr->abilities[(CLASS_SHADOW * 10) + 2] * 3) + 20;
                        int duration = (p_ptr->abilities[(CLASS_SHADOW * 10) + 2] * 2) + 10;
                        (void)set_invis(duration, invpower);
                        update_and_handle();
                        energy_use = 100;
                }
                else if (powers[i] == -95)
                {
                        if (p_ptr->tim_invisible >= 1)
                        {
                                s32b dam = (p_ptr->skill_stealth * p_ptr->lev) + (((p_ptr->skill_stealth * p_ptr->lev) * (p_ptr->abilities[(CLASS_SHADOW * 10) + 3] * 10)) / 100);
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DARK, dir, dam, (p_ptr->abilities[(CLASS_SHADOW * 10) + 3] / 30)+2);                                                
                                energy_use = 100;
                        }
                        else msg_print("You can only use this if you are using a temporary invisibility ability(such as Hide in Shadows or Shadow Cloak)");
                }
                else if (powers[i] == -96)
                {
                        int x, y, rad;
                        rad = 3 + (p_ptr->abilities[(CLASS_SHADOW * 10) + 5] / 2);
                        msg_print("Phase where? ");
                        if (!tgt_pt(&x,&y)) return;
                        if (!cave_empty_bold(y,x) || (cave[y][x].info & CAVE_ICKY) || (distance(y,x,py,px) > rad))
                        {
                                msg_print("You can't warp there...");
                        }
                        else
                        {
                                if (!(cave[y][x].info & CAVE_MARK))
                                {
                                        if (cave[y][x].info & CAVE_LITE) msg_print("You can't warp there...");
                                        else
                                        {
                                                teleport_player_to(y,x);
                                                msg_print("You phase in shadows, at the price of your life...");
                                                take_hit(p_ptr->mhp / 4, "Shadow Phase ability");
                                        }
                                }
                                else msg_print("You can't warp there...");
                        }
                        energy_use = 100;
                        
                }
                else if (powers[i] == -97)
                {
                        dark_mist_ability();
                }
                else if (powers[i] == -98)
                {
                        if (p_ptr->tim_invisible >= 1)
                        {
                        
                        s32b dam;
                        int x, y, flg;

                        flg = PROJECT_GRID | PROJECT_KILL;

                        dam = (weapon_damages() / 2) * ((p_ptr->abilities[(CLASS_SHADOW * 10) + 9] / 3) + 1);
                        if (!tgt_pt(&x,&y)) return;
                        if (distance(y,x,py,px) > 3)
                        {
                                msg_print("You can't target that far.");
                                return;
                        }
                        else
                        {
                                project(0, 1, y, x, dam, GF_DARK, flg);
                                energy_use = 100;
                        }
                        }
                        else msg_print("You can only use this ability while under temporary invisibility!");
                }
                else if (powers[i] == -995)
                {
                        msg_format("You turn on your %s aura.", get_element_name(p_ptr->elemlord));
                        p_ptr->auraon = TRUE;
                }
                else if (powers[i] == -996)
                {
                        msg_format("You turn off your %s aura.", get_element_name(p_ptr->elemlord));
                        p_ptr->auraon = FALSE;
                }

                else if (powers[i] == -999)
                {
                        int dir,x,y;
                        cave_type *c_ptr;
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        object_type     *q_ptr;
                        object_type     forge;

                        if (!get_rep_dir(&dir)) return;
                        y = py + ddy[dir];
                        x = px + ddx[dir];
                        c_ptr = &cave[y][x];
                        if(c_ptr->m_idx)
                        {
                                m_ptr = &m_list[c_ptr->m_idx];
                                r_ptr = &r_info[m_ptr->r_idx];
                                if (is_pet(m_ptr))
                                {
                                        q_ptr=&forge;
                                        object_prep(q_ptr, lookup_kind(TV_HYPNOS, 1));
                                        q_ptr->number = 1;
                                        q_ptr->pval = m_ptr->r_idx;
                                        q_ptr->pval2 = m_ptr->hp;
                                        q_ptr->pval3 = m_ptr->level;
                                        q_ptr->xtra1 = m_ptr->maxhp;
                                        object_aware(q_ptr);
                                        object_known(q_ptr);

                                        q_ptr->ident |= IDENT_STOREB;
                        
                                        drop_near(q_ptr, 0,y,x);

                                        delete_monster(y,x);
                                        health_who = 0;
                                }
                                else
                                        msg_print("The monster must be in your gang!");
                        }
                        else msg_print("There is no pet here !");
                }

        }

	else
	{
                        /* The feats! */
                        if (powers[i] == 100)
                        {
                                sword_spin();
                                energy_use = 100;
                        }
                        else if (powers[i] == 101)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= (p_ptr->skill_hafted + 1);
                                dam += p_ptr->to_d;
                                dam *= p_ptr->multiplier;
                                dam *= 2;
                                if (!get_rep_dir(&dir)) return;
                                smash(dir, dam, 1);
                                energy_use = 100;
                        }
                        else if (powers[i] == 102)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= (p_ptr->skill_hafted + 1);
                                dam += p_ptr->to_d;
                                dam *= p_ptr->multiplier;
                                dam *= 4;
                                if (!get_rep_dir(&dir)) return;
                                dizzy_smash(dir, dam, 1);
                                energy_use = 100;
                        }                
                        else if (powers[i] == 103)
                        {
                                if (!get_rep_dir(&dir)) return;
                                shattering_blow(dir);
                                energy_use = 100;
                        }
                        else if (powers[i] == 104)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= (p_ptr->skill_polearms + 1);
                                dam += p_ptr->to_d;
                                dam *= p_ptr->multiplier;
                                dam *= 2;
                                if (!get_rep_dir(&dir)) return;
                                chain_attack(dir, GF_PHYSICAL, dam, 0, 2);
                                energy_use = 100;
                        }
                        else if (powers[i] == 105)
                        {
                                sharpen_ammos();
                                energy_use = 100;
                        }
                        else if (powers[i] == 106)
                        {
                                if (!get_rep_dir(&dir)) return;
                                power_punch(dir);
                                energy_use = 100;
                        }
                        else if (powers[i] == 107)
                        {
                                if (!get_rep_dir(&dir)) return;
                                stunning_blow(dir);
                                energy_use = 100;
                        }
                        else if (powers[i] == 108)
                        {
                                agility_jump();
                                energy_use = 100;
                        }
                        else if (powers[i] == 109)
                        {
                                msg_print("You hide nearby...");
                                (void)set_invis(3, 5);
                                energy_use = 100;
                        }
                        else if (powers[i] == 110)
                        {
                                teleport_player(10);
                                energy_use = 100;
                        }
                        else if (powers[i] == 111)
                        {
                                enchanted_blood();
                                energy_use = 100;
                        }
                        else if (powers[i] == 112)
                        {
                                mana_shield();
                                energy_use = 100;
                        }
                        else if (powers[i] == 113)
                        {
                                if (!get_rep_dir(&dir)) return;
                                eye_stab(dir);
                                energy_use = 100;
                        }
                        else if (powers[i] == 114)
                        {
                                dagger_fatal_stab();
                                energy_use = 100;
                        }
                        else if (powers[i] == 115)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= (p_ptr->skill_axes + 1);
                                dam += p_ptr->to_d;
                                dam *= p_ptr->multiplier;
                                dam *= 5;
                                if (!get_aim_dir(&dir)) return;
                                axe_chop(dir, dam);
                                energy_use = 100;
                        }
                        else if (powers[i] == 116)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= (p_ptr->skill_axes + 1);
                                dam += p_ptr->to_d;
                                dam *= p_ptr->multiplier;
                                dam *= 2;
                                if (!get_aim_dir(&dir)) return;
                                mutilate_legs(dir, dam);
                                energy_use = 100;
                        }
                        else if (powers[i] == 117)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= (p_ptr->skill_axes + 1);
                                dam += p_ptr->to_d;
                                dam *= p_ptr->multiplier;
                                dam *= 2;
                                if (!get_aim_dir(&dir)) return;
                                mutilate_arms(dir, dam);
                                energy_use = 100;
                        }
                        else if (powers[i] == 118)
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_OLD_HEAL, dir, (p_ptr->lev * 10));
                                energy_use = 100;                                
                        }
                        else if (powers[i] == 119)
                        {
                                attack_aura(GF_MORALE_BOOST, 0, 3);
                                energy_use = 100;
                        }
                        else if (powers[i] == 120)
                        {
                                combine_items();
                                energy_use = 100;
                        }
                        else if (powers[i] == 121)
                        {
                                decompose_item();
                                energy_use = 100;
                        }

                else {
                
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
						p_ptr->csp += 2 * lev;
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
                                                py_attack(y, x, -1);
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
                                        if (dun_level && (max_dlv[dungeon_type] > dun_level))
					{
						if (get_check("Reset recall depth? "))
                                                        max_dlv[dungeon_type] = dun_level;
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
	}

        /* One last thing... */
        /* Combat feats don't take a move here. Fear their power! :) */
        /* if (combat_feat && energy_use >= 100) energy_use -= 100; */

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
    py_attack(y, x, -1);
  }

	/* Exit the area */
        else if ((!dun_level) && (!p_ptr->wild_mode) &&
		((x == 0) || (x == cur_wid-1) ||
		 (y == 0) || (y == cur_hgt-1)))
	{
		/* Can the player enter the grid? */
		if (player_can_enter(c_ptr->mimic))
		{
			/* Hack: move to new area */
			if ((y == 0) && (x == 0))
			{
				p_ptr->wilderness_y--;
				p_ptr->wilderness_x--;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = cur_wid - 2;
                                ambush_flag = FALSE;
			}

			else if ((y == 0) && (x == MAX_WID-1))
			{
				p_ptr->wilderness_y--;
				p_ptr->wilderness_x++;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = 1;
                                ambush_flag = FALSE;
			}

			else if ((y == MAX_HGT-1) && (x == 0))
			{
				p_ptr->wilderness_y++;
				p_ptr->wilderness_x--;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = cur_wid - 2;
                                ambush_flag = FALSE;
			}

			else if ((y == MAX_HGT-1) && (x == MAX_WID-1))
			{
				p_ptr->wilderness_y++;
				p_ptr->wilderness_x++;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = 1;
                                ambush_flag = FALSE;
			}

			else if (y == 0)
			{
				p_ptr->wilderness_y--;
				p_ptr->oldpy = cur_hgt - 2;
				p_ptr->oldpx = x;
                                ambush_flag = FALSE;
			}

			else if (y == cur_hgt-1) 
			{
				p_ptr->wilderness_y++;
				p_ptr->oldpy = 1;
				p_ptr->oldpx = x;
                                ambush_flag = FALSE;
			}

			else if (x == 0) 
			{
				p_ptr->wilderness_x--;
				p_ptr->oldpx = cur_wid - 2;
				p_ptr->oldpy = y;
                                ambush_flag = FALSE;
			}

			else if (x == cur_wid-1) 
			{
				p_ptr->wilderness_x++;
				p_ptr->oldpx = 1;
				p_ptr->oldpy = y;
                                ambush_flag = FALSE;
			}

			p_ptr->leftbldg = TRUE;
			p_ptr->leaving = TRUE;

			return;
		}
	}

  /* Hack -- Ignore weird terrain types. */
  else if (!cave_floor_grid(c_ptr)) {
    teleport_player(10);
  }

  /* Enter quests */
  else if (((feat >= FEAT_QUEST_ENTER) && (feat <= FEAT_QUEST_UP)) ||
           ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL)) ||
           ((feat >= FEAT_LESS) && (feat <= FEAT_MORE))) {
    move_player(dir, FALSE);
    more = FALSE;
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
#if 0
    if (dun_level >= d_info[dungeon_type].maxdepth) {
      msg_print("The floor is impermeable.");
      return FALSE;
    }
#endif

    msg_print("You sink through the floor.");
    dun_level++;
    p_ptr->leaving = TRUE;
  } else {
#if 0
    if (!dun_level) {
      msg_print("The only thing above you is air.");
      return FALSE;
    }
#endif

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
    msg_format("You hear the voice of %s: 'You want to serve me %s? Very well then, but you'd better be strong!",
            deity_info[p_ptr->pgod-1].name, player_name);


  } else if (p_ptr->pgod != what_god) {
    msg_format("%s thunders in outrage at your blasphemy!",
            deity_info[p_ptr->pgod-1].name);

    set_grace(p_ptr->grace - val*10);
    (void) do_dec_stat(A_WIS, STAT_DEC_NORMAL);
    p_ptr->update |= PU_BONUS;

    if (val > 2500) {
      msg_format("You feel %s abandon you.",
              deity_info[p_ptr->pgod-1].name);

      p_ptr->pgod = what_god;
      set_grace(val);
      p_ptr->god_favor = -60000;
    }

  } else {
    if (o_ptr->name2 == EGO_BLESS_BLADE && p_ptr->pgod == GOD_GOD)
    {
        msg_print("How dare you sacrifice a weapon blessed by me? I wont forget it easily!");
        set_grace(p_ptr->grace - 50000);
    }
    else set_grace(p_ptr->grace + val * 3);
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
                msg_print("You have not chosen a second rune!");
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

        /* Use the spell multiplicator */
        power *= (p_ptr->to_s)?p_ptr->to_s:1;

        /* To reduce the high level powr, while increasing the low levels */
        powerdiv = power / (2 + (p_ptr->lev / 25));

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
        {TR1_INFRA,6,"Infravision"},
        {TR1_INFRA,6,"Infravision"},
        {TR1_INFRA,6,"Infravision"},
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

        s32b max_lev = (p_ptr->lev * 5000);

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
                        msg_print("Not enough energy to enchant more than one object!");
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
                msg_print("You fail to create the artifact, the powerful forces in it are released!");
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

  /*
   * scan_monst --
   *
   * Return a list of o_list[] indexes of items of the given monster
   */
  bool scan_monst(int *items, int *item_num, int m_idx)
  {
        int this_o_idx, next_o_idx;
  
        int num = 0;
   
        (*item_num) = 0;
  
        /* Scan all objects in the grid */
        for (this_o_idx = m_list[m_idx].hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
                object_type *o_ptr;
  
                /* Acquire object */
                o_ptr = &o_list[this_o_idx];
  
                /* Acquire next object */
                next_o_idx = o_ptr->next_o_idx;
  
                /* Accept this item */
                items[num++] = this_o_idx;
         
                /* XXX Hack -- Enforce limit */
                if (num == 23) break;
        }
  
        /* Number of items */
        (*item_num) = num;
   
        /* Result */
        return (num != 0);
  }

  /*
   * Display a list of the items that the given monster carries.
   */
  byte show_monster_inven(int m_idx, int *monst_list)
  {
        int i, j, k, l;
        int col, len, lim;
  
        object_type *o_ptr;
  
        char o_name[80];
  
        char tmp_val[80];
  
        int out_index[23];
        byte out_color[23];
        char out_desc[23][80];
  
        int monst_num;
   
        /* Default length */
        len = 79 - 50;
  
        /* Maximum space allowed for descriptions */
        lim = 79 - 3;
  
        /* Require space for weight (if needed) */
        if (show_weights) lim -= 9;
  
        /* Scan for objects on the monster */
        (void) scan_monst(monst_list, &monst_num, m_idx);
  
        /* Display the inventory */
        for (k = 0, i = 0; i < monst_num; i++)
        {
                o_ptr = &o_list[monst_list[i]];
  
                /* Describe the object */
                object_desc(o_name, o_ptr, TRUE, 3);
  
                /* Hack -- enforce max length */
                o_name[lim] = '\0';
  
                /* Save the index */
                out_index[k] = i;
  
                /* Acquire inventory color */
                out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];
  
                /* Save the object description */
                strcpy(out_desc[k], o_name);
  
                /* Find the predicted "line length" */
                l = strlen(out_desc[k]) + 5;
  
                /* Be sure to account for the weight */
                if (show_weights) l += 9;
  
                /* Maintain the maximum length */
                if (l > len) len = l;
  
                /* Advance to next "line" */
                k++;
        }
  
        /* Find the column to start in */
        col = (len > 76) ? 0 : (79 - len);
  
        /* Output each entry */
        for (j = 0; j < k; j++)
        {
                /* Get the index */
                i = monst_list[out_index[j]];
  
                /* Get the item */
                o_ptr = &o_list[i];
  
                /* Clear the line */
                prt("", j + 1, col ? col - 2 : col);
  
                /* Prepare an index --(-- */
                sprintf(tmp_val, "%c)", index_to_label(j));
  
                /* Clear the line with the (possibly indented) index */
                put_str(tmp_val, j + 1, col);
  
                /* Display the entry itself */
                c_put_str(out_color[j], out_desc[j], j + 1, col + 3);
  
                /* Display the weight if needed */
                if (show_weights)
                {
                        int wgt = o_ptr->weight * o_ptr->number;
                        sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
                        put_str(tmp_val, j + 1, 71);
                }
        }
  
        /* Make a "shadow" below the list (only if needed) */
        if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

        return monst_num;
  }


/*
 * Steal an object from a monster
 */
void do_cmd_steal()
{
        int x, y, dir = 0, item = -1, k = -1;

        cave_type *c_ptr;

        monster_type *m_ptr;

        object_type *o_ptr, forge;

        byte num = 0;

        bool done = FALSE;

        int monst_list[23];

        /* Only works on adjacent monsters */
        if (!get_rep_dir(&dir)) return;
        y = py + ddy[dir];
        x = px + ddx[dir];
        c_ptr = &cave[y][x];

        if (!(c_ptr->m_idx))
        {
                msg_print("There is no monster there!");
                return;
        }

        m_ptr = &m_list[c_ptr->m_idx];

        /* There were no non-gold items */
        if (!m_ptr->hold_o_idx)
        {
                msg_print("That monster has no objects!");
                return;
        }

        screen_save();

        num = show_monster_inven(c_ptr->m_idx, monst_list);

        /* Repeat until done */
        while (!done)
        {
                char tmp_val[80];
                char which = ' ';

                /* Build the prompt */
                sprintf(tmp_val, "Choose an item to steal (a-%c) or ESC:", 'a' - 1 + num);
  
                /* Show the prompt */
                prt(tmp_val, 0, 0);
  
                /* Get a key */
                which = inkey();
  
                /* Parse it */
                switch (which)
                {
                        case ESCAPE:
                        {
                                done = TRUE;
                                break;
                        }
  
                        default:
                        {
                                int ver;
  
                                /* Extract "query" setting */
                                ver = isupper(which);
                                which = tolower(which);
  
                                k = islower(which) ? A2I(which) : -1;
                                if (k < 0 || k >= num)
                                {
                                        bell();
                                        break;
                                }
  
                                /* Verify the item */
                                if (ver && !verify("Try", 0 - monst_list[k]))
                                {
                                        done = TRUE;
                                        break;
                                }
  
                                /* Accept that choice */
                                item = monst_list[k];
                                done = TRUE;
                                break;
                        }
                }
        }

        if(item != -1)
        {
                int stealmod = p_ptr->abilities[(CLASS_ROGUE + 10) + 3] / 3;
                /* Failure check */
                if(rand_int((40 - p_ptr->stat_ind[A_DEX] - stealmod) + (o_list[item].weight / ((p_ptr->pclass == CLASS_ROGUE)?30:5)) + ((p_ptr->pclass != CLASS_ROGUE)?25:0) - ((m_ptr->csleep)?10:0) + r_info[m_ptr->r_idx].level) > 10)
                {
                        /* Take a turn */
                        energy_use = 100;

                        /* Wake up */
                        m_ptr->csleep = 0;

                        /* Speed up because monsters are ANGRY when you try to thief them */
                        m_ptr->mspeed += 5;

                        screen_load();

                        msg_print("Oops ! The monster is now really *ANGRY*.");

                        return;
                }

                /* Reconnect the objects list */
                if(k > 0) o_list[monst_list[k - 1]].next_o_idx = monst_list[k + 1];
                if(k + 1 >= num) o_list[monst_list[k - 1]].next_o_idx = 0;
                if(k == 0) m_ptr->hold_o_idx = monst_list[k + 1];
                if(num == 1) m_ptr->hold_o_idx = 0;

                /* Rogues gain some xp */
                if(p_ptr->pclass == CLASS_ROGUE)
                {
                        gain_exp((randint((o_list[item].weight / 2) + (r_info[m_ptr->r_idx].level * 10)) / 2) + (((o_list[item].weight / 2) + (r_info[m_ptr->r_idx].level * 10)) / 2));
                        if(get_check("Phase door now ?")) teleport_player(10);
                }

                /* Get the item */
                o_ptr = &forge;

                /* Special handling for gold */
                if(o_list[item].tval == TV_GOLD)
                {
                        /* Collect the gold */
                        p_ptr->au += o_list[item].pval;
  
                        /* Redraw gold */
                        p_ptr->redraw |= (PR_GOLD);
  
                        /* Window stuff */
                        p_ptr->window |= (PW_PLAYER);
                }
                else
                {
                        object_copy(o_ptr, &o_list[item]);

                        inven_carry(o_ptr, FALSE);
                }

                /* Delete it */
                o_list[item].k_idx = 0;
        }

        screen_load();

	/* Take a turn */
	energy_use = 100;
}

/* A new fun function! */
void change_age(void)
{
        char ch = 0;
        if (!get_com("Modify your age? [Y]es, [N]o ", &ch))
        {
                return;
        }

        if (ch == 'Y' || ch == 'y')
        {
                p_ptr->age = get_quantity("Enter your new age: ", 255);
                /* Must be at least 1 year old */
                if (p_ptr->age < 1) p_ptr->age = 1;
        }
}

void turn_in_crystal(monster_type *m_ptr)
{
                        int x,y;
                        cave_type *c_ptr;
                        monster_race *r_ptr;
                        object_type     *q_ptr;
                        object_type     forge;

                        y = m_ptr->fy;
                        x = m_ptr->fx;
                        c_ptr = &cave[y][x];
                        if(c_ptr->m_idx)
                        {
                                m_ptr = &m_list[c_ptr->m_idx];
                                r_ptr = &r_info[m_ptr->r_idx];
                                if (is_pet(m_ptr))
                                {
                                        q_ptr=&forge;
                                        object_prep(q_ptr, lookup_kind(TV_HYPNOS, 1));
                                        q_ptr->number = 1;
                                        q_ptr->pval = m_ptr->r_idx;
                                        q_ptr->pval2 = m_ptr->hp;
                                        q_ptr->pval3 = m_ptr->level;
                                        q_ptr->xtra1 = m_ptr->maxhp;
                                        object_aware(q_ptr);
                                        object_known(q_ptr);

                                        q_ptr->ident |= IDENT_STOREB;
                        
                                        drop_near(q_ptr, 0,y,x);

                                        delete_monster(y,x);
                                        health_who = 0;
                                }
                                else
                                        msg_print("The monster must be in your gang!");
                        }
}

void accurate_teleport()
{
                        int ii, ij;
                        msg_print("You teleport...");
                        if (!tgt_pt(&ii,&ij)) return;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY))
                        {
                                msg_print("Target obstructed.");
                        }
                        else
                        {
                                teleport_player_to(ij, ii);
                        }        
}        

/* Let's face it... reviving dead monsters is fun! */
void revive_monster()
{
        int x, y;
        cave_type *c_ptr;
        object_type *o_ptr;
        msg_print("Point the location of the corpse...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("No corpse here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (o_ptr->tval != TV_CORPSE)
        {
                msg_print("No corpse here!");
                return;
        }
        else
        {
                if (o_ptr->sval != 1)
                {
                        msg_print("You must use a full corpse!");
                        return;
                }
                place_monster_one_return_no_boss(y, x, o_ptr->pval2, FALSE, TRUE, p_ptr->lev * 2, o_ptr->pval3, o_ptr->pval3);
                msg_print("You revive the dead monster!");
                delete_object_idx(c_ptr->o_idx);
                update_and_handle();
        }
}

/* Turn the corpse into a Skeleton Minion! */
void raise_skeleton()
{
        int x, y;
        cave_type *c_ptr;
        object_type *o_ptr;
        if (p_ptr->csp < 5)
        {
                msg_print("You need at least 5 mana points!");
                return;
        }
        msg_print("Point the location of the corpse...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("No corpse here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (o_ptr->tval != TV_CORPSE)
        {
                msg_print("No corpse here!");
                return;
        }
        else
        {
                if (o_ptr->sval != 1)
                {
                        msg_print("You must use a full corpse!");
                        return;
                }
                place_monster_one_return_no_boss(y, x, 1115, FALSE, TRUE, p_ptr->lev * 2, o_ptr->pval3, o_ptr->pval3);
                msg_print("You raise a skeleton!");
                delete_object_idx(c_ptr->o_idx);
                p_ptr->csp -= 5;
                update_and_handle();
        }
}

/* Turn the corpse into a Bone Wall! */
void raise_bonewall()
{
        int x, y;
        s32b wallhp = 0;
        cave_type *c_ptr;
        object_type *o_ptr;
        if (p_ptr->csp < 30)
        {
                msg_print("You need at least 30 mana points!");
                return;
        }
        msg_print("Point the location of the corpse...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("No corpse here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (o_ptr->tval != TV_CORPSE)
        {
                msg_print("No corpse here!");
                return;
        }
        else
        {
                if (o_ptr->sval != 1)
                {
                        msg_print("You must use a full corpse!");
                        return;
                }
                wallhp = o_ptr->pval3 * 5;
                if (wallhp > 1000000 || wallhp < o_ptr->pval3) wallhp = o_ptr->pval3;
                place_monster_one_return_no_boss(y, x, 1116, FALSE, TRUE, 1, wallhp, wallhp);
                msg_print("You raise a wall of bones!");
                delete_object_idx(c_ptr->o_idx);
                p_ptr->csp -= 30;
                update_and_handle();
        }
}

/* Similar to Revive Monster spell, but better, beacause necros are specialists... */
/* The good thing about this is that it is 100% better than the spell, */
/* because it cost a bit less mana, and it revive monsters with full hp! */ 
void necro_revive_monster()
{
        int x, y;
        cave_type *c_ptr;
        object_type *o_ptr;
        if (p_ptr->csp < 75)
        {
                msg_print("You need at least 75 mana points!");
                return;
        }
        msg_print("Point the location of the corpse...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("No corpse here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (o_ptr->tval != TV_CORPSE)
        {
                msg_print("No corpse here!");
                return;
        }
        else
        {
                if (o_ptr->sval != 1)
                {
                        msg_print("You must use a full corpse!");
                        return;
                }
                place_monster_one_return_no_boss(y, x, o_ptr->pval2, FALSE, TRUE, p_ptr->lev * 2, o_ptr->pval3 * 2, o_ptr->pval3 * 2);
                msg_print("You revive the dead monster!");
                delete_object_idx(c_ptr->o_idx);
                p_ptr->csp -= 75;
                update_and_handle();
        }
}

/* Background Editor!! */
void change_background(void)
{
        char ch = 0;
        if (!get_com("Edit background? [Y]es, [N]o ", &ch))
        {
                return;
        }

        if (ch == 'Y' || ch == 'y')
        {
                edit_background();
        }
}

/* Calculate your ability to dig trough walls */
int digging_ability()
{
        int abil = 0;
        object_type *o_ptr = &inventory[INVEN_TOOL];

        abil = p_ptr->stat_ind[A_STR];
        /* Your digging ability is better if you have a good digger! */
        if (o_ptr->tval == TV_DIGGING) abil *= (o_ptr->pval + 2);

        return (abil);
}

/* Agility jump! */
void agility_jump(void)
{
          int     ii = 0, ij = 0;
          msg_print("You jump very high!");
          if (!tgt_pt(&ii,&ij)) return;
          if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (distance(ij,ii,py,px) > 1 + (p_ptr->skill_agility / 10)))
          {
                msg_print("You can't jump there...");
          }
          else
          {
                if (!(cave[ij][ii].info & CAVE_MARK))
                {
                        if (cave[ij][ii].info & CAVE_LITE) teleport_player_to(ij,ii);
                        else msg_print("You can't jump there...");
                }
                else teleport_player_to(ij,ii);
          }
          energy_use = 100;
}

/* Awaken a monster from a soul! */
/* The monster will be three times your level! */
void simulacrum()
{
        monster_race *r_ptr;
        int m_idx, monlvl, snum;
        s32b monhp;
        int item,x,y,i;
        object_type *o_ptr;

        cptr q, s;

        /* Restrict choices to monsters */
        item_tester_tval = TV_SOUL;

        /* Get an item */
        q = "Use which soul? ";
        s = "You have no souls!.";
        if (!get_item(&item, q, s, (USE_INVEN))) return;

        o_ptr = &inventory[item]; 

        if (o_ptr->timeout >= 1)
        {
                msg_print("This soul still need to recharge.");
                return;
        }

        /* Get the monster */
        r_ptr = &r_info[o_ptr->pval];

        /* Calculate monster's hp */
        r_ptr = &r_info[o_ptr->pval];

        monhp = maxroll(r_ptr->hdice, r_ptr->hside);
        monlvl = p_ptr->lev + (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 1] * 2);
        monhp *= monlvl;
        snum = 1 + (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 1] / 10);


        for (i = 0; i < snum; i++)
        {
                msg_print("Place where?");
                if (!tgt_pt(&x,&y)) return;
                if((m_idx=place_monster_one_simulacrum(y, x, o_ptr->pval, FALSE, TRUE, monlvl, monhp))==0) return;
        }

        o_ptr->timeout = 10;

        /*inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);*/
        update_and_handle();
        energy_use = 100;
}        

/* Ranger's Warp On Trees */
void warp_on_trees()
{
          int     ii = 0, ij = 0;
          msg_print("Teleport to which tree?");
          if (!tgt_pt(&ii,&ij)) return;
          if ((cave[ij][ii].feat != FEAT_TREES) || (distance(ij,ii,py,px) > 5 + (p_ptr->abilities[(CLASS_RANGER * 10) + 5] / 2)))
          {
                msg_print("You can't teleport there...");
          }
          else teleport_player_to(ij,ii);
}

/* Called Shots! :) */
void called_shots()
{
	int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];

        /* List the powers */
        strcpy(power_desc[num],"Legs");powers[num++]=CALLED_LEGS;
        strcpy(power_desc[num],"Arms");powers[num++]=CALLED_ARMS;
        strcpy(power_desc[num],"Head");powers[num++]=CALLED_HEAD;

        if(!num) {msg_print("No available called shots.");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Called Shots %c-%c, *=List, ESC=exit) Which body part? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Called Shots %c-%c, *=List, ESC=exit) Which body part? ",
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

                                while (ctr < num && ctr < 19)
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
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
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

        /* Called shot! */
        do_cmd_fire(Power, FALSE);
}

int get_a_dir()
{
        char ch;        
        if (!get_com("Direction? (USE NUMERICAL KEYPAD)", &ch)) return(5);
        else
        {
                if (ch == '1')
                {
                        return(1);
                }
                if (ch == '2')
                {
                        return(2);
                }
                if (ch == '3')
                {
                        return(3);
                }
                if (ch == '4')
                {
                        return(4);
                }
                if (ch == '4')
                {
                        return(4);
                }
                if (ch == '6')
                {
                        return(6);
                }
                if (ch == '7')
                {
                        return(7);
                }
                if (ch == '8')
                {
                        return(8);
                }
                if (ch == '9')
                {
                        return(9);
                }
        }
        return(5);
}

void sealing_light()
{
        monster_race *r_ptr;
        int item, dir;
        object_type *o_ptr;

        cptr q, s;

        /* Restrict choices to monsters */
        item_tester_tval = TV_SOUL;

        /* Get an item */
        q = "Use which soul? ";
        s = "You have no souls!.";
        if (!get_item(&item, q, s, (USE_INVEN))) return;

        o_ptr = &inventory[item]; 

        if (o_ptr->timeout >= 1)
        {
                msg_print("This soul still need to recharge.");
                return;
        }

        /* Get the monster */
        r_ptr = &r_info[o_ptr->pval];

        o_ptr->timeout = 5;

        if (!get_aim_dir(&dir)) return;
        no_magic_return = TRUE;
        fire_ball(GF_LITE_WEAK, dir, 0, 5 + p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 3]);
        fire_ball(GF_SEAL_LIGHT, dir, r_ptr->level, 5 + p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 3]);
        no_magic_return = FALSE;
        energy_use = 100;
}

void soul_energize()
{
        monster_race *r_ptr;
        int item;
        s32b amt;
        object_type *o_ptr;

        cptr q, s;

        /* Restrict choices to monsters */
        item_tester_tval = TV_SOUL;

        /* Get an item */
        q = "Use which soul? ";
        s = "You have no souls!.";
        if (!get_item(&item, q, s, (USE_INVEN))) return;

        o_ptr = &inventory[item]; 

        if (o_ptr->timeout >= 1)
        {
                msg_print("This soul still need to recharge.");
                return;
        }

        /* Get the monster */
        r_ptr = &r_info[o_ptr->pval];
        amt = maxroll(r_ptr->hdice, r_ptr->hside) / 3;
        amt = amt + ((amt * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 4] * 10)) / 100);
        p_ptr->csp += amt;
        if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
        msg_print("You recover your mana!");

        o_ptr->timeout = 15;
        update_and_handle();
        energy_use = 100;
}

