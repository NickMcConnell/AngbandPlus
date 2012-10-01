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
                        /* Are we leaving the quest? */
			if (c_ptr->event == 5)
			{
				/* If the event has a condition, ressolve it. */
				if (c_ptr->eventcond > 0)
				{
					if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
					{
						dun_level = 1;
						p_ptr->inside_quest = 0;
						p_ptr->startx = c_ptr->eventtype;
						p_ptr->starty = c_ptr->eventextra;
						/* Set events */
						p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
					}
					else
					{
						msg_print("You cannot leave right now.");
						return;
					}
				}
				else
				{
					dun_level = 1;
					p_ptr->inside_quest = 0;
					p_ptr->startx = c_ptr->eventtype;
					p_ptr->starty = c_ptr->eventextra;
					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
				}
			}
                }
  
                /* Create a way back */
                if (go_up_many)
                        create_down_shaft = TRUE;
                else
                create_down_stair = TRUE;
  
                /* New depth */
                if (go_up)
		{
                        dun_level--;
			/* Leave a quest if no event 5 is found... */
			p_ptr->inside_quest = 0;
		}
                else
                {
                        dun_level -= randint(3) + 1;
                        if (dun_level <= 0) dun_level = 0;
			/* Leave a quest if no event 5 is found... */
			p_ptr->inside_quest = 0;
                }
		/* If we're in a random dungeon, return to the wilderness */
		if (dungeon_type == 200 && dun_level <= 0)
		{
			p_ptr->wild_mode = 1;
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
		/* Save the position when we go back in town. */
		if (!dun_level)
		{
  			p_ptr->startx = px;
			p_ptr->starty = py;
		}
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

                /*if(c_ptr->special)
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
                }*/
		/* Are we entering a dungeon? */
		if (c_ptr->event == 2)
		{
			/* If the event has a condition, ressolve it. */
			if (c_ptr->eventcond > 0)
			{
				if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
				{
					dungeon_type = c_ptr->eventtype;
					p_ptr->recall_dungeon = dungeon_type;
					dun_level = d_info[dungeon_type].mindepth;
					msg_format("You go into %s", d_text + d_info[dungeon_type].text);
					if (c_ptr->eventextra != 0) p_ptr->startx = c_ptr->eventextra;
					if (c_ptr->eventextra2 != 0) p_ptr->starty = c_ptr->eventextra2;
					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
				}
				else
				{
					msg_print("You cannot enter this dungeon.");
					return;
				}
			}
			else
			{
				dungeon_type = c_ptr->eventtype;
				p_ptr->recall_dungeon = dungeon_type;
				dun_level = d_info[dungeon_type].mindepth;
				if (c_ptr->eventtype == 200)
				{
					p_ptr->wild_startx = px;
					p_ptr->wild_starty = py;
					msg_print("You go into an unknown dungeon...");
				}
				else msg_format("You go into %s", d_text + d_info[dungeon_type].text);
				if (c_ptr->eventextra != 0) p_ptr->startx = c_ptr->eventextra;
				if (c_ptr->eventextra2 != 0) p_ptr->starty = c_ptr->eventextra2;
				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
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

	if (c_ptr->event == 3)
	{
		/* If the event has a condition, ressolve it. */
		if (c_ptr->eventcond > 0)
		{
			if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
			{
				int j = 0;
				object_type *o_ptr;
				bool canenter = FALSE;
        			while (j <= 52)
        			{
                			/* Get the item */
                			o_ptr = &inventory[j];

                			if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
					{
						canenter = TRUE;
						j = 52;
					}

                			j++;
        			}
				if (!(canenter))
				{
					msg_print("You cannot open this door.");
					return FALSE;
				}

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
			}
		}
		else
		{
			int j = 0;
			object_type *o_ptr;
			bool canenter = FALSE;
        		while (j <= 52)
        		{
                		/* Get the item */
                		o_ptr = &inventory[j];

                		if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
				{
					canenter = TRUE;
					j = 52;
				}

                		j++;
        		}
			if (!(canenter))
			{
				msg_print("You cannot open this door.");
				return FALSE;
			}

			/* Set events */
			p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
		}
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
                        /*py_attack(y, x, -1);*/
			call_lua("py_attack", "(ddd)", "", y, x, -1);
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
                        /*py_attack(y, x, -1);*/
			call_lua("py_attack", "(ddd)", "", y, x, -1);
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
                        /*py_attack(y, x, -1);*/
			call_lua("py_attack", "(ddd)", "", y, x, -1);
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

	/* There may be an event... */
	if (c_ptr->event == 3)
	{
		/* If the event has a condition, ressolve it. */
		if (c_ptr->eventcond > 0)
		{
			if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
			{
				int j = 0;
				object_type *o_ptr;
				bool canenter = FALSE;
        			while (j <= 52)
        			{
                			/* Get the item */
                			o_ptr = &inventory[j];

                			if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
					{
						canenter = TRUE;
						j = 52;
					}

                			j++;
        			}
				if (!(canenter))
				{
					msg_print("You cannot open this door.");
					return FALSE;
				}

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
			}
		}
		else
		{
			int j = 0;
			object_type *o_ptr;
			bool canenter = FALSE;
        		while (j <= 52)
        		{
                		/* Get the item */
                		o_ptr = &inventory[j];

                		if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
				{
					canenter = TRUE;
					j = 52;
				}

                		j++;
        		}
			if (!(canenter))
			{
				msg_print("You cannot open this door.");
				return FALSE;
			}

			/* Set events */
			p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
		}
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
                        /*py_attack(y, x, -1);*/
			call_lua("py_attack", "(ddd)", "", y, x, -1);
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

	if (c_ptr->event == 3)
	{
		/* If the event has a condition, ressolve it. */
		if (c_ptr->eventcond > 0)
		{
			if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
			{
				int j = 0;
				object_type *o_ptr;
				bool canenter = FALSE;
        			while (j <= 52)
        			{
                			/* Get the item */
                			o_ptr = &inventory[j];

                			if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
					{
						canenter = TRUE;
						j = 52;
					}

                			j++;
        			}
				if (!(canenter))
				{
					msg_print("You cannot bash this door.");
					return FALSE;
				}

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
			}
		}
		else
		{
			int j = 0;
			object_type *o_ptr;
			bool canenter = FALSE;
        		while (j <= 52)
        		{
                		/* Get the item */
                		o_ptr = &inventory[j];

                		if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
				{
					canenter = TRUE;
					j = 52;
				}

                		j++;
        		}
			if (!(canenter))
			{
				msg_print("You cannot bash this door.");
				return FALSE;
			}

			/* Set events */
			p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
		}
	}

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
                        /*py_attack(y, x, -1);*/
			call_lua("py_attack", "(ddd)", "", y, x, -1);
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
                        /*py_attack(y, x, -1);*/
			call_lua("py_attack", "(ddd)", "", y, x, -1);
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
                        /*py_attack(y, x, -1);*/
			call_lua("py_attack", "(ddd)", "", y, x, -1);
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
                move_player(dir, pickup);

                /* Ranger's Wilderness Lore! */
                if (p_ptr->abilities[(CLASS_RANGER * 10)] >= 1) wilderness_lore();

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
        do_cmd_walk_jump(pickup);
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

	/* Prompt for time if needed */
	if (command_arg <= 0)
	{
		cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed '*' hp/mana): ";

		char out_val[80];

		/* Default */
		strcpy(out_val, "*");

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
	bool shoot_bow = FALSE;
	bool shoot_crossbow = FALSE;
	bool shoot_sling = FALSE;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;
	object_type *j_ptr;

	bool hit_body = FALSE;
	bool blocked = FALSE;

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
		if (p_ptr->body_monster > 0)
		{
			use_monster_ranged_attack(p_ptr->body_monster);
			return;
		}
		else
		{
			msg_print("You have nothing to fire with.");
			return;
		}
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
        if (item >= 0 && !(q_ptr->art_flags4 & (TR4_RETURNING)) && (p_ptr->skill[2] < 70 || ammoqty > 1))
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
	tdam = damroll(q_ptr->dd, q_ptr->ds);

	/* Assume a base multiplier */
	tmul = 1;

	shoot_bow = FALSE;
	/* Analyze the launcher */
	switch (j_ptr->sval)
	{
		/* Sling and ammo */
		case SV_SLING:
		{
			tmul = 2;
			shoot_sling = TRUE;
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			tmul = 2;
			shoot_bow = TRUE;
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			tmul = 3;
			shoot_bow = TRUE;
			break;
		}
                /* Elven Bow and Arrow */
                case SV_ELVEN_BOW:
		{
                        tmul = 4;
			shoot_bow = TRUE;
			break;
		}
                /* High Elf Bow and Arrow */
                case SV_HIELF_BOW:
		{
                        tmul = 5;
			shoot_bow = TRUE;
			break;
		}
                /* Hunter's Bow and Arrow */
                case 16:
		{
                        tmul = 6;
			shoot_bow = TRUE;
			break;
		}


		/* Light Crossbow and Bolt */
		case SV_LIGHT_XBOW:
		{
			tmul = 3;
			shoot_crossbow = TRUE;
			break;
		}

		/* Heavy Crossbow and Bolt */
		case SV_HEAVY_XBOW:
		{
			tmul = 4;
			shoot_crossbow = TRUE;
			break;
		}

                /* Masterwork Crossbow and Bolt */
                case 25:
		{
                        tmul = 5;
			shoot_crossbow = TRUE;
			break;
		}
                /* Mighty Crossbow and Bolt */
                case 26:
		{
                        tmul = 6;
			shoot_crossbow = TRUE;
			break;
		}

	}

	/* Get extra "power" from "extra might" */
	if (p_ptr->xtra_might) tmul++;

        tdam = bow_damages(tdam, j_ptr->to_d, q_ptr->to_d);

	/* Base range */
	tdis = 10 + 5 * tmul;

        /* No negative dams */
        if (tdam < 0) tdam = 0;

	/* Take a (partial) turn */
	if (shoot_bow && (p_ptr->skill[19] >= 80) && (randint(100) <= 25))
	{
		msg_print("You fire at the speed of light!");
	}
	else energy_use = (100 / thits);
        
        /* Ricochets ? */
        /*if(p_ptr->pclass == CLASS_ARCHER)
        {
                num_ricochet = (p_ptr->lev / 10) - 1;
                num_ricochet = (num_ricochet < 0)?0:num_ricochet;
        }*/
	num_ricochet = 0;
        
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
                        if (!cave_floor_bold_project(ny, nx)) break;

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
				int hit = 0;
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

				/* Crossbow users can get a bonus! */
				if (shoot_crossbow && p_ptr->skill[20] >= 30) hitbonus += (p_ptr->dis_to_h / 2);

                                /* Called shots actually LOWERS hit rate! */
                                if (penalitypercent < 0) penalitypercent = 0;
                                if (calledshot > 0) hitpenality = ((p_ptr->to_h + hitbonus) * penalitypercent) / 100;

                                /* Did we hit it (penalize range) */
				call_lua("player_hit_monster", "(Md)", "d", m_ptr, j_ptr->to_h + hitbonus - hitpenality, &hit);
                                if (hit == 1)
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
					/* First, check for ranged counters */
					if ((r_ptr->countertype == 16 || r_ptr->countertype == 17 || r_ptr->countertype == 18 || r_ptr->countertype == 19) && randint(100) <= r_ptr->counterchance)
					{
						if (randint(m_ptr->dex) >= randint(p_ptr->stat_ind[A_DEX]))
						{
							char m_name[80];
                                                        monster_desc(m_name, m_ptr, 0);
                                                        msg_format("%s blocked your ammo!", m_name);
                                                        tdam = 0;
							blocked = TRUE;
						}
					}
					if ((r_ptr->countertype == 20 || r_ptr->countertype == 21 || r_ptr->countertype == 22 || r_ptr->countertype == 23) && randint(100) <= r_ptr->counterchance)
					{
						char m_name[80];
                                                monster_desc(m_name, m_ptr, 0);
                                                msg_format("%s blocked your ammo!", m_name);
                                                tdam = 0;
						blocked = TRUE;
					}

					if (!(blocked))
					{
                                        /* Check for the Piercing Shots ability! */
                                        if (p_ptr->abilities[(CLASS_ARCHER * 10) + 2] >= 1 || (q_ptr->art_flags4 & (TR4_LOWER_DEF)))
                                        {
                                                int defamount = 0;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                if (q_ptr->art_flags4 & (TR4_LOWER_DEF))
                                                {
                                                        defamount = (damroll(q_ptr->dd, q_ptr->ds) * 3) / 10;
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

                                                hitamount = (damroll(q_ptr->dd, q_ptr->ds) * 3) / 10;
                                                if (m_ptr->hitrate <= 0) hitamount = 0;
                                                msg_format("%s loses %d hit rate!", m_name, hitamount);
                                                m_ptr->hitrate -= hitamount;
                                                if (m_ptr->hitrate <= 0) m_ptr->hitrate = 0;
                                        }
                                        /* Called shot! */
                                        if (calledshot == CALLED_LEGS)
                                        {
                                                int hitamount;
						int bowskill = 0;
						int xbowskill = 0;
						int slingskill = 0;
						int totskill;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

						if (shoot_bow) bowskill = p_ptr->skill[19] / 3;
						if (shoot_crossbow) xbowskill = p_ptr->skill[20] / 3;
						if (shoot_sling) slingskill = p_ptr->skill[21] / 3;

						totskill = (p_ptr->skill[2] / 3) + bowskill + xbowskill + slingskill;

                                                hitamount = totskill + (totskill * (p_ptr->abilities[(CLASS_ARCHER * 10) + 1] / 4) / 100);
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
						int bowskill = 0;
						int xbowskill = 0;
						int slingskill = 0;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

						if (shoot_bow) bowskill = p_ptr->skill[19] / 4;
						if (shoot_crossbow) xbowskill = p_ptr->skill[20] / 4;
						if (shoot_sling) slingskill = p_ptr->skill[21] / 4;

                                                hitamount = ((p_ptr->skill[2] / 4) + bowskill + xbowskill + slingskill) * (p_ptr->abilities[(CLASS_ARCHER * 10) + 1]);

                                                m_ptr->hitrate -= hitamount;
                                                if (m_ptr->hitrate < 0) m_ptr->hitrate = 0;
                                                msg_format("%s loses %d hit rate!", m_name, hitamount);
                                        }
                                        if (q_ptr->tval == TV_SHOT && p_ptr->abilities[(CLASS_ARCHER * 10) + 8] >= 1)
                                        {
                                                int confpower = p_ptr->skill[2] + p_ptr->skill[21] + (p_ptr->abilities[(CLASS_ARCHER * 10) + 8] * 2);
                                                int mresconf = m_ptr->level + m_ptr->str;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                if ((r_ptr->flags1 & (RF1_UNIQUE)) || m_ptr->boss >= 1) mresconf *= 5;
                                                if (randint(confpower) >= randint(mresconf))
                                                {
                                                        if (!(r_ptr->flags1 & (RF1_QUESTOR)) && !(r_ptr->flags3 & (RF3_NO_CONF)) && !(m_ptr->abilities & (BOSS_IMMUNE_WEAPONS)) && (!(r_ptr->physres >= 100)))
                                                        {
                                                                msg_format("%s is confused!", m_name);
                                                                m_ptr->confused = 5;
                                                        }
                                                }
                                        }
					if (q_ptr->tval == TV_SHOT && p_ptr->skill[21] >= 40)
					{
						int stunpower = p_ptr->skill[21] + p_ptr->stat_ind[A_DEX];
                                                int mresconf = m_ptr->level + m_ptr->str;
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                if (!((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flags3 & (RF3_NO_STUN)) || m_ptr->boss >= 1))
						{
                                                	if (randint(stunpower) >= randint(mresconf))
                                                	{
                                                                msg_format("%s is stunned!", m_name);
                                                                m_ptr->seallight = 3;
                                                	}
						}
					}
					if (q_ptr->tval == TV_SHOT && p_ptr->skill[21] >= 70)
					{
						char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

						if (!(m_ptr->abilities & (BOSS_IMMUNE_WEAPONS)) && (!(r_ptr->physres >= 100)))
						{
							if (randint(p_ptr->skill[21] + p_ptr->stat_ind[A_DEX]) >= randint(m_ptr->level + r_ptr->ac))
							{
								msg_format("%s's defense has been shattered!", m_name);
								m_ptr->defense = 0;
							}
							else
							{
								int tdef = (m_ptr->defense / 5);
								msg_format("%s has lost %d defense!", tdef);
								m_ptr->defense -= tdef;
							}
						}
					}

					} /* End of blocked check. */
                                        
                                        /* Apply special damage XXX XXX XXX */                                        
                                        tdam = tot_dam_aux(q_ptr, tdam, m_ptr);

                                        /* Head called shot */
                                        if (calledshot == CALLED_HEAD) tdam *= 5;

					/* Physical resistance of monsters... */
					tdam -= ((tdam * r_ptr->physres) / 100);
					r_ptr->r_resist[GF_PHYSICAL] = 1;

                                        /* No negative damage */
                                        if (tdam < 0) tdam = 0;

                                        /* Archers shoots multiple arrows at once! */
                                        tdam *= ammoqty;

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
						int spellstat;

						spellstat = (p_ptr->stat_ind[A_DEX] - 5);
						if (spellstat < 0) spellstat = 0;

                                                /* Message */
                                                message_pain(c_ptr->m_idx, tdam);

						/* Ranged ammos brand! */
                                                if (q_ptr->brandtype > 0 && !(blocked))
                                                {
                                                        no_magic_return = TRUE;
                                                        corpse_explode(q_ptr->branddam * (q_ptr->pval + 1), m_ptr->fx, m_ptr->fy, q_ptr->brandrad, q_ptr->brandtype);
                                                        no_magic_return = FALSE;
                                                }

                                                /* Burning shots ability! :) */
                                                if (p_ptr->abilities[(CLASS_ARCHER * 10) + 4] >= 1 && !(blocked))
                                                {
                                                        no_magic_return = TRUE;
                                                        corpse_explode(((p_ptr->abilities[(CLASS_ARCHER * 10) + 4] * 20) * spellstat), m_ptr->fx, m_ptr->fy, 0, GF_FIRE);
                                                        no_magic_return = FALSE;
                                                }
                                                /* Venomous shots ability! :) */
                                                /* Basically the same as burning shots */
                                                if (p_ptr->abilities[(CLASS_ARCHER * 10) + 5] >= 1 && !(blocked))
                                                {
                                                        no_magic_return = TRUE;
                                                        corpse_explode(((p_ptr->abilities[(CLASS_ARCHER * 10) + 5] * 20) * spellstat), m_ptr->fx, m_ptr->fy, 0, GF_POIS);
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
        if (p_ptr->skill[2] < 70 && ammoqty <= 1)
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
	bool blocked = FALSE;

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
	tdam = damroll(q_ptr->dd, q_ptr->ds);
        tdam *= (p_ptr->skill[3] + 1);
        tdam += ((tdam * p_ptr->dis_to_d) / 100);
        tdam += ((tdam * p_ptr->stat_ind[A_STR]) / 100);

        /* Throwing an ammo!?? This is ridiculous... */
        if (is_ammo(q_ptr)) tdam = tdam / 10;

        /* Power Throw feat! */
        if (p_ptr->skill[3] >= 25) tdam += tdam / 2;

	/* Take a turn */
        if (p_ptr->skill[3] >= 50) energy_use = 50;
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
		if (!cave_floor_bold_project(ny, nx))
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
			int hit = 0;
			cave_type *c_ptr = &cave[y][x];

			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			call_lua("player_hit_monster", "(Md)", "d", m_ptr, 0, &hit);
                        if (hit == 1)
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

				/* First, check for ranged counters */
				if (r_ptr->countertype == 16 || r_ptr->countertype == 17 || r_ptr->countertype == 18 || r_ptr->countertype == 19)
				{
					if (randint(m_ptr->dex) >= randint(p_ptr->stat_ind[A_DEX]))
					{
						char m_name[80];
                                                monster_desc(m_name, m_ptr, 0);
                                                msg_format("%s blocked your object!", m_name);
                                                tdam = 0;
						blocked = TRUE;
					}
				}
				if (r_ptr->countertype == 20 || r_ptr->countertype == 21 || r_ptr->countertype == 22 || r_ptr->countertype == 23)
				{
					char m_name[80];
                                        monster_desc(m_name, m_ptr, 0);
                                        msg_format("%s blocked your object!", m_name);
                                        tdam = 0;
					blocked = TRUE;
				}

				/* Physical resistance of monsters... */
				tdam -= ((tdam * r_ptr->physres) / 100);
				r_ptr->r_resist[GF_PHYSICAL] = 1;

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

					/* Ranged ammos brand! */
                                        if (q_ptr->brandtype > 0 && !(blocked))
                                        {
                                                no_magic_return = TRUE;
                                                corpse_explode(q_ptr->branddam * (q_ptr->pval + 1), m_ptr->fx, m_ptr->fy, q_ptr->brandrad, q_ptr->brandtype);
                                                no_magic_return = FALSE;
                                        }

                                        /* Throwing skill high enough to reduce defense? */
                                        if (p_ptr->skill[3] >= 70 && !(blocked))
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
	tdam = damroll(q_ptr->dd, q_ptr->ds);
	tdam *= (p_ptr->skill[3] + 1);
	tdam += ((tdam * (q_ptr->to_d + (p_ptr->stat_ind[A_STR] * 5))) / 100);
	tdam += ((tdam * p_ptr->stat_ind[A_STR]) / 100);

        /* Power Throw feat! */
        if (p_ptr->skill[3] >= 25) tdam += tdam / 2;

	/* Take a turn */
        if (p_ptr->skill[3] >= 50) energy_use = 50;
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
		if (!cave_floor_bold_project(ny, nx))
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
			int hit = 0;
			cave_type *c_ptr = &cave[y][x];

			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
			call_lua("player_hit_monster", "(Md)", "d", m_ptr, q_ptr->to_h, &hit);
                        if (hit == 1)
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

				/* Physical resistance of monsters... */
				tdam -= ((tdam * r_ptr->physres) / 100);
				r_ptr->r_resist[GF_PHYSICAL] = 1;

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
                                        if (p_ptr->skill[3] >= 70)
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

        if((!p_ptr->body_monster))
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

	p_ptr->redraw |= (PR_HP | PR_MANA);
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Allow user to choose a power (racial / mutation / mimic) to activate
 */
void do_cmd_racial_power(int combat_feat)
{
	int                     i = 0;
	int			x = 0;

	int                     Power = -1;
	int                     num = 0, dir = 0;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
	int             ask;

	char            choice;

	char            out_val[160];
	char	str[80];

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
		msg_print("You are too confused to use any abilities!");
		energy_use = 0;
		return;
	}
	
	/* Fill up the menu based on the learned abilities! */
	for (x = 0; x < 36; x++)
	{
		if (p_ptr->abilities_powers[x] != 0)
		{
			if (p_ptr->abilities_powers[x] < 0)
			{
				if ((combat_feat == 1 && feats_def[(p_ptr->abilities_powers[x] * (-1))].combatfeat == 1) || (combat_feat == 0))
				{
					powers[num] = p_ptr->abilities_powers[x];
					strcpy(power_desc[num], feats_def[(p_ptr->abilities_powers[x] * (-1))].name);
					num++;
				}
			}
			else
			{
				if ((combat_feat == 1 && abilities_def[p_ptr->abilities_powers[x]].combatfeat == 1) || (combat_feat == 0))
				{
					if (p_ptr->abilities_powers[x] == ((CLASS_MONSTER_MAGE * 10) + 1))
					{
						powers[num] = ((CLASS_APPRENTICE * 10) + 6);
						strcpy(power_desc[num], "Memorize");
						num++;

                				if (p_ptr->body_monster == 0)
        					{
                					monster_race *r_ptr = &r_info[p_ptr->memorized];
                					powers[num] = ((CLASS_APPRENTICE * 10) + 7);
                					sprintf(power_desc[num], "Morph: %s", (r_name + r_ptr->name));
                					num++;
        					}
        					else
        					{
                					powers[num] = ((CLASS_APPRENTICE * 10) + 8);
                					strcpy(power_desc[num], "Revert to normal shape");
                					num++;
        					}
					}
					else if (p_ptr->abilities_powers[x] == ((CLASS_APPRENTICE * 10) + 9))
					{
						powers[num] = ((CLASS_APPRENTICE * 10) + 9);
						strcpy(power_desc[num], "Capture Soul");
						num++;
					}
					else
					{
						powers[num] = p_ptr->abilities_powers[x];
						strcpy(power_desc[num], abilities_def[p_ptr->abilities_powers[x]].name);
						num++;
					}
				}
			}
		}
	}
	if (num == 0)
	{
		msg_print("No abilities available.");
		return;
	}
        
        /*if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1 && !(p_ptr->auraon) && !combat_feat)
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
	if ((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] >= 1 || p_ptr->pclass == CLASS_MONSTER_MAGE) && !(p_ptr->learning) && !combat_feat)
        {
                powers[num] = -997;
                strcpy(power_desc[num], "Turn on learning.");
                num++;
        }        
        if ((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] >= 1 || p_ptr->pclass == CLASS_MONSTER_MAGE) && p_ptr->learning && !combat_feat)
        {
                powers[num] = -998;
                strcpy(power_desc[num], "Turn off learning.");
                num++;
        }*/       
                        

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
		strnfmt(out_val, 78, "(Abilities %c-%c, *=List, ESC=exit) Use which ability? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
		strnfmt(out_val, 78, "(Abilities %c-%c, *=List, ESC=exit) Use which ability? ",
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

	/* Actually use the power. */
	/* First, we will determine if it's hardcoded or soft-coded(trough lua). */
	if (Power > 0)
	{
		/* Is is hard-coded? */
		if (abilities_def[Power].hardcode == 1) use_hardcode_ability(abilities_def[Power].powerid);
		else call_lua("use_ability", "(d)", "", abilities_def[Power].powerid);
	}
	else if (Power < 0)
	{
		/* Is is hard-coded? */
		Power = Power * (-1);
		if (feats_def[Power].hardcode == 1) use_hardcode_ability(feats_def[Power].powerid);
		else call_lua("use_ability", "(d)", "", feats_def[Power].powerid);
	}

	/* Success */
	return;
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
        {TR2_RES_EARTH,42,"Resist Earth"},
        {TR2_RES_CHAOS,39,"Resist Chaos"},
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
                place_monster_one_return_no_boss(y, x, o_ptr->pval2, FALSE, TRUE, p_ptr->lev * 2, o_ptr->pval3, o_ptr->pval3, 0);
                msg_print("You revive the dead monster!");
                delete_object_idx(c_ptr->o_idx);
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
          if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (distance(ij,ii,py,px) > 1 + (p_ptr->skill[5] / 10)))
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
	int mondur;

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
	if (((p_ptr->stat_ind[A_WIS] - 5) / 2) <= 0) monlvl = 1;
        else monlvl = ((p_ptr->stat_ind[A_WIS] - 5) / 2) + (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 1] * 2);
        monhp *= monlvl;
        snum = 1 + (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 1] / 10);
	mondur = 5 + (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 1] / 4);


        for (i = 0; i < snum; i++)
        {
                msg_print("Place where?");
                if (!tgt_pt(&x,&y)) msg_print("Summon failed.");
                if((m_idx=place_monster_one_simulacrum(y, x, o_ptr->pval, FALSE, TRUE, monlvl, monhp, mondur))==0) msg_print("Summon failed.");
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

s32b bow_damages(s32b dam, int lbonus, int abonus)
{
	s32b k;
	s32b tot_to_d;
	int tmul, tskill;
	bool is_crossbow = FALSE;
        object_type *o_ptr = &inventory[INVEN_BOW];

	/* Assume a base multiplier */
	tmul = 1;

	/* Base Shooting skill */
	tskill = p_ptr->skill[2];

	/* Analyze the launcher */
	switch (o_ptr->sval)
	{
		/* Sling and ammo */
		case SV_SLING:
		{
			tmul = 2;
			tskill += (p_ptr->skill[21] + (p_ptr->skill[21] / 2));
			break;
		}

		/* Short Bow and Arrow */
		case SV_SHORT_BOW:
		{
			tmul = 2;
			tskill += (p_ptr->skill[19] + (p_ptr->skill[19] / 2));
			break;
		}

		/* Long Bow and Arrow */
		case SV_LONG_BOW:
		{
			tmul = 3;
			tskill += (p_ptr->skill[19] + (p_ptr->skill[19] / 2));
			break;
		}
                /* Elven Bow and Arrow */
                case SV_ELVEN_BOW:
		{
                        tmul = 4;
			tskill += (p_ptr->skill[19] + (p_ptr->skill[19] / 2));
			break;
		}
                /* High Elf Bow and Arrow */
                case SV_HIELF_BOW:
		{
                        tmul = 5;
			tskill += (p_ptr->skill[19] + (p_ptr->skill[19] / 2));
			break;
		}
                /* Hunter's Bow and Arrow */
                case 16:
		{
                        tmul = 6;
			tskill += (p_ptr->skill[19] + (p_ptr->skill[19] / 2));
			break;
		}

		/* Light Crossbow and Bolt */
		case SV_LIGHT_XBOW:
		{
			tmul = 3;
			tskill += (p_ptr->skill[20] + (p_ptr->skill[20] / 2));
			is_crossbow = TRUE;
			break;
		}

		/* Heavy Crossbow and Bolt */
		case SV_HEAVY_XBOW:
		{
			tmul = 4;
			tskill += (p_ptr->skill[20] + (p_ptr->skill[20] / 2));
			is_crossbow = TRUE;
			break;
		}

                /* Masterwork Crossbow and Bolt */
                case 25:
		{
                        tmul = 5;
			tskill += (p_ptr->skill[20] + (p_ptr->skill[20] / 2));
			is_crossbow = TRUE;
			break;
		}

                /* Mighty Crossbow and Bolt */
                case 26:
		{
                        tmul = 6;
			tskill += (p_ptr->skill[20] + (p_ptr->skill[20] / 2));
			is_crossbow = TRUE;
			break;
		}

	}
        if (p_ptr->xtra_might) tmul++;

	if (is_crossbow && p_ptr->skill[20] >= 60) dam *= 2;
        k = dam * tmul;
	k *= (tskill + 1);
	tot_to_d = lbonus + abonus + (p_ptr->stat_ind[A_DEX] * 5);    
        k += ((k * tot_to_d) / 100);
        k += ((k * p_ptr->stat_ind[A_DEX]) / 100);
        return (k);        
}

void use_monster_ranged_attack(int r_idx)
{
		int             powers[36];
		char            power_desc[36][80];
		char 		powdesc[120];

		int num = 0;
		int i, dir;
		int Power = -1;
                s32b damage = 0;

		bool            flag, redraw;
        	int             ask, plev = p_ptr->lev;

		char            choice;

		char            out_val[160];

		cptr act = NULL;
		monster_race *r_ptr = &r_info[r_idx];
		
		/* List the powers */
		i = 0;
		while (i < 20 && r_ptr->attack[i].type > 0) 
		{
			if (r_ptr->attack[i].type == 3)
			{
				sprintf(powdesc, "%s  Element: %s  Dam: %dd%d", r_ptr->attack[i].name, get_element_name(r_ptr->attack[i].element), r_ptr->attack[i].ddice, r_ptr->attack[i].dside);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			i++;
		}

        	if(!num) {msg_print("No attacks to use.");return;}

		/* Nothing chosen yet */
		flag = FALSE;

		/* No redraw yet */
		redraw = FALSE;

		/* Build a prompt (accept all spells) */
		if (num <= 26)
		{
			/* Build a prompt (accept all spells) */
        	        strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
			I2A(0), I2A(num - 1));
		}
		else
		{
        	        strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
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

		/* Actually use the power! */
		switch (r_ptr->attack[Power].type)
		{
			/* Ranged attack */
			case 3:
			{
				damage = damroll(r_ptr->attack[Power].ddice, r_ptr->attack[Power].dside);
        			damage *= (p_ptr->skill[2] + 1);
        			damage += ((damage * (p_ptr->stat_ind[A_DEX] * 5)) / 100);
        			damage += ((damage * p_ptr->stat_ind[A_DEX]) / 100);
				monster_ranged = TRUE;
				if (!get_aim_dir(&dir)) return;
                        	fire_bolt(r_ptr->attack[Power].element, dir, damage);
				monster_ranged = FALSE;
				break;
			}
		
			default:
			{
				break;
			}
		}	
		energy_use = 100;
		update_and_handle();
}

/* Pick one of your two weapons */
void choose_current_weapon()
{
	char ch;
	if (two_weapon_wield())
	{
		get_com("Use which weapon? (1 or 2) ", &ch);

                if (ch == '1') current_weapon = &inventory[INVEN_WIELD];
		else current_weapon = &inventory[INVEN_WIELD+1];
	}
	else if (one_weapon_wield())
	{
		if (inventory[INVEN_WIELD].k_idx) current_weapon = &inventory[INVEN_WIELD];
		else current_weapon = &inventory[INVEN_WIELD+1];
	}
}

/* Use an hard-coded ability */
/* Of course, the goal is to eventually make this function obsolete */
/* and have all abilities scripted! ;) */
void use_hardcode_ability(int powernum)
{
	int dir;
	int spellstat;
	switch(powernum)
	{
		case 1:
		{
			if (!combatfeat) choose_current_weapon();
                        spin_attack();
                        energy_use = 100;
			break;
		}
		case 2:
		{
			if (!combatfeat) choose_current_weapon();
                        if (!get_rep_dir(&dir)) return;
                        accurate_strike(dir);
                        energy_use = 100;
			break;
		}
		case 3:
		{
			s32b duration;
                        duration = p_ptr->abilities[(CLASS_WARRIOR * 10) + 6] + 4;
                        attack_aura(GF_WARCRY, duration, 3);
                        update_and_handle();
			energy_use = 100;
			break;
		}
		case 4:
		{
			if (!combatfeat) choose_current_weapon();
                        leaping_spin();
                        energy_use = 100;
			break;
		}
		case 5:
		{
			attack_aura(GF_TAUNT, 1, 5);
                        update_and_handle();
			energy_use = 100;
			break;
		}
		case 6:
		{
			if (!get_rep_dir(&dir)) return;
                        fighter_throw(dir);
                        energy_use = 100;
			break;
		}
		case 7:
		{
			if (p_ptr->powerattack > 0)
			{
				s32b mdam;
				if (unarmed()) call_lua("monk_damages", "", "l", &mdam);
				else call_lua("weapon_damages", "", "l", &mdam);
				mdam *= p_ptr->abilities[(CLASS_FIGHTER * 10) + 8];
                        	if (!get_rep_dir(&dir)) return;
                        	hard_kick(dir, mdam, 5 + (p_ptr->abilities[(CLASS_FIGHTER * 10) + 8] / 5));
                        	energy_use = 100;
			}
			else msg_print("This can only be used while in Power Attack mode.");
			break;
		}
		case 8:
		{
			p_ptr->pres = 25 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] / 5);
                        p_ptr->mres = 25 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] / 5);
                        (void)set_pres((5 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] * 2)));
                        (void)set_mres((5 + (p_ptr->abilities[(CLASS_MAGE * 10) + 1] * 2)));
                        energy_use = 100;
			break;
		}
		case 9:
		{
			s32b dam;
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_INT] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
                        dam = p_ptr->abilities[(CLASS_MAGE * 10) + 2] * 10;
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_MISSILE, dir, (spellstat * dam));
                        update_and_handle();
                        energy_use = 100;
			break;
		}
		case 10:
		{
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_SLOW_DOWN, dir, 0, (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 20));
                        energy_use = 100;
			break;
		}
		case 11:
		{
			int a = 0;
                        msg_print("You create illusions of yourself!");
                        for (a = 0; a < ((p_ptr->abilities[(CLASS_MAGE * 10) + 5] / 5) + 3); a++)
                        {                        
                                summon_specific_friendly(py, px, 127, SUMMON_ILLUSION, FALSE, p_ptr->abilities[(CLASS_MAGE * 10) + 5] + 10);
                        }
                        energy_use = 100;
			break;
		}
		case 12:
		{
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DAMAGES_CURSE, dir, 0, (p_ptr->abilities[(CLASS_MAGE * 10) + 6] / 20));
                        energy_use = 100;
			break;
		}
		case 13:
		{
			drain_object();
                        energy_use = 100;
			break;
		}
		case 14:
		{
			stone_to_gold();
                        energy_use = 100;
			break;
		}
		case 15:
		{
			animate_knight();
                        energy_use = 100;
			break;
		}
		case 16:
		{
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_WIS] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
			msg_print("You heal yourself!");
                        p_ptr->chp += ((10 + (p_ptr->abilities[(CLASS_PRIEST * 10)] / 2)) * spellstat);
                        if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                        if (p_ptr->abilities[(CLASS_PRIEST * 10)] >= 15)
                        {
                                (void)set_cut(0);
                                (void)set_poisoned(0);
                        }
                        update_and_handle();
                        energy_use = 100;
			break;
		}
		case 17:
		{
			s32b turn_damages;
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_WIS] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
                        turn_damages = (spellstat * 20) * p_ptr->abilities[(CLASS_PRIEST * 10) + 1];
                        attack_aura(GF_TURN_UNDEAD, turn_damages, 10);
                        energy_use = 100;
			break;
		}
		case 18:
		{
			mace_of_heaven();
                        energy_use = 100;
			break;
		}
		case 19:
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
			break;
		}
		case 20:
		{
			msg_print("You feel empowered...");
                        (void)set_fast(4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5]));
                        (void)set_blessed(4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5]));
                        p_ptr->pres = 20 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5] / 3);
                        (void)set_pres(4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5]));
                        energy_use = 100;
			break;
		}
		case 21:
		{
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_WIS] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
			if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, (p_ptr->abilities[(CLASS_PRIEST * 10) + 6] * 20) * spellstat, 0, 1);
                        energy_use = 100;
			break;
		}
		case 22:
		{
			p_ptr->ac_boost = p_ptr->abilities[(CLASS_PRIEST * 10) + 8] * 30;
                        (void)set_ac_boost(p_ptr->abilities[(CLASS_PRIEST * 10) + 8] + 5);
                        energy_use = 100;
			break;
		}
		case 23:
		{
			godly_wrath();
                        energy_use = 100;
			break;
		}
		case 24:
		{
			int invpower = p_ptr->abilities[(CLASS_ROGUE * 10)] + 10;
                        (void)set_invis(invpower, invpower);
                        p_ptr->ac_boost = p_ptr->abilities[(CLASS_ROGUE * 10)] * 10;
                        (void)set_ac_boost(invpower);
                        energy_use = 100;
			break;
		}
		case 25:
		{
			assassin_poison_weapon();
                        energy_use = 100;
			break;
		}
		case 26:
		{
			set_gas_trap();
                        energy_use = 100;
			break;
		}
		case 27:
		{
			set_poison_trap();
                        energy_use = 100;
			break;
		}
		case 28:
		{
			set_spike_trap();
                        energy_use = 100;
			break;
		}
		case 29:
		{
			ranger_entangle();
                        energy_use = 100;
			break;
		}
		case 30:
		{
			attack_aura(GF_ANIMAL_EMPATHY, 0, (p_ptr->abilities[(CLASS_RANGER * 10) + 3] / 10) + 5);
                        update_and_handle();
                        energy_use = 100;
			break;
		}
		case 31:
		{
			msg_print("You call an animal to your aid!");
                        summon_specific_friendly(py, px, p_ptr->abilities[(CLASS_RANGER * 10) + 4], SUMMON_ANIMAL, FALSE, (p_ptr->abilities[(CLASS_RANGER * 10) + 4] / 2) + 10);
                        energy_use = 100;
			break;
		}
		case 32:
		{
			warp_on_trees();
                        energy_use = 100;
			break;
		}
		case 33:
		{
			ranger_thorned_vines();
                        energy_use = 100;
			break;
		}
		case 34:
		{
			attack_aura(GF_SLEEP_POLLEN, 0, 3 + (p_ptr->abilities[(CLASS_RANGER * 10) + 8] / 10));
                        energy_use = 100;
			break;
		}
		case 35:
		{
			ranger_force_of_nature();
                        energy_use = 100;
			break;
		}
		case 36:
		{
			p_ptr->str_boost = (p_ptr->abilities[(CLASS_PALADIN * 10)] * 2);
                        (void)set_str_boost(5 + p_ptr->abilities[(CLASS_PALADIN * 10)]);
                        energy_use = 100;
			break;
		}
		case 37:
		{
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_WIS] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
			if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_LITE, dir, (p_ptr->abilities[(CLASS_PALADIN * 10) + 1] * 10) * spellstat);
                        energy_use = 100;
			break;
		}
		case 38:
		{
			if (!combatfeat) choose_current_weapon();
                        if (!get_rep_dir(&dir)) return;
                        smite_evil(dir);
                        energy_use = 100;
			break;
		}
		case 39:
		{
			blade_of_purity();
                        energy_use = 100;
			break;
		}
		case 40:
		{
			p_ptr->str_boost = (p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 5);
                        p_ptr->dex_boost = (p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 5);
                        p_ptr->ac_boost = p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 100;
                        (void)set_ac_boost(2);
                        (void)set_blessed(2);
                        (void)set_str_boost(2);
                        (void)set_dex_boost(2);
                        energy_use = 100;
			break;
		}
		case 41:
		{
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_RETROGRADE_DARKNESS, dir, 0, (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] / 10));                        
                        energy_use = 100;
			break;
		}
		case 42:
		{
			paladin_shining_armor();
                        energy_use = 100;
			break;
		}
		case 43:
		{
			word_of_peace();
                        energy_use = 100;
			break;
		}
		case 44:
		{
			s32b mdam;
			call_lua("monk_damages", "", "l", &mdam);
			mdam += ((mdam * ((p_ptr->abilities[(CLASS_MONK * 10) + 1] - 1) * 20)) / 100);
                        spin_kick(mdam, 1);
                        energy_use = 100;
			break;
		}
		case 45:
		{
			s32b mdam;
			call_lua("monk_damages", "", "l", &mdam);
			mdam += ((mdam * ((p_ptr->abilities[(CLASS_MONK * 10) + 2] - 1) * 10)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        hard_kick(dir, mdam, 3 + (p_ptr->abilities[(CLASS_MONK * 10) + 2] / 10));
                        energy_use = 100;
			break;
		}
		case 46:
		{
			s32b mdam;
			call_lua("monk_damages", "", "l", &mdam);
			mdam += ((mdam * ((p_ptr->abilities[(CLASS_MONK * 10) + 5] - 1) * 10)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MISSILE, mdam, 0, 1);
                        energy_use = 100;
			break;
		}
		case 47:
		{
			int x, y;
			s32b mdam;
			call_lua("monk_damages", "", "l", &mdam);
			mdam += ((mdam * ((p_ptr->abilities[(CLASS_MONK * 10) + 8] - 1) * 20)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, mdam, 0, 1);
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
			break;
		}
		case 48:
		{
			called_shots();
			break;
		}
		case 49:
		{
			s32b dam;
                        object_type *o_ptr = &inventory[INVEN_AMMO];
                        object_type *q_ptr = &inventory[INVEN_BOW];
                        if (!q_ptr) {msg_print("You need a shooter!"); return;}
                        if (!o_ptr) {msg_print("You need ammos!"); return;}
                        dam = damroll(o_ptr->dd, o_ptr->ds);
			dam = bow_damages(dam, q_ptr->to_d, o_ptr->to_d);
                        dam += ((dam * (p_ptr->abilities[(CLASS_ARCHER * 10) + 3] * 10)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, (p_ptr->abilities[(CLASS_ARCHER * 10) + 3] + 4));
                        inven_item_increase(INVEN_AMMO, -1);
                        inven_item_describe(INVEN_AMMO);
                        inven_item_optimize(INVEN_AMMO);
                        energy_use = 100;
			break;
		}
		case 50:
		{
			do_cmd_fire(0, TRUE);
			break;
		}
		case 51:
		{
			s32b dam;
                        object_type *o_ptr = &inventory[INVEN_AMMO];
                        object_type *q_ptr = &inventory[INVEN_BOW];
                        if (!q_ptr || (q_ptr->sval != SV_LIGHT_XBOW && q_ptr->sval != SV_HEAVY_XBOW)) {msg_print("You need a crossbow!"); return;}
                        if (o_ptr->tval != TV_BOLT) {msg_print("You need bolts!"); return;}
                        dam = damroll(o_ptr->dd, o_ptr->ds);
			dam = bow_damages(dam, q_ptr->to_d, o_ptr->to_d);
			dam += ((dam * ((p_ptr->abilities[(CLASS_ARCHER * 10) + 7] - 1) * 10)) / 100);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir, dam, (p_ptr->abilities[(CLASS_ARCHER * 10) + 7] / 30)+1);
                        inven_item_increase(INVEN_AMMO, -1);
                        inven_item_describe(INVEN_AMMO);
                        inven_item_optimize(INVEN_AMMO);
                        energy_use = 100;
			break;
		}
		case 52:
		{
			s32b dam;
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_INT] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
			dam = (p_ptr->abilities[(CLASS_ELEM_LORD * 10)] * 10) * spellstat;
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(p_ptr->elemlord, dir, dam, (p_ptr->abilities[(CLASS_ELEM_LORD * 10)] / 20)+1);                                                
                        energy_use = 100;
			break;
		}
		case 53:
		{
			if (!combatfeat) choose_current_weapon();
                        if (!get_rep_dir(&dir)) return;
                        element_strike(dir);
                        energy_use = 100;
			break;
		}
		case 54:
		{
			(void)set_elem_shield(p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 2] + 9);
                        energy_use = 100;
			break;
		}
		case 55:
		{
			s32b mdam;
			call_lua("monk_damages", "", "l", &mdam);
			mdam += ((mdam * ((p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 3] - 1) * 10)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, p_ptr->elemlord, mdam, p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 3] / 30, 1);
                        energy_use = 100;
			break;
		}
		case 56:
		{
			explosive_throw();
                        energy_use = 100;
			break;
		}
		case 57:
		{
			if (!combatfeat) choose_current_weapon();
                        elem_wave();
                        energy_use = 100;
			break;
		}
		/* Power 58 is split into 10000, 10001 and 10002 */
		case 10000:
		{
			morph_memorize();
                        energy_use = 100;
			break;
		}
		case 10001:
		{
			morph_into_memorized();
                        lite_spot(py, px);
                        energy_use = 100;
			break;
		}
		case 10002:
		{
			p_ptr->body_monster = 0;
                        lite_spot(py, px);
                        msg_print("You return to your normal shape!");
                        update_and_handle();
                        energy_use = 100;
			break;
		}
		
		case 59:
		{
			if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_DOMINATE_MONSTER, dir, 0);
                        energy_use = 100;
			break;
		}
		case 60:
		{
			monstrous_wave();
			break;
		}
		case 61:
		{
			s32b dam;
                        int dis;
                        object_type *o_ptr = &inventory[INVEN_ARM];
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_STR] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
                        dam = ((o_ptr->ac * p_ptr->abilities[(CLASS_DEFENDER * 10) + 2]) * spellstat);
                        dam += ((dam * p_ptr->dis_to_d) / 100);
			dam += ((dam * p_ptr->stat_ind[A_STR]) / 100);
                        dis = 2 + (p_ptr->abilities[(CLASS_DEFENDER * 10) + 2] / 20);
                        if (!get_rep_dir(&dir)) return;
                        smash(dir, dam, dis);
                        energy_use = 100;
			break;
		}
		case 62:
		{
			s32b dam;
                        if (!combatfeat) choose_current_weapon();
                        dam = damroll(current_weapon->dd, current_weapon->ds);
                        dam *= (p_ptr->skill[4] + 1);
                        dam += ((dam * p_ptr->dis_to_d) / 100);
			dam += ((dam * p_ptr->stat_ind[A_STR]) / 100);
                        dam += ((dam * ((p_ptr->abilities[(CLASS_DEFENDER * 10) + 7] - 1) * 20)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, 1);
                        energy_use = 100;
			break;
		}
		case 63:
		{
			s32b dam;
                        object_type *o_ptr = &inventory[INVEN_ARM];
                        dam = ((o_ptr->ac + o_ptr->to_a) * p_ptr->skill[3]) * p_ptr->abilities[(CLASS_DEFENDER * 10) + 8];
                        dam += ((dam * p_ptr->dis_to_d) / 100);
			dam += ((dam * p_ptr->stat_ind[A_STR]) / 100);
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_PHYSICAL, dir, dam);
                        energy_use = 100;
			break;
		}
		case 64:
		{
			s32b dam;
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_WIS] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
			dam = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10)] * 20) * spellstat;
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_SHATTER_EVIL, dir, dam, (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10)] / 30)+2);                                                
                        energy_use = 100;
			break;
		}
		case 65:
		{
			attack_aura(GF_ANGELIC_VOICE, 0, 15);
                        energy_use = 100;
			break;
		}
		case 66:
		{
			justice_bless_weapon();
                        energy_use = 100;
			break;
		}
		case 67:
		{
			s32b dam;
                        dam = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 5] * 10) * p_ptr->lev;
                        attack_aura(GF_LITE, dam, 3 + (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 5] / 20));
                        energy_use = 100;
			break;
		}
		case 68:
		{
			s32b dam;
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_WIS] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
                        dam = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 6] * 30) * spellstat;
                        if (!get_aim_dir(&dir)) return;
                        no_magic_return = TRUE;
                        fire_bolt(GF_SLAY_EVIL, dir, dam);
                        no_magic_return = FALSE;
                        update_and_handle();
                        energy_use = 100;
			break;
		}
		case 69:
		{
			int x;
                        msg_print("You call angels to your aid!");
                        for (x = 0; x < ((p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 7] / 5) + 1); x++)
                        {
                                summon_specific_friendly(py, px, p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 7], SUMMON_ANGEL, FALSE, (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 7] / 5) + 5);
                        }
                        energy_use = 100;
			break;
		}
		case 70:
		{
			s32b dam;
                        dam = (p_ptr->chp * p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 9]);
                        attack_aura(GF_LITE, dam, 5 + (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 9] / 30));
                        energy_use = 100;
			break;
		}
		case 71:
		{
			s32b dam;
			call_lua("monk_damages", "", "l", &dam);
			dam += ((dam * ((p_ptr->abilities[(CLASS_ZELAR * 10)] - 1) * 40)) / 100);
                        attack_aura(GF_MANA, dam, 2 + (p_ptr->abilities[(CLASS_ZELAR * 10)] / 20));
                        energy_use = 100;
			break;
		}
		case 72:
		{
			s32b dam;
                        int range = 10 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 1] / 2);
			call_lua("monk_damages", "", "l", &dam);
			dam += ((dam * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 1] - 1) * 50)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, dam, 0, range);
                        energy_use = 100;
			break;
		}
		case 73:
		{
			s32b dam;
			call_lua("monk_damages", "", "l", &dam);
			dam += ((dam * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 3] - 1) * 20)) / 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, dam, 0, 1);
                        energy_use = 100;
			break;
		}
		case 74:
		{
			if (!get_rep_dir(&dir)) return;
                        zelar_leg_throw(dir);
                        energy_use = 100;
			break;
		}
		case 75:
		{
			int spellstat;

			spellstat = (p_ptr->stat_ind[A_WIS] - 5);
			if (spellstat < 0) spellstat = 0;
			msg_print("You energize yourself!");
                        p_ptr->chp += ((10 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 6]*2)) * spellstat);
                        p_ptr->csp += ((5 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 6]) / 2) * spellstat);
                        if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                        if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
                        (void)set_cut(0);
                        update_and_handle();
                        energy_use = 100;
			break;
		}
		case 76:
		{
			{
                                s32b dam;
				call_lua("monk_damages", "", "l", &dam);
				dam += ((dam * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 7] - 1) * 5)) / 100);
                                dir = get_a_dir();
                                chain_attack(dir, GF_MANA, dam, 0, 10);
                        }
                        {
                                s32b dam;
				call_lua("monk_damages", "", "l", &dam);
				dam += ((dam * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 7] - 1) * 5)) / 100);
                                dir = get_a_dir();
                                chain_attack(dir, GF_MANA, dam, 0, 10);
                        }
                        energy_use = 100;
			break;
		}
		case 77:
		{
			p_ptr->str_boost = (p_ptr->abilities[(CLASS_ZELAR * 10) + 8] * 20);
                        (void)set_str_boost(2);
                        energy_use = 100;
			break;
		}
		case 10003:
		{
			int x, y;
                        if (!tgt_pt(&x,&y)) return;
                        capture_soul(x, y);
                        energy_use = 100;
			break;
		}
		case 78:
		{
			use_soul_power();
			break;
		}
		case 79:
		{
			simulacrum();
			break;
		}
		case 80:
		{
			soul_bind();
			break;
		}
		case 81:
		{
			sealing_light();
			break;
		}
		case 82:
		{
			soul_energize();
			break;
		}
		case 83:
		{
			monster_race *r_ptr;
                        int m_idx;
                        s32b monhp;
                        s32b dam;
                        int item;
                        object_type *o_ptr;

                        cptr q, s;

			if (!combatfeat) choose_current_weapon();

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

			call_lua("weapon_damages", "", "l", &dam);
                        dam = dam + monhp;
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MISSILE, dam, 0, 1);
                        energy_use = 100;
			break;
		}
		case 84:
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

                        p_ptr->ac_boost = (r_ptr->ac) * p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7];
                        p_ptr->mres = r_ptr->level + ((r_ptr->level * p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7]) / 100);
                        if (p_ptr->mres > 75) p_ptr->mres = 75;
                        (void)set_mres(p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7] + 10);
                        (void)set_ac_boost(p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7] + 10);

                        o_ptr->timeout = p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 7] + 10;
                        energy_use = 100;
			break;
		}
		case 85:
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
			break;
		}
		case 86:
		{
			if (p_ptr->tim_invisible >= 1)
                        {
                                s32b dam;
                                object_type *o_ptr = &inventory[INVEN_WIELD];
                                dam = damroll(o_ptr->dd, o_ptr->ds);
                                dam *= (p_ptr->skill[6] + 1);
                                dam += ((dam * p_ptr->dis_to_d) / 100);
				dam += ((dam * p_ptr->stat_ind[A_STR]) / 100);
				dam += ((dam * ((p_ptr->abilities[(CLASS_SHADOW * 10)] - 1) * 20)) / 100);
                                if (!get_rep_dir(&dir)) return;
                                chain_attack(dir, GF_STEALTH_ATTACK, dam, 0, 1);
                        }
                        else msg_print("You can only use this if you are using a temporary invisibility ability!");
			energy_use = 100;
			break;
		}
		case 87:
		{
			int invpower = (p_ptr->abilities[(CLASS_SHADOW * 10) + 2] * 3) + 20;
                        int duration = (p_ptr->abilities[(CLASS_SHADOW * 10) + 2] * 2) + 10;
                        (void)set_invis(duration, invpower);
                        update_and_handle();
                        energy_use = 100;
			break;
		}
		case 88:
		{
			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_DEX] - 5);

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
			if (p_ptr->tim_invisible >= 1)
                        {
                                s32b dam = (p_ptr->skill[6] * spellstat) + (((p_ptr->skill[6] * p_ptr->lev) * (p_ptr->abilities[(CLASS_SHADOW * 10) + 3] * 20)) / 100);
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DARK, dir, dam, (p_ptr->abilities[(CLASS_SHADOW * 10) + 3] / 30)+2);                                                
                                energy_use = 100;
                        }
                        else msg_print("You can only use this if you are using a temporary invisibility ability!");
			break;
		}
		case 89:
		{
			if (!(p_ptr->inside_quest))
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
			else msg_print("You cannot use this here.");
			break;
		}
		case 90:
		{
			dark_mist_ability();
			break;
		}
		case 91:
		{
			if (p_ptr->tim_invisible >= 1)
                        {
                        
                        s32b dam;
                        int x, y, flg;

                        flg = PROJECT_GRID | PROJECT_KILL;

			if (!combatfeat) choose_current_weapon();
			call_lua("weapon_damages", "", "l", &dam);
			dam += ((dam * ((p_ptr->abilities[(CLASS_SHADOW * 10) + 9] - 1) * 20)) / 100);
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
			break;
		}

		/* Feats */
		case 1000:
		{
			p_ptr->powerlevel = 1;
			p_ptr->str_boost = p_ptr->stat_cur[A_STR];
			if (p_ptr->abilities[(CLASS_FIGHTER * 10) + 6] >= 1)
			{
				p_ptr->pres = 15 + p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				p_ptr->mres = 15 + p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				p_ptr->ac_boost = 100 * p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				if (p_ptr->pres > 75) p_ptr->pres = 75;
				if (p_ptr->mres > 75) p_ptr->mres = 75;
				set_pres(3);
				set_mres(3);
				set_ac_boost(3);
			}
			set_powerattack(3);
			set_str_boost(3);
                        energy_use = 200;
			break;
		}
		case 1001:
		{
			p_ptr->powerlevel = 2;
			p_ptr->str_boost = p_ptr->stat_cur[A_STR] * 5;
			if (p_ptr->abilities[(CLASS_FIGHTER * 10) + 6] >= 1)
			{
				p_ptr->pres = 15 + p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				p_ptr->mres = 15 + p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				p_ptr->ac_boost = 100 * p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				if (p_ptr->pres > 75) p_ptr->pres = 75;
				if (p_ptr->mres > 75) p_ptr->mres = 75;
				set_pres(4);
				set_mres(4);
				set_ac_boost(4);
			}
			set_powerattack(4);
			set_str_boost(4);
                        energy_use = 300;
			break;
		}
		case 1002:
		{
			p_ptr->powerlevel = 3;
			p_ptr->str_boost = p_ptr->stat_cur[A_STR] * 9;
			if (p_ptr->abilities[(CLASS_FIGHTER * 10) + 6] >= 1)
			{
				p_ptr->pres = 15 + p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				p_ptr->mres = 15 + p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				p_ptr->ac_boost = 100 * p_ptr->abilities[(CLASS_FIGHTER * 10) + 6];
				if (p_ptr->pres > 75) p_ptr->pres = 75;
				if (p_ptr->mres > 75) p_ptr->mres = 75;
				set_pres(5);
				set_mres(5);
				set_ac_boost(5);
			}
			set_powerattack(5);
			set_str_boost(5);
                        energy_use = 400;
			break;
		}
		case 1003:
		{
			enchanted_blood();
                        energy_use = 100;
			break;
		}
		case 1004:
		{
			mana_shield();
                        energy_use = 100;
			break;
		}
		case 1005:
		{
			sharpen_ammos();
                        energy_use = 100;
			break;
		}
		case 1006:
		{
			agility_jump();
                        energy_use = 100;
			break;
		}
		case 1007:
		{
			msg_print("You hide nearby...");
                        (void)set_invis(3, 5);
                        energy_use = 100;
			break;
		}
		case 1008:
		{
			teleport_player(10);
                        energy_use = 100;
			break;
		}
		case 1009:
		{
			s32b dam;
			s32b dam2;
			current_weapon = &inventory[INVEN_WIELD];
			call_lua("weapon_damages", "", "l", &dam);
			current_weapon = &inventory[INVEN_WIELD+1];
			call_lua("weapon_damages", "", "l", &dam2);
			dam += dam2;
			dam *= 2;
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, 1);
                        energy_use = 100;
			break;
		}
		case 1010:
		{
			if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_OLD_HEAL, dir, (p_ptr->lev * 10));
                        energy_use = 100;
			break;
		}
		case 1011:
		{
			attack_aura(GF_MORALE_BOOST, 0, 3);
                        energy_use = 100;
			break;
		}
		case 1012:
		{
			decompose_item();
                        energy_use = 100;
			break;
		}
		case 1013:
		{
			combine_items();
                        energy_use = 100;
			break;
		}
		case 1014:
		{
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_SWORD) return;
                        sword_spin();
                        energy_use = 100;
			break;
		}
		case 1015:
		{
			s32b dam;
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_HAFTED) return;
                        call_lua("weapon_damages", "", "l", &dam);                                
                        dam += (dam / 2);
                        if (!get_rep_dir(&dir)) return;
                        smash(dir, dam, 1);
                        energy_use = 100;
			break;
		}
		case 1016:
		{
			s32b dam;
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_HAFTED) return;
			call_lua("weapon_damages", "", "l", &dam);
                        dam *= 2;
                        if (!get_rep_dir(&dir)) return;
                        dizzy_smash(dir, dam, 1);
                        energy_use = 100;
			break;
		}
		case 1017:
		{
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_HAFTED && current_weapon->tval != TV_MSTAFF) return;
                        if (!get_rep_dir(&dir)) return;
                        shattering_blow(dir);
                        energy_use = 100;
			break;
		}
		case 1018:
		{
			s32b dam;
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_POLEARM) return;
			call_lua("weapon_damages", "", "l", &dam);
                        dam *= 2;
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, 2);
                        energy_use = 100;
			break;
		}
		case 1019:
		{
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_DAGGER) return;
                        if (!get_rep_dir(&dir)) return;
                        eye_stab(dir);
                        energy_use = 100;
			break;
		}
		case 1020:
		{
			dagger_fatal_stab();
                        energy_use = 100;
			break;
		}
		case 1021:
		{
			s32b dam;
                        if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_AXE) return;
                        call_lua("weapon_damages", "", "l", &dam);
                        dam *= 2;
                        if (!get_aim_dir(&dir)) return;
                        axe_chop(dir, dam);
                        energy_use = 100;
			break;
		}
		case 1022:
		{
			s32b dam;
                        if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_AXE) return;
                        call_lua("weapon_damages", "", "l", &dam);
                        dam += (dam / 2);
                        if (!get_aim_dir(&dir)) return;
                        mutilate_legs(dir, dam);
                        energy_use = 100;
			break;
		}
		case 1023:
		{
			s32b dam;
                        if (!combatfeat) choose_current_weapon();
			if (current_weapon->tval != TV_AXE) return;
                        call_lua("weapon_damages", "", "l", &dam);
                        dam += (dam / 2);
                        if (!get_aim_dir(&dir)) return;
                        mutilate_arms(dir, dam);
                        energy_use = 100;
			break;
		}
		case 1024:
		{
			if (!get_rep_dir(&dir)) return;
                        power_punch(dir);
                        energy_use = 100;
			break;
		}
		case 1025:
		{
			if (!get_rep_dir(&dir)) return;
                        stunning_blow(dir);
                        energy_use = 100;
			break;
		}
		case 1026:
		{
			sharpen_arrows();
                        energy_use = 100;
			break;
		}
		case 1027:
		{
			int item;
        			int y, x;
        			int tdis, tmul;
				int flg;
        			s32b tdam;
				s32b tot_to_d;
        			int ammoqty = 1;
				int tskill;

				object_type forge;
				object_type *q_ptr;

				object_type *o_ptr;
				object_type *j_ptr;

				bool hit_body = FALSE;

				char o_name[80];

        			cptr q, s;

				int msec = delay_factor * delay_factor * delay_factor;

				/* Total skill */
				tskill = p_ptr->skill[2] + (p_ptr->skill[19] + (p_ptr->skill[19] / 2));

				/* Get the "bow" (if any) */
				j_ptr = &inventory[INVEN_BOW];

				/* Require a launcher */
				if (!j_ptr->tval)
				{
					msg_print("You have nothing to fire with.");
					return;
				}
				/* Must be a bow. */
				if (!(j_ptr->sval == SV_SHORT_BOW || j_ptr->sval == SV_LONG_BOW || j_ptr->sval == SV_ELVEN_BOW
				|| j_ptr->sval == SV_HIELF_BOW || j_ptr->sval == 16))
				{
					msg_print("You must be shooting with a bow.");
					return;
				}

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

				/* Get local object */
				q_ptr = &forge;

				/* Obtain a local object */
				object_copy(q_ptr, o_ptr);

				/* Single object */
        			/*if (p_ptr->abilities[(CLASS_ARCHER * 10) + 6] >= 1 && o_ptr->tval == TV_ARROW && multishot)
        			{
                			char tmpstring[80];
                			int maxarrows = (p_ptr->abilities[(CLASS_ARCHER * 10) + 6] / 5) + 2;
                			if (maxarrows > o_ptr->number) maxarrows = o_ptr->number;
                			sprintf(tmpstring, "Shoot how many arrows?(Max: %d) ", maxarrows);
                			ammoqty = get_quantity(tmpstring, maxarrows);
                			if (ammoqty <= 0) ammoqty = 1;
        			}*/
        			q_ptr->number = ammoqty;
        
				/* Reduce and describe inventory */
        			if (item >= 0 && !(q_ptr->art_flags4 & (TR4_RETURNING)) && (p_ptr->skill[2] < 70 || ammoqty > 1))
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

				/* Base damage from thrown object plus launcher bonus */
				tdam = damroll(q_ptr->dd, q_ptr->ds);

				/* Assume a base multiplier */
				tmul = 1;

				/* Analyze the launcher */
				switch (j_ptr->sval)
				{
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
				}

				/* Get extra "power" from "extra might" */
				if (p_ptr->xtra_might) tmul++;

				tdam = tdam * tmul;
				tdam *= (tskill + 1);
				tot_to_d = j_ptr->to_d + q_ptr->to_d + (p_ptr->stat_ind[A_DEX] * 5);    
        			tdam += ((tdam * tot_to_d) / 100);
        			tdam += ((tdam * p_ptr->stat_ind[A_DEX]) / 100);

                        	flg = PROJECT_GRID | PROJECT_KILL;

                        	if (!tgt_pt(&x,&y)) return;
                        	if (distance(y,x,py,px) > 5)
                        	{
                                	msg_print("You can't target that far.");
                                	return;
                        	}
                        	else
                        	{
                                	project(0, 0, y, x, tdam, GF_PHYSICAL, flg);
                                	energy_use = 100;
                        	}
			break;
		}
		case 1028:
		{
			sharpen_bolts();
                        energy_use = 100;
			break;
		}
		case 1029:
		{
			sharpen_shots();
                        energy_use = 100;
			break;
		}
	}
}

/* Used to turn on auras/learning for Elemental Lords/Monster Mages! */
void do_cmd_turn_on_off_misc()
{
	int                     i = 0;
	int			x = 0;

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

	num = 0;

	if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1 && !(p_ptr->auraon))
        {
                powers[num] = 1;
                strcpy(power_desc[num], "Turn on your aura.");
                num++;
        }        
        if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1 && p_ptr->auraon)
        {
                powers[num] = 2;
                strcpy(power_desc[num], "Turn off your aura.");
                num++;
        }
	if ((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] >= 1 || p_ptr->pclass == CLASS_MONSTER_MAGE) && !(p_ptr->learning))
        {
                powers[num] = 3;
                strcpy(power_desc[num], "Turn on learning.");
                num++;
        }        
        if ((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] >= 1 || p_ptr->pclass == CLASS_MONSTER_MAGE) && p_ptr->learning)
        {
                powers[num] = 4;
                strcpy(power_desc[num], "Turn off learning.");
                num++;
        }
	if ((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] >= 1 || p_ptr->pclass == CLASS_MONSTER_MAGE))
        {
                strcpy(power_desc[num], "Switch magic/monster magics");
                powers[num] = 5;
		num++;
        }
	
	if (num == 0)
	{
		msg_print("Nothing to turn on/off.");
		return;
	}
                        

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
		strnfmt(out_val, 78, "(Choices %c-%c, *=List, ESC=exit) Choose an option... ",
			I2A(0), I2A(num - 1));
	}
	else
	{
		strnfmt(out_val, 78, "(Choices %c-%c, *=List, ESC=exit) Choose an option... ",
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

	if (Power == 1)
	{
		msg_format("You turn on your %s aura.", get_element_name(p_ptr->elemlord));
                p_ptr->auraon = TRUE;
	}
	if (Power == 2)
	{
		msg_format("You turn off your %s aura.", get_element_name(p_ptr->elemlord));
                p_ptr->auraon = FALSE;
	}
	if (Power == 3)
	{
		msg_print("You are now learning monster magics.");
                p_ptr->learning = TRUE;
	}
	if (Power == 4)
	{
		msg_print("You are no longer learning monster magics.");
                p_ptr->learning = FALSE;
	}
	if (Power == 5)
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

	/* Success */
	return;
}