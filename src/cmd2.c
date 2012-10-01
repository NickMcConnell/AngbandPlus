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

static bool item_tester_hook_brandable(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_WEAPON) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_AMMO) || (o_ptr->tval == TV_GLOVES) || (o_ptr->tval == TV_THROWING)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_efusion(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_WEAPON) || (o_ptr->tval == TV_RANGED) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_AMMO) || (o_ptr->tval == TV_GLOVES) || (o_ptr->tval == TV_THROWING)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_brandable_potions(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_WEAPON) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_AMMO) || (o_ptr->tval == TV_GLOVES) || (o_ptr->tval == TV_POTION) || (o_ptr->tval == TV_THROWING) || (o_ptr->tval == TV_CRYSTAL)) return (TRUE);

	/* Assume not */
	return (FALSE);
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
  
        /* Normal up stairs */
        if (c_ptr->feat == FEAT_LESS)
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
						p_ptr->inside_secret = 0;
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
					p_ptr->inside_secret = 0;
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
			p_ptr->inside_secret = 0;
		}
                else
                {
                        dun_level -= randint(3) + 1;
                        if (dun_level <= 0) dun_level = 0;
			/* Leave a quest if no event 5 is found... */
			p_ptr->inside_quest = 0;
			p_ptr->inside_secret = 0;
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
		if (dungeon_type == 200 && dun_level >= d_info[dungeon_type].maxdepth)
		{
			char k;
			msg_print("You are about to enter the last level. Proceed? [y/n]");
			k = inkey();
			if (k != 'y' && k != 'y') return;
			else msg_print(NULL);
		}

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
		if (dungeon_type == 200 && dun_level >= d_info[dungeon_type].maxdepth)
		{
			char k;
			msg_print("You are about to enter the last level. Proceed? [y/n]");
			k = inkey();
			if (k != 'y' && k != 'y') return;
			else msg_print(NULL);
		}

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

					/* Trigger an event */
					call_lua("player_enter_dungeon", "(d)", "", dungeon_type);
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

				/* Trigger an event */
				call_lua("player_enter_dungeon", "(d)", "", dungeon_type);
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
	
	bool gold = FALSE;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr = &o_list[o_idx];


	/* Gold or items? */
	if (o_ptr->sval == 1 && randint(100) >= 50) gold = TRUE;

	/* Determine how much to drop */
	if (o_ptr->sval == 6 || o_ptr->sval == 7) number = 1;
	else if (o_ptr->sval == 4 || o_ptr->sval == 5) number = randint(3) + 2;
	else if (o_ptr->sval == 3) number = randint(7) + 3;
	else if (o_ptr->sval == 2) number = randint(4) + 2;
	else number = rand_int(2) + 1;

	/* Opening a chest */
	opening_chest = TRUE;

	/* Chest type. */
	opening_chest_type = o_ptr->sval;

	/* Determine the "value" of the items */
	object_level = o_ptr->pval;

	/* Drop some objects (non-chests) */
	for (; number > 0; --number)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Small chests often drop gold */
		if (gold)
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;
		}

		/* Otherwise drop an item */
		else
		{
			/* Make an object */
			if (o_ptr->sval >= 3)
			{
				if (!make_object(q_ptr, TRUE, FALSE)) continue;
			}
			else
			{
				if (!make_object(q_ptr, FALSE, FALSE)) continue;
			}
		}

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = dun_level;

	/* No longer opening a chest */
	opening_chest_type = 0;
	opening_chest = FALSE;

	/* Empty */
	o_ptr->pval = 0;

	/* Known */
	object_known(o_ptr);
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * As of Portralis 0.4, this code has been changed.
 * there are no more traps on chests, and chests can ACTUALLY be useful!
 */
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
	object_type *o_ptr = &o_list[o_idx];
	int ppower;
	int cpower;

	/* Already open. */
	if (o_ptr->pval == 0)
	{
		msg_print("This chest is empty.");
		return (FALSE);
	}

	/* Attempt to unlock it */
	if (o_ptr->extra1 >= 1)
	{
		msg_print("You've already tried to open this chest.");
		return (FALSE);
	}

	ppower = p_ptr->stat_ind[A_DEX] + p_ptr->stat_ind[A_INT];
	cpower = (o_ptr->pval) * (o_ptr->sval - 1);

	if (p_ptr->abilities[(CLASS_ROGUE * 10) + 4] > 0) ppower = ppower + multiply_divide(ppower, p_ptr->abilities[(CLASS_ROGUE * 10) + 4] * 50, 100);

	/* Magic chests and higher REQUIRES the Lockpicking ability. */
	if (o_ptr->sval == 4 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] == 0)
	{
		msg_print("You need at least 1 point in Lockpicking to unlock this chest.");
		return (FALSE);
	}
	if (o_ptr->sval == 5 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] < 5)
	{
		msg_print("You need at least 5 points in Lockpicking to unlock this chest.");
		return (FALSE);
	}
	if (o_ptr->sval == 6 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] < 10)
	{
		msg_print("You need at least 10 points in Lockpicking to unlock this chest.");
		return (FALSE);
	}
	if (o_ptr->sval == 7 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] < 10)
	{
		msg_print("You need at least 30 points in Lockpicking to unlock this chest.");
		return (FALSE);
	}

	/* Must have enough power. */
	if (ppower < cpower)
	{
		msg_format("You need %d dex+int to attempt to open this chest. Current power: %d", cpower, ppower);
		return (FALSE);
	}

	/* Guaranteed success rate? */
	if (o_ptr->sval == 2 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] >= 10) cpower = 0;
	if (o_ptr->sval == 3 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] >= 20) cpower = 0;
	if (o_ptr->sval == 4 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] >= 40) cpower = 0;
	if (o_ptr->sval == 5 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] >= 50) cpower = 0;
	if (o_ptr->sval == 6 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] >= 75) cpower = 0;
	if (o_ptr->sval == 7 && p_ptr->abilities[(CLASS_ROGUE * 10) + 4] >= 100) cpower = 0;

	if (randint(ppower) >= randint(cpower))
	{
		/* Let the Chest drop items */
		chest_death(y, x, o_idx);
		do_cmd_save_game();
	}
	else
	{
		o_ptr->extra1 = 1;
		do_cmd_save_game();
		msg_print("You've failed to open this chest.");
		return (FALSE);
	}

	opening_chest = FALSE;
	opening_chest_type = 0;

	/* Take a turn */
	energy_use = 100;
	
	/* Result */
	return (FALSE);
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

	/* Jammed ice door */
	if (c_ptr->feat >= FEAT_ICE_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked ice door */
	else if (c_ptr->feat >= FEAT_ICE_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
                i = p_ptr->stat_ind[A_DEX];

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = c_ptr->feat - FEAT_ICE_DOOR_HEAD;

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
			cave_set_feat(y, x, FEAT_ICE_OPEN);

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

	/* Closed ice door */
	else if (c_ptr->feat == FEAT_ICE_DOOR_HEAD)
	{
		/* Set off trap */
		if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

		/* Open the door */
		cave_set_feat(y, x, FEAT_ICE_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);
		
		/* Sound */
		sound(SOUND_OPENDOOR);
	}

	/* Jammed door */
	else if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
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
	    num_doors += count_dt(&y, &x, FEAT_ICE_DOOR_HEAD, FEAT_ICE_DOOR_TAIL);
	    
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
	if (get_rep_dir_repeat(&dir))
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
		      (c_ptr->feat <= FEAT_DOOR_TAIL)) && !((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)) &&
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

	/* Take a turn */
	energy_use = 100;

	/* Get grid and contents */
	c_ptr = &cave[y][x];

	/* Set off trap */
	if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

	/* Broken door */
	if (c_ptr->feat == FEAT_BROKEN || c_ptr->feat == FEAT_ICE_BROKEN)
	{

		/* Message */
		msg_print("The door appears to be broken.");
	}

	/* Open ice door */
	else if (c_ptr->feat == FEAT_ICE_OPEN)
	{
		/* Close the door */
		cave_set_feat(y, x, FEAT_ICE_DOOR_HEAD + 0x00);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);

		/* Sound */
		sound(SOUND_SHUTDOOR);
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
		if ((count_dt(&y, &x, FEAT_OPEN, FEAT_OPEN) == 1) || (count_dt(&y, &x, FEAT_ICE_OPEN, FEAT_ICE_OPEN) == 1))
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
	if (get_rep_dir_repeat(&dir))
	{
		/* Get requested location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid and contents */
		c_ptr = &cave[y][x];

		/* Require open/broken door */
		if ((c_ptr->feat != FEAT_OPEN) && (c_ptr->feat != FEAT_BROKEN) && (c_ptr->feat != FEAT_ICE_OPEN) && (c_ptr->feat != FEAT_ICE_BROKEN))
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
	else if (c_ptr->feat == FEAT_MOUNTAIN || c_ptr->feat == FEAT_GLACIER)
	{
		msg_print("You can't tunnel through that!");
	}

	else if (c_ptr->feat == FEAT_TREES)
	{
		/* Chop Down */
                if ((randint(digging_ability()) > rand_int(200)) && twall(y, x, FEAT_GRASS))
		{
			msg_print("You have cleared away the trees.");

			/* Call lua. */
			call_lua("mining_treasures", "(ddd)", "", x, y, c_ptr->feat);
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
	else if (c_ptr->feat == FEAT_SNOW_TREES)
	{
		/* Chop Down */
                if ((randint(digging_ability()) > rand_int(200)) && twall(y, x, FEAT_SNOW))
		{
			msg_print("You have cleared away the trees.");

			/* Call lua. */
			call_lua("mining_treasures", "(ddd)", "", x, y, c_ptr->feat);
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

			/* Call lua. */
			call_lua("mining_treasures", "(ddd)", "", x, y, c_ptr->feat);
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			msg_print("You tunnel into the granite wall.");
			more = TRUE;
		}
	}
	/* Ice Wall */
	else if (c_ptr->feat == FEAT_ICE_WALL)
	{
		msg_print("Ice walls are too hard to tunnel.");
	}


	/* Quartz / Magma */
	else if ((c_ptr->feat >= FEAT_MAGMA) &&
	    (c_ptr->feat <= FEAT_QUARTZ_K))
	{
		bool okay = FALSE;
		bool hard = FALSE;

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
			/* Message */
			msg_print("You have finished the tunnel.");

			/* Call lua. */
			call_lua("mining_treasures", "(ddd)", "", x, y, c_ptr->feat);
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

			/* Call lua. */
			call_lua("mining_treasures", "(ddd)", "", x, y, c_ptr->feat);
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
	if (get_rep_dir_repeat(&dir))
	{
		/* Get location */
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* Get grid */
		c_ptr = &cave[y][x];

		/* No tunnelling through doors */
		if (((c_ptr->feat >= FEAT_DOOR_HEAD) && (c_ptr->feat <= FEAT_DOOR_TAIL)) ||
		    ((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)) ||
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
		else if (c_ptr->feat == FEAT_MOUNTAIN || c_ptr->feat == FEAT_GLACIER)
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
	      (c_ptr->feat <= FEAT_DOOR_TAIL)) && !((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) &&
	      (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)))
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

	/* Jammed ice door */
	if (c_ptr->feat >= FEAT_ICE_DOOR_HEAD + 0x08)
	{
		/* Stuck */
		msg_print("The door appears to be stuck.");
	}

	/* Locked ice door */
	else if (c_ptr->feat >= FEAT_ICE_DOOR_HEAD + 0x01)
	{
		/* Disarm factor */
                i = p_ptr->stat_ind[A_DEX];

		/* Penalize some conditions */
		if (p_ptr->blind || no_lite()) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Extract the lock power */
		j = c_ptr->feat - FEAT_ICE_DOOR_HEAD;

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
			cave_set_feat(y, x, FEAT_ICE_OPEN);

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

	/* Closed ice door */
	else if (c_ptr->feat == FEAT_ICE_DOOR_HEAD)
	{
		/* Set off trap */
		if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

		/* Open the door */
		cave_set_feat(y, x, FEAT_ICE_OPEN);

		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS);
		
		/* Sound */
		sound(SOUND_OPENDOOR);
	}

	/* Jammed door */
	else if (c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
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
	if (get_rep_dir_repeat(&dir))
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
	if (p_ptr->stat_ind[A_STR] <= 5) bash = 3;
	else bash = (p_ptr->stat_ind[A_STR] - 5) * 5;

	/* Check if it's really a door and not an ice door */
	if (c_ptr->feat >= FEAT_DOOR_HEAD && c_ptr->feat <= FEAT_DOOR_TAIL)
	{
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

		else
		{
			/* Message */
			msg_print("The door holds firm.");

			/* Allow repeated bashing */
			more = TRUE;
		}
	}
	else
	{
		/* Extract door power */
		temp = ((c_ptr->feat - FEAT_ICE_DOOR_HEAD) & 0x07);

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

				cave_set_feat(y, x, FEAT_ICE_BROKEN);
			}

			/* Open the door */
			else
			{
				/* Set off trap */
				if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);

				cave_set_feat(y, x, FEAT_ICE_OPEN);
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
		else
		{
			/* Message */
			msg_print("The door holds firm.");

			/* Allow repeated bashing */
			more = TRUE;
		}
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
	if (get_rep_dir_repeat(&dir))
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
                       c_ptr->feat > FEAT_ALTAR_TAIL) && (c_ptr->feat < FEAT_ICE_DOOR_HEAD ||
                      c_ptr->feat > FEAT_ICE_DOOR_TAIL))
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
	if (get_rep_dir_repeat(&dir))
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
			(c_ptr->feat == FEAT_MOUNTAIN) || (c_ptr->feat == FEAT_SNOW_TREES) || (c_ptr->feat == FEAT_ICE_WALL) || (c_ptr->feat == FEAT_PERM_ICE_WALL) || (c_ptr->feat == FEAT_GLACIER)))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x, dir);
		}

		/* Bash jammed doors */
		else if (((c_ptr->feat >= FEAT_DOOR_HEAD + 0x08) &&
		    (c_ptr->feat < FEAT_MINOR_GLYPH)) || ((c_ptr->feat >= FEAT_ICE_DOOR_HEAD + 0x08) && (c_ptr->feat < 120)))
		{
			/* Tunnel */
			more = do_cmd_bash_aux(y, x, dir);
		}

		/* Open closed doors */
		else if (((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		    (c_ptr->feat < FEAT_MINOR_GLYPH)) || ((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) && (c_ptr->feat < 120)))
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
		      (c_ptr->feat <= FEAT_DOOR_TAIL)) && !((c_ptr->feat >= FEAT_ICE_DOOR_HEAD) &&
		      (c_ptr->feat <= FEAT_ICE_DOOR_TAIL)))
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

			if (c_ptr->feat >= FEAT_ICE_DOOR_HEAD && c_ptr->feat <= FEAT_ICE_DOOR_TAIL)
			{
				/* Convert "locked" to "stuck" XXX XXX XXX */
				if (c_ptr->feat < FEAT_ICE_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

				/* Add one spike to the door */
				if (c_ptr->feat < FEAT_ICE_DOOR_TAIL) c_ptr->feat++;
			}
			else
			{
				/* Convert "locked" to "stuck" XXX XXX XXX */
				if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

				/* Add one spike to the door */
				if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;
			}

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
	if (get_rep_dir_repeat(&dir))
	{
		/* Take a turn */
		energy_use = 100;

		/* Actually move the character */
                move_player(dir, pickup);

		/* Allow more walking */
		more = TRUE;
	}

	if (p_ptr->leaving) energy_use = 0;

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

	/* Call a lua event. */
	call_lua("player_skip_turn", "", "");

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
        int reducer = 1 + ((p_ptr->pclass == CLASS_MARKSMAN)?(p_ptr->lev / 10):0);

	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
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
		case TV_WAND:
		case TV_SPIKE:
		{
			return (25);
		}

		case TV_AMMO:
		{
                        return (25 / reducer);
		}
	}

	/* Rarely break */
	return (10);
}


/* No more code here, it serves only to call the lua code. */
void do_cmd_fire()
{
	call_lua("ranged_shoot", "", "");
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
		int ppower;
		int mpower;

		ppower = p_ptr->abilities[(CLASS_ROGUE * 10) + 1] * (20 + multiply_divide(p_ptr->skill[6], 3, 100) + multiply_divide(p_ptr->stat_ind[A_DEX], 3, 100));
		mpower = m_ptr->dex + m_ptr->level;

                /* Failure check */
                if (randint(ppower) < randint(mpower))
                {
                        /* Take a turn */
                        energy_use = 100;

                        /* Wake up */
                        m_ptr->csleep = 0;

                        screen_load();

                        msg_print("You have failed to steal the item!");

                        return;
                }
		
                /* Reconnect the objects list */
                if(k > 0) o_list[monst_list[k - 1]].next_o_idx = monst_list[k + 1];
                if(k + 1 >= num && !(k == 0 && num == 1)) o_list[monst_list[k - 1]].next_o_idx = 0;
                if(k == 0) m_ptr->hold_o_idx = monst_list[k + 1];
                if(num == 1) m_ptr->hold_o_idx = 0;

                /* Get the item */
                o_ptr = &forge;

                /* Special handling for gold */
                if(o_list[item].tval == TV_GOLD)
                {
                        /* Collect the gold */
                        p_ptr->au += o_list[item].pval + multiply_divide(o_list[item].pval, p_ptr->abilities[(CLASS_ROGUE * 10) + 1] * 20, 100);
  
                        /* Redraw gold */
                        p_ptr->redraw |= (PR_GOLD);
  
                        /* Window stuff */
                        p_ptr->window |= (PW_PLAYER);
                }
                else
                {
                        object_copy(o_ptr, &o_list[item]);

			call_lua("stolen_item_enhance", "O", "", o_ptr);

                        inven_carry(o_ptr, FALSE);
                }

                /* Delete it */
                o_list[item].k_idx = 0;
        }

        screen_load();
	do_cmd_redraw();

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

	/* Mining skill */
	if (p_ptr->skill[29] >= 1)
	{
		if (randint(100) <= p_ptr->skill[29])
		{
			abil = 30000;
		}
	}

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
		if (cave[y][x].info & CAVE_LITE)
		{
                	if((m_idx=place_monster_one_simulacrum(y, x, o_ptr->pval, FALSE, TRUE, monlvl, monhp, mondur))==0) msg_print("Summon failed.");
		}
		else msg_print("You can only use simulacrum on lit grids.");
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
          if (((cave[ij][ii].feat != FEAT_TREES) && (cave[ij][ii].feat != FEAT_SNOW_TREES)) || (distance(ij,ii,py,px) > 5 + (p_ptr->abilities[(CLASS_RANGER * 10) + 5])))
          {
                msg_print("You can't teleport there...");
          }
          else teleport_player_to(ij,ii);
}

int get_a_dir()
{
        char ch; 
	cptr p;      
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
                if (ch == '5')
                {
                        return(5);
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
        fire_ball(GF_LITE, dir, 0, 5 + p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 3]);
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
        amt = r_ptr->mind;
        amt = amt + ((amt * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 4] * 10)) / 100);
        p_ptr->wis_boost = amt;
	set_wis_boost(2);

        o_ptr->timeout = 15;
        update_and_handle();
        energy_use = 100;
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
				damage += multiply_divide(damage, (p_ptr->stat_ind[A_DEX] * 5), 100);
				damage += multiply_divide(damage, p_ptr->stat_ind[A_DEX], 100);
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
	if (two_weapon_wield() && p_ptr->dualwield == 1)
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
	else current_weapon = &inventory[INVEN_WIELD]; /* <- Thanks Adelie. :) */
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
				summon_specific_kind(py, px, 1, 33, FALSE, TRUE, p_ptr->abilities[(CLASS_MAGE * 10) + 5] + 10);
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
			msg_print("Ability 13 has no effects.");
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
		/* 17 is free! */
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
                        (void)set_fast((4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5])) / 2);
                        (void)set_blessed(4 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 5]));
                        p_ptr->pres = p_ptr->abilities[(CLASS_PRIEST * 10) + 5];
			if (p_ptr->pres > 75) p_ptr->pres = 75;
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
			p_ptr->ac_boost = p_ptr->stat_ind[A_WIS] * p_ptr->abilities[(CLASS_PRIEST * 10) + 8];
                        (void)set_ac_boost(p_ptr->abilities[(CLASS_PRIEST * 10) + 8] + 5);
                        energy_use = 100;
			break;
		}
		
		case 24:
		{
			do_cmd_steal();
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
		case 31:
		{
			int animalsnum;
			msg_print("You call animals to your aid!");
			for (animalsnum = 0; animalsnum < p_ptr->abilities[(CLASS_RANGER * 10) + 4]; animalsnum++)
			{
				summon_specific_rflag(py, px, p_ptr->abilities[(CLASS_RANGER * 10) + 4], RF3_ANIMAL, FALSE, TRUE, (p_ptr->abilities[(CLASS_RANGER * 10) + 4]) + 10);
			}
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
			p_ptr->str_boost = (p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 20);
                        p_ptr->dex_boost = (p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 20);
                        p_ptr->ac_boost = multiply_divide(p_ptr->dis_ac, p_ptr->abilities[(CLASS_PALADIN * 10) + 6] * 10, 100);
                        (void)set_ac_boost(2);
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
			mdam += multiply_divide(mdam, ((p_ptr->abilities[(CLASS_MONK * 10) + 1] - 1) * 20), 100);
                        spin_kick(mdam, 1);
                        energy_use = 100;
			break;
		}
		case 45:
		{
			s32b mdam;
			call_lua("monk_damages", "", "l", &mdam);
			mdam += multiply_divide(mdam, ((p_ptr->abilities[(CLASS_MONK * 10) + 2] - 1) * 10), 100);
                        if (!get_rep_dir(&dir)) return;
                        hard_kick(dir, mdam, 3 + (p_ptr->abilities[(CLASS_MONK * 10) + 2] / 10));
                        energy_use = 100;
			break;
		}
		case 46:
		{
			s32b mdam;
			call_lua("monk_damages", "", "l", &mdam);
			mdam += multiply_divide(mdam, ((p_ptr->abilities[(CLASS_MONK * 10) + 5] - 1) * 10), 100);
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
			multiply_divide(mdam, ((p_ptr->abilities[(CLASS_MONK * 10) + 8] - 1) * 20), 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, mdam, 0, 1);
                        msg_print("Jump where? ");
                        if (!tgt_pt(&x,&y)) return;
                        if (!cave_empty_bold(y,x) || (distance(y,x,py,px) > 3))
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
		
		case 49:
		{
			s32b dam;
                        object_type *o_ptr = &inventory[INVEN_AMMO];
                        object_type *q_ptr = &inventory[INVEN_BOW];
                        if (!q_ptr) {msg_print("You need a shooter!"); return;}
                        if (!o_ptr) {msg_print("You need ammos!"); return;}
                        dam = damroll(o_ptr->dd, o_ptr->ds);
			/*dam = bow_damages(dam, q_ptr->to_d, o_ptr->to_d);*/
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] * 10), 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, (p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] + 4));
                        inven_item_increase(INVEN_AMMO, -1);
                        inven_item_describe(INVEN_AMMO);
                        inven_item_optimize(INVEN_AMMO);
                        energy_use = 100;
			
			break;
		}
		case 50:
		{
			do_cmd_fire(TRUE);
			break;
		}
		case 51:
		{
			s32b dam;
                        object_type *o_ptr = &inventory[INVEN_AMMO];
                        object_type *q_ptr = &inventory[INVEN_BOW];
                        if (!q_ptr) {msg_print("You need a crossbow!"); return;}
                        if (o_ptr->itemskill != 20) {msg_print("You need bolts!"); return;}
                        dam = damroll(o_ptr->dd, o_ptr->ds);
			/*dam = bow_damages(dam, q_ptr->to_d, o_ptr->to_d);*/
			dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_MARKSMAN * 10) + 7] - 1) * 10), 100);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir, dam, (p_ptr->abilities[(CLASS_MARKSMAN * 10) + 7] / 30)+1);
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
			int item;
        		object_type             *o_ptr;
        		cptr q, s;
        		u32b f1, f2, f3, f4;

        		/* Restrict choices to melee weapons */
        		item_tester_tval = TV_WEAPON;

        		/* Get an item */
        		q = "Enchant which weapon? ";
        		s = "You have no valid weapons!";
        		if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
        		msg_print("Damages type changed to your element!");
        		o_ptr->extra1 = p_ptr->elemlord;
                        energy_use = 100;
			break;
		}
		case 56:
		{
			if (!(choose_command_element_status())) return;

                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_COMMAND_ELEMENT, dir, 100, (p_ptr->abilities[(CLASS_ELEM_LORD * 10)] / 20)+3);                                                
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
		case 60:
		{
			monstrous_wave();
			break;
		}
		case 61:
		{
			s32b dam;
                        int dis;
			object_type *q_ptr = &inventory[INVEN_WIELD];
			object_type *q2_ptr = &inventory[INVEN_WIELD+1];

			/* This determines casting power */
			spellstat = (p_ptr->stat_ind[A_STR] - 5);

			/* Choose a shield. */
			if (q_ptr->tval == TV_SHIELD && q2_ptr->tval == TV_SHIELD) choose_current_weapon();
			else
			{
				if (q_ptr->tval == TV_SHIELD) current_weapon = &inventory[INVEN_WIELD];
				else if (q2_ptr->tval == TV_SHIELD) current_weapon = &inventory[INVEN_WIELD+1];
				else
				{
					msg_print("You must wield a shield.");
					return;
				}
			}

			/* No lower than 0. */
			if (spellstat < 0) spellstat = 0;
                        dam = ((current_weapon->ac * p_ptr->abilities[(CLASS_DEFENDER * 10) + 2]) * spellstat);
			dam += multiply_divide(dam, p_ptr->dis_to_d, 100);
			dam += multiply_divide(dam, p_ptr->stat_ind[A_STR], 100);
                        dis = 2 + (p_ptr->abilities[(CLASS_DEFENDER * 10) + 2] / 20);
                        if (!get_rep_dir(&dir)) return;
                        smash(dir, dam, dis);
                        energy_use = 100;
			break;
		}
		case 63:
		{
			s32b dam;

			object_type *q_ptr = &inventory[INVEN_WIELD];
			object_type *q2_ptr = &inventory[INVEN_WIELD+1];

			/* Choose a shield. */
			if (q_ptr->tval == TV_SHIELD && q2_ptr->tval == TV_SHIELD) choose_current_weapon();
			else
			{
				if (q_ptr->tval == TV_SHIELD) current_weapon = &inventory[INVEN_WIELD];
				else if (q2_ptr->tval == TV_SHIELD) current_weapon = &inventory[INVEN_WIELD+1];
				else
				{
					msg_print("You must wield a shield.");
					return;
				}
			}

                        dam = ((current_weapon->ac + current_weapon->to_a) * p_ptr->skill[3]) * p_ptr->abilities[(CLASS_DEFENDER * 10) + 8];
			dam += multiply_divide(dam, p_ptr->dis_to_d, 100);
			dam += multiply_divide(dam, p_ptr->stat_ind[A_STR], 100);
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_PHYSICAL, dir, dam);
                        energy_use = 100;
			break;
		}
		
		case 66:
		{
			justice_bless_weapon();
                        energy_use = 100;
			break;
		}
		
		
		/* 70 is free */
		case 71:
		{
			s32b dam;
			call_lua("monk_damages", "", "l", &dam);
			dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_ZELAR * 10)] - 1) * 40), 100);
                        attack_aura(GF_MANA, dam, 2 + (p_ptr->abilities[(CLASS_ZELAR * 10)] / 20));
                        energy_use = 100;
			break;
		}
		case 72:
		{
			s32b dam;
                        int range = 10 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 1] / 2);
			call_lua("monk_damages", "", "l", &dam);
			dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_ZELAR * 10) + 1] - 1) * 50), 100);
                        if (!get_rep_dir(&dir)) return;
                        chain_attack(dir, GF_MANA, dam, 0, range);
                        energy_use = 100;
			break;
		}
		case 73:
		{
			s32b dam;
			call_lua("monk_damages", "", "l", &dam);
			dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_ZELAR * 10) + 3] - 1) * 20), 100);
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
				dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_ZELAR * 10) + 7] - 1) * 5), 100);
                                dir = get_a_dir();
                                chain_attack(dir, GF_MANA, dam, 0, 10);
                        }
                        {
                                s32b dam;
				call_lua("monk_damages", "", "l", &dam);
				dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_ZELAR * 10) + 7] - 1) * 5), 100);
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
                                s32b dam = (p_ptr->skill[6] * spellstat);
				dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_SHADOW * 10) + 3] * 20), 100);
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
                        	rad = 5 + (p_ptr->abilities[(CLASS_SHADOW * 10) + 5]);
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
                                                	msg_print("You phase in shadows!");
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
			energy_use = 100;
			break;
		}
		case 91:
		{
			if (p_ptr->tim_invisible >= 1)
                        {
                        
                        s32b dam;
                        int x, y, flg, rad;

                        flg = PROJECT_GRID | PROJECT_KILL;

			if (!combatfeat) choose_current_weapon();
			call_lua("weapon_damages", "", "l", &dam);
			dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_SHADOW * 10) + 9]) * 20), 100);

			rad = 1 + (p_ptr->abilities[(CLASS_SHADOW * 10) + 9] / 10);
                        if (!tgt_pt(&x,&y)) return;
                        if (distance(y,x,py,px) > 5)
                        {
                                msg_print("You can't target that far.");
                                return;
                        }
                        else
                        {
				stormshadow = TRUE;
                                project(0, rad, y, x, dam, GF_DARK, flg);
				stormshadow = FALSE;
                                energy_use = 100;
                        }
                        }
                        else msg_print("You can only use this ability while under temporary invisibility!");
			break;
		}
		case 100:
		{
			essence_transfer();
			update_and_handle();
			energy_use = 100;
			break;
		}
		case 101:
		{
			defense_transfer();
			update_and_handle();
			energy_use = 100;
			break;
		}
		case 102:
		{
			etch_runes();
			update_and_handle();
			energy_use = 100;
			break;
		}
		case 103:
		{
			essence_fusion();
			update_and_handle();
			energy_use = 100;
			break;
		}
		case 104:
		{
			combine_items(TRUE);
			update_and_handle();
			energy_use = 100;
			break;
		}

		case 107:
		{
			diviner_wish();
			update_and_handle();
			energy_use = 100;
			break;
		}

		/* Feats */
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
			int inv_dur;

			inv_dur = 10 + (p_ptr->skill[6] / 10);
			msg_print("You hide nearby...");
                        (void)set_invis(inv_dur, 5);
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
			melee_attack = TRUE;
			no_magic_return = TRUE;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, 1);
			melee_attack = FALSE;
			no_magic_return = FALSE;
                        energy_use = 100;
			break;
		}
		case 1010:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_OLD_HEAL, dir, 1000 * p_ptr->skill[9], 3);
                        energy_use = 100;
			break;
		}
		case 1011:
		{
			attack_aura(GF_MORALE_BOOST, 0, 5);
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
			combine_items(FALSE);
                        energy_use = 100;
			break;
		}
		case 1014:
		{
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->itemskill != 12) return;
                        sword_spin();
                        energy_use = 100;
			break;
		}
		case 1015:
		{
			s32b dam;
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->itemskill != 13) return;
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
			if (current_weapon->itemskill != 13) return;
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
			if (current_weapon->itemskill != 13) return;
                        if (!get_rep_dir(&dir)) return;
                        shattering_blow(dir);
                        energy_use = 100;
			break;
		}
		case 1018:
		{
			s32b dam;
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->itemskill != 14) return;
			call_lua("weapon_damages", "", "l", &dam);
                        dam *= 2;
                        if (!get_rep_dir(&dir)) return;
			melee_attack = TRUE;
			no_magic_return = TRUE;
                        chain_attack(dir, GF_PHYSICAL, dam, 0, 2);
			melee_attack = FALSE;
			no_magic_return = FALSE;
                        energy_use = 100;
			break;
		}
		case 1019:
		{
			if (!combatfeat) choose_current_weapon();
			if (current_weapon->itemskill != 15) return;
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
			if (current_weapon->itemskill != 16) return;
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
			if (current_weapon->itemskill != 16) return;
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
			if (current_weapon->itemskill != 16) return;
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
			int flg;
        		s32b tdam;
        		int ammoqty = 1;
			int tskill;

			object_type *o_ptr;
			object_type *j_ptr;

			u32b            f1, f2, f3, f4;

			char o_name[80];

        		cptr q, s;

			/* Total skill */
			tskill = p_ptr->skill[2] + (p_ptr->skill[19] + (p_ptr->skill[19] / 2));

			/* Get the "bow" (if any) */
			j_ptr = &inventory[INVEN_WIELD];
			current_weapon = &inventory[INVEN_WIELD];
			if ((j_ptr->tval != TV_RANGED) || (j_ptr->itemtype != 1))
			{
				j_ptr = &inventory[INVEN_WIELD+1];
				current_weapon = &inventory[INVEN_WIELD+1];
			}

			/* Require a launcher */
			if ((j_ptr->tval != TV_RANGED) || (j_ptr->itemtype != 1))
			{
				msg_print("You must wield a bow to use this ability.");
				return;
			}

        		/* Get the "ammo" (if any) */
        		o_ptr = &inventory[INVEN_AMMO];

        		item = INVEN_AMMO;

        		/* Check ammo type. */
        		if ((o_ptr->itemtype != j_ptr->itemtype) || (!o_ptr->k_idx))
			{
				msg_print("You must use arrows.");
				return;
			}

			/* Damages. */
			call_lua("ranged_damages", "", "l", &tdam);

                        flg = PROJECT_GRID | PROJECT_KILL;

                        if (!tgt_pt(&x,&y)) return;
                        if (distance(y,x,py,px) > 5)
                        {
                                msg_print("You can't target that far.");
                                return;
                        }
                        else
                        {
				int elem;

				if (j_ptr->extra1 != 0) elem = j_ptr->extra1;
				else if (o_ptr->extra1 != 0) elem = o_ptr->extra1;
				else elem = GF_PHYSICAL;

                                project(0, 0, y, x, tdam, elem, flg);

				/* Reduce and describe inventory */
				object_flags(o_ptr, &f1, &f2, &f3, &f4);
				
				if (!(f4 & (TR4_RETURNING)) && !(p_ptr->skill[2] >= 70))
				{

					if (randint(100) > o_ptr->extra2)
					{
						drop_ranged = o_ptr;
						dropnum = 1;
						drop_near_ammo(drop_ranged, dropnum, y, x);
					}

					if (item >= 0)
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
				}
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
		case 1030:
		{
			alchemy_brand();
                        energy_use = 100;
			break;
		}
		case 1031:
		{
			alchemy_resist();
                        energy_use = 100;
			break;
		}
		case 20000:
		{
			use_monster_special_attack(p_ptr->body_monster);
			break;
		}
		case 20001:
		{
			if (p_ptr->events[29015] == 0)
			{
				p_ptr->events[29015] = 1;
				msg_print("Essences capturing is now On.");
			}
			else
			{
				p_ptr->events[29015] = 0;
				msg_print("Essences capturing is now Off.");
			}
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
	if (p_ptr->prace == RACE_MONSTER)
        {
                powers[num] = 8;
                strcpy(power_desc[num], "Switch unarmed melee attack.");
                num++;
        }
	if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29030] == 0)
        {
                powers[num] = 11;
                strcpy(power_desc[num], "Turn On auto ranged weapon special attack(if any).");
                num++;
        }
	if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29030] == 1)
        {
                powers[num] = 11;
                strcpy(power_desc[num], "Turn Off auto ranged weapon special attack(if any).");
                num++;
        }
	if (p_ptr->prace == RACE_MONSTER && !(p_ptr->auraon))
        {
                powers[num] = 6;
                strcpy(power_desc[num], "Turn On passive abilities(if any).");
                num++;
        }        
        if (p_ptr->prace == RACE_MONSTER && p_ptr->auraon)
        {
                powers[num] = 7;
                strcpy(power_desc[num], "Turn Off passive abilities(if any).");
                num++;
        }
	if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29027] == 1)
        {
                powers[num] = 10;
                strcpy(power_desc[num], "Turn Off main essence unarmed bonus.");
                num++;
        }
	if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29027] == 0)
        {
                powers[num] = 10;
                strcpy(power_desc[num], "Turn On main essence unarmed bonus.");
                num++;
        }
	if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29030] == 1)
        {
                powers[num] = 12;
                strcpy(power_desc[num], "Switch auto ranged special attack.");
                num++;
        }
	if (p_ptr->skill[28] > 0 && p_ptr->events[29040] != 0 && p_ptr->events[29041] == 0)
        {
                powers[num] = 13;
                strcpy(power_desc[num], "Turn On Song.");
                num++;
        }
	if (p_ptr->skill[28] > 0 && p_ptr->events[29040] != 0 && p_ptr->events[29041] == 1)
        {
                powers[num] = 13;
                strcpy(power_desc[num], "Turn Off Song.");
                num++;
        }
	if (p_ptr->abilities[(CLASS_BARD * 10) + 7] > 0 && p_ptr->events[29042] == 1)
        {
                powers[num] = 14;
                strcpy(power_desc[num], "Turn Off War Songs.");
                num++;
        }
	if (p_ptr->abilities[(CLASS_BARD * 10) + 7] > 0 && p_ptr->events[29042] == 0)
        {
                powers[num] = 14;
                strcpy(power_desc[num], "Turn On War Songs.");
                num++;
        }
	if (p_ptr->abilities[(CLASS_BARD * 10) + 8] > 0 && p_ptr->events[29043] == 1)
        {
                powers[num] = 15;
                strcpy(power_desc[num], "Turn Off Enthralling Songs.");
                num++;
        }
	if (p_ptr->abilities[(CLASS_BARD * 10) + 8] > 0 && p_ptr->events[29043] == 0)
        {
                powers[num] = 15;
                strcpy(power_desc[num], "Turn On Enthralling Songs.");
                num++;
        }
	if (p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] > 0 && p_ptr->events[29045] == 1)
        {
                powers[num] = 16;
                strcpy(power_desc[num], "Turn Off Counter Shot.");
                num++;
        }
	if (p_ptr->abilities[(CLASS_MARKSMAN * 10) + 3] > 0 && p_ptr->events[29045] == 0)
        {
                powers[num] = 16;
                strcpy(power_desc[num], "Turn On Counter Shot.");
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
	if (Power == 6)
	{
		msg_format("You turn on your special powers.");
                p_ptr->auraon = TRUE;
	}
	if (Power == 7)
	{
		msg_format("You turn off your special powers.");
                p_ptr->auraon = FALSE;
	}
	if (Power == 8)
	{
		select_monster_melee();
	}
	if (Power == 11)
	{
		if (p_ptr->events[29030] == 1)
                {
                        p_ptr->events[29030] = 0;
                        msg_print("Auto ranged special turned Off.");
                }
                else
                {
                        p_ptr->events[29030] = 1;
                        msg_print("Auto ranged special turned On.");
                }
	}
	if (Power == 12)
	{
		select_monster_ranged();
	}
	if (Power == 13)
	{
		if (p_ptr->events[29041] == 1)
                {
                        p_ptr->events[29041] = 0;
                        msg_print("Song turned Off.");
                }
                else
                {
                        p_ptr->events[29041] = 1;
                        msg_print("Song turned On.");
                }
	}
	if (Power == 14)
	{
		if (p_ptr->events[29042] == 1)
                {
                        p_ptr->events[29042] = 0;
                        msg_print("War Songs turned Off.");
                }
                else
                {
                        p_ptr->events[29042] = 1;
                        msg_print("War Songs turned On.");
                }
	}
	if (Power == 15)
	{
		if (p_ptr->events[29043] == 1)
                {
                        p_ptr->events[29043] = 0;
                        msg_print("Enthralling Songs turned Off.");
                }
                else
                {
                        p_ptr->events[29043] = 1;
                        msg_print("Enthralling Songs turned On.");
                }
	}
	if (Power == 16)
	{
		if (p_ptr->events[29045] == 1)
                {
                        p_ptr->events[29045] = 0;
                        msg_print("Counter Shot turned Off.");
                }
                else
                {
                        p_ptr->events[29045] = 1;
                        msg_print("Counter Shot turned On.");
                }
	}

	/* Success */
	return;
}

/* Reload a ranged weapon. */
void reload_ranged()
{
	int             item;
	object_type     *o_ptr;
        u32b            f1, f2, f3, f4;
	char            o_name[80];
	char            buf[128];
	cptr            q, s;

	/* First, look at the wielded weapons. */
	o_ptr = &inventory[INVEN_WIELD];

	if ((o_ptr->tval == TV_RANGED) && (o_ptr->pval2 < o_ptr->extra3))
	{
		object_desc(buf, o_ptr, 1, 0);
                msg_format("%s has been reloaded!", buf);
		o_ptr->pval2 = o_ptr->extra3;
		energy_use = 100;
		update_and_handle();
		return;
	}

	o_ptr = &inventory[INVEN_WIELD+1];

	if ((o_ptr->tval == TV_RANGED) && (o_ptr->pval2 < o_ptr->extra3))
	{
		object_desc(buf, o_ptr, 1, 0);
                msg_format("%s has been reloaded!", buf);
		o_ptr->pval2 = o_ptr->extra3;
		energy_use = 100;
		update_and_handle();
		return;
	}

	/* Otherwise, reload a selected weapon in inventory. */
	item_tester_tval = TV_RANGED;

	/* Get an item */
	q = "Reload which weapon? ";
	s = "You have weapons to reload.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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

	o_ptr->pval2 = o_ptr->extra3;
	msg_print("The weapon has been reloaded!");

	energy_use = 100;
	update_and_handle();
}

/* Choose an item to throw. */
bool throw_select()
{
	int item;
	bool force = FALSE;
	object_type *o_ptr;
	object_type forge;
	u32b f1, f2, f3, f4;

	cptr q, s;

	/* Get an item */
	if (p_ptr->events[29044] == 1)
	{
		item = INVEN_AMMO;
		o_ptr = &inventory[item];
	}
	else
	{
		q = "Throw which item? ";
		s = "You have no items to throw.";
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

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
	}

	/* Object selected. */
	/*drop_ranged = &forge;
	object_prep(drop_ranged, o_ptr->tval);
	object_copy(drop_ranged, o_ptr);*/

	drop_ranged = o_ptr;
	throw_item = item;
	if (item >= 0) throw_floorpack = 0;
	else throw_floorpack = 1;

	/* Destroy item...or not? */
	/*object_flags(o_ptr, &f1, &f2, &f3, &f4);*/

	/*if (!(f4 & (TR4_RETURNING)))
	{
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}*/

	return (TRUE);
}

/* Alchemic branding of an item. */
void alchemy_brand()
{
	int item, weapon;
	s32b maxbranddam;
	s32b potionpower;
	object_type *o_ptr;
	object_type *pot_ptr;

	cptr q, s;

	item_tester_tval = TV_POTION;

	/* Choose a potion. */
	q = "Use which potion? ";
	s = "You have no potions.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		pot_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		pot_ptr = &o_list[0 - item];
	}

	item_tester_tval = 0;
	item_tester_hook = item_tester_hook_brandable;

	/* Choose a weapon. */
	q = "Brand which weapon? ";
	s = "You have no weapons.";
	if (!get_item(&weapon, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (weapon >= 0)
	{
		o_ptr = &inventory[weapon];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - weapon];
	}

	/* Brand the weapon. */
	maxbranddam = p_ptr->skill[10] * 100;
	call_lua("potion_power", "(O)", "d", pot_ptr, &potionpower);
	potionpower = potionpower / 4;
	if (potionpower > maxbranddam) potionpower = maxbranddam;
	
	o_ptr->branddam = potionpower;
	o_ptr->brandtype = pot_ptr->brandtype;
	o_ptr->brandrad = 0;

	msg_print("Your weapon has been branded!");

	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return;
}

/* Use Alchemy to add resistances to items. */
void alchemy_resist()
{
	int item, item2, i;
	s16b resistsum, maxresist, resamount;
	object_type *o_ptr;
	object_type *pot_ptr;

	cptr q, s;

	item_tester_tval = TV_POTION;

	/* Choose a potion. */
	q = "Use which potion? ";
	s = "You have no potions.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		pot_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		pot_ptr = &o_list[0 - item];
	}

	item_tester_tval = 0;

	/* Choose an item. */
	q = "Apply to which item? ";
	s = "You have no items.";
	if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item2 >= 0)
	{
		o_ptr = &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item2];
	}

	/* Apply resistances(if possible) */
	maxresist = p_ptr->skill[10] / 4;
	
	resistsum = 0;
	for (i = 0; i < MAX_RESIST; i++)
	{
		resistsum += o_ptr->resistances[i];
	}

	if (resistsum >= maxresist)
	{
		msg_print("This item's total resistances exceeds the maximum you can apply for now.");
		return;
	}

	resamount = pot_ptr->branddam / 10;
	if ((maxresist - resistsum) > 0) maxresist = maxresist - resistsum;
	else maxresist = 0;
	if (resamount > maxresist) resamount = maxresist;
	
	o_ptr->resistances[pot_ptr->brandtype] += resamount;
	if (o_ptr->resistances[pot_ptr->brandtype] > 100) o_ptr->resistances[pot_ptr->brandtype] = 100;

	if (resamount > 0) msg_print("Resistances were added to the item.");
	else msg_print("No additional resistances were added to this item.");

	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return;
}

/* Essence Transfer ability. */
void essence_transfer()
{
	int item, item2;
	s32b maxbranddam;
	s32b maxpotionpower;
	s32b newbranddam;
	object_type *o_ptr;
	object_type *q_ptr;
	u32b f1, f2, f3, f4;
	u32b f11, f22, f33, f44;

	cptr q, s;

	item_tester_hook = item_tester_hook_brandable_potions;

	/* Choose a potion. */
	q = "Transfer from? ";
	s = "You have no valid items.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

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

	/* Is it branded? */
	if (o_ptr->branddam == 0)
	{
		msg_print("This item has no brands.");
		return;
	}

	object_flags(o_ptr, &f11, &f22, &f33, &f44);

	item_tester_hook = item_tester_hook_brandable_potions;

	/* Choose a weapon. */
	q = "Transfer to which item? ";
	s = "You have no valid items.";
	if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item2 >= 0)
	{
		q_ptr = &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		q_ptr = &o_list[0 - item2];
	}

	object_flags(q_ptr, &f1, &f2, &f3, &f4);

	if (!(f4 & (TR4_CRAFTED)) && q_ptr->tval != TV_POTION && q_ptr->tval != TV_CRYSTAL)
	{
		msg_print("The item must be an enchanted crafted item, a potion or a crystal.");
		return;
	}

	/* Begin the transfer. */
	maxbranddam = p_ptr->abilities[(CLASS_ENCHANTER * 10) + 2] * 1000;
	maxpotionpower = p_ptr->abilities[(CLASS_ENCHANTER * 10) + 2] * 10;

	newbranddam = o_ptr->branddam;
	if (newbranddam > maxbranddam) newbranddam = maxbranddam;

	if (q_ptr->tval == TV_POTION || q_ptr->tval == TV_CRYSTAL)
	{
		if (newbranddam > maxpotionpower) newbranddam = maxpotionpower;
		q_ptr->branddam = newbranddam;
		q_ptr->brandtype = o_ptr->brandtype;

		/* If the original item had "MODERATE_POWER", give it to the target. */
		if (f44 & (TR4_MODERATE_POWER))
		{
			q_ptr->art_flags4 |= (TR4_MODERATE_POWER);
		}
	}
	else
	{
		q_ptr->branddam = newbranddam;
		q_ptr->brandtype = o_ptr->brandtype;
		q_ptr->brandrad = o_ptr->brandrad;
	}

	msg_print("The essences were transferred!");

	/* Remove the brand from the first item. */
	/* If it's a potion, destroy it instead. */
	if (o_ptr->tval == TV_POTION)
	{
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}
	else
	{
		o_ptr->branddam = 0;
		o_ptr->brandtype = 0;
		o_ptr->brandrad = 0;
	}

	return;
}

/* Defense Transfer ability. */
void defense_transfer()
{
	int item, item2;
	s32b maxbranddam;
	s32b maxpotionpower;
	s32b newbranddam;
	object_type *o_ptr;
	object_type *q_ptr;
	object_kind *k_ptr;
	u32b f1, f2, f3, f4;

	cptr q, s;

	/* Choose an original item. */
	q = "Transfer from? ";
	s = "You have no items.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

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

	/* Does it has base AC? */
	if (o_ptr->ac <= 0)
	{
		msg_print("This item provides no base AC.");
		return;
	}

	/* Do we have the proper ability level? */
	k_ptr = &k_info[o_ptr->k_idx];
	if (k_ptr->level > (p_ptr->abilities[(CLASS_ENCHANTER * 10) + 3] * 4))
	{
		msg_print("You cannot transfer from this item yet.");
		return;
	}

	/* Choose a target. */
	q = "Transfer to which item? ";
	s = "You have no valid items.";
	if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item2 >= 0)
	{
		q_ptr = &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		q_ptr = &o_list[0 - item2];
	}

	object_flags(q_ptr, &f1, &f2, &f3, &f4);

	if (!(f4 & (TR4_CRAFTED)))
	{
		msg_print("The item must be an enchanted crafted item.");
		return;
	}

	/* Begin the transfer. */
	if (q_ptr->tval == TV_SOFT_ARMOR || q_ptr->tval == TV_HARD_ARMOR || q_ptr->tval == TV_DRAG_ARMOR)
	{
		q_ptr->ac = o_ptr->ac;
	}
	else
	{
		q_ptr->ac = o_ptr->ac / 4;
	}

	msg_print("The base AC transfer was successful!");

	o_ptr->ac = 0;

	return;
}

/* Etch Runes ability. */
void etch_runes()
{
	int item;
	int spellnum;
	int ispellnum;
	int i;
	object_type *o_ptr;
	object_kind *k_ptr;
	u32b f1, f2, f3, f4;
	magic_spells *spell_ptr;

	cptr q, s;

	/* First we needs to pick a spell. */
	spellnum = pick_spell();

	if (spellnum <= 0) return;

	spell_ptr = &magic_spell[spellnum];

	/* What kind of spell did we pick? Not all are valid. */
	if (!(spell_ptr->effect[0] == 1 && (spell_ptr->shape[0] == 1 || spell_ptr->shape[0] == 2))
	&& !(spell_ptr->effect[0] == 2 && (spell_ptr->shape[0] == 1 || spell_ptr->shape[0] == 2))
	&& !(spell_ptr->effect[0] == 3)
	&& !(spell_ptr->effect[0] == 6)
	&& !(spell_ptr->effect[0] == 13)
	&& !(spell_ptr->effect[0] == 14))
	{
		msg_print("You cannot enchant your item with that spell.");
		return;
	}

	/* Choose an original item. */
	q = "Add the spell to which item? ";
	s = "You have no items.";
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

	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (!(f4 & (TR4_CRAFTED)))
	{
		msg_print("The item must be an enchanted crafted item.");
		return;
	}

	/* Determine which slot of the item is free. */
	for (i = 0; i < 20; i++)
	{
		if (o_ptr->spell[i].type == 0) break;
	}

	/* Shouldn't happen, but who knows? */
	if (i >= 20)
	{
		msg_print("All slots of this item have been taken.");
		return;
	}

	/* Do we have enough points to use an extra slot? */
	if (i > (p_ptr->abilities[(CLASS_ENCHANTER * 10) + 6] / 10))
	{
		msg_print("You cannot use an extra slot with your current ability level.");
		return;
	}

	/* Spell's cost check. */
	if (spell_ptr->finalcost > (p_ptr->abilities[(CLASS_ENCHANTER * 10) + 6] * 30))
	{
		msg_print("This spell is too costly for your ability level.");
		return;
	}

	/* Finally, let's start enchanting! */
	if (spell_ptr->effect[0] == 1)
	{
		strcpy(o_ptr->spell[i].name, spell_ptr->name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = spell_ptr->shape[0];
		o_ptr->spell[i].power = spell_ptr->power[0];
		o_ptr->spell[i].special1 = spell_ptr->type[0];
		o_ptr->spell[i].special2 = spell_ptr->radius[0];
		o_ptr->spell[i].special3 = 0;
		o_ptr->spell[i].summchar = '0';
		o_ptr->spell[i].cost = spell_ptr->finalcost / 4;
	}
	if (spell_ptr->effect[0] == 2)
	{
		strcpy(o_ptr->spell[i].name, spell_ptr->name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = spell_ptr->shape[0];
		o_ptr->spell[i].power = spell_ptr->power[0];
		o_ptr->spell[i].special1 = spell_ptr->type[0];
		o_ptr->spell[i].special2 = spell_ptr->radius[0];
		o_ptr->spell[i].special3 = 1;
		o_ptr->spell[i].summchar = '0';
		o_ptr->spell[i].cost = spell_ptr->finalcost / 4;
	}
	if (spell_ptr->effect[0] == 3)
	{
		strcpy(o_ptr->spell[i].name, spell_ptr->name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = 4;
		o_ptr->spell[i].power = spell_ptr->power[0];
		o_ptr->spell[i].special1 = 0;
		o_ptr->spell[i].special2 = 0;
		o_ptr->spell[i].special3 = 0;
		o_ptr->spell[i].summchar = '0';
		o_ptr->spell[i].cost = spell_ptr->finalcost / 4;
	}
	if (spell_ptr->effect[0] == 6)
	{
		strcpy(o_ptr->spell[i].name, spell_ptr->name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = 3;
		o_ptr->spell[i].power = spell_ptr->power[0];
		o_ptr->spell[i].special1 = 0;
		o_ptr->spell[i].special2 = 0;
		o_ptr->spell[i].special3 = 0;
		o_ptr->spell[i].summchar = '0';
		o_ptr->spell[i].cost = spell_ptr->finalcost / 4;
	}
	if (spell_ptr->effect[0] == 13)
	{
		strcpy(o_ptr->spell[i].name, spell_ptr->name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = 6;
		o_ptr->spell[i].power = spell_ptr->power[0];
		o_ptr->spell[i].special1 = spell_ptr->shape[0];
		o_ptr->spell[i].special2 = spell_ptr->type[0];
		o_ptr->spell[i].special3 = 0;
		o_ptr->spell[i].summchar = spell_ptr->schar1;
		o_ptr->spell[i].cost = spell_ptr->finalcost / 4;
	}
	if (spell_ptr->effect[0] == 14)
	{
		strcpy(o_ptr->spell[i].name, spell_ptr->name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = 7;
		o_ptr->spell[i].power = get_mon_num_name(spell_ptr->sspeci1);
		o_ptr->spell[i].special1 = spell_ptr->shape[0];
		o_ptr->spell[i].special2 = spell_ptr->type[0];
		o_ptr->spell[i].special3 = 0;
		o_ptr->spell[i].summchar = '0';
		o_ptr->spell[i].cost = spell_ptr->finalcost / 4;
	}

	if (!(f3 & (TR3_ACTIVATE))) o_ptr->art_flags3 |= (TR3_ACTIVATE);

	msg_print("Magical runes have been etched on your item!");

	return;
}

/* Essence Fusion ability. */
void essence_fusion()
{
	int item, item2, i;
	object_type *o_ptr;
	object_type *pot_ptr;
	object_kind *k_ptr;
	int istr, fstr;
	u32b f1, f2, f3, f4;

	cptr q, s;

	item_tester_tval = TV_POTION;

	/* Choose a potion. */
	q = "Use which potion? ";
	s = "You have no potions.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		pot_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		pot_ptr = &o_list[0 - item];
	}

	item_tester_tval = 0;
	item_tester_hook = item_tester_hook_efusion;

	/* Choose an item. */
	q = "Fuse to which item? ";
	s = "You have no items.";
	if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item2 >= 0)
	{
		o_ptr = &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item2];
	}

	k_ptr = &k_info[o_ptr->k_idx];

	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (!(f4 & (TR4_CRAFTED)))
	{
		msg_print("The item must be an enchanted crafted item.");
		return;
	}

	if (k_ptr->level > (p_ptr->abilities[(CLASS_ENCHANTER * 10) + 7] * 5))
	{
		msg_print("This item is too complicated for your current ability level.");
		return;
	}

	if (k_ptr->level > (pot_ptr->branddam / 5))
	{
		msg_print("You must a potion with a stronger base power.");
		return;
	}

	fstr = p_ptr->abilities[(CLASS_ENCHANTER * 10) + 7] * 20;
	istr = k_ptr->level * 5;

	if (randint(fstr) >= randint(istr))
	{
		msg_print("The fusion was successful!");
		o_ptr->extra1 = pot_ptr->brandtype;

		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}
	else
	{
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}

		do_cmd_save_game();
		msg_print("The fusion has failed.");
	}

	return;
}

/* Diviner's Wish ability! */
void diviner_wish()
{
        char name[80];
        int x;

	/* Only in towns. */
	if (dun_level != 0 || (p_ptr->inside_quest))
	{
		msg_print("You cannot use this ability here.");
		return;
	}

        /* Make an empty string */
        name[0] = 0;

	/* Reset the previous wish. */
	p_ptr->events[29013] = 0;

        /* Ask for the wish */
        if(!get_string("Wish for what item? ", name, 80)) return;

	/* You WANT a twisted boss, hmmm? */
	if (strstr("Twisted Boss", name))
	{
		msg_print("You make the wish to fight a powerful enemy.");
		p_ptr->events[29013] = -1;
		return;
	}

        /* Try all objects, make sure it exists. */
        for (x = 1; x <= max_k_idx; x++)
        {
                object_kind *k_ptr = &k_info[x];
                if (strstr((k_name + k_ptr->name), name) && !(k_ptr->flags4 & (TR4_SPECIAL_GENE)) && !(k_ptr->flags3 & (TR3_INSTA_ART)))
                {
                        p_ptr->events[29013] = x;
			msg_print("You wish for the item. Now you must reach the end of a random dungeon.");
                        return;
                }
        }

	/* No items was found. */
	msg_print("You cannot wish for this item.");
}

/* Select the Monster race default melee attack */
void select_monster_melee()
{
        monster_race    *r_ptr;
	int             i;
	int		num = 0;
	int             ac,pt;
	int		powers[36];

	char		powdesc[160];
	char            power_desc[36][160];
	bool            blinked = FALSE, touched = FALSE;

	int Power = -1;
	bool            flag, redraw;
        int             ask;
	char            choice;
	char            out_val[160];

        r_ptr = &r_info[p_ptr->body_monster];
		
	/* List the powers */
	i = 0;
	while (i < 20 && r_ptr->attack[i].type > 0) 
	{
		if (r_ptr->attack[i].type == 1 || r_ptr->attack[i].type == 3)
		{
			sprintf(powdesc, "%s", get_monster_attack_name_no_type(p_ptr->body_monster, i));
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		i++;
	}

        if(!num) {msg_print("No attacks available.");return;}

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

	p_ptr->events[29019] = i;
	msg_print("Attack selected.");
}

char *get_monster_attack_name_no_type(int m_idx, int num)
{
	char attackname[160];
	char aname[80];
	char extra[80];
	monster_race *r_ptr = &r_info[m_idx];

	/*plog(r_ptr->attack[num].name[0]);*/

	/* Generate the name. */
	/* If it's just "!", let's make a NICE name! ;) */
	if (r_ptr->attack[num].name[0] != '!') sprintf(aname, "%s", r_ptr->attack[num].name);
	else
	{
		char aact[80];
		sprintf(aact, "%s", r_ptr->attack[num].act);
		aact[0] = toupper(aact[0]);
		sprintf(aname, "%s %s", get_element_name(r_ptr->attack[num].element), aact);
	}

	if (r_ptr->attack[num].type == 1 || r_ptr->attack[num].type == 3) sprintf(extra, "(%s, base dam: %dd%d)", get_element_name(r_ptr->attack[num].element), r_ptr->attack[num].ddice, r_ptr->attack[num].dside);
	else sprintf(extra, "(Special attack)");

	sprintf(attackname, "%s %s", aname, extra);

        return(attackname);
}

/* Select the Monster race default ranged attack */
void select_monster_ranged()
{
        monster_race    *r_ptr;
	int             i;
	int		num = 0;
	int             ac,pt;
	int		powers[36];

	char		powdesc[160];
	char            power_desc[36][160];
	bool            blinked = FALSE, touched = FALSE;

	int Power = -1;
	bool            flag, redraw;
        int             ask;
	char            choice;
	char            out_val[160];

        r_ptr = &r_info[p_ptr->body_monster];
		
	/* List the powers */
	i = 0;
	while (i < 20 && r_ptr->attack[i].type > 0) 
	{
		if (r_ptr->attack[i].type == 3)
		{
			sprintf(powdesc, "%s", get_monster_attack_name_no_type(p_ptr->body_monster, i));
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		i++;
	}

        if(!num) {msg_print("No attacks available.");return;}

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

	p_ptr->events[29031] = Power;
	msg_print("Attack selected.");
}
