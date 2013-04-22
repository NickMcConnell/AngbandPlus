
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

/* Try to bash a fountain */
static bool do_cmd_bash_fountain(int y, int x)
{
	int bash, temp;
	cave_type *c_ptr;
	bool more = TRUE;
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
	msg_print("You smash into the fountain!");

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - 50);

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(200) < temp)
	{
		/* Message */
		msg_print("The fountain breaks!");
		
		fire_ball(GF_WATER, 5, damroll(6, 8), 2);
		
		cave_set_feat(y, x, FEAT_DEEP_WATER);
		more = FALSE;
	}
	
	return(more);
}


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
        bool go_up = FALSE, go_up_many = FALSE, prob_traveling = FALSE;
        cave_type *c_ptr;
        char i;
        int oldl = dun_level;
        dungeon_info_type *d_ptr = &d_info[dungeon_type];
  
        /* Player grid */
        c_ptr = &cave[py][px];
  
        /* test if on special level */
        if ((dungeon_flags1 & LF1_ASK_LEAVE))
        {
                prt("Leave this unique level forever (y/n) ? ",0,0);
                flush();
                i=inkey();
                prt("",0,0);
                if (i != 'y') return;
        }

        /* Can we ? */
        if (process_hooks(HOOK_STAIR, FALSE)) return;
  
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
        /* Shaft (The brown private FEAT that's UP machine for all the @'s) */
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
        else if (p_ptr->prob_travel && !p_ptr->inside_quest)
        {
                if (d_ptr->mindepth == dun_level) return;

                prob_traveling = TRUE;

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
  
                if (p_ptr->inside_quest)
                {
                        dun_level = 1;
                        leaving_quest = p_ptr->inside_quest;
  
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
  
                if (c_ptr->special && (!prob_traveling))
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
                msg_print("Brrrr! It's deadly cold.");

                if (p_ptr->prace != RACE_DRAGONRIDDER)
                {
                        int reduc = ((p_ptr->ac + p_ptr->to_a) / 50) + 1;

                        take_hit(distance(by,bx,py,px) / (10 * reduc), "going Between");
                }

                swap_position(by, bx);

                /* To avoid being teleported back */
                energy_use = 0;

                return TRUE;
        }
        else if (cave[py][px].feat == FEAT_BETWEEN2)
        {
                between_exit *be_ptr = &between_exits[cave[py][px].special];

                p_ptr->wild_mode = FALSE;
                p_ptr->wilderness_x = be_ptr->wild_x;
                p_ptr->wilderness_y = be_ptr->wild_y;
                p_ptr->oldpx = px = be_ptr->px;
                p_ptr->oldpy = py = be_ptr->py;
                dungeon_type = be_ptr->d_idx;
                dun_level = be_ptr->level;
                p_ptr->leaving = TRUE;

                return (TRUE);
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
    bool go_down = FALSE, go_down_many = FALSE, prob_traveling = FALSE;
    bool fall_trap = FALSE;
    char i;
    int old_dun = dun_level;
    dungeon_info_type *d_ptr = &d_info[dungeon_type];
    /* Between Gates MUST be actived now */
    if (between_effect()) return;
  
    /* Player grid */
    c_ptr = &cave[py][px];

    if (p_ptr->astral && (dun_level == 98)) return;
  
    if (c_ptr->t_idx == TRAP_OF_SINKING) fall_trap = TRUE;
  
    /* test if on special level */
    if ((dungeon_flags1 & LF1_ASK_LEAVE))
    {
        prt("Leave this unique level forever (y/n) ? ",0,0);
        flush();
        i=inkey();
        prt("",0,0);
        if (i != 'y') return;
    }

    /* Can we ? */
    if (process_hooks(HOOK_STAIR, TRUE)) return;
  
    /* Normal up stairs */
    if (c_ptr->feat == FEAT_SHAFT_DOWN)
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
        if (p_ptr->prob_travel)
        {
	    if (d_ptr->maxdepth == dun_level) return;
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

    else if (p_ptr->prob_travel && !p_ptr->inside_quest)
    {
        if (d_ptr->maxdepth == dun_level) return;

        prob_traveling = TRUE;

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
  
    /* Go down */
    if (go_down)
    {
        dun_level++;
    }
    else if (go_down_many)
         {
            int i = randint(3) + 1, j;

            for(j = 1; j < i; j++)
            {
                dun_level++;
                if (is_quest(dun_level + i - 1)) break;
                if (d_ptr->maxdepth == dun_level) break;
            }
    }

    /* We change place */
    if (c_ptr->special && (!prob_traveling))
    {                       	
        if(d_info[c_ptr->special].min_plev <= p_ptr->lev)
        {
            dungeon_info_type *d_ptr = &d_info[c_ptr->special];

            /* Dump the price information */
	    dump_price(d_info[dungeon_type].subdir);
	    
	    /* Ok go in the new dungeon */
            dungeon_type = c_ptr->special;
	        
	    /*Initialize the dungeon specific *_info.txt files*/ 
	    init_dun_entry(d_ptr->subdir);

	    /*Finally we have a use for oops ;-)*/
	    if(d_ptr->flags1 & DF1_SILLY)
	        p_ptr->oops = TRUE;
	    	        
	    if ((p_ptr->wilderness_x == d_ptr->ix) && (p_ptr->wilderness_y == d_ptr->iy))
            {
                dun_level = d_ptr->mindepth;
            }
            else if ((p_ptr->wilderness_x == d_ptr->ox) && (p_ptr->wilderness_y == d_ptr->oy))
            {
                dun_level = d_ptr->maxdepth;
            }
            else
            {
                dun_level = d_ptr->mindepth;
            }

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
                        if (!make_object(q_ptr, FALSE, FALSE, d_info[dungeon_type].objs)) continue;
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
	o_ptr->pval2 = 0;

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
	if(!rl_mess)
	  msg_print("You found a trap!");
	else
	  msg_print("trap!");
	/* Set off trap */
	ident = player_activate_trap_type(y, x, o_ptr, o_idx);
	if (ident)
	{
		t_info[o_ptr->pval].ident = TRUE;
		if(!roguelike_messages)
		  msg_format("You identified the trap as %s.",
			   t_name + t_info[trap].name);
		else
		  msg_format("%s", t_name + t_info[trap].name);
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

        monster_race *r_ptr = &r_info[p_ptr->body_monster];
   
        if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_OPEN_DOOR))
        {
                msg_print("You cannot open chests.");
         
                return(FALSE);
        }

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

        monster_race *r_ptr = &r_info[p_ptr->body_monster];
   
        if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_OPEN_DOOR))
        {
                msg_print("You cannot open doors.");
         
                return(FALSE);
        }

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

#if 0  /*JKB: Not any more... (I'm evil, aren't I?)*/
		/* Always have a small chance of success */
		if (j < 2) j = 2;
#endif

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
			/* JKB: Well, maybe a bit less evil */
			gain_exp(100 - j);
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
 * Unlocking a locked door/chest is worth greater experience for 
 * less skill (You know, all that "learning from our mistakes" dr*ck)
 */
void do_cmd_open(void)
{
	int y, x, dir;
	
	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

        monster_race *r_ptr = &r_info[p_ptr->body_monster];
   
        if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_OPEN_DOOR))
        {
                msg_print("You cannot open doors.");
         
                return;
        }

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

        /* Process the appropriate hooks */
        process_hooks(HOOK_OPEN, is_quest(dun_level));

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

        /* Must be tunnelable */
        if (!(f_info[cave[y][x].feat].flags1 & FF1_TUNNELABLE))
	{
		/* Message */
                msg_print(f_text + f_info[cave[y][x].feat].tunnel);

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
        cave_type *c_ptr = &cave[y][x];
        feature_type *f_ptr = &f_info[c_ptr->feat];

	bool more = FALSE;

        /* Must be have something to dig with (except for sandwalls) */
	if ((c_ptr->feat < FEAT_SANDWALL) || (c_ptr->feat > FEAT_SANDWALL_K))
		if(!inventory[INVEN_TOOL].k_idx || (inventory[INVEN_TOOL].tval != TV_DIGGING))
	{
                msg_print("You need to have a shovel or pick in your tool slot.");

                return FALSE;
        }

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Take a turn */
	energy_use = 100;

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Sound */
	sound(SOUND_DIG);

	/* Titanium */
        if (f_ptr->flags1 & FF1_PERMANENT)
	{
                msg_print(f_text + f_ptr->tunnel);
	}

        else if ((c_ptr->feat == FEAT_TREES) || (c_ptr->feat == FEAT_DEAD_TREE))
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
                        msg_print(f_text + f_ptr->tunnel);
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
                        msg_print(f_text + f_ptr->tunnel);
			more = TRUE;
		}
	}


	/* Quartz / Magma / Sandwall */
	else if (((c_ptr->feat >= FEAT_MAGMA) &&
		  (c_ptr->feat <= FEAT_QUARTZ_K)) ||
		 ((c_ptr->feat >= FEAT_SANDWALL) &&
		  (c_ptr->feat <= FEAT_SANDWALL_K)))
	{
		bool okay = FALSE;
		bool gold = FALSE;
		bool hard = FALSE;
		bool soft = FALSE;

		/* Found gold */
		if ((c_ptr->feat >= FEAT_MAGMA_H) &&
		    (c_ptr->feat <= FEAT_QUARTZ_K)) gold = TRUE;
		
		if ((c_ptr->feat == FEAT_SANDWALL_H) ||
                    (c_ptr->feat == FEAT_SANDWALL_K))
                {
			gold = TRUE;
			soft = TRUE;
		}
		else
		/* Extract "quartz" flag XXX XXX XXX */
		if ((c_ptr->feat - FEAT_MAGMA) & 0x01) hard = TRUE;

		/* Quartz */
		if (hard)
		{
			okay = (p_ptr->skill_dig > 20 + rand_int(800));
		}

		/* Sandwall */
		else if (soft)
		{
			okay = (p_ptr->skill_dig > 5 + rand_int(250));
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

                /* Failure */
		else
		{
			/* Message, continue digging */
                        msg_print(f_text + f_ptr->tunnel);
			more = TRUE;
		}
	}

	/* Rubble */
	else if (c_ptr->feat == FEAT_RUBBLE)
	{
		/* Remove the rubble */
                if ((p_ptr->skill_dig > rand_int(200)) && twall(y, x, d_info[dungeon_type].floor1))
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
                        msg_print(f_text + f_ptr->tunnel);
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

			/* Set off trap */
			if (c_ptr->t_idx != 0) player_activate_door_trap(y, x);
		}

		/* Keep trying */
		else
		{
                        int feat;

                        if (c_ptr->mimic) feat = c_ptr->mimic;
                        else feat = c_ptr->feat;

			/* We may continue tunelling */
                        msg_print(f_text + f_info[feat].tunnel);
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
                        msg_print(f_text + f_ptr->tunnel);
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

        if (p_ptr->wild_mode) return;

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
                    (c_ptr->feat == FEAT_SHOP))
		{
			/* Message */
			msg_print("You cannot tunnel through doors.");
		}

		/* No tunnelling through air */
                else if (cave_floor_grid(c_ptr))
		{
			/* Message */
			msg_print("You cannot tunnel through air.");
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

        monster_race *r_ptr = &r_info[p_ptr->body_monster];
   
        if ((p_ptr->body_monster != 0) && !(r_ptr->flags2 & RF2_OPEN_DOOR))
        {
                msg_print("You cannot open doors.");
         
                return(FALSE);
        }

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
	i = p_ptr->skill_dis;

	/* Penalize some conditions */
	if (p_ptr->blind || no_lite()) i = i / 10;
	if (p_ptr->confused || p_ptr->image) i = i / 10;

	/* Extract the difficulty */
	j = i - t_ptr->difficulty * 3;

#if 0
	/* Always have a small chance of success */
	if (j < 2) j = 2;
#endif
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
	i = p_ptr->skill_dis;

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
		if(!rl_mess)
		  msg_format("You have disarmed the %s.", name);
		else
		  msg_format("disarmed %s", name);

		/* Reward */
		gain_exp(power);

		/* Forget the trap */
		c_ptr->info &= ~(CAVE_MARK);

		/* Remove the trap */
		c_ptr->t_idx = 0;

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
		if(!rl_mess)
		  msg_format("You failed to disarm the %s.", name);

		/* We may keep trying */
		more = TRUE;
	}

	/* Failure -- Set off the trap */
	else
	{
		/* Message */
		if(!rl_mess)
		  msg_format("You set off the %s!", name);
		else
		  msg_format("set off %s", name);
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
 * Disarms the monster traps(no failure)
 */
void do_cmd_disarm_mon_trap(int y, int x)
{
        msg_print("You disarm the monster trap.");

        cave_set_feat(y, x, FEAT_FLOOR);
        cave[py][px].special = cave[py][px].special2 = 0;
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
                if (((c_ptr->t_idx == 0) || (!(c_ptr->info & CAVE_TRDT))) && 
                    !o_idx && (c_ptr->feat != FEAT_MON_TRAP))
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
                        if (c_ptr->feat == FEAT_MON_TRAP)
                        {
                                do_cmd_disarm_mon_trap(y, x);
                                more = FALSE;
                        }
                        else
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

	/* Hack -- weaklings really shouldn't mess with doors */
	if (temp < 1) msg_print("This is getting nowhere");

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

	/* Ah, well, we must all have a low dexterity sometimes */
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
                       c_ptr->feat > FEAT_ALTAR_TAIL) &&
		     (c_ptr->feat != FEAT_FOUNTAIN))
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
		else if (c_ptr->feat == FEAT_FOUNTAIN)
		{
			more = do_cmd_bash_fountain(y, x);
		}
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
                else if (f_info[c_ptr->feat].flags1 & FF1_TUNNELABLE)
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x, dir);
		}
#if 0
		/* Bash jammed doors */
                else if ((c_ptr->feat >= FEAT_DOOR_HEAD + 0x08) &&
                    (c_ptr->feat <= FEAT_DOOR_TAIL))
		{
			/* Tunnel */
			more = do_cmd_bash_aux(y, x, dir);
		}
#endif
		/* Open closed doors */
		else if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
                    (c_ptr->feat <= FEAT_DOOR_TAIL))
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
			msg_print("You clap your hands.  You stomp your feet.  You jump in the air");
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
	    if(dir % 2)
		{
		/* Moving diagonally takes longer */
		energy_use = 100;
		if(magik(41))
		  dir = 5;
		}
	    else
		{	
		/* Take a turn */
		energy_use = 100;
		}
	    /* Actually move the character */
	    move_player(dir, pickup);
                
	    /* Allow more walking */
	    more = TRUE;
	}

        /* Hack -- In small scale wilderness it takes MUCH more time to move */
        energy_use *= (p_ptr->wild_mode)?((MAX_HGT + MAX_WID) / 2):1;

        /* Hack again -- Is there a special encounter ??? */
        if(p_ptr->wild_mode && magik(wf_info[wild_map[py][px].feat].level - (p_ptr->lev * 2)))
        {
                /* Go into large wilderness view */
                p_ptr->wilderness_x = px;
                p_ptr->wilderness_y = py;
                energy_use = 100;
                change_wild_mode();

                /* HACk -- set the encouter flag for the wilderness generation */
                generate_encounter = TRUE;
                p_ptr->oldpx = MAX_WID / 2;
                p_ptr->oldpy = MAX_HGT / 2;

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
        if (c_ptr->feat == FEAT_SHOP)
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
        /* Can't rest on a Between Gate -- too dangerous */
        if (cave[py][px].feat == FEAT_BETWEEN)
        {
                msg_print("Resting on a between gate is too dangerous!");
                return;
        }

        /* Can't rest while undead, it would mean dying */
        if (p_ptr->class_extra3 & CLASS_UNDEAD)
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
int breakage_chance(object_type *o_ptr)
{
        int reducer = 1 + ((p_ptr->pclass == CLASS_ARCHER)?(p_ptr->lev / 10):0);

	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break (Huh?  Food can break?)*/
		case TV_FLASK:
		case TV_POTION:
                case TV_POTION2:
		case TV_BOTTLE:
		case TV_FOOD:
                case TV_FIRESCONE:
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
void do_cmd_fire(void)
{
	int dir, item;
        int j, y, x, ny, nx, ty, tx, by, bx;
	int tdam, tdis, thits, tmul;
	int bonus, chance;
	int cur_dis, visible;
        int breakage = -1, num_ricochet = 0;
        s32b special = 0;

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
            (inventory[INVEN_WIELD].tval == p_ptr->class_extra1))
                bonus = ((p_ptr->to_h - (p_ptr->lev / 2)) + q_ptr->to_h + j_ptr->to_h);
	else
		bonus = (p_ptr->to_h + q_ptr->to_h + j_ptr->to_h);

        chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ)) - p_ptr->lev;
        if(chance < 5) chance = 5;

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
        tmul += p_ptr->xtra_might;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range */
	tdis = 10 + 5 * tmul;


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
                                /* Pause anyway, for consistency */
                                Term_xtra(TERM_XTRA_DELAY, msec);
                        }


                        /* Monster here, Try to hit it */
                        if (cave[y][x].m_idx)
                        {
                                cave_type *c_ptr = &cave[y][x];

                                monster_type *m_ptr = &m_list[c_ptr->m_idx];
                                monster_race *r_ptr = race_inf(m_ptr);

                                /* Check the visibility */
                                visible = m_ptr->ml;

                                /* Note the collision */
                                hit_body = TRUE;

                                /* Did we hit it (penalize range) */
                                if (test_hit_fire(chance - cur_dis, m_ptr->ac, m_ptr->ml))
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
                                                if(!rl_mess)
						  msg_format("The %s finds a mark.", o_name);
                                        	else
						  msg_format("it hits");
					}

                                        /* Handle visible monster */
                                        else
                                        {
                                                char m_name[80];

                                                /* Get "the monster" or "it" */
                                                monster_desc(m_name, m_ptr, 0);

                                                /* Message */
                                                if(!rl_mess)
						  msg_format("The %s hits %s.", o_name, m_name);
						else
						  msg_format("it hits");

                                                /* Hack -- Track this monster race */
                                                if (m_ptr->ml) monster_race_track(m_ptr->r_idx, m_ptr->ego);

                                                /* Hack -- Track this monster */
                                                if (m_ptr->ml) health_track(c_ptr->m_idx);

                                                /* Anger friends */
                                                {
                                                        char m_name[80];
                                                        monster_desc(m_name, m_ptr, 0);
                                                        switch (is_friend(m_ptr))
                                                        {
                                                                case 1:
                                                                        msg_format("%^s gets angry!", m_name);
                                                                        change_side(m_ptr);
                                                                        break;
                                                                case 0:
                                                                        msg_format("%^s gets angry!", m_name);
                                                                        m_ptr->status = MSTATUS_NEUTRAL_M;
                                                                        break;
                                                        }
                                                }
                                        }

                                        /* Apply special damage XXX XXX XXX */
                                        tdam = tot_dam_aux(q_ptr, tdam, m_ptr, &special);
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

                                                if (special) attack_special(m_ptr, special, tdam);

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

                /* Exploding arrow ? */
                if (q_ptr->pval2 != 0)
                {
                        int rad = 0, dam = (damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d) * 2;
                        int flag = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP;
                        switch(q_ptr->sval)
                        {
                                case SV_AMMO_LIGHT: rad = 2; dam /= 2; break;
                                case SV_AMMO_NORMAL: rad = 3; break;
                                case SV_AMMO_HEAVY: rad = 4; dam *= 2; break;
                        }
		   
                        project(0, rad, y, x, dam, q_ptr->pval2, flag);
		}
		
	        /* Chance of breakage (during attacks) */
                j = (hit_body ? breakage_chance(q_ptr) : 0);

                /* Break ? */
                if((q_ptr->pval2 != 0)||(rand_int(100) < j))
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

                        if(!rl_mess)
			  msg_format("The %s ricochets!", o_name);
                	else
			  msg_format("it ricochets");
		}
                else
                        break;
        }

	/* Drop (or break) near that location */
        drop_near(q_ptr, breakage, y, x);
}


int throw_mult = 1;

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
        s32b special = 0;
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

	/*
	 * Hack -- If rods or wands are thrown, the total maximum timeout or
	 * charges need to be allocated between the two stacks.
	 */
        if ((o_ptr->tval == TV_WAND))
	{
		q_ptr->pval = o_ptr->pval / o_ptr->number;

		if (o_ptr->number > 1) o_ptr->pval -= q_ptr->pval;
	}

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
        /* Changed for 'launcher' corruption */
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

	/* Chance of hitting - adjusted for Weaponmasters -- Gumby */
	if ((p_ptr->pclass == CLASS_WEAPONMASTER) &&
            (inventory[INVEN_WIELD].tval == p_ptr->class_extra1))
                chance = (p_ptr->skill_tht + ((p_ptr->to_h - (p_ptr->lev / 2)) * BTH_PLUS_ADJ));
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
                        monster_race *r_ptr = race_inf(m_ptr);

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
                        if (test_hit_fire(chance - cur_dis, m_ptr->ac, m_ptr->ml))
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
					if(!rl_mess)
					  msg_format("The %s finds a mark.", o_name);
					else
					  msg_format("it hits");
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					if(!rl_mess)
					  msg_format("The %s hits %s.", o_name, m_name);
					else
					  msg_format("it hits");
					
					/* Hack -- Track this monster race */
                                        if (m_ptr->ml) monster_race_track(m_ptr->r_idx, m_ptr->ego);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* Apply special damage XXX XXX XXX */
                                tdam = tot_dam_aux(q_ptr, tdam, m_ptr, &special);
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

                                        if (special) attack_special(m_ptr, special, tdam);

					/* Anger friends */
                                        if (!(k_info[q_ptr->k_idx].tval == TV_POTION))
					{
						char m_name[80];
						monster_desc(m_name, m_ptr, 0);
                                                switch (is_friend(m_ptr))
                                                {
                                                        case 1:
                                                                msg_format("%^s gets angry!", m_name);
                                                                change_side(m_ptr);
                                                                break;
                                                        case 0:
                                                                msg_format("%^s gets angry!", m_name);
                                                                m_ptr->status = MSTATUS_NEUTRAL_M;
                                                                break;
                                                }
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
			if(!rl_mess)
			  msg_format("The %s shatters!", o_name);
			else
			  msg_format("it shatters");
			
                        if (potion_smash_effect(0, y, x, q_ptr->sval))
			{
                                if (cave[y][x].m_idx)
				{
					char m_name[80];
					monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
                                        switch (is_friend(&m_list[cave[y][x].m_idx]))
                                        {
                                                case 1:
                                                        msg_format("%^s gets angry!", m_name);
                                                        change_side(&m_list[cave[y][x].m_idx]);
                                                        break;
                                                case 0:
                                                        msg_format("%^s gets angry!", m_name);
                                                        m_list[cave[y][x].m_idx].status = MSTATUS_NEUTRAL_M;
                                                        break;
                                        }
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
        s32b special = 0;

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
        /* Changed for 'launcher' corruption */
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
                        monster_race *r_ptr = race_inf(m_ptr);

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			hit_body = TRUE;

			/* Did we hit it (penalize range) */
                        if (test_hit_fire(chance - cur_dis, m_ptr->ac, m_ptr->ml))
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
					if(!rl_mess)
					  msg_format("The %s finds a mark.", o_name);
					else
					  msg_format("it hits");
				}

				/* Handle visible monster */
				else
				{
					char m_name[80];

					/* Get "the monster" or "it" */
					monster_desc(m_name, m_ptr, 0);

					/* Message */
					if(!rl_mess)
					  msg_format("The %s hits %s.", o_name, m_name);
					else
					  msg_format("it hits");
					/* Hack -- Track this monster race */
                                        if (m_ptr->ml) monster_race_track(m_ptr->r_idx, m_ptr->ego);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* Apply special damage XXX XXX XXX */
                                tdam = tot_dam_aux(q_ptr, tdam, m_ptr, &special);
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

                                        if (special) attack_special(m_ptr, special, tdam);

					/* Anger friends */
                                        if (!(k_info[q_ptr->k_idx].tval == TV_POTION))
					{
						char m_name[80];
						monster_desc(m_name, m_ptr, 0);
                                                switch (is_friend(m_ptr))
                                                {
                                                        case 1:
                                                                msg_format("%^s gets angry!", m_name);
                                                                change_side(m_ptr);
                                                                break;
                                                        case 0:
                                                                msg_format("%^s gets angry!", m_name);
                                                                m_ptr->status = MSTATUS_NEUTRAL_M;
                                                                break;
                                                }
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
						if(!rl_mess)
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

static bool tport_vertically(bool how) {
  if ((p_ptr->inside_arena)||(p_ptr->inside_quest)) { /* arena or quest -KMW- */
    msg_print("There is no effect.");
    return FALSE;
  }

  /* Go down */

  if (how) {
    if (dun_level >= d_info[dungeon_type].maxdepth) {
      msg_print("The floor is impermeable.  Your body squishes down.");
      p_ptr->slow += 5;
      return FALSE;
    }

    msg_print("You sink through the floor.");
    dun_level++;
    p_ptr->leaving = TRUE;
  } else {
    if (!dun_level) {
      msg_print("You rise up into the air a few feet, then fall to the ground.");
      if(!p_ptr->ffall)
	take_hit(damroll(3, 2), "falling");
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
    set_grace(5000 + val);
    p_ptr->god_favor = -60000;

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
    set_grace(p_ptr->grace + val * 3);
  }

        /* Eliminate the item (from the pack) */
        if (item >= 0)
        {
                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
        }
}

/*
 * Set of variables and functions to create an artifact
 */
typedef struct flag flag;
struct flag {
        u32b flag;
        int level;
        cptr desc;
        u32b xp;
        bool pval;
};

flag flags_level[4][32]=
{
{
        {TR1_STR,40,"Strength", 5000, TRUE},
        {TR1_INT,43,"Intelligence", 5000, TRUE},
        {TR1_WIS,38,"Wisdom", 5000, TRUE},
        {TR1_DEX,46,"Dexterity", 5000, TRUE},
        {TR1_CON,42,"Constitution", 5000, TRUE},
        {TR1_CHR,30,"Charisma", 5000, TRUE},
        {TR1_STEALTH,32,"Stealth", 5000, TRUE},
        {TR1_SEARCH,29,"Search", 2000, TRUE},
        {TR1_INFRA,6,"Infravision", 1000, TRUE},
        {TR1_SPEED,49,"Speed", 50000, TRUE},
        {TR1_BLOWS,38,"Blows", 150000, TRUE},
        {TR1_VAMPIRIC,26,"Vampiric", 60000, FALSE},
        {TR1_SLAY_ANIMAL,16,"Slay Annimal", 1000, FALSE},
        {TR1_SLAY_EVIL,25,"Slay Evil", 2000, FALSE},
        {TR1_SLAY_UNDEAD,30,"Slay Undead", 2000, FALSE},
        {TR1_SLAY_DEMON,40,"Slay Demon", 1500, FALSE},
        {TR1_SLAY_ORC,10,"Slay Orc", 700, FALSE},
        {TR1_SLAY_TROLL,16,"Slay Troll", 700, FALSE},
        {TR1_SLAY_GIANT,25,"Slay Giant", 900, FALSE},
        {TR1_SLAY_DRAGON,33,"Slay Dragon", 2000, FALSE},
        {TR1_KILL_DRAGON,41,"*Slay* Dragon", 5000, FALSE},
        {TR1_VORPAL,36,"Vorpal", 20000, FALSE},
        {TR1_BRAND_POIS,3,"Brand Poison", 2000, FALSE},
        {TR1_BRAND_ACID,12,"Brand Acid", 2000, FALSE},
        {TR1_BRAND_ELEC,10,"Brand Lightning", 2000, FALSE},
        {TR1_BRAND_FIRE,6,"Brand Fire", 2000, FALSE},
        {TR1_BRAND_COLD,8,"Brand Cold", 2000, FALSE},
        {0,0,NULL, 0, FALSE},
},
{
        {TR2_SUST_STR,32,"Sustain Strength", 1000, FALSE},
        {TR2_SUST_INT,34,"Sustain Intelligence", 1000, FALSE},
        {TR2_SUST_WIS,28,"Sustain Wisdom", 1000, FALSE},
        {TR2_SUST_DEX,36,"Sustain Dexterity", 1000, FALSE},
        {TR2_SUST_CON,36,"Sustain Constitution", 1000, FALSE},
        {TR2_SUST_CHR,25,"Sustain Charisma", 1000, FALSE},
        {TR2_INVIS,20,"Invisibility", 15000, TRUE},
        {TR2_IM_ACID,49,"Immune Acid", 500000, FALSE},
        {TR2_IM_ELEC,50,"Immune Ligthning", 500000, FALSE},
        {TR2_IM_FIRE,49,"Immune Fire", 500000, FALSE},
        {TR2_IM_COLD,50,"Immune Cold", 500000, FALSE},
        {TR2_REFLECT,38,"Reflection", 90000, FALSE},
        {TR2_FREE_ACT,20,"Free Action", 30000, FALSE},
        {TR2_HOLD_LIFE,30,"Hold Life", 30000, FALSE},
        {TR2_RES_ACID,12,"Resist Acid", 10000, FALSE},
        {TR2_RES_ELEC,15,"Resist Lightning", 10000, FALSE},
        {TR2_RES_FIRE,13,"Resist Fire", 10000, FALSE},
        {TR2_RES_COLD,14,"Resist Cold", 10000, FALSE},
        {TR2_RES_POIS,25,"Resist Poison", 30000, FALSE},
        {TR2_RES_FEAR,26,"Resist Fear", 10000, FALSE},
        {TR2_RES_LITE,31,"Resist Lite", 60000, FALSE},
        {TR2_RES_DARK,33,"Resist Darkness", 60000, FALSE},
        {TR2_RES_BLIND,30,"Resist Blindness", 30000, FALSE},
        {TR2_RES_CONF,36,"Resist Confusion", 30000, FALSE},
        {TR2_RES_SOUND,38,"Resist Sound", 60000, FALSE},
        {TR2_RES_SHARDS,42,"Resist Shards", 60000, FALSE},
        {TR2_RES_NETHER,39,"Resist Nether", 60000, FALSE},
        {TR2_RES_NEXUS,46,"Resist Nexus", 60000, FALSE},
        {TR2_RES_CHAOS,39,"Resist Chaos", 60000, FALSE},
        {TR2_RES_DISEN,47,"Resist Disenchantment", 60000, FALSE},
        {0,0,NULL, 0},
},
{
        {TR3_SH_FIRE,20,"Aura Fire", 30000, FALSE},
        {TR3_SH_ELEC,25,"Aura Ligthning", 30000, FALSE},
        {TR3_NO_TELE,29,"Anti Teleportaton", 2000, FALSE},
        {TR3_NO_MAGIC,34,"Anti Magic", 2000, FALSE},
        {TR3_WRAITH,50,"Wraith Form", 100000, FALSE},
        {TR3_FEATHER,15,"Levitation", 1000, FALSE},
        {TR3_LITE1,8,"Lite", 1000, FALSE},
        {TR3_SEE_INVIS,20,"See Invisible", 4000, FALSE},
        {TR3_REGEN,32,"Regeneration", 20000, FALSE},
        {TR3_TELEPORT,12,"Teleport", 20000, FALSE},
        {0,0,NULL, 0, FALSE},
},
{
        {TR4_PRECOGNITION,50,"Precognition", 20000000, FALSE},
        {TR4_FLY,40,"Fly", 200000, FALSE},
        {0,0,NULL, 0, FALSE},
},
};

bool flags_select[4][32];

int show_flags(byte flag, s32b *exp, int pval)
{
        int i, x, color = TERM_WHITE;

        char ttt[80];

	Term_clear();

        i = 0;
        while (i < 32)
        {
                if (i < 16) x = 5;
                else x = 45;

                if (flags_level[flag][i].xp == 0)
                {
                        break;
                }
                else
                {
                        sprintf(ttt, "%c) %s(exp %ld)", (i < 26)?I2A(i):('0' + i - 26), flags_level[flag][i].desc, flags_level[flag][i].xp);
                        if (flags_level[flag][i].level > p_ptr->lev) color = TERM_L_DARK;
                        else if (flags_select[flag][i])
                        {
                                int j = pval;
                                u32b xp = flags_level[flag][i].xp;

                                color = TERM_L_GREEN;
                                if (flags_level[flag][i].pval)
                                {
                                        while (j)
                                        {
                                                *exp += xp;
                                                xp *= 2;
                                                j--;
                                        }
                                }
                                else
                                {
                                        *exp += xp;
                                }
                        }
                        else color = TERM_WHITE;
                }
                c_prt(color, ttt, ((i < 16)?i:i - 16) + 5, x);

                i++;
        }
        return i;
}


/*
 * Check if an object is artifactized
 */
bool item_tester_hook_artifactized(object_type *o_ptr)
{
        u32b f1, f2, f3, f4, f5, esp;

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        return ((f4 & TR4_ART_EXP) && (o_ptr->exp >= 1000));
}

void do_cmd_create_artifact()
{
        int max, i = 0, pval = 0, j, cur_set = 0, abord = FALSE, done = FALSE;
        s32b exp = 0;

        char out_val[160];

        bool okay = FALSE;

        char choice = 0;

        cptr q, s;

        int item;

        object_type forge;
        object_type *q_ptr = &forge;
        
        char o_name[80];

        energy_use = 100;

        /* Restrict choices to artifactable items */
        item_tester_hook = item_tester_hook_artifactized;

        /* Get an item */
        q = "Finalize which item? ";
        s = "You have nothing to finalize.";
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

        if (artifact_p(q_ptr))
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

        if (!okay) return;

        pval = 1;

        /* Save the screen */
        character_icky = TRUE;
        Term_save();

        for(j = 0; j < 4; j++)
        for(i = 0; i < 32; i++)
        {
                flags_select[j][i] = FALSE;
        }

        /* Everlasting love ... ... nevermind :) (JKB: ???)*/
        while (!done && !abord)
        {
                /* Chose the flags */
                exp = 0;
                max = show_flags(cur_set, &exp, pval);
                c_prt((q_ptr->exp - exp > 0)?TERM_L_GREEN:TERM_L_RED, format("Experience left: %ld", q_ptr->exp - exp), 2, 0);

                /* Build a prompt (accept all flags) */
                if (max <= 26)
                {
                        /* Build a prompt (accept all flags) */
                        strnfmt(out_val, 78, "(Flags %c-%c, +/-=next/prev set of flags) Add/Remove which flag? ",
                                I2A(0), I2A(max - 1));
                }
                else
                {
                        strnfmt(out_val, 78, "(Flags %c-%c, +/-=next/prev set of flags) Add/Remove which flag? ",
                                I2A(0), '0' + max - 27);
                }
                prt("Enter to accept, Escape to abord", 1, 0);
                c_prt(TERM_L_BLUE, format("Power(I/D to increase/decrease): %d", pval), 3, 0);

                /* Get a spell from the user */
                while (!(abord = !get_com(out_val, &choice)))
                {
                        if (choice == '+')
                        {
                                cur_set++;
                                if (cur_set > 3) cur_set = 0;
                                break;
                        }
                        else if (choice == '-')
                        {
                                cur_set--;
                                if (cur_set < 0) cur_set = 3;
                                break;
                        }
                        else if (choice == 'I')
                        {
                                pval++;
                                if (pval > 5) pval = 5;
                                break;
                        }
                        else if (choice == 'D')
                        {
                                pval--;
                                if (pval < 0) pval = 0;
                                break;
                        }
                        else if (choice == '\r')
                        {
                                done = TRUE;
                                break;
                        }
                        else if (isalpha(choice))
                        {
                                /* Lowercase */
                                if (isupper(choice)) choice = tolower(choice);

                                /* Extract request */
                                i = (islower(choice) ? A2I(choice) : -1);
                        }
                        else
                        {
                                i = D2I(choice) + 26;

                                /* Illegal */
                                if (i <= 26) i = -1;
                        }

                        /* Totally Illegal */
                        if ((i < 0) || (i >= max))
                        {
                                bell();
                                continue;
                        }
                        else if(p_ptr->lev < flags_level[cur_set][i].level)
                        {
                                bell();
                                continue;
                        }
                        else
                        {
                                if (flags_select[cur_set][i]) flags_select[cur_set][i] = FALSE;
                                else if (!flags_select[cur_set][i]) flags_select[cur_set][i] = TRUE;
                                break;
                        }
                }
        }

        /* Restore the screen */
        Term_load();
	character_icky = FALSE;

        if (abord || (q_ptr->exp - exp < 0)) return;

        /* Actually create the artifact */
        q_ptr->pval = pval;
        q_ptr->exp = 0;
        q_ptr->art_flags4 &= ~TR4_ART_EXP;

        /* Just to be sure */
        q_ptr->art_flags3 |= ( TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
	                       TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

        /* Apply the flags */
        for(i = 0; i < 32; i++)
        {
                if (flags_select[0][i]) q_ptr->art_flags1 |= flags_level[0][i].flag;
                if (flags_select[1][i]) q_ptr->art_flags2 |= flags_level[1][i].flag;
                if (flags_select[2][i]) q_ptr->art_flags3 |= flags_level[2][i].flag;
                if (flags_select[3][i]) q_ptr->art_flags4 |= flags_level[3][i].flag;
        }

	{
		char dummy_name[80];
                char new_name[80];

                strcpy(dummy_name, "of someone");
                identify_fully_aux(q_ptr, NULL);
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

	/* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
}

  /*
   * scan_monst --
   *
   * Return a list of o_list[] indexes of items of the given monster
   */
  /*
   * JKB: That comment should be taken out and shot.
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

        if (item != -1)
        {
                int chance;

                chance = 40 - p_ptr->stat_ind[A_DEX];
                chance += o_list[item].weight / ((p_ptr->pclass == CLASS_ROGUE)?20:1);
                chance += (p_ptr->pclass != CLASS_ROGUE)?30:0;
                chance -= (m_ptr->csleep)?10:0;
                chance += m_ptr->level;

                /* Failure check */
                if(rand_int(chance) > 1 + (p_ptr->lev / 2))
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
                if(num == 1) m_ptr->hold_o_idx = 0;
                else
                {
	                if(k > 0) o_list[monst_list[k - 1]].next_o_idx = monst_list[k + 1];
	                if(k + 1 >= num) o_list[monst_list[k - 1]].next_o_idx = 0;
	                if(k == 0) m_ptr->hold_o_idx = monst_list[k + 1];
                }

                /* Rogues gain some xp */
                if(p_ptr->pclass == CLASS_ROGUE)
                {
                        gain_exp((randint((o_list[item].weight / 2) + (m_ptr->level * 10)) / 2) + (((o_list[item].weight / 2) + (m_ptr->level * 10)) / 2));
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

/* Give an item to a monster */
void do_cmd_give()
{
        int dir, x ,y;
        cave_type *c_ptr;
	cptr q, s;
        int item;

        if (!get_rep_dir(&dir)) return;
        y = py + ddy[dir];
        x = px + ddx[dir];
        c_ptr = &cave[y][x];

        if (!(c_ptr->m_idx))
        {
                msg_print("There is no monster there.");
                return;
        }

	/* Get an item */
        q = "What item do you want to offer? ";
        s = "You have nothing to offer.";
        if (!get_item(&item, q, s, USE_INVEN)) return;

        hook_option = item;
        if (!process_hooks(HOOK_GIVE, c_ptr->m_idx))
        {
                msg_print("The monster does not want your item.");
        }
}
