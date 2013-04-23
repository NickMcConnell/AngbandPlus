#define CMD2_C
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

static int chest_check(int y, int x);

 #if defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM)
 
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
 * Move up or down via stairs.
 * deeper stores whether we are moving to a deeper or a shallower level.
 * stairs stores whether we want stairs at the player's feet or not.
 */
static void use_stairs(bool deeper, byte stairs)
{
	byte j = (multi_stair) ? randint(5) : 1;
	byte start = (stairs) ? START_STAIRS : START_RANDOM;
	if (j > dun_level) j = 1;

	/* Don't allow the player to pass a quest level.
	 * This is very inefficient, but it's a very simple process.
	 */
	if (deeper)
	{
		byte i;
		for (i = 1; i < j; i++)
		{
			if (is_quest(dun_level+i)) break;
			if (dun_level+i == dun_defs[cur_dungeon].max_level) break;
		}
		change_level(dun_level+i, start);
	}
	else
	{
		change_level(dun_level-j, start);

		/* Check for leaving dungeon */
		if(dun_level == 0)
		{
			recall_dungeon = cur_dungeon;
			wildx=dun_defs[cur_dungeon].x;
			wildy=dun_defs[cur_dungeon].y;
		}
	}
}

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
		if (dun_level <= 0)
		{
			go_up = TRUE;
		}
		else
		{
			if (confirm_stairs)
			{
				if (get_check("Really leave the level? "))
				{
					go_up = TRUE;
				}
			}
			else
			{
				go_up = TRUE;
			}
		}

		if(go_up == FALSE) msg_print("'go_up' = FALSE");

		if (go_up)
		{
			energy_use = 0;
			/* Success */
			/* Check for entering a tower */
			if(dun_level==0)
			{
				cur_dungeon = wild_grid[wildy][wildx].dungeon;
				p_ptr->max_dlv = 1;
				msg_format("You enter %s",dun_name+dun_defs[cur_dungeon].name);
			}
			else
			{
				msg_print("You enter a maze of up staircases.");
			}

			/* Actually go up. */
			use_stairs(!!(dun_defs[cur_dungeon].flags & DF_TOWER), TRUE);
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
		if (dun_level < 1)
		{
			go_down = TRUE;
		}
		else
		{
			if (confirm_stairs)
			{
				if (get_check("Really leave the level? "))
				{
					go_down = TRUE;
				}
			}
			else
			{
				go_down = TRUE;
			}
		}
		if (go_down)
		{
			energy_use = 0;
			if (fall_trap)
			{
				msg_print("You deliberately jump through the trap door.");
			}
			else
			{
				/* Success */
				/* Check for entering a dungeon */
				if(dun_level==0)
				{
					cur_dungeon = wild_grid[wildy][wildx].dungeon;
					p_ptr->max_dlv = 1;
					msg_format("You enter %s",dun_name+dun_defs[cur_dungeon].name);
				}
				else
				{
					msg_print("You enter a maze of down staircases.");
				}
			}
				/* Go down */
			use_stairs(!(dun_defs[cur_dungeon].flags & DF_TOWER), !fall_trap);
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
	energy_use = extract_energy[p_ptr->pspeed];

	/* Search */
	search();
}


/*
 * Hack -- toggle sneak mode
 */
void do_cmd_toggle_sneak(void)
{
	/* Stop sneaking */
	if (p_ptr->sneaking)
	{
		/* Clear the sneaking flag */
		p_ptr->sneaking = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Start sneaking */
	else
	{
		/* Set the sneaking flag */
		p_ptr->sneaking = TRUE;

		/* Update stuff */
		p_ptr->update |= (PU_BONUS);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_STATE | PR_SPEED);
	}
}



/*
 * Determine if a grid contains a chest
 */
static int chest_check(int y, int x)
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
	object_kind *k_ptr = &k_info[o_ptr->k_idx];


	/* Small chests often hold "gold" */
	small = (k_ptr->extra / 10) == XT_CHEST_SMALL;

	/* Determine how much to drop (see above) */
	number = chest_number(k_ptr);

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
	object_level = dun_depth;

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
            if (randint(100)<(dun_depth))
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


	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

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
static bool do_cmd_open_aux(int y, int x, int UNUSED dir)
{
	int i, j;

	cave_type *c_ptr;

	bool more = FALSE;

	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

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
	int y, x, dir;
	
	s16b o_idx;

	cave_type *c_ptr;

	bool more = FALSE;

 #ifdef ALLOW_EASY_OPEN
 
 	/* Option: Pick a direction */
 	if (easy_open) {
 
 	    int num_doors, num_chests;
 	
 	    /* Count closed doors (locked or jammed) */
 	    num_doors = count_dt(&y, &x, FEAT_DOOR_HEAD, FEAT_DOOR_TAIL);
 	    
 	    /* Count chests (locked) */
 	    num_chests = count_chests(&y, &x, FALSE);
 	    
 	    /* See if only one target */
 	    if (num_doors || num_chests) {
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
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

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
static bool do_cmd_close_aux(int y, int x, int UNUSED dir)
{
	cave_type	*c_ptr;

	bool		more = FALSE;

	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

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
	int			y, x, dir;

	cave_type	*c_ptr;

	bool		more = FALSE;

 #ifdef ALLOW_EASY_OPEN
 
 	/* Option: Pick a direction */
 	if (easy_open) {
 
 		/* Count open doors */
 		if (count_dt(&y, &x, FEAT_OPEN, FEAT_OPEN) == 1) {
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
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

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
static void twall(int y, int x)
{
	cave_type	*c_ptr = &cave[y][x];

	/* Forget the wall */
	c_ptr->info &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* Update some things */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
}



/* Special wall features for do_cmd_tunnel_aux(). */
#define WALL_GOLD	1	/* Contains gold. */
#define WALL_OBJ	2	/* May contain an object */
#define WALL_DOOR	3	/* Is really a secret door. */
#define WALL_NO_DIG	4	/* Undiggable. */

/* Messages given by a particular type of wall. */
#define WMSG_TREE	0	/* Tree, Bush */
#define WMSG_WATER	1	/* Water */
#define WMSG_WALL	2	/* Walls (all types), Door */
#define WMSG_GOLD	3	/* Walls with gold in them (no success message) */
#define WMSG_RUBBLE	4	/* Rubble */
#define WMSG_MAX	5

/*
 * Perform the basic "tunnel" command
 *
 * Assumes that the feature type is in wall_info[].feature below.
 *
 * Assumes that no monster is blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x, int UNUSED dir)
{
	cave_type *c_ptr;

	typedef struct wall_type wall_type;
	typedef struct wall_message_type wall_message_type;

	struct wall_type
	{
		byte feature;	/* Name of the feature being dug. */
		byte skill_min;	/* Minimum p_ptr->skill_dig to dig through the wall. */
		s16b skill_rand;	/* Random factor for digging. */
		byte special;	/* WALL_* entry for special cases. */
		byte msg;	/* The set of messages to use (see wall_message) */
		cptr name;	/* Name of the wall type. Derived from f_name if none. */
	};

	struct wall_message_type
	{
		cptr cont;
		cptr fail;
		cptr succeed;
	};

	wall_type wall_info[] =
	{
		{FEAT_SECRET, 30, 1200, WALL_DOOR, WMSG_WALL, NULL},
		{FEAT_RUBBLE, 0, 200, WALL_OBJ, WMSG_RUBBLE, "rubble"},
		{FEAT_MAGMA, 10, 400, 0, WMSG_WALL, NULL},
		{FEAT_QUARTZ, 20, 800, 0, WMSG_WALL, NULL},
		{FEAT_MAGMA_H, 10, 400, WALL_GOLD, WMSG_GOLD, NULL},
		{FEAT_QUARTZ_H, 20, 800, WALL_GOLD, WMSG_GOLD, NULL},
		{FEAT_MAGMA_K, 10, 400, WALL_GOLD, WMSG_GOLD, "magma vein"},
		{FEAT_QUARTZ_K, 20, 800, WALL_GOLD, WMSG_GOLD, "quartz vein"},
		{FEAT_WALL_EXTRA, 40, 1600, 0, WMSG_WALL, NULL},
		{FEAT_WALL_INNER, 40, 1600, 0, WMSG_WALL, NULL},
		{FEAT_WALL_OUTER, 40, 1600, 0, WMSG_WALL, NULL},
		{FEAT_WALL_SOLID, 40, 1600, 0, WMSG_WALL, NULL},
		{FEAT_PERM_BUILDING, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
		{FEAT_PERM_INNER, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
		{FEAT_PERM_OUTER, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
		{FEAT_PERM_SOLID, 0, 0, WALL_NO_DIG, WMSG_WALL, NULL},
		{FEAT_WATER, 0, 0, WALL_NO_DIG, WMSG_WATER, NULL},
		{FEAT_TREE, 40, 100, 0, WMSG_TREE, NULL},
		{FEAT_BUSH, 0, 1, 0, WMSG_TREE, NULL},
		{FEAT_NONE,0,0,0,0,0}
	}, *w_ptr;
	wall_message_type wall_message[WMSG_MAX] =
	{
		{"You hack away at the %s.", "You leave no mark on the %s.", "You have chopped down the %s."},
		{"You empty out the %s.", "The %s fills up your tunnel as quickly as you dig!", "You have removed the %s."},
		{"You tunnel into the %s.", "You make no impression on the %s.", "You have finished the tunnel."},
		{"You tunnel into the %s.", "You make no impression on the %s.", ""},
		{"You dig in the %s.", "You make no impression on the %s.", "You have removed the %s."},
	}, *wm_ptr;

	bool more = FALSE;

	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];

	/* Get grid */
	c_ptr = &cave[y][x];

	/* Sound */
	sound(SOUND_DIG);

	for (w_ptr = wall_info; w_ptr->feature != FEAT_NONE; w_ptr++)
		if (w_ptr->feature == c_ptr->feat) break;

	/* Paranoia - not a valid wall (should not reach here). */
	if (w_ptr->feature == FEAT_NONE)
		{
		if (alert_failure) msg_print("Strange wall type found!");
		return FALSE;
	}

	/* the "name" field is only used if the name in f_name isn't suitable. */
	if (!w_ptr->name) w_ptr->name = f_name+f_info[f_info[w_ptr->feature].mimic].name;

	/* Find the message set. */
	wm_ptr = &wall_message[w_ptr->msg];

	/* Certain failure */
	if (w_ptr->special == WALL_NO_DIG || p_ptr->skill_dig <= w_ptr->skill_min)
		{
		msg_format(wm_ptr->fail, w_ptr->name);
		}
	/* Normal failure */
	else if (p_ptr->skill_dig <= w_ptr->skill_min + rand_int(w_ptr->skill_rand))
		{
		msg_format(wm_ptr->cont, w_ptr->name);
		more = TRUE;

		/* Occasional Search XXX XXX */
		if (w_ptr->special == WALL_DOOR && rand_int(100) < 25) search();
	}
		/* Success */
	else
			{
		bool found = FALSE;

		/* Actually tunnel through wall. */
		twall(y, x);

				/* Message */
		msg_format(wm_ptr->succeed, w_ptr->name);

		/* Handle special cases. */
		switch (w_ptr->special)
		{
			case WALL_GOLD:
		{
				place_gold(y,x);
				found = TRUE;
	}
			break;
			case WALL_OBJ:
			if (rand_int(100) < 10)
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE);

				/* Observe new object */
				if (player_can_see_bold(y, x)) found = TRUE;
			}
			break;
		}

		if (found) msg_print("You have found something!");
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
        if (cave_floor_grid(c_ptr) || ((c_ptr->feat >= FEAT_MINOR_GLYPH)
                                       && (c_ptr->feat <= FEAT_PATTERN_XTRA2)))
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
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

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

 #ifdef ALLOW_EASY_OPEN
 
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
	energy_use = extract_energy[p_ptr->pspeed];

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
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
 #ifdef ALLOW_EASY_DISARM
 
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
	energy_use = extract_energy[p_ptr->pspeed];

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
		skill_exp(SKILL_DISARM);

		/* Forget the trap */
		c_ptr->info &= ~(CAVE_MARK);

		/* Remove the trap */
		cave_set_feat(y, x, FEAT_FLOOR);

 #ifdef ALLOW_EASY_DISARM
 
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

 #ifdef ALLOW_EASY_DISARM
 
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

 #ifdef ALLOW_EASY_DISARM
 
 	/* Option: Pick a direction */
 	if (easy_disarm) {
 
 	    int num_traps, num_chests;
 	
 	    /* Count visible traps */
 	    num_traps = count_dt(&y, &x, FEAT_TRAP_HEAD, FEAT_TRAP_TAIL);
 	    
 	    /* Count chests (trapped) */
 	    num_chests = count_chests(&y, &x, TRUE);
 	    
 	    /* See if only one target */
 	    if (num_traps || num_chests) {
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
	energy_use = extract_energy[p_ptr->pspeed];

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
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

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
 * Attack a monster at a given location, return FALSE if it dies or disappears.
 *
 * This only aborts the repetition when the player is unable to continue
 * attacking. Any abort desired based on the monster's actions should be
 * handled elsewhere.
 */
static bool alter_monster(int y, int x)
{
	cave_type *c_ptr = cave[y]+x;

	monster_type *m_ptr = m_list+c_ptr->m_idx;
	
	/* Paranoia */
	if (!cave[y][x].m_idx || !m_ptr->r_idx) return FALSE;

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
		energy_use = extract_energy[p_ptr->pspeed];

		/* Attack monsters */
		if (c_ptr->m_idx)
		{
			/* Attack */
			more = alter_monster(y, x);
			alter_cmd = 'H';
		}

		/* Avoid continuing to fight after the monster has gone. */
		else if (alter_cmd == 'H')
		{
			energy_use = 0;
		}
		
		/* Tunnel through walls */
        else if (((c_ptr->feat >= FEAT_SECRET)
            && (c_ptr->feat < FEAT_MINOR_GLYPH)) ||
				(c_ptr->feat == FEAT_WATER) ||
				(c_ptr->feat == FEAT_TREE) ||
				(c_ptr->feat == FEAT_BUSH))
		{
			/* Tunnel */
			more = do_cmd_tunnel_aux(y, x, dir);
			alter_cmd = 'T';
		}

		/* Bash jammed doors */
        else if ((c_ptr->feat >= FEAT_DOOR_HEAD + 0x08)
            && (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_bash_aux(y, x, dir);
			alter_cmd = 'B';
		}

		/* Open closed doors */
        else if ((c_ptr->feat >= FEAT_DOOR_HEAD)
            && (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_open_aux(y, x, dir);
			alter_cmd = 'o';
		}

		/* Disarm traps */
        else if ((c_ptr->feat >= FEAT_TRAP_HEAD)
            && (c_ptr->feat < FEAT_MINOR_GLYPH))
		{
			/* Tunnel */
			more = do_cmd_disarm_aux(y, x, dir);
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
		disturb(0, 0);
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
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
	int                  y, x, dir;
	object_type *o_ptr;

	cave_type		*c_ptr;


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
		else if (!(o_ptr = get_spike()))
		{
			/* Message */
			msg_print("You have no spikes!");
		}

		/* Is a monster in the way? */
		else if (c_ptr->m_idx)
		{
			/* Take a half turn */
			energy_use = (extract_energy[p_ptr->pspeed]+1)/2;

			/* Message */
			msg_print("There is a monster in the way!");

			/* Attack */
			py_attack(y, x);
		}

		/* Go for it */
		else
		{
			/* Take a turn */
			energy_use = extract_energy[p_ptr->pspeed];

			/* Successful jamming */
			msg_print("You jam the door with a spike.");

			/* Convert "locked" to "stuck" XXX XXX XXX */
			if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

			/* Add one spike to the door */
			if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;

			/* Use up, and describe, a single spike, from the bottom */
			item_increase(o_ptr, -1);
			item_describe(o_ptr);
			item_optimize(o_ptr);
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
		energy_use = extract_energy[p_ptr->pspeed]; 

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
	energy_use = extract_energy[p_ptr->pspeed];


	/* Spontaneous Searching */
	if ((p_ptr->skill_fos >= 50) || (0 == rand_int(50 - p_ptr->skill_fos)))
	{
		search();
	}

	/* Continuous sneaking */
	if (p_ptr->sneaking)
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
		command_new = KTRL('E');
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
	energy_use = extract_energy[p_ptr->pspeed];

	/* Save the rest code */
	resting = command_arg;

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
		case TV_SCROLL:
		case TV_ARROW:
		case TV_SKELETON:
		case TV_CHARM:
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
	errr err;
	int dir;
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
	if (!((o_ptr = get_item(&err, "Fire which item? ", FALSE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to fire.");
		return;
	}


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir))
	{
		return;
	}


	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Single object */
	q_ptr->number = 1;

	/* Reduce and describe object */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);


	/* Sound */
	sound(SOUND_SHOOT);


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

	/* Find the base multiplier */
	tmul = get_bow_mult(j_ptr);

	/* Get extra "power" from "extra might" */
	if (p_ptr->xtra_might) tmul++;

	/* Boost the damage */
	tdam *= tmul;

	/* Base range */
	tdis = 10 + 5 * tmul;


	/* Take a (partial) turn */
	energy_use = (60*TURN_ENERGY / thits);


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
				
				/* Give experience (if it wasn't too easy) */
				if ((chance - cur_dis) < (r_ptr->ac * 3))
				{
					skill_exp(SKILL_MISSILE);
				}

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
				    (r_ptr->flags3 & (RF3_UNDEAD)) ||
				    (r_ptr->flags3 & (RF3_CTHULOID)) ||
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
					msg_format("The %v finds a mark.",
						object_desc_f3, q_ptr, FALSE, 3);
				}

				/* Handle visible monster */
				else
				{
					/* Message */
					msg_format("The %v hits %v.",
						object_desc_f3, q_ptr, FALSE, 3,
						monster_desc_f2, m_ptr, 0);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);

					/* Anger friends */
					if (m_ptr->smart & SM_ALLY) {
						msg_format("%v gets angry!", monster_desc_f2, m_ptr, 0);
						m_ptr->smart &= ~SM_ALLY;
					}
              }

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr);
				tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (cheat_wzrd)
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
						/* Sound */
						sound(SOUND_FLEE);

						/* Message */
						msg_format("%^v flees in terror!",
							monster_desc_f2, m_ptr, 0);
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
 * Hook to determine if an item is not both worn and cursed.
 */
bool item_tester_hook_destroy(object_type *o_ptr)
{
	/* Accept all uncursed items. */
	if (!cursed_p(o_ptr)) return TRUE;

	/* Accept all non-worn items. */
	if (o_ptr < &inventory[INVEN_WIELD] || o_ptr > &inventory[INVEN_FEET]) return TRUE;

	/* Reject everything else. */
	return FALSE;
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
	errr err;
	int dir;
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

	int msec = delay_factor * delay_factor * delay_factor;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_destroy;

	/* Get an item (from inven or floor) */
	if (!((o_ptr = get_item(&err, "Throw which item? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to throw.");
		return;
	}


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return;


	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Single object */
	q_ptr->number = 1;

	/* Reduce and describe item */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);


	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(q_ptr);
	missile_char = object_char(q_ptr);


	/* Extract a "distance multiplier" */
	/* Changed for 'launcher' chaos feature */
	mul = 10 + 2 * (throw_mult - 1);

	/* Enforce a minimum "weight" of one pound */
	div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

	/* Max distance of 10 */
	if (tdis > 10) tdis = 10;

	/* Hack -- Base damage from thrown object */
	tdam = damroll(q_ptr->dd, q_ptr->ds) + q_ptr->to_d;
	tdam *= throw_mult;

	/* Chance of hitting */
	chance = (p_ptr->skill_tht + (p_ptr->to_h * BTH_PLUS_ADJ));


	/* Take a turn */
	energy_use = extract_energy[p_ptr->pspeed];


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

				/* Give experience (if it wasn't too easy) */
				if ((chance - cur_dis) < (r_ptr->ac * 3))
				{
					skill_exp(SKILL_MISSILE);
				}

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
				    (r_ptr->flags3 & (RF3_UNDEAD)) ||
				    (r_ptr->flags3 & (RF3_CTHULOID)) ||
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
					msg_format("The %v finds a mark.",
						object_desc_f3, q_ptr, FALSE, 3);
				}

				/* Handle visible monster */
				else
				{
					/* Message */
					msg_format("The %v hits %v.",
						object_desc_f3, q_ptr, FALSE, 3, 
						monster_desc_f2, m_ptr, 0);

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
				if (cheat_wzrd)
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
                   if ((m_ptr->smart & SM_ALLY)
                    && (!(k_info[q_ptr->k_idx].tval == TV_POTION)))
                    {
                      msg_format("%v gets angry!", monster_desc_f2, m_ptr, 0);
                      m_ptr->smart &= ~SM_ALLY;
                   }

					/* Take note */
                    if (fear && m_ptr->ml)
					{
						/* Sound */
						sound(SOUND_FLEE);

						/* Message */
						msg_format("%^v flees in terror!",
							monster_desc_f2, m_ptr, 0);
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
     if (k_info[q_ptr->k_idx].tval == TV_POTION) {
       if ((hit_body) || (!cave_floor_bold(ny, nx)) || (cave[ny][nx].feat == FEAT_WATER) || (randint(100) < j)) {
       /* Message */
       msg_format("The %v shatters!", object_desc_f3, q_ptr, FALSE, 3);
       if (potion_smash_effect(1, y, x, q_ptr->k_idx))
       {
              if (cave[y][x].m_idx && (m_list[cave[y][x].m_idx].smart & SM_ALLY))
                    {
                   msg_format("%v gets angry!", monster_desc_f2, &m_list[cave[y][x].m_idx], 0);
                   m_list[cave[y][x].m_idx].smart &= ~SM_ALLY;
               }
            }

       return;
       } else {
       j = 0;
       }
      }


	/* Drop (or break) near that location */
	drop_near(q_ptr, j, y, x);

	return;
}


/* If a power has a negative cost, the cost given is a reference to this table. This can only express
minc+plev/levc at present, but could be replaced by a more complex expression, or by pointers to a string and a function to allow arbitrary functions. */

typedef struct powercosttype powercosttype;
struct powercosttype
{
	byte minc;
	byte levc;
};
static powercosttype powercosts[] = {
{0,0},{0,1},
{0,0},{1,3},
{0,0},{5,5},
};

/* The methods by which a power may be derived. */
#define POWER_RACIAL 1 /* Relevant if (p_ptr->prace == power % MAX_RACES). */
#define POWER_MUTA1 2 /* Relevant if (p_ptr->muta1 & 1<<(power%32)). */
#define POWER_MUTA2 3 /* Relevant if (p_ptr->muta2 & 1<<(power%32)). */
#define POWER_MUTA3 4 /* Relevant if (p_ptr->muta3 & 1<<(power%32)). */
#define POWER_DISMISS 5 /* Relevant if you have pets. */
#define POWER_DISMISS_ONE 0 /* Dismiss one pet */
#define POWER_DISMISS_MANY 1 /* Dismiss more than one pet */

typedef struct powertype powertype;
struct powertype
{
	byte type;	/* Determine which style of index to use. */
	byte power;	/* An index to the race or mutation responsible. */
	s16b min_level;	/* A penalty for racial_success_chance() which depends partly on level. */
	s16b cost;	/* Cost of power. Negative values refer to skill-based values */
	byte use_stat;  /* The stat considered by racial_success_chance() */
	int difficulty; /* Another penalty for racial_success_chance(). */
	cptr text;	/* The description of the power given to the player. */
	cptr text2;	/* An additional phrase to be inserted after the cost is given. */
};
/* The string will be turned into a longer form later. 
 * In general, this is text+"     "+"(racial, cost power->cost, power->use_stat power->difficulty)".
 * If power->cost is negative, the cost is taken from powercosts[-power->cost].
 * If text2 isn't 0, it is added as a separate statement after the cost, separated from that and
 * power->use_stat by commas. 
 * The number of spaces will be adjusted as necessary.
 */
/*
 * The table of powers, as defined above. Note that the effects of powers must be described in use_power().
 */
static powertype powers[] = {
{POWER_RACIAL, RACE_DWARF, 5, 5, A_WIS, 12, "detect doors+traps",0},
{POWER_RACIAL, RACE_NIBELUNG, 10, 5, A_WIS, 10,"detect doors+traps",0},
{POWER_RACIAL, RACE_HOBBIT, 15, 10, A_INT, 10, "create food",0},
{POWER_RACIAL, RACE_GNOME, 5, -5, A_INT, 12, "teleport",0},
{POWER_RACIAL, RACE_HALF_ORC, 3, 5, A_WIS, 10, "remove fear",0},
{POWER_RACIAL, RACE_HALF_TROLL, 10, 12, A_WIS, 12, "berserk",0},
{POWER_RACIAL, RACE_BARBARIAN, 8, 10, A_WIS, 12, "berserk",0},
{POWER_RACIAL, RACE_GREAT, 40, 75, A_WIS, 50, "dreaming",0},
{POWER_RACIAL, (RACE_GREAT+MAX_RACES), 30, 50, A_INT, 50, "dream travel",0},
{POWER_RACIAL, RACE_HALF_OGRE, 25, 35, A_INT, 15, "explosive rune",0},
{POWER_RACIAL, RACE_HALF_GIANT, 20, 10, A_STR, 12, "stone to mud",0},
{POWER_RACIAL, RACE_HALF_TITAN, 35, 20, A_INT, 12, "probing",0},
{POWER_RACIAL, RACE_CYCLOPS, 20, 15, A_STR, 12, "throw boulder","dam 3*lvl"},
{POWER_RACIAL, RACE_YEEK, 15, 15, A_WIS, 10, "scare monster",0},
{POWER_RACIAL, RACE_SPECTRE, 4, 6, A_INT, 3, "scare monster",0},
{POWER_RACIAL, RACE_BROO, 4, 6, A_INT, 3, "scare monster",0},
{POWER_RACIAL, RACE_KLACKON, 9, 9, A_DEX, 14, "spit acid","dam lvl"},
{POWER_RACIAL, RACE_KOBOLD, 12, 8, A_DEX, 18, "poison dart","dam lvl"},
{POWER_RACIAL, RACE_DARK_ELF, 2, 2, A_INT, 9, "magic missile",0},
{POWER_RACIAL, RACE_DRACONIAN, 1, -1, A_CON, 12, "breath weapon","dam 2*lvl"},
{POWER_RACIAL, RACE_MIND_FLAYER, 15, 12, A_INT, 14, "mind blast","dam lvl"},
{POWER_RACIAL, RACE_IMP, 9, 15, A_WIS, 15, "fire bolt/ball(30)","dam lvl"},
{POWER_RACIAL, RACE_GOLEM, 20, 15, A_CON, 8, "stone skin","dur 30+d20"},
{POWER_RACIAL, RACE_SKELETON, 30, 30, A_WIS, 18, "restore life",0},
{POWER_RACIAL, RACE_ZOMBIE, 30, 30, A_WIS, 18, "restore life",0},
{POWER_RACIAL, RACE_VAMPIRE, 2, -3, A_CON, 9, "drain life",0},
{POWER_RACIAL, RACE_SPRITE, 12, 12, A_INT, 15, "sleeping dust",0},
{POWER_MUTA1, iilog(MUT1_SPIT_ACID), 9, 9, A_DEX, 15, "spit acid","dam lvl"},
{POWER_MUTA1, iilog(MUT1_BR_FIRE), 20, -1, A_CON, 18, "fire breath","dam lvl*2"},
{POWER_MUTA1, iilog(MUT1_HYPN_GAZE), 12, 12, A_CHR, 18, "hypnotic gaze",0},
{POWER_MUTA1, iilog(MUT1_TELEKINES), 9, 9, A_WIS, 14, "telekinesis",0},
{POWER_MUTA1, iilog(MUT1_VTELEPORT), 7, 7, A_WIS, 15, "teleport",0},
{POWER_MUTA1, iilog(MUT1_MIND_BLST), 5, 3, A_WIS, 15, "mind blast",0},
{POWER_MUTA1, iilog(MUT1_RADIATION), 15, 15, A_CON, 14, "emit radiation",0},
{POWER_MUTA1, iilog(MUT1_VAMPIRISM), 13, -1, A_CON, 14, "vampiric drain",0},
{POWER_MUTA1, iilog(MUT1_SMELL_MET), 3, 2, A_INT, 12, "smell metal",0},
{POWER_MUTA1, iilog(MUT1_SMELL_MON), 5, 4, A_INT, 15, "smell monsters",0},
{POWER_MUTA1, iilog(MUT1_BLINK), 3, 3, A_WIS, 12, "blink",0},
{POWER_MUTA1, iilog(MUT1_EAT_ROCK), 8, 12, A_CON, 18, "eat rock",0},
{POWER_MUTA1, iilog(MUT1_SWAP_POS), 15, 12, A_DEX, 16, "swap position",0},
{POWER_MUTA1, iilog(MUT1_SHRIEK), 4, 4, A_CON, 6, "shriek",0},
{POWER_MUTA1, iilog(MUT1_ILLUMINE), 3, 2, A_INT, 10, "illuminate",0},
{POWER_MUTA1, iilog(MUT1_DET_CURSE), 7, 14, A_WIS, 14, "detect curses",0},
{POWER_MUTA1, iilog(MUT1_BERSERK), 8, 8, A_STR, 14, "berserk",0},
{POWER_MUTA1, iilog(MUT1_POLYMORPH), 18, 20, A_CON, 18, "polymorph",0},
{POWER_MUTA1, iilog(MUT1_MIDAS_TCH), 10, 5, A_INT, 12, "midas touch",0},
{POWER_MUTA1, iilog(MUT1_GROW_MOLD), 1, 6, A_CON, 14, "grow mold",0},
{POWER_MUTA1, iilog(MUT1_RESIST), 10, 12, A_CON, 12, "resist elements",0},
{POWER_MUTA1, iilog(MUT1_EARTHQUAKE), 12, 12, A_STR, 16, "earthquake",0},
{POWER_MUTA1, iilog(MUT1_EAT_MAGIC), 17, 1, A_WIS, 15, "eat magic",0},
{POWER_MUTA1, iilog(MUT1_WEIGH_MAG), 6, 6, A_INT, 10, "weigh magic",0},
{POWER_MUTA1, iilog(MUT1_STERILITY), 20, 40, A_CHR, 18, "sterilize",0},
{POWER_MUTA1, iilog(MUT1_PANIC_HIT), 10, 12, A_DEX, 14, "panic hit",0},
{POWER_MUTA1, iilog(MUT1_DAZZLE), 7, 15, A_CHR, 8, "dazzle",0},
{POWER_MUTA1, iilog(MUT1_EYE_BEAM), 7, 10, A_WIS, 9, "eye beams",0},
{POWER_MUTA1, iilog(MUT1_RECALL), 17, 50, A_INT, 16, "recall",0},
{POWER_MUTA1, iilog(MUT1_BANISH), 25, 25, A_WIS, 18, "banish evil",0},
{POWER_MUTA1, iilog(MUT1_COLD_TOUCH), 2, 2, A_CON, 11, "cold touch",0},
{POWER_MUTA1, iilog(MUT1_LAUNCHER), 1, -1, A_STR, 6, "throw object",0},
{POWER_DISMISS, POWER_DISMISS_ONE, 0, 0, 0, 0, "dismiss ally",0},
{POWER_DISMISS, POWER_DISMISS_MANY, 0, 0, 0, 0, "dismiss allies",0},
{0,0,0,0,0,0,0,0}};
#define MAX_POWERS	36 /* There shouldn't be more powers than this at a time. */
static powertype *cur_powers[MAX_POWERS];


#define RACIAL_MIN_X 13 /* The distance across the screen at which the display starts. */
#define MIN_STRING_LEN 25 /* The space allowed for all description strings. Longer strings are still longer. */


#ifndef A_MAX
#define A_MAX 6
#endif

static void dismiss_pets(bool some);
static void racial_confirm_string(byte choice, char * out);
static bool racial_aux(powertype *pw_ptr);
static void racial_success_chance(powertype *pw_ptr, s16b *num, s16b *denom);
static byte display_list(void (*display)(byte, byte *, char *), void (*confirm)(byte, char *), byte, cptr, cptr, cptr);
static void racial_string(byte num, byte *x, char * text);
static s16b count_powers(void);
static void use_power(powertype *pw_ptr);

/*
 * Actually put a racial or mutation-based activation into effect.
 * See the individual entries for details.
 */

static void use_power(powertype *pw_ptr)
	{
	const byte plev = MIN(skill_set[SKILL_RACIAL].value/2,1);
	int dir;
	s16b i;

	switch (pw_ptr->type)
{
		case POWER_RACIAL:
		switch (pw_ptr->power)
    {
        case RACE_DWARF:
                    msg_print("You examine your surroundings.");
                    (void)detect_traps();
					mark_traps();
                    (void)detect_doors();
                    (void)detect_stairs();
                break;
        case RACE_HOBBIT:
                {
                    /* Get local object */
				object_type q;

                   /* Create the item */
				object_prep(&q, OBJ_RATION_OF_FOOD);

                    /* Drop the object from heaven */
				drop_near(&q, -1, py, px);
                    msg_print("You cook some food.");
                }
                break;
        case RACE_GNOME:
                msg_print("Blink!");
                teleport_player(10 + (plev));
            break;
        case RACE_HALF_ORC:
                    msg_print("You play tough.");
                    (void)set_afraid(0);
            break;
        case RACE_HALF_TROLL:
                msg_print("RAAAGH!");
                (void)set_afraid(0);
                (void)set_shero(p_ptr->shero + 10 + randint(plev));
                (void)hp_player(30);
                        break;
			case RACE_GREAT: /* Dreaming */
                    msg_print("You dream of a time of health and peace...");
                    (void)set_poisoned(0);
                    (void)set_image(0);
                    (void)set_stun(0);
                    (void)set_cut(0);
                    (void)set_blind(0);
                    (void)set_afraid(0);
                    (void)do_res_stat(A_STR);
                    (void)do_res_stat(A_INT);
                    (void)do_res_stat(A_WIS);
                    (void)do_res_stat(A_DEX);
                    (void)do_res_stat(A_CON);
                    (void)do_res_stat(A_CHR);
                    (void)restore_level();
				break;
			case RACE_GREAT+MAX_RACES: /* dream travel */
                    msg_print("You start walking around. Your surroundings change.");
			change_level(dun_level, START_RANDOM);
            break;
        case RACE_BARBARIAN:
                msg_print("Raaagh!");
                (void)set_afraid(0);
                (void)set_shero(p_ptr->shero + 10 + randint(plev));
                (void)hp_player(30);
            break;
        case RACE_HALF_OGRE:
                msg_print("You carefully set an explosive rune...");
                explosive_rune();
            break;
        case RACE_HALF_GIANT:
                if (!get_aim_dir(&dir)) break;
                msg_print("You bash at a stone wall.");
                (void)wall_to_mud(dir);
            break;
        case RACE_HALF_TITAN:
                msg_print("You examine your foes...");
                probing();
            break;
        case RACE_CYCLOPS:
                if (!get_aim_dir(&dir)) break;
                msg_print("You throw a huge boulder.");
                fire_bolt(GF_MISSILE, dir, (3 * plev) / 2);
            break;
        case RACE_YEEK:
                if (!get_aim_dir(&dir)) break;
                msg_print("You make a horrible scream!");
                (void)fear_monster(dir, plev);
            break;
        case RACE_KLACKON:
                if (!(get_aim_dir(&dir))) break;
                msg_print("You spit acid.");
                if (plev < 25)
                    fire_bolt(GF_ACID, dir, plev);
                else
                    fire_ball(GF_ACID, dir, plev, 2);
            break;
        case RACE_KOBOLD:
                if(!get_aim_dir(&dir)) break;
                msg_print("You throw a dart of poison.");
                fire_bolt(GF_POIS, dir, plev);
            break;
        case RACE_NIBELUNG:
                    msg_print("You examine your surroundings.");
                    (void)detect_traps();
					mark_traps();
                    (void)detect_doors();
                    (void)detect_stairs();
                break;
        case RACE_DARK_ELF:
                if (!get_aim_dir(&dir)) break;
                msg_print("You cast a magic missile.");
                fire_bolt_or_beam(10, GF_MISSILE, dir,
                          damroll(3 + ((plev - 1) / 5), 4));
            break;
        case RACE_DRACONIAN:
                if (!get_aim_dir(&dir)) break;
				msg_format("You breathe poison.");
				fire_ball(GF_POIS, dir, (plev)*2, -((plev)/15) + 1);
            break;
        case RACE_MIND_FLAYER:
                if (!get_aim_dir(&dir)) break;
                    msg_print("You concentrate and your eyes glow red...");
                    fire_bolt(GF_PSI, dir, plev);
            break;
        case RACE_IMP:
                if (!get_aim_dir(&dir)) break;
                if (plev >= 30)
                {
                    msg_print("You cast a ball of fire.");
                    fire_ball(GF_FIRE, dir, plev, 2);
                }
                else
                {
                    msg_print("You cast a bolt of fire.");
                    fire_bolt(GF_FIRE, dir, plev);
                }
            break;
        case RACE_GOLEM:
                    (void)set_shield(p_ptr->shield + randint(20) + 30);
            break;
        case RACE_SKELETON: case RACE_ZOMBIE:
                msg_print("You attempt to restore your lost energies.");
                (void)restore_level();
            break;
       case RACE_VAMPIRE:
       /* Only works on adjacent monsters */
			{
				cave_type *c_ptr;
				s16b dummy;
                     if (!get_rep_dir(&dir)) break;   /* was get_aim_dir */
				c_ptr = &cave[py+ddy[dir]][px+ddx[dir]];
           if (!(c_ptr->m_idx)) {
               msg_print("You bite into thin air!");
               break;
           }
                     msg_print("You grin and bare your fangs...");
           dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
				if (!drain_life(dir, dummy))
				{
					msg_print("Yechh. That tastes foul.");
				}
				else
				{
                       if (p_ptr->food < PY_FOOD_FULL)
						/* No heal if we are "full" (although we still get more full). */
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
            }
            break;
        case RACE_SPECTRE:
                     msg_print("You emit an eldritch howl!");
                     if (!get_aim_dir(&dir)) break;
                     (void)fear_monster(dir, plev);
              break;
        case RACE_BROO:
                     msg_print("You emit a fearsome growl!");
                     if (!get_aim_dir(&dir)) break;
                     (void)fear_monster(dir, plev);
              break;
        case RACE_SPRITE:
                msg_print("You throw some magic dust...");
                if (plev < 25)
                    sleep_monsters_touch(plev);
                else
                    (void)sleep_monsters(plev);
            break;
        default:
				msg_print("You still don't know what that does.");
        energy_use = 0;
    }
            break;
		case POWER_MUTA1:
		switch (pw_ptr->power)
    {
			case iilog(MUT1_SPIT_ACID):
				msg_print("You spit acid...");
				if (get_aim_dir(&dir))
					fire_ball(GF_ACID, dir, plev, 1 + (plev/30));
            break;
			case iilog(MUT1_BR_FIRE):
				msg_print("You breathe fire...");
				if (get_aim_dir(&dir))
					fire_ball(GF_FIRE, dir, plev * 2, -(1 + (plev/20)));
            break;
			case iilog(MUT1_HYPN_GAZE):
				msg_print("Your eyes look mesmerizing...");
				if (get_aim_dir(&dir))
					(void) charm_monster(dir, plev);
            break;
			case iilog(MUT1_TELEKINES):
				msg_print("You concentrate...");
				if (get_aim_dir(&dir))
					fetch(dir, plev * 10, TRUE);
            break;
			case iilog(MUT1_VTELEPORT):
				msg_print("You concentrate...");
				teleport_player(10 + 4*(plev));
            break;
			case iilog(MUT1_MIND_BLST):
				msg_print("You concentrate...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_PSI, dir, damroll(3 + ((plev - 1) / 5), 3));
            break;
			case iilog(MUT1_RADIATION):
				msg_print("Radiation flows from your body!");
				fire_ball(GF_NUKE, 0, (plev * 2), 3 + (plev / 20));
            break;
			case iilog(MUT1_VAMPIRISM):
				if (!get_aim_dir(&dir)) return;
				if (drain_life(dir, (plev * 2)))
					hp_player(plev + randint(plev));
            break;
			case iilog(MUT1_SMELL_MET):
				(void)detect_treasure();
            break;
			case iilog(MUT1_SMELL_MON):
				(void)detect_monsters_normal();
            break;
			case iilog(MUT1_BLINK):
				teleport_player(10);
            break;
			case iilog(MUT1_EAT_ROCK):
			{
				int x,y;
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
				else if (((c_ptr->feat >= FEAT_PERM_BUILDING) &&
					(c_ptr->feat <= FEAT_PERM_SOLID)))
				{
					msg_print("Ouch!  This wall is harder than your teeth!");
            break;
		}
				else if (c_ptr->m_idx)
		{
					msg_print("There's something in the way!");
            break;
		}
				else if (c_ptr->feat == FEAT_TREE)
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
				move_to(y,x);
				break;
		}
			case iilog(MUT1_SWAP_POS):
				if (!get_aim_dir(&dir)) return;
				(void)teleport_swap(dir);
				break;
			case iilog(MUT1_SHRIEK):
				(void)fire_ball(GF_SOUND, 0, 4 * plev, 8);
				(void)aggravate_monsters(0);
			case iilog(MUT1_ILLUMINE):
				(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			case iilog(MUT1_DET_CURSE):
				for (i=0; i < INVEN_TOTAL; i++)
		{
					object_type *o_ptr = &inventory[i];
					
					if (!o_ptr->k_idx) continue;
					o_ptr->ident |= IDENT_SENSE_CURSED;
		}
			break;
			case iilog(MUT1_BERSERK):
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)hp_player(30);
				(void)set_afraid(0);
				break;
			case iilog(MUT1_POLYMORPH):
				do_poly_self();
				break;
			case iilog(MUT1_MIDAS_TCH):
				(void)alchemy();
				break;
			case iilog(MUT1_GROW_MOLD):
				for (i=0; i < 8; i++)
		{
					summon_specific_friendly(py, px, plev, SUMMON_MOULD, FALSE);
		}
				break;
			case iilog(MUT1_RESIST):
		{
				int num = plev/10;
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
				break;
    }
			case iilog(MUT1_EARTHQUAKE):
				/* Prevent destruction of quest levels and town */
				if (!is_quest(dun_level) && dun_level)
					earthquake(py, px, 10);
				break;
			case iilog(MUT1_EAT_MAGIC):
			{
				object_type * o_ptr;
				errr err;
				int lev;
				cptr q;

				item_tester_hook = item_tester_hook_recharge;

				/* Get an item */
				q = "Drain which item? ";
				if (!((o_ptr = get_item(&err, q, TRUE,TRUE,TRUE)))) break;

				lev = wand_power(&k_info[o_ptr->k_idx]);

				if (o_ptr->tval == TV_ROD)
        {
					if (o_ptr->timeout > 0)
		{
						msg_print("You can't absorb energy from a discharged rod.");
		}
					else
		{
						p_ptr->csp += 2*lev;
						o_ptr->timeout = 500;
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
				break;
                }
			case iilog(MUT1_WEIGH_MAG):
				report_magics();
				break;
			case iilog(MUT1_STERILITY):
				/* Fake a population explosion. */
				msg_print("You suddenly have a headache!");
				take_hit(randint(30) + 30, "the strain of forcing abstinence", MON_DANGEROUS_MUTATION);
				num_repro += MAX_REPRO;
				break;
			case iilog(MUT1_PANIC_HIT):
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
				break;
				}
			case iilog(MUT1_DAZZLE):
				stun_monsters(plev * 4);
				confuse_monsters(plev * 4);
				turn_monsters(plev * 4);
				break;
			case iilog(MUT1_EYE_BEAM):
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_LITE, dir, 2*plev);
				break;
			case iilog(MUT1_RECALL):
				set_recall(FALSE);
				break;
			case iilog(MUT1_BANISH):
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
					delete_monster_idx(c_ptr->m_idx,TRUE);
					msg_print("The evil creature vanishes in a puff of sulfurous smoke!");
				}
				else
				{
					msg_print("Your invocation is ineffectual!");
				}
				break;
			}
			case iilog(MUT1_COLD_TOUCH):
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
				fire_bolt(GF_COLD, dir, 2 * (plev));
						break;
					}
			case iilog(MUT1_LAUNCHER):
				/* Gives a multiplier of 2 at first, up to 5 at 48th */
				throw_mult = 2 + (plev)/16;
				do_cmd_throw();
				throw_mult = 1;
						break;
			default:
				msg_print("You don't know what that does.");
					}
						break;
		case POWER_MUTA2:
		case POWER_MUTA3:
		default:
		msg_print("You don't know what that does.");
					}
}

/*
 * Dismiss your allies.
 */
static void dismiss_pets(bool some)
{
	bool all_pets = FALSE;
	s16b i,Dismissed = 0;
	if (some && get_check("Dismiss all allies? ")) all_pets = TRUE;
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		monster_type *m_ptr = &m_list[i];
		if (m_ptr->smart & (SM_ALLY)) /* Get rid of it! */
		{
			bool delete_this = all_pets || get_check(
					format("Dismiss %v? ", monster_desc_f2, m_ptr, 0x80));

			if (delete_this)
			{
				delete_monster_idx(i,TRUE);
				Dismissed++;
			}
		}
	}
	msg_format("You have dismissed %d all%s.", Dismissed,
		(Dismissed==1?"y":"ies"));
}
					
/* 
 * Describe a power for do_cmd_racial_power.
 * In general, this becomes text+"     "+"(racial, cost power->cost, power->use_stat power->difficulty)".
 * If power->cost is negative, the cost is taken from powercosts[-power->cost].
 * If there is a ; in the string, the text immediately after it is added as a separate statement after
 * the cost, separated from that and power->use_stat by commas. This can contain semi-colons.
 * The number of spaces will be adjusted as necessary.
 * Display the powers available in choose_power()
 */
static void racial_string(byte num, byte *x, char * text)
					{
	powertype *pw_ptr = cur_powers[num];
						
	/* Set the distance from the left margin. */
	(*x) = RACIAL_MIN_X;

	sprintf(text, ")  %s", pw_ptr->text);
	switch (pw_ptr->type)
				{
		/* Racial and mutated powers have a fairly complex format. */
		case POWER_RACIAL: case POWER_MUTA1: case POWER_MUTA2: case POWER_MUTA3:
		{
			char stat[A_MAX][4] = {"STR","INT","WIS","DEX","CON","CHR"};
			char text2[80], cost[80];
			s16b num, denom;
			if (pw_ptr->cost > 0)
			{
				sprintf(cost, "%d", pw_ptr->cost);
				}
			else if (!pw_ptr->cost)
				{
				cost[0]='\0';
				}
			else
				{
				powercosttype *pc_ptr = &powercosts[-pw_ptr->cost];
				if (!(pc_ptr->levc))
					strcpy(cost, "odd");
				else
				{
					if (pc_ptr->minc)
						sprintf(cost, "%d+lev", pc_ptr->minc);
					else
						strcpy(cost, "lvl");
					if (pc_ptr->levc>1)
						strcat(cost, format("/%d", pc_ptr->levc));
				}
				}
			racial_success_chance(pw_ptr, &num, &denom);
			sprintf(text2, "%-*s(%scost %s, %s%s %d %d%%)", MIN_STRING_LEN, text,
			(pw_ptr->type == POWER_RACIAL) ? "racial, " : "", cost,
			(pw_ptr->text2) ? format("%s, ", pw_ptr->text2) : "",
			stat[pw_ptr->use_stat], pw_ptr->difficulty, 100-num*100/denom);
			strcpy(text, text2);
				break;
				}
		/* Dismissing uses a constant string, however. */
		case POWER_DISMISS:
				break;
		/* As do unknown strings. */
		default:
		strcpy(text, "Unknown power");
	}
}

/* 
 * The confirmation string for do_cmd_racial_power.
 */
static void racial_confirm_string(byte choice, char * out)
{
	/* Prompt */
	strnfmt(out, 78, "Use %s? ", cur_powers[choice]->text);
}

/*
 * Count the powers the player has.
 */
static s16b count_powers(void)
				{
	s16b i, pets, total;
					
	for (pets = 0, i = 1; (i < m_max) && (pets < 2); i++)
					{
		monster_type *m_ptr = &m_list[i];
		if (m_ptr->smart & SM_ALLY) pets++;
					}

	for (total = 0, i = 0; powers[i].type && (total < MAX_POWERS); i++)
					{
		bool available = FALSE;
		powertype *pw_ptr = &powers[i];
		switch (pw_ptr->type)
					{
			case POWER_RACIAL:
			if (pw_ptr->power%MAX_RACES == p_ptr->prace) available = TRUE;
			break;
			case POWER_MUTA1:
			if (p_ptr->muta1 & 1<<(pw_ptr->power%32)) available = TRUE;
			break;
			case POWER_MUTA2:
			if (p_ptr->muta2 & 1<<(pw_ptr->power%32)) available = TRUE;
			break;
			case POWER_MUTA3:
			if (p_ptr->muta3 & 1<<(pw_ptr->power%32)) available = TRUE;
			break;
			case POWER_DISMISS:
			switch (pets)
					{
				case 0:
				break;
				case 1:
				available = (pw_ptr->power == POWER_DISMISS_ONE);
				break;
				default:
				available = (pw_ptr->power == POWER_DISMISS_MANY);
					}
			break;
			default: /* How did we get here? */
			msg_format("Strange power %d,%d", pw_ptr->type, pw_ptr->power);
			msg_print(NULL);
					}
		if (available) cur_powers[total++] = pw_ptr;
				}
	return total;
				}

/*
 * A generic menu display function for lists of options such as a spell list. 
 * "display" must set the x co-ordinate for the display and return the string which is to be displayed
 * to describe option "num" when the full list is displayed.
 * "confirm" must display a prompt asking for confirmation of option "choice" and return the 
 * String1 and string2 are configurable strings in the initial prompt.
 * string contains the string displayed at the top of the screen when the command is called.
 * (*display)(byte num) is a pointer to the function which describes an option, taking its index.
 * number as a parameter and returning a string.
 * (*confirm)(byte num) is a pointer to the function which asks for confirmation of a selection, taking the index
 * of the option as a parameter. It should return the player's response.
 * The function returns the index of the option chosen, 255 if none are chosen.
 */
static byte display_list(void (*display)(byte, byte *, char *), void (*confirm)(byte, char *), byte num, cptr string1, cptr string2, cptr substring)
{
	bool ask, started = FALSE;
	char choice;
	int select, first = 0, last = 0;
	int page = (substring) ? 22 : 23;

	while (1)
	{
		/* Display the available choices */
		cptr prompt = format("(%s %c-%c, *=List %s, ESC=exit) %s ",
		string1, I2A(0), (num > 26) ? I2A(MIN(num-1-first, page-1)) :
		I2A(num - 1), (!last) ? "Open" : (last < num) ? "More" : "Close",
		string2);

		if (started || !show_choices_main)
		{
			(void)get_com(prompt, &choice);
		}
		else
		{
			/* Hack - pretend the player pressed space initially given show_choices_main. */
			choice = ' ';
			started = TRUE;
		}
		switch (choice)
		{
			/* Don't choose anything. */
			case ESCAPE:
			{
				if (last) Term_load();
				return 255;
			}
			/* Request redraw */
			case ' ': case '*': case '?':
			{
				/* Avoid leaving old text on the screen. */
				if (last) Term_load();

				if (last < num)
				{
					byte y, x, ctr;
					first = last;
					y = 24-page;
					if (substring) prt(substring, 1, x);

					if (!first) Term_save();
					for (ctr = 0; ctr < num-first && ctr < page; ctr++)
					{
						char text[160];
						(*display)(last+ctr, &x, text+1);
						/* Use a-z if possible, but use the currently displayed page otherwise. */
						if (num > 26)
							text[0] = I2A(ctr);
						else
							text[0] = I2A(ctr+last);
						prt(text, y+ctr, x);
					}
					prt ("", y+ctr, x);
					last += ctr;
				}
				else
				{
					first = last = 0;
				}
				break;
			}
			/* Choose any default option. */
			case '\r':
			{
				if (num != 1) break;
				choice = 'a';
			}
			/* Possible valid choices. */
			case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
			case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p':
			case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x':
			case 'y': case 'z': case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
			case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
			case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V':
			case 'W': case 'X': case 'Y': case 'Z': 
			{
				ask = isupper(choice);
				select = A2I(FORCELOWER(choice));
				/* If there are more than 26 entries, adjust for the current position of 'a'.*/
				if (num > 26) select += first;
				/* A plausible choice, */
				if (select < num)
				{
					if (ask) /* Need confirmation. */
					{
						char tmp_val[160];
						(*confirm)(select, tmp_val);
						if (!get_check(tmp_val)) select = 255;
					}
					if (last) Term_load();
					return select;
				}
				/* else fall through. */
			}
			/* Not a valid choice. */
			default:
			{
				bell();
			}
		}
	}
}
				
/*
 * Determine the chance of using a given power based on features of the power and the player.
 * The power should succeed if rand_int((*denom))<(*num).
 */
static void racial_success_chance(powertype *pw_ptr, s16b *num, s16b *denom)
{
	int difficulty = pw_ptr->difficulty;
	
				
	const byte plev = MIN(1,skill_set[SKILL_RACIAL].value/2);
	const byte stat = (byte)p_ptr->stat_cur[pw_ptr->use_stat];

	/* Should never happen, but... */
	if (difficulty > 100)
	{
		difficulty = 100;
	}

	/* Adjust by racial skill */
	if (plev > pw_ptr->min_level + 30) difficulty -= 10;
	else difficulty += (pw_ptr->min_level-plev)/3;

	/* Put an upper limit on difficulty. */
	if (difficulty > 100) difficulty = 100;

	/* Stunning is excluded from the upper limit calculation, but included
	 * in the lower limit one. */
	if (p_ptr->stun) difficulty += p_ptr->stun;

	/* Put a lower limit on difficulty. */
	if (difficulty < 4) difficulty = 4;

	/* Halve it to approximate a range from difficulty/2 to difficulty below. */
	difficulty /= 2;

	/* Calculate the numerator. */
	if (p_ptr->confused)
		(*num) = 0;
	else if (stat > difficulty * 2)
		(*num) = difficulty*(2*stat-3*difficulty+1)/2;
	else if (stat > difficulty)
		(*num) = (stat-difficulty)*(stat-difficulty+1)/2;
	else
		(*num = 0);

	/* Calculate the denominator */
	(*denom) = stat*difficulty;
}

/*
 * Decide whether a power has been used successfully.
 */
static bool racial_aux(powertype *pw_ptr)
{
	bool plev = MIN(1,skill_set[SKILL_RACIAL].value/2);
	bool use_hp = FALSE;
	bool use_chi = FALSE;
	bool use_mana = TRUE;
	s16b num, denom;
	int cost = pw_ptr->cost;
	
	/* Determine the cost if level-based. */
	if (cost < 0)
	{
		powercosttype *pc_ptr = powercosts-cost;
		cost = pc_ptr->minc + plev / pc_ptr->levc;
	}

	if (p_ptr->cchi >= p_ptr->csp)
	{
		use_mana = FALSE;
		use_chi = TRUE;
		if (p_ptr->cchi < cost)
		{
			use_chi = FALSE;
			use_hp = TRUE;
		}
	}
	else
	{
		if (p_ptr->csp < cost)
		{
			use_mana = FALSE;
			use_hp = TRUE;
		}
	}


	if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		return FALSE;
	}

	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!(get_check("Really use the power in your weakened state? ")))
		{
			return FALSE;
		}
	}
            
	/* Else attempt to do it! */

	/* take time and pay the price */
	energy_use = spell_energy(plev,pw_ptr->min_level);

	/* Hack - use num to store the actual cost of the attempt. */
	num = rand_range(cost/2+1, cost/2*2);

	if (use_hp)
	{
		take_hit (num, "concentrating too hard", MON_CONCENTRATING_TOO_HARD);
		p_ptr->redraw |= PR_HP;
	}
	else if (use_mana)
	{
		p_ptr->csp -= num;
		p_ptr->redraw |= PR_MANA;
	}
	else
	{
		p_ptr->cchi -= num;
		p_ptr->redraw |= PR_MANA;
	}
					
	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

					
	/* Success? */
	racial_success_chance(pw_ptr, &num, &denom);
	if (rand_int(denom) < num)
	{
		skill_exp(SKILL_RACIAL);
		return TRUE;
	}
	else
	{
		msg_print("You've failed to concentrate hard enough.");
	}
	return FALSE;
}
				
/*
 * Use a racial/mutated power.
 */
void do_cmd_racial_power(void)
{
	/* Count the available powers. */
	byte total = count_powers();

	/* Assume free for now. Will be changed if the player attempts to use a power. */
	energy_use = 0;

	if (!total)
	{
		msg_print("You have no abilities to use.");
		return;
	}

	/* Display the available powers */
	total = display_list(racial_string, racial_confirm_string, total, "Powers", "Use which ability?", 0);

	/* Something has been selected */
	if (total != 255)
	{
		powertype *pw_ptr = cur_powers[total];
				
		switch (pw_ptr->type)
		{
			case POWER_RACIAL: case POWER_MUTA1: case POWER_MUTA2: case POWER_MUTA3:
			/* Try to use the power. */
			if (racial_aux(pw_ptr))
				use_power(pw_ptr);
			break;
			case POWER_DISMISS:
		 	dismiss_pets(pw_ptr->power != POWER_DISMISS_ONE);
			break;
            default:
			msg_print("You don't know what that does.");
        }
    }
}
