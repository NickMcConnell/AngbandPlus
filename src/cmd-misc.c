/* File: cmd-misc.c */

/*
 * Commands that didn't fit anywhere else.
 *
 * Copyright (c) 1999 Leon Marrick, Ben Harrison, James E. Wilson, 
 * Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set_interactive(TARGET_KILL, 0, 0, 0))
	{
		message(MSG_GENERIC, 0, "Target Selected.");
	}

	/* Target aborted */
	else
	{
		message(MSG_GENERIC, 0, "Target Aborted.");
	}
}

/*
 * Look command
 */
void do_cmd_look(void)
{
	/* Look around */
	if (target_set_interactive(TARGET_LOOK, 0, 0, 0))
	{
		message(MSG_GENERIC, 0, "Target Selected.");
	}
}

/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
	int dir, y1, x1, y2, x2;

	char tmp_val[80];

	char out_val[160];

	/* Start at current panel */
	y2 = y1 = p_ptr->wy;
	x2 = x1 = p_ptr->wx;

	/* Show panels until done */
	while (TRUE)
	{
		/* Describe the location */
		if ((y2 == y1) && (x2 == x1))
		{
			tmp_val[0] = '\0';
		}
		else
		{
			strnfmt(tmp_val, sizeof(tmp_val), "%s%s of",
			        ((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
			        ((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
		}

		/* Prepare to ask which way to look */
		strnfmt(out_val, sizeof(out_val), 
		        "Map sector [%d,%d], which is%s your sector.  Direction?",
		        (y2 / PANEL_HGT), (x2 / PANEL_WID), tmp_val);

		/* More detail */
		if (center_player)
		{
			strnfmt(out_val, sizeof(out_val),
		        	"Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
		        	(y2 / PANEL_HGT), (y2 % PANEL_HGT),
		        	(x2 / PANEL_WID), (x2 % PANEL_WID), tmp_val);
		}

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command)) break;

			/* Extract direction */
			dir = target_dir(command);

			/* Error */
			if (!dir) bell("Illegal direction for locate!");
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		y2 += (ddy[dir] * PANEL_HGT);
		x2 += (ddx[dir] * PANEL_WID);

		/* Verify the row */
		if (y2 > p_ptr->cur_map_hgt - SCREEN_HGT) y2 = p_ptr->cur_map_hgt - SCREEN_HGT;
		if (y2 < 0) y2 = 0;

		/* Verify the col */
		if (x2 > p_ptr->cur_map_wid - SCREEN_WID) x2 = p_ptr->cur_map_wid - SCREEN_WID;
		if (x2 < 0) x2 = 0;

		/* Handle "changes" */
		if ((p_ptr->wy != y2) || (p_ptr->wx != x2))
		{
			/* Update panel */
			p_ptr->wy = y2;
			p_ptr->wx = x2;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Handle stuff */
			handle_stuff();
		}
	}

	/* Verify panel */
	p_ptr->update |= (PU_PANEL);

	/* Handle stuff */
	handle_stuff();
}

/*
 * Return TRUE if the given feature is an open (or broken) door
 */
static bool is_open(int feat)
 {
	return ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN));
}

/*
 * Return TRUE if the given feature is a closed door or chest
 */
static bool is_closed(int feat)
{
	return ((feat == FEAT_CLOSED) || (feat == FEAT_CHEST) || (feat == FEAT_QST_CHEST));
}

/*
 * Return the number of doors around (or under) the character.
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a closed door */
	if ((cave_feat[y][x] != FEAT_CLOSED) &&
	    (cave_feat[y][x] != FEAT_CHEST) &&
		(cave_feat[y][x] != FEAT_QST_CHEST))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to open.");

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

	/* Locked door */
	if (trap_lock(y, x))
	{
		trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

		/* Disarm factor */
		i = p_ptr->skill[SK_PER];

		/* Penalize some conditions */
		if (p_ptr->blind || !player_can_see_bold(p_ptr->py, p_ptr->px)) i = i / 10;
		if (p_ptr->confused || p_ptr->image) i = i / 10;

		/* Already mastered */
		if (t_ptr->spot_factor == 99)
		{
		}
		/* Only one try */
		else if (t_ptr->spot_factor == 100)
		{
			message(MSG_DISARM_FAIL, 0, "You have no idea how to open the lock.");
			return (FALSE);
		}

		/* Success */
		else if (rand_int(100) < i)
		{
			/* Message */
			message(MSG_OPENDOOR, 0, "Using Perception...");
			message(MSG_OPENDOOR, 0, "You have mastered the lock.");

			t_ptr->spot_factor = 99;

			/* Delete the lock
			delete_trap(y, x); */

			/* Experience */
			gain_exp(p_ptr->depth * 1.5);
		}

		/* Failure */
		else
		{
			/* Failure */
			if (flush_failure) flush();

			/* Message */
			message(MSG_OPENDOOR, 0, "Using Perception...");
			message(MSG_DISARM_FAIL, 0, "You failed to pick the lock.");

			t_ptr->spot_factor = 100;

			/* Can keep trying */
			return (FALSE);
		}
	}

	/* Open the door */
	if (cave_feat[y][x] == FEAT_CLOSED)
	{
		if (t_list[cave_t_idx[y][x]].w_idx == WG_SHELF_CLOSED_DOOR)
		{
			place_decoration(y, x, WG_SHELF_OPEN_DOOR);
			lite_spot(y,x);
		}
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_PAINTING_CLOSED_DOOR)
		{
			place_decoration(y, x, WG_PAINTING_OPEN_DOOR);
			lite_spot(y,x);
		}
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_RACK_CLOSED_DOOR)
		{
			place_decoration(y, x, WG_RACK_OPEN_DOOR);
			lite_spot(y,x);
		}
		else if (t_list[cave_t_idx[y][x]].w_idx == WG_CLOSET_CLOSED_DOOR)
		{
			place_decoration(y, x, WG_CLOSET_OPEN_DOOR);
			lite_spot(y,x);
		}
		else
		{
			cave_set_feat(y, x, FEAT_OPEN);
		}
	}

	/* Open a chest */
	else
	{
		/* Trap */
		if (cave_t_idx[y][x])
		{
			/* Message */
			message(MSG_TRAP, 0, "You set off a trap!");

			t_list[cave_t_idx[y][x]].visible = TRUE;

			hit_trap(y, x);
		}
		/* No trap - quest chest */
		else if (cave_feat[y][x] == FEAT_QST_CHEST)
		{
			/* Delete Chest */
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Make the quest item */
			create_quest_item(y, x);
		}
		/* Normal chest */
		else 
		{
			object_type *i_ptr;
			object_type object_type_body;

			byte tval = 0;
			int theme;
			bool placed = FALSE;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Delete Chest */
			cave_set_feat(y, x, FEAT_FLOOR);

			/* 
			 * Hack - code for making themed chest drops 
			 * This can probably be improved considerably
			 */

			/* Pick a theme */
			theme = rand_int(3);

			/* How many items to generate? */
			j = 3 + rand_int(3);
			
			/* Attempt to place some objects */
			for (i = 0; i < j; i++)
			{
				/* Wipe the object */
				object_wipe(i_ptr);

				/* Get the actual item from the theme */
				switch (theme)
				{
					/* Warrior theme */
					case 0:
					{
						switch(rand_int(22))
						{
							case 0: case 1: tval = TV_SWORD; break;
							case 2: case 3: tval = TV_POLEARM; break;
							case 4: case 5: tval = TV_BLUNT; break;
							case 6: case 7: tval = TV_BODY_ARMOR; break;
							case 8: case 9: tval = TV_BOW; break;	
							case 10: case 11: tval = TV_BOOTS; break;
							case 12: case 13: tval = TV_GLOVES; break;
							case 14: case 15: tval = TV_HEADGEAR; break;
							case 16: case 17: tval = TV_SHIELD; break;
							case 18: case 19: tval = TV_CLOAK; break;	
							case 20: tval = TV_DRAG_ARMOR; break;
							case 21: tval = TV_ARROW; break;
						}
						break;
					}
					/* Spellcaster theme */
					case 1:
					{
						switch(rand_int(16))
						{
							case 0: case 1: tval = TV_SCROLL; break;
							case 2: case 3: tval = TV_POTION; break;
							case 4: case 5: tval = TV_WAND; break;
							case 6: case 7: tval = TV_STAFF; break;
							case 8: case 9: tval = TV_ROD; break;	
							case 10: case 11: tval = TV_TALISMAN; break;
							case 12: tval = TV_MAGIC_BOOK; break;
							case 13: tval = TV_AMULET; break;
							case 14: tval = TV_POWDER; break;
							case 15: tval = TV_RING; break;	
						}
						break;
					}
					/* Misc theme */
					case 2:
					{
						switch(rand_int(14))
						{
							case 0: case 1: tval = TV_SCROLL; break;
							case 2: case 3: tval = TV_POTION; break;
							case 4: case 5: tval = TV_LITE; break;
							case 6: case 7: tval = TV_AMULET; break;
							case 8: case 9: tval = TV_RING; break;	
							case 10: tval = TV_MUSIC; break;
							case 11: tval = TV_CLOAK; break;
							case 12: tval = TV_POWDER; break;	
							case 13: tval = TV_ARROW; break;
						}
						break;
					}
				}

				/* Increase item depth */
				object_level = p_ptr->depth + 10;

				/* Make a themed object (if possible) */
				if (make_typed(i_ptr, tval, TRUE, FALSE, TRUE))
				{
					/* Mark history */
					object_history(i_ptr, ORIGIN_CHEST, 0, 0, 0);

					/* Drop the object */
					drop_near(i_ptr, -1, y, x, FALSE);

					placed = TRUE;
				}

				/* Restore item depth */
				object_level = p_ptr->depth;
			}				
		}
	}

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Sound */
	sound(MSG_OPENDOOR);

	/* Result */
	return (more);
}

/*
 * Open a closed/locked/jammed door 
 *
 * Unlocking a locked door is worth one experience point.
 */
void do_cmd_open(void)
{
	int y, x, dir;

	bool more = FALSE;

	/* Easy Open */
	if (easy_direction)
	{
		int num_doors;

		/* Count closed doors */
		num_doors = count_feats(&y, &x, is_closed, FALSE);

		/* See if only one target */
		if (num_doors == 1)
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
	if (!do_cmd_open_test(y, x)) return;

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
		message(MSG_FAIL, 0, "There is a monster in the way!");

		/* Attack */
		py_attack(y, x, FALSE);
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

 	/* Require open/broken door */
	if ((cave_feat[y][x] != FEAT_OPEN) &&
	    (cave_feat[y][x] != FEAT_BROKEN))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to close.");

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
		message(MSG_FAIL, 0, "The door appears to be broken.");
	}

	/* Close door */
	else
	{
		/* Close the door */

		if (t_list[cave_t_idx[y][x]].w_idx == WG_SHELF_OPEN_DOOR)
		{
			place_decoration(y, x, WG_SHELF_CLOSED_DOOR);
			lite_spot(y,x);
		}

		else if (t_list[cave_t_idx[y][x]].w_idx == WG_PAINTING_OPEN_DOOR)
		{
			place_decoration(y, x, WG_PAINTING_CLOSED_DOOR);
			lite_spot(y,x);
		}

		else if (t_list[cave_t_idx[y][x]].w_idx == WG_CLOSET_OPEN_DOOR)
		{
			place_decoration(y, x, WG_CLOSET_CLOSED_DOOR);
			lite_spot(y,x);
		}

		else if (t_list[cave_t_idx[y][x]].w_idx == WG_RACK_OPEN_DOOR)
		{
			place_decoration(y, x, WG_RACK_CLOSED_DOOR);
			lite_spot(y,x);
		}

		else cave_set_feat(y, x, FEAT_CLOSED);

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
	if (easy_direction)
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
		message(MSG_FAIL, 0, "There is a monster in the way!");

		/* Attack */
		py_attack(y, x, FALSE);
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
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
	/* Must have knowledge */
	if (!(cave_info[y][x] & (CAVE_MARK)))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Hack - Can only tunnel in rubble */
	if (!(cave_feat[y][x] == FEAT_RUBBLE))
	{
		/* Message */
		message(MSG_FAIL, 0, "You can only tunnel through rubble or trees.");

		/* Nope */
		return (FALSE);
	}

	/* Hack - Can't tunnel into a chest */
	if ((cave_feat[y][x] == FEAT_CHEST) || (cave_feat[y][x] == FEAT_QST_CHEST))
	{
		/* Message */
		message(MSG_FAIL, 0, "You can't tunnel into a chest.");

		/* Nope */
		return (FALSE);
	}

	/* Must be a wall/door/etc */
	if (cave_floor_bold(y, x))
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to tunnel.");

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
static bool twall(int y, int x)
{
	/* Paranoia -- Require a wall or door or some such */
	if (cave_floor_bold(y, x)) return (FALSE);

	/* Sound */
	sound(MSG_DIG);

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* If there's a lock, delete it */
	if (trap_lock(y, x)) delete_trap(y, x);

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

	/* Verify legality */
	if (!do_cmd_tunnel_test(y, x)) return (FALSE);

	/* Titanium */
	if (cave_feat[y][x] >= FEAT_PERM_EXTRA)
	{
		message(MSG_FAIL, 0, "This seems to be permanent rock.");
	}

	/* Granite */
	else if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
	{
		/* Tunnel */
		if ((p_ptr->digging > 40 + rand_int(1600)) && twall(y, x))
		{
			message(MSG_DIG, 0, "You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			message(MSG_DIG, 0, "You tunnel into the granite wall.");
			more = TRUE;
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

		/* Quartz */
		if (hard)
		{
			okay = (p_ptr->digging > 20 + rand_int(800));
		}

		/* Magma */
		else
		{
			okay = (p_ptr->digging > 10 + rand_int(400));
		}

		/* Success */
		if (okay && twall(y, x))
		{
			/* Found treasure */
			if (gold)
			{
				/* Augment value */
				object_level = p_ptr->depth + 5;

				/* Place some gold */
				place_gold(y, x);

				object_level = p_ptr->depth;

				/* Message */
				message(MSG_DIG, 0, "You have found something!");
			}

			/* Found nothing */
			else
			{
				/* Message */
				message(MSG_DIG, 0, "You have finished the tunnel.");
			}
		}

		/* Failure (quartz) */
		else if (hard)
		{
			/* Message, continue digging */
			message(MSG_DIG, 0, "You tunnel into the quartz vein.");
			more = TRUE;
		}

		/* Failure (magma) */
		else
		{
			/* Message, continue digging */
			message(MSG_DIG, 0, "You tunnel into the magma vein.");
			more = TRUE;
		}
	}

	/* Rubble */
	else if (cave_feat[y][x] == FEAT_RUBBLE)
	{
		/* Remove the rubble */
		if ((p_ptr->digging > rand_int(200)) && twall(y, x))
		{
			/* Message */
			
			if decoration(y, x)
			{
				message(MSG_DIG, 0, "Timber!");
			}
			else
			{
				message(MSG_DIG, 0, "You have removed the rubble.");
			}

			/* Hack -- place an object */
			if ((rand_int(100) < 10) && (!(decoration(y,x))))
			{
				/* Create a simple object */
				place_object(y, x, FALSE, FALSE);

				/* Observe new object */
				if (player_can_see_bold(y, x))
				{
					message(MSG_DIG, 0, "You have found something!");
				}
			}

			if decoration(y, x)
			{
				delete_trap(y , x);

				trap_type trap_type_body;
				trap_type *t_ptr = &trap_type_body;

				t_ptr->visible = TRUE;
				t_ptr->charges = 0;
				t_ptr->w_idx = WG_STUMP;
				trap_place(y, x, t_ptr);

				lite_spot(y,x);
			}

		}

		else
		{
			/* Message, keep digging */
			if decoration(y, x)
			{
				message(MSG_DIG, 0, "You chop the tree.");
			}
			else
			{
				message(MSG_DIG, 0, "You dig in the rubble.");
			}

			more = TRUE;
		}
	}

	/* Secret doors */
	else if (cave_feat[y][x] == FEAT_SECRET)
	{
		/* Tunnel */
		if ((p_ptr->digging > 30 + rand_int(1200)) && twall(y, x))
		{
			message(MSG_DIG, 0, "You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			message(MSG_DIG, 0, "You tunnel into the granite wall.");
			more = TRUE;

			/* Hack - Occasional chance of finding the door */
			if (rand_int(100) < 25)
			{
				if (rand_int(100) < (p_ptr->skill[SK_PER]))
				{
					/* Message */
					message(MSG_FIND, 0, "You have found a secret door.");

					/* Create closed door */
					cave_set_feat(y, x, FEAT_CLOSED);

					if (trap_lock(y, x)) t_list[cave_t_idx[y][x]].visible = TRUE;

					/* Stop tunneling */
					more = FALSE;
				}
			}
		}
	}

	/* Doors */
	else
	{
		/* Tunnel */
		if ((p_ptr->digging > 30 + rand_int(1200)) && twall(y, x))
		{
			message(MSG_DIG, 0, "You have finished the tunnel.");
		}

		/* Keep trying */
		else
		{
			/* We may continue tunelling */
			message(MSG_DIG, 0, "You tunnel into the door.");
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
	/* if (cave_m_idx[y][x] > 0)
	{
		message(MSG_FAIL, 0, "There is a monster in the way!");

		py_attack(y, x, FALSE);
	} */

	/* Tunnel through walls */
	more = do_cmd_tunnel_aux(y, x);

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0);
}

/*
 * Determine if a given grid may be "disarmed"
 */
static bool do_cmd_disarm_test(int y, int x)
{
	/* Paranoia */
	if (!in_bounds(y, x)) return (FALSE);

	/* No visible trap */
	if (!trap_disarmable(y, x))
	{
		message(MSG_FAIL, 0, "There's nothing there to disarm!");
		return (FALSE);
	}

	/* Legal */
	return (TRUE);
}

/*
 * Return the number of doors around (or under) the character.
 */
static int count_traps(int *y, int *x)
{
	int d;
	int xx, yy;
	int count = 0; /* Count how many matches */
 
 	/* Check around (and under) the character */
 	for (d = 0; d < 9; d++)
 	{
 		/* Extract adjacent (legal) location */
		yy = p_ptr->py + ddy_ddd[d];
		xx = p_ptr->px + ddx_ddd[d];

		/* Paranoia */
		if (!in_bounds_fully(yy, xx)) continue;
 
 		/* Must have knowledge */
 		if (!(cave_info[yy][xx] & (CAVE_MARK))) continue;
 
 		/* Not a disarmable trap */
		if (!trap_disarmable(yy, xx)) continue;
 
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
 * Disarms a trap
 */
void do_cmd_disarm(void)
{
	int y = 0, x = 0, dir;

	bool more = FALSE;

	/* Easy Disarm */
	if (easy_direction)
	{
		int num_traps;

		/* Count closed doors */
		num_traps = count_traps(&y, &x);

		/* See if only one target */
		if (num_traps == 1)
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
	if (!do_cmd_disarm_test(y, x)) return;

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
		message(MSG_FAIL, 0, "There is a monster in the way!");

		/* Attack */
		py_attack(y, x, FALSE);
	}

	/* Disarm trap */
	else
	{
		/* Disarm the trap */
		more = do_disarm_trap(y, x);
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
		message(MSG_FAIL, 0, "You see nothing there.");

		/* Nope */
		return (FALSE);
	}

	/* Require a door */
	if (cave_feat[y][x] != FEAT_CLOSED) 
	{
		/* Message */
		message(MSG_FAIL, 0, "You see nothing there to bash.");

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
	message(MSG_BASH, 0, "You smash into the door!");

	/* Hack -- Bash power based on strength */
	/* (Ranges from 3 to 20 to 100 to 200) */
	bash = adj_str_blow[p_stat(A_STR)];

	/* Extract door power !!!!! */
	temp = 1;

	/* Compare bash power to door power XXX XXX XXX */
	temp = (bash - (temp * 10));

	/* Hack -- always have a chance */
	if (temp < 1) temp = 1;

	/* Hack -- attempt to bash down the door */
	if (rand_int(100) < temp)
	{
		/* Break down the door */
		if (rand_int(100) < 50)
		{
			cave_set_feat(y, x, FEAT_FLOOR);
		}

		/* Open the door */
		else
		{
			cave_set_feat(y, x, FEAT_OPEN);
		}

		/* Remove the lock */
		delete_trap(y, x);

		/* Message */
		message(MSG_OPENDOOR, 0, "The door crashes open!");

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Saving throw against stun */
	else if (rand_int(100) < adj_dex_safe[p_stat(A_DEX)] + p_ptr->lev)
	{
		/* Message */
		message(MSG_FAIL, 0, "The door holds firm.");

		/* Allow repeated bashing */
		more = TRUE;
	}

	/* High dexterity yields coolness */
	else
	{
		/* Message */
		message(MSG_EFFECT, 0, "You are off-balance.");

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
		message(MSG_FAIL, 0, "There is a monster in the way!");

		/* Attack */
		py_attack(y, x, FALSE);
	}

	/* Door */
	else
	{
		/* Bash the door */
		if (!do_cmd_bash_aux(y, x))
		{
			/* Cancel repeat */
			disturb(0);
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

	/* Tunnel through walls */
	if ((feat >= FEAT_SECRET) && (feat <= FEAT_PERM_SOLID))
	{
		/* Tunnel */
		more = do_cmd_tunnel_aux(y, x);
	}

	/* Attack monsters */
	else if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x, FALSE);
	}

	/* Open closed doors */
	else if (feat == FEAT_CLOSED)
	{
		/* Tunnel */
		more = do_cmd_open_aux(y, x);
	}

	/* Disarm traps */
	else if (trap_disarmable(y, x))
	{
		more = do_disarm_trap(y, x);
	}

	/* Oops */
	else
	{
		/* Oops */
		message(MSG_FAIL, 0, "You spin around.");
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0);
}

/*
 * Use a racial ability
 */
void do_cmd_racial(void)
{
	if ((!rp_ptr->special) || (!rsp_ptr[p_ptr->max_lev / 5]->activation))
	{
		message(MSG_FAIL, 0, "You have no racial powers!");
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
		message(MSG_FAIL, 0, "You are too confused!");
		return;
	}

	/* Not if still recharging */
	if (p_ptr->racial_power)
	{
		message(MSG_FAIL, 0, "You have to collect your energy.");
		return;
	}

	/* Hack - angels can't use their powers while tainted */
	if (!p_ptr->taint || (rp_ptr->special != RACE_SPECIAL_ANGEL))
	{
		bool ignore_me;

		do_power(rsp_ptr[p_ptr->max_lev / 5]->activation, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);

		/* Delay */
		p_ptr->racial_power = rsp_ptr[p_ptr->max_lev / 5]->turns;
	}
	else
	{
		message(MSG_FAIL, 0, "The taint on your soul prevents you from using your abilities!");
	}

	/* Take a turn */
	p_ptr->energy_use = 100;
}

/*
 * Display a "small-scale" map of the dungeon.
 *
 * Note that the "player" is always displayed on the map.
 */
void do_cmd_view_map(void)
{
	int cy, cx;

	cptr prompt = "Hit any key to continue";
	
	/* Save screen */
	screen_save();

	/* Note */
	prt("Please wait...", 0, 0);

	/* Flush */
	Term_fresh();

	/* Clear the screen */
	Term_clear();

	/* Display the map */
	display_map(&cy, &cx);

	/* Show the prompt */
	put_str(prompt, Term->hgt - 1, Term->wid / 2 - strlen(prompt) / 2);

	/* Hilite the player */
	Term_gotoxy(cx, cy);

	/* Get any key */
	(void)inkey();

	/* Load screen */
	screen_load();
}

/*
 * Use proficiency
 */
void do_cmd_proficiency(void)
{
	int lore, reserves, escapes, superlore;
	bool ignore_me;
	int use_charge = 0;
	int temp_lore_bonus = 0;
	bool berserk = 0;

	/* Temp Lore bonus when near a bookshelf & inside a room */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
	}

	/* Temp Lore bonus when on a Circle of Knowledge */
	if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_KNOWLEDGE) temp_lore_bonus += 7;

	/* Does any goddess grant Berserk power? */
	if (p_ptr->obsession_status >= 2)
	{
		if (p_ptr->obsession_bonus_a == DEITY_BERSERK) berserk = TRUE;
		if (p_ptr->obsession_bonus_b == DEITY_BERSERK) berserk = TRUE;
	}
	if (p_ptr->conflict_status >= 2)
	{
		if (p_ptr->conflict_bonus_a == DEITY_BERSERK) berserk = TRUE;
		if (p_ptr->conflict_bonus_b == DEITY_BERSERK) berserk = TRUE;
	}
	if (p_ptr->purity_status >= 2)
	{
		if (p_ptr->purity_bonus_a == DEITY_BERSERK) berserk = TRUE;
		if (p_ptr->purity_bonus_b == DEITY_BERSERK) berserk = TRUE;
	}
	if (p_ptr->transformation_status >= 2)
	{
		if (p_ptr->transformation_bonus_a == DEITY_BERSERK) berserk = TRUE;
		if (p_ptr->transformation_bonus_b == DEITY_BERSERK) berserk = TRUE;
	}
	if (p_ptr->deceit_status >= 2)
	{
		if (p_ptr->deceit_bonus_a == DEITY_BERSERK) berserk = TRUE;
		if (p_ptr->deceit_bonus_b == DEITY_BERSERK) berserk = TRUE;
	}

	lore = p_ptr->lore - p_ptr->lore_uses;
	reserves = p_ptr->reserves - p_ptr->reserves_uses;
	escapes = p_ptr->escapes - p_ptr->escapes_uses;
	superlore = 0;
	if (p_stat(A_INT) + p_stat(A_WIS) + temp_lore_bonus >= 30) superlore = 1;

	/* Templars have different Lore proficiencies */
	if (cp_ptr->flags & CF_FENCING)
	{
		if ((lore > 0) && (superlore))
		{
			if ((reserves > 0) && (berserk))
			{
				if (escapes > 0) prt ("Pick: (a)rchery, (f)encing, (i)dentify pack, (s)tudy item, (b)erserk, (e)scape?", 0 , 0);
				else prt ("Which one: (a)rchery, (f)encing, (i)dentify pack, (s)tudy item, (b)erserk?", 0 , 0);
			}
			else if (reserves > 0)
			{
				if (escapes > 0) prt ("Pick: (a)rchery, (f)encing, (i)dentify pack, (s)tudy item, (r)ecover, (e)scape?", 0 , 0);
				else prt ("Which one: (a)rchery, (f)encing, (i)dentify pack, (s)tudy item, (r)ecover?", 0 , 0);
			}
			else if (escapes > 0) prt ("Which one: (a)rchery, (f)encing, (i)dentify pack, (s)tudy item, (e)scape?", 0, 0);
			else prt ("Use which proficiency: (a)rchery, (f)encing, (i)dentify pack, (s)tudy item?", 0, 0);
		}
		else if (lore > 0)
		{
			if ((reserves > 0) && (berserk))
			{
				if (escapes > 0) prt ("Use which proficiency: (f)encing, (i)dentify, (b)erserk, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (f)encing, (i)dentify, (b)erserk?", 0 , 0);
			}
			else if (reserves > 0)
			{
				if (escapes > 0) prt ("Use which proficiency: (f)encing, (i)dentify, (r)ecover, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (f)encing, (i)dentify, (r)ecover?", 0 , 0);
			}
			else if (escapes > 0) prt ("Use which proficiency: (f)encing, (i)dentify, (e)scape?", 0, 0);
			else prt ("Use which proficiency: (f)encing, (i)dentify?", 0, 0);
		}
		else
		{
			if ((reserves > 0) && (berserk))
			{
				if (escapes > 0) prt ("Use which proficiency: (b)erserk, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (b)erserk?", 0 , 0);
			}
			else if (reserves > 0)
			{
				if (escapes > 0) prt ("Use which proficiency: (r)ecover, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (r)ecover?", 0 , 0);
			}
			else if (escapes > 0) prt ("Use which proficiency: (e)scape?", 0, 0);
			else
			{
				prt ("You can't use any proficiencies right now.", 0, 0);
				return;
			}
		}


	}

	/* Non-Templars */
	else
	{
		if ((lore > 0) && (superlore))
		{
			if ((reserves > 0) && (berserk))
			{
				if (escapes > 0) prt ("Which one: (a)lertness, (i)dentify pack, (s)tudy item, (b)erserk, (e)scape?", 0 , 0);
				else prt ("Which proficiency: (a)lertness, (i)dentify pack, (s)tudy item, (b)erserk?", 0 , 0);
			}
			else if (reserves > 0)
			{
				if (escapes > 0) prt ("Which one: (a)lertness, (i)dentify pack, (s)tudy item, (r)ecover, (e)scape?", 0 , 0);
				else prt ("Which proficiency: (a)lertness, (i)dentify pack, (s)tudy item, (r)ecover?", 0 , 0);
			}
			else if (escapes > 0) prt ("Which proficiency: (a)lertness, (i)dentify pack, (s)tudy item, (e)scape?", 0, 0);
			else prt ("Use which proficiency: (a)lertness, (i)dentify pack, (s)tudy item?", 0, 0);
		}
		else if (lore > 0)
		{
			if ((reserves > 0) && (berserk))
			{
				if (escapes > 0) prt ("Use which proficiency: (a)lertness, (i)dentify, (b)erserk, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (a)lertness, (i)dentify, (b)erserk?", 0 , 0);
			}
			else if (reserves > 0)
			{
				if (escapes > 0) prt ("Use which proficiency: (a)lertness, (i)dentify, (r)ecover, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (a)lertness, (i)dentify, (r)ecover?", 0 , 0);
			}
			else if (escapes > 0) prt ("Use which proficiency: (a)lertness, (i)dentify, (e)scape?", 0, 0);
			else prt ("Use which proficiency: (a)lertness, (i)dentify?", 0, 0);
		}
		else
		{
			if ((reserves > 0) && (berserk))
			{
				if (escapes > 0) prt ("Use which proficiency: (b)erserk, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (b)erserk?", 0 , 0);
			}
			else if (reserves > 0)
			{
				if (escapes > 0) prt ("Use which proficiency: (r)ecover, (e)scape?", 0 , 0);
				else prt ("Use which proficiency: (r)ecover?", 0 , 0);
			}
			else if (escapes > 0) prt ("Use which proficiency: (e)scape?", 0, 0);
			else
			{
				prt ("You can't use any proficiencies right now.", 0, 0);
				return;
			}
		}
	}

       	flush();
       	int ch;
       	ch = inkey();
      	prt("", 0, 0);

	/* Analyse the answer */

	/* Archery */
	if ((ch == 'a') && (lore > 0) && (superlore) && (cp_ptr->flags & CF_FENCING))
	{
		use_charge = do_power(POW_ARCHERY, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->lore_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Alertness */
	else if ((ch == 'a') && (lore > 0))
	{
		use_charge = do_power(POW_ALERTNESS, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->lore_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Fencing */
	if ((ch == 'f') && (lore > 0) && (cp_ptr->flags & CF_FENCING))
	{
		use_charge = do_power(POW_FENCING, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->lore_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Identify Pack */
	else if ((ch == 'i') && (lore > 0) && (superlore))
	{
		/* Check some conditions */
		if (p_ptr->blind)
		{
			message(MSG_FAIL, 0, "You can't see anything.");
			return;
		}
		if (!player_can_see_bold(p_ptr->py, p_ptr->px))
		{
			message(MSG_FAIL, 0, "You need some light.");
			return;
		}
		if (p_ptr->confused)
		{
			message(MSG_FAIL, 0, "You are too confused!");
			return;
		}

		use_charge = do_power(POW_IDENTIFY_PACK, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->lore_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Study Item */
	else if ((ch == 's') && (lore > 0) && (superlore))
	{
		/* Check some conditions */
		if (p_ptr->blind)
		{
			message(MSG_FAIL, 0, "You can't see anything.");
			return;
		}
		if (!player_can_see_bold(p_ptr->py, p_ptr->px))
		{
			message(MSG_FAIL, 0, "You need some light.");
			return;
		}
		if (p_ptr->confused)
		{
			message(MSG_FAIL, 0, "You are too confused!");
			return;
		}

		use_charge = do_power(POW_ANALYSE_ITEM, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->lore_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Identify */
	else if ((ch == 'i') && (lore > 0))
	{
		/* Check some conditions */
		if (p_ptr->blind)
		{
			message(MSG_FAIL, 0, "You can't see anything.");
			return;
		}
		if (!player_can_see_bold(p_ptr->py, p_ptr->px))
		{
			message(MSG_FAIL, 0, "You need some light.");
			return;
		}
		if (p_ptr->confused)
		{
			message(MSG_FAIL, 0, "You are too confused!");
			return;
		}

		use_charge = do_power(POW_IDENTIFY, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->lore_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Recover */
	else if ((ch == 'r') && (reserves > 0) && (!(berserk)))
	{
		use_charge = do_power(POW_HEAL_CURE_2, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->reserves_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Berserk */
	else if ((ch == 'b') && (reserves > 0) && (berserk))
	{
		use_charge = do_power(POW_RAGE_1, 0, 0, 0, 0, 15, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->reserves_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	/* Escape */
	else if ((ch == 'e') && (escapes > 0))
	{
		use_charge = do_power(POW_SHIFT, 0, 0, 0, 0, 0, 0, FALSE, &ignore_me);
		if (use_charge) p_ptr->escapes_uses++;

		/* Take a turn */
		if (use_charge) p_ptr->energy_use = 100;
	}

	else return;

	p_ptr->redraw |= (PR_MANA);
}
