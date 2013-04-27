/* File: cmd1.c */

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
#include "z-quark.h"
#include "option.h"
#include "tvalsval.h"

#include "keypad.h"


/**
 * Search for hidden things
 */
void search(void)
{
	int chance = p_ptr->skills[SKILL_SEARCH];	/* Start with base search ability */

	object_type *o_ptr;

	coord t;

	/* Penalize various conditions */
	if (p_ptr->timed[TMD_BLIND] || no_lite()) chance /= 10;
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) chance /= 10;

	/* Search the nearby grids, which are always in bounds */
	for (t.y = (p_ptr->loc.y - 1); t.y <= (p_ptr->loc.y + 1); t.y++)
	{
		for (t.x = (p_ptr->loc.x - 1); t.x <= (p_ptr->loc.x + 1); t.x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Invisible trap */
				if (cave_feat[t.y][t.x] == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(t);

					/* Message */
					msg_print("You have found a trap.");

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (cave_feat[t.y][t.x] == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door */
					place_closed_door(t.y, t.x);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (o_ptr = get_first_object(t.y, t.x); o_ptr; o_ptr = get_next_object(o_ptr))
				{
					/* Skip non-chests */
					if (o_ptr->obj_id.tval != TV_CHEST) continue;

					/* Skip disarmed chests */
					if (o_ptr->pval <= 0) continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval]) continue;

					/* Identify once */
					if (!o_ptr->known())
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0, 0);
					}
				}
			}
		}
	}
}


/**
 * Determine if the object can be picked up, and has "=g" in its inscription.
 */
static bool auto_pickup_okay(const object_type *o_ptr)
{
	/* It can't be carried */
	if (!inven_carry_okay(o_ptr)) return (FALSE);

	return check_for_inscrip(o_ptr,"=g");
}


/**
 * Helper routine for py_pickup() and py_pickup_floor().
 *
 * Add the given dungeon object to the character's inventory.
 *
 * Delete the object afterwards.
 */
static void py_pickup_aux(int o_idx)
{
	char o_name[80];
	object_type* o_ptr = &o_list[o_idx];
	int slot = inven_carry(o_ptr);	/* Carry the object */

	/* Get the object again */
	o_ptr = &p_ptr->inventory[slot];

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

	/* Message */
	msg_format("You have %s (%c).", o_name, index_to_label(slot));

	/* Delete the object */
	delete_object_idx(o_idx);
}


/**
 * Make the player carry everything in a grid.
 *
 * \param pickup If FALSE, then only gold will be picked up.
 */
void py_pickup(int pickup)
{
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	char o_name[80];

	int sound_msg;
	int last_o_idx = 0;

	int can_pickup = 0;
	int not_pickup = 0;


	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->obj_id.tval == TV_GOLD)
		{
			/* Determine which sound to play */
			if ((long)o_ptr->pval < 200) sound_msg = MSG_MONEY1;
			else if ((long)o_ptr->pval < 600) sound_msg = MSG_MONEY2;
			else sound_msg = MSG_MONEY3;

			/* Message */
			message_format(sound_msg, 0, "You have found %ld gold pieces worth of %s.",
			               (long)o_ptr->pval, o_name);

			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Delete the gold */
			delete_object_idx(this_o_idx);

			/* Check the next object */
			continue;
		}

		/* Test for auto-pickup */
		if (auto_pickup_okay(o_ptr))
		{
			/* Pick up the object */
			py_pickup_aux(this_o_idx);

			/* Check the next object */
			continue;
		}

		/* Easy Floor */
		if (OPTION(easy_floor))
		{
			/* Pickup if possible */
			if (pickup && inven_carry_okay(o_ptr))
			{
				/* Pick up if allowed */
				if (!OPTION(carry_query_flag))
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
				}

				/* Else count */
				else
				{
					/* Remember */
					last_o_idx = this_o_idx;

					/* Count */
					++can_pickup;
				}
			}

			/* Else count */
			else
			{
				/* Remember */
				last_o_idx = this_o_idx;

				/* Count */
				++not_pickup;
			}

			/* Check the next object */
			continue;
		}

		/* Describe the object */
		if (!pickup)
		{
			msg_format("You see %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Note that the pack is too full */
		if (!inven_carry_okay(o_ptr))
		{
			msg_format("You have no room for %s.", o_name);

			/* Check the next object */
			continue;
		}

		/* Query before picking up */
		if (OPTION(carry_query_flag))
		{
			char out_val[160];
			strnfmt(out_val, sizeof(out_val), "Pick up %s? ", o_name);
			if (!get_check(out_val)) continue;
		}

		/* Pick up the object */
		py_pickup_aux(this_o_idx);
	}

	/* Easy floor, objects left */
	if (OPTION(easy_floor) && (can_pickup + not_pickup > 0))
	{
		/* Not picking up */
		if (!pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				/* Describe the object */
				object_desc(o_name, sizeof(o_name), &o_list[last_o_idx], TRUE, ODESC_FULL);

				/* Message */
				msg_format("You see %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				msg_format("You see a pile of %d objects.", not_pickup);
			}

			/* Done */
			return;
		}

		/* No room */
		if (!can_pickup)
		{
			/* One object */
			if (not_pickup == 1)
			{
				/* Describe the object */
				object_desc(o_name, sizeof(o_name), &o_list[last_o_idx], TRUE, ODESC_FULL);

				/* Message */
				msg_format("You have no room for %s.", o_name);
			}

			/* Multiple objects */
			else
			{
				/* Message */
				msg_print("You have no room for any of the objects on the floor.");
			}

			/* Done */
			return;
		}

		/* Pick up objects */
		while (1)
		{
			const char* q = "Get which item? ";

			int item;

			/* Restrict the choices */
			item_tester_hook = inven_carry_okay;

			/* Get an object*/
			if (!get_item(&item, q, NULL, (USE_FLOOR))) break;

			/* Pick up the object */
			py_pickup_aux(0 - item);
		}
	}
}





/**
 * Move player in the given direction, with the given "pickup" flag.
 *
 * \pre This routine should only be called when energy has been expended.
 *
 * \note This routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
void move_player(int dir, int jumping)
{
	/* Find the result of moving */
	coord dest_g(p_ptr->loc + dd_coord[dir]);

	/* Hack -- attack monsters */
	if (cave_m_idx[dest_g.y][dest_g.x] > 0)
	{
		/* Attack */
		py_attack(dest_g);
	}

	/* Optionally alter known traps/doors on (non-jumping) movement */
	else if (OPTION(easy_alter) && !jumping &&
	         (cave_info[dest_g.y][dest_g.x] & (CAVE_MARK)) &&
	         cave_feat_in_range(dest_g.y,dest_g.x,FEAT_TRAP_HEAD,FEAT_DOOR_TAIL))
	{
		/* Not already repeating */
		if (!p_ptr->command_rep)
		{
			/* Hack -- Optional auto-repeat */
			if (OPTION(always_repeat) && (p_ptr->command_arg <= 0))
			{
				/* Repeat 99 times */
				p_ptr->command_rep = 99;

				/* Reset the command count */
				p_ptr->command_arg = 0;
			}
		}

		/* Alter */
		do_cmd_alter();
	}

	/* Player can not walk through "walls" */
	else if (!cave_floor_bold(dest_g.y, dest_g.x))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice unknown obstacles */
		if (!(cave_info[dest_g.y][dest_g.x] & (CAVE_MARK)))
		{
			/* Rubble */
			if (cave_feat[dest_g.y][dest_g.x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "You feel a pile of rubble blocking your way.");
				cave_info[dest_g.y][dest_g.x] |= (CAVE_MARK);
				lite_spot(dest_g);
			}

			/* Closed door */
			else if (cave_feat[dest_g.y][dest_g.x] < FEAT_SECRET)
			{
				message(MSG_HITWALL, 0, "You feel a door blocking your way.");
				cave_info[dest_g.y][dest_g.x] |= (CAVE_MARK);
				lite_spot(dest_g);
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "You feel a wall blocking your way.");
				cave_info[dest_g.y][dest_g.x] |= (CAVE_MARK);
				lite_spot(dest_g);
			}
		}

		/* Mention known obstacles */
		else
		{
			/* Rubble */
			if (cave_feat[dest_g.y][dest_g.x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way.");
			}

			/* Closed door */
			else if (cave_feat[dest_g.y][dest_g.x] < FEAT_SECRET)
			{
				message(MSG_HITWALL, 0, "There is a door blocking your way.");
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "There is a wall blocking your way.");
			}
		}
	}

	/* Normal movement */
	else
	{
		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* Move player */
		monster_swap(p_ptr->loc, dest_g);

		/* New location */
		dest_g = p_ptr->loc;

		/* Zaiband: if a diagonal move, use 150 energy rather than 100 energy */
		if (0!=dd_coord[dir].x && 0!=dd_coord[dir].y) p_ptr->energy_use += 50;

		/* Spontaneous Searching */
		if ((p_ptr->skills[SKILL_SEARCH_FREQUENCY]) ||
		    one_in_(50 - p_ptr->skills[SKILL_SEARCH_FREQUENCY]))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
		py_pickup(jumping != OPTION(always_pickup));

		/* Handle "store doors" */
		if (cave_feat_in_range(dest_g.y,dest_g.x,FEAT_SHOP_HEAD,FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';

			/* Free turn XXX XXX XXX */
			p_ptr->energy_use = 0;
		}

		/* Discover invisible traps */
		else if (cave_feat[dest_g.y][dest_g.x] == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0, 0);

			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(dest_g);

			/* Hit the trap */
			hit_trap(dest_g);
		}

		/* Set off an visible trap */
		else if (cave_feat_in_range(dest_g.y,dest_g.x,FEAT_TRAP_HEAD,FEAT_TRAP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_trap(dest_g);
		}
	}
}


/**
 * Hack -- Check for a "known wall" (see below)
 */
static bool see_wall(int dir, coord g)
{
	/* Get the new location */
	g += dd_coord[dir];

	/* Illegal grids are not known walls XXX XXX XXX */
	if (!in_bounds(g.y, g.x)) return (FALSE);

	/* Non-wall grids are not known walls */
	if (cave_feat[g.y][g.x] < FEAT_SECRET) return (FALSE);

	/* Unknown walls are not known walls; default */
	return cave_info[g.y][g.x] & (CAVE_MARK);
}


/**
 * Hack -- Check for an "unknown corner" (see below)
 */
static bool see_nothing(int dir, coord g)
{
	/* Get the new location */
	g += dd_coord[dir];

	/* Illegal grids are unknown XXX XXX XXX */
	if (!in_bounds(g.y, g.x)) return (TRUE);

	/* Memorized grids are always known */
	return !(cave_info[g.y][g.x] & (CAVE_MARK));
}





/*
 * The running algorithm  -CJS-
 *
 * Basically, once you start running, you keep moving until something
 * interesting happens.  In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.  This means that we
 * must be careful when dealing with "illegal" grids.
 *
 * No assumptions are made about the layout of the dungeon, so this
 * algorithm works in hallways, rooms, town, destroyed areas, etc.
 *
 * In the diagrams below, the player has just arrived in the grid
 * marked as '@', and he has just come from a grid marked as 'o',
 * and he is about to enter the grid marked as 'x'.
 *
 * Running while confused is not allowed, and so running into a wall
 * is only possible when the wall is not seen by the player.  This
 * will take a turn and stop the running.
 *
 * Several conditions are tracked by the running variables.
 *
 *   p_ptr->run_open_area (in the open on at least one side)
 *   p_ptr->run_break_left (wall on the left, stop if it opens)
 *   p_ptr->run_break_right (wall on the right, stop if it opens)
 *
 * When running begins, these conditions are initialized by examining
 * the grids adjacent to the requested destination grid (marked 'x'),
 * two on each side (marked 'L' and 'R').  If either one of the two
 * grids on a given side is a wall, then that side is considered to
 * be "closed".  Both sides enclosed yields a hallway.
 *
 *    LL                     @L
 *    @x      (normal)       RxL   (diagonal)
 *    RR      (east)          R    (south-east)
 *
 * In the diagram below, in which the player is running east along a
 * hallway, he will stop as indicated before attempting to enter the
 * intersection (marked 'x').  Starting a new run in an.y direction
 * will begin a new hallway run.
 *
 * #.#
 * ##.##
 * o@x..
 * ##.##
 * #.#
 *
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * In the diagram below, the player is running east down a hallway,
 * and will stop in the grid (marked '1') before the intersection.
 * Continuing the run to the south-east would result in a long run
 * stopping at the end of the hallway (marked '2').
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After each step, the surroundings are examined to determine if
 * the running should stop, and to determine if the running should
 * change direction.  We examine the new current player location
 * (at which the runner has just arrived) and the direction from
 * which the runner is considered to have come.
 *
 * Moving one grid in some direction places you adjacent to three
 * or five new grids (for straight and diagonal moves respectively)
 * to which you were not previously adjacent (marked as '!').
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * If any of the newly adjacent grids are "interesting" (monsters,
 * objects, some terrain features) then running stops.
 *
 * If any of the newly adjacent grids seem to be open, and you are
 * looking for a break on that side, then running stops.
 *
 * If any of the newly adjacent grids do not seem to be open, and
 * you are in an open area, and the non-open side was previously
 * entirely open, then running stops.
 *
 * If you are in a hallway, then the algorithm must determine if
 * the running should continue, turn, or stop.  If only one of the
 * newly adjacent grids appears to be open, then running continues
 * in that direction, turning if necessary.  If there are more than
 * two possible choices, then running stops.  If there are exactly
 * two possible choices, separated by a grid which does not seem
 * to be open, then running stops.  Otherwise, as shown below, the
 * player has probably reached a "corner".
 *
 *    ###             o##
 *    o@x  (normal)   #@!   (diagonal)
 *    ##!  (east)     ##x   (south east)
 *
 * In this situation, there will be two newly adjacent open grids,
 * one touching the player on a diagonal, and one directly adjacent.
 * We must consider the two "option" grids further out (marked '?').
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid.  For some unknown reason, we assign "check_dir" to
 * the grid marked 's', which may be incorrectly labelled.
 *
 *    ###s
 *    o@x?   (may be incorrect diagram!)
 *    ##!?
 *
 * If both "option" grids are closed, then there is no reason to enter
 * the corner, and so we can cut the corner, by moving into the other
 * grid (diagonally).  If we choose not to cut the corner, then we may
 * go straight, but we pretend that we got there by moving diagonally.
 * Below, we avoid the obvious grid (marked 'x') and cut the corner
 * instead (marked 'n').
 *
 *    ###:               o##
 *    o@x#   (normal)    #@n    (maybe?)
 *    ##n#   (east)      ##x#
 *                       ####
 *
 * If one of the "option" grids is open, then we may have a choice, so
 * we check to see whether it is a potential corner or an intersection
 * (or room entrance).  If the grid two spaces straight ahead, and the
 * space marked with 's' are both open, then it is a potential corner
 * and we enter it if requested.  Otherwise, we stop, because it is
 * not a corner, and is instead an intersection or a room entrance.
 *
 *    ###
 *    o@x
 *    ##!#
 *
 * I do not think this documentation is correct.
 */




/**
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. (?)
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
	int i;
	coord t;

	/* Assume no nearby walls */
	bool deepleft = false;
	bool deepright = false;
	bool shortleft = false;
	bool shortright = false;


	/* Save the direction */
	p_ptr->run_cur_dir = dir;

	/* Assume running straight */
	p_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	p_ptr->run_open_area = TRUE;

	/* Assume not looking for breaks */
	p_ptr->run_break_right = FALSE;
	p_ptr->run_break_left = FALSE;

	/* Find the destination grid */
	t = p_ptr->loc + dd_coord[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], p_ptr->loc))
	{
		p_ptr->run_break_left = TRUE;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], t))
	{
		p_ptr->run_break_left = TRUE;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], p_ptr->loc))
	{
		p_ptr->run_break_right = TRUE;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], t))
	{
		p_ptr->run_break_right = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (p_ptr->run_break_left && p_ptr->run_break_right)
	{
		/* Not looking for open area */
		p_ptr->run_open_area = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				p_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				p_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], t))
		{
			if (shortleft && !shortright)
			{
				p_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				p_ptr->run_old_dir = cycle[i + 2];
			}
		}
	}
}


/**
 * Update the current "run" path
 *
 * \return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int prev_dir = p_ptr->run_old_dir;	/* Where we came from */
	int new_dir;
	int check_dir = 0;

	int i, inv;
	int max = (prev_dir & 0x01) + 1;	/* Range of newly adjacent grids */

	/* No options yet */
	int option = 0;
	int option2 = 0;

	coord t;

	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		object_type *o_ptr;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		t = p_ptr->loc + dd_coord[new_dir];

		/* Visible monsters abort running */
		if (cave_m_idx[t.y][t.x] > 0)
		{
			/* Visible monster */
			if (mon_list[cave_m_idx[t.y][t.x]].ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (o_ptr = get_first_object(t.y, t.x); o_ptr; o_ptr = get_next_object(o_ptr))
		{
			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}


		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (cave_info[t.y][t.x] & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (cave_feat[t.y][t.x])
			{
				/* Floors */
				case FEAT_FLOOR:

				/* Invis traps */
				case FEAT_INVIS:

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (OPTION(run_ignore_doors)) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (OPTION(run_ignore_stairs)) notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(t.y, t.x))
		{
			/* Looking for open area */
			if (p_ptr->run_open_area)
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (p_ptr->run_open_area)
			{
				if (i < 0)
				{
					/* Break to the right */
					p_ptr->run_break_right = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					p_ptr->run_break_left = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (p_ptr->run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			t = p_ptr->loc + dd_coord[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[t.y][t.x] & (CAVE_MARK)) ||
			    (cave_feat[t.y][t.x] < FEAT_SECRET))
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			t = p_ptr->loc + dd_coord[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[t.y][t.x] & (CAVE_MARK)) ||
			    (cave_feat[t.y][t.x] < FEAT_SECRET))
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* No other options */
			p_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else if (OPTION(run_use_corners) && !OPTION(run_cut_corners))
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			p_ptr->run_old_dir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			t = p_ptr->loc + dd_coord[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, t) ||
			    !see_wall(check_dir, t))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (OPTION(run_use_corners) &&
				    see_nothing(option, t) &&
				    see_nothing(option2, t))
				{
					p_ptr->run_cur_dir = option;
					p_ptr->run_old_dir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (OPTION(run_cut_corners))
			{
				p_ptr->run_cur_dir = option2;
				p_ptr->run_old_dir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				p_ptr->run_cur_dir = option;
				p_ptr->run_old_dir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(p_ptr->run_cur_dir, p_ptr->loc))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/**
 * Take one step along the current "run" path
 *
 * \param dir a real direction to begin a new run; zero to continue a run in progress.
 */
void run_step(int dir)
{
	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);

		/* Hack -- Set the run counter */
		p_ptr->running = (p_ptr->command_arg ? p_ptr->command_arg : 1000);

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Continue run */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}
	}

	/* Decrease counter */
	p_ptr->running--;

	/* Take time */
	p_ptr->energy_use = 100;

	/* Move the player */
	move_player(p_ptr->run_cur_dir, FALSE);
}

