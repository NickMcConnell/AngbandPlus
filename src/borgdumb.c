/* File: borgdumb.c */

/*
 * Code for the "mindless borg", an aid to debugging and profiling the game.
 *
 * Copyright (c) 2007
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */


#include "angband.h"

#ifdef ALLOW_BORG


/*
 * Global variables
 */
u32b count_stop = 0L;
int count_change_level = 0;
int count_teleport = 0;
byte allowed_depth[2] = { 0, 0 };
byte borg_dir;


/*
 * Run the "Mindless Borg".
 */
void do_cmd_borg(void)
{
	int i, j;
	int y, x;
	int feat;
	int dir_good[8];


	/* Start the borg. */
	if (count_stop == 0)
	{
		char ch[80] = "";

		/* Query */
		if (!get_string("How many turns shall the borg play for?", ch, 9)) return;

		/* Set timer */
		count_stop = 1L + (long)atol(ch);


		/* Query */
		if (!get_string("What is the minimun level that the borg should travel on?", ch, 3)) return;

		/* Set minimum depth */
		allowed_depth[0] = (byte)atoi(ch);
		if (allowed_depth[0] >= MAX_DEPTH) allowed_depth[0] = MAX_DEPTH - 1;

		/* Query */
		if (!get_string("What is the maximum level that the borg should travel on?", ch, 3)) return;

		/* Set maximum depth */
		allowed_depth[1] = (byte)atoi(ch);
		if (allowed_depth[1] >= MAX_DEPTH) allowed_depth[1] = MAX_DEPTH - 1;

		/* Check sanity. */
		if (allowed_depth[1] < allowed_depth[0])
		{
			msg_print("Clearing all level restrictions.");
			allowed_depth[0] = 0;
			allowed_depth[1] = MAX_DEPTH - 1;

		}

		/* Change level immediately, wait a bit before teleporting. */
		count_change_level = 0;
		count_teleport = rand_range(100, 150);

		/* Pick a direction of travel at random */
		borg_dir = rand_int(8);
	}

	/* Stop when needed. */
	count_stop--;
	if (count_stop == 0)
	{
		/* Redraw everything */
		do_cmd_redraw();

		return;
	}
	else
	{
		/* The borg has a light radius of three. */
		p_ptr->cur_lite = 3;

		/* The borg is always in perfect health */

		/* Remove curses */
		(void)remove_all_curse();

		/* Restore stats */
		(void)res_stat(A_STR);
		(void)res_stat(A_INT);
		(void)res_stat(A_WIS);
		(void)res_stat(A_CON);
		(void)res_stat(A_DEX);
		(void)res_stat(A_CHR);

		/* No maladies */
		p_ptr->blind = 0;
		p_ptr->confused = 0;
		p_ptr->poisoned = 0;
		p_ptr->diseased = 0;
		p_ptr->afraid = 0;
		p_ptr->paralyzed = 0;
		p_ptr->image = 0;
		p_ptr->slow = 0;
		p_ptr->stun = 0;
		p_ptr->paralyzed = 0;
		p_ptr->cut = 0;
		p_ptr->black_breath = 0;
		p_ptr->mania = 0;

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;
		p_ptr->chp_frac = 0;

		/* Normal luck */
		p_ptr->luck = 0;

		/* No longer hungry */
		p_ptr->food = PY_FOOD_MAX - 1;
	}

	/* Do not wait */
	inkey_scan = TRUE;

	/* Check for a key, and stop the borg if one is pressed */
	if (inkey(FALSE))
	{
		/* Flush input */
		flush();

		/* Disturb */
		disturb(0, 0);

		/* Stop the borg */
		count_stop = 0;
		msg_print("Stopping...");

		/* Redraw everything */
		do_cmd_redraw();

		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100 - (p_ptr->depth / 2);

	/* Refresh the screen */
	(void)Term_fresh();

	/* Delay the borg */
	if (op_ptr->delay_factor) pause_for(op_ptr->delay_factor * op_ptr->delay_factor);


	/* Change level when needed. */
	if (count_change_level > 0) count_change_level--;
	else
	{
		/* New depth */
		p_ptr->depth = rand_range(allowed_depth[0], allowed_depth[1]);

		/* Leaving */
		p_ptr->leaving = TRUE;

		count_change_level = rand_range(1000, 2500);
		return;
	}

	/* Teleport when needed. */
	if (count_teleport > 0) count_teleport--;
	else
	{
		teleport_player(rand_range(20, 200), TRUE, FALSE);
		count_teleport = rand_range(100, 150);
		return;
	}

	/* Look up to a distance of three for monsters to attack. */
	for (i = 1; i < 37; i++)
	{
		y = p_ptr->py + nearby_grids_y[i];
		x = p_ptr->px + nearby_grids_x[i];

		if ((in_bounds(y, x)) && (cave_m_idx[y][x] > 0))
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

			if (m_ptr->ml)
			{
				bool dummy;

				/* Wizard zap the monster. */
				mon_take_hit(cave_m_idx[y][x], -1, p_ptr->depth * 2, &dummy, NULL);
				return;
			}
		}
	}

	/*
	 * Look at next grid.  Assume the dungeon to be surrounded by a
	 * wall that the character cannot pass.
	 */
	y = p_ptr->py + ddy_ddd[borg_dir];
	x = p_ptr->px + ddx_ddd[borg_dir];

	/*
	 * If grid in our current direction of travel is not passable, or
	 * sometimes even if it is, choose a passable adjacent grid at random.
	 */
	if ((cave_feat[y][x] > FEAT_RUBBLE) ||
	   ((cave_info[y][x] & (CAVE_ROOM)) && (one_in_(8))))
	{
		/* Look all around */
		for (j = 0, i = 0; i < 8; i++)
		{
			y = p_ptr->py + ddy_ddd[i];
			x = p_ptr->px + ddx_ddd[i];

			/* Accept any passable grid. */
			if (cave_feat[y][x] <= FEAT_RUBBLE)
			{
				/* Store good directions. */
				dir_good[j] = i;
				j++;
			}
		}

		/* If we're totally trapped, teleport away (later). */
		if (j == 0)
		{
			count_teleport = 0;
			return;
		}

		/* Choose a direction at random from our list. */
		borg_dir = dir_good[rand_int(j)];

		/* look at grid in chosen direction. */
		y = p_ptr->py + ddy_ddd[borg_dir];
		x = p_ptr->px + ddx_ddd[borg_dir];
	}

	/* Get the feature in the grid. */
	feat = cave_feat[y][x];

	/* Move into terrain that need not be altered. */
	if ((feat == FEAT_FLOOR) ||
	    (feat == FEAT_OPEN) ||
	    (feat == FEAT_BROKEN) ||
	    (feat == FEAT_LESS) ||
	    (feat == FEAT_MORE) ||
	    (feat == FEAT_WATER) ||
	    (feat == FEAT_TREE) ||
	    (feat == FEAT_LAVA))
	{
		monster_swap(p_ptr->py, p_ptr->px, y, x);
	}

	/* Open doors. */
	else if ((feat == FEAT_SECRET) ||
		((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL)))
	{
		cave_set_feat(y, x, FEAT_OPEN);
	}

	/* Disarm traps. */
	if (cave_trap(y, x))
	{
		remove_trap(y, x, -1);
	}

	/* Remove rubble. */
	else if (feat == FEAT_RUBBLE) cave_set_feat(y, x, FEAT_FLOOR);

	/* If the terrain is anything else, do nothing. */
}

#endif
