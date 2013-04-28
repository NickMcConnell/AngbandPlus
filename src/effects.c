/* File: effects.c */

/*
 * The lingering spell effects code.
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



/*
 * Try to find space for a new effect, prepare the first available one.
 */
int effect_prep(void)
{
	int i;

	/* Get a new effect */
	for (i = 0; i < z_info->x_max; i++)
	{
		/* Effect is not in use */
		if (!x_list[i].index) break;
	}

	/* Effect array is completely filled */
	if (i == z_info->x_max) return (-1);

	/* We found some space */
	else
	{
		/* Get this effect */
		effect_type *x_ptr = &x_list[i];

		/* Wipe it */
		WIPE(x_ptr, effect_type);
	}

	/* Return the index */
	return (i);
}



/*
 * Adjust effects.
 *
 * Make various tweaks to effects to handle special conditions.
 */
static void adjust_effect(int x_idx)
{
	/* Get this effect */
	effect_type *x_ptr = &x_list[x_idx];


	/* Paranoia -- no tweaking dead effects */
	if (!x_ptr->index) return;

	/* This effect has already been tweaked */
	if (x_ptr->flags & (EF1_TWEAKED)) return;


	/*
	 * Hack^Hack -- as wall spells must move orthogonally, they move
	 * more slowly when fired diagonally.  Adjust both the lifespan
	 * and the rate of travel to compensate for this.
	 */
	if (x_ptr->index == EFFECT_WALL)
	{
		int dist = distance(x_ptr->y0, x_ptr->x0, x_ptr->y1, x_ptr->x1);
		int dy = ABS(x_ptr->y0 - x_ptr->y1);
		int dx = ABS(x_ptr->x0 - x_ptr->x1);
		int diag_factor;

		/* No distance */
		if (!dy && !dx) return;

		/* Get "diagonal factor" */
		diag_factor = 10 * dist / MAX(dy, dx);

		/* Adjust lifespan */
		x_ptr->lifespan = (x_ptr->lifespan * diag_factor + 5) / 10;

		/* Adjust rate of travel */
		x_ptr->time_delay =
			(x_ptr->time_delay * 10 + diag_factor / 2) / diag_factor;
	}

	/* This effect has been tweaked */
	x_ptr->flags |= (EF1_TWEAKED);
}



/*
 * Get (one of) the effects impacting the given grid.
 *
 * The lingering effect code can have multiple entries for a given grid
 * at any one time.  This may occur due to two different effects zapping
 * the grid, or to the grid being zapped between effect turns.  As long
 * as a character or monster only gets zapped once when they enter a
 * grid, regardless of the number of effects in that grid, this isn't a
 * problem.  If this changes, however, then we have to rewrite.  We
 * might also have to rebalance.
 */
int effect_grid_idx(int y, int x)
{
	/* Start at the last record */
	int i = effect_grid_n - 1;

	/* Search for the grid (backwards) */
	for (; i >= 0; i--)
	{
		/* We found the grid */
		if ((effect_grid[i].y == y) && (effect_grid[i].x == x))
		{
			/* Return index of effect */
			return (effect_grid[i].x_idx);
		}
	}

	/* We did not find the grid -- remove effect marker XXX XXX */
	cave_info[y][x] &= ~(CAVE_EFFT);

	/* Return "no effect" */
	return (-1);
}

/*
 * Get the projection type (and thus the graphics) for an effect.
 *
 * This function is also responsible for cleaning up effect markers.  XXX XXX
 */
int effect_grid_proj_type(int y, int x)
{
	/* Start at the last record */
	int i = effect_grid_n - 1;

	/* Search for the grid (backwards) */
	for (; i >= 0; i--)
	{
		/* We found the grid */
		if ((effect_grid[i].y == y) && (effect_grid[i].x == x))
		{
			/* Return projection type of effect in this grid */
			return (x_list[effect_grid[i].x_idx].type);
		}
	}

	/* We did not find the grid -- remove effect marker XXX XXX */
	cave_info[y][x] &= ~(CAVE_EFFT);

	/* Return "no projection type" */
	return (-1);
}



/*
 * Sorting hook -- comp function -- array of elements four bytes in size.
 */
static bool ang_sort_comp_hook_effect_grids(const void *u, const void *v, int a, int b)
{
	effect_grid_type *x_grid = (effect_grid_type*)(u);

	/* Unused parameter */
	(void)v;

	/* Sort by y, then by x, in descending order */
	if (x_grid[a].y > x_grid[b].y) return (TRUE);
	if (x_grid[a].y < x_grid[b].y) return (FALSE);
	return (x_grid[a].x >= x_grid[b].x);
}

/*
 * Sorting hook -- swap function -- array of elements four bytes in size.
 */
static void ang_sort_swap_hook_effect_grids(void *u, void *v, int a, int b)
{
	effect_grid_type *x_grid = (effect_grid_type*)(u);
	effect_grid_type temp_grid;

	/* Unused parameter */
	(void)v;

	/* Swap records */
	COPY(&temp_grid, &x_grid[a], effect_grid_type);
	COPY(&x_grid[a], &x_grid[b], effect_grid_type);
	COPY(&x_grid[b], &temp_grid, effect_grid_type);
}


/*
 * Remove all grids belonging to a given effect from the effect grid
 * array, then sort and compact it.
 */
static void optimize_effect_array(int x_idx)
{
	int i;

	/* Remove all of the effect's markers and graphics */
	for (i = 0; i < effect_grid_n; i++)
	{
		/* Get the grid */
		effect_grid_type *xg_ptr = &effect_grid[i];

		/* Grid belongs to this effect */
		if (xg_ptr->x_idx == x_idx)
		{
			/* Save the grid */
			int y = xg_ptr->y;
			int x = xg_ptr->x;

			/* Wipe this array entry */
			WIPE(xg_ptr, effect_grid_type);

			/*
			 * Refresh the graphics, remove marker if this was the
			 * only effect impacting this grid.
			 */
			lite_spot(y, x);
		}
	}


	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook_effect_grids;
	ang_sort_swap = ang_sort_swap_hook_effect_grids;

	/* Sort the array (descending order by grid location) */
	ang_sort(effect_grid, 0, effect_grid_n);


	/* Work from the end of the array, compacting as we go */
	for (i = effect_grid_n - 1; i >= 0; i--)
	{
		/* Ignore all records with zero y-values */
		if (!effect_grid[i].y) effect_grid_n--;
	}
}



/*
 * Effect zaps a grid with lingering effect.  Used for all effects that
 * need to display graphics and affect monsters/the character/the dungeon
 * between turns.
 *
 * Return "grid was projectable".
 */
bool do_effect_linger(int x_idx, int y, int x)
{
	/* Get this effect */
	effect_type *x_ptr = &x_list[x_idx];

	/* Basic projection flags */
	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID;

	/* Get caster (who gets exp, etc.) */
	int who = ((x_ptr->flags & (EF1_CHARACTER)) ? -1 : 0);

	/* Optional flags */
	if (x_ptr->flags)
	{
		/* Affect only evil */
		if (x_ptr->flags & (EF1_HURT_EVIL)) p_ptr->proj_mon_flags = (RF3_EVIL);

		/* Hurt the character */
		if (x_ptr->flags & (EF1_HURT_PLAY)) flg |= (PROJECT_PLAY);
	}

	/* Usually, do not affect non-projectable terrain */
	if (!cave_project_bold(y, x) && !(flg & (PROJECT_PASS))) return (FALSE);

	/* We have not run out of space in the effect grid array */
	if (effect_grid_n < EFFECT_GRID_MAX-1)
	{
		/* Mark the grid */
		cave_info[y][x] |= (CAVE_EFFT);

		/* Store location and effect index */
		effect_grid[effect_grid_n].y = y;
		effect_grid[effect_grid_n].x = x;
		effect_grid[effect_grid_n].x_idx = x_idx;

		/* Another entry in the array */
		effect_grid_n++;
	}


	/* Has hit the character */
	if ((y == p_ptr->py) && (x == p_ptr->px))
	{
		/* Print messages  XXX */
		switch (x_ptr->type)
		{
			case GF_ACID:  msg_print("You are covered in acid!");  break;
			case GF_ELEC:  msg_print("You are struck by lightning!");  break;
			case GF_FIRE:  msg_print("You are enveloped in fire!");  break;
			case GF_COLD:  msg_print("You are very cold!");  break;
			case GF_POIS:  msg_print("You are surrounded by poison!");  break;

			case GF_PLASMA:  msg_print("You are enveloped in electric fire!");  break;
			case GF_HELLFIRE:  msg_print("You are enveloped by Udun-fire!");  break;
			case GF_ICE:  msg_print("You are hit by shards of ice!");  break;

			default:  msg_print("You are hit!");  break;
		}
	}

	/* Practice a skill */
	if (x_ptr->practice_skill) skill_being_used = x_ptr->practice_skill;

	/* Cast the spell */
	(void)project(who, 0, y, x, y, x, x_ptr->power, x_ptr->type, flg, 0, 0);


	/* Display lingering spell colors (require ability to see or illuminated effect in LOS) */
	if ((player_can_see_bold(y, x)) ||
	    ((x_ptr->flags & (EF1_SHINING)) && (player_has_los_bold(y, x))))
	{
		/* Redraw this grid */
		lite_spot(y, x);
	}

	/* Success */
	return (TRUE);
}


/*
 * Process an individual effect.
 */
static void process_effect(int x_idx)
{
	/* Get this effect */
	effect_type *x_ptr = &x_list[x_idx];

	int i, j, axis;
	int y, x;
	int y0, x0;

	int grids = 0;

	bool notice = FALSE; /* Assume nothing noticed */


	/* Paranoia -- no processing dead effects */
	if (!x_ptr->index) return;

	/* Count down to next turn, return if not yet ready */
	if (--x_ptr->time_count > 0) return;


	/* Reset count */
	x_ptr->time_count = x_ptr->time_delay;

	/* If the effect has recently been "born", make various tweaks to it */
	if (x_ptr->age == 0) adjust_effect(x_idx);

	/* Clear graphics and effect markers from last iteration of this effect */
	optimize_effect_array(x_idx);


	/* Effects eventually "die" */
	if (x_ptr->age >= x_ptr->lifespan)
	{
		/* Special message for clouds of death (they have no graphics) */
		if (x_ptr->index == EFFECT_DEATH_CLOUD)
		{
			msg_print("The cloud of death dissipates.");
		}

		/* Effect is "dead" */
		x_ptr->index = 0;
		return;
	}


	/* Standard lingering clouds */
	if (x_ptr->index == EFFECT_IRREGULAR_CLOUD)
	{
		/* We always fire a star and refresh the graphics */
		u32b flg = PROJECT_STAR | PROJECT_BOOM;

		int dam = 0;
		int rad = x_ptr->power2;
		int typ = x_ptr->type;

		y = x_ptr->y0;
		x = x_ptr->x0;


		/* Add the kill flags */
		flg |= PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

		/* Damage equals power */
		dam = x_ptr->power;

		/* Cast a star */
		(void)project(-1, rad, y, x, y, x, dam, typ, flg, 0, 0);
	}


	/* Advancing walls */
	else if (x_ptr->index == EFFECT_WALL)
	{
		/* Get the direction of travel (angular dir is 2x this) */
		int major_axis =
			get_angle_to_target(x_ptr->y0, x_ptr->x0, x_ptr->y1, x_ptr->x1, 0);

		/* Get the length of the wall on either side of the major axis */
		int spread = x_ptr->power2;

		/* Store target grid */
		y = x_ptr->y1;
		x = x_ptr->x1;

		/* Calculate the projection path -- require orthogonal movement */
		(void)project_path(99, x_ptr->y0, x_ptr->x0,
			&y, &x, PROJECT_PASS | PROJECT_THRU | PROJECT_ORTH);

		/* Require a working projection path */
		if (path_n > x_ptr->age)
		{
			/* Remember delay factor */
			int old_delay = op_ptr->delay_factor;

			int zaps = 0;

			/* Get center grid (walls travel one grid per turn) */
			y0 = GRID_Y(path_g[x_ptr->age]);
			x0 = GRID_X(path_g[x_ptr->age]);

			/* Count grid */
			grids++;

			/* Set delay to half normal */
			op_ptr->delay_factor /= 2;

			/*
			 * If the center grid is both projectable and in LOS
			 * from the origin of the effect, zap it.
			 */
			if ((cave_project_bold(y0, x0)) &&
				(los(x_ptr->y0, x_ptr->x0, y0, x0)))
			{
				(void)do_effect_linger(x_idx, y0, x0);
				zaps++;
			}

			/* Notice visibility */
			if (player_can_see_bold(y0, x0)) notice = TRUE;


			/* If this wall spreads out from the origin, */
			if (spread >= 1)
			{
				/* Get the directions of spread (angular dir is 150% of this) */
				int minor_axis1 = (major_axis +  60) % 240;
				int minor_axis2 = (major_axis + 180) % 240;

				/* Process the left, then right-hand sides of the wall */
				for (i = 0; i < 2; i++)
				{
					if (i == 0) axis = minor_axis1;
					else        axis = minor_axis2;

					/* Get the target grid for this side */
					get_grid_using_angle(axis, y0, x0, &y, &x);

					/* If we have a legal target, */
					if ((y != y0) || (x != x0))
					{
						/* Calculate the projection path */
						(void)project_path(spread, y0, x0, &y, &x,
							PROJECT_PASS | PROJECT_THRU);

						/* Check all the grids */
						for (j = 0; j < path_n; j++)
						{
							/* Get grid */
							y = GRID_Y(path_g[j]);
							x = GRID_X(path_g[j]);

							/*
							 * If this grid is both projectable and in LOS
							 * from the origin of the effect, zap it.
							 */
							if ((cave_project_bold(y, x)) &&
							    (los(x_ptr->y0, x_ptr->x0, y, x)))
							{
								(void)do_effect_linger(x_idx, y, x);
								zaps++;
							}

							/* Notice visibility */
							if (player_can_see_bold(y, x)) notice = TRUE;
						}
					}
				}
			}

			/* Kill wall if nothing got zapped this turn */
			if (zaps == 0)
			{
				if (notice) msg_print("The spell fizzles out.");
				x_ptr->age = 250;
			}

			/* Restore standard delay */
			op_ptr->delay_factor = old_delay;
		}

		/* No working projection path -- kill the wall */
		else
		{
			x_ptr->age = 250;
		}
	}


	/* Seeker vortexes */
	else if (x_ptr->index == EFFECT_SEEKER_VORTEX)
	{
		int dir;
		int ty = 0, tx = 0;

		/* Check around (and under) the vortex */
		for (dir = 0; dir < 9; dir++)
		{
			/* Extract adjacent (legal) location */
			y = x_ptr->y0 + ddy_ddd[dir];
			x = x_ptr->x0 + ddx_ddd[dir];

			/* Count passable grids */
			if (cave_passable_bold(y, x)) grids++;
		}

		/*
		 * Vortexes only seek in open spaces.  This makes them useful in
		 * rooms and not too powerful in corridors.
		 */
		if      (grids >= 5) i = 85;
		else if (grids == 4) i = 50;
		else                 i = 0;

		/* Seek out monsters */
		if (rand_int(100) < i)
		{
			/* Try to get a target (nearest or next-nearest monster) */
			get_closest_los_monster(randint(2), x_ptr->y0, x_ptr->x0,
				&ty, &tx, FALSE);
		}

		/* No valid target, or monster is in an impassable grid */
		if (((ty == 0) && (tx == 0)) || (!cave_passable_bold(ty, tx)))
		{
			/* Move randomly */
			dir = randint(9);
		}

		/* Valid target in passable terrain */
		else
		{
			/* Get change in position from current location to target */
			int dy = x_ptr->y0 - ty;
			int dx = x_ptr->x0 - tx;

			/* Calculate vertical and horizontal distances */
			int ay = ABS(dy);
			int ax = ABS(dx);

			/* We mostly want to move vertically */
			if (ay > (ax * 2))
			{
				/* Choose between directions '8' and '2' */
				if (dy > 0) dir = 8;
				else        dir = 2;
			}

			/* We mostly want to move horizontally */
			else if (ax > (ay * 2))
			{
				/* Choose between directions '4' and '6' */
				if (dx > 0) dir = 4;
				else        dir = 6;
			}

			/* We want to move up and sideways */
			else if (dy > 0)
			{
				/* Choose between directions '7' and '9' */
				if (dx > 0) dir = 7;
				else        dir = 9;
			}

			/* We want to move down and sideways */
			else
			{
				/* Choose between directions '1' and '3' */
				if (dx > 0) dir = 1;
				else        dir = 3;
			}
		}

		/* Convert dir into a grid */
		for (i = 0; i < 100; i++)
		{
			/* Extract adjacent (legal) location */
			y = x_ptr->y0 + ddy[dir];
			x = x_ptr->x0 + ddx[dir];

			/* Require passable grids */
			if (cave_passable_bold(y, x))
			{
				/* Try not to stay in place */
				if ((y != x_ptr->y0) || (x != x_ptr->x0)) break;
			}

			/* Move randomly */
			dir = randint(9);
		}

		/* Note failure */
		if (i == 100)
		{
			y = x_ptr->y0;
			x = x_ptr->x0;
		}

		/* Move the vortex */
		x_ptr->y0 = y;
		x_ptr->x0 = x;

		/* Zap the new grid */
		(void)do_effect_linger(x_idx, x_ptr->y0, x_ptr->x0);
	}


	/* Clouds of death */
	else if (x_ptr->index == EFFECT_DEATH_CLOUD)
	{
		/* Player's current location is not the center of the cloud */
		if ((p_ptr->py != x_ptr->y0) || (p_ptr->px != x_ptr->x0))
		{
			int dist = distance(p_ptr->py, p_ptr->px, x_ptr->y0, x_ptr->x0);

			/* If distance between the two is > 1, weaken the cloud */
			if (dist > 1)
			{
				/* Too much disruption -- kill the cloud */
				if (dist >= x_ptr->power)
				{
					msg_print("Your violent motions tear apart the cloud of death.");
					x_ptr->age = 250;
					return;
				}

				/* Weaken the cloud */
				else
				{
					x_ptr->power -= dist;
				}
			}

			/* Re-center the cloud */
			x_ptr->y0 = p_ptr->py;
			x_ptr->x0 = p_ptr->px;
		}

		/* Fire a star-shaped cloud of death */
		fire_star(GF_DEATH_CLOUD, 0, randint(x_ptr->power), x_ptr->power2);
	}

	/* Effects age */
	x_ptr->age++;
}


/*
 * Process effects.
 */
void process_effects(void)
{
	int i;

	/* Process all effects */
	for (i = 0; i < z_info->x_max; i++)
	{
		/* Get this effect */
		effect_type *x_ptr = &x_list[i];

		/* Skip empty effects */
		if (!x_ptr->index) continue;

		/* Process effect */
		process_effect(i);
	}
}

