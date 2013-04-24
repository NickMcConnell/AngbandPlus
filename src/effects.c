/* File: effects.c */

/*
 * Special lingering spell effects.
 *
 * Copyright (c) 2002
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
 * Zap a grid.  Allow mere display of color or actual spell effects.
 */
static bool do_effect_grid(int x_idx, int y, int x, bool for_real)
{
	/* Get this effect */
	effect_type *x_ptr = &x_list[x_idx];

	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_PLAY | PROJECT_ITEM |
	           PROJECT_GRID;

	bool wall = FALSE;

	/* Note walls */
	if (cave_info[y][x] & (CAVE_WALL)) wall = TRUE;

#if 0
	/* Handle various flags */
	if (x_ptr->flags)
	{
		/* Affect only evil */
		if (x_ptr->flags & (0x0001)) p_ptr->proj_mon_flags = (RF3_EVIL);
	}
#endif

	/* Zap the grid */
	if (for_real)
	{
		/* Has hit the character */
		if ((y == p_ptr->py) && (x == p_ptr->px))
		{
			/* Print messages  XXX */
			switch (x_ptr->type)
			{
				case GF_FIRE: 
				case GF_HEAT: 
				case GF_PLASMA:  
					/* msg_print("You are enveloped in fire!");  */ break;
				case GF_ROCK: 
				case GF_EARTH: 
				case GF_SHARDS:
					/*  msg_print("You are surrounded by rocks!");*/  break;
				case GF_GUST: 
				case GF_WIND: 
				case GF_GALE: 
					/*  msg_print("You are enveloped in winds!");*/  break;
				case GF_RUST: 
				case GF_STEAM: 
				case GF_STORM: 
					/*  msg_print("You are enveloped in water!");*/  break;
				case GF_SHOCK: 
				case GF_ELEC: 
				case GF_VOLT:
					/*   msg_print("You are covered lightning!");*/  break;
				case GF_CHILL: 
				case GF_ICE: 
				case GF_GLACIAL:
					/*   msg_print("You are sheathed in frost!");*/  break;
				case GF_CORROSIVE: 
				case GF_ACID: 
				case GF_LIQUESCE:
					/*   msg_print("You are surrounded by acid!");*/  break;
				case GF_CAUSTIC: 
				case GF_POISON: 
				case GF_CONTAGION:
					/*   msg_print("You are surrounded by poison!");*/  break;
				case GF_AGE: 
				case GF_TIME: 
				case GF_CHRONOS:
					/*   msg_print("You are enveloped in a taychon field!");*/  break;
				case GF_VAPOR: 
				case GF_ETHER: 
				case GF_NEXUS:
					/*   msg_print("You are enveloped by ether!");*/  break;
				case GF_VIBE: 
				case GF_SOUND: 
				case GF_SONIC:
					/*   msg_print("You are enveloped in a sonic field!");*/  break;
				case GF_UNHOLY: 
				case GF_NETHER: 
				case GF_ABYSS:
					/*   msg_print("You are surrounded by dark forces!");*/  break;
				case GF_SPIRIT: /* msg_print("You feel spirits pass through you!"); */ break;
				default:  /* msg_print("You are hit!");*/  break;
			}
		}

		/* Practice a skill */
		/* if (x_ptr->practice_skill) skill_being_used = x_ptr->practice_skill; */

		/* Cast the spell */
		(void)project(-1, 0, y, x, y, x, x_ptr->power, x_ptr->type, flg, 0, 0);
	}

	/* Display lingering spell colors (require ability to see) */
	if (player_can_see_bold(y, x))
	{
		byte a[4];

		/* Translate the 16 basic colors to binary values */
		switch (spell_color(x_ptr->type))
		{
			case TERM_WHITE:   a[0] = 0; a[1] = 0; a[2] = 0; a[3] = 1; break;
			case TERM_SLATE:   a[0] = 0; a[1] = 0; a[2] = 1; a[3] = 0; break;
			case TERM_ORANGE:  a[0] = 0; a[1] = 0; a[2] = 1; a[3] = 1; break;
			case TERM_RED:     a[0] = 0; a[1] = 1; a[2] = 0; a[3] = 0; break;
			case TERM_GREEN:   a[0] = 0; a[1] = 1; a[2] = 0; a[3] = 1; break;
			case TERM_BLUE:    a[0] = 0; a[1] = 1; a[2] = 1; a[3] = 0; break;
			case TERM_UMBER:   a[0] = 0; a[1] = 1; a[2] = 1; a[3] = 1; break;
			case TERM_L_DARK:  a[0] = 1; a[1] = 0; a[2] = 0; a[3] = 0; break;
			case TERM_L_WHITE: a[0] = 1; a[1] = 0; a[2] = 0; a[3] = 1; break;
			case TERM_VIOLET:  a[0] = 1; a[1] = 0; a[2] = 1; a[3] = 0; break;
			case TERM_YELLOW:  a[0] = 1; a[1] = 0; a[2] = 1; a[3] = 1; break;
			case TERM_L_RED:   a[0] = 1; a[1] = 1; a[2] = 0; a[3] = 0; break;
			case TERM_L_GREEN: a[0] = 1; a[1] = 1; a[2] = 0; a[3] = 1; break;
			case TERM_L_BLUE:  a[0] = 1; a[1] = 1; a[2] = 1; a[3] = 0; break;
			case TERM_L_UMBER: a[0] = 1; a[1] = 1; a[2] = 1; a[3] = 1; break;

			default:           a[0] = 0; a[1] = 0; a[2] = 0; a[3] = 0; break;
		}

		/* Translate to flags, mark the dungeon */
		if (a[0]) cave_info[y][x] |= (CAVE_ATT1);
		if (a[1]) cave_info[y][x] |= (CAVE_ATT2);
		if (a[2]) cave_info[y][x] |= (CAVE_ATT3);
		if (a[3]) cave_info[y][x] |= (CAVE_ATT4);

		/* Redraw this grid */
		lite_spot(y, x);
	}

	/* Return */
	return (!wall);
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
	if (x_ptr->flags & (0x8000)) return;


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
	x_ptr->flags |= (0x8000);
}


/*
 * Redraw an effect.   XXX XXX
 */
static void redraw_effect(int x_idx)
{
	int y, x;

	/* Get this effect */
	effect_type *x_ptr = &x_list[x_idx];

	/* The lingering graphics flags */
	u16b flg = CAVE_ATT1 | CAVE_ATT2 | CAVE_ATT3 | CAVE_ATT4;

	/* Clear all lingering spell effects near center XXX XXX */
	for (y = x_ptr->y0 - 10; y < x_ptr->y0 + 10; y++)
	{
		for (x = x_ptr->x0 - 10; x < x_ptr->x0 + 10; x++)
		{
			if (in_bounds(y, x))
			{
				if (cave_info[y][x] & (flg))
				{
					cave_info[y][x] &= ~(flg);
					lite_spot(y, x);
				}
			}
		}
	}
}



/*
 * Process an individual effect.
 */
static void process_effect(int x_idx)
{
	/* Get this effect */
	effect_type *x_ptr = &x_list[x_idx];

	/* Assume that we're just refreshing the graphics */
	bool for_real = FALSE;

	int path_n = 0;
	u16b path_g[256];

	int i, j, axis;
	int y, x;
	int y0, x0;

	int grids = 0;
	int walls = 0;


	/* Paranoia -- no processing dead effects */
	if (!x_ptr->index) return;


	/* Decrement turn counter */
	if (x_ptr->time_count > 0) x_ptr->time_count--;

	/* This effect can take a "turn" */
	if (x_ptr->time_count == 0)
	{
		/* Do it for real */
		for_real = TRUE;

		/* Reset count */
		x_ptr->time_count = x_ptr->time_delay;
	}

	/* If the effect has recently been "born", make various tweaks to it */
	if (x_ptr->age == 0) adjust_effect(x_idx);



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

		/* Actual attack */
		if (for_real)
		{
			/* Add the kill flags */
			flg |= PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

			dam = x_ptr->power;

			/* Cast a star */
			(void)project(-1, rad, y, x, y, x, dam, typ, flg, 0, 0);
		}
	}


	if (x_ptr->index == EFFECT_SPHERE)
	{
		/* We always fire a star and refresh the graphics */
		u32b flg = PROJECT_BOOM;

		int dam = 0;
		int rad = x_ptr->power2;
		int typ = x_ptr->type;

		y = x_ptr->y0;
		x = x_ptr->x0;

		/* Actual attack */
		if (for_real)
		{
			/* Add the kill flags */
			flg |= PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

			dam = x_ptr->power;

			/* Cast a star */
			(void)project(-1, rad, y, x, y, x, dam, typ, flg, 0, 0);
		}
	}

	/* Advancing walls */
	else if (x_ptr->index == EFFECT_WALL)
	{
		/* Get the direction of travel (angular dir is 2x this) */
		int major_axis =
			get_angle_to_target(x_ptr->y0, x_ptr->x0, x_ptr->y1, x_ptr->x1, 0);

		/* Get the directions of spread (angular dir is 2x this)*/
		int minor_axis1 = (major_axis +  45) % 180;
		int minor_axis2 = (major_axis + 135) % 180;

		/* Get the length of the wall on either side of the major axis */
		int spread = 1 + x_ptr->power / 33;
		if (spread > 8) spread = 8;

		/* Store target grid */
		y = x_ptr->y1;
		x = x_ptr->x1;

		/* Calculate the projection path -- require orthogonal movement */
		path_n = project_path(path_g, 99, x_ptr->y0, x_ptr->x0,
			&y, &x, PROJECT_PASS | PROJECT_THRU | PROJECT_ORTH);

		/* Require a working projection path */
		if (path_n > x_ptr->age)
		{
			/* Get center grid (walls travel one grid per turn) */
			y0 = GRID_Y(path_g[x_ptr->age]);
			x0 = GRID_X(path_g[x_ptr->age]);

			/* Count grid */
			grids++;

			/* Zap grid, count walls */
			if (!do_effect_grid(x_idx, y0, x0, for_real)) walls++;


			/* Process the left, then the right-hand sides of the wall */
			for (i = 0; i < 2; i++)
			{
				if (i == 0) axis = minor_axis1;
				else        axis = minor_axis2;

				/* Get the target grid for this side */
				get_grid_using_angle(axis, y0, x0, &y, &x);

				/* If we have a legal target, */
				if ((y != y0) || (x != x0))
				{
					/* Calculate the path of projection */
					path_n = project_path(path_g, spread, y0, x0, &y, &x,
						PROJECT_PASS | PROJECT_THRU);

					/* Zap all the grids */
					for (j = 0; j < path_n; j++)
					{
						/* Get grid */
						y = GRID_Y(path_g[j]);
						x = GRID_X(path_g[j]);

						/* Count grid */
						grids++;

						/* Zap grid, count walls */
						if (!do_effect_grid(x_idx, y, x, for_real)) walls++;
					}
				}
			}
		}

		/* If the effect is not just being refreshed */
		if (for_real)
		{
			/* Wall spells die if they hit too many walls at once */
			if ((walls >= 3) && (walls > grids / 2)) x_ptr->lifespan = 0;

			/* Walls spells weaken when they hit walls */
			else if (walls > grids / 3)
				x_ptr->power = x_ptr->power * grids / (grids + walls);
		}
	}


	/* Advancing walls */
	else if (x_ptr->index == EFFECT_STATIC_WALL)
	{
		/* Get the direction of travel (angular dir is 2x this) */
		int major_axis =
			get_angle_to_target(x_ptr->y0, x_ptr->x0, x_ptr->y1, x_ptr->x1, 0);

		/* Get the directions of spread (angular dir is 2x this)*/
		int minor_axis1 = (major_axis +  45) % 180;
		int minor_axis2 = (major_axis + 135) % 180;

		/* Get the length of the wall on either side of the major axis */
		int spread = 1 + x_ptr->power / 33;
		if (spread > 8) spread = 8;

		/* Store target grid */
		y = x_ptr->y1;
		x = x_ptr->x1;
#if 0
		/* Calculate the projection path -- require orthogonal movement */
		path_n = project_path(path_g, 99, x_ptr->y0, x_ptr->x0,
			&y, &x, PROJECT_PASS | PROJECT_THRU | PROJECT_ORTH);

#endif
		/* Require a working projection path */
		if (x_ptr->age)
		{
			/* Get center grid (walls travel one grid per turn) */
			y0 = y;
			x0 = x;

			/* Count grid */
			grids++;

			/* Zap grid, count walls */
			if (!do_effect_grid(x_idx, y0, x0, for_real)) walls++;


			/* Process the left, then the right-hand sides of the wall */
			for (i = 0; i < 2; i++)
			{
				if (i == 0) axis = minor_axis1;
				else        axis = minor_axis2;

				/* Get the target grid for this side */
				get_grid_using_angle(axis, y0, x0, &y, &x);

				/* If we have a legal target, */
				if ((y != y0) || (x != x0))
				{
					/* Calculate the path of projection */
					path_n = project_path(path_g, spread, y0, x0, &y, &x,
						PROJECT_PASS | PROJECT_THRU);

					/* Zap all the grids */
					for (j = 0; j < path_n; j++)
					{
						/* Get grid */
						y = GRID_Y(path_g[j]);
						x = GRID_X(path_g[j]);

						/* Count grid */
						grids++;

						/* Zap grid, count walls */
						if (!do_effect_grid(x_idx, y, x, for_real)) walls++;
					}
				}
			}
		}

		/* If the effect is not just being refreshed */
		if (for_real)
		{
			/* Wall spells die if they hit too many walls at once */
			if ((walls >= 3) && (walls > grids / 2)) x_ptr->lifespan = 0;

			/* Walls spells weaken when they hit walls */
			else if (walls > grids / 3)
				x_ptr->power = x_ptr->power * grids / (grids + walls);
		}
	}

	/* Seeker vortexes */
	else if (x_ptr->index == EFFECT_SEEKER_VORTEX)
	{
		int dir;
		int ty = 0, tx = 0;

		/* If refreshing the graphics, zap the current grid */
		if (!for_real)
		{
			(void)do_effect_grid(x_idx, x_ptr->y0, x_ptr->x0, for_real);
		}

		/* Move the vortex */
		else
		{
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
				/* Try to get a target */
				get_closest_los_monster(1, x_ptr->y0, x_ptr->x0, &ty, &tx, FALSE);

			}

			/* No valid target, or monster is in an impassable wall */
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
			(void)do_effect_grid(x_idx, x_ptr->y0, x_ptr->x0, for_real);
		}
	}
	/* Seeker vortexes */
	else if (x_ptr->index == EFFECT_SPIRIT_VORTEX)
	{
		int dir;
		int ty = 0, tx = 0;

		/* If refreshing the graphics, zap the current grid */
		if (!for_real)
		{
			(void)do_effect_grid(x_idx, x_ptr->y0, x_ptr->x0, for_real);
		}

		/* Move the vortex */
		else
		{
			/* Check around (and under) the vortex */
			for (dir = 0; dir < 9; dir++)
			{
				/* Extract adjacent (legal) location */
				y = x_ptr->y0 + ddy_ddd[dir];
				x = x_ptr->x0 + ddx_ddd[dir];

				/* Count passable grids */
				if (!cave_perma_bold(y, x)) grids++;
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
				/* Try to get a target */
				get_closest_los_monster(1, x_ptr->y0, x_ptr->x0, &ty, &tx, FALSE);

			}

			/* No valid target, or monster is in an impassable wall */
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
				if (!cave_perma_bold(y, x))
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
			(void)do_effect_grid(x_idx, x_ptr->y0, x_ptr->x0, for_real);
		}
	}

	/* Clouds of death */
	else if (x_ptr->index == EFFECT_DEATH_CLOUD)
	{
		int typ = x_ptr->type;
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
					x_ptr->index = 0;
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

		if (for_real)
		{
			/* Fire a star-shaped cloud of death */
			fire_star(typ, 0, randint(x_ptr->power), x_ptr->power2);
		}
	}

	/* Whirlpool vortexes */
	else if (x_ptr->index == EFFECT_WHIRLPOOL)
	{
		int cdir, direction;
		int ty = 0, tx = 0;

		/* If refreshing the graphics, zap the current grid */
		if (!for_real)
		{
			(void)do_effect_grid(x_idx, x_ptr->y0, x_ptr->x0, for_real);
		}

		/* Move the vortex */
		else
		{
			cdir = x_ptr->age;
						
			direction = randint(20);
						
			if (p_ptr->depth > 25)		
			{
				if (direction > 1)
				{	/* Extract adjacent (legal) location */
					y = x_ptr->y0 + ddy_cdd[(cdir + 1) % 8];
					x = x_ptr->x0 + ddx_cdd[(cdir + 1) % 8];
				}
				else 
				{
					/* Extract adjacent (legal) location */
					y = x_ptr->y0 + ddy_cdd[(cdir - 1) % 8];
					x = x_ptr->x0 + ddx_cdd[(cdir - 1) % 8];
				}
			}
			else 
			{
				if (direction > 1)
				{	/* Extract adjacent (legal) location */
					y = x_ptr->y0 + ddy_cdd[(cdir - 1) % 8];
					x = x_ptr->x0 + ddx_cdd[(cdir - 1) % 8];
				}
				else 
				{
					/* Extract adjacent (legal) location */
					y = x_ptr->y0 + ddy_cdd[(cdir + 1) % 8];
					x = x_ptr->x0 + ddx_cdd[(cdir + 1) % 8];
				}
			}
			if (cave_perma_bold(y, x) || !cave_passable_bold(y, x)) 
			{
				y = x_ptr->y0;
				x = x_ptr->x0;
			}
			
			/* Move the vortex */
			x_ptr->y0 = y;
			x_ptr->x0 = x;

			/* Zap the new grid */
			(void)do_effect_grid(x_idx, x_ptr->y0, x_ptr->x0, for_real);
		}
	}


	/* Handle aging on active turns only */
	if (for_real)
	{
		/* Effects age */
		x_ptr->age++;

		/* Effects can "die" */
		if (x_ptr->age >= x_ptr->lifespan)
		{
			/* Special message for clouds of death (they have no graphics) */
			if (x_ptr->index == EFFECT_DEATH_CLOUD)
			{
				msg_print("The cloud of death dissipates.");
			}

			/* Effect is "dead" */
			x_ptr->index = 0;

			/* Hack -- erase the graphics */
			redraw_effect(x_idx);
		}
	}
}

/*
 * Process effects.
 */
void process_effects(void)
{
	int i;

	/* Scan all effects -- graphics */
	for (i = 0; i < z_info->x_max; i++)
	{
		/* Get this effect */
		effect_type *x_ptr = &x_list[i];

		/* Skip empty effects */
		if (!x_ptr->index) continue;

		/* Clean up lingering graphics */
		redraw_effect(i);
	}

	/* Scan all effects -- actions */
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

