/* File: spells2.c */

/*
 * Projection types (specialized, simple).  Handle the "temp" array.
 * Teleport monsters and the character.  Character-effect magics:  Burn,
 * freeze, melt, and electrocute you and your inventory, apply disenchant-
 * ment and nexus.  Healing.  Increase, decrease, restore, shuffle stats.
 * Inflict disease, forget, cursing.  Monster-effect magics:  hurt, heal,
 * hinder, aggravate, genocide, probing.  Detection spells.  Dungeon-effect
 * magics:  destruction, light and unlight rooms, other spells.  Weather.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"




/************************************************************************
 *                                                                      *
 *                           Projection types                           *
 *                                                                      *
 ************************************************************************/


/*
 * Handle bolt spells.
 *
 * Bolts stop as soon as they hit a monster, whiz past missed targets, and
 * (almost) never affect items on the floor.
 */
bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg)
{
	/* Add the bolt bitflags */
	flg |= PROJECT_STOP | PROJECT_KILL | PROJECT_THRU;

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= PROJECT_PLAY;

	/* Limit range */
	if ((rad > MAX_RANGE) || (rad <= 0)) rad = MAX_RANGE;

	/* Cast a bolt */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg, 0, 0));
}

/*
 * Handle beam spells.
 *
 * Beams affect every grid they touch, go right through monsters, and
 * (almost) never affect items on the floor.
 */
bool project_beam(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg)
{
	/* Add the beam bitflags */
	flg |= PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU;

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Limit range */
	if ((rad > MAX_RANGE) || (rad <= 0)) rad = MAX_RANGE;

	/* Cast a beam */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg, 0, 0));
}


/*
 * Handle ball spells.
 *
 * Balls act like bolt spells, except that they do not pass their target,
 * and explode when they hit a monster, a wall, their target, or the edge
 * of sight.  Within the explosion radius, they affect items on the floor.
 *
 * Balls may jump to the target, and have any source diameter (which affects
 * how quickly their damage falls off with distance from the center of the
 * explosion).
 */
bool project_ball(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg, int source_diameter)
{
	/* Add the ball bitflags */
	flg |= PROJECT_BOOM | PROJECT_GRID |
	       PROJECT_ITEM | PROJECT_KILL;

	/* Add the STOP flag if appropriate */
	if ((who < 0) &&
	    (!target_okay() || y1 != p_ptr->target_row || x1 != p_ptr->target_col))
	{
		flg |= (PROJECT_STOP);
	}

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Limit radius to nine (up to 256 grids affected) */
	if (rad > 9) rad = 9;

	/* Cast a ball */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg,
	                0, source_diameter));
}

/*
 * Handle ball spells that explode immediately on the target and
 * hurt everything.
 */
bool explosion(int who, int rad, int y0, int x0, int dam, int typ)
{
	/* Add the explosion bitflags */
	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_JUMP |
	           PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;

	/* Explode */
	return (project(who, rad, y0, x0, y0, x0, dam, typ, flg, 0, 0));
}

/*
 * Handle monster-centered explosions.
 */
bool mon_explode(int who, int rad, int y0, int x0, int dam, int typ)
{
	return (project_ball(who, rad, y0, x0, y0, x0, dam, typ, 0L, 20));
}


/*
 * Handle arc spells.
 *
 * Arcs are a pie-shaped segment (with a width determined by "degrees")
 * of an explosion outwards from the source grid.  They are centered
 * along a line extending from the source towards the target.  -LM-
 *
 * There are 360 degrees in a circle (until later translation).
 *
 * Because all arcs start out as being one grid wide, arc spells with a
 * value for degrees of arc less than (roughly) 60 do not dissipate as
 * quickly.  In the extreme case where degrees of arc is 0, the arc is
 * actually a defined length beam, and loses no strength at all over the
 * ranges found in the game.
 *
 * Arcs affect items on the floor.
 */
bool project_arc(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                 int typ, u32b flg, int degrees)
{
	/* Diameter of source of energy is normally, but not always, 20. */
	int source_diameter = 20;

	/* Calculate the effective diameter of the energy source, if necessary. */
	if (degrees < ARC_STANDARD_WIDTH)
	{
		if (degrees <= 9) source_diameter = rad * 10;
		else source_diameter = source_diameter * ARC_STANDARD_WIDTH / degrees;
	}

	/* If the arc has no spread, it's actually a beam */
	if (degrees <= 0)
	{
		/* Add the beam bitflags */
		flg |= (PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU);

		source_diameter = 0;
	}

	/* If a full circle is asked for, we cast a ball spell. */
	else if (degrees >= 360)
	{
		/* Add the ball bitflags */
		flg |= PROJECT_STOP | PROJECT_BOOM | PROJECT_GRID |
		       PROJECT_ITEM | PROJECT_KILL;

		source_diameter = 0;
	}

	/* Otherwise, we fire an arc */
	else
	{
		/* Add the arc bitflags */
		flg |= PROJECT_ARC  | PROJECT_BOOM | PROJECT_GRID |
		       PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU;
	}

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Cast an arc (or a ball) */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg, degrees,
	                (byte)source_diameter));
}

/*
 * Handle starburst spells.
 *
 * Starbursts are randomized balls that use the same sort of code that
 * governs the shape of starburst rooms in the dungeon.  -LM-
 *
 * Starbursts always do full damage to every grid they affect:  however,
 * the chances of affecting grids drop off significantly towards the
 * edge of the starburst.
 */
bool project_star(int who, int rad, int y0, int x0, int y1, int x1, int dam,
	int typ, u32b flg)
{
	/* Add the star bitflags (including no visible trail to the target) */
	flg |= PROJECT_STAR | PROJECT_BOOM | PROJECT_GRID | PROJECT_NO_TRAIL |
	       PROJECT_ITEM | PROJECT_KILL;

	/* Add the STOP flag if appropriate */
	if ((who < 0) &&
	    (!target_okay() || y1 != p_ptr->target_row || x1 != p_ptr->target_col))
	{
		flg |= (PROJECT_STOP);
	}

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Cast a star */
	return (project(who, rad, y0, x0, y1, x1, dam, typ, flg, 0, 0));
}

/*
 * Handle target grids for projections under the control of
 * the character.  - Chris Wilde, Morgul
 */
void find_target(int dir, int range, int y0, int x0, int *y1, int *x1)
{
	/* If no direction is given, and a target is, use the target. */
	if ((dir == 5) && target_okay())
	{
		*y1 = p_ptr->target_row;
		*x1 = p_ptr->target_col;
	}

	/* Otherwise, use the given direction */
	else
	{
		*y1 = y0 + range * ddy[dir];
		*x1 = x0 + range * ddx[dir];
	}
}

/*
 * Handle target grids for projections under the control of
 * the character.  - Chris Wilde, Morgul
 */
static void adjust_target(int dir, int y0, int x0, int *y1, int *x1)
{
	find_target(dir, MAX_RANGE, y0, x0, y1, x1);
}


/*
 * Character casts a bolt spell.
 */
bool fire_bolt(int typ, int dir, int dam)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a bolt */
	return (project_bolt(-1, MAX_RANGE, p_ptr->py, p_ptr->px, y1, x1, dam,
	                     typ, 0L));
}

/*
 * Character casts a beam spell.
 */
bool fire_beam(int typ, int dir, int dam)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a beam */
	return (project_beam(-1, MAX_RANGE, p_ptr->py, p_ptr->px, y1, x1, dam,
	                     typ, 0L));
}

/*
 * Cast a bolt or a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
	if (rand_int(100) < prob)
	{
		return (fire_beam(typ, dir, dam));
	}
	else
	{
		return (fire_bolt(typ, dir, dam));
	}
}

/*
 * Character casts a special-purpose bolt or beam spell.
 */
bool fire_bolt_beam_special(int typ, int dir, int dam, int rad, u32b flg)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* This is a beam spell */
	if (flg & (PROJECT_BEAM))
	{
		/* Cast a beam */
		return (project_beam(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dam,
	                        typ, flg));
	}

	/* This is a bolt spell */
	else
	{
		/* Cast a bolt */
		return (project_bolt(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dam,
									typ, flg));
	}
}

/*
 * Character casts a (simple) ball spell.
 */
bool fire_ball(int typ, int dir, int dam, int rad)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a (simple) ball */
	return (project_ball(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                     0L, 0));
}

/*
 * Character casts an orb spell (a ball that loses no strength out
 * from the origin).
 */
bool fire_orb(int typ, int dir, int dam, int rad)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast an orb */
	return (project_ball(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                     0L, 10 + rad * 10));
}

/*
 * Character casts a ball spell with a specified source diameter, that
 * jumps to the target, or does various other special things.
 */
bool fire_ball_special(int typ, int dir, int dam, int rad, u32b flg,
                       int source_diameter)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a ball with specified source diameter */
	return (project_ball(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                     flg, source_diameter));
}

/*
 * Character casts an arc spell.
 */
bool fire_arc(int typ, int dir, int dam, int rad, int degrees)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast an arc */
	return (project_arc(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ,
	                    0L, degrees));
}

/*
 * Character casts a star-shaped spell.
 */
bool fire_star(int typ, int dir, int dam, int rad)
{
	int y1, x1;

	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &y1, &x1);

	/* Cast a star */
	return (project_star(-1, rad, p_ptr->py, p_ptr->px, y1, x1, dam, typ, 0L));
}


/*
 * Fire a number of bolts, beams, or arcs that start in semi-random grids
 * near the character, and head in totally random directions.  The larger
 * the number of grids in the area of fire, and the more monsters inhabit
 * those grids, the more effective this spell is.
 * -LM-
 */
void fire_storm(int who, int typ0, int y0, int x0, int dam, int rad, int len,
	byte projection, bool lingering)
{
	int i, j;
	int y, x, y1, x1, last_y, last_x;
	int dir;
	int typ;
	long num_missiles;
	int choice;
	monster_type *m_ptr;

	/* Save standard delay */
	int std_delay = op_ptr->delay_factor;

	/* Array of grids (max radius is 20) */
	u16b grid[1681];

	/* Grid count */
	int grid_count = 0;

	/* Array of monsters to hurt (indexes, initial HPs) */
	s16b mon_array[100][2];

	/* Monster count */
	int mon_count = 0;

	/* Allow spell graphics to accumulate */
	u32b flg = (lingering ? PROJECT_NO_REDRAW : 0L);


	/* We can't handle a radius of more than 20 */
	if (rad > 20) rad = 20;

	/* Very little delay while projecting each missile */
	op_ptr->delay_factor = (std_delay + 1) / 2;


	/* Build up an array of all nearby projectable grids */
	for (y = y0 - rad; y <= y0 + rad; y++)
	{
		for (x = x0 - rad; x <= x0 + rad; x++)
		{
			/* Stay legal */
			if (!in_bounds(y, x)) continue;

			/* Require that grid be projectable */
			if (projectable(y0, x0, y, x, 0L))
			{
				/* Convert location to a grid, save and count it */
				grid[grid_count++] = GRID(y, x);
			}
		}
	}


	/* Scan the monster list */
	for (i = 0; i < m_max; i++)
	{
		/* Get this monster */
		m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip monsters not in LOF (or LOS), and if too far away */
		if ((y0 == p_ptr->py) && (x0 == p_ptr->px))
		{
			if (!player_can_fire_bold(m_ptr->fy, m_ptr->fx)) continue;
			if (m_ptr->cdis > rad) continue;
		}
		else
		{
			if (!los(y0, x0, m_ptr->fy, m_ptr->fx)) continue;
			if (distance(y0, x0, m_ptr->fy, m_ptr->fx) > rad) continue;
		}

		/* Store this monster and its current HPs */
		if (mon_count < 100)
		{
			mon_array[mon_count][0] = i;
			mon_array[mon_count][1] = m_ptr->hp;
			mon_count++;
		}
	}


	/* Calculate the minimum number of missiles */
	num_missiles = MAX(1L, grid_count / 8);


	/* Handle each missile in turn */
	for (i = 0;; i++)
	{
		/* Limit -- never fire more than num_missiles * 8 */
		if (i > num_missiles * 8) break;

		/* We've used up our guaranteed missiles */
		if (i >= num_missiles)
		{
			/* Assume we stop */
			bool stop = TRUE;

			/* Keep firing until all monsters have been hurt */
			for (j = 0; j < mon_count; j++)
			{
				/* Get this monster */
				m_ptr = &m_list[mon_array[j][0]];

				/* Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				/* Skip monsters with HPs < initial value */
				if (m_ptr->hp < mon_array[j][1]) continue;

				/* This monster hasn't been hurt - keep firing */
				stop = FALSE;
				break;
			}

			/* Stop if all monsters have been hurt */
			if (stop) break;
		}


		/* Start with a very far away location */
		last_y = -255;
		last_x = -255;

		/* Bias for closer grids */
		for (j = 0; j < 3; j++)
		{
			/* Choose a grid at random */
			choice = rand_int(grid_count);

			/* Get the coordinates */
			y = GRID_Y(grid[choice]);
			x = GRID_X(grid[choice]);

			/* Save if less than previous distance */
			if (distance(y, x, y0, x0) < distance(last_x, last_x, y0, x0))
			{
				/* Save these coordinates */
				last_y = y;
				last_x = x;
			}
		}

		/* No movement */
		dir = 5;

		/* Get any direction other than 5 */
		while (dir == 5) dir = randint(9);

		/* Get target grid */
		y1 = last_y + ddy[dir];
		x1 = last_x + ddx[dir];


		/* Allow wizardly projection types */
		if (typ0 == -1)
		{
			choice = rand_int(12);

			if      (choice ==  1) typ = GF_FIRE;
			else if (choice ==  2) typ = GF_COLD;
			else if (choice ==  3) typ = GF_ACID;
			else if (choice ==  4) typ = GF_ELEC;
			else if (choice ==  5) typ = GF_POIS;
			else if (choice ==  6) typ = GF_LITE;
			else if (choice ==  7) typ = GF_DARK;
			else if (choice ==  8) typ = GF_NEXUS;
			else if (choice ==  9) typ = GF_CONFUSION;
			else if (choice == 10) typ = GF_SOUND;
			else if (choice == 11) typ = GF_SHARD;
			else                   typ = GF_CHAOS;
		}

		/* Allow light, darkness, and confusion */
		else if (typ0 == -2)
		{
			choice = rand_int(3);

			if      (choice == 1) typ = GF_LITE;
			else if (choice == 2) typ = GF_DARK;
			else                  typ = GF_CONFUSION;
		}

		/* Use given projection */
		else
		{
			typ = typ0;
		}

		/* Fire a projection using the calculated data */
		if (projection == 0)
		{
			(void)project_bolt(who, len, last_y, last_x, y1, x1, dam, typ, flg);
		}
		else if (projection == 1)
		{
			(void)project_beam(who, len, last_y, last_x, y1, x1, dam, typ, flg);
		}
		else if (projection == 2)
		{
			/* Used for the "Prismatic Armageddon" spell */
			(void)project_arc(who, rand_range(len - 1, len + 1), last_y, last_x,
				y1, x1, dam, typ, flg, rand_range(40, 55));
		}
		else if (projection == 3)
		{
			(void)project_ball(who, rad, y1, x1, y1, x1, dam, typ, flg, 0);
		}
	}

	/* We allowed spell graphics to accumulate */
	if (lingering)
	{
		/* Clear all lingering spell effects on screen XXX */
		for (y = p_ptr->wy; y < p_ptr->wy + map_rows; y++)
		{
			for (x = p_ptr->wx; x < p_ptr->wx + map_cols; x++)
			{
				lite_spot(y, x);
			}
		}
	}

	/* Restore standard delay */
	op_ptr->delay_factor = std_delay;
}


/*
 * Fire beams in random directions.
 */
bool beam_burst(int y, int x, int typ, int num, int dam)
{
	int i, yy, xx;

	bool notice = FALSE;

	int old_delay = op_ptr->delay_factor;

	/* Require legal centerpoint */
	if (!in_bounds_fully(y, x)) return (FALSE);


	/* Hack -- lower delay factor */
	if (op_ptr->delay_factor)
	{
		op_ptr->delay_factor = (op_ptr->delay_factor + 1) / 2;
	}

	/* Fire beams in all directions */
	for (i = 0; i < num; i++)
	{
		/* Get a totally random grid within six grids from current position */
		yy = rand_spread(y, 6);
		xx = rand_spread(x, 6);

		/* Fire a beam of (strong) light towards it */
		if (project(-1, 0, y, x, yy, xx, dam, typ,
			PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID, 0, 0)) notice = TRUE;
	}

	/* Restore standard delay */
	op_ptr->delay_factor = old_delay;

	/* Return "anything noticed" */
	return (notice);
}






/*
 * Apply a "project()" directly to all monsters in view of a certain spot.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * We are able to check LOS from either the character (in which case we
 * use line of fire for speed and accuracy), or from any given grid.
 *
 * To avoid misbehavior when monster deaths have side-effects,
 * this is done in two passes. -- JDL
 */
bool project_los(int y0, int x0, int dam, int typ)
{
	int i, d, x, y;

	u32b saved_proj_mon_flags = p_ptr->proj_mon_flags;
	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool obvious = FALSE;

	/* Determine whether we are using LOF or LOS */
	bool line_of_fire = FALSE;

	if ((y0 == p_ptr->py) && (x0 == p_ptr->px)) line_of_fire = TRUE;


	/* Mark monsters in LOS */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Apply character-centered test */
		if (line_of_fire)
		{
			/* Require line of fire */
			if (!player_can_fire_bold(y, x)) continue;
		}

		/* Apply generic grid test */
		else
		{
			/* Get distance between source and monster */
			d = distance(y0, x0, y, x);

			/* LOS extends only to max sight range */
			if (d > MAX_RANGE) continue;

			/* Check LOS if not at grid or adjacent */
			if (d > 1)
			{
				/* Ignore if not in LOS */
				if (!los(y0, x0, y, x)) continue;
			}
		}

		/* Mark the monster */
		m_ptr->mflag |= (MFLAG_TEMP);
	}

	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip unmarked monsters */
		if (!(m_ptr->mflag & (MFLAG_TEMP))) continue;

		/* Remove mark */
		m_ptr->mflag &= ~(MFLAG_TEMP);

		/* Restore projection limitations */
		p_ptr->proj_mon_flags = saved_proj_mon_flags;

		/* Jump directly to the monster */
		if (project(-1, 0, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx,
		            dam, typ, flg, 0, 0))
		{
			obvious = TRUE;
		}
	}

	/* Result */
	return (obvious);
}




/************************************************************************
 *                                                                      *
 *                             Teleportation                            *
 *                                                                      *
 ************************************************************************/



/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 *
 * Note that this function will not teleport monsters into terrain that
 * might hurt them (water and lava in particular).  This may be a little
 * overgenerous.
 * Monsters can be teleported into vaults.
 */
void teleport_away(int m_idx, int dis, bool require_los)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int d, i, min;
	int ny, nx, oy, ox;

	bool look = TRUE;


	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (TRUE)
			{
				ny = rand_spread(oy, dis);
				nx = rand_spread(ox, dis);
				d = distance(oy, ox, ny, nx);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(ny, nx)) continue;

			/* Optionally, ignore grids not in LOS */
			if ((require_los) && (!los(oy, ox, ny, nx))) continue;

			/* Require a grid that the monster can exist in */
			if (!cave_exist_mon(r_ptr, ny, nx, FALSE, FALSE)) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis *= 2;

		/* Decrease the minimum distance */
		min /= 2;
	}

	/* Sound */
	sound(MSG_TPOTHER);

	/* Swap the monsters */
	monster_swap(oy, ox, ny, nx);

	/* Clear old target */
	m_ptr->ty = 0;
	m_ptr->tx = 0;

	/* Clear the cave_temp flag (the "project()" code may have set it). */
	cave_info[ny][nx] &= ~(CAVE_TEMP);
}



/*
 * Thrust the player or a monster away from the source of a projection.
 * Used for GF_FORCE only (GF_WIND, GF_STORM, and GF_GRAVITY blink the
 * player in a random direction).  Also used for impact blows.  -LM-
 *
 * Monsters and players can be pushed past monsters or players weaker than
 * they are.
 */
static void thrust_creature(int who, bool push, int t_y, int t_x, int grids_away)
{
	int y, x, yy, xx;
	int i, d, first_d;
	int centerline, angle;
	int dy, dx;

	int c_y, c_x;

	/* Assume a default death */
	cptr note_dies = " dies.";

	/*** Find a suitable endpoint for testing. ***/

	/* Get location of caster (assumes index of caster is not zero) */
	if (who > 0)
	{
		c_y = m_list[who].fy;
		c_x = m_list[who].fx;
	}
	else
	{
		c_y = p_ptr->py;
		c_x = p_ptr->px;
	}

	/* Determine where target is in relation to caster. */
	y = t_y - c_y + 20;
	x = t_x - c_x + 20;

	/* Find the normalized angle from caster to target */
	centerline = ABS(120 - get_angle_to_grid[y][x]);

	/* Start at the target grid. */
	y = t_y;
	x = t_x;

	/*
	 * Up to the number of grids requested, force the target away from the
	 * source of the projection, until it hits something it can't travel
	 * around.
	 */
	for (i = 0; i < grids_away; i++)
	{
		/* Randomize initial direction. */
		first_d = rand_int(8);

		/* Look around. */
		for (d = first_d; d < 8 + first_d; d++)
		{
			/* Get change in position */
			dy = ddy_ddd[d % 8];
			dx = ddx_ddd[d % 8];

			/* Get angle for this direction */
			angle = ABS(120 - get_angle_to_grid[20 + dy][20 + dx]);

			/* Ignore if angular difference is too great (45 degrees+) */
			if (ABS(centerline - angle) > 30) continue;

			/* Extract adjacent location, adjusting for direction */
			if (push)
			{
				yy = y + dy;
				xx = x + dx;
			}
			else
			{
				yy = y - dy;
				xx = x - dx;
			}

			/* Cannot switch places with stronger monsters. */
			if (cave_m_idx[yy][xx] != 0)
			{
				/* A monster is trying to pass. */
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

					/* Monsters cannot pass by stronger monsters. */
					if (cave_m_idx[yy][xx] > 0)
					{
						monster_type *n_ptr = &m_list[cave_m_idx[yy][xx]];

						if (r_info[n_ptr->r_idx].mexp > r_info[m_ptr->r_idx].mexp)
							continue;
					}

					/* Monsters cannot pass by stronger characters. */
					else
					{
						if (p_ptr->power > r_info[m_ptr->r_idx].level)
							continue;
					}
				}

				/* The player is trying to pass. */
				if (cave_m_idx[y][x] < 0)
				{
					/* Players cannot pass by stronger monsters. */
					if (cave_m_idx[yy][xx] > 0)
					{
						monster_type *n_ptr = &m_list[cave_m_idx[yy][xx]];

						if (r_info[n_ptr->r_idx].level > p_ptr->power)
							continue;
					}
				}
			}

			/* Check for obstruction. */
			if (!cave_project_bold(yy, xx))
			{
				/* Some features allow entrance, but not exit. */
				if (cave_passable_bold(yy, xx))
				{
					/* Travel down the path. */
					monster_swap(y, x, yy, xx);

					/* Jump to new location. */
					y = yy;
					x = xx;

					/* We can't travel any more. */
					i = grids_away;

					/* Stop looking. */
					break;
				}

				/* If there are non-passables everywhere, stop here. */
				else if (d == (8 + first_d - 1))
				{
					/* Message for player. */
					if (cave_m_idx[y][x] < 0)
						msg_print("You come to rest next to a wall.");
					i = grids_away;
				}
			}
			else
			{
				/* Travel down the path. */
				monster_swap(y, x, yy, xx);

				/* Jump to new location. */
				y = yy;
				x = xx;

				/* Stop looking at previous location. */
				break;
			}
		}
	}

	/* Some special messages or effects for player. */
	if (cave_m_idx[y][x] < 0)
	{
		if (cave_feat[y][x] == FEAT_TREE)
			msg_print("You come to rest in some trees.");
		if (cave_feat[y][x] == FEAT_RUBBLE)
			msg_print("You come to rest in some rubble.");
		if (cave_feat[y][x] == FEAT_WATER)
			msg_print("You come to rest in a pool of water.");
		if (cave_feat[y][x] == FEAT_LAVA)
		{
			fire_dam(damroll(4, 100), 0, "You are thrown into molten lava!",
				"burnt up in molten lava");
		}
	}

	/* Some monsters don't like lava or water. */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		if (cave_feat[y][x] == FEAT_WATER)
		{
			if (!cave_exist_mon(r_ptr, y, x, TRUE, TRUE))
			{
				note_dies = " is drowned.";

				/* Hurt the monster.  No fear. */
				mon_take_hit(cave_m_idx[y][x], 0,
					damroll(2, 18 + m_ptr->maxhp / 12), FALSE, note_dies);

				/* XXX - If still alive, monster escapes. */
				teleport_away(cave_m_idx[y][x], 3, TRUE);
			}
		}
		if (cave_feat[y][x] == FEAT_LAVA)
		{
			if (!cave_exist_mon(r_ptr, y, x, TRUE, TRUE))
			{
				note_dies = " is burnt up.";

				/* Hurt the monster.  No fear. */
				mon_take_hit(cave_m_idx[y][x], 0,
					damroll(2, 18 + m_ptr->maxhp / 12), FALSE, note_dies);

				/* XXX - If still alive, monster escapes. */
				teleport_away(cave_m_idx[y][x], 3, TRUE);
			}
		}
	}

	/* Encourage the player to blow away monsters */
	if ((who < 0) && (distance(t_y, t_x, y, x) > 5))
	{
		/* Require initial visibility and living monster */
		if ((player_can_see_bold(t_y, t_x)) && (cave_m_idx[y][x] > 0))
		{
			/* Require that monster not be visible now */
			if (!m_list[cave_m_idx[y][x]].ml)
			{
				msg_print("You hear screams fading off into the distance.");
			}
		}
	}

	/* Clear the cave_temp flag (the "project()" code may have set it). */
	cave_info[y][x] &= ~(CAVE_TEMP);
}

/*
 * Thrust a monster away from the source of the projection
 */
void thrust_away(int who, int t_y, int t_x, int grids_away)
{
	thrust_creature(who, TRUE, t_y, t_x, grids_away);
}

/*
 * Thrust a monster away toward the source of the projection
 */
void thrust_toward(int who, int t_y, int t_x, int grids_away)
{
	thrust_creature(who, FALSE, t_y, t_x, grids_away);
}


/*
 * Teleport the player to a location approximately "dist" grids away.
 *
 * Relax the distance constraints until a legal grid is found.
 *
 * If "unsafe" is TRUE, then feel free to slam the player into trees,
 * dunk him in water, or burn him in lava.
 */
void teleport_player(int dist, bool safe, bool require_los)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int d, i, spread, y = 0, x = 0;

	bool look = TRUE;


	/* Get distances to dungeon edges (top-left corner must be (0, 0) */
	y = MAX(p_ptr->py, dungeon_hgt - p_ptr->py) - 1;
	x = MAX(p_ptr->px, dungeon_wid - p_ptr->px) - 1;

	/* Use the wider of the two margins */
	d = MAX(y, x);

	/* Cannot ask for a distance greater than this margin */
	dist = MIN(d, dist);

	/* Require a distance of at least 1 */
	if (dist < 1) dist = 1;

	/* Allow some leeway */
	spread = dist / 3;

	/* Look until done */
	while (look)
	{
		int tries = 20 + dist * 3;

		/* Try several locations */
		for (i = 0; i < tries; i++)
		{
			/* Pick a (possibly illegal) location */
			while (TRUE)
			{
				y = rand_spread(py, dist);
				x = rand_spread(px, dist);
				d = distance(py, px, y, x);
				if ((d >= dist - spread) && (d <= dist + spread)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Optionally, ignore grids not in LOS */
			if ((require_los) && (!los(py, px, y, x))) continue;

			/* Usually, no teleporting into vaults */
			if (cave_info[y][x] & (CAVE_ICKY))
			{
				/* Don't teleport into a vault from outside */
				if (!(cave_info[py][px] & (CAVE_ICKY))) continue;

				/* We haven't looked hard enough for a non-vault grid */
				if ((spread < 2 * dist / 3) || (i < 2 * tries / 3)) continue;
			}

			/* Allow more terrain if not safe */
			if (safe)
			{
				/* Require unoccupied floor space */
				if (!cave_empty_bold(y, x)) continue;
			}
			else
			{
				/* Require any terrain capable of holding the player. */
				if (!cave_passable_bold(y, x)) continue;

				/* Must be unoccupied. */
				if (cave_m_idx[y][x] != 0) continue;
			}


			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* We have maximum leeway, and are still failing */
		if (spread >= dist)
		{
			/* Return */
			return;
		}

		/* Grant a little more leeway */
		spread += div_round(dist, 6);
	}

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff (update display and such) */
	handle_stuff();


	/* "Flash" the character icon, if colored characters are allowed */
	if (colored_hurt_char)
	{
		py = p_ptr->py;
		px = p_ptr->px;

		/* Hide the character  XXX XXX */
		i = cave_m_idx[py][px];
		cave_m_idx[py][px] = 0;

		/* Display the empty grid */
		lite_spot(py, px);

		/* Optional pause */
		if (op_ptr->delay_factor)
		{
			(void)Term_fresh();
			pause_for(50 + op_ptr->delay_factor * op_ptr->delay_factor);
		}

		/* Show the character XXX XXX */
		cave_m_idx[py][px] = i;

		/* Re-display */
		lite_spot(py, px);
	}


	/* Flying around the dungeon isn't always safe... */
	if (!safe)
	{
		/* The player may hit a tree, slam into rubble, or even land in lava. */
		if ((cave_feat[y][x] == FEAT_TREE) && (one_in_(2)))
		{
			(void)take_hit(damroll(2, 8), 0, "You hit a tree!",
				"being hurtled into a tree");
			if (!one_in_(3)) set_stun(p_ptr->stun + damroll(2, 8));
		}
		else if ((cave_feat[y][x] == FEAT_RUBBLE) && (one_in_(2)))
		{
			(void)take_hit(damroll(2, 14), 0, "You slam into jagged rock!",
				"being slammed into rubble");
			if (one_in_(3)) set_stun(p_ptr->stun + damroll(2, 14));
			if (!one_in_(3)) set_cut(p_ptr->cut + damroll(2, 14) * 2);
		}
		else if (cave_feat[y][x] == FEAT_LAVA)
		{
			fire_dam(damroll(4, p_ptr->power / 2), 0, "You land in molten lava!",
				"thrown into molten lava");
		}
	}

	/* Clear the cave_temp flag (the "project()" code may have set it). */
	cave_info[y][x] &= ~(CAVE_TEMP);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport monster to a grid near the given location.  This function is
 * used in the monster spell "TELE_SELF_TO", to allow monsters both to
 * suddenly jump near the character, and to make them "dance" around the
 * character.
 *
 * Usually, monster will teleport to a grid that is not more than 4
 * squares away from the given location, and not adjacent to the given
 * location.  These restrictions are relaxed if necessary.
 *
 * This function allows teleporting into vaults.
 */
void teleport_towards(int oy, int ox, int ny, int nx)
{
	int y, x;

	int dist;
	int ctr = 0;
	int min = 2, max = 4;

	/* Find a usable location */
	while (TRUE)
	{
		/* Pick a nearby legal location */
		while (TRUE)
		{
			y = rand_spread(ny, max);
			x = rand_spread(nx, max);
			if (in_bounds_fully(y, x)) break;
		}

		/* Consider all unoccupied, passable grids  XXX */
		if ((cave_passable_bold(y, x)) && (cave_m_idx[y][x] == 0))
		{
			/* Don't allow monster to teleport onto glyphs */
			if (cave_glyph(y, x)) continue;

			/* Calculate distance between target and current grid */
			dist = distance(ny, nx, y, x);

			/* Accept grids that are the right distance away. */
			if ((dist >= min) && (dist <= max)) break;
		}

		/* Occasionally relax the constraints */
		if (++ctr > 15)
		{
			ctr = 0;

			max++;
			if (max > 5) min = 0;
		}
	}

	/* Sound (assumes monster is moving) */
	sound(MSG_TPOTHER);

	/* Move monster */
	monster_swap(oy, ox, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}


/*
 * Teleport player to a grid near the given location.  Allow variable
 * inaccuracy, teleporting into vaults, and special messages.
 */
void teleport_player_to(int ny, int nx, int dis, bool allow_vault, int mode)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	int ctr = 0;


	/* Find a usable location */
	while (TRUE)
	{
		/* Occasionally advance the distance */
		if (ctr++ > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}

		/* Pick a nearby legal location */
		y = rand_spread(ny, dis);
		x = rand_spread(nx, dis);

		/* Must be fully in bounds */
		if (!in_bounds_fully(y, x)) continue;

		/* No teleporting into vaults and such */
		if ((!allow_vault) && (cave_info[y][x] & (CAVE_ICKY))) continue;

		/* Require any terrain capable of holding the player */
		if (!cave_passable_bold(y, x)) continue;

		/* Must be unoccupied */
		if (cave_m_idx[y][x] != 0) continue;

		/* Accept */
		break;
	}

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff (update view, etc.) */
	handle_stuff();

	/* Handle special modes of operation - only if monster is target */
	if ((mode) && (cave_m_idx[ny][nx] > 0))
	{
		char m_name[DESC_LEN];

		/* Get the monster */
		monster_type *m_ptr = &m_list[cave_m_idx[ny][nx]];

		/* Handle special modes */
		if (mode == 1)
		{
			/* Get the definite monster name (or "something") */
			monster_desc(m_name, m_ptr, 0x44);

			/* Message */
			msg_format("%^s commands you to return.", m_name);
		}
		else if (mode == 2)
		{
			/* Get the indefinite monster name (or "something") */
			monster_desc(m_name, m_ptr, 0x4C);

			/* Message */
			msg_format("You blink towards %^s!", m_name);
		}
		else
		{
			/* Error */
			msg_print("teleport_to(): mode not recognized.");
		}
	}


	/* Clear the cave_temp flag (the "project()" code may have set it). */
	cave_info[y][x] &= ~(CAVE_TEMP);

	/* Handle nasty terrain */
	if (cave_feat[y][x] == FEAT_LAVA)
	{
		fire_dam(damroll(4, p_ptr->power / 2), 0, "You land in molten lava!",
		"thrown into molten lava");
	}
}



/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(void)
{
	if (!p_ptr->depth)
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

		/* New depth */
		p_ptr->depth++;
	}

	else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
	   (p_ptr->depth >= MAX_DEPTH - 1))
	{
		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

		/* New depth */
		p_ptr->depth--;
	}
	else if ((p_ptr->character_type != PCHAR_IRONMAN) && (one_in_(2)))
	{
		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

		/* New depth */
		p_ptr->depth--;
	}
	else
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

		/* New depth */
		p_ptr->depth++;
	}

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Semi-controlled teleportation.  -LM-
 * From PsiAngband, through Zangband and Oangband.
 */
bool phase_warp(int range, int spread, bool wizard)
{
	int ny;
	int nx;
	bool okay;
	int dir = 5;

	/* Declare and save existing target */
	TARGET_DECLARE
	TARGET_PRESERVE

	/* Get a target */
	okay = target_set_interactive(TARGET_LOOK | TARGET_GRID);
	if (!okay) return (FALSE);

	/* Grab the target coords. */
	ny = p_ptr->target_row;
	nx = p_ptr->target_col;

	/* Not in wizard mode */
	if (!wizard)
	{
		int dist = distance(ny, nx, p_ptr->py, p_ptr->px);

		/*
		 * Test for passable terrain, forbid vaults or too large a
		 * distance, and insure that this spell is never certain.
		 */
		if (!(cave_passable_bold(ny, nx)) ||
		    (cave_info[ny][nx] & (CAVE_ICKY)) ||
		    (dist > range) ||
		    (one_in_(get_skill(S_WIZARDRY, 20, 60) - dist)))
		{
			msg_print("You fail to exit the astral plane correctly!");

			p_ptr->energy -= 50;
			teleport_player(15, FALSE, FALSE);

			/* Restore target */
			TARGET_RESTORE

			/* Update */
			handle_stuff();

			return (TRUE);
		}
	}

	/* Restore target */
	TARGET_RESTORE

	/* Wizard -- only require passable terrain. */
	else if (!cave_passable_bold(ny, nx))
	{
		msg_print("The square you are aiming for is impassable.");
		return (FALSE);
	}


	/* (Semi) Controlled teleport. */
	teleport_player_to(ny, nx, spread, wizard, 0);
	return (TRUE);
}

/*
 * Attempt to blink away from a monster if hit.  -LM-
 *
 * Affects depend on wizardry skill.
 */
bool do_blink_away(int who)
{
	int ty = 0;
	int tx = 0;

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Get and randomize skill */
	int skill = randint(get_skill(S_WIZARDRY, 0, 100));


	/* Spell messed up and teleported you /to/ a monster */
	if (skill <= 5)
	{
		msg_print("You blink towards a monster!");

		/* Get a nearby monster */
		while (!ty)
		{
			get_closest_los_monster(randint(5), py, px,
				&ty, &tx, FALSE);
		}

		/* Allow jumping into vaults */
		teleport_player_to(ty, tx, skill / 2, TRUE, 0);
	}

	/* Spell normally either blinks you or thrusts you away */
	else
	{
		if (one_in_(4))
		{
			msg_print("You are magically thrust away.");
			thrust_away(who, py, px, rand_range(4, 6));
		}
		else
		{
			msg_print("You blink away.");

			/* Safe teleportation with a skill of 50 */
			teleport_player(rand_range(6, 10), skill >= 50, FALSE);
		}
	}

	/* Reduce blinks */
	set_blink_away(p_ptr->blink_away - 1);



	/* Check to see if we moved */
	if ((p_ptr->py != py) || (p_ptr->px != px))
	{
		/* It takes a moment to recover from a blink */
		p_ptr->energy -= (25 - skill / 5);

		return (TRUE);
	}

	/* We didn't move */
	return (FALSE);
}

/*
 * Helper function for passwall()
 *
 * Handles moving the player to the final location, and
 * what happens when he gets there.
 */
static void passwall_finish(int y, int x)
{
	int py = p_ptr->py;
	int px = p_ptr->px;


	/* Did the player actually move? */
	if (py == y && px == x) return;

	/* Grid is not passable, and character cannot walk though walls */
	if ((!cave_passable_bold(y, x)) && (!p_ptr->wraithform))
	{
		/* Smash doors, wreck walls, take damage */
		if (cave_closed_door(y, x))
		{
			take_hit(damroll(4, 5) + p_ptr->chp / 3, 0, "You emerge in a door!",
				"becoming one with a door");
			cave_set_feat(y, x, FEAT_BROKEN);
		}
		else
		{
			take_hit(damroll(6, 8) + p_ptr->chp / 2, 0, "You emerge in the wall!",
				"becoming one with a wall");
			cave_set_feat(y, x, FEAT_RUBBLE);
		}
	}

	/* Move player */
	monster_swap(py, px, y, x);
}

/*
 * Send the player shooting through walls in the given direction until
 * they reach a non-wall space, or a monster, or a permanent wall.  -JDL-
 */
bool passwall(int dir, bool local)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y1, x1, y2, x2;
	int ty, tx, dy, dx;
	int oy, ox, ny = 0, nx = 0;

	int i;

	/* We extend the path through walls. */
	u32b flg = PROJECT_PASS;

	bool in_wall = FALSE;


	/* Get target */
	adjust_target(dir, p_ptr->py, p_ptr->px, &ty, &tx);

	/* In case we need to relocate the target */
	dy = ty - py;
	dx = tx - px;

	/* Starting location */
	y1 = py;
	x1 = px;

	/* Default "destination" */
	y2 = ty;
	x2 = tx;

	/* Make sure we're actually going somewhere */
	if (x1 == x2 && y1 == y2)
	{
		msg_print("You twist around.");
		return (FALSE);
	}

	/* Project until done */
	while (TRUE)
	{
		/* Calculate the projection path */
		(void)project_path(MAX_RANGE, y1, x1, &y2, &x2, flg);

		oy = y1;
		ox = x1;

		/* Project along the path */
		for (i = 0; i < path_n; i++)
		{
			ny = GRID_Y(path_g[i]);
			nx = GRID_X(path_g[i]);

			/* Stop if we tried to go through a monster or a permanent wall. */
			if (cave_m_idx[ny][nx] || cave_perma_bold(ny, nx) ||
			    (!in_bounds_fully(ny, nx)))
			{
				/* Assume that more messages will appear */
				cptr s = "...";

				/* We're safe -- end the sentence */
				if (cave_passable_bold(oy, ox)) s = ".";

				if (cave_m_idx[ny][nx])
					msg_format("A monster blocks your path%s", s);
				else
					msg_format("An impenetrable wall bars your path%s", s);

				passwall_finish(oy, ox);
				return (TRUE);
			}

			if (in_wall)
			{
				/* Stop when we reach a passable grid */
				if (cave_passable_bold(ny, nx))
				{
					passwall_finish(ny, nx);
					msg_print("A passage opens, and you step through.");
					return (TRUE);
				}
			}
			else
			{
				/* Check if we've entered the walls yet */
				if (!cave_passable_bold(ny, nx)) in_wall = TRUE;
				else
				{
					/* Abort if we needed to start by a wall and didn't */
					if (local)
					{
						msg_print("There is no wall there!");
						return (FALSE);
					}
				}
			}

			oy = ny;
			ox = nx;
		}

		/* Continue */
		y1 = ny;
		x1 = nx;

		/* If we reached the target, but aren't done yet, move it. */
		if (y1 == y2 && x1 == x2)
		{
			y2 = y1 + dy;
			x2 = x1 + dx;
		}
	}
}




/************************************************************************
 *                                                                      *
 *                        Character-effect magics                       *
 *                                                                      *
 ************************************************************************/



/*
 * Save the equipment damage message, so that it appears after the
 * take_hit message.
 */
cptr minus_ac_msg;


/*
 * Acid has hit the player, attempt to affect some armor, if the armor
 * is not melded with the player's body because of a shapechange.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(void)
{
	object_type *o_ptr = NULL;

	char o_name[DESC_LEN];

	u32b f1, f2, f3;


	/*
	 * If the player has shapechanged, their armor is part of their
	 * body, and cannot be damaged.  A mixed blessing.
	 */
	if (p_ptr->schange) return (FALSE);

	/* Pick a (possibly empty) inventory slot */
	switch (randint(6))
	{
		case 1: o_ptr = &inventory[INVEN_BODY];   break;
		case 2: o_ptr = &inventory[INVEN_ARM];    break;
		case 3: o_ptr = &inventory[INVEN_OUTER];  break;
		case 4: o_ptr = &inventory[INVEN_HANDS];  break;
		case 5: o_ptr = &inventory[INVEN_HEAD];   break;
		case 6: o_ptr = &inventory[INVEN_FEET];   break;
		default: return (FALSE);
	}

	/* Nothing to damage */
	if (!o_ptr->k_idx) return (FALSE);

	/* No damage left to be done */
	if (o_ptr->ac + o_ptr->to_a <= 0) return (FALSE);

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Object resists */
	if (f2 & (TR2_IGNORE_ACID))
	{
		minus_ac_msg = format("Your %s is unaffected!", o_name);
		return (TRUE);
	}

	/* Message */
	minus_ac_msg = format("Your %s is damaged!", o_name);

	/* Damage the item */
	o_ptr->to_a--;

	/* Calculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Item was damaged */
	return (TRUE);
}

/*
 * Hurt the player with Acid.  Resistances now reduce inventory
 * destruction.   Acid can reduce CHR, as in Zangband.
 */
void acid_dam(int dam0, int msg_type, cptr hit_str, cptr kb_str)
{
	int inv = 0;
	int odds;
	int dam = dam0;


	/* No damage. */
	if (dam <= 0) return;

	/* Assume no message */
	minus_ac_msg = NULL;

	/* Resistances reduce the chance to attack armor */
	odds = 2;
	if (p_ptr->resist_acid) odds *= 2;
	if (p_ptr->oppose_acid) odds *= 2;

	/* Sometimes attack armor (unless immune) */
	if ((!p_ptr->immune_acid) && (one_in_(odds)))
	{
		/* If any armor gets hit, defend the player (and his backpack) */
		if (minus_ac()) dam = 2 * dam / 3;
	}

	/*
	 * Determine the chance in 1000 of an inventory item being lost (note
	 * that scrolls and potions have a +50% extra chance to be lost).
	 */
	inv = div_round(dam, 15) + 4;
	inv = MIN(40, inv);
	if (p_ptr->immune_acid) inv = div_round(inv, 5);
	else if ((p_ptr->resist_acid) || (p_ptr->oppose_acid)) inv /= 2;

	/* Handle vulnerability */
	if (p_ptr->vuln_acid) dam += dam / 2;

	/* Total (bodily) Immunity */
	if (p_ptr->immune_acid) dam = 0;

	/* Resist the damage */
	if (p_ptr->resist_acid) dam = div_round(dam, 3);
	if (p_ptr->oppose_acid) dam = div_round(dam, 3);

	/* Take damage */
	(void)take_hit(dam, msg_type, hit_str, kb_str);


	/* Player is still alive */
	if (!p_ptr->is_dead)
	{
		/* We have an equipment damage message to print */
		if (minus_ac_msg) msg_format("%s", minus_ac_msg);

		/* Acid can reduce charisma */
		if ((!(p_ptr->oppose_acid || p_ptr->resist_acid)) &&
			 (!p_ptr->sustain_chr) && (dam >= 20) && (one_in_(HURT_CHANCE)))
		{
			(void)do_dec_stat(A_CHR, 1, FALSE,
				"The acid eats into your skin!", NULL);
		}

		/* Check for protective blanket */
		if (!check_blanket(CHECK_BLANKET_ACID, inv))
		{
			/* Inventory damage */
			(void)inven_damage(set_acid_destroy, inv, dam0);
		}
	}
}


/*
 * Hurt the player with electricity.  Resistances now reduce inventory
 * destruction.   Electricity can reduce DEX, as in Zangband.  Electricity can
 * stun the player.
 */
void elec_dam(int dam0, int msg_type, cptr hit_str, cptr kb_str)
{
	int inv = 0;
	int dam = dam0;

	/* No damage. */
	if (dam <= 0) return;


	/*
	 * Determine the chance in 1000 of an inventory item being lost (note
	 * that scrolls and potions have a +50% extra chance to be lost).
	 */
	inv = div_round(dam, 15) + 4;
	inv = MIN(40, inv);
	if (p_ptr->immune_elec) inv = div_round(inv, 5);
	else if ((p_ptr->resist_elec) || (p_ptr->oppose_elec)) inv /= 2;


	/* Total (bodily) Immunity */
	if (p_ptr->immune_elec) dam = 0;

	/* Resist the damage */
	if (p_ptr->oppose_elec) dam = div_round(dam, 3);
	if (p_ptr->resist_elec) dam = div_round(dam, 3);

	/* Take damage */
	(void)take_hit(dam, msg_type, hit_str, kb_str);

	/* Handle vulnerability */
	if (p_ptr->vuln_elec) dam += dam / 2;

	/* Player is still alive */
	if (!p_ptr->is_dead)
	{
		/* Electricity can reduce dexterity */
		if ((!(p_ptr->oppose_elec || p_ptr->resist_elec)) &&
			 (!p_ptr->sustain_dex) && (dam >= 20) && (one_in_(HURT_CHANCE)))
		{
			(void)do_dec_stat(A_DEX, 1, FALSE,
				"Your muscles spasm in shock!", NULL);
		}

		/* Electricity can stun, if enough damage is done. */
		if ((dam > 30) && (randint(dam) > 20 + dam / 4))
		{
			if (dam > 900) set_stun(p_ptr->stun + 50);
			else           set_stun(p_ptr->stun + 5 + div_round(dam, 20));
		}

		/* Check for protective blanket */
		if (!check_blanket(CHECK_BLANKET_ELEC, inv))
		{
			/* Inventory damage */
			(void)inven_damage(set_elec_destroy, inv, dam0);
		}
	}
}

/*
 * Hurt the player with Fire.  Resistances now reduce inventory
 * destruction.   Fire can reduce STR.
 */
void fire_dam(int dam0, int msg_type, cptr hit_str, cptr kb_str)
{
	int inv = 0;
	int dam = dam0;
	int extra_dam = 0;

	/* No damage. */
	if (dam <= 0) return;


	/*
	 * Determine the chance in 1000 of an inventory item being lost (note
	 * that scrolls and potions have a 50% chance to be lost).
	 */
	inv = div_round(dam, 15) + 4;
	inv = MIN(40, inv);
	if (p_ptr->immune_fire) inv = div_round(inv, 5);
	else if ((p_ptr->resist_fire) || (p_ptr->oppose_fire)) inv /= 2;

	/* Handle vulnerability */
	if (p_ptr->vuln_fire) dam += dam / 2;

	/* Total (bodily) Immunity */
	if (p_ptr->immune_fire) dam = 0;

	/* Resist the damage */
	if (p_ptr->resist_fire) dam = div_round(dam, 3);
	if (p_ptr->oppose_fire) dam = div_round(dam, 3);

	dam += extra_dam;

	/* Take damage */
	(void)take_hit(dam, msg_type, hit_str, kb_str);


	/* Player is still alive */
	if (!p_ptr->is_dead)
	{
		/* Fire can reduce strength */
		if ((!(p_ptr->oppose_fire || p_ptr->resist_fire)) &&
			 (!p_ptr->sustain_str) && (dam >= 20) && (one_in_(HURT_CHANCE)))
		{
			(void)do_dec_stat(A_STR, 1, FALSE,
				"The heat burns you badly; you are weakened!", NULL);
		}

		/* Check for protective blanket */
		if (!check_blanket(CHECK_BLANKET_FIRE, inv))
		{
			/* Inventory damage */
			(void)inven_damage(set_fire_destroy, inv, dam0);
		}
	}
}

/*
 * Hurt the player with Cold.  Resistances now reduce inventory
 * destruction.   Cold can reduce CON.
 */
void cold_dam(int dam0, int msg_type, cptr hit_str, cptr kb_str)
{
	int inv = 0;
	int dam = dam0;

	/* No damage. */
	if (dam <= 0) return;


	/*
	 * Determine the chance in 1000 of an inventory item being lost (note
	 * that scrolls and potions have a +50% extra chance to be lost).
	 */
	inv = div_round(dam, 15) + 4;
	inv = MIN(40, inv);
	if (p_ptr->immune_cold) inv = div_round(inv, 5);
	else if ((p_ptr->resist_cold) || (p_ptr->oppose_cold)) inv /= 2;


	/* Total (bodily) Immunity */
	if (p_ptr->immune_cold) dam = 0;

	/* Resist the damage */
	if (p_ptr->resist_cold) dam = div_round(dam, 3);
	if (p_ptr->oppose_cold) dam = div_round(dam, 3);

	/* Handle vulnerability */
	if (p_ptr->vuln_cold) dam += dam / 2;

	/* Take damage */
	(void)take_hit(dam, msg_type, hit_str, kb_str);


	/* Player is still alive */
	if (!p_ptr->is_dead)
	{
		/* Cold can reduce constitution */
		if ((!(p_ptr->oppose_cold || p_ptr->resist_cold)) &&
			 (!p_ptr->sustain_con) && (dam >= 20) && (one_in_(HURT_CHANCE)))
		{
			(void)do_dec_stat(A_CON, 1, FALSE,
				"The cold seeps into your bones!", NULL);
		}

		/* Check for protective blanket */
		if (!check_blanket(CHECK_BLANKET_COLD, inv))
		{
			/* Inventory damage */
			(void)inven_damage(set_cold_destroy, inv, dam0);
		}
	}
}

/*
 * Apply disenchantment to the player's stuff, unless it is melded with
 * his body because of a shapechange.
 *
 * Some effects require a high enough damage.
 *
 * Return "TRUE" if the player notices anything.
 */
bool apply_disenchant(int dam)
{
	int i;
	int slot = 0;

	object_type *o_ptr;

	char o_name[DESC_LEN];


	/* Character is gone */
	if (p_ptr->leaving) return (FALSE);

	/* Character is shapeshifted */
	if (p_ptr->schange)
	{
		/* Disenchantment can force the player back into his normal form */
		if ((randint(dam) > 20 + (dam / 2)) && (!check_save(150)))
		{
			/* Message */
			msg_print("You are wrenched back into your normal form!");

			/* Change back to normal form */
			shapechange_perm(SHAPE_NORMAL);
		}

		/* A shapechanged character's armor is safe from disenchantment */
		else
		{
			/* Pick a random slot -- armor is safe */
			switch (randint(9))
			{
				case 1: slot = INVEN_WIELD; break;
				case 2: slot = INVEN_BOW; break;
				case 3: return (FALSE);
				case 4: return (FALSE);
				case 5: return (FALSE);
				case 6: return (FALSE);
				case 7: return (FALSE);
				case 8: return (FALSE);
				case 9: slot = INVEN_Q1; break;
			}
		}
	}
	else
	{
		/* Pick a random slot -- weapons and armor are both fair game */
		switch (randint(9))
		{
			case 1: slot = INVEN_WIELD; break;
			case 2: slot = INVEN_BOW; break;
			case 3: slot = INVEN_BODY; break;
			case 4: slot = INVEN_OUTER; break;
			case 5: slot = INVEN_ARM; break;
			case 6: slot = INVEN_HEAD; break;
			case 7: slot = INVEN_HANDS; break;
			case 8: slot = INVEN_FEET; break;
			case 9: slot = INVEN_Q1; break;
		}
	}

	/* Check for protective blanket */
	if (check_blanket(CHECK_BLANKET_DISEN, 1)) return (FALSE);


	/* Search the quiver */
	if (slot == INVEN_Q1)
	{
		int start = rand_int(QUIVER_SLOTS);

		/* Search the entire quiver with random start point */
		for (i = start; i < QUIVER_SLOTS + start; i++)
		{
			/* Get the item here */
			o_ptr = &inventory[INVEN_Q1 + (i % QUIVER_SLOTS)];

			/* No item -- continue */
			if (!o_ptr->k_idx) continue;

			/* Choose this item */
			slot = INVEN_Q1 + (i % QUIVER_SLOTS);
			break;
		}
	}

	/* Get the item in this slot */
	o_ptr = &inventory[slot];


	/* No item, nothing happens */
	if (!o_ptr->k_idx) return (FALSE);


	/* Nothing to disenchant */
	if ((o_ptr->to_h <= 0) && (o_ptr->to_d <= 0) && (o_ptr->to_a <= 0))
	{
		/* Nothing to notice */
		return (FALSE);
	}


	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);


	/* Artifacts have a two-thirds chance to resist */
	if (artifact_p(o_ptr) && (!one_in_(3)))
	{
		/* Message */
		msg_format("Your %s (%c) resist%s disenchantment!",
			   o_name, index_to_label(slot),
			   ((o_ptr->number != 1) ? "" : "s"));

		/* Notice */
		return (TRUE);
	}


	/* Disenchant tohit */
	if (o_ptr->to_h > 0) o_ptr->to_h--;
	if ((o_ptr->to_h > 5) && (one_in_(5))) o_ptr->to_h--;

	/* Disenchant todam */
	if (o_ptr->to_d > 0) o_ptr->to_d--;
	if ((o_ptr->to_d > 5) && (one_in_(5))) o_ptr->to_d--;

	/* Disenchant toac */
	if (o_ptr->to_a > 0) o_ptr->to_a--;
	if ((o_ptr->to_a > 5) && (one_in_(5))) o_ptr->to_a--;

	/* Message */
	msg_format("Your %s (%c) %s disenchanted!",
		   o_name, index_to_label(slot),
		   ((o_ptr->number != 1) ? "were" : "was"));

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Notice */
	return (TRUE);
}



/*
 * Drain a wand, staff, rod, or activatable item.
 */
int apply_draining(int power)
{
	int i, slot;

	int gain = 2 + div_round(power, 20);

	bool flag;

	object_type *o_ptr;
	object_kind *k_ptr;

	char o_name[DESC_LEN];


	/* Check for protective blanket */
	if (check_blanket(CHECK_BLANKET_DRAIN, 1)) return (0);

	/* Allow a saving throw (sometimes) */
	if (TRUE)
	{
		/* Saving throw and wizardly protection both help */
		i = p_ptr->skill_sav;
		if (p_ptr->wiz_prot) i += 50;

		/* Roll for resistance (it is hard to resist) */
		if (i > randint(power * 3)) return (0);
	}



	/* Blindly hunt fifty times for an item. */
	for (i = 0; i < 50; i++)
	{
		/* Pick an item (can be in backpack or inventory) */
		slot = rand_int(INVEN_TOTAL);

		/* Get the item */
		o_ptr = &inventory[slot];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Assume we cannot drain this object */
		flag = FALSE;


		/* Case of charged wands. */
		if ((o_ptr->tval == TV_WAND) && (o_ptr->pval))
		{
			o_ptr->pval -= k_ptr->pval * randint(o_ptr->number);
			if (o_ptr->pval < 0) o_ptr->pval = 0;

			gain *= MIN(15, o_ptr->pval);
		}

		/* Case of charged staffs. */
		else if ((o_ptr->tval == TV_STAFF) && (o_ptr->pval))
		{
			o_ptr->pval -= k_ptr->pval * randint(o_ptr->number);
			if (o_ptr->pval < 0) o_ptr->pval = 0;

			gain *= MIN(15, o_ptr->pval * o_ptr->number / 2);
		}

		/* Case of (at least partially) charged rods. */
		else if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout < o_ptr->pval))
		{
			o_ptr->timeout += k_ptr->pval * randint(o_ptr->number);

			gain *= MIN(10, o_ptr->number *
				(o_ptr->pval - o_ptr->timeout) / 5);
		}

		/* Case of fully-charged activatable items. */
		else if ((o_ptr->activate) && (!o_ptr->timeout))
		{
			o_ptr->timeout += 200;
			gain *= 10;
		}

		/* Ignore all other objects */
		else
		{
			continue;
		}

		/* Get short object name */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Make it lowercase */
		strlower(o_name);

		/* Message */
		msg_format("Energy drains from your %s!", o_name);

		/* Combine / Reorder the pack */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);

		/* Only hit one inventory slot. */
		break;
	}

	return (gain);
}


/*
 * Apply Nexus
 *
 * Uses damage to control severity of effects
 */
void apply_nexus(int fy, int fx, int dam)
{
	/* Assume normal damage */
	int nexus = 9;

	/* Light damage */
	if (dam < rand_range(5, 20)) nexus = 3;

	/* Heavy damage */
	if (dam > rand_range(200, 400)) nexus = 12;

	/* Character is gone */
	if (p_ptr->leaving) return;


	/* Force the character back into his normal shape */
	if ((p_ptr->schange != SHAPE_NORMAL) && (dam > randint(200)))
	{
		msg_print("You are wrenched back into your normal form!");
		shapechange_perm(SHAPE_NORMAL);
	}

	/* Effects of nexus */
	switch (randint(nexus))
	{
		case 1: case 2: case 3:
		{
			teleport_player(MAX(10, dam), FALSE, FALSE);
			break;
		}

		case 4: case 5:
		{
			teleport_player(200, FALSE, FALSE);
			break;
		}

		case 6:
		{
			/* Illegal fy, fx -- just do a teleport */
			if (!in_bounds_fully(fy, fx))
			{
				teleport_player(200, FALSE, FALSE);
			}

			/* Legal fy, fx -- use it */
			else teleport_player_to(fy, fx, 0, TRUE, 0);
			break;
		}

		case 7:
		{
			if (check_save(100))
			{
				msg_print("You resist the effects!");
				break;
			}

			/* Set word of recall (unless ironman) */
			if (p_ptr->character_type != PCHAR_IRONMAN) set_recall(rand_range(3, 30));
			break;
		}

		case 8:
		{
			if (check_save(100))
			{
				msg_print("You resist the effects!");
				break;
			}

			/* Either reroll hitdice or throw character off the level */
			if (one_in_(2))
			{
				teleport_player_level();
			}
			else
			{
				/* Re-roll life ratings */
				msg_print("Your endurance changes...");
				get_extra();

				/* Update and refresh some things */
				p_ptr->update |= (PU_HP | PU_BONUS);
				p_ptr->redraw |= (PR_HP);
			}

			/* Hack -- cancel nexus field */
			if (p_ptr->nexus_field)
			{
				set_nexus_field(0, 0);
			}

			break;
		}

		case 9:
		case 10:
		{
			/* Allow resistance */
			if ((p_ptr->resist_nexus) || (check_save(100)))
			{
				msg_print("You resist the effects!");
				break;
			}

			/* Shuffle a pair of stats */
			else
			{
				shuffle_stats(1);
			}

			break;
		}

		default:
		{
			/* Hack -- apply nexus again */
			apply_nexus(fy, fx, 199);

			/* Character is surrounded in a nexus field */
			set_nexus_field(rand_range(dam / 80, dam / 40), dam);

			break;
		}
	}

	/* Disturbing */
	disturb(0, 0);
}


/*
 * Recall player
 */
void recall_player()
{
	/* Ironman */
	if ((p_ptr->character_type == PCHAR_IRONMAN) && !p_ptr->total_winner)
	{
		msg_print("Nothing happens.");
		return;
	}

	/* Toggle recall */
	if (p_ptr->word_recall == 0) set_recall(rand_range(15, 35));
	else                         set_recall(0);
}


/*
 * Increase player's hit points, notice effects
 */
bool hp_player(int num)
{
	/* Healing needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		/* Gain hitpoints */
		p_ptr->chp += num;

		/* Enforce maximum */
		if (p_ptr->chp >= p_ptr->mhp)
		{
			p_ptr->chp = p_ptr->mhp;
			p_ptr->chp_frac = 0;
		}

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Heal a little */
		if (num < 3 + (p_ptr->mhp / 15))
		{
			msg_print("You feel a little better.");
		}

		/* Heal some */
		else if (num < 6 + (p_ptr->mhp / 5))
		{
			msg_print("You feel better.");
		}

		/* Heal a fair amount */
		else if (num < 12 + (p_ptr->mhp / 2))
		{
			msg_print("You feel much better.");
		}

		/* Heal a lot */
		else
		{
			msg_print("You feel very good.");
		}

		/* If character color changes with damage taken, redraw */
		if (colored_hurt_char) lite_spot(p_ptr->py, p_ptr->px);

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}

/*
 * Increase player's hit points arranging for points above the player's
 * current maximum to be saved for when the maximum changed.
 *
 * This is used for effects like potions of Heroism, to avoid
 * wasting the extra hp they grant if you didn't need healing.
 */
bool extra_hp_player(int num)
{
	if (p_ptr->mhp < p_ptr->chp + num)
		p_ptr->extrahp += p_ptr->chp + num - p_ptr->mhp;
	return (hp_player(num));
}


/*
 * Heal a player a percentage of total hitpoints.  -EZ-
 */
bool heal_player(int perc, int min)
{
	int heal;

	/* No healing needed */
	if (p_ptr->chp >= p_ptr->mhp) return (FALSE);

	/* Figure healing level */
	heal = p_ptr->mhp * perc / 100;

	/* Enforce minimums */
	if (heal < min) heal = min;

	/* Actual healing */
	return (hp_player(heal));
}

/*
 * Hack -- return how much healing the "heal_player()" function will
 * do now.
 */
int get_heal_amount(int perc, int min)
{
	/* Figure healing level */
	int heal = p_ptr->mhp * perc / 100;

	/* Enforce minimums */
	if (heal < min) heal = min;

	/* Actual healing */
	return (heal);
}


/*
 * Increase player's spell points, notice effects
 */
bool sp_player(int num, cptr msg)
{
	/* Recovery needed */
	if (p_ptr->csp < p_ptr->msp)
	{
		/* Gain spell points */
		p_ptr->csp += num;

		/* Enforce maximum */
		if (p_ptr->csp >= p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
			p_ptr->csp_frac = 0;
		}

		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Messages */
		if (msg)
		{
			msg_format("%s", msg);
		}
		else
		{
			/* Recover a little */
			if ((p_ptr->csp < p_ptr->msp) && (num <= p_ptr->msp / 3))
			{
				msg_print("You feel your head clear a little.");
			}

			/* Recover a lot */
			else
			{
				msg_print("You feel your head clear.");
			}
		}

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
	"weak",
	"stupid",
	"naive",
	"clumsy",
	"sickly",
	"ugly"
};

/*
 * Increase a stat
 *
 * Most code will "restore" a stat before calling this function,
 * in particular, stat potions will always restore the stat and
 * then increase the fully restored value.
 */
bool inc_stat(int stat, int points)
{
	int value, gain;
	int max = 18 + 100;

	/* Augment the current stat */
	value = p_ptr->stat_cur[stat];

	/* Raise stats until we run out of power */
	while (points--)
	{
		/* Cannot go above 18/100 */
		if (value < max)
		{
			/* Gain one (sometimes two) points */
			if (value < 18)
			{
				gain = (one_in_(6) ? 2 : 1);
				value += gain;
			}

			/* Gain according to distance from max */
			else
			{
				/* Gain 1/6 to 1/3 of distance to 18/100 */
				gain = rand_range((max - value) / 6, (max - value) / 3);

				/* Minimum gain is 4 points */
				if (gain < 4) gain = 4;

				/* Apply the bonus */
				value += gain;

				/* Maximal value */
				if (value > max) value = max;
			}
		}
	}

	/* Note gain */
	if (value > p_ptr->stat_cur[stat])
	{
		/* Save the new value */
		p_ptr->stat_cur[stat] = value;

		/* Bring up the maximum too */
		if (value > p_ptr->stat_max[stat])
		{
			p_ptr->stat_max[stat] = value;
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to gain */
	return (FALSE);
}

/*
 * Gain one or more "points" in a stat.
 *
 * Allow special messages, default to standard ones.
 */
bool do_inc_stat(int stat, int points, cptr msg)
{
	bool res;

	/* Restore stat first */
	res = res_stat(stat);

	/* Attempt to increase */
	if (inc_stat(stat, points))
	{
		/* Special message */
		if (msg) msg_format("%s", msg);

		/* Standard messages */
		else
		{
			if (stat == A_STR) msg_print("Wow!  What bulging muscles!");
			if (stat == A_INT) msg_print("Aren't you brilliant!");
			if (stat == A_WIS) msg_print("You suddenly have a profound thought!");
			if (stat == A_DEX) msg_print("You feel more limber!");
			if (stat == A_CON) msg_print("Your endurance increases!");
			if (stat == A_CHR) msg_print("Gee, ain't you cute!");
		}

		/* Notice */
		return (TRUE);
	}

	/* Restoration only */
	if (res)
	{
		/* Message */
		msg_format("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);

	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Decreases a stat.  Use "amount" to determine the number of points taken
 * away.  Stats of the form 18/xxx are reduced by an average of 10 for each
 * point.
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.
 *
 * This function ignores sustains.
 */
bool dec_stat(int stat, int points, int permanent)
{
	int i, cur, max;

	bool same;

	/* Assume no change */
	bool change = FALSE;

	/* Get current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the current value of the stat equals its maximum */
	same = (cur == max);


	/* Loop for the number of points we wish to suck away */
	for (i = 0; i < points; i++)
	{
		/* Handle stats at or below 18 */
		if (cur <= 18)
		{
			/* Reduce by one point */
			cur--;
		}

		/* Handle values above 18 */
		else
		{
			/* Reduce stat by a variable amount, averaging 10 */
			int loss = damroll(4, 4);

			/* Apply loss */
			cur -= loss;

			/* Hack -- Only reduce stat below 18 sometimes */
			if (cur < 18) cur = (points <= 2) ? 18 : 17;
		}

		/* Optionally reduce maximum value of stat */
		if (permanent)
		{
			/* Handle stats at or below 18 */
			if (max <= 18)
			{
				/* Reduce by one point */
				max--;
			}

			/* Handle values above 18 */
			else
			{
				/* Reduce stat by a variable amount, averaging 10 */
				int loss = damroll(4, 4);

				/* Apply loss */
				max -= loss;

				/* Hack -- Only reduce stat below 18 sometimes */
				if (max < 18) max = (points <= 2) ? 18 : 17;
			}

			/* Hack -- Neaten up maximum values */
			if (same || (max < cur)) max = cur;
		}
	}

	/* Stats may not go below 3 */
	if (cur < 3) cur = 3;
	if (max < 3) max = 3;

	/* Something happened */
	if (cur != p_ptr->stat_cur[stat]) change = TRUE;
	if (max != p_ptr->stat_max[stat]) change = TRUE;

	/* Apply changes */
	if (change)
	{
		/* Actually set the stat to its new value */
		p_ptr->stat_cur[stat] = cur;
		p_ptr->stat_max[stat] = max;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
	}

	/* Done */
	return (change);
}

/*
 * Lose one or more points in a stat
 *
 * Allow special messages, default to standard ones.
 */
bool do_dec_stat(int stat, int points, bool perm, cptr msg_drain,
	cptr msg_sustain)
{
	bool sust = FALSE;

	/* Note sustains */
	switch (stat)
	{
		case A_STR: if (p_ptr->sustain_str) sust = TRUE; break;
		case A_INT: if (p_ptr->sustain_int) sust = TRUE; break;
		case A_WIS: if (p_ptr->sustain_wis) sust = TRUE; break;
		case A_DEX: if (p_ptr->sustain_dex) sust = TRUE; break;
		case A_CON: if (p_ptr->sustain_con) sust = TRUE; break;
		case A_CHR: if (p_ptr->sustain_chr) sust = TRUE; break;

		/* For stats not yet handled */
		default:    return (FALSE);
	}

	/* Sustain */
	if (sust)
	{
		/* Special message */
		if (msg_sustain)
		{
			msg_format("%s", msg_sustain);
		}

		/* Standard message */
		else
		{
			msg_format("You feel very %s for a moment, but the feeling passes.",
			desc_stat_neg[stat]);
		}

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, points, perm))
	{
		/* Special message */
		if (msg_drain)
		{
			message_format(MSG_DRAIN_STAT, 0, "%s", msg_drain);
		}

		/* Standard message -- loss is permanent */
		else if (perm)
		{
			if (stat == A_STR)
				message(MSG_L_RED, 0, "You are permanently weakened!");
			if (stat == A_INT)
				message(MSG_L_RED, 0, "Your intellect is permanently damaged!");
			if (stat == A_WIS)
				message(MSG_L_RED, 0, "You lose some of your wisdom, permanently!");
			if (stat == A_DEX)
				message(MSG_L_RED, 0, "Your dexterity drains away, and cannot be restored!");
			if (stat == A_CON)
				message(MSG_L_RED, 0, "Your stamina is permanently reduced!");
			if (stat == A_CHR)
				message(MSG_L_RED, 0, "Your charisma is damaged, permanently!");
		}

		/* Standard message -- loss is merely temporary */
		else
		{
			message_format(MSG_DRAIN_STAT, 0, "You feel very %s.", desc_stat_neg[stat]);
		}

		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}

/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
	/* Restore if needed */
	if (p_ptr->stat_cur[stat] != p_ptr->stat_max[stat])
	{
		/* Restore */
		p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to restore */
	return (FALSE);
}

/*
 * Restore lost "points" in a stat
 *
 * Allow special messages, default to standard ones.
 */
bool do_res_stat(int stat, cptr msg)
{
	/* Attempt to increase */
	if (res_stat(stat))
	{
		/* Special message */
		if (msg) msg_format("%s", msg);

		/* Standard message */
		else msg_format("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}

/*
 * Restore all stats
 */
bool restore_stats()
{
	int i;
	bool r = FALSE;

	for (i = 0; i < A_MAX; i++)
	{
		if (do_res_stat(i, NULL)) r = TRUE;
	}
	return (r);
}


/*
 * Shuffle one or more pairs of stats
 */
void shuffle_stats(int num)
{
	int max1, cur1, max2, cur2;
	int i, stat1, stat2;


	/* Paranoia -- Disturb (strongly) */
	disturb(1, 0);

	/* Message */
	msg_print("Your body starts to scramble...");

	/* Shuffle stats */
	for (i = 0; i < num; i++)
	{
		/* Pick the first stat */
		stat1 = rand_int(A_MAX);

		/* Choose a second, different stat */
		for (stat2 = stat1; stat2 == stat1; stat2 = rand_int(A_MAX));

		/* Swap them */
		max1 = p_ptr->stat_max[stat1];
		cur1 = p_ptr->stat_cur[stat1];
		max2 = p_ptr->stat_max[stat2];
		cur2 = p_ptr->stat_cur[stat2];

		p_ptr->stat_max[stat1] = max2;
		p_ptr->stat_cur[stat1] = cur2;
		p_ptr->stat_max[stat2] = max1;
		p_ptr->stat_cur[stat2] = cur1;
	}

	/* Recalculate bonuses (later) */
	p_ptr->update |= (PU_BONUS);
}



/*
 * Inflict disease on the character.
 *
 * The higher your constitution, the more you resist.  The stronger the
 * attack, the harder it is to resist.
 */
void disease(int *damage)
{
	int resistance;
	int i, pow;

	/* Can't infect a corpse */
	if (p_ptr->is_dead) return;

	/* Get resistance factor (ranges from 4 to ~32) */
	resistance = 2 + p_ptr->stat_ind[A_CON] - rsqrt(*damage);
	if (resistance < 5) resistance = 5;

	/* Natural vitality helps.  Resistance to poison helps a little. */
	if (p_ptr->vitality) resistance += 8;
	if (p_ptr->resist_pois) resistance += 3;
	if (p_ptr->oppose_pois) resistance += 3;

	/* Adjust power based on resistance factor */
	pow = div_round(*damage * 10, resistance);

	/* Choose message based on adjusted power */
	if (pow >= *damage * 2)
		message(MSG_YELLOW, 0, "You feel deathly ill!");
	else if (pow > 3 * *damage / 2)
		message(MSG_YELLOW, 0, "You feel gravely ill!");
	else if (pow > *damage)
		message(MSG_YELLOW, 0, "You feel seriously ill!");
	else if (pow > 2 * *damage / 3)
		message(MSG_WHITE, 0, "You feel quite ill.");
	else if (pow > *damage / 2)
		message(MSG_WHITE, 0, "You feel ill.");
	else if (pow > 2 * *damage / 5)
		message(MSG_WHITE, 0, "You feel sick.");
	else if (pow >= *damage / 3)
		message(MSG_WHITE, 0, "You feel a little sick.");
	else
	{
		message(MSG_L_BLUE, 0, "You shake off the disease.");
		*damage /= 3;
		pow = 0;
	}


	/* If we did not shake off the disease */
	if (pow)
	{
		/* Reduce damage if resisted */
		if (*damage > pow) *damage = pow;

		/* Infect the character (fully cumulative).  Do not display messages. */
		set_diseased(p_ptr->diseased + *damage, "");

		/* Attack stats */
		for (i = 0; i < (5 + pow) / 5; i++)
		{
			/* Each attempt has a 10% chance of success */
			if (one_in_(10))
			{
				/* Damage a random stat */
				(void)do_dec_stat(rand_int(A_MAX), 1, FALSE, NULL, NULL);
			}
		}
	}


}


/*
 * Forget your inventory, your map, and objects on the floor.
 *
 * Automatically se-sense items being carried or worn.  -clefs-
 */
bool lose_all_info(cptr msg)
{
	int i;
	object_type *o_ptr;


	/* Forget info about objects */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- Skip pouch  XXX */
		if (i == INVEN_POUCH) continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Remove any special inscription */
		o_ptr->inscrip = 0;

		/* Hack -- Clear the "worn flag" */
		o_ptr->ident &= ~(IDENT_WORN);

		/* Hack -- Clear the "felt" flag */
		o_ptr->ident &= ~(IDENT_SENSE);

		/* Hack -- Clear the "known" flag */
		o_ptr->ident &= ~(IDENT_KNOWN);

		/* Hack -- Clear the "empty" flag */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}

	/* Forget all items on the floor -- allow a saving throw */
	for (i = 0; i < o_max; i++)
	{
		/* Get this object */
		o_ptr = &o_list[i];

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Allow a saving throw through wisdom  XXX */
		if (p_ptr->stat_ind[A_WIS] > randint(40)) continue;

		/* Remove any special inscription */
		o_ptr->inscrip = 0;

		/* Hack -- Clear the "worn flag" */
		o_ptr->ident &= ~(IDENT_WORN);

		/* Hack -- Clear the "felt" flag */
		o_ptr->ident &= ~(IDENT_SENSE);

		/* Hack -- Clear the "known" flag */
		o_ptr->ident &= ~(IDENT_KNOWN);

		/* Hack -- Clear the "empty" flag */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}


	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Mega-Hack -- Forget (but do not darken) the map */
	wiz_dark(FALSE);


	/* Print message */
	if (msg) msg_format("%s", msg);

	/* Update the map */
	handle_stuff();


	/* Automatically re-sense all carried items */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* In pack -- Sense weakly */
		if (i < INVEN_WIELD) sense_object(o_ptr, i, FALSE, FALSE);

		/* Being used -- Sense strongly */
		else                 sense_object(o_ptr, i, TRUE, FALSE);
	}

	/* Recalculate bonuses (again) */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* It worked */
	return (TRUE);
}


/*
 * Cursing a character can have all sorts of effects.  -LM-
 */
void curse_player(int power)
{
	int i;

	while (TRUE)
	{
		int choice = randint(100);

		/* Drain away luck (30%) */
		if (choice <= 30)
		{
			(void)set_luck(p_ptr->luck - rand_range((2 + power / 12), (10 + power / 3)),
				"You feel strangely unlucky...");
		}

		/* Reduce any stat (10%) */
		else if (choice <= 40)
		{
			/* Damage a stat if magic is not very weak */
			if (power >= 5)
			{
				(void)do_dec_stat(rand_int(A_MAX), 1, FALSE, NULL, NULL);
			}
		}

		/* Cancel any of various enhancement magics (20%) */
		else if (choice <= 60)
		{
			/* Randomly look for active magics */
			for (i = 0; i < 3; i++)
			{
				int choice2 = randint(2 + MIN(power / 12, 4));

				if ((choice2 == 6) && (p_ptr->wiz_prot))
				{
					set_wiz_prot(0);   break;
				}
				else if ((choice2 == 6) && (p_ptr->wraithform))
				{
					set_wraithform(0);   break;
				}
				else if ((choice2 == 5) && (p_ptr->holy))
				{
					set_holy(0);   break;
				}
				else if ((choice2 == 5) &&
							((p_ptr->shield) || (p_ptr->steelskin)))
				{
					set_shield(0, NULL);
					set_steelskin(0, NULL);   break;
				}
				else if ((choice2 == 4) && (p_ptr->forbid_summoning))
				{
					set_forbid_summoning(0);   break;
				}
				else if ((choice2 == 4) && (p_ptr->tim_invis))
				{
					set_invis(0, 0);   break;
				}
				else if ((choice2 == 3) && (p_ptr->protevil))
				{
					set_protevil(0);   break;
				}
				else if ((choice2 == 3) && (p_ptr->fast))
				{
					set_fast(0);   break;
				}
				else if ((choice2 == 2) && (p_ptr->hero))
				{
					set_hero(0);   break;
				}
				else if ((choice2 == 2) && (p_ptr->blessed))
				{
					set_blessed(0, NULL);   break;
				}
				else if ((choice2 <= 1) && (p_ptr->bold))
				{
					set_bold(0);   break;
				}
			}
		}

		/* Cause disease (5%) */
		else if (choice <= 65)
		{
			/* Damage depends on power */
			int dam = power * 2;

			/* Cause disease */
			disease(&dam);

			/* Hurt (not very much) */
			(void)take_hit(dam / 2, 0, NULL, "disease");
		}

		/* Suck away some exp (10%) */
		else if (choice <= 75)
		{
			s32b d = (power * power);

			/* Drain life */
			if (p_ptr->hold_life)
			{
				if (d >= 20)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(d / 10, FALSE);
				}
			}
			else
			{
				msg_print("You feel your life draining away!");
				lose_exp(d, FALSE);
			}
		}

		/* Curse an item in equipment (25%) */
		else
		{
			curse_equipment(MIN(power, 100));
		}


		/* Usually, but not always, stop after one effect */
		if (power < randint(100 + power)) break;
	}
}











/************************************************************************
 *                                                                      *
 *                        Monster-effect Magics                         *
 *                                                                      *
 ************************************************************************/


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_los(p_ptr->py, p_ptr->px, 30, GF_DO_SPEED));
}

/*
 * Slow monsters
 */
bool slow_monsters(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_SLOW));
}

/*
 * Slow undead
 */
bool slow_undead(int power)
{
	p_ptr->proj_mon_flags = (RF3_UNDEAD);
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_SLOW));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_SLEEP));
}


/*
 * Scare demons
 */
bool fear_demons(int power)
{
	p_ptr->proj_mon_flags = (RF3_DEMON);
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_FEAR));
}

/*
 * Scare monsters
 */
bool fear_monsters(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_FEAR));
}

/*
 * Confuse monsters.
 */
bool confu_monsters(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_CONF));
}

/*
 * Banish creatures having the specified flags in flags3.
 */
bool banishment(u32b flags, int power)
{
	p_ptr->proj_mon_flags = flags;
	return (project_los(p_ptr->py, p_ptr->px, power, GF_AWAY));
}

/*
 * Turn undead
 */
bool turn_undead(int power)
{
	p_ptr->proj_mon_flags = (RF3_UNDEAD);
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_TURN));
}

/*
 * Turn evil (special priest prayer)
 */
bool turn_evil_priest(int power)
{
	p_ptr->proj_mon_flags = (RF3_EVIL | RF3_UNDEAD | RF3_DEMON);
	return (project_los(p_ptr->py, p_ptr->px, power, GF_DO_TURN));
}


/*
 * Dispelling spells
 */
bool dispel_undead(int dam)
{
	p_ptr->proj_mon_flags = (RF3_UNDEAD);
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISPEL));
}

bool dispel_evil(int dam)
{
	p_ptr->proj_mon_flags = (RF3_EVIL);
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISPEL));
}

bool dispel_monsters(int dam)
{
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISPEL));
}

bool dispel_animals(int dam)
{
	p_ptr->proj_mon_flags = (RF3_ANIMAL);
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISPEL));
}

/*
 * Dispel all small monsters
 */
bool dispel_small_monsters(int dam)
{
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_DISPEL_SMALL));
}

/*
 * Dispel monsters who can't stand bright light
 */
bool dispel_light_hating(int dam)
{
	return (project_los(p_ptr->py, p_ptr->px, dam, GF_LITE_WEAK));
}

/*
 * Dispel demons and undead, try to panic evil
 */
bool exorcise_monsters(int power)
{
	bool notice = FALSE;

	p_ptr->proj_mon_flags = (RF3_UNDEAD | RF3_DEMON);
	if (project_los(p_ptr->py, p_ptr->px, power, GF_DISPEL)) notice = TRUE;

	p_ptr->proj_mon_flags = (RF3_EVIL);
	if (project_los(p_ptr->py, p_ptr->px, 60 + power/3, GF_DO_FEAR)) notice = TRUE;

	return (notice);
}

/*
 * Try to drive monsters mad
 */
bool judgement(int power)
{
	return (project_los(p_ptr->py, p_ptr->px, power, GF_MADNESS));
}


/*
 * Wake and hasten nearby monsters of a particular race.
 */
void aggravate_monster_race(u32b race_flag, cptr weapon, cptr monster)
{
	int i;

	bool flag = FALSE;

	/* Scan all monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Monster is of the requested race */
		if (r_ptr->flags3 & (race_flag))
		{
			/* Wake or speed up */
			if (m_ptr->cdis < MAX_SIGHT * 2)
			{
				/* Note presence of monster */
				flag = TRUE;

				/* Wake up */
				if (m_ptr->csleep)
				{
					m_ptr->csleep = 0;
				}

				/* Speed up */
				else
				{
					m_ptr->hasted = MIN(200, m_ptr->hasted + 5);
				}
			}
		}
	}

	/* Update monster display (always) */
	p_ptr->redraw |= (PR_HEALTH);

	/* Messages */
	if (flag)
		msg_format("Your %s challenges nearby %s ... you hear stirring!", weapon, monster);
	else
		msg_format("Your %s challenges nearby %s ... you hear no response.", weapon, monster);
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who, bool the_entire_level, cptr msg)
{
	int i;

	bool unsleep = FALSE;
	bool speed = FALSE;

	int y0 = p_ptr->py, x0 = p_ptr->px;

	if (who > 0)
	{
		y0 = m_list[who].fy;
		x0 = m_list[who].fx;
	}

	/* Scan all monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (if any) */
		if (i == who) continue;


		/* Wake up and hasten all monsters */
		if (the_entire_level)
		{
			/* Wake up */
			m_ptr->csleep = 0;

			/* Get mad */
			m_ptr->hasted = MIN(200, m_ptr->hasted + 100);

			break;
		}


		/* Handle character-centered aggravation */
		else if (who <= 0)
		{
			/* Speed up awake monsters in line of sight */
			if ((!m_ptr->csleep) &&
			    (player_has_los_bold(m_ptr->fy, m_ptr->fx)))
			{
				/* Speed up (not for very long, though) */
				m_ptr->hasted = MIN(200, m_ptr->hasted + 10);
				speed = TRUE;
			}

			/* Wake up nearby monsters */
			else if (m_ptr->cdis < MAX_SIGHT * 2)
			{
				/* Wake up */
				if (m_ptr->csleep)
				{
					m_ptr->csleep = 0;
					unsleep = TRUE;
				}
			}
		}

		/* Handle monster-centered aggravation  -JG */
		else
		{
			/* Speed up awake monsters in line of sight */
			if ((!m_ptr->csleep) &&
			    (los(y0, x0, m_ptr->fy, m_ptr->fx)))
			{
				/* Speed up (not for very long, though) */
				m_ptr->hasted = MIN(200, m_ptr->hasted + 10);
				speed = TRUE;
			}

			/* Wake up nearby monsters */
			else if (distance(y0, x0, m_ptr->fy, m_ptr->fx) < MAX_SIGHT)
			{
				/* Wake up */
				if (m_ptr->csleep)
				{
					m_ptr->csleep = 0;
					unsleep = TRUE;
				}
			}
		}
	}

	/* Update monster display (always) */
	p_ptr->redraw |= (PR_HEALTH);

	/* Messages */
	if (msg)
	{
		if (the_entire_level) message_format(MSG_L_RED, 0, "%s", msg);
		else                  message_format(MSG_WHITE, 0, "%s", msg);
	}
	else if (the_entire_level)
		message_format(MSG_L_RED, 0, "You hear angry stirring everywhere!");
	else if (speed)
		msg_print("You feel a sudden stirring nearby!");
	else if (unsleep)
		msg_print("You hear a sudden stirring in the distance!");
}


/*
 * Make monsters wary.  Option to make only monsters in LOS/earshot wary.
 * -JG-, -LM-
 */
bool make_monsters_wary(int y, int x, bool req_los, bool trap)
{
	int i;
	bool notice = FALSE;
	int dist = 10;  /* Earshot */


	/*
	 * If we require LOS, and the source grid is not illuminated, then
	 * only very nearby monsters are able to notice the event.
	 */
	if ((req_los) && (!(cave_info[y][x] & (CAVE_SEEN)) ||
	                  ((cave_info[y][x] & (CAVE_GLOW)))))
	{
		dist = 3;
	}

	/* Scan all monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Option -- Skip distant monsters not in LOS */
		if (req_los)
		{
			if ((distance(y, x, m_ptr->fy, m_ptr->fx) > dist) &&
			    (!los(y, x, m_ptr->fy, m_ptr->fx)))
			{
				continue;
			}
		}

		/* Skip stupid and motionless monsters */
		if (r_ptr->flags2 & (RF2_STUPID)) continue;
		if (r_ptr->flags1 & (RF1_NEVER_MOVE)) continue;

		/* Skip a lot of monsters, unless smart */
		else if ((strchr("BFJKMSXabceijlrsmvwjz,", r_ptr->d_char)) &&
			(!(r_ptr->flags2 & (RF2_SMART)))) continue;

		/* Skip monsters who are not paying attention */
		if ((m_ptr->csleep) || (m_ptr->confused) || (m_ptr->monfear)) continue;

		/* Monster is already wary */
		if (monster_wary(m_ptr)) continue;


		/* Make wary */
		mon_make_wary(m_ptr);

		/* Monster is fully visible; character is paying attention */
		if ((!p_ptr->image) && (!p_ptr->confused) &&
		    (mon_fully_visible(m_ptr)) &&
		    (player_can_see_bold(m_ptr->fy, m_ptr->fx)))
		{
			cptr note = "";
			char m_name[DESC_LEN];

			/* Describe the monster */
			monster_desc(m_name, m_ptr, 0x40);

			/* Message -- awareness */
			if ((strchr("phntyPdDGILOoTuUvV&", r_ptr->d_char)) ||
				(r_ptr->flags2 & (RF2_SMART)))
			{
				if (trap) note = "becomes aware of your crafty abilities.";
				else      note = "takes heed of your cunning tactics.";
			}

			/* Message -- instinct */
			else
			{
				if (trap) note = "senses your crafty abilities.";
				else      note = "senses you are a cunning foe.";
			}

			/* Message */
			msg_format("%^s %s", m_name, note);

			/* Take note */
			notice = TRUE;
		}
	}

	/* Return "anything seen" */
	return (notice);
}


/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool genocide(char typ)
{
	int i;

	int play = !typ;


	/* Mega-Hack -- Get a monster symbol */
	if (!typ)
	{
		if (!get_com("Choose a monster race (by symbol) to genocide:", &typ))
		{
			/* Allow cancel */
			return (FALSE);
		}
	}

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Hack -- Skip questor monsters */
		if ((m_ptr->r_idx == q_info[quest_num(p_ptr->depth)].r_idx))
		{
			continue;
		}

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		if (play) (void)take_hit(randint(4), 0, NULL, "the strain of casting Genocide");
	}

	return (TRUE);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(int y, int x)
{
	int i;

	bool result = FALSE;

	bool play = FALSE;
	if ((y == p_ptr->py) && (x == p_ptr->px)) play = TRUE;

	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Hack -- Skip questor monsters */
		if ((m_ptr->r_idx == q_info[quest_num(p_ptr->depth)].r_idx))
		{
			continue;
		}

		/* Skip distant monsters */
		if (distance(y, x, m_ptr->fy, m_ptr->fx) > MAX_SIGHT) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		if (play)
			(void)take_hit(randint(3), 0, NULL, "the strain of casting Mass Genocide");

		/* Note effect */
		result = TRUE;
	}

	return (result);
}


/*
 * Probe nearby monsters
 */
bool probing(void)
{
	int i;

	bool probe = FALSE;


	/* Must not be hallucinating */
	if (p_ptr->image) return (FALSE);

	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char m_name[DESC_LEN];

			/* Start the message */
			if (!probe) msg_print("Probing...");

			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);

			/* Describe the monster */
			if (!(r_ptr->mana))
				msg_format("%^s has %d hit points.", m_name, m_ptr->hp);
			else
				msg_format("%^s has %d hit points and %d mana.", m_name, m_ptr->hp, m_ptr->mana);

			/* If monster is fully visible, learn all of the non-spell, non-treasure flags */
			if ((m_ptr->ml >= ML_FULL) && (lore_do_probe(i)))
			{
				char buf[DESC_LEN];

				/* Get base name of monster */
				strcpy(buf, (r_name + r_ptr->name));

				/* Pluralize it (unless unique) */
				if (!(r_ptr->flags1 & (RF1_UNIQUE)))
				{
					plural_aux(buf);
				}

				/* Note that we learnt some new flags */
				msg_format("You now know more about %s.", buf);
			}

			/* Probe worked */
			probe = TRUE;
		}
	}

	/* Result */
	return (probe);
}


/*
 * Specialized bolt and beam functions.
 */
bool lite_line(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID;
	return (fire_bolt_beam_special(GF_LITE_WEAK, dir, damroll(4, 5),
	                               MAX_RANGE, flg));
}

bool drain_life(int dir, int dam)
{
	u32b flg = PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DEATH, dir, dam, MAX_RANGE, flg));
}

bool heal_monster(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_HEAL, dir, dam, MAX_RANGE, flg));
}

bool speed_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_SPEED, dir, 30, MAX_RANGE, flg));
}

bool slow_monster(int dir, int power)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_SLOW, dir, power, MAX_RANGE, flg));
}

bool sleep_monster(int dir, int power)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_SLEEP, dir, power, MAX_RANGE, flg));
}

bool confuse_monster(int dir, int power)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_CONF, dir, power, MAX_RANGE, flg));
}

bool stun_monster(int dir, int power)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_STUN, dir, power, MAX_RANGE, flg));
}

bool poly_monster(int dir, int power)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_POLY, dir, power, MAX_RANGE, flg));
}

bool clone_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_CLONE, dir, 0, MAX_RANGE, flg));
}

bool fear_monster(int dir, int power)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DO_FEAR, dir, power, MAX_RANGE, flg));
}

bool curse_monster(int dir, int power)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE | PROJECT_NO_TRAIL;
	return (fire_bolt_beam_special(GF_CURSE, dir, power, MAX_RANGE, flg));
}

bool teleport_monster(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_AWAY, dir, MAX_SIGHT * 5, MAX_RANGE, flg));
}

bool come_hither(int dir)
{
	u32b flg = PROJECT_STOP | PROJECT_HIDE | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_COME_HITHER, dir, 0, MAX_RANGE, flg));
}

/*
 * Dispel an evil creature, try to stun, slow, and confuse it.
 */
bool smite_evil(int dir, int power)
{
	u32b flg = PROJECT_STOP;
	p_ptr->proj_mon_flags = (RF3_EVIL);

	return (fire_bolt_beam_special(GF_SMITE, dir, power, MAX_RANGE, flg));
}

/*
 * Dispel demons and undead, try to panic evil
 */
bool exorcise_monster(int dir, int power)
{
	bool notice = FALSE;

	u32b flg = PROJECT_STOP | PROJECT_KILL;

	p_ptr->proj_mon_flags = (RF3_UNDEAD | RF3_DEMON);
	if (fire_bolt_beam_special(GF_DISPEL, dir, power, MAX_RANGE, flg))
		notice = TRUE;

	p_ptr->proj_mon_flags = (RF3_EVIL);
	if (fire_bolt_beam_special(GF_DO_FEAR, dir, power, MAX_RANGE, flg))
		notice = TRUE;

	return (notice);
}

bool dispel_an_undead(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_DISPEL, dir, dam, MAX_RANGE, flg));
}

bool dispel_a_demon(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_DISPEL, dir, dam, MAX_RANGE, flg));
}

bool dispel_a_dragon(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_DISPEL, dir, dam, MAX_RANGE, flg));
}



/*
 * Specialized adjacent area spells
 */

bool sleep_monsters_touch(int power)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_KILL | PROJECT_HIDE;

	return (project(-1, 1, py, px, py, px, power, GF_DO_SLEEP, flg, 0, 20));
}





/************************************************************************
 *                                                                      *
 *                        Detection and mapping                         *
 *                                                                      *
 ************************************************************************/


/*
 * Fix the bounds of a detection area.  -LM-
 *
 * We detect within a square-shaped area consisting of 11x11 dungeon blocks.
 * Usually, this area is 5 grids wide and five high; if extended, it is
 * 7 on a side.  The reason for matching detection area to 11x11 blocks
 * is because the dungeon is laid out and panels usually scroll using the
 * same increments.
 */
static void get_detection_area(int *top, int *left, int *bottom, int *right,
	bool extended)
{
	/* Set an illegal detection centerpoint to character's grid */
	if (!in_bounds(detect_y, detect_x))
	{
		detect_y = p_ptr->py;
		detect_x = p_ptr->px;
	}

	/* Figure out which 11x11 grid the character is in */
	*top  = (detect_y / BLOCK_HGT) * BLOCK_HGT;
	*left = (detect_x / BLOCK_WID) * BLOCK_WID;

	/* Extend out either two or three 11x11 grids in all directions */
	*top  -= BLOCK_HGT * (extended ? 3 : 2);
	*left -= BLOCK_WID * (extended ? 3 : 2);
	*bottom = *top + BLOCK_HGT * (extended ? 7 : 5);
	*right = *left + BLOCK_WID * (extended ? 7 : 5);

	/* Test for legality (vertical) */
	if (*top < 0) *top = 0;
	if (*bottom > dungeon_hgt) *bottom = dungeon_hgt;

	/* Test for legality (horizontal) */
	if (*left < 0) *left = 0;
	if (*right > dungeon_wid) *right = dungeon_wid;

	/* Assume next detection centerpoint to be character's grid */
	detect_y = -1;
	detect_x = -1;
}


/*
 * Detect all traps nearby
 *
 * Allow extended detection.  Allow messages if no traps are detected.
 */
bool detect_traps(bool extended, bool verbose)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan the detection area */
	for (y = top; y < bottom; y++)
	{
		for (x = left; x < right; x++)
		{
			/* Learn about traps (but not loose rocks) */
			if (cave_invisible_trap(y, x))
			{
				if (reveal_trap(y, x, 100, FALSE, FALSE))
				{
					detect = TRUE;
				}
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of traps!");
	}
	else if (verbose)
	{
		msg_print("You sense no traps nearby.");
	}

	/* Result */
	return (detect);
}



/*
 * Detect all doors nearby
 */
bool detect_doors(bool extended)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan the detection area */
	for (y = top; y < bottom; y++)
	{
		for (x = left; x < right; x++)
		{
			/* Reveal secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);
			}

			/* Detect doors */
			if (cave_any_door(y, x))
			{
				/* Note new doors */
				if (!(cave_info[y][x] & (CAVE_MARK)))
				{
					detect = TRUE;

					/* Hack -- Memorize */
					cave_info[y][x] |= (CAVE_MARK);
				}

				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of doors!");
	}


	/* Result */
	return (detect);
}


/*
 * Detect all stairs nearby
 */
bool detect_stairs(bool extended)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan the detection area */
	for (y = top; y < bottom; y++)
	{
		for (x = left; x < right; x++)
		{
			/* Detect stairs */
			if (cave_any_stairs(y, x))
			{
				/* Note new stairs */
				if (!(cave_info[y][x] & CAVE_MARK))
				{
					detect = TRUE;

					/* Hack -- Memorize */
					cave_info[y][x] |= (CAVE_MARK);
				}

				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of stairs!");
	}


	/* Result */
	return (detect);
}

/*
 * Detect any treasure nearby
 */
bool detect_treasure(bool extended)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan the detection area */
	for (y = top; y < bottom; y++)
	{
		for (x = left; x < right; x++)
		{
			/* Notice embedded gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_H) ||
				(cave_feat[y][x] == FEAT_QUARTZ_H))
			{
				/* Expose the gold */
				cave_feat[y][x] += 0x02;
			}

			/* Magma/Quartz + Known Gold */
			if ((cave_feat[y][x] == FEAT_MAGMA_K) ||
				(cave_feat[y][x] == FEAT_QUARTZ_K))
			{
				/* Note new treasure */
				if (!(cave_info[y][x] & CAVE_MARK))
				{
					detect = TRUE;

					/* Hack -- Memorize */
					cave_info[y][x] |= (CAVE_MARK);
				}

				/* Redraw */
				lite_spot(y, x);
			}

			/* Pillars of gold */
			if (cave_feat[y][x] == FEAT_PILLAR_GOLD)
			{
				/* Note new treasure */
				if (!(cave_info[y][x] & CAVE_MARK))
				{
					detect = TRUE;

					/* Hack -- Memorize */
					cave_info[y][x] |= (CAVE_MARK);
				}

				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of buried treasure!");
	}

	/* Result */
	return (detect);
}



/*
 * Detect all "gold" objects nearby
 */
bool detect_objects_gold(bool extended)
{
	int i, y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if ((y < top) || (y >= bottom) || (x < left) || (x >= right)) continue;

		/* Detect "gold" objects */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Note new gold */
			if (!o_ptr->marked)
			{
				detect = TRUE;

				/* Hack -- memorize */
				o_ptr->marked = TRUE;
			}

			/* Redraw */
			lite_spot(y, x);
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of treasure!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "normal" objects nearby
 */
bool detect_objects_normal(bool extended)
{
	int i, y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if ((y < top) || (y >= bottom) || (x < left) || (x >= right)) continue;

		/* Detect all objects, except gold and essences */
		if ((o_ptr->tval != TV_GOLD) && (o_ptr->tval != TV_ESSENCE))
		{
			/* Note new non-gold */
			if (!o_ptr->marked)
			{
				detect = TRUE;

				/* Hack -- memorize */
				o_ptr->marked = TRUE;
			}

			/* Redraw */
			lite_spot(y, x);
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of objects!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "magic" objects nearby.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.  It does not detect
 * essences (this is important).
 */
bool detect_objects_magic(bool extended)
{
	int i, y, x, tv;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan all objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if ((y < top) || (y >= bottom) || (x < left) || (x >= right)) continue;

		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artifacts, misc magic items, or enchanted wearables */
		if (artifact_p(o_ptr) || ego_item_p(o_ptr) ||
		    (tv == TV_AMULET) || (tv == TV_RING) ||
		    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
		    (tv == TV_SCROLL) || (tv == TV_POTION) ||
		    (tv == TV_MAGIC_BOOK) || (tv == TV_PRAYER_BOOK) ||
		    (tv == TV_NATURE_BOOK) || (tv == TV_DARK_BOOK) ||
		    (o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0))
		{
			/* Note new magic objects */
			if (!o_ptr->marked)
			{
				detect = TRUE;

				/* Hack -- memorize */
				o_ptr->marked = TRUE;
			}

			/* Redraw */
			lite_spot(y, x);
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of magic objects!");
	}

	/* Return result */
	return (detect);
}


/*
 * Detect objects and gold in the current room, or in line of sight.
 */
bool detect_objects_in_room(int y0, int x0)
{
	int i, y, x;
	bool gold = FALSE;
	bool objects = FALSE;


	/* Mark nearby grids, fill "temp" array */
	spread_cave_temp(y0, x0, 0, TRUE);

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only objects in the current room or in line of sight */
		if (!(cave_info[y][x] & (CAVE_TEMP | CAVE_VIEW))) continue;

		/* Detect all objects except essences */
		if (o_ptr->tval != TV_ESSENCE)
		{
			/* Note new object */
			if (!o_ptr->marked)
			{
				if (o_ptr->tval == TV_GOLD) gold = TRUE;
				else                        objects = TRUE;

				/* Hack -- memorize */
				o_ptr->marked = TRUE;
			}

			/* Redraw */
			lite_spot(y, x);
		}
	}

	/* Describe */
	if (gold || objects)
	{
		if (!gold) msg_print("You sense the presence of objects!");
		else if (!objects) msg_print("You sense the presence of gold!");
		else msg_print("You sense the presence of objects and gold!");
	}


	/* Scan the "temp" array */
	for (i = 0; i < temp_n; i++)
	{
		y = temp_y[i];
		x = temp_x[i];

		/* Unmark */
		cave_info[y][x] &= ~(CAVE_TEMP);
	}

	/* None left */
	temp_n = 0;

	/* Return "detected anything" */
	if (gold || objects) return (TRUE);
	else return (FALSE);
}


/*
 * Detect monsters nearby
 *
 * If "match" is TRUE, we detect monsters with any of the given flags.
 * Otherwise, we detect monsters having none of the given flags.
 *
 * If "flags" is empty, we detect all monsters.
 *
 * Update the monster lore for all detected monsters by adding the given
 * lore flags.
 */
bool detect_monsters(bool extended, bool match, u32b flags, int flag_set,
	const char *str, bool verbose)
{
	int i, y, x;

	bool flag = FALSE;

	u32b tmp_flags = 0L;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extended);

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		bool hit = FALSE;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if ((y < top) || (y >= bottom) || (x < left) || (x >= right)) continue;

		/* Extract the correct flag set */
		switch (flag_set)
		{
			case 1:  tmp_flags = r_ptr->flags1;  break;
			case 2:  tmp_flags = r_ptr->flags2;  break;
			case 3:  tmp_flags = r_ptr->flags3;  break;
			case 4:  tmp_flags = r_ptr->flags4;  break;
			case 5:  tmp_flags = r_ptr->flags5;  break;
			case 6:  tmp_flags = r_ptr->flags6;  break;
			case 7:  tmp_flags = r_ptr->flags7;  break;
		}

		/* Matches the given flag set */
		if (tmp_flags & (flags)) hit = TRUE;

		/* Detect all appropriate monsters */
		if ((!flags) || (match == hit))
		{
			/* Detect */
			flag = TRUE;

			/* Monster will stay fully visible through next turn */
			m_ptr->mflag |= (MFLAG_FULL | MFLAG_SHOW);
			repair_mflag_show = FALSE;

			/* XXX XXX - Mimics are revealed */
			m_ptr->mflag &= ~(MFLAG_MIME);

			/* Learn about attributes that allow detection */
			if (match)
			{
				switch (flag_set)
				{
					case 1:  l_ptr->flags1 |= (flags & (r_ptr->flags1));  break;
					case 2:  l_ptr->flags2 |= (flags & (r_ptr->flags2));  break;
					case 3:  l_ptr->flags3 |= (flags & (r_ptr->flags3));  break;
					case 4:  l_ptr->flags4 |= (flags & (r_ptr->flags4));  break;
					case 5:  l_ptr->flags5 |= (flags & (r_ptr->flags5));  break;
					case 6:  l_ptr->flags6 |= (flags & (r_ptr->flags6));  break;
					case 7:  l_ptr->flags7 |= (flags & (r_ptr->flags7));  break;
				}
			}

			/* Update the monster */
			(void)update_mon(i, FALSE);
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_format("You sense the presence of %s!", str);
	}
	else if (verbose)
	{
		msg_print("You feel safe.");
	}

	/* Result */
	return (flag);
}

/*
 * Detect all non-invisible monsters nearby.
 */
bool detect_monsters_normal(bool extended, bool verbose)
{
	return (detect_monsters(extended, FALSE, RF2_INVISIBLE,
	        2, "monsters", verbose));
}

/*
 * Detect all invisible monsters nearby.
 */
bool detect_monsters_invis(bool extended, bool verbose)
{
	return (detect_monsters(extended, TRUE, RF2_INVISIBLE, 2,
		"invisible creatures", verbose));
}


/*
 * Detect all evil monsters nearby.
 */
bool detect_evil(bool extended, bool verbose)
{
	return (detect_monsters(extended, TRUE, RF3_EVIL, 3,
		"evil", verbose));
}

/*
 * Detect all undead monsters nearby.
 */
bool detect_undead(bool extended, bool verbose)
{
	return (detect_monsters(extended, TRUE, RF3_UNDEAD, 3,
		"undead", verbose));
}

/*
 * Detect all non-undead, non-demonic monsters nearby.
 */
bool detect_life(bool extended, bool verbose)
{
	return (detect_monsters(extended, FALSE, RF3_UNDEAD | RF3_DEMON, 3,
		"life", verbose));
}

/*
 * Detect all animals nearby.
 */
bool detect_animals(bool extended, bool verbose)
{
	return (detect_monsters(extended, TRUE, RF3_ANIMAL, 3,
		"animals", verbose));
}

/*
 * Detect all monsters nearby.
 */
bool detect_all_monsters(bool extended, bool verbose)
{
	return (detect_monsters(extended, TRUE, 0L, 3,
		"monsters", verbose));
}

/*
 * Detect everything nearby.
 */
bool detect_all(bool extended, bool verbose)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps(extended, verbose)) detect = TRUE;
	if (detect_doors(extended))          detect = TRUE;
	if (detect_stairs(extended))         detect = TRUE;
	if (detect_treasure(extended))       detect = TRUE;
	if (detect_objects_gold(extended))   detect = TRUE;
	if (detect_objects_normal(extended)) detect = TRUE;

	if (detect_all_monsters(extended, verbose))   detect = TRUE;

	/* Result */
	return (detect);
}






/************************************************************************
 *                                                                      *
 *                        Dungeon-effect Magics                         *
 *                                                                      *
 ************************************************************************/


/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r, bool full, bool hit_center)
{
	int y, x, k, t;

	/* Assume character not affected */
	bool flag = FALSE;


	/* Unused parameter */
	(void)full;


	/* Big area of effect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			/* Skip illegal grids */
			if (!in_bounds_fully(y, x)) continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Ignore vault squares */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Lose room */
			cave_info[y][x] &= ~(CAVE_ROOM);

			/* Lose light and knowledge */
			cave_info[y][x] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Hack -- Notice player affect */
			if (cave_m_idx[y][x] < 0)
			{
				/* Hurt the player later */
				if (!p_ptr->wraithform) flag = TRUE;

				/* Do not hurt this grid */
				continue;
			}

			/* Hack -- Usually skip the epicenter */
			if ((y == y1) && (x == x1) && (!hit_center)) continue;

			/* Delete the monster (if any) */
			delete_monster(y, x);

			/* Destroy all non-permanent grids */
			if (!cave_perma_bold(y, x))
			{
				object_type *o_ptr;
				int feat = FEAT_NONE;

				/* Scan all objects in the grid */
				for (o_ptr = get_first_object(y, x); o_ptr;
				     o_ptr = get_next_object(o_ptr))
				{
					/* Hack -- Preserve unknown artifacts */
					if (artifact_p(o_ptr) && !object_known_p(o_ptr))
					{
						/* Mega-Hack -- Preserve the artifact */
						a_info[o_ptr->artifact_index].cur_num = 0;
					}
				}

				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 20) feat = FEAT_WALL_EXTRA;

				/* Quartz */
				else if (t < 70) feat = FEAT_QUARTZ;

				/* Magma */
				else if (t < 100) feat = FEAT_MAGMA;

				/* Floor */
				else feat = get_nearby_floor(y, x);

				/* Change the feature */
				cave_set_feat(y, x, feat);

				/* Traps get crushed if no longer allowed */
				if (!cave_trap_allowed(y, x)) remove_trap(y, x, -1);
			}
		}
	}


	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
		msg_print("There is a searing blast of light!");

		/* Blind the player */
		if (!p_ptr->resist_blind && !p_ptr->resist_lite)
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + rand_range(25, 50),
				"You are blinded!");
		}
	}

	/* Hard not to notice */
	add_wakeup_chance = MAX(add_wakeup_chance, 500000);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}

/*
 * The wizard's patented destruction spell
 */
bool call_destruction(bool safe)
{
	/* Assume illegal target */
	int ty = -255;
	int tx = -255;

	int dir;


	/* We've got it under control */
	if (safe)
	{
		/* Ask until satisfied */
		while (TRUE)
		{
			/* Chose a target */
			if (!get_aim_dir(&dir)) return (FALSE);

			/* Update target */
			if ((dir == 5) && (target_okay()))
			{
				/* Target must always be in field of fire */
				if (player_can_fire_bold(p_ptr->target_row, p_ptr->target_col))
				{
					ty = p_ptr->target_row;
					tx = p_ptr->target_col;
					break;
				}
			}

			/* We need a valid target */
			msg_print("Please choose a target in line of fire.");

			/* Hack -- reset direction, force targeting */
			p_ptr->command_dir = dir = 0;
		}

		/* Cast word of destruction (randomizing helps prevent abuse) */
		destroy_area(ty, tx, randint(4), TRUE, TRUE);
	}

	/* We're out of control */
	else
	{
		/* Find any grid in LOS */
		scatter(&ty, &tx, p_ptr->py, p_ptr->px, 5, 0);

		/* Cast word of destruction */
		destroy_area(ty, tx, rand_range(4, 8), TRUE, TRUE);
	}

	return (TRUE);
}


/*
 * Collapse the ceiling.  Create rubble, bash player and monsters, destroy
 * all non-artifact objects.  Damage decreases with increasing distance from
 * center.
 *
 * Severity controls how much damage we are allowed to do to the character
 * and monsters, and also effects radius of effect.
 */
bool collapse_ceiling(int cy, int cx, int severity)
{
	int i, y, x, yy, xx, dy, dx;

	int damage = 0;
	int dist, radius;

	int grid_dam[33][33];


	/* Cannot collapse the sky */
	if (p_ptr->depth == 0) return (FALSE);


	/* Determine maximum range (16 max) */
	radius = 6 + severity / 10;
	if (radius > 16) radius = 16;

	/* Clear the collapse area */
	for (y = 0; y < 33; y++)
	{
		for (x = 0; x < 33; x++)
		{
			grid_dam[y][x] = 0;
		}
	}

	/* Scan the entire collapse area */
	for (dy = -radius; dy <= radius; dy++)
	{
		for (dx = -radius; dx <= radius; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip illegal grids */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Skip grids not in LOS */
			if (!los(cy, cx, yy, xx)) continue;

			/* Calculate if this grid can be affected */
			if (TRUE)
			{
				/* Get base distance.  Must not be 0. */
				dist = distance(cy, cx, yy, xx);
				if (dist < 1) dist = 1;

				/* Impassable terrain in adjacent grids offer protection */
				for (i = 0; i < 8; i++)
				{
					/* Get adjacent grid */
					y = yy + ddy[i];
					x = xx + ddx[i];

					/* Increase effective distance from source */
					if (!cave_passable_bold(y, x)) dist++;
				}

				/* Skip distant grids - non-room grids are tougher */
				if (dist > (cave_info[yy][xx] & (CAVE_ROOM) ? radius : radius / 2))
				{
					continue;
				}
			}


			/* Lose room (but not vault) */
			cave_info[yy][xx] &= ~(CAVE_ROOM);

			/* Lose light and knowledge */
			cave_info[yy][xx] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Get effective damage */
			damage = severity * (severity / 20) / dist;
			if (damage > severity) damage = severity;

			/* Damage this grid */
			grid_dam[16+yy-cy][16+xx-cx] = damage;
		}
	}

	/* Collapse the ceiling outwards from the origin */
	for (dist = 0; dist <= radius; dist++)
	{
		/* Pause */
		pause_for(op_ptr->delay_factor * op_ptr->delay_factor);

		for (dy = -dist; dy <= dist; dy++)
		{
			for (dx = -dist; dx <= dist; dx++)
			{
				/* Extract the location */
				yy = cy + dy;
				xx = cx + dx;

				/* Skip unaffected grids */
				if (!grid_dam[16+yy-cy][16+xx-cx]) continue;

				/* Affect only grids at the current distance */
				if (distance(cy, cx, yy, xx) != dist) continue;

				/* Delay (briefly) */
				pause_for(op_ptr->delay_factor);

				/* Get damage */
				damage = grid_dam[16+yy-cy][16+xx-cx];

				/* Handle character */
				if ((cave_m_idx[yy][xx] < 0) && (!p_ptr->wraithform))
				{
					/* Calculate damage - dodging skill helps */
					switch (randint(10) - randint(get_skill(S_DODGING, 0, 10)))
					{
						case 3: case 4: case 5:
						{
							damage = damroll(1, damage);
							(void)take_hit(damage, 0, "You are pummeled with debris!",
								"a collapsing ceiling");

							(void)set_stun(p_ptr->stun + randint(5));
							break;
						}
						case 6: case 7:
						{
							damage = damroll(2, damage);
							(void)take_hit(damage, 0, "You are bashed by rubble!",
								"a collapsing ceiling");

							(void)set_stun(p_ptr->stun + randint(20));
							break;
						}
						case 8: case 9:
						{
							damage = damroll(3, damage);
							(void)take_hit(damage, 0, "You are crushed by falling rock!",
								"a collapsing ceiling");

							(void)set_stun(p_ptr->stun + rand_range(10, 30));
							break;
						}
						case 10:
						{
							damage = damroll(5, damage);
							(void)take_hit(damage, 0, "You are severely crushed!",
								"a collapsing ceiling");

							(void)set_stun(p_ptr->stun + rand_range(20, 50));
							break;
						}
						case 0: case 1: case 2:
						default:
						{
							msg_print("You nimbly dodge aside!");
							damage = 0;
							break;
						}
					}
				}

				/* Process monsters */
				else if (cave_m_idx[yy][xx] > 0)
				{
					bool fear;
					monster_type *m_ptr = &m_list[cave_m_idx[yy][xx]];
					monster_race *r_ptr = &r_info[m_ptr->r_idx];

					/* Most monsters do not like rubble on top of them */
					if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
					    !(r_ptr->flags2 & (RF2_PASS_WALL)) &&
					    !(r_ptr->flags3 & (RF3_HURT_ROCK)))
					{
						char m_name[DESC_LEN];

						/* Describe the monster */
						monster_desc(m_name, m_ptr, 0x40);

						/* Hurt monster - allow fear */
						(void)mon_take_hit(cave_m_idx[yy][xx], 0,
						        damroll(rand_int(5), damage), &fear,
						        format("%^s is crushed!", m_name));
					}
				}

				/* Place rubble */
				if (cave_valid_bold(yy, xx))
				{
					/* Delete objects */
					delete_object(yy, xx);

					/* Change the feature */
					cave_set_feat(yy, xx, FEAT_RUBBLE);

					/* Traps get crushed */
					remove_trap(yy, xx, -1);
				}
			}
		}
	}

	/* Hard not to notice */
	add_wakeup_chance = MAX(add_wakeup_chance, 50000);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Ceiling got collapsed */
	return (TRUE);
}


/*
 * Produce an earthquake of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when genocided.
 *
 * Note that players and monsters (except eaters of walls and passers
 * through walls) will never occupy the same grid as a wall (or door).
 */
void earthquake(int cy, int cx, int r)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, t, y, x, yy, xx, dy, dx;
	int feat;

	int damage = 0;

	int sn = 0, sy = 0, sx = 0;

	bool hurt = FALSE;

	bool map[32][32];


	/* Paranoia -- Enforce maximum range */
	if (r > 12) r = 12;

	/* Clear the "maximal blast" area */
	for (y = 0; y < 32; y++)
	{
		for (x = 0; x < 32; x++)
		{
			map[y][x] = FALSE;
		}
	}

	/* Check around the epicenter */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip illegal grids */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Skip distant grids */
			if (distance(cy, cx, yy, xx) > r) continue;

			/* Lose room and vault */
			cave_info[yy][xx] &= ~(CAVE_ROOM);

			/* Lose light and knowledge */
			cave_info[yy][xx] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (!one_in_(4)) continue;

			/* Damage this grid */
			map[16+yy-cy][16+xx-cx] = TRUE;

			/* Hack -- Take note of player damage */
			if ((yy == py) && (xx == px))
			{
				if (!p_ptr->wraithform) hurt = TRUE;
			}
		}
	}

	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			/* Access the grid */
			y = py + ddy_ddd[i];
			x = px + ddx_ddd[i];

			/* Skip non-empty grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+y-cy][16+x-cx]) continue;

			/* Count "safe" grids, apply the randomizer */
			if ((++sn > 1) && (!one_in_(sn))) continue;

			/* Save the safe location */
			sy = y; sx = x;
		}

		/* Random message */
		switch (randint(3))
		{
			case 1:
			{
				if (p_ptr->depth)
					msg_print("The cave ceiling collapses!");
				else
					msg_print("The ground jerks suddenly!");
				break;
			}
			case 2:
			{
				if (p_ptr->depth)
					msg_print("The cave floor twists in an unnatural way!");
				else msg_print("The ground twists in an unnatural way!");
				break;
			}
			default:
			{
				if (p_ptr->depth) msg_print("The cave quakes!");
				else msg_print("The ground writhes violently!");

				msg_print("You are pummeled with debris!");
				break;
			}
		}

		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
			damage = damroll(5, 80);
			(void)take_hit(damage, 0, "You are severely crushed!", "an earthquake");

		}

		/* Destroy the grid, and push the player to safety */
		else
		{
			/* Calculate results */
			switch (randint(3))
			{
				case 1:
				{
					msg_print("You nimbly dodge the blast!");
					damage = 0;
					break;
				}
				case 2:
				{
					damage = damroll(10, 4);
					(void)take_hit(damage, 0, "You are bashed by rubble!", "an earthquake");

					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
				case 3:
				{
					damage = damroll(10, 8);
					(void)take_hit(damage, 0,
						p_ptr->depth ? "You are crushed between the floor and ceiling!" : "You are severely shaken!",
						"an earthquake");

					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
			}

			/* Move player */
			monster_swap(py, px, sy, sx);
		}
	}


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Process monsters */
			if (cave_m_idx[yy][xx] > 0)
			{
				monster_type *m_ptr = &m_list[cave_m_idx[yy][xx]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
				    !(r_ptr->flags2 & (RF2_PASS_WALL)))
				{
					char m_name[DESC_LEN];

					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Access the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							/* Skip grids that the monster cannot exist in */
							if (!cave_exist_mon(r_ptr, y, x, FALSE, FALSE)) continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							/* Count "safe" grids, apply the randomizer */
							if ((++sn > 1) && (!one_in_(sn))) continue;

							/* Save the safe grid */
							sy = y;
							sx = x;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0x40);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : damroll(5, 80));

					/* Monster is certainly awake */
					m_ptr->csleep = 0;

					/* Apply damage directly  XXX XXX */
					m_ptr->hp -= damage;

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
						msg_format("%^s is embedded in the rock!", m_name);

						/* Delete the monster */
						delete_monster(yy, xx);

						/* No longer safe */
						sn = 0;
					}

					/* Hack -- Escape from the rock */
					if (sn)
					{
						/* Move the monster */
						monster_swap(yy, xx, sy, sx);
					}
				}
			}
		}
	}


	/* XXX XXX XXX */

	/* New location */
	py = p_ptr->py;
	px = p_ptr->px;

	/* Important -- no wall on player */
	map[16+py-cy][16+px-cx] = FALSE;


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				/* Delete objects */
				delete_object(yy, xx);

				/* Currently a wall grid */
				if (cave_wall_bold(yy, xx))
				{
					/* Make it a floor grid */
					feat = get_nearby_floor(yy, xx);
				}

				/* A monster is here */
				else if (cave_m_idx[yy][xx] > 0)
				{
					/* Dump rubble on it */
					feat = FEAT_RUBBLE;
				}

				/* All other cases */
				else
				{
					/* Choose randomly */
					t = rand_int(120);

					if      (t < 20) feat = FEAT_WALL_EXTRA;
					else if (t < 55) feat = FEAT_QUARTZ;
					else if (t < 90) feat = FEAT_MAGMA;
					else             feat = FEAT_RUBBLE;
				}

				/* Change the feature */
				cave_set_feat(yy, xx, feat);

				/* Traps get crushed if no longer allowed */
				if (!cave_trap_allowed(yy, xx)) remove_trap(yy, xx, -1);
			}
		}
	}

	/* Hard not to notice */
	add_wakeup_chance = MAX(add_wakeup_chance, 50000);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Light up and clear all grids currently in the "temp" array.
 *
 * Monsters will usually wake up when illuminated.
 */
static void cave_temp_room_lite(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Perma-Lite */
		cave_info[y][x] |= (CAVE_GLOW);
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Update stuff */
	update_stuff();

	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* Redraw the grid */
		lite_spot(y, x);

		/* Process affected monsters */
		if (cave_m_idx[y][x] > 0)
		{
			/* It's hard to ignore bright lights shining in your eyes */
			int do_disturb = rand_range(50, 250);

			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

			/* Monster is sleeping */
			if (m_ptr->csleep)
			{
				/* Wake up or be disturbed */
				if (m_ptr->csleep <= do_disturb)
				{
					/* Wake up! */
					m_ptr->csleep = 0;

					/* Notice the "waking up" */
					if (mon_fully_visible(m_ptr))
					{
						char m_name[DESC_LEN];

						/* Get the monster name */
						monster_desc(m_name, m_ptr, 0x40);

						/* Dump a message */
						msg_format("%^s wakes up.", m_name);
					}
				}
				else
				{
					m_ptr->csleep -= do_disturb;
				}
			}
		}
	}

	/* None left */
	temp_n = 0;
}


/*
 * Darken and clear all grids currently in the "temp" array.
 *
 * Some grids will also be unmarked.
 */
static void cave_temp_room_unlite(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);

		/* Darken the grid */
		cave_info[y][x] &= ~(CAVE_GLOW);

		/* Hack -- Forget all grids that allow line of sight */
		if (cave_los_bold(y, x))
		{
			/* Forget the grid */
			cave_info[y][x] &= ~(CAVE_MARK);
		}
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Update stuff */
	update_stuff();

	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* Redraw the grid */
		lite_spot(y, x);
	}

	/* None left */
	temp_n = 0;
}


/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
	/* Mark nearby grids */
	spread_cave_temp(y1, x1, 0, TRUE);

	/* Now, light them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken any room containing the given location
 */
void unlite_room(int y1, int x1)
{
	/* Mark nearby grids */
	spread_cave_temp(y1, x1, 0, TRUE);

	/* Now, darken them all at once */
	cave_temp_room_unlite();
}

/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by white light.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, py, px, dam, GF_LITE_WEAK, flg, 0, 0);

	/* Light up the room */
	lite_room(py, px);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, py, px, dam, GF_DARK_WEAK, flg, 0, 0);

	/* Darken the room */
	unlite_room(py, px);

	/* Assume seen */
	return (TRUE);
}


/*
 * Concentrate light, note how much we gained.
 *
 * Allow use by both the character and monsters.
 *
 * -DarkGod-, -LM-
 */
int concentrate_light(int who, int y0, int x0, int radius, char *desc,
	size_t desc_len, bool for_real)
{
	int light = 0;
	int y, x, r;


	/* Hack -- Flush any pending output */
	handle_stuff();

	/* Drain light inwards */
	for (r = radius; r >= 0; r--)
	{
		/* Scan the grids in range */
		for (y = y0 - r; y <= y0 + r; y++)
		{
			for (x = x0 - r; x <= x0 + r; x++)
			{
				/* Stay legal */
				if (!in_bounds(y, x)) continue;

				/* Must permit line of sight */
				if (!cave_los_bold(y, x)) continue;

				/* Drain this distance only */
				if (distance(y, x, y0, x0) != r) continue;

				/* Must be in line of sight */
				if (!los(y, x, y0, x0)) continue;

				/* Grid has light */
				if (cave_info[y][x] & (CAVE_GLOW))
				{
					/* Count this grid */
					light++;

					/* We're doing this for real, boys */
					if (for_real)
					{
						/* No longer in the array */
						cave_info[y][x] &= ~(CAVE_TEMP);

						/* Darken the grid */
						cave_info[y][x] &= ~(CAVE_GLOW);

						/* Forget all grids that allow line of sight */
						if (cave_los_bold(y, x))
						{
							/* Forget the grid */
							cave_info[y][x] &= ~(CAVE_MARK);
						}

						/* Process affected monsters */
						if (cave_m_idx[y][x] > 0)
						{
							/* Update the monster */
							(void)update_mon(cave_m_idx[y][x], FALSE);
						}

						/* Redraw */
						lite_spot(y, x);
					}
				}
			}
		}

		/* Graphics */
		if ((for_real) && (op_ptr->delay_factor))
		{
			/* Screen refresh */
			(void)Term_fresh();

			/* Update the view (now) */
			p_ptr->update |= (PU_UPDATE_VIEW);
			handle_stuff();

			/* Longish delay for character */
			if (who <= 0)
				pause_for(50 + op_ptr->delay_factor * op_ptr->delay_factor);

			/* Allow a brief one for monsters */
			else if (op_ptr->delay_factor > 2)
				pause_for(op_ptr->delay_factor);
		}
	}

	/* Update the view (later) */
	if (for_real) p_ptr->update |= (PU_UPDATE_VIEW);


	/* Vary description based on strength */
	if      (light <=  10) (void)strnfmt(desc, desc_len, "in a tiny bolt.");
	else if (light <=  25) (void)strnfmt(desc, desc_len, "in a small bolt.");
	else if (light <=  45) (void)strnfmt(desc, desc_len, "as a bolt.");
	else if (light <=  70) (void)strnfmt(desc, desc_len, "as a large bolt.");
	else if (light <= 100) (void)strnfmt(desc, desc_len, "as a large bolt!");
	else if (light <= 145) (void)strnfmt(desc, desc_len, "in a powerful bolt!");
	else                   (void)strnfmt(desc, desc_len, "in a massive bolt!");

	/* Note how much light we concentrated */
	return (light);
}



/*
 * Create stairs at the character's location
 */
void stair_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(py, px);

	/* Create a staircase */
	if (!p_ptr->depth || p_ptr->character_type == PCHAR_IRONMAN)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
	   (p_ptr->depth >= MAX_DEPTH - 1))
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
	else if (one_in_(2))
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
}


/*
 * Other dungeon-altering magics
 */
bool wall_to_mud(int dir, int dam)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID;
	return (fire_bolt_beam_special(GF_KILL_WALL, dir, dam, MAX_RANGE, flg));
}

bool destroy_door(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID;
	return (fire_bolt_beam_special(GF_KILL_DOOR, dir, 0, MAX_RANGE, flg));
}

bool jam_door(int dir)
{
	u32b flg = PROJECT_GRID;
	return (fire_bolt_beam_special(GF_JAM_DOOR, dir, 0, MAX_RANGE, flg));
}

bool fetch_obj(int dir, int wgt)
{
	u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_BEAM | PROJECT_STOP;
	return (fire_bolt_beam_special(GF_FETCH_OBJ, dir, wgt, MAX_RANGE, flg));
}

bool disarm_trap(int dir)
{
	/* Use the given direction */
	int ty = p_ptr->py + ddy[dir];
	int tx = p_ptr->px + ddx[dir];

	u32b flg = PROJECT_STOP | PROJECT_GRID;
	return (project(-1, 0, ty, tx, ty, tx, 0, GF_KILL_TRAP, flg, 0, 0));
}

bool door_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_HIDE;

	return (project(-1, 1, py, px, py, px, 0, GF_MAKE_DOOR, flg, 0, 0));
}

bool trap_creation(int y, int x)
{
	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_HIDE;

	return (project(-1, 1, y, x, y, x, 0, GF_MAKE_TRAP, flg, 0, 0));
}

bool destroy_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_HIDE;

	return (project(-1, 1, py, px, py, px, 0, GF_KILL_DOOR, flg, 0, 0));
}

bool force_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_HIDE;

	return (project(-1, 1, py, px, py, px, 0, GF_FORCE_DOOR, flg, 0, 0));
}

bool force_door(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_HIDE;

	return (fire_bolt_beam_special(GF_FORCE_DOOR, dir, 0, MAX_RANGE, flg));
}



/************************************************************************
 *                                                                      *
 *                               Weather                                *
 *                                                                      *
 ************************************************************************/


/*
 * Describe the weather.
 *
 * Called with a value controlling how likely the function is to give
 * accurate results.
 *
 * If the value for MAX_WEATHER (currently 7) changes, some of the
 * numbers in this function will need to be adjusted.
 */
void predict_weather(int accuracy)
{
	cptr desc_humid, desc_wind, desc_temp;

	s16b humid = p_ptr->humid;
	s16b wind =  p_ptr->wind;
	s16b temp =  p_ptr->temp;

	int inaccuracy = 0;

	/* Allow inaccurate readings */
	while ((inaccuracy <= 3) && (accuracy < randint(100))) inaccuracy++;

	/* Guess at the weather */
	humid = rand_spread(p_ptr->humid, inaccuracy);
	wind  = rand_spread(p_ptr->wind,  inaccuracy);
	temp  = rand_spread(p_ptr->temp,  inaccuracy);

	/* Save this weather report */
	p_ptr->humid_forecast = humid;
	p_ptr->wind_forecast  = wind;
	p_ptr->temp_forecast  = temp;


	/* Describe humidity */
	if      (humid <= -7) desc_humid = "Saharan";
	else if (humid == -6) desc_humid = "dry as dust";
	else if (humid == -5) desc_humid = "bone-dry";
	else if (humid == -4) desc_humid = "parched";
	else if (humid == -3) desc_humid = "dry";
	else if (humid <= -1) desc_humid = "arid";
	else if (humid ==  0) desc_humid = "comfortable";
	else if (humid ==  1) desc_humid = "damp";
	else if (humid ==  2) desc_humid = "humid";
	else if (humid ==  3) desc_humid = "soggy";
	else if (humid ==  4) desc_humid = "wet";
	else if (humid ==  5) desc_humid = "drenched";
	else if (humid ==  6) desc_humid = "downpour";
	else                  desc_humid = "flash flood";

	/* Describe winds */
	if      (wind <= -7) desc_wind = "paralyzed";
	else if (wind == -6) desc_wind = "motionless";
	else if (wind == -5) desc_wind = "dead";
	else if (wind == -4) desc_wind = "still";
	else if (wind == -3) desc_wind = "quiet";
	else if (wind <= -1) desc_wind = "calm";
	else if (wind ==  0) desc_wind = "gentle";
	else if (wind ==  1) desc_wind = "breezy";
	else if (wind ==  2) desc_wind = "gusty";
	else if (wind ==  3) desc_wind = "windy";
	else if (wind ==  4) desc_wind = "quite windy";
	else if (wind ==  5) desc_wind = "heavy winds";
	else if (wind ==  6) desc_wind = "half-gale";
	else                 desc_wind = "hurricane";


	/* Describe temperature */
	if      (temp <= -7) desc_temp = "Arctic";
	else if (temp == -6) desc_temp = "frigid";
	else if (temp == -5) desc_temp = "icy";
	else if (temp == -4) desc_temp = "freezing";
	else if (temp == -3) desc_temp = "cold";
	else if (temp <= -1) desc_temp = "cool";
	else if (temp ==  0) desc_temp = "pleasant";
	else if (temp <=  2) desc_temp = "warm";
	else if (temp ==  3) desc_temp = "very warm";
	else if (temp ==  4) desc_temp = "hot";
	else if (temp ==  5) desc_temp = "very hot";
	else if (temp ==  6) desc_temp = "scorching";
	else                 desc_temp = "fiery";


	/* Print out the weather report */
	msg_print("The weather seems to be:");
	msg_format("humidity: %s.  wind: %s.  temperature: %s.",
		desc_humid, desc_wind, desc_temp);

	/* Update the display */
	(void)left_panel_display(DISPLAY_WEATHER, 0);
}


/*
 * Change weather.  -LM-
 *
 * We are called with inputs of 0, 0, 0 to change weather semi-randomly,
 * and with non-zero inputs for exact adjustments.
 *
 * Random weather changes consist of calculating a totally random "target"
 * weather pattern, then adjusting the current weather a little towards it.
 * The further away any component of weather drifts from an average value,
 * the more likely it is that it will change in the direction of the
 * average.  Extremes of weather are rare.
 *
 * The values for weather will never be greater than "MAX_WEATHER" (7).
 */
bool change_weather(s16b humid_change, s16b wind_change, s16b temp_change)
{
	/* Remember old weather conditions */
	s16b old_humid = p_ptr->humid;
	s16b old_wind  = p_ptr->wind;
	s16b old_temp  = p_ptr->temp;


	/* Handle random changes */
	if ((!humid_change) && (!wind_change) && (!temp_change))
	{
		/* Get a totally random weather pattern. */
		int target_humid = rand_spread(0, MAX_WEATHER);
		int target_wind  = rand_spread(0, MAX_WEATHER);
		int target_temp  = rand_spread(0, MAX_WEATHER);

		/*
		 * Adjust the three components of weather slightly, in the
		 * direction of our target weather pattern.
		 */
		while (TRUE)
		{
			if      (p_ptr->humid > target_humid) p_ptr->humid--;
			else if (p_ptr->humid < target_humid) p_ptr->humid++;

			if      (p_ptr->wind > target_wind) p_ptr->wind--;
			else if (p_ptr->wind < target_wind) p_ptr->wind++;

			if      (p_ptr->temp > target_temp) p_ptr->temp--;
			else if (p_ptr->temp < target_temp) p_ptr->temp++;

			/* Usually don't change the weather more than once */
			if (!one_in_(3)) break;
		}
	}

	/* Handle adjustments to weather */
	else
	{
		/* Adjust as instructed */
		p_ptr->humid += humid_change;
		p_ptr->wind  += wind_change;
		p_ptr->temp  += temp_change;
	}

	/* Weather always remains within certain boundaries */
	if (p_ptr->humid < -MAX_WEATHER) p_ptr->humid = -MAX_WEATHER;
	if (p_ptr->wind  < -MAX_WEATHER) p_ptr->wind  = -MAX_WEATHER;
	if (p_ptr->temp  < -MAX_WEATHER) p_ptr->temp  = -MAX_WEATHER;

	if (p_ptr->humid > MAX_WEATHER) p_ptr->humid = MAX_WEATHER;
	if (p_ptr->wind  > MAX_WEATHER) p_ptr->wind  = MAX_WEATHER;
	if (p_ptr->temp  > MAX_WEATHER) p_ptr->temp  = MAX_WEATHER;


	/* Return TRUE if the weather has changed any, FALSE if it hasn't. */
	if ((old_humid != p_ptr->humid) ||
	    (old_wind  != p_ptr->wind)  ||
	    (old_temp  != p_ptr->temp))
	{
		return (TRUE);
	}
	else
	{
		return (FALSE);
	}
}



