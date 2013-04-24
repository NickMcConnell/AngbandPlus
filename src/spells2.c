/* File: spells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* need to replace the functions that rely on project_hook */


/************************************************************************
 *                                                                      *
 *                           Projection types                           *
 *                                                                      *
 ************************************************************************/

/* Need to add the barrage code. */
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

#if 0 
fix this-> monster explosions need to be added.
#endif
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
 * of a explosion outwards from the source grid.  They are centered
 * along a line extending from the source towards the target.  -LM-
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
		flg |= (PROJECT_BEAM | PROJECT_KILL);

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
		       PROJECT_ITEM | PROJECT_KILL;
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
 * edge of the starburst.  They always "jump" to their target and affect
 * items on the floor.
 */
bool project_star(int who, int rad, int y0, int x0, int dam, int typ, u32b flg)
{
	/* Add the star bitflags */
	flg |= PROJECT_STAR | PROJECT_BOOM | PROJECT_GRID | PROJECT_JUMP |
	       PROJECT_ITEM | PROJECT_KILL;

	/* Hurt the character unless he controls the spell */
	if (who != -1) flg |= (PROJECT_PLAY);

	/* Cast a star */
	return (project(who, rad, y0, x0, y0, x0, dam, typ, flg, 0, 0));
}


/*
 * Handle target grids for projections under the control of
 * the character.  - Chris Wilde, Morgul
 */
static void adjust_target(int dir, int y0, int x0, int *y1, int *x1)
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
		*y1 = y0 + MAX_RANGE * ddy[dir];
		*x1 = x0 + MAX_RANGE * ddx[dir];
	}
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
	return (project_star(-1, rad, y1, x1, dam, typ, 0L));
}


/*
 * Fire a number of bolts, beams, or arcs that start in semi-random grids
 * near the character, and head in totally random directions.  The larger
 * the number of grids in line of fire, the more effective this spell is.
 * -LM-
 */
void fire_storm(int who, int typ0, int y0, int x0, int dam, int rad, int len,
	int perc, byte projection, bool lingering)
{
	/* Save standard delay */
	int std_delay = op_ptr->delay_factor;

	/* Array of grids (max radius is 20) */
	u16b grid[1681];

	/* Grid count */
	int grid_count = 0;

	int i, j;
	int y, x, y1, x1, last_y, last_x;
	int dir;
	int typ;
	long num_missiles;
	int choice;

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


	/* Figure out how many missiles we're going to fire */
	num_missiles = perc * grid_count / 100;

	/* Always have at least one missile */
	if (num_missiles < 1L) num_missiles = 1L;

	/* Handle each missile in turn */
	for (i = 0; i < num_missiles; i++)
	{
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
			else if (choice ==  2) typ = GF_ICE;
			else if (choice ==  3) typ = GF_ACID;
			else if (choice ==  4) typ = GF_ELEC;
			else if (choice ==  5) typ = GF_POISON;
			else if (choice ==  6) typ = GF_LIGHT;
			else if (choice ==  7) typ = GF_DARK;
			else if (choice ==  8) typ = GF_NEXUS;
			else if (choice ==  9) typ = GF_CONFUSION;
			else if (choice == 10) typ = GF_SOUND;
			else if (choice == 11) typ = GF_SHARDS;
			else                   typ = GF_ECTOPLASM;
		}

		/* Allow light, darkness, and confusion */
		else if (typ0 == -2)
		{
			choice = rand_int(3);

			if      (choice == 1) typ = GF_LIGHT;
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
	}

	/* We allowed spell graphics to accumulate */
	if (lingering)
	{
		/* Clear all lingering spell effects on screen XXX */
		for (y = p_ptr->wy; y < p_ptr->wy + SCREEN_HGT; y++)
		{
			for (x = p_ptr->wx; x < p_ptr->wx + SCREEN_WID; x++)
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
		yy = rand_spread(y, 8);
		xx = rand_spread(x, 8);

		/* Fire a beam of (strong) light towards it */
		if (project(-1, 0, y, x, yy, xx, dam, typ,
			PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID | PROJECT_PASS, 0, 0)) notice = TRUE;
	}

	/* Restore standard delay */
	op_ptr->delay_factor = old_delay;

	/* Return "anything noticed" */
	return (notice);
}
/*
 * Why the fuck isn't this working!?!
 *								-ccc
 * Keee-rist. Well it looks like it _is_ working. At freaking long
 * ranges. .. 
 *
 * I fixed this. There were several problems, most of which were 
 * solved by simplying checking the code. This whole piece of code
 * was a strange occurance, what with the flickering endpoints. Made
 * me think of trying to add artillery-ccc
 *
 * Cast a volley of bolt spells
 * Scatter "bolts" around the target or first obstacle reached.
 * Affect grids, objects, and monsters
 *
 * dd - bolt damage dice
 * ds - bolt damage sides
 * num - number of bolts in volley
 * dev - maximum distance to spread
 */
bool fire_blast(int typ, int dir, int dd, int ds, int num, int dev, bool beam)
{
      int ld;            /* distance between player & target*/
      int ty, y;            /* target y */
      int tx, x;            /* target x */
      int dist;
      int i;
      int py = p_ptr->py; /* player current location */
      int px = p_ptr->px; /* player current location */
      int flg;                  /* projectile flags */

      /* Assume okay */
      bool result = TRUE;

      if (beam) flg = PROJECT_BEAM | PROJECT_THRU | PROJECT_KILL | PROJECT_GRID;
      else flg = PROJECT_THRU | PROJECT_STOP | PROJECT_KILL | PROJECT_GRID;

      /* Initially use the given direction (and 20 squares range) */

      ty = py + 20 * ddy[dir];
      tx = px + 20 * ddx[dir];
      ld = MAX_RANGE;

      /* why this initialization ? */
      y = py;
      x = px;

      /* Hack -- Use an actual "target" */
      if ((dir == 5) && target_okay())
      {
            tx = p_ptr->target_col;
            ty = p_ptr->target_row;
            ld = distance(py, px, ty, tx);
      }
      else
      {
            /* Find the REAL target :) */
            for (dist = 0; dist <= MAX_RANGE; dist++)
            {
                  /* Calculate the new location */
                  mmove2(&y, &x, py, px, ty, tx);

                  /* Never pass through walls */
                  if (!cave_floor_bold(y, x)) break;

                  /* Never pass through monsters */
                  if (cave_m_idx[y][x]) break;

                  /* Check for arrival at "final target" */
                  if ((x == tx) && (y == ty)) break;

            }

            /* the target is at reached destination */
            ty = y;
            tx = x;
            ld = distance(py, px, ty, tx);
      }


      /* Blast */
      for (i = 0; i < num; i++)
      {
            while (1)
            {
                  /* Get targets for some bolts */
                  y = rand_spread(ty, dev * ld / MAX_RANGE);
                  x = rand_spread(tx, dev * ld / MAX_RANGE);

                  if (distance(ty, tx, y, x) <= dev) break;
            }

            /* Analyze the "dir" and the "target". */
            if (!project(-1, 0, py, px, y, x, damroll(dd, ds), typ, flg, 0, 0))
            {
                  result = FALSE;
            }
      }

      return (result);
}

bool fire_barrage(int typ, int dir, int dd, int ds, int num, int dev, int rad)
{ 
	int ly, lx, ld;
	int ty, tx, y, x, dist;
	int i;
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_KILL | PROJECT_GRID | PROJECT_BOOM;

	/* Assume okay */
	bool result = TRUE;

	/* Use the given direction */
	ly = ty = py + 20 * ddy[dir];
	lx = tx = px + 20 * ddx[dir];
	ld = 20;

	y = py;
	x = px;

	/* Hack -- Use an actual "target" */
	if (dir == 5)
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;

		lx = 20 * (tx - px) + px;
		ly = 20 * (ty - py) + py;
		ld = distance(py, px, ly, lx);
	}

	if ((dir != 5) || !target_okay())
	{
		/* Find the REAL target :) */
		for (dist = 0; dist <= MAX_RANGE; dist++)
		{
			/* Never pass through walls */
			if (dist && !cave_floor_bold(y, x)) break;

			/* Never pass through monsters */
			if (dist && cave_m_idx[y][x]) break;
			/* Check for arrival at "final target" */
			if ((x == tx) && (y == ty)) break;

			/* Calculate the new location */
			mmove2(&y, &x, py, px, ty, tx);
		}
	}

	/* Blast */
	for (i = 0; i < num; i++)
	{
		while (1)
		{
			/* Get targets for some bolts */
			y = rand_spread(ly, ld * dev / 20);
			x = rand_spread(lx, ld * dev / 20);

			if (distance(ly, lx, y, x) <= ld * dev / 20) break;
		}

		/* Analyze the "dir" and the "target". */
		if (!project(-1, rad, py, px, y, x, damroll(dd, ds), typ, flg, 0, 0))
		{
			result = FALSE;
		}
	}

	return (result);
}


/*
 * Apply a "project()" directly to all monsters in line of fire.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * To avoid misbehavior when monster deaths have side-effects,
 * this is done in two passes. -- JDL
 */
bool project_los(int typ, int dam)
{
	int i, x, y;
	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool obvious = FALSE;

	/* There is currently another method of doing this in */
	/* Use that does not depend on the monster flags being */
	/* contained in the p_ptr-> */
	/* u32b saved_proj_mon_flags = p_ptr->proj_mon_flags; */

	/* Mark monsters in line of sight */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of fire */
		if (!player_can_fire_bold(y, x)) continue;

		/* Mark the monster */
		m_ptr->mflag |= (MFLAG_TEMP);
	}

	/* Affect all marked monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip unmarked monsters */
		if (!(m_ptr->mflag & (MFLAG_TEMP))) continue;

		/* Remove mark */
		m_ptr->mflag &= ~(MFLAG_TEMP);

		/* Restore projection limitations */
		/* p_ptr->proj_mon_flags = saved_proj_mon_flags; */

		/* Jump directly to the target monster */
		if (project(-1, 0, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx,
		            dam, typ, flg, 0, 0))
		{
			obvious = TRUE;
		}
	}

	/* Result */
	return (obvious);
}


/*
 * This routine clears the entire "temp" set.
 */
void clear_temp_array(void)
{
	int i;

	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);
	}

	/* None left */
	temp_n = 0;
}


/*
 * Aux function -- see below
 */
void cave_temp_mark(int y, int x, bool room)
{
	/* Avoid infinite recursion */
	if (cave_info[y][x] & (CAVE_TEMP)) return;

	/* Option -- do not leave the current room */
	if ((room) && (!(cave_info[y][x] & (CAVE_ROOM)))) return;

	/* Verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid */
	cave_info[y][x] |= (CAVE_TEMP);

	/* Add it to the marked set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}

/*
 * Mark the nearby area with CAVE_TEMP flags.  Allow limited range.
 */
void spread_cave_temp(int y1, int x1, int range, bool room)
{
	int i, y, x;

	/* Add the initial grid */
	cave_temp_mark(y1, x1, room);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get marked, but stop further spread */
		if (!cave_floor_bold(y, x)) continue;

		/* Note limited range (note:  we spread out one grid further) */
		if ((range) && (distance(y1, x1, y, x) >= range)) continue;

		/* Spread adjacent */
		cave_temp_mark(y + 1, x, room);
		cave_temp_mark(y - 1, x, room);
		cave_temp_mark(y, x + 1, room);
		cave_temp_mark(y, x - 1, room);

		/* Spread diagonal */
		cave_temp_mark(y + 1, x + 1, room);
		cave_temp_mark(y - 1, x - 1, room);
		cave_temp_mark(y - 1, x + 1, room);
		cave_temp_mark(y + 1, x - 1, room);
	}
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
			msg_print("You feel a little rested.");
		}

		/* Heal some */
		else if (num < 6 + (p_ptr->mhp / 5))
		{
			msg_print("You feel rested.");
		}

		/* Heal a fair amount */
		else if (num < 12 + (p_ptr->mhp / 2))
		{
			msg_print("You feel much better.");
		}

		/* Heal a lot */
		else
		{
			msg_print("You feel very rested.");
		}

		/* If character color changes with damage taken, redraw */
		lite_spot(p_ptr->py, p_ptr->px);

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}

/*
 * Increase player's hit points, notice effects
 */
bool wp_player(int num)
{
	/* Healing needed */
	if (p_ptr->cwp < p_ptr->mwp)
	{
		/* Gain hitpoints */
		p_ptr->cwp += num;

		/* Enforce maximum */
		if (p_ptr->cwp >= p_ptr->mwp)
		{
			p_ptr->cwp = p_ptr->mwp;
			p_ptr->cwp_frac = 0;
		}

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Heal a little */
		if (num < 3 + (p_ptr->mwp / 15))
		{
			msg_print("You feel a little better.");
		}

		/* Heal some */
		else if (num < 6 + (p_ptr->mwp / 5))
		{
			msg_print("You feel better.");
		}

		/* Heal a fair amount */
		else if (num < 12 + (p_ptr->mwp / 2))
		{
			msg_print("You feel much better.");
		}

		/* Heal a lot */
		else
		{
			msg_print("You feel very good.");
		}

		/* If character color changes with damage taken, redraw */
		lite_spot(p_ptr->py, p_ptr->px);

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
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
 * Heal a player a percentage of total hitpoints.  -EZ-
 */
bool heal_player_sp(int perc, int min)
{
	int heal;

	/* No healing needed */
	if (p_ptr->csp >= p_ptr->msp) return (FALSE);

	/* Figure healing level */
	heal = p_ptr->msp * perc / 100;

	/* Enforce minimums */
	if (heal < min) heal = min;

	/* Actual healing */
	return (sp_player(heal, NULL));
}

/*
 * Hack -- return how much healing the "heal_player()" function will
 * do now.
 */
int get_heal_sp_amount(int perc, int min)
{
	/* Figure healing level */
	int heal = p_ptr->msp * perc / 100;

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
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);
}




/*
 * Array of stat "descriptions"
 */
 /* must change the order of these */
static cptr desc_stat_pos[] =
{
	"strong",
	"agile",
	"hearty",
	"learned",
	"willful",
	"persuasive"
};


/*
 * Array of stat "descriptions"
 * external due to level.c XCCCX
 */
cptr desc_stat_neg[] =
{
	"weak",
	"clumsy",
	"sickly",
	"ignorant",
	"weak-willed",
	"repulsive"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
	bool sust = FALSE;

	/* Get the "sustain" */
	switch (stat)
	{
		case A_MUS: if (p_ptr->sustain_mus) sust = TRUE; break;
		case A_AGI: if (p_ptr->sustain_agi) sust = TRUE; break;
		case A_VIG: if (p_ptr->sustain_vig) sust = TRUE; break;
		case A_SCH: if (p_ptr->sustain_sch) sust = TRUE; break;
		case A_EGO: if (p_ptr->sustain_ego) sust = TRUE; break;
		case A_CHR: if (p_ptr->sustain_chr) sust = TRUE; break;
	}

	/* Sustain */
	if (sust)
	{
		/* Message */
		msg_format("You feel very %s for a moment, but the feeling passes.",
		           desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, FALSE))
	{
		/* Message */
		msg_format("You feel very %s.", desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
	/* Attempt to increase */
	if (res_stat(stat))
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
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
	bool res;

	/* Restore strength */
	res = res_stat(stat);

	/* Attempt to increase */
	if (inc_stat(stat))
	{
		/* Message */
		msg_format("You feel very %s!", desc_stat_pos[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Restoration worked */
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
 * Identify everything being carried.
 * Done by a tonic of "self knowledge".
 */
void identify_pack(void)
{
	int i;

	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
}






/*
 * Used by the "enchant" function (chance of failure)
 */
static const int enchant_table[16] =
{
	0, 10,  50, 100, 200,
	300, 400, 500, 700, 950,
	990, 992, 995, 997, 999,
	1000
};


/*
 * Hack -- Removes curse from an object.
 */
static void uncurse_object(object_type *o_ptr)
{
	/* Uncurse it */
	o_ptr->ident &= ~(IDENT_CURSED);

	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

	/* Take note if allowed */
	if (o_ptr->discount == 0) o_ptr->discount = INSCRIP_UNCURSED;

	/* The object has been "sensed" */
	o_ptr->ident |= (IDENT_SENSE);
}


/*
 * Removes curses from items in inventory.
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
	int i, cnt = 0;

	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Heavily Cursed Items need a special spell */
		if (!all && (f3 & (TR3_HEAVY_CURSE))) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE)) continue;

		/* Uncurse the object */
		uncurse_object(o_ptr);

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Count the uncursings */
		cnt++;
	}

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(void)
{
	return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
	return (remove_curse_aux(TRUE));
}



/*
 * Restores any drained experience
 */
bool restore_level(void)
{
	/* Restore experience */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Message */
		msg_print("You feel your life energies returning.");

		/* Restore the experience */
		p_ptr->exp = p_ptr->max_exp;

		/* Check the experience */
		check_experience();

		/* Did something */
		return (TRUE);
	}

	/* No effect */
	return (FALSE);
}


/*
 * Hack -- acquire self knowledge
 *
 * List various information about the player and/or his current equipment.
 *
 * See also "identify_fully()".
 *
 * Use the "roff()" routines, perhaps.  XXX XXX XXX
 *
 * Use the "show_file()" method, perhaps.  XXX XXX XXX
 *
 * This function cannot display more than 20 lines.  XXX XXX XXX
 *
 * I need to come back to this function and go down the flag list
 * in defines.h
 *
 */
void self_knowledge(void)
{
	int i = 0, j, k;

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	object_type *o_ptr;

	cptr info[128];


	/* Get item flags from equipment */
	for (k = INVEN_WIELD; k < INVEN_SUBTOTAL; k++)
	{
		u32b t1, t2, t3;

		o_ptr = &inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, &t1, &t2, &t3);

		/* Extract flags */
		f1 |= t1;
		f2 |= t2;
		f3 |= t3;
	}


	if (p_ptr->blind)
	{
		info[i++] = "You cannot see.";
	}
	if (p_ptr->confused)
	{
		info[i++] = "You are confused.";
	}
	if (p_ptr->afraid)
	{
		info[i++] = "You are terrified.";
	}
	if (p_ptr->cut)
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || 
			(p_ptr->prace == RACE_STEAM_MECHA))
		{
			info[i++] = "You are leaking.";		
		}
		else info[i++] = "You are bleeding.";
	}
	if (p_ptr->stun)
	{
		info[i++] = "You are stunned.";
	}
	if (p_ptr->poisoned)
	{
		info[i++] = "You are poisoned.";
	}
	if (p_ptr->image)
	{
		info[i++] = "You are hallucinating.";
	}

	if (p_ptr->aggravate)
	{
		info[i++] = "You aggravate monsters.";
	}
	if (p_ptr->teleport)
	{
		info[i++] = "Your position is very uncertain.";
	}

	if (p_ptr->blessed)
	{
		info[i++] = "You feel righteous.";
	}
	if (p_ptr->hero)
	{
		info[i++] = "You feel heroic.";
	}
	if (p_ptr->shero)
	{
		info[i++] = "You are in a battle rage.";
	}
	if (p_ptr->protevil)
	{
		info[i++] = "You are protected from evil.";
	}
	if (p_ptr->shield)
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->invuln)
	{
		info[i++] = "You are temporarily invulnerable.";
	}
	if (p_ptr->confusing)
	{
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->searching)
	{
		info[i++] = "You are looking around very carefully.";
	}
	if (p_ptr->word_recall)
	{
		info[i++] = "You will soon be recalled.";
	}
	if (p_ptr->see_infra)
	{
		info[i++] = "Your eyes are sensitive to infrared light.";
	}

	if (p_ptr->slow_digest)
	{
		info[i++] = "Your appetite is small.";
	}
	if (p_ptr->ffall)
	{
		info[i++] = "You land gently.";
	}
	if (p_ptr->lite)
	{
		info[i++] = "You are glowing with light.";
	}
	if (p_ptr->telepathy)
	{
		info[i++] = "You have ESP.";
	}
	if (p_ptr->see_inv || p_ptr->tim_invis)
	{
		info[i++] = "You can see invisible creatures.";
	}
	if (p_ptr->free_act)
	{
		info[i++] = "You have free action.";
	}
	if (p_ptr->hold_life)
	{
		info[i++] = "You have a firm hold on your life force.";
	}
	if (p_ptr->regenerate_25)
	{
		info[i++] = "You regenerate quickly.";
	}
	if (p_ptr->regenerate_50)
	{
		info[i++] = "You regenerate very quickly.";
	}
	if (p_ptr->regenerate_75)
	{
		info[i++] = "Your wounds close before your eyes.";
	}
	if (p_ptr->resist_fear)
	{
		info[i++] = "You are completely fearless.";
	}
	if (p_ptr->resist_blind)
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}
	if (p_ptr->resist_confu)
	{
		info[i++] = "You are resistant to confusion.";
	}
	if (p_ptr->sustain_mus)
	{
		info[i++] = "Your muscle is sustained.";
	}
	if (p_ptr->sustain_agi)
	{
		info[i++] = "Your agility is sustained.";
	}
	if (p_ptr->sustain_vig)
	{
		info[i++] = "Your vigor is sustained.";
	}
	if (p_ptr->sustain_sch)
	{
		info[i++] = "Your schooling is sustained.";
	}
	if (p_ptr->sustain_ego)
	{
		info[i++] = "Your ego is sustained.";
	}
	if (p_ptr->sustain_chr)
	{
		info[i++] = "Your charm is sustained.";
	}

	/* Get the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_ANIMAL))
		{
			info[i++] = "Your weapon strikes at animals with extra force.";
		}
		if (f1 & (TR1_SLAY_EVIL))
		{
			info[i++] = "Your weapon strikes at evil with extra force.";
		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your weapon strikes at undead with holy wrath.";
		}
		if (f1 & (TR1_SLAY_DEMON))
		{
			info[i++] = "Your weapon strikes at demons with holy wrath.";
		}
		if (f1 & (TR1_SLAY_AUTOMATA))
		{
			info[i++] = "Your weapon is especially deadly against automata.";
		}
		if (f1 & (TR1_SLAY_DINOSAUR))
		{
			info[i++] = "Your weapon is especially deadly against dinosaurs.";
		}
		if (f1 & (TR1_SLAY_CONSTRUCT))
		{
			info[i++] = "Your weapon is especially deadly against constructs.";
		}
		if (f1 & (TR1_SLAY_ELEMENTAL))
		{
			info[i++] = "Your weapon is especially deadly against elementals.";
		}
		if (f1 & (TR1_SLAY_ALIEN))
		{
			info[i++] = "Your weapon is especially deadly against aliens.";
		}
		if (f1 & (TR1_SLAY_BEASTMAN))
		{
			info[i++] = "Your weapon is especially deadly against beastman.";
		}
		if (f1 & (TR1_SLAY_CARDS))
		{
			info[i++] = "Your weapon is especially deadly against cards.";
		}
	}


	/* Save screen */
	screen_save();


	/* Clear the screen */
	Term_clear();

	/* Label the information */
	prt("     Your Attributes:", 1, 0);

	/* Dump the info */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prt(info[j], k++, 0);

		/* Page wrap */
		if ((k == 22) && (j+1 < i))
		{
			prt("-- more --", k, 0);
			inkey();

			/* Clear the screen */
			Term_clear();

			/* Label the information */
			prt("     Your Attributes:", 1, 0);

			/* Reset */
			k = 2;
		}
	}

	/* Pause */
	prt("[Press any key to continue]", k, 0);
	(void)inkey();


	/* Load screen */
	screen_load();
}






/*
 * Forget everything
 */
bool lose_all_info(void)
{
	int i;

	/* Forget info about objects */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

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

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}



/*
 * Set word of recall as appropriate
 */
void set_recall(void)
{
	/* Ironman */
	if ((adult_ironman && !p_ptr->total_winner) || p_ptr->wonderland)
	{
		msg_print("Nothing happens.");
		return;
	}

	/* Activate recall */
	if (!p_ptr->word_recall)
	{
		p_ptr->word_recall = rand_int(20) + 15;
		msg_print("The air about you becomes charged...");
	}

	/* Deactivate recall */
	else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	}
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
 * seven on a side.  The reason for matching detection area to 11x11 blocks
 * is because the dungeon is laid out and panels usually scroll using the
 * same increments.
 */
static void get_detection_area(int *top, int *left, int *bottom, int *right,
	bool extrarange)
{
	/* Figure out which 11x11 grid the character is in */
	*top  = (p_ptr->py / BLOCK_HGT) * BLOCK_HGT;
	*left = (p_ptr->px / BLOCK_WID) * BLOCK_WID;

	/* Extend out either two or three 11x11 grids in all directions */
	*top  -= BLOCK_HGT * (extrarange ? 3 : 2);
	*left -= BLOCK_WID * (extrarange ? 3 : 2);
	*bottom = *top + BLOCK_HGT * (extrarange ? 7 : 5);
	*right = *left + BLOCK_WID * (extrarange ? 7 : 5);

	/* Test for legality (vertical) */
	if (*top < 0) *top = 0;
	if (*bottom > DUNGEON_HGT) *bottom = DUNGEON_HGT;

	/* Test for legality (horizontal) */
	if (*left < 0) *left = 0;
	if (*right > DUNGEON_WID) *right = DUNGEON_WID;
}

/*
 * Detect all traps on current panel
 */
bool detect_traps(bool extrarange)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

	/* Scan the detection area */
	for (y = top; y < bottom; y++)
	{
		for (x = left; x < right; x++)
		{
			/* Detect invisible traps */
			if (cave_feat[y][x] == FEAT_INVIS)
			{
				/* Pick a trap */
				pick_trap(y, x);
			}

			/* Detect traps */
			if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
			    (cave_feat[y][x] <= FEAT_TRAP_TAIL))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of traps!");
	}

	/* Result */
	return (detect);
}



/*
 * Detect all doors nearby
 */
bool detect_doors(bool extrarange)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

	/* Scan the detection area */
	for (y = top; y < bottom; y++)
	{
		for (x = left; x < right; x++)
		{
			/* Detect secret doors */
			if (cave_feat[y][x] == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);
			}

			/* Detect doors */
			if (((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
			     (cave_feat[y][x] <= FEAT_DOOR_TAIL)) ||
			     (cave_feat[y][x] == FEAT_OPEN) ||
			     (cave_feat[y][x] == FEAT_BROKEN))
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
bool detect_stairs(bool extrarange)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

	/* Scan the detection area */
	for (y = top; y < bottom; y++)
	{
		for (x = left; x < right; x++)
		{
			/* Detect stairs */
			if ((cave_feat[y][x] == FEAT_LESS) ||
			    (cave_feat[y][x] == FEAT_MORE))
			{
				/* Hack -- Memorize */
				cave_info[y][x] |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
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
bool detect_treasure(bool extrarange)
{
	int y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

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
bool detect_objects_gold(bool extrarange)
{
	int i, y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

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
bool detect_objects_normal(bool extrarange)
{
	int i, y, x;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

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
		if (o_ptr->tval != TV_GOLD)
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
bool detect_objects_magic(bool extrarange)
{
	int i, y, x, tv;

	bool detect = FALSE;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

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
		    (tv == TV_TOOL) || (tv == TV_RAY) || (tv == TV_APPARATUS) ||
		    (tv == TV_MECHANISM) || (tv == TV_TONIC) ||
		    (tv == TV_MAGIC_BOOK) || 
		    ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
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
 * Detect monsters nearby
 *
 * If "match" is TRUE, will detect monsters with any of the given flags.
 * Otherwise, will detect monsters having none of the given flags.
 *
 * If "flags" is empty, we detect all monsters.
 *
 * Update the monster lore for all detected monsters by adding the given
 * lore flags.
 */
bool detect_monsters(bool extrarange, bool match, u32b flags, int flag_set,
	const char *str)
{
	int i, y, x;

	bool flag = FALSE;

	u32b tmp_flags = 0L;

	int top, left, bottom, right;

	/* Determine the limits of the detection area */
	get_detection_area(&top, &left, &bottom, &right, extrarange);

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

			/* Monster will stay fully visible until next turn */
			m_ptr->mflag |= (MFLAG_SHOW);
			m_ptr->mflag |= (MFLAG_FULL);
			
			/* XXX XXX - Mimics are revealed */
			m_ptr->mflag &= ~(MFLAG_MIME);

			/* Learn about attributes that allow detection */
			if (match)
			{
				switch (flag_set)
				{
					case 1:  l_ptr->r_flags1 |= (flags & (r_ptr->flags1));  break;
					case 2:  l_ptr->r_flags2 |= (flags & (r_ptr->flags2));  break;
					case 3:  l_ptr->r_flags3 |= (flags & (r_ptr->flags3));  break;
					case 4:  l_ptr->r_flags4 |= (flags & (r_ptr->flags4));  break;
					case 5:  l_ptr->r_flags5 |= (flags & (r_ptr->flags5));  break;
					case 6:  l_ptr->r_flags6 |= (flags & (r_ptr->flags6));  break;
					case 7:  l_ptr->r_flags7 |= (flags & (r_ptr->flags7));  break;
				}
			}

			/* Update the monster */
			(void)update_mon(i, FALSE, FALSE);
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_format("You sense the presence of %s!", str);
	}

	/* Result */
	return (flag);
}

/*
 * Detect all non-invisible monsters nearby.
 */
bool detect_monsters_normal(bool extrarange)
{
	return (detect_monsters(extrarange, FALSE, RF2_INVISIBLE,
	        2, "monsters"));
}


/*
 * Detect all invisible monsters nearby.
 */
bool detect_monsters_invis(bool extrarange)
{
	return (detect_monsters(extrarange, TRUE, RF2_INVISIBLE, 2,
		"invisible creatures"));
}

/*
 * Detect all charmable monsters nearby.
 */
bool detect_monsters_charm(bool extrarange)
{
	return (detect_monsters(extrarange, FALSE, 
		RF3_NO_CONF | RF3_AUTOMATA | RF3_CONSTRUCT | RF3_ELEMENTAL | 
		RF3_DEMON | RF3_UNDEAD | RF3_PLANT, 3,
		"recruitable creatures"));
}

/*
 * Detect all evil monsters nearby.
 */
bool detect_evil(bool extrarange)
{
	return (detect_monsters(extrarange, TRUE, RF3_EVIL, 3, "evil"));
}

/*
 * Detect all undead monsters nearby.
 */
bool detect_undead(bool extrarange)
{
	return (detect_monsters(extrarange, TRUE, RF3_UNDEAD, 3, "undead"));
}

/*
 * Detect all non-undead, non-demonic monsters nearby.
 */
bool detect_life(bool extrarange)
{
	return (detect_monsters(extrarange, FALSE, RF3_UNDEAD | RF3_DEMON | RF3_AUTOMATA | 
		RF3_CONSTRUCT | RF3_ELEMENTAL, 3,
		"life"));
}

/*
 * Detect all animals nearby.
 */
bool detect_animals(bool extrarange)
{
	return (detect_monsters(extrarange, TRUE, RF3_ANIMAL, 3, "animals"));
}

/*
 * Detect all monsters nearby.
 */
bool detect_all_monsters(bool extrarange)
{
	return (detect_monsters(extrarange, TRUE, 0L, 3, "monsters"));
}



/*
 * Detect everything
 */
bool detect_all(void)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps(FALSE)) detect = TRUE;
	if (detect_doors(FALSE)) detect = TRUE;
	if (detect_stairs(FALSE)) detect = TRUE;
	if (detect_treasure(FALSE)) detect = TRUE;
	if (detect_objects_gold(FALSE)) detect = TRUE;
	if (detect_objects_normal(FALSE)) detect = TRUE;
	if (detect_monsters_invis(FALSE)) detect = TRUE;
	if (detect_monsters_normal(FALSE)) detect = TRUE;

	/* Result */
	return (detect);
}



/*
 * Create stairs at the player location
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
	if (!p_ptr->depth)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
			 (quest_check(p_ptr->depth) == QUEST_FIXED_U) ||
			 (p_ptr->depth >= MAX_DEPTH-1))
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
	else if ((rand_int(100) < 50) && (!p_ptr->wonderland))
	{
		if ((quest_check(p_ptr->depth + 1) == QUEST_FIXED) ||
		(quest_check(p_ptr->depth + 1) == QUEST_FIXED_U) ||
		(p_ptr->depth <= 1))
		cave_set_feat(py, px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
}


/*
 * Hook to specify "book"
 */
/*
static bool item_tester_hook_book(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_MAGIC_BOOK:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}
*/

/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DAGGER:
		case TV_AXES:
		case TV_BLUNT:
		case TV_DIGGING:
		case TV_GUN:
		case TV_SHOT:
		case TV_BULLET:
		case TV_AMMO:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_LEG:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


static bool item_tester_unknown(const object_type *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
}


static bool item_tester_unknown_star(const object_type *o_ptr)
{
	if (o_ptr->ident & IDENT_MENTAL)
		return FALSE;
	else
		return TRUE;
}


/*
 * Enchant an item
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
	int i, chance, prob;

	bool res = FALSE;

	bool a = artifact_p(o_ptr);

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_SHOT) ||
	    (o_ptr->tval == TV_BULLET) ||
	    (o_ptr->tval == TV_AMMO))
	{
		prob = prob / 20;
	}

	/* Try "n" times */
	for (i=0; i<n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if ((prob > 100) && (rand_int(prob) >= 100)) continue;

		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_h];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_h++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_h >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (o_ptr->to_d < 0) chance = 0;
			else if (o_ptr->to_d > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_d++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_d >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}

		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			if (o_ptr->to_a < 0) chance = 0;
			else if (o_ptr->to_a > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];

			/* Attempt to enchant */
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;

				/* Enchant */
				o_ptr->to_a++;

				/* Break curse */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_a >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");

					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}
	}

	/* Failure */
	if (!res) return (FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Success */
	return (TRUE);
}



/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
	int item;
	bool okay = FALSE;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item */
	q = "Enchant which item? ";
	s = "You have nothing to enchant.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Describe */
	msg_format("%s %s glow%s brightly!",
	           ((item >= 0) ? "Your" : "The"), o_name,
	           ((o_ptr->number > 1) ? "" : "s"));

	/* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The enchantment failed.");
	}

	/* Something happened */
	return (TRUE);
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;


	/* Only un-id'ed items */
	item_tester_hook = item_tester_unknown;

	/* Get an item */
	q = "Identify which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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


	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
		           describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
		           o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
		           o_name);
	}

	/* Something happened */
	return (TRUE);
}



/*
 * Fully "identify" an object in the inventory
 *
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;


	/* Only un-*id*'ed items */
	item_tester_hook = item_tester_unknown_star;

	/* Get an item */
	q = "*Identify* which item? ";
	s = "You have nothing to identify.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

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


	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);
	object_mental(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Handle stuff */
	handle_stuff();

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
		           describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).",
		           o_name, index_to_label(item));
	}
	else
	{
		msg_format("On the ground: %s.",
		           o_name);
	}

	/* Describe it fully */
	do_cmd_observe(o_ptr, FALSE);

	/* Success */
	return (TRUE);
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
static bool item_tester_hook_recharge(const object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_TOOL) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_RAY) return (TRUE);

	/* Hack -- Recharge rods */
	if (o_ptr->tval == TV_APPARATUS) return (TRUE);

	/* Nope */
	return (FALSE);
}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 *
 * Mage -- Recharge I --> recharge(5)
 * Mage -- Recharge II --> recharge(40)
 * Mage -- Recharge III --> recharge(100)
 *
 * Priest -- Recharge --> recharge(15)
 *
 * Scroll of recharging --> recharge(60)
 *
 * recharge(20) = 1/6 failure for empty 10th level wand
 * recharge(60) = 1/10 failure for empty 10th level wand
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staffs/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 * XXX XXX XXX Perhaps we should auto-unstack recharging stacks.
 */
bool recharge(int num)
{
	int i, t, item, lev;

	object_type *o_ptr;

	cptr q, s;
	bool plural;
	char o_name[120];


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
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


	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;

	/* Recharge a rod */
	if (o_ptr->tval == TV_APPARATUS)
	{
		/* Extract a recharge power */
		i = (50 - lev + num) / 5;

		/* Back-fire */
		if ((i <= 1) || (rand_int(i) == 0))
		{
			/* Hack -- backfire */
			msg_print("The recharge backfires, draining the apparatus further!");

			/* Hack -- decharge the rod */
			if (o_ptr->pval < 10000) o_ptr->pval = (o_ptr->pval + 100) * 2;
		}

		/* Recharge */
		else
		{
			/* Rechange amount */
			t = (num * damroll(2, 4));

			/* Recharge by that amount */
			if (o_ptr->pval > t)
			{
				o_ptr->pval -= t;
			}

			/* Fully recharged */
			else
			{
				o_ptr->pval = 0;
			}
		}
	}

	/* Recharge wand/staff */
	else
	{
		/* Recharge power */
		i = (num + 50 - lev - (10 * o_ptr->pval/o_ptr->number)) / 15;

		/* Back-fire XXX XXX XXX */
		if ((i <= 1))
		{
			/* Adding a power cell to an item should be much */
			/* less risky */
			if (randint(100) < 1)
			{
				/* Dangerous Hack -- Destroy the item */
				msg_print("There is a bright flash of light.");
	
				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -999);
					inven_item_describe(item);
					inven_item_optimize(item);
				}
	
				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -999);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
				}
			}
			else
				/* Useless */
				msg_print("This power cell has no effect.");
		}

		/* Recharge */
		else
		{
			/* Extract a "power" */
			t = (num / (lev + 2)) + 1;

			/* Recharge based on the power */
			if (t > 0) o_ptr->pval += 2 + randint(t);

			/* Hack -- we no longer "know" the item */
			// o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);

			/* Note if plural */
			plural = (o_ptr->number > 1);

			/* Get the object name */
			object_desc(o_name, o_ptr, FALSE, 0);

			/* Identify it */
			// object_known(o_ptr);

			/* Message */
			if (o_ptr->ident & IDENT_KNOWN)
			{
			msg_format("You sense %s %s %s %d charges%s.",
				(item >= 0 ? "your" : "the"),
				o_name, (plural ? "have" : "has"), o_ptr->pval,
				((o_ptr->tval == TV_TOOL && plural) ? " each" : ""));
			}
			else
			{
				msg_format("Your %s has been recharged.", o_name);
			}
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Something was done */
	return (TRUE);
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who)
{
	int i;

	bool sleep = FALSE;
	bool speed = FALSE;

	/* Aggravate everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up nearby sleeping monsters */
		if (m_ptr->cdis < MAX_SIGHT * 2)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
				sleep = TRUE;
			}
		}

		/* Speed up monsters in line of sight */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			/* Speed up (instantly) to racial base + 10 */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				/* Speed up */
				m_ptr->mspeed = r_ptr->speed + 10;
				speed = TRUE;
			}
		}
	}

	/* Messages */
	if (speed) msg_print("You feel a sudden stirring nearby!");
	else if (sleep) msg_print("You hear a sudden stirring in the distance!");
}



/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool genocide(void)
{
	int i;

	char typ;

	bool result = FALSE;


	/* Mega-Hack -- Get a monster symbol */
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(randint(4), "the strain of casting Genocide", TRUE);

		/* Take note */
		result = TRUE;
	}

	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(void)
{
	int i;

	bool result = FALSE;


	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Delete the monster */
		delete_monster_idx(i);

		/* Take some damage */
		take_hit(randint(3), "the strain of casting Mass Genocide", TRUE);

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


	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Start the message */
			if (!probe) msg_print("Probing...");

			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);

			/* Describe the monster */
			msg_format("%^s has %d hit points.", m_name, m_ptr->hp);

			/* Learn all of the non-spell, non-treasure flags */
			lore_do_probe(i);

			/* Probe worked */
			probe = TRUE;
		}
	}

	/* Done */
	if (probe)
	{
		msg_print("That's all.");
	}

	/* Result */
	return (probe);
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r, bool full)
{
	int y, x, k, t;

	bool flag = FALSE;


	/* Unused parameter */
	(void)full;

	/* Big area of affect */
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

			/* Lose room and vault */
			cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			cave_info[y][x] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Hack -- Notice player affect */
			if (cave_m_idx[y][x] < 0)
			{
				/* Hurt the player later */
				flag = TRUE;

				/* Do not hurt this grid */
				continue;
			}

			/* Hack -- Skip the epicenter */
			if ((y == y1) && (x == x1)) continue;

			/* Delete the monster (if any) */
			delete_monster(y, x);

			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x))
			{
				int feat = FEAT_FLOOR;

				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					feat = FEAT_MAGMA;
				}

				/* Change the feature */
				cave_set_feat(y, x, feat);
			}
		}
	}


	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
		msg_print("There is a searing blast of light!");

		/* Blind the player */
		
		if (!p_ptr->resist_blind && !(resist_effect(RS_LIT)))
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + 10 + randint(10));
		}
	}


	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Induce an "earthquake" of the given radius at the given location.
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
			cave_info[yy][xx] &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			cave_info[yy][xx] &= ~(CAVE_GLOW | CAVE_MARK);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (rand_int(100) < 85) continue;

			/* Damage this grid */
			map[16+yy-cy][16+xx-cx] = TRUE;

			/* Hack -- Take note of player damage */
			if ((yy == py) && (xx == px)) hurt = TRUE;
		}
	}

	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			/* Get the location */
			y = py + ddy_ddd[i];
			x = px + ddx_ddd[i];

			/* Skip non-empty grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+y-cy][16+x-cx]) continue;

			/* Count "safe" grids, apply the randomizer */
			if ((++sn > 1) && (rand_int(sn) != 0)) continue;

			/* Save the safe location */
			sy = y; sx = x;
		}

		/* Random message */
		switch (randint(3))
		{
			case 1:
			{
				msg_print("The cave ceiling collapses!");
				break;
			}
			case 2:
			{
				msg_print("The cave floor twists in an unnatural way!");
				break;
			}
			default:
			{
				msg_print("The cave quakes!");
				msg_print("You are pummeled with debris!");
				break;
			}
		}

		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
			msg_print("You are severely crushed!");
			damage = 300;
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
					msg_print("You are bashed by rubble!");
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
			}

			/* Move player */
			monster_swap(py, px, sy, sx);
		}

		/* Take some damage */
		if (damage) take_hit(damage, "an earthquake", TRUE);
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
					char m_name[80];

					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Get the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							/* Skip non-empty grids */
							if (!cave_empty_bold(y, x)) continue;

							/* Hack -- no safety on glyph of warding */
							if (cave_feat[y][x] == FEAT_GLYPH) continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							/* Count "safe" grids, apply the randomizer */
							if ((++sn > 1) && (rand_int(sn) != 0)) continue;

							/* Save the safe grid */
							sy = y;
							sx = x;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

					/* Monster is certainly awake */
					m_ptr->csleep = 0;

					/* Apply damage directly */
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
				int feat = FEAT_FLOOR;

				bool floor = cave_floor_bold(yy, xx);

				/* Delete objects */
				delete_object(yy, xx);

				/* Wall (or floor) type */
				t = (floor ? rand_int(100) : 200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					feat = FEAT_MAGMA;
				}

				/* Change the feature */
				cave_set_feat(yy, xx, feat);
			}
		}
	}


	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
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
			int chance = 25;

			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

			/* Sometimes monsters wake up */
			if (m_ptr->csleep && (rand_int(100) < chance))
			{
				/* Wake up! */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);
				}
			}
		}
	}

	/* None left */
	temp_n = 0;
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlite_room()"
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

		/* Hack -- Forget "boring" grids */
		if (cave_feat[y][x] <= FEAT_INVIS)
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
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
	/* Avoid infinite recursion */
	if (cave_info[y][x] & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	if (!(cave_info[y][x] & (CAVE_ROOM))) return;

	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid as "seen" */
	cave_info[y][x] |= (CAVE_TEMP);

	/* Add it to the "seen" set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}




/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get lit, but stop light */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}

	/* Now, lite them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get dark, but stop darkness */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}

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

	int flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, py, px, dam, GF_GLOW, flg, 0, 0);

	/* Lite up the room */
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

	int flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, py, px, dam, GF_DIM, flg, 0, 0);

	/* Lite up the room */
	unlite_room(py, px);

	/* Assume seen */
	return (TRUE);
}


/*
 * Some of the old functions
 */

bool lite_line(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID;
	return (fire_bolt_beam_special(GF_GLOW, dir, damroll(4, 5),
	                               MAX_RANGE, flg));
}

#if 0
must fix drain flag
#endif
bool drain_life(int dir, int dam)
{
	u32b flg = PROJECT_HIDE;
	return (fire_bolt_beam_special(GF_DRAIN, dir, dam, MAX_RANGE, flg));
}

bool wall_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_KILL_WALL, dir, 20 + randint(30), MAX_RANGE, flg));
}

bool destroy_door(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_DOOR, dir, 0, MAX_RANGE, flg));
}

bool disarm_trap(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (fire_bolt_beam_special(GF_KILL_TRAP, dir, 0, MAX_RANGE, flg));
}

bool heal_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_HEAL, dir, damroll(4, 6), MAX_RANGE, flg));
}

bool speed_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_SPEED, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool slow_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_SLOW, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool sleep_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_SLEEP, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool confuse_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_CONFUSION, dir, plev, MAX_RANGE, flg));
}

bool poly_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_POLY, dir, p_ptr->lev, MAX_RANGE, flg));
}

bool clone_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_CLONE, dir, 0, MAX_RANGE, flg));
}

bool fear_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_TURN_ALL, dir, plev, MAX_RANGE, flg));
}

bool teleport_monster(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_AWAY_ALL, dir, MAX_SIGHT * 5, MAX_RANGE, flg));
}

bool charm_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_CHARM, dir, plev, MAX_RANGE, flg));
}


bool charm_animal(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (fire_bolt_beam_special(GF_CONTROL_ANIMAL, dir, plev, MAX_RANGE, flg));
}




/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, py, px, 0, GF_MAKE_DOOR, flg, 0, 0));
}

bool trap_creation(int y, int x)
{
	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

	return (project(-1, 1, y, x, y, x, 0, GF_MAKE_TRAP, flg, 0, 0));
}

bool destroy_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, py, px, 0, GF_KILL_DOOR, flg, 0, 0));
}

bool sleep_monsters_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int flg = PROJECT_BOOM | PROJECT_KILL;
	return (project(-1, 1, py, px, py, px, p_ptr->lev, GF_SLEEP, flg, 0, 20));
}

/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_los(GF_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
	return (project_los(GF_SLOW, p_ptr->lev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
	return (project_los(GF_SLEEP, p_ptr->lev));
}

/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_los(GF_AWAY_EVIL, dist));
}

/*
 * Turn undead
 */
bool turn_undead(int dam)
{
	return (project_los(GF_TURN_UNDEAD, dam));
}

/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_los(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (project_los(GF_DISP_EVIL, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_demon(int dam)
{
	return (project_los(GF_DISP_DEMON, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (project_los(GF_DISP_ALL, dam));
}


/*
 * Stun monsters
 */
bool stun_monsters(int dam)
{
	return (project_los(GF_STUN, dam));
}

/*
 * Confuse monsters
 */
bool confuse_monsters(int dam)
{
	return (project_los(GF_CONFUSION, dam));
}


/*
 * Turn everyone
 */
bool turn_monsters(int dam)
{
	return (project_los(GF_TURN_ALL, dam));
}

/*
 * Charm monsters
 */
bool charm_monsters(int dam)
{
	return (project_los(GF_CHARM, dam));
}


/*
 * Charm animals
 */
bool charm_animals(int dam)
{
	return (project_los(GF_CONTROL_ANIMAL, dam));
}

/*
 * Send everyone away
 */
bool manifest_god(void)
{
	return (project_los(GF_AWAY_ALL, 30));
}

