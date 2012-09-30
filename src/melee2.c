/* File: melee2.c */

/* Purpose: Monster spells and movement */

/*
* Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
*
* This software may be copied and distributed for educational, research, and
* not for profit purposes provided that this copyright and statement are
* included in all such copies.
*/

/*
* This file has several additions to it by Keldon Jones (keldon@umr.edu)
* to improve the general quality of the AI (version 0.1.1).
*/

#include "angband.h"

#define SPEAK_CHANCE 8
#define GRINDNOISE 20
#define CYBERNOISE 20


/*
 * Calculate the direction to the next enemy
 */
static bool get_enemy_dir(monster_type *m_ptr, int *mm)
{
	int i;
	int x, y;
	int t_idx;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_type *t_ptr;

	/* Scan thru all monsters */
	for (i = 1; i < m_max; i++)
	{
		t_idx = i;
		t_ptr = &m_list[t_idx];

		/* The monster itself isn't a target */
		if (t_ptr == m_ptr) continue;

		/* Paranoia -- Skip dead monsters */
		if (!t_ptr->r_idx) continue;
		
		/* Monster must be 'an enemy' */
		if (!are_enemies(m_ptr, t_ptr)) continue;

		/* Mega Hack - Monster must be close */
		if (ABS(m_ptr->fy - t_ptr->fy) + ABS(m_ptr->fx - t_ptr->fx) > 20)
		{
			continue;
		}
		
		if (is_pet(m_ptr))
		{
			/* Hack -- only fight away from player */
			if (p_ptr->pet_follow_distance < 0)
			{
				/* No fighting near player */
				if (t_ptr->cdis <= (0 - p_ptr->pet_follow_distance))
				{
					continue;
				}
			}
			/* Hack -- no fighting away from player */
			else if ((m_ptr->cdis < t_ptr->cdis) &&
						(t_ptr->cdis > p_ptr->pet_follow_distance))
			{
				continue;
			}
		}

		/* Monster must be projectable if we can't pass through walls */
		if (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) &&
			!projectable(m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx))
		{
			continue;
		}

		/* OK -- we've got a target */
		y = t_ptr->fy;
		x = t_ptr->fx;

		/* Extract the direction */
		x -= m_ptr->fx;
		y -= m_ptr->fy;

		/* North */
		if ((y < 0) && (x == 0))
		{
			mm[0] = 8;
			mm[1] = 7;
			mm[2] = 9;
		}
		/* South */
		else if ((y > 0) && (x == 0))
		{
			mm[0] = 2;
			mm[1] = 1;
			mm[2] = 3;
		}
		/* East */
		else if ((x > 0) && (y == 0))
		{
			mm[0] = 6;
			mm[1] = 9;
			mm[2] = 3;
		}
		/* West */
		else if ((x < 0) && (y == 0))
		{
			mm[0] = 4;
			mm[1] = 7;
			mm[2] = 1;
		}
		/* North-West */
		else if ((y < 0) && (x < 0))
		{
			mm[0] = 7;
			mm[1] = 4;
			mm[2] = 8;
		}
		/* North-East */
		else if ((y < 0) && (x > 0))
		{
			mm[0] = 9;
			mm[1] = 6;
			mm[2] = 8;
		}
		/* South-West */
		else if ((y > 0) && (x < 0))
		{
			mm[0] = 1;
			mm[1] = 4;
			mm[2] = 2;
		}
		/* South-East */
		else if ((y > 0) && (x > 0))
		{
			mm[0] = 3;
			mm[1] = 6;
			mm[2] = 2;
		}

		/* Found a monster */
		return TRUE;
	}

	/* No monster found */
	return FALSE;
}


/*
 * Hack, based on mon_take_hit... perhaps all monster attacks on
 * other monsters should use this?
 */
void mon_take_hit_mon(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type	*m_ptr = &m_list[m_idx];

	monster_race	*r_ptr = &r_info[m_ptr->r_idx];

	char m_name[160];

	/* Can the player be aware of this attack? */
	bool known = (m_ptr->cdis <= MAX_SIGHT);

	/* Extract monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	if (m_ptr->invulner && !one_in_(PENETRATE_INVULNERABILITY))
	{
		if (m_ptr->ml)
		{
			msg_format("%^s is unharmed.", m_name);
		}

		return;
	}

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now... or is it? */
	if (m_ptr->hp < 0)
	{
		if ((r_ptr->flags1 & RF1_UNIQUE) ||
			(r_ptr->flags1 & RF1_QUESTOR) ||
			(r_ptr->flags3 & RF3_UNIQUE_7))
		{
			m_ptr->hp = 1;
		}
		else
		{
			/* Make a sound */
			if (!monster_living(r_ptr))
			{
				sound(SOUND_N_KILL);
			}
			else
			{
				sound(SOUND_KILL);
			}

			if (known)
			{
				/* Unseen death by normal attack */
				if (!m_ptr->ml)
				{
					p_ptr->mon_fight = TRUE;
				}
				/* Death by special attack */
				else if (note)
				{
					msg_format("%^s%s", m_name, note);
				}
				/* Death by normal attack -- nonliving monster */
				else if (!monster_living(r_ptr))
				{
					msg_format("%^s is destroyed.", m_name);
				}
				/* Death by normal attack -- living monster */
				else
				{
					msg_format("%^s is killed.", m_name);
				}
			}

			/* Generate treasure */
			(void)monster_death(m_idx, TRUE);
			
			/* Delete the monster */
			delete_monster_idx(m_idx);

			/* Not afraid */
			(*fear) = FALSE;

			/* Monster is dead */
			return;
		}
	}

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint1(dam / 4);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cure fear */
			m_ptr->monfear = 0;

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & RF3_NO_FEAR))
	{
		int		percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		* Run (sometimes) if at 10% or less of max hit points,
		* or (usually) when hit for half its current hit points
		*/
		if (((percentage <= 10) && (randint0(10) < percentage)) ||
			((dam >= m_ptr->hp) && (randint0(100) < 80)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* XXX XXX XXX Hack -- Add some timed fear */
			m_ptr->monfear += (randint1(10) +
				(((dam >= m_ptr->hp) && (percentage > 7)) ?
				20 : ((11 - percentage) * 5)));
		}
	}

	/* Not dead yet */
	return;
}


/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players.  See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency.  Like not using "mod" or "div" when possible.  And
 * attempting to check the conditions in an optimal order.  Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
static int mon_will_run(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

	/* Friends can be commanded to avoid the player */
	if (is_pet(m_ptr))
	{
		/* Are we trying to avoid the player? */
		return ((p_ptr->pet_follow_distance < 0) &&
				  (m_ptr->cdis <= (0 - p_ptr->pet_follow_distance)));
	}

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis <= 5) return (FALSE);

	/* Examine player power (level) */
	p_lev = p_ptr->lev;

	/* Examine monster power (level plus morale) */
	m_lev = r_ptr->level + (m_idx & 0x08) + 25;

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 4 <= p_lev) return (TRUE);

	/* Examine player health */
	p_chp = p_ptr->chp;
	p_mhp = p_ptr->mhp;

	/* Examine monster health */
	m_chp = m_ptr->hp;
	m_mhp = m_ptr->maxhp;

	/* Prepare to optimize the calculation */
	p_val = (p_lev * p_mhp) + (p_chp << 2); /* div p_mhp */
	m_val = (m_lev * m_mhp) + (m_chp << 2); /* div m_mhp */

	/* Strong players scare strong monsters */
	if (p_val * m_mhp > m_val * p_mhp) return (TRUE);

	/* Assume no terror */
	return (FALSE);
}


/*
 * Choose the "best" direction for "flowing"
 *
 * Note that ghosts and rock-eaters are never allowed to "flow",
 * since they should move directly towards the player.
 *
 * Prefer "non-diagonal" directions, but twiddle them a little
 * to angle slightly towards the player's actual location.
 *
 * Allow very perceptive monsters to track old "spoor" left by
 * previous locations occupied by the player.  This will tend
 * to have monsters end up either near the player or on a grid
 * recently occupied by the player (and left via "teleport").
 *
 * Note that if "smell" is turned on, all monsters get vicious.
 *
 * Also note that teleporting away from a location will cause
 * the monsters who were chasing you to converge on that location
 * as long as you are still near enough to "annoy" them without
 * being close enough to chase directly.  I have no idea what will
 * happen if you combine "smell" with low "aaf" values.
 */
static void get_moves_aux(int m_idx, int *yp, int *xp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, y1, x1, when = 0, cost = 999;

	cave_type *c_ptr;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* Monster can go through rocks */
	if (r_ptr->flags2 & RF2_PASS_WALL) return;
	if (r_ptr->flags2 & RF2_KILL_WALL) return;

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* Monster grid */
	c_ptr = area(y1, x1);

	/* The player is not currently near the monster grid */
	if (c_ptr->when < area(py, px)->when)
	{
		/* The player has never been near the monster grid */
		if (!c_ptr->when) return;
	}

	/* Non-pets are too far away to notice the player */
	if (!is_pet(m_ptr))
	{
		if (c_ptr->cost > MONSTER_FLOW_DEPTH) return;
		if (c_ptr->cost > r_ptr->aaf) return;
	}

	/* Hack XXX XXX -- Player can see us, run towards him */
	if (player_has_los_grid(c_ptr)) return;

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Ignore locations off of edge */
		if (!in_bounds2(y, x)) return;

		c_ptr = area(y, x);

		/* Ignore illegal locations */
		if (!c_ptr->when) continue;

		/* Ignore ancient locations */
		if (c_ptr->when < when) continue;

		/* Ignore distant locations */
		if (c_ptr->cost > cost) continue;

		/* Save the cost and time */
		when = c_ptr->when;
		cost = c_ptr->cost;

		/* Hack -- Save the "twiddled" location */
		(*yp) = py + 16 * ddy_ddd[i];
		(*xp) = px + 16 * ddx_ddd[i];
	}
}


/*
* Provide a location to flee to, but give the player a wide berth.
*
* A monster may wish to flee to a location that is behind the player,
* but instead of heading directly for it, the monster should "swerve"
* around the player so that he has a smaller chance of getting hit.
*/
static bool get_fear_moves_aux(int m_idx, int *yp, int *xp)
{
	int y, x, y1, x1, fy, fx, px, py, gy = 0, gx = 0;
	int when = 0, score = -1;
	int i;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Player location */
	py = p_ptr->py;
	px = p_ptr->px;

	/* Monster location */
	fy = m_ptr->fy;
	fx = m_ptr->fx;

	/* Desired destination */
	y1 = fy - (*yp);
	x1 = fx - (*xp);

	/* The player is not currently near the monster grid */
	if (area(fy,fx)->when < area(py, px)->when)
	{
		/* No reason to attempt flowing */
		return (FALSE);
	}

	/* Monster is too far away to use flow information */
	if (area(fy,fx)->cost > MONSTER_FLOW_DEPTH) return (FALSE);
	if (area(fy,fx)->cost > r_ptr->aaf) return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		int dis, s;

		/* Get the location */
		y = fy + ddy_ddd[i];
		x = fx + ddx_ddd[i];

		/* Ignore locations off of edge */
		if (!in_bounds2(y, x)) continue;

		/* Ignore illegal locations */
		if (area(y, x)->when == 0) continue;

		/* Ignore ancient locations */
		if (area(y, x)->when < when) continue;

		/* Calculate distance of this grid from our destination */
		dis = distance(y, x, y1, x1);

		/* Score this grid */
		s = 5000 / (dis + 3) - 500 / (area(y, x)->cost + 1);

		/* No negative scores */
		if (s < 0) s = 0;

		/* Ignore lower scores */
		if (s < score) continue;

		/* Save the score and time */
		when = area(y, x)->when;
		score = s;

		/* Save the location */
		gy = y;
		gx = x;
	}

	/* No legal move (?) */
	if (!when) return (FALSE);

	/* Find deltas */
	(*yp) = fy - gy;
	(*xp) = fx - gx;

	/* Success */
	return (TRUE);
}


/*
 * Hack -- Precompute a bunch of calls to distance() in find_safety() and
 * find_hiding().
 *
 * The pair of arrays dist_offsets_y[n] and dist_offsets_x[n] contain the
 * offsets of all the locations with a distance of n from a central point,
 * with an offset of (0,0) indicating no more offsets at this distance.
 *
 * This is, of course, fairly unreadable, but it eliminates multiple loops
 * from the previous version.
 *
 * It is probably better to replace these arrays with code to compute
 * the relevant arrays, even if the storage is pre-allocated in hard
 * coded sizes.  At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()").  XXX XXX XXX
 *
 * Also, the storage needs could be halved by using bytes.  XXX XXX XXX
 *
 * These arrays could be combined into two big arrays, using sub-arrays
 * to hold the offsets and lengths of each portion of the sub-arrays, and
 * this could perhaps also be used somehow in the "look" code.  XXX XXX XXX
 */


static sint d_off_y_0[] =
{ 0 };

static sint d_off_x_0[] =
{ 0 };


static sint d_off_y_1[] =
{ -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static sint d_off_x_1[] =
{ -1, 0, 1, -1, 1, -1, 0, 1, 0 };


static sint d_off_y_2[] =
{ -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static sint d_off_x_2[] =
{ -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };


static sint d_off_y_3[] =
{ -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
  3, 3, 3, 0 };

static sint d_off_x_3[] =
{ -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
  -1, 0, 1, 0 };


static sint d_off_y_4[] =
{ -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
  0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0 };

static sint d_off_x_4[] =
{ -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
  -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0 };


static sint d_off_y_5[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
  -5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 0 };

static sint d_off_x_5[] =
{ -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
  -5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
  0, 1, 0 };


static sint d_off_y_6[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
  5, 5, 6, 6, 6, 0 };

static sint d_off_x_6[] =
{ -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
  0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
  -3, 2, 3, -1, 0, 1, 0 };


static sint d_off_y_7[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
  3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0 };

static sint d_off_x_7[] =
{ -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
  -3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
  6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
  1, 0 };


static sint d_off_y_8[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
  2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
  8, 8, 8, 0 };

static sint d_off_x_8[] =
{ -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
  5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
  7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
  2, 3, -1, 0, 1, 0 };


static sint d_off_y_9[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
  0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
  7, 8, 8, 8, 8, 9, 9, 9, 0 };

static sint d_off_x_9[] =
{ -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
  -5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
  9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
  4, 5, -2, -3, 2, 3, -1, 0, 1, 0 };


static sint *dist_offsets_y[10] =
{
	d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
	d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

static sint *dist_offsets_x[10] =
{
	d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
	d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};

/*
* Choose a "safe" location near a monster for it to run toward.
*
* A location is "safe" if it can be reached quickly and the player
* is not able to fire into it (it isn't a "clean shot").  So, this will
* cause monsters to "duck" behind walls.  Hopefully, monsters will also
* try to run towards corridor openings if they are in a room.
*
* This function may take lots of CPU time if lots of monsters are
* fleeing.
*
* Return TRUE if a safe location is available.
*/
static bool find_safety(int m_idx, int *yp, int *xp)
{
	monster_type *m_ptr = &m_list[m_idx];

	int py = p_ptr->py;
	int px = p_ptr->px;

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int y, x, dy, dx, d, dis, i;
	int gy = 0, gx = 0, gdis = 0;

	sint *y_offsets;
	sint *x_offsets;

	cave_type *c_ptr;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
		     dx != 0 || dy != 0;
		     i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds(y, x)) continue;

			c_ptr = area(y, x);

			/* Skip locations in a wall */
			if (!cave_floor_grid(c_ptr)) continue;

			/* Ignore grids very far from the player */
			if (c_ptr->when < area(py, px)->when) continue;

			/* Ignore too-distant grids */
			if (c_ptr->cost > area(fy, fx)->cost + 2 * d) continue;

			/* Check for absence of shot (more or less) */
			if (clean_shot(fy, fx, y, x, FALSE))
			{

				/* Calculate distance from player */
				dis = distance(y, x, py, px);

				/* Remember if further than previous */
				if (dis > gdis)
				{
					gy = y;
					gx = x;
					if (!player_has_los_grid(c_ptr))
					{
						gdis = dis * 5;
					}
					else
					{
						gdis = dis;
					}
				}
			}
		}

		/* Check for success */
		if (gdis > d + m_ptr->cdis)
		{
			/* Good location */
			(*yp) = fy - gy;
			(*xp) = fx - gx;

			/* Found safe place */
			return (TRUE);
		}
	}

	/* No safe place */

	/* Save farthest location from player in LOS of monster */
	(*yp) = fy - gy;
	(*xp) = fx - gx;

	/* Hack - return TRUE anyway. */
	return (TRUE);
}


/*
 * Choose a good hiding place near a monster for it to run toward.
 *
 * Pack monsters will use this to "ambush" the player and lure him out
 * of corridors into open space so they can swarm him.
 *
 * Return TRUE if a good location is available.
 */
static bool find_hiding(int m_idx, int *yp, int *xp)
{
	monster_type *m_ptr = &m_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, dy, dx, d, dis, i;
	int gy = 0, gx = 0, gdis = 999;

	sint *y_offsets, *x_offsets;

	cave_type *c_ptr;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
		     dx != 0 || dy != 0;
		     i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds(y, x)) continue;

			c_ptr = area(y, x);

			/* Skip occupied locations */
			if (!cave_empty_grid(c_ptr)) continue;

			/* Check for hidden, available grid */
			if (!player_has_los_grid(c_ptr) && clean_shot(fy, fx, y, x, FALSE))
			{
				/* Calculate distance from player */
				dis = distance(y, x, py, px);

				/* Remember if closer than previous */
				if (dis < gdis && dis >= 2)
				{
					gy = y;
					gx = x;
					gdis = dis;
				}
			}
		}

		/* Check for success */
		if (gdis < 999)
		{
			/* Good location */
			(*yp) = fy - gy;
			(*xp) = fx - gx;

			/* Found good place */
			return (TRUE);
		}
	}

	/* No good place */
	return (FALSE);
}


/*
 * Choose "logical" directions for monster movement
 */
static bool get_moves(int m_idx, int *mm)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int          y, ay, x, ax;
	int          move_val = 0;
	int          y2 = py;
	int          x2 = px;
	bool         done = FALSE;
	bool         will_run = mon_will_run(m_idx);
	cave_type	*c_ptr;

	/* Flow towards the player */
	(void)get_moves_aux(m_idx, &y2, &x2);

	/* Extract the "pseudo-direction" */
	y = m_ptr->fy - y2;
	x = m_ptr->fx - x2;

	if (!stupid_monsters && !will_run && is_hostile(m_ptr))
	{
	/*
	 * Animal packs try to get the player out of corridors
	 * (...unless they can move through walls -- TY)
	 */
		if ((r_ptr->flags1 & RF1_FRIENDS) &&
			 (r_ptr->flags3 & RF3_ANIMAL) &&
			 !(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL)) &&
			 smart_packs)
		{
			int i, room = 0;

			/* Count room grids next to player */
			for (i = 0; i < 8; i++)
			{
				int x = px + ddx_ddd[i];
				int y = py + ddy_ddd[i];

				cave_type *c_ptr;

				if (!in_bounds2(y, x)) continue;

				c_ptr = area(y, x);

				/* Check grid */
				if (((cave_floor_grid(c_ptr)) || ((c_ptr->feat & 0x60) == 0x60)) &&
					 monster_can_cross_terrain(c_ptr->feat, r_ptr))
				{
					/* One more room grid */
					room++;
				}
			}

			/* Not in a room and strong player */
			if (room <= (8 * (p_ptr->chp + p_ptr->csp)) /
			    (p_ptr->mhp + p_ptr->msp))
			{
				/* Find hiding place */
				if (find_hiding(m_idx, &y, &x)) done = TRUE;
			}
		}

		/* Monster groups try to surround the player */
		if (!done && (r_ptr->flags1 & RF1_FRIENDS))
		{
			int i;

			/* Find an empty square near the player to fill */
			for (i = 0; i < 8; i++)
			{
				/* Pick squares near player (semi-randomly) */
				y2 = py + ddy_ddd[(m_idx + i) & 7];
				x2 = px + ddx_ddd[(m_idx + i) & 7];

				/* Already there? */
				if ((m_ptr->fy == y2) && (m_ptr->fx == x2))
				{
					/* Attack the player */
					y2 = py;
					x2 = px;

					break;
				}

				if (!in_bounds2(y2, x2)) continue;

				/* Ignore filled grids */
				c_ptr = area(y2, x2);
				if (!cave_empty_grid(c_ptr)) continue;

				/* Try to fill this hole */
				break;
			}

			/* Extract the new "pseudo-direction" */
			y = m_ptr->fy - y2;
			x = m_ptr->fx - x2;

			/* Done */
			done = TRUE;
		}
	}

	/* Apply fear if possible and necessary */
	if ((stupid_monsters || is_pet(m_ptr)) && will_run)
	{
		/* XXX XXX Not very "smart" */
		y = (-y), x = (-x);
	}
	else
	{
		if (!done && will_run)
		{
			/* Try to find safe place */
			if (!find_safety(m_idx, &y, &x))
			{
				/* This is not a very "smart" method XXX XXX */
				y = (-y);
				x = (-x);
			}
			else
			{
				/* Adjust movement */
				(void)get_fear_moves_aux(m_idx, &y, &x);
			}
		}
	}


	if (!stupid_monsters)
	{
		/* Check for no move */
		if (!x && !y) return (FALSE);
	}


	/* Extract the "absolute distances" */
	ax = ABS(x);
	ay = ABS(y);

	/* Do something weird */
	if (y < 0) move_val += 8;
	if (x > 0) move_val += 4;

	/* Prevent the diamond maneuvre */
	if (ay > (ax << 1))
	{
		move_val++;
		move_val++;
	}
	else if (ax > (ay << 1))
	{
		move_val++;
	}

	/* Extract some directions */
	switch (move_val)
	{
	case 0:
		mm[0] = 9;
		if (ay > ax)
		{
			mm[1] = 8;
			mm[2] = 6;
			mm[3] = 7;
			mm[4] = 3;
		}
		else
		{
			mm[1] = 6;
			mm[2] = 8;
			mm[3] = 3;
			mm[4] = 7;
		}
		break;
	case 1:
	case 9:
		mm[0] = 6;
		if (y < 0)
		{
			mm[1] = 3;
			mm[2] = 9;
			mm[3] = 2;
			mm[4] = 8;
		}
		else
		{
			mm[1] = 9;
			mm[2] = 3;
			mm[3] = 8;
			mm[4] = 2;
		}
		break;
	case 2:
	case 6:
		mm[0] = 8;
		if (x < 0)
		{
			mm[1] = 9;
			mm[2] = 7;
			mm[3] = 6;
			mm[4] = 4;
		}
		else
		{
			mm[1] = 7;
			mm[2] = 9;
			mm[3] = 4;
			mm[4] = 6;
		}
		break;
	case 4:
		mm[0] = 7;
		if (ay > ax)
		{
			mm[1] = 8;
			mm[2] = 4;
			mm[3] = 9;
			mm[4] = 1;
		}
		else
		{
			mm[1] = 4;
			mm[2] = 8;
			mm[3] = 1;
			mm[4] = 9;
		}
		break;
	case 5:
	case 13:
		mm[0] = 4;
		if (y < 0)
		{
			mm[1] = 1;
			mm[2] = 7;
			mm[3] = 2;
			mm[4] = 8;
		}
		else
		{
			mm[1] = 7;
			mm[2] = 1;
			mm[3] = 8;
			mm[4] = 2;
		}
		break;
	case 8:
		mm[0] = 3;
		if (ay > ax)
		{
			mm[1] = 2;
			mm[2] = 6;
			mm[3] = 1;
			mm[4] = 9;
		}
		else
		{
			mm[1] = 6;
			mm[2] = 2;
			mm[3] = 9;
			mm[4] = 1;
		}
		break;
	case 10:
	case 14:
		mm[0] = 2;
		if (x < 0)
		{
			mm[1] = 3;
			mm[2] = 1;
			mm[3] = 6;
			mm[4] = 4;
		}
		else
		{
			mm[1] = 1;
			mm[2] = 3;
			mm[3] = 4;
			mm[4] = 6;
		}
		break;
	case 12:
		mm[0] = 1;
		if (ay > ax)
		{
			mm[1] = 2;
			mm[2] = 4;
			mm[3] = 3;
			mm[4] = 7;
		}
		else
		{
			mm[1] = 4;
			mm[2] = 2;
			mm[3] = 7;
			mm[4] = 3;
		}
		break;
	}

	/* Wants to move... */
	return (TRUE);
}


static int check_hit2(int power, int level, int ac)
{
	int i, k;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint1(i) > ((ac * 3) / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/* Monster attacks monster */
static bool monst_attack_monst(int m_idx, int t_idx)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_type    *t_ptr = &m_list[t_idx];

	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	monster_race    *tr_ptr = &r_info[t_ptr->r_idx];

	int             ap_cnt;
	int             ac, rlev, pt;
	char            m_name[80], t_name[80];
	char            ddesc[80], temp[80];
	bool            blinked, heal_effect;
	bool            explode = FALSE, touched = FALSE, fear = FALSE;
	int             y_saver = t_ptr->fy;
	int             x_saver = t_ptr->fx;

	bool see_m = m_ptr->ml;
	bool see_t = t_ptr->ml;
	bool see_either = see_m || see_t;

	/* Can the player be aware of this attack? */
	bool known = (m_ptr->cdis <= MAX_SIGHT) || (t_ptr->cdis <= MAX_SIGHT);

	/* Cannot attack self */
	if (m_idx == t_idx) return FALSE;

	/* Not allowed to attack */
	if (r_ptr->flags1 & RF1_NEVER_BLOW) return FALSE;

	/* Wake it up */
	t_ptr->csleep = 0;

	/* Total armor */
	ac = tr_ptr->ac;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the monster name (or "it") */
	monster_desc(t_name, t_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Assume no blink */
	blinked = FALSE;

	if (!see_either && known)
	{
		p_ptr->mon_fight = TRUE;
	}

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool obvious = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;

		/* Stop attacking if the target teleports away */
		if (t_ptr->fx != x_saver || t_ptr->fy != y_saver)
			break;

		/* Stop attacking if the aggressor dies (fire sheath etc.) */
		if ((!m_ptr->r_idx) || (!t_ptr->r_idx)) return TRUE;

		/* Hack -- no more attacks */
		if (!method) break;

		if (blinked) /* Stop! */
		{
			/* break; */
		}

		/* Extract the attack "power" */
		switch (effect)
		{
		case RBE_HURT:          power = 60; break;
		case RBE_POISON:        power =  5; break;
		case RBE_UN_BONUS:      power = 20; break;
		case RBE_UN_POWER:      power = 15; break;
		case RBE_EAT_GOLD:      power =  5; break;
		case RBE_EAT_ITEM:      power =  5; break;
		case RBE_EAT_FOOD:      power =  5; break;
		case RBE_EAT_LITE:      power =  5; break;
		case RBE_ACID:          power =  0; break;
		case RBE_ELEC:          power = 10; break;
		case RBE_FIRE:          power = 10; break;
		case RBE_COLD:          power = 10; break;
		case RBE_BLIND:         power =  2; break;
		case RBE_CONFUSE:       power = 10; break;
		case RBE_TERRIFY:       power = 10; break;
		case RBE_PARALYZE:      power =  2; break;
		case RBE_LOSE_STR:      power =  0; break;
		case RBE_LOSE_DEX:      power =  0; break;
		case RBE_LOSE_CON:      power =  0; break;
		case RBE_LOSE_INT:      power =  0; break;
		case RBE_LOSE_WIS:      power =  0; break;
		case RBE_LOSE_CHR:      power =  0; break;
		case RBE_LOSE_ALL:      power =  2; break;
		case RBE_SHATTER:       power = 60; break;
		case RBE_EXP_10:        power =  5; break;
		case RBE_EXP_20:        power =  5; break;
		case RBE_EXP_40:        power =  5; break;
		case RBE_EXP_80:        power =  5; break;
		case RBE_DISEASE:       power =  5; break;
		case RBE_TIME:          power =  5; break;
		case RBE_EXP_VAMP:      power =  5; break;
		}


		/* Monster hits */
		if (!effect || check_hit2(power, rlev, ac))
		{
			/* Describe the attack method */
			switch (method)
			{
			case RBM_HIT:
				{
					act = "hits %s.";
					touched = TRUE;
					break;
				}

			case RBM_TOUCH:
				{
					act = "touches %s.";
					touched = TRUE;
					break;
				}

			case RBM_PUNCH:
				{
					act = "punches %s.";
					touched = TRUE;
					break;
				}

			case RBM_KICK:
				{
					act = "kicks %s.";
					touched = TRUE;
					break;
				}

			case RBM_CLAW:
				{
					act = "claws %s.";
					touched = TRUE;
					break;
				}

			case RBM_BITE:
				{
					act = "bites %s.";
					touched = TRUE;
					break;
				}

			case RBM_STING:
				{
					act = "stings %s.";
					touched = TRUE;
					break;
				}

			case RBM_XXX1:
				{
					act = "XXX1's %s.";
					break;
				}

			case RBM_BUTT:
				{
					act = "butts %s.";
					touched = TRUE;
					break;
				}

			case RBM_CRUSH:
				{
					act = "crushes %s.";
					touched = TRUE;
					break;
				}

			case RBM_ENGULF:
				{
					act = "engulfs %s.";
					touched = TRUE;
					break;
				}

			case RBM_CHARGE:
				{
					act = "charges %s.";
					touched = TRUE;
					break;
				}

			case RBM_CRAWL:
				{
					act = "crawls on %s.";
					touched = TRUE;
					break;
				}

			case RBM_DROOL:
				{
					act = "drools on %s.";
					touched = FALSE;
					break;
				}

			case RBM_SPIT:
				{
					act = "spits on %s.";
					touched = FALSE;
					break;
				}

			case RBM_EXPLODE:
				{
					if (see_either) disturb(TRUE);
					act = "explodes.";
					explode = TRUE;
					touched = FALSE;
					break;
				}

			case RBM_GAZE:
				{
					act = "gazes at %s.";
					touched = FALSE;
					break;
				}

			case RBM_WAIL:
				{
					act = "wails at %s.";
					touched = FALSE;
					break;
				}

			case RBM_SPORE:
				{
					act = "releases spores at %s.";
					touched = FALSE;
					break;
				}

			case RBM_XXX4:
				{
					act = "projects XXX4's at %s.";
					touched = FALSE;
					break;
				}

			case RBM_BEG:
				{
					act = "begs %s for money.";
					touched = FALSE;
					break;
				}

			case RBM_INSULT:
				{
					act = "insults %s.";
					touched = FALSE;
					break;
				}

			case RBM_MOAN:
				{
					act = "moans at %s.";
					touched = FALSE;
					break;
				}

			case RBM_SHOW:
				{
					act = "sings to %s.";
					touched = FALSE;
					break;
				}
			}

			/* Message */
			if (act && see_either)
			{
				/* Look to see if we've spotted a mimic */
				if ((m_ptr->smart & SM_MIMIC) && see_m)
				{
					char m_name2[80];
		
					/* Get name */
					monster_desc (m_name2, m_ptr, 0x88);
		
					/* Toggle flag */
					m_ptr->smart &= ~(SM_MIMIC);
					
					/* It is in the monster list now */
					update_mon_vis(m_ptr->r_idx, 1);
		
					/* We've spotted it */
					msg_format("You see %s!", m_name2);
				}

				/* Look to see if we've spotted a mimic */
				if ((t_ptr->smart & SM_MIMIC) && see_t)
				{
					char t_name2[80];
		
					/* Get name */
					monster_desc (t_name2, t_ptr, 0x88);
					
					/* Toggle flag */
					t_ptr->smart &= ~(SM_MIMIC);

					/* It is in the monster list now */
					update_mon_vis(t_ptr->r_idx, 1);
		
					/* We've spotted it */
					msg_format("You see %s!", t_name2);
				}

				if ((p_ptr->image) && one_in_(3))
				{
					strfmt(temp, "%s %s.",
					       silly_attacks[randint0(MAX_SILLY_ATTACK)],t_name);
				}
				else
					strfmt(temp, act, t_name);

				msg_format("%^s %s", m_name, temp);
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

			/* Assume no healing effect */
			heal_effect = FALSE;

			pt = GF_MISSILE;

			/* Apply appropriate damage */
			switch (effect)
			{
			case 0:
				{
					damage = 0;
					pt = 0;
					break;
				}

			case RBE_HURT:
				{
					damage -= (damage * ((ac < 150) ? ac : 150) / 250);
					break;
				}

			case RBE_POISON:
			case RBE_DISEASE:
				{
					pt = GF_POIS;
					break;
				}

			case RBE_UN_BONUS:
			case RBE_UN_POWER:
				{
					pt = GF_DISENCHANT;
					break;
				}

			case RBE_EAT_FOOD:
			case RBE_EAT_LITE:
				{
					pt = damage = 0;
					break;
				}

			case RBE_EAT_ITEM:
			case RBE_EAT_GOLD:
				{
					pt = damage = 0;
					if (one_in_(2)) blinked = TRUE;
					break;
				}

			case RBE_ACID:
				{
					pt = GF_ACID;
					break;
				}

			case RBE_ELEC:
				{
					pt = GF_ELEC;
					break;
				}

			case RBE_FIRE:
				{
					pt = GF_FIRE;
					break;
				}

			case RBE_COLD:
				{
					pt = GF_COLD;
					break;
				}

			case RBE_BLIND:
				{
					break;
				}

			case RBE_CONFUSE:
				{
					pt = GF_CONFUSION;
					break;
				}

			case RBE_TERRIFY:
				{
					pt = GF_TURN_ALL;
					break;
				}

			case RBE_PARALYZE:
				{
					pt = GF_OLD_SLEEP; /* sort of close... */
					break;
				}

			case RBE_LOSE_STR:
			case RBE_LOSE_INT:
			case RBE_LOSE_WIS:
			case RBE_LOSE_DEX:
			case RBE_LOSE_CON:
			case RBE_LOSE_CHR:
			case RBE_LOSE_ALL:
				{
					break;
				}
			case RBE_SHATTER:
				{
					if (damage > 23)
					{
						earthquake(m_ptr->fy, m_ptr->fx, 8);
					}
					break;
				}
			case RBE_EXP_10:
			case RBE_EXP_20:
			case RBE_EXP_40:
			case RBE_EXP_80:
				{
					pt = GF_NETHER;
					break;
				}
			case RBE_TIME:
				{
					pt = GF_TIME;
					break;
				}
			case RBE_EXP_VAMP:
				{
					pt = GF_OLD_DRAIN;
					heal_effect = TRUE;
					break;
				}

			default:
				{
					pt = 0;
					break;
				}
			}

			if (pt)
			{
				/* Do damage if not exploding */
				if (!explode)
				{
					project(m_idx, 0, t_ptr->fy, t_ptr->fx,
						(pt == GF_OLD_SLEEP ? r_ptr->level : damage), pt, PROJECT_KILL | PROJECT_STOP);
				}

				if (heal_effect)
				{
					if ((monster_living(tr_ptr)) && (damage > 2))
					{
						bool did_heal = FALSE;

						if (m_ptr->hp < m_ptr->maxhp) did_heal = TRUE;

						/* Heal */
						m_ptr->hp += damroll(4, damage / 6);
						if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

						/* Redraw (later) if needed */
						if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

						/* Special message */
						if (see_m && did_heal)
						{
							msg_format("%^s appears healthier.", m_name);
						}
					}
				}

				if (touched)
				{
					/* Aura fire */
					if ((tr_ptr->flags2 & RF2_AURA_FIRE) &&
						!(r_ptr->flags3 & RF3_IM_FIRE))
					{
						if (see_either)
						{
							blinked = FALSE;
							msg_format("%^s is suddenly very hot!", m_name);
							if (see_t)
								tr_ptr->r_flags2 |= RF2_AURA_FIRE;
						}
						project(t_idx, 0, m_ptr->fy, m_ptr->fx,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_FIRE, PROJECT_KILL | PROJECT_STOP);
					}

					/* Aura cold */
					if ((tr_ptr->flags3 & RF3_AURA_COLD) &&
						!(r_ptr->flags3 & RF3_IM_COLD))
					{
						if (see_either)
						{
							blinked = FALSE;
							msg_format("%^s is suddenly very cold!", m_name);
							if (see_t)
								tr_ptr->r_flags3 |= RF3_AURA_COLD;
						}
						project(t_idx, 0, m_ptr->fy, m_ptr->fx,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_COLD, PROJECT_KILL | PROJECT_STOP);
					}

					/* Aura elec */
					if ((tr_ptr->flags2 & (RF2_AURA_ELEC)) && !(r_ptr->flags3 & (RF3_IM_ELEC)))
					{
						if (see_either)
						{
							blinked = FALSE;
							msg_format("%^s gets zapped!", m_name);
							if (see_t)
								tr_ptr->r_flags2 |= RF2_AURA_ELEC;
						}
						project(t_idx, 0, m_ptr->fy, m_ptr->fx,
							damroll (1 + ((tr_ptr->level) / 26),
							1 + ((tr_ptr->level) / 17)),
							GF_ELEC, PROJECT_KILL | PROJECT_STOP);
					}

				}
			}
		}

		/* Monster missed the monster */
		else
		{
			/* Analyze failed attacks */
			switch (method)
			{
			case RBM_HIT:
			case RBM_TOUCH:
			case RBM_PUNCH:
			case RBM_KICK:
			case RBM_CLAW:
			case RBM_BITE:
			case RBM_STING:
			case RBM_XXX1:
			case RBM_BUTT:
			case RBM_CRUSH:
			case RBM_ENGULF:
			case RBM_CHARGE:
				{
					/* Visible monsters */
					if (see_m)
					{
						/* Message */
						msg_format("%^s misses %s.", m_name, t_name);
					}

					break;
				}
			}
		}


		/* Analyze "visible" monsters only */
		if (see_m)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					r_ptr->r_blows[ap_cnt]++;
				}
			}
		}
	}

	if (explode)
	{
		sound(SOUND_EXPLODE);

		/* Cancel Invulnerability */
		if (m_ptr->invulner) m_ptr->invulner = 0;

		mon_take_hit_mon(m_idx, m_ptr->hp + 1, &fear, " explodes into tiny shreds.");

		blinked = FALSE;
	}


	/* Blink away */
	if (blinked)
	{
		if (see_m)
		{
			msg_print("The thief flees laughing!");
		}
		else if (known)
		{
			p_ptr->mon_fight = TRUE;
		}

		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}

	return TRUE;
}


/*
 * Process a monster
 *
 * The monster is known to be within 100 grids of the player
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * XXX XXX XXX In addition, monsters which *cannot* open or bash
 * down a door will still stand there trying to open it...
 *
 * XXX Technically, need to check for monster in the way
 * combined with that monster being in a wall (or door?)
 *
 * A "direction" of "5" means "pick a random direction".
 */
static void process_monster(int m_idx)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	int             i, d, oy, ox, ny, nx;

	int             mm[8];

	cave_type       *c_ptr;

	monster_type    *y_ptr;

	
	char m_name[80];

	bool            do_turn;
	bool            do_move;
	bool            do_view;

	bool            did_open_door;
	bool            did_bash_door;
	bool            did_take_item;
	bool            did_kill_item;
	bool            did_move_body;
	bool            did_pass_wall;
	bool            did_kill_wall;
	bool            gets_angry = FALSE;
	field_mon_test	mon_enter_test;

	/* Quantum monsters are odd */
	if (r_ptr->flags2 & (RF2_QUANTUM))
	{
		/* Sometimes skip move */
		if (one_in_(2)) return;

		/* Sometimes die */
		if (one_in_((m_idx % 100) + 10) && !(r_ptr->flags1 & RF1_QUESTOR))
		{
			bool sad = FALSE;

			if (is_pet(m_ptr) && !(m_ptr->ml))
				sad = TRUE;

			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Oops */
				msg_format("%^s disappears!", m_name);
			}

			/* Generate treasure, etc */
			(void)monster_death(m_idx, TRUE);

			/* Delete the monster */
			delete_monster_idx(m_idx);

			if (sad)
			{
				msg_print("You feel sad for a moment.");
			}

			return;
		}
	}
	
	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;
	
	/* Access that cave grid */
	c_ptr = area(oy,ox);

	/* Process fields under the monster. */
	field_hook(&c_ptr->fld_idx, FIELD_ACT_MONSTER_ON, (vptr) m_ptr);

	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		u32b notice = 0;

		/* Hack -- handle non-aggravation */
		if (!p_ptr->aggravate) notice = randint0(1024);

		/* Nightmare monsters are more alert */
		if (ironman_nightmare) notice /= 2;

		/* Hack -- See if monster "notices" player */
		if ((notice * notice * notice) <= p_ptr->noise)
		{
			/* Hack -- amount of "waking" */
			int d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50) d = (100 / m_ptr->cdis);

			/* Hack -- handle aggravation */
			if (p_ptr->aggravate) d = m_ptr->csleep;

			/* Still asleep */
			if (m_ptr->csleep > d)
			{
				/* Monster wakes up "a little bit" */
				m_ptr->csleep -= d;

				/* Notice the "not waking up" */
				if (m_ptr->ml)
				{
					/* Hack -- Count the ignores */
					if (r_ptr->r_ignore < MAX_UCHAR)
					{
						r_ptr->r_ignore++;
					}
				}
			}

			/* Just woke up */
			else
			{
				/* Reset sleep counter */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if ((m_ptr->ml) && (!(m_ptr->smart & SM_MIMIC)))
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);

					/* Redraw the health bar */
					if (p_ptr->health_who == m_idx)
						p_ptr->redraw |= (PR_HEALTH);

					/* Hack -- Count the wakings */
					if (r_ptr->r_wake < MAX_UCHAR)
					{
						r_ptr->r_wake++;
					}
				}
			}
		}

		/* Still sleeping */
		if (m_ptr->csleep) return;
	}


	/* Handle "stun" */
	if (m_ptr->stunned)
	{
		int d = 1;

		/* Make a "saving throw" against stun */
		if (randint0(10000) <= r_ptr->level * r_ptr->level)
		{
			/* Recover fully */
			d = m_ptr->stunned;
		}

		/* Hack -- Recover from stun */
		if (m_ptr->stunned > d)
		{
			/* Recover somewhat */
			m_ptr->stunned -= d;
		}

		/* Fully recover */
		else
		{
			/* Recover fully */
			m_ptr->stunned = 0;

			/* Message if visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);
			}
		}

		/* Still stunned */
		if (m_ptr->stunned) return;
	}


	/* Handle confusion */
	if (m_ptr->confused)
	{
		/* Amount of "boldness" */
		int d = randint1(r_ptr->level / 20 + 1);

		/* Still confused */
		if (m_ptr->confused > d)
		{
			/* Reduce the confusion */
			m_ptr->confused -= d;
		}

		/* Recovered */
		else
		{
			/* No longer confused */
			m_ptr->confused = 0;

			/* Message if visible */
			if ((m_ptr->ml) && (!(m_ptr->smart & SM_MIMIC)))
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);
			}
		}
	}

	/* Handle Invulnerability */
	if (m_ptr->invulner)
	{
		/* Reduce by one, note if expires */
		m_ptr->invulner--;

		if (!(m_ptr->invulner) && m_ptr->ml)
		{
			char m_name[80];

			/* Acquire the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			msg_format("%^s is no longer invulnerable.", m_name);
		}
	}

	/* No one wants to be your friend if you're aggravating */
	if (!is_hostile(m_ptr) && p_ptr->aggravate)
		gets_angry = TRUE;

	if (gets_angry)
	{
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);
		msg_format("%^s suddenly becomes hostile!", m_name);
		set_hostile(m_ptr);
	}

	/* Handle "fear" */
	if (m_ptr->monfear)
	{
		/* Amount of "boldness" */
		int d = randint1(r_ptr->level / 20 + 1);

		/* Still afraid */
		if (m_ptr->monfear > d)
		{
			/* Reduce the fear */
			m_ptr->monfear -= d;
		}

		/* Recover from fear, take note if seen */
		else
		{
			/* No longer afraid */
			m_ptr->monfear = 0;

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];
				char m_poss[80];

				/* Acquire the monster name/poss */
				monster_desc(m_name, m_ptr, 0);
				monster_desc(m_poss, m_ptr, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);
			}
		}
	}


	/* Attempt to "multiply" if able and allowed */
	if ((r_ptr->flags2 & RF2_MULTIPLY) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters */
		for (k = 0, y = oy - 1; y <= oy + 1; y++)
		{
			for (x = ox - 1; x <= ox + 1; x++)
			{
				/* Ignore locations off of edge */
				if (!in_bounds2(y, x)) continue;

				if (area(y, x)->m_idx) k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || one_in_(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(m_idx, FALSE, is_friendly(m_ptr), is_pet(m_ptr)))
			{
				/* Take note if visible */
				if (m_ptr->ml)
				{
					r_ptr->r_flags2 |= (RF2_MULTIPLY);
				}

				/* Multiplying takes energy */
				return;
			}
		}
	}


	/* Hack! "Cyber" monster makes noise... */
	if (strstr((r_name + r_ptr->name), "Cyber") && one_in_(CYBERNOISE) &&
	    !m_ptr->ml && (m_ptr->cdis <= MAX_SIGHT))
	{
		msg_print("You hear heavy steps.");
	}

	/* Access that cave grid */
	c_ptr = area(oy,ox);

	/* Some monsters can speak */
	if (speak_unique &&
	    (r_ptr->flags2 & RF2_CAN_SPEAK) && one_in_(SPEAK_CHANCE) &&
		player_has_los_grid(c_ptr))
	{
		char m_name[80];
		char monmessage[1024];
		cptr filename;

		/* Acquire the monster name/poss */
		if (m_ptr->ml)
			monster_desc(m_name, m_ptr, 0);
		else
			strcpy(m_name, "It");

		/* Select the file for monster quotes */
		if (m_ptr->monfear)
			filename = "monfear.txt";
		else if (is_friendly(m_ptr))
			filename = "monfrien.txt";
		else
			filename = "monspeak.txt";

		/* Get the monster line */
		if (get_rnd_line(filename, m_ptr->r_idx, monmessage) == 0)
		{
			/* Say something */
			msg_format("%^s %s", m_name, monmessage);
		}
	}


	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;

	/*
	 * Attempt to cast a spell at an enemy other than the player
	 * (may slow the game a smidgeon, but I haven't noticed.)
	 */
	if (monst_spell_monst(m_idx)) return;


	/* Hack -- Assume no movement */
	mm[0] = mm[1] = mm[2] = mm[3] = 0;
	mm[4] = mm[5] = mm[6] = mm[7] = 0;

#ifdef USE_SCRIPT
	if (monster_move_callback(mm, m_idx))
	{
	}
	else
#endif /* USE_SCRIPT */


	/* Confused -- 100% random */
	if (m_ptr->confused)
	{
		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* 75% random movement */
	else if ((r_ptr->flags1 & RF1_RAND_50) && (r_ptr->flags1 & RF1_RAND_25) &&
	         (randint0(100) < 75))
	{
		/* Memorize flags */
		if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_50);
		if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_25);

		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* 50% random movement */
	else if ((r_ptr->flags1 & RF1_RAND_50) && (randint0(100) < 50))
	{
		/* Memorize flags */
		if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_RAND_50);

		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* 25% random movement */
	else if ((r_ptr->flags1 & RF1_RAND_25) && (randint0(100) < 25))
	{
		/* Memorize flags */
		if (m_ptr->ml) r_ptr->r_flags1 |= RF1_RAND_25;

		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* Can't reach player - find something else to hit */
	else if ((r_ptr->flags1 & RF1_NEVER_MOVE) && (m_ptr->cdis > 1))
	{
		/* Try four "random" directions */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;
	}

	/* Pets will follow the player */
	else if (is_pet(m_ptr))
	{
		/* Are we trying to avoid the player? */
		bool avoid = ((p_ptr->pet_follow_distance < 0) &&
						  (m_ptr->cdis <= (0 - p_ptr->pet_follow_distance)));

		/* Do we want to find the player? */
		bool lonely = (!avoid && (m_ptr->cdis > p_ptr->pet_follow_distance));

		/* Should we find the player if we can't find a monster? */
		bool distant = (m_ptr->cdis > PET_SEEK_DIST);

		/* by default, move randomly */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;

		/* Look for an enemy */
		if (!get_enemy_dir(m_ptr, mm))
		{
			/* Find the player if necessary */
			if (avoid || lonely || distant)
			{
				/* Remember the leash length */
				int dis = p_ptr->pet_follow_distance;

				/* Hack -- adjust follow distance temporarily */
				if (p_ptr->pet_follow_distance > PET_SEEK_DIST)
				{
					p_ptr->pet_follow_distance = PET_SEEK_DIST;
				}

				/* Find the player */
				get_moves(m_idx, mm);

				/* Restore the leash */
				p_ptr->pet_follow_distance = dis;
			}
		}
	}

	/* Friendly monster movement */
	else if (!is_hostile(m_ptr))
	{
		/* by default, move randomly */
		mm[0] = mm[1] = mm[2] = mm[3] = 5;

		/* Look for an enemy */
		get_enemy_dir(m_ptr, mm);
	}
	/* Normal movement */
	else if (stupid_monsters)
	{
		/* Logical moves */
		get_moves(m_idx, mm);
	}
	else
	{
		/* Logical moves, may do nothing */
		if (!get_moves(m_idx, mm)) return;
	}

	/* Assume nothing */
	do_turn = FALSE;
	do_move = TRUE;
	do_view = FALSE;

	/* Assume nothing */
	did_open_door = FALSE;
	did_bash_door = FALSE;
	did_take_item = FALSE;
	did_kill_item = FALSE;
	did_move_body = FALSE;
	did_pass_wall = FALSE;
	did_kill_wall = FALSE;


	/* Take a zero-terminated array of "directions" */
	for (i = 0; mm[i]; i++)
	{
		/* Get the direction */
		d = mm[i];

		/* Hack -- allow "randomized" motion */
		if (d == 5) d = ddd[randint0(8)];

		/* Get the destination */
		ny = oy + ddy[d];
		nx = ox + ddx[d];

		/* Ignore locations off of edge */
		if (!in_bounds2(ny, nx)) continue;

		/* Access that cave grid */
		c_ptr = area(ny, nx);

		/* Access that cave grid's contents */
		y_ptr = &m_list[c_ptr->m_idx];

		/* Floor is open? */
		if (cave_floor_grid(c_ptr))
		{
			/* Go ahead and move */
			do_move = TRUE;
		}

		/* Hack -- player 'in' wall */
		else if ((ny == p_ptr->py) && (nx == p_ptr->px))
		{
			do_move = TRUE;
		}

		else if (c_ptr->m_idx)
		{
			/* Possibly a monster to attack */
			do_move = TRUE;
		}

		/* Permanent wall */
		else if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
			(c_ptr->feat <= FEAT_PERM_SOLID))
		{
			do_move = FALSE;
		}

		/* Hack -- semi-transparent terrains are no obstacle */
		else if ((c_ptr->feat & 0x60) == 0x60)
		{
			do_move = TRUE;
		}


		/* Monster moves through walls (and doors) */
		else if (r_ptr->flags2 & RF2_PASS_WALL)
		{
			/* Pass through walls/doors/rubble */
			do_move = TRUE;

			/* Monster went through a wall */
			did_pass_wall = TRUE;
		}

		/* Monster destroys walls (and doors) */
		else if (r_ptr->flags2 & RF2_KILL_WALL)
		{
			/* Eat through walls/doors/rubble */
			do_move = TRUE;

			/* Monster destroyed a wall */
			did_kill_wall = TRUE;

			if (one_in_(GRINDNOISE))
			{
				msg_print("There is a grinding sound.");
			}

			/* Forget the wall */
			c_ptr->info &= ~(CAVE_MARK);

			/* Notice */
			cave_set_feat(ny, nx, FEAT_FLOOR);

			/* Note changes to viewable region */
			if (player_can_see_bold(ny, nx)) do_view = TRUE;
		}
		else if (((c_ptr->feat >= FEAT_RUBBLE) &&
			(c_ptr->feat <= FEAT_WALL_SOLID))
			 || (c_ptr->feat == FEAT_PILLAR))
		{
			/* This monster cannot walk through walls */
			do_move = FALSE;
		}

		/* Some monsters never attack */
		if (do_move && (ny == p_ptr->py) && (nx == p_ptr->px) &&
			(r_ptr->flags1 & RF1_NEVER_BLOW))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_BLOW);

			/* Do not move */
			do_move = FALSE;
		}
		
		/* Require "empty" fields */
		if (fields_have_flags(c_ptr->fld_idx, FIELD_INFO_NO_ENTER))
		{
			do_move = FALSE;
		}
		
		/* 
		 * Test for fields that will not allow this
		 * specific monster to pass.  (i.e. Glyph of warding)
		 */
		 
		/* Initialise information to pass to action functions */
		mon_enter_test.m_ptr = m_ptr;
		mon_enter_test.do_move = do_move;
		
		/* Call the hook */
		field_hook(&c_ptr->fld_idx, FIELD_ACT_MON_ENTER_TEST,
			 (vptr) &mon_enter_test);
			 
		/* Take turn in some cases. */
		if (!mon_enter_test.do_move && do_move) do_turn = TRUE;
		
		/* Get result */
		do_move = mon_enter_test.do_move;

		/* Handle closed doors and secret doors */
		if (do_move && ((c_ptr->feat == FEAT_CLOSED)
			 || (c_ptr->feat == FEAT_SECRET)) &&
			 (r_ptr->flags2 & RF2_OPEN_DOOR) &&
			 (!is_pet(m_ptr) || p_ptr->pet_open_doors))
		{
			/* Open the door */
			cave_set_feat(ny, nx, FEAT_OPEN);

			/* Handle viewable doors */
			if (player_can_see_bold(ny, nx)) do_view = TRUE;
				
			/* Take a turn */
			do_turn = TRUE;
				
			/* Do not move in any case. */
			do_move = FALSE;
			
			/* The door was opened */ 
			did_open_door = TRUE;
		}
		else if (((c_ptr->feat == FEAT_CLOSED) || (c_ptr->feat == FEAT_SECRET))
			 && !did_pass_wall)
		{
			/* Monsters cannot walk through closed doors */
			do_move = FALSE;
		}

		/* The player is in the way.  Attack him. */
		if (do_move && (ny == p_ptr->py) && (nx == p_ptr->px))
		{
			/* Do the attack */
			(void)make_attack_normal(m_idx);

			/* Do not move */
			do_move = FALSE;

			/* Took a turn */
			do_turn = TRUE;
		}

		if ((c_ptr->feat >= FEAT_PATTERN_START) &&
			(c_ptr->feat <= FEAT_PATTERN_XTRA2) &&
			!do_turn && !(r_ptr->flags7 & RF7_CAN_FLY))
		{
			do_move = FALSE;
		}


		/* A monster is in the way */
		if (do_move && c_ptr->m_idx)
		{
			monster_race *z_ptr = &r_info[y_ptr->r_idx];
			monster_type *m2_ptr = &m_list[c_ptr->m_idx];

			/* Assume no movement */
			do_move = FALSE;

			/* Attack 'enemies' */
			if (((r_ptr->flags2 & (RF2_KILL_BODY)) &&
				  (r_ptr->mexp * r_ptr->level > z_ptr->mexp * z_ptr->level) &&
				  (cave_floor_grid(c_ptr))) ||
				 are_enemies(m_ptr, m2_ptr) || m_ptr->confused)
			{
				do_move = FALSE;

				if (r_ptr->flags2 & RF2_KILL_BODY)
					r_ptr->r_flags2 |= (RF2_KILL_BODY);

				/* attack */
				if ((m2_ptr->r_idx) && (m2_ptr->hp >= 0))
				{
					if (monst_attack_monst(m_idx, area(ny,nx)->m_idx))
					return;
				}
			}

			/* Push past weaker monsters (unless leaving a wall) */
			else if ((r_ptr->flags2 & RF2_MOVE_BODY) &&
				(r_ptr->mexp > z_ptr->mexp) && cave_floor_grid(c_ptr) &&
				(cave_floor_grid(area(m_ptr->fy, m_ptr->fx))))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster pushed past another monster */
				did_move_body = TRUE;

				/* XXX XXX XXX Message */
			}
		}

		/*
		 * Check if monster can cross terrain
		 * This is checked after the normal attacks
		 * to allow monsters to attack an enemy,
		 * even if it can't enter the terrain.
		 */
		if (do_move && !monster_can_cross_terrain(c_ptr->feat, r_ptr))
		{
			/* Assume no move allowed */
			do_move = FALSE;
		}

		/* Some monsters never move */
		if (do_move && (r_ptr->flags1 & RF1_NEVER_MOVE))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) r_ptr->r_flags1 |= (RF1_NEVER_MOVE);

			/* Do not move */
			do_move = FALSE;
		}



		/* Creature has been allowed move */
		if (do_move)
		{
			s16b this_o_idx, next_o_idx;
			
			cave_type *old_ptr = area(oy, ox);

			/* Take a turn */
			do_turn = TRUE;

			/* Look to see if we've spotted a mimic */
			if ((m_ptr->smart & SM_MIMIC) && m_ptr->ml)
			{
				char m_name2[80];
		
				/* Get name */
				monster_desc (m_name2, m_ptr, 0x88);
				
				/* Toggle flag */
				m_ptr->smart &= ~(SM_MIMIC);
		
				/* It is in the monster list now */
				update_mon_vis(m_ptr->r_idx, 1);
						
				/* We've spotted it */
				msg_format("You see %s!", m_name2);
			}

			/* Process fields under the monster. */
			field_hook(&old_ptr->fld_idx,
				 FIELD_ACT_MONSTER_LEAVE, (vptr) m_ptr);
			
			/* Hack -- Update the old location */
			old_ptr->m_idx = c_ptr->m_idx;

			/* Mega-Hack -- move the old monster, if any */
			if (c_ptr->m_idx)
			{
				/* Move the old monster */
				y_ptr->fy = oy;
				y_ptr->fx = ox;

				/* Update the old monster */
				update_mon(c_ptr->m_idx, TRUE);

				/* Wake up the moved monster */
				m_list[c_ptr->m_idx].csleep = 0;
			}

			/* Hack -- Update the new location */
			c_ptr->m_idx = m_idx;

			/* Move the monster */
			m_ptr->fy = ny;
			m_ptr->fx = nx;
			
			/* Update the monster */
			update_mon(m_idx, TRUE);
			
			/* Process fields under the monster. */
			field_hook(&old_ptr->fld_idx,
				 FIELD_ACT_MONSTER_ENTER, (vptr) m_ptr);

			/* Redraw the old grid */
			lite_spot(oy, ox);

			/* Redraw the new grid */
			lite_spot(ny, nx);

			/* Possible disturb */
			if (m_ptr->ml && (disturb_move ||
				((m_ptr->mflag & MFLAG_VIEW) &&
				disturb_near)))
			{
				/* Disturb */
				if (is_hostile(m_ptr))
					disturb(FALSE);
			}

			/* Scan all objects in the grid */
			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Acquire object */
				o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD) continue;

				/*
				 * Skip statues, to avoid extreme silliness
				 * like a novice rogue with pockets full of them.
				 */
				if (o_ptr->tval == TV_STATUE) continue;

				/* Take or Kill objects on the floor */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM | RF2_KILL_ITEM)) &&
					 (!is_pet(m_ptr) || p_ptr->pet_pickup_items))
				{
					u32b f1, f2, f3;

					u32b flg3 = 0L;

					char o_name[80];

					/* Extract some flags */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Acquire the object name */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0x04);

					/* React to objects that hurt the monster */
					if (f1 & TR1_KILL_DRAGON) flg3 |= (RF3_DRAGON);
					if (f1 & TR1_SLAY_DRAGON) flg3 |= (RF3_DRAGON);
					if (f1 & TR1_SLAY_TROLL)  flg3 |= (RF3_TROLL);
					if (f1 & TR1_SLAY_GIANT)  flg3 |= (RF3_GIANT);
					if (f1 & TR1_SLAY_ORC)    flg3 |= (RF3_ORC);
					if (f1 & TR1_SLAY_DEMON)  flg3 |= (RF3_DEMON);
					if (f1 & TR1_SLAY_UNDEAD) flg3 |= (RF3_UNDEAD);
					if (f1 & TR1_SLAY_ANIMAL) flg3 |= (RF3_ANIMAL);
					if (f1 & TR1_SLAY_EVIL)   flg3 |= (RF3_EVIL);

					/* The object cannot be picked up by the monster */
					if ((o_ptr->flags3 & TR3_INSTA_ART) ||
						 (r_ptr->flags3 & flg3))
					{
						/* Only give a message for "take_item" */
						if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) && (r_ptr->flags2 & (RF2_STUPID)))
						{
							/* Take note */
							did_take_item = TRUE;

							/* Describe observable situations */
							if (m_ptr->ml && player_can_see_bold(ny, nx))
							{
								/* Dump a message */
								msg_format("%^s tries to pick up %s, but fails.",
									m_name, o_name);
							}
						}
					}

					/* Pick up the item */
					else if (r_ptr->flags2 & RF2_TAKE_ITEM)
					{
						/* Take note */
						did_take_item = TRUE;

						/* Describe observable situations */
						if (player_can_see_bold(ny, nx))
						{
							/* Dump a message */
							msg_format("%^s picks up %s.", m_name, o_name);
						}

						/* Excise the object */
						excise_object_idx(this_o_idx);

						/* Forget mark */
						o_ptr->marked = FALSE;

						/* Forget location */
						o_ptr->iy = o_ptr->ix = 0;

						/* Memorize monster */
						o_ptr->held_m_idx = m_idx;

						/* Build a stack */
						o_ptr->next_o_idx = m_ptr->hold_o_idx;

						/* Carry object */
						m_ptr->hold_o_idx = this_o_idx;
					}

					/* Destroy the item if not a pet */
					else if (!is_pet(m_ptr))
					{
						/* Take note */
						did_kill_item = TRUE;

						/* Describe observable situations */
						if (player_can_see_bold(ny, nx))
						{
							/* Dump a message */
							msg_format("%^s destroys %s.", m_name, o_name);
						}

						/* Delete the object */
						delete_object_idx(this_o_idx);
					}
				}
			}
		}

		/* Stop when done */
		if (do_turn) break;
	}


	/* If we haven't done anything, try casting a spell again */
	if (!do_turn && !do_move && !m_ptr->monfear && !stupid_monsters &&
		!make_attack_spell(m_idx))
	{
		/* Cast spell */
		if (make_attack_spell(m_idx)) return;
	}


	/* Notice changes in view */
	if (do_view)
	{
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
	}

	/* Notice changes in view */
	if (do_move && (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2)))
	{
		/* Update some things */
		p_ptr->update |= (PU_MON_LITE);
	}

	/* Learn things from observable monster */
	if (m_ptr->ml)
	{
		/* Monster opened a door */
		if (did_open_door) r_ptr->r_flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) r_ptr->r_flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) r_ptr->r_flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) r_ptr->r_flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		if (did_move_body) r_ptr->r_flags2 |= (RF2_MOVE_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) r_ptr->r_flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) r_ptr->r_flags2 |= (RF2_KILL_WALL);
	}


	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear)
	{
		/* No longer afraid */
		m_ptr->monfear = 0;

		/* Message if seen */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Acquire the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			msg_format("%^s turns to fight!", m_name);

			chg_virtue(V_COMPASSION, -1);
		}

		/* XXX XXX XXX Actually do something now (?) */
	}
}




/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Note that monsters can never move in the monster array (except when the
 * "compact_monsters()" function is called by "dungeon()" or "save_player()").
 *
 * This function is responsible for at least half of the processor time
 * on a normal system with a "normal" amount of monsters and a player doing
 * normal things.
 *
 * When the player is resting, virtually 90% of the processor time is spent
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "lite_spot()",
 * especially when the player is running.
 *
 * Note the special "MFLAG_BORN" flag, which allows us to ignore "fresh"
 * monsters while they are still being "born".  A monster is "fresh" only
 * during the turn in which it is created, and we use the "hack_m_idx" to
 * determine if the monster is yet to be processed during the current turn.
 *
 * Note the special "MFLAG_NICE" flag, which allows the player to get one
 * move before any "nasty" monsters get to use their spell attacks.
 *
 * Note that when the "knowledge" about the currently tracked monster
 * changes (flags, attacks, spells), we induce a redraw of the monster
 * recall window.
 */
void process_monsters(int min_energy)
{
	int             i;
	int             fx, fy;

	bool            test;

	monster_type    *m_ptr;
	monster_race    *r_ptr;

	int             old_monster_race_idx;

	u32b    old_r_flags1 = 0L;
	u32b    old_r_flags2 = 0L;
	u32b    old_r_flags3 = 0L;
	u32b    old_r_flags4 = 0L;
	u32b    old_r_flags5 = 0L;
	u32b    old_r_flags6 = 0L;

	byte    old_r_blows0 = 0;
	byte    old_r_blows1 = 0;
	byte    old_r_blows2 = 0;
	byte    old_r_blows3 = 0;

	byte    old_r_cast_inate = 0;
	byte    old_r_cast_spell = 0;

	int old_total_friends = total_friends;
	s32b old_friend_align = friend_align;

	cave_type *c_ptr;

	/* Clear some variables */
	total_friends = 0;
	total_friend_levels = 0;
	friend_align = 0;

	/* Clear monster fighting indicator */
	p_ptr->mon_fight = FALSE;

	/* Memorize old race */
	old_monster_race_idx = p_ptr->monster_race_idx;

	/* Acquire knowledge */
	if (p_ptr->monster_race_idx)
	{
		/* Acquire current monster */
		r_ptr = &r_info[p_ptr->monster_race_idx];

		/* Memorize flags */
		old_r_flags1 = r_ptr->r_flags1;
		old_r_flags2 = r_ptr->r_flags2;
		old_r_flags3 = r_ptr->r_flags3;
		old_r_flags4 = r_ptr->r_flags4;
		old_r_flags5 = r_ptr->r_flags5;
		old_r_flags6 = r_ptr->r_flags6;

		/* Memorize blows */
		old_r_blows0 = r_ptr->r_blows[0];
		old_r_blows1 = r_ptr->r_blows[1];
		old_r_blows2 = r_ptr->r_blows[2];
		old_r_blows3 = r_ptr->r_blows[3];

		/* Memorize castings */
		old_r_cast_inate = r_ptr->r_cast_inate;
		old_r_cast_spell = r_ptr->r_cast_spell;
	}


	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Calculate "upkeep" for pets */
		if (is_pet(m_ptr))
		{
			total_friends++;
			total_friend_levels += r_ptr->level;

			/* Determine pet alignment */
			if (r_ptr->flags3 & RF3_GOOD)
			{
				friend_align += r_ptr->level;
			}
			else if (r_ptr->flags3 & RF3_EVIL)
			{
				friend_align -= r_ptr->level;
			}
		}

		/* Handle "fresh" monsters */
		if (m_ptr->mflag & MFLAG_BORN)
		{
			/* No longer "fresh" */
			m_ptr->mflag &= ~(MFLAG_BORN);

			/* Skip */
			continue;
		}

		/* Not enough energy to move */
		if (m_ptr->energy < min_energy) continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;


		/* Hack -- Require proximity */
		if (m_ptr->cdis >= 100) continue;


		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		c_ptr = area(fy, fx);

		/* Assume no move */
		test = FALSE;

		/* Allow more activity with pets around */
		if (old_total_friends)
		{
			test = TRUE;
		}

		/* Handle "sensing radius" */
		if (m_ptr->cdis <= r_ptr->aaf)
		{
			/* We can "sense" the player */
			test = TRUE;
		}

		/* Handle "sight" and "aggravation" */
		else if ((m_ptr->cdis <= MAX_SIGHT) &&
			(player_has_los_grid(c_ptr) || p_ptr->aggravate))
		{
			/* We can "see" or "feel" the player */
			test = TRUE;
		}

		/* 
		 * Hack -- Monsters can "smell" the player from far away
		 * Note that most monsters have "aaf" of "20" or so
		 */
		else if ((area(p_ptr->py, p_ptr->px)->when == c_ptr->when) &&
			(c_ptr->cost < MONSTER_FLOW_DEPTH) &&
			(c_ptr->cost < r_ptr->aaf))
		{
			/* We can "smell" the player */
			test = TRUE;
		}

		/* Do nothing */
		if (!test) continue;

		/* Save global index */
		hack_m_idx = i;

		/* Process the monster */
		process_monster(i);

		/* Hack -- notice death or departure */
		if (!p_ptr->playing || p_ptr->is_dead) break;

		/* Notice leaving */
		if (p_ptr->leaving) break;
	}

	/* Reset global index */
	hack_m_idx = 0;


	/* Tracking a monster race (the same one we were before) */
	if (p_ptr->monster_race_idx && (p_ptr->monster_race_idx == old_monster_race_idx))
	{
		/* Acquire monster race */
		r_ptr = &r_info[p_ptr->monster_race_idx];

		/* Check for knowledge change */
		if ((old_r_flags1 != r_ptr->r_flags1) ||
			(old_r_flags2 != r_ptr->r_flags2) ||
			(old_r_flags3 != r_ptr->r_flags3) ||
			(old_r_flags4 != r_ptr->r_flags4) ||
			(old_r_flags5 != r_ptr->r_flags5) ||
			(old_r_flags6 != r_ptr->r_flags6) ||
			(old_r_blows0 != r_ptr->r_blows[0]) ||
			(old_r_blows1 != r_ptr->r_blows[1]) ||
			(old_r_blows2 != r_ptr->r_blows[2]) ||
			(old_r_blows3 != r_ptr->r_blows[3]) ||
			(old_r_cast_inate != r_ptr->r_cast_inate) ||
			(old_r_cast_spell != r_ptr->r_cast_spell))
		{
			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);
		}
	}

	if (old_friend_align != friend_align) p_ptr->update |= (PU_BONUS);
}
