/* File: generate.c */

/* Purpose: Dungeon generation */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 * Note that Level generation is *not* an important bottleneck,
 * though it can be annoyingly slow on older machines...  Thus
 * we emphasize "simplicity" and "correctness" over "speed".
 *
 * This entire file is only needed for generating levels.
 * This may allow smart compilers to only load it when needed.
 *
 * Consider the "v_info.txt" file for vault generation.
 *
 * In this file, we use the "special" granite and perma-wall sub-types,
 * where "basic" is normal, "inner" is inside a room, "outer" is the
 * outer wall of a room, and "solid" is the outer wall of the dungeon
 * or any walls that may not be pierced by corridors.  Thus the only
 * wall type that may be pierced by a corridor is the "outer granite"
 * type.  The "basic granite" type yields the "actual" corridors.
 *
 * Note that we use the special "solid" granite wall type to prevent
 * multiple corridors from piercing a wall in two adjacent locations,
 * which would be messy, and we use the special "outer" granite wall
 * to indicate which walls "surround" rooms, and may thus be "pierced"
 * by corridors entering or leaving the room.
 *
 * Note that a tunnel which attempts to leave a room near the "edge"
 * of the dungeon in a direction toward that edge will cause "silly"
 * wall piercings, but will have no permanently incorrect effects,
 * as long as the tunnel can *eventually* exit from another side.
 * And note that the wall may not come back into the room by the
 * hole it left through, so it must bend to the left or right and
 * then optionally re-enter the room (at least 2 grids away).  This
 * is not a problem since every room that is large enough to block
 * the passage of tunnels is also large enough to allow the tunnel
 * to pierce the room itself several times.
 *
 * Note that no two corridors may enter a room through adjacent grids,
 * they must either share an entryway or else use entryways at least
 * two grids apart.  This prevents "large" (or "silly") doorways.
 *
 * To create rooms in the dungeon, we first divide the dungeon up
 * into "blocks" of 11x11 grids each, and require that all rooms
 * occupy a rectangular group of blocks.  As long as each room type
 * reserves a sufficient number of blocks, the room building routines
 * will not need to check bounds.  Note that most of the normal rooms
 * actually only use 23x11 grids, and so reserve 33x11 grids.
 *
 * Note that the use of 11x11 blocks (instead of the old 33x11 blocks)
 * allows more variability in the horizontal placement of rooms, and
 * at the same time has the disadvantage that some rooms (two thirds
 * of the normal rooms) may be "split" by panel boundaries.  This can
 * induce a situation where a player is in a room and part of the room
 * is off the screen.  It may be annoying enough to go back to 33x11
 * blocks to prevent this visual situation.
 *
 * Note that the dungeon generation routines are much different (2.7.5)
 * and perhaps "DUN_ROOMS" should be less than 50.
 *
 * XXX XXX XXX Note that it is possible to create a room which is only
 * connected to itself, because the "tunnel generation" code allows a
 * tunnel to leave a room, wander around, and then re-enter the room.
 *
 * XXX XXX XXX Note that it is possible to create a set of rooms which
 * are only connected to other rooms in that set, since there is nothing
 * explicit in the code to prevent this from happening.  But this is less
 * likely than the "isolated room" problem, because each room attempts to
 * connect to another room, in a giant cycle, thus requiring at least two
 * bizarre occurances to create an isolated section of the dungeon.
 *
 * Note that (2.7.9) monster pits have been split into monster "nests"
 * and monster "pits".  The "nests" have a collection of monsters of a
 * given type strewn randomly around the room (jelly, animal, or undead),
 * while the "pits" have a collection of monsters of a given type placed
 * around the room in an organized manner (orc, troll, giant, dragon, or
 * demon).  Note that both "nests" and "pits" are now "level dependant",
 * and both make 16 "expensive" calls to the "get_mon_num()" function.
 *
 * Note that the cave grid flags changed in a rather drastic manner
 * for Angband 2.8.0 (and 2.7.9+), in particular, dungeon terrain
 * features, such as doors and stairs and traps and rubble and walls,
 * are all handled as a set of 64 possible "terrain features", and
 * not as "fake" objects (440-479) as in pre-2.8.0 versions.
 *
 * The 64 new "dungeon features" will also be used for "visual display"
 * but we must be careful not to allow, for example, the user to display
 * hidden traps in a different way from floors, or secret doors in a way
 * different from granite walls, or even permanent granite in a different
 * way from granite.  XXX XXX XXX
 */

#include "angband.h"
#include "generate.h"
#include "grid.h"
#include "rooms.h"
#include "streams.h"

static int dun_rooms;

int dun_tun_rnd;
int dun_tun_chg;
int dun_tun_con;
int dun_tun_pen;
int dun_tun_jct;


/*
 * Dungeon generation data -- see "cave_gen()"
 */
dun_data *dun;

static int dun_rating;
static int is_special;

/*
 * Check if the current level is a "dead" quest level.
 */
static bool is_dead(void)
{
	return (p_ptr->depth < active_level(current_quest));
}


static void clean_duplicates(void)
{
	int x,y;
	cave_type *c_ptr;

	/* No monster under the player */
	delete_monster(p_ptr->px, p_ptr->py);
	delete_field(p_ptr->px, p_ptr->py);
	delete_object(p_ptr->px, p_ptr->py);
	
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
		{
			c_ptr = cave_p(x,y);
			
			if (c_ptr->feat == FEAT_MORE || c_ptr->feat == FEAT_LESS ||
				c_ptr->feat == FEAT_OPEN || c_ptr->feat == FEAT_BROKEN ||
				c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET)
			{
				delete_object(x,y);
				delete_field(x,y);
			}

			if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET)
			{
				delete_monster(x,y);
			}
		
		}
	}
}

/*
 * Try really really hard to not fail
 * in picking a player spot.
 */
static void new_player_spot_panic(void)
{
	int i, j;


	/* First, find ANY empty, non-icky grid */
	for (i = p_ptr->min_hgt; i < p_ptr->max_hgt; i++)
	{
		for (j = p_ptr->min_wid; j < p_ptr->max_wid; j++)
		{
			if (!cave_empty_grid(cave_p(j,i))) continue;
			if (cave_p(j,i)->info & CAVE_ICKY) continue;

			p_ptr->py = i;
			p_ptr->px = j;
			Term_move_player();
		}
	}

	/* If that fails, find a non-icky, non-monster-occupied grid and make it a floor */
	for (i = p_ptr->min_hgt; i < p_ptr->max_hgt; i++)
	{
		for (j = p_ptr->min_wid; j < p_ptr->max_wid; j++)
		{
			if (cave_p(j,i)->info & CAVE_ICKY) continue;
			if (cave_perma_grid(cave_p(j,i))) continue;
			if (cave_p(j,i)->m_idx) continue;

			delete_field_location(cave_p(j,i));
			set_feat_grid(cave_p(j,i), dun->feat_floor);

			p_ptr->py = i;
			p_ptr->px = j;
			Term_move_player();
		}
	}

	/* Yuck!  Just pick a random spot and make it a floor grid. */
	i = rand_range(p_ptr->min_hgt, p_ptr->max_hgt - 1);
	j = rand_range(p_ptr->min_wid, p_ptr->max_wid - 1);

	delete_field_location(cave_p(j,i));
	set_feat_grid(cave_p(j,i), dun->feat_floor);

	p_ptr->py = i;
	p_ptr->px = j;
	Term_move_player();
}

/*
 * Handle "boss" generation:
 *   BOSS fixed quest: replace strongest, farthest monster with boss.
 *   KILL fixed quest: same, and also delete all other monsters.
 */
static void place_boss(void)
{
	bool kill = (current_quest->type == QUEST_TYPE_FIXED_KILL);
	int best_x = 20, best_y = 20;
	int best_dist = 0;
	int best_depth = 0;
	int r_idx;
	int boss_r_idx;
	monster_race * r_ptr;
	cave_type * c_ptr;
	int x, y, i;

	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			c_ptr = cave_p(x,y);
			if (!c_ptr->m_idx) continue;

			r_ptr = &r_info[m_list[c_ptr->m_idx].r_idx];

			if (r_ptr->level > best_depth ||
				(r_ptr->level == best_depth &&
					 distance(p_ptr->px, p_ptr->py, x, y) > best_dist))
			{
				best_dist = distance(p_ptr->px, p_ptr->py, x, y);
				best_depth = r_ptr->level;
				best_x = x;
				best_y = y;
			}

			/* on "kill" levels, remove the monster */
			if (kill)  delete_monster(x, y);
		}
	}

	if (!best_dist)
	{
		/* No monsters?  Ok, try not to crash. */
		scatter(&best_x, &best_y, p_ptr->px, p_ptr->py, 3);

		for (i = 0; i < 10; i++)
		{
			x = rand_range (p_ptr->min_wid, p_ptr->max_wid - 1);
			y = rand_range (p_ptr->min_hgt, p_ptr->max_hgt - 1);

			c_ptr = cave_p(x,y);

			if (!cave_naked_grid(c_ptr))
			{
				i--;
				continue;
			}

			if (distance(p_ptr->px, p_ptr->py, x, y) > best_dist)
			{
				best_x = x;
				best_y = y;
				best_dist = distance(p_ptr->px, p_ptr->py, x, y);
			}

		}
	}
	else if (!kill)
	{
		/* Have to delete this monster before we can replace with the boss */
		delete_monster(best_x, best_y);
	}

	if (current_quest->type == QUEST_TYPE_FIXED_KILL)
		r_idx = current_quest->data.fix.data.kill.r_idx;
	else
		r_idx = current_quest->data.fix.data.boss.r_idx;

	/* Should have location now, place the boss */
	place_monster_one(best_x, best_y, r_idx, TRUE, FALSE, FALSE);
}

/*
 * Check if the current level is "dead" and if so, remove all
 * monsters, objects, and fields.
 */
static void dead_level(void)
{
	int x, y;
	cave_type *c_ptr;
	byte floor = dungeon()->floor;

	if (!is_dead()) return;

	/* Must be dead, delete everything */
	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			delete_monster(x,y);
			delete_object(x,y);
			delete_field(x,y);

			/* Get rid of rubble too */
			c_ptr = cave_p(x,y);
			if (c_ptr->feat == FEAT_RUBBLE)
				c_ptr->feat = floor;

			/* And buried treasure. */
			if (c_ptr->feat == FEAT_MAGMA_K)
				c_ptr->feat = FEAT_MAGMA;

			if (c_ptr->feat == FEAT_QUARTZ_K)
				c_ptr->feat = FEAT_QUARTZ;
		}
	}
}

/*
 * Get rid of objects to avoid easy farming.
 *
 * Decimate traps too, to avoid experience farming.
 */
static void reduce_objects(void)
{
	int x, y;
	bool die;

	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			die = (current_quest->type == QUEST_TYPE_FIXED_KILL ? TRUE : !one_in_(10));

			if (cave_p(x,y)->o_idx && (distance(x, y, p_ptr->px, p_ptr->py) < 20 || die))
				delete_object(x,y);

			if (cave_p(x,y)->fld_idx)
			{
				/* Hack: manually remove all trap doors */
				field_type *f_ptr;

				FLD_ITT_START (cave_p(x,y)->fld_idx, f_ptr)
				{
					if (f_ptr->t_idx == FT_TRAP_DOOR)
					{
						die = TRUE;
					}
				}
				FLD_ITT_END;

				if (die) delete_field(x,y);
			}
		}
	}
}


/* Control the rating */
void inc_rating(int delta_rating)
{
	dun_rating += delta_rating;
}

/* Set the special feeling */
void set_special(void)
{
	is_special = TRUE;
}

static byte extract_feeling(void)
{
	/* Hack -- no feeling in the town */
	if (!p_ptr->depth) return 0;

	/* Hack -- Have a special feeling sometimes */
	if (is_special && !preserve_mode) return 1;

	if (dun_rating > 100) return 2;
	if (dun_rating > 80) return 3;
	if (dun_rating > 60) return 4;
	if (dun_rating > 40) return 5;
	if (dun_rating > 30) return 6;
	if (dun_rating > 20) return 7;
	if (dun_rating > 10) return 8;
	if (dun_rating > 0) return 9;

	if ((turn - old_turn) > TOWN_HALF_DAY)
		chg_virtue(V_PATIENCE, 1);

	return 10;
}

/*
 * Places some staircases near walls
 */
static bool alloc_stairs(int feat, int num, int walls)
{
	int y, x, i, j, flag;
	cave_type *c_ptr;
	bool q_stop = FALSE;

	if (feat == FEAT_LESS)
	{
		/* No up stairs in town or in ironman mode */
		if (ironman_downward || !p_ptr->depth) return TRUE;
	}
	else if (feat == FEAT_MORE)
	{
		/* No downstairs on winner quest levels */
		if (is_special_level(p_ptr->depth)) return TRUE;

		/* No downstairs at the bottom */
		if (p_ptr->depth >= dungeon()->max_level && !dungeon_abyss) return TRUE;

		/* On an active quest level, no down stairs */
		/* Note that we can't just return, as that would alter later random
		   choices and wreck consistency. */
		if (current_quest)
		{
			if (current_quest->type == QUEST_TYPE_FIXED_DEN)
			{
				if (p_ptr->depth - current_quest->data.fix.data.den.cleared >=
						dungeon()->min_level)
					q_stop = TRUE;
			}
			else if (current_quest->type == QUEST_TYPE_FIXED_CLEAROUT)
			{
				if (p_ptr->depth - current_quest->data.fix.data.clearout.cleared >=
						dungeon()->min_level)
					q_stop = TRUE;
			}
			else if (current_quest->type == QUEST_TYPE_FIXED_BOSS ||
					 current_quest->type == QUEST_TYPE_FIXED_KILL)
				 q_stop = TRUE;
		}
	}

	/* Place "num" stairs */
	for (i = 0; i < num; i++)
	{
		/* Place some stairs */
		for (flag = FALSE; !flag;)
		{
			/* Try several times, then decrease "walls" */
			for (j = 0; !flag && j <= 10000; j++)
			{
				/* Pick a random grid */
				y = rand_range(p_ptr->min_hgt + 1, p_ptr->max_hgt - 2);
				x = rand_range(p_ptr->min_wid + 1, p_ptr->max_wid - 2);

				/* Access the grid */
				c_ptr = cave_p(x, y);

				/* Require "naked" floor grid */
				if (!cave_naked_grid(c_ptr)) continue;

				/* Require a certain number of adjacent walls */
				if (next_to_walls(x, y) < walls) continue;

				/* Clear previous contents, add stairs */
				if (!q_stop || feat == FEAT_LESS) set_feat_grid(c_ptr, feat);

				/* All done */
				flag = TRUE;
			}

			/* If cannot find a blank spot - exit */
			if (!walls)
			{
				/* Placed at least one. */
				if (i > 0) return TRUE;

				/* Couldn't place any stairs */
				return FALSE;
			}

			/* Require fewer walls */
			walls--;
		}
	}

	/* Done */
	return TRUE;
}

/*
 * Try to make sure a symmetric dungeon is connected.
 */
static void apply_symmetry_connect(dun_type * d_ptr, bool horiz)
{
	int x, y, i;
	int yfirst, ylast;
	int xfirst, xlast;
	cave_type * c_ptr;

	xlast = ylast = 0;

	/* Check the horizontal "seam" first */

	if (d_ptr->flags & (DF_SYM_4 | DF_SYM_R4) || horiz)
	{
		x = (p_ptr->min_wid + p_ptr->max_wid)/2;

		/* Look next to the seam for the first and last connection to it. */
		yfirst = -1;

		for (y = 0; y < p_ptr->max_hgt; y++)
		{
			c_ptr = cave_p(x+1,y);

			if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
				!(c_ptr->info & FF_BLOCK))
			{
				if (yfirst == -1)  yfirst = y;
				ylast = y;
			}

			/* Paranoia: check both sides.  Maybe not paranoia: could be an issue of width parity? */
			c_ptr = cave_p(x-1,y);

			if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
				!(c_ptr->info & FF_BLOCK))
			{
				if (yfirst == -1)  yfirst = y;
				ylast = y;
			}
		}

		/* If there were no adjacent open areas, draw a *perpendicular* path. */
		if (yfirst == -1)
		{
			bool ok = FALSE;
			y = (p_ptr->min_hgt + p_ptr->max_hgt)/2;

			for (i = 0; !ok; i++)
			{
				if (x+i >= p_ptr->max_wid) break;

				c_ptr = cave_p(x+i, y);

				if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
					!(c_ptr->info & FF_BLOCK))
				{
					ok = TRUE;
				}
				else
				{
					set_feat_bold(x+i, y, dun->feat_floor);
				}

				if (x-i < 0) break;

				c_ptr = cave_p(x-i, y);

				if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
					!(c_ptr->info & FF_BLOCK))
				{
					ok = TRUE;
				}
				else
				{
					set_feat_bold(x-i, y, dun->feat_floor);
				}
			}
		}
		/* Otherwise, open a path along the seam from yfirst to ylast. */
		else
		{
			for (y = yfirst; y <= ylast; y++)
			{
				c_ptr = cave_p(x,y);

				if (c_ptr->feat != FEAT_CLOSED && c_ptr->feat != FEAT_SECRET &&
					(c_ptr->info & FF_BLOCK))
				{
					set_feat_bold(x,y, dun->feat_floor);
				}
			}
		}
	}

	/* Now the vertical seam */
	if (d_ptr->flags & (DF_SYM_4 | DF_SYM_R4) || !horiz)
	{
		y = (p_ptr->min_hgt + p_ptr->max_hgt)/2;

		/* Look next to the seam for the first and last connection to it. */
		xfirst = -1;

		for (x = 0; x < p_ptr->max_wid; x++)
		{
			c_ptr = cave_p(x,y+1);

			if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
				!(c_ptr->info & FF_BLOCK))
			{
				if (xfirst == -1)  xfirst = x;
				xlast = x;
			}

			/* Paranoia: check both sides.  Maybe not paranoia: could be an issue of width parity? */
			c_ptr = cave_p(x,y-1);

			if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
				!(c_ptr->info & FF_BLOCK))
			{
				if (xfirst == -1)  xfirst = x;
				xlast = x;
			}
		}

		/* If there were no adjacent open areas, draw a *perpendicular* path. */
		if (xfirst == -1)
		{
			bool ok = FALSE;
			x = (p_ptr->min_wid + p_ptr->max_wid)/2;

			for (i = 0; !ok; i++)
			{
				if (y+i >= p_ptr->max_hgt) break;

				c_ptr = cave_p(x, y+i);

				if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
					!(c_ptr->info & FF_BLOCK))
				{
					ok = TRUE;
				}
				else
				{
					set_feat_bold(x, y+i, dun->feat_floor);
				}

				if (y-i < 0) break;

				c_ptr = cave_p(x, y-i);

				if (c_ptr->feat == FEAT_CLOSED || c_ptr->feat == FEAT_SECRET ||
					!(c_ptr->info & FF_BLOCK))
				{
					ok = TRUE;
				}
				else
				{
					set_feat_bold(x, y-i, dun->feat_floor);
				}
			}
		}
		/* Otherwise, open a path along the seam from yfirst to ylast. */
		else
		{
			for (x = xfirst; x <= xlast; x++)
			{
				c_ptr = cave_p(x,y);

				if (c_ptr->feat != FEAT_CLOSED && c_ptr->feat != FEAT_SECRET &&
					(c_ptr->info & FF_BLOCK))
				{
					set_feat_bold(x,y, dun->feat_floor);
				}
			}
		}
	}
}

/*
 * Make the current dungeon symmetric.
 */
static void apply_symmetry(dun_type * d_ptr, bool horiz)
{
	int x, y, x2, y2, i;
	cave_type * c_ptr, * c2_ptr;
	monster_race * r_ptr;
	monster_type * m_ptr;
	bool stairs_up = FALSE;
	bool stairs_down = FALSE;

	/* We should never have multiple symmetries, but just in case, we will
	   prioritize: SYM_2 then SYM_R2, then SYM_4 then SYM_R4 */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2))
	{
		for (x = p_ptr->min_wid; x < (horiz ? (p_ptr->min_wid + p_ptr->max_wid)/2 :
				p_ptr->max_wid); x++)
		{
			for (y = p_ptr->min_hgt; y < (horiz ? p_ptr->max_hgt :
				(p_ptr->min_hgt + p_ptr->max_hgt)/2); y++)
			{
				c_ptr = cave_p (x,y);

				if (c_ptr->feat == FEAT_MORE) stairs_down = TRUE;
				if (c_ptr->feat == FEAT_LESS) stairs_up = TRUE;

				/* Determine coords of symmetric grid */
				if (horiz && d_ptr->flags & DF_SYM_2)
				{
					x2 = p_ptr->max_wid - x - 1;
					y2 = y;
				}
				else if (!horiz & d_ptr->flags & DF_SYM_2)
				{
					x2 = x;
					y2 = p_ptr->max_hgt - y - 1;
				}
				else
				{
					x2 = p_ptr->max_wid - x - 1;
					y2 = p_ptr->max_hgt - y - 1;
				}

				c2_ptr = cave_p (x2, y2);

				delete_monster(x2, y2);
				delete_object(x2, y2);
				delete_field(x2, y2);

				c2_ptr->info = c_ptr->info;
				c2_ptr->feat = c_ptr->feat;

				/* Copy monster, if not unique */
				m_ptr = &m_list[c_ptr->m_idx];
				r_ptr = &r_info[m_ptr->r_idx];
				if (!FLAG(r_ptr, RF_UNIQUE))
				{
					place_monster_one(x2, y2, m_ptr->r_idx, m_ptr->csleep, FALSE, FALSE);
				}

				/* Place an object if there was one before */
				if (c_ptr->o_idx)
					place_object(x2, y2, FALSE, FALSE, 0);

				/* Copy field */
				if (c_ptr->fld_idx)
				{
					switch (t_info[fld_list[c_ptr->fld_idx].t_idx].type)
					{
						case FT_JAM_DOOR:
						case FT_LOCK_DOOR:
							/* Hack: ignore. */
							break;
						default:
							/* Assume trap. */
							place_trap(x2, y2);
							break;
					}
				}
			}
		}
	}
	else if (d_ptr->flags & (DF_SYM_4 | DF_SYM_R4))
	{
		for (x = p_ptr->min_wid; x < (p_ptr->min_wid + p_ptr->max_wid)/2; x++)
		{
			for (y = p_ptr->min_hgt; y < (p_ptr->min_hgt + p_ptr->max_hgt)/2; y++)
			{
				c_ptr = cave_p (x,y);

				if (c_ptr->feat == FEAT_MORE) stairs_down = TRUE;
				if (c_ptr->feat == FEAT_LESS) stairs_up = TRUE;

				m_ptr = &m_list[c_ptr->m_idx];

				for (i = 0; i < 3; i++)
				{
					/* Determine coords of symmetric grid */
					x2 = (i == 0 ? x : p_ptr->max_wid-1 - x);
					y2 = (i == 1 ? y : p_ptr->max_hgt-1 - y);

					delete_monster(x2, y2);
					delete_object(x2, y2);
					delete_field(x2, y2);

					c2_ptr = cave_p (x2, y2);

					c2_ptr->info = c_ptr->info;
					c2_ptr->feat = c_ptr->feat;

					/* Copy monster, if not unique */
					m_ptr = &m_list[c_ptr->m_idx];
					r_ptr = &r_info[m_ptr->r_idx];
					if (!FLAG(r_ptr, RF_UNIQUE))
					{
						place_monster_one(x2, y2, m_ptr->r_idx, m_ptr->csleep, FALSE, FALSE);
					}

					/* Place an object if there was one before */
					if (c_ptr->o_idx)
						place_object(x2, y2, FALSE, FALSE, 0);

					/* Copy field */
					if (c_ptr->fld_idx)
					{
						switch (t_info[fld_list[c_ptr->fld_idx].t_idx].type)
						{
							case FT_JAM_DOOR:
							case FT_LOCK_DOOR:
								/* Hack: ignore. */
								break;
							default:
								/* Assume trap. */
								place_trap(x2, y2);
								break;
						}
					}
				}
			}
		}
	}

	/* No stairs down.  Oops? */
	if (!stairs_down)
	{
		/* Not unexpected if this is a non-dead quest level. */
		if (!current_quest || is_dead())
		{
			(void)alloc_stairs(FEAT_MORE, 1, 0);
		}
	}

	/* No stairs up.  Definitely oops. */
	if (!stairs_up)
		(void)alloc_stairs(FEAT_LESS, 1, 0);
}

/*
 * Allocates some objects (using "place" and "type")
 */
static void alloc_object(int set, int typ, int num)
{
	int x = 0, y = 0, k;
	int dummy = 0;
	cave_type *c_ptr = NULL;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (dummy < SAFE_MAX_ATTEMPTS)
		{
			bool room;

			dummy++;

			/* Location */
			y = rand_range(p_ptr->min_hgt + 1, p_ptr->max_hgt - 2);
			x = rand_range(p_ptr->min_wid + 1, p_ptr->max_wid - 2);

			c_ptr = cave_p(x, y);

			/* Require "naked" floor grid */
			if (!cave_naked_grid(c_ptr)) continue;

			/* Check for "room" */
			room = (c_ptr->info & CAVE_ROOM) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* Traps cannot be placed on 'icky' grids (rivers/lakes) */
			if ((typ == ALLOC_TYP_TRAP) && (c_ptr->info & CAVE_ICKY)) continue;

			/* Accept it */
			break;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
				msgf("Warning! Could not place object!");
			}
			return;
		}


		/* Place something */
		switch (typ)
		{
			case ALLOC_TYP_RUBBLE:
			{
				set_feat_grid(c_ptr, FEAT_RUBBLE);
				break;
			}

			case ALLOC_TYP_TRAP:
			{
				place_trap(x, y);
				break;
			}

			case ALLOC_TYP_GOLD:
			{
				place_gold(x, y);
				break;
			}

			case ALLOC_TYP_OBJECT:
			{
				place_object(x, y, FALSE, FALSE, 0);
				break;
			}

			case ALLOC_TYP_INVIS:
			{
				/* Create invisible wall */
				set_feat_grid(c_ptr, dun->feat_floor);
				(void)place_field(x, y, FT_WALL_INVIS);
				break;
			}
		}
	}
}


/*
 * Count the number of "corridor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(x1, y1)"
 *
 * XXX XXX This routine currently only counts actual "empty floor"
 * grids which are not in rooms.  We might want to also count stairs,
 * open doors, closed doors, etc.
 */
static int next_to_corr(int x1, int y1)
{
	int i, y, x, k = 0;

	cave_type *c_ptr;

	/* Scan adjacent grids */
	for (i = 0; i < 4; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Access the grid */
		c_ptr = cave_p(x, y);

		/* Skip non clean floors */
		if (!cave_clean_grid(c_ptr)) continue;

		/* Skip grids inside rooms */
		if (c_ptr->info & (CAVE_ROOM)) continue;

		/* Count these grids */
		k++;
	}

	/* Return the number of corridors */
	return (k);
}


/*
 * Determine if the given location is "between" two walls,
 * and "next to" two corridor spaces.  XXX XXX XXX
 *
 * Assumes "in_bounds(x, y)"
 */
static bool possible_doorway(int x, int y)
{
	/* Count the adjacent corridors */
	if (next_to_corr(x, y) >= 2)
	{
		/* Check Vertical */
		if (cave_wall_grid(cave_p(x, y - 1)) &&
			cave_wall_grid(cave_p(x, y + 1)))
		{
			return (TRUE);
		}

		/* Check Horizontal */
		if (cave_wall_grid(cave_p(x - 1, y)) &&
			cave_wall_grid(cave_p(x + 1, y)))
		{
			return (TRUE);
		}
	}

	/* No doorway */
	return (FALSE);
}


/*
 * Places door at y, x position if at least 2 walls found
 */
static void try_door(int x, int y)
{
	cave_type *c_ptr;

	/* Paranoia */
	if (!in_bounds(x, y)) return;

	c_ptr = cave_p(x, y);

	/* Ignore walls */
	if (cave_wall_grid(c_ptr)) return;

	/* Ignore room grids */
	if (c_ptr->info & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
	if ((randint0(100) < dun_tun_jct) && possible_doorway(x, y))
	{
		/* Place a door */
		place_random_door(x, y);
	}
}

static void add_monsters(int count)
{
	int i, j;
	int delta_level, level, best_level;

	u16b best_r_idx;

	int min_depth;

	u16b r_idx;
	monster_race *r_ptr;

	int num  = 0;

	bool group;

	int x = 0, y = 0;

	cave_type *c_ptr;

	int target_rating = randint1(10) + p_ptr->depth * 2 / 3;

	/* Put some monsters in the dungeon */
	for (i = 0; i < count; i++)
	{
		/*
		 * Calculate the total levels of monster ood'ness to get
         * an appropriate level feeling.
		 *
		 * The more boring the dungeon is right now,
		 * the more out of depth to pick monsters.
		 *
		 * Each monster gets a fraction of the total levels we want
		 * that depends on the number of monsters left to generate.
		 */
		delta_level = (target_rating - dun_rating) / (count - i);
		if (delta_level < 0) delta_level = 0;
		if (delta_level > 10) delta_level = 10;

		/* Place the monster */
		if (alloc_monster(0, TRUE, delta_level)) num++;

		/* If we failed, set back the counter: should guarantee at least one monster. */
		else if (i+num < count) i--;
	}

	/* Sometimes have lots of monster of a given type */
	if (!current_quest && one_in_(10) && 5+randint1(30) <= p_ptr->depth)
	{
		level = p_ptr->depth + 6;

		best_r_idx = 1;
		best_level = 1;

		/* Get monster */
		for (j = 0; j < 100; j++)
		{
			min_depth = level + (level / 20) + 1;

			/*
			 * Random monster out of depth
			 */
			r_idx = get_mon_num(level);

			r_ptr = monst_race(r_idx);

			/* Save the index if the monster is deeper than current monster */
			if (!best_r_idx || (r_ptr->level > best_level))
			{
				best_r_idx = r_idx;
				best_level = r_ptr->level;
			}

			/* Accept monsters that are a few levels out of depth */
			if (best_level > min_depth) break;
		}

		r_ptr = monst_race(best_r_idx);

		/* Get the number of monsters */
		if (FLAG(r_ptr, RF_UNIQUE))
		{
			num = 1;
		}
		else if (FLAG(r_ptr, RF_UNIQUE_7))
		{
			num = randint1(r_ptr->max_num);
		}
		else
		{
			num = 5 + (s16b)randint0(level / 3 + 5) / r_ptr->rarity;
		}

		for (i = 0; i < num; i++)
		{
			/* Find an empty grid */
			while (TRUE)
			{
				y = rand_range(p_ptr->min_hgt + 1,
								p_ptr->max_hgt - 2);
				x = rand_range(p_ptr->min_wid + 1,
								p_ptr->max_wid - 2);

				/* Access the grid */
				c_ptr = area(x, y);

				if (!cave_naked_grid(c_ptr)) continue;

				if (distance(x, y, p_ptr->px, p_ptr->py) < 10)
					continue;
				else
					break;
			}

			if (FLAG(r_ptr, RF_FRIENDS))
				group = FALSE;
			else
				group = TRUE;

			/* Try to place the monster */
			place_monster_aux(x, y, best_r_idx, FALSE, group,
								  FALSE, FALSE, TRUE);
		}

		/*
		 * Make a great object somewhere in the dungeon to compensate
		 * (Hack - use location of last monster as target)
		 */
		place_object(x, y, TRUE, TRUE, best_level - p_ptr->depth);
	}
}




/*
 * Generate a new dungeon level
 *
 * Note that "dun_body" adds about 4000 bytes of memory to the stack.
 */
static bool cave_gen(dun_type *d_ptr)
{
	int i, j, k, y, x, y1, x1, p, n;
	int min_wid, max_wid, min_hgt, max_hgt;

	bool castle = d_ptr->flags & (DF_CASTLE | DF_PURE_CASTLE);
	bool horiz = one_in_(2);

	int max_vault_ok = 2;

	int treasure_chance = 0;

	cave_type *c_ptr;

	bool empty_level = FALSE;
	bool cavern = FALSE;

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;

	/* Initialize */
	min_hgt = p_ptr->min_hgt;
	min_wid = p_ptr->min_wid;
	max_hgt = p_ptr->max_hgt;
	max_wid = p_ptr->max_wid;

	if (p_ptr->max_hgt - p_ptr->min_hgt < 23) max_vault_ok--;
	if (p_ptr->max_wid - p_ptr->min_wid < 34) max_vault_ok--;

	/* Randomize the dungeon creation values */
	dun_rooms = rand_range(DUN_ROOMS_MIN, DUN_ROOMS_MAX);

	/* Apply limit */
	if (d_ptr->room_limit) dun_rooms = MIN(d_ptr->room_limit, dun_rooms);

	dun_tun_rnd = rand_range(DUN_TUN_RND_MIN, DUN_TUN_RND_MAX);
	dun_tun_chg = rand_range(DUN_TUN_CHG_MIN, DUN_TUN_CHG_MAX);
	dun_tun_con = rand_range(DUN_TUN_CON_MIN, DUN_TUN_CON_MAX);
	dun_tun_pen = rand_range(DUN_TUN_PEN_MIN, DUN_TUN_PEN_MAX);
	dun_tun_jct = rand_range(DUN_TUN_JCT_MIN, DUN_TUN_JCT_MAX);

	/*** Store in the terrain types ***/

	/* Get floor type */
	dun->feat_floor = d_ptr->floor;

	/* Get room types */
	dun->room_types = d_ptr->rooms;

	/* No vaults yet */
	dun->vaults = 0;

	/* Empty arena levels */
	if (d_ptr->freq_arena && one_in_(d_ptr->freq_arena))
	{
		empty_level = TRUE;

		if (cheat_room) msgf("City level.");
	}

	/* Start with wall type */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
		{
			if (empty_level)
			{
				set_feat_bold(x, y, dun->feat_floor);
			}
			else
			{
				/* Create default wall */
				/* This gets replaced with dun->wall later */
				set_feat_bold(x, y, FEAT_WALL_EXTRA);
			}
		}
	}

	/* Set dun->feat_shal_liquid and dun->feat_deep_liquid */
	if (d_ptr->river[0].rarity && d_ptr->river[1].rarity)
	{
		if (randint1((100 / d_ptr->river[0].rarity) + (100 / d_ptr->river[1].rarity)) <
			(100 / d_ptr->river[0].rarity))
		{
			dun->feat_shal_liquid = d_ptr->river[0].shal;
			dun->feat_deep_liquid = d_ptr->river[0].deep;
		} else {
			dun->feat_shal_liquid = d_ptr->river[1].shal;
			dun->feat_deep_liquid = d_ptr->river[1].deep;
		}
	} else if (d_ptr->river[0].rarity) {
		dun->feat_shal_liquid = d_ptr->river[0].shal;
		dun->feat_deep_liquid = d_ptr->river[0].deep;
	} else if (d_ptr->river[1].rarity) {
		dun->feat_shal_liquid = d_ptr->river[1].shal;
		dun->feat_deep_liquid = d_ptr->river[1].deep;
	} else {
		/* Hack: replace liquids with floor */
		dun->feat_shal_liquid = dun->feat_deep_liquid = d_ptr->floor;
	}

	/* Castle levels always are in caverns. */
	if (castle)
	{
		cavern = TRUE;

		/* make a large fractal cave in the middle of the dungeon */

		if (cheat_room)
			msgf("Cavern on level.");

		build_cavern();
	}

	/* Make a lake some of the time */
	else if (d_ptr->lake.rarity && !empty_level && one_in_(d_ptr->lake.rarity)
			&& p_ptr->max_wid > 32 && p_ptr->max_hgt > 32)
	{
		if (cheat_room) msgf("Lake on the level.");
		build_lake(d_ptr->lake.deep, d_ptr->lake.shal, d_ptr->wall, d_ptr->lake.size);
	}

	else if (d_ptr->freq_cavern && !empty_level && (p_ptr->depth >= MIN_CAVERN) &&
			 one_in_((3*d_ptr->freq_cavern) / (p_ptr->depth + DUN_CAV2)))
	{
		cavern = TRUE;

		/* make a large fractal cave in the middle of the dungeon */

		if (cheat_room)
			msgf("Cavern on level.");

		build_cavern();
	}

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = (p_ptr->max_hgt - p_ptr->min_hgt) / BLOCK_HGT;
	dun->col_rooms = (p_ptr->max_wid - p_ptr->min_hgt) / BLOCK_WID;

	/* Initialize the room table */
	for (y = 0; y < dun->row_rooms; y++)
	{
		for (x = 0; x < dun->col_rooms; x++)
		{
			dun->room_map[y][x] = FALSE;
		}
	}


	/* No "crowded" rooms yet */
	dun->crowded = 0;

	/* No rooms yet */
	dun->cent_n = 0;

	/* Build some rooms */
	if (!castle)
	{
		for (i = 0; i < dun_rooms; i++)
		{
			int count = 0;

			while (!room_build() && (count++ < 20))
				/* loop */ ;
		}
	}
	/* Create one big castle. */
	else
	{
		for (i = 0; i < 500; i++)
			if (castle_build(d_ptr->flags & DF_PURE_CASTLE)) break;
	}

	/* Make a hole in the dungeon roof sometimes at level 1 */
	if (p_ptr->depth == 1)
	{
		while (one_in_(DUN_MOS_DEN))
		{
			place_trees(rand_range(p_ptr->min_wid + 1, p_ptr->max_wid - 2),
						rand_range(p_ptr->min_hgt + 1, p_ptr->max_hgt - 2));
		}
	}

	/* Hack -- Add some rivers */
	if (d_ptr->river[0].rarity && one_in_(d_ptr->river[0].rarity) && (randint1(p_ptr->depth) > 5))
	{
		add_river(d_ptr->river[0].deep, d_ptr->river[0].shal, d_ptr->river[0].size);
	}

	if (d_ptr->river[1].rarity && one_in_(d_ptr->river[1].rarity) && (randint1(p_ptr->depth) > 5))
	{
		add_river(d_ptr->river[1].deep, d_ptr->river[1].shal, d_ptr->river[1].size);
	}

	/* Special boundary walls -- Top */
	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(x, p_ptr->min_hgt, d_ptr->perm_wall);
	}

	/* Special boundary walls -- Bottom */
	for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(x, p_ptr->max_hgt - 1, d_ptr->perm_wall);
	}

	/* Special boundary walls -- Left */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(p_ptr->min_wid, y, d_ptr->perm_wall);
	}

	/* Special boundary walls -- Right */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		/* Clear previous contents, add "solid" perma-wall */
		set_feat_bold(p_ptr->max_wid - 1, y, d_ptr->perm_wall);
	}

	/* Tunnels */
	if (!castle)
	{
		/* Hack -- Scramble the room order */
		for (i = 0; i < dun->cent_n; i++)
		{
			int pick1 = randint0(dun->cent_n);
			int pick2 = randint0(dun->cent_n);
			y1 = dun->cent[pick1].y;
			x1 = dun->cent[pick1].x;
			dun->cent[pick1].y = dun->cent[pick2].y;
			dun->cent[pick1].x = dun->cent[pick2].x;
			dun->cent[pick2].y = y1;
			dun->cent[pick2].x = x1;
		}

		/* Start with no tunnel doors */
		dun->door_n = 0;

		/* Hack -- connect the first room to the last room */
		y = dun->cent[dun->cent_n - 1].y;
		x = dun->cent[dun->cent_n - 1].x;

		/* Connect all the rooms together */
		for (i = 0; i < dun->cent_n; i++)
		{

			/* Reset the arrays */
			dun->tunn_n = 0;
			dun->wall_n = 0;

			/* Check tunnel frequency */
			if (d_ptr->freq_tunnel != 100 && randint1(100) >= d_ptr->freq_tunnel)
			{
				/* Remember the "previous" room */
				y = dun->cent[i].y;
				x = dun->cent[i].x;

				/* Don't tunnel. */
				continue;
			}


			/* Connect the room to the previous room */
#ifdef PILLAR_TUNNELS

			if ((randint1(20) > p_ptr->depth) && one_in_(4))
			{
				/* make catacomb-like tunnel */
				(void)build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y, 3, 30);
			}
			else if (randint1(p_ptr->depth) > 50)
#else
			if (randint1(p_ptr->depth) > 50)
#endif /* PILLAR_TUNNELS */
			{
				/* make cave-like tunnel */
				(void)build_tunnel2(dun->cent[i].x, dun->cent[i].y, x, y, 2, 2);
			}
			else
			{
				/* make normal tunnel */
				build_tunnel(dun->cent[i].x, dun->cent[i].y, x, y);
			}

			/* Turn the tunnel into corridor */
			for (j = 0; j < dun->tunn_n; j++)
			{
				/* Access the grid */
				y = dun->tunn[j].y;
				x = dun->tunn[j].x;

				/* Access the grid */
				c_ptr = cave_p(x, y);

				/* Deleting a locked or jammed door is problematical */
				delete_field_location(c_ptr);

				/* Clear previous contents if wall, add a floor */
				if (cave_wall_grid(c_ptr))
				{
					set_feat_grid(c_ptr, dun->feat_floor);
				}
			}

			/* Apply the piercings that we found */
			for (j = 0; j < dun->wall_n; j++)
			{
				/* Access the grid */
				y = dun->wall[j].y;
				x = dun->wall[j].x;

				/* Access the grid */
				c_ptr = cave_p(x, y);

				/* Deleting a locked or jammed door is problematical */
				delete_field_location(c_ptr);

				/* Clear previous contents, add up floor */
				set_feat_grid(c_ptr, dun->feat_floor);

				/* Occasional doorway */
				if (randint0(100) < dun_tun_pen)
				{
					/* Place a random door */
					place_random_door(x, y);
				}
			}

			/* Remember the "previous" room */
			y = dun->cent[i].y;
			x = dun->cent[i].x;
		}

		/* Place intersection doors  */
		for (i = 0; i < dun->door_n; i++)
		{
			/* Extract junction location */
			y = dun->door[i].y;
			x = dun->door[i].x;

			/* Try placing doors */
			try_door(x, y - 1);
			try_door(x, y + 1);
			try_door(x - 1, y);
			try_door(x + 1, y);
		}
	}

	/* Remove all doors, if there aren't supposed to be any */
	if (!d_ptr->freq_doors)
	{
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
			{
				c_ptr = cave_p(x,y);
				if (c_ptr->feat == FEAT_OPEN || c_ptr->feat == FEAT_BROKEN ||
					c_ptr->feat == FEAT_SECRET || c_ptr->feat == FEAT_CLOSED)
				{
					delete_field_location(c_ptr);
					set_feat_grid(c_ptr, dun->feat_floor);
				}
			}
		}
	}


	/* Add some streamers */
	for (i = 0; i < 2; i++)
	{
		treasure_chance = (d_ptr->vein[i].deep == FEAT_MAGMA ? DUN_STR_MC :
			(d_ptr->vein[i].deep == FEAT_QUARTZ ? DUN_STR_QC : 0));
		treasure_chance *= (d_ptr->freq_treasure / 100);
		for (j = 0; j < d_ptr->vein[i].number; j++)
		{
			build_streamer(d_ptr->vein[i].deep, treasure_chance, d_ptr->vein[i].size);
		}
	}

	/* Big hack: Modify the bounds on symmetric levels, so as to force allocated things not to get overwritten. */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2 | DF_SYM_4 | DF_SYM_R4))
	{
		min_hgt = p_ptr->min_hgt;
		min_wid = p_ptr->min_wid;
		max_hgt = p_ptr->max_hgt;
		max_wid = p_ptr->max_wid;

		if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2))
		{
			if (horiz)
			{
				p_ptr->max_wid = (p_ptr->min_wid + p_ptr->max_wid)/2;
			}
			else
			{
				p_ptr->max_hgt = (p_ptr->min_hgt + p_ptr->max_hgt)/2;
			}
		}
		else
		{
			p_ptr->max_wid = (p_ptr->min_wid + p_ptr->max_wid)/2;
			p_ptr->max_hgt = (p_ptr->min_hgt + p_ptr->max_hgt)/2;
		}
	}

	/* Place 2-4 down stairs near some walls */
	n = (rand_range(1,1000) * d_ptr->freq_stairs) / 100;
	n = (n / (1000/3))+2;

	(void)alloc_stairs(FEAT_MORE, MAX(n, 1), 3);

	/* Place 1-2 up stairs near some walls */
	n = (rand_range(1,1000) * d_ptr->freq_stairs) / 100;
	n = (n / (1000/2))+1;

	(void)alloc_stairs(FEAT_LESS, MAX(n, 1), 3);

	/* Place quest monsters in the dungeon */
	trigger_quest_create(QC_DUN_MONST, NULL);

	/* Place quest artifacts in the dungeon */
	trigger_quest_create(QC_DUN_ARTIFACT, NULL);

	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL;

	/* To make small levels a bit more playable */
	if (max_hgt < MAX_HGT || max_wid < MAX_WID)
	{
		int small_tester = i;

		i = (i * max_hgt) / MAX_HGT;
		i = (i * max_wid) / MAX_WID;
		i += 1;

		if (i > small_tester) i = small_tester;
		else if (cheat_hear)
		{
			msgf("Reduced monsters base from %d to %d", small_tester, i);
		}
	}

	i += randint1(8);

	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	p = ((i + k)*d_ptr->freq_monsters)/100;

	/* Apply population density dungeon flags */
	if (d_ptr->flags & DF_DENSE)
	{
		p *= 2;
	}
	if (d_ptr->flags & DF_SPARSE)
	{
		p /= 2;
	}

	/* Compensate for duplicating that happens in apply_symmetry */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2))
		p /= 2;

	if (d_ptr->flags & (DF_SYM_4 | DF_SYM_R4))
		p /= 4;

	/* Bounds forcing */
	p = MAX(1,p);

	/* Add some monsters to the dungeon */
	add_monsters(p);

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, (randint1(k)*d_ptr->freq_traps)/100);

	/* Put some rubble in corridors */
	alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, (randint1(k)*d_ptr->freq_rubble)/100);

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT,
				 Rand_normal((DUN_AMT_ROOM*d_ptr->freq_objects)/100, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT,
				 Rand_normal((DUN_AMT_ITEM*d_ptr->freq_objects)/100, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal((DUN_AMT_GOLD*d_ptr->freq_objects)/100, 3));

	/* Restore actual bounds */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2 | DF_SYM_4 | DF_SYM_R4))
	{
		p_ptr->min_wid = min_wid;
		p_ptr->max_wid = max_wid;
		p_ptr->min_hgt = min_hgt;
		p_ptr->max_hgt = max_hgt;
	}

	/* Replace standard stone with dungeon-specified walls */
	for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
	{
		for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
		{
			c_ptr = cave_p(x,y);
			if (c_ptr->feat >= FEAT_WALL_EXTRA && c_ptr->feat <= FEAT_WALL_SOLID)
			{
				delete_field_location(c_ptr);
				set_feat_grid(c_ptr, d_ptr->wall);
			}
			else if (c_ptr->feat >= FEAT_PERM_EXTRA && c_ptr->feat <= FEAT_PERM_SOLID)
			{
				delete_field_location(c_ptr);
				set_feat_grid(c_ptr, d_ptr->perm_wall);
			}
		}
	}

	/* Put some invisible walls in the dungeon for nightmare mode */
	if (ironman_nightmare && !current_quest)
	{
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_INVIS,
					 Rand_normal(DUN_AMT_INVIS, 3));
	}

	/* Do we light the whole level? */
	if (d_ptr->flags & DF_FORCE_LIT ||
		(empty_level && (!one_in_(DARK_EMPTY) || (randint1(100) > p_ptr->depth))))
	{
		/* Lite the cave */
		for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
		{
			for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
			{
				cave_p(x, y)->info |= (CAVE_GLOW);
			}
		}
	}

	/* Apply cave symmetry */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2 | DF_SYM_4 | DF_SYM_R4))
	{
		apply_symmetry(d_ptr, horiz);
		apply_symmetry_connect(d_ptr, horiz);


	}

	/* Determine the character location */
	if (d_ptr->flags & DF_CENTER)
	{
		/* Must be in the center.  Clear the space if we need to. */
		x = (p_ptr->min_wid + p_ptr->max_wid)/2;
		y = (p_ptr->min_hgt + p_ptr->max_hgt)/2;
		c_ptr = cave_p(x,y);

		delete_monster(x,y);
		delete_object(x,y);
		delete_field(x,y);

		c_ptr->feat = d_ptr->floor;

		p_ptr->px = x;
		p_ptr->py = y;
		Term_move_player();
	}

	else if (!new_player_spot())
	{
		/* Couldn't place the player.  */
		if (!current_quest) return FALSE;

		/* In a quest level, avoid at all costs */
		new_player_spot_panic();
	}

	/* Place the "boss" on boss/kill quest levels */
	if (current_quest && (current_quest->type == QUEST_TYPE_FIXED_BOSS ||
						  current_quest->type == QUEST_TYPE_FIXED_KILL))
	{
		place_boss();
	}

	/* Apparently not just paranoia: clean up duplicates */
	clean_duplicates();
	
	/* Check if this is a "dead level" and do whatever is appropriate */
	if (current_quest)
	{
		dead_level();
	}
	else if (current_quest && current_quest->type != QUEST_TYPE_FIXED_CLEAROUT)
	{
		reduce_objects();
	}

	return TRUE;
}

static bool castle_gen(dun_type *d_ptr)
{
	int x, y, xmid, ymid, gate;
	int min_hgt, max_hgt, min_wid, max_wid;
	int i, k, p, n;
	bool horiz = one_in_(2);

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;

	/* For this, only need floor */
	dun->feat_floor = d_ptr->floor;

	/* No vaults yet */
	dun->vaults = 0;

	/* Initialize */
	min_hgt = p_ptr->min_hgt;
	min_wid = p_ptr->min_wid;
	max_hgt = p_ptr->max_hgt;
	max_wid = p_ptr->max_wid;

	xmid = (p_ptr->min_wid + p_ptr->max_wid - 1)/2;
	ymid = (p_ptr->min_hgt + p_ptr->max_hgt - 1)/2;

	/* Start with permanent walls */
	generate_fill(p_ptr->min_wid, p_ptr->min_hgt, p_ptr->max_wid - 1, p_ptr->max_hgt - 1,
		d_ptr->perm_wall);

	/* Path outside */
	generate_fill(p_ptr->min_wid + 1, p_ptr->min_hgt + 1, p_ptr->max_wid - 2, p_ptr->max_hgt - 2,
		d_ptr->floor);

	/* Stone wall */
	generate_fill(p_ptr->min_wid + 2, p_ptr->min_hgt + 2, p_ptr->max_wid - 3, p_ptr->max_hgt - 3,
		FEAT_WALL_OUTER);

	/* Fill inside of castle with floor */
	generate_fill(p_ptr->min_wid+3, p_ptr->min_hgt+3, p_ptr->max_wid - 4, p_ptr->max_hgt - 4,
		d_ptr->floor);

	/* Determine where the castle gate will be */
	gate = randint0(4);
	switch(gate)
	{
		case 0:
			x = xmid;
			y = p_ptr->min_hgt+2;
			break;
		case 1:
			x = xmid;
			y = p_ptr->max_hgt-3;
			break;
		case 2:
			y = ymid;
			x = p_ptr->min_wid+2;
			break;
		case 3:
			y = ymid;
			x = p_ptr->max_wid-3;
			break;
	}

	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2))
	{
		/* Adjust to anticipate the symmetry */
		if (gate < 2)
		{
			x = rand_range(p_ptr->min_wid+2, xmid-1);
			y = (horiz ? y : p_ptr->min_hgt+2);
		}
		else
		{
			x = (horiz ? p_ptr->min_wid+2 : x);
			y = rand_range(p_ptr->min_hgt+2, ymid-1);
		}
	}

	place_random_door(x, y);

	/* Try to make double entrance */
	x = (gate < 2 ? x-1 : x);
	y = (gate < 2 ? y : y-1);

	place_random_door(x, y);

	/* Fill castle with rooms */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2))
	{
		if (horiz)
		{
			if (one_in_(2))
				generate_fill( (p_ptr->max_wid - 3)/2, p_ptr->min_hgt+2,  (p_ptr->max_wid - 3)/2, p_ptr->max_hgt - 3, FEAT_WALL_EXTRA);
			build_recursive_room(p_ptr->min_wid+2, p_ptr->min_hgt+2, (p_ptr->max_wid - 3)/2, p_ptr->max_hgt - 3, 5);
		}
		else
		{
			if (one_in_(2))
				generate_fill( p_ptr->min_wid + 2, (p_ptr->max_hgt - 3)/2, p_ptr->max_wid - 3, (p_ptr->max_hgt - 3)/2, FEAT_WALL_EXTRA);
			build_recursive_room(p_ptr->min_wid+2, p_ptr->min_hgt+2, p_ptr->max_wid - 3, (p_ptr->max_hgt - 3)/2, 5);
		}
	}
	else if (d_ptr->flags & (DF_SYM_4 | DF_SYM_R4))
	{
		if (one_in_(2))
		{
			generate_fill( (p_ptr->max_wid - 3)/2, p_ptr->min_hgt+2,  (p_ptr->max_wid - 3)/2, p_ptr->max_hgt - 3, FEAT_WALL_EXTRA);
			generate_fill( p_ptr->min_wid + 2, (p_ptr->max_hgt - 3)/2, p_ptr->max_wid - 3, (p_ptr->max_hgt - 3)/2, FEAT_WALL_EXTRA);
		}
		build_recursive_room(p_ptr->min_wid+2, p_ptr->min_hgt+2, (p_ptr->max_wid - 3)/2, (p_ptr->max_hgt - 3)/2, 5);
	}
	else
		build_recursive_room(p_ptr->min_wid+2, p_ptr->min_hgt+2, p_ptr->max_wid - 3, p_ptr->max_hgt - 3, 5);


	/* Big hack: Modify the bounds on symmetric levels, so as to force allocated things not to get overwritten. */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2 | DF_SYM_4 | DF_SYM_R4))
	{
		min_hgt = p_ptr->min_hgt;
		min_wid = p_ptr->min_wid;
		max_hgt = p_ptr->max_hgt;
		max_wid = p_ptr->max_wid;

		if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2))
		{
			if (horiz)
			{
				p_ptr->max_wid = (p_ptr->min_wid + p_ptr->max_wid)/2;
			}
			else
			{
				p_ptr->max_hgt = (p_ptr->min_hgt + p_ptr->max_hgt)/2;
			}
		}
		else
		{
			p_ptr->max_wid = (p_ptr->min_wid + p_ptr->max_wid)/2;
			p_ptr->max_hgt = (p_ptr->min_hgt + p_ptr->max_hgt)/2;
		}
	}

	/* Place 2-4 down stairs near some walls */
	n = (rand_range(1,1000) * d_ptr->freq_stairs) / 100;
	n = (n / (1000/3))+2;

	(void)alloc_stairs(FEAT_MORE, MAX(n, 1), 3);

	/* Place 1-2 up stairs near some walls */
	n = (rand_range(1,1000) * d_ptr->freq_stairs) / 100;
	n = (n / (1000/2))+1;

	(void)alloc_stairs(FEAT_LESS, MAX(n, 1), 3);

	/* Use as player starting location if we fail */
	if (!new_player_spot())
	{
		/* In a quest level, avoid at all costs */
		new_player_spot_panic();
	}

	/* Place quest monsters in the dungeon */
	trigger_quest_create(QC_DUN_MONST, NULL);

	/* Place quest artifacts in the dungeon */
	trigger_quest_create(QC_DUN_ARTIFACT, NULL);

	/* Pick a base number of monsters */
	i = MIN_M_ALLOC_LEVEL;

	/* To make small levels a bit more playable */
	if (max_hgt < MAX_HGT || max_wid < MAX_WID)
	{
		int small_tester = i;

		i = (i * max_hgt) / MAX_HGT;
		i = (i * max_wid) / MAX_WID;
		i += 1;

		if (i > small_tester) i = small_tester;
		else if (cheat_hear)
		{
			msgf("Reduced monsters base from %d to %d", small_tester, i);
		}
	}

	i += randint1(8);

	/* Basic "amount" */
	k = (p_ptr->depth / 3);
	if (k > 10) k = 10;
	if (k < 2) k = 2;

	p = ((i + k)*d_ptr->freq_monsters)/100;

	/* Apply population density dungeon flags */
	if (d_ptr->flags & DF_DENSE)
	{
		p *= 2;
	}
	if (d_ptr->flags & DF_SPARSE)
	{
		p /= 2;
	}

	/* Compensate for duplicating that happens in apply_symmetry */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2))
		p /= 2;

	if (d_ptr->flags & (DF_SYM_4 | DF_SYM_R4))
		p /= 4;

	/* Add some monsters to the dungeon */
	add_monsters(p);

	/* Place some traps in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, (randint1(k)*d_ptr->freq_traps)/100);

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT,
				 Rand_normal((DUN_AMT_ROOM*d_ptr->freq_objects)/100, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT,
				 Rand_normal((DUN_AMT_ITEM*d_ptr->freq_objects)/100, 3));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, Rand_normal((DUN_AMT_GOLD*d_ptr->freq_objects)/100, 3));

	/* Restore actual bounds */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2 | DF_SYM_4 | DF_SYM_R4))
	{
		p_ptr->min_wid = min_wid;
		p_ptr->max_wid = max_wid;
		p_ptr->min_hgt = min_hgt;
		p_ptr->max_hgt = max_hgt;
	}

	/* Apply cave symmetry */
	if (d_ptr->flags & (DF_SYM_2 | DF_SYM_R2 | DF_SYM_4 | DF_SYM_R4))
	{
		apply_symmetry(d_ptr, horiz);
	}

	/* Place the "boss" on boss/kill quest levels */
	if (current_quest && (current_quest->type == QUEST_TYPE_FIXED_BOSS ||
						  current_quest->type == QUEST_TYPE_FIXED_KILL))
	{
		place_boss();
	}

	/* Check if this is a "dead level" and do whatever is appropriate */
	if (current_quest)
	{
		dead_level();
	}
	else if (current_quest && current_quest->type != QUEST_TYPE_FIXED_CLEAROUT)
	{
		reduce_objects();
	}

	return TRUE;
}

/* Make a "pure castle" level */
static bool castle_level_gen(cptr *why, dun_type *d_ptr)
{
	int i;
	bool rv = TRUE;

	if (current_quest && current_quest->type != QUEST_TYPE_FIXED_CLEAROUT)
	{
		/* Use the simple RNG */
		Rand_quick = TRUE;

		/* Force consistent levels in quests */
		Rand_value = current_quest->data.fix.seed;

		/* Consistent but different values for subsequent levels */
		for (i = current_quest->data.fix.min_level; i < p_ptr->depth; i++)
			Rand_value = randint0(0x10000000);
	}

	/* For castles, DF_SMALL takes precedence, then medium, then big. */
	if (d_ptr->flags & DF_SMALL)
	{
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = rand_range (MAX_HGT/8, MAX_HGT/4);
		p_ptr->min_wid = 0;
		p_ptr->max_wid = rand_range (MAX_WID/8, MAX_WID/4);
	}
	else if (d_ptr->flags & DF_MEDIUM)
	{
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = rand_range (MAX_HGT/4, MAX_HGT/2);
		p_ptr->min_wid = 0;
		p_ptr->max_wid = rand_range (MAX_WID/4, MAX_WID/2);
	}
	else if (d_ptr->flags & DF_BIG)
	{
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = MAX_HGT;
		p_ptr->min_wid = 0;
		p_ptr->max_wid = MAX_WID;
	}
	else
	{
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = rand_range (MAX_HGT/2, MAX_HGT);
		p_ptr->min_wid = 0;
		p_ptr->max_wid = rand_range (MAX_WID/2, MAX_WID);
	}

	/* Get the new region */
	create_region(d_ptr, p_ptr->max_wid, p_ptr->max_hgt, REGION_CAVE);

	/* Grab the reference to it */
	incref_region(cur_region);

	/* Make a dungeon */
	if (!castle_gen(d_ptr))
	{
		*why = "could not place player";
		rv = FALSE;
	}

	if (current_quest && current_quest->type != QUEST_TYPE_FIXED_CLEAROUT)
	{
		/* Back to the "complex" RNG */
		Rand_quick = FALSE;
	}

	return rv;
}


/* Make a real level */
static bool level_gen(cptr *why, dun_type *d_ptr)
{
	int level_height, level_width;
	bool rv = TRUE;
	int i;

	if (d_ptr->flags & DF_PURE_CASTLE)  return castle_level_gen(why, d_ptr);

	if (current_quest && current_quest->type != QUEST_TYPE_FIXED_CLEAROUT)
	{
		/* Use the simple RNG */
		Rand_quick = TRUE;

		/* Force consistent levels in quests */
		Rand_value = current_quest->data.fix.seed;

		/* Consistent but different values for subsequent levels */
		for (i = current_quest->data.fix.min_level; i < p_ptr->depth; i++)
			Rand_value = randint0(0x10000000);
	}

	if (d_ptr->flags & (DF_BIG | DF_MEDIUM | DF_SMALL))
	{
		int n = count_bits(d_ptr->flags & (DF_BIG | DF_MEDIUM | DF_SMALL));
		int x = randint1(n);
		bool done = FALSE;

		if (d_ptr->flags & DF_BIG && x == 1)
		{
			/* Big dungeon */
			p_ptr->min_hgt = 0;
			p_ptr->max_hgt = MAX_HGT;
			p_ptr->min_wid = 0;
			p_ptr->max_wid = MAX_WID;
			done = TRUE;
		}
		else if (!(d_ptr->flags & DF_BIG))
		{
			x++;
		}

		if (!done && (d_ptr->flags & DF_MEDIUM) && x == 2)
		{
			/* Medium dungeon */
			p_ptr->min_hgt = 0;
			p_ptr->max_hgt = MAX_HGT/2;
			p_ptr->min_wid = 0;
			p_ptr->max_wid = MAX_WID/2;
		}

		else if (!done)
		{
			/* Small dungeon */
			p_ptr->min_hgt = 0;
			p_ptr->max_hgt = MAX_HGT/4;
			p_ptr->min_wid = 0;
			p_ptr->max_wid = MAX_WID/4;
		}
	}
	else if (ironman_small_levels || (one_in_(SMALL_LEVEL) && small_levels))
	{
		if (cheat_room)
			msgf("A 'small' dungeon level.");

		while (TRUE)
		{
			level_height = randint1(MAX_HGT / BLOCK_HGT);
			level_width = randint1(MAX_WID / BLOCK_WID);

			/* Exit if larger than one screen, but less than normal dungeon */
			if ((level_height < (MAX_HGT / BLOCK_HGT)) &&
				(level_height >= (22 / BLOCK_HGT)) &&
				(level_width < (MAX_WID / BLOCK_WID)) &&
				(level_width >= (66 / BLOCK_WID))) break;
		}

		/* Get bounds of dungeon */
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = level_height * BLOCK_HGT;
		p_ptr->min_wid = 0;
		p_ptr->max_wid = level_width * BLOCK_WID;

		if (cheat_room)
			msgf("X:%d, Y:%d.", p_ptr->max_wid, p_ptr->max_hgt);
	}
	else
	{
		/* Big dungeon */
		p_ptr->min_hgt = 0;
		p_ptr->max_hgt = MAX_HGT;
		p_ptr->min_wid = 0;
		p_ptr->max_wid = MAX_WID;
	}

	/* Get the new region */
	create_region(d_ptr, p_ptr->max_wid, p_ptr->max_hgt, REGION_CAVE);

	/* Grab the reference to it */
	incref_region(cur_region);

	/* Make a dungeon */
	if (!cave_gen(d_ptr))
	{
		*why = "could not place player";
		rv = FALSE;
	}

	if (current_quest && current_quest->type != QUEST_TYPE_FIXED_CLEAROUT)
	{
		/* Back to the "complex" RNG */
		Rand_quick = FALSE;
	}

	return rv;
}


/*
 * Delete a region from the region list
 *
 * Only call this when cleaning up the game during
 * exit - use unref_region() below normally.
 */
void del_region(int rg_idx)
{
	int i, j;

	pcave_type *pc_ptr;

	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/*
	 * Deallocate everything if region uses
	 * literal meanings of cave_type structure values.
	 *
	 * Note - quests have this flag unset.
	 * m_idx refers to race of monster.
	 * o_idx refers to type of object.
	 * fld_idx refers to type of field.
	 *
	 * (Rather than index of monster, object or fields.)
	 */
	if (ri_ptr->flags & REGION_CAVE)
	{
		/* Delete everything in the region */
		wipe_monsters(rg_idx);

		/*
		 * Objects are deleted after the monsters,
		 * because monsters carry them.
		 */
		wipe_objects(rg_idx);
		wipe_fields(rg_idx);

		/* Hack - delete player knowledge */
		for (i = 0; i < MAX_WID; i++)
		{
			for (j = 0; j < MAX_HGT; j++)
			{
				pc_ptr = &p_ptr->pcave[j][i];

				/* Clear the player dungeon flags */
				pc_ptr->player = 0x00;

				/* Clear the player dungeon memory */
				forget_grid(pc_ptr);
			}
		}
	}

	/* Deallocate the cave information */

	/* Free the cave */
	for (i = 0; i < ri_ptr->ysize; i++)
	{
		/* Deallocate one row of the cave */
		FREE(rg_list[rg_idx][i]);
	}

	/* Free the region + info */
	KILL(rg_list[rg_idx]);
	(void)WIPE(&ri_list[rg_idx], region_info);

	/* Decrement counter */
	rg_cnt--;
}

/*
 * Decrease refcount on region - deallocate if empty
 */
int unref_region(int rg_idx)
{
	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/* Paranoia */
	if (!ri_ptr->refcount) quit("Region refcount missmatch");

	/* Decrease refcount */
	ri_ptr->refcount--;

	/* Delete if just lost final reference */
	if (!ri_ptr->refcount)
	{
		/* Paranoia */
		if (!rg_list[rg_idx]) quit("Deleting unallocated region");

		del_region(rg_idx);

		/* Region no longer exists */
		return (0);
	}

	/* No change */
	return (rg_idx);
}


/*
 * Increase refcount on region
 */
void incref_region(int rg_idx)
{
	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/* Paranoia */
	if (!rg_list[rg_idx]) quit("Incrementing unallocated region");

	/* Increase refcount */
	ri_ptr->refcount++;
}


/*
 * Set the global region
 */
void set_region(int rg_idx)
{
	/* Paranoia */
	if (rg_idx >= rg_max) quit("Setting invalid region");

	/* Set the region */
	cur_region = rg_idx;

	/* Set region pointer */
	cave_data = rg_list[cur_region];
}


/*
 * Delete all regions - and everything inside them
 */
void wipe_rg_list(void)
{
	int i;

	/* Wipe each active region */
	for (i = 1; i < rg_max; i++)
	{
		/*
		 * Hack - use del_region rather than unref_region.
		 *
		 * This function will not clean up all outstanding
		 * references to the regions.  Only call this when you
		 * know no such references exist.
		 */
		if (rg_list[i]) del_region(i);
	}

	/* Wipe the remaining objects, monsters and fields (in wilderness) */
	wipe_m_list();
	wipe_o_list();
	wipe_f_list();
}


/*
 * Do the actual work of allocating a region
 */
static void allocate_region(int rg_idx, int x, int y)
{
	int i;

	/* Acquire region info */
	region_info *ri_ptr = &ri_list[rg_idx];

	/* Save size */
	ri_ptr->xsize = x;
	ri_ptr->ysize = y;

	/* Hack set the refcount to zero - assume caller increments refcount */
	ri_ptr->refcount = 0;

	/* Make the array of pointers to the cave */
	C_MAKE(rg_list[rg_idx], y, cave_type *);

	/* Allocate and wipe each line of the region */
	for (i = 0; i < y; i++)
	{
		/* Allocate one row of the cave */
		C_MAKE(rg_list[rg_idx][i], x, cave_type);
	}

	/* Hack - set this region to be the currently used one */
	set_region(rg_idx);
}


/*
 * Allocate a region.
 *
 * (Usually used to store the dungeon,
 * However, can be used in the wilderness to store
 * town info.)
 *
 * This rountine should never fail - but be prepared
 * for when it does.
 */
void create_region_aux(s16b *region, int x, int y, byte flags)
{
	int rg_idx;
	int i;

	if (rg_max < z_info->rg_max)
	{
		/* Get next space */
		rg_idx = rg_max;

		/* Expand region array */
		rg_max++;

		/* Count regions */
		rg_cnt++;

		/* Allocate the region */
		allocate_region(rg_idx, x, y);

		/* Save the flags */
		ri_list[rg_idx].flags = flags;

		/* Save the region number */
		*region = rg_idx;

		/* Done */
		return;
	}

	/* Recycle dead regions */
	for (i = 1; i < rg_max; i++)
	{
		/* Skip used regions */
		if (rg_list[i]) continue;

		/* Count regions */
		rg_cnt++;

		/* Allocate the region */
		allocate_region(i, x, y);

		/* Save the flags */
		ri_list[i].flags = flags;

		/* Use this region */
		*region = i;

		return;
	}

	/* Warn the player */
	get_check("Too many regions!  The game is probably about to crash.");

	/* Paranoia */
	*region = 0;

	/* Oops */
	return;
}


/*
 * Generates a random dungeon level			-RAK-
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 */
void generate_cave(void)
{
	int num;

	dun_type *dundata = place[p_ptr->place_num].dungeon;

	/* Build the wilderness */
	if (!p_ptr->depth)
	{
		/* The "dungeon" is ready */
		character_dungeon = TRUE;

		return;
	}

	/* Prepare for object memory */
	current_object_source.type = OM_FLOOR;
	current_object_source.place_num = p_ptr->place_num;
	current_object_source.depth = p_ptr->depth;
	current_object_source.data = 0;


	/* Get random dungeon */
	if (vanilla_town)
	{
		/* Use d_ptr = DUN_TYPE_VANILLA */
		const dun_gen_type *d_ptr = &dungeons[13];

		/* Get floor type */
		dundata->floor = d_ptr->floor;

		/* Default streamer settings */
		dundata->vein[0].deep = FEAT_MAGMA;
		dundata->vein[0].number = DUN_STR_MAG;
		dundata->vein[0].size = DUN_STR_RNG;
		dundata->vein[1].deep = FEAT_QUARTZ;
		dundata->vein[1].number = DUN_STR_QUA;
		dundata->vein[1].size = DUN_STR_RNG;

		dundata->river[0].deep = FEAT_DEEP_WATER;
		dundata->river[0].shal = FEAT_SHAL_WATER;
		dundata->river[0].rarity = 8;
		dundata->river[0].size = 2;
		dundata->river[1].rarity = 0;

		dundata->lake.deep = FEAT_DEEP_WATER;
		dundata->lake.shal = FEAT_SHAL_WATER;
		dundata->lake.rarity = 15;
		dundata->lake.size = 66;

		/* Get room types */
		dundata->rooms = d_ptr->rooms;

		/* Set the object theme (structure copy) */
		dundata->theme = d_ptr->theme;

		/* Hack - Reset the dungeon habitat to be everything */
		dundata->habitat = d_ptr->habitat;
	}

	/* Generate */
	for (num = 0; TRUE; num++)
	{
		bool okay = TRUE;

		cptr why = NULL;

		/* Nothing special here yet */
		dundata->good_item_flag = FALSE;

		/* Nothing good here yet */
		dun_rating = 0;
		is_special = FALSE;

		/* Generate the level.  While it's being generated, it's "in progress" */
		level_gen_in_progress = TRUE;
		okay = level_gen(&why, dundata);
		level_gen_in_progress = FALSE;

		/* Save rating for later */
		dundata->rating = dun_rating;
		dundata->good_item_flag = is_special;

		/* Extract the feeling */
		p_ptr->state.feeling = extract_feeling();

		/* Prevent object over-flow */
		if (o_cnt + 1 >= z_info->o_max)
		{
			/* Message */
			why = "too many objects";

			/* Message */
			okay = FALSE;
		}
		/* Prevent monster over-flow */
		else if (m_cnt + 1 >= z_info->m_max)
		{
			/* Message */
			why = "too many monsters";

			/* Message */
			okay = FALSE;
		}

		/* Accept */
		if (okay) break;

		/* Message */
		if (why) msgf("Generation restarted (%s)", why);

		/* Delete the level - not good enough */
		dundata->region = unref_region(dundata->region);

		/* Ugly Hack: If we failed to generate a quest level, make a new seed.
		   This may screw up consistency in a multi-level quest dungeon.  */
		if (current_quest)
		{
			current_quest->data.fix.seed = randint0(0x10000000);
		}
	}

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Remember when this level was "created" */
	old_turn = turn;
}

/*
 * Interprets DUN_TYPES as a set of bits, picks one at random, and uses it to select
 * a dungeon type, and the default dungeon parameters are then copied into d_ptr.
 */
void pick_dungeon(dun_type * d_ptr, u32b dun_types)
{
	byte b = 0;
	byte num_bits = 0;
	dun_gen_type *dg_ptr;

	/* Just in case, defaults to vanilla dungeon */
	int d_num  = 13;
	int i;

	/* Hack: make sure there is a type. */
	if (!dun_types) dun_types = DUN_TYPE_VANILLA;

	/* Count the number of bits in dun_types */
	for (i = 0; i < 32; i++)
	{
		if (dun_types & (1 << i)) num_bits++;
	}

	/* Pick a bit */
	b = randint0(num_bits);

	/* Figure out which bit we picked, and set d_num to the number of that bit. */
	for (i = 0; i < 32; i++)
	{
		if (dun_types & (1 << i)) b++;

		if (b == num_bits)
		{
			d_num = i;
			break;
		}
	}

	/* Find the "gen type */
	dg_ptr = &dungeons[d_num];

	/* Copy / initialize stuff */
	d_ptr->theme.treasure = dg_ptr->theme.treasure;
	d_ptr->theme.combat = dg_ptr->theme.combat;
	d_ptr->theme.magic = dg_ptr->theme.magic;
	d_ptr->theme.tools = dg_ptr->theme.tools;

	d_ptr->habitat = dg_ptr->habitat;

	d_ptr->min_level = dg_ptr->min_level;
	d_ptr->max_level = dg_ptr->max_level;

	d_ptr->rating = 0;
	d_ptr->region = 0;

	d_ptr->rooms = dg_ptr->rooms;

	d_ptr->recall_depth = 0;

	d_ptr->good_item_flag = FALSE;

	d_ptr->floor = dg_ptr->floor;
	d_ptr->wall = dg_ptr->wall;
	d_ptr->perm_wall = dg_ptr->perm_wall;

	for (i = 0; i < 2; i++)
	{
		d_ptr->vein[i].deep = dg_ptr->vein[i].deep;
		d_ptr->vein[i].shal = dg_ptr->vein[i].shal;
		d_ptr->vein[i].rarity = dg_ptr->vein[i].rarity;
		d_ptr->vein[i].size = dg_ptr->vein[i].size;
		d_ptr->vein[i].number = dg_ptr->vein[i].number;

		d_ptr->river[i].deep = dg_ptr->river[i].deep;
		d_ptr->river[i].shal = dg_ptr->river[i].shal;
		d_ptr->river[i].rarity = dg_ptr->river[i].rarity;
		d_ptr->river[i].size = dg_ptr->river[i].size;
		d_ptr->river[i].number = dg_ptr->river[i].number;
	}

	d_ptr->lake.deep = dg_ptr->lake.deep;
	d_ptr->lake.shal = dg_ptr->lake.shal;
	d_ptr->lake.rarity = dg_ptr->lake.rarity;
	d_ptr->lake.size = dg_ptr->lake.size;
	d_ptr->lake.number = dg_ptr->lake.number;

	d_ptr->freq_monsters = dg_ptr->freq_monsters;
	d_ptr->freq_objects = dg_ptr->freq_objects;
	d_ptr->freq_doors = dg_ptr->freq_doors;
	d_ptr->freq_traps = dg_ptr->freq_traps;
	d_ptr->freq_rubble = dg_ptr->freq_rubble;
	d_ptr->freq_treasure = dg_ptr->freq_treasure;
	d_ptr->freq_stairs = dg_ptr->freq_stairs;
	d_ptr->freq_arena = dg_ptr->freq_arena;
	d_ptr->freq_cavern = dg_ptr->freq_cavern;
	d_ptr->freq_tunnel = dg_ptr->freq_tunnel;

	d_ptr->room_limit = dg_ptr->room_limit;

	d_ptr->flags = dg_ptr->flags;
}
