/*
 * File: grid.c
 * Purpose: low-level dungeon creation primitives
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "generate.h"
#include "grid.h"

/*
 * Returns random co-ordinates for player/monster/object
 */
bool new_player_spot(void)
{
	int	y, x;
	int max_attempts = 5000;

	cave_type *c_ptr;

	/* Place the player */
	while (max_attempts--)
	{
		/* Pick a legal spot */
		y = rand_range(min_hgt, max_hgt - 1);
		x = rand_range(min_wid, max_wid - 1);

		c_ptr = &cave[y][x];

		/* Must be a "naked" floor grid */
		if (!cave_naked_grid(c_ptr)) continue;

		/* Refuse to start on anti-teleport grids */
		if (c_ptr->info & (CAVE_ICKY)) continue;

		/* Done */
		break;
	}

	if (max_attempts < 1) /* Should be -1, actually if we failed... */
		return FALSE;


	/* Save the new player grid */
	/* py = y; */
	/* px = x; */

	/* Save the new player grid */
	p_ptr->py = y;
	p_ptr->px = x;

	return TRUE;
}


/*
 * Place an up/down staircase at given location
 */
void place_random_stairs(int y, int x)
{
	bool up_stairs = TRUE;
	bool down_stairs = TRUE;
	cave_type *c_ptr;

	/* Paranoia */
	c_ptr = &cave[y][x];
	if (!cave_clean_grid(c_ptr)) return;

	/* Town */
	if (!p_ptr->depth)
		up_stairs = FALSE;

	/* Ironman */
	if (ironman_downward)
		up_stairs = FALSE;

	/* Bottom */
	if (p_ptr->depth >= MAX_DEPTH - 1)
		down_stairs = FALSE;

	/* Quest-level */
	if (quest_number(p_ptr->depth) && (p_ptr->depth > 1))
		down_stairs = FALSE;

	/* We can't place both */
	if (down_stairs && up_stairs)
	{
		/* Choose a staircase randomly */
		if (one_in_(2))
			up_stairs = FALSE;
		else
			down_stairs = FALSE;
	}

	/* Place the stairs */
	if (up_stairs)
	{
		c_ptr->feat = FEAT_LESS;
	}
	else if (down_stairs)
	{
		c_ptr->feat = FEAT_MORE;
	}
}


/*
 * Place a random type of door at the given location
 */
void place_random_door(int y, int x)
{
	int tmp;

	/* Making a door on top of fields is problematical */
	delete_field(y, x);
	
	/* Invisible wall */
	if (ironman_nightmare && one_in_(666))
	{
		/* Create invisible wall */
		cave_set_feat(y, x, FEAT_FLOOR);
		(void)place_field(y, x, FT_WALL_INVIS);
		return;
	}

	/* Choose an object */
	tmp = randint0(1000);

	/* Open doors (300/1000) */
	if (tmp < 300)
	{
		/* Create open door */
		cave_set_feat(y, x, FEAT_OPEN);
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		cave_set_feat(y, x, FEAT_BROKEN);
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		cave_set_feat(y, x, FEAT_SECRET);
	}

	/* Closed, locked, or stuck doors (400/1000) */
	else place_closed_door(y, x);
}


/*
 * Place a random type of normal door at the given location.
 */
void place_closed_door(int y, int x)
{
	int tmp;

	/* Invisible wall */
	if (ironman_nightmare && one_in_(666))
	{
		/* Create invisible wall */
		cave_set_feat(y, x, FEAT_FLOOR);
		(void)place_field(y, x, FT_WALL_INVIS);
		return;
	}

	/* Choose an object */
	tmp = randint0(400);
	
	/* Closed doors (300/400) */
	if (tmp < 300)
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_CLOSED);
	}

	/* Locked doors (99/400) */
	else if (tmp < 399)
	{
		/* Create locked door */
		make_lockjam_door(y, x, randint1(10) + p_ptr->depth / 10, FALSE);
	}

	/* Stuck doors (1/400) */
	else
	{
		/* Create jammed door */
		make_lockjam_door(y, x, randint1(5) + p_ptr->depth / 10, TRUE);
	}
}


/*
 * Create up to "num" objects near the given coordinates
 * Only really called by some of the "vault" routines.
 */
void vault_objects(int y, int x, int num)
{
	int dummy = 0;
	int i = 0, j = y, k = x;

	cave_type *c_ptr;


	/* Attempt to place 'num' objects */
	for (; num > 0; --num)
	{
		/* Try up to 11 spots looking for empty space */
		for (i = 0; i < 11; ++i)
		{
			/* Pick a random location */
			while (dummy < SAFE_MAX_ATTEMPTS)
			{
				j = rand_spread(y, 2);
				k = rand_spread(x, 3);
				dummy++;
				if (!in_bounds(j, k)) continue;
				break;
			}


			if (dummy >= SAFE_MAX_ATTEMPTS)
			{
				if (cheat_room)
				{
					msg_print("Warning! Could not place vault object!");
				}
			}


			/* Require "clean" floor space */
			c_ptr = &cave[j][k];
			if (!cave_clean_grid(c_ptr)) continue;

			/* Place an item */
			if (randint0(100) < 75)
			{
				place_object(j, k, FALSE, FALSE);
			}

			/* Place gold */
			else
			{
				place_gold(j, k);
			}

			/* Placement accomplished */
			break;
		}
	}
}


/*
 * Place a trap with a given displacement of point
 */
static void vault_trap_aux(int y, int x, int yd, int xd)
{
	int count = 0, y1 = y, x1 = x;
	int dummy = 0;

	cave_type *c_ptr;

	/* Place traps */
	for (count = 0; count <= 5; count++)
	{
		/* Get a location */
		while (dummy < SAFE_MAX_ATTEMPTS)
		{
			y1 = rand_spread(y, yd);
			x1 = rand_spread(x, xd);
			dummy++;
			if (!in_bounds(y1, x1)) continue;
			break;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
				msg_print("Warning! Could not place vault trap!");
			}
		}

		/* Require "naked" floor grids */
		c_ptr = &cave[y1][x1];
		if (!cave_naked_grid(c_ptr)) continue;

		/* Place the trap */
		place_trap(y1, x1);

		/* Done */
		break;
	}
}


/*
 * Place some traps with a given displacement of given location
 */
void vault_traps(int y, int x, int yd, int xd, int num)
{
	int i;

	for (i = 0; i < num; i++)
	{
		vault_trap_aux(y, x, yd, xd);
	}
}


/*
 * Hack -- Place some sleeping monsters near the given location
 */
void vault_monsters(int y1, int x1, int num)
{
	int k, i, y, x;
	cave_type *c_ptr;

	/* Try to summon "num" monsters "near" the given location */
	for (k = 0; k < num; k++)
	{
		/* Try nine locations */
		for (i = 0; i < 9; i++)
		{
			int d = 1;

			/* Pick a nearby location */
			scatter(&y, &x, y1, x1, d);

			/* Require "empty" floor grids */
			c_ptr = &cave[y][x];
			if (!cave_empty_grid(c_ptr)) continue;

			/* Place the monster (allow groups) */
			monster_level = base_level + 2;
			(void)place_monster(y, x, TRUE, TRUE);
			monster_level = base_level;
			
			/* Have placed a monster */
			break;
		}
	}
}


/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(y, x)"
 *
 * We count only granite walls and permanent walls.
 */
int next_to_walls(int y, int x)
{
	int	k = 0;

	if (cave_floor_grid(&cave[y + 1][x])) k++;
	if (cave_floor_grid(&cave[y - 1][x])) k++;
	if (cave_floor_grid(&cave[y][x + 1])) k++;
	if (cave_floor_grid(&cave[y][x - 1])) k++;

	return (k);
}


/*
 * Generate helper -- create a new room with optional light
 */
void generate_room(int y1, int x1, int y2, int x2, int light)
{
	int y, x;
	
	cave_type *c_ptr;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* Point to grid */
			c_ptr = &cave[y][x];
			
			c_ptr->info |= (CAVE_ROOM);
			if (light) c_ptr->info |= (CAVE_GLOW);
		}
	}
}


/*
 * Generate helper -- set flags for random vault.
 */
void generate_vault(int y1, int x1, int y2, int x2)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave[y][x].info |= (CAVE_ROOM | CAVE_ICKY);
		}
	}
}


/*
 * Generate helper -- unset the CAVE_ICKY flag in a region.
 */
void clear_vault(int y1, int x1, int y2, int x2)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave[y][x].info &= ~(CAVE_ICKY);
		}
	}
}


/*
 * Generate helper -- fill a rectangle with a feature
 */
void generate_fill(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* Hack - only draw on cave[][] */
			cave[y][x].feat = feat;
		}
	}
}


/*
 * Generate helper -- draw a rectangle with a feature
 */
void generate_draw(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		cave_set_feat(y, x1, feat);
		cave_set_feat(y, x2, feat);
	}

	for (x = x1; x <= x2; x++)
	{
		cave_set_feat(y1, x, feat);
		cave_set_feat(y2, x, feat);
	}
}


/*
 * Generate helper -- split a rectangle with a feature
 */
void generate_plus(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	for (y = y1; y <= y2; y++)
	{
		cave_set_feat(y, x0, feat);
	}

	for (x = x1; x <= x2; x++)
	{
		cave_set_feat(y0, x, feat);
	}
}


/*
 * Generate helper -- open all sides of a rectangle with a feature
 */
void generate_open(int y1, int x1, int y2, int x2, int feat)
{
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open all sides */
	cave_set_feat(y1, x0, feat);
	cave_set_feat(y0, x1, feat);
	cave_set_feat(y2, x0, feat);
	cave_set_feat(y0, x2, feat);
}


/*
 * Generate helper -- open one side of a rectangle with a feature
 */
void generate_hole(int y1, int x1, int y2, int x2, int feat)
{
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open random side */
	switch (randint0(4))
	{
		case 0:
		{
			cave_set_feat(y1, x0, feat);
			break;
		}
		case 1:
		{
			cave_set_feat(y0, x1, feat);
			break;
		}
		case 2:
		{
			cave_set_feat(y2, x0, feat);
			break;
		}
		case 3:
		{
			cave_set_feat(y0, x2, feat);
			break;
		}
	}
}


/*
 * Generate helper -- open one side of a rectangle with a door
 */
void generate_door(int y1, int x1, int y2, int x2, bool secret)
{
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open random side */
	switch (randint0(4))
	{
		case 0:
		{
			y0 = y1;
			break;
		}
		case 1:
		{
			x0 = x1;
			break;
		}
		case 2:
		{
			y0 = y2;
			break;
		}
		case 3:
		{
			x0 = x2;
			break;
		}
	}
	
	/* Add the door */
	if (secret)
	{
		place_secret_door(y0, x0);
	}
	else
	{
		place_closed_door(y0, x0);
	}
}


/*
 * Always picks a correct direction
 */
static void correct_dir(int *rdir, int *cdir, int y1, int x1, int y2, int x2)
{
	/* Extract vertical and horizontal directions */
	*rdir = (y1 == y2) ? 0 : (y1 < y2) ? 1 : -1;
	*cdir = (x1 == x2) ? 0 : (x1 < x2) ? 1 : -1;

	/* Never move diagonally */
	if (*rdir && *cdir)
	{
		if (one_in_(2))
			*rdir = 0;
		else
			*cdir = 0;
	}
}



/*
 * Pick a random direction
 */
static void rand_dir(int *rdir, int *cdir)
{
	/* Pick a random direction */
	int i = randint0(4);

	/* Extract the dy/dx components */
	*rdir = ddy_ddd[i];
	*cdir = ddx_ddd[i];
}


/* Function that sees if a square is a floor.  (Includes range checking.) */
bool get_is_floor(int x, int y)
{
	if (!in_bounds(y, x))
	{
		/* Out of bounds */
		return (FALSE);
	}

	/* Do not count floors internal to other rooms */
	if (cave[y][x].info & CAVE_ROOM) return (FALSE);

	/* Do the real check */
	if (cave[y][x].feat == FEAT_FLOOR) return (TRUE);

	/* Not a floor */
	return (FALSE);
}


/* Set a square to be floor.  (Includes range checking.) */
void set_floor(int x, int y)
{
	cave_type *c_ptr;
	
	if (!in_bounds(y, x))
	{
		/* Out of bounds */
		return;
	}

	c_ptr = &cave[y][x];
	
	if (c_ptr->info & CAVE_ROOM)
	{
		/* A room border don't touch. */
		return;
	}

	/* Set to be floor if is a wall (don't touch lakes). */
	if (c_ptr->feat == FEAT_WALL_EXTRA)
	{
		c_ptr->feat = FEAT_FLOOR;
	}
}


/*
 * Constructs a tunnel between two points
 *
 * This function must be called BEFORE any streamers are created,
 * since we use the special "granite wall" sub-types to keep track
 * of legal places for corridors to pierce rooms.
 *
 * We use "door_flag" to prevent excessive construction of doors
 * along overlapping corridors.
 *
 * We queue the tunnel grids to prevent door creation along a corridor
 * which intersects itself.
 *
 * We queue the wall piercing grids to prevent a corridor from leaving
 * a room and then coming back in through the same entrance.
 *
 * We "pierce" grids which are "outer" walls of rooms, and when we
 * do so, we change all adjacent "outer" walls of rooms into "solid"
 * walls so that no two corridors may use adjacent grids for exits.
 *
 * The "solid" wall check prevents corridors from "chopping" the
 * corners of rooms off, as well as "silly" door placement, and
 * "excessively wide" room entrances.
 *
 * Useful "feat" values:
 *   FEAT_WALL_EXTRA -- granite walls
 *   FEAT_WALL_INNER -- inner room walls
 *   FEAT_WALL_OUTER -- outer room walls
 *   FEAT_WALL_SOLID -- solid room walls
 *   FEAT_PERM_EXTRA -- shop walls (perma)
 *   FEAT_PERM_INNER -- inner room walls (perma)
 *   FEAT_PERM_OUTER -- outer room walls (perma)
 *   FEAT_PERM_SOLID -- dungeon border (perma)
 */
void build_tunnel(int row1, int col1, int row2, int col2)
{
	int y, x;
	int tmp_row, tmp_col;
	int row_dir, col_dir;
	int start_row, start_col;
	int main_loop_count = 0;

	bool door_flag = FALSE;

	cave_type *c_ptr;

	/* Save the starting location */
	start_row = row1;
	start_col = col1;

	/* Start out in the correct direction */
	correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

	/* Keep going until done (or bored) */
	while ((row1 != row2) || (col1 != col2))
	{
		/* Mega-Hack -- Paranoia -- prevent infinite loops */
		if (main_loop_count++ > 2000) break;

		/* Allow bends in the tunnel */
		if (randint0(100) < dun_tun_chg)
		{
			/* Acquire the correct direction */
			correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Random direction */
			if (randint0(100) < dun_tun_rnd)
			{
				rand_dir(&row_dir, &col_dir);
			}
		}

		/* Get the next location */
		tmp_row = row1 + row_dir;
		tmp_col = col1 + col_dir;


		/* Extremely Important -- do not leave the dungeon */
		while (!in_bounds(tmp_row, tmp_col))
		{
			/* Acquire the correct direction */
			correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Random direction */
			if (randint0(100) < dun_tun_rnd)
			{
				rand_dir(&row_dir, &col_dir);
			}

			/* Get the next location */
			tmp_row = row1 + row_dir;
			tmp_col = col1 + col_dir;
		}


		/* Access the location */
		c_ptr = &cave[tmp_row][tmp_col];


		/* Avoid the edge of the dungeon */
		if (c_ptr->feat == FEAT_PERM_SOLID) continue;

		/* Avoid the edge of vaults */
		if (c_ptr->feat == FEAT_PERM_OUTER) continue;

		/* Avoid "solid" granite walls */
		if (c_ptr->feat == FEAT_WALL_SOLID) continue;

		/* Pierce "outer" walls of rooms */
		if (c_ptr->feat == FEAT_WALL_OUTER)
		{
			/* Acquire the "next" location */
			y = tmp_row + row_dir;
			x = tmp_col + col_dir;

			/* Hack -- Avoid outer/solid permanent walls */
			if (cave[y][x].feat == FEAT_PERM_SOLID) continue;
			if (cave[y][x].feat == FEAT_PERM_OUTER) continue;

			/* Hack -- Avoid outer/solid granite walls */
			if (cave[y][x].feat == FEAT_WALL_OUTER) continue;
			if (cave[y][x].feat == FEAT_WALL_SOLID) continue;

			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Save the wall location */
			if (dun->wall_n < WALL_MAX)
			{
				dun->wall[dun->wall_n].y = row1;
				dun->wall[dun->wall_n].x = col1;
				dun->wall_n++;
			}

			/* Forbid re-entry near this piercing */
			for (y = row1 - 1; y <= row1 + 1; y++)
			{
				for (x = col1 - 1; x <= col1 + 1; x++)
				{
					/* Convert adjacent "outer" walls as "solid" walls */
					if (cave[y][x].feat == FEAT_WALL_OUTER)
					{
						/* Change the wall to a "solid" wall */
						cave[y][x].feat = FEAT_WALL_SOLID;
					}
				}
			}
		}

		/* Travel quickly through rooms */
		else if (c_ptr->info & (CAVE_ROOM))
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;
		}

		/* Tunnel through all other walls */
		else if (c_ptr->feat >= FEAT_WALL_EXTRA)
		{
			/* Accept this location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Save the tunnel location */
			if (dun->tunn_n < TUNN_MAX)
			{
				dun->tunn[dun->tunn_n].y = row1;
				dun->tunn[dun->tunn_n].x = col1;
				dun->tunn_n++;
			}

			/* Allow door in next grid */
			door_flag = FALSE;
		}

		/* Handle corridor intersections or overlaps */
		else
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;

			/* Collect legal door locations */
			if (!door_flag)
			{
				/* Save the door location */
				if (dun->door_n < DOOR_MAX)
				{
					dun->door[dun->door_n].y = row1;
					dun->door[dun->door_n].x = col1;
					dun->door_n++;
				}

				/* No door in next grid */
				door_flag = TRUE;
			}

			/* Hack -- allow pre-emptive tunnel termination */
			if (randint0(100) >= dun_tun_con)
			{
				/* Distance between row1 and start_row */
				tmp_row = row1 - start_row;
				if (tmp_row < 0) tmp_row = (-tmp_row);

				/* Distance between col1 and start_col */
				tmp_col = col1 - start_col;
				if (tmp_col < 0) tmp_col = (-tmp_col);

				/* Terminate the tunnel */
				if ((tmp_row > 10) || (tmp_col > 10)) break;
			}
		}
	}
}


/*
 * This routine adds the square to the tunnel
 * It also checks for SOLID walls - and returns a nearby
 * non-SOLID square in (x,y) so that a simple avoiding
 * routine can be used. The returned boolean value reflects
 * whether or not this routine hit a SOLID wall.
 *
 * "affectwall" toggles whether or not this new square affects
 * the boundaries of rooms. - This is used by the catacomb
 * routine.
 */
static bool set_tunnel(int *x, int *y, bool affectwall)
{
	int feat, i, j, dx, dy;


	if (!in_bounds(*y, *x)) return TRUE;

	feat = cave[*y][*x].feat;

	if ((feat == FEAT_PERM_OUTER) ||
	    (feat == FEAT_PERM_INNER) ||
	    (feat == FEAT_WALL_INNER))
	{
		/*
		 * Ignore permanent walls - sometimes cannot tunnel around them anyway
		 * so don't try - it just complicates things unnecessarily.
		 */
		return TRUE;
	}

	if (feat == FEAT_WALL_EXTRA)
	{
		/* Save the tunnel location */
		if (dun->tunn_n < TUNN_MAX)
		{
				dun->tunn[dun->tunn_n].y = *y;
				dun->tunn[dun->tunn_n].x = *x;
				dun->tunn_n++;
		}

		return TRUE;
	}

	if (feat == FEAT_FLOOR)
	{
		/* Don't do anything */
		return TRUE;
	}

	if ((feat == FEAT_WALL_OUTER) && affectwall)
	{
		/* Save the wall location */
		if (dun->wall_n < WALL_MAX)
		{
			dun->wall[dun->wall_n].y = *y;
			dun->wall[dun->wall_n].x = *x;
			dun->wall_n++;
		}

		/* Forbid re-entry near this piercing */
		for (j = *y - 1; j <= *y + 1; j++)
		{
			for (i = *x - 1; i <= *x + 1; i++)
			{
				/* Convert adjacent "outer" walls as "solid" walls */
				if (cave[j][i].feat == FEAT_WALL_OUTER)
				{
					/* Change the wall to a "solid" wall */
					cave_set_feat(j, i, FEAT_WALL_SOLID);
				}
			}
		}
		cave_set_feat(*y, *x, FEAT_FLOOR);

		return TRUE;
	}

	if ((feat == FEAT_WALL_SOLID) && affectwall)
	{
		/* cannot place tunnel here - use a square to the side */

		/* find usable square and return value in (x,y) */

		i = 50;

		dy = 0;
		dx = 0;
		while ((i > 0) && (cave[*y + dy][*x + dx].feat == FEAT_WALL_SOLID))
		{
			dy = randint0(3) - 1;
			dx = randint0(3) - 1;

			if (!in_bounds(*y + dy, *x + dx))
			{
				dx = 0;
				dy = 0;
			}

			i--;
		}

		if (i == 0)
		{
			/* Failed for some reason: hack - ignore the solidness */
			cave[*y][*x].feat = FEAT_WALL_OUTER;
			dx = 0;
			dy = 0;
		}

		/* Give new, acceptable coordinate. */
		*x = *x + dx;
		*y = *y + dy;

		return FALSE;
	}

	return TRUE;
}


/*
 * This routine creates the catacomb-like tunnels by removing extra rock.
 * Note that this routine is only called on "even" squares - so it gives
 * a natural checkerboard pattern.
 */
static void create_cata_tunnel(int x, int y)
{
	int x1, y1;

	/* Build tunnel */
	x1 = x - 1;
	y1 = y;
	(void)set_tunnel(&x1, &y1, FALSE);

	x1 = x + 1;
	y1 = y;
	(void)set_tunnel(&x1, &y1, FALSE);

	x1 = x;
	y1 = y - 1;
	(void)set_tunnel(&x1, &y1, FALSE);

	x1 = x;
	y1 = y + 1;
	(void)set_tunnel(&x1, &y1, FALSE);
}


/*
 * This routine does the bulk of the work in creating the new types of tunnels.
 * It is designed to use very simple algorithms to go from (x1,y1) to (x2,y2)
 * It doesn't need to add any complexity - straight lines are fine.
 * The SOLID walls are avoided by a recursive algorithm which tries random ways
 * around the obstical until it works.  The number of itterations is counted, and it
 * this gets too large the routine exits. This should stop any crashes - but may leave
 * small gaps in the tunnel where there are too many SOLID walls.
 *
 * Type 1 tunnels are extremely simple - straight line from A to B.  This is only used
 * as a part of the dodge SOLID walls algorithm.
 *
 * Type 2 tunnels are made of two straight lines at right angles. When this is used with
 * short line segments it gives the "cavelike" tunnels seen deeper in the dungeon.
 *
 * Type 3 tunnels are made of two straight lines like type 2, but with extra rock removed.
 * This, when used with longer line segments gives the "catacomb-like" tunnels seen near
 * the surface.
 */
static void short_seg_hack(int x1, int y1, int x2, int y2, int type, int count, bool *fail)
{
	int i, x, y;
	int length;

	/* Check for early exit */
	if (!(*fail)) return;

	length = distance(x1, y1, x2, y2);

	count++;

	if ((type == 1) && (length != 0))
	{

		for (i = 0; i <= length; i++)
		{
			x = x1 + i * (x2 - x1) / length;
			y = y1 + i * (y2 - y1) / length;
			if (!set_tunnel(&x, &y, TRUE))
			{
				if (count > 50)
				{
					/* This isn't working - probably have an infinite loop */
					*fail = FALSE;
					return;
				}

				/* solid wall - so try to go around */
				short_seg_hack(x, y, x1 + (i - 1) * (x2 - x1) / length, y1 + (i - 1) * (y2 - y1) / length, 1, count, fail);
				short_seg_hack(x, y, x1 + (i + 1) * (x2 - x1) / length, y1 + (i + 1) * (y2 - y1) / length, 1, count, fail);
			}
		}
	}
	else if ((type == 2) || (type == 3))
	{
		if (x1 < x2)
		{
			for (i = x1; i <= x2; i++)
			{
				x = i;
				y = y1;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, i - 1, y1, 1, count, fail);
					short_seg_hack(x, y, i + 1, y1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(i, y1);
				}
			}
		}
		else
		{
			for (i = x2; i <= x1; i++)
			{
				x = i;
				y = y1;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, i - 1, y1, 1, count, fail);
					short_seg_hack(x, y, i + 1, y1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(i, y1);
				}
			}

		}
		if (y1 < y2)
		{
			for (i = y1; i <= y2; i++)
			{
				x = x2;
				y = i;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, x2, i - 1, 1, count, fail);
					short_seg_hack(x, y, x2, i + 1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(x2, i);
				}
			}
		}
		else
		{
			for (i = y2; i <= y1; i++)
			{
				x = x2;
				y = i;
				if (!set_tunnel(&x, &y, TRUE))
				{
					/* solid wall - so try to go around */
					short_seg_hack(x, y, x2, i - 1, 1, count, fail);
					short_seg_hack(x, y, x2, i + 1, 1, count, fail);
				}
				if ((type == 3) && ((x + y) % 2))
				{
					create_cata_tunnel(x2, i);
				}
			}
		}
	}
}


/*
 * This routine maps a path from (x1, y1) to (x2, y2) avoiding SOLID walls.
 * Permanent rock is ignored in this path finding- sometimes there is no
 * path around anyway -so there will be a crash if we try to find one.
 * This routine is much like the river creation routine in Zangband.
 * It works by dividing a line segment into two.  The segments are divided
 * until they are less than "cutoff" - when the corresponding routine from
 * "short_seg_hack" is called.
 * Note it is VERY important that the "stop if hit another passage" logic
 * stays as is.  Without this the dungeon turns into Swiss Cheese...
 */
bool build_tunnel2(int x1, int y1, int x2, int y2, int type, int cutoff)
{
	int x3, y3, dx, dy;
	int changex, changey;
	int midval;
	int length;
	int i;
	bool retval, firstsuccede;

	length = distance(x1, y1, x2, y2);

	if (length > cutoff)
	{
		/*
		* Divide path in half and call routine twice.
		*/
		dx = (x2 - x1) / 2;
		dy = (y2 - y1) / 2;

		/* perturbation perpendicular to path */
		changex = (randint0(ABS(dy) + 2) * 2 - ABS(dy) - 1) / 2;

		/* perturbation perpendicular to path */
		changey = (randint0(ABS(dx) + 2) * 2 - ABS(dx) - 1) / 2;

		/* Work out "mid" ponit */
		x3 = x1 + dx + changex;
		y3 = y1 + dy + changey;

		/* See if in bounds - if not - do not perturb point */
		if (!in_bounds(y3, x3))
		{
			x3 = (x1 + x2) / 2;
			y3 = (y1 + y2) / 2;
		}
		/* cache midvalue */
		midval = cave[y3][x3].feat;
		if (midval == FEAT_WALL_SOLID)
		{
			/* move midpoint a bit to avoid problem. */

			i = 50;

			dy = 0;
			dx = 0;
			while ((i > 0) && (cave[y3 + dy][x3 + dx].feat == FEAT_WALL_SOLID))
			{
				dy = randint0(3) - 1;
				dx = randint0(3) - 1;
				if (!in_bounds(y3 + dy, x3 + dx))
				{
					dx = 0;
					dy = 0;
				}
				i--;
			}

			if (i == 0)
			{
				/* Failed for some reason: hack - ignore the solidness */
				cave[y3][x3].feat = FEAT_WALL_OUTER;
				dx = 0;
				dy = 0;
			}
			y3 += dy;
			x3 += dx;
			midval = cave[y3][x3].feat;
		}

		if (midval == FEAT_FLOOR)
		{
			if (build_tunnel2(x1, y1, x3, y3, type, cutoff))
			{
				if ((cave[y3][x3].info & CAVE_ROOM) || (randint1(100) > 95))
				{
					/* do second half only if works + if have hit a room */
					retval = build_tunnel2(x3, y3, x2, y2, type, cutoff);
				}
				else
				{
					/* have hit another tunnel - make a set of doors here */
					retval = FALSE;

					/* Save the door location */
					if (dun->door_n < DOOR_MAX)
					{
						dun->door[dun->door_n].y = y3;
						dun->door[dun->door_n].x = x3;
						dun->door_n++;
					}
				}
				firstsuccede = TRUE;
			}
			else
			{
				/* false- didn't work all the way */
				retval = FALSE;
				firstsuccede = FALSE;
			}
		}
		else
		{
			/* tunnel through walls */
			if (build_tunnel2(x1, y1, x3, y3, type, cutoff))
			{
				retval = build_tunnel2(x3, y3, x2, y2, type, cutoff);
				firstsuccede = TRUE;
			}
			else
			{
				/* false- didn't work all the way */
				retval = FALSE;
				firstsuccede = FALSE;
			}
		}

		if (firstsuccede)
		{
			/* only do this if the first half has worked */
			(void)set_tunnel(&x3, &y3, TRUE);
		}

		/* return value calculated above */
		return retval;
	}
	else
	{
		/* Do a short segment */
		retval = TRUE;
		short_seg_hack(x1, y1, x2, y2, type, 0, &retval);

		/* Hack - ignore return value so avoid infinite loops */
		return TRUE;
	}
}


/*
 * Structure to hold all "fill" data
 */

typedef struct fill_data_type fill_data_type;

struct fill_data_type
{
	/* area size */
	int xmin;
	int ymin;
	int xmax;
	int ymax;

	/* cutoffs */
	int c1;
	int c2;
	int c3;

	/* features to fill with */
	int feat1;
	int feat2;
	int feat3;

	/* number of filled squares */
	int amount;
};

static fill_data_type fill_data;


/* Store routine for the fractal cave generator */
/* this routine probably should be an inline function or a macro. */
static void store_height(int x, int y, int val)
{
	/* if on boundary set val > cutoff so walls are not as square */
	if (((x == fill_data.xmin) || (y == fill_data.ymin) ||
	     (x == fill_data.xmax) || (y == fill_data.ymax)) &&
	    (val <= fill_data.c1)) val = fill_data.c1 + 1;

	/* store the value in height-map format */
	cave[y][x].feat = val;

	return;
}


/*
* Explanation of the plasma fractal algorithm:
*
* A grid of points is created with the properties of a 'height-map'
* This is done by making the corners of the grid have a random value.
* The grid is then subdivided into one with twice the resolution.
* The new points midway between two 'known' points can be calculated
* by taking the average value of the 'known' ones and randomly adding
* or subtracting an amount proportional to the distance between those
* points.  The final 'middle' points of the grid are then calculated
* by averaging all four of the originally 'known' corner points.  An
* random amount is added or subtracted from this to get a value of the
* height at that point.  The scaling factor here is adjusted to the
* slightly larger distance diagonally as compared to orthogonally.
*
* This is then repeated recursively to fill an entire 'height-map'
* A rectangular map is done the same way, except there are different
* scaling factors along the x and y directions.
*
* A hack to change the amount of correlation between points is done using
* the grd variable.  If the current step size is greater than grd then
* the point will be random, otherwise it will be calculated by the
* above algorithm.  This makes a maximum distance at which two points on
* the height map can affect each other.
*
* How fractal caves are made:
*
* When the map is complete, a cut-off value is used to create a cave.
* Heights below this value are "floor", and heights above are "wall".
* This also can be used to create lakes, by adding more height levels
* representing shallow and deep water/ lava etc.
*
* The grd variable affects the width of passages.
* The roug variable affects the roughness of those passages
*
* The tricky part is making sure the created cave is connected.  This
* is done by 'filling' from the inside and only keeping the 'filled'
* floor.  Walls bounding the 'filled' floor are also kept.  Everything
* else is converted to the normal granite FEAT_WALL_EXTRA.
 */


/*
 *  Note that this uses the cave.feat array in a very hackish way
 *  the values are first set to zero, and then each array location
 *  is used as a "heightmap"
 *  The heightmap then needs to be converted back into the "feat" format.
 *
 *  grd=level at which fractal turns on.  smaller gives more mazelike caves
 *  roug=roughness level.  16=normal.  higher values make things more convoluted
 *    small values are good for smooth walls.
 *  size=length of the side of the square cave system.
 */
void generate_hmap(int y0, int x0, int xsiz, int ysiz, int grd, int roug, int cutoff)
{
	int xhsize, yhsize, xsize, ysize, maxsize;

	/*
	 * fixed point variables- these are stored as 256 x normal value
	 * this gives 8 binary places of fractional part + 8 places of normal part
	 */

	u16b xstep, xhstep, ystep, yhstep;
	u16b xstep2, xhstep2, ystep2, yhstep2;
	u16b i, j, ii, jj, diagsize, xxsize, yysize;

	/* Cache for speed */
	u16b xm, xp, ym, yp;

	/* redefine size so can change the value if out of range */
	xsize = xsiz;
	ysize = ysiz;

	/* Paranoia about size of the system of caves */
	if (xsize > 254) xsize = 254;
	if (xsize < 4) xsize = 4;
	if (ysize > 254) ysize = 254;
	if (ysize < 4) ysize = 4;

	/* get offsets to middle of array */
	xhsize = xsize / 2;
	yhsize = ysize / 2;

	/* fix rounding problem */
	xsize = xhsize * 2;
	ysize = yhsize * 2;

	/* get limits of region */
	fill_data.xmin = x0 - xhsize;
	fill_data.ymin = y0 - yhsize;
	fill_data.xmax = x0 + xhsize;
	fill_data.ymax = y0 + yhsize;

	/* Store cutoff in global for quick access */
	fill_data.c1 = cutoff;

	/*
	* Scale factor for middle points:
	* About sqrt(2) * 256 - correct for a square lattice
	* approximately correct for everything else.
	*/
	diagsize = 362;

	/* maximum of xsize and ysize */
	maxsize = (xsize > ysize) ? xsize : ysize;

	/* Clear the section */
	for (i = 0; i <= xsize; i++)
	{
		for (j = 0; j <= ysize; j++)
		{
			/* 255 is a flag for "not done yet" */
			cave[(int)(fill_data.ymin + j)][(int)(fill_data.xmin + i)].feat = 255;
			/* Clear icky flag because may be redoing the cave */
			cave[(int)(fill_data.ymin + j)][(int)(fill_data.xmin + i)].info &= ~(CAVE_ICKY);
		}
	}

	/* Boundaries are walls */
	cave[fill_data.ymin][fill_data.xmin].feat = maxsize;
	cave[fill_data.ymax][fill_data.xmin].feat = maxsize;
	cave[fill_data.ymin][fill_data.xmax].feat = maxsize;
	cave[fill_data.ymax][fill_data.xmax].feat = maxsize;

	/* Set the middle square to be an open area. */
	cave[y0][x0].feat = 0;

	/* Initialize the step sizes */
	xstep = xhstep = xsize * 256;
	ystep = yhstep = ysize * 256;
	xxsize = xsize * 256;
	yysize = ysize * 256;

	/*
	 * Fill in the rectangle with fractal height data -
	 * like the 'plasma fractal' in fractint.
	 */
	while ((xhstep > 256) || (yhstep > 256))
	{
		/* Halve the step sizes */
		xstep = xhstep;
		xhstep /= 2;
		ystep = yhstep;
		yhstep /= 2;

		/* cache well used values */
		xstep2 = xstep / 256;
		ystep2 = ystep / 256;

		xhstep2 = xhstep / 256;
		yhstep2 = yhstep / 256;

		/* middle top to bottom. */
		for (i = xhstep; i <= xxsize - xhstep; i += xstep)
		{
			for (j = 0; j <= yysize; j += ystep)
			{
				/* cache often used values */
				ii = i / 256 + fill_data.xmin;
				jj = j / 256 + fill_data.ymin;

				/* Test square */
				if (cave[jj][ii].feat == 255)
				{
					if (xhstep2 > grd)
					{
						/* If greater than 'grid' level then is random */
						store_height(ii, jj, randint1(maxsize));
					}
			  	 	else
					{
						/* Average of left and right points +random bit */
						store_height(ii, jj,
							(cave[jj][fill_data.xmin + (i - xhstep) / 256].feat
							 + cave[jj][fill_data.xmin + (i + xhstep) / 256].feat) / 2
							 + (randint1(xstep2) - xhstep2) * roug / 16);
					}
				}
			}
		}


		/* middle left to right. */
		for (j = yhstep; j <= yysize - yhstep; j += ystep)
		{
			for (i = 0; i <= xxsize; i += xstep)
		   	{
				/* cache often used values */
				ii = i / 256 + fill_data.xmin;
				jj = j / 256 + fill_data.ymin;

				/* Test square */
				if (cave[jj][ii].feat == 255)
				{
					if (xhstep2 > grd)
					{
						/* If greater than 'grid' level then is random */
						store_height(ii, jj, randint1(maxsize));
					}
		   			else
					{
						/* Average of up and down points +random bit */
						store_height(ii, jj,
							(cave[fill_data.ymin + (j - yhstep) / 256][ii].feat
							+ cave[fill_data.ymin + (j + yhstep) / 256][ii].feat) / 2
							+ (randint1(ystep2) - yhstep2) * roug / 16);
					}
				}
			}
		}

		/* center. */
		for (i = xhstep; i <= xxsize - xhstep; i += xstep)
		{
			for (j = yhstep; j <= yysize - yhstep; j += ystep)
			{
			   	/* cache often used values */
				ii = i / 256 + fill_data.xmin;
				jj = j / 256 + fill_data.ymin;

				/* Test square */
				if (cave[jj][ii].feat == 255)
				{
					if (xhstep2 > grd)
					{
						/* If greater than 'grid' level then is random */
						store_height(ii, jj, randint1(maxsize));
					}
		   			else
					{
						/* Cache reused values. */
						xm = fill_data.xmin + (i - xhstep) / 256;
						xp = fill_data.xmin + (i + xhstep) / 256;
						ym = fill_data.ymin + (j - yhstep) / 256;
						yp = fill_data.ymin + (j + yhstep) / 256;

						/*
						 * Average over all four corners + scale by diagsize to
						 * reduce the effect of the square grid on the shape of the fractal
						 */
						store_height(ii, jj,
							(cave[ym][xm].feat + cave[yp][xm].feat
							+ cave[ym][xp].feat + cave[yp][xp].feat) / 4
							+ (randint1(xstep2) - xhstep2) * (diagsize / 16) / 256 * roug);
					}
				}
			}
		}
	}
}


static bool hack_isnt_wall(int y, int x, int c1, int c2, int c3,
	 int feat1, int feat2, int feat3)
{
	cave_type *c_ptr = &cave[y][x];
	/*
	 * function used to convert from height-map back to the
	 *  normal angband cave format
	 */
	if (c_ptr->info & CAVE_ICKY)
	{
		/* already done */
		return FALSE;
	}
	else
	{
		/* Show that have looked at this square */
		c_ptr->info |= (CAVE_ICKY);

		/* Use cutoffs c1-c3 to allocate regions of floor /water/ lava etc. */
		if (c_ptr->feat <= c1)
		{
			/* 25% of the time use the other tile : it looks better this way */
			if (randint1(100) < 75)
			{
				c_ptr->feat = feat1;
				return TRUE;
			}
			else
			{
				c_ptr->feat = feat2;
				return TRUE;
			}
		}
		else if (c_ptr->feat <= c2)
		{
			/* 25% of the time use the other tile : it looks better this way */
			if (randint1(100) < 75)
			{
				c_ptr->feat = feat2;
				return TRUE;
			}
			else
			{
				c_ptr->feat = feat1;
				return TRUE;
			}
		}
		else if (c_ptr->feat <= c3)
		{
			c_ptr->feat = feat3;
			return TRUE;
		}
		
		/* if greater than cutoff then is a wall */
		else
		{
			c_ptr->feat = FEAT_WALL_OUTER;
			return FALSE;
		}
	}
}


/*
 * Fill the fractal height-map using the features specified in in fill_data.
 *
 * This routine is similar to the method used to update the monster flow
 * information.  It uses the temp grids as a circular queue.
 */
static void cave_fill(int y, int x)
{
	int i, j, d;
	int ty, tx;
		
	int flow_tail = 1;
	int flow_head = 0;
	
	
	/*** Start Grid ***/

	/* Enqueue that entry */
	temp_y[0] = y;
	temp_x[0] = x;
	
	
	/* Now process the queue */
	while (flow_head != flow_tail)
	{
		/* Extract the next entry */
		ty = temp_y[flow_head];
		tx = temp_x[flow_head];

		/* Forget that entry */
		if (++flow_head == TEMP_MAX) flow_head = 0;

		/* Add the "children" */
		for (d = 0; d < 8; d++)
		{
			int old_head = flow_tail;

			/* Child location */
			j = ty + ddy_ddd[d];
			i = tx + ddx_ddd[d];
			
			/* Paranoia Don't leave the cave */
			if (!in_bounds(j, i)) return;

			/* If within bounds */
			if ((i > fill_data.xmin) && (i < fill_data.xmax)
				&& (j > fill_data.ymin) && (j < fill_data.ymax))
			{
				/* If not a wall or floor done before */
				if (hack_isnt_wall(j, i,
					fill_data.c1, fill_data.c2, fill_data.c3,
					fill_data.feat1, fill_data.feat2, fill_data.feat3))
		 		{
					
					/* Enqueue that entry */
					temp_y[flow_tail] = j;
					temp_x[flow_tail] = i;

					/* Advance the queue */
					if (++flow_tail == TEMP_MAX) flow_tail = 0;

					/* Hack -- Overflow by forgetting new entry */
					if (flow_tail == flow_head)
					{
						flow_tail = old_head;
					}
					else
					{
						/* keep tally of size of cave system */
						(fill_data.amount)++;
					}
				}
			}
			else
			{
				/* affect boundary */
				cave[j][i].info |= CAVE_ICKY;
			}
		}
	}
}


bool generate_fracave(int y0, int x0, int xsize, int ysize, int cutoff,
	 bool light)
{
	int x, y, i, xhsize, yhsize;
	cave_type *c_ptr;
	byte info;

	/* offsets to middle from corner */
	xhsize = xsize / 2;
	yhsize = ysize / 2;


	/*
	 * select region connected to center of cave system
	 * this gets rid of alot of isolated one-sqaures that
	 * can make teleport traps instadeaths...
	 */

	/* cutoffs */
	fill_data.c1 = cutoff;
	fill_data.c2 = 0;
	fill_data.c3 = 0;

	/* features to fill with */
	fill_data.feat1 = FEAT_FLOOR;
	fill_data.feat2 = FEAT_FLOOR;
	fill_data.feat3 = FEAT_FLOOR;

	/* number of filled squares */
	fill_data.amount = 0;

	cave_fill(y0, x0);

	/* if tally too small, try again */
	if (fill_data.amount < 10)
	{
		/* too small - clear area and try again later */
		
		/* Clear the height map */
		generate_fill(y0 - yhsize , x0 - xhsize,
			 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1, FEAT_WALL_EXTRA);
		
		/* Clear the icky flag */
		clear_vault(y0 - yhsize , x0 - xhsize,
			 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1);		
		
		/* Try again */
		return FALSE;
	}
	
	/* Get the cave info value to logical OR to the grids */
	info = CAVE_ROOM;
	if (light) info |= CAVE_GLOW; 
	
	
	/*
	 * Do boundarys-check to see if they are next to a filled region
	 * If not then they are set to normal granite
	 * If so then they are marked as room walls.
	 */
	for (i = 0; i <= xsize; ++i)
	{
		/* top boundary */
		c_ptr = &cave[0 + y0 - yhsize][i + x0 - xhsize];
		
		if (c_ptr->info & CAVE_ICKY)
		{
			/* Next to a 'filled' region? - set to be room walls */
			c_ptr->feat = FEAT_WALL_OUTER;
			c_ptr->info |= info;
		}
		else
		{
			/* set to be normal granite */
			c_ptr->feat = FEAT_WALL_EXTRA;
		}	

		/* bottom boundary */
		c_ptr = &cave[ysize + y0 - yhsize][i + x0 - xhsize];
		if (c_ptr->info & CAVE_ICKY)
		{
			/* Next to a 'filled' region? - set to be room walls */
			c_ptr->feat = FEAT_WALL_OUTER;
			c_ptr->info |= info;
		}
		else
		{
			/* set to be normal granite */
			c_ptr->feat = FEAT_WALL_EXTRA;
		}
	}

	/* Do the left and right boundaries minus the corners (done above) */
	for (i = 1; i < ysize; ++i)
	{
		/* left boundary */
		c_ptr = &cave[i + y0 - yhsize][0 + x0 - xhsize];
		
		if (c_ptr->info & CAVE_ICKY)
		{
			/* room boundary */
			c_ptr->feat = FEAT_WALL_OUTER;
			c_ptr->info |= info;
		}
		else
		{
			/* outside room */
			c_ptr->feat = FEAT_WALL_EXTRA;
		}

		/* right boundary */
		c_ptr = &cave[i + y0 - yhsize][xsize + x0 - xhsize];
		if (c_ptr->info & CAVE_ICKY)
		{
			/* room boundary */
			c_ptr->feat = FEAT_WALL_OUTER;
			c_ptr->info |= info;
		}
		else
		{
			/* outside room */
			c_ptr->feat = FEAT_WALL_EXTRA;
		}
	}


	/* Do the rest: convert back to the normal format */
	for (x = 1; x < xsize; ++x)
	{
		for (y = 1; y < ysize; ++y)
		{
			c_ptr = &cave[y0 + y - yhsize][x0 + x - xhsize];
			
			if (!(c_ptr->info & CAVE_ICKY))
			{
				/* Clear the unconnected regions */
				c_ptr->feat = FEAT_WALL_EXTRA;
			}
			else
			{
				/* Is part of the cave */
				c_ptr->info |= info;
			}
		}
	}
	
	/* Clear the icky flag */
	clear_vault(y0 - yhsize , x0 - xhsize,
		 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1);

	/*
	 * XXX XXX XXX There is a slight problem when tunnels pierce the caves:
	 * Extra doors appear inside the system.  (Its not very noticeable
	 * though.) This can be removed by "filling" from the outside in.
	 * This allows a separation from FEAT_WALL_OUTER with FEAT_WALL_INNER.
	 * (Internal walls are  F.W.OUTER instead.) The extra effort for what
	 * seems to be only a minor thing (even non-existant if you think of the
	 * caves not as normal rooms, but as holes in the dungeon), doesn't seem
	 * worth it.
	 */

	return TRUE;
}


bool generate_lake(int y0, int x0, int xsize, int ysize,
	 int c1, int c2, int c3, int type)
{
	int x, y, xhsize, yhsize;
	int feat1, feat2, feat3;

	cave_type *c_ptr;

	/* offsets to middle from corner */
	xhsize = xsize / 2;
	yhsize = ysize / 2;

	/* Get features based on type */
	switch (type)
	{
		case LAKE_LAVA:
		{
			/* Lava */
			feat1 = FEAT_DEEP_LAVA;
			feat2 = FEAT_SHAL_LAVA;
			feat3 = FEAT_FLOOR;
			break;
		}
		
		case LAKE_WATER:
		{
			/* Water */
			feat1 = FEAT_DEEP_WATER;
			feat2 = FEAT_SHAL_WATER;
			feat3 = FEAT_FLOOR;
			break;
		}
		
		case LAKE_DESTROY:
		{
			/* Collapsed cave */
			feat1 = FEAT_FLOOR;
			feat2 = FEAT_FLOOR;
			feat3 = FEAT_RUBBLE;
			break;
		}
		
		case LAKE_EEARTH:
		{
			/* Earth vault */
			feat1 = FEAT_RUBBLE;
			feat2 = FEAT_FLOOR;
			feat3 = FEAT_RUBBLE;
			break;
		}
		
		case LAKE_EAIR:
		{
			/* Air vault */
			feat1 = FEAT_FLOOR;
			feat2 = FEAT_TREES;
			feat3 = FEAT_FLOOR;
			break;
		}
		
		case LAKE_EWATER:
		{
			/* Water vault */
			feat1 = FEAT_SHAL_WATER;
			feat2 = FEAT_DEEP_WATER;
			feat3 = FEAT_SHAL_WATER;
			break;
		}
		
		case LAKE_EFIRE:
		{
			/* Fire Vault */
			feat1 = FEAT_SHAL_LAVA;
			feat2 = FEAT_DEEP_LAVA;
			feat3 = FEAT_SHAL_LAVA;
			break;
		}
		
		case LAKE_CAVERN:
		{
			/* Cavern */
			feat1 = FEAT_FLOOR;
			feat2 = FEAT_FLOOR;
			feat3 = FEAT_FLOOR;
			break;
		}
		
		case LAKE_RUBBLE:
		{
			/* Rubble everywhere */
			feat1 = FEAT_RUBBLE;
			feat2 = FEAT_RUBBLE;
			feat3 = FEAT_FLOOR;
			break;
		}
		
		case LAKE_SAND:
		{
			/* Sand everywhere */
			feat1 = FEAT_SAND;
			feat2 = FEAT_SAND;
			feat3 = FEAT_FLOOR;
			break;		
		}
		
		case LAKE_ROCK:
		{
			/* Rock formation */
			feat1 = FEAT_WALL_INNER;
			feat2 = FEAT_WALL_INNER;
			feat3 = FEAT_FLOOR;
			break;
		}
		
		/* Paranoia */
		default: return FALSE;
	}

	/*
	 * select region connected to center of cave system
	 * this gets rid of alot of isolated one-sqaures that
	 * can make teleport traps instadeaths...
	 */

	/* cutoffs */
	fill_data.c1 = c1;
	fill_data.c2 = c2;
	fill_data.c3 = c3;

	/* features to fill with */
	fill_data.feat1 = feat1;
	fill_data.feat2 = feat2;
	fill_data.feat3 = feat3;

	/* number of filled squares */
	fill_data.amount = 0;

	/* select region connected to center of cave system
	* this gets rid of alot of isolated one-sqaures that
	* can make teleport traps instadeaths... */
	cave_fill(y0, x0);

	/* if tally too small, try again */
	if (fill_data.amount < 3)
	{
		/* too small -clear area and try again later */
		
		/* Clear the height map */
		generate_fill(y0 - yhsize , x0 - xhsize,
			 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1, FEAT_WALL_EXTRA);
		
		/* Clear the icky flag */
		clear_vault(y0 - yhsize , x0 - xhsize,
			 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1);
			 
		/* Try again */
		return FALSE;
	}

	/* Do boundarys- set to normal granite */
	generate_draw(y0 - yhsize , x0 - xhsize,
		y0 - yhsize + ysize, x0 - xhsize + xsize, FEAT_WALL_EXTRA);

	/* Do the rest: convert back to the normal format */
	for (x = 1; x < xsize; ++x)
	{
		for (y = 1; y < ysize; ++y)
		{
			c_ptr = &cave[y0 + y - yhsize][x0 + x - xhsize];
			
			/* Fill unconnected regions with granite */
			if ((!(c_ptr->info & CAVE_ICKY)) || (c_ptr->feat == FEAT_WALL_OUTER))
			{
				c_ptr->feat = FEAT_WALL_EXTRA;
			}

			/* Light lava and trees */
			if ((c_ptr->feat == FEAT_DEEP_LAVA) ||
				(c_ptr->feat == FEAT_SHAL_LAVA) ||
				(c_ptr->feat == FEAT_TREES))
			{
				c_ptr->info |= CAVE_GLOW;
			}
		}
	}

	/* Clear the icky flag */
	clear_vault(y0 - yhsize , x0 - xhsize,
		 y0 - yhsize + ysize - 1, x0 - xhsize + xsize - 1);

	/* Done */
	return TRUE;
}
