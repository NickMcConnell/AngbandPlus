/* File: genfeat.c */

/* Purpose: Dungeon feature generation */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/* Prfnoff -- reorganized generate.c into generate.c, genroom.c, genfeat.c, wild.c */
#include "angband.h"


/*
 * Convert existing terrain type to floor
 */
void place_floor(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create floor */
	c_ptr->feat = cave_get_feat(PV_FLOOR_NORMAL, FF1_FLOOR);
}


/*
 * Convert existing terrain type to rubble
 */
void place_rubble(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create rubble */
	c_ptr->feat = cave_get_feat(PV_BLOCK_RUBBLE, FF1_WALL);
}



/*
 * Convert existing terrain type to "up stairs"
 */
void place_up_stairs(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create up stairs */
	c_ptr->feat = cave_get_feat(PV_STAIR_NORMAL, FF1_STAIR);
}


/*
 * Convert existing terrain type to "down stairs"
 */
static void place_down_stairs(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create down stairs */
	c_ptr->feat = cave_get_feat(PV_STAIR_NORMAL, (FF1_STAIR | FF1_HARD));
}


/*
 * Place an up/down staircase at given location
 */
void place_random_stairs(int y, int x)
{
	bool up_stairs = TRUE;
	bool down_stairs = TRUE;

	/* Paranoia */
	if (!cave_clean_bold(y, x)) return;

	/* Town */
	if (!dun_level)
		up_stairs = FALSE;

	/* Ironman */
	if (ironman_downward)
		up_stairs = FALSE;

	/* Bottom */
	if (dun_level >= MAX_DEPTH - 1)
		down_stairs = FALSE;

	/* Quest-level */
	if (quest_number(dun_level) && (dun_level > 1))
		down_stairs = FALSE;

	/* We can't place both */
	if (down_stairs && up_stairs)
	{
		/* Choose a staircase randomly */
		if (rand_int(100) < 50)
			up_stairs = FALSE;
		else
			down_stairs = FALSE;
	}

	/* Place the stairs */
	if (up_stairs)
		place_up_stairs(y, x);
	else if (down_stairs)
		place_down_stairs(y, x);
}


/*
 * Place a locked door at the given location
 */
void place_locked_door(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create locked door */
	c_ptr->feat = cave_get_feat(randint(PV_DOOR_MAX-1), FF1_DOOR);
}


/*
 * Place a secret door at the given location
 */
void place_secret_door(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create secret door */
	c_ptr->feat = cave_get_feat(PV_BLOCK_SECRET, FF1_WALL);
}


/*
 * Place a random type of door at the given location
 */
void place_random_door(int y, int x)
{
	int tmp;

	/* Choose an object */
	tmp = rand_int(1000);

	/* Open doors (300/1000) */
	if (tmp < 300)
	{
		/* Create open door */
		cave_pick_feat(y, x, PV_DOOR_OPEN, FF1_OPEN);
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		cave_pick_feat(y, x, PV_DOOR_BROKEN, FF1_OPEN);
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		cave_pick_feat(y, x, PV_BLOCK_SECRET, FF1_WALL);
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
	u32b flags1 = FF1_DOOR; /* Prfnoff */
	s16b pval; /* Prfnoff */

	/* Choose an object */
	tmp = rand_int(400);

	/* Closed doors (300/400) */
	if (tmp < 300)
	{
		/* Create closed door */
		pval = 0;
	}

	/* Locked doors (99/400) */
	else if (tmp < 399)
	{
		/* Create locked door */
		pval = randint(PV_DOOR_MAX - 1);
	}

	/* Stuck doors (1/400) */
	else
	{
		/* Create jammed door */
		pval = rand_int(PV_DOOR_MAX);
		flags1 |= FF1_HARD;
	}

	/* Set the door -- Prfnoff */
	cave_pick_feat(y, x, pval, flags1);
}


/*
 * Convert existing terrain type to "quartz vein"
 */
void place_quartz_vein(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create quartz vein */
	c_ptr->feat = cave_get_feat(PV_VEIN_NO_GOLD, (FF1_VEIN | FF1_HARD));
}


/*
 * Convert existing terrain type to "magma vein"
 */
void place_magma_vein(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create magma vein */
	c_ptr->feat = cave_get_feat(PV_VEIN_NO_GOLD, FF1_VEIN);
}


/*
 * Convert existing terrain type to "inner wall"
 */
void place_inner_wall(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create inner wall */
	c_ptr->feat = cave_get_feat(PV_WALL_INNER, (FF1_WALL | FF1_GRANITE));
}


/*
 * Convert existing terrain type to "outer wall"
 */
void place_outer_wall(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create outer wall */
	c_ptr->feat = cave_get_feat(PV_WALL_OUTER, (FF1_WALL | FF1_GRANITE));
}


/*
 * Convert existing terrain type to "extra wall"
 */
void place_extra_wall(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create extra wall */
	c_ptr->feat = cave_get_feat(PV_WALL_EXTRA, (FF1_WALL | FF1_GRANITE));
}


/*
 * Convert existing terrain type to "solid wall"
 */
void place_solid_wall(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Create extra wall */
	c_ptr->feat = cave_get_feat(PV_WALL_SOLID, (FF1_WALL | FF1_GRANITE));
}


/*
 * Convert existing terrain type to "permanent wall (solid)"
 */
void place_solid_perm(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Clear previous contents, add "solid" perma-wall */
	c_ptr->feat = cave_get_feat(PV_PERMA_SOLID, (FF1_WALL | FF1_PERMA));
}


/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(y, x)"
 *
 * We count only granite walls and permanent walls.
 * (And veins, too. -- Prfnoff)
 */
static int next_to_walls(int y, int x)
{
	int	k = 0;

	if (cave_realwall_bold(y+1, x)) k++;
	if (cave_realwall_bold(y-1, x)) k++;
	if (cave_realwall_bold(y, x+1)) k++;
	if (cave_realwall_bold(y, x-1)) k++;

	return (k);
}


/*
 * Places some staircases near walls
 */
void alloc_stairs(u32b flags1, int num, int walls)
{
	int         y, x, i, j, flag;
	cave_type   *c_ptr;
	byte        feat; /* Prfnoff */

	if (flags1 & FF1_HARD)
	{
		/* No downstairs on quest levels */
		if ((dun_level > 1) && quest_number(dun_level)) return;

		/* No downstairs at the bottom */
		if (dun_level >= MAX_DEPTH - 1) return;
	}
	else
	{
		/* No up stairs in town or in ironman mode */
		if (ironman_downward || !dun_level) return;
	}

	feat = cave_get_feat(PV_STAIR_NORMAL, flags1);

	/* Place "num" stairs */
	for (i = 0; i < num; i++)
	{
		/* Place some stairs */
		for (flag = FALSE; !flag; )
		{
			/* Try several times, then decrease "walls" */
			for (j = 0; !flag && j <= 3000; j++)
			{
				/* Pick a random grid */
				y = rand_int(cur_hgt);
				x = rand_int(cur_wid);

				/* Require "naked" floor grid */
				if (!cave_naked_bold(y, x)) continue;

				/* Require floor */
				if (!cave_plain_bold(y, x)) continue; /* Prfnoff */

				/* Require a certain number of adjacent walls */
				if (next_to_walls(y, x) < walls) continue;

				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Clear previous contents, add stairs */
				c_ptr->feat = feat;

				/* All done */
				flag = TRUE;
			}

			/* Require fewer walls */
			if (walls) walls--;
		}
	}
}


/*
 * Allocates some objects (using "place" and "type")
 */
void alloc_object(int set, int typ, int num)
{
	int y, x, k;
	int dummy = 0;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (dummy < SAFE_MAX_ATTEMPTS)
		{
			bool room;

			dummy++;

			/* Location */
			y = rand_int(cur_hgt);
			x = rand_int(cur_wid);

			/* Require "naked" floor grid */
			if (!cave_naked_bold(y, x) &&
			    !((y == py) && (x == px))) continue;

			/* Check for "room" */
			room = (cave[y][x].info & CAVE_ROOM) ? TRUE : FALSE;

			/* Require corridor? */
			if ((set == ALLOC_SET_CORR) && room) continue;

			/* Require room? */
			if ((set == ALLOC_SET_ROOM) && !room) continue;

			/* Accept it */
			break;
		}

		if (dummy >= SAFE_MAX_ATTEMPTS)
		{
			if (cheat_room)
			{
				msg_print("Warning! Could not place object!");
			}
			return;
		}


		/* Place something */
		switch (typ)
		{
			case ALLOC_TYP_RUBBLE:
			{
				place_rubble(y, x);
				break;
			}

			case ALLOC_TYP_TRAP:
			{
				place_trap(y, x);
				break;
			}

			case ALLOC_TYP_GOLD:
			{
				place_gold(y, x);
				break;
			}

			case ALLOC_TYP_OBJECT:
			{
				place_object(y, x, FALSE, FALSE);
				break;
			}
		}
	}
}


/*
 * Make a grid part of a room -- Prfnoff
 */
void make_room_grid(int y, int x, bool light, bool icky)
{
	cave_type *c_ptr = &cave[y][x];

	c_ptr->info |= (CAVE_ROOM);
	if (light) c_ptr->info |= (CAVE_GLOW);
	if (icky) c_ptr->info |= (CAVE_ICKY);
}


/*
 * Destroy a grid's "room" attributes -- Prfnoff
 */
void wipe_room_grid(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* No longer part of a room or vault */
	c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

	/* No longer illuminated or known */
	c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);
}


/*
 * Make an empty square floor, for the middle of rooms
 */
static void place_room_floor(int x1, int x2, int y1, int y2, bool light)
{
	int x, y;

	/* Place a full floor under the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		for (x = x1 - 1; x <= x2 + 1; x++)
		{
			place_floor(y, x);
			make_room_grid(y, x, light, FALSE);
		}
	}
}


/*
 * Make an empty square room, only floor and wall grids
 */
void place_room(int x1, int x2, int y1, int y2, bool light)
{
	int y, x;

	place_room_floor(x1, x2, y1, y2, light);

	/* Walls around the room */
	for (y = y1 - 1; y <= y2 + 1; y++)
	{
		place_outer_wall(y, x1 - 1);
		place_outer_wall(y, x2 + 1);
	}
	for (x = x1 - 1; x <= x2 + 1; x++)
	{
		place_outer_wall(y1 - 1, x);
		place_outer_wall(y2 + 1, x);
	}
}



/* Function that sees if a square is a floor.  (Includes range checking.) */
bool get_is_floor(int x, int y)
{
	if (!in_bounds(y, x))
	{
		/* Out of bounds */
		return (FALSE);
	}

	/* Do the real check */
	if (!cave_plain_bold(y, x)) return (TRUE); /* Prfnoff */

	return (FALSE);
}


/* Set a square to be floor.  (Includes range checking.) */
void set_floor(int x, int y)
{
	if (!in_bounds(y, x))
	{
		/* Out of bounds */
		return;
	}

	if (cave[y][x].info & CAVE_ROOM)
	{
		/* A room border don't touch. */
		return;
	}

	/* Set to be floor if is a wall (don't touch lakes). */
	if (cave_granite_bold(y, x) && (cave_pval_bold(y, x) == PV_WALL_EXTRA))
		place_floor(y, x);
}



/*
 * Create up to "num" objects near the given coordinates
 * Only really called by some of the "vault" routines.
 */
void vault_objects(int y, int x, int num)
{
	int dummy = 0;
	int i = 0, j = y, k = x;


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
			if (!cave_clean_bold(j, k)) continue;

			/* Place an item */
			if (rand_int(100) < 75)
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
		if (!cave_naked_bold(y1, x1)) continue;

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

	/* Try to summon "num" monsters "near" the given location */
	for (k = 0; k < num; k++)
	{
		/* Try nine locations */
		for (i = 0; i < 9; i++)
		{
			int d = 1;

			/* Pick a nearby location */
			scatter(&y, &x, y1, x1, d, 0);

			/* Require "empty" floor grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Place the monster (allow groups) */
			monster_level = base_level + 2;
			(void)place_monster(y, x, TRUE, TRUE);
			monster_level = base_level;
		}
	}
}
