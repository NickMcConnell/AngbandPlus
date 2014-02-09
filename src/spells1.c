/* File: spells1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
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
#include "cmds.h"

#define MAX_DAMAGE	1600


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 */
bool teleport_away(int m_idx, int dis)
{
	monster_type *m_ptr = &mon_list[m_idx];

	int my = m_ptr->fy;
	int mx = m_ptr->fx;

	byte x_location_tables [MAX_DUNGEON_AREA];
	byte y_location_tables [MAX_DUNGEON_AREA];
	int spot_counter = 0;

	int d, d1, i, min, y, x;

	/* Paranoia */
	if (!m_ptr->r_idx) return (FALSE);

	/* Minimum distance */
	min = dis / 2;

	/* Gauge the distance from the player to the 4 corners of the dungeon, take the highest*/
	d = distance(my, mx, 1, 1);
	d1 = distance(my, mx, p_ptr->cur_map_hgt-1, 1);
	if (d1 > d) d = d1;
	d1 = distance(my, mx, 1, p_ptr->cur_map_wid-1);
	if (d1 > d) d = d1;
	d1 = distance(my, mx, p_ptr->cur_map_hgt-11, p_ptr->cur_map_wid-1);
	if (d1 > d) d = d1;

	/* start with a realistic range*/
	if (dis > d) dis = d;

	/*must have a realistic minimum*/
	if (min > (d * 4 / 10))
	{
		min = (d * 4 / 10);
	}

	/* Look for a spot */
	while (TRUE)
	{
		u32b min_squared = min * min;
		u32b dis_squared = dis * dis;
		int y_min = my - dis;
		int y_max = my + dis;
		int x_min = mx - dis;
		int x_max = mx + dis;

		/* Boundry control */
		if (x_min < 0) x_min = 0;
		if (y_min < 0) y_min = 0;
		if (x_max > p_ptr->cur_map_wid) x_max = p_ptr->cur_map_wid;
		if (y_max > p_ptr->cur_map_hgt) y_max = p_ptr->cur_map_hgt;

		/* Analyze the actual map */
		for (y = y_min; y < y_max; y++)
		{
			for (x = x_min; x < x_max; x++)
			{

				u32b dist_squared;

				/* Require "start" floor space */
				if (!cave_empty_bold(y, x)) continue;

				/* No teleporting into vaults and such */
				if (cave_info[y][x] & (CAVE_ICKY)) continue;

				/* Use pythagorean theorem to ensure the distance is right */
				dist_squared = (((mx - x) * (mx - x)) +  ((my - y) * (my - y)));

				/* Stay within the min and the max */
				if (dist_squared <= min_squared) continue;
				if (dist_squared > dis_squared) continue;

				x_location_tables[spot_counter] = x;
				y_location_tables[spot_counter] = y;

				/*increase the counter*/
				spot_counter++;
			}
		}

		/*we have at least one random spot*/
		if (spot_counter) break;

		/* Make sure we aren't trapped in an infinite loop */
		if ((!min) && (dis == d))
		{
			return (FALSE);
		}

		/* Increase the maximum distance */
		dis = dis * 2 + 1;
		if (dis > d) dis = d;

		/* Decrease the minimum distance */
		min = min * 6 / 10;

	}

	i = randint0(spot_counter);

	/* Mark the location */
	x = x_location_tables[i];
	y = y_location_tables[i];

	/* Sound */
	sound(MSG_TPOTHER);

	/*the monster should re-evaluate their target*/
	m_ptr->target_y = 0;
	m_ptr->target_x = 0;

	/* Swap the monsters */
	monster_swap(my, mx, y, x);

	return (TRUE);
}

/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 *
 * When native is true, player must be standing on native terrain, and land on native terrain.
 */
bool teleport_player(int dis, bool native)
{
	byte x_location_tables [MAX_DUNGEON_AREA];
	byte y_location_tables [MAX_DUNGEON_AREA];
	int spot_counter = 0;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int d, d1, i, min, y, x;

	u32b flags = cave_ff3_match(py, px, TERRAIN_MASK);

	/* First, take damage from terrain */
	process_player_terrain_damage();

	/* Player could have died. */
	if (p_ptr->is_dead) return (FALSE);

	if (native)
	{
		/* Check terrain */
		if (!flags || !is_player_native(py, px))
		{
			msg_print("You must be standing over native terrain!");
			return (FALSE);
		}
	}

	/* Minimum distance */
	min = dis / 2;

	/* Gauge the distance from the player to the 4 corners of the dungeon, take the highest*/
	d = distance(py, px, 1, 1);
	d1 = distance(py, px, p_ptr->cur_map_hgt-1, 1);
	if (d1 > d) d = d1;
	d1 = distance(py, px, 1, p_ptr->cur_map_wid-1);
	if (d1 > d) d = d1;
	d1 = distance(py, px, p_ptr->cur_map_hgt-11, p_ptr->cur_map_wid-1);
	if (d1 > d) d = d1;

	/* start with a realistic range*/
	if (dis > d) dis = d;

	/*must have a realistic minimum*/
	if (min > (d * 4 / 10))
	{
		min = (d * 4 / 10);
	}

	/* Look for a spot */
	while (TRUE)
	{
		u32b min_squared = min * min;
		u32b dis_squared = dis * dis;
		int y_min = py - dis;
		int y_max = py + dis;
		int x_min = px - dis;
		int x_max = px + dis;

		/* Boundry control */
		if (x_min < 0) x_min = 0;
		if (y_min < 0) y_min = 0;
		if (x_max > p_ptr->cur_map_wid) x_max = p_ptr->cur_map_wid;
		if (y_max > p_ptr->cur_map_hgt) y_max = p_ptr->cur_map_hgt;

		/* Analyze the actual map */
		for (y = y_min; y < y_max; y++)
		{
			for (x = x_min; x < x_max; x++)
			{

				u32b dist_squared;

				/* Require "start" floor space */
				if (!cave_teleport_bold(y, x)) continue;

				/* No teleporting into vaults and such */
				if (cave_info[y][x] & (CAVE_ICKY)) continue;

				if (native)
				{
					/* It must contain the same kind of element */
					if (cave_ff3_match(y, x, TERRAIN_MASK) != flags) continue;
				}

				/* Use pythagorean theorem to ensure the distance is right */
				dist_squared = (((px - x) * (px - x)) +  ((py - y) * (py - y)));

				/* Stay within the min and the max */
				if (dist_squared <= min_squared) continue;
				if (dist_squared > dis_squared) continue;

				x_location_tables[spot_counter] = x;
				y_location_tables[spot_counter] = y;

				/*increase the counter*/
				spot_counter++;
			}
		}

		/*we have at least one random spot*/
		if (spot_counter) break;

		/* Make sure we aren't trapped in an infinite loop */
		if ((!min) && (dis == d))
		{
			msg_print("Failed to find a suitable spot for you!");
			return (FALSE);
		}

		/* Increase the maximum distance */
		dis = dis * 2;
		if (dis > d) dis = d;

		/* Decrease the minimum distance */
		min = min * 6 / 10;

	}

	i = randint0(spot_counter);

	/* Mark the location */
	x = x_location_tables[i];
	y = y_location_tables[i];

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();

	return (TRUE);
}

/*
 * Teleport player to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_to(int ny, int nx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	int dis = 0, ctr = 0;

	/* Initialize */
	y = py;
	x = px;

	/* Find a usable location */
	while (1)
	{
		/* Pick a nearby legal location */
		while (1)
		{
			y = rand_spread(ny, dis);
			x = rand_spread(nx, dis);
			if (in_bounds_fully(y, x)) break;
		}

		/* Require "start" floor space */
		if (cave_teleport_bold(y, x)) break;

		/* Occasionally advance the distance */
		if (++ctr > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}
	}

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

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

	monster_type *m_ptr = &mon_list[cave_m_idx[oy][ox]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

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

		/* Consider all empty grids */
		if (cave_empty_bold(y, x))
		{
			/* Ignore dangerous locations */
			if (!cave_no_dam_for_mon(y, x, r_ptr)) continue;

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
 * Teleport the player one level up or down (random when legal)
 * Returns false if the player chooses to stay on a quest level
 */
bool teleport_player_level(int who)
{
	quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

	bool go_up = FALSE;
	bool go_down = FALSE;

	/* Take damage from terrain */
	process_player_terrain_damage();

	/* Dead player? */
	if (p_ptr->is_dead) return (FALSE);

	if (adult_ironman)
	{
		msg_print("Nothing happens.");
		return(TRUE);
	}

	if (!p_ptr->depth) go_down = TRUE;

	/*
	 * Fixed quests where the player can't go lower until they finish the quest, or the
	 * bottom of the dungeon.
	 */
	if (no_down_stairs(p_ptr->depth))
	{
		go_up = TRUE;
	}

	/*Not fair to fail the quest if the monster teleports the player off*/
	if ((who >= SOURCE_MONSTER_START) &&
			(quest_might_fail_if_leave_level() || quest_shall_fail_if_leave_level()))
	{

		/*de-activate the quest*/
		q_ptr->q_flags &= ~(QFLAG_STARTED);

		go_up = TRUE;
	}

	/* Verify leaving normal quest level */
	if ((who == SOURCE_PLAYER) && (verify_leave_quest))
	{
		char out_val[160];

		if (quest_might_fail_if_leave_level())
		{
			sprintf(out_val, "Really risk failing your quest? ");
			if (!get_check(out_val)) return(FALSE);
		}

		/* Verify leaving normal quest level */
		else if (quest_shall_fail_if_leave_level())

		{
			sprintf(out_val, "Really fail your quest? ");
			if (!get_check(out_val)) return(FALSE);
		}
	}

	/*We don't have a direction yet, pick one at random*/
	if ((!go_up) && (!go_down))
	{
		if (one_in_(2)) go_up = TRUE;
		else go_down = TRUE;
	}

	/*up*/
	if (go_up == TRUE)
	{
		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

		/* New depth */
		dungeon_change_level(p_ptr->depth - 1);
	}

	else
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

		/* New depth */
		dungeon_change_level(p_ptr->depth + 1);
	}

	return (TRUE);

}



/*
 * Draw some projections in multi-hued colors.
 * -TY-, -EB-
 */
static byte mh_attr(void)
{
	switch (randint(8))
	{
		case 1:  return (TERM_RED);
		case 2:  return (TERM_GREEN);
		case 3:  return (TERM_BLUE);
		case 4:  return (TERM_YELLOW);
		case 5:  return (TERM_ORANGE);
		case 6:  return (TERM_L_RED);
		case 7:  return (TERM_L_GREEN);
		case 8:  return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte acid_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_SLATE);
		case 2: return (TERM_L_DARK);
	}
	return (TERM_WHITE);
}

static byte elec_color(void)
{
	byte base;

	switch (rand_int(3))
	{
		case 0: case 1: base = (TERM_BLUE); break;
		default: base = (TERM_L_BLUE); break;
	}

	if (one_in_(4)) return (MAKE_EXTENDED_COLOR(base, 1));

	return (base);
}

static byte fire_color(void)
{
	byte base;

	switch (rand_int(3))
	{
		case 0: case 1: base = (TERM_RED); break;
		default: base = (TERM_L_RED); break;
	}

	if (one_in_(4)) return (MAKE_EXTENDED_COLOR(base, 1));

	return (base);
}

static byte cold_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_WHITE);
	}
	return (TERM_WHITE);
}

static byte pois_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_GREEN);
		case 2: return (TERM_L_GREEN);
	}
	return (TERM_WHITE);
}

static byte plasma_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_RED);
		case 3: return (TERM_YELLOW);
	}

	return (TERM_WHITE);
}

static byte disen_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: return (TERM_VIOLET);
		case 2: case 3: return (TERM_D_PURPLE);
		case 4: return (TERM_RASPBERRY);
	}

	return (TERM_WHITE);
}


static byte nexus_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_VIOLET);
		case 2: return (TERM_RED);
		case 3: return (TERM_D_PURPLE);
	}

	return (TERM_WHITE);
}

static byte ice_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte light_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_YELLOW);
		case 3: return (TERM_ORANGE);
	}

	return (TERM_WHITE);
}

static byte confu_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: case 2: return (TERM_L_UMBER);
		case 3: return (TERM_UMBER);
		case 4: return (TERM_WHITE);
	}

	return (TERM_WHITE);
}

static byte spore_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: case 2: return (TERM_L_UMBER);
		case 3: return (TERM_UMBER);
		case 4: return (TERM_ORANGE);
	}

	return (TERM_WHITE);
}

static byte sound_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: case 2: return (TERM_YELLOW);
		case 3: return (TERM_GOLD);
		case 4: return (TERM_MAIZE);
	}

	return (TERM_WHITE);
}

static byte grav_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_DARK);
		case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_WHITE);
}

static byte iner_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: case 2: return (TERM_PINK);
		case 3: return (TERM_RASPBERRY);
		case 4: return (TERM_RED_RUST);
	}

	return (TERM_WHITE);
}

static byte meteor_color(void)
{
	switch (rand_int(6))
	{
		case 0: case 1: return (TERM_L_DARK);
		case 2: return (TERM_WHITE);
		case 3: return (TERM_RED);
		case 4: return (TERM_ORANGE);
		case 5: return (TERM_YELLOW);
	}

	return (TERM_WHITE);
}

static byte water_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: case 2: return (TERM_SLATE);
		case 3: return (TERM_L_BLUE);
		case 4: return (TERM_SNOW_WHITE);
	}

	return (TERM_L_DARK);
}

static byte orb_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_L_DARK);
}

static byte mana_color(void)
{
	byte base;

	switch (rand_int(2))
	{
		case 1: base = (TERM_BLUE); break;
		default: base = (TERM_RED); break;
	}

	if (one_in_(2)) return (MAKE_EXTENDED_COLOR(base, 1));

	return (base);
}

static byte fog_color(void)
{
	switch (rand_int(5))
	{
		case 0: return (TERM_VIOLET);
		case 1: return (MAKE_EXTENDED_COLOR(TERM_VIOLET, 1));
	}

	return (TERM_SLATE);
}

static byte bwater_color(void)
{
	if (one_in_(3)) return (TERM_L_BLUE);

	return (TERM_WHITE);
}


static byte lava_color(void)
{
	if (one_in_(4)) return (TERM_L_RED);

	return (TERM_RED);
}

static byte smoke_color(void)
{
	if (one_in_(4)) return (TERM_L_RED);

	return (TERM_L_DARK);
}

/*
 * Return a color to use for the bolt/ball spells
 */
byte gf_color(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_MISSILE:	return (TERM_VIOLET);
		case GF_ACID:		return (acid_color());
		case GF_ELEC:		return (elec_color());
		case GF_ELEC_BURST:	return (elec_color());
		case GF_FIRE:		return (fire_color());
		case GF_COLD:		return (cold_color());
		case GF_POIS:		return (pois_color());
		case GF_HOLY_ORB:	return (orb_color());
		case GF_MANA:		return (mana_color());
		case GF_STATIC:		return (TERM_WHITE);
		case GF_ARROW:		return (TERM_WHITE);
		case GF_WATER:		return (water_color());
		case GF_EXTINGUISH:	return (TERM_BLUE);
		case GF_CLEAR_AIR:	return (TERM_WHITE);
		case GF_NETHER:		return (TERM_L_GREEN);
		case GF_CHAOS:		return (mh_attr());
		case GF_DISENCHANT:	return (disen_color());
		case GF_STERILIZE:	return (TERM_YELLOW);
		case GF_NEXUS:		return (nexus_color());
		case GF_CONFUSION:	return (confu_color());
		case GF_SOUND:		return (sound_color());
		case GF_SPORE:		return (spore_color());
		case GF_SHARD:		return (TERM_UMBER);
		case GF_FORCE:		return (TERM_UMBER);
		case GF_KILL_WALL:	return (TERM_COPPER);
		case GF_KILL_TRAP:	return (TERM_L_BLUE);
		case GF_KILL_DOOR:	return (TERM_SILVER);
		case GF_MAKE_WALL:	return (TERM_EARTH_YELLOW);
		case GF_MAKE_DOOR:	return (TERM_COPPER);
		case GF_MAKE_TRAP:	return (TERM_GOLD);
		case GF_AWAY_UNDEAD:return (TERM_ORANGE_PEEL);
		case GF_AWAY_EVIL:	return (TERM_L_WHITE_2);
		case GF_AWAY_ALL:	return (TERM_JUNGLE_GREEN);
		case GF_TURN_UNDEAD:return (TERM_MAHAGONY);
		case GF_TURN_EVIL:	return (TERM_MAIZE);
		case GF_TURN_ALL:	return (TERM_RED_RUST);
		case GF_DISP_UNDEAD:return (TERM_TAUPE);
		case GF_DISP_EVIL:	return (TERM_SKY_BLUE);
		case GF_DISP_ALL:	return (TERM_D_PURPLE);
		case GF_MAKE_WARY:	return (TERM_MAIZE);
		case GF_OLD_CLONE:	return (TERM_BLUE);
		case GF_OLD_POLY:	return (mh_attr());
		case GF_OLD_HEAL:	return (TERM_SNOW_WHITE);
		case GF_OLD_SPEED:	return (TERM_ORANGE);
		case GF_OLD_SLOW:	return (TERM_PINK);
		case GF_OLD_CONF:	return (confu_color());
		case GF_OLD_SLEEP:	return (TERM_L_DARK);
		case GF_INERTIA:	return (iner_color());
		case GF_GRAVITY:	return (grav_color());
		case GF_TIME:		return (TERM_L_BLUE);
		case GF_LIGHT_WEAK:	return (light_color());
		case GF_LIGHT:		return (light_color());
		case GF_DARK_WEAK:	return (TERM_L_DARK);
		case GF_DARK:		return (TERM_L_DARK);
		case GF_PLASMA:		return (plasma_color());
		case GF_METEOR:		return (meteor_color());
		case GF_ICE:		return (ice_color());
		case GF_MASS_IDENTIFY:	return (TERM_WHITE);
		case GF_SMOKE:		return (smoke_color());
		case GF_FOG:		return (fog_color());
		case GF_SAND:		return (TERM_YELLOW);
		case GF_BMUD:		return (TERM_ORANGE);
		case GF_BWATER:		return (bwater_color());
		case GF_LAVA:		return (lava_color());
		case GF_LIFE_DRAIN:  return (TERM_VIOLET);
	}

	/* Standard "color" */
	return (TERM_WHITE);
}

/*
 * Helper function for bolt_pic.  For Adam Bolt's tileset, there are several series of
 * arrows pointed in all 8 directions.  This functions helps display the right one based
 * on a particular direction.
 * Projectile is moving (or has moved) from (x,y) to (nx,ny).
 */
static int get_arrow_direction_new(int y, int x, int ny, int nx)
{
	int adjust = 0;

	/* On the same row */
	if (y == ny)
	{
		/* Headed left */
		if (x > nx) 		adjust = 2;
		/* Headed right */
		else if (x < nx)	adjust = 3;
	}
	/* On the same column */
	else if (x == nx)
	{
		/* Headed up */
		if (y > ny) 		adjust = 0;

		/* Headed down */
		else /*if (y < ny)*/adjust = 1;
	}
	/* headed down */
	else if (y < ny)
	{
		/* Diagonally right */
		if (x > nx)			adjust = 7;

		/* Diagonally left */
		else if (x < nx)  	adjust = 6;
	}
	/* headed up */
	else /*if (y > ny) */
	{
		/* Diagonally right */
		if (x > nx)			adjust = 5;

		/* Diagonally left */
		else if (x < nx)	adjust = 4;
	}
	return (adjust);
}

/*
 * Helper function for bolt_pic.  For the DVG tileset, there are several series of
 * arrows pointed in all 8 directions.  This functions helps display the right one based
 * on a particular direction.
 * Projectile is moving (or has moved) from (x,y) to (nx,ny).
 */
static int get_arrow_direction_dvg(int y, int x, int ny, int nx)
{
	int adjust = 0;

	/* On the same row */
	if (y == ny)
	{
		/* Headed left */
		if (x > nx) 		adjust = 3;
		/* Headed right */
		else if (x < nx)	adjust = 4;
	}
	/* On the same column */
	else if (x == nx)
	{
		/* Headed up */
		if (y > ny) 		adjust = 6;

		/* Headed down */
		else /*if (y < ny)*/adjust = 1;
	}
	/* headed down */
	else if (y < ny)
	{
		/* Diagonally right */
		if (x > nx)			adjust = 0;

		/* Diagonally left */
		else if (x < nx)  	adjust = 2;
	}
	/* headed up */
	else /*if (y > ny) */
	{
		/* Diagonally right */
		if (x > nx)			adjust = 5;

		/* Diagonally left */
		else if (x < nx)	adjust = 7;
	}
	return (adjust);
}

/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x,y) to (nx,ny).
 *
 * If the distance is not "one", we (may) return "*".
 */
u16b bolt_pict(int y, int x, int ny, int nx, int typ, u32b flg)
{
	/* Get the color */
	byte typ_color = gf_color(typ);

	byte a;
	char c;

	/* Special handling of boulders */
	if (flg & (PROJECT_ROCK))
	{
		/* Special handling GRAPHICS_DAVID_GERVAIS graphics */
		if (use_graphics && (arg_graphics == GRAPHICS_DAVID_GERVAIS))
		{
			a = (byte)0x97;
			c = (char)0xfe;
		}
		else
		{
			a = TERM_SLATE;
			c = '0';
		}
	}
	/* Special handling of shots */
	else if (flg & (PROJECT_SHOT))
	{
		/* Use character for the iron shot */
		int k_idx = lookup_kind(TV_SHOT, SV_AMMO_NORMAL);
		a = object_type_attr(k_idx);
		c = object_type_char(k_idx);

	}

	else if (flg & (PROJECT_AMMO))
	{
		/* Special handling GRAPHICS_DAVID_GERVAIS and GRAPHICS_ADAM_BOLT graphics */
		if (use_graphics && ((arg_graphics == GRAPHICS_DAVID_GERVAIS) || (arg_graphics == GRAPHICS_ADAM_BOLT)))
		{
			if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
			{
				int add = get_arrow_direction_dvg(y, x, ny, nx);

				a = (byte)0x81;
				c = (char)0xec + add;
			}
			else /* if (arg_graphics == GRAPHICS_ADAM_BOLT) */
			{
				int add = get_arrow_direction_new(y, x, ny, nx);

				a = (byte)0xae;
				c = (char)0x83 + add;
			}
		}
		else
		{
			/* Use character for the arrow */
			int k_idx = lookup_kind(TV_ARROW, SV_AMMO_NORMAL);
			a = object_type_attr(k_idx);
			c = object_type_char(k_idx);

		}
	}

	/* Using ASCII */
	else if (!use_graphics)
	{
		/* No motion (*) */
		if ((ny == y) && (nx == x)) c = '*';

		/* Vertical (|) */
		else if (nx == x) c = '|';

		/* Horizontal (-) */
		else if (ny == y) c = '-';

		/* Diagonal (/) */
		else if ((ny-y) == (x-nx)) c = '/';

		/* Diagonal (\) */
		else if ((ny-y) == (nx-x)) c = '\\';

		/* Weird (*) */
		else c = '*';

		a = typ_color;
	}
	/* Using a tileset */
	else
	{
		int add;

		/* Assume bolt unless otherwise specified below (no motion) */
		byte tile_type = TILE_BOLT_INFO;

		/* No motion (*) */
		if ((ny == y) && (nx == x)) {tile_type = TILE_BALL_INFO; add = 0;}

		/* Vertical (|) */
		else if (nx == x) add = 0;

		/* Horizontal (-) */
		else if (ny == y) add = 1;

		/* Diagonal (/) */
		else if ((ny-y) == (x-nx)) add = 2;

		/* Diagonal (\) */
		else if ((ny-y) == (nx-x)) add = 3;

		/* Weird (*) */
		else {tile_type = TILE_BALL_INFO; add = 0;}

		/* Obtain attr/char */
		a = color_to_attr[tile_type][typ_color];
		c = color_to_char[tile_type][typ_color] + add;
	}

	/* Create pict */
	return (PICT(a,c));
}


/*
 * Hurt the player (maybe killing him/her). The damage comes from the given
 * feature. kb_str is the cause of the death.
 */
void take_terrain_hit(int dam, int feat, cptr kb_str)
{
	char name[80];

	cptr action;

	int gf_type;

	/* Get the feature */
	feature_lore *f_l_ptr = &f_l_list[feat];

	/* Paranoia */
	if (dam == 0) return;

	/* Count the number of times this damage has been felt */
	if (f_l_ptr->f_l_dam_non_native < MAX_UCHAR) f_l_ptr->f_l_dam_non_native++;

	/* Get spell type */
	get_spell_type_from_feature(feat, &gf_type, &action);

	/* Ignore certain spell types if the player is immune to them */
	if (is_player_immune(gf_type)) return;

	/* Show a message */
	if (!p_ptr->timed[TMD_BLIND])
	{
		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, FALSE, TRUE);

		msg_format("The %s %s you!", name, action);
	}

	/* Take the hit */
	(void)project_p(SOURCE_OTHER, p_ptr->py, p_ptr->px, dam, gf_type, kb_str);
}


/*
 * Decreases players hit points and sets death flag if necessary
 *
 * Invulnerability needs to be changed into a "shield" XXX XXX XXX
 *
 * Hack -- this function allows the user to save (or quit) the game
 * when he dies, since the "You die." message is shown before setting
 * the player to "dead".
 */
void take_hit(int dam, cptr kb_str)
{
	int old_chp = p_ptr->chp;

	int warning = (p_ptr->mhp * op_ptr->hitpoint_warn / 10);

	/* Paranoia */
	if (p_ptr->is_dead) return;

	/* Disturb */
	disturb(1, 0);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->timed[TMD_INVULN] && (dam < 9000)) return;

	/* Hurt the player */
	p_ptr->chp -= dam;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Hack -- Note death */
		message(MSG_DEATH, 0, "You die.");
		message_flush();

		/* Note cause of death */
		my_strcpy(p_ptr->died_from, kb_str, sizeof(p_ptr->died_from));

		/* No longer a winner */
		p_ptr->total_winner = FALSE;

		/* Note death */
		p_ptr->is_dead = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Write a note */
		if (adult_take_notes)
		{
			time_t ct = time((time_t*)0);
			char long_day[25];
			char buf[120];

 		  	/* Get time */
 		  	(void)strftime(long_day, 25, "%m/%d/%Y at %I:%M %p", localtime(&ct));

 		  	/* Add note */

		  	file_putf(notes_file, "============================================================\n");

			/*killed by */
 		  	sprintf(buf, "Killed by %s.", p_ptr->died_from);

			/* Write message */
            do_cmd_note(buf,  p_ptr->depth);

			/* date and time*/
			sprintf(buf, "Killed on %s.", long_day);

			/* Write message */
            do_cmd_note(buf,  p_ptr->depth);

			file_putf(notes_file, "============================================================\n");

		}

		/* Dead */
		return;
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (old_chp > warning)
		{
			bell("Low hitpoint warning!");
		}

		/* Message */
		message(MSG_HITPOINT_WARN, 0, "*** LOW HITPOINT WARNING! ***");
		message_flush();
	}
}





/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
static bool hates_acid(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable items */
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls are wood/paper */
		case TV_STAFF:
		case TV_SCROLL:
		case TV_PARCHMENT:
		{
			return (TRUE);
		}

		/* Ouch */
		case TV_CHEST:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate electricity?
 */
static bool hates_elec(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_RING:
		case TV_WAND:
		case TV_ROD:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate fire?
 * Hafted/Polearm weapons have wooden shafts.
 * Arrows/Bows are mostly wooden.
 */
static bool hates_fire(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Wearable */
		case TV_LIGHT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		{
			return (TRUE);
		}

		/* Books */
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_DRUID_BOOK:
		{
			return (TRUE);
		}

		/* Chests */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Staffs/Scrolls burn */
		case TV_STAFF:
		case TV_SCROLL:
		case TV_PARCHMENT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given object (usually) hate cold?
 */
static bool hates_cold(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_POTION:
		case TV_FLASK:
		case TV_BOTTLE:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Does a given class of objects (usually) hate sand?
 */
static bool hates_sand(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Destroys Wearable items */
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate boiling mud?
 */
static bool hates_boiling_mud(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{

		/* Scrolls get destroyed */
		case TV_SCROLL:
		case TV_PARCHMENT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate boiling mud?
 */
static bool hates_boiling_water(const object_type *o_ptr)
{

	/* Analyze the type */
	switch (o_ptr->tval)
	{

		/* Potions and flasks get destroyed */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate lava?
 */
static bool hates_lava(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Ignore elements that resist fire damage */
	if (f3 & (TR3_IGNORE_FIRE)) return (FALSE);

	/*Most everything*/
	switch (o_ptr->tval)
	{
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_RING:
		case TV_WAND:
		case TV_ROD:
		case TV_LIGHT:
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_DRUID_BOOK:
		case TV_CHEST:
		case TV_STAFF:
		case TV_SCROLL:
		case TV_PARCHMENT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Return TRUE if the object can't exist on the given feature
 */
bool object_hates_feature(int feat, const object_type *o_ptr)
{
	/* Get the element flags of the location */
	u32b terrain_native = feat_ff3_match(feat, TERRAIN_MASK);

	u32b f1, f2, f3, fn;

	/* Get object flags (we'll check object resistance) */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/*Artifacts are comfortable anywhere*/
	if (artifact_p(o_ptr)) return (FALSE);

	/*Check if object is native*/
	if (fn)
	{
		u32b filtered_native = terrain_native;

		/*Strip out unneeded ones*/
		filtered_native &= fn;

		/*if we have a match, the object is comfortable at the location*/
		if (fn == filtered_native) return (FALSE);
	}

	/* Check fire and lava */
	if ((terrain_native & (ELEMENT_FIRE | ELEMENT_LAVA)) &&
		!(f3 & (TR3_IGNORE_FIRE)) && hates_fire(o_ptr))
	{
		return (TRUE);
	}
	/* Check acid */
	else if ((terrain_native & (ELEMENT_ACID)) &&
		!(f3 & (TR3_IGNORE_ACID)) && hates_acid(o_ptr))
	{
		return (TRUE);
	}
	/* Check ice */
	else if ((terrain_native & (ELEMENT_ICE)) &&
		!(f3 & (TR3_IGNORE_COLD)) && hates_cold(o_ptr))
	{
		return (TRUE);
	}
	/* The object can exist in the grid */
	else
	{
		return (FALSE);
	}
}



/*
 * Return TRUE if the object can't exist in the given location
 */
bool object_hates_location(int y, int x, const object_type *o_ptr)
{
	int feat = cave_feat[y][x];

	return object_hates_feature(feat, o_ptr);
}

/*
 * destroy cursed items
 */
static void holy_orb_destroy(int damage)
{
	int i;

	object_type *o_ptr;

	char o_name[80];

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr)) continue;

		/* Give any crused item a shot at destruction */
		if ((cursed_p(o_ptr)) && (rand_int(100) < damage))
		{
			int amt = o_ptr->number;

			/* Get a description */
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

			/* Message */
			message_format(MSG_DESTROY, 0, "%sour %s (%c) %s destroyed!",
				       ((amt > 1) ? "All of y" : "Y"),
				       o_name,
					   index_to_label(i),
					   ((amt > 1) ? "were" : "was"));

			/* Destroy "amt" items */
			inven_item_increase(i, -amt);
			inven_item_optimize(i);

		}
	}
}




/*
 * Melt something
 */
static int set_acid_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_acid(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_ACID)) return (FALSE);
	if (fn & (ELEMENT_ACID)) return (FALSE);
	return (TRUE);
}


/*
 * Electrical damage
 */
static int set_elec_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_elec(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_ELEC)) return (FALSE);
	return (TRUE);
}


/*
 * Burn something
 */
static int set_fire_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_fire(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_FIRE)) return (FALSE);
	if (fn & (ELEMENT_LAVA)) return (FALSE);
	return (TRUE);
}


/*
 * Freeze things
 */
static int set_cold_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_cold(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (f3 & (TR3_IGNORE_COLD)) return (FALSE);
	if (fn & (ELEMENT_ICE)) return (FALSE);
	return (TRUE);
}

/*
 * Melt something
 */
static int set_lava_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_lava(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if (fn & (ELEMENT_LAVA)) return (FALSE);
	return (TRUE);
}

/*
 * Destroy something
 */
static int set_boiling_water_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_boiling_water(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if ((fn & (ELEMENT_BWATER)) == ELEMENT_BWATER) return (FALSE);
	return (TRUE);
}

/*
 * Destroy something
 */
static int set_boiling_mud_destroy(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	if (!hates_boiling_mud(o_ptr)) return (FALSE);
	object_flags(o_ptr, &f1, &f2, &f3, &fn);
	if ((fn & (ELEMENT_BMUD)) == ELEMENT_BMUD) return (FALSE);
	return (TRUE);
}


/* Drain an activation.
 * If the timeout is increased, return true.
 * This function assumes object can be activated
 */
static bool drain_activation(object_type *o_ptr, int time)
{
	s16b old_timeout = o_ptr->timeout;

	/*Add the penalty*/
	o_ptr->timeout += (time / 5);

	/* Limit timeout*/

	/*Artifact*/
	if ((o_ptr->art_num) && (o_ptr->art_num < z_info->art_norm_max))
	{
		artifact_type *a_ptr = &a_info[o_ptr->art_num];

		if (o_ptr->timeout > (a_ptr->time + a_ptr->randtime)) o_ptr->timeout = a_ptr->time + a_ptr->randtime;
	}
	/*Dragon Armor*/
	else if ((o_ptr->tval == TV_DRAG_ARMOR) ||
				(o_ptr->tval == TV_DRAG_SHIELD))
	{
		/*The *2 is the random time element of the dragon armor re-charge*/
		u16b value = o_ptr->sval * 2;

		/*Armor is more powerful than shields*/
		if (o_ptr->tval == TV_DRAG_ARMOR) value *= 2;

		/* Branch on the sub-type */
		switch (o_ptr->ego_num)
		{
			case EGO_DRAGON_BLACK:
			case EGO_DRAGON_BLUE:
			case EGO_DRAGON_WHITE:
			case EGO_DRAGON_GREEN:
			case EGO_DRAGON_RED:
			case EGO_DRAGON_BRONZE:
			case EGO_DRAGON_GOLD:
			{
				value *= 50;

				break;
			}

			case EGO_DRAGON_MULTIHUED:
			case EGO_DRAGON_BALANCE:
			{
				value *= 75;
				break;
			}

			case EGO_DRAGON_CHAOS:
			case EGO_DRAGON_LAW:
			{
				value *= 60;

				break;
			}

			case EGO_DRAGON_PSEUDO:
			{
				value *= 65;
				break;
			}

			case EGO_DRAGON_POWER:
			{
				value *= 100;
				break;
			}
		}

		if (o_ptr->timeout > value) o_ptr->timeout = value;
	}

	/* Hack -- certain Rings - currently all the rings have the same actication timeout*/
	else if (o_ptr->tval == TV_RING)
	{
		if (o_ptr->timeout > 100) o_ptr->timeout = 100;
	}

	/* The timeout was increased*/
	if (o_ptr->timeout > old_timeout) return (TRUE);

	/*else*/
	return (FALSE);
}

/*
 * This seems like a pretty standard "typedef"
 */
typedef int (*inven_func)(const object_type *);

/*
 * Drain the chargable and activable items
 *
 * Returns number of slots drained.
 */
static int inven_drain(int dam)
{
	int i, k;

	object_type *o_ptr;
	object_kind *k_ptr;

	char o_name[80];

	/* Count the casualties */
	k = 0;

	/* Scan through the equipment and slots backwards */
 	for (i = 0; i < INVEN_TOTAL; i++)
	{

		o_ptr = &inventory[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Ignore the swap weapon */
		if ((adult_swap_weapons) && (i == INVEN_SWAP_WEAPON)) continue;

		/*
		 * No messages needed.
		 * We do not notice this with objects sitting on the ground.
		 */
		if (!(item_tester_hook_activate(o_ptr))) continue;

		/*Allow objects a saving throw*/
		if (rand_int((dam / 2)) <= k_ptr->k_level) continue;

		/*Artifacts get another saving throw*/
		if (o_ptr->art_num)
		{
			artifact_type *a_ptr = &a_info[o_ptr->art_num];

			if (rand_int((dam / 2)) <= a_ptr->a_level) continue;
		}

		(void)drain_activation(o_ptr, dam);

		/*Count it*/
		k++;

		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

		/* Message */
		msg_format("Your %s (%c) %s drained!", o_name, index_to_label(i), ((o_ptr->number > 1) ? "were" : "was"));

	}

	/* Return the casualty count */
	return (k);
}



/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 *
 * Returns number of items destroyed.
 */
static int inven_damage(inven_func typ, int perc, bool protected)
{
	int i, j, k, amt;

	object_type *o_ptr;

	char o_name[80];

	/* Count the casualties */
	k = 0;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- for now, skip artifacts */
		if (artifact_p(o_ptr)) continue;

		/* Give this item slot a shot at death */
		if ((*typ)(o_ptr))
		{
			int percent = 125;

			/* Rods are tough. */
			if (o_ptr->tval == TV_ROD)
			{
				percent *= 5;
				percent /= 3;
			}

			if (protected)
			{
				percent *= 5;
				percent /= 3;
			}

			/* Count the casualties */
			for (amt = j = 0; j < o_ptr->number; ++j)
			{
				if (rand_int(percent) < perc) amt++;
			}

			/* Some casualities */
			if (amt)
			{
				int old_charges = 0;

				/*hack, make sure the proper number of charges is displayed in the message*/
				if (((o_ptr->tval == TV_WAND) ||
					(o_ptr->tval == TV_STAFF) ||
					(o_ptr->tval == TV_ROD))
	    			&& (amt < o_ptr->number))
				{
					/*save the number of charges*/
					old_charges = o_ptr->pval;

					/*distribute the charges*/
					o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;

					o_ptr->pval = old_charges - o_ptr->pval;
				}

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

				/* Message */
				message_format(MSG_DESTROY, 0, "%sour %s (%c) %s destroyed!",
				           ((o_ptr->number > 1) ?
				            ((amt == o_ptr->number) ? "All of y" :
				             (amt > 1 ? "Some of y" : "One of y")) : "Y"),
				           o_name, index_to_label(i),
				           ((amt > 1) ? "were" : "was"));

				/*hack, restore the proper number of charges after the messages have printed
	 			 * so the proper number of charges are destroyed*/
				 if (old_charges) o_ptr->pval = old_charges;

				/* Hack -- If rods,wand or staff are destroyed, the total maximum
				 * timeout or charges of the stack needs to be reduced,
				 * unless all the items are being destroyed. -LM-
				 */
				if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)
					|| (o_ptr->tval == TV_STAFF)) && (amt < o_ptr->number))
				{
					o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
				}

				/* Destroy "amt" items */
				inven_item_increase(i, -amt);
				inven_item_optimize(i);

				/* Count the casualties */
				k += amt;
			}
		}
	}

	/* Return the casualty count */
	return (k);
}




/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(void)
{
	object_type *o_ptr = NULL;

	u32b f1, f2, f3, fn;

	char o_name[80];


	/* Pick a (possibly empty) inventory slot */
	switch (randint(6))
	{
		case 1: o_ptr = &inventory[INVEN_BODY]; break;
		case 2: o_ptr = &inventory[INVEN_ARM]; break;
		case 3: o_ptr = &inventory[INVEN_OUTER]; break;
		case 4: o_ptr = &inventory[INVEN_HANDS]; break;
		case 5: o_ptr = &inventory[INVEN_HEAD]; break;
		case 6: o_ptr = &inventory[INVEN_FEET]; break;
	}

	/* Nothing to damage */
	if (!o_ptr->k_idx) return (FALSE);

	/* No damage left to be done */
	if (o_ptr->ac + o_ptr->to_a <= 0) return (FALSE);


	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Object resists */
	if (f3 & (TR3_IGNORE_ACID))
	{
		msg_format("Your %s is unaffected!", o_name);

		return (TRUE);
	}

	/* Message */
	msg_format("Your %s is damaged!", o_name);

	/* Damage the item */
	o_ptr->to_a--;

	/* Calculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_EQUIP);

	/* Item was damaged */
	return (TRUE);
}


/*
 * Hurt the player with Acid
 */
void acid_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Total Immunity */
	if (p_ptr->state.immune_acid || (dam <= 0)) return;

	/*inventory  gets protected better with double resistance*/
	if ((p_ptr->state.resist_acid) && (p_ptr->timed[TMD_OPP_ACID])) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->state.resist_acid) dam = (dam + 2) / 3;
	if (p_ptr->timed[TMD_OPP_ACID]) dam = (dam + 2) / 3;
	if (p_ptr->p_native & (P_NATIVE_ACID)) dam = (dam * 4 + 1) / 5;

	/* If any armor gets hit, defend the player */
	if (minus_ac()) dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_acid_destroy, inv, double_resist);
}


/*
 * Hurt the player with electricity
 */
void elec_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Total immunity */
	if (p_ptr->state.immune_elec || (dam <= 0)) return;

	/*inventory gets protected better with double resistance*/
	if ((p_ptr->state.resist_elec) && (p_ptr->timed[TMD_OPP_ELEC])) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->timed[TMD_OPP_ELEC]) dam = (dam + 2) / 3;
	if (p_ptr->state.resist_elec) dam = (dam + 2) / 3;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_elec_destroy, inv, double_resist);
}




/*
 * Hurt the player with Fire
 */
void fire_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Totally immune */
	if (p_ptr->state.immune_fire || (dam <= 0)) return;

	/*inventory  gets protected better with double resistance*/
	if ((p_ptr->state.resist_fire) && (p_ptr->timed[TMD_OPP_FIRE])) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->state.resist_fire) dam = (dam + 2) / 3;
	if (p_ptr->timed[TMD_OPP_FIRE]) dam = (dam + 2) / 3;
	if (p_ptr->p_native & (P_NATIVE_FIRE)) dam = (dam * 4 + 1) / 5;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_fire_destroy, inv, double_resist);
}


/*
 * Hurt the player with Cold
 */
void cold_dam(int dam, cptr kb_str)
{
	int inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
	bool double_resist = FALSE;

	/* Total immunity */
	if (p_ptr->state.immune_cold || (dam <= 0)) return;

	/*inventory  gets protected better with double resistance*/
	if ((p_ptr->state.resist_cold) && (p_ptr->timed[TMD_OPP_COLD])) double_resist = TRUE;

	/* Resist the damage */
	if (p_ptr->state.resist_cold) dam = (dam + 2) / 3;
	if (p_ptr->timed[TMD_OPP_COLD]) dam = (dam + 2) / 3;
	if (p_ptr->p_native & P_NATIVE_ICE) dam = (dam  * 4 + 1) / 5;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_cold_destroy, inv, double_resist);
}


/*
 * Hurt the player with Fire
 */
static void lava_dam(int dam, cptr kb_str, bool player_native)
{
	int inv = (dam < 30) ? 2 : (dam < 60) ? 3 : 4;
	bool double_resist = FALSE;

	/* Resist most of the damage, equipment is spared */
	if (player_native)
	{
		dam /= 3;
		double_resist = TRUE;
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_lava_destroy, inv, double_resist);
}

/*
 * Hurt the player with Boiling Water
 */
static void boiling_water_dam(int dam, cptr kb_str, bool player_native)
{
	int inv = (dam < 30) ? 2 : (dam < 60) ? 3 : 4;
	bool double_resist = FALSE;

	/* Resist most of the damage */
	if (player_native)
	{
		dam /= 9;
		double_resist = TRUE;
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_boiling_water_destroy, inv, double_resist);
}

/*
 * Hurt the player with Boiling Mud
 */
static void boiling_mud_dam(int dam, cptr kb_str, bool player_native)
{
	int inv = (dam < 30) ? 2 : (dam < 60) ? 3 : 4;
	bool double_resist = FALSE;

	/* Resist most of the damage */
	if (player_native)
	{
		dam /= 9;
		double_resist = TRUE;
	}

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	inven_damage(set_boiling_mud_destroy, inv, double_resist);
}


/*
 * Increase a stat by one randomized level
 *
 * Most code will "restore" a stat before calling this function,
 * in particular, stat potions will always restore the stat and
 * then increase the fully restored value.
 */
bool inc_stat(int stat)
{
	int value, gain;

	/* Then augment the current/max stat */
	value = p_ptr->stat_cur[stat];

	/* Cannot go above 18/100 */
	if (value < 18+100)
	{
		/* Gain one (sometimes two) points */
		if (value < 18)
		{
			gain = ((rand_int(100) < 75) ? 1 : 2);
			value += gain;
		}

		/* Gain 1/6 to 1/3 of distance to 18/100 */
		else if (value < 18+98)
		{
			/* Approximate gain value */
			gain = (((18+100) - value) / 2 + 3) / 2;

			/* Paranoia */
			if (gain < 1) gain = 1;

			/* Apply the bonus */
			value += randint(gain) + gain / 2;

			/* Maximal value */
			if (value > 18+99) value = 18 + 99;
		}

		/* Gain one point at a time */
		else
		{
			value++;
		}

		/* Save the new value */
		p_ptr->stat_cur[stat] = value;

		/* Bring up the maximum too */
		if (value > p_ptr->stat_max[stat])
		{
			p_ptr->stat_max[stat] = value;
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to gain */
	return (FALSE);
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount,bool permanent)
{
	int cur, max, loss, same, res = FALSE;


	/* Get the current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
 	same = (cur == max);

	/* Damage "current" value */
	if (cur > 3)
	{
		/* Handle "low" values */
		if (cur <= 18)
		{
			if (amount > 90) cur--;
			if (amount > 50) cur--;
			if (amount > 20) cur--;
			cur--;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((cur-18) / 2 + 1) / 2 + 1);

			/* Paranoia */
			if (loss < 1) loss = 1;

			/* Randomize the loss */
			loss = ((randint(loss) + loss) * amount) / 100;

			/* Maximal loss */
			if (loss < amount/2) loss = amount/2;

			/* Lose some points */
			cur = cur - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (cur < 18) cur = (amount <= 20) ? 18 : 17;
		}

		/* Prevent illegal values */
		if (cur < 3) cur = 3;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent && (max > 3))
	{
		/* Handle "low" values */
		if (max <= 18)
		{
			if (amount > 90) max--;
			if (amount > 50) max--;
			if (amount > 20) max--;
			max--;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((max-18) / 2 + 1) / 2 + 1);
			if (loss < 1) loss = 1;
			loss = ((randint(loss) + loss) * amount) / 100;
			if (loss < amount/2) loss = amount/2;

			/* Lose some points */
			max = max - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (max < 18) max = (amount <= 20) ? 18 : 17;
		}

		/* Hack -- keep it clean */
		if (same || (max < cur)) max = cur;

		/* Something happened */
		if (max != p_ptr->stat_max[stat]) res = TRUE;
	}

	/* Apply changes */
	if (res)
	{
		/* Actually set the stat to its new value. */
		p_ptr->stat_cur[stat] = cur;
		p_ptr->stat_max[stat] = max;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);
	}

	/* Done */
	return (res);
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

		/* Redisplay the stats later */
		p_ptr->redraw |= (PR_STATS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to restore */
	return (FALSE);
}

/*
 * Inflict disease on the character.
 */
void disease(int *damage)
{
	int con, attempts;
	int i;

	/* Get current constitution */
	con = p_ptr->stat_cur[A_CON];

	/* Adjust damage and choose message based on constitution */
	if (con < 8)
	{
		msg_print("You feel deathly ill.");
		*damage *= 2;
	}

	else if (con < 14)
	{
		msg_print("You feel seriously ill.");
	}

	else if (con < 18)
	{
		msg_print("You feel quite ill.");
		*damage = *damage * 2 / 3;
	}

	/* CON is at least 18, and less than 18/50 */
	else if (con < 68)
	{
		msg_print("You feel ill.");
		*damage /= 2;
	}

	/* CON is at least 18/50, and less than 18/100 */
	else if (con < 118)
	{
		msg_print("You feel sick.");
		*damage /= 3;
	}

	/* CON is at least 18/100 */
	else
	{
		msg_print("You feel a bit sick.");
		*damage /= 4;
	}

	/* Infect the character (fully cumulative) */

	if (!(p_ptr->state.immune_pois))
	{
		(void)inc_timed(TMD_POISONED, *damage + 1, TRUE);
	}

	/* Determine # of stat-reduction attempts */
	attempts = (5 + *damage) / 5;

	/* Attack stats */
	for (i = 0; i < attempts; i++)
	{
		/* Each attempt has a 10% chance of success */
		if (one_in_(10))
		{
			/* Damage a random stat */
			(void)do_dec_stat(rand_int(A_MAX));
		}
	}
}



/*
 * Apply disenchantment to the player's stuff
 *
 * This function is also called from the "melee" code.
 *
 * The "mode" changes the message for disenchantment vs. damage.
 *
 * Return "TRUE" if the player notices anything.
 *
 * Also used for sand (with mode TRUE.  False is disenchantment.
 */
bool apply_disenchant(int mode)
{
	int t = 0;

	object_type *o_ptr;

	char o_name[80];

	/* Pick a random slot */
	/* Ignore the swap weapon */
	if (adult_swap_weapons) switch (randint(7))
	{
		case 1: {t = INVEN_WIELD; break;}
		case 2: {t = INVEN_BODY; break;}
		case 3: {t = INVEN_OUTER; break;}
		case 4: {t = INVEN_ARM; break;}
		case 5: {t = INVEN_HEAD; break;}
		case 6: {t = INVEN_HANDS; break;}
		default:{t = INVEN_FEET; break;}
	}

	else switch (randint(8))
	{
		case 1: {t = INVEN_WIELD; break;}
		case 2: {t = INVEN_BOW; break;}
		case 3: {t = INVEN_BODY; break;}
		case 4: {t = INVEN_OUTER; break;}
		case 5: {t = INVEN_ARM; break;}
		case 6: {t = INVEN_HEAD; break;}
		case 7: {t = INVEN_HANDS; break;}
		default:{t = INVEN_FEET; break;}
	}

	/* Get the item */
	o_ptr = &inventory[t];

	/* No item, nothing happens */
	if (!o_ptr->k_idx) return (FALSE);

	/* Nothing to disenchant */
	if ((o_ptr->to_h <= 0) && (o_ptr->to_d <= 0) && (o_ptr->to_a <= 0))
	{
		/* Nothing to notice */
		return (FALSE);
	}

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

	/* Artifacts have 60% chance to resist */
	if (artifact_p(o_ptr) && (rand_int(100) < 60))
	{
		/* Message */
		msg_format("Your %s (%c) resist%s %s!",
		           o_name, index_to_label(t),
		           ((o_ptr->number != 1) ? "" : "s"),
				   ((!mode) ? "disenchantment" : "damage"));

		/* Notice */
		return (TRUE);
	}

	/* Disenchant tohit */
	if (o_ptr->to_h > 0) o_ptr->to_h--;
	if ((o_ptr->to_h > 5) && (rand_int(100) < 20)) o_ptr->to_h--;

	/* Disenchant todam */
	if (o_ptr->to_d > 0) o_ptr->to_d--;
	if ((o_ptr->to_d > 5) && (rand_int(100) < 20)) o_ptr->to_d--;

	/* Disenchant toac */
	if (o_ptr->to_a > 0) o_ptr->to_a--;
	if ((o_ptr->to_a > 5) && (rand_int(100) < 20)) o_ptr->to_a--;

	/* Message */
	msg_format("Your %s (%c) %s %s!",
	           o_name, index_to_label(t),
	           ((o_ptr->number != 1) ? "were" : "was"),
			   ((!mode) ? "disenchanted" : "damaged"));

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw stuff */
	p_ptr->redraw |= (PR_EQUIP);

	/* Notice */
	return (TRUE);
}


/*
 * Apply Nexus
 */
static void apply_nexus(int who)
{

	int x, y, i;

	int max1, cur1, max2, cur2, ii, jj;

	bool is_quest_level = quest_check(p_ptr->depth);

	if (who >= SOURCE_MONSTER_START)
	{
		/* Source monster */
		monster_type *m_ptr;

		/* Get the source monster */
		m_ptr = &mon_list[who];

		y = m_ptr->fy;
		x = m_ptr->fx;
	}
	else
	{
		y = p_ptr->py;
		x = p_ptr->px;
	}

	while (TRUE)
	{
		i = randint(7);

		/* Don't teleport players off of quest levels */
		if ((is_quest_level) && (i == 6)) continue;

		/*This is a monster*/
		if (who > SOURCE_MONSTER_START) break;

		/*Hack - don't teleport_player_to if not a monster*/
		if ((i == 4) || (i == 5)) continue;

		/*We are fine*/
		break;

	}

	switch (i)
	{
		case 1: case 2: case 3:
		{
			teleport_player(200, FALSE);
			break;
		}

		case 4: case 5:
		{

			teleport_player_to(y, x);
			break;
		}

		/* Note the exception above for quest levels */
		case 6:
		{
			if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You resist the effects!");
				break;
			}

			/* Teleport Level */
			(void)teleport_player_level(who);
			break;
		}

		case 7:
		{
			if (rand_int(100) < p_ptr->state.skills[SKILL_SAVE])
			{
				msg_print("You resist the effects!");
				break;
			}

			msg_print("Your body starts to scramble...");

			/* Pick a pair of stats */
			ii = rand_int(A_MAX);
			for (jj = ii; jj == ii; jj = rand_int(A_MAX)) /* loop */;

			max1 = p_ptr->stat_max[ii];
			cur1 = p_ptr->stat_cur[ii];
			max2 = p_ptr->stat_max[jj];
			cur2 = p_ptr->stat_cur[jj];

			p_ptr->stat_max[ii] = max2;
			p_ptr->stat_cur[ii] = cur2;
			p_ptr->stat_max[jj] = max1;
			p_ptr->stat_cur[jj] = cur1;

			p_ptr->update |= (PU_BONUS);

			break;
		}
	}
}


/*
 * Mega-Hack -- track "affected" monsters (see "project()" comments)
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;


/*
 * Temporarily light a grid.
 *
 * Memorise a monster or terrain if visible.
 *
 * We employ hacks here in order to temporarily make
 * the floor visible.
 *
 */
static bool temp_light(int y, int x)
{
	/* Grid is in line of sight */
	if (player_has_los_bold(y, x))
	{
		if (!(cave_info[y][x] & (CAVE_SEEN))
		    && !(p_ptr->timed[TMD_BLIND]))
		{
			/* Temporarily seen */
			cave_info[y][x] |= (CAVE_SEEN);

			/* Remember? */
			note_spot(y,x);

			/* Temporarily seen */
			cave_info[y][x] &= ~(CAVE_SEEN);

			/* Light? */
			light_spot(y,x);

			/* Get monster */
			if (cave_m_idx[y][x] > 0 )
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Detect all non-invisible monsters */
				if ((!(r_ptr->flags2 & (RF2_INVISIBLE)) || (p_ptr->state.see_inv)) &&
					(!(m_ptr->mflag & (MFLAG_HIDE))))
				{
					/* Optimize -- Repair flags */
					repair_mflag_mark = repair_mflag_show = TRUE;

					/* Hack -- Detect the monster */
					m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

					/* Update the monster */
					update_mon(cave_m_idx[y][x], FALSE);
				}
			}
		}

		/* Something seen */
		return (TRUE);
	}

	return (FALSE);
}


/*
 * Apply bonuses/penalties to the damage made by spells, based on terrain.
 * You should supply the native element flags of the target, since we
 * inhibite some increments to damage based on those flags.
 * This function shouldn't be used if the source of the spell is the terrain
 * itself.
 * Set verbose_mode to TRUE if you want a message telling what happened to
 * damage.
 */
static int terrain_adjust_damage(int dam, int spell_type, int y, int x,
	u32b target_native, bool verbose_mode)
{
	/* Get the element flags of the location */
	u32b terrain_element = cave_ff3_match(y, x, TERRAIN_MASK);

	/* Save the unmodified damage */
	int old_dam = MIN(dam, MAX_DAMAGE);

	/* Some spells get bonuses/penalties based on terrain */
	switch (spell_type)
	{
		case GF_ACID:
		{
			if ((terrain_element & (ELEMENT_ACID)) && !(target_native & (ELEMENT_ACID)))
			{
				dam += dam / 5;
			}

			break;
		}

		case GF_FIRE:
		{
			if ((terrain_element == (ELEMENT_LAVA)) && !(target_native & (ELEMENT_LAVA)))
			{
				if (one_in_(4)) dam += dam / 3;
				else dam += dam / 5;
			}
			else if (((terrain_element == (ELEMENT_FIRE)) ||
					(terrain_element == (ELEMENT_BMUD))) &&
				!(target_native & (ELEMENT_FIRE | ELEMENT_LAVA)))
			{
				dam += dam / 5;
			}
			else if ((terrain_element == (ELEMENT_BWATER)) &&
				!(target_native & (ELEMENT_FIRE | ELEMENT_LAVA)))
			{
				dam += dam / 10;
			}
			else if (terrain_element & (ELEMENT_WATER))
			{
				dam /= 2;
			}

			break;
		}

		case GF_COLD:
		case GF_ICE:
		{
			if ((terrain_element & (ELEMENT_ICE)) && !(target_native & (ELEMENT_ICE)))
			{
				dam += dam / 5;
			}
			else if ((terrain_element == (ELEMENT_WATER)) &&
				!(target_native & (ELEMENT_WATER | ELEMENT_ICE)))
			{
				dam += dam / 10;
			}
			else if ((spell_type != GF_ICE) &&
				(terrain_element & (ELEMENT_FIRE | ELEMENT_LAVA)))
			{
				dam /= 2;
			}

			break;
		}

		case GF_ELEC:
		{
			if ((terrain_element & (ELEMENT_WATER)) && !(target_native & (ELEMENT_WATER)))
			{
				dam += 2 * dam / 3;
			}

			break;
		}

		case GF_WATER:
		{
			if ((terrain_element & (ELEMENT_WATER)) && !(target_native & (ELEMENT_WATER)))
			{
				dam += dam / 7;
			}
			else if (terrain_element & (ELEMENT_LAVA | ELEMENT_ICE))
			{
				dam /= 2;
			}

			break;
		}

		case GF_SAND:
		{
			if ((terrain_element & (ELEMENT_SAND)) && !(target_native & (ELEMENT_SAND)))
			{
				dam += dam / 5;
			}
			else if (terrain_element & (ELEMENT_WATER))
			{
				dam /= 2;
			}

			break;
		}

		case GF_BWATER:
		{
			if ((terrain_element == (ELEMENT_BWATER)) &&
				((target_native & (ELEMENT_BWATER)) != (ELEMENT_BWATER)))
			{
				dam += dam / 7;
			}
			else if ((terrain_element & (ELEMENT_LAVA | ELEMENT_FIRE)) &&
				!(target_native & (ELEMENT_LAVA | ELEMENT_FIRE)))
			{
				dam += dam / 10;
			}
			else if ((terrain_element & (ELEMENT_WATER)) &&
				!(target_native & (ELEMENT_WATER)))
			{
				dam += dam / 10;
			}

			break;
		}

		case GF_BMUD:
		{
			if ((terrain_element == (ELEMENT_BMUD)) &&
				((target_native & (ELEMENT_BMUD)) != (ELEMENT_BMUD)))
			{
				dam += dam / 5;
			}
			else if ((terrain_element & (ELEMENT_LAVA | ELEMENT_FIRE)) &&
				!(target_native & (ELEMENT_LAVA | ELEMENT_FIRE)))
			{
				dam += dam / 10;
			}
			else if (terrain_element & (ELEMENT_WATER))
			{
				dam -= dam / 3;
			}

			break;
		}

		case GF_LAVA:
		{
			if ((terrain_element == (ELEMENT_LAVA)) && !(target_native & (ELEMENT_LAVA)))
			{
				dam += dam / 3;
			}
			else if ((terrain_element & (ELEMENT_FIRE | ELEMENT_LAVA)) &&
				!(target_native & (ELEMENT_LAVA | ELEMENT_FIRE)))
			{
				dam += dam / 10;
			}

			break;
		}

	}

	/* Check bounds */
	if (dam < 0) dam = 0;

	if (dam > MAX_DAMAGE) dam = MAX_DAMAGE;

	/* Check if we have to show a message */
	if ((dam != old_dam) && verbose_mode && player_can_see_bold(y, x) && !p_ptr->timed[TMD_BLIND])
	{
		char name[80];

		/* Get the feature's name */
		feature_desc(name, sizeof(name), cave_feat[y][x], FALSE, TRUE);

		/* Damage was increased */
		if (dam > old_dam)
		{
			if (dam >= (3 * old_dam / 2))
			{
				msg_format("The %s caused a GREAT boost to damage!", name);
			}
			else
			{
				msg_format("The %s increased the damage!", name);
			}
		}
		/* Damage was decreased */
		else
		{
			if (dam <= (old_dam / 2))
			{
				msg_format("The %s caused a MAJOR loss of damage!", name);
			}
			else
			{
				msg_format("The %s reduced the damage!", name);
			}
		}
	}

	/* Return adjusted damage */
	return (dam);
}


/*
 * We are called from "project()" to "damage" terrain features
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 */
static bool project_f(int who, int y, int x, int dist, int dam, int typ, int flg)
{
	bool hurt_feature = FALSE;
	int action = FS_FLAGS_END;

	bool obvious = FALSE;

	char name[80];

	/* Get the name */
	feature_desc(name, sizeof(name), cave_feat[y][x], FALSE, TRUE);

	/* Unused parameters */
	(void)dist;

	/* Adjust damage based on terrain */
	if (who != SOURCE_OTHER)
	{
		dam = terrain_adjust_damage(dam, typ, y, x, 0, FALSE);
	}

	/* Some features aren't always affected by certain attacks */
	switch (typ)
	{
		case GF_ACID:
		case GF_VAPOUR:
		{
			if (cave_ff2_match(y, x, FF2_HURT_ACID))
			{
				action = FS_HURT_ACID;
			}

			break;
		}
		case GF_FIRE:
		case GF_LAVA:
		case GF_PLASMA:
		{
			if (cave_ff2_match(y, x, FF2_HURT_FIRE))
			{
				action = FS_HURT_FIRE;
			}

			break;
		}
		case GF_COLD:
		case GF_ICE:
		{
			if (cave_ff2_match(y, x, FF2_HURT_COLD))
			{
				action = FS_HURT_COLD;
			}

			break;
		}
		case GF_ELEC:
		{
			if (cave_ff2_match(y, x, FF2_HURT_ELEC))
			{
				action = FS_HURT_ELEC;
			}

			break;
		}
		case GF_EXTINGUISH:
		{
			if (cave_ff2_match(y, x, FF2_HURT_COLD))
			{
				action = FS_HURT_COLD;
			}

			break;
		}
		case GF_WATER:
		{
			if (cave_ff2_match(y, x, FF2_HURT_WATER))
			{
				action = FS_HURT_WATER;
			}

			break;
		}
		case GF_STEAM:
		case GF_BWATER:
		{
			if (cave_ff3_match(y, x, FF3_HURT_BOIL_WATER))
			{
				action = FS_HURT_BWATER;
			}

			break;
		}
		case GF_POIS:
		{
			if (cave_ff3_match(y, x, FF3_HURT_POIS))
			{
				action = FS_HURT_POIS;
			}

			break;
		}
		case GF_KILL_WALL:
		{
			if (cave_ff2_match(y, x, FF2_HURT_ROCK))
			{
				/*
				 * Hack -- If the terrain has an explicit K: line for HURT_ROCK
				 * it can resists the stone to mud spell (example: sand dunes)
				 */
				if (feat_state_explicit_power(cave_feat[y][x], FS_HURT_ROCK) > 0)
				{
					action = FS_HURT_ROCK;
				}
				/*
				 * Usual case
				 */
				else
				{
					hurt_feature = TRUE;
				}
			}

			break;
		}
	}

	/* We have to fetch the power of the state action */
	if (action != FS_FLAGS_END)
	{
		/* Get the power */
		int power = feat_state_power(cave_feat[y][x], action);

		/* Analyze resistance to the action */
		if ((power < 1) || (rand_int(power) < dam))
		{
			/* The feature can't resist */
			hurt_feature = TRUE;
		}
	}

	/* Analyze the type */
	switch (typ)
	{
		/* Ignore most effects */
		case GF_ACID:
		case GF_VAPOUR:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_ACID);

					msg_format("The %s dissolves.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_ACID);
			}

			break;
		}
		case GF_FIRE:
		case GF_LAVA:
		case GF_PLASMA:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_FIRE);

					/* Show a proper message */
					if (cave_ff3_match(y, x, FF3_ICE))
					{
						msg_format("The %s melts.", name);
					}
					else if (cave_ff3_match(y, x, FF3_WATER))
					{
						msg_format("The %s evaporates.", name);
					}
					else
					{
 					msg_format("The %s burns up.", name);
					}

					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_FIRE);
			}

			if (temp_light(y, x)) obvious = TRUE;

			break;
		}
		case GF_COLD:
		case GF_ICE:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_COLD);

					msg_format("The %s freezes.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_COLD);
			}

			break;
		}
		case GF_ELEC:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_ELEC);

					msg_format("The %s is struck by lightening.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_ELEC);

			}

			if (temp_light(y, x)) obvious = TRUE;

			break;
		}
		case GF_EXTINGUISH:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_COLD);

					if (cave_ff3_match(y, x, FF3_FIRE))
					{
						msg_format("The %s is put out.", name);
					}
					else msg_format("The %s cools.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_COLD);
			}

			break;
		}
		/* Water */
		case GF_WATER:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_WATER);

					/* Show a proper message */
					if (cave_ff3_match(y, x, FF3_FIRE | FF3_LAVA))
					{
						msg_format("The %s is extinguished.", name);
					}
					else if (!cave_ff3_match(y, x, FF3_WATER))
					{
						msg_format("The %s floods.", name);
					}

					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_WATER);
			}

			break;
		}
		case GF_METEOR:
		case GF_SHARD:
		case GF_FORCE:
		case GF_SOUND:
		case GF_MANA:
		case GF_HOLY_ORB:
		{
			break;
		}
		/* Boiling water */
		case GF_STEAM:
		case GF_BWATER:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags3 |= (FF3_HURT_BOIL_WATER);

					msg_format("The %s evapourates.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_BWATER);
			}

			break;
		}
		case GF_MAKE_WALL:
		{
			int feat = FEAT_WALL_INNER;

			obvious = TRUE;

			/* If a monster there, leave it alone */
			if (cave_m_idx[y][x] > 0) break;

			/* First clear all effects */
			delete_effects(y, x);

			/* Now make it a wall, depending on the terrain below it */
			if (cave_ff3_match(y, x, FF3_WATER)) feat = FEAT_LIMESTONE;
			if (cave_ff3_match(y, x, FF3_SAND)) feat = FEAT_SANDSTONE;
			if (cave_ff3_match(y, x, FF3_ICE)) feat = FEAT_ICE_WALL;
			if (cave_ff3_match(y, x, FF3_OIL)) feat = FEAT_SHALE;
			if (cave_ff3_match(y, x, FF3_LAVA)) feat = FEAT_LAVA_W;
			if (cave_ff3_match(y, x, FF3_FOREST)) feat = FEAT_VINES;
			if (cave_ff3_match(y, x, FF3_ACID)) feat = FEAT_ACID_WALL;
			if (cave_ff3_match(y, x, (FF3_LAVA | FF3_WATER)) == (FF3_LAVA | FF3_WATER)) feat = FEAT_BWATER_WALL;
			if (cave_ff3_match(y, x, (FF3_LAVA | FF3_MUD)) == (FF3_LAVA | FF3_MUD)) feat = FEAT_BMUD_WALL;

			cave_set_feat(y, x, feat);

			break;
		}

		case GF_BMUD:
		{
			/*Do nothing??*/

			break;
		}
		case GF_SAND:
		{
			/*Do nothing??*/

			break;
		}
		case GF_POIS:
		{
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags3 |= (FF3_HURT_POIS);

					msg_format("The %s is poisoned.", name);
					obvious = TRUE;
				}

				/* Destroy the feature */
				cave_alter_feat(y, x, FS_HURT_POIS);
			}

			break;
		}

		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Reveal secret doors */
			if (cave_secret_door_bold(y, x))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					obvious = TRUE;
				}

				/* Create closed door */
				find_secret(y, x);
			}

			/* Destroy traps */
			if (cave_player_trap_bold(y, x))
			{
				effect_type *x_ptr = &x_list[cave_x_idx[y][x]];

				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[x_ptr->x_f_idx];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_DISARM);

					msg_print("There is a bright flash of light!");
					obvious = TRUE;
				}

				/* Destroy the trap */
				delete_effect_idx(cave_x_idx[y][x]);

				/* Forget the trap */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Redraw the grid */
				note_spot(y, x);

				light_spot(y, x);
			}

			/* Locked doors are unlocked */
			else if (cave_ff3_match(y, x, FF3_DOOR_LOCKED))
			{
				/* Check line of sound */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_OPEN);

					msg_print("Click!");

					obvious = TRUE;
				}

				/* Unlock the door */
				cave_alter_feat(y, x, FS_OPEN);
			}

			break;
		}

		/* Destroy Doors (and traps) */
		case GF_KILL_DOOR:
		{
			if (cave_door_bold(y, x) || cave_player_trap_bold(y, x))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Destroy the door */
					if (cave_door_bold(y, x))
					{
						/*Mark the feature lore*/
						feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
						f_l_ptr->f_l_flags1 |= (FF1_CAN_TUNNEL);
					}
					/* Destroy the trap */
					else
					{
						/*Mark the feature lore*/
						feature_lore *f_l_ptr = &f_l_list[x_list[cave_x_idx[y][x]].x_f_idx];
						f_l_ptr->f_l_flags1 |= (FF1_CAN_DISARM);
					}


					/* Message */
					msg_print("There is a bright flash of light!");
					obvious = TRUE;

				}

				/* Destroy the door */
				if (cave_door_bold(y, x))
				{
					cave_alter_feat(y, x, FS_TUNNEL);
				}
				/* Destroy the trap/effect*/
				else
				{
					/* Destroy the trap */
					delete_effect_idx(cave_x_idx[y][x]);

					/* Forget the trap */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Redraw the grid */
					note_spot(y, x);

					light_spot(y, x);
				}

				/* Fully update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS);

			}

			break;
		}

		/* Jam Doors */
		case GF_LOCK_DOOR:
		{
			/* Check doors */
			if (!cave_door_bold(y, x)) break;

			/* Close doors */
			if (cave_ff1_match(y, x, FF1_CAN_CLOSE))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_CLOSE);

					obvious = TRUE;
				}

				/* Close the door */
				cave_alter_feat(y, x, FS_CLOSE);

				/* Fully update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS);
			}

			/* Jam doors */
			while (cave_ff1_match(y, x, FF1_CAN_SPIKE))
			{
				int feat = cave_feat[y][x];

				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags1 |= (FF1_CAN_SPIKE);

					obvious = TRUE;
				}

				/* Jam the door */
				cave_alter_feat(y, x, FS_SPIKE);

				/* Paranoia */
				if (feat == cave_feat[y][x]) break;
			}

			break;
		}

		/* Destroy walls (and doors) */
		case GF_KILL_WALL:
		{
			/* Destroy walls/doors */
			if (hurt_feature)
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/*Mark the feature lore*/
					feature_lore *f_l_ptr = &f_l_list[cave_feat[y][x]];
					f_l_ptr->f_l_flags2 |= (FF2_HURT_ROCK);

					msg_format("The %s dissolves.", name);
					obvious = TRUE;

				}

				/* Destroy the wall/door */
				cave_alter_feat(y, x, FS_HURT_ROCK);

				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS  | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

				/* Make it a room, if called for. */
				if (flg & PROJECT_ROOM)
				{
					int d;

					/* Look in all directions. */
					for (d = 0; d < 8; d++)
					{
						/* Extract adjacent location */
						int yy = y + ddy_ddd[d];
						int xx = x + ddx_ddd[d];

						/* Ignore annoying locations */
						if (!in_bounds_fully(yy, xx)) continue;

						/* Make part of the room and light it up*/
						cave_info[yy][xx] |= (CAVE_ROOM | CAVE_GLOW);
					}
				}
			}

			break;
		}

		/* Make doors */
		case GF_MAKE_DOOR:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Create closed door */
			place_boring_closed_door(y, x);

			/* Observe */
			if (cave_info[y][x] & (CAVE_MARK)) obvious = TRUE;

			break;
		}

		/* Make features */
		case GF_FEATURE:
		{
			/* Require a "floor or ground" grid */
			if (!cave_ff1_match(y, x, FF1_FLOOR)) break;

			/* Don't hit caster */
			if (cave_m_idx[y][x] == who) break;

			/* Place a feature */
			if (dam) cave_set_feat(y, x, dam);

			/* Check line of sight */
			if (player_has_los_bold(y, x))
			{
				obvious = TRUE;
			}

			break;
		}

		/* Make bridge */
		case GF_BRIDGE:
		{
			int old_feat = cave_feat[y][x];

			if (cave_ff1_match(y, x, FF1_SECRET))
			{
				cave_alter_feat(y, x, FS_SECRET);
			}

			cave_alter_feat(y, x, FS_BRIDGE);

			feature_desc(name, sizeof(name), cave_feat[y][x],
				FALSE, TRUE);

			if (!strstr(name,"stone bridge"))
			{
				cave_set_feat(y, x, old_feat);
			}
			else if (player_has_los_bold(y, x))
			{
				obvious = TRUE;
			}

			break;
		}

		/* Lite up the grid */
		case GF_LIGHT_WEAK:
		case GF_LIGHT:
		{
			/* Turn on the light */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				if (!p_ptr->timed[TMD_BLIND])
				{
					/* Observe */
					obvious = TRUE;
				}

				/* Fully update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}

		/* Darken the grid */
		case GF_DARK_WEAK:
		case GF_DARK:
		{
			/* Turn off the light */
			cave_info[y][x] &= ~(CAVE_GLOW);

			/* Hack -- Forget "boring" grids */
			if (((cave_info[y][x] & (CAVE_MARK | CAVE_HALO)) == (CAVE_MARK)) &&
				!cave_ff1_match(y, x, FF1_REMEMBER) &&
				(!cave_any_trap_bold(y, x) ||
					(x_list[cave_x_idx[y][x]].x_flags &
					(EF1_HIDDEN))))
			{
				/* Forget */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Redraw */
				light_spot(y, x);

				/* Grid is in line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Observe */
					obvious = TRUE;

					/* Hack -- A monster is turning off the light */
					if ((who > SOURCE_MONSTER_START) && CHECK_DISTURB(TRUE))
					{
						/* Stop everything */
						disturb(1, 0);
					}

					/* Fully update the visuals */
					p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
				}
			}

			/* All done */
			break;
		}


	}

	/* Return "Anything seen?" */
	return (obvious);
}



/*
 * We are called from "project()" to "affect" objects
 *
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * Hack -- We also "see" objects which are "memorized".
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_o(int who, int y, int x, int dam, int typ)
{
	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;

	u32b f1, f2, f3, fn;

	char o_name[80];

	/* Glaciers and other unpassable effects protect objects */
	if (!cave_project_bold(y, x) && !cave_passable_bold(y, x)) return (FALSE);

	/* Adjust damage based on terrain */
	if (who != SOURCE_OTHER)
	{
		dam = terrain_adjust_damage(dam, typ, y, x, 0, FALSE);
	}

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;
		object_kind *k_ptr;

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool plural = FALSE;
		bool do_kill = FALSE;
		bool kill_art = FALSE;

		cptr note_kill = NULL;

		/* Get the object & object type*/
		o_ptr = &o_list[this_o_idx];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3, &fn);

		/* Get the "plural"-ness */
		if (o_ptr->number > 1) plural = TRUE;

		/* Check for artifact */
		if (artifact_p(o_ptr)) is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{
			/* Acid -- Lots of things */
			case GF_ACID:
			{

				if (hates_acid(o_ptr) && dam > rand_int(50))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (f3 & (TR3_IGNORE_ACID)) ignore = TRUE;
					if (fn & (ELEMENT_ACID)) ignore = TRUE;
				}
				break;
			}

			/* Elec -- Rings, Rods, Wands */
			case GF_ELEC:
			{

				if (hates_elec(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire -- Flammable objects */
			case GF_FIRE:
			{

				if (hates_fire(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA | ELEMENT_FIRE)) ignore = TRUE;
				}
				break;
			}

			/* Cold -- potions and flasks */
			case GF_COLD:
			{

				if (hates_cold(o_ptr) && dam > rand_int(40))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (f3 & (TR3_IGNORE_COLD)) ignore = TRUE;
					if (fn & (ELEMENT_ICE)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Elec */
			case GF_PLASMA:
			{
				if (hates_fire(o_ptr) && (dam > rand_int(40)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA)) ignore = TRUE;
				}
				if (hates_elec(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Cold */
			case GF_METEOR:
			{
				if (hates_fire(o_ptr) && (dam > rand_int(80)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
					if (fn & (ELEMENT_LAVA)) ignore = TRUE;
				}
				if (hates_cold(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " shatter!" : " shatters!");
					if (f3 & (TR3_IGNORE_COLD)) ignore = TRUE;
				}
				break;
			}

			/* Hack -- break potions and such */
			case GF_ICE:
			case GF_SHARD:
			case GF_FORCE:
			case GF_SOUND:
			{
				if (hates_cold(o_ptr) && (dam > rand_int(40)))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}

			/* Mana -- destroys everything */
			case GF_MANA:
			{
				do_kill = TRUE;
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
				break;
			}

			/* Drains charges of staves/wands, activatable objects/ego items/artifacts */
			case GF_STATIC:
			{
				if (dam > rand_int(k_ptr->k_level * 2))
				{

					/*
				 	 * No messages needed.
				 	 * We do not notice this with objects sitting on the ground.
					 */
					if (item_tester_hook_activate(o_ptr))
					{

						(void)drain_activation(o_ptr, dam);
					}
					/* Drain charged wands/staffs */
					else if (item_tester_hook_recharge(o_ptr))
					{
						(void)drain_charges(o_ptr, 0);
					}
				}
				break;
			}


			/* Holy Orb -- destroys cursed non-artifacts */
			case GF_HOLY_ORB:
			{
				if (cursed_p(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

			/* Unlock chests */
			case GF_KILL_TRAP:
			case GF_KILL_DOOR:
			{
				/* Chests are noticed only if trapped or locked, and not special ches items */
				if ((o_ptr->tval == TV_CHEST) && !(o_ptr->ident & (IDENT_QUEST)))
				{
					/* Disarm/Unlock traps */
					if (o_ptr->pval > 0)
					{
						/* Disarm or Unlock */
						o_ptr->pval = (0 - o_ptr->pval);

						/* Identify */
						object_known(o_ptr);

						/* Notice */
						if (o_ptr->marked)
						{
							msg_print("Click!");
							obvious = TRUE;
						}
					}
				}

				break;
			}

			/* Mass-identify */
			case GF_MASS_IDENTIFY:
			{
			  	int squelch;

				/* Ignore hidden objects */
			  	if (!o_ptr->marked) continue;

				/*Don't identify gold*/
				if (o_ptr->tval == TV_GOLD) continue;

				/* Ignore known objects */
				if (object_known_p(o_ptr)) continue;

			  	/* Identify object and get squelch setting */
				/* Note the first argument */
			  	squelch = do_ident_item(-1, o_ptr);

				/* Redraw purple dots */
				light_spot(y, x);

				/* Squelch? */
				if (squelch == SQUELCH_YES) do_kill = TRUE;

 				break;
			}

			case GF_SAND:
			{
				if (hates_sand(o_ptr) && dam > rand_int(150))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

			case GF_BMUD:
			{
				if (hates_boiling_mud(o_ptr) && dam > rand_int(75))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

			/* Make wall kills everything, even artifacts */
			case GF_MAKE_WALL:
			{
				do_kill = TRUE;
				kill_art = TRUE;
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
				break;
			}

			case GF_BWATER:
			{
				if (hates_boiling_water(o_ptr) && dam > rand_int(150))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}


			case GF_LAVA:
			{
				if (hates_lava(o_ptr) && dam > rand_int(150))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

		}

		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
				object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);
			}

			/* Artifacts, and other objects, get to resist */
			if ((is_art || ignore) && (!kill_art))
			{
				/* Observe the resist */
				if (o_ptr->marked)
				{
					msg_format("The %s %s unaffected!",
					           o_name, (plural ? "are" : "is"));
				}
			}

			/* The mimics cover is blown */
			else if (o_ptr->mimic_r_idx)
			{
				reveal_mimic(this_o_idx, o_ptr->marked);
			}

			/* Kill it */
			else
			{
				/* Describe if needed */
				if (o_ptr->marked && note_kill)
				{
					message_format(MSG_DESTROY, 0, "The %s%s", o_name, note_kill);
				}

				/* Delete the object */
				delete_object_idx(this_o_idx);

				/* Redraw */
				light_spot(y, x);
			}
		}
	}

	/* For mass_identify, squelch the pile to get rid of junk */
	if (typ == GF_MASS_IDENTIFY) do_squelch_pile(y, x);

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to a monster.
 *
 * Note that this routine can handle "no damage" attacks (like teleport) by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.  Note that the "damage" parameter is
 * divided by the radius, so monsters not at the "epicenter" will not take
 * as much damage (or whatever)...
 *
 *
 * Various messages are produced, and damage is applied.
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" big balls of it.
 *
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.  If can breath nether, then it resists it as well.
 *
 * Damage reductions use the following formulas:
 *   Note that "dam = dam * 6 / (randint(6) + 6);"
 *     gives avg damage of .655, ranging from .858 to .500
 *   Note that "dam = dam * 5 / (randint(6) + 6);"
 *     gives avg damage of .544, ranging from .714 to .417
 *   Note that "dam = dam * 4 / (randint(6) + 6);"
 *     gives avg damage of .444, ranging from .556 to .333
 *   Note that "dam = dam * 3 / (randint(6) + 6);"
 *     gives avg damage of .327, ranging from .427 to .250
 *   Note that "dam = dam * 2 / (randint(6) + 6);"
 *     gives something simple.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "useful" happen.
 */
bool project_m(int who, int y, int x, int damage, int typ, u32b flg)
{
	int tmp;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	int mon_idx = cave_m_idx[y][x];

	cptr name;

	/* Is the monster "seen"? */
	bool seen = FALSE;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

	/* Were the effects "irrelevant"? */
	bool skipped = FALSE;

	/* Did the monster die? */
	bool mon_died = FALSE;

	/* Polymorph setting (true or false) */
	bool do_poly = FALSE;

	/* Teleport setting (max distance) */
	int do_dist = 0;

	/* Confusion setting (amount to confuse) */
	int do_conf = 0;
	int conf_note = 0;

	/* Stunning setting (amount to stun) */
	int do_stun = 0;
	int stun_note = 0;

	/* Slow setting (amount to haste) */
	int do_slow = 0;
	int slow_note = 0;

	/* Haste setting (amount to haste) */
	int do_haste = 0;
	int haste_note = 0;

	/* Sleep amount (amount to sleep) */
	int do_sleep = 0;
	int sleep_note = 0;

	/* Fear amount (amount to fear) */
	int do_fear = 0;
	int fear_note = 0;

	/* Hold the monster name */
	char m_name[80];

	/* Assume no note */
	int m_note = MON_MSG_NONE;

	/* Assume a default death */
	byte note_dies = MON_MSG_DIE;

	int timed_flag = 0L;

	/* Unused parameter*/
	(void)flg;

	/* Walls protect monsters */
	if ((who != SOURCE_OTHER) && !cave_project_bold(y, x) && !cave_passable_bold(y, x)) return (FALSE);

	/* No monster here */
	if (!mon_idx) return (FALSE);

	/* Never affect projector */
	if (mon_idx == who) return (FALSE);

	if ((who != SOURCE_PLAYER)) timed_flag |= MON_TMD_MON_SOURCE;

	/* Obtain monster info */
	m_ptr = &mon_list[mon_idx];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];
	name = r_ptr->name_full;
	if (m_ptr->ml) seen = TRUE;

	/*
	 * Hack - place some limitations on monsters hurting other monsters.
	 *
	 * This is mostly to allow breathers to breathe without
	 */
	if ((flg & (PROJECT_SAME)) && (who >= SOURCE_MONSTER_START))
	{
		monster_type *m2_ptr = &mon_list[who];
		monster_race *r2_ptr = &r_info[m2_ptr->r_idx];

		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Monsters are same race, or uniques suffer no damage */
		if ((m_ptr->r_idx == m2_ptr->r_idx) || (r_ptr->flags1 & (RF1_UNIQUE)) ||
			(race_similar_breaths(r_ptr, r2_ptr)) || (m_ptr->mflag & (MFLAG_QUEST)))
		{
			if (seen) add_monster_message(m_name, mon_idx, MON_MSG_UNAFFECTED);
			return(FALSE);
		}

		/*
		 * Damage greatly reduced for similar monsters and similar breathers.
		 *
		 * (Such as, an ancient blue dragon getting caught in the breath
		 *  of a medium red dragon.
		 */
		if ((race_similar_monsters(who, y, x)) || race_breathes_element(r_ptr, typ))
		{
			damage /= 3;
		}
	}

	/* If we need a monster to track, use this one. */
	if ((!p_ptr->health_who) && (who != SOURCE_OTHER) && (seen)) health_track(mon_idx);

	/* Special case. Hidden monsters */
	if (m_ptr->mflag & (MFLAG_HIDE))
	{
		if (!(flg & (PROJECT_GRID))) return (FALSE);
	}

	/* Some monsters get "destroyed" */
	if (monster_nonliving(r_ptr))
	{
		/* Special note at death */
		note_dies = MON_MSG_DESTROYED;
	}

	/* Monster goes active */
	m_ptr->mflag |= (MFLAG_ACTV);

	/*Mark the monster as attacked by the player, if the player is in sight. */
	if ((who == SOURCE_PLAYER) && player_has_los_bold(y, x)) m_ptr->mflag |= (MFLAG_HIT_BY_RANGED);

	/* Adjust damage based on terrain */
	if (who != SOURCE_OTHER)
	{
		damage = terrain_adjust_damage(damage, typ, y, x, r_ptr->r_native, (who == SOURCE_PLAYER));
	}

	/* Analyze the damage type */
	switch (typ)
	{
		/* Magic Missile -- pure damage */
		case GF_ARROW:
		case GF_MISSILE:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Acid */
		case GF_ACID:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_IM_ACID)) || (r_ptr->r_native & (RN1_N_ACID)))
			{
				m_note = MON_MSG_RESIST_A_LOT;
				damage /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_ACID);
			}
			else if (r_ptr->flags3 & (RF3_HURT_ACID))
			{
				m_note = MON_MSG_BADLY_BURNED;
				note_dies = MON_MSG_MELTS_AWAY;
				if (game_mode == GAME_NPPMORIA) damage = damage * 3 / 2;
				else damage = damage * 4 / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_ACID);
			}
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				m_note = MON_MSG_RESIST_A_LOT;
				damage /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_ELEC);
			}
			break;
		}

		/* Fire damage */
		case GF_FIRE:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_IM_FIRE)) || (r_ptr->r_native & (RN1_N_FIRE)))
			{
				m_note = MON_MSG_RESIST_A_LOT;
				damage /= 9;
				if (seen)
				{
					if (r_ptr->flags3 & (RF3_IM_FIRE)) l_ptr->r_l_flags3 |= (RF3_IM_FIRE);
					if (r_ptr->r_native & (RN1_N_FIRE))l_ptr->r_l_native |= (RN1_N_FIRE);
				}
			}
			else if (r_ptr->flags3 & (RF3_HURT_FIRE))
			{
				m_note = MON_MSG_CATCH_FIRE;
				note_dies = MON_MSG_DISENTEGRATES;
				if (game_mode == GAME_NPPMORIA) damage = damage * 3 / 2;
				else damage = damage * 4 / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_FIRE);
			}
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				m_note = MON_MSG_RESIST_A_LOT;
				damage /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_COLD);
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
				m_note = MON_MSG_BADLY_FROZEN;
				note_dies = MON_MSG_FREEZE_SHATTER;
				if (game_mode == GAME_NPPMORIA) damage = damage * 3 / 2;
				else damage = damage * 4 / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_COLD);
			}
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				m_note = MON_MSG_RESIST_A_LOT;
				damage /= 9;
				if (seen) l_ptr->r_l_flags3 |= (RF3_IM_POIS);
			}
			else if (r_ptr->flags3 & (RF3_HURT_POIS))
			{
				m_note = MON_MSG_BADLY_POISONED;
				note_dies = MON_MSG_CHOKE_DIE;
				if (game_mode == GAME_NPPMORIA) damage = damage * 3 / 2;
				else damage = damage * 4 / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_POIS);
			}
			break;
		}

		/* Holy Orb -- hurts Evil */
		case GF_HOLY_ORB:
		{

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				damage *= 2;
				m_note = MON_MSG_HIT_HARD;
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Plasma -- perhaps check ELEC or FIRE XXX */
		case GF_PLASMA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_PLAS))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_PLAS);
			}
			else if (prefix(name, "Plasma") ||
			    (r_ptr->flags4 & (RF4_BRTH_PLAS)))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if ((seen) && (r_ptr->flags4 & (RF4_BRTH_PLAS)))
					l_ptr->r_l_flags4 |= (RF4_BRTH_PLAS);
			}
			break;
		}

		/* Nether -- see above */
		case GF_NETHER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				m_note = MON_MSG_IMMUNE;
				damage = 0;
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);
			}
			else if (r_ptr->flags3 & (RF3_RES_NETHR))

			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_NETHR);
			}

			else if (r_ptr->flags4 & (RF4_BRTH_NETHR))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_NETHR);
			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				damage /= 2;
				m_note = MON_MSG_RESIST_SOMEWHAT;
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Water (acid) damage -- Water spirits/elementals are immune */
		case GF_WATER:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->d_char == 'E') && prefix(name, "W"))
			{
				m_note = MON_MSG_IMMUNE;
				damage = 0;
			}
			else if ((r_ptr->flags3 & (RF3_RES_WATER)) ||
					 (r_ptr->r_native & (RN1_N_WATER)))
			{
				m_note = MON_MSG_RESIST;
				damage /= 9;
				if (seen)
				{
					if (r_ptr->flags3 & (RF3_RES_WATER)) l_ptr->r_l_flags3 |= (RF3_RES_WATER);
					if (r_ptr->r_native & (RN1_N_WATER)) l_ptr->r_l_native |= (RN1_N_WATER);
				}

			}
			break;
		}

		/* Chaos -- Chaos breathers resist */
		case GF_CHAOS:
		{
			if (seen) obvious = TRUE;
			do_poly = TRUE;

			do_conf = rand_range(15, 15 + (damage > 600 ? 30 : damage / 20));

			if (r_ptr->flags3 & (RF3_RES_CHAOS))
			{
				damage *= 3; damage /= (randint(6)+6);
				do_poly = FALSE;
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_CHAOS);
			}
			else if (r_ptr->flags4 & (RF4_BRTH_CHAOS))
			{
				damage *= 3; damage /= (randint(6)+6);
				do_poly = FALSE;
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_CHAOS);
			}
			break;
		}

		/* Shards -- Shard breathers resist */
		case GF_SHARD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_SHARD))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_SHARD);
			}
			break;
		}

		/* Sound -- Sound breathers resist */
		case GF_SOUND:
		{
			if (seen) obvious = TRUE;
			do_stun = randint(damage > 300 ? 15 : damage / 20);
			if (r_ptr->flags4 & (RF4_BRTH_SOUND))
			{
				m_note = MON_MSG_RESIST;
				damage *= 2; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_SOUND);
			}
			break;
		}

		/* Confusion */
		case GF_CONFUSION:
		{
			if (seen) obvious = TRUE;
			do_conf = rand_range(20, 20 + (damage > 300 ? 15 : damage / 20));
			if (r_ptr->flags4 & (RF4_BRTH_CONFU))
			{
				damage *= 2; damage /= (randint(6)+6);
				l_ptr->r_l_flags4 |= (RF4_BRTH_CONFU);
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{

				damage /= 2;
				l_ptr->r_l_flags3 |= (RF3_NO_CONF);
			}
			break;
		}

		/* Disenchantment -- Breathers and Disenchanters resist */
		case GF_DISENCHANT:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_DISEN))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_DISEN);
			}
			else if ((r_ptr->flags4 & (RF4_BRTH_DISEN)) ||
			    prefix(name, "Disen"))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen)l_ptr->r_l_flags4 |= (RF4_BRTH_DISEN);
			}
			break;
		}

		/* Keeps monsters from multiplying */
		case GF_STERILIZE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags2 & (RF2_MULTIPLY))
			{
				m_note = MON_MSG_STERILIZE;
				m_ptr->mflag |= MFLAG_STERILE;
				if (seen) l_ptr->r_l_flags2 |= (RF2_MULTIPLY);
			}

			damage = 0;

			break;
		}

		/* Nexus -- Breathers and Existers resist */
		case GF_NEXUS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_RES_NEXUS))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags3 |= (RF3_RES_NEXUS);
			}
			else if ((r_ptr->flags4 & (RF4_BRTH_NEXUS)) ||
			    prefix(name, "Nexus"))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_NEXUS);
			}
			break;
		}

		/* Force */
		case GF_FORCE:
		{
			if (seen) obvious = TRUE;
			if (one_in_(2)) do_stun = randint(damage > 240 ? 20 : damage / 12);
			if (r_ptr->flags4 & (RF4_BRTH_FORCE))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_FORCE);
			}
			break;
		}

		/* Inertia -- breathers resist */
		case GF_INERTIA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_INER))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_INER);
			}

			do_slow = rand_int(4) + 4;

			break;
		}

		/* Drains monsters mana */
		case GF_STATIC:
		{
			int drain = MAX(damage / 25,1);

			if (seen) obvious = TRUE;
			if (m_ptr->mana > 0)
			{

				if (drain > m_ptr->mana) m_ptr->mana = 0;
				else m_ptr->mana -= drain;

				m_note = MON_MSG_MANA_DRAIN;

			}

			break;
		}


		/* Time -- breathers resist */
		case GF_TIME:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_TIME))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_TIME);
			}
			break;
		}

		/* Gravity -- breathers resist */
		case GF_GRAVITY:
		{
			if (seen) obvious = TRUE;

			/* Higher level monsters can resist the teleportation better */
			if (randint(127) > r_ptr->level) do_dist = 10;

			if (r_ptr->flags4 & (RF4_BRTH_GRAV))
			{
				m_note = MON_MSG_RESIST;
				damage *= 3; damage /= (randint(6)+6);
				do_dist = 0;
				if (seen) l_ptr->r_l_flags4 |= (RF4_BRTH_GRAV);
			}

			do_slow = rand_int(4) + 4;

			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Meteor -- powerful magic missile */
		case GF_METEOR:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Ice -- Cold + Cuts + Stun */
		case GF_ICE:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_IM_COLD)) || (r_ptr->r_native & RN1_N_ICE))
			{
				m_note = MON_MSG_RESIST_A_LOT;
				damage /= 9;
				if (seen)
				{
					if (r_ptr->flags3 & (RF3_IM_COLD)) l_ptr->r_l_flags3 |= (RF3_IM_COLD);
					if (r_ptr->r_native & (RN1_N_ICE)) l_ptr->r_l_native |= (RN1_N_ICE);
				}
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
				m_note = MON_MSG_HIT_HARD;
				damage  = (damage * 4) / 3;
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_COLD);
			}
			if ((r_ptr->flags3 & (RF3_IM_COLD)) == 0)
			{
				do_stun = randint(damage > 240 ? 12 : damage / 20);
			}

			break;
		}

		/* Drain Life */
		case GF_LIFE_DRAIN:
		{
			if (seen) obvious = TRUE;
			if (monster_nonliving(r_ptr))
			{
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				}
				if (r_ptr->flags3 & (RF3_DEMON))
				{
					if (seen) l_ptr->r_l_flags3 |= (RF3_DEMON);
				}

				m_note = MON_MSG_UNAFFECTED;
				obvious = FALSE;
				damage = 0;
			}

			break;
		}

		/* Polymorph monster (Use "dam" as "power") */
		case GF_OLD_POLY:
		{
			if (seen) obvious = TRUE;

			/* Attempt to polymorph (see below) */
			do_poly = TRUE;

			/* Powerful monsters and questors resist */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || (m_ptr->mflag & (MFLAG_QUEST)) ||
			    (r_ptr->level > randint((damage - 10) < 1 ? 1 : (damage - 10)) + 10))
			{
				m_note = MON_MSG_UNAFFECTED;
				do_poly = FALSE;
				obvious = FALSE;
			}

			/* No "real" damage */
			damage = 0;

			break;
		}


		/* Clone monsters (Ignore "dam") */
		case GF_OLD_CLONE:
		{
			if (seen) obvious = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Speed up */
			do_haste = 25 + rand_int(25);

			/* Attempt to clone. */
			if (multiply_monster(mon_idx, TRUE))
			{
				m_note = MON_MSG_SPAWN;
			}

			/* No "real" damage */
			damage = 0;

			break;
		}


		/* Heal Monster (use "dam" as amount of healing) */
		case GF_OLD_HEAL:
		{
			bool healed = TRUE;

			/*does monster need healing?*/
			if (m_ptr->hp == m_ptr->maxhp) healed = FALSE;

			if (seen) obvious = TRUE;

			/* Wake up */
			wake_monster_attack(m_ptr, MON_TMD_FLG_NOTIFY);

			/* Monster goes active */
			m_ptr->mflag |= (MFLAG_ACTV);

			/* Heal */
			m_ptr->hp += damage;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if ((p_ptr->health_who == mon_idx)  || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);

			/*monster was at full hp to begin*/
			if (!healed)
			{
				obvious = FALSE;

				m_note = MON_MSG_UNAFFECTED;

			}

			/* Message */
			else m_note = MON_MSG_HEALTHIER;

			/* No "real" damage */
			damage = 0;

			/* If it just woke up, update the monster list */
			p_ptr->redraw |= PR_MONLIST;

			break;
		}


		/* Speed Monster (Ignore "dam") */
		case GF_OLD_SPEED:
		{

			if (seen) obvious = TRUE;

			/* Speed up */
			do_haste = 50 + rand_int(50);

			/* No "real" damage */
			damage = 0;
			break;
		}


		/* Slow Monster (Use "dam" as "power") */
		case GF_OLD_SLOW:
		{
			if (seen) obvious = TRUE;

			do_slow = damage;

			/* No "real" damage */
			damage = 0;
			break;
		}

		/*
		 * Sleep (Use "dam" as "power")
		 *
		 * See comments for GF_OLD_SLOW. -AR
		 */
		case GF_OLD_SLEEP:
		{
			if (seen) obvious = TRUE;

			/*
			 * No "real" damage, but damage is
			 * used to determine the length of the sleep.
			 */
			do_sleep = damage;
			damage = 0;
			break;
		}

		/*
		 * Confusion (Use "dam" as "power")
		 *
		 * See comments for GF_OLD_SLOW
		 */
		case GF_OLD_CONF:
		{
			if (seen) obvious = TRUE;

			/* Get confused later */
			do_conf = damroll(3, (damage / 2)) + 1;

			if (do_conf > 300) do_conf = 300;

			/* No "real" damage */
			damage = 0;
			break;
		}

		/* Lite, but only hurts susceptible creatures */
		case GF_LIGHT_WEAK:
		{
			/* Hurt by light */
			if (r_ptr->flags3 & (RF3_HURT_LIGHT))
			{
				/* Obvious effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_LIGHT);

				/* Special effect */
				m_note = MON_MSG_CRINGE_LIGHT;
				note_dies = MON_MSG_SHRIVEL_LIGHT;
			}

			/* Normally no damage */
			else
			{
				/* No damage */
				damage = 0;
			}

			break;
		}


		/*
		 * Lite -- opposite of Dark
		 * Different kinds of light sensitive
		 * monsters react differently to hard light.
		 *
		 * Undead or incorporeal monsters are particulary
		 * vulnerable, those with both traits especially
		 * so.
		 *
		 * A monster that is light sensitive and/or undead and/or
		 * incorporeal can have the damage multiplied by anywhere
		 * between 3/2 to 5/2. -AR
		 */
		case GF_LIGHT:
		{
			int multiplier = 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_LIGHT))
			{
				m_note = MON_MSG_RESIST;
				damage *= 2; damage /= (randint(6)+6);
			}

			if (r_ptr->flags3 & (RF3_HURT_LIGHT))
			{
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_LIGHT);
				m_note = MON_MSG_CRINGE_LIGHT;
				note_dies = MON_MSG_SHRIVEL_LIGHT;
				multiplier++;
			}

			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				/* As a perk, hard light reveals the Undead flag*/
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				multiplier++;
			}

			/* Light breathers are not penalised for being incorporeal, all
			 * other incorporeals are due to the light not just hitting them
			 * but engulfing them. -AR
			 */
			if ((r_ptr->flags3 & (RF2_PASS_WALL)) & !(r_ptr->flags4 & (RF4_BRTH_LIGHT)))
			{
				/* As a perk, hard light reveals incorporeality*/
				if (seen) l_ptr->r_l_flags3 |= (RF2_PASS_WALL);
				multiplier++;
			}

			damage = (multiplier * damage) / 2;

			break;
		}


		/* Dark -- opposite of Lite */
		case GF_DARK:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_DARK))
			{
				m_note = MON_MSG_RESIST;
				damage *= 2; damage /= (randint(6)+6);
			}
			break;
		}


		/* Stone to Mud */
		case GF_KILL_WALL:
		{
			/* Hurt by rock remover */
			if (r_ptr->flags3 & (RF3_HURT_ROCK))
			{
				/* Notice effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) l_ptr->r_l_flags3 |= (RF3_HURT_ROCK);

				/* Cute little message */
				m_note = MON_MSG_LOSE_SKIN;
				note_dies = MON_MSG_DISSOLVE;
			}

			/* Usually, ignore the effects */
			else
			{
				/* No damage */
				damage = 0;
			}

			break;
		}

		case GF_MAKE_WALL:
		{
			/* Healed by make wall */
			if (r_ptr->flags2 & (RF2_KILL_WALL))
			{
				bool healed = TRUE;

				/*does monster need healing?*/
				if (m_ptr->hp == m_ptr->maxhp) healed = FALSE;

				/* Notice effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) l_ptr->r_l_flags2 |= (RF2_KILL_WALL);

				/* Wake up */
				wake_monster_attack(m_ptr, MON_TMD_FLG_NOTIFY);

				/* Monster goes active */
				m_ptr->mflag |= (MFLAG_ACTV);

				/* Heal */
				m_ptr->hp += damage;

				/* No overflow */
				if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

				/* Redraw (later) if needed */
				if ((p_ptr->health_who == mon_idx)  || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);

				/*monster was at full hp to begin*/
				if (!healed)
				{
					m_note = MON_MSG_UNAFFECTED;
				}

				/* Message */
				else m_note = MON_MSG_HEALTHIER;

				/* No "real" damage */
				damage = 0;

				/* If it just woke up, update the monster list */
				p_ptr->redraw |= PR_MONLIST;
				break;
			}

			/* Monsters who pass wall are unaffected */
			else if (r_ptr->flags2 & (RF2_PASS_WALL))
			{
				m_note = MON_MSG_UNAFFECTED;

				/* No "real" damage */
				damage = 0;

				/* Notice effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) l_ptr->r_l_flags2 |= (RF2_PASS_WALL);

				/* Wake up */
				wake_monster_attack(m_ptr, MON_TMD_FLG_NOTIFY);

				/* Monster goes active */
				m_ptr->mflag |= (MFLAG_ACTV);

				break;
			}

			/* Normal monsters */
			if (seen) obvious = TRUE;

			note_dies = MON_MSG_BURIED_ROCK;

			break;
		}


		/* Teleport undead (Use "dam" as "power") */
		case GF_AWAY_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				if (seen) obvious = TRUE;
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);
				do_dist = damage;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			damage = 0;
			break;
		}


		/* Teleport evil (Use "dam" as "power") */
		case GF_AWAY_EVIL:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				if (seen) obvious = TRUE;
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);
				do_dist = damage;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			damage = 0;
			break;
		}


		/* Teleport monster (Use "dam" as "power") */
		case GF_AWAY_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Prepare to teleport */
			do_dist = damage;

			/* No "real" damage */
			damage= 0;
			break;
		}


		/* Turn undead (Use "dam" as "power") */
		case GF_TURN_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (damage / 2)) + 1;

			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			damage = 0;
			break;
		}


		/* Turn evil (Use "dam" as "power") */
		case GF_TURN_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (damage / 2)) + 1;

			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			damage = 0;
			break;
		}


		/*
		 * Turn monster (Use "dam" as "power")
		 *
		 * I didn't modify the limited fear effects,
		 * aside from the boost to the charisma adjustment,
		 * because they ignore the NO_FEAR flag anyway.
		 */
		case GF_TURN_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Apply some fear */
			do_fear = damroll(3, (damage / 2)) + 1;

			/* No "real" damage */
			damage = 0;
			break;
		}


		/* Dispel undead */
		case GF_DISP_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				m_note = MON_MSG_SHUDDER;
				note_dies = MON_MSG_DISSOLVE;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				damage = 0;
			}

			break;
		}


		/* Dispel evil */
		case GF_DISP_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				/* Learn about type */
				if (seen) l_ptr->r_l_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
				m_note = MON_MSG_SHUDDER;
				note_dies = MON_MSG_DISSOLVE;
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				damage = 0;
			}

			break;
		}

		/* make_wary */
		case GF_MAKE_WARY:
		{
			/*Sleeping, confused, or afraid monsters never notice anything*/
			if ((m_ptr->m_timed[MON_TMD_SLEEP]) || (m_ptr->m_timed[MON_TMD_CONF]) ||
				(m_ptr->m_timed[MON_TMD_FEAR])) skipped = TRUE;

			/*certain monsters never notice anything either*/
			else if (r_ptr->flags2 & (RF2_STUPID)) skipped = TRUE;
			else if (r_ptr->flags1 & (RF1_NEVER_MOVE)) skipped = TRUE;
			else if (r_ptr->flags2 & (RF2_EMPTY_MIND)) skipped = TRUE;
			else if (r_ptr->flags2 & (RF2_WEIRD_MIND)) skipped = TRUE;
			else if ((strchr("BFIJKSXabceijlrsmvwjz,", r_ptr->d_char)) &&
				(!(r_ptr->flags2 & (RF2_SMART)))) skipped = TRUE;

			/*don't make monsters wary again to avoid the message*/
			else if (m_ptr->mflag & (MFLAG_WARY)) skipped = TRUE;;

			/* Obvious */
			if (seen) obvious = TRUE;

			/*don't mention anything if they are already*/
			if (!skipped)
			{

				m_ptr->mflag |= (MFLAG_WARY);

				/*senitent beings get a more human-like message*/
				if ((strchr("phntyPdDGLOoTuUvV", r_ptr->d_char)) || (r_ptr->flags2 & (RF2_SMART)))
				{
					if (one_in_(2))	m_note = MON_MSG_AWARE_OF_CRAFTY_ABILITIES;
					else m_note = MON_MSG_AWARE_OF_CUNNING_TACTICS;
				}

				/* or else a more message based on instinct*/
				else
				{
				    if (one_in_(2)) m_note = MON_MSG_SENSE_CRAFTY_ABILITIES;
					else m_note = MON_MSG_SENSE_CUNNING_FOE;
				}

			}

			break;
		}


		/* Dispel monster */
		case GF_DISP_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			m_note = MON_MSG_SHUDDER;
			note_dies = MON_MSG_DISSOLVE;

			break;
		}

		case GF_SAND:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if (r_ptr->r_native & (FF3_SAND))
			{
				m_note = MON_MSG_RESIST;
				damage /= 3;
				if (seen) l_ptr->r_l_native |= (FF3_SAND);
			}

			break;
		}
		case GF_BMUD:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if ((r_ptr->r_native & (ELEMENT_BMUD)) == ELEMENT_BMUD)
			{
				m_note = MON_MSG_RESIST;
				damage /= 9;
				if (seen) l_ptr->r_l_native |= (ELEMENT_BMUD);
			}

			break;
		}
		case GF_BWATER:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if ((r_ptr->r_native & (ELEMENT_BWATER)) == ELEMENT_BWATER)
			{
				m_note = MON_MSG_RESIST;
				damage /= 9;
				if (seen) l_ptr->r_l_native |= (ELEMENT_BWATER);
			}

			break;
		}
		case GF_LAVA:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			if (r_ptr->r_native & (FF3_LAVA))
			{
				m_note = MON_MSG_RESIST;
				damage /= 9;
				if (seen) l_ptr->r_l_native |= (ELEMENT_LAVA);
			}

			break;
		}


		/* Default */
		default:
		{
			/* Irrelevant */
			skipped = TRUE;

			/* No damage */
			damage = 0;

			break;
		}
	}

	/* Absolutely no effect */
	if (skipped) return (FALSE);

	/* "Unique" monsters or quest monsters cannot be polymorphed */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) || (m_ptr->mflag & (MFLAG_QUEST))) do_poly = FALSE;

	/* Get the actual monster name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Check for death */
	if (damage > m_ptr->hp)
	{
		/* Extract method of death */
		m_note = note_dies;
	}

	/* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
	else if (do_poly && (randint(90) > r_ptr->level))
	{
		/* Default -- assume no polymorph */
		m_note = MON_MSG_UNAFFECTED;

		/* Pick a "new" monster race */
		tmp = poly_r_idx(m_ptr);

		/* Handle polymorph */
		if (tmp != m_ptr->r_idx)
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Monster polymorphs */
			m_note = MON_MSG_CHANGE;

			/* Add the message now before changing the monster race */
			add_monster_message(m_name, mon_idx, m_note);

			/* No more messages */
			m_note = MON_MSG_NONE;

			/* Turn off the damage */
			damage = 0;

			/* "Kill" the "old" monster */
			delete_monster_idx(mon_idx);

			/* Create a new monster (no groups) */
			(void)place_monster_aux(y, x, tmp, MPLACE_OVERRIDE);

			/* Hack -- Assume success XXX XXX XXX */

			/* Hack -- Get new monster */
			m_ptr = &mon_list[cave_m_idx[y][x]];

			/* Hack -- Get new race */
			r_ptr = &r_info[m_ptr->r_idx];

			/* Hack -- Get the new monster name */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);

			if ((p_ptr->health_who == mon_idx) || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);
		}
	}

	/* Handle "teleport" */
	else if (do_dist)
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Native terrain grants resistance to hostile teleportation */
		if (cave_ff3_match(y, x, TERRAIN_MASK) && is_monster_native(y, x, r_ptr) && one_in_(5))
		{
			/*Mark the lore*/
			if ((m_ptr->ml) && player_can_observe() && (cave_info[y][x] & (CAVE_MARK)))
			{
				u32b native = f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3;
				native &= r_ptr->r_native;
				l_ptr->r_l_native |= native;
			}

			/* Message */
			m_note = MON_MSG_RESIST;
		}
		/* Teleport the monster */
		else if (teleport_away(cave_m_idx[y][x], do_dist))
		{
			/* Message */
			m_note = MON_MSG_DISAPPEAR;

			/* Hack -- get new location */
			y = m_ptr->fy;
			x = m_ptr->fx;
		}
	}

	/* Sound and Impact breathers never stun */
	else if (do_stun)
	{
		bool was_stunned = (m_ptr->m_timed[MON_TMD_STUN] ? TRUE : FALSE);

		/* Obvious */
		if (seen) obvious = TRUE;

		/* Do notes */
		if (mon_inc_timed(mon_idx, MON_TMD_STUN, do_stun, (timed_flag | MON_TMD_FLG_NOMESSAGE)))
		{
			if (was_stunned) stun_note = MON_MSG_MORE_DAZED;
			else stun_note = MON_MSG_DAZED;
		}

		else if (damage > 0)
		{
			/* No resist message. They will get a damage message instead */
		}

		/*some creatures are resistant to stunning*/
		else if ((r_ptr->flags3 & RF3_NO_STUN) || (r_ptr->flags4 & (RF4_BRTH_FORCE)))
		{
			stun_note = MON_MSG_UNAFFECTED;
		}

		else stun_note = MON_MSG_RESIST;

		/*possibly update the monster health bar*/
		if ((p_ptr->health_who == mon_idx) || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);

	}

	/* Confusion and Chaos breathers (and sleepers) never confuse */
	else if (do_conf)
	{
		bool was_confused = (m_ptr->m_timed[MON_TMD_CONF] ? TRUE : FALSE);

		/* Obvious */
		if (seen) obvious = TRUE;

		/* Apply confusion */
		if (mon_inc_timed(mon_idx, MON_TMD_CONF, do_conf, (timed_flag | MON_TMD_FLG_NOMESSAGE)))
		{
			if (was_confused) conf_note = MON_MSG_MORE_CONFUSED;
			/* Was not confused */
			else conf_note = MON_MSG_CONFUSED;
		}

		else if (damage > 0)
		{
			/* No message. They will get a damage message instead */
		}

		else if (typ == GF_OLD_CONF) conf_note = MON_MSG_UNAFFECTED;
		else if (r_ptr->flags4 & (RF4_BRTH_CONFU)) conf_note = MON_MSG_RESIST;
		else conf_note =  MON_MSG_RESIST_SOMEWHAT;

		if (p_ptr->health_who == mon_idx) p_ptr->redraw |= (PR_HEALTH);
	}

	/*Slowing*/
	else if (do_slow)
	{
		bool was_slowed = (m_ptr->m_timed[MON_TMD_SLOW] ? TRUE : FALSE);

		if(mon_inc_timed(mon_idx, MON_TMD_SLOW, do_slow, (timed_flag | MON_TMD_FLG_NOMESSAGE)))
		{
			if (was_slowed) slow_note = MON_MSG_MORE_SLOWED;
			else slow_note = MON_MSG_SLOWED;
		}
		else if (damage > 0)
		{
			/* No message. They will get a damage message instead */
		}
		else if (r_ptr->flags4 & (RF4_BRTH_GRAV))
		{
			slow_note = MON_MSG_UNAFFECTED;
		}
		else slow_note = MON_MSG_RESIST_SOMEWHAT;
	}

	/* Hasting */
	else if (do_haste)
	{
		bool was_hasted = (m_ptr->m_timed[MON_TMD_FAST] ? TRUE : FALSE);

		if (mon_inc_timed(mon_idx, MON_TMD_FAST, do_haste, (timed_flag | MON_TMD_FLG_NOMESSAGE)))
		{
			if (was_hasted) haste_note = MON_MSG_MORE_HASTED;
			else haste_note = MON_MSG_HASTED;
		}
	}

	/* Fear */
	if (do_fear)
	{

		bool was_afraid = (m_ptr->m_timed[MON_TMD_FEAR] ? TRUE : FALSE);
		if (mon_inc_timed(mon_idx, MON_TMD_FEAR, do_fear, (timed_flag | MON_TMD_FLG_NOMESSAGE)))
		{
			if (was_afraid) fear_note = MON_MSG_MORE_AFRAID;
			else fear_note = MON_MSG_FLEE_IN_TERROR;

			/*a monster can't be wary and afraid*/
			m_ptr->mflag &= ~(MFLAG_WARY);
			/* Monster is paniced */
			m_ptr->min_range = PANIC_RANGE;
		}
		/* Message only if no damage */
		else if (damage == 0) fear_note = MON_MSG_UNAFFECTED;
	}

	/* If another monster did the damage, hurt the monster by hand */
	if (who != SOURCE_PLAYER)
	{
		/* Redraw (later) if needed */
		if ((p_ptr->health_who == mon_idx) || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);

		/* Wake the monster up, no message */
		wake_monster_attack(m_ptr, (timed_flag | MON_TMD_FLG_NOMESSAGE));

		/* Monster goes active */
		m_ptr->mflag |= (MFLAG_ACTV);

		/* Hurt the monster */
		m_ptr->hp -= damage;

		/* Dead monster */
		if (m_ptr->hp < 0)
		{
			mon_died = TRUE;

			/* Give detailed messages if destroyed */
			if (!seen) note_dies = MON_MSG_MORIA_DEATH;

			/* dump the note*/
			add_monster_message(m_name, mon_idx, note_dies);

			/* Generate treasure, etc */
			monster_death(mon_idx, who);

			/* Delete the monster */
			delete_monster_idx(mon_idx);
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if ((m_note != MON_MSG_NONE) && seen)
			{
				/* dump the note*/
				add_monster_message(m_name, mon_idx, m_note);
			}

			/* Hack -- Pain message */
			else if (damage > 0)
			{
				message_pain(mon_idx, damage);
			}

			/* Hack -- handle sleep */
			if (do_sleep)
			{
				/* Sleep is applied as a percentage of "damage". */
				int sleep = (rand_range((r_ptr->sleep + 1) / 2, r_ptr->sleep) * do_sleep) / 100;

				if (mon_inc_timed(mon_idx, MON_TMD_SLEEP, sleep, (timed_flag | MON_TMD_FLG_NOMESSAGE)))
				{
					sleep_note = MON_MSG_FALL_ASLEEP;
				}
				/* No note if the monster was already asleep */
				else if (!m_ptr->m_timed[MON_TMD_SLEEP]) sleep_note = MON_MSG_UNAFFECTED;
			}
		}

		/* If it just woke up, update the monster list */
		p_ptr->redraw |= PR_MONLIST;
	}

	/* If the player did it, give him experience, check fear */
	else
	{
		bool fear = FALSE;

		/* The monster is going to be killed */
		if (damage > m_ptr->hp)
		{
			mon_died = TRUE;

			/* Adjust message for unseen monsters */
			if (!seen) note_dies = MON_MSG_MORIA_DEATH;

			/* Save the death notification for later */
			add_monster_message(m_name, mon_idx, note_dies);
		}

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(mon_idx, damage, &fear, "", who))
		{
			/* Dead monster. Empty statement */

		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if ((m_note != MON_MSG_NONE) && seen)
			{
				add_monster_message(m_name, mon_idx, m_note);
			}

			/* Hack -- Pain message */
			else if (damage > 0)
			{
				/* Player mesage pain */
				message_pain(mon_idx, damage);
			}

			/* Hack -- handle sleep */
			if (do_sleep)
			{
				/* Sleep is applied as a percentage of "damage". */
				int sleep = (rand_range((r_ptr->sleep + 1) / 2, r_ptr->sleep) * do_sleep) / 100;

				if (sleep_monster_spell(m_ptr, sleep, (timed_flag | MON_TMD_FLG_NOMESSAGE)))
				{
					sleep_note = MON_MSG_FALL_ASLEEP;
				}
				/* No note if the monster was already asleep */
				else if (!m_ptr->m_timed[MON_TMD_SLEEP]) sleep_note = MON_MSG_UNAFFECTED;
			}
		}
	}

	if (!mon_died)
	{

		/* Do the effect messags here so they appear after the pain messages */
		if (stun_note) add_monster_message(m_name, mon_idx, stun_note);
		if (conf_note) add_monster_message(m_name, mon_idx, conf_note);
		if (slow_note) add_monster_message(m_name, mon_idx, slow_note);
		if (haste_note) add_monster_message(m_name, mon_idx, haste_note);
		if (fear_note) add_monster_message(m_name, mon_idx, fear_note);
		if (sleep_note) add_monster_message(m_name, mon_idx, sleep_note);
	}

	/* Verify this code XXX XXX XXX */

	/* Update the monster */
	update_mon(mon_idx, FALSE);

	/* Redraw the monster grid */
	light_spot(y, x);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->redraw |= (PR_MONSTER);
	}

	/* Track it */
	project_m_n++;
	project_m_x = x;
	project_m_y = y;

	/* Hack -- Sound attacks are extremely noisy. */
	if (typ == GF_SOUND) add_wakeup_chance = 10000;

	/*
	 * Otherwise, if this is the first monster hit, the spell was capable
	 * of causing damage, and the player was the source of the spell,
	 * make noise. -LM-
	 */
	else if ((project_m_n == 1) && (who == SOURCE_PLAYER) && (damage))
	{
		add_wakeup_chance += p_ptr->base_wakeup_chance / 2 + 1000;
	}

	/* Return "Anything seen?" */
	return (obvious);
}






/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to the player.
 *
 * This routine takes a "source monster" (by index), a "distance", a default
 * "damage", and a "damage type".  See "project_m()" above.
 *
 * If "rad" is non-zero, then the blast was centered elsewhere, and the damage
 * is reduced (see "project_m()" above).  This can happen if a monster breathes
 * at the player and hits a wall instead.
 *
 * We return "TRUE" if any "obvious" effects were observed.
 *
 * Actually, for historical reasons, we just assume that the effects were
 * obvious.  XXX XXX XXX
 */
bool project_p(int who, int y, int x, int dam, int typ, cptr msg)
{
	int k = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->timed[TMD_BLIND] ? TRUE : FALSE);

	/* Monster name (for damage) */
	char killer[80];

	bool player_native = FALSE;

	/* Hack -- messages */
	cptr act = NULL;

	bool is_terrain = FALSE;

	/* No player here */
	if ((y != p_ptr->py) || (p_ptr->px != x)) return (FALSE);

	/* Limit maximum damage XXX XXX XXX */
	if (dam > MAX_DAMAGE) dam = MAX_DAMAGE;

	if (who > SOURCE_MONSTER_START)
	{
		/* Source monster */
		monster_type *m_ptr;

		/* Monster name (for attacks) */
		char m_name[80];

		/* Get the source monster */
		m_ptr = &mon_list[who];

		/* Get the monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Get the monster's real name */
		monster_desc(killer, sizeof(killer), m_ptr, 0x88);
	}
	else if (who == SOURCE_TRAP)
	{

		/* Get the effect name */
		feature_desc(killer, sizeof(killer), x_list[cave_x_idx[y][x]].x_f_idx, TRUE, TRUE);
	}
	else
	{
		/* Remember dangerous terrain */
		if (who == SOURCE_OTHER)
		{
 			is_terrain = TRUE;
		}

 		my_strcpy(killer, msg ? msg: "the dungeon", sizeof(killer));
	}

	/*
	 * Apply bonuses/penalties from terrain, only if damage doesn't come
	 * from terrain already
	 */
	if (!is_terrain)
	{
		/* Adjust damage */
		dam = terrain_adjust_damage(dam, typ, y, x, p_ptr->p_native, TRUE);
	}

	/* Analyze the damage */
	switch (typ)
	{
		/* Standard damage -- hurts inventory too */
		case GF_ACID:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by acid!");
			   else msg_format("You are %s", msg);
			}

			acid_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_FIRE:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by fire!");
			   else msg_format("You are %s", msg);
			}

			fire_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_COLD:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by cold!");
			   else msg_format("You are %s", msg);
			}

			cold_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_ELEC:
		{
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by lightning!");
			   else msg_format("You are %s", msg);
			}

			elec_dam(dam, killer);
			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			/*player is immune*/
			if (p_ptr->state.immune_pois) break;
			if (blind) msg_print("You are hit by poison!");
			if (p_ptr->state.resist_pois) dam = (dam + 2) / 3;
			if (p_ptr->timed[TMD_OPP_POIS]) dam = (dam + 2) / 3;
			take_hit(dam, killer);
			if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
			{
				(void)inc_timed(TMD_POISONED, rand_int(dam) + 10, TRUE);
			}
			break;
		}

		/* Standard damage */
		case GF_MISSILE:
		{
			if ((blind) && (!is_terrain)) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* No resist to Lava damage, unless native. */
		case GF_LAVA:
		{

			/*Player is native*/
			if (p_ptr->p_native & (FF3_LAVA))
			{

				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are seared by lava!");
				}
				else
				{
					msg_print("You are hit by lava!");
				}

			}

			lava_dam(dam, killer, player_native);
			break;
		}

		/* Boiling water  */
		case GF_BWATER:
		{
			/*Player is native*/
			if ((p_ptr->p_native & (ELEMENT_BWATER)) == ELEMENT_BWATER)
			{
				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

				/*Player is being hit by something.  Take nominal damage*/
				dam /= 9;
			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are scorched by boiling water!");
				}
				else
				{
					msg_print("You are hit with boiling water!");
				}

			}

			boiling_water_dam(dam, killer, player_native);
			break;
		}

		/* Boiling mud  */
		case GF_BMUD:
		{
			/*Player is native*/
			if ((p_ptr->p_native & (ELEMENT_BMUD)) == ELEMENT_BMUD)
			{

				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

				/*Player is being hit by something.  Take nominal damage*/
				dam /= 9;
			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are seared by boiling mud!");
				}
				else
				{
					msg_print("You are hit with boiling mud!");
				}

			}

			boiling_mud_dam(dam, killer, player_native);
			break;
		}

		/* Holy Orb -- Player only takes partial damage */
		case GF_HOLY_ORB:
		{
			if (blind) msg_print("You are hit by a holy force!");
			take_hit(dam, killer);
			holy_orb_destroy(dam);
			break;
		}

		/* Arrow --  */
		case GF_ARROW:
		{
			if (who > SOURCE_MONSTER_START)
			{

				/* Source monster */
				monster_type *m_ptr;
				monster_race *r_ptr;

				/* Get the source monster */
				m_ptr = &mon_list[who];

				/* Get the monster race. */
				r_ptr = &r_info[m_ptr->r_idx];

				/* Test for a miss or armour deflection. */
				if ((p_ptr->state.ac + ((p_ptr->state.to_a < 150) ? p_ptr->state.ac + p_ptr->state.to_a : 150)) >
			   	  randint((10 + r_ptr->level) * 5))
				{
					if ((p_ptr->state.ac > 9) && (one_in_(2)))
					{
						msg_print("It glances off your armour.");
					}
					else msg_print("It misses.");

					/* No damage. */
					dam = 0;
				}
			}

			if (dam)
			{
				/* Test for a deflection. */
				if ((inventory[INVEN_ARM].k_idx) &&
					(inventory[INVEN_ARM].ac  > rand_int(50)))
				{

					msg_print("It ricochets off your shield.");

					/* No damage. */
					dam = 0;
				}

				/* Reduce damage if missile did not get deflected. */
				else dam -= (dam * ((p_ptr->state.ac + (p_ptr->state.to_a < 150) ?
				                 p_ptr->state.ac + p_ptr->state.to_a : 150)) / 250);
			}

			if (dam)
			{
				if (blind) msg_print("You are hit by something sharp!");
				take_hit(dam, killer);
			}
			break;
		}

		/* Plasma -- No resist XXX */
		case GF_PLASMA:
		{
			if ((blind)  && (!is_terrain)) msg_print("You are hit by something!");
			take_hit(dam, killer);
			if (!p_ptr->state.resist_sound)
			{
				int k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
				(void)set_stun(p_ptr->timed[TMD_STUN] + k);
			}
			break;
		}

		/* Nether -- drain experience */
		case GF_NETHER:
		{
			if (blind) msg_print("You are hit by something strange!");
			if (p_ptr->state.resist_nethr)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				s32b d = 200 + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

				if (p_ptr->state.hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(d / 10);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(d);
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Water -- stun/confuse */
		case GF_WATER:
		{
			bool native = ((p_ptr->p_native & (P_NATIVE_WATER)) ? TRUE : FALSE);

			if (blind) msg_print("You are hit by something!");

			if (native)
			{
				dam /= 2;
			}
			if ((!p_ptr->state.resist_sound) || (native))
			{
				(void)set_stun(p_ptr->timed[TMD_STUN] + randint(40));
			}
			if (allow_player_confusion() && (!native))
			{
				(void)inc_timed(TMD_CONFUSED, randint(5) + 5, TRUE);
			}
			take_hit(dam, killer);
			break;
		}

		/* Chaos -- many effects */
		case GF_CHAOS:
		{
			if (blind) msg_print("You are hit by something strange!");
			if (p_ptr->state.resist_chaos)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			if (allow_player_confusion())
			{
				(void)inc_timed(TMD_CONFUSED, rand_int(20) + 10, TRUE);
			}
			if (!p_ptr->state.resist_chaos)
			{
				(void)inc_timed(TMD_IMAGE, randint(10), TRUE);
			}
			if (!p_ptr->state.resist_nethr && !p_ptr->state.resist_chaos)
			{
				if (p_ptr->state.hold_life && (rand_int(100) < 75))
				{
					msg_print("You keep hold of your life force!");
				}
				else
				{
					s32b d = 200 + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

					if (p_ptr->state.hold_life)
					{
						msg_print("You feel your life slipping away!");
						lose_exp(d / 10);
					}
					else
					{
						msg_print("You feel your life draining away!");
						lose_exp(d);
					}
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Shards -- mostly cutting */
		case GF_SHARD:
		{
			if (blind) msg_print("You are hit by something sharp!");
			if (p_ptr->state.resist_shard)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				(void)set_cut(p_ptr->timed[TMD_CUT] + dam);
			}
			take_hit(dam, killer);
			break;
		}

		/* Sound -- mostly stunning */
		case GF_SOUND:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->state.resist_sound)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			else
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->timed[TMD_STUN] + k);
			}
			take_hit(dam, killer);
			break;
		}

		/* Pure confusion */
		case GF_CONFUSION:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->state.resist_confu)
			{
				dam *= 5; dam /= (randint(6) + 6);
			}
			if (allow_player_confusion())
			{
				(void)inc_timed(TMD_CONFUSED, randint(20) + 10, TRUE);
			}
			take_hit(dam, killer);
			break;
		}

		/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
			if (blind) msg_print("You are hit by something strange!");
			if (p_ptr->state.resist_disen)
			{
				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				(void)apply_disenchant(0);
			}
			take_hit(dam, killer);
			break;
		}

		/* Nexus -- see above */
		case GF_NEXUS:
		{
			if (blind) msg_print("You are hit by something strange!");

			if (p_ptr->state.resist_nexus)
			{

				dam *= 6; dam /= (randint(6) + 6);
			}
			else
			{
				apply_nexus(who);
			}

			take_hit(dam, killer);

			break;
		}

		/*Static - drain player mana, rods, wands, staves, activatable items*/
		case GF_STATIC:
		{
			int drain = MAX(dam / 25,1);
			if (blind) msg_print("You are hit by something!");

			if (p_ptr->csp > 0)
			{
				if (blind) msg_print("You feel your head cloud up!");
				if (drain > p_ptr->csp) p_ptr->csp = 0;
				else p_ptr->csp -= drain;

			}

			take_hit(dam, killer);

			/*Drain the inventory if needed*/
			if (!p_ptr->is_dead) inven_drain(dam);

			break;
		}

		/* Force -- mostly stun */
		case GF_FORCE:
		{
			if (blind) msg_print("You are hit by something!");
			if (!p_ptr->state.resist_sound)
			{
				(void)set_stun(p_ptr->timed[TMD_STUN] + randint(20));
			}
			take_hit(dam, killer);
			break;
		}

		/* Inertia -- slowness */
		case GF_INERTIA:
		{
			if (blind) msg_print("You are hit by something strange!");
			(void)inc_timed(TMD_SLOW, rand_int(4) + 4, TRUE);
			take_hit(dam, killer);
			break;
		}

		/* Lite -- blinding */
		case GF_LIGHT:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->state.resist_light)
			{
				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->state.resist_blind)
			{
				(void)inc_timed(TMD_BLIND, randint(5) + 2, TRUE);
			}
			take_hit(dam, killer);
			break;
		}

		/* Dark -- blinding */
		case GF_DARK:
		{
			if (blind) msg_print("You are hit by something!");
			if (p_ptr->state.resist_dark)
			{
				dam *= 4; dam /= (randint(6) + 6);
			}
			else if (!blind && !p_ptr->state.resist_blind)
			{
				(void)inc_timed(TMD_BLIND, randint(5) + 2, TRUE);
			}
			take_hit(dam, killer);
			break;
		}

		/* Time -- bolt fewer effects XXX */
		case GF_TIME:
		{
			if (blind) msg_print("You are hit by something strange!");

			switch (randint(10))
			{
				case 1: case 2: case 3: case 4: case 5:
				{
					msg_print("You feel life has clocked back.");
					lose_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
					break;
				}

				case 6: case 7: case 8: case 9:
				{
					switch (randint(6))
					{
						case 1: k = A_STR; act = "strong"; break;
						case 2: k = A_INT; act = "bright"; break;
						case 3: k = A_WIS; act = "wise"; break;
						case 4: k = A_DEX; act = "agile"; break;
						case 5: k = A_CON; act = "healthy"; break;
						case 6: k = A_CHR; act = "beautiful"; break;
					}

					msg_format("You're not as %s as you used to be...", act);

					p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
					if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
					p_ptr->update |= (PU_BONUS);
					break;
				}

				case 10:
				{
					msg_print("You're not as powerful as you used to be...");

					for (k = 0; k < A_MAX; k++)
					{
						p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
						if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
					}
					p_ptr->update |= (PU_BONUS);
					break;
				}
			}
			take_hit(dam, killer);
			break;
		}

		/* Gravity -- stun plus slowness plus teleport */
		case GF_GRAVITY:
		{
			if (blind) msg_print("You are hit by something strange!");
			msg_print("Gravity warps around you.");

			/* Higher level players can resist the teleportation better */
			if (randint(127) > p_ptr->lev) teleport_player(5, FALSE);

			(void)inc_timed(TMD_SLOW, rand_int(4) + 4, TRUE);
			if (!p_ptr->state.resist_sound)
			{
				int k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->timed[TMD_STUN] + k);
			}

			take_hit(dam, killer);

			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (blind) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* Pure damage */
		case GF_METEOR:
		{
			if (blind) msg_print("You are hit by something!");
			take_hit(dam, killer);
			break;
		}

		/* Ice -- cold plus stun plus cuts */
		case GF_ICE:
		{
			bool native = ((p_ptr->p_native & (P_NATIVE_ICE)) ? TRUE : FALSE);
			if (blind)
			{
			   if (!is_terrain) msg_print("You are hit by something sharp!");
			   else msg_format("You are %s", msg);
			}

			cold_dam(dam, killer);
			if ((!p_ptr->state.resist_shard) || (native))
			{
				(void)set_cut(p_ptr->timed[TMD_CUT] + damroll(5, 8));
			}
			if ((!p_ptr->state.resist_sound) && (!native))
			{
				(void)set_stun(p_ptr->timed[TMD_STUN] + randint(15));
			}
			break;
		}

		/* Spores - poison, cause disease */
		case GF_SPORE:
		{
			int power;

			if (who > SOURCE_MONSTER_START)
			{
				/* Source monster */
				monster_type *m_ptr;
				monster_race *r_ptr;

				/* Get the source monster */
				m_ptr = &mon_list[who];

				/* Get the monster race. */
				r_ptr = &r_info[m_ptr->r_idx];

				power = r_ptr->level;

			}
			else power = effective_depth(p_ptr->depth);

			if (blind) msg_print("You feel spores all around you...");

			take_hit(dam, killer);

			/* Poison */
			if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
			{
				inc_timed(TMD_POISONED, randint(dam * 2), TRUE);
			}

			/* Disease */
			if (randint(power) >= 15)
			{
				int dummy = randint(dam * 2);
				disease(&dummy);
			}

			break;
		}

		case GF_SAND:
		{
			/*Player is native*/
			if (p_ptr->p_native & (FF3_SAND))
			{

				/*Nothing happens*/
				if (is_terrain) return (FALSE);

				player_native = TRUE;

				/*Player is being hit by something.  Reduce the damage a bit.*/
				dam /= 2;
			}

			if (blind) msg_format("You are %s", msg);

			else
			{
				if (!player_native)
				{
					msg_print("You are blasted by sand!");
				}
				else
				{
					msg_print("You are hit by sand!");
				}

			}

			/*Sand wears down the equipment*/
			apply_disenchant(TRUE);

			take_hit(dam, killer);
			break;
		}

		/* Default */
		default:
		{
			take_hit(dam, killer);

			break;
		}
	}


	/* Disturb */
	disturb(1, 0);


	/* Return "Anything seen?" */
	return (obvious);
}

/*
 * Helper function for "project()" below.
 *
 * Create effects based on the spell type.
 *
 *
 * We return "TRUE" if any "obvious" effects were observed.
 *
 */
static bool project_x(int who, int y, int x, int dam, int typ, u32b project_flg)
{
	u16b effect_flag = 0L;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	bool always = FALSE;

	/* Player blind-ness */
	bool blind = (p_ptr->timed[TMD_BLIND] ? TRUE : FALSE);

	int source = who;

	if (project_flg & (PROJECT_CLOUD)) always = TRUE;

	/*No effects in walls*/
	if (!cave_ff1_match(y, x, FF1_MOVE)) return (FALSE);

	/*We can't see this square*/
	if (!player_can_see_bold(y, x)) obvious = FALSE;

	if (game_mode == GAME_NPPMORIA)
	{
		/* Allow traps, but that's all */
		if (typ != GF_MAKE_TRAP) return FALSE;
	}

	/*
	 * Apply bonuses/penalties from terrain, only if damage doesn't come
	 * from terrain already
	 */
	if (who != SOURCE_OTHER)
	{
		/* Adjust damage */
		dam = terrain_adjust_damage(dam, typ, y, x, 0, FALSE);
	}

	/* Remember if the player is the source. */
	if (source == SOURCE_PLAYER) 		effect_flag |= EF1_CHARACTER;
	if (project_flg & (PROJECT_PLAY)) 	effect_flag |= EF1_HURT_PLAY;

	/*If a monster, make the source the race of the monster*/
	if (who > SOURCE_MONSTER_START)
	{
		monster_type *m_ptr = &mon_list[who];
		source = m_ptr->r_idx;
	}

	/* Analyze the damage */
	switch (typ)
	{

		/* Fire and lava leave smoke */
		case GF_FIRE:
		case GF_LAVA:
		{
			/*Lingering Smoke Cloud*/
			if ((dam > 1200) || always) (void)set_effect_lingering_cloud(FEAT_SMOKE, y, x, dam, source, effect_flag);
			break;
		}
		/* Fire leaves smoke */
		case GF_ACID:
		case GF_ELEC:
		{
			int feat = ((typ == GF_ACID) ? FEAT_SMOKE : FEAT_SPARKS);

			/*Lingering Cloud*/
			if ((dam > 1200) || always)  (void)set_effect_lingering_cloud(feat, y, x, (dam / 3), source, effect_flag);
			break;
		}
		case GF_GRAVITY:
		case GF_INERTIA:
		{
			int feat = ((typ == GF_GRAVITY) ? FEAT_GRAVITY : FEAT_INERTIA);

			/*Not noticed*/
			obvious = FALSE;

			/*Leave residual effects if damage is high enough*/
			if ((dam > 200) || always)
			{
				int repeats = ((dam / 100) + randint(dam / 100));

				(void)set_effect_shimmering_cloud(feat, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		/* Standard damage -- also poisons player */
		case GF_COLD:
		{
			/*Lingering Poison Cloud*/
			if ((dam > 250) || always)  (void)set_effect_lingering_cloud(FEAT_FROST_CLOUD, y, x, dam / 3, source, effect_flag);
			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			/*Lingering Poison Cloud*/
			if ((dam > 250) || always)  (void)set_effect_lingering_cloud(FEAT_POISON_CLOUD, y, x, dam / 3, source, effect_flag);
			break;
		}

		/* Make traps */
		case GF_MAKE_TRAP:
		{
			/*Not noticed*/
			obvious = FALSE;

			place_trap(y, x, 0);

			break;
		}

		/* Boiling water or mud -- on player */
		case GF_BWATER:
		case GF_BMUD:
		{
			/* Steam Cloud */
			if ((dam > 250) || always)  (void)set_effect_lingering_cloud(FEAT_STEAM, y, x, dam, source, effect_flag);
			break;
		}

		/* Lite  */
		case GF_LIGHT:
		case GF_DARK:
		{
			int feat = ((typ == GF_LIGHT) ? FEAT_LIGHT : FEAT_DARK);

			/*Not noticed*/
			obvious = FALSE;

			if ((dam > 200) || always)
			{
				int repeats = ((dam / 100) + randint(dam / 100));

				(void)set_effect_shimmering_cloud(feat, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		case GF_LIFE_DRAIN:
		{
			/*Leave residual effects if a player spell*/
			if (always)
			{
				int dam1 = (dam * f_info[FEAT_LIFE_DRAIN].x_damage) / 100;

				int repeats = ((dam1 / 25) + randint(dam1 / 25));

				(void)set_effect_shimmering_cloud(FEAT_LIFE_DRAIN, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		case GF_ELEC_BURST: /*repeated lightning strikes*/
		{
			/*Leave residual effects if a player spell*/
			if (always)
			{
				int dam1 = (dam * f_info[FEAT_SPARKS].x_damage) / 100;

				int repeats = ((dam1 / 25) + randint(dam1 / 25));

				(void)set_effect_shimmering_cloud(FEAT_SPARKS, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		case GF_METEOR: /*repeated meteor strikes*/
		{
			/*Leave residual effects if a player spell*/
			if (always)
			{
				int dam1 = (dam * f_info[FEAT_METEOR_BURST].x_damage) / 100;

				int repeats = ((dam1 / 25) + randint(dam1 / 25));

				(void)set_effect_shimmering_cloud(FEAT_METEOR_BURST, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		/* Spores - poison, */
		case GF_SPORE:
		{
			/*Lingering Poison Cloud*/
			if ((dam > 300) || always)  (void)set_effect_lingering_cloud(FEAT_POISON_CLOUD, y, x, dam, source, effect_flag);
			break;
		}

		/*Clear the air of cloud effects*/
		case GF_CLEAR_AIR:
		{

			/* Get the first effect */
			u16b x_idx = cave_x_idx[y][x];

			/* Scan the effects on that grid */
			while (x_idx)
			{
				u16b this_x_idx = x_idx;

				/* Get the effect data */
				effect_type *x_ptr = &x_list[x_idx];

				/* Point to the next effect */
				x_idx = x_ptr->next_x_idx;

				if ((x_ptr->x_type == EFFECT_LINGERING_CLOUD) ||
					(x_ptr->x_type == EFFECT_SHIMMERING_CLOUD) ||
					(x_ptr->x_type == EFFECT_PERMANENT_CLOUD))
				{
					/*Delete it*/
					delete_effect_idx(this_x_idx);
				}


			}

			break;

		}

		case GF_VAPOUR:
		case GF_STEAM:

		{
			/*Lingering Steam Cloud*/
			(void)set_effect_lingering_cloud(FEAT_STEAM, y, x, dam, source, effect_flag);
			break;
		}

		case GF_SMOKE:
		{
			/*Lingering Smoke Cloud*/
			(void)set_effect_lingering_cloud(FEAT_SMOKE, y, x, dam / 3, source, effect_flag);
			break;
		}
		case GF_STATIC:
		{
			/*Not noticed*/
			obvious = FALSE;

			if ((dam > 200) || always)
			{
				int repeats = ((dam / 100) + randint(dam / 100));

				(void)set_effect_shimmering_cloud(FEAT_STATIC, y, x, repeats, dam, source, effect_flag);
			}

			break;
		}

		case GF_NETHER:
		case GF_CHAOS:
		case GF_DISENCHANT:
		case GF_NEXUS:
		case GF_TIME:
		case GF_CONFUSION:
		case GF_SHARD:
		{
			int feat;

			if (typ == GF_NETHER) 			feat = FEAT_NETHER;
			else if (typ == GF_CHAOS) 		feat = FEAT_CHAOS;
			else if (typ == GF_DISENCHANT) 		feat = FEAT_DISENCHANTMENT;
			else if (typ == GF_NEXUS) 		feat = FEAT_NEXUS;
			else if (typ == GF_TIME) 		feat = FEAT_TIME;
			else if (typ == GF_CONFUSION) 	feat = FEAT_CONFUSION;
			else if (typ == GF_SHARD) 	    feat = FEAT_SHARD;
			/*Paranoia*/
			else break;

			/*Lingering Cloud*/
			if ((dam > 500) || always)  (void)set_effect_lingering_cloud(feat, y, x, (dam / 5), source, effect_flag);
			break;
		}

		/* Default */
		default:
		{
			return (FALSE);
		}
	}

	/* Return "Anything seen?" */
	return ((obvious) && (!blind));
}

/*
 * Calculate and store the arcs used to make starbursts.
 */
static void calc_starburst(int height, int width, byte *arc_first,
	byte *arc_dist, int *arc_num)
{
	int i;
	int size, dist, vert_factor;
	int degree_first, center_of_arc;


	/* Note the "size" */
	size = 2 + div_round(width + height, 22);

	/* Ask for a reasonable number of arcs. */
	*arc_num = 8 + (height * width / 80);
	*arc_num = rand_spread(*arc_num, 3);
	if (*arc_num < 8)  *arc_num = 8;
	if (*arc_num > 45) *arc_num = 45;

	/* Determine the start degrees and expansion distance for each arc. */
	for (degree_first = 0, i = 0; i < *arc_num; i++)
	{
		/* Get the first degree for this arc (using 180-degree circles). */
		arc_first[i] = degree_first;

		/* Get a slightly randomized start degree for the next arc. */
		degree_first += div_round(180, *arc_num);

		/* Do not entirely leave the usual range */
		if (degree_first < 180 * (i+1) / *arc_num)
		    degree_first = 180 * (i+1) / *arc_num;
		if (degree_first > (180 + *arc_num) * (i+1) / *arc_num)
		    degree_first = (180 + *arc_num) * (i+1) / *arc_num;


		/* Get the center of the arc (convert from 180 to 360 circle). */
		center_of_arc = degree_first + arc_first[i];

		/* Get arc distance from the horizontal (0 and 180 degrees) */
		if      (center_of_arc <=  90) vert_factor = center_of_arc;
		else if (center_of_arc >= 270) vert_factor = ABS(center_of_arc - 360);
		else                           vert_factor = ABS(center_of_arc - 180);

		/*
		 * Usual case -- Calculate distance to expand outwards.  Pay more
		 * attention to width near the horizontal, more attention to height
		 * near the vertical.
		 */
		dist = ((height * vert_factor) + (width * (90 - vert_factor))) / 90;

		/* Randomize distance (should never be greater than radius) */
		arc_dist[i] = rand_range(dist / 4, dist / 2);

		/* Keep variability under control (except in special cases). */
		if ((dist != 0) && (i != 0))
		{
			int diff = arc_dist[i] - arc_dist[i-1];

			if (ABS(diff) > size)
			{
				if (diff > 0)
					arc_dist[i] = arc_dist[i-1] + size;
				else
					arc_dist[i] = arc_dist[i-1] - size;
			}
		}
	}

	/* Neaten up final arc of circle by comparing it to the first. */
	if (TRUE)
	{
		int diff = arc_dist[*arc_num - 1] - arc_dist[0];

		if (ABS(diff) > size)
		{
			if (diff > 0)
				arc_dist[*arc_num - 1] = arc_dist[0] + size;
			else
				arc_dist[*arc_num - 1] = arc_dist[0] - size;
		}
	}
}

/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *
 * Input:
 *   who: Index of "source" monster (negative for "player")
 *   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball)
 *   y,x: Target location (or location to travel "towards")
 *   dam: Base damage roll to apply to affected monsters (or player)
 *   typ: Type of damage to apply to monsters (and objects)
 *   flg: Extra bit flags (see PROJECT_xxxx in "defines.h")
 *   degrees: How wide an arc spell is (in degrees).
 *   source_diameter: how wide the source diameter is.
 *
 * Return:
 *   TRUE if any "effects" of the projection were observed, else FALSE
 *
* At present, there are five major types of projections:
 *
 * Point-effect projection:  (no PROJECT_BEAM flag, radius of zero, and either
 *   jumps directly to target or has a single source and target grid)
 * A point-effect projection has no line of projection, and only affects one
 *   grid.  It is used for most area-effect spells (like dispel evil) and
 *   pinpoint strikes.
 *
 * Bolt:  (no PROJECT_BEAM flag, radius of zero, has to travel from source to
 *   target)
 * A bolt travels from source to target and affects only the last grid it
 *   enters.  If given the PROJECT_STOP flag, it is stopped by any
 *   monster or character in its path (at present, all bolts use this flag).
 *
 * Beam:  (PROJECT_BEAM)
 * A beam travels from source to target, affecting all grids passed through
 *   with full damage.  It is never stopped by monsters in its path.  Beams
 *   may never be combined with any other projection type.
 *
 * Ball:  (positive radius, no PROJECT_ARC flag)
 * A ball travels from source towards the target, and always explodes.  Unless
 *   specified, it does not affect wall grids, but otherwise affects any grids
 *   in LOS from the center of the explosion.
 * If used with a direction, a ball will explode on the first occupied grid in
 *   its path.  If given a target, it will explode on that target.  If a
 *   wall is in the way, it will explode against the wall.  If a ball reaches
 *   MAX_RANGE without hitting anything or reaching its target, it will
 *   explode at that point.
 *
 * Arc:  (positive radius, with PROJECT_ARC flag)
 * An arc is a portion of a source-centered ball that explodes outwards
 *   towards the target grid.  Like a ball, it affects all non-wall grids in
 *   LOS of the source in the explosion area.  The width of arc spells is con-
 *   trolled by tthe variable "degrees".
 * An arc is created by rejecting all grids that form the endpoints of lines
 *   whose angular difference (in degrees) from the centerline of the arc is
 *   greater than one-half the input "degrees".  See the table "get_
 *   angle_to_grid" in "util.c" for more information.
 * Note:  An arc with a value for degrees of zero is actually a beam of
 *   defined length.
 *
 * Projections that affect all monsters in LOS are handled through the use
 *   of "project_los()", which applies a single-grid projection to individual
 *   monsters.  Projections that light up rooms or affect all monsters on the
 *   level are more efficiently handled through special functions.
 *
 *
 * Variations:
 *
 * PROJECT_STOP forces a path of projection to stop at the first occupied
 *   grid it hits.  This is used with bolts, and also by ball spells
 *   travelling in a specific direction rather than towards a target.
 *
 * PROJECT_THRU allows a path of projection towards a target to continue
 *   past that target.  This is appropriate for physical missiles (crossbow
 *   bolts, arrows, etc.)
 *
 * PROJECT_JUMP allows a projection to immediately set the source of the pro-
 *   jection to the target.  This is used for all area effect spells (like
 *   dispel evil), and can also be used for bombardments.
 *
 * PROJECT_WALL allows a projection, not just to affect one layer of any
 *   passable wall (rubble, trees), but to affect the surface of any wall.
 *   Certain projection types always have this flag.
 *
 * PROJECT_PASS allows projections to ignore walls completely.
 *   Certain projection types always have this flag.
 *
 * PROJECT_HIDE erases all graphical effects, making the projection
 *   invisible.
 *
 * PROJECT_GRID allows projections to affect terrain features.
 *
 * PROJECT_ITEM allows projections to affect objects on the ground.
 *
 * PROJECT_KILL allows projections to affect monsters.
 *
 * PROJECT_PLAY allows projections to affect the player.
 *
 * degrees controls the width of arc spells.  With a value for
 *   degrees of zero, arcs act like beams of defined length.
 *
 * source_diameter controls how quickly explosions lose strength with dis-
 *   tance from the target.  Most ball spells have a source diameter of 10,
 *   which means that they do 1/2 damage at range 1, 1/3 damage at range 2,
 *   and so on.   Caster-centered balls usually have a source diameter of 20,
 *   which allows them to do full damage to all adjacent grids.   Arcs have
 *   source diameters ranging up to 200, which allows the spell designer to
 *   fine-tune how quickly a breath loses strength outwards from the breather.
 *   It is expected, but not required, that wide arcs lose strength more
 *   quickly over distance.   (see the math in the "fire_arc()" function).
 *
 *
 * The player will only get "experience" for monsters killed by himself
 * Unique monsters can only be destroyed by attacks from the player
 *
 * Implementation notes:
 *
 * If the source grid is not the same as the target, we project along the path
 *   between them.  Bolts stop if they hit anything, beams stop if they hit a
 *   wall, and balls and arcs may exhibit either behavior.  When they reach
 *   the final grid in the path, balls and arcs explode.  We do not allow beams
 *   to be combined with explosions.
 * Balls affect all floor grids in LOS (optionally, also wall grids adjacent
 *   to a grid in LOS) within their radius.  Arcs do the same, but only within
 *   their cone of projection.
 * Because affected grids are only scanned once, and it is really helpful to
 *   have explosions that travel outwards from the source, they are sorted by
 *   distance.  For each distance, an adjusted damage is calculated.
 * In successive passes, the code then displays explosion graphics, erases
 *   these graphics, marks terrain for possible later changes, affects
 *   objects, monsters, the character, and finally changes features and
 *   teleports monsters and characters in marked grids.
 *
 *


 * Usage and graphics notes:
 *
 * If the delay factor is anything other
 * than zero, bolt and explosion pictures will be momentarily shown on screen.
 *
 * Only 256 grids can be affected per projection, limiting the effective
 * radius of standard ball attacks to nine units (diameter nineteen).  Arcs
 * can have larger radii; an arc capable of going out to range 20 should not
 * be wider than 70 degrees.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters on
 * both sides of a wall.
 *
 * Note that for consistency, we pretend that the bolt actually takes time
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 *
 * Hack -- when only a single monster is affected, we automatically track
 * (and recall) that monster, unless "PROJECT_JUMP" is used.
 *
 * Note that we must call "handle_stuff()" after affecting terrain features
 * in the blast radius, in case the illumination of the grid was changed,
 * and "update_view()" and "update_monsters()" need to be called.
 */
bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
			 u32b flg, int degrees, byte source_diameter)
{
	int i, j, k, m;
	int dist = 0;

	u32b dam_temp;
	int centerline = 0;

	int y = y0;
	int x = x0;
	int n1y = 0;
	int n1x = 0;
	int y2, x2;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Assume the player sees nothing */
	bool notice = FALSE;

	/* Assume the player has seen nothing */
	bool visual = FALSE;

	/* Assume the player has seen no blast grids */
	bool drawn = FALSE;

	/* Is the player blind? */
	bool blind = (p_ptr->timed[TMD_BLIND] ? TRUE : FALSE);

	int path_n;
	u16b path_g[PATH_SIZE];
	u16b path_gx[PATH_SIZE];

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	int gx[256], gy[256];

	/* Distance to each of the affected grids. */
	int gd[256];

	/* Precalculated damage values for each distance. */
	int dam_at_dist[MAX_RANGE+1];

	/*
	 * Starburst projections only --
	 * Holds first degree of arc, maximum effect distance in arc.
	 */
	byte arc_first[45];
	byte arc_dist[45];

	/* Number (max 45) of arcs. */
	int arc_num = 0;

	int degree, max_dist;

	/* Hack -- Flush any pending output */
	handle_stuff();

	/* Make certain that the radius is not too large */
	if (rad > MAX_SIGHT) rad = MAX_SIGHT;

	/* Some projection types always PROJECT_WALL. */
	if ((typ == GF_KILL_WALL) || (typ == GF_KILL_DOOR))
	{
		flg |= (PROJECT_WALL);
	}

	/* Cancel effects if necessary */
	if (flg & (PROJECT_NO_EFCT)) flg &= ~(PROJECT_EFCT);

	/* Hack -- Jump to target, but require a valid target */
	if ((flg & (PROJECT_JUMP)) && (y1) && (x1))
	{
		x = y0 = y1;
		x = x0 = x1;

	}

	/* If a single grid is both source and destination, store it. */
	if ((x1 == x0) && (y1 == y0))
	{
		gy[grids] = y0;
		gx[grids] = x0;
		gd[grids++] = 0;
	}

	/* Otherwise, travel along the projection path (unless arc or star). */
	else if (!(flg & (PROJECT_ARC | PROJECT_STAR)))
	{
		/* Determine maximum length of projection path */
		if (flg & (PROJECT_BOOM)) dist = MAX_RANGE;
		else if (rad <= 0)        dist = MAX_RANGE;
		else                      dist = rad;

		/* Calculate the projection path */
		path_n = project_path(path_g, path_gx, dist, y0, x0, &y1, &x1, flg);

		/* Project along the path */
		for (i = 0; i < path_n; ++i)
		{
			int oy = y;
			int ox = x;

			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);

			/* Hack -- Balls explode before reaching walls. */
			if ((flg & (PROJECT_BOOM)) && (!cave_project_bold(ny, nx)))
			{
				break;
			}

			/* Advance */
			y = ny;
			x = nx;

			/* Grid is in projection path */
			if (path_gx[i] < PATH_G_NONE)
			{
				/* If a beam, collect all grids in the path. */
				if (flg & (PROJECT_BEAM))
				{
					gy[grids] = y;
					gx[grids] = x;
					gd[grids++] = 0;
				}

				/* Otherwise, collect only the final grid in the path. */
				else if (i == path_n - 1)
				{
					gy[grids] = y;
					gx[grids] = x;
					gd[grids++] = 0;
				}
			}

			/* Only do visuals if requested */
			if (!blind && !(flg & (PROJECT_HIDE)))
			{
				/* Only do visuals if the player can "see" the projection */
				if (panel_contains(y, x) && player_has_los_bold(y, x) &&
				    (path_gx[i] < PATH_G_NONE))
				{
					u16b p;

					byte a;
					char c;

					/* Obtain the bolt or explosion pict */
					if (flg & (PROJECT_BEAM)) p = bolt_pict(y, x, y, x, typ, flg);
					else                      p = bolt_pict(oy, ox, y, x, typ, flg);

					/* Extract attr/char */
					a = PICT_A(p);
					c = PICT_C(p);

					/* Display the visual effects */
					print_rel(c, a, y, x);
					move_cursor_relative(y, x);

					if (op_ptr->delay_factor)
					{
					    Term_fresh();
						handle_stuff();
					}

					/* Extra delay if monster in way, if PROJECT_STOP */
					if ((flg & (PROJECT_STOP)) && (cave_m_idx[y][x] != 0))
					{
						m = 10 + msec;

					}
					else m = msec;


					/* Delay */
					Term_xtra(TERM_XTRA_DELAY, m);

					/* Erase the visual effects, unless a beam */
					if (!(flg & (PROJECT_BEAM)))
					{
						light_spot(y, x);
						if (op_ptr->delay_factor) (void)Term_fresh();
					}

					/* If a beam, erase later */
					else
					{
						drawn = TRUE;
					}

					/* Hack -- Activate delay */
					visual = TRUE;

				}

				/* Hack -- Always delay for consistency */
				else if (visual)
				{
					/* Delay for consistency */
					Term_xtra(TERM_XTRA_DELAY, msec);
				}
			}
		}
	}

	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Beams have already stored all the grids they will affect. */
	if (flg & (PROJECT_BEAM))
	{
		/* No special actions */
	}

	/* Handle explosions */
	else if (flg & (PROJECT_BOOM))

	{
		/* Some projection types always PROJECT_WALL. */
		if (typ == GF_ACID)
		{
			/* Note that acid only affects monsters if it melts the wall. */
			flg |= (PROJECT_WALL);
		}

		/* Pre-calculate some things for starbursts. */
		if (flg & (PROJECT_STAR))
		{
			calc_starburst(1 + rad * 2, 1 + rad * 2, arc_first, arc_dist,
				&arc_num);

			/* Mark the area nearby -- limit range, ignore rooms */
			spread_cave_temp(y0, x0, rad, FALSE, (flg & (PROJECT_PASS)) != 0);

		}

		/* Pre-calculate some things for arcs. */
		if (flg & (PROJECT_ARC))
		{
			/* Calculate the first part of the projection path  XXX XX */
			path_n = project_path(path_g, path_gx, rad/2, y0, x0, &y1, &x1, flg);

			/* Store the grids  XXX XXX */
			for (i = 0; i < path_n; i++)
			{
				/* Grid is not skipped, and is not a wall */
				if ((path_gx[i] < PATH_G_NONE) &&
				    (path_gx[i] != PATH_G_WALL))
				{
					/* Save the grid */
					gy[grids] = GRID_Y(path_g[i]);
					gx[grids] = GRID_X(path_g[i]);
					gd[grids++] = i;

					/* Mark the grid (it will not be used again) */
					cave_temp_mark(gy[grids-1], gx[grids-1], FALSE);
				}
			}


			/* The radius of arcs cannot be more than 20 */
			if (rad > 20) rad = 20;

			/* Reorient the grid forming the end of the arc's centerline */
			n1y = y1 - y0 + 20;
			n1x = x1 - x0 + 20;

			/* Correct overly large or small values */
			if (n1y > 40) n1y = 40;
			if (n1x > 40) n1x = 40;
			if (n1y <  0) n1y =  0;
			if (n1x <  0) n1x =  0;

			/* Get the angle of the arc's centerline */
			centerline = 90 - get_angle_to_grid[n1y][n1x];
		}


		/*
		 * If the center of the explosion hasn't been
		 * saved already, save it now.
		 */
		if (grids == 0)
		{
			gy[grids] = y2;
			gx[grids] = x2;
			gd[grids++] = 0;
		}

		/*
		 * Scan every grid that might possibly
		 * be in the blast radius.
		 */
		for (y = y2 - rad; y <= y2 + rad; y++)
		{
			for (x = x2 - rad; x <= x2 + rad; x++)
			{
				/* Center grid has already been stored. */
				if ((y == y2) && (x == x2)) continue;

				/* Precaution: Stay within area limit. */
				if (grids >= 255) break;

				/* Ignore "illegal" locations */
				if (!in_bounds(y, x)) continue;

				/* This is a non-projectable grid */
				if (!cave_project_bold(y, x))

				{
					/* Spell with PROJECT_PASS ignore these grids */
					if (!(flg & (PROJECT_PASS)))
					{
						/* PROJECT_WALL is active or terrain is passable */
						if ((flg & (PROJECT_WALL)) || cave_passable_bold(y, x))
						{
							/* Allow grids next to grids in LOS of explosion center */
							for (i = 0, k = 0; i < 8; i++)
							{
								int yy = y + ddy_ddd[i];
								int xx = x + ddx_ddd[i];

								/* Stay within dungeon */
								if (!in_bounds(yy, xx)) continue;

								if (generic_los(y2, x2, yy, xx, CAVE_PROJECT))
								{
									k++;
									break;
								}
							}

							/* Require at least one adjacent grid in LOS */
							if (!k) continue;
						}

						/* We can't affect this non-passable wall */
						else continue;
					}
				}

				/* Must be within maximum distance. */
				dist = (distance(y2, x2, y, x));
				if (dist > rad) continue;


				/* Projection is a starburst */
				if (flg & (PROJECT_STAR))
				{
					/* Grid is within effect range */
					if (cave_info[y][x] & (CAVE_TEMP))
					{
						/* Reorient current grid for table access. */
						int ny = y - y2 + 20;
						int nx = x - x2 + 20;

						/* Illegal table access is bad. */
						if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))
							continue;

						/* Get angle to current grid. */
						degree = get_angle_to_grid[ny][nx];

						/* Scan arcs to find the one that applies here. */
						for (i = arc_num - 1; i >= 0; i--)
						{
							if (arc_first[i] <= degree)
							{
								max_dist = arc_dist[i];

								/* Must be within effect range. */
								if (max_dist >= dist)
								{
									gy[grids] = y;
									gx[grids] = x;
									gd[grids] = 0;
									grids++;
								}

								/* Arc found.  End search */
								break;
							}
						}
					}
				}

				/* Use angle comparison to delineate an arc. */
				else if (flg & (PROJECT_ARC))
				{
					int n2y, n2x, tmp, diff;

					/* Skip already marked grids */
					if (cave_info[y][x] & (CAVE_TEMP)) continue;

					/* Reorient current grid for table access. */
					n2y = y - y2 + 20;
					n2x = x - x2 + 20;

					/*
					 * Find the angular difference (/2) between
					 * the lines to the end of the arc's center-
					 * line and to the current grid.
					 */
					tmp = ABS(get_angle_to_grid[n2y][n2x] + centerline) % 180;
					diff = ABS(90 - tmp);

					/*
					 * If difference is not greater then that
					 * allowed, and the grid is in LOS, accept it.
					 */
					if (diff < (degrees + 6) / 4)
					{
						if (generic_los(y2, x2, y, x, CAVE_PROJECT))
						{
							gy[grids] = y;
							gx[grids] = x;
							gd[grids] = dist;
							grids++;
						}
					}
				}

				/* Standard ball spell -- accept all grids in LOS. */
				else
				{
					if ((flg & (PROJECT_PASS)) || generic_los(y2, x2, y, x, CAVE_PROJECT))
					{
						gy[grids] = y;
						gx[grids] = x;
						gd[grids] = dist;
						grids++;
					}
				}
			}
		}
	}

	/* Clear the "temp" array  XXX */
	clear_temp_array();

	/* Calculate and store the actual damage at each distance. */
	for (i = 0; i <= MAX_RANGE; i++)
	{
		/* No damage outside the radius. */
		if (i > rad) dam_temp = 0;

		/* Standard damage calc. for 10' source diameters, or at origin. */
		else if ((!source_diameter) || (i == 0))
		{
			dam_temp = (dam + i) / (i + 1);
		}

		/*
		 * If a particular diameter for the source of the explosion's
		 * energy is given, calculate an adjusted damage.
		 */
		else
		{
			dam_temp = (source_diameter * dam) / ((i + 1) * 10);
			if (dam_temp > (u32b)dam) dam_temp = dam;
		}

		/* Store it. */
		dam_at_dist[i] = dam_temp;
	}

	/* Sort the blast grids by distance, starting at the origin. */
	for (i = 0, k = 0; i < rad; i++)
	{
		int tmp_y, tmp_x, tmp_d;

		/* Collect all the grids of a given distance together. */
		for (j = k; j < grids; j++)
		{
			if (gd[j] == i)
			{
				tmp_y = gy[k];
				tmp_x = gx[k];
				tmp_d = gd[k];

				gy[k] = gy[j];
				gx[k] = gx[j];
				gd[k] = gd[j];

				gy[j] = tmp_y;
				gx[j] = tmp_x;
				gd[j] = tmp_d;

				/* Write to next slot */
				k++;
			}
		}
	}


	/* Display the blast area if allowed. (unless a bolt) */
	if (!blind && !(flg & (PROJECT_HIDE)) && ((grids > 1) || (dist == 0)))
	{
		/* Do the blast from inside out */
		for (i = 0; i < grids; i++)
		{
			/* Extract the location */
			y = gy[i];
			x = gx[i];

			/* Only do visuals if the player can "see" the blast */
			if (player_has_los_bold(y, x))
			{
				u16b p;

				byte a;
				char c;

				drawn = TRUE;

				/* Obtain the explosion pict */
				p = bolt_pict(y, x, y, x, typ, flg);

				/* Extract attr/char */
				a = PICT_A(p);
				c = PICT_C(p);

				/* Visual effects -- Display */
				print_rel(c, a, y, x);
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* New radius is about to be drawn */
			if ((i == grids - 1) || ((i < grids - 1) && (gd[i + 1] > gd[i])))
			{
				/* Flush each radius separately */
				if (op_ptr->delay_factor)
				{
				    (void)Term_fresh();
				}

				/* Flush */
				handle_stuff();

				/* Delay (efficiently) */
				if (visual || drawn)
				{
					m = (rad <= 4 ? msec : (rad <= 8 ? 2*msec/3 : msec/2));
					Term_xtra(TERM_XTRA_DELAY, m);
				}
			}
		}

		/* Delay for a while if there are pretty graphics to show */
		/* Delay for a while if there are pretty graphics to show */
		if ((grids > 1) && (visual || drawn))
		{
			if (!op_ptr->delay_factor) (void)Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, who <= 0 ? msec*2 : msec);
		}
	}

	/* Flush the erasing -- except if we specify lingering graphics */
	if ((drawn) && (!(flg & (PROJECT_NO_REDRAW))))
	{
		/* Erase the explosion drawn above */
		for (i = 0; i < grids; i++)
		{
			/* Extract the location */
			y = gy[i];
			x = gx[i];

			/* Hack -- Erase if needed */
			if (player_has_los_bold(y, x))
			{
				light_spot(y, x);
			}
		}

		/* Hack -- center the cursor */
		move_cursor_relative(y2, x2);

		/* Flush the explosion */
		if (op_ptr->delay_factor)
		{
			(void)Term_fresh();
		}
	}

	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();

	/* Check objects */
	if (flg & (PROJECT_ITEM))
	{

		/* Scan for objects */
		for (i = 0; i < grids; i++)
		{

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the object in the grid */
			if (project_o(who, y, x, dam_at_dist[gd[i]], typ)) notice = TRUE;
		}
	}


	/* Check monsters */
	if (flg & (PROJECT_KILL))
	{
		/* Mega-Hack */
		project_m_n = 0;
		project_m_x = 0;
		project_m_y = 0;

		/* Scan for monsters */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* No monster here */
			if (cave_m_idx[y][x] <= SOURCE_MONSTER_START) continue;

			/* Affect the monster in the grid */
			if (project_m(who, y, x, dam_at_dist[gd[i]], typ, flg))
				notice = TRUE;
		}

		/* Player affected one monster (without "jumping") */
		if ((who == SOURCE_PLAYER) && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
		{
			/* Location */
			x = project_m_x;
			y = project_m_y;

			/* Track if possible */
			if (cave_m_idx[y][x] > SOURCE_MONSTER_START)
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

				/* Hack -- auto-recall */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(cave_m_idx[y][x]);
			}
		}
	}

	/* Check player */
	if (flg & (PROJECT_PLAY))
	{

		/* Scan for player */
		for (i = 0; i < grids; i++)
		{

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Player is in this grid */
			if (cave_m_idx[y][x] < 0)
			{

				/* Affect the player */
				if (project_p(who, y, x, dam_at_dist[gd[i]], typ, NULL))
				{
					notice = TRUE;

					/* Only affect the player once */
					break;
				}
			}
		}
	}

	/* Check features */
	if (flg & (PROJECT_GRID))
	{

		/* Scan for features */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the feature in that grid */
			if (project_f(who, y, x, gd[i], dam_at_dist[gd[i]], typ, flg))
				notice = TRUE;
		}
	}

	/* Check effects */
	if (flg & (PROJECT_EFCT))
	{

		/* Scan for features */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the feature in that grid */
			if (project_x(who, y, x, dam_at_dist[gd[i]], typ, flg))
				notice = TRUE;
		}
	}

	/* Clear the "temp" array  (paranoia is good) */
	clear_temp_array();

	/* Update stuff if needed */
	notice_stuff();
	handle_stuff();


	/* Return "something was noticed" */
	return (notice);
}


