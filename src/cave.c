/* File: cave.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
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

/*
 * Support for Adam Bolt's tileset, lighting and transparency effects
 * by Robert Ruehlmann (rr9@thangorodrim.net)
 */

/*
 * Hack -- function hook to point to the right "get_energy_*flow*" function
 */
static int (*get_energy_to_move)(int y, int x, byte which_flow, u32b elem_flag);


/*
 * Approximate distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
 */
int distance(int y1, int x1, int y2, int x2)
{
	int ay, ax;

	/* Find the absolute y/x distance components */
	ay = (y1 > y2) ? (y1 - y2) : (y2 - y1);
	ax = (x1 > x2) ? (x1 - x2) : (x2 - x1);

	/* Hack -- approximate the distance */
	return ((ay > ax) ? (ay + (ax>>1)) : (ax + (ay>>1)));
}


/*
 * A simple, fast, integer-based line-of-sight algorithm.  By Joseph Hall,
 * 4116 Brewster Drive, Raleigh NC 27606.  Email to jnh@ecemwl.ncsu.edu.
 *
 * This function returns TRUE if a "line of sight" can be traced from the
 * center of the grid (x1,y1) to the center of the grid (x2,y2), with all
 * of the grids along this path (except for the endpoints) being non-wall
 * grids.  Actually, the "chess knight move" situation is handled by some
 * special case code which allows the grid diagonally next to the player
 * to be obstructed, because this yields better gameplay semantics.  This
 * algorithm is totally reflexive, except for "knight move" situations.
 *
 * Because this function uses (short) ints for all calculations, overflow
 * may occur if dx and dy exceed 90.
 *
 * Once all the degenerate cases are eliminated, we determine the "slope"
 * ("m"), and we use special "fixed point" mathematics in which we use a
 * special "fractional component" for one of the two location components
 * ("qy" or "qx"), which, along with the slope itself, are "scaled" by a
 * scale factor equal to "abs(dy*dx*2)" to keep the math simple.  Then we
 * simply travel from start to finish along the longer axis, starting at
 * the border between the first and second tiles (where the y offset is
 * thus half the slope), using slope and the fractional component to see
 * when motion along the shorter axis is necessary.  Since we assume that
 * vision is not blocked by "brushing" the corner of any grid, we must do
 * some special checks to avoid testing grids which are "brushed" but not
 * actually "entered".
 *
 * Angband three different "line of sight" type concepts, including this
 * function (which is used almost nowhere), the "project()" method (which
 * is used for determining the paths of projectables and spells and such),
 * and the "update_view()" concept (which is used to determine which grids
 * are "viewable" by the player, which is used for many things, such as
 * determining which grids are illuminated by the player's torch, and which
 * grids and monsters can be "seen" by the player, etc).
 */
bool generic_los(int y1, int x1, int y2, int x2, u16b flg)
{
	/* Delta */
	int dx, dy;

	/* Absolute */
	int ax, ay;

	/* Signs */
	int sx, sy;

	/* Fractions */
	int qx, qy;

	/* Scanners */
	int tx, ty;

	/* Scale factors */
	int f1, f2;

	/* Slope, or 1/Slope, of LOS */
	int m;

	/* Paranoia */
	if (!flg) flg = CAVE_LOS;

	/* Extract the offset */
	dy = y2 - y1;
	dx = x2 - x1;

	/* Extract the absolute offset */
	ay = ABS(dy);
	ax = ABS(dx);

	/* Handle adjacent (or identical) grids */
	if ((ax < 2) && (ay < 2)) return (TRUE);

	/* Directly South/North */
	if (!dx)
	{
		/* South -- check for the flag */
		if (dy > 0)
		{
			for (ty = y1 + 1; ty < y2; ty++)
			{
				if (!cave_flag_bold(ty, x1, flg)) return (FALSE);
			}
		}

		/* North -- check for the flag  */
		else
		{
			for (ty = y1 - 1; ty > y2; ty--)
			{
				if (!cave_flag_bold(ty, x1, flg)) return (FALSE);
			}
		}

		/* Assume los */
		return (TRUE);
	}

	/* Directly East/West */
	if (!dy)
	{
		/* East -- check for the flag  */
		if (dx > 0)
		{
			for (tx = x1 + 1; tx < x2; tx++)
			{
				if (!cave_flag_bold(y1, tx, flg)) return (FALSE);
			}
		}

		/* West -- check for the flag  */
		else
		{
			for (tx = x1 - 1; tx > x2; tx--)
			{
				if (!cave_flag_bold(y1, tx, flg)) return (FALSE);
			}
		}

		/* Assume los */
		return (TRUE);
	}

	/* Extract some signs */
	sx = (dx < 0) ? -1 : 1;
	sy = (dy < 0) ? -1 : 1;

	/* Vertical "knights" */
	if (ax == 1)
	{
		if (ay == 2)
		{
			if (cave_flag_bold(y1 + sy, x1, flg)) return (TRUE);
		}
	}

	/* Horizontal "knights" */
	else if (ay == 1)
	{
		if (ax == 2)
		{
			if (cave_flag_bold(y1, x1 + sx, flg)) return (TRUE);
		}
	}

	/* Calculate scale factor div 2 */
	f2 = (ax * ay);

	/* Calculate scale factor */
	f1 = f2 << 1;

	/* Travel horizontally */
	if (ax >= ay)
	{
		/* Let m = dy / dx * 2 * (dy * dx) = 2 * dy * dy */
		qy = ay * ay;
		m = qy << 1;

		tx = x1 + sx;

		/* Consider the special case where slope == 1. */
		if (qy == f2)
		{
			ty = y1 + sy;
			qy -= f1;
		}
		else
		{
			ty = y1;
		}

		/* Note (below) the case (qy == f2), where */
		/* the LOS exactly meets the corner of a tile. */
		while (x2 - tx)
		{
			if (!cave_flag_bold(ty, tx, flg)) return (FALSE);

			qy += m;

			if (qy < f2)
			{
				tx += sx;
			}
			else if (qy > f2)
			{
				ty += sy;
				if (!cave_flag_bold(ty, tx, flg)) return (FALSE);
				qy -= f1;
				tx += sx;
			}
			else
			{
				ty += sy;
				qy -= f1;
				tx += sx;
			}
		}
	}

	/* Travel vertically */
	else
	{
		/* Let m = dx / dy * 2 * (dx * dy) = 2 * dx * dx */
		qx = ax * ax;
		m = qx << 1;

		ty = y1 + sy;

		if (qx == f2)
		{
			tx = x1 + sx;
			qx -= f1;
		}
		else
		{
			tx = x1;
		}

		/* Note (below) the case (qx == f2), where */
		/* the LOS exactly meets the corner of a tile. */
		while (y2 - ty)
		{
			if (!cave_flag_bold(ty, tx, flg)) return (FALSE);

			qx += m;

			if (qx < f2)
			{
				ty += sy;
			}
			else if (qx > f2)
			{
				tx += sx;
				if (!cave_flag_bold(ty, tx, flg)) return (FALSE);
				qx -= f1;
				ty += sy;
			}
			else
			{
				tx += sx;
				qx -= f1;
				ty += sy;
			}
		}
	}

	/* Assume los */
	return (TRUE);
}


/*
 * Returns true if the player's grid is dark
 */
bool no_light(void)
{
	return (!player_can_see_bold(p_ptr->py, p_ptr->px));
}


/*
 * Determine if a given location may be "destroyed"
 *
 * Used by destruction spells, and for placing stairs, etc.
 */
bool cave_valid_bold(int y, int x)
{
	object_type *o_ptr;

	/* Forbid perma-grids */
	if (cave_perma_bold(y, x)) return (FALSE);

	/* Check objects */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Forbid artifact grids */
		if (artifact_p(o_ptr)) return (FALSE);
	}

	/* Accept */
	return (TRUE);
}


/*
 * Table of breath colors.  Must match listings in a single set of
 * monster spell flags.
 *
 * The value "255" is special.  Monsters with that kind of breath
 * may be any color.
 */
static byte breath_to_attr[32][2] =
{
	{  0,  0 },
	{  0,  0 },
	{  0,  0 },
	{  0,  0 },
	{  0,  0 },
	{  0,  0 },
	{  0,  0 },
	{  0,  0 },
	{  TERM_SLATE, TERM_L_DARK },       /* RF4_BRTH_ACID */
	{  TERM_BLUE,  TERM_L_BLUE },       /* RF4_BRTH_ELEC */
	{  TERM_RED,  TERM_L_RED },         /* RF4_BRTH_FIRE */
	{  TERM_WHITE,  TERM_L_WHITE },     /* RF4_BRTH_COLD */
	{  TERM_GREEN,  TERM_L_GREEN },     /* RF4_BRTH_POIS */
	{  TERM_ORANGE,  TERM_RED },        /* RF4_BRTH_PLAS */
	{  TERM_YELLOW,  TERM_ORANGE },     /* RF4_BRTH_LIGHT */
	{  TERM_L_DARK,  TERM_SLATE },      /* RF4_BRTH_DARK */
	{  TERM_L_UMBER,  TERM_UMBER },     /* RF4_BRTH_CONFU */
	{  TERM_YELLOW,  TERM_L_UMBER },    /* RF4_BRTH_SOUND */
	{  TERM_UMBER,  TERM_L_UMBER },     /* RF4_BRTH_SHARD */
	{  TERM_L_WHITE,  TERM_SLATE },     /* RF4_BRTH_INER */
	{  TERM_L_WHITE,  TERM_SLATE },     /* RF4_BRTH_GRAV */
	{  TERM_WHITE,  TERM_L_BLUE },      /* RF4_BRTH_WIND */
	{  TERM_UMBER,  TERM_L_UMBER },     /* RF4_BRTH_FORCE */
	{  TERM_L_RED,  TERM_VIOLET },      /* RF4_BRTH_NEXUS */
	{  TERM_L_GREEN,  TERM_GREEN },     /* RF4_BRTH_NETHR */
	{  255,  255 },   /* (any color) */ /* RF4_BRTH_CHAOS */
	{  TERM_VIOLET,  TERM_VIOLET },     /* RF4_BRTH_DISEN */
	{  TERM_VIOLET,  TERM_BLUE },       /* RF4_BRTH_MANA */
	{  0,  0 },     /*  */
	{  0,  0 },     /*  */
	{  0,  0 }      /*  */
};

#define MAX_FLICKER_COLORS 38

static byte flicker_colors[MAX_FLICKER_COLORS] =
{
	TERM_DARK,
	TERM_WHITE,
	TERM_SLATE,
	TERM_ORANGE,
	TERM_RED,
	TERM_GREEN,
	TERM_BLUE,
	TERM_UMBER,
	TERM_L_DARK,
	TERM_L_WHITE,
	TERM_VIOLET,
	TERM_YELLOW,
	TERM_L_RED,
	TERM_L_GREEN,
	TERM_L_BLUE,
	TERM_L_UMBER,
	TERM_SNOW_WHITE,
	TERM_SLATE_GRAY,
	TERM_ORANGE_PEEL,
	TERM_RED_LAVA,
	TERM_JUNGLE_GREEN,
	TERM_NAVY_BLUE,
	TERM_AUBURN,
	TERM_TAUPE,
	TERM_L_WHITE_2,
	TERM_D_PURPLE,
	TERM_MAIZE,
	TERM_RASPBERRY,
	TERM_LIME_GREEN,
	TERM_SKY_BLUE,
	TERM_L_BROWN,
	TERM_SILVER,
	TERM_MAHAGONY,
	TERM_RED_RUST,
	TERM_COPPER,
	TERM_GOLD,
	TERM_PINK,
	TERM_EARTH_YELLOW,
};


/*
 * Multi-hued monsters shimmer according to their default attr or to their
 * breaths.  -LM-
 *
 * If a monster has an attr other than 'v', it uses both colors associated
 * with that attr.
 * If a monster has only one kind of breath, it uses both colors
 * associated with that breath.  Otherwise, it just uses the first
 * color for any of its breaths.
 *
 * If a monster does not breath anything, it can be any color.
 */
byte multi_hued_attr(monster_race *r_ptr)
{
	byte allowed_attrs[15];

	int i, j;

	int stored_colors = 0;
	int breaths = 0;
	int first_color = 0;
	int second_color = 0;


	/* Monsters with an attr other than 'v' choose colors according to attr */
	if ((r_ptr->x_attr != TERM_VIOLET) && (r_ptr->x_attr != TERM_D_PURPLE))
	{
		switch (r_ptr->x_attr)
		{
			case TERM_RED:
			case TERM_L_RED:
			case TERM_RED_LAVA:
			case TERM_RED_RUST:
			case TERM_RASPBERRY:
			{
				int x = randint0(5);
				if (x == 1) return TERM_RED;
				if (x == 2) return TERM_L_RED;
				if (x == 3) return TERM_RED_LAVA;
				if (x == 4)	return TERM_RASPBERRY;
				return TERM_RED_RUST;
			}
			case TERM_BLUE:
			case TERM_L_BLUE:
			case TERM_NAVY_BLUE:
			case TERM_SKY_BLUE:
			{
				int x = randint0(5);
				if (x == 1) return TERM_BLUE;
				if (x == 2) return TERM_L_BLUE;
				if (x == 3) return TERM_NAVY_BLUE;
				return TERM_SKY_BLUE;
			}
			case TERM_WHITE:
			case TERM_L_WHITE:
			case TERM_SNOW_WHITE:
			case TERM_L_WHITE_2:
			{
				int x = randint0(4);
				if (x == 1) return TERM_WHITE;
				if (x == 2) return TERM_L_WHITE;
				if (x == 3) return TERM_SNOW_WHITE;
				return TERM_L_WHITE_2;
			}
			case TERM_GREEN:
			case TERM_L_GREEN:
			case TERM_JUNGLE_GREEN:
			case TERM_LIME_GREEN:
			{
				int x = randint0(4);
				if (x == 1) return TERM_GREEN;
				if (x == 2) return TERM_L_GREEN;
				if (x == 3) return TERM_JUNGLE_GREEN;
				return TERM_LIME_GREEN;
			}
			case TERM_UMBER:
			case TERM_L_UMBER:
			case TERM_AUBURN:
			case TERM_L_BROWN:
			{
				int x = randint0(4);
				if (x == 1) return TERM_UMBER;
				if (x == 2) return TERM_L_UMBER;
				if (x == 3) return TERM_AUBURN;
				return TERM_L_BROWN;
			}
			case TERM_ORANGE:
			case TERM_ORANGE_PEEL:
			case TERM_MAHAGONY:
			{
				int x = randint0(3);
				if (x == 1) return TERM_ORANGE;
				if (x == 2) return TERM_ORANGE_PEEL;
				return TERM_MAHAGONY;
			}
			case TERM_YELLOW:
			case TERM_MAIZE:
			case TERM_EARTH_YELLOW:
			{
				int x = randint0(3);
				if (x == 1) return TERM_YELLOW;
				if (x == 2) return TERM_MAIZE;
				return TERM_EARTH_YELLOW;
			}
			case TERM_SLATE:
			case TERM_L_DARK:
			case TERM_SLATE_GRAY:
			case TERM_TAUPE:
			{
				int x = randint0(4);
				if (x == 1) return TERM_SLATE;
				if (x == 2) return TERM_L_DARK;
				if (x == 3) return TERM_SLATE_GRAY;
				return TERM_TAUPE;
			}
		}
	}

	/* Monsters with no ranged attacks can be any color */
	if (!r_ptr->freq_ranged) return (flicker_colors[randint0(MAX_FLICKER_COLORS)]);

	/* Check breaths */
	for (i = 0; i < 32; i++)
	{
		bool stored = FALSE;

		/* Don't have that breath */
		if (!(r_ptr->flags4 & (1L << i))) continue;

		/* Get the first color of this breath */
		first_color = breath_to_attr[i][0];

		/* Breath has no color associated with it */
		if (first_color == 0) continue;

		/* Monster can be of any color */
		if (first_color == 255) return (flicker_colors[randint0(MAX_FLICKER_COLORS)]);

		/* Increment the number of breaths */
		breaths++;

		/* Monsters with lots of breaths may be any color. */
		if (breaths == 6) return (flicker_colors[randint0(MAX_FLICKER_COLORS)]);

		/* Always store the first color */
		for (j = 0; j < stored_colors; j++)
		{
			/* Already stored */
			if (allowed_attrs[j] == first_color) stored = TRUE;
		}
		if (!stored)
		{
			allowed_attrs[stored_colors] = first_color;
			stored_colors++;
		}

		/*
		 * Remember (but do not immediately store) the second color
		 * of the first breath.
		 */
		if (breaths == 1)
		{
			second_color = breath_to_attr[i][1];
		}
	}

	/* Monsters with no breaths may be of any color. */
	if (breaths == 0) return (flicker_colors[randint0(MAX_FLICKER_COLORS)]);

	/* If monster has one breath, store the second color too. */
	if (breaths == 1)
	{
		allowed_attrs[stored_colors] = second_color;
		stored_colors++;
	}

	/* Pick a color at random */
	return (allowed_attrs[rand_int(stored_colors)]);
}


/*
 * Hack -- Hallucinatory monster
 */
static u16b image_monster(bool use_default)
{
	monster_race *r_ptr;

	byte a;
	char c;

	while (1)
	{
		/* Select a random monster */
		r_ptr = &r_info[rand_int(z_info->r_max)];

		/* Skip non-entries */
		if (!r_ptr->r_speed) continue;

		/* Retrieve attr/char */
		if (use_default)
		{
			a = r_ptr->d_attr;
			c = r_ptr->d_char;
		}
		else
		{
			a = r_ptr->x_attr;
			c = r_ptr->x_char;
		}

		/* Encode */
		return (PICT(a,c));
	}

	/*just to avoid compiler warnings*/
	return (TRUE);
}


/*
 * Hack -- Hallucinatory object
 */
static u16b image_object(bool use_default)
{
	object_kind *k_ptr;

	byte a;
	char c;

	while (1)
	{
		/* Select a random object */
		k_ptr = &k_info[rand_int(z_info->k_max - 1) + 1];

		/* Skip non-entries */
		if (!k_ptr->name) continue;

		/* Retrieve attr/char (HACK - without flavors) */
		if (use_default)
		{
			a = k_ptr->d_attr;
			c = k_ptr->d_char;
		}
		else
		{
			a = k_ptr->x_attr;
			c = k_ptr->x_char;
		}

		/* HACK - Skip empty entries */
		if ((a == 0) || (c == 0)) continue;

		/* Encode */
		return (PICT(a,c));
	}

	/*just to avoid compiler warnings*/
	return (TRUE);
}


/*
 * Hack -- Random hallucination
 */
static u16b image_random(bool use_default)
{
	/* Normally, assume monsters */
	if (rand_int(100) < 75)
	{
		return (image_monster(use_default));

	}

	/* Otherwise, assume objects */
	else
	{
		return (image_object(use_default));
	}
}


/*
 * Specify which of the 32x32 tiles support lighting
 */
static bool feat_supports_lighting_dvg(u16b feat)
{
	/* ALl the walls and floor support lighting*/
	if (f_info[feat].f_flags1 & (FF1_WALL | FF1_FLOOR))
	{
		return TRUE;
	}
	/* All water, lava, ice, acid, oil, forest, sand, mud and terrains support lighting*/
	if (f_info[feat].f_flags3 & (FF3_WATER | FF3_LAVA | FF3_ICE | FF3_ACID |
								 FF3_OIL | FF3_FOREST | FF3_SAND | FF3_MUD |
								 FF3_FIRE))
	{
		return TRUE;
	}

	/* A couple others */
	switch (feat)
	{
		case FEAT_LESS:
		case FEAT_MORE:
		case FEAT_CLOSED_DOOR_WOODEN:
		case FEAT_OPEN_DOOR_WOODEN:
		case FEAT_OPEN_DOOR_STEEL:
		case FEAT_OPEN_DOOR_IRON:
		case FEAT_RUBBLE:
		case FEAT_L_ROCK:
		case FEAT_FLOOR_EARTH:
		case FEAT_BURNT_S:
		case FEAT_GLACIER:
		case FEAT_RUBBLE_OBJ:
		case FEAT_ROCK:
		case FEAT_WALL_INSCRIPTION:
		case FEAT_EARTH:
		case FEAT_PEBBLES:
		return (TRUE);
	}

	return (FALSE);
}


/*
 * Determine whether a tile of the terrain supports lighting
 */
bool feat_supports_lighting(u16b feat)
{
	/* Pseudo graphics and 8x8 doesn't support lighting */
	if (use_graphics == GRAPHICS_PSEUDO) return FALSE;
	if (use_graphics == GRAPHICS_ORIGINAL) return FALSE;

	else if (use_graphics == GRAPHICS_DAVID_GERVAIS) return feat_supports_lighting_dvg(feat);

	/* GRAPHICS_ADAM_BOLT */
	if (f_info[feat].f_flags1 & (FF1_TRAP))
	{
		return TRUE;
	}
	if (f_info[feat].f_flags1 & (FF1_STAIRS))
	{
		return TRUE;
	}

	switch (feat)
	{
		case FEAT_FLOOR:
		case FEAT_MAGMA:
		case FEAT_QUARTZ:
		case FEAT_MAGMA_H:
		case FEAT_QUARTZ_H:
		case FEAT_WALL_EXTRA:
		case FEAT_WALL_INNER:
		case FEAT_WALL_OUTER:
		case FEAT_WALL_SOLID:
		case FEAT_PERM_EXTRA:
		case FEAT_PERM_INNER:
		case FEAT_PERM_OUTER:
		case FEAT_PERM_SOLID:
		case FEAT_GLYPH:
		case FEAT_RUBBLE:
		case FEAT_RUBBLE_OBJ:
		case FEAT_GRANITE_C:
		case FEAT_ICE_WALL_C:
		case FEAT_EARTH_WALL:
		case FEAT_LIMESTONE:
		case FEAT_SANDSTONE:
		case FEAT_SCORCHED_WALL:
		case FEAT_ELVISH_WALL:
		case FEAT_VINES:
		case FEAT_WAVE:
		case FEAT_WAVE_S:
		case FEAT_FLOOR_WET:
		case FEAT_WATER:
		case FEAT_WATER_H:
		case FEAT_WATER_H_D:
		case FEAT_BMUD:
		case FEAT_MUD:
		case FEAT_MUD_H:
		case FEAT_ICE:
		case FEAT_THICKET:
		case FEAT_TREE:
		case FEAT_BURNING_TREE:
		case FEAT_BUSH:
		case FEAT_BRANCH:
		case FEAT_GLACIER:
		case FEAT_COBBLESTONE_FLOOR:
			return TRUE;
		default:
			return FALSE;
	}
}


static void special_lighting_floor(byte *a, char *c, int info)
{
	/* Handle "seen" grids */
	if (info & (CAVE_SEEN))
	{
		/* Only lit by "torch" lite */
		if (view_yellow_light && !(info & (CAVE_GLOW | CAVE_HALO)))
		{
			/* Use a brightly lit tile */
			switch (use_graphics)
			{
				case GRAPHICS_NONE:
				case GRAPHICS_PSEUDO:
					/* Use "yellow" */
					if (*a == TERM_WHITE) *a = TERM_YELLOW;
					break;
				case GRAPHICS_ADAM_BOLT:
					*c += 2;
					break;
				case GRAPHICS_DAVID_GERVAIS:
					*c -= 1;
					break;
			}
		}
	}

	/* Handle "dark" grids and "blindness" */
	else if (p_ptr->timed[TMD_BLIND] || !(info & (CAVE_GLOW | CAVE_HALO)))
	{
		/* Use a dark tile */
		switch (use_graphics)
		{
			case GRAPHICS_NONE:
			case GRAPHICS_PSEUDO:
			{
				/* Use "dark gray" */
				/* if (*a == TERM_WHITE) *a = TERM_L_DARK; */

				/* Playtesting -DG- */
				*a = (*a == TERM_L_DARK) ? TERM_DARK: TERM_L_DARK;

				break;
			}
			case GRAPHICS_ADAM_BOLT:
			case GRAPHICS_DAVID_GERVAIS:
			{
				*c += 1;

				break;
			}
		}
	}

	/* Handle "view_bright_light" */
	else if (view_bright_light)
	{
		switch (use_graphics)
		{
			case GRAPHICS_NONE:
			case GRAPHICS_PSEUDO:
				/* Use "gray" */
				if (*a == TERM_WHITE) *a = TERM_SLATE;
				break;
			case GRAPHICS_ADAM_BOLT:
			case GRAPHICS_DAVID_GERVAIS:
				*c += 1;
				break;
		}
	}
}


static void special_lighting_wall(byte *a, char *c, u16b feat, int info)
{
	/* Handle "seen" grids */
	if (info & (CAVE_SEEN))
	{
		/* Do nothing */
	}

	/* Handle "blind" */
	else if (p_ptr->timed[TMD_BLIND])
	{
		switch (use_graphics)
		{
			case GRAPHICS_NONE:
			case GRAPHICS_PSEUDO:
				/* Use "dark gray" */
				if (*a == TERM_WHITE) *a = TERM_L_DARK;
				break;
			case GRAPHICS_ADAM_BOLT:
			case GRAPHICS_DAVID_GERVAIS:
				if (feat_supports_lighting(feat)) *c += 1;
				break;
		}
	}

	/* Handle "view_bright_light" */
	else if (view_bright_light)
	{
		switch (use_graphics)
		{
			case GRAPHICS_NONE:
			case GRAPHICS_PSEUDO:
				/* Use "gray" */
				if (*a == TERM_WHITE) *a = TERM_SLATE;
				break;
			case GRAPHICS_ADAM_BOLT:
			case GRAPHICS_DAVID_GERVAIS:
				if (feat_supports_lighting(feat)) *c += 1;
				break;
		}
	}
	else
	{
		/* Use a brightly lit tile */
		switch (use_graphics)
		{
			case GRAPHICS_ADAM_BOLT:
				if (feat_supports_lighting(feat)) *c += 2;
				break;
			case GRAPHICS_DAVID_GERVAIS:
				if (feat_supports_lighting(feat)) *c -= 1;
				break;
		}
	}
}


/*
 * Table of complementary colors (used for shimmering traps).
 * Note the color selected for TERM_VIOLET.
 */
static byte shimmer_color_table[] =
{
	TERM_DARK,    	/* TERM_DARK */
	TERM_L_DARK,    /* TERM_L_DARK */
	TERM_VIOLET,	/* TERM_VIOLET */
	TERM_YELLOW,	/* TERM_ORANGE */
	TERM_L_RED,		/* TERM_RED */
	TERM_L_GREEN,	/* TERM_GREEN */
	TERM_L_BLUE,	/* TERM_BLUE */
	TERM_L_UMBER,	/* TERM_UMBER */
	TERM_GREEN,		/* TERM_GREEN */
	TERM_L_BLUE,	/* TERM_L_BLUE */
	TERM_L_RED,		/* TERM_VIOLET */
	TERM_ORANGE,	/* TERM_YELLOW */
	TERM_RED,		/* TERM_L_RED */
	TERM_GREEN,		/* TERM_L_GREEN	*/
	TERM_BLUE,		/* TERM_L_BLUE */
	TERM_UMBER		/* TERM_L_UMBER	*/
};


/*
 * Returns the given color or a complementary color (shimmering effects)
 */
static byte shimmer_color(byte x_attr)
{
	/* Use the given color 50% of the time */
	if (one_in_(2))
	{
		/* Save the shade */
		byte shade = GET_SHADE(x_attr);

		/* Get the shimmering color. We use only the base color */
		x_attr = shimmer_color_table[GET_BASE_COLOR(x_attr)];

		/* Re-apply the shade */
		x_attr = MAKE_EXTENDED_COLOR(x_attr, shade);
	}

	/* Done */
	return (x_attr);
}


/*
 * Checks if a square is at the (inner) edge of a trap detect area
 */
bool dtrap_edge(int y, int x)
{
	/* Check if the square is a dtrap in the first place */
 	if (!(cave_info[y][x] & (CAVE_DTRAP))) return FALSE;

 	/* Check for non-dtrap adjacent grids */
 	if (in_bounds_fully(y + 1, x    ) && (!(cave_info[y + 1][x    ] & (CAVE_DTRAP)))) return TRUE;
 	if (in_bounds_fully(y    , x + 1) && (!(cave_info[y    ][x + 1] & (CAVE_DTRAP)))) return TRUE;
 	if (in_bounds_fully(y - 1, x    ) && (!(cave_info[y - 1][x    ] & (CAVE_DTRAP)))) return TRUE;
 	if (in_bounds_fully(y    , x - 1) && (!(cave_info[y    ][x - 1] & (CAVE_DTRAP)))) return TRUE;

	return FALSE;
}


/*
 * Set attribute and character for the given hidden monster
 * Attribute is set only in graphics mode
 */
static void map_hidden_monster(monster_type *m_ptr, byte *ap, char *cp)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	u32b level;
	u32b div;

	/* Hack -- Uniques are always powerful */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		level = 9;
	}
	/* First attempt. Compare monster power with the average power at that depth */
	else if ((div = mon_power_ave[effective_depth(p_ptr->depth)][CREATURE_NON_UNIQUE]) > 0)
	{
		level = (r_ptr->mon_power * 9) / div + 1;
	}
	/* Second attempt. Compare monster's hit points with player's hit points */
	else if ((div = p_ptr->mhp) > 0)
	{
		level = (m_ptr->maxhp * 9) / div + 1;
	}
	/* Default. It should never happen */
	else
	{
		level = 9;
	}

	/* Check bounds */
	if (level > 9) level = 9;

	/* Get character */
	*cp = '0' + level;

	/* Set attribute */
	if (use_graphics) *ap = r_ptr->d_attr;
}


static void get_dtrap_edge_char(byte *a, char *c)
{
	if (use_graphics)
	{
		if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
		{
			*a = (byte)0x9A;
			*c = (char)0xC5;
		}
		else
		{
			*a = color_to_attr[TILE_BALL_INFO][TERM_GREEN];
			*c = color_to_char[TILE_BALL_INFO][TERM_GREEN];
		}

	}
	else
	{
		*a = TERM_L_GREEN;
		*c = '*';
	}
}


/*
 * Extract the attr/char to display at the given (legal) map location
 *
 * Note that this function, since it is called by "light_spot()" which
 * is called by "update_view()", is a major efficiency concern.
 *
 * Basically, we examine each "layer" of the world (terrain, objects,
 * monsters/players), from the bottom up, extracting a new attr/char
 * if necessary at each layer, and defaulting to "darkness".  This is
 * not the fastest method, but it is very simple, and it is about as
 * fast as it could be for grids which contain no "marked" objects or
 * "visible" monsters.
 *
 * We apply the effects of hallucination during each layer.  Objects will
 * always appear as random "objects", monsters will always appear as random
 * "monsters", and normal grids occasionally appear as random "monsters" or
 * "objects", but note that these random "monsters" and "objects" are really
 * just "colored ascii symbols" (which may look silly on some machines).
 *
 * The hallucination functions avoid taking any pointers to local variables
 * because some compilers refuse to use registers for any local variables
 * whose address is taken anywhere in the function.
 *
 * As an optimization, we can handle the "player" grid as a special case.
 *
 * Note that the memorization of "objects" and "monsters" is not related
 * to the memorization of "terrain".  This allows the player to memorize
 * the terrain of a grid without memorizing any objects in that grid, and
 * to detect monsters without detecting anything about the terrain of the
 * grid containing the monster.
 *
 * The fact that all interesting "objects" and "terrain features" are
 * memorized as soon as they become visible for the first time means
 * that we only have to check the "CAVE_SEEN" flag for "boring" grids.
 *
 * Note that bizarre things must be done when the "attr" and/or "char"
 * codes have the "high-bit" set, since these values are used to encode
 * various "special" pictures in some versions, and certain situations,
 * such as "multi-hued" or "clear" monsters, cause the attr/char codes
 * to be "scrambled" in various ways.
 *
 * Note that the "zero" entry in the feature/object/monster arrays are
 * used to provide "special" attr/char codes, with "monster zero" being
 * used for the player attr/char, "object zero" being used for the "pile"
 * attr/char, and "feature zero" being used for the "darkness" attr/char.
 *
 * Note that eventually we may want to use the "&" symbol for embedded
 * treasure, and use the "*" symbol to indicate multiple objects, but
 * currently, we simply use the attr/char of the first "marked" object
 * in the stack, if any, and so "object zero" is unused.  XXX XXX XXX
 *
 * Note the assumption that doing "x_ptr = &x_info[x]" plus a few of
 * "x_ptr->xxx", is quicker than "x_info[x].xxx", even if "x" is a fixed
 * constant.  If this is incorrect then a lot of code should be changed.
 *
 *
 * Some comments on the "terrain" layer...
 *
 * Note that "boring" grids (floors, invisible traps, and any illegal grids)
 * are very different from "interesting" grids (all other terrain features),
 * and the two types of grids are handled completely separately.  The most
 * important distinction is that "boring" grids may or may not be memorized
 * when they are first encountered, and so we must use the "CAVE_SEEN" flag
 * to see if they are "see-able".
 *
 *
 * Some comments on the "terrain" layer (boring grids)...
 *
 * Note that "boring" grids are always drawn using the picture for "empty
 * floors", which is stored in "f_info[FEAT_FLOOR]".  Sometimes, special
 * lighting effects may cause this picture to be modified.
 *
 * Note that "invisible traps" are always displayes exactly like "empty
 * floors", which prevents various forms of "cheating", with no loss of
 * efficiency.  There are still a few ways to "guess" where traps may be
 * located, for example, objects will never fall into a grid containing
 * an invisible trap.  XXX XXX
 *
 * To determine if a "boring" grid should be displayed, we simply check to
 * see if it is either memorized ("CAVE_MARK"), or currently "see-able" by
 * the player ("CAVE_SEEN").  Note that "CAVE_SEEN" is now maintained by the
 * "update_view()" function.
 *
 * Note the "special lighting effects" which can be activated for "boring"
 * grids using the "view_special_light" option, causing certain such grids
 * to be displayed using special colors (if they are normally "white").
 * If the grid is "see-able" by the player, we will use the normal "white"
 * (except that, if the "view_yellow_light" option is set, and the grid
 * is *only* "see-able" because of the player's torch, then we will use
 * "yellow"), else if the player is "blind", we will use "dark gray",
 * else if the grid is not "illuminated", we will use "dark gray", else
 * if the "view_bright_light" option is set, we will use "slate" (gray),
 * else we will use the normal "white".
 *
 *
 * Some comments on the "terrain" layer (non-boring grids)...
 *
 * Note the use of the "mimic" field in the "terrain feature" processing,
 * which allows any feature to "pretend" to be another feature.  This is
 * used to "hide" secret doors, and to make all "doors" appear the same,
 * and all "walls" appear the same, and "hidden" treasure stay hidden.
 * Note that it is possible to use this field to make a feature "look"
 * like a floor, but the "view_special_light" flag only affects actual
 * "boring" grids.
 *
 * Since "interesting" grids are always memorized as soon as they become
 * "see-able" by the player ("CAVE_SEEN"), such a grid only needs to be
 * displayed if it is memorized ("CAVE_MARK").  Most "interesting" grids
 * are in fact non-memorized, non-see-able, wall grids, so the fact that
 * we do not have to check the "CAVE_SEEN" flag adds some efficiency, at
 * the cost of *forcing* the memorization of all "interesting" grids when
 * they are first seen.  Since the "CAVE_SEEN" flag is now maintained by
 * the "update_view()" function, this efficiency is not as significant as
 * it was in previous versions, and could perhaps be removed.
 *
 * Note the "special lighting effects" which can be activated for "wall"
 * grids using the "view_granite_light" option, causing certain such grids
 * to be displayed using special colors (if they are normally "white").
 * If the grid is "see-able" by the player, we will use the normal "white"
 * else if the player is "blind", we will use "dark gray", else if the
 * "view_bright_light" option is set, we will use "slate" (gray), else we
 * will use the normal "white".
 *
 * Note that "wall" grids are more complicated than "boring" grids, due to
 * the fact that "CAVE_GLOW" for a "wall" grid means that the grid *might*
 * be glowing, depending on where the player is standing in relation to the
 * wall.  In particular, the wall of an illuminated room should look just
 * like any other (dark) wall unless the player is actually inside the room.
 *
 * Thus, we do not support as many visual special effects for "wall" grids
 * as we do for "boring" grids, since many of them would give the player
 * information about the "CAVE_GLOW" flag of the wall grid, in particular,
 * it would allow the player to notice the walls of illuminated rooms from
 * a dark hallway that happened to run beside the room.
 *
 *
 * Some comments on the "object" layer...
 *
 * Currently, we do nothing with multi-hued objects, because there are
 * not any.  If there were, they would have to set "shimmer_objects"
 * when they were created, and then new "shimmer" code in "dungeon.c"
 * would have to be created handle the "shimmer" effect, and the code
 * in "cave.c" would have to be updated to create the shimmer effect.
 * This did not seem worth the effort.  XXX XXX
 *
 *
 * Some comments on the "monster"/"player" layer...
 *
 * Note that monsters can have some "special" flags, including "ATTR_MULTI",
 * which means their color changes, and "ATTR_CLEAR", which means they take
 * the color of whatever is under them, and "CHAR_CLEAR", which means that
 * they take the symbol of whatever is under them.  Technically, the flag
 * "CHAR_MULTI" is supposed to indicate that a monster looks strange when
 * examined, but this flag is currently ignored.
 *
 * Normally, players could be handled just like monsters, except that the
 * concept of the "torch lite" of others player would add complications.
 * For efficiency, however, we handle the (only) player first, since the
 * "player" symbol always "pre-empts" any other facts about the grid.
 *
 *
 * ToDo: The transformations for tile colors, or brightness for the 16x16
 * tiles should be handled differently.  One possibility would be to
 * extend feature_type with attr/char definitions for the different states.
 */

#define GRAF_BROKEN_BONE 440

void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp, bool use_default)
{
	byte a, default_a;
	char c, default_c;

	u16b feat;
	u16b info;

	feature_type *f_ptr;
	object_type *o_ptr;

	s16b m_idx;

	s16b image = p_ptr->timed[TMD_IMAGE];

	int floor_num = 0;

	bool det_trap_edge = dtrap_edge(y, x);
	bool do_dtrap = FALSE;

	bool sq_flag = FALSE;
	bool do_purple_dot = TRUE;

	bool have_object = FALSE;

	/* Monster/Player */
	m_idx = cave_m_idx[y][x];

	/* Feature */
	feat = cave_feat[y][x];

	/* Cave flags */
	info = cave_info[y][x];

	/* Hack -- rare random hallucination on non-outer walls */
	if ((image) && (feat != FEAT_PERM_SOLID) && (image_count-- <= 0))
	{
		int i;

		/* Display a random image, reset count. */
		image_count = randint(200);
		i = image_random(FALSE);

		a = PICT_A(i);
		c = PICT_C(i);

		i = image_random(FALSE);
		default_a = PICT_A(i);
		default_c = PICT_C(i);

	}

	/* Boring grids (floors, etc) */
	else if (!(f_info[feat].f_flags1 & (FF1_REMEMBER)))
	{
		/* Trap detection edge */
		do_dtrap = TRUE;

		/* Memorized (or seen) floor */
		if ((info & (CAVE_MARK)) || (info & (CAVE_SEEN)))
		{
			/* Apply "mimic" field */
			feat = f_info[feat].f_mimic;

			/* Get the feature */
			f_ptr = &f_info[feat];

			/* We have seen the feature */
			f_ptr->f_everseen = TRUE;

				/* Normal attr */
			a = f_ptr->x_attr;
			default_a = f_ptr->d_attr;

			/* Normal char */
			c = f_ptr->x_char;
			default_c = f_ptr->d_char;

			/* Special lighting effects */
			if (view_special_light)
			{
				special_lighting_floor(&a, &c, info);
				special_lighting_floor(&default_a, &default_c, info);
			}
		}

		/* Unknown */
		else
		{
			/* Get the darkness feature */
			f_ptr = &f_info[FEAT_NONE];

			/* Normal attr */
			a = f_ptr->x_attr;
			default_a = f_ptr->d_attr;

			/* Normal char */
			c = f_ptr->x_char;
			default_c = f_ptr->d_char;
		}
	}

	/* Interesting grids (non-floors) */
	else
	{
		/* Memorized grids */
		if (info & (CAVE_MARK))
		{
			/* Apply "mimic" field */
			feat = f_info[feat].f_mimic;

			/* Get the feature */
			f_ptr = &f_info[feat];

			/* We have seen the feature */
			f_ptr->f_everseen = TRUE;

			/* Normal attr */
			a = f_ptr->x_attr;
			default_a = f_ptr->d_attr;

			/* Normal char */
			c = f_ptr->x_char;
			default_c = f_ptr->d_char;

			/* Special lighting effects (walls only) */
			if (view_granite_light)
			{
				special_lighting_wall(&a, &c, feat, info);
				special_lighting_wall(&default_a, &default_c, feat, info);
			}
		}

		/* Unknown */
		else
		{
			/* Trap detection edge */
			do_dtrap = TRUE;

			/* Get the darkness feature */
			f_ptr = &f_info[FEAT_NONE];

			/* Normal attr */
			a = f_ptr->x_attr;
			default_a = f_ptr->d_attr;

			/* Normal char */
			c = f_ptr->x_char;
			default_c = f_ptr->d_char;
		}
	}

	/*Reveal the square with the right cheat option*/
	if (info & (CAVE_MARKED))
	{
		if (cheat_xtra)
		{
			a = default_a = TERM_L_BLUE;
		}
	}

	/* Save the terrain info for the transparency effects */
	(*tap) = a;
	(*tcp) = c;

	/* Now add the dtrap edge characteristic as an overlay*/
	if ((do_dtrap) && (det_trap_edge))
	{
		get_dtrap_edge_char(&a, &c);
		default_a = TERM_L_GREEN;
		default_c = '*';
	}

	/* Objects */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorized objects */
		if (o_ptr->marked)
		{
			/* Remember object */
			have_object = TRUE;

			/* Hack -- object hallucination */
			if (image)
			{
				int i = image_object(FALSE);

				a = PICT_A(i);
				c = PICT_C(i);

				i = image_object(TRUE);
				default_a = PICT_A(i);
				default_c = PICT_C(i);

				break;
			}

			/*autosquelch insert*/
			sq_flag = ((mark_squelch_items) &&
				(k_info[o_ptr->k_idx].squelch == SQUELCH_ALWAYS) && (k_info[o_ptr->k_idx].aware));

			/*hack - never allow quest items to appear as dot*/
			if ((!sq_flag) || (o_ptr->ident & IDENT_QUEST))
			{
				/* Normal attr */
				a = object_attr(o_ptr);
				default_a = object_attr_default(o_ptr);

				/* Normal char */
				c = object_char(o_ptr);
				default_c = object_char_default(o_ptr);

				/*found a non-squelchable item, unless showing piles, display this one*/
				if (!show_piles) break;

				/*if only one item in a pile is not squelchable, show that one*/
				do_purple_dot = FALSE;

			}

			if (do_purple_dot)
			{
				if (use_graphics)
				{
					/* Unused violet cloud symbol */
					if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
					{
						a = (byte)0x9A;
						c = (char)0xC4;
					}
					else
					{
						/* Special squelch character HACK */
						/* This can't be right, but I am not sure what to do for graphics */
						a = k_info[GRAF_BROKEN_BONE].x_attr;

						/* Symbol of floor */
						c = k_info[GRAF_BROKEN_BONE].x_char;
					}
				}
				else
				{
					/* Special squelch character HACK */
					/* Colour of Blade of Chaos */
					a = TERM_VIOLET;
					default_a = a;
					/* Symbol of floor */
					c = f_info[1].x_char;
					default_c = f_info[1].d_char;
				}
			}

			/* Special stack symbol, unless everything in the pile is squelchable */
			else if (++floor_num > 1)
			{
				if ((use_graphics) && (arg_graphics == GRAPHICS_DAVID_GERVAIS))
				{
					a = (byte)0x87;
					c = (char)0xB7;
				}

				else
				{
					object_kind *k_ptr;

					/* Get the "pile" feature */
					k_ptr = &k_info[0];

					/* Normal attr */
					a = k_ptr->x_attr;
					default_a = k_ptr->d_attr;

					/* Normal char */
					c = k_ptr->x_char;
					default_c = k_ptr->d_char;
				}
				break;
			}
		}
	}

	/* Hack -- Objects on known dangerous locations get the attribute of the terrain */
	if (have_object && !use_graphics && !image && (info & (CAVE_MARK)) &&
		 (f_info[cave_feat[y][x]].dam_non_native > 0))
	{
		a = (*tap);
	}

	/* Handle effects */
	if ((cave_x_idx[y][x] > 0) && (info & (CAVE_SEEN | CAVE_MARK)))
	{
		/* Visual ordering of the effects, biggest number first */
		static byte priority_table[] =
		{
			0,	/* nothing */
			30,	/* EFFECT_LINGERING_CLOUD */
			40,	/* EFFECT_SHIMMERING_CLOUD */
			10,	/* EFFECT_PERMANENT_CLOUD */
			20,	/* EFFECT_TRAP_SMART  */
			20,	/* EFFECT_TRAP_DUMB  */
			20,	/* EFFECT_TRAP_PLAYER  */
			20,	/* EFFECT_GLYPH */
			50,	/* EFFECT_GLACIER */
			9,	/* EFFECT_INSCRIPTION */
		};

		int max_priority = -1;

		/* No suitable effect yet */
		effect_type *x_ptr = NULL;

		/* Get the first effect */
		u16b x_idx = cave_x_idx[y][x];

		/* Scan the effects on that grid */
		while (x_idx)
		{
			u16b feat;

			/* Default priority */
			byte priority = 0;

			/* Get the effect data */
			effect_type *tmp_x_ptr = &x_list[x_idx];

			/* Point to the next effect */
			x_idx = tmp_x_ptr->next_x_idx;

			/* Ignore hidden effects */
			if (!(tmp_x_ptr->x_f_idx) || (tmp_x_ptr->x_flags & (EF1_HIDDEN))) continue;

			/* Get the mimic field */
			feat = f_info[tmp_x_ptr->x_f_idx].f_mimic;

			/* We have seen this effect */
 			f_info[feat].f_everseen = TRUE;

			/* Hack - Permanent clouds and inscriptions shouldn't override objects */
			if (have_object && ((tmp_x_ptr->x_type == EFFECT_PERMANENT_CLOUD) ||
				(tmp_x_ptr->x_type == EFFECT_INSCRIPTION))) continue;

			/* Get the priority of the effect, if possible */
			if (tmp_x_ptr->x_type < N_ELEMENTS(priority_table))
			{
				priority = priority_table[tmp_x_ptr->x_type];
			}

 			/* We have a candidate */
			if (priority > max_priority)
			{
				/* Remember the effect */
				x_ptr = tmp_x_ptr;

				/* Remember priority */
				max_priority = priority;
			}
		}

		/* Do we have a visible effect? */
		if (x_ptr)
		{
			u16b feat;

			/* Apply "mimic" field to the feature effect. */
			feat = f_info[x_ptr->x_f_idx].f_mimic;

			/* Get the feature */
			f_ptr = &f_info[feat];

			/* We have seen the feature */
			f_ptr->f_everseen = TRUE;

			/* No special effects in graphics mode */
			if (use_graphics)
			{
				a = f_ptr->x_attr;
			}
			/* Permanent clouds */
			else if (x_ptr->x_type == EFFECT_PERMANENT_CLOUD)
			{
				/* Get the feature under the cloud */
				u16b feat2 = cave_feat[y][x];

				/* Not boring floor? */
				if ((feat2 != FEAT_FLOOR) && (feat2 != FEAT_COBBLESTONE_FLOOR))
				{
					/* Get mimiced feature */
					feat2 = f_info[feat2].f_mimic;

					/* Use the color of that feature */
					a = shimmer_color(f_info[feat2].x_attr);
					default_a = shimmer_color(f_info[feat2].d_attr);
				}
				/* Harmless terrain */
				else
				{
					/* Use the default color of the cloud */
					a = gf_color(f_ptr->x_gf_type);
					default_a = a;
				}
			}
			/* Lingering clouds */
			else if (x_ptr->x_type == EFFECT_LINGERING_CLOUD)
			{
				a = gf_color(f_ptr->x_gf_type);
				default_a = a;
			}
			/* Smart traps */
			else if (x_ptr->x_type == EFFECT_TRAP_SMART)
			{
				a = shimmer_color(f_ptr->x_attr);
				default_a = shimmer_color(f_ptr->d_attr);
			}
			/* Glacier */
			else if (x_ptr->x_type == EFFECT_GLACIER)
			{
				a = shimmer_color(f_ptr->x_attr);
				default_a = shimmer_color(f_ptr->d_attr);
			}
			/* Traps, glyphs, etc. */
			else
			{
				a = f_ptr->x_attr;
				default_a = f_ptr->d_attr;
			}

			/* Normal char */
			c = f_ptr->x_char;
			default_c = f_ptr->d_char;
		}
	}

	/* Monsters */
	if (m_idx > 0)
	{
		monster_type *m_ptr = &mon_list[m_idx];

		/* Visible monster*/
		if (m_ptr->ml)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			byte da, default_da;
			char dc, default_dc;

			/* Desired attr */
			da = r_ptr->x_attr;
			default_da = r_ptr->d_attr;

			/* Desired char */
			dc = r_ptr->x_char;
			default_dc = r_ptr->d_char;

			/* Hack -- monster hallucination */
			if (image)
			{
				int i = image_monster(FALSE);

				a = PICT_A(i);
				c = PICT_C(i);

				i = image_monster(TRUE);
				default_a = PICT_A(i);
				default_c = PICT_C(i);
			}

			/* Special attr/char codes */
			else if ((da & 0x80) && (dc & 0x80))
			{
				/* Use attr */
				a = da;
				default_a = default_da;

				/* Use char */
				c = dc;
				default_c = default_dc;
			}

			/* Multi-hued monster */
			else if (r_ptr->flags1 & (RF1_ATTR_MULTI))
			{
				/* Multi-hued attr */
				a = m_ptr->m_attr ? m_ptr->m_attr : 1;
				default_a = default_da;

				/* Normal char */
				c = dc;
				default_c = default_dc;
			}

			/* Normal monster (not "clear" in any way) */
			else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR | RF1_CHAR_CLEAR)))
			{
				/* Use attr */
				a = da;
				default_a = default_da;

				/* Use char */
				c = dc;
				default_c = default_dc;
			}

			/* Hack -- Bizarre grid under monster */
			else if ((a & 0x80) || (c & 0x80))
			{
				/* Use attr */
				a = da;
				default_a = default_da;

				/* Use char */
				c = dc;
				default_c = default_dc;
			}

			/* Normal char, Clear attr, monster */
			else if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)))
			{
				/* Normal char */
				c = dc;
				default_c = default_dc;
			}

			/* Normal attr, Clear char, monster */
			else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)))
			{
				/* Normal attr */
				a = da;
				default_a = default_da;
			}

			/* Hack -- hidden monsters */
			/* TODO -- pick a tile for graphics mode */
			/* Note that we keep the original looks if the monster is being detected */
			if (((m_ptr->mflag & (MFLAG_MARK | MFLAG_HIDE)) == (MFLAG_HIDE)) && (!image))
			{
				map_hidden_monster(m_ptr, &a, &c);
				default_a = a;
				default_c = c;
			}
		}
	}

	/* Handle "player" */
	else if (m_idx < 0)
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		/*  DSV:  I've chosen the following sequence of colors to indicate
				the player's current HP.  There are colors are left over, but I
				left them in this comment for easy reference, in the likely case
				that I decide to change the order of color changes.

			TERM_WHITE		90-100% of HP remaining
			TERM_YELLOW		70- 89% of HP remaining
			TERM_ORANGE		50- 69% of HP remaining
			TERM_L_RED		30- 49% of HP remaining
			TERM_RED		 0- 29% of HP remaining

		*/

		if ((hp_changes_color) && (arg_graphics == GRAPHICS_NONE))
		{
			/* check for hunger first */
			a = TERM_WHITE;
			if (p_ptr->food < PY_FOOD_ALERT)
			{
				a = TERM_L_UMBER;
			}
			
			/* overwrite with health check */
			switch(p_ptr->chp * 10 / p_ptr->mhp)
			{
				case  8:
				case  7:	a = TERM_YELLOW ;	break;
				case  6:
				case  5:	a = TERM_ORANGE ;	break;
				case  4:
				case  3:	a = TERM_L_RED  ;	break;
				case  2:
				case  1:
				case  0:	a = TERM_RED    ;	break;
			}

			default_a = a;
		}

		else
		{
			a = r_ptr->x_attr;
			default_a = r_ptr->d_attr;
		}

		/* Get the "player" char */
		c = r_ptr->x_char;
		default_c = r_ptr->d_char;
	}

	if (use_default)
	{
		(*ap) = default_a;
		(*cp) = default_c;
	}
	else
	{
		/* Result */
		(*ap) = a;
		(*cp) = c;
	}
}


/*
 * Move the cursor to a given map location.
 */
static void move_cursor_relative_map(int y, int x)
{
	int ky, kx;

	term *old;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MAP))) continue;

		/* Location relative to panel */
		ky = y - t->offset_y;

		/* Verify location */
		if ((ky < 0) || (ky >= t->hgt)) continue;

		/* Location relative to panel */
		kx = x - t->offset_x;

		if (use_bigtile) kx += kx;

		/* Verify location */
		if ((kx < 0) || (kx >= t->wid)) continue;

		/* Go there */
		old = Term;
		Term_activate(t);
		(void)Term_gotoxy(kx, ky);
		Term_activate(old);
	}
}


/*
 * Move the cursor to a given map location.
 *
 * The main screen will always be at least 24x80 in size.
 */
void move_cursor_relative(int y, int x)
{
	int ky, kx;
	int vy, vx;

	/* Move the cursor on map sub-windows */
	move_cursor_relative_map(y, x);

	/* Location relative to panel */
	ky = y - Term->offset_y;

	/* Verify location */
	if ((ky < 0) || (ky >= SCREEN_HGT)) return;

	/* Location relative to panel */
	kx = x - Term->offset_x;

	/* Verify location */
	if ((kx < 0) || (kx >= SCREEN_WID)) return;

	/* Location in window */
	vy = ky + ROW_MAP;

	/* Location in window */
	vx = kx + COL_MAP;

	if (use_bigtile) vx += kx;

	/* Go there */
	(void)Term_gotoxy(vx, vy);
}


/*
 * Display an attr/char pair at the given map location
 *
 * Note the inline use of "panel_contains()" for efficiency.
 *
 * Note the use of "Term_queue_char()" for efficiency.
 */
static void print_rel_map(char c, byte a, int y, int x)
{
	int ky, kx;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MAP))) continue;

		/* Location relative to panel */
		ky = y - t->offset_y;

		/* Verify location */
		if ((ky < 0) || (ky >= t->hgt)) continue;

		/* Location relative to panel */
		kx = x - t->offset_x;

		if (use_bigtile)
		{
			kx += kx;
			if (kx + 1 >= t->wid) continue;
		}

		/* Verify location */
		if ((kx < 0) || (kx >= t->wid)) continue;

		/* Hack -- Queue it */
		Term_queue_char(t, kx, ky, a, c, 0, 0);

		if (use_bigtile)
		{
			/* Mega-Hack : Queue dummy char */
			if (a & 0x80)
				Term_queue_char(t, kx+1, ky, 255, -1, 0, 0);
			else
				Term_queue_char(t, kx+1, ky, TERM_WHITE, ' ', 0, 0);
		}

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);
	}
}


/*
 * Display an attr/char pair at the given map location
 *
 * Note the inline use of "panel_contains()" for efficiency.
 *
 * Note the use of "Term_queue_char()" for efficiency.
 *
 * The main screen will always be at least 24x80 in size.
 */
void print_rel(char c, byte a, int y, int x)
{
	int ky, kx;
	int vy, vx;

	/* Print on map sub-windows */
	print_rel_map(c, a, y, x);

	/* Location relative to panel */
	ky = y - Term->offset_y;

	/* Verify location */
	if ((ky < 0) || (ky >= SCREEN_HGT)) return;

	/* Location relative to panel */
	kx = x - Term->offset_x;

	/* Verify location */
	if ((kx < 0) || (kx >= SCREEN_WID)) return;

	/* Location in window */
	vy = ky + ROW_MAP;

	/* Location in window */
	vx = kx + COL_MAP;

	if (use_bigtile) vx += kx;

	/* Hack -- Queue it */
	Term_queue_char(Term, vx, vy, a, c, 0, 0);

	if (use_bigtile)
	{
		/* Mega-Hack : Queue dummy char */
		if (a & 0x80)
			Term_queue_char(Term, vx+1, vy, 255, -1, 0, 0);
		else
			Term_queue_char(Term, vx+1, vy, TERM_WHITE, ' ', 0, 0);
	}
}


/*
 * Memorize interesting viewable object/features in the given grid
 *
 * This function should only be called on "legal" grids.
 *
 * This function will memorize the object and/or feature in the given grid,
 * if they are (1) see-able and (2) interesting.  Note that all objects are
 * interesting, all terrain features except floors (and invisible traps) are
 * interesting, and floors (and invisible traps) are interesting sometimes
 * (depending on various options involving the illumination of floor grids).
 *
 * The automatic memorization of all objects and non-floor terrain features
 * as soon as they are displayed allows incredible amounts of optimization
 * in various places, especially "map_info()" and this function itself.
 *
 * Note that the memorization of objects is completely separate from the
 * memorization of terrain features, preventing annoying floor memorization
  * when a detected object is picked up from a dark floor, and object
 * memorization when an object is dropped into a floor grid which is
 * memorized but out-of-sight.
 *
 * This function should be called every time the "memorization" of a grid
 * (or the object in a grid) is called into question, such as when an object
 * is created in a grid, when a terrain feature "changes" from "floor" to
 * "non-floor", and when any grid becomes "see-able" for any reason.
 *
 * This function is called primarily from the "update_view()" function, for
 * each grid which becomes newly "see-able".
 */
void note_spot(int y, int x)
{
	u16b info;

	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	object_type *o_ptr;

	/* Get cave info */
	info = cave_info[y][x];

	/* Require "seen" flag */
	if (!(info & (CAVE_SEEN))) return;

	/* Hack -- memorize objects */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorize objects */
		o_ptr->marked = TRUE;
	}

	/* Hack -- memorize grids */
	if (!(info & (CAVE_MARK)))
	{
		/* Memorize some "boring" grids */
		if (!_feat_ff1_match(f_ptr, FF1_REMEMBER))
		{
			/* Option -- memorize certain floors */
			if ((view_perma_grids &&
				(info & (CAVE_GLOW | CAVE_HALO))) ||
				(view_torch_grids &&
				_feat_ff2_match(f_ptr, FF2_ATTR_LIGHT)))
			{
				/* Memorize */
				cave_info[y][x] |= (CAVE_MARK);
			}

			/* Hack -- Emulate the REMEMBER flag for certain effects */
 			else
 			{
 				/* Get the first effect on that grid */
 				int x_idx = cave_x_idx[y][x];

 				/* Iterate through the effects */
 				while (x_idx)
 				{
 					/* Get the effect data */
 					effect_type *x_ptr = &x_list[x_idx];

 					/* Prepare the next effect */
 					x_idx = x_ptr->next_x_idx;

 					/* Ignore hidden effects */
 					if (x_ptr->x_flags & (EF1_HIDDEN)) continue;

 					/* We'll remember traps and glyphs for now */
 					if (!(x_ptr->x_flags & (EF1_TRAP_DUMB |
 							EF1_TRAP_SMART | EF1_TRAP_PLAYER |
 							EF1_GLYPH))) continue;

 					/* Remember the grid */
 					cave_info[y][x] |= (CAVE_MARK);

 					/* Done */
 					break;
 				}
 			}

		}

		/* Memorize all "interesting" grids */
		else
		{
			/* Memorize */
			cave_info[y][x] |= (CAVE_MARK);
		}
	}
}


static void light_spot_map(int y, int x)
{
	byte a, ta;
	char c, tc;

	int ky, kx;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MAP))) continue;

		/* Location relative to panel */
		ky = y - t->offset_y;
		kx = x - t->offset_x;

		if (use_bigtile)
		{
			kx += kx;
			if (kx + 1 >= t->wid) continue;
		}

		/* Verify location */
		if ((ky < 0) || (ky >= t->hgt)) continue;
		if ((kx < 0) || (kx >= t->wid)) continue;

		/* Hack -- redraw the grid */
		map_info(y, x, &a, &c, &ta, &tc, FALSE);

		/* Hack -- Queue it */
		Term_queue_char(t, kx, ky, a, c, ta, tc);

		if (use_bigtile)
		{
			kx++;

			/* Mega-Hack : Queue dummy char */
			if (a & 0x80)
				Term_queue_char(t, kx, ky, 255, -1, 0, 0);
			else
				Term_queue_char(t, kx, ky, TERM_WHITE, ' ', TERM_WHITE, ' ');
		}

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);
	}
}


/*
 * Redraw (on the screen) a given map location
 *
 * This function should only be called on "legal" grids.
 *
 * Note the inline use of "print_rel()" for efficiency.
 *
 * The main screen will always be at least 24x80 in size.
 */
void light_spot(int y, int x)
{
	byte a;
	char c;
	byte ta;
	char tc;

	int ky, kx;
	int vy, vx;

	/* Update map sub-windows */
	light_spot_map(y, x);

	/* Location relative to panel */
	ky = y - Term->offset_y;

	/* Verify location */
	if ((ky < 0) || (ky >= SCREEN_HGT)) return;

	/* Location relative to panel */
	kx = x - Term->offset_x;

	/* Verify location */
	if ((kx < 0) || (kx >= SCREEN_WID)) return;

	/* Location in window */
	vy = ky + ROW_MAP;

	/* Location in window */
	vx = kx + COL_MAP;

	if (use_bigtile) vx += kx;

	/* Hack -- redraw the grid */
	map_info(y, x, &a, &c, &ta, &tc, FALSE);

	/* Hack -- Queue it */
	Term_queue_char(Term, vx, vy, a, c, ta, tc);

	if (use_bigtile)
	{
		vx++;

		/* Mega-Hack : Queue dummy char */
		if (a & 0x80)
			Term_queue_char(Term, vx, vy, 255, -1, 0, 0);
		else
			Term_queue_char(Term, vx, vy, TERM_WHITE, ' ', TERM_WHITE, ' ');
	}

	/* Update the object list */
	if (cave_m_idx[y][x] > 0) p_ptr->redraw |= PR_MONLIST;

	/* Update the object list */
	if (cave_o_idx[y][x] > 0) p_ptr->redraw |= PR_ITEMLIST;
}


static void prt_map_aux(void)
{
	byte a;
	char c;
	byte ta;
	char tc;

	int y, x;
	int vy, vx;
	int ty, tx;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MAP))) continue;

		/* Assume screen */
		ty = t->offset_y + t->hgt;
		tx = t->offset_x + t->wid;

		if (use_bigtile) tx = t->offset_x + (t->wid / 2);

		/* Dump the map */
		for (y = t->offset_y, vy = 0; y < ty; vy++, y++)
		{
			for (x = t->offset_x, vx = 0; x < tx; vx++, x++)
			{
				/* Check bounds */
				if (!in_bounds(y, x)) continue;

				if (use_bigtile && (vx + 1 >= t->wid)) continue;

				/* Determine what is there */
				map_info(y, x, &a, &c, &ta, &tc, FALSE);

				/* Hack -- Queue it */
				Term_queue_char(t, vx, vy, a, c, ta, tc);

				if (use_bigtile)
				{
					vx++;

					/* Mega-Hack : Queue dummy char */
					if (a & 0x80)
						Term_queue_char(t, vx, vy, 255, -1, 0, 0);
					else
						Term_queue_char(t, vx, vy, TERM_WHITE, ' ', TERM_WHITE, ' ');
				}
			}
		}

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);
	}
}


/*
 * Redraw (on the screen) the current map panel
 *
 * Note the inline use of "light_spot()" for efficiency.
 *
 * The main screen will always be at least 24x80 in size.
 */
void prt_map(void)
{
	byte a;
	char c;
	byte ta;
	char tc;

	int y, x;
	int vy, vx;
	int ty, tx;

	/* Redraw map sub-windows */
	prt_map_aux();

	/* Assume screen */
	ty = Term->offset_y + SCREEN_HGT;
	tx = Term->offset_x + SCREEN_WID;

	/* Dump the map */
	for (y = Term->offset_y, vy = ROW_MAP; y < ty; vy++, y++)
	{
		for (x = Term->offset_x, vx = COL_MAP; x < tx; vx++, x++)
		{
			/* Check bounds */
			if (!in_bounds(y, x)) continue;

			/* Determine what is there */
			map_info(y, x, &a, &c, &ta, &tc, FALSE);

			/* Hack -- Queue it */
			Term_queue_char(Term, vx, vy, a, c, ta, tc);

			if (use_bigtile)
			{
				vx++;

				/* Mega-Hack : Queue dummy char */
				if (a & 0x80)
					Term_queue_char(Term, vx, vy, 255, -1, 0, 0);
				else
					Term_queue_char(Term, vx, vy, TERM_WHITE, ' ', TERM_WHITE, ' ');
			}
		}
	}
}


/*
 * Hack -- a priority function (see below)
 */
static byte priority(byte a, char c)
{
	int i, p1;

	feature_type *f_ptr;

	/* Scan the table */
	for (i = 0; i < z_info->f_max; i++)
	{
		/* Priority level */
		p1 = f_info[i].priority;

		/* Get the feature */
		f_ptr = &f_info[i];

		/* Check character and attribute, accept matches */
		if ((f_ptr->x_char == c) && (f_ptr->x_attr == a)) return (p1);
	}

	/* Default */
	return (20);
}


/*
 * Display a "small-scale" map of the dungeon in the active Term.
 *
 * Note that this function must "disable" the special lighting effects so
 * that the "priority" function will work.
 *
 * Note the use of a specialized "priority" function to allow this function
 * to work with any graphic attr/char mappings, and the attempts to optimize
 * this function where possible.
 *
 * If "cy" and "cx" are not NULL, then returns the screen location at which
 * the player was displayed, so the cursor can be moved to that location,
 * and restricts the horizontal map size to SCREEN_WID.  Otherwise, nothing
 * is returned (obviously), and no restrictions are enforced.
 */
void display_map(int *cy, int *cx)
{
	int map_hgt, map_wid;
	int row, col;

	int x, y;

	byte ta;
	char tc;

	byte tp;

	/* Large array on the stack */
	byte mp[MAX_DUNGEON_HGT][MAX_DUNGEON_WID];

	bool old_view_special_light;
	bool old_view_granite_light;

	monster_race *r_ptr = &r_info[0];

	/* Desired map height */
	map_hgt = Term->hgt - 2;
	map_wid = Term->wid - 2;

	/* Prevent accidents */
	if (map_hgt > p_ptr->cur_map_hgt) map_hgt = p_ptr->cur_map_hgt;
	if (map_wid > p_ptr->cur_map_wid) map_wid = p_ptr->cur_map_wid;

	/* Prevent accidents */
	if ((map_wid < 1) || (map_hgt < 1)) return;

	/* Save lighting effects */
	old_view_special_light = view_special_light;
	old_view_granite_light = view_granite_light;

	/* Disable lighting effects */
	view_special_light = FALSE;
	view_granite_light = FALSE;

	/* Nothing here */
	ta = TERM_WHITE;
	tc = ' ';

	/* Clear the priorities */
	for (y = 0; y < MAX_DUNGEON_HGT; ++y)
	{
		for (x = 0; x < MAX_DUNGEON_WID; ++x)
		{
			/* No priority */
			mp[y][x] = 0;
		}
	}

	/* Clear the screen (but don't force a redraw) */
	clear_from(0);

	/* Corners */
	x = map_wid + 1;
	y = map_hgt + 1;

	/* Draw the corners */
	Term_putch(0, 0, ta, '+');
	Term_putch(x, 0, ta, '+');
	Term_putch(0, y, ta, '+');
	Term_putch(x, y, ta, '+');

	/* Draw the horizontal edges */
	for (x = 1; x <= map_wid; x++)
	{
		Term_putch(x, 0, ta, '-');
		Term_putch(x, y, ta, '-');
	}

	/* Draw the vertical edges */
	for (y = 1; y <= map_hgt; y++)
	{
		Term_putch(0, y, ta, '|');
		Term_putch(x, y, ta, '|');
	}

	/* Analyze the actual map */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			row = (y * map_hgt / p_ptr->cur_map_hgt);
			col = (x * map_wid / p_ptr->cur_map_wid);

			if (use_bigtile)
				col = col & ~1;

			/* Get the attr/char at that map location */
			map_info(y, x, &ta, &tc, &ta, &tc, FALSE);

			/* Get the priority of that attr/char */
			tp = priority(ta, tc);

			/* Examine boring grids */
			if ((tp == 20) && (cave_m_idx[y][x] > 0))
			{
				monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Notice dangerous monsters */
				tp = MAX(20, (int)r_ptr->level - (2 * p_ptr->lev / 3) + 20);

				/* Ignore invisible monsters */
				if (!m_ptr->ml) tp = 20;
			}

			/* Save "best" */
			if (mp[row][col] < tp)
			{
				/* Add the character */
				Term_putch(col + 1, row + 1, ta, tc);

				if (use_bigtile)
				{
					if (ta & 0x80)
						Term_putch(col + 2, row + 1, 255, -1);
					else
						Term_putch(col + 2, row + 1, TERM_WHITE, ' ');
				}

				/* Save priority */
				mp[row][col] = tp;
			}
		}
	}

	/* Player location */
	row = (p_ptr->py * map_hgt / p_ptr->cur_map_hgt);
	col = (p_ptr->px * map_wid / p_ptr->cur_map_wid);

	if (use_bigtile)
		col = col & ~1;

	/*** Make sure the player is visible ***/

	/* Get the "player" attr */
	ta = r_ptr->x_attr;

	/* Get the "player" char */
	tc = r_ptr->x_char;

	/* Draw the player */
	Term_putch(col + 1, row + 1, ta, tc);

	/* Return player location */
	if (cy != NULL) (*cy) = row + 1;
	if (cx != NULL) (*cx) = col + 1;

	/* Restore lighting effects */
	view_special_light = old_view_special_light;
	view_granite_light = old_view_granite_light;
}


/*
 * Display a "small-scale" map of the dungeon.
 *
 * Note that the "player" is always displayed on the map.
 */
void do_cmd_view_map(void)
{
	int cy, cx;
	cptr prompt = "Hit any key to continue";

	/* Save screen */
	screen_save();

	/* Note */
	prt("Please wait...", 0, 0);

	/* Flush */
	(void)Term_fresh();

	/* Clear the screen */
	Term_clear();

	/* Display the map */
	display_map(&cy, &cx);

	/* Show the prompt */
	put_str(prompt, Term->hgt - 1, Term->wid / 2 - strlen(prompt) / 2);

	/* Hilite the player */
	Term_gotoxy(cx, cy);

	/* Get any key */
	(void)inkey();

	/* Load screen */
	screen_load();
}


/*
 * Maximum number of grids in a single octant
 */
#define VINFO_MAX_GRIDS 161


/*
 * Maximum number of slopes in a single octant
 */
#define VINFO_MAX_SLOPES 126


/*
 * Mask of bits used in a single octant
 */
#define VINFO_BITS_3 0x3FFFFFFF
#define VINFO_BITS_2 0xFFFFFFFF
#define VINFO_BITS_1 0xFFFFFFFF
#define VINFO_BITS_0 0xFFFFFFFF


/*
 * Forward declare
 */
typedef struct vinfo_type vinfo_type;


/*
 * The 'vinfo_type' structure
 */
struct vinfo_type
{
	u16b grid[8];

	/* LOS slopes (up to 128) */
	u32b bits_3;
	u32b bits_2;
	u32b bits_1;
	u32b bits_0;

	/* Index of the first LOF slope */
	byte slope_fire_index1;

	/* Index of the (possible) second LOF slope */
	byte slope_fire_index2;

	vinfo_type *next_0;
	vinfo_type *next_1;

	byte y;
	byte x;
	byte d;
	byte r;
};


/*
 * The array of "vinfo" objects, initialized by "vinfo_init()"
 */
static vinfo_type vinfo[VINFO_MAX_GRIDS];


/*
 * Slope scale factor
 */
#define SCALE 100000L


/*
 * The actual slopes (for reference)
 */

/* Bit :     Slope   Grids */
/* --- :     -----   ----- */
/*   0 :      2439      21 */
/*   1 :      2564      21 */
/*   2 :      2702      21 */
/*   3 :      2857      21 */
/*   4 :      3030      21 */
/*   5 :      3225      21 */
/*   6 :      3448      21 */
/*   7 :      3703      21 */
/*   8 :      4000      21 */
/*   9 :      4347      21 */
/*  10 :      4761      21 */
/*  11 :      5263      21 */
/*  12 :      5882      21 */
/*  13 :      6666      21 */
/*  14 :      7317      22 */
/*  15 :      7692      20 */
/*  16 :      8108      21 */
/*  17 :      8571      21 */
/*  18 :      9090      20 */
/*  19 :      9677      21 */
/*  20 :     10344      21 */
/*  21 :     11111      20 */
/*  22 :     12000      21 */
/*  23 :     12820      22 */
/*  24 :     13043      22 */
/*  25 :     13513      22 */
/*  26 :     14285      20 */
/*  27 :     15151      22 */
/*  28 :     15789      22 */
/*  29 :     16129      22 */
/*  30 :     17241      22 */
/*  31 :     17647      22 */
/*  32 :     17948      23 */
/*  33 :     18518      22 */
/*  34 :     18918      22 */
/*  35 :     20000      19 */
/*  36 :     21212      22 */
/*  37 :     21739      22 */
/*  38 :     22580      22 */
/*  39 :     23076      22 */
/*  40 :     23809      22 */
/*  41 :     24137      22 */
/*  42 :     24324      23 */
/*  43 :     25714      23 */
/*  44 :     25925      23 */
/*  45 :     26315      23 */
/*  46 :     27272      22 */
/*  47 :     28000      23 */
/*  48 :     29032      23 */
/*  49 :     29411      23 */
/*  50 :     29729      24 */
/*  51 :     30434      23 */
/*  52 :     31034      23 */
/*  53 :     31428      23 */
/*  54 :     33333      18 */
/*  55 :     35483      23 */
/*  56 :     36000      23 */
/*  57 :     36842      23 */
/*  58 :     37142      24 */
/*  59 :     37931      24 */
/*  60 :     38461      24 */
/*  61 :     39130      24 */
/*  62 :     39393      24 */
/*  63 :     40740      24 */
/*  64 :     41176      24 */
/*  65 :     41935      24 */
/*  66 :     42857      23 */
/*  67 :     44000      24 */
/*  68 :     44827      24 */
/*  69 :     45454      23 */
/*  70 :     46666      24 */
/*  71 :     47368      24 */
/*  72 :     47826      24 */
/*  73 :     48148      24 */
/*  74 :     48387      24 */
/*  75 :     51515      25 */
/*  76 :     51724      25 */
/*  77 :     52000      25 */
/*  78 :     52380      25 */
/*  79 :     52941      25 */
/*  80 :     53846      25 */
/*  81 :     54838      25 */
/*  82 :     55555      24 */
/*  83 :     56521      25 */
/*  84 :     57575      26 */
/*  85 :     57894      25 */
/*  86 :     58620      25 */
/*  87 :     60000      23 */
/*  88 :     61290      25 */
/*  89 :     61904      25 */
/*  90 :     62962      25 */
/*  91 :     63636      25 */
/*  92 :     64705      25 */
/*  93 :     65217      25 */
/*  94 :     65517      25 */
/*  95 :     67741      26 */
/*  96 :     68000      26 */
/*  97 :     68421      26 */
/*  98 :     69230      26 */
/*  99 :     70370      26 */
/* 100 :     71428      25 */
/* 101 :     72413      26 */
/* 102 :     73333      26 */
/* 103 :     73913      26 */
/* 104 :     74193      27 */
/* 105 :     76000      26 */
/* 106 :     76470      26 */
/* 107 :     77777      25 */
/* 108 :     78947      26 */
/* 109 :     79310      26 */
/* 110 :     80952      26 */
/* 111 :     81818      26 */
/* 112 :     82608      26 */
/* 113 :     84000      26 */
/* 114 :     84615      26 */
/* 115 :     85185      26 */
/* 116 :     86206      27 */
/* 117 :     86666      27 */
/* 118 :     88235      27 */
/* 119 :     89473      27 */
/* 120 :     90476      27 */
/* 121 :     91304      27 */
/* 122 :     92000      27 */
/* 123 :     92592      27 */
/* 124 :     93103      28 */
/* 125 :    100000      13 */


/*
 * Forward declare
 */
typedef struct vinfo_hack vinfo_hack;


/*
 * Temporary data used by "vinfo_init()"
 *
 *	- Number of line of sight slopes
 *
 *	- Slope values
 *
 *	- Slope range for each grid
 */
struct vinfo_hack
{
	int num_slopes;

	long slopes[VINFO_MAX_SLOPES];

	long slopes_min[MAX_SIGHT+1][MAX_SIGHT+1];
	long slopes_max[MAX_SIGHT+1][MAX_SIGHT+1];
};


/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
static bool ang_sort_comp_hook_longs(const void *u, const void *v, int a, int b)
{
	long *x = (long*)(u);

	/* Unused parameter */
	(void)v;

	return (x[a] <= x[b]);
}


/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
static void ang_sort_swap_hook_longs(void *u, void *v, int a, int b)
{
	long *x = (long*)(u);

	long temp;

	/* Unused parameter */
	(void)v;

	/* Swap */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;
}


/*
 * Save a slope
 */
static void vinfo_init_aux(vinfo_hack *hack, int y, int x, long m)
{
	int i;

	/* Handle "legal" slopes */
	if ((m > 0) && (m <= SCALE))
	{
		/* Look for that slope */
		for (i = 0; i < hack->num_slopes; i++)
		{
			if (hack->slopes[i] == m) break;
		}

		/* New slope */
		if (i == hack->num_slopes)
		{
			/* Paranoia */
			if (hack->num_slopes >= VINFO_MAX_SLOPES)
			{
				quit_fmt("Too many LOS slopes (%d)!", VINFO_MAX_SLOPES);
			}

			/* Save the slope, increment count */
			hack->slopes[hack->num_slopes++] = m;
		}
	}

	/* Track slope range */
	if (hack->slopes_min[y][x] > m) hack->slopes_min[y][x] = m;
	if (hack->slopes_max[y][x] < m) hack->slopes_max[y][x] = m;

}


/*
 * Initialize the "vinfo" array
 *
 * Full Octagon (radius 20), Grids=1149
 *
 * Quadrant (south east), Grids=308, Slopes=251
 *
 * Octant (east then south), Grids=161, Slopes=126
 *
 * This function assumes that VINFO_MAX_GRIDS and VINFO_MAX_SLOPES
 * have the correct values, which can be derived by setting them to
 * a number which is too high, running this function, and using the
 * error messages to obtain the correct values.
 */
errr vinfo_init(void)
{
	int i;
	int y, x;
	u16b g;

	long m;

	vinfo_hack *hack;

	int num_grids = 0;

	int queue_head = 0;
	int queue_tail = 0;
	vinfo_type *queue[VINFO_MAX_GRIDS*2];


	/* Make hack */
	hack = ZNEW(vinfo_hack);


	/* Analyze grids */
	for (y = 0; y <= MAX_SIGHT; ++y)
	{
		for (x = y; x <= MAX_SIGHT; ++x)
		{
			/* Skip grids which are out of sight range */
			if (distance(0, 0, y, x) > MAX_SIGHT) continue;

			/* Default slope range */
			hack->slopes_min[y][x] = 999999999;
			hack->slopes_max[y][x] = 0;

			/* Paranoia */
			if (num_grids >= VINFO_MAX_GRIDS)
			{
				quit_fmt("Too many grids (%d >= %d)!",
					num_grids, VINFO_MAX_GRIDS);
			}

			/* Count grids */
			num_grids++;

			/* Slope to the top right corner */
			m = SCALE * (1000L * y - 500) / (1000L * x + 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);

			/* Slope to top left corner */
			m = SCALE * (1000L * y - 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);

			/* Slope to bottom right corner */
			m = SCALE * (1000L * y + 500) / (1000L * x + 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);

			/* Slope to bottom left corner */
			m = SCALE * (1000L * y + 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);
		}
	}

	/* Enforce maximal efficiency (grids) */
	if (num_grids < VINFO_MAX_GRIDS)
	{
		quit_fmt("Too few grids (%d < %d)!",
			num_grids, VINFO_MAX_GRIDS);
	}

	/* Enforce maximal efficiency (line of sight slopes) */
	if (hack->num_slopes < VINFO_MAX_SLOPES)
	{
		quit_fmt("Too few LOS slopes (%d < %d)!",
			hack->num_slopes, VINFO_MAX_SLOPES);
	}

	/* Sort slopes numerically */
	ang_sort_comp = ang_sort_comp_hook_longs;

	/* Sort slopes numerically */
	ang_sort_swap = ang_sort_swap_hook_longs;

	/* Sort the (unique) LOS slopes */
	ang_sort(hack->slopes, NULL, hack->num_slopes);


	/* Enqueue player grid */
	queue[queue_tail++] = &vinfo[0];

	/* Process queue */
	while (queue_head < queue_tail)
	{
		int e;

		/* Index */
		e = queue_head++;

		/* Main Grid */
		g = vinfo[e].grid[0];

		/* Location */
		y = GRID_Y(g);
		x = GRID_X(g);

		/* Compute grid offsets */
		vinfo[e].grid[0] = GRID(+y,+x);
		vinfo[e].grid[1] = GRID(+x,+y);
		vinfo[e].grid[2] = GRID(+x,-y);
		vinfo[e].grid[3] = GRID(+y,-x);
		vinfo[e].grid[4] = GRID(-y,-x);
		vinfo[e].grid[5] = GRID(-x,-y);
		vinfo[e].grid[6] = GRID(-x,+y);
		vinfo[e].grid[7] = GRID(-y,+x);


		/* Skip player grid */
		if (e > 0)
		{
			long slope_fire;

			long tmp0 = 0;
			long tmp1 = 0;
			long tmp2 = 999999L;

			/* Determine LOF slope for this grid */
			if (x == 0) slope_fire = SCALE;
			else slope_fire = SCALE * (1000L * y) / (1000L * x);

			/* Analyze LOS slopes */
			for (i = 0; i < hack->num_slopes; ++i)
			{
				m = hack->slopes[i];

				/* Memorize intersecting slopes */
				if ((hack->slopes_min[y][x] < m) &&
				    (hack->slopes_max[y][x] > m))
				{
					/* Add it to the LOS slope set */
					switch (i / 32)
					{
						case 3: vinfo[e].bits_3 |= (1L << (i % 32)); break;
						case 2: vinfo[e].bits_2 |= (1L << (i % 32)); break;
						case 1: vinfo[e].bits_1 |= (1L << (i % 32)); break;
						case 0: vinfo[e].bits_0 |= (1L << (i % 32)); break;
					}

					/* Check for exact match with the LOF slope */
					if (m == slope_fire) tmp0 = i;

					/* Remember index of nearest LOS slope < than LOF slope */
					else if ((m < slope_fire) && (m > tmp1)) tmp1 = i;

					/* Remember index of nearest LOS slope > than LOF slope */
					else if ((m > slope_fire) && (m < tmp2)) tmp2 = i;
				}
			}

			/* There is a perfect match with one of the LOS slopes */
			if (tmp0)
			{
				/* Save the (unique) slope */
				vinfo[e].slope_fire_index1 = tmp0;
				vinfo[e].slope_fire_index2 = tmp0;
			}

			/* The LOF slope lies between two LOS slopes */
			else
			{
				/* Save the first slope */
				vinfo[e].slope_fire_index1 = tmp1;

				/* Save the second slope */
				vinfo[e].slope_fire_index2 = tmp2;
			}
		}

		/* Default */
		vinfo[e].next_0 = &vinfo[0];

		/* Grid next child */
		if (distance(0, 0, y, x+1) <= MAX_SIGHT)
		{
			g = GRID(y,x+1);

			if (queue[queue_tail-1]->grid[0] != g)
			{
				vinfo[queue_tail].grid[0] = g;
				queue[queue_tail] = &vinfo[queue_tail];
				queue_tail++;
			}

			vinfo[e].next_0 = &vinfo[queue_tail - 1];
		}


		/* Default */
		vinfo[e].next_1 = &vinfo[0];

		/* Grid diag child */
		if (distance(0, 0, y+1, x+1) <= MAX_SIGHT)
		{
			g = GRID(y+1,x+1);

			if (queue[queue_tail-1]->grid[0] != g)
			{
				vinfo[queue_tail].grid[0] = g;
				queue[queue_tail] = &vinfo[queue_tail];
				queue_tail++;
			}

			vinfo[e].next_1 = &vinfo[queue_tail - 1];
		}


		/* Hack -- main diagonal has special children */
		if (y == x) vinfo[e].next_0 = vinfo[e].next_1;


		/* Grid coordinates, approximate distance  */
		vinfo[e].y = y;
		vinfo[e].x = x;
		vinfo[e].d = ((y > x) ? (y + x/2) : (x + y/2));
		vinfo[e].r = ((!y) ? x : (!x) ? y : (y == x) ? y : 0);
	}

	/* Verify maximal bits XXX XXX XXX */
	if (((vinfo[1].bits_3 | vinfo[2].bits_3) != VINFO_BITS_3) ||
	    ((vinfo[1].bits_2 | vinfo[2].bits_2) != VINFO_BITS_2) ||
	    ((vinfo[1].bits_1 | vinfo[2].bits_1) != VINFO_BITS_1) ||
	    ((vinfo[1].bits_0 | vinfo[2].bits_0) != VINFO_BITS_0))
	{
		quit("Incorrect bit masks!");
	}


	/* Kill hack */
	FREE(hack);


	/* Success */
	return (0);
}


/*
 * Forget the "CAVE_VIEW" grids, redrawing as needed
 */
void forget_view(void)
{
	int i, g;

	int fast_view_n = view_n;
	u16b *fast_view_g = view_g;

	u16b *fast_cave_info = &cave_info[0][0];


	/* None to forget */
	if (!fast_view_n) return;

	/* Clear them all */
	for (i = 0; i < fast_view_n; i++)
	{
		int y, x;

		/* Grid */
		g = fast_view_g[i];

		/* Location */
		y = GRID_Y(g);
		x = GRID_X(g);

		/* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
		fast_cave_info[g] &= ~(CAVE_VIEW | CAVE_SEEN);

		/* Clear "CAVE_LITE" flag */
		/* fast_cave_info[g] &= ~(CAVE_LITE); */

		/* Redraw */
		light_spot(y, x);
	}

	/* None left */
	fast_view_n = 0;

	/* Save 'view_n' */
	view_n = fast_view_n;

	/* Clear the CAVE_FIRE flag */
	for (i = 0; i < fire_n; i++)
	{
		/* Grid */
		g = fire_g[i];

		/* Clear */
		fast_cave_info[g] &= ~(CAVE_FIRE);
	}

	/* None left */
	fire_n = 0;
}


/*
 * Calculate the complete field of view using a new algorithm
 *
 * If "view_g" and "temp_g" were global pointers to arrays of grids, as
 * opposed to actual arrays of grids, then we could be more efficient by
 * using "pointer swapping".
 *
 * Note the following idiom, which is used in the function below.
 * This idiom processes each "octant" of the field of view, in a
 * clockwise manner, starting with the east strip, south side,
 * and for each octant, allows a simple calculation to set "g"
 * equal to the proper grids, relative to "pg", in the octant.
 *
 *   for (o2 = 0; o2 < 8; o2++)
 *   ...
 *         g = pg + p->grid[o2];
 *   ...
 *
 *
 * Normally, vision along the major axes is more likely than vision
 * along the diagonal axes, so we check the bits corresponding to
 * the lines of sight near the major axes first.
 *
 * We use the "temp_g" array (and the "CAVE_TEMP" flag) to keep track of
 * which grids were previously marked "CAVE_SEEN", since only those grids
 * whose "CAVE_SEEN" value changes during this routine must be redrawn.
 *
 * This function is now responsible for maintaining the "CAVE_SEEN"
 * flags as well as the "CAVE_VIEW" flags, which is good, because
 * the only grids which normally need to be memorized and/or redrawn
 * are the ones whose "CAVE_SEEN" flag changes during this routine.
 *
 * Basically, this function divides the "octagon of view" into octants of
 * grids (where grids on the main axes and diagonal axes are "shared" by
 * two octants), and processes each octant one at a time, processing each
 * octant one grid at a time, processing only those grids which "might" be
 * viewable, and setting the "CAVE_VIEW" flag for each grid for which there
 * is an (unobstructed) line of sight from the center of the player grid to
 * any internal point in the grid (and collecting these "CAVE_VIEW" grids
 * into the "view_g" array), and setting the "CAVE_SEEN" flag for the grid
 * if, in addition, the grid is "illuminated" in some way.
 *
 * This function relies on a theorem (suggested and proven by Mat Hostetter)
 * which states that in each octant of a field of view, a given grid will
 * be "intersected" by one or more unobstructed "lines of sight" from the
 * center of the player grid if and only if it is "intersected" by at least
 * one such unobstructed "line of sight" which passes directly through some
 * corner of some grid in the octant which is not shared by any other octant.
 * The proof is based on the fact that there are at least three significant
 * lines of sight involving any non-shared grid in any octant, one which
 * intersects the grid and passes though the corner of the grid closest to
 * the player, and two which "brush" the grid, passing through the "outer"
 * corners of the grid, and that any line of sight which intersects a grid
 * without passing through the corner of a grid in the octant can be "slid"
 * slowly towards the corner of the grid closest to the player, until it
 * either reaches it or until it brushes the corner of another grid which
 * is closer to the player, and in either case, the existanc of a suitable
 * line of sight is thus demonstrated.
 *
 * It turns out that in each octant of the radius 20 "octagon of view",
 * there are 161 grids (with 128 not shared by any other octant), and there
 * are exactly 126 distinct "lines of sight" passing from the center of the
 * player grid through any corner of any non-shared grid in the octant.  To
 * determine if a grid is "viewable" by the player, therefore, you need to
 * simply show that one of these 126 lines of sight intersects the grid but
 * does not intersect any wall grid closer to the player.  So we simply use
 * a bit vector with 126 bits to represent the set of interesting lines of
 * sight which have not yet been obstructed by wall grids, and then we scan
 * all the grids in the octant, moving outwards from the player grid.  For
 * each grid, if any of the lines of sight which intersect that grid have not
 * yet been obstructed, then the grid is viewable.  Furthermore, if the grid
 * is a wall grid, then all of the lines of sight which intersect the grid
 * should be marked as obstructed for future reference.  Also, we only need
 * to check those grids for whom at least one of the "parents" was a viewable
 * non-wall grid, where the parents include the two grids touching the grid
 * but closer to the player grid (one adjacent, and one diagonal).  For the
 * bit vector, we simply use 4 32-bit integers.  All of the static values
 * which are needed by this function are stored in the large "vinfo" array
 * (above), which is machine generated by another program.  XXX XXX XXX
 *
 * Hack -- The queue must be able to hold more than VINFO_MAX_GRIDS grids
 * because the grids at the edge of the field of view use "grid zero" as
 * their children, and the queue must be able to hold several of these
 * special grids.  Because the actual number of required grids is bizarre,
 * we simply allocate twice as many as we would normally need.  XXX XXX XXX
 */
void update_view(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	u16b pg = GRID(py,px);

	int i, j, o2, d;
	u16b g;

	int radius;

	/*used for monster lite patch*/
	int fy,fx,k;

	int fast_view_n = view_n;
	u16b *fast_view_g = view_g;

	int fast_temp_n = 0;
	u16b *fast_temp_g = temp_g;

	u16b *fast_cave_info = &cave_info[0][0];

	u16b info;

	/*** Step 0 -- Begin ***/

	/* Save the old "view" grids for later */
	for (i = 0; i < fast_view_n; i++)
	{
		/* Grid */
		g = fast_view_g[i];

		/* Get grid info */
		info = fast_cave_info[g];

		/* Save "CAVE_SEEN" grids */
		if (info & (CAVE_SEEN))
		{
			/* Set "CAVE_TEMP" flag */
			info |= (CAVE_TEMP);

			/* Save grid for later */
			fast_temp_g[fast_temp_n++] = g;
		}

		/* Clear "CAVE_VIEW", "CAVE_SEEN" & cave_fire flags */
		info &= ~(CAVE_VIEW | CAVE_SEEN);

		/* Clear "CAVE_LIGHT" flag */
		/* info &= ~(CAVE_LIGHT); */

		/* Save cave info */
		fast_cave_info[g] = info;
	}

	/* Reset the "view" array */
	fast_view_n = 0;

	/* Clear the CAVE_FIRE flag */
	for (i = 0; i < fire_n; i++)
	{
		/* Grid */
		g = fire_g[i];

		/* Clear */
		fast_cave_info[g] &= ~(CAVE_FIRE);
	}

	/* Reset the "fire" array */
	fire_n = 0;

	/* Extract "radius" value */
	radius = p_ptr->state.cur_light;

	/* Handle real light */
	if (radius > 0) ++radius;

	/* Scan monster list and add monster lites */
	for ( k = 1; k < mon_max; k++)
	{
		/* Check the k'th monster */
		monster_type *m_ptr = &mon_list[k];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		/* Carrying lite */
		if (r_ptr->flags2 & (RF2_HAS_LIGHT))
		{
			for (i = -1; i <= 1; i++)
			{
				for (j = -1; j <= 1; j++)
				{
					/*
					 * Compute distance, so you don't have an empty lite
					 * floating around past the max_site range.
					 */

					int dy = (p_ptr->py > (fy+i)) ? (p_ptr->py - (fy+i)) : ((fy+i) - p_ptr->py);
					int dx = (p_ptr->px > (fx+j)) ? (p_ptr->px - (fx+j)) : ((fx+j) - p_ptr->px);

					/* Approximate distance */
					d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

					if ((d <= MAX_SIGHT) && (los(p_ptr->py,p_ptr->px,fy+i,fx+j)))
					{
						g = GRID(fy+i,fx+j);

						info = fast_cave_info[g];

						info |= (CAVE_VIEW);
						info |= (CAVE_SEEN);

						/* Save cave info */
						fast_cave_info[g] = info;

						/* Save in array */
						fast_view_g[fast_view_n++] = g;
					}

				}
			}

		}

	}

	/*** Step 1 -- player grid ***/

	/* Player grid */
	g = pg;

	/* Get grid info */
	info = fast_cave_info[g];

	/* Assume viewable */
	info |= (CAVE_VIEW | CAVE_FIRE);

	/* Torch-lit grid */
	if (0 < radius)
	{
		/* Mark as "CAVE_SEEN" */
		info |= (CAVE_SEEN);
	}

	/* Perma-lit grid */
	else if (info & (CAVE_GLOW | CAVE_HALO))
	{
		/* Mark as "CAVE_SEEN" */
		info |= (CAVE_SEEN);
	}

	/* Save cave info */
	fast_cave_info[g] = info;

	/* Save in the "view" array */
	fast_view_g[fast_view_n++] = g;

	/* Save in the "fire" array */
	fire_g[fire_n++] = g;

	/*** Step 2a -- octants (CAVE_VIEW + CAVE_SEEN) ***/

	/* Scan each octant */
	for (o2 = 0; o2 < 8; o2++)
	{
		vinfo_type *p;

		/* Last added */
		vinfo_type *last = &vinfo[0];

		/* Grid queue */
		int queue_head = 0;
		int queue_tail = 0;
		vinfo_type *queue[VINFO_MAX_GRIDS*2];

		/* Slope bit vector */
		u32b bits0 = VINFO_BITS_0;
		u32b bits1 = VINFO_BITS_1;
		u32b bits2 = VINFO_BITS_2;
		u32b bits3 = VINFO_BITS_3;

		/* Reset queue */
		queue_head = queue_tail = 0;

		/* Initial grids */
		queue[queue_tail++] = &vinfo[1];
		queue[queue_tail++] = &vinfo[2];

		/* Process queue */
		while (queue_head < queue_tail)
		{
			/* Dequeue next grid */
			p = queue[queue_head++];

			/* Check bits */
			if ((bits0 & (p->bits_0)) ||
			    (bits1 & (p->bits_1)) ||
			    (bits2 & (p->bits_2)) ||
			    (bits3 & (p->bits_3)))
			{
				/* Extract grid value XXX XXX XXX */
				g = pg + p->grid[o2];

				/* Get grid info */
				info = fast_cave_info[g];

				/* Handle opaque grids */
				if (!(info & (CAVE_LOS)))
				{
					/* Clear bits */
					bits0 &= ~(p->bits_0);
					bits1 &= ~(p->bits_1);
					bits2 &= ~(p->bits_2);
					bits3 &= ~(p->bits_3);

					/* Newly viewable grid */
					if (!(info & (CAVE_VIEW)))
					{
						/* Mark as viewable */
						info |= (CAVE_VIEW);

						/* Torch-lit grids */
						if (p->d < radius)
						{
							/* Mark as "CAVE_SEEN" */
							info |= (CAVE_SEEN);

							/* Mark as "CAVE_LIGHT" */
							/* info |= (CAVE_LIGHT); */
						}

						/* Perma-lit grids */
						else if (info & (CAVE_GLOW))
						{
							int y = GRID_Y(g);
							int x = GRID_X(g);

							/* Hack -- move towards player */
							int yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
							int xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

							/* Check for "complex" illumination */
							if (((cave_info[yy][xx] & (CAVE_LOS)) &&
								 (cave_info[yy][xx] & (CAVE_GLOW))) ||
								((cave_info[y][xx] & (CAVE_LOS)) &&
								 (cave_info[y][xx] & (CAVE_GLOW))) ||
								((cave_info[yy][x] & (CAVE_LOS)) &&
								 (cave_info[yy][x] & (CAVE_GLOW))))
							{
								/* Mark as seen */
								info |= (CAVE_SEEN);
							}

						}


						/* Save in array */
						fast_view_g[fast_view_n++] = g;
					}
				}

				/* Handle transparent grids */
				else
				{
					/* Enqueue child */
					if (last != p->next_0)
					{
						queue[queue_tail++] = last = p->next_0;
					}

					/* Enqueue child */
					if (last != p->next_1)
					{
						queue[queue_tail++] = last = p->next_1;
					}

					/* Newly viewable grid */
					if (!(info & (CAVE_VIEW)))
					{
						/* Mark as "viewable" */
						info |= (CAVE_VIEW);

						/* Torch-lit grids */
						if (p->d < radius)
						{
							/* Mark as "CAVE_SEEN" */
							info |= (CAVE_SEEN);

							/* Mark as "CAVE_LIGHT" */
							/* info |= (CAVE_LIGHT); */
						}

						/* Perma-lit grids */
						else if (info & (CAVE_GLOW | CAVE_HALO))
						{
							/* Mark as "CAVE_SEEN" */
							info |= (CAVE_SEEN);
						}

						/* Save in array */
						fast_view_g[fast_view_n++] = g;
					}
				}

				/* Save cave info */
				fast_cave_info[g] = info;

			}
		}
	}

	/*** Step 2b -- octants (CAVE_FIRE) ***/

	/* Scan each octant */
	for (o2 = 0; o2 < 8; o2++)
	{
		vinfo_type *p;

		/* Last added */
		vinfo_type *last = &vinfo[0];

		/* Grid queue */
		int queue_head = 0;
		int queue_tail = 0;
		vinfo_type *queue[VINFO_MAX_GRIDS*2];

		/* Slope bit vector */
		u32b bits0 = VINFO_BITS_0;
		u32b bits1 = VINFO_BITS_1;
		u32b bits2 = VINFO_BITS_2;
		u32b bits3 = VINFO_BITS_3;

		/* Reset queue */
		queue_head = queue_tail = 0;

		/* Initial grids */
		queue[queue_tail++] = &vinfo[1];
		queue[queue_tail++] = &vinfo[2];

		/* Process queue */
		while (queue_head < queue_tail)
		{
			/* Assume no line of fire */
			bool line_fire = FALSE;

			/* Dequeue next grid */
			p = queue[queue_head++];

			/* Check bits */
			if ((bits0 & (p->bits_0)) ||
			    (bits1 & (p->bits_1)) ||
			    (bits2 & (p->bits_2)) ||
			    (bits3 & (p->bits_3)))
			{
				/* Extract grid value XXX XXX XXX */
				g = pg + p->grid[o2];

				/* Get grid info */
				info = fast_cave_info[g];

				/* Check for first possible line of fire */
				i = p->slope_fire_index1;

				/* Check line(s) of fire */
				while (TRUE)
				{
					switch (i / 32)
					{
						case 3:
						{
							if (bits3 & (1L << (i % 32))) line_fire = TRUE;
							break;
						}
						case 2:
						{
							if (bits2 & (1L << (i % 32))) line_fire = TRUE;
							break;
						}
						case 1:
						{
							if (bits1 & (1L << (i % 32))) line_fire = TRUE;
							break;
						}
						case 0:
						{
							if (bits0 & (1L << (i % 32))) line_fire = TRUE;
							break;
						}
					}

					/* Check second LOF slope only if necessary */
					if ((line_fire) ||
					    (p->slope_fire_index2 == p->slope_fire_index1) ||
					    (i == p->slope_fire_index2))
					{
						break;
					}

					/* Check second possible line of fire */
					i = p->slope_fire_index2;
				}

				/* Note line of fire */
				if (line_fire)
				{
					info |= (CAVE_FIRE);

					/* Save in array */
					fire_g[fire_n++] = g;
				}

				/* Handle non-projectable grids */
				if (!(info & (CAVE_PROJECT)))
				{
					/* Clear bits */
					bits0 &= ~(p->bits_0);
					bits1 &= ~(p->bits_1);
					bits2 &= ~(p->bits_2);
					bits3 &= ~(p->bits_3);
				}

				/* Handle projectable grids */
				else
				{
					/* Enqueue child */
					if (last != p->next_0)
					{
						queue[queue_tail++] = last = p->next_0;
					}

					/* Enqueue child */
					if (last != p->next_1)
					{
						queue[queue_tail++] = last = p->next_1;
					}
				}

				/* Save cave info */
				fast_cave_info[g] = info;
			}
		}
	}


	/*** Step 3 -- Complete the algorithm ***/

	/* Handle blindness */
	if (p_ptr->timed[TMD_BLIND])
	{
		/* Process "new" grids */
		for (i = 0; i < fast_view_n; i++)
		{
			/* Grid */
			g = fast_view_g[i];

			/* Grid cannot be "CAVE_SEEN" */
			fast_cave_info[g] &= ~(CAVE_SEEN);
		}
	}

	/* Process "new" grids */
	for (i = 0; i < fast_view_n; i++)
	{
		/* Grid */
		g = fast_view_g[i];

		/* Get grid info */
		info = fast_cave_info[g];

		/* Was not "CAVE_SEEN", is now "CAVE_SEEN" */
		if ((info & (CAVE_SEEN)) && !(info & (CAVE_TEMP)))
		{
			int y, x;

			/* Location */
			y = GRID_Y(g);
			x = GRID_X(g);

			/* Note */
			note_spot(y, x);

			/* Redraw */
			light_spot(y, x);
		}
	}

	/* Process "old" grids */
	for (i = 0; i < fast_temp_n; i++)
	{
		/* Grid */
		g = fast_temp_g[i];

		/* Get grid info */
		info = fast_cave_info[g];

		/* Clear "CAVE_TEMP" flag */
		info &= ~(CAVE_TEMP);

		/* Save cave info */
		fast_cave_info[g] = info;

		/* Was "CAVE_SEEN", is now not "CAVE_SEEN" */
		if (!(info & (CAVE_SEEN)))
		{
			int y, x;

			/* Location */
			y = GRID_Y(g);
			x = GRID_X(g);

			/* Redraw */
			light_spot(y, x);
		}
	}


	/* Save 'view_n' */
	view_n = fast_view_n;
}


static int get_energy_no_doors(int y, int x, byte which_flow, u32b elem_flag)
{
	/*Quick pointer*/
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/*Unused*/
	(void)elem_flag;

	/*We have not done this flow yet*/
	if (cave_cost[which_flow][y][x] == 0)
	{
		/* Can't move here */
		if (!cave_passable_bold(y, x)) return (0);
	}

	/*This is an elemental terrain*/
	if (_feat_ff3_match(f_ptr, TERRAIN_MASK))
	{
		/*Monsters don't want to suffer damage going to this square*/
		if (f_ptr->dam_non_native) return (0);
	}

	/*Use non-native energy movement*/
	return(f_ptr->non_native_energy_move);
}


static int get_energy_pass_doors(int y, int x, byte which_flow, u32b elem_flag)
{

	/*Quick pointer*/
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/*Use the elem_flag for a completey different purpose. */
	elem_flag = _feat_ff3_match(f_ptr, TERRAIN_MASK);

	/*We have not done this flow yet*/
	if (cave_cost[which_flow][y][x] == 0)
	{
		/* Can't move here */
		if (!cave_passable_bold(y, x))
		{
			 /* An effect is preventing movement. Ignore grid */
			if (cave_x_idx[y][x]) return (0);

			/* Closed doors are allowed */
			if (!_feat_ff1_match(f_ptr, FF1_DOOR)) return (0);
		}
	}

	/*This is an elemental terrain*/
	if (elem_flag)
	{
		/* Mark any flows that need doing*/
		p_ptr->update |= elem_flag;

		/*Monsters don't want to suffer damage going to this square*/
		if (f_ptr->dam_non_native) return (0);
	}

	/*Use non-native energy movement*/
	return(f_ptr->non_native_energy_move);
}


static int get_energy_elemental_flow(int y, int x, byte which_flow, u32b elem_flag)
{
	/*Quick pointer*/
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/*We have not done this flow yet*/
	if (cave_cost[which_flow][y][x] == 0)
	{
		/* Can't move here */
		if (!cave_passable_bold(y, x))
		{
			/* An effect is preventing movement. Ignore grid */
			if (cave_x_idx[y][x]) return (0);

			/* Closed doors are allowed */
			if (!_feat_ff1_match(f_ptr, FF1_DOOR)) return (0);
		}
	}

	/*If a match, return the native movement energy*/
	if (_feat_ff3_match(f_ptr, elem_flag))
	{
		return (f_ptr->native_energy_move);
	}

	/*Not native*/
	/*Monsters don't want to suffer damage going to this square*/
	if (f_ptr->dam_non_native) return (0);

	/*Use non-native energy movement*/
	return(f_ptr->non_native_energy_move);
}


static int get_energy_elemental_flow_no_doors(int y, int x, byte which_flow, u32b elem_flag)
{
	/*Quick pointer*/
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/*We have not done this flow yet*/
	if (cave_cost[which_flow][y][x] == 0)
	{
		/* Can't move here */
		if (!cave_passable_bold(y, x)) return 0;
	}

	/*If a match, return the native movement energy*/
	if (_feat_ff3_match(f_ptr, elem_flag))
	{
		return (f_ptr->native_energy_move);
	}

	/*Not native*/
	/*Monsters don't want to suffer damage going to this square*/
	if (f_ptr->dam_non_native) return (0);

	/*Use non-native energy movement*/
	return(f_ptr->non_native_energy_move);

}


static int get_energy_flying(int y, int x, byte which_flow, u32b elem_flag)
{

	/*Quick pointer*/
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/* Use the elem_flag for a completey different purpose */
	elem_flag = _feat_ff2_match(f_ptr, FF2_CAN_FLY);

	/*We have not done this flow yet*/
	if (cave_cost[which_flow][y][x] == 0)
	{
		/* Can't move here */
		if (!cave_passable_bold(y, x))
		{
			/* An effect is preventing movement. Ignore */
			if (cave_x_idx[y][x]) return 0;

			/* Feature doesn't have the CAN_FLY flag and it isn't a door. Ignore */
			if (!elem_flag && !_feat_ff1_match(f_ptr, FF1_DOOR)) return 0;
		}
	}

	/*We can fly here, non-native damage doesn't matter*/
	if (elem_flag)
	{
		return (BASE_ENERGY_MOVE);
	}

	/*Monsters don't want to suffer damage going to this square*/
	if (f_ptr->dam_non_native) return (0);

	/*Use non-native energy movement*/
	return (f_ptr->non_native_energy_move);
}


static int get_energy_flying_no_doors(int y, int x, byte which_flow, u32b elem_flag)
{

	/*Quick pointer*/
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/* Use the elem_flag for a completey different purpose */
	elem_flag = _feat_ff2_match(f_ptr, FF2_CAN_FLY);

	/*We have not done this flow yet*/
	if (cave_cost[which_flow][y][x] == 0)
	{
		/* Can't move here */
		if (!cave_passable_bold(y, x))
		{
			/* An effect is preventing movement. Ignore */
			if (cave_x_idx[y][x]) return 0;

			/* Feature doesn't have the CAN_FLY. Ignore */
			if (!elem_flag) return 0;
		}
	}

	/*We can fly here, non-native damage doesn't matter*/
	if (elem_flag)
	{
		return (BASE_ENERGY_MOVE);
	}

	/*Monsters don't want to suffer damage going to this square*/
	if (f_ptr->dam_non_native) return (0);

	/*Use non-native energy movement*/
	return (f_ptr->non_native_energy_move);
}

/*Movement through walls*/
static int get_energy_pass_walls(int y, int x, byte which_flow, u32b elem_flag)
{

	/*Quick pointer*/
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/*Unused*/
	(void)elem_flag;

	/*We have not done this flow yet*/
	if (cave_cost[which_flow][y][x] == 0)
	{
		/*The monster can't go here*/
		if (_feat_ff1_match(f_ptr, FF1_PERMANENT | FF1_MOVE) == (FF1_PERMANENT)) return (0);
	}

	/*Monsters don't want to suffer damage going to this square*/
	if (f_ptr->dam_non_native) return (0);

	/*Use non-native energy movement*/
	return(MIN(BASE_ENERGY_MOVE, f_ptr->non_native_energy_move));
}


/*
 *
 *
 *Partial update of the monster movement flow code.
 */
static void update_flow_partial(byte which_flow, u32b elem_flag)
{

	int i, d, k;
	byte y, x, y2, x2;
	int last_index;
	int grid_count = 0;


	/* Note where we get information from, and where we overwrite */
	int this_cycle = 0;
	int next_cycle = 1;

	byte flow_table[2][2][15 * FLOW_LENGTH_PARTIAL];

	/*Reduce the base flow*/
	cost_at_center[which_flow] -= (ENERGY_TO_MOVE * 2);

	/*Point to the right function for the flows*/
	if (which_flow == FLOW_NO_DOORS) get_energy_to_move = get_energy_no_doors;
	else if (which_flow == FLOW_PASS_DOORS) get_energy_to_move = get_energy_pass_doors;
	else if (which_flow == FLOW_FLYING) get_energy_to_move = get_energy_flying;
	else if (which_flow == FLOW_FLYING_NO_DOORS) get_energy_to_move = get_energy_flying_no_doors;
	else if (which_flow == FLOW_PASS_WALLS) get_energy_to_move = get_energy_pass_walls;
	/* All the native elements*/
	else if (which_flow >= ELEM_FLOW_BASE_NO_DOORS) get_energy_to_move = get_energy_elemental_flow_no_doors;
	else get_energy_to_move = get_energy_elemental_flow;

	/* Store base cost at the character location */
	cave_cost[which_flow][p_ptr->py][p_ptr->px] = cost_at_center[which_flow];

	/* Store this first grid in the flow table, note that we've done so */
	flow_table[this_cycle][0][0] = p_ptr->py;
	flow_table[this_cycle][1][0] = p_ptr->px;
	grid_count = 1;

	/*** Partial Update of the flow ***/

	/* Extend the noise burst out to its limits */
	for (k = 0; k < FLOW_LENGTH_PARTIAL; k++)
	{
		/* Get the number of grids we'll be looking at */
		last_index = grid_count;

		/* Stop if we've run out of work to do */
		if (last_index == 0) break;

		/* Clear the grid count */
		grid_count = 0;

		/* Get each valid entry in the flow table in turn. */
		for (i = 0; i < last_index; i++)
		{
			/* Get this grid */
			y = flow_table[this_cycle][0][i];
			x = flow_table[this_cycle][1][i];

			/* Look at all adjacent grids */
			for (d = 0; d < 8; d++)
			{
				int new_cave_cost;
				int energy_to_move;

				/* Child location */
				y2 = y + ddy_ddd[d];
				x2 = x + ddx_ddd[d];

				/* Check Bounds */
				if (!in_bounds(y2, x2)) continue;

				/*Use a functional pointer for speed to get the proper energy*/
				energy_to_move = (*get_energy_to_move)(y2, x2, which_flow, elem_flag);

				/*Not a useable square*/
				if (energy_to_move == 0) continue;

				/*Calculate the energy required to move to this square*/
				new_cave_cost = cave_cost[which_flow][y][x] + energy_to_move;

				/*This is not a quicker route*/
				if ((cave_cost[which_flow][y2][x2] > 0) &&
						 (cave_cost[which_flow][y2][x2] <= new_cave_cost)) continue;

				/* Store cost at this location */
				cave_cost[which_flow][y2][x2] = new_cave_cost;

				/*Don't store the same grid twice*/
				if (cave_info[y2][x2] & (CAVE_FLOW)) continue;

				/*mark that we stored this grid for the next cycle*/
				cave_info[y2][x2] |= (CAVE_FLOW);

				/* Store this grid in the flow table */
				flow_table[next_cycle][0][grid_count] = y2;
				flow_table[next_cycle][1][grid_count] = x2;

				/* Increment number of grids stored */
				grid_count++;
			}
		}

		/*Clear the CAVE_FLOW marks*/
		for (i = 0; i < grid_count; i++)
		{
			/* Not very legible, but this is using the y and x coordinates from the nest cycle*/
			cave_info[flow_table[next_cycle][0][i]][flow_table[next_cycle][1][i]] &= ~(CAVE_FLOW);
		}

		/* Swap write and read portions of the table */
		if (this_cycle == 0)
		{
			this_cycle = 1;
			next_cycle = 0;
		}
		else
		{
			this_cycle = 0;
			next_cycle = 1;
		}
	}
}


/*
 *Clear the monster_flow for partial updating
 */
static void update_flows_partial_prep(void)
{
	int i, j, d, k;
	byte y, x, y2, x2;
	int last_index;
	int grid_count = 0;

	/* Note where we get information from, and where we overwrite */
	int this_cycle = 0;
	int next_cycle = 1;

	byte flow_table[2][2][12 * FLOW_LENGTH_PARTIAL];

	byte clear_flows[MAX_FLOWS];
	byte active_flows = 0;

	/*Clear all the cave_cost at the player center*/
	/*Also record the active flows*/
	for (i = FLOW_NO_DOORS; i < MAX_FLOWS; i++)
	{
		/*We have an active flow*/
		if (cost_at_center[i] > 0)
		{
			clear_flows[active_flows] = i;
			active_flows++;

			/* Store cost at this location */
			cave_cost[i][p_ptr->py][p_ptr->px] = 0;
		}
	}

	/* Save the last update noise epicenter */
	p_ptr->update_center_y = p_ptr->py;
	p_ptr->update_center_x = p_ptr->px;

	/* Store this first grid in the flow table, note that we've done so */
	flow_table[this_cycle][0][0] = p_ptr->py;
	flow_table[this_cycle][1][0] = p_ptr->px;
	grid_count = 1;

	/*** Partial Update of the flow ***/

	/* Extend the noise burst out to its limits */
	for (k = 0; k < FLOW_LENGTH_PARTIAL; k++)
	{
		/* Get the number of grids we'll be looking at */
		last_index = grid_count;

		/* Stop if we've run out of work to do */
		if (last_index == 0) break;

		/* Clear the grid count */
		grid_count = 0;

		/* Get each valid entry in the flow table in turn. */
		for (i = 0; i < last_index; i++)
		{
			/* Get this grid */
			y = flow_table[this_cycle][0][i];
			x = flow_table[this_cycle][1][i];

			/* Look at all adjacent grids */
			for (d = 0; d < 8; d++)
			{
				/* Child location */
				y2 = y + ddy_ddd[d];
				x2 = x + ddx_ddd[d];

				/* Check Bounds */
				if (!in_bounds(y2, x2)) continue;

				/*Ignore updated features already*/

				/* Ignore walls. */
				if (!cave_passable_bold(y2, x2)) continue;

				/*We have done this one already*/
				if (cave_cost[FLOW_PASS_DOORS][y2][x2] == 0) continue;

				/*Clear all the active flows*/
				for (j = 0; j < active_flows; j++)
				{
					/* Store cost at this location */
					cave_cost[clear_flows[j]][y2][x2] = 0;
				}

				/* Store this grid in the flow table */
				flow_table[next_cycle][0][grid_count] = y2;
				flow_table[next_cycle][1][grid_count] = x2;

				/* Increment number of grids stored */
				grid_count++;
			}
		}

		/* Swap write and read portions of the table */
		if (this_cycle == 0)
		{
			this_cycle = 1;
			next_cycle = 0;
		}
		else
		{
			this_cycle = 0;
			next_cycle = 1;
		}
	}
}


/*
 * Fill in the "cave_cost" field of every grid that the player can
 * reach with the amount of time needed to reach that grid.
 * Monsters use this information by moving to adjacent grids with
 * lower flow costs, thereby homing in on the player even though
 * twisty tunnels and mazes.  Monsters can also run away.
 *
 *
 * The flow table is three-dimensional.  The first dimension allows the
 * table to both store and overwrite grids safely.  The second indicates
 * whether this value is that for x or for y.  The third is the number
 * of grids able to be stored at any flow distance.
 */

static void update_flow_full(byte which_flow, u32b elem_flag)
{
	int i, d, k;
	byte y, x, y2, x2;
	int last_index;
	int grid_count = 0;


	/* Note where we get information from, and where we overwrite */
	int this_cycle = 0;
	int next_cycle = 1;

	byte flow_table[2][2][10 * FLOW_LENGTH_FULL];

	/*
	 * Set the initial cost to 10000 updates will progressively
	 * lower this value.  When it reaches zero, another full
	 * rebuild has to be done.
	 */
	cost_at_center[which_flow] = BASE_FLOW_CENTER;

	/*Point to the right function for the flows*/
	if (which_flow == FLOW_NO_DOORS) get_energy_to_move = get_energy_no_doors;
	else if (which_flow == FLOW_PASS_DOORS) get_energy_to_move = get_energy_pass_doors;
	else if (which_flow == FLOW_FLYING) get_energy_to_move = get_energy_flying;
	else if (which_flow == FLOW_FLYING_NO_DOORS) get_energy_to_move = get_energy_flying_no_doors;
	else if (which_flow == FLOW_PASS_WALLS) get_energy_to_move = get_energy_pass_walls;
	/* All the native elements*/
	else if (which_flow >= ELEM_FLOW_BASE_NO_DOORS) get_energy_to_move = get_energy_elemental_flow_no_doors;
	else get_energy_to_move = get_energy_elemental_flow;


	/*** Create the flow ***/

	/* Store base cost at the character location */
	cave_cost[which_flow][p_ptr->py][p_ptr->px] = BASE_FLOW_CENTER;

	/* Store this first grid in the flow table, note that we've done so */
	flow_table[this_cycle][0][0] = p_ptr->py;
	flow_table[this_cycle][1][0] = p_ptr->px;
	grid_count = 1;

	/* Extend the flow information out to its limits */
	for (k = 0; k < FLOW_LENGTH_FULL; k++)
	{
		/* Get the number of grids we'll be looking at */
		last_index = grid_count;

		/* Stop if we've run out of work to do */
		if (last_index == 0) break;

		/* Clear the grid count */
		grid_count = 0;

		/* Get each valid entry in the flow table in turn. */
		for (i = 0; i < last_index; i++)
		{
			/* Get this grid */
			y = flow_table[this_cycle][0][i];
			x = flow_table[this_cycle][1][i];

			/* Look at all adjacent grids */
			for (d = 0; d < 8; d++)
			{
				int new_cave_cost;
				int energy_to_move;

				/* Child location */
				y2 = y + ddy_ddd[d];
				x2 = x + ddx_ddd[d];

				/* Check Bounds */
				if (!in_bounds(y2, x2)) continue;

				/*Use a functional pointer for speed to get the proper energy*/
				energy_to_move = (*get_energy_to_move)(y2, x2, which_flow, elem_flag);

				/*Not a usable square*/
				if (energy_to_move == 0) continue;

				/*
				 * Calculate the energy required to move to this square
				 * from the previous square.
				 */
				new_cave_cost = cave_cost[which_flow][y][x] + energy_to_move;

				/*This is not a quicker route*/
				if ((cave_cost[which_flow][y2][x2] > 0) &&
						 (cave_cost[which_flow][y2][x2] <= new_cave_cost)) continue;

				/*Don't store the same grid twice*/
				if (cave_info[y2][x2] & (CAVE_FLOW)) continue;

				/*mark that we stored this grid for the next cycle*/
				cave_info[y2][x2] |= (CAVE_FLOW);

				/* Store cost at this location */
				cave_cost[which_flow][y2][x2] = new_cave_cost;

				/* Store this grid in the flow table */
				flow_table[next_cycle][0][grid_count] = y2;
				flow_table[next_cycle][1][grid_count] = x2;

				/* Increment number of grids stored */
				grid_count++;
			}
		}

		/*Clear the CAVE_FLOW marks*/
		for (i = 0; i < grid_count; i++)
		{
			/* Not very legible, but this is using the y and x coordinates from the nest cycle*/
			cave_info[flow_table[next_cycle][0][i]][flow_table[next_cycle][1][i]] &= ~(CAVE_FLOW);
		}

		/* Swap write and read portions of the table */
		if (this_cycle == 0)
		{
			this_cycle = 1;
			next_cycle = 0;
		}
		else
		{
			this_cycle = 0;
			next_cycle = 1;
		}
	}
}


/*
 * Clear the flow information and prepare to build completely new flow information
 */
static void update_flows_full_prep(void)
{
	int i;

	/* Clear the cost at centers */
	/* Clear the cost at centers and all flow (noise) information */
 	for (i = FLOW_NO_DOORS; i < MAX_FLOWS; i++)
 	{
		if (cost_at_center[i] > 0)
		{
			WIPE(&cave_cost[i][0][0], u16b_dungeon);
		}

 		cost_at_center[i] = 0;
 	}

	/* Save the new noise epicenter */
	p_ptr->flow_center_y = p_ptr->py;
	p_ptr->flow_center_x = p_ptr->px;
	p_ptr->update_center_y = p_ptr->py;
	p_ptr->update_center_x = p_ptr->px;

	/*Clear the flows*/

	p_ptr->update &= ~(TERRAIN_MASK);

	/*
	 * All monsters need to re-consider their targets and flow.
	 * Townsman AI or fleeing monsters are the exception
	 */

	/* Process the monsters */
	for (i = 1; i < mon_max; i++)
	{
		/* Get the monster */
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;

		/*re-evaluate the flow the monster is using*/
		m_ptr->using_flow = NEED_FLOW;

		/*Do we need the flying flow*/
		if (r_ptr->flags3 & RF3_FLYING)
		{
			/*If we are in range, we need this flow*/
			if (m_ptr->mflag & (MFLAG_ACTV)) p_ptr->update |= (PU_FLOW_FLYING);
		}

		/*Do we need the pass_walls flow*/
		if ((r_ptr->flags2 & RF2_KILL_WALL) ||
			(r_ptr->flags2 & RF2_PASS_WALL))
		{
			/*Are we in range?*/
			if (m_ptr->mflag & (MFLAG_ACTV)) p_ptr->update |= (PU_FLOW_PASS_WALLS);
		}

		 /* The monster cannot handle doors so we must use the doorless versions of the flows */
		if (MONSTER_HATES_DOORS(r_ptr))
		{
			/*Are we in range?*/
			if (m_ptr->mflag & (MFLAG_ACTV)) p_ptr->update |= (PU_FLOW_NO_DOORS_SPECIAL);
		}

		/*
		 * Always a target y for each target x.
		 * Don't clear the townman AI or fleeing monsters
		 */
		if ((m_ptr->target_x) && (!(m_ptr->mflag & (MFLAG_TOWN))) &&
			(m_ptr->min_range != FLEE_RANGE))
		{
			/*We need to re-evaluate target*/
			m_ptr->target_x = m_ptr->target_y = 0;
		}
	}
}


/*
 * Fill in the "cave_cost" field of every grid that the player can
 * reach with the number of steps needed to reach that grid.  This
 * also yields the route distance of the player from every grid.
 * Monsters use this information by moving to adjacent grids with
 * lower flow costs, thereby homing in on the player even though
 * twisty tunnels and mazes.  Monsters can also run away from loud
 * noises.
 *
 *
 * The flow table is three-dimensional.  The first dimension allows the
 * table to both store and overwrite grids safely.  The second indicates
 * whether this value is that for x or for y.  The third is the number
 * of grids able to be stored at any flow distance.
 */

void update_flows(bool full)
{
	/*First check if we need a full update*/
	if (!full)
	{
		if (p_ptr->update & (PU_FLOW_DOORS | PU_FLOW_NO_DOORS)) full = TRUE;

		/*We have done something to move more than two squares in one turn*/
		else if (distance(p_ptr->py, p_ptr->px,
				p_ptr->update_center_y, p_ptr->update_center_x) > 1) full = TRUE;

		/*we have moved too far since the last full update*/
		else if (distance(p_ptr->flow_center_y, p_ptr->flow_center_x,
				p_ptr->update_center_y, p_ptr->update_center_x) >
						    MAX_DISTANCE_BETWEEN_UPDATES) full = TRUE;
		/*We can't do any more partial updates*/
		else if (cost_at_center[FLOW_NO_DOORS] < 500) full = TRUE;

		/* Arena levels are too small to have partial updates */
		else if (p_ptr->dungeon_type == DUNGEON_TYPE_ARENA) full = TRUE;
	}

	if (!full)
	{
		u32b elemental_flows = ELEM_BASE;

		int i;

		/*clear all the old information*/
		update_flows_partial_prep();

		/*Partial Update the two main flows, and the two non-elemental flows if applicable*/
		update_flow_partial(FLOW_PASS_DOORS, 0L);
		update_flow_partial(FLOW_NO_DOORS, 0L);
		if (cost_at_center[FLOW_FLYING] > 0)		update_flow_partial(FLOW_FLYING, 0L);
		if (cost_at_center[FLOW_FLYING_NO_DOORS] > 0)   update_flow_partial(FLOW_FLYING_NO_DOORS, 0L);
		if (cost_at_center[FLOW_PASS_WALLS] > 0)	update_flow_partial(FLOW_PASS_WALLS, 0L);

		p_ptr->update &= ~(PU_FLOW_DOORS | PU_FLOW_NO_DOORS | PU_FLOW_FLYING | PU_FLOW_PASS_WALLS);

		/*Check all the optional flows*/
		for (i = ELEM_FLOW_BASE; i <= ELEM_FLOW_TAIL; i++)
		{
			int j;

			/*Update if necessary*/
			if (cost_at_center[i] > 0)
			{
				/*Update the flow*/
				update_flow_partial(i, elemental_flows);
			}

			/* Get the respective NO_DOORS flow */
			j = i - ELEM_FLOW_BASE + ELEM_FLOW_BASE_NO_DOORS;

			/* Update if necessary */
			if (cost_at_center[j] > 0)
			{
				/* Update the flow */
				update_flow_partial(j, elemental_flows);
			}

			/*Clear the flow marker*/
			p_ptr->update &= ~(elemental_flows);

			/*Shift to the next flow*/
			elemental_flows = (elemental_flows << 1);
		}
	}
	else
	{
		int flow;
		u32b element;

		/*clear all the old information*/
		update_flows_full_prep();

		/*Update the two main flows, and the flying or pass walls flow as needed*/
		update_flow_full(FLOW_PASS_DOORS, 0L);
   		update_flow_full(FLOW_NO_DOORS, 0L);
		if (p_ptr->update & (PU_FLOW_FLYING))
		{
			update_flow_full(FLOW_FLYING, 0L);

			/* Update the doorless version of the FLYING flow if necessary */
			if (p_ptr->update & (PU_FLOW_NO_DOORS_SPECIAL))
			{
				update_flow_full(FLOW_FLYING_NO_DOORS, 0L);
			}
		}

		if (p_ptr->update & (PU_FLOW_PASS_WALLS)) update_flow_full(FLOW_PASS_WALLS, 0L);
		p_ptr->update &= ~(PU_FLOW_DOORS | PU_FLOW_NO_DOORS | PU_FLOW_FLYING | PU_FLOW_PASS_WALLS);

		element = ELEM_BASE;

		/* Update the elemental flows if necessary */
		for (flow = ELEM_FLOW_BASE; flow <= ELEM_FLOW_TAIL; flow++)
		{
			/* Only if we are allowed */
			if (p_ptr->update & element)
			{
				/* Clear the mark */
				p_ptr->update &= ~element;

				/* Do it */
				update_flow_full(flow, element);

				/* Update the doorless version of the flow if requested */
				if (p_ptr->update & (PU_FLOW_NO_DOORS_SPECIAL))
				{
					/* Do it */
					update_flow_full(flow - ELEM_FLOW_BASE + ELEM_FLOW_BASE_NO_DOORS, element);
				}
			}

			/* Get the next element flag */
			element <<= 1;
		}

		/* Clear any mark */
		p_ptr->update &= ~(PU_FLOW_NO_DOORS_SPECIAL);
	}
}

#ifdef MONSTER_SMELL

/*
 * Characters leave scent trails for perceptive monsters to track.  -LM-
 *
 * Smell is rather more limited than sound.  Many creatures cannot use
 * it at all, it doesn't extend very far outwards from the character's
 * current position, and monsters can use it to home in the character,
 * but not to run away from him.
 *
 * Smell is valued according to age.  When a character takes his turn,
 * scent is aged by one, and new scent of the current age is laid down.
 * Speedy characters leave more scent, true, but it also ages faster,
 * which makes it harder to hunt them down.
 *
 * Whenever the age count loops, most of the scent trail is erased and
 * the age of the remainder is recalculated.
 */
void update_smell(void)
{
	int i, j;
	int y, x;
	int py = p_ptr->py;
	int px = p_ptr->px;
	/* Create a table that controls the spread of scent */
	int scent_adjust[5][5] =
	{
		{ 250,  2,  2,  2, 250 },
		{   2,  1,  1,  1,   2 },
		{   2,  1,  0,  1,   2 },
		{   2,  1,  1,  1,   2 },
		{ 250,  2,  2,  2, 250 },
	};
	/* Scent becomes "younger" */
	scent_when--;

	/* Loop the age and adjust scent values when necessary */
	if (scent_when <= 0)
	{
		/* Scan the entire dungeon */
		for (y = 0; y < p_ptr->cur_map_hgt; y++)
		{
			for (x = 0; x < p_ptr->cur_map_wid; x++)
			{
				/* Ignore non-existent scent */
				if (cave_when[y][x] == 0) continue;

				/* Erase the earlier part of the previous cycle */
				if (cave_when[y][x] > SMELL_STRENGTH) cave_when[y][x] = 0;

				/* Reset the ages of the most recent scent */
				else cave_when[y][x] = 250 - SMELL_STRENGTH + cave_when[y][x];
			}
		}

		/* Reset the age value */
		scent_when = 250 - SMELL_STRENGTH;
	}
	/* Lay down new scent */
	for (i = 0; i < 5; i++)
	{
		for (j = 0; j < 5; j++)
		{
			feature_type *f_ptr;

			/* Translate table to map grids */
			y = i + py - 2;
			x = j + px - 2;

			/* Check Bounds */
			if (!in_bounds(y, x)) continue;

			/* Fast access to feature */
			f_ptr = &f_info[cave_feat[y][x]];

			/* Walls, water, and lava cannot hold scent. */
			if (!cave_passable_bold(y, x) ||
				_feat_ff2_match(f_ptr, FF2_SHALLOW | FF2_DEEP))
			{
				continue;
			}

			/* Grid must not be blocked by walls from the character */
			if (!los(p_ptr->py, p_ptr->px, y, x)) continue;

			/* Note grids that are too far away */
			if (scent_adjust[i][j] == 250) continue;

			/* Mark the grid with new scent */
			cave_when[y][x] = scent_when + scent_adjust[i][j];
		}
	}

}

#endif /*MONSTER_SMELL*/

/*
 * Light up the dungeon using "claravoyance"
 *
 * This function "illuminates" every grid in the dungeon, memorizes all
 * "objects", memorizes all grids as with magic mapping, and, under the
 * standard option settings (view_perma_grids but not view_torch_grids)
 * memorizes all floor grids too.
 *
 * Note that if "view_perma_grids" is not set, we do not memorize floor
 * grids, since this would defeat the purpose of "view_perma_grids", not
 * that anyone seems to play without this option.
 *
 * Note that if "view_torch_grids" is set, we do not memorize floor grids,
 * since this would prevent the use of "view_torch_grids" as a method to
 * keep track of what grids have been observed directly.
 */
void wiz_light(void)
{
	int i, y, x;

	/* Memorize objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Memorize */
		o_ptr->marked = TRUE;
	}

	/* Scan all normal grids */
	for (y = 0; y < p_ptr->cur_map_hgt-1; y++)
	{
		/* Scan all normal grids */
		for (x = 0; x < p_ptr->cur_map_wid-1; x++)
		{
			/* Process all non-walls, but don't count rubble */
			if (!(f_info[cave_feat[y][x]].f_flags1 & (FF1_WALL)))
			{
				/* Scan all neighbors */
				for (i = 0; i < 9; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Perma-lite the grid */
					cave_info[yy][xx] |= (CAVE_GLOW);

					/* Memorize normal features */
					if (f_info[cave_feat[yy][xx]].f_flags1 & (FF1_REMEMBER))
					{
						/* Memorize the grid */
						cave_info[yy][xx] |= (CAVE_MARK);
					}

					/* Normally, memorize floors (see above) */
					if (view_perma_grids && !view_torch_grids)
					{
						/* Memorize the grid */
						cave_info[yy][xx] |= (CAVE_MARK);
					}
				}
			}
		}
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST | PR_ITEMLIST);

}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(void)
{
	int i, y, x;


	/* Forget every grid */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Process the grid */
			cave_info[y][x] &= ~(CAVE_MARK | CAVE_DTRAP);
		}
	}

	/* Forget all objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Forget the object */
		o_ptr->marked = FALSE;
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP | PR_MAP | PR_MONLIST | PR_ITEMLIST);

}



/*
 * Light or Darken the town
 */
void town_illuminate(bool daytime)
{
	int y, x, i;

	/* Apply light or darkness */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			bool always_lit = FALSE;

			/* Obvious */
			if (cave_feat[y][x] != FEAT_COBBLESTONE_FLOOR) always_lit = TRUE;

			/* Glyphs and visible traps */
			else if (cave_any_trap_bold(y, x) &&
					!(x_list[cave_x_idx[y][x]].x_flags & (EF1_HIDDEN))) always_lit = TRUE;

			/* Interesting grids */
			if (always_lit)
			{
				/* Illuminate the grid */
				cave_info[y][x] |= (CAVE_GLOW);

				/* Memorize the grid */
				cave_info[y][x] |= (CAVE_MARK);
			}

			/* Boring grids (light) */
			else if (daytime)
			{
				/* Illuminate the grid */
				cave_info[y][x] |= (CAVE_GLOW);

				/* Hack -- Memorize grids */
				if (view_perma_grids)
				{
					cave_info[y][x] |= (CAVE_MARK);
				}
			}

			/* Boring grids (dark) */
			else
			{
				/* Darken the grid */
				cave_info[y][x] &= ~(CAVE_GLOW);

				/* Hack -- Forget grids */
				if (view_perma_grids)
				{
					cave_info[y][x] &= ~(CAVE_MARK);
				}
			}
		}
	}

	/* Handle shop doorways */
	for (y = 0; y < p_ptr->cur_map_hgt; y++)
	{
		for (x = 0; x < p_ptr->cur_map_wid; x++)
		{
			/* Track shop doorways */
			if (cave_shop_bold(y,x))
			{
				for (i = 0; i < 8; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Illuminate the grid */
					cave_info[yy][xx] |= (CAVE_GLOW);

					/* Hack -- Memorize grids */
					if (view_perma_grids)
					{
						cave_info[yy][xx] |= (CAVE_MARK);
					}
				}
			}
		}
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->redraw |= (PR_MONLIST | PR_MAP | PR_ITEMLIST);
}


/*
 * Set or unset the CAVE_LOS, CAVE_PROJECT and CAVE_MOVE flags of the given
 * grid depending on the terrain feature and effects contained in such grid.
 * This function is very IMPORTANT. It must be called whenever the content of
 * a grid changes (cave_set_feat_aux), we add/remove effects or we load a
 * savefile.
 *
 * CAVE_LOS controls the CAVE_VIEW and CAVE_SEEN flags.
 * CAVE_PROJECT controls the CAVE_FIRE flag.
 * CAVE_MOVE controls monster/player movement and noise flows.
 *
 * Note on CAVE_MOVE. This flag is actually equal to FF1_MOVE plus effects.
 * Some places in the sources still need to check the presence of the FF1_MOVE
 * flag sans effects. You'll see checks for CAVE_MOVE or FF1_MOVE depending
 * on the context.
 */
void update_los_proj_move(int y, int x)
{
	feature_type *f_ptr = &f_info[cave_feat[y][x]];
	u16b x_idx;
	u32b mask = 0;
	u32b old_flags;

	/* Save the old flags */
	old_flags = cave_info[y][x];

	/* Paranoia */
	old_flags &= (CAVE_LOS | CAVE_PROJECT | CAVE_MOVE);

	/* Turn off those flags by default */
	cave_info[y][x] &= ~(CAVE_LOS | CAVE_PROJECT | CAVE_MOVE);

	/* Feature allows LOS */
	if (_feat_ff1_match(f_ptr, FF1_LOS)) mask |= (CAVE_LOS);

	/* Feature allows projections */
	if (_feat_ff1_match(f_ptr, FF1_PROJECT)) mask |= (CAVE_PROJECT);

	/* Feature allows movement */
	if (_feat_ff1_match(f_ptr, FF1_MOVE)) mask |= (CAVE_MOVE);

	/* Check effects if necessary */
	x_idx = cave_x_idx[y][x];

	/* Traverse the effects applied to that grid */
	while (x_idx)
	{
		/* Get the effect */
		effect_type *x_ptr = &x_list[x_idx];

		/* Get the next effect (for the next iteration) */
		x_idx = x_ptr->next_x_idx;

		/* Ignore certain effects */
		if (!(x_ptr->x_f_idx) || (x_ptr->x_flags & (EF1_HIDDEN))) continue;

		/* Get the pseudo-feature associated to the effect */
		f_ptr = &f_info[x_ptr->x_f_idx];

		/* Disable los if necessary */
		if (!_feat_ff1_match(f_ptr, FF1_LOS)) mask &= ~(CAVE_LOS);

		/* Disable projections if necessary */
		if (!_feat_ff1_match(f_ptr, FF1_PROJECT)) mask &= ~(CAVE_PROJECT);

		/* Disable movement if necessary */
		if (!_feat_ff1_match(f_ptr, FF1_MOVE)) mask &= ~(CAVE_MOVE);
	}

	/* Turn on the collected flags */
	cave_info[y][x] |= mask;

	/* Request view update if necessary */
	if ((old_flags & (CAVE_LOS | CAVE_PROJECT)) !=
		(mask & (CAVE_LOS | CAVE_PROJECT)))
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Request flow update if necessary */
	if ((old_flags & (CAVE_MOVE)) != (mask & (CAVE_MOVE)))
	{
		/* Full update of the flows */
		p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
	}
}


/*
 * Hack -- Really change the feature
 */
static void cave_set_feat_aux(int y, int x, u16b feat)
{
	/* Old feature */
	feature_type *f_ptr = &f_info[cave_feat[y][x]];

	/* New feature */
	feature_type *f2_ptr = &f_info[feat];

	/* We aren't changing anything */
	if (f2_ptr == f_ptr) return;

	/* Check hidden monsters exposed by change */
	if (cave_m_idx[y][x] > 0)
	{
		/* Get the monster */
		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

		/* Get the race */
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Check if the monster is currently hidden and if the new terrain can't hide it anymore */
		if ((m_ptr->mflag & (MFLAG_HIDE)) &&
			!(feat_ff2_match(feat, FF2_COVERED) &&
			       	is_monster_native_aux(feat, r_ptr->r_native)))
		{
			/* Show the monster */
			monster_unhide(m_ptr);
		}
	}

	/* Really set the feature */
	cave_feat[y][x] = feat;

	/* Remove inscriptions */
	if (cave_x_idx[y][x] > 0)
	{
		/* Get the index */
		s16b x_idx = cave_x_idx[y][x];

		/* Scan all the effects on that grid */
		while (x_idx)
		{
			/* Get the effect */
			effect_type *x_ptr = &x_list[x_idx];

			/* Prepare the next index */
			x_idx = x_ptr->next_x_idx;

			/* Check inscriptions */
			if (x_ptr->x_type == EFFECT_INSCRIPTION)
			{
				/* Remove it */
				delete_effect_idx((int)(x_ptr - x_list));
			}
		}
	}

	/* Update CAVE_LOS, CAVE_PROJECT and CAVE_MOVE flags */
	update_los_proj_move(y, x);

	/* Update noise information if certain things change */
	/* EXPERIMENTAL -DG- */
	if ((f2_ptr->dam_non_native != f_ptr->dam_non_native) ||
		(f2_ptr->native_energy_move != f_ptr->native_energy_move) ||
		(f2_ptr->non_native_energy_move !=
		 	f_ptr->non_native_energy_move) ||
		(_feat_ff3_match(f2_ptr, TERRAIN_MASK) !=
		 	_feat_ff3_match(f_ptr, TERRAIN_MASK)))
	{
		/* Full update of the flows */
		p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
	}

	/* Remove the old feature from dyna_g if necessary */
	if (_feat_ff3_match(f_ptr, FF3_DYNAMIC))
	{
		remove_dynamic_terrain(y, x);
	}

	/* Add the new feature to dyna_g if necessary */
	if (_feat_ff3_match(f2_ptr, FF3_DYNAMIC))
	{
		(void)add_dynamic_terrain(y, x);
	}

	/* Update the flags of the current level */
	if (_feat_ff3_match(f2_ptr, TERRAIN_MASK))
	{
		/* Get the LF1_* flag */
		u32b flag = get_level_flag(feat);

		/* Debug message */
		if (cheat_room && !(level_flag & flag))
		{
			char name[80];

			feature_desc(name, sizeof(name), feat, TRUE, TRUE);

			msg_c_format(MSG_NOTICE, "Adding %s to the level.", name);
		}

		/* Update the level flags */
		level_flag |= flag;
	}

	/* Check if we have to reactivate some adjacent dynamic grid */
	/* We only do this when we have a generated dungeon (efficiency) */
	if (character_dungeon)
	{
		int d;

		/* Check adjacent grids */
		for (d = 0; d < 8; d++)
		{
			/* Get coordinates */
			int yy = y + ddy_ddd[d];
			int xx = x + ddx_ddd[d];

			/* Ignore annoying locations */
			if (!in_bounds(yy, xx)) continue;

			/* Ignore non-dynamic grids */
			if (!cave_ff3_match(yy, xx, FF3_DYNAMIC)) continue;

			/* The dynamic grid already is in dyna_g. Ignore */
			if (get_dynamic_terrain(yy, xx)) continue;

			/* Try to put the grid in dyna_g */
			(void)add_dynamic_terrain(yy, xx);
		}
	}

	/* Check if location changed under player */
	if ((p_ptr->py == y) && (p_ptr->px == x))
	{
		/*Track the new feature*/
		feature_kind_track(cave_feat[y][x]);

		/* Window stuff */
		p_ptr->redraw |= (PR_FEATURE);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
	}

	 /* Don't allow effects where players can't move. */
	if (!_feat_ff1_match(f2_ptr, FF1_MOVE) && cave_x_idx[y][x]) delete_effects(y, x);

	/* This is a generated dungeon*/
	if (character_dungeon)
	{
		/* Hack -- Forget most of the new features */
		/*
		if (!_feat_ff1_match(f2_ptr, FF1_DOOR))
		{
			cave_info[y][x] &= ~(CAVE_MARK);
			}
		*/

		/* Notice */
		note_spot(y, x);

		/* Redraw */
		light_spot(y, x);
	}
}


/*
 * Takes a location and action and changes the feature at that
 * location through applying the given action.
 */
void cave_alter_feat(int y, int x, int action)
{
	int newfeat;

	/* Set old feature */
	int oldfeat = cave_feat[y][x];

	/*Mark the feature lore*/
	feature_type *f_ptr = &f_info[oldfeat];

	/*Mark the feature lore*/
	feature_lore *f_l_ptr = &f_l_list[oldfeat];

	/* Get the new feat */
	newfeat = feat_state(cave_feat[y][x], action);

	/* Mark the transition in the feature_lore */
	if ((player_can_see_bold(y, x)) && player_can_observe())
	{
		int i;

		/* Check an specific entry for the transition first */
 		for (i = 0; i < MAX_FEAT_STATES; i++)
 		{
 			/* Found the action */
			if (f_ptr->state[i].fs_action == action)
 			{
 				/*Count the number of times this transition has been seen*/
				if (f_l_ptr->f_l_state[i] < MAX_UCHAR) f_l_ptr->f_l_state[i]++;

				/* Done */
				break;
			}
 		}

		/* Check the default if we can't find the transition */
		if ((i >= MAX_FEAT_STATES) && (f_ptr->defaults == newfeat))
		{
			/*Count the number of times this transition has been seen*/
			if (f_l_ptr->f_l_defaults < MAX_UCHAR) f_l_ptr->f_l_defaults++;
 		}

	}


	/* Invisible trap */
	if (newfeat == oldfeat)
	{
		if (feat_ff3_match(oldfeat, FF3_PICK_TRAP))
		{
			/*Mark the lore*/
			if (player_can_see_bold(y, x)) f_l_ptr->f_l_flags3 |= (FF3_PICK_TRAP);

			/* Pick a trap */
			pick_and_set_trap(y, x, 0);

			/* Disturb */
			disturb(0, 0);
		}
		else if (feat_ff3_match(oldfeat, FF3_PICK_DOOR))
		{

			/*Mark the lore*/
			if (player_can_see_bold(y, x)) f_l_ptr->f_l_flags3 |= (FF3_PICK_DOOR);

			/* Pick a trap */
			place_closed_door(y, x);

			/* Disturb */
			disturb(0, 0);
		}
		else
		{
			return;
		}

		/* This is a generated dungeon*/
		if (character_dungeon)
		{
			/* Notice */
			note_spot(y, x);

			/* Redraw */
			light_spot(y, x);
		}
	}

	/* Other stuff */
	else
	{
		/* Set the new feature */
		cave_set_feat(y, x, newfeat);
	}
}


/*
 * Change the "feat" flag for a grid, and notice/redraw the grid
 */
void cave_set_feat(int y, int x, u16b feat)
{
	int i, j;
	u16b feat2;

	/* Old Feature */
	u16b old_feat = cave_feat[y][x];
	feature_type *f_ptr = &f_info[old_feat];

	/* New Feature */
	feature_type *f2_ptr = &f_info[feat];

	/* We aren't changing anything */
	if (f2_ptr == f_ptr) return;

	/* Take damage if this is the player square*/
	if (character_dungeon)
	{
		if ((p_ptr->py == y) && (p_ptr->px == x))
		{

			/* Take any accumulated damage from terrain */
			process_player_terrain_damage();
		}
	}

	/* Change the feature */
	cave_set_feat_aux(y, x, feat);

	/* The new feature is radiating its own light */
	if (_feat_ff2_match(f2_ptr, FF2_GLOW))
	{
		/* Turn on super glow */
		cave_info[y][x] |= (CAVE_HALO);

		/* Spread the super glow through adjacent grids */
		for (i = 0; i < 8; i++)
		{
			/* Get coordinates */
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Turn on super glow */
			cave_info[yy][xx] |= (CAVE_HALO);
		}

		/* Fully update the visuals */
		if (player_has_los_bold(y, x))
		{
			p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW |	PU_MONSTERS);
		}
	}

	/* The grid has lost its own light */
	else if (_feat_ff2_match(f_ptr, FF2_GLOW))
	{
		/* Turn off super glow */
		cave_info[y][x] &= ~(CAVE_HALO);

		/* Check adjacent grids */
		for (i = 0; i < 8; i++)
		{
			/* Get coordinates */
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* An adjacent grid is glowing */
			if (cave_ff2_match(yy, xx, FF2_GLOW))
			{
				/* Turn on super glow again */
				cave_info[y][x] |= (CAVE_HALO);

				/* Jump to the next adjacent grid */
				continue;
			}

			/* Regular grid. Turn off super glow (temporarily) */
			cave_info[yy][xx] &= ~(CAVE_HALO);

			/* Check adjacent grids */
			for (j = 0; j < 8; j++)
			{
				/* Get coordinates */
				int yyy = yy + ddy_ddd[j];
				int xxx = xx + ddx_ddd[j];

				/* Ignore annoying locations */
				if (!in_bounds_fully(yyy, xxx)) continue;

				/* Found glowing grid */
				if (cave_ff2_match(yyy, xxx, FF2_GLOW))
				{
					/* Turn on super glow again */
					cave_info[yy][xx] |= (CAVE_HALO);

					/* Done */
					break;
				}
			}

			/* We have to darken the grid, for real */
			if (!(cave_info[yy][xx] & (CAVE_HALO | CAVE_GLOW)) &&
				!cave_ff1_match(yy, xx, FF1_REMEMBER))
			{
				/* Forget */
				cave_info[yy][xx] &= ~(CAVE_MARK);
			}
		}

		/* We have to darken the grid, for real */
		if (!(cave_info[y][x] & (CAVE_HALO | CAVE_GLOW)) &&
			!cave_ff1_match(y, x, FF1_REMEMBER))
		{
			/* Forget */
			cave_info[y][x] &= ~(CAVE_MARK);
		}

		/* Fully update the visuals */
		if (player_has_los_bold(y, x))
		{
			p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW |	PU_MONSTERS);
		}
	}

	/* Create a tree */
	if (_feat_ff3_match(f2_ptr, FF3_TREE))
	{
		/* Hack -- Reduce number of calls to rand_int */
		int k = rand_int(1 << 17);

		/* Hack -- Some trees have more branches */
		bool dense_tree = one_in_(3);

		/* Create branches on adjacent grids */
		for (i = 0; i < 8; i++)
		{
			/* Get coordinates */
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/*
			 * We have a 50% (or 25% for dense trees) chance to
			 * ignore the grid
			 */
			if ((k & (1 << i)) && (!dense_tree ||
				(k & (1 << (i + 8))))) continue;

			/* Convert to branch */
			feat2 = feat_state(cave_feat[yy][xx], FS_TREE);

			/* Set the new feature */
			cave_set_feat_aux(yy, xx, feat2);

			/* Hack - lite the branch if the trunk is lit */
			if (cave_info[y][x] & (CAVE_GLOW))
			{
				cave_info[yy][xx] |= (CAVE_GLOW);
			}
		}
	}
	/* Handle NEED_TREE locations */
	else if (_feat_ff3_match(f_ptr, FF3_TREE))
	{
		/*
		 * We have destroyed the trunk, what do we have to do
		 * with the branches?
		 */
		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Get the feature */
			feat2 = cave_feat[yy][xx];

			/* Ignore non-branches */
			if (!feat_ff3_match(feat2, FF3_NEED_TREE)) continue;

			/* Look for adjacent trees */
			for (j = 0; j < 8; j++)
			{
				int yyy = yy + ddy_ddd[j];
				int xxx = xx + ddx_ddd[j];

				/* Ignore annoying locations */
				if (!in_bounds_fully(yyy, xxx)) continue;

				/* Found a tree */
				if (cave_ff3_match(yyy, xxx, FF3_TREE)) break;
			}

			/*
			 * Remove the branch if we couldn't find a tree.
			 * We also have a slight chance to remove the branch
			 * if we found one.
			 */
			if ((j >= 8) || one_in_(4))
			{
				/* Remove the branch */
				feat2 = feat_state(feat2, FS_NEED_TREE);

				/* Set the new feature */
				cave_set_feat_aux(yy, xx, feat2);
			}
		}
	}

	/* Handle gold */
	if (character_dungeon && _feat_ff1_match(f_ptr, FF1_HAS_GOLD) &&
		!_feat_ff1_match(f2_ptr, FF1_HAS_GOLD))
	{
		/* Mark the lore if seen*/
		if (player_can_see_bold(y,x))
		{
			/*Mark the feature lore*/
			feature_lore *f_l_ptr = &f_l_list[old_feat];
			f_l_ptr->f_l_flags1 |= (FF1_HAS_GOLD);
		}

		place_gold(y, x);
	}

	/* Handle items */
	else if (character_dungeon && _feat_ff1_match(f_ptr, FF1_HAS_ITEM) &&
		!_feat_ff1_match(f2_ptr, FF1_HAS_ITEM))
	{
		/* Mark the lore if seen*/
		if (player_can_see_bold(y,x))
		{
			/*Mark the feature lore*/
			feature_lore *f_l_ptr = &f_l_list[old_feat];
			f_l_ptr->f_l_flags1 |= (FF1_HAS_ITEM);
		}

		place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
	}

}


/*
 * Determine the path taken by a projection.  -BEN-, -LM-
 *
 * The projection will always start one grid from the grid (y1,x1), and will
 * travel towards the grid (y2,x2), touching one grid per unit of distance
 * along the major axis, and stopping when it satisfies certain conditions
 * or has traveled the maximum legal distance of "range".  Projections
 * cannot extend further than MAX_SIGHT (at least at present).
 *
 * A projection only considers those grids which contain the line(s) of fire
 * from the start to the end point.  Along any step of the projection path,
 * either one or two grids may be valid options for the next step.  When a
 * projection has a choice of grids, it chooses that which offers the least
 * resistance.  Given a choice of clear grids, projections prefer to move
 * orthogonally.
 *
 * Also, projections to or from the character must stay within the pre-
 * calculated field of fire ("cave_info & (CAVE_FIRE)").  This is a hack.
 * XXX XXX
 *
 * The path grids are saved into the grid array "gp".  Note that
 * due to the way in which distance is calculated, this function normally
 * uses fewer than "range" grids for the projection path, so the result
 * of this function should never be compared directly to "range".  Note
 * that the initial grid (y1,x1) is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * We modify y2 and x2 if they are too far away, or (for PROJECT_PASS only)
 * if the projection threatens to leave the dungeon.
 *
 * The "flg" flags can be used to modify the behavior of this function:
 *    PROJECT_STOP:  projection stops when it cannot bypass a monster.
 *    PROJECT_CHCK:  projection notes when it cannot bypass a monster.
 *    PROJECT_THRU:  projection extends past destination grid
 *    PROJECT_PASS:  projection passes through walls
 *
 * This function returns the number of grids (if any) in the path.  This
 * may be zero if no grids are legal except for the starting one.
 */
int project_path(u16b *path_g, u16b *path_gx, int range,
		int y1, int x1, int *y2, int *x2, u32b flg)
{
	int i, j;
	int dy, dx;
	int num, dist, octant;
	int grids = 0;
	bool line_fire;
	bool full_stop = FALSE;

	int y_a, x_a, y_b, x_b;
	int y = 0;
	int x = 0;

	int path_n;

	/* Start with all lines of sight unobstructed */
	u32b bits0 = VINFO_BITS_0;
	u32b bits1 = VINFO_BITS_1;
	u32b bits2 = VINFO_BITS_2;
	u32b bits3 = VINFO_BITS_3;

	int slope_fire1 = -1, slope_fire2 = -1;

	/* Projections are either vertical or horizontal */
	bool vertical;

	/* Assume our target is in-bounds (we can use ordinary grid math) */
	bool y2_neg = FALSE, y2_large = FALSE;
	bool x2_neg = FALSE, x2_large = FALSE;

	/* Optionally require grids to be strictly in line of fire */
	bool require_strict_lof = FALSE;

	/* Count of grids in LOF */
	u16b tmp_grids[160];

	/* Line of fire slope(s) to each legal grid */
	byte tmp_slope_fire1[160];
	byte tmp_slope_fire2[160];

	/* Count of grids in projection path */
	int step;

	/* Remember whether and how a grid is blocked */
	int blockage[2];
	int blockage_tmp;

	/* Assume no monsters in way */
	bool monster_in_way = FALSE;

	/* Initial grid */
	u16b g0 = GRID(y1, x1);

	u16b g;

	/* Pointer to vinfo data */
	vinfo_type *p;

	/* Assume no path */
	path_n = 0;

	/* Handle projections of zero length */
	if ((range <= 0) || ((*y2 == y1) && (*x2 == x1))) return (0);

	/* The character is the source or target of the projection */
	if ((( y1 == p_ptr->py) && ( x1 == p_ptr->px)) ||
	    ((*y2 == p_ptr->py) && (*x2 == p_ptr->px)))
	{
		/* Require strict LOF */
		require_strict_lof = TRUE;
	}

	/* Get position change (signed) */
	dy = *y2 - y1;
	dx = *x2 - x1;

	/* Get distance from start to finish */
	dist = distance(y1, x1, *y2, *x2);

	/* Rescale Large Distances */
	if (dist > MAX_SIGHT)
	{
		/* Always watch your (+/-) when doing rounded integer math. */
		int round_y = (dy < 0 ? -(dist / 2) : (dist / 2));
		int round_x = (dx < 0 ? -(dist / 2) : (dist / 2));

		/* Rescale the endpoints */
		dy = ((dy * (MAX_SIGHT - 1)) + round_y) / dist;
		dx = ((dx * (MAX_SIGHT - 1)) + round_x) / dist;
		*y2 = y1 + dy;
		*x2 = x1 + dx;
	}

	/* Get the correct octant */
	if (dy < 0)
	{
		/* Up and to the left */
		if (dx < 0)
		{
			/* More upwards than to the left - octant 4 */
			if (ABS(dy) > ABS(dx)) octant = 5;

			/* At least as much left as upwards - octant 3 */
			else                   octant = 4;
		}
		else
		{
			if (ABS(dy) > ABS(dx)) octant = 6;
			else                   octant = 7;
		}
	}
	else
	{
		if (dx < 0)
		{
			if (ABS(dy) > ABS(dx)) octant = 2;
			else                   octant = 3;
		}
		else
		{
			if (ABS(dy) > ABS(dx)) octant = 1;
			else                   octant = 0;
		}
	}

	/* Determine whether the major axis is vertical or horizontal */
	if ((octant == 5) || (octant == 6) || (octant == 2) || (octant == 1))
	{
		vertical = TRUE;
	}
	else
	{
		vertical = FALSE;
	}

	/* Is our target out-of-bounds (y or x < 0 or > 255)? */
	if      (*y2 <   0) y2_neg   = TRUE;
	else if (*y2 > 255) y2_large = TRUE;

	if      (*x2 <   0) x2_neg   = TRUE;
	else if (*x2 > 255) x2_large = TRUE;


	/* Scan the octant, find the grid corresponding to the end point */
	for (j = 1; j < VINFO_MAX_GRIDS; j++)
	{
		int vy, vx;

		/* Point to this vinfo record */
		p = &vinfo[j];

		/* Extract grid value */
		g = g0 + p->grid[octant];

		/* Get axis coordinates */
		vy = GRID_Y(g);
		vx = GRID_X(g);

		/* Translate out-of-bounds y-values */
		if ((y2_neg) && (vy >= 128)) vy -= 256;
		else if ((y2_large) && (vy < 128)) vy += 256;

		/* Translate out-of-bounds x-values */
		if ((x2_neg) && (vx >= 128))
		{
			vx -= 256;
			vy++;
		}
		else if ((x2_large) && (vx < 128))
		{
			vx += 256;
			vy--;
		}

		/* Require that grid be correct */
		if ((vy != *y2) || (vx != *x2)) continue;

		/* Store slopes of fire */
		slope_fire1 = p->slope_fire_index1;
		slope_fire2 = p->slope_fire_index2;

		break;
	}

	/* Note failure XXX XXX */
	if (slope_fire1 == -1) return (0);

	/* Scan the octant, collect all grids having the correct line of fire */
	for (j = 1; j < VINFO_MAX_GRIDS; j++)
	{
		line_fire = FALSE;

		/* Point to this vinfo record */
		p = &vinfo[j];

		/* See if any lines of sight pass through this grid */
		if (!((bits0 & (p->bits_0)) ||
			  (bits1 & (p->bits_1)) ||
			  (bits2 & (p->bits_2)) ||
			  (bits3 & (p->bits_3))))
		{
			continue;
		}

		/*
		 * Extract grid value.  Use pointer shifting to get the
		 * correct grid offset for this octant.
		 */
		g = g0 + *((u16b*)(((byte*)(p)) + (octant * 2)));

		y = GRID_Y(g);
		x = GRID_X(g);

		/* Must be legal (this is important) */
		if (!in_bounds_fully(y, x)) continue;

		/* Check for first possible slope of fire */
		i = slope_fire1;

		/* Check slope(s) of fire */
		while (TRUE)
		{
			switch (i / 32)
			{
				case 3:
				{
					if (bits3 & (1L << (i % 32)))
					{
						if (p->bits_3 & (1L << (i % 32))) line_fire = TRUE;
					}
					break;
				}
				case 2:
				{
					if (bits2 & (1L << (i % 32)))
					{
						if (p->bits_2 & (1L << (i % 32))) line_fire = TRUE;
					}
					break;
				}
				case 1:
				{
					if (bits1 & (1L << (i % 32)))
					{
						if (p->bits_1 & (1L << (i % 32))) line_fire = TRUE;
					}
					break;
				}
				case 0:
				{
					if (bits0 & (1L << (i % 32)))
					{
						if (p->bits_0 & (1L << (i % 32))) line_fire = TRUE;
					}
					break;
				}
			}

			/* We're done if no second SOF exists, or when we've checked it */
			if (i == slope_fire2) break;

			/* Check second possible slope of fire */
			i = slope_fire2;
		}

		/* This grid contains at least one of the slopes of fire */
		if (line_fire)
		{
			/* Store grid value (always) */
			tmp_grids[grids] = g;

			/* Store the LOF slope(s) to this grid */
			tmp_slope_fire1[grids] = p->slope_fire_index1;
			tmp_slope_fire2[grids] = p->slope_fire_index2;

			/* Go to next grid */
			grids++;
		}

		/*
		 * Check for all terrain that won't support projection.  They can be in
		 * a projection path, but the path usually cannot pass through them.
		 */
		if (!(flg & (PROJECT_PASS)) && !cave_project_bold(y, x))
		{
			/* Clear any lines of sight passing through this grid */
			bits0 &= ~(p->bits_0);
			bits1 &= ~(p->bits_1);
			bits2 &= ~(p->bits_2);
			bits3 &= ~(p->bits_3);
		}
	}

	/* Scan the grids along the slope(s) of fire */
	for (step = 0, j = 0; j < grids;)
	{
		/* Get the coordinates of this grid */
		y_a = GRID_Y(tmp_grids[j]);
		x_a = GRID_X(tmp_grids[j]);

		/* Get the coordinates of the next grid, if legal */
		if (j < grids - 1)
		{
			y_b = GRID_Y(tmp_grids[j+1]);
			x_b = GRID_X(tmp_grids[j+1]);
		}
		else
		{
			y_b = -1;
			x_b = -1;
		}

		/*
		 * We always have at least one legal grid, and may have two.  Allow
		 * the second grid if its position differs only along the minor axis.
		 */
		if (vertical ? y_a == y_b : x_a == x_b) num = 2;
		else                                    num = 1;

		/* We choose the diagonal grid if it is closer to the true path */
		if (num == 2)
		{
			int d_slope_fire_a =
				MIN(ABS(slope_fire1 - tmp_slope_fire1[j]),
					ABS(slope_fire2 - tmp_slope_fire2[j]));

			int d_slope_fire_b =
				MIN(ABS(slope_fire1 - tmp_slope_fire1[j+1]),
					ABS(slope_fire2 - tmp_slope_fire2[j+1]));

			/* Swap the grids  XXX */
			if (d_slope_fire_b < d_slope_fire_a)
			{
				int y_c = y_b;
				int x_c = x_b;
				u16b tmp_grids_swap = tmp_grids[j];

				y_b = y_a;
				x_b = x_a;
				y_a = y_c;
				x_a = x_c;
				tmp_grids[j] = tmp_grids[j+1];
				tmp_grids[j+1] = tmp_grids_swap;
			}
		}

		/* Scan one or both grids */
		for (i = 0; i < num; i++)
		{
			/*Assume no blockage*/
			blockage[i] = PATH_G_FULL;

			/* Get the coordinates of this grid */
			y = (i == 0 ? y_a : y_b);
			x = (i == 0 ? x_a : x_b);

			/* Check projection range */
			if ((step > range - 2) &&
			    (distance(y1, x1, y, x) >= range))
			{
				/* End of projection */
				full_stop = TRUE;
			}

			/* Usually stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((y == *y2) && (x == *x2))
				{
					/* End of projection */
					full_stop = TRUE;
				}
			}

			/* Usually stop at wall grids */
			if (!(flg & (PROJECT_PASS)))
			{
				if (!cave_project_bold(y, x)) blockage[i] = PATH_G_WALL;
			}

			/* If we don't stop at wall grids, we explicitly check legality */
			else if (!in_bounds_fully(y, x))
			{
				/* End of projection */
				break;
			}

			/* Hidden monsters cannot stop projections */
			if ((cave_m_idx[y][x] > 0) && (mon_list[cave_m_idx[y][x]].mflag & (MFLAG_HIDE)))
			{
				/* Blank test */
			}

			/* Try to avoid occupied grids */
			else if ((cave_m_idx[y][x] != 0) && (blockage[i] < PATH_G_WALL))
			{
				if      (flg & (PROJECT_STOP)) blockage[i] = PATH_G_WALL;
				else if (flg & (PROJECT_CHCK)) blockage[i] = PATH_G_BLCK;
			}

			/* Usual case:  we are requiring strict LOF */
			if (require_strict_lof)
			{
				/* This grid does not qualify; it will be skipped */
				if (!(cave_info[y][x] & (CAVE_FIRE)))
				{
					blockage[i] += PATH_G_NONE;
				}
			}
		}/* Grids have been scanned */

		/* Prefer the 1st grid */
		if ((num == 1) || (blockage[0] <= blockage[1]))
		{
			path_gx[step] = blockage_tmp = blockage[0];
			path_g[step] = tmp_grids[j];
		}

		/* Accept second if necessary */
		else
		{
			path_gx[step] = blockage_tmp = blockage[1];
			path_g[step] = tmp_grids[j+1];
		}

		/* Get the grid coordinates */
		y = GRID_Y(path_g[step]);
		x = GRID_X(path_g[step]);

		/* Take a step */
		step++;

		/* Handle end of projection */
		if (full_stop) break;

		/* The projection ends at walls (usually) */
		if (blockage_tmp == PATH_G_WALL) break;

		/* Blockage of 1 means a creature bars the path */
		if (blockage_tmp == PATH_G_BLCK)
		{
			/* If not the start or endpoint, note the blockage */
			if ((y != *y2) || (x != *x2))

				monster_in_way = TRUE;
		}

		/* Grid should be skipped, but a creature blocks it */
		if ((blockage_tmp >= PATH_G_NONE) && (cave_m_idx[y][x] != 0))
		{
			/* Any sort of bolt projection stops immediately */
			if (!(flg & (PROJECT_BEAM | PROJECT_BOOM | PROJECT_CHCK)))
				break;

			/* If we don't stop, we note the potential blockage */
			else monster_in_way = TRUE;
		}

		/*
		 * Hack -- If we require orthogonal movement, but are moving
		 * diagonally, we have to plot an extra grid.  XXX XXX
		 */
		if ((flg & (PROJECT_ORTH)) && (step >= 2))
		{
			/* Get grids for this projection step and the last */
			y_a = GRID_Y(path_g[step-1]);
			x_a = GRID_X(path_g[step-1]);

			y_b = GRID_Y(path_g[step-2]);
			x_b = GRID_X(path_g[step-2]);

			/* The grids differ along both axis -- we moved diagonally */
			if ((y_a != y_b) && (x_a != x_b))
			{
				/* Get locations for the connecting grids */
				int y_c = y_a;
				int x_c = x_b;
				int y_d = y_b;
				int x_d = x_a;

				/* Back up one step */
				step--;

				/* Assume both grids are available */
				blockage[0] = 0;
				blockage[1] = 0;

				/* Hack -- Check legality */
				if (!in_bounds_fully(y_c, x_c)) blockage[0] = PATH_G_WALL;
				if (!in_bounds_fully(y_d, x_d)) blockage[1] = PATH_G_WALL;

				/* Usually stop at wall grids */
				if (!(flg & (PROJECT_PASS)))
				{
					if (!cave_project_bold(y_c, x_c)) blockage[0] = PATH_G_WALL;
					if (!cave_project_bold(y_d, x_d)) blockage[1] = PATH_G_WALL;
				}

				/* Try to avoid non-initial monsters/players */
				if (cave_m_idx[y_c][x_c] != 0)
				{
					if      (flg & (PROJECT_STOP)) blockage[0] = PATH_G_WALL;
					else if (flg & (PROJECT_CHCK)) blockage[0] = PATH_G_BLCK;
				}
				if (cave_m_idx[y_d][x_d] != 0)
				{
					if      (flg & (PROJECT_STOP)) blockage[1] = PATH_G_WALL;
					else if (flg & (PROJECT_CHCK)) blockage[1] = PATH_G_BLCK;
				}

				/* Both grids are blocked -- we have to stop now */
				if ((blockage[0] >= 2) && (blockage[1] >= PATH_G_WALL)) break;

				/* Accept the first grid if possible, the second if necessary */
				if (blockage[0] <= blockage[1]) path_g[step++] = GRID(y_c, x_c);
				else 							path_g[step++] = GRID(y_d, x_d);

				/* Re-insert the original grid, take an extra step */
				path_g[step++] = GRID(y_a, x_a);

				/* Increase range to accommodate this extra step */
				range++;
			}
		}

		/* Advance to the next unexamined LOF grid */
		j += num;
	}

	/* Accept last grid as the new endpoint */
	*y2 = GRID_Y(path_g[step - 1]);
	*x2 = GRID_X(path_g[step - 1]);

	/* Remember number of grids in projection path */
	path_n = step;

	/* Return number of grids in path, negative if blocked */
	if (monster_in_way) return (-path_n);
	else return (path_n);
}


/*
 * Determine if a spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination.
 *
 * This function is used to determine if the player can (easily) target
 * a given grid, if a monster can target the player, and if a clear shot
 * exists from monster to player.
 *
 * In cases where the character is either the source or the target of the
 * projection (which is most of the time), we now can save much time by
 * forbidding all projections that do not remain in line of fire.  -LM-
 *
 * Those projections that survive this check use the "project_path()"
 * function to determine the projection path and find any obstacles.
 * What qualifies as an obstacle depends on the projection flags.
 *
 * Note that no grid is ever "projectable()" from itself.
 */
byte projectable(int y1, int x1, int y2, int x2, u32b flg)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	int path_n;
	u16b path_g[PATH_SIZE];
	u16b path_gx[PATH_SIZE];

	int old_y2 = y2;
	int old_x2 = x2;

	/* We do not have permission to pass through walls */
	if (!(flg & (PROJECT_WALL | PROJECT_PASS)))
	{
		/* The character is the source of the projection */
		if ((y1 == py) && (x1 == px))
		{
			/* Require that destination be in line of fire */
			if (!(cave_info[y2][x2] & (CAVE_FIRE))) return (PROJECT_NO);
		}

		/* The character is the target of the projection */
		else if ((y2 == py) && (x2 == px))
		{
			/* Require that source be in line of fire */
			if (!(cave_info[y1][x1] & (CAVE_FIRE))) return (PROJECT_NO);
		}
	}

	/* Check the projection path */
	path_n = project_path(path_g, path_gx, MAX_RANGE, y1, x1, &y2, &x2, flg);

	/* No grid is ever projectable from itself */
	if (!path_n) return (PROJECT_NO);

	/* Final grid  */
	y = GRID_Y(path_g[path_n - 1]);
	x = GRID_X(path_g[path_n - 1]);

	/* May not end in an unrequested grid, unless PROJECT_THRU */
	if (!(flg & (PROJECT_THRU)))
	{
		if ((y != old_y2) || (x != old_x2)) return (PROJECT_NO);
	}

	/* Usually, cannot pass through walls */
	if (!(flg & (PROJECT_PASS)))
	{
		/* May not end in a non-projectable grid*/
		if (!cave_project_bold(y, x) && !cave_passable_bold(y, x)) return (PROJECT_NO);
	}

	/* Promise a clear bolt shot if we have verified that there is one */
	if ((flg & (PROJECT_STOP)) || (flg & (PROJECT_CHCK)))
	{
		/* Positive value for projectable mean no obstacle was found. */
		if (path_n > 0) return (PROJECT_CLEAR);
	}

	/* Assume projectable, but make no promises about clear shots */
	return (PROJECT_NOT_CLEAR);
}


/*
 * Standard "find me a location" function
 *
 * Obtains a legal location within the given distance of the initial
 * location, and with "los()" from the source to destination location.
 *
 * This function is often called from inside a loop which searches for
 * locations while increasing the "d" distance.
 *
 * Currently the "m" parameter is unused.
 */
void scatter(int *yp, int *xp, int y, int x, int d, int m)
{
	int nx, ny;


	/* Unused parameter */
	(void)m;

	/* Pick a location */
	while (TRUE)
	{
		/* Pick a new location */
		ny = rand_spread(y, d);
		nx = rand_spread(x, d);

		/* Ignore annoying locations */
		if (!in_bounds_fully(ny, nx)) continue;

		/* Ignore "excessively distant" locations */
		if ((d > 1) && (distance(y, x, ny, nx) > d)) continue;

		/* Require "line of sight" */
		if (los(y, x, ny, nx)) break;
	}

	/* Save the location */
	(*yp) = ny;
	(*xp) = nx;
}


/*
 * Track a new monster
 */
void health_track(int m_idx)
{
	/* Track a new guy */
	p_ptr->health_who = m_idx;

	/* Redraw (later) */
	p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);
}


/*
 * Hack -- track the given object kind
 */
void track_object(int item)
{
	p_ptr->object_idx = item;
	p_ptr->object_kind_idx = 0;
	p_ptr->redraw |= (PR_OBJECT);
}

void track_object_kind(int k_idx)
{
	p_ptr->object_idx = 0;
	p_ptr->object_kind_idx = k_idx;
	p_ptr->redraw |= (PR_OBJECT);
}


/*
 * Hack -- track the given monster race
 */
void monster_race_track(int r_idx)
{
	/* Save this monster ID */
	p_ptr->monster_race_idx = r_idx;

	/* Window stuff */
	p_ptr->redraw |= (PR_MONSTER);
}


/*
 * Hack -- track the given object kind
 */
void feature_kind_track(int f_idx)
{
	/* Save this object ID */
	p_ptr->feature_kind_idx = f_idx;

	/* Window stuff */
	p_ptr->redraw |= (PR_FEATURE);
}


/*
 * Something has happened to disturb the player.
 *
 * The first arg indicates a major disturbance, which affects search.
 *
 * The second arg is currently unused, but could induce output flush.
 *
 * All disturbance cancels repeated commands, resting, and running.
 */
void disturb(int stop_search, int unused_flag)
{
	/* Unused parameter */
	(void)unused_flag;

	/* Cancel auto-commands */
	/* p_ptr->command_new = 0; */

	/* Cancel repeated commands */
	if (p_ptr->command_rep)
	{
		/* Cancel */
		p_ptr->command_rep = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel Resting */
	if (p_ptr->resting)
	{
		/* Cancel */
		p_ptr->resting = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel running */
	if (p_ptr->running)
	{
		/* Cancel */
		p_ptr->running = 0;

 		/* Check for new panel if appropriate */
 		if (center_player) verify_panel();

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Cancel searching if requested */
	if (stop_search && p_ptr->searching)
	{
		/* Cancel */
		p_ptr->searching = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel noun-verb menu */
	p_ptr->noun_verb = FALSE;

	/* Flush the input if requested */
	if (flush_disturb) flush();
}

