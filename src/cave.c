/* File: cave.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-2 Andrew Doull. Modifications to the Angband 2.9.6
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"

/*
 * Support for Adam Bolt's tileset, lighting and transparency effects
 * by Robert Ruehlmann (rr9@angband.org)
 */

/*
 * Approximate distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
 */
sint distance(int y1, int x1, int y2, int x2)
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
bool los(int y1, int x1, int y2, int x2)
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
		/* South -- check for walls */
		if (dy > 0)
		{
			for (ty = y1 + 1; ty < y2; ty++)
			{
				if (!cave_floor_bold(ty, x1)) return (FALSE);
			}
		}

		/* North -- check for walls */
		else
		{
			for (ty = y1 - 1; ty > y2; ty--)
			{
				if (!cave_floor_bold(ty, x1)) return (FALSE);
			}
		}

		/* Assume los */
		return (TRUE);
	}

	/* Directly East/West */
	if (!dy)
	{
		/* East -- check for walls */
		if (dx > 0)
		{
			for (tx = x1 + 1; tx < x2; tx++)
			{
				if (!cave_floor_bold(y1, tx)) return (FALSE);
			}
		}

		/* West -- check for walls */
		else
		{
			for (tx = x1 - 1; tx > x2; tx--)
			{
				if (!cave_floor_bold(y1, tx)) return (FALSE);
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
			if (cave_floor_bold(y1 + sy, x1)) return (TRUE);
		}
	}

	/* Horizontal "knights" */
	else if (ay == 1)
	{
		if (ax == 2)
		{
			if (cave_floor_bold(y1, x1 + sx)) return (TRUE);
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
			if (!cave_floor_bold(ty, tx)) return (FALSE);

			qy += m;

			if (qy < f2)
			{
				tx += sx;
			}
			else if (qy > f2)
			{
				ty += sy;
				if (!cave_floor_bold(ty, tx)) return (FALSE);
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
			if (!cave_floor_bold(ty, tx)) return (FALSE);

			qx += m;

			if (qx < f2)
			{
				ty += sy;
			}
			else if (qx > f2)
			{
				tx += sx;
				if (!cave_floor_bold(ty, tx)) return (FALSE);
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
bool no_lite(void)
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
	s16b this_o_idx, next_o_idx = 0;


	/* Forbid perma-grids */
	if (cave_perma_bold(y, x)) return (FALSE);

	/* Check objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Forbid artifact grids */
		if (artifact_p(o_ptr)) return (FALSE);
	}

	/* Accept */
	return (TRUE);
}



/*
 * Hack -- Legal monster codes
 */
static const char image_monster_hack[] = \
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

/*
 * Hack -- Hallucinatory monster
 */
static u16b image_monster(void)
{
	byte a;
	char c;

	/* Random symbol from set above (not including final nul) */
	c = image_monster_hack[rand_int(sizeof(image_monster_hack) - 1)];

	/* Random color */
	a = randint(15);

	/* Encode */
	return (PICT(a,c));
}


/*
 * Hack -- Legal object codes
 */
static const char image_object_hack[] = \
"?/|\\\"!$()_-=[]{},~"; /* " */

/*
 * Hack -- Hallucinatory object
 */
static u16b image_object(void)
{
	byte a;
	char c;

	/* Random symbol from set above (not including final nul) */
	c = image_object_hack[rand_int(sizeof(image_object_hack) - 1)];

	/* Random color */
	a = randint(15);

	/* Encode */
	return (PICT(a,c));
}


/*
 * Hack -- Random hallucination
 */
static u16b image_random(void)
{
	/* Normally, assume monsters */
	if (rand_int(100) < 75)
	{
		return (image_monster());
	}

	/* Otherwise, assume objects */
	else
	{
		return (image_object());
	}
}


/*
 * The 16x16 tile of the terrain supports lighting
 */
bool feat_supports_lighting(byte feat)
{
	if ((feat >= FEAT_TRAP_HEAD) && (feat <= FEAT_TRAP_TAIL))
		return TRUE;

	switch (feat)
	{
		case FEAT_FLOOR:
		case FEAT_INVIS:
		case FEAT_SECRET:
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
			return TRUE;
		default:
			return FALSE;
	}
}

/*
 * Table lookup for 'metallic' attributes for all metal/gem monsters.
 * These are 1 shade lighter, or yellow if already light.
 */
byte lite_attr[16] =
{
	TERM_L_DARK,	/* TERM_DARK */
	TERM_L_WHITE, 	/* TERM_WHITE - silver */
	TERM_L_WHITE, 	/* TERM_SLATE - iron */
	TERM_YELLOW, 	/* TERM_ORANGE - brass */
	TERM_L_RED, 	/* TERM_RED - ruby */
	TERM_L_GREEN, 	/* TERM_GREEN - emerald */
	TERM_L_BLUE, 	/* TERM_BLUE - sapphire */
	TERM_L_UMBER, 	/* TERM_UMBER - copper */
	TERM_SLATE, 	/* TERM_L_DARK - coal */
	TERM_WHITE, 	/* TERM_L_WHITE - diamond */
	TERM_YELLOW, 	/* TERM_VIOLET - amethyst */
	TERM_WHITE, 	/* TERM_YELLOW - gold */
	TERM_YELLOW, 	/* TERM_L_RED */
	TERM_YELLOW, 	/* TERM_L_GREEN - adamantite*/
	TERM_YELLOW, 	/* TERM_L_BLUE - mithril*/
	TERM_YELLOW 	/* TERM_L_UMBER - bronze */
};



/*
 * Extract the attr/char to display at the given (legal) map location
 *
 * Note that this function, since it is called by "lite_spot()" which
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
 * grids using the "view_special_lite" option, causing certain such grids
 * to be displayed using special colors (if they are normally "white").
 * If the grid is "see-able" by the player, we will use the normal "white"
 * (except that, if the "view_yellow_lite" option is set, and the grid
 * is *only* "see-able" because of the player's torch, then we will use
 * "yellow"), else if the player is "blind", we will use "dark gray",
 * else if the grid is not "illuminated", we will use "dark gray", else
 * if the "view_bright_lite" option is set, we will use "slate" (gray),
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
 * like a floor, but the "view_special_lite" flag only affects actual
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
 * grids using the "view_granite_lite" option, causing certain such grids
 * to be displayed using special colors (if they are normally "white").
 * If the grid is "see-able" by the player, we will use the normal "white"
 * else if the player is "blind", we will use "dark gray", else if the
 * "view_bright_lite" option is set, we will use "slate" (gray), else we
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
 * examined, but this flag is currently ignored.  All of these flags are
 * ignored if the "avoid_other" option is set, since checking for these
 * conditions is expensive (and annoying) on some systems.
 *
 * Normally, players could be handled just like monsters, except that the
 * concept of the "torch lite" of others player would add complications.
 * For efficiency, however, we handle the (only) player first, since the
 * "player" symbol always "pre-empts" any other facts about the grid.
 *
 * The "hidden_player" efficiency option, which only makes sense with a
 * single player, allows the player symbol to be hidden while running.
 *
 * ToDo: The transformations for tile colors, or brightness for the 16x16
 * tiles should be handled differently.  One possibility would be to
 * extend feature_type with attr/char definitions for the different states.
 */
void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp)
{
	byte a;
	char c;

	s16b feat;
	byte info;

	feature_type *f_ptr;

	s16b this_o_idx, next_o_idx = 0;

	s16b m_idx;

	s16b image = p_ptr->image;

	int floor_num = 0;

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));
	bool daytime = (((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)));

	/* Hack -- Assume that "new" means "Adam Bolt Tiles" */
	bool graf_new = (use_graphics && streq(ANGBAND_GRAF, "new"));

	/* Monster/Player */
	m_idx = cave_m_idx[y][x];

	/* Feature */
	feat = cave_feat[y][x];

	/* Cave flags */
	info = cave_info[y][x];

	/* Hack -- rare random hallucination on non-outer walls */
	if (image && (!rand_int(256)) && (feat != FEAT_PERM_SOLID))
	{
		int i = image_random();

		a = PICT_A(i);
		c = PICT_C(i);
	}

	/* Boring grids (floors, etc) */
	else if (!(f_info[feat].flags1 & (FF1_REMEMBER)))
	{
		/* Memorized (or seen) floor */
		if ((info & (CAVE_MARK)) ||
		    (info & (CAVE_SEEN)))
		{
			/* Looks like floor */
			if (f_info[feat].flags2 & (FF2_ATTR_LITE))
			{
				/* Get the floor feature */
				f_ptr = &f_info[FEAT_FLOOR];

				/* Normal attr */
				a = f_ptr->x_attr;

				/* Normal char */
				c = f_ptr->x_char;

				/* Special lighting effects */
				if (view_special_lite)
				{
					/* Handle "seen" grids */
					if (info & (CAVE_SEEN))
					{
						/* Only lit by "torch" lite */
						if (view_yellow_lite && !(info & (CAVE_GLOW)))
						{
							if (graf_new)
							{
								/* Use a brightly lit tile */
								c += 2;
							}
							else
							{
								/* Use "yellow" */
								a = TERM_YELLOW;
							}
						}
						else if (view_glowing_lite && (info & (CAVE_GLOW)))
						{
							int i;

							for (i = 0; i < 8; i++)
							{
								int yy = y + ddy_ddd[i];
								int xx = x + ddx_ddd[i];

								/* Ignore annoying locations */
								if (!in_bounds_fully(yy, xx)) continue;

								if (f_info[cave_feat[yy][xx]].flags2 & (FF2_GLOW))
								{
									if (graf_new)
									{
										/* Use a brightly lit tile */
										c += 2;
									}
									else
									{
										/* Use "yellow" */
										a = TERM_YELLOW;
									}
								}
							}
						}
					}

					/* Handle "blind" */
					else if (p_ptr->blind)
					{
						if (graf_new)
						{
							/* Use a dark tile */
							c += 1;
						}
						else
						{
							/* Use "dark gray" */
							a = TERM_L_DARK;
						}
					}

					/* Handle "dark" grids */
					else if (!(info & (CAVE_GLOW)))
					{
						if (graf_new)
						{
							/* Use a dark tile */
							c += 1;
						}
						else
						{
							/* Use "dark gray" */
							a = TERM_L_DARK;
						}
					}

					/* Handle "view_bright_lite" */
					else if (view_bright_lite)
					{
						if (graf_new)
						{
							/* Use a dark tile */
							c += 1;
						}
						else
						{
							/* Use "gray" */
							a = TERM_SLATE;
						}
					}
				}
			}
			/* Boring non-floor-like grids */
			else
			{
				/* Apply "mimic" field */
				feat = f_info[feat].mimic;

				/* Get the feature */
				f_ptr = &f_info[feat];

				/* Normal attr */
				a = f_ptr->x_attr;

				/* Normal char */
				c = f_ptr->x_char;

				/* Special lighting effects */
				if (view_special_lite)
				{
					/* Handle "seen" grids */
					if (info & (CAVE_SEEN))
					{
						/* Only lit by "torch" lite */
						if (view_yellow_lite && !(info & (CAVE_GLOW)))
						{
							if (!graf_new)
							{
								/* Use lighter shade */
								a = lite_attr[f_ptr->x_attr];
							}
						}
						else if (view_glowing_lite && (info & (CAVE_GLOW)))
						{
							int i;

							for (i = 0; i < 8; i++)
							{
								int yy = y + ddy_ddd[i];
								int xx = x + ddx_ddd[i];

								/* Ignore annoying locations */
								if (!in_bounds_fully(yy, xx)) continue;

								if (f_info[cave_feat[yy][xx]].flags2 & (FF2_GLOW))
								{
									if (!graf_new)
									{
										/* Use lighter shade */
										a = lite_attr[f_ptr->x_attr];
									}
								}
							}
						}
					}
				}
			}
		}
		/* Hack -- display unseen graphic in wilderness during daytime */
		else if (surface)
		{
			/* Apply "unseen" field */
			if (variant_save_feats) feat = f_info[feat].unseen;
			else feat = f_info[feat].mimic;

			/* Get the feature */
			f_ptr = &f_info[feat];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;

			/* Hard to see at night */
			if (!daytime)
			{
				if (graf_new)
				{
					/* Use a dark tile */
					c += 1;
				}
				else
				{
					/* Use "gray" */
					a = TERM_L_DARK;
				}
			}
			/* Handle "view_bright_lite" */
			else if ((view_bright_lite) && (f_ptr->flags2 & (FF2_ATTR_LITE)))
			{
				if (graf_new)
				{
					/* Use a dark tile */
					c += 1;
				}
				else
				{
					/* Use "gray" */
					a = TERM_SLATE;
				}
			}

		}
		/* Hack -- Safe cave grid -- now use 'invisible trap' */
		else if (view_safe_grids && !(info & (CAVE_SAFE)))
		{
#if 0
			/* Get the darkness feature */
			f_ptr = &f_info[FEAT_INVIS];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;
#endif
			a = TERM_L_DARK;
			c = 'x';
		}
		/* Unknown */
		else
		{
			/* Get the darkness feature */
			f_ptr = &f_info[FEAT_NONE];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;
		}
	}

	/* Interesting grids (non-floors) */
	else
	{
		/* Memorized grids */
		if (info & (CAVE_MARK))
		{
			/* Apply "mimic" field */
			feat = f_info[feat].mimic;

			/* Get the feature */
			f_ptr = &f_info[feat];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;

			/* Special lighting effects (walls only) */
			if (view_granite_lite &&
			    (((f_info[feat].flags2 & (FF2_ATTR_LITE)) && !use_transparency) ||
			     (use_transparency && feat_supports_lighting(feat))))
			{
				/* Handle "seen" grids */
				if (info & (CAVE_SEEN))
				{
					if (graf_new)
					{
						/* Use a lit tile */
					}
					else
					{
						/* Use "white" */
					}
				}

				/* Handle "blind" */
				else if (p_ptr->blind)
				{
					if (graf_new)
					{
						/* Use a dark tile */
						c += 1;
					}
					else
					{
						/* Use "dark gray" */
						a = TERM_L_DARK;
					}
				}

				/* Handle "view_bright_lite" */
				else if (view_bright_lite)
				{
					if (graf_new)
					{
						/* Use a lit tile */
						c += 1;
					}
					else
					{
						/* Use "gray" */
						a = TERM_SLATE;
					}
				}
				else
				{
					if (graf_new)
					{
						/* Use a brightly lit tile */
						c += 2;
					}
					else
					{
						/* Use "white" */
					}
				}
			}
		}
		/* Hack -- display unseen graphic in wilderness during daytime */
		else if (surface)
		{
			/* Apply "unseen" field */
			if (variant_save_feats) feat = f_info[feat].unseen;
			else feat = f_info[feat].mimic;

			/* Get the feature */
			f_ptr = &f_info[feat];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;

			/* Hard to see at night */
			if (!daytime)
			{
				if (graf_new)
				{
					/* Use a dark tile */
					c += 1;
				}
				else
				{
					/* Use "gray" */
					a = TERM_L_DARK;
				}
			}
			/* Handle "view_bright_lite" */
			else if ((view_bright_lite) && (f_ptr->flags2 & (FF2_ATTR_LITE)))
			{
				if (graf_new)
				{
					/* Use a dark tile */
					c += 1;
				}
				else
				{
					/* Use "gray" */
					a = TERM_SLATE;
				}
			}

		}

		/* Hack -- Safe cave grid -- now use 'invisible trap' */
		else if (view_safe_grids && !(info & (CAVE_SAFE)))
		{
#if 0
			/* Get the darkness feature */
			f_ptr = &f_info[FEAT_INVIS];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;
#endif
			a = TERM_L_DARK;
			c = 'x';

		}

		/* Unknown */
		else
		{
			/* Get the darkness feature */
			f_ptr = &f_info[FEAT_NONE];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;
		}
	}

#ifdef USE_TRANSPARENCY

	/* Save the terrain info for the transparency effects */
	(*tap) = a;
	(*tcp) = c;

#endif /* USE_TRANSPARENCY */

	/* Objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized objects */
		if (o_ptr->marked)
		{
			/* Hack -- object hallucination */
			if (image)
			{
				int i = image_object();

				a = PICT_A(i);
				c = PICT_C(i);

				break;
			}

			/* Normal attr */
			a = object_attr(o_ptr);

			/* Normal char */
			c = object_char(o_ptr);

			/* First marked object */
			if (!show_piles) break;

			/* Special stack symbol */
			if (++floor_num > 1)
			{
				object_kind *k_ptr;

				/* Get the "pile" feature */
				k_ptr = &k_info[0];

				/* Normal attr */
				a = k_ptr->x_attr;

				/* Normal char */
				c = k_ptr->x_char;

				break;
			}
		}
	}


	/* Monsters */
	if (m_idx > 0)
	{
		monster_type *m_ptr = &m_list[m_idx];

		/* Visible monster */
		if (m_ptr->ml)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			byte da;
			char dc;

			/* Desired attr */
			da = r_ptr->x_attr;

			/* Desired char */
			dc = r_ptr->x_char;

			/* Hack -- monster hallucination */
			if (image)
			{
				int i = image_monster();

				a = PICT_A(i);
				c = PICT_C(i);
			}

			/* Ignore weird codes */
			else if (avoid_other)
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Special attr/char codes */
			else if ((da & 0x80) && (dc & 0x80))
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Multi-hued monster */
			else if (r_ptr->flags1 & (RF1_ATTR_MULTI))
			{
				/* Multi-hued attr */
				a = randint(15);

				/* Normal char */
				c = dc;
			}

			/* Metallic monster */
			else if (r_ptr->flags1 & (RF1_ATTR_METAL))
			{
				/* Flickering metallic attr - predominate base color */
				if (!rand_int(3)) a = lite_attr[da];
				else a = da;

				/* Normal char */
				c = dc;
			}

			/* Normal monster (not "clear" in any way) */
			else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR | RF1_CHAR_CLEAR)))
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Hack -- Bizarre grid under monster */
			else if ((a & 0x80) || (c & 0x80))
			{
				/* Use attr */
				a = da;

				/* Use char */
				c = dc;
			}

			/* Normal char, Clear attr, monster */
			else if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)))
			{
				/* Normal char */
				c = dc;
			}

			/* Normal attr, Clear char, monster */
			else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)))
			{
				/* Normal attr */
				a = da;
			}
		}
	}

	/* Handle "player" */
	else if ((m_idx < 0) && !(p_ptr->running && hidden_player))
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		a = r_ptr->x_attr;

		/* Get the "player" char */
		c = r_ptr->x_char;
	}

#ifdef MAP_INFO_MULTIPLE_PLAYERS
	/* Players */
	else if (m_idx < 0)
#else /* MAP_INFO_MULTIPLE_PLAYERS */
	/* Handle "player" */
	else if ((m_idx < 0) && !(p_ptr->running && hidden_player))
#endif /* MAP_INFO_MULTIPLE_PLAYERS */
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		a = r_ptr->x_attr;

		/* Get the "player" char */
		c = r_ptr->x_char;
	}

	/* Result */
	(*ap) = a;
	(*cp) = c;
}



/*
 * Move the cursor to a given map location.
 *
 * The main screen will always be at least 24x80 in size.
 */
void move_cursor_relative(int y, int x)
{
	unsigned ky, kx;
	unsigned vy, vx;

	/* Location relative to panel */
	ky = (unsigned)(y - p_ptr->wy);

	/* Verify location */
	if (ky >= (unsigned)(SCREEN_HGT)) return;

	/* Location relative to panel */
	kx = (unsigned)(x - p_ptr->wx);

	/* Verify location */
	if (kx >= (unsigned)(SCREEN_WID)) return;

	/* Location in window */
	vy = ky + ROW_MAP;

	/* Location in window */
	vx = kx + COL_MAP;

	/* Go there */
	Term_gotoxy(vx, vy);
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
	unsigned ky, kx;
	unsigned vy, vx;

	/* Location relative to panel */
	ky = (unsigned)(y - p_ptr->wy);

	/* Verify location */
	if (ky >= (unsigned)(SCREEN_HGT)) return;

	/* Location relative to panel */
	kx = (unsigned)(x - p_ptr->wx);

	/* Verify location */
	if (kx >= (unsigned)(SCREEN_WID)) return;

	/* Location in window */
	vy = ky + ROW_MAP;

	/* Location in window */
	vx = kx + COL_MAP;

	/* Hack -- Queue it */
	Term_queue_char(vx, vy, a, c, 0, 0);

	return;
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
	s16b feat;
	byte info;

	s16b this_o_idx, next_o_idx = 0;


	/* Get cave info */
	info = cave_info[y][x];

	/* Get cave feat */
	feat = cave_feat[y][x];

	/* Require "seen" flag */
	if (!(info & (CAVE_SEEN))) return;

	/* Mark it */
	cave_info[y][x] |= (CAVE_SAFE);

	/* Hack -- memorize objects */
	/* ANDY -- Only memorise objects if they are not hidden by the feature */
	if (!(f_info[feat].flags2 & (FF2_HIDE_ITEM)))
	{
		for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr = &o_list[this_o_idx];
			object_kind *k_ptr = &k_info[o_ptr->k_idx];

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Memorize objects */
			if (!auto_pickup_ignore(o_ptr)) o_ptr->marked = TRUE;

			/* Hack -- have seen object */
			if (!(k_ptr->flavor)) k_ptr->aware = TRUE;
		}
	}

	/* Hack -- memorize grids */
	if (!(info & (CAVE_MARK)))
	{
		/* Memorize some "boring" grids */
		if (!(f_info[feat].flags1 & (FF1_REMEMBER)))
		{

			bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

			if (surface && !(info & (CAVE_GLOW))) cave_info[y][x] |= (CAVE_MARK);

			/* Option -- memorize certain floors */
			else if (((info & (CAVE_GLOW)) && view_perma_grids) ||
			    view_torch_grids)
			{
				/* Memorize */
				cave_info[y][x] |= (CAVE_MARK);
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


/*
 * Redraw (on the screen) a given map location
 *
 * This function should only be called on "legal" grids.
 *
 * Note the inline use of "print_rel()" for efficiency.
 *
 * The main screen will always be at least 24x80 in size.
 */
void lite_spot(int y, int x)
{
	byte a;
	char c;

	byte ta;
	char tc;

	unsigned ky, kx;
	unsigned vy, vx;

	/* Location relative to panel */
	ky = (unsigned)(y - p_ptr->wy);

	/* Verify location */
	if (ky >= (unsigned)(SCREEN_HGT)) return;

	/* Location relative to panel */
	kx = (unsigned)(x - p_ptr->wx);

	/* Verify location */
	if (kx >= (unsigned)(SCREEN_WID)) return;

	/* Location in window */
	vy = ky + ROW_MAP;

	/* Location in window */
	vx = kx + COL_MAP;

	/* Hack -- redraw the grid */
	map_info(y, x, &a, &c, &ta, &tc);

	/* Hack -- Queue it */
	Term_queue_char(vx, vy, a, c, ta, tc);

	return;
}



/*
 * Redraw (on the screen) the current map panel
 *
 * Note the inline use of "lite_spot()" for efficiency.
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

	/* Assume screen */
	ty = ROW_MAP + SCREEN_HGT;
	tx = COL_MAP + SCREEN_WID;

	/* Dump the map */
	for (y = p_ptr->wy, vy = ROW_MAP; vy < ty; vy++, y++)
	{
		for (x = p_ptr->wx, vx = COL_MAP; vx < tx; vx++, x++)
		{
			/* Check bounds */
			if (!in_bounds(y, x)) continue;

			/* Determine what is there */
			map_info(y, x, &a, &c, &ta, &tc);

			/* Hack -- Queue it */
			Term_queue_char(vx, vy, a, c, ta, tc);

		}
	}

	return;
}


/*
 * Hack -- a priority function (rewritten by ANDY)
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
 * Maximum size of map.
 */
#define MAP_HGT (DUNGEON_HGT / 3)
#define MAP_WID (DUNGEON_WID / 3)


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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int map_hgt, map_wid;
	int dungeon_hgt, dungeon_wid;
	int row, col;

	int x, y;

	byte ta;
	char tc;

	byte tp;

	/* Large array on the stack */
	byte mp[DUNGEON_HGT][DUNGEON_WID];

	bool old_view_special_lite;
	bool old_view_granite_lite;

	monster_race *r_ptr = &r_info[0];

	town_type *t_ptr = &t_info[p_ptr->dungeon];

	dungeon_zone *zone=&t_ptr->zone[0];;

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Desired map height */
	map_hgt = Term->hgt - 2;
	map_wid = Term->wid - 2;

	dungeon_hgt = (!(zone->fill)) ? TOWN_HGT : DUNGEON_HGT;
	dungeon_wid = (!(zone->fill)) ? TOWN_WID : DUNGEON_WID;

	/* Prevent accidents */
	if (map_hgt > dungeon_hgt) map_hgt = dungeon_hgt;
	if (map_wid > dungeon_wid) map_wid = dungeon_wid;

	/* Prevent accidents */
	if ((map_wid < 1) || (map_hgt < 1)) return;


	/* Save lighting effects */
	old_view_special_lite = view_special_lite;
	old_view_granite_lite = view_granite_lite;

	/* Disable lighting effects */
	view_special_lite = FALSE;
	view_granite_lite = FALSE;


	/* Nothing here */
	ta = TERM_WHITE;
	tc = ' ';

	/* Clear the priorities */
	for (y = 0; y < map_hgt; ++y)
	{
		for (x = 0; x < map_wid; ++x)
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
	for (y = 0; y < dungeon_hgt; y++)
	{
		for (x = 0; x < dungeon_wid; x++)
		{
			row = (y * map_hgt / dungeon_hgt);
			col = (x * map_wid / dungeon_wid);

			/* Get the attr/char at that map location */
			map_info(y, x, &ta, &tc, &ta, &tc);

			/* Get the priority of that attr/char */
			tp = priority(ta, tc);

			/* Save "best" */
			if (mp[row][col] < tp)
			{
				/* Add the character */
				Term_putch(col + 1, row + 1, ta, tc);

				/* Save priority */
				mp[row][col] = tp;
			}
		}
	}


	/* Player location */
	row = (py * map_hgt / dungeon_hgt);
	col = (px * map_wid / dungeon_wid);


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
	view_special_lite = old_view_special_lite;
	view_granite_lite = old_view_granite_lite;
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
	Term_fresh();

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
 * Some comments on the dungeon related data structures and functions...
 *
 * Angband is primarily a dungeon exploration game, and it should come as
 * no surprise that the internal representation of the dungeon has evolved
 * over time in much the same way as the game itself, to provide semantic
 * changes to the game itself, to make the code simpler to understand, and
 * to make the executable itself faster or more efficient in various ways.
 *
 * There are a variety of dungeon related data structures, and associated
 * functions, which store information about the dungeon, and provide methods
 * by which this information can be accessed or modified.
 *
 * Some of this information applies to the dungeon as a whole, such as the
 * list of unique monsters which are still alive.  Some of this information
 * only applies to the current dungeon level, such as the current depth, or
 * the list of monsters currently inhabiting the level.  And some of the
 * information only applies to a single grid of the current dungeon level,
 * such as whether the grid is illuminated, or whether the grid contains a
 * monster, or whether the grid can be seen by the player.  If Angband was
 * to be turned into a multi-player game, some of the information currently
 * associated with the dungeon should really be associated with the player,
 * such as whether a given grid is viewable by a given player.
 *
 * One of the major bottlenecks in ancient versions of Angband was in the
 * calculation of "line of sight" from the player to various grids, such
 * as those containing monsters, using the relatively expensive "los()"
 * function.  This was such a nasty bottleneck that a lot of silly things
 * were done to reduce the dependancy on "line of sight", for example, you
 * could not "see" any grids in a lit room until you actually entered the
 * room, at which point every grid in the room became "illuminated" and
 * all of the grids in the room were "memorized" forever.  Other major
 * bottlenecks involved the determination of whether a grid was lit by the
 * player's torch, and whether a grid blocked the player's line of sight.
 * These bottlenecks led to the development of special new functions to
 * optimize issues involved with "line of sight" and "torch lit grids".
 * These optimizations led to entirely new additions to the game, such as
 * the ability to display the player's entire field of view using different
 * colors than were used for the "memorized" portions of the dungeon, and
 * the ability to memorize dark floor grids, but to indicate by the way in
 * which they are displayed that they are not actually illuminated.  And
 * of course many of them simply made the game itself faster or more fun.
 * Also, over time, the definition of "line of sight" has been relaxed to
 * allow the player to see a wider "field of view", which is slightly more
 * realistic, and only slightly more expensive to maintain.
 *
 * Currently, a lot of the information about the dungeon is stored in ways
 * that make it very efficient to access or modify the information, while
 * still attempting to be relatively conservative about memory usage, even
 * if this means that some information is stored in multiple places, or in
 * ways which require the use of special code idioms.  For example, each
 * monster record in the monster array contains the location of the monster,
 * and each cave grid has an index into the monster array, or a zero if no
 * monster is in the grid.  This allows the monster code to efficiently see
 * where the monster is located, while allowing the dungeon code to quickly
 * determine not only if a monster is present in a given grid, but also to
 * find out which monster.  The extra space used to store the information
 * twice is inconsequential compared to the speed increase.
 *
 * Some of the information about the dungeon is used by functions which can
 * constitute the "critical efficiency path" of the game itself, and so the
 * way in which they are stored and accessed has been optimized in order to
 * optimize the game itself.  For example, the "update_view()" function was
 * originally created to speed up the game itself (when the player was not
 * running), but then it took on extra responsibility as the provider of the
 * new "special effects lighting code", and became one of the most important
 * bottlenecks when the player was running.  So many rounds of optimization
 * were performed on both the function itself, and the data structures which
 * it uses, resulting eventually in a function which not only made the game
 * faster than before, but which was responsible for even more calculations
 * (including the determination of which grids are "viewable" by the player,
 * which grids are illuminated by the player's torch, and which grids can be
 * "seen" in some way by the player), as well as for providing the guts of
 * the special effects lighting code, and for the efficient redisplay of any
 * grids whose visual representation may have changed.
 *
 * Several pieces of information about each cave grid are stored in various
 * two dimensional arrays, with one unit of information for each grid in the
 * dungeon.  Some of these arrays have been intentionally expanded by a small
 * factor to make the two dimensional array accesses faster by allowing the
 * use of shifting instead of multiplication.
 *
 * Several pieces of information about each cave grid are stored in the
 * "cave_info" array, which is a special two dimensional array of bytes,
 * one for each cave grid, each containing eight separate "flags" which
 * describe some property of the cave grid.  These flags can be checked and
 * modified extremely quickly, especially when special idioms are used to
 * force the compiler to keep a local register pointing to the base of the
 * array.  Special location offset macros can be used to minimize the number
 * of computations which must be performed at runtime.  Note that using a
 * byte for each flag set may be slightly more efficient than using a larger
 * unit, so if another flag (or two) is needed later, and it must be fast,
 * then the two existing flags which do not have to be fast should be moved
 * out into some other data structure and the new flags should take their
 * place.  This may require a few minor changes in the savefile code.
 *
 * The "CAVE_ROOM" flag is saved in the savefile and is used to determine
 * which grids are part of "rooms", and thus which grids are affected by
 * "illumination" spells.  This flag does not have to be very fast.
 *
 * The "CAVE_ICKY" flag is saved in the savefile and is used to determine
 * which grids are part of "vaults", and thus which grids cannot serve as
 * the destinations of player teleportation.  This flag does not have to
 * be very fast.
 *
 * The "CAVE_MARK" flag is saved in the savefile and is used to determine
 * which grids have been "memorized" by the player.  This flag is used by
 * the "map_info()" function to determine if a grid should be displayed.
 * This flag is used in a few other places to determine if the player can
 * "know" about a given grid.  This flag must be very fast.
 *
 * The "CAVE_GLOW" flag is saved in the savefile and is used to determine
 * which grids are "permanently illuminated".  This flag is used by the
 * "update_view()" function to help determine which viewable flags may
 * be "seen" by the player.  This flag is used by the "map_info" function
 * to determine if a grid is only lit by the player's torch.  This flag
 * has special semantics for wall grids (see "update_view()").  This flag
 * must be very fast.
 *
 * The "CAVE_WALL" flag is used to determine which grids block the player's
 * line of sight.  This flag is used by the "update_view()" function to
 * determine which grids block line of sight, and to help determine which
 * grids can be "seen" by the player.  This flag must be very fast.
 *
 * The "CAVE_VIEW" flag is used to determine which grids are currently in
 * line of sight of the player.  This flag is set by (and used by) the
 * "update_view()" function.  This flag is used by any code which needs to
 * know if the player can "view" a given grid.  This flag is used by the
 * "map_info()" function for some optional special lighting effects.  The
 * "player_has_los_bold()" macro wraps an abstraction around this flag, but
 * certain code idioms are much more efficient.  This flag is used to check
 * if a modification to a terrain feature might affect the player's field of
 * view.  This flag is used to see if certain monsters are "visible" to the
 * player.  This flag is used to allow any monster in the player's field of
 * view to "sense" the presence of the player.  This flag must be very fast.
 *
 * The "CAVE_SEEN" flag is used to determine which grids are currently in
 * line of sight of the player and also illuminated in some way.  This flag
 * is set by the "update_view()" function, using computations based on the
 * "CAVE_VIEW" and "CAVE_WALL" and "CAVE_GLOW" flags of various grids.  This
 * flag is used by any code which needs to know if the player can "see" a
 * given grid.  This flag is used by the "map_info()" function both to see
 * if a given "boring" grid can be seen by the player, and for some optional
 * special lighting effects.  The "player_can_see_bold()" macro wraps an
 * abstraction around this flag, but certain code idioms are much more
 * efficient.  This flag is used to see if certain monsters are "visible" to
 * the player.  This flag is never set for a grid unless "CAVE_VIEW" is also
 * set for the grid.  Whenever the "CAVE_WALL" or "CAVE_GLOW" flag changes
 * for a grid which has the "CAVE_VIEW" flag set, the "CAVE_SEEN" flag must
 * be recalculated.  The simplest way to do this is to call "forget_view()"
 * and "update_view()" whenever the "CAVE_WALL" or "CAVE_GLOW" flags change
 * for a grid which has "CAVE_VIEW" set.  This flag must be very fast.
 *
 * The "CAVE_TEMP" flag is used for a variety of temporary purposes.  This
 * flag is used to determine if the "CAVE_SEEN" flag for a grid has changed
 * during the "update_view()" function.  This flag is used to "spread" light
 * or darkness through a room.  This flag is used by the "monster flow code".
 * This flag must always be cleared by any code which sets it, often, this
 * can be optimized by the use of the special "temp_g", "temp_y", "temp_x"
 * arrays (and the special "temp_n" global).  This flag must be very fast.
 *
 * Note that the "CAVE_MARK" flag is used for many reasons, some of which
 * are strictly for optimization purposes.  The "CAVE_MARK" flag means that
 * even if the player cannot "see" the grid, he "knows" about the terrain in
 * that grid.  This is used to "memorize" grids when they are first "seen" by
 * the player, and to allow certain grids to be "detected" by certain magic.
 * Note that most grids are always memorized when they are first "seen", but
 * "boring" grids (floor grids) are only memorized if the "view_torch_grids"
 * option is set, or if the "view_perma_grids" option is set, and the grid
 * in question has the "CAVE_GLOW" flag set.
 *
 * Objects are "memorized" in a different way, using a special "marked" flag
 * on the object itself, which is set when an object is observed or detected.
 * This allows objects to be "memorized" independant of the terrain features.
 *
 * The "update_view()" function is an extremely important function.  It is
 * called only when the player moves, significant terrain changes, or the
 * player's blindness or torch radius changes.  Note that when the player
 * is resting, or performing any repeated actions (like digging, disarming,
 * farming, etc), there is no need to call the "update_view()" function, so
 * even if it was not very efficient, this would really only matter when the
 * player was "running" through the dungeon.  It sets the "CAVE_VIEW" flag
 * on every cave grid in the player's field of view, and maintains an array
 * of all such grids in the global "view_g" array.  It also checks the torch
 * radius of the player, and sets the "CAVE_SEEN" flag for every grid which
 * is in the "field of view" of the player and which is also "illuminated",
 * either by the players torch (if any) or by any permanent light source.
 * It could use and help maintain information about multiple light sources,
 * which would be helpful in a multi-player version of Angband.
 *
 * The "update_view()" function maintains the special "view_g" array, which
 * contains exactly those grids which have the "CAVE_VIEW" flag set.  This
 * array is used by "update_view()" to (only) memorize grids which become
 * newly "seen", and to (only) redraw grids whose "seen" value changes, which
 * allows the use of some interesting (and very efficient) "special lighting
 * effects".  In addition, this array could be used elsewhere to quickly scan
 * through all the grids which are in the player's field of view.
 *
 * Note that the "update_view()" function allows, among other things, a room
 * to be "partially" seen as the player approaches it, with a growing cone
 * of floor appearing as the player gets closer to the door.  Also, by not
 * turning on the "memorize perma-lit grids" option, the player will only
 * "see" those floor grids which are actually in line of sight.  And best
 * of all, you can now activate the special lighting effects to indicate
 * which grids are actually in the player's field of view by using dimmer
 * colors for grids which are not in the player's field of view, and/or to
 * indicate which grids are illuminated only by the player's torch by using
 * the color yellow for those grids.
 *
 * The old "update_view()" algorithm uses the special "CAVE_EASY" flag as a
 * temporary internal flag to mark those grids which are not only in view,
 * but which are also "easily" in line of sight of the player.  This flag
 * is actually just the "CAVE_SEEN" flag, and the "update_view()" function
 * makes sure to clear it for all old "CAVE_SEEN" grids, and then use it in
 * the algorithm as "CAVE_EASY", and then clear it for all "CAVE_EASY" grids,
 * and then reset it as appropriate for all new "CAVE_SEEN" grids.  This is
 * kind of messy, but it works.  The old algorithm may disappear eventually.
 *
 * The new "update_view()" algorithm uses a faster and more mathematically
 * correct algorithm, assisted by a large machine generated static array, to
 * determine the "CAVE_VIEW" and "CAVE_SEEN" flags simultaneously.  See below.
 *
 * It seems as though slight modifications to the "update_view()" functions
 * would allow us to determine "reverse" line-of-sight as well as "normal"
 * line-of-sight", which would allow monsters to have a more "correct" way
 * to determine if they can "see" the player, since right now, they "cheat"
 * somewhat and assume that if the player has "line of sight" to them, then
 * they can "pretend" that they have "line of sight" to the player.  But if
 * such a change was attempted, the monsters would actually start to exhibit
 * some undesirable behavior, such as "freezing" near the entrances to long
 * hallways containing the player, and code would have to be added to make
 * the monsters move around even if the player was not detectable, and to
 * "remember" where the player was last seen, to avoid looking stupid.
 *
 * Note that the "CAVE_GLOW" flag means that a grid is permanently lit in
 * some way.  However, for the player to "see" the grid, as determined by
 * the "CAVE_SEEN" flag, the player must not be blind, the grid must have
 * the "CAVE_VIEW" flag set, and if the grid is a "wall" grid, and it is
 * not lit by the player's torch, then it must touch a grid which does not
 * have the "CAVE_WALL" flag set, but which does have both the "CAVE_GLOW"
 * and "CAVE_VIEW" flags set.  This last part about wall grids is induced
 * by the semantics of "CAVE_GLOW" as applied to wall grids, and checking
 * the technical requirements can be very expensive, especially since the
 * grid may be touching some "illegal" grids.  Luckily, it is more or less
 * correct to restrict the "touching" grids from the eight "possible" grids
 * to the (at most) three grids which are touching the grid, and which are
 * closer to the player than the grid itself, which eliminates more than
 * half of the work, including all of the potentially "illegal" grids, if
 * at most one of the three grids is a "diagonal" grid.  In addition, in
 * almost every situation, it is possible to ignore the "CAVE_VIEW" flag
 * on these three "touching" grids, for a variety of technical reasons.
 * Finally, note that in most situations, it is only necessary to check
 * a single "touching" grid, in fact, the grid which is strictly closest
 * to the player of all the touching grids, and in fact, it is normally
 * only necessary to check the "CAVE_GLOW" flag of that grid, again, for
 * various technical reasons.  However, one of the situations which does
 * not work with this last reduction is the very common one in which the
 * player approaches an illuminated room from a dark hallway, in which the
 * two wall grids which form the "entrance" to the room would not be marked
 * as "CAVE_SEEN", since of the three "touching" grids nearer to the player
 * than each wall grid, only the farthest of these grids is itself marked
 * "CAVE_GLOW".
 *
 *
 * Here are some pictures of the legal "light source" radius values, in
 * which the numbers indicate the "order" in which the grids could have
 * been calculated, if desired.  Note that the code will work with larger
 * radiuses, though currently yields such a radius, and the game would
 * become slower in some situations if it did.
 *
 *       Rad=0     Rad=1      Rad=2Rad=3
 *      No-Lite  Torch,etc   Lantern     Artifacts
 *
 *  333
 *     333 43334
 *  212       32123       3321233
 * @1@1       31@13       331@133
 *  212       32123       3321233
 *     333 43334
 *  333
 *
 *
 * Here is an illustration of the two different "update_view()" algorithms,
 * in which the grids marked "%" are pillars, and the grids marked "?" are
 * not in line of sight of the player.
 *
 *
 *    Sample situation
 *
 *  #####################
 *  ############.%.%.%.%#
 *  #...@..#####........#
 *  #............%.%.%.%#
 *  #......#####........#
 *  ############........#
 *  #####################
 *
 *
 *  New Algorithm     Old Algorithm
 *
 *      ########?????????????    ########?????????????
 *      #...@..#?????????????    #...@..#?????????????
 *      #...........?????????    #.........???????????
 *      #......#####.....????    #......####??????????
 *      ########?????????...#    ########?????????????
 *
 *      ########?????????????    ########?????????????
 *      #.@....#?????????????    #.@....#?????????????
 *      #............%???????    #...........?????????
 *      #......#####........?    #......#####?????????
 *      ########??????????..#    ########?????????????
 *
 *      ########?????????????    ########?????%???????
 *      #......#####........#    #......#####..???????
 *      #.@..........%???????    #.@..........%???????
 *      #......#####........#    #......#####..???????
 *      ########?????????????    ########?????????????
 *
 *      ########??????????..#    ########?????????????
 *      #......#####........?    #......#####?????????
 *      #............%???????    #...........?????????
 *      #.@....#?????????????    #.@....#?????????????
 *      ########?????????????    ########?????????????
 *
 *      ########?????????%???    ########?????????????
 *      #......#####.....????    #......####??????????
 *      #...........?????????    #.........???????????
 *      #...@..#?????????????    #...@..#?????????????
 *      ########?????????????    ########?????????????
 */




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
	s16b grid[8];

	u32b bits_3;
	u32b bits_2;
	u32b bits_1;
	u32b bits_0;

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
 *	- Number of grids
 *
 *	- Number of slopes
 *
 *	- Slope values
 *
 *	- Slope range per grid
 */
struct vinfo_hack {

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
static bool ang_sort_comp_hook_longs(vptr u, vptr v, int a, int b)
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
static void ang_sort_swap_hook_longs(vptr u, vptr v, int a, int b)
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
				quit_fmt("Too many slopes (%d)!",
			 	VINFO_MAX_SLOPES);
			}

			/* Save the slope, and advance */
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
	int i, g;
	int y, x;

	long m;

	vinfo_hack *hack;

	int num_grids = 0;

	int queue_head = 0;
	int queue_tail = 0;
	vinfo_type *queue[VINFO_MAX_GRIDS*2];


	/* Make hack */
	MAKE(hack, vinfo_hack);


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


	/* Enforce maximal efficiency */
	if (num_grids < VINFO_MAX_GRIDS)
	{
		quit_fmt("Too few grids (%d < %d)!",
		 num_grids, VINFO_MAX_GRIDS);
	}

	/* Enforce maximal efficiency */
	if (hack->num_slopes < VINFO_MAX_SLOPES)
	{
		quit_fmt("Too few slopes (%d < %d)!",
		 hack->num_slopes, VINFO_MAX_SLOPES);
	}


	/* Sort slopes numerically */
	ang_sort_comp = ang_sort_comp_hook_longs;

	/* Sort slopes numerically */
	ang_sort_swap = ang_sort_swap_hook_longs;

	/* Sort the (unique) slopes */
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


		/* Analyze slopes */
		for (i = 0; i < hack->num_slopes; ++i)
		{
			m = hack->slopes[i];

			/* Memorize intersection slopes (for non-player-grids) */
			if ((e > 0) &&
			    (hack->slopes_min[y][x] < m) &&
			    (m < hack->slopes_max[y][x]))
			{
				switch (i / 32)
				{
					case 3: vinfo[e].bits_3 |= (1L << (i % 32)); break;
					case 2: vinfo[e].bits_2 |= (1L << (i % 32)); break;
					case 1: vinfo[e].bits_1 |= (1L << (i % 32)); break;
					case 0: vinfo[e].bits_0 |= (1L << (i % 32)); break;
				}
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


		/* Extra values */
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
	KILL(hack);


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

	byte *fast_cave_info = &cave_info[0][0];


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
		lite_spot(y, x);
	}

	/* None left */
	fast_view_n = 0;


	/* Save 'view_n' */
	view_n = fast_view_n;
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
 * g = pg + p->grid[o2];
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

	int pg = GRID(py,px);

	int i, g, o2;

	int radius;

	int fast_view_n = view_n;
	u16b *fast_view_g = view_g;

	int fast_temp_n = 0;
	u16b *fast_temp_g = temp_g;

	byte *fast_cave_info = &cave_info[0][0];

	byte info;

#ifdef MONSTER_LITE
	int fy,fx;
#endif

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

		/* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
		info &= ~(CAVE_VIEW | CAVE_SEEN);

		/* Clear "CAVE_LITE" flag */
		/* info &= ~(CAVE_LITE); */

		/* Save cave info */
		fast_cave_info[g] = info;
	}

	/* Reset the "view" array */
	fast_view_n = 0;

	/* Extract "radius" value */
	radius = p_ptr->cur_lite;

	/* Handle real light */
	if (radius > 0) ++radius;

#ifdef MONSTER_LITE
	/*** Step 1A -- monster lites ***/
	/* Scan monster list and add monster lites */
	for ( i = 1; i < z_info->m_max; i++)
	{

		/* Check the i'th monster */
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

	      /* Skip dead monsters */
      	if (!m_ptr->r_idx) continue;

		/* Skip sleeping monsters */
		if (m_ptr->csleep) continue;

		/* Skip hiding monsters */
		if (m_ptr->mflag & (MFLAG_HIDE)) continue;

		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		/* Carrying lite */
		if ((r_ptr->flags2 & (RF2_HAS_LITE))
			|| ((r_ptr->flags2 & (RF2_NEED_LITE)) && !(radius)
                  && !(fast_cave_info[pg] & (CAVE_GLOW))))
		{
			/* monster grid */
			if (los(py,px,fy,fx))
			{
				g = GRID(fy,fx);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			/* Radius 1 -- torch radius */

			/* Adjacent grid */
			if (los(py,px,fy+1,fx))
			{
				g = GRID(fy+1,fx);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			if (los(py,px,fy-1,fx))
			{
				g = GRID(fy-1,fx);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			if (los(py,px,fy,fx+1))
			{
				g = GRID(fy,fx+1);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			if (los(py,px,fy,fx-1))
			{
				g = GRID(fy,fx-1);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			/* Diagonal grids */
			if (los(py,px,fy+1,fx+1))
			{
				g = GRID(fy+1,fx+1);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			if (los(py,px,fy+1,fx-1))
			{
				g = GRID(fy+1,fx-1);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			if (los(py,px,fy-1,fx+1))
			{
				g = GRID(fy-1,fx+1);
				info = fast_cave_info[g];

				info |= (CAVE_VIEW);
				info |= (CAVE_SEEN);

				/* Save cave info */
				fast_cave_info[g] = info;

				/* Save in array */
				fast_view_g[fast_view_n++] = g;
			}

			if (los(py,px,fy-1,fx-1))
			{
				g = GRID(fy-1,fx-1);
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
#endif
	/*** Step 1B -- player grid ***/

	/* Player grid */
	g = pg;

	/* Get grid info */
	info = fast_cave_info[g];

	/* Assume viewable */
	info |= (CAVE_VIEW);

	/* Torch-lit grid */
	if (0 < radius)
	{
		/* Mark as "CAVE_SEEN" */
		info |= (CAVE_SEEN);

		/* Mark as "CAVE_LITE" */
		/* info |= (CAVE_LITE); */
	}

	/* Perma-lit grid */
	else if (info & (CAVE_GLOW))
	{
		/* Mark as "CAVE_SEEN" */
		info |= (CAVE_SEEN);
	}

	/* Save cave info */
	fast_cave_info[g] = info;

	/* Save in array */
	fast_view_g[fast_view_n++] = g;


	/*** Step 2 -- octants ***/

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

				/* Handle wall */
				if (info & (CAVE_WALL))
				{
					/* Clear bits */
					bits0 &= ~(p->bits_0);
					bits1 &= ~(p->bits_1);
					bits2 &= ~(p->bits_2);
					bits3 &= ~(p->bits_3);

					/* Newly viewable wall */
					if (!(info & (CAVE_VIEW)))
					{
						/* Mark as viewable */
						info |= (CAVE_VIEW);

						/* Torch-lit grids */
						if (p->d < radius)
						{
							/* Mark as "CAVE_SEEN" */
							info |= (CAVE_SEEN);

							/* Mark as "CAVE_LITE" */
							/* info |= (CAVE_LITE); */
						}

						/* Perma-lit grids */
						else if (info & (CAVE_GLOW))
						{
							int y = GRID_Y(g);
							int x = GRID_X(g);

							/* Hack -- move towards player */
							int yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
							int xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

#ifdef UPDATE_VIEW_COMPLEX_WALL_ILLUMINATION

							/* Check for "complex" illumination */
							if ((!(cave_info[yy][xx] & (CAVE_WALL)) &&
							      (cave_info[yy][xx] & (CAVE_GLOW))) ||
							    (!(cave_info[y][xx] & (CAVE_WALL)) &&
							      (cave_info[y][xx] & (CAVE_GLOW))) ||
							    (!(cave_info[yy][x] & (CAVE_WALL)) &&
							      (cave_info[yy][x] & (CAVE_GLOW))))
							{
								/* Mark as seen */
								info |= (CAVE_SEEN);
							}

#else /* UPDATE_VIEW_COMPLEX_WALL_ILLUMINATION */

							/* Check for "simple" illumination */
							if (cave_info[yy][xx] & (CAVE_GLOW))
							{
								/* Mark as seen */
								info |= (CAVE_SEEN);
							}

#endif /* UPDATE_VIEW_COMPLEX_WALL_ILLUMINATION */

						}

						/* Save cave info */
						fast_cave_info[g] = info;

						/* Save in array */
						fast_view_g[fast_view_n++] = g;
					}
				}

				/* Handle non-wall */
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

					/* Newly viewable non-wall */
					if (!(info & (CAVE_VIEW)))
					{
						/* Mark as "viewable" */
						info |= (CAVE_VIEW);

						/* Torch-lit grids */
						if (p->d < radius)
						{
							/* Mark as "CAVE_SEEN" */
							info |= (CAVE_SEEN);

							/* Mark as "CAVE_LITE" */
							/* info |= (CAVE_LITE); */
						}

						/* Perma-lit grids */
						else if (info & (CAVE_GLOW))
						{
							/* Mark as "CAVE_SEEN" */
							info |= (CAVE_SEEN);
						}

						/* Save cave info */
						fast_cave_info[g] = info;

						/* Save in array */
						fast_view_g[fast_view_n++] = g;
					}
				}
			}
		}
	}


	/*** Step 3 -- Complete the algorithm ***/

	/* Handle blindness */
	if (p_ptr->blind)
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
			lite_spot(y, x);
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
			lite_spot(y, x);
		}
	}


	/* Save 'view_n' */
	view_n = fast_view_n;
}

/*
 * Update the features that have dynamic flags. These grids need to be
 * checked every turn to see if they affected adjacent grids with
 * an attack of some kind.
 *
 * INSTANT grids always alter themselves.
 *
 * EXPLODE grids always explode for a radius 1 ball attack equal to twice the
 * damage of their blow attack.
 *
 * TIMED grids will 2% of the time alter themselves.
 *
 * SPREAD grids will 66% of the time affect an adjacent grid with an attack
 * equal to their blow attack, along with their own grid. 8% of the time they
 * will affect their own grid only.
 *
 * ERUPT grids will 5% of the time project a radius 2 ball attack equal to three times
 * the damage of their blow attack, centred on their grid.
 *
 * STRIKE grids will 5% of the time project a radius 0 ball attack equal to ten times
 * the damage of their blow attack, centred on their grid. At some point, it'd be
 * nice to change this to a beam attack in a random direction, but this will require
 * changes to the project function.
 *
 * Examples of dynamic attacks include fire, smoke, steam, acidic vapours and
 * poison gas.  Examples of erupt attacks include vents. Examples of strike
 * attacks include charged clouds.
 *
 * Because we can potentially have the whole level consisting of features of
 * this type, we maintain an array of 'nearby' grids, that is a list of all
 * grids out to an approximate distance of 20 away. We update the list of grids
 * whenever we move 5 or more distance away from the current location.
 *
 * As an efficiency gain, we only do the above, when we note such a level is
 * 'dyna_full', that is, we have tried adding a dyna_grid that would overflow
 * the array. In instances where we are not dyna_full, the dyna_grid array contains
 * every instance of a dynamic feature of some kind on the level. Once a level
 * has been noted as being 'dyna_full', we do not reset this information until
 * the start of level generation on a new level.
 *
 * We also update this list by adding and removing features that are either
 * dynamic as they are created/destroyed. However, this requires that we create
 * a temporary copy of this list when we are applying the attacks in
 * this function.
 * This may prove not to be such an efficiency gain.
 */
void update_dyna(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dist, y, x, g, i;

	int dam, flg;

	int fast_dyna_n = dyna_n;
	u16b *fast_dyna_g = dyna_g;

	u16b temp_dyna_g[DYNA_MAX];

	s16b feat;

	feature_type *f_ptr;

	bool full = FALSE;

	/* Must be able to modify features */
	if (!variant_hurt_feats) return;

	/* Efficiency */
	if (dyna_full)
	{
		dist = ABS(p_ptr->py - dyna_cent_y);
		if (ABS(p_ptr->px - dyna_cent_x) > dist)
			dist = ABS(p_ptr->px - dyna_cent_x);

		/*
		 * Character is far enough away from the previous dyna center - 
		 * do a full rebuild.
		 */
		if (dist >= 5) full = TRUE;
	}

	/* Fully update */
	if (full)
	{
		int min_y = MAX(0, py - MAX_SIGHT);
		int max_y = MIN(DUNGEON_HGT, py + MAX_SIGHT + 1);
		int min_x = MAX(0, px - MAX_SIGHT);
		int max_x = MIN(DUNGEON_WID, px + MAX_SIGHT + 1);

		fast_dyna_n = 0;

		/* Scan the potentially visible area of the dungeon */
		for (y = min_y; y < max_y; y++)
		{
			for (x = min_x; x < max_x; x++)
			{
				/* Check distance */
				if (distance(y,x,py,px) > MAX_SIGHT) continue;

				/* Get grid feat */
				feat = cave_feat[y][x];

				/* Get the feature */
				f_ptr = &f_info[feat];

				/* Check for dynamic */
				if (f_ptr->flags3 & (FF3_DYNAMIC_MASK))
				{
					fast_dyna_g[fast_dyna_n++] = GRID(y,x);
				}
			}
		}

		dyna_cent_y = py;
		dyna_cent_x = px;
		dyna_n = fast_dyna_n;
	}

	/* Actually apply the attacks */
	for (i = 0; i < fast_dyna_n; i++)
	{
		temp_dyna_g[i] = fast_dyna_g[i];
	}

	/* Actually apply the attacks */
	for (i = 0; i < fast_dyna_n; i++)
	{
		feature_type *f_ptr;

		/* Grid */
		g = fast_dyna_g[i];

		/* Coordinates */
		y = GRID_Y(g);
		x = GRID_X(g);

		/* Get grid feat */
		feat = cave_feat[y][x];

		/* Get the feature */
		f_ptr = &f_info[feat];

		/* Instant */
		if (f_ptr->flags3 & (FF3_INSTANT))
		{
			cave_alter_feat(y,x,FS_INSTANT);
		}

		/* Explode */
		if (f_ptr->flags3 & (FF3_EXPLODE))
		{
			flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

			dam = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice) * 10;
   
			/* Apply the blow */
			project(0, 1, y, x, dam, f_ptr->blow.effect, flg);
		}

		/* Timed */
		if (f_ptr->flags3 & (FF3_TIMED))
		{
			if (!(rand_int(50)))
			{
				cave_alter_feat(y,x,FS_TIMED);
			}
		}

		/* Erupt */
		if (f_ptr->flags3 & (FF3_ERUPT))
		{
			if (!(rand_int(20)))
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				dam = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice) * 2;
   
				/* Apply the blow */
				project(0, 2, y, x, dam, f_ptr->blow.effect, flg);
			}
		}

		/* Strike */
		if (f_ptr->flags3 & (FF3_STRIKE))
		{
			if (!(rand_int(20)))
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				dam = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice) * 10;
   
				/* Apply the blow */
				project(0, 0, y, x, dam, f_ptr->blow.effect, flg);
			}
		}

		/* Dynamic */
		if (f_ptr->flags3 & (FF3_SPREAD))
		{
			int dir = rand_int(12);

			if (dir < 8)
			{
				int yy = y + ddy_ddd[dir];
				int xx = x + ddx_ddd[dir];

				int adjfeat = cave_feat[yy][xx];

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				dam = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice);
   
				/* Apply the blow */
				project(0, 0, yy, xx, dam, f_ptr->blow.effect, flg);

				/* Hack -- require smoke/acid clouds to move */
				if (adjfeat == cave_feat[yy][xx]) dir = 12;
			}

			if (dir < 9)
			{
				cave_alter_feat(y,x,FS_SPREAD);
			}
		}
	}
}



/*
 * Every so often, the character makes enough noise that nearby 
 * monsters can use it to home in on him.
 *
 * Fill in the "cave_cost" field of every grid that the player can 
 * reach with the number of steps needed to reach that grid.  This 
 * also yields the route distance of the player from every grid.
 *
 * Monsters use this information by moving to adjacent grids with 
 * lower flow costs, thereby homing in on the player even though 
 * twisty tunnels and mazes.  Monsters can also run away from loud 
 * noises.
 *
 * The biggest limitation of this code is that it does not easily 
 * allow for alternate ways around doors (not all monsters can handle 
 * doors) and lava/water (many monsters are not allowed to enter 
 * water, lava, or both).
 *
 * The flow table is three-dimensional.  The first dimension allows the 
 * table to both store and overwrite grids safely.  The second indicates 
 * whether this value is that for x or for y.  The third is the number 
 * of grids able to be stored at any flow distance.
 */
void update_noise(void)
{
#ifdef MONSTER_FLOW
	int cost;
	int route_distance = 0;

	int i, d;
	int y, x, y2, x2;
	int last_index;
	int grid_count = 0;

	int dist;
	bool full = FALSE;

	/* Note where we get information from, and where we overwrite */
	int this_cycle = 0;
	int next_cycle = 1;

	byte flow_table[2][2][8 * NOISE_STRENGTH];

	/* The character's grid has no flow info.  Do a full rebuild. */
	if (cave_cost[p_ptr->py][p_ptr->px] == 0) full = TRUE;

	/* Determine when to rebuild, update, or do nothing */
	if (!full)
	{
		dist = ABS(p_ptr->py - flow_center_y);
		if (ABS(p_ptr->px - flow_center_x) > dist)
			dist = ABS(p_ptr->px - flow_center_x);

		/*
		 * Character is far enough away from the previous flow center - 
		 * do a full rebuild.
		 */
		if (dist >= 15) full = TRUE;

		else
		{
			/* Get axis distance to center of last update */
			dist = ABS(p_ptr->py - update_center_y);
			if (ABS(p_ptr->px - update_center_x) > dist)
				dist = ABS(p_ptr->px - update_center_x);

			/*
			 * We probably cannot decrease the center cost any more.
			 * We should assume that we have to do a full rebuild.
			 */
			if (cost_at_center - (dist + 5) <= 0) full = TRUE;


			/* Less than five grids away from last update */
			else if (dist < 5)
			{
				/* We're in LOS of the last update - don't update again */
				if (los(p_ptr->py, p_ptr->px, update_center_y, 
		 		    update_center_x)) return;

				/* We're not in LOS - update */
				else full = FALSE;
			}

			/* Always update if at least five grids away */
			else full = FALSE;
		}
	}

	/* Update */
	if (!full)
	{
		bool found = FALSE;

		/* Start at the character's location */
		flow_table[this_cycle][0][0] = p_ptr->py;
		flow_table[this_cycle][1][0] = p_ptr->px;
		grid_count = 1;

		/* Erase outwards until we hit the previous update center */
		for (cost = 0; cost <= NOISE_STRENGTH; cost++)
		{
			/*
			 * Keep track of the route distance to the previous 
			 * update center.
			 */
			route_distance++;


			/* Get the number of grids we'll be looking at */
			last_index = grid_count;

			/* Clear the grid count */
			grid_count = 0;

			/* Get each valid entry in the flow table in turn */
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

					/* Ignore illegal grids */
					if (cave_cost[y2][x2] == 0) continue;

					/* Ignore previously erased grids */
					if (cave_cost[y2][x2] == 255) continue;

					/* Erase previous info, mark grid */
					cave_cost[y2][x2] = 255;

					/* Store this grid in the flow table */
					flow_table[next_cycle][0][grid_count] = y2;
					flow_table[next_cycle][1][grid_count] = x2;

					/* Increment number of grids stored */
					grid_count++;

					/* If this is the previous update center, we can stop */
					if ((y2 == update_center_y) && 
						(x2 == update_center_x)) found = TRUE;
				}
			}

			/* Stop when we find the previous update center. */
			if (found) break;


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

		/*
		 * Reduce the flow cost assigned to the new center grid by 
		 * enough to maintain the correct cost slope out to the range 
		 * we have to update the flow.
		 */
		cost_at_center -= route_distance;

		/* We can't reduce the center cost any more.  Do a full rebuild. */
		if (cost_at_center < 0) full = TRUE;

		else
		{
			/* Store the new update center */
			update_center_y = p_ptr->py;
			update_center_x = p_ptr->px;
		}
	}


	/* Full rebuild */
	if (full)
	{
		/*
		 * Set the initial cost to 100; updates will progressively 
		 * lower this value.  When it reaches zero, another full 
		 * rebuild has to be done.
		 */
		cost_at_center = 100;

		/* Save the new noise epicenter */
		flow_center_y = p_ptr->py;
		flow_center_x = p_ptr->px;
		update_center_y = p_ptr->py;
		update_center_x = p_ptr->px;


		/* Erase all of the current flow (noise) information */
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				cave_cost[y][x] = 0;
			}
		}
	}


	/*** Update or rebuild the flow ***/


	/* Store base cost at the character location */
	cave_cost[p_ptr->py][p_ptr->px] = cost_at_center;

	/* Store this grid in the flow table, note that we've done so */
	flow_table[this_cycle][0][0] = p_ptr->py;
	flow_table[this_cycle][1][0] = p_ptr->px;
	grid_count = 1;

	/* Extend the noise burst out to its limits */
	for (cost = cost_at_center + 1; cost <= cost_at_center + NOISE_STRENGTH; cost++)
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

				/* When doing a rebuild... */
				if (full)
				{
					/* Ignore previously marked grids */
					if (cave_cost[y2][x2]) continue;

					/* Ignore walls.  Do not ignore rubble. */
					if (f_info[cave_feat[y2][x2]].flags1 & (FF1_WALL))
					{
						continue;
					}
				}

				/* When doing an update... */
				else
				{
					/* Ignore all but specially marked grids */
					if (cave_cost[y2][x2] != 255) continue;
				}

				/* Store cost at this location */
				cave_cost[y2][x2] = cost;

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

#endif
}


/*
 * Characters leave scent trails for perceptive monsters to track.
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
#ifdef MONSTER_FLOW

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
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
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
			/* Translate table to map grids */
			y = i + py - 2;
			x = j + px - 2;

			/* Check Bounds */
			if (!in_bounds(y, x)) continue;

			/* Walls, water, and lava cannot hold scent. */
			if ((f_info[cave_feat[y][x]].flags1 & (FF1_WALL)) || 
			    (f_info[cave_feat[y][x]].flags2 & (FF2_SHALLOW)) ||
			    (f_info[cave_feat[y][x]].flags2 & (FF2_DEEP)) ||
			    (f_info[cave_feat[y][x]].flags2 & (FF2_FILLED)))
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

#endif
}




/*
 * Map the current panel (plus some) ala "magic mapping"
 *
 * We must never attempt to map the outer dungeon walls, or we
 * might induce illegal cave grid references.
 */
void map_area(void)
{
	int i, x, y, y1, y2, x1, x2;


	/* Pick an area to map */
	y1 = p_ptr->wy - randint(10);
	y2 = p_ptr->wy+SCREEN_HGT + randint(10);
	x1 = p_ptr->wx - randint(20);
	x2 = p_ptr->wx+SCREEN_WID + randint(20);

	/* Efficiency -- shrink to fit legal bounds */
	if (y1 < 1) y1 = 1;
	if (y2 > DUNGEON_HGT-1) y2 = DUNGEON_HGT-1;
	if (x1 < 1) x1 = 1;
	if (x2 > DUNGEON_WID-1) x2 = DUNGEON_WID-1;

	/* Scan that area */
	for (y = y1; y < y2; y++)
	{
		for (x = x1; x < x2; x++)
		{
			/* All non-walls are "checked" */
			if (!(f_info[cave_feat[y][x]].flags1 & (FF1_WALL)))
			{ 

				/* Memorize normal features */
				if (f_info[cave_feat[y][x]].flags1 & (FF1_REMEMBER))
				{
					/* Memorize the object */
					cave_info[y][x] |= (CAVE_MARK);
				}

				/* Memorize known walls */
				for (i = 0; i < 8; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Memorize walls (etc) */
					if (f_info[cave_feat[yy][xx]].flags1 & (FF1_REMEMBER))
					{
						/* Memorize the walls */
						cave_info[yy][xx] |= (CAVE_MARK);
					}
				}
			}
		}
	}

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



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
void wiz_lite(void)
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
	for (y = 1; y < DUNGEON_HGT-1; y++)
	{
		/* Scan all normal grids */
		for (x = 1; x < DUNGEON_WID-1; x++)
		{
			/* Process all non-walls */
			if (!(f_info[cave_feat[y][x]].flags1 & (FF1_WALL)))
			{
				/* Scan all neighbors */
				for (i = 0; i < 9; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Perma-lite the grid */
					cave_info[yy][xx] |= (CAVE_GLOW);

					/* Memorize normal features */
					if (f_info[cave_feat[yy][xx]].flags1 & (FF1_REMEMBER))
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
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(void)
{
	int i, y, x;


	/* Forget every grid */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Process the grid */
			cave_info[y][x] &= ~(CAVE_MARK);
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
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * Light or Darken the town
 */
void town_illuminate(bool daytime)
{
	int y, x, i;

	town_type *t_ptr = &t_info[p_ptr->dungeon];

	dungeon_zone *zone=&t_ptr->zone[0];

	int dungeon_hgt, dungeon_wid;

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	dungeon_hgt = (!(zone->fill)) ? TOWN_HGT : DUNGEON_HGT;
	dungeon_wid = (!(zone->fill)) ? TOWN_WID : DUNGEON_WID;

	/* Apply light or darkness */
	for (y = 0; y < dungeon_hgt; y++)
	{
		for (x = 0; x < dungeon_wid; x++)
		{
			/* Don't affect indoors */
			if (!(f_info[cave_feat[y][x]].flags3 & (FF3_OUTSIDE)))
			{
				if ((daytime) && (cave_info[y][x] & (CAVE_WALL)))
				{
					for (i = 0; i < 8; i++)
					{
						int yy = y + ddy_ddd[i];
						int xx = x + ddx_ddd[i];

						/* Ignore annoying locations */
						if (!in_bounds_fully(yy, xx)) continue;

						if (f_info[cave_feat[yy][xx]].flags3 & (FF3_OUTSIDE))
						{
							/* Illuminate the grid */
							cave_info[y][x] |= (CAVE_GLOW);
						}
					}
				}
			}

			/* Boring grids (light) */
			else if (daytime)
			{
				/* Illuminate the grid */
				cave_info[y][x] |= (CAVE_GLOW);
			}

			/* Boring grids (dark) */
			else
			{
				/* Darken the grid */
				if (!(f_info[cave_feat[y][x]].flags2 & (FF2_GLOW))) cave_info[y][x] &= ~(CAVE_GLOW);

				for (i = 0; i < 8; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Ignore annoying locations */
					if (!in_bounds_fully(yy, xx)) continue;

					if (f_info[cave_feat[yy][xx]].flags2 & (FF2_GLOW))
					{
						/* Illuminate the grid */
						cave_info[y][x] |= (CAVE_GLOW);
					}
				}
			}

			/* Reveal it */
			lite_spot(y,x);
		}
	}

	/* Megahack --- darkness brings out the bad guys */
	if ((!daytime) && (zone->guard) && (r_info[zone->guard].cur_num <= 0))
	{
		int y, x;

		/* Pick a location */
		while (1)
		{
			y = rand_int(dungeon_hgt);
			x = rand_int(dungeon_wid);

			if (cave_naked_bold(y, x)) break;
		}

		/* Place the questor */
		place_monster_aux(y, x, zone->guard, TRUE, TRUE);
	}

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * Hack -- Really change the feature
 */
static void cave_set_feat_aux(int y, int x, int feat)
{
	/* Set if blocks los */
	bool los = cave_floor_bold(y,x);

	/* Get feature */
	feature_type *f_ptr = &f_info[feat];

	/* Set if dynamic */
	bool dyna = (f_info[cave_feat[y][x]].flags3 & (FF3_DYNAMIC_MASK)) != 0;

	bool hide_item = (f_info[cave_feat[y][x]].flags2 & (FF2_HIDE_ITEM)) != 0;

	s16b this_o_idx, next_o_idx = 0;

	/* Really set the feature */
	cave_feat[y][x] = feat;

	/* Check for bit 5 set*/
	if (f_ptr->flags1 & (FF1_LOS))
	{
		cave_info[y][x] &= ~(CAVE_WALL);
	}

	/* Handle wall grids */
	else
	{
		cave_info[y][x] |= (CAVE_WALL);
	}

	/* Check for change to boring grid */
	if (!(f_ptr->flags1 & (FF1_REMEMBER))) cave_info[y][x] &= ~(CAVE_MARK);

	/* Check for change to out of sight grid */
	else if (!(player_can_see_bold(y,x))) cave_info[y][x] &= ~(CAVE_MARK);

	/* Check if adding to dynamic list */
	if (!dyna && (f_ptr->flags3 & (FF3_DYNAMIC_MASK)))
	{
		if (dyna_full)
		{
			if (distance(y,x,p_ptr->py,p_ptr->px) < MAX_SIGHT)
			{
				dyna_g[dyna_n++] = GRID(y,x);
			}
		}
		else if (dyna_n == (DYNA_MAX -1))
		{
			dyna_full = TRUE;

			/* Hack to force rebuild */
			dyna_cent_y = 255;
			dyna_cent_x = 255;
		}
		else
		{
			dyna_g[dyna_n++] = GRID(y,x);			
		}
	}
	/* Check if removing from dynamic list */
	else if (dyna && !(f_ptr->flags3 & (FF3_DYNAMIC_MASK)))
	{
		int i, new_i = 0;

		for (i = 0; i < dyna_n; i++)
		{
			if (dyna_g[i] == GRID(y,x)) continue;

			dyna_g[new_i++] = dyna_g[i];
		}

		dyna_n = new_i;
	}

	/* Check to see if monster exposed by change */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		bool hidden = ((m_ptr->mflag & (MFLAG_HIDE)) != 0);

		if (hidden)
		{
			monster_hide(y,x,place_monster_here(y,x,m_ptr->r_idx), m_ptr);

			if (!(m_ptr->mflag & (MFLAG_HIDE)))
			{
				/* And update */
				update_mon(cave_m_idx[y][x],FALSE);

		/* Hack --- tell the player if something unhides */
		if (m_ptr->ml)
		{
		char m_name[80];

		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		msg_format("%^s emerges from %s%s.",m_name,
			((f_info[cave_feat[m_ptr->fy][m_ptr->fx]].flags2 & (FF2_FILLED))?"":"the "),
			f_name+f_info[cave_feat[m_ptr->fy][m_ptr->fx]].name);
		}

				/* Disturb on "move" */
				if (m_ptr->ml &&
				    (disturb_move ||
				     ((m_ptr->mflag & (MFLAG_VIEW)) &&
				      disturb_near)))
				{
					/* Disturb */
					disturb(0, 0);
				}
			}
		}
	}

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hide stuff */
		if ((!hide_item) && (f_ptr->flags2 & (FF2_HIDE_ITEM)))
		{
			/* Hide it */
			o_ptr->marked = FALSE;
		}
	}

	/* Check if los has changed */
	if ((los) && (player_has_los_bold(y,x)) && !(cave_floor_bold(y,x)))
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}
	else if ((!los) && (player_has_los_bold(y,x)) && (cave_floor_bold(y,x)))
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	if (player_has_los_bold(y,x))
	{
		/* Notice */
		note_spot(y, x);

		/* Redraw */
		lite_spot(y, x);
	}

}


/*
 * Change the "feat" flag for a grid, and notice/redraw the grid
 */
void cave_set_feat(int y, int x, int feat)
{
	int i,ii;

	int feat2;

	feature_type *f_ptr = &f_info[cave_feat[y][x]];
	feature_type *f_ptr2 = &f_info[feat];

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));
	bool daytime = (((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)));

	bool glow = (f_ptr->flags2 & (FF2_GLOW)) !=0;
	bool glow2 = (f_ptr2->flags2 & (FF2_GLOW)) !=0;

	bool wall = (f_ptr->flags1 & (FF1_WALL)) !=0;
	bool wall2 = (f_ptr2->flags1 & (FF1_WALL)) != 0;

	bool tree = (f_ptr->flags3 & (FF3_TREE_BIG)) !=0;
	bool tree2 = (f_ptr2->flags3 & (FF3_TREE_BIG)) != 0;

	bool dayt = ((f_ptr->flags3 & (FF3_OUTSIDE)) !=0) && (daytime) && (surface);
	bool dayt2 = ((f_ptr2->flags3 & (FF3_OUTSIDE)) !=0) && (daytime) && (surface);

	/* Change the feature */
	cave_set_feat_aux(y,x,feat);

	if (glow && !glow2)
	{
		/* Darken temporarily */
		if (!dayt2) cave_info[y][x] &= ~(CAVE_GLOW);

		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			bool glow3;

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			glow3 = (f_info[cave_feat[yy][xx]].flags2 & (FF2_GLOW)) != 0;

			if (glow3)
			{
				/* Illuminate the grid */
				cave_info[y][x] |= (CAVE_GLOW);
			}

			/* Was destroyed grid only one illuminating this one? */
			else if (cave_info[yy][xx] & (CAVE_GLOW))
			{
				/* Darken temporarily */
				if (!daytime || !surface || !(f_ptr2->flags3 & (FF3_OUTSIDE))) cave_info[yy][xx] &= ~(CAVE_GLOW);

				for (ii = 0; ii < 8; ii++)
				{
					int yyy = yy + ddy_ddd[ii];
					int xxx = xx + ddx_ddd[ii];

					bool glow4;

					/* Ignore annoying locations */
					if (!in_bounds_fully(yyy, xxx)) continue;

					glow4 = (f_info[cave_feat[yyy][xxx]].flags2 & (FF2_GLOW)) != 0;

					if (glow4)
					{
						/* Illuminate the grid */
						cave_info[yy][xx] |= (CAVE_GLOW);
					}
				}
			}

			/* Notice change */
			if ((view_glowing_lite) || !(cave_info[yy][xx] & (CAVE_GLOW)))
			{
				note_spot(yy,xx);

				lite_spot(yy,xx);
			}
		}

		/* Notice change */
		if ((view_glowing_lite) || !(cave_info[y][x] & (CAVE_GLOW)))
		{
			note_spot(y,x);

			lite_spot(y,x);
		}
	}
	else if (dayt && !dayt2)
	{
		/* Darken temporarily */
		cave_info[y][x] &= ~(CAVE_GLOW);

		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			bool glow3;

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			glow3 = (f_info[cave_feat[yy][xx]].flags2 & (FF2_GLOW)) != 0;

			if (glow3)
			{
				/* Illuminate the grid */
				cave_info[y][x] |= (CAVE_GLOW);
			}
		}

		/* Notice change */
		if (/*(view_glowing_lite) || */ !(cave_info[y][x] & (CAVE_GLOW)))
		{
			note_spot(y,x);

			lite_spot(y,x);
		}
	}

	/* Set the level type */
	if (f_ptr2->flags2 & (FF2_WATER)) level_flag |= (LF1_WATER);
	if (f_ptr2->flags2 & (FF2_LAVA)) level_flag |= (LF1_LAVA);
	if (f_ptr2->flags2 & (FF2_ICE)) level_flag |= (LF1_ICE);
	if (f_ptr2->flags2 & (FF2_ACID)) level_flag |= (LF1_ACID);
	if (f_ptr2->flags2 & (FF2_OIL)) level_flag |= (LF1_OIL);
	if (f_ptr2->flags2 & (FF2_CHASM)) level_flag |= (LF1_CHASM);

	/*
	 * ANDY - Handle creation of big trees.
	 */
	if (feat == FEAT_TREE_BIG)
	{
		for (i = 0; i < 8; i++)
		{
			int yy,xx;
	
			int k = rand_int(8);
	
			yy = y + ddy_ddd[k];
			xx = x + ddx_ddd[k];
	
			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;
	
			if (f_info[cave_feat[yy][xx]].flags3 & (FF3_GROUND))
			{
				cave_set_feat_aux(yy,xx,FEAT_TREE_SHADE);
			}
			else if (f_info[cave_feat[yy][xx]].flags2 & (FF2_CHASM))
			{
				cave_set_feat_aux(yy,xx,FEAT_TREE_SHADE_C);
			}
			else if (cave_feat[yy][xx] == FEAT_WALL_EXTRA)
			{
				cave_set_feat_aux(yy,xx,FEAT_TREE_SHADE_W);
			}
		}
	}
	else if (feat == FEAT_TREE_BIG_S)
	{
		for (i = 0; i < 8; i++)
		{
			int yy,xx;

			int k = rand_int(8);

			yy = y + ddy_ddd[k];
			xx = x + ddx_ddd[k];

     			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			if (f_info[cave_feat[yy][xx]].flags3 & (FF3_GROUND))
			{
				cave_set_feat_aux(yy,xx,FEAT_TREE_SHADE_S);
			}
		}
	}


	/* Handle NEED_WALL/ NEED_TREE locations */
	if ((wall && !wall2) || (tree && !tree2))
	{
		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			int feat2;

			bool need_wall, need_tree;

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			feat2 = cave_feat[yy][xx];

			need_wall = (f_info[feat2].flags3 & (FF3_NEED_WALL));
			need_tree = (f_info[feat2].flags3 & (FF3_NEED_TREE));

			/* Was destroyed grid only one holding this one up */
			if ((wall && !wall2 && need_wall) || (tree && !wall2 && need_tree))
			{
				/* Collapse temporarily */
				feat2 = feat_state(cave_feat[yy][xx],FS_TUNNEL);

				for (ii = 0; ii < 8; ii++)
				{
					int yyy = yy + ddy_ddd[ii];
					int xxx = xx + ddx_ddd[ii];

					bool wall4, tree4;

					/* Ignore annoying locations */
					if (!in_bounds_fully(yyy, xxx)) continue;

					wall4 = (f_info[cave_feat[yyy][xxx]].flags1 & (FF1_WALL)) !=0;
					tree4 = (f_info[cave_feat[yyy][xxx]].flags3 & (FF3_TREE_BIG)) !=0;

					if (wall && !wall2 && need_wall && wall4 && (rand_int(100) < 50)) feat2 = cave_feat[yy][xx];

					if (tree && !tree2 && need_tree && tree4 && (rand_int(100) < 50)) cave_feat[yy][xx] = feat2;
					feat2 = cave_feat[yy][xx];
				}
			}

			/* Affect the grid */
			cave_set_feat_aux(yy,xx,feat2);

		}
	}

	/*
	 * ANDY - Handle removal of orphaned chasm edges. This is a pretting
	 * up function, and not necessary, but it makes chasms look a lot
	 * better. I prefer this here, than generate.c because it matches the
	 * algorithm for creation and destruction of glow features.
	 */
	if (f_ptr2->flags2 & (FF2_CHASM))
	{
		if (feat == FEAT_CHASM) feat = FEAT_CHASM_E;

		cave_set_feat_aux(y,x,FEAT_CHASM);

		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx))
			{
				/* Restore the grid */
				cave_set_feat_aux(y,x,feat);

				continue;
			}

			if ((cave_feat[yy][xx] != FEAT_NONE) &&
			    !((f_info[cave_feat[yy][xx]].flags2 & (FF2_CHASM)) ||
			      (f_info[cave_feat[yy][xx]].flags2 & (FF2_BRIDGED))))
			{
				cave_set_feat_aux(yy,xx,feat);
			}
			/* Was destroyed grid only one edging for this one? */
			else if (f_info[cave_feat[yy][xx]].flags2 & (FF2_CHASM))
			{
				feat2 = cave_feat[yy][xx];

				if (feat2 == FEAT_CHASM) feat2 = FEAT_CHASM_E;

				/* Temporarily chasm the grid */
				cave_set_feat_aux(yy,xx,FEAT_CHASM);

				for (ii = 0; ii < 8; ii++)
				{
					int yyy = yy + ddy_ddd[ii];
					int xxx = xx + ddx_ddd[ii];

					/* Ignore annoying locations */
					if (!in_bounds_fully(yyy, xxx))
					{
						/* Restore the grid */
						cave_set_feat_aux(yy,xx,feat2);

						continue;
					}

					if ((cave_feat[yy][xx] != FEAT_NONE) &&
					    !((f_info[cave_feat[yyy][xxx]].flags2 & (FF2_CHASM))||
					     (f_info[cave_feat[yyy][xxx]].flags2 & (FF2_BRIDGED))))
					{
						/* Restore the grid */
						cave_set_feat_aux(yy,xx,feat2);
					}
				}
			}
		}
	}
	else if (!(f_ptr2->flags2 & (FF2_BRIDGED)))
	{
		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

     			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			if (f_info[cave_feat[yy][xx]].flags2 == FEAT_CHASM)
			{
				/* Restore the grid */
				cave_set_feat_aux(yy,xx,FEAT_CHASM_E);
			}
		}
	}

	/* Handle creating 'GLOW' */
	if (f_ptr2->flags2 & FF2_GLOW)
	{
		if ((view_glowing_lite) || !(cave_info[y][x] & (CAVE_GLOW)))
		{
			/* Illuminate the grid */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Notice */
			note_spot(y, x);

			/* Redraw */
			lite_spot(y, x);
		}

		for (i = 0; i < 8; i++)
		{
			int yy = y + ddy_ddd[i];
			int xx = x + ddx_ddd[i];

			/* Ignore annoying locations */
			if (!in_bounds_fully(yy, xx)) continue;

			/* Notice/Redraw */
			if ((view_glowing_lite) || !(cave_info[yy][xx] & (CAVE_GLOW)))
			{
				/* Illuminate the grid */
				cave_info[yy][xx] |= (CAVE_GLOW);

				/* Notice */
				note_spot(yy, xx);

				/* Redraw */
				lite_spot(yy, xx);
			}
		}    
	}
	/* Handle creating 'OUTSIDE' */
	else if ( daytime && surface && (f_ptr2->flags3 & (FF3_OUTSIDE)))
	{
		if ((view_glowing_lite) || !(cave_info[y][x] & (CAVE_GLOW)))
		{
			/* Illuminate the grid */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Notice */
			note_spot(y, x);

			/* Redraw */
			lite_spot(y, x);
		}
	}

	/* Handle gold/items */
	if (((f_ptr->flags1 & FF1_HAS_ITEM) && !(f_ptr2->flags1 & (FF1_HAS_ITEM)))
		|| ((f_ptr->flags1 & FF1_HAS_GOLD) && !(f_ptr2->flags1 & (FF1_HAS_GOLD))))
	{
		int number = 0;
		int j;

		bool good = (f_ptr->flags3 & (FF3_DROP_GOOD)) ? TRUE : FALSE;
		bool great = (f_ptr->flags3 & (FF3_DROP_GREAT)) ? TRUE : FALSE;

		bool do_gold = (!(f_ptr->flags1 & (FF1_HAS_ITEM)));
		bool do_item = (!(f_ptr->flags1 & (FF1_HAS_GOLD)));

		object_type *i_ptr;
		object_type object_type_body;

		if (f_ptr->flags1 & (FF3_DROP_1D2)) number += damroll(1, 2);
		if (f_ptr->flags1 & (FF3_DROP_2D2)) number += damroll(2, 2);

		/* Always drop something */
		if (!number) number = 1;

		/* Drop some objects */
		for (j = 0; j < number; j++)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make Gold */
			if (do_gold && (!do_item || (rand_int(100) < 50)))
			{
				/* Make some gold */
				if (!make_gold(i_ptr)) continue;
			}

			/* Make Object */
			else
			{
				/* Make an object */
				if (!make_object(i_ptr, good, great)) continue;
			}

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, y, x);
		}

	}
}


/*
 * Take a feature, determine what that feature becomes
 * through applying the given action.
 */
int feat_state(int feat, int action)
{
	int newfeat=feat;
	int i;

	/* Permanent stuff never gets changed */
	if (f_info[feat].flags1 & FF1_PERMANENT) return (feat);

	/* Set default feat */
	newfeat = f_info[feat].defaults;

	/* Get the new feature */
	for (i=0;i<MAX_FEAT_STATES;i++)
	{
		if (f_info[feat].state[i].action == action)
		{
			newfeat = f_info[feat].state[i].result;
			break;
		}
	}

	/* No change in state */
	return (newfeat);
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

	/* Get the new feat */
	newfeat = feat_state(cave_feat[y][x],action);

	/* Invisible trap */
	if (newfeat == oldfeat)
	{
		if (f_info[oldfeat].flags3 & (FF3_PICK_TRAP))
		{
			/* Pick a trap */
			pick_trap(y, x);

			/* Update new feature */
			newfeat = cave_feat[y][x];

			/* Disturb */
			disturb(0, 0);
		}
		else if (f_info[oldfeat].flags3 & (FF3_PICK_DOOR))
		{
			/* Pick a trap */
			pick_door(y, x);

			/* Update new feature */
			newfeat = cave_feat[y][x];

			/* Disturb */
			disturb(0, 0);
		}
		else
		{
			return;
		}
	}

	/* Other stuff */
	else
	{

		/* Set the new feature */
		cave_set_feat(y,x,newfeat);
	}

	/* Notice */
	note_spot(y, x);

	/* Redraw */
	lite_spot(y, x);

}


/*
 * Determine the path taken by a projection.
 *
 * The projection will always start from the grid (y1,x1), and will travel
 * towards the grid (y2,x2), touching one grid per unit of distance along
 * the major axis, and stopping when it enters the destination grid or a
 * wall grid, or has travelled the maximum legal distance of "range".
 *
 * Note that "distance" in this function (as in the "update_view()" code)
 * is defined as "MAX(dy,dx) + MIN(dy,dx)/2", which means that the player
 * actually has an "octagon of projection" not a "circle of projection".
 *
 * The path grids are saved into the grid array pointed to by "gp", and
 * there should be room for at least "range" grids in "gp".  Note that
 * due to the way in which distance is calculated, this function normally
 * uses fewer than "range" grids for the projection path, so the result
 * of this function should never be compared directly to "range".  Note
 * that the initial grid (y1,x1) is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * The "flg" flags can be used to modify the behavior of this function.
 *
 * In particular, the "PROJECT_STOP" and "PROJECT_THRU" flags have the same
 * semantics as they do for the "project" function, namely, that the path
 * will stop as soon as it hits a monster, or that the path will continue
 * through the destination grid, respectively.
 *
 * The "PROJECT_JUMP" flag, which for the "project()" function means to
 * start at a special grid (which makes no sense in this function), means
 * that the path should be "angled" slightly if needed to avoid any wall
 * grids, allowing the player to "target" any grid which is in "view".
 * This flag is non-trivial and has not yet been implemented, but could
 * perhaps make use of the "vinfo" array (above).  XXX XXX XXX
 *
 * This function returns the number of grids (if any) in the path.  This
 * function will return zero if and only if (y1,x1) and (y2,x2) are equal.
 *
 * This algorithm is similar to, but slightly different from, the one used
 * by "update_view_los()", and very different from the one used by "los()".
 */
sint project_path(u16b *gp, int range, int y1, int x1, int y2, int x2, int flg)
{
	int y, x;

	int n = 0;
	int k = 0;

	/* Absolute */
	int ay, ax;

	/* Offsets */
	int sy, sx;

	/* Fractions */
	int frac;

	/* Scale factors */
	int full, half;

	/* Slope */
	int m;


	/* No path necessary (or allowed) */
	if ((x1 == x2) && (y1 == y2)) return (0);


	/* Analyze "dy" */
	if (y2 < y1)
	{
		ay = (y1 - y2);
		sy = -1;
	}
	else
	{
		ay = (y2 - y1);
		sy = 1;
	}

	/* Analyze "dx" */
	if (x2 < x1)
	{
		ax = (x1 - x2);
		sx = -1;
	}
	else
	{
		ax = (x2 - x1);
		sx = 1;
	}


	/* Number of "units" in one "half" grid */
	half = (ay * ax);

	/* Number of "units" in one "full" grid */
	full = half << 1;


	/* Vertical */
	if (ay > ax)
	{
		/* Start at tile edge */
		frac = ax * ax;

		/* Let m = ((dx/dy) * full) = (dx * dx * 2) = (frac * 2) */
		m = frac << 1;

		/* Start */
		y = y1 + sy;
		x = x1;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			/* Always stop at non-initial wall grids */
			if ((n > 0) && (!(f_info[cave_feat[y][x]].flags1 & (FF1_PROJECT)))) break;

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				/* Don't stop if monster is hidden */
				if ((n > 0) && (cave_m_idx[y][x] != 0))
				{
					/* Player */
					if (cave_m_idx[y][x] < 0)
					{
						break;										
					}
					/* Non-hidden monster */
					else if (!(m_list[cave_m_idx[y][x]].mflag & (MFLAG_HIDE)))
					{
						break;
					}
				}
			}

			/* Slant */
			if (m)
			{
				/* Advance (X) part 1 */
				frac += m;

				/* Horizontal change */
				if (frac >= half)
				{
					/* Advance (X) part 2 */
					x += sx;

					/* Advance (X) part 3 */
					frac -= full;

					/* Track distance */
					k++;
				}
			}

			/* Advance (Y) */
			y += sy;
		}
	}

	/* Horizontal */
	else if (ax > ay)
	{
		/* Start at tile edge */
		frac = ay * ay;

		/* Let m = ((dy/dx) * full) = (dy * dy * 2) = (frac * 2) */
		m = frac << 1;

		/* Start */
		y = y1;
		x = x1 + sx;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			/* Always stop at non-initial wall grids */
			if ((n > 0) && (!(f_info[cave_feat[y][x]].flags1 & (FF1_PROJECT)))) break;

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				/* Don't stop if monster is hidden */
				if ((n > 0) && (cave_m_idx[y][x] != 0))
				{
					/* Player */
					if (cave_m_idx[y][x] < 0)
					{
						break;										
					}
					/* Non-hidden monster */
					else if (!(m_list[cave_m_idx[y][x]].mflag & (MFLAG_HIDE)))
					{
						break;
					}
				}
			}

			/* Slant */
			if (m)
			{
				/* Advance (Y) part 1 */
				frac += m;

				/* Vertical change */
				if (frac >= half)
				{
					/* Advance (Y) part 2 */
					y += sy;

					/* Advance (Y) part 3 */
					frac -= full;

					/* Track distance */
					k++;
				}
			}

			/* Advance (X) */
			x += sx;
		}
	}

	/* Diagonal */
	else
	{
		/* Start */
		y = y1 + sy;
		x = x1 + sx;

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n++] = GRID(y,x);

			/* Hack -- Check maximum range */
			if ((n + (n >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			/* Always stop at non-initial wall grids */
			if ((n > 0) && (!(f_info[cave_feat[y][x]].flags1 & (FF1_PROJECT)))) break;

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				/* Don't stop if monster is hidden */
				if ((n > 0) && (cave_m_idx[y][x] != 0))
				{
					/* Player */
					if (cave_m_idx[y][x] < 0)
					{
						break;										
					}
					/* Non-hidden monster */
					else if (!(m_list[cave_m_idx[y][x]].mflag & (MFLAG_HIDE)))
					{
						break;
					}
				}
			}

			/* Advance (Y) */
			y += sy;

			/* Advance (X) */
			x += sx;
		}
	}


	/* Length */
	return (n);
}


/*
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming that no monster gets in the way,
 * using the "project_path()" function to check the projection path.
 *
 * Note that no grid is ever "projectable()" from itself.
 *
 * This function is used to determine if the player can (easily) target
 * a given grid, and if a monster can target the player.
 */
bool projectable(int y1, int x1, int y2, int x2)
{
	int y, x;

	int grid_n = 0;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, 0);

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in a wall grid */
	if (!(f_info[cave_feat[y][x]].flags1 & (FF1_PROJECT))) return (FALSE);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);

	/* Assume okay */
	return (TRUE);
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
		if (!in_bounds_fully(y, x)) continue;

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
	p_ptr->redraw |= (PR_HEALTH);
}



/*
 * Hack -- track the given monster race
 */
void monster_race_track(int r_idx)
{
	/* Save this monster ID */
	p_ptr->monster_race_idx = r_idx;

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);
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
 		if (center_player && run_avoid_center) verify_panel();

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Redraw the player */
		if (hidden_player)
		{
			int py = p_ptr->py;
			int px = p_ptr->px;

			/* Redraw player */
			lite_spot(py, px);
		}
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

	/* Flush the input if requested */
	if (flush_disturb) flush();

	/* Last disturbed */
	p_ptr->last_disturb = turn;

}




/*
 * Hack -- Check if a level is a "quest" level
 */
bool is_quest(int level)
{
	int i;

	/* Town is never a quest */
	if (!level) return (FALSE);

	/* Check quests */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		/* Check for quest */
		if (q_list[i].level == level) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}
