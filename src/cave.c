/* File: cave.c */

/* Purpose: low level dungeon routines -BEN- */


#include "angband.h"


/*
 * Support for Adam Bolt's tileset, lighting and transparency effects
 * by Robert Ruehlmann (rr9@angband.org)
 */


/*
 * Distance between two points via Newton-Raphson technique
 */
int distance (int y1, int x1, int y2, int x2)
{
	int dy = (y1 > y2) ? (y1 - y2) : (y2 - y1);
	int dx = (x1 > x2) ? (x1 - x2) : (x2 - x1);

	/* Squared distance */
	int target = (dy * dy) + (dx * dx);

	/* Approximate distance: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2 */
	int d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

	int err;

	/* Simple case */
	if (!dy || !dx) return d;

	while (1)
	{
		/* Approximate error */
		err = (target - d * d) / (2 * d);

		/* No error - we are done */
		if (!err) break;

		/* Adjust distance */
		d += err;
	}

	return d;
}


/*
 * A simple, fast, integer-based line-of-sight algorithm.  By Joseph Hall,
 * 4116 Brewster Drive, Raleigh NC 27606.  Email to jnh@ecemwl.ncsu.edu.
 *
 * Returns TRUE if a line of sight can be traced from (x1,y1) to (x2,y2).
 *
 * The LOS begins at the center of the tile (x1,y1) and ends at the center of
 * the tile (x2,y2).  If los() is to return TRUE, all of the tiles this line
 * passes through must be floor tiles, except for (x1,y1) and (x2,y2).
 *
 * We assume that the "mathematical corner" of a non-floor tile does not
 * block line of sight.
 *
 * Because this function uses (short) ints for all calculations, overflow may
 * occur if dx and dy exceed 90.
 *
 * Once all the degenerate cases are eliminated, the values "qx", "qy", and
 * "m" are multiplied by a scale factor "f1 = abs(dx * dy * 2)", so that
 * we can use integer arithmetic.
 *
 * We travel from start to finish along the longer axis, starting at the border
 * between the first and second tiles, where the y offset = .5 * slope, taking
 * into account the scale factor.  See below.
 *
 * Also note that this function and the "move towards target" code do NOT
 * share the same properties.  Thus, you can see someone, target them, and
 * then fire a bolt at them, but the bolt may hit a wall, not them.  However,
 * by clever choice of target locations, you can sometimes throw a "curve".
 *
 * Note that "line of sight" is not "reflexive" in all cases.
 *
 * Use the "projectable()" routine to test "spell/missile line of sight".
 *
 * Use the "update_view()" function to determine player line-of-sight.
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


	/* Paranoia -- require "safe" origin */
	/* if (!in_bounds(y1, x1)) return (FALSE); */


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
 * Determine if a given location may be "destroyed"
 *
 * Used by destruction spells, and for placing stairs, etc.
 */
bool cave_valid_bold(int y, int x)
{
	s16b this_o_idx, next_o_idx;


	/* Forbid perma-grids */
	if (cave_perma_bold(y, x)) return (FALSE);

	/* Check objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
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
static cptr image_monster_hack = \
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

/*
 * Hack -- Legal monster codes for IBM pseudo-graphics
 */
static cptr image_monster_hack_ibm = \
"ÉÑÖÜáàâäçéèêëíïñôöõúùûü°•®©™Ø∞≤≥¥µ∂∑∏æ«Ã—‘’◊Ÿ›ﬁﬂ‡·‚„ÂËÈÊÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘˙˚¸˝˛";


/*
 * Mega-Hack -- Hallucinatory monster
 */
static void image_monster(byte *ap, char *cp)
{
	int n = strlen(image_monster_hack);

	/* Random symbol from set above */
	if (use_graphics)
	{
		/* Normal graphics */
		if (!(streq(ANGBAND_SYS, "ibm")))
		{
			(*cp) = r_info[randint(MAX_R_IDX-2)].x_char;
			(*ap) = r_info[randint(MAX_R_IDX-2)].x_attr;
		}
		else
		/* IBM-pseudo graphics */
		{
			n = strlen(image_monster_hack_ibm);
			(*cp) = (image_monster_hack_ibm[rand_int(n)]);

			/* Random color */
			(*ap) = randint(15);
		}
	}
	else
	/* Text mode */
	{
		(*cp) = (image_monster_hack[rand_int(n)]);

		/* Random color */
		(*ap) = randint(15);
	}
}




/*
 * Hack -- Legal object codes
 */
static cptr image_object_hack = \
"?/|\\\"!$()_-=[]{},~";

static cptr image_object_hack_ibm = \
"ÄÅìîó§¶´≠Æ∫ªºΩ¿¡¬√ƒ»… ÀÕŒ”⁄Á";

/*
 * Mega-Hack -- Hallucinatory object
 */
static void image_object(byte *ap, char *cp)
{
	int n = strlen(image_object_hack);

	if (use_graphics)
	{
		if (!(streq(ANGBAND_SYS, "ibm")))
		{
			(*cp) = k_info[randint(MAX_K_IDX-1)].x_char;
			(*ap) = k_info[randint(MAX_K_IDX-1)].x_attr;
		}
		else
		{
			n = strlen(image_object_hack_ibm);
			(*cp) = (image_object_hack_ibm[rand_int(n)]);

			/* Random color */
			(*ap) = randint(15);
		}
	}
	else
	{
		(*cp) = (image_object_hack[rand_int(n)]);

		/* Random color */
		(*ap) = randint(15);
	}
}


/*
 * Hack -- Random hallucination
 */
static void image_random(byte *ap, char *cp)
{
	/* Normally, assume monsters */
	if (rand_int(100) < 75)
	{
		image_monster(ap, cp);
	}

	/* Otherwise, assume objects */
	else
	{
		image_object(ap, cp);
	}
}



/*
 * Extract the attr/char to display at the given (legal) map location
 *
 * Basically, we "paint" the chosen attr/char in several passes, starting
 * with any known "terrain features" (defaulting to darkness), then adding
 * any known "objects", and finally, adding any known "monsters".  This
 * is not the fastest method but since most of the calls to this function
 * are made for grids with no monsters or objects, it is fast enough.
 *
 * Note that this function, if used on the grid containing the "player",
 * will return the attr/char of the grid underneath the player, and not
 * the actual player attr/char itself, allowing a lot of optimization
 * in various "display" functions.
 *
 * Note that the "zero" entry in the feature/object/monster arrays are
 * used to provide "special" attr/char codes, with "monster zero" being
 * used for the player attr/char, "object zero" being used for the "stack"
 * attr/char, and "feature zero" being used for the "nothing" attr/char,
 * though this function makes use of only "feature zero".
 *
 * Note that monsters can have some "special" flags, including "ATTR_MULTI",
 * which means their color changes, and "ATTR_CLEAR", which means they take
 * the color of whatever is under them, and "CHAR_CLEAR", which means that
 * they take the symbol of whatever is under them.  Technically, the flag
 * "CHAR_MULTI" is supposed to indicate that a monster looks strange when
 * examined, but this flag is currently ignored.  All of these flags are
 * ignored if the "avoid_other" option is set, since checking for these
 * conditions is expensive and annoying on some systems.
 *
 * Currently, we do nothing with multi-hued objects, because there are
 * not any.  If there were, they would have to set "shimmer_objects"
 * when they were created, and then new "shimmer" code in "dungeon.c"
 * would have to be created handle the "shimmer" effect, and the code
 * in "cave.c" would have to be updated to create the shimmer effect.
 *
 * Note the effects of hallucination.  Objects always appear as random
 * "objects", monsters as random "monsters", and normal grids occasionally
 * appear as random "monsters" or "objects", but note that these random
 * "monsters" and "objects" are really just "colored ascii symbols".
 *
 * Note that "floors" and "invisible traps" (and "zero" features) are
 * drawn as "floors" using a special check for optimization purposes,
 * and these are the only features which get drawn using the special
 * lighting effects activated by "view_special_lite".
 *
 * Note the use of the "mimic" field in the "terrain feature" processing,
 * which allows any feature to "pretend" to be another feature.  This is
 * used to "hide" secret doors, and to make all "doors" appear the same,
 * and all "walls" appear the same, and "hidden" treasure stay hidden.
 * It is possible to use this field to make a feature "look" like a floor,
 * but the "special lighting effects" for floors will not be used.
 *
 * Note the use of the new "terrain feature" information.  Note that the
 * assumption that all interesting "objects" and "terrain features" are
 * memorized allows extremely optimized processing below.  Note the use
 * of separate flags on objects to mark them as memorized allows a grid
 * to have memorized "terrain" without granting knowledge of any object
 * which may appear in that grid.
 *
 * Note the efficient code used to determine if a "floor" grid is
 * "memorized" or "viewable" by the player, where the test for the
 * grid being "viewable" is based on the facts that (1) the grid
 * must be "lit" (torch-lit or perma-lit), (2) the grid must be in
 * line of sight, and (3) the player must not be blind, and uses the
 * assumption that all torch-lit grids are in line of sight.
 *
 * Note that floors (and invisible traps) are the only grids which are
 * not memorized when seen, so only these grids need to check to see if
 * the grid is "viewable" to the player (if it is not memorized).  Since
 * most non-memorized grids are in fact walls, this induces *massive*
 * efficiency, at the cost of *forcing* the memorization of non-floor
 * grids when they are first seen.  Note that "invisible traps" are
 * always treated exactly like "floors", which prevents "cheating".
 *
 * Note the "special lighting effects" which can be activated for floor
 * grids using the "view_special_lite" option (for "white" floor grids),
 * causing certain grids to be displayed using special colors.  If the
 * player is "blind", we will use "dark gray", else if the grid is lit
 * by the torch, and the "view_yellow_lite" option is set, we will use
 * "yellow", else if the grid is "dark", we will use "dark gray", else
 * if the grid is not "viewable", and the "view_bright_lite" option is
 * set, and the we will use "slate" (gray).  We will use "white" for all
 * other cases, in particular, for illuminated viewable floor grids.
 *
 * Note the "special lighting effects" which can be activated for wall
 * grids using the "view_granite_lite" option (for "white" wall grids),
 * causing certain grids to be displayed using special colors.  If the
 * player is "blind", we will use "dark gray", else if the grid is lit
 * by the torch, and the "view_yellow_lite" option is set, we will use
 * "yellow", else if the "view_bright_lite" option is set, and the grid
 * is not "viewable", or is "dark", or is glowing, but not when viewed
 * from the player's current location, we will use "slate" (gray).  We
 * will use "white" for all other cases, in particular, for correctly
 * illuminated viewable wall grids.
 *
 * Note that, when "view_granite_lite" is set, we use an inline version
 * of the "player_can_see_bold()" function to check the "viewability" of
 * grids when the "view_bright_lite" option is set, and we do NOT use
 * any special colors for "dark" wall grids, since this would allow the
 * player to notice the walls of illuminated rooms from a hallway that
 * happened to run beside the room.  The alternative, by the way, would
 * be to prevent the generation of hallways next to rooms, but this
 * would still allow problems when digging towards a room.
 *
 * Note that bizarre things must be done when the "attr" and/or "char"
 * codes have the "high-bit" set, since these values are used to encode
 * various "special" pictures in some versions, and certain situations,
 * such as "multi-hued" or "clear" monsters, cause the attr/char codes
 * to be "scrambled" in various ways.
 *
 * Note that eventually we may use the "&" symbol for embedded treasure,
 * and use the "*" symbol to indicate multiple objects, though this will
 * have to wait for Angband 2.8.0 or later.  Note that currently, this
 * is not important, since only one object or terrain feature is allowed
 * in each grid.  If needed, "k_info[0]" will hold the "stack" attr/char.
 *
 * Note the assumption that doing "x_ptr = &x_info[x]" plus a few of
 * "x_ptr->xxx", is quicker than "x_info[x].xxx", if this is incorrect
 * then a whole lot of code should be changed...  XXX XXX
 */
#ifdef USE_TRANSPARENCY
void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp)
#else /* USE_TRANSPARENCY */
void map_info(int y, int x, byte *ap, char *cp)
#endif /* USE_TRANSPARENCY */
{
	feature_type *f_ptr;

	s16b this_o_idx, next_o_idx;

	int feat;

	int py = p_ptr->py;
	int px = p_ptr->px;

	byte a;
	char c;

	bool graf_new = (use_graphics && (strcmp(ANGBAND_GRAF, "new") == 0));

	/* Feature code */
	feat = cave_feat[y][x];

	/* Floors (etc) */
	if (feat <= FEAT_INVIS)
	{
		/* Memorized (or visible) floor */
		if ((cave_info[y][x] & (CAVE_MARK)) ||
			 (((cave_info[y][x] & (CAVE_SEEN)) ||
				((cave_info[y][x] & (CAVE_GLOW)) &&
				 (cave_info[y][x] & (CAVE_VIEW)))) &&
			  !p_ptr->blind))
		{
			/* Access floor */
			f_ptr = &f_info[FEAT_FLOOR];

			/* Normal char */
			c = f_ptr->x_char;

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Special lighting effects */
			if (view_special_lite && ((a == TERM_WHITE) || graf_new))
			{
				/* Handle "blind" */
				if (p_ptr->blind)
				{
					if (graf_new)
					{
						/* Use a dark tile */
						c++;
					}
					else
					{
						/* Use "dark gray" */
						a = TERM_L_DARK;
					}
				}

				/* Handle "torch-lit" grids */
				else if ((cave_info[y][x] & (CAVE_SEEN)) && !(cave_info[y][x] & (CAVE_GLOW)))
				{
					/* Torch lite */
					if (view_yellow_lite)
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

							/* Some races have a special light */
							if (!inventory[INVEN_LITE].k_idx)
							{
								switch (p_ptr->prace)
								{
									case RACE_VAMPIRE:
									{
										a = TERM_L_RED;
										break;
									}
									case RACE_FIEND:
									{
										a = TERM_L_DARK;
										break;
									}
								}
							}
						}
					}
				}

				/* Handle "dark" grids */
				else if (!(cave_info[y][x] & (CAVE_GLOW)))
				{
					if (graf_new)
					{
						/* Use a dark tile */
						c++;
					}
					else
					{
						/* Use "dark gray" */
						a = TERM_L_DARK;
					}
				}

				/* Handle "out-of-sight" grids */
				else if (!(cave_info[y][x] & (CAVE_VIEW)))
				{
					/* Special flag */
					if (view_bright_lite)
					{
						if (graf_new)
						{
							/* Use a dark tile */
							c++;
						}
						else
						{
							/* Use "gray" */
							a = TERM_SLATE;
						}
					}
				}
			}
		}

		/* Unknown */
		else
		{
			/* Access darkness */
			f_ptr = &f_info[FEAT_NONE];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;
		}
	}

	/* Non floors */
	else
	{
		/* Memorized grids */
		if (cave_info[y][x] & (CAVE_MARK))
		{
			/* Apply "mimic" field */
			feat = f_info[feat].mimic;

			/* Access feature */
			f_ptr = &f_info[feat];

			/* Normal char */
			c = f_ptr->x_char;

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Special lighting effects */
			if (view_granite_lite && ((a == TERM_WHITE) || (graf_new)) &&
				(((feat >= FEAT_SECRET) && (feat <= FEAT_PERM_SOLID)) ||
				(feat == FEAT_MORE) || (feat == FEAT_GLYPH) || (feat == FEAT_LESS) ||
				(f_ptr->d_char == '^')))
			{
				/* Handle "blind" */
				if (p_ptr->blind)
				{
					if (graf_new)
					{
						/* Use a dark tile */
						c++;
					}
					else
					{
						/* Use "dark gray" */
						a = TERM_L_DARK;
					}
				}

				/* Handle "torch-lit" grids */
				else if ((cave_info[y][x] & (CAVE_SEEN)) && !(cave_info[y][x] & (CAVE_GLOW)))
				{
					/* Torch lite */
					if (view_yellow_lite)
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

							/* Some races have a special light */
							if (!inventory[INVEN_LITE].k_idx)
							{
								switch (p_ptr->prace)
								{
									case RACE_VAMPIRE:
									case RACE_FIEND:
									{
										a = TERM_RED;
										break;
									}
								}
							}
						}
					}
				}

				/* Handle "view_bright_lite" */
				else if (view_bright_lite)
				{
					/* Not viewable */
					if (!(cave_info[y][x] & (CAVE_VIEW)))
					{
						if (graf_new)
						{
							/* Use a dark tile */
							c++;
						}
						else
						{
							/* Use "gray" */
							a = TERM_SLATE;
						}
					}

					/* Not glowing */
					else if (!(cave_info[y][x] & (CAVE_GLOW)))
					{
						if (graf_new)
						{
							/* Use a lit tile */
						}
						else
						{
							/* Use "gray" */
							a = TERM_SLATE;
						}
					}

					/* Not glowing correctly */
					else
					{
						int xx, yy;

						/* Hack -- move towards player */
						yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
						xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

						/* Check for "local" illumination */
						if (!(cave_info[yy][xx] & (CAVE_GLOW)))
						{
							if (graf_new)
							{
								/* Use a lit tile */
							}
							else
							{
								/* Use "gray" */
								a = TERM_SLATE;
							}
						}
					}
				}
			}
		}

		/* Unknown */
		else
		{
			/* Access darkness */
			f_ptr = &f_info[FEAT_NONE];

			/* Normal attr */
			a = f_ptr->x_attr;

			/* Normal char */
			c = f_ptr->x_char;
		}
	}

	/* Hack -- rare random hallucination, except on outer dungeon walls */
	if (p_ptr->image && (!rand_int(256)) && (cave_feat[y][x] < FEAT_PERM_SOLID))
	{
		/* Hallucinate */
		image_random(ap, cp);
	}

#ifdef USE_TRANSPARENCY
	/* Save the terrain info for the transparency effects */
	(*tap) = a;
	(*tcp) = c;
#endif /* USE_TRANSPARENCY */

	/* Save the info */
	(*ap) = a;
	(*cp) = c;

	/* Objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized objects */
		if (o_ptr->marked)
		{
			/* Normal char */
			(*cp) = object_char(o_ptr);

			/* Normal attr */
			(*ap) = object_attr(o_ptr);

			/* Hack -- hallucination */
			if (p_ptr->image) image_object(ap, cp);

			/* Done */
			break;
		}
	}


	/* Handle monsters */
	if (cave_monster_bold(y, x))
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		/* Visible monster */
		if (m_ptr->ml)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Desired attr */
			a = r_ptr->x_attr;

			/* Desired char */
			c = r_ptr->x_char;

			/* Ignore weird codes */
			if (avoid_other)
			{
				/* Use char */
				(*cp) = c;

				/* Use attr */
				(*ap) = a;
			}

			/* Mimics' colors vary */
			else if (strchr("\"!=", c) && !(r_ptr->flags1 & RF1_UNIQUE))
			{
				/* Use char */
				(*cp) = c;

				/* Use semi-random attr */
				(*ap) = cave_m_idx[y][x] % 16 + 1;
			}

			/* Special attr/char codes */
			else if ((a & 0x80) && (c & 0x80))
			{
				/* Use char */
				(*cp) = c;

				/* Use attr */
				(*ap) = a;
			}

			/* Multi-hued monster */
			else if (r_ptr->flags1 & (RF1_ATTR_MULTI))
			{
				/* Is it a shapechanger? */
				if (r_ptr->flags2 & (RF2_SHAPECHANGER))
				{
					if (use_graphics)
					{
						if (!(streq(ANGBAND_SYS, "ibm")))
						{
							(*cp) = r_info[randint(MAX_R_IDX-2)].x_char;
							(*ap) = r_info[randint(MAX_R_IDX-2)].x_attr;
						}
						else
						{
							int n =  strlen(image_monster_hack_ibm);
							(*cp) = (image_monster_hack_ibm[rand_int(n)]);

							/* Random color */
							(*ap) = randint(15);
						}
					}
					else
					{
						(*cp) = (randint(25)==1?
							image_object_hack[randint(strlen(image_object_hack))]:
							image_monster_hack[randint(strlen(image_monster_hack))]);
					}
				}
				else
					(*cp) = c;

				/* Multi-hued attr */
				if (r_ptr->flags2 & (RF2_ATTR_ANY))
					(*ap) = randint(15);
				else switch (randint(7))
				{
					case 1:
						(*ap)=TERM_RED;
						break;
					case 2:
						(*ap)=TERM_L_RED;
						break;
					case 3:
						(*ap)=TERM_WHITE;
						break;
					case 4:
						(*ap)=TERM_L_GREEN;
						break;
					case 5:
						(*ap)=TERM_BLUE;
						break;
					case 6:
						(*ap)=TERM_L_DARK;
						break;
					case 7:
						(*ap)=TERM_GREEN;
						break;
				}
			}

			/* Normal monster (not "clear" in any way) */
			else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR | RF1_CHAR_CLEAR)))
			{
				/* Use char */
				(*cp) = c;

				/* Use attr */
				(*ap) = a;
			}

			/* Hack -- Bizarre grid under monster */
			else if ((*ap & 0x80) || (*cp & 0x80))
			{
				/* Use char */
				(*cp) = c;

				/* Use attr */
				(*ap) = a;
			}

			/* Normal */
			else
			{
				/* Normal (non-clear char) monster */
				if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)))
				{
					/* Normal char */
					(*cp) = c;
				}

				/* Normal (non-clear attr) monster */
				else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)))
				{
					/* Normal attr */
					(*ap) = a;
				}
			}

			/* Hack -- hallucination */
			if (p_ptr->image)
			{
				/* Hallucinatory monster */
				image_monster(ap, cp);
			}
		}
	}

	/* Handle "player" */
	if (cave_player_bold(y, x))
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		a = r_ptr->x_attr;

		/* Get the "player" char */
		c = r_ptr->x_char;

		if (!use_graphics && player_symbols)
		{
			switch(p_ptr->pclass)
			{
				case CLASS_PALADIN:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_L_WHITE;
					}
					else
					{
						a = TERM_WHITE;
					}
					break;
				}
				case CLASS_WARRIOR_MAGE:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_RED;
					}
					else
					{
						a = TERM_UMBER;
					}
					break;
				}
				case CLASS_CHAOS_WARRIOR:
				{
					a = randint(15);
					break;
				}
				case CLASS_MAGE:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_L_RED;
					}
					else
					{
						a = TERM_RED;
					}
					break;
				}
				case CLASS_HIGH_MAGE:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_ORANGE;
					}
					else
					{
						a = TERM_VIOLET;
					}
					break;
				}
				case CLASS_PRIEST:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_L_BLUE;
					}
					else
					{
						a = TERM_BLUE;
					}
					break;
				}
				case CLASS_RANGER:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_L_GREEN;
					}
					else
					{
						a = TERM_GREEN;
					}
					break;
				}
				case CLASS_ROGUE:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_SLATE;
					}
					else
					{
						a = TERM_L_DARK;
					}
					break;
				}
				case CLASS_WARRIOR:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_L_UMBER;
					}
					else
					{
						a = TERM_UMBER;
					}
					break;
				}
				case CLASS_MONK:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_UMBER;
					}
					else
					{
						a = TERM_ORANGE;
					}
					break;
				}
				case CLASS_MINDCRAFTER:
				{
					if (p_ptr->level < 20)
					{
						a = TERM_VIOLET;
					}
					else
					{
						a = TERM_L_RED;
					}
					break;
				}
				default:
				{
					a = TERM_WHITE;
					break;
				}
			}
			switch(p_ptr->prace)
			{
				case RACE_GNOME:
				case RACE_HOBBIT:
				case RACE_DWARF:
				case RACE_ELF:
				case RACE_HALF_ELF:
				case RACE_HIGH_ELF:
				case RACE_NIBELUNG:
				case RACE_DARK_ELF:
				case RACE_MIND_FLAYER:
				case RACE_SPRITE:
				{
					c = 'h';
					break;
				}
				case RACE_HALF_ORC:
				{
					c = 'o';
					break;
				}
				case RACE_HALF_TROLL:
				{
					c = 'T';
					break;
				}
				case RACE_HALF_OGRE:
				{
					c = 'O';
					break;
				}
				case RACE_HALF_GIANT:
				case RACE_HALF_TITAN:
				case RACE_CYCLOPS:
				{
					c = 'P';
					break;
				}
				case RACE_YEEK:
				{
					c = 'y';
					break;
				}
				case RACE_KLACKON:
				{
					c = 'I';
					break;
				}
				case RACE_KOBOLD:
				{
					c = 'k';
					break;
				}
				case RACE_DRACONIAN:
				{
					if (p_ptr->level < 40) c = 'd';
					else c = 'D';
					break;
				}
				case RACE_IMP:
				case RACE_FIEND:
				{
					c = 'u';
					break;
				}
				case RACE_GOLEM:
				{
					c = 'g';
					break;
				}
				case RACE_SKELETON:
				{
					if (p_ptr->pclass == CLASS_MAGE ||
						 p_ptr->pclass == CLASS_PRIEST ||
						 p_ptr->pclass == CLASS_HIGH_MAGE ||
						 p_ptr->pclass == CLASS_MONK ||
						 p_ptr->pclass == CLASS_MINDCRAFTER)
					{
						c = 'L';
					}
					else
					{
						c = 's';
					}
					break;
				}
				case RACE_ZOMBIE:
				{
					c = 'z';
					break;
				}
				case RACE_VAMPIRE:
				{
					c = 'V';
					break;
				}
				case RACE_SPECTRE:
				{
					c = 'G';
					break;
				}
				case RACE_BEASTMAN:
				{
					c = 'H';
					break;
				}
				default:
				{
					c = 'p';
					break;
				}
			}
		}

#ifdef USE_GRAPHICS
#ifdef VARIABLE_PLAYER_GRAPH

		if (!graf_new)
		{
			if (!(streq(ANGBAND_SYS,"ibm")))
			{

				if (use_graphics && player_symbols)
				{
					a = BMP_FIRST_PC_CLASS + p_ptr->pclass;
					c = BMP_FIRST_PC_RACE  + p_ptr->prace;
				}
			}
			else
			{
				if (use_graphics && player_symbols)
				{
					if (p_ptr->psex == SEX_FEMALE) c = 242;
								switch(p_ptr->pclass)
					{
						case CLASS_PALADIN:
							if (p_ptr->level < 20)
								a = TERM_L_WHITE;
							else
								a = TERM_WHITE;
							c = 253;
													break;
						case CLASS_WARRIOR_MAGE:
							if (p_ptr->level < 20)
								a = TERM_L_RED;
							else
								a = TERM_VIOLET;
							break;
						case CLASS_CHAOS_WARRIOR:
							do
							{
								a = randint(15);
							}
							while (a == TERM_DARK);
							break;
						case CLASS_MAGE:
						case CLASS_HIGH_MAGE:
							if (p_ptr->level < 20)
								a = TERM_L_RED;
							else
								a = TERM_RED;
							c = 248;
							break;
						case CLASS_PRIEST:
							if (p_ptr->level < 20)
								a = TERM_L_BLUE;
							else
								a = TERM_BLUE;
							c = 248;
							break;
						case CLASS_RANGER:
							if (p_ptr->level < 20)
								a = TERM_L_GREEN;
							else
								a = TERM_GREEN;
							break;
						case CLASS_ROGUE:
							if (p_ptr->level < 20)
								a = TERM_SLATE;
							else
								a = TERM_L_DARK;
							break;
						case CLASS_WARRIOR:
							if (p_ptr->level < 20)
								a = TERM_L_UMBER;
							else
								a = TERM_UMBER;
							break;
						case CLASS_MONK:
						case CLASS_MINDCRAFTER:
							if (p_ptr->level < 20)
								a = TERM_L_UMBER;
							else
								a = TERM_UMBER;
							c = 248;
							break;
						default: /* Unknown */
							a = TERM_WHITE;
					}

					switch (p_ptr->prace)
					{
						case RACE_GNOME:
						case RACE_HOBBIT:
							c = 144;
							break;
						case RACE_DWARF:
							c = 236;
							break;
						case RACE_HALF_ORC:
							c = 243;
							break;
						case RACE_HALF_TROLL:
							c = 184;
							break;
						case RACE_ELF:
						case RACE_HALF_ELF:
						case RACE_HIGH_ELF:
							c = 223;
							break;
						case RACE_HALF_OGRE:
							c = 168;
							break;
						case RACE_HALF_GIANT:
						case RACE_HALF_TITAN:
						case RACE_CYCLOPS:
							c = 145;
							break;
						case RACE_YEEK:
							c = 209;
							break;
						case RACE_KLACKON:
							c = 229;
							break;
						case RACE_KOBOLD:
							c = 204;
							break;
						case RACE_NIBELUNG:
							c = 144;
							break;
						case RACE_DARK_ELF:
							c = 223;
							break;
						case RACE_DRACONIAN:
							if (p_ptr->level < 20)
								c = 240;
							else if (p_ptr->level < 40)
								c = 22;
							else
								c = 137;
							break;
						case RACE_MIND_FLAYER:
							c = 236;
							break;
						case RACE_IMP:
							c = 142;
							break;
						case RACE_GOLEM:
							c = 6;
							break;
						case RACE_SKELETON:
							if (p_ptr->pclass == CLASS_MAGE ||
								p_ptr->pclass == CLASS_PRIEST ||
								p_ptr->pclass == CLASS_HIGH_MAGE ||
								p_ptr->pclass == CLASS_MONK ||
								p_ptr->pclass == CLASS_MINDCRAFTER)
								c = 159;
							else
								c = 181;
							break;
						case RACE_ZOMBIE:
							c = 221;
							break;
						case RACE_VAMPIRE:
							c = 217;
							break;
						case RACE_SPECTRE:
							c = 241;
							break;
						case RACE_SPRITE:
							c = 244;
							break;
						case RACE_BEASTMAN:
							c = 154;
							break;
					}
				}
			}
		}

#endif /* VARIABLE_PLAYER_GRAPH */
#endif /* USE_GRAPHICS */

		/* Save the info */
		(*ap) = a;
		(*cp) = c;
	}
}



/*
 * Moves the cursor to a given MAP (y,x) location
 */
void move_cursor_relative(int row, int col)
{
	/* Real co-ords convert to screen positions */
	row -= panel_row_prt;
	col -= panel_col_prt;

	/* Go there */
	Term_gotoxy(col, row);
}


static byte invuln_color(byte a)
{
	switch(a)
	{
		case TERM_DARK:		return TERM_DARK;
		case TERM_WHITE:		return TERM_WHITE;
		case TERM_SLATE:		return TERM_SLATE;
		case TERM_ORANGE:		return TERM_WHITE;
		case TERM_RED:			return TERM_L_WHITE;
		case TERM_GREEN:		return TERM_SLATE;
		case TERM_BLUE:		return TERM_WHITE;
		case TERM_UMBER:		return TERM_SLATE;
		case TERM_L_DARK:		return TERM_L_DARK;
		case TERM_L_WHITE:	return TERM_L_WHITE;
		case TERM_VIOLET:		return TERM_WHITE;
		case TERM_YELLOW:		return TERM_WHITE;
		case TERM_L_RED:		return TERM_WHITE;
		case TERM_L_GREEN:	return TERM_WHITE;
		case TERM_L_BLUE:		return TERM_WHITE;
		case TERM_L_UMBER:	return TERM_SLATE;
		default:					return TERM_WHITE;
	}
}


static byte berserk_color(byte a)
{
	switch(a)
	{
		case TERM_DARK:		return TERM_DARK;
		case TERM_WHITE:		return TERM_L_RED;
		case TERM_SLATE:		return TERM_RED;
		case TERM_ORANGE:		return TERM_L_RED;
		case TERM_RED:			return TERM_RED;
		case TERM_GREEN:		return TERM_L_DARK;
		case TERM_BLUE:		return TERM_L_DARK;
		case TERM_UMBER:		return TERM_RED;
		case TERM_L_DARK:		return TERM_L_DARK;
		case TERM_L_WHITE:	return TERM_RED;
		case TERM_VIOLET:		return TERM_L_RED;
		case TERM_YELLOW:		return TERM_L_RED;
		case TERM_L_RED:		return TERM_L_RED;
		case TERM_L_GREEN:	return TERM_L_DARK;
		case TERM_L_BLUE:		return TERM_L_DARK;
		case TERM_L_UMBER:	return TERM_RED;
		default:					return TERM_L_RED;
	}
}


static byte poison_color(byte a)
{
	switch(a)
	{
		case TERM_DARK:		return TERM_DARK;
		case TERM_WHITE:		return TERM_L_GREEN;
		case TERM_SLATE:		return TERM_GREEN;
		case TERM_ORANGE:		return TERM_L_DARK;
		case TERM_RED:			return TERM_L_DARK;
		case TERM_GREEN:		return TERM_GREEN;
		case TERM_BLUE:		return TERM_L_DARK;
		case TERM_UMBER:		return TERM_L_DARK;
		case TERM_L_DARK:		return TERM_L_DARK;
		case TERM_L_WHITE:	return TERM_GREEN;
		case TERM_VIOLET:		return TERM_L_DARK;
		case TERM_YELLOW:		return TERM_L_GREEN;
		case TERM_L_RED:		return TERM_L_DARK;
		case TERM_L_GREEN:	return TERM_L_GREEN;
		case TERM_L_BLUE:		return TERM_L_GREEN;
		case TERM_L_UMBER:	return TERM_GREEN;
		default:					return TERM_L_GREEN;
	}
}


/*
 * Place an attr/char pair at the given map coordinate, if legal.
 */
void print_rel(char c, byte a, int y, int x)
{
	/* Only do "legal" locations */
	if (panel_contains(y, x))
	{
		/* Hack -- fake monochrome */
		if (!use_graphics || streq(ANGBAND_SYS, "ibm"))
		{
			if (!use_color) a = TERM_WHITE;
			else if (p_ptr->invuln) a = invuln_color(a);
			else if (p_ptr->shero) a = berserk_color(a);
			else if (p_ptr->oppose_pois) a = poison_color(a);
			else if (p_ptr->wraith_form) a = TERM_L_DARK;
		}

		/* Draw the char using the attr */
		Term_draw(x-panel_col_prt, y-panel_row_prt, a, c);
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
	byte info;

	s16b this_o_idx, next_o_idx;


	/* Get cave info */
	info = cave_info[y][x];

	/* Require "seen" flag */
	if (!(info & (CAVE_SEEN))) return;


	/* Hack -- memorize objects */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorize objects */
		o_ptr->marked = TRUE;
	}


	/* Hack -- memorize grids */
	if (!(info & (CAVE_MARK)))
	{
		/* Memorize some "boring" grids */
		if (cave_feat[y][x] <= FEAT_INVIS)
		{
			/* Option -- memorize certain floors */
			if (((info & (CAVE_GLOW)) && view_perma_grids) ||
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
 * Redraw (on the screen) a given MAP location
 *
 * This function should only be called on "legal" grids
 */
void lite_spot(int y, int x)
{
	/* Redraw if on screen */
	if (panel_contains(y, x))
	{
		byte a;
		byte c;

#ifdef USE_TRANSPARENCY
		byte ta;
		char tc;

		/* Examine the grid */
		map_info(y, x, &a, &c, &ta, &tc);
#else /* USE_TRANSPARENCY */
		/* Examine the grid */
		map_info(y, x, &a, &c);
#endif /* USE_TRANSPARENCY */

		/* Hack -- fake monochrome */
		if (!use_graphics || streq(ANGBAND_SYS, "ibm"))
		{
			if (!use_color) a = TERM_WHITE;
			else if (p_ptr->invuln) a = invuln_color(a);
			else if (p_ptr->shero) a = berserk_color(a);
			else if (p_ptr->oppose_pois) a = poison_color(a);
			else if (p_ptr->wraith_form) a = TERM_L_DARK;
		}

#ifdef USE_TRANSPARENCY
		/* Hack -- Queue it */
		Term_queue_char(x-panel_col_prt, y-panel_row_prt, a, c, ta, tc);
#else /* USE_TRANSPARENCY */
		/* Hack -- Queue it */
		Term_queue_char(x-panel_col_prt, y-panel_row_prt, a, c);
#endif /* USE_TRANSPARENCY */
	}
}




/*
 * Prints the map of the dungeon
 *
 * Note that, for efficiency, we contain an "optimized" version
 * of both "lite_spot()" and "print_rel()", and that we use the
 * "lite_spot()" function to display the player grid, if needed.
 */
void prt_map(void)
{
	int x, y;

	int v;

	/* Access the cursor state */
	(void)Term_get_cursor(&v);

	/* Hide the cursor */
	(void)Term_set_cursor(0);

	/* Dump the map */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		/* Scan the columns of row "y" */
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			byte a;
			char c;

#ifdef USE_TRANSPARENCY
			byte ta;
			char tc;

			/* Determine what is there */
			map_info(y, x, &a, &c, &ta, &tc);

			/* Hack -- fake monochrome */
			if (!use_graphics || streq(ANGBAND_SYS, "ibm"))
			{
				if (!use_color) a = TERM_WHITE;
				else if (p_ptr->invuln) a = invuln_color(a);
				else if (p_ptr->shero) a = berserk_color(a);
				else if (p_ptr->oppose_pois) a = poison_color(a);
				else if (p_ptr->wraith_form) a = TERM_L_DARK;
			}

			/* Efficiency -- Redraw that grid of the map */
			Term_queue_char(x-panel_col_prt, y-panel_row_prt, a, c, ta, tc);
#else /* USE_TRANSPARENCY */
			/* Determine what is there */
			map_info(y, x, &a, &c);

			/* Hack -- fake monochrome */
			if (!use_graphics || streq(ANGBAND_SYS, "ibm"))
			{
				if (!use_color) a = TERM_WHITE;
				else if (p_ptr->invuln) a = invuln_color(a);
				else if (p_ptr->shero) a = berserk_color(a);
				else if (p_ptr->oppose_pois) a = poison_color(a);
				else if (p_ptr->wraith_form) a = TERM_L_DARK;
			}

			/* Efficiency -- Redraw that grid of the map */
			Term_queue_char(x-panel_col_prt, y-panel_row_prt, a, c);
#endif /* USE_TRANSPARENCY */
		}
	}

	/* Display player */
	lite_spot(p_ptr->py, p_ptr->px);

	/* Restore the cursor */
	(void)Term_set_cursor(v);
}


/*
 * Hack -- priority array (see below)
 *
 * Note that all "walls" always look like "secret doors" (see "map_info()").
 */
static byte priority_table[][2] =
{
	/* Dark */
	{ FEAT_NONE, 2 },

	/* Floors */
	{ FEAT_FLOOR, 5 },

	/* Walls */
	{ FEAT_SECRET, 10 },

	/* Quartz */
	{ FEAT_QUARTZ, 11 },

	/* Magma */
	{ FEAT_MAGMA, 12 },

	/* Rubble */
	{ FEAT_RUBBLE, 13 },

	/* Open doors */
	{ FEAT_OPEN, 15 },
	{ FEAT_BROKEN, 15 },

	/* Closed doors */
	{ FEAT_DOOR_HEAD + 0x00, 17 },

	/* Hidden gold */
	{ FEAT_QUARTZ_K, 19 },
	{ FEAT_MAGMA_K, 19 },

	/* Stairs */
	{ FEAT_LESS, 25 },
	{ FEAT_MORE, 25 },

	/* End */
	{ 0, 0 }
};


/*
 * Hack -- a priority function (see below)
 */
static byte priority(byte a, char c)
{
	int i, p0, p1;

	feature_type *f_ptr;

	/* Scan the table */
	for (i = 0; TRUE; i++)
	{
		/* Priority level */
		p1 = priority_table[i][1];

		/* End of table */
		if (!p1) break;

		/* Feature index */
		p0 = priority_table[i][0];

		/* Access the feature */
		f_ptr = &f_info[p0];

		/* Check character and attribute, accept matches */
		if ((f_ptr->x_char == c) && (f_ptr->x_attr == a)) return (p1);
	}

	/* Default */
	return (20);
}


/*
 * Display a "small-scale" map of the dungeon in the active Term
 *
 * Note that the "map_info()" function must return fully colorized
 * data or this function will not work correctly.
 *
 * Note that this function must "disable" the special lighting
 * effects so that the "priority" function will work.
 *
 * Note the use of a specialized "priority" function to allow this
 * function to work with any graphic attr/char mappings, and the
 * attempts to optimize this function where possible.
 */
void display_map(int *cy, int *cx)
{
	int i, j, x, y;

	byte ta;
	char tc;

	byte tp;

	byte ma[SCREEN_HGT + 2][SCREEN_WID + 2];
	char mc[SCREEN_HGT + 2][SCREEN_WID + 2];

	byte mp[SCREEN_HGT + 2][SCREEN_WID + 2];

	bool old_view_special_lite;
	bool old_view_granite_lite;

	int yrat = cur_hgt / SCREEN_HGT;
	int xrat = cur_wid / SCREEN_WID;

	/* Save lighting effects */
	old_view_special_lite = view_special_lite;
	old_view_granite_lite = view_granite_lite;

	/* Disable lighting effects */
	view_special_lite = FALSE;
	view_granite_lite = FALSE;


	/* Clear the chars and attributes */
	for (y = 0; y < SCREEN_HGT+2; ++y)
	{
		for (x = 0; x < SCREEN_WID+2; ++x)
		{
			/* Nothing here */
			ma[y][x] = TERM_WHITE;
			mc[y][x] = ' ';

			/* No priority */
			mp[y][x] = 0;
		}
	}

	/* Fill in the map */
	for (i = 0; i < cur_wid; ++i)
	{
		for (j = 0; j < cur_hgt; ++j)
		{
			/* Location */
			x = i / xrat + 1;
			y = j / yrat + 1;

			/* Extract the current attr/char at that map location */
#ifdef USE_TRANSPARENCY
			map_info(j, i, &ta, &tc, &ta, &tc);
#else /* USE_TRANSPARENCY */
			map_info(j, i, &ta, &tc);
#endif /* USE_TRANSPARENCY */

			/* Extract the priority of that attr/char */
			tp = priority(ta, tc);

			/* Save "best" */
			if (mp[y][x] < tp)
			{
				/* Save the char */
				mc[y][x] = tc;

				/* Save the attr */
				ma[y][x] = ta;

				/* Save priority */
				mp[y][x] = tp;
			}
		}
	}


	/* Corners */
	x = SCREEN_WID + 1;
	y = SCREEN_HGT + 1;

	/* Draw the corners */
	mc[0][0] = mc[0][x] = mc[y][0] = mc[y][x] = '+';

	/* Draw the horizontal edges */
	for (x = 1; x <= SCREEN_WID; x++) mc[0][x] = mc[y][x] = '-';

	/* Draw the vertical edges */
	for (y = 1; y <= SCREEN_HGT; y++) mc[y][0] = mc[y][x] = '|';


	/* Display each map line in order */
	for (y = 0; y < SCREEN_HGT+2; ++y)
	{
		/* Start a new line */
		Term_gotoxy(0, y);

		/* Display the line */
		for (x = 0; x < SCREEN_WID+2; ++x)
		{
			ta = ma[y][x];
			tc = mc[y][x];

			/* Hack -- fake monochrome */
			if (!use_graphics || streq(ANGBAND_SYS, "ibm"))
			{
				if (!use_color) ta = TERM_WHITE;
				else if (p_ptr->invuln) ta = invuln_color(ta);
				else if (p_ptr->shero) ta = berserk_color(ta);
				else if (p_ptr->oppose_pois) ta = poison_color(ta);
				else if (p_ptr->wraith_form) ta = TERM_L_DARK;
			}

			/* Add the character */
			Term_addch(ta, tc);
		}
	}


	/* Player location */
	(*cy) = p_ptr->py / yrat + 1;
	(*cx) = p_ptr->px / xrat + 1;


	/* Restore lighting effects */
	view_special_lite = old_view_special_lite;
	view_granite_lite = old_view_granite_lite;
}


/*
 * Display a "small-scale" map of the dungeon for the player
 *
 * Currently, the "player" is displayed on the map.  XXX XXX XXX
 */
void do_cmd_view_map(void)
{
	int cy, cx;

	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Note */
	prt("Please wait...", 0, 0);

	/* Flush */
	Term_fresh();

	/* Clear the screen */
	Term_clear();

	/* Display the map */
	display_map(&cy, &cx);

	/* Wait for it */
	put_str("Hit any key to continue", 23, 23);

	/* Hilite the player */
	move_cursor(cy, cx);

	/* Get any key */
	inkey();

	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
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
 *       Rad=0     Rad=1      Rad=2        Rad=3
 *      No-Lite  Torch,etc   Lantern     Artifacts
 *
 *                                          333
 *                             333         43334
 *                  212       32123       3321233
 *         @        1@1       31@13       331@133
 *                  212       32123       3321233
 *                             333         43334
 *                                          333
 *
 *
 * Here is an illustration of the two different "update_view()" algorithms,
 * in which the grids marked "%" are pillars, and the grids marked "?" are
 * not in line of sight of the player.
 *
 *
 *                    Sample situation
 *
 *                  #####################
 *                  ############.%.%.%.%#
 *                  #...@..#####........#
 *                  #............%.%.%.%#
 *                  #......#####........#
 *                  ############........#
 *                  #####################
 *
 *
 *          New Algorithm             Old Algorithm
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
#define VINFO_MAX_GRIDS 175


/*
 * Maximum number of slopes in a single octant
 */
#define VINFO_MAX_SLOPES 135


/*
 * Mask of bits used in a single octant
 */
#define VINFO_BITS_4 0x0000007FL
#define VINFO_BITS_3 0xFFFFFFFFL
#define VINFO_BITS_2 0xFFFFFFFFL
#define VINFO_BITS_1 0xFFFFFFFFL
#define VINFO_BITS_0 0xFFFFFFFFL


/*
 * Forward declare
 */
typedef struct vinfo_type vinfo_type;


/*
 * The 'vinfo_type' structure
 */
struct vinfo_type
{
	s16b grid_0;
	s16b grid_1;
	s16b grid_2;
	s16b grid_3;
	s16b grid_4;
	s16b grid_5;
	s16b grid_6;
	s16b grid_7;

	u32b bits_4;
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

		vinfo_type *p;


		/* Index */
		e = queue_head;

		/* Dequeue next grid */
		p = queue[queue_head++];

		/* Main Grid */
		g = vinfo[e].grid_0;

		/* Location */
		y = GRID_Y(g);
		x = GRID_X(g);


		/* Compute grid offsets */
		vinfo[e].grid_0 = GRID(+y,+x);
		vinfo[e].grid_1 = GRID(+x,+y);
		vinfo[e].grid_2 = GRID(+x,-y);
		vinfo[e].grid_3 = GRID(+y,-x);
		vinfo[e].grid_4 = GRID(-y,-x);
		vinfo[e].grid_5 = GRID(-x,-y);
		vinfo[e].grid_6 = GRID(-x,+y);
		vinfo[e].grid_7 = GRID(-y,+x);


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
					case 4: vinfo[e].bits_4 |= (1L << (i % 32)); break;
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

			if (queue[queue_tail-1]->grid_0 != g)
			{
				vinfo[queue_tail].grid_0 = g;
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

			if (queue[queue_tail-1]->grid_0 != g)
			{
				vinfo[queue_tail].grid_0 = g;
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
	if (((vinfo[1].bits_4 | vinfo[2].bits_4) != VINFO_BITS_4) ||
		 ((vinfo[1].bits_3 | vinfo[2].bits_3) != VINFO_BITS_3) ||
		 ((vinfo[1].bits_2 | vinfo[2].bits_2) != VINFO_BITS_2) ||
		 ((vinfo[1].bits_1 | vinfo[2].bits_1) != VINFO_BITS_1) ||
		 ((vinfo[1].bits_0 | vinfo[2].bits_0) != VINFO_BITS_0))
	{
		quit("Incorrect bit masks!");
	}


	/* Kill hack */
	KILL(hack, vinfo_hack);


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
 *   for (o2 = 0; o2 < 16; o2 += 2)
 *   ...
 *         g = pg + *((s16b*)(((byte*)(p))+o2));
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

	byte *fast_cave_feat = &cave_feat[0][0];
	byte *fast_cave_info = &cave_info[0][0];

	byte feat;
	byte info;

	byte *s_ptr = &p_ptr->sur_floor;

	/*** Notice surroundings ***/

	/* Reset surroundings info */
	*s_ptr = 0;

	for (i = 0; i < 8; i++)
	{
		int y = py + ddy_ddd[i];
		int x = px + ddx_ddd[i];

		/* Ignore invalid grids */
		if (!in_bounds(y, x)) continue;

		/* Adjacent grid is part of a room or clear */
		if ((cave_info[y][x] & CAVE_ROOM) || cave_floor_bold(y, x))
		{
			/* Take note */
			(*s_ptr)++;
		}
	}


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

		/* Save cave info */
		fast_cave_info[g] = info;
	}

	/* Reset the "view" array */
	fast_view_n = 0;

	/* Extract "radius" value */
	radius = p_ptr->cur_lite;

	/* Handle real light */
	if (radius > 0) ++radius;


	/*** Step 1 -- player grid ***/

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
	for (o2 = 0; o2 < 16; o2 += 2)
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
		u32b bits4 = VINFO_BITS_4;

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
				 (bits3 & (p->bits_3)) ||
				 (bits4 & (p->bits_4)))
			{
				/* Extract grid value XXX XXX XXX */
				g = pg + *((s16b*)(((byte*)(p))+o2));

				/* Get grid info */
				feat = fast_cave_feat[g];
				info = fast_cave_info[g];

				/* Handle wall */
				if (feat & 0x20)
				{
					/* Clear bits */
					bits0 &= ~(p->bits_0);
					bits1 &= ~(p->bits_1);
					bits2 &= ~(p->bits_2);
					bits3 &= ~(p->bits_3);
					bits4 &= ~(p->bits_4);

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
						}

						/* Perma-lit grids */
						else if (info & (CAVE_GLOW))
						{
							int y = GRID_Y(g);
							int x = GRID_X(g);

							/* Hack -- move towards player */
							int yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
							int xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

							/* Check for "simple" illumination */
							if (cave_info[yy][xx] & (CAVE_GLOW))
							{
								/* Mark as seen */
								info |= (CAVE_SEEN);
							}
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




#ifdef MONSTER_FLOW

/*
 * Size of the circular queue used by "update_flow()"
 */
#define FLOW_MAX 2048

/*
 * Hack -- provide some "speed" for the "flow" code
 * This entry is the "current index" for the "when" field
 * Note that a "when" value of "zero" means "not used".
 *
 * Note that the "cost" indexes from 1 to 127 are for
 * "old" data, and from 128 to 255 are for "new" data.
 *
 * This means that as long as the player does not "teleport",
 * then any monster up to 128 + MONSTER_FLOW_DEPTH will be
 * able to track down the player, and in general, will be
 * able to track down either the player or a position recently
 * occupied by the player.
 */
static int flow_save = 0;

#endif /* MONSTER_FLOW */



/*
 * Hack -- forget the "flow" information
 */
void forget_flow(void)
{

#ifdef MONSTER_FLOW

	int x, y;

	/* Nothing to forget */
	if (!flow_save) return;

	/* Check the entire dungeon */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Forget the old data */
			cave_cost[y][x] = 0;
			cave_when[y][x] = 0;
		}
	}

	/* Start over */
	flow_save = 0;

#endif

}


/*
 * Hack -- fill in the "cost" field of every grid that the player can
 * "reach" with the number of steps needed to reach that grid.  This
 * also yields the "distance" of the player from every grid.
 *
 * In addition, mark the "when" of the grids that can reach the player
 * with the incremented value of "flow_save".
 *
 * Hack -- use the local "flow_y" and "flow_x" arrays as a "circular
 * queue" of cave grids.
 *
 * We do not need a priority queue because the cost from grid to grid
 * is always "one" (even along diagonals) and we process them in order.
 */
void update_flow(void)
{

#ifdef MONSTER_FLOW

	int py = p_ptr->py;
	int px = p_ptr->px;

	int ty, tx;

	int y, x;

	int n, d;

	int flow_n;

	int flow_tail = 0;
	int flow_head = 0;

	byte flow_y[FLOW_MAX];
	byte flow_x[FLOW_MAX];


	/* Hack -- disabled */
	if (!flow_by_sound) return;


	/*** Cycle the flow ***/

	/* Cycle the flow */
	if (flow_save++ == 255)
	{
		/* Cycle the flow */
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				int w = cave_when[y][x];
				cave_when[y][x] = (w >= 128) ? (w - 128) : 0;
			}
		}

		/* Restart */
		flow_save = 128;
	}

	/* Local variable */
	flow_n = flow_save;


	/*** Player Grid ***/

	/* Save the time-stamp */
	cave_when[py][px] = flow_n;

	/* Save the flow cost */
	cave_cost[py][px] = 0;

	/* Enqueue that entry */
	flow_y[flow_head] = py;
	flow_x[flow_head] = px;

	/* Advance the queue */
	++flow_tail;


	/*** Process Queue ***/

	/* Now process the queue */
	while (flow_head != flow_tail)
	{
		/* Extract the next entry */
		ty = flow_y[flow_head];
		tx = flow_x[flow_head];

		/* Forget that entry (with wrap) */
		if (++flow_head == FLOW_MAX) flow_head = 0;

		/* Child cost */
		n = cave_cost[ty][tx] + 1;

		/* Hack -- Limit flow depth */
		if (n == MONSTER_FLOW_DEPTH) continue;

		/* Add the "children" */
		for (d = 0; d < 8; d++)
		{
			int old_head = flow_tail;

			/* Child location */
			y = ty + ddy_ddd[d];
			x = tx + ddx_ddd[d];

			/* Ignore "pre-stamped" entries */
			if (cave_when[y][x] == flow_n) continue;

			/* Ignore "walls" and "rubble" */
			if (cave_feat[y][x] >= FEAT_RUBBLE) continue;

			/* Save the time-stamp */
			cave_when[y][x] = flow_n;

			/* Save the flow cost */
			cave_cost[y][x] = n;

			/* Enqueue that entry */
			flow_y[flow_tail] = y;
			flow_x[flow_tail] = x;

			/* Advance the queue */
			if (++flow_tail == FLOW_MAX) flow_tail = 0;

			/* Hack -- Overflow by forgetting new entry */
			if (flow_tail == flow_head) flow_tail = old_head;
		}
	}

#endif

}


/*
 * Hack -- map the current panel (plus some) ala "magic mapping"
 */
void map_area(void)
{
	int i, x, y, y1, y2, x1, x2;

	/* Pick an area to map */
	y1 = panel_row_min - randint(10);
	y2 = panel_row_max + randint(10);
	x1 = panel_col_min - randint(20);
	x2 = panel_col_max + randint(20);

	/* Speed -- shrink to fit legal bounds */
	if (y1 < 1) y1 = 1;
	if (y2 > cur_hgt-2) y2 = cur_hgt-2;
	if (x1 < 1) x1 = 1;
	if (x2 > cur_wid-2) x2 = cur_wid-2;

	/* Scan that area */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* All non-walls are "checked" */
			if (cave_feat[y][x] < FEAT_SECRET)
			{
				/* Memorize normal features */
				if (cave_feat[y][x] > FEAT_INVIS)
				{
					/* Memorize the object */
					cave_info[y][x] |= (CAVE_MARK);
				}

				/* Memorize known walls */
				for (i = 0; i < 8; i++)
				{
					/* Memorize walls (etc) */
					if (cave_feat[y+ddy_ddd[i]][x+ddx_ddd[i]] >= FEAT_SECRET)
					{
						/* Memorize the walls */
						cave_info[y+ddy_ddd[i]][x+ddx_ddd[i]] |= (CAVE_MARK);
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
	for (y = 1; y < cur_hgt-1; y++)
	{
		/* Scan all normal grids */
		for (x = 1; x < cur_wid-1; x++)
		{
			/* Process all non-walls */
			if (cave_feat[y][x] < FEAT_SECRET)
			{
				/* Scan all neighbors */
				for (i = 0; i < 9; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Perma-lite the grid */
					cave_info[yy][xx] |= (CAVE_GLOW);

					/* Memorize normal features */
					if (cave_feat[yy][xx] > FEAT_INVIS)
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

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

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
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
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

	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW);

	/* Update the view and lite */
	p_ptr->update |= (PU_VIEW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Calculate "incremental motion". Used by project() and shoot().
 * Assumes that (*y,*x) lies on the path from (y1,x1) to (y2,x2).
 */
void mmove2(int *y, int *x, int y1, int x1, int y2, int x2)
{
	int dy, dx, dist, shift;

	/* Extract the distance travelled */
	dy = (*y < y1) ? y1 - *y : *y - y1;
	dx = (*x < x1) ? x1 - *x : *x - x1;

	/* Number of steps */
	dist = (dy > dx) ? dy : dx;

	/* We are calculating the next location */
	dist++;


	/* Calculate the total distance along each axis */
	dy = (y2 < y1) ? (y1 - y2) : (y2 - y1);
	dx = (x2 < x1) ? (x1 - x2) : (x2 - x1);

	/* Paranoia -- Hack -- no motion */
	if (!dy && !dx) return;


	/* Move mostly vertically */
	if (dy > dx)
	{
		/* Extract a shift factor */
		shift = (dist * dx + (dy-1) / 2) / dy;

		/* Sometimes move along the minor axis */
		(*x) = (x2 < x1) ? (x1 - shift) : (x1 + shift);

		/* Always move along major axis */
		(*y) = (y2 < y1) ? (y1 - dist) : (y1 + dist);
	}

	/* Move mostly horizontally */
	else
	{
		/* Extract a shift factor */
		shift = (dist * dy + (dx-1) / 2) / dx;

		/* Sometimes move along the minor axis */
		(*y) = (y2 < y1) ? (y1 - shift) : (y1 + shift);

		/* Always move along major axis */
		(*x) = (x2 < x1) ? (x1 - dist) : (x1 + dist);
	}
}


/*
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming no monster gets in the way.
 *
 * This is slightly (but significantly) different from "los(y1,x1,y2,x2)".
 */
bool projectable(int y1, int x1, int y2, int x2)
{
	int y, x;

	int grid_n;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, 0);

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in a wall grid */
	if (!cave_floor_bold(y, x)) return (FALSE);

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
	int nx = y, ny = x;

	int c = 0;

	/* Pick a location */
	while (c++ < 1000)
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

	if (c == 1000)
	{
		ny = y;
		nx = x;
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
	health_who = m_idx;

	/* Redraw (later) */
	p_ptr->redraw |= (PR_HEALTH);
}



/*
 * Hack -- track the given monster race
 */
void monster_race_track(int r_idx)
{
	/* Save this monster ID */
	monster_race_idx = r_idx;

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);
}



/*
 * Hack -- track the given object kind
 */
void object_kind_track(int k_idx)
{
	/* Save this monster ID */
	object_kind_idx = k_idx;

	/* Window stuff */
	p_ptr->window |= (PW_OBJECT);
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
	/* Cancel repeated commands */
	if (command_rep)
	{
		/* Cancel */
		command_rep = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel Resting */
	if (resting)
	{
		/* Cancel */
		resting = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel running */
	if (running)
	{
		/* Cancel */
		running = 0;

		/* Check for new panel if appropriate -- Prfnoff (center) */
		if (center_player && avoid_center) verify_panel();

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

	/* Flush the input if requested */
	if (flush_disturb) flush();
}

