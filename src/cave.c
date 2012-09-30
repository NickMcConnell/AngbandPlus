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
int distance(int y1, int x1, int y2, int x2)
{
	int dy = ABS(y2 - y1);
	int dx = ABS(x2 - x1);

	/* Simple case */
	if (!dy) return dx;
	if (!dx) return dy;

	/* Group */
	{
		/* Squared distance */
		int target = (dy * dy) + (dx * dx);

		/* Approximate distance: hypot(dy,dx) ~= max(dy,dx) + min(dy,dx) / 2 */
		int d = (dy > dx) ? (dy + dx / 2) : (dx + dy / 2);

		while (1)
		{
			/* Approximate error */
			int err = (target - d * d) / (2 * d);

			/* No error - we are done */
			if (!err) break;

			/* Adjust distance */
			d += err;
		}

		/* Return the distance */
		return d;
	}
}


/*
 * Return TRUE if the given square contains a trap
 */
bool is_trap(cave_type *c_ptr)
{
	return (*field_is_type(&c_ptr->fld_idx, FTYPE_TRAP) != 0);
}


/*
 * Return TRUE if the given square contains a known trap
 */
bool is_visible_trap(cave_type *c_ptr)
{
	return (*field_first_known(&c_ptr->fld_idx, FTYPE_TRAP) != 0);
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

	cave_type *c_ptr;

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
				c_ptr = area(ty, x1);
				if (!cave_floor_grid(c_ptr)) return (FALSE);
			}
		}

		/* North -- check for walls */
		else
		{
			for (ty = y1 - 1; ty > y2; ty--)
			{
				c_ptr = area(ty, x1);
				if (!cave_floor_grid(c_ptr)) return (FALSE);
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
				c_ptr = area(y1, tx);
				if (!cave_floor_grid(c_ptr)) return (FALSE);
			}
		}

		/* West -- check for walls */
		else
		{
			for (tx = x1 - 1; tx > x2; tx--)
			{
				c_ptr = area(y1, tx);
				if (!cave_floor_grid(c_ptr)) return (FALSE);
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
			c_ptr = area(y1 + sy, x1);
			if (cave_floor_grid(c_ptr)) return (TRUE);
		}
	}

	/* Horizontal "knights" */
	else if (ay == 1)
	{
		if (ax == 2)
		{
			c_ptr = area(y1, x1 + sx);
			if (cave_floor_grid(c_ptr)) return (TRUE);
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
		m = ay * ay << 1;

		tx = x1 + sx;

		/* Consider the special case where slope == 1. */
		if (ax == ay)
		{
			ty = y1 + sy;
			qy = -f2;
		}
		else
		{
			ty = y1;
			qy = f2;
		}

		/* Note (below) the case (qy == f2), where */
		/* the LOS exactly meets the corner of a tile. */
		while (x2 - tx)
		{
			c_ptr = area(ty, tx);
			if (!cave_floor_grid(c_ptr)) return (FALSE);

			qy += m;

			if (qy < f2)
			{
				tx += sx;
			}
			else if (qy > f2)
			{
				ty += sy;
				c_ptr = area(ty, tx);
				if (!cave_floor_grid(c_ptr)) return (FALSE);
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
		m = ax * ax << 1;

		ty = y1 + sy;

		if (ax == ay)
		{
			tx = x1 + sx;
			qx = -f2;
		}
		else
		{
			tx = x1;
			qx = f2;
		}

		/* Note (below) the case (qx == f2), where */
		/* the LOS exactly meets the corner of a tile. */
		while (y2 - ty)
		{
			c_ptr = area(ty, tx);
			if (!cave_floor_grid(c_ptr)) return (FALSE);

			qx += m;

			if (qx < f2)
			{
				ty += sy;
			}
			else if (qx > f2)
			{
				tx += sx;
				c_ptr = area(ty, tx);
				if (!cave_floor_grid(c_ptr)) return (FALSE);
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
 * Can the player "see" the given grid in detail?
 *
 * He must have vision, illumination, and line of sight.
 *
 * Note -- "CAVE_LITE" is only set if the "torch" has "los()".
 * So, given "CAVE_LITE", we know that the grid is "fully visible".
 *
 * Note that "CAVE_GLOW" makes little sense for a wall, since it would mean
 * that a wall is visible from any direction.  That would be odd.  Except
 * under wizard light, which might make sense.  Thus, for walls, we require
 * not only that they be "CAVE_GLOW", but also, that they be adjacent to a
 * grid which is not only "CAVE_GLOW", but which is a non-wall, and which is
 * in line of sight of the player.
 *
 * This extra check is expensive, but it provides a more "correct" semantics.
 *
 * Note that "glowing walls" are only considered to be "illuminated" if the
 * grid which is next to the wall in the direction of the player is also a
 * "glowing" grid.  This prevents the player from being able to "see" the
 * walls of illuminated rooms from a corridor outside the room.
 */
bool player_can_see_bold(int y, int x)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int xx, yy;

	cave_type *c_ptr;

	/* Blind players see nothing */
	if (p_ptr->blind) return (FALSE);

	/* Access the cave grid */
	c_ptr = area(y, x);

	/* Note that "torch-lite" yields "illumination" */
	if (c_ptr->info & (CAVE_LITE)) return (TRUE);

	/* Require line of sight to the grid */
	if (!player_has_los_grid(c_ptr)) return (FALSE);

	/* Require "perma-lite" of the grid or monster lit grid */
	if (!(c_ptr->info & (CAVE_GLOW | CAVE_MNLT))) return (FALSE);

	/* Floors are simple */
	if (cave_floor_grid(c_ptr)) return (TRUE);

	/* Hack -- move towards player */
	yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
	xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

	/* Check for "local" illumination */
	if (area(yy,xx)->info & (CAVE_GLOW | CAVE_MNLT))
	{
		/* Assume the wall is really illuminated */
		return (TRUE);
	}

	/* Assume not visible */
	return (FALSE);
}


/*
 * Returns true if the player's grid is dark
 */
bool no_lite(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	return (!player_can_see_bold(py, px));
}


/*
 * Determine if a given location may be "destroyed"
 *
 * Used by destruction spells, and for placing stairs, etc.
 */
bool cave_valid_grid(cave_type *c_ptr)
{
	s16b this_o_idx, next_o_idx = 0;


	/* Forbid perma-grids */
	if (cave_perma_grid(c_ptr)) return (FALSE);

	/* Check objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Forbid artifact grids */
		if (o_ptr->flags3 & TR3_INSTA_ART) return (FALSE);
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
 * Mega-Hack -- Hallucinatory monster
 */
static void image_monster(byte *ap, char *cp)
{
	int n = strlen(image_monster_hack);

	/* Random symbol from set above */
	if (use_graphics)
	{
		(*cp) = r_info[randint1(max_r_idx-1)].x_char;
		(*ap) = r_info[randint1(max_r_idx-1)].x_attr;
	}
	else
	/* Text mode */
	{
		(*cp) = (image_monster_hack[randint0(n)]);

		/* Random color */
		(*ap) = randint1(15);
	}
}



/*
 * Hack -- Legal object codes
 */
static cptr image_object_hack = \
"?/|\\\"!$()_-=[]{},~";


/*
 * Mega-Hack -- Hallucinatory object
 */
static void image_object(byte *ap, char *cp)
{
	int n = strlen(image_object_hack);

	if (use_graphics)
	{
		(*cp) = k_info[randint1(max_k_idx-1)].x_char;
		(*ap) = k_info[randint1(max_k_idx-1)].x_attr;
	}
	else
	{
		(*cp) = (image_object_hack[randint0(n)]);

		/* Random color */
		(*ap) = randint1(15);
	}
}



/*
 * Hack -- Random hallucination
 */
static void image_random(byte *ap, char *cp)
{
	/* Normally, assume monsters */
	if (randint0(100) < 75)
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
 * The 16x16 tile of the terrain supports lighting
 */
static bool feat_supports_lighting[256] =
{
	FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE, /* 0x00 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x08 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x10 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x18 */
	FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,/* 0x20 */
	FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,/* 0x28 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x30 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x38 */
	TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,/* 0x40 */
	FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,/* 0x48 */
	FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x50 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x58 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x60 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x68 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x70 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x78 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x80 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x88 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x90 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0x98 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xA0 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xA8 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xB0 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xB8 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xC0 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xC8 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xD8 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xD8 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xE0 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xE8 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, /* 0xF0 */
	TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE  /* 0xF8 */
};


/*
 * Two arrays listing the effects of "brightness"
 * and "darkness" on various "base" colours.
 *
 * This is used to do dynamic lighting effects in ascii :-)
 * At the moment, only the various "floor" tiles are affected.
 */

static byte lighting_colours[16] =
{
	/* TERM_DARK */
	TERM_L_DARK,

	/* TERM_WHITE */
	TERM_YELLOW,

	/* TERM_SLATE */
	TERM_WHITE,

	/* TERM_ORANGE */
	TERM_YELLOW,

	/* TERM_RED */
	TERM_RED,

	/* TERM_GREEN */
	TERM_L_GREEN,

	/* TERM_BLUE */
	TERM_BLUE,

	/* TERM_UMBER */
	TERM_L_UMBER,

	/* TERM_L_DARK */
	TERM_SLATE,

	/* TERM_L_WHITE */
	TERM_WHITE,

	/* TERM_VIOLET */
	TERM_L_RED,

	/* TERM_YELLOW */
	TERM_YELLOW,

	/* TERM_L_RED */
	TERM_L_RED,

	/* TERM_L_GREEN */
	TERM_YELLOW,

	/* TERM_L_BLUE */
	TERM_L_BLUE,

	/* TERM_L_UMBER */
	TERM_L_UMBER,
};

static byte darking_colours[16] =
{
	/* TERM_DARK */
	TERM_DARK,

	/* TERM_WHITE */
	TERM_SLATE,

	/* TERM_SLATE */
	TERM_L_DARK,

	/* TERM_ORANGE */
	TERM_UMBER,

	/* TERM_RED */
	TERM_RED,

	/* TERM_GREEN */
	TERM_GREEN,

	/* TERM_BLUE */
	TERM_BLUE,

	/* TERM_UMBER */
	TERM_RED,

	/* TERM_L_DARK */
	TERM_L_DARK,

	/* TERM_L_WHITE */
	TERM_SLATE,

	/* TERM_VIOLET */
	TERM_BLUE,

	/* TERM_YELLOW */
	TERM_ORANGE,

	/* TERM_L_RED */
	TERM_L_RED,

	/* TERM_L_GREEN */
	TERM_GREEN,

	/* TERM_L_BLUE */
	TERM_L_BLUE,

	/* TERM_L_UMBER */
	TERM_UMBER
};



#ifdef VARIABLE_PLAYER_GRAPH
static void variable_player_graph(byte *a, char *c)
{
	if (!streq(ANGBAND_GRAF, "new"))
	{
		if (!streq(ANGBAND_SYS, "ibm"))
		{
			if (use_graphics)
			{
				*a = BMP_FIRST_PC_CLASS + p_ptr->pclass;
				*c = BMP_FIRST_PC_RACE + p_ptr->prace;
			}
		}
		else
		{
			if (use_graphics)
			{
				if (p_ptr->psex == SEX_FEMALE) *c = (char)242;
				switch (p_ptr->pclass)
				{
					case CLASS_PALADIN:
						if (p_ptr->lev < 20)
							*a = TERM_L_WHITE;
						else
							*a = TERM_WHITE;
						*c = 253;
						break;
					case CLASS_WARRIOR_MAGE:
						if (p_ptr->lev < 20)
							*a = TERM_L_RED;
						else
							*a = TERM_VIOLET;
						break;
					case CLASS_CHAOS_WARRIOR:
						*a = randint0(14) + 1;
						break;
					case CLASS_MAGE:
					case CLASS_HIGH_MAGE:
						if (p_ptr->lev < 20)
							*a = TERM_L_RED;
						else
							*a = TERM_RED;
						*c = 248;
						break;
					case CLASS_PRIEST:
						if (p_ptr->lev < 20)
							*a = TERM_L_BLUE;
						else
							*a = TERM_BLUE;
						*c = 248;
						break;
					case CLASS_RANGER:
						if (p_ptr->lev < 20)
							*a = TERM_L_GREEN;
						else
							*a = TERM_GREEN;
						break;
					case CLASS_ROGUE:
						if (p_ptr->lev < 20)
							*a = TERM_SLATE;
						else
							*a = TERM_L_DARK;
						break;
					case CLASS_WARRIOR:
						if (p_ptr->lev < 20)
							*a = TERM_L_UMBER;
						else
							*a = TERM_UMBER;
						break;
					case CLASS_MONK:
					case CLASS_MINDCRAFTER:
						if (p_ptr->lev < 20)
							*a = TERM_L_UMBER;
						else
							*a = TERM_UMBER;
						*c = 248;
						break;
					default: /* Unknown */
						*a = TERM_WHITE;
				}

				switch (p_ptr->prace)
				{
					case RACE_GNOME:
					case RACE_HOBBIT:
						*c = 144;
						break;
					case RACE_DWARF:
						*c = 236;
						break;
					case RACE_HALF_ORC:
						*c = 243;
						break;
					case RACE_HALF_TROLL:
						*c = 184;
						break;
					case RACE_ELF:
					case RACE_HALF_ELF:
					case RACE_HIGH_ELF:
						*c = 223;
						break;
					case RACE_HALF_OGRE:
						*c = 168;
						break;
					case RACE_HALF_GIANT:
					case RACE_HALF_TITAN:
					case RACE_CYCLOPS:
						*c = 145;
						break;
					case RACE_YEEK:
						*c = 209;
						break;
					case RACE_KLACKON:
						*c = 229;
						break;
					case RACE_KOBOLD:
						*c = 204;
						break;
					case RACE_NIBELUNG:
						*c = 144;
						break;
					case RACE_DARK_ELF:
						*c = 223;
						break;
					case RACE_DRACONIAN:
						if (p_ptr->lev < 20)
							*c = 240;
						else if (p_ptr->lev < 40)
							*c = 22;
						else
							*c = 137;
						break;
					case RACE_MIND_FLAYER:
						*c = 236;
						break;
					case RACE_IMP:
						*c = 142;
						break;
					case RACE_GOLEM:
						*c = 6;
						break;
					case RACE_SKELETON:
						if (p_ptr->pclass == CLASS_MAGE ||
							p_ptr->pclass == CLASS_PRIEST ||
							p_ptr->pclass == CLASS_HIGH_MAGE ||
							p_ptr->pclass == CLASS_MONK ||
							p_ptr->pclass == CLASS_MINDCRAFTER)
							*c = 159;
						else
							*c = 181;
						break;
					case RACE_ZOMBIE:
						*c = 221;
						break;
					case RACE_VAMPIRE:
						*c = 217;
						break;
					case RACE_SPECTRE:
						*c = 241;
						break;
					case RACE_SPRITE:
						*c = 244;
						break;
					case RACE_BEASTMAN:
						*c = 154;
						break;
				}
			}
		}
	}
}
#endif /* VARIABLE_PLAYER_GRAPH */

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
 * Note that bizarre things must be done when the "attr" and/or "char"
 * codes have the "high-bit" set, since these values are used to encode
 * various "special" pictures in some versions, and certain situations,
 * such as "multi-hued" or "clear" monsters, cause the attr/char codes
 * to be "scrambled" in various ways.
 */
#ifdef USE_TRANSPARENCY
void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp)
#else /* USE_TRANSPARENCY */
void map_info(int y, int x, byte *ap, char *cp)
#endif /* USE_TRANSPARENCY */
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	cave_type *c_ptr;

	feature_type *f_ptr;

	object_type *o_ptr;

	monster_type *m_ptr;

	field_type *fld_ptr;

	s16b this_o_idx, next_o_idx;

	s16b this_f_idx, next_f_idx;

	byte feat;
	byte info;

	byte a;
	byte c;

	bool feat_not_ascii;
	s16b halluc = p_ptr->image;

	/* Get the cave */
	c_ptr = area(y,x);

	/* Info flags */
	info = c_ptr->info;

	/* Is this feature memorized? */
	if (info & (CAVE_MARK | CAVE_LITE | CAVE_MNLT))
	{
		feat = c_ptr->feat;

		/* Pointer to the feature */
		f_ptr = &f_info[feat];

		/* The feats attr */
		a = f_ptr->x_attr;

		/* The feats char */
		c = f_ptr->x_char;

		/*
		 * Look for more lighting effects.
		 *
		 * Hack - the cave_floor_grid() macro is not used
		 * since the feat is already known.  This gives a
		 * tiny speedup.  (But any speed increase in this
		 * extremely important routine is good.)
		 */
		if (view_bright_lite && !p_ptr->blind
			 && (!(feat & 0x20) || view_granite_lite))
		{
			/* It's not in view? */
			if (((!(info & (CAVE_VIEW))) && view_special_lite) ||
				((!(info & (CAVE_GLOW | CAVE_LITE | CAVE_MNLT)))
					 &&  view_torch_grids))
			{
				/* If is ascii graphics */
				if (a < 16)
				{
					/* Use darkened colour */
					a = darking_colours[a];
				}
				else if (use_transparency && feat_supports_lighting[feat])
				{
					/* Use a dark tile */
					c++;
				}
			}
			else if ((info & (CAVE_LITE | CAVE_MNLT)) && view_yellow_lite)
			{
				/* Use the torch effect */
				if (a < 16)
				{
					/* Use bright colour */
					a = lighting_colours[a];
				}
				else if (use_transparency && feat_supports_lighting[feat])
				{
					/* Use a light tile */
					c+=2;
				}
			}
		}

		/* Hack -- rare random hallucination, except on outer dungeon walls */
		if (halluc && !((feat == FEAT_PERM_SOLID) || randint0(256)))
		{
			/* Hallucinate */
			image_random(&a, &c);
		}
	}
	else
	{
		/*
		 * Hack - when blind the players light radius is 0
		 * This means that the CAVE_LITE flag does not affect
		 * the result of the comparison in that case.
		 *
		 * This gives a very quick test for the FEAT_NONE case.
		 */

		/* Unknown. Access darkness */
		f_ptr = &f_info[FEAT_NONE];

		/* Blank attr */
		a = f_ptr->x_attr;

		/* Blank char */
		c = f_ptr->x_char;
	}

#ifdef USE_TRANSPARENCY
	/* Save the terrain info for the transparency effects */
	(*tap) = a;
	(*tcp) = c;

#endif /* USE_TRANSPARENCY */

	/* Handle "player" */
	if ((y == py) && (x == px))
	{
		monster_race *r_ptr = &r_info[0];

		/* Get the "player" attr */
		*ap = r_ptr->x_attr;

		/* Get the "player" char */
		*cp = r_ptr->x_char;
#ifdef VARIABLE_PLAYER_GRAPH

		variable_player_graph(ap, cp)

#endif /* VARIABLE_PLAYER_GRAPH */

		/* Done */
		return;
	}

	/* Handle monsters */
	if (c_ptr->m_idx)
	{
		m_ptr = &m_list[c_ptr->m_idx];

		/* Visible monster */
		if (m_ptr->ml)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			feat_not_ascii = (a & 0x80);

			/* Desired attr */
			if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)) || feat_not_ascii)
			{
				a = r_ptr->x_attr;
			}

			/* Desired char */
			if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)) || feat_not_ascii)
			{
				c = r_ptr->x_char;
			}

			/* Ignore weird codes + graphics */
			if ((a & 0x80) || (avoid_other))
			{
				/* Do nothing */
			}

			/* Multi-hued monster */
			else if (r_ptr->flags1 & (RF1_ATTR_MULTI))
			{
				/* Is it a shapechanger? */
				if (r_ptr->flags2 & (RF2_SHAPECHANGER))
				{
					if (use_graphics)
					{
						c = r_info[randint1(max_r_idx-1)].x_char;
						a = r_info[randint1(max_r_idx-1)].x_attr;
					}
					else
					{
						c = (randint1(25) == 1 ?
							image_object_hack[randint1(strlen(image_object_hack))] :
							image_monster_hack[randint1(strlen(image_monster_hack))]);
					}
				}

				/* Multi-hued attr */
				if (r_ptr->flags2 & RF2_ATTR_ANY)
					a = randint1(15);
				else switch (randint1(7))
				{
					case 1:
						a = TERM_RED;
						break;
					case 2:
						a = TERM_L_RED;
						break;
					case 3:
						a = TERM_WHITE;
						break;
					case 4:
						a = TERM_L_GREEN;
						break;
					case 5:
						a = TERM_BLUE;
						break;
					case 6:
						a = TERM_L_DARK;
						break;
					case 7:
						a = TERM_GREEN;
						break;
				}
			}
			/* Mimics' colors vary */
			else if (((c == '\"') || (c == '!') || (c == '='))
				 && !(r_ptr->flags1 & RF1_UNIQUE))
			{
				/* Use char */;

				/* Use semi-random attr */
				a = c_ptr->m_idx % 15 + 1;
			}

			/* Hack -- hallucination */
			if (halluc)
			{
				/* Hallucinatory monster */
				image_monster(&a, &c);
			}


			/* Hack -- fake monochrome */
			if (fake_monochrome)
			{
				if (p_ptr->invuln || !use_color || ironman_moria)
				{
					a = TERM_WHITE;
				}
				else if (p_ptr->wraith_form) a = TERM_L_DARK;
			}

			/* Save the info */
			(*ap) = a;
			(*cp) = c;

			/* Done */
			return;
		}
	}

	/* Fields */
	for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
	{
		/* Acquire field */
		fld_ptr = &fld_list[this_f_idx];

		/* Acquire next field */
		next_f_idx = fld_ptr->next_f_idx;

		/* Memorized, visible fields */
		if ((fld_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_VIS)) ==
			 (FIELD_INFO_MARK | FIELD_INFO_VIS))
		{
			/* Which display level to use? */
			if (fld_ptr->info & FIELD_INFO_FEAT)
			{
				/* Terrain level */
				if (use_transparency && (fld_ptr->info & (FIELD_INFO_TRANS)))
				{
					/* Take into account dynamic lighting. */
					c += fld_ptr->f_char - f_ptr->x_char;
				}
				else
				{
					/* Normal char */
					c = fld_ptr->f_char;
				}

				/* Normal attr */
				a = fld_ptr->f_attr;
#ifdef USE_TRANSPARENCY
				/* Save the terrain info for the transparency effects */
				(*tap) = a;
				(*tcp) = c;
#endif /* USE_TRANSPARENCY */
			}
			else
			{
				/* Above objects */

				/* Normal char */
				(*cp) = fld_ptr->f_char;

				/* Normal attr */
				(*ap) = fld_ptr->f_attr;

				/* Hack - no monochrome effects.  Add them later? */

				/* Done */
				return;
			}

			/* Done */
			break;
		}
	}

	/* Objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized objects */
		if (o_ptr->marked)
		{
			/* Normal char */
			c = object_char(o_ptr);

			/* Normal attr */
			a = object_attr(o_ptr);

			/* Hack -- hallucination */
			if (halluc) image_object(&a, &c);

			/* Done */
			break;
		}
	}

	/* Hack -- fake monochrome */
	if (fake_monochrome)
	{
		if (p_ptr->invuln || !use_color || ironman_moria) a = TERM_WHITE;
		else if (p_ptr->wraith_form) a = TERM_L_DARK;
	}

	/* Save the info */
	(*ap) = a;
	(*cp) = c;
}



/*
 * Moves the cursor to a given MAP (y,x) location
 */
void move_cursor_relative(int row, int col)
{
	/* Real coordinates convert to screen positions */
	row -= panel_row_prt;
	col -= panel_col_prt;

	/* Go there */
	Term_gotoxy(col, row);
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
		if (fake_monochrome)
		{
			if (p_ptr->invuln || !use_color || ironman_moria) a = TERM_WHITE;
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
 * This function will memorize the object and/or feature in the given
 * grid, if they are (1) viewable and (2) interesting.  Note that all
 * objects are interesting, all terrain features except floors (and
 * invisible traps) are interesting, and floors (and invisible traps)
 * are interesting sometimes (depending on various options involving
 * the illumination of floor grids).
 *
 * The automatic memorization of all objects and non-floor terrain
 * features as soon as they are displayed allows incredible amounts
 * of optimization in various places, especially "map_info()".
 *
 * Note that the memorization of objects is completely separate from
 * the memorization of terrain features, preventing annoying floor
 * memorization when a detected object is picked up from a dark floor,
 * and object memorization when an object is dropped into a floor grid
 * which is memorized but out-of-sight.
 *
 * This function should be called every time the "memorization" of
 * a grid (or the object in a grid) is called into question, such
 * as when an object is created in a grid, when a terrain feature
 * "changes" from "floor" to "non-floor", when any grid becomes
 * "illuminated" or "viewable", and when a "floor" grid becomes
 * "torch-lit".
 *
 * Note the relatively efficient use of this function by the various
 * "update_view()" and "update_lite()" calls, to allow objects and
 * terrain features to be memorized (and drawn) whenever they become
 * viewable or illuminated in any way, but not when they "maintain"
 * or "lose" their previous viewability or illumination.
 *
 * Note the butchered "internal" version of "player_can_see_bold()",
 * optimized primarily for the most common cases, that is, for the
 * non-marked floor grids.
 */
void note_spot(int y, int x)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	cave_type *c_ptr = area(y,x);

	s16b this_o_idx, next_o_idx = 0;
	s16b this_f_idx, next_f_idx = 0;


	/* Is it lit + in view + player is not blind? */
	if ((c_ptr->info & (CAVE_GLOW | CAVE_MNLT | CAVE_LITE))
		 && (c_ptr->info & CAVE_VIEW) && !p_ptr->blind)
	{

		/* Hack -- memorize objects */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Memorize objects */
			o_ptr->marked = TRUE;
		}

		/* Hack -- memorize fields */
		for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
		{
			field_type *f_ptr = &fld_list[this_f_idx];

			/* Acquire next field */
			next_f_idx = f_ptr->next_f_idx;

			/* Memorize fields */
			f_ptr->info |= FIELD_INFO_MARK;
		}


		/* Hack -- memorize grids */
		if (!(c_ptr->info & (CAVE_MARK)))
		{
			/* Handle floor grids first */
			if (cave_floor_grid(c_ptr))
			{
				/* Option -- memorize all torch-lit floors */
				if (view_torch_grids && (c_ptr->info & (CAVE_LITE)))
				{
					/* Memorize */
					c_ptr->info |= (CAVE_MARK);
				}

				/* Option -- memorize all perma-lit floors */
				else if (view_perma_grids && (c_ptr->info & (CAVE_GLOW)))
				{
					/* Memorize */
					c_ptr->info |= (CAVE_MARK);
				}
			}

			/* Memorize torch-lit walls */
			else if (c_ptr->info & (CAVE_LITE))
			{
				/* Memorize */
				c_ptr->info |= (CAVE_MARK);
			}

			/* Memorize certain non-torch-lit wall grids */
			else
			{
				int yy, xx;

				/* Hack -- move one grid towards player */
				yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
				xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

				/* Check for "local" illumination */
				if (area(yy,xx)->info & (CAVE_GLOW))
				{
					/* Memorize */
					c_ptr->info |= (CAVE_MARK);
				}
			}
		}
	}

	/* Light the spot, now that we have noticed the changes. */
	lite_spot(y, x);
}

/*
 * This function is nearly identical to the above one - but since it
 * is used in lighting / darkening the wilderness - it doesn't have to
 * deal with walls next to lit rooms.
 *
 * This simplifies the code.
 *
 * Also note that FEAT_FLOOR is not common in the wilderness - this means
 * that the optimisations in note_spot() probably are not useful.
 */

void note_wild_spot(cave_type *c_ptr)
{
	s16b this_o_idx, next_o_idx = 0;
	s16b this_f_idx, next_f_idx = 0;

	/* Blind players see nothing */
	if (p_ptr->blind) return;

	/* Analyze non-torch-lit grids */
	if (!(c_ptr->info & (CAVE_LITE)))
	{
		/* Require line of sight to the grid */
		if (!(c_ptr->info & (CAVE_VIEW))) return;

		/* Require "perma-lite" of the grid or monster lit grid. */
		if (!(c_ptr->info & (CAVE_GLOW | CAVE_MNLT))) return;
	}


	/* Hack -- memorize objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorize objects */
		o_ptr->marked = TRUE;
	}

	/* Hack -- memorize fields */
	for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
	{
		field_type *f_ptr = &fld_list[this_f_idx];

		/* Acquire next field */
		next_f_idx = f_ptr->next_f_idx;

		/* Memorize fields */
		f_ptr->info |= FIELD_INFO_MARK;
	}


	/* Hack -- memorize grids */
	if (!(c_ptr->info & (CAVE_MARK)))
	{
		/* Handle floor grids first */
		if (c_ptr->feat == FEAT_FLOOR)
		{
			/* Option -- memorize all torch-lit floors */
			if (view_torch_grids && (c_ptr->info & (CAVE_LITE)))
			{
				/* Memorize */
				c_ptr->info |= (CAVE_MARK);
			}

			/* Option -- memorize all perma-lit floors */
			else if (view_perma_grids && (c_ptr->info & (CAVE_GLOW)))
			{
				/* Memorize */
				c_ptr->info |= (CAVE_MARK);
			}
		}
		/* Memorize torch-lit squares */
		else if (c_ptr->info & (CAVE_LITE))
		{
			/* Memorize */
			c_ptr->info |= (CAVE_MARK);
		}
	}
}



void display_dungeon(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int x, y;
	byte a;
	char c;

	int wid = Term->wid / 2, hgt = Term->hgt / 2;

#ifdef USE_TRANSPARENCY
	byte ta;
	char tc;
#endif /* USE_TRANSPARENCY */

	for (x = px - wid + 1; x <= px + wid; x++)
	{
		for (y = py - hgt + 1; y <= py + hgt; y++)
		{
			if (in_bounds2(y, x))
			{

#ifdef USE_TRANSPARENCY
				/* Examine the grid */
				map_info(y, x, &a, &c, &ta, &tc);
#else /* USE_TRANSPARENCY */
				/* Examine the grid */
				map_info(y, x, &a, &c);
#endif /* USE_TRANSPARENCY */

#ifdef USE_TRANSPARENCY
				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c, ta, tc);
#else /* USE_TRANSPARENCY */
				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c);
#endif /* USE_TRANSPARENCY */

			}
			else
			{
				/* Clear out-of-bound tiles */

				/* Access darkness */
				feature_type *f_ptr = &f_info[FEAT_NONE];

				/* Normal attr */
				a = f_ptr->x_attr;

				/* Normal char */
				c = f_ptr->x_char;

#ifdef USE_TRANSPARENCY
				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c, ta, tc);
#else /* USE_TRANSPARENCY */
				/* Hack -- Queue it */
				Term_queue_char(x - px + wid - 1, y - py + hgt - 1, a, c);
#endif /* USE_TRANSPARENCY */
			}
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
	if (panel_contains(y, x) && in_bounds2(y, x))
	{
		byte a;
		char c;

#ifdef USE_TRANSPARENCY
		byte ta;
		char tc;

		/* Examine the grid */
		map_info(y, x, &a, &c, &ta, &tc);
#else /* USE_TRANSPARENCY */
		/* Examine the grid */
		map_info(y, x, &a, &c);
#endif /* USE_TRANSPARENCY */

#ifdef USE_TRANSPARENCY
		/* Hack -- Queue it */
		Term_queue_char(x - panel_col_prt, y - panel_row_prt, a, c, ta, tc);
#else /* USE_TRANSPARENCY */
		/* Hack -- Queue it */
		Term_queue_char(x - panel_col_prt, y - panel_row_prt, a, c);
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
	int     x, y;
	int     v, n;

	/* map bounds */
	s16b	xmin, xmax, ymin, ymax;

	int wid, hgt;

	/* Temp variables to speed up deletion loops */
	s16b l1, l2, l3;

	byte *pa;
	char *pc;

#ifdef USE_TRANSPARENCY
	byte *pta;
	char *ptc;
#endif /* USE_TRANSPARENCY */


	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Remove map offset */
	wid -= COL_MAP + 1;
	hgt -= ROW_MAP + 1;

	/* Access the cursor state */
	(void)Term_get_cursor(&v);

	/* Hide the cursor */
	(void)Term_set_cursor(0);


	/* Get bounds */
	xmin = (min_wid < panel_col_min) ? panel_col_min : min_wid;
	xmax = (max_wid - 1 > panel_col_max) ? panel_col_max : max_wid - 1;
	ymin = (min_hgt < panel_row_min) ? panel_row_min : min_hgt;
	ymax = (max_hgt - 1 > panel_row_max) ? panel_row_max : max_hgt - 1;

	/* Bottom section of screen */
	for (y = 1; y <= ymin - panel_row_prt; y++)
	{
		/* Erase the section */
		Term_erase(COL_MAP, y, wid);
	}

	/* Top section of screen */
	for (y = ymax - panel_row_prt; y <= hgt; y++)
	{
		/* Erase the section */
		Term_erase(COL_MAP, y, wid);
	}

	/* Sides of screen */
	/* Left side */
	l1 = xmin - panel_col_min;

	/* Right side */
	l2 = xmax - panel_col_prt;
	l3 = Term->wid - l2;

	for (y = ymin - panel_row_prt; y <= ymax - panel_row_prt; y++)
	{
		/* Erase the sections */
		Term_erase(COL_MAP, y, l1);
		Term_erase(l2, y, l3);
	}

	/* Pointers to current position in the string */
	pa = mp_a;
	pc = mp_c;

#ifdef USE_TRANSPARENCY
	pta = mp_ta;
	ptc = mp_tc;
#endif /* USE_TRANSPARENCY */


	/* Dump the map */
	for (y = ymin; y <= ymax; y++)
	{
		/* No characters yet */
		n = 0;

		/* Scan the columns of row "y" */
		for (x = xmin; x <= xmax; x++)
		{
#ifdef USE_TRANSPARENCY
			/* Determine what is there */
			map_info(y, x, pa++, pc++, pta++, ptc++);

#else /* USE_TRANSPARENCY */

			/* Determine what is there */
			map_info(y, x, pa++, pc++);

#endif /* USE_TRANSPARENCY */
		}


#ifdef USE_TRANSPARENCY

		/* Point to start of line */
		pa = mp_a;
		pc = mp_c;
		pta = mp_ta;
		ptc = mp_tc;

		/* Efficiency -- Redraw that row of the map */
		Term_queue_line(xmin - panel_col_prt, y - panel_row_prt, xmax - xmin + 1
			, pa, pc, pta, ptc);

#else /* USE_TRANSPARENCY */

		/* Point to start of line */
		pa = mp_a;
		pc = mp_c;

		/* Efficiency -- Redraw that row of the map */
		Term_queue_line(xmin - panel_col_prt, y - panel_row_prt, xmax - xmin + 1
			, pa, pc);

#endif /* USE_TRANSPARENCY */
	}

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
	{ FEAT_CLOSED, 17 },

	/* Hidden gold */
	{ FEAT_QUARTZ_K, 19 },
	{ FEAT_MAGMA_K, 19 },

	/* water, lava, & trees */
	{ FEAT_DEEP_WATER, 20 },
	{ FEAT_SHAL_WATER, 20 },
	{ FEAT_DEEP_LAVA, 20 },
	{ FEAT_SHAL_LAVA, 20 },
	{ FEAT_DIRT, 6 },
	{ FEAT_GRASS, 6 },
	{ FEAT_TREES, 6 },
	{ FEAT_MOUNTAIN, 20 },

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
 * Tunnels are important.  (While bare floor is not.)
 */
static int priority_tunnel(int y, int x)
{
	int i, count = 0;

	/* Count number of floors around square */
	for (i = 1; i < 10; i++)
	{
		if (!in_bounds2(y + ddy[i], x + ddx[i])) continue;

		if (cave_floor_grid(&cave[y + ddy[i]][x + ddx[i]]))
			count++;
	}

	/* Three or less floor squares - Important */
	if (count < 4) return (19);

	/* Not important. */
	return (0);
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
 *
 * cx and cy are offsets from the position of the player.  This
 * allows the map to be shifted around - but only works in the
 * wilderness.  cx and cy return the position of the player on the
 * possibly shifted map.
 */
void display_map(int *cy, int *cx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, j, x, y;

	byte ta;
	char tc;

	byte tp;

	u16b w_type, w_info, town;

	byte **ma;
	char **mc;

	byte **mp;

	bool old_view_special_lite = view_special_lite;
	bool old_view_granite_lite = view_granite_lite;

	int hgt, wid, yrat, xrat, xfactor, yfactor;

	/* Get size */
	Term_get_size(&wid, &hgt);
	hgt -= 2;
	wid -= 14;



	/* Disable lighting effects */
	view_special_lite = FALSE;
	view_granite_lite = FALSE;

	/* Allocate the maps */
	C_MAKE(ma, (hgt + 2), byte_ptr);
	C_MAKE(mc, (hgt + 2), char_ptr);
	C_MAKE(mp, (hgt + 2), byte_ptr);

	/* Allocate and wipe each line map */
	for (i = 0; i < (hgt + 2); i++)
	{
		/* Allocate one row each array */
		C_MAKE(ma[i], (wid + 2), byte);
		C_MAKE(mc[i], (wid + 2), char);
		C_MAKE(mp[i], (wid + 2), byte);
	}

	/* Clear the chars and attributes */
	for (y = 0; y < hgt + 2; ++y)
	{
		for (x = 0; x < wid + 2; ++x)
		{
			/* Nothing here */
			ma[y][x] = TERM_WHITE;
			mc[y][x] = ' ';

			/* No priority */
			mp[y][x] = 0;
		}
	}

	if (!p_ptr->depth)
	{
		/* Plot wilderness */

		/* work out coords of player in wilderness */
		x = px / 16 + *cx;
		y = py / 16 + *cy;

		/* recenter */
		x = x - wid / 2;
		if (x < 0) x = 0;

		y = y - hgt / 2;
		if (y < 0) y = 0;

		/* Player location in wilderness */
		(*cy) += py / 16 - y + ROW_MAP;
		(*cx) += px / 16 - x + COL_MAP;

		/* Fill in the map */
		for (i = 0; i < wid; ++i)
		{
			for (j = 0; j < hgt; ++j)
			{
				/* Only draw blocks inside map */
				if (((x + i + 1) >= max_wild) || ((y + j + 1) >= max_wild)) continue;

				/* Only draw blocks that have been seen */
				if (!(wild[j + y][i + x].done.info & WILD_INFO_SEEN)) continue;

				w_type = wild[j + y][i + x].done.wild;
				w_info = wild[j + y][i + x].done.info;

				/* Get attr / char pair for wilderness block type */

				/* This is a nasty hack */

				/* Add in effects of sea / roads */
				if ((w_type >= WILD_SEA) || (w_info & (WILD_INFO_WATER)))
				{
					ma[j + 1][i + 1] = TERM_BLUE;
					mc[j + 1][i + 1] = '~';
				}
				else if (w_info & (WILD_INFO_ACID))
				{
					ma[j + 1][i + 1] = TERM_GREEN;
					mc[j + 1][i + 1] = '~';
				}
				else if (w_info & (WILD_INFO_LAVA))
				{
					ma[j + 1][i + 1] = TERM_RED;
					mc[j + 1][i + 1] = '~';
				}
				else if (w_info & (WILD_INFO_ROAD))
				{
					ma[j + 1][i + 1] = TERM_UMBER;
					mc[j + 1][i + 1] = '+';
				}
				else if (w_info & (WILD_INFO_TRACK))
				{
					ma[j + 1][i + 1] = TERM_L_UMBER;
					mc[j + 1][i + 1] = '+';
				}
				else
				{
					ma[j + 1][i + 1] = wild_gen_data[w_type].w_attr;
					mc[j + 1][i + 1] = wild_gen_data[w_type].w_char;
				}

				/* Hack - draw towns/specials */
				/* Eventually will get attr,char from town data structure. */

				town = wild[j + y][i + x].done.town;

				/* If there is a town... */
				if (town)
				{
					/* Hack make a char /attr */
					ma[j + 1][i + 1] = TERM_WHITE;
					mc[j + 1][i + 1] = '0' + town % 10;

				}

				/* Finally show position of player */
				if ((i + x == px / 16) && (j + y == py / 16))
				{
					ma[j + 1][i + 1] = TERM_WHITE;
					mc[j + 1][i + 1] = '@';
				}
			}
		}
	}
	else
	{
		yrat = max_hgt - min_hgt;
		xrat = max_wid - min_wid;

		/* Get scaling factors */
		yfactor = ((yrat / hgt < 4) && (yrat > hgt)) ? 10 : 1;
		xfactor = ((xrat / wid < 4) && (xrat > wid)) ? 10 : 1;

		yrat = (yrat * yfactor + hgt - 1) / hgt;
		xrat = (xrat * xfactor + wid - 1) / wid;

		/* Player location in dungeon */
		(*cy) = py * yfactor / yrat + ROW_MAP;
		(*cx) = px * xfactor / xrat + COL_MAP;

		/* Fill in the map of dungeon */
		for (i = min_wid; i < max_wid; ++i)
		{
			for (j = min_hgt; j < max_hgt; ++j)
			{
				/* Location */
				x = i * xfactor / xrat + 1;
				y = j * yfactor / yrat + 1;

				/* Priority zero */
				tp = 0;

				if (cave_floor_grid(&cave[j][i]))
				{
					/* Corridors are important */
					tp = priority_tunnel(j, i);
				}

				/* Extract the current attr/char at that map location */
#ifdef USE_TRANSPARENCY
				map_info(j, i, &ta, &tc, &ta, &tc);
#else /* USE_TRANSPARENCY */
				map_info(j, i, &ta, &tc);
#endif /* USE_TRANSPARENCY */

				/* Extract the priority of that attr/char */
				tp += priority(ta, tc);

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
	}

	/* Corners */
	i = wid + 1;
	j = hgt + 1;

	/* Draw the corners */
	mc[0][0] = mc[0][i] = mc[j][0] = mc[j][i] = '+';

	/* Draw the horizontal edges */
	for (i = 1; i <= wid; i++) mc[0][i] = mc[j][i] = '-';

	/* Draw the vertical edges */
	for (j = 1; j <= hgt; j++) mc[j][0] = mc[j][i] = '|';


	/* Display each map line in order */
	for (j = 0; j < hgt + 2; ++j)
	{
		/* Start a new line */
		Term_gotoxy(COL_MAP - 1, j);

		/* Display the line */
		for (i = 0; i < wid + 2; ++i)
		{
			ta = ma[j][i];
			tc = mc[j][i];

			/* Add the character */
			Term_addch(ta, tc);
		}
	}


	/* Restore lighting effects */
	view_special_lite = old_view_special_lite;
	view_granite_lite = old_view_granite_lite;

	/* Free each line map */
	for (i = 0; i < (hgt + 2); i++)
	{
		/* Free one row each array */
		C_FREE(ma[i], (wid + 2), byte);
		C_FREE(mc[i], (wid + 2), char);
		C_FREE(mp[i], (wid + 2), byte);
	}

	/* Free the maps */
	C_FREE(ma, (hgt + 2), byte_ptr);
	C_FREE(mc, (hgt + 2), char_ptr);
	C_FREE(mp, (hgt + 2), byte_ptr);
}


/*
 * Display a "small-scale" map of the dungeon for the player
 *
 * Currently, the "player" is displayed on the map.  XXX XXX XXX
 */
void do_cmd_view_map(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int cy, cx;
	int wid, hgt;

	/* No overhead map in vanilla town mode. */
	if (!p_ptr->depth && vanilla_town) return;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Save the screen */
	screen_save();

	/* Note */
	prt("Please wait...", 0, 0);

	/* Flush */
	Term_fresh();

	/* Clear the screen */
	Term_clear();

	if (p_ptr->depth)
	{
		/* In the dungeon - All we have to do is display the map */

		/* No offset from player */
		cx = 0;
		cy = 0;

		/* Display the map */
		display_map(&cy, &cx);

		/* Wait for it */
		put_str("Hit any key to continue", hgt - 1, (wid - COL_MAP) / 2);

		/* Hilite the player */
		move_cursor(cy, cx);

		/* Get any key */
		inkey();
	}
	else
	{
		/* Offset from player */
		int x, y;

		/* Direction */
		int d;

		/* No offset yet */
		x = 0;
		y = 0;

		/* In the wilderness - Display the map + move it around */

		while (TRUE)
		{
			/* Reset offset of map */
			cx = x;
			cy = y;

			display_map(&cy, &cx);

			/* Wait for it */
			put_str("Move around, or hit any other key to continue.",
			        hgt - 1, COL_MAP - 23 + (wid - COL_MAP) / 2);

			/* Hilite the player */
			move_cursor(cy, cx);

			/* Get a response */
			d = get_keymap_dir(inkey());

			/* Done if not a direction */
			if (!d) break;

			x += ddx[d];
			y += ddy[d];

			/* Bounds checking */
			if (x + px / WILD_BLOCK_SIZE < 0)
			{
				x = -px / WILD_BLOCK_SIZE;
			}
			if (y + py / WILD_BLOCK_SIZE < 0)
			{
				y = -py / WILD_BLOCK_SIZE;
			}
			if (x + px / WILD_BLOCK_SIZE > max_wild - 2)
			{
				x = max_wild - px / WILD_BLOCK_SIZE - 2;
			}
			if (y + py / WILD_BLOCK_SIZE > max_wild - 2)
			{
				y = max_wild - py / WILD_BLOCK_SIZE - 2;
			}
		}
	}

	/* Restore the screen */
	screen_load();
}





/*
 * Some comments on the cave grid flags.  -BEN-
 *
 *
 * One of the major bottlenecks in previous versions of Angband was in
 * the calculation of "line of sight" from the player to various grids,
 * such as monsters.  This was such a nasty bottleneck that a lot of
 * silly things were done to reduce the dependancy on "line of sight",
 * for example, you could not "see" any grids in a lit room until you
 * actually entered the room, and there were all kinds of bizarre grid
 * flags to enable this behavior.  This is also why the "call light"
 * spells always lit an entire room.
 *
 * The code below provides functions to calculate the "field of view"
 * for the player, which, once calculated, provides extremely fast
 * calculation of "line of sight from the player", and to calculate
 * the "field of torch lite", which, again, once calculated, provides
 * extremely fast calculation of "which grids are lit by the player's
 * lite source".  In addition to marking grids as "CAVE_VIEW" and/or
 * "CAVE_LITE", as appropriate, these functions maintain an array
 * containing the locations of all of the grids marked with the
 * CAVE_VIEW flag, which can be used to very quickly scan through all
 * of those grids.
 *
 * To allow more "semantically valid" field of view semantics, whenever
 * the field of view (or the set of torch lit grids) changes, all of the
 * grids in the field of view (or the set of torch lit grids) are "drawn"
 * so that changes in the world will become apparent as soon as possible.
 * This has been optimized so that only grids which actually "change" are
 * redrawn, using the "temp" array and the "CAVE_TEMP" flag to keep track
 * of the grids which are entering or leaving the relevent set of grids.
 *
 * These new methods are so efficient that the old nasty code was removed.
 *
 * Note that there is no reason to "update" the "viewable space" unless
 * the player "moves", or walls/doors are created/destroyed, and there
 * is no reason to "update" the "torch lit grids" unless the field of
 * view changes, or the "light radius" changes, or HACK: if the player
 * becomes blind.  This means that when the player is resting, or digging,
 * or doing anything that does not involve movement or changing the state
 * of the dungeon, there is no need to update the "view" or the "lite"
 * regions, which is nice.
 *
 * Note that the calls to the nasty "los()" function have been removed with
 * regards to the view/lite code.  This results in a huge speed increase.
 *
 *
 * Note that every "CAVE_LITE" grid is also a "CAVE_VIEW" grid, and in
 * fact, the player can always "see" all grids which are marked as
 * "CAVE_LITE", unless they are "off screen".
 *
 * The "update_view()" function maintains the "CAVE_VIEW" flag for each
 * grid and maintains an array of all "CAVE_VIEW" grids.  It also looks
 * after the "CAVE_LITE" flag, and the memorization of new map sqaures.
 *
 *
 * The current "update_view()" algorithm uses the "CAVE_XTRA" flag as a
 * temporary internal flag to mark those grids which have been previously
 * memorized.  This is to prevent blind players from gaining information
 * about their surroundings.  This flag is always cleared when we are done.
 *
 *
 * The current "update_view()" algorithm uses the
 * "CAVE_TEMP" flag, and the array of grids which are marked as "CAVE_TEMP",
 * to keep track of which grids were previously marked as "CAVE_VIEW", which
 * allows us to optimize the "screen updates".
 *
 * The "CAVE_TEMP" flag, and the array of "CAVE_TEMP" grids, is also used
 * for various other purposes, such as spreading lite or darkness during
 * "lite_room()" / "unlite_room()", for calculating monster flow, and filling
 * the fractal caves.
 *
 *
 * Any grid can be marked as "CAVE_GLOW" which means that the grid itself is
 * in some way permanently lit.  However, for the player to "see" anything
 * in the grid, as determined by "player_can_see()", the player must not be
 * blind, the grid must be marked as "CAVE_VIEW", and, in addition, "wall"
 * grids, even if marked as "perma lit", are only illuminated if they touch
 * a grid which is not a wall and is marked both "CAVE_GLOW" and "CAVE_VIEW".
 *
 *
 * To simplify various things, a grid may be marked as "CAVE_MARK", meaning
 * that even if the player cannot "see" the grid, he "knows" the terrain in
 * that grid.  This is used to "remember" walls/doors/stairs/floors when they
 * are "seen" or "detected", and also to "memorize" floors, after "wiz_lite()",
 * or when one of the "memorize floor grids" options induces memorization.
 *
 * Objects are "memorized" in a different way, using a special "marked" flag
 * on the object itself, which is set when an object is observed or detected.
 *
 *
 * A grid may be marked as "CAVE_ROOM" which means that it is part of a "room".
 * This is used only in dungeon generation.  Perhaps this flag can be used in
 * other code if required.
 *
 *
 * A grid may be marked as "CAVE_ICKY" which means it is part of a "vault",
 * and should be unavailable for "teleportation" destinations.
 *
 *
 * The "view_perma_grids" allows the player to "memorize" every perma-lit grid
 * which is observed, and the "view_torch_grids" allows the player to memorize
 * every torch-lit grid.  The player will always memorize important walls,
 * doors, stairs, and other terrain features, as well as any "detected" grids.
 * Note that currently the processing of the "view_perma_grids" option is
 * broken.  If it is off, then you can't see floor with the "CAVE_GLOW" flag.
 * Perhaps the "view_perma_grids" flag, and the "view_torch_grids" flags should
 * be combined.
 *
 * Note that the new "update_view()" method allows, among other things, a room
 * to be "partially" seen as the player approaches it, with a growing cone of
 * floor appearing as the player gets closer to the door.
 *
 * And my favorite "plus" is that you can now use a special option to draw the
 * "floors" in the "viewable region" brightly (actually, to draw the *other*
 * grids dimly), providing a "pretty" effect as the player runs around, and
 * to efficiently display the "torch lite" in a special color.
 *
 * Here are some pictures of the legal "light source" radius values, in
 * which the numbers indicate the "order" in which the grids could have
 * been calculated, if desired.  Larger radii are possible...
 *
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
 * Clear the viewable space
 */
void forget_view(void)
{
	int i, x, y;

	cave_type *c_ptr;

	/* None to forget */
	if (!view_n) return;

	/* Clear them all */
	for (i = 0; i < view_n; i++)
	{
		y = view_y[i];
		x = view_x[i];

		/* Access the grid */
		c_ptr = area(y,x);

		/* Forget that the grid is viewable or lit */
		c_ptr->info &= ~(CAVE_VIEW | CAVE_LITE);

		/* Only lite the spot if is on the panel (can change due to resizing */
		if (!panel_contains(y, x)) continue;

		/* Update the screen */
		lite_spot(y, x);
	}

	/* None left */
	view_n = 0;
}


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
	s16b grid_x[8];
	s16b grid_y[8];

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
				quit_fmt("Too many slopes (%d)!", VINFO_MAX_SLOPES);
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
	int i;
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

		/* Location */
		y = vinfo[e].grid_y[0];
		x = vinfo[e].grid_x[0];


		/* Compute grid offsets */
		vinfo[e].grid_x[0] = x;
		vinfo[e].grid_x[1] = y;
		vinfo[e].grid_x[2] = -y;
		vinfo[e].grid_x[3] = -x;
		vinfo[e].grid_x[4] = -x;
		vinfo[e].grid_x[5] = -y;
		vinfo[e].grid_x[6] = y;
		vinfo[e].grid_x[7] = x;

		vinfo[e].grid_y[0] = y;
		vinfo[e].grid_y[1] = x;
		vinfo[e].grid_y[2] = x;
		vinfo[e].grid_y[3] = y;
		vinfo[e].grid_y[4] = -y;
		vinfo[e].grid_y[5] = -x;
		vinfo[e].grid_y[6] = -x;
		vinfo[e].grid_y[7] = -y;


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
			if (!((queue[queue_tail-1]->grid_x[0] == x + 1) &&
			 		(queue[queue_tail-1]->grid_y[0] == y)))
			{
				vinfo[queue_tail].grid_x[0] = x + 1;
				vinfo[queue_tail].grid_y[0] = y;
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
			if (!((queue[queue_tail-1]->grid_x[0] == x + 1) &&
			 		(queue[queue_tail-1]->grid_y[0] == y + 1)))
			{
				vinfo[queue_tail].grid_x[0] = x + 1;
				vinfo[queue_tail].grid_y[0] = y + 1;
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
		vinfo[e].d = ((y > x) ? (y + x / 2) : (x + y / 2));
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
	FREE(hack, vinfo_hack);


	/* Success */
	return (0);
}

/*
 * Calculate the complete field of view using a new algorithm
 *
 *
 * Normally, vision along the major axes is more likely than vision
 * along the diagonal axes, so we check the bits corresponding to
 * the lines of sight near the major axes first.
 *
 * We use the "temp_x/y" arrays (and the "CAVE_TEMP" flag) to keep track of
 * which grids were previously marked "CAVE_VIEW", since only those grids
 * whose "CAVE_VIEW" value changes during this routine must be redrawn.
 *
 * This function is now responsible for maintaining the "CAVE_LITE"
 * flags as well as the "CAVE_VIEW" flags, which is good, because
 * the only grids which normally need to be memorized and/or redrawn
 * are the ones whose "CAVE_VIEW" flag changes during this routine.
 *
 * Basically, this function divides the "octagon of view" into octants of
 * grids (where grids on the main axes and diagonal axes are "shared" by
 * two octants), and processes each octant one at a time, processing each
 * octant one grid at a time, processing only those grids which "might" be
 * viewable, and setting the "CAVE_VIEW" flag for each grid for which there
 * is an (unobstructed) line of sight from the center of the player grid to
 * any internal point in the grid (and collecting these "CAVE_VIEW" grids
 * into the "view_g" array), and setting the "CAVE_LITE" flag for the grid
 * if, in addition, the grid is "illuminated" in some way (by a torch).
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
 * is closer to the player, and in either case, the existance of a suitable
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
 * (above), which is initialised at startup.
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

	cave_type *c_ptr;

	byte info;

	int x, y, i, o2;

	s16b this_o_idx, next_o_idx = 0;
	s16b this_f_idx, next_f_idx = 0;

	/* Light radius */
	s16b radius = p_ptr->cur_lite;


	/*** Save the old "lite" grids for later ***/

	/* Save the old "view" grids for later */
	for (i = 0; i < view_n; i++)
	{
		y = view_y[i];
		x = view_x[i];

		if (!in_bounds2(y, x)) continue;

		c_ptr = area(y, x);
		info = c_ptr->info;

		/* Save "CAVE_VIEW" grids */
		if (info & (CAVE_VIEW))
		{
			/* Set "CAVE_TEMP" flag */
			info |= (CAVE_TEMP);

			/* Save grid for later */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}

		/* Clear "CAVE_VIEW" and "CAVE_LITE" flags */
		info &= ~(CAVE_VIEW | CAVE_LITE);

		/* Save cave info */
		c_ptr->info = info;
	}

	/* empty the viewable list */
	view_n = 0;

	/*** Step 1 -- player grid ***/

	/* Player grid */

	/* Get grid info */
	info = area(py, px)->info;

	/* Assume viewable */
	info |= (CAVE_VIEW);

	/* Torch-lit grid */
	if (0 < radius)
	{
		/* Mark as "CAVE_LITE" and "CAVE_VIEW" */
		info |= (CAVE_LITE | CAVE_VIEW);
	}

	/* Perma-lit grid */
	else if (info & (CAVE_GLOW | CAVE_MNLT))
	{
		/* Mark as "CAVE_VIEW" */
		info |= (CAVE_VIEW);
	}

	/* Save mark flag */
	if (info & CAVE_MARK)
	{
		info |= CAVE_XTRA;
	}

	/* Save cave info */
	area(py, px)->info = info;

	/* Redraw player*/
	/*lite_spot(py, px);*/

	/* Save in array */

	view_y[view_n] = py;
	view_x[view_n] = px;
	view_n++;

	/*** Step 2 -- octants ***/

	/* Scan each octant */
	for (o2 = 0; o2 < 8; o2 += 1)
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
				/* Get location */
				x = p->grid_x[o2] + px;
				y = p->grid_y[o2] + py;

				/* Is it in bounds? */
				if (!in_bounds2(y, x))
				{
					/* Clear bits */
					bits0 &= ~(p->bits_0);
					bits1 &= ~(p->bits_1);
					bits2 &= ~(p->bits_2);
					bits3 &= ~(p->bits_3);
					bits4 &= ~(p->bits_4);

					continue;
				}

				/* Point to the location on the map */
				c_ptr = area(y, x);

				/* Get current info flags for the square */
				info = c_ptr->info;

				/* Save mark flag */
				if (info & CAVE_MARK)
				{
					info |= CAVE_XTRA;
				}

				if (cave_los_grid(c_ptr))
				{
					/* Floor or semi-blocking terrain like trees */

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

					if (info & CAVE_VIEW) continue;

					/* Mark as "viewable" */
					info |= (CAVE_VIEW);

					/* Torch-lit grids */
					if (p->d <= radius)
					{
						/* Mark as "CAVE_LITE" */
						info |= (CAVE_LITE);
					}

					/* Memorize? */
					if (c_ptr->feat == FEAT_FLOOR)
					{
						/* Why should floor be different? */
						if (((info & (CAVE_LITE)) && view_torch_grids) ||
							info & (CAVE_GLOW))
						{
							/*
							 * Hack - Memorize
							 *
							 * These floors are forgotten unless the
							 * view_perma_grids flag is set.
							 *
							 * This hack is done to simplify the map_info()
							 * function enormously.  All lit grids in
							 * view are marked...
							 */

							/* Memorize */
							info |= (CAVE_MARK);
						}
					}
					else
					{
						if (info & (CAVE_LITE | CAVE_GLOW))
						{
							/* Memorize */
							info |= (CAVE_MARK);
						}
					}

					/* Save cave info */
					c_ptr->info = info;

					/* Save in array */
					view_y[view_n] = y;
					view_x[view_n] = x;
					view_n++;
				}
				/* Handle wall */
				else
				{
					/* Clear bits */
					bits0 &= ~(p->bits_0);
					bits1 &= ~(p->bits_1);
					bits2 &= ~(p->bits_2);
					bits3 &= ~(p->bits_3);
					bits4 &= ~(p->bits_4);

					/* All ready seen.  Next... */
					if (info & CAVE_VIEW) continue;

					/* Mark as viewable */
					info |= (CAVE_VIEW);

					/* Torch-lit grids */
					if (p->d <= radius)
					{
						/* Mark as "CAVE_LITE", "CAVE_MARK" */
						info |= (CAVE_LITE | CAVE_MARK);
					}

					/* Perma-lit grids */
					else if (info & (CAVE_GLOW))
					{
						int yy, xx;

						/* Hack -- move one grid towards player */
						yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
						xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

						/* Check for "local" illumination */
						if (area(yy, xx)->info & (CAVE_GLOW))
						{
							/* Memorize */
							info |= (CAVE_MARK);
						}
					}

					/* Save cave info */
					c_ptr->info = info;

					/* Save in array */
					view_y[view_n] = y;
					view_x[view_n] = x;
					view_n++;
				}
			}
		}
	}

	/*** Step 3 -- Complete the algorithm ***/

	/* Process "new" grids */
	for (i = 0; i < view_n; i++)
	{
		/* Grid */
		x = view_x[i];
		y = view_y[i];

		c_ptr = area(y, x);

		/* Get grid info */
		info = c_ptr->info;

		/* Handle blindness */
		if ((p_ptr->blind) && !(info & CAVE_XTRA))
		{
			/* Grid cannot be "CAVE_MARK" (wasn't before) */
			c_ptr->info &= ~(CAVE_MARK);

			/* Don't do anything else */
			continue;
		}

		/*
		 * We know we have LOS, but is it visible?
		 */
		if (info & (CAVE_LITE | CAVE_GLOW | CAVE_MNLT))
		{
			/* Show the objects */
			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Memorize objects */
				o_ptr->marked = TRUE;
			}

			/* Show the fields */
			for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
			{
				field_type *f_ptr = &fld_list[this_f_idx];

				/* Acquire next field */
				next_f_idx = f_ptr->next_f_idx;

				/* Memorize fields */
				f_ptr->info |= FIELD_INFO_MARK;
			}

			/* Must note the new information on the screen */
			if (!(info & CAVE_TEMP))
			{
				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Process "old" grids */
	for (i = 0; i < temp_n; i++)
	{
		/* Grid */
		x = temp_x[i];
		y = temp_y[i];

		c_ptr = area(y, x);

		/* Get grid info */
		info = c_ptr->info;

		/* Clear "CAVE_TEMP" and "CAVE_XTRA" flags */
		info &= ~(CAVE_TEMP | CAVE_XTRA);

		/* Was "CAVE_VIEW", is now not "CAVE_VIEW" */
		if (!(info & (CAVE_VIEW)))
		{
			if ((info & (CAVE_GLOW) && !view_perma_grids)
				 && cave_floor_grid(c_ptr))
			{
				info &= ~(CAVE_MARK);
			}

			/* Redraw */
			lite_spot(y, x);
		}

		/* Save cave info */
		c_ptr->info = info;
	}

	/* None left */
	temp_n = 0;
}

static bool mon_invis;

/*
 * Add a square to the changes array
 */
static void mon_lite_hack(int y, int x)
{
	cave_type *c_ptr;

	/* Out of bounds */
	if (!in_bounds2(y, x)) return;

	c_ptr = area(y, x);

	/* Want an unlit square in view of the player */
	if ((c_ptr->info & (CAVE_MNLT | CAVE_VIEW)) != CAVE_VIEW) return;

	/* Hack XXX XXX - Is it a wall and monster not in LOS? */
	if (!cave_los_grid(c_ptr) && mon_invis) return;

	/* Save this square */
	if (temp_n < TEMP_MAX)
	{
		temp_x[temp_n] = x;
		temp_y[temp_n] = y;
		temp_n++;
	}

	/* Light it */
	c_ptr->info |= CAVE_MNLT;
}




/*
 * Update squares illuminated by monsters.
 *
 * Hack - use the CAVE_ROOM flag (renamed to be CAVE_MNLT) to
 * denote squares illuminated by monsters.
 *
 * The CAVE_TEMP flag is used to store the state during the
 * updating.  Only squares in view of the player, whos state
 * changes are drawn via lite_spot().
 */
void update_mon_lite(void)
{
	int i, rad;
	cave_type *c_ptr;

	s16b fx, fy;

	s16b end_temp;

	/* Blindness check */
	if (p_ptr->blind)
	{
		/* Clear all the monster lit squares */
		for (i = 0; i < lite_n; i++)
		{
			fx = lite_x[i];
			fy = lite_y[i];

			/* Point to grid */
			c_ptr = area(fy, fx);

			/* Clear monster illumination flag */
			c_ptr->info &= ~(CAVE_MNLT);

			/* It is now unlit */
			note_spot(fy, fx);
		}

		/* Clear the lit list */
		lite_n = 0;

		/* Done */
		return;
	}

	/* Clear all monster lit squares */
	for (i = 0; i < lite_n; i++)
	{
		/* Point to grid */
		c_ptr = area(lite_y[i], lite_x[i]);

		/* Set temp flag */
		c_ptr->info |= (CAVE_TEMP);

		/* Clear monster illumination flag */
		c_ptr->info &= ~(CAVE_MNLT);
	}

	/* Empty temp list of new squares to lite up */
	temp_n = 0;

	/* Loop through monsters, adding newly lit squares to changes list */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Is it too far away? */
		if (m_ptr->cdis > MAX_SIGHT + 3) continue;

		/* Get lite radius */
		rad = 0;

		/* Note the radii are cumulative */
		if (r_ptr->flags7 & (RF7_LITE_1)) rad++;
		if (r_ptr->flags7 & (RF7_LITE_2)) rad += 2;

		/* Exit if has no light */
		if (!rad) continue;

		/* Access the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		/* Is the monster visible? */
		mon_invis = !(area(fy, fx)->info & CAVE_VIEW);

		/* The square it is on */
		mon_lite_hack(fy, fx);

		/* Adjacent squares */
		mon_lite_hack(fy + 1, fx);
		mon_lite_hack(fy - 1, fx);
		mon_lite_hack(fy, fx + 1);
		mon_lite_hack(fy, fx - 1);
		mon_lite_hack(fy + 1, fx + 1);
		mon_lite_hack(fy + 1, fx - 1);
		mon_lite_hack(fy - 1, fx + 1);
		mon_lite_hack(fy - 1, fx - 1);

		/* Radius 2 */
		if (rad >= 2)
		{
			/* South of the monster */
			if (in_bounds2(fy + 1, fx) && cave_floor_grid(area(fy + 1, fx)))
			{
				mon_lite_hack(fy + 2, fx + 1);
				mon_lite_hack(fy + 2, fx);
				mon_lite_hack(fy + 2, fx - 1);

				if (in_bounds2(fy + 2, fx))
				{
					c_ptr = area(fy + 2, fx);
	
					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fy + 3, fx + 1);
						mon_lite_hack(fy + 3, fx);
						mon_lite_hack(fy + 3, fx - 1);
					}
				}
			}

			/* North of the monster */
			if (in_bounds2(fy - 1, fx) && cave_floor_grid(area(fy - 1, fx)))
			{
				mon_lite_hack(fy - 2, fx + 1);
				mon_lite_hack(fy - 2, fx);
				mon_lite_hack(fy - 2, fx - 1);

				if (in_bounds2(fy - 2, fx))
				{
					c_ptr = area(fy - 2, fx);

					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fy - 3, fx + 1);
						mon_lite_hack(fy - 3, fx);
						mon_lite_hack(fy - 3, fx - 1);
					}
				}
			}

			/* East of the monster */
			if (in_bounds2(fy, fx + 1) && cave_floor_grid(area(fy, fx + 1)))
			{
				mon_lite_hack(fy + 1, fx + 2);
				mon_lite_hack(fy, fx + 2);
				mon_lite_hack(fy - 1, fx + 2);

				if (in_bounds2(fy, fx + 2))
				{
					c_ptr = area(fy, fx + 2);

					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fy + 1, fx + 3);
						mon_lite_hack(fy, fx + 3);
						mon_lite_hack(fy - 1, fx + 3);
					}
				}
			}

			/* West of the monster */
			if (in_bounds2(fy, fx - 1) && cave_floor_grid(area(fy, fx - 1)))
			{
				mon_lite_hack(fy + 1, fx - 2);
				mon_lite_hack(fy, fx - 2);
				mon_lite_hack(fy - 1, fx - 2);

				if (in_bounds2(fy, fx - 2))
				{
					c_ptr = area(fy, fx - 2);
	
					/* Radius 3 */
					if ((rad == 3) && cave_floor_grid(c_ptr))
					{
						mon_lite_hack(fy + 1, fx - 3);
						mon_lite_hack(fy, fx - 3);
						mon_lite_hack(fy - 1, fx - 3);
					}
				}
			}
		}

		/* Radius 3 */
		if (rad == 3)
		{
			/* South-East of the monster */
			if (in_bounds2(fy + 1, fx + 1) && 
				cave_floor_grid(area(fy + 1, fx + 1)))
			{
				mon_lite_hack(fy + 2, fx + 2);
			}

			/* South-West of the monster */
			if (in_bounds2(fy + 1, fx - 1) && 
				cave_floor_grid(area(fy + 1, fx - 1)))
			{
				mon_lite_hack(fy + 2, fx - 2);
			}

			/* North-East of the monster */
			if (in_bounds2(fy - 1, fx + 1) &&
				cave_floor_grid(area(fy - 1, fx + 1)))
			{
				mon_lite_hack(fy - 2, fx + 2);
			}

			/* North-West of the monster */
			if (in_bounds2(fy - 1, fx - 1) &&
				cave_floor_grid(area(fy - 1, fx - 1)))
			{
				mon_lite_hack(fy - 2, fx - 2);
			}
		}
	}

	/* Save end of list of new squares */
	end_temp = temp_n;

	/*
	 * Look at old set flags to see if there are any changes.
	 */
	for (i = 0; i < lite_n; i++)
	{
		fx = lite_x[i];
		fy = lite_y[i];

		if (!in_bounds2(fy, fx)) continue;

		/* Point to grid */
		c_ptr = area(fy, fx);

		/* It it no longer lit? */
		if (!(c_ptr->info & CAVE_MNLT) && player_has_los_grid(c_ptr))
		{
			/* It is now unlit */
			note_spot(fy, fx);
		}

		/* Add to end of temp array */
		temp_x[temp_n] = fx;
		temp_y[temp_n] = fy;
		temp_n++;
	}

	/* Clear the lite array */
	lite_n = 0;

	/* Copy the temp array into the lit array lighting the new squares. */
	for (i = 0; i < temp_n; i++)
	{
		fx = temp_x[i];
		fy = temp_y[i];

		if (!in_bounds2(fy, fx)) continue;

		/* Point to grid */
		c_ptr = area(fy, fx);

		if (i >= end_temp)
		{
			/* Clear the temp flag for the old lit grids */
			c_ptr->info &= ~(CAVE_TEMP);
		}
		else
		{
			/* The is the square newly lit and visible? */
			if ((c_ptr->info & (CAVE_VIEW | CAVE_TEMP)) == CAVE_VIEW)
			{
				/* It is now lit */
				note_spot(fy, fx);
			}

			/* Save in the monster lit array */
			lite_x[lite_n] = fx;
			lite_y[lite_n] = fy;
			lite_n++;
		}
	}

	/* Finished with temp_n */
	temp_n = 0;
}

void clear_mon_lite(void)
{
	int i;
	cave_type *c_ptr;

	/* Clear all monster lit squares */
	for (i = 0; i < lite_n; i++)
	{
		/* Point to grid */
		c_ptr = area(lite_y[i], lite_x[i]);

		/* Clear monster illumination flag */
		c_ptr->info &= ~(CAVE_MNLT);
	}

	/* Empty the array */
	lite_n = 0;
}


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
static int flow_n = 0;


/*
 * Hack -- forget the "flow" information
 */
void forget_flow(void)
{

#ifdef MONSTER_FLOW

	int x, y;

	/* Nothing to forget */
	if (!flow_n) return;

	/* Check the entire dungeon */
	for (y = min_hgt; y < max_hgt; y++)
	{
		for (x = min_wid; x < max_wid; x++)
		{
			/* Forget the old data */
			area(y, x)->cost = 0;
			area(y, x)->when = 0;
		}
	}

	/* Start over */
	flow_n = 0;

#endif /* MONSTER_FLOW */

}


#ifdef MONSTER_FLOW

/*
 * Hack - speed up the update_flow algorithm by only doing
 * it everytime the player moves out of LOS of the last
 * "way-point".
 */
static u16b flow_x = 0;
static u16b flow_y = 0;


#endif /* MONSTER_FLOW */


/*
 * Hack -- fill in the "cost" field of every grid that the player
 * can "reach" with the number of steps needed to reach that grid.
 * This also yields the "distance" of the player from every grid.
 *
 * In addition, mark the "when" of the grids that can reach
 * the player with the incremented value of "flow_n".
 *
 * Hack -- use the "seen" array as a "circular queue".
 *
 * We do not need a priority queue because the cost from grid
 * to grid is always "one" and we process them in order.
 */
void update_flow(void)
{

#ifdef MONSTER_FLOW

	int py = p_ptr->py;
	int px = p_ptr->px;

	int x, y, d, w, n;
	int ty, tx;

	int flow_tail = 1;
	int flow_head = 0;

	cave_type *c_ptr;
	byte feat;

	/* Hack -- disabled */
	if (!flow_by_sound) return;

	/* Paranoia -- make sure the array is empty */
	if (temp_n) return;

	/* The last way-point is on the map */
	if (in_bounds2(flow_y, flow_x))
	{
		/* The way point is in sight - do not update.  (Speedup) */
		if (area(flow_y, flow_x)->info & CAVE_VIEW) return;
	}

	/* Save player position */
	flow_y = py;
	flow_x = px;

	/* Cycle the old entries (once per 128 updates) */
	if (flow_n++ == 255)
	{
		/* Rotate the time-stamps */
		for (y = min_hgt; y < max_hgt; y++)
		{
			for (x = min_wid; x < max_wid; x++)
			{
				c_ptr = area(y, x);
				w = c_ptr->when;
				c_ptr->when = (w > 128) ? (w - 128) : 0;
			}
		}

		/* Restart */
		flow_n = 128;
	}


	/*** Player Grid ***/
	c_ptr = area(py, px);

	/* Save the time-stamp */
	c_ptr->when = flow_n;

	/* Save the flow cost */
	c_ptr->cost = 0;

	/* Enqueue that entry */
	temp_y[0] = py;
	temp_x[0] = px;


	/* Now process the queue */
	while (flow_head != flow_tail)
	{
		/* Extract the next entry */
		ty = temp_y[flow_head];
		tx = temp_x[flow_head];

		/* Forget that entry */
		if (++flow_head == TEMP_MAX) flow_head = 0;

		/* Child cost */
		n = area(ty, tx)->cost + 1;

		/* Hack -- Limit flow depth */
		if (n == MONSTER_FLOW_DEPTH) continue;

		/* Add the "children" */
		for (d = 0; d < 8; d++)
		{
			int old_head = flow_tail;

			/* Child location */
			y = ty + ddy_ddd[d];
			x = tx + ddx_ddd[d];

			if (!in_bounds2(y, x)) continue;

			c_ptr = area(y, x);

			feat = c_ptr->feat;

			/* Ignore "pre-stamped" entries */
			if (c_ptr->when == flow_n) continue;

			/* Ignore all "walls" except doors + terrain */
			if ((feat & 0x20) && (feat != FEAT_CLOSED) &&
				 ((feat & 0x60) != 0x60)) continue;

			/* Save the time-stamp */
			c_ptr->when = flow_n;

			/* Save the flow cost */
			c_ptr->cost = n;


			/* Enqueue that entry */
			temp_y[flow_tail] = y;
			temp_x[flow_tail] = x;

			/* Advance the queue */
			if (++flow_tail == TEMP_MAX) flow_tail = 0;

			/* Hack -- Overflow by forgetting new entry */
			if (flow_tail == flow_head) flow_tail = old_head;
		}
	}

#endif /* MONSTER_FLOW */

}



/*
 * Hack -- map a region ala "magic mapping"
 */
void map_area(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, x, y, y1, y2, x1, x2;

	cave_type *c_ptr;


	/* Pick an area to map */
	y1 = py - MAX_DETECT - randint1(10);
	y2 = py + MAX_DETECT + randint1(10);
	x1 = px - MAX_DETECT - randint1(20);
	x2 = px + MAX_DETECT + randint1(20);

	/* Speed -- shrink to fit legal bounds */
	if (y1 < min_hgt + 1) y1 = min_hgt + 1;
	if (y2 > max_hgt - 2) y2 = max_hgt - 2;
	if (x1 < min_wid + 1) x1 = min_wid + 1;
	if (x2 > max_wid - 2) x2 = max_wid - 2;

	/* Scan that area */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			c_ptr = area(y,x);

			/* All non-walls are "checked" */
			if ((c_ptr->feat < FEAT_SECRET) ||
			    (c_ptr->feat == FEAT_RUBBLE) ||
			    (c_ptr->feat >= FEAT_PATTERN_START))
			{
				/* Memorize normal features */
				if (c_ptr->feat >= FEAT_OPEN)
				{
					/* Memorize the object */
					c_ptr->info |= (CAVE_MARK);
				}

				/* Memorize known walls */
				for (i = 0; i < 8; i++)
				{
					c_ptr = area(y + ddy_ddd[i],x + ddx_ddd[i]);

					/* Memorize walls (etc) */
					if (c_ptr->feat >= FEAT_SECRET)
					{
						/* Memorize the walls */
						c_ptr->info |= (CAVE_MARK);
					}
				}
			}
		}
	}

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}



/*
 * Light up the dungeon using "clairvoyance"
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

	chg_virtue(V_KNOWLEDGE, 1);
	chg_virtue(V_ENLIGHTEN, 1);

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

	/* Detect monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Repair visibility later */
		repair_monsters = TRUE;

		/* Hack -- Detect monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Update the monster */
		update_mon(i, FALSE);
	}

	/* Scan all normal grids */
	for (y = min_hgt; y < max_hgt; y++)
	{
		for (x = min_wid; x < max_wid; x++)
		{
			cave_type *c_ptr = area(y,x);

			/* Memorize normal features */
			if (c_ptr->feat >= FEAT_OPEN)
			{
				/* Memorize the grid */
				c_ptr->info |= (CAVE_MARK);
			}
		}
	}


	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(void)
{
	int i, y, x;

	/* Forget every grid */
	for (y = min_hgt; y < max_hgt; y++)
	{
		for (x = min_wid; x < max_wid; x++)
		{
			cave_type *c_ptr = area(y, x);

			/* Process the grid */
			c_ptr->info &= ~(CAVE_MARK);
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

	/* Forget all fields */
	for (i = 1; i < fld_max; i++)
	{
		field_type *f_ptr = &fld_list[i];

		/* Skip dead fields */
		if (!f_ptr->t_idx) continue;

		/* Forget the object */
		f_ptr->info &= (~FIELD_INFO_MARK);
	}

	/* Mega-Hack -- Forget the view */
	p_ptr->update |= (PU_UN_VIEW);

	/* Update the view */
	p_ptr->update |= (PU_VIEW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}





/*
 * Change the "feat" flag for a grid, and notice/redraw the grid
 */
void cave_set_feat(int y, int x, int feat)
{
	cave_type *c_ptr = area(y,x);

	/* Change the feature */
	c_ptr->feat = feat;

	/* Notice + Redraw */
	note_spot(y, x);
}


/*
 * Calculate incremental motion
 * Assumes that (*x, *y) lies on the path from (x1, y1) to (x2, y2).
 */
void mmove2(int *y, int *x, int y1, int x1, int y2, int x2)
{
	int shift;

	/* Assume horizontal movement */
	int *max = x, *min = y;
	int min1 = y1, min2 = y2;
	int max1 = x1, max2 = x2;
	int dmin, dmax;

	/* Extract the distance travelled */
	int dy = ABS(*y - y1);
	int dx = ABS(*x - x1);

	/* Number of steps */
	int dist = MAX(dy, dx) + 1;

	/* Calculate the total distance along each axis */
	dy = ABS(y2 - y1);
	dx = ABS(x2 - x1);

	/* No motion */
	if (!dy && !dx) return;

	/* Vertical movement */
	if (dy > dx)
	{
		max = y; min = x;
		min1 = x1; min2 = x2;
		max1 = y1; max2 = y2;
		dmin = dx; dmax = dy;
	}
	else
	{
		dmin = dy; dmax = dx;
	}

	/* Extract a shift factor */
	shift = (dist * dmin + (dmax - 1) / 2) / dmax;

	/* Sometimes move along the minor axis */
	(*min) = min1 + ((min2 < min1) ? -shift : shift);

	/* Always move along major axis */
	(*max) = max1 + ((max2 < max1) ? -dist : dist);
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

	int grid_n = 0;
	coord grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, 0);

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = grid_g[grid_n - 1].y;
	x = grid_g[grid_n - 1].x;

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
 */
void scatter(int *yp, int *xp, int y, int x, int d)
{
	int nx = 0, ny = 0;

	int c = 0;

	/* Pick a location */
	while (c++ < 1000)
	{
		/* Pick a new location */
		ny = rand_spread(y, d);
		nx = rand_spread(x, d);

		/* Ignore annoying locations */
		if (!in_bounds(ny, nx)) continue;

		/* Ignore excessively distant locations */
		if ((d > 1) && (distance(y, x, ny, nx) > d)) continue;

		/* Require line of sight */
		if (los(y, x, ny, nx)) break;
	}

	if (c > 999)
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
 * Hack -- track the given object kind
 */
void object_kind_track(int k_idx)
{
	/* Save this monster ID */
	p_ptr->object_kind_idx = k_idx;

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
