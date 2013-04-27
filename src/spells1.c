/* File: spells1.c */

/* Purpose: Spell projection */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* ToDo: Make this global */
/* 1/x chance of reducing stats (for elemental attacks) */
#define HURT_CHANCE 16


/*
 * Get a legal "multi-hued" color for drawing "spells"
 */
static byte mh_attr(int max)
{
	switch (randint1(max))
	{
		case  1: return (TERM_RED);
		case  2: return (TERM_GREEN);
		case  3: return (TERM_BLUE);
		case  4: return (TERM_YELLOW);
		case  5: return (TERM_ORANGE);
		case  6: return (TERM_VIOLET);
		case  7: return (TERM_L_RED);
		case  8: return (TERM_L_GREEN);
		case  9: return (TERM_L_BLUE);
		case 10: return (TERM_UMBER);
		case 11: return (TERM_L_UMBER);
		case 12: return (TERM_SLATE);
		case 13: return (TERM_WHITE);
		case 14: return (TERM_L_WHITE);
		case 15: return (TERM_L_DARK);
	}

	return (TERM_WHITE);
}


/*
 * Return a color to use for the bolt/ball spells
 */
static byte spell_color(int type)
{
	/* Check if A.B.'s new graphics should be used (rr9) */
	if (streq(ANGBAND_GRAF, "new"))
	{
		/* Analyze */
		switch (type)
		{
			case GF_PSY_SPEAR:      return (0x06);
			case GF_MISSILE:        return (0x0F);
			case GF_ACID:           return (0x04);
			case GF_ELEC:           return (0x02);
			case GF_FIRE:           return (0x00);
			case GF_COLD:           return (0x01);
			case GF_POIS:           return (0x03);
			case GF_HOLY_FIRE:      return (0x00);
			case GF_HELL_FIRE:      return (0x00);
			case GF_MANA:           return (0x0E);
			case GF_ARROW:          return (0x0F);
			case GF_WATER:          return (0x04);
			case GF_NETHER:         return (0x07);
			case GF_CHAOS:          return (mh_attr(15));
			case GF_DISENCHANT:     return (0x05);
			case GF_NEXUS:          return (0x0C);
			case GF_CONFUSION:      return (mh_attr(4));
			case GF_SOUND:          return (0x09);
			case GF_SHARDS:         return (0x08);
			case GF_FORCE:          return (0x09);
			case GF_INERTIA:        return (0x09);
			case GF_GRAVITY:        return (0x09);
			case GF_TIME:           return (0x09);
			case GF_LITE_WEAK:      return (0x06);
			case GF_LITE:           return (0x06);
			case GF_DARK_WEAK:      return (0x07);
			case GF_DARK:           return (0x07);
			case GF_PLASMA:         return (0x0B);
			case GF_METEOR:         return (0x00);
			case GF_ICE:            return (0x01);
			case GF_ROCKET:         return (0x0F);
			case GF_DEATH_RAY:      return (0x07);
			case GF_NUKE:           return (mh_attr(2));
			case GF_DISINTEGRATE:   return (0x05);
			case GF_PSI:
			case GF_PSI_DRAIN:
			case GF_TELEKINESIS:
			case GF_DOMINATION:
			case GF_GENOCIDE:
						return (0x09);
			case GF_JUMP:
			case GF_JUMP_ATTACK:
						return (0x0F);
			case GF_SOFTEN:         return (0x0F);
		}
	}
	/* Normal tiles or ASCII */
	else if (use_color)
	{
		byte a;
		char c;

		/* Lookup the default colors for this type */
		cptr s = quark_str(gf_color[type]);

		/* Oops */
		if (!s) return (TERM_WHITE);

		/* Pick a random color */
		c = s[randint0(strlen(s))];

		/* Lookup this color */
		a = my_strchr(color_char, c) - color_char;

		/* Invalid color (note check for < 0 removed, gave a silly
		 * warning because bytes are always >= 0 -- RG) */
		if (a > 15) return (TERM_WHITE);

		/* Use this color */
		return (a);
	}

	/* Standard "color" */
	return (TERM_WHITE);
}

/*
 * Length drawing project_path
 */
int project_length = 0;

/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x,y) to (nx,ny).
 *
 * If the distance is not "one", we (may) return "*".
 */
u16b bolt_pict(int y, int x, int ny, int nx, int typ)
{
	int base;

	byte k;

	byte a;
	char c;

	/* No motion (*) */
	if ((ny == y) && (nx == x)) base = 0x30;

	/* Vertical (|) */
	else if (nx == x) base = 0x40;

	/* Horizontal (-) */
	else if (ny == y) base = 0x50;

	/* Diagonal (/) */
	else if ((ny - y) == (x - nx)) base = 0x60;

	/* Diagonal (\) */
	else if ((ny - y) == (nx - x)) base = 0x70;

	/* Weird (*) */
	else base = 0x30;

	/* Basic spell color */
	k = spell_color(typ);

	/* Obtain attr/char */
	a = misc_to_attr[base + k];
	c = misc_to_char[base + k];

	/* Create pict */
	return (PICT(a, c));
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
sint project_path(coord *gp, int range, int y1, int x1, int y2, int x2, u16b flg)
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

		frac = m;

		if (frac > half)
		{
			/* Advance (X) part 2 */
			x += sx;

			/* Advance (X) part 3 */
			frac -= full;

			/* Track distance */
			k++;
		}

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n].x = x;
			gp[n].y = y;
			n++;

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			if (!(flg & (PROJECT_PATH)))
			{
				/* Always stop at non-initial wall grids */
				if ((n > 0) && !cave_floor_bold(y, x)) break;
			}

			/* Sometimes stop at non-initial monsters/players */
			if ((cave[y][x].m_idx != 0) && (n > 0))
			{
				if (flg & (PROJECT_STOP)) break;
				if ((flg & (PROJECT_FRND)) && is_pet(&m_list[cave[y][x].m_idx])) break;		
			}

			if (!in_bounds(y, x)) break;

			/* Slant */
			if (m)
			{
				/* Advance (X) part 1 */
				frac += m;

				/* Horizontal change */
				if (frac > half)
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

		frac = m;

		/* Vertical change */
		if (frac > half)
		{
			/* Advance (Y) part 2 */
			y += sy;

			/* Advance (Y) part 3 */
			frac -= full;

			/* Track distance */
			k++;
		}

		/* Create the projection path */
		while (1)
		{
			/* Save grid */
			gp[n].x = x;
			gp[n].y = y;
			n++;

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			if (!(flg & (PROJECT_PATH)))
			{
				/* Always stop at non-initial wall grids */
				if ((n > 0) && !cave_floor_bold(y, x)) break;
			}

			/* Sometimes stop at non-initial monsters/players */
			if ((cave[y][x].m_idx != 0) && (n > 0))
			{
				if (flg & (PROJECT_STOP)) break;
				if ((flg & (PROJECT_FRND)) && is_pet(&m_list[cave[y][x].m_idx])) break;		
			}
			
			if (!in_bounds(y, x)) break;

			/* Slant */
			if (m)
			{
				/* Advance (Y) part 1 */
				frac += m;

				/* Vertical change */
				if (frac > half)
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
			gp[n].x = x;
			gp[n].y = y;
			n++;

			/* Hack -- Check maximum range */
			if ((n + (n >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			if (!(flg & (PROJECT_PATH)))
			{
				/* Always stop at non-initial wall grids */
				if ((n > 0) && !cave_floor_bold(y, x)) break;
			}

			/* Sometimes stop at non-initial monsters/players */
			if ((cave[y][x].m_idx != 0) && (n > 0))
			{
				if (flg & (PROJECT_STOP)) break;
				if ((flg & (PROJECT_FRND)) && is_pet(&m_list[cave[y][x].m_idx])) break;		
			}

			if (!in_bounds(y, x)) break;

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
 * Mega-Hack -- track "affected" monsters (see "project()" comments)
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;



/*
 * We are called from "project()" to "damage" terrain features
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 * XXX XXX XXX We also "see" grids which are "memorized", probably a hack
 *
 * XXX XXX XXX Perhaps we should affect doors?
 */
static bool project_f(int who, int r, int y, int x, int dam, int typ)
{
	cave_type       *c_ptr = &cave[y][x];

	bool obvious = FALSE;
	bool known = player_can_see_bold(y, x);

	/* Unused */
	(void)who;
	(void)r;
	(void)dam;

	/* Analyze the type */
	switch (typ)
	{
		/* Ignore most effects */
		case GF_PSY_SPEAR:
		case GF_ACID:
		case GF_ELEC:
		case GF_FIRE:
		case GF_COLD:
		case GF_PLASMA:
		case GF_METEOR:
		case GF_ICE:
		case GF_SHARDS:
		case GF_FORCE:
		case GF_SOUND:
		case GF_MANA:
		case GF_HOLY_FIRE:
		case GF_HELL_FIRE:
		case GF_DISINTEGRATE:
		case GF_PSI:
		case GF_PSI_DRAIN:
		case GF_TELEKINESIS:
		case GF_DOMINATION:
		case GF_JUMP:
		case GF_JUMP_ATTACK:
		case GF_DIST_ATTACK:
		{
			break;
		}

		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Reveal secret doors */
			if (c_ptr->feat == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);

				/* Check line of sight */
				if (known)
				{
					obvious = TRUE;
				}
			}

			/* Destroy traps */
			if ((c_ptr->feat == FEAT_INVIS) || is_trap(c_ptr->feat))
			{
				/* Check line of sight */
				if (known)
				{
#ifdef JP
msg_print("まばゆい閃光が走った！");
#else
					msg_print("There is a bright flash of light!");
#endif

					obvious = TRUE;
				}

				/* Forget the trap */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the trap */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Locked doors are unlocked */
			else if ((c_ptr->feat >= FEAT_DOOR_HEAD + 0x01) &&
						 (c_ptr->feat <= FEAT_DOOR_HEAD + 0x07))
			{
				/* Unlock the door */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

				/* Check line of sound */
				if (known)
				{
#ifdef JP
msg_print("カチッと音がした！");
#else
					msg_print("Click!");
#endif

					obvious = TRUE;
				}
			}

			/* Remove "unsafe" flag if player is not blind */
			if (!p_ptr->blind && player_has_los_bold(y, x))
			{
				c_ptr->info &= ~(CAVE_UNSAFE);

				/* Redraw */
				lite_spot(y, x);

				obvious = TRUE;
			}

			break;
		}

		/* Destroy Doors (and traps) */
		case GF_KILL_DOOR:
		{
			/* Destroy all doors and traps */
			if ((c_ptr->feat == FEAT_OPEN) ||
				 (c_ptr->feat == FEAT_BROKEN) ||
				 (c_ptr->feat == FEAT_INVIS) ||
				(is_trap(c_ptr->feat)) ||
				((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				 (c_ptr->feat <= FEAT_DOOR_TAIL)))
			{
				/* Check line of sight */
				if (known)
				{
					/* Message */
#ifdef JP
msg_print("まばゆい閃光が走った！");
#else
					msg_print("There is a bright flash of light!");
#endif

					obvious = TRUE;

					/* Visibility change */
					if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
						 (c_ptr->feat <= FEAT_DOOR_TAIL))
					{
						/* Update some things */
						p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);
					}
				}

				/* Forget the door */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Remove "unsafe" flag if player is not blind */
			if (!p_ptr->blind && player_has_los_bold(y, x))
			{
				c_ptr->info &= ~(CAVE_UNSAFE);

				/* Redraw */
				lite_spot(y, x);

				obvious = TRUE;
			}

			break;
		}

		case GF_JAM_DOOR: /* Jams a door (as if with a spike) */
		{
			if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				 (c_ptr->feat <= FEAT_DOOR_TAIL))
			{
				/* Convert "locked" to "stuck" XXX XXX XXX */
				if (c_ptr->feat < FEAT_DOOR_HEAD + 0x08) c_ptr->feat += 0x08;

				/* Add one spike to the door */
				if (c_ptr->feat < FEAT_DOOR_TAIL) c_ptr->feat++;

				/* Check line of sight */
				if (known)
				{
					/* Message */
#ifdef JP
msg_print("何かがつっかえてドアが開かない。");
#else
					msg_print("The door seems stuck.");
#endif

					obvious = TRUE;
				}
			}
			break;
		}

		/* Destroy walls (and doors) */
		case GF_KILL_WALL:
		{
			/* Non-walls (etc) */
			if (cave_floor_bold(y, x)) break;

			/* Permanent walls */
			if (c_ptr->feat >= FEAT_PERM_EXTRA) break;

			/* Granite */
			if (c_ptr->feat >= FEAT_WALL_EXTRA)
			{
				/* Message */
				if (known && (c_ptr->info & (CAVE_MARK)))
				{
#ifdef JP
					msg_print("壁が溶けて泥になった！");
#else
					msg_print("The wall turns into mud!");
#endif
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Quartz / Magma with treasure */
			else if (c_ptr->feat >= FEAT_MAGMA_H)
			{
				/* Message */
				if (known && (c_ptr->info & (CAVE_MARK)))
				{
#ifdef JP
					msg_print("鉱脈が溶けて泥になった！");
					msg_print("何かを発見した！");
#else
					msg_print("The vein turns into mud!");
					msg_print("You have found something!");
#endif
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Place some gold */
				place_gold(y, x);
			}

			/* Quartz / Magma */
			else if (c_ptr->feat >= FEAT_MAGMA)
			{
				/* Message */
				if (known && (c_ptr->info & (CAVE_MARK)))
				{
#ifdef JP
					msg_print("鉱脈が溶けて泥になった！");
#else
					msg_print("The vein turns into mud!");
#endif
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Rubble */
			else if (c_ptr->feat == FEAT_RUBBLE)
			{
				int prob = 13;

				/* Message */
				if (known && (c_ptr->info & (CAVE_MARK)))
				{
#ifdef JP
					msg_print("岩石が溶けて泥になった！");
#else
					msg_print("The rubble turns into mud!");
#endif
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the rubble */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Hack -- place an object */
				if (one_in_(prob))
				{
					/* Found something */
					if (player_can_see_bold(y, x))
					{
#ifdef JP
						msg_print("岩石の下に何か隠されていた！");
#else
						msg_print("There was something buried in the rubble!");
#endif
						obvious = TRUE;
					}

					/* Place object */
					place_object(y, x, FALSE, FALSE);
				}
			}

			/* Destroy doors (and secret doors) */
			else /* if (c_ptr->feat >= FEAT_DOOR_HEAD) */
			{
				/* Hack -- special message */
				if (known && (c_ptr->info & (CAVE_MARK)))
				{
#ifdef JP
					msg_print("ドアが溶けて泥になった！");
#else
					msg_print("The door turns into mud!");
#endif
					obvious = TRUE;
				}

				/* Forget the wall */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

			break;
		}

		/* Make doors */
		case GF_MAKE_DOOR:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Not on the player */
			if ((y == py) && (x == px)) break;

			/* Create a closed door */
			cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

			/* Observe */
			if (c_ptr->info & (CAVE_MARK)) obvious = TRUE;

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

			break;
		}

		/* Make traps */
		case GF_MAKE_TRAP:
		{
			/* Require a "naked" floor grid */
			if ((cave[y][x].feat != FEAT_FLOOR) &&
				 (cave[y][x].o_idx == 0) &&
				 (cave[y][x].m_idx == 0))
				 break;

			/* Place a trap */
			place_trap(y, x);

			break;
		}

		case GF_MAKE_GLYPH:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			cave_set_feat(y, x, FEAT_GLYPH);

			break;
		}

		case GF_STONE_WALL:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Not on the player */
			if ((y == py) && (x == px)) break;

			/* Place a trap */
			cave_set_feat(y, x, FEAT_WALL_EXTRA);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

			break;
		}

		/* Lite up the grid */
		case GF_LITE_WEAK:
		case GF_LITE:
		{
			/* Turn on the light */
			c_ptr->info |= (CAVE_GLOW);

			/* Notice */
			note_spot(y, x);

			/* Redraw */
			lite_spot(y, x);

			/* Observe */
			if (player_can_see_bold(y, x)) obvious = TRUE;

			/* Mega-Hack -- Update the monster in the affected grid */
			/* This allows "spear of light" (etc) to work "correctly" */
			if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);

			break;
		}

		/* Darken the grid */
		case GF_DARK_WEAK:
		case GF_DARK:
		{
			/* Notice */
			if (player_can_see_bold(y, x)) obvious = TRUE;

			/* Turn off the light. */
			c_ptr->info &= ~(CAVE_GLOW);

			/* Hack -- Forget "boring" grids */
			if ((c_ptr->feat <= FEAT_INVIS) || (c_ptr->feat == FEAT_WALL_INVIS))
			{
				/* Forget */
				c_ptr->info &= ~(CAVE_MARK);

				/* Notice */
				note_spot(y, x);
			}

			/* Redraw */
			lite_spot(y, x);

			/* Mega-Hack -- Update the monster in the affected grid */
			/* This allows "spear of light" (etc) to work "correctly" */
			if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);

			/* All done */
			break;
		}

		/* Stone to Flesh */
		case GF_SOFTEN:
		{
			if ((c_ptr->feat == FEAT_RUBBLE) ||
				((c_ptr->feat >= FEAT_WALL_EXTRA) && (c_ptr->feat <= FEAT_WALL_SOLID)))
			{
				int i;
				object_type forge;

				/* Notice */
				if (known && (c_ptr->info & (CAVE_MARK)))
				{
#ifdef JP
					msg_print("岩石が軟らかくなった！");
#else
					if (c_ptr->feat == FEAT_RUBBLE)
						msg_print("The rubble becomes soft!");
					else
						msg_print("The rock becomes soft!");
#endif
					obvious = TRUE;
				}

				/* Forget */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Make many foods */
				i = lookup_kind(TV_FOOD, SV_FOOD_SLIME_MOLD);
				object_prep(&forge, i);
				forge.number = (byte)rand_range(20, 40);
				drop_near(&forge, -1, y, x);

				/* Notice */
				note_spot(y, x);
			}

			/* Redraw */
			lite_spot(y, x);

			break;
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}



/*
 * We are called from "project()" to "damage" objects
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * Perhaps we should only SOMETIMES damage things on the ground.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * XXX XXX XXX We also "see" grids which are "memorized", probably a hack
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_o(int who, int r, int y, int x, int dam, int typ)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx;

	bool obvious = FALSE;
	bool known = player_can_see_bold(y, x);

	u32b f1, f2, f3;

	char o_name[MAX_NLEN];

	int k_idx;
	bool is_potion;

	/* Unused */
	(void)r;
	(void)dam;

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool do_kill = FALSE;
#ifndef JP
		/* Get the "plural"-ness */
		bool plural;
#endif

		cptr note_kill = NULL;
		int sound_kill = 0;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];
#ifndef JP
		plural = (o_ptr->number > 1);
#endif

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Check for artifact */
		if ((artifact_p(o_ptr) || o_ptr->art_name)) is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{
			/* Acid -- Lots of things */
			case GF_ACID:
			{
				if (hates_acid(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "融けてしまった！";
#else
					note_kill = (plural ? " melt!" : " melts!");
#endif
					sound_kill = SOUND_BR_ACID; /* (Sound substitute) No sound for acid */
					if (f3 & (TR3_IGNORE_ACID)) ignore = TRUE;
				}
				break;
			}

			/* Elec -- Rings and Wands */
			case GF_ELEC:
			{
				if (hates_elec(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "壊れてしまった！";
#else
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif
					sound_kill = SOUND_BR_ELEC; /* (Sound substitute) No sound for elec */
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire -- Flammable objects */
			case GF_FIRE:
			{
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "燃えてしまった！";
#else
					note_kill = (plural ? " burn up!" : " burns up!");
#endif
					sound_kill = SOUND_BR_FIRE; /* (Sound substitute) No sound for fire */
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
				}
				break;
			}

			/* Cold -- potions and flasks */
			case GF_COLD:
			{
				if (hates_cold(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "砕け散ってしまった！";
#else
					note_kill = (plural ? " shatter!" : " shatters!");
#endif
					sound_kill = SOUND_BR_FROST; /* (Sound substitute) No sound for cold */
					if (f3 & (TR3_IGNORE_COLD)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Elec */
			case GF_PLASMA:
			{
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "燃えてしまった！";
#else
					note_kill = (plural ? " burn up!" : " burns up!");
#endif
					sound_kill = SOUND_BR_FIRE; /* (Sound substitute) No sound for fire */
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
				}
				if (hates_elec(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
#ifdef JP
					note_kill = "壊れてしまった！";
#else
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif
					sound_kill = SOUND_BR_ELEC; /* (Sound substitute) No sound for elec */
					if (f3 & (TR3_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Cold */
			case GF_METEOR:
			{
				if (hates_fire(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "燃えてしまった！";
#else
					note_kill = (plural ? " burn up!" : " burns up!");
#endif
					sound_kill = SOUND_BR_FIRE; /* (Sound substitute) No sound for fire */
					if (f3 & (TR3_IGNORE_FIRE)) ignore = TRUE;
				}
				if (hates_cold(o_ptr))
				{
					ignore = FALSE;
					do_kill = TRUE;
#ifdef JP
					note_kill = "砕け散ってしまった！";
#else
					note_kill = (plural ? " shatter!" : " shatters!");
#endif
					sound_kill = SOUND_BR_FROST; /* (Sound substitute) No sound for cold */
					if (f3 & (TR3_IGNORE_COLD)) ignore = TRUE;
				}
				break;
			}

			/* Hack -- break potions and such */
			case GF_ICE:
			case GF_SHARDS:
			case GF_FORCE:
			case GF_SOUND:
			{
				if (hates_cold(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "砕け散ってしまった！";
#else
					note_kill = (plural ? " shatter!" : " shatters!");
#endif
					sound_kill = SOUND_BR_FROST; /* (Sound substitute) No sound for cold */
				}
				break;
			}

			/* Mana and Chaos -- destroy everything */
			case GF_MANA:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "壊れてしまった！";
#else
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif
				sound_kill = SOUND_DESTROY;
				break;
			}

			case GF_DISINTEGRATE:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "蒸発してしまった！";
#else
				note_kill = (plural ? " evaporate!" : " evaporates!");
#endif
				sound_kill = SOUND_DESTROY;
				break;
			}

			case GF_CHAOS:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "壊れてしまった！";
#else
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif
				sound_kill = SOUND_DESTROY;
				if (f2 & (TR2_RES_CHAOS)) ignore = TRUE;
				break;
			}

			/* Holy Fire and Hell Fire -- destroys cursed non-artifacts */
			case GF_HOLY_FIRE:
			case GF_HELL_FIRE:
			{
				if (cursed_p(o_ptr))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "壊れてしまった！";
#else
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif
					sound_kill = SOUND_DESTROY;
				}
				break;
			}

			/* Unlock chests */
			case GF_KILL_TRAP:
			case GF_KILL_DOOR:
			{
				/* Chests are noticed only if trapped or locked */
				if (o_ptr->tval == TV_CHEST)
				{
					/* Disarm/Unlock traps */
					if (o_ptr->pval > 0)
					{
						/* Disarm or Unlock */
						o_ptr->pval = (0 - o_ptr->pval);

						/* Identify */
						object_known(o_ptr);

						/* Notice */
						if (known && (o_ptr->marked & OM_FOUND))
						{
#ifdef JP
							msg_print("カチッと音がした！");
#else
							msg_print("Click!");
#endif
							obvious = TRUE;
						}
					}
				}

				break;
			}

			/* Stone to flesh */
			case GF_SOFTEN:
			{
				/* Shots to foods */
				if (o_ptr->tval == TV_SHOT)
				{
					int i;
					int oldn = o_ptr->number;
					int nidx = o_ptr->next_o_idx;
					int ox = o_ptr->ix;
					int oy = o_ptr->iy;

					if (known) obvious = TRUE;

					/* Morph to meat */
					object_wipe(o_ptr);
					i = lookup_kind(TV_FOOD, SV_FOOD_SLIME_MOLD);
					object_prep(o_ptr, i);
					o_ptr->number = (byte)(oldn - randint0(oldn / 5));
					o_ptr->next_o_idx = nidx;
					o_ptr->ix = ox;
					o_ptr->iy = oy;

					/* Redraw */
					note_spot(y, x);
					lite_spot(y, x);
				}

				/* Statue to monster */
				if (o_ptr->tval == TV_STATUE)
				{
					bool success = TRUE;
					int r_idx = 0;
					monster_race *r_ptr = NULL;

					if (o_ptr->pval > 0)
					{
						r_idx = o_ptr->pval;
						r_ptr = &r_info[r_idx];
					}
					else
					{
						success = FALSE;
					}

					/* Delete the statue */
					delete_object_idx(this_o_idx);

					if (success)
					{
						if (r_ptr->flags1 & RF1_QUESTOR)
						{
							success = FALSE;
						}
						else if (r_ptr->flags1 & RF1_UNIQUE)
						{
							int d = r_ptr->level - dun_level;

							if (d > 0)
							{
								if (!one_in_(d)) success = FALSE;
							}
						}
					}

					/* Place a monster */
					if (success) success = place_monster_one(y, x, r_idx, FALSE, FALSE, FALSE);

					/* Notice */
					if (known)
					{
#ifdef JP
						msg_print("像が活力を帯びたような気がした。");
#else
						msg_print("You feel life energy pour into the statue.");
#endif

						if (!success)
						{
#ifdef JP
							msg_print("……しかし、像は完全に溶けてしまった！");
#else
							msg_print("... but the statue has melted completely!");
#endif
						}

						obvious = TRUE;
					}
				}
			}
		}


		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Effect "observed" */
			if (known && (o_ptr->marked & OM_FOUND))
			{
				obvious = TRUE;
				object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NAME_ONLY);
			}

			/* Artifacts, and other objects, get to resist */
			if (is_art || ignore)
			{
				/* Observe the resist */
				if (known && (o_ptr->marked & OM_FOUND))
				{
#ifdef JP
					msg_format("%sは影響を受けない！",
							o_name);
#else
					msg_format("The %s %s unaffected!",
							o_name, (plural ? "are" : "is"));
#endif
				}
			}

			/* Kill it */
			else
			{
				/* Describe if needed */
				if (known && (o_ptr->marked & OM_FOUND) && note_kill)
				{
#ifdef JP
					msg_format("%sは%s", o_name, note_kill);
#else
					msg_format("The %s%s", o_name, note_kill);
#endif
					sound(sound_kill);
				}

				k_idx = o_ptr->k_idx;
				is_potion = object_is_potion(o_ptr);


				/* Delete the object */
				delete_object_idx(this_o_idx);

				/* Potions produce effects when 'shattered' */
				if (is_potion)
				{
					(void)potion_smash_effect(who, y, x, k_idx);
				}

				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to a monster.
 *
 * This routine takes a "source monster" (by index) which is mostly used to
 * determine if the player is causing the damage, and a "radius" (see below),
 * which is used to decrease the power of explosions with distance, and a
 * location, via integers which are modified by certain types of attacks
 * (polymorph and teleport being the obvious ones), a default damage, which
 * is modified as needed based on various properties, and finally a "damage
 * type" (see below).
 *
 * Note that this routine can handle "no damage" attacks (like teleport) by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.  Note that the "damage" parameter is
 * divided by the radius, so monsters not at the "epicenter" will not take
 * as much damage (or whatever)...
 *
 * Note that "polymorph" is dangerous, since a failure in "place_monster()"'
 * may result in a dereference of an invalid pointer.  XXX XXX XXX
 *
 * Various messages are produced, and damage is applied.
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" big balls of it.
 *
 * We assume that "Plasma" monsters, and "Plasma" breathers, are immune
 * to plasma.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.  If can breath nether, then it resists it as well.
 *
 * Damage reductions use the following formulas:
 *   Note that "dam = dam * 6 / (randint1(6) + 6);"
 *     gives avg damage of .655, ranging from .858 to .500
 *   Note that "dam = dam * 5 / (randint1(6) + 6);"
 *     gives avg damage of .544, ranging from .714 to .417
 *   Note that "dam = dam * 4 / (randint1(6) + 6);"
 *     gives avg damage of .444, ranging from .556 to .333
 *   Note that "dam = dam * 3 / (randint1(6) + 6);"
 *     gives avg damage of .327, ranging from .427 to .250
 *   Note that "dam = dam * 2 / (randint1(6) + 6);"
 *     gives something simple.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "useful" happen.
 */
static bool project_m(int who, int r, int y, int x, int dam, int typ, int flg)
{
	int tmp;

	cave_type *c_ptr = &cave[y][x];

	monster_type *m_ptr = &m_list[c_ptr->m_idx];
	monster_type *w_ptr = &m_list[who];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char killer[80];

	/* Is the monster "seen"? */
	bool seen = is_seen(m_ptr);

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

	/* Can the player know about this effect? */
	bool known = (m_ptr->cdis <= MAX_SIGHT);

	/* Can the player see the source of this effect? */
	bool see_s = ((who <= 0) || is_seen(w_ptr));

	/* Were the effects "irrelevant"? */
	bool skipped = FALSE;

	/* Gets the monster angry at the source of the effect? */
	bool get_angry = FALSE;

	/* Polymorph setting (true or false) */
	int do_poly = 0;

	/* Teleport setting (max distance) */
	int do_dist = 0;

	/* Confusion setting (amount to confuse) */
	int do_conf = 0;

	/* Stunning setting (amount to stun) */
	int do_stun = 0;

	/* Sleep amount (amount to sleep) */
	int do_sleep = 0;

	/* Fear amount (amount to fear) */
	int do_fear = 0;

	/* Hold the monster name */
	char m_name[80];

	/* Assume no note */
	cptr note = NULL;

	/* Assume a default death */
#ifdef JP
cptr note_dies = "は死んだ。";
#else
	cptr note_dies = " dies.";
#endif


	/* Nobody here */
	if (!c_ptr->m_idx) return (FALSE);

	/* Never affect projector */
	if (who && (c_ptr->m_idx == who)) return (FALSE);

	/* Don't affect already death monsters */
	/* Prevents problems with chain reactions of exploding monsters */
	if (m_ptr->hp < 0) return (FALSE);

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* Get the monster name (BEFORE polymorphing) */
	monster_desc(m_name, m_ptr, 0);


	/* Some monsters get "destroyed" */
	if (!monster_living(r_ptr))
	{
		/* Special note at death */
#ifdef JP
note_dies = "を倒した。";
#else
		note_dies = " is destroyed.";
#endif

	}

	/* Analyze the damage type */
	switch (typ)
	{
		/* Magic Missile -- pure damage */
		case GF_MISSILE:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Acid */
		case GF_ACID:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
#ifdef JP
note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_ACID);
			}
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
#ifdef JP
note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_ELEC);
			}
			break;
		}

		/* Fire damage */
		case GF_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
#ifdef JP
note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_FIRE);
			}
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
#ifdef JP
note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_COLD);
			}
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_IM_POIS)
			{
#ifdef JP
note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_POIS);
			}
			break;
		}

		/* Nuclear waste */
		case GF_NUKE:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & RF3_IM_POIS)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_POIS);
			}
			else if (randint1(3) == 1) do_poly = TRUE;
			break;
		}

		/* Hellfire -- hurts Evil */
		case GF_HELL_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_GOOD)
			{
				dam *= 2;
#ifdef JP
				note = "はひどい痛手を受けた。";
#else
				note = " is hit hard.";
#endif
				if (seen) r_ptr->r_flags3 |= (RF3_GOOD);
			}
			break;
		}

		/* Holy Fire -- hurts Evil, Good are immune, others _resist_ */
		case GF_HOLY_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_GOOD)
			{
				dam = 0;
#ifdef JP
				note = "には完全な耐性がある。";
#else
				note = " is immune.";
#endif
				if (seen) r_ptr->r_flags3 |= RF3_GOOD;
			}
			else if (r_ptr->flags3 & RF3_EVIL)
			{
				dam *= 2;
#ifdef JP
				note = "はひどい痛手を受けた。";
#else
				note = " is hit hard.";
#endif
				if (seen) r_ptr->r_flags3 |= RF3_EVIL;
			}
			else
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif
				dam *= 3; dam /= randint1(6) + 6;
			}
			break;
		}

		/* Arrow -- XXX no defense */
		case GF_ARROW:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Plasma -- XXX perhaps check ELEC or FIRE */
		case GF_PLASMA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_RES_PLAS)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen)
					r_ptr->r_flags3 |= (RF3_RES_PLAS);
			}
			break;
		}

		/* Nether -- see above */
		case GF_NETHER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_UNDEAD)
			{
#ifdef JP
note = "には完全な耐性がある。";
#else
				note = " is immune.";
#endif

				dam = 0;
				if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
			}
			else if (r_ptr->flags3 & RF3_RES_NETH)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;

				if (seen) r_ptr->r_flags3 |= (RF3_RES_NETH);
			}
			else if (r_ptr->flags3 & RF3_EVIL)
			{
				dam /= 2;
#ifdef JP
note = "はいくらか耐性を示した。";
#else
				note = " resists somewhat.";
#endif

				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Water (acid) damage -- Water spirits/elementals are immune */
		case GF_WATER:
		{
			if (seen) obvious = TRUE;
#if 0
			if (m_ptr->r_idx == MON_WATER_ELEM || m_ptr->r_idx == MON_UNMAKER)

			{
#ifdef JP
				note = "には完全な耐性がある。";
#else
				note = " is immune.";
#endif
				dam = 0;
			}
			else
#endif
			if (r_ptr->flags3 & RF3_RES_WATE)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flags3 |= (RF3_RES_WATE);
			}
			break;
		}

		/* Chaos -- Chaos breathers resist */
		case GF_CHAOS:
		{
			if (seen) obvious = TRUE;
			do_poly = TRUE;
			do_conf = (5 + randint1(11) + r) / (r + 1);
			if ((r_ptr->flags4 & RF4_BR_CHAO) ||
			   ((r_ptr->flags3 & RF3_DEMON) && (randint1(3) == 1)))
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				do_poly = FALSE;
			}
			break;
		}

		/* Shards -- Shard breathers resist */
		case GF_SHARDS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & RF4_BR_SHAR)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
			}
			break;
		}

		/* Rocket: Shard resistance helps */
		case GF_ROCKET:
		{
			if (seen) obvious = TRUE;

			if (r_ptr->flags4 & RF4_BR_SHAR)
			{
#ifdef JP
note = "はいくらか耐性を示した。";
#else
				note = " resists somewhat.";
#endif

				dam /= 2;
			}
			break;
		}


		/* Sound -- Sound breathers resist */
		case GF_SOUND:
		{
			if (seen) obvious = TRUE;
			do_stun = (10 + randint1(15) + r) / (r + 1);
			if (r_ptr->flags4 & RF4_BR_SOUN)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= randint1(6) + 6;
			}
			break;
		}

		/* Confusion */
		case GF_CONFUSION:
		{
			if (seen) obvious = TRUE;
			do_conf = (10 + randint1(15) + r) / (r + 1);
			if (r_ptr->flags4 & RF4_BR_CONF)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= randint1(6) + 6;
			}
			else if (r_ptr->flags3 & RF3_NO_CONF)
			{
#ifdef JP
note = "はいくらか耐性を示した。";
#else
				note = " resists somewhat.";
#endif

				dam /= 2;
			}
			break;
		}

		/* Disenchantment -- Breathers and Disenchanters resist */
		case GF_DISENCHANT:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_RES_DISE)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flags3 |= (RF3_RES_DISE);
			}
			break;
		}

		/* Nexus -- Breathers and Existers resist */
		case GF_NEXUS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_RES_NEXU)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flags3 |= (RF3_RES_NEXU);
			}
			break;
		}

		/* Force */
		case GF_FORCE:
		{
			if (seen) obvious = TRUE;
			do_stun = (randint1(15) + r) / (r + 1);
			if (r_ptr->flags4 & RF4_BR_WALL)
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
			}
			break;
		}

		/* Inertia -- breathers resist */
		case GF_INERTIA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_INER))
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
			}
			else
			{
				/* Powerful monsters can resist */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				    (r_ptr->level*2 > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
				{
					obvious = FALSE;
				}
				/* Normal monsters slow down */
				else
				{
					if (!m_ptr->slowed)
					{
#ifdef JP
						note = "の動きが遅くなった。";
#else
						note = " starts moving slower.";
#endif
					}
					m_ptr->slowed = MIN(200, m_ptr->slowed + 50);
				}
			}
			break;
		}

		/* Time -- breathers resist */
		case GF_TIME:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_TIME))
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
			}
			break;
		}

		/* Gravity -- breathers resist */
		case GF_GRAVITY:
		{
			bool resist_tele = FALSE;

			if (seen) obvious = TRUE;

			if (r_ptr->flags3 & (RF3_RES_TELE))
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には効果がなかった。";
#else
					note = " is unaffected!";
#endif

					resist_tele = TRUE;
				}
				else if (r_ptr->level > randint1(100))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には耐性がある！";
#else
					note = " resists!";
#endif

					resist_tele = TRUE;
				}
			}

			if (!resist_tele) do_dist = 10;
			else do_dist = 0;

			if (r_ptr->flags4 & (RF4_BR_GRAV))
			{
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				do_dist = 0;
			}
			else
			{
				/* 1. slowness */
				/* Powerful monsters can resist */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
				{
					obvious = FALSE;
				}
				/* Normal monsters slow down */
				else
				{
					if (!m_ptr->slowed)
					{
#ifdef JP
						note = "の動きが遅くなった。";
#else
						note = " starts moving slower.";
#endif
					}
					m_ptr->slowed = MIN(200, m_ptr->slowed + 50);
				}

				/* 2. stun */
				do_stun = damroll((p_ptr->lev / 10) + 3 , (dam)) + 1;

				/* Attempt a saving throw */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
				{
					/* Resist */
					do_stun = 0;
					/* No obvious effect */
#ifdef JP
note = "には効果がなかった。";
#else
					note = " is unaffected!";
#endif

					obvious = FALSE;
				}
			}
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			if (seen) obvious = TRUE;
			break;
		}


		/* Pure damage */
		case GF_DISINTEGRATE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_HURT_ROCK)
			{
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ROCK);
#ifdef JP
				note = "の皮膚がただれた！";
				note_dies = "は蒸発した！";
#else
				note = " loses some skin!";
				note_dies = " evaporates!";
#endif
				dam *= 2;
			}
#if 0
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				if (randint0(r_ptr->level + 10) > randint0(p_ptr->lev))
				{
#ifdef JP
					note = "には耐性がある。";
#else
					note = " resists.";
#endif
					dam >>= 3;
				}
			}
#endif
			break;
		}

		case GF_PSI:
		{
			if (seen) obvious = TRUE;

			/* PSI only works if the monster can see you! -- RG */
			if (!(los(m_ptr->fy, m_ptr->fx, py, px)))
			{
				dam = 0;
#ifdef JP
note = "はあなたが見えないので影響されない！";
#else
				note = " can't see you, and isn't affected!";
#endif

			}

			if (r_ptr->flags2 & RF2_EMPTY_MIND)
			{
				dam = 0;
#ifdef JP
note = "には完全な耐性がある！";
#else
				note = " is immune!";
#endif


				/* Memorize a flag */
				if (seen) r_ptr->r_flags2 |= (RF2_EMPTY_MIND);
			}
			else if ((r_ptr->flags2 & RF2_STUPID) ||
						(r_ptr->flags2 & RF2_WEIRD_MIND) ||
						(r_ptr->flags3 & RF3_ANIMAL) ||
						(r_ptr->level > randint1(3 * dam)))
			{
				dam /= 3;
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif


				/*
				 * Powerful demons & undead can turn a mindcrafter's
				 * attacks back on them
				 */
				if (((r_ptr->flags3 & RF3_UNDEAD) ||
					  (r_ptr->flags3 & RF3_DEMON)) &&
					  (r_ptr->level > p_ptr->lev / 2) &&
					  (randint1(2) == 1))
				{
					note = NULL;
#ifdef JP
msg_format("%^sの堕落した精神は攻撃を跳ね返した！",
    m_name);
#else
					msg_format("%^s%s corrupted mind backlashes your attack!",
					    m_name, (seen ? "'s" : "s"));
#endif

					/* Saving throw */
					if (randint0(100) < p_ptr->skill_sav)
					{
#ifdef JP
msg_print("しかし効力を跳ね返した！");
#else
						msg_print("You resist the effects!");
#endif

					}
					else
					{
						/* Injure +/- confusion */
						monster_desc(killer, m_ptr, 0x88);
						take_hit(dam, killer);  /* has already been /3 */
						if (randint1(4) == 1)
						{
							switch (randint1(4))
							{
								case 1:
									set_confused(p_ptr->confused + 3 + randint1(dam));
									break;
								case 2:
									set_stun(p_ptr->stun + randint1(dam));
									break;
								case 3:
								{
									if (r_ptr->flags3 & RF3_NO_FEAR)
#ifdef JP
note = "には効果がなかった。";
#else
										note = " is unaffected.";
#endif

									else
										set_afraid(p_ptr->afraid + 3 + randint1(dam));
									break;
								}
								default:
									if (!p_ptr->free_act)
										(void)set_paralyzed(p_ptr->paralyzed + randint1(dam));
									break;
							}
						}
					}
					dam = 0;
				}
			}

			if ((dam > 0) && (randint1(4) == 1))
			{
				switch (randint1(4))
				{
					case 1:
						do_conf = 3 + randint1(dam);
						break;
					case 2:
						do_stun = 3 + randint1(dam);
						break;
					case 3:
						do_fear = 3 + randint1(dam);
						break;
					default:
#ifdef JP
note = "は眠り込んでしまった！";
#else
						note = " falls asleep!";
#endif

						do_sleep = 3 + randint1(dam);
						break;
				}
			}

#ifdef JP
note_dies = "の精神は崩壊し、肉体は抜け殻となった。";
#else
			note_dies = " collapses, a mindless husk.";
#endif

			break;
		}

		case GF_PSI_DRAIN:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags2 & RF2_EMPTY_MIND)
			{
				dam = 0;
#ifdef JP
note = "には完全な耐性がある！";
#else
				note = " is immune!";
#endif


				/* Memorize a flag */
				if (seen) r_ptr->r_flags2 |= (RF2_EMPTY_MIND);
			}
			else if ((r_ptr->flags2 & RF2_STUPID) ||
				 (r_ptr->flags2 & RF2_WEIRD_MIND) ||
				 (r_ptr->flags3 & RF3_ANIMAL) ||
						(r_ptr->level > randint1(3 * dam)))
			{
				dam /= 3;
#ifdef JP
note = "には耐性がある。";
#else
				note = " resists.";
#endif


				/*
				 * Powerful demons & undead can turn a mindcrafter's
				 * attacks back on them
				 */
				if (((r_ptr->flags3 & RF3_UNDEAD) ||
				     (r_ptr->flags3 & RF3_DEMON)) &&
				     (r_ptr->level > p_ptr->lev / 2) &&
				     (randint1(2) == 1))
				{
					note = NULL;
#ifdef JP
msg_format("%^sの堕落した精神は攻撃を跳ね返した！",
    m_name);
#else
					msg_format("%^s%s corrupted mind backlashes your attack!",
					    m_name, (seen ? "'s" : "s"));
#endif

					/* Saving throw */
					if (randint0(100) < p_ptr->skill_sav)
					{
#ifdef JP
msg_print("あなたは効力を跳ね返した！");
#else
						msg_print("You resist the effects!");
#endif

					}
					else
					{
						/* Injure + mana drain */
						monster_desc(killer, m_ptr, 0x88);
#ifdef JP
msg_print("超能力パワーを吸いとられた！");
#else
						msg_print("Your psychic energy is drained!");
#endif

						p_ptr->csp = MAX(0, p_ptr->csp - damroll(5, dam) / 2);
						p_ptr->redraw |= PR_MANA;
						p_ptr->window |= (PW_SPELL);
						take_hit(dam, killer);  /* has already been /3 */
					}
					dam = 0;
				}
			}
			else if (dam > 0)
			{
				int b = damroll(5, dam) / 4;
#ifdef JP
msg_format("あなたは%sの苦痛を超能力パワーに変換した！",
    m_name);
#else
				msg_format("You convert %s%s pain into psychic energy!",
				    m_name, (seen ? "'s" : "s"));
#endif

				b = MIN(p_ptr->msp, p_ptr->csp + b);
				p_ptr->csp = b;
				p_ptr->redraw |= PR_MANA;
				p_ptr->window |= (PW_SPELL);
			}

#ifdef JP
note_dies = "の精神は崩壊し、肉体は抜け殻となった。";
#else
			note_dies = " collapses, a mindless husk.";
#endif

			break;
		}

		case GF_TELEKINESIS:
		{
			if (seen) obvious = TRUE;
			do_dist = 7;
			/* 1. stun */
			do_stun = damroll((p_ptr->lev / 10) + 3 , dam) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    (r_ptr->level > 5 + randint1(dam)))
			{
				/* Resist */
				do_stun = 0;
				/* No obvious effect */
				obvious = FALSE;
			}
			break;
		}

		/* Psycho spear -- powerful magic missile */
		case GF_PSY_SPEAR:
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

		case GF_DOMINATION:
		{
			if (!is_hostile(m_ptr)) break;
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    (r_ptr->flags1 & RF1_QUESTOR) ||
			    (r_ptr->flags3 & RF3_NO_CONF) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & RF3_NO_CONF)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				do_conf = 0;

				/*
				 * Powerful demons & undead can turn a mindcrafter's
				 * attacks back on them
				 */
				if (((r_ptr->flags3 & RF3_UNDEAD) ||
				     (r_ptr->flags3 & RF3_DEMON)) &&
				     (r_ptr->level > p_ptr->lev / 2) &&
				     (randint1(2) == 1))
				{
					note = NULL;
#ifdef JP
msg_format("%^sの堕落した精神は攻撃を跳ね返した！",
    m_name);
#else
					msg_format("%^s%s corrupted mind backlashes your attack!",
					    m_name, (seen ? "'s" : "s"));
#endif

					/* Saving throw */
					if (randint0(100) < p_ptr->skill_sav)
					{
#ifdef JP
msg_print("しかし効力を跳ね返した！");
#else
						msg_print("You resist the effects!");
#endif

					}
					else
					{
						/* Confuse, stun, terrify */
						switch (randint1(4))
						{
							case 1:
								set_stun(p_ptr->stun + dam / 2);
								break;
							case 2:
								set_confused(p_ptr->confused + dam / 2);
								break;
							default:
							{
								if (r_ptr->flags3 & RF3_NO_FEAR)
#ifdef JP
note = "には効果がなかった。";
#else
									note = " is unaffected.";
#endif

								else
									set_afraid(p_ptr->afraid + dam);
							}
						}
					}
				}
				else
				{
					/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
					note = " is unaffected!";
#endif

					obvious = FALSE;
				}
			}
			else
			{
				if ((dam > 29) && (randint1(100) < dam))
				{
#ifdef JP
note = "があなたに隷属した。";
#else
					note = " is in your thrall!";
#endif

					set_pet(m_ptr);
				}
				else
				{
					switch (randint1(4))
					{
						case 1:
							do_stun = dam / 2;
							break;
						case 2:
							do_conf = dam / 2;
							break;
						default:
							do_fear = dam;
					}
				}
			}

			/* No "real" damage */
			dam = 0;
			break;
		}



		/* Ice -- Cold + Cuts + Stun */
		case GF_ICE:
		{
			if (seen) obvious = TRUE;
			do_stun = (randint1(15) + 1) / (r + 1);
			if (r_ptr->flags3 & RF3_IM_COLD)
			{
#ifdef JP
note = "にはかなり耐性がある。";
#else
				note = " resists a lot.";
#endif

				dam /= 9;
				if (seen) r_ptr->r_flags3 |= (RF3_IM_COLD);
			}
			break;
		}


		/* Drain Life */
		case GF_OLD_DRAIN:
		{
			if (seen) obvious = TRUE;

			if (!monster_living(r_ptr))
			{
				if (r_ptr->flags3 & RF3_UNDEAD)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
				}

				if (r_ptr->flags3 & (RF3_DEMON))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_DEMON);
				}

#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif
				obvious = FALSE;
				dam = 0;
			}

			break;
		}

		/* Death Ray */
		case GF_DEATH_RAY:
		{
			if (seen) obvious = TRUE;

			if ((r_ptr->flags3 & RF3_UNDEAD) ||
			    (r_ptr->flags3 & RF3_NONLIVING))
			{
				if (r_ptr->flags3 & RF3_UNDEAD)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
				}

#ifdef JP
note = "には完全な耐性がある。";
#else
				note = " is immune.";
#endif

				obvious = FALSE;
				dam = 0;
			}
			else if (((r_ptr->flags1 & RF1_UNIQUE) &&
				 (randint1(888) != 666)) ||
				 (((r_ptr->level + randint1(20)) > randint1(p_ptr->lev + randint1(10))) &&
				 randint1(100) != 66))
			{
#ifdef JP
note = "には耐性がある！";
#else
				note = " resists!";
#endif

				obvious = FALSE;
				dam = 0;
			}

			break;
		}

		/* Polymorph monster (Use "dam" as "power") */
		case GF_OLD_POLY:
		{
			if (seen) obvious = TRUE;

			/* Attempt to polymorph (see below) */
			do_poly = TRUE;

			/* Powerful monsters can resist */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    (r_ptr->flags1 & RF1_QUESTOR) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
#ifdef JP
note = "には効果がなかった。";
#else
				note = " is unaffected!";
#endif

				do_poly = FALSE;
				obvious = FALSE;
			}

			/* No "real" damage */
			dam = 0;

			break;
		}


		/* Clone monsters (Ignore "dam") */
		case GF_OLD_CLONE:
		{
			bool friendly = FALSE;
			bool pet = FALSE;

			if (seen) obvious = TRUE;
			if (is_friendly(m_ptr) && (randint1(3) != 1))
				friendly = TRUE;
			if (is_pet(m_ptr) && (randint1(3) != 1))
				pet = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Speed up */
			if (m_ptr->mspeed < 150) m_ptr->mspeed += 10;

			/* Attempt to clone. */
			if (multiply_monster(c_ptr->m_idx, TRUE, friendly, pet))
			{
#ifdef JP
note = "が分裂した！";
#else
				note = " spawns!";
#endif

			}

			/* No "real" damage */
			dam = 0;

			break;
		}


		/* Heal Monster (use "dam" as amount of healing) */
		case GF_OLD_HEAL:
		{
			if (seen) obvious = TRUE;

			/* Wake up */
			m_ptr->csleep = 0;
			if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Message */
#ifdef JP
note = "は体力を回復したようだ。";
#else
			note = " looks healthier.";
#endif


			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Speed Monster (Ignore "dam") */
		case GF_OLD_SPEED:
		{
			if (seen) obvious = TRUE;

			/* Speed up */
			if (!m_ptr->hasted)
			{
#ifdef JP
				note = "の動きが速くなった。";
#else
				note = " starts moving faster.";
#endif
			}
			m_ptr->hasted = MIN(200, m_ptr->hasted + 100);

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Slow Monster (Use "dam" as "power") */
		case GF_OLD_SLOW:
		{
			if (seen) obvious = TRUE;

			/* Powerful monsters can resist */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}

			/* Normal monsters slow down */
			else
			{
					if (!m_ptr->slowed)
					{
#ifdef JP
						note = "の動きが遅くなった。";
#else
						note = " starts moving slower.";
#endif
					}
					m_ptr->slowed = MIN(200, m_ptr->slowed + 50);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Sleep (Use "dam" as "power") */
		case GF_OLD_SLEEP:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    (r_ptr->flags3 & RF3_NO_SLEEP) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & RF3_NO_SLEEP)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_SLEEP);
				}

				/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}
			else
			{
				/* Go to sleep (much) later */
#ifdef JP
note = "は眠り込んでしまった！";
#else
				note = " falls asleep!";
#endif

				do_sleep = 500;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Sleep (Use "dam" as "power") */
		case GF_STASIS:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}
			else
			{
				/* Go to sleep (much) later */
#ifdef JP
note = "は動けなくなった！";
#else
				note = " is suspended!";
#endif

				do_sleep = 500;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Charm monster */
		case GF_CHARM:
		{
			dam += (adj_con_fix[p_ptr->stat_ind[A_CHR]] - 1);

			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    (r_ptr->flags1 & RF1_QUESTOR) ||
			    (r_ptr->flags3 & RF3_NO_CONF) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 5))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & RF3_NO_CONF)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}
			else if (p_ptr->aggravate)
			{
#ifdef JP
note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

			}
			else
			{
#ifdef JP
note = "は突然友好的になったようだ！";
#else
				note = " suddenly seems friendly!";
#endif

				set_pet(m_ptr);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Control undead */
		case GF_CONTROL_UNDEAD:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
				 (r_ptr->flags1 & RF1_QUESTOR) ||
			  (!(r_ptr->flags3 & RF3_UNDEAD)) ||
				 (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}
			else if (p_ptr->aggravate)
			{
#ifdef JP
note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

			}
			else
			{
#ifdef JP
note = "は既にあなたの奴隷だ！";
#else
				note = " is in your thrall!";
#endif

				set_pet(m_ptr);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Tame animal */
		case GF_CONTROL_ANIMAL:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				 (r_ptr->flags1 & (RF1_QUESTOR)) ||
			  (!(r_ptr->flags3 & (RF3_ANIMAL))) ||
				 (r_ptr->flags3 & (RF3_NO_CONF)) ||
				 (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}
			else if (p_ptr->aggravate)
			{
#ifdef JP
note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

			}
			else
			{
#ifdef JP
note = "はなついた。";
#else
				note = " is tamed!";
#endif

				set_pet(m_ptr);
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Confusion (Use "dam" as "power") */
		case GF_OLD_CONF:
		{
			if (seen) obvious = TRUE;

			/* Get confused later */
			do_conf = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			    (r_ptr->flags3 & (RF3_NO_CONF)) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_CONF);
				}

				/* Resist */
				do_conf = 0;

				/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		case GF_STUN:
		{
			if (seen) obvious = TRUE;

			do_stun = damroll((p_ptr->lev / 10) + 3 , (dam)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Resist */
				do_stun = 0;

				/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}




		/* Lite, but only hurts susceptible creatures */
		case GF_LITE_WEAK:
		{
			/* Hurt by light */
			if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				/* Obvious effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_LITE);

				/* Special effect */
#ifdef JP
note = "は光に身をすくめた！";
note_dies = "は光を受けてしぼんでしまった！";
#else
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
#endif

			}

			/* Normally no damage */
			else
			{
				/* No damage */
				dam = 0;
			}

			break;
		}



		/* Lite -- opposite of Dark */
		case GF_LITE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BR_LITE))
			{
#ifdef JP
note = "には耐性がある！";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= (randint1(6) + 6);
			}
			else if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_LITE);
#ifdef JP
note = "は光に身をすくめた！";
note_dies = "は光を受けてしぼんでしまった！";
#else
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
#endif

				dam *= 2;
			}
			break;
		}


		/* Dark -- opposite of Lite */
		case GF_DARK:
		{
			if (seen) obvious = TRUE;

			/* Likes darkness... */
			if ((r_ptr->flags4 & (RF4_BR_DARK)) ||
			    (r_ptr->flags3 & RF3_ORC) ||
			    (r_ptr->flags3 & RF3_HURT_LITE))
			{
#ifdef JP
note = "には耐性がある！";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= (randint1(6) + 6);
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
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ROCK);

				/* Cute little message */
#ifdef JP
				note = "の皮膚がただれた！";
				note_dies = "はドロドロに溶けた！";
#else
				note = " loses some skin!";
				note_dies = " dissolves!";
#endif
			}

			/* Usually, ignore the effects */
			else
			{
				/* No damage */
				dam = 0;
			}

			break;
		}


		/* Teleport undead (Use "dam" as "power") */
		case GF_AWAY_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				bool resists_tele = FALSE;

				if (r_ptr->flags3 & (RF3_RES_TELE))
				{
					if (r_ptr->flags1 & (RF1_UNIQUE))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には効果がなかった！";
#else
						note = " is unaffected!";
#endif

						resists_tele = TRUE;
					}
					else if (r_ptr->level > randint1(100))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には耐性がある！";
#else
						note = " resists!";
#endif

						resists_tele = TRUE;
					}
				}

				if (!resists_tele)
				{
					if (seen) obvious = TRUE;
					if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);
					do_dist = dam;
				}
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Teleport evil (Use "dam" as "power") */
		case GF_AWAY_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				bool resists_tele = FALSE;

				if (r_ptr->flags3 & (RF3_RES_TELE))
				{
					if (r_ptr->flags1 & (RF1_UNIQUE))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には効果がなかった！";
#else
						note = " is unaffected!";
#endif

						resists_tele = TRUE;
					}
					else if (r_ptr->level > randint1(100))
					{
						if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には耐性がある！";
#else
						note = " resists!";
#endif

						resists_tele = TRUE;
					}
				}

				if (!resists_tele)
				{
					if (seen) obvious = TRUE;
					if (seen) r_ptr->r_flags3 |= (RF3_EVIL);
					do_dist = dam;
				}
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Teleport monster (Use "dam" as "power") */
		case GF_AWAY_ALL:
		{
			bool resists_tele = FALSE;

			if (r_ptr->flags3 & (RF3_RES_TELE))
			{
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には効果がなかった！";
#else
					note = " is unaffected!";
#endif

					resists_tele = TRUE;
				}
				else if (r_ptr->level > randint1(100))
				{
					if (seen) r_ptr->r_flags3 |= RF3_RES_TELE;
#ifdef JP
note = "には耐性がある！";
#else
					note = " resists!";
#endif

					resists_tele = TRUE;
				}
			}

			if (!resists_tele)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				do_dist = dam;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Turn undead (Use "dam" as "power") */
		case GF_TURN_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				/* Learn about type */
				if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
				{
					/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
					note = " is unaffected!";
#endif

					obvious = FALSE;
					do_fear = 0;
				}
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Turn evil (Use "dam" as "power") */
		case GF_TURN_EVIL:
		{
			/* Only affect evil */
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				/* Learn about type */
				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Apply some fear */
				do_fear = damroll(3, (dam / 2)) + 1;

				/* Attempt a saving throw */
				if (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
				{
					/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
					note = " is unaffected!";
#endif

					obvious = FALSE;
					do_fear = 0;
				}
			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Turn monster (Use "dam" as "power") */
		case GF_TURN_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Apply some fear */
			do_fear = damroll(3, (dam / 2)) + 1;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			    (r_ptr->flags3 & (RF3_NO_FEAR)) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* No obvious effect */
#ifdef JP
note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
				do_fear = 0;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}


		/* Dispel undead */
		case GF_DISP_UNDEAD:
		{
			/* Only affect undead */
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				/* Learn about type */
				if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
#ifdef JP
note = "は身震いした。";
note_dies = "はドロドロに溶けた！";
#else
				note = " shudders.";
				note_dies = " dissolves!";
#endif

			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;
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
				if (seen) r_ptr->r_flags3 |= (RF3_EVIL);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
#ifdef JP
note = "は身震いした。";
note_dies = "はドロドロに溶けた！";
#else
				note = " shudders.";
				note_dies = " dissolves!";
#endif

			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;
			}

			break;
		}

		/* Dispel good */
		case GF_DISP_GOOD:
		{
			/* Only affect good */
			if (r_ptr->flags3 & (RF3_GOOD))
			{
				/* Learn about type */
				if (seen) r_ptr->r_flags3 |= (RF3_GOOD);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
#ifdef JP
note = "は身震いした。";
note_dies = "はドロドロに溶けた！";
#else
				note = " shudders.";
				note_dies = " dissolves!";
#endif

			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;
			}

			break;
		}

		/* Dispel living */
		case GF_DISP_LIVING:
		{
			/* Only affect non-undead */
			if (monster_living(r_ptr))
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
#ifdef JP
note = "は身震いした。";
note_dies = "はドロドロに溶けた！";
#else
				note = " shudders.";
				note_dies = " dissolves!";
#endif

			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;
			}

			break;
		}

		/* Dispel demons */
		case GF_DISP_DEMON:
		{
			/* Only affect demons */
			if (r_ptr->flags3 & (RF3_DEMON))
			{
				/* Learn about type */
				if (seen) r_ptr->r_flags3 |= (RF3_DEMON);

				/* Obvious */
				if (seen) obvious = TRUE;

				/* Message */
#ifdef JP
note = "は身震いした。";
note_dies = "はドロドロに溶けた！";
#else
				note = " shudders.";
				note_dies = " dissolves!";
#endif

			}

			/* Others ignore */
			else
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;
			}

			break;
		}

		/* Dispel monster */
		case GF_DISP_ALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
#ifdef JP
note = "は身震いした。";
note_dies = "はドロドロに溶けた！";
#else
			note = " shudders.";
			note_dies = " dissolves!";
#endif


			break;
		}

		case GF_JUMP_ATTACK:
		{
			if (seen) obvious = TRUE;
			skipped = TRUE;

			if ((y != py) || (x != px))
			{
#ifdef JP
				msg_print("素早く相手の懐に入り込んだ！");
#else
				msg_format("You quickly jump in and attack %s!", m_name);
#endif
			}
			if (c_ptr->m_idx)
				py_attack(y, x);
			else
			{
#ifdef JP
				msg_print("攻撃は空を切った。");
#else
				msg_print("You attack the empty air.");
#endif
			}
			break;
		}

		/* Annihilation */
		case GF_GENOCIDE:
		{
			if (seen) obvious = TRUE;

			if ((p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM))
			{
#ifdef JP
				msg_format("%^sには効果がなかった。", m_name);
#else
				msg_format("%^s is unaffected. ", m_name);
#endif
			}
			else if(remove_monster(dam, c_ptr->m_idx, "消滅"))
			{
#ifdef JP
				msg_format("%sは消滅した！",m_name);
#else
				msg_format("%^s disappered!",m_name);
#endif
			}
			else
			{
				get_angry = TRUE;
			}

			skipped = TRUE;
			dam = 0;
			break;
		}

		/* Stone to Flesh */
		case GF_SOFTEN:
		{
			/* Hurt by rock remover */
			if (r_ptr->flags3 & (RF3_HURT_ROCK))
			{
				/* Notice effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ROCK);

				if ((p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM) ||
					(r_ptr->flags1 & RF1_UNIQUE) ||
					(r_ptr->flags1 & RF1_QUESTOR))
				{
					/* Take a damage */
				}
				else if (!ironman_hengband || (r_ptr->level < randint1(150)))
				{
					int i;
					object_type forge;
#ifdef JP
					msg_format("%^sはドロドロに溶けた！", m_name);
#else
					msg_format("%^s dissolves!", m_name);
#endif
					delete_monster(y, x);
					skipped = TRUE;

					i = lookup_kind(TV_FOOD, SV_FOOD_SLIME_MOLD);
					object_prep(&forge, i);
					forge.number = (byte)rand_range(10, 20);
					drop_near(&forge, -1, y, x);
				}
			}

			/* Usually, ignore the effects */
			else
			{
				/* No damage */
				dam = 0;
			}

			break;
		}

		case GF_DIST_ATTACK:
		{
			if (seen) obvious = TRUE;
			skipped = TRUE;

			if (c_ptr->m_idx)
				py_attack(y, x);
			else
			{
#ifdef JP
				msg_print("攻撃は空を切った。");
#else
				msg_print("You attack the empty air.");
#endif
			}
			break;
		}

		/* Default */
		default:
		{
			/* Irrelevant */
			skipped = TRUE;

			break;
		}
	}



	/* Absolutely no effect */
	if (skipped) return (FALSE);


	/* "Unique" monsters cannot be polymorphed */
	if (r_ptr->flags1 & (RF1_UNIQUE)) do_poly = FALSE;

	/* Quest monsters cannot be polymorphed */
	if (r_ptr->flags1 & RF1_QUESTOR) do_poly = FALSE;

	/* "Unique" and "quest" monsters can only be "killed" by the player. */
	if ((r_ptr->flags1 & RF1_UNIQUE) ||
	    (r_ptr->flags3 & RF3_UNIQUE_7) ||
	    (r_ptr->flags1 & RF1_QUESTOR))
	{
		if (who && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}

	/* Modify the damage */
	if (typ == GF_PSY_SPEAR){
	/* GF_PSY_SPEAR bypass invulnerability */
		if (m_ptr->invulner)
#ifdef JP
			msg_print("光の剣が無敵のバリアを斬り裂いた！");
#else
			msg_print("The barrier is penetrated!");
#endif
	}
	else
	{
		/* Modify the damage */
		dam = mon_damage_mod(m_ptr, dam, 0);
	}

	/* Check for death */
	if (dam > m_ptr->hp)
	{
		/* Extract method of death */
		note = note_dies;
	}

	/* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
	else if (do_poly && (randint1(90) > r_ptr->level))
	{
		if (polymorph_monster(y, x))
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Monster polymorphs */
#ifdef JP
note = "が変身した！";
#else
			note = " changes!";
#endif


			/* Turn off the damage */
			dam = 0;

			/* Hack -- Get new monster */
			m_ptr = &m_list[c_ptr->m_idx];

			/* Hack -- Get new race */
			r_ptr = &r_info[m_ptr->r_idx];
		}
		else
		{
			/* No polymorph */
#ifdef JP
note = "には効果がなかった！";
#else
			note = " is unaffected!";
#endif

		}
	}

	/* Handle "teleport" */
	else if (do_dist)
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Message */
#ifdef JP
note = "が消え去った！";
#else
		note = " disappears!";
#endif

		/* Teleport */
		teleport_away(c_ptr->m_idx, do_dist);

		/* Hack -- get new location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Hack -- get new grid */
		c_ptr = &cave[y][x];
	}

	/* Sound and Impact breathers never stun */
	else if (do_stun &&
	    !(r_ptr->flags4 & (RF4_BR_SOUN)) &&
	    !(r_ptr->flags4 & (RF4_BR_WALL)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Get confused */
		if (m_ptr->stunned)
		{
#ifdef JP
note = "はひどくもうろうとした。";
#else
			note = " is more dazed.";
#endif

			tmp = m_ptr->stunned + (do_stun / 2);
		}
		else
		{
#ifdef JP
note = "はもうろうとした。";
#else
			note = " is dazed.";
#endif

			tmp = do_stun;
		}

		/* Apply stun */
		m_ptr->stunned = (tmp < 200) ? tmp : 200;

		/* Get angry */
		get_angry = TRUE;
	}

	/* Confusion and Chaos breathers (and sleepers) never confuse */
	else if (do_conf &&
		 !(r_ptr->flags3 & (RF3_NO_CONF)) &&
		 !(r_ptr->flags4 & (RF4_BR_CONF)) &&
		 !(r_ptr->flags4 & (RF4_BR_CHAO)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Already partially confused */
		if (m_ptr->confused)
		{
#ifdef JP
note = "はさらに混乱したようだ。";
#else
			note = " looks more confused.";
#endif

			tmp = m_ptr->confused + (do_conf / 2);
		}

		/* Was not confused */
		else
		{
#ifdef JP
note = "は混乱したようだ。";
#else
			note = " looks confused.";
#endif

			tmp = do_conf;
		}

		/* Apply confusion */
		m_ptr->confused = (tmp < 200) ? tmp : 200;

		/* Get angry */
		get_angry = TRUE;
	}


	/* Fear */
	if (do_fear)
	{
		/* Increase fear */
		tmp = m_ptr->monfear + do_fear;

		/* Set fear */
		m_ptr->monfear = (tmp < 200) ? tmp : 200;

		/* Get angry */
		get_angry = TRUE;
	}


	/* If another monster did the damage, hurt the monster by hand */
	if (who)
	{
		/* Redraw (later) if needed */
		if (p_ptr->health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);

		/* Wake the monster up */
		m_ptr->csleep = 0;
		if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

		/* Hurt the monster */
		m_ptr->hp -= dam;

		/* Dead monster */
		if (m_ptr->hp < 0)
		{
			bool sad = FALSE;

			if (is_pet(m_ptr) && !(m_ptr->ml))
				sad = TRUE;

			/* Generate treasure, etc */
			monster_death(c_ptr->m_idx, FALSE);

			/* Delete the monster */
			delete_monster_idx(c_ptr->m_idx);

			/* Give detailed messages if destroyed */
			if (known && note)
			{
				if (see_s)
				{
#ifdef JP
msg_format("%s%s", m_name, note);
#else
					msg_format("%^s%s", m_name, note);
#endif

				}
				else
				{
					mon_fight = TRUE;
				}
			}

			if (sad)
			{
#ifdef JP
msg_print("少し悲しい気分がした。");
#else
				msg_print("You feel sad for a moment.");
#endif

			}
		}

		/* Damaged monster */
		else
		{
			/* Give detailed messages if visible or destroyed */
			if (note && seen)
			{
#ifdef JP
msg_format("%^s%s", m_name, note);
#else
				msg_format("%^s%s", m_name, note);
#endif

			}
			/* Hack -- Pain message */
			else if (see_s)
			{
				message_pain(c_ptr->m_idx, dam);
			}

			/* Hack -- handle sleep */
			if (do_sleep) m_ptr->csleep = do_sleep;
		}
	}

	/* If the player did it, give him experience, check fear */
	else
	{
		bool fear = FALSE;

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(c_ptr->m_idx, dam, &fear, note_dies))
		{
			/* Dead monster */
		}

		/* Damaged monster */
		else
		{
			/* HACK - anger the monster before showing the sleep message */
			if (do_sleep) anger_monster(m_ptr);

			/* Give detailed messages if visible or destroyed */
			if (note && seen)
			{
#ifdef JP
msg_format("%^s%s", m_name, note);
#else
				msg_format("%^s%s", m_name, note);
#endif

			}
			/* Hack -- Pain message */
			else if (see_s)
			{
				message_pain(c_ptr->m_idx, dam);
			}
			else
			{
				mon_fight = TRUE;
			}

			/* Anger monsters */
			if (((dam > 0) || get_angry) && !do_sleep)
				anger_monster(m_ptr);

			/* Take note */
			if ((fear || do_fear) && (m_ptr->ml))
			{
				/* Sound */
				sound(SOUND_FLEE);

				/* Message */
#ifdef JP
msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
				msg_format("%^s flees in terror!", m_name);
#endif

			}

			/* Hack -- handle sleep */
			if (do_sleep) m_ptr->csleep = do_sleep;
		}
	}


	/* XXX XXX XXX Verify this code */

	/* Update the monster */
	update_mon(c_ptr->m_idx, FALSE);

	/* Redraw the monster grid */
	lite_spot(y, x);


	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}

	if (ironman_hengband && (dam > 0) && !is_pet(m_ptr) && !is_friendly(m_ptr))
	{
		if (!who)
		{
			/* Set counter target to !los player */
			if (!(flg & PROJECT_NO_COUNTER))
			{
				m_ptr->target_y = py;
				m_ptr->target_x = px;
			}
		}
		else if (is_pet(&m_list[who]) && (m_ptr->target_y != py) && (m_ptr->target_x != px))
		{
			m_ptr->target_y = m_list[who].fy;
			m_ptr->target_x = m_list[who].fx;
		}
	}

	/* Track it */
	project_m_n++;
	project_m_x = x;
	project_m_y = y;


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
 * NOTE (Zangband): 'Bolt' attacks can be reflected back, so we need
 * to know if this is actually a ball or a bolt spell
 *
 *
 * We return "TRUE" if any "obvious" effects were observed.  XXX XXX Actually,
 * we just assume that the effects were obvious, for historical reasons.
 */
static bool project_p(int who, int r, int y, int x, int dam, int typ, int a_rad)
{
	int k = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = FALSE;

	/* Source monster */
	monster_type *m_ptr;

	/* Monster name (for attacks) */
	char m_name[80];

	/* Monster name (for damage) */
	char killer[80];

	/* Hack -- messages */
	cptr act = NULL;

	int get_damage = 0;


	/* Player is not here */
	if ((x != px) || (y != py)) return (FALSE);

	/* Player cannot hurt himself */
	if (!who) return (FALSE);


	if (p_ptr->reflect && !a_rad && (randint1(10) != 1))
	{
		int t_y, t_x;
		int max_attempts = 10;

#ifdef JP
if (blind) msg_print("何かが跳ね返った！");
    else msg_print("攻撃が跳ね返った！");
#else
		if (blind) msg_print("Something bounces!");
		else msg_print("The attack bounces!");
#endif


		/* Choose 'new' target */
		do
		{
			t_y = m_list[who].fy - 1 + randint1(3);
			t_x = m_list[who].fx - 1 + randint1(3);
			max_attempts--;
		}
		while (max_attempts && in_bounds2u(t_y, t_x) &&
		     !(player_has_los_bold(t_y, t_x)));

		if (max_attempts < 1)
		{
			t_y = m_list[who].fy;
			t_x = m_list[who].fx;
		}

		project(0, 0, t_y, t_x, dam, typ, (PROJECT_STOP | PROJECT_KILL));

		disturb(1, 0);
		return TRUE;
	}

	/* XXX XXX XXX */
	/* Limit maximum damage */
	if (dam > 1600) dam = 1600;

	/* Reduce damage by distance */
	dam = (dam + r) / (r + 1);


	/* If the player is blind, be more descriptive */
	if (blind) fuzzy = TRUE;


	/* Get the source monster */
	m_ptr = &m_list[who];

	/* Get the monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Get the monster's real name */
	monster_desc(killer, m_ptr, 0x88);


	/* Analyze the damage */
	switch (typ)
	{
		/* Standard damage -- hurts inventory too */
		case GF_ACID:
		{
#ifdef JP
if (fuzzy) msg_print("酸で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by acid!");
#endif

			get_damage = acid_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_FIRE:
		{
#ifdef JP
if (fuzzy) msg_print("火炎で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by fire!");
#endif

			get_damage = fire_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_COLD:
		{
#ifdef JP
if (fuzzy) msg_print("冷気で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by cold!");
#endif

			get_damage = cold_dam(dam, killer);
			break;
		}

		/* Standard damage -- hurts inventory too */
		case GF_ELEC:
		{
#ifdef JP
if (fuzzy) msg_print("電撃で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by lightning!");
#endif

			get_damage = elec_dam(dam, killer);
			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
#ifdef JP
if (fuzzy) msg_print("毒で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by poison!");
#endif

			if (p_ptr->resist_pois) dam = (dam + 2) / 3;
			if (p_ptr->oppose_pois) dam = (dam + 2) / 3;

			if ((!(p_ptr->oppose_pois || p_ptr->resist_pois)) &&
			     randint1(HURT_CHANCE) == 1)
			{
				do_dec_stat(A_CON);
			}

			get_damage = take_hit(dam, killer);

			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				set_poisoned(p_ptr->poisoned + randint0(dam) + 10);
			}
			break;
		}

		/* Standard damage -- also poisons / mutates player */
		case GF_NUKE:
		{
#ifdef JP
if (fuzzy) msg_print("放射能で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by radiation!");
#endif

			if (p_ptr->resist_pois) dam = (2 * dam + 2) / 5;
			if (p_ptr->oppose_pois) dam = (2 * dam + 2) / 5;
			get_damage = take_hit(dam, killer);
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				set_poisoned(p_ptr->poisoned + randint0(dam) + 10);

				if (randint1(5) == 1) /* 6 */
				{
#ifdef JP
msg_print("奇形的な変身を遂げた！");
#else
					msg_print("You undergo a freakish metamorphosis!");
#endif

					if (randint1(4) == 1) /* 4 */
						do_poly_self();
					else
						mutate_player();
				}

				if (randint1(6) == 1)
				{
					inven_damage(set_acid_destroy, 2);
				}
			}
			break;
		}

		/* Standard damage */
		case GF_MISSILE:
		{
#ifdef JP
if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Holy Orb -- Player only takes partial damage */
		case GF_HOLY_FIRE:
		{
#ifdef JP
if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

#if 0
			if (p_ptr->realm1 == REALM_LIFE || p_ptr->realm2 == REALM_LIFE)
				dam /= 2;
			else if (p_ptr->realm1 == REALM_DEATH || p_ptr->realm2 == REALM_DEATH)
				dam *= 2;
#endif
			get_damage = take_hit(dam, killer);
			break;
		}

		case GF_HELL_FIRE:
		{
#ifdef JP
if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

#if 0
			if (p_ptr->realm1 == REALM_DEATH || p_ptr->realm2 == REALM_DEATH)
				dam /= 2;
			else if (p_ptr->realm1 == REALM_LIFE || p_ptr->realm2 == REALM_LIFE)
				dam *= 2;
#endif
			get_damage = take_hit(dam, killer);
			break;
		}

		/* Arrow -- XXX no dodging */
		case GF_ARROW:
		{
#ifdef JP
if (fuzzy) msg_print("何か鋭いもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something sharp!");
#endif

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Plasma -- XXX No resist */
		case GF_PLASMA:
		{
#ifdef JP
if (fuzzy) msg_print("何かとても熱いものでで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something *HOT*!");
#endif

			get_damage = take_hit(dam, killer);

			if (!p_ptr->resist_sound)
			{
				int k = (randint1((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}

			if (!(p_ptr->resist_fire ||
			      p_ptr->oppose_fire ||
			      p_ptr->immune_fire))
			{
				inven_damage(set_acid_destroy, 3);
			}

			break;
		}

		/* Nether -- drain experience */
		case GF_NETHER:
		{
#ifdef JP
			if (fuzzy) msg_print("地獄の力で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by nether forces!");
#endif

			if (p_ptr->resist_neth)
			{
				dam *= 6; dam /= (randint1(6) + 6);
			}
			else
			{
				if (p_ptr->hold_life && (randint0(100) < 75))
				{
#ifdef JP
					msg_print("しかし自己の生命力を守りきった！");
#else
					msg_print("You keep hold of your life force!");
#endif
				}
				else if (p_ptr->hold_life)
				{
#ifdef JP
					msg_print("生命力が少し体から抜け落ちた気がする！");
#else
					msg_print("You feel your life slipping away!");
#endif
					lose_exp(200 + (p_ptr->exp / 1000) * MON_DRAIN_LIFE);
				}
				else
				{
#ifdef JP
					msg_print("生命力が体から吸い取られた気がする！");
#else
					msg_print("You feel your life draining away!");
#endif
					lose_exp(200 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
				}
			}

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Water -- stun/confuse */
		case GF_WATER:
		{
#ifdef JP
if (fuzzy) msg_print("何か湿ったもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something wet!");
#endif

			if (!p_ptr->resist_sound)
			{
				set_stun(p_ptr->stun + randint1(40));
			}
			if (!p_ptr->resist_conf)
			{
				set_confused(p_ptr->confused + randint1(5) + 5);
			}

			if (randint1(5) == 1)
			{
				inven_damage(set_cold_destroy, 3);
			}

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Chaos -- many effects */
		case GF_CHAOS:
		{
#ifdef JP
if (fuzzy) msg_print("無秩序の波動で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by a wave of anarchy!");
#endif

			if (p_ptr->resist_chaos)
			{
				dam *= 6; dam /= (randint1(6) + 6);
			}
			if (!p_ptr->resist_conf)
			{
				(void)set_confused(p_ptr->confused + randint0(20) + 10);
			}
			if (!p_ptr->resist_chaos)
			{
				(void)set_image(p_ptr->image + randint1(10));
				if (randint1(3) == 1)
				{
#ifdef JP
msg_print("あなたの身体はカオスの力で捻じ曲げられた！");
#else
					msg_print("Your body is twisted by chaos!");
#endif

					(void)gain_random_mutation(0);
				}
			}
			if (!p_ptr->resist_neth && !p_ptr->resist_chaos)
			{
				if (p_ptr->hold_life && (randint0(100) < 75))
				{
#ifdef JP
msg_print("しかし自己の生命力を守りきった！");
#else
					msg_print("You keep hold of your life force!");
#endif

				}
				else if (p_ptr->hold_life)
				{
#ifdef JP
msg_print("生命力が少し体から抜け落ちた気がする！");
#else
					msg_print("You feel your life slipping away!");
#endif

					lose_exp(500 + (p_ptr->exp / 1000) * MON_DRAIN_LIFE);
				}
				else
				{
#ifdef JP
msg_print("生命力が体から吸い取られた気がする！");
#else
					msg_print("You feel your life draining away!");
#endif

					lose_exp(5000 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
				}
			}
			if (!p_ptr->resist_chaos || (randint1(9) == 1))
			{
				inven_damage(set_elec_destroy, 2);
				inven_damage(set_fire_destroy, 2);
			}
			get_damage = take_hit(dam, killer);
			break;
		}

		/* Shards -- mostly cutting */
		case GF_SHARDS:
		{
#ifdef JP
if (fuzzy) msg_print("何か鋭いもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something sharp!");
#endif

			if (p_ptr->resist_shard)
			{
				dam *= 6; dam /= (randint1(6) + 6);
			}
			else
			{
				(void)set_cut(p_ptr->cut + dam);
			}

			if (!p_ptr->resist_shard || (randint1(13) == 1))
			{
				inven_damage(set_cold_destroy, 2);
			}

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Sound -- mostly stunning */
		case GF_SOUND:
		{
#ifdef JP
if (fuzzy) msg_print("轟音で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by a loud noise!");
#endif

			if (p_ptr->resist_sound)
			{
				dam *= 5; dam /= (randint1(6) + 6);
			}
			else
			{
				int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}

			if (!p_ptr->resist_sound || (randint1(13) == 1))
			{
				inven_damage(set_cold_destroy, 2);
			}

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Pure confusion */
		case GF_CONFUSION:
		{
#ifdef JP
if (fuzzy) msg_print("何か混乱するもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something puzzling!");
#endif

			if (p_ptr->resist_conf)
			{
				dam *= 5; dam /= (randint1(6) + 6);
			}
			if (!p_ptr->resist_conf)
			{
				(void)set_confused(p_ptr->confused + randint1(20) + 10);
			}
			get_damage = take_hit(dam, killer);
			break;
		}

		/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
#ifdef JP
if (fuzzy) msg_print("何かさえないものでで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something static!");
#endif

			if (p_ptr->resist_disen)
			{
				dam *= 6; dam /= (randint1(6) + 6);
			}
			else
			{
				(void)apply_disenchant(0);
			}
			get_damage = take_hit(dam, killer);
			break;
		}

		/* Nexus -- see above */
		case GF_NEXUS:
		{
#ifdef JP
if (fuzzy) msg_print("何か奇妙なもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something strange!");
#endif

			if (p_ptr->resist_nexus)
			{
				dam *= 6; dam /= (randint1(6) + 6);
			}
			else
			{
				apply_nexus(m_ptr);
			}
			get_damage = take_hit(dam, killer);
			break;
		}

		/* Force -- mostly stun */
		case GF_FORCE:
		{
#ifdef JP
if (fuzzy) msg_print("運動エネルギーで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by kinetic force!");
#endif

			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint1(20));
			}
			get_damage = take_hit(dam, killer);
			break;
		}


		/* Rocket -- stun, cut */
		case GF_ROCKET:
		{
#ifdef JP
if (fuzzy) msg_print("爆発があった！");
#else
			if (fuzzy) msg_print("There is an explosion!");
#endif

			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint1(20));
			}
			if (p_ptr->resist_shard)
			{
				dam /= 2;
			}
			else
			{
				(void)set_cut(p_ptr->cut + (dam / 2));
			}

			if (!p_ptr->resist_shard || (randint1(12) == 1))
			{
				inven_damage(set_cold_destroy, 3);
			}

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Inertia -- slowness */
		case GF_INERTIA:
		{
#ifdef JP
if (fuzzy) msg_print("何か遅いもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something slow!");
#endif

			(void)set_slow(p_ptr->slow + randint0(4) + 4);
			get_damage = take_hit(dam, killer);
			break;
		}

		/* Lite -- blinding */
		case GF_LITE:
		{
#ifdef JP
if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

			if (p_ptr->resist_lite)
			{
				dam *= 4; dam /= (randint1(6) + 6);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint1(5) + 2);
			}
#if 0
			if (p_ptr->prace == RACE_VAMPIRE)
			{
#ifdef JP
				msg_print("光で肉体が焦がされた！");
#else
				msg_print("The light scorches your flesh!");
#endif
				dam *= 2;
			}
#endif
			get_damage = take_hit(dam, killer);

			if (p_ptr->tim_wraith)
			{
				p_ptr->tim_wraith = 0;
				p_ptr->wraith_form = FALSE;
#ifdef JP
msg_print("閃光のため非物質的な影の存在でいられなくなった。");
#else
				msg_print("The light forces you out of your incorporeal shadow form.");
#endif
				/* Redraw map and status bar */
				p_ptr->redraw |= (PR_MAP | PR_STATUS);
				/* Update monsters */
				p_ptr->update |= (PU_MONSTERS);
				/* Window stuff */
				p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
			}

			break;
		}

		/* Dark -- blinding */
		case GF_DARK:
		{
#ifdef JP
			if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

			if (p_ptr->wraith_form) hp_player(dam);
			else
			{
				if (p_ptr->resist_dark)
				{
					dam *= 4; dam /= (randint1(6) + 6);
#if 0
					if (p_ptr->prace == RACE_VAMPIRE) dam = 0;
#endif
				}
				else if (!blind && !p_ptr->resist_blind)
				{
					(void)set_blind(p_ptr->blind + randint1(5) + 2);
				}
				get_damage = take_hit(dam, killer);
			}
			break;
		}

		/* Time -- bolt fewer effects XXX */
		case GF_TIME:
		{
#ifdef JP
			if (fuzzy) msg_print("過去からの衝撃に攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by a blast from the past!");
#endif

			if (p_ptr->muta & MUT_RES_TIME)
			{
				dam *= 4;
				dam /= (randint1(6) + 6);
#ifdef JP
				msg_print("時間が通り過ぎていく気がする。");
#else
				msg_print("You feel as if time is passing you by.");
#endif
			}
			else
			{
				switch (randint1(10))
				{
					case 1: case 2: case 3: case 4: case 5:
					{
#ifdef JP
msg_print("人生が逆戻りした気がする。");
#else
						msg_print("You feel life has clocked back.");
#endif

						lose_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
						break;
					}

					case 6: case 7: case 8: case 9:
					{
						switch (randint1(6))
						{
#ifdef JP
case 1: k = A_STR; act = "強く"; break;
case 2: k = A_INT; act = "聡明で"; break;
case 3: k = A_WIS; act = "賢明で"; break;
case 4: k = A_DEX; act = "器用で"; break;
case 5: k = A_CON; act = "健康で"; break;
case 6: k = A_CHR; act = "美しく"; break;
#else
							case 1: k = A_STR; act = "strong"; break;
							case 2: k = A_INT; act = "bright"; break;
							case 3: k = A_WIS; act = "wise"; break;
							case 4: k = A_DEX; act = "agile"; break;
							case 5: k = A_CON; act = "hale"; break;
							case 6: k = A_CHR; act = "beautiful"; break;
#endif

						}

#ifdef JP
msg_format("あなたは以前ほど%sなくなってしまった...。", act);
#else
						msg_format("You're not as %s as you used to be...", act);
#endif


						p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
						if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
						p_ptr->update |= (PU_BONUS);
						break;
					}

					case 10:
					{
#ifdef JP
msg_print("あなたは以前ほど力強くなくなってしまった...。");
#else
						msg_print("You're not as powerful as you used to be...");
#endif


						for (k = 0; k < 6; k++)
						{
							p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
							if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
						}
						p_ptr->update |= (PU_BONUS);
						break;
					}
				}
			}
			get_damage = take_hit(dam, killer);
			break;
		}

		/* Gravity -- stun plus slowness plus teleport */
		case GF_GRAVITY:
		{
#ifdef JP
if (fuzzy) msg_print("何か重いものでで攻撃された！");
msg_print("周辺の重力がゆがんだ。");
#else
			if (fuzzy) msg_print("You are hit by something heavy!");
			msg_print("Gravity warps around you.");
#endif

			teleport_player(5);
			if (!p_ptr->ffall)
				(void)set_slow(p_ptr->slow + randint0(4) + 4);
			if (!(p_ptr->resist_sound || p_ptr->ffall))
			{
				int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}
			if (p_ptr->ffall)
			{
				dam = (dam * 2) / 3;
			}

			if (!p_ptr->ffall || (randint1(13) == 1))
			{
				inven_damage(set_cold_destroy, 2);
			}

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Standard damage */
		case GF_DISINTEGRATE:
		{
#ifdef JP
if (fuzzy) msg_print("純粋なエネルギーで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by pure energy!");
#endif
			get_damage = take_hit(dam, killer);
			break;
		}

		case GF_OLD_HEAL:
		{
#ifdef JP
if (fuzzy) msg_print("何か生命力を奪うもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something invigorating!");
#endif

			(void)hp_player(dam);
			break;
		}

		case GF_OLD_SPEED:
		{
#ifdef JP
if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

			(void)set_fast(p_ptr->fast + randint1(5));
			break;
		}

		case GF_OLD_SLOW:
		{
#ifdef JP
if (fuzzy) msg_print("何か遅いもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something slow!");
#endif

			(void)set_slow(p_ptr->slow + randint0(4) + 4);
			break;
		}

		case GF_OLD_SLEEP:
		{
			if (p_ptr->free_act) break;
#ifdef JP
if (fuzzy) msg_print("眠ってしまった！");
#else
			if (fuzzy) msg_print("You fall asleep!");
#endif


			if (ironman_nightmare)
			{
#ifdef JP
msg_print("恐ろしい光景が頭に浮かんできた。");
#else
				msg_print("A horrible vision enters your mind.");
#endif


				/* Pick a nightmare */
				get_mon_num_prep(get_nightmare, NULL);

				/* Have some nightmares */
				have_nightmare(get_mon_num(MAX_DEPTH));

				/* Remove the monster restriction */
				get_mon_num_prep(NULL, NULL);
			}

			set_paralyzed(p_ptr->paralyzed + dam);
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
#ifdef JP
if (fuzzy) msg_print("魔法のオーラで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by an aura of magic!");
#endif

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Pure damage */
		case GF_PSY_SPEAR:
		{
#ifdef JP
if (fuzzy) msg_print("エネルギーの塊で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by an energy!");
#endif

			get_damage = take_hit(dam, killer);
			break;
		}

		/* Pure damage */
		case GF_METEOR:
		{
#ifdef JP
if (fuzzy) msg_print("何かが空からあなたの頭上に落ちてきた！");
#else
			if (fuzzy) msg_print("Something falls from the sky on you!");
#endif

			get_damage = take_hit(dam, killer);
			if (!p_ptr->resist_shard || (randint1(13) == 1))
			{
				if (!p_ptr->immune_fire) inven_damage(set_fire_destroy, 2);
				inven_damage(set_cold_destroy, 2);
			}

			break;
		}

		/* Ice -- cold plus stun plus cuts */
		case GF_ICE:
		{
#ifdef JP
if (fuzzy) msg_print("何か鋭く冷たいもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something sharp and cold!");
#endif

			get_damage = cold_dam(dam, killer);
			if (!p_ptr->resist_shard)
			{
				(void)set_cut(p_ptr->cut + damroll(5, 8));
			}
			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint1(15));
			}

			if ((!(p_ptr->resist_cold || p_ptr->oppose_cold)) || (randint1(12) == 1))
			{
				if (!p_ptr->immune_cold) inven_damage(set_cold_destroy, 3);
			}

			break;
		}

		/* Death Ray */
		case GF_DEATH_RAY:
		{
#ifdef JP
if (fuzzy) msg_print("何か非常に冷たいもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something extremely cold!");
#endif


			switch (p_ptr->prace)
			{
#if 0
				/* Some races are immune */
				case RACE_VAMPIRE:
					break;
#endif
				/* Hurt a lot */
				default:
				{
					get_damage = take_hit(dam, killer);
					break;
				}
			}

			break;
		}
	}


	/* Disturb */
	disturb(1, 0);


	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Find the distance from (x, y) to a line.
 */
int dist_to_line(int y, int x, int y1, int x1, int y2, int x2)
{
	/* Vector from (x, y) to (x1, y1) */
	int py = y1 - y;
	int px = x1 - x;

	/* Normal vector */
	int ny = x2 - x1;
	int nx = y1 - y2;

	/* Length of N */
	int d = distance(y1, x1, y2, x2);

	/* Component of P on N */
	d = ((d) ? ((py * ny + px * nx) / d) : 0);

	/* Absolute value */
	return ((d >= 0) ? d : 0 - d);
}


/*
 * Does the grid stop disintegration?
 */
#define cave_stop_disintegration(Y,X) \
	(((cave[Y][X].feat >= FEAT_PERM_EXTRA) && \
	  (cave[Y][X].feat <= FEAT_PERM_SOLID)) || \
	  (cave[Y][X].feat == FEAT_MOUNTAIN) || \
	 ((cave[Y][X].feat >= FEAT_SHOP_HEAD) && \
	  (cave[Y][X].feat <= FEAT_SHOP_TAIL)) || \
	 ((cave[Y][X].feat >= FEAT_BLDG_HEAD) && \
	  (cave[Y][X].feat <= FEAT_BLDG_TAIL)))


/*
 * XXX XXX XXX
 * Modified version of los() for calculation of disintegration balls.
 * Disintegration effects are stopped by permanent walls.
 */
static bool in_disintegration_range(int y1, int x1, int y2, int x2)
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
				if (cave_stop_disintegration(ty, x1)) return (FALSE);
			}
		}

		/* North -- check for walls */
		else
		{
			for (ty = y1 - 1; ty > y2; ty--)
			{
				if (cave_stop_disintegration(ty, x1)) return (FALSE);
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
				if (cave_stop_disintegration(y1, tx)) return (FALSE);
			}
		}

		/* West -- check for walls */
		else
		{
			for (tx = x1 - 1; tx > x2; tx--)
			{
				if (cave_stop_disintegration(y1, tx)) return (FALSE);
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
			if (!cave_stop_disintegration(y1 + sy, x1)) return (TRUE);
		}
	}

	/* Horizontal "knights" */
	else if (ay == 1)
	{
		if (ax == 2)
		{
			if (!cave_stop_disintegration(y1, x1 + sx)) return (TRUE);
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
			if (cave_stop_disintegration(ty, tx)) return (FALSE);

			qy += m;

			if (qy < f2)
			{
				tx += sx;
			}
			else if (qy > f2)
			{
				ty += sy;
				if (cave_stop_disintegration(ty, tx)) return (FALSE);
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
			if (cave_stop_disintegration(ty, tx)) return (FALSE);

			qx += m;

			if (qx < f2)
			{
				ty += sy;
			}
			else if (qx > f2)
			{
				tx += sx;
				if (cave_stop_disintegration(ty, tx)) return (FALSE);
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


static bool do_disintegration(int y2, int x2, int y, int x)
{
	/* Disintegration balls explosions are stopped by perma-walls */
	if (!in_disintegration_range(y2, x2, y, x)) return FALSE;

	if (cave_valid_bold(y, x) &&
		(cave[y][x].feat < FEAT_PATTERN_START ||
		cave[y][x].feat > FEAT_PATTERN_XTRA2) &&
		(cave[y][x].feat < FEAT_DEEP_WATER ||
		cave[y][x].feat > FEAT_GRASS))
	{
		if (cave[y][x].feat == FEAT_TREES)
			cave_set_feat(y, x, FEAT_GRASS);
		else
			cave_set_feat(y, x, FEAT_FLOOR);
	}

	/* Update some things -- similar to GF_KILL_WALL */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

	return TRUE;
}


/*
 * breath shape
 */ 
static void breath_shape(coord *path_g, int dist, int *pgrids, byte *gx, byte *gy, byte *gm, int *pgm_rad, int rad, int y1, int x1, int y2, int x2, bool disint_ball)
{
	int by = y1;
	int bx = x1;
	int brad = 0;
	int brev = rad * rad / dist;
	int bdis = 0;
	int cdis;
	int path_n = 0;
	int tdis = distance(y2, x2, y1, x1);
	int mdis = tdis + rad;

	while (bdis <= mdis)
	{
		int x, y;

		if ((0 < dist) && (path_n < dist))
		{
			int ny = path_g[path_n].y;
			int nx = path_g[path_n].x;
			int nd = distance(ny, nx, y1, x1);

			/* Get next base point */
			if (bdis >= nd)
			{
				by = ny;
				bx = nx;
				path_n++;
			}
		}

		/* Travel from center outward */
		for (cdis = 0; cdis <= brad; cdis++)
		{
			/* Scan the maximal blast area of radius "cdis" */
			for (y = by - cdis; y <= by + cdis; y++)
			{
				for (x = bx - cdis; x <= bx + cdis; x++)
				{
					/* Ignore "illegal" locations */
					if (!in_bounds(y, x)) continue;

					/* Enforce an arc */
					if (distance(y1, x1, y, x) != bdis) continue;

					/* Enforce a circular "ripple" */
					if (distance(by, bx, y, x) != cdis) continue;

					if (disint_ball)
					{
						if (!do_disintegration(y2, x2, y, x)) continue;
					}
					else
					{
						/* Ball explosions are stopped by walls */
						if (!los(by, bx, y, x)) continue;
					}

					/* Save this grid */
					gy[*pgrids] = y;
					gx[*pgrids] = x;
					(*pgrids)++;
				}
			}
		}

		/* Encode some more "radius" info */
		gm[bdis + 1] = *pgrids;

		/* Increase the size */
		brad = rad * (path_n + brev) / (dist + brev);

		/* Find the next ripple */
		bdis++;
	}

	/* Store the effect size */
	*pgm_rad = bdis;
}

/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *
 * Input:
 *   who: Index of "source" monster (zero for "player")
 *   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball)
 *   y,x: Target location (or location to travel "towards")
 *   dam: Base damage roll to apply to affected monsters (or player)
 *   typ: Type of damage to apply to monsters (and objects)
 *   flg: Extra bit flags (see PROJECT_xxxx in "defines.h")
 *
 * Return:
 *   TRUE if any "effects" of the projection were observed, else FALSE
 *
 * Allows a monster (or player) to project a beam/bolt/ball of a given kind
 * towards a given location (optionally passing over the heads of interposing
 * monsters), and have it do a given amount of damage to the monsters (and
 * optionally objects) within the given radius of the final location.
 *
 * A "bolt" travels from source to target and affects only the target grid.
 * A "beam" travels from source to target, affecting all grids passed through.
 * A "ball" travels from source to the target, exploding at the target, and
 *   affecting everything within the given radius of the target location.
 *
 * Traditionally, a "bolt" does not affect anything on the ground, and does
 * not pass over the heads of interposing monsters, much like a traditional
 * missile, and will "stop" abruptly at the "target" even if no monster is
 * positioned there, while a "ball", on the other hand, passes over the heads
 * of monsters between the source and target, and affects everything except
 * the source monster which lies within the final radius, while a "beam"
 * affects every monster between the source and target, except for the casting
 * monster (or player), and rarely affects things on the ground.
 *
 * Two special flags allow us to use this function in special ways, the
 * "PROJECT_HIDE" flag allows us to perform "invisible" projections, while
 * the "PROJECT_JUMP" flag allows us to affect a specific grid, without
 * actually projecting from the source monster (or player).
 *
 * The player will only get "experience" for monsters killed by himself
 * Unique monsters can only be destroyed by attacks from the player
 *
 * Only 1024 grids can be affected per projection, limiting the effective
 * "radius" of standard ball attacks to nine units (diameter nineteen).
 *
 * One can project in a given "direction" by combining PROJECT_THRU with small
 * offsets to the initial location (see "line_spell()"), or by calculating
 * "virtual targets" far away from the player.
 *
 * One can also use PROJECT_THRU to send a beam/bolt along an angled path,
 * continuing until it actually hits somethings (useful for "stone to mud").
 *
 * Bolts and Beams explode INSIDE walls, so that they can destroy doors.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters
 * on both sides of a wall.  Some bug reports indicate that this is still
 * happening in 2.7.8 for Windows, though it appears to be impossible.
 *
 * We "pre-calculate" the blast area only in part for efficiency.
 * More importantly, this lets us do "explosions" from the "inside" out.
 * This results in a more logical distribution of "blast" treasure.
 * It also produces a better (in my opinion) animation of the explosion.
 * It could be (but is not) used to have the treasure dropped by monsters
 * in the middle of the explosion fall "outwards", and then be damaged by
 * the blast as it spreads outwards towards the treasure drop location.
 *
 * Walls and doors are included in the blast area, so that they can be
 * "burned" or "melted" in later versions.
 *
 * This algorithm is intended to maximize simplicity, not necessarily
 * efficiency, since this function is not a bottleneck in the code.
 *
 * We apply the blast effect from ground zero outwards, in several passes,
 * first affecting features, then objects, then monsters, then the player.
 * This allows walls to be removed before checking the object or monster
 * in the wall, and protects objects which are dropped by monsters killed
 * in the blast, and allows the player to see all affects before he is
 * killed or teleported away.  The semantics of this method are open to
 * various interpretations, but they seem to work well in practice.
 *
 * We process the blast area from ground-zero outwards to allow for better
 * distribution of treasure dropped by monsters, and because it provides a
 * pleasing visual effect at low cost.
 *
 * Note that the damage done by "ball" explosions decreases with distance.
 * This decrease is rapid, grids at radius "dist" take "1/dist" damage.
 *
 * Notice the "napalm" effect of "beam" weapons.  First they "project" to
 * the target, and then the damage "flows" along this beam of destruction.
 * The damage at every grid is the same as at the "center" of a "ball"
 * explosion, since the "beam" grids are treated as if they ARE at the
 * center of a "ball" explosion.
 *
 * Currently, specifying "beam" plus "ball" means that locations which are
 * covered by the initial "beam", and also covered by the final "ball", except
 * for the final grid (the epicenter of the ball), will be "hit twice", once
 * by the initial beam, and once by the exploding ball.  For the grid right
 * next to the epicenter, this results in 150% damage being done.  The center
 * does not have this problem, for the same reason the final grid in a "beam"
 * plus "bolt" does not -- it is explicitly removed.  Simply removing "beam"
 * grids which are covered by the "ball" will NOT work, as then they will
 * receive LESS damage than they should.  Do not combine "beam" with "ball".
 *
 * The array "gy[],gx[]" with current size "grids" is used to hold the
 * collected locations of all grids in the "blast area" plus "beam path".
 *
 * Note the rather complex usage of the "gm[]" array.  First, gm[0] is always
 * zero.  Second, for N>1, gm[N] is always the index (in gy[],gx[]) of the
 * first blast grid (see above) with radius "N" from the blast center.  Note
 * that only the first gm[1] grids in the blast area thus take full damage.
 * Also, note that gm[rad+1] is always equal to "grids", which is the total
 * number of blast grids.
 *
 * Note that once the projection is complete, (y2,x2) holds the final location
 * of bolts/beams, and the "epicenter" of balls.
 *
 * Note also that "rad" specifies the "inclusive" radius of projection blast,
 * so that a "rad" of "one" actually covers 5 or 9 grids, depending on the
 * implementation of the "distance" function.  Also, a bolt can be properly
 * viewed as a "ball" with a "rad" of "zero".
 *
 * Note that if no "target" is reached before the beam/bolt/ball travels the
 * maximum distance allowed (MAX_RANGE), no "blast" will be induced.  This
 * may be relevant even for bolts, since they have a "1x1" mini-blast.
 *
 * Note that for consistency, we "pretend" that the bolt actually takes "time"
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 *
 * Hack -- when only a single monster is affected, we automatically track
 * (and recall) that monster, unless "PROJECT_JUMP" is used.
 *
 * Note that all projections now "explode" at their final destination, even
 * if they were being projected at a more distant destination.  This means
 * that "ball" spells will *always* explode.
 *
 * Note that we must call "handle_stuff()" after affecting terrain features
 * in the blast radius, in case the "illumination" of the grid was changed,
 * and "update_view()" and "update_monsters()" need to be called.
 */
bool project(int who, int rad, int y, int x, int dam, int typ, u16b flg)
{
	int i, t, dist;

	int y1, x1;
	int y2, x2;

	int dist_hack = 0;

	int y_saver, x_saver; /* For reflecting monsters */

	int msec = delay_factor * delay_factor * delay_factor;

	/* Assume the player sees nothing */
	bool notice = FALSE;

	/* Assume the player has seen nothing */
	bool visual = FALSE;

	/* Assume the player has seen no blast grids */
	bool drawn = FALSE;

	/* Assume to be a normal ball spell */
	bool breath = FALSE;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
	coord path_g[512];

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	byte gx[1024], gy[1024];

	/* Encoded "radius" info (see above) */
	byte gm[32];

	/* Actual radius encoded in gm[] */
	int gm_rad;

	bool jump = FALSE;

	/* Hack -- some weapons always stop at monsters */
	if (typ == GF_ROCKET) flg |= PROJECT_STOP;

	/* Hack -- Jump to target */
	if (flg & (PROJECT_JUMP))
	{
		x1 = x;
		y1 = y;

		/* Clear the flag */
		flg &= ~(PROJECT_JUMP);

		jump = TRUE;
	}

	/* Start at player */
	else if (who <= 0)
	{
		x1 = px;
		y1 = py;
	}

	/* Start at monster */
	else if (who > 0)
	{
		x1 = m_list[who].fx;
		y1 = m_list[who].fy;
	}

	/* Oops */
	else
	{
		x1 = x;
		y1 = y;
	}

	y_saver = y1;
	x_saver = x1;

	/* Default "destination" */
	y2 = y;
	x2 = x;


	/* Hack -- verify stuff */
	if (flg & (PROJECT_THRU))
	{
		if ((x1 == x2) && (y1 == y2))
		{
			flg &= ~(PROJECT_THRU);
		}
	}

	/* Handle a breath attack */
	if (rad < 0)
	{
		rad = 0 - rad;

		if ((x1 != x2) || (y1 != y2))
		{
			breath = TRUE;
			flg |= PROJECT_HIDE;
		}
	}

	gm_rad = rad;

	/* Hack -- Assume there will be no blast (max radius 32) */
	for (dist = 0; dist < 32; dist++) gm[dist] = 0;


	/* Initial grid */
	y = y1;
	x = x1;
	dist = 0;

	/* Collect beam grids */
	if (flg & (PROJECT_BEAM))
	{
		gy[grids] = y;
		gx[grids] = x;
		grids++;
	}


	/* Calculate the projection path */
	if ((typ == GF_JUMP) || (typ == GF_JUMP_ATTACK))
	{
		int ny = py, nx = px;

		if ((y2 == py) && (x2 == px))
		{
			msg_print("あなたはその場で飛び跳ねた。");
			teleport_player_to(ny, nx);
		}
		else
		{
			/* Teleport player */
			path_n = project_path(path_g, project_length, y1, x1, y2, x2, flg);
			if (cave_empty_bold(path_g[path_n - 1].y, path_g[path_n - 1].x))
			{
				ny = path_g[path_n - 1].y;
				nx = path_g[path_n - 1].x;
			}
			else if (path_n > 1)
			{
				ny = path_g[path_n - 2].y;
				nx = path_g[path_n - 2].x;
			}
			teleport_player_to(ny, nx);
		}
	}
	else
		path_n = project_path(path_g, ((project_length) ? project_length : MAX_RANGE), y1, x1, y2, x2, flg);


	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int oy = y;
		int ox = x;

		int ny = path_g[i].y;
		int nx = path_g[i].x;

		/* Hack -- Balls explode before reaching walls */
		if (!cave_floor_bold(ny, nx) && (rad > 0)) break;

		/* Advance */
		y = ny;
		x = nx;

		/* Collect beam grids */
		if (flg & (PROJECT_BEAM))
		{
			gy[grids] = y;
			gx[grids] = x;
			grids++;
		}

		/* Only do visuals if requested */
		if (!blind && !(flg & (PROJECT_HIDE)))
		{
			/* Only do visuals if the player can "see" the bolt */
			if (panel_contains(y, x) && player_has_los_bold(y, x))
			{
				u16b p;

				byte a;
				char c;

				/* Obtain the bolt pict */
				p = bolt_pict(oy, ox, y, x, typ);

				/* Extract attr/char */
				a = PICT_A(p);
				c = PICT_C(p);

				/* Visual effects */
				print_rel(c, a, y, x);
				move_cursor_relative(y, x);
				/*if (fresh_before)*/ Term_fresh();
				Term_xtra(TERM_XTRA_DELAY, msec);
				lite_spot(y, x);
				/*if (fresh_before)*/ Term_fresh();

				/* Display "beam" grids */
				if (flg & (PROJECT_BEAM))
				{
					/* Obtain the explosion pict */
					p = bolt_pict(y, x, y, x, typ);

					/* Extract attr/char */
					a = PICT_A(p);
					c = PICT_C(p);

					/* Visual effects */
					print_rel(c, a, y, x);
				}

				/* Hack -- Activate delay */
				visual = TRUE;
			}

			/* Hack -- delay anyway for consistency */
			else if (visual)
			{
				/* Delay for consistency */
				Term_xtra(TERM_XTRA_DELAY, msec);
			}
		}
	}

	path_n = i;

	if (breath && !path_n)
	{
		breath = FALSE;
		gm_rad = rad;
		flg &= ~(PROJECT_HIDE);
	}

	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Start the "explosion" */
	gm[0] = 0;

	/* Hack -- make sure beams get to "explode" */
	gm[1] = grids;

	dist = path_n;
	dist_hack = dist;

	/* If we found a "target", explode there */
	if (dist <= MAX_RANGE)
	{
		/* Mega-Hack -- remove the final "beam" grid */
		if ((flg & (PROJECT_BEAM)) && (grids > 0)) grids--;

		/*
		 * Create a conical breath attack
		 *
		 *         ***
		 *     ********
		 * D********@**
		 *     ********
		 *         ***
		 */
		if (breath)
		{
			flg &= ~(PROJECT_HIDE);

			breath_shape(path_g, dist, &grids, gx, gy, gm, &gm_rad, rad, y1, x1, y2, x2, (bool)(typ == GF_DISINTEGRATE));
		}

		else
		{
			/* Determine the blast area, work from the inside out */
			for (dist = 0; dist <= rad; dist++)
			{
				/* Scan the maximal blast area of radius "dist" */
				for (y = y2 - dist; y <= y2 + dist; y++)
				{
					for (x = x2 - dist; x <= x2 + dist; x++)
					{
						/* Ignore "illegal" locations */
						if (!in_bounds2(y, x)) continue;

						/* Enforce a "circular" explosion */
						if (distance(y2, x2, y, x) != dist) continue;

						if (typ == GF_DISINTEGRATE)
						{
							/* Disintegration balls explosions are stopped by perma-walls */
							if (!in_disintegration_range(y2, x2, y, x)) continue;

							if (cave_valid_bold(y, x) &&
								(cave[y][x].feat < FEAT_PATTERN_START ||
								 cave[y][x].feat > FEAT_PATTERN_XTRA2) &&
								(cave[y][x].feat < FEAT_DEEP_WATER ||
								 cave[y][x].feat > FEAT_GRASS))
							{
								if (cave[y][x].feat == FEAT_TREES)
									cave_set_feat(y, x, FEAT_GRASS);
								else
									cave_set_feat(y, x, FEAT_FLOOR);
							}

							/* Update some things -- similar to GF_KILL_WALL */
							p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);
						}
						else
						{
							/* Ball explosions are stopped by walls */
							if (!los(y2, x2, y, x)) continue;
						}

						/* Save this grid */
						gy[grids] = y;
						gx[grids] = x;
						grids++;
					}
				}

				/* Encode some more "radius" info */
				gm[dist+1] = grids;
			}
		}
	}


	/* Speed -- ignore "non-explosions" */
	if (!grids) return (FALSE);


	/* Display the "blast area" if requested */
	if (!blind && !(flg & (PROJECT_HIDE)))
	{
		/* Then do the "blast", from inside out */
		for (t = 0; t <= gm_rad; t++)
		{
			/* Dump everything with this radius */
			for (i = gm[t]; i < gm[t+1]; i++)
			{
				/* Extract the location */
				y = gy[i];
				x = gx[i];

				/* Only do visuals if the player can "see" the blast */
				if (panel_contains(y, x) && player_has_los_bold(y, x))
				{
					u16b p;

					byte a;
					char c;

					drawn = TRUE;

					/* Obtain the explosion pict */
					p = bolt_pict(y, x, y, x, typ);

					/* Extract attr/char */
					a = PICT_A(p);
					c = PICT_C(p);

					/* Visual effects -- Display */
					print_rel(c, a, y, x);
				}
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush each "radius" seperately */
			/*if (fresh_before)*/ Term_fresh();

			/* Delay (efficiently) */
			if (visual || drawn)
			{
				Term_xtra(TERM_XTRA_DELAY, msec);
			}
		}

		/* Flush the erasing */
		if (drawn)
		{
			/* Erase the explosion drawn above */
			for (i = 0; i < grids; i++)
			{
				/* Extract the location */
				y = gy[i];
				x = gx[i];

				/* Hack -- Erase if needed */
				if (panel_contains(y, x) && player_has_los_bold(y, x))
				{
					lite_spot(y, x);
				}
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush the explosion */
			/*if (fresh_before)*/ Term_fresh();
		}
	}


	/* Check features */
	if (flg & (PROJECT_GRID))
	{
		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for features */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist+1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Find the closest point in the blast */
			if (breath)
			{
				int d = dist_to_line(y, x, y1, x1, y2, x2);

				/* Affect the grid */
				if (project_f(who, d, y, x, dam, typ)) notice = TRUE;
			}
			else
			{
				/* Affect the grid */
				if (project_f(who, dist, y, x, dam, typ)) notice = TRUE;
			}
		}
	}


	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();


	/* Check objects */
	if (flg & (PROJECT_ITEM))
	{
		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for objects */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist+1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Find the closest point in the blast */
			if (breath)
			{
				int d = dist_to_line(y, x, y1, x1, y2, x2);

				/* Affect the object in the grid */
				if (project_o(who, d, y, x, dam, typ)) notice = TRUE;
			}
			else
			{
				/* Affect the object in the grid */
				if (project_o(who, dist, y, x, dam, typ)) notice = TRUE;
			}
		}
	}


	/* Check monsters */
	if (flg & (PROJECT_KILL))
	{
		/* Mega-Hack */
		project_m_n = 0;
		project_m_x = 0;
		project_m_y = 0;

		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for monsters */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist + 1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			if (grids > 1)
			{
				/* Find the closest point in the blast */
				if (breath)
				{
					int d = dist_to_line(y, x, y1, x1, y2, x2);

					/* Affect the monster in the grid */
					if (project_m(who, d, y, x, dam, typ, flg)) notice = TRUE;
				}
				else
				{
					/* Affect the monster in the grid */
					if (project_m(who, dist, y, x, dam, typ, flg)) notice = TRUE;
				}
			}
			else
			{
				monster_race *ref_ptr = &r_info[m_list[cave[y][x].m_idx].r_idx];

				if ((ref_ptr->flags2 & RF2_REFLECTING) && (flg & PROJECT_REFLECTABLE) &&
				    (randint1(10) != 1) && (!who || dist_hack > 1))
				{
					int t_y, t_x;
					int max_attempts = 10;

					/* Choose 'new' target */
					do
					{
						t_y = y_saver - 1 + randint1(3);
						t_x = x_saver - 1 + randint1(3);
						max_attempts--;
					}

					while (max_attempts && in_bounds2u(t_y, t_x) &&
					    !(los(y, x, t_y, t_x)));

					if (max_attempts < 1)
					{
						t_y = y_saver;
						t_x = x_saver;
					}

					if (m_list[cave[y][x].m_idx].ml)
					{
#ifdef JP
						msg_print("攻撃は跳ね返った！");
#else
						msg_print("The attack bounces!");
#endif
						ref_ptr->r_flags2 |= RF2_REFLECTING;
					}

					project(cave[y][x].m_idx, 0, t_y, t_x,  dam, typ, flg);
				}
				else
				{
					if (project_m(who, dist, y, x, dam, typ, flg)) notice = TRUE;
				}
			}
		}

		/* Player affected one monster (without "jumping") */
		if (!who && (project_m_n == 1) && !jump)
		{
			/* Location */
			x = project_m_x;
			y = project_m_y;

			/* Track if possible */
			if (cave[y][x].m_idx > 0)
			{
				monster_type *m_ptr = &m_list[cave[y][x].m_idx];

				/* Hack -- auto-recall */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(cave[y][x].m_idx);
			}
		}
	}


	/* Check player */
	if (flg & (PROJECT_KILL))
	{
		/* Start with "dist" of zero */
		dist = 0;

		/* Scan for player */
		for (i = 0; i < grids; i++)
		{
			/* Hack -- Notice new "dist" values */
			if (gm[dist+1] == i) dist++;

			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Find the closest point in the blast */
			if (breath)
			{
				int d = dist_to_line(y, x, y1, x1, y2, x2);

				/* Affect the player */
				if (project_p(who, d, y, x, dam, typ, rad)) notice = TRUE;
			}
			else
			{
				/* Affect the player */
				if (project_p(who, dist, y, x, dam, typ, rad)) notice = TRUE;
			}
		}
	}

	/* Return "something was noticed" */
	return (notice);
}
