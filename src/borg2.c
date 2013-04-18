/* File: borg2.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#include "borg.h"


#ifdef ALLOW_BORG


/*
 * This file supplies some low level support code.
 *
 * This file includes the lowest level dungeon analysis code.
 *
 * Currently, this includes general routines involving dungeon grids,
 * including calculating "flow" values from place to place, determining
 * "line of sight", plus "field of view" and "torch-lit grids", setting
 * the target to a given location, and extracting the optimal direction
 * for "movement" from place to place.
 *
 * Note that the dungeon is assumed to be smaller than 256 by 256.
 */



/*
 * Make a note for later
 */
bool borg_make_note(cptr what)
{
	/* Send action */
	borg_keypress(':');

	/* Send note string */
	borg_keypresses(what);

	/* Send end of string */
	borg_keypress('\n');

	/* Success */
	return (TRUE);
}



/*
 * Attempt to change the borg's name
 */
bool borg_change_name(cptr str)
{
	/* Send leave store XXX XXX XXX */
	borg_keypress(ESCAPE);

	/* Send action */
	borg_keypress('C');

	/* Send change name key */
	borg_keypress('c');

	/* Send name string */
	borg_keypresses(str);

	/* Send end of string */
	borg_keypress('\n');

	/* Send cancel XXX XXX XXX */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Success */
	return (TRUE);
}


/*
 * Attempt to dump a character description file
 */
bool borg_dump_character(cptr str)
{
	/* Send leave store XXX XXX XXX */
	borg_keypress(ESCAPE);

	/* Send action */
	borg_keypress('C');

	/* Send dump character key */
	borg_keypress('f');

	/* Send filename string */
	borg_keypresses(str);

	/* Send end of string */
	borg_keypress('\n');

	/* Send cancel XXX XXX XXX */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Success */
	return (TRUE);
}




/*
 * Attempt to save the game
 */
bool borg_save_game(void)
{
	/* Send leave store XXX XXX XXX */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Send action */
	borg_keypress('^');
	borg_keypress('S');

	/* Send cancel XXX XXX XXX */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Success */
	return (TRUE);
}




/*
 * Update the Borg based on the current "frame"
 *
 * Assumes the Borg is actually in the dungeon.
 */
void borg_update_frame(void)
{
	int i;

	byte t_a;

	char buf[160];


	/* Check for "   Town" or "  Lev 8" or " Lev 13" */
	if (0 == borg_what_text(COL_DEPTH, ROW_DEPTH, -7, &t_a, buf))
	{
		cptr s;

		/* Skip the non-digits */
		for (s = buf; *s && !isdigit(*s); s++) /* loop */;

		/* Extract the current level */
		b_ptr->depth = borg_base_depth = atoi(s);
	}


	/* Title (ignore) XXX XXX XXX */


	/* Info (monster health) XXX XXX XXX */


	/* Assume level is fine */
	borg_base_fix_lev = FALSE;

	/* Check for drained level */
	if (0 == borg_what_text(COL_LEVEL, ROW_LEVEL, -3, &t_a, buf))
	{
		/* Note "Lev" vs "LEV" */
		if (islower(buf[2])) borg_base_fix_lev = TRUE;
	}

	/* Extract current level */
	if (0 == borg_what_text(COL_LEVEL + 6, ROW_LEVEL, -6, &t_a, buf))
	{
		/* Extract "LEVEL xxxxxx" */
		b_ptr->lev = borg_base_level = atoi(buf);
	}


	/* Assume experience is fine */
	borg_base_fix_exp = FALSE;

	/* Check for drained experience */
	if (0 == borg_what_text(COL_EXP, ROW_EXP, -3, &t_a, buf))
	{
		/* Note "Exp" vs "EXP" */
		if (islower(buf[2])) borg_base_fix_exp = TRUE;
	}

	/* Extract current experience */
	if (0 == borg_what_text(COL_EXP + 4, ROW_EXP, -8, &t_a, buf))
	{
		/* Extract "EXP xxxxxxxx" */
		b_ptr->exp = borg_base_exp = atol(buf);
	}


	/* Extract current gold */
	if (0 == borg_what_text(COL_GOLD + 3, ROW_GOLD, -9, &t_a, buf))
	{
		/* Extract "AU xxxxxxxxx" */
		b_ptr->au = borg_base_au = atol(buf);
	}


	/* Extract speed */
	if (0 == borg_what_text(COL_SPEED, ROW_SPEED, -14, &t_a, buf))
	{
		/* Extract "Fast (+x)" or "Slow (-x)" */
		b_ptr->pspeed = borg_base_pspeed = 110 + atoi(buf + 6);
	}

	/* Extract armor class */
	if (0 == borg_what_text(COL_AC + 7, ROW_AC, -5, &t_a, buf))
	{
		/* Extract "Cur AC xxxxx" */
		b_ptr->ac = borg_base_ac = atoi(buf);
	}


	/* Extract maximum hitpoints */
	if (0 == borg_what_text(COL_MAXHP + 7, ROW_MAXHP, -5, &t_a, buf))
	{
		/* Extract "Max HP xxxxx" */
		b_ptr->mhp = borg_base_mhp = atoi(buf);
	}

	/* Extract current hitpoints */
	if (0 == borg_what_text(COL_CURHP + 7, ROW_CURHP, -5, &t_a, buf))
	{
		/* Extract "Cur HP xxxxx" */
		b_ptr->chp = borg_base_chp = atoi(buf);
	}


	/* Extract maximum spell points */
	if (0 == borg_what_text(COL_MAXSP + 7, ROW_MAXSP, -5, &t_a, buf))
	{
		/* Extract "Max SP xxxxx" (or zero) */
		b_ptr->msp = borg_base_msp = atoi(buf);
	}

	/* Extract current spell points */
	if (0 == borg_what_text(COL_CURSP + 7, ROW_CURSP, -5, &t_a, buf))
	{
		/* Extract "Cur SP xxxxx" (or zero) */
		b_ptr->csp = borg_base_csp = atoi(buf);
	}



	/* Clear all the "state flags" */
	borg_base_is_weak = borg_base_is_hungry = borg_base_is_full = borg_base_is_gorged = FALSE;
	borg_base_is_blind = borg_base_is_confused = borg_base_is_afraid = borg_base_is_poisoned = FALSE;
	borg_base_is_cut = borg_base_is_stun = borg_base_is_image = borg_base_is_study = FALSE;

	/* Check for hunger */
	if (0 == borg_what_text(COL_HUNGRY, ROW_HUNGRY, -1, &t_a, buf))
	{
		/* Check for "Hungry" */
		if (buf[0] == 'H') borg_base_is_hungry = TRUE;

		/* Check for "Weak" */
		if (buf[0] == 'W') borg_base_is_weak = borg_base_is_hungry = TRUE;

		/* Check for "Full" */
		if (buf[0] == 'F') borg_base_is_full = TRUE;

		/* Check for "Gorged" */
		if (buf[0] == 'G') borg_base_is_gorged = borg_base_is_full = TRUE;
	}

	/* Check for blind */
	if (0 == borg_what_text(COL_BLIND, ROW_BLIND, -1, &t_a, buf))
	{
		/* Check for "Blind" */
		if (buf[0] == 'B') borg_base_is_blind = TRUE;
	}

	/* Check for confused */
	if (0 == borg_what_text(COL_CONFUSED, ROW_CONFUSED, -1, &t_a, buf))
	{
		/* Check for "Confused" */
		if (buf[0] == 'C') borg_base_is_confused = TRUE;
	}

	/* Check for afraid */
	if (0 == borg_what_text(COL_AFRAID, ROW_AFRAID, -1, &t_a, buf))
	{
		/* Check for "Afraid" */
		if (buf[0] == 'A') borg_base_is_afraid = TRUE;
	}

	/* Check for poisoned */
	if (0 == borg_what_text(COL_POISONED, ROW_POISONED, -1, &t_a, buf))
	{
		/* Check for "Poisoned" */
		if (buf[0] == 'P') borg_base_is_poisoned = TRUE;
	}


	/* Check for cut XXX XXX */
	if (0 == borg_what_text(COL_CUT, ROW_CUT, -1, &t_a, buf))
	{
		/* Check for any text */
		if (isalpha(buf[0])) borg_base_is_cut = TRUE;
	}

	/* Check for stun XXX XXX */
	if (0 == borg_what_text(COL_STUN, ROW_STUN, -1, &t_a, buf))
	{
		/* Check for any text */
		if (isalpha(buf[0])) borg_base_is_stun = TRUE;
	}


	/* Parse "State" XXX XXX XXX */


	/* Check for study */
	if (0 == borg_what_text(COL_STUDY, ROW_STUDY, -1, &t_a, buf))
	{
		/* Check for "Study" */
		if (buf[0] == 'S') borg_base_is_study = TRUE;
	}


	/* Parse stats */
	for (i = 0; i < 6; i++)
	{
		/* Check "NNN   xxxxxx" */
		if (0 == borg_what_text(COL_STAT, ROW_STAT+i, -4, &t_a, buf))
		{
			/* Note "Nnn" vs "NNN" */
			borg_base_fix_stat[i] = (islower(buf[2]));

			/* Note maximized stats */
			borg_stat_max[i] = (buf[3] == '!');
		}

		/* Check "NNN   xxxxxx" */
		if (0 == borg_what_text(COL_STAT+6, ROW_STAT+i, -6, &t_a, buf))
		{
			/* Parse "18/NNN" */
			if (buf[2] == '/') borg_base_stat[i] = 18 + atoi(buf+3);

			/* Parse " 18/NN" */
			else if (buf[3] == '/') borg_base_stat[i] = 18 + atoi(buf+4);

			/* Parse "    NN" */
			else borg_base_stat[i] = atoi(buf+4);
		}
	}
}



/*
 * This function was copied from "cave.c".
 *
 * The following regular expression substitutions were performed:
 *
 *   cave_floor_bold ==> borg_cave_floor_bold
 *
 *
 * A simple, fast, integer-based line-of-sight algorithm.
 *
 * See "los()" in "cave.c" for complete documentation
 */
bool borg_los(int y1, int x1, int y2, int x2)
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
				if (!borg_cave_floor_bold(ty, x1)) return (FALSE);
			}
		}

		/* North -- check for walls */
		else
		{
			for (ty = y1 - 1; ty > y2; ty--)
			{
				if (!borg_cave_floor_bold(ty, x1)) return (FALSE);
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
				if (!borg_cave_floor_bold(y1, tx)) return (FALSE);
			}
		}

		/* West -- check for walls */
		else
		{
			for (tx = x1 - 1; tx > x2; tx--)
			{
				if (!borg_cave_floor_bold(y1, tx)) return (FALSE);
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
			if (borg_cave_floor_bold(y1 + sy, x1)) return (TRUE);
		}
	}

	/* Horizontal "knights" */
	else if (ay == 1)
	{
		if (ax == 2)
		{
			if (borg_cave_floor_bold(y1, x1 + sx)) return (TRUE);
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
			if (!borg_cave_floor_bold(ty, tx)) return (FALSE);

			qy += m;

			if (qy < f2)
			{
				tx += sx;
			}
			else if (qy > f2)
			{
				ty += sy;
				if (!borg_cave_floor_bold(ty, tx)) return (FALSE);
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
			if (!borg_cave_floor_bold(ty, tx)) return (FALSE);

			qx += m;

			if (qx < f2)
			{
				ty += sy;
			}
			else if (qx > f2)
			{
				tx += sx;
				if (!borg_cave_floor_bold(ty, tx)) return (FALSE);
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
 * This function was copied from "cave.c".
 *
 * The following regular expression substitutions were performed:
 *
 *   project_path ==> borg_project_path
 *
 *   cave_floor_bold ==> borg_cave_floor_bold
 *
 *   cave_m_idx ==> borg_cave_m_idx
 *
 *
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
sint borg_project_path(u16b *gp, int range, \
                       int y1, int x1, int y2, int x2, int flg)
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
			if ((n > 0) && !borg_cave_floor_bold(y, x)) break;

			/* Hack -- assume unknown grids are walls */
			/* if (n && (borg_cave_feat[y][x] == FEAT_NONE)) break; */

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && (borg_cave_m_idx[y][x] != 0)) break;
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
			if ((n > 0) && !borg_cave_floor_bold(y, x)) break;

			/* Hack -- assume unknown grids are walls */
			/* if (n && (borg_cave_feat[y][x] == FEAT_NONE)) break; */

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && (borg_cave_m_idx[y][x] != 0)) break;
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
			if ((n > 0) && !borg_cave_floor_bold(y, x)) break;

			/* Hack -- assume unknown grids are walls */
			/* if (n && (borg_cave_feat[y][x] == FEAT_NONE)) break; */

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && (borg_cave_m_idx[y][x] != 0)) break;
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
 * This function was copied from "cave.c".
 *
 * The following regular expression substitutions were performed:
 *
 *   project_path ==> borg_project_path
 *
 *   projectable ==> borg_projectable
 *
 *   cave_floor_bold ==> borg_cave_floor_bold
 *
 *
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming that no monster gets in the way,
 * using the "project_path()" function to check the projection path.
 *
 * Note that no grid is ever "projectable()" from itself.  XXX XXX
 *
 * This function is used to determine if the player can (easily) target
 * a given grid, and if a monster can target the player.
 */
bool borg_projectable(int y1, int x1, int y2, int x2)
{
	int y, x;

	int grid_n = 0;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = borg_project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, 0);

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in a wall grid */
	if (!borg_cave_floor_bold(y, x)) return (FALSE);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}


/*
 * Almost all of the following lines were taken from "cave.c".
 *
 * The following modifications were then performed.
 *
 * First, in each of a few places where we know that a non-torch-lit grid,
 * which may be a floor grid, is in line of sight, we changed the test for
 * perma-lit from "CAVE_GLOW" to "CAVE_GLOW | CAVE_HACK", and added code
 * to check if the grid has the "CAVE_HACK" flag, and if so, to set the
 * "CAVE_GLOW" flag, and clear the "CAVE_DARK" flag.  This allows us to
 * accept glowing floor grids at the same time that we actually mark them
 * as glowing.  See "borg_update()" for more information.  XXX XXX XXX
 *
 * Second, we commented out all calls to "note_spot()" and "lite_spot()",
 * and any "if" and/or "loop" code which was thus rendered pointless, by
 * surrounding them with "#if 0" ... "#endif" blocks.
 *
 * Then, we performed the following 'regexp' substitutions:
 *
 *   p_ptr->blind ==> borg_base_is_blind
 *
 *   p_ptr ==> b_ptr
 *
 *   vinfo_init ==> borg_vinfo_init
 *
 *   forget_view ==> borg_forget_view
 *   update_view ==> borg_update_view
 *
 *   \<cave_info\> ==> borg_cave_info
 *
 *   \<temp_g\> ==> borg_temp_g
 *
 *   \<view_n\> ==> borg_view_n
 *   \<view_g\> ==> borg_view_g
 *
 * Last adaptation performed on 1997-12-03
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
	s16b grid_0;
	s16b grid_1;
	s16b grid_2;
	s16b grid_3;
	s16b grid_4;
	s16b grid_5;
	s16b grid_6;
	s16b grid_7;

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
 * The array of "vinfo" objects, initialized by "borg_vinfo_init()"
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
 * Temporary data used by "borg_vinfo_init()"
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
static void borg_vinfo_init_aux(vinfo_hack *hack, int y, int x, long m)
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
errr borg_vinfo_init(void)
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
			borg_vinfo_init_aux(hack, y, x, m);

			/* Slope to top left corner */
			m = SCALE * (1000L * y - 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			borg_vinfo_init_aux(hack, y, x, m);

			/* Slope to bottom right corner */
			m = SCALE * (1000L * y + 500) / (1000L * x + 500);

			/* Handle "legal" slopes */
			borg_vinfo_init_aux(hack, y, x, m);

			/* Slope to bottom left corner */
			m = SCALE * (1000L * y + 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			borg_vinfo_init_aux(hack, y, x, m);
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
	if (((vinfo[1].bits_3 | vinfo[2].bits_3) != VINFO_BITS_3) ||
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
void borg_forget_view(void)
{
	int i, g;

	int fast_view_n = borg_view_n;
	u16b *fast_view_g = borg_view_g;

	byte *fast_cave_info = &borg_cave_info[0][0];


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

#if 0
		/* Redraw */
		lite_spot(y, x);
#endif

	}

	/* None left */
	fast_view_n = 0;


	/* Save 'borg_view_n' */
	borg_view_n = fast_view_n;
}


/*
 * Calculate the complete field of view using a new algorithm
 *
 * If "borg_view_g" and "borg_temp_g" were global pointers to arrays of grids,
 * as opposed to actual arrays of grids, then we could be more efficient by
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
 * We use the "borg_temp_g" array (and the "CAVE_TEMP" flag) to keep track of
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
 * into the "borg_view_g" array), and setting the "CAVE_SEEN" flag for the grid
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
void borg_update_view(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int pg = GRID(py,px);

	int i, g, o2;

	int radius;

	int fast_view_n = borg_view_n;
	u16b *fast_view_g = borg_view_g;

	int fast_temp_n = 0;
	u16b *fast_temp_g = borg_temp_g;

	byte *fast_cave_feat = &borg_cave_feat[0][0];
	byte *fast_cave_info = &borg_cave_info[0][0];

	byte feat;
	byte info;


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
	radius = b_ptr->cur_lite;

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
				g = pg + *((s16b*)(((byte*)(p))+o2));

				/* Get grid info */
				feat = fast_cave_feat[g];
				info = fast_cave_info[g];

				/* Handle wall */
				if (feat & (CAVE_WALL))
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
							if ((!(borg_cave_feat[yy][xx] & (CAVE_WALL)) &&
									(borg_cave_info[yy][xx] & (CAVE_GLOW))) ||
								 (!(borg_cave_feat[y][xx] & (CAVE_WALL)) &&
									(borg_cave_info[y][xx] & (CAVE_GLOW))) ||
								 (!(borg_cave_feat[yy][x] & (CAVE_WALL)) &&
									(borg_cave_info[yy][x] & (CAVE_GLOW))))
							{
								/* Mark as seen */
								info |= (CAVE_SEEN);
							}

#else /* UPDATE_VIEW_COMPLEX_WALL_ILLUMINATION */

							/* Check for "simple" illumination */
							if (borg_cave_info[yy][xx] & (CAVE_GLOW))
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
						else if (info & (CAVE_GLOW | CAVE_HACK))
						{
							/* Note glowing floors */
							if (info & (CAVE_HACK))
							{
                						/* Assume perma-lit */
                						info |= (CAVE_GLOW);

                						/* Assume not dark */
                						info &= ~(CAVE_DARK);
							}

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
	if (borg_base_is_blind)
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

#if 0
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
#endif

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

#if 0
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
#endif

	}


	/* Save 'borg_view_n' */
	borg_view_n = fast_view_n;
}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

