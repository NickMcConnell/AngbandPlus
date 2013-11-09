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
 * Does the grid stop disintegration?
 */
#define cave_stop_disintegration(Y,X) \
	(((cave[Y][X].feat >= FEAT_PERM_EXTRA) && \
	  (cave[Y][X].feat <= FEAT_PERM_SOLID)) || \
	  (cave[Y][X].feat == FEAT_MOUNTAIN) || \
	 ((cave[Y][X].feat >= FEAT_SHOP_HEAD) && \
	  (cave[Y][X].feat <= FEAT_SHOP_TAIL)) || \
	 ((cave[Y][X].feat >= FEAT_BLDG_HEAD) && \
	  (cave[Y][X].feat <= FEAT_BLDG_TAIL)) || \
	  (cave[Y][X].feat == FEAT_MUSEUM) || \
	  (cave[Y][X].feat == FEAT_DENEB_SHOP))

static int rakubadam_m;
static int rakubadam_p;

int project_length = 0;


/*
 * Return a color to use for the bolt/ball spells
 */
static byte spell_color(int type)
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


#define stoppable_bold(Y, X, FLG) \
	((cave[(Y)][(X)].m_idx && ((p_ptr->riding != cave[(Y)][(X)].m_idx) || !((FLG) & PROJECT_AVOIDABLE))) || \
	(((Y) == py) && ((X) == px) && !((FLG) & PROJECT_AVOIDABLE)) || \
	(p_ptr->use_decoy && ((Y) == p_ptr->decoy_y) && ((X) == p_ptr->decoy_x)))

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
sint project_path(u16b *gp, int range, int y1, int x1, int y2, int x2, u32b flg)
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
		/* Let m = ((dx/dy) * full) = (dx * dx * 2) */
		m = ax * ax * 2;

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
			gp[n++] = GRID(y, x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			if (flg & (PROJECT_DISI))
			{
				if ((n > 0) && cave_stop_disintegration(y, x)) break;
			}
			else if (!(flg & (PROJECT_PATH)))
			{
				/* Always stop at non-initial wall grids */
				if ((n > 0) && !cave_floor_bold(y, x)) break;
			}

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && stoppable_bold(y, x, flg)) break;
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
		/* Let m = ((dy/dx) * full) = (dy * dy * 2) */
		m = ay * ay * 2;

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
			gp[n++] = GRID(y, x);

			/* Hack -- Check maximum range */
			if ((n + (k >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			if (flg & (PROJECT_DISI))
			{
				if ((n > 0) && cave_stop_disintegration(y, x)) break;
			}
			else if (!(flg & (PROJECT_PATH)))
			{
				/* Always stop at non-initial wall grids */
				if ((n > 0) && !cave_floor_bold(y, x)) break;
			}

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && stoppable_bold(y, x, flg)) break;
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
			gp[n++] = GRID(y, x);

			/* Hack -- Check maximum range */
			if ((n + (n >> 1)) >= range) break;

			/* Sometimes stop at destination grid */
			if (!(flg & (PROJECT_THRU)))
			{
				if ((x == x2) && (y == y2)) break;
			}

			if (flg & (PROJECT_DISI))
			{
				if ((n > 0) && cave_stop_disintegration(y, x)) break;
			}
			else if (!(flg & (PROJECT_PATH)))
			{
				/* Always stop at non-initial wall grids */
				if ((n > 0) && !cave_floor_bold(y, x)) break;
			}

			/* Sometimes stop at non-initial monsters/players */
			if (flg & (PROJECT_STOP))
			{
				if ((n > 0) && stoppable_bold(y, x, flg)) break;
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
/* Mega-Hack -- monsters target */
static s16b monster_target_x;
static s16b monster_target_y;


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
static bool project_f(int who, int r, int y, int x, int dam, int typ, u32b flg, int mod_elem_mode)
{
	cave_type       *c_ptr = &cave[y][x];

	bool obvious = FALSE;
	bool known = player_has_los_bold(y, x);
	s16b typ_elem = get_elem_type(typ);


	/* XXX XXX XXX */
	who = who ? who : 0;

	/* Reduce damage by distance */
	if (!(flg & PROJECT_NO_REDUCE)) dam = (dam + r) / (r + 1);

	/* Affect elements (except by player) */
	if ((typ_elem >= MIN_ELEM) && (((who != 0) && (mod_elem_mode == MODIFY_ELEM_MODE_MAGIC)) || hack_elem_amount))
	{
		if (hack_elem_amount)
		{
			change_grid_elem(c_ptr, typ_elem, hack_elem_amount);
			change_grid_elem(c_ptr, get_opposite_elem(typ_elem), -(hack_elem_amount));
		}
		else
		{
			change_grid_elem(c_ptr, typ_elem, 1);
			change_grid_elem(c_ptr, get_opposite_elem(typ_elem), -1);
		}
	}

	if (c_ptr->feat == FEAT_TREES)
	{
		cptr message;
		switch (typ)
		{
		case GF_POIS:
		case GF_NUKE:
		case GF_DEATH_RAY:
#ifdef JP
			message = "枯れた";break;
#else
			message = "was blasted.";break;
#endif
		case GF_TIME:
#ifdef JP
			message = "縮んだ";break;
#else
			message = "shrank.";break;
#endif
		case GF_ACID:
		case GF_PURE_EARTH:
#ifdef JP
			message = "溶けた";break;
#else
			message = "melted.";break;
#endif
		case GF_COLD:
		case GF_ICE:
		case GF_PURE_AQUA:
#ifdef JP
			message = "凍り、砕け散った";break;
#else
			message = "was frozen and smashed.";break;
#endif
		case GF_ELEC:
		case GF_FIRE:
		case GF_PLASMA:
		case GF_PURE_FIRE:
		case GF_STRIKE_NOVA:
#ifdef JP
			message = "燃えた";break;
#else
			message = "burns up!";break;
#endif
		case GF_SHARDS:
		case GF_SOUND:
		case GF_CHAOS:
		case GF_DISENCHANT:
		case GF_FORCE:
		case GF_GRAVITY:
		case GF_ROCKET:
		case GF_MANA:
		case GF_METEOR:
		case GF_PURE_WIND:
#ifdef JP
			message = "粉砕された";break;
#else
			message = "was crushed.";break;
#endif
		case GF_STONE:
		case GF_SPECIAL_STONE:
#ifdef JP
			message = "石になった";break;
#else
			message = "was stoned.";break;
#endif
		default:
			message = NULL;break;
		}
		if (message)
		{
			if (c_ptr->info & (CAVE_MARK))
#ifdef JP
				msg_format("木は%s。", message);
#else
				msg_format("A tree %s", message);
#endif
			cave_set_feat(y, x, (one_in_(3) ? FEAT_DEEP_GRASS : FEAT_GRASS));

			/* Observe */
			if (c_ptr->info & (CAVE_MARK)) obvious = TRUE;

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

			if (!who && one_in_(20)) change_your_alignment(ALI_GNE, -1);
		}
	}

	/* Analyze the type */
	switch (typ)
	{
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
			if ((c_ptr->feat <= FEAT_INVIS) || (c_ptr->feat == FEAT_DIRT) || (c_ptr->feat == FEAT_GRASS) || (c_ptr->feat == FEAT_SWAMP) || (c_ptr->feat == FEAT_TUNDRA))
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
				cave_force_set_floor(y, x);
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
				cave_force_set_floor(y, x);

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
				cave_force_set_floor(y, x);
			}

			/* Rubble */
			else if (c_ptr->feat == FEAT_RUBBLE)
			{
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
				cave_force_set_floor(y, x);

				/* Hack -- place an object */
				if (randint0(100) < 10)
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
					place_object(y, x, AMF_OKAY);
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
				cave_force_set_floor(y, x);
			}

			/* Notice */
			note_spot(y, x);

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

			break;
		}

		/* Destroy Doors (and traps) */
		case GF_KILL_DOOR:
		{
			/* Destroy all doors and traps */
			if ((c_ptr->feat == FEAT_OPEN) ||
			    (c_ptr->feat == FEAT_BROKEN) ||
			    is_trap(c_ptr->feat) ||
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
				}

				/* Visibility change */
				if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
					 (c_ptr->feat <= FEAT_DOOR_TAIL))
				{
					/* Update some things */
					p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);
				}

				/* Forget the door */
				c_ptr->info &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_force_set_floor(y, x);
			}

			/* Notice */
			note_spot(y, x);

			break;
		}

		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Reveal secret doors */
			if (is_hidden_door(c_ptr))
			{
				/* Pick a door */
				disclose_grid(y, x);

				/* Check line of sight */
				if (known)
				{
					obvious = TRUE;
				}
			}

			/* Destroy traps */
			if (is_trap(c_ptr->feat))
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
				cave_force_set_floor(y, x);
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

			break;
		}

		/* Make traps */
		case GF_MAKE_TRAP:
		{
			/* Require a "naked" floor grid */
			if ((c_ptr->feat != FEAT_FLOOR) &&
			    (c_ptr->feat != FEAT_GRASS) &&
			    (c_ptr->feat != FEAT_DIRT) &&
			    (c_ptr->feat != FEAT_SWAMP) &&
			    (c_ptr->feat != FEAT_TUNDRA) &&
			    (c_ptr->o_idx == 0) &&
			    (c_ptr->m_idx == 0))
				 break;

			/* Place a trap */
			place_trap(y, x);

			break;
		}

		/* Make doors */
		case GF_MAKE_TREE:
		{
			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Not on the player */
			if ((y == py) && (x == px)) break;

			/* Create a closed door */
			cave_set_feat(y, x, FEAT_TREES);

			/* Observe */
			if (c_ptr->info & (CAVE_MARK)) obvious = TRUE;

			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_MONSTERS | PU_MON_LITE);

			break;
		}

		case GF_MAKE_GLYPH:
		{
			/* Require a "naked" floor grid */
			if (!can_create_glyph_bold(y, x)) break;

			/* Create a glyph */
			c_ptr->info |= CAVE_OBJECT;
			c_ptr->mimic = FEAT_GLYPH;

			/* Notice */
			note_spot(y, x);

			/* Redraw */
			lite_spot(y, x);

			break;
		}

		/* Erase elements of the grid */
		case GF_ERASE_ELEM:
		{
			int cur_elem;
			feature_type *f_ptr = &f_info[c_ptr->mimic ? c_ptr->mimic : f_info[c_ptr->feat].mimic];

			/* Notice */
			if (player_can_see_bold(y, x)) obvious = TRUE;

			for (cur_elem = MIN_ELEM; cur_elem < ELEM_NUM; cur_elem++)
			{
				c_ptr->elem[cur_elem] = f_ptr->elem[cur_elem] + get_cur_weather_elem(cur_elem);
				if (c_ptr->elem[cur_elem] < -99) c_ptr->elem[cur_elem] = -99;
				else if (c_ptr->elem[cur_elem] > 99) c_ptr->elem[cur_elem] = 99;
			}

			if (p_ptr->action == ACTION_ELEMSCOPE)
			{
				/* Redraw */
				lite_spot(y, x);

				/* Mega-Hack -- Update the monster in the affected grid */
				if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);
			}

			/* All done */
			break;
		}

		/* Set CAVE_TEMP flag on the grid (dengerous) */
		case GF_CAVE_TEMP:
		{
			/* Avoid infinite recursion */
			if (c_ptr->info & (CAVE_TEMP)) break;

			/* Paranoia -- verify space */
			if (temp_n == TEMP_MAX) break;

			/* Mark the grid as "seen" */
			c_ptr->info |= (CAVE_TEMP);

			/* Add it to the "seen" set */
			temp_y[temp_n] = y;
			temp_x[temp_n] = x;
			temp_n++;

			/* All done */
			break;
		}

		case GF_WATER_FLOW:
		{
			/* Shallow Water */
			if (dam == 1)
			{
				/* Require a "naked" floor grid */
				if (!cave_naked_bold(y, x)) break;

				/* Place a shallow water */
				cave_set_feat(y, x, FEAT_SHAL_WATER);
			}
			/* Deep Water */
			else
			{
				/* Require a "naked" floor grid */
				if (cave_perma_bold(y, x) || !dam) break;

				/* Place a deep water */
				cave_set_feat(y, x, FEAT_DEEP_WATER);

				/* Dam is used as a counter for the number of grid to convert */
				dam--;
			}
			break;
		}

		/* Ignore most effects */
		default:
		{
			break;
		}
	}

	lite_spot(y, x);
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
static bool project_o(int who, int r, int y, int x, int dam, int typ, u32b flg)
{
	cave_type *c_ptr = &cave[y][x];

	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;
	bool known = player_has_los_bold(y, x);

	u32b flgs[TR_FLAG_SIZE];

	char o_name[MAX_NLEN];

	int k_idx = 0;
	bool is_potion = FALSE;


	/* XXX XXX XXX */
	who = who ? who : 0;

	/* Reduce damage by distance */
	if (!(flg & PROJECT_NO_REDUCE)) dam = (dam + r) / (r + 1);


	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		bool is_art_or_tarot = FALSE;
		bool ignore = FALSE;
		bool do_kill = FALSE;
#ifndef JP
		bool plural = FALSE;
#endif

		cptr note_kill = NULL;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Extract the flags */
		object_flags(o_ptr, flgs);

#ifndef JP
		/* Get the "plural"-ness */
		if (o_ptr->number > 1) plural = TRUE;
#endif

		/* Check for artifact or tarot */
		if (object_is_artifact(o_ptr)) is_art_or_tarot = TRUE;
		else if (o_ptr->tval == TV_TAROT) is_art_or_tarot = TRUE;

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

					if (have_flag(flgs, TR_IGNORE_ACID)) ignore = TRUE;
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

					if (have_flag(flgs, TR_IGNORE_ELEC)) ignore = TRUE;
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

					if (have_flag(flgs, TR_IGNORE_FIRE)) ignore = TRUE;
				}
				break;
			}

			/* Cold -- potions and flasks */
			case GF_COLD:
			{
				if (hates_cold(o_ptr))
				{
#ifdef JP
					note_kill = "砕け散ってしまった！";
#else
					note_kill = (plural ? " shatter!" : " shatters!");
#endif

					do_kill = TRUE;
					if (have_flag(flgs, TR_IGNORE_COLD)) ignore = TRUE;
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

					if (have_flag(flgs, TR_IGNORE_FIRE)) ignore = TRUE;
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

					if (have_flag(flgs, TR_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Hack -- break potions and such */
			case GF_SHARDS:
			case GF_SOUND:
			case GF_FORCE:
			case GF_ICE:
			{
				if (hates_cold(o_ptr))
				{
#ifdef JP
					note_kill = "砕け散ってしまった！";
#else
					note_kill = (plural ? " shatter!" : " shatters!");
#endif

					do_kill = TRUE;
				}
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

				if (have_flag(flgs, TR_RES_CHAOS)) ignore = TRUE;
				else if ((o_ptr->tval == TV_SCROLL) && (o_ptr->sval == SV_SCROLL_CHAOS)) ignore = TRUE;
				break;
			}

			/* Mana -- destroy everything */
			case GF_MANA:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "壊れてしまった！";
#else
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif

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

					if (have_flag(flgs, TR_IGNORE_FIRE)) ignore = TRUE;
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

					if (have_flag(flgs, TR_IGNORE_COLD)) ignore = TRUE;
				}
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

				break;
			}

			/* Holy Fire -- destroys cursed non-artifacts */
			case GF_HOLY_FIRE:
			{
				if (object_is_cursed(o_ptr) || have_flag(flgs, TR_UNHOLY))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "壊れてしまった！";
#else
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif

				}
				break;
			}

			/* Hell Fire -- destroys blessed non-artifacts */
			case GF_HELL_FIRE:
			{
				if (have_flag(flgs, TR_BLESSED))
				{
					do_kill = TRUE;
#ifdef JP
					note_kill = "壊れてしまった！";
#else
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif

				}
				break;
			}

			case GF_PURE_FIRE:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "燃えてしまった！";
#else
				note_kill = (plural ? " burn up!" : " burns up!");
#endif

				break;
			}

			case GF_PURE_AQUA:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "砕け散ってしまった！";
#else
				note_kill = (plural ? " shatter!" : " shatters!");
#endif

				break;
			}

			case GF_PURE_EARTH:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "壊れてしまった！";
#else
				note_kill = (plural ? " are destroyed!" : " is destroyed!");
#endif

				break;
			}

			case GF_PURE_WIND:
			{
				do_kill = TRUE;
#ifdef JP
				note_kill = "蒸発してしまった！";
#else
				note_kill = (plural ? " evaporate!" : " evaporates!");
#endif

				break;
			}

			/* Unlock chests */
			case GF_KILL_DOOR:
			case GF_KILL_TRAP:
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
						if (known && o_ptr->marked)
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

			case GF_ANIM_DEAD:
			{
				if (o_ptr->tval == TV_CORPSE)
				{
					int i;
					u32b mode = 0L;
					monster_race *z_ptr = &r_info[o_ptr->pval];
					bool opposite_align = FALSE;
					int dummy_who;

					if (who > 0) dummy_who = who;
					else if (!who) dummy_who = -1;
					else dummy_who = 0;

					if (!who || is_pet(&m_list[who]))
						mode |= PM_FORCE_PET;

					if (who > 0)
					{
						monster_type *m_ptr = &m_list[who];
						if (monster_has_hostile_alignment(m_ptr, z_ptr)) opposite_align = TRUE;
					}
					else if (!who)
					{
						if (monster_has_hostile_alignment(NULL, z_ptr)) opposite_align = TRUE;
					}

					for (i = 0; i < o_ptr->number ; i++)
					{
						if (((o_ptr->sval == SV_CORPSE) && (randint1(100) > 80)) ||
						    ((o_ptr->sval == SV_SKELETON) && (randint1(100) > 60)) ||
						    opposite_align)
						{
							if (!note_kill)
							{
#ifdef JP
					note_kill = "灰になった。";
#else
					note_kill = (plural ? " become dust." : " becomes dust.");
#endif
							}
							continue;
						}
						else if (summon_named_creature(dummy_who, y, x, o_ptr->pval, mode))
						{
#ifdef JP
					note_kill = "生き返った。";
#else
					note_kill = "rivived.";
#endif
						}
						else if (!note_kill)
						{
#ifdef JP
							note_kill = "灰になった。";
#else
							note_kill = (plural ? " become dust." : " becomes dust.");
#endif
						}
					}
					do_kill = TRUE;
					obvious = TRUE;
				}
				break;
			}
		}


		/* Attempt to destroy the object */
		if (do_kill)
		{
			/* Effect "observed" */
			if (known && o_ptr->marked)
			{
				obvious = TRUE;
				object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
			}

			/* Artifacts, and other objects, get to resist */
			if (is_art_or_tarot || ignore)
			{
				/* Observe the resist */
				if (known && o_ptr->marked)
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
				if (known && o_ptr->marked && note_kill)
				{
#ifdef JP
					msg_format("%sは%s", o_name, note_kill);
#else
					msg_format("The %s%s", o_name, note_kill);
#endif

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
/* "flg" was added. */
static bool project_m(int who, int r, int y, int x, int dam, int typ, u32b flg, int mod_elem_mode)
{
	int tmp;

	cave_type *c_ptr = &cave[y][x];

	monster_type *m_ptr = &m_list[c_ptr->m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char killer [80];

	/* Is the monster "seen"? */
	bool seen = m_ptr->ml;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

	/* Can the player know about this effect? */
	bool known = (m_ptr->cdis <= MAX_SIGHT);

	/* Can the player see the source of this effect? */
	bool see_s = ((who <= 0) || m_list[who].ml);

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

	/* Stoning */
	bool do_stone = FALSE;

	/* Time amount (amount to time) */
	int do_time = 0;

	/* HP Drain */
	bool do_drain = FALSE;

	/* HP & Mana Drain */
	bool dual_drain = FALSE;

	bool heal_leper = FALSE;

	int caster_lev = (who > 0) ? r_info[who].level : (p_ptr->lev * 2);

	/* Hold the monster name */
	char m_name[80];

	char m_poss[10];

	int photo = 0;

	/* Assume no note */
	cptr note = NULL;

	/* Assume a default death */
	cptr note_dies = extract_note_dies(r_ptr);

	/* Nobody here */
	if (!c_ptr->m_idx) return (FALSE);

	/* Never affect projector */
	if (who && (c_ptr->m_idx == who)) return (FALSE);
	if ((c_ptr->m_idx == p_ptr->riding) && !who && !(typ == GF_OLD_HEAL) && !(typ == GF_OLD_SPEED) && !(typ == GF_STAR_HEAL)) return (FALSE);

	/* Don't affect already death monsters */
	/* Prevents problems with chain reactions of exploding monsters */
	if (m_ptr->hp < 0) return (FALSE);

	/* Reduce damage by distance */
	if (!(flg & PROJECT_NO_REDUCE)) dam = (dam + r) / (r + 1);

	dam = modify_dam_by_elem(who, c_ptr->m_idx, dam, typ, mod_elem_mode);

	/* Get the monster name (BEFORE polymorphing) */
	monster_desc(m_name, m_ptr, 0);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, MD_PRON_VISIBLE | MD_POSSESSIVE);

	if (p_ptr->riding && (c_ptr->m_idx == p_ptr->riding)) disturb(1, 0);

	/* Analyze the damage type */
	switch (typ)
	{
		/* Acid */
		case GF_ACID:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_ACID))
			{
#ifdef JP
				note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 3;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_ACID);
			}
			else if (r_ptr->flags3 & (RF3_HURT_ACID))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ACID);
			}
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_ELEC))
			{
#ifdef JP
				note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 3;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_ELEC);
			}
			else if (r_ptr->flags3 & (RF3_HURT_ELEC))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ELEC);
			}
			break;
		}

		/* Fire damage */
		case GF_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_FIRE))
			{
#ifdef JP
				note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 3;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_FIRE);
			}
			else if (r_ptr->flags3 & (RF3_HURT_FIRE))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_FIRE);
			}
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_COLD))
			{
#ifdef JP
				note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 3;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_COLD);
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_COLD);
			}
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_POIS)
			{
#ifdef JP
				note = "にはかなり耐性がある！";
#else
				note = " resists a lot.";
#endif

				dam /= 3;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_POIS);
			}
			break;
		}

		/* Lite -- opposite of Dark */
		case GF_LITE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_LITE))
			{
#ifdef JP
				note = "には耐性がある！";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= (randint1(6)+6);
				if (seen) r_ptr->r_flagsr |= (RFR_RES_LITE);
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

		/* Lite, but only hurts susceptible creatures */
		case GF_LITE_WEAK:
		{
			if (!dam)
			{
				skipped = TRUE;
				break;
			}
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

		/* Dark -- opposite of Lite */
		case GF_DARK:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_DARK))
			{
#ifdef JP
				note = "には耐性がある！";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= (randint1(6)+6);
				if (seen) r_ptr->r_flagsr |= (RFR_RES_DARK);
			}
			break;
		}

		/* Nether */
		case GF_NETHER:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_NETH)
			{
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
				else
				{
#ifdef JP
					note = "には耐性がある。";
#else
					note = " resists.";
#endif

					dam *= 3; dam /= randint1(6) + 6;
				}
				if (seen) r_ptr->r_flagsr |= (RFR_RES_NETH);
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
			if (m_ptr->r_idx == MON_WATER_ELEM)
			{
#ifdef JP
				note = "には完全な耐性がある。";
#else
				note = " is immune.";
#endif

				dam = 0;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_WATE);
			}
			else if (r_ptr->flagsr & RFR_RES_WATE)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_WATE);
			}
			break;
		}

		/* Plasma -- XXX perhaps check ELEC or FIRE */
		case GF_PLASMA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_PLAS)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_PLAS);
			}
			break;
		}

		/* Shards */
		case GF_SHARDS:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_SHAR)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_SHAR);
			}
			break;
		}

		/* Sound */
		case GF_SOUND:
		{
			if (seen) obvious = TRUE;
			do_stun = (10 + randint1(15) + r) / (r + 1);
			if (r_ptr->flagsr & RFR_RES_SOUN)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_SOUN);
			}
			break;
		}

		/* Confusion */
		case GF_CONFUSION:
		{
			if (seen) obvious = TRUE;
			do_conf = (10 + randint1(15) + r) / (r + 1);
			if (r_ptr->flagsr & RFR_RES_CONF)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 2; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_CONF);
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

		/* Chaos */
		case GF_CHAOS:
		{
			if (seen) obvious = TRUE;
			do_poly = TRUE;
			do_conf = (5 + randint1(11) + r) / (r + 1);
			if (r_ptr->flags7 & RF7_EGG_ONLY) do_poly = FALSE;
			if ((r_ptr->flagsr & RFR_RES_CHAO) ||
			    ((r_ptr->flags3 & RF3_DEMON) && one_in_(3)))
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				do_poly = FALSE;
				if (seen && (r_ptr->flagsr & RFR_RES_CHAO))
					r_ptr->r_flagsr |= (RFR_RES_CHAO);
			}
			break;
		}

		/* Stone */
		case GF_STONE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_STON)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_STON);
			}
			else if (r_ptr->flags3 & RF3_NO_STONE)
			{
#ifdef JP
				note = "はいくらか耐性を示した。";
#else
				note = " resists somewhat.";
#endif

				dam /= 2;
			}
			else if ((((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2))) && !one_in_(888))
			         || (r_ptr->level > randint1((caster_lev / 2) + 20)))
			{
#ifdef JP
				note = "は石化に抵抗した！";
#else
				note = " resists stoning!";
#endif
			}
			else if (r_ptr->flagsr & (RFR_RES_ACID | RFR_RES_SHAR))
			{
				if (set_monster_stoning(c_ptr->m_idx, 1))
				{
#ifdef JP
					note = "が石化し始めた！";
#else
					note = " gets stoning!";
#endif
				}
			}
			else
			{
#ifdef JP
				note_dies = "は石像になった。";
#else
				note_dies = " turns into a stone statue.";
#endif
				do_stone = TRUE;
			}
			break;
		}

		/* Disenchantment */
		case GF_DISENCHANT:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_DISE)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_DISE);
			}
			break;
		}

		/* Force */
		case GF_FORCE:
		{
			if (seen) obvious = TRUE;
			do_stun = (randint1(15) + r) / (r + 1);
			if (r_ptr->flagsr & RFR_RES_WALL)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_WALL);
			}
			break;
		}

		/* Inertia */
		case GF_INERTIA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_INER))
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_INER);
			}
			else
			{
				/* Powerful monsters can resist */
				if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
				    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
				{
					obvious = FALSE;
				}
				/* Normal monsters slow down */
				else
				{
					if (set_monster_slow(c_ptr->m_idx, MON_SLOW(m_ptr) + 50))
					{
#ifdef JP
						note = "の動きが遅くなった。";
#else
						note = " starts moving slower.";
#endif
					}
				}
			}
			break;
		}

		/* Time */
		case GF_TIME:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_TIME))
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_TIME);
			}
			else do_time = (dam+1)/2;
			break;
		}

		/* Gravity */
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
			if (p_ptr->riding && (c_ptr->m_idx == p_ptr->riding)) do_dist = 0;

			if (r_ptr->flagsr & (RFR_RES_GRAV))
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				do_dist = 0;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_GRAV);
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
					if (set_monster_slow(c_ptr->m_idx, MON_SLOW(m_ptr) + 50))
					{
#ifdef JP
						note = "の動きが遅くなった。";
#else
						note = " starts moving slower.";
#endif
					}
				}

				/* 2. stun */
				do_stun = damroll((caster_lev / 20) + 3 , (dam)) + 1;

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

		/* Ice -- Cold + Cuts + Stun */
		case GF_ICE:
		{
			if (seen) obvious = TRUE;
			do_stun = (randint1(15) + 1) / (r + 1);
			if (r_ptr->flagsr & RFR_RES_COLD)
			{
#ifdef JP
				note = "にはかなり耐性がある。";
#else
				note = " resists a lot.";
#endif

				dam /= 3;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_COLD);
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_COLD);
			}
			break;
		}

		/* Nuclear waste */
		case GF_NUKE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_POIS)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_POIS);
			}
			else if (one_in_(3)) do_poly = TRUE;
			break;
		}

		/* Rocket: Shard resistance helps */
		case GF_ROCKET:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_SHAR)
			{
#ifdef JP
				note = "はいくらか耐性を示した。";
#else
				note = " resists somewhat.";
#endif

				dam /= 2;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_SHAR);
			}
			break;
		}

		/* Magic Missile -- pure damage */
		/* Mana -- pure damage */
		/* Meteor -- powerful magic missile */
		/* Godly Spear -- powerful magic missile */
		case GF_MISSILE:
		case GF_MANA:
		case GF_METEOR:
		case GF_GODLY_SPEAR:
		{
			if (seen) obvious = TRUE;
			break;
		}

		/* Physical (miss chance by AC) */
		case GF_PHYSICAL:
		{
			if (seen) obvious = TRUE;
			if (!test_hit_fire(100 + caster_lev * 4, r_ptr->ac + MON_STONING(m_ptr) / 5, (who || m_ptr->ml)))
			{
#ifdef JP
				note = "は攻撃をかわした。";
#else
				note = " avoids attack.";
#endif

				/* No damage */
				dam = 0;
			}
			break;
		}

		/* Blunt Weapon (miss chance by AC) */
		case GF_BLUNT:
		{
			if (seen) obvious = TRUE;
			if (!test_hit_fire(100 + caster_lev * 4, r_ptr->ac + MON_STONING(m_ptr) / 5, (who || m_ptr->ml)))
			{
#ifdef JP
				note = "は攻撃をかわした。";
#else
				note = " avoids attack.";
#endif

				/* No damage */
				dam = 0;
			}
			else if (r_ptr->flagsr & (RFR_IM_BLUNT))
			{
#ifdef JP
				note = "にはかなり耐性がある！";
#else
				note = " resists a lot!";
#endif

				dam /= 6;
				if (seen)
				{
					r_ptr->r_flagsr |= (RFR_IM_BLUNT);
					if (r_ptr->flagsr & (RFR_RES_BLUNT)) r_ptr->r_flagsr |= (RFR_RES_BLUNT);
				}
			}
			else if (r_ptr->flagsr & (RFR_RES_BLUNT))
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam /= 2;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_BLUNT);
			}
			break;
		}

		/* Edged Weapon (miss chance by AC) */
		case GF_EDGED:
		{
			if (seen) obvious = TRUE;
			if (!test_hit_fire(100 + caster_lev * 4, r_ptr->ac + MON_STONING(m_ptr) / 5, (who || m_ptr->ml)))
			{
#ifdef JP
				note = "は攻撃をかわした。";
#else
				note = " avoids attack.";
#endif

				/* No damage */
				dam = 0;
			}
			else if (r_ptr->flagsr & (RFR_IM_EDGED))
			{
#ifdef JP
				note = "にはかなり耐性がある！";
#else
				note = " resists a lot!";
#endif

				dam /= 6;
				if (seen)
				{
					r_ptr->r_flagsr |= (RFR_IM_EDGED);
					if (r_ptr->flagsr & (RFR_RES_EDGED)) r_ptr->r_flagsr |= (RFR_RES_EDGED);
				}
			}
			else if (r_ptr->flagsr & (RFR_RES_EDGED))
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam /= 2;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_EDGED);
			}
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
			break;
		}

		/* Holy Fire -- hurts Evil */
		case GF_HOLY_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & RF3_EVIL)
			{
				dam *= 2;
#ifdef JP
				note = "はひどい痛手を受けた。";
#else
				note = " is hit hard.";
#endif

				if (seen) r_ptr->r_flags3 |= RF3_EVIL;
			}
			break;
		}

		/* Hellfire -- hurts Good */
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

		/* *Fire*: Pure damage */
		case GF_PURE_FIRE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_FIRE))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_FIRE);
			}
			break;
		}

		/* *Aqua*: Pure damage */
		case GF_PURE_AQUA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_COLD);
			}
			break;
		}

		/* *Earth*: Pure damage */
		case GF_PURE_EARTH:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_ACID))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ACID);
			}
			break;
		}

		/* *Wind*: Pure damage */
		case GF_PURE_WIND:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_HURT_ELEC))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ELEC);
			}
			if (r_ptr->flags3 & (RF3_HURT_ROCK))
			{
#ifdef JP
				note = "の皮膚がただれた！";
				note_dies = "は蒸発した！";
#else
				note = " loses some skin!";
				note_dies = " evaporates!";
#endif

				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_ROCK);
			}
			break;
		}

		/* CAUSE_1 */
		case GF_CAUSE_1:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if (randint0(100 + (caster_lev / 2)) < (r_ptr->level + 35))
			{

#ifdef JP
				note = "には効果がなかった。";
#else
				note = "is unaffected!";
#endif
				dam = 0;
			}
			break;
		}

		/* CAUSE_2 */
		case GF_CAUSE_2:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if (randint0(100 + (caster_lev / 2)) < (r_ptr->level + 35))
			{

#ifdef JP
				note = "には効果がなかった。";
#else
				note = "is unaffected!";
#endif
				dam = 0;
			}
			break;
		}

		/* CAUSE_3 */
		case GF_CAUSE_3:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if (randint0(100 + (caster_lev / 2)) < (r_ptr->level + 35))
			{

#ifdef JP
				note = "には効果がなかった。";
#else
				note = "is unaffected!";
#endif
				dam = 0;
			}
			break;
		}

		/* CAUSE_4 */
		case GF_CAUSE_4:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if (randint0(100 + (caster_lev / 2)) < (r_ptr->level + 35))
			{

#ifdef JP
				note = "には効果がなかった。";
#else
				note = "is unaffected!";
#endif
				dam = 0;
			}
			break;
		}

		/* Hand of Doom (Use "dam" as "power") */
		case GF_HAND_DOOM:
		{
			if (seen) obvious = TRUE;

			if (!(r_ptr->flags1 & RF1_UNIQUE) && (dam > (r_ptr->level + 10 + randint1(20))))
			{
				dam = ((40 + randint1(20)) * m_ptr->hp) / 100;

				if (m_ptr->hp < dam) dam = m_ptr->hp - 1;
			}
			else
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = "is unaffected!";
#endif
				dam = 0;
			}
			break;
		}

		/* Death Ray */
		case GF_DEATH_RAY:
		{
			if (seen) obvious = TRUE;

			if (!monster_living(r_ptr))
			{
				if (seen)
				{
					if (r_ptr->flags3 & RF3_DEMON) r_ptr->r_flags3 |= (RF3_DEMON);
					if (r_ptr->flags3 & RF3_UNDEAD) r_ptr->r_flags3 |= (RF3_UNDEAD);
					if (r_ptr->flags3 & RF3_NONLIVING) r_ptr->r_flags3 |= (RF3_NONLIVING);
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
				 (((r_ptr->level + randint1(20)) > randint1((caster_lev / 2) + randint1(10))) &&
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
					  (r_ptr->level > (caster_lev / 4)) &&
					  one_in_(2))
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
					if (randint0(100 + r_ptr->level/2) < p_ptr->skill_sav)
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
						monster_desc(killer, m_ptr, MD_IGNORE_HALLU | MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);
						ACTIVATE_MULTISHADOW();
						take_hit(DAMAGE_ATTACK, dam, killer);  /* has already been /3 */
						if (one_in_(4) && !IS_MULTISHADOW(0))
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
						STOP_MULTISHADOW();
					}
					dam = 0;
				}
			}

			if ((dam > 0) && one_in_(4))
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

		/* Mind blast */
		case GF_MIND_BLAST:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
				 (r_ptr->flags3 & RF3_NO_CONF) ||
				 (r_ptr->level > randint1((caster_lev - 10) < 1 ? 1 : (caster_lev - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					r_ptr->r_flags3 |= (RF3_NO_CONF);
				}
#ifdef JP
				note = "には効果がなかった。";
#else
				note = "is unaffected!";
#endif
				dam = 0;
			}
			else
			{
#ifdef JP
				msg_format("%sは精神攻撃を食らった。",m_name);
				note_dies = "の精神は崩壊し、肉体は抜け殻となった。";
#else
				msg_format("%^s is blasted by psionic energy.", m_name);
				note_dies = " collapses, a mindless husk.";
#endif

				do_conf = randint0(8) + 8;
			}
			break;
		}

		/* Brain smash */
		case GF_BRAIN_SMASH:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
				 (r_ptr->flags3 & RF3_NO_CONF) ||
				 (r_ptr->level > randint1((caster_lev - 10) < 1 ? 1 : (caster_lev - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					r_ptr->r_flags3 |= (RF3_NO_CONF);
				}
#ifdef JP
				note = "には効果がなかった。";
#else
				note = "is unaffected!";
#endif
				dam = 0;
			}
			else
			{
#ifdef JP
				msg_format("%sは精神攻撃を食らった。",m_name);
				note_dies = "の精神は崩壊し、肉体は抜け殻となった。";
#else
				msg_format("%^s is blasted by psionic energy.", m_name);
				note_dies = " collapses, a mindless husk.";
#endif

				do_conf = randint0(8) + 8;
				do_stun = randint0(8) + 8;
				(void)set_monster_slow(c_ptr->m_idx, MON_SLOW(m_ptr) + 10);
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

		/* Clone monsters (Ignore "dam") */
		case GF_OLD_CLONE:
		{
			if (seen) obvious = TRUE;

			if (is_pet(m_ptr) || (r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)))
			{
#ifdef JP
				note = "には効果がなかった。";
#else
				note = " is unaffected!";
#endif
			}
			else
			{
				/* Heal fully */
				m_ptr->hp = m_ptr->maxhp;

				/* Attempt to clone. */
				if (multiply_monster(c_ptr->m_idx, TRUE, 0L))
				{
#ifdef JP
					note = "が分裂した！";
#else
					note = " spawns!";
#endif

				}
			}

			/* No "real" damage */
			dam = 0;

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

		/* Heal Monster (use "dam" as amount of healing) */
		case GF_STAR_HEAL:
		{
			if (seen) obvious = TRUE;

			/* Wake up */
			(void)set_monster_csleep(c_ptr->m_idx, 0);

			if (m_ptr->maxhp < m_ptr->max_maxhp)
			{
#ifdef JP
				msg_format("%^sの強さが戻った。", m_name);
#else
				msg_format("%^s recovers %s vitality.", m_name, m_poss);
#endif
				m_ptr->maxhp = m_ptr->max_maxhp;
			}
			if (!dam) break;

			if (MON_STONING(m_ptr))
			{
#ifdef JP
				msg_format("%^sの石化が止まった。", m_name);
#else
				msg_format("%^s is no longer stoning.", m_name);
#endif
				(void)set_monster_stoning(c_ptr->m_idx, 0);
			}
			/* Fall through */
		}
		case GF_OLD_HEAL:
		{
			if (seen) obvious = TRUE;

			/* Wake up */
			(void)set_monster_csleep(c_ptr->m_idx, 0);

			if (MON_STUNNED(m_ptr))
			{
#ifdef JP
				msg_format("%^sは朦朧状態から立ち直った。", m_name);
#else
				msg_format("%^s is no longer stunned.", m_name);
#endif
				(void)set_monster_stunned(c_ptr->m_idx, 0);
			}
			if (MON_CONFUSED(m_ptr))
			{
#ifdef JP
				msg_format("%^sは混乱から立ち直った。", m_name);
#else
				msg_format("%^s is no longer confused.", m_name);
#endif
				(void)set_monster_confused(c_ptr->m_idx, 0);
			}
			if (MON_MONFEAR(m_ptr))
			{
#ifdef JP
				msg_format("%^sは勇気を取り戻した。", m_name);
#else
				msg_format("%^s recovers %s courage.", m_name, m_poss);
#endif
				(void)set_monster_monfear(c_ptr->m_idx, 0);
			}

			/* Heal */
			if (m_ptr->hp < MAX_MAX_MAXHP) m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			if ((m_ptr->r_idx == MON_LEPER) && !p_ptr->inside_arena)
			{
				if (!dun_level && p_ptr->town_num) change_chaos_frame(town[p_ptr->town_num].ethnic, 5);
				heal_leper = TRUE;
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);
			if (p_ptr->riding == c_ptr->m_idx) p_ptr->redraw |= (PR_UHEALTH);

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
			if (set_monster_fast(c_ptr->m_idx, MON_FAST(m_ptr) + 100))
			{
#ifdef JP
				note = "の動きが速くなった。";
#else
				note = " starts moving faster.";
#endif
			}

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
				if (set_monster_slow(c_ptr->m_idx, MON_SLOW(m_ptr) + 50))
				{
#ifdef JP
					note = "の動きが遅くなった。";
#else
					note = " starts moving slower.";
#endif
				}
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

		/* Drain Life */
		case GF_OLD_DRAIN:
		{
			if (seen) obvious = TRUE;

			if (!monster_living(r_ptr))
			{
				if (seen)
				{
					if (r_ptr->flags3 & RF3_DEMON) r_ptr->r_flags3 |= (RF3_DEMON);
					if (r_ptr->flags3 & RF3_UNDEAD) r_ptr->r_flags3 |= (RF3_UNDEAD);
					if (r_ptr->flags3 & RF3_NONLIVING) r_ptr->r_flags3 |= (RF3_NONLIVING);
				}

#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
				dam = 0;
			}
			else do_time = (dam+7)/8;

			break;
		}

		/* Stoning (Use "dam" as "power") */
		case GF_OLD_STONE:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)) ||
			    (r_ptr->flags3 & (RF3_NO_STONE)) || (r_ptr->flagsr & (RFR_RES_STON)) ||
			    (r_ptr->level > randint1((caster_lev / 2) + 20)))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NO_STONE))
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_STONE);
				}

				/* No obvious effect */
#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
			}
			else if (r_ptr->flagsr & (RFR_RES_ACID | RFR_RES_SHAR))
			{
				if (!MON_STUNNED(m_ptr))
				{
#ifdef JP
					note = "が石化し始めた！";
#else
					note = " gets stoning!";
#endif
					(void)set_monster_stoning(c_ptr->m_idx, 1);
				}
			}
			else
			{
#ifdef JP
				note_dies = "は石像になった。";
#else
				note_dies = " turns into a stone statue.";
#endif
				/* Get stoned later */
				do_stone = TRUE;
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Slow Monster (Use "dam" as "turn") */
		case GF_NEW_SLOW:
		{
			if (seen) obvious = TRUE;

			/* Unique monsters can resist */
			if (((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & RF7_UNIQUE2)) &&
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
				if (set_monster_slow(c_ptr->m_idx, MON_SLOW(m_ptr) + dam))
				{
#ifdef JP
					note = "の動きが遅くなった。";
#else
					note = " starts moving slower.";
#endif
				}
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		/* Drain Life (Type-2) */
		case GF_NEW_DRAIN:
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
			else
			{
				do_time = (dam+7)/8;
				do_drain = TRUE;
			}

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

		/* Sleep (Use "dam" as "power") */
		case GF_STASIS_EVIL:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_UNIQUE) ||
			    !(r_ptr->flags3 & RF3_EVIL) ||
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

		case GF_STUN:
		{
			if (seen) obvious = TRUE;

			do_stun = damroll((caster_lev / 20) + 3 , (dam)) + 1;

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

		/* Charm monster */
		case GF_CHARM:
		{
			dam += (adj_con_fix[p_ptr->stat_ind[A_CHR]] - 1);

			if (seen) obvious = TRUE;

			if (p_ptr->inside_arena)
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is immune.";
#endif
				dam = 0;
				break;
			}

			if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL))
				dam = dam * 2 / 3;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_QUESTOR) ||
			    (r_ptr->flags3 & RF3_NO_CONF) ||
			    (m_ptr->mflag2 & MFLAG2_NOPET) ||
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

				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
			}
			else if (p_ptr->cursed & TRC_AGGRAVATE)
			{
#ifdef JP
				note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
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

		/* Silent (Use "dam" as power)  */
		case GF_SILENT:
		{
			if (seen) obvious = TRUE;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) ||
			    (r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)) ||
			    (r_ptr->flags3 & RF3_NO_STUN) ||
			    (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & RF3_NO_STUN)
				{
					if (seen) r_ptr->r_flags3 |= (RF3_NO_STUN);
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
				if (set_monster_silent(c_ptr->m_idx, MON_SILENT(m_ptr) + 50))
				{
#ifdef JP
					note = "は沈黙した！";
#else
					note = " is quiet!";
#endif
				}
			}

			/* No "real" damage */
			dam = 0;
			break;
		}

		case GF_CRUSADE:
		{
			bool success = FALSE;
			byte sub_align_p = SUB_ALIGN_NEUTRAL;
			byte sub_align_m = SUB_ALIGN_NEUTRAL;

			if (who > 0) break;
			if (seen) obvious = TRUE;

			switch (get_your_alignment_lnc())
			{
			case ALIGN_LNC_LAWFUL:
				sub_align_p |= (SUB_ALIGN_LAWFUL);
				break;
			case ALIGN_LNC_CHAOTIC:
				sub_align_p |= (SUB_ALIGN_CHAOTIC);
				break;
			}

			if (r_ptr->flags7 & RF7_LAWFUL) sub_align_m |= SUB_ALIGN_LAWFUL;
			if (r_ptr->flags7 & RF7_CHAOTIC) sub_align_m |= SUB_ALIGN_CHAOTIC;

			if ((sub_align_p == sub_align_m) && !p_ptr->inside_arena)
			{
				if (r_ptr->flags3 & (RF3_NO_CONF)) dam -= 50;
				if (dam < 1) dam = 1;

				/* No need to tame your pet */
				if (is_pet(m_ptr))
				{
#ifdef JP
					note = "の動きが速くなった。";
#else
					note = " starts moving faster.";
#endif

					(void)set_monster_fast(c_ptr->m_idx, MON_FAST(m_ptr) + 100);
					success = TRUE;
				}

				/* Attempt a saving throw */
				else if ((r_ptr->flags1 & (RF1_QUESTOR)) ||
				    (r_ptr->flags1 & (RF1_UNIQUE)) ||
				    (m_ptr->mflag2 & MFLAG2_NOPET) ||
				    (p_ptr->cursed & TRC_AGGRAVATE) ||
					 ((r_ptr->level+10) > randint1(dam)))
				{
					/* Resist */
					if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
				}
				else
				{
#ifdef JP
					note = "を支配した。";
#else
					note = " is tamed!";
#endif

					set_pet(m_ptr);
					(void)set_monster_fast(c_ptr->m_idx, MON_FAST(m_ptr) + 100);

					success = TRUE;
				}
			}

			if (!success && !is_pet(m_ptr))
			{
				if (!(r_ptr->flags3 & RF3_NO_FEAR))
				{
					do_fear = randint1(90)+10;
				}
				else if (seen)
				{
					r_ptr->r_flags3 |= (RF3_NO_FEAR);
				}
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

		/* Control undead */
		case GF_CONTROL_UNDEAD:
		{
			if (seen) obvious = TRUE;

			if (p_ptr->inside_arena)
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is immune.";
#endif
				dam = 0;
				break;
			}

			if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL))
				dam = dam * 2 / 3;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & RF1_QUESTOR) ||
			  (!(r_ptr->flags3 & RF3_UNDEAD)) ||
			    (m_ptr->mflag2 & MFLAG2_NOPET) ||
				 (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				/* No obvious effect */
#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
			}
			else if (p_ptr->cursed & TRC_AGGRAVATE)
			{
#ifdef JP
				note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
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

			if (p_ptr->inside_arena)
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is immune.";
#endif
				dam = 0;
				break;
			}

			if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL))
				dam = dam * 2 / 3;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) ||
			  (!(r_ptr->flags3 & (RF3_ANIMAL))) ||
			    (m_ptr->mflag2 & MFLAG2_NOPET) ||
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
				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
			}
			else if (p_ptr->cursed & TRC_AGGRAVATE)
			{
#ifdef JP
				note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
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

		/* Tame beast */
		case GF_CONTROL_BEAST:
		{
			if (seen) obvious = TRUE;

			if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL))
				dam = dam * 2 / 3;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) ||
			  (!(r_ptr->d_char == 'H')) ||
			    (m_ptr->mflag2 & MFLAG2_NOPET) ||
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
				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
			}
			else if (p_ptr->cursed & TRC_AGGRAVATE)
			{
#ifdef JP
				note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
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

		/* Tame dragon */
		case GF_CONTROL_DRAGON:
		{
			if (seen) obvious = TRUE;

			if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL))
				dam = dam * 2 / 3;

			/* Attempt a saving throw */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) ||
			  (!(r_ptr->flags3 & (RF3_DRAGON))) ||
			    (m_ptr->mflag2 & MFLAG2_NOPET) ||
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
				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
			}
			else if (p_ptr->cursed & TRC_AGGRAVATE)
			{
#ifdef JP
				note = "はあなたに敵意を抱いている！";
#else
				note = " hates you too much!";
#endif

				if (one_in_(4)) m_ptr->mflag2 |= MFLAG2_NOPET;
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

		/* GENOCIDE */
		case GF_GENOCIDE:
		{
			bool angry = FALSE;
			if (seen) obvious = TRUE;

			if (one_in_(3)) change_your_alignment(ALI_GNE, -1);

			if (((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & (RF7_UNIQUE2)) || (c_ptr->m_idx == p_ptr->riding)) || p_ptr->inside_arena || p_ptr->inside_quest)
			{
				dam = 0;
				angry = TRUE;
			}
			else
			{
				if ((r_ptr->level > randint0(dam)) || (m_ptr->mflag2 & MFLAG2_NOGENO))
				{
					dam = 0;
					angry = TRUE;
				}
				else
				{
					if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
					{
						char m2_name[80];

						monster_desc(m2_name, m_ptr, MD_INDEF_VISIBLE);
						do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_GENOCIDE, m2_name);
					}

					delete_monster_idx(c_ptr->m_idx);
#ifdef JP
					msg_format("%sは消滅した！",m_name);
#else
					msg_format("%^s disappered!",m_name);
#endif

#ifdef JP
					take_hit(DAMAGE_GENO, randint1((r_ptr->level+1)/2), "モンスター消滅の呪文を唱えた疲労");
#else
					take_hit(DAMAGE_GENO, randint1((r_ptr->level+1)/2), "the strain of casting Genocide One");
#endif
					dam = 0;

					skipped = TRUE;

					/* Redraw */
					p_ptr->redraw |= (PR_HP);

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER);
					return TRUE;
				}
			}
			if (angry)
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = "is unaffected!";
#endif
				get_angry = TRUE;
				if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
			}
			break;
		}

		/* GENOCIDE_UNDEAD */
		case GF_GENOCIDE_UNDEAD:
		{
			bool angry = FALSE;
			if (seen) obvious = TRUE;

			if (one_in_(3)) change_your_alignment(ALI_GNE, 1);

			if (!(r_ptr->flags3 & RF3_UNDEAD))
			{
				/* Irrelevant */
				skipped = TRUE;

				/* No damage */
				dam = 0;

				break;
			}

			if (seen) r_ptr->r_flags3 |= (RF3_UNDEAD);

			if (((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & (RF7_UNIQUE2)) || (c_ptr->m_idx == p_ptr->riding)) || p_ptr->inside_arena || p_ptr->inside_quest)
			{
				dam = 0;
				angry = TRUE;
			}
			else
			{
				if ((r_ptr->level > randint0(dam)) || (m_ptr->mflag2 & MFLAG2_NOGENO))
				{
					dam = 0;
					angry = TRUE;
				}
				else
				{
					delete_monster_idx(c_ptr->m_idx);
#ifdef JP
					msg_format("%sは消滅した！",m_name);
#else
					msg_format("%^s disappered!",m_name);
#endif

#ifdef JP
					take_hit(DAMAGE_GENO, randint1((r_ptr->level+1)/2), "アンデッドを消滅させた疲労");
#else
					take_hit(DAMAGE_GENO, randint1((r_ptr->level+1)/2), "the strain of Genocide Undead");
#endif
					dam = 0;

					skipped = TRUE;

					/* Redraw */
					p_ptr->redraw |= (PR_HP);

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER);
					return TRUE;
				}
			}
			if (angry)
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = "is unaffected!";
#endif
				get_angry = TRUE;
				if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
			}
			break;
		}

		/* Drain mana */
		case GF_DRAIN_MANA:
		{
			if (seen) obvious = TRUE;

			if ((r_ptr->flags4 & ~(RF4_NOMAGIC_MASK)) || (r_ptr->flags5 & ~(RF5_NOMAGIC_MASK)) || (r_ptr->flags6 & ~(RF6_NOMAGIC_MASK)) || (r_ptr->flagsa & ~(RFA_NOMAGIC_MASK)))
			{
				if (who > 0)
				{
					monster_type *caster_ptr = &m_list[who];

					/* Heal the monster */
					if (caster_ptr->hp < caster_ptr->maxhp)
					{
						/* Heal */
						caster_ptr->hp += 6 * dam;
						if (caster_ptr->hp > caster_ptr->maxhp) caster_ptr->hp = caster_ptr->maxhp;

						/* Redraw (later) if needed */
						if (p_ptr->health_who == who) p_ptr->redraw |= (PR_HEALTH);
						if (p_ptr->riding == who) p_ptr->redraw |= (PR_UHEALTH);

						/* Special message */
						if (caster_ptr->ml)
						{
							/* Get the monster name */
							monster_desc(killer, caster_ptr, 0);
#ifdef JP
							msg_format("%^sは気分が良さそうだ。", killer);
#else
							msg_format("%^s appears healthier.", killer);
#endif
						}
					}
				}
				else
				{
					/* Message */
#ifdef JP
 					msg_format("%sから精神エネルギーを吸いとった。",m_name);
#else
					msg_format("You draw psychic energy from %s.", m_name);
#endif

					p_ptr->csp += dam;
					if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
					p_ptr->redraw |= (PR_MANA);
				}
			}
			else
			{
#ifdef JP
				msg_format("%sには効果がなかった。",m_name);
#else
				msg_format("%s is unaffected.", m_name);
#endif
				obvious = FALSE;
			}
			dam = 0;
			break;
		}

		case GF_ATTACK:
		{
			if (seen) obvious = TRUE;
			skipped = TRUE;
			if (dam == PY_ATTACK_NYUSIN)
			{
				int i;
				int ny = y, nx = x;
				bool success = FALSE;
				for (i = 0; i < 8; i++)
				{
					if (cave_empty_bold(y+ddy[i], x+ddx[i]) || ((y+ddy[i] == py) && (x+ddx[i] == px)))
					{
						success = TRUE;
						if (distance(py, px, ny, nx) > distance(py, px, y+ddy[i], x+ddx[i]))
						{
							ny = y+ddy[i];
							nx = x+ddx[i];
						}
					}
				}
				if (success)
				{
					if ((ny != py) || (nx != px))
					{
						teleport_player_to(ny, nx, FALSE, TRUE);
#ifdef JP
						msg_print("素早く相手の懐に入り込んだ！");
#else
						msg_format("You quickly jump in and attack %s!", m_name);
#endif
					}
				}
				else
				{
#ifdef JP
					msg_print("失敗！");
#else
					msg_print("Failed!");
#endif
					dam = 0;
					break;
				}
			}
			if (c_ptr->m_idx)
				return (py_attack(y, x, dam));
			else
#ifdef JP
				msg_print("攻撃は空を切った。");
#else
				msg_print("You attack the empty air.");
#endif
			dam = 0;
			break;
		}

		case GF_PHOTO:
		{
#ifdef JP
			if (!who) msg_format("%sを写真に撮った。", m_name);
#else
			if (!who) msg_format("You take a photograph of %s.", m_name);
#endif
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

			photo = m_ptr->r_idx;

			break;
		}

		/* Strike Nova */
		case GF_STRIKE_NOVA:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & (RFR_RES_FIRE))
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif
				dam = 0;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_FIRE);
			}
			else if (r_ptr->flags3 & (RF3_HURT_FIRE))
			{
#ifdef JP
				note = "はひどい痛手をうけた。";
#else
				note = " is hit hard.";
#endif
				dam *= 2;
				if (seen) r_ptr->r_flags3 |= (RF3_HURT_FIRE);
			}
			break;
		}

		/*
		 * Word of Pain: Pure damage (Note: This GF is not affected elements)
		 * Use "dam" as "mode"
		 */
		case GF_WORD_OF_PAIN:
		{
			if (seen) obvious = TRUE;
			if (dam == 1)
			{
				if (monster_living(r_ptr)) do_drain = TRUE;
			}

			if (who) dam = m_list[who].maxhp - m_list[who].hp;
			else dam = p_ptr->mhp - p_ptr->chp;
			break;
		}

		/* Shining (Use "dam" as "power", real damage is HP/3 of caster) */
		case GF_SHINING:
		{
			if (seen) obvious = TRUE;

			/* Saving throw */
			if (r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
			{
#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
				dam = 0;
			}
			else
			{
				/* "dam" is from "power" to real damage */
				if (who)
				{
					dam = m_list[who].hp / 3;
				}
				else
				{
					dam = p_ptr->chp / 3;
				}

				/* Reduce damage by distance */
				if (!(flg & PROJECT_NO_REDUCE)) dam = (dam + r) / (r + 1);

				dam = modify_dam_by_elem(who, c_ptr->m_idx, dam, typ, mod_elem_mode);

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
			}
			break;
		}

		/* Stone of special blow */
		case GF_SPECIAL_STONE:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flagsr & RFR_RES_STON)
			{
#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				dam *= 3; dam /= randint1(6) + 6;
				if (seen) r_ptr->r_flagsr |= (RFR_RES_STON);
			}
			else if (r_ptr->flags3 & RF3_NO_STONE)
			{
#ifdef JP
				note = "はいくらか耐性を示した。";
#else
				note = " resists somewhat.";
#endif

				dam /= 2;
				if (seen)
				{
					r_ptr->r_flags3 |= (RF3_NO_STONE);
					if (r_ptr->flagsr & RFR_RES_STON) r_ptr->r_flagsr |= (RFR_RES_STON);
				}
			}
			else if (((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2))) && !one_in_(888))
			{
#ifdef JP
				note = "は石化に抵抗した！";
#else
				note = " resists stoning!";
#endif
			}
			else if (r_ptr->flagsr & (RFR_RES_ACID | RFR_RES_SHAR))
			{
				if (!MON_STONING(m_ptr))
				{
#ifdef JP
					note = "が石化し始めた！";
#else
					note = " gets stoning!";
#endif
					(void)set_monster_stoning(c_ptr->m_idx, 1);
				}
			}
			else
			{
#ifdef JP
				note_dies = "は石像になった。";
#else
				note_dies = " turns into a stone statue.";
#endif
				do_stone = TRUE;
			}
			break;
		}

		/* Drain Life (Type-3: Drain HP & Mana) */
		case GF_DUAL_DRAIN:
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
			else
			{
				do_time = (dam+7)/8;
				dual_drain = TRUE;
			}

			break;
		}

		/* Capture monster */
		case GF_CAPTURE:
		{
			if (is_pet(m_ptr) && !m_ptr->parent_m_idx &&
				!(r_ptr->flags1 & (RF1_UNIQUE)) && !(r_ptr->flags7 & (RF7_NAZGUL)) && !(r_ptr->flags7 & (RF7_UNIQUE2)))
			{
#ifdef JP
				msg_format("%sをカードにした！",m_name);
#else
				msg_format("You capture %^s!", m_name);
#endif
				cap_mon = m_ptr->r_idx;
				cap_mspeed = m_ptr->mspeed;
				cap_hp = m_ptr->hp;
				cap_maxhp = m_ptr->max_maxhp;
				cap_nickname = m_ptr->nickname; /* Quark transfer */
				if (c_ptr->m_idx == p_ptr->riding)
				{
					if (rakuba(-1, FALSE))
					{
#ifdef JP
						msg_print("地面に落とされた。");
#else
						msg_format("You have fallen from %s.", m_name);
#endif
					}
				}

				delete_monster_idx(c_ptr->m_idx);

				return (TRUE);
			}
			else
			{
#ifdef JP
				msg_format("%sには効果がなかった。",m_name);
#else
				msg_format("%^s is unaffected.", m_name);
#endif
				skipped = TRUE;
			}
			break;
		}

		/* Drain Life (Type-4: Drain HP & Mana) */
		case GF_DRAIN_SOUL:
		{
			if (seen) obvious = TRUE;

			if ((r_ptr->flags3 & RF3_NONLIVING) || (r_ptr->flags2 & RF2_EMPTY_MIND))
			{
					if ((r_ptr->flags2 & (RF2_EMPTY_MIND)) && seen) r_ptr->r_flags2 |= (RF2_EMPTY_MIND);
					if ((r_ptr->flags3 & (RF3_DEMON)) && seen) r_ptr->r_flags3 |= (RF3_NONLIVING);

#ifdef JP
				note = "には効果がなかった！";
#else
				note = " is unaffected!";
#endif

				obvious = FALSE;
				dam = 0;
			}
			else if (r_ptr->flags2 & RF2_WEIRD_MIND)
			{
				if (seen) r_ptr->r_flags2 |= (RF2_WEIRD_MIND);

#ifdef JP
				note = "には耐性がある。";
#else
				note = " resists.";
#endif

				obvious = FALSE;
				dam /= 2;
			}
			else
			{
				/* Message */
#ifdef JP
 				msg_format("%sからエネルギーを吸いとった。",m_name);
#else
				msg_format("You draw energy from %s.", m_name);
#endif
				p_ptr->chp += dam / 5;
				p_ptr->csp += dam / 5;
				if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
				if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
				p_ptr->redraw |= (PR_HP | PR_MANA);
			}

			break;
		}

		/* Default */
		default:
		{
			/* Irrelevant */
			skipped = TRUE;

			/* No damage */
			dam = 0;

			break;
		}
	}


	/* Absolutely no effect */
	if (skipped) return (FALSE);

	/* "Unique" monsters cannot be polymorphed */
	if (r_ptr->flags1 & (RF1_UNIQUE)) do_poly = FALSE;

	/* Quest monsters cannot be polymorphed */
	if (r_ptr->flags1 & RF1_QUESTOR) do_poly = FALSE;

	if (p_ptr->riding & (c_ptr->m_idx == p_ptr->riding)) do_poly = FALSE;

	/* "Unique" and "quest" monsters can only be "killed" by the player. */
	if (((r_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (r_ptr->flags7 & RF7_NAZGUL)) && who)
	{
		if (dam > m_ptr->hp) dam = m_ptr->hp;
		if (do_stone) do_stone = FALSE;
	}

	/* Modify the damage */
	tmp = dam;
	dam = mon_damage_mod(m_ptr, dam,
		(bool)((typ == GF_GODLY_SPEAR) || (typ == GF_PURE_FIRE) || (typ == GF_PURE_AQUA) || (typ == GF_PURE_EARTH) || (typ == GF_PURE_WIND)));
#ifdef JP
	if ((tmp > 0) && (dam == 0)) note = "はダメージを受けていない。";
#else
	if ((tmp > 0) && (dam == 0)) note = " is unharmed.";
#endif

	/* Check for death */
	if ((dam > m_ptr->hp) || do_stone)
	{
		/* Extract method of death */
		note = note_dies;
	}

	/* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
	else if (do_poly && (randint1(90) > r_ptr->level))
	{
		if (polymorph_monster(y, x, !who))
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

		/* Hack -- Get new monster */
		m_ptr = &m_list[c_ptr->m_idx];

		/* Hack -- Get new race */
		r_ptr = &r_info[m_ptr->r_idx];
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

	/* Sound and Impact resisters never stun */
	else if (do_stun &&
	    !(r_ptr->flagsr & (RFR_RES_SOUN)) &&
	    !(r_ptr->flagsr & (RFR_RES_WALL)) &&
	    !(r_ptr->flags3 & (RF3_NO_STUN)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Get confused */
		if (MON_STUNNED(m_ptr))
		{
#ifdef JP
			note = "はひどくもうろうとした。";
#else
			note = " is more dazed.";
#endif

			tmp = MON_STUNNED(m_ptr) + (do_stun / 2);
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
		(void)set_monster_stunned(c_ptr->m_idx, tmp);

		/* Get angry */
		get_angry = TRUE;
	}

	/* Confusion and Chaos resisters (and sleepers) never confuse */
	else if (do_conf &&
		 !(r_ptr->flags3 & (RF3_NO_CONF)) &&
		 !(r_ptr->flagsr & (RFR_RES_CONF)) &&
		 !(r_ptr->flagsr & (RFR_RES_CHAO)))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Already partially confused */
		if (MON_CONFUSED(m_ptr))
		{
#ifdef JP
			note = "はさらに混乱したようだ。";
#else
			note = " looks more confused.";
#endif

			tmp = MON_CONFUSED(m_ptr) + (do_conf / 2);
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
		(void)set_monster_confused(c_ptr->m_idx, tmp);

		/* Get angry */
		get_angry = TRUE;
	}
	else if (do_time)
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		if (do_time >= m_ptr->maxhp) do_time = m_ptr->maxhp-1;

		if (do_time)
		{
#ifdef JP
			note = "は弱くなったようだ。";
#else
			note = " seems weakened.";
#endif
			m_ptr->maxhp -= do_time;
			if ((m_ptr->hp - dam) > m_ptr->maxhp) dam = m_ptr->hp-m_ptr->maxhp;
		}
		get_angry = TRUE;
	}


	/* Fear */
	if (do_fear)
	{
		/* Set fear */
		(void)set_monster_monfear(c_ptr->m_idx, MON_MONFEAR(m_ptr) + do_fear);

		/* Get angry */
		get_angry = TRUE;
	}

	/* HP drain */
	if (do_drain)
	{
		if (who > 0)
		{
			char tmp_m_name[80];
			monster_type *tmp_m_ptr = &m_list[who];

			/* Get the monster name (BEFORE polymorphing) */
			monster_desc(tmp_m_name, tmp_m_ptr, 0);

			tmp_m_ptr->hp += MIN(dam, m_ptr->hp);

			if (tmp_m_ptr->hp >= tmp_m_ptr->maxhp)
			{
				/* Fully healed */
				tmp_m_ptr->hp = tmp_m_ptr->maxhp;

				/* Message */
				if (tmp_m_ptr->ml)
				{
#ifdef JP
					msg_format("%^sは完全に治った！", tmp_m_name);
#else
					msg_format("%^s looks completely healed!", tmp_m_name);
#endif
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (tmp_m_ptr->ml)
				{
#ifdef JP
					msg_format("%^sは体力を回復したようだ。", tmp_m_name);
#else
					msg_format("%^s looks healthier.", tmp_m_name);
#endif
				}
			}
		}
		else if (!who)
		{
			hp_player(MIN(dam, m_ptr->hp));
		}
	}

	/* HP & Mana Drain */
	if (dual_drain)
	{
		if (who > 0)
		{
			char tmp_m_name[80];
			monster_type *tmp_m_ptr = &m_list[who];

			/* Get the monster name (BEFORE polymorphing) */
			monster_desc(tmp_m_name, tmp_m_ptr, 0);

			tmp_m_ptr->hp += MIN(dam, m_ptr->hp) / 4;

			if (tmp_m_ptr->hp >= tmp_m_ptr->maxhp)
			{
				/* Fully healed */
				tmp_m_ptr->hp = tmp_m_ptr->maxhp;

				/* Message */
				if (tmp_m_ptr->ml)
				{
#ifdef JP
					msg_format("%^sは完全に治った！", tmp_m_name);
#else
					msg_format("%^s looks completely healed!", tmp_m_name);
#endif
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (tmp_m_ptr->ml)
				{
#ifdef JP
					msg_format("%^sは体力を回復したようだ。", tmp_m_name);
#else
					msg_format("%^s looks healthier.", tmp_m_name);
#endif
				}
			}
		}
		else if (!who)
		{
			int dual_drain_val = MIN(dam, m_ptr->hp) / 4;

			hp_player(dual_drain_val);
			p_ptr->csp += dual_drain_val;
			if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
			p_ptr->redraw |= (PR_MANA);
		}
	}

	/* If another monster did the damage, hurt the monster by hand */
	if (who)
	{
		/* Redraw (later) if needed */
		if (p_ptr->health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);
		if (p_ptr->riding == c_ptr->m_idx) p_ptr->redraw |= (PR_UHEALTH);

		/* Wake the monster up */
		(void)set_monster_csleep(c_ptr->m_idx, 0);

		/* Hurt the monster */
		m_ptr->hp -= dam;
		if (show_damage && seen && (dam > 0))
#ifdef JP
			msg_format("%^sに%dのダメージ。", m_name, dam);
#else
			msg_format("%^s takes %d damages.", m_name, dam);
#endif

		/* Dead monster */
		if ((m_ptr->hp < 0) || do_stone)
		{
			bool sad = FALSE;

			if (is_pet(m_ptr) && !(m_ptr->ml))
				sad = TRUE;

			/* Make a sound */
			if (r_ptr->flags1 & RF1_MALE)
			{
				sound(SOUND_M_KILL);
			}
			else if (r_ptr->flags1 & RF1_MALE)
			{
				sound(SOUND_F_KILL);
			}
			else if (monster_living(r_ptr))
			{
				sound(SOUND_KILL);
			}
			else
			{
				sound(SOUND_N_KILL);
			}

			/* Give detailed messages if destroyed */
			if (known && note)
			{
				monster_desc(m_name, m_ptr, 0);
				if (see_s)
				{
					msg_format("%^s%s", m_name, note);
				}
				else
				{
					mon_fight = TRUE;
				}
			}

			monster_gain_exp(who, c_ptr->m_idx);

			/* Generate treasure, etc */
			monster_death(c_ptr->m_idx, FALSE, do_stone);

			/* Delete the monster */
			delete_monster_idx(c_ptr->m_idx);

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
			if (note && seen) msg_format("%^s%s", m_name, note);

			/* Hack -- Pain message */
			else if (see_s)
			{
				message_pain(c_ptr->m_idx, dam);
			}
			else
			{
				mon_fight = TRUE;
			}

			/* Hack -- handle sleep */
			if (do_sleep) (void)set_monster_csleep(c_ptr->m_idx, do_sleep);
		}
	}

	else if (heal_leper)
	{
#ifdef JP
		msg_print("不潔な病人は病気が治った！");
#else
		msg_print("The Mangy looking leper is healed!");
#endif

		/* Check for quest completion */
		check_quest_completion(m_ptr);

		if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
		{
			char m2_name[80];

			monster_desc(m2_name, m_ptr, MD_INDEF_VISIBLE);
			do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_HEAL_LEPER, m2_name);
		}

		delete_monster_idx(c_ptr->m_idx);
	}
	/* If the player did it, give him experience, check fear */
	else if (typ != GF_DRAIN_MANA)
	{
		bool fear = FALSE;

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(c_ptr->m_idx, dam, &fear, note_dies, do_stone))
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
#ifdef JP
				msg_format("%s%s", m_name, note);
#else
				msg_format("%^s%s", m_name, note);
#endif


			/* Hack -- Pain message */
			else if (known && (dam || !do_fear))
			{
				message_pain(c_ptr->m_idx, dam);
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
			if (do_sleep) (void)set_monster_csleep(c_ptr->m_idx, do_sleep);
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

	if ((dam > 0) && !is_pet(m_ptr) && !is_friendly(m_ptr))
	{
		if (!who)
		{
			if (!projectable(m_ptr->fy, m_ptr->fx, py, px) && !(flg & PROJECT_NO_HANGEKI))
			{
				set_target(m_ptr, monster_target_y, monster_target_x);
			}
		}
		else if (is_pet(&m_list[who]) && (m_ptr->target_y != py) && (m_ptr->target_x != px))
		{
			set_target(m_ptr, m_list[who].fy, m_list[who].fx);
		}
	}

	if (p_ptr->riding && (p_ptr->riding == c_ptr->m_idx) && (dam > 0))
	{
		if (m_ptr->hp > m_ptr->maxhp/3) dam = (dam + 1) / 2;
		rakubadam_m = (dam > 200) ? 200 : dam;
	}


	if (photo)
	{
		object_type *q_ptr;
		object_type forge;

		/* Get local object */
		q_ptr = &forge;

		/* Prepare to make a Blade of Chaos */
		object_prep(q_ptr, lookup_kind(TV_STATUE, SV_PHOTO));

		q_ptr->pval = photo;

		/* Mark the item as fully known */
		q_ptr->ident |= (IDENT_MENTAL);

		/* Drop it in the dungeon */
		(void)drop_near(q_ptr, -1, py, px);
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
static bool project_p(int who, cptr who_name, int r, int y, int x, int dam, int typ, u32b flg, int mod_elem_mode)
{
	int k = 0;
	int rlev = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = FALSE;

	/* Source monster */
	monster_type *m_ptr = NULL;

	/* Monster name (for attacks) */
	char m_name[80];

	/* Monster name (for damage) */
	char killer[80];

	/* Hack -- messages */
	cptr act = NULL;

	int get_damage = 0;

	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];


	/* Player is not here */
	if ((x != px) || (y != py)) return (FALSE);

	/* Player cannot hurt himself */
	if (!who) return (FALSE);
	if (who == p_ptr->riding) return (FALSE);

	if ((flg & PROJECT_AVOIDABLE) && (typ != GF_ROCKET)) return FALSE;

	if (p_ptr->reflect && (flg & PROJECT_REFLECTABLE) && !one_in_(10))
	{
		byte t_y, t_x;
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

		project(0, 0, t_y, t_x, dam, typ, (PROJECT_STOP | PROJECT_THRU | PROJECT_KILL | PROJECT_REFLECTABLE), mod_elem_mode);

		disturb(1, 0);
		return TRUE;
	}

	/* Reduce damage by distance */
	if (!(flg & PROJECT_NO_REDUCE)) dam = (dam + r) / (r + 1);

	dam = modify_dam_by_elem(who, 0, dam, typ, mod_elem_mode);

	/* If the player is blind, be more descriptive */
	if (blind) fuzzy = TRUE;


	if (who > 0)
	{
		/* Get the source monster */
		m_ptr = &m_list[who];

		/* Extract the monster level */
		rlev = ((r_info[m_ptr->r_idx].level >= 1) ? r_info[m_ptr->r_idx].level : 1);

		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Get the monster's real name (gotten before polymorph!) */
		strcpy(killer, who_name);
	}
	else if (who < 0)
	{
#ifdef JP
		strcpy(killer, "罠");
#else
		strcpy(killer, "a trap");
#endif
	}

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

			if (p_ptr->weak_earth) dam *= 4 / 3;

			get_damage = acid_dam(dam, killer);
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

			if (p_ptr->weak_wind) dam *= 4 / 3;

			get_damage = elec_dam(dam, killer);
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

			if (p_ptr->weak_fire) dam *= 4 / 3;

			if (prace_is_(RACE_PUMPKINHEAD))
			{
				dam = dam * 4 / 3;
			}
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

			if (p_ptr->weak_aqua) dam *= 4 / 3;

			get_damage = cold_dam(dam, killer);
			break;
		}

		/* Standard damage -- also poisons player */
		case GF_POIS:
		{
			bool double_resist = p_ptr->oppose_pois;
#ifdef JP
			if (fuzzy) msg_print("毒で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by poison!");
#endif

			if (p_ptr->resist_pois) dam = (dam + 2) / 3;
			if (double_resist) dam = (dam + 2) / 3;

			ACTIVATE_MULTISHADOW();
			if ((!(double_resist || p_ptr->resist_pois)) &&
			     one_in_(HURT_CHANCE) && !IS_MULTISHADOW(0))
			{
				do_dec_stat(A_CON);
			}

			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);

			if (!(double_resist || p_ptr->resist_pois) && !IS_MULTISHADOW(0))
			{
				set_poisoned(p_ptr->poisoned + randint0(dam) + 10);
			}
			STOP_MULTISHADOW();
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
				dam *= 4; dam /= (randint1(4) + 7);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint1(5) + 2);
			}
			if (p_ptr->hurt_lite)
			{
				dam = dam * 4 / 3;
			}
			ACTIVATE_MULTISHADOW();
			if (p_ptr->ogre_equip)
			{
#ifdef JP
				if (!IS_MULTISHADOW(0)) msg_print("ひどい痛手を受けた！");
#else
				if (!IS_MULTISHADOW(0)) msg_print("You are hit hard!");
#endif

				dam *= 2;
			}
			if (p_ptr->wraith_form && !p_ptr->wraith_form_perm) dam *= 2;
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);

			if (p_ptr->wraith_form && !p_ptr->wraith_form_perm && !IS_MULTISHADOW(0))
			{
				p_ptr->wraith_form = 0;
#ifdef JP
				msg_print("閃光のため非物質的な影の存在でいられなくなった。");
#else
				msg_print("The light forces you out of your incorporeal shadow form.");
#endif

				p_ptr->redraw |= PR_MAP;
				/* Update monsters */
				p_ptr->update |= (PU_MONSTERS);
				/* Window stuff */
				p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

				/* Redraw status bar */
				p_ptr->redraw |= (PR_STATUS);
			}
			STOP_MULTISHADOW();

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

			if (WRAITH_FORM() || p_ptr->evil_equip || (p_ptr->pclass == CLASS_VAMPIRE)) break;

			if (p_ptr->resist_dark)
			{
				dam *= 4; dam /= (randint1(4) + 7);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint1(5) + 2);
			}
			if (prace_is_(RACE_FAIRY))
			{
				dam = dam * 4 / 3;
			}
			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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


			ACTIVATE_MULTISHADOW();
			if ((prace_is_(RACE_GHOST)) || (prace_is_(RACE_SKELETON)))
			{
				/* Later */
			}
			else if (p_ptr->evil_equip)
			{
				STOP_MULTISHADOW();
				break;
			}
			else if (p_ptr->resist_neth)
			{
				dam *= 6; dam /= (randint1(4) + 7);
			}
			else if (!IS_MULTISHADOW(0)) drain_exp(200 + (p_ptr->exp / 100), 200 + (cexp_ptr->cexp / 100), 200 + (p_ptr->exp / 1000), 200 + (cexp_ptr->cexp / 1000), 75);

			if (prace_is_(RACE_GHOST) && !IS_MULTISHADOW(0))
			{
#ifdef JP
				msg_print("気分がよくなった。");
#else
				msg_print("You feel invigorated!");
#endif

				hp_player(dam / 4);
			}
			else if (prace_is_(RACE_SKELETON) && !IS_MULTISHADOW(0))
			{
#ifdef JP
				msg_print("しかし効果がなかった！");
#else
				msg_print("You are unaffected!");
#endif
			}
			else
			{
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			}
			STOP_MULTISHADOW();

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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->resist_water)
			{
				dam *= 4;
				dam /= (randint1(4) + 7);
			}
			else if (!IS_MULTISHADOW(0))
			{
				if (!p_ptr->resist_sound)
				{
					set_stun(p_ptr->stun + randint1(40));
				}
				if (!p_ptr->resist_conf)
				{
					set_confused(p_ptr->confused + randint1(5) + 5);
				}

				if (one_in_(5))
				{
					inven_damage(set_cold_destroy, 3);
				}
			}

			if (p_ptr->zoshonel_protect) dam = dam * 3 / 2;
			if (p_ptr->weak_aqua) dam *= 4 / 3;
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			if (p_ptr->zoshonel_protect) break;

			ACTIVATE_MULTISHADOW();
			if (p_ptr->weak_aqua) dam *= 4; dam /= (randint1(4) + 7);
			if (p_ptr->weak_fire) dam *= 4 / 3;
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);

			if (!p_ptr->resist_sound && !IS_MULTISHADOW(0))
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
			STOP_MULTISHADOW();

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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->weak_earth) dam *= 4 / 3;
			if (p_ptr->resist_shard)
			{
				dam *= 6; dam /= (randint1(4) + 7);
			}
			else if (!IS_MULTISHADOW(0))
			{
				(void)set_cut(p_ptr->cut + dam);
			}

			if (!p_ptr->resist_shard || one_in_(13))
			{
				inven_damage(set_cold_destroy, 2);
			}

			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->weak_wind) dam *= 4 / 3;
			if (p_ptr->resist_sound)
			{
				dam *= 5; dam /= (randint1(4) + 7);
			}
			else if (!IS_MULTISHADOW(0))
			{
				int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);
			}

			if (!p_ptr->resist_sound || one_in_(13))
			{
				inven_damage(set_cold_destroy, 2);
			}

			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->resist_conf)
			{
				dam *= 5; dam /= (randint1(4) + 7);
			}
			else if (!IS_MULTISHADOW(0))
			{
				(void)set_confused(p_ptr->confused + randint1(20) + 10);
			}
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->resist_chaos)
			{
				dam *= 6; dam /= (randint1(4) + 7);
			}
			if (!IS_MULTISHADOW(0))
			{
				if (!p_ptr->resist_conf)
				{
					(void)set_confused(p_ptr->confused + randint0(20) + 10);
				}
				if (!p_ptr->resist_chaos)
				{
					(void)set_image(p_ptr->image + randint1(10));
					if (one_in_(3))
					{
#ifdef JP
						msg_print("あなたの身体はカオスの力で捻じ曲げられた！");
#else
						msg_print("Your body is twisted by chaos!");
#endif

						(void)gain_random_mutation(0, TRUE);
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

						lose_class_exp(500 + (cexp_ptr->cexp / 1000) * MON_DRAIN_LIFE);
						lose_racial_exp(500 + (p_ptr->exp / 1000) * MON_DRAIN_LIFE);
					}
					else
					{
#ifdef JP
						msg_print("生命力が体から吸い取られた気がする！");
#else
						msg_print("You feel your life draining away!");
#endif

						lose_class_exp(5000 + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE);
						lose_racial_exp(5000 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
					}
				}
			}
			if (!p_ptr->resist_chaos || one_in_(9))
			{
				inven_damage(set_elec_destroy, 2);
				inven_damage(set_fire_destroy, 2);
			}
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

		/* Stone -- see above */
		case GF_STONE:
		{
#ifdef JP
			if (fuzzy) msg_print("何か乾いたもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something dry!");
#endif

			ACTIVATE_MULTISHADOW();
			if (p_ptr->weak_earth) dam *= 4 / 3;
			if (p_ptr->resist_stone)
			{
				dam *= 6; dam /= (randint1(4) + 7);
			}
			else if (IS_MULTISHADOW(0))
			{
				/* Nothing */
			}
			else if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし石化を跳ね返した！");
#else
				msg_print("You resist stoning!");
#endif
			}
			else if (p_ptr->resist_acid || p_ptr->oppose_acid || p_ptr->immune_acid || p_ptr->resist_shard)
			{
				if (!p_ptr->stoning) set_stoning(1);
			}
			else
			{
#ifdef JP
				msg_print("生きたまま石像になっていく...");
#else
				msg_print("You are into a living statue...");
#endif
				p_ptr->is_dead |= DEATH_STONED;
			}
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->resist_disen)
			{
				dam *= 6; dam /= (randint1(4) + 7);
			}
			else if (!IS_MULTISHADOW(0))
			{
				(void)apply_disenchant();
			}
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (!p_ptr->resist_sound && !IS_MULTISHADOW(0))
			{
				(void)set_stun(p_ptr->stun + randint1(20));
			}
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (!IS_MULTISHADOW(0)) (void)set_slow(p_ptr->slow + randint0(4) + 4, FALSE);
			if (p_ptr->weak_wind) dam *= 4 / 3;
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->resist_time)
			{
				dam *= 4;
				dam /= (randint1(4) + 7);
#ifdef JP
				msg_print("時間が通り過ぎていく気がする。");
#else
				msg_print("You feel as if time is passing you by.");
#endif

			}
			else if (!IS_MULTISHADOW(0))
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

						lose_class_exp(100 + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE);
						lose_racial_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
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


						for (k = 0; k < A_MAX; k++)
						{
							p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 7) / 8;
							if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
						}
						p_ptr->update |= (PU_BONUS);
						break;
					}
				}
			}

			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (!IS_MULTISHADOW(0))
			{
				if (!(p_ptr->earth_spike || p_ptr->weak_earth)) teleport_player(5);
				if (!(p_ptr->ffall || p_ptr->weak_earth))
					(void)set_slow(p_ptr->slow + randint0(4) + 4, FALSE);
				if (!(p_ptr->resist_sound || p_ptr->weak_earth || p_ptr->ffall))
				{
					int k = (randint1((dam > 90) ? 35 : (dam / 3 + 5)));
					(void)set_stun(p_ptr->stun + k);
				}
			}
			if (p_ptr->ffall || p_ptr->weak_earth)
			{
				dam = (dam * 2) / 3;
			}

			if (!p_ptr->ffall || one_in_(13))
			{
				inven_damage(set_cold_destroy, 2);
			}
			if (p_ptr->weak_wind) dam *= 4 / 3;

			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			if (p_ptr->weak_aqua) dam *= 4; dam /= (randint1(4) + 7);
			if (p_ptr->weak_fire) dam *= 4 / 3;
			get_damage = cold_dam(dam, killer);
			if (!IS_MULTISHADOW(1))
			{
				if (!p_ptr->resist_shard)
				{
					(void)set_cut(p_ptr->cut + damroll(5, 8));
				}
				if (!p_ptr->resist_sound)
				{
					(void)set_stun(p_ptr->stun + randint1(15));
				}

				if (!(p_ptr->resist_cold || p_ptr->oppose_cold) || one_in_(12))
				{
					if (!p_ptr->immune_cold) inven_damage(set_cold_destroy, 3);
				}
			}

			break;
		}

		/* Standard damage -- also poisons / mutates player */
		case GF_NUKE:
		{
			bool double_resist = p_ptr->oppose_pois;
#ifdef JP
			if (fuzzy) msg_print("放射能で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by radiation!");
#endif

			if (p_ptr->resist_pois) dam = (2 * dam + 2) / 5;
			if (double_resist) dam = (2 * dam + 2) / 5;
			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			if (!(double_resist || p_ptr->resist_pois) && !IS_MULTISHADOW(0))
			{
				set_poisoned(p_ptr->poisoned + randint0(dam) + 10);

				if (one_in_(5)) /* 6 */
				{
#ifdef JP
					msg_print("奇形的な変身を遂げた！");
#else
					msg_print("You undergo a freakish metamorphosis!");
#endif

					if (one_in_(4)) /* 4 */
						do_poly_self();
					else
						mutate_player();
				}

				if (one_in_(6))
				{
					inven_damage(set_acid_destroy, 2);
				}
			}
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (!IS_MULTISHADOW(0))
			{
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
					(void)set_cut(p_ptr->cut + ( dam / 2));
				}
			}

			if (!p_ptr->resist_shard || one_in_(12))
			{
				inven_damage(set_cold_destroy, 3);
			}
			if (p_ptr->weak_earth) dam *= 4 / 3;

			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

		/* Physical -- XXX no dodging */
		case GF_PHYSICAL:
		{
			if (test_hit_fire(100 + rlev * 4, p_ptr->ac + p_ptr->to_a, TRUE))
			{
#ifdef JP
				if (fuzzy) msg_print("何かで攻撃された！");
#else
				if (fuzzy) msg_print("You are hit by something!");
#endif

				ACTIVATE_MULTISHADOW();
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}
			else
			{
#ifdef JP
				if (fuzzy) msg_print("何かをかわしたようだ。");
				else msg_print("攻撃をかわした。");
#else
				if (fuzzy) msg_print("Something misses you.");
				else msg_print("Attack misses you.");
#endif

			}
			break;
		}

		/* Blunt Weapon -- XXX no dodging */
		case GF_BLUNT:
		{
			if (test_hit_fire(100 + rlev * 4, p_ptr->ac + p_ptr->to_a, TRUE))
			{
#ifdef JP
				if (fuzzy) msg_print("何か鈍いもので攻撃された！");
#else
				if (fuzzy) msg_print("You are hit by something blunt!");
#endif

				ACTIVATE_MULTISHADOW();
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}
			else
			{
#ifdef JP
				if (fuzzy) msg_print("何か鈍いものをかわしたようだ。");
				else msg_print("攻撃をかわした。");
#else
				if (fuzzy) msg_print("Something blunt misses you.");
				else msg_print("Attack misses you.");
#endif

			}
			break;
		}

		/* Edged Weapon -- XXX no dodging */
		case GF_EDGED:
		{
			if (test_hit_fire(100 + rlev * 4, p_ptr->ac + p_ptr->to_a, TRUE))
			{
#ifdef JP
				if (fuzzy) msg_print("何か鋭いもので攻撃された！");
#else
				if (fuzzy) msg_print("You are hit by something sharp!");
#endif

				ACTIVATE_MULTISHADOW();
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}
			else
			{
#ifdef JP
				if (fuzzy) msg_print("何か鋭いものをかわしたようだ。");
				else msg_print("攻撃をかわした。");
#else
				if (fuzzy) msg_print("Something sharp misses you.");
				else msg_print("Attack misses you.");
#endif

			}
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

			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			ACTIVATE_MULTISHADOW();
			if (p_ptr->weak_wind) dam *= 4; dam /= (randint1(4) + 7);
			if (p_ptr->weak_earth) dam *= 4 / 3;
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
			if (!p_ptr->resist_shard || one_in_(13))
			{
				if (!p_ptr->immune_fire) inven_damage(set_fire_destroy, 2);
				inven_damage(set_cold_destroy, 2);
			}

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

			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
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

			if ((prace_is_(RACE_GHOST)) || (prace_is_(RACE_SKELETON)) || (get_your_alignment_gne() == ALIGN_GNE_EVIL))
				dam *= 2;
			ACTIVATE_MULTISHADOW();
			if (p_ptr->ogre_equip)
			{
#ifdef JP
				if (!IS_MULTISHADOW(0)) msg_print("ひどい痛手を受けた！");
#else
				if (!IS_MULTISHADOW(0)) msg_print("You are hit hard!");
#endif

				dam *= 3;
			}
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

		case GF_HELL_FIRE:
		{
#ifdef JP
			if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

			if ((prace_is_(RACE_GHOST)) || (prace_is_(RACE_SKELETON))) dam /= 2;
			else if (get_your_alignment_gne() == ALIGN_GNE_GOOD)
				dam *= 2;
			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

		/* Pure damage */
		case GF_GODLY_SPEAR:
		{
#ifdef JP
			if (fuzzy) msg_print("エネルギーの塊で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by an energy!");
#endif

			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_FORCE, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

#define HURT_CHANCE 16

		/* Pure damage */
		case GF_PURE_FIRE:
		{
#ifdef JP
			if (fuzzy) msg_print("純粋な火炎で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by pure fire!");
#endif

			if (!p_ptr->immune_fire)
			{
				bool double_resist = p_ptr->oppose_fire;

				if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;
				if ((!(double_resist || p_ptr->resist_fire)) && one_in_(HURT_CHANCE)) do_dec_stat(A_STR);
				if (!(double_resist && p_ptr->resist_fire)) inven_damage(set_fire_destroy, 3);
			}

			if (p_ptr->no_elem) dam /= 2;
			if (p_ptr->weak_aqua) dam = 0;
			if (p_ptr->weak_fire) dam *= 4 / 3;
			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_FORCE, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

		/* Pure damage */
		case GF_PURE_AQUA:
		{
#ifdef JP
			if (fuzzy) msg_print("純粋な水流で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by pure water flow!");
#endif

			if (!p_ptr->immune_cold)
			{
				bool double_resist = p_ptr->oppose_cold;

				if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;
				if ((!(double_resist || p_ptr->resist_cold)) && one_in_(HURT_CHANCE)) do_dec_stat(A_STR);
				if (!(double_resist && p_ptr->resist_cold)) inven_damage(set_cold_destroy, 3);
			}

			if (p_ptr->no_elem) dam /= 2;
			if (p_ptr->weak_fire) dam = 0;
			if (p_ptr->weak_aqua) dam *= 4 / 3;
			if (p_ptr->zoshonel_protect) dam = dam * 3 / 2;
			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_FORCE, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

		/* Pure damage */
		case GF_PURE_EARTH:
		{
#ifdef JP
			if (fuzzy) msg_print("純粋な大地の力で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by pure earth force!");
#endif

			if (!p_ptr->immune_acid)
			{
				bool double_resist = p_ptr->oppose_acid;

				if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;
				if ((!(double_resist || p_ptr->resist_acid)) && one_in_(HURT_CHANCE)) do_dec_stat(A_CHR);
				if (!(double_resist && p_ptr->resist_acid)) inven_damage(set_acid_destroy, 3);
			}

			if (p_ptr->no_elem) dam /= 2;
			if (p_ptr->weak_wind) dam = 0;
			if (p_ptr->weak_earth) dam *= 4 / 3;
			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_FORCE, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

		/* Pure damage */
		case GF_PURE_WIND:
		{
#ifdef JP
			if (fuzzy) msg_print("純粋な風で攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by pure wind!");
#endif

			if (!p_ptr->immune_elec)
			{
				bool double_resist = p_ptr->oppose_elec;

				if (p_ptr->muta3 & MUT3_VULN_ELEM) dam *= 2;
				if ((!(double_resist || p_ptr->resist_elec)) && one_in_(HURT_CHANCE)) do_dec_stat(A_DEX);
				if (!(double_resist && p_ptr->resist_elec)) inven_damage(set_elec_destroy, 3);
			}

			if (p_ptr->no_elem) dam /= 2;
			if (p_ptr->weak_earth) dam = 0;
			if (p_ptr->weak_wind) dam *= 4 / 3;
			ACTIVATE_MULTISHADOW();
			get_damage = take_hit(DAMAGE_FORCE, dam, killer);
			STOP_MULTISHADOW();
			break;
		}

#undef HURT_CHANCE

		/* Cause 1 */
		case GF_CAUSE_1:
		{
			if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			}
			else
			{
				ACTIVATE_MULTISHADOW();
				if (!IS_MULTISHADOW(0)) curse_equipment(15, 0);
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}
			break;
		}

		/* Cause 2 */
		case GF_CAUSE_2:
		{
			if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			}
			else
			{
				ACTIVATE_MULTISHADOW();
				if (!IS_MULTISHADOW(0)) curse_equipment(25, MIN(rlev/2-15, 5));
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}
			break;
		}

		/* Cause 3 */
		case GF_CAUSE_3:
		{
			if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			}
			else
			{
				ACTIVATE_MULTISHADOW();
				if (!IS_MULTISHADOW(0)) curse_equipment(33, MIN(rlev/2-15, 15));
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}
			break;
		}

		/* Cause 4 */
		case GF_CAUSE_4:
		{
			if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			}
			else
			{
				ACTIVATE_MULTISHADOW();
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				if (!IS_MULTISHADOW(0)) (void)set_cut(p_ptr->cut + damroll(10, 10));
				STOP_MULTISHADOW();
			}
			break;
		}

		/* Hand of Doom (Use "dam" as "power") */
		case GF_HAND_DOOM:
		{
			if (dam < p_ptr->skill_sav)
			{
#ifdef JP
				msg_format("しかし効力を跳ね返した！");
#else
				msg_format("You resist the effects!");
#endif
			}
			else
			{
#ifdef JP
				msg_print("あなたは命が薄まっていくように感じた！");
#else
				msg_print("You feel your life fade away!");
#endif

				dam = ((40 + randint1(20)) * p_ptr->chp) / 100;
				ACTIVATE_MULTISHADOW();
				get_damage = take_hit(DAMAGE_ATTACK, dam, m_name);
				if (!IS_MULTISHADOW(0)) curse_equipment(40, 20);
				STOP_MULTISHADOW();

				if (p_ptr->chp < 1) p_ptr->chp = 1;
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


			/* Some races are immune */
			if ((rp_ptr->r_flags & PRF_UNDEAD) || (cp_ptr->c_flags & PCF_UNDEAD))
			{
				dam = 0;
			}

			/* Hurt a lot */
			else
			{
				ACTIVATE_MULTISHADOW();
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}

			break;
		}

		/* Mind blast */
		case GF_MIND_BLAST:
		{
			if (randint0(100 + rlev/2) < (MAX(5, p_ptr->skill_sav)))
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			}
			else
			{
#ifdef JP
				msg_print("霊的エネルギーで精神が攻撃された。");
#else
				msg_print("Your mind is blasted by psyonic energy.");
#endif

				ACTIVATE_MULTISHADOW();
				if (!IS_MULTISHADOW(0))
				{
					if (!p_ptr->resist_conf)
					{
						(void)set_confused(p_ptr->confused + randint0(4) + 4);
					}

					if (!p_ptr->resist_chaos && one_in_(3))
					{
						(void)set_image(p_ptr->image + randint0(250) + 150);
					}

					p_ptr->csp -= 50;
					if (p_ptr->csp < 0)
					{
						p_ptr->csp = 0;
						p_ptr->csp_frac = 0;
					}
					p_ptr->redraw |= PR_MANA;
				}

				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}
			break;
		}

		/* Brain smash */
		case GF_BRAIN_SMASH:
		{
			if (randint0(100 + rlev/2) < (MAX(5, p_ptr->skill_sav)))
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			}
			else
			{
#ifdef JP
				msg_print("霊的エネルギーで精神が攻撃された。");
#else
				msg_print("Your mind is blasted by psionic energy.");
#endif

				ACTIVATE_MULTISHADOW();
				if (!IS_MULTISHADOW(0))
				{
					p_ptr->csp -= 100;
					if (p_ptr->csp < 0)
					{
						p_ptr->csp = 0;
						p_ptr->csp_frac = 0;
					}
					p_ptr->redraw |= PR_MANA;
				}

				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				if (!IS_MULTISHADOW(0))
				{
					if (!p_ptr->resist_blind)
					{
						(void)set_blind(p_ptr->blind + 8 + randint0(8));
					}
					if (!p_ptr->resist_conf)
					{
						(void)set_confused(p_ptr->confused + randint0(4) + 4);
					}
					if (!p_ptr->free_act)
					{
						(void)set_paralyzed(p_ptr->paralyzed + randint0(4) + 4);
					}
					(void)set_slow(p_ptr->slow + randint0(4) + 4, FALSE);

					while (randint0(100 + rlev/2) > (MAX(5, p_ptr->skill_sav)))
						(void)do_dec_stat(A_INT);
					while (randint0(100 + rlev/2) > (MAX(5, p_ptr->skill_sav)))
						(void)do_dec_stat(A_WIS);

					if (!p_ptr->resist_chaos)
					{
						(void)set_image(p_ptr->image + randint0(250) + 150);
					}
				}
				STOP_MULTISHADOW();
			}
			break;
		}

		case GF_OLD_HEAL:
		{
#ifdef JP
			if (fuzzy) msg_print("何らかの攻撃によって気分がよくなった。");
#else
			if (fuzzy) msg_print("You are hit by something vigorating!");
#endif

			(void)hp_player(dam);
			dam = 0;
			break;
		}

		case GF_OLD_SPEED:
		{
#ifdef JP
			if (fuzzy) msg_print("何かで攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something!");
#endif

			(void)set_fast(p_ptr->fast + randint1(5), FALSE);
			dam = 0;
			break;
		}

		case GF_OLD_SLOW:
		case GF_NEW_SLOW:
		{
#ifdef JP
			if (fuzzy) msg_print("何か遅いもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something slow!");
#endif

			(void)set_slow(p_ptr->slow + randint0(4) + 4, FALSE);
			break;
		}

		case GF_OLD_SLEEP:
		{
			if (p_ptr->free_act)  break;
#ifdef JP
			if (fuzzy) msg_print("眠ってしまった！");
#else
			if (fuzzy) msg_print("You fall asleep!");
#endif


			set_paralyzed(p_ptr->paralyzed + dam);
			dam = 0;
			break;
		}

		/* Drain Life */
		case GF_OLD_DRAIN:
		{
#ifdef JP
			if (fuzzy) msg_print("生命力を奪うもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something draining life!");
#endif


			/* Some races are immune */
			if ((rp_ptr->r_flags & PRF_DEMON) || (cp_ptr->c_flags & PCF_DEMON))
			{
				dam = 0;
			}

			if ((rp_ptr->r_flags & PRF_UNDEAD) || (cp_ptr->c_flags & PCF_UNDEAD))
			{
				dam = 0;
			}

			/* Hurt a lot */
			else
			{
				ACTIVATE_MULTISHADOW();
				if (!IS_MULTISHADOW(0))
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

						lose_class_exp(200 + (cexp_ptr->cexp / 1000) * MON_DRAIN_LIFE);
						lose_racial_exp(200 + (p_ptr->exp / 1000) * MON_DRAIN_LIFE);
					}
					else
					{
#ifdef JP
						msg_print("生命力が体から吸い取られた気がする！");
#else
						msg_print("You feel your life draining away!");
#endif

						lose_class_exp(200 + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE);
						lose_racial_exp(200 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
					}
				}
				get_damage = take_hit(DAMAGE_ATTACK, dam, killer);
				STOP_MULTISHADOW();
			}

			break;
		}

		case GF_OLD_STONE:
		{
#ifdef JP
			if (fuzzy) msg_print("何か乾いたもので攻撃された！");
#else
			if (fuzzy) msg_print("You are hit by something dry!");
#endif

			if (p_ptr->resist_stone) break;
			else if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし石化を跳ね返した！");
#else
				msg_print("You resist stoning!");
#endif
			}
			else if (p_ptr->resist_acid || p_ptr->oppose_acid || p_ptr->immune_acid || p_ptr->resist_shard)
			{
				if (!p_ptr->stoning) set_stoning(1);
			}
			else
			{
#ifdef JP
				msg_print("生きたまま石像になっていく...");
#else
				msg_print("You are into a living statue...");
#endif
				p_ptr->is_dead |= DEATH_STONED;
				dam = 0;
				get_damage = take_hit(DAMAGE_NOESCAPE, dam, killer);
			}
			break;
		}

		/* Default */
		default:
		{
			/* No damage */
			dam = 0;

			break;
		}
	}

	if (p_ptr->tim_eyeeye && get_damage > 0 && !p_ptr->is_dead)
	{
#ifdef JP
		msg_format("攻撃が%s自身を傷つけた！", m_name);
#else
		char m_name_self[80];
		
		/* hisself */
		monster_desc(m_name_self, m_ptr, MD_PRON_VISIBLE | MD_POSSESSIVE | MD_OBJECTIVE);

		msg_format("The attack of %s has wounded %s!", m_name, m_name_self);
#endif
		project(0, 0, m_ptr->fy, m_ptr->fx, get_damage, GF_MISSILE, PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
		set_tim_eyeeye(p_ptr->tim_eyeeye-5, TRUE);
	}

	if (p_ptr->riding && dam > 0)
	{
		rakubadam_p = (dam > 200) ? 200 : dam;
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
	int pd = distance(y1, x1, y, x);
	int nd = distance(y1, x1, y2, x2);

	if (pd > nd) return distance(y, x, y2, x2);

	/* Component of P on N */
	nd = ((nd) ? ((py * ny + px * nx) / nd) : 0);

   /* Absolute value */
   return((nd >= 0) ? nd : 0 - nd);
}



/*
 * XXX XXX XXX
 * Modified version of los() for calculation of disintegration balls.
 * Disintegration effects are stopped by permanent walls.
 */
bool in_disintegration_range(int y1, int x1, int y2, int x2)
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


/*
 *  Do disintegration effect on the terrain
 *  before we decide the region of the effect.
 */
static bool do_disintegration(int by, int bx, int y, int x)
{
	byte feat;

	/* Disintegration balls explosions are stopped by perma-walls */
	if (!in_disintegration_range(by, bx, y, x)) return FALSE;

	/* Permanent walls and artifacts don't get effect */
	/* But not protect monsters and other objects */
	if (!cave_valid_bold(y, x)) return TRUE;

	feat = cave[y][x].feat;

	if ((feat != FEAT_DEEP_WATER) &&
	    (feat != FEAT_DEEP_LAVA) &&
	    (feat != FEAT_DARK_PIT))
	{
		cave_force_set_floor(y, x);
	}

	/* Update some things -- similar to GF_KILL_WALL */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

	return TRUE;
}


/*
 * breath shape
 */ 
void breath_shape(u16b *path_g, int dist, int *pgrids, byte *gx, byte *gy, u16b *gm, int *pgm_rad, int rad, int y1, int x1, int y2, int x2, bool disint_ball, bool real_breath)
{
	int by = y1;
	int bx = x1;
	int brad = 0;
	int brev = rad * rad / dist;
	int bdis = 0;
	int cdis;
	int path_n = 0;
	int tdis = distance(y1, x1, y2, x2);
	int mdis = tdis + rad;

	while (bdis <= mdis)
	{
		int x, y;

		if ((0 < dist) && (path_n < dist))
		{
			int ny = GRID_Y(path_g[path_n]);
			int nx = GRID_X(path_g[path_n]);
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

					/* Enforce a circular "ripple" */
					if (distance(y1, x1, y, x) != bdis) continue;

					/* Enforce an arc */
					if (distance(by, bx, y, x) != cdis) continue;

					if (disint_ball)
					{
						/* Disintegration are stopped only by perma-walls */
						if (real_breath)
						{
							/* Destroy terrains */
							if (!do_disintegration(by, bx, y, x)) continue;
						}
						else
						{
							/* No actual disintegration */
							if (!in_disintegration_range(by, bx, y, x)) continue;
						}
					}
					else
					{
						/* The blast is stopped by walls */
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
 * Only 256 grids can be affected per projection, limiting the effective
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
bool project(int who, int rad, int y, int x, int dam, int typ, u32b flg, int mod_elem_mode)
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

	bool old_hide = FALSE;

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
	u16b path_g[512];

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	byte gx[2048], gy[2048];

	/* Encoded "radius" info (see above) */
	u16b gm[32];

	/* Actual radius encoded in gm[] */
	int gm_rad = rad;

	bool jump = FALSE;

	/* Attacker's name (prepared before polymorph)*/
	char who_name[80];

	if (flg & PROJECT_AVOIDABLE)
	{
		if (p_ptr->wind_guard)
		{
			if (p_ptr->stat_use[A_INT] < (18 + 150))
			{
				switch (typ)
				{
				case GF_PHYSICAL:
				case GF_BLUNT:
				case GF_EDGED:
					flg |= (PROJECT_THRU);
					break;
				default:
					flg &= ~(PROJECT_AVOIDABLE);
					break;
				}
			}
			else if (p_ptr->stat_use[A_INT] < (18 + 200))
			{
				if (typ != GF_ROCKET) flg |= (PROJECT_THRU);
				else flg &= ~(PROJECT_AVOIDABLE);
			}
			else
			{
				flg |= (PROJECT_THRU);
			}
		}
		else
		{
			flg &= ~(PROJECT_AVOIDABLE);
		}
	}

	/* Initialize by null string */
	who_name[0] = '\0';

	rakubadam_p = 0;
	rakubadam_m = 0;

	/* Default target of monsterspell is player */
	monster_target_y = py;
	monster_target_x = px;

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

	/* Attacker's name (prepared before polymorph)*/
	if (who > 0) monster_desc(who_name, &m_list[who], MD_IGNORE_HALLU | MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);

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
		breath = TRUE;
		if (flg & PROJECT_HIDE) old_hide = TRUE;
		flg |= PROJECT_HIDE;
	}


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

	if (breath && typ == GF_DISINTEGRATE)
	{
		flg |= (PROJECT_DISI);
	}

	/* Calculate the projection path */

	path_n = project_path(path_g, (project_length ? project_length : MAX_RANGE), y1, x1, y2, x2, flg);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int oy = y;
		int ox = x;

		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		if (flg & PROJECT_DISI)
		{
			if (cave_stop_disintegration(ny, nx) && (rad > 0)) break;
		}
		else
		{
			/* Hack -- Balls explode before reaching walls */
			if (!cave_floor_bold(ny, nx) && (rad > 0)) break;
		}

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
		if (!blind && !(flg & (PROJECT_HIDE | PROJECT_FAST)))
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
		if ((typ == GF_ATTACK) && (dam == PY_ATTACK_NYUSIN) && ((i+1) == path_n))
		{
			if (cave_empty_bold(y, x)) teleport_player_to(ny, nx, FALSE, TRUE);
		}

	}

	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	if (breath && (y1 == y2) && (x1 == x2))
	{
		breath = FALSE;
		gm_rad = 1;
		if (!old_hide)
		{
			flg &= ~(PROJECT_HIDE);
		}
	}

	/* Start the "explosion" */
	gm[0] = 0;

	/* Hack -- make sure beams get to "explode" */
	gm[1] = grids;

	dist = path_n;
	dist_hack = dist;

	project_length = 0;

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

			breath_shape(path_g, dist, &grids, gx, gy, gm, &gm_rad, rad, y1, x1, y2, x2, (bool)(typ == GF_DISINTEGRATE), TRUE);
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
							/* Disintegration are stopped only by perma-walls */
							if (!do_disintegration(y2, x2, y, x)) continue;
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


	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();


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
				if (project_f(who, d, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
			}
			else
			{
				/* Affect the grid */
				if (project_f(who, dist, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
			}
		}
	}


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
				if (project_o(who, d, y, x, dam, typ, flg)) notice = TRUE;
			}
			else
			{
				/* Affect the object in the grid */
				if (project_o(who, dist, y, x, dam, typ, flg)) notice = TRUE;
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
					if ((y == y2) && (x == x2) && (y == py) && (x == px) && (flg & PROJECT_PLAYER))
					{
						if (project_m(who, d+1, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
					}
					else if (project_m(who, d, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
				}
				else
				{
					/* Affect the monster in the grid */
					if ((y == y2) && (x == x2) && (y == py) && (x == px) && (flg & PROJECT_PLAYER))
					{
						if (!(flg & PROJECT_BEAM))
						{
							if (project_m(who, dist+1, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
						}
					}
					else if (project_m(who, dist, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
				}
			}
			else
			{
				monster_race *ref_ptr = &r_info[m_list[cave[y][x].m_idx].r_idx];

				if ((ref_ptr->flags2 & RF2_REFLECTING) && !one_in_(10) && (flg & PROJECT_REFLECTABLE) && (!who || dist_hack > 1))
				{
					byte t_y, t_x;
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
					flg &= ~(PROJECT_MONSTER);
					flg |= (PROJECT_THRU | PROJECT_PLAYER | PROJECT_REFLECTABLE);

					project(cave[y][x].m_idx, 0, t_y, t_x, dam, typ, flg, mod_elem_mode);
				}
				else
				{
					if ((y == y2) && (x == x2) && (y == py) && (x == px) && (flg & PROJECT_PLAYER))
					{
					}
					else if (project_m(who, dist, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
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
				if (m_ptr->ml) monster_race_track(m_ptr->ap_r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(cave[y][x].m_idx);
			}
		}
	}


	/* Check player */
	if (flg & (PROJECT_KILL))
	{
		int d;

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

			if (p_ptr->use_decoy)
			{
				if ((dam > 0) && who && (y == p_ptr->decoy_y) && (x == p_ptr->decoy_x))
				{
					switch (typ)
					{
					case GF_ACID:
					case GF_ELEC:
					case GF_FIRE:
					case GF_COLD:
					case GF_POIS:
					case GF_LITE:
					case GF_DARK:
					case GF_NETHER:
					case GF_WATER:
					case GF_PLASMA:
					case GF_SHARDS:
					case GF_SOUND:
					case GF_CONFUSION:
					case GF_CHAOS:
					case GF_STONE:
					case GF_DISENCHANT:
					case GF_FORCE:
					case GF_INERTIA:
					case GF_TIME:
					case GF_GRAVITY:
					case GF_ICE:
					case GF_NUKE:
					case GF_ROCKET:
					case GF_MISSILE:
					case GF_PHYSICAL:
					case GF_BLUNT:
					case GF_EDGED:
					case GF_MANA:
					case GF_METEOR:
					case GF_DISINTEGRATE:
					case GF_HOLY_FIRE:
					case GF_HELL_FIRE:
					case GF_GODLY_SPEAR:
					case GF_PURE_FIRE:
					case GF_PURE_AQUA:
					case GF_PURE_EARTH:
					case GF_PURE_WIND:
					case GF_CAUSE_1:
					case GF_CAUSE_2:
					case GF_CAUSE_3:
					case GF_CAUSE_4:
					case GF_HAND_DOOM:
					case GF_DEATH_RAY:
					case GF_MIND_BLAST:
					case GF_BRAIN_SMASH:
					case GF_OLD_DRAIN:
					case GF_NEW_DRAIN:
					case GF_DUAL_DRAIN:
					case GF_DRAIN_SOUL:
						break_decoy();
					}
				}
			}

			/* Find the closest point in the blast */
			if (breath) d = dist_to_line(y, x, y1, x1, y2, x2);
			else d = dist;

			if ((y == y2) && (x == x2) && (y == py) && (x == px) && (!flg & PROJECT_MONSTER)) d++;

			/* Affect the player */
			if (project_p(who, who_name, d, y, x, dam, typ, flg, mod_elem_mode)) notice = TRUE;
		}
	}

	if (p_ptr->riding)
	{
		char m_name[80];

		monster_desc(m_name, &m_list[p_ptr->riding], 0);

		if (rakubadam_m > 0)
		{
			if (rakuba(rakubadam_m, FALSE))
			{
#ifdef JP
				msg_format("%^sに振り落とされた！", m_name);
#else
				msg_format("%^s has thrown you off!", m_name);
#endif
			}
		}
		if (p_ptr->riding && rakubadam_p > 0)
		{
			if(rakuba(rakubadam_p, FALSE))
			{
#ifdef JP
				msg_format("%^sから落ちてしまった！", m_name);
#else
				msg_format("You have fallen from %s.", m_name);
#endif
			}
		}
	}

	/* Return "something was noticed" */
	return (notice);
}
