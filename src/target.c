/*
 * File: target.c
 * Purpose: Targeting code
 *
 * Copyright (c) 1997-2007 Angband contributors, Jeff Greene, Diego Gonzalez
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
#include "game-cmd.h"
#include "ui-menu.h"
#include "game-event.h"
/*#include "cmds.h"*/

/*
 * Height of the help screen; any higher than 4 will overlap the health
 * bar which we want to keep in targeting mode.
 */
#define HELP_HEIGHT 3



/*** Functions ***/

/*
 * Monster health description.
 * This function should not be called without a check that the monster is a mimic
 */
static void look_mon_desc(char *buf, size_t max, int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool living = TRUE;

	/* Determine if the monster is "living" (vs "undead") */
	if (monster_nonliving(r_ptr)) living = FALSE;

	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		my_strcpy(buf, (living ? "unhurt" : "undamaged"), max);
	}
	else
	{
		/* Calculate a health "percentage" */
		int perc = 100L * m_ptr->hp / m_ptr->maxhp;

		if (perc >= 60)
			my_strcpy(buf, (living ? "somewhat wounded" : "somewhat damaged"), max);
		else if (perc >= 25)
			my_strcpy(buf, (living ? "wounded" : "damaged"), max);
		else if (perc >= 10)
			my_strcpy(buf, (living ? "badly wounded" : "badly damaged"), max);
		else
			my_strcpy(buf, (living ? "almost dead" : "almost destroyed"), max);
	}

	if (m_ptr->mflag & (MFLAG_TOWN)) my_strcat(buf, ", town", sizeof(buf));
	if (m_ptr->mflag & (MFLAG_STERILE)) my_strcat(buf, ", sterile", sizeof(buf));
	if (m_ptr->mflag & (MFLAG_WARY)) my_strcat(buf, ", wary", sizeof(buf));
	if (m_ptr->mflag & (MFLAG_FLYING)) my_strcat(buf, ", flying", sizeof(buf));
	if (m_ptr->m_timed[MON_TMD_SLEEP]) my_strcat(buf, ", asleep", max);
	if (m_ptr->m_timed[MON_TMD_CONF]) my_strcat(buf, ", confused", max);
	if (m_ptr->m_timed[MON_TMD_FEAR]) my_strcat(buf, ", afraid", max);
	if (m_ptr->m_timed[MON_TMD_STUN]) my_strcat(buf, ", stunned", max);
	if ((m_ptr->m_timed[MON_TMD_SLOW]) && (!m_ptr->m_timed[MON_TMD_FAST])) my_strcat(buf, ", slowed", max);
	if ((!m_ptr->m_timed[MON_TMD_SLOW]) && (m_ptr->m_timed[MON_TMD_FAST])) my_strcat(buf, ", hasted", max);
}


/*
 * Draw a visible path over the squares between (x1,y1) and (x2,y2).
 * The path consists of "*", which are white except where there is a
 * monster, object or feature in the grid.
 *
 * This routine has (at least) three weaknesses:
 * - remembered objects/walls which are no longer present are not shown,
 * - squares which (e.g.) the player has walked through in the dark are
 *   treated as unknown space.
 * - walls which appear strange due to hallucination aren't treated correctly.
 *
 * The first two result from information being lost from the dungeon arrays,
 * which requires changes elsewhere
 */
static int draw_path(u16b path_n, u16b *path_g, char *c, byte *a, int y1, int x1, int cur_tar_y, int cur_tar_x)
{
	int i;
	bool on_screen;
	byte color_type;

	/* No path, so do nothing. */
	if (path_n < 1) return (FALSE);

	/* The starting square is never drawn, but notice if it is being
     * displayed. In theory, it could be the last such square.
     */
	on_screen = panel_contains(y1, x1);

	/* Draw the path. */
	for (i = 0; i < path_n; i++)
	{
		/* Find the co-ordinates on the level. */
		int y = GRID_Y(path_g[i]);
		int x = GRID_X(path_g[i]);

		byte this_a;
		char this_c;

		/*
		 * As path[] is a straight line and the screen is oblong,
		 * there is only section of path[] on-screen.
		 * If the square being drawn is visible, this is part of it.
		 * If none of it has been drawn, continue until some of it
		 * is found or the last square is reached.
		 * If some of it has been drawn, finish now as there are no
		 * more visible squares to draw.
		 *
		 */
		 if (panel_contains(y,x)) on_screen = TRUE;
		 else if (on_screen) break;
		 else continue;

	 	/* Find the position on-screen */
		move_cursor_relative(y,x);

		/* This square is being overwritten, so save the original. */
		Term_what(Term->scr->cx, Term->scr->cy, a+i, c+i);

		/* Choose a colour. */
		/* Visible monsters are orange. */
		if (cave_m_idx[y][x] && mon_list[cave_m_idx[y][x]].ml)
		{
			color_type = TERM_ORANGE;
		}

		/* Known objects are yellow. */
		else if (cave_o_idx[y][x] && o_list[cave_o_idx[y][x]].marked)
		{
			color_type = TERM_YELLOW;
    	}

		/* Effects are green */
		else if ((cave_x_idx[y][x] > 0) && (cave_info[y][x] & (CAVE_SEEN | CAVE_MARK)))
		{
			color_type = TERM_GREEN;
		}

		/* Known walls are blue. */
		else if (!cave_project_bold(y,x) &&
				((cave_info[y][x] & (CAVE_MARK)) ||	player_can_see_bold(y,x)))
		{
			color_type = TERM_BLUE;
		}
		/* Unknown squares are grey. */
		else if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			color_type = TERM_L_DARK;
		}
		/* Unoccupied squares are white. */
		else
		{
			color_type = TERM_WHITE;
		}

		/* ALways use red for the current target square */
		if ((cur_tar_y == y) && (cur_tar_x == x)) color_type = TERM_RED;

		/* Get the character */
		if (!use_graphics)
		{
			this_a = color_type;
			this_c = '*';
		}
		/* Graphics are being used */
		else
		{
			this_a = color_to_attr[TILE_BALL_INFO][color_type];
			this_c = color_to_char[TILE_BALL_INFO][color_type];
		}

		/* Visual effects -- Display */
		print_rel(this_c, this_a, y, x);
	}
	return i;
}

/*
 * Load the attr/char at each point along "path" which is on screen from
 * "a" and "c". This was saved in draw_path().
 */
static void load_path(u16b path_n, u16b *path_g, char *c, byte *a)
{
	int i;
	for (i = 0; i < path_n; i++)
	{
		if (!panel_contains(GRID_Y(path_g[i]), GRID_X(path_g[i]))) continue;

		move_cursor_relative(GRID_Y(path_g[i]), GRID_X(path_g[i]));

		(void)Term_addch(a[i], c[i]);
	}

	(void)Term_fresh();
}


/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targetting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(int m_idx)
{

	monster_type *m_ptr;

	/* No monster */
	if (m_idx <= 0) return (FALSE);

	/* Get monster */
	m_ptr = &mon_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	/* Monster must be projectable */
	if (!m_ptr->project) return (FALSE);

	/* Walls protect monsters */
	if (!cave_project_bold(m_ptr->fy, m_ptr->fx) &&
		!cave_passable_bold(m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->timed[TMD_IMAGE]) return (FALSE);

	/* Hack -- Never target trappers XXX XXX XXX */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Hidden monsters cannot be targets */
	if (m_ptr->mflag & (MFLAG_HIDE)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}



/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void)
{
	/* No target */
	if (!p_ptr->target_set) return (FALSE);

	/* Accept "location" targets */
	if (p_ptr->target_who == 0) return (TRUE);

	/* Check "monster" targets */
	if (p_ptr->target_who > 0)
	{
		int m_idx = p_ptr->target_who;

		/* Accept reasonable targets */
		if (target_able(m_idx))
		{
			monster_type *m_ptr = &mon_list[m_idx];

			/* Get the monster location */
			p_ptr->target_row = m_ptr->fy;
			p_ptr->target_col = m_ptr->fx;

			/* Good target */
			return (TRUE);
		}
	}

	/* Assume no target */
	return (FALSE);
}



/*
 * Set the target to a monster (or nobody)
 */
void target_set_monster(int m_idx)
{
	/* Acceptable target */
	if ((m_idx > 0) && target_able(m_idx))
	{
		monster_type *m_ptr = &mon_list[m_idx];

		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = m_idx;
		p_ptr->target_row = m_ptr->fy;
		p_ptr->target_col = m_ptr->fx;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
	}

	p_ptr->redraw |= (PR_HEALTH);
}



/*
 * Set the target to a location
 */
void target_set_location(int y, int x)
{
	/* Legal target */
	if (in_bounds_fully(y, x))
	{
		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = 0;
		p_ptr->target_row = y;
		p_ptr->target_col = x;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
	}

	p_ptr->redraw |= (PR_HEALTH);
}


/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(const void *u, const void *v, int a, int b)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	int da, db, kx, ky;

	/* Absolute distance components */
	kx = x[a]; kx -= px; kx = ABS(kx);
	ky = y[a]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Absolute distance components */
	kx = x[b]; kx -= px; kx = ABS(kx);
	ky = y[b]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Compare the distances */
	return (da <= db);
}


/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
static void ang_sort_swap_distance(void *u, void *v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	byte temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}

/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
	int i, v;

	int x2, y2, x3, y3, x4, y4;

	int b_i = -1, b_v = 9999;


	/* Scan the locations */
	for (i = 0; i < temp_n; i++)
	{
		/* Point 2 */
		x2 = temp_x[i];
		y2 = temp_y[i];

		/* Directed distance */
		x3 = (x2 - x1);
		y3 = (y2 - y1);

		/* Verify quadrant */
		if (dx && (x3 * dx <= 0)) continue;
		if (dy && (y3 * dy <= 0)) continue;

		/* Absolute distance */
		x4 = ABS(x3);
		y4 = ABS(y3);

		/* Verify quadrant */
		if (dy && !dx && (x4 > y4)) continue;
		if (dx && !dy && (y4 > x4)) continue;

		/* Approximate Double Distance */
		v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

		/* Penalize location XXX XXX XXX */

		/* Track best */
		if ((b_i >= 0) && (v >= b_v)) continue;

		/* Track best */
		b_i = i; b_v = v;
	}

	/* Result */
	return (b_i);
}





/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_interactive_accept(int y, int x)
{
	object_type *o_ptr;

	/* Player grids are always interesting */
	if (cave_m_idx[y][x] < 0) return (TRUE);

	/* Handle hallucination */
	if (p_ptr->timed[TMD_IMAGE]) return (FALSE);

	/* Visible monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorized object */
		if (o_ptr->marked) return (TRUE);
	}

	/* Interesting memorized features */
	/* Ignore unknown features */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (FALSE);

	/* Find interesting effects */
   	if (cave_x_idx[y][x] > 0)
	{
		/* Get the first effect */
		u16b x_idx = cave_x_idx[y][x];

		/* Scan the effects on that grid */
		while (x_idx)
		{
			/* Get the effect data */
			effect_type *x_ptr = &x_list[x_idx];

			/* Point to the next effect */
			x_idx = x_ptr->next_x_idx;

			/* Ignore hidden effects */
			if (!(x_ptr->x_f_idx) ||
				(x_ptr->x_flags & (EF1_HIDDEN))) continue;

			/* We have an interesting effect */
			return (TRUE);
		}
	}

	/* Check grid type with dungeon capabilities */
	return ((*dun_cap->can_target_feature)(cave_feat[y][x]));
}

/*
 * Determine if a trap makes a reasonable target
 */
static bool target_able_trap(int y, int x)
{
	/* Must be on line of fire */
	if (!player_can_fire_bold(y, x)) return (FALSE);

	/* Only player traps allowed. Ignore monster traps and glyphs */
	if (!cave_player_trap_bold(y, x)) return (FALSE);

	/* Ignore hidden traps */
	if (x_list[cave_x_idx[y][x]].x_flags & (EF1_HIDDEN)) return (FALSE);

	/* Known player traps are okay */
	return (TRUE);
}


/*
 * Prepare the "temp" array for "target_interactive_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_interactive_prepare(int mode)
{
	int y, x;

	bool expand_look = (mode & (TARGET_LOOK)) ? TRUE : FALSE;

	/* Reset "temp" array */
	clear_temp_array();

	/* Scan the current panel */
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			bool do_continue = FALSE;

			/* Check overflow */
			if (temp_n >= TEMP_MAX) continue;

			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Require line of sight, unless "look" is "expanded" */
			if (!player_has_los_bold(y, x) && (!expand_look)) continue;

			/* Require "interesting" contents */
			if (!target_set_interactive_accept(y, x)) continue;

			/* Special mode */
			if (mode & (TARGET_KILL))
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) do_continue = TRUE;

				/* Must be a targettable monster */
			 	if (!target_able(cave_m_idx[y][x])) do_continue = TRUE;
			}

			/* Don't continue on the trap exception, or if probing. */
			if ((mode & (TARGET_TRAP)) && target_able_trap(y, x)) do_continue = FALSE;
			else if (mode & (TARGET_PROBE)) do_continue = FALSE;

			if (do_continue) continue;

			/*
			 * Hack - don't go over redundant elemental terrain \
			 * (since we have large lakes and pools of the same terrain)
			 */
			if ((p_ptr->target_row > 0) || (p_ptr->target_col > 0))
			{
				if (cave_feat[p_ptr->target_row][p_ptr->target_col] == cave_feat[y][x])
				{
					if (cave_ff3_match(y, x, TERRAIN_MASK)) continue;
				}
			}

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_distance;
	ang_sort_swap = ang_sort_swap_distance;

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);
}


/*
 * Perform the minimum "whole panel" adjustment to ensure that the given
 * location is contained inside the current panel, and return TRUE if any
 * such adjustment was performed.
 */
static bool adjust_panel(int y, int x)
{
	bool changed = FALSE;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		int wx, wy;
		int screen_hgt, screen_wid;

		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & PW_MAP)) continue;

		wy = t->offset_y;
		wx = t->offset_x;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		/* Adjust as needed */
		while (y >= wy + screen_hgt) wy += screen_hgt / 2;
		while (y < wy) wy -= screen_hgt / 2;

		/* Adjust as needed */
		while (x >= wx + screen_wid) wx += screen_wid / 2;
		while (x < wx) wx -= screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
}


/*
 * Describe a location relative to the player position.
 * e.g. "12 S 35 W" or "0 N, 33 E" or "0 N, 0 E"
 */
static void coords_desc(char *buf, int size, int y, int x) {

	const char *east_or_west;
	const char *north_or_south;

	int py = p_ptr->py;
	int px = p_ptr->px;

	if (y > py)
		north_or_south = "S";
	else
		north_or_south = "N";

	if (x < px)
		east_or_west = "W";
	else
		east_or_west = "E";

	strnfmt(buf, size, "%d %s, %d %s",
		ABS(y-py), north_or_south, ABS(x-px), east_or_west);
}

/*
 * Display targeting help at the bottom of the screen.
 */
static void target_display_help(bool monster, bool free)
{
	/* Determine help location */
	int wid, hgt, help_loc;
	Term_get_size(&wid, &hgt);
	help_loc = hgt - HELP_HEIGHT - (mouse_buttons ? 1 : 0);

	/* Clear */
	clear_from(help_loc);

	/* Prepare help hooks */
	text_out_hook = text_out_to_screen;
	text_out_indent = 1;
	Term_gotoxy(1, help_loc);

	/* Display help */
	text_out_c(TERM_L_GREEN, "<dir>");
	text_out(" and ");
	text_out_c(TERM_L_GREEN, "<click>");
	text_out(" look around. '");
	text_out_c(TERM_L_GREEN, "g");
	text_out(" moves to the selection. '");
	text_out_c(TERM_L_GREEN, "p");
	text_out("' selects the player. '");
	text_out_c(TERM_L_GREEN, "q");
	text_out("' exits. '");
	text_out_c(TERM_L_GREEN, "r");
	text_out("' displays details. '");

	if (free)
	{
		text_out_c(TERM_L_GREEN, "m");
		text_out("' restricts to interesting places. ");
	}
	else
	{
		text_out_c(TERM_L_GREEN, "+");
		text_out("' and '");
		text_out_c(TERM_L_GREEN, "-");
		text_out("' cycle through interesting places. '");
		text_out_c(TERM_L_GREEN, "o");
		text_out("' allows free selection. ");
	}

	if (monster || free)
	{
		text_out("'");
		text_out_c(TERM_L_GREEN, "t");
		text_out("' targets the current selection.");
	}

	/* Reset */
	text_out_indent = 0;
}


/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * This function correctly handles multiple objects per grid, and objects
 * and terrain features in the same grid, though the latter never happens.
 *
 * This function must handle blindness/hallucination.
 */
static ui_event_data target_set_interactive_aux(int y, int x, int mode, cptr info, bool list_floor_objects)
{
	s16b this_o_idx, next_o_idx = 0;
	s16b this_x_idx, next_x_idx = 0;

	cptr s1, s2, s3;

	bool floored;

	u16b feat;

	ui_event_data query;

	char out_val[256];

	char coords[20];

	/* Describe the square location */
	coords_desc(coords, sizeof(coords), y, x);

	/* Repeat forever */
	while (1)
	{
		int i;

		char feat_name[80];
		/* Terrain suffix for monsters and objects */
		char terrain_suffix[200];

		/* Temporary array of visible effects */
		s16b x_seen[50];
		u16b size_x_seen = 0;

		/* Paranoia */
		query.key = ' ';

		/* Default */
		s1 = "You see ";
		s2 = "";
		s3 = "on ";


		/* The player */
		if (cave_m_idx[y][x] < 0)
		{
			/* Description */
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
		}

		/* Feature (apply "mimic") */
		feat = f_info[cave_feat[y][x]].f_mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		else
		{
			/* Hack -- track the current feature */
			feature_kind_track(feat);

			/* Window stuff */
			p_ptr->redraw |= (PR_FEATURE);
		}

		/* Pick a prefix */
		if (*s2 && (!feat_ff1_match(feat, FF1_MOVE) ||
			!feat_ff1_match(feat, FF1_LOS) ||
			feat_ff1_match(feat, FF1_SHOP | FF1_DOOR) ||
			feat_ff2_match(feat, FF2_SHALLOW | FF2_DEEP) ||
			feat_ff3_match(feat, FF3_NEED_TREE)))
		{
			s3 = "in ";
		}

		/* Get a default name */
		if (feat <= FEAT_NONE)
		{
			my_strcpy(feat_name, "an unknown grid", sizeof(feat_name));
		}
		/* Get the real name */
		else
		{
			feature_desc(feat_name, sizeof (feat_name), feat, TRUE, FALSE);
		}

		/* List all effects in the grid */
		for (this_x_idx = cave_x_idx[y][x]; this_x_idx; this_x_idx = next_x_idx)
		{
			effect_type *x_ptr;

			/* Get the effect */
			x_ptr = &x_list[this_x_idx];

			/* Get the next effect */
			next_x_idx = x_ptr->next_x_idx;

			/* Describe it, if not hidden */
			if (!(x_ptr->x_flags & (EF1_HIDDEN)) && x_ptr->x_f_idx)
			{
				/* Check for available space */
				if (size_x_seen < N_ELEMENTS(x_seen))
				{
					x_seen[size_x_seen++] = x_ptr->x_f_idx;
				}
			}
		}

		/* Prepare the terrain suffix for monsters and objects */
		my_strcpy(terrain_suffix, format(" %s%s", s3, feat_name), sizeof(terrain_suffix));

		/* Concat the collected effect names */
		for (i = 0; i < size_x_seen; i++)
		{
			char x_name[80];

			/* Obtain an object description */
			feature_desc(x_name, sizeof(x_name), x_seen[i], TRUE, TRUE);

			/* First effect */
			if (i == 0)
			{
				if ((feat == FEAT_NONE) || !feat_ff1_match(feat, FF1_MOVE) ||
					cave_any_trap_bold(y, x))
				{
					/* Basic info */
					my_strcat(terrain_suffix, format(" with %s", x_name),
						sizeof(terrain_suffix));
				}
				else
				{
					/* Basic info */
					my_strcat(terrain_suffix, format(" beneath %s", x_name),
						sizeof(terrain_suffix));
				}
			}

			/* Basic info */
			else if (i < (size_x_seen - 1))
			{
				my_strcat(terrain_suffix, format(", %s", x_name), sizeof(terrain_suffix));
			}

			/* Basic info */
			else
			{
				my_strcat(terrain_suffix, format(" and %s", x_name), sizeof(terrain_suffix));
			}
		}

		/* Ignore the terrain suffix if certain things happen */
		if ((size_x_seen == 0) && (feat <= FEAT_FLOOR))
		{
			terrain_suffix[0] = '\0';
		}

		/* Hack -- hallucination */
		if (p_ptr->timed[TMD_IMAGE])
		{
			cptr name = "something strange";

			/* Display a message */
			if (p_ptr->wizard)
			{
				strnfmt(out_val, sizeof(out_val),
						"%s%s%s, [%s] %s (%d:%d).", s1, s2, name, info, coords, y, x);
			}
			else
			{
				strnfmt(out_val, sizeof(out_val),
						"%s%s%s [%s], %s.", s1, s2, name, info, coords);
			}

			prt(out_val, 0, 0);
			move_cursor_relative(y, x);

			query = inkey_ex();

			/* Stop on everything but "return" */
			if ((query.key != '\n') && (query.key != '\r')) break;

			/* Repeat forever */
			continue;
		}

		/* Actual monsters */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Visible */
			if (m_ptr->ml)
			{
				bool recall = FALSE;

				char m_name[80];

				/* Get the monster name ("a kobold") */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0x08);

				/* Hack -- track this monster race */
				monster_race_track(m_ptr->r_idx);

				/* Hack -- health bar for this monster */
				health_track(cave_m_idx[y][x]);

				/*Track the feature*/
				feature_kind_track(cave_feat[y][x]);

				/* Window stuff */
				p_ptr->redraw |= (PR_FEATURE);

				/* Hack -- handle stuff */
				handle_stuff();

				/* Interact */
				while (1)
				{
					if (recall)	button_add("[CLEAR_RECALL]", 'r');
					else 		button_add("[RECALL]", 'r');
					if (cave_o_idx[y][x] > 0)button_add("[VIEW_FLOOR]", 'f');
					event_signal(EVENT_MOUSEBUTTONS);

					/* Recall */
					if (recall)
					{
						/* Save screen */
						screen_save();

						/* Recall on screen */
						screen_roff(m_ptr->r_idx);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format(" [r,%s]", info));

						/* Command */
						query = inkey_ex();

						/* Load screen */
						screen_load();
					}

					/* Normal */
					else
					{
						char buf[80];

						/* Basic info */
						strnfmt(out_val, sizeof(out_val),
							"%s%s%s", s1, s2, m_name);

						/* Describe the monster, unless a mimic */
						look_mon_desc(buf, sizeof(buf), cave_m_idx[y][x]);

						/* Monster state, terrain suffix, options  */
						my_strcat(out_val, format(" (%s)%s [r,%s]",
							buf, terrain_suffix, info),
							sizeof(out_val));

						/* Wizards want coordinates */
						if (p_ptr->wizard)
						{
							my_strcat(out_val, format(" (%d:%d)", y, x),
								sizeof(out_val));
						}

						/* Hack -- handle stuff */
						handle_stuff();

						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey_ex();
					}

					button_kill('r');
					button_kill('f');
					event_signal(EVENT_MOUSEBUTTONS);

					/* Normal commands */
					if (query.key != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Stop on everything but "return"/"space", or floor */
				if ((query.key != '\n') && (query.key != '\r') &&
					(query.key != ' ') && (query.key != 'f')) break;

				/* continue with 'f' only if there are floor items....*/
				if ((query.key == 'f') && (!cave_o_idx[y][x])) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Hack -- take account of gender */
				if (r_ptr->flags1 & (RF1_FEMALE)) s1 = "She is ";
				else if (r_ptr->flags1 & (RF1_MALE)) s1 = "He is ";

				/* Use a preposition */
				s2 = "carrying ";

				/* Scan all objects being carried */
				for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					char o_name[80];

					object_type *o_ptr;

					/* Get the object */
					o_ptr = &o_list[this_o_idx];

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/*Don't let the player see certain objects (used for vault treasure)*/
					if ((o_ptr->ident & (IDENT_HIDE_CARRY)) && (!p_ptr->wizard) &&
						(!cheat_peek))	 continue;

					/* Obtain an object description */
					object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

					/* Describe the object */
					strnfmt(out_val, sizeof(out_val),
						"%s%s%s [%s]", s1, s2, o_name, info);

					/* Wizards want coordinates */
					if (p_ptr->wizard)
					{
						my_strcat(out_val, format(" (%d:%d)", y, x), sizeof(out_val));
					}

					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey_ex();

					/* Stop on everything but "return"/"space" */
					if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

					/* Sometimes stop at "space" key */
					if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

					/* Change the intro */
					s2 = "also carrying ";
				}

				/* Double break */
				if (this_o_idx) break;

				/* Use a preposition */
				s2 = "on ";
			}
		}

		/* Assume not floored */
		floored = FALSE;

		/* Scan all objects in the grid */
		if (TRUE)
		{
			int floor_list[MAX_FLOOR_STACK];
			int floor_num;

			track_object(-floor_list[0]);
			handle_stuff();

			/* Scan for floor objects */
			floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, y, x, 0x02);

			/* Actual pile */
			if (floor_num > 1)
			{
				/* Floored */
				floored = TRUE;

				/* Describe */
				while (1)
				{
					/* Basic info */
					strnfmt(out_val, sizeof(out_val),
						"%s%sa pile of %d objects%s [r,%s]", s1, s2,
						floor_num, terrain_suffix, info);

					/* Wizards want coordinates */
					if (p_ptr->wizard)
					{
						my_strcat(out_val, format(" (%d:%d)", y, x), sizeof(out_val));
					}

					prt(out_val, 0, 0);

					if (list_floor_objects)
					{
						/* Save screen */
						screen_save();

						/* Display */
						show_floor(floor_list, floor_num, (OLIST_WEIGHT | OLIST_GOLD));
					}
					move_cursor_relative(y, x);
					query = inkey_ex();

					if (list_floor_objects)
					{
						screen_load();
					}

					/* Display objects */
					if (query.key == 'r')
					{
						int pos;

						pos = query.key - 'a';
						if (0 <= pos && pos < floor_num)
						{
							track_object(-floor_list[pos]);
							handle_stuff();
						}
					}

					/* Done */
					break;
				}

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Preposition */
				s2 = "on ";
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

			/* Skip objects if floored */
			if (floored) continue;

			/* Describe it */
			if (o_ptr->marked)
			{
				char o_name[80];

				/* Obtain an object description */
				object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

				/* Basic info */
				strnfmt(out_val, sizeof(out_val), "%s%s%s%s [I,%s]",
					s1, s2, o_name, terrain_suffix, info);

				/* Wizards want coordinates */
				if (p_ptr->wizard)
				{
					my_strcat(out_val, format(" (%d:%d)", y, x),
						sizeof(out_val));
				}

				/* Show object. Handle object recall */
				while (TRUE)
				{
					/* Print the prompt */
					prt(out_val, 0, 0);

					/* Move cursor to that location */
					move_cursor_relative(y, x);

					/* Read input key */
					query = inkey_ex();

					/* No object recall */
					if (query.key != 'I') break;

					/* Object recall. Clear the first line */
					prt("", 0, 0);

					/* Do it */
					object_info_screen(o_ptr);
				}

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Plurals */
				if (o_ptr->number != 1) s1 = "They are ";

				/* Preposition */
				s2 = "on ";
			}
		}

		/* Double break */
		if (this_o_idx) break;

		/* Display terrain */
		if (TRUE)
		{
			u16b temp_feat;
			bool enable_recall;
			bool show_recall = FALSE;
			char temp_name[80];

			/*
			 * Display terrain and effects
			 */
			for (i = 0; i <= size_x_seen; i++)
			{
				/* Hack - This is the mark for the feature stored in cave_feat */
				if (i == size_x_seen)
				{
				       	temp_feat = feat;

					/* Just copy the feature name */
					my_strcpy(temp_name, feat_name, sizeof(temp_name));
				}
				/* Any other value is an effect stored x_list */
				else
				{
					temp_feat = x_seen[i];

					/* Get the effect's name */
					feature_desc(temp_name, sizeof(temp_name), temp_feat, TRUE, TRUE);
				}

				/* Don't display feature recall if the grid is unknown */
				enable_recall = (temp_feat != FEAT_NONE);

				/* Handle recall */
				while (TRUE)
				{

					/* Handle recall mode */
					if (show_recall && enable_recall)
					{
						/* Save screen */
						screen_save();

						/* Recall feature on screen */
						screen_feature_roff(temp_feat);
					}

					/* Display a message */
					strnfmt(out_val, sizeof(out_val),
						"%s%s%s [%s%s]%s", s1, s2, temp_name,
						(enable_recall ? "r,": ""), info,
						(i < size_x_seen) ? " (more)": "");

					/* Wizards want coordinates */
					if (p_ptr->wizard)
					{
						my_strcat(out_val, format(" (%d:%d)", y, x), sizeof(out_val));
					}

					/*Track this feature*/
					feature_kind_track(temp_feat);

					/* Hack -- handle stuff */
					handle_stuff();

					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey_ex();

					/* Load screen if necessary */
					if (show_recall && enable_recall)
					{
						screen_load();
					}

					/* Stop on everything but the recall command, if enabled */
					if (!enable_recall || (query.key != 'r')) break;

					/* Toggle recall */
					show_recall = !show_recall;
				}

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;
			}
		}

		/* Hack -- handle stuff */
		handle_stuff();

		/* Stop on everything but "return" */
		if ((query.key != '\n') && (query.key != '\r')) break;
	}

	/* Keep going */
	return (query);
}

/* checks if there is a visible monster in line-of-sight */
bool valid_target_exists(int mode)
{
	int y, x, m_idx;

	/* Save target info */
	s16b old_target_set = p_ptr->target_set;
	s16b old_target_who = p_ptr->target_who;
	s16b old_target_row = p_ptr->target_row;
	s16b old_target_col = p_ptr->target_col;

	/* Cancel old target */
	target_set_monster(0);

	/* Get ready to do targetting */
	target_set_interactive_prepare(mode);

	/* Find the first monster in the queue */
	y = temp_y[0];
	x = temp_x[0];
	m_idx = cave_m_idx[y][x];

	/* restore old target into */
	p_ptr->target_set = old_target_set;
	p_ptr->target_who = old_target_who;
	p_ptr->target_row = old_target_row;
	p_ptr->target_col = old_target_col ;

	/* If nothing was prepared, then return */
	 if (temp_n < 1)
	{
		return FALSE;
	}

	/* Target the monster, if possible */
	if ((m_idx <= 0) || !target_able(m_idx))
	{
		return FALSE;
	}

	return (TRUE);
}

bool target_set_closest(int mode)
{
	int y, x, m_idx;
	monster_type *m_ptr;
	char m_name[80];
	bool visibility;

	/* Cancel old target */
	target_set_monster(0);

	/* Get ready to do targetting */
	target_set_interactive_prepare(mode);

	/* Find the first monster in the queue */
	y = temp_y[0];
	x = temp_x[0];
	m_idx = cave_m_idx[y][x];

	/* If nothing was prepared, then return */
  	if (temp_n < 1)
	{
		msg_print("No Available Target.");
		return FALSE;
	}

	/* Target the monster, if possible */
	if ((m_idx <= 0) || !target_able(m_idx))
	{
		msg_print("No Available Target.");
		return FALSE;
	}

	/* Target the monster */
	m_ptr = &mon_list[m_idx];
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
	if (!(mode & TARGET_QUIET))
		msg_format("%^s is targeted.", m_name);
	Term_fresh();

	/* Set up target information */
	monster_race_track(m_ptr->r_idx);
	health_track(cave_m_idx[y][x]);
	target_set_monster(m_idx);

	/* Visual cue */
	Term_get_cursor(&visibility);
	(void)Term_set_cursor(TRUE);
	move_cursor_relative(y, x);
	Term_redraw_section(x, y, x, y);

	/* TODO: what's an appropriate amount of time to spend highlighting */
	Term_xtra(TERM_XTRA_DELAY, 150);
	(void)Term_set_cursor(visibility);

	return TRUE;
}



/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
 *
 * Hack -- targetting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 *
 * 'mode' is one of TARGET_LOOK or TARGET_KILL.
 * 'x' and 'y' are the initial position of the target to be highlighted,
 * or -1 if no location is specified.
 * Returns TRUE if a target has been successfully set, FALSE otherwise.
 */
bool target_set_interactive(int mode, int x, int y)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, d, m, t, bd;
	int wid, hgt, help_prompt_loc;

	bool done = FALSE;
	bool flag = TRUE;
	bool help = FALSE;
	bool list_floor_objects = auto_display_lists;

	u16b path_n;
	u16b path_g[PATH_SIZE];
	u16b path_gx[PATH_SIZE];

	ui_event_data query;

	char info[80];

	/* These are used for displaying the path to the target */
	char path_char[MAX_RANGE];
	byte path_attr[MAX_RANGE];

	/* Temporarily turn off animate_flicker, must be re-set before exiting the function  */
	bool temp_animate_flicker = animate_flicker;
	animate_flicker = FALSE;

	/* If we haven't been given an initial location, start on the
	   player. */
	if (x == -1 || y == -1)
	{
		x = p_ptr->px;
		y = p_ptr->py;
	}
    /* If we /have/ been given an initial location, make sure we
	   honour it by going into "free targeting" mode. */
	else
	{
		flag = FALSE;
	}


	/* Cancel target */
	target_set_monster(0);

	/* make some buttons */
	button_backup_all();
	button_kill_all();
	button_add("[ESCAPE]", ESCAPE);
	button_add("[NEXT]", '+');
	button_add("[PREV]", '-');
	button_add("[PLAYER]", 'p');
	button_add("[PATHFIND]", 'g');
	button_add("[TARGET]", 't');

	/* health_track(0); */

	  /* All grids are selectable */
	if (mode & (TARGET_GRID))
	{
		/* Disable other modes */
		mode &= ~(TARGET_LOOK | TARGET_KILL | TARGET_TRAP);

		/* Disable interesting grids */
		flag = FALSE;
	}

	/* Calculate the window location for the help prompt */
	Term_get_size(&wid, &hgt);
	help_prompt_loc = hgt - (mouse_buttons ? 2 : 1);

	/* Display the help prompt */
	prt("'?' - help", help_prompt_loc, 0);

	/* Prepare the "temp" array */
	target_set_interactive_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		button_kill('f');
		button_kill('?');
		if (list_floor_objects)
		{
			button_add("[HIDE_OBJLIST]", 'f');
		}
		else button_add("[SHOW_OBJLIST]", 'f');
		if (help)
		{
			button_add("[HIDE_HELP]", '?');
		}
		else button_add("[SHOW_HELP]",'?');

		/* Interesting grids */
		if (flag && temp_n)
		{
			bool path_drawn = FALSE;
			int yy, xx;

			y = temp_y[m];
			x = temp_x[m];

			button_add("[SCAN]",'o');

			/* Update help */
			if (help)
			{
				bool good_target = ((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x]));
				target_display_help(good_target, !(flag && temp_n));
			}

			/* Dummy pointers to send to project_path */
			yy = y;
			xx = x;

			/* Allow targets for monsters....or traps, if applicable */
			if (((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x])) ||
				((mode & (TARGET_TRAP)) && target_able_trap(y, x)))
			{
				strcpy(info, "q,t,r,f,p,o,+,-,<dir>");
			}

			/* Dis-allow target */
			else
			{
				strcpy(info, "q,p,f,o,+,-,<dir>");
			}

			/* Adjust panel if needed */
			if (adjust_panel(y, x))
			{
				/* Handle stuff */
				handle_stuff();
			}

			/* Find the path. */
			path_n = project_path(path_g, path_gx, MAX_RANGE, py, px, &yy, &xx, PROJECT_THRU);

			/* Draw the path in "target" mode. If there is one */
			if ((mode & (TARGET_KILL)) && (cave_info[y][x] & (CAVE_FIRE)))
			{
				path_drawn = draw_path(path_n, path_g, path_char, path_attr, py, px, y, x);
			}
			event_signal(EVENT_MOUSEBUTTONS);

			/* Describe and Prompt */
			query = target_set_interactive_aux(y, x, mode, info, list_floor_objects);

			/* Remove the path */
			if (path_drawn) load_path(path_n, path_g, path_char, path_attr);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query.key)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				{
					if (++m == temp_n) m = 0;
					break;
				}

				case '-':
				{
					if (m-- == 0)  m = temp_n - 1;
					break;
				}

				case 'p':
				{
					/* Recenter around player */
					verify_panel();

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;
					break;
				}

				case 'o':
				{
					flag = FALSE;
					break;
				}

				case 'm':
				{
					break;
				}

				/* If we click, move the target location to the click and
				   switch to "free targetting" mode by unsetting 'flag'.
				   This means we get some info about wherever we've picked. */
				case DEFINED_XFF:
				{
					x = KEY_GRID_X(query);
					y = KEY_GRID_Y(query);
					flag = FALSE;
					break;
				}

				case 't':
				case '5':
				case '0':
				case '.':
				{
					int m_idx = cave_m_idx[y][x];

					if ((m_idx > 0) && target_able(m_idx))
					{
						health_track(m_idx);
						target_set_monster(m_idx);
						done = TRUE;
					}
					else if ((mode & (TARGET_TRAP)) && target_able_trap(y, x))
 					{
 						target_set_location(y, x);
 						done = TRUE;
 					}
					else if (mode & (TARGET_PROBE))
					{
					 	target_set_location(y, x);
					 	done = TRUE;
					}
					else
					{
						bell("Illegal target!");
					}
					break;
				}

				case 'g':
				{
					cmd_insert(CMD_PATHFIND, y, x);
					done = TRUE;
					break;
				}

				case 'f':
				{
					list_floor_objects = (!list_floor_objects);
					break;
				}

				case '?':
				{
					help = !help;

					/* Redraw main window */
					p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIP);
					Term_clear();
					handle_stuff();
					if (!help)
						prt("'?' - help", help_prompt_loc, 0);

					break;
				}

				default:
				{
					/* Extract direction */
					d = target_dir(query.key);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				int old_y = temp_y[m];
				int old_x = temp_x[m];

				/* Find a new monster */
				i = target_pick(old_y, old_x, ddy[d], ddx[d]);

				/* Scroll to find interesting grid */
				if (i < 0)
				{
					int old_wy = Term->offset_y;
					int old_wx = Term->offset_x;

					/* Change if legal */
					if (change_panel(d))
					{
						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Find a new monster */
						i = target_pick(old_y, old_x, ddy[d], ddx[d]);

						/* Restore panel if needed */
						if ((i < 0) && modify_panel(Term, old_wy, old_wx))
						{
							/* Recalculate interesting grids */
							target_set_interactive_prepare(mode);
						}

						/* Handle stuff */
						handle_stuff();
					}
				}

				/* Use interesting grid if found */
				if (i >= 0) m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			bool path_drawn = FALSE;

			/* Dummy pointers to send to project_path */
			int yy = y;
			int xx = x;

			/* Don't need this button any more */
			button_kill('o');

			/* Update help */
			if (help)
			{
				bool good_target = ((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x]));
				target_display_help(good_target, !(flag && temp_n));
			}

			/* Default prompt */
			if (!(mode & (TARGET_GRID)))
			{
				strcpy(info, "q,t,f,p,m,+,-,<dir>");
			}

			/* Disable monster selection */
			else
			{
				strcpy(info, "q,t,f.p,+,-,<dir>");
			}

			/* Find the path. */
			path_n = project_path(path_g, path_gx, MAX_RANGE, py, px, &yy, &xx, PROJECT_THRU);

			/* Draw the path in "target" mode. If there is one */
			if ((mode & (TARGET_KILL)) && (cave_info[y][x] & (CAVE_FIRE)))
			{
				/* Save target info */
				path_drawn = draw_path(path_n, path_g, path_char, path_attr, py, px, y, x);
			}

			event_signal(EVENT_MOUSEBUTTONS);

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(y, x, (mode | TARGET_LOOK), info, list_floor_objects);

			/* Remove the path */
			if (path_drawn)  load_path(path_n, path_g, path_char, path_attr);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query.key)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				case '-':
				{
					break;
				}

				case 'p':
				{
					/* Recenter around player */
					verify_panel();

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;

					break;
				}

				case 'o':
				{
					break;
				}

				case 'm':
				{
					/* Monster selection is disabled */
					if (mode & (TARGET_GRID)) break;

					flag = TRUE;

					m = 0;
					bd = 999;

					/* Pick a nearby monster */
					for (i = 0; i < temp_n; i++)
					{
						t = distance(y, x, temp_y[i], temp_x[i]);

						/* Pick closest */
						if (t < bd)
						{
							m = i;
							bd = t;
						}
					}

					/* Nothing interesting */
					if (bd == 999) flag = FALSE;

					break;
				}

				case DEFINED_XFF:
				{

					/* We only target if we click somewhere where the cursor
					   is already (i.e. a double-click without a time limit) */
					if (KEY_GRID_X(query) == x && KEY_GRID_Y(query) == y)
					{
						/* Make an attempt to target the monster on the given
						   square rather than the square itself (it seems this
						   is the more likely intention of clicking on a
						   monster). */
						int m_idx = cave_m_idx[y][x];

						if ((m_idx > 0) && target_able(m_idx))
						{
							health_track(m_idx);
							target_set_monster(m_idx);
						}
						else
						{
							/* There is no monster, or it isn't targettable,
							   so target the location instead. */
							target_set_location(y, x);
						}

						done = TRUE;
					}
					else
					{
						/* Just move the cursor for now - another click will
						   target. */
						x = KEY_GRID_X(query);
						y = KEY_GRID_Y(query);
					}
					break;
				}

				case 't':
				case '5':
				case '0':
				case '.':
				{
					target_set_location(y, x);
					done = TRUE;
					break;
				}

				case 'g':
				{
					cmd_insert(CMD_PATHFIND, y, x);
					done = TRUE;
					break;
				}

				case 'f':
				{
					list_floor_objects = (!list_floor_objects);
				}

				case '?':
				{
					help = !help;

					/* Redraw main window */
					p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIP);
					Term_clear();
					handle_stuff();
					if (!help)
						prt("'?' - help.", help_prompt_loc, 0);

					break;
				}


				default:
				{
					/* Extract a direction */
					d = target_dir(query.key);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				int dungeon_hgt = p_ptr->cur_map_hgt;
				int dungeon_wid = p_ptr->cur_map_wid;

				/* Move */
				x += ddx[d];
				y += ddy[d];

				/* Slide into legality */
				if (x >= dungeon_wid - 1) x--;
				else if (x <= 0) x++;

				/* Slide into legality */
				if (y >= dungeon_hgt - 1) y--;
				else if (y <= 0) y++;

				/* Adjust panel if needed */
				if (adjust_panel(y, x))
				{
					/* Handle stuff */
					handle_stuff();

					/* Recalculate interesting grids */
					target_set_interactive_prepare(mode);

				}
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Redraw as necessary */
	if (help)
	{
		p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIP);
		Term_clear();
	}
	else
	{
		prt("", 0, 0);
		prt("", help_prompt_loc, 0);
		p_ptr->redraw |= (PR_DEPTH | PR_STATUS);
	}



	/* Recenter around player */
	verify_panel();

	/* Restore buttons */
	button_restore();

	/* Handle stuff */
	handle_stuff();

	/* Re-set animate flicker */
	animate_flicker = temp_animate_flicker;

	/* Failure to set target */
	if (!p_ptr->target_set) return (FALSE);

	/* Success */
	return (TRUE);
}


