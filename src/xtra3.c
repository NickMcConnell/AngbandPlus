/*
 * File: xtra3.c
 * Purpose: Handles the setting up updating, and cleaning up of the various 
 *          things that are displayed by the game.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Antony Sidwell
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
#include "game-event.h"
#include "option.h"
#include "wind_flg.h"

#include "keypad.h"

/*
 * Yet more bit flags for the "p_ptr->window" variable (etc)
 *
 * Ultimately, this is the only file that should need them.
 */
#define PW_PLAYER_0         0x00000004L /* Display player (basic) */
#define PW_PLAYER_1         0x00000008L /* Display player (extra) */
#define PW_PLAYER_2         0x00000010L /* Display player (compact) */
#define PW_MESSAGE          0x00000040L /* Display messages */
#define PW_OVERHEAD         0x00000080L /* Display overhead view */
#define PW_MONSTER          0x00000100L /* Display monster recall */
#define PW_OBJECT           0x00000200L /* Display object recall */
#define PW_MONLIST          0x00000400L /* Display monster list */
#ifdef 	ALLOW_BORG
#define PW_BORG_1           0x00004000L /* Display borg messages */
#define PW_BORG_2           0x00008000L /* Display borg status */
#endif

/* 
 * There are a few functions installed to be triggered by several 
 * of the basic player events.  For convenience, these have been grouped 
 * in this list.
 */
game_event_type player_events[] =
{
	EVENT_RACE_CLASS,
	EVENT_PLAYERTITLE,
	EVENT_EXPERIENCE,
	EVENT_PLAYERLEVEL,
	EVENT_GOLD,
	EVENT_EXPERIENCE,
	EVENT_EQUIPMENT,
	EVENT_STATS,
	EVENT_AC,
	EVENT_MANA,
	EVENT_HP,
	EVENT_MONSTERHEALTH,
	EVENT_SPEED,
	EVENT_DUNGEONLEVEL,
};

game_event_type statusline_events[] =
{
	EVENT_STATE,
	EVENT_STATUS,
	EVENT_DETECTIONSTATUS,
	EVENT_STUDYSTATUS
};

/**
 *  Counts windows that match a given bitflag map at all
 */
size_t
player_other::count_flagged_windows(u32b flag_test) const
{
	size_t count = 0;

	/* Scan windows */
	size_t i = ANGBAND_TERM_MAX;
	do	{
		if (!angband_term[--i]) continue;	/* Unused */
		if (window_flag[i] & flag_test) ++count;
		}
	while(0 < i);

	return count;
}

/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val, size_t out_len)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 100)
			strnfmt(out_val, out_len, "18/%03d", bonus);
		else
			strnfmt(out_val, out_len, " 18/%02d", bonus);
	}

	/* From 3 to 18 */
	else
	{
		strnfmt(out_val, out_len, "    %2d", val);
	}
}

/*
 * Perform the minimum "whole panel" adjustment to ensure that the given
 * location is contained inside the current panel, and return TRUE if any
 * such adjustment was performed.
 */
bool adjust_panel(coord g)
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
		if (use_bigtile) screen_wid /= 2;

		/* Adjust as needed */
		while ((int)(g.y) >= wy + screen_hgt) wy += screen_hgt / 2;
		while ((int)(g.y) < wy) wy -= screen_hgt / 2;

		/* Adjust as needed */
		while ((int)(g.x) >= wx + screen_wid) wx += screen_wid / 2;
		while ((int)(g.x) < wx) wx -= screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
}


/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
bool change_panel(int dir)
{
	bool changed = FALSE;
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		int screen_hgt, screen_wid;
		int wx, wy;

		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & PW_MAP)) continue;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		/* Shift by half a panel */
		wy = t->offset_y + ddy[dir] * screen_hgt / 2;
		wx = t->offset_x + ddx[dir] * screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
}


/*
 * Verify the current panel (relative to the player location).
 *
 * By default, when the player gets "too close" to the edge of the current
 * panel, the map scrolls one panel in that direction so that the player
 * is no longer so close to the edge.
 *
 * The "center_player" option allows the current panel to always be centered
 * around the player, which is very expensive, and also has some interesting
 * gameplay ramifications.
 */
static void verify_panel(void)
{
	int wy, wx;
	int screen_hgt, screen_wid;

	int panel_wid, panel_hgt;

	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & (PW_MAP))) continue;

		wy = t->offset_y;
		wx = t->offset_x;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		panel_wid = screen_wid / 2;
		panel_hgt = screen_hgt / 2;

		/* Scroll screen vertically when off-center */
		if (OPTION(center_player) && (!p_ptr->running || !OPTION(run_avoid_center)) &&
		    (py != wy + panel_hgt))
		{
			wy = py - panel_hgt;
		}

		/* Scroll screen vertically when 3 grids from top/bottom edge */
		else if ((py < wy + 3) || (py >= wy + screen_hgt - 3))
		{
			wy = py - panel_hgt;
		}


		/* Scroll screen horizontally when off-center */
		if (OPTION(center_player) && (!p_ptr->running || !OPTION(run_avoid_center)) &&
		    (px != wx + panel_wid))
		{
			wx = px - panel_wid;
		}

		/* Scroll screen horizontally when 3 grids from left/right edge */ 
		else if ((px < wx + 3) || (px >= wx + screen_wid - 3))
		{
			wx = px - panel_wid;
		}

		/* Scroll if needed */
		modify_panel(t, wy, wx);
	}
}

/* ------------------------------------------------------------------------
 * Sidebar display functions
 * ------------------------------------------------------------------------ */

/*
 * Print character info at given row, column in a 13 char field
 */
static void prt_field(const char* const info, int row, int col)
{
	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", row, col);

	/* Dump the info itself */
	c_put_str(TERM_L_BLUE, info, row, col);
}


/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat, int row, int col)
{
	char tmp[32];

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		put_str(stat_names_reduced[stat], row, col);
		cnv_stat(p_ptr->stat_use[stat], tmp, sizeof(tmp));
		c_put_str(TERM_YELLOW, tmp, row, col + 6);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], row, col);
		cnv_stat(p_ptr->stat_use[stat], tmp, sizeof(tmp));
		c_put_str(TERM_L_GREEN, tmp, row, col + 6);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18+100)
	{
		put_str("!", row, col + 3);
	}
}


/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(int row, int col)
{
	prt_field(p_ptr->title(), row, col);
}


/*
 * Prints level
 */
static void prt_level(int row, int col)
{
	char tmp[32];

	strnfmt(tmp, sizeof(tmp), "%6d", p_ptr->lev);

	if (p_ptr->lev >= p_ptr->max_lev)
	{
		put_str("LEVEL ", row, col);
		c_put_str(TERM_L_GREEN, tmp, row, col + 6);
	}
	else
	{
		put_str("Level ", row, col);
		c_put_str(TERM_YELLOW, tmp, row, col + 6);
	}
}


/*
 * Display the experience
 */
static void prt_exp(int row, int col)
{
	char out_val[32];
	bool lev50 = (p_ptr->lev == 50);

	long xp = (long)p_ptr->exp;


	/* Calculate XP for next level */
	if (!lev50)
		xp = (long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L) - p_ptr->exp;

	/* Format XP */
	strnfmt(out_val, sizeof(out_val), "%8ld", (long)xp);


	if (p_ptr->exp >= p_ptr->max_exp)
	{
		put_str((lev50 ? "EXP" : "NXT"), row, col);
		c_put_str(TERM_L_GREEN, out_val, row, col + 4);
	}
	else
	{
		put_str((lev50 ? "Exp" : "Nxt"), row, col);
		c_put_str(TERM_YELLOW, out_val, row, col + 4);
	}
}


/*
 * Prints current gold
 */
static void prt_gold(int row, int col)
{
	char tmp[32];

	put_str("AU ", row, col);
	strnfmt(tmp, sizeof(tmp), "%9ld", (long)p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, row, col + 3);
}


/*
 * Equippy chars
 */
static void prt_equippy(int row, int col)
{
	int i;

	byte a;
	char c;

	/* No equippy chars in bigtile mode */
	if (use_bigtile) return;

	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Object */
		const object_type* const o_ptr = &p_ptr->inventory[i];

		a = o_ptr->attr_user();
		c = o_ptr->char_user();

		/* Clear the part of the screen */
		if (!o_ptr->k_idx)
		{
			c = ' ';
			a = TERM_WHITE;
		}

		/* Dump */
		Term_putch(col + i - INVEN_WIELD, row, a, c);
	}
}


/*
 * Prints current AC
 */
static void prt_ac(int row, int col)
{
	char tmp[32];

	put_str("Cur AC ", row, col);
	strnfmt(tmp, sizeof(tmp), "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, row, col + 7);
}


/*
 * Prints Cur hit points
 */
static void prt_hp(int row, int col)
{
	char cur_hp[32], max_hp[32];
	byte color;

	put_str("HP ", row, col);

	strnfmt(max_hp, sizeof(max_hp), "%4d", p_ptr->mhp);
	strnfmt(cur_hp, sizeof(cur_hp), "%4d", p_ptr->chp);

	if (p_ptr->chp >= p_ptr->mhp)
		color = TERM_L_GREEN;
	else if (p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
		color = TERM_YELLOW;
	else
		color = TERM_RED;

	c_put_str(color, cur_hp, row, col + 3);
	c_put_str(TERM_WHITE, "/", row, col + 7);
	c_put_str(TERM_L_GREEN, max_hp, row, col + 8);
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(int row, int col)
{
	char cur_sp[32], max_sp[32];
	byte color;

	/* Do not show mana unless it matters */
	if (!p_ptr->spell_book()) return;

	put_str("SP ", row, col);

	strnfmt(max_sp, sizeof(max_sp), "%4d", p_ptr->msp);
	strnfmt(cur_sp, sizeof(cur_sp), "%4d", p_ptr->csp);

	if (p_ptr->csp >= p_ptr->msp)
		color = TERM_L_GREEN;
	else if (p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
		color = TERM_YELLOW;
	else
		color = TERM_RED;

	/* Show mana */
	c_put_str(color, cur_sp, row, col + 3);
	c_put_str(TERM_WHITE, "/", row, col + 7);
	c_put_str(TERM_L_GREEN, max_sp, row, col + 8);
}

void
agent_type::stars_color(int& stars, byte& attr) const
{
	/* Extract the "percent" of health */
	int pct = 100L * (long)chp / (long)mhp;

	/* Default to almost dead */
	attr = TERM_RED;

	/* Badly wounded */
	if (pct >= 10) attr = TERM_L_RED;

	/* Wounded */
	if (pct >= 25) attr = TERM_ORANGE;

	/* Somewhat Wounded */
	if (pct >= 60) attr = TERM_YELLOW;

	/* Healthy */
	if (pct >= 100) attr = TERM_L_GREEN;

	/* Convert percent into "health" */
	stars = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;
}

/*
 * Redraw the "monster health bar"
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.  When nothing
 * is being tracked, we clear the health bar.  If the monster being
 * tracked is not currently visible, a special health bar is shown.
 */
static void prt_health(int row, int col)
{
	/* Not tracking */
	if (!p_ptr->health_who)
	{
		/* Erase the health bar */
		Term_erase(col, row, 12);
		return;
	}

	const monster_type* const m_ptr = m_ptr_from_m_idx(p_ptr->health_who);

	/* Tracking an unseen monster */
	if (!m_ptr->ml)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->timed[TMD_IMAGE])
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a dead monster (?) */
	else if (0 > m_ptr->chp)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int len;
		byte attr = TERM_RED;

		/* Get stars and color for health bar */
		m_ptr->stars_color(len, attr);

		/* Afraid */
		if (m_ptr->monfear) attr = TERM_VIOLET;

		/* Confused */
		if (m_ptr->confused) attr = TERM_UMBER;

		/* Stunned */
		if (m_ptr->stunned) attr = TERM_L_BLUE;

		/* Asleep */
		if (m_ptr->csleep) attr = TERM_BLUE;

		/* Default to "unknown" */
		Term_putstr(col, row, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(col + 1, row, len, attr, "**********");
	}
}

/*
 * Redraw maximum monster hit points.
 */
static void prt_max_mon_hp(int row, int col)
{
	if (   !p_ptr->health_who						/* Not tracking */
		|| p_ptr->timed[TMD_IMAGE])					/* Hallucinatory */
	{
BlankLine:
		/* Erase the health bar */
		Term_erase(col, row, 12);
		return;
	}

	const monster_type* const m_ptr = m_ptr_from_m_idx(p_ptr->health_who);
	if (   !m_ptr->ml			/* Unseen */
		|| (0 > m_ptr->chp))	/* Dead */
		goto BlankLine;

	/* Tracking a visible monster */
	else
	{
		char tmp[32];

		sprintf(tmp, "M. MHP %5d", (int)(m_ptr->mhp));
		Term_putstr(col, row, 12, TERM_WHITE, tmp);
	}
}

/*
 * Redraw the move ratio.
 */
static void prt_move_ratio(int row, int col)
{
	if (   !p_ptr->health_who						/* Not tracking */
		|| p_ptr->timed[TMD_IMAGE])					/* Hallucinatory */
	{
BlankLine:
		/* Erase the health bar */
		Term_erase(col, row, 12);
		return;
	}

	const monster_type* const m_ptr = m_ptr_from_m_idx(p_ptr->health_who);
	if (   !m_ptr->ml			/* Unseen */
		|| (0 > m_ptr->chp))	/* Dead */
		goto BlankLine;

	/* Tracking a visible monster */
	else
	{
		char tmp[32];
		int player_moves = 1;
		int monster_moves = 1;

		/* calculate move ratio */
		p_ptr->move_ratio(monster_moves, player_moves, *m_ptr, 0, 0);
		if (9<player_moves) player_moves = 9;
		if (9<monster_moves) monster_moves = 9;

		sprintf(tmp, "Nxt Turn %d/%d", player_moves, monster_moves);
		Term_putstr(col, row, 12, TERM_WHITE, tmp);
	}
}

static void prt_energy(int row, int col)
{
	Term_erase(col, row, 12);		/* Erase the energy line */
	put_str("Energy ", row, col);


	int i = p_ptr->speed;

	byte attr = TERM_L_GREEN;
	char buf[32] = "";

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	if (   !p_ptr->health_who						/* Not tracking */
		|| p_ptr->timed[TMD_IMAGE])					/* Hallucinatory */
	{
NoTarget:
		/* Fast */
		if (i > 110)
		{
			attr = TERM_GREEN;
		}

		/* Slow */
		else if (i < 110)
		{
			attr = TERM_L_UMBER;
		};

		sprintf(buf, "%2d", (int)(extract_energy[i]));

		/* Display the speed */
		c_put_str(attr, buf, row, col+7);
		return;
	}


	const monster_type* const m_ptr = m_ptr_from_m_idx(p_ptr->health_who);
	if (   !m_ptr->ml			/* Unseen */
		|| (0 > m_ptr->chp))	/* Dead */
		goto NoTarget;
		
	/* Tracking a visible monster */
	else
	{
		/* Fast */
		if (i > m_ptr->speed)
		{
			attr = TERM_GREEN;
		}

		/* Slow */
		else if (i < m_ptr->speed)
		{
			attr = TERM_L_UMBER;
		};

		sprintf(buf, "%d/%d", (int)(extract_energy[i]), (int)(extract_energy[m_ptr->speed]));

		/* Display the energy ratio */
		c_put_str(attr, buf, row, col+7);
	}
}

/*
 * Prints the speed of a character.
 */
static void prt_speed(int row, int col)
{
	int i = p_ptr->speed;

	byte attr = TERM_WHITE;
	const char *type = NULL;
	char buf[32] = "";

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	/* Fast */
	if (i > 110)
	{
		attr = TERM_L_GREEN;
		type = "Fast";
	}

	/* Slow */
	else if (i < 110)
	{
		attr = TERM_L_UMBER;
		type = "Slow";
	}

	if (type)
		strnfmt(buf, sizeof(buf), "%s (%+d)", type, (i - 110));

	/* Display the speed */
	c_put_str(attr, format("%-10s", buf), row, col);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(int row, int col)
{
	char depths[32];

	if (!p_ptr->depth)
	{
		my_strcpy(depths, "Town", sizeof(depths));
	}
	else if (OPTION(depth_in_feet))
	{
		strnfmt(depths, sizeof(depths), "%d ft", p_ptr->depth * 50);
	}
	else
	{
		strnfmt(depths, sizeof(depths), "Lev %d", p_ptr->depth);
	}

	/* Right-Adjust the "depth", and clear old values */
	put_str(format("%-13s", depths), row, col);
}




/* Some simple wrapper functions */
static void prt_str(int row, int col) { prt_stat(A_STR, row, col); }
static void prt_dex(int row, int col) { prt_stat(A_DEX, row, col); }
static void prt_wis(int row, int col) { prt_stat(A_WIS, row, col); }
static void prt_int(int row, int col) { prt_stat(A_INT, row, col); }
static void prt_con(int row, int col) { prt_stat(A_CON, row, col); }
static void prt_chr(int row, int col) { prt_stat(A_CHR, row, col); }
static void prt_race(int row, int col) { prt_field(p_ptr->racename(), row, col); }
static void prt_class(int row, int col) { prt_field(p_ptr->classname(), row, col); }


/*
 * Struct of sidebar handlers.
 */
static const struct side_handler_t
{
	void (*hook)(int, int);	 /* int row, int col */
	int priority;		 /* 1 is most important (always displayed) */
	game_event_type type;	 /* PR_* flag this corresponds to */
} side_handlers[] =
{
	{ prt_race,    19, EVENT_RACE_CLASS },
	{ prt_title,   18, EVENT_PLAYERTITLE },
	{ prt_class,   20, EVENT_RACE_CLASS },
	{ prt_level,   10, EVENT_DUNGEONLEVEL },
	{ prt_exp,     16, EVENT_EXPERIENCE },
	{ prt_gold,    11, EVENT_GOLD },
	{ prt_equippy, 17, EVENT_EQUIPMENT },
	{ prt_str,      1, EVENT_STATS },
	{ prt_int,      2, EVENT_STATS },
	{ prt_wis,      3, EVENT_STATS },
	{ prt_dex,      4, EVENT_STATS },
	{ prt_con,      5, EVENT_STATS },
	{ prt_chr,      6, EVENT_STATS },
	{ prt_ac,       7, EVENT_AC },
	{ prt_hp,       8, EVENT_HP },
	{ prt_sp,       9, EVENT_MANA },
	{ prt_health,  12, EVENT_MONSTERHEALTH },
	{ prt_max_mon_hp,	13, EVENT_MONSTERHEALTH },
	{ NULL,        22, EVENT_NO_CHANGE },
	{ prt_move_ratio,	21, EVENT_MONSTERHEALTH },
	{ prt_energy,  15, EVENT_SPEED },
	{ prt_speed,   14, EVENT_SPEED }, /* Slow (-NN) / Fast (+NN) */
	{ prt_depth,    1, EVENT_DUNGEONLEVEL }, /* Lev NNN / NNNN ft */	/* status line starts at col 13 */
};


/*
 * This prints the sidebar, using a clever method which means that it will only
 * print as much as can be displayed on <24-line screens.
 *
 * Each row is given a priority; the least important higher numbers and the most
 * important lower numbers.  As the screen gets smaller, the rows start to
 * disappear in the order of lowest to highest importance.
 */
static void update_sidebar(game_event_type type, game_event_data *data, void *user)
{
	int x, y, row;
	int max_priority;
	size_t i;


	Term_get_size(&x, &y);

	/* Keep the top and bottom lines clear. */
	max_priority = y - 2;

	/* Display list entries */
	for (i = 0, row = 1; i < N_ELEMENTS(side_handlers); i++)
	{
		const struct side_handler_t *hnd = &side_handlers[i];
		int priority = hnd->priority;
		bool from_bottom = FALSE;

		/* Negative means print from bottom */
		if (priority < 0)
		{
			priority = -priority;
			from_bottom = TRUE;
		}

		/* If this is high enough priority, display it */
		if (priority <= max_priority)
		{
			if (hnd->type == type && hnd->hook)
			{
				if (from_bottom)
					hnd->hook(Term->hgt - (N_ELEMENTS(side_handlers) - i), 0);
				else
				    hnd->hook(row, 0);
			}

			/* Increment for next time */
			row++;
		}
	}
}

static void hp_colour_change(game_event_type type, game_event_data *data, void *user)
{
	/*
	 * hack:  redraw player, since the player's color
	 * now indicates approximate health.  Note that
	 * using this command when graphics mode is on
	 * causes the character to be a black square.
	 */
//	if ((hp_changes_color) && (arg_graphics == GRAPHICS_NONE))
	if (false)
	{
		lite_spot(p_ptr->loc);
	}
}



/* ------------------------------------------------------------------------
 * Status line display functions
 * ------------------------------------------------------------------------ */

/* Simple macro to initialise structs */
#define S(s)		s, sizeof(s)

/*
 * Struct to describe different timed effects
 */
struct state_info
{
	int value;
	const char *str;
	size_t len;
	byte attr;
};

/* TMD_CUT descriptions */
static const struct state_info cut_data[] =
{
	{ 1000, S("Mortal wound"), TERM_L_RED },
	{  200, S("Deep gash"),    TERM_RED },
	{  100, S("Severe cut"),   TERM_RED },
	{   50, S("Nasty cut"),    TERM_ORANGE },
	{   25, S("Bad cut"),      TERM_ORANGE },
	{   10, S("Light cut"),    TERM_YELLOW },
	{    0, S("Graze"),        TERM_YELLOW },
};

/* TMD_STUN descriptions */
static const struct state_info stun_data[] =
{
	{   100, S("Knocked out"), TERM_RED },
	{    50, S("Heavy stun"),  TERM_ORANGE },
	{     0, S("Stun"),        TERM_ORANGE },
};

/* p_ptr->hunger descriptions */
static const struct state_info hunger_data[] =
{
	{ PY_FOOD_FAINT, S("Faint"),    TERM_RED },
	{ PY_FOOD_WEAK,  S("Weak"),     TERM_ORANGE },
	{ PY_FOOD_ALERT, S("Hungry"),   TERM_YELLOW },
	{ PY_FOOD_FULL,  S(""),         TERM_L_GREEN },
	{ PY_FOOD_MAX,   S("Full"),     TERM_L_GREEN },
	{ PY_FOOD_UPPER, S("Gorged"),   TERM_GREEN },
};

/* For the various TMD_* effects */
static const struct state_info effects[] =
{
	{ TMD_BLIND,     S("Blind"),      TERM_ORANGE },
	{ TMD_PARALYZED, S("Paralyzed!"), TERM_RED },
	{ TMD_CONFUSED,  S("Confused"),   TERM_ORANGE },
	{ TMD_AFRAID,    S("Afraid"),     TERM_ORANGE },
	{ TMD_IMAGE,     S("Halluc"),     TERM_ORANGE },
	{ TMD_POISONED,  S("Poisoned"),   TERM_ORANGE },
	{ TMD_PROTEVIL,  S("ProtEvil"),   TERM_L_GREEN },
//	{ TMD_TELEPATHY, S("ESP"),        TERM_L_BLUE },
	{ TMD_INVULN,    S("Invuln"),     TERM_L_GREEN },
	{ TMD_HERO,      S("Hero"),       TERM_L_GREEN },
	{ TMD_SHERO,     S("Berserk"),    TERM_L_GREEN },
	{ TMD_SHIELD,    S("Shield"),     TERM_L_GREEN },
	{ TMD_BLESSED,   S("Blssd"),      TERM_L_GREEN },
	{ TMD_SINVIS,    S("SInvis"),     TERM_L_GREEN },
	{ TMD_SINFRA,    S("Infra"),      TERM_L_GREEN },
	{ TMD_OPP_ACID,  S("RAcid"),      TERM_SLATE },
	{ TMD_OPP_ELEC,  S("RElec"),      TERM_BLUE },
	{ TMD_OPP_FIRE,  S("RFire"),      TERM_RED },
	{ TMD_OPP_COLD,  S("RCold"),      TERM_WHITE },
	{ TMD_OPP_POIS,  S("RPois"),      TERM_GREEN },
//	{ TMD_AMNESIA,   S("Amnesiac"),   TERM_ORANGE },
};

#define PRINT_STATE(sym, data, index, row, col) \
{ \
	size_t i; \
	\
	for (i = 0; i < N_ELEMENTS(data); i++) \
	{ \
		if (index sym data[i].value) \
		{ \
			if (data[i].str[0]) \
			{ \
				c_put_str(data[i].attr, data[i].str, row, col); \
				return data[i].len; \
			} \
			else \
			{ \
				return 0; \
			} \
		} \
	} \
}


/*
 * Print cut indicator.
 */
static size_t prt_cut(int row, int col)
{
	PRINT_STATE(>, cut_data, p_ptr->timed[TMD_CUT], row, col);
	return 0;
}


/*
 * Print stun indicator.
 */
static size_t prt_stun(int row, int col)
{
	PRINT_STATE(>, stun_data, p_ptr->timed[TMD_STUN], row, col);
	return 0;
}


/*
 * Prints status of hunger
 */
static size_t prt_hunger(int row, int col)
{
	PRINT_STATE(<, hunger_data, p_ptr->food, row, col);
	return 0;
}



/*
 * Prints Searching, Resting, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static size_t prt_state(int row, int col)
{
	byte attr = TERM_WHITE;

	char text[16] = "";


	/* Resting */
	if (p_ptr->resting)
	{
		int i;
		int n = p_ptr->resting;

		/* Start with "Rest" */
		my_strcpy(text, "Rest      ", sizeof(text));

		/* Extensive (timed) rest */
		if (n >= 1000)
		{
			i = n / 100;
			text[9] = '0';
			text[8] = '0';
			text[7] = I2D(i % 10);
			if (i >= 10)
			{
				i = i / 10;
				text[6] = I2D(i % 10);
				if (i >= 10)
				{
					text[5] = I2D(i / 10);
				}
			}
		}

		/* Long (timed) rest */
		else if (n >= 100)
		{
			i = n;
			text[9] = I2D(i % 10);
			i = i / 10;
			text[8] = I2D(i % 10);
			text[7] = I2D(i / 10);
		}

		/* Medium (timed) rest */
		else if (n >= 10)
		{
			i = n;
			text[9] = I2D(i % 10);
			text[8] = I2D(i / 10);
		}

		/* Short (timed) rest */
		else if (n > 0)
		{
			i = n;
			text[9] = I2D(i);
		}

		/* Rest until healed */
		else if (n == -1)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '*';
		}

		/* Rest until done */
		else if (n == -2)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '&';
		}
	}

	/* Repeating */
	else if (p_ptr->command_rep)
	{
		if (p_ptr->command_rep > 999)
			strnfmt(text, sizeof(text), "Rep. %3d00", p_ptr->command_rep / 100);
		else
			strnfmt(text, sizeof(text), "Repeat %3d", p_ptr->command_rep);
	}

	/* Searching */
	else if (p_ptr->searching)
	{
		my_strcpy(text, "Searching ", sizeof(text));
	}

	/* Display the info (or blanks) */
	c_put_str(attr, text, row, col);

	return strlen(text);
}


/*
 * Prints trap detection status
 */
static size_t prt_dtrap(int row, int col)
{
#if 0
	byte info = cave_info2[p_ptr->loc.y][p_ptr->loc.x];

	/* The player is in a trap-detected grid */
	if (info & (CAVE2_DTRAP))
	{
		c_put_str(TERM_GREEN, "DTrap", row, col);
		return 5;
	}
#endif

	return 0;
}


/*
 * Print whether a character is studying or not.
 */
static size_t prt_study(int row, int col)
{
	if (p_ptr->new_spells)
	{
		char *text = format("Study (%d)", p_ptr->new_spells);
		put_str(text, row, col);
		return strlen(text) + 1;
	}

	return 0;
}



/*
 * Print all timed effects.
 */
static size_t prt_tmd(int row, int col)
{
	size_t i, len = 0;

	for (i = 0; i < N_ELEMENTS(effects); i++)
	{
		if (p_ptr->timed[effects[i].value])
		{
			c_put_str(effects[i].attr, effects[i].str, row, col + len);
			len += effects[i].len;
		}
	}

	return len;
}


/* Useful typedef */
typedef size_t status_f(int row, int col);

status_f *status_handlers[] =
{ prt_state, prt_cut, prt_stun, prt_hunger, prt_study, prt_tmd, prt_dtrap };


/*
 * Print the status line.
 */
static void update_statusline(game_event_type type, game_event_data *data, void *user)
{
	int row = Term->hgt - 1;
	int col = 13;
	size_t i;

	/* Clear the remainder of the line */
	prt("", row, col);

	/* Display those which need redrawing */
	for (i = 0; i < N_ELEMENTS(status_handlers); i++)
		col += status_handlers[i](row, col);
}


/* ------------------------------------------------------------------------
 * Map redraw.
 * ------------------------------------------------------------------------ */
#if 0
static void trace_map_updates(game_event_type type, game_event_data *data, void *user)
{
	if (data->point.x == -1 && data->point.y == -1)
	{
		printf("Redraw whole map\n");
	}
	else
	{
		printf("Redraw (%i, %i)\n", data->point.x, data->point.y);
	}
}
#endif

static void update_maps(game_event_type type, game_event_data *data, void *user)
{
	term *t = (term*)user;

	/* This signals a whole-map redraw. */
	if (data->point.x == -1 && data->point.y == -1)
	{
		prt_map();
	}
	/* Single point to be redrawn */
	else
	{
		byte a, ta;
		char c, tc;
		
		int ky, kx;
		int vy, vx;
		
		/* Location relative to panel */
		ky = data->point.y - t->offset_y;
		kx = data->point.x - t->offset_x;

		if (t == angband_term[0])
		{
			/* Verify location */
			if ((ky < 0) || (ky >= SCREEN_HGT)) return;
			
			/* Verify location */
			if ((kx < 0) || (kx >= SCREEN_WID)) return;
			
			/* Location in window */
			vy = ky + ROW_MAP;
			vx = kx + COL_MAP;

			if (use_bigtile) vx += kx;
		}
		else
		{
			if (use_bigtile)
			{
				kx += kx;
				if (kx + 1 >= t->wid) return;
			}
			
			/* Verify location */
			if ((ky < 0) || (ky >= t->hgt)) return;
			if ((kx < 0) || (kx >= t->wid)) return;
			
			/* Location in window */
			vy = ky;
			vx = kx;
		}

		
		/* Redraw the grid spot */
		grid_data(data->point.y, data->point.x).as_text(a, c, ta, tc);

		Term_queue_char(t, vx, vy, a, c, ta, tc);
#if 0
		/* Plot 'spot' updates in light green to make them visible */
		Term_queue_char(t, vx, vy, TERM_L_GREEN, c, ta, tc);
#endif
		
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

/* ------------------------------------------------------------------------
 * Subwindow displays
 * ------------------------------------------------------------------------ */

/* 
 * TRUE when we're supposed to display the equipment in the inventory 
 * window, or vice-versa.
 */
static bool flip_inven = false;

static void update_inven_subwindow(game_event_type type, game_event_data *data,
				       void *user)
{
	term *old = Term;
	term *inv_term = (term*)user;

	/* Activate */
	Term_activate(inv_term);

	if (!flip_inven)
		display_inven();
	else
		display_equip();


	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}

static void update_equip_subwindow(game_event_type type, game_event_data *data,
				   void *user)
{
	term *old = Term;
	term *inv_term = reinterpret_cast<term*>(user);

	/* Activate */
	Term_activate(inv_term);

	if (!flip_inven)
		display_equip();
	else
		display_inven();

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}

/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
	flip_inven = !flip_inven;
}

static void update_monlist_subwindow(game_event_type type, game_event_data *data, void *user)
{
	term *old = Term;
	term *inv_term = (term*)user;

	/* Activate */
	Term_activate(inv_term);

	display_monlist();
	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}


static void update_monster_subwindow(game_event_type type, game_event_data *data, void *user)
{
	term *old = Term;
	term *inv_term = reinterpret_cast<term*>(user);

	/* Activate */
	Term_activate(inv_term);

	/* Display monster race info */
	if (p_ptr->monster_race_idx)
		display_roff(p_ptr->monster_race_idx);

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}

static void update_object_subwindow(game_event_type type, game_event_data *data, void *user)
{
	term *old = Term;
	term *inv_term = reinterpret_cast<term*>(user);

	/* Activate */
	Term_activate(inv_term);

	/* Display object kind info */
	if (p_ptr->object_kind_idx)
		display_koff(p_ptr->object_kind_idx);

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}


static void update_messages_subwindow(game_event_type type, game_event_data *data, void *user)
{
	term *old = Term;
	term *inv_term = reinterpret_cast<term*>(user);

	int i;
	int w, h;
	int x, y;

	const char *msg;

	/* Activate */
	Term_activate(inv_term);

	/* Get size */
	Term_get_size(&w, &h);

	/* Dump messages */
	for (i = 0; i < h; i++)
	{
		byte color = message_color(i);
		u16b count = message_count(i);
		const char *str = message_str(i);

		if (count == 1)
			msg = str;
		else
			msg = format("%s <%dx>", str, count);

		Term_putstr(0, (h - 1) - i, -1, color, msg);


		/* Cursor */
		Term_locate(&x, &y);

		/* Clear to end of line */
		Term_erase(x, y, 255);
	}

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}

static struct minimap_flags
{
	int win_idx;
	bool needs_redraw;
} minimap_data[ANGBAND_TERM_MAX];

static void update_minimap_subwindow(game_event_type type, game_event_data *data, void *user)
{
	minimap_flags* flags = reinterpret_cast<minimap_flags*>(user);

	if (type == EVENT_MAP)
	{
		flags->needs_redraw = TRUE;
	}
	else if (type == EVENT_END)
	{
		term *old = Term;
		term *t = angband_term[flags->win_idx];
		
		/* Activate */
		Term_activate(t);
		
		/* Redraw map */
		display_map(NULL, NULL);
		Term_fresh();
		
		/* Restore */
		Term_activate(old);

		flags->needs_redraw = FALSE;
	}
}


/*
 * Hack -- display player in sub-windows (mode 0)
 */
static void update_player0_subwindow(game_event_type type, game_event_data *data, void *user)
{
	term *old = Term;
	term *inv_term = reinterpret_cast<term*>(user);

	/* Activate */
	Term_activate(inv_term);

	/* Display flags */
	display_player(0);

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}

/*
 * Hack -- display player in sub-windows (mode 1)
 */
static void update_player1_subwindow(game_event_type type, game_event_data *data, void *user)
{
	term *old = Term;
	term *inv_term = reinterpret_cast<term*>(user);

	/* Activate */
	Term_activate(inv_term);

	/* Display flags */
	display_player(1);

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}


/*
 * Display the left-hand-side of the main term, in more compact fashion.
 */
static void update_player_compact_subwindow(game_event_type type, game_event_data *data, void *user)
{
	int row = 0;
	int col = 0;
	int i;

	term *old = Term;
	term *inv_term = reinterpret_cast<term*>(user);

	/* Activate */
	Term_activate(inv_term);

	/* Race and Class */
	prt_field(p_ptr->racename(), row++, col);
	prt_field(p_ptr->classname(), row++, col);

	/* Title */
	prt_title(row++, col);

	/* Level/Experience */
	prt_level(row++, col);
	prt_exp(row++, col);

	/* Gold */
	prt_gold(row++, col);

	/* Equippy chars */
	prt_equippy(row++, col);

	/* All Stats */
	for (i = 0; i < A_MAX; i++) prt_stat(i, row++, col);

	/* Empty row */
	row++;

	/* Armor */
	prt_ac(row++, col);

	/* Hitpoints */
	prt_hp(row++, col);

	/* Spellpoints */
	prt_sp(row++, col);

	/* Monster health */
	prt_health(row++, col);

	/* maximum monster hitpoints */
	prt_max_mon_hp(row++, col);

	/* move ratio */
	prt_move_ratio(row++, col);

	/* Energy */
	prt_energy(row++, col);

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}


static void flush_subwindow(game_event_type type, game_event_data *data, void *user)
{
	term *old = Term;
	term *t = reinterpret_cast<term*>(user);

	/* Activate */
	Term_activate(t);

	Term_fresh();
	
	/* Restore */
	Term_activate(old);
}


static void subwindow_flag_changed(int win_idx, u32b flag, bool new_state)
{
	void (*register_or_deregister)(game_event_type type, game_event_handler *fn, void *user);
	void (*set_register_or_deregister)(game_event_type *type, size_t n_events, game_event_handler *fn, void *user);

	/* Decide whether to register or deregister an event handler */
	if (!new_state)
	{
		register_or_deregister = event_deregister;
		set_register_or_deregister = event_deregister_set;
	}
	else
	{
		register_or_deregister = event_register;
		set_register_or_deregister = event_register_set;
	}

	switch (flag)
	{
		case PW_INVEN:
		{
			register_or_deregister(EVENT_INVENTORY,
					       update_inven_subwindow,
					       angband_term[win_idx]);
			break;
		}

		case PW_EQUIP:
		{
			register_or_deregister(EVENT_EQUIPMENT,
					       update_equip_subwindow,
					       angband_term[win_idx]);
			break;
		}

		case PW_PLAYER_0:
		{
			set_register_or_deregister(player_events, 
						   N_ELEMENTS(player_events),
						   update_player0_subwindow,
						   angband_term[win_idx]);
			break;
		}

		case PW_PLAYER_1:
		{
			set_register_or_deregister(player_events, 
						   N_ELEMENTS(player_events),
						   update_player1_subwindow,
						   angband_term[win_idx]);
			break;
		}

		case PW_PLAYER_2:
		{
			set_register_or_deregister(player_events, 
						   N_ELEMENTS(player_events),
						   update_player_compact_subwindow,
						   angband_term[win_idx]);
			break;
		}

		case PW_MAP:
		{
			register_or_deregister(EVENT_MAP,
					       update_maps,
					       angband_term[win_idx]);

			register_or_deregister(EVENT_END,
					       flush_subwindow,
					       angband_term[win_idx]);
			break;
		}


		case PW_MESSAGE:
		{
			register_or_deregister(EVENT_MESSAGES,
					       update_messages_subwindow,
					       angband_term[win_idx]);
			break;
		}

		case PW_OVERHEAD:
		{
			minimap_data[win_idx].win_idx = win_idx;

			register_or_deregister(EVENT_MAP,
					       update_minimap_subwindow,
					       &minimap_data[win_idx]);

			register_or_deregister(EVENT_END,
					       update_minimap_subwindow,
					       &minimap_data[win_idx]);
			break;
		}

		case PW_MONSTER:
		{
			register_or_deregister(EVENT_MONSTERTARGET,
					       update_monster_subwindow,
					       angband_term[win_idx]);
			break;
		}

		case PW_OBJECT:
		{
			register_or_deregister(EVENT_OBJECTTARGET,
					       update_object_subwindow,
					       angband_term[win_idx]);
			break;
		}

		case PW_MONLIST:
		{
			register_or_deregister(EVENT_MONSTERLIST,
					       update_monlist_subwindow,
					       angband_term[win_idx]);
			break;
		}
	}
}


/*
 * Set the flags for one Term, calling "subwindow_flag_changed" with each flag that
 * has changed setting so that it can do any housekeeping to do with 
 * displaying the new thing or no longer displaying the old one.
 */
static void subwindow_set_flags(int win_idx, u32b new_flags)
{
	term *old = Term;
	int i;

	/* Deal with the changed flags by seeing what's changed */
	for (i = 0; i < 32; i++)
	{
		/* Only process valid flags */
		if (window_flag_desc[i])
		{
			if ((new_flags & (1L << i)) != (op_ptr->window_flag[win_idx] & (1L << i)))
			{
				subwindow_flag_changed(win_idx, (1L << i), (new_flags & (1L << i)) != 0);
			}
		}
	}

	/* Store the new flags */
	op_ptr->window_flag[win_idx] = new_flags;
	
	/* Activate */
	Term_activate(angband_term[win_idx]);
	
	/* Erase */
	Term_clear();
	
	/* Refresh */
	Term_fresh();
			
	/* Restore */
	Term_activate(old);
}

/*
 * Called with an array of the new flags for all the subwindows, in order
 * to set them to the new values, with a chance to perform housekeeping.
 */
void subwindows_set_flags(u32b *new_flags, size_t n_subwindows)
{
	size_t j;

	for (j = 0; j < n_subwindows; j++)
	{
		/* Dead window */
		if (!angband_term[j]) continue;

		/* Ignore non-changes */
		if (op_ptr->window_flag[j] != new_flags[j])
		{
			subwindow_set_flags(j, new_flags[j]);
		}
	}

}

/* ------------------------------------------------------------------------
 * Showing and updating the splash screen.
 * ------------------------------------------------------------------------ */
/*
 * Hack -- Explain a broken "lib" folder and quit (see below).
 */
static void init_angband_aux(const char* const why)
{
	quit_fmt("%s\n\n%s", why,
	         "The 'lib' directory is probably missing or broken.\n"
	         "Perhaps the archive was not extracted correctly.\n"
	         "See the 'readme.txt' file for more information.");
}

/*
 * Hack -- take notes on line 23
 */
static void splashscreen_note(game_event_type type, game_event_data *data, void *user)
{
	Term_erase(0, 23, 255);
	Term_putstr(20, 23, -1, TERM_WHITE, format("[%s]", data->string));
	Term_fresh();
}

static void show_splashscreen(game_event_type type, game_event_data *data, void *user)
{
	int fd;
	FILE *fp;
	char buf[1024];

	/*** Verify the "news" file ***/

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "news.txt");

	/* Attempt to open the file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure */
	if (fd < 0)
	{
		char why[1024];

		/* Message */
		strnfmt(why, sizeof(why), "Cannot access the '%s' file!", buf);

		/* Crash and burn */
		init_angband_aux(why);
	}

	/* Close it */
	fd_close(fd);


	/*** Display the "news" file ***/

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "news.txt");

	/* Open the News file */
	fp = my_fopen(buf, "r");

	/* Dump */
	if (fp)
	{
		int i = 0;

		/* Dump the file to the screen */
		while (0 == my_fgets(fp, buf, sizeof(buf)))
		{
			/* Display and advance */
			Term_putstr(0, i++, -1, TERM_WHITE, buf);
		}

		/* Close */
		my_fclose(fp);
	}

	/* Flush it */
	Term_fresh();

	/*** Verify (or create) the "high score" file ***/

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Attempt to open the high score file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure */
	if (fd < 0)
	{
		/* File type is "DATA" */
		FILE_TYPE(FILE_TYPE_DATA);

		/* Grab permissions */
		safe_setuid_grab();

		/* Create a new high score file */
		fd = fd_make(buf, 644);

		/* Drop permissions */
		safe_setuid_drop();

		/* Failure */
		if (fd < 0)
		{
			char why[1024];

			/* Message */
			strnfmt(why, sizeof(why), "Cannot create the '%s' file!", buf);

			/* Crash and burn */
			init_angband_aux(why);
		}
	}

	/* Close it */
	fd_close(fd);
}


/* ------------------------------------------------------------------------
 * Temporary (hopefully) hackish solutions.
 * ------------------------------------------------------------------------ */
static void check_panel(game_event_type type, game_event_data *data, void *user)
{
	verify_panel();
}

/* ------------------------------------------------------------------------
 * Initialising
 * ------------------------------------------------------------------------ */
void init_display(void)
{
	/* Because of the "flexible" sidebar, all these things trigger
	   the same function. */
	event_register_set(player_events, N_ELEMENTS(player_events),
			      update_sidebar, NULL);

	/* The flexible statusbar has similar requirements, so is
	   also trigger by a large set of events. */
	event_register_set(statusline_events, N_ELEMENTS(statusline_events),
			      update_statusline, NULL);

	/* Player HP can optionally change the colour of the '@' now. */
	event_register(EVENT_HP, hp_colour_change, NULL);

	/* Simplest way to keep the map up to date - will do for now */
	event_register(EVENT_MAP, update_maps, angband_term[0]);
#if 0
	event_register(EVENT_MAP, trace_map_updates, angband_term[0]);
#endif
	/* Check if the panel should shift when the player's moved */
	event_register(EVENT_PLAYERMOVED, check_panel, NULL);

	/* Set up our splashscreen handlers */
	event_register(EVENT_ENTER_INIT, show_splashscreen, NULL);
	event_register(EVENT_INITSTATUS, splashscreen_note, NULL);
}
