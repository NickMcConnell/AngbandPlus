/* File: cave.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Kenneth 'Bessarion' Boyd
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
#include "raceflag.h"
#include "wind_flg.h"

#include "flow.h"
#include "keypad.h"

/*
 * Support for Adam Bolt's tileset, lighting and transparency effects
 * by Robert Ruehlmann (rr9@thangorodrim.net)
 */

/**
 * Angband has three different "line of sight" type concepts, including this
 * function (which is used almost nowhere), the "project()" method (which
 * is used for determining the paths of projectables and spells and such),
 * and the "update_view()" concept (which is used to determine which grids
 * are "viewable" by the player, which is used for many things, such as
 * determining which grids are illuminated by the player's torch, and which
 * grids and monsters can be "seen" by the player, etc).
 *
 * Zaiband defines all of these in terms of project.
 */
bool los(coord g1, coord g2)
{
	coord grid_g[MAX(DUNGEON_HGT,DUNGEON_WID)];
	int grid_n = project_path(grid_g, MAX(DUNGEON_HGT,DUNGEON_WID), g1, g2, true, &wall_stop);	/* Check the projection path */
	if (0==grid_n) return TRUE;
	if (grid_g[grid_n-1]==g2) return TRUE;
	return FALSE;
}




/*
 * Returns true if the player's grid is dark
 */
bool no_lite(void)
{
	return (!player_can_see_bold(p_ptr->loc.y, p_ptr->loc.x));
}




/**
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
		if (o_ptr->is_artifact()) return (FALSE);
	}

	/* Accept */
	return (TRUE);
}


/**
 * Hack -- Hallucinatory monster
 */
static u16b hallucinatory_monster(void)
{
	monster_race *r_ptr;
	
	byte a;
	char c;
	
	while (1)
	{
		/* Select a random monster */
		r_ptr = &monster_type::r_info[rand_int(z_info->r_max)];
		
		/* Skip non-entries */
		if (!r_ptr->_name) continue;
		
		/* Retrieve attr/char */
		a = r_ptr->x_attr;
		c = r_ptr->x_char;
		
		/* Encode */
		return (PICT(a,c));
	}
}


/**
 * Hack -- Hallucinatory object
 */
static u16b hallucinatory_object(void)
{
	object_kind *k_ptr;
	
	byte a;
	char c;
	
	while (1)
	{
		/* Select a random object */
		k_ptr = &object_type::k_info[rand_int(z_info->k_max - 1) + 1];
		
		/* Skip non-entries */
		if (!k_ptr->_name) continue;
		
		/* Retrieve attr/char (HACK - without flavors) */
		a = k_ptr->x_attr;
		c = k_ptr->x_char;
		
		/* HACK - Skip empty entries */
		if ((a == 0) || (c == 0)) continue;
		
		/* Encode */
		return (PICT(a,c));
	}
}

/**
 * The 16x16 tile of the terrain supports lighting
 */
bool feat_supports_lighting(int feat)
{
	/* Pseudo graphics don't support lighting */
	if (use_graphics == GRAPHICS_PSEUDO) return FALSE;

	if ((use_graphics != GRAPHICS_DAVID_GERVAIS) &&
	    (feat >= FEAT_TRAP_HEAD) && (feat <= FEAT_TRAP_TAIL))
	{
		return TRUE;
	}

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

/**
 * This function modifies the attr/char pair for an empty floor space
 * to reflect the various lighting options available.
 *
 * For text, this means changing the colouring for view_yellow_lite or
 * view_bright_lite, and for graphics it means modifying the char to
 * use a different tile in the tileset.  These modifications are different
 * for different sets, depending on the tiles available, and their position 
 * in the set.
 */
static void special_lighting_floor(byte *a, char *c, enum grid_light_level lighting, bool in_view)
{
	/* The floor starts off "lit" - i.e. rendered in white or the default 
	 * tile. */

	if (lighting == LIGHT_TORCH && OPTION(view_yellow_lite))
	{
		/* 
		 * OPTION(view_yellow_lite) distinguishes between torchlit and 
		 * permanently-lit areas 
		 */
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
	else if (lighting == LIGHT_DARK)
	{
		/* Use a dark tile */
		switch (use_graphics)
		{
			case GRAPHICS_NONE:
			case GRAPHICS_PSEUDO:
				/* Use "dark gray" */
				if (*a == TERM_WHITE) *a = TERM_L_DARK;
				break;
			case GRAPHICS_ADAM_BOLT:
			case GRAPHICS_DAVID_GERVAIS:
				*c += 1;
				break;
		}
	}
	else
	{
		/* 
		 * view_bright_lite makes tiles that aren't in the "eyeline" 
		 * of the player show up dimmer than those that are.
		 */
		if (OPTION(view_bright_lite) && !in_view)
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
}

/**
 * This function modifies the attr/char pair for a wall (or other "interesting"
 * grids to show them as more-or-less lit.  Note that how walls are drawn 
 * isn't directly related to how they are lit - walls are always "lit".
 * The lighting effects we use are as a visual cue to emphasise blindness 
 * and to show field-of-view (OPTION(view_bright_lite)).
 *
 * For text, we change the attr and for graphics we modify the char to
 * use a different tile in the tileset.  These modifications are different
 * for different sets, depending on the tiles available, and their position 
 * in the set.
 */
static void special_wall_display(byte *a, char *c, bool in_view, int feat)
{
	/* Grids currently in view are left alone, rendered as "white" */
	if (in_view) return;

	/* When blind, we make walls and other "white" things dark */
	if (p_ptr->timed[TMD_BLIND])
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

	/* Handle "OPTION(view_bright_lite)" by dimming walls not "in view" */
	else if (OPTION(view_bright_lite))
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


/**
 * This function takes a pointer to a grid info struct describing the 
 * contents of a grid location (as obtained through the grid_data constructor)
 * and fills in the character and attr pairs for display.
 *
 * ap and cp are filled with the attr/char pair for the monster, object or 
 * floor tile that is at the "top" of the grid (monsters covering objects, 
 * which cover floor, assuming all are present).
 *
 * tap and tcp are filled with the attr/char pair for the floor, regardless
 * of what is on it.  This can be used by graphical displays with
 * transparency to place an object onto a floor tile, is desired.
 *
 * Any lighting effects are also applied to these pairs, clear monsters allow
 * the underlying colour or feature to show through (ATTR_CLEAR and
 * CHAR_CLEAR), multi-hued colour-changing (ATTR_MULTI) is applied, and so on.
 * Technically, the flag "CHAR_MULTI" is supposed to indicate that a monster 
 * looks strange when examined, but this flag is currently ignored.
 *
 * NOTES:
 * This is called pretty frequently, whenever a grid on the map display
 * needs updating, so don't overcomplicate it.
 *
 * The "zero" entry in the feature/object/monster arrays are
 * used to provide "special" attr/char codes, with "monster zero" being
 * used for the player attr/char, "object zero" being used for the "pile"
 * attr/char, and "feature zero" being used for the "darkness" attr/char.
 *
 * TODO:
 * The transformations for tile colors, or brightness for the 16x16
 * tiles should be handled differently.  One possibility would be to
 * extend feature_type with attr/char definitions for the different states.
 * This will probably be done outside of the current text->graphics mappings
 * though.
 */
void
grid_data::as_text(byte& ap, char& cp, byte& tap, char& tcp) const
{
	const feature_type* const f_ptr = &feature_type::f_info[f_idx];

	/* Normal attr and char */
	byte a = f_ptr->x_attr;
	char c = f_ptr->x_char;

	/* Special lighting effects */
	if (f_idx <= FEAT_INVIS && OPTION(view_special_lite))
		special_lighting_floor(&a, &c, lighting, in_view);

	/* Special lighting effects (walls only) */
	if (f_idx > FEAT_INVIS && OPTION(view_granite_lite)) 
		special_wall_display(&a, &c, in_view, f_idx);

	/* Save the terrain info for the transparency effects */
	tap = a;
	tcp = c;


	/* If there's an object, deal with that. */
	if (first_k_idx)
	{
		if (hallucinate)
		{
			/* Just pick a random object to display. */
			int i = hallucinatory_object();
			
			a = PICT_A(i);
			c = PICT_C(i);
		}
		else
		{
			/* Get the "pile" feature instead, if more than one and supposed to show it */
			object_kind *k_ptr = &object_type::k_info[(OPTION(show_piles) && multiple_objects) ? 0
													 : first_k_idx];
			
			/* Normal attr and char */
			a = k_ptr->attr_user();
			c = k_ptr->char_user();
		}
	}

	/* If there's a monster */
	if (m_idx > 0)
	{
		if (hallucinate)
		{
			/* Just pick a random monster to display. */
			int i = hallucinatory_monster();
			
			a = PICT_A(i);
			c = PICT_C(i);
		}
		else
		{
			const monster_race* const r_ptr = m_ptr_from_m_idx(m_idx)->race();
				
			byte da;
			char dc;
			
			/* Desired attr & char*/
			da = r_ptr->x_attr;
			dc = r_ptr->x_char;
			
			/* Special attr/char codes */
			if ((da & 0x80) && (dc & 0x80))
			{
				/* Use attr */
				a = da;
				
				/* Use char */
				c = dc;
			}
			
			/* Multi-hued monster */
			else if (r_ptr->flags[0] & RF0_ATTR_MULTI)
			{
				/* Multi-hued attr */
				a = randint(15);
				
				/* Normal char */
				c = dc;
			}
			
			/* Normal monster (not "clear" in any way) */
			else if (!(r_ptr->flags[0] & (RF0_ATTR_CLEAR | RF0_CHAR_CLEAR)))
			{
				/* Use attr */
				a = da;

				/* Desired attr & char */
				da = r_ptr->x_attr;
				dc = r_ptr->x_char;
				
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
			else if (!(r_ptr->flags[0] & RF0_CHAR_CLEAR))
			{
				/* Normal char */
				c = dc;
			}
				
			/* Normal attr, Clear char, monster */
			else if (!(r_ptr->flags[0] & RF0_ATTR_CLEAR))
			{
				/* Normal attr */
					a = da;
			}
		}
	}

	/* Handle "player" */
#ifdef MAP_INFO_MULTIPLE_PLAYERS
	/* Players */
	else if (is_player)
#else /* MAP_INFO_MULTIPLE_PLAYERS */
	/* Handle "player" */
	else if (is_player && !(p_ptr->running && OPTION(hidden_player)))
#endif /* MAP_INFO_MULTIPLE_PLAYERS */
	{
		monster_race *r_ptr = &monster_type::r_info[0];

		/* Get the "player" attr */
		a = r_ptr->x_attr;
#if 0
		if ((hp_changes_color) && (arg_graphics == GRAPHICS_NONE))
		{
			switch(p_ptr->chp * 10 / p_ptr->mhp)
			{
				case 10:
				case  9: 
				{
					a = TERM_WHITE; 
					break;
				}
				case  8:
				case  7:
				{
					a = TERM_YELLOW;
					break;
				}
				case  6:
				case  5:
				{
					a = TERM_ORANGE;
					break;
				}
				case  4:
				case  3:
				{
					a = TERM_L_RED;
					break;
				}
				case  2:
				case  1:
				case  0:
				{
					a = TERM_RED;
					break;
				}
				default:
				{
					a = TERM_WHITE;
					break;
				}
			}
		}
#endif

		/* Get the "player" char */
		c = r_ptr->x_char;
	}

	/* Result */
	ap = a;
	cp = c;	
}


/**
 * This constructor takes a grid location (x, y) and extracts information the
 * player is allowed to know about it, initializing itself with it.
 *
 * The information filled in is as follows:
 *  - f_idx is filled in with the terrain's feature type, or FEAT_NONE
 *    if the player doesn't know anything about the grid.  The function
 *    makes use of the "mimic" field in terrain in order to allow one
 *    feature to look like another (hiding secret doors, invisible traps,
 *    etc).  This will return the terrain type the player "Knows" about,
 *    not necessarily the real terrain.
 *  - m_idx is set to the monster index, or 0 if there is none (or the
 *    player doesn't know it).
 *  - first_k_idx is set to the index of the first object in a grid
 *    that the player knows (and cares, as per hide_squelchable) about,
 *    or zero for no object in the grid.
 *  - multiple_objects is TRUE if there is more than one object in the
 *    grid that the player knows and cares about (to facilitate any special
 *    floor stack symbol that might be used).
 *  - in_view is TRUE if the player can currently see the grid - this can
 *    be used to indicate field-of-view, such as through OPTION(view_bright_lite).
 *  - lighting is set to indicate the lighting level for the grid:
 *    LIGHT_DARK for unlit grids, LIGHT_TORCH for those lit by the player's
 *    light source, and LIGHT_GLOW for inherently light grids (lit rooms, etc).
 *    Note that lighting is always LIGHT_GLOW for known "interesting" grids
 *    like walls.
 *  - is_player is TRUE if the player is on the given grid.
 *  - hallucinate is TRUE if the player is hallucinating something "strange"
 *    for this grid - this should pick a random monster to show if the m_idx
 *    is non-zero, and a random object if first_k_idx is non-zero.
 *       
 * NOTES:
 * This is called pretty frequently, whenever a grid on the map display
 * needs updating, so don't overcomplicate it.
 *
 * Terrain is remembered separately from objects and monsters, so can be
 * shown even when the player can't "see" it.  This leads to things like
 * doors out of the player's view still change from closed to open and so on.
 *
 * TODO:
 * Hallucination is currently disabled (it was a display-level hack before,
 * and we need it to be a knowledge-level hack).  The idea is that objects
 * may turn into different objects, monsters into different monsters, and
 * terrain may be objects, monsters, or stay the same.
 */
grid_data::grid_data(unsigned int y, unsigned int x)
:	first_k_idx(0),				/* Default */
	multiple_objects(false),	/* Default */
	lighting(LIGHT_GLOW),		/* Default */
	hallucinate(p_ptr->timed[TMD_IMAGE])	/* know this now */
{
	assert(x < DUNGEON_WID);
	assert(y < DUNGEON_HGT);

	const object_type* o_ptr;
	byte info = cave_info[y][x];
	
	/* Set things we can work out right now */
	f_idx = cave_feat[y][x];
	in_view = (info & CAVE_SEEN);
	is_player = (cave_m_idx[y][x] < 0);
	m_idx = (is_player) ? 0 : cave_m_idx[y][x];

	/* If the grid is memorised or can currently be seen */
	if (info & (CAVE_MARK | CAVE_SEEN))
	{
		/* Apply "mimic" field */
		f_idx = feature_type::f_info[f_idx].mimic;
			
		/* Boring grids (floors, etc) */
		if (f_idx <= FEAT_INVIS)
		{
			/* Get the floor feature */
			f_idx = FEAT_FLOOR;

			/* Handle currently visible grids */
			if (info & CAVE_SEEN)
			{
				/* Only lit by "torch" lite */
				if (info & CAVE_GLOW)
					lighting = LIGHT_GLOW;
				else
					lighting = LIGHT_TORCH;
			}

			/* Handle "dark" grids and "blindness" */
			else if (p_ptr->timed[TMD_BLIND] || !(info & CAVE_GLOW))
				lighting = LIGHT_DARK;
		}
	}
	/* Unknown */
	else
	{
		f_idx = FEAT_NONE;
	}
       


	/* Objects */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorized objects */
		if (o_ptr->marked /* && !squelch_hide_item(o_ptr) */)
		{
			/* First item found */
			if (first_k_idx == 0)
			{
				first_k_idx = o_ptr->k_idx;
			}
			else
			{
				multiple_objects = TRUE;

				/* And we know all we need to know. */
				break;
			}
		}
	}

	/* Monsters */
	if (0 < m_idx)
	{
		/* If the monster isn't "visible", make sure we don't list it.*/
		if (!m_ptr_from_m_idx(m_idx)->ml) m_idx = 0;
	}

	/* Rare random hallucination on non-outer walls */
	if (hallucinate && 0 == m_idx && 0 == first_k_idx)
	{
		if ((f_idx < FEAT_PERM_SOLID) && one_in_(256))
		{
			/* Normally, make an imaginary monster */
			if (!one_in_(4))
			{
				m_idx = 1;
			}
			/* Otherwise, an imaginary object */
			else
			{
				first_k_idx = 1;
			}
		}
		else
		{
			hallucinate = FALSE;
		}
	}

	/* All other g fields are 'flags', mostly booleans. */
	assert(m_idx < (u32b) mon_max);
	assert(first_k_idx < z_info->k_max);
}

/**
 * Move the cursor to a given map location.
 */
static void move_cursor_relative_map(coord g)
{
	term *old;

	int j;

	coord k;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MAP))) continue;

		/* Verify location */
		if (g.y<t->offset_y || g.x<t->offset_x) continue;

		/* Location relative to panel */
		k.y = g.y - t->offset_y;

		/* Verify location */
		if (k.y >= t->hgt) continue;

		/* Location relative to panel */
		k.x = g.x - t->offset_x;

		if (use_bigtile) k.x += k.x;

		/* Verify location */
		if (k.x >= t->wid) continue;

		/* Go there */
		old = Term;
		Term_activate(t);
		(void)Term_gotoxy(k.x, k.y);
		Term_activate(old);
	}
}


/**
 * Move the cursor to a given map location.
 *
 * The main screen will always be at least 24x80 in size.
 */
void move_cursor_relative(coord g)
{
	coord k,v;

	/* Move the cursor on map sub-windows */
	move_cursor_relative_map(g);

	/* Verify location */
	if (g.y<Term->offset_y || g.x<Term->offset_x) return;

	/* Location relative to panel */
	k.y = g.y - Term->offset_y;

	/* Verify location */
	if (k.y >= SCREEN_HGT) return;

	/* Location relative to panel */
	k.x = g.x - Term->offset_x;

	/* Verify location */
	if (k.x >= SCREEN_WID) return;

	/* Location in window */
	v = k + coord(COL_MAP,ROW_MAP);

	if (use_bigtile) v.x += k.x;

	/* Go there */
	(void)Term_gotoxy(v.x, v.y);
}



/**
 * Display an attr/char pair at the given map location
 *
 * Note the inline use of "panel_contains()" for efficiency.
 *
 * Note the use of "Term_queue_char()" for efficiency.
 */
static void print_rel_map(char c, byte a, coord g)
{
	coord k;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MAP))) continue;

		/* Verify location */
		if (g.y<t->offset_y || g.x<t->offset_x) continue;

		/* Location relative to panel */
		k.y = g.y - t->offset_y;

		/* Verify location */
		if (k.y >= t->hgt) continue;

		/* Location relative to panel */
		k.x = g.x - t->offset_x;

		if (use_bigtile)
		{
			k.x += k.x;
			if (k.x + 1 >= t->wid) continue;
		}

		/* Verify location */
		if (k.x >= t->wid) continue;

		/* Hack -- Queue it */
		Term_queue_char(t, k.x, k.y, a, c, 0, 0);

		if (use_bigtile)
		{
			/* Mega-Hack : Queue dummy char */
			if (a & 0x80)
				Term_queue_char(t, k.x+1, k.y, 255, -1, 0, 0);
			else
				Term_queue_char(t, k.x+1, k.y, TERM_WHITE, ' ', 0, 0);
		}

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);
	}
}



/**
 * Display an attr/char pair at the given map location
 *
 * Note the inline use of "panel_contains()" for efficiency.
 *
 * Note the use of "Term_queue_char()" for efficiency.
 *
 * The main screen will always be at least 24x80 in size.
 */
void print_rel(char c, byte a, coord g)
{
	coord k,v;

	/* Print on map sub-windows */
	print_rel_map(c, a, g);

	/* Verify location */
	if (g.y<Term->offset_y || g.x<Term->offset_x) return;

	/* Location relative to panel */
	k.y = g.y - Term->offset_y;

	/* Verify location */
	if (k.y >= SCREEN_HGT) return;

	/* Location relative to panel */
	k.x = g.x - Term->offset_x;

	/* Verify location */
	if (k.x >= SCREEN_WID) return;

	/* Location in window */
	v = k + coord(COL_MAP,ROW_MAP);

	if (use_bigtile) v.x += k.x;

	/* Hack -- Queue it */
	Term_queue_char(Term, v.x, v.y, a, c, 0, 0);

	if (use_bigtile)
	{
		/* Mega-Hack : Queue dummy char */
		if (a & 0x80)
			Term_queue_char(Term, v.x+1, v.y, 255, -1, 0, 0);
		else
			Term_queue_char(Term, v.x+1, v.y, TERM_WHITE, ' ', 0, 0);
	}
}




/**
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
void note_spot(coord g)
{
	byte info = cave_info[g.y][g.x];	/* Get cave info */
	object_type *o_ptr;

	/* Require "seen" flag */
	if (!(info & (CAVE_SEEN))) return;


	/* Hack -- memorize objects */
	for (o_ptr = get_first_object(g.y, g.x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorize objects */
		o_ptr->marked = TRUE;
	}


	/* Hack -- memorize grids */
	if (!(info & (CAVE_MARK)))
	{
		/* Memorize some "boring" grids */
		if (cave_feat[g.y][g.x] <= FEAT_INVIS)
		{
			/* Option -- memorize certain floors */
			if (((info & (CAVE_GLOW)) && OPTION(view_perma_grids)) ||
			    OPTION(view_torch_grids))
			{
				/* Memorize */
				cave_info[g.y][g.x] |= (CAVE_MARK);
			}
		}

		/* Memorize all "interesting" grids */
		else
		{
			/* Memorize */
			cave_info[g.y][g.x] |= (CAVE_MARK);
		}
	}
}


static void lite_spot_map(coord g)
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

		/* Verify location */
		if (g.y<t->offset_y || g.x<t->offset_x) continue;

		/* Location relative to panel */
		ky = g.y - t->offset_y;
		kx = g.x - t->offset_x;

		if (use_bigtile)
		{
			kx += kx;
			if (kx + 1 >= t->wid) continue;
		}

		/* Verify location */
		if (ky >= t->hgt) continue;
		if (kx >= t->wid) continue;

		/* Hack -- redraw the grid */
		grid_data(g.y, g.x).as_text(a, c, ta, tc);

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



/**
 * Redraw (on the screen) a given map location
 *
 * This function should only be called on "legal" grids.
 *
 * Note the inline use of "print_rel()" for efficiency.
 *
 * The main screen will always be at least 24x80 in size.
 */
void lite_spot(coord g)
{
	byte a;
	char c;
	byte ta;
	char tc;

	coord k,v;

	/* Update map sub-windows */
	lite_spot_map(g);

	/* Verify location */
	if (g.y<Term->offset_y || g.x<Term->offset_x) return;

	/* Location relative to panel */
	k.y = g.y - Term->offset_y;

	/* Verify location */
	if (k.y >= SCREEN_HGT) return;

	/* Location relative to panel */
	k.x = g.x - Term->offset_x;

	/* Verify location */
	if (k.x >= SCREEN_WID) return;

	/* Location in window */
	v = k + coord(COL_MAP,ROW_MAP);

	if (use_bigtile) v.x += k.x;

	/* Hack -- redraw the grid */
	grid_data(g.y, g.x).as_text(a, c, ta, tc);

	/* Hack -- Queue it */
	Term_queue_char(Term, v.x, v.y, a, c, ta, tc);

	if (use_bigtile)
	{
		v.x++;

		/* Mega-Hack : Queue dummy char */
		if (a & 0x80)
			Term_queue_char(Term, v.x, v.y, 255, -1, 0, 0);
		else
			Term_queue_char(Term, v.x, v.y, TERM_WHITE, ' ', TERM_WHITE, ' ');
	}
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
				grid_data(y, x).as_text(a, c, ta, tc);

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



/**
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
			grid_data(y, x).as_text(a, c, ta, tc);

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





/**
 * Hack -- priority array (see below)
 *
 * Note that all "walls" always look like "secret doors" (see "map_info()").
 */
static const int priority_table[14][2] =
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


/**
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

		/* Get the feature */
		f_ptr = &feature_type::f_info[p0];

		/* Check character and attribute, accept matches */
		if ((f_ptr->x_char == c) && (f_ptr->x_attr == a)) return (p1);
	}

	/* Default */
	return (20);
}


/**
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
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	/* Desired map height */
	int map_hgt = Term->hgt - 2;
	int map_wid = Term->wid - 2;

	int dungeon_hgt = (p_ptr->depth == 0) ? TOWN_HGT : DUNGEON_HGT;
	int dungeon_wid = (p_ptr->depth == 0) ? TOWN_WID : DUNGEON_WID;

	int row, col;

	int x, y;

	byte ta;
	char tc;

	byte tp;

	/* Large array on the stack */
	byte mp[DUNGEON_HGT][DUNGEON_WID];

	/* Save lighting effects */
	bool old_view_special_lite = OPTION(view_special_lite);
	bool old_view_granite_lite = OPTION(view_granite_lite);

	monster_race *r_ptr = &monster_type::r_info[0];

	/* Prevent accidents */
	if (map_hgt > dungeon_hgt) map_hgt = dungeon_hgt;
	if (map_wid > dungeon_wid) map_wid = dungeon_wid;

	/* Prevent accidents */
	if ((map_wid < 1) || (map_hgt < 1)) return;


	/* Disable lighting effects */
	OPTION(view_special_lite) = FALSE;
	OPTION(view_granite_lite) = FALSE;


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

			if (use_bigtile)
				col = col & ~1;

			/* Get the attr/char at that map location */
			grid_data(y, x).as_text(ta, tc, ta, tc);

			/* Get the priority of that attr/char */
			tp = priority(ta, tc);

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
	row = (py * map_hgt / dungeon_hgt);
	col = (px * map_wid / dungeon_wid);

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
	OPTION(view_special_lite) = old_view_special_lite;
	OPTION(view_granite_lite) = old_view_granite_lite;
}


/**
 * Display a "small-scale" map of the dungeon.
 *
 * Note that the "player" is always displayed on the map.
 */
void do_cmd_view_map(void)
{
	int cy, cx;
	const char* prompt = "Hit any key to continue";
	
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
 * "boring" grids (floor grids) are only memorized if the "OPTION(view_torch_grids)"
 * option is set, or if the "OPTION(view_perma_grids)" option is set, and the grid
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
 * on every cave grid in the player's field of view.  It also checks the torch
 * radius of the player, and sets the "CAVE_SEEN" flag for every grid which
 * is in the "field of view" of the player and which is also "illuminated",
 * either by the players torch (if any) or by any permanent light source.
 * It could use and help maintain information about multiple light sources,
 * which would be helpful in a multi-player version of Angband.
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




/**
 * Maximum number of grids in a single octant
 */
#define VINFO_MAX_GRIDS 161


/**
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
#ifndef __cplusplus
typedef struct vinfo_type vinfo_type;
#endif


/*
 * The 'vinfo_type' structure
 */
struct vinfo_type
{
	coord grid[8];

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



/**
 * The array of "vinfo" objects, initialized by "vinfo_init()"
 */
static vinfo_type vinfo[VINFO_MAX_GRIDS];




/**
 * Slope scale factor
 */
#define SCALE 100000L


/**
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



#ifndef __cplusplus
/**
 * Forward declare
 */
typedef struct vinfo_hack vinfo_hack;
#endif


/**
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


/**
 * Sorting hook for stdlib qsort of longs
 *
 * We use "u" to point to an array of long integers.
 */
static int qsort_hook_longs(const void* x, const void* y)
{
	long LHS = *((const long*)x);
	long RHS = *((const long*)y);
	if (LHS<RHS) return -1;
	if (LHS>RHS) return 1;
	return 0;
}

/**
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



/**
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
	coord g;

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

	qsort(hack->slopes,hack->num_slopes,sizeof(long),qsort_hook_longs);

	/* Enqueue player grid */
	queue[queue_tail++] = &vinfo[0];

	/* Process queue */
	while (queue_head < queue_tail)
	{
		int e = queue_head++;	/* Index */

		/* Main Grid */
		g = vinfo[e].grid[0];

		/* Location */
		y = g.y;
		x = g.x;


		/* Compute grid offsets */
/*		vinfo[e].grid[0].x = g.x;
		vinfo[e].grid[0].y = g.y; */
		vinfo[e].grid[1].x = g.y;
		vinfo[e].grid[1].y = g.x;
		vinfo[e].grid[2].x = -g.y;
		vinfo[e].grid[2].y = g.x;
		vinfo[e].grid[3].x = -g.x;
		vinfo[e].grid[3].y = g.y;
		vinfo[e].grid[4].x = -g.x;
		vinfo[e].grid[4].y = -g.y;
		vinfo[e].grid[5].x = -g.y;
		vinfo[e].grid[5].y = -g.x;
		vinfo[e].grid[6].x = g.y;
		vinfo[e].grid[6].y = -g.x;
		vinfo[e].grid[7].x = g.x;
		vinfo[e].grid[7].y = -g.y;


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
			g.x = x+1;
			g.y = y;

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
			g.x = x+1;
			g.y = y+1;

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



/**
 * Forget the "CAVE_VIEW" grids, redrawing as needed
 */
void forget_view(void)
{
	coord g;
	for(g.y = 0; g.y < DUNGEON_HGT; ++g.y)
	{
		for (g.x = 0; g.x < DUNGEON_WID; ++g.x)
		{
		/* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
		cave_info[g.y][g.x] &= ~(CAVE_VIEW | CAVE_SEEN);

		/* Redraw */
		lite_spot(g);
		}
	}
}

/**
 * Zaiband: change view to be equivalent to projectability
 */
void view_sieve(coord* coord_list,coord src, int range, size_t& StrictUB)
{	/* define viewability in terms of projectability by a Wand/Rod of Light */
	assert(NULL!=coord_list);
	assert(0<range);
	assert(in_bounds_fully(src.y,src.x));
	assert(coord_list[0]==src);

	/* first one in : src */
	/* next 8 in: cardinal directions, 1 distant */
	/* first questionable test square is 9th */
	if (9>=StrictUB) return;

	{ /* C-ish blocking brace */
	coord grid_g[MAX(DUNGEON_HGT,DUNGEON_WID)];
	size_t TestIdx = 9;
	size_t ScanIdx = StrictUB;

	/* XXX could get some more testing out of 8 cardinal directions, using PROJECT_THRU XXX */
	while(TestIdx <= --ScanIdx)
	{
		/* check the projection path for a lightbeam */
		/* note that grid_n is merely the last illuminated grid on this path, grids are valid out to distance range in Zaiband implementation */
		int grid_n = project_path(grid_g, range, src, coord_list[ScanIdx], true, &wall_stop);
		if (0>=grid_n || grid_g[grid_n-1]!=coord_list[ScanIdx])
		{	/* did not reach target; dark */
			if (ScanIdx+1<StrictUB) memmove(coord_list+ScanIdx,coord_list+ScanIdx+1,sizeof(coord)*(StrictUB-(ScanIdx+1)));
			--StrictUB;
		}
		/* reached target, illuminated: do nothing */
	}
	} /* end C-ish blocking brace */
}

void update_view(void)
{
	/* set up coordinate lists */
	/* perhaps these should be explicit caches in p_ptr? */
	coord* coord_list = (coord*)calloc(squares_in_view_octagon(MAX_SIGHT),sizeof(coord));
	coord* old_seen_list = (coord*)calloc(squares_in_view_octagon(MAX_SIGHT),sizeof(coord));
	size_t StrictUB = 0;	/* technically redundant, will be set correctly anyway later */
	size_t StrictUB_old_seen = 0;
	size_t Idx;
	coord g;
	byte info;

	assert(NULL!=coord_list);
	assert(NULL!=old_seen_list);

	/* archive old seen grids */
	for(g.y = 0; g.y < DUNGEON_HGT; ++g.y)
	{
		for (g.x = 0; g.x < DUNGEON_WID; ++g.x)
		{
			info = cave_info[g.y][g.x];
			if (info & (CAVE_SEEN))
			{
				/* Set "CAVE_TEMP" flag */
				info |= (CAVE_TEMP);

				/* Save grid for later */
				old_seen_list[StrictUB_old_seen++] = g;
			}

			/* Clear "CAVE_VIEW" and "CAVE_SEEN" flags */
			info &= ~(CAVE_VIEW | CAVE_SEEN);

			/* Save cave info */
			cave_info[g.y][g.x] = info;
		}
	}

	check_these_for_view(coord_list,p_ptr->loc,MAX_SIGHT,StrictUB,DUNGEON_HGT,DUNGEON_WID);
	view_sieve(coord_list,p_ptr->loc,MAX_SIGHT,StrictUB);

	/* actually use coord_list */
	assert(coord_list[0]==p_ptr->loc);

	/* Process "new" grids */
	Idx = StrictUB;
	while(0<Idx)
	{
		--Idx;
		info = cave_info[coord_list[Idx].y][coord_list[Idx].x];
		info |= (CAVE_VIEW);	/* Assume viewable */
		if (!p_ptr->timed[TMD_BLIND])
		{
			if (	(info & (CAVE_GLOW))	/* perma-lit grid */
				||	(distance(p_ptr->loc.y,p_ptr->loc.x,coord_list[Idx].y,coord_list[Idx].x)<=p_ptr->cur_lite))	/* torch-lit grid */
			{
				/* Mark as "CAVE_SEEN" */
				info |= (CAVE_SEEN);
			}
		}

		/* Save cave info */
		cave_info[coord_list[Idx].y][coord_list[Idx].x] = info;

		/* Was not "CAVE_SEEN", is now "CAVE_SEEN" */
		if ((info & (CAVE_SEEN)) && !(info & (CAVE_TEMP)))
		{
			/* Note */
			note_spot(coord_list[Idx]);

			/* Redraw */
			lite_spot(coord_list[Idx]);
		}
	}

	/* Process "old" grids */
	Idx = StrictUB_old_seen;
	while(0<Idx)
	{
		--Idx;
		info = cave_info[old_seen_list[Idx].y][old_seen_list[Idx].x];

		/* Clear "CAVE_TEMP" flag */
		info &= ~(CAVE_TEMP);

		/* Save cave info */
		cave_info[old_seen_list[Idx].y][old_seen_list[Idx].x] = info;

		/* Was "CAVE_SEEN", is now not "CAVE_SEEN" */
		if (!(info & (CAVE_SEEN)))
		{
			/* Redraw */
			lite_spot(old_seen_list[Idx]);
		}
	}

	free(coord_list);
	free(old_seen_list);
}

/**
 * Size of the circular queue used by "update_flow()"
 */
#define FLOW_MAX 2048

/**
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



/**
 * Hack -- forget the "flow" information
 */
void forget_flow(void)
{
	int x, y;

	/* Nothing to forget */
	if (!flow_save) return;

	/* Check the entire dungeon */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* Forget the old data */
			cave_cost[y][x] = 0;
			cave_when[y][x] = 0;
		}
	}

	/* Start over */
	flow_save = 0;
}


/**
 * Hack -- fill in the "cost" field of every grid that the player can
 * "reach" with the number of steps needed to reach that grid.  This
 * also yields the "distance" of the player from every grid.
 *
 * In addition, mark the "when" of the grids that can reach the player
 * with the incremented value of "flow_save".
 *
 * Hack -- use the local "flow_c" array as a circular queue of cave grids.
 *
 * We do not need a priority queue because the cost from grid to grid
 * is always "one" (even along diagonals) and we process them in order.
 */
void update_flow(void)
{
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	int y, x;

	int n, d;

	int flow_n;

	int flow_tail = 0;
	int flow_head = 0;

	coord flow_c[FLOW_MAX];

	/* Hack -- disabled */
	if (!OPTION(adult_flow_by_sound)) return;


	/*** Cycle the flow ***/

	/* Cycle the flow */
	if (flow_save++ == 255)
	{
		/* Cycle the flow */
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
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
	flow_c[flow_head] = p_ptr->loc;

	/* Advance the queue */
	++flow_tail;


	/*** Process Queue ***/

	/* Now process the queue */
	while (flow_head != flow_tail)
	{
		/* Extract the next entry */
		coord t = flow_c[flow_head];

		/* Forget that entry (with wrap) */
		flow_head = (flow_head+1)%FLOW_MAX;

		/* Child cost */
		n = cave_cost[t.y][t.x] + 1;

		/* Hack -- Limit flow depth */
		if (n == MONSTER_FLOW_DEPTH) continue;

		/* Add the "children" */
		for (d = 0; d < KEYPAD_DIR_MAX; d++)
		{
			int old_head = flow_tail;

			/* Child location */
			coord new_coord = t+dd_coord_ddd[d];

			/* Ignore "pre-stamped" entries */
			if (cave_when[new_coord.y][new_coord.x] == flow_n) continue;

			/* Ignore "walls" and "rubble" */
			if (cave_feat[new_coord.y][new_coord.x] >= FEAT_RUBBLE) continue;

			/* Save the time-stamp */
			cave_when[new_coord.y][new_coord.x] = flow_n;

			/* Save the flow cost */
			cave_cost[new_coord.y][new_coord.x] = n;

			/* Enqueue that entry */
			flow_c[flow_tail] = new_coord;

			/* Advance the queue */
			flow_tail = (flow_tail+1)%FLOW_MAX;

			/* Hack -- Overflow by forgetting new entry */
			if (flow_tail == flow_head) flow_tail = old_head;
		}
	}
}




/**
 * Map the current panel (plus some) ala "magic mapping"
 *
 * We must never attempt to map the outer dungeon walls, or we
 * might induce illegal cave grid references.
 */
void map_area(void)
{
	int i, x, y, y1, y2, x1, x2;


	/* Pick an area to map */
	y1 = Term->offset_y - randint(10);
	y2 = Term->offset_y + SCREEN_HGT + randint(10);
	x1 = Term->offset_x - randint(20);
	x2 = Term->offset_x + SCREEN_WID + randint(20);

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
			if (cave_feat[y][x] < FEAT_SECRET)
			{
				/* Memorize normal features */
				if (cave_feat[y][x] > FEAT_INVIS)
				{
					/* Memorize the object */
					cave_info[y][x] |= (CAVE_MARK);
				}

				/* Memorize known walls */
				for (i = 0; i < KEYPAD_DIR_MAX; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Memorize walls (etc) */
					if (cave_feat[yy][xx] >= FEAT_SECRET)
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
}

static bool wiz_lite_object(object_type& o)
{
	/* Memorize non-held objects */
	if (!o.held_m_idx) o.marked = TRUE;

	return false;
}

/**
 * Light up the dungeon using "clairvoyance"
 *
 * This function "illuminates" every grid in the dungeon, memorizes all
 * "objects", memorizes all grids as with magic mapping, and, under the
 * standard option settings (OPTION(view_perma_grids) but not OPTION(view_torch_grids))
 * memorizes all floor grids too.
 *
 * Note that if "OPTION(view_perma_grids)" is not set, we do not memorize floor
 * grids, since this would defeat the purpose of "OPTION(view_perma_grids)", not
 * that anyone seems to play without this option.
 *
 * Note that if "OPTION(view_torch_grids)" is set, we do not memorize floor grids,
 * since this would prevent the use of "OPTION(view_torch_grids)" as a method to
 * keep track of what grids have been observed directly.
 */
void wiz_lite(void)
{
	int i, y, x;


	/* Memorize objects */
	object_scan(wiz_lite_object);

	/* Scan all normal grids */
	for (y = 1; y < DUNGEON_HGT-1; y++)
	{
		/* Scan all normal grids */
		for (x = 1; x < DUNGEON_WID-1; x++)
		{
			/* Process all non-walls */
			if (cave_feat[y][x] < FEAT_SECRET)
			{
				/* Scan all neighbors */
				for (i = 0; i < KEYPAD_DIR_MAX+1; i++)
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
					if (OPTION(view_perma_grids) && !OPTION(view_torch_grids))
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

	/* Redraw map, monster list */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST);
}

static bool wiz_dark_object(object_type& o)
{
	/* Forget non-held objects */
	if (!o.held_m_idx) o.marked = FALSE;

	return false;
}

/**
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(void)
{
	int y, x;


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
	object_scan(wiz_dark_object);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map, monster list */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST);
}



/**
 * Light or Darken the town
 */
void town_illuminate(bool daytime)
{
	int y, x, i;


	/* Apply light or darkness */
	for (y = 0; y < TOWN_HGT; y++)
	{
		for (x = 0; x < TOWN_WID; x++)
		{
			/* Interesting grids */
			if (cave_feat[y][x] > FEAT_INVIS)
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
				if (OPTION(view_perma_grids))
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
				if (OPTION(view_perma_grids))
				{
					cave_info[y][x] &= ~(CAVE_MARK);
				}
			}
		}
	}


	/* Handle shop doorways */
	for (y = 0; y < TOWN_HGT; y++)
	{
		for (x = 0; x < TOWN_WID; x++)
		{
			/* Track shop doorways */
			if (cave_feat_in_range(y,x,FEAT_SHOP_HEAD,FEAT_SHOP_TAIL))
			{
				for (i = 0; i < KEYPAD_DIR_MAX; i++)
				{
					int yy = y + ddy_ddd[i];
					int xx = x + ddx_ddd[i];

					/* Illuminate the grid */
					cave_info[yy][xx] |= (CAVE_GLOW);

					/* Hack -- Memorize grids */
					if (OPTION(view_perma_grids)) cave_info[yy][xx] |= (CAVE_MARK);
				}
			}
		}
	}


	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map, monster list */
	p_ptr->redraw |= (PR_MAP | PR_MONLIST);
}



/**
 * Change the "feat" flag for a grid, and notice/redraw the grid
 */
void cave_set_feat(int y, int x, int feat)
{
	assert(0 <= feat && feat < z_info->f_max);
	/* Change the feature */
	cave_feat[y][x] = feat;

	/* Handle "wall/door" grids */
	if (feat >= FEAT_DOOR_HEAD)
	{
		cave_info[y][x] |= (CAVE_WALL);
	}

	/* Handle "floor"/etc grids */
	else
	{
		cave_info[y][x] &= ~(CAVE_WALL);
	}

	/* Notice/Redraw */
	if (character_dungeon)
	{
		/* Notice */
		note_spot(coord(x, y));

		/* Redraw */
		lite_spot(coord(x, y));
	}
}

bool wall_stop(coord g)
{
	/* Always stop at non-initial wall grids */
	if (!cave_floor_bold(g.y, g.x)) return true;
	return false;
}

bool wall_mon_stop(coord g)
{
	/* Always stop at non-initial wall grids */
	if (!cave_floor_bold(g.y, g.x)) return true;

	/* stop at non-initial monsters/players */
	if (cave_m_idx[g.y][g.x] != 0) return true;
	return false;
}


/**
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming that no monster gets in the way,
 * using the "project_path()" function to check the projection path.
 *
 * Note that no grid is ever "projectable()" from itself.
 *
 * This function is used to determine if the player can (easily) target
 * a given grid, and if a monster can target the player.
 */
bool projectable(coord g1, coord g2)
{
	coord grid_g[MAX(DUNGEON_HGT,DUNGEON_WID)];
	int grid_n = project_path(grid_g, MAX_RANGE, g1, g2, true, &wall_stop);	/* Check the projection path */

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* May not end in an unrequested grid */
	if (grid_g[grid_n-1]!=g2) return (FALSE);

	/* May not end in a wall grid */
	if (!cave_floor_bold(g2.y, g2.x)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}



/**
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
void scatter(coord& g, coord g2, int d, int m)
{
	coord_scan n;


	/* Unused parameter */
	(void)m;

	/* Pick a location */
	do	{
		/* Pick a new location */
		n.y = rand_spread(g2.y, d);
		n.x = rand_spread(g2.x, d);

		/* Ignore annoying locations */
		if (!in_bounds_fully(n.y, n.x)) continue;

		/* Ignore "excessively distant" locations */
		if ((d > 1) && (distance(g2.y, g2.x, n.y, n.x) > d)) continue;
		}
	while(!los(g2,n));	/* Require "line of sight" */

	/* Save the location */
	g = n;
}






/**
 * Track a new monster
 */
void health_track(int m_idx)
{
	p_ptr->health_who = m_idx;		/* Track a new guy */
	p_ptr->redraw |= (PR_HEALTH);	/* Redraw (later) */
}



/**
 * Hack -- track the given monster race
 */
void monster_race_track(int r_idx)
{
	p_ptr->monster_race_idx = r_idx;	/* Save this monster ID */
	p_ptr->redraw |= (PR_MONSTER);		/* Redraw (later) */
}



/**
 * Hack -- track the given object kind
 */
void object_kind_track(int k_idx)
{
	p_ptr->object_kind_idx = k_idx;		/* Save this object ID */
	p_ptr->redraw |= (PR_OBJECT);		/* Redraw (later) */
}



/**
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
 		if (OPTION(center_player) && OPTION(run_avoid_center)) event_signal(EVENT_PLAYERMOVED);

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Redraw the player, if needed */
		if (OPTION(hidden_player)) lite_spot(p_ptr->loc);
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
	if (OPTION(flush_disturb)) flush();
}




/**
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

