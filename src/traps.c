/* File: traps.c */

/*
 * YATS (Yet another trap system).  Table of trap kinds, tests for kinds
 * of traps, display traps and graphics, pick a trap and place it.
 * Disarm and load traps.  Monster trap effects.  Character trap effects.
 *
 *
 * Copyright (c) 2007
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * A table of trap_kind information.  Move this to an external text file
 * at some point.
 *
 * d_attr, d_char, x_attr, x_char, min_depth, xtra,
 * flags, name
 *
 * A max_depth of 0 means no limit
 * A rarity of 0 means it doesn't get generated normally
 */
trap_kind t_kind_info[TRAP_KIND_MAX] =
{
	{ 'd', '^', 'd', '^', 0, 0, 0, "" },  /* Blank trap */
	{ 'B', ';', 'B', ';', 0, 0,  /* Loose rock -- has special-case code */
	  TRAP_NO_DISARM | TRAP_PLAY, "loose rock" },
	{ 'y', ';', 'y', ';', 0, 0,
	  TRAP_VISIBLE | TRAP_EASY_DISARM | TRAP_KILL, "glyph of warding" },
	{ 'd', '^', 'd', '^', 0, 0, 0, "" },
	{ 'd', '^', 'd', '^', 0, 0, 0, "" },

	{ 'y', '^', 'y', '^', 0, 0,
	  TRAP_VISIBLE | TRAP_EASY_DISARM | TRAP_PLAY | TRAP_KILL, "monster trap" },
	{ 'w', '^', 'w', '^', 3, 0,  /* Trap door -- has special-case code */
	  TRAP_PLAY | TRAP_NASTY, "trap door" },
	{ 'u', '^', 'u', '^', 2, 0,
	  TRAP_PLAY | TRAP_NASTY, "pit" },
	{ 'r', '^', 'r', '^', 6, 0,
	  TRAP_PLAY | TRAP_NASTY, "dart trap" },
	{ 's', '^', 's', '^', 5, 0,
	  TRAP_PLAY | TRAP_NASTY, "discolored spot" },
	{ 'g', '^', 'g', '^', 0, 0,
	  TRAP_PLAY | TRAP_NASTY, "gas trap" },
	{ 'o', '^', 'o', '^', 3, 0,
	  TRAP_PLAY | TRAP_NASTY, "strange rune" },
	{ 'U', '^', 'U', '^', 3, 0,
	  TRAP_PLAY | TRAP_NASTY, "strange rune" },
	{ 'v', '^', 'v', '^', 30, 0,
	  TRAP_PLAY | TRAP_NASTY, "hex" },
	{ 'b', '^', 'b', '^', 0, 0,
	  TRAP_PLAY | TRAP_NASTY, "shimmering portal" },
	{ 'W', '^', 'W', '^', 4, 0,
	  TRAP_PLAY | TRAP_NASTY, "murder hole" },
	{ 'd', '^', 'd', '^', 0, 0, 0, "" },
	{ 'd', '^', 'd', '^', 0, 0, 0, "" },
	{ 'd', '^', 'd', '^', 0, 0, 0, "" },
	{ 'd', '^', 'd', '^', 0, 0, 0, "" }
};




/*
 * Is there a specific kind of trap in this grid?
 */
bool cave_trap_specific(int y, int x, int t_idx)
{
	int i;

	/* First, check the trap marker */
	if (!cave_trap(y, x)) return (FALSE);

	/* Scan the current trap list */
	for (i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* We found a trap of the right kind */
			if (t_ptr->t_idx == t_idx) return (TRUE);
		}
	}

	/* Report failure */
	return (FALSE);
}

/*
 * Is there a loose rock in this grid?
 */
bool cave_loose_rock(int y, int x)
{
	/* Look for a loose rock */
	return (cave_trap_specific(y, x, TRAP_LOOSE_ROCK));
}

/*
 * Is there a glyph in this grid?
 */
bool cave_glyph(int y, int x)
{
	/* Look for a glyph */
	return (cave_trap_specific(y, x, TRAP_GLYPH));
}

/*
 * Is there a pit trap in this grid?
 */
bool cave_pit_trap(int y, int x)
{
	/* Look for a glyph */
	return (cave_trap_specific(y, x, TRAP_PIT));
}


/*
 * Check for a monster trap trap that affects monsters
 */
bool cave_monster_trap(int y, int x)
{
	/* Look for a monster trap */
	return (cave_trap_specific(y, x, TRAP_MONSTER));
}


/*
 * Determine if a trap actually exists in this grid.
 *
 * Called with vis = 0 to accept any trap, = 1 to accept only visible
 * traps, and = -1 to accept only invisible traps.
 *
 * Clear the CAVE_TRAP flag if none exist.
 */
static bool verify_trap(int y, int x, int vis)
{
	int i;
	bool trap = FALSE;

	/* Scan the current trap list */
	for (i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Accept any trap */
			if (!vis) return (TRUE);

			/* Accept traps that match visibility requirements */
			if (vis == 1)
			{
				if (t_ptr->flags & (TRAP_VISIBLE)) return (TRUE);
			}

			if (vis == -1)
			{
				if (!(t_ptr->flags & (TRAP_VISIBLE))) return (TRUE);
			}

			/* Note that a trap does exist */
			trap = TRUE;
		}
	}

	/* No traps in this location. */
	if (!trap)
	{
		/* No traps */
		cave_info[y][x] &= ~(CAVE_TRAP);

		/* No reason to mark this grid, ... */
		cave_info[y][x] &= ~(CAVE_MARK);

		/* ... unless certain conditions apply */
		note_spot(y, x);
	}

	/* Report failure */
	return (FALSE);
}

/*
 * Is there a visible trap in this grid?
 */
bool cave_visible_trap(int y, int x)
{
	/* First, check the trap marker */
	if (!cave_trap(y, x)) return (FALSE);

	/* Verify trap, require that it be visible */
	return (verify_trap(y, x, 1));
}

/*
 * Is there an invisible trap in this grid?
 */
bool cave_invisible_trap(int y, int x)
{
	/* First, check the trap marker */
	if (!cave_trap(y, x)) return (FALSE);

	/* Verify trap, require that it be invisible */
	return (verify_trap(y, x, -1));
}



/*
 * Get the graphics of any trap in this grid.
 *
 * We should probably have better handling of stacked traps, but that can
 * wait until we do, in fact, have stacked traps under normal conditions.
 *
 * Note:  If an error has occurred, and there is in fact no trap in this
 * grid, we clear the CAVE_TRAP flag and return FALSE.
 */
bool get_trap_graphics(int y, int x, byte *a, char *c, bool require_visible)
{
	int i;
	bool trap = FALSE;

	/* Scan the current trap list */
	for (i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Trap is visible, or we don't care */
			if ((!require_visible) || (t_ptr->flags & (TRAP_VISIBLE)))
			{
				/* Get the graphics */
				*a = color_char_to_attr(t_kind_info[t_ptr->t_idx].x_attr);
				*c = t_kind_info[t_ptr->t_idx].x_char;

				/* We found a trap */
				return (TRUE);
			}

			/* Note that a trap does exist */
			trap = TRUE;
		}
	}

	/* We found no traps in this location.  Clear the CAVE_TRAP flag */
	if (!trap) cave_info[y][x] &= ~(CAVE_TRAP);

	/* No traps found */
	return (FALSE);
}


/*
 * Hack -- toggle on TRAP_NO_EXP markers for nearby traps.   XXX XXX
 */
void no_exp_traps(int y, int x)
{
	int d, i;

	/* Look in all directions (and underneath). */
	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent location */
		int yy = y + ddy_ddd[d];
		int xx = x + ddx_ddd[d];

		/* Require a trap */
		if (cave_info[yy][xx] & (CAVE_TRAP))
		{
			/* Scan the current trap list */
			for (i = 0; i < t_max; i++)
			{
				/* Point to this trap */
				trap_type *t_ptr = &t_list[i];

				/* Find any traps in this position */
				if ((t_ptr->fy == yy) && (t_ptr->fx == xx))
				{
					/* Trap gives no exp */
					t_ptr->flags |= (TRAP_NO_EXP);
				}
			}
		}
	}
}


/*
 * Reveal some of the traps in a grid
 */
bool reveal_trap(int y, int x, int chance, bool msg, bool see_loose_rocks)
{
	int i;
	int found_trap = 0;
	int found_rock = 0;


	/* Check the trap marker */
	if (!cave_trap(y, x)) return (FALSE);


	/* Scan the current trap list */
	for (i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Trap is invisible */
			if (!(t_ptr->flags & (TRAP_VISIBLE)))
			{
				/* Hack -- sometimes ignore loose rocks */
				if ((!see_loose_rocks) && (t_ptr->t_idx == TRAP_LOOSE_ROCK))
					continue;

				/* See the trap */
				t_ptr->flags |= (TRAP_VISIBLE);
				cave_info[y][x] |= (CAVE_MARK);

				/* We found a trap */
				found_trap++;

				/* We found a loose rock */
				if (t_ptr->t_idx == TRAP_LOOSE_ROCK) found_rock++;

				/* If chance is < 100, sometimes stop */
				if ((chance < 100) && (randint(100) > chance)) break;
			}
		}
	}

	/* We found at least one trap (or loose rock) */
	if (found_trap || found_rock)
	{
		/* We want to talk about it */
		if (msg)
		{
			if (found_rock == found_trap)
			{
				msg_print("You have found a loose rock.");
			}
			else if (found_trap == 1) msg_print("You have found a trap.");
			else msg_format("You have found %d traps.", found_trap);
		}

		/* Memorize */
		cave_info[y][x] |= (CAVE_MARK);

		/* Redraw */
		lite_spot(y, x);
	}

	/* Return TRUE if we found any traps */
	return (found_trap != 0);
}

/*
 * Count the nasty traps in this location.
 *
 * Called with vis = 0 to accept any trap, = 1 to accept only visible
 * traps, and = -1 to accept only invisible traps.
 */
int nasty_traps(int y, int x, int vis)
{
	int i, num;

	/* Scan the current trap list */
	for (num = 0, i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find all traps in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Require that trap be capable of affecting the character */
			if (!(t_kind_info[t_ptr->t_idx].flags & (TRAP_PLAY))) continue;

			/* Require that trap be "nasty" */
			if (!(t_kind_info[t_ptr->t_idx].flags & (TRAP_NASTY))) continue;

			/* Require correct visibility */
			if (vis >= 1)
			{
				if (t_ptr->flags & (TRAP_VISIBLE)) num++;
			}
			else if (vis <= -1)
			{
				if (!(t_ptr->flags & (TRAP_VISIBLE))) num++;
			}
			else
			{
				num++;
			}
		}
	}

	/* Return the number of nasty traps */
	return (num);
}

/*
 * Get a random, but appropriate, trap for this depth.
 *
 * Accept only "nasty" traps.
 *
 * Note:  We use current character depth, not the given trap level, for
 * deciding whether trap doors can be placed.
 */
static int pick_trap(int trap_level)
{
	bool trap_is_okay = FALSE;
	int i = 0;
	int trap = -1;

	trap_kind *t_kind_ptr;

	/* Try to create a trap appropriate to the level. */
	while ((!trap_is_okay) && (i++ < 50))
	{
		/* Pick at random. */
		trap = rand_int(TRAP_KIND_MAX);

		/* Get this trap */
		t_kind_ptr = &t_kind_info[trap];

		/* Require that trap be capable of hurting the character */
		if (!(t_kind_ptr->flags & (TRAP_PLAY))) continue;

		/* Require that trap be "nasty" */
		if (!(t_kind_ptr->flags & (TRAP_NASTY))) continue;

		/* Require that trap_level not be too low */
		if (t_kind_ptr->min_depth > trap_level) continue;

		/* Assume legal until proven otherwise. */
		trap_is_okay = TRUE;

		/* Check legality. */
		switch (trap)
		{
			/* Trap doors */
			case TRAP_DOOR:
			{
				/* Hack -- no trap doors on quest levels */
				if (quest_check(p_ptr->depth)) trap_is_okay = FALSE;

				/* Hack -- no trap doors on the deepest level */
				if (p_ptr->depth >= MAX_DEPTH - 1) trap_is_okay = FALSE;

				break;
			}

			/* Pits */
			case TRAP_PIT:
			case TRAP_DART:
			case TRAP_SPOT:
			case TRAP_GAS:
			case TRAP_SUMMON:
			case TRAP_ALTER_DUNGEON:
			case TRAP_HEX:
			case TRAP_PORTAL:
			case TRAP_MURDER_HOLE:
			{
				/* No special restrictions */
				break;
			}

			/* Any other selection is not defined. */
			default:
			{
				trap_is_okay = FALSE;
				break;
			}
		}
	}

	/* Return our chosen trap */
	return (trap);
}



/*
 * Determine if a cave grid is allowed to have traps in it.
 */
bool cave_trap_allowed(int y, int x)
{
	/*
	 * We currently forbid multiple traps in a grid under normal conditions.
	 * If this changes, various bits of code elsewhere will have to change too.
	 */
	if (cave_trap(y, x)) return (FALSE);

	/* Check the feature trap flag */
	return ((f_info[cave_feat[y][x]].flags & (TF_TRAP)) != 0);
}

/*
 * Make a new trap of the given type.  Return TRUE if successful.
 *
 * We choose a trap at random if the index is not legal.
 *
 * This should be the only function that places traps in the dungeon
 * except the savefile loading code.
 */
bool place_trap(int y, int x, int t_idx, int trap_level)
{
	int i;


	/* Require the correct terrain */
	if (!cave_trap_allowed(y, x)) return (FALSE);

	/* Hack -- don't use up all the trap slots during dungeon generation */
	if (!character_dungeon)
	{
		if (t_max > z_info->t_max - 50) return (FALSE);
	}

	/* We've been called with an illegal index; choose a random trap */
	if ((t_idx <= 0) || (t_idx >= TRAP_KIND_MAX))
	{
		t_idx = pick_trap(trap_level);
	}

	/* Note failure */
	if (t_idx < 0) return (FALSE);


	/* Scan the entire trap list */
	for (i = 1; i < z_info->t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* This space is available */
		if (!t_ptr->t_idx)
		{
			/* Fill in the trap index */
			t_ptr->t_idx = t_idx;

			/* Fill in the trap details */
			t_ptr->fy = y;
			t_ptr->fx = x;

			t_ptr->hold_o_idx = 0;
			t_ptr->flags = t_kind_info[t_ptr->t_idx].flags;

			/* Adjust trap count if necessary */
			if (i + 1 > t_max) t_max = i + 1;

			/* We created a glyph */
			if (t_idx == TRAP_GLYPH) num_glyph_on_level++;

			/* We created a monster trap */
			if (t_idx == TRAP_MONSTER)
			{
				p_ptr->redraw |= PR_CONDITIONS;
				num_trap_on_level++;
			}

			/* Toggle on the trap marker */
			cave_info[y][x] |= (CAVE_TRAP);

			/* Redraw the grid */
			lite_spot(y, x);

			/* Report success */
			return (TRUE);
		}
	}

	/* No luck -- report failure */
	return (FALSE);
}


/*
 * Leave a "glyph of warding" which prevents monster movement
 */
bool warding_glyph(int y, int x)
{
	/* Limit total number of glyphs. -LM- */
	if (num_glyph_on_level >= MAX_GLYPHS)
	{
		/* Messages vary depending on where the glyph is being set */
		if ((y == p_ptr->py) && (x == p_ptr->px))
		{
		msg_print("You cannot set any more glyphs until you desanctify your existing ones.");
		}
		else
		{
			/* This could use a rethink  XXX */
			msg_print("No more glyphs can be created until you desanctify some of the existing ones.");
		}
		return (FALSE);
	}

	/* Trap (or glyph) can't be placed here */
	if (!cave_trap_allowed(y, x))
	{
		if ((y == p_ptr->py) && (x == p_ptr->px))
		{
			msg_print("You cannot place a glyph here.");
		}
		else
		{
			/* This could use a rethink  XXX */
			msg_print("(glyph could not be placed)");
		}
		return (FALSE);
	}

	/* Create a glyph, increment glyph count */
	place_trap(y, x, TRAP_GLYPH, 0);

	/* Warning. */
	if (num_glyph_on_level >= MAX_GLYPHS)
	{
		if ((y == p_ptr->py) && (x == p_ptr->px))
		{
			msg_print("You have now reached your glyph limit.  In order to set more, desanctify some.");
		}
		else
		{
			/* This could use a rethink  XXX */
			msg_print("(glyph limit now reached)");
		}
	}

	/* Success */
	return (TRUE);
}


/*
 * Delete/Remove all the traps when the player leaves the level
 */
void wipe_t_list(void)
{
	int i;

	/* Delete all the traps */
	for (i = t_max - 1; i >= 0; i--)
	{
		trap_type *t_ptr = &t_list[i];

		/* Wipe the trap */
		WIPE(t_ptr, trap_type);
	}

	/* Reset "t_max" */
	t_max = 0;

	/* Reset the number of glyphs on the level. */
	num_glyph_on_level = 0;

	/* Reset the number of monster traps on the level. */
	num_trap_on_level = 0;
}



/*
 * Remove a trap
 */
static void remove_trap_aux(trap_type *t_ptr, int y, int x)
{
	object_type *i_ptr;
	object_type *o_ptr;
	object_type forge;

	/* We are deleting a glyph */
	if (t_ptr->t_idx == TRAP_GLYPH) num_glyph_on_level--;

	/* We are deleting a monster trap */
	if (t_ptr->t_idx == TRAP_MONSTER) num_trap_on_level--;

	/* Trap drops all of its objects */
	while (t_ptr->hold_o_idx)
	{
		/* Get this object */
		o_ptr = &o_list[t_ptr->hold_o_idx];

		/* Hack -- handle "nothings" in traps */
		if (!o_ptr->k_idx) break;

		/* Get local object */
		i_ptr = &forge;

		/* Wipe the copy */
		object_wipe(i_ptr);

		/* Copy the object */
		object_copy(i_ptr, o_ptr);

		/* Delete the original object */
		delete_object_idx(t_ptr->hold_o_idx);

		/* Drop the copy */
		drop_near(i_ptr, 0, y, x, DROP_HERE);
	}

	/* Wipe the trap */
	(void)WIPE(t_ptr, trap_type);
}

/*
 * Remove traps.
 *
 * If called with t_idx < 0, will remove all traps in the location given.
 * Otherwise, will remove the trap with the given index.
 *
 * Return TRUE if no traps now exist in this grid.
 */
bool remove_trap(int y, int x, int t_idx)
{
	int i;
	bool trap;

	/* Called with a specific index */
	if (t_idx >= 0)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[t_idx];

		/* Remove it */
		remove_trap_aux(t_ptr, y, x);

		/* Note when trap list actually gets shorter */
		if (t_idx == t_max - 1) t_max--;
	}

	/* No specific index -- remove all traps here */
	else
	{
		/* Scan the current trap list (backwards) */
		for (i = t_max - 1; i >= 0; i--)
		{
			/* Point to this trap */
			trap_type *t_ptr = &t_list[i];

			/* Find all traps in this position */
			if ((t_ptr->fy == y) && (t_ptr->fx == x))
			{
				/* Remove it */
				remove_trap_aux(t_ptr, y, x);

				/* Note when trap list actually gets shorter */
				if (i == t_max - 1) t_max--;
			}
		}
	}

	/* Refresh grids that the character can see */
	if (player_can_see_or_infra_bold(y, x)) lite_spot(y, x);

	/* Verify traps (remove marker if appropriate) */
	trap = verify_trap(y, x, 0);

	/* Report whether any traps exist in this grid */
	return (trap);
}

/*
 * Remove all traps of a specific kind from a location.
 */
void remove_trap_kind(int y, int x, int t_idx)
{
	int i;

	/* Scan the current trap list */
	for (i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Require that it be of this type */
			if (t_ptr->t_idx == t_idx) (void)remove_trap(y, x, i);
		}
	}
}

/* Convert letters to indexes */
static int trap_indexes[99];

/*
 * Display traps in a list.
 *
 * Remember have many traps were shown, and what their indexes were.
 */
static void display_traps(int y, int x, int *count)
{
	int i;

	/* Display the individual traps */
	for (*count = 0, i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Trap must be visible */
			if (!(t_ptr->flags & (TRAP_VISIBLE))) continue;

			/* Display this trap */
			prt(format(" %c) %.30s", index_chars_lower[*count],
				t_kind_info[t_ptr->t_idx].name), *count + 1, Term->cols - 34);

			/* Remember this trap */
			trap_indexes[(*count)++] = i;
		}
	}

	/* Print a blank line below the list of traps */
	prt("                                  ", i, Term->cols - 34);
}


/*
 * Choose a trap.  If there is more than one trap in this grid, we display
 * a menu, and let the player choose among them.
 *
 * Possible enhancements to this code include remembering which trap was
 * chosen last so that command repeats can work correctly on grids with
 * multiple traps.
 */
bool get_trap(int y, int x, int *idx)
{
	int i;
	int count;
	int choice;
	char ch;

	/* Require that grid be legal */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Look for the trap marker */
	if (!cave_trap(y, x)) return (FALSE);

	/* Scan the current trap list */
	for (count = 0, i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Trap must be visible */
			if (!(t_ptr->flags & (TRAP_VISIBLE))) continue;

			/* Count all traps */
			count++;

			/* Remember last trap index */
			*idx = i;
		}
	}

	/* We have no visible traps */
	if (!count) return (FALSE);

	/* We have one trap (usual case) */
	if (count == 1) return (TRUE);

	/* We have more than one trap */
	else
	{
		/* Save the screen */
		screen_save(FALSE);

		/* Display traps */
		display_traps(y, x, &count);

		/* Interact */
		while (TRUE)
		{
			/* Ask player to choose a trap, allow escape */
			if (!get_com(format("Choose which trap (%c-%c)?",
				index_chars_lower[0], index_chars_lower[count - 1]), &ch))
			{
				/* Restore the screen */
				screen_load();

				return (FALSE);
			}

			/* Convert command to a choice (squash case) */
			choice = get_index(ch, TRUE);

			/* If choice is legal... */
			if ((choice >= 0) && (choice < count))
			{
				/* Select trap */
				*idx = trap_indexes[choice];

				/* Restore the screen */
				screen_load();

				/* Report success */
				return (TRUE);
			}

			/* Otherwise... */
			else
			{
				/* Be helpful */
				msg_print("Please press a letter to select a trap, or ESC to cancel");

				/* Allow user to try again */
			}
		}
	}

	/* Return */
	return (FALSE);
}


/*
 * Determine if a trap is disarmable
 */
static bool is_disarmable_trap(trap_type *t_ptr)
{
	if (!(t_ptr->flags & (TRAP_VISIBLE))) return (FALSE);
	return (!(t_kind_info[t_ptr->t_idx].flags & (TRAP_NO_DISARM)));
}

/*
 * Determine if a grid contains disarmable traps.
 */
bool has_disarmable_trap(int y, int x)
{
	int i;

	/* First, check the trap marker */
	if (!cave_trap(y, x)) return (FALSE);

	/* Scan the current trap list */
	for (i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find all traps in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Accept first disarmable trap */
			if (is_disarmable_trap(t_ptr)) return (TRUE);
		}
	}

	/* No disarmable traps found */
	return (FALSE);
}

/*
 * Attempt to magically disarm traps using magic.
 */
bool magic_disarm(int y, int x, int chance)
{
	int i;
	bool obvious = FALSE;
	object_type *o_ptr;


	/* There is a trap in this grid */
	if (cave_info[y][x] & (CAVE_TRAP))
	{
		/* Scan the current trap list */
		for (i = 0; i < t_max; i++)
		{
			/* Point to this trap */
			trap_type *t_ptr = &t_list[i];

			/* Find a trap in this position */
			if ((t_ptr->fy == y) && (t_ptr->fx == x))
			{
				/* Ignore glyphs, monster traps, and loose rocks */
				if ((t_ptr->t_idx == TRAP_GLYPH) ||
					 (t_ptr->t_idx == TRAP_MONSTER) ||
					 (t_ptr->t_idx == TRAP_LOOSE_ROCK))
				{
					continue;
				}

				/* Attempt to disarm the trap */
				if (rand_int(100) < chance)
				{
					/* Check line of sight */
					if (player_has_los_bold(y, x))
					{
						msg_print("There is a bright flash of light.");
						obvious = TRUE;
					}

					/* Kill the trap (always visible) */
					remove_trap(y, x, i);
					lite_spot(y, x);
				}

				/* Some chance of setting off the trap. */
				else
				{
					msg_print("Your magic was too weak!");
					(void)hit_trap(-1, y, x);
					obvious = TRUE;
				}
			}
		}
	}

	/* Scan objects in the grid */
	for (o_ptr = get_first_object(y, x); o_ptr;
		  o_ptr = get_next_object(o_ptr))
	{
		/* Look for traps */
		if (check_chest_traps(o_ptr, FALSE))
		{
			/* Attempt to disarm the trap */
			if (rand_int(100) < chance)
			{
				/* Hack -- mark the chest as being untrapped */
				o_ptr->pval = (0 - o_ptr->pval);

				msg_print("Click!");
			}

			/* Oops */
			else
			{
				/* Hit the trap */
				(void)hit_chest_trap(y, x, o_ptr);

				/* Notice */
				obvious = TRUE;

				/* Stop */
				break;
			}
		}
	}

	/* Return whether anything was noticed */
	return (obvious);
}

bool item_tester_hook_trap(const object_type *o_ptr)
{
    if (o_ptr->tval != TV_POUCH)  /* You can put almost anything in a trap */
    {
        return (TRUE);
    }
    return (FALSE);
}

/*
 * Interact with a monster trap.
 *
 * Return 0 when we did nothing, 1 when we did something, and -1 when we
 * want to disarm the trap.
 */
int load_trap(int y, int x)
{
	trap_type *t_ptr;
	int i, idx, item;
	int action = 0;

	char o_name[DESC_LEN];
	char ch;
	object_type *o_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	bool correct_launcher = FALSE;

	s16b this_o_idx, next_o_idx = 0;


	/* Scan the current trap list */
	for (idx = 1; idx < t_max; idx++)
	{
		/* Point to this trap */
		t_ptr = &t_list[idx];

		/* Find a trap in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			break;
		}
	}

	/* No monster trap in this location */
	if (idx == t_max) return (FALSE);

	/* Point to this trap (again) */
	t_ptr = &t_list[idx];

	/* Save the screen */
	screen_save(FALSE);


	/* Interact with the trap */
	while (TRUE)
	{
		bool no_objects = FALSE;

		/* List all the items this trap contains */
		for (i = 1, this_o_idx = t_ptr->hold_o_idx; this_o_idx;
			  this_o_idx = next_o_idx, i++)
		{
			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Get the object name */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Print object */
			prt(format("%s", o_name), i, 0);
		}

		/* Print an indication that no objects exist */
		if (i == 1)
		{
			prt("                  (This trap contains no objects yet)", i++, 0);
			no_objects = TRUE;
		}

		/* Print a blank line */
		prt("", i++, 0);

		/* Print some helpful notes */
		move_cursor(i, 0);
		c_roff(TERM_L_BLUE, "     Traps may be loaded with a single melee weapon, a single missile launcher, any number of wands, rods, or staffs, any number of potions or scrolls, or any number of weapons or objects that you wish to be thrown.\n          Traps can usually only contain one type of object.  However, if you have loaded a missile weapon, you also need to supply ammunition (after you load the weapon).  You can also have any number of boulders.\n ", 0, 80);

		/* Instructions (similar to stores, etc.) */
		if (no_objects)
		{
			prt("Press 'd' to drop objects, 'k' to disarm the trap, or ESC to exit.", 0, 0);
		}
		else
		{
			prt("Press 'd' to drop objects, 'g' to get objects, or ESC to exit.", 0, 0);
		}


		/* Get a command */
		ch = inkey(FALSE);

		/* Blank the message line */
		prt("", 0, 0);

		/* Disarm the trap */
		if ((ch == 'k') || (ch == 'K') || (ch == '+'))
		{
			/* Restore screen, exit */
			screen_load();
			return (-1);
		}

		/* Load new objects */
		else if ((ch == 'd') || (ch == 'D'))
		{
			/* Get an item */
			cptr q = "Load which item?";
			cptr s = "You have nothing to load the trap with.";
			item_tester_hook = item_tester_hook_trap;
			if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR)))
				continue;
			item_to_object(o_ptr, item);

			/* Refuse to load cursed items from equipment */
			if ((cursed_cling(o_ptr)) && (item > INVEN_PACK))
			{
				msg_print("Hmmm, it seems to be cursed.");
				message_flush();
				continue;
			}

			/* Refuse to take off some equipment when shapechanged */
			if ((p_ptr->schange) && (item > INVEN_PACK))
			{
				/* Test for items that cannot be taken off */
				if (!item_tester_hook_wear_shapechange(o_ptr))
				{
					msg_print("You cannot take off equipment while shapechanged.");
					msg_print("Use the ']' command to return to your normal form.");
					message_flush();
					continue;
				}
			}

			/* Forbid double-loading with any object other than ammo */
			if ((t_ptr->hold_o_idx) && (!is_missile(o_ptr)) &&
			    (o_ptr->tval != TV_JUNK) && (o_ptr->sval != SV_BOULDER))
			{
				/* Assume no combination possible */
				bool can_combine = FALSE;

				/* See if this item will combine with others already loaded */
				for (this_o_idx = t_ptr->hold_o_idx; this_o_idx;
					  this_o_idx = next_o_idx)
				{
					/* Get the object */
					object_type *j_ptr = &o_list[this_o_idx];

					/* Check for combination */
					if (object_similar(j_ptr, o_ptr)) can_combine = TRUE;
				}

				/* Check to see if this is the appropriate launcher for all the ammo in the trap */
				if (is_missile_weapon(o_ptr))
				{
					int ammo_tval;

					if (o_ptr->tval == TV_SLING)    ammo_tval = TV_SHOT;
					if (o_ptr->tval == TV_BOW)      ammo_tval = TV_ARROW;
					if (o_ptr->tval == TV_CROSSBOW) ammo_tval = TV_BOLT;

					correct_launcher = TRUE;
					for (this_o_idx = t_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
					{
						object_type *j_ptr = &o_list[this_o_idx];

						if (j_ptr->tval != ammo_tval)
						{
							correct_launcher = FALSE;
							break;
						}
						next_o_idx = j_ptr->next_o_idx;
					}
				}

				/* Require suitable object */
				if (!can_combine && !correct_launcher)
				{
					msg_print("Traps can usually contain only one type of object.");
					message_flush();
					continue;
				}
			}

			/* Determine how many items we wish to load the trap with */
			if (o_ptr->number > 1)
			{
				get_quantity_default = o_ptr->number;
				i = (int)get_quantity(format("How many items (0-%d)?", o_ptr->number), 0, o_ptr->number);

				/* Cancel */
				if (!i)	continue;
			}
			else
			{
				i = 1;
			}

			/* Get local object */
			i_ptr = &object_type_body;

			/* Copy the item being loaded */
			object_copy(i_ptr, o_ptr);

			/* Adjust quantity */
			i_ptr->number = i;

			/* Hack -- Adjust charges and timeouts */
			distribute_charges(o_ptr, i_ptr, i_ptr->number);

			/* Reduce (but do not describe) inventory */
			if (item >= 0)
			{
				inven_item_increase(item, -i);
				inven_item_optimize(item);
			}
			else
			{
				floor_item_increase(0 - item, -i);
				floor_item_optimize(0 - item);
			}

			/* See if this item will combine with others already loaded */
			for (this_o_idx = t_ptr->hold_o_idx; this_o_idx;
				  this_o_idx = next_o_idx)
			{
				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Check for combination */
				if (object_similar(o_ptr, i_ptr))
				{
					/* Combine the items */
					object_absorb(o_ptr, i_ptr);

					/* Done */
					i_ptr->number = 0;
					break;
				}

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;
			}

			/* Load the trap with a new object */
			if (i_ptr->number)
			{
				/* Make an object */
				int o_idx = o_pop();

				/* Get the object */
				object_type *j_ptr = &o_list[o_idx];

				/* Copy object */
				object_copy(j_ptr, i_ptr);

				/* Forget location */
				j_ptr->iy = j_ptr->ix = 0;

				/* Object will be held by the trap */
				j_ptr->held_m_idx = -(idx);



				/* Trap has no objects yet */
				if (!t_ptr->hold_o_idx)
				{
					/* Link the trap to the object */
					t_ptr->hold_o_idx = o_idx;

					/* Object goes to the bottom of the pile */
					j_ptr->next_o_idx = 0;
				}
				/* Trap is gaining a launcher, which must come first */
				else if (correct_launcher)
				{
					/* First item becomes second item */
					j_ptr->next_o_idx = t_ptr->hold_o_idx;

					/* Launcher becomes first item in trap */
					t_ptr->hold_o_idx = o_idx;

				}
				/* Trap has objects */
				else
				{
					/* Scan through the list of objects */
					for (this_o_idx = t_ptr->hold_o_idx;; this_o_idx = next_o_idx)
					{
						/* Get the object */
						o_ptr = &o_list[this_o_idx];

						/* This object is at the bottom of the pile */
						if (!o_ptr->next_o_idx)
						{
							/* Link it to the new object */
							o_ptr->next_o_idx = o_idx;

							/* Done */
							break;
						}

						/* Object goes to the bottom of the pile */
						j_ptr->next_o_idx = 0;

						/* Get the next object */
						next_o_idx = o_ptr->next_o_idx;
					}
				}
			}

			/* We did something */
			action = TRUE;

			/* Object loaded -- await next command */
			continue;
		}

		/* Remove existing objects */
		else if ((ch == 'g') || (ch == 'G'))
		{
			/* Note that no objects exist */
			if (!t_ptr->hold_o_idx)
			{
				msg_print("The trap had no objects to remove.");
				message_flush();
			}

			/* Remove the first object (we could get more sophisticated here) */
			else
			{
				/* Get this object */
				o_ptr = &o_list[t_ptr->hold_o_idx];

				/* Trap is now linked to the second object (if any) */
				t_ptr->hold_o_idx = o_ptr->next_o_idx;

				/* Give the object to the character */
				give_object(o_ptr, FALSE);

				/* Flush messages */
				message_flush();

				/* We did something */
				action = TRUE;
			}

			/* Object removed -- await next command */
			continue;
		}

		/* Confirm (or escape) */
		else if ((ch == '\r') || (ch == '\n') || (ch == ESCAPE)) break;

		/* Error */
		else
		{
			msg_print("Unknown command.");
			message_flush();
		}
	}

	/* Restore the screen */
	screen_load();

	/* Return "anything done" */
	return (action);
}



/*
 * Trap attacks in melee, with missile weapons, or by throwing objects.
 *
 * We use a somewhat simplified (and bastardized) version of the code in
 * "attack.c".
 */
static void trap_combat(int mode, int y, int x, object_type *o_ptr,
	object_type *i_ptr, int power, char *m_name, bool *learn)
{
	int i, tmp;
	int dice;
	long die_average, temp, sides;
	int total_deadliness, chance;
	long mult = 100L;
	int damage, resist;

	/* Assume one attack */
	int attacks = 1;

	bool fear = FALSE;

	u32b f1, f2, f3;

	monster_race *r_ptr;
	monster_type *m_ptr;

	int m_idx;

	/* Get the index of the creature in this grid */
	m_idx = cave_m_idx[y][x];

	/* Require that a monster (not a character) be present */
	if (m_idx <= 0) return;

	/* Get the monster in this grid */
	m_ptr = &m_list[m_idx];

	/* Get the monster race */
	r_ptr = &r_info[m_ptr->r_idx];


	/* Monster evaded or resisted */
	resist = monster_evade_or_resist(o_ptr, m_ptr, BLOW_TRAP);

	/* Evaded */
	if (resist == 100) return;

	/* Using melee */
	if (mode == 1)
	{
		/* Require a valid melee weapon */
		if (!is_melee_weapon(o_ptr)) return;

		total_deadliness = o_ptr->to_d;
		chance = 15 + (BTH_PLUS_ADJ * o_ptr->to_h) + power;
	}

	/* Using archery (no accuracy penalty for point-blank range) */
	else if (mode == 2)
	{
		/* Require a valid missile launcher */
		if (!is_missile_weapon(i_ptr)) return;

		/* Require valid missiles */
		if (!is_missile(o_ptr)) return;

		total_deadliness = i_ptr->to_d + o_ptr->to_d;
		chance = 15 + (BTH_PLUS_ADJ * (i_ptr->to_h + o_ptr->to_h)) + power;
	}

	/* Throwing the object (no penalty for point-blank range) */
	else
	{
		total_deadliness = o_ptr->to_d;
		chance = 15 + (BTH_PLUS_ADJ * o_ptr->to_h) + power;
	}

	/* Using archery */
	if (mode == 2)
	{
		/* Hack -- transfer launcher attributes to the missile */
		transfer_attributes_to_missile(o_ptr, i_ptr);
	}

	/* Get weapon, missile, or thrown object flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Base damage dice */
	dice = o_ptr->dd;

	/* We do not allow for critical hits (see trap power multiplier) */

	/* Get the average value of a single damage die. (x10 inflation) */
	die_average = (10 * (o_ptr->ds + 1)) / 2;

	/* Get deadliness from object (100x inflation) */
	apply_deadliness(&mult, total_deadliness);

	/* Deflate multiplier */
	mult = div_round(mult, 100);

	/* Apply trap power multiplier (+20% - +180%) */
	mult += deadliness_conversion[power / 2];

	/* Traps can get multiple melee blows */
	if (mode == 1)
	{
		/* Heavy weapons get fewer blows  XXX */
		int divisor = 10 + (o_ptr->weight / 12);

		/* More powerful traps get more blows */
		attacks = 1 + div_round((2 * power / 3), divisor);

		/* Limit blows */
		if (attacks > 5) attacks = 5;

		/* Apply extra blows */
		attacks += get_object_pval(o_ptr, TR_PVAL_BLOWS);
	}

	/* Using archery */
	else if (mode == 2)
	{
		/* Apply missile weapon multiplier (200% - 500%) */
		mult *= o_ptr->dd + get_object_pval(i_ptr, TR_PVAL_MIGHT);

		/* Handle shots.  Triple crossbows shoot more quickly. */
		if (o_ptr->sval == SV_TRIPLE_XBOW) tmp = 300;
		else                               tmp = 100;

		/* Calculate shooting speed (115% - ~400%) */
		tmp += (get_object_pval(i_ptr, TR_PVAL_SHOTS) * 50) + (3 * power / 2);

		/* Translate into attacks */
		attacks = MAX(1, div_round(tmp, 100));
	}

	/* Throwing */
	else
	{
		/* We are throwing a weapon designed for such use (200% - 1600%) */
		if (f1 & (TR1_THROWING))
		{
			mult += 40 + (9 * power);

			/* We are throwing a perfectly balanced weapon */
			if (f1 & (TR1_PERFECT_BALANCE)) mult += mult / 2;
		}
	}


	/* Apply multiplier to die average (100x inflation) */
	die_average *= mult;

	/* Convert die average to die sides. */
	temp = (2 * die_average) - 1000;

	/* Calculate the actual number of sides to each die. */
	sides = div_round(temp, 1000);


	/* Always have at least one attack */
	if (attacks < 1) attacks = 1;

	/* Use one attack after another */
	for (damage = 0, i = 0; i < attacks; i++)
	{
		/* Check for hit */
		if (!test_hit_combat(chance, r_ptr->ac, 2)) continue;

		/* Roll for damage */
		tmp = damroll(dice, (s16b)sides);

		/* Adjust damage for slays, brands, resists. */
		adjust_dam(&tmp, o_ptr, m_ptr, TRUE);

		/* HACK - help missile and thrown weapons  XXX XXX */
		if ((mode == 2) || (mode == 3)) tmp += 2 + power / 15;

		/* Add to total */
		damage += tmp;
	}

	/* Apply resistances */
	if (resist) damage -= (damage * resist + 50) / 100;

	/* Player is in line of sight */
	if (player_has_los_bold(y, x))
	{
		/* Note hits */
		if (damage > 0)
		{
			msg_format("%^s sets off your cunning trap!", m_name);
		}

		/* Note misses */
		else
		{
			msg_format("%^s dodges your trap.", m_name);
		}
	}

	/* Pass back indicator to learn about used items */
	if (damage > 0 && player_has_los_bold(y, x) && one_in_(10)) *learn = (TRUE);

	/* Monster gets hurt (note that we give experience for trap kills) */
	if (damage > 0)
	{
		if (mon_take_hit(m_idx, -1, (s16b)damage, &fear, NULL)) return;
	}

	/* Take note of fear */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(MSG_FLEE);

		/* Get "the monster" or "it" */
		monster_desc(m_name, m_ptr, 0x40);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}

	/* Monster is not visible, but is nearby */
	else if ((!m_ptr->ml) && (m_ptr->cdis <= 10) && (damage > 0))
	{
		/* Nice messages, but not always appropriate  XXX */
		if (player_has_los_bold(y, x))
			msg_print("You hear moaning.");
		else
			msg_print("You hear anguished yells.");
	}

	return;
}


/*
 * I thought traps only affected players!  Unfair!
 * -LM-
 *
 * Rogues may set traps for monsters.  They can be fairly deadly, but
 * monsters can also sometimes disarm, smash, or fly over them.  They can
 * also become wary.
 *
 * Traps need objects to do damage; damage depends partly on object quality
 * and partly on burglary skill.  The relative importance of these two
 * factors varies greatly on the class of object used; this may cause
 * balance problems.  XXX XXX
 *
 * Traps almost always give experience for killed monsters.
 *
 * Note that skilled burglars can set up traps to use magical devices that
 * he cannot understand, wield weapons that he cannot lift, and fire
 * missiles with deadly accuracy regardless of combat skill.  All of this
 * is deliberate.
 */
static void hit_monster_trap(int who, int y, int x, int t_idx)
{
	monster_type *m_ptr = &m_list[who];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	char m_name[DESC_LEN];

	trap_type *t_ptr = &t_list[t_idx];

	object_type *o_ptr;
	object_type *i_ptr;
	object_type *j_ptr;
	object_type object_type_body;
	s16b this_o_idx, next_o_idx = 0;

	bool do_throw = FALSE;
	bool kill_trap = FALSE;
	int break_chance;

	int ident = 0;

	int skill = get_skill(S_BURGLARY, 0, 100);
	int power;
	bool learn;

	/* Is this grid lit? */
	bool light = (cave_info[y][x] & (CAVE_GLOW | CAVE_SEEN));


	/* Get the monster name (or "something") */
	if (m_ptr->ml) monster_desc(m_name, m_ptr, 0x40);
	else           strcpy(m_name, "Something");


	/* Ghosts can usually avoid traps entirely.  XXX XXX */
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) && (!strchr("gEX", r_ptr->d_char)) &&
	    (!one_in_(light ? 5 : 3)))
	{
		/* Message */
		if (m_ptr->ml) msg_format("%^s floats over your trap.", m_name);

		return;
	}

	/* Evasive monsters can usually avoid traps entirely. */
	if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!one_in_(light ? 4 : 2)))
	{
		if (m_ptr->ml)
		{
			/* Message */
			msg_format("%^s dodges your trap.", m_name);

			/* Note that monster is evasive */
			l_ptr->flags2 |= (RF2_EVASIVE);
		}

		return;
	}


	/* Traps can sometimes be disarmed */
	if ((r_ptr->flags2 & (RF2_SMART)) && (!m_ptr->confused))
	{
		int odds = 8;
		if (m_ptr->stunned) odds = 12;
		if (!light) odds *= 2;
		if (monster_wary(m_ptr)) odds /= 2;

		if (one_in_(odds))
		{
			/* Messages */
			if (m_ptr->ml)
			{
				msg_format("%^s finds your trap and disarms it.", m_name);
			}
			else if (distance(y, x, p_ptr->py, p_ptr->px) < 12)
			{
				msg_print("You hear a click.");
			}

			kill_trap = TRUE;
		}
	}

	/* Any non-ghostly monster can sometimes smash traps */
	else if (!(r_ptr->flags2 & (RF2_PASS_WALL)))
	{
		int odds = 16;
		if (m_ptr->stunned) odds += 6;
		if (m_ptr->confused) odds += 16;
		if (!light) odds = 3 * odds / 2;
		if (monster_wary(m_ptr)) odds /= 2;

		if (one_in_(odds))
		{
			/* Messages */
			if (m_ptr->ml)
			{
				msg_format("%^s finds your trap and smashes it.", m_name);
			}
			else if (distance(y, x, p_ptr->py, p_ptr->px) < 25)
			{
				msg_print("You hear smashing noises.");
			}

			kill_trap = TRUE;
		}
	}

	/* Kill the trap */
	if (kill_trap)
	{
		/* Delete the trap */
		remove_trap(y, x, t_idx);

		/* Return */
		return;
	}


	/* Trap has no objects to use */
	if (!t_ptr->hold_o_idx) return;


	/* Power goes up with skill (ranging from 10 to 136) */
	power = skill;

	/* Highly skilled burglars get extra bonuses */
	if (skill > 70) power += (skill - 70) / 2;
	if (skill > 85) power += (skill - 85);
	if (skill > 94) power += (skill - 94);


	/* Practice the burglary skill */
	skill_being_used = S_BURGLARY;



	/* Get this trap's first object */
	o_ptr = &o_list[t_ptr->hold_o_idx];

	/* Object is a melee weapon */
	if (is_melee_weapon(o_ptr))
	{
		u32b f[4];

		/* Get object attributes */
		object_flags(o_ptr, &f[1], &f[2], &f[3]);

		/* Throw multiple melee weapons */
		if (o_ptr->number > 1) do_throw = TRUE;

		/* Throw anything that's perfectly balanced (not just throwing) */
		else if (f[1] & (TR1_PERFECT_BALANCE)) do_throw = TRUE;

		/* Fight with anything else */
		if (!do_throw)
		{
			/* Engage in combat (type 1 = melee) */
			trap_combat(1, y, x, o_ptr, o_ptr, power, m_name, &learn);

			/* Learn about weapon*/
			if (learn) learn_about_wearable(o_ptr, -1, FALSE);
		}
	}

	/* Object is a missile weapon */
	else if (is_missile_weapon(o_ptr))
	{
		/* Scan the trap's objects, use the first stack of ammo */
		for (this_o_idx = o_ptr->next_o_idx; this_o_idx;
		     this_o_idx = next_o_idx)
		{
			/* Get the object */
			i_ptr = &o_list[this_o_idx];

			/* Is this object ammunition? */
			if (is_missile(i_ptr))
			{
				/* Is this object the right sort of ammunition?  XXX XXX */
				if (((o_ptr->tval == TV_SLING)    && (i_ptr->tval == TV_SHOT)) ||
				    ((o_ptr->tval == TV_BOW)      && (i_ptr->tval == TV_ARROW)) ||
				    ((o_ptr->tval == TV_CROSSBOW) && (i_ptr->tval == TV_BOLT)))
				{
					/* Get local object */
					j_ptr = &object_type_body;

					/* Make a temporary copy of the missiles */
					object_copy(j_ptr, i_ptr);

					/* Engage in combat (type 2 = archery) */
					trap_combat(2, y, x, j_ptr, o_ptr, power, m_name, &learn);

					/* Learn about weapon*/
					if (learn)
					{
						learn_about_wearable(o_ptr, -1, FALSE);
						learn_about_wearable(i_ptr, -1, FALSE);

						/* Copy acquired knowledge to the used missile */
						object_copy(j_ptr, i_ptr);
						j_ptr->number = 1;
					}

					/* Get local object (forget fired missile) */
					j_ptr = &object_type_body;

					/* Restore the old missiles */
					object_copy(j_ptr, i_ptr);

					/* Fire one object at a time */
					j_ptr->number = 1;

					/* Determine chance to break */
					break_chance = breakage_chance(j_ptr);

					/* Drop the fired object */
					drop_near(j_ptr, break_chance, y, x, DROP_NO_MSG);

					/* Delete one object from the trap */
					i_ptr->number--;

					/* Delete this stack of objects from the trap */
					if (!i_ptr->number)
					{
						/* Skip over the empty stack */
						o_ptr->next_o_idx = i_ptr->next_o_idx;

						/* Wipe the object */
						object_wipe(i_ptr);
					}
				}
			}

			/* Get the next object */
			next_o_idx = i_ptr->next_o_idx;
		}
	}

	/* Object is a potion */
	else if (o_ptr->tval == TV_POTION)
	{
		/* Player is in line of sight */
		if (player_has_los_bold(y, x))
		{
			msg_format("%^s sets off your cunning trap!", m_name);
		}

		/* Potion explodes (special case of character = -2) */
		(bool)potion_smash_effect(-2, y, x, o_ptr);

		/* Remove one potion */
		o_ptr->number--;

		/* Remove the stack of potions */
		if (!o_ptr->number)
		{
			/* Trap is now linked to the second object (if any) */
			t_ptr->hold_o_idx = o_ptr->next_o_idx;

			/* Wipe the object */
			object_wipe(o_ptr);
		}
	}

	/* Object is a scroll */
	else if (o_ptr->tval == TV_SCROLL)
	{
		/* Player is in line of sight */
		if (player_has_los_bold(y, x))
		{
			msg_format("%^s sets off your cunning trap!", m_name);
		}

		/* Read the scroll */
		ident = scroll_read_effect(-1, y, x, o_ptr);

		/* The scroll was read */
		if (ident)
		{
			/* Remove one scroll */
			o_ptr->number--;

			/* Remove the stack of scrolls */
			if (!o_ptr->number)
			{
				/* Trap is now linked to the second object (if any) */
				t_ptr->hold_o_idx = o_ptr->next_o_idx;

				/* Wipe the object */
				object_wipe(o_ptr);
			}
		}
	}

	/* Object is food */
	else if (o_ptr->tval == TV_FOOD)
	{
		/* Player is in line of sight */
		if (player_has_los_bold(y, x))
		{
			msg_format("%^s sets off your cunning trap!", m_name);
		}

		/* Use the food */
		(void)food_hit_effect(-2, y, x, o_ptr);

		/* Destroy one sometimes */
		if (breakage_chance(o_ptr))	o_ptr->number--;

        /* Make sure to clear out stack if necessary */
        if (!o_ptr->number)
        {
            /* Trap is now linked to the second object (if any) */
            t_ptr->hold_o_idx = o_ptr->next_o_idx;

            /* Wipe the object */
            object_wipe(o_ptr);
        }
	}

	/* Object is a magical device */
	else if (is_magical_device(o_ptr))
	{
		/* Player is in line of sight */
		if (player_has_los_bold(y, x))
		{
			msg_format("%^s sets off your cunning trap!", m_name);
		}

		/* Use the magical device */
		(void)device_use_effect(-2, power, y, x, o_ptr);
	}

	/* Object is anything else */
	else
	{
		/* Throw it */
		do_throw = TRUE;
	}

	/* Throw an object if requested */
	if (do_throw)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Copy the thrown object */
		object_copy(i_ptr, o_ptr);

		/* Set quantity of new object to 1 */
		i_ptr->number = 1;

		/* Engage in combat (type 3 = throwing) */
		trap_combat(3, y, x, o_ptr, o_ptr, power, m_name, &learn);

		/* Learn about weapon */
		if (learn)
		{
			learn_about_wearable(o_ptr, -1, FALSE);

			/* Copy acquired knowledge to thrown object */
			object_copy(i_ptr, o_ptr);
			i_ptr->number = 1;
		}

		/* Determine chance to break */
		break_chance = breakage_chance(i_ptr);

		/* Drop the thrown object */
		drop_near(i_ptr, break_chance, y, x, DROP_NO_MSG);

		/* Delete one object from the trap */
		o_ptr->number--;

		/* Delete this stack of objects from the trap */
		if (!o_ptr->number)
		{
			/* Trap is now linked to the second object (if any) */
			t_ptr->hold_o_idx = o_ptr->next_o_idx;

			/* Wipe the object */
			object_wipe(o_ptr);
		}
	}

	/* Practice no special skill */
	skill_being_used = S_NOSKILL;


	/* Many kinds of monsters in LOS/earshot become wary */
	(void)make_monsters_wary(y, x, TRUE, TRUE);


	/* Note that objects or the trap itself may be destroyed at this point. */
}




/*
 * Determine if a trap affects the character.
 * Match trap power against player dodging ability, but never hit more than
 * 95% of the time or hit less than 5% of the time.
 */
static int check_trap_hit(int power)
{
	int percent = 0;

	/* Power competes against armor */
	if (power)
	{
		percent = div_round(100 * (power - dodging_ability(100)), power);
	}

	/* Minimum of 5% chance to hit */
	if (percent < 5) percent = 5;

	/* Maximum of 95% chance to hit */
	if (percent > 95) percent = 95;


	/*
	 * This function is called from the trap attack code, which generally
	 * uses the simple RNG.   We temporarily switch over to the complex
	 * RNG for true randomness.
	 */
	Rand_quick = FALSE;

	/* Return hit or miss */
	return (rand_int(100) < percent);
}


/*
 * Loot a trap.
 *
 * Return TRUE if an object was created.
 */
bool loot_trap(int y, int x, int t_idx)
{
	trap_type *t_ptr = &t_list[t_idx];
	int i;

	/* Assume no object */
	int tval = 0, sval = -1, quan = 1, pval = 0;

	object_type *i_ptr;
	object_type forge;

	/* Dangerousness of trap varies, but is usually near current depth */
	int lev = Rand_normal(p_ptr->depth, p_ptr->depth / 6);


	/* Analyze */
	switch (t_ptr->t_idx)
	{
		/* Loose rock */
		case TRAP_LOOSE_ROCK:
		{
			/* Get local object */
			i_ptr = &forge;

			/* Make the object */
			if (make_object(i_ptr, FALSE, FALSE, FALSE))
			{
				/* Coolness */
				msg_print("Hmm.  There was something under this rock.");

				/* Drop the object */
				drop_near(i_ptr, 0, y, x, DROP_HERE);
			}

			break;
		}

		/* Trap door */
		case TRAP_DOOR:
		{
			/* Upon occasion, get Scrolls of Teleport Level */
			if (one_in_(3))
			{
				tval = TV_SCROLL;
				sval = SV_SCROLL_TELEPORT_LEVEL;
				quan = rand_range(2, 5);
			}

			break;
		}

		/* Pits */
		case TRAP_PIT:
		{
			/* Poison loot */
			if (lev > 5)
			{
				/* Poison 'shrooms */
				if (one_in_(2))
				{
					tval = TV_FOOD;
					quan = rand_range(4, 6);

					if      (lev <= 15) sval = SV_FOOD_POISON;
					else if (lev <= 30) sval = SV_FOOD_SICKNESS;
					else if (lev <= 45) sval = SV_FOOD_ENVENOMATION;
					else if (lev <= 60) sval = SV_FOOD_DISEASE;
					else                sval = SV_FOOD_RUINATION;
				}

				/* Melee weapons (random) */
				else
				{
					i = randint(3);
					if      (i == 1) tval = TV_SWORD;
					else if (i == 2) tval = TV_POLEARM;
					else             tval = TV_HAFTED;
				}
			}

			break;
		}

		/* Stat-reducing dart traps */
		case TRAP_DART:
		{
			/* Nothing */
			break;
		}

		/* Discolored spots */
		case TRAP_SPOT:
		{
			/* Magical grenades in elementalist flavours */
			if (one_in_(2))
			{
				tval = TV_POTION;
				sval = SV_POTION_GRENADE;
				quan = rand_range(4, 6);

				i = randint(4);
				if      (i == 1) pval = ESSENCE_ACID;
				else if (i == 2) pval = ESSENCE_ELEC;
				else if (i == 3) pval = ESSENCE_FIRE;
				else             pval = ESSENCE_COLD;
			}

			/* Random magical devices  XXX */
			else
			{
				i = randint(3);
				if      (i == 1) tval = TV_STAFF;
				else if (i == 2) tval = TV_WAND;
				else             tval = TV_ROD;
			}

			break;
		}

		/* Gas traps */
		case TRAP_GAS:
		{
			/* Magical grenades in illusionist flavours */
			if (one_in_(2))
			{
				tval = TV_POTION;
				sval = SV_POTION_GRENADE;
				quan = rand_range(4, 6);

				i = randint(4);
				if      (i == 1) pval = ESSENCE_LITE;
				else if (i == 2) pval = ESSENCE_DARK;
				else if (i == 3) pval = ESSENCE_CONFU;
				else             pval = ESSENCE_MAGIC;
			}

			/* Random magical devices  XXX */
			else
			{
				i = randint(3);
				if      (i == 1) tval = TV_STAFF;
				else if (i == 2) tval = TV_WAND;
				else             tval = TV_ROD;
			}

			break;
		}

		/* Summoning traps */
		case TRAP_SUMMON:
		{
			i = randint(5);

			/* A variety of summoning tools, mostly useless */
			if (i == 1)
			{
				tval = TV_SCROLL;
				if      (lev < 30) sval = SV_SCROLL_SUMMON_MONSTER;
				else if (lev < 60) sval = SV_SCROLL_SUMMON_UNDEAD;
				else               sval = SV_SCROLL_SUMMON_DEMONS;
			}
			else if ((i == 2) && (lev >= 15))
			{
				tval = TV_STAFF;
				sval = SV_STAFF_SUMMONING;
			}
			else if (i == 3)
			{
				tval = TV_WAND;
				sval = SV_WAND_CLONE_MONSTER;
			}
			else if ((i == 4) && (lev >= 20))
			{
				tval = TV_ROD;
				sval = SV_ROD_SUMMON_HITHER;
			}

			/* Some life essence grenades */
			else
			{
				tval = TV_POTION;
				sval = SV_POTION_GRENADE;
				pval = ESSENCE_LIFE;
				quan = rand_range(4, 6);
			}

			break;
		}

		/* Dungeon alteration traps */
		case TRAP_ALTER_DUNGEON:
		{
			i = randint(4);

			/* A variety of destruction tools */
			if ((i == 1) && (lev >= 35))
			{
				tval = TV_SCROLL;
				sval = SV_SCROLL_STAR_DESTRUCTION;
				quan = (lev > 50 ? randint(2) : 1);
			}
			else if ((i == 2) && (lev >= 10))
			{
				tval = TV_STAFF;
				if (lev < 70) sval = SV_STAFF_EARTHQUAKES;
				else          sval = SV_STAFF_DESTRUCTION;
			}
			else if (i == 3)
			{
				tval = TV_JUNK;
				sval = SV_BOULDER;
			}

			/* Some force essence grenades */
			else
			{
				tval = TV_POTION;
				sval = SV_POTION_GRENADE;
				pval = ESSENCE_FORCE;
				quan = rand_range(4, 6);
			}

			break;
		}

		/* Character and equipment-alteration traps */
		case TRAP_HEX:
		{
			/* Magical grenades in destructive flavours */
			if (one_in_(2))
			{
				tval = TV_POTION;
				sval = SV_POTION_GRENADE;
				quan = rand_range(4, 6);

				i = randint(4);
				if      (i == 1) pval = ESSENCE_NETHR;
				else if (i == 2) pval = ESSENCE_CHAOS;
				else if (i == 3) pval = ESSENCE_TIME;
				else             pval = ESSENCE_DEATH;
			}

			/* Random magical devices  XXX */
			else
			{
				i = randint(3);
				if      (i == 1) tval = TV_STAFF;
				else if (i == 2) tval = TV_WAND;
				else             tval = TV_ROD;
			}

			break;
		}

		/* Teleport trap */
		case TRAP_PORTAL:
		{
			i = randint(4);

			/* A variety of teleportation tools */
			if (i == 1)
			{
				tval = TV_SCROLL;
				if (one_in_(2)) sval = SV_SCROLL_PHASE_DOOR;
				else            sval = SV_SCROLL_TELEPORT;
				quan = rand_range(2, 5);
			}
			else if ((i == 2) && (lev >= 35))
			{
				tval = TV_STAFF;
				sval = SV_STAFF_TELEPORTATION;
			}
			else if ((i == 3) && (lev >= 15))
			{
				tval = TV_ROD;
				sval = SV_ROD_BLINKING;
			}

			/* Some nexus essence grenades */
			else
			{
				tval = TV_POTION;
				sval = SV_POTION_GRENADE;
				pval = ESSENCE_NEXUS;
				quan = rand_range(4, 6);
			}

			break;
		}

		/* Murder holes */
		case TRAP_MURDER_HOLE:
		{
			i = randint(3);

			if      (i == 1) tval = TV_SHOT;
			else if (i == 2) tval = TV_ARROW;
			else             tval = TV_BOLT;

			if (lev > rand_range(50, 90)) sval = SV_AMMO_HEAVY;
			else                          sval = SV_AMMO_NORMAL;

			quan = rand_range(10, 15);

			break;
		}

		/* Undefined trap */
		default:
		{
			break;
		}
	}

	/* We have an object we want to make */
	if (tval > 0)
	{
		/* Get local object */
		i_ptr = &forge;

		/* We have a specific object in mind */
		if (sval >= 0)
		{
			/* Make the object */
			object_prep(i_ptr, lookup_kind(tval, sval));

			/* Note success */
			if (i_ptr->tval)
			{
				/* Apply magic */
				apply_magic(i_ptr, lev, FALSE, FALSE, FALSE);
			}
			else return (FALSE);
		}

		/* We want a random object of a given general kind */
		else
		{
			bool success;

			/* Require the given tval */
			required_tval = tval;

			/* Make the object */
			success = make_object(i_ptr, FALSE, FALSE, TRUE);

			/* Clear tval restriction */
			required_tval = tval;

			/* Note failure */
			if (!success) return (FALSE);
		}

		/* Specify pval */
		if (pval) i_ptr->pval = pval;

		/* Specify quantity */
		i_ptr->number = MAX(1, quan);

		/* Drop the object right here (suppress all messages) */
		drop_near(i_ptr, 0, y, x, DROP_HERE | DROP_NO_MSG | DROP_CHAR_DROP);

		/* Note generation of object */
		return (TRUE);
	}

	return (FALSE);
}




/*
 * Handle hitting a real trap.
 *
 * Rewritten in Oangband to allow a greater variety of traps, with
 * effects controlled by dungeon level.
 *
 * To allow a trap to choose one of a variety of effects consistently,
 * the quick RNG is often used, and trap location input as a seed value.
 */
static bool hit_trap_aux(int who, int y, int x, int t_idx)
{
	trap_type *t_ptr = &t_list[t_idx];

	int num;
	int dam = 0;

	/* Assume trap actually does go off */
	bool flag = (who < 0);

	int nastiness, selection;

	/* Use the "simple" RNG to insure that traps are consistent. */
	Rand_quick = TRUE;

	/* Hack -- Use the coordinates of the trap to seed the RNG. */
	Rand_value = GRID(t_ptr->fy, t_ptr->fx);

	/* Disturb the player */
	if (who < 0) disturb(0, 0);


	/* Analyze */
	switch (t_ptr->t_idx)
	{
		/* Loose rock */
		case TRAP_LOOSE_ROCK:
		{
			/* Lift the rock... */
			remove_trap(y, x, t_idx);

			/* ... and peek inside */
			if (TRUE)
			{
				object_type *i_ptr;
				object_type forge;

				/* Use current depth as object level */
				object_level = p_ptr->depth;

				/* Get local object */
				i_ptr = &forge;

				/* Make the object */
				if (make_object(i_ptr, FALSE, FALSE, FALSE))
				{
					/* Coolness */
					msg_print("Hmm.  There was something under this rock.");

					/* Drop the object */
					drop_near(i_ptr, 0, y, x, DROP_HERE);
				}
				else
				{
					/* Aw shucks */
					if (!p_ptr->blind)
						msg_print("There was nothing under this rock.");
				}
			}

			/* Delete the trap */
			remove_trap(y, x, t_idx);

			break;
		}

		/* Glyph */
		case TRAP_GLYPH:
		{
			/* Nothing -- monsters currently can't enter them */
			flag = FALSE;
			break;
		}

		/* Burglar's monster trap */
		case TRAP_MONSTER:
		{
			if (who < 0)
			{
				msg_print("You inspect your cunning trap.");
				flag = FALSE;
			}

			/* Attack monsters */
			else if (who > 0)
			{
				/* Truly random */
				Rand_quick = FALSE;

				hit_monster_trap(who, y, x, t_idx);
				flag = TRUE;

				Rand_quick = TRUE;
			}
			break;
		}

		/* Trap door */
		case TRAP_DOOR:
		{
			if (p_ptr->ffall)
			{
				bool confirmed;

				msg_print("You float over a trap door.");
				confirmed = get_check("Go down to the next level?");

				if (confirmed)
				{
					msg_print("You deliberately jump through the trap door.");
				}
				else break;
			}
			else
			{
				msg_print("You fall through a trap door!");
				dam = damroll(2, 8);
				(void)take_hit(dam, 0, NULL, "falling through a trap door");
			}

			/* New depth */
			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		/* Pits */
		case TRAP_PIT:
		{
			/* Determine how dangerous the trap is allowed to be */
			nastiness = randint(p_ptr->depth);
			if (one_in_(20)) nastiness += 20;
			else if (one_in_(5)) nastiness += 10;

			/* Player is now in pit */
			if ((who < 0) && ((p_ptr->py != y) || (p_ptr->px != x)))
				monster_swap(p_ptr->py, p_ptr->px, y, x);

			/* Center on player */
			y = p_ptr->py;
			x = p_ptr->px;

			/* Pit of daggers */
			if ((nastiness > 70) && (!one_in_(3)))
			{
				msg_print("You fall into a pit of daggers!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid setting off the daggers.");
				}

				else
				{
					/* A trap of Morgul */
					if (one_in_(6))
					{
						Rand_quick = FALSE;

						/* Lots of damage */
						dam = damroll(20, 20);

						/* Take the damage */
						(void)take_hit(dam, 0,
						   "A single coldly gleaming dagger pierces you deeply!",
							"a Blade of Morgul");

						/* Character is still alive */
						if (!p_ptr->is_dead)
						{
							/* Activate the Black Breath */
							p_ptr->black_breath = TRUE;
							message(MSG_L_RED, 200, "You feel the Black Breath infecting your soul...");

							/* Undead may be attracted */
							if (one_in_(2))
							{
								msg_print("Undead appear and call you to them!");
								summon_specific(y, x, FALSE, p_ptr->depth,
									SUMMON_UNDEAD, rand_range(3, 5));
							}
						}

						/* Morgul-traps are one-time only */
						remove_trap(y, x, t_idx);
					}

					/* Dagger trap */
					else
					{
						Rand_quick = FALSE;

						/* Activate the ordinary daggers */
						dam += damroll(8, 8 + p_ptr->depth / 2);

						/* Take the damage. */
						(void)take_hit(dam, 0, "Daggers pierce you everywhere!",
							"a pit of daggers");

						Rand_quick = TRUE;
					}

					/* Cut the character */
					(void)set_cut(p_ptr->cut + randint(dam));
				}
			}

			/* Poisoned spiked pit */
			else if ((nastiness > 40) && (!one_in_(3)))
			{
				msg_print("You fall into a spiked pit!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the spikes.");
				}

				/* Disease pit */
				else if (one_in_(4))
				{
					Rand_quick = FALSE;

					/* Base damage */
					dam = damroll(5, 5 + p_ptr->depth / 2);

					/* Cause disease, adjust damage */
					disease(&dam);

					/* Take the damage */
					(void)take_hit(dam, 0, "You are impaled on disease-bearing spikes!",
					   "a disease pit");

					/* Cause cuts */
					(void)set_cut(p_ptr->cut + randint(dam));

					Rand_quick = TRUE;
				}

				else
				{
					Rand_quick = FALSE;

					/* Base damage */
					dam = damroll(4, 4 + p_ptr->depth / 2);

					if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
					{
						dam = 3 * dam / 2;
						(void)set_poisoned(p_ptr->poisoned + randint(dam));
					}

					/* Take the damage */
					(void)take_hit(dam, 0, "You are impaled on venomous spikes!",
					   "a poison spiked pit");

					/* Cause cuts */
					(void)set_cut(p_ptr->cut + randint(dam));

					if (p_ptr->resist_pois || p_ptr->oppose_pois)
					{
						if (!p_ptr->is_dead)
							msg_print("The poison does not affect you!");
					}

					Rand_quick = TRUE;
				}
			}

			/* Spiked pit */
			else if ((nastiness > 15) && (!one_in_(3)))
			{
				msg_print("You fall into a spiked pit!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the spikes.");
				}
				else
				{
					Rand_quick = FALSE;

					/* Base damage */
					dam = damroll(3, 3 + p_ptr->depth / 4);

					/* Take the damage */
					(void)take_hit(dam, 0, "You are impaled!", "a spiked pit");

					/* Cause cuts */
					(void)set_cut(p_ptr->cut + randint(dam));

					Rand_quick = TRUE;
				}
			}

			/* Ordinary pit in all other cases */
			else
			{
				msg_print("You fall into a pit!");
				if (p_ptr->ffall)
				{
					msg_print("You float gently to the bottom of the pit.");
				}
				else
				{
					Rand_quick = FALSE;

					dam = damroll(2, 6);
					(void)take_hit(dam, 0, NULL, "a pit trap");

					Rand_quick = TRUE;
				}
			}

			break;
		}

		/* Stat-reducing dart traps */
		case TRAP_DART:
		{
			cptr msg = "";
			bool sust = FALSE;

			/* Decide if the dart hits */
			if (check_trap_hit(50 + p_ptr->depth))
			{
				/* Select a stat to drain */
				selection = rand_int(6);

				/* Note sustain, otherwise colorful message */
				if (selection == A_STR)
				{
					if (p_ptr->sustain_str) sust = TRUE;
					else msg = "A small dart saps your strength!";
				}
				else if (selection == A_INT)
				{
					if (p_ptr->sustain_int) sust = TRUE;
					else msg = "A small dart attacks your mind!";
				}
				else if (selection == A_WIS)
				{
					if (p_ptr->sustain_wis) sust = TRUE;
					else msg = "A small dart hits you!  You feel more naive!";
				}
				else if (selection == A_DEX)
				{
					if (p_ptr->sustain_dex) sust = TRUE;
					else msg = "A small dart hits you; you suddenly feel clumsy!";
				}
				else if (selection == A_CON)
				{
					if (p_ptr->sustain_con) sust = TRUE;
					else msg = "A small dart drains away your health!";
				}
				else if (selection == A_CHR)
				{
					if (p_ptr->sustain_chr) sust = TRUE;
					else msg = "A small dart twists your features!";
				}

				Rand_quick = FALSE;

				/* Note sustain */
				if (sust)
				{
					msg = "A small dart hits you.";
				}

				/* Hit the character */
				dam = damroll(1, 4);
				(void)take_hit(dam, 0, msg, "a dart trap");

				/* Drain the stat */
				if ((!p_ptr->is_dead) && (!sust))
				{
					/* Determine how dangerous the trap is allowed to be */
					nastiness = randint(p_ptr->depth);

					/* Decide how much to drain the stat by */
					if ((nastiness > 25) && (one_in_(4)))
					{
						num = randint(5);
					}
					else num = 1;

					/* Drain the stat */
					(void)dec_stat(selection, num, FALSE);
				}

				Rand_quick = TRUE;
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		/* Discolored spots */
		case TRAP_SPOT:
		{
			cptr msg = "";

			/* Determine how dangerous the trap is allowed to be. */
			nastiness = randint(p_ptr->depth);
			if (one_in_(5)) nastiness += 10;

			/* Pick an elemental attack type. */
			selection = randint(4);


			/* Electricity trap */
			if (selection == 1)
			{
				if ((nastiness >= 40) && (one_in_(2)))
				{
					Rand_quick = FALSE;

					msg = "You are struck by lightning!";
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg = "You get zapped!";
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				elec_dam(dam, 0, msg, "an electricity trap");
				Rand_quick = TRUE;

			}

			/* Frost trap */
			if (selection == 2)
			{
				if ((nastiness >= 40) && (one_in_(2)))
				{
					Rand_quick = FALSE;

					msg = "You are lost within a blizzard!";
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg = "You are coated in frost!";
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				cold_dam(dam, 0, msg, "a frost trap");
				Rand_quick = TRUE;
			}

			/* Fire trap */
			if (selection == 3)
			{
				if ((nastiness >= 40) && (one_in_(2)))
				{
					Rand_quick = FALSE;

					msg = "You are enveloped in a column of fire!";
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg = "You are surrounded by flames!";
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				fire_dam(dam, 0, msg, "a fire trap");
				Rand_quick = TRUE;
			}

			/* Acid trap */
			if (selection == 4)
			{
				if ((nastiness >= 40) && (one_in_(2)))
				{
					Rand_quick = FALSE;

					msg = "A cauldron of acid is tipped over your head!";
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg = "You are splashed with acid!";
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				acid_dam(dam, 0, msg, "an acid trap");
				Rand_quick = TRUE;
			}

			break;
		}

		/* Gas traps */
		case TRAP_GAS:
		{
			selection = randint(4);

			/* blinding trap */
			if (selection == 1)
			{
				msg_print("You are surrounded by a black gas!");
				if (!p_ptr->resist_blind)
				{
					Rand_quick = FALSE;

					(void)set_blind(p_ptr->blind + rand_range(30, 45),
						"It gets into your eyes!");

					Rand_quick = TRUE;
				}
			}

			/* Confusing trap */
			if (selection == 2)
			{
				msg_print("You are surrounded by a gas of scintillating colors!");
				if (!p_ptr->resist_confu)
				{
					Rand_quick = FALSE;

					(void)set_confused(p_ptr->confused + rand_range(10, 30));

					Rand_quick = TRUE;
				}
			}

			/* Poisoning trap */
			if (selection == 3)
			{
				msg_print("You are surrounded by a pungent green gas!");
				if (!p_ptr->resist_pois || !p_ptr->oppose_pois)
				{
					if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
					{
						Rand_quick = FALSE;

						(void)set_poisoned(p_ptr->poisoned + rand_range(10, 30));

						Rand_quick = TRUE;
					}
					else
					{
						Rand_quick = FALSE;

						(void)set_poisoned(p_ptr->poisoned + rand_range(5, 10));

						Rand_quick = TRUE;
					}
				}
			}

			/* Sleeping trap */
			if (selection == 4)
			{
				msg_print("You are surrounded by a strange white mist!");
				if (!p_ptr->free_act)
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_range(5, 10));
				}
			}

			break;
		}

		/* Summoning traps */
		case TRAP_SUMMON:
		{
			/* Sometimes summon thieves */
			if ((p_ptr->depth > 8) && (one_in_(5)))
			{
				message(MSG_SUM_MONSTER, 0, "You have aroused a den of thieves!");

				Rand_quick = FALSE;

				(void)summon_specific(y, x, FALSE, p_ptr->depth + 2,
						SUMMON_THIEF, rand_range(3, 5));

				Rand_quick = TRUE;
			}

			/* Sometimes summon a nasty unique */
			else if (one_in_(8))
			{
				message(MSG_SUM_MONSTER, 0, "You are enveloped in a cloud of smoke!");

				Rand_quick = FALSE;

				(void)summon_specific(y, x, FALSE, p_ptr->depth + 5,
					SUMMON_UNIQUE, 1);

				Rand_quick = TRUE;
			}

			/* Otherwise, the ordinary summon monsters */
			else
			{
				message(MSG_SUM_MONSTER, 0, "You are enveloped in a cloud of smoke!");

				Rand_quick = FALSE;

				(void)summon_specific(y, x, FALSE, p_ptr->depth + 2,
					0, rand_range(3, 5));

				Rand_quick = TRUE;
			}

			/* These are all one-time traps */
			remove_trap(y, x, t_idx);

			break;
		}

		/* Dungeon alteration traps */
		case TRAP_ALTER_DUNGEON:
		{
			/* Determine how dangerous the trap is allowed to be. */
			nastiness = randint(p_ptr->depth);
			if (one_in_(5)) nastiness += 10;

			/* Make room for alterations */
			remove_trap(y, x, t_idx);

			/* Everything truly random from here on */
			Rand_quick = FALSE;

			/* Dungeon destruction trap */
			if ((nastiness > 60) && (one_in_(12)))
			{
				msg_print("The dungeon is smashed by hammer blows!");

				(void)destroy_level(FALSE);

				msg_print("An ear-splitting howl shatters your mind!");

				/* the player is hard-hit (ignore resistances) */
				(void)set_confused(p_ptr->confused + rand_range(20, 30));
				(void)set_blind(p_ptr->blind + rand_range(30, 45), NULL);
				(void)set_stun(p_ptr->stun + rand_range(50, 100));

				if (!p_ptr->wraithform)
				{
					dam = damroll(15, 15);
					(void)take_hit(dam, 0, NULL, "a dungeon destruction trap");
				}
			}

			/* Collapse ceiling trap */
			if ((nastiness > 35) && (one_in_(3)))
			{
				(void)collapse_ceiling(y, x, p_ptr->depth);
			}

			/* Earthquake trap */
			else if ((nastiness > 20) && (one_in_(4)))
			{
				msg_print("A tremor shakes the dungeon around you.");
				earthquake(y, x, 10);
			}

			/* Characters in wraithform are safe from rock traps */
			else if (!p_ptr->wraithform)
			{
				/* Falling rock trap */
				if (nastiness >= 6)
				{
					(void)take_hit(damroll(2, p_ptr->depth), 0,
						"A boulder falls on your head.", "a falling boulder");

					(void)set_stun(p_ptr->stun + rand_range(10, 20));

					make_boulder(y, x, p_ptr->depth);
				}

				/* A few pebbles */
				else
				{
					dam = damroll(1, 8);
					(void)take_hit(dam, 0, "A bunch of pebbles rain down on you.",
						"falling pebbles");
				}
			}

			Rand_quick = TRUE;

			break;
		}

		/*
		 * Various char and equipment-alteration traps, lumped together
		 * to avoid any one effect being too common (some of them can be
		 * rather nasty).
		 */
		case TRAP_HEX:
		{
			/* Random effect, based on grid location. */
			int choice = rand_int(100);

			/* These are all one-time traps */
			remove_trap(y, x, t_idx);

			/* Everything truly random from here on */
			Rand_quick = FALSE;

			/* Trap of fire off magical devices */
			if (choice < 25)
			{
				/* Fire off some charges */
				fire_off_devices(20 + p_ptr->depth / 2);
			}

			/* Trap of forgetting */
			else if (choice < 40)
			{
				if (check_save(25 + p_ptr->depth))
				{
					msg_print("You hang on to your memories!");
				}
				else
				{
					(void)lose_all_info("Your memories fade away.");
				}
			}

			/* Trap of draining */
			else if (choice < 55)
			{
				/* Drain a device */
				(void)apply_draining(p_ptr->depth);
			}

			/* Trap of remold player */
			else if (choice < 85)
			{
				/* Message */
				msg_print("You feel yourself being twisted by wild magic!");

				/* Resist */
				if ((p_ptr->resist_nexus) || (check_save(25 + p_ptr->depth)))
				{
					msg_print("You resist the effects!");
				}

				/* Shuffle a pair of stats */
				else
				{
					shuffle_stats(1);
				}
			}

			/* Time ball trap */
			else
			{
				msg_print("Time twists around you!");

				/* Target the player with a radius 1 ball attack. */
				(void)project_ball(0, 1, y, x, y,
				                   x, p_ptr->depth, GF_TIME, 0L, 20);
			}

			Rand_quick = TRUE;

			break;
		}

		/* Teleport and alter reality trap */
		case TRAP_PORTAL:
		{
			/* Determine how dangerous the trap is allowed to be. */
			nastiness = randint(p_ptr->depth);

			if ((nastiness < 10) || (!one_in_(3)) || (p_ptr->resist_nexus))
			{
				msg_print("You teleport across the dungeon.");

				Rand_quick = FALSE;
				teleport_player(150, FALSE, FALSE);
				Rand_quick = TRUE;
			}
			else if ((nastiness < 40) || (!one_in_(3)))
			{
				(void)set_dancing_feet(p_ptr->dancing_feet +
						rand_range(10, 20),
						"You start to blink around uncontrollably!", FALSE);
			}

			/* Trap of teleport off the level (rare) */
			else
			{
				msg_print("You are teleported to an entirely different dungeon!");

				/* Leaving */
				p_ptr->leaving = TRUE;
			}

			break;
		}

		/* Murder holes */
		case TRAP_MURDER_HOLE:
		{
			/* hold the object info. */
			object_type *o_ptr;
			object_type object_type_body;

			/* hold the missile type and name. */
			int sval = 0;
			int tval = 0;
			cptr missile_name = "";

			/* Determine how dangerous the trap is allowed to be. */
			nastiness = rand_range(p_ptr->depth / 2, p_ptr->depth);

			/* Determine the missile type and base damage. */
			if (nastiness < 15)
			{
				missile_name = "shot";
				dam = damroll(3, 3 * nastiness / 4);
				tval = TV_SHOT;
				sval = SV_AMMO_NORMAL;
			}
			else if (nastiness < 30)
			{
				missile_name = "arrow";
				dam = damroll(3, nastiness);
				tval = TV_ARROW;
				sval = SV_AMMO_NORMAL;
			}
			else if (nastiness < 45)
			{
				missile_name = "bolt";
				dam = damroll(3, 5 * nastiness / 4);
				tval = TV_BOLT;
				sval = SV_AMMO_NORMAL;
			}
			else if (nastiness < 60)
			{
				missile_name = "seeker shot";
				dam = damroll(3, 3 * nastiness / 2);
				tval = TV_SHOT;
				sval = SV_AMMO_HEAVY;
			}
			else if (nastiness < 75)
			{
				missile_name = "seeker arrow";
				dam = damroll(3, 7 * nastiness / 4);
				tval = TV_ARROW;
				sval = SV_AMMO_HEAVY;
			}
			else
			{
				missile_name = "seeker bolt";
				dam = damroll(3, 2 * nastiness);
				tval = TV_BOLT;
				sval = SV_AMMO_HEAVY;
			}

			/* Determine if the missile hits */
			if (check_trap_hit(75 + p_ptr->depth))
			{
				msg_format("A %s hits you from above.", missile_name);

				Rand_quick = FALSE;

				/* Critical hits */
				if (one_in_(3))
				{
					msg_print("It was well-aimed!");
					dam *= 2;
				}
				else if (one_in_(2))
				{
					msg_print("It gouges you!");
					dam = 3 * dam / 2;

					/* Cut the player */
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				/* Player armor reduces damage */
				dam = 1 + dam * 110 / (110 + p_ptr->ac + p_ptr->to_a);

				Rand_quick = TRUE;

				(void)take_hit(dam, 0, NULL, format("a %s trap", missile_name));
			}

			/* Notice misses. */
			else msg_format("A %s whizzes by your head.", missile_name);

			/* Get local object */
			o_ptr = &object_type_body;

			/* Make a missile, identify it, and drop it near the player. */
			object_prep(o_ptr, lookup_kind(tval, sval));
			object_aware(o_ptr);
			object_known(o_ptr);
			drop_near(o_ptr, 0, y, x, 0x00);

			/* These are one-time traps */
			remove_trap(y, x, t_idx);

			break;
		}

		/* Undefined trap */
		default:
		{
			if (who < 0)
			{
				dam = damroll(3, 4);
				(void)take_hit(dam, 0, "A dagger is thrown at you from the shadows!",
					"a dagger trap");
			}

			break;
		}
	}

	/* Revert to usage of the complex RNG. */
	Rand_quick = FALSE;

	/* Return "something happened" */
	return (flag);
}

/*
 * Hit a trap.  Who of 0 fires off all traps.  Who of less than 0 fires off
 * traps that affect characters.  Who of greater than zero fires off traps
 * that affect monsters.
 *
 * Return TRUE if a trap did actually go off.
 */
bool hit_trap(int who, int y, int x)
{
	int i;
	bool trap_went_off = FALSE;

	/* Character stumbles into invisible, nasty traps */
	if (who < 0)
	{
		/* Count the nasty, hidden traps here */
		int num = nasty_traps(y, x, -1);

		/* Oops.  We've walked right into trouble. */
		if      (num == 1) msg_print("You stumble upon a trap!");
		else if (num >  1) msg_print("You stumble upon some traps!");
	}

	/* Scan the current trap list */
	for (i = 0; i < t_max; i++)
	{
		/* Point to this trap */
		trap_type *t_ptr = &t_list[i];

		/* Find all traps in this position */
		if ((t_ptr->fy == y) && (t_ptr->fx == x))
		{
			/* Sometimes require a trap that does something to the character */
			if ((who < 0) &&
				(!(t_kind_info[t_ptr->t_idx].flags & (TRAP_PLAY)))) continue;

			/* Sometimes require a trap that does something to monsters */
			if ((who > 0) &&
				(!(t_kind_info[t_ptr->t_idx].flags & (TRAP_KILL)))) continue;

			/* Fire off the trap */
			if (hit_trap_aux(who, y, x, i)) trap_went_off = TRUE;

			/* Trap becomes visible (always XXX) */
			if (who < 0)
			{
				t_ptr->flags |= (TRAP_VISIBLE);
				cave_info[y][x] |= (CAVE_MARK);
			}
		}
	}

	/* Verify traps (remove marker if appropriate) */
	(void)verify_trap(y, x, 0);

	/* Return */
	return (trap_went_off);
}


