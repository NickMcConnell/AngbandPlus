#define SQUELCH_C
/* File: squelch.c */

/* Purpose: Auto-squelching */

#include "angband.h"

/*
 * Object squelching code. This is run whenever an object (either in the
 * inventory or on the ground under the character) may need to be squelched,
 * and runs the squelch command on it whenever such objects are found.
 */

/*
 * Return the minimum squelch setting for an object for it to be suitable for
 * destruction.
 */
static int PURE object_quality(object_ctype *o_ptr)
{
	switch (find_feeling(o_ptr))
	{
		case SENSE_C_ART: case SENSE_C_EGO:
			return HIDE_V_BAD;
		case SENSE_CP_OBJ: case SENSE_C_OBJ: case SENSE_BROKEN:
			return HIDE_CURSED;
		case SENSE_U_OBJ:
			return HIDE_AVERAGE;
		case SENSE_G_OBJ:
			return HIDE_GOOD;
		case SENSE_G_EGO:
			return HIDE_V_GOOD;

		/* Artefacts are the best category of weapon. */
		case SENSE_G_ART: case SENSE_Q_ART:

		/* Not enough is known to squelch by quality. */
		case SENSE_Q_OBJ: case SENSE_QP_OBJ: case SENSE_GP_OBJ:

		/* Empty things cannot be squelched on this basis. */
		case SENSE_EMPTY:

		/* Feelings aware items never have. */
		case SENSE_TRIED: case SENSE_PCURSE:
		default:
			return HIDE_ALL;
	}
}


/*
 * Determine whether a particular object should be squelched or not.
 * It gets a bit paranoid at times but I'm pretty sure that it won't
 * destroy anything you didn't tell it to...  Sensing stuff in the rest
 * of the program can be a bit strange.
 * Broken items get squelched with cursed. Is that right?
 * Items sensed as 'uncursed' but not known are ignored.
 */
static PURE bool destroy_it(object_ctype *o1_ptr)
{
	cptr s;
	object_type o_ptr[1];

	/* Unsetting allow_squelch prevents all squelching. */
	if (!allow_squelch) return FALSE;

	object_info_known(o_ptr, o1_ptr);

	/* Don't hide hidden things. */
	if (hidden_p(o_ptr)) return FALSE;

	/* Things inscribed with !k or !K won't be destroyed! */
	s = get_inscription(o_ptr);
	if (strstr(s, "!k") || strstr(s, "!K")) return FALSE;

	/*
	 * Other things are destroyed if the "destroy" setting is at least as good
	 * as that justified by the feeling.
	 */
	return (k_info[o_ptr->k_idx].squelch >= object_quality(o1_ptr));
}


/*
 * If o_ptr points to a stack of objects in which one or more is to be
 * squelched, set *o_ptr to it and return TRUE.
 * Otherwise, set *o_ptr to NULL and return FALSE.
 */
static bool squelch_object(object_type **o_ptr)
{
	/* Find the next squelched object, if any. */
	for (; *o_ptr; next_object(o_ptr))
	{
		if (destroy_it(*o_ptr)) return TRUE;
	}

	/* Nothing left to squelch, so finish. */
	return FALSE;
}


/*
 * Process a sequence of commands to squelch a stack of objects.
 * Finish once the player has examined (and possibly squelched) everything,
 * if any of the commands take energy or if various "impossible" things happen.
 *
 * This should be similar to (but simpler than) process_player(),
 *
 * This messes around with keymaps in order to allow keymap to be a sequence
 * of keypresses to execute for every squelched object. It would not work if
 * inkey() did anything more complex than advance inkey_next or set it to 0.
 *
 * These commands should take no energy, although the game will simply stop
 * processing after the first command which does take energy if it does so.
 *
 * The object being squelched will always be accessible as ! at the first
 * command,start as !, but any action which may involve other objects should
 * inscribe the object appropriately beforehand.
 *
 * If there is a keymap in operation as squelching begins, the command sequence
 * here is added to the beginning of it and it is used for any additional
 * keypresses the game requires when processing each object. The keymap given
 * here is reapplied whenever the game reaches the command prompt after it has
 * finished one iteration.
 */
static void process_objects(object_type *o_ptr)
{
	cptr keymap = "K!";

	/* Remember how much energy should really be used. */
	int old_energy_use = energy_use;

	/* Nothing to do if squelching is disabled. */
	if (!allow_squelch) return;

	/* Place the cursor on the player */
	move_cursor_relative(py, px);

	/* Repeat until some energy is used. */
	for (energy_use = 0; !energy_use && o_ptr; )
	{
		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();


		/* Refresh (optional) */
		if (fresh_before) Term_fresh();

		/* Select a new squelched object if the old one is finished with. */
		if (gnext_clear())
		{
			/* Find the next object. */
			if (!squelch_object(&o_ptr))
			{
				/* None left. */
				break;
			}
		}

		/* Weird conditions. */
		if (p_ptr->paralyzed || p_ptr->stun >= 100 || 
			!alive || death || new_level_flag || inventory[INVEN_PACK].k_idx)
		{
			break;
		}

		/* Hack - let the player clear any pending keys. */
		msg_print(NULL);

		/* Highlight this object. */
		object_track(o_ptr);

		/* Find the next object now, as this one may not exist later. */
		next_object(&o_ptr);

		/* Start the keymap again. */
		set_gnext(keymap);

		/* Get a command (normal) */
		request_command(FALSE);

		/* Process the command */
		process_command();
	}

	/* Add back the energy which was used initially. */
	energy_use += old_energy_use;
}

/*
 * Start searching the floor for "crap".
 * Return FALSE if the game is already squelching something.
 * Unset PN_FSQUELCH, as t
 */
void squelch_grid(void)
{
	int o = cave[py][px].o_idx;
	if (o) process_objects(o_list+o);
}

/*
 * Start searching the inventory for "crap".
 * Return FALSE if the game is already squelching something.
 */
void squelch_inventory(void)
{
	process_objects(inventory+INVEN_TOTAL-1);
}
