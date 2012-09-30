/* File: fields.c */

/* Purpose: Field code */

/*
 * Copyright (c) 2000 Steven Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "grid.h"
#include "script.h"


/*
 * Is the field visible to the player?
 */
static bool field_visible(field_type *f_ptr)
{
	int x = f_ptr->fx;
	int y = f_ptr->fy;

	/* Refuse "illegal" locations */
	if (in_boundsp(x, y))
	{
		/* Can the player see the square? */
		if (player_has_los_grid(parea(x, y)))
		{
			return (TRUE);
		}
	}
	
	return (FALSE);
}

/*
 * Notice changes to a field
 */
void notice_field(field_type *f_ptr)
{
	if (field_visible(f_ptr))
	{
		/* Note + Lite the spot */
		note_spot(f_ptr->fx, f_ptr->fy);
	}
}

/*
 * The name of a field
 */
cptr field_name(const field_type *f_ptr)
{
	return (t_info[f_ptr->t_idx].name);
}

/*
 * Excise a field from a stack
 */
void excise_field_idx(int fld_idx)
{
	/* Field */
	field_type *f_ptr = &fld_list[fld_idx];
	
	field_type *j_ptr = NULL;
	field_type *q_ptr;
	
	int y = j_ptr->fy;
	int x = j_ptr->fx;

	s16b *f_idx_ptr = &area(x, y)->fld_idx;
	
	/* Scan all fields in the list */
	FLD_ITT_START (*f_idx_ptr, q_ptr)
	{
		/* Hack - Done? */
		if (q_ptr == f_ptr)
		{
			/* No previous */
			if (!j_ptr)
			{
				/* Remove from list */
				*f_idx_ptr = q_ptr->next_f_idx;
			}

			/* Real previous */
			else
			{
				/* Remove from list */
				j_ptr->next_f_idx = q_ptr->next_f_idx;
			}

			/* Forget next pointer */
			q_ptr->next_f_idx = 0;

			/* Done */
			break;
		}

		/* Save previous object */
		j_ptr = q_ptr;
	}
	FLD_ITT_END;
}


/*
 * Delete a field
 *
 * Handle "lists" of fields correctly.
 */
void delete_field_ptr(field_type *f_ptr)
{
	int x = f_ptr->fx;
	int y = f_ptr->fy;
	cave_type *c_ptr = area(x, y);
	field_type *j_ptr;
	field_type *q_ptr = NULL;
	
	/* Paranoia */
	if (f_ptr->region != cur_region) quit("Trying to find unregioned field");
	
	/* Find field to delete */
	FLD_ITT_START (c_ptr->fld_idx, j_ptr);
	{
		/* Found it? */
		if (j_ptr == f_ptr)
		{
			/* Delete it */
			if (q_ptr)
			{
				q_ptr->next_f_idx = f_ptr->next_f_idx;
			}
			else
			{
				c_ptr->fld_idx = f_ptr->next_f_idx;
			}
			
			/* Notice the change */
			notice_field(f_ptr);

			/* Wipe the field */
			field_wipe(f_ptr);

			/* Count fields */
			fld_cnt--;
			
			return;
		}
	
		/* Remember previous field */
		q_ptr = j_ptr;
	}
	FLD_ITT_END;
	
	/* We shouldn't get here! */
	quit("Cannot find field to delete!");
	return;
}


/*
 * Deletes all fields at given location
 *
 * Note it does not display the changes on screen
 */
void delete_field_location(cave_type *c_ptr)
{
	field_type *f_ptr;
	
	/* Scan all fields in the grid */
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Wipe the field */
		field_wipe(f_ptr);

		/* Count fields */
		fld_cnt--;
	}
	FLD_ITT_END;

	/* Nothing left */
	c_ptr->fld_idx = 0;
}


/*
 * Deletes all fields at given location
 */
void delete_field(int x, int y)
{
	cave_type *c_ptr;

	/* Refuse "illegal" locations */
	if (!in_bounds2(x, y)) return;

	/* Grid */
	c_ptr = area(x, y);

	delete_field_location(c_ptr);

	/* Paranoia */
	if (!in_boundsp(x, y)) return;

	/* Note + Lite the spot */
	if (character_dungeon) note_spot(x, y);
}


/*
 * Move a field from index i1 to index i2 in the field list
 */
static void compact_fields_aux(int i1, int i2)
{
	int i;

	cave_type *c_ptr;

	field_type *f_ptr;

	int y, x;


	/* Do nothing */
	if (i1 == i2) return;


	/* Repair fields */
	for (i = 1; i < fld_max; i++)
	{
		/* Acquire field */
		f_ptr = &fld_list[i];

		/* Skip "dead" fields */
		if (!f_ptr->t_idx) continue;

		/* Repair "next" pointers */
		if (f_ptr->next_f_idx == i1)
		{
			/* Repair */
			f_ptr->next_f_idx = i2;
		}
	}


	/* Acquire object */
	f_ptr = &fld_list[i1];

	/* Acquire location */
	y = f_ptr->fy;
	x = f_ptr->fx;

	/* If the square exists */
	if ((y) && (x))
	{
		/* Acquire grid */
		c_ptr = area(x, y);

		/* Repair grid */
		if (c_ptr->fld_idx == i1)
		{
			/* Repair */
			c_ptr->fld_idx = i2;
		}
	}

	/* Structure copy */
	fld_list[i2] = fld_list[i1];

	/* Wipe the hole */
	field_wipe(f_ptr);
}


/*
 * Compact and Reorder the field list
 *
 * This function can be very dangerous, use with caution!
 *
 * When actually "compacting" field, we base the saving throw on a
 * combination of field level, distance from player, and current
 * "desperation".
 *
 * After "compacting" (if needed), we "reorder" the fields into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_fields(int size)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int y, x, num, cnt;
	int cur_lev, cur_dis, chance;
	s16b i;

	field_thaum *t_ptr;

	/* Compact */
	if (size)
	{
		/* Message */
		msgf("Compacting fields...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
	}


	/* Compact at least 'size' fields */
	for (num = 0, cnt = 1; num < size; cnt++)
	{
		/* Get more vicious each iteration */
		cur_lev = 5 * cnt;

		/* Get closer each iteration */
		cur_dis = 5 * (20 - cnt);

		/* Examine the fields */
		for (i = 1; i < fld_max; i++)
		{
			field_type *f_ptr = &fld_list[i];

			byte fld_level = 100;

			/* Skip dead fields */
			if (!f_ptr->t_idx) continue;

			/* Point to the field type */
			t_ptr = &t_info[f_ptr->t_idx];

			switch (t_ptr->type)
			{
				case FTYPE_NOTHING:
				{
					/* This field is dead - get rid of it */
					fld_level = 0;

					break;
				}
				case FTYPE_TRAP:
				{
					/* Traps can be gotten rid of safely */
					fld_level = 100;

					break;
				}
				case FTYPE_DOOR:
				{
					/* Spikes / locked doors can be turned off if required */
					fld_level = 80;

					break;
				}
				case FTYPE_BUILD:
				{
					/* Buildings should not be removed */
					fld_level = 255;

					break;
				}
				case FTYPE_FEAT:
				{
					/* General features are important */
					fld_level = 250;

					break;
				}
				case FTYPE_QUEST:
				{
					/* Quest events can be important */
					fld_level = 150;

					break;
				}
				case FTYPE_FIELD:
				{
					/* Does the field have a counter? */
					if (t_ptr->count_init)
					{
						/* Compact fields that are nearly done */
						fld_level =
							20 + 40 * f_ptr->counter / t_ptr->count_init;
					}
					else
					{
						/* Permanent magical fields can be gotten rid of */
						fld_level = 50;
					}

					break;
				}

				case FTYPE_CORPSE:
				{
					/* Corpses have no real value */
					fld_level = f_ptr->counter / 10;

					break;
				}
			}

			/* Hack -- High level fields start out "immune" */
			if (fld_level > cur_lev) continue;

			/* Get the location */
			y = f_ptr->fy;
			x = f_ptr->fx;

			/* Nearby fields start out "immune" */
			if ((cur_dis > 0) && (distance(px, py, x, y) < cur_dis)) continue;

			/* Saving throw */
			chance = 90;

			/* Apply the saving throw */
			if (randint0(100) < chance) continue;

			/* Call completion routine */
			if (field_script_single(f_ptr, FIELD_ACT_EXIT,
								"b", LUA_VAR_NAMED(field_visible(f_ptr), "visible")))
			{
				/* Handler failed to delete field - delete it by hand. */
				delete_field_ptr(f_ptr);
			}

			/* Count it */
			num++;
		}
	}


	/* Excise dead fields (backwards!) */
	for (i = fld_max - 1; i >= 1; i--)
	{
		field_type *f_ptr = &fld_list[i];

		/* Skip real fields */
		if (f_ptr->t_idx) continue;

		/* Move last object into open hole */
		compact_fields_aux(fld_max - 1, i);

		/* Compress "fld_max" */
		fld_max--;
	}
}


/*
 * Delete all the fields when player leaves the level
 *
 * Note -- we do NOT visually reflect these (irrelevant) changes
 *
 * Hack -- we clear the "c_ptr->fld_idx" field for every grid
 * since we know we are clearing every field.  Technically,
 * we only clear those grids containing fields, and we clear
 * it once for every such field.
 */
void wipe_f_list(void)
{
	int i;

	cave_type *c_ptr;

	int y, x;

	/* Delete the existing fields */
	for (i = 1; i < fld_max; i++)
	{
		field_type *f_ptr = &fld_list[i];

		/* Skip dead fields */
		if (!f_ptr->t_idx) continue;

		/* Access location */
		y = f_ptr->fy;
		x = f_ptr->fx;

		/* Access grid */
		c_ptr = area(x, y);

		/* Hack -- see above */
		c_ptr->fld_idx = 0;

		/* Wipe the field */
		field_wipe(f_ptr);
	}

	/* Reset "fld_max" */
	fld_max = 1;

	/* Reset "fld_cnt" */
	fld_cnt = 0;
}


/*
 * Wipe fields in region
 */
void wipe_fields(int rg_idx)
{
	int i;

	/* Delete the existing fields */
	for (i = 1; i < fld_max; i++)
	{
		field_type *f_ptr = &fld_list[i];

		/* Skip dead fields */
		if (!f_ptr->t_idx) continue;

		/* Enforce region */
		if (f_ptr->region != rg_idx) continue;

		/* Hack - delete all fields on this square */
		delete_field(f_ptr->fx, f_ptr->fy);
	}

	/* Compress the fields list */
	compact_fields(0);
}


/*
 * Acquires and returns the index of a "free" field.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
s16b f_pop(void)
{
	int i;

	/* Initial allocation */
	if (fld_max < z_info->fld_max)
	{
		/* Get next space */
		i = fld_max;

		/* Expand field array */
		fld_max++;

		/* Count fields */
		fld_cnt++;

		/* Use this field */
		return (i);
	}


	/* Recycle dead fields */
	for (i = 1; i < fld_max; i++)
	{
		field_type *f_ptr;

		/* Acquire field */
		f_ptr = &fld_list[i];

		/* Skip live fields */
		if (f_ptr->t_idx) continue;

		/* Count fields */
		fld_cnt++;

		/* Use this field */
		return (i);
	}


	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msgf("Too many fields!");

	/* Oops */
	return (0);
}


/*
 * Wipe an field clean.
 */
void field_wipe(field_type *f_ptr)
{
	/* Wipe the structure */
	(void)WIPE(f_ptr, field_type);
}


/*
 * Prepare a field based on an existing one
 */
void field_copy(field_type *f_ptr, field_type *j_ptr)
{
	/* Copy the structure */
	COPY(f_ptr, j_ptr, field_type);
}


/*
 * Add a field to a list of fields.
 * The list is sorted so that the field with the highest
 * priority is on top.
 * f_ptr is the field to add.
 */
s16b field_add(field_type *f_ptr, cave_type *c_ptr)
{
	s16b new_idx;
	
	field_type *j_ptr;
	field_type *q_ptr = NULL;
	
	FLD_ITT_START (c_ptr->fld_idx, j_ptr)
	{
		/* Look for fields that can be merged */
		if ((f_ptr->info & FIELD_INFO_MERGE) && (f_ptr->t_idx == j_ptr->t_idx))
		{
			s32b counter;
		
			/* Merge the two together */
			counter = j_ptr->counter + f_ptr->counter;

			/* Bounds checking */
			if (counter > MAX_SHORT) counter = MAX_SHORT;

			/* Store in new counter */
			j_ptr->counter = (s16b)counter;

			/* Return index */
			if (q_ptr)
			{
				return (q_ptr->next_f_idx);
			}
			else
			{
				return (c_ptr->fld_idx);
			}
		}
		
		/* Sort in priority order */
		if (j_ptr->priority < f_ptr->priority) break;
	
		/* Save previous object */
		q_ptr = j_ptr;
	}
	FLD_ITT_END;

	/* Add the field to the list */

	/* Get new node in list */
	new_idx = f_pop();

	if (!new_idx) return (0);

	/* Move field to location */
	field_copy(&fld_list[new_idx], f_ptr);

	/* If a previous node exists */
	if (q_ptr)
	{
		fld_list[new_idx].next_f_idx = q_ptr->next_f_idx;
		q_ptr->next_f_idx = new_idx;
	}
	else
	{
		/* No old node - just link directly */
		fld_list[new_idx].next_f_idx = c_ptr->fld_idx;
		c_ptr->fld_idx = new_idx;
	}

	return (new_idx);
}

#ifdef UNUSED_FUNC
/*
 * Sort a list of fields so that they are in priority order.
 *
 * Since the linked list is one-way. A simple bubble sort is
 * used.  If this is too slow, perhaps hooks to the quicksort
 * routine should be made.
 *
 * This routine is so slow - it should really never be used.
 * If at all possible - use a merge sort based on the above code.
 */
static void field_sort_priority(s16b *fld_idx_ptr)
{
	s16b *i_ptr, *j_ptr;

	s16b temp;
	bool swapped = TRUE;

	/* Paranoia - exit if list is empty */
	if (!(*fld_idx_ptr)) return;

	/* Keep scanning until no more changes */
	while (swapped)
	{
		/* Initialize */
		i_ptr = fld_idx_ptr;
		j_ptr = &(fld_list[*i_ptr].next_f_idx);

		swapped = FALSE;

		/* Scan the list until the end */
		while (*j_ptr)
		{
			/* Check to see if in order */
			if (fld_list[*i_ptr].priority < fld_list[*j_ptr].priority)
			{
				/* swap the links */
				temp = *i_ptr;
				*i_ptr = *j_ptr;
				*j_ptr = temp;
				swapped = TRUE;
			}

			/* Advance the pointers */
			i_ptr = j_ptr;
			j_ptr = &(fld_list[*j_ptr].next_f_idx);
		}
	}
}
#endif /* UNUSED_FUNC */


/*
 * Prepare a field based on kind.
 */
void field_prep(field_type *f_ptr, s16b t_idx)
{
	field_thaum *t_ptr;
	int i;

	/* Clear the record */
	field_wipe(f_ptr);

	/* Save the kind index */
	f_ptr->t_idx = t_idx;

	/* Get pointer to thaum type. */
	t_ptr = &t_info[t_idx];

	/* What it looks like */
	f_ptr->f_attr = t_ptr->f_attr;
	f_ptr->f_char = t_ptr->f_char;

	f_ptr->priority = t_ptr->priority;
	f_ptr->counter = t_ptr->count_init;
	f_ptr->info = t_ptr->info;

	for (i = 0; i < 8; i++)
	{
		/* Store the value */
		f_ptr->data[i] = t_ptr->data_init[i];
	}
}


/*
 * Initialise all fields after the game has been loaded.
 */
void init_fields(void)
{
	s16b fld_idx;
	field_type *f_ptr;
	field_thaum *t_ptr;

	for (fld_idx = 0; fld_idx < fld_max; fld_idx++)
	{
		/* Point to field */
		f_ptr = &fld_list[fld_idx];

		/* No dead fields */
		if (!f_ptr->t_idx) continue;

		/* Get pointer to thaum type. */
		t_ptr = &t_info[f_ptr->t_idx];

		/* What it looks like */
		f_ptr->f_attr = t_ptr->f_attr;
		f_ptr->f_char = t_ptr->f_char;

		/* Call loading routine */
		(void) field_script_single(f_ptr, FIELD_ACT_LOAD, "");
	}
}

/*
 * See if a field of a particular type is on a square.
 * (eg. call with (c_ptr, FTYPE_TRAP) to get traps)
 */
field_type *field_is_type(const cave_type *c_ptr, byte typ)
{
	field_type *f_ptr;

	/* While the field exists */
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Is it the correct type? */
		if (t_info[f_ptr->t_idx].type == typ) return (f_ptr);
	}
	FLD_ITT_END;
	
	/* Nothing found */
	return (NULL);
}


/*
 * Return the first known field of the requested type
 * in the list.
 */
field_type *field_first_known(const cave_type *c_ptr, byte typ)
{
	field_type *f_ptr;

	/* While the field exists */
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Is it known to be the correct type? */
		if ((t_info[f_ptr->t_idx].type == typ) &&
			((f_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_VIS)) ==
			 (FIELD_INFO_MARK | FIELD_INFO_VIS))) return (f_ptr);
	}
	FLD_ITT_END;

	/* Nothing found */
	return (NULL);
}


/*
 * Set all fields of the given type to have INFO_VIS set.
 * Return TRUE if a field is "found" that was previously
 * unknown.
 */
bool field_detect_type(const cave_type *c_ptr, byte typ)
{
	field_type *f_ptr;

	bool flag = FALSE;

	/* Scan the list */
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Is it the correct type? */
		if (t_info[f_ptr->t_idx].type == typ)
		{
			/* Is it invisible? */
			if (!(f_ptr->info & FIELD_INFO_VIS))
			{
				/* Now is visible + known */
				f_ptr->info |= (FIELD_INFO_VIS | FIELD_INFO_MARK);

				/* Lookable */
				f_ptr->info &= ~(FIELD_INFO_NO_LOOK);
			}
			else
			{
				/* We know it now */
				f_ptr->info |= (FIELD_INFO_MARK);
			}

			/* We found something */
			flag = TRUE;

			/* Note + Lite the spot */
			note_spot(f_ptr->fx, f_ptr->fy);
		}
	}
	FLD_ITT_END;

	/* Return whether we found something or not */
	return (flag);
}


/*
 * Destroy all fields of a given type in the list.
 */
void field_destroy_type(cave_type *c_ptr, byte typ)
{
	field_type *f_ptr;

	/* While the field exists */
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Is it the correct type? */
		if (t_info[f_ptr->t_idx].type == typ)
		{
			/* Call completion routine */
			field_script_single(f_ptr, FIELD_ACT_EXIT,
								"b", LUA_VAR_NAMED(field_visible(f_ptr), "visible"));
		}
	}
	FLD_ITT_END;
}


/*
 * See if flags are set in a list of fields
 *
 * This is used to see of a grid blocks movement or magic
 */
u16b fields_have_flags(const cave_type *c_ptr, u16b info)
{
	field_type *f_ptr;

	u16b flags = 0;

	/* Scan the fields */
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Or the flags together */
		flags |= f_ptr->info;
	}
	FLD_ITT_END;

	return (flags & info);
}


/*
 * Place a field of a given type on a square
 */
field_type *place_field(int x, int y, s16b t_idx)
{
	field_type *f_ptr;

	s16b fld_idx;
	region_info *ri_ptr = &ri_list[cur_region];

	field_type temp_field;
	field_type *ft_ptr = &temp_field;

	/* Paranoia */
	if ((t_idx <= 0) || (t_idx >= z_info->t_max)) return (NULL);

	/* Overlay regions are easy - just store the field type for later */
	if (ri_ptr->flags & REGION_OVER)
	{
		cave_data[y][x].fld_idx = t_idx;
	
		/* Hack - we didn't actually place a real field */
		return (NULL);
	}

	/* Make the field */
	field_prep(ft_ptr, t_idx);

	/* Place it */
	fld_idx = field_add(ft_ptr, area(x, y));

	/* Paranoia */
	if (!fld_idx) return (NULL);

	/* Get new field */
	f_ptr = &fld_list[fld_idx];

	/* Connect to ground */
	f_ptr->fy = y;
	f_ptr->fx = x;

	/* Region */
	f_ptr->region = cur_region;

	return (f_ptr);
}


/*
 * Call the script for the field *f_ptr.
 *
 * This function does not do a list of fields like field_script().
 *
 * It returns FALSE if the field deleted itself, TRUE otherwise.
 */
bool field_script_single(field_type *f_ptr, int action, cptr format, ...)
{
	va_list vp;
	cptr script;

	/* Point to the field */
	field_thaum *t_ptr = &t_info[f_ptr->t_idx];
    
	/* Paranoia - Is there a function to call? */
	if (t_ptr->action[action])
	{
		bool exists = TRUE;
	
		 /* Begin the Varargs Stuff */
		va_start(vp, format);
	
		/* Get script to use */
		script = quark_str(t_ptr->action[action]);
	
		/* Call the action script */
		if (apply_field_trigger(script, f_ptr, format, vp))
		{
			/* The field wants to be deleted */
			delete_field_ptr(f_ptr);
            
			/* The field no longer exists */
			exists = FALSE;
		}
		
		/* End the Varargs Stuff */
		va_end(vp);
		
		/* Does the field exist still? */
		return (exists);
	}

	/*
	 * XXX XXX Is this logic correct?
	 * What should we do if the field doesn't have the function?
	 */

	/* Check for deletion */
	if (f_ptr->t_idx)
	{
		/* The field is still there */
		return TRUE;
	}
	else
	{
		/* The field has deleted itself */
		return FALSE;
	}
}

/*
 * This is the const version of the above function.
 * The field cannot delete itself, which simplifies things.
 */
void field_script_const(const field_type *f_ptr, int action, cptr format, ...)
{
	va_list vp;
	cptr script;

	/* Point to the field */
	field_thaum *t_ptr = &t_info[f_ptr->t_idx];
    
	/* Paranoia - Is there a function to call? */
	if (t_ptr->action[action])
	{
		 /* Begin the Varargs Stuff */
		va_start(vp, format);
	
		/* Get script to use */
		script = quark_str(t_ptr->action[action]);
	
		/* Call the action script */
		const_field_trigger(script, f_ptr, format, vp);
		
		/* End the Varargs Stuff */
		va_end(vp);
	}
}


/*
 * Call the specified action script for each field
 * in the list at the square c_ptr
 *
 * Note the code must take into account fields deleting
 * themselves.
 */
void field_script(cave_type *c_ptr, int action, cptr format, ...)
{
	field_type *f_ptr;
	field_thaum *t_ptr;
	
	cptr script;

	FLD_ITT_START (c_ptr->fld_idx, f_ptr);
	{
		/* Point to the field */
		t_ptr = &t_info[f_ptr->t_idx];
		
		/* Get script to use */
		script = quark_str(t_ptr->action[action]);

		/* Paranoia - Is there a function to call? */
		if (script)
		{
			va_list vp;
		
			/* Begin the Varargs Stuff */
			va_start(vp, format);
					
			/* Call the action script */
			if (apply_field_trigger(script, f_ptr, format, vp))
			{
				/* The field wants to be deleted */
				delete_field_ptr(f_ptr);
			}
			
			/* End the Varargs Stuff */
			va_end(vp);
		}
	}
	FLD_ITT_END;
}


/*
 * Call the "special" action script for fields
 * in the specified list which match the required
 * field type.
 */
bool field_script_special(cave_type *c_ptr, u16b ftype, cptr format, ...)
{
	field_type *f_ptr;
	field_thaum *t_ptr;

	bool deleted = FALSE;
	
	cptr script;
    
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Point to the field */
		t_ptr = &t_info[f_ptr->t_idx];
		
		/* Get script to use */
		script = quark_str(t_ptr->action[FIELD_ACT_SPECIAL]);

		/* Check for the right field + existance of a function to call */
		if ((t_ptr->type == ftype) && script)
		{
			va_list vp;
		
			/* Begin the Varargs Stuff */
			va_start(vp, format);
		
			/* Call the action script */
			if (apply_field_trigger(script, f_ptr, format, vp))
			{
				/* The field wants to be deleted */
				delete_field_ptr(f_ptr);

				deleted = TRUE;
			}
			
			/* End the Varargs Stuff */
			va_end(vp);
		}
	}
	FLD_ITT_END;

	/* Was a field deleted? */
	return (deleted);
}


/*
 * Call the required action function for the first field
 * in the specified list with that function.
 */
field_type *field_script_find(cave_type *c_ptr, int action, cptr format, ...)
{
	field_type *f_ptr;
	field_thaum *t_ptr;
	
	va_list vp;
	cptr script;
    
	FLD_ITT_START (c_ptr->fld_idx, f_ptr)
	{
		/* Point to the field */
		t_ptr = &t_info[f_ptr->t_idx];
		
		/* Get script to use */
		script = quark_str(t_ptr->action[action]);
		
		if (script)
		{
			/* Begin the Varargs Stuff */
			va_start(vp, format);
		
			/* Call the action script */
			if (apply_field_trigger(script, f_ptr, format, vp))
			{
				/* The field wants to be deleted */
				delete_field_ptr(f_ptr);
			}
		 
        	/* End the Varargs Stuff */
			va_end(vp);

			/* Done */
			return (f_ptr);
		}
	}
	FLD_ITT_END;
    
	/* Found nothing */
	return (NULL);
}


void process_fields(void)
{
	s16b fld_idx;
	field_type *f_ptr;

	for (fld_idx = 0; fld_idx < fld_max; fld_idx++)
	{
		/* Point to field */
		f_ptr = &fld_list[fld_idx];

		/* No dead fields */
		if (!f_ptr->t_idx) continue;

		/* If it is a temporary field, count down every 10 turns */
		if ((f_ptr->info & FIELD_INFO_TEMP) && !(turn % 10))
		{
			/* Decrement counter */
			f_ptr->counter--;

			/* If at bottom */
			if (!f_ptr->counter)
			{
				/* Call completion routine */
				field_script_single(f_ptr, FIELD_ACT_EXIT,
									"b", LUA_VAR_NAMED(field_visible(f_ptr), "visible"));

				/* Nothing else to do now */
				continue;
			}
		}
	}
}


/*
 * "Testing" function, used to find bugs.
 * It will test cave and field data structures.
 * (A similar function was used to detect the
 * "invisible monster" bug in the wilderness).
 */
void test_field_data_integrity(void)
{
	int i, j;
	cave_type *c_ptr;
	field_type *f_ptr, *j_ptr;
	
	bool found;

	/* Test cave data structure */
	for (i = p_ptr->min_wid; i < p_ptr->max_wid; i++)
	{
		for (j = p_ptr->min_hgt; j < p_ptr->max_hgt; j++)
		{
			/* Point to location */
			c_ptr = area(i, j);

			/* Want a field */
			FLD_ITT_START (c_ptr->fld_idx, f_ptr)
			{
				/* Dead field? */
				if (!f_ptr->t_idx)
				{
					msgf("Dead Field");
					msgf("Field %d", _this_f_idx);
				}

				if (_this_f_idx > fld_max)
				{
					msgf("Field index inconsistancy.");
				}

				if ((f_ptr->fy != j) || (f_ptr->fx != i))
				{
					msgf("Field location inconsistancy.");
					msgf("Field x, cave x,%d,%d", f_ptr->fx, i);
					msgf("Field y, cave y,%d,%d", f_ptr->fy, j);
				}
			}
			FLD_ITT_END;
		}
	}
	
	/* Test linkage to cave data structures */
	for (i = 0; i < fld_max; i++)
	{
		f_ptr = &fld_list[i];
		
		if (!f_ptr->t_idx) continue;
		
		/* Needs to be in bounds */
		if (!in_bounds2(f_ptr->fx, f_ptr->fy))
		{
			msgf("Field out of bounds: %d", i);
			continue;
		}
		
		c_ptr = area(f_ptr->fx, f_ptr->fy);
		
		found = FALSE;
		
		/* We need to be in the linked list at the location */
		FLD_ITT_START (c_ptr->fld_idx, j_ptr)
		{
			if (f_ptr == j_ptr)
			{
				found = TRUE;
				break;
			}
		}
		FLD_ITT_END;
		
		if (!found)
		{
			msgf("Field not linked correctly (%d,%d): %d", f_ptr->fx, f_ptr->fy, i);
		}
	}
}

/*
 * Helper functions for fields lua scripts.
 */

/*
 * Set the size of a corpse - and change the fields picture.
 */
void set_corpse_size(field_type *f_ptr, int size)
{
	/* Initialise the graphic */
	if ((use_graphics == GRAPHICS_ADAM_BOLT) ||
			(use_graphics == GRAPHICS_DAVID_GERVAIS))
	{
		/* Paranoia */
		if ((size > 0) && (size < 7))
		{
			/* Hack - get new tile via offset table */
			f_ptr->f_char += size;
		}
	}
}


/*
 * Traps code.
 *
 * data[0]  Trap power  (used to be always 5).
 * data[1]  Check_hit  (used to be always 125).
 *
 * data[3]  Random number used to define effect in some cases.
 */


/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
bool check_trap_hit(int power)
{
	int k, ac;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- 5% hit, 5% miss */
	if (k < 10) return (k < 5);

	/* Paranoia -- No power */
	if (power <= 0) return (FALSE);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if (randint1(power) > ((ac * 3) / 4)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


typedef struct field_trap_type field_trap_type;

struct field_trap_type
{
	/* Field type */
	u16b t_idx;

	/* Average level found on */
	byte level;
};


/* Array used to work out which trap to place */
static field_trap_type trap_num[] =
{
	{FT_TRAP_DOOR, 5},
	{FT_TRAP_PIT, 5},
	{FT_TRAP_SPIKE_PIT, 15},
	{FT_TRAP_POISON_PIT, 30},
	{FT_TRAP_CURSE, 20},
	{FT_TRAP_TELEPORT, 40},
	{FT_TRAP_ELEMENT, 10},
	{FT_TRAP_BA_ELEMENT, 50},
	{FT_TRAP_GAS, 5},
	{FT_TRAP_TRAPS, 60},
	{FT_TRAP_TEMP_STAT, 25},
	{FT_TRAP_PERM_STAT, 70},
	{FT_TRAP_LOSE_XP, 80},
	{FT_TRAP_DISENCHANT, 35},
	{FT_TRAP_DROP_ITEM, 55},
	{FT_TRAP_MUTATE, 90},
	{FT_TRAP_NEW_LIFE, 100},
	{FT_TRAP_NO_LITE, 0},
	{FT_TRAP_HUNGER, 0},
	{FT_TRAP_NO_GOLD, 25},
	{FT_TRAP_HASTE_MON, 15},
	{FT_TRAP_RAISE_MON, 40},
	{FT_TRAP_DRAIN_MAGIC, 65},
	{FT_TRAP_AGGRAVATE, 15},
	{FT_TRAP_SUMMON, 20},
	{FT_TRAP_LOSE_MEMORY, 30},
	{0, 0}
};


/*
 * Places a random trap at the given location.
 *
 * The location must be a legal, naked, floor grid.
 */
void place_trap(int x, int y)
{
	u16b t_idx;

	int tmp, total, i;

	field_trap_type *n_ptr = trap_num;

	field_type *f_ptr;

	/* Paranoia -- verify location */
	if (!in_bounds2(x, y)) return;

	/* Calculate the total possibilities */
	for (total = 0; TRUE; n_ptr++)
	{
		/* Note end */
		if (!n_ptr->t_idx) break;

		/* Ignore excessive depth */
		if (n_ptr->level > p_ptr->depth) continue;

		/* Count this possibility */
		total += MAX_DEPTH / (p_ptr->depth - n_ptr->level + 15);
	}

	/* Pick a trap */
	while (1)
	{
		/* Pick a random type */
		tmp = randint0(total);

		/* Find this type */
		for (n_ptr = trap_num, i = 0; TRUE; n_ptr++)
		{
			/* Note end - this should never happen */
			if (!n_ptr->t_idx)
			{
				/* Go back one */
				n_ptr--;

				/* exit */
				break;
			}

			/* Ignore excessive depth */
			if (n_ptr->level > p_ptr->depth) continue;

			/* Count this possibility */
			i += MAX_DEPTH / (p_ptr->depth - n_ptr->level + 15);

			/* Found the type */
			if (tmp < i) break;
		}

		t_idx = n_ptr->t_idx;

		/* Accept non-trapdoors */
		if (t_idx != FT_TRAP_DOOR) break;

		/* Hack -- no trap doors on special levels */
		if (is_special_level(p_ptr->depth)) continue;
		
		/* Probably should prevent trap doors in the wilderness */
		if (!p_ptr->depth) continue;

		/* Hack -- no trap doors on the deepest level */
		if (p_ptr->depth >= dungeon()->max_level) continue;

		break;
	}

	f_ptr = place_field(x, y, t_idx);
	
	/* Activate the trap */
	if (f_ptr)
	{
		/* Initialise it */
		(void)field_script_single(f_ptr, FIELD_ACT_INIT, "");
	}
}


/*
 * Common stuff that happens whenever the player
 * hits a trap.
 *
 * The trap is noticed, and the player is disturbed
 */
void hit_trap(field_type *f_ptr)
{
	/* Look for invisible traps and detect them. */
	if (!(f_ptr->info & FIELD_INFO_VIS))
	{
		/* Detect it. */
		f_ptr->info |= FIELD_INFO_VIS;

		/* Trap is "lookable" */
		f_ptr->info &= ~(FIELD_INFO_NO_LOOK);

		/* Message */
		msgf("You found a trap!");

		/* Notice the changes */
		notice_field(f_ptr);
	}

	/* Disturb the player */
	disturb(FALSE);
}


/* 
 * Trap interaction functions.
 * What horrible fate awaits the player after stepping
 * on this particular trap?
 */

/*
 * Nasty cursing of equipment and player
 */
void evil_trap(void)
{
	/* Curse the equipment */
	curse_equipment(p_ptr->depth, p_ptr->depth / 10);

	/* TY Curse */
	if (p_ptr->depth > randint1(100))	/* No nasty effect for low levels */
	{
		bool stop_ty = FALSE;
		int count = 0;

		do
		{
			stop_ty = activate_ty_curse(stop_ty, &count);
		}
		while (one_in_(6));
	}

	/* Blast weapon */
	else if (p_ptr->depth > randint1(500))	/* No nasty effect for low levels */
	{
		(void)curse_weapon();
	}

	/* Blast armour */
	else if (p_ptr->depth > randint1(500))	/* No nasty effect for low levels */
	{
		(void)curse_armor();
	}
}


/* Drop a random item from the players inventory */
void drop_random_item(void)
{
	int item;

	object_type *o_ptr;

	/* Get the item to drop */
	item = randint1(get_list_length(p_ptr->inventory));

	o_ptr = get_list_item(p_ptr->inventory, item);

	/* Paranoia */
	if (!o_ptr) return;

	/* Only if not cursed */
	if (!cursed_p(o_ptr))
	{
		/* Drop it */
		inven_drop(o_ptr, o_ptr->number);
	}
}


void drain_lite(void)
{
	object_type *o_ptr;

	/* Access the lite */
	o_ptr = &p_ptr->equipment[EQUIP_LITE];

	if ((o_ptr->k_idx) &&
		((o_ptr->sval == SV_LITE_LANTERN) || (o_ptr->sval == SV_LITE_TORCH)))
	{
		/* Drain all light. */
		o_ptr->timeout = 0;
	}
	
	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Notice changes */
	notice_equip();
	
	/* Darkeness */
	unlite_room(p_ptr->px, p_ptr->py);
}


void drain_food(void)
{
	/* Only effect non-starving people */
	if (p_ptr->food > PY_FOOD_WEAK)
	{
		/* You are very hungry */
		(void)set_food(PY_FOOD_WEAK);
	}
}

void drain_magic(void)
{
	object_type *o_ptr;
	
	/* Find an item */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Only work some of the time */
		if (one_in_(2)) continue;

		/* Drain charged wands/staffs */
		if (((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)) &&
			(o_ptr->pval))
		{
			/* Uncharge */
			if (o_ptr->tval == TV_WAND) o_ptr->ac += o_ptr->pval;

			o_ptr->pval = 0;

			/* Notice changes */
			notice_inven();
		}
	}
	OBJ_ITT_END;


}

/*
 * Make a locked or jammed door on a square
 */
void make_lockjam_door(int x, int y, int power, bool jam)
{
	cave_type *c_ptr;
	field_type *f_ptr;

	int old_power = 0;

	
	/* Overlays are simpler */
	if (ri_list[cur_region].flags & REGION_OVER)
	{
		/* Make a closed door on the square */
		set_feat_bold(x, y, FEAT_CLOSED);
	
		/* Make a new field */
		if (jam)
		{
			/* Add a jammed door field */
			(void) place_field(x, y, FT_JAM_DOOR);
		}
		else
		{
			/* Add a locked door field */
			(void) place_field(x, y, FT_LOCK_DOOR);
		}

		return;
	}
	
	/* Make a closed door on the square */
	cave_set_feat(x, y, FEAT_CLOSED);

	c_ptr = area(x, y);

	f_ptr = field_is_type(c_ptr, FTYPE_DOOR);

	/* look for a door field on the square */
	if (f_ptr)
	{
		/* There already is a door field here... */

		/* HACK - Look at type */
		if (!((f_ptr->t_idx == FT_JAM_DOOR) || (f_ptr->t_idx == FT_LOCK_DOOR)))
		{
			/* 
			 * Not a locked door or a jammed door.
			 *
			 * Probably a store or building... exit.
			 */

			msgf("Cannot make door! Already one there!");

			/* Exit */
			return;
		}

		/* Save old power */
		old_power = f_ptr->counter;

		/* Get rid of old field */
		delete_field_ptr(f_ptr);
	}

	/* Make a new field */
	if (jam)
	{
		/* Add a jammed door field */
		f_ptr = place_field(x, y, FT_JAM_DOOR);
	}
	else
	{
		/* Add a locked door field */
		f_ptr = place_field(x, y, FT_LOCK_DOOR);
	}

	/* It didn't work for some reason? */
	if (!f_ptr)
	{
		msgf("Cannot make door! Too many fields.");
		
		return;
	}

	power = power + old_power;

	/* 
	 * Initialise it.
	 */
	(void)field_script_single(f_ptr, FIELD_ACT_INIT, "i:", LUA_VAR(power));
}


/*
 * Can the given monster race open the locked/jammed door.
 *
 * Used to be (randint0(m_ptr->hp) > power * power)
 * Now just uses average hp (from full to wounded) so
 * we don't have to export the monster internals to lua.
 */
bool monster_can_open(monster_race *r_ptr, int power)
{
	return (randint0(r_ptr->hdice * r_ptr->hside / 4) > power * power);
}


/*
 * Take num strings in an array, and then print them on the screen.
 * 
 * The strings are centered on the term.
 */
void print_building_options(cptr strings[], int num)
{
	int i;
	
	int max = 0, len;
	
	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);
	
	/* Get size of longest string */
	for (i = 0; i < num; i++)
	{
		len = strlen(strings[i]);
		
		if (len > max) max = len;
	}

	/* Print them out */
	for (i = 0; i < num; i++)
	{
		put_fstr(40 - len / 2, 20 - num + i, CLR_YELLOW "%s", strings[i]);
	}
}

