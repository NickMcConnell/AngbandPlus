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

/*
 * Find the connection to the cave array for the field
 * fld_list[fld_idx].
 *
 * This is used so that an arbitrary field can found.
 * This routine is fairly fast if there are not too many fields
 * on a square at one time.  However - it should only be used
 * by routines in this file.
 */
static s16b *field_find(s16b fld_idx)
{
	field_type *f_ptr;

	/* pointer to a field index in a list. */
	s16b *location;

	/* Point to the field */
	f_ptr = &fld_list[fld_idx];

	location = &(area(f_ptr->fy, f_ptr->fx)->fld_idx);

	while (*location != fld_idx)
	{
		/* Paranoia: Is the list broken? */
		if (!(*location)) return (location);

		/* Get the next field in the chain */
		location = &(fld_list[*location].next_f_idx);
	}

	/* Found a pointer to our field */
	return (location);
}


/*
 * Excise a field from a stacks
 */
void excise_field_idx(int fld_idx)
{
	s16b this_f_idx, next_f_idx = 0;

	s16b prev_f_idx = 0;

	/* Dungeon */
	cave_type *c_ptr;

	/* Field */
	field_type *j_ptr = &fld_list[fld_idx];

	int y = j_ptr->fy;
	int x = j_ptr->fx;


	/* Exit if is a "dummy" object */
	if ((x == 0) && (y == 0)) return;

	/* Grid */
	c_ptr = area(y, x);

	/* Scan all fields in the grid */
	for (this_f_idx = c_ptr->fld_idx; this_f_idx; this_f_idx = next_f_idx)
	{
		field_type *f_ptr;

		/* Acquire field */
		f_ptr = &fld_list[this_f_idx];

		/* Acquire next field */
		next_f_idx = f_ptr->next_f_idx;

		/* Done */
		if (this_f_idx == fld_idx)
		{
			/* No previous */
			if (prev_f_idx == 0)
			{
				/* Remove from list */
				c_ptr->fld_idx = next_f_idx;
			}

			/* Real previous */
			else
			{
				field_type *k_ptr;

				/* Previous field */
				k_ptr = &fld_list[prev_f_idx];

				/* Remove from list */
				k_ptr->next_f_idx = next_f_idx;
			}

			/* Forget next pointer */
			f_ptr->next_f_idx = 0;

			/* Done */
			break;
		}

		/* Save prev_f_idx */
		prev_f_idx = this_f_idx;
	}
}


/*
 * Notice changes to a field
 */
void notice_field(field_type *f_ptr)
{
	int x = f_ptr->fx;
	int y = f_ptr->fy;
	
	/* Refuse "illegal" locations */
	if (in_bounds(y, x))
	{
		/* Can the player see the square? */
		if (area(y, x)->info & CAVE_VIEW)
		{		
			/* Note + Lite the spot */
			note_spot(y, x);
		}
	}
}


/*
 * Delete a dungeon field
 *
 * Handle "lists" of fields correctly.
 */
void delete_field_idx(int fld_idx)
{
	/* Field */
	field_type *j_ptr = &fld_list[fld_idx];

	/* Dungeon floor */
	int y, x;

	/* Location */
	y = j_ptr->fy;
	x = j_ptr->fx;

	/* Excise */
	excise_field_idx(fld_idx);

	/* Refuse "illegal" locations */
	if (in_bounds(y, x))
	{
		/* Note + Lite the spot */
		note_spot(y, x);
	}

#ifdef USE_SCRIPT
	field_delete_callback(j_ptr);
#endif /* USE_SCRIPT */

	/* Wipe the field */
	field_wipe(j_ptr);

	/* Count fields */
	fld_cnt--;
}


/*
 * Delete a dungeon field
 *
 * Handle "lists" of fields correctly.
 *
 * Given a pointer to a s16b that points to this fld_idx.
 */
void delete_field_ptr(s16b *fld_idx)
{
	/* Field */
	field_type *f_ptr = &fld_list[*fld_idx];

	/* Dungeon floor */
	int y, x;

	/* Location */
	y = f_ptr->fy;
	x = f_ptr->fx;
	
	/* Remove from list */
	*fld_idx = f_ptr->next_f_idx;

	/* Refuse "illegal" locations */
	if (in_bounds(y, x))
	{
		/* Note + Lite the spot */
		note_spot(y, x);
	}

#ifdef USE_SCRIPT
	field_delete_callback(f_ptr);
#endif /* USE_SCRIPT */

	/* Wipe the field */
	field_wipe(f_ptr);

	/* Count fields */
	fld_cnt--;
}



/*
 * Deletes the list of fields attached to something.
 */
void delete_field_aux(s16b *fld_idx_ptr)
{
	s16b this_f_idx, next_f_idx = 0;

	/* Scan all fields in the grid */
	for (this_f_idx = *fld_idx_ptr; this_f_idx; this_f_idx = next_f_idx)
	{
		field_type *f_ptr;

		/* Acquire field */
		f_ptr = &fld_list[this_f_idx];

		/* Acquire next field */
		next_f_idx = f_ptr->next_f_idx;

#ifdef USE_SCRIPT
		field_delete_callback(f_ptr);
#endif /* USE_SCRIPT */

		/* Wipe the field */
		field_wipe(f_ptr);

		/* Count fields */
		fld_cnt--;
	}

	/* Nothing left */
	*fld_idx_ptr = 0;
}


/*
 * Deletes all fields at given location
 */
void delete_field(int y, int x)
{
	cave_type *c_ptr;

	/* Refuse "illegal" locations */
	if (!in_bounds(y, x)) return;

	/* Grid */
	c_ptr = area(y,x);

	delete_field_aux(&(c_ptr->fld_idx));

	/* Note + Lite the spot */
	note_spot(y, x);
}


/*
 * Deletes all fields at given location
 */
void delete_field_location(cave_type *c_ptr)
{
	delete_field_aux(&c_ptr->fld_idx);
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
		c_ptr = area(y, x);

		/* Repair grid */
		if (c_ptr->fld_idx == i1)
		{
			/* Repair */
			c_ptr->fld_idx = i2;
		}
	}


	/* Structure copy */
	fld_list[i2] = fld_list[i1];

#ifdef USE_SCRIPT
	field_delete_callback(f_ptr);
#endif /* USE_SCRIPT */

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
	int px = p_ptr-> px;
	int py = p_ptr-> py;

	int y, x, num, cnt;
	int cur_lev, cur_dis, chance;
	s16b i;
	
	s16b *fld_ptr;
	field_thaum *t_ptr;


	/* Compact */
	if (size)
	{
		/* Message */
		msg_print("Compacting fields...");

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
						fld_level = 20 + 40 * f_ptr->counter / t_ptr->count_init;
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
			if ((cur_dis > 0) && (distance(py, px, y, x) < cur_dis)) continue;

			/* Saving throw */
			chance = 90;

			/* Apply the saving throw */
			if (randint0(100) < chance) continue;
			
			fld_ptr = field_find(i);

			/* Call completion routine */
			if (field_hook_single(fld_ptr, FIELD_ACT_EXIT, NULL))
			{
				/* It didn't delete itself, so we do it now */
				delete_field_ptr(fld_ptr);
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

		/* Skip dead objects */
		if (!f_ptr->t_idx) continue;

		/* Access location */
		y = f_ptr->fy;
		x = f_ptr->fx;

		/* Access grid */
		c_ptr = area(y,x);

		/* Hack -- see above */
		c_ptr->fld_idx = 0;

#ifdef USE_SCRIPT
		field_delete_callback(f_ptr);
#endif /* USE_SCRIPT */

		/* Wipe the field */
		field_wipe(f_ptr);
	}

	/* Reset "fld_max" */
	fld_max = 1;

	/* Reset "fld_cnt" */
	fld_cnt = 0;
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
	if (fld_max < max_fld_idx)
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
	if (character_dungeon) msg_print("Too many fields!");

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

#ifdef USE_SCRIPT
	j_ptr->python = field_copy_callback(f_ptr, j_ptr);
#endif /* USE_SCRIPT */
}


/*
 * Add a field to a list of fields.
 * The list is sorted so that the field with the highest
 * priority is on top.
 * f_ptr is the field to add.
 * *fld_idx2 is a pointer to the head of the list of fields.
 */
s16b field_add(field_type *f_ptr, s16b *fld_idx2)
{
	s16b fld_idx = 0;
	s16b new_idx;
	field_type *j_ptr = &fld_list[*fld_idx2];
	
	bool merge = FALSE;
	
	s32b counter;

	/* Add to a list of fields */
	while ((*fld_idx2) && (j_ptr->priority > f_ptr->priority))
	{
		/* Look for fields that can be merged */
		if ((f_ptr->info & FIELD_INFO_MERGE) && (f_ptr->t_idx == j_ptr->t_idx))
		{
			/* Set merging flag */
			merge = TRUE;
			break;
		}
		
		/* Save old field number */
		fld_idx = *fld_idx2;

		/* Get next field in the list */
		fld_idx2 = &(j_ptr->next_f_idx);
		
		/* Update the pointer */
		j_ptr = &fld_list[*fld_idx2];
	}
	
	if (merge)
	{
		/* Merge the two together */
		counter = j_ptr->counter + f_ptr->counter;
		
		/* Bounds checking */
		if (counter > MAX_SHORT) counter = MAX_SHORT;
		
		/* Store in new counter */
		j_ptr->counter = (s16b)counter;
		
		return (*fld_idx2);
	}

	/* Add the field to the list */
	else
	{
		/*
		 * fld_idx points to node before this one.
		 * *fld_idx2 points to the node after this one.
		 */

		/* The next node */
		f_ptr->next_f_idx = *fld_idx2;
		
		/* Get new node in list */
		new_idx = f_pop();

		if (!new_idx) return (0);
		
		/* Move field to location */
		field_copy(&fld_list[new_idx], f_ptr);

		/* Make node before this one, point to this one. */
		if (fld_idx)
		{
			/* If a previous node exists */
			fld_list[fld_idx].next_f_idx = new_idx;
		}
		else
		{
			/* No old node - just link directly */
			*fld_idx2 = new_idx;
		}
		
		/* Hack - save the location */
		hack_fld_ptr = fld_idx2;

		return (new_idx);
	}
}


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
void field_sort_priority(s16b *fld_idx_ptr)
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

	/* create actions */
	for (i = 0; i < FIELD_ACTION_MAX; i++)
	{
		/* copy function pointers */
		f_ptr->action[i] = t_ptr->action[i];
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
		(void) field_hook_single(field_find(fld_idx), FIELD_ACT_LOAD, NULL);
	}
}

/*
 * See if a field of a particular type is on a square.
 * (eg. call with (&c_ptr->fld_idx, FTYPE_TRAP) to get traps)
 */
s16b *field_is_type(s16b *fld_ptr, byte typ)
{
	field_type *f_ptr;

	/* While the field exists */
	while (*fld_ptr)
	{
		/* Get field */
		f_ptr = &fld_list[*fld_ptr];

		/* Is it the correct type? */
		if (t_info[f_ptr->t_idx].type == typ) break;

		/* If not, get next one. */
		fld_ptr = &f_ptr->next_f_idx;
	}

	/* Return result */
	return (fld_ptr);
}


/*
 * Return the first known field of the requested type
 * in the list.
 */
s16b *field_first_known(s16b *fld_ptr, byte typ)
{
	field_type *f_ptr;

	/* While the field exists */
	while (*fld_ptr)
	{
		/* Get field */
		f_ptr = &fld_list[*fld_ptr];

		/* Is it known to be the correct type? */
		if ((t_info[f_ptr->t_idx].type == typ) &&
		    ((f_ptr->info & (FIELD_INFO_MARK | FIELD_INFO_VIS)) ==
		    (FIELD_INFO_MARK | FIELD_INFO_VIS))) break;

		/* If not, get next one. */
		fld_ptr = &f_ptr->next_f_idx;
	}

	/* Return Result */
	return (fld_ptr);
}


/*
 * Set all fields of the given type to have INFO_VIS set.
 * Return TRUE if a field is "found" that was previously
 * unknown.
 */
bool field_detect_type(s16b fld_idx, byte typ)
{
	field_type *f_ptr;
	
	bool flag = FALSE;

	/* While the field exists */
	while (fld_idx)
	{
		/* Get field */
		f_ptr = &fld_list[fld_idx];

		/* Is it the correct type + invisible? */
		if (t_info[f_ptr->t_idx].type == typ)
		{
			if (!(f_ptr->info & FIELD_INFO_VIS))
			{
				/* Now is visible + known */
				f_ptr->info |= (FIELD_INFO_VIS | FIELD_INFO_MARK);
				
				/* Lookable */
				f_ptr->info &= ~(FIELD_INFO_NO_LOOK);
			}

			/* We found something */
			flag = TRUE;
			
			/* Note + Lite the spot */
			note_spot(f_ptr->fy, f_ptr->fx);
		}

		/* If not, get next one. */
		fld_idx = f_ptr->next_f_idx;
	}

	/* Return whether we found something or not */
	return (flag);
}


/*
 * Destroy all fields of a given type in the list.
 */
void field_destroy_type(s16b fld_idx, byte typ)
{
	field_type *f_ptr;
	s16b *fld_ptr;

	/* While the field exists */
	while (fld_idx)
	{
		/* Get field */
		f_ptr = &fld_list[fld_idx];

		/* Is it the correct type? */
		if (t_info[f_ptr->t_idx].type == typ)
		{
			fld_ptr = field_find(fld_idx);
			
			/* Call completion routine */
			if (field_hook_single(fld_ptr, FIELD_ACT_EXIT, NULL))
			{
				/* It didn't delete itself, so we do it now */
				delete_field_ptr(fld_ptr);
			}
		}
		else
		{
			/* If not, get next one. */
			fld_idx = f_ptr->next_f_idx;
		}
	}
}



/*
 * See if flags are set in a list of fields
 *
 * This is used to see of a grid blocks movement or magic
 */
u16b fields_have_flags(s16b fld_idx, u16b info)
{
	field_type *f_ptr;
	
	u16b flags = 0;

	/* While the field exists */
	while (fld_idx)
	{
		/* Get field */
		f_ptr = &fld_list[fld_idx];

		/* Or the flags together */
		flags |= f_ptr->info;

		/* Get next field. */
		fld_idx = f_ptr->next_f_idx;
	}
	
	return (flags & info);
}


/*
 * Place a field of a given type on a square
 */
s16b place_field(int y, int x, s16b t_idx)
{
	field_type *f_ptr;

	s16b fld_idx;
	
	s16b *fld_ptr;

	field_type temp_field;
	field_type *ft_ptr = &temp_field;

	/* Make the field */
	field_prep(ft_ptr, t_idx);
	
	/* Get pointer to field list */
	fld_ptr = &(area(y, x)->fld_idx);

	/* Place it */
	fld_idx = field_add(ft_ptr, fld_ptr);

	/* Paranoia */
	if (!fld_idx) return (0);

	/* Get new field */
	f_ptr = &fld_list[fld_idx];

	/* Connect to ground */
	f_ptr->fy = y;
	f_ptr->fx = x;	

	return (fld_idx);
}



/*
 * Call the action function for the field pointed to by *field_ptr.
 *
 * This function does not do a list of fields like the one below.
 *
 * It returns FALSE if the field deleted itself, TRUE otherwise.
 */
bool field_hook_single(s16b *field_ptr, int action, void *action_struct)
{
	/* Point to the field */
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Paranoia - Is there a function to call? */
	if (f_ptr->action[action])
	{
		/* Call the action function */
		f_ptr->action[action](field_ptr, action_struct);
	}
	
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
 * Call the specified action routine for each field
 * in the list specified by *field_ptr.
 *
 * Note the code must take into account fields deleting
 * themselves.
 */
void field_hook(s16b *field_ptr, int action, void *action_struct)
{
	field_type *f_ptr;
	
	while (*field_ptr)
	{
		/* Point to the field */
		f_ptr = &fld_list[*field_ptr];

		/* Paranoia - Is there a function to call? */
		if (f_ptr->action[action])
		{
			/* Call the action function */
			f_ptr->action[action](field_ptr, action_struct);
			
			/* Check for no deletion */
			if (f_ptr->t_idx)
			{
				/* Get next field in the list */
				field_ptr = &f_ptr->next_f_idx;
			}
		}
		else
		{
			/* Get next field in the list */
			field_ptr = &f_ptr->next_f_idx;
		}
	}
}


/*
 * Call the "special" action function for fields
 * in the specified list which match the required
 * field type.
 */
bool field_hook_special(s16b *field_ptr, u16b ftype, void *action_struct)
{
	field_type *f_ptr;
	field_thaum *t_ptr;
	
	bool deleted = FALSE;
	
	while (*field_ptr)
	{
		/* Point to the field */
		f_ptr = &fld_list[*field_ptr];
		t_ptr = &t_info[f_ptr->t_idx];

		/* Check for the right field + existance of a function to call */
		if ((t_ptr->type == ftype) && (f_ptr->action[FIELD_ACT_SPECIAL]))
		{
			/* Call the action function */
			f_ptr->action[FIELD_ACT_SPECIAL](field_ptr, action_struct);
			
			/* Check for no deletion */
			if (f_ptr->t_idx)
			{
				/* Get next field in the list */
				field_ptr = &f_ptr->next_f_idx;
			}
			else
			{
				deleted = TRUE;
			}
		}
		else
		{
			/* Get next field in the list */
			field_ptr = &f_ptr->next_f_idx;
		}
	}
	
	/* Was a field deleted? */
	return (deleted);
}


/*
 * Call the required action function for the first field
 * in the specified list with that function.
 */
s16b *field_hook_find(s16b *field_ptr, int action, void *action_struct)
{
	field_type *f_ptr;
	
	while (*field_ptr)
	{
		/* Point to the field */
		f_ptr = &fld_list[*field_ptr];

		/* Is there a function to call? */
		if (f_ptr->action[action])
		{
			/* Call the action function */
			f_ptr->action[action](field_ptr, action_struct);
			
			/* Done */
			break;
		}
		else
		{
			/* Get next field in the list */
			field_ptr = &f_ptr->next_f_idx;
		}
	}
	
	/* Done */
	return (field_ptr);
}



void process_fields(void)
{
	s16b fld_idx;
	field_type *f_ptr;
	
	s16b *fld_ptr;

	for (fld_idx = 0; fld_idx < fld_max; fld_idx++)
	{
		/* Point to field */
		f_ptr = &fld_list[fld_idx];

		/* No dead fields */
		if (!f_ptr->t_idx) continue;
		
		/* Get pointer to field index */
		fld_ptr = field_find(fld_idx);

		/* If it is a temporary field, count down every 10 turns */
		if ((f_ptr->info & FIELD_INFO_TEMP) && !(turn % 10))
		{
			/* Decrement counter */
			f_ptr->counter--;

			/* If at bottom */
			if (!f_ptr->counter)
			{
				/* Call completion routine */
				if (field_hook_single(fld_ptr, FIELD_ACT_EXIT, NULL))
				{
					/* It didn't delete itself - do it now */
					delete_field_ptr(fld_ptr);
				}

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
	field_type *f_ptr;

	s16b fld_idx;

	/* Test cave data structure */
	for (i = min_wid; i < max_wid; i++)
	{
		for (j = min_hgt; j < max_hgt; j++)
		{
			/* Point to location */
			c_ptr = area(j, i);

			fld_idx = c_ptr->fld_idx;

			/* Want a field */
			while (fld_idx)
			{
				f_ptr = &fld_list[fld_idx];

				/* Dead field? */
				if (!f_ptr->t_idx)
				{
					msg_print("Dead Field");
					msg_format("Field %d",fld_idx);
				}

				if (fld_idx > fld_max)
				{
					msg_print("Field index inconsistancy.");
				}

				if ((f_ptr->fy != j) || (f_ptr->fx != i))
				{
					msg_print("Field location inconsistancy.");
					msg_format("Field x, cave x,%d,%d",f_ptr->fx, i);
					msg_format("Field y, cave y,%d,%d",f_ptr->fy, j);
				}

				fld_idx = f_ptr->next_f_idx;
			}
		}
	}
}


/* Field action functions - later will be implemented in python */


/*
 * The type of the void pointer is:
 *
 * FIELD_ACT_INIT			Function dependent.  (Be careful)
 * FIELD_ACT_LOAD			NULL
 * FIELD_ACT_PLAYER_ENTER	NULL
 * FIELD_ACT_PLAYER_ON		NULL
 * FIELD_ACT_PLAYER_LEAVE	NULL
 * FIELD_ACT_MONSTER_ENTER	monster_type*	(m_ptr)
 * FIELD_ACT_MONSTER_ON		monster_type*	(m_ptr)
 * FIELD_ACT_MONSTER_LEAVE	monster_type*	(m_ptr)
 * FIELD_ACT_OBJECT_DROP	object_type*	(o_ptr)	 
 * FIELD_ACT_OBJECT_ON		object_type*	(o_ptr)	 
 * FIELD_ACT_INTERACT		int*
 * FIELD_ACT_MAGIC_TARGET	field_magic_target*
 * FIELD_ACT_LOOK			char*
 * FIELD_ACT_EXIT			NULL
 * FIELD_ACT_MONSTER_AI		Not implemented yet.
 * FIELD_ACT_SPECIAL		Function dependent.   (Be careful)
 * FIELD_ACT_INTERACT_TEST	int*
 * FIELD_ACT_MON_ENTER_TEST field_mon_test*
 */	


/* Simple function that does nothing */
void field_action_nothing(s16b *field_ptr, void *nothing)
{

	/* Action: Do nothing at all */
	return;
}


/* Simple function that deletes the field */
void field_action_delete(s16b *field_ptr, void *nothing)
{		
	/* Delete the field */
	delete_field_ptr(field_ptr);

	/* Done */
	return;
}


/*
 * The function that now controls the glyph of warding rune.
 */
void field_action_glyph_warding(s16b *field_ptr, void *input)
{
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Look at input data */
	field_mon_test *mon_enter = (field_mon_test *) input;

	monster_type *m_ptr = mon_enter->m_ptr;
	
	monster_race *r_ptr;
	
	/* Hack: No monster - just test for existance of glyph */
	if (!m_ptr)
	{
		/* Monsters cannot be generated / teleported onto glyph */
		mon_enter->do_move = FALSE;
		
		/* Done */
		return;
	}
	
	/* Get race */
	r_ptr = &r_info[m_ptr->r_idx];
	
	if (mon_enter->do_move && !(r_ptr->flags1 & RF1_NEVER_BLOW) && 
	    (randint1(BREAK_GLYPH) < r_ptr->level)) 
	{
		/* Describe observable breakage */
		if (area(f_ptr->fy, f_ptr->fx)->info & CAVE_MARK)
		{
			msg_print("The rune of protection is broken!");
		}

		/* Delete the field */
		delete_field_ptr(field_ptr);
			
		/* Allow movement */
		mon_enter->do_move = TRUE;
	}
	else
	{
		/* No move allowed */
		mon_enter->do_move = FALSE;
	}
	
	/* Done */
	return;
}


/*
 * The function that now controls the exploding rune spell.
 */
void field_action_glyph_explode(s16b *field_ptr, void *input)
{
	int px = p_ptr-> px;
	int py = p_ptr-> py;

	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Look at input data */
	field_mon_test *mon_enter = (field_mon_test *) input;

	monster_type *m_ptr = mon_enter->m_ptr;
	
	monster_race *r_ptr;
	
	bool do_move = mon_enter->do_move;
	
	/* Hack: No monster - just test for existance of glyph */
	if (!m_ptr)
	{
		/* Monsters cannot be generated / teleported onto glyph */
		mon_enter->do_move = FALSE;
				
		/* Done */
		return;
	}
	
	/* Get race */
	r_ptr = &r_info[m_ptr->r_idx];
	
	if (do_move && !(r_ptr->flags1 & RF1_NEVER_BLOW) && 
	    (randint1(BREAK_GLYPH) < r_ptr->level)) 
	{
		if ((f_ptr->fy == py) && (f_ptr->fx == px))
		{
			msg_print("The rune explodes!");
			fire_ball(GF_MANA, 0, 2 * ((p_ptr->lev / 2) + damroll(7, 7)), 2);
		}
		else
			msg_print("An explosive rune was disarmed.");
		
		/* Delete the field */
		delete_field_ptr(field_ptr);

		/* Allow movement */
		do_move = TRUE;
	}
	else
	{
		/* No move allowed */
		do_move = FALSE;
	}

	/* Save result */
	mon_enter->do_move = do_move;
	
	/* Done */
	return;
}


/* 
 * Corpses disappear after the 100 turns...
 *
 * In nightmare mode, they reappear as monsters.
 */
void field_action_corpse_decay(s16b *field_ptr, void *nothing)
{
	field_type *f_ptr = &fld_list[*field_ptr];

	field_thaum *t_ptr = &t_info[f_ptr->t_idx];
	
	/*
	 * Data[1] * 256 + Data[2] = r_idx of monster.
	 */
	
	/* Monster race */
	u16b r_idx = ((u16b) f_ptr->data[1]) * 256 + f_ptr->data[2];
	
	if (ironman_nightmare)
	{
		/* Make a monster nearby if possible */
		if (summon_named_creature(f_ptr->fy, f_ptr->fx,
				 r_idx, FALSE, FALSE, FALSE))
		{
			if (area(f_ptr->fy, f_ptr->fx)->info & CAVE_VIEW)
			{
				msg_format("The %s rises.", t_ptr->name);
			}

			/* Set the cloned flag, so no treasure is dropped */
			m_list[hack_m_idx_ii].smart |= SM_CLONED;
		}
		
		/* Paranoia */
		else if (area(f_ptr->fy, f_ptr->fx)->info & CAVE_VIEW)
		{
			/* Let player know what happened. */
			msg_format("The %s decays.", t_ptr->name);
		}
		
	}
	else
	{
		if (area(f_ptr->fy, f_ptr->fx)->info & CAVE_VIEW)
		{
			/* Let player know what happened. */
			msg_format("The %s decays.", t_ptr->name);
		}
	}

	/* Delete the field */
	delete_field_ptr(field_ptr);

	return;
}


/* 
 * Special action to raise corpses.
 */
void field_action_corpse_raise(s16b *field_ptr, void *input)
{
	field_type *f_ptr = &fld_list[*field_ptr];
	
	bool *want_pet = (bool *) input; 
	
	/*
	 * Data[1] * 256 + Data[2] = r_idx of monster.
	 */
	
	/* Monster race */
	u16b r_idx = ((u16b) f_ptr->data[1]) * 256 + f_ptr->data[2];

	/* Make a monster nearby if possible */
	if (summon_named_creature(f_ptr->fy, f_ptr->fx,
	                          r_idx, FALSE, FALSE, *want_pet))
	{
		/* Set the cloned flag, so no treasure is dropped */
		m_list[hack_m_idx_ii].smart |= SM_CLONED;
	}

	/* Delete the field */
	delete_field_ptr(field_ptr);

	return;
}


/*
 * Hack XXX XXX Convert the char of the monster to a corpse type
 *
 * There are seven sizes of corpses.
 * 0 is large, 6 is small
 */ 
static char corpse_type(char feat)
{
	switch (feat)
	{
		case 'a': return (6);
		case 'b': return (6);
		case 'c': return (5);
		case 'd': return (0);
		case 'e': return (6);
		case 'f': return (4);
		case 'g': return (1);
		case 'h': return (2);
		case 'i': return (5);
		case 'j': return (3);
		case 'k': return (4);
		case 'l': return (0);
		case 'm': return (6);
		case 'n': return (3);
		case 'o': return (3);
		case 'p': return (2);
		case 'q': return (4);
		case 'r': return (6);
		case 's': return (2);
		case 't': return (2);
		case 'u': return (3);
		case 'v': return (4);
		case 'w': return (5);
		case 'x': return (5);
		case 'y': return (4);
		case 'z': return (3);
		case 'A': return (2);
		case 'B': return (5);
		case 'C': return (5);
		case 'D': return (0);
		case 'E': return (3);
		case 'F': return (4);
		case 'G': return (3);
		case 'H': return (2);
		case 'I': return (6);
		case 'J': return (5);
		case 'K': return (3);
		case 'L': return (1);
		case 'M': return (1);
		case 'N': return (3);
		case 'O': return (1);
		case 'P': return (0);
		case 'Q': return (3);
		case 'R': return (5);
		case 'S': return (6);
		case 'T': return (1);
		case 'U': return (0);
		case 'V': return (1);
		case 'W': return (6);
		case 'X': return (1);
		case 'Y': return (1);
		case 'Z': return (5);
		case ',': return (6);
		default : return (3);
	}
}


/*
 * Initialise a corpse / skeleton after being loaded from a savefile.
 */
void field_action_corpse_load(s16b *field_ptr, void *nothing)
{
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Monster race */
	u16b r_idx = ((u16b) f_ptr->data[1]) * 256 + f_ptr->data[2];
	
	monster_race	*r_ptr = &r_info[r_idx];
	
	/* Initialise the graphic */
	if (streq(ANGBAND_GRAF, "new"))
	{
		/* Hack - get new tile via offset table */
		f_ptr->f_char += corpse_type(r_ptr->d_char);
	}
}


/*
 * Initialise corpse / skeletons
 */
void field_action_corpse_init(s16b *field_ptr, void *input)
{
	field_type *f_ptr = &fld_list[*field_ptr];

	monster_type *m_ptr = (monster_type *) input;
	
	monster_race	*r_ptr = &r_info[m_ptr->r_idx];
	
	/*
	 * Data[1] * 256 + Data[2] = r_idx of monster.
	 */
	
	/* Store the r_idx in the data fields so that the corpse can be raised */
	f_ptr->data[1] = m_ptr->r_idx / 256;
	f_ptr->data[2] = m_ptr->r_idx % 256;
	
	/* Initialise the graphic */
	if (streq(ANGBAND_GRAF, "new"))
	{
		/* Hack - get new tile via offset table */
		f_ptr->f_char += corpse_type(r_ptr->d_char);
	}
	
	/* Notice the changes */
	notice_field(f_ptr);
	
	return;
}


/*
 * Looking at a corpse tells you what type of monster it was
 */
void field_action_corpse_look(s16b *field_ptr, void *output)
{
	field_type *f_ptr = &fld_list[*field_ptr];

	char *name = (char *)output;
	
	/* Monster race */
	u16b r_idx = ((u16b)f_ptr->data[1]) * 256 + f_ptr->data[2];
	
	monster_race *r_ptr = &r_info[r_idx];
	
	/* Copy name to the output string. */
	(void)strnfmt(name, 40, "%s %s", (r_name + r_ptr->name),
	              t_info[f_ptr->t_idx].name);

	/* Done */
	return;
}


/*
 * Try to tunnel into a wall.
 */
void field_action_wall_tunnel(s16b *field_ptr, void *input)
{	
	int *dig = (int *) input;
	
	if (*dig > 40 + randint0(1600))
	{
		/* Success */
		
		/* Delete the field */
		delete_field_ptr(field_ptr);
		
		msg_print("You have finished the tunnel.");
	}
	else
	{
		/* Failure */
		
		msg_print("You tunnel into it.");
	}
}


/*
 * Invisible walls interact with GF_KILL_WALL
 */
void field_action_wall_gf(s16b *field_ptr, void *input)
{	
	field_magic_target *f_m_t = (field_magic_target*) input;
	
	if (f_m_t->typ == GF_KILL_WALL)
	{
		/* Check line of sight */
		if (f_m_t->known)
		{
			f_m_t->notice = TRUE;
		}
		
		/* Delete the field */
		delete_field_ptr(field_ptr);
	}
}


/*
 * The various types of interaction used by
 * the "interact with grid" command.
 */
void field_action_interact_tunnel(s16b *field_ptr, void *output)
{
	int *action = (int *)output;
	
	/* Tunnel flag */
	*action = 0;

	return;
}


void field_action_interact_disarm(s16b *field_ptr, void *output)
{
	int *action = (int *)output;
	
	/* Disarm flag */
	*action = 1;

	return;
}


void field_action_interact_open(s16b *field_ptr, void *output)
{
	int *action = (int *)output;
	
	/* Open flag */
	*action = 2;

	return;
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
static bool check_hit(int power)
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


/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against players saving throw.
 */
static bool check_save(int power)
{
	int k;

	/* Percentile dice */
	k = randint0(100);

	/* Hack -- 5% hit, 5% miss */
	if (k < 10) return (k < 5);

	/* Paranoia -- No power */
	if (power <= 0) return (FALSE);

	/* Power competes against saving throw */
	if (randint1(power) > (p_ptr->skill_sav)) return (TRUE);

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
	{FT_TRAP_DOOR,			5},
	{FT_TRAP_PIT,			5},	
	{FT_TRAP_SPIKE_PIT,		15},
	{FT_TRAP_POISON_PIT,	30},
	{FT_TRAP_CURSE,			20},
	{FT_TRAP_TELEPORT,		40},
	{FT_TRAP_ELEMENT,		10},
	{FT_TRAP_BA_ELEMENT,	50},
	{FT_TRAP_GAS,			5},
	{FT_TRAP_TRAPS,			60},
	{FT_TRAP_TEMP_STAT,		25},
	{FT_TRAP_PERM_STAT,		70},
	{FT_TRAP_LOSE_XP,		80},
	{FT_TRAP_DISENCHANT,	35}, 
	{FT_TRAP_DROP_ITEM,		55},
	{FT_TRAP_MUTATE,		90},
	{FT_TRAP_NEW_LIFE,		100},
	{FT_TRAP_NO_LITE,		0},
	{FT_TRAP_HUNGER,		0},
	{FT_TRAP_NO_GOLD,		25},
	{FT_TRAP_HASTE_MON,		15},
	{FT_TRAP_RAISE_MON,		40},
	{FT_TRAP_DRAIN_MAGIC,	65},
	{FT_TRAP_AGGRAVATE,		15},
	{FT_TRAP_SUMMON,		20},
	{FT_TRAP_LOSE_MEMORY,	30},
	{0,						0}
};


/*
 * Places a random trap at the given location.
 *
 * The location must be a legal, naked, floor grid.
 */
void place_trap(int y, int x)
{	
	u16b t_idx;

	int tmp, total, i;

	field_trap_type *n_ptr = trap_num;

	/* Paranoia -- verify location */
	if (!in_bounds(y, x)) return;

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
		if (quest_number(p_ptr->depth)) continue;

		/* Hack -- no trap doors on the deepest level */
		if (p_ptr->depth >= MAX_DEPTH-1) continue;
		
		/* Probably should prevent trap doors in the wilderness */
		if (!p_ptr->depth) continue;

		break;
	}

	/* Activate the trap */
	if (place_field(y, x, t_idx))
	{
		/* Initialise it */
		(void)field_hook_single(hack_fld_ptr, FIELD_ACT_INIT, NULL);
	}
}


/*
 * Initialise the trap
 */
void field_action_trap_init(s16b *field_ptr, void *nothing)
{
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/*
	 * Data[3] is equal to randint0(rand)
	 */
	if (f_ptr->data[3])
	{
		/* Some traps use this field to store their sub-type. */
		f_ptr->data[3] = (byte)randint0(f_ptr->data[3]);
	}

	/* Initialize the name here? */
	
	/* Initialize the graphic here? */
	return;
}


/*
 * Try to disarm a trap.
 */
void field_action_trap_disarm(s16b *field_ptr, void *input)
{
	field_type *f_ptr = &fld_list[*field_ptr];
	
	int *disarm = (int *) input;
	
	/* Extract trap "power" */
	int	power = f_ptr->data[0];	
	
	/* Extract the difficulty */
	int j = *disarm - power;

	/* Always have a small chance of success */
	if (j < 2) j = 2;
	
	if (randint0(100) < j)
	{
		/* Success */
		
		/* Delete the field */
		delete_field_ptr(field_ptr);
	}
}


/*
 * Traps interact with magic.
 */
void field_action_trap_gf(s16b *field_ptr, void *input)
{
	field_magic_target *f_m_t = (field_magic_target*) input;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Destroy traps */
	if ((f_m_t->typ == GF_KILL_TRAP) || (f_m_t->typ == GF_KILL_DOOR))
	{
		/* Extract trap "power" */
		int	power = f_ptr->data[0];	
	
		/* Extract the difficulty */
		int j = f_m_t->dam - power;

		/* Always have a small chance of success */
		if (j < 2) j = 2;
	
		if (randint0(100) < j)
		{
			/* Success */
		
			/* Check line of sight */
			if (f_m_t->known)
			{
				f_m_t->notice = TRUE;
				
				msg_print("There is a bright flash of light!");
			}
		
			/* Delete the field */
			delete_field_ptr(field_ptr);
		}
	}
}


/*
 * Common stuff that happens whenever the player
 * hits a trap.
 *
 * The trap is noticed, and the player is disturbed
 */
static void hit_trap(field_type *f_ptr)
{
	/* Look for invisible traps and detect them.*/
	if (!(f_ptr->info & FIELD_INFO_VIS))
	{
		/* Detect it. */
		f_ptr->info |= FIELD_INFO_VIS;
		
		/* Trap is "lookable" */
		f_ptr->info &= ~(FIELD_INFO_NO_LOOK);
		
		/* Message */
		msg_print("You found a trap!");
		
		/* Notice the changes */
		notice_field(f_ptr);
	}

	/* Disturb the player */
	disturb(0, 0);
}


/* 
 * Trap interaction functions.
 * What horrible fate awaits the player after stepping
 * on this particular trap?
 */
void field_action_hit_trap_door(s16b *field_ptr, void *nothing)
{	
	int dam;
	
	cptr name;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	if (p_ptr->ffall)
	{
		msg_print("You fly over a trap door.");
	}
	else
	{
		if (!p_ptr->leaving)
		{
			msg_print("You have fallen through a trap door!");
			sound(SOUND_FALL);
			dam = damroll(4, 8);
			name = "a trap door";
			take_hit(dam, name);

			/* Still alive and autosave enabled */
			if (autosave_l && (p_ptr->chp >= 0))
				do_cmd_save_game(TRUE);

			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;
		}
	}
}


void field_action_hit_trap_pit(s16b *field_ptr, void *nothing)
{	
	int dam;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	cptr name;
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	if (p_ptr->ffall)
	{
		msg_print("You fly over a pit trap.");
	}
	else
	{
		msg_print("You have fallen into a pit!");
		dam = damroll(3, 8);
		name = "a pit trap";
		take_hit(dam, name);
	}
}


void field_action_hit_trap_spike(s16b *field_ptr, void *nothing)
{	
	int dam;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	cptr name;
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	if (p_ptr->ffall)
	{
		msg_print("You fly over a spiked pit.");
	}
	else
	{
		msg_print("You fall into a spiked pit!");

		/* Base damage */
		name = "a pit trap";
		dam = damroll(4, 8);

		/* Extra spike damage */
		if (randint0(100) < 50)
		{
			msg_print("You are impaled!");

			name = "a spiked pit";
			dam *= 2;
			(void)set_cut(p_ptr->cut + randint1(dam));
		}

		/* Take the damage */
		take_hit(dam, name);
	}
}


void field_action_hit_trap_poison_pit(s16b *field_ptr, void *nothing)
{	
	int dam;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	cptr name;
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	if (p_ptr->ffall)
	{
		msg_print("You fly over a spiked pit.");
	}
	else
	{
		msg_print("You fall into a spiked pit!");

		/* Base damage */
		dam = damroll(6, 8);

		name = "a pit trap";

		/* Extra spike damage */
		if (randint0(100) < 50)
		{
			msg_print("You are impaled on poisonous spikes!");

			name = "a spiked pit";

			dam *= 2;
			(void)set_cut(p_ptr->cut + randint1(dam));

			if (p_ptr->resist_pois || p_ptr->oppose_pois)
			{
				msg_print("The poison does not affect you!");
			}
			else
			{
				dam *= 2;
				(void)set_poisoned(p_ptr->poisoned + randint1(dam));
			}
		}

		/* Take the damage */
		take_hit(dam, name);
	}
}


void field_action_hit_trap_curse(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_hit(f_ptr->data[1])) return;
	
	msg_print("There is a flash of shimmering light!");
	
	/* Curse the equipment */
	curse_equipment(p_ptr->depth, p_ptr->depth / 10);	
	
	/* TY Curse */
	if (p_ptr->depth > randint1(100)) /* No nasty effect for low levels */
	{
		bool stop_ty = FALSE;
		int count = 0;

		do
		{
			stop_ty = activate_ty_curse(stop_ty, &count);
		}
		while (randint1(6) == 1);
	}
	
	/* Blast weapon */
	else if (p_ptr->depth > randint1(500)) /* No nasty effect for low levels */
	{
		(void) curse_weapon();
	}
	
	/* Blast armour */
	else if (p_ptr->depth > randint1(500)) /* No nasty effect for low levels */
	{
		(void) curse_armor();
	}
	
	/* Delete the field */
	delete_field_ptr(field_ptr);
}


void field_action_hit_trap_teleport(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_hit(f_ptr->data[1])) return;
	
	msg_print("You hit a teleport trap!");
	teleport_player(100);
}


void field_action_hit_trap_element(s16b *field_ptr, void *nothing)
{	
	int dam;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Analyze type of element */
	switch (f_ptr->data[3])
	{
		case 0:
		{
			msg_print("You are enveloped in flames!");
			dam = damroll(4, 6);
			fire_dam(dam, "a fire trap");
			break;
		}

		case 1:
		{
			msg_print("You are splashed with acid!");
			dam = damroll(4, 6);
			acid_dam(dam, "an acid trap");
			break;
		}
		
		case 2:
		{
			msg_print("A pungent green gas surrounds you!");
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + randint0(20) + 10);
			}
			break;
		}
		
		case 3:
		{
			msg_print("You are splashed with freezing liquid!");
			dam = damroll(4, 6);
			cold_dam(dam, "a cold trap");
			break;
		}
		
		case 4:
		{
			msg_print("You are hit by a spark!");
			dam = damroll(4, 6);
			elec_dam(dam, "an electric trap");
			break;
		}
	}
}


void field_action_hit_trap_ba_element(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Analyze type of element */
	switch (f_ptr->data[3])
	{
		case 0:
		{
			msg_print("You are enveloped in a ball of flames!");
			fire_ball(GF_FIRE, 0, 350, 4);
			
			fire_dam(150, "a fire trap");
			break;
		}

		case 1:
		{
			msg_print("You are soaked with acid!");
			fire_ball(GF_ACID, 0, 350, 4);
			
			acid_dam(150, "an acid trap");
			break;
		}
		
		case 2:
		{
			msg_print("A pungent grey gas surrounds you!");
			fire_ball(GF_POIS, 0, 350, 4);
			
			if (!p_ptr->resist_pois && !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + randint0(50) + 100);
			}
			break;
		}
		
		case 3:
		{
			msg_print("You are soaked with freezing liquid!");
			fire_ball(GF_ICE, 0, 350, 4);
			
			cold_dam(150, "a cold trap");
			break;
		}
		
		case 4:
		{
			msg_print("You are hit by lightning!");
			fire_ball(GF_ELEC, 0, 350, 4);
			
			elec_dam(150, "a lightning trap");
			break;
		}
	}
}


void field_action_hit_trap_gas(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Analyze type of trap */
	switch (f_ptr->data[3])
	{
		case 0:
		{
			msg_print("A blue gas surrounds you!");
			(void)set_slow(p_ptr->slow + randint0(20) + 20);
		}

		case 1:
		{
			msg_print("A black gas surrounds you!");
			if (!p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + randint0(50) + 25);
			}
			break;
		}
		
		case 2:
		{
			msg_print("A gas of scintillating colors surrounds you!");
			if (!p_ptr->resist_confu)
			{
				(void)set_confused(p_ptr->confused + randint0(20) + 10);
			}
			break;
		}
		
		case 3:
		{
			msg_print("A strange white mist surrounds you!");
			if (!p_ptr->free_act)
			{
				msg_print("You fall asleep.");

				if (ironman_nightmare)
				{
					msg_print("A horrible vision enters your mind.");

					/* Pick a nightmare */
					get_mon_num_prep(get_nightmare, NULL);

					/* Have some nightmares */
					have_nightmare(get_mon_num(MAX_DEPTH));

					/* Remove the monster restriction */
					get_mon_num_prep(NULL, NULL);
				}
				(void)set_paralyzed(p_ptr->paralyzed + randint0(10) + 5);
			}
			break;
		}
		
		case 4:
		{
			msg_print("A gas of scintillating colors surrounds you!");

			if (!p_ptr->resist_chaos)
			{
				(void)set_image(p_ptr->image + randint0(20) + 10);
			}
			break;
		}
	}
}


void field_action_hit_trap_traps(s16b *field_ptr, void *nothing)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_hit(f_ptr->data[1])) return;
	
	msg_print("There is a bright flash of light!");

	/* Make some new traps */
	project(0, 1, py, px, 0, GF_MAKE_TRAP,
	        PROJECT_HIDE | PROJECT_JUMP | PROJECT_GRID);

	/* Delete the field */
	delete_field_ptr(field_ptr);
}


void field_action_hit_trap_temp_stat(s16b *field_ptr, void *nothing)
{	
	int dam;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Analyze type of trap */
	switch (f_ptr->data[3])
	{
		case 0:
		{
			if (check_hit(f_ptr->data[1]))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, "a dart trap");
				(void)do_dec_stat(A_STR);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case 1:
		{
			if (check_hit(f_ptr->data[1]))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, "a dart trap");
				(void)do_dec_stat(A_DEX);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}
		
		case 2:
		{
			if (check_hit(f_ptr->data[1]))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, "a dart trap");
				(void)do_dec_stat(A_CON);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}
	}
}


void field_action_hit_trap_perm_stat(s16b *field_ptr, void *nothing)
{	
	int dam;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
		
	if (check_hit(f_ptr->data[1]))
	{
		msg_print("A small dart hits you!");
		dam = damroll(1, 4);
		take_hit(dam, "a dart trap");
		(void)dec_stat(f_ptr->data[3], 30, TRUE);
	}
	else
	{
		msg_print("A small dart barely misses you.");
	}
}


void field_action_hit_trap_lose_xp(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	/* Hit the trap */
	hit_trap(f_ptr);
		
	msg_print("Your head throbs!");
	lose_exp(p_ptr->exp / 5);
}


void field_action_hit_trap_disenchant(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
		
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("There is a bright flash of light!");
	(void)apply_disenchant(0);
}


void field_action_hit_trap_drop_item(s16b *field_ptr, void *nothing)
{	
	int item;
	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
		
	msg_print("You fumble with your equipment!");
	
	/* Get the item to drop */
	item = randint1(p_ptr->inven_cnt);
	
	if (inventory[item].k_idx)
	{
		/* Only if not cursed */
		if (!cursed_p(&inventory[item]))
		{
			/* Drop it */
			inven_drop(item, inventory[item].number);
		}
	}
}


void field_action_hit_trap_mutate(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
		
	(void)gain_random_mutation(0);
}


void field_action_hit_trap_new_life(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
		
	if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
	{
		msg_print("You are cured of all mutations.");
		p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
		p_ptr->update |= PU_BONUS;
		handle_stuff();
	}
	
	/* Delete the field */
	delete_field_ptr(field_ptr);
}


void field_action_hit_trap_no_lite(s16b *field_ptr, void *nothing)
{	
	int px = p_ptr->px;
	int py = p_ptr->py;

	field_type *f_ptr = &fld_list[*field_ptr];
	
	object_type *o_ptr;
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("Darkness surrounds you!");
	
	/* Access the lite */
	o_ptr = &inventory[INVEN_LITE];

	if ((o_ptr->k_idx) &&
		((o_ptr->sval == SV_LITE_LANTERN) || (o_ptr->sval == SV_LITE_TORCH)))
	{
		/* Drain all light. */
		o_ptr->pval = 0;
	}
	
	/* Darkeness */
	unlite_room(py, px);
	
	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);
}


void field_action_hit_trap_hunger(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
		
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("You suddenly feel very, very hungry!");
	
	/* Only effect non-starving people */
	if (p_ptr->food > PY_FOOD_WEAK)
	{
		/* You are very hungry */
		(void)set_food(PY_FOOD_WEAK);
	}
		
	/* Delete the field */
	delete_field_ptr(field_ptr);
}


void field_action_hit_trap_no_gold(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
		
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("Your purse becomes weightless!");
	
	/* No gold! */
	p_ptr->au = p_ptr->au / 2;

	/* Redraw gold */
	p_ptr->redraw |= (PR_GOLD);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
}


void field_action_hit_trap_haste_mon(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("A shrill note sounds!");
	
	(void)speed_monsters();	
	
	/* Delete the field */
	delete_field_ptr(field_ptr);
}


void field_action_hit_trap_raise_mon(s16b *field_ptr, void *nothing)
{	
	int px = p_ptr->px;
	int py = p_ptr->py;

	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("You smell something musty.");
	
	(void)raise_dead(py, px, FALSE);
}


void field_action_hit_trap_drain_magic(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	int i, k;
	object_type *o_ptr;
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("Static fills the air.");
	
	/* Find an item */
	for (k = 0; k < 10; k++)
	{
		/* Pick an item */
		i = randint0(INVEN_PACK);

		/* Obtain the item */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Drain charged wands/staffs */
		if (((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)) &&
			(o_ptr->pval))
		{
			/* Uncharge */
			if (o_ptr->tval == TV_WAND) o_ptr->ac += o_ptr->pval;
			
			o_ptr->pval = 0;

			/* Combine / Reorder the pack */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);
		}
	}
}


void field_action_hit_trap_aggravate(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("Shouts fill the air!");
	
	aggravate_monsters(0);
}


void field_action_hit_trap_summon(s16b *field_ptr, void *nothing)
{	
	int px = p_ptr->px;
	int py = p_ptr->py;

	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Hit the trap */
	hit_trap(f_ptr);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1])) return;
	
	msg_print("Zap!");
	
	/* Summon monsters */
	summon_specific(0, py, px, p_ptr->depth, 0, TRUE, FALSE, FALSE);
	
	/* Delete the field */
	delete_field_ptr(field_ptr);
}


void field_action_hit_trap_lose_memory(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	/* Disturb the player */
	disturb(0, 0);
	
	/* Saving throw */
	if (!check_save(f_ptr->data[1]))
	{
		/* Find the trap */
		hit_trap(f_ptr);
		
		return;
	}
	
	msg_print("You are not sure what just happened!");
	
	lose_all_info();
}


/*
 * Make a locked or jammed door on a square
 */
void make_lockjam_door(int y, int x, int power, bool jam)
{
	cave_type *c_ptr = area(y, x);
	field_type *f_ptr;
	
	s16b fld_idx = *field_is_type(&c_ptr->fld_idx, FTYPE_DOOR);
	
	int old_power = 0;
	
	/* Hack - Make a closed door on the square */
	c_ptr->feat = FEAT_CLOSED;
	
	/* look for a door field on the square */
	if (fld_idx)
	{
		/* Point to the field */
		f_ptr = &fld_list[fld_idx];
		
		/* There already is a door field here... */
		
		/* HACK - Look at type */
		if (!((f_ptr->t_idx == FT_JAM_DOOR) || (f_ptr->t_idx == FT_LOCK_DOOR)))
		{
			/* 
			 * Not a locked door or a jammed door.
			 *
			 * Probably a store or building... exit.
			 */
			
			msg_print("Cannot make door! Already one there!");
			
			/* Exit */
			return;
		}
		
		/* Save old power */
		old_power = f_ptr->counter;
		
		/* Get rid of old field */
		delete_field_idx(fld_idx);
	}
	
	/* Make a new field */
	if (jam)
	{
		/* Add a jammed door field */
		fld_idx = place_field(y, x, FT_JAM_DOOR);
	}
	else
	{	
		/* Add a locked door field */
		fld_idx = place_field(y, x, FT_LOCK_DOOR);
	}
		
	if (!fld_idx)
	{
		msg_print("Cannot make door! Too many fields.");
		return;
	}
	
	power = power + old_power;
	
	/* 
	 * Initialise it.
	 * Hack - note that hack_fld_ptr is a global that is overwritten
	 * by the place_field() function.
	 */
	(void)field_hook_single(hack_fld_ptr, FIELD_ACT_INIT, (void *) &power);
}

/*
 * Initialise a field with a counter
 */
void field_action_counter_init(s16b *field_ptr, void *input)
{
	field_type *f_ptr = &fld_list[*field_ptr];
	
	int *value = (int *) input;
	int max;
	int new_value;
	
	/* 
	 * Add the value to the counter
	 * but not if the counter will overflow.
	 * data[6] and data[7] control the counter maximum.
	 */
	max = f_ptr->data[6] * 256 + f_ptr->data[7];
	
	new_value = f_ptr->counter + *value;
	
	/* Bounds checking */
	if (new_value > max)
	{
		f_ptr->counter = max;
	}
	else if (new_value < 0)
	{
		f_ptr->counter = 0;
		
		/* Call completion routine */
		if (field_hook_single(field_ptr, FIELD_ACT_EXIT, NULL))
		{
			/* It didn't delete itself - do it now */
			delete_field_ptr(field_ptr);
		}
	}
	else
	{
		/* Store in the new value */
		f_ptr->counter = new_value;
	}
}


void field_action_door_unlock(s16b *field_ptr, void *input)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	int *lock = (int *) input;
	
	/* Extract door "power" */
	int power = *lock - f_ptr->counter;	
	
	/* Always have a small chance of success */
	if (power < 2) power = 2;
		
	if (randint0(100) < power)
	{
		/* Success */
			
		/* Message */
		msg_print("The door is unlocked.");
		
		/* Open the door */
		cave_set_feat(f_ptr->fy, f_ptr->fx, FEAT_OPEN);
		
		/* Delete the field */
		delete_field_ptr(field_ptr);
	}
	else
	{
		/* Failure */
		
		/* Message */
		msg_print("You failed to unlock the door.");
		
		/* We know the door is locked */
		f_ptr->info |= FIELD_INFO_NFT_LOOK;
		f_ptr->info &= ~(FIELD_INFO_NO_LOOK);
	}
}


void field_action_door_bash(s16b *field_ptr, void *input)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	int *jam = (int *) input;
	
	/* Extract unjamming "power" */
	int power = *jam / 10 + adj_str_wgt[p_ptr->stat_ind[A_STR]] / 2;
		
	if (randint0(power) > f_ptr->counter)
	{
		/* Success */
			
		/* Message */
		msg_print("The door crashes open!");
		
		/* Break down the door */
		if (randint0(100) < 50)
		{
			cave_set_feat(f_ptr->fy, f_ptr->fx, FEAT_BROKEN);
		}

		/* Open the door */
		else
		{
			cave_set_feat(f_ptr->fy, f_ptr->fx, FEAT_OPEN);
		}
		
		/* Delete the field */
		delete_field_ptr(field_ptr);
	}
	
	/* We know the door is jammed */
	f_ptr->info |= FIELD_INFO_NFT_LOOK;
	f_ptr->info &= ~(FIELD_INFO_NO_LOOK);
}


void field_action_door_lock_monster(s16b *field_ptr, void *input)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	field_mon_test *mon_enter = (field_mon_test *) input;
	
	monster_type *m_ptr = mon_enter->m_ptr;
	
	monster_race *r_ptr;
	
	/* Hack: No monster - cannot enter the grid */
	if (!m_ptr)
	{
		/* Monsters cannot be generated / teleported on doors */
		mon_enter->do_move = FALSE;
		
		/* Done */
		return;
	}
	
	/* Get race */
	r_ptr = &r_info[m_ptr->r_idx];
	
	if (!mon_enter->do_move)
	{
		/* Monster cannot open the door */
		
		/* Done */
		return;
	}			
	
	/* Locked doors */
	if ((r_ptr->flags2 & RF2_OPEN_DOOR) &&
			(!is_pet(m_ptr) || p_ptr->pet_open_doors))
	{
		/* Attempt to Unlock */
		if (randint0(m_ptr->hp) > f_ptr->counter * f_ptr->counter)
		{
			/* Open the door */
			cave_set_feat(f_ptr->fy, f_ptr->fx, FEAT_OPEN);
				
			/* Update view */
			if (player_can_see_bold(f_ptr->fy, f_ptr->fx))
			{
				p_ptr->update |= (PU_VIEW | PU_FLOW 
					| PU_MONSTERS | PU_MON_LITE);
			}
			
			/* Delete the field */
			delete_field_ptr(field_ptr);
		}
	}
	
	/* Cannot move */
	mon_enter->do_move = FALSE;
}


void field_action_door_jam_monster(s16b *field_ptr, void *input)
{	
	field_type *f_ptr = &fld_list[*field_ptr];
	
	field_mon_test *mon_enter = (field_mon_test *) input;
	
	monster_type *m_ptr = mon_enter->m_ptr;
	
	monster_race *r_ptr;
	
	/* Hack: No monster - cannot enter the grid */
	if (!m_ptr)
	{
		/* Monsters cannot be generated / teleported on doors */
		mon_enter->do_move = FALSE;
		
		/* Done */
		return;
	}
	
	/* Get race */
	r_ptr = &r_info[m_ptr->r_idx];
	
	if (!mon_enter->do_move)
	{
		/* Monster cannot open the door */
		
		/* Done */
		return;
	}
			

	/* Stuck Door */
	if ((r_ptr->flags2 & RF2_BASH_DOOR) &&
	    (!is_pet(m_ptr) || p_ptr->pet_open_doors))
	{
		/* Attempt to Bash */
		if (randint0(m_ptr->hp) > f_ptr->counter * f_ptr->counter)
		{
			/* Message */
			msg_print("You hear a door burst open!");

			/* Disturb (sometimes) */
			if (disturb_minor) disturb(0, 0);

			/* Break down the door */
			if (randint0(100) < 50)
			{
				cave_set_feat(f_ptr->fy, f_ptr->fx, FEAT_BROKEN);
			}

			/* Open the door */
			else
			{
				cave_set_feat(f_ptr->fy, f_ptr->fx, FEAT_OPEN);
			}
				
			/* Update view */
			if (player_can_see_bold(f_ptr->fy, f_ptr->fx))
			{
				p_ptr->update |= (PU_VIEW | PU_FLOW |
				                  PU_MONSTERS | PU_MON_LITE);
			}
			
			/* Delete the field */
			delete_field_ptr(field_ptr);

			/* Hack -- fall into doorway */
			mon_enter->do_move = TRUE;
				
			return;
		}
	}
	
	/* Cannot move */
	mon_enter->do_move = FALSE;
}

/*
 * Doors interact with various magic effects
 */
void field_action_door_gf(s16b *field_ptr, void *input)
{	
	field_type *f_ptr = &fld_list[*field_ptr];

	field_magic_target *f_m_t = (field_magic_target*) input;
	
	cave_type *c_ptr;
	
	if (f_m_t->typ == GF_KILL_WALL)
	{
		/* Destroy the door */
		if (f_m_t->known)
		{
			msg_print("The door turns into mud!");
			f_m_t->notice = TRUE;
		}

		c_ptr = area(f_ptr->fy, f_ptr->fx);
		
		/* Forget the door */
		c_ptr->info &= ~(CAVE_MARK);
		
		/* Destroy the feature */
		c_ptr->feat = FEAT_FLOOR;
		
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

		/* Delete the field */
		delete_field_ptr(field_ptr);
	}
	else if (f_m_t->typ == GF_KILL_DOOR)
	{
		/* Destroy the door */
		if (f_m_t->known)
		{
			msg_print("There is a bright flash of light!");
			f_m_t->notice = TRUE;
		}

		c_ptr = area(f_ptr->fy, f_ptr->fx);
		
		/* Forget the door */
		c_ptr->info &= ~(CAVE_MARK);
		
		/* Destroy the feature */
		c_ptr->feat = FEAT_FLOOR;
		
		/* Update some things */
		p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS | PU_MON_LITE);

		/* Delete the field */
		delete_field_ptr(field_ptr);
	}
	else if (f_m_t->typ == GF_KILL_TRAP)
	{
		/* Unlock the door */
		if (f_m_t->known)
		{
			msg_print("Click!");
			f_m_t->notice = TRUE;
		}
		
		/* Delete the field */
		delete_field_ptr(field_ptr);
	}
}

/*
 * Interact with a store
 */
void field_action_door_store(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];

	/* Disturb */
	disturb(0, 0);

	/*
	 * data[0] contains the type of store.
	 */
	do_cmd_store(f_ptr);
}


/*
 * Interact with a building
 */
void field_action_door_build(s16b *field_ptr, void *nothing)
{	
	field_type *f_ptr = &fld_list[*field_ptr];

	/* Disturb */
	disturb(0, 0);

	/*
	 * data[0] contains the type of building.
	 */
	do_cmd_bldg(f_ptr);
}
