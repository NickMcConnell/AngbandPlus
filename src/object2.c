/* File: object2.c */

/*
 * Manage object stacks and monster inventories.  Delete and compact
 * objects.  Get a new random or themed random object.  Make an object
 * known, aware, or tried.  Determine the value of an object.  Handle
 * wand and staff charges.  Determine if two objects can combine, combine
 * them.  Initialize an object.  Make ego-items, special and normal
 * artifacts.  Add magic to objects, apply random qualities.  Determine
 * if an object is "good".  Make objects and gold.  Breakage chance.
 * Drop and place objects, give them to the floor.  Manage inventory.
 * Carry, take off, and drop objects.  Reorder the pack and the quiver.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"

/*
 * Excise a dungeon object from any stacks
 */
void excise_object_idx(int o_idx)
{
	object_type *j_ptr;

	s16b this_o_idx, next_o_idx = 0;

	s16b prev_o_idx = 0;


	/* Object */
	j_ptr = &o_list[o_idx];

	/* Monster */
	if (j_ptr->held_m_idx > 0)
	{
		monster_type *m_ptr;

		/* Monster */
		m_ptr = &m_list[j_ptr->held_m_idx];

		/* Scan all objects being carried by this monster */
		for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Get object */
			o_ptr = &o_list[this_o_idx];

			/* Get next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Done */
			if (this_o_idx == o_idx)
			{
				/* No previous */
				if (prev_o_idx == 0)
				{
					/* Remove from list */
					m_ptr->hold_o_idx = next_o_idx;
				}

				/* Real previous */
				else
				{
					object_type *i_ptr;

					/* Previous object */
					i_ptr = &o_list[prev_o_idx];

					/* Remove from list */
					i_ptr->next_o_idx = next_o_idx;
				}

				/* Forget next pointer */
				o_ptr->next_o_idx = 0;

				/* Done */
				break;
			}

			/* Save prev_o_idx */
			prev_o_idx = this_o_idx;
		}
	}

	/* Trap */
	else if (j_ptr->held_m_idx < 0)
	{
		trap_type *t_ptr;

		/* Trap */
		t_ptr = &t_list[ABS(j_ptr->held_m_idx)];

		/* Scan all objects being carried by this trap */
		for (this_o_idx = t_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Get object */
			o_ptr = &o_list[this_o_idx];

			/* Get next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Done */
			if (this_o_idx == o_idx)
			{
				/* No previous */
				if (prev_o_idx == 0)
				{
					/* Remove from list */
					t_ptr->hold_o_idx = next_o_idx;
				}

				/* Real previous */
				else
				{
					object_type *i_ptr;

					/* Previous object */
					i_ptr = &o_list[prev_o_idx];

					/* Remove from list */
					i_ptr->next_o_idx = next_o_idx;
				}

				/* Forget next pointer */
				o_ptr->next_o_idx = 0;

				/* Done */
				break;
			}

			/* Save prev_o_idx */
			prev_o_idx = this_o_idx;
		}
	}

	/* Dungeon */
	else
	{
		int y = j_ptr->iy;
		int x = j_ptr->ix;

		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Get object */
			o_ptr = &o_list[this_o_idx];

			/* Get next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Done */
			if (this_o_idx == o_idx)
			{
				/* No previous */
				if (prev_o_idx == 0)
				{
					/* Remove from list */
					cave_o_idx[y][x] = next_o_idx;
				}

				/* Real previous */
				else
				{
					object_type *i_ptr;

					/* Previous object */
					i_ptr = &o_list[prev_o_idx];

					/* Remove from list */
					i_ptr->next_o_idx = next_o_idx;
				}

				/* Forget next pointer */
				o_ptr->next_o_idx = 0;

				/* Done */
				break;
			}

			/* Save prev_o_idx */
			prev_o_idx = this_o_idx;
		}
	}
}


/*
 * Delete a dungeon object
 */
void delete_object_idx(int o_idx)
{
	object_type *j_ptr;

	/* Excise */
	excise_object_idx(o_idx);

	/* Object */
	j_ptr = &o_list[o_idx];

	/* Dungeon floor */
	if (!(j_ptr->held_m_idx))
	{
		int y, x;

		/* Location */
		y = j_ptr->iy;
		x = j_ptr->ix;

		/* Visual update */
		lite_spot(y, x);

		/* Update nearby objects list */
		p_ptr->window |= (PW_O_LIST);
	}

	/* Wipe the object */
	object_wipe(j_ptr);

	/* Count objects */
	o_cnt--;
}

/*
 * Deletes all objects at given location
 */
void delete_object(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Point to this object */
		object_type *o_ptr = &o_list[this_o_idx];

		/* Get next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Wipe the object */
		object_wipe(o_ptr);

		/* Count objects */
		o_cnt--;
	}

	/* Objects are gone */
	cave_o_idx[y][x] = 0;

	/* Visual update */
	lite_spot(y, x);
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_objects_aux(int i1, int i2)
{
	int i;

	object_type *o_ptr;


	/* Do nothing */
	if (i1 == i2) return;


	/* Repair objects */
	for (i = 1; i < o_max; i++)
	{
		/* Get object */
		o_ptr = &o_list[i];

		/* Skip "dead" objects */
		if (!o_ptr->k_idx) continue;

		/* Repair "next" pointers */
		if (o_ptr->next_o_idx == i1)
		{
			/* Repair */
			o_ptr->next_o_idx = i2;
		}
	}

	/* Get object */
	o_ptr = &o_list[i1];


	/* Monster */
	if (o_ptr->held_m_idx > 0)
	{
		monster_type *m_ptr;

		/* Get monster */
		m_ptr = &m_list[o_ptr->held_m_idx];

		/* Repair monster */
		if (m_ptr->hold_o_idx == i1)
		{
			/* Repair */
			m_ptr->hold_o_idx = i2;
		}
	}

	/* Trap */
	else if (o_ptr->held_m_idx < 0)
	{
		trap_type *t_ptr;

		/* Get trap */
		t_ptr = &t_list[ABS(o_ptr->held_m_idx)];

		/* Repair trap */
		if (t_ptr->hold_o_idx == i1)
		{
			/* Repair */
			t_ptr->hold_o_idx = i2;
		}
	}

	/* Dungeon */
	else
	{
		int y, x;

		/* Get location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Repair grid */
		if (cave_o_idx[y][x] == i1)
		{
			/* Repair */
			cave_o_idx[y][x] = i2;
		}
	}


	/* Hack -- move object */
	COPY(&o_list[i2], &o_list[i1], object_type);

	/* Hack -- wipe hole */
	object_wipe(o_ptr);
}


/*
 * Compact and Reorder the object list
 *
 * This function can be very dangerous; use with caution!
 *
 * When compacting objects, we first try to clear up enough space by
 * combining objects into piles, using the same sort of logic that
 * operates when objects are dropped.  If this is not sufficient, we
 * determine the real value of all objects, then delete objects, starting
 * with the least valuable, until we've cleared enough space.  This whole
 * algorithm is intended to prevent the player suffering too much from a
 * limited object list.  -LM-
 *
 * After "compacting" (if needed), we "reorder" the objects into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_objects(int size)
{
	int i, j, y, x, num, cnt;

	object_type *o_ptr;
	object_kind *k_ptr;

	/* Paranoia -- refuse to wipe too many objects at one time */
	if (size > z_info->o_max / 2) size = z_info->o_max / 2;


	/* Compact */
	if (size)
	{
		/* Message */
		msg_print("Compacting objects...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_O_LIST);


		/* In the first round, we combine objects into piles */
		for (num = 0, i = 1; i < o_max; i++)
		{
			object_type *i_ptr = &o_list[i];

			int ty, tx;

			/* Assume that object will not combine */
			bool flag = FALSE;

			/* Note if we've deleted enough objects */
			if (num >= size) break;

			/* Skip dead objects */
			if (!i_ptr->k_idx) continue;

			/* Skip objects held by monsters or traps */
			if (i_ptr->held_m_idx) continue;

			/* Get location of object */
			y = i_ptr->iy;
			x = i_ptr->ix;

			/* Ignore objects that are not fully in bounds  XXX XXX */
			if (!in_bounds_fully(y, x)) continue;

			/* Get object kind */
			k_ptr = &k_info[i_ptr->k_idx];


			/* Scan local grids (up to radius 3) */
			for (j = 1; j < grids_in_radius[3]; j++)
			{
				ty = y + nearby_grids_y[j];
				tx = x + nearby_grids_x[j];

				/* Skip illegal grids */
				if (!in_bounds_fully(ty, tx)) continue;

				/* Require line of sight */
				if (!los(y, x, ty, tx)) continue;

				/* Scan objects in that grid */
				for (o_ptr = get_first_object(ty, tx); o_ptr;
					  o_ptr = get_next_object(o_ptr))
				{
					/* Check gold */
					if ((i_ptr->tval == TV_GOLD) && (o_ptr->tval == TV_GOLD) &&
					    (o_ptr->pval + i_ptr->pval < 10000))
					{
						/* Automatically combine gold */
						o_ptr->pval += i_ptr->pval;

						/* Save the higher quality treasure */
						if (o_ptr->sval < i_ptr->sval)
						    o_ptr->sval = i_ptr->sval;

						/* Done */
						flag = TRUE;

						break;
					}

					/* Check for possible combination */
					if (object_similar(o_ptr, i_ptr))
					{
						/* Combine i_ptr into o_ptr */
						object_absorb(o_ptr, i_ptr);

						/* Done */
						flag = TRUE;

						break;
					}
				}

				/* All done */
				if (flag) break;
			}

			/* We've combined this object */
			if (flag)
			{
				/* Delete the object */
				delete_object_idx(i);

				/* Count it */
				num++;
			}
		}

		/* In the second round, we kill objects, starting with the least valuable */
		if (num < size)
		{
			s32b *obj_value;
			s16b *obj_index;

			/* Allocate the "obj_value and obj_index" arrays */
			C_MAKE(obj_value, o_max, s32b);
			C_MAKE(obj_index, o_max, s16b);


			/* Scan the object list */
			for (i = 1; i < o_max; i++)
			{
				o_ptr = &o_list[i];

				/* Dead objects have no value (but are counted!) */
				if (!o_ptr->k_idx) obj_value[i] = 0L;

				/* Hack -- Get the real object value */
				else obj_value[i] = object_value_real(o_ptr) * o_ptr->number;

				/* Objects with notes are considered to be very valuable */
				if ((o_ptr->note) && (obj_value[i] < 10000))
					obj_value[i] = 10000;

				/* Save this object index */
				obj_index[i] = i;
			}

			/* Sort all the objects by value */
			for (i = 0; i < o_max - 1; i++)
			{
				for (j = 0; j < o_max - 1; j++)
				{
					int j1 = j;
					int j2 = j + 1;

					/* Bubble sort - ascending values */
					if (obj_value[j1] > obj_value[j2])
					{
						s32b tmp_value = obj_value[j1];
						u16b tmp_index = obj_index[j1];

						obj_value[j1] = obj_value[j2];
						obj_index[j1] = obj_index[j2];

						obj_value[j2] = tmp_value;
						obj_index[j2] = tmp_index;
					}
				}
			}

			/* Delete objects until we've reached our quota */
			for (cnt = 0, i = 0; i < o_max; i++)
			{
				/* We've deleted enough objects */
				if (cnt >= size) break;

				/* Get this object, using our saved index */
				o_ptr = &o_list[obj_index[i]];

				/* "And another one bites the dust" */
				cnt++;

				/* No need to delete dead objects again */
				if (!o_ptr->k_idx) continue;

				/* Delete the object */
				delete_object_idx(obj_index[i]);
			}

			/* Free the "obj_value and obj_index" arrays */
			FREE(obj_value);
			FREE(obj_index);
		}
	}


	/* Excise dead objects (backwards!) */
	for (i = o_max - 1; i >= 1; i--)
	{
		o_ptr = &o_list[i];

		/* Skip real objects */
		if (o_ptr->k_idx) continue;

		/* Move last object into open hole */
		compact_objects_aux(o_max - 1, i);

		/* Compress "o_max" */
		o_max--;
	}
}




/*
 * Delete all the items when player leaves the level
 *
 * Note -- we do NOT visually reflect these (irrelevant) changes
 */
void wipe_o_list(void)
{
	int i;

	/* Delete the existing objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Hack -- Either preserve artifact or notice their loss */
		if (artifact_p(o_ptr))
		{
            if (object_known_p(o_ptr))  history_lose_artifact(o_ptr->artifact_index);   /* Note the loss */
			else	                    a_info[o_ptr->artifact_index].cur_num = 0;      /* Mega-Hack -- Preserve the artifact */
		}

		/* Monster */
		if (o_ptr->held_m_idx > 0)
		{
			monster_type *m_ptr;

			/* Monster */
			m_ptr = &m_list[o_ptr->held_m_idx];

			/* Hack -- see above */
			m_ptr->hold_o_idx = 0;
		}

		/* Trap */
		else if (o_ptr->held_m_idx < 0)
		{
			trap_type *t_ptr;

			/* Trap */
			t_ptr = &t_list[ABS(o_ptr->held_m_idx)];

			/* Hack -- see above */
			t_ptr->hold_o_idx = 0;
		}

		/* Dungeon */
		else
		{
			/* Get location */
			int y = o_ptr->iy;
			int x = o_ptr->ix;

			/* Hack -- see above */
			cave_o_idx[y][x] = 0;
		}

		/* Wipe the object */
		object_wipe(o_ptr);
	}

	/* Reset "o_max" */
	o_max = 1;

	/* Reset "o_cnt" */
	o_cnt = 0;
}


/*
 * Helper function for "o_pop()".
 */
static s16b o_pop_aux(void)
{
	int i;

	/* Initial allocation */
	if (o_max < z_info->o_max)
	{
		/* Get next space */
		i = o_max;

		/* Expand object array */
		o_max++;

		/* Count objects */
		o_cnt++;

		/* Use this object */
		return (i);
	}


	/* Recycle dead objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr;

		/* Get object */
		o_ptr = &o_list[i];

		/* Skip live objects */
		if (o_ptr->k_idx) continue;

		/* Count objects */
		o_cnt++;

		/* Paranoia -- wipe the object clean */
		object_wipe(o_ptr);

		/* Use this object */
		return (i);
	}

	/* Failure */
	return (0);
}


/*
 * Gets and returns the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle failure robustly.
 */
s16b o_pop(void)
{
	int i;

	/* Try to get an object */
	i = o_pop_aux();

	/* We have an object index */
	if (i) return (i);

	/* We've run out of space.  We must compact objects and try again */
	compact_objects(64);

	/* Try again */
	i = o_pop_aux();

	/* Return if successful */
	if (i) return (i);

	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msg_print("Too many objects!");

	/* Oops */
	return (0);
}


/*
 * Get the first object at a dungeon location or NULL if there isn't one.
 */
object_type* get_first_object(int y, int x)
{
	s16b o_idx = cave_o_idx[y][x];

	if (o_idx) return (&o_list[o_idx]);

	/* No object */
	return (NULL);
}


/*
 * Get the next object in a stack or NULL if there isn't one.
 */
object_type* get_next_object(const object_type *o_ptr)
{
	if (o_ptr->next_o_idx) return (&o_list[o_ptr->next_o_idx]);

	/* No more objects */
	return (NULL);
}

/*
 * Apply a "object restriction function" to the "object allocation table"
 *
 * (Sangband 1.0.0 note)
 * In Sangband, applying an object allocation restriction is a matter
 * of toggling permissions for individual allocations, which are ordered
 * not by level, but by object index.
 */
void get_obj_num_prep(void)
{
	int i;

	/* Clear restrictions */
	if (get_obj_num_hook == NULL)
	{
		/* Accept all objects */
		for (i = 0; i < z_info->k_max; i++) permit_kind_table[i] = TRUE;
	}

	/* Apply restrictions */
	else
	{
		/* Scan all objects */
		for (i = 0; i < z_info->k_max; i++)
		{
			/* Apply the restriction */
			permit_kind_table[i] = (*get_obj_num_hook)(i);
		}
	}
}

/*
 * Returns the index for the realm that is correct for the player.
 */
int get_equivalent_book(int index, byte realm)
{
    int i;
    int tval = PLAYER_BOOK_TVAL;
    int sval = k_info[index].sval;

    /* Book is already acceptable, no work to be done */
    if (k_info[index].tval == tval) return index;

    /* Find appropriate item -- match tval and sval */
    for (i = 0; i < 800; i++)
    {
        if ((k_info[i].tval == tval) && (k_info[i].sval == sval)) return i;

    }

    return index;  /* Failure :(*/
}


/*
 * Choose an object kind that seems "appropriate" to the given level.
 *
 * This function calculates level-dependant probabilities for all allowed
 * objects, sums them up, and chooses among them.  -LM-
 *
 * If no objects match the restrictions laid down, this function will
 * fail, and return zero, but this should *almost* never happen.
 *
 * This is very slow code.  XXX XXX
 */
s16b get_obj_num(int level)
{
	int i, j, z;
	int lev1, lev2, chance1, chance2;
    int fix_freq;

	object_kind *k_ptr;

	s32b value;
	bool quick = FALSE;


	/* Sometimes boost object level */
	if ((level > 0) && (one_in_(GREAT_OBJ)))
	{
		/* Boost on 20th level can (rarely) be 16 + 4 * 20 / 3, or 42 */
		int boost = Rand_normal(0, 4 + level / 3);

		/* Boost the depth */
		level += ABS(boost);
	}

	/* Restrict level */
	if (level > MAX_DEPTH) level = MAX_DEPTH;


	/* We are using the same generation level as last time */
	if ((level == old_object_level) && (required_tval == old_required_tval))
	{
		/* We are using the same generation restrictions as last time */
		if (get_obj_num_hook == old_get_obj_num_hook)
		{
			/* There is no need to rebuild the generation table */
			if (alloc_kind_total) quick = TRUE;
		}
	}


	/* We are not using quick generation */
	if (!quick)
	{
		/* Remember the generation level we are using */
		old_object_level = level;

		/* Remember the restrictions we are using */
		old_get_obj_num_hook = get_obj_num_hook;

		/* Remember the object restriction we are using */
		old_required_tval = required_tval;

		/* Clear generation total */
		alloc_kind_total = 0L;


		/* Scan all objects */
		for (i = 0; i < z_info->k_max; i++)
		{
			/* Assume that object cannot be made */
			chance_kind_table[i] = 0;

			/* Object is forbidden */
			if (!permit_kind_table[i]) continue;

			/* Clear some variables */
			lev1 = -1; lev2 = MAX_DEPTH + 1; chance1 = 0; chance2 = 0;

			/* Get the object index */
			k_ptr = &k_info[i];

			/* Scan allocation pairs */
			for (j = 0; j < 3; j++)
			{
				/* Require a non-empty allocation, check permissions */
				if ((k_ptr->chance[j]) || (k_ptr->locale[j]))
				{
					/* Get the closest allocation <= than the current depth */
					if ((k_ptr->locale[j] <= level) && (lev1 <= k_ptr->locale[j]))
					{
						lev1    = k_ptr->locale[j];
						chance1 = k_ptr->chance[j];
					}

					/* Get the closest allocation > than the current depth */
					else if ((k_ptr->locale[j] > level) && (lev2 > k_ptr->locale[j]))
					{
						lev2    = k_ptr->locale[j];
						chance2 = k_ptr->chance[j];
					}
				}
			}

			/* Simple case - object is too high-level */
			if (lev1 < 0)
			{
				continue;
			}

			/* Simple case - no allocations exceed the current depth */
			else if (lev2 == MAX_DEPTH + 1)
			{
				chance_kind_table[i] = chance1;
			}

			/* Simple case - current depth matches an allocation entry */
			else if ((lev1 == level) || (lev2 == lev1))
			{
				chance_kind_table[i] = chance1;
			}

			/* Usual case - apply the weighted average of two allocations */
			else
			{
				z = chance1 + (level - lev1) * (chance2 - chance1) / (lev2 - lev1);
				chance_kind_table[i] = (byte)z;
			}

			/* Sum up allocation chances */
			alloc_kind_total += chance_kind_table[i];
		}
	}

	/* No legal objects */
	if (alloc_kind_total <= 0L) return (0);

	/* Pick an object */
	value = rand_int(alloc_kind_total);

	/* Find the object */
	for (i = 0; i < z_info->k_max; i++)
	{
		/* Found the entry */
		if (value < chance_kind_table[i]) break;

		/* Decrement */
		value -= chance_kind_table[i];
	}

    /* Sometimes correct magic book to equivalent book of the correct realm */
	if (is_magic_book(&k_info[i]) && p_ptr->realm)
	{
        /* Ironmen get correct townbooks, everyone gets more of the correct books*/
        if (ironman_play && k_info[i].sval <= 4) fix_freq = 1;
        else                                     fix_freq = 3;  /* One in 3 gives a 50% overall chance of getting a book of the correct realm */

        /* Change the index */
        if (one_in_(fix_freq)) i = get_equivalent_book(i, p_ptr->realm);
	}


	/* Return the object index */
	return (i);
}


/*
 * Known is true when the "attributes" of an object are "known".
 * These include tohit, todam, toac, cost, and pval (charges, bonuses, etc.).
 *
 * Note that "knowing" an object gives you everything that an "awareness"
 * gives you, and much more.  In fact, the player is always "aware" of any
 * item of which he has full "knowledge".
 *
 * But having full knowledge of, say, one Wand of Wonder, does not, by
 * itself, give you knowledge, or even awareness, of other Wands of Wonder.
 * It happens that most "identify" routines (including buying from a shop)
 * will make the player "aware" of the object as well as fully "know" it.
 *
 * This routine also removes any inscriptions generated by "feelings".
 */
void object_known(object_type *o_ptr)
{
	/* Remove any special inscription */
	o_ptr->inscrip = 0;

	/* The object is not "sensed" */
	o_ptr->ident &= ~(IDENT_SENSE);

	/* Clear the "Empty" info */
	o_ptr->ident &= ~(IDENT_EMPTY);

	/* Now we know about the item */
	o_ptr->ident |= (IDENT_KNOWN);

    /* Add artifacts to history */
	if (artifact_p(o_ptr))
	  history_add_artifact(o_ptr->artifact_index, TRUE);
}


/*
 * Add the "mental" flag to an object, then apply "object_known()".
 */
void object_mental(object_type *o_ptr)
{
	/* Now we know the item completely */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Apply normal knowledge */
	object_known(o_ptr);
}


/*
 * The player is now aware of the effects of the given object.
 */
void object_aware(object_type *o_ptr)
{
	/* Fully aware of the effects */
	k_info[o_ptr->k_idx].special |= (SPECIAL_AWARE);

	/* You must have seen it */
	k_info[o_ptr->k_idx].special |= (SPECIAL_EVER_SEEN);

	/* Hack -- update available spells */
	if (o_ptr->tval == mp_ptr->spell_book) p_ptr->update |= (PU_SPELLS);
}

/*
 * Something has been "sampled"
 */
void object_tried(object_type *o_ptr)
{
	/* Mark it as tried (even if "aware") */
	k_info[o_ptr->k_idx].special |= (SPECIAL_TRIED);
}


/*
 * Use a common set of rules to value all pval-dependant qualities.
 *
 * Note the semi-exponential valuations.
 */
s32b pval_value(u32b flg, int pval0)
{
	s32b val;
	int pval = ABS(pval0);


	/* Handle pval-dependant qualities */
	switch (flg)
	{
		case TR_PVAL_STR:      val = ( 250L * pval) + ( 250L * pval * pval);  break;
		case TR_PVAL_INT:      val = ( 250L * pval) + ( 250L * pval * pval);  break;
		case TR_PVAL_WIS:      val = ( 250L * pval) + ( 250L * pval * pval);  break;
		case TR_PVAL_DEX:      val = ( 250L * pval) + ( 250L * pval * pval);  break;
		case TR_PVAL_CON:      val = ( 250L * pval) + ( 250L * pval * pval);  break;
		case TR_PVAL_CHR:      val = ( 150L * pval) + ( 150L * pval * pval);  break;

		case TR_PVAL_STEALTH:  val = ( 300L * pval) + ( 200L * pval * pval);  break;
		case TR_PVAL_AWARE:    val = ( 125L * pval) + ( 125L * pval * pval);  break;
		case TR_PVAL_INFRA:    val = (  50L * pval) + (  25L * pval * pval);  break;
		case TR_PVAL_TUNNEL:   val = ( 120L * pval) + (  60L * pval * pval);  break;
		case TR_PVAL_SPEED:    val = (2500L * pval) + (2500L * pval * pval);  break;
		case TR_PVAL_INVIS:    val = ( 400L * pval) + ( 100L * pval * pval);  break;
		case TR_PVAL_DISARM:   val = ( 250L * pval) + ( 125L * pval * pval);  break;
		case TR_PVAL_DEVICE:   val = ( 500L * pval) + ( 250L * pval * pval);  break;
		case TR_PVAL_SAVE:     val = ( 400L * pval) + ( 100L * pval * pval);  break;
		case TR_PVAL_MANA:     val = ( 400L * pval) + ( 100L * pval * pval);  break;

		/* Note that only the best pval counts for light radius */
		case TR_PVAL_LIGHT:    val = (25L * pval * pval * pval * pval);  break;

		case TR_PVAL_BLOWS:    val = (2000L * pval) + (2000L * pval * pval);  break;
		case TR_PVAL_SHOTS:    val = (2000L * pval) + (2000L * pval * pval);  break;
		case TR_PVAL_MIGHT:    val = (3500L * pval) + (3500L * pval * pval);  break;

		default:               return (0);
	}

	return ((pval0 >= 0) ? val : -(val));
}


/*
 * Get the cost bonus granted to an object by having a pval.
 */
static s32b pval_value_obj(const object_type *o_ptr, u32b flg)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	int pval = get_object_pval(o_ptr, flg);

	/* Pvals are priced in relation to normal value  XXX */
	if (k_ptr->flags_pval & (flg)) pval -= k_ptr->pval;

	return (pval_value(flg, pval));
}


/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Character is aware of the object kind - use object value */
	if (object_aware_p(o_ptr))
	{
		/* For most items, the "aware" value is simply that of the object kind. */
		s32b aware_cost = k_ptr->cost;

		/*
		 * Because weapons and ammo may have enhanced damage dice,
		 * their aware value may vary. -LM-
		 */
		switch (o_ptr->tval)
		{
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			{
				if (o_ptr->ds > k_ptr->ds)
				{
					aware_cost += (1 + o_ptr->ds - k_ptr->ds) *
								     (2 + o_ptr->ds - k_ptr->ds) * 3L;
				}
				break;
			}

			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				/* Bonus for improved damage dice */
				if (o_ptr->ds > k_ptr->ds)
				{
					aware_cost = (200L + aware_cost) * o_ptr->ds / k_ptr->ds;
				}
				if (o_ptr->dd > k_ptr->dd)
				{
					if (o_ptr->weight > k_ptr->weight)
					{
						aware_cost +=
							(300L + aware_cost / 2) * (o_ptr->dd - k_ptr->dd);
					}
					else
					{
						aware_cost = (500L + aware_cost) * o_ptr->dd / k_ptr->dd;
					}
				}

				/* Hack -- some kinds of weapons come in bunches  XXX */
				if ((!artifact_p(o_ptr)) &&
					 (k_ptr->gen_dice * k_ptr->gen_side > 1))
				{
					aware_cost -= k_ptr->cost;

					/* Divide bonus to cost by average quantity */
					aware_cost /= (k_ptr->gen_dice * (k_ptr->gen_side + 1) / 2);

					aware_cost += k_ptr->cost;
				}

				break;
			}
		}

		/* Blankets have varying costs, and it is easy to know them */
		if ((o_ptr->tval == TV_JUNK) && (o_ptr->sval == SV_BLANKET))
		{
			aware_cost = o_ptr->b_cost;
		}

		/* Artifacts may come in groups */
		if (artifact_p(o_ptr))
		{
			aware_cost /= MAX(1, a_info[o_ptr->artifact_index].max_num);
		}

		/* Use template cost, modified by enhanced dice if needed. */
		return (aware_cost);
	}


	/*** Un-aware Objects ***/

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Un-aware Food -- this value is _deliberate_ */
		case TV_FOOD: return (0L);

		/* Un-aware Potions */
		case TV_POTION: return (20L);

		/* Un-aware Scrolls */
		case TV_SCROLL: return (20L);

		/* Un-aware Staffs */
		case TV_STAFF: return (80L);

		/* Un-aware Wands */
		case TV_WAND: return (80L);

		/* Un-aware Rods */
		case TV_ROD: return (150L);

		/* Un-aware Rings */
		case TV_RING: return (60L);

		/* Un-aware Amulets */
		case TV_AMULET: return (50L);
	}

	/* Paranoia -- Oops */
	return (0L);
}


/*
 * Return the "real" price of a "known" item, not including price
 * adjustments.  -LM-
 *
 * Artifacts use the artifact price.  All other items use a stored value,
 * then adjust it for pvals and plusses.
 */
s32b object_value_real(const object_type *o_ptr)
{
	s32b value, tmp;
	u32b utmp;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Base to_h, to_d, and to_a */
	int base_h = k_ptr->to_h;
	int base_d = k_ptr->to_d;
	int base_a = k_ptr->to_a;

	u32b f1, f2, f3;


	/* Artifact use artifact cost */
	if (o_ptr->artifact_index)
	{
		/* Price is cost divided by generation quantity */
		return (a_info[o_ptr->artifact_index].cost /
		        MAX(1, a_info[o_ptr->artifact_index].max_num));
	}


	/* Base cost */
	value = o_ptr->b_cost;

	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Wearable items get adjustments for pvals */
	if ((is_wearable(o_ptr)) && (get_object_pval(o_ptr, 0L)))
	{
		/* Scan all possible pval-dependant attributes */
		for (utmp = 1L;; utmp *= 2L)
		{
			/* Light sources don't get extra value for light radius */
			if ((utmp == TR_PVAL_LIGHT) && (o_ptr->tval == TV_LITE)) continue;

			/* Otherwise, all positive values are always worth something */
			value += pval_value_obj(o_ptr, utmp);

			/* Done */
			if (utmp == 0x80000000L) break;
		}
	}


	/* Object has low value */
	if (value < 250L)
	{
		/* Cannot get a bonus until certain things are positive */
		if (is_any_weapon(o_ptr) || is_missile(o_ptr))
		{
			if (base_h < 0) base_h = 0;
			if (base_d < 0) base_d = 0;
		}
		else if (is_any_armor(o_ptr))
		{
			if (base_a < 0) base_a = 0;
		}
	}

	/* Analyze the item */
	switch (o_ptr->tval)
	{
		/* Wands/Staffs */
		case TV_WAND:
		{
			/*
			 * Pay extra for charges, depending on standard number of
			 * charges.  Handle new-style wands correctly.
			 */
			value += (value * o_ptr->pval / o_ptr->number / (k_ptr->pval * 2));

			/* Magical devices can be damaged  XXX XXX */
			if (o_ptr->ac < k_ptr->ac) value /= 2;

			/* Done */
			break;
		}

		case TV_STAFF:
		{
			/*
			 * Pay extra for charges, depending on standard number of
			 * charges.
			 */
			value += (value * o_ptr->pval / (k_ptr->pval * 2));

			/* Magical devices can be damaged  XXX XXX */
			if (o_ptr->ac < k_ptr->ac) value /= 2;

			/* Done */
			break;
		}

		/* Rings/Amulets/Armor */
		case TV_RING:
		case TV_AMULET:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Give credit for hit bonus */
			value += (o_ptr->to_h - base_h) * 150L;

			/* Give credit for damage bonus */
			value += (o_ptr->to_d - base_d) * 150L;

			/* Give credit for armor bonus */
			value += (o_ptr->to_a - base_a) * 100L;

			/* Done */
			break;
		}

		/* Bows/Weapons */
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		{
			/* Give credit for extra shots and extra might */
			value += pval_value_obj(o_ptr, TR_PVAL_SHOTS);
			value += pval_value_obj(o_ptr, TR_PVAL_MIGHT);

			/* Fall through */
		}

		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		{
			if (o_ptr->ds > k_ptr->ds)
			{
				value = (200L + value) * o_ptr->ds / k_ptr->ds;
			}
			if (o_ptr->dd > k_ptr->dd)
			{
				if (o_ptr->weight > k_ptr->weight)
				{
					value +=
						(300L + value / 2) * (o_ptr->dd - k_ptr->dd);
				}
				else
				{
					value = (500L + value) * o_ptr->dd / k_ptr->dd;
				}
			}

			/* Give credit for perfect balance. */
			if (f1 & (TR1_PERFECT_BALANCE))
			{
				value += MIN(value/2 + 500L, 6000L);
			}

			/* Give credit for extra blows */
			value += pval_value_obj(o_ptr, TR_PVAL_BLOWS);

			/* Bonuses for Skill and Deadliness depend on value */
			tmp = MIN(250L, (value + 1000L) / 25L);

			/* Give credit (or penalty) for AC bonus */
			value += (o_ptr->to_a - base_a) * 100;

			/* Give credit (or penalty) for Skill bonus */
			value += (o_ptr->to_h - base_h) * tmp;

			/* Give credit (or penalty) for Deadliness bonus */
			value += (o_ptr->to_d - base_d) * tmp;

			/* Hack -- some kinds of weapons come in bunches  XXX */
			if ((!artifact_p(o_ptr)) &&
			    (k_ptr->gen_dice * k_ptr->gen_side > 1))
			{
				value -= k_ptr->cost;

				/* Divide bonus to cost by average quantity */
				value /= (k_ptr->gen_dice * (k_ptr->gen_side + 1) / 2);

				value += k_ptr->cost;
			}

			/* Done */
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Improved damage dice add to value */
			if (o_ptr->ds > k_ptr->ds)
			{
				value += (1 + o_ptr->ds - k_ptr->ds) *
				         (2 + o_ptr->ds - k_ptr->ds) * 3L;
			}

			/* Bonus for being powerful and an ego-item */
			if (o_ptr->ego_item_index)
			{
				if ((o_ptr->ds > k_ptr->ds) || (o_ptr->sval > SV_AMMO_NORMAL))
					value *= 2;
			}

			/* Bonuses for Skill and Deadliness depend on value */
			tmp = MIN(10L, (value + 100L) / 33L);

			/* Give credit (or penalty) for Skill bonus */
			value += (o_ptr->to_h - base_h) * tmp;

			/* Give credit (or penalty) for Deadliness bonus */
			value += (o_ptr->to_d - base_d) * tmp;

			/* Done */
			break;
		}

		/* Who wants an empty chest?  */
		case TV_CHEST:
		{
			if (o_ptr->pval == 0) return (0L);
			break;
		}

		/* Blankets */
		case TV_JUNK:
		{
			if (o_ptr->sval == SV_BLANKET)
			{
				/* Cursed blankets are Worthless! */
				if (cursed_p(o_ptr)) return (0L);
			}
		}
	}

	/* No negative value */
	if (value < 0L) value = 0L;

	/* Return the value */
	return (value);
}


/*
 * Return the price of an item including plusses (and charges).
 *
 * This function returns the "value" of the given item (qty one).
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
 */
s32b object_value(const object_type *o_ptr)
{
	s32b value;


	/* Known (or easy-know aware) items -- Get the actual value */
	if (object_known_p(o_ptr))
	{
		/* Cursed wearable items are always worthless */
		if (is_wearable(o_ptr) && cursed_p(o_ptr)) return (0L);

		/* Real value (see above) */
		value = object_value_real(o_ptr);
	}

	/* Unknown items -- Get a base value */
	else
	{
		/* Some inscriptions indicate lack of value */
		if (o_ptr->inscrip == INSCRIP_TERRIBLE) return (0L);
		if (o_ptr->inscrip == INSCRIP_WORTHLESS) return (0L);
		if (o_ptr->inscrip == INSCRIP_BROKEN) return (0L);
		if (o_ptr->inscrip == INSCRIP_UNCURSED) return (0L);

		/* Hack -- Felt cursed items */
		if ((o_ptr->ident & (IDENT_SENSE)) && cursed_p(o_ptr) &&
		    (o_ptr->inscrip != INSCRIP_UNCERTAIN && o_ptr->inscrip != INSCRIP_NULL)) return (0L);

		/* Base value (see above) */
		value = object_value_base(o_ptr);
	}

	/* Apply price adjustment (if any) */
	if ((o_ptr->cost_adjust > 0) && (value > 0L))
	{
		value = (value * o_ptr->cost_adjust + 50L) / 100L;

		/* Never become free */
		if (value < 1L) value = 1L;
	}

	/* Return the final value */
	return (value);
}





/*
 * Distribute charges of rods or wands.
 *
 * o_ptr = source item
 * i_ptr = target item, must be of the same type as o_ptr
 * amt = number of items that are transferred
 */
void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt)
{
	/*
	 * Hack -- If rods or wands are dropped, the total maximum timeout or
	 * charges need to be allocated between the two stacks.   If all the items
	 * are being dropped, it makes for a neater message to leave the original
	 * stack's pval alone.  -LM-
	 */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD))
	{
		i_ptr->pval = o_ptr->pval * amt / o_ptr->number;

		if (amt < o_ptr->number) o_ptr->pval -= i_ptr->pval;

		/*
		 * Hack -- Rods also need to have their timeouts distributed.  The
		 * dropped stack will accept all time remaining to charge up to its
		 * maximum.
		 */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			if (i_ptr->pval > o_ptr->timeout) i_ptr->timeout = o_ptr->timeout;
			else                              i_ptr->timeout = i_ptr->pval;

			if (amt < o_ptr->number) o_ptr->timeout -= i_ptr->timeout;
		}
	}
}


/*
 * Hack -- If rods or wands are destroyed, the total maximum timeout or
 * charges of the stack needs to be reduced, unless all the items are
 * being destroyed.  -LM-
 */
void reduce_charges(object_type *o_ptr, int amt)
{
	if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)) &&
		(amt < o_ptr->number))
	{
		o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
	}
}


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow wands (if both are either known or confirmed
 * empty) and rods (in all cases) to combine.
 *
 * Staffs are big and bulky.  They never stack.
 *
 * If permitted, we allow weapons/armor to stack, if fully "known".
 *
 * Missiles combine if both stacks have the same "known" status, but only
 * if both stacks have been "worn".  This helps make unidentified stacks
 * of missiles useful, but prevents the character learning that two
 * collections of missiles are the same, and thus probably both average,
 * without taking the risk of placing them in his quiver.  -LM-
 * Stacks of missiles without plusses will combine if both are at least
 * known to be "average".  -RML-
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests, and activatable items, never stack (for various reasons).
 */
bool object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;


	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return (FALSE);


	/* Analyze the items */
	switch (o_ptr->tval)
	{
		/* Chests */
		case TV_CHEST:
		{
			/* Never okay */
			return (FALSE);
		}

		/* Bottles and parchments */
		case TV_BOTTLE:
		case TV_PARCHMENT:
		{
			/* No special tests */
			break;
		}

		/* Food and Potions and Scrolls */
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		{
			/* Require identical pvals */
			if (o_ptr->pval != j_ptr->pval) return (FALSE);

			/* Require identical values of certain basic attributes */
			/* if (o_ptr->ac != j_ptr->ac) return (FALSE); */
			if (o_ptr->dd != j_ptr->dd) return (FALSE);
			if (o_ptr->ds != j_ptr->ds) return (FALSE);

			/* Assume okay XXX XXX */
			break;
		}

		/* Staffs */
		case TV_STAFF:
		{
			/* Never okay */
			return (FALSE);
		}

		/* Wands */
		case TV_WAND:
		{
			/* Enforce a maximum pval  -KBB- */
			if ((long)j_ptr->pval + o_ptr->pval > (long)MAX_SHORT)
			{
				return (FALSE);
			}

			/* Wand charges can combine.  */

			/* Assume okay */
			break;
		}

		/* Rods */
		case TV_ROD:
		{
			/* Enforce a maximum pval  -KBB- */
			if ((long)j_ptr->pval + o_ptr->pval > (long)MAX_SHORT)
			{
				return (FALSE);
			}

			/* Assume okay. */
			break;
		}

		/* Rings, Amulets, Light sources */
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			/* Require full knowledge of both items */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr))
				return (FALSE);

			/* Fall through */
		}

		/* Weapons and Armor */
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Fall through */
		}

		/* Missiles */
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Normally, require knowledge of both items */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr))
			{
				/* Allow "worn" ammo to stack with identical "worn" ammo */
				if ((o_ptr->ident & (IDENT_WORN)) &&
				    (j_ptr->ident & (IDENT_WORN)) &&
				    (o_ptr->inscrip == j_ptr->inscrip))
				{
					/* No automatic failure */
				}

				/*
				 * Allow {average} pseudo-id'ed items to stack with
				 * otherwise identical known items.
				 */
				else if ((!object_known_p(o_ptr)) &&
					 (o_ptr->inscrip != INSCRIP_AVERAGE))
				{
					return (FALSE);
				}
				else if ((!object_known_p(j_ptr)) &&
					 (j_ptr->inscrip != INSCRIP_AVERAGE))
				{
					return (FALSE);
				}
			}

			/* Require identical values of certain basic attributes */
			if (o_ptr->ac != j_ptr->ac) return (FALSE);
			if (o_ptr->dd != j_ptr->dd) return (FALSE);
			if (o_ptr->ds != j_ptr->ds) return (FALSE);

			/* Require identical bonuses */
			if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
			if (o_ptr->to_d != j_ptr->to_d) return (FALSE);
			if (o_ptr->to_a != j_ptr->to_a) return (FALSE);

			/* Require identical "pvals" */
			if (o_ptr->pval  != j_ptr->pval)  return (FALSE);
			if (o_ptr->pval2 != j_ptr->pval2) return (FALSE);
			if (o_ptr->pval3 != j_ptr->pval3) return (FALSE);

			/* Require identical object pval-dependant flags */
			if (o_ptr->flags_pval1 != j_ptr->flags_pval1) return (FALSE);
			if (o_ptr->flags_pval2 != j_ptr->flags_pval2) return (FALSE);
			if (o_ptr->flags_pval3 != j_ptr->flags_pval3) return (FALSE);

			/* Require identical object flags */
			if (o_ptr->flags1 != j_ptr->flags1) return (FALSE);
			if (o_ptr->flags2 != j_ptr->flags2) return (FALSE);
			if (o_ptr->flags3 != j_ptr->flags3) return (FALSE);

			/* Require identical "artifact" indexes */
			if (o_ptr->artifact_index != j_ptr->artifact_index) return (FALSE);

			/* Require identical "ego-item" indexes */
			if (o_ptr->ego_item_index != j_ptr->ego_item_index) return (FALSE);

			/* Hack -- Never stack recharging items (other than rods) */
			if (o_ptr->timeout || j_ptr->timeout) return (FALSE);

			/* Probably okay */
			break;
		}

		/* Components */
		case TV_COMPONENT:
		{
			/* Must have identical pval */
			if (o_ptr->pval != j_ptr->pval) return (FALSE);

			/* Probably okay */
			break;
		}

		/* Junk (especially boulders) */
		case TV_JUNK:
		{
			/* Must have identical damage dice */
			if (o_ptr->dd != j_ptr->dd) return (FALSE);
			if (o_ptr->ds != j_ptr->ds) return (FALSE);

			/* Blankets never stack */
			if (o_ptr->sval == SV_BLANKET) return (FALSE);

			/* Probably okay */
			break;
		}
		case TV_MAGIC_BOOK:
		case TV_NATURE_BOOK:
		case TV_DARK_BOOK:
		case TV_PRAYER_BOOK:
		{
			if (o_ptr->ego_item_index != j_ptr->ego_item_index)
			{
				return (FALSE);
			}
		}

		/* Various */
		default:
		{
			/* Require knowledge */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr))
				return (FALSE);

			/* Probably okay */
			break;
		}
	}

	/* Must have identical weight */
	if (o_ptr->weight != j_ptr->weight) return (FALSE);

	/* Hack -- Require identical "cursed" status */
	if ((o_ptr->ident & (IDENT_CURSED)) != (j_ptr->ident & (IDENT_CURSED)))
	{
		return (FALSE);
	}

	/* Hack -- Require compatible inscriptions */
	if (o_ptr->note != j_ptr->note)
	{
		/* Normally require matching inscriptions */
		if (!stack_force_notes) return (FALSE);

		/* Never combine different inscriptions */
		if (o_ptr->note && j_ptr->note) return (FALSE);
	}

	/* Hack -- Require compatible cost_adjustments */
	if ((!stack_force_costs) && (o_ptr->cost_adjust != j_ptr->cost_adjust) && !birth_stores_only_sell)
	{
		return (FALSE);
	}

	/* Enforce a maximum stack size (currently 99) */
	if (total >= MAX_STACK_SIZE) return (FALSE);

	/* They match, so they must be similar */
	return (TRUE);
}

/*
 * Allow one item to "absorb" another, assuming they are similar.
 *
 * The blending of the "note" field assumes that either (1) one has an
 * inscription and the other does not, or (2) neither has an inscription.
 * In both these cases, we can simply use the existing note, unless the
 * blending object has a note, in which case we use that note.
 *
 * These assumptions are enforced by the "object_similar()" code.
 */
void object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	u16b ident_flags;

	/* Add together the item counts */
	o_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Combine most of the ident flags ("empty" and "known" being the exceptions) */
	ident_flags = o_ptr->ident & (IDENT_SENSE | IDENT_FIXED |
		IDENT_RUMOUR | IDENT_MENTAL | IDENT_CURSED |
		IDENT_WORN);

	ident_flags |= j_ptr->ident & (IDENT_SENSE | IDENT_FIXED |
		IDENT_RUMOUR | IDENT_MENTAL | IDENT_CURSED |
		IDENT_WORN);


	/* Add the "empty" flag only if both objects have it */
	if ((o_ptr->ident & (IDENT_EMPTY)) && (j_ptr->ident & (IDENT_EMPTY)))
	{
		o_ptr->ident |= (IDENT_EMPTY);
	}

	if (((o_ptr->ident & (IDENT_EMPTY)) && object_known_p(j_ptr)) ||
		((j_ptr->ident & (IDENT_EMPTY)) && object_known_p(o_ptr)))
	{
		ident_flags |= (IDENT_KNOWN);
	}

	/* Retain knowledge only if both items are known */
	if ((object_known_p(o_ptr)) && (object_known_p(j_ptr)))
	{
		/* Add knowledge */
		ident_flags |= (IDENT_KNOWN);
	}

	/* Apply ident flags */
	o_ptr->ident = ident_flags;


	/* Hack -- Blend "notes" */
	if      (j_ptr->note != 0) o_ptr->note = j_ptr->note;

	/* Hack -- Blend special inscriptions */
	if ((j_ptr->inscrip != 0) && (o_ptr->inscrip == 0))
		o_ptr->inscrip = j_ptr->inscrip;

	/* Mega-Hack -- Use lower cost adjustment */
	if (o_ptr->cost_adjust > j_ptr->cost_adjust)
	    o_ptr->cost_adjust = j_ptr->cost_adjust;

	/* Hack! -- average armor classes */
	if (o_ptr->ac != j_ptr->ac)
	{
		int tmp = ((o_ptr->ac * (total - j_ptr->number)) +
		           (j_ptr->ac * j_ptr->number)) / total;

		o_ptr->ac = (s16b)tmp;
	}

	/*
	 * Hack -- if rods are stacking, add the pvals (maximum timeouts) and
	 * current timeouts together.
	 */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval += j_ptr->pval;
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Hack -- if wands/staves are stacking, combine the charges */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		o_ptr->pval += j_ptr->pval;
	}
}



/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_kind(int tval, int sval)
{
	int k;

	/* Look for it */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Found a match */
		if ((k_ptr->tval == tval) && (k_ptr->sval == sval)) return (k);
	}

	/* Oops  XXX XXX XXX */
	msg_format("No object (%d,%d)", tval, sval);
	message_flush();

	/* Oops */
	return (0);
}

/*
 * Wipe an object clean.
 */
void object_wipe(object_type *o_ptr)
{
	/* Wipe the structure */
	(void)WIPE(o_ptr, object_type);
}

/*
 * Prepare the first object (o_ptr) based on the second object (i_ptr).
 */
void object_copy(object_type *o_ptr, const object_type *i_ptr)
{
	/* Copy the structure */
	COPY(o_ptr, i_ptr, object_type);
}


/*
 * Prepare an object based on an object kind.
 */
void object_prep(object_type *o_ptr, int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Clear the record */
	WIPE(o_ptr, object_type);

	/* Save the kind index */
	o_ptr->k_idx = k_idx;

	/* Efficiency -- tval/sval */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;

	/* Default "pval" */
	o_ptr->pval = k_ptr->pval;
	o_ptr->pval2 = 0;
	o_ptr->pval3 = 0;

	/* Default pval-dependent flags */
	o_ptr->flags_pval1 = k_ptr->flags_pval;
	o_ptr->flags_pval2 = 0L;
	o_ptr->flags_pval3 = 0L;

	/* Default object flags */
	o_ptr->flags1 = k_ptr->flags1;
	o_ptr->flags2 = k_ptr->flags2;
	o_ptr->flags3 = k_ptr->flags3;

	/* Default number */
	o_ptr->number = 1;

	/* Default weight */
	o_ptr->weight = k_ptr->weight;

	/* Default magic */
	o_ptr->to_h = k_ptr->to_h;
	o_ptr->to_d = k_ptr->to_d;
	o_ptr->to_a = k_ptr->to_a;

	/* Default power */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* Default activation */
	o_ptr->activate = k_ptr->activate;

	/* Default cost */
	o_ptr->b_cost = k_ptr->cost;

	/* Default cost adjustment */
	o_ptr->cost_adjust = 100;

	/* Hack -- cursed items are, by default, marked "cursed", and cling */
	if (k_ptr->flags3 & (TR3_LIGHT_CURSE))
	{
		o_ptr->ident |= (IDENT_CURSED);
		o_ptr->flags3 |= (TR3_LIGHT_CURSE);
	}
}

/*
 * Help determine an "enchantment bonus" for an object.
 *
 * To avoid floating point but still provide a smooth distribution of bonuses,
 * we simply round the results of division in such a way as to "average" the
 * correct floating point value.
 *
 * This function has been changed.  It uses "Rand_normal()" to choose values
 * from a normal distribution, whose mean moves from zero towards the max as
 * the level increases, and whose standard deviation is equal to 1/4 of the
 * max, and whose values are forced to lie between zero and the max, inclusive.
 *
 * Since "level" rarely passes 100 before Morgoth is dead, it is very
 * rare to get the maximum enchantment on an object.
 *
 * It is always possible (albeit unlikely) to get the maximum enchantment.
 *
 * Note:
 * Maximum level can now vary.
 *
 * A sample distribution of values from "m_bonus(10, N, 128)" is shown below:
 *
 *   N       0     1     2     3     4     5     6     7     8     9    10
 * ---    ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
 *   0   66.37 13.01  9.73  5.47  2.89  1.31  0.72  0.26  0.12  0.09  0.03
 *   8   46.85 24.66 12.13  8.13  4.20  2.30  1.05  0.36  0.19  0.08  0.05
 *  16   30.12 27.62 18.52 10.52  6.34  3.52  1.95  0.90  0.31  0.15  0.05
 *  24   22.44 15.62 30.14 12.92  8.55  5.30  2.39  1.63  0.62  0.28  0.11
 *  32   16.23 11.43 23.01 22.31 11.19  7.18  4.46  2.13  1.20  0.45  0.41
 *  40   10.76  8.91 12.80 29.51 16.00  9.69  5.90  3.43  1.47  0.88  0.65
 *  48    7.28  6.81 10.51 18.27 27.57 11.76  7.85  4.99  2.80  1.22  0.94
 *  56    4.41  4.73  8.52 11.96 24.94 19.78 11.06  7.18  3.68  1.96  1.78
 *  64    2.81  3.07  5.65  9.17 13.01 31.57 13.70  9.30  6.04  3.04  2.64
 *  72    1.87  1.99  3.68  7.15 10.56 20.24 25.78 12.17  7.52  4.42  4.62
 *  80    1.02  1.23  2.78  4.75  8.37 12.04 27.61 18.07 10.28  6.52  7.33
 *  88    0.70  0.57  1.56  3.12  6.34 10.06 15.76 30.46 12.58  8.47 10.38
 *  96    0.27  0.60  1.25  2.28  4.30  7.60 10.77 22.52 22.51 11.37 16.53
 * 104    0.22  0.42  0.77  1.36  2.62  5.33  8.93 13.05 29.54 15.23 22.53
 * 112    0.15  0.20  0.56  0.87  2.00  3.83  6.86 10.06 17.89 27.31 30.27
 * 120    0.03  0.11  0.31  0.46  1.31  2.48  4.60  7.78 11.67 25.53 45.72
 * 128    0.02  0.01  0.13  0.33  0.83  1.41  3.24  6.17  9.57 14.22 64.07
 */
s16b m_bonus(int max, int level, int max_level)
{
	int bonus, stand, value;


	/* Enforce bounds on level */
	if (level >= max_level) level = max_level - 1;
	if (level < 0) level = 0;

	/* Average bonus depends on level  (use perfect rounding) */
	bonus = div_round(max * level, max_level);

	/* The standard deviation is equal to one quarter of the max */
	stand = div_round(max, 4);

	/* Calculate the bonus */
	value = Rand_normal(bonus, stand);


	/* Enforce the minimum value */
	if (value < 0) return (0);

	/* Enforce the maximum value */
	if (value > max) return (max);

	/* Result */
	return (value);
}




/*
 * Cheat -- describe a created object for the user
 */
static void object_mention(object_type *o_ptr)
{
	char o_name[DESC_LEN];

	/* Describe */
	object_desc_store(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Artifact */
	if (artifact_p(o_ptr))
	{
		/* Silly message */
		msg_format("Artifact (%s)", o_name);
	}

	/* Ego-item */
	else if (ego_item_p(o_ptr))
	{
		/* Silly message */
		msg_format("Ego-item (%s)", o_name);
	}

	/* Normal item */
	else
	{
		/* Silly message */
		msg_format("Object (%s)", o_name);
	}
}


/*
 * Change an object with a hidden curse to a cursed ego-item.  -LM-
 *
 * Always remove hidden curse, whether we found a suitable ego-item
 * or not.
 */
bool make_cursed_ego_item(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	object_type *i_ptr;
	object_type object_type_body;

	int i;
	u32b f1, f2, f3;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the object */
	object_copy(i_ptr, o_ptr);


	/* Look many times for a suitable ego-item */
	for (i = 0;; i++)
	{
		/* Set to a quantity of one */
		i_ptr->number = 1;

		/* Default "pval" */
		i_ptr->pval = k_ptr->pval;
		i_ptr->pval2 = 0;
		i_ptr->pval3 = 0;

		/* Default pval-dependent flags */
		i_ptr->flags_pval1 = k_ptr->flags_pval;
		i_ptr->flags_pval2 = 0L;
		i_ptr->flags_pval3 = 0L;

		/* Default object flags */
		i_ptr->flags1 = k_ptr->flags1;
		i_ptr->flags2 = k_ptr->flags2;
		i_ptr->flags3 = k_ptr->flags3;

		/* Default weight */
		i_ptr->weight = k_ptr->weight;

		/* Default magic */
		i_ptr->to_h = k_ptr->to_h;
		i_ptr->to_d = k_ptr->to_d;
		i_ptr->to_a = k_ptr->to_a;

		/* Default power */
		i_ptr->ac = k_ptr->ac;
		i_ptr->dd = k_ptr->dd;
		i_ptr->ds = k_ptr->ds;

		/* Default activation, no longer an ego-item */
		i_ptr->activate = k_ptr->activate;
		i_ptr->ego_item_index = 0;

		/* Default cost */
		i_ptr->b_cost = k_ptr->cost;


		/* Force bad ego-items */
		obj_gen_flags |= (OBJ_GEN_HIDDEN_CURSE);

		/* Apply magic */
		apply_magic(i_ptr, p_ptr->max_depth, FALSE, FALSE, FALSE);

		/* Get object attributes */
		object_flags(i_ptr, &f1, &f2, &f3);

		/* Require light curse, forbid heavy curse */
		if ((f3 & (TR3_LIGHT_CURSE)) && !(f3 & (TR3_HEAVY_CURSE)))
		{
			/* Remove any hidden curse */
			i_ptr->flags3 &= ~(TR3_CURSE_HIDDEN);

			break;
		}

		/* Note failure */
		if (i >= 50)
		{
			/* Cancel the hidden curse */
			o_ptr->flags3 &= ~(TR3_CURSE_HIDDEN);

			return (FALSE);
		}
	}

	/* Restore old quantity */
	i_ptr->number = o_ptr->number;

	/* Ensure correct (possibly different) weight */
	i_ptr->weight *= i_ptr->number;

	/* Restore old damage dice  XXX */
	if ((is_melee_weapon(i_ptr)) || (is_missile(i_ptr)))
	{
		i_ptr->dd = o_ptr->dd;
		i_ptr->ds = o_ptr->ds;
	}

	/* Save the altered object */
	object_copy(o_ptr, i_ptr);

	/* Report success */
	return (TRUE);
}


/*
 * Attempt to change an object into an ego-item.  -MWK-
 * Better only called by apply_magic()
 * The return value is currently unused, but a wizard might be interested
 * in it.
 */
static bool make_ego_item(object_type *o_ptr, bool cursed)
{
	int i, j;
	int level = object_level;

	int e_idx;

	long value, total;

	u32b cursed_flags = TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE;

	ego_item_type *e_ptr;

	alloc_entry *table = alloc_ego_table;


	/* Fail if object already is ego or artifact */
	if (o_ptr->artifact_index) return (FALSE);
	if (o_ptr->ego_item_index) return (FALSE);


	/* Boost level (like with object base types) */
	if (object_level > 0)
	{
		/* Occasionally allow out of depth ego-items */
		if (one_in_(GREAT_EGO))
		{
			/* Boost on 20th level can (rarely) be 16 + 4 * 20 / 3, or 42 */
			int boost = Rand_normal(0, 4 + level / 3);

			/* Boost the depth */
			level += ABS(boost);
		}
	}


	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_ego_size; i++)
	{
		/* Default */
		table[i].prob3 = 0;

		/* Ignore out-of-depth ego-item types */
		if (table[i].level > level) continue;

		/* Get the index */
		e_idx = table[i].index;

		/* Get the actual kind */
		e_ptr = &e_info[e_idx];

		/* Test if this is a possible ego-item for value of (cursed) */
		if (!cursed && (e_ptr->flags3 & (cursed_flags))) continue;
		if (cursed && !(e_ptr->flags3 & (cursed_flags))) continue;

		/* Test if this is a legal ego-item type for this object */
		for (j = 0; j < EGO_TVALS_MAX; j++)
		{
			/* Require identical base type */
			if (o_ptr->tval == e_ptr->tval[j])
			{
				/* Require sval in bounds, lower */
				if (o_ptr->sval >= e_ptr->min_sval[j])
				{
					/* Require sval in bounds, upper */
					if (o_ptr->sval <= e_ptr->max_sval[j])
					{
						/* Accept */
						table[i].prob3 = table[i].prob2;
					}
				}
			}
		}

		/* Total */
		total += table[i].prob3;
	}

	/* No legal ego-items */
	if (total <= 0) return (FALSE);


	/* Pick an ego-item */
	value = rand_int(total);

	/* Find the object */
	for (i = 0; i < alloc_ego_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}

	/* We have one */
	o_ptr->ego_item_index = (byte)table[i].index;
	return (TRUE);
}


/*
 * Mega-Hack -- Attempt to create one of the "Special Objects".
 *
 * We are only called from "make_object()", and we assume that
 * "apply_magic()" is called immediately after we return.
 *
 * Note -- see "make_artifact()" and "apply_magic()".
 *
 * This function now does not run down the list sequentially, which
 * favored some artifacts at the expense of others, but starts at
 * a random index, and then wraps around.
 */
static bool make_artifact_special(object_type *o_ptr)
{
	int i, first_pick;

	int k_idx;

	/* No artifacts, do nothing */
	if (birth_no_artifacts) return (FALSE);

	/* No artifacts in the town */
	if (!object_level) return (FALSE);

	/* Pick an initial artifact at random */
	first_pick = rand_int(ART_MIN_NORMAL);

	/* Check the artifact list (just the "specials") */
	for (i = first_pick; i < first_pick + ART_MIN_NORMAL; i++)
	{
		/* Convert i into an index from 1 to ART_MIN_NORMAL - 1. */
		int choice = (i % (ART_MIN_NORMAL - 1)) + 1;
		artifact_type *a_ptr = &a_info[choice];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		/* Ignore artifacts not of the required tval */
		if ((required_tval) && (a_ptr->tval != required_tval)) continue;

		/* Enforce minimum "depth" */
		if (a_ptr->level > object_level)
		{
			/* Get the "out-of-depth factor" */
			int d = (a_ptr->level - object_level) * 5;

			/* Roll for out-of-depth creation (stay sane) */
			if ((d > 30) || (!one_in_(d))) continue;
		}

		/* Artifact "rarity roll" */
		if (!one_in_(a_ptr->rarity)) continue;

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Assign the template */
		object_prep(o_ptr, k_idx);

		/* Mark the item as an artifact */
		o_ptr->artifact_index = choice;

		/* Success */
		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}



/*
 * Attempt to change an object into an artifact
 *
 * This routine should only be called by "apply_magic()"
 *
 * Note -- see "make_artifact_special()" and "apply_magic()"
 */
static bool make_artifact(object_type *o_ptr)
{
	s16b i, chance;
	s32b total = 0L;
	s32b roll;
	int val;

	/* No artifacts, do nothing */
	if (birth_no_artifacts) return (FALSE);

	/* No artifacts in the town */
	if (!object_level) return (FALSE);

	/* Roll for artifact */
	if (TRUE)
	{
		/* Table of artifact chances */
		s16b *art_chance;

		/* Allocate the "art_chance" array */
		C_MAKE(art_chance, z_info->a_max, s16b);

		/* Initialize the chance table */
		art_chance[ART_MIN_NORMAL - 1] = 0;


		/* Check the artifact list (skip the "specials") */
		for (i = ART_MIN_NORMAL; i < z_info->a_max; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			/* Clear chance */
			chance = 10000;
			art_chance[i] = art_chance[i-1];

			/* Skip "empty" items */
			if (!a_ptr->name) continue;

			/* Cannot make an artifact twice */
			if (a_ptr->cur_num) continue;

			/* Must have the correct fields */
			if (a_ptr->tval != o_ptr->tval) continue;
			if (a_ptr->sval != o_ptr->sval) continue;

			/* Get artifact value modifier */
			val = get_max_potential(i);
			val /= 15;

			/* Artifacts never show up when they would seem wimpy */
			if ((a_ptr->cost) && (a_ptr->cost < object_level * val) &&
				 ((4 * a_ptr->level / 3 + 15) < object_level))
			{
				continue;
			}

			/* XXX XXX Enforce minimum "depth" (loosely) */
			if (a_ptr->level > object_level)
			{
				/* Get the "out-of-depth factor" */
				int d = (a_ptr->level - object_level) * 2;

				/* Penalize out-of-depth creation */
				chance /= d;
			}

			/* Artifact rarity reduces chance */
			if (a_ptr->rarity > 1)
			{
				chance /= a_ptr->rarity;
			}

			/* Tally up totals */
			total += (s32b)chance;

			/* Store this chance */
			art_chance[i] += chance;
		}

		/* No artifact templates are available */
		if (!total) return (FALSE);

		/* Roll for artifact -- at least a 50% chance of failure */
		roll = rand_int((total < 5000L) ? 10000L : (total * 2L));

		/* Try to make the artifact */
		if (roll < total)
		{
			/* Check the artifact list (skip the "specials") */
			for (i = ART_MIN_NORMAL; i < z_info->a_max; i++)
			{
				/* Find that artifact whose chance value contains our roll */
				if (art_chance[i] > roll)
				{
					/* Mark the item as an artifact */
					o_ptr->artifact_index = i;

					/* Success */
					break;
				}
			}
		}

		/* Free the "art_chance" array */
		FREE(art_chance);
	}


	/* Return "object is now an artifact" */
	return ((bool)o_ptr->artifact_index);
}


/*
 * Hack -- Create a "forged" artifact
 */
bool make_fake_artifact(object_type *o_ptr, int artifact_index)
{
	int i;

	artifact_type *a_ptr = &a_info[artifact_index];


	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return (FALSE);

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (FALSE);

	/* Create the artifact */
	object_prep(o_ptr, i);

	/* Save the name */
	o_ptr->artifact_index = artifact_index;

	/* Extract the other fields */
	o_ptr->ac = a_ptr->ac;
	o_ptr->dd = a_ptr->dd;
	o_ptr->ds = a_ptr->ds;
	o_ptr->to_a = a_ptr->to_a;
	o_ptr->to_h = a_ptr->to_h;
	o_ptr->to_d = a_ptr->to_d;
	o_ptr->weight = a_ptr->weight;
	o_ptr->number = MAX(1, a_ptr->max_num);

	/* Extract pvals and related flags */
	o_ptr->pval = a_ptr->pval1;
	o_ptr->flags_pval1 = a_ptr->flags_pval1;
	o_ptr->pval2 = a_ptr->pval2;
	o_ptr->flags_pval2 = a_ptr->flags_pval2;
	o_ptr->pval3 = a_ptr->pval3;
	o_ptr->flags_pval3 = a_ptr->flags_pval3;

	o_ptr->flags1 = a_ptr->flags1;
	o_ptr->flags2 = a_ptr->flags2;
	o_ptr->flags3 = a_ptr->flags3;

	/* Save cost and activation */
	o_ptr->b_cost = a_ptr->cost;
	o_ptr->activate = a_ptr->activate;

	/* Hack -- artifacts ignore some things */
	o_ptr->flags2 |= (TR2_IGNORE_MASK);

	/* Cursed artifacts are obviously cursed, and cling */
	if (a_ptr->flags3 & (TR3_LIGHT_CURSE))
	{
		o_ptr->flags3 |= (TR3_LIGHT_CURSE);
		o_ptr->ident |= (IDENT_CURSED);
	}

	/* Success */
	return (TRUE);
}


/*
 * Apply magic to a melee or missile weapon, or to ammo.
 *
 * Weapons and missiles may have their damage dice sides increased,
 * regardless of the magic applied to the object.  On very rare occasions,
 * non-throwing weapons may have another damage die added.
 *
 * Hack -- note special pval processing for diggers
 */
static void add_magic_to_weapon(object_type *o_ptr, int level, int power)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	int i;

	int tohit1 = randint(5) + m_bonus(5, level, MAX_DEPTH);
	int todam1 = randint(5) + m_bonus(5, level, MAX_DEPTH);

	int tohit2 = m_bonus(10, level, MAX_DEPTH);
	int todam2 = m_bonus(10, level, MAX_DEPTH);


	/*
	 * Weapon is an ego-item.  We alter values in the same direction as
	 * the ego-item template does.  If it doesn't make any changes, we
	 * give good weapons bonuses and bad ones penalties.
	 */
	if (o_ptr->ego_item_index)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->ego_item_index];

		/* Handle bonuses or penalties to Skill and Deadliness */
		if (power > 0)
		{
			/* Good weapons with negative bonuses are left alone */
			if (e_ptr->mod_to_h >= 0) o_ptr->to_h += (tohit1 + tohit2);
			if (e_ptr->mod_to_d >= 0) o_ptr->to_d += (todam1 + todam2);
		}
		else if (power < 0)
		{
			/* Bad weapons with positive bonuses are left alone */
			if (e_ptr->mod_to_h <= 0) o_ptr->to_h -= (tohit1 + tohit2);
			if (e_ptr->mod_to_d <= 0) o_ptr->to_d -= (todam1 + todam2);
		}
	}

	/* Weapon is not an ego-item */
	else
	{
		/* Good */
		if (power > 0)
		{
			/* Enchant */
			o_ptr->to_h += tohit1;
			o_ptr->to_d += todam1;

			/* Sometimes very good */
			if (one_in_(XTRA_BONUS))
			{
				/* Enchant again */
				o_ptr->to_h += tohit2;
				o_ptr->to_d += todam2;
			}
		}

		/* Cursed */
		else if (power < 0)
		{
			/* Penalize */
			o_ptr->to_h -= tohit1;
			o_ptr->to_d -= todam1;

			/* Very cursed */
			if (one_in_(XTRA_BONUS))
			{
				/* Penalize again */
				o_ptr->to_h -= tohit2;
				o_ptr->to_d -= todam2;
			}
		}
	}


	/* Various special handling of weapons */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		{
			/* Very bad */
			if (power < -1)
			{
				/* Hack -- Horrible digging bonus */
				o_ptr->pval = 0 - rand_range(5, 10);
			}

			/* Bad */
			else if (power < 0)
			{
				/* Hack -- Reverse digging bonus */
				o_ptr->pval = 0 - (o_ptr->pval);
			}

			break;
		}


		/* Enhance damage dice of melee weapons.  -LM- */
		case TV_HAFTED:

		case TV_POLEARM:
		case TV_SWORD:
		{
			int extra_depth = object_level - k_ptr->level;
			int sides = o_ptr->ds;

			/* Randomize the limit slightly (swords are penalized) */
			int limit = (o_ptr->tval == TV_SWORD ? rand_range(30, 34) :
			             rand_range(38, 42));

			/* Roll for better dice */
			for (i = 0; i < 3; i++)
			{
				/* One in 10 chance to improve a dagger to 1d6 */
				int odds = 8 + (o_ptr->dd * sides) / 2;

				/* Make well OOD weapons more useful */
				if (extra_depth > 20 + 8 * i) odds--;
				if (extra_depth > 40 + 8 * i) odds--;
				if (extra_depth > 60 + 8 * i) odds--;
				if (extra_depth > 80 + 8 * i) odds--;

				/* Throwing weapons are very likely to be upgraded */
				if (o_ptr->flags1 & TR1_THROWING) odds /= 2;

				/* Require that weapon be well in depth */
				if (extra_depth >= rand_range(5, 10) + (i * 8)) break;

				/* Weapon must not be over-powerful */
				if (o_ptr->dd * sides > limit) break;

				/* Must make the rarity roll */
				if (!one_in_(odds)) break;

				/* Improve the dice (with some randomization) */
				if      (i == 0) sides = div_round(3 * o_ptr->ds, 2);
				else if (i == 1) sides = 2 * o_ptr->ds;
				else             sides = div_round(5 * o_ptr->ds, 2);
			}

			/* Adjust dice */
			o_ptr->ds = sides;


			/* Upon occasion, throwing weapons may be perfectly balanced. */
			if ((k_ptr->flags1 & (TR1_THROWING)) && (extra_depth > randint(9)))
			{
				o_ptr->flags1 |= (TR1_THROWING);

				if ((power >= 0) && (one_in_(5)))
				{
					o_ptr->flags1 |= (TR1_PERFECT_BALANCE);
				}
			}

			/*
			 * On very rare occasions, non-throwing weapons will get an
			 * extra damage die.
			 */
			else
			{
				/* Must not have super dice already */
				if (o_ptr->ds <= 3 * k_ptr->ds / 2)
				{
					/* Must have low dice and be well in depth */
					if ((o_ptr->dd == 1) && (extra_depth >= 15))
					{
						if (one_in_(80)) o_ptr->dd = 2;
					}
					else if ((o_ptr->dd == 2) && (extra_depth >= 15))
					{
						if (one_in_(160)) o_ptr->dd = 3;
					}
				}
			}

			break;
		}

		/* Enhance damage dice of missiles. -LM- */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			int odds = 6;

			/* Drop better ammo if it's out of depth */
			if (object_level - k_ptr->level > 30) odds--;
			if (object_level - k_ptr->level > 60) odds--;

			/* Boost the dice */
			if ((object_level >= 30) && (one_in_(odds)))
			{
				/* Up to +5 on rare occasions */
				o_ptr->ds += rand_int(m_bonus(5, object_level, MAX_DEPTH));
			}

			break;
		}
	}

	/* Weapons and missiles in the dungeon may bear a hidden curse */
	if ((object_level >= 15) && (!(obj_gen_flags & (OBJ_GEN_STORE))) &&
		(((o_ptr->ego_item_index) && (one_in_(20))) ||
	     ((!o_ptr->ego_item_index) && (one_in_(200)))))
	{
		o_ptr->flags3 |= (TR3_CURSE_HIDDEN);
	}
}


/*
 * Apply magic to any armor.
 */
static void add_magic_to_armor(object_type *o_ptr, int level, int power)
{
	int toac1, toac2;

	/* Boots and handgear aren't as important to total AC. */
	if ((o_ptr->tval == TV_BOOTS) || (o_ptr->tval == TV_GLOVES))
	{
		toac1 = randint(3) + m_bonus(3, level, MAX_DEPTH);
		toac2 = m_bonus(8, level, MAX_DEPTH);
	}
	else
	{
		toac1 = randint(4) + m_bonus(4, level, MAX_DEPTH);
		toac2 = m_bonus(10, level, MAX_DEPTH);
	}


	/* Armor is an ego-item. */
	if (o_ptr->ego_item_index)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->ego_item_index];

		/* Handle bonuses or penalties to AC */
		if (power > 0)
		{
			/* Good armors with negative bonuses are left alone */
			if (e_ptr->mod_to_a >= 0) o_ptr->to_a += (toac1 + toac2);
		}
		else if (power < 0)
		{
			/* Bad armors with positive bonuses are left alone */
			if (e_ptr->mod_to_a <= 0) o_ptr->to_a -= (toac1 + toac2);
		}
	}

	/* Armor is not an ego-item */
	else
	{
		/* Good */
		if (power > 0)
		{
			/* Enchant */
			o_ptr->to_a += toac1;

			/* Sometimes very good */
			if (one_in_(XTRA_BONUS))
			{
				/* Enchant again */
				o_ptr->to_a += toac2;
			}
		}

		/* Cursed */
		else if (power < 0)
		{
			/* Penalize */
			o_ptr->to_a -= toac1;

			/* Very cursed */
			if (one_in_(XTRA_BONUS))
			{
				/* Penalize again */
				o_ptr->to_a -= toac2;
			}
		}
	}

	/* Apply various special magics */
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		{
			/* Hack -- Rating boost */
			if (power >= 0) level_rating += 15;

			/* Mention the item */
			if (cheat_peek) object_mention(o_ptr);
			if (can_precog(100, LEV_REQ_PRECOG + 10))
			{
				precog_msg(PRECOG_OBJ_GREAT_POWER);
			}

			/* Done */
			break;
		}
	}

	/* Armor in the dungeon may bear a hidden curse */
	if ((object_level >= 15) && (!(obj_gen_flags & (OBJ_GEN_STORE))) &&
		(((o_ptr->ego_item_index) && (one_in_(20))) ||
	     ((!o_ptr->ego_item_index) && (one_in_(200)))))
	{
		o_ptr->flags3 |= (TR3_CURSE_HIDDEN);
	}
}



/*
 * Apply magic to rings and amulets
 *
 * Hack -- note special rating boost for ring of speed
 * Hack -- note special rating boost for certain amulets
 * Hack -- note special "pval boost" code for ring of speed
 * Hack -- note that some items must be cursed (or blessed)
 */
static void add_magic_to_ring(object_type *o_ptr, int level, int power)
{
	int dd = 0, ds = 0;

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		/* Rings */
		case TV_RING:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Ring of WOE */
				case SV_RING_WOE:
				{
					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->to_a = 0 - (5 + m_bonus(10, level, MAX_DEPTH));
					o_ptr->pval = 0 - (1 + m_bonus(5, level, MAX_DEPTH));

					/* Does damage when thrown */
					dam_to_dice(3 * level / 2, &dd, &ds, TRUE);
					o_ptr->dd = dd;
					o_ptr->ds = ds;

					break;
				}

				/* Ring of Aggravation */
				case SV_RING_AGGRAVATION:
				{
					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);
					}
					break;
				}

				/* Awareness */
				case SV_RING_AWARENESS:
				{
					/* Bonus to awareness */
					o_ptr->pval = 1 + m_bonus(4, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Invisibility */
				case SV_RING_INVIS:
				{
					o_ptr->pval = 1 + m_bonus(5, level, MAX_DEPTH);
					break;
				}

				/* Ring of Speed! */
				case SV_RING_SPEED:
				{
					/* Base speed (1 to 10) */
					o_ptr->pval = randint(5) + m_bonus(5, level, MAX_DEPTH);

					/* Super-charge the ring */
					while (one_in_(2)) o_ptr->pval++;

					/* Cursed Ring */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);
						break;
					}
					else
					{
						/* Rating boost -- only if good */
						level_rating += 20;
					}

					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);

					if (can_precog(100, LEV_REQ_PRECOG + 10))
					{
						precog_msg(PRECOG_OBJ_GREAT_POWER);
					}

					break;
				}

				/* Strength, Constitution, Dexterity */
				case SV_RING_STR:
				case SV_RING_CON:
				case SV_RING_DEX:
				{
					/* Stat bonus */
					o_ptr->pval = 1 + m_bonus(5, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Ring of Protection */
				case SV_RING_PROTECTION:
				{
					/* Bonus to armor class */
					o_ptr->to_a = rand_range(6, 10) + m_bonus(10, level, MAX_DEPTH);

					/* Possible super-charging */
					if (one_in_(10)) o_ptr->to_a += randint(10);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonus */
						o_ptr->to_a = 0 - (o_ptr->to_a);
					}

					break;
				}

				/* Acid, Electricity, Flames, Ice, Venom */
				case SV_RING_ACID:
				case SV_RING_ELEC:
				case SV_RING_FLAMES:
				case SV_RING_ICE:
				case SV_RING_POIS:
				{
					/* Bonus to armor class */
					o_ptr->to_a = rand_range(6, 10) + m_bonus(10, level, MAX_DEPTH);

					/* Possible super-charging */
					if (one_in_(10)) o_ptr->to_a += randint(10);

					break;
				}

				/* Ring of Deadliness */
				case SV_RING_DEADLINESS:
				{
					/* Bonus to Deadliness */
					o_ptr->to_d = rand_range(6, 10) + m_bonus(10, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonus */
						o_ptr->to_d = 0 - (o_ptr->to_d);
					}

					break;
				}

				/* Ring of Skill */
				case SV_RING_SKILL:
				{
					/* Bonus to Skill */
					o_ptr->to_h = rand_range(7, 11) + m_bonus(10, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonus */
						o_ptr->to_h = 0 - (o_ptr->to_h);
					}

					break;
				}

				/* Ring of Combat */
				case SV_RING_COMBAT:
				{
					/* Bonus to both Deadliness and to Skill */
					o_ptr->to_d = rand_range(3, 8) + m_bonus(7, level, MAX_DEPTH);
					o_ptr->to_h = rand_range(3, 8) + m_bonus(7, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->to_h = 0 - (o_ptr->to_h);
						o_ptr->to_d = 0 - (o_ptr->to_d);
					}

					break;
				}
			}

			break;
		}


		/* Amulets */
		case TV_AMULET:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Amulet of Doom -- always cursed */
				case SV_AMULET_DOOM:
				{
					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = 0 - (randint(5) + m_bonus(5, level, MAX_DEPTH));
					o_ptr->to_a = 0 - (randint(5) + m_bonus(5, level, MAX_DEPTH));

					/* Does damage when thrown */
					dam_to_dice(level * 2, &dd, &ds, TRUE);
					o_ptr->dd = dd;
					o_ptr->ds = ds;

					break;
				}

				/* Amulet of Infravision */
				case SV_AMULET_INFRAVISION:
				{
					/* Pval varies by level, but with a fast rise and early cap */
					o_ptr->pval = 1 + m_bonus(3, level, 40);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->pval = 0 - 3 * (o_ptr->pval);
					}

					break;
				}

				/* Amulet of Magic Mastery */
				case SV_AMULET_MAGIC_MASTERY:
				{
					o_ptr->pval = 1 + m_bonus(3, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Amulet of the Magi -- never cursed */
				case SV_AMULET_THE_MAGI:
				{
					o_ptr->pval = randint(3) + m_bonus(3, level, MAX_DEPTH);
					o_ptr->to_a = rand_range(3, 8) + m_bonus(8, level, MAX_DEPTH);

					/* Boost the rating */
					level_rating += 10;


					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);

					if (can_precog(100, LEV_REQ_PRECOG + 10))
					{
						precog_msg(PRECOG_OBJ_GREAT_POWER);
					}

					break;
				}

				/* Amulet of Mana -- never cursed */
				case SV_AMULET_MANA:
				{
					o_ptr->pval = 1 + m_bonus(5, level, MAX_DEPTH);

					/* Boost the rating */
					level_rating += o_ptr->pval / 2;

					break;
				}

				/* Amulet of wisdom/intelligence/charisma */
				case SV_AMULET_WISDOM:
				case SV_AMULET_INTELLIGENCE:
				case SV_AMULET_CHARISMA:
				{
					o_ptr->pval = 1 + m_bonus(5, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Amulet of Magical Protection */
				case SV_AMULET_SAVING_THROW:
				{
					o_ptr->pval = 1 + m_bonus(3, level, MAX_DEPTH);

					o_ptr->ac = 5 + m_bonus(15, level, MAX_DEPTH);

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->pval = 0 - (o_ptr->pval);
						o_ptr->ac = 0 - (o_ptr->ac);
					}

					break;
				}

				case SV_AMULET_TRICKERY:
				{
					o_ptr->pval = 1 + m_bonus(2, level, MAX_DEPTH);
					o_ptr->pval2 = -1;
					o_ptr->flags_pval2 = TR_PVAL_LIGHT;

					/* Cursed */
					if (power < 0)
					{
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}
			}

			break;
		}
	}
}


/*
 * Apply magic to miscellaneous items
 */
static void add_magic_to_others(object_type *o_ptr, int level, int power)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Unused */
	(void)power;

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		case TV_LITE:
		{
			/* Give random fuel */
			if (o_ptr->pval > 0) o_ptr->pval = randint(o_ptr->pval);

			/* Mark it as a light source */
			o_ptr->flags_pval2 |= (TR_PVAL_LIGHT);

			/* Hack -- torches have a light radius of 2 (normally) */
			if (o_ptr->sval == SV_LITE_TORCH) o_ptr->pval2 = 2;

			/* Hack -- lanterns have a light radius of 2 */
			if (o_ptr->sval == SV_LITE_LANTERN) o_ptr->pval2 = 2;

			/* Light sources in the dungeon may bear a hidden curse */
			if ((object_level >= 15) && (!(obj_gen_flags & (OBJ_GEN_STORE))) &&
				(((o_ptr->ego_item_index) && (one_in_(20))) ||
				 ((!o_ptr->ego_item_index) && (one_in_(200)))))
			{
				o_ptr->flags3 |= (TR3_CURSE_HIDDEN);
			}

			break;
		}

		case TV_WAND:
		case TV_STAFF:
		{
			/*
			 * The wand or staff gets a number of initial charges equal
			 * to between 1/2 (rounded up) and the full object kind's pval.
			 */
			o_ptr->pval = rand_range((k_ptr->pval + 1) / 2, k_ptr->pval);

			/* Hack -- handle multiple wands */
			if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
				o_ptr->pval *= o_ptr->number;

			break;
		}

		case TV_ROD:
		{
			/* Transfer the pval. */
			o_ptr->pval = k_ptr->pval;

			/* Hack -- handle multiple rods */
			if (o_ptr->number > 1) o_ptr->pval *= o_ptr->number;

			break;
		}

		case TV_CHEST:
		{
			/* Hack -- pick a "value/difficulty" */
			if ((required_tval == TV_CHEST) && (character_dungeon))
				o_ptr->pval = object_level + rand_range(-5, 5);
			else
				o_ptr->pval = (k_info[o_ptr->k_idx].level) + rand_range(-5, 5);

			/* Value/difficulty cannot be less than 5. */
			if (o_ptr->pval < 5) o_ptr->pval = 5;

			/* Never exceed "value/difficulty" of 99. */
			if (o_ptr->pval > 99) o_ptr->pval = 99;

			/* Chests are nice */
			level_rating += 10;

			break;
		}

		case TV_POTION:
		{
			/* Grenades come in many varieties */
			if (o_ptr->sval == SV_POTION_GRENADE)
			{
				int choice = rand_int(MIN(100, 40 + p_ptr->depth));
				s32b dam = 1L + rand_range(3 * object_level / 2,
				                           5 * object_level / 2);
				int adjust = 100;
				int pval;

				/* Determine type of grenade */
				if      (choice < 10) o_ptr->pval = ESSENCE_ACID;
				else if (choice < 20) o_ptr->pval = ESSENCE_ELEC;
				else if (choice < 30) o_ptr->pval = ESSENCE_FIRE;
				else if (choice < 40) o_ptr->pval = ESSENCE_COLD;
				else if (choice < 48) o_ptr->pval = ESSENCE_POIS;

				else if (choice < 54) o_ptr->pval = ESSENCE_LITE;
				else if (choice < 60) o_ptr->pval = ESSENCE_DARK;

				else if (choice < 64) o_ptr->pval = ESSENCE_CONFU;
				else if (choice < 68) o_ptr->pval = ESSENCE_FORCE;
				else if (choice < 72) o_ptr->pval = ESSENCE_NEXUS;
				else if (choice < 76) o_ptr->pval = ESSENCE_NETHR;
				else if (choice < 80) o_ptr->pval = ESSENCE_CHAOS;
				else if (choice < 83) o_ptr->pval = ESSENCE_TIME;
				else if (choice < 88) o_ptr->pval = ESSENCE_MAGIC;
				else if (choice < 91) o_ptr->pval = ESSENCE_LIFE;
				else if (choice < 96) o_ptr->pval = ESSENCE_DEATH;
				else if (choice < 98) o_ptr->pval = ESSENCE_KNOW;
				else if (choice < 99) o_ptr->pval = ESSENCE_SHIELD;
				else if (choice <100) o_ptr->pval = ESSENCE_BLESS;

				/* Note pval */
				pval = o_ptr->pval;

				/* Get damage adjustment for this type of magic */
				if (essence_to_magic(&adjust, &pval) < 0) break;

				/* Adjust damage */
				dam = dam * adjust / 100;

				/* Turn into dice */
				dam_to_dice(dam, (int *)&o_ptr->dd, (int *)&o_ptr->ds, FALSE);

				/* Grenades have real value */
				o_ptr->b_cost = 10L + (long)(dam * dam / 30L);

				/* Identify XXX XXX */
				object_aware(o_ptr);
				object_known(o_ptr);
			}
			break;
		}

		case TV_JUNK:
		{
			/* Boulders vary in size and weight */
			if (o_ptr->sval == SV_BOULDER)
			{
				/* Boulders come in specific weights (better stacking) */
				int weights[19][2] = {
					{ 10, 0 },  /* Weight, first dungeon level */
					{ 15, 3 },
					{ 25, 5 },
					{ 35, 8 },
					{ 45, 10 },
					{ 60, 14 },
					{ 80, 19 },
					{ 100, 23 },
					{ 120, 27 },
					{ 150, 32 },
					{ 175, 37 },
					{ 200, 41 },
					{ 230, 47 },
					{ 260, 52 },
					{ 300, 60 },
					{ 350, 70 },
					{ 400, 80 },
					{ 450, 90 },
					{ 500, 99 }};

				int i, dam;
				int dice, sides;

				/* Find the correct weight and assign it */
				for (i = 18; i > 0; i--)
				{
					if (level >= weights[i][1]) break;
				}
				o_ptr->weight = weights[i][0];


				/* Damage depends on weight (can be as great as 300) */
				dam = 3 * o_ptr->weight / 5;

				/* Turn raw damage into dice (do not randomize) */
				dam_to_dice(dam, &dice, &sides, FALSE);
				o_ptr->dd = dice;
				o_ptr->ds = sides;
			}

			/* Blankets can protect against many things */
			else if (o_ptr->sval == SV_BLANKET)
			{
				/* 2/3rds of blankets protect against the elements */
				if (!one_in_(3))
				{
					o_ptr->flags2 |= (TR2_RES_ACID);
					o_ptr->flags2 |= (TR2_RES_ELEC);
					o_ptr->flags2 |= (TR2_RES_FIRE);
					o_ptr->flags2 |= (TR2_RES_COLD);
					o_ptr->flags2 |= (TR2_IGNORE_ACID);
					o_ptr->flags2 |= (TR2_IGNORE_ELEC);
					o_ptr->flags2 |= (TR2_IGNORE_FIRE);
					o_ptr->flags2 |= (TR2_IGNORE_COLD);

					o_ptr->b_cost = 3200;

					/* (Damage is usually 4 to 30 per hit) */
					o_ptr->pval = o_ptr->ac = (level * 5) +
						rand_range(400, 600);
				}

				/* The others protect against special attacks */
				else
				{
					o_ptr->flags2 |= (TR2_RES_DISEN);
					o_ptr->flags2 |= (TR2_RES_DRAIN);
					o_ptr->flags3 |= (TR3_BLESSED);

					o_ptr->b_cost = 4000;

					/* Proof against 30 to 40 attacks */
					o_ptr->pval = o_ptr->ac = rand_range(30, 40);
				}

				/* Blankets can be cursed */
				if (one_in_(3)) o_ptr->ident |= (IDENT_CURSED);
			}

			break;
		}
	}
}


/*
 * Calculate the plusses on an ego-item
 */
static s16b ego_item_bonus(int val)
{
	/* Half the bonus is random, the other half is guaranteed */
	int tmp = rand_range(ABS(val / 2), ABS(val));

	/* Return a positive or negative value */
	if (val < 0) return (-tmp);
	else         return (tmp);
}

/*
 * Choose one of a selection of flags at random.
 *
 * Flags must all be in the same set.
 */
static u32b pick_flag(u32b choices)
{
	int count = 0, i, pick;

	/* Count the choices */
	for (i = 0; i < 32; i++)
	{
		if (choices & (1L << i)) count++;
	}

	/* Accept the only choice */
	if (count == 1) return (choices);

	/* Pick at random */
	pick = randint(count);

	/* Scan all the bits in this set */
	for (count = 0, i = 0; i < 32; i++)
	{
		/* When we reach our chosen flag, return it */
		if (choices & (1L << i)) count++;
		if (count == pick) return (1L << i);
	}

	/* Oops */
	return (0L);
}

/*
 * Apply random qualities to an object according to information from the
 * "xtra" field.
 */
void apply_random_qualities(object_type *o_ptr)
{
	u16b xtra = 0;
	int i;

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];


	/* Artifact */
	if (o_ptr->artifact_index)
	{
		artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

		xtra = a_ptr->xtra;
	}

	/* Ego-item */
	else if (o_ptr->ego_item_index)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->ego_item_index];

		xtra = e_ptr->xtra;
	}

	/* The base object kind sometimes also has random qualities */
	xtra |= (k_ptr->xtra);

	/* No random flags to add */
	if (!xtra) return;


	/* Sustain */
	if (xtra & (XTRA_SUSTAIN))
	{
		f1 |= pick_flag(TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
		                TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR);
	}

	/* Random high resist */
	if (xtra & (XTRA_HIGH_RES))
	{
		f2 |= pick_flag(TR2_RES_BLIND | TR2_RES_CONFU | TR2_RES_SOUND |
		                TR2_RES_SHARD | TR2_RES_NETHR | TR2_RES_NEXUS |
		                TR2_RES_CHAOS | TR2_RES_DISEN | TR2_RES_POIS);
	}

	/* Random low resist */
	if (xtra & (XTRA_LOW_RES))
	{
		if (one_in_(4))
		{
			f2 |= pick_flag(TR2_RES_LITE | TR2_RES_DARK);
		}
		else
		{
			f2 |= pick_flag(TR2_RES_ACID | TR2_RES_FIRE |
			                TR2_RES_COLD | TR2_RES_ELEC);
		}
	}

	/* Random resist */
	if (xtra & (XTRA_ANY_RES))
	{
		f2 |= pick_flag(TR2_RES_ACID  | TR2_RES_FIRE  | TR2_RES_COLD |
		                TR2_RES_ELEC  | TR2_RES_POIS  |
		                TR2_RES_LITE  | TR2_RES_DARK  |
		                TR2_RES_BLIND | TR2_RES_CONFU | TR2_RES_FEAR |
		                TR2_RES_SOUND | TR2_RES_SHARD | TR2_RES_NETHR |
		                TR2_RES_NEXUS | TR2_RES_CHAOS | TR2_RES_DISEN);
	}

	/* Get a random power */
	if (xtra & (XTRA_POWER))
	{
		if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
		{
			f3 |= pick_flag(TR3_SLOW_DIGEST | TR3_FEATHER | TR3_LITE |
			                TR3_REGEN | TR3_TELEPATHY | TR3_SEE_INVIS |
			                TR3_FREE_ACT | TR3_HOLD_LIFE | TR3_BLESSED);
		}
		else
		{
			f3 |= pick_flag(TR3_SLOW_DIGEST | TR3_FEATHER | TR3_LITE |
			                TR3_REGEN | TR3_TELEPATHY | TR3_SEE_INVIS |
			                TR3_FREE_ACT | TR3_HOLD_LIFE);
		}
	}

	/* Chance of being a vorpal or concussion weapon */
	if (xtra & (XTRA_VORPAL))
	{
		if ((object_level > k_ptr->level + 3) && (one_in_(3)))
			f1 |= (TR1_VORPAL);
	}

	/* Chance of impact blows */
	if (xtra & (XTRA_IMPACT))
	{
		if ((object_level > k_ptr->level + 3) && (one_in_(3)))
			f3 |= (TR3_IMPACT);
	}

	/* Chance of being extra heavy and powerful (not in stores) */
	if ((xtra & (XTRA_CAN_BE_HEAVY)) && (!(obj_gen_flags & (OBJ_GEN_STORE))))
	{
		int dice, in_depth, odds;

		/* Allow up to +2 dice on occasion */
		for (dice = o_ptr->dd; dice < o_ptr->dd + 2;)
		{
			/* High damage reduce odds, extra depth increases odds */
			odds = 4 + (o_ptr->dd * o_ptr->ds) / 2;
			if (object_level - k_ptr->level > 20) odds--;
			if (object_level - k_ptr->level > 40) odds--;
			if (object_level - k_ptr->level > 60) odds--;
			if (object_level - k_ptr->level > 80) odds--;

			/* Stop most of the time */
			if (!one_in_(odds)) break;

			/* Objects have to be well in-depth to become powerful */
			in_depth = 10 * (dice - o_ptr->dd) + rand_range(10, 15);

			/* Stop if object is not well in depth */
			if (object_level < k_ptr->level + in_depth) break;

			/* Add another damage die */
			dice++;
		}

		/* We've got a heavy weapon */
		if (dice > o_ptr->dd)
		{
			/* Add weight */
			o_ptr->weight += 50 * (dice - o_ptr->dd);

			/* Add dice */
			o_ptr->dd = dice;
		}
	}

	/* Make ego-item light sources more interesting */
	if ((xtra & (XTRA_LIGHT_QUALITY)) && (one_in_(2)))
	{
		int choice;
		if((o_ptr->ego_item_index == EGO_BRIGHTNESS) ||
		   (o_ptr->ego_item_index == EGO_SHADOWS))
		   choice = randint(6) + 1;
		else
			choice = randint(7);

		/* Larger light radius */
		if ((choice == 1) && (o_ptr->ego_item_index != EGO_BRIGHTNESS))
		{
			if (o_ptr->flags_pval2 & (TR_PVAL_LIGHT))
			{
				o_ptr->pval2 += 1;
				o_ptr->b_cost += 1500;
			}
		}

		/* Requires no fuel */
		else if (choice == 2) f3 |= (TR3_NOFUEL);

		/* Grants see invisible */
		else if (choice == 3) f3 |= (TR3_SEE_INVIS);

		/* Grants resist blindness */
		else if (choice == 4) f2 |= (TR2_RES_BLIND);

		/* Grants resist light */
		else if (choice == 5) f2 |= (TR2_RES_LITE);

		/* Grants resist dark */
		else if (choice == 6) f2 |= (TR2_RES_DARK);

		/* Activates to light up a room */
		else if ((choice == 7) && (!o_ptr->activate))
		{
			o_ptr->activate = ACTIV_RANDOM_LIGHT_AREA;
			o_ptr->b_cost += 1500;

			/* Lights of Shadows activate for darkness */
			if (o_ptr->ego_item_index == EGO_SHADOWS)
			{
				o_ptr->activate = ACTIV_RANDOM_DARK_AREA;
			}

		}
	}

	/* Make ego-item missile launchers more interesting */
	if ((xtra & (XTRA_MISSILE_POWER)) && (one_in_(2)))
	{
		int choice = randint(6);

		/* Piercing shot */
		if (choice == 1) o_ptr->activate = ACTIV_SHOT_PIERCING;

		/* Deadly shot (or accurate shot) */
		if (choice == 2)
		{
			if ((o_ptr->ego_item_index == EGO_ACCURACY) ||
			    (o_ptr->ego_item_index == EGO_EXTRA_SHOTS))
			{
				o_ptr->activate = ACTIV_SHOT_ACCURATE;
			}
			else
			{
				o_ptr->activate = ACTIV_SHOT_DAMAGE;
			}
		}

		/* Impact shot */
		if (choice == 3)
		{
			o_ptr->activate = ACTIV_SHOT_IMPACT;
		}

		/* Accurate shot (or deadly shot) */
		if (choice == 4)
		{
			if ((o_ptr->ego_item_index == EGO_VELOCITY) ||
			    (o_ptr->ego_item_index == EGO_EXTRA_MIGHT))
			{
				o_ptr->activate = ACTIV_SHOT_DAMAGE;
			}
			else
			{
				o_ptr->activate = ACTIV_SHOT_ACCURATE;
			}
		}

		/* Burning shot */
		if (choice == 5)
		{
			o_ptr->activate = ACTIV_SHOT_FIRE;
			if (one_in_(2)) f2 |= (TR2_RES_FIRE);
		}

		/* Freezing shot */
		if (choice == 6)
		{
			o_ptr->activate = ACTIV_SHOT_COLD;
			if (one_in_(2)) f2 |= (TR2_RES_COLD);
		}

		/* Add some value */
		o_ptr->b_cost += 1500;
	}

	/* Often have enhanced damage dice - not for artifacts */
	if ((xtra & (XTRA_ADD_DICE_SIDES)) && (!(o_ptr->artifact_index)) &&
	    (o_ptr->ds == k_ptr->ds) && (object_level > k_ptr->level))
	{
		/* Lower-level objects (below 68) reach maximum values earlier */
		int max_lev = MIN(MAX_DEPTH, k_ptr->level + 60);

		/* Bonuses depend on object level being well above base level */
		xtra = m_bonus(k_ptr->ds, object_level - k_ptr->level, max_lev);

		/* Enhance the dice sides (up to 100%) */
		o_ptr->ds += rand_int(xtra + 1);
	}

	/* Enhance object value for new flags */
	if ((!o_ptr->artifact_index) && ((f1) || (f2) || (f3)))
	{
		/* Ignore new pvals (they are handled in "object_value()") */
		for (i = 32; i < 128; i++)
		{
			/* Convert a flag index to a flag */
			int flag = 1L << (i % 32);

			/* Skip if we're not adding this flag */
			if ((i >= 32) && (i <  64) && (!(f1 & (flag)))) continue;
			if ((i >= 64) && (i <  96) && (!(f2 & (flag)))) continue;
			if ((i >= 96) && (i < 128) && (!(f3 & (flag)))) continue;

			/* Skip if object already had this flag */
			if ((i >= 32) && (i <  64) && (o_ptr->flags1 & (flag))) continue;
			if ((i >= 64) && (i <  96) && (o_ptr->flags2 & (flag))) continue;
			if ((i >= 96) && (i < 128) && (o_ptr->flags3 & (flag))) continue;

			/* Add the cost of this flag (divided by 2 or 20) */
			if (o_ptr->ego_item_index)
			{
				o_ptr->b_cost += get_cost_of_flag(i, 0) * 5L;
			}
			else
			{
				o_ptr->b_cost += get_cost_of_flag(i, 0) / 2L;
			}
		}
	}

	/* Assign new flags */
	o_ptr->flags1 |= (f1);
	o_ptr->flags2 |= (f2);
	o_ptr->flags3 |= (f3);
}


/*
 * Complete the creation of an object by applying magic to it.
 *
 * This includes not only rolling for random bonuses, but also putting the
 * finishing touches on ego-items and artifacts, giving charges to wands and
 * staffs, giving fuel to light sources, and placing traps on chests.
 *
 * In particular, note that "Instant Artifacts", if "created" by an external
 * routine, must pass through this function to complete the actual creation.
 *
 * The base "chance" of the item being "good" increases with the "level"
 * parameter, which is usually derived from the dungeon level, being equal
 * to 80% of the level plus 20, up to a maximum of 75.  If "good" is true,
 * then the object is guaranteed to be "good".  If an object is "good", then
 * the chance that the object will be "great" (ego-item or artifact), also
 * increases with the "level", being equal to half the level, up to
 * a maximum of 20.  If "great" is true, then the object is guaranteed to be
 * "great".  At dungeon level 69 and below, 15/100 objects are "great".
 *
 * If the object is not "good", there is a chance it will be "cursed", and
 * if it is "cursed", there is a chance it will be "broken".  These chances
 * are related to the "good" / "great" chances above.
 *
 * Otherwise "normal" rings and amulets will be "good" half the time and
 * "cursed" half the time, unless the ring/amulet is always good or cursed.
 *
 * If "okay" is positive, and the object is going to be "great", then there is
 * a chance that an artifact will be created.  This is true even if both the
 * "good" and "great" arguments are false.  Objects which are forced "great"
 * get three extra artifact-creation rolls.
 *
 * If "okay" has a value of -1, no "great" items can be created.
 * If "okay" has a value of -2, no "good" items can be created.
 */
void apply_magic(object_type *o_ptr, int lev, int okay, bool good, bool great)
{
	int i, rolls, f1, f2, power, adjust;

	/* Assume not cursed */
	bool cursed = FALSE;

	bool can_make_ego = TRUE;


	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;


	/* Base chance of being "good" */
	f1 = 4 * lev / 5 + 20;

	/* Maximal chance of being "good" */
	if (f1 > 75) f1 = 75;

	/* Base chance of being "great" */
	f2 = lev / 2;

	/* Maximal chance of being "great" */
	if (f2 > 20) f2 = 20;

	/* Greater probability of being "great" when dropped by a unique */
	if (obj_gen_flags & (OBJ_GEN_UNIQUE)) f2 = (f2 * 2) + 10;

	/* Skip rolling for "good", "great", and artifacts  XXX XXX */
	if (obj_gen_flags & (OBJ_GEN_NO_ROLLS))
	{
		if (o_ptr->ego_item_index) power = 2;
		else power = 0;
		can_make_ego = FALSE;
		goto skipped_rolls;
	}

	/* Force curses, skip rolling for "good", etc.  XXX */
	if (obj_gen_flags & (OBJ_GEN_HIDDEN_CURSE))
	{
		power = -2;
		cursed = TRUE;
		goto skipped_rolls;
	}


	/* Apply (bad) luck */
	if (p_ptr->luck < 100)
	{
		f1 = f1 * p_ptr->luck / 100;
		f2 = f2 * p_ptr->luck / 100;
	}

	/* Hack -- light sources are much more likely to be ego-items */
	if (o_ptr->tval == TV_LITE)
	{
		/* Are forced "great" if "good" */
		if (good) f2 = 100;

		/* Otherwise, are 2x as likely to be ego-items (level 15+) */
		else if (object_level >= 15)  f2 *= 2;
	}

	/* Apply restrictions on "good" and "great" items */
	if (okay <= -1) f2 = 0;
	if (okay <= -2) f1 = 0;


	/* Assume normal */
	power = 0;

	/* Roll for "good" */
	if (good || (rand_int(100) < f1))
	{
		/* Assume "good" */
		power = 1;

		/* Roll for "great" */
		if (great || (rand_int(100) < f2)) power = 2;
	}

	/* Roll for "bad" */
	else if ((!(obj_gen_flags & (OBJ_GEN_STORE))) && (rand_int(100) < f1))
	{
		/* Assume "bad" */
		power = -1;

		/* Roll for "awful" */
		if (rand_int(100) < f2) power = -2;

		/* Note cursed */
		cursed = TRUE;
	}


	/* Assume no rolls */
	rolls = 0;

	/* Get one roll if excellent */
	if (power >= 2) rolls = 1;

	/* Hack -- Get four rolls if forced great */
	if (great) rolls = 4;

	/* Get no rolls if not allowed */
	if (!okay || o_ptr->artifact_index) rolls = 0;

	/* Roll for artifacts if allowed */
	for (i = 0; i < rolls; i++)
	{
		/* Roll for an artifact */
		if (make_artifact(o_ptr)) break;
	}


	/* Skip all rolling for "good", "great", and artifacts */
	skipped_rolls:


	/* Hack -- analyze artifacts */
	if (o_ptr->artifact_index)
	{
		artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

		/* Hack -- Mark the artifact as "created" */
		a_ptr->cur_num = 1;

		/* Artifacts usually do not come in groups, but they can */
		o_ptr->number = MAX(1, a_ptr->max_num);

		/* Extract the other fields */
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;
		o_ptr->to_a = a_ptr->to_a;
		o_ptr->to_h = a_ptr->to_h;
		o_ptr->to_d = a_ptr->to_d;
		o_ptr->weight = a_ptr->weight;

		/* Extract pvals and related flags */
		o_ptr->pval = a_ptr->pval1;
		o_ptr->flags_pval1 = a_ptr->flags_pval1;
		o_ptr->pval2 = a_ptr->pval2;
		o_ptr->flags_pval2 = a_ptr->flags_pval2;
		o_ptr->pval3 = a_ptr->pval3;
		o_ptr->flags_pval3 = a_ptr->flags_pval3;

		o_ptr->flags1 = a_ptr->flags1;
		o_ptr->flags2 = a_ptr->flags2;
		o_ptr->flags3 = a_ptr->flags3;

		/* Save cost and activation */
		o_ptr->b_cost = a_ptr->cost;
		o_ptr->activate = a_ptr->activate;

		/* Add random qualities (if needed) */
		apply_random_qualities(o_ptr);

		/* Hack -- artifacts ignore some things */
		o_ptr->flags2 |= (TR2_IGNORE_MASK);

		/* Hack -- extract the "cursed" flag */
		if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

		/* Mega-Hack -- increase the level rating */
		level_rating += 15;
		if (a_ptr->cost > rand_range(30000, 50000L)) level_rating += 5;
		if (a_ptr->cost > rand_range(70000, 100000L)) level_rating += 5;

		/* Cheat -- peek at the item */
		if (cheat_peek) object_mention(o_ptr);
		if (can_precog(100, LEV_REQ_PRECOG + 10))
		{
			/* Highly specific message for truly perceptive characters */
			if ((get_skill(S_PERCEPTION, 0, 100) >= 95) && (one_in_(2)))
				precog_msg(PRECOG_OBJ_WONDROUS);

			else precog_msg(PRECOG_OBJ_GREAT_POWER);
		}

		/* Done */
		return;
	}


	/**** Apply Magic ****/
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Make powerful objects into ego-items */
			if ((ABS(power) > 1) && (can_make_ego))
			{
				(void)make_ego_item(o_ptr, cursed);
			}

			/* Give objects with any power some plusses */
			if (power) add_magic_to_weapon(o_ptr, lev, power);

			break;
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_HELM:
		case TV_CROWN:
		case TV_CLOAK:
		case TV_GLOVES:
		case TV_BOOTS:
		{
			/* Make powerful objects into ego-items */
			if ((ABS(power) > 1) && (can_make_ego))
			{
				(void)make_ego_item(o_ptr, cursed);
			}

			/* Add magic */
			add_magic_to_armor(o_ptr, lev, power);

			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			/* Some objects with no power are cursed (but not in stores) */
			if (!(obj_gen_flags & (OBJ_GEN_STORE)) &&
			    !power && (one_in_(3))) power = -1;

			/* Add magic */
			add_magic_to_ring(o_ptr, lev, power);

			break;
		}

		default:
		{
			/* Add fuel, charges, etc. */
			add_magic_to_others(o_ptr, lev, power);

			/* Light sources can be ego-items */
			if (((o_ptr->tval == TV_LITE) || is_magic_book(o_ptr)) && (ABS(power) > 1) && (can_make_ego))
			{
				make_ego_item(o_ptr, cursed);
			}

			break;
		}
	}


	/* Hack -- analyze ego-items */
	if (o_ptr->ego_item_index)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->ego_item_index];

		/* Hack! -- Only allow one, unless item comes in bunches */
		if ((k_info[o_ptr->k_idx].gen_dice *
		     k_info[o_ptr->k_idx].gen_side <= 1))
		{
			o_ptr->number = 1;
		}

		/* Hack -- acquire "cursed" flag */
		if (e_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

		/* Apply extra bonuses or penalties. */
		if (e_ptr->mod_to_h != 0)
			o_ptr->to_h += ego_item_bonus(e_ptr->mod_to_h);
		if (e_ptr->mod_to_d != 0)
			o_ptr->to_d += ego_item_bonus(e_ptr->mod_to_d);
		if (e_ptr->mod_to_a != 0)
			o_ptr->to_a += ego_item_bonus(e_ptr->mod_to_a);


		/* Hack -- some ego-items are unusually heavy or light */
		if (o_ptr->ego_item_index == EGO_MYSTIC)
			o_ptr->weight = 2 * o_ptr->weight / 3;
		else if (o_ptr->ego_item_index == EGO_BALROG)
			o_ptr->weight += 100;
		else if (o_ptr->ego_item_index == EGO_GREAT_MASS)
			o_ptr->weight = 10 * rand_range(150, 200);


		/* Hack -- Whips of the Balrog get an extra damage die */
		if (o_ptr->ego_item_index == EGO_BALROG)
		{
			o_ptr->dd++;
		}

		/* Determine pvals and related flags */
		for (i = 0; i < 2; i++)
		{
			/* Get the right pval and set of flags */
			s16b e_pval =
				((i == 0) ? e_ptr->max_pval1 : e_ptr->max_pval2);
			u32b e_flags_pval =
				((i == 0) ? e_ptr->flags_pval1 : e_ptr->flags_pval2);

			/* Ego-item has no pval, or it doesn't affect anything */
			if (!e_pval || !e_flags_pval) continue;

			/* Pvals vary */
			if (!is_missile_weapon(o_ptr))
			{
				/* Most ego-items get good pvals at any depth */
				adjust = rand_range(ABS(e_pval / 2), ABS(e_pval));
				if (e_pval < 0) adjust = -(adjust);
			}
			else
			{
				/* Missile weapons cannot be too strong early */
				adjust = m_bonus(ABS(e_pval), lev, MAX_DEPTH);
				if (e_pval < 0) adjust = -(adjust);
			}

			/* Always have some adjustment */
			if (adjust == 0) adjust = (e_pval > 0 ? 1 : -1);

			/* If we already have this attribute, adjust the existing pval */
			if      (o_ptr->flags_pval1 & (e_flags_pval)) o_ptr->pval  += adjust;
			else if (o_ptr->flags_pval2 & (e_flags_pval)) o_ptr->pval2 += adjust;
			else if (o_ptr->flags_pval3 & (e_flags_pval)) o_ptr->pval3 += adjust;

			/* If we don't have this attribute already, we'll have to add it */
			else if (!o_ptr->pval)
			{
				o_ptr->pval = adjust;
				o_ptr->flags_pval1 = e_flags_pval;
			}
			else if (!o_ptr->pval2)
			{
				o_ptr->pval2 = adjust;
				o_ptr->flags_pval2 = e_flags_pval;
			}
			else if (!o_ptr->pval3)
			{
				o_ptr->pval3 = adjust;
				o_ptr->flags_pval3 = e_flags_pval;
			}
		}

		/* Assign activation if present */
		if (e_ptr->activate) o_ptr->activate = e_ptr->activate;

		/* Hack -- apply rating bonus */
		level_rating += e_ptr->rating;

		/* Cheat -- describe the item */
		if (cheat_peek) object_mention(o_ptr);

		if (can_precog(100, LEV_REQ_PRECOG + 10))
			precog_msg(PRECOG_OBJ_GREAT_POWER);

		/* Apply ego-item cost bonus, note worthless items */
		if (e_ptr->cost > 0L) o_ptr->b_cost += e_ptr->cost;
		else                  o_ptr->b_cost = 0L;
	}

	/* Examine all non-artifacts (if a real object) */
	if (o_ptr->k_idx)
	{
		/* Get object kind */
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Add random qualities (if needed) */
		apply_random_qualities(o_ptr);

		/* Hack -- Cursed objects are, by default, clinging */
		if (o_ptr->flags3 & (TR3_LIGHT_CURSE | TR3_HEAVY_CURSE |
		                     TR3_PERMA_CURSE))
		{
			o_ptr->ident |= (IDENT_CURSED);
		}

		/* If total plusses have gone down, non-ego item may be cursed */
		else if ((!o_ptr->ego_item_index) &&
		        ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) <
		         (k_ptr->to_h + k_ptr->to_d + k_ptr->to_a)))
		{
			/* Chance of cursing is low very early on, but increases to 80%. */
			int chance = MIN(80, object_level * 4);

			/* Curse the object */
			if (chance > rand_int(100)) o_ptr->ident |= (IDENT_CURSED);
		}

		/* Objects marked as cursed have (at least) light cursing  */
		if (o_ptr->ident & (IDENT_CURSED))
		{
			o_ptr->flags3 |= (TR3_LIGHT_CURSE);
		}
	}
}


/*
 * Hack -- determine if a template matches the tval given
 */
bool kind_fits_tval(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* We are only looking for non-worthless items with the tval asked for. */
	if (k_ptr->tval == required_tval)
	{
		if (required_tval == TV_JUNK) return (TRUE);
		if (required_tval == TV_SKELETON) return (TRUE);
		if (required_tval == TV_GOLD) return (TRUE);
		if (required_tval == TV_ESSENCE) return (TRUE);
		if (k_ptr->cost > 0L) return (TRUE);
	}
	return (FALSE);
}

/*
 * Hack -- determine if a template is "good".  This concept has been
 * considerably expanded in Oangband. -LM-
 */
static bool kind_is_good(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Reasonably valuable objects of high level are always good */
	if ((k_ptr->cost > 500L) &&
		 (k_ptr->level >= object_level + 5))
	{
		return (TRUE);
	}

	/* Analyze the item type */
	switch (k_ptr->tval)
	{
		/* Armor -- Good unless damaged */
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		{
			if (k_ptr->to_a < 0) return (FALSE);
			return (TRUE);
		}

		/* Weapons -- Good unless damaged.  Diggers are no longer good. */
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			if (k_ptr->to_h < 0) return (FALSE);
			if (k_ptr->to_d < 0) return (FALSE);
			return (TRUE);
		}

		/* Ammo -- Shots/Arrows/Bolts are good. */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			return (TRUE);
		}

		/* Books -- High level books are good */
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_NATURE_BOOK:
		case TV_DARK_BOOK:
		{
			if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return (TRUE);
			return (FALSE);
		}

		/* Chests are always good. */
		case TV_CHEST:
		{
			return (TRUE);
		}

		/* Light sources are good (they are required to be ego-items) */
		case TV_LITE:
		{
			return (TRUE);
		}

		/*  Check cost and level. */
		case TV_RING:
		case TV_AMULET:
		case TV_WAND:
		case TV_STAFF:
		case TV_ROD:
		case TV_POTION:
		case TV_SCROLL:
		{
			if ((k_ptr->cost >= 300L + object_level * 130L) &&
			    ((object_level + 3 < k_ptr->level))) return (TRUE);
			return (FALSE);
		}
	}

	/* Assume not good */
	return (FALSE);
}


/*
 * Attempt to make an object (normal or good/great, or with a specified tval)
 *
 * This routine uses "object_level" for the "generation level".
 *
 * We assume that the given object has been "wiped".
 */
bool make_object(object_type *o_ptr, bool good, bool great, bool exact_kind)
{
	object_kind *k_ptr;

	int odds, base;
	int k_idx;

	int precog = get_skill(S_PERCEPTION, 0, 100);


	/* Odds against making a special artifact */
	if (good) odds = 15;
	else      odds = 1000;

	/* Base level for the object */
	base = (good ? (object_level + 10) : object_level);


	/* Attempt to make a special artifact */
	if ((one_in_(odds)) && (make_artifact_special(o_ptr)))
	{
		/* Process later */
	}

	/* Generate a normal object */
	else
	{
		/* Good objects */
		if (good)
		{
			/* Activate restriction */
			get_obj_num_hook = kind_is_good;

			/* Prepare allocation table */
			get_obj_num_prep();
		}

		/*
		 * Objects with a particular tval.  Only activate if there is a
		 * valid tval to restrict to.
		 */
		if (exact_kind && required_tval)
		{
			/* Activate restriction */
			get_obj_num_hook = kind_fits_tval;

			/* Prepare allocation table */
			get_obj_num_prep();
		}

		/* Get an object index */
		k_idx = get_obj_num(base);

		/*
		 * Clear any special object restrictions, and prepare the standard
		 * object allocation table.
		 */
		if (get_obj_num_hook)
		{
			/* Clear restriction */
			get_obj_num_hook = NULL;

			/* Prepare allocation table */
			get_obj_num_prep();
		}

		/* Handle failure */
		if (!k_idx) return (FALSE);

		/* Prepare the object */
		object_prep(o_ptr, k_idx);
	}

	/* Get the object kind */
	k_ptr = &k_info[o_ptr->k_idx];


	/* Generate multiple items */
	if ((k_ptr->gen_mult_prob) && ((k_ptr->gen_mult_prob >= 100) ||
	    (k_ptr->gen_mult_prob >= randint(100))) &&
	    (k_ptr->gen_dice * k_ptr->gen_side))
	{
		o_ptr->number = damroll(k_ptr->gen_dice, k_ptr->gen_side);

		/* Scrolls, potions, and mushrooms get more abundant slowly */
		if ((o_ptr->number > 1) &&
			 ((k_ptr->tval == TV_POTION) || (k_ptr->tval == TV_SCROLL) ||
		    ((k_ptr->tval == TV_FOOD) && (k_ptr->sval < SV_FOOD_MIN_FOOD))) &&
		    (object_level < k_ptr->level + 10))
		{
			/* Randomize, depending on level (can be zero) */
			o_ptr->number = m_bonus(o_ptr->number, object_level,
				k_ptr->level + 10);
		}
	}

	/* Apply magic */
	apply_magic(o_ptr, object_level, TRUE, good, great);

	/* Always have at least one item */
	if (o_ptr->number < 1) o_ptr->number = 1;


	/* Notice "okay" out-of-depth objects during level generation */
	if (!character_dungeon)
	{
		if (!cursed_p(o_ptr) && !broken_p(o_ptr) &&
			 (k_ptr->level > p_ptr->depth))
		{
			/* Rating increase */
			level_rating += (k_ptr->level - p_ptr->depth);

			/* Cheat -- peek at items */
			if (cheat_peek) object_mention(o_ptr);

			/* Generate precognition messages sometimes */
			if (can_precog(40 + 4 * (k_ptr->level - p_ptr->depth),
				LEV_REQ_PRECOG + 10))
			{
				precog_msg(PRECOG_OBJ_REMARKABLE);
			}
		}

		/* Sometimes generate spurious precognition messages */
		if ((precog >= LEV_REQ_PRECOG + 10) && (precog < 96) &&
			 (one_in_(precog - LEV_REQ_PRECOG)))
		{
			/* Frequency depends on expected goodness of objects */
			if (object_level > rand_int(500))
			{
				/* Generic messages */
				if (one_in_(2))
					precog_msg(PRECOG_OBJ_REMARKABLE);
				else
					precog_msg(PRECOG_OBJ_GREAT_POWER);
			}
		}
	}

	/* Store drop depth */
	o_ptr->drop_depth = p_ptr->depth;

	/* Success */
	return (TRUE);
}


/*
 * Make an item of the required tval, and give it either to the character or
 * the floor.
 */
void make_specific_tval(int tval, int lev, bool in_pack)
{
	object_type *i_ptr;
	object_type forge;

	int tmp_object_level = object_level;

	/* Get local object */
	i_ptr = &forge;

	/* Require given tval, use given level as generation depth */
	object_level = MAX(1, lev);
	required_tval = tval;

	/* Make an object */
	if (make_object(i_ptr, FALSE, FALSE, TRUE))
	{
		/* Item should go in the pack */
		if (in_pack)
		{
			/* Give it to character (if no room, drop it on the floor) */
			give_object(i_ptr, FALSE);
		}

		/* Item should go on on the floor */
		else
		{
			/* Place it on the floor */
			drop_near(i_ptr, 0, p_ptr->py, p_ptr->px, DROP_HERE);
		}
	}

	/* Reset object-creation variables */
	required_tval = 0;
	object_level = tmp_object_level;
}


/*
 * Make a treasure object.
 */
bool make_gold(object_type *o_ptr)
{
	int i;
	int treasure = 0;
	int gold_depth, first_gold_idx;

	/* Make a special treasure */
	if (coin_type >= SV_SPECIAL_GOLD_MIN)
	{
		return (make_special_gold(o_ptr));
	}


	/* Hack -- Creeping Coins only generate "themselves" */
	if (coin_type)
	{
		treasure = lookup_kind(TV_GOLD, coin_type);
	}

	/* No special rules */
	else
	{
		/* Hack -- Start looking at location of copper. */
		first_gold_idx = lookup_kind(TV_GOLD, SV_COPPER);

		/* Normal treasure level is between 1/2 and the full object level. */
		gold_depth = rand_range(object_level / 2, object_level);

		/* Apply "extra" magic. */
		if (one_in_(GREAT_OBJ))
		{
			gold_depth = 1 + (gold_depth * MAX_DEPTH / randint(MAX_DEPTH));
		}

		/* Find the highest-level legal gold object. */
		for (i = first_gold_idx; i < first_gold_idx + SV_GOLD_MAX; i++)
		{
			/* Paranoia -- Skip objects without value. */
			if (!k_info[i].cost) continue;

			/* Stop if we're going too deep. */
			if (k_info[i].level > gold_depth) break;

			/* Otherwise, keep this index. */
			treasure = i;
		}
	}

	/* Report failure.  This should never happen. */
	if (!treasure) return (FALSE);

	/* Prepare a gold object */
	object_prep(o_ptr, treasure);

	/* Treasure can be worth between 1/2 and the full maximal value. */
	o_ptr->pval = rand_range(k_info[treasure].cost / 2 * GOLD_ADJ,
	                         k_info[treasure].cost * GOLD_ADJ);

	/* Success */
	return (TRUE);
}


/*
 * Make a special treasure object.
 */
bool make_special_gold(object_type *o_ptr)
{
	long price;
	int treasure;

	/* Hack -- Giant Gems only generate "themselves" */
	if (coin_type)
	{
		treasure = lookup_kind(TV_GOLD, coin_type);
	}
	else
	{
		int sval = rand_range(SV_SPECIAL_GOLD_MIN, SV_SPECIAL_GOLD_MAX);

		/* Get a random treasure */
		treasure = lookup_kind(TV_GOLD, sval);
	}

	/* Report failure.  This should never happen. */
	if (!treasure) return (FALSE);

	/* Prepare a gold object */
	object_prep(o_ptr, treasure);

	/* Base value */
	price = k_info[treasure].cost;

	/* Special treasures increase in value with depth */
	price += m_bonus(k_info[treasure].cost * 5, object_level, MAX_DEPTH);

    /* Adjust price for birth options */
    if (birth_stores_only_sell) price *= GOLD_ADJ;

	/* Neaten up values */
	price -= price % 500;

	/* Enforce maximum */
	if (price > 30000L) price = 30000L;

	/* Save the value */
	o_ptr->pval = (s16b)price;

	/* Success */
	return (TRUE);
}


/*
 * Determine the odds of an object breaking when thrown at a monster.
 */
int breakage_chance(object_type *o_ptr)
{
	/* Artifacts never break */
	if (o_ptr->artifact_index) return (0);

	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break (potions in particular are required to break) */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_PARCHMENT:
		case TV_SKELETON:
		case TV_FOOD:
		{
			return (40);
		}

		/* Frequently break */
		case TV_ARROW:
		{
			if (o_ptr->ego_item_index) return (20 - o_ptr->ac);
			return (30 - (3 * o_ptr->ac / 2));
		}

		/* Sometimes break */
		case TV_SHOT:
		case TV_BOLT:
		{
			if (o_ptr->ego_item_index) return (15 - o_ptr->ac);
			return (20 - o_ptr->ac);
		}

		case TV_JUNK:
		{
			if (o_ptr->sval == SV_BOULDER) return (2);
			else return (40);
		}

		/* Seldom break */
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_WAND:
		case TV_STAFF:
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_SPIKE:
		{
			u32b f1, f2, f3;

			/* Get object attributes */
			object_flags(o_ptr, &f1, &f2, &f3);

			/* Throwing weapons are designed not to break. */
			if (f1 & (TR1_PERFECT_BALANCE))
			{
				return (0);
			}
			else if (f1 & (TR1_THROWING))
			{
				return (rand_int(2)); /* Break 1 time in 200 */
			}
			else
			{
				return (10 - o_ptr->ac);
			}
		}

		/* Rarely break */
		case TV_AMULET:
		case TV_CHEST:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_NATURE_BOOK:
		case TV_DARK_BOOK:
		{
			return (5 - (o_ptr->ac / 2));
		}

		/* Never break */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_RING:
		case TV_ROD:
		case TV_COMPONENT:
		{
			return (0);
		}
	}

	/* Rarely break */
	return (5 - (o_ptr->ac / 2));
}


/*
 * Let the floor carry an object
 */
s16b floor_carry(int y, int x, object_type *j_ptr)
{
	s16b o_idx;

	s16b this_o_idx, next_o_idx = 0;


	/* Scan objects in that grid for combination */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Check for combination */
		if (object_similar(o_ptr, j_ptr))
		{
			/* Combine the items */
			object_absorb(o_ptr, j_ptr);

			/* Result */
			return (this_o_idx);
		}
	}

	/* Make an object */
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;

		/* Get object */
		o_ptr = &o_list[o_idx];

		/* Structure Copy */
		object_copy(o_ptr, j_ptr);

		/* Location */
		o_ptr->iy = y;
		o_ptr->ix = x;

		/* Forget monster or trap */
		o_ptr->held_m_idx = 0;

		/* Link the object to the pile */
		o_ptr->next_o_idx = cave_o_idx[y][x];

		/* Link the floor to the object */
		cave_o_idx[y][x] = o_idx;

		/* Notice */
		note_spot(y, x);

		/* Redraw -- if main screen is active */
		if (!main_screen_inactive)
		{
			lite_spot(y, x);

			/* Update nearby objects list */
			p_ptr->window |= (PW_O_LIST);
		}
	}

	/* Result */
	return (o_idx);
}


/*
 * Let an object fall to the ground at or near a location.
 *
 * The initial location is assumed to be "in_bounds_fully()".
 *
 * This function takes a parameter "chance".  This is the percentage
 * chance that the item will break instead of drop.  Negative values
 * count as zero.
 *
 * Allow options to suppress messages, try hard to drop at exact location,
 * and/or handle objects deliberately dropped by the character.
 *
 * We check several locations to see if we can find a location at which
 * the object can combine, stack, or be placed.  Artifacts will try very
 * hard to be placed, including "teleporting" to a useful grid if needed.
 */
void drop_near(object_type *j_ptr, int chance, int y, int x, byte flags)
{
	int i, k, d, s, objs;

	int bs, bn;
	int by, bx;
	int ty, tx;

	object_type *o_ptr;

	char o_name[DESC_LEN];

	bool flag = FALSE;
	bool msg = !(flags & (DROP_NO_MSG));
	bool here = (flags & (DROP_HERE));
	bool char_drop = (flags & (DROP_CHAR_DROP));
	bool plural = FALSE;


	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Describe object (briefly) */
	object_desc(o_name, sizeof(o_name), j_ptr, FALSE, 0);


	/* Handle normal breakage */
	if ((!artifact_p(j_ptr)) && (chance) && (rand_int(100) < chance))
	{
		/* Potions shatter */
		if (j_ptr->tval == TV_POTION)
		{
			/* Message */
			if (msg) msg_format("The %s shatter%s!", o_name, (plural ? "" : "s"));

			/* Smash the potion -- do not learn anything  XXX XXX */
			(void)potion_smash_effect(-1, y, x, j_ptr);
		}

		/* Some objects are "shattered" */
		else if ((j_ptr->tval == TV_BOTTLE) || (j_ptr->tval == TV_FLASK))
		{
			if (msg) msg_format("The %s shatter%s.", o_name, (plural ? "" : "s"));
		}

		/* Some objects are "ruined" */
		else if ((j_ptr->tval == TV_SCROLL) || (j_ptr->tval == TV_CHEST) ||
		         (is_any_armor(j_ptr)) || (j_ptr->tval == TV_FOOD) ||
		         (j_ptr->tval == TV_PARCHMENT) ||
		         (j_ptr->tval == TV_MAGIC_BOOK) ||
		         (j_ptr->tval == TV_PRAYER_BOOK) ||
		         (j_ptr->tval == TV_NATURE_BOOK) ||
		         (j_ptr->tval == TV_DARK_BOOK))
		{
			if (msg) msg_format("The %s %s ruined.", o_name, (plural ? "are" : "is"));
		}

		/* Other objects "break" */
		else
		{
			/* Message */
			if (msg) msg_format("The %s break%s.", o_name, (plural ? "" : "s"));
		}

		/* Return */
		return;
	}


	/* Score */
	bs = -1;

	/* Picker */
	bn = 0;

	/* Default */
	by = y;
	bx = x;

	/* Scan local grids (up to radius 3) */
	for (d = 0, i = 0; i < grids_in_radius[3]; i++)
	{
		bool comb = FALSE;

		ty = y + nearby_grids_y[i];
		tx = x + nearby_grids_x[i];

		/* Adjust distance */
		if (i >= grids_in_radius[d]) d++;

		/* Skip illegal grids */
		if (!in_bounds_fully(ty, tx)) continue;

		/* Require space capable of holding the object */
		if (!cave_allow_object_bold(ty, tx)) continue;

		/* Check line of sight (always true for start & adjacent grids) */
		if (!los(y, x, ty, tx)) continue;

		/* No objects, and no observable objects */
		objs = k = 0;

		/* Scan objects in that grid */
		for (o_ptr = get_first_object(ty, tx); o_ptr;
			  o_ptr = get_next_object(o_ptr))
		{
			/* Count total objects */
			objs++;

			/* Hack -- skip unseen objects in view */
			if ((!o_ptr->marked) && (player_can_see_or_infra_bold(ty, tx))) continue;

			/* Check for possible combination */
			if (object_similar(o_ptr, j_ptr)) comb = TRUE;

			/* Count observable objects */
			k++;
		}

		/* Add new object */
		if (!comb)
		{
			objs++;
			k++;
		}

		/* Do not pile up more than 23 objects */
		if (objs > MAX_FLOOR_STACK) continue;

		/*
		 * Calculate goodness of location, given distance from source of
		 * drop and number of objects.
		 */

		/* We want to drop the object right here. */
		if ((here) && (d == 0) && (k < 20))
		{
			s = 10000;
		}

		/* Encourage objects to combine */
		else if (comb)
		{
			s = 1000 - (d + 5);
		}

		/* Normally try not to pile up */
		else
		{
			s = 1000 - (d + k * 5);
		}

		/* Try to avoid visible traps */
		if (cave_visible_trap(ty, tx)) s -= 250;

		/* Try to avoid areas other than floor */
		if (!cave_floor_bold(ty, tx)) s -= 150;

		/* Skip bad values */
		if (s < bs) continue;

		/* New best value */
		if (s > bs) bn = 0;

		/* Apply the randomizer to equivalent values */
		if ((++bn >= 2) && (!one_in_(bn))) continue;

		/* Keep score */
		bs = s;

		/* Track it */
		by = ty;
		bx = tx;

		/* Accept this grid */
		flag = TRUE;
	}


	/* We are determined to find a grid for this object */
	for (i = 0; !flag; i++)
	{
		/* Bounce around */
		if (i <= 400)
		{
			ty = rand_spread(by, 1 + i / 20);
			tx = rand_spread(bx, 1 + i / 20);
		}

		/* Random locations */
		else
		{
			ty = rand_int(dungeon_hgt);
			tx = rand_int(dungeon_wid);
		}

		/* Skip illegal grids */
		if (!in_bounds_fully(ty, tx)) continue;

		/* Require space capable of holding the object */
		if (!cave_allow_object_bold(ty, tx)) continue;

		/* Count objects in that grid */
		for (objs = 0, o_ptr = get_first_object(ty, tx); o_ptr;
			  o_ptr = get_next_object(o_ptr)) objs++;

		/* Do not pile up more than 23 objects */
		if (objs > MAX_FLOOR_STACK) continue;

		/* Bounce to that location */
		by = ty;
		bx = tx;

		/* Accept this grid */
		flag = TRUE;
	}

	/*
	 * Object will (or at least should) go underneath the character, or
	 * it was dropped by the character.
	 */
	if ((cave_m_idx[by][bx] < 0) || (char_drop))
	{
		/* Note this object (will be "seen" even if blind) */
		j_ptr->marked = TRUE;
	}

	/* Otherwise, unmark the object */
	else j_ptr->marked = FALSE;


	/* Give it to the floor */
	if (!floor_carry(by, bx, j_ptr))
	{
		/* Debug */
		if (p_ptr->wizard)
		{
			msg_format("Breakage (grid %d:%d cannot hold an object (2)).", by, bx);
		}
		else
		{
			/* Message XXX XXX */
			if (player_can_see_or_infra_bold(by, bx))
			{
				msg_format("The %s disappear%s.", o_name, (plural ? "" : "s"));
			}
		}

		/* Hack -- Preserve artifacts */
		a_info[j_ptr->artifact_index].cur_num = 0;

		/* Failure */
		return;
	}

	/* Sound (only if seen) */
	if (player_can_see_or_infra_bold(by, bx)) sound(MSG_DROP);

	/* Update object list window */
	p_ptr->window |= (PW_O_LIST);

	/* An object not dropped by the character is now underneath him */
	if ((cave_m_idx[by][bx] < 0) && (!char_drop))
		msg_print("You feel something roll beneath your feet.");
}


/*
 * Scatter some special ("good" or "great") objects near the player
 *
 * It no longer matters at what dungeon level you read this scroll; only your maximum depth matters.
 */
void acquirement(int y1, int x1, int num, bool great)
{
	object_type *i_ptr;
	object_type forge;
	int prev_object_level = object_level;

	/* Object level is the furthest the character has even gone */
	object_level = p_ptr->max_depth;

	/* Acquirement */
	while (num--)
	{
		/* Get local object */
		i_ptr = &forge;

		/* Make a good (or great) object (if possible) */
		if (make_object(i_ptr, TRUE, great, FALSE))
		{
			/* Drop the object */
			drop_near(i_ptr, 0, y1, x1, 0x00);
		}
	}

	/* Restore standard object level */
	object_level = prev_object_level;
}


/*
 * Attempt to place an object (normal or good/great) at the given location.
 */
void place_object(int y, int x, bool good, bool great, bool exact_kind)
{
	object_type *i_ptr;
	object_type forge;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require a grid capable of holding the object */
	if (!cave_allow_object_bold(y, x)) return;

	/* Get local object */
	i_ptr = &forge;

	/* Make an object (if possible) */
	if (make_object(i_ptr, good, great, exact_kind))
	{
		/* Give it to the floor */
		if (!floor_carry(y, x, i_ptr))
		{
			/* Hack -- Preserve artifacts */
			a_info[i_ptr->artifact_index].cur_num = 0;
		}
	}
}


/*
 * Places a treasure (Gold or Gems) at given location
 */
void place_gold(int y, int x)
{
	object_type *i_ptr;
	object_type forge;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require space to hold the object */
	if (!cave_allow_object_bold(y, x)) return;

	/* Get local object */
	i_ptr = &forge;

	/* Make some gold */
	if (make_gold(i_ptr))
	{
		/* Give it to the floor */
		(void)floor_carry(y, x, i_ptr);
	}
}

/*
 * Make a boulder, place it at given location
 */
void make_boulder(int y, int x, int level)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Make a boulder */
	object_prep(i_ptr, lookup_kind(TV_JUNK, SV_BOULDER));

	/* Apply magic */
	apply_magic(i_ptr, level, FALSE, FALSE, FALSE);

	/* Give it to the floor */
	(void)floor_carry(y, x, i_ptr);
}


/*
 * Make some food, place it at given location
 */
void make_food(int y, int x)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Restrict to food/mushrooms */
	required_tval = TV_FOOD;

	/* Keep trying */
	while (TRUE)
	{
		/* Make an object (if possible) */
		if (make_object(i_ptr, FALSE, FALSE, TRUE))
		{
			/* Require edible food */
			if (i_ptr->sval >= SV_FOOD_MIN_FOOD)
			{
				/* Give it to the floor */
				(void)floor_carry(y, x, i_ptr);

				/* All done */
				break;
			}
		}
	}

	/* Clear restriction */
	required_tval = 0;
}


/*
 * Describe the charges on an item in the inventory.
 */
void inven_item_charges(int item)
{
	object_type *o_ptr = &inventory[item];

	/* Require staff/wand */
	if ((o_ptr->tval != TV_STAFF) && (o_ptr->tval != TV_WAND)) return;

	/* Require known item */
	if (!object_known_p(o_ptr)) return;

	/* Print a message */
	msg_format("You have %d charge%s remaining.", o_ptr->pval,
		(o_ptr->pval != 1) ? "s" : "");
}




/*
 * Describe an item being used from the equipment, inventory, or floor.
 */
void use_item_describe(int item, int mode)
{
	object_type *o_ptr = &inventory[item];

	char buf[DESC_LEN];
	char o_name[DESC_LEN];

	cptr p;

	bool artifact = FALSE;


	/* Get the object */
	if (item >= 0) o_ptr = &inventory[item];
	else           o_ptr = &o_list[(-item)];

	/* Describe as singular */
	object_desc_plural = -1;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 2);

	/* Note artifact, or inscribed item */
	if (strstr(o_name, "the ")) artifact = TRUE;


	/* Describe what is being done (return if mode not recognized) */
	if (mode == USE_ITEM_DESC_FIRE) p = "fire";
	else if (mode == USE_ITEM_DESC_THROW)
	{
		/* We "cast a throwing knife" and "throw a javelin" */
		if (strstr("throwing", o_name)) p = "cast";
		else                            p = "throw";
	}
	else return;

	/* Build a string */
	(void)my_strcpy(buf, format("You %s %s", p, o_name), sizeof(buf));

	/* Item indicator */
	if (item >= 0)
		(void)my_strcat(buf, format(" (%c)", index_to_label(item)), sizeof(buf));
	else
		(void)my_strcat(buf, format(" (-)", o_name), sizeof(buf));

	/* If not a solitary artifact, indicate number remaining */
	if ((!artifact) || (o_ptr->number))
	{
		(void)my_strcat(buf, (o_ptr->number > 0) ?
			format(" (%d left)", o_ptr->number) : " (none left)", sizeof(buf));
	}

	/* Message */
	msg_format("%s.", buf);
}



/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
	object_type *o_ptr = &inventory[item];

	char o_name[DESC_LEN];

	/* Hack -- No messages when dead */
	if (p_ptr->is_dead) return;

	/* Special case -- solitary artifacts */
	if (artifact_p(o_ptr) && object_known_p(o_ptr) &&
	    (a_info[o_ptr->artifact_index].max_num <= 1) &&
	    (o_ptr->number == 0))
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

		/* Print a message */
		msg_format("You no longer have the %s (%c).", o_name, index_to_label(item));
	}

	/* Usual case */
	else
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Print a message */
		msg_format("You have %s (%c).", o_name, index_to_label(item));
	}
}


/*
 * Increase the "number" of an item in the inventory
 */
void inven_item_increase(int item, int num)
{
	object_type *o_ptr = &inventory[item];

	/* Apply */
	num += o_ptr->number;

	/* Bounds check */
	if (num > 255) num = 255;
	else if (num < 0) num = 0;

	/* Un-apply */
	num -= o_ptr->number;

	/* Change the number and weight */
	if (num)
	{
		/* Add the number */
		o_ptr->number += num;

		/* Add the weight */
		p_ptr->total_weight += (num * o_ptr->weight);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana XXX */
		p_ptr->update |= (PU_MANA);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}

/*
 * Remove one of an item from the inventory
 */
void inven_item_decrease(int item)
{
	inven_item_increase(item, -1);
	inven_item_describe(item);
	inven_item_optimize(item);
}

/*
 * Erase an inventory slot if it has no more items
 */
void inven_item_optimize(int item)
{
	object_type *o_ptr = &inventory[item];

	/* Only optimize real items */
	if (!o_ptr->k_idx) return;

	/* Only optimize empty items */
	if (o_ptr->number) return;

	/* The item is in the pack */
	if (item < INVEN_WIELD)
	{
		int i;

		/* One less item */
		p_ptr->inven_cnt--;

		/* Slide everything down */
		for (i = item; i < INVEN_PACK; i++)
		{
			/* Hack -- slide object */
			COPY(&inventory[i], &inventory[i + 1], object_type);
		}

		/* Hack -- wipe hole */
		(void)WIPE(&inventory[i], object_type);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* The item is being wielded */
	else
	{
		/* One less item */
		p_ptr->equip_cnt--;

		/* Erase the empty slot */
		object_wipe(&inventory[item]);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate torch */
		p_ptr->update |= (PU_TORCH);

		/* Recalculate mana XXX */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

		/* Equippy chars */
		p_ptr->redraw |= (PR_EQUIPPY);
	}
}

/*
 * Describe the charges on an item on the floor.
 */
void floor_item_charges(int item)
{
	object_type *o_ptr = &o_list[item];

	/* Require staff/wand */
	if ((o_ptr->tval != TV_STAFF) && (o_ptr->tval != TV_WAND)) return;

	/* Require known item */
	if (!object_known_p(o_ptr)) return;

	/* Message */
	msg_format("There are %d charge%s remaining.", o_ptr->pval,
		(o_ptr->pval != 1) ? "s" : "");
}

/*
 * Describe an item on the floor.
 */
void floor_item_describe(int item)
{
	object_type *o_ptr = &o_list[item];

	char o_name[DESC_LEN];

	if (!p_ptr->blind)
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Print a message */
		msg_format("You see %s.", o_name);
	}
	else
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

		/* Print a message */
		msg_format("You feel %s.", o_name);
	}
}

/*
 * Increase the "number" of an item on the floor
 */
void floor_item_increase(int item, int num)
{
	object_type *o_ptr = &o_list[item];

	/* Apply */
	num += o_ptr->number;

	/* Bounds check */
	if (num > 255) num = 255;
	else if (num < 0) num = 0;

	/* Un-apply */
	num -= o_ptr->number;

	/* Change the number */
	o_ptr->number += num;

	/* Update object list window */
	p_ptr->window |= (PW_O_LIST);
}

/*
 * Optimize an item on the floor (destroy "empty" items)
 */
void floor_item_optimize(int item)
{
	object_type *o_ptr = &o_list[item];

	/* Paranoia -- be sure it exists */
	if (!o_ptr->k_idx) return;

	/* Only optimize empty items */
	if (o_ptr->number) return;

	/* Delete the object */
	delete_object_idx(item);

	/* Update object list window */
	p_ptr->window |= (PW_O_LIST);
}



/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(const object_type *o_ptr)
{
	int j;

	/* Empty slot? */
	if (p_ptr->inven_cnt < INVEN_PACK - p_ptr->pack_size_reduce) return (TRUE);

	/* Similar slot? */
	for (j = 0; j < INVEN_PACK - p_ptr->pack_size_reduce; j++)
	{
		object_type *j_ptr = &inventory[j];

		/* Skip non-objects */
		if (!j_ptr->k_idx) continue;

		/* Check if the two items can be combined */
		if (object_similar(j_ptr, o_ptr)) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}

/*
 * Check if we have space for an item in the quiver without overflow
 */
bool quiver_carry_okay(const object_type *o_ptr)
{
    int ammo_num, added_ammo_num, attempted_quiver_slots;

	/* Paranoia */
	if (!o_ptr) return (FALSE);

	/* Must be suitable for the quiver */
	if (!is_missile(o_ptr) && !(o_ptr->flags1 & TR1_THROWING)) return (FALSE);

	ammo_num = quiver_count();

	/* Get the new item's quiver size */
	added_ammo_num = quiver_count_item(o_ptr, o_ptr->number);

	/* How many quiver slots would be needed */
	attempted_quiver_slots = ((ammo_num + added_ammo_num + 98) / 99);

	/* Is there room, given normal inventory? */
	if (attempted_quiver_slots + p_ptr->inven_cnt > INVEN_PACK)
	{
		return (FALSE);
	}

    return (TRUE);
}

/*
 * Add an item to the player's inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", else,
 * the item will be placed into the "proper" location in the inventory.
 *
 * This function can be used to "over-fill" the player's pack, but only
 * once, and such an action must trigger the "overflow" code immediately.
 * Note that when the pack is being "over-filled", the new item must be
 * placed into the "overflow" slot, and the "overflow" must take place
 * before the pack is reordered, but (optionally) after the pack is
 * combined.  This may be tricky.  See "dungeon.c" for info.
 *
 * It is crucial to use the correct "overflow" slot.  XXX XXX
 *
 * Note that this code must remove any location/stack information
 * from the object once it is placed into the inventory.
 */
s16b inven_carry(object_type *o_ptr)
{
	int i, j;
	int n = -1;

	object_type *j_ptr;


	/* Paranoia -- refuse to place a blank object */
	if (!o_ptr->k_idx) return (-1);

	/* Check for combining */
	for (j = 0; j < INVEN_PACK; j++)
	{
		j_ptr = &inventory[j];

		/* Skip non-objects */
		if (!j_ptr->k_idx) continue;

		/* Hack -- track last item */
		n = j;

		/* Check if the two items can be combined */
		if (object_similar(j_ptr, o_ptr))
		{
			/* Combine the items */
			object_absorb(j_ptr, o_ptr);

			/* Increase the weight */
			p_ptr->total_weight += (o_ptr->number * o_ptr->weight);

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);

			/* Success */
			return (j);
		}
	}

	/* Paranoia */
	if (p_ptr->inven_cnt > INVEN_PACK) return (-1);


	/* Find an empty slot (use the "overflow" slot, but only temporarily) */
	for (j = 0; j <= INVEN_PACK; j++)
	{
		/* Use it if found */
		if (!inventory[j].k_idx) break;
	}

	/* Copy the item */
	object_copy(&inventory[j], o_ptr);

	/* Get new object */
	j_ptr = &inventory[j];

	/* Forget stack */
	j_ptr->next_o_idx = 0;

	/* Forget monster or trap */
	j_ptr->held_m_idx = 0;

	/* Forget location */
	j_ptr->iy = j_ptr->ix = 0;

	/* No longer marked */
	j_ptr->marked = FALSE;

	/* Increase the weight */
	p_ptr->total_weight += (j_ptr->number * j_ptr->weight);

	/* Count the items */
	p_ptr->inven_cnt++;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Reorder pack immediately, track change in inventory slot */
	i = reorder_pack(j, -1, FALSE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Return the slot */
	return (i);
}


/*
 * Steal an object, and give it to the character.  If he doesn't have space
 * in his backpack, drop the object on the floor nearby.
 *
 * This is duplicate code, kept separate to allow for special messages.
 */
void steal_object(object_type *o_ptr)
{
	char o_name[DESC_LEN];


	/* Grab the gold */
	if (o_ptr->tval == TV_GOLD)
	{
		/* Message -- regular gold and gems */
		if (o_ptr->sval < SV_SPECIAL_GOLD_MIN)
		{
			/* Pilferage versus outright theft */
			if (o_ptr->pval < p_ptr->depth * 5)
				msg_format("You pilfer %d gold.", o_ptr->pval);
			else
				msg_format("You steal %d gold.", o_ptr->pval);
		}

		/* Message -- special treasures */
		else
		{
			/* Describe the object */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			msg_format("You purloin %s worth %d gold!",
				o_name, o_ptr->pval);
		}

		/* Collect the gold */
		p_ptr->au += o_ptr->pval;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}


	/* (Attempt to) carry a non-gold item */
	else if ((o_ptr->weight <= get_skill(S_BURGLARY, 50, 400)) &&
	         (inven_carry_okay(o_ptr)))
	{
		int slot;

		/* Describe the object */
		/* Do this before you move to slot to retain quantity.
		 * This might do something funny with a renaming item,
		 * like a blanket of elemental destruction, but that's
		 * just something that I'll have to live with. -JM
		 */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		slot = inven_carry(o_ptr);

		/* Get the item again */
		o_ptr = &inventory[slot];

		/* Message */
		msg_format("You burgle %s (%c)%c", o_name, index_to_label(slot),
			(o_ptr->tval == TV_CHEST ? '!' : '.'));

		/* Sense the object */
		sense_object(o_ptr, -3, FALSE, FALSE);
	}

	/* Item cannot be carried */
	else
	{
		/* Describe the object (again, first -JM) */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Drop it underneath the character */
		drop_near(o_ptr, 0, p_ptr->py, p_ptr->px, DROP_HERE);

		/* Message */
		msg_format("You snatch away %s (on the floor).", o_name);
	}
}

/*
 * Give a newly-created object to the character.  If he doesn't have space
 * in his backpack, drop the object on the floor nearby.
 */
void give_object(object_type *o_ptr, bool allow_equip)
{
	char o_name[DESC_LEN];

	/* For later use */
	(void)allow_equip;

	/* Carry the item */
	if (inven_carry_okay(o_ptr))
	{
		int slot = inven_carry(o_ptr);

		/* Get the item again */
		o_ptr = &inventory[slot];

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Message */
		msg_format("You have %s (%c).", o_name, index_to_label(slot));

		/* Sense the object */
		sense_object(o_ptr, -3, FALSE, FALSE);
	}
	else
	{
		/* Can't carry - drop it underneath the character */
		drop_near(o_ptr, 0, p_ptr->py, p_ptr->px, DROP_HERE);
	}
}


/*
 * Take off (some of) a non-cursed equipment item
 *
 * Note that only one item at a time can be wielded per slot.
 *
 * Note that taking off an item when "full" may cause that item
 * to fall to the ground (later).
 *
 * Return the inventory slot into which the item is placed.
 */
s16b inven_takeoff(int item, int amt)
{
	int slot;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr act;
	cptr act2 = "";
	cptr msg = "";

	char o_name[DESC_LEN];


	/* Get the item to take off */
	o_ptr = &inventory[item];

	/* Paranoia */
	if (amt <= 0) return (-1);

	/* Verify */
	if (amt > o_ptr->number) amt = o_ptr->number;

	/* Check to see if a Set Item is being removed */
	if (o_ptr->artifact_index)
	{
		/* Get this artifact */
		artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

		/* If it is part of a set, cancel the set bonuses */
		if (check_set(a_ptr->set_index)) msg = remove_set(a_ptr->set_index);
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Took off weapon */
	if (is_melee_weapon(i_ptr) && item < INVEN_Q1)
	{
		act = "You were wielding";

		/*
		 * Hack -- If the player has two weapons, and takes off the
		 * primary, then we want to swap the secondary into the primary
		 * slot. This is handled by reorder_pack().
		 */
		if ((p_ptr->twoweap) && (item == INVEN_WIELD))
		{
			p_ptr->notice |= (PN_REORDER);
		}
	}

	/* Took off bow */
	else if (item == INVEN_BOW)
	{
		act = "You were holding";
	}

	/* Took off a light source */
	else if (item == INVEN_LITE)
	{
		act = "You were holding";
	}

	/* Removed ammo from the quiver slots */
	else if ((item >= INVEN_Q1) && (item <= INVEN_Q0))
	{
		act = "You removed";
		act2 = " from your quiver";

		/* Remember that the item is no longer quivered */
		i_ptr->quivered = FALSE;
	}

	/* Took off something */
	else
	{
		act = "You were wearing";
	}

	/* Modify, Optimize */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

	/* Carry the object */
	slot = inven_carry(i_ptr);

	/* Message */
	message_format(MSG_WIELD, 0, "%s %s (%c)%s.", act, o_name, index_to_label(slot), act2);

	/* Special broken set message */
	if (strlen(msg)) msg_format("%s", msg);

	/* Redraw equippy chars */
	p_ptr->redraw |= (PR_EQUIPPY);

	/* Return slot */
	return (slot);
}


/*
 * Drop (some of) a non-cursed inventory/equipment item
 */
void inven_drop(int item, int amt)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[DESC_LEN];


	/* Get original object */
	o_ptr = &inventory[item];

	/* Error check */
	if (amt <= 0) return;

	/* Not too many */
	if (amt > o_ptr->number) amt = o_ptr->number;


	/* Take off equipment */
	if (item >= INVEN_WIELD)
	{
		/* Take off first */
		item = inven_takeoff(item, amt);

		/* Get original object */
		o_ptr = &inventory[item];

		/* Redraw equippy chars */
		p_ptr->redraw |= (PR_EQUIPPY);
	}


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

	/* Distribute charges of wands or rods */
	distribute_charges(o_ptr, i_ptr, amt);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Describe local object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Message */
	msg_format("You drop %s (%c).", o_name, index_to_label(item));

	/* Drop it near the player */
	drop_near(i_ptr, 0, py, px, DROP_HERE | DROP_CHAR_DROP);

	/* Modify, Describe, Optimize */
	inven_item_increase(item, -amt);
	inven_item_describe(item);
	inven_item_optimize(item);
}


/*
 * Combine items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void combine_pack(void)
{
	int i, j, k;

	object_type *o_ptr;
	object_type *j_ptr;

	bool flag = FALSE;


	/* Combine the pack (backwards) */
	for (i = INVEN_PACK; i > 0; i--)
	{
		/* Get the item */
		o_ptr = &inventory[i];

		/* Skip empty items */
		if (!o_ptr->k_idx) continue;

		/* Scan the items above that item */
		for (j = 0; j < i; j++)
		{
			/* Get the item */
			j_ptr = &inventory[j];

			/* Skip empty items */
			if (!j_ptr->k_idx) continue;

			/* Can we drop "o_ptr" onto "j_ptr"? */
			if (object_similar(j_ptr, o_ptr))
			{
				/* Take note */
				flag = TRUE;

				/* Add together the item counts */
				object_absorb(j_ptr, o_ptr);

				/* One object is gone */
				p_ptr->inven_cnt--;

				/* Slide everything down */
				for (k = i; k < INVEN_PACK; k++)
				{
					/* Hack -- slide object */
					COPY(&inventory[k], &inventory[k + 1], object_type);
				}

				/* Hack -- wipe empty slot */
				object_wipe(&inventory[k]);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN);

				/* Done */
				break;
			}
		}
	}

	/* Message */
	if (flag) msg_print("You combine some items in your pack.");
}

/*
 * Reorder items in the pack or in a store (but not in the quiver yet).
 *
 * Accept the current slot for an object that needs tracking and return its
 * new position.
 */
int reorder_pack(int slot, int store_num, bool verbose)
{
	int i, j, k;
	int max;

	s32b o_value;
	s32b j_value;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	store_type *st_ptr = NULL;

	bool flag = FALSE;


	/* If store_num is non-negative, access the given store */
	if (store_num >= 0)
	{
		/* Get the store */
		st_ptr = &store[store_num];

		/* Note maximum stock size */
		max = st_ptr->stock_size;
	}

	/* Otherwise, use the pack */
	else
	{
		/* Note available pack size */
		max = INVEN_PACK - p_ptr->pack_size_reduce;
	}


	/* Re-order the inventory (forwards) */
	for (i = 0; i < max; i++)
	{
		/* Get the item */
		if (!st_ptr) o_ptr = &inventory[i];
		else         o_ptr = &st_ptr->stock[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Get the "value" of the item */
		o_value = object_value(o_ptr);

		/* Scan every occupied slot */
		for (j = 0; j < max; j++)
		{
			/* Get the item already there */
			if (!st_ptr) j_ptr = &inventory[j];
			else         j_ptr = &st_ptr->stock[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;

			/* Hack -- readable books always come first */
			if ((o_ptr->tval == mp_ptr->spell_book) &&
			    (j_ptr->tval != mp_ptr->spell_book)) break;
			if ((j_ptr->tval == mp_ptr->spell_book) &&
			    (o_ptr->tval != mp_ptr->spell_book)) continue;

			/* In stores, known artifacts and ego-items come next */
			if ((st_ptr) && (store_num != STORE_HOME) &&
				object_known_p(o_ptr) && object_known_p(j_ptr))
			{
				if ((artifact_p(o_ptr)) && (!artifact_p(j_ptr))) break;
				if ((artifact_p(j_ptr)) && (!artifact_p(o_ptr))) continue;
				if ((ego_item_p(o_ptr)) && (!ego_item_p(j_ptr))) break;
				if ((ego_item_p(j_ptr)) && (!ego_item_p(o_ptr))) continue;
			}

			/* Objects sort by decreasing type */
			if (o_ptr->tval > j_ptr->tval) break;
			if (o_ptr->tval < j_ptr->tval) continue;

			/* Non-aware objects go below aware ones, except in stores */
			if (object_aware_p(o_ptr) && !object_aware_p(j_ptr) && !st_ptr) break;
			if (!object_aware_p(o_ptr) && object_aware_p(j_ptr) && !st_ptr) continue;

			/* New non-aware objects go below old, except in stores */
			if (!object_aware_p(o_ptr) && !object_aware_p(j_ptr) && !st_ptr) continue;

			/* If aware, objects sort by increasing sval */
			if (o_ptr->sval < j_ptr->sval) break;
			if (o_ptr->sval > j_ptr->sval) continue;

			/* Unidentified objects go below identified ones */
			if (object_known_p(o_ptr) && !object_known_p(j_ptr)) break;
			if (!object_known_p(o_ptr) && object_known_p(j_ptr)) continue;

			/* Determine the "value" of the existing item */
			j_value = object_value(j_ptr);

			/* Objects sort by decreasing value */
			if (o_value > j_value) break;
			if (o_value < j_value) continue;

			/* Boulders sort by decreasing weight */
			if ((o_ptr->tval == TV_JUNK) && (o_ptr->sval == SV_BOULDER))
			{
				if (o_ptr->weight > j_ptr->weight) break;
				if (o_ptr->weight < j_ptr->weight) continue;
			}

			/* Light sources sort by decreasing fuel */
			if (o_ptr->tval == TV_LITE)
			{
				if (o_ptr->pval > j_ptr->pval) break;
				if (o_ptr->pval < j_ptr->pval) continue;
			}
		}

		/* Never move down */
		if (j >= i) continue;

		/* Take note */
		flag = TRUE;

		/* Handle player inventory */
		if (!st_ptr)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Save a copy of the moving item */
			object_copy(i_ptr, &inventory[i]);

			/* Slide the objects */
			for (k = i; k > j; k--)
			{
				/* Slide the item */
				object_copy(&inventory[k], &inventory[k - 1]);
			}

			/* Insert the moving item */
			object_copy(&inventory[j], i_ptr);
		}

		/* Handle store stock */
		else
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Save a copy of the moving item */
			object_copy(i_ptr, &st_ptr->stock[i]);

			/* Slide the objects */
			for (k = i; k > j; k--)
			{
				/* Slide the item */
				object_copy(&st_ptr->stock[k], &st_ptr->stock[k - 1]);
			}

			/* Insert the moving item */
			object_copy(&st_ptr->stock[j], i_ptr);
		}

		/* Track slot change */
		if (i == slot) slot = j;
	}

	/* If a store, we're done at this point */
	if (st_ptr) return (slot);


	/* Notice the change */
	if (flag)
	{
		if (verbose) msg_print("You reorder some items in your pack.");
		p_ptr->window |= (PW_INVEN);
	}

	/*
	 * Hack -- If the player has two weapons, and loses the primary,
	 * then we swap the secondary into the primary slot.
	 */
	if (!(inventory[INVEN_WIELD].k_idx) &&
		inventory[INVEN_ARM].k_idx &&
		inventory[INVEN_ARM].tval != TV_SHIELD)
	{
		object_copy(&inventory[INVEN_WIELD], &inventory[INVEN_ARM]);
		object_wipe(&inventory[INVEN_ARM]);

		/* Redraw equippy chars */
		p_ptr->redraw |= (PR_EQUIPPY);
	}

	/* Return the new position of the tracked item, if any */
	return (slot);
}


/*
 * Switch primary and secondary weapons
 */
bool switch_weapons(bool allow_empty)
{
    object_type obj;
    object_type *o_ptr = &obj;

    object_type *i_ptr = &inventory[INVEN_WIELD];
    object_type *j_ptr = &inventory[INVEN_ARM];

    /* Disallow empty primary weapons if necessary */
    if (allow_empty || (is_melee_weapon(i_ptr) && is_melee_weapon(j_ptr)))
    {
        object_copy(o_ptr, i_ptr);
        object_copy(i_ptr, j_ptr);
        object_copy(j_ptr, o_ptr);
        object_wipe(o_ptr);

        p_ptr->update |= PU_BONUS;
        return (TRUE);
    }
	return (FALSE);
}

/*
 * Copy of "get_tag" (in object1.c) that looks only at the given
 * inventory slot, and accepts any letter between '@' and the number.
 * Stores the number found.
 */
static bool get_tag_num(int i, int *tag_num)
{
	cptr s;

	object_type *o_ptr = &inventory[i];
	bool flag = FALSE;

	/* Skip non-objects */
	if (!o_ptr->k_idx) return (FALSE);

	/* Skip empty inscriptions */
	if (!o_ptr->note) return (FALSE);

	/* Find a '@' */
	s = strchr(quark_str(o_ptr->note), '@');

	/* Process all tags */
	while (s)
	{
		/* Look for a digit (scan for either "@1" or "@x1") */
		if (isdigit(s[1]))
		{
			*tag_num = D2I(s[1]);
			flag = TRUE;
		}
		else if (s[1] && isdigit(s[2]))
		{
			*tag_num = D2I(s[2]);
			flag = TRUE;
		}

		/* We found a digit */
		if (flag)
		{
			/* Hack -- translate tag_num from 1-10 to 0-9 format */
			if (*tag_num == 0) *tag_num = 9;
			else               *tag_num -= 1;

			/* Success */
			return (TRUE);
		}

		/* Find another '@' */
		s = strchr(s + 1, '@');
	}

	/* No tag of the type we're looking for */
	return (FALSE);
}


/*
 * Get the space used in the quiver by an object.  Assume object is legal.
 */
int quiver_count_item(const object_type *o_ptr, int quantity)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Throwing weapons take up more space */
	if ((f1 & (TR1_THROWING)) && (!is_missile(o_ptr)))
	{
		return (quantity * THROWER_AMMO_FACTOR);
	}

	/* All other objects take up the ordinary amount of space */
	return (quantity);
}



/*
 * Count number of missiles in the quiver slots.
 */
int quiver_count(void)
{
	int i;
	int ammo_num = 0;

	/* Scan the slots */
	for (i = INVEN_Q1; i <= INVEN_Q0; i++)
	{
		/* Get the item */
		object_type *i_ptr = &inventory[i];

		/* Ignore empty */
		if (!i_ptr->k_idx) continue;

		/* Tally up items, correctly allowing for various sizes */
		ammo_num += quiver_count_item(i_ptr, i_ptr->number);
	}

	/* Return */
	return (ammo_num);
}


/*
 * Calculate and apply the reduction in pack size due to use of the
 * quiver.
 */
void find_quiver_size(void)
{
	int ammo_num = quiver_count();

	/* Every 99 missile-equivalents in the quiver takes up one backpack slot. */
	p_ptr->pack_size_reduce =
		(ammo_num + MAX_STACK_SIZE - 2) / (MAX_STACK_SIZE - 1);
}


/*
 * Update (combine and sort ammo in) the quiver.  -LM-
 *
 * If requested, find the right slot to put new ammo in, make it
 * available, and return it.
 *
 * Items marked with inscriptions of the form "@ [any letter or none]
 * [any digit]" ("@f4", "@4", etc.) will be placed in the slot the
 * digit corresponds to.  Everything else will be sorted around them.
 */
int process_quiver(int num_new, object_type *o_ptr)
{
	int i, j, k, num;
	int tag_num;

	int slot=0;

	s32b i_value;
	s32b j_value;

	object_type *i_ptr;
	object_type *j_ptr;

	object_type object_type_body;

	bool flag = FALSE;

	bool track = FALSE;

	bool temp_slot = FALSE;

	bool untouchable[1 + INVEN_Q0 - INVEN_Q1];

	/* All slots start out being alterable. */
	for (i = 0; i < 1 + INVEN_Q0 - INVEN_Q1; i++)
		untouchable[i] = FALSE;


	/* Combine the quiver (backwards) */
	for (i = INVEN_Q0; i > INVEN_Q1; i--)
	{
		/* Get the item */
		i_ptr = &inventory[i];

		/* Skip empty items */
		if (!i_ptr->k_idx) continue;

		/* Scan the items above that item */
		for (j = INVEN_Q1; j < i; j++)
		{
			/* Get the item */
			j_ptr = &inventory[j];

			/* Skip empty items */
			if (!j_ptr->k_idx) continue;

			/* Can we drop "i_ptr" onto "j_ptr"? */
			if (object_similar(j_ptr, i_ptr))
			{
				/* Take note */
				flag = TRUE;

				/* Add together the item counts */
				object_absorb(j_ptr, i_ptr);

				/* One object is gone */
				p_ptr->equip_cnt--;

				/* Slide everything down */
				for (k = i; k < INVEN_Q0; k++)
				{
					/* Hack -- slide object */
					COPY(&inventory[k], &inventory[k+1], object_type);
				}

				/* Hack -- wipe empty slot */
				object_wipe(&inventory[k]);

				/* Window stuff */
				p_ptr->window |= (PW_EQUIP);

				/* Done */
				break;
			}
		}
	}

	/* If requested, find a slot for new ammo. */
	if ((num_new) && (o_ptr))
	{
		/* Search for available slots. */
		for (i = INVEN_Q1; i <= INVEN_Q0; i++)
		{
			/* Get the item */
			i_ptr = &inventory[i];

			/* Accept empty slot */
			if (!i_ptr->k_idx)
			{
				slot = i;
				temp_slot = TRUE;
				continue;
			}

			/* Accept slot that has space to absorb more */
			if ((object_similar(o_ptr, i_ptr)) &&
			    ((num_new + i_ptr->number) < MAX_STACK_SIZE))
			{
				slot = i;
				temp_slot = FALSE;
				break;
			}
		}

		/* TEMPORARILY put the new ammo in the quiver for sorting. */
		if (temp_slot)
		{
			object_copy(&inventory[slot], o_ptr);
		}

	}

	/* Re-order the quiver (forwards) */
	for (i = INVEN_Q1; i <= INVEN_Q0; i++)
	{
		/* Get the item */
		i_ptr = &inventory[i];

		/* Skip empty slots */
		if (!i_ptr->k_idx) continue;

		/*
		 * Put items inscribed "@#" into the quiver corresponding
		 * to their number.
		 */

		/* Get the inscription of the item. */
		if ((get_tag_num(i, &tag_num)) && (tag_num != i - INVEN_Q1))
		{
			/* Save tag number of this object */
			int tag_num1 = tag_num;

			/* Get local object */
			j_ptr = &object_type_body;

			/* Save a copy of the moving item */
			object_copy(j_ptr, &inventory[i]);

			/* Move the item in the destination slot to the original location. */
			object_copy(&inventory[i], &inventory[INVEN_Q1 + tag_num]);

			/* Put the tagged item where it wants to go. */
			object_copy(&inventory[INVEN_Q1 + tag_num], j_ptr);

			/* XXX -- Delete any duplicate tags. */
			if ((get_tag_num(i, &tag_num)) && (tag_num == tag_num1))
			{
				j_ptr = &inventory[i];
				j_ptr->note = 0;
			}

			/* Mark the destination slot as being untouchable. */
			untouchable[tag_num] = TRUE;

			/* Keep track of 'new' item */
			if (slot == i) slot = INVEN_Q1 + tag_num;
			else if (slot == INVEN_Q1 + tag_num) slot = i;

			/* Go to next slot. */
			continue;
		}

		/* Leave items with inscriptions that match their slots alone. */
		if ((get_tag_num(i, &tag_num)) && (tag_num == i - INVEN_Q1))
		{
			/* Mark this slot as being untouchable */
			untouchable[i - INVEN_Q1] = TRUE;

			/* Go to next slot. */
			continue;
		}

		/* Get the "value" of the item */
		i_value = object_value(i_ptr);

		/* Scan every occupied slot */
		for (j = INVEN_Q1; j <= INVEN_Q0; j++)
		{
			/* Skip untouchable slots. */
			if (untouchable[j - INVEN_Q1]) continue;

			/* Get the item already there */
			j_ptr = &inventory[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;

			/* Hack -- suitable ammo always comes first */
			if ((i_ptr->tval == p_ptr->ammo_tval) &&
			    (j_ptr->tval != p_ptr->ammo_tval)) break;
			if ((j_ptr->tval == p_ptr->ammo_tval) &&
			    (i_ptr->tval != p_ptr->ammo_tval)) continue;

			/* Objects sort by decreasing type */
			if (i_ptr->tval > j_ptr->tval) break;
			if (i_ptr->tval < j_ptr->tval) continue;

			/* Non-aware objects go below aware ones */
			if (object_aware_p(i_ptr) && !object_aware_p(j_ptr)) break;
			if (!object_aware_p(i_ptr) && object_aware_p(j_ptr)) continue;

			/* New non-aware objects go below old */
			if (!object_aware_p(i_ptr) && !object_aware_p(j_ptr)) continue;

			/* If aware, objects sort by increasing sval */
			if (i_ptr->sval < j_ptr->sval) break;
			if (i_ptr->sval > j_ptr->sval) continue;

			/* Unidentified objects go below identified ones */
			if (object_known_p(i_ptr) && !object_known_p(j_ptr)) break;
			if (!object_known_p(i_ptr) && object_known_p(j_ptr)) continue;

			/* Determine the "value" of the pack item */
			j_value = object_value(j_ptr);

			/* Objects sort by *increasing* value, which makes it easier to preserve good ammo */
			if (i_value > j_value) continue;
			if (i_value < j_value) break;
		}

		/* Never move down */
		if (j >= i) continue;

		/* Take note */
		flag = TRUE;

		/* Get local object */
		j_ptr = &object_type_body;

		/* Save a copy of the moving item */
		object_copy(j_ptr, &inventory[i]);

		/* Keep track of 'new' item */
		if (slot == i)
		{
			slot = j;
			track = TRUE;
		}

		/* Slide the objects */
		for (k = i; k > j;)
		{
			/* Do not alter untouchable slots. */
			for (num = 1; num < 11;)
			{
				if (untouchable[k - num - INVEN_Q1]) num++;
				else break;
			}

			/* Slide the item */
			object_copy(&inventory[k], &inventory[k-num]);

			/* Keep track of 'new' item */
			if ((slot == k-num) && (!track)) slot = k;

			/* Move down to the next alterable slot. */
			k -= num;
		}

		/* Helper for tracking 'new' ammo */
		track = FALSE;

		/* Insert the moving item */
		object_copy(&inventory[j], j_ptr);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}

	/* Remove temporary ammo.  Will be added for real later. */
	if (temp_slot) object_wipe(&inventory[slot]);

	/* Calculate backpack reduction */
	find_quiver_size();

	/* Message */
	if ((!num_new) && (flag)) msg_print("You reorganize your quiver.");

	return (slot);

}
