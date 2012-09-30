/* File: object2.c */

/* Purpose: Object code, part 2 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "script.h"

/*
 * Temp object used to return an object not allocated via o_pop().
 *
 * This is used by object_dup() and by object_prep() to
 * return "new" objects.  Note that these routines delete
 * any object stored here before they use it.  This means
 * that object (quark) refcounting will work.  This should
 * be the only "static" object used, unless you make
 * absolutely sure the memory management is correct.
 *
 * Be very careful - object_copy() does not duplicate
 * references, where as object_dup() does.  Make sure any
 * allocated object will be wiped eventually.  temp_object
 * is wiped when ever it is used so it is safe.
 *
 * Note - because several routines use this variable, any
 * statically allocated object returned must be used asap.
 */
static object_type temp_object;


/*
 * Prepare an object based on an existing object
 */
static void object_copy(object_type *o_ptr, const object_type *j_ptr)
{
	/* Copy the structure */
	COPY(o_ptr, j_ptr, object_type);
}

/*
 * Excise a dungeon object from any stacks
 */
static void excise_object_idx(s16b *o_idx_ptr, object_type *o_ptr)
{
	object_type *j_ptr = NULL;

	object_type *q_ptr;

	/* Scan all objects in the list */
	OBJ_ITT_START (*o_idx_ptr, q_ptr)
	{
		/* Hack - Done? */
		if (q_ptr == o_ptr)
		{
			/* No previous */
			if (!j_ptr)
			{
				/* Remove from list */
				*o_idx_ptr = q_ptr->next_o_idx;
			}

			/* Real previous */
			else
			{
				/* Remove from list */
				j_ptr->next_o_idx = q_ptr->next_o_idx;
			}

			/* Forget next pointer */
			q_ptr->next_o_idx = 0;

			/* Done */
			break;
		}

		/* Save previous object */
		j_ptr = q_ptr;
	}
	OBJ_ITT_END;
}


/*
 * Delete a dungeon object
 *
 * Handle "stacks" of objects correctly.
 */
void delete_held_object(s16b *o_idx_ptr, object_type *o_ptr)
{
	/* Excise */
	excise_object_idx(o_idx_ptr, o_ptr);

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Count objects */
	o_cnt--;
}

/*
 * Delete an object we know is lying on the dungeon floor.
 */
void delete_dungeon_object(object_type *o_ptr)
{
	int x, y;

	cave_type *c_ptr;

	/* Location */
	x = o_ptr->ix;
	y = o_ptr->iy;

	c_ptr = area(x, y);

	/* Excise */
	excise_object_idx(&c_ptr->o_idx, o_ptr);

	/* Visual update */
	lite_spot(x, y);

	/* Wipe the object */
	object_wipe(o_ptr);

	/* Count objects */
	o_cnt--;
}


/*
 * Deletes all objects at given location
 */
void delete_object(int x, int y)
{
	cave_type *c_ptr;

	/* Refuse "illegal" locations */
	if (!in_bounds2(x, y)) return;

	/* Grid */
	c_ptr = area(x, y);

	/* Delete the objects */
	delete_object_list(&c_ptr->o_idx);

	/* Visual update */
	lite_spot(x, y);
}


/*
 * Delete a statically-allocated object
 */
static void delete_static_object(object_type *o_ptr)
{
	int i;
	
	/* Deallocate quarks */
	quark_remove(&o_ptr->xtra_name);
	quark_remove(&o_ptr->inscription);

	for (i = 0; i < MAX_TRIGGER; i++)
		quark_remove(&o_ptr->trigger[i]);
	
	/* Do nothing */
	return;
}

/*
 * Deletes all objects at given location
 */
void delete_object_list(s16b *o_idx_ptr)
{
	object_type *o_ptr;

	/*
	 * Special exception: if the first object is (nothing),
	 * just zero the index.
	 * This happens when loading savefiles.
	 */
	if (!o_list[*o_idx_ptr].k_idx)
		*o_idx_ptr = 0;

	/* Scan all objects in the grid */
	OBJ_ITT_START (*o_idx_ptr, o_ptr)
	{
		/* Wipe the object */
		object_wipe(o_ptr);

		/* Count objects */
		if (o_cnt) o_cnt--;
	}
	OBJ_ITT_END;

	/* Objects are gone */
	*o_idx_ptr = 0;
}

/*
 * Drop all the items in the list near the location (x, y).
 */
void drop_object_list(s16b *o_idx_ptr, int x, int y)
{
	object_type *o_ptr;
	object_type *q_ptr;

	/* Drop objects being carried */
	OBJ_ITT_START (*o_idx_ptr, o_ptr)
	{
		/* Duplicate object */
		q_ptr = object_dup(o_ptr);

		/* Delete held object */
		delete_held_object(o_idx_ptr, o_ptr);

		/* Drop object */
		drop_near(q_ptr, -1, x, y);
	}
	OBJ_ITT_END;

	/* No more objects in the list */
	*o_idx_ptr = 0;
}


/*
 * Compact and Reorder the object list
 *
 * This function can be very dangerous, use with caution!
 *
 * When actually "compacting" objects, we base the saving throw on a
 * combination of object level, distance from player, and current
 * "desperation".
 *
 * After "compacting" (if needed), we "reorder" the objects into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_objects(int size)
{
	int i, y, x, num, cnt;

	int cur_lev, cur_dis, chance;

	object_type *o_ptr;

	monster_type *m_ptr;


	/* Compact */
	if (size)
	{
		/* Message */
		msgf("Compacting objects...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
	}


	/* Compact at least 'size' objects */
	for (num = 0, cnt = 1; num < size; cnt++)
	{
		/* Get more vicious each iteration */
		cur_lev = 5 * cnt;

		/* Get closer each iteration */
		cur_dis = 5 * (20 - cnt);

		/* Examine the objects */
		for (i = 1; i < o_max; i++)
		{
			o_ptr = &o_list[i];

			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			/* Hack -- High level objects start out "immune" */
			if (get_object_level(o_ptr) > cur_lev) continue;

			/* Only objects in the dungeon */
			if (!((o_ptr->ix) && (o_ptr->iy))) continue;

			/* Get the location */
			x = o_ptr->ix;
			y = o_ptr->iy;

			/* Nearby objects start out "immune" */
			if (distance(p_ptr->px, p_ptr->py, x, y) < cur_dis) continue;

			/* Saving throw */
			chance = 90;

			/* Hack -- only compact artifacts in emergencies */
			if ((FLAG(o_ptr, TR_INSTA_ART)) && (cnt < 1000)) chance = 100;

			/* Apply the saving throw */
			if (randint0(100) < chance) continue;

			/* Delete the object */
			delete_dungeon_object(o_ptr);

			/* Count it */
			num++;
		}

		/* Scan monster inventories */
		for (i = 1; i < m_max; i++)
		{
			m_ptr = &m_list[i];

			/* Ignore dead monsters */
			if (!m_ptr->r_idx) continue;

			/* We need to hold some objects */
			if (!m_ptr->hold_o_idx) continue;

			/* Get the location */
			x = m_ptr->fx;
			y = m_ptr->fy;

			/* Nearby objects start out "immune" */
			if (distance(p_ptr->px, p_ptr->py, x, y) < cur_dis) continue;

			/* Saving throw (higher than dungeon objects) */
			chance = 99;

			OBJ_ITT_START (m_ptr->hold_o_idx, o_ptr)
			{
				/* Hack -- High level objects start out "immune" */
				if (get_object_level(o_ptr) > cur_lev) continue;

				/* Hack -- only compact artifacts in emergencies */
				if ((FLAG(o_ptr, TR_INSTA_ART)) &&
					(cnt < 1000)) chance = 100;

				/* Apply the saving throw */
				if (randint0(100) < chance) continue;

				/* Delete the object */
				delete_held_object(&m_ptr->hold_o_idx, o_ptr);

				/* Count it */
				num++;
			}
			OBJ_ITT_END;
		}

	}


	/* Excise dead objects (backwards!) */
	while (o_max > 1)
	{
		object_type *o_ptr = &o_list[o_max - 1];

		/* Stop when we get to an object */
		if (o_ptr->k_idx) break;

		/* Compress "o_max" */
		o_max--;
	}
}


/*
 * Delete all the non-held items.
 * Items in the players inventory are not touched.
 * Items in monster inventories are wiped by
 * calling wipe_m_list() before this function.
 *
 * Hack -- we clear the "c_ptr->o_idx" field for every grid
 * since we know we are clearing every object.  Technically, we
 * only clear those fields for grids containing objects,
 * and we clear it once for every such object.
 */
void wipe_o_list(void)
{
	int i;

	int x, y;

	cave_type *c_ptr;
	
	object_type *o_ptr;
	
	/* Set all objects to be unallocated */
	for (i = 1; i < o_max; i++)
	{
		o_ptr = &o_list[i];
		
		o_ptr->allocated = FALSE;
	}
	
	/* Only if inventory exists */
	if (o_list[p_ptr->inventory].k_idx)
	{
		/* Save players inventory (only objects in a list to save) */
		OBJ_ITT_START (p_ptr->inventory, o_ptr)
		{
			o_ptr->allocated = TRUE;
		}
		OBJ_ITT_END;
	}
	
	/* Delete the existing objects */
	for (i = 1; i < o_max; i++)
	{
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip allocated objects */
		if (o_ptr->allocated) continue;

		/* Preserve artifacts */
		if (preserve_mode && (FLAG(o_ptr, TR_INSTA_ART)) &&
			o_ptr->a_idx &&
			(a_info[o_ptr->a_idx].cur_num == 1))
		{
			a_info[o_ptr->a_idx].cur_num = 0;
		}
		
		/* Access location */
		y = o_ptr->iy;
		x = o_ptr->ix;
		
		/* Store item? */
		if (!x && !y)
		{
			/* Hack - just kill it */
			object_wipe(o_ptr);

			/* Count objects */
			o_cnt--;
			
			continue;
		}

		/* Access grid */
		c_ptr = area(x, y);

		/* Hack -- see above */
		c_ptr->o_idx = 0;

		/* Delete the object */
		delete_dungeon_object(o_ptr);
	}

	/* Compress the object list */
	compact_objects(0);
}


/*
 * Wipe objects in region
 */
void wipe_objects(int rg_idx)
{
	int i;

	object_type *o_ptr;

	/* Delete the existing objects */
	for (i = 1; i < o_max; i++)
	{
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Enforce region */
		if (o_ptr->region != rg_idx) continue;

		/* Preserve artifacts */
		if (preserve_mode && (FLAG(o_ptr, TR_INSTA_ART)) &&
			o_ptr->a_idx &&
			(a_info[o_ptr->a_idx].cur_num == 1))
		{
			a_info[o_ptr->a_idx].cur_num = 0;
		}

		/* Delete the object */
		delete_dungeon_object(o_ptr);
	}
}


/* Current object counter */
static s16b o_cur = 1;

/*
 * Acquires and returns the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 *
 *
 * We have a choice... scan from o_cur onwards, or use o_max.
 *
 * Using o_max is fast, but it leads to fragmentation.
 * Using o_cur is slower (and might not even give a better
 * result than using o_max), but it leads to a more compact
 * object distribution.
 */
static s16b o_pop(void)
{
	bool wrapped = FALSE;

	/* Wrap counter */
	if (o_cur >= o_max) o_cur = 1;

	/*
	 * If the number remaining is less than one third of the
	 * total number of allocated objects, then add a new object
	 * to the end of the list.
	 *
	 * We add 1 to o_cnt because object 0 is unusable.
	 *
	 * Feel free to tune this parameter.
	 */
	if ((o_max - (o_cnt + 1)) * 3 < o_max)
	{
		/* Initial allocation */
		if (o_max < z_info->o_max)
		{
			/* Expand object array */
			o_max++;

			/* Count objects */
			o_cnt++;

			/* Use this object */
			return (o_max - 1);
		}
	}

	/* Recycle dead objects */
	while (TRUE)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[o_cur];

		/* Skip live objects */
		if (o_ptr->k_idx)
		{
			/* Increment counter */
			o_cur++;

			/* Wrap counter */
			if (o_cur >= o_max)
			{
				/* Infinite loop protection */
				if (wrapped) break;

				o_cur = 1;
				wrapped = TRUE;
			}
			continue;
		}

		/* Count objects */
		o_cnt++;

		/* Use this object */
		return (o_cur);
	}


	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msgf("Too many objects!");

	/* Oops */
	return (0);
}

/*
 * Add an object to a list.  o_ptr is a pointer
 * to a statically declared object that is assumed
 * not to be in the standard object array.
 *
 * We do not adjust position, held or region information.
 *
 * The old object is wiped.
 */
object_type *add_object_list(s16b *o_idx_ptr, object_type *o_ptr)
{
	s16b o_idx;

	object_type *j_ptr;

	/* Get a new object */
	o_idx = o_pop();

	/* Hack - bail out */
	if (!o_idx) return (NULL);

	/* Point to the object */
	j_ptr = &o_list[o_idx];

	/* Move to the list */
	swap_objects(j_ptr, o_ptr);

	/* Add to the list */
	j_ptr->next_o_idx = *o_idx_ptr;
	*o_idx_ptr = o_idx;

	/* Now held */
	j_ptr->allocated = TRUE;

	/* Return the new item */
	return (j_ptr);
}

/*
 * Move an object from one list to another
 */
void move_object(s16b *tgt_list_ptr, s16b *cur_list_ptr, object_type *o_ptr)
{
	/* Excise object from the first list */
	excise_object_idx(cur_list_ptr, o_ptr);

	/* Add to the new list */
	o_ptr->next_o_idx = *tgt_list_ptr;

	/* *tgt_list_ptr = o_idx; */
	*tgt_list_ptr = GET_ARRAY_INDEX(o_list, o_ptr);
}

/*
 * Swap objects
 */
void swap_objects(object_type *o1_ptr, object_type *o2_ptr)
{
	object_type temp;

	/* Copy the object */
	object_copy(&temp, o2_ptr);

	/* Copy the object */
	object_copy(o2_ptr, o1_ptr);

	/* Get correct next-object fields */
	o2_ptr->next_o_idx = temp.next_o_idx;
	temp.next_o_idx = o1_ptr->next_o_idx;

	/* Get correct position fields */
	o2_ptr->ix = temp.ix;
	temp.ix = o1_ptr->ix;
	o2_ptr->iy = temp.iy;
	temp.iy = o1_ptr->iy;

	/* Get correct region */
	o2_ptr->region = temp.region;
	temp.region = o1_ptr->region;

	/* Get correct allocated value */
	o2_ptr->allocated = temp.allocated;
	temp.allocated = o1_ptr->allocated;

	/* Copy the object */
	object_copy(o1_ptr, &temp);
}

/*
 * Apply a "object restriction function" to the "object allocation table"
 */
void get_obj_num_prep(object_hook_type object_hook)
{
	int i;

	byte prob;

	/* Get the entry */
	alloc_entry *table = alloc_kind_table;

	/* Scan the allocation table */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* Accept objects which pass the restriction, if any */
		if (object_hook)
		{
			/* Get probability */
			prob = (*object_hook) (table[i].index);

			/* Paranoia */
			if (prob > 100) prob = 100;

			/* Accept this object */
			table[i].prob2 = (table[i].prob1 * prob) / 100;
		}
		else
		{
			/* Accept this object */
			table[i].prob2 = table[i].prob1;
		}
	}
}


/*
 * Choose an object kind that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "object allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" object, in
 * a relatively efficient manner.
 *
 * It is more likely to acquire an object of the given level
 * than one of a lower level.  This is done by choosing three objects
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no objects are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 */
s16b get_obj_num(int level, int min_level)
{
	int i;
	long value1, value2, total;
	alloc_entry *table = alloc_kind_table;

	/* Luck gives occasional out-of-depth items */
	if ((FLAG(p_ptr, TR_STRANGE_LUCK)) && one_in_(13))
	{
		level += randint1(one_in_(7) ? 40 : 10);
	}

	/* Occasional "boost" */
	if (one_in_(GREAT_OBJ))
	{
		/* What a bizarre calculation */
		level = 1 + (level * MAX_DEPTH / randint1(MAX_DEPTH));
	}

	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* Objects are sorted by depth */
		if (table[i].level > level) break;

		/* What John West rejects, makes John West the best. */
		if (table[i].level < min_level) continue;

		/* Total */
		total += table[i].prob2;
	}

	/* No legal objects */
	if (total <= 0) return (0);


	/* Pick an object */
	value1 = randint0(total);

	for (i = 0; i < 3; i++)
	{
		/* Try for a "better" object once */
		value2 = randint0(total);

		/* Is it better? */
		if (value2 > value1)
		{
			/* This hack works because the object table is sorted by depth */
			value1 = value2;
		}
	}

	/* Find the object */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* What John West rejects, makes John West the best. */
		if (table[i].level < min_level) continue;

		/* Decrement */
		value1 -= table[i].prob2;

		/* A match? */
		if (value1 < 0L) break;
	}

	/* Result */
	return (table[i].index);
}


/*
 * Known is true when the "attributes" of an object are "known".
 * These include tohit, todam, toac, cost, and pval (charges).
 *
 * Note that "knowing" an object gives you everything that an "awareness"
 * gives you, and much more.  In fact, the player is always "aware" of any
 * item of which he has full "knowledge".
 *
 * But having full knowledge of, say, one "wand of wonder", does not, by
 * itself, give you knowledge, or even awareness, of other "wands of wonder".
 * It happens that most "identify" routines (including "buying from a shop")
 * will make the player "aware" of the object as well as fully "know" it.
 *
 * This routine also removes any inscriptions generated by "feelings".
 */
void object_known(object_type *o_ptr)
{
	/* Remove "default inscriptions" */
	o_ptr->feeling = FEEL_NONE;

	/* Clear the "Felt" info */
	o_ptr->info &= ~(OB_SENSE);

	/* Clear the "Empty" info */
	o_ptr->info &= ~(OB_EMPTY);

	/* Now we know about the item */
	o_ptr->info |= (OB_KNOWN);
}


/*
 * The player is now aware of the effects of the given object.
 */
void object_aware(object_type *o_ptr)
{
	/* Fully aware of the effects */
	k_info[o_ptr->k_idx].aware = TRUE;
}


/*
 * Something has been "sampled"
 */
void object_tried(object_type *o_ptr)
{
	/* Mark it as tried (even if "aware") */
	k_info[o_ptr->k_idx].tried = TRUE;
}

/*
 * The player has now *identified* the item
 */
void object_mental(object_type *o_ptr)
{
	o_ptr->info |= (OB_MENTAL);
}


/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(const object_type *o_ptr)
{
	/* Aware item -- use template cost */
	if (object_aware_p(o_ptr)) return (k_info[o_ptr->k_idx].cost);

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		case TV_FOOD:
		{
			/* Un-aware Food */
			return (5L);
		}

		case TV_POTION:
		{
			/* Un-aware Potions */
			return (20L);
		}

		case TV_SCROLL:
		{
			/* Un-aware Scrolls */
			return (20L);
		}

		case TV_STAFF:
		{
			/* Un-aware Staffs */
			return (70L);
		}

		case TV_WAND:
		{
			/* Un-aware Wands */
			return (50L);
		}

		case TV_ROD:
		{
			/* Un-aware Rods */
			return (90L);
		}

		case TV_RING:
		{
			/* Un-aware Rings */
			return (45L);
		}

		case TV_AMULET:
		{
			/* Un-aware Amulets */
			return (45L);
		}
	}

	/* Paranoia -- Oops */
	return (0L);
}

/* Return the sign of the argument * x * x. */
static s32b sqvalue(s32b x)
{
	if (x < 0) return (-x * x);
	return (x * x);
}

/* Return a number dependent on the argument */
static s32b bonus_value(s32b x)
{
	if (x <= 0)
		return 5 * x;
	if (x < 10)
		return x + 5;
	if (x < 20)
		return 2 * (x - 10) + 15;
	return 5 * (x - 20) + 35;
}


/* Return the value of the flags the object has... */
s32b flag_cost(const object_type *o_ptr, int plusses)
{
	s32b total = 0;

	if (FLAG(o_ptr, TR_STR)) total += (500 * plusses);
	if (FLAG(o_ptr, TR_INT)) total += (500 * plusses);
	if (FLAG(o_ptr, TR_WIS)) total += (500 * plusses);
	if (FLAG(o_ptr, TR_DEX)) total += (500 * plusses);
	if (FLAG(o_ptr, TR_CON)) total += (500 * plusses);
	if (FLAG(o_ptr, TR_CHR)) total += (250 * plusses);

	if ((FLAG(o_ptr, TR_SP)) && (plusses > 0)) total += (2500 * plusses);
	if (FLAG(o_ptr, TR_CHAOTIC)) total += 500;
	if (FLAG(o_ptr, TR_VAMPIRIC)) total += 5000;
	if (FLAG(o_ptr, TR_STEALTH)) total += (50 * plusses);
	if (FLAG(o_ptr, TR_SEARCH)) total += (50 * plusses);
	if (FLAG(o_ptr, TR_INFRA)) total += (30 * plusses);
	if (FLAG(o_ptr, TR_TUNNEL)) total += (20 * plusses);
	if ((FLAG(o_ptr, TR_SPEED)) && (plusses > 0)) total += (500 * sqvalue(plusses));
	if ((FLAG(o_ptr, TR_BLOWS)) && (plusses > 0)) total += (2500 * sqvalue(plusses));

	if (FLAG(o_ptr, TR_SLAY_ANIMAL)) total += 750;
	if (FLAG(o_ptr, TR_SLAY_EVIL))   total += 750;
	if (FLAG(o_ptr, TR_SLAY_UNDEAD)) total += 750;
	if (FLAG(o_ptr, TR_SLAY_DEMON))  total += 750;
	if (FLAG(o_ptr, TR_SLAY_ORC))    total += 750;
	if (FLAG(o_ptr, TR_SLAY_TROLL))  total += 750;
	if (FLAG(o_ptr, TR_SLAY_GIANT))  total += 750;
	if (FLAG(o_ptr, TR_SLAY_DRAGON)) total += 750;
	if (FLAG(o_ptr, TR_KILL_DRAGON)) total += 1500;

	if (FLAG(o_ptr, TR_VORPAL)) total += 1500;
	if (FLAG(o_ptr, TR_IMPACT)) total += 1500;

	if (FLAG(o_ptr, TR_BRAND_POIS)) total += 750;
	if (FLAG(o_ptr, TR_BRAND_ACID)) total += 750;
	if (FLAG(o_ptr, TR_BRAND_ELEC)) total += 750;
	if (FLAG(o_ptr, TR_BRAND_FIRE)) total += 750;
	if (FLAG(o_ptr, TR_BRAND_COLD)) total += 750;

	if (FLAG(o_ptr, TR_SUST_STR)) total += 200;
	if (FLAG(o_ptr, TR_SUST_INT)) total += 200;
	if (FLAG(o_ptr, TR_SUST_WIS)) total += 200;
	if (FLAG(o_ptr, TR_SUST_DEX)) total += 200;
	if (FLAG(o_ptr, TR_SUST_CON)) total += 200;
	if (FLAG(o_ptr, TR_SUST_CHR)) total += 200;

	if (FLAG(o_ptr, TR_IM_POIS)) total += 10000;
	if (FLAG(o_ptr, TR_IM_ACID)) total += 10000;
	if (FLAG(o_ptr, TR_IM_ELEC)) total += 10000;
	if (FLAG(o_ptr, TR_IM_FIRE)) total += 10000;
	if (FLAG(o_ptr, TR_IM_COLD)) total += 10000;
	if (FLAG(o_ptr, TR_IM_LITE)) total += 10000;
	if (FLAG(o_ptr, TR_IM_DARK)) total += 10000;

	if (FLAG(o_ptr, TR_THROW)) total += 2000;
	if (FLAG(o_ptr, TR_REFLECT)) total += 5000;

	if (FLAG(o_ptr, TR_RES_ACID))   total += 500;
	if (FLAG(o_ptr, TR_RES_ELEC))   total += 500;
	if (FLAG(o_ptr, TR_RES_FIRE))   total += 500;
	if (FLAG(o_ptr, TR_RES_COLD))   total += 500;
	if (FLAG(o_ptr, TR_RES_POIS))   total += 1500;
	if (FLAG(o_ptr, TR_RES_FEAR))   total += 1500;
	if (FLAG(o_ptr, TR_RES_LITE))   total += 1500;
	if (FLAG(o_ptr, TR_RES_DARK))   total += 1500;
	if (FLAG(o_ptr, TR_RES_BLIND))  total += 1500;
	if (FLAG(o_ptr, TR_RES_CONF))   total += 1500;
	if (FLAG(o_ptr, TR_RES_SOUND))  total += 1500;
	if (FLAG(o_ptr, TR_RES_SHARDS)) total += 1500;
	if (FLAG(o_ptr, TR_RES_NETHER)) total += 1500;
	if (FLAG(o_ptr, TR_RES_NEXUS))  total += 1500;
	if (FLAG(o_ptr, TR_RES_CHAOS))  total += 1500;
	if (FLAG(o_ptr, TR_RES_DISEN))  total += 1500;

	if (FLAG(o_ptr, TR_SH_FIRE)) total += 1000;
	if (FLAG(o_ptr, TR_SH_ELEC)) total += 1000;
	if (FLAG(o_ptr, TR_SH_ACID)) total += 1000;
	if (FLAG(o_ptr, TR_SH_COLD)) total += 1000;

	if (FLAG(o_ptr, TR_QUESTITEM)) total += 0;
	if (FLAG(o_ptr, TR_XXX4)) total += 0;
	if (FLAG(o_ptr, TR_NO_TELE)) total += 2500;
	if (FLAG(o_ptr, TR_NO_MAGIC)) total += 2500;
	if (FLAG(o_ptr, TR_TY_CURSE)) total -= 15000;
	if (FLAG(o_ptr, TR_EASY_KNOW)) total += 0;
	if (FLAG(o_ptr, TR_HIDE_TYPE)) total += 0;
	if (FLAG(o_ptr, TR_SHOW_MODS)) total += 0;
	if (FLAG(o_ptr, TR_INSTA_ART)) total += 0;

	/* EGO_XTRA_ABILITY */
	if (FLAG(o_ptr, TR_SLOW_DIGEST)) total += 250;
	if (FLAG(o_ptr, TR_FEATHER))     total += 250;
	if (FLAG(o_ptr, TR_LITE))        total += 500;
	if (FLAG(o_ptr, TR_SEE_INVIS))   total += 500;
	if (FLAG(o_ptr, TR_REGEN))       total += 1000;
	if (FLAG(o_ptr, TR_FREE_ACT))    total += 1000;
	if (FLAG(o_ptr, TR_HOLD_LIFE))   total += 2000;
	if (FLAG(o_ptr, TR_TELEPATHY))   total += 2000;

	if (FLAG(o_ptr, TR_XTRA_MIGHT)) total += 1000;
	if (FLAG(o_ptr, TR_XTRA_SHOTS)) total += 1000;

	if (FLAG(o_ptr, TR_IGNORE_ACID)) total += 50;
	if (FLAG(o_ptr, TR_IGNORE_ELEC)) total += 50;
	if (FLAG(o_ptr, TR_IGNORE_FIRE)) total += 50;
	if (FLAG(o_ptr, TR_IGNORE_COLD)) total += 50;

	if (FLAG(o_ptr, TR_ACTIVATE)) total += 0;
	if (FLAG(o_ptr, TR_DRAIN_EXP)) total -= 12500;
	if (FLAG(o_ptr, TR_TELEPORT))
	{
		if (cursed_p(o_ptr))
			total -= 7500;
		else
			total += 250;
	}
	if (FLAG(o_ptr, TR_AGGRAVATE)) total -= 5000;
	if (FLAG(o_ptr, TR_BLESSED)) total += 200;
	if (FLAG(o_ptr, TR_CURSED)) total -= 5000;
	if (FLAG(o_ptr, TR_HEAVY_CURSE)) total -= 12500;
	if (FLAG(o_ptr, TR_PERMA_CURSE)) total -= 15000;
	if (FLAG(o_ptr, TR_LUCK_10)) total += 3000;
	if (FLAG(o_ptr, TR_WILD_SHOT)) total += 50;
	if (FLAG(o_ptr, TR_WILD_WALK)) total += 200;
	if (FLAG(o_ptr, TR_MUTATE)) total += 500;
	if (FLAG(o_ptr, TR_PATRON)) total += 500;
	if (FLAG(o_ptr, TR_STRANGE_LUCK)) total += 2000;
	if (FLAG(o_ptr, TR_PASS_WALL)) total += 25000;
	if (FLAG(o_ptr, TR_GHOUL_TOUCH)) total += 750;
	if (FLAG(o_ptr, TR_PSI_CRIT)) total += 1500;
	if (FLAG(o_ptr, TR_RETURN)) total += 500;
	if (FLAG(o_ptr, TR_EXPLODE)) total += 500;
	if (FLAG(o_ptr, TR_HURT_ACID)) total -= 5000;
	if (FLAG(o_ptr, TR_HURT_ELEC)) total -= 5000;
	if (FLAG(o_ptr, TR_HURT_FIRE)) total -= 5000;
	if (FLAG(o_ptr, TR_HURT_COLD)) total -= 5000;
	if (FLAG(o_ptr, TR_HURT_LITE)) total -= 2500;
	if (FLAG(o_ptr, TR_HURT_DARK)) total -= 2500;
	if (FLAG(o_ptr, TR_XXX27)) total += 0;
	if (FLAG(o_ptr, TR_XXX28)) total += 0;
	if (FLAG(o_ptr, TR_AUTO_CURSE)) total -= 2500;
	if (FLAG(o_ptr, TR_DRAIN_STATS)) total -= 5000;
	if (FLAG(o_ptr, TR_CANT_EAT)) total -= 500;
	if (FLAG(o_ptr, TR_SLOW_HEAL)) total -= 2000;


	return total;
}


/*
 * Return the "real" price of a "known" item, not including discounts
 *
 * Wand and staffs get cost for each charge
 *
 * Armor is worth an extra 5 gold per bonus point to armor class^2.
 *
 * Weapons are worth an extra 5 gold per bonus point^2 (AC,TH,TD).
 *
 * Note: the bonuses are proportional to the points squared.
 *
 * Missiles are only worth 5 gold per bonus point, since they
 * usually appear in groups of 20, and we want the player to get
 * the same amount of cash for any "equivalent" item.  Note that
 * missiles never have any of the "pval" flags, and in fact, they
 * only have a few of the available flags, primarily of the "slay"
 * and "brand" and "ignore" variety.
 *
 * Armor with a negative armor bonus is worthless.
 *
 * Every wearable item with a "pval" bonus is worth extra (see below).
 */
s32b object_value_real(const object_type *o_ptr)
{
	s32b value;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	object_type dummy;

	/* Base cost */
	value = o_ptr->cost;

	/* Hack -- "worthless" items */
	if (!value) return (0L);

	/* 
	 * For non-artifact items, count the flag value
	 */
	if (!o_ptr->a_idx)
	{
		int divisor = 1;

		/* Ammo gets less value from flags */
		if (o_ptr->tval == TV_SHOT ||
		    o_ptr->tval == TV_ARROW ||
		    o_ptr->tval == TV_BOLT)
		{
			divisor = 20;
		}

		/* Initialize the dummy object */
		memcpy(&dummy, o_ptr, sizeof(object_type));

		/* Copy the flags from the type to the dummy object */
		memcpy(dummy.flags, k_ptr->flags, sizeof(dummy.flags));

		/* Price the object's flags vs the default flags */
		value += flag_cost(o_ptr, o_ptr->pval) / divisor;
		value -= flag_cost(&dummy, 1) / divisor;
	}


	/* Analyze the item */
	switch (o_ptr->tval)
	{
		case TV_WAND:
		{
			/* Wands */

			/* Pay extra for charges, depending on standard number of
			 * charges.  Handle new-style wands correctly. -LM-
			 */
			value += (value * o_ptr->pval / o_ptr->number / (k_ptr->pval * 2));

			/* Done */
			break;
		}
		case TV_STAFF:
		{
			/* Staffs */

			/* Pay extra for charges, depending on standard number of
			 * charges.  -LM-
			 */
			value += (value * o_ptr->pval / (k_ptr->pval * 2));

			/* Done */
			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
            /* Rings/Amulets */

            /* Hack -- special handling for amulets of Berserk Strength */
            if (o_ptr->sval == SV_AMULET_BERSERK)
            {
                value += (bonus_value(o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 20);

                /* Done */
                break;
            }

			/* Hack -- negative bonuses are bad */
			if (o_ptr->to_a < 0) return (0L);
			if (o_ptr->to_h < 0) return (0L);
			if (o_ptr->to_d < 0) return (0L);

			/* Factor in the bonuses */
			/*
			 * Note that combat bonuses on a ring/amulet are worth
			 * twice what they are on a weapon/armor
			 */
			value += (bonus_value(o_ptr->to_h + o_ptr->to_d) * 40 +
					  bonus_value(o_ptr->to_a * 2) * 20);

			/* Done */
			break;
		}

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
			/* Armour */

			/* Factor in the bonuses */
			value += (bonus_value(o_ptr->to_h - k_ptr->to_h +
								  o_ptr->to_d - k_ptr->to_d) * 20);
			value += (bonus_value((o_ptr->to_a - k_ptr->to_a) * 2) * 10);

			/* Done */
			break;
		}

		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		{
			/* Bows/Weapons */

			/* Factor in the bonuses */
			value += (bonus_value(o_ptr->to_h + o_ptr->to_d) * 20 +
					  bonus_value(o_ptr->to_a * 2) * 10);

			/* Hack -- Factor in extra damage dice */
			if (k_ptr->dd * k_ptr->ds)
			{
				value = value * o_ptr->dd * o_ptr->ds / (k_ptr->dd * k_ptr->ds);
			}

			/* Done */
			break;
		}

		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Ammo */

			/* Factor in the bonuses */
			value += (bonus_value(o_ptr->to_h + o_ptr->to_d) * 2);

			/* Hack -- Factor in extra damage dice */
			if (k_ptr->dd * k_ptr->ds)
			{
				value = value * o_ptr->dd * o_ptr->ds / (k_ptr->dd * k_ptr->ds);
			}

			/* Done */
			break;
		}

		case TV_FIGURINE:
		{
			/* Figurines, relative to monster level */
			value = (r_info[o_ptr->pval].level *
					 r_info[o_ptr->pval].level * 5L);
			break;
		}
	}

	/* No negative value */
	if (value < 0) value = 0;

	/* Return the value */
	return (value);
}


/*
 * Return the price of an item including plusses (and charges)
 *
 * This function returns the "value" of the given item (qty one)
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
 *
 * Note that discounted items stay discounted forever, even if
 * the discount is "forgotten" by the player via memory loss.
 */
s32b object_value(const object_type *o_ptr)
{
	s32b value;

	/* Known items -- acquire the actual value */
	if (object_known_p(o_ptr))
	{
		/* Broken items -- worthless */
		if (!o_ptr->cost) return (0L);

		/* Cursed items -- worthless */
		if (cursed_p(o_ptr)) return (0L);

		/* Real value (see above) */
		value = object_value_real(o_ptr);
	}

	/* Unknown items -- acquire a base value */
	else
	{
		/* Hack -- Felt broken items */
		if ((o_ptr->info & (OB_SENSE)) && !o_ptr->cost) return (0L);

		/* Hack -- Felt cursed items */
		if ((o_ptr->info & (OB_SENSE)) && cursed_p(o_ptr)) return (0L);

		/* Base value (see above) */
		value = object_value_base(o_ptr);
	}


	/* Apply discount (if any) */
	if (o_ptr->discount) value -= (value * o_ptr->discount / 100L);


	/* Return the final value */
	return (value);
}


/*
 * Distribute charges of rods or wands.
 *
 * o_ptr = source item
 * q_ptr = target item, must be of the same type as o_ptr
 * amt   = number of items that are transfered
 */
void distribute_charges(object_type *o_ptr, object_type *q_ptr, int amt)
{
	int new_charges;

	/* Paranoia */
	if (!o_ptr->number) return;

	/*
	 * Hack -- If rods or wands are dropped, the total maximum timeout or
	 * charges needs to be allocated between the two stacks.  If all the items
	 * are being dropped, it makes for a neater message to leave the original
	 * stack's pval alone. -LM-
	 */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD))
	{
		new_charges = (o_ptr->pval + o_ptr->ac) * amt / o_ptr->number;

		/* Hack - AC is a count of the "used" charges */
		if (o_ptr->tval == TV_WAND)
		{
			/* Used more changes than a single wand has */
			if (o_ptr->ac > new_charges)
			{
				/* Drop an empty wand */
				q_ptr->pval = 0;

				o_ptr->ac -= new_charges;
				q_ptr->ac = new_charges;
			}
			else
			{
				/* Split the charges evenly - then move the "used ones" */
				q_ptr->pval = new_charges - o_ptr->ac;

				q_ptr->ac = o_ptr->ac;
				o_ptr->ac = 0;
			}
		}
		else
		{
			/* Rods are simple - just split them geometrically */
			q_ptr->pval = o_ptr->pval * amt / o_ptr->number;
		}

		/* Subtract moved charges */
		if (amt < o_ptr->number) o_ptr->pval -= q_ptr->pval;

		/* Hack -- Rods also need to have their timeouts distributed.  The
		 * dropped stack will accept all time remaining to charge up to its
		 * maximum.
		 */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			if (q_ptr->pval > o_ptr->timeout)
				q_ptr->timeout = o_ptr->timeout;
			else
				q_ptr->timeout = q_ptr->pval;

			if (amt < o_ptr->number) o_ptr->timeout -= q_ptr->timeout;
		}
	}
}


void reduce_charges(object_type *o_ptr, int amt)
{
	/*
	 * Hack -- If rods or wand are destroyed, the total maximum timeout or
	 * charges of the stack needs to be reduced, unless all the items are
	 * being destroyed. -LM-
	 */
	if (((o_ptr->tval == TV_WAND) ||
		 (o_ptr->tval == TV_ROD)) && (amt < o_ptr->number))
	{
		o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
		o_ptr->ac -= o_ptr->ac * amt / o_ptr->number;
	}
}


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow staffs (if they are known to have equal charges
 * and both are either known or confirmed empty) and rods (in all cases)
 * to combine.
 * Staffs will unstack (if necessary) when they are used, but wands and
 * rods will only unstack if one is dropped. -LM-
 *
 * If permitted, we allow weapons/armor to stack, if fully "known".
 *
 * Missiles will combine if both stacks have the same "known" status.
 * This is done to make unidentified stacks of missiles useful.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests, and activatable items, never stack (for various reasons).
 */
bool object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
	int i;
	
	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return (FALSE);

	/* Analyze the items */
	switch (o_ptr->tval)
	{
		case TV_CHEST:
		{
			/* Chests */

			/* Never okay */
			return (FALSE);
		}

		case TV_FIGURINE:
		case TV_STATUE:
		{
			/* Figurines and Statues */

			/* Never okay */
			return (FALSE);
		}

		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		{
			/* Food and Potions and Scrolls */

			/* Assume okay */
			break;
		}

		case TV_STAFF:
		{
			/* Staffs */

			/* Require either knowledge or known empty for both staffs. */
			if ((!(o_ptr->info & (OB_EMPTY)) &&
				 !object_known_p(o_ptr)) ||
				(!(j_ptr->info & (OB_EMPTY)) &&
				 !object_known_p(j_ptr))) return (FALSE);

			/* Require identical charges, since staffs are bulky. */
			if (o_ptr->pval != j_ptr->pval) return (FALSE);

			/* Assume okay */
			break;
		}

		case TV_WAND:
		{
			/* Wands */

			/* Wand charges combine in O&ZAngband. */
			if (object_known_p(o_ptr) != object_known_p(j_ptr)) return (FALSE);

			/* Assume okay */
			break;
		}

		case TV_ROD:
		{
			/* Rods */

			/* Assume okay */
			break;
		}
		
		case TV_BOW:
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
			/* Weapons and Armor stack if throwable */
			if (!(FLAG(o_ptr, TR_THROW))) return (FALSE);

			/* Fall through */
		}

		case TV_LITE:
		{
			/* Hack - Require identical fuel levels / timeouts */
			if (o_ptr->timeout != j_ptr->timeout) return (FALSE);

			/* Fall through */
		}

		case TV_RING:
		case TV_AMULET:
		{
			/* Rings, Amulets, Lites */

			/* Require full knowledge of both items */
			if (!object_known_p(o_ptr)
				|| !object_known_p(j_ptr)) return (FALSE);
			/* Fall through */
		}

		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Missiles */

			/* Require identical knowledge of both items */
			if (object_known_p(o_ptr) != object_known_p(j_ptr)) return (FALSE);

			/* Require identical "bonuses" */
			if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
			if (o_ptr->to_d != j_ptr->to_d) return (FALSE);
			if (o_ptr->to_a != j_ptr->to_a) return (FALSE);

			/* Require identical "pval" code */
			if (o_ptr->pval != j_ptr->pval) return (FALSE);

			/* Require identical "values" */
			if (o_ptr->ac != j_ptr->ac) return (FALSE);
			if (o_ptr->dd != j_ptr->dd) return (FALSE);
			if (o_ptr->ds != j_ptr->ds) return (FALSE);

			/* Probably okay */
			break;
		}

		default:
		{
			/* Various */

			/* Require knowledge */
			if (!object_known_p(o_ptr)
				|| !object_known_p(j_ptr)) return (FALSE);

			/* Probably okay */
			break;
		}
	}

	/* Hack -- Identical flags! */
	if ((o_ptr->flags[0] != j_ptr->flags[0]) ||
		(o_ptr->flags[1] != j_ptr->flags[1]) || 
		(o_ptr->flags[2] != j_ptr->flags[2]) ||
		(o_ptr->flags[3] != j_ptr->flags[3]))
		return (FALSE);

	/* Hack -- Require identical "cursed" status */
	if (cursed_p(o_ptr) != cursed_p(j_ptr)) return (FALSE);

	/* Hack -- Require identical "broken" status */
	if ((!o_ptr->cost) != (!j_ptr->cost)) return (FALSE);

	/* Need to be identical ego items or artifacts */
	if (o_ptr->xtra_name != j_ptr->xtra_name) return (FALSE);

	/* Hack -- require semi-matching "inscriptions" */
	if (o_ptr->inscription && j_ptr->inscription &&
		(o_ptr->inscription != j_ptr->inscription))
		return (FALSE);

	/* Require matching scripts */
	for (i = 0; i < MAX_TRIGGER; i++)
		if (o_ptr->trigger[i] != j_ptr->trigger[i])
			return (FALSE);

	/* Maximal "stacking" limit */
	if (o_ptr->number + j_ptr->number >= MAX_STACK_SIZE) return (FALSE);

	/* They match, so they must be similar */
	return (TRUE);
}


/*
 * Allow one item to "absorb" another, assuming they are similar
 */
void object_absorb(object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Add together the item counts */
	o_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Hack -- blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr);

	/* If the extra object was fully known */
	if (object_known_full(j_ptr))
	{
		/* Then the stack is fully known too */
		object_mental(o_ptr);
	}

	/* Hack -- blend "mental" status */
	o_ptr->kn_flags[0] |= j_ptr->kn_flags[0];
	o_ptr->kn_flags[1] |= j_ptr->kn_flags[1];
	o_ptr->kn_flags[2] |= j_ptr->kn_flags[2];
	o_ptr->kn_flags[3] |= j_ptr->kn_flags[3];

	/* Hack -- blend "inscriptions" */
    if (j_ptr->inscription)
    {
        quark_remove(&o_ptr->inscription);
        o_ptr->inscription = j_ptr->inscription;
        quark_dup(j_ptr->inscription);
    }

	/* Hack -- blend "feelings" */
	if (j_ptr->feeling) o_ptr->feeling = j_ptr->feeling;

	/* Hack -- could average discounts XXX XXX XXX */
	/* Hack -- save largest discount XXX XXX XXX */
	if (o_ptr->discount < j_ptr->discount) o_ptr->discount = j_ptr->discount;

	/*
	 * Hack -- if rods are stacking, add the pvals
	 * (maximum timeouts) and current timeouts together. -LM-
	 */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval += j_ptr->pval;
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Hack -- if wands are stacking, combine the charges. -LM- */
	if (o_ptr->tval == TV_WAND)
	{
		o_ptr->pval += j_ptr->pval;
		o_ptr->ac += j_ptr->ac;

		/* Hack XXX XXX - remove {empty} inscription. */
		if (o_ptr->pval) o_ptr->info &= ~(OB_EMPTY);
	}
}


/*
 * Are these objects the same except for next_o_idx?  Originally meant for
 * matching object in the inventory so there may be some redundancy here.
 */
bool object_equal(const object_type *o_ptr, const object_type *j_ptr)
{
	int i;

	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return (FALSE);

	/* Identical position */
	if (o_ptr->ix != j_ptr->ix ||
		o_ptr->iy != j_ptr->iy) return (FALSE);

	/* Require identical weights */
	if (o_ptr->weight != j_ptr->weight) return (FALSE);

	/* Require identical tval, sval, pval */
	if (o_ptr->tval != j_ptr->tval ||
		o_ptr->sval != j_ptr->sval ||
		o_ptr->pval != j_ptr->pval) return (FALSE);

	/* Require identical discounts */
	if (o_ptr->discount != j_ptr->discount) return (FALSE);

	/* There is an equal number in the pile */
	if (o_ptr->number != j_ptr->number) return (FALSE);

	/* Require identical to_h, to_d, to_a, ac */
	if (o_ptr->to_h != j_ptr->to_h ||
		o_ptr->to_d != j_ptr->to_d ||
		o_ptr->to_a != j_ptr->to_a ||
		o_ptr->ac != j_ptr->ac) return (FALSE);

	/* The timeout is the same */
	if (o_ptr->timeout != j_ptr->timeout) return (FALSE);

	/* Identical dice */
	if (o_ptr->dd != j_ptr->dd ||
		o_ptr->ds != j_ptr->ds) return (FALSE);

	/* Hack -- require semi-matching "inscriptions" */
	if (o_ptr->inscription && j_ptr->inscription &&
		(o_ptr->inscription != j_ptr->inscription))
		return (FALSE);

	/* Need to be identical ego items or artifacts */
	if (o_ptr->xtra_name != j_ptr->xtra_name) return (FALSE);

	/* Identical flags! */
	if ((o_ptr->flags[0] != j_ptr->flags[0]) ||
		(o_ptr->flags[1] != j_ptr->flags[1]) || 
		(o_ptr->flags[2] != j_ptr->flags[2]) ||
		(o_ptr->flags[3] != j_ptr->flags[3]))
		return (FALSE);

	/* Identical kn_flags! */
	if ((o_ptr->kn_flags[0] != j_ptr->kn_flags[0]) ||
		(o_ptr->kn_flags[1] != j_ptr->kn_flags[1]) || 
		(o_ptr->kn_flags[2] != j_ptr->kn_flags[2]) ||
		(o_ptr->kn_flags[3] != j_ptr->kn_flags[3]))
		return (FALSE);

	/* Require identical "broken" status */
	if ((!o_ptr->cost) != (!j_ptr->cost)) return (FALSE);

	/* The feeling is the same */
	if (o_ptr->feeling != j_ptr->feeling) return (FALSE);

	/* The a_idx is the same */
	if (o_ptr->a_idx != j_ptr->a_idx) return (FALSE);

	/* The info is the same */
	if (o_ptr->info != j_ptr->info) return (FALSE);

	/* Require matching scripts */
	for (i = 0; i < MAX_TRIGGER; i++)
		if (o_ptr->trigger[i] != j_ptr->trigger[i])
			return (FALSE);

	/* The objects are the same */
	return (TRUE);
}


/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_kind(int tval, int sval)
{
	int k;
	int num = 0;
	int bk = 0;

	/* Look for it */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Require correct tval */
		if (k_ptr->tval != tval) continue;

		/* Found a match */
		if (k_ptr->sval == sval) return (k);

		/* Ignore illegal items */
		if (sval != SV_ANY) continue;

		/* Apply the randomizer */
		if (!one_in_(++num)) continue;

		/* Use this value */
		bk = k;
	}

	/* Return this choice */
	if (sval == SV_ANY)
	{
		return bk;
	}

	/* Oops */
	msgf("No object (%d,%d)", tval, sval);

	/* Oops */
	return (0);
}


/*
 * Wipe an object clean.
 */
void object_wipe(object_type *o_ptr)
{
	/* Delete the references */
	delete_static_object(o_ptr);

	/* Wipe the structure */
	(void)WIPE(o_ptr, object_type);
}

/*
 * Duplicate an object, and have the copy stored in the
 * standard static temp object.
 */
object_type *object_dup(const object_type *o_ptr)
{
	int i;
	
	object_type *q_ptr = &temp_object;

	/* Delete old static object */
	delete_static_object(q_ptr);

	/* Copy it */
	object_copy(q_ptr, o_ptr);

	/* Allocate quarks */
	quark_dup(o_ptr->xtra_name);
	quark_dup(o_ptr->inscription);

	for (i = 0; i < MAX_TRIGGER; i++)
		quark_dup(o_ptr->trigger[i]);

	/* Return a pointer to the static object */
	return (q_ptr);
}


/*
 * Prepare an object based on an object kind.
 */
object_type *object_prep(int k_idx)
{
	object_type *o_ptr = &temp_object;

	object_kind *k_ptr = &k_info[k_idx];

	/* Clear the record */
	object_wipe(o_ptr);

	/* Save the kind index */
	o_ptr->k_idx = k_idx;

	/* Efficiency -- tval/sval */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;

	/* Default "pval" */
	o_ptr->pval = k_ptr->pval;

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

	/* Set cost */
	o_ptr->cost = k_ptr->cost;

	/* Save the flags */
	o_ptr->flags[0] = k_ptr->flags[0];
	o_ptr->flags[1] = k_ptr->flags[1];
	o_ptr->flags[2] = k_ptr->flags[2];
	o_ptr->flags[3] = k_ptr->flags[3];

#if 0
	/* 
	 * Don't add the triggers - apply_object_trigger looks up the object
	 * kind's trigger if the specific object has none
	 */
	for (i = 0; i < MAX_TRIGGER; i++)
	{
		if (k_ptr->trigger[i])
		{
			o_ptr->trigger[i] = 
				quark_add(k_text + k_ptr->trigger[i]);
		}
	}
#endif

	return (o_ptr);
}


/*
 * Help determine an "enchantment bonus" for an object.
 *
 * To avoid floating point but still provide a smooth distribution of bonuses,
 * we simply round the results of division in such a way as to "average" the
 * correct floating point value.
 *
 * This function has been changed.  It uses "Rand_mormal()" to choose values from
 * a normal distribution, whose mean moves from zero towards the max as the
 * level increases, and whose standard deviation is equal to 1/4 of the max,
 * and whose values are forced to lie between zero and the max, inclusive.
 *
 * Since the "level" rarely passes 100 before Morgoth is dead, it is very
 * rare to get the "full" enchantment on an object, even at deep levels.
 *
 * It is always possible (albeit unlikely) to get the "full" enchantment.
 *
 * A sample distribution of values from "m_bonus(10, N)" is shown below:
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
s16b m_bonus(int max, int level)
{
	int bonus, stand, extra, value;


	/* Paranoia -- enforce maximal "level" */
	if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;


	/* The "bonus" moves towards the max */
	bonus = ((max * level) / MAX_DEPTH);

	/* Hack -- determine fraction of error */
	extra = ((max * level) % MAX_DEPTH);

	/* Hack -- simulate floating point computations */
	if (randint0(MAX_DEPTH) < extra) bonus++;


	/* The "stand" is equal to one quarter of the max */
	stand = (max / 4);

	/* Hack -- determine fraction of error */
	extra = (max % 4);

	/* Hack -- simulate floating point computations */
	if (randint0(4) < extra) stand++;


	/* Choose an "interesting" value */
	value = Rand_normal(bonus, stand);

	/* Enforce the minimum value */
	if (value < 0) return (0);

	/* Enforce the maximum value */
	if (value > max) return (max);

	/* Result */
	return (value);
}


/*
 * Weighted bonus
 *
 * This function creates a "wieghted bonus" that
 * depends on the difference between the "normal"
 * level of an object, and the current level.
 * [O] based combat makes the extra plusses not
 * quite as important as in the old combat system.
 *
 * A 4d4 weapon with negative multipliers often is
 * much better than a 1d4 weapon with high positive
 * modifiers.
 *
 * This function attempts to balance that effect by
 * making things that are normally "junk" more powerful,
 * and things that are too powerful, weak.
 */
static s16b w_bonus(int max, int lev_dif)
{
	/* Paranoia - max must be greater than 5 */
	if (max < 6) return (0);

	/* Level difference is too small? */
	if (ABS(lev_dif) < 10) return (0);

	if (lev_dif < 0)
	{
		/* Negative bonus */
		return (-m_bonus(max - 5, lev_dif));

	}
	else
	{
		/* Positive bonus */
		return (randint1(5) + m_bonus(max - 5, lev_dif * 3));
	}
}


/*
 * Cheat -- describe a created object for the user
 */
static void object_mention(object_type *o_ptr)
{
	cptr type;

	/* Artifact */
	if (FLAG(o_ptr, TR_INSTA_ART))
	{
		if (o_ptr->a_idx)
		{
			/* Silly message */
			type = "Artifact (";
		}
		else
		{
			/* Silly message */
			type = "Random artifact (";
		}
	}

	/* Ego-item */
	else if (ego_item_p(o_ptr))
	{
		/* Silly message */
		type = "Ego-item (";
	}

	/* Normal item */
	else
	{
		/* Silly message */
		type = "Object (";
	}
	
	msgf("%s%v)", type, OBJECT_STORE_FMT(o_ptr, FALSE, 0));
}


/* Select ego items for the required slot */
static bool get_ego_prep(byte slot, bool good)
{
	int i;
	ego_item_type *e_ptr;

	bool match = FALSE;

	alloc_entry *table = alloc_ego_table;

	/* Scan the allocation table */
	for (i = 0; i < alloc_ego_size; i++)
	{
		/* Get pointer to ego item type */
		e_ptr = &e_info[table[i].index];

		/* Keep matching items */
		if ((e_ptr->slot == slot) &&
			((good && e_ptr->rating) || (!good && !e_ptr->rating)))
		{
			/* Accept this ego item */
			table[i].prob2 = table[i].prob1;

			/* There is a matching type */
			match = TRUE;
		}
		else
		{
			/* Reject this ego item */
			table[i].prob2 = 0;
		}
	}

	/* Was there a matching item? */
	return (match);
}


/*
 * Get an ego item appropriate to a level.
 *
 * The "standard" method of using an allocation table is used to
 * make the selection.
 */
static byte get_ego_num(int level)
{
	int i;

	long total, value;

	alloc_entry *table = alloc_ego_table;


	/* Boost the level from time to time */
	if (one_in_(EGO_INFLATE))
	{
		/* What a bizzare calculation */
		level = 1 + (level * MAX_DEPTH / randint1(MAX_DEPTH));
	}

	total = 0L;

	/* Process the probabilities */
	for (i = 0; i < alloc_ego_size; i++)
	{
		/* Ego items are _not_ sorted by depth (yet) */
		if (table[i].level <= level)
		{
			total += table[i].prob2;
		}
	}

	/* No legal ego items? */
	if (total <= 0L) return (0);


	/* Pick an ego item */
	value = randint1(total);

	/* Find the ego item */
	for (i = 0; i < alloc_ego_size; i++)
	{
		if (table[i].level <= level)
		{
			value -= table[i].prob2;

			/* Match? */
			if (value <= 0L) break;
		}
	}

	/* Result */
	return ((byte)(table[i].index));
}


static void init_ego_item(object_type *o_ptr, byte ego)
{
	ego_item_type *e_ptr = &e_info[ego];

	/* Hack -- apply extra penalties if needed */
	if (cursed_p(o_ptr) || !o_ptr->cost)
	{
		/* Hack -- obtain bonuses */
		if (e_ptr->max_to_h) o_ptr->to_h -= randint1(abs(e_ptr->max_to_h));
		if (e_ptr->max_to_d) o_ptr->to_d -= randint1(abs(e_ptr->max_to_d));
		if (e_ptr->max_to_a) o_ptr->to_a -= randint1(abs(e_ptr->max_to_a));

		/* Hack -- obtain pval */
		if (e_ptr->max_pval) o_ptr->pval -= randint1(abs(e_ptr->max_pval));
	}

	/* Hack -- apply extra bonuses if needed */
	else
	{
		/* Hack -- obtain bonuses */
		if (e_ptr->max_to_h > 0) 
			o_ptr->to_h += randint1(e_ptr->max_to_h);
		else if (e_ptr->max_to_h < 0) 
			o_ptr->to_h -= randint1(-e_ptr->max_to_h);

		if (e_ptr->max_to_d > 0) 
			o_ptr->to_d += randint1(e_ptr->max_to_d);
		else if (e_ptr->max_to_d < 0) 
			o_ptr->to_d -= randint1(-e_ptr->max_to_d);

		if (e_ptr->max_to_a > 0) 
			o_ptr->to_a += randint1(e_ptr->max_to_a);
		else if (e_ptr->max_to_a < 0) 
			o_ptr->to_a -= randint1(-e_ptr->max_to_a);

		/* Hack -- obtain pval */
		if ((e_ptr->max_pval) && ((!o_ptr->pval) || k_info[o_ptr->k_idx].pval))
		{
			/*
			 * Add the ego pval only if object has no pval, or normally
			 * has a pval - in which case the bonus should be added.
			 * (Eg with diggers)
			 */
			if (e_ptr->max_pval > 0)
				o_ptr->pval += randint1(e_ptr->max_pval);
			else
				o_ptr->pval -= randint1(-e_ptr->max_pval);
		}
	}

	/* Hack -- apply rating bonus */
	inc_rating(e_ptr->rating);

	/* Cheat -- describe the item */
	if (cheat_peek) object_mention(o_ptr);
}


/*
 * Turn an item into an ego item
 */
void add_ego_flags(object_type *o_ptr, byte ego)
{
	int i;

	ego_item_type *e_ptr = &e_info[ego];

	/* Set the flags */
	o_ptr->flags[0] |= e_ptr->flags[0];
	o_ptr->flags[1] |= e_ptr->flags[1];
	o_ptr->flags[2] |= e_ptr->flags[2];
	o_ptr->flags[3] |= e_ptr->flags[3];

	/* Save all the known ego flags */
	o_ptr->kn_flags[0] = e_ptr->flags[0];
	o_ptr->kn_flags[1] = e_ptr->flags[1];
	o_ptr->kn_flags[2] = e_ptr->flags[2];
	o_ptr->kn_flags[3] = e_ptr->flags[3];

	/* Save the inscription */
	o_ptr->xtra_name = quark_add(e_name + e_ptr->name);

	/* Add any special scripts */
	for (i = 0; i < MAX_TRIGGER; i++)
	{
		if (e_ptr->trigger[i])
			o_ptr->trigger[i] = quark_add(e_text + e_ptr->trigger[i]);
	}

	/* Add in cost of ego item */
	o_ptr->cost = k_info[o_ptr->k_idx].cost + e_ptr->cost;
	
	/* Lose information */
	o_ptr->info &= ~(OB_MENTAL | OB_KNOWN);
}


/*
 * Mega-Hack -- Attempt to create an artifact.
 */
static object_type *make_artifact(void)
{
	int i;
	int k_idx = 0;

	object_type *o_ptr;

	/* Check the artifact list */
	for (i = 1; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		/* No quest items */
		if (FLAG(a_ptr, TR_QUESTITEM)) continue;

		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > p_ptr->depth)
		{
			/* Acquire the "out-of-depth factor" */
			int d = (a_ptr->level - p_ptr->depth) * 2;

			/* Roll for out-of-depth creation */
			if (!one_in_(d)) continue;
		}

		/* Artifact "rarity roll" */
		if (!one_in_(a_ptr->rarity)) continue;

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* XXX XXX Enforce minimum "object" level (loosely) */
		if (!(FLAG(&k_info[k_idx], TR_INSTA_ART)) &&
				k_info[k_idx].level > base_level())
		{
			/* Acquire the "out-of-depth factor" */
			int d = (k_info[k_idx].level - base_level()) * 5;

			/* Roll for out-of-depth creation */
			if (!one_in_(d)) continue;
		}

		/* Assign the template */
		o_ptr = object_prep(k_idx);

		/* Save the artifact flags */
		o_ptr->flags[0] |= a_ptr->flags[0];
		o_ptr->flags[1] |= a_ptr->flags[1];
		o_ptr->flags[2] |= a_ptr->flags[2];
		o_ptr->flags[3] |= a_ptr->flags[3];

		/* Set the fields */
		o_ptr->pval = a_ptr->pval;
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;
		o_ptr->to_a = a_ptr->to_a;
		o_ptr->to_h = a_ptr->to_h;
		o_ptr->to_d = a_ptr->to_d;
		o_ptr->weight = a_ptr->weight;

		/* Mega-Hack XXX XXX -- set activation */
		o_ptr->a_idx = i;

		/* Add any special scripts */
		for (i = 0; i < MAX_TRIGGER; i++)
		{
			if (a_ptr->trigger[i])
				o_ptr->trigger[i] = quark_add(a_text + a_ptr->trigger[i]);
		}
		
		/* Do not make another one */
		a_ptr->cur_num = 1;

		/* Save the inscription */
		o_ptr->xtra_name = quark_add(a_name + a_ptr->name);

		/* Apply special scripts */
		apply_object_trigger(TRIGGER_MAKE, o_ptr, "i", "lev",
				a_ptr->level);

		/* Hack - increase the level rating */
		inc_rating(30);

		if (!a_ptr->cost)
		{
			/* Hack -- "worthless" artifacts */
			o_ptr->cost = 0L;
		}
		else
		{
			/* Hack - use the artifact price */
			o_ptr->cost = k_info[o_ptr->k_idx].cost + a_ptr->cost;
		}

		/* Cheat -- peek at the item */
		if (cheat_peek) object_mention(o_ptr);

		/* Set the good item flag */
		set_special();

		/* Success */
		return (o_ptr);
	}

	/* Failure */
	return (NULL);
}

/*
 * Mega-Hack -- Attempt to create a "special" randart.
 */
static object_type *make_special_randart(void)
{
	int i;
	int k_idx = 0;

	object_type *o_ptr;

	/* Check the artifact list */
	for (i = 1; i <= MAX_ART_SPECIAL; i++)
	{
		int lev;
		
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Skip "forbidden" artifacts */
		if (!k_info[k_idx].extra) continue;
		
		/* Enforce minimum "object" level (loosely) */
		if (k_info[k_idx].level > base_level())
		{
			/* Acquire the "out-of-depth factor" */
			int d = (k_info[k_idx].level - base_level()) * 2;

			/* Roll for out-of-depth creation */
			if (!one_in_(d)) continue;
		}

		/* Enforce "object rarity" */
		if (!one_in_(k_info[k_idx].extra)) continue;

		/* Assign the template */
		o_ptr = object_prep(k_idx);

		/* Use the object kind's depth */
		lev = k_info[k_idx].level;

		/* Create using the object kind's depth */
		create_artifact(o_ptr, lev, FALSE);

		/* Run special scripts */
		apply_object_trigger(TRIGGER_MAKE, o_ptr, "i", LUA_VAR(lev));

		return (o_ptr);
	}

	return (NULL);
}

/*
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base damage dice boosting
 * Hack -- note special processing for weapon/digger
 * Hack -- note special rating boost for dragon scale mail
 */
static void a_m_aux_1(object_type *o_ptr, int level, int lev_dif, byte flags)
{
	int tohit1 = w_bonus(10, lev_dif);
	int todam1 = w_bonus(10, lev_dif);

	int tohit2 = m_bonus(10, level);
	int todam2 = m_bonus(10, level);

	byte ego = 0;

	/* Enchant */
	o_ptr->to_h += tohit1;
	o_ptr->to_d += todam1;


	/* Good */
	if (flags & OC_FORCE_GOOD)
	{
		/* Enchant again */
		o_ptr->to_h += tohit2;
		o_ptr->to_d += todam2;
	}

	/* Bad */
	else if (flags & OC_FORCE_BAD)
	{
		/* Penalize again */
		o_ptr->to_h -= tohit2 * 2;
		o_ptr->to_d -= todam2 * 2;
	}


	/* Analyze type */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		{
			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for ego item */
				if (get_ego_prep(ES_DIG, TRUE))
				{
					ego = get_ego_num(level);
				}
			}

			/* Very bad */
			else if (flags & OC_FORCE_BAD)
			{
				/* Hack -- Horrible digging bonus */
				o_ptr->pval = 0 - (s16b)(rand_range(2, 7));
			}

			break;
		}


		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/* Elfblades are always special */
			if (o_ptr->sval == SV_ELFBLADE)
			{
				char new_name[1024];

				(void)create_artifact(o_ptr, level, FALSE);

				/* Hack - always use name made of random syllables */
				quark_remove(&o_ptr->xtra_name);
				get_table_name(new_name, TRUE);
				o_ptr->xtra_name = quark_add(new_name);

				break;
			}

			/* Very Good */
			else if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random artifact */
				if (one_in_(40))
				{
					(void)create_artifact(o_ptr, level, FALSE);

					break;
				}

				/* Roll for an ego-item */
				if (get_ego_prep(ES_WIELD, TRUE))
				{
					ego = get_ego_num(level);
				}

				/* Only sharp weapons can have sharpness */
				if (ego == EGO_SHARPNESS && o_ptr->tval != TV_SWORD)
					ego = 0;

				/* Only hafted weapons can have earthquakes */
				if (ego == EGO_EARTHQUAKES && o_ptr->tval != TV_HAFTED)
					ego = 0;

				/* Hack -- Super-charge the damage dice */
				if (ego)
				{
					if (one_in_(10L * o_ptr->dd * o_ptr->ds))
					{
						o_ptr->ds += (o_ptr->ds * randint1(5)) / 5;
					}
				}
			}

			/* Very cursed */
			else if (flags & OC_FORCE_BAD)
			{
				/* Roll for an ego-item */
				if (get_ego_prep(ES_WIELD, FALSE))
				{
					ego = get_ego_num(level);

					/* Extra powers */
					if (ego == EGO_MORGUL)
					{
						SET_FLAG(o_ptr, TR_TY_CURSE);
					}
				}
			}

			break;
		}


		case TV_BOW:
		{
			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random artifact */
				if (one_in_(21))
				{
					(void)create_artifact(o_ptr, level, FALSE);

					break;
				}

				/* Roll for ego-item */
				if (get_ego_prep(ES_BOW, TRUE))
				{
					ego = get_ego_num(level);
				}
			}

			break;
		}


		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for ego-item */
				if (get_ego_prep(ES_AMMO, TRUE))
				{
					ego = get_ego_num(level);
				}

				/* Hack -- super-charge the damage dice */
				if (one_in_(10L * o_ptr->dd * o_ptr->ds))
				{
					o_ptr->ds += (o_ptr->ds * randint1(5)) / 5;
				}
			}

			/* Very cursed */
			else if (flags & OC_FORCE_BAD)
			{
				/* Roll for ego-item */
				if (get_ego_prep(ES_AMMO, FALSE))
				{
					ego = get_ego_num(level);
				}
			}

			break;
		}
	}

	/* Add ego item powers */
	if (ego)
	{
		add_ego_flags(o_ptr, ego);
		init_ego_item(o_ptr, ego);
	}


	/* Cursed some of the time (only wearable items) */
	if ((randint0(100) < 15) && (flags & OC_NORMAL))
	{
		SET_FLAG(o_ptr, TR_CURSED);
	}

	/* Run any special scripts */
	apply_object_trigger(TRIGGER_MAKE, o_ptr, "i", LUA_VAR(level));
}


static void dragon_resist(object_type *o_ptr)
{
	do
	{
		add_ego_power(EGO_XTRA_ANY_RESIST, o_ptr);
	}
	while (one_in_(2));
}


/*
 * Apply magic to an item known to be "armor"
 *
 * Hack -- note special processing for crown/helm
 * Hack -- note special processing for robe of permanence
 */
static void a_m_aux_2(object_type *o_ptr, int level, int lev_dif, byte flags)
{
	int toac1 = w_bonus(10, lev_dif);

	int toac2 = m_bonus(10, level);

	byte ego = 0;

	/* Hack - some items have an increased chance of being great */
	if ((((o_ptr->tval == TV_HELM) && (o_ptr->sval == SV_DRAGON_HELM)) ||
		 ((o_ptr->tval == TV_SHIELD) && (o_ptr->sval == SV_DRAGON_SHIELD)) ||
		 ((o_ptr->tval == TV_CLOAK) && (o_ptr->sval == SV_ELVEN_CLOAK))) &&
		lev_dif > 30)
	{
		/* Not cursed */
		flags |= OC_FORCE_GOOD;
		flags &= ~OC_FORCE_BAD;
	}


	/* Enchant */
	o_ptr->to_a += toac1;

	/* Good */
	if (flags & OC_FORCE_GOOD)
	{
		/* Enchant again */
		o_ptr->to_a += toac2;
	}

	/* Bad */
	else if (flags & OC_FORCE_BAD)
	{
		/* Penalize again */
		o_ptr->to_a -= toac2 * 2;
	}

	/* Analyze type */
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		{
			/* Rating boost */
			inc_rating(30);

			/* Mention the item */
			if (cheat_peek) object_mention(o_ptr);

			break;
		}

		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random artifact */
				if (one_in_(21))
				{
					(void)create_artifact(o_ptr, level, FALSE);

					break;
				}

				/* Roll for an ego item */
				if (get_ego_prep(ES_BODY, TRUE))
				{
					ego = get_ego_num(level);
				}

				/* Only robes can have permanence */
				if (ego == EGO_PERMANENCE && (o_ptr->tval != TV_SOFT_ARMOR ||
							o_ptr->sval != SV_ROBE))
					ego = 0;
			}

			break;
		}

		case TV_SHIELD:
		{

			/* Dragon shields are already good */
			if (o_ptr->sval == SV_DRAGON_SHIELD)
			{
				/* Rating boost */
				inc_rating(5);

				/* Mention the item */
				if (cheat_peek) object_mention(o_ptr);
				dragon_resist(o_ptr);
			}
			else
			{
				/* Very good */
				if (flags & OC_FORCE_GOOD)
				{
					/* Roll for random artifact */
					if (one_in_(21))
					{
						(void)create_artifact(o_ptr, level, FALSE);

						break;
					}

					/* Roll for ego-item */
					if (get_ego_prep(ES_ARM, TRUE))
					{
						ego = get_ego_num(level);
					}
				}
			}
			break;
		}

		case TV_GLOVES:
		{
			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random artifact */
				if (one_in_(20))
				{
					(void)create_artifact(o_ptr, level, FALSE);

					break;
				}

				/* Roll for ego-item */
				if (get_ego_prep(ES_HANDS, TRUE))
				{
					ego = get_ego_num(level);
				}
			}

			/* Very cursed */
			else if (flags & OC_FORCE_BAD)
			{
				/* Roll for ego-item */
				if (get_ego_prep(ES_HANDS, FALSE))
				{
					ego = get_ego_num(level);
				}
			}

			break;
		}

		case TV_BOOTS:
		{
			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random artifact */
				if (one_in_(20))
				{
					(void)create_artifact(o_ptr, level, FALSE);

					break;
				}

				/* Roll for ego-item */
				if (get_ego_prep(ES_FEET, TRUE))
				{
					ego = get_ego_num(level);
				}
			}

			/* Very cursed */
			else if (flags & OC_FORCE_BAD)
			{
				/* Roll for ego-item */
				if (get_ego_prep(ES_FEET, FALSE))
				{
					ego = get_ego_num(level);
				}
			}

			break;
		}

		case TV_CROWN:
		{
			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random artifact */
				if (one_in_(20))
				{
					(void)create_artifact(o_ptr, level, FALSE);

					break;
				}

				/* Roll for ego-item */
				if (get_ego_prep(ES_CROWN, TRUE))
				{
					ego = get_ego_num(level);
				}
			}

			/* Very cursed */
			else if (flags & OC_FORCE_BAD)
			{
				/* Roll for ego-item */
				if (get_ego_prep(ES_HEAD, FALSE))
				{
					ego = get_ego_num(level);
				}
			}

			break;
		}

		case TV_HELM:
		{
			if (o_ptr->sval == SV_DRAGON_HELM)
			{
				/* Rating boost */
				inc_rating(5);

				/* Mention the item */
				if (cheat_peek) object_mention(o_ptr);
				dragon_resist(o_ptr);
			}
			else
			{
				/* Very good */
				if (flags & OC_FORCE_GOOD)
				{
					/* Roll for a random artifacts */
					if (one_in_(20))
					{
						(void)create_artifact(o_ptr, level, FALSE);

						break;
					}

					/* Roll for ego-item */
					if (get_ego_prep(ES_HEAD, TRUE))
					{
						ego = get_ego_num(level);
					}
				}

				/* Very cursed */
				else if (flags & OC_FORCE_BAD)
				{
					/* Roll for ego-item */
					if (get_ego_prep(ES_HEAD, FALSE))
					{
						ego = get_ego_num(level);
					}
				}
			}
			break;
		}

		case TV_CLOAK:
		{
			if (o_ptr->sval == SV_ELVEN_CLOAK)
			{
				/* No cursed elven cloaks */
				o_ptr->pval = randint1(4);
			}

			/* Very good */
			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random artifact */
				if (one_in_(20))
				{
					(void)create_artifact(o_ptr, level, FALSE);

					break;
				}

				/* Roll for ego-item */
				if (get_ego_prep(ES_OUTER, TRUE))
				{
					ego = get_ego_num(level);
				}
			}

			/* Very cursed */
			else if (flags & OC_FORCE_BAD)
			{

				/* Roll for ego-item */
				if (get_ego_prep(ES_OUTER, TRUE))
				{
					ego = get_ego_num(level);
				}
			}

			break;
		}
	}

	/* Add ego item powers */
	if (ego)
	{
		add_ego_flags(o_ptr, ego);
		init_ego_item(o_ptr, ego);
	}

	/* Cursed some of the time */
	if ((randint0(100) < 15) && (flags & OC_NORMAL))
	{
		SET_FLAG(o_ptr, TR_CURSED);
	}

	/* Run any special scripts */
	apply_object_trigger(TRIGGER_MAKE, o_ptr, "i", LUA_VAR(level));
}


/*
 * Apply magic to an item known to be a "ring" or "amulet"
 */
static void a_m_aux_3(object_type *o_ptr, int level, byte flags)
{
	bool allow_curse = FALSE;
	int rating_boost = 0;

	if (!(flags & OC_FORCE_GOOD) && one_in_(3))
	{
		/* Sometimes, the stuff can be bad */

		flags |= OC_FORCE_BAD;
	}

	/* Apply magic according to type */
	apply_object_trigger(TRIGGER_MAKE, o_ptr, "i:bi", LUA_VAR(level),
			LUA_RETURN(allow_curse), LUA_RETURN(rating_boost));

	/* Cursed? */
	if (allow_curse && (flags & OC_FORCE_BAD))
	{
		/* Broken */
		o_ptr->cost = 0;

		/* Cursed */
		SET_FLAG(o_ptr, TR_CURSED);

		/* Reverse bonuses */
		o_ptr->pval = 0 - o_ptr->pval;
		o_ptr->to_a = 0 - o_ptr->to_a;
		o_ptr->to_h = 0 - o_ptr->to_h;
		o_ptr->to_d = 0 - o_ptr->to_d;
	}
	else if (rating_boost)
	{
		/* Boost the rating */
		inc_rating(rating_boost);

		/* Mention the item */
		if (cheat_peek && rating_boost >= 10) object_mention(o_ptr);
	}
}


/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
static void a_m_aux_4(object_type *o_ptr, int level, byte flags)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	byte ego = 0;

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		case TV_LITE:
		{
			/* Hack -- Torches -- random fuel */
			if (o_ptr->sval == SV_LITE_TORCH)
			{
				if (o_ptr->pval > 0) o_ptr->timeout = randint1(o_ptr->pval);
			}

			/* Hack -- Lanterns -- random fuel */
			if (o_ptr->sval == SV_LITE_LANTERN)
			{
				if (o_ptr->pval > 0) o_ptr->timeout = randint1(o_ptr->pval);
			}

			/* Hack - remove pval */
			o_ptr->pval = 0;

			if (flags & OC_FORCE_GOOD)
			{
				/* Roll for a random ego */
				if (get_ego_prep(ES_LITE, TRUE))
				{
					ego = get_ego_num(level);

					/* Initialise the ego item, if one is picked */
					if (ego)
					{
						add_ego_flags(o_ptr, ego);
						init_ego_item(o_ptr, ego);
					}
				}
			}


			break;
		}

		case TV_WAND:
		{
			/*
			 * The wand or staff gets a number of initial charges equal
			 * to between 1/2 (+1) and the full object kind's pval. -LM-
			 */
			o_ptr->pval = k_ptr->pval / 2 + randint1((k_ptr->pval + 1) / 2);

			/* The number of "used" charges starts out as zero */
			o_ptr->ac = 0;
			break;
		}

		case TV_STAFF:
		{
			/*
			 * The wand or staff gets a number of initial charges equal
			 * to between 1/2 (+1) and the full object kind's pval. -LM-
			 */
			o_ptr->pval = k_ptr->pval / 2 + randint1((k_ptr->pval + 1) / 2);
			break;
		}

		case TV_ROD:
		{
			/* Transfer the pval. -LM- */
			o_ptr->pval = k_ptr->pval;
			break;
		}

		case TV_FIGURINE:
		{
			int i = 1;

			monster_race *r_ptr;

			/* Pick a random non-unique monster race */
			while (1)
			{
				i = randint1(z_info->r_max - 1);

				r_ptr = &r_info[i];

				/* Prefer less out-of-depth monsters */
				if ((level < r_ptr->level) &&
					!one_in_(r_ptr->level - level)) continue;

				/* Ignore dead monsters */
				if (!r_ptr->rarity) continue;

				/* No uniques */
				if (FLAG(r_ptr, RF_UNIQUE)) continue;

				break;
			}

			o_ptr->pval = i;

			if (cheat_peek)
			{
				msgf("Figurine of %s", mon_race_name(r_ptr));
			}

			break;
		}

		case TV_STATUE:
		{
			int i = 1;

			monster_race *r_ptr;

			/* Pick a random monster race */
			while (1)
			{
				i = randint1(z_info->r_max - 1);

				r_ptr = &r_info[i];

				/* Ignore dead monsters */
				if (!r_ptr->rarity) continue;

				break;
			}

			o_ptr->pval = i;

			if (cheat_peek)
			{
				msgf("Statue of %s", mon_race_name(r_ptr));
			}

			break;
		}

		case TV_CHEST:
		{
			byte obj_level = get_object_level(o_ptr);

			/* Hack -- skip ruined chests */
			if (obj_level <= 0) break;

			/* Hack -- pick a "difficulty" */
			o_ptr->pval = randint1(obj_level);

			/* Never exceed "difficulty" of 55 to 59 */
			if (o_ptr->pval > 55) o_ptr->pval = (byte)rand_range(55, 60);

			break;
		}
	}

	/* Run any special scripts */
	apply_object_trigger(TRIGGER_MAKE, o_ptr, "i", LUA_VAR(level));
}


void add_ego_power(int power, object_type *o_ptr)
{
	if (power == EGO_XTRA_ANY_RESIST)
	{
		if (one_in_(4))
			power = EGO_XTRA_LO_RESIST;
		else
			power = EGO_XTRA_HI_RESIST;
	}

	if (power == EGO_XTRA_POWER)
	{
		if (one_in_(2))
			power = EGO_XTRA_ABILITY;
		else
			power = EGO_XTRA_HI_RESIST;
	}

	switch (power)
	{
		case EGO_XTRA_ABILITY:
		{
			/* Choose an ability */
			switch (randint0(8))
			{
				case 0:
				{
					(o_ptr->flags[2]) |= (TR2_FEATHER);
					break;
				}
				case 1:
				{
					(o_ptr->flags[2]) |= (TR2_LITE);
					break;
				}
				case 2:
				{
					(o_ptr->flags[2]) |= (TR2_SEE_INVIS);
					break;
				}
				case 3:
				{
					(o_ptr->flags[2]) |= (TR2_TELEPATHY);
					break;
				}
				case 4:
				{
					(o_ptr->flags[2]) |= (TR2_SLOW_DIGEST);
					break;
				}
				case 5:
				{
					(o_ptr->flags[2]) |= (TR2_REGEN);
					break;
				}
				case 6:
				{
					(o_ptr->flags[1]) |= (TR1_FREE_ACT);
					break;
				}
				case 7:
				{
					(o_ptr->flags[1]) |= (TR1_HOLD_LIFE);
					break;
				}
			}

			break;
		}
		case EGO_XTRA_SUSTAIN:
		{
			/* Choose a sustain */
			switch (randint0(6))
			{
				case 0:
				{
					(o_ptr->flags[1]) |= (TR1_SUST_STR);
					break;
				}
				case 1:
				{
					(o_ptr->flags[1]) |= (TR1_SUST_INT);
					break;
				}
				case 2:
				{
					(o_ptr->flags[1]) |= (TR1_SUST_WIS);
					break;
				}
				case 3:
				{
					(o_ptr->flags[1]) |= (TR1_SUST_DEX);
					break;
				}
				case 4:
				{
					(o_ptr->flags[1]) |= (TR1_SUST_CON);
					break;
				}
				case 5:
				{
					(o_ptr->flags[1]) |= (TR1_SUST_CHR);
					break;
				}
			}

			break;
		}

		case EGO_XTRA_LO_RESIST:
		{
			/* Choose a low resistance */
			switch (randint0(4))
			{
				case 0:
				{
					(o_ptr->flags[1]) |= (TR1_RES_ACID);
					break;
				}
				case 1:
				{
					(o_ptr->flags[1]) |= (TR1_RES_ELEC);
					break;
				}
				case 2:
				{
					(o_ptr->flags[1]) |= (TR1_RES_FIRE);
					break;
				}
				case 3:
				{
					(o_ptr->flags[1]) |= (TR1_RES_COLD);
					break;
				}
			}

			break;
		}

		case EGO_XTRA_HI_RESIST:
		{
			/* Choose a power */
			switch (randint0(12))
			{
				case 0:
				{
					(o_ptr->flags[1]) |= (TR1_RES_BLIND);
					break;
				}
				case 1:
				{
					(o_ptr->flags[1]) |= (TR1_RES_CONF);
					break;
				}
				case 2:
				{
					(o_ptr->flags[1]) |= (TR1_RES_SOUND);
					break;
				}
				case 3:
				{
					(o_ptr->flags[1]) |= (TR1_RES_SHARDS);
					break;
				}
				case 4:
				{
					(o_ptr->flags[1]) |= (TR1_RES_NETHER);
					break;
				}
				case 5:
				{
					(o_ptr->flags[1]) |= (TR1_RES_NEXUS);
					break;
				}
				case 6:
				{
					(o_ptr->flags[1]) |= (TR1_RES_CHAOS);
					break;
				}
				case 7:
				{
					(o_ptr->flags[1]) |= (TR1_RES_DISEN);
					break;
				}
				case 8:
				{
					(o_ptr->flags[1]) |= (TR1_RES_POIS);
					break;
				}
				case 9:
				{
					(o_ptr->flags[1]) |= (TR1_RES_DARK);
					break;
				}
				case 10:
				{
					(o_ptr->flags[1]) |= (TR1_RES_LITE);
					break;
				}
				case 11:
				{
					(o_ptr->flags[1]) |= (TR1_RES_FEAR);
					break;
				}
			}

			break;
		}
	}
}


/*
 * Complete the "creation" of an object by applying "magic" to the item
 *
 * This includes not only rolling for random bonuses, but also putting the
 * finishing touches on ego-items and artifacts, giving charges to wands and
 * staffs, giving fuel to lites, and placing traps on chests.
 *
 * The base "chance" of the item being "good" increases with the "level"
 * parameter, which is usually derived from the dungeon level, being equal
 * to the level plus 10, up to a maximum of 75.  If "good" is true, then
 * the object is guaranteed to be "good".  If an object is "good", then
 * the chance that the object will be "great" (ego-item or artifact), also
 * increases with the "level", being equal to half the level, plus 5, up to
 * a maximum of 20.  If "great" is true, then the object is guaranteed to be
 * "great".  At dungeon level 65 and below, 15/100 objects are "great".
 *
 * If the object is not "good", there is a chance it will be "cursed", and
 * if it is "cursed", there is a chance it will be "broken".  These chances
 * are related to the "good" / "great" chances above.
 *
 * Otherwise "normal" rings and amulets will be "good" half the time and
 * "cursed" half the time, unless the ring/amulet is always good or cursed.
 *
 * If "okay" is true, and the object is going to be "great", then there is
 * a chance that an artifact will be created.  This is true even if both the
 * "good" and "great" arguments are false.  As a total hack, if "great" is
 * true, then the item gets 3 extra "attempts" to become an artifact.
 */
void apply_magic(object_type *o_ptr, int lev, int lev_dif, byte flags)
{
	int f;

	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

	/* Base chance of being "good" */
	f = (lev * 3) / 5 + 10;

	/* Maximal chance of being "good" */
	if (f > 42) f = 42;

	if (FLAG(p_ptr, TR_STRANGE_LUCK))
		f = f * 3 / 2;

	/* Lights are more likely to be ego-items */
	if (o_ptr->tval == TV_LITE) f *= 2;

	/* Roll for ego items */
	if ((flags & OC_NORMAL) && (randint0(100) < f))
	{
		if (randint0(100) < f) flags |= OC_FORCE_GOOD;
		else if (randint0(100) < f) flags |= OC_FORCE_BAD;
	}

	if (FLAG(o_ptr, TR_INSTA_ART))
	{
		/* Paranoia - we have an artifact!!! */
		msgf("Error Condition - artifact passed to apply_magic");
		msgf("Object sval:%d Object flags3:%d",
				   o_ptr->sval, o_ptr->flags[2]);
		msgf("Submit a bugreport please. :-)");
		return;
	}

	/* Apply magic */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOW:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			a_m_aux_1(o_ptr, lev, lev_dif, flags);
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
			a_m_aux_2(o_ptr, lev, lev_dif, flags);
			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			a_m_aux_3(o_ptr, lev, flags);
			break;
		}

		default:
		{
			a_m_aux_4(o_ptr, lev, flags);
			break;
		}
	}

	/* Change level feeling for random artifacts */
	if (FLAG(o_ptr, TR_INSTA_ART)) inc_rating(30);

	/* Examine real objects */
	if (o_ptr->k_idx)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Mega Hack - reset cost if not a powerful item */
		if (!o_ptr->xtra_name)
		{
			o_ptr->cost = k_ptr->cost;
		}
	}
}

/* The tval / sval pair to match */
static byte match_tv;
static byte match_sv;


void init_match_hook(byte tval, byte sval)
{
	/* Save the tval/ sval pair to match */
	match_tv = tval;
	match_sv = sval;
}


/*
 * Hack -- match certain types of object only.
 *
 * Return 0% or 100% of matching based on tval and sval.
 */
byte kind_is_match(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Does the tval match? */
	if ((match_tv != TV_ANY) && (k_ptr->tval != match_tv)) return (0);

	/* Does the sval match? */
	if ((match_sv == SV_ANY) || (k_ptr->sval == match_sv)) return (100);

	/* Not a match */
	return (0);
}


/* The themed objects to use */
static obj_theme match_theme;

void init_match_theme(obj_theme theme)
{
	/* Save the theme */
	match_theme = theme;
}

/*
 * Hack -- match certain types of object only.
 *
 * Return percentage probability of match.
 */
byte kind_is_theme(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Pick probability to use */
	switch (k_ptr->tval)
	{
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		{
			/* Degree of junk is defined in terms of the other 4 quantities */
			return (100 - (match_theme.treasure + match_theme.combat +
						   match_theme.magic + match_theme.tools));
		}
		case TV_SPIKE: return (match_theme.tools);
		case TV_CHEST: return (match_theme.treasure);
		case TV_FIGURINE: return (match_theme.treasure);
		case TV_STATUE: return (match_theme.treasure);
		case TV_SHOT: return (match_theme.combat);
		case TV_ARROW: return (match_theme.combat);
		case TV_BOLT: return (match_theme.combat);
		case TV_BOW: return (match_theme.combat);
		case TV_DIGGING: return (match_theme.tools);
		case TV_HAFTED: return (match_theme.combat);
		case TV_POLEARM: return (match_theme.combat);
		case TV_SWORD: return (match_theme.combat);
		case TV_BOOTS: return (match_theme.combat);
		case TV_GLOVES: return (match_theme.combat);
		case TV_HELM: return (match_theme.combat);
		case TV_CROWN: return (match_theme.treasure);
		case TV_SHIELD: return (match_theme.combat);
		case TV_CLOAK: return (match_theme.combat);
		case TV_SOFT_ARMOR: return (match_theme.combat);
		case TV_HARD_ARMOR: return (match_theme.combat);
		case TV_DRAG_ARMOR: return (match_theme.treasure +
					 match_theme.combat);
		case TV_LITE: return (match_theme.tools);
		case TV_AMULET: return (match_theme.treasure);
		case TV_RING: return (match_theme.treasure);
		case TV_STAFF: return (match_theme.magic);
		case TV_WAND: return (match_theme.magic);
		case TV_ROD: return (match_theme.magic);
		case TV_SCROLL: return (match_theme.magic);
		case TV_POTION: return (match_theme.magic);
		case TV_FLASK: return (match_theme.tools);
		case TV_FOOD: return (match_theme.tools);
		case TV_LIFE_BOOK: return (match_theme.magic);
		case TV_SORCERY_BOOK: return (match_theme.magic);
		case TV_NATURE_BOOK: return (match_theme.magic);
		case TV_CHAOS_BOOK: return (match_theme.magic);
		case TV_DEATH_BOOK: return (match_theme.magic);
		case TV_TRUMP_BOOK: return (match_theme.magic);
		case TV_ARCANE_BOOK: return (match_theme.magic);

			/* Paranoia */
		default: return (0);
	}
}


/*
 * Attempt to make an object (normal or good/great)
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "level" for the "generation level".
 *
 * We assume that the given object has been "wiped".
 */
object_type *make_object(int level, int delta_level, obj_theme *theme)
{
	int prob, base, min_level;
	byte obj_level;
	byte flags;
	int k_idx = 0, count = 5;

	object_type *o_ptr;

	/* Chance of "special object" */
	if (delta_level > 0)
	{
		prob = 800 / delta_level;

		/* bounds checking */
		if (prob < 10) prob = 10;
	}
	else
	{
		/* No divide by zero */
		prob = 800;
	}

	/* "Good Luck" mutation */
	if ((p_ptr->muta3 & MUT3_GOOD_LUCK) && one_in_(13))
	{
		/* The player is lucky - the item is better than normal */
		delta_level += 20;
	}
	
	/* Renormalise the change in level */
	delta_level = randint0(delta_level);

	/* Base level for the object */
	base = level + delta_level;

	/* Paranoia - don't let this get too high */
	if (base > 100) base = 100;

	/* Hack - Set flags based on delta_level */
	if (delta_level > 15)
	{
		flags = OC_FORCE_GOOD;

		min_level = level + delta_level / 2;
	}
	else
	{
		flags = OC_NORMAL;
		min_level = 0;
	}

	/* Make an artifact */
	if (one_in_(prob))
	{
		/* Try for a fixed art */
		o_ptr = make_artifact();

		if (o_ptr) return (o_ptr);
	}

	if (one_in_(prob * 2))
	{
		/* Try for a special randart */
		o_ptr = make_special_randart();

		if (o_ptr) return (o_ptr);
	}

	/* Default to themed objects? */
	if (theme)
	{
		while (!k_idx && (count > 0))
		{
			/* No infinite loops */
			count--;

			/* Select items based on "theme" */
			init_match_theme(*theme);

			/* Prepare allocation table */
			get_obj_num_prep(kind_is_theme);

			/* Pick a random object */
			k_idx = get_obj_num(base, min_level);

			/* Paranoia - try less hard to get something */
			if (!k_idx) min_level /= 2;
		}
	}
	else
	{
		/* We already have a restriction */
	
		/* Pick a random object using the current restriction */
		k_idx = get_obj_num(base, 0);
	}

	/* Handle failure */
	if (!k_idx) return (NULL);

	/* Prepare the object */
	o_ptr = object_prep(k_idx);

	/* Apply magic (allow artifacts) */
	apply_magic(o_ptr, base, base - k_info[k_idx].level, flags);

	/* Hack -- generate multiple spikes/missiles/ mushrooms */
	switch (o_ptr->tval)
	{
		case TV_SPIKE:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			o_ptr->number = (byte)damroll(6, 7);
			break;
		}

		case TV_FOOD:
		{
			if (o_ptr->sval < SV_FOOD_BISCUIT)
			{
				/* mushrooms appear in clumps */
				o_ptr->number = (byte)randint1(6);
			}
			break;
		}
	}

	obj_level = get_object_level(o_ptr);

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(o_ptr) && o_ptr->cost && (obj_level > p_ptr->depth))
	{
		/* Rating increase */
		inc_rating(obj_level - p_ptr->depth);

		/* Cheat -- peek at items */
		if (cheat_peek) object_mention(o_ptr);
	}

	/* Success */
	return (o_ptr);
}


/*
 * Put an object on the ground.
 * We assume the grid is in bounds.
 */
static bool put_object(object_type *o_ptr, int x, int y)
{
	/* Acquire grid */
	cave_type *c_ptr = area(x, y);

	object_type *j_ptr;

	/* Require nice floor space */
	if (!cave_nice_grid(c_ptr)) return (FALSE);

	/* Paranoia */
	if (!o_ptr) return (FALSE);

	/* Add the object to the ground */
	j_ptr = add_object_list(&c_ptr->o_idx, o_ptr);

	/* Success */
	if (j_ptr)
	{
		/* Location */
		j_ptr->iy = y;
		j_ptr->ix = x;

		/* Region */
		j_ptr->region = cur_region;

		/* Notice + Redraw */
		note_spot(x, y);
		
		/* Debug - scan player list for this item and complain if we find it */
		look_up_list(j_ptr);

		return (TRUE);
	}

	/* Warn the player */
	msgf("Failed to place object!");

	/* Paranoia - preserve artifacts */
	if ((preserve_mode) && FLAG(o_ptr, TR_INSTA_ART) &&
		o_ptr->a_idx)
	{
		a_info[o_ptr->a_idx].cur_num = 0;
	}

	/* Failure */
	return (FALSE);
}

/*
 * Put an object of the requested type on the location given.
 *
 * This is mostly used for creating items in quests.
 */
void place_specific_object(int x, int y, int level, int k_idx)
{
	object_type *o_ptr;
	object_kind *k_ptr;

	int i;

	/* Paranoia */
	if (!k_idx) return;

	k_ptr = &k_info[k_idx];

	/* Instant artifacts are special */
	if (FLAG(k_ptr, TR_INSTA_ART))
	{
		/* Find the "special" artifact this object belongs to */
		for (i = 1; i < z_info->a_max; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			/* Skip "empty" artifacts */
			if (!a_ptr->name) continue;

			if ((a_ptr->tval == k_ptr->tval) && (a_ptr->sval == k_ptr->sval))
			{
				/* Found it */
				create_named_art(i, x, y);
				return;
			}
		}

		/* Exit */
		return;
	}
	else
	{
		/* Create the item */
		o_ptr = object_prep(k_idx);

		/* Apply magic */
		apply_magic(o_ptr, level, 0, OC_NORMAL);

		/* Hack -- generate multiple spikes/missiles/ mushrooms */
		switch (o_ptr->tval)
		{
			case TV_SPIKE:
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			{
				o_ptr->number = (byte)damroll(6, 7);
				break;
			}

			case TV_FOOD:
			{
				if (o_ptr->sval < SV_FOOD_BISCUIT)
				{
					/* Mushrooms appear in clumps */
					o_ptr->number = (byte)randint1(6);
				}

				break;
			}
		}
	}

	/* Add the object to the ground */
	drop_near(o_ptr, -1, x, y);
}


/*
 * Attempt to place an object (normal or good/great) at the given location.
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "base_level()" + "delta_level" for the "generation level".
 *
 * This routine requires a clean floor grid destination.
 */
void place_object(int x, int y, bool good, bool great, int delta_level)
{
	cave_type *c_ptr;

	object_type *o_ptr;

	place_type *pl_ptr = &place[p_ptr->place_num];
	
	obj_theme *o_theme = &pl_ptr->dungeon->theme;

	/* Paranoia -- check bounds */
	if (!in_bounds2(x, y)) return;

	/* Acquire grid */
	c_ptr = area(x, y);

	/* Do not generate items on "nasty" terrain */
	if ((c_ptr->feat == FEAT_SHAL_LAVA) ||
		(c_ptr->feat == FEAT_SHAL_WATER) || (c_ptr->feat == FEAT_SHAL_ACID))
	{
		return;
	}
	
	/* Make an object (if possible) */
	o_ptr = make_object(base_level() + delta_level,
						(good ? 15 : 0) + (great ? 15 : 0), o_theme);

	/* Put it on the ground */
	(void)put_object(o_ptr, x, y);
}


/*
 * Make a treasure object
 */
object_type *make_gold(int level, int coin_type)
{
	s16b i;

	s32b base;

	object_type *o_ptr;

	if (coin_type)
	{
		/* Hack -- Creeping Coins only generate "themselves" */
		i = coin_type;
	}
	else
	{
		/* Hack -- Pick a Treasure variety */
		i = ((randint1(level + 2) + 2) / 2) - 1;

		/* Apply "extra" magic */
		if (one_in_(GREAT_OBJ))
		{
			i += randint1(level + 1);
		}
	}

	/* Do not create "illegal" Treasure Types */
	if (i >= MAX_GOLD) i = MAX_GOLD - 1;

	/* Prepare a gold object */
	o_ptr = object_prep(OBJ_GOLD_LIST + i);

	/* Hack -- Base coin cost */
	base = k_info[OBJ_GOLD_LIST + i].cost;

	/* Determine how much the treasure is "worth" */
	o_ptr->pval = (base + (8L * randint1(base)) + randint1(8));

	/* return pointer to gold */
	return (o_ptr);
}


/*
 * Places a treasure (Gold or Gems) at given location
 *
 * The location must be a legal, clean, floor grid.
 */
void place_gold(int x, int y)
{
	object_type *o_ptr;

	/* Make some gold */
	o_ptr = make_gold(base_level(), 0);

	/* Put it on the ground */
	(void)put_object(o_ptr, x, y);
}


/*
 * Let an object fall to the ground at or near a location.
 *
 * The initial location is assumed to be "in_bounds2()".
 *
 * This function takes a parameter "chance".  This is the percentage
 * chance that the item will "disappear" instead of drop.  If the object
 * has been thrown, then this is the chance of disappearance on contact.
 *
 * Hack -- this function uses "chance" to determine if it should produce
 * some form of "description" of the drop event (under the player).
 *
 * We check several locations to see if we can find a location at which
 * the object can combine, stack, or be placed.  Artifacts will try very
 * hard to be placed, including "teleporting" to a useful grid if needed.
 */
void drop_near(object_type *j_ptr, int chance, int x, int y)
{
	int i, k, d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;

	cave_type *c_ptr;
	object_type *o_ptr = NULL;

	char o_name[256];

	bool flag = FALSE;
	bool done = FALSE;

	bool plural = FALSE;


	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Describe object */
	object_desc(o_name, j_ptr, FALSE, 0, 256);


	/* Handle normal "breakage" */
	if (!(FLAG(j_ptr, TR_INSTA_ART)) && (randint0(100) < chance))
	{
		/* Message */
		msgf("The %s disappear%s.", o_name, (plural ? "" : "s"));

		/* Debug */
		if (p_ptr->state.wizard) msgf("(breakage)");

		/* Failure */
		return;
	}


	/* Score */
	bs = -1;

	/* Picker */
	bn = 0;

	/* Default */
	by = y;
	bx = x;

	/* Scan local grids */
	for (dy = -3; dy <= 3; dy++)
	{
		/* Scan local grids */
		for (dx = -3; dx <= 3; dx++)
		{
			bool comb = FALSE;

			/* Calculate actual distance */
			d = (dy * dy) + (dx * dx);

			/* Ignore distant grids */
			if (d > 10) continue;

			/* Location */
			ty = y + dy;
			tx = x + dx;

			/* Skip illegal grids */
			if (!in_bounds2(tx, ty)) continue;

			/* Require line of sight */
			if (!los(x, y, tx, ty)) continue;

			/* Obtain grid */
			c_ptr = area(tx, ty);

			/* Require floor space */
			if (!cave_nice_grid(c_ptr)) continue;

			/* Not on "nasty" terrains */
			if ((c_ptr->feat == FEAT_SHAL_LAVA) ||
				(c_ptr->feat == FEAT_SHAL_ACID) ||
				(c_ptr->feat == FEAT_SHAL_WATER)) continue;

			/* Check to see if fields dissallow placement */
			if (fields_have_flags(c_ptr, FIELD_INFO_NO_OBJCT))
			{
				continue;
			}

			/* No objects */
			k = 0;

			/* Scan objects in that grid */
			OBJ_ITT_START (c_ptr->o_idx, o_ptr)
			{
				/* Check for possible combination */
				if (object_similar(o_ptr, j_ptr)) comb = TRUE;

				/* Count objects */
				k++;
			}
			OBJ_ITT_END;

			/* Add new object */
			if (!comb) k++;

			/* No stacking (allow combining) [Optional for Topi] */
			if (!testing_stack && (k > 1)) continue;

			/* Paranoia */
			if (k > 99) continue;

			/* Calculate score */
			s = 1000 - (d + k * 5);

			/* Skip bad values */
			if (s < bs) continue;

			/* New best value */
			if (s > bs) bn = 0;

			/* Apply the randomizer to equivalent values */
			if ((++bn >= 2) && !one_in_(bn)) continue;

			/* Keep score */
			bs = s;

			/* Track it */
			by = ty;
			bx = tx;

			/* Okay */
			flag = TRUE;
		}
	}


	/* Handle lack of space */
	if (!flag && !(FLAG(j_ptr, TR_INSTA_ART)))
	{
		/* Message */
		msgf("The %s disappear%s.", o_name, (plural ? "" : "s"));

		/* Debug */
		if (p_ptr->state.wizard) msgf("(no floor space)");

		/* Failure */
		return;
	}

	/* Find a grid */
	for (i = 0; !flag; i++)
	{
		/* Bounce around */
		if (i < 1000)
		{
			ty = rand_spread(by, 1);
			tx = rand_spread(bx, 1);

			/* Do some bound checking */
			ty = MAX(ty, p_ptr->min_hgt);
			ty = MIN(ty, p_ptr->max_hgt - 1);
			tx = MAX(tx, p_ptr->min_wid);
			tx = MIN(tx, p_ptr->max_wid - 1);
		}

		/* Random locations */
		else
		{
			/* Pick a location */
			ty = rand_range(p_ptr->min_hgt, p_ptr->max_hgt - 1);
			tx = rand_range(p_ptr->min_wid, p_ptr->max_wid - 1);
		}

		/* Grid */
		c_ptr = area(tx, ty);

		/* Check to see if fields dissallow placement */
		if (fields_have_flags(c_ptr, FIELD_INFO_NO_OBJCT)) continue;

		/* Bounce to that location */
		by = ty;
		bx = tx;

		/* Require floor space */
		if (!cave_nice_grid(c_ptr)) continue;

		/* Not on "nasty" terrains */
		if ((c_ptr->feat == FEAT_SHAL_LAVA) ||
			(c_ptr->feat == FEAT_SHAL_ACID) ||
			(c_ptr->feat == FEAT_SHAL_WATER)) continue;

		/* Okay */
		flag = TRUE;
	}

	/* Grid */
	c_ptr = area(bx, by);

	/* Hack - artifacts will not be affected by terrain */
	if (!(FLAG(j_ptr, TR_INSTA_ART)))
	{
		/* Check to see if the object will burn on contact with lava. */
		if ((c_ptr->feat == FEAT_SHAL_LAVA) &&
			((j_ptr->tval == TV_STAFF) ||
			 (j_ptr->tval == TV_SCROLL) || (j_ptr->tval == TV_WAND)))
		{
			/* only display messages if player throws */
			if (!chance)
			{
				/* Message */
				msgf("The %s%s burns in the lava.",
						   o_name, (plural ? "" : "s"));
			}

			/* Debug */
			if (p_ptr->state.wizard) msgf("(contact with lava)");

			/* Failure */
			return;
		}

		/* Check to see if the object will disappear in water. */
		if ((c_ptr->feat == FEAT_SHAL_WATER) && (j_ptr->tval == TV_RING))
		{
			/* only display messages if player throws */
			if (!chance)
			{
				/* Message */
				msgf("The %s disappear%s.", o_name, (plural ? "" : "s"));
			}

			/* Debug */
			if (p_ptr->state.wizard) msgf("(contact with water)");

			/* Failure */
			return;
		}
	}

	/* Scan objects in that grid for combination */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Check for combination */
		if (object_similar(o_ptr, j_ptr))
		{
			/* Combine the items */
			object_absorb(o_ptr, j_ptr);

			/* Success */
			done = TRUE;

			/* Done */
			break;
		}
	}
	OBJ_ITT_END;

	/* Get new object */
	if (!done)
	{
		/* Put it on the ground */
		if (!put_object(j_ptr, bx, by))
		{
			/* Message */
			msgf("The %s disappear%s.", o_name, (plural ? "" : "s"));

			/* Debug */
			if (p_ptr->state.wizard) msgf("(too many objects)");

			/* Failure */
			return;
		}
	}

	/* Sound */
	sound(SOUND_DROP);

	/* Mega-Hack -- no message if "dropped" by player */
	/* Message when an object falls under the player */
	if (chance && (by == p_ptr->py) && (bx == p_ptr->px))
	{
		msgf("You feel something roll beneath your feet.");
	}

	/* Fields may interact with an object in some way */
	field_script(area(bx, by), FIELD_ACT_OBJECT_DROP, "p",
		 LUA_OBJECT(o_ptr));
}


/*
 * Scatter some "great" objects near the player
 */
void acquirement(int x1, int y1, int num, bool great, bool known)
{
	object_type *o_ptr = NULL;

	int i;

	obj_theme theme;

	/* Set theme - more weapons than normal */
	theme.treasure = 10;
	theme.combat = 80;
	theme.magic = 10;
	theme.tools = 0;

	/* Acquirement */
	while (num--)
	{
		/* We want a good object */
		for (i = 0; i < 1000; i++)
		{
			if (great)
			{
				/* Make a great object (if possible) */
				o_ptr = make_object(base_level(), 40, &theme);

				/* Paranoia */
				if (!o_ptr) continue;
			}
			else
			{
				/* Make a good object (if possible) */
				o_ptr = make_object(base_level(), 20, &theme);

				/* Paranoia */
				if (!o_ptr) continue;
            }

            /* Skip cursed items */
            if (cursed_p(o_ptr)) continue;

			/* Check to see if the object is worth anything */
			if (object_value_real(o_ptr) > 0) break;
		}

		/* Paranoia */
		if (i >= 1000) return;

		if (known)
		{
			object_aware(o_ptr);
			object_known(o_ptr);
		}

		/* Drop the object */
		drop_near(o_ptr, -1, x1, y1);
	}
}

/*
 * Look up the list that corresponds to a given
 * object returned by the get_item() function.
 *
 * We know the item is in our equipment, our
 * inventory, or is on the floor underneith us.
 */
s16b *look_up_list(object_type *o_ptr)
{
	object_type *j_ptr;

	cave_type *c_ptr;

	place_type *pl_ptr = &place[p_ptr->place_num];

	int i;

	/* Some objects have no list */
	if (!o_ptr->allocated) return (NULL);
	
	/* Scan player inventory */
	OBJ_ITT_START (p_ptr->inventory, j_ptr)
	{
		if (o_ptr == j_ptr) return (&p_ptr->inventory);
		
		/* Debug - make sure we don't have a corrupted inventory */
		if (j_ptr->ix || j_ptr->iy) quit("Corrupted inventory contains dungeon objects!");
		
		/* Debug - test array bounds */
		if (GET_ARRAY_INDEX(o_list, j_ptr) >= o_max) quit("Inven outside bounds!");
	}
	OBJ_ITT_END;

	/* Scan dungeon */
	if (o_ptr->ix || o_ptr->iy)
	{
		c_ptr = area(o_ptr->ix, o_ptr->iy);

		/* Scan square */
		OBJ_ITT_START (c_ptr->o_idx, j_ptr)
		{
			if (o_ptr == j_ptr) return (&c_ptr->o_idx);
		}
		OBJ_ITT_END;
	}

	/* Scan stores */
	for (i = 0; i < pl_ptr->numstores; i++)
	{
		/* Scan store stock */
		OBJ_ITT_START (pl_ptr->store[i].stock, j_ptr)
		{
			if (o_ptr == j_ptr) return (&pl_ptr->store[i].stock);
		}
		OBJ_ITT_END;
	}

	/* Failure - the object is inconsistant */
	quit("Failed to look up object.");
	return (NULL);
}

/*
 * Get the nth item in a list
 */
object_type *get_list_item(s16b list_start, int number)
{
	object_type *o_ptr;

	/* Paranoia */
	if (number < 0) return (NULL);

	OBJ_ITT_START (list_start, o_ptr)
	{
		if (!number) return (o_ptr);

		number--;
	}
	OBJ_ITT_END;

	/* We didn't find it */
	return (NULL);
}

/*
 * The inverse function of get_list_item()
 *
 * Find the position in the list of a given item
 */
int get_item_position(s16b list_start, object_type *o_ptr)
{
	int i = 0;

	object_type *j_ptr;

	OBJ_ITT_START (list_start, j_ptr)
	{
		if (j_ptr == o_ptr) return (i);

		i++;
	}
	OBJ_ITT_END;

	/* Failure */
	return (-1);
}

/*
 * How many items are in the list?
 */
int get_list_length(s16b list_start)
{
	int i = 0;

	object_type *j_ptr;

	OBJ_ITT_START (list_start, j_ptr)
	{
		/* Count items */
		i++;
	}
	OBJ_ITT_END;

	return (i);
}


/* Is the item on the floor? */
bool floor_item(object_type *o_ptr)
{
	s16b *o_list = look_up_list(o_ptr);

	cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

	/* On floor? */
	if (o_list == &c_ptr->o_idx) return (TRUE);

	/* Elsewhere */
	return (FALSE);
}

/* Is the item in the players inventory or equipment? */
bool player_item(object_type *o_ptr)
{
	s16b *o_list = look_up_list(o_ptr);

	/* Equipment? */
	if (!o_list) return (TRUE);

	/* Inventory */
	if (o_list == &p_ptr->inventory) return (TRUE);

	/* Elsewhere */
	return (TRUE);
}



/*
 * Describe the charges on an item in the inventory.
 */
void item_charges(object_type *o_ptr)
{
	/* Require staff/wand */
	if ((o_ptr->tval != TV_STAFF) && (o_ptr->tval != TV_WAND)) return;

	/* Require known item */
	if (!object_known_p(o_ptr)) return;

	/* Print a message */
	msgf("%s %d charge%s remaining.",
			   floor_item(o_ptr) ? "It has" : "You have", o_ptr->pval,
			   (o_ptr->pval != 1) ? "s" : "");
}


/*
 * Describe an item in the inventory.
 */
static cptr item_describe_aux(object_type *o_ptr, bool back_step)
{
	char o_name[256];
	char lab[40] = "";

	int item;

	/* Get list */
	s16b *list = look_up_list(o_ptr);

	cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

	/* Get a description */
	object_desc(o_name, o_ptr, TRUE, 3, 256);

	if (o_ptr->number <= 0)
	{
		if (!list)
		{
			/* Hack XXX XXX pretend there is one item */
			int num = o_ptr->number;
			o_ptr->number = 1;
			
			/* Get a (new) description */
			object_desc(o_name, o_ptr, TRUE, 3, 256);
			
			/* Item is in the equipment */
			item = GET_ARRAY_INDEX(p_ptr->equipment, o_ptr);
			
			/* No more items? */
			return (format("You were %s: %s (%c).", describe_use(item), o_name, I2A(item)));
			
			/* Restore old number of items */
			o_ptr->number = num;
		}
		else if (list == &p_ptr->inventory)
		{
			/* No more items? */
			return (format("There are %s.", o_name));
		}
		else if (list == &c_ptr->o_idx)
		{
			return (format("On the ground: %s.", o_name));
		}
	}
	else
	{
		if (!list)
		{
			/* Item is in the equipment */
			item = GET_ARRAY_INDEX(p_ptr->equipment, o_ptr);

			if (show_labels) strnfmt(lab, 40, "%^s: ", describe_use(item));

			return (format("%s%s (%c).", lab, o_name, I2A(item)));
		}
		else if (list == &p_ptr->inventory)
		{
			/* Get number of item in inventory */
			item = get_item_position(p_ptr->inventory, o_ptr);

			if (show_labels) strnfmt(lab, 40, "In your pack: ");

			/* Hack to get that letter correct in case a scroll disappears */
			if (back_step)
				return (format("%s%s (%c).", lab, o_name, I2A(item - 1)));
			else
				return (format("%s%s (%c).", lab, o_name, I2A(item)));
		}
		else if (list == &c_ptr->o_idx)
		{
			return (format("On the ground: %s.", o_name));
		}
		/* Then it is in the shop */
		else
		{
			if (show_labels) strnfmt(lab, 40, "In the shop: ");

			return (format("%s%s", lab, o_name));
		}
	}

	/* Missed it all */
	return (NULL);
}


/*
 * Describe an item in the inventory.
 */
void item_describe(object_type *o_ptr)
{
	msgf("%s", item_describe_aux(o_ptr, FALSE));
}


/*
 * Describe an item in the inventory.
 */
void item_describe_roff(object_type *o_ptr)
{
	roff("%s", item_describe_aux(o_ptr, FALSE));
}


/*
 * Describe an item in the inventory and pretend it is one slot lower than it
 * seems to be.  This is usefull when a the item is being identified by a scroll
 * of idenitfy and it was the last scroll of identify so there is some shuffling
 * in the inventory.
 *
 * Faux is for faux pas as this is a hack.
 */
void item_describe_faux(object_type *o_ptr)
{
	msgf("%s", item_describe_aux(o_ptr, TRUE));
}


/*
 * Erase an inventory slot if it has no more items
 */
static void item_optimize(object_type *o_ptr)
{
	s16b *list;

	/* Default to looking under the player */
	cave_type *c_ptr = area(p_ptr->px, p_ptr->py);
	
	/* The player could have moved due to a phase door scroll */
	if (in_bounds2(o_ptr->ix, o_ptr->iy))
	{
		c_ptr = area(o_ptr->ix, o_ptr->iy);
	}
	
	/* Only optimize real items */
	if (!o_ptr->k_idx) return;

	/* Only optimize empty items */
	if (o_ptr->number) return;

	/* Get list */
	list = look_up_list(o_ptr);

	/* The item is being wielded */
	if (!list)
	{
		/* Erase the empty slot */
		object_wipe(o_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate torch */
		p_ptr->update |= (PU_TORCH);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}
	else
	{
		/* Delete the object */
		if (list == &p_ptr->inventory)
		{
			/* Inventory item */
			delete_held_object(list, o_ptr);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);
		}
		else if (list == &c_ptr->o_idx)
		{
			/* Floor item */
			delete_dungeon_object(o_ptr);
		}
		else
		{
			/* Store item */
			delete_held_object(list, o_ptr);
		}
	}

	/* Window stuff */
	p_ptr->window |= (PW_SPELL);
}


/*
 * Split a pile into two bits.  Return a pointer to
 * the split off piece.  This piece will not be in the
 * original pile's list, but will be in the static
 * temp_object defined above. 
 */
object_type *item_split(object_type *o_ptr, int num)
{
	object_type *q_ptr;

	/* Paranoia */
	if (o_ptr->number < num) num = o_ptr->number;

	/* Duplicate the object */
	q_ptr = object_dup(o_ptr);
	
	/* Distribute charges of wands or rods */
	distribute_charges(o_ptr, q_ptr, num);

	/* Update item totals */
	o_ptr->number -= num;
	q_ptr->number = num;

	/* Notice the change */
	if (num && player_item(o_ptr))
	{
		/* Recalculate bonuses and weight */
		p_ptr->update |= (PU_BONUS | PU_WEIGHT);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Notice changes */
		notice_item();
	}

	/* Fill in holes... */
	item_optimize(o_ptr);

	/* Done - return new item */
	return (q_ptr);
}


/*
 * Increase the "number" of an item in the inventory
 */
static void item_increase_aux(object_type *o_ptr, int num, bool silent)
{
	/* Apply */
	num += o_ptr->number;

	/* Bounds check */
	if (num > 255) num = 255;
	else if (num < 0) num = 0;

	/* Un-apply */
	num -= o_ptr->number;

	/* Add the number */
	o_ptr->number += num;

	/* Notice the change */
	if (num && player_item(o_ptr))
	{
		/* Recalculate bonuses and weight */
		p_ptr->update |= (PU_BONUS | PU_WEIGHT);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		/* Notice changes */
		notice_item();
	}

	/* The shop upkeeping shouldn't be mentioned */
	if (!silent) item_describe(o_ptr);

	item_optimize(o_ptr);
}

/*
 * Increase the "number" of an item in the inventory
 */
void item_increase(object_type *o_ptr, int num)
{
	item_increase_aux(o_ptr, num, FALSE);
}

/*
 * Increase the "number" of an item in the inventory without a message
 */
void item_increase_silent(object_type *o_ptr, int num)
{
	item_increase_aux(o_ptr, num, TRUE);
}

/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(const object_type *o_ptr)
{
	object_type *j_ptr;

	/* Empty slot? */
	if (get_list_length(p_ptr->inventory) < INVEN_PACK) return (TRUE);

	/* Similar slot? */
	OBJ_ITT_START (p_ptr->inventory, j_ptr)
	{
		/* Check if the two items can be combined */
		if (object_similar(j_ptr, o_ptr)) return (TRUE);
	}
	OBJ_ITT_END;

	/* Nope */
	return (FALSE);
}

/*
 * Compare two items to see if they are in pack-order.
 */
static bool reorder_pack_comp(const object_type *o1_ptr,
                              const object_type *o2_ptr)
{
	/* Hack -- readable books always come first */
	if ((o1_ptr->tval == REALM1_BOOK) &&
		(o2_ptr->tval != REALM1_BOOK)) return (TRUE);
	if ((o2_ptr->tval == REALM1_BOOK) &&
		(o1_ptr->tval != REALM1_BOOK)) return (FALSE);


	if ((o1_ptr->tval == REALM2_BOOK) &&
		(o2_ptr->tval != REALM2_BOOK)) return (TRUE);
	if ((o2_ptr->tval == REALM2_BOOK) &&
		(o1_ptr->tval != REALM2_BOOK)) return (FALSE);


	/* Objects sort by decreasing type */
	if (o1_ptr->tval > o2_ptr->tval) return (TRUE);
	if (o1_ptr->tval < o2_ptr->tval) return (FALSE);

	/* Non-aware (flavored) items always come last */
	if (!object_aware_p(o2_ptr)) return (TRUE);
	if (!object_aware_p(o1_ptr)) return (FALSE);

	/* Objects sort by increasing sval */
	if (o1_ptr->sval < o2_ptr->sval) return (TRUE);
	if (o1_ptr->sval > o2_ptr->sval) return (FALSE);

	/* Unidentified objects always come last */
	if (!object_known_p(o2_ptr)) return (TRUE);
	if (!object_known_p(o1_ptr)) return (FALSE);

	/* Lites sort by increasing timeout */
	if (o1_ptr->tval == TV_LITE)
	{
		if (o1_ptr->sval < o2_ptr->sval) return (TRUE);
		if (o1_ptr->sval > o2_ptr->sval) return (FALSE);
	}

	/*
	 * Hack:  otherwise identical rods sort by
	 * increasing recharge time -DSB-
	 */
	if (o1_ptr->tval == TV_ROD)
	{
		if (o1_ptr->pval < o2_ptr->pval) return (TRUE);
		if (o1_ptr->pval > o2_ptr->pval) return (FALSE);
	}

	/* Objects sort by decreasing value */
	if (object_value(o1_ptr) >= object_value(o2_ptr)) return (TRUE);

	return (FALSE);
}

/*
 * Actually reorder the objects
 *
 * This uses a simple bubble sort.
 *
 * Usually we only need to make a few swaps.
 */
object_type *reorder_objects_aux(object_type *q_ptr, object_comp comp_func,
                                 u16b o_idx)
{
	object_type *o_ptr, *j_ptr;

	int i;

	/*
	 * Hack - Do this twice because we invert the order
	 * of 'similar' objects on the first pass
	 */
	for (i = 0; i < 2; i++)
	{
		/* Re-order the pack */
		OBJ_ITT_START (o_idx, o_ptr)
		{
			OBJ_ITT_START (o_ptr->next_o_idx, j_ptr)
			{
				/* Are they in the right order? */
				if (comp_func(j_ptr, o_ptr))
				{
					/* Are we moving the watched item? */
					if (q_ptr)
					{
						if (q_ptr == o_ptr) q_ptr = j_ptr;
						else if (q_ptr == j_ptr) q_ptr = o_ptr;
					}

					/* Swap the objects */
					swap_objects(o_ptr, j_ptr);
				}
			}
			OBJ_ITT_END;
		}
		OBJ_ITT_END;
	}

	return (q_ptr);
}


/*
 * Add an item to the players inventory, and return the slot used.
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
 * Note that this code must remove any location/stack information
 * from the object once it is placed into the inventory.
 */
object_type *inven_carry(object_type *o_ptr)
{
	object_type *j_ptr;

	/* Check for combining */
	OBJ_ITT_START (p_ptr->inventory, j_ptr)
	{
		/* Check if the two items can be combined */
		if (object_similar(j_ptr, o_ptr))
		{
			/* Combine the items */
			object_absorb(j_ptr, o_ptr);

			/* Recalculate bonuses and weight */
			p_ptr->update |= (PU_BONUS | PU_WEIGHT);

			/* Notice changes */
			notice_inven();

			/* Wipe old object */
			object_wipe(o_ptr);

			/* Success */
			return (j_ptr);
		}
	}
	OBJ_ITT_END;

	/* Add the item to the pack */
	o_ptr = add_object_list(&p_ptr->inventory, o_ptr);

	/* Paranoia */
	if (!o_ptr) return (NULL);

	/* Forget location */
	o_ptr->iy = o_ptr->ix = 0;

	/* Forget Region */
	o_ptr->region = 0;

	/* No longer marked */
	o_ptr->info &= ~(OB_SEEN);

	/* Reorder the pack */
	o_ptr = reorder_objects_aux(o_ptr, reorder_pack_comp, p_ptr->inventory);

	/* Recalculate bonuses and weight */
	p_ptr->update |= (PU_BONUS | PU_WEIGHT);

	/* Notice changes */
	notice_inven();

	/* Return the new item */
	return (o_ptr);
}


/*
 * Take off (some of) a non-cursed equipment item
 *
 * Note that only one item at a time can be wielded per slot.
 *
 * Note that taking off an item when "full" may cause that item
 * to fall to the ground.
 *
 * Return the inventory slot into which the item is placed.
 */
object_type *inven_takeoff(object_type *o_ptr)
{
	int item;

	object_type *q_ptr;

	cptr act;

	/* Look up item number */
	item = GET_ARRAY_INDEX(p_ptr->equipment, o_ptr);

	/* Took off weapon */
	if (item == EQUIP_WIELD)
	{
		act = "You were wielding";
	}

	/* Took off bow */
	else if (item == EQUIP_BOW)
	{
		act = "You were holding";
	}

	/* Took off light */
	else if (item == EQUIP_LITE)
	{
		act = "You were holding";
	}

	/* Took off something */
	else
	{
		act = "You were wearing";
	}
	
	/* Message */
	msgf("%s %v (%c).", act, OBJECT_FMT(o_ptr, TRUE, 3), I2A(item));

	/* Carry the object */
	q_ptr = inven_carry(o_ptr);

	/* Paranoia */
	if (!q_ptr)
	{
		msgf("You cannot take off the item - too many dungeon objects!");
		return (NULL);
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	p_ptr->redraw |= (PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_EQUIP);

	/* Return the item */
	return (q_ptr);
}


/*
 * Drop (some of) a non-cursed inventory/equipment item
 *
 * The object will be dropped "near" the current location
 */
void inven_drop(object_type *o_ptr, int amt)
{
	object_type *q_ptr;

	int slot;

	s16b *list;

	/* Error check */
	if (amt <= 0) return;

	/* Describe item */
	item_describe(o_ptr);

	/* Get list */
	list = look_up_list(o_ptr);

	/* Take off equipment */
	if (!list)
	{
		/* Take off first */
		o_ptr = inven_takeoff(o_ptr);

		/* Paranoia */
		if (!o_ptr) return;
	}

	/* Get item slot */
	slot = get_item_position(p_ptr->inventory, o_ptr);

	/* Get local object */
	q_ptr = item_split(o_ptr, amt);

	/* Message */
	msgf("You drop %v (%c).", OBJECT_FMT(q_ptr, TRUE, 3), I2A(slot));

	/* Drop it near the player */
	drop_near(q_ptr, 0, p_ptr->px, p_ptr->py);

	/* Update total weight */
	p_ptr->update |= PU_WEIGHT;
}


/*
 * Combine items in the pack
 */
static object_type *combine_pack_aux(object_type *q_ptr)
{
	object_type *o_ptr;
	object_type *j_ptr;
	bool flag = FALSE;
	
	/* Combine the pack */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Scan the items above that item */
		OBJ_ITT_START (o_ptr->next_o_idx, j_ptr)
		{
			/* Can we drop "o_ptr" onto "j_ptr"? */
			if (object_similar(o_ptr, j_ptr))
			{
				/* Take note */
				flag = TRUE;

				/* The original is about to disappear so assign to the new */
				if (o_ptr == q_ptr) q_ptr = j_ptr;

				/* Add together the item counts */
				object_absorb(j_ptr, o_ptr);

				/* Delete the item */
				delete_held_object(&p_ptr->inventory, o_ptr);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN);

				/* Done */
				break;
			}
		}
		OBJ_ITT_END;
	}
	OBJ_ITT_END;

	/* Message */
	if (flag) msgf("You combine some items in your pack.");

	return (q_ptr);
}


/*
 * Combine items in the pack
 */
void combine_pack(void)
{
	(void)combine_pack_aux(NULL);
}

/*
 * Combine items in the pack and keep track of an object
 */
object_type *combine_pack_watch(object_type *q_ptr)
{
	return (combine_pack_aux(q_ptr));
}

/*
 * Reorder items in the pack
 */
void reorder_pack(void)
{
	(void)reorder_objects_aux(NULL, reorder_pack_comp, p_ptr->inventory);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);
}


/*
 * Reorder items in the pack and return the watched object.
 */
object_type *reorder_pack_watch(object_type *o_ptr)
{
	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	return (reorder_objects_aux(o_ptr, reorder_pack_comp, p_ptr->inventory));
}


bool can_player_destroy_object(object_type *o_ptr)
{
	/*
	 * Artifacts cannot be destroyed
	 * However only notice if we have not *id'ed* it.
	 */
	if (FLAG(o_ptr, TR_INSTA_ART))
	{
		if (!(o_ptr->info & OB_MENTAL))
		{
			byte feel = FEEL_SPECIAL;
	
			/* Hack -- Handle icky artifacts */
			if (cursed_p(o_ptr) || !o_ptr->cost) feel = FEEL_TERRIBLE;

			/* Hack -- inscribe the artifact */
			o_ptr->feeling = feel;

			/* We have "felt" it (again) */
			o_ptr->info |= (OB_SENSE);

			/* Redraw equippy chars */
			p_ptr->redraw |= (PR_EQUIPPY);
			
			/* Notice changes */
			notice_item();
		}
		
		/* Done */
		return FALSE;
	}

	return TRUE;
}


/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readible books
 */
void display_koff(int k_idx)
{
	/* Get local object */
	object_type *q_ptr;

	/* Erase the window */
    clear_from(0);

	/* No info */
	if (!k_idx) return;

	/* Prepare the object */
	q_ptr = object_prep(k_idx);

	/* Mention the object name */
	prtf(0, 0, "%v", OBJECT_STORE_FMT(q_ptr, FALSE, 0));

	/* Warriors are illiterate */
	if (!(p_ptr->spell.r[0].realm || p_ptr->spell.r[1].realm)) return;

	/* Display spells in readible books */
	if ((q_ptr->tval == REALM1_BOOK) || (q_ptr->tval == REALM2_BOOK))
	{
		int sval;
		int spell = -1;
		int num = 0;
		byte spells[PY_MAX_SPELLS];


		/* Access the item's sval */
		sval = q_ptr->sval;

		/* Extract spells */
		for (spell = 0; spell < 32; spell++)
		{
			/* Check for this spell */
			if (fake_spell_flags[sval] & (1L << spell))
			{
				/* Collect this spell */
				spells[num++] = spell;
			}
		}

		/* Print spells */
		print_spells(spells, num, 0, 2,
					 ((q_ptr->tval == REALM1_BOOK) ?
					 	 p_ptr->spell.r[0].realm - 1 : p_ptr->spell.r[1].realm - 1));
	}
}
