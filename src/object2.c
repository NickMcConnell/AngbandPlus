#define OBJECT2_C
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



/*
 * Excise a dungeon object from any stacks
 */
void excise_dun_object(object_type *j_ptr)
{
	s16b this_o_idx, next_o_idx = 0;
	
	s16b prev_o_idx = 0;


	/* Index */
	int o_idx = j_ptr - o_list;

	/* Monster */
	if (j_ptr->held_m_idx)
	{
		monster_type *m_ptr;
		
		/* Monster */
		m_ptr = &m_list[j_ptr->held_m_idx];

		/* Scan all objects in the grid */
		for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;
		
			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
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

                                        object_type *k_ptr;

                                        /* Previous object */
                                        k_ptr = &o_list[prev_o_idx];

                                        /* Remove from list */
                                        k_ptr->next_o_idx = next_o_idx;
 
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
		cave_type *c_ptr;

		int y = j_ptr->iy;
		int x = j_ptr->ix;

		/* Grid */
		c_ptr = &cave[y][x];

		/* Scan all objects in the grid */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;
		
			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Done */
			if (this_o_idx == o_idx)
			{
				/* No previous */
				if (prev_o_idx == 0)
				{
					/* Remove from list */
					c_ptr->o_idx = next_o_idx;
				}

				/* Real previous */
				else
				{

                                        object_type *k_ptr;

                                        /* Previous object */
                                        k_ptr = &o_list[prev_o_idx];
                                        
                                        /* Remove from list */
                                        k_ptr->next_o_idx = next_o_idx;
 


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
 *
 * Handle "stacks" of objects correctly.
 */
void delete_dun_object(object_type *j_ptr)
{
	/* Excise */
	excise_dun_object(j_ptr);

	/* Dungeon floor */
	if (!(j_ptr->held_m_idx))
	{
		int y, x;

		/* Location */
		y = j_ptr->iy;
		x = j_ptr->ix;

		/* Visual update */
		lite_spot(y, x);

		/* Window stuff (if appropriate). */
		if (y == py && x == px) cave_track(py, px);
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
	cave_type *c_ptr;

	s16b this_o_idx, next_o_idx = 0;
	

	/* Refuse "illegal" locations */
	if (!in_bounds(y, x)) return;


	/* Grid */
	c_ptr = &cave[y][x];

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Wipe the object */
		object_wipe(o_ptr);

		/* Count objects */
		o_cnt--;
	}

	/* Objects are gone */
	c_ptr->o_idx = 0;

	/* Visual update */
	lite_spot(y, x);
}


/*
 * Rotate the objects in the stack under the player, so that the top item is
 * sent to the bottom and the rest are moved up. 
 */
void do_cmd_rotate_stack(void)
{
	int i, o_idx;
	int y = py, x = px;
	cave_type *c_ptr = &cave[y][x];

	/* Get the object being moved. */
	o_idx =	c_ptr->o_idx;

	/* Only rotate a pile of two or more objects. */
	if (o_idx && o_list[o_idx].next_o_idx)
	{
		/* Remove the first object from the list. */
		excise_dun_object(o_list+o_idx);
	
		/* Find end of the list. */
		for (i = c_ptr->o_idx; o_list[i].next_o_idx; i = o_list[i].next_o_idx);
	
		/* Add after the last object. */
		o_list[i].next_o_idx = o_idx;
	
		/* Display the square again. */
		lite_spot(y,x);
	}

	/* Remember the object beneath the player. */
	object_track(&o_list[c_ptr->o_idx]);
}


/*
 * Try to increase the size of o_list to accommodate current requirements.
 * Return FALSE if this failed for one reason or another.
 */
bool grow_o_list(void)
{
#ifdef USE_DYNAMIC_LISTS
	object_type *new;

	/* This function returns FALSE on out of memory errors, so need not crash. */
	vptr (*old_rpanic_aux)(huge) = rpanic_aux;

	uint new_max = z_info->o_max*2;

	size_t new_size = new_max*sizeof(object_type);

	/* Can't store o_list's new size. */
	if (new_max <= z_info->o_max || new_max > MAX_SHORT) return FALSE;

	/* Can't request m_list's new size. */
	if (new_size <= sizeof(object_type) * z_info->o_max) return FALSE;

	/* Failure is safe here. */
	rpanic_aux = rpanic_none;

	/* Try to allcoate the new memory. */
	C_MAKE(new, new_max, object_type);

	/* Restore rpanic_aux. */
	rpanic_aux = old_rpanic_aux;

	/* Handle success. */
	if (new)
	{
		/* Let cheaters know the level is really full. */
		if (cheat_peek) msg_format("Object list grown from %u to %u.",
			z_info->o_max, new_max);

		/* Start using the new m_list. */
		C_COPY(new, o_list, z_info->o_max, object_type);
		FREE(o_list);
		o_list = new;
		z_info->o_max = new_max;
	}

	return (new != NULL);
#else /* USE_DYNAMIC_LISTS */

	/* Not allowed to grow the array. */
	return FALSE;

#endif /* USE_DYNAMIC_LISTS */
}

/*
 * Actually remove objects during compacting.
 */
static void compact_objects_purge(int size)
{
	int i, y, x, num, cnt;

	int cur_lev, cur_dis, chance;

	assert(size > 0); /* See caller(s). */

	msg_print("Compacting objects...");


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
			object_type *o_ptr = &o_list[i];

			object_kind *k_ptr = &k_info[o_ptr->k_idx];

			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			/* Hack -- High level objects start out "immune" */
			if (object_k_level(k_ptr) > cur_lev) continue;

			/* Monster */
			if (o_ptr->held_m_idx)
			{
				monster_type *m_ptr;
				
				/* Acquire monster */
				m_ptr = &m_list[o_ptr->held_m_idx];

				/* Get the location */
				y = m_ptr->fy;
				x = m_ptr->fx;

				/* Monsters protect their objects */
				if (rand_int(100) < 90) continue;
			}
			
			/* Dungeon */
			else
			{
				/* Get the location */
				y = o_ptr->iy;
				x = o_ptr->ix;
			}

			/* Nearby objects start out "immune" */
			if ((cur_dis > 0) && (distance(py, px, y, x) < cur_dis)) continue;

			/* Saving throw */
			chance = 90;

			/* Hack -- only compact artifacts in emergencies */
	    if (allart_p(o_ptr)
		&& (cnt < 1000)) chance = 100;

			/* Apply the saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the object */
			delete_dun_object(o_ptr);

			/* Count it */
			num++;
		}
	}

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_objects_aux(int i1, int i2)
{
	int i;

	cave_type *c_ptr;

	object_type *o_ptr;


	/* Do nothing */
	if (i1 == i2) return;


	/* Repair objects */
	for (i = 1; i < o_max; i++)
	{
		/* Acquire object */
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


	/* Acquire object */
	o_ptr = &o_list[i1];


	/* Monster */
	if (o_ptr->held_m_idx)
	{
		monster_type *m_ptr;
		
		/* Acquire monster */
		m_ptr = &m_list[o_ptr->held_m_idx];
		
		/* Repair monster */
		if (m_ptr->hold_o_idx == i1)
		{
			/* Repair */
			m_ptr->hold_o_idx = i2;
		}
	}

	/* Dungeon */
	else
	{
		int y, x;

		/* Acquire location */  
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Acquire grid */
		c_ptr = &cave[y][x];

		/* Repair grid */
		if (c_ptr->o_idx == i1)
		{
			/* Repair */
			c_ptr->o_idx = i2;
		}
	}


	/* Structure copy */
	o_list[i2] = o_list[i1];

	/* Wipe the hole */
	object_wipe(o_ptr);
}

/*
 * Remove dead (or purged) objects from the object list.
 */	
static void compact_objects_excise(void)
{
	int i;

	/* Excise dead objects (backwards!) */
	for (i = o_max - 1; i >= 1; i--)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip real objects */
		if (o_ptr->k_idx) continue;

		/* Move last object into open hole */
		compact_objects_aux(o_max - 1, i);

		/* Compress "o_max" */
		o_max--;
	}
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
	/* Try to increase the space available. */
	if (size > 0 && !grow_o_list())
	{
		/* Purge objects if impossible. */
		compact_objects_purge(size);
	}

	/* Remove dead objects from the list. */
	compact_objects_excise();
}



/*
 * Delete all the items when player leaves the level
 *
 * Note -- we do NOT visually reflect these (irrelevant) changes
 *
 * Hack -- we clear the "c_ptr->o_idx" field for every grid,
 * and the "m_ptr->next_o_idx" field for every monster, since
 * we know we are clearing every object.  Technically, we only
 * clear those fields for grids/monsters containing objects,
 * and we clear it once for every such object.
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

		/* Mega-Hack -- preserve artifacts */
		if (!character_dungeon || preserve_mode)
		{
			/* Hack -- Preserve unknown artifacts */
			if (artifact_p(o_ptr))
			{
				/* Mega-Hack -- Preserve the artifact */
				a_info[o_ptr->name1].cur_num = 0;
			}
		}

		/* Monster */
		if (o_ptr->held_m_idx)
		{
			monster_type *m_ptr;
			
			/* Monster */
			m_ptr = &m_list[o_ptr->held_m_idx];
			
			/* Hack -- see above */
			m_ptr->hold_o_idx = 0;
		}
		
		/* Dungeon */
		else
		{
			cave_type *c_ptr;

			/* Access location */
			int y = o_ptr->iy;
			int x = o_ptr->ix;

			/* Access grid */
			c_ptr = &cave[y][x];

			/* Hack -- see above */
			c_ptr->o_idx = 0;
		}

		/* Wipe the object */
		WIPE(o_ptr, object_type);
	}

	/* Reset "o_max" */
	o_max = 1;

	/* Reset "o_cnt" */
	o_cnt = 0;
}


/*
 * Acquires and returns the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
object_type *o_pop(void)
{
	int i;


	/* Initial allocation */
	if (o_max < MAX_O_IDX)
	{
		/* Get next space */
		i = o_max;

		/* Expand object array */
		o_max++;

		/* Count objects */
		o_cnt++;

		/* Use this object */
		return (o_list+i);
	}


	/* Recycle dead objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr;
		
		/* Acquire object */
		o_ptr = &o_list[i];

		/* Skip live objects */
		if (o_ptr->k_idx) continue;

		/* Count objects */
		o_cnt++;

		/* Use this object */
		return (o_ptr);
	}


	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msg_print("Too many objects!");

	/* Oops */
	return (NULL);
}



/*
 * Apply a "object restriction function" to the "object allocation table"
 */
static errr get_obj_num_prep(bool (*hook)(int))
{
	int i;

	/* Get the entry */
	alloc_entry *table = alloc_kind_table;

	/* Scan the allocation table */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* Accept objects which pass the restriction, if any */
		if (!hook || (*hook)(table[i].index))
		{
			/* Accept this object */
			table[i].prob2 = table[i].prob1;
		}

		/* Do not use this object */
		else
		{
			/* Decline this object */
			table[i].prob2 = 0;
		}
	}

	/* Success */
	return (0);
}



/*
 * Choose an object kind that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "object allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" object, in
 * a relatively efficient manner.
 *
 * It is (slightly) more likely to acquire an object of the given level
 * than one of a lower level.  This is done by choosing several objects
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no objects are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 */
s16b get_obj_num(int level)
{
	int                     i, j, p;

	int                     k_idx;

	long            value, total;

	object_kind             *k_ptr;

	alloc_entry             *table = alloc_kind_table;


	/* Boost level */
	if (level > 0)
	{
		/* Occasional "boost" */
		if (rand_int(GREAT_OBJ) == 0)
		{
			/* What a bizarre calculation */
			level = 1 + (level * MAX_DEPTH / randint(MAX_DEPTH));
		}
	}


	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* Objects are sorted by depth */
		if (table[i].level > level) break;

		/* Default */
		table[i].prob3 = 0;

		/* Access the index */
		k_idx = table[i].index;

		/* Access the actual kind */
		k_ptr = &k_info[k_idx];

		/* Hack -- prevent embedded chests */
		if (opening_chest && (k_ptr->tval == TV_CHEST)) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Total */
		total += table[i].prob3;
	}

	/* No legal objects */
	if (total <= 0) return (0);


	/* Pick an object */
	value = rand_int(total);

	/* Find the object */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}


	/* Power boost */
	p = rand_int(100);

	/* Try for a "better" object once (50%) or twice (10%) */
	if (p < 60)
	{
		/* Save old */
		j = i;

		/* Pick a object */
		value = rand_int(total);

		/* Find the monster */
		for (i = 0; i < alloc_kind_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Try for a "better" object twice (10%) */
	if (p < 10)
	{
		/* Save old */
		j = i;

		/* Pick a object */
		value = rand_int(total);

		/* Find the object */
		for (i = 0; i < alloc_kind_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
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
 */
void object_known(object_type *o_ptr)
{
	/* Clear the "Empty" info */
	o_ptr->ident &= ~(IDENT_EMPTY);

	/* Now we know about the item */
	o_ptr->ident |= (IDENT_KNOWN);

	/* And we even know if it's cursed */
	o_ptr->ident |= (IDENT_SENSE_CURSED);
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
 * Return the value of the flags the object has...
 * Consider all flags if this is being done to find an appropriate randart.
 * If this is being done to price an item, flags based on the k_idx, name1
 * and name2 fields are counted elsewhere. 
 */
s32b PURE flag_cost(object_ctype *o_ptr, bool all)
{
    s32b total = 0;
    u32b f1, f2, f3;
	int plusses = o_ptr->pval;
        object_flags(o_ptr, &f1, &f2, &f3);

	/* Remove the standard flags. */
	if (!all)
	{
		object_kind *k_ptr = k_info+o_ptr->k_idx;
		ego_item_type *e_ptr = e_info+o_ptr->name2;
		artifact_type *a_ptr = a_info+o_ptr->name1;

		f1 &= ~(k_ptr->flags1 | e_ptr->flags1 | a_ptr->flags1);
		f2 &= ~(k_ptr->flags2 | e_ptr->flags2 | a_ptr->flags2);
		f3 &= ~(k_ptr->flags3 | e_ptr->flags3 | a_ptr->flags3);
	}		

    if (f1 & TR1_STR) total += (1000 * plusses);
    if (f1 & TR1_INT) total += (1000 * plusses);
    if (f1 & TR1_WIS) total += (1000 * plusses);
    if (f1 & TR1_DEX) total += (1000 * plusses);
    if (f1 & TR1_CON) total += (1000 * plusses);
    if (f1 & TR1_CHR) total += (250 * plusses);
    if (f1 & TR1_CHAOTIC) total += 10000;
    if (f1 & TR1_VAMPIRIC) total += 13000;
    if (f1 & TR1_STEALTH) total += (250 * plusses);
    if (f1 & TR1_SEARCH) total += (100 * plusses);
    if (f1 & TR1_INFRA) total += (150 * plusses);
    if (f1 & TR1_TUNNEL) total += (175 * plusses);
    if ((f1 & TR1_SPEED) && (plusses > 0))
            total += (10000 + (2500 * plusses));
    if ((f1 & TR1_BLOWS) && (plusses > 0))
            total += (10000 + (2500 * plusses));
    if (f1 & TR1_XXX1) total += 0;
    if (f1 & TR1_XXX2) total += 0;
    if (f1 & TR1_SLAY_ANIMAL) total += 3500;
    if (f1 & TR1_SLAY_EVIL) total += 4500;
    if (f1 & TR1_SLAY_UNDEAD) total += 3500;
    if (f1 & TR1_SLAY_DEMON) total += 3500;
    if (f1 & TR1_SLAY_ORC) total += 3000;
    if (f1 & TR1_SLAY_TROLL) total += 3500;
    if (f1 & TR1_SLAY_GIANT) total += 3500;
	switch (f1 & TR1_ALL_SLAY_DRAGON)
	{
		case TR1_SLAY_DRAGON: total += 3500; break;
		case TR1_KILL_DRAGON: total += 5500; break;
		case TR1_X15_DRAGON: total += 10000; break;
	}
    if (f1 & TR1_VORPAL) total += 5000;
    if (f1 & TR1_IMPACT) total += 5000;
    if (f1 & TR1_BRAND_POIS) total += 7500;
    if (f1 & TR1_BRAND_ACID) total += 7500;
    if (f1 & TR1_BRAND_ELEC) total += 7500;
    if (f1 & TR1_BRAND_FIRE) total += 5000;
    if (f1 & TR1_BRAND_COLD) total += 5000;
    if (f2 & TR2_SUST_STR) total += 850;
    if (f2 & TR2_SUST_INT) total += 850;
    if (f2 & TR2_SUST_WIS) total += 850;
    if (f2 & TR2_SUST_DEX) total += 850;
    if (f2 & TR2_SUST_CON) total += 850;
    if (f2 & TR2_SUST_CHR) total += 250;
    if (f2 & TR2_RAND_RESIST) total += 0;
    if (f2 & TR2_RAND_POWER) total += 0;
    if (f2 & TR2_IM_ACID) total += 10000;
    if (f2 & TR2_IM_ELEC) total += 10000;
    if (f2 & TR2_IM_FIRE) total += 10000;
    if (f2 & TR2_IM_COLD) total += 10000;
    if (f2 & TR2_RAND_EXTRA) total += 0;
    if (f2 & TR2_REFLECT) total += 10000;
    if (f2 & TR2_FREE_ACT) total += 4500;
    if (f2 & TR2_HOLD_LIFE) total += 8500;
    if (f2 & TR2_RES_ACID) total += 1250;
    if (f2 & TR2_RES_ELEC) total += 1250;
    if (f2 & TR2_RES_FIRE) total += 1250;
    if (f2 & TR2_RES_COLD) total += 1250;
    if (f2 & TR2_RES_POIS) total += 2500;
    if (f2 & TR2_RES_FEAR) total += 2500;
    if (f2 & TR2_RES_LITE) total += 1750;
    if (f2 & TR2_RES_DARK) total += 1750;
    if (f2 & TR2_RES_BLIND) total += 2000;
    if (f2 & TR2_RES_CONF) total += 2000;
    if (f2 & TR2_RES_SOUND) total += 2000;
    if (f2 & TR2_RES_SHARDS) total += 2000;
    if (f2 & TR2_RES_NETHER) total += 2000;
    if (f2 & TR2_RES_NEXUS) total += 2000;
    if (f2 & TR2_RES_CHAOS) total += 2000;
    if (f2 & TR2_RES_DISEN) total += 10000;
    if (f3 & TR3_SH_FIRE) total += 3750;
    if (f3 & TR3_SH_ELEC) total += 5000;
    if (f3 & TR3_SHOW_ARMOUR) total += 0;
    if (f3 & TR3_NO_TELE) total += 2500;
    if (f3 & TR3_NO_MAGIC) total += 2500;
    if (f3 & TR3_WRAITH) total += 250000;
    if (f3 & TR3_TY_CURSE) total -= 15000;
    if (f3 & TR3_EASY_KNOW) total += 0;
    if (f3 & TR3_HIDE_TYPE) total += 0;
    if (f3 & TR3_SHOW_MODS) total += 0;
    if (f3 & TR3_GOOD) total += 0;
    if (f3 & TR3_FEATHER) total += 1250;
    if (f3 & TR3_LITE) total += 1250;
    if (f3 & TR3_SEE_INVIS) total += 2000;
    if (f3 & TR3_TELEPATHY) total += 12500;
    if (f3 & TR3_SLOW_DIGEST) total += 750;
    if (f3 & TR3_REGEN) total += 2500;
    if (f3 & TR3_XTRA_MIGHT) total += 2250;
    if (f3 & TR3_XTRA_SHOTS) total += 10000;
    if (f3 & TR3_IGNORE_ACID) total += 100;
    if (f3 & TR3_IGNORE_ELEC) total += 100;
    if (f3 & TR3_IGNORE_FIRE) total += 100;
    if (f3 & TR3_IGNORE_COLD) total += 100;
    if (f3 & TR3_ACTIVATE) total += 100;
    if (f3 & TR3_DRAIN_EXP) total -= 12500;
    if (f3 & TR3_TELEPORT)
           { if (o_ptr->ident & IDENT_CURSED)
                    total -= 7500;
             else
                    total += 250;
            }
    if (f3 & TR3_AGGRAVATE) total -= 10000;
    if (f3 & TR3_BLESSED) total += 750;
    if (f3 & TR3_CURSED) total -= 5000;
    if (f3 & TR3_AUTO_CURSE) total -= 10000;
    if (f3 & TR3_HEAVY_CURSE) total -= 12500;
    if (f3 & TR3_PERMA_CURSE) total -= 15000;

    /* Also, give some extra for activatable powers... */

    if ((o_ptr->art_name) && (o_ptr->flags3 & (TR3_ACTIVATE)))
    {
        int type = o_ptr->activation;

        if (type == ACT_SUNLIGHT) total += 250;
        else if (type == ACT_BO_MISS_1) total += 250;
        else if (type == ACT_BA_POIS_1) total += 300;
        else if (type == ACT_BO_ELEC_1) total += 250;
        else if (type == ACT_BO_ACID_1) total += 250;
        else if (type == ACT_BO_COLD_1) total += 250;
        else if (type == ACT_BO_FIRE_1) total += 250;
        else if (type == ACT_BA_COLD_1) total += 750;
        else if (type == ACT_BA_FIRE_1) total += 1000;
        else if (type == ACT_DRAIN_1) total += 500;
        else if (type == ACT_BA_COLD_2) total += 1250;
        else if (type == ACT_BA_ELEC_2) total += 1500;
        else if (type == ACT_DRAIN_2) total += 750;
        else if (type == ACT_VAMPIRE_1) total = 1000;
        else if (type == ACT_BO_MISS_2) total += 1000;
        else if (type == ACT_BA_FIRE_2) total += 1750;
        else if (type == ACT_BA_COLD_3) total += 2500;
        else if (type == ACT_BA_ELEC_3) total += 2500;
        else if (type == ACT_WHIRLWIND) total += 7500;
        else if (type == ACT_VAMPIRE_2) total += 2500;
        else if (type == ACT_CALL_CHAOS) total += 5000;
        else if (type == ACT_SHARD) total += 5000;
        else if (type == ACT_DISP_EVIL) total += 4000;
        else if (type == ACT_DISP_GOOD) total += 3500;
        else if (type == ACT_BA_MISS_3) total += 5000;
        else if (type == ACT_CONFUSE) total += 500;
        else if (type == ACT_SLEEP) total += 750;
        else if (type == ACT_QUAKE) total += 600;
        else if (type == ACT_TERROR) total += 2500;
        else if (type == ACT_TELE_AWAY) total += 2000;
        else if (type == ACT_GENOCIDE) total += 10000;
        else if (type == ACT_MASS_GENO) total += 10000;
        else if (type == ACT_CHARM_ANIMAL) total += 7500;
        else if (type == ACT_CHARM_UNDEAD) total += 10000;
        else if (type == ACT_CHARM_OTHER) total += 10000;
        else if (type == ACT_CHARM_ANIMALS) total += 12500;
        else if (type == ACT_CHARM_OTHERS) total += 17500;
        else if (type == ACT_SUMMON_ANIMAL) total += 10000;
        else if (type == ACT_SUMMON_PHANTOM) total += 12000;
        else if (type == ACT_SUMMON_ELEMENTAL) total += 15000;
        else if (type == ACT_SUMMON_DEMON) total += 20000;
        else if (type == ACT_SUMMON_UNDEAD) total += 20000;
        else if (type == ACT_CURE_LW) total += 500;
        else if (type == ACT_CURE_MW) total += 750;
        else if (type == ACT_REST_LIFE) total += 7500;
        else if (type == ACT_REST_ALL) total += 15000;
        else if (type == ACT_CURE_700) total += 10000;
        else if (type == ACT_CURE_1000) total += 15000;
        else if (type == ACT_ESP) total += 1500;
        else if (type == ACT_BERSERK) total += 800;
        else if (type == ACT_PROT_EVIL) total += 5000;
        else if (type == ACT_RESIST_ALL) total += 5000;
        else if (type == ACT_SPEED) total += 15000;
        else if (type == ACT_XTRA_SPEED) total += 25000;
        else if (type == ACT_WRAITH) total += 25000;
        else if (type == ACT_INVULN) total += 25000;
        else if (type == ACT_LIGHT) total += 150;
        else if (type == ACT_MAP_LIGHT) total += 500;
        else if (type == ACT_DETECT_ALL) total += 1000;
        else if (type == ACT_DETECT_XTRA) total += 12500;
        else if (type == ACT_ID_FULL) total += 10000;
        else if (type == ACT_ID_PLAIN) total += 1250;
        else if (type == ACT_RUNE_EXPLO) total += 4000;
        else if (type == ACT_RUNE_PROT) total += 10000;
        else if (type == ACT_SATIATE) total += 2000;
        else if (type == ACT_DEST_DOOR) total += 100;
        else if (type == ACT_STONE_MUD) total += 1000;
        else if (type == ACT_RECHARGE) total += 1000;
        else if (type == ACT_ALCHEMY) total += 10000;
        else if (type == ACT_DIM_DOOR) total += 10000;
        else if (type == ACT_TELEPORT) total += 2000;
        else if (type == ACT_RECALL) total += 7500;
    }


    return total;

}

/*
 * Determine how various unusual numerical bonuses for an item are and return
 * the bonus to value this gives to most things.
 *
 * Maybe it should exclude the expected bonus from being an artefact.
 * Maybe it should differentiate between differences which can be modified by
 * scrolls and those which can't. Each enchantment point costs at least 125 AU
 * with scrolls (doubled for artefacts), so each point below +0 costs that much
 * to produce, and above it the following cumulative values (see enchant()):
 *
 *  +1  +2  +3  +4  +5  +6   +7   +8   +9  +10  +11   +12   +13   +14    +15
 * 125 251 382 522 678 857 1065 1315 1672 2297 4797 14412 32270 57270 119770
 *
 * Enchantments above +15, and enchantments which are not armour to_a or
 * weapon to_d or to_h cannot be obtained by scrolls.
 */
static s32b PURE mod_cost(object_ctype *o_ptr)
{
	object_kind *k_ptr = k_info+o_ptr->k_idx;
	return 100 * (

		/* Bonuses to hit, damage and AC. */
		o_ptr->to_h - k_ptr->to_h +
		o_ptr->to_d - k_ptr->to_d +
		o_ptr->to_a - k_ptr->to_a +

		/* Hack -- Factor in extra damage dice */
		(((o_ptr->dd > k_ptr->dd) && (o_ptr->ds == k_ptr->ds)) ?
		(o_ptr->dd - k_ptr->dd) * o_ptr->ds : 0)
	);
}


/*
 * Return the "real" price of an "aware" item, not including discounts
 *
 * Wand and staffs get cost for each charge
 *
 * Armor is worth an extra 100 gold per bonus point to armor class.
 *
 * Weapons are worth an extra 100 gold per bonus point (AC,TH,TD).
 *
 * Missiles are only worth 5 gold per bonus point, since they
 * usually appear in groups of 20, and we want the player to get
 * the same amount of cash for any "equivalent" item.  Note that
 * missiles never have any of the "pval" flags, and in fact, they
 * only have a few of the available flags, primarily of the "slay"
 * and "brand" and "ignore" variety.
 *
 * Armor with a negative armor bonus is worthless.
 * Weapons with negative hit+damage bonuses are worthless.
 *
 * Every wearable item with a "pval" bonus is worth extra (see below).
 */
static s32b PURE object_value_aux(object_ctype *o_ptr)
{
	s32b value;

	u32b f1, f2, f3;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];


	/* Hack -- "worthless" items */
	if (!k_ptr->cost) return (0L);

	/* Base cost */
	value = k_ptr->cost;


	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* Hack -- "worthless" artifacts */
		if (!a_ptr->cost) return (0L);

		/* Hack -- Use the artifact cost instead */
		value = a_ptr->cost;
	}

	/* Ego-Item */
	else if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		/* Hack -- "worthless" ego-items */
		if (!e_ptr->cost) return (0L);

		/* Hack -- Reward the ego-item with a bonus */
		value += e_ptr->cost;
	}

	/* Add the modifiers for random flags. */
    if (o_ptr->flags1 || o_ptr->flags2 || o_ptr->flags3)
             value += flag_cost (o_ptr, FALSE);

	/* Analyze pval bonus */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
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
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
		{
			/* Don't treat negative pvals in a special way, as worthless items
			 * should be broken. */
			/* if (o_ptr->pval < 0) return (0L); */

			/* No pval */
			if (!o_ptr->pval) break;

			/* Give credit for stat bonuses */
			if (f1 & (TR1_STR)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_INT)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_WIS)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_DEX)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_CON)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_CHR)) value += (o_ptr->pval * 200L);

			/* Give credit for stealth and searching */
			if (f1 & (TR1_STEALTH)) value += (o_ptr->pval * 100L);
			if (f1 & (TR1_SEARCH)) value += (o_ptr->pval * 100L);

			/* Give credit for infra-vision and tunneling */
			if (f1 & (TR1_INFRA)) value += (o_ptr->pval * 50L);
			if (f1 & (TR1_TUNNEL)) value += (o_ptr->pval * 50L);

			/* Give credit for extra attacks */
			if (f1 & (TR1_BLOWS)) value += (o_ptr->pval * 5000L);

			/* Give credit for speed bonus */
			if (f1 & (TR1_SPEED)) value += (o_ptr->pval * 3000L);

			break;
		}
	}


	/* Analyze the item */
	switch (o_ptr->tval)
	{
		/* Wands/Staffs */
		case TV_WAND:
		case TV_STAFF:
		{
			/* Pay extra for charges */
			value += ((value / 20) * o_ptr->pval);

			/* Done */
			break;
		}

		/* Rings/Amulets/Armour/Bows/Weapons */
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
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		{
			/* Factor in the bonuses */
			value += mod_cost(o_ptr);

			/* Done */
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Factor in the bonuses */
			value += mod_cost(o_ptr)/20;

			/* Done */
			break;
		}
	}


	/* Return the value */
	return MAX(value, 0L);
}


/*
 * Return the price of an item including plusses (and charges) if known.
 *
 * If full is set, the object should be valued as if it was fully known.
 * It also represents the difference between the value of an object as it is
 * and its value as the player sees it.
 */
s32b PURE object_value(object_ctype *o1_ptr, bool full)
{
	s32b value;
	object_type j_ptr[1], o_ptr[1];

	/* j_ptr is the object with the desired extra knowledge. */
	object_copy(j_ptr, o1_ptr);
	if (full) j_ptr->ident |= IDENT_MENTAL;

	/* o_ptr is what the player would be assumed to know about j_ptr. */
	object_info_known(o_ptr, j_ptr);

	/* The player can't always value ego items correctly. */
	if (!full && ((!spoil_ego && o_ptr->name2) || (!spoil_art && o_ptr->name1)))
		o_ptr->name1 = o_ptr->name2 = 0;

	/* Known broken and cursed items are worthless. */
	if (broken_p(o_ptr) || cursed_p(o_ptr)) value = 0;

	/* aware items are handled elsewhere. */
	else if (o_ptr->k_idx != OBJ_UNKNOWN) value = object_value_aux(o_ptr);

	/* Extract the assumed price from o_base. */
	else value = o_base[u_info[k_info[o1_ptr->k_idx].u_idx].p_id].cost;

	/* Apply discount (if any) */
	if (o_ptr->discount) value -= (value * o_ptr->discount / 100L);

	return value;
}






/*
 * Set o_ptr->stack to an appropriate value.
 *
 * Non-objects and single objects are given the special index of 0.
 *
 * Other objects are given numbers in order.
 *
 * NB: This makes no effort to check that indices are unique (which may be
 * impossible with an 8 bit index). It merely makes strange stacks unlikely.
 */
void set_stack_number(object_type *o_ptr)
{
	static int cur = 0;
	assert(o_ptr);

	/* Boring objects are not part of a stack. */
	if (!o_ptr->k_idx || o_ptr->number == 1)
	{
		o_ptr->stack = 0;
	}
	else
	{
		/* This uses the 255 indices in order. This means that same_stack()
		 * gives some false positives, but this should not be easy to induce. */
		if (cur == 255) cur = 1;
		else cur++;

		o_ptr->stack = cur;
	}
}

/*
 * Return TRUE if the specified objects came from the same stack originally.
 */
static bool PURE same_stack(object_ctype *o_ptr, object_ctype *j_ptr)
{
	return (o_ptr->stack && o_ptr->stack == j_ptr->stack);
}

/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow wands/staffs (if they are known to have equal
 * charges) and rods (if fully charged) to combine.  They will unstack
 * (if necessary) when they are used.
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
int PURE object_similar_2(object_ctype *o_ptr, object_ctype *j_ptr)
{

	int total = o_ptr->number + j_ptr->number;


	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return (0);


	/* Analyze the items */
	switch (o_ptr->tval)
	{
		/* Chests and money */
		case TV_GOLD:
		case TV_CHEST:
		{
			/* Never okay */
			return (0);
		}

		/* Food and Potions and Scrolls */
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		{
			/* Assume okay */
			break;
		}

		/* Staffs and Wands */
		case TV_STAFF:
		case TV_WAND:
		{
			/* Require permission */
			if (!stack_allow_wands) return (0);

			/* Require knowledge */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr))
			{
				if (!same_stack(o_ptr, j_ptr)) return (0);
			}

			/* Require identical charges */
			if (o_ptr->pval != j_ptr->pval) return (0);

			/* Probably okay */
			break;
		}

		/* Staffs and Wands and Rods */
		case TV_ROD:
		{
			/* Require permission */
			if (!stack_allow_wands) return (0);

			/* Require identical charges */
			if (o_ptr->timeout != j_ptr->timeout) return (0);

			/* Probably okay */
			break;
		}

		/* Weapons and Armor */
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
			/* Require permission */
			if (!stack_allow_items) return (0);

			/* Fall through */
		}

		/* Rings, Amulets, Lites, Missiles */
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Require identical knowledge of both items */
			if (object_known_p(o_ptr) != object_known_p(j_ptr)) return (0);

			/* Require identical "bonuses" */
			if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
			if (o_ptr->to_d != j_ptr->to_d) return (FALSE);
			if (o_ptr->to_a != j_ptr->to_a) return (FALSE);

			/* Require identical "pval" code */
			if (o_ptr->pval != j_ptr->pval) return (FALSE);

			/* Require identical "artifact" names */
			if (o_ptr->name1 != j_ptr->name1) return (FALSE);

            /* Random artifacts never stack */
            if (o_ptr->art_name || j_ptr->art_name) return (FALSE);

			/* Require identical "ego-item" names */
			if (o_ptr->name2 != j_ptr->name2) return (FALSE);

			/* Hack -- Never stack "powerful" items */
			if (o_ptr->activation) return (FALSE);

			/* Hack -- Never stack recharging items */
			if (o_ptr->timeout || j_ptr->timeout) return (FALSE);

			/* Require identical "values" */
			if (o_ptr->ac != j_ptr->ac) return (FALSE);
			if (o_ptr->dd != j_ptr->dd) return (FALSE);
			if (o_ptr->ds != j_ptr->ds) return (FALSE);

			/* Fall through. */
		}

		/* Various */
		default:
		{
			/* Require knowledge or former similarity. */
			if (!(k_info[o_ptr->k_idx].flags3 & TR3_EASY_KNOW) && 
				(!object_known_p(o_ptr) || !object_known_p(j_ptr)))
			{
				if (!same_stack(o_ptr, j_ptr)) return (0);
			}

			/* Probably okay */
			break;
		}
	}



    /* Hack -- Identical flags! */
    if ((o_ptr->flags1 != j_ptr->flags1) ||
        (o_ptr->flags2 != j_ptr->flags2) ||
        (o_ptr->flags3 != j_ptr->flags3))
            return (0);

	/* Hack -- Require identical "cursed" status */
	if ((o_ptr->ident & (IDENT_CURSED)) != (j_ptr->ident & (IDENT_CURSED))) return (0);

	/* Hack -- Require identical "broken" status */
	if ((o_ptr->ident & (IDENT_BROKEN)) != (j_ptr->ident & (IDENT_BROKEN))) return (0);


	/* Hack -- require semi-matching "inscriptions" */
	if (!stack_force_notes_all && o_ptr->note && j_ptr->note && (o_ptr->note != j_ptr->note)) return (0);

	/* Hack -- normally require matching "inscriptions" */
	if (!stack_force_notes && (o_ptr->note != j_ptr->note)) return (0);

	/* Hack -- normally require matching "discounts" */
	if (!stack_force_costs && !strchr(quark_str(o_ptr->note), '%') && !strchr(quark_str(j_ptr->note), '%') && (o_ptr->discount != j_ptr->discount)) return (0);


	/* They can't all stack, so return how many do. */
	if (total >= MAX_STACK_SIZE) return MAX_STACK_SIZE-1-o_ptr->number;


	/* They can, so remove how many there are. */
	return j_ptr->number;
}


bool PURE object_similar(object_ctype *o_ptr, object_ctype *j_ptr)
{
	return (object_similar_2(o_ptr, j_ptr) == j_ptr->number);
}

/*
 * Find an appropriate discount when two similar piles of items (which
 * are assumed to have the same per-item value) are merged.
 *
 * This can never decrease the total value of the merged stacks.
 */
static PURE byte merge_discounts(object_ctype *o_ptr, object_ctype *j_ptr)
{
	int d1 = o_ptr->discount, d2 = j_ptr->discount;
	int n1 = o_ptr->number, n2 = j_ptr->number;

	return 100 - ((100-d1)*n1 + (100-d2)*n2) / (n1+n2);
}

/*
 * Combine two strings, adding a space between them if necessary, and return
 * as a quark.
 */
static u16b merge_quarks(cptr s, cptr t)
{
	assert(s && t); /* quark_str() does not return 0. */

	if (!*s) return quark_add(t);
	if (!*t) return quark_add(s);

	return quark_add(format("%s %s", s, t));
}

/*
 * Allow one item to "absorb" another, assuming they are similar
 * Return true if every object was completely absorbed.
 */
bool object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	bool rc;

	/* Hack -- blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr);

    /* Hack -- clear "storebought" if only one has it */
	o_ptr->ident &= ~(~j_ptr->ident & IDENT_STOREB);

	/* Hack - blend other IDENT_* flags */
	o_ptr->ident |= j_ptr->ident;

	/* Hack -- blend "inscriptions" */
	if (j_ptr->note != o_ptr->note)
	{
		cptr jq = quark_str(j_ptr->note);
		cptr oq = quark_str(o_ptr->note);
		o_ptr->note = merge_quarks(oq, jq);
	}

	/* Combine the discounts. */
	o_ptr->discount = merge_discounts(o_ptr, j_ptr);

	/* Add together the stacks, and return TRUE if the second is empty. */
	rc = store_object_absorb(o_ptr, j_ptr);

	/* Combine the stack numbers. */
	if (!o_ptr->stack) o_ptr->stack = j_ptr->stack;

	/* Give a new stack number if necessary. */
	if (!same_stack(o_ptr, j_ptr))
	{
		set_stack_number(o_ptr);
		j_ptr->stack = o_ptr->stack;
	}

	return rc;
}



/*
 * Wipe an object clean.
 */
void object_wipe(object_type *o_ptr)
{
	/* Wipe the structure */
	WIPE(o_ptr, object_type);
}


/*
 * Prepare an object based on an existing object
 */
void object_copy(object_type *o_ptr, object_ctype *j_ptr)
{
	/* Copy the structure */
	COPY(o_ptr, j_ptr, object_type);
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

	/* Efficiency -- tval */
	o_ptr->tval = k_ptr->tval;

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

	/* Hack -- worthless items are always "broken" */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

	/* Hack -- known items are always "sensed" */
	if object_known_p(o_ptr) o_ptr->ident |= (IDENT_SENSE);
}


/*
 * Help determine an "enchantment bonus" for an object.
 *
 * To avoid floating point but still provide a smooth distribution of bonuses,
 * we simply round the results of division in such a way as to "average" the
 * correct floating point value.
 *
 * This function has been changed.  It uses "randnor()" to choose values from
 * a normal distribution, whose mean moves from zero towards the max as the
 * level increases, and whose standard deviation is equal to 1/4 of the max,
 * and whose values are forced to lie between zero and the max, inclusive.
 *
 * Since the "level" rarely passes 100 before Morgoth is dead, it is very
 * rare to get the "full" enchantment on an object, even a deep levels.
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
static s16b m_bonus(int max, int level)
{
	int bonus, stand, extra, value;


	/* Paranoia -- enforce maximal "level" */
	if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;


	/* The "bonus" moves towards the max */
	bonus = ((max * level) / MAX_DEPTH);

	/* Hack -- determine fraction of error */
	extra = ((max * level) % MAX_DEPTH);

	/* Hack -- simulate floating point computations */
	if (extra && rand_int(MAX_DEPTH) < extra) bonus++;


	/* The "stand" is equal to one quarter of the max */
	stand = (max / 4);

	/* Hack -- determine fraction of error */
	extra = (max % 4);

	/* Hack -- simulate floating point computations */
	if (extra && rand_int(4) < extra) stand++;


	/* Choose an "interesting" value */
	value = randnor(bonus, stand);

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
static void object_mention(object_ctype *o_ptr)
{
	/* Only describe for cheaters. */
	if (!cheat_peek) return;

	/* Artifact */
	if (artifact_p(o_ptr))
	{
		/* Silly message */
		msg_format("Artifact (%v)", object_desc_f3, o_ptr, OD_SHOP, 0);
	}

	else if (o_ptr->art_name)
	{
		msg_print("Random artifact");
	}

	/* Ego-item */
	else if (ego_item_p(o_ptr))
	{
		/* Silly message */
		msg_format("Ego-item (%v)", object_desc_f3, o_ptr, OD_SHOP, 0);
	}

	/* Normal item */
	else
	{
		/* Silly message */
		msg_format("Object (%v)", object_desc_f3, o_ptr, OD_SHOP, 0);
	}
}


/*
 * Add a random sustain to *o_ptr.
 */
static void add_sustain(object_type *o_ptr)
{
	switch (rand_int(6))
	{
		case 0: o_ptr->flags2 |= (TR2_SUST_STR); return;
		case 1: o_ptr->flags2 |= (TR2_SUST_INT); return;
		case 2: o_ptr->flags2 |= (TR2_SUST_WIS); return;
		case 3: o_ptr->flags2 |= (TR2_SUST_DEX); return;
		case 4: o_ptr->flags2 |= (TR2_SUST_CON); return;
		case 5: o_ptr->flags2 |= (TR2_SUST_CHR); return;
	}
}

/*
 * Add a random ability to *o_ptr.
 */
static void add_ability(object_type *o_ptr)
{
	switch (rand_int(8))
	{
		case 0: o_ptr->flags3 |= (TR3_FEATHER); return;
		case 1: o_ptr->flags3 |= (TR3_LITE); return;
		case 2: o_ptr->flags3 |= (TR3_SEE_INVIS); return;
		case 3: o_ptr->flags3 |= (TR3_TELEPATHY); return;
		case 4: o_ptr->flags3 |= (TR3_SLOW_DIGEST); return;
		case 5: o_ptr->flags3 |= (TR3_REGEN); return;
		case 6: o_ptr->flags2 |= (TR2_FREE_ACT); return;
		case 7: o_ptr->flags2 |= (TR2_HOLD_LIFE); return;
	}
}
		
/*
 * Add a random power to *o_ptr.
 */
static void add_power(object_type *o_ptr)
{
	switch (rand_int(11))
	{
		case 0: o_ptr->flags2 |= (TR2_RES_BLIND); return;
		case 1: o_ptr->flags2 |= (TR2_RES_CONF); return;
		case 2: o_ptr->flags2 |= (TR2_RES_SOUND); return;
		case 3: o_ptr->flags2 |= (TR2_RES_SHARDS); return;
		case 4: o_ptr->flags2 |= (TR2_RES_NETHER); return;
		case 5: o_ptr->flags2 |= (TR2_RES_NEXUS); return;
		case 6: o_ptr->flags2 |= (TR2_RES_CHAOS); return;
		case 7: o_ptr->flags2 |= (TR2_RES_DISEN); return;
		case 8: o_ptr->flags2 |= (TR2_RES_POIS); return;
		case 9: o_ptr->flags2 |= (TR2_RES_DARK); return;
		case 10: o_ptr->flags2 |= (TR2_RES_LITE); return;
	}
}

/*
 * Add random resistances and powers to a new normal artefact as required.
 */
static void random_artifact_resistance(object_type * o_ptr)
{
	artifact_type *a_ptr = &a_info[o_ptr->name1];
	bool give_resistance = a_ptr->flags2 & TR2_RAND_RESIST;
	bool give_power = a_ptr->flags2 & TR2_RAND_POWER;
	if (a_ptr->flags2 & TR2_RAND_EXTRA)
	{
		if (one_in(2))
			give_resistance = TRUE;
		else
			give_power = TRUE;
	}
	if (o_ptr->name1 == ART_STORMBRINGER)
	{
		/* Give something nice. */
		if (randint(2)==1)
			o_ptr->flags3 |= TR3_DRAIN_EXP;
		else
			o_ptr->flags3 |= TR3_AGGRAVATE;
	}

    if (give_power)
    {
        /* Randomize the "xtra" power */
		add_ability(o_ptr);
    }

    artifact_bias = 0;

    if (give_resistance)
    {
        random_resistance(o_ptr, FALSE, ((randint(22))+16));
    }
}


/*
 * Decide whether to create an object of a given depth and rarity.
 */
static bool rarity_roll(byte level, byte rarity)
	{
	/* Roll for rarity */
	if (rand_int(rarity)) return FALSE;

	/* Only roll for depth when necessary */
	if (level <= (dun_depth)) return TRUE;

			/* Roll for out-of-depth creation */
	return (rand_int((level - (dun_depth)) * 2) == 0);
}

/*
 * Copy the normal name of an artefact into a buffer, return it.
 */
static cptr get_art_name(const artifact_type *a_ptr, char *buf)
{
	object_type forge;
	make_fake_artifact(&forge, a_ptr-a_info);
	strnfmt(buf, ONAME_MAX, "%v", object_desc_f3, &forge, OD_SHOP, 0);
	return buf;
}

/*
 * Makes an artefact. Is either called from make_object() on a blank object in
 * order to create a "special" artefact, or from apply_magic() on a real object
 * in order to create a "normal" artefact. In the former case, apply_magic()
 * should be called immediately afterwards.
 *
 * Returns 0 for failure, 1 for success and 2 for no available artefacts.
 *
 * If x is an artefact, rarity(x) and ood(x) how rare and out-of-depth
 * it is, and class(x) the set of artefacts in the same group (either same k_idx
 * or all special), the probability of creating a particular artefact
 * when this function is reached is:
 *
 * P(x) = rarity(x)*ood(x)*product(1-rarity(y)*ood(y)/2 | y in class(x))
 *
 * As of Cthangband 4.1.0, this means that, if first asked for a special
 * artefact at ground level, the probability of generating the Star Essence
 * of Polaris is 0.9968.
 * More interestingly, both Skullkeeper and the Terror Mask have a chance
 * of 1/5*(1-1/5/2)=18% of being generated if in depth, rather than the
 * former having a probability of 20% and the latter 16%.
 */
#define MAKE_ART_CERTAIN_FAIL	1
#define MAKE_ART_RANDOM_FAIL	2
#define MAKE_ART_NONE	3

static errr make_artifact(object_type *o_ptr, bool special)
{
	int                     i, arts = 0;

	C_TNEW(order, MAX_A_IDX, byte);

	/* Paranoia -- no "plural" artifacts */
	if (o_ptr->number > 1)
	{
		TFREE(order);
		return MAKE_ART_CERTAIN_FAIL;
	}

	for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" items */
		if (!a_ptr->name) continue;

		/* Cannot make an artefact twice */
		if (a_ptr->cur_num) continue;

		/* Exclude special/non-special artefacts as required. */
		if (!a_ptr->level2 != !special) continue;

		/* Must have the correct fields if "normal" */
		if (!special)
		{
			if (a_ptr->k_idx != o_ptr->k_idx) continue;
		}
			
		/* Note artefact */
		order[arts++] = i;
		/* Give a basic description */
	}

	if (!arts)
	{
		TFREE(order);
		return MAKE_ART_NONE;
	}

	/* Roll for each of the available artefacts */
	while (arts)
	{
		/* Pick an artefact from the list. */
		artifact_type *a_ptr = &a_info[order[i = rand_int(arts--)]];

		/* Remove it from further consideration. */
		order[i] = order[arts];

		/* Give a basic description */
		if (cheat_peek)
		{
			C_TNEW(buf, ONAME_MAX, char);
			msg_format("Rolling for %s.", get_art_name(a_ptr, buf));
			TFREE(buf);
		}

		/* Roll for the artefact. */
		if (!rarity_roll(a_ptr->level, a_ptr->rarity)) continue;

		/* Secondary roll for special artefacts */
		if (!rarity_roll(a_ptr->level2, 1)) continue;

		/* Assign the template */
		object_prep(o_ptr, a_ptr->k_idx);

		/* Mega-Hack -- mark the item as an artifact */
		o_ptr->name1 = a_ptr-a_info;

		/* Success */
		TFREE(order);
		return SUCCESS;
	}

	TFREE(order);

	/* Nothing was created. */
	return MAKE_ART_RANDOM_FAIL;
}

/*
 * Test that an ego type is suitable for the current object.
 */
static bool PURE get_ego_test(object_ctype *o_ptr, const ego_item_type *e_ptr,
	bool cursed)
{
	bool ecursed = !(e_ptr->cost);

	/* Not true if the range is wrong. */
	if (e_ptr->min_obj > o_ptr->k_idx) return FALSE;
	if (e_ptr->max_obj < o_ptr->k_idx) return FALSE;

	/* True if the cursed status matches the desired one. */
	return cursed == ecursed;
}

/*
 * Return TRUE if the k_idx given can be cursed by an ego type.
 */
static bool PURE ego_can_curse(int k_idx)
{
	object_type o_ptr[1];
	const ego_item_type *e_ptr;
	object_prep(o_ptr, k_idx);
	for (e_ptr = e_info; e_ptr < e_info+z_info->e_max; e_ptr++)
	{
		if (get_ego_test(o_ptr, e_ptr, TRUE)) return TRUE;
	}
	return FALSE;
}
	

/* 
 * Choose an ego type appropriate for o_ptr at random.
 */
static byte get_ego_item(object_ctype *o_ptr, int level, bool cursed)
{
	int e_idx, num;

	/* Paranoia. */
	if (o_ptr->name1 || o_ptr->name2 || o_ptr->art_name) return o_ptr->name2;

	for (e_idx = num = 0; e_idx < z_info->e_max; e_idx++)
	{
		ego_item_type *e_ptr = e_info+e_idx;

		if (!get_ego_test(o_ptr, e_ptr, cursed)) continue;

		/* Hack - 255 gives a special rarity. */
		if (e_ptr->chance == 255)
		{
			if (rand_int(MAX_DEPTH) < level)
			{
				return e_idx;
			}
		}
		else
		{
			num += e_ptr->chance;
		}
	}

	/* No appropriate ego types. */
	if (!num) return 0;

	num = rand_int(num);

	for (e_idx = 0; e_idx < z_info->e_max; e_idx++)
	{
		ego_item_type *e_ptr = e_info+e_idx;

		if (!get_ego_test(o_ptr, e_ptr, cursed)) continue;

		if (e_ptr->chance != 255) num -= e_ptr->chance;

		if (num < 0)
		{
			return e_idx;
		}
	}

	/* Paranoia - should never get here. */
	if (alert_failure) msg_format("Failed to find an ego type for %s %v!",
		(cursed) ? "a cursed" : "an uncursed", object_desc_f3, o_ptr, FALSE, 0);

	return 0;
}


/*
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base damage dice boosting
 * Hack -- note special processing for weapon/digger
 */
static void a_m_aux_1(object_type *o_ptr, int level, int power)
{
    int tohit1 = randint(5) + m_bonus(5, level);
	int todam1 = randint(5) + m_bonus(5, level);

	int tohit2 = m_bonus(10, level);
	int todam2 = m_bonus(10, level);

	/* Good */
	if (power > 0)
	{
		/* Enchant */
		o_ptr->to_h += tohit1;
		o_ptr->to_d += todam1;

		/* Very good */
		if (power > 1)
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
		if (power < -1)
		{
			/* Penalize again */
			o_ptr->to_h -= tohit2;
			o_ptr->to_d -= todam2;
		}

		/* Cursed (if "bad") */
		if (o_ptr->to_h + o_ptr->to_d < 0) o_ptr->ident |= (IDENT_CURSED);
	}

	/* Analyze type */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		{
			/* Very bad */
			if (power < -1)
			{
				/* Hack -- Horrible digging bonus */
				o_ptr->pval = rand_range(-10, -6);
			}

			/* Bad */
			else if (power < 0)
			{
				/* Hack -- Reverse digging bonus */
				o_ptr->pval = 0 - (o_ptr->pval);
			}

			break;
		}


		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Very good */
			if (power > 1)
			{
				/* Hack -- super-charge the damage dice */
				while (o_ptr->dd < 9 && !rand_int(10L * o_ptr->dd * o_ptr->ds))
				{
					o_ptr->dd++;
				}
			}

			break;
		}
	}
}

static void dragon_resist(object_type * o_ptr)
{
    do
    {
        artifact_bias = 0;

        if (randint(4)==1)
            random_resistance(o_ptr, FALSE, rand_range(5, 18));
        else
            random_resistance(o_ptr, FALSE, rand_range(17, 38));
        }
        while (one_in(2));
}


/*
 * Apply magic to an item known to be "armor"
 */
static void a_m_aux_2(object_type *o_ptr, const int level, const int power)
{
	const int toac1 = randint(5) + m_bonus(5, level);

	const int toac2 = m_bonus(10, level);

    artifact_bias = 0;

	/* Good */
	if (power > 0)
	{
		/* Enchant */
		o_ptr->to_a += toac1;

		/* Very good */
		if (power > 1)
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
		if (power < -1)
		{
			/* Penalize again */
			o_ptr->to_a -= toac2;
		}

		/* Cursed (if "bad") */
		if (o_ptr->to_a < 0) o_ptr->ident |= (IDENT_CURSED);
	}
}

typedef const struct bonus_type bonus_type;
struct bonus_type
{
	s16b k_idx; /* Object. */
	s16b min; /* Gets a bonus of rand_range(min, max)+m_bonus(bonus, level) */
	s16b max; /* "" */
	s16b bonus; /* "", should always be >=0, but is signed anyway. */
	s16b var; /* The variable, as in the definitions below. */
	s16b power; /* Cursed status, see BT_* flags. */
};

#define BV_PVAL	1 /* Affects pval */
#define BV_TO_H 2 /* Affects to hit bonus */
#define BV_TO_D 3 /* Affects damage bonus */
#define BV_TO_A 4 /* Affects armour bonus */

#define BT_CURSED -1 /* Object always generated cursed. */
#define BT_VARY 0 /* Object generated cursed or uncursed according to "power". */
#define BT_UNCURSED 1 /* Object always generated uncursed. */

/* A large table of modifiers.
 * Power acts as a modifier for the bonus. If power+bonus_type.power < 0,
 * the object becomes cursed and the current modifier is negated.
 * The power plays no other role at present.
 */
static bonus_type bonus_table[] =
{
	{OBJ_RING_EXTRA_ATTACKS, 0, 0, 3, BV_PVAL, BT_VARY},
	{OBJ_RING_INC_STR, 1, 1, 5, BV_PVAL, BT_VARY},
	{OBJ_RING_INC_CON, 1, 1, 5, BV_PVAL, BT_VARY},
	{OBJ_RING_INC_DEX, 1, 1, 5, BV_PVAL, BT_VARY},
	{OBJ_RING_INC_INT, 1, 1, 5, BV_PVAL, BT_VARY},
	{OBJ_RING_SPEED, 1, 5, 5, BV_PVAL, BT_VARY},
	{OBJ_RING_LORDLY_PROTECTION, 11, 15, 10, BV_TO_A, BT_UNCURSED},
	{OBJ_RING_SEARCHING, 1, 1, 5, BV_PVAL, BT_VARY},
	{OBJ_RING_FIRE, 6, 10, 10, BV_TO_A, BT_UNCURSED},
	{OBJ_RING_ACID, 6, 10, 10, BV_TO_A, BT_UNCURSED},
	{OBJ_RING_ICE, 6, 10, 10, BV_TO_A, BT_UNCURSED},
	{OBJ_RING_DEC_STR, 1, 1, 5, BV_PVAL, BT_CURSED},
	{OBJ_RING_DEC_INT, 1, 1, 5, BV_PVAL, BT_CURSED},
	{OBJ_RING_WOE, 1, 1, 5, BV_PVAL, BT_CURSED},
	{OBJ_RING_WOE, 5, 5, 5, BV_TO_A, BT_CURSED},
	{OBJ_RING_DAMAGE, 6, 13, 10, BV_TO_D, BT_VARY},
	{OBJ_RING_ACCURACY, 6, 13, 10, BV_TO_H, BT_VARY},
	{OBJ_RING_PROTECTION, 6, 13, 10, BV_TO_A, BT_VARY},
	{OBJ_RING_SLAYING, 1, 7, 10, BV_TO_H, BT_VARY},
	{OBJ_RING_SLAYING, 1, 7, 10, BV_TO_D, BT_VARY},
	{OBJ_AMULET_BRILLIANCE, 1, 1, 5, BV_PVAL, BT_VARY},
	{OBJ_AMULET_INC_CHR, 1, 1, 5, BV_PVAL, BT_VARY},
	{OBJ_AMULET_ANTI_MAGIC, 0, 0, 0, BV_PVAL, BT_VARY},
	{OBJ_AMULET_ANTI_TELEPORTATION, 0, 0, 0, BV_PVAL, BT_VARY},
	{OBJ_AMULET_SEARCHING, 1, 5, 5, BV_PVAL, BT_VARY},
	{OBJ_AMULET_THE_MAGI, 1, 5, 5, BV_PVAL, BT_UNCURSED},
	{OBJ_AMULET_THE_MAGI, 1, 5, 5, BV_TO_A, BT_UNCURSED},
	{OBJ_AMULET_DOOM, 1, 5, 5, BV_PVAL, BT_CURSED},
	{OBJ_AMULET_DOOM, 1, 5, 5, BV_TO_A, BT_CURSED},
	{OBJ_WOODEN_TORCH, 1, 4000, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_BRASS_LANTERN, 1, 7500, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_HEAL_MONSTER, 9, 28, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_HASTE_MONSTER, 9, 28, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_CLONE_MONSTER, 4, 8, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_TELEPORT_OTHER, 7, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_DISARMING, 5, 9, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_TRAP_DOOR_DESTRUCTION, 7, 14, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_STONE_TO_MUD, 4, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_LIGHT, 7, 16, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_SLEEP_MONSTER, 9, 23, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_SLOW_MONSTER, 7, 16, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_CONFUSE_MONSTER, 7, 18, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_SCARE_MONSTER, 4, 8, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_DRAIN_LIFE, 4, 6, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_POLYMORPH, 7, 14, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_STINKING_CLOUD, 7, 14, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_MAGIC_MISSILE, 7, 16, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_ACID_BOLT, 7, 14, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_TAME_MONSTER, 3, 8, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_FIRE_BOLT, 7, 14, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_COLD_BOLT, 7, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_ACID_BALL, 3, 7, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_ELEC_BALL, 5, 12, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_FIRE_BALL, 3, 6, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_COLD_BALL, 3, 8, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_WONDER, 9, 23, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_ANNIHILATION, 2, 3, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_DRAGON_FIRE, 2, 4, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_DRAGON_COLD, 2, 4, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_DRAGON_BREATH, 2, 4, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_WAND_SHARD_BALL, 2, 3, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_DARKNESS, 9, 16, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_SLOWNESS, 9, 16, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_HASTE_MONSTERS, 9, 16, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_SUMMONING, 2, 4, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_TELEPORTATION, 6, 9, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_PERCEPTION, 6, 20, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_REMOVE_CURSE, 5, 7, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_STARLIGHT, 7, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_LIGHT, 9, 28, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_ENLIGHTENMENT, 6, 10, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_TREASURE_LOCATION, 9, 28, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_OBJECT_LOCATION, 7, 21, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_TRAP_LOCATION, 7, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_DOOR_STAIR_LOCATION, 7, 14, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_DETECT_INVIS, 9, 23, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_DETECT_EVIL, 9, 23, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_CURE_LIGHT, 7, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_CURING, 5, 7, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_HEALING, 2, 3, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_THE_MAGI, 3, 4, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_SLEEP_MONSTERS, 7, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_SLOW_MONSTERS, 7, 11, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_SPEED, 5, 7, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_PROBING, 3, 8, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_DISPEL_EVIL, 5, 7, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_POWER, 2, 4, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_HOLINESS, 3, 4, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_GENOCIDE, 2, 3, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_EARTHQUAKES, 4, 8, 0, BV_PVAL, BT_UNCURSED},
	{OBJ_STAFF_STAR_DESTRUCTION, 2, 4, 0, BV_PVAL, BT_UNCURSED},
};

/*
 * Return a pointer to the specified variable within an object_type, or
 * 0 if unknown.
 */
static s16b *get_bonus_idx(object_type *o_ptr, int var)
{
	switch (var)
	{
		case BV_PVAL: return &o_ptr->pval;
		case BV_TO_H: return &o_ptr->to_h;
		case BV_TO_D: return &o_ptr->to_d;
		case BV_TO_A: return &o_ptr->to_a;
		default: return 0;
	}
}

/*
 * Check that bonus_table[] contains no unidentified variables.
 */
#ifdef CHECK_ARRAYS
void check_bonus_table(void)
{
	object_type o_ptr[1];
	bonus_type *b_ptr;
	for (b_ptr = bonus_table; b_ptr < END_PTR(bonus_table); b_ptr++)
	{
		if (!get_bonus_idx(o_ptr, b_ptr->var))
		{
			quit_fmt("Unidentified variable index %d found in bonus_table[%d]",
				b_ptr->var, b_ptr-bonus_table);
		}
		if (b_ptr->bonus < 0)
		{
			quit_fmt("Negative m_bonus %d found in bonus_table[%d]",
				b_ptr->bonus, b_ptr-bonus_table);
		}
	}
}
#endif /* CHECK_ARRAYS */

/*
 * Apply a modification from bonus_table[] to an object.
 */
static void set_var_aux(object_type *o_ptr, bonus_type *b_ptr, const int level,
	int power)
{
	s16b *var = get_bonus_idx(o_ptr, b_ptr->var);

	/* Allow the table to specify whether the object is cursed or not. */
	if (b_ptr->power != BT_VARY) power = b_ptr->power;

	/* Set the variable for an uncursed object. */
	(*var) = rand_range(b_ptr->min, b_ptr->max) + m_bonus(b_ptr->bonus, level);

	/* Apply a curse. */
	if (power < 0)
	{
		/* Broken */
		o_ptr->ident |= (IDENT_BROKEN);

		/* Cursed */
		o_ptr->ident |= (IDENT_CURSED);

		(*var) *= -1;
	}
}

static void set_var(object_type *o_ptr, const int level, const int power)
{
	bonus_type *b_ptr;
	for (b_ptr = bonus_table; b_ptr < END_PTR(bonus_table); b_ptr++)
	{
		if (b_ptr->k_idx == o_ptr->k_idx)
			set_var_aux(o_ptr, b_ptr, level, power);
	}
}


/*
 * Apply magic to an item known not to be a weapon or armour.
 *
 * Hack -- note that some items must be cursed (or blessed)
 * Hack -- note the special code for various items
 */
static void a_m_aux_3(object_type *o_ptr, const int level, const int power)
{
	/* Apply magic (good or bad) according to type */
	if (o_ptr->k_idx == OBJ_WOODEN_TORCH || o_ptr->k_idx == OBJ_BRASS_LANTERN
		|| o_ptr->tval == TV_CHEST)
	{
		if (o_ptr->pval) o_ptr->pval = randint(o_ptr->pval);
	}

	/* Set most of the values as in the table above. */
	set_var(o_ptr, level, power);
}

/*
 * Return TRUE if an object can be cursed by a_m_aux_3().
 * Only set_var() can do this at present.
 */
static bool PURE aux3_can_curse(int k_idx)
{
	bonus_type *b_ptr;
	FOR_ALL_IN(bonus_table, b_ptr)
	{
		if (b_ptr->k_idx == k_idx && b_ptr->power == BT_VARY) return TRUE;
	}
	return FALSE;
}


/*
 * Set a quark to the depth of the item in question, return it.
 */
static u16b depth_string(void)
{
	/* Not wanted. */
	if (!inscribe_depth) return 0;

	/* Suppress on the surface (I think I prefer it this way...). */
	else if (!dun_level) return 0;

	/* Use the normal depth format from prt_depth() */
	else if (depth_in_feet) return quark_add(format("%d'", dun_depth*50));

	else return quark_add(format("L%d", dun_depth));
}


/*
 * Add a special ego effect to an object (which may have negative pval, etc.).
 * Returns FALSE if the effect was unidentified.
 */
static bool add_ego_special(object_type *o_ptr, const byte special,
	const int level)
{
	/* Hack - handle special ego types. */
	switch (special)
	{
		case E_SPEC_ELDER_SIGN:
		{
			add_sustain(o_ptr);
			if (rand_int(4)) return TRUE;
			o_ptr->flags1 |= TR1_BLOWS;
			if (o_ptr->pval > 2) o_ptr->pval -= randint(2);
			return TRUE;
		}
		case E_SPEC_DF:
		{
			add_sustain(o_ptr);
			if (one_in(3)) o_ptr->flags2 |= TR2_RES_POIS;
			random_resistance(o_ptr, FALSE, ((randint(22))+16));
			return TRUE;
		}
		case E_SPEC_PLANAR:
		{
			if (one_in(7)) add_ability(o_ptr);
			if (one_in(5)) o_ptr->flags1 |= TR1_SLAY_DEMON;
			random_resistance(o_ptr, FALSE, ((randint(22))+16));
			o_ptr->activation = ACT_TELEPORT_WAIT;
			return TRUE;
		}
		case E_SPEC_KADATH:
		{
			if (one_in(3)) o_ptr->flags2 |= TR2_RES_FEAR;
			return TRUE;
		}
		case E_SPEC_SLAYING:
		{
			if (one_in(3))
			{
				o_ptr->dd *= 2;
			}
			else
			{
				do { o_ptr->dd++; } while (one_in(o_ptr->dd));
				do { o_ptr->ds++; } while (one_in(o_ptr->ds));
			}
			if (one_in(5)) o_ptr->flags1 |= TR1_BRAND_POIS;
			if (o_ptr->tval == TV_SWORD && one_in(3))
			{
				o_ptr->flags1 |= TR1_VORPAL;
			}
			return TRUE;
		}
		case E_SPEC_DRAGON_BANE:
		{
			random_resistance(o_ptr, FALSE, rand_range(5, 16));
			random_resistance(o_ptr, FALSE, rand_range(5, 18));
			if (one_in(3)) o_ptr->flags2 |= TR2_RES_POIS;
			return TRUE;
		}
		case E_SPEC_A_RESISTANCE:
		{
			if (one_in(4)) o_ptr->flags2 |= TR2_RES_POIS;
			random_resistance(o_ptr, FALSE, rand_range(17,38));
			return TRUE;
		}
		case E_SPEC_S_RESISTANCE:
		{
			if (one_in(4)) o_ptr->flags2 |= TR2_RES_POIS;
			random_resistance(o_ptr, FALSE, rand_range(5,38));
			return TRUE;
		}
		case E_SPEC_SEEING:
		{
			if (one_in(3)) o_ptr->flags3 |= TR3_TELEPATHY;
			return TRUE;
		}
		case E_SPEC_SHARPNESS:
		{
			o_ptr->pval = m_bonus(5, level)+1;
			return TRUE;
		}
		case E_SPEC_EARTHQUAKES:
		{
			o_ptr->pval = m_bonus(3, level);
			return TRUE;
		}
		case E_SPEC_LAW:
		{
			random_resistance(o_ptr, FALSE, rand_range(17, 38));
			if (one_in(3)) o_ptr->flags2 |= TR2_HOLD_LIFE;
			if (one_in(3)) o_ptr->flags1 |= TR1_DEX;
			if (one_in(5)) o_ptr->flags2 |= TR2_RES_FEAR;
			return TRUE;
		}
		case E_SPEC_LENG:
		{
			if (one_in(6)) o_ptr->flags3 |= TR3_TY_CURSE;
			return TRUE;
		}
		case E_SPEC_DIE:
		{
			o_ptr->dd++;
			return TRUE;
		}
		case E_SPEC_AM_RESISTANCE:
		{
			if (one_in(3)) random_resistance(o_ptr, FALSE, rand_range(5, 38));
			if (one_in(5)) o_ptr->flags2 |= TR2_RES_POIS;
			return TRUE;
		}
		case E_SPEC_AM_THE_MAGI:
		{
			if (one_in(3)) o_ptr->flags3 |= TR3_SLOW_DIGEST;
			return TRUE;
		}
		case E_SPEC_RING_LORDLY:
		{
			do
			{
				random_resistance(o_ptr, FALSE, rand_range(19, 38));
			}
			while (one_in(4));
			return TRUE;
		}
		case E_SPEC_RING_SPEED:
		{
			/* Cursed rings can become very bad, uncursed ones very good. */
			int mod = (cursed_p(o_ptr)) ? -1 : 1;

			/* Super-charge the ring */
			while (one_in(2)) o_ptr->pval += mod;
			return TRUE;
		}
		case E_SPEC_RING_EXTRA_ATTACKS:
		{
			/* Always change a pval of 0 to 1 or -1. */
			if (o_ptr->pval);
			else if (cursed_p(o_ptr)) o_ptr->pval--;
			else o_ptr->pval++;
			return TRUE;
		}
		case E_SPEC_DRAGON_RESIST:
		{
			dragon_resist(o_ptr);
			return TRUE;
		}
		case E_SPEC_RNDPVAL:
		{
			o_ptr->pval = rand_range(1, o_ptr->pval);
			return TRUE;
		}
		/* Hack - hide the fact that this was a very good/bad item. */
		case E_SPEC_NO_EGO:
		{
			o_ptr->name2 = 0;
			return TRUE;
		}
		/* Hack - turn into a randart, not an ego item. */
		case E_SPEC_RANDART:
		{
		    artifact_bias = 0;
			o_ptr->name2 = 0;
			create_artifact(o_ptr, FALSE);
			return TRUE;
		}
		case E_SPEC_ABILITY:
		{
			add_ability(o_ptr);
			return TRUE;
		}
		case E_SPEC_POWER:
		{
			add_power(o_ptr);
			return TRUE;
		}
		case E_SPEC_SUSTAIN:
		{
			add_sustain(o_ptr);
			return TRUE;
		}
		case E_SPEC_HIGH:
		{
			random_resistance(o_ptr, FALSE, rand_range(17,38));
			return TRUE;
		}
		case E_SPEC_LOW:
		{
			random_resistance(o_ptr, FALSE, rand_range(5, 16));
		}
		case E_SPEC_RESIST:
		{
			random_resistance(o_ptr, FALSE, rand_range(5, 38));
			return TRUE;
		}
		/* Nothing. */
		case 0:
		{
			return TRUE;
		}
		/* Unidentified power. */
		default:
		{
			return FALSE;
		}
	}
}

/*
 * Return the fake "ego_item_type.special" contained in some object kinds.
 */
static byte k_ego_special(object_kind *k_ptr)
{
	switch (k_ptr->tval)
	{
		case TV_BOOTS: case TV_GLOVES: case TV_HELM: case TV_CROWN:
		case TV_SHIELD: case TV_CLOAK: case TV_SOFT_ARMOR: case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR: case TV_AMULET: case TV_RING:
		{
			return k_ptr->extra;
		}
		default:
		{
			return 0;
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
 * In particular, note that "Instant Artifacts", if "created" by an external
 * routine, must pass through this function to complete the actual creation.
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

/*
 * Turn an ordinary object into an ego item or artefact, and give it appropriate
 * bonuses.
 */
static void apply_magic_1(object_type *o_ptr, const int lev, const bool okay,
	const bool good, const bool great)
{
	int i, rolls, f1, f2, power;

	/* Normal artefacts have no business here. */
	if (o_ptr->name1) return;


	/* Base chance of being "good" */
	f1 = lev + 10;

	/* Maximal chance of being "good" */
	if (f1 > 75) f1 = 75;

	/* Base chance of being "great" */
	f2 = f1 / 2;

	/* Maximal chance of being "great" */
	if (f2 > 20) f2 = 20;


	/* Assume normal */
	power = 0;

	/* Roll for "good" */
	if (good || magik(f1))
	{
		/* Assume "good" */
		power = 1;

		/* Roll for "great" */
		if (great || magik(f2)) power = 2;
	}

	/* Roll for "cursed" */
	else if (magik(f1))
	{
		/* Assume "cursed" */
		power = -1;

		/* Roll for "broken" */
		if (magik(f2)) power = -2;
	}


	/* Assume no rolls */
	rolls = 0;

	/* Get one roll if excellent */
	if (power >= 2) rolls = 1;

	/* Hack -- Get four rolls if forced great */
	if (great) rolls = 4;

	/* Hack -- Get no rolls if not allowed */
	if (!okay) rolls = 0;

	/* Roll for artifacts if allowed */
	for (i = 0; i < rolls; i++)
	{
		errr err;
		if (cheat_peek) msg_format("Rolling %d", i);
		/* Roll for an artifact */
		err = make_artifact(o_ptr, FALSE);

		/* Continue if the roll went against us. */
		if (err == MAKE_ART_RANDOM_FAIL) continue;

		/* Return if an artefact has been produced. */
		else if (err == SUCCESS) return;

		/* Finish if it failed for some other reason. */
		else break;
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
			a_m_aux_1(o_ptr, lev, power);
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
			a_m_aux_2(o_ptr, lev, power);
			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			if (!power && (rand_int(100) < 50)) power = -1;
		}
		/* Fall through... */

		default:
		{
			a_m_aux_3(o_ptr, lev, power);
			break;
		}
	}

	/* Turn into an ego item, if allowed. */
	if (ABS(power) > 1) o_ptr->name2 = get_ego_item(o_ptr, lev, power < -1);
}

/*
 * Put the finishing touches on ego items and artefacts, and give random
 * bonuses to a few rings, amulets and armour.
 */
void apply_magic_2(object_type *o_ptr, const int lev)
{
	object_kind *k_ptr = k_info+o_ptr->k_idx;
	artifact_type *a_ptr = a_info+o_ptr->name1;
	ego_item_type *e_ptr = e_info+o_ptr->name2;

	/* Hack -- analyze artifacts */
	if (o_ptr->name1)
	{
		/* Hack -- Mark the artifact as "created" */
		a_ptr->cur_num = 1;

		/* Hack -- some artifacts get random extra powers */
		random_artifact_resistance(o_ptr);

		/* Extract the other fields */
		o_ptr->pval = a_ptr->pval;
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;
		o_ptr->to_a = a_ptr->to_a;
		o_ptr->to_h = a_ptr->to_h;
		o_ptr->to_d = a_ptr->to_d;
		o_ptr->weight = a_ptr->weight;

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- extract the "cursed" flag */
		if (a_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

		/* Mega-Hack -- increase the rating */
		rating += 10;

		/* Mega-Hack -- increase the rating again */
		if (a_ptr->cost > 50000L) rating += 10;

		/* Set the good item flag */
		good_item_flag = TRUE;

		/* Cheat -- describe the item */
		object_mention(o_ptr);
	}

	/* Hack -- analyze ego-items */
	if (o_ptr->name2)
	{
		/* Hack -- acquire "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (e_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

		/* Hack -- apply extra penalties if needed */
		if (cursed_p(o_ptr) || broken_p(o_ptr))
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h) o_ptr->to_h -= randint(e_ptr->max_to_h);
			if (e_ptr->max_to_d) o_ptr->to_d -= randint(e_ptr->max_to_d);
			if (e_ptr->max_to_a) o_ptr->to_a -= randint(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval) o_ptr->pval -= randint(e_ptr->max_pval);
		}

		/* Hack -- apply extra bonuses if needed */
		else
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h) o_ptr->to_h += randint(e_ptr->max_to_h);
			if (e_ptr->max_to_d) o_ptr->to_d += randint(e_ptr->max_to_d);
			if (e_ptr->max_to_a) o_ptr->to_a += randint(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval) o_ptr->pval += randint(e_ptr->max_pval);
		}

		/* Add special effects from the ego type. */
		add_ego_special(o_ptr, e_ptr->special, lev);

		/* Hack -- apply rating bonus */
		rating += e_ptr->rating;

		/* Cheat -- describe the item */
		object_mention(o_ptr);
	}


	/* Examine real objects */
	if (o_ptr->k_idx && !o_ptr->name1 && !o_ptr->name2)
	{
		/* Hack -- acquire "broken" flag */
		if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

		/* Describe if a rating boost is to be given. */
		if (k_ptr->rating) object_mention(o_ptr);

		/* Add special effects for rings and amulets. */
		add_ego_special(o_ptr, k_ego_special(k_ptr), lev);
	}

	/* Give a rating boost for all items apart from artefacts (?). */
	if (o_ptr->k_idx && !o_ptr->name1)
	{
		rating += k_ptr->rating;
	}

}


void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great)
{
	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

	/* Turn the object into an ego-item or artefact (or not). */
	apply_magic_1(o_ptr, lev, okay, good, great);

	/* Hack - inscribe with creation depth if desired. */
	o_ptr->note = depth_string();

	/* Give it any bonuses its ego- or artefact type requires, and add random
	 * bonuses to rings and armour. */
	apply_magic_2(o_ptr, lev);
}

/*
 * Return TRUE if apply_magic() can leave an object either cursed or uncursed.
 * This is deficient
 */
bool PURE magic_can_curse(int k_idx)
{
	switch (k_info[k_idx].tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOW:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
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
			/* a_m_aux_1() and a_m_aux_2() can curse almost anything. */
			return TRUE;
		}
		default:
		{
			return aux3_can_curse(k_idx) || ego_can_curse(k_idx);
		}
	}
}


/*
 * Hack -- determine if a template is "good"
 */
static bool PURE kind_is_good(int k_idx)
{
	return ((k_info[k_idx].flags3 & TR3_GOOD) != 0);
}




/*
 * Attempt to make an object (normal or good/great)
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "object_level" for the "generation level".
 *
 * We assume that the given object has been "wiped".
 */
bool make_object(object_type *j_ptr, bool good, bool great)
{
	int prob, base;


	/* Chance of "special object" */
	prob = (good ? 10 : 1000);

	/* Base level for the object */
	base = (good ? (object_level + 10) : object_level);


	/* Generate a special object, or a normal object */
	if ((rand_int(prob) != 0) || (make_artifact(j_ptr, TRUE) != SUCCESS))
	{
		int k_idx;

		/* Good objects */
		if (good)
		{
			/* Prepare "good" allocation table */
			get_obj_num_prep(kind_is_good);
		}

		/* Pick a random object */
		k_idx = get_obj_num(base);

		/* Good objects */
		if (good)
		{
			/* Prepare normal allocation table */
			get_obj_num_prep(NULL);
		}

		/* Handle failure */
		if (!k_idx) return (FALSE);

		/* Prepare the object */
		object_prep(j_ptr, k_idx);
	}

	/* Apply magic (allow artifacts) */
	apply_magic(j_ptr, object_level, TRUE, good, great);

	/* Hack -- generate multiple spikes/missiles */
	switch (j_ptr->tval)
	{
		case TV_SPIKE:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			j_ptr->number = (byte)(damroll(6, 7));
		}
	}

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) &&
	    (object_k_level(k_info+j_ptr->k_idx) > (dun_depth)))
	{
		/* Rating increase */
		rating += (object_k_level(k_info+j_ptr->k_idx) - (dun_depth));

		/* Cheat -- peek at items */
		object_mention(j_ptr);
	}
	
	/* Set the stack field for the new object. */
	set_stack_number(j_ptr);

	/* Success */
	return (TRUE);
}



/*
 * Attempt to place an object (normal or good/great) at the given location.
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "object_level" for the "generation level".
 *
 * This routine requires a clean floor grid destination.
 */
void place_object(int y, int x, bool good, bool great)
{
	cave_type *c_ptr;

	object_type forge;
	object_type *q_ptr, *o_ptr;


	/* Paranoia -- check bounds */
	if (!in_bounds(y, x)) return;

	/* Require clean floor space */
	if (!cave_clean_bold(y, x)) return;


	/* Get local object */
	q_ptr = &forge;

	/* Wipe the object */
	object_wipe(q_ptr);

	/* Make an object (if possible) */
	if (!make_object(q_ptr, good, great)) return;


	/* Make an object */
	o_ptr = o_pop();

	/* Success */
	if (o_ptr)
	{
		/* Structure Copy */
		object_copy(o_ptr, q_ptr);

		/* Location */
		o_ptr->iy = y;
		o_ptr->ix = x;

		/* Acquire grid */
		c_ptr = &cave[y][x];

		/* Build a stack */
		o_ptr->next_o_idx = c_ptr->o_idx;

		/* Place the object */
		c_ptr->o_idx = o_ptr - o_list;

		/* Notice */
		note_spot(y, x);
		
		/* Redraw */
		lite_spot(y, x);
	}
    else
    {
			/* Hack -- Preserve artifacts */
            if (q_ptr->name1)
            {
                a_info[q_ptr->name1].cur_num = 0;
            }
    }

}





/*
 * XXX XXX XXX Do not use these hard-coded values.
 */
#define OBJ_GOLD_LIST	OBJ_COPPER	/* First "gold" entry */
#define MAX_GOLD (OBJ_ADAMANTITE-OBJ_COPPER+1) /* Number of "gold" entries */

/*
 * Make a treasure object
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_gold(object_type *j_ptr)
{
	int i;

	s32b base;


	/* Hack -- Pick a Treasure variety */
	i = ((randint(object_level + 2) + 2) / 2) - 1;

	/* Apply "extra" magic */
	if (rand_int(GREAT_OBJ) == 0)
	{
		i += randint(object_level + 1);
	}

	/* Hack -- Creeping Coins only generate "themselves" */
	if (coin_type) i = coin_type;

	/* Do not create "illegal" Treasure Types */
	if (i >= MAX_GOLD) i = MAX_GOLD - 1;

	/* Prepare a gold object */
	object_prep(j_ptr, OBJ_GOLD_LIST + i);

	/* Hack -- Base coin cost */
	base = k_info[OBJ_GOLD_LIST+i].cost;

	/* Determine how much the treasure is "worth" */
	j_ptr->pval = (base + (8L * randint(base)) + randint(8));

	/* Success */
	return (TRUE);
}


/*
 * Places a treasure (Gold or Gems) at given location
 *
 * The location must be a legal, clean, floor grid.
 */
void place_gold(int y, int x)
{
	cave_type *c_ptr;

	object_type forge;
	object_type *q_ptr, *o_ptr;


	/* Paranoia -- check bounds */
	if (!in_bounds(y, x)) return;

	/* Require clean floor space */
	if (!cave_clean_bold(y, x)) return;


	/* Get local object */
	q_ptr = &forge;

	/* Wipe the object */
	object_wipe(q_ptr);

	/* Make some gold */
	if (!make_gold(q_ptr)) return;


	/* Make an object */
	o_ptr = o_pop();

	/* Success */
	if (o_ptr)
	{
		/* Copy the object */
		object_copy(o_ptr, q_ptr);

		/* Save location */
		o_ptr->iy = y;
		o_ptr->ix = x;

		/* Acquire grid */
		c_ptr = &cave[y][x];

		/* Build a stack */
		o_ptr->next_o_idx = c_ptr->o_idx;

		/* Place the object */
		c_ptr->o_idx = o_ptr - o_list;

		/* Notice */
		note_spot(y, x);
		
		/* Redraw */
		lite_spot(y, x);
	}
}

/*
 * Mark the landing place of a dropped item.
 */
static void drop_near_finish(int chance, int by, int bx)
{
	/* Note the spot */
	note_spot(by, bx);

	/* Draw the spot */
	lite_spot(by, bx);

	/* Sound */
	sound(SOUND_DROP);

	/* Mega-Hack -- no message if "dropped" by player */
	/* Message when an object falls under the player */
	if (chance && (by == py) && (bx == px))
	{
		msg_print("You feel something roll beneath your feet.");
	}

	/* Window stuff (if needed) */
	if (by == py && bx == px) cave_track(by, bx);
}

/*
 * Let an object fall to the ground at or near a location.
 *
 * The initial location is assumed to be "in_bounds()".
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
object_type *drop_near(object_type *j_ptr, int chance, int y, int x)
{
	object_type *ou_ptr;

	int i, k, d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;

	s16b this_o_idx, next_o_idx = 0;

	cave_type *c_ptr;

	bool flag = FALSE;

	bool plural = FALSE;


	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;


	/* Handle normal "breakage" */
    if (!(allart_p(j_ptr)) && (rand_int(100) < chance))
	{
		/* Message */
		msg_format("The %v disappear%s.",
			object_desc_f3, j_ptr, FALSE, 0, (plural ? "" : "s"));

		/* Debug */
		if (cheat_wzrd) msg_print("(breakage)");

        /* Failure */
		return NULL;
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
			if (!in_bounds(ty, tx)) continue;

			/* Require line of sight */
			if (!los(y, x, ty, tx)) continue;

			/* Obtain grid */
			c_ptr = &cave[ty][tx];

			/* Require floor space */
			if (c_ptr->feat != FEAT_FLOOR) continue;

			/* No objects */
			k = 0;

			/* Scan objects in that grid */
			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;
					
				/* Acquire object */
				o_ptr = &o_list[this_o_idx];
						
				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Check for possible combination */
				if (object_similar(o_ptr, j_ptr)) comb = TRUE;

				/* Count objects */
				k++;
			}

			/* Add new object */
			if (!comb) k++;

			/* No stacking (allow combining) */
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
			if ((++bn >= 2) && (rand_int(bn) != 0)) continue;

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
    if (!flag && !(allart_p(j_ptr)))
	{
		/* Message */
		msg_format("The %v disappear%s.",
			   object_desc_f3, j_ptr, FALSE, 0, (plural ? "" : "s"));

		/* Debug */
		if (cheat_wzrd) msg_print("(no floor space)");

		return NULL;
	}


	/* Find a grid */
	for (i = 0; !flag; i++)
	{
		/* Bounce around */
		if (i < 1000)
		{
			ty = rand_spread(by, 1);
			tx = rand_spread(bx, 1);
		}
		
		/* Random locations */
		else
		{
			ty = rand_int(cur_hgt);
			tx = rand_int(cur_wid);
		}

		/* Grid */
		c_ptr = &cave[ty][tx];

		/* Require floor space */
		if (c_ptr->feat != FEAT_FLOOR) continue;

		/* Bounce to that location */
		by = ty;
		bx = tx;

		/* Require floor space */
		if (!cave_clean_bold(by, bx)) continue;

		/* Okay */
		flag = TRUE;
	}


	/* Grid */
	c_ptr = &cave[by][bx];

	/* Scan objects in that grid for combination */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;
	
		/* Acquire object */
		o_ptr = &o_list[this_o_idx];
		
		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;
		
		/* Check for combination */
		if (object_similar(o_ptr, j_ptr))
		{
			/* Combine the items */
			(void)object_absorb(o_ptr, j_ptr);

			/* Clean up. */
			drop_near_finish(chance, by, bx);

			/* Result */
			return o_ptr;
		}
	}

	/* Get new object if not absorbed. */
	ou_ptr = o_pop();

	/* Failure */
	if (!ou_ptr)
	{
		/* Message */
		msg_format("The %v disappear%s.",
			   object_desc_f3, j_ptr, FALSE, 0, (plural ? "" : "s"));

		/* Debug */
		if (cheat_wzrd) msg_print("(too many objects)");

        /* Hack -- Preserve artifacts */
        if (j_ptr->name1)
        {
            a_info[j_ptr->name1].cur_num = 0;
        }

		/* Failure */
		return NULL;
	}

	/* Structure copy */
	object_copy(ou_ptr, j_ptr);

	/* Access new object */
	j_ptr = ou_ptr;

	/* Locate */
	j_ptr->iy = by;
	j_ptr->ix = bx;

	/* No monster */
	j_ptr->held_m_idx = 0;

	/* Build a stack */
	j_ptr->next_o_idx = c_ptr->o_idx;

	/* Place the object */
	c_ptr->o_idx = ou_ptr - o_list;

	/* Clean up. */
	drop_near_finish(chance, by, bx);

	/* Result */
	return ou_ptr;
}




/*
 * Scatter some "great" objects near the player
 */
void acquirement(int y1, int x1, int num, bool great)
{
	object_type forge;
	object_type *q_ptr;

	/* Acquirement */
	while (num--)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Make a good (or great) object (if possible) */
		if (!make_object(q_ptr, TRUE, great)) continue;

		/* Drop the object */
		drop_near(q_ptr, -1, y1, x1);
	}
}



/*
 * Hack -- instantiate a trap
 *
 * XXX XXX XXX This routine should be redone to reflect trap "level".
 * That is, it does not make sense to have spiked pits at 50 feet.
 * Actually, it is not this routine, but the "trap instantiation"
 * code, which should also check for "trap doors" on quest levels.
 */
void pick_trap(int y, int x)
{
	int feat;

	cave_type *c_ptr = &cave[y][x];

	/* Paranoia */
	if (c_ptr->feat != FEAT_INVIS) return;

	/* Pick a trap */
	while (1)
	{
		/* Hack -- pick a trap */
		feat = FEAT_TRAP_HEAD + rand_int(16);

		/* Hack -- no trap doors on quest levels */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && is_quest(dun_level)) continue;

		/* Hack -- no trap doors on the deepest level */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && (dun_level >= dun_defs[cur_dungeon].max_level)) continue;

		/* Done */
		break;
	}

	/* Activate the trap */
	cave_set_feat(y, x, feat);
}



/*
 * Places a random trap at the given location.
 *
 * The location must be a legal, naked, floor grid.
 *
 * Note that all traps start out as "invisible" and "untyped", and then
 * when they are "discovered" (by detecting them or setting them off),
 * the trap is "instantiated" as a visible, "typed", trap.
 */
void place_trap(int y, int x)
{
	/* Paranoia -- verify location */
	if (!in_bounds(y, x)) return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;

	/* Place an invisible trap */
	cave_set_feat(y, x, FEAT_INVIS);
}






/*
 * Describe the charges on an item.
 */
void item_charges(object_ctype *o_ptr)
{
	cptr pref, plu;

	/* Require staff/wand */
	if ((o_ptr->tval != TV_STAFF) && (o_ptr->tval != TV_WAND)) return;

	/* Require known item */
	if (!object_known_p(o_ptr)) return;

	/* Choose the first two words. */
	if (is_inventory_p(o_ptr))
		pref = "You have";
	else if (o_ptr->pval == 1)
		pref = "There is";
	else
		pref = "There are";

	/* Decide whether to pluralise it. */
	if (o_ptr->pval == 1)
		plu = "";
	else
		plu = "s";

	/* Print the message. */
	msg_format("%s %d charge%s remaining.", pref, o_ptr->pval, plu);
}

/*
 * Increase the "number" of an item.
 */
void item_increase(object_type *o_ptr, int num)
{
	/* Apply */
	num += o_ptr->number;

	/* Bounds check */
	if (num > 255) num = 255;
	else if (num < 0) num = 0;

	/* Un-apply */
	num -= o_ptr->number;

	/* Change the number */
	o_ptr->number += num;

	/* Change the number and weight for inventory items. */
	if (find_object(o_ptr) & OUP_CARRIED_MASK)
	{
		/* Add the weight */
		total_weight += (num * o_ptr->weight);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr, 0);
	}
}

/*
 * Describe an item.
 */
void item_describe(object_ctype *o_ptr)
{
	cptr verb;

	switch (find_object(o_ptr))
	{
		case OUP_INVEN: case OUP_EQUIP: case OUP_POUCH:
		{
			verb = "have";
			break;
		}
		case OUP_FLOOR:
		{
			verb = "see";
			break;
		}
		/* Paranoia. */
		default:
		{
			if (alert_failure) msg_print("Unusual item requesed.");
			verb = "imagine";
		}
	}

	msg_format("You %s %v", verb, object_desc_f3, o_ptr, TRUE, 3);
}

/*
 * Optimize an item somewhere (destroy "empty" items)
 */
void item_optimize(object_type *o_ptr)
{
	/* Paranoia -- be sure it exists */
	if (!o_ptr->k_idx) return;

	/* Only optimize empty items */
	if (o_ptr->number) return;

	/* Floor. */
	if (!is_inventory_p(o_ptr))
	{
		/* Delete the object */
		delete_dun_object(o_ptr);
	}
	/* Inventory. */
	else if (o_ptr < inventory+INVEN_WIELD)
	{
		object_type *j_ptr;

		/* Slide everything down */
		for (j_ptr = o_ptr; j_ptr < inventory+INVEN_PACK; j_ptr++)
		{
			/* Structure copy */
			object_copy(j_ptr, j_ptr+1);
		}

		/* Erase the "final" slot */
		object_wipe(j_ptr);
		
		/* Stop tracking */
		object_track(j_ptr);
		
		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_SPELL);

	}
	/* Equipment. */
	else
	{
		/* Erase the empty slot */
		object_wipe(o_ptr);

		/* Stop tracking */
		object_track(o_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate torch */
		p_ptr->update |= (PU_TORCH);

		/* Recalculate mana XXX */
		p_ptr->update |= (PU_MANA);
		
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP | PW_SPELL);
	}
}


/*
 * Check if we have space for an item in the pack without overflow
 */
bool PURE inven_carry_okay(object_ctype *o_ptr)
{
	int j;

	/* Similar slot? */
	for (j = 0; j < INVEN_PACK; j++)
	{
		object_type *j_ptr = &inventory[j];

		/* Empty slot? */
		if (!j_ptr->k_idx) return TRUE;

		/* Check if the two items can be combined */
		if (object_similar(j_ptr, o_ptr)) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}

/* Forward declare. */
static bool ang_sort_comp_pack_aux(object_ctype *a_ptr, object_ctype *b_ptr);

/*
 * Set *j_ptr to the slot object o_ptr should go into in the player's
 * inventory.
 * Return TRUE if the objects can be combined, FALSE if it needs to be inserted.
 * 
 * This assumes that ang_sort_comp_pack_aux() and object_similar() will fail
 * for every object after the first one for which the former fails.
 */
static bool PURE inven_carry_aux(object_type **j_ptr, object_type *o_ptr)
{
	for (*j_ptr = inventory; *j_ptr <= inventory+INVEN_PACK; (*j_ptr)++)
	{
		/* An object which can absorb o_ptr. */
		if (object_similar(*j_ptr, o_ptr)) return TRUE;

		/* No object, or an object which will be later in the pack. */
		if (!ang_sort_comp_pack_aux(*j_ptr, o_ptr)) return FALSE;
	}

	/* Paranoia - the INVEN_PACK slot at least should be free. */
	*j_ptr = NULL;
	return FALSE;
}

/*
 * Add an item to the players inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", otherwise,
 * the item will be placed into the "proper" location in the inventory.
 *
 * This function can be used to "over-fill" the player's pack, but only
 * once, and such an action must trigger the "overflow" code immediately.
 *
 * Note that this code must remove any location/stack information
 * from the object once it is placed into the inventory.
 */
object_type *inven_carry(object_type *o_ptr)
{
	object_type     *j_ptr, *q_ptr;

	if (inven_carry_aux(&j_ptr, o_ptr))
	{
		object_absorb(j_ptr, o_ptr);
	}
	else if (j_ptr && !inventory[INVEN_PACK-1].k_idx)
	{
		/* Find the last real slot. */
		for (q_ptr = inventory+INVEN_PACK; q_ptr > j_ptr; q_ptr--)
		{
			if (q_ptr[-1].k_idx)
			{
				object_copy(q_ptr, q_ptr-1);
			}
		}
		object_copy(j_ptr, o_ptr);
	}
	else if (j_ptr)
	{
		/* No free slots, so leave it in the overflow slot. */
		j_ptr = &inventory[INVEN_PACK];
		object_copy(j_ptr, o_ptr);
	}
	else
	{
		/* Paranoia - the INVEN_PACK slot should be free. */
		return NULL;
	}

	/* Increase the weight */
	total_weight += (o_ptr->number * o_ptr->weight);

	/* Recalculate/redraw various things. */
	update_object(j_ptr, 0);

	return j_ptr;
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
object_type *inven_takeoff(object_type *o_ptr, int amt)
{
	object_type forge;
	object_type *q_ptr;

	cptr act;

	/* Get the item to take off */
	int item = o_ptr - inventory;

	/* Paranoia */
	if (amt <= 0)
	{
		return NULL;
	}

	/* Verify */
	if (amt > o_ptr->number) amt = o_ptr->number;

	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = amt;

	/* Took off weapon */
	if (item == INVEN_WIELD)
	{
		act = "You were wielding";
	}

	/* Took off bow */
	else if (item == INVEN_BOW)
	{
		act = "You were holding";
	}

	/* Took off light */
	else if (item == INVEN_LITE)
	{
		act = "You were holding";
	}

	/* Took off something */
	else
	{
		act = "You were wearing";
	}

	/* Carry the object */
	q_ptr = inven_carry(q_ptr);

	/* Successfully moved. */
	if (q_ptr)
	{
		/* Modify, Optimize */
		item_increase(o_ptr, -amt);
		item_optimize(o_ptr);
	}

	/* Message */
	msg_format("%s %v (%c).", act, object_desc_f3, q_ptr, TRUE, 3,
		index_to_label(q_ptr));

	/* Return slot */
	return q_ptr;
}




/*
 * Drop (some of) a non-cursed inventory/equipment item
 *
 * The object will be dropped "near" the current location
 */
void inven_drop(object_type *o_ptr, int amt)
{
	object_type forge;
	object_type *q_ptr;

	/* Error check */
	if (amt <= 0) return;

	/* Not too many */
	if (amt > o_ptr->number) amt = o_ptr->number;


	/* Take off equipment */
	if (o_ptr >= inventory+INVEN_WIELD)
	{
		/* Take off first */
		o_ptr = inven_takeoff(o_ptr, amt);
	}


	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = amt;

	/* Message */
	msg_format("You drop %v (%c).", object_desc_f3, q_ptr, TRUE, 3,
		index_to_label(o_ptr));

	/* Drop it near the player */
	q_ptr = drop_near(q_ptr, 0, py, px);

	/* Remember the object */
	object_track(q_ptr);

	/* Modify, Describe, Optimize */
	item_increase(o_ptr, -amt);
	item_describe(o_ptr);
	item_optimize(o_ptr);
}



/*
 * Combine items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void combine_pack(void)
{
	int             i, j, k;

	object_type     *o_ptr;
	object_type     *j_ptr;

	bool    flag = FALSE;


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

			/* Can we drop any of "o_ptr" onto "j_ptr"? */
			if (object_similar_2(j_ptr, o_ptr))
			{
				/* Add together the item counts, continue looking if o_ptr
				 * isn't empty. */
				if (!object_absorb(j_ptr, o_ptr)) continue;

				/* Take note */
				flag = TRUE;

				/* Slide everything down */
				for (k = i; k < INVEN_PACK; k++)
				{
					/* Structure copy */
					inventory[k] = inventory[k+1];
				}

				/* Erase the "final" slot */
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

/* Check k_idx as a macro. */
#define k_idx(O) ((O)->k_idx)

/* 
 * A custom macro for the below which return if either a_ptr or b_ptr fails
 * the PAR function/macro.
 */
#define SORT_BOOL(PAR) \
	a = !PAR(a_ptr); \
	b = !PAR(b_ptr); \
	if (a != b) return b; \
	else if (b) return old;

/*
 * Return TRUE if the desired position for a_ptr in the pack >=  that of b_ptr.
 */
static bool ang_sort_comp_pack_aux(object_ctype *a_ptr, object_ctype *b_ptr)
{
	s32b dif;

	/* Notice (and try to preserve) the order before this call. */
	bool a, b, old = (a_ptr->ix <= b_ptr->ix);

	/* Skip empty slots */
	SORT_BOOL(k_idx)

	/* Objects sort by decreasing type */
	dif = a_ptr->tval - b_ptr->tval;
	if (dif) return (dif > 0);

	/* Non-aware (flavored) items always come last */
	SORT_BOOL(object_aware_p);

	/* Objects sort by increasing k_idx */
	dif = a_ptr->k_idx - b_ptr->k_idx;
	if (dif) return (dif < 0);

	/* Unidentified objects always come last */
	SORT_BOOL(object_known_p);

	/* Hack:  otherwise identical rods sort by
		increasing recharge time --dsb */
	if (a_ptr->tval == TV_ROD)
	{
		dif = a_ptr->timeout - b_ptr->timeout;
		if (dif) return (dif < 0);
	}

	/* Objects sort by decreasing value. */
	dif = object_value(a_ptr, FALSE) - object_value(b_ptr, FALSE);
	if (dif) return (dif > 0);

	/* Objects sort by decreasing stack size. */
	dif = a_ptr->number - b_ptr->number;
	if (dif) return (dif > 0);

	/* Use the previous order where nothing better suggests itself. */
	return old;
}

/*
 * A comp hook for reorder_pack.
 */
static bool PURE ang_sort_comp_pack(vptr u, vptr UNUSED v, int a, int b)
{
	object_ctype *inv = u, *a_ptr = inv+a, *b_ptr = inv+b;
	return ang_sort_comp_pack_aux(a_ptr, b_ptr);
}
	
/*
 * Swap two inventory items.
 */
static void ang_sort_swap_pack(vptr u, vptr UNUSED v, int a, int b)
{
	object_type *inv = u, tmp[1];
	object_copy(tmp, inv+a);
	object_copy(inv+a, inv+b);
	object_copy(inv+b, tmp);
}

/*
 * Reorder items in the pack
 */
void reorder_pack(void)
{
	int i;

	/* Hack - write the previous order into the objects. */
	for (i = 0; i < INVEN_PACK; i++) inventory[i].ix = i;

	/* Sort the pack using the above functions. */
	ang_sort_comp = ang_sort_comp_pack;
	ang_sort_swap = ang_sort_swap_pack;
	ang_sort(inventory, 0, INVEN_PACK);

	/* Hack - erase the order again. */
	for (i = 0; i < INVEN_PACK; i++) inventory[i].ix = 0;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);
}

/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for books
 */
void display_koff(int k_idx)
{
	int y;

	object_type forge;
	object_type *q_ptr;


	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* No info */
	if (!k_idx)
	{
		return;
	}


	/* Get local object */
	q_ptr = &forge;

	/* Prepare the object */
	object_wipe(q_ptr);

	/* Prepare the object */
	object_prep(q_ptr, k_idx);


	/* Mention the object name */
	mc_put_fmt(0, 0, "%v", object_desc_f3, q_ptr, OD_SHOP, 0);


	/* Display spells in books */
    if (display_spells_p(q_ptr))
	{
		display_spells(2, 0, q_ptr);
	}
}

/*
 * Hide an object.
 */
void object_hide(object_type *o_ptr)
{
	/* No longer visible. */
	o_ptr->marked = FALSE;

	/* Shall not reappear. */
	o_ptr->ident |= IDENT_HIDDEN;

	/* Redraw appropriate stuff. */
	update_object(o_ptr, 0);
}
