/* File: object2.c */

/* Purpose: misc code for objects */

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
void excise_object_idx(int o_idx)
{
	object_type *j_ptr;

	s16b this_o_idx, next_o_idx = 0;
	
	s16b prev_o_idx = 0;


	/* Object */
	j_ptr = &o_list[o_idx];

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
 *
 * Handle "stacks" of objects correctly.
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
	int			i, y, x, num, cnt;

	int                 cur_lev, cur_dis, chance;


	/* Compact */
	if (size)
	{
		/* Message */
		msg_print("Compacting objects...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
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
			object_type *o_ptr = &o_list[i];

			object_kind *k_ptr = &k_info[o_ptr->k_idx];
			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			/* Hack -- High level objects start out "immune" */
			if (k_ptr->level > cur_lev) continue;

			/* Get the location */
			y = o_ptr->iy;
			x = o_ptr->ix;

			/* Nearby objects start out "immune" */
			if ((cur_dis > 0) && (distance(py, px, y, x) < cur_dis)) continue;

			/* Saving throw */
			chance = 90;

			/* Hack -- only compact artifacts in emergencies */
			if (artifact_p(o_ptr) && (cnt < 1000)) chance = 100;

			/* Apply the saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the object */
			delete_object_idx(i);

			/* Count it */
			num++;
		}
	}

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
		if (!character_dungeon || p_ptr->preserve)
		{
			/* Hack -- Preserve unknown artifacts */
			if (artifact_p(o_ptr) && !object_known_p(o_ptr))
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
s16b o_pop(void)
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
		return (i);
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
		return (i);
	}


	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msg_print("Too many objects!");

	/* Oops */
	return (0);
}



/*
 * Apply a "object restriction function" to the "object allocation table"
 */
errr get_obj_num_prep(void)
{
	int i;

	/* Get the entry */
	alloc_entry *table = alloc_kind_table;

	/* Scan the allocation table */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* Accept objects which pass the restriction, if any */
		if (!get_obj_num_hook || (*get_obj_num_hook)(table[i].index))
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
	int			i, j, p;

	int			k_idx;

	long		value, total;

	object_kind		*k_ptr;

	alloc_entry		*table = alloc_kind_table;


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
 *
 * This routine also removes any inscriptions generated by "feelings".
 */
void object_known(object_type *o_ptr)
{
	/* Remove "default inscriptions" */
	if (o_ptr->note && (o_ptr->ident & (IDENT_SENSE)))
	{
		/* Access the inscription */
		cptr q = quark_str(o_ptr->note);

		/* Hack -- Remove auto-inscriptions */
		if ((streq(q, "cursed")) ||
		    (streq(q, "broken")) ||
		    (streq(q, "good")) ||
		    (streq(q, "average")) ||
		    (streq(q, "excellent")) ||
		    (streq(q, "worthless")) ||
		    (streq(q, "special")) ||
		    (streq(q, "terrible")))
		{
			/* Forget the inscription */
			o_ptr->note = 0;
		}
	}

	/* Clear the "Felt" info */
	o_ptr->ident &= ~(IDENT_SENSE);

	/* Clear the "Empty" info */
	o_ptr->ident &= ~(IDENT_EMPTY);

	/* Now we know about the item */
	o_ptr->ident |= (IDENT_KNOWN);
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
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(object_type *o_ptr)
{
	/* Aware item -- use template cost */
	if (object_aware_p(o_ptr)) return (o_ptr->b_cost);

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Un-aware Food */
		case TV_FOOD: return (5L);

		/* Un-aware Potions */
		case TV_POTION: return (20L);

		/* Un-aware Scrolls */
		case TV_SCROLL: return (20L);

		/* Un-aware Staffs */
		case TV_STAFF: return (70L);

		/* Un-aware Wands */
		case TV_WAND: return (50L);

		/* Un-aware Rods */
		case TV_ROD: return (90L);

		/* Un-aware Rings */
		case TV_RING: return (45L);

		/* Un-aware Amulets */
		case TV_AMULET: return (45L);
	}
	/* Paranoia -- Oops */
	return (0L);
}


/*
 * Return the "real" price of a "known" item, not including discounts
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
static s32b object_value_real(object_type *o_ptr)
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
			/* Hack -- Negative "pval" is always bad */
			if (o_ptr->pval < 0) return (0L);

			/* No pval */
			if (!o_ptr->pval) break;

			/* Give credit for stat bonuses */
			if (f1 & TR1_STR) value += (o_ptr->pval * 200L);
			if (f1 & TR1_INT) value += (o_ptr->pval * 200L);
			if (f1 & TR1_WIS) value += (o_ptr->pval * 200L);
			if (f1 & TR1_DEX) value += (o_ptr->pval * 200L);
			if (f1 & TR1_CON) value += (o_ptr->pval * 200L);
			if (f1 & TR1_CHR) value += (o_ptr->pval * 200L);
			/* Give credit for stealth and searching */
			if (f1 & TR1_STEALTH) value += (o_ptr->pval * 100L);
			if (f1 & TR1_SEARCH) value += (o_ptr->pval * 100L);

			/* Give credit for infra-vision and tunneling */
			if (f1 & TR1_INFRA) value += (o_ptr->pval * 50L);
			if (f1 & TR1_TUNNEL) value += (o_ptr->pval * 50L);

			/* Give credit for extra attacks */
			if (f1 & TR1_BLOWS) value += (o_ptr->pval * 2000L);

			/* Give credit for speed bonus */
			if (f1 & TR1_SPEED) value += (o_ptr->pval * 30000L);

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

		/* Rings/Amulets */
		case TV_RING:
		case TV_AMULET:
		{
			/* Hack -- negative bonuses are bad */
			if (o_ptr->to_a < 0) return (0L);
			if (o_ptr->to_h < 0) return (0L);
			if (o_ptr->to_d < 0) return (0L);

			/* Give credit for bonuses */
			value += ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 100L);

			/* Done */
			break;
		}
		/* Armor */
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
			/* Hack -- negative armor bonus */
			if (o_ptr->to_a < 0) return (0L);

			/* Give credit for bonuses */
			value += ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 100L);

			/* Done */
			break;
		}

		/* Bows/Weapons */
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		{
			/* Hack -- negative hit/damage bonuses */
			if (o_ptr->to_h + o_ptr->to_d < 0) return (0L);

			/* Factor in the bonuses */
			value += ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 100L);

			/* Hack -- Factor in extra damage dice */
			if ((o_ptr->dd > k_ptr->dd) && (o_ptr->ds == k_ptr->ds))
			{
				value += (o_ptr->dd - k_ptr->dd) * o_ptr->ds * 100L;
			}

			/* Done */
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Hack -- negative hit/damage bonuses */
			if (o_ptr->to_h + o_ptr->to_d < 0) return (0L);

			/* Factor in the bonuses */
			value += ((o_ptr->to_h + o_ptr->to_d) * 5L);
			/* Hack -- Factor in extra damage dice */
			if ((o_ptr->dd > k_ptr->dd) && (o_ptr->ds == k_ptr->ds))
			{
				value += (o_ptr->dd - k_ptr->dd) * o_ptr->ds * 5L;
			}

			/* Done */
			break;
		}
	}


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
s32b object_value(object_type *o_ptr)
{
	s32b value;


	/* Unknown items -- acquire a base value */
	if (object_known_p(o_ptr))
	{
		/* Broken items -- worthless */
		if (broken_p(o_ptr)) return (0L);

		/* Cursed items -- worthless */
		if (cursed_p(o_ptr)) return (0L);

		/* Real value (see above) */
		value = object_value_real(o_ptr);
	}

	/* Known items -- acquire the actual value */
	else
	{
		/* Hack -- Felt broken items */
		if ((o_ptr->ident & IDENT_SENSE) && broken_p(o_ptr)) return (0L);

		/* Hack -- Felt cursed items */
		if ((o_ptr->ident & IDENT_SENSE) && cursed_p(o_ptr)) return (0L);
		/* Base value (see above) */
		value = object_value_base(o_ptr);
	}
	/* Apply discount (if any) */
	if (o_ptr->discount) value -= (value * o_ptr->discount / 100L);

	/* Return the final value */
	return (value);
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
bool object_similar(object_type *o_ptr, object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;


	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return (0);


	/* Analyze the items */
	switch (o_ptr->tval)
	{
		/* Chests */
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
			/* Require knowledge */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr)) return (0);

			/* Fall through */
		}

		/* Staffs and Wands and Rods */
		case TV_ROD:
		{
			/* Require permission */
			if (!stack_allow_wands) return (0);

			/* Require identical charges */
			if (o_ptr->pval != j_ptr->pval) return (0);

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

			/* Must have identical weight */
			if(o_ptr->weight != j_ptr->weight) return (FALSE);
			/* XXX XXX XXX Require identical "sense" status */
			/* if ((o_ptr->ident & ID_SENSE) != */
			/*     (j_ptr->ident & ID_SENSE)) return (0); */

			/* Fall through */
		}
		/* Rings, Amulets, Lites */
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			/* Require full knowledge of both items */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr)) return (0);

			/* Fall through */
		}

		/* Missiles */
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

			/* Require identical "ego-item" names */
			if (o_ptr->name2 != j_ptr->name2) return (FALSE);

			/* Require identical flags */
			if(o_ptr->flags1!=j_ptr->flags1 ||
				o_ptr->flags2!=j_ptr->flags2||
				o_ptr->flags3!=j_ptr->flags3) return(FALSE);

			/* Hack -- Never stack recharging items */
			if (o_ptr->timeout || j_ptr->timeout) return (FALSE);

			/* Require identical "values" */
			if (o_ptr->ac != j_ptr->ac) return (FALSE);
			if (o_ptr->dd != j_ptr->dd) return (FALSE);
			if (o_ptr->ds != j_ptr->ds) return (FALSE);

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

		/* Various */
		default:
		{
			/* Require knowledge */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr)) return (0);

			/* Probably okay */
			break;
		}
	}


	/* Hack -- Require identical "cursed" status */
	if ((o_ptr->ident & (IDENT_CURSED)) != (j_ptr->ident & (IDENT_CURSED))) return (0);

	/* Hack -- Require identical "broken" status */
	if ((o_ptr->ident & (IDENT_BROKEN)) != (j_ptr->ident & (IDENT_BROKEN))) return (0);


	/* Hack -- require semi-matching "inscriptions" */
	if (o_ptr->note && j_ptr->note && (o_ptr->note != j_ptr->note)) return (0);

	/* Hack -- normally require matching "inscriptions" */
	if (!stack_force_notes && (o_ptr->note != j_ptr->note)) return (0);

	/* Hack -- normally require matching "discounts" */
	if (!stack_force_costs && (o_ptr->discount != j_ptr->discount)) return (0);


	/* Maximal "stacking" limit */
	if (total >= MAX_STACK_SIZE) return (0);


	/* They match, so they must be similar */
	return (TRUE);
}

/*
 * Allow one item to "absorb" another, assuming they are similar
 */
void object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Add together the item counts */
	o_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Hack -- blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr);

	/* Hack -- blend "rumour" status */
	if (j_ptr->ident & (IDENT_RUMOUR)) o_ptr->ident |= (IDENT_RUMOUR);

	/* Hack -- blend "mental" status */
	if (j_ptr->ident & (IDENT_MENTAL)) o_ptr->ident |= (IDENT_MENTAL);

	/* Hack -- blend "inscriptions" */
	if (j_ptr->note) o_ptr->note = j_ptr->note;

	/* Hack -- could average discounts XXX XXX XXX */
	/* Hack -- save largest discount XXX XXX XXX */
	if (o_ptr->discount < j_ptr->discount) o_ptr->discount = j_ptr->discount;
}



/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_kind(int tval, int sval)
{
	int k;

	/* Look for it */
	for (k = 1; k < MAX_K_IDX; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Found a match */
		if ((k_ptr->tval == tval) && (k_ptr->sval == sval)) return (k);
	}

	/* Oops */
	msg_format("No object (%d,%d)", tval, sval);

	/* Oops */
	return (0);
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
void object_copy(object_type *o_ptr, object_type *j_ptr)
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

	/* Hack -- worthless items are always "broken" */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);
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
	if (rand_int(MAX_DEPTH) < extra) bonus++;
	/* The "stand" is equal to one quarter of the max */
	stand = (max / 4);
	/* Hack -- determine fraction of error */
	extra = (max % 4);

	/* Hack -- simulate floating point computations */
	if (rand_int(4) < extra) stand++;


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
static void object_mention(object_type *o_ptr)
{
	char o_name[80];

	/* Describe */
	object_desc_store(o_name, o_ptr, FALSE, 0);

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
 * Mega-Hack -- Attempt to create one of the "Special Objects"
 *
 * We are only called from "make_object()", and we assume that
 * "apply_magic()" is called immediately after we return.
 *
 * Note -- see "make_artifact()" and "apply_magic()"
 */
static bool make_artifact_special(object_type *o_ptr)
{
	int			i, dun;

	int			k_idx = 0;


	/* No artifacts in the town */
	if (!dun_level) return (FALSE);

	dun = (dun_level==-1? 75: dun_level);

	/* Check the artifact list (just the "specials") */
	for (i = 0; i < ART_MIN_NORMAL; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > dun)
		{
			/* Acquire the "out-of-depth factor" */
			int d = (a_ptr->level - dun) * 2;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* Artifact "rarity roll" */
		if (rand_int(a_ptr->rarity==1? 1:
			(a_ptr->rarity*100)/(100+luck())) != 0) return (0);
		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* XXX XXX Enforce minimum "object" level (loosely) */
		if (k_info[k_idx].level > object_level)
		{
			/* Acquire the "out-of-depth factor" */
			int d = (k_info[k_idx].level - object_level) * 5;
			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* Assign the template */
		object_prep(o_ptr, k_idx);

		/* Mega-Hack -- mark the item as an artifact */
		o_ptr->name1 = i;

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
	int i, dun;


	/* No artifacts in the town */
	if (!dun_level) return (FALSE);

	dun = (dun_level==-1? 75: dun_level);

	/* Paranoia -- no "plural" artifacts */
	if (o_ptr->number != 1) return (FALSE);

	/* Check the artifact list (skip the "specials") */
	for (i = ART_MIN_NORMAL; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		/* Skip "empty" items */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;
		/* Must have the correct fields */
		if (a_ptr->tval != o_ptr->tval) continue;
		if (a_ptr->sval != o_ptr->sval) continue;
		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > dun)
		{
			/* Acquire the "out-of-depth factor" */
			int d = (a_ptr->level - dun) * 2;
			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* We must make the "rarity roll" */
		if (rand_int(a_ptr->rarity==1? 1:
			(a_ptr->rarity*100)/(100+luck())) != 0) continue;

		/* Hack -- mark the item as an artifact */
		o_ptr->name1 = i;

		/* Success */
		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}


/*
 * Charge a new wand.
 */
static void charge_wand(object_type *o_ptr)
{
	switch (o_ptr->sval)
	{
		case SV_WAND_HEAL_MONSTER:		o_ptr->pval = randint(20) + 8; break;
		case SV_WAND_HASTE_MONSTER:		o_ptr->pval = randint(20) + 8; break;
		case SV_WAND_CLONE_MONSTER:		o_ptr->pval = randint(5)  + 3; break;
		case SV_WAND_TELEPORT_AWAY:		o_ptr->pval = randint(5)  + 6; break;
		case SV_WAND_DISARMING:			o_ptr->pval = randint(5)  + 4; break;
		case SV_WAND_TRAP_DOOR_DEST:	o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_STONE_TO_MUD:		o_ptr->pval = randint(4)  + 3; break;
		case SV_WAND_LITE:				o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_SLEEP_MONSTER:		o_ptr->pval = randint(15) + 8; break;
		case SV_WAND_SLOW_MONSTER:		o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_CONFUSE_MONSTER:	o_ptr->pval = randint(12) + 6; break;
		case SV_WAND_FEAR_MONSTER:		o_ptr->pval = randint(5)  + 3; break;
		case SV_WAND_DRAIN_LIFE:		o_ptr->pval = randint(3)  + 3; break;
		case SV_WAND_POLYMORPH:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_STINKING_CLOUD:	o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_MAGIC_MISSILE:		o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_ACID_BOLT:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_ELEC_BOLT:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_FIRE_BOLT:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_COLD_BOLT:			o_ptr->pval = randint(5)  + 6; break;
		case SV_WAND_ACID_BALL:			o_ptr->pval = randint(5)  + 2; break;
		case SV_WAND_ELEC_BALL:			o_ptr->pval = randint(8)  + 4; break;
		case SV_WAND_FIRE_BALL:			o_ptr->pval = randint(4)  + 2; break;
		case SV_WAND_COLD_BALL:			o_ptr->pval = randint(6)  + 2; break;
		case SV_WAND_WONDER:			o_ptr->pval = randint(15) + 8; break;
		case SV_WAND_ANNIHILATION:		o_ptr->pval = randint(2)  + 1; break;
		case SV_WAND_DRAGON_FIRE:		o_ptr->pval = randint(3)  + 1; break;
		case SV_WAND_DRAGON_COLD:		o_ptr->pval = randint(3)  + 1; break;
		case SV_WAND_DRAGON_BREATH:		o_ptr->pval = randint(3)  + 1; break;
	}
}



/*
 * Charge a new staff.
 */
static void charge_staff(object_type *o_ptr)
{
	switch (o_ptr->sval)
	{
		case SV_STAFF_DARKNESS:			o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_SLOWNESS:			o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_HASTE_MONSTERS:	o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_SUMMONING:		o_ptr->pval = randint(3)  + 1; break;
		case SV_STAFF_TELEPORTATION:	o_ptr->pval = randint(4)  + 5; break;
		case SV_STAFF_IDENTIFY:			o_ptr->pval = randint(15) + 5; break;
		case SV_STAFF_REMOVE_CURSE:		o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_STARLITE:			o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_LITE:				o_ptr->pval = randint(20) + 8; break;
		case SV_STAFF_MAPPING:			o_ptr->pval = randint(5)  + 5; break;
		case SV_STAFF_DETECT_GOLD:		o_ptr->pval = randint(20) + 8; break;
		case SV_STAFF_DETECT_ITEM:		o_ptr->pval = randint(15) + 6; break;
		case SV_STAFF_DETECT_TRAP:		o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_DETECT_DOOR:		o_ptr->pval = randint(8)  + 6; break;
		case SV_STAFF_DETECT_INVIS:		o_ptr->pval = randint(15) + 8; break;
		case SV_STAFF_DETECT_EVIL:		o_ptr->pval = randint(15) + 8; break;
		case SV_STAFF_CURE_LIGHT:		o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_CURING:			o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_HEALING:			o_ptr->pval = randint(2)  + 1; break;
		case SV_STAFF_THE_MAGI:			o_ptr->pval = randint(2)  + 2; break;
		case SV_STAFF_SLEEP_MONSTERS:	o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_SLOW_MONSTERS:	o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_SPEED:			o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_PROBING:			o_ptr->pval = randint(6)  + 2; break;
		case SV_STAFF_DISPEL_EVIL:		o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_POWER:			o_ptr->pval = randint(3)  + 1; break;
		case SV_STAFF_HOLINESS:			o_ptr->pval = randint(2)  + 2; break;
		case SV_STAFF_GENOCIDE:			o_ptr->pval = randint(2)  + 1; break;
		case SV_STAFF_EARTHQUAKES:		o_ptr->pval = randint(5)  + 3; break;
		case SV_STAFF_DESTRUCTION:		o_ptr->pval = randint(3)  + 1; break;
	}
}


/*
 * Select an ego flag for an item
*/
static void ego_flag(object_type *o_ptr, int min, int num)
{
	int et;

	do et = rand_int(num) + min;
		while(randint(e_info[et].rarity) > 1);
	o_ptr->name2 = et;
}

/*
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base damage dice boosting
 * Hack -- note special processing for weapon/digger
 * Hack -- note special rating boost for dragon scale mail
 */
static void a_m_aux_1(object_type *o_ptr, int level, int power)
{
	int tohit1 = randint(5) + m_bonus(5, level);
	int todam1 = randint(5) + m_bonus(5, level);

	int tohit2 = m_bonus(10, level);
	int todam2 = m_bonus(10, level);

	int et;

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
			/* Very good */
			if (power > 1)
			{
				/* Special Ego-item */
				o_ptr->name2 = EGO_DIGGING;
			}

			/* Very bad */
			else if (power < -1)
			{
				/* Hack -- Horrible digging bonus */
				o_ptr->pval = 0 - (5 + randint(5));
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
		{
			/* Very Good */
			if (power > 1)
			{
				/* Roll for an ego-item */
				while(1)
				{
					et = rand_int(27)+64;
					if(randint(e_info[et].rarity) > 1) continue;
					if(et==EGO_BLESS_BLADE &&
						o_ptr->tval != TV_SWORD) continue;
					break;
				}
				o_ptr->name2 = et;
				if (et == EGO_MYSTIC) o_ptr->weight /= 2;
				if (et == EGO_MORGUL) o_ptr->weight += 5;

				/* Hack -- Super-charge the damage dice */
				while (rand_int(o_ptr->dd * o_ptr->ds) == 0) o_ptr->dd++;
				/* Hack -- Lower the damage dice */
				if (o_ptr->dd > 9) o_ptr->dd = 9;
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				if (rand_int(MAX_DEPTH) < level)
				{
					o_ptr->name2 = EGO_MORGUL;
				}
			}

			break;
		}


		case TV_BOW:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_BOW, EGO_BOW_NUM);
			}

			break;
		}


		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_MISSILE, EGO_MISSILE_NUM);

				/* Hack -- super-charge the damage dice */
				while (rand_int(5L * o_ptr->dd * o_ptr->ds) == 0) o_ptr->dd++;

				/* Hack -- restrict the damage dice */
				if (o_ptr->dd > 9) o_ptr->dd = 9;
			}
			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				if (rand_int(MAX_DEPTH) < level)
				{
					o_ptr->name2 = EGO_BACKBITING;
				}
			}

			break;
		}
	}
}


/*
 * Apply magic to an item known to be "armor"
 *
 * Hack -- note special processing for crown/helm
 * Hack -- note special processing for robe of permanence
 */
static void a_m_aux_2(object_type *o_ptr, int level, int power)
{
	int toac1 = randint(5) + m_bonus(5, level);

	int toac2 = m_bonus(10, level);

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

	/* Analyze type */
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		{
			/* Rating boost */
			rating += 30;

			/* Mention the item */
			if (cheat_peek) object_mention(o_ptr);

			break;
		}


		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			/* Very good */
			if (power > 1)
			{
				/* Hack -- Try for "Robes of the Magi" */
				if ((o_ptr->tval == TV_SOFT_ARMOR) &&
				    (o_ptr->sval == SV_ROBE) &&
				    (rand_int(100) < 10))
				{
					o_ptr->name2 = EGO_PERMANENCE;
					break;
				}

				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_ARMOR, EGO_ARMOR_NUM);
			}

			break;
		}


		case TV_SHIELD:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_SHIELD, EGO_SHIELD_NUM);
			}

			break;
		}
		case TV_GLOVES:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_GLOVES, EGO_GLOVES_NUM);
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_GLOVES_C, EGO_GLOVES_C_NUM);
			}

			break;
		}

		case TV_BOOTS:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_BOOTS, EGO_BOOTS_NUM);
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_BOOTS_C, EGO_BOOTS_C_NUM);
			}

			break;
		}

		case TV_CROWN:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_CROWN, EGO_CROWN_NUM);
			}
			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_HELM_C, EGO_HELM_C_NUM);
			}
			break;
		}
		case TV_HELM:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_HELM, EGO_HELM_NUM);
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_HELM_C, EGO_HELM_C_NUM);
			}

			break;
		}


		case TV_CLOAK:
		{
			/* Very good */
			if (power > 1)
			{
				/* Roll for ego-item */
				ego_flag(o_ptr, EGO_CLOAK, EGO_CLOAK_NUM);
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Choose some damage */
				ego_flag(o_ptr, EGO_CLOAK_C, EGO_CLOAK_C_NUM);
			}

			break;
		}
	}
}



/*
 * Apply magic to an item known to be a "ring" or "amulet"
 *
 * Hack -- note special rating boost for ring of speed
 * Hack -- note special rating boost for amulet of the magi
 * Hack -- note special "pval boost" code for ring of speed
 * Hack -- note that some items must be cursed (or blessed)
 */
static void a_m_aux_3(object_type *o_ptr, int level, int power)
{
	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		case TV_RING:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Strength, Constitution, Dexterity, Intelligence */
				case SV_RING_STR:
				case SV_RING_CON:
				case SV_RING_DEX:
				case SV_RING_INT:
				{
					/* Stat bonus */
					o_ptr->pval = 1 + m_bonus(5, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Ring of Speed! */
				case SV_RING_SPEED:
				{
					/* Base speed (1 to 10) */
					o_ptr->pval = randint(5) + m_bonus(5, level);

					/* Super-charge the ring */
					while (rand_int(100) < 50) o_ptr->pval++;

					/* Cursed Ring */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);
						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);
						break;
					}
					/* Rating boost */
					rating += 25;
					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);

					break;
				}

				/* Searching */
				case SV_RING_SEARCHING:
				{
					/* Bonus to searching */
					o_ptr->pval = 1 + m_bonus(5, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Flames, Acid, Ice */
				case SV_RING_FLAMES:
				case SV_RING_ACID:
				case SV_RING_ICE:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 5 + randint(5) + m_bonus(10, level);
					break;
				}

				/* Weakness, Stupidity */
				case SV_RING_WEAKNESS:
				case SV_RING_STUPIDITY:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);
					/* Penalize */
					o_ptr->pval = 0 - (1 + m_bonus(5, level));

					break;
				}
				/* WOE, Stupidity */
				case SV_RING_WOE:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);
					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);
					/* Penalize */
					o_ptr->to_a = 0 - (5 + m_bonus(10, level));
					o_ptr->pval = 0 - (1 + m_bonus(5, level));
					break;
				}

				/* Ring of damage */
				case SV_RING_DAMAGE:
				{
					/* Bonus to damage */
					o_ptr->to_d = 5 + randint(5) + m_bonus(10, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);
						/* Reverse bonus */
						o_ptr->to_d = 0 - (o_ptr->to_d);
					}

					break;
				}

				/* Ring of Accuracy */
				case SV_RING_ACCURACY:
				{
					/* Bonus to hit */
					o_ptr->to_h = 5 + randint(5) + m_bonus(10, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);
						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);
						/* Reverse tohit */
						o_ptr->to_h = 0 - (o_ptr->to_h);
					}
					break;
				}
				/* Ring of Protection */
				case SV_RING_PROTECTION:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 5 + randint(5) + m_bonus(10, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse toac */
						o_ptr->to_a = 0 - (o_ptr->to_a);
					}

					break;
				}

				/* Ring of Slaying */
				case SV_RING_SLAYING:
				{
					/* Bonus to damage and to hit */
					o_ptr->to_d = randint(5) + m_bonus(10, level);
					o_ptr->to_h = randint(5) + m_bonus(10, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

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

		case TV_AMULET:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Amulet of wisdom/charisma */
				case SV_AMULET_WISDOM:
				case SV_AMULET_CHARISMA:
				{
					o_ptr->pval = 1 + m_bonus(5, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

						/* Cursed */
						o_ptr->ident |= (IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Amulet of searching */
				case SV_AMULET_SEARCHING:
				{
					o_ptr->pval = randint(5) + m_bonus(5, level);
					/* Cursed */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

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
					o_ptr->pval = randint(5) + m_bonus(5, level);
					o_ptr->to_a = randint(5) + m_bonus(5, level);

					/* Boost the rating */
					rating += 25;

					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);
					break;
				}
				/* Amulet of Doom -- always cursed */
				case SV_AMULET_DOOM:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);
					/* Penalize */
					o_ptr->pval = 0 - (randint(5) + m_bonus(5, level));
					o_ptr->to_a = 0 - (randint(5) + m_bonus(5, level));

					break;
				}
			}

			break;
		}
	}
}


/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
static void a_m_aux_4(object_type *o_ptr, int level, int power)
{
	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		case TV_LITE:
		{
			/* Hack -- Torches -- random fuel */
			if (o_ptr->sval == SV_LITE_TORCH)
			{
				if (o_ptr->pval) o_ptr->pval = randint(o_ptr->pval);
			}

			/* Hack -- Lanterns -- random fuel */
			if (o_ptr->sval == SV_LITE_LANTERN)
			{
				if (o_ptr->pval) o_ptr->pval = randint(o_ptr->pval);
			}
			break;
		}

		case TV_WAND:
		{
			/* Hack -- charge wands */
			charge_wand(o_ptr);
			break;
		}

		case TV_STAFF:
		{
			/* Hack -- charge staffs */
			charge_staff(o_ptr);
			break;
		}

		case TV_CHEST:
		{
			/* Hack -- skip ruined chests */
			if (k_info[o_ptr->k_idx].level <= 0) break;

			/* Hack -- pick a "difficulty" */
			o_ptr->pval = randint(k_info[o_ptr->k_idx].level);

			/* Never exceed "difficulty" of 55 to 59 */
			if (o_ptr->pval > 55) o_ptr->pval = 55 + rand_int(5);
			break;
		}
	}
}

void apply_flags(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	u32b f1, f2, f3;

	/* Base object */
	f1 = k_ptr->flags1;
	f2 = k_ptr->flags2;
	f3 = k_ptr->flags3;
	o_ptr->b_cost = k_ptr->cost;

	/* Ego-item */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		f1 |= e_ptr->flags1;
		f2 |= e_ptr->flags2;
		f3 |= e_ptr->flags3;
		/* Extra powers */
		switch (e_ptr->xtra1)
		{
			case EGO_XTRA_SUSTAIN:
			{
				/* Choose a sustain */
				add_sust(50, o_ptr);
				break;
			}

			case EGO_XTRA_POWER:
			{
				/* Choose a power */
				select_attrib(50, 2, TR2_RES_BLIND | TR2_RES_CONF |
					TR2_RES_SOUND | TR2_RES_SHARDS | TR2_RES_NETHER|
					TR2_RES_NEXUS | TR2_RES_CHAOS | TR2_RES_DISEN|
					TR2_RES_POIS, o_ptr);
				break;
			}
			case EGO_XTRA_ABILITY:
			{
				/* Choose an ability */
				if(randint(4)>0) select_attrib(50, 3, TR3_FEATHER | TR3_LITE |
					TR3_SEE_INVIS | TR3_TELEPATHY |
					TR3_SLOW_DIGEST | TR3_REGEN, o_ptr);
					else select_attrib(50, 2, TR2_FREE_ACT|
						TR2_HOLD_LIFE, o_ptr);
				break;
			}
		case EGO_XTRA_VORPAL:
		  {
		    /* Is it a blade? */
		    if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM)
		      {
			if (rand_int(10) == 0)
			  f1 |= TR1_VORPAL;
		      }
		  }
		}
		f1 |= o_ptr->flags1;
		f2 |= o_ptr->flags2;
		f3 |= o_ptr->flags3;
	}
	o_ptr->flags1 = f1;
	o_ptr->flags2 = f2;
	o_ptr->flags3 = f3;
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
void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great)
{
	int i, rolls, f1, f2, power, luc;
	luc = luck()/2;	/* Only a slight modification */
	lev = lev + luc/5;	/* Extreme good luck makes better items */

	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

	/* Base chance of being "good" */
	f1 = lev + 10 +luc;

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
	if (!okay || o_ptr->name1) rolls = 0;

	/* Roll for artifacts if allowed */
	for (i = 0; i < rolls; i++)
	{
		/* Roll for an artifact */
		if (make_artifact(o_ptr)) break;
	}


	/* Hack -- analyze artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* Hack -- Mark the artifact as "created" */
		a_ptr->cur_num = 1;

		/* Extract the other fields */
		o_ptr->pval = a_ptr->pval;
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;
		o_ptr->to_a = a_ptr->to_a;
		o_ptr->to_h = a_ptr->to_h;
		o_ptr->to_d = a_ptr->to_d;
		o_ptr->weight = a_ptr->weight;
		o_ptr->flags1 = a_ptr->flags1;
		o_ptr->flags2 = a_ptr->flags2;
		o_ptr->flags3 = a_ptr->flags3;

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
		/* Hack -- extract the "cursed" flag */
		if (a_ptr->flags3 & TR3_CURSED) o_ptr->ident |= (IDENT_CURSED);

		/* Mega-Hack -- increase the rating */
		rating += 10;

		/* Mega-Hack -- increase the rating again */
		if (a_ptr->cost > 50000L) rating += 10;

		/* Set the good item flag */
		good_item_flag = TRUE;

		/* Cheat -- peek at the item */
		if (cheat_peek) object_mention(o_ptr);

		/* Done */
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
			if (power) a_m_aux_1(o_ptr, lev, power);
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
			if (power) a_m_aux_2(o_ptr, lev, power);
			break;
		}
		case TV_RING:
		case TV_AMULET:
		{
			if (!power && (rand_int(100) < 50)) power = -1;
			a_m_aux_3(o_ptr, lev, power);
			break;
		}

		default:
		{
			a_m_aux_4(o_ptr, lev, power);
			break;
		}
	}


	/* Hack -- analyze ego-items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

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

			/* Hack -- negate to_h and to_d for "mystic" weapons */
			if (o_ptr->name2 == EGO_MYSTIC)
			{
				o_ptr->to_h = -o_ptr->to_h;
				o_ptr->to_d = -o_ptr->to_d;
			}

			/* Hack -- obtain pval */
			if (e_ptr->max_pval) o_ptr->pval += randint(e_ptr->max_pval);
		}

		/* Hack -- apply rating bonus */
		rating += e_ptr->rating;

		/* Cheat -- describe the item */
		if (cheat_peek) object_mention(o_ptr);

		/* Apply flags */
		apply_flags(o_ptr);

		/* Done */
		return;
	}

	/* Examine real objects */
	if (o_ptr->k_idx)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];
		/* Hack -- acquire "broken" flag */
		if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Apply flags */
		apply_flags(o_ptr);

		/* Hack -- acquire "cursed" flag */
		if (o_ptr->flags3 & TR3_CURSED) o_ptr->ident |= (IDENT_CURSED);
	}
}
/*
 * Hack -- determine if a template is "good"
 */
static bool kind_is_good(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

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

		/* Weapons -- Good unless damaged */
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			if (k_ptr->to_h < 0) return (FALSE);
			if (k_ptr->to_d < 0) return (FALSE);
			return (TRUE);
		}
		/* Ammo -- Arrows/Bolts are good */
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

		/* Rings -- Rings of Speed are good */
		case TV_RING:
		{
			if (k_ptr->sval == SV_RING_SPEED) return (TRUE);
			return (FALSE);
		}
		/* Amulets -- Amulets of the Magi are good */
		case TV_AMULET:
		{
			if (k_ptr->sval == SV_AMULET_THE_MAGI) return (TRUE);
			return (FALSE);
		}
	}

	/* Assume not good */
	return (FALSE);
}

/*
 * Determine whether an object fits the specified tval
 */
bool kind_fits_tval(int k_idx)
{
	int t;
	t = item_tester_tval;
	if(k_info[k_idx].tval == t) return (TRUE);
	return (FALSE);
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

	int dun = (dun_level==-1? 70: dun_level);
	int luc = luck()/2;

	/* Chance of "special object" */
	prob = 10-luc;
	if(!good) prob*=100;

	/* Base level for the object */
	base = (good ? (object_level + 10) : object_level);


	/* Generate a special object, or a normal object */
	if ((rand_int(prob) != 0) || !make_artifact_special(j_ptr))
	{
		int k_idx;

		/* Good objects */
		if (good)
		{
			/* Activate restriction */
			get_obj_num_hook = kind_is_good;

			/* Prepare allocation table */
			get_obj_num_prep();
		}

		/* Pick a random object */
		k_idx = get_obj_num(base);

		/* Good objects */
		if (good)
		{
			/* Clear restriction */
			get_obj_num_hook = NULL;

			/* Prepare allocation table */
			get_obj_num_prep();
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
			j_ptr->number = damroll(6, 7);
		}
	}

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) &&
	    (k_info[j_ptr->k_idx].level > dun))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - dun);

		/* Cheat -- peek at items */
		if (cheat_peek) object_mention(j_ptr);
	}
	
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
	s16b o_idx;

	cave_type *c_ptr;

	object_type forge;
	object_type *q_ptr;


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
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[o_idx];

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
		c_ptr->o_idx = o_idx;

		/* Notice */
		note_spot(y, x);
		
		/* Redraw */
		lite_spot(y, x);
	}
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
 * Places a random trap at the given location.
 *
 * The location must be a valid, empty, clean, floor grid.
 *
 * Note that all traps start out as "invisible" and "untyped", and then
 * when they are "discovered" (by detecting them or setting them off),
 * the trap is "instantiated" as a visible, "typed", trap.
 */
void place_trap(int y, int x)
{
	cave_type *c_ptr;

	/* Paranoia -- verify location */
	if (!in_bounds(y, x)) return;
	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;
	/* Access the grid */
	c_ptr = &cave[y][x];
	/* Place an invisible trap */
	c_ptr->feat = FEAT_INVIS;
}

/*
 * XXX XXX XXX Do not use these hard-coded values.
 */
#define OBJ_GOLD_LIST	480	/* First "gold" entry */
#define MAX_GOLD	18	/* Number of "gold" entries */

/*
 * Make a treasure object
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_gold(object_type *j_ptr)
{
	int i;

	s32b	base;
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
	s16b o_idx;

	cave_type *c_ptr;

	object_type forge;
	object_type *q_ptr;


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
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;
		
		/* Acquire object */
		o_ptr = &o_list[o_idx];

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
		c_ptr->o_idx = o_idx;

		/* Notice */
		note_spot(y, x);
		
		/* Redraw */
		lite_spot(y, x);
	}
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
s16b drop_near(object_type *j_ptr, int chance, int y, int x)
{
	int i, k, d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;

	s16b o_idx;

	s16b this_o_idx, next_o_idx = 0;

	cave_type *c_ptr;

	char o_name[80];

	bool flag = FALSE;
	bool done = FALSE;

	bool plural = FALSE;


	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Describe object */
	object_desc(o_name, j_ptr, FALSE, 0);


	/* Handle normal "breakage" */
	if (!artifact_p(j_ptr) && (rand_int(100) < chance))
	{
		/* Message */
		msg_format("The %s disappear%s.",
		           o_name, (plural ? "" : "s"));

		/* Debug */
		if (wizard) msg_print("(breakage)");

		/* Failure */
		return (0);
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
	if (!flag && !artifact_p(j_ptr))
	{
		/* Message */
		msg_format("The %s disappear%s.",
		           o_name, (plural ? "" : "s"));

		/* Debug */
		if (wizard) msg_print("(no floor space)");

		/* Failure */
		return (0);
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
			object_absorb(o_ptr, j_ptr);

			/* Success */
			done = TRUE;

			/* Done */
			break;
		}
	}

	/* Get new object */
	o_idx = o_pop();

	/* Failure */
	if (!done && !o_idx)
	{
		/* Message */
		msg_format("The %s disappear%s.",
		           o_name, (plural ? "" : "s"));

		/* Debug */
		if (wizard) msg_print("(too many objects)");

		/* Failure */
		return (0);
	}

	/* Stack */
	if (!done)
	{
		/* Structure copy */
		object_copy(&o_list[o_idx], j_ptr);

		/* Access new object */
		j_ptr = &o_list[o_idx];

		/* Locate */
		j_ptr->iy = by;
		j_ptr->ix = bx;

		/* No monster */
		j_ptr->held_m_idx = 0;

		/* Build a stack */
		j_ptr->next_o_idx = c_ptr->o_idx;

		/* Place the object */
		c_ptr->o_idx = o_idx;

		/* Success */
		done = TRUE;
	}

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
	
	/* XXX XXX XXX */

	/* Result */
	return (o_idx);
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
		if ((feat == FEAT_TRAP_HEAD + 0x00) && (dun_level >= MAX_DEPTH-1)) continue;

		/* Done */
		break;
	}
	/* Activate the trap */
	c_ptr->feat = feat;

	/* Notice */
	note_spot(y, x);

	/* Redraw */
	lite_spot(y, x);
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
	/* Multiple charges */
	if (o_ptr->pval != 1)
	{
		/* Print a message */
		msg_format("You have %d charges remaining.", o_ptr->pval);
	}

	/* Single charge */
	else
	{
		/* Print a message */
		msg_format("You have %d charge remaining.", o_ptr->pval);
	}
}


/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
	object_type	*o_ptr = &inventory[item];

	char	o_name[80];

	/* Get a description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Print a message */
	msg_format("You have %s.", o_name);
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
		total_weight += (num * o_ptr->weight);
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
		inven_cnt--;
		/* Slide everything down */
		for (i = item; i < INVEN_PACK; i++)
		{
			/* Structure copy */
			inventory[i] = inventory[i+1];
		}

		/* Erase the "final" slot */
		object_wipe(&inventory[i]);
		
		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* The item is being wielded */
	else
	{
		/* One less item */
		equip_cnt--;
		/* Erase the empty slot */
		object_wipe(&inventory[item]);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate torch */
		p_ptr->update |= (PU_TORCH);
		/* Recalculate mana XXX */
		p_ptr->update |= (PU_MANA);
		
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Window stuff */
	p_ptr->window |= (PW_SPELL);
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

	/* Multiple charges */
	if (o_ptr->pval != 1)
	{
		/* Print a message */
		msg_format("There are %d charges remaining.", o_ptr->pval);
	}

	/* Single charge */
	else
	{
		/* Print a message */
		msg_format("There is %d charge remaining.", o_ptr->pval);
	}
}
/*
 * Describe an item in the inventory.
 */
void floor_item_describe(int item)
{
	object_type	*o_ptr = &o_list[item];

	char	o_name[80];

	/* Get a description */
	object_desc(o_name, o_ptr, TRUE, 3);
	/* Print a message */
	msg_format("You see %s.", o_name);
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
}
/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(object_type *o_ptr)
{
	int j;

	/* Empty slot? */
	if (inven_cnt < INVEN_PACK) return (TRUE);

	/* Similar slot? */
	for (j = 0; j < INVEN_PACK; j++)
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
 * Add an item to the players inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", otherwise,
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
 *
 * The "final" flag tells this function to bypass the "combine"
 * and "reorder" code until later.
 */
s16b inven_carry(object_type *o_ptr, bool final)
{
	int i, j, k;
	int n = -1;

	object_type	*j_ptr;


	/* Not final */
	if (!final)
	{
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
				total_weight += (o_ptr->number * o_ptr->weight);

				/* Recalculate bonuses */
				p_ptr->update |= (PU_BONUS);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN | PW_SPELL);

				/* Success */
				return (j);
			}
		}
	}


	/* Paranoia */
	if (inven_cnt > INVEN_PACK) return (-1);


	/* Find an empty slot */
	for (j = 0; j <= INVEN_PACK; j++)
	{
		j_ptr = &inventory[j];

		/* Use it if found */
		if (!j_ptr->k_idx) break;
	}

	/* Use that slot */
	i = j;


	/* Hack -- pre-reorder the pack */
	if (!final && (i < INVEN_PACK))
	{
		s32b		o_value, j_value;

		/* Get the "value" of the item */
		o_value = object_value(o_ptr);

		/* Scan every occupied slot */
		for (j = 0; j < INVEN_PACK; j++)
		{
			j_ptr = &inventory[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;

			/* Hack -- readable books always come first */
			if ((o_ptr->tval == mp_ptr->spell_book) &&
			    (j_ptr->tval != mp_ptr->spell_book)) break;
			if ((j_ptr->tval == mp_ptr->spell_book) &&
			    (o_ptr->tval != mp_ptr->spell_book)) continue;

			/* Objects sort by decreasing type */
			if (o_ptr->tval > j_ptr->tval) break;
			if (o_ptr->tval < j_ptr->tval) continue;

			/* Non-aware (flavored) items always come last */
			if (!object_aware_p(o_ptr)) continue;
			if (!object_aware_p(j_ptr)) break;

			/* Objects sort by increasing sval */
			if (o_ptr->sval < j_ptr->sval) break;
			if (o_ptr->sval > j_ptr->sval) continue;
			/* Unidentified objects always come last */
			if (!object_known_p(o_ptr)) continue;
			if (!object_known_p(j_ptr)) break;

			/* Determine the "value" of the pack item */
			j_value = object_value(j_ptr);

			/* Objects sort by decreasing value */
			if (o_value > j_value) break;
			if (o_value < j_value) continue;
		}

		/* Use that slot */
		i = j;

		/* Slide objects */
		for (k = n; k >= i; k--)
		{
			/* Hack -- Slide the item */
			object_copy(&inventory[k+1], &inventory[k]);
		}

		/* Wipe the empty slot */
		object_wipe(&inventory[i]);
	}


	/* Acquire a copy of the item */
	object_copy(&inventory[i], o_ptr);

	/* Access new object */
	o_ptr = &inventory[i];

	/* Clean out unused fields */
	o_ptr->iy = o_ptr->ix = 0;
	o_ptr->next_o_idx = 0;
	o_ptr->held_m_idx = 0;
	
	/* Increase the weight */
	total_weight += (o_ptr->number * o_ptr->weight);

	/* Count the items */
	inven_cnt++;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine and Reorder pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_SPELL);

	/* Return the slot */
	return (i);
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
s16b inven_takeoff(int item, int amt)
{
	int slot;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	cptr act;

	char o_name[80];

	/* Get the item to take off */
	o_ptr = &inventory[item];
	/* Paranoia */
	if (amt <= 0) return (-1);

	/* Verify */
	if (amt > o_ptr->number) amt = o_ptr->number;

	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);
	/* Modify quantity */
	q_ptr->number = amt;

	/* Describe the object */
	object_desc(o_name, q_ptr, TRUE, 3);

	/* Took off weapon */
	if (item == INVEN_WIELD || (item == INVEN_ARM && p_ptr->twoweap))
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

	/* Modify, Optimize */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

	/* Carry the object */
	slot = inven_carry(q_ptr, FALSE);

	/* Message */
	msg_format("%s %s (%c).", act, o_name, index_to_label(slot));
	
	/* Return slot */
	return (slot);
}


/*
 * Drop (some of) a non-cursed inventory/equipment item
 *
 * The object will be dropped "near" the current location
 */
void inven_drop(int item, int amt)
{
	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	char o_name[80];


	/* Access original object */
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

		/* Access original object */
		o_ptr = &inventory[item];
	}


	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = amt;

	/* Describe local object */
	object_desc(o_name, q_ptr, TRUE, 3);

	/* Message */
	msg_format("You drop %s (%c).", o_name, index_to_label(item));

	/* Drop it near the player */
	drop_near(q_ptr, 0, py, px);
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
	int		i, j, k;
	object_type	*o_ptr;
	object_type	*j_ptr;

	bool	flag = FALSE;

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
				inven_cnt--;

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

/*
 * Reorder items in the pack
 *
 * Note special handling of the "overflow" slot
 *
 * Note special handling of empty slots  XXX XXX XXX XXX
 */
void reorder_pack(void)
{
	int		i, j, k;

	s32b	o_value;
	s32b	j_value;
	object_type *o_ptr;
	object_type *j_ptr;
	object_type	temp;
	bool	flag = FALSE;

	/* Re-order the pack (forwards) */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Mega-Hack -- allow "proper" over-flow */
		if ((i == INVEN_PACK) && (inven_cnt == INVEN_PACK)) break;
		/* Get the item */
		o_ptr = &inventory[i];
		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;
		/* Get the "value" of the item */
		o_value = object_value(o_ptr);
		/* Scan every occupied slot */
		for (j = 0; j < INVEN_PACK; j++)
		{
			/* Get the item already there */
			j_ptr = &inventory[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;
			/* Hack -- readable books always come first */
			if ((o_ptr->tval == mp_ptr->spell_book) &&
			    (j_ptr->tval != mp_ptr->spell_book)) break;
			if ((j_ptr->tval == mp_ptr->spell_book) &&
			    (o_ptr->tval != mp_ptr->spell_book)) continue;
			/* Objects sort by decreasing type */
			if (o_ptr->tval > j_ptr->tval) break;
			if (o_ptr->tval < j_ptr->tval) continue;
			/* Non-aware (flavored) items always come last */
			if (!object_aware_p(o_ptr)) continue;
			if (!object_aware_p(j_ptr)) break;

			/* Objects sort by increasing sval */
			if (o_ptr->sval < j_ptr->sval) break;
			if (o_ptr->sval > j_ptr->sval) continue;

			/* Unidentified objects always come last */
			if (!object_known_p(o_ptr)) continue;
			if (!object_known_p(j_ptr)) break;

			/* Determine the "value" of the pack item */
			j_value = object_value(j_ptr);

			/* Objects sort by decreasing value */
			if (o_value > j_value) break;
			if (o_value < j_value) continue;
		}

		/* Never move down */
		if (j >= i) continue;
		/* Take note */
		flag = TRUE;

		/* Save the moving item */
		temp = inventory[i];
		/* Structure slide (make room) */
		for (k = i; k > j; k--)
		{
			/* Slide the item */
			inventory[k] = inventory[k-1];
		}
		/* Insert the moved item */
		inventory[j] = temp;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
	/* Message */
	if (flag) msg_print("You reorder some items in your pack.");

	/* Hack -- If the player has two weapons, and loses the primary,
	   then we swap the secondary into the primary slot. */
	if (!(inventory[INVEN_WIELD].k_idx) && 
	    ((inventory[INVEN_ARM].tval) == TV_SWORD ||
	    ((inventory[INVEN_ARM].tval) == TV_HAFTED)))
	  {
	    temp = inventory[INVEN_ARM];
	    inventory[INVEN_ARM] = inventory[INVEN_WIELD];
	    inventory[INVEN_WIELD] = temp;
	  }
}


/*
 * Place a specific object at a location
 */
void place_general(int y, int x, byte tval, byte sval)
{
	object_type obj;
	/* Create the object */
	object_prep(&obj, lookup_kind(tval, sval));

	/* Magic */
	apply_magic(&obj, (dun_level==-1? 70: dun_level), TRUE, TRUE, TRUE);

	/* Drop it */
	drop_near(&obj, 0, y, x);
}

/*
 * Place a specific object at a random location on the level
 */
void provide_object(byte tval, byte sval)
{
	int x, y;

	/* Find a place for it */
	do {
		x = randint(cur_wid - 2);
		y = randint(cur_hgt - 2);
		} while(!cave_naked_bold(y, x));
	/* Create it */
	place_general(y, x, tval, sval);
}
/* Tries to add ONE unique attribute to the passed inventory item */
void select_attrib(int lvl, int which, u32b flags, object_type *o_ptr)
{
	u32b find;
	u32b mask;
	int count;
	int i,j,k,p,l2;
	count=2+(lvl/10);
	switch(which)
	{
		case 1: find = o_ptr->flags1; break;
		case 2: find = o_ptr->flags2; break;
		default: find = o_ptr->flags3;
	}
	/* First, go through the passed flags.  Then pick one.  If the attribute
	   passes, then we can stop.  Otherwise, go on.  If end of loop reached
	   without a flag selected, repeat */
	k=0;
	for(i=1;i<count && !k;i++)  
	{
		mask=1;
		/* Scan the specific bits */
		for(j=1;j<33 && !k;j++)
		{
			/* Add a UNIQUE attribute---it's not on the item already */
			if (!(find & mask) && (flags & mask))
			{
				l2=levels[(j-1)+((which-1)*32)];
				p=MAXP;
				if (lvl<l2)
					p-=(LOW*(l2-lvl));
				else if (lvl>l2)
					p-=(HIGH*(lvl-l2));
				if (p<2 && lvl<l2)
					p=2; /* Hard to make things ya don't know how---but doable */
				if (p<5 && lvl>l2)
					p=5; /* More likely to make weaker ones */
				if (randint(100)<=p)
					k=j;
			}
			if (!k)
				mask=mask << 1;
		}
	}
	if (k) /* Don't add 'null' attribute */
	{
		switch(which)
		{
			case 1: o_ptr->flags1 |= 1<<(k-1); break;
			case 2: o_ptr->flags2 |= 1<<(k-1); break;
			default: o_ptr->flags3 |= 1<<(k-1);
		}
		mask=1<<k;
		/* Here, fix Pval for the flag */
		if (o_ptr->tval==TV_BOW)
			return; /* Do NOT touch Pval */
		if (which==1)
		{
			if (mask==TR1_STR || mask==TR1_INT || mask==TR1_WIS ||
				mask==TR1_DEX || mask==TR1_CON || mask==TR1_TUNNEL)
				o_ptr->pval=1+randint(lvl/15);
			else if (mask==TR1_SPEED)
				o_ptr->pval=1+randint(lvl/50);
			else if (mask==TR1_INFRA || mask==TR1_STEALTH)
				o_ptr->pval=2+randint(lvl/20);
		}
	}
}
/* Add a sustain */
void add_sust(int lev, object_type *o_ptr)
{
	select_attrib(lev, 2, TR2_SUST_STR| TR2_SUST_INT| TR2_SUST_WIS|
		TR2_SUST_DEX| TR2_SUST_CON| TR2_SUST_CHR, o_ptr);
}

/*
 * Hack -- Display all known spells in a window
 *
 * XXX XXX XXX Need to analyze size of the window.
 *
 * XXX XXX XXX Need more color coding.
 */
void display_spell_list(void)
{
	int			i, j;
	int			y, x;

	int			m[9];

	magic_type	*s_ptr;
	char		name[80];
	char		out_val[160];


	/* Erase window */
	clear_from(0);

	/* Warriors are illiterate */
	if (!mp_ptr->spell_book) return;

	/* Scan books */
	for (j = 0; j < 9; j++)
	{
		int n = 0;
		/* Reset vertical */
		m[j] = 0;

		/* Vertical location */
		y = (j < 3) ? 0 : (m[j - 3] + 2);

		/* Horizontal location */
		x = 27 * (j % 3);

		/* Scan spells */
		for (i = 0; i < 64; i++)
		{
			/* Check for spell */
			if ((i < 32) ?
			    (spell_flags[p_ptr->realm - 1][j][0] & (1L << i)) :
			    (spell_flags[p_ptr->realm - 1][j][1] & (1L << (i - 32))))
			{
				byte a = TERM_WHITE;
				/* Access the spell */
				s_ptr = &mp_ptr->info[i];

				/* Default name */
				strcpy(name, spell_names[p_ptr->realm - 1][i]);

				/* Illegible */
				if (s_ptr->slevel >= 99)
				{
					/* Illegible */
					strcpy(name, "(illegible)");

					/* Unusable */
					a = TERM_L_DARK;
				}

				/* Forgotten */
				else if ((i < 32) ?
				         ((spell_forgotten1 & (1L << i))) :
				         ((spell_forgotten2 & (1L << (i - 32)))))
				{
					/* Forgotten */
					a = TERM_ORANGE;
				}

				/* Unknown */
				else if (!((i < 32) ?
				           (spell_learned1 & (1L << i)) :
				           (spell_learned2 & (1L << (i - 32)))))
				{
					/* Unknown */
					a = TERM_RED;
				}
				/* Untried */
				else if (!((i < 32) ?
				           (spell_worked1 & (1L << i)) :
				           (spell_worked2 & (1L << (i - 32)))))
				{
					/* Untried */
					a = TERM_YELLOW;
				}

				/* Dump the spell --(-- */
				sprintf(out_val, "%c/%c) %-20.20s",
		        		I2A(j), I2A(n), name);
				/* Track maximum */
				m[j] = y + n;

				/* Dump onto the window */
				Term_putstr(x, m[j], -1, a, out_val);

				/* Next */
				n++;
			}
		}
	}
}



/*
 * Returns spell chance of failure for spell		-RAK-
 */
s16b spell_chance(int spell)
{
	int		chance, minfail, plev;
	magic_type	*s_ptr;


	/* Paranoia -- must be literate */
	if (!p_ptr->realm) return (100);

	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Get effective level */
	plev = smod(S_MAGIC) + p_ptr->cur_skill[mp_ptr->spell_skill]/20 -5;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (plev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

	/* Not enough mana to cast */
	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}
	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];
	/* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
	if ((p_ptr->realm == PRIEST) && (p_ptr->icky_wield)) chance += 25;
	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;
	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}




/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known)
{
	magic_type *s_ptr;

	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];
	/* Spell is illegal */
	if (s_ptr->slevel > smod(S_MAGIC)) return (FALSE);
	/* Spell is forgotten */
	if ((spell < 32) ?
	    (spell_forgotten1 & (1L << spell)) :
	    (spell_forgotten2 & (1L << (spell - 32))))
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if ((spell < 32) ?
	    (spell_learned1 & (1L << spell)) :
	    (spell_learned2 & (1L << (spell - 32))))
	{
		/* Okay to cast, not to study */
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}



/*
 * Extra information on a spell		-DRS-
 *
 * We can use up to 14 characters of the buffer 'p'
 */
static void spell_info(char *p, int j)
{
	int plev;
	/* Default */
	strcpy(p, "");
	plev= smod(S_MAGIC) + (p_ptr->cur_skill[mp_ptr->spell_skill]/20)-5;

#ifdef DRS_SHOW_SPELL_INFO

	/* Analyze the spell */

	switch (j+((p_ptr->realm-1)*64))
	{
		/*** Mage spells ***/
		case 0: sprintf(p, " dam %dd4", 3 + (plev-1)/5 ); break;
		case 2: strcpy(p, " range 10"); break;
		case 8: sprintf(p, " dam %d", 10 + (plev / 2)); break;
		case 10: sprintf(p, " dam %dd8", (3+((plev-5)/4))); break;
		case 11: sprintf(p, " dam %d", 10+plev/4); break;
		case 14: sprintf(p, " range %d", plev * 5); break;
		case 15: sprintf(p, " dam %dd6", 4+((plev-5)/45)); break;
		case 16: sprintf(p, " dam %dd8", (5+((plev-5)/4))); break;
		case 23: sprintf(p, " dam %d", 30+plev/4); break;
		case 24: sprintf(p, " dam %dd8", (8+((plev-5)/4))); break;
		case 26: sprintf(p, " dam %d", 30 + plev); break;
		case 29: sprintf(p, " dur %d+d20", plev); break;
		case 30: sprintf(p, " dam %d", 55 + plev); break;
		case 32: strcpy(p, " restore d35"); break;
		case 33: case 34: case 35: case 36: case 37:
			strcpy(p," dur 20+d20"); break;
		case 46: strcpy(p, " dur 25+d25"); break;
		case 47: strcpy(p, " dur 30+d20"); break;
		case 48: strcpy(p, " dur 25+d25"); break;
		case 49: sprintf(p, " dur %d+d30", plev+30); break;
		case 50: strcpy(p, " dur 8+d8"); break;
		case 51: sprintf(p, " dam %d", (plev*3)/2+150); break;
		case 52: sprintf(p, " dam %d", (plev*3)/2+220); break;
		case 53: sprintf(p, " dam %d", (plev*3)/2+240); break;
		case 54: sprintf(p, " dam %d", (plev*3)/2+260); break;
		case 55: sprintf(p, " dam %d", (plev*3)/2+180); break;
		case 56: sprintf(p, " dam %d", (plev*3)/2+180); break;
		case 57: sprintf(p, " dam %d", (plev*3)/2+190); break;
		case 58: strcpy(p, " dam 300"); break;

		/*** Priest spells ***/
		case 65: sprintf(p," heal %dd%d", 3+plev/15, 3+plev/8); break;
		case 66: strcpy(p, " dur 12+d12"); break;
		case 70: strcpy(p, " dam 3d5"); break;
		case 73: sprintf(p, " range %d", 3*plev); break;
		case 74: sprintf(p, " heal %dd%d", 4+plev/5, 5+plev/8); break;
		case 75: strcpy(p, " dur 24+d24"); break;
		case 79: strcpy(p, " dur 10+d10"); break;
		case 81: sprintf(p, " dam %d+4d4", (plev*3)/2); break;
		case 82: sprintf(p, " heal %dd%d", 5+plev/5, 5+plev/6); break;
		case 83: sprintf(p, " dur %d+d24", plev+15); break;
		case 84: sprintf(p, " dur %d+d25", 3*plev); break;
		case 87: sprintf(p, " heal %dd%d", 10+plev/9, 4+plev/8); break;
		case 90: sprintf(p, " dam d%d", plev*3); break;
		case 91: sprintf(p, " heal %d", 200+plev/3); break;
		case 92: sprintf(p, " dam d%d", plev*3); break;
		case 94: strcpy(p, " heal 2000"); break;
		case 95: strcpy(p, " range 10"); break;
		case 96: sprintf(p, " range %d", 8*plev); break;
	case 99: sprintf(p, " dur %d", 300+plev*2); break;
		case 106: sprintf(p, " dur %d+d%d", 50+plev, plev+10); break;
		case 107: sprintf(p, " dur %d+d%d", 50+plev/3, plev); break;
		case 110: sprintf(p, " dur %d+d%d", plev/5+2, plev/10); break;
	case 113: sprintf(p, " dur %d", plev+20); break;
		case 117: sprintf(p, " dam %d", plev+40); break;
		case 118: sprintf(p, " dam %d", plev*4); break;
		case 119: sprintf(p, " dam %d", (plev*3)/2+150); break;
		case 121: strcpy(p, " dam 300"); break;

		/*** Druid spells ***/
		case 130: sprintf(p, " dur %d", plev*2+10); break;
		case 134: strcpy(p, " heal 3d4"); break;
		case 138: sprintf(p, " heal 5d6+%d", (plev*2)/3); break;
		case 140: sprintf(p, " range %d", plev*2+10); break;
		case 141: sprintf(p, " dur %d+d%d", plev+20, plev); break;
		case 142: sprintf(p, " heal %d+6d7", plev); break;
		case 144: sprintf(p, " dam %d", 20+plev/4); break;
		case 145: sprintf(p, " dam %d", 25+plev/4); break;
		case 147: sprintf(p, " dam %d", 30+plev/4); break;
		case 148: sprintf(p, " hl %d+10d10", (plev*3)/2); break;
		case 149: sprintf(p, " dam %d", 35+plev/4); break;
		case 150: sprintf(p, " dam %d", 40+plev/4); break;
		case 151: sprintf(p, " dam %d", 45+plev/2); break;
		case 152: sprintf(p, " dam %d", 50+plev/2); break;
		case 153: sprintf(p, " dam %d", 55+plev/2); break;
		case 154: sprintf(p, " dam %d", 60+plev/2); break;
		case 157: strcpy(p, " heal 1000"); break;
		case 158: sprintf(p, " dur %d+d%d", plev, plev+50); break;
		case 159: strcpy(p, " restore d50"); break;
		case 170: sprintf(p, " dur %d", plev+60); break;
		case 171: sprintf(p, " dur %d", plev+60); break;
		case 172: sprintf(p, " dur %d", 50+plev); break;
		case 173: sprintf(p, " dur %d+d%d", plev+10, plev); break;
		case 175: sprintf(p, " dam %d", (plev*3)/2+200); break;
		case 176: sprintf(p, " dam %d", (plev*3)/2+210); break;
		case 177: sprintf(p, " dam %d", (plev*3)/2+220); break;
		case 178: sprintf(p, " dam %d", (plev*3)/2+230); break;
		case 179: sprintf(p, " dam %d", (plev*3)/2+240); break;

		/*** Necro spells ***/
		case 193: sprintf(p, " range 20"); break;
		case 200: sprintf(p, " dam %d", plev*4); break;
		case 202: sprintf(p, " dur %d", 30+plev); break;
		case 204: sprintf(p, " dur %d", plev*2+30); break;
		case 205: sprintf(p, " range %d", plev*4); break;
		case 208: sprintf(p," dur %d", 2*plev+50); break;
		case 209: sprintf(p, " dam %d", plev/3+20); break;
		case 210: sprintf(p, " heal %d", (plev*3)/2+50); break;
		case 211: sprintf(p," dur %d", 2*plev+50); break;
		case 213: sprintf(p, " dam %d", plev/3+35); break;
		case 217: sprintf(p, " dur %d", plev+30); break;
		case 218: sprintf(p, " dur %d", plev+50); break;
		case 219: sprintf(p, " dur 30+d20"); break;
		case 220: sprintf(p, " dam %d", (plev*3)/2+80); break;
		case 224: sprintf(p, " dam %d", plev*5); break;
		case 225: sprintf(p, " dam %d", (plev*3)/2+220); break;
		case 226: sprintf(p, " dam %d", (plev+150)); break;
	case 231: sprintf(p, " restore d35"); break;
		case 232: sprintf(p, " dam %d", plev*5); break;
		case 233: sprintf(p, " dam %d", (plev*3)/2+120); break;
		case 236: sprintf(p, " heal %d", plev*2+200); break;
		case 237: sprintf(p, " dur %d", plev+50); break;
		case 238: sprintf(p, " dur %d", plev*2+50); break;
		case 239: sprintf(p, " dur 10+d%d", plev/5); break;
	}
#endif
}


/*
 * Print a list of spells (for browsing or casting or viewing)
 */
void print_spells(byte *spells, int num, int y, int x)
{
	int			i, spell;

	magic_type		*s_ptr;

	cptr		comment;

	char		info[80];

	char		out_val[160];

	/* Title the list */
	prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Lv Mana Fail Info", y, x + 35);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		/* Access the spell */
		spell = spells[i];

		/* Access the spell */
		s_ptr = &mp_ptr->info[spell];
		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), "(illegible)");
			prt(out_val, y + i + 1, x);
			continue;
		}

		/* Get extra info */
		spell_info(info, spell);

		/* Use that info */
		comment = info;

		/* Analyze the spell */
		if ((spell < 32) ?
		    ((spell_forgotten1 & (1L << spell))) :
		    ((spell_forgotten2 & (1L << (spell - 32)))))
		{
			comment = " forgotten";
		}
		else if (!((spell < 32) ?
		           (spell_learned1 & (1L << spell)) :
		           (spell_learned2 & (1L << (spell - 32)))))
		{
		  if (s_ptr->slevel > smod(S_MAGIC))
		    comment = " too hard";
		  else
		    comment = " unknown";
		}
		else if (!((spell < 32) ?
		           (spell_worked1 & (1L << spell)) :
		           (spell_worked2 & (1L << (spell - 32)))))
		{
			comment = " untried";
		}

		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s%2d %4d %3d%%%s",
		        I2A(i), spell_names[p_ptr->realm - 1][spell],
		        s_ptr->slevel, s_ptr->smana, spell_chance(spell), comment);
		prt(out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}



/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readible books
 */
void display_koff(int k_idx)
{
	int y;

	object_type forge;
	object_type *q_ptr;

	char o_name[80];

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* No info */
	if (!k_idx) return;
	/* Get local object */
	q_ptr = &forge;

	/* Prepare the object */
	object_wipe(q_ptr);

	/* Prepare the object */
	object_prep(q_ptr, k_idx);


	/* Describe */
	object_desc_store(o_name, q_ptr, FALSE, 0);

	/* Mention the object name */
	Term_putstr(0, 0, -1, TERM_WHITE, o_name);


	/* Warriors are illiterate */
	if (!mp_ptr->spell_book) return;
	/* Display spells in readible books */
	if (q_ptr->tval == mp_ptr->spell_book)
	{
		int			sval;

		int			spell = -1;
		int			num = 0;

		byte		spells[64];


		/* Access the item's sval */
		sval = q_ptr->sval;
		/* Extract spells */
		for (spell = 0; spell < 64; spell++)
		{
			/* Check for this spell */
			if ((spell < 32) ?
			    (spell_flags[p_ptr->realm - 1][sval][0] & (1L << spell)) :
			    (spell_flags[p_ptr->realm - 1][sval][1] & (1L << (spell - 32))))
			{
				/* Collect this spell */
				spells[num++] = spell;
			}
		}
		/* Print spells */
		print_spells(spells, num, 2, 0);
	}
}

