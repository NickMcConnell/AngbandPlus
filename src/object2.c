/* File: object2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
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

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
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
		int y = j_ptr->iy;
		int x = j_ptr->ix;

		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
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
	s16b this_o_idx, next_o_idx = 0;


	/* Paranoia */
	if (!in_bounds(y, x)) return;


	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
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
		/* Get the object */
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


	/* Get the object */
	o_ptr = &o_list[i1];


	/* Monster */
	if (o_ptr->held_m_idx)
	{
		monster_type *m_ptr;

		/* Get the monster */
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, num, cnt;

	int cur_lev, cur_dis, chance;


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

			/* Monster */
			if (o_ptr->held_m_idx)
			{
				monster_type *m_ptr;

				/* Get the monster */
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
 * Hack -- we clear the "cave_o_idx[y][x]" field for every grid,
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
		if (!character_dungeon || adult_preserve)
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
			/* Get the location */
			int y = o_ptr->iy;
			int x = o_ptr->ix;

			/* Hack -- see above */
			cave_o_idx[y][x] = 0;
		}

		/* Wipe the object */
		(void)WIPE(o_ptr, object_type);
	}

	/* Reset "o_max" */
	o_max = 1;

	/* Reset "o_cnt" */
	o_cnt = 0;
}


/*
 * Get and return the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
s16b o_pop(void)
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

		/* Get the object */
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
	int i, j, p;

	int k_idx;

	long value, total;

	object_kind *k_ptr;

	alloc_entry *table = alloc_kind_table;


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

		/* Get the index */
		k_idx = table[i].index;

		/* Get the actual kind */
		k_ptr = &k_info[k_idx];

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



/* Know an object fully */
/*
 * Call this after knowing an object. Currently just marks object with
 * mental flag.
 */
void object_mental(object_type *o_ptr)
{
	u32b f1,f2,f3;

	/* Now we know about the item */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Spoil the object */
	object_flags(o_ptr,&f1,&f2,&f3);

	object_can_flags(o_ptr,f1,f2,f3);

	object_not_flags(o_ptr,~(f1),~(f2),~(f3));
}




/*
 * Known is true when the "attributes" of an object are "known".
 *
 * These attributes include tohit, todam, toac, cost, and pval (charges).
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
	/* Remove special inscription, except special inscriptions */
	if ((o_ptr->discount >= INSCRIP_NULL) && (o_ptr->discount < INSCRIP_MIN_HIDDEN)) o_ptr->discount = 0;

	/* The object is not "sensed" */
	o_ptr->ident &= ~(IDENT_SENSE);

	/* The object is not "partially sensed" */
	o_ptr->ident &= ~(IDENT_BONUS);

	/* The object name is not guessed */
	o_ptr->guess1 = 0;
	o_ptr->guess2 = 0;

	/* Remove name3 if artifact */
	if (o_ptr->name1) o_ptr->name3 = 0;

	/* Auto-inscribe */
	if (o_ptr->name2)
	{
		if (!o_ptr->note) o_ptr->note = e_info[o_ptr->name2].note;
	}
	else
	{
		if (!o_ptr->note) o_ptr->note = k_info[o_ptr->k_idx].note;
	}

	/* Now we know about the item */
	o_ptr->ident |= (IDENT_KNOWN);

	/* Now we know what it is, update what we know about it from our artifact memory */
	if (o_ptr->name1)
	{
		object_can_flags(o_ptr,a_list[o_ptr->name1].can_flags1,
				a_list[o_ptr->name1].can_flags2,
				a_list[o_ptr->name1].can_flags3);

		object_not_flags(o_ptr,a_list[o_ptr->name1].not_flags1,
				a_list[o_ptr->name1].not_flags2,
				a_list[o_ptr->name1].not_flags3);
	}
	else
	{
		/* Now we know what it is, update what we know about it from our ego item memory */
		if (o_ptr->name2)
		{
			/* Obvious flags */
			object_can_flags(o_ptr,e_info[o_ptr->name2].obv_flags1,
					 e_info[o_ptr->name2].obv_flags2,
					 e_info[o_ptr->name2].obv_flags3);

			/* Known flags */
			object_can_flags(o_ptr,e_list[o_ptr->name2].can_flags1,
					 e_list[o_ptr->name2].can_flags2,
					 e_list[o_ptr->name2].can_flags3);
			
			object_not_flags(o_ptr,e_list[o_ptr->name2].not_flags1,
					 e_list[o_ptr->name2].not_flags2,
					 e_list[o_ptr->name2].not_flags3);
		}
		else
			object_obvious_flags(o_ptr);
	}

	/* Now we know what it is, update what we know about it */
	object_can_flags(o_ptr,o_ptr->can_flags1,
			o_ptr->can_flags2,
			o_ptr->can_flags3);

	object_not_flags(o_ptr,o_ptr->not_flags1,
			o_ptr->not_flags2,
			o_ptr->not_flags3);

	object_may_flags(o_ptr,o_ptr->may_flags1,
			o_ptr->may_flags2,
			o_ptr->may_flags3);

	/* Know about ego-type */
	if ((o_ptr->name2) && !(o_ptr->ident & (IDENT_STORE)))
	{
		e_info[o_ptr->name2].aware = TRUE;
	}
}


/*
 * Sense the bonus/charges of an object
 *
 * Note under some circumstances, we fully know the object (When
 * its bonuses/charges are all there is to know about it).
 */
void object_bonus(object_type *o_ptr)
{
	int feel = 0;

	/* Identify the bonuses */
	o_ptr->ident |= (IDENT_BONUS);

	/* Is this all we need to know */
	if (!(o_ptr->name1) && ((o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_RING)))
	{
		/* Cursed/Broken */
		if (!cursed_p(o_ptr) && !broken_p(o_ptr)) return;

		/* Remove special inscription, if any */
		if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

		/* Sense the object if allowed */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) o_ptr->discount = INSCRIP_CURSED;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

	}

	/* Is this all we need to know */
	else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		if (object_aware_p(o_ptr)) object_known(o_ptr);
	}

	/* Sense the item (if appropriate) */
	else if (!object_known_p(o_ptr))
	{
		/* Valid "tval" codes */
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
			{
				/* Artifacts */
				if (artifact_p(o_ptr))
				{
					/* Cursed/Broken */
					if (cursed_p(o_ptr) || broken_p(o_ptr)) feel = INSCRIP_TERRIBLE;

					/* Normal */
					else feel = INSCRIP_SPECIAL;
				}

				/* Ego-Items */
				else if (ego_item_p(o_ptr))
				{
					/* Cursed/Broken */
					if (cursed_p(o_ptr) || broken_p(o_ptr)) feel = INSCRIP_WORTHLESS;

					/* Normal */
					else feel = INSCRIP_EXCELLENT;

					/* Superb */
					if (o_ptr->xtra1) feel = INSCRIP_SUPERB;

				}

				/* Still more to know? */
				if (feel)
				{

					/* Sense the object */
					o_ptr->discount = feel;

					/* The item has been sensed */
					o_ptr->ident |= (IDENT_SENSE);

				}

				/* Object is completely known */
				else
				{
					object_known(o_ptr);
				}

				break;
			}
		}

	}

}



/*
 * The player is now aware of the effects of the given object.
 */
void object_aware(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	int i;

	int inven_max = INVEN_TOTAL;

	if (variant_belt_slot) inven_max++;

	/* No longer guessing */
	k_ptr->guess = 0;

	/* No longer tried */
	k_ptr->tried = FALSE;

	/* Auto-inscribe */
	if (!o_ptr->note) o_ptr->note = k_info[o_ptr->k_idx].note;

	/* Fully aware of the effects */
	k_info[o_ptr->k_idx].aware = TRUE;

	/* Process objects */
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		object_type *i_ptr = &o_list[i];

		/* Skip dead objects */
		if (!i_ptr->k_idx) continue;

		/* Re-evaluate the object */
		object_guess_name(i_ptr);
	
	}

	/* Process objects */
	for (i = 1; i < inven_max; i++)
	{
		/* Get the object */
		object_type *i_ptr = &inventory[i];

		/* Skip dead objects */
		if (!i_ptr->k_idx) continue;

		/* Re-evaluate the object */
		object_guess_name(i_ptr);
	
	}

}



/*
 * Something has been "sampled"
 */
void object_tried(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Don't mark it if aware */
	if (k_ptr->aware) return;

	/* Don't mark it if guessed */
	if (k_ptr->aware) return;

	/* Mark it as tried */
	k_ptr->tried = TRUE;
}



/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Use template cost for aware objects */
	if (object_aware_p(o_ptr)) return (k_ptr->cost);

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
 * Return the "real" price of a "known" item, not including discounts.
 *
 * Wand and staffs get cost for each charge.
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
 * Weapons with negative hit+damage bonuses are worthless.
 *
 * Every wearable item with a "pval" bonus is worth extra (see below).
 */
static s32b object_value_real(const object_type *o_ptr)
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
		case TV_INSTRUMENT:
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
			if (f1 & (TR1_BLOWS)) value += (o_ptr->pval * 2000L);

			/* Give credit for speed bonus */
			if (f1 & (TR1_SPEED)) value += (o_ptr->pval * 30000L);

			break;
		}
	}


	/* Analyze the item */
	switch (o_ptr->tval)
	{
		/* Wands/Staffs */
		case TV_WAND:
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
			/* Give credit for hit bonus */
			value += ((o_ptr->to_h - k_ptr->to_h) * 100L);

			/* Give credit for damage bonus */
			value += ((o_ptr->to_d - k_ptr->to_d) * 100L);

			/* Give credit for armor bonus */
			value += (o_ptr->to_a * 100L);

			/* Done */
			break;
		}

		/* Staffs */
		case TV_STAFF:
		{
			/* Pay extra for charges */
			value += ((value / 20) * o_ptr->pval);

		/* Fall through */
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

			/* Hack -- Factor in extra damage sides */
			if ((o_ptr->ds > k_ptr->ds) && (o_ptr->dd == k_ptr->dd))
			{
				value += (o_ptr->ds - k_ptr->ds) * o_ptr->dd * 100L;
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

	/* No negative value */
	if (value < 0) value = 0;

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
 *
 * Now add a value for sensed items. This should be dependent on
 * the minimum good item values in e_info.txt, but just uses a rule
 * of thumb at this stage.
 *
 * Note that discounted items stay discounted forever.
 */
s32b object_value(const object_type *o_ptr)
{
	s32b value;


	/* Known items -- acquire the actual value */
	if (object_known_p(o_ptr))
	{
		/* Broken items -- worthless */
		if (broken_p(o_ptr)) return (0L);

		/* Cursed items -- worthless */
		if (cursed_p(o_ptr)) return (0L);

		/* Real value (see above) */
		value = object_value_real(o_ptr);
	}

	/* Unknown items -- acquire a base value */
	else
	{
		/* Hack -- Felt broken items */
		if ((o_ptr->ident & (IDENT_SENSE)) && broken_p(o_ptr)) return (0L);

		/* Hack -- Felt cursed items */
		if ((o_ptr->ident & (IDENT_SENSE)) && cursed_p(o_ptr)) return (0L);

		/* Base value (see above) */
		value = object_value_base(o_ptr);

		/* Hack -- Partially identified items */
		if (o_ptr->ident & (IDENT_BONUS))
		{

			switch(o_ptr->tval)
			{
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
					/* Give credit for hit bonus */
					value += ((o_ptr->to_h) * 100L);

					/* Give credit for damage bonus */
					value += ((o_ptr->to_d) * 100L);

					/* Give credit for armor bonus */
					value += (o_ptr->to_a * 100L);

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

					/* Done */
					break;
				}


				/* Wands/staffs */
				case TV_WAND:
				case TV_STAFF:
				{
					/* Hack -- negative/zero hit/damage bonuses */
					if (o_ptr->pval <= 0) return (0L);

					/* Factor in the bonuses */
					value += (o_ptr->pval * 5L);

					/* Done */
					break;
				}

			}

		}
		/* Hack -- Felt good items */
		else if (o_ptr->ident & (IDENT_SENSE))
		{
			s32b bonus=0L;

			if (variant_great_id) switch (o_ptr->discount)
			{
				case INSCRIP_SPECIAL:
				{
					bonus =10000;
					break;
				}
				case INSCRIP_SUPERB:
				{
					bonus =2000;
					break;
				}
				case INSCRIP_EXCELLENT:
				{
					bonus =400;
					break;
				}
				case INSCRIP_GREAT:
				{
					bonus =800;
					break;
				}
				case INSCRIP_VERY_GOOD:
				{
					bonus =400;
					break;
				}
				case INSCRIP_GOOD:
				{
					bonus =100;
					break;
				}
			}

			if ((o_ptr->tval == TV_SHOT) ||
				(o_ptr->tval == TV_ARROW) || 
				(o_ptr->tval == TV_BOLT))
			{
				value += bonus/20;
			}
			else
			{
				value += bonus;
			}
		}
	}


	/* Apply discount (if any) */
	if (o_ptr->discount > 0 && o_ptr->discount < INSCRIP_NULL)
	{
		value -= (value * o_ptr->discount / 100L);
	}


	/* Return the final value */
	return (value);
}


/*
 * Determine if the object has "=s" in its inscription.
 */
static bool auto_stack_okay(const object_type *o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->note) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->note), '=');

	/* Process inscription */
	while (s)
	{
		/* Auto-stack on "=s" */
		if (s[1] == 's') return (TRUE);

		/* Find another '=' */
		s = strchr(s + 1, '=');
	}

	/* Don't auto pickup */
	return (FALSE);
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
 */
bool object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return (0);

	/* Analyze the items */
	switch (o_ptr->tval)
	{
		/* Spells */
		case TV_SPELL:
		{
			/* Never okay */
			return (0);
		}


		/* Bodies, Skeletons, Skins, Eggs, Statues */
		case TV_STATUE:
		case TV_EGG:
		case TV_BONE:
		case TV_BODY:
		case TV_SKIN:
		case TV_HOLD:
		case TV_FIGURE:
		{
			/* Require 'similar' timeouts */
			if ((o_ptr->timeout != j_ptr->timeout) && (!stack_force_times)
				&& !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr)) )
			{
				if ((o_ptr->timeout != 0) && (j_ptr->timeout != 0)) return (0);
				if (!variant_time_stacks) return (0);
			}

			/* Probably okay */
			break;
		}

		/* Food and Potions and Scrolls */
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		case TV_RUNESTONE:
		case TV_MAP:
		case TV_FLASK:
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_SONG_BOOK:
		{
			/* Assume okay */
			break;
		}

		/* Staffs and Wands */
		case TV_STAFF:
		case TV_WAND:
		{
			/* Require identical knowledge of both items */
			if (object_known_p(o_ptr) != object_known_p(j_ptr)) return (0);

			/* Require 'similar' pvals */
			if ((o_ptr->pval != j_ptr->pval))
			{
				/* Check for stacks allowed */
				if (!variant_pval_stacks) return (0);

				/* Check for sign difference */
				if ((o_ptr->pval > 0)&&(j_ptr->pval <= 0)) return (0);
				if ((o_ptr->pval < 0)&&(j_ptr->pval >= 0)) return (0);
				if ((o_ptr->pval == 0)&&(j_ptr->pval != 0)) return (0);

				/* Line 1 -- no force pval stacking option */
				/* Line 2 -- 1st pval is not 1 less than 2nd pval, or 1st pval is a pval stack */
				/* Line 3 -- 1st pval is not 1 greater than 2nd pval, or 2nd pval is a pval stack */
				/* Line 4 -- an item has auto_stack */
				if ((!stack_force_pvals)
					&& ((o_ptr->pval != j_ptr->pval-1) || (o_ptr->stackc))
					&& ((o_ptr->pval != j_ptr->pval+1) || (j_ptr->stackc))
					&& !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr))) return (0);
			}

			/* Probably okay */
			break;
		}

		/* Staffs and Wands and Rods */
		case TV_ROD:
		{
			/* Require identical knowledge of both items */
			if (object_known_p(o_ptr) != object_known_p(j_ptr)) return (0);

			/* Require 'similar' timeouts */
			if ((o_ptr->timeout != j_ptr->timeout) && (!stack_force_times)
				&& !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr)) )
			{
				if ((o_ptr->timeout != 0) && (j_ptr->timeout != 0)) return (0);
				if (!variant_time_stacks) return (0);
			}

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
			/* Fall through */
		}

		/* Rings, Amulets, Lites */
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			/* Require knowledge */
			if ((!variant_pval_stacks) &&
				(!object_known_p(o_ptr)
				  || !object_known_p(j_ptr))) return (0);
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

			/* Require similar "pval" code */
			if (o_ptr->pval != j_ptr->pval)
			{

				if (!variant_pval_stacks) return (0);

				/* Check for sign difference */
				if ((o_ptr->pval > 0)&&(j_ptr->pval <= 0)) return (0);
				if ((o_ptr->pval < 0)&&(j_ptr->pval >= 0)) return (0);
				if ((o_ptr->pval == 0)&&(j_ptr->pval != 0)) return (0);

				/* Line 1 -- no force pval stacking option */
				/* Line 2 -- 1st pval is not 1 less than 2nd pval, or 1st pval is a pval stack */
				/* Line 3 -- 1st pval is not 1 greater than 2nd pval, or 2nd pval is a pval stack */
				/* Line 4 -- an item has auto_stack */
				if ((!stack_force_pvals) &&
					((o_ptr->pval != j_ptr->pval-1) || (o_ptr->stackc))  &&
					((o_ptr->pval != j_ptr->pval+1) || (j_ptr->stackc))
					&& !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr)) ) return (0);             }

			/* Require identical "artifact" names */
			if (o_ptr->name1 != j_ptr->name1) return (FALSE);

			/* Require identical "ego-item" names */
			if (o_ptr->name2 != j_ptr->name2) return (FALSE);

			/* Hack -- Never stack "powerful" items */
			if (o_ptr->xtra1 || j_ptr->xtra1) return (FALSE);

			/* Require 'similar' timeouts */
			if ((o_ptr->timeout != j_ptr->timeout) && (!stack_force_times)
				&& !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr)) )
			{
				if ((o_ptr->timeout != 0) && (j_ptr->timeout != 0)) return (0);
				if (!variant_pval_stacks) return (0);
			}

			/* Require identical "values" */
			if (o_ptr->ac != j_ptr->ac) return (FALSE);
			if (o_ptr->dd != j_ptr->dd) return (FALSE);
			if (o_ptr->ds != j_ptr->ds) return (FALSE);

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

	/* Hack -- Require identical "cursed" and "broken" status */
	if (((o_ptr->ident & (IDENT_CURSED)) != (j_ptr->ident & (IDENT_CURSED))) ||
	    ((o_ptr->ident & (IDENT_BROKEN)) != (j_ptr->ident & (IDENT_BROKEN))))
	{
		return (0);
	}

	/* Hack -- Require identical "xtra1" and "xtra2" status */
	if ((o_ptr->xtra1 != j_ptr->xtra1) || (o_ptr->xtra2 !=j_ptr->xtra2)) return (0);

	/* Hack -- Require identical "name3" */
	if (o_ptr->name3 != j_ptr->name3) return(0);

	/* Hack -- Require compatible inscriptions */
	if (o_ptr->note != j_ptr->note)
	{
		/* Normally require matching inscriptions */
		if (!stack_force_notes && !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr)) ) return (0);

		/* Never combine different inscriptions */
		if (o_ptr->note && j_ptr->note) return (0);
	}


	/* Hack -- Require compatible "discount" fields */
	if (o_ptr->discount != j_ptr->discount)
	{
		/* Both are (different) special inscriptions */
		if ((o_ptr->discount >= INSCRIP_NULL) &&
		    (j_ptr->discount >= INSCRIP_NULL))
		{
			/* Normally require matching inscriptions */
			return (0);
		}

		/* One is a special inscription, one is a discount or nothing */
		else if ((o_ptr->discount >= INSCRIP_NULL) ||
			 (j_ptr->discount >= INSCRIP_NULL))
		{
			/* Normally require matching inscriptions */
			if (!stack_force_notes && !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr)) ) return (0);

			/* Hack -- Never merge a special inscription with a discount */
			if ((o_ptr->discount > 0) && (j_ptr->discount > 0)) return (0);
		}

		/* One is a discount, one is a (different) discount or nothing */
		else
		{
			/* Normally require matching discounts */
			if (!stack_force_costs && !(auto_stack_okay(o_ptr) && auto_stack_okay(j_ptr))) return (0);
		}
	}


	/* Maximal "stacking" limit */
	if (total >= MAX_STACK_SIZE) return (0);


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
 * The blending of the "discount" field assumes that either (1) one is a
 * special inscription and one is nothing, or (2) one is a discount and
 * one is a smaller discount, or (3) one is a discount and one is nothing,
 * or (4) both are nothing.  In all of these cases, we can simply use the
 * "maximum" of the two "discount" fields.
 *
 * These assumptions are enforced by the "object_similar()" code.
 *
 * We now support 'blending' of timeouts and pvals. - ANDY
 *
 * With stack_force_pvals, pvals are averaged across a stack of items.
 *
 * Without, we only stack items that have 1 pval difference,
 * and use the pvals field to tell us how many of the higher pval
 * items are in the stack. This should be particularly helpful with
 * wands and staves as we will not unstack a pile of same charge items
 * by using them until a wand is emptied completely.
 *
 * With stack_force_timeouts, timeouts are the maximum across a stack
 * of items.
 *
 * Without, we allow 1 charging item to stack with charged items.
 *
 * Note we treat rods like timeout items rather than pval items. This
 * unnecessarily complicates the code in a lot of places.
 *
 * Note for negative pval items, we don't use higher in the absolute
 * sense, and we have to mess around because division and modulo on
 * negative numbers is undefined in C.
 */

void object_absorb(object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	int number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Blend "pval" if necessary*/
	if (o_ptr->pval != j_ptr->pval)
	{
		/* Weird calculation. But it works. */
		s32b stackt = ((o_ptr->pval * o_ptr->number)
				+ (j_ptr->pval * j_ptr->number)
				- o_ptr->stackc
				- j_ptr->stackc);

		bool negative = (stackt < 0) ? TRUE : FALSE;

		/* Modulo on negative signs undefined in C */
		if (negative) stackt = -stackt;

		/* Assign pval */
		o_ptr->pval = stackt / total;
		o_ptr->stackc = number - (stackt % total);

		/* Fix pval */
		if ((o_ptr->stackc)) o_ptr->pval++;

		/* Hack -- Fix stack count */
		if (o_ptr->stackc >= number) o_ptr->stackc = 0;

		/* Correction for negative pvals */
		if (negative)
		{
			/* Reverse sign */
			o_ptr->pval = -o_ptr->pval;

			/* Reverse stack count */
			o_ptr->stackc = number - o_ptr->stackc;

			/* Fix pval */
			if ((o_ptr->stackc)) o_ptr->pval++;

			/* Paranoia -- always ensure cursed items */
			if (o_ptr->pval >=0)
			{
				o_ptr->pval = -1;
				o_ptr->stackc = 0;
			}

		}
	}

	/* Blend "timeout" if necessary */
	if (o_ptr->timeout != j_ptr->timeout)
	{
		/* Hack -- Fix stack count */
		if ((o_ptr->timeout) && !(o_ptr->stackc)) o_ptr->stackc = o_ptr->number;

		/* Hack -- Fix stack count */
		if ((j_ptr->timeout) && !(j_ptr->stackc)) o_ptr->stackc = j_ptr->number;

		/* Add stack count */
		o_ptr->stackc += j_ptr->stackc;

		/* Hack -- Fix stack count */
		if (o_ptr->stackc == number) o_ptr->stackc = 0;

		/* Hack -- Use Maximum timeout */
		if (o_ptr->timeout < j_ptr->timeout) o_ptr->timeout = j_ptr->timeout;
	}

	/* Add together the item counts */
	o_ptr->number = number;

	/* Hack -- Blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr);

	/* Hack -- Blend "bonus" status */
	if (j_ptr->ident & (IDENT_BONUS)) o_ptr->ident |= (IDENT_BONUS);

	/* Hack -- Blend "store" status */
	if (j_ptr->ident & (IDENT_STORE)) o_ptr->ident |= (IDENT_STORE);

	/* Hack -- Blend "mental" status */
	if (j_ptr->ident & (IDENT_MENTAL)) o_ptr->ident |= (IDENT_MENTAL);

	/* Hack -- Blend "notes" */
	if (j_ptr->note != 0) o_ptr->note = j_ptr->note;

	/* Hack -- Blend flags */
	object_can_flags(o_ptr,j_ptr->can_flags1,j_ptr->can_flags2,j_ptr->can_flags3);
	object_not_flags(o_ptr,j_ptr->not_flags1,j_ptr->not_flags2,j_ptr->not_flags3);
	object_may_flags(o_ptr,j_ptr->may_flags1,j_ptr->may_flags2,j_ptr->may_flags3);

	/* Mega-Hack -- Blend "discounts" */
	if (o_ptr->discount < j_ptr->discount) o_ptr->discount = j_ptr->discount;

	/* Mega Hack -- Blend "usages" */
	if (o_ptr->usage < j_ptr->usage) o_ptr->usage = j_ptr->usage;

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

	/* Since this function will be called at startup, msg_format is not yet usable. */
#if 0
	/* Oops */
	msg_format("No object (%d,%d)", tval, sval);
#endif

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
 * Prepare an object based on an existing object
 */
void object_copy(object_type *o_ptr, const object_type *j_ptr)
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
	(void)WIPE(o_ptr, object_type);

	/* Save the kind index */
	o_ptr->k_idx = k_idx;

	/* Efficiency -- tval/sval */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;

	/* Default "pval" */
	o_ptr->pval = k_ptr->pval;

	/* Default number */
	o_ptr->number = 1;

	/* Default stack count */
	o_ptr->stackc = 0;

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
	if (k_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

	o_ptr->can_flags1 = 0x0L;
	o_ptr->can_flags2 = 0x0L;
	o_ptr->can_flags3 = 0x0L;

	o_ptr->may_flags1 = 0x0L;
	o_ptr->may_flags2 = 0x0L;
	o_ptr->may_flags3 = 0x0L;

	o_ptr->not_flags1 = 0x0L;
	o_ptr->not_flags2 = 0x0L;
	o_ptr->not_flags3 = 0x0L;
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
	if (rand_int(MAX_DEPTH) < extra) bonus++;


	/* The "stand" is equal to one quarter of the max */
	stand = (max / 4);

	/* Hack -- determine fraction of error */
	extra = (max % 4);

	/* Hack -- simulate floating point computations */
	if (rand_int(4) < extra) stand++;


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
 * Cheat -- describe a created object for the user
 */
static void object_mention(object_type *o_ptr)
{
	char o_name[80];

	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

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
 * Attempt to change an object into an ego-item -MWK-
 * Better only called by apply_magic()
 * The return value is currently unused, but a wizard might be interested in it.
 */
static bool make_ego_item(object_type *o_ptr, bool cursed, bool great)
{
	int i, j, level;

	int e_idx;

	long value, total;

	ego_item_type *e_ptr;

	alloc_entry *table = alloc_ego_table;

	/* Fail if object already is ego or artifact */
	if (o_ptr->name1) return (FALSE);
	if (o_ptr->name2) return (FALSE);

	level = p_ptr->depth;

	/* Boost level (like with object base types) */
	if (level > 0)
	{
		/* Occasional "boost" */
		if (rand_int(GREAT_EGO) == 0)
		{
			/* The bizarre calculation again */
			level = 1 + (level * MAX_DEPTH / randint(MAX_DEPTH));
		}
	}

	/*
	 * Hack - *Cursed* items have a totally different level distribution
	 * This is needed to obtain the old Weapons of Morgul distribution.
	 */
	if (cursed)
	{
		/* Probability goes linear with level */
		level = p_ptr->depth + rand_int(127);
	}

	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_ego_size; i++)
	{
		/* Default */
		table[i].prob3 = 0;

		/* Objects are sorted by depth */
		if (table[i].level > level) continue;

		/* Get the index */
		e_idx = table[i].index;

		/* Get the actual kind */
		e_ptr = &e_info[e_idx];

		/* Test if this is a possible ego-item for value of (cursed) */
		if (!cursed && (e_ptr->flags3 & TR3_LIGHT_CURSE)) continue;
		if (cursed && !(e_ptr->flags3 & TR3_LIGHT_CURSE)) continue;

		/* Test if this is a great item or continue */
		if (great && (e_ptr->cost < 1000) && (e_ptr->level == 0)) continue;

		/* Test if this is a legal ego-item type for this object */
		for (j = 0; j < 3; j++)
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
	o_ptr->name2 = (byte)table[i].index;

	/* Auto-inscribe if necessary */
	if (cheat_auto) o_ptr->note = e_info[o_ptr->name2].note;

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
 * We *prefer* to create the special artifacts in order, but this is
 * normally outweighed by the "rarity" rolls for those artifacts.  The
 * only major effect of this logic is that the Phial (with rarity one)
 * is always the first special artifact created.
 */
static bool make_artifact_special(object_type *o_ptr)
{
	int i;

	int k_idx;


	/* No artifacts, do nothing */
	if (adult_no_artifacts) return (FALSE);

	/* No artifacts in the basic town */
	if (!p_ptr->depth) return (FALSE);

	/* Check the special artifacts */
	for (i = 0; i < ART_MIN_NORMAL; ++i)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		/* Enforce minimum "depth" (loosely) */
		if (a_ptr->level > p_ptr->depth)
		{
			/* Get the "out-of-depth factor" */
			int d = (a_ptr->level - p_ptr->depth) * 2;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* Artifact "rarity roll" */
		if (rand_int(a_ptr->rarity) != 0) continue;

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Enforce minimum "object" level (loosely) */
		if (k_info[k_idx].level > object_level)
		{
			/* Get the "out-of-depth factor" */
			int d = (k_info[k_idx].level - object_level) * 5;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* Assign the template */
		object_prep(o_ptr, k_idx);

		/* Mark the item as an artifact */
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
	int i;


	/* No artifacts, do nothing */
	if (adult_no_artifacts) return (FALSE);

	/* No artifacts in the basic town */
	if (!p_ptr->depth) return (FALSE);

	/* Paranoia -- no "plural" artifacts */
	if (o_ptr->number != 1) return (FALSE);

	/* Check the artifact list (skip the "specials") */
	for (i = ART_MIN_NORMAL; i < z_info->a_max; i++)
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
		if (a_ptr->level > p_ptr->depth)
		{
			/* Get the "out-of-depth factor" */
			int d = (a_ptr->level - p_ptr->depth) * 2;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* We must make the "rarity roll" */
		if (rand_int(a_ptr->rarity) != 0) continue;

		/* Mark the item as an artifact */
		o_ptr->name1 = i;

		/* Success */
		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}


/*
 * Charge a new wand/staff.
 */
static void charge_item(object_type *o_ptr)
{
	if (k_info[o_ptr->k_idx].level < 2)
	{
		o_ptr->pval = randint(15)+8; 
	}
	else if (k_info[o_ptr->k_idx].level < 4)
	{
		o_ptr->pval = randint(10)+6; 
	}
	else if (k_info[o_ptr->k_idx].level < 40)
	{
		o_ptr->pval = randint(8)+6; 
	}
	else if (k_info[o_ptr->k_idx].level < 50)
	{
		o_ptr->pval = randint(6)+2; 
	}
	else if (k_info[o_ptr->k_idx].level < 60)
	{
		o_ptr->pval = randint(4)+2; 
	}
	else if (k_info[o_ptr->k_idx].level < 70)
	{
		o_ptr->pval = randint(3)+1; 
	}
	else
	{
		o_ptr->pval = randint(2)+1; 
	}

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
		case TV_STAFF:
		{
			/* Very Good */
			if (power > 1)
			{
				/* Hack -- Super-charge the damage dice */
				while ((o_ptr->dd * o_ptr->ds > 0) &&
				       (rand_int(15) == 0))
				{
					o_ptr->dd++;
				}

				/* Hack -- Limit the damage dice to max of 9*/
				if (o_ptr->dd > 9) o_ptr->dd = 9;

				/* Hack -- Super-charge the damage sides */
				while ((o_ptr->dd * o_ptr->ds > 0) &&
				       (rand_int(15) == 0))
				{
					o_ptr->ds++;
				}

				/* Hack -- Limit the damage dice to max of 9*/
				if (o_ptr->ds > 9) o_ptr->ds = 9;
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
				/* Hack -- super-charge the damage side */
				while ((o_ptr->dd * o_ptr->ds > 0) &&
				       (rand_int(25) == 0))
				{
					o_ptr->ds++;
				}

				/* Hack -- restrict the damage side */
				if (o_ptr->ds > 9) o_ptr->ds = 9;
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
#if 0
				/* Flames, Acid, Ice */
				case SV_RING_FLAMES:
				case SV_RING_ACID:
				case SV_RING_ICE:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 5 + randint(5) + m_bonus(10, level);
					break;
				}
#endif
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
				case SV_AMULET_INFRAVISION:
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

	/* Prevent compiler warning */
	(void)level;
	(void)power;

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		case TV_LITE:
		{
			/* Hack -- Torches -- random fuel */
			if (o_ptr->sval == SV_LITE_TORCH)
			{
				if (o_ptr->pval > 0) o_ptr->pval = randint(o_ptr->pval);
			}

			/* Hack -- Lanterns -- random fuel */
			if (o_ptr->sval == SV_LITE_LANTERN)
			{
				if (o_ptr->pval > 0) o_ptr->pval = randint(o_ptr->pval);
			}

			break;
		}

		case TV_WAND:
		case TV_STAFF:
		{
			/* Hack -- charge wands */
			charge_item(o_ptr);

			break;
		}

		case TV_EGG:
		{
			/* Hack -- eggs/spores will hatch */
			if (o_ptr->name3 > 0) o_ptr->timeout = o_ptr->weight * damroll(2,6) * 10;
		}

	}
}






/*
 * Complete the "creation" of an object by applying "magic" to the item
 *
 * This includes not only rolling for random bonuses, but also putting the
 * finishing touches on ego-items and artifacts, giving charges to wands and
 * staffs and giving fuel to lites.
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
 * "good" and "great" arguments are false.  Objects which are forced "great"
 * get three extra "attempts" to become an artifact.
 */
void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great)
{
	int i, rolls, f1, f2, power;


	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;


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
	if (good || (rand_int(100) < f1))
	{
		/* Assume "good" */
		power = 1;

		/* Roll for "great" */
		if (great || (rand_int(100) < f2)) power = 2;
	}

	/* Roll for "cursed" */
	else if (rand_int(100) < f1)
	{
		/* Assume "cursed" */
		power = -1;

		/* Roll for "broken" */
		if (rand_int(100) < f2) power = -2;
	}


	/* Assume no rolls */
	rolls = 0;

	/* Get one roll if excellent */
	if (power >= 2) rolls = 1;

	/* Get four rolls if forced great */
	if (great) rolls = 4;

	/* Get no rolls if not allowed */
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

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- extract the "cursed" flag */
		if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

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
			if (((power > 1) ? TRUE : FALSE) || ((power < -1) ? TRUE : FALSE))
				(void)make_ego_item(o_ptr, (bool)((power < 0) ? TRUE : FALSE),great);
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
			if (((power > 1) ? TRUE : FALSE) || (power < -1))
				(void)make_ego_item(o_ptr, (bool)((power < 0) ? TRUE : FALSE),great);
			break;
		}

		case TV_INSTRUMENT:
		{
			if (((power > 1) ? TRUE : FALSE) || (power < -1))
				(void)make_ego_item(o_ptr, (bool)((power < 0) ? TRUE : FALSE),great);
			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			if (!power && (rand_int(100) < 50)) power = -1;
			a_m_aux_3(o_ptr, lev, power);
			break;
		}

		/* Worthless staffs are cursed */
		case TV_STAFF:
		{
			if (!(k_info[o_ptr->k_idx].cost)) power = -1;
			else if (power < 0) power = 0;

			if (power) a_m_aux_1(o_ptr, lev, power);
			a_m_aux_4(o_ptr, lev, power);
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

		/* Extra powers */
		if (e_ptr->xtra)
		{
			o_ptr->xtra1 = e_ptr->xtra;
			o_ptr->xtra2 = (byte)rand_int(object_xtra_size[e_ptr->xtra]);
		}

		/* Hack -- acquire "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (e_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

		/* Hack -- apply extra penalties if needed */
		if (cursed_p(o_ptr) || broken_p(o_ptr))
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h > 0) o_ptr->to_h -= randint(e_ptr->max_to_h);
			if (e_ptr->max_to_d > 0) o_ptr->to_d -= randint(e_ptr->max_to_d);
			if (e_ptr->max_to_a > 0) o_ptr->to_a -= randint(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval > 0) o_ptr->pval -= randint(e_ptr->max_pval);
		}

		/* Hack -- apply extra bonuses if needed */
		else
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h > 0) o_ptr->to_h += randint(e_ptr->max_to_h);
			if (e_ptr->max_to_d > 0) o_ptr->to_d += randint(e_ptr->max_to_d);
			if (e_ptr->max_to_a > 0) o_ptr->to_a += randint(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval > 0) o_ptr->pval += randint(e_ptr->max_pval);
		}

		/* Hack -- apply rating bonus */
		rating += e_ptr->rating;

		/* Cheat -- describe the item */
		if (cheat_peek) object_mention(o_ptr);

		/* Done */
		return;
	}


	/* Examine real objects */
	if (o_ptr->k_idx)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Hack -- acquire "broken" flag */
		if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
	}
}



static int hack_body_weight[52]=
{
	500,
	5,
	10,
	300,
	100,
	10,
	0,
	40,
	1,
	5,
	20,
	15,
	30,
	30,
	30,
	50,
	20,
	10,
	6,
	40,
	25,
	15,
	15,
	30,
	20,
	20,
	6,
	3,
	5,
	60,
	20,
	12,
	60,
	12,
	4,
	4,
	12,
	14,
	4,
	15,
	18,
	16,
	16,
	8,
	12,
	16,
	12,
	15,
	6,
	0,
	25,
	16
};

static void modify_weight(object_type *j_ptr, int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Are we done? */
	if ((j_ptr->tval != TV_BONE) && (j_ptr->tval != TV_EGG)
		&& (j_ptr->tval != TV_SKIN) && (j_ptr->tval != TV_BODY) &&
		(j_ptr->tval != TV_HOLD)) return;

	/* Hack -- Increase weight */
	if ((r_ptr->d_char >='A') && (r_ptr->d_char <='Z'))
	{
		j_ptr->weight = j_ptr->weight * hack_body_weight[r_ptr->d_char-'A'] / 10;
	}

	/* Hack -- Increase weight */
	else if ((r_ptr->d_char >='a') && (r_ptr->d_char <='z'))
	{
		j_ptr->weight = j_ptr->weight * hack_body_weight[r_ptr->d_char-'a'+ 26] / 10;
	}

	/* Minimum weight */
	if (j_ptr->weight < 1) j_ptr->weight = 1;
}


static bool (*get_mon_old_hook)(int r_idx);

static int name_drop_k_idx;

static bool name_drop_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	object_kind *j_ptr = &k_info[name_drop_k_idx];

	/* Apply old hook restriction first */
	if (get_mon_old_hook && !(get_mon_old_hook(r_idx))) return (FALSE);

	/* Skip uniques */
	if ((j_ptr->tval != TV_STATUE) && (r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	if (j_ptr->tval == TV_BONE)
	{
		/* Skip if monster does not have body part */
		if ((j_ptr->sval == SV_BONE_SKULL) && !(r_ptr->flags7 & (RF7_HAS_SKULL))) return (FALSE);
		else if ((j_ptr->sval == SV_BONE_BONE) && !(r_ptr->flags7 & (RF7_HAS_SKELETON))) return (FALSE);
		else if ((j_ptr->sval == SV_BONE_SKELETON) && !(r_ptr->flags7 & (RF7_HAS_SKELETON))) return (FALSE);
		else if ((j_ptr->sval == SV_BONE_TEETH) && !(r_ptr->flags7 & (RF7_HAS_TEETH))) return (FALSE);
	}
	else if (j_ptr->tval == TV_EGG)
	{
		/* Spore shooters have spores */
		if ((j_ptr->sval == SV_EGG_SPORE) && !(r_ptr->flags7 & (RF7_HAS_SPORE))) return (FALSE);

		/* Egg-layers shed their skin or have feathers or scales */
		else if ((j_ptr->sval == SV_EGG_EGG) || (!(r_ptr->flags7 & (RF7_HAS_SKIN | RF7_HAS_FEATHER | RF7_HAS_SCALE)))) return (FALSE);

		/* Undead/demons never have eggs */
		if (r_ptr->flags3 & (RF3_UNDEAD | RF3_DEMON)) return (FALSE);

		/* Hack -- dragons only hatch babies */
		if ((r_ptr->flags3 & (RF3_DRAGON)) && (!strstr(r_name+r_ptr->name, "aby"))) return (FALSE);

	}
	else if (j_ptr->tval == TV_SKIN)
	{
		/* Skip if monster does not have body part */
		if ((j_ptr->sval == SV_SKIN_FEATHER) && !(r_ptr->flags7 & (RF7_HAS_FEATHER))) return (FALSE);
		else if ((j_ptr->sval == SV_SKIN_SCALE) && !(r_ptr->flags7 & (RF7_HAS_SCALE))) return (FALSE);
		else if ((j_ptr->sval == SV_SKIN_FUR) && !(r_ptr->flags7 & (RF7_HAS_FUR))) return (FALSE);

		/* Hack -- we allow lots of skins, Mr Lecter */
		else if ((j_ptr->sval == SV_SKIN_SKIN) && !(r_ptr->flags7 & (RF7_HAS_SLIME | RF7_HAS_FEATHER | RF7_HAS_SCALE | RF7_HAS_FUR))
			&& (r_ptr->flags7 & (RF7_HAS_CORPSE))) return (FALSE);

	}
	else if (j_ptr->tval == TV_BODY)
	{
		/* Skip if monster does not have body part */
		if ((j_ptr->sval == SV_BODY_HEAD) && !(r_ptr->flags7 & (RF7_HAS_HEAD))) return (FALSE);
		else if ((j_ptr->sval == SV_BODY_HAND) && !(r_ptr->flags7 & (RF7_HAS_HAND))) return (FALSE);
		else if ((j_ptr->sval == SV_BODY_ARM) && !(r_ptr->flags7 & (RF7_HAS_ARM))) return (FALSE);
		else if ((j_ptr->sval == SV_BODY_LEG) && !(r_ptr->flags7 & (RF7_HAS_LEG))) return (FALSE);
		else if ((j_ptr->sval == SV_BODY_WING) && !(r_ptr->flags7 & (RF7_HAS_WING))) return (FALSE);
		else if ((j_ptr->sval == SV_BODY_CLAW) && !(r_ptr->flags7 & (RF7_HAS_CLAW))) return (FALSE);
	}
	else if (j_ptr->tval == TV_HOLD)
	{
		/* Only fit elementals in bottles */
		if ((j_ptr->sval == SV_HOLD_BOTTLE) && !(r_ptr->d_char == 'E') && !(r_ptr->d_char == 'v')) return (FALSE);

		/* Only fit non-spellcasters in sacks */
		else if ((j_ptr->sval == SV_HOLD_SACK) && ((r_ptr->flags4) || (r_ptr->flags5) || (r_ptr->flags6))) return (FALSE);

		/* Only fit undead in boxes */
		else if ((j_ptr->sval == SV_HOLD_BOX) && !(r_ptr->flags3 & (RF3_UNDEAD))) return (FALSE);

		/* Only fit animals in cages */
		else if ((j_ptr->sval == SV_HOLD_CAGE) && !(r_ptr->flags3 & (RF3_ANIMAL))) return (FALSE);
	}

	else if ((j_ptr->tval == TV_STATUE) || (j_ptr->tval == TV_FIGURE))
	{

		/* Skip never move/never blow */
		if (r_ptr->flags1 & (RF1_NEVER_MOVE | RF1_NEVER_BLOW)) return (FALSE);

		/* Skip stupid */
		if (r_ptr->flags2 & (RF2_STUPID)) return (FALSE);

		/* Hack -- force unique statues */
		if ((j_ptr->tval == TV_STATUE) && !(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	}

	/* Accept */
	return (TRUE);

}



/*
 * Mark item as dropped by monster. Also deal with randomly generated
 * skeletons, bodies and skins.
 */
static void name_drop(object_type *j_ptr)
{
	int r_idx = 0;

	/* Are we done? */
	if ((j_ptr->tval != TV_BONE) && (j_ptr->tval != TV_EGG) && (j_ptr->tval != TV_STATUE)
		&& (j_ptr->tval != TV_SKIN) && (j_ptr->tval != TV_BODY) &&
		(j_ptr->tval != TV_HOLD) && (j_ptr->tval != TV_FIGURE)) return;

	if ((rand_int(100) < (30+ (p_ptr->depth/2))) || (race_drop_idx))
	{
		/* Store the old hook */
		get_mon_old_hook = get_mon_num_hook;

		/* Set the hook */
		get_mon_num_hook = name_drop_okay;

		/* Store the item kind */
		name_drop_k_idx = j_ptr->k_idx;

		/* Prep the list */
		get_mon_num_prep();

		/* Generate monster appropriate for level */
		r_idx = get_mon_num(p_ptr->depth);

		/* Restore the old hook */
		get_mon_num_hook = get_mon_old_hook;

		/* Prep the list */
		get_mon_num_prep();

		/* Failure? */
		if (!r_idx) return;

		/* Flavor the skeleton */
		j_ptr->name3 = r_idx;

	}
}

/*
 * Hack -- determine if a template is "mushroom".
 */
static bool kind_is_shroom(int k_idx)
{

	object_kind *k_ptr = &k_info[k_idx];

	if (k_ptr->tval != TV_FOOD) return (FALSE);

	if (k_ptr->sval >= SV_FOOD_MIN_FOOD) return (FALSE);

	return (TRUE);

}

/*
 * Hack -- determine if a template is "good".
 *
 * Note that this test only applies to the object *kind*, so it is
 * possible to choose a kind which is "good", and then later cause
 * the actual object to be cursed.  We do explicitly forbid objects
 * which are known to be boring or which start out somewhat damaged.
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

		case TV_STAFF:
		{
			if (k_ptr->level < 50) return (FALSE);
		/* Fall through */
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
		case TV_SONG_BOOK:
		{
			if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return (TRUE);
			return (FALSE);
		}

		/* Rune stones are good */
		case TV_RUNESTONE:
		{
			return (TRUE);
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

		/* Scrolls/Potions -- Deep is good */
		case TV_SCROLL:
		case TV_POTION:
		{
			if (k_ptr->level >= 50) return (TRUE);
			return (FALSE);
		}

	}

	/* Assume not good */
	return (FALSE);
}

/*
 * Hack -- determine if a template is dropped by a race.
 */
static bool kind_is_race(int k_idx)
{
	monster_race *r_ptr = &r_info[race_drop_idx];
	object_kind *k_ptr = &k_info[k_idx];

	if ((r_ptr->flags1 & (RF1_DROP_GOOD)) && (!kind_is_good(k_idx))) return (FALSE);

	/* Analyze the item type */
	switch (k_ptr->tval)
	{
		/* Hard Armor/Dragon Armor/Shield/Helm */
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_SHIELD:
		case TV_HELM:
		{
			if (r_ptr->flags7 & (RF7_DROP_ARMOR)) return (TRUE);
			return (FALSE);
		}
		/* Soft armor/boots/cloaks/gloves */
		case TV_SOFT_ARMOR:
		{
			/* Hack -- armored monsters don't carry soft armor */
			if (r_ptr->flags2 & (RF2_ARMOR)) return (FALSE);
		}
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_BOOTS:
		{
			if (r_ptr->flags7 & (RF7_DROP_CLOTHES)) return (TRUE);
			return (FALSE);
		}
		/* Weapons */
		case TV_SWORD:
		case TV_POLEARM:
		{
			/* Hack -- priests other than shamans only carry hafted weapons */
			if ((r_ptr->flags2 & (RF2_PRIEST)) && !(r_ptr->flags2 & (RF2_MAGE))) return (FALSE);

		/* Fall through */
		}
		case TV_HAFTED:
		{
			/* Hack -- mages/priests/thieves/archers only carry weapons < 20 lbs */
			if ((r_ptr->flags2 & (RF2_PRIEST | RF2_MAGE | RF2_SNEAKY | RF2_ARCHER)) && (k_ptr->weight >= 200)) return (FALSE);

			/* Hack -- warriors only carry weapons >= 7 lbs */
			if ((r_ptr->flags2 & (RF2_ARMOR)) && (k_ptr->weight < 70)) return (FALSE);

			if (r_ptr->flags7 & (RF7_DROP_WEAPON)) return (TRUE);
			return (FALSE);
		}

		/* Bows/Ammo */
		case TV_BOW:
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			if (r_ptr->flags7 & (RF7_DROP_MISSILE)) return (TRUE);
			return (FALSE);
		}

		/* Books/Scrolls */
		case TV_MAGIC_BOOK:
		{
			/* Hack -- priests other than shamans do not carry magic books*/
			if ((r_ptr->flags2 & (RF2_PRIEST)) && !(r_ptr->flags2 & (RF2_MAGE))) return (FALSE);                            

			/* Mega hack -- priests and paladins other than shamans do not carry magic books */
			if ((r_ptr->d_char == 'p') && !(r_ptr->flags2 & (RF2_MAGE))) return (FALSE);                            

			if (r_ptr->flags7 & (RF7_DROP_WRITING)) return (TRUE);
			return (FALSE);
		}
		case TV_PRAYER_BOOK:
		{
			/* Hack -- mages other than shamans do not carry priest books*/
			if ((r_ptr->flags2 & (RF2_MAGE)) && !(r_ptr->flags2 & (RF2_PRIEST))) return (FALSE);                            

			/* Mega hack -- mages and rangers other than shamans do not carry priest books */
			if ((r_ptr->d_char == 'q') && !(r_ptr->flags2 & (RF2_PRIEST))) return (FALSE);                          

			if (r_ptr->flags7 & (RF7_DROP_WRITING)) return (TRUE);
			return (FALSE);
		}
		case TV_SCROLL:
		{
			if (r_ptr->d_char == '?') return (TRUE);

		}
		case TV_RUNESTONE:
		case TV_MAP:
		{
			if (r_ptr->flags7 & (RF7_DROP_WRITING)) return (TRUE);
			return (FALSE);
		}

		/* Rings/Amulets/Crowns */
		case TV_RING:
		{
			if (r_ptr->d_char == '=') return (TRUE);
		}
		case TV_AMULET:
		case TV_CROWN:
		{
			if (r_ptr->flags7 & (RF7_DROP_JEWELRY)) return (TRUE);
			return (FALSE);
		}

		/* Potions */
		case TV_POTION:
		{
			if (r_ptr->d_char == '!') return (TRUE);
			if (r_ptr->flags7 & (RF7_DROP_POTION)) return (TRUE);
			return (FALSE);
		}

		/* Food */
		case TV_FOOD:
		{
			if (r_ptr->flags7 & (RF7_DROP_FOOD)) return (TRUE);
			return (FALSE);
		}

		/* Lite/Fuel */
		case TV_LITE:
		{
			if (r_ptr->flags2 & (RF2_HAS_LITE | RF2_NEED_LITE)) return (TRUE);
			if (r_ptr->flags7 & (RF7_DROP_LITE)) return (TRUE);
			return (FALSE);
		}

		case TV_HOLD:
		{
			if (r_ptr->flags7 & (RF7_DROP_CHEST)) return (TRUE);
			return (FALSE);
		}


		/* Bottles/Skeletons/Broken Pottery */
		case TV_BONE:
		case TV_JUNK:
		case TV_SKIN:
		{
			if (r_ptr->flags7 & (RF7_DROP_JUNK)) return (TRUE);
			return (FALSE);

		}

		/* Diggers/Spikes */
		case TV_DIGGING:
		case TV_SPIKE:
		case TV_FLASK:
		{
			if (r_ptr->flags7 & (RF7_DROP_TOOL)) return (TRUE);
			return (FALSE);

		}

		/* Instruments/Song Books */
		case TV_SONG_BOOK:
		case TV_INSTRUMENT:
		{
			if (r_ptr->flags7 & (RF7_DROP_MUSIC)) return (TRUE);
			return (FALSE);
		}

		/* Rod/staff/wand */
		case TV_ROD:
		case TV_STAFF:
		case TV_WAND:
		case TV_FIGURE:
		{
			if (r_ptr->flags7 & (RF7_DROP_RSW)) return (TRUE);
			return (FALSE);
		}


	}

	/* Assume not allowed */
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

	/* Chance of "special object" */
	prob = (good ? 10 : 1000);

	/* Base level for the object */
	base = (good ? (object_level + 10) : object_level);

	/* Hack -- mushrooms only drop themselves */
	if (food_type)
	{
		int k_idx;

		/* Hack -- handle no type */
		if (!(variant_mushrooms)) return (FALSE);

		if (food_type > 0)
		{
			k_idx = lookup_kind(TV_FOOD,food_type-1);

			/* Handle failure */
			if (!k_idx) return (FALSE);
		}
		else
		{
			/* Activate restriction */
			get_obj_num_hook = kind_is_shroom;

			/* Prepare allocation table */
			get_obj_num_prep();

			/* Pick a random object */
			k_idx = get_obj_num(base);

			/* Clear restriction */
			get_obj_num_hook = NULL;

			/* Prepare allocation table */
			get_obj_num_prep();
		}

		/* Handle failure */
		if (!k_idx) return (FALSE);

		/* Prepare the object */
		object_prep(j_ptr, k_idx);

		/* Auto-inscribe if necessary */
		if ((cheat_auto) || (object_aware_p(j_ptr))) j_ptr->note = k_info[k_idx].note;

	}

	/* Generate a special artifact, or a normal object */
	else if ((rand_int(prob) != 0) || !make_artifact_special(j_ptr))
	{
		int k_idx;

		/* Good objects */
		if ((good) || (race_drop_idx))
		{
			/* Activate restriction */
			if (race_drop_idx) get_obj_num_hook = kind_is_race;

			/* Activate restriction */
			else get_obj_num_hook = kind_is_good;

			/* Prepare allocation table */
			get_obj_num_prep();
		}

		/* Pick a random object */
		k_idx = get_obj_num(base);

		/* Good objects */
		if ((good) || (race_drop_idx))
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

		/* Auto-inscribe if necessary */
		if ((cheat_auto) || (object_aware_p(j_ptr))) j_ptr->note = k_info[k_idx].note;
	}

	/* Mark as dropped */
	name_drop(j_ptr);

	/* Modify weight */
	modify_weight(j_ptr,j_ptr->name3);

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
	    (k_info[j_ptr->k_idx].level > p_ptr->depth))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - p_ptr->depth);

		/* Cheat -- peek at items */
		if (cheat_peek) object_mention(j_ptr);
	}

	/* Success */
	return (TRUE);
}

/*
 * XXX XXX XXX Do not use these hard-coded values.
 */
#define OBJ_GOLD_LIST   480     /* First "gold" entry */
#define MAX_GOLD        18      /* Number of "gold" entries */

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
 * Make a body
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_body(object_type *j_ptr, int r_idx)
{
	int k_idx = 0;

	monster_race *r_ptr = &r_info[r_idx];

	/* Hack -- handle trees */
	if (r_ptr->d_char == '<')
	{
		k_idx = lookup_kind(TV_JUNK,SV_JUNK_STUMP);

		if (!k_idx) return (FALSE);

		object_prep(j_ptr,k_idx);
#if 0
		/* Inscribe it */
		j_ptr->note = r_info[r_idx].note;
#endif
		return (TRUE);
	}

	/* No body */
	if (!(r_ptr->flags7 & (RF7_HAS_CORPSE | RF7_HAS_SKELETON | RF7_HAS_SKULL | RF7_HAS_SPORE))) return (FALSE);

	/* Usually produce a corpse */
	if (r_ptr->flags3 & (RF3_HURT_ROCK)) k_idx = lookup_kind(TV_STATUE,1);
	else if (r_ptr->flags7 & (RF7_HAS_CORPSE)) k_idx = lookup_kind(TV_BODY,SV_BODY_CORPSE);
	else if (r_ptr->flags7 & (RF7_HAS_SKELETON)) k_idx = lookup_kind(TV_BONE,SV_BONE_SKELETON);
	else if (r_ptr->flags7 & (RF7_HAS_SKULL)) k_idx = lookup_kind(TV_BONE,SV_BONE_SKULL);
	else if (r_ptr->flags7 & (RF7_HAS_SPORE)) k_idx = lookup_kind(TV_EGG,SV_EGG_SPORE);

	if (!k_idx) return (FALSE);

	/* Prepare the object */
	object_prep(j_ptr, k_idx);
#if 0
	/* Inscribe it */
	j_ptr->note = r_info[r_idx].note;
#endif
	/* Re-inscribe it */
	if ((!j_ptr->note) && !(r_ptr->flags1 & (RF1_UNIQUE))) j_ptr->note = k_info[k_idx].note;

	/* Modify weight */
	modify_weight(j_ptr,r_idx);

	/* Success */
	return (TRUE);
}

/*
 * Make a head
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_head(object_type *j_ptr, int r_idx)
{
	int k_idx = 0;

	monster_race *r_ptr = &r_info[r_idx];

	/* No body */
	if (!(r_ptr->flags7 & (RF7_HAS_HEAD))) return (FALSE);

	/* Usually produce a corpse */
	k_idx = lookup_kind(TV_BODY,SV_BODY_HEAD);

	if (!k_idx) return (FALSE);

	/* Prepare the object */
	object_prep(j_ptr, k_idx);
#if 0
	/* Inscribe it */
	j_ptr->note = r_info[r_idx].note;
#endif
	/* Re-inscribe it */
	if ((!j_ptr->note) && !(r_ptr->flags1 & (RF1_UNIQUE))) j_ptr->note = k_info[k_idx].note;

	/* Modify weight */
	modify_weight(j_ptr,r_idx);

	/* Success */
	return (TRUE);
}


/*
 * Make a body part
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_part(object_type *j_ptr, int r_idx)
{
	int k_idx = 0;

	monster_race *r_ptr = &r_info[r_idx];

	/* Hack -- handle trees */
	if (r_ptr->d_char == '<')
	{
		k_idx = lookup_kind(TV_JUNK,SV_JUNK_BRANCH);

		if (!k_idx) return (FALSE);

		object_prep(j_ptr,k_idx);

		/* Re-inscribe it */
		if (!j_ptr->note) j_ptr->note = k_info[k_idx].note;

		return (TRUE);
	}

	/* Usually hack off a hand or nearest approximation */
	if (r_ptr->flags7 & (RF7_HAS_CLAW)) k_idx = lookup_kind(TV_BODY,SV_BODY_CLAW);
	else if (r_ptr->flags7 & (RF7_HAS_HAND)) k_idx = lookup_kind(TV_BODY,SV_BODY_HAND);
	else if (r_ptr->flags7 & (RF7_HAS_WING)) k_idx = lookup_kind(TV_BODY,SV_BODY_WING);
	else if (r_ptr->flags7 & (RF7_HAS_ARM)) k_idx = lookup_kind(TV_BODY,SV_BODY_ARM);
	else if (r_ptr->flags7 & (RF7_HAS_LEG)) k_idx = lookup_kind(TV_BODY,SV_BODY_LEG);

	/* Sometimes hack off something else */
	if ((rand_int(100)<30) &&(r_ptr->flags7 & (RF7_HAS_ARM))) k_idx = lookup_kind(TV_BODY,SV_BODY_ARM);
	if ((rand_int(100)<30) &&(r_ptr->flags7 & (RF7_HAS_LEG))) k_idx = lookup_kind(TV_BODY,SV_BODY_LEG);
	if ((rand_int(100)<30) &&(r_ptr->flags7 & (RF7_HAS_WING))) k_idx = lookup_kind(TV_BODY,SV_BODY_WING);
	if ((rand_int(100)<30) &&(r_ptr->flags7 & (RF7_HAS_TEETH))) k_idx = lookup_kind(TV_BONE,SV_BONE_TEETH);

	/* Have a part */
	if (!k_idx) return(FALSE);

	/* Prepare the object */
	object_prep(j_ptr, k_idx);

#if 0
	/* Inscribe it */
	j_ptr->note = r_info[r_idx].note;
#endif
	/* Re-inscribe it */
	if ((!j_ptr->note) && !(r_ptr->flags1 & (RF1_UNIQUE))) j_ptr->note = k_info[k_idx].note;

	/* Modify weight */
	modify_weight(j_ptr,r_idx);

	/* Success */
	return (TRUE);
}

/*
 * Make a body skin
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_skin(object_type *j_ptr, int m_idx)
{
	int k_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Hack -- handle trees */
	if (r_ptr->d_char == '<')
	{
		k_idx = lookup_kind(TV_JUNK,SV_JUNK_STICK);

		if (!k_idx) return (FALSE);

		object_prep(j_ptr,k_idx);

		/* Re-inscribe it */
		if (!j_ptr->note) j_ptr->note = k_info[k_idx].note;

		return (TRUE);
	}

	/* Usually produce a skin */
	if (r_ptr->flags7 & (RF7_HAS_SKIN)) k_idx = lookup_kind(TV_SKIN,SV_SKIN_SKIN);
	else if (r_ptr->flags7 & (RF7_HAS_FUR)) k_idx = lookup_kind(TV_SKIN,SV_SKIN_FUR);
	else if (r_ptr->flags7 & (RF7_HAS_FEATHER)) k_idx = lookup_kind(TV_SKIN,SV_SKIN_FEATHER);
	else if (r_ptr->flags7 & (RF7_HAS_SCALE)) k_idx = lookup_kind(TV_SKIN,SV_SKIN_SCALE);

	/* No object? */
	if (!k_idx) return (FALSE);

	/* Prepare the object */
	object_prep(j_ptr, k_idx);

#if 0
	/* Inscribe it */
	j_ptr->note = r_info[r_idx].note;
#endif
	/* Re-inscribe it */
	if ((!j_ptr->note) && !(r_ptr->flags1 & (RF1_UNIQUE))) j_ptr->note = k_info[k_idx].note;

	/* Modify weight */
	modify_weight(j_ptr,m_ptr->r_idx);

	/* Success */
	return (TRUE);
}



static int feat_tval;

/*
 * Hack -- determine if a template matches feats_tval.
 */
static bool kind_is_feat_tval(int k_idx)
{

	object_kind *k_ptr = &k_info[k_idx];

	if (k_ptr->tval != feat_tval) return (FALSE);

	return (TRUE);
}


/*
 * Either get a copy of an existing feat item, or create a new one.
 *
 * If a new feat item is created, place on the floor in the specified location.
 */
bool make_feat(object_type *j_ptr, int y, int x)
{
	feature_type *f_ptr;

	int k_idx;
	int item;

	/* Sanity */
	if (!in_bounds(y, x)) return (0);

	/* Get the feat */	
	f_ptr = &f_info[cave_feat[y][x]];

	/* Get the item */
	k_idx = f_ptr->k_idx;

	/* Get existing item */
	item = scan_feat(y,x);

	/* Are we done */
	if (item >= 0)
	{
		object_copy(j_ptr,&o_list[item]);

		return (TRUE);
	}

	/* Hack -- Pick random flavor, if flavored */
	if (f_ptr->flags3 & (FF3_FLAVOR))
	{
		/* Set restriction */
		feat_tval = k_info[k_idx].tval;

		/* Activate restriction */
		get_obj_num_hook = kind_is_feat_tval;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Pick a random object */
		k_idx = get_obj_num(object_level);

		/* Clear restriction */
		get_obj_num_hook = NULL;

		/* Prepare allocation table */
		get_obj_num_prep();

		/* Failed? */
		if (!k_idx) k_idx = f_ptr->k_idx;
	}

	/* Prepare the object */
	object_prep(j_ptr, k_idx);

	/* This is a 'store' item */
	j_ptr->ident |= (IDENT_STORE);

	/* Hack -- only apply magic to boring objects */
	a_m_aux_4(j_ptr, object_level, 0);

	/* Auto-inscribe if necessary */
	if ((cheat_auto) || (object_aware_p(j_ptr))) j_ptr->note = k_info[k_idx].note;

	/* Add to the floor */
	if (floor_carry(y,x,j_ptr)) return (TRUE);

	/* Failed */
	return(FALSE);
}


/*
 * Let the floor carry an object
 */
s16b floor_carry(int y, int x, object_type *j_ptr)
{
	int n = 0;

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

		/* Count objects */
		n++;
	}

	/* The stack is already too large */
	if (n > MAX_FLOOR_STACK) return (0);

	/* Option -- disallow stacking */
	if (adult_no_stacking && n) return (0);

	/* Make an object */
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[o_idx];

		/* Structure Copy */
		object_copy(o_ptr, j_ptr);

		/* Location */
		o_ptr->iy = y;
		o_ptr->ix = x;

		/* Forget monster */
		o_ptr->held_m_idx = 0;

		/* Link the object to the pile */
		o_ptr->next_o_idx = cave_o_idx[y][x];

		/* Link the floor to the object */
		cave_o_idx[y][x] = o_idx;

		/* Notice */
		note_spot(y, x);

		/* Redraw */
		lite_spot(y, x);
	}

	/* Result */
	return (o_idx);
}


/*
 * Let a monster fall to the ground near a location.
 */
void race_near(int r_idx, int y1, int x1)
{
	int i, x, y;

	/* Try up to 18 times */
	for (i = 0; i < 18; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require an "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Require monster can survive on terrain */
		if (!place_monster_here(y, x, r_idx)) continue;

		/* Create a new monster (awake, no groups) */
		(void)place_monster_aux(y, x, r_idx, FALSE, FALSE);

		/* Hack -- monster does not drop anything */
		m_list[cave_m_idx[y][x]].mflag |= (MFLAG_MADE);

		/* Done */
		break;
	}
}

/*
 * Let an object fall to the ground at or near a location.
 *
 * The initial location is assumed to be "in_bounds_fully()".
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
void drop_near(object_type *j_ptr, int chance, int y, int x)
{
	int i, k, d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;

	s16b this_o_idx, next_o_idx = 0;

	char o_name[80];

	bool flag = FALSE;

	bool plural = FALSE;

	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Describe object */
	object_desc(o_name, sizeof(o_name), j_ptr, FALSE, 0);   

	/* Handle normal "breakage" */
	if (!artifact_p(j_ptr) && (rand_int(100) < chance))
	{
		/* Containers/figurines release contents */
		if (((j_ptr->tval == TV_FIGURE) || (j_ptr->tval == TV_HOLD))
			&& (j_ptr->name3 > 0))
		{
			while (j_ptr->number)
			{
				race_near(j_ptr->name3, y, x);

				j_ptr->number--;
			}
		}

		/* Message */
		msg_format("The %s disappear%s.",o_name, (plural ? "" : "s"));

		/* Debug */
		if (p_ptr->wizard) msg_print("Breakage (breakage).");

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
			if (!in_bounds_fully(ty, tx)) continue;

			/* Require line of sight */
			if (!los(y, x, ty, tx)) continue;

			/* Require drop space */
			if (!(f_info[cave_feat[ty][tx]].flags1 & (FF1_DROP))) continue;

			/* Don't like hiding items space */
			if ((f_info[cave_feat[ty][tx]].flags2 & (FF2_HIDE_ITEM)) && (rand_int(100)<80)) continue;

			/* No objects */
			k = 0;

			/* Scan objects in that grid */
			for (this_o_idx = cave_o_idx[ty][tx]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Count objects */
				k++;

				/* Check for possible combination */
				if (object_similar(o_ptr, j_ptr)) comb = TRUE;

			}

			/* Add new object */
			if (!comb) k++;

			/* Option -- disallow stacking */
			if (adult_no_stacking && (k > 1)) continue;
			
			/* Paranoia */
			if (k > MAX_FLOOR_STACK) continue;

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
		if (p_ptr->wizard) msg_print("Breakage (no floor space).");

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
		}

		/* Random locations */
		else
		{
			ty = rand_int(DUNGEON_HGT);
			tx = rand_int(DUNGEON_WID);
		}

		/* Bounce to that location */
		by = ty;
		bx = tx;

		/* Require floor space */
		if (!cave_clean_bold(by, bx)) continue;

		/* Okay */
		flag = TRUE;
	}


	/* Give it to the floor */
	if (!floor_carry(by, bx, j_ptr))
	{
		/* Message */
		msg_format("The %s disappear%s.",
			   o_name, (plural ? "" : "s"));

		/* Debug */
		if (p_ptr->wizard) msg_print("Breakage (too many objects).");

		/* Hack -- Preserve artifacts */
		a_info[j_ptr->name1].cur_num = 0;

		/* Failure */
		return;
	}


	/* Warn if we lose the item from view */
	if (f_info[cave_feat[by][bx]].flags2 & (FF2_HIDE_ITEM))
	{
		/* Message */
		msg_format("The %s disappear%s from view.",
			   o_name, (plural ? "" : "s"));
	}


	/* Sound */
	sound(MSG_DROP);

	/* Mega-Hack -- no message if "dropped" by player */
	/* Message when an object falls under the player */
	if (chance && (cave_m_idx[by][bx] < 0))
	{
		msg_print("You feel something roll beneath your feet.");

		/* Recalculate runes */
		p_ptr->update |= (PU_RUNES);
	}
}


/*
 * Let an feature appear near a location.
 *
 * The initial location is assumed to be "in_bounds_fully()".
 *
 */
void feat_near(int feat, int y, int x)
{
	int d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;


	bool flag = FALSE;

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
			/* Calculate actual distance */
			d = (dy * dy) + (dx * dx);

			/* Ignore distant grids */
			if (d > 10) continue;

			/* Location */
			ty = y + dy;
			tx = x + dx;

			/* Skip illegal grids */
			if (!in_bounds_fully(ty, tx)) continue;

			/* Require line of sight */
			if (!los(y, x, ty, tx)) continue;

			/* Prevent overwriting permanets */
			if (f_info[cave_feat[ty][tx]].flags1 & (FF1_PERMANENT)) continue;

			/* Don't like non-floor space */
			if (!(f_info[cave_feat[ty][tx]].flags1 & (FF1_FLOOR))) continue;

			/* Don't like objects */
			if (cave_o_idx[ty][tx]) continue;

			/* Calculate score */
			s = 1000 - (d - cave_feat[ty][tx]);

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

	/* Give it to the floor */
	if (flag) cave_set_feat(by, bx, feat);
}

/*
 * Scatter some "great" objects near the player
 */
void acquirement(int y1, int x1, int num, bool great)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Acquirement */
	while (num--)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make a good (or great) object (if possible) */
		if (!make_object(i_ptr, TRUE, great)) continue;

		/* Drop the object */
		drop_near(i_ptr, -1, y1, x1);
	}
}


/*
 * Attempt to place an object (normal or good/great) at the given location.
 */
void place_object(int y, int x, bool good, bool great)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Hack -- clean floor space */
	if (!cave_clean_bold(y, x)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Make an object (if possible) */
	if (make_object(i_ptr, good, great))
	{
		/* Give it to the floor */
		if (!floor_carry(y, x, i_ptr))
		{
			/* Hack -- Preserve artifacts */
			a_info[i_ptr->name1].cur_num = 0;
		}
	}
}


/*
 * Places a treasure (Gold or Gems) at given location
 */
void place_gold(int y, int x)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require clean floor space */
	if (!cave_clean_bold(y, x)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Make some gold */
	if (make_gold(i_ptr))
	{
		/* Give it to the floor */
		(void)floor_carry(y, x, i_ptr);
	}
}


/*
 * Apply a "feature restriction function" to the "feature allocation table"
 */
errr get_feat_num_prep(void)
{
	int i;

	/* Get the entry */
	alloc_entry *table = alloc_feat_table;

	/* Scan the allocation table */
	for (i = 0; i < alloc_feat_size; i++)
	{
		/* Accept objects which pass the restriction, if any */
		if (!get_feat_num_hook || (*get_feat_num_hook)(table[i].index))
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
 * Choose an feature type that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "feature allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" feature, in
 * a relatively efficient manner.
 *
 * It is (slightly) more likely to acquire a feature of the given level
 * than one of a lower level.  This is done by choosing several features
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no features are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 * Happening all the time for trapped doors.
 */
s16b get_feat_num(int level)
{
	int i, j, p;

	int f_idx;

	long value, total;

	feature_type *f_ptr;

	alloc_entry *table = alloc_feat_table;


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
	for (i = 0; i < alloc_feat_size; i++)
	{

		/* Default */
		table[i].prob3 = 0;

		/* Features are not sorted by depth */
		if (table[i].level > level) break;

		/* Get the index */
		f_idx = table[i].index;

		/* Get the actual feature */
		f_ptr = &f_info[f_idx];

		/* Hack -- no upstairs on surface */
		if ((f_ptr->flags1 & (FF1_LESS)) && (p_ptr->depth == min_depth(p_ptr->dungeon))) continue;

		/* Hack -- no chasm/trap doors/down stairs on quest levels */
		if ((f_ptr->flags1 & (FF1_MORE)) && is_quest(p_ptr->depth)) continue;

		/* Hack -- no chasm/trap doors/down stairs at the bottom of a tower dungeon */
		if ((t_info[p_ptr->dungeon].zone[0].tower) && (p_ptr->depth == 1)) continue;

		/* Hack -- no chasm/trap doors/down stairs on the deepest level */
		else if ((f_ptr->flags1 & (FF1_MORE)) && (p_ptr->depth == max_depth(p_ptr->dungeon))) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Total */
		total += table[i].prob3;
	}

	/* No legal features */
	if (total <= 0) return (0);


	/* Pick a feature */
	value = rand_int(total);

	/* Find the feature */
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
 * Helper function for "features"
 */
static bool vault_feat_alloc(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Require random allocation */
	if (!(f_ptr->flags3 & (FF3_ALLOC))) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Places a random chest at the given location.
 *
 * The location must be a legal, naked, floor grid.
 */
void place_feature(int y, int x)
{
	int feat;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;

	/*Set the hook */
	get_feat_num_hook = vault_feat_alloc;

	get_feat_num_prep();

	/* Click! */
	feat = get_feat_num(object_level);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Activate the trap */
	cave_set_feat(y, x, feat);
}





/*
 * Helper function for "floor traps"
 */
static bool vault_trap_floor(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Require trap */
	if (!(f_ptr->flags1 & (FF1_TRAP))) return (FALSE);

	/* Decline door traps */
	if (f_ptr->flags1 & (FF1_DOOR)) return (FALSE);

	/* Decline chest traps */
	if (f_ptr->flags3 & (FF3_CHEST)) return (FALSE);

	/* Decline traps we have to pick */
	if (f_ptr->flags3 & (FF3_PICK_TRAP)) return (FALSE);

	/* Decline allocated */
	if (f_ptr->flags3 & (FF3_ALLOC)) return (FALSE);

	/* Decline high traps */
	if ((!variant_new_feats) && (f_idx > 63)) return (FALSE);

	/* Okay */
	return (TRUE);
}



/* Hack -- make sure we drop the same items */
static int chest_drops;

static bool chest_drop_good;
static bool chest_drop_great;
static bool chest_has_item;
static bool chest_has_gold;

/*
 * Helper function for "chest traps"
 */
static bool vault_trap_chest(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	int test_drops =  ((f_ptr->flags3 & (FF3_DROP_2D2)) ? 4 : 0) + ((f_ptr->flags3 & (FF3_DROP_1D2)) ? 2 : 0);
	bool test_drop_good = (f_ptr->flags3 & (FF3_DROP_GOOD)) ? TRUE : FALSE;
	bool test_drop_great = (f_ptr->flags3 & (FF3_DROP_GREAT)) ? TRUE : FALSE;
	bool test_has_item = (f_ptr->flags1 & (FF1_HAS_ITEM)) ? TRUE : FALSE;
	bool test_has_gold = (f_ptr->flags1 & (FF1_HAS_GOLD)) ? TRUE : FALSE;

	/* Decline non-chests */
	if (!(f_ptr->flags3 & (FF3_CHEST))) return (FALSE);

	/* Decline non-traps */
	if (!(f_ptr->flags1 & (FF1_TRAP))) return (FALSE);

	/* Decline allocated */
	if (f_ptr->flags3 & (FF3_ALLOC)) return (FALSE);

	/* Must match chest drops */
	if (test_drop_good != chest_drop_good) return (FALSE);
	if (test_drop_great != chest_drop_great) return (FALSE);
	if (test_has_item != chest_has_item) return (FALSE);
	if (test_has_gold != chest_has_gold) return (FALSE);
	if (test_drops != chest_drops) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for "chests"
 */
static bool vault_chest(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Require alloc flag */
	if (!(f_ptr->flags3 & (FF3_ALLOC))) return (FALSE);

	/* Require chest flag */
	if (!(f_ptr->flags3 & (FF3_CHEST))) return (FALSE);

	/* Not okay */
	return (TRUE);
}




/* Hack -- global */
static char pick_attr;

/*
 * Helper function for "matched trap"
 */
static bool vault_trap_attr(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-traps */
	if (!(f_ptr->flags1 & (FF1_TRAP))) return (FALSE);

	/* Decline door traps */
	if (f_ptr->flags1 & (FF1_DOOR)) return (FALSE);

	/* Decline chest traps */
	if (f_ptr->flags3 & (FF3_CHEST)) return (FALSE);

	/* Decline traps we have to pick */
	if (f_ptr->flags3 & (FF3_PICK_TRAP)) return (FALSE);

	/* Restrict traps to same color */
	if (f_ptr->d_attr != pick_attr) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- instantiate a trap
 *
 * Have modified this routine to use the modified feature selection
 * code above.
 */
void pick_trap(int y, int x)
{
	int feat= cave_feat[y][x];

	/* Paranoia */
	if (!(f_info[feat].flags1 & (FF1_TRAP))) return;

	/* Floor trap */
	if (f_info[feat].flags3 & (FF3_ALLOC))
	{
		/* Set hook */
		if (f_info[feat].flags3 & (FF3_CHEST))
		{
			feature_type *f_ptr = &f_info[feat];

			chest_drops =  ((f_ptr->flags3 & (FF3_DROP_2D2)) ? 4 : 0) + ((f_ptr->flags3 & (FF3_DROP_1D2)) ? 2 : 0);
			chest_drop_good = (f_ptr->flags3 & (FF3_DROP_GOOD)) ? TRUE : FALSE;
			chest_drop_great = (f_ptr->flags3 & (FF3_DROP_GREAT)) ? TRUE : FALSE;
			chest_has_item = (f_ptr->flags1 & (FF1_HAS_ITEM)) ? TRUE : FALSE;
			chest_has_gold = (f_ptr->flags1 & (FF1_HAS_GOLD)) ? TRUE : FALSE;

			get_feat_num_hook = vault_trap_chest;
		}
		else if (cave_o_idx[y][x])
		{
			switch (o_list[cave_o_idx[y][x]].tval)
			{
				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				case TV_BOW:
					pick_attr = TERM_L_RED;		/* Murder hole */
					break;

				case TV_HAFTED:
				case TV_SWORD:
				case TV_POLEARM:
					pick_attr = TERM_RED;		/* Spring-loaded trap */
					break;

				case TV_WAND:
					pick_attr = TERM_YELLOW;	/* Tripwire */
					break;

				case TV_STAFF:
				case TV_ROD:
					pick_attr = TERM_L_BLUE;	/* Magic symbol */
					break;

				case TV_POTION:
					pick_attr = TERM_BLUE;		/* Explosive device */
					break;

				case TV_SCROLL:
					pick_attr = TERM_L_GREEN;	/* Ancient hex */
					break;

				case TV_FLASK:
					pick_attr = TERM_UMBER;		/* Discoloured spot */
					break;

				case TV_DRAG_ARMOR:
					pick_attr = TERM_L_WHITE;	/* Stone face */
					break;

				case TV_FOOD:
					if (o_list[cave_o_idx[y][x]].sval < SV_FOOD_MIN_FOOD) pick_attr = TERM_GREEN;		/* Gas trap */
					else pick_attr = TERM_VIOLET;
					break;

				case TV_RUNESTONE:
					pick_attr = TERM_ORANGE;	/* Strange rune */
					break;

 				default:
					pick_attr = TERM_VIOLET;	/* Loose rock */
					break;
			}

			/* Set hook*/
			get_feat_num_hook = vault_trap_attr;
			
		}
		else get_feat_num_hook = vault_trap_floor;
	}
	else
	{
		/* Set attribute */
		pick_attr = f_info[cave_feat[y][x]].d_attr;

		/* Set hook*/
		get_feat_num_hook = vault_trap_attr;
	}

	get_feat_num_prep();

	/* Hack --- force dungeon traps in town */
	if (!p_ptr->depth) object_level = 3;

	/* Click! */
	feat = get_feat_num(object_level);

	/* Clear hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* Hack --- force dungeon traps in town */
	if (!p_ptr->depth) object_level = 0;

	/* More paranoia */
	if (!feat) return;

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
	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;

	/* Place an invisible trap */
	cave_set_feat(y, x, FEAT_INVIS);
}


/*
 * 'Makes' a chest
 */
bool make_chest(int *feat)
{
	/*Set the hook */
	get_feat_num_hook = vault_chest;

	get_feat_num_prep();

	/* Click! */
	*feat = get_feat_num(object_level);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return (FALSE);

	return(TRUE);
}

/*
 * Places a random chest at the given location.
 *
 * The location must be a legal, naked, floor grid.
 */
void place_chest(int y, int x)
{
	int feat;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;

	/*Set the hook */
	get_feat_num_hook = vault_chest;

	get_feat_num_prep();

	/* Click! */
	feat = get_feat_num(object_level);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();


	/* More paranoia */
	if (!feat) return;

	/* Activate the trap */
	cave_set_feat(y, x, feat);
}


/*
 * Pick a door
 */
void pick_door(int y, int x)
{
	int feat = cave_feat[y][x];

	if (feat == FEAT_DOOR_INVIS) place_trapped_door(y,x);
	else if (feat == FEAT_SECRET) place_secret_door(y,x);
	else place_jammed_door(y,x);
}


/*
 * Place a secret door at the given location
 */
void place_secret_door(int y, int x)
{
	/* Create secret door */
	cave_set_feat(y, x, FEAT_SECRET);
}

/*
 * Helper function for "closed doors"
 */
static bool vault_closed_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->flags1 & (FF1_DOOR))) return (FALSE);

	/* Decline known trapped doors */
	if ((f_ptr->flags1 & (FF1_TRAP)) && !(f_ptr->flags1 & (FF1_SECRET))) return (FALSE);

	/* Decline open/secret/known jammed doors/broken */
	if (!(f_ptr->flags1 & (FF1_OPEN))) return (FALSE);

	/* Decline unknown trapped doors */
	if ((!variant_new_feats) && (f_ptr->flags1 & (FF1_TRAP))) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Place a random type of closed door at the given location.
 */
void place_closed_door(int y, int x)
{
	int feat;

	if (randint(400) < 300)
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD);

		return;
	}
	else
	{
		/*Set the hook */
		get_feat_num_hook = vault_closed_door;

		get_feat_num_prep();

		/* Click! */
		feat = get_feat_num(object_level);

		/* Clear the hook */
		get_feat_num_hook = NULL;

		get_feat_num_prep();

		/* More paranoia */
		if (!feat) return;

		/* Activate the trap */
		cave_set_feat(y, x, feat);

	}

}

/*
 * Helper function for "closed doors"
 */
static bool vault_locked_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->flags1 & (FF1_DOOR))) return (FALSE);

	/* Decline known trapped doors */
	if ((f_ptr->flags1 & (FF1_TRAP)) && !(f_ptr->flags1 & (FF1_SECRET))) return (FALSE);

	/* Decline open/secret/known jammed doors/broken */
	if (!(f_ptr->flags1 & (FF1_OPEN))) return (FALSE);

	/* Decline unlocked doors */
	if (f_idx == FEAT_DOOR_HEAD) return (FALSE);

	/* Decline unknown trapped doors */
	if ((!variant_new_feats) && (f_ptr->flags1 & (FF1_TRAP))) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Place a random type of trapped/locked door at the given location
 */
void place_locked_door(int y, int x)
{
	int feat;

	/*Set the hook */
	get_feat_num_hook = vault_locked_door;

	get_feat_num_prep();

	/* Click! */
	feat = get_feat_num(object_level);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Activate the trap */
	cave_set_feat(y, x, feat);
}

/*
 * Helper function for "closed doors"
 */
static bool vault_jammed_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->flags1 & (FF1_DOOR))) return (FALSE);

	/* Decline trapped doors */
	if (f_ptr->flags1 & (FF1_TRAP)) return (FALSE);

	/* Decline unjammed doors */
	if (f_ptr->flags1 & (FF1_OPEN)) return (FALSE);

	/* Decline secret/unknown jammed doors */
	if (f_ptr->flags1 & (FF1_SECRET)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Place a random type of jammed door at the given location.
 */
void place_jammed_door(int y, int x)
{
	int feat;

	/*Set the hook */
	get_feat_num_hook = vault_jammed_door;

	get_feat_num_prep();

	/* Click! */
	feat = get_feat_num(object_level);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Activate the trap */
	cave_set_feat(y, x, feat);
}

/*
 * Helper function for "door traps"
 */
static bool vault_trapped_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-doors */
	if (!(f_ptr->flags1 & (FF1_DOOR))) return (FALSE);

	/* Decline non-traps */
	if (!(f_ptr->flags1 & (FF1_TRAP))) return (FALSE);

	/* Decline pick doors */
	if (f_ptr->flags3 & (FF3_PICK_DOOR)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Place a random type of trapped door at the given location
 */
void place_trapped_door(int y, int x)
{
	int feat;

	/* Set the hook */
	get_feat_num_hook = vault_trapped_door;

	get_feat_num_prep();

	/* Click! */
	feat = get_feat_num(object_level);

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Activate the trap */
	cave_set_feat(y, x, feat);
}

/*
 * Place a random type of door at the given location.
 */
void place_random_door(int y, int x)
{
	int tmp;

	/* Choose an object */
	tmp = rand_int(1000);

	/* Open doors (300/1000) */
	if (tmp < 300)
	{
		/* Create open door */
		cave_set_feat(y, x, FEAT_OPEN);
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		cave_set_feat(y, x, FEAT_BROKEN);
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		cave_set_feat(y, x, FEAT_SECRET);
	}

	/* Closed, locked, or stuck doors (400/1000) */
	else
	{
		/* Create closed door */
		place_closed_door(y, x);
	}
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
	object_type *o_ptr = &inventory[item];

	char o_name[80];

	/* Get a description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Print a message */
	msg_format("You have %s (%c).", o_name, index_to_label(item));
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

	/* Forget about item */
	if (!num) inven_drop_flags(o_ptr);

	/* Change the number and weight */
	if (num)
	{
		/* Add the number */
		o_ptr->number += num;

		/* Check stack count overflow */
		if (o_ptr->stackc >= o_ptr->number)
		{
			/* Reset stack counter */
			o_ptr->stackc = 0;

			/* Decrease pval */
			if (o_ptr->pval) o_ptr->pval--;

			/* Reset timeout */
			if (o_ptr->timeout) o_ptr->timeout = 0;
		}

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
			COPY(&inventory[i], &inventory[i+1], object_type);
		}

		/* Hack -- wipe hole */
		(void)WIPE(&inventory[i], object_type);

		/* Recalculate runes */
		p_ptr->update |= (PU_RUNES);

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
		p_ptr->update |= (PU_MANA | PU_RUNES);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
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
	object_type *o_ptr = &o_list[item];

	char o_name[80];

	/* Hack -- haven't seen item on floor */
	if (!(o_ptr->marked)) return;

	/* Get a description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

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

	/* Check stack count overflow */
	if (o_ptr->stackc >= o_ptr->number)
	{
		/* Reset stack counter */
		o_ptr->stackc = 0;

		/* Decrease pval */
		if (o_ptr->pval) o_ptr->pval--;

		/* Reset timeout */
		if (o_ptr->timeout) o_ptr->timeout = 0;
	}

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
bool inven_carry_okay(const object_type *o_ptr)
{
	int j;

	/* Empty slot? */
	if (p_ptr->inven_cnt < INVEN_PACK) return (TRUE);

	/* Similar slot? */
	for (j = 0; j < INVEN_PACK; j++)
	{
		object_type *j_ptr = &inventory[j];

		/* Skip non-objects */
		if (!j_ptr->k_idx) continue;

		/* Check if the two items can be combined */
		if (object_similar(j_ptr, o_ptr)) return (TRUE);
	}

	/* Hack -- check belt slot */
	if ((variant_belt_slot)
	     && (inventory[INVEN_BELT].k_idx)
		&& (object_similar(&inventory[INVEN_BELT],o_ptr))) return (TRUE);

	if ((variant_belt_slot)
	     && (inventory[INVEN_WIELD].k_idx)
		&& (object_similar(&inventory[INVEN_WIELD],o_ptr))
		&& (inventory[INVEN_WIELD].number > 1)) return (TRUE);


	/* Nope */
	return (FALSE);
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
s16b inven_carry(object_type *o_ptr)
{
	int i, j, k;
	int n = -1;

	object_type *j_ptr;

	int book_tval= c_info[p_ptr->pclass].spell_book;

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

	/* Hack -- check for combining - belt */
	if ((variant_belt_slot)
	     && (inventory[INVEN_BELT].k_idx)
		&& (object_similar(&inventory[INVEN_BELT],o_ptr)))
	{

			/* Combine the items */
			object_absorb(&inventory[INVEN_BELT], o_ptr);

			/* Increase the weight */
			p_ptr->total_weight += (o_ptr->number * o_ptr->weight);

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);

			/* Success */
			return (INVEN_BELT);
	}

	/* Hack -- check for combining - wielded */
	/* MegaHack -- only combine when wielding more than 1 */
	if ((variant_belt_slot)
	     && (inventory[INVEN_WIELD].k_idx)
		&& (object_similar(&inventory[INVEN_WIELD],o_ptr))
		&& (inventory[INVEN_WIELD].number > 1))
	{

			/* Combine the items */
			object_absorb(&inventory[INVEN_WIELD], o_ptr);

			/* Increase the weight */
			p_ptr->total_weight += (o_ptr->number * o_ptr->weight);

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);

			/* Success */
			return (INVEN_WIELD);
	}


	/* Paranoia */
	if (p_ptr->inven_cnt > INVEN_PACK) return (-1);


	/* Find an empty slot */
	for (j = 0; j <= INVEN_PACK; j++)
	{
		j_ptr = &inventory[j];

		/* Use it if found */
		if (!j_ptr->k_idx) break;
	}

	/* Use that slot */
	i = j;


	/* Reorder the pack */
	if (i < INVEN_PACK)
	{
		s32b o_value, j_value;

		/* Get the "value" of the item */
		o_value = object_value(o_ptr);

		/* Scan every occupied slot */
		for (j = 0; j < INVEN_PACK; j++)
		{
			j_ptr = &inventory[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;

			/* Hack -- readable books always come first */
			if ((o_ptr->tval == book_tval) &&
			    (j_ptr->tval != book_tval)) break;
			if ((j_ptr->tval == book_tval) &&
			    (o_ptr->tval != book_tval)) continue;

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

			/* Rods sort by increasing recharge time */
			if (o_ptr->tval == TV_ROD)
			{
				if (o_ptr->pval < j_ptr->timeout) break;
				if (o_ptr->pval > j_ptr->timeout) continue;
			}

			/* Wands/Staffs sort by decreasing charges */
			if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
			{
				if (o_ptr->pval > j_ptr->pval) break;
				if (o_ptr->pval < j_ptr->pval) continue;
			}

			/* Lites sort by decreasing fuel */
			if (o_ptr->tval == TV_LITE)
			{
				if (o_ptr->pval > j_ptr->pval) break;
				if (o_ptr->pval < j_ptr->pval) continue;
			}

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


	/* Copy the item */
	object_copy(&inventory[i], o_ptr);

	/* Get the new object */
	j_ptr = &inventory[i];

	/* Forget stack */
	j_ptr->next_o_idx = 0;

	/* Forget monster */
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
	p_ptr->update |= (PU_BONUS | PU_RUNES);

	/* Combine and Reorder pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

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
 *
 * Destroys spells if taken off, rather than placing in inventory.
 *
 */
s16b inven_takeoff(int item, int amt)
{
	int slot;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr act;

	char o_name[80];


	/* Get the item to take off */
	o_ptr = &inventory[item];

	/* Paranoia */
	if (amt <= 0) return (-1);

	/* Verify */
	if (amt > o_ptr->number) amt = o_ptr->number;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain a local object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Reset stack count */
	i_ptr->stackc = 0;

	/* Sometimes reverse the stack object */
	if (!object_known_p(o_ptr) && (rand_int(o_ptr->number)< o_ptr->stackc))
	{
		if (amt >= o_ptr->stackc)
		{
			i_ptr->stackc = o_ptr->stackc;

			o_ptr->stackc = 0;
		}
		else
		{
			if (i_ptr->pval) i_ptr->pval--;
			if (i_ptr->timeout) i_ptr->timeout = 0;

			o_ptr->stackc -= amt;
		}
	}
	/* Get stack count */
	else if (amt >= (o_ptr->number - o_ptr->stackc))
	{
		/* Set stack count */
		i_ptr->stackc = amt - (o_ptr->number - o_ptr->stackc);
	}


	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Destroy spell */
	if (i_ptr->tval == TV_SPELL)
	{
		act = "You were enchanted with";
	}

	/* Where is the item now */
	else if (((item == INVEN_WIELD) && (i_ptr->number > 1)) || (item == INVEN_BELT))
	{
		act = "You were carrying";
	}
	else if ((i_ptr->tval == TV_SWORD) || (i_ptr->tval == TV_POLEARM)
			|| (i_ptr->tval == TV_HAFTED) || (i_ptr->tval== TV_STAFF))
	{
		act = "You were wielding";
		if (item == INVEN_ARM) act = "You were wielding off-handed";
	}
	else if (item == INVEN_WIELD)
	{
		act = "You were using";
	}

	/* Took off weapon */
	else if (item == INVEN_WIELD)
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

	/* Hack -- clear may flags to avoid forgetting them */
	drop_may_flags(o_ptr);

	/* Modify, Optimize */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

	/* Carry the object - spells are destroyed */
	if (i_ptr->tval != TV_SPELL) slot = inven_carry(i_ptr);
	else slot = -1;

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
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[80];


	/* Get the original object */
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

		/* Check if destroyed by removal */
		if (item == -1) return;

		/* Get the original object */
		o_ptr = &inventory[item];
	}

	
	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Reset stack count */
	i_ptr->stackc = 0;

	/* Sometimes reverse the stack object */
	if (!object_known_p(o_ptr) && (rand_int(o_ptr->number)< o_ptr->stackc))
	{
		if (amt >= o_ptr->stackc)
		{
			i_ptr->stackc = o_ptr->stackc;

			o_ptr->stackc = 0;
		}
		else
		{
			if (i_ptr->pval) i_ptr->pval--;
			if (i_ptr->timeout) i_ptr->timeout = 0;

			o_ptr->stackc -= amt;
		}
	}

	/* Get stack count */
	else if (amt >= (o_ptr->number - o_ptr->stackc))
	{
		/* Set stack count */
		i_ptr->stackc = amt - (o_ptr->number - o_ptr->stackc);
	}

	/* Forget information on dropped object */
	drop_may_flags(i_ptr);

	/* Forget guessed information */
	if (o_ptr->number == amt) inven_drop_flags(o_ptr);

	/* Describe local object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Message */
	msg_format("You drop %s (%c).", o_name, index_to_label(item));

	/* Drop it near the player */
	drop_near(i_ptr, 0, py, px);

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
					COPY(&inventory[k], &inventory[k+1], object_type);
				}

				/* Hack -- wipe hole */
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
 */
void reorder_pack(void)
{
	int i, j, k;

	s32b o_value;
	s32b j_value;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool flag = FALSE;

	int book_tval= c_info[p_ptr->pclass].spell_book;

	/* Re-order the pack (forwards) */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Mega-Hack -- allow "proper" over-flow */
		if ((i == INVEN_PACK) && (p_ptr->inven_cnt == INVEN_PACK)) break;

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
			if ((o_ptr->tval == book_tval) &&
			    (j_ptr->tval != book_tval)) break;
			if ((j_ptr->tval == book_tval) &&
			    (o_ptr->tval != book_tval)) continue;

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

			/* Rods sort by increasing recharge time */
			if (o_ptr->tval == TV_ROD)
			{
				if (o_ptr->timeout < j_ptr->timeout) break;
				if (o_ptr->timeout > j_ptr->timeout) continue;
			}

			/* Wands/Staffs sort by decreasing charges */
			if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
			{
				if (o_ptr->pval > j_ptr->pval) break;
				if (o_ptr->pval < j_ptr->pval) continue;
			}

			/* Lites sort by decreasing fuel */
			if (o_ptr->tval == TV_LITE)
			{
				if (o_ptr->pval > j_ptr->pval) break;
				if (o_ptr->pval < j_ptr->pval) continue;
			}

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

		/* Get local object */
		i_ptr = &object_type_body;

		/* Save a copy of the moving item */
		object_copy(i_ptr, &inventory[i]);

		/* Slide the objects */
		for (k = i; k > j; k--)
		{
			/* Slide the item */
			object_copy(&inventory[k], &inventory[k-1]);
		}

		/* Insert the moving item */
		object_copy(&inventory[j], i_ptr);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* Message */
	if (flag) msg_print("You reorder some items in your pack.");
}



/*
 * Fills a book with spells (in order). Note hack for runestones
 * in order to fit them all in is to use book as a hashtable.
 */
void fill_book(const object_type *o_ptr, s16b *book, int *num)
{
	int i,ii;

	spell_type *s_ptr;

	/* No spells */
	*num = 0;

	/* Fill book with nothing */
	for (i=0;i<26;i++) book[i]=0;

	/* Fill book with spells */
	for (i=0;i<z_info->s_max;i++)
	{
		s_ptr=&s_info[i];

		for (ii=0;ii<MAX_SPELL_APPEARS;ii++)
		{
			int tval = s_ptr->appears[ii].tval;
			int sval = s_ptr->appears[ii].sval;
			int slot = s_ptr->appears[ii].slot;

			if ((tval == o_ptr->tval) &&
				(sval == o_ptr->sval))
			{
				if (o_ptr->tval == TV_RUNESTONE)
				{
					if (p_ptr->cur_runes & (2 << (slot-1)))
					{
						/* Use book as hash table */
                                                slot = slot % 20;

						/* Free entry in book */
						if (book[slot] == 0)
						{
							book[slot] = i;
						}

						/* Collision -- minimise impact by going from end of table */
						else
						{
                                                        for (slot = 19; (slot >=0) && book[slot]; slot--) ;

							if ((slot >= 0) && (!book[slot])) book[slot] = i;
						}

						if ((*num) < slot) (*num) = slot;
					}
				}
				else
				{
					book[s_ptr->appears[ii].slot-1] = i;
					(*num)++;
				}
			}
		}
	}
}

/*
 * Returns level for a spell
 */
s16b spell_level(int spell)
{
	int i;

	s16b level;

	spell_type *s_ptr;

	spell_cast *sc_ptr = &(s_info[0].cast[0]);

	bool legible = FALSE;

	/* Paranoia -- must be literate */
	if (c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL) return (100);

	/* Get the spell */
	s_ptr = &s_info[spell];

	/* Get casting information */
	for (i=0;i<MAX_SPELL_CASTERS;i++)
	{
		if (s_ptr->cast[i].class == p_ptr->pclass)
		{
			legible = TRUE;
			sc_ptr=&(s_ptr->cast[i]);
		}
	}

	/* Illegible */
	if (!legible) return (100);


	/* Get the level */
	level = sc_ptr->level;

	/* Modify the level */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].benefit != WB_POWER) continue;

		    /* Check for styles */
		if ((w_info[i].styles==0) || (w_info[i].styles & (p_ptr->cur_style & (1L << p_ptr->pstyle))))
		switch (p_ptr->pstyle)
		{
			case WS_MAGIC_BOOK:
			{
				int j;

				for(j=0;j<MAX_SPELL_APPEARS;j++)
				{
					if ((s_info[spell].appears[j].tval == TV_MAGIC_BOOK) &&
					   (s_info[spell].appears[j].sval == p_ptr->psval))
					{
						 level -= (level - w_info[i].level)/5;
					}

				}
				break;
			}
			case WS_PRAYER_BOOK:
			{
				int j;

				for(j=0;j<MAX_SPELL_APPEARS;j++)
				{
					if ((s_info[spell].appears[j].tval == TV_PRAYER_BOOK) &&
					   (s_info[spell].appears[j].sval == p_ptr->psval))
					{
						 level -= (level - w_info[i].level)/5;
					}

				}
				break;
			}
			case WS_SONG_BOOK:
			{
				int j;

				for(j=0;j<MAX_SPELL_APPEARS;j++)
				{
					if ((s_info[spell].appears[j].tval == TV_SONG_BOOK) &&
					   (s_info[spell].appears[j].sval == p_ptr->psval))
					{
						 level -= (level - w_info[i].level)/5;
					}

				}
				break;
			}

		}

	}
	return(level);
}


s16b spell_power(int spell)
{
	int i;

	int plev = p_ptr->lev;

	/* Modify the spell power */
	for (i = 0;i< z_info->w_max;i++)
	{
		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if (w_info[i].benefit != WB_POWER) continue;

		if ((w_info[i].styles==0) || (w_info[i].styles & (p_ptr->cur_style & (1L << p_ptr->pstyle))))
		switch (p_ptr->pstyle)
		{
			case WS_MAGIC_BOOK:
			{
				int j;

				for(j=0;j<MAX_SPELL_APPEARS;j++)
				{
					if ((s_info[spell].appears[j].tval == TV_MAGIC_BOOK) &&
					   (s_info[spell].appears[j].sval == p_ptr->psval))
					{
						 plev += (p_ptr->lev - w_info[i].level);
					}

				}
				break;
			}
			case WS_PRAYER_BOOK:
			{
				int j;

				for(j=0;j<MAX_SPELL_APPEARS;j++)
				{
					if ((s_info[spell].appears[j].tval == TV_PRAYER_BOOK) &&
					   (s_info[spell].appears[j].sval == p_ptr->psval))
					{
						 plev += (p_ptr->lev - w_info[i].level);
					}

				}
				break;
			}
			case WS_SONG_BOOK:
			{
				int j;

				for(j=0;j<MAX_SPELL_APPEARS;j++)
				{
					if ((s_info[spell].appears[j].tval == TV_SONG_BOOK) &&
					   (s_info[spell].appears[j].sval == p_ptr->psval))
					{
						 plev += (p_ptr->lev - w_info[i].level);
					}

				}
				break;
			}
			case WS_INSTRUMENT:
			{
				int j;

				if (!(p_ptr->cur_style & (1L << WS_INSTRUMENT))) break;

				for(j=0;j<MAX_SPELL_APPEARS;j++)
				{
					/* Line 1 - has item wielded */
					/* Line 2 - item is instrument */
					/* Line 3 - instrument sval matches spellbook sval */
					/* Line 4 - appears in spellbook */
					if ((inventory[INVEN_BOW].k_idx) &&
						(inventory[INVEN_BOW].tval == TV_INSTRUMENT) &&
						(s_info[spell].appears[j].sval == inventory[INVEN_BOW].sval) &&
						 (s_info[spell].appears[j].tval == TV_SONG_BOOK))
					{
						plev += (p_ptr->lev - w_info[i].level);
					}
	
				}
				break;

			}
			default:
			{
				if (w_info[i].styles & p_ptr->cur_style) plev += (p_ptr->lev - w_info[i].level);
				break;
			}
		}

	}

	return(plev);

}

/*
 * Returns chance of failure for a spell
 */
s16b spell_chance(int spell)
{
	int chance, minfail;

	spell_type *s_ptr;

	spell_cast *sc_ptr = &(s_info[0].cast[0]);

	bool legible = FALSE;

	int i;

	/* Paranoia -- must be literate */
	if (c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL) return (100);

	/* Get the spell */
	s_ptr = &s_info[spell];

	/* Get casting information */
	for (i=0;i<MAX_SPELL_CASTERS;i++)
	{
		if (s_ptr->cast[i].class == p_ptr->pclass)
		{
			legible = TRUE;
			sc_ptr=&(s_ptr->cast[i]);
		}
	}

	/* Illegible */
	if (!legible) return (100);

	/* Extract the base spell failure rate */
	chance = sc_ptr->fail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - spell_level(spell));

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[c_info[p_ptr->pclass].spell_stat]] - 1);

	/* Not enough mana to cast */
	if (sc_ptr->mana > p_ptr->csp)
	{
		chance += 5 * (sc_ptr->mana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[c_info[p_ptr->pclass].spell_stat]];

	/* Non mage/priest characters never get better than 5 percent */
	if (!(c_info[p_ptr->pclass].spell_power))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if (p_ptr->icky_wield)
	{
		chance += 25;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder (after minfail) */
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
	spell_type *s_ptr;

	spell_cast *sc_ptr = &(s_info[0].cast[0]);

	int i;

	bool legible =FALSE;

	/* Get the spell */
	s_ptr = &s_info[spell];

	/* Get casting information */
	for (i=0;i<MAX_SPELL_CASTERS;i++)
	{
		if (s_ptr->cast[i].class == p_ptr->pclass)
		{
			legible = TRUE;
			sc_ptr=&(s_ptr->cast[i]);
		}
	}

	/* Spell is illegible */
	if (!legible) return (FALSE);

	/* Spell is illegal */
	if (spell_level(spell) > p_ptr->lev) return (FALSE);

	for (i=0;i<PY_MAX_SPELLS;i++)
	{
		if (p_ptr->spell_order[i] == spell) break;
	}

	/* Spell okay to study, not to cast */
	if (i == PY_MAX_SPELLS) return (!known);

	/* Spell is forgotten */
	if ((i < 32) ? (p_ptr->spell_forgotten1 & (1L << i)) :
	      ((i < 64) ? (p_ptr->spell_forgotten2 & (1L << (i - 32))) :
	      ((i < 96) ? (p_ptr->spell_forgotten3 & (1L << (i - 64))) :
	      (p_ptr->spell_forgotten4 & (1L << (i - 96))))))
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if ((i < 32) ? (p_ptr->spell_learned1 & (1L << i)) :
	      ((i < 64) ? (p_ptr->spell_learned2 & (1L << (i - 32))) :
	      ((i < 96) ? (p_ptr->spell_learned3 & (1L << (i - 64))) :
	      (p_ptr->spell_learned4 & (1L << (i - 96))))))
	{
		/* Okay to cast, not to study */
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}

