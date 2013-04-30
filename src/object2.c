/* File: object2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
		m_ptr = &mon_list[j_ptr->held_m_idx];

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
		m_ptr = &mon_list[o_ptr->held_m_idx];

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
	int i, y, x, num, cnt;

	int cur_lev, cur_dis, chance;

	/* Compact */
	if (size)
	{
		/* Message */
		message(MSG_GENERIC, 0, "Compacting objects...");

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
				m_ptr = &mon_list[o_ptr->held_m_idx];

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
			if ((cur_dis > 0) && (distance(p_ptr->py, p_ptr->px, y, x) < cur_dis)) continue;

			/* Saving throw */
			chance = 90;

			/* Squelched items get compacted */
			if ((k_ptr->aware) && (k_ptr->squelch)) chance = 0;

			/* Hack -- only compact artifacts in emergencies */
			if (o_ptr->a_idx && (cnt < 1000)) chance = 100;

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
			if (o_ptr->a_idx && !object_known_p(o_ptr))
			{
				/* Mega-Hack -- Preserve the artifact */
				a_info[o_ptr->a_idx].status &= ~(A_STATUS_CREATED);
			}
		}

		/* If it wasn't perserved, we saw it */
		if (o_ptr->a_idx && (a_info[o_ptr->a_idx].status & A_STATUS_CREATED))
		{
			if (!(a_info[o_ptr->a_idx].status & A_STATUS_AWARE))
				a_info[o_ptr->a_idx].status |= (A_STATUS_LOST);
			artifact_aware(&a_info[o_ptr->a_idx]);
		}

		/* Monster */
		if (o_ptr->held_m_idx)
		{
			monster_type *m_ptr;

			/* Monster */
			m_ptr = &mon_list[o_ptr->held_m_idx];

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
	if (character_dungeon) message(MSG_GENERIC, 0, "Too many objects!");

	/* Oops */
	return (0);
}

/*
 * Get the first object at a dungeon location
 * or NULL if there isn't one.
 */
object_type* get_first_object(int y, int x)
{
	s16b o_idx = cave_o_idx[y][x];

	if (o_idx) return (&o_list[o_idx]);
	
	/* No object */
	return (NULL);
}

/*
 * Get the next object in a stack or
 * NULL if there isn't one.
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
 * Now, applying an object allocation restriction is a matter of toggling 
 * permissions for individual allocations, which are ordered not by level, 
 * but by object index. (From Sangband)
 */
errr get_obj_num_prep(void)
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

	/* Success */
	return (0);
}

/*
 * Choose an object kind that seems "appropriate" to the given level
 *
 * This function calculates level-dependant probabilities for all allowed
 * objects, sums them up, and chooses among them.
 *
 * If no objects match the restrictions laid down, this function will
 * fail, and return zero, but this should *almost* never happen.
 *
 * This is very slow code.  XXX XXX (From Sangband)
 */
s16b get_obj_num(int level)
{
	int i, j, z;
	int lev1, lev2, chance1, chance2;

	object_kind *k_ptr;

	s32b value;
	bool quick = FALSE;

	/* Sometimes boost object level */
	int inflate_chance = GREAT_OBJ;
	int boost = 0;
	if (p_ptr->luck) inflate_chance = LUCK_GREAT_OBJ;

	if ((level > 0) && (rand_int(inflate_chance) == 0))
	{
		/* Roll twice, and take the lowest roll */
		int boost_a = randint(20);
		int boost_b = randint(20);

		if (boost_a > boost_b) boost = boost_b;
		else boost = boost_a;

		/* Boost the depth */
		level += boost;
	}

	/* Restrict level */
	if (level > MAX_DEPTH) level = MAX_DEPTH;

	/* We are not using quick generation */
	if (!quick)
	{
		/* Remember the generation level we are using */
		old_object_level = level;

		/* Remember the restrictions we are using */
		old_get_obj_num_hook = get_obj_num_hook;

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
			for (j = 0; j < MAX_OBJ_ALLOC; j++)
			{
				/* Stop when you first encounter a non-chance */
				if (!k_ptr->chance[j]) break;
				
				/* Look for the closest allocation <= than the current depth */
				if ((k_ptr->locale[j] <= level) && (lev1 <= k_ptr->locale[j]))
				{
					lev1    = k_ptr->locale[j];
					chance1 = k_ptr->chance[j];
				}

				/* Look for the closest allocation > than the current depth */
				else if ((k_ptr->locale[j] > level) && (lev2 > k_ptr->locale[j]))
				{
					lev2    = k_ptr->locale[j];
					chance2 = k_ptr->chance[j];
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

	/* Return the object index */
	return (i);
}

/*
 * The player is now aware of the existance of a given artifact
 */
void artifact_aware(artifact_type *a_ptr)
{
	/* Aware the artifact is in the current game */
	a_ptr->status |= A_STATUS_AWARE;

	/* Aware the artifact exists for history's sake */
	a_ptr->status |= A_STATUS_HISTORY;
}

/*
 * The player knows the full abilities of a given artifact
 */
void artifact_known(artifact_type *a_ptr)
{
	/* Know the effects */
	a_ptr->status |= A_STATUS_KNOWN;

	/* Know the activation */
	a_ptr->status |= A_STATUS_ACTIVATE;

	/* Know for history's sake */
	a_ptr->status |= A_STATUS_MEMORY;
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
	/* Remove special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;

	/* The object is not "sensed" */
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

	/* You must have seen it */
	k_info[o_ptr->k_idx].everseen = TRUE;
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
static s32b object_value_base(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	long value = 0L;

	/* Use template cost for aware objects */
	if (object_aware_p(o_ptr)) value = k_ptr->cost;

	else
	{
		/* Analyze the type */
		switch (o_ptr->tval)
		{
			/* Un-aware Food */
			case TV_FOOD: value = 20L; break;

			/* Un-aware Potions */
			case TV_POTION: value = 100L; break;

			/* Un-aware Scrolls */
			case TV_SCROLL: value = 100L; break;

			/* Un-aware Powders */
			case TV_POWDER: value = 100L; break;

			/* Un-aware Staffs */
			case TV_STAFF: value = 375L; break;

			/* Un-aware Wands */
			case TV_WAND: value = 250L; break;

			/* Un-aware Talismans */
			case TV_TALISMAN: value = 450L; break;
			
			/* Un-aware Rods */
			case TV_ROD: value = 450L; break;
			
			/* Un-aware Rings */
			case TV_RING: value = 235L; break;

			/* Un-aware Amulets */
			case TV_AMULET: value = 240L; break;

			/* Un-aware lites */
			case TV_LITE: value = 230L; break;
		}
	}

	/* Modify according to prefix */
	if (o_ptr->pfx_idx) 
	{
		if (weapon_p(o_ptr))
		{
			value = (value * wpx_info[o_ptr->pfx_idx].cost) / 100;
		}
		if ((o_ptr->tval == TV_BODY_ARMOR))
		{
			value = (value * apx_info[o_ptr->pfx_idx].cost) / 100;
		}

		if ((k_ptr->cost > 0) && (!value)) value = 1;
	}
	
	/* Return value */
	return (value);
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

	/* Modify according to prefix */
	if (o_ptr->pfx_idx) 
	{
		if (weapon_p(o_ptr))
		{
			value = (value * wpx_info[o_ptr->pfx_idx].cost) / 100;
		}
		if ((o_ptr->tval == TV_BODY_ARMOR))
		{
			value = (value * apx_info[o_ptr->pfx_idx].cost) / 100;
		}
	}

	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Artifact */
	if (o_ptr->a_idx)
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];

		/* Hack -- "worthless" artifacts */
		if (!a_ptr->cost) return (0L);

		/* Hack -- Use the artifact cost instead */
		value = a_ptr->cost;
	}

	/* Ego-Item */
	else if (o_ptr->e_idx)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

		/* Hack -- "worthless" ego-items */
		if (!e_ptr->cost) return (0L);

		/* Hack -- Reward the ego-item with a bonus */
		value += e_ptr->cost;

		/* Price bonus for random abilities */
		if (o_ptr->xtra1 && o_ptr->ident & IDENT_MENTAL)
		{
			switch (o_ptr->xtra1)
			{
				case OBJECT_XTRA_TYPE_SUSTAIN:
				{
					switch (o_ptr->xtra2)
					{
						case 0: case 1: case 2: case 3: case 4: value += 2250; break;
						case 5: value += 1750; break; 
					}

					break;
				}

				case OBJECT_XTRA_TYPE_MID_RESIST:
				case OBJECT_XTRA_TYPE_HIGH_RESIST:
				{
					switch (o_ptr->xtra2) 
					{
						case RS_WTR: value += 3000;  break; /* Resist Water */
						case RS_PSN: value += 6000; break; /* Resist Poison */
						case RS_DIS: value += 5000; break; /* Resist Disease */
						case RS_LIT: value += 2500;  break; /* Resist Lite */
						case RS_DRK: value += 2500;  break; /* Resist Dark */
						case RS_SND: value += 3000;  break; /* Resist Sound */
						case RS_SHR: value += 2500;  break; /* Resist Shards */
						case RS_NEX: value += 4000;  break; /* Resist Nexus */
						case RS_NTH: value += 10000; break; /* Resist Nether */
						case RS_CHS: value += 8000; break; /* Resist Chaos */
						case RS_DSN: value += 8000; break; /* Resist Disenchantment */
						case RS_TIM: value += 10000; break; /* Resist Time */
					}
					break;
				}

				case OBJECT_XTRA_TYPE_POWER:
				{
					switch (o_ptr->xtra2) 
					{
						case 0: value += 1250;  break; /* Slow digest */
						case 1: value += 1000;  break; /* Feather Falling */
						case 2: value += 3000;  break; /* Regeneration */
						case 3: value += 15000; break; /* Telepathy */
						case 4: value += 3000;  break; /* See invis */
						case 5: value += 15000; break; /* Invisibility*/
						case 6: value += 1500;  break; /* Perma-lite */
						case 7: value += 5000; break; /* Luck */
					} 
					break;
				}
			}
		}		
	}

	/* Analyze pval bonus */
	switch (o_ptr->tval)
	{
		case TV_BOW:
		case TV_DIGGING:
		case TV_BLUNT:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HEADGEAR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BODY_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_AMULET:
		case TV_RING:
		{
			/* Hack -- Negative "pval" is always bad */
			if (o_ptr->pval < 0) return (0L);

			/* No pval */
			if (!o_ptr->pval) break;

			/* Give credit for stat bonuses */
			if (f1 & (TR1_STR)) value += (o_ptr->pval * 1250L);
			if (f1 & (TR1_INT)) value += (o_ptr->pval * 1250L);
			if (f1 & (TR1_WIS)) value += (o_ptr->pval * 1250L);
			if (f1 & (TR1_DEX)) value += (o_ptr->pval * 1250L);
			if (f1 & (TR1_CON)) value += (o_ptr->pval * 1250L);
			if (f1 & (TR1_CHR)) value += (o_ptr->pval * 1250L);

			/* Give credit for health and mana bonusees */
			if (f1 & (TR1_HEALTH)) value += (o_ptr->pval * 1750L);
			if (f1 & (TR1_MANA)) value += (o_ptr->pval * 1750L);

			/* Give credit for duration bonuses */
			if (f1 & (TR1_SP_DUR)) value += (o_ptr->pval * 2000L);
			if (f1 & (TR1_SP_DAM)) value += (o_ptr->pval * 4000L);

			/* Give credit for stealth and perception */
			if (f1 & (TR1_STEALTH)) value += (o_ptr->pval * 750L);
			if (f1 & (TR1_PERCEPTION)) value += (o_ptr->pval * 500L);

			/* Give credit for infra-vision and tunneling */
			if (f1 & (TR1_INFRA)) value += (o_ptr->pval * 250L);

			/* Give credit for weird things */
			if (f1 & (TR1_MELEE)) value += (o_ptr->pval * 1000L);
			if (f1 & (TR1_MISSILE_SKILL)) value += (o_ptr->pval * 1500L);
			if (f1 & (TR1_ESCAPES)) value += (o_ptr->pval * 1000L);
			if (f1 & (TR1_POWDER_RADIUS)) value += (o_ptr->pval * 2000L);
			if (f1 & (TR1_JUMPING)) value += (o_ptr->pval * 750L);
			if (f1 & (TR1_BOW_THROWN_RANGE)) value += (o_ptr->pval * 500L);
			if (f1 & (TR1_AMBUSH)) value += (o_ptr->pval * 1000L);

			/* Give credit for extra attacks */
			if (f1 & (TR1_BLOWS)) value += (o_ptr->pval * 10000L);

			/* Give credit for speed bonus */
			if (f1 & (TR1_SPEED)) value += (o_ptr->pval * 150000L);

			break;
		}
		case TV_LITE:
		case TV_LITE_SPECIAL:
		{
			/* Hack -- Negative "pval" is always bad */
			if (o_ptr->pval < 0) return (0L);

			/* No pval */
			if (!o_ptr->pval) break;

			/* Give credit for stat bonuses */
			if (f1 & (TR1_STR)) value += (o_ptr->pval * 400L);
			if (f1 & (TR1_INT)) value += (o_ptr->pval * 400L);
			if (f1 & (TR1_WIS)) value += (o_ptr->pval * 400L);
			if (f1 & (TR1_DEX)) value += (o_ptr->pval * 400L);
			if (f1 & (TR1_CON)) value += (o_ptr->pval * 400L);
			if (f1 & (TR1_CHR)) value += (o_ptr->pval * 400L);

			/* Give credit for health and mana bonusees */
			if (f1 & (TR1_HEALTH)) value += (o_ptr->pval * 400L);
			if (f1 & (TR1_MANA)) value += (o_ptr->pval * 400L);

			/* Give credit for duration bonuses */
			if (f1 & (TR1_SP_DUR)) value += (o_ptr->pval * 500L);
			if (f1 & (TR1_SP_DAM)) value += (o_ptr->pval * 1000L);

			/* Give credit for stealth and perception */
			if (f1 & (TR1_STEALTH)) value += (o_ptr->pval * 50L);
			if (f1 & (TR1_PERCEPTION)) value += (o_ptr->pval * 50L);

			/* Give credit for infra-vision and tunneling */
			if (f1 & (TR1_INFRA)) value += (o_ptr->pval * 50L);

			/* Give credit for weird things */
			if (f1 & (TR1_MELEE)) value += (o_ptr->pval * 500L);
			if (f1 & (TR1_MISSILE_SKILL)) value += (o_ptr->pval * 750L);
			if (f1 & (TR1_ESCAPES)) value += (o_ptr->pval * 500L);
			if (f1 & (TR1_POWDER_RADIUS)) value += (o_ptr->pval * 300L);
			if (f1 & (TR1_JUMPING)) value += (o_ptr->pval * 300L);
			if (f1 & (TR1_BOW_THROWN_RANGE)) value += (o_ptr->pval * 250L);
			if (f1 & (TR1_AMBUSH)) value += (o_ptr->pval * 300L);

			/* Give credit for extra attacks */
			if (f1 & (TR1_BLOWS)) value += (o_ptr->pval * 5000L);

			/* Give credit for speed bonus */
			if (f1 & (TR1_SPEED)) value += (o_ptr->pval * 25000L);

			break;
		}

		case TV_MUSIC:
		{
			if (o_ptr->pval) value += ((o_ptr->pval-1) * 5000L);
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

			/* Give credit for bonuses */
			value += ((o_ptr->to_h) * 1250L);

			value += (o_ptr->to_a * 600L);

			/* Done */
			break;
		}

		/* Armor */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_HEADGEAR:
		case TV_SHIELD:
		case TV_BODY_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Give credit for hit bonus */
			value += ((o_ptr->to_h - k_ptr->to_h) * 500L);

			/* Give credit for armor bonus */
			value += (o_ptr->to_a * 600L);

			/* Done */
			break;
		}

		/* Bows/Weapons */
		case TV_BOW:
		case TV_DIGGING:
		case TV_BLUNT:
		case TV_SWORD:
		case TV_POLEARM:
		{
			int to_h_val;
			weapon_prefix_type *wpx_ptr = &wpx_info[o_ptr->pfx_idx];

			/* Hack -- negative hit/damage bonuses */
			if (o_ptr->to_h < 0) return (0L);

			/* Reduce the values if there is a negative prefix */
			if (wpx_ptr->to_h >= 0) to_h_val = o_ptr->to_h;
			else to_h_val = o_ptr->to_h + wpx_ptr->to_h;

			if (to_h_val < 0) to_h_val = 0;

			/* Factor in the bonuses */
			value += ((to_h_val + o_ptr->to_a) * 600L);

			/* Small price increase for lower bonuses */
			if (to_h_val < o_ptr->to_h) value += (o_ptr->to_h - to_h_val) * 50;

			/* Done */
			break;
		}

		/* Ammo */
		case TV_ARROW:
		{
			/* Hack -- negative hit/damage bonuses */
			if (o_ptr->to_h < 0) return (0L);

			/* Factor in the bonuses */
			value += ((o_ptr->to_h) * 25L);

			/* Done */
			break;
		}
	}

	/* No negative value */
	if (value < 0) value = 0;

	/* Return the value */
	return (value);
}

/* Check if a player can destroy an item - and update the inscription accordingly*/
bool destroy_check(object_type *o_ptr)
{
	/* Quest items cannot be destroyed */
	if (o_ptr->tval == TV_QUEST) return FALSE;

	/* Artifacts cannot be destroyed */
	if (o_ptr->a_idx)
	{
		/* Remove special inscription, if any */
		if (!object_known_p(o_ptr)) switch (o_ptr->discount)
		{
			case 0:
			case INSCRIP_NULL:
			case INSCRIP_UNCURSED:
			case INSCRIP_INDESTRUCT:
			{
				o_ptr->discount = INSCRIP_INDESTRUCT;
				break;
			}
			case INSCRIP_TERRIBLE:
			case INSCRIP_CURSED:
			{
				o_ptr->discount = INSCRIP_TERRIBLE;
				break;
			}
			case INSCRIP_GOOD:
			case INSCRIP_SPECIAL:
			{
				o_ptr->discount = INSCRIP_SPECIAL;
				break;
			}
		}

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return FALSE;
	}

	return TRUE;
}

/*
 * Distribute charges of rods or talismanss.
 *
 * o_ptr = source item
 * q_ptr = target item, must be of the same type as o_ptr
 * amt   = number of items that are transfered
 */
void distribute_charges(object_type *o_ptr, object_type *q_ptr, int amt)
{
	/*
	 * Hack -- If rods are dropped, the total maximum timeout or
	 * charges need to be allocated between the two stacks.  If all the items
	 * are being dropped, it makes for a neater message to leave the original
	 * stack's pval alone. -LM-
	 */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
	{
		/* 
		 * Hack -- Rods need to have their timeouts distributed.  The
		 * dropped stack will accept all time remaining to charge up to its
		 * maximum.
		 */
		if (o_ptr->timeout)
		{
			if ((amt * o_ptr->pval > o_ptr->timeout)) q_ptr->timeout = o_ptr->timeout;
			else q_ptr->timeout = amt * o_ptr->pval;

			if (amt < o_ptr->number) o_ptr->timeout -= q_ptr->timeout;
		}
	}
}

void reduce_charges(object_type *o_ptr, int amt)
{
	int i = o_ptr->number - amt;

	/* Entire stack is being destoryed */
	if (!i) return;

	/*
	 * Reduce the charge value in the stack - assume destroying the partially destroyed ones 
	 * first
	 */
	if (((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN)))
	{
		/* Some timeout left */
		if (o_ptr->timeout > i * o_ptr->pval) o_ptr->timeout = i * o_ptr->pval;
		/* All gone */
		else o_ptr->timeout = 0;
	}
}

/*
 * Return the price of an item including plusses (and charges).
 *
 * This function returns the "value" of the given item (qty one).
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
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
bool object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return (FALSE);

	/* Analyze the items */
	switch (o_ptr->tval)
	{
		/* Food and Potions and Scrolls */
		case TV_POTION:
		case TV_POWDER:
		case TV_SCROLL:
		case TV_FOOD:
		{
			/* Assume okay */
			break;
		}

		/* Staffs and Wands */
		case TV_STAFF:
		case TV_WAND:
		{
			return (FALSE);
		}

		/* Rods */
		case TV_ROD:
		{
			/* Require identical recharge times */
			if (o_ptr->pval != j_ptr->pval) return FALSE;

			/* Okay if not too many */
			if (total >= MAX_STACK_ROD) return (FALSE);
			break;
		}

		case TV_TALISMAN:
		{
			/* Require identical recharge times */
			if (o_ptr->pval != j_ptr->pval) return FALSE;

			/* Okay if not too many */
			if (total >= MAX_STACK_TALIS) return FALSE;
			break;
		}

		case TV_LITE: /* Lites */
		{
			/* Okay if not too many */
			if (total >= MAX_STACK_LITE) return FALSE;

			/* Require identical fuel*/
			if (o_ptr->timeout != j_ptr->timeout) return (FALSE);
		}

		/* Weapons and Armor */
		case TV_BOW:
		case TV_DIGGING:
		case TV_BLUNT:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HEADGEAR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BODY_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Fall through */
		}

		/* Rings, Amulets */
		case TV_RING:
		case TV_AMULET:
		case TV_MUSIC:
		{
			/* Require full knowledge of both items */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr)) return (FALSE);

			/* Fall through */
		}

		/* Missiles */
		case TV_ARROW:
		{
			/* Require identical knowledge of both items */
			if (object_known_p(o_ptr) != object_known_p(j_ptr)) return (FALSE);

			/* Require identical "bonuses" */
			if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
			if (o_ptr->to_a != j_ptr->to_a) return (FALSE);

			/* Require identical "pval" code */
			if (o_ptr->pval != j_ptr->pval) return (FALSE);

			/* Require identical "artifact" names */
			if (o_ptr->a_idx != j_ptr->a_idx) return (FALSE);

			/* Require identical "ego-item" names */
			if (o_ptr->e_idx != j_ptr->e_idx) return (FALSE);

			/* Hack -- Never stack "powerful" items */
			if (o_ptr->xtra1 || j_ptr->xtra1) return (FALSE);

			/* Hack -- Never stack recharging items, except lites */
			if ((!o_ptr == TV_LITE) && (o_ptr->timeout || j_ptr->timeout)) return (FALSE);

			/* Probably okay */
			break;
		}

		/* Various */
		default:
		{
			/* Require knowledge */
			if (!object_known_p(o_ptr) || !object_known_p(j_ptr)) return (FALSE);

			/* Probably okay */
			break;
		}
	}

	/* Hack -- Require identical "cursed" and "broken" status */
	if (((o_ptr->ident & (IDENT_CURSED)) != (j_ptr->ident & (IDENT_CURSED))) ||
	    ((o_ptr->ident & (IDENT_BROKEN)) != (j_ptr->ident & (IDENT_BROKEN))))
	{
		return (FALSE);
	}

	/* Hack -- Require compatible prefixs */
	if (o_ptr->pfx_idx != j_ptr->pfx_idx)
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

	/* Hack -- Require compatible "discount" fields */
	if (o_ptr->discount != j_ptr->discount)
	{
		/* Both are (different) special inscriptions */
		if ((o_ptr->discount >= INSCRIP_NULL) &&
		    (j_ptr->discount >= INSCRIP_NULL))
		{
			/* Normally require matching inscriptions */
			return (FALSE);
		}

		/* One is a special inscription, one is a discount or nothing */
		else if ((o_ptr->discount >= INSCRIP_NULL) ||
		         (j_ptr->discount >= INSCRIP_NULL))
		{
			/* Normally require matching inscriptions */
			if (!stack_force_notes) return (FALSE);

			/* Hack -- Never merge a special inscription with a discount */
			if ((o_ptr->discount > 0) && (j_ptr->discount > 0)) return (FALSE);
		}

		/* One is a discount, one is a (different) discount or nothing */
		else
		{
			/* Normally require matching discounts */
			if (!stack_force_costs) return (FALSE);
		}
	}

	/* Maximal "stacking" limit */
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
 * The blending of the "discount" field assumes that either (1) one is a
 * special inscription and one is nothing, or (2) one is a discount and
 * one is a smaller discount, or (3) one is a discount and one is nothing,
 * or (4) both are nothing.  In all of these cases, we can simply use the
 * "maximum" of the two "discount" fields.
 *
 * These assumptions are enforced by the "object_similar()" code.
 */
void object_absorb(object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Add together the item counts */
	o_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Hack -- Blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr);

	/* Hack -- Blend "mental" status */
	if (j_ptr->ident & (IDENT_MENTAL)) o_ptr->ident |= (IDENT_MENTAL);

	/* Hack -- Blend "notes" */
	if (j_ptr->note != 0) o_ptr->note = j_ptr->note;

	/* Mega-Hack -- Blend "discounts" */
	if (o_ptr->discount < j_ptr->discount) o_ptr->discount = j_ptr->discount;

	/* Hack -- if rods are stacking, add current timeouts together. -LM- */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
	{
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Combine the histories */
	stack_histories(o_ptr, j_ptr);

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

	/* Oops */
	message_format(MSG_GENERIC, 0, "No object (%d,%d)", tval, sval);

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

	/* Default magic */
	o_ptr->to_h = k_ptr->to_h;
	o_ptr->to_a = k_ptr->to_a;

	/* Hack -- worthless items are always "broken" */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
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
static void object_mention(const object_type *o_ptr)
{
	char o_name[80];

	/* Describe */
	object_desc_store(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Artifact */
	if (o_ptr->a_idx)
	{
		/* Silly message */
		message_format(MSG_CHEAT, 0, "Artifact (%s)", o_name);
	}

	/* Ego-item */
	else if (o_ptr->e_idx)
	{
		/* Silly message */
		message_format(MSG_CHEAT, 0, "Ego-item (%s)", o_name);
	}

	/* Normal item */
	else
	{
		/* Silly message */
		message_format(MSG_CHEAT, 0, "Object (%s)", o_name);
	}
}

/*
 * Attempt to change an object into an ego-item -MWK-
 * Better only called by apply_magic().
 * The return value says if we picked a cursed item (if allowed) and is
 * passed on to a_m_aux1/2().
 * If no legal ego item is found, this routine returns 0, resulting in
 * an unenchanted item.
 */
static int make_ego_item(object_type *o_ptr, bool only_good)
{
	int i, j, level;

	int e_idx;

	long value, total;

	ego_item_type *e_ptr;

	alloc_entry *table = alloc_ego_table;

	/* Fail if object already is ego or artifact */
	if (o_ptr->a_idx) return (FALSE);
	if (o_ptr->e_idx) return (FALSE);

	level = object_level;

	/* Sometimes boost object level (just like with object base types) */
	int inflate_chance = GREAT_OBJ;
	int boost = 0;
	if (p_ptr->luck) inflate_chance = LUCK_GREAT_OBJ;

	if ((level > 0) && (rand_int(inflate_chance) == 0))
	{
		/* Roll twice, and take the lowest roll */
		int boost_a = randint(20);
		int boost_b = randint(20);

		if (boost_a > boost_b) boost = boost_b;
		else boost = boost_a;

		/* Boost the depth */
		level += boost;
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

		/* If we force good/great, don't create cursed */
		if (only_good && (e_ptr->flags3 & TR3_LIGHT_CURSE)) continue;

		/* Require proper material */
		if ((e_ptr->base_material) && (object_material(o_ptr) != e_ptr->base_material)) continue;

		/* Require weight in bounds, lower */
		if ((e_ptr->min_weight) && (object_weight(o_ptr) < e_ptr->min_weight)) continue;

		/* Require sval in bounds, upper */
		if ((e_ptr->max_weight) && (object_weight(o_ptr) > e_ptr->max_weight)) continue;

		/* Test if this is a legal ego-item type for this object */
		for (j = 0; j < EGO_TVALS_MAX; j++)
		{
			/* Require identical base type */
			if (o_ptr->tval != e_ptr->tval[j]) continue;

			/* Require sval in bounds, lower */
			if (o_ptr->sval < e_ptr->min_sval[j]) continue;

			/* Require sval in bounds, upper */
			if (o_ptr->sval > e_ptr->max_sval[j]) continue;

			/* Accept */
			table[i].prob3 = table[i].prob2;

			break;
		}

		/* Total */
		total += table[i].prob3;
	}

	/* No legal ego-items -- create a normal unenchanted one */
	if (total == 0) return (0);

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
	e_idx = (byte)table[i].index;
	o_ptr->e_idx = e_idx;

	return ((e_info[e_idx].flags3 & TR3_LIGHT_CURSE) ? -2 : 2);
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
static bool make_artifact_special(object_type *o_ptr, bool real_depth)
{
	int i;

	int k_idx;

	int depth_check = ((real_depth) ? p_ptr->depth : object_level);

	/* No artifacts, do nothing */
	if (adult_no_artifacts) return (FALSE);

	/* No artifacts in the town */
	if (!object_level) return (FALSE);

	/* Check the special artifacts */
	for (i = 0; i < z_info->a_min_normal; ++i)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->status & A_STATUS_CREATED) continue;

		/* Enforce minimum "depth" (loosely) */
		if (a_ptr->level > depth_check)
		{
			/* Get the "out-of-depth factor" */
			int d = (a_ptr->level - depth_check) * 2;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* Artifact "rarity roll" */
		if (rand_int(a_ptr->rarity) != 0) continue;

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Enforce minimum "object" level (loosely) */
		if (k_info[k_idx].level > depth_check)
		{
			/* Get the "out-of-depth factor" */
			int d = (k_info[k_idx].level - depth_check) * 5;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* Assign the template */
		object_prep(o_ptr, k_idx);

		/* Mark the item as an artifact */
		o_ptr->a_idx = i;

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
static bool make_artifact(object_type *o_ptr, bool real_depth)
{
	int i;
	int depth_check = ((real_depth) ? p_ptr->depth : object_level);

	/* No artifacts, do nothing */
	if (adult_no_artifacts) return (FALSE);

	/* No artifacts in the town */
	if (!object_level) return (FALSE);

	/* Paranoia -- no "plural" artifacts */
	if (o_ptr->number != 1) return (FALSE);

	/* Sometimes boost object level (just like with object base types) */
	int inflate_chance = GREAT_OBJ;
	int boost = 0;
	if (p_ptr->luck) inflate_chance = LUCK_GREAT_OBJ;

	if ((depth_check > 0) && (rand_int(inflate_chance) == 0))
	{
		/* Roll twice, and take the lowest roll */
		int boost_a = randint(MAX_DEPTH - depth_check);
		int boost_b = randint(MAX_DEPTH - depth_check);

		if (boost_a > boost_b) boost = boost_b;
		else boost = boost_a;
	}

	depth_check += boost;

	/* Check the artifact list (skip the "specials") */
	for (i = z_info->a_min_normal; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" items */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->status & A_STATUS_CREATED) continue;

		/* Must have the correct fields */
		if (a_ptr->tval != o_ptr->tval) continue;
		if (a_ptr->sval != o_ptr->sval) continue;

		/* Enforce minimum "depth" */
		if (a_ptr->level > depth_check) continue;

		/* We must make the "rarity roll" */
		if (rand_int(a_ptr->rarity) != 0) continue;

		/* Mark the item as an artifact */
		o_ptr->a_idx = i;

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
		case SV_WAND_GROWTH:			o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_HASTE_MONSTER:		o_ptr->pval = randint(20) + 8; break;
		case SV_WAND_CLONE_MONSTER:		o_ptr->pval = randint(5)  + 3; break;
		case SV_WAND_TELEPORT_AWAY:		o_ptr->pval = randint(5)  + 6; break;
		case SV_WAND_HARPOON:			o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_TRAP_DOOR_DEST:		o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_STONE_TO_MUD:		o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_LITE:			o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_DRYAD:			o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_SLOW_MONSTER:		o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_CALM_MONSTER:		o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_CURSE_MONSTER:		o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_STUN_BURST:		o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_SWAP_PLACES:		o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_DRAIN_LIFE:		o_ptr->pval = randint(3)  + 3; break;
		case SV_WAND_POLYMORPH:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_STINKING_CLOUD:		o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_MAGIC_MISSILE:		o_ptr->pval = randint(8) + 6; break;
		case SV_WAND_ACID_BOLT:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_ELEC_BOLT:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_FIRE_BOLT:			o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_COLD_BOLT:			o_ptr->pval = randint(5)  + 6; break;
		case SV_WAND_WONDER:			o_ptr->pval = randint(15) + 6; break;
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
		case SV_STAFF_EDEN:			o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_SLOWNESS:			o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_HASTE_MONSTERS:	o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_STORM_SHIELD:		o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_TELEPORTATION:	o_ptr->pval = randint(4)  + 5; break;
		case SV_STAFF_DETECT_LIFE:			o_ptr->pval = randint(15) + 8; break;
		case SV_STAFF_REMOVE_CURSE:		o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_STARLITE:			o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_LITE:				o_ptr->pval = randint(20) + 8; break;
		case SV_STAFF_DETECT_FORCE:			o_ptr->pval = randint(15)  + 8; break;
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
		case SV_STAFF_BARRIERS:			o_ptr->pval = randint(4)  + 2; break;
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
	int tohit1, tohit2;

	tohit1 = randint(3) + m_bonus(7, level);
	tohit2 = m_bonus(10, level);

	/* Good */
	if (power > 0)
	{
		/* Enchant */
		o_ptr->to_h += tohit1;

		/* Very good */
		if (power > 1)
		{
			/* Enchant again */
			o_ptr->to_h += tohit2;
		}
	}

	/* Cursed */
	else if (power < 0)
	{
		/* Penalize */
		o_ptr->to_h -= tohit1;

		/* Very cursed */
		if (power < -1)
		{
			/* Penalize again */
			o_ptr->to_h -= tohit2;
		}

		/* Cursed (if "bad") */
		if (o_ptr->to_h < 0) o_ptr->ident |= (IDENT_CURSED);
	}

	/* Digging items */
	if (o_ptr->tval == TV_DIGGING)
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
	}
}

/*
 * Apply magic to an item known to be "armor"
 */
static void a_m_aux_2(object_type *o_ptr, int level, int power)
{
	int toac1 = randint(3) + m_bonus(3, level);
	int toac2 = m_bonus(5, level);

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
			rating += 20;

			/* Mention the item */
			if (cheat_peek) object_mention(o_ptr);

			break;
		}
	}
}

/*
 * Apply magic to an item known to be a "ring" or "amulet"
 *
 * Hack -- note special rating boost for ring of speed, greatness, and elements
 * Hack -- note special rating boost for amulet of power
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
				/* Strength, Constitution, Dexterity*/
				case SV_RING_STR:
				case SV_RING_CON:
				case SV_RING_DEX:
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

				/* Health, Mana */
				case SV_RING_HEALTH:
				case SV_RING_MANA:
				{
					/* Stat bonus */
					o_ptr->pval = 1 + m_bonus(4, level);

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

				/* Greatness */
				case SV_RING_GREATNESS:
				{
					/* Stat bonus */
					o_ptr->pval = 1 + m_bonus(1, level);

					/* Rating boost */
					rating += 20;

					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);

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

					else
					{
						/* Rating boost */
						rating += 20;

						/* Mention the item */
						if (cheat_peek) object_mention(o_ptr);
					}

					break;
				}

				/* Searching */
				case SV_RING_SEARCHING:
				case SV_RING_AMBUSH:
				case SV_RING_WIND_CONTROL:
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
				case SV_RING_LIGHTNING:
				{
					/* Rating boost */
					rating += 10;

					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);

					break;
				}

				/* Ring of Accuracy */
				case SV_RING_ACCURACY:
				{
					/* Bonus to hit */
					o_ptr->to_h = damroll(2, 5) + m_bonus(10, level);

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
			}

			break;
		}

		case TV_AMULET:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Amulet of wisdom/intelligence/charisma */
				case SV_AMULET_WIS:
				case SV_AMULET_INT:
				case SV_AMULET_CHR:
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
				case SV_AMULET_MARKSMAN:
				{
					o_ptr->pval = randint(5) + m_bonus(3, level);

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

				/* Amulet of Infravision */
				case SV_AMULET_INFRAVISION:
				{
					o_ptr->pval = randint(2) + m_bonus(4, level);

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
					o_ptr->to_a = randint(3) + m_bonus(3, level);

					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);

					break;
				}

				/* Amulet of Power -- never cursed */
				case SV_AMULET_POWER:
				{
					o_ptr->pval = randint(2) + m_bonus(2, level);

					/* Boost the rating */
					rating += 15;

					/* Mention the item */
					if (cheat_peek) object_mention(o_ptr);

					break;
				}

				/* Amulet of Unmagic -- always cursed */
				case SV_AMULET_UNMAGIC:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = 0 - (15 - (randint(5) + m_bonus(5, level)));

					break;
				}
			}

			break;
		}
	}
}

/*
 * Apply magic to an item known to be a "lite"
 */
static void a_m_aux_4(object_type *o_ptr, int level, int power)
{
	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		case TV_LITE:
		{
			/* Torches */
			if (o_ptr->sval == SV_WOODEN_TORCH)
			{
				if (!o_ptr->timeout) o_ptr->timeout = FUEL_TORCH;
			}

			/* Ego Torches */
			if (o_ptr->sval == SV_ENCHANTED_TORCH)
			{
				if (!o_ptr->timeout) o_ptr->timeout = FUEL_ENCHANTED;
			}

			/* Ego Torches */
			if (o_ptr->sval > SV_ENCHANTED_TORCH)
			{
				if (!o_ptr->timeout) o_ptr->timeout = FUEL_EGO;
			}

			switch (o_ptr->sval)
			{
				/* Lanterns of int/wis/thievery */
				case SV_TORCH_THIEVERY:
				case SV_TORCH_MEM:
				case SV_TORCH_WIS:
				case SV_TORCH_TRUTH:
				{
					/* Stat bonus */
					o_ptr->pval = 1 + m_bonus(5, level);

					/* Note that torches may not be cursed, even with negative pval */
					if (power < 0)
					{
						/* Broken */
						o_ptr->ident |= (IDENT_BROKEN);

						/* Reverse pval */
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
 * Apply magic to a musical instrument
 */
static void a_m_aux_5(object_type *o_ptr, int level)
{
	int i;

	/* +1 on levels 1-12, +2 13-24, +3 25-36, +4 37-48, +5 49 onwards */
	i = ((level-1)/12)+1;

	/* Paranoia */
	if (i<1) i = 1;

	/* Detemine Exact plus */
	o_ptr->pval = randint(i);

	/* Paranoia */
	if (o_ptr->pval > 5) o_ptr->pval = 5;
}

/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
static void a_m_aux_6(object_type *o_ptr)
{
	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
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
 * "good" and "great" arguments are false.  Objects which are forced "great"
 * get three extra "attempts" to become an artifact.
 */
void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, bool real_depth)
{
	int i, rolls, f1, f2, power;
	
	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

	/* Base chance of being "good" */
	f1 = lev + 12;

	/* Maximal chance of being "good" */
	if (f1 > 75) f1 = 75;

	/* Base chance of being "great" */
	f2 = ((f1 * 3) / 4) + 3;

	/* Maximal chance of being "great" */
	if (f2 > 44) f2 = 44;

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

	/* Lucky characters get a second try for a "good" item with a reduced possibility */
	else if (p_ptr->luck && (rand_int(140) < f1))
	{
		/* Assume "good" */
		power = 1;

		/* Roll for "great" */
		if (great || (rand_int(100) < f2)) power = 2;
	}

	/* Roll for "cursed" */
	else if (rand_int(100) < f1 / 2)
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
	if (!okay || o_ptr->a_idx) rolls = 0;

	/* Roll for artifacts if allowed */
	for (i = 0; i < rolls; i++)
	{
		/* Roll for an artifact */
		if (make_artifact(o_ptr, real_depth)) break;
	}

	/* Hack -- analyze artifacts */
	if (o_ptr->a_idx)
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];

		/* Hack -- Mark the artifact as "created" */
		a_ptr->status |= A_STATUS_CREATED;
		
		/* Extract the other fields */
		o_ptr->pval = a_ptr->pval;
		o_ptr->to_a = a_ptr->to_a;
		o_ptr->to_h = a_ptr->to_h;
		
		/* Add prefix */
		o_ptr->pfx_idx = a_ptr->prefix_idx;

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
		case TV_BLUNT:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/* Try applying a prefix */
			int k, l;
			int depth_check = ((real_depth) ? p_ptr->depth : object_level);

			/* Sometimes boost object level (just like with object base types) */
			int inflate_chance = GREAT_OBJ;
			int boost = 0;
			if (p_ptr->luck) inflate_chance = LUCK_GREAT_OBJ;

			if ((depth_check > 0) && (rand_int(inflate_chance) == 0))
			{
				/* Roll twice, and take the lowest roll */
				int boost_a = randint(20);
				int boost_b = randint(20);

				if (boost_a > boost_b) boost = boost_b;
				else boost = boost_a;
			}

			depth_check += boost;

			/* 13 tries */
			for (k = 1; k < 13; k++)
			{
				int base = k_info[o_ptr->k_idx].level;

				weapon_prefix_type *wpx_ptr;

				/* Try to apply a random prefix */
				l = randint(z_info->wpx_max - 1);
				wpx_ptr = &wpx_info[l];

				/* Not a real prefix */
				if (!wpx_ptr->rarity) continue;

				/* Check for material constraints */
				if (wpx_ptr->base_material)
				{
					if (wpx_ptr->base_material != k_info[o_ptr->k_idx].material) continue;
				}

				/* Hack - Good items must have prefixes worth more than 50% */
				if (good && !(wpx_ptr->flags & PXF_GOOD)) continue;

				/* Roll for rarity */
				if (rand_int(wpx_ptr->rarity) == 0)
				{
					o_ptr->pfx_idx = l;

					break;
				}

				/* Discard if out of depth */
				if ((base + wpx_ptr->depth) > depth_check) continue;
			}

			/* Fall through */

		}
		case TV_DIGGING:
		case TV_BOW:
		case TV_ARROW:
		{
			if ((power > 1) || (power < -1))
			{
				int ego_power;

				ego_power = make_ego_item(o_ptr, (bool)(good || great));

				if (ego_power) power = ego_power;
			}

 			if (power) a_m_aux_1(o_ptr, lev, power);

			break;
		}

		case TV_BODY_ARMOR:
		{
			/* Try applying a prefix */
			int k, l;
			int depth_check = ((real_depth) ? p_ptr->depth : object_level);

			/* Sometimes boost object level (just like with object base types) */
			int inflate_chance = GREAT_OBJ;
			int boost = 0;
			if (p_ptr->luck) inflate_chance = LUCK_GREAT_OBJ;

			if ((depth_check > 0) && (rand_int(inflate_chance) == 0))
			{
				/* Roll twice, and take the lowest roll */
				int boost_a = randint(20);
				int boost_b = randint(20);

				if (boost_a > boost_b) boost = boost_b;
				else boost = boost_a;
			}

			depth_check += boost;

			/* 13 tries */
			for (k = 1; k < 13; k++)
			{
				int base = k_info[o_ptr->k_idx].level;

				armor_prefix_type *apx_ptr;

				/* Try to apply a random prefix */
				l = randint(z_info->apx_max - 1);
				apx_ptr = &apx_info[l];

				/* Not a real prefix */
				if (!apx_ptr->rarity) continue;

				/* Check for material constraints */
				if (apx_ptr->base_material)
				{
					if (apx_ptr->base_material != k_info[o_ptr->k_idx].material) continue;
				}

				/* Hack - Good items must have prefixes worth more than 50% */
				if (good && !(apx_ptr->flags & PXF_GOOD)) continue;

				/* Roll for rarity */
				if (rand_int(apx_ptr->rarity) == 0)
				{
					o_ptr->pfx_idx = l;

					break;
				}

				/* Discard if out of depth */
				if ((base + apx_ptr->depth) > depth_check) continue;
			}

			/* Fall through */

		}
		case TV_DRAG_ARMOR:
		case TV_SHIELD:
		case TV_HEADGEAR:
		case TV_CLOAK:
		case TV_GLOVES:
		case TV_BOOTS:
		{
			if ((power > 1) || (power < -1))
			{
				int ego_power;

				ego_power = make_ego_item(o_ptr, (bool)(good || great));

				if (ego_power) power = ego_power;
			}

 			if (power) a_m_aux_2(o_ptr, lev, power);

			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			if (!power && (rand_int(100) < 75)) power = -1;
			a_m_aux_3(o_ptr, lev, power);
			break;
		}

		case TV_LITE:
		{
			if (!power && (rand_int(100) < 35)) power = -1;
			a_m_aux_4(o_ptr, lev, power);
			break;
		}

		case TV_MUSIC:
		{
			a_m_aux_5(o_ptr, lev);
			break;
		}

		default:
		{
			a_m_aux_6(o_ptr);
			break;
		}
	}

	/* Hack -- analyze ego-items */
	if (o_ptr->e_idx)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

		/* Extra powers */
		if (e_ptr->xtra)
		{
			if (e_ptr->xtra == OBJECT_XTRA_TYPE_RESIST)
			{
				if (randint(4) == 0) o_ptr->xtra1 = OBJECT_XTRA_TYPE_HIGH_RESIST;
				else o_ptr->xtra1 = OBJECT_XTRA_TYPE_MID_RESIST;
			}
			else o_ptr->xtra1 = e_ptr->xtra;

			switch (o_ptr->xtra1)
			{
				case OBJECT_XTRA_TYPE_SUSTAIN:
				{
					o_ptr->xtra2 = (byte)rand_int(OBJECT_XTRA_SIZE_SUSTAIN);
					break;
				}

				case OBJECT_XTRA_TYPE_MID_RESIST:
				{
					/* Mega hack - Water - Shards resist */
					o_ptr->xtra2 = (byte)rand_int(RS_SHR - RS_WTR + 1) + RS_WTR;
					break;
				}

				case OBJECT_XTRA_TYPE_HIGH_RESIST:
				{
					/* Mega hack Nexus - Time resist */
					o_ptr->xtra2 = (byte)rand_int(RS_TIM - RS_NEX + 1) + RS_NEX;
					break;
				}

				case OBJECT_XTRA_TYPE_POWER:
				{
					int n;
					u32b flag;

					/* Mega hack - not all powers are equal */
					while (TRUE)
					{
						n = (byte)rand_int(OBJECT_XTRA_SIZE_POWER);
						flag = (OBJECT_XTRA_BASE_POWER << n);

						/* Low usefulness */
						if (flag & (TR3_LOW_USE_MASK))
						{
							/* More likely for low-powered items */
							if (rand_int(lev) < 20) break;
						}

						/* Medium usefulness */
						if (flag & (TR3_MID_USE_MASK)) break;

						/* High usefulness */
						if (flag & (TR3_HIGH_USE_MASK))
						{
							/* Always a slim chance */
							if (rand_int(10) == 0) break;

							/* Otherwise, more likely for good items */
							if (rand_int(lev) > 30) break;
						}
					}

					o_ptr->xtra2 = n;

					break;
				}
			}
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
			if (e_ptr->max_to_a > 0) o_ptr->to_a -= randint(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval > 0) o_ptr->pval -= randint(e_ptr->max_pval);
		}

		/* Hack -- apply extra bonuses if needed */
		else
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h > 0) o_ptr->to_h += randint(e_ptr->max_to_h);
			if (e_ptr->max_to_a > 0) o_ptr->to_a += randint(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval > 0) o_ptr->pval += randint(e_ptr->max_pval);
		}

		/* Hack -- apply rating bonus */
		rating += e_ptr->rating;

		/* Cheat -- describe the item */
		if (cheat_peek) object_mention(o_ptr);

	}
	/* Examine real objects */
	else if (o_ptr->k_idx)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Hack -- acquire "broken" flag */
		if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
	}
}

/* Hack - paremeters for "typed" items */
static byte typed_tval;

/*
 * Determine if a template is a proper "mimic" item
 */
static bool kind_typed(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];
	
	/* Flavored items */
	if (k_ptr->tval == typed_tval) return (TRUE);
	
	/* Not a mimic item */
	return (FALSE);
}

/* Hack - parameters for "mimic" items */
static byte mimic_attr;
static char mimic_char;

/*
 * Determine if a template is a proper "mimic" item
 */
static bool kind_mimic(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];
	
	/* Flavored items */
	if ((k_ptr->flavor) && ((k_ptr->flavor % 16) == mimic_attr)
		&& (k_ptr->d_char == mimic_char)) return (TRUE);
	/* Non-flavored items */
	else if ((k_ptr->d_attr == mimic_attr) && (k_ptr->d_char == mimic_char)) return (TRUE);
	
	/* Not a mimic item */
	return (FALSE);
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
		case TV_BODY_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HEADGEAR:
		{
			if (k_ptr->to_a < 0) return (FALSE);
			return (TRUE);
		}

		/* Weapons -- Good unless damaged */
		case TV_BOW:
		case TV_SWORD:
		case TV_BLUNT:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			if (k_ptr->to_h < 0) return (FALSE);
			return (TRUE);
		}

		/* Ammo -- Arrows/Bolts are good */
		case TV_ARROW:
		{
			return (TRUE);
		}

		/* Books -- Books with the good flag are good */
		case TV_MAGIC_BOOK:
		{
			if (books[k_ptr->sval].flags & SBF_GOOD) return (TRUE);
			return (FALSE);
		}

		/* Rings and amulets - depends on cost/depth */
		case TV_RING:
		case TV_AMULET:
		{
			if (k_ptr->cost >= 2000 + object_level * 1000) return (TRUE);
			return (FALSE);
		}

		/* Rods/Staves/Wands/Talismans - depends on cost/depth */
		case TV_WAND:
		case TV_STAFF:
		case TV_ROD:
		case TV_TALISMAN:
		{
			if (k_ptr->cost >= 3000 + object_level * 500) return (TRUE);
			return (FALSE);
		}

		/* Lights - depends on cost/depth */
		case TV_LITE:
		{
			if (k_ptr->cost >= 2000 + object_level * 300) return (TRUE);
			return (FALSE);
		}
	}

	/* Assume not good */
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
bool make_object(object_type *j_ptr, bool good, bool great, bool real_depth)
{
	int prob, base;
	int temp_level = object_level;
	
	object_kind *k_ptr;

	/* Base level for the object */
	base = (good ? (object_level + 10) : object_level);

	/* Chance of "special object" */
	prob = (good ? 10 : 1000);

	/* Generate a special artifact, or a normal object */
	if ((rand_int(prob) != 0) || !make_artifact_special(j_ptr, real_depth))
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
	apply_magic(j_ptr, object_level, TRUE, good, great, real_depth);

	k_ptr = &k_info[j_ptr->k_idx];

	/* Hack -- some objects appear in stacks */
	if ((k_ptr->qd>1) || (k_ptr->qs>1)) j_ptr->number = damroll(k_ptr->qd, k_ptr->qs);

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) && (k_ptr->level > p_ptr->depth))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - p_ptr->depth);

		/* Cheat -- peek at items */
		if (cheat_peek) object_mention(j_ptr);
	}

	object_level = temp_level;

	/* Success */
	return (TRUE);
}

/*
 * Attempt to make an object (normal or good/great)
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "object_level" for the "generation level".
 *
 * We assume that the given object has been "wiped".
 *
 * Great gives "great" magic to all applicable items.
 */
bool make_typed(object_type *j_ptr, byte tval, bool good, bool great, bool real_depth)
{
	int temp_level = object_level;
	int prob, base;
	
	object_kind *k_ptr;

	/* Base level for the object */
	base = (good ? (object_level + 10) : object_level);

	prob = (good ? 10 : 1000);

	while (TRUE)
	{
		/* Generate a special artifact, or a normal object */
		if ((rand_int(prob) != 0) || !make_artifact_special(j_ptr, real_depth))
		{
			int k_idx;

			typed_tval = tval;

			/* Activate restriction */
			get_obj_num_hook = kind_typed;

			/* Prepare allocation table */
			get_obj_num_prep();

			/* Pick a random object */
			k_idx = get_obj_num(base);

			/* Clear restriction */
			get_obj_num_hook = NULL;

			/* Prepare allocation table */
			get_obj_num_prep();

			/* Handle failure */
			if (!k_idx) return (FALSE);

			/* Prepare the object */
			object_prep(j_ptr, k_idx);
		}

		/* Don't allow cursed objects if "good" is set */
		if (!good || (object_value_real(j_ptr) > 0)) break;
	}

	/* Apply magic (allow artifacts) */
	apply_magic(j_ptr, object_level, TRUE, good, great, real_depth);

	k_ptr = &k_info[j_ptr->k_idx];

	/* Hack -- some objects appear in stacks */
	if ((k_ptr->qd>1) || (k_ptr->qs>1)) j_ptr->number = damroll(k_ptr->qd, k_ptr->qs);

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) && (k_ptr->level > p_ptr->depth))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - p_ptr->depth);

		/* Cheat -- peek at items */
		if (cheat_peek) object_mention(j_ptr);
	}

	object_level = temp_level;

	/* Success */
	return (TRUE);
}

/*
 * Hack - make an object according to its character and color
 */
bool make_mimic(object_type *j_ptr, byte a, char c)
{
	int k_idx;
	object_kind *k_ptr;
	bool good = FALSE;
	bool great = FALSE;

	/* Hack - gold mimics */
   	if (c == '$') 
	{
		int coin_type = 0;

		/* 
		 * Hack - determine coin type according to color. A hack, but better
		 * than the old method of using monster names for this. 
		 */
		switch (a)
		{
			case TERM_UMBER: coin_type = 3; break; /* copper */
			case TERM_SLATE: coin_type = 6; break; /* silver */
			/* case TERM_RED: coin_type = 7; break; - garnets, same color as rubies */
			case TERM_YELLOW: coin_type = 11; break; /* gold */
			case TERM_L_WHITE: coin_type = 12; break; /* opals */
			case TERM_BLUE: coin_type = 13; break; /* sapphires */
			case TERM_RED: coin_type = 14; break; /* rubies */
			case TERM_WHITE: coin_type = 15; break; /* diamonds */
			case TERM_GREEN: coin_type = 16; break; /* emeralds */
			case TERM_L_BLUE: coin_type = 17; break; /* mithril */
			case TERM_L_GREEN: coin_type = 18; break; /* adamantite */
		}			

		/* Make gold */
		return (make_gold(j_ptr, coin_type));
	}

	/* Set the variables */
	mimic_char = c;
	mimic_attr = a;
	
	/* Activate restriction */
	get_obj_num_hook = kind_mimic;

	/* Prepare allocation table */
	get_obj_num_prep();

	/* Pick a random object */
	k_idx = get_obj_num(object_level);

	/* Clear restriction */
	get_obj_num_hook = NULL;

	/* Prepare allocation table */
	get_obj_num_prep();

	/* Handle failure */
	if (!k_idx) return (FALSE);

	/* Prepare the object */
	object_prep(j_ptr, k_idx);
	
	/* Apply magic (allow artifacts) */
	apply_magic(j_ptr, object_level, TRUE, good, great, TRUE);

	k_ptr = &k_info[j_ptr->k_idx];

	/* Hack -- some objects appear in stacks */
	if ((k_ptr->qd>1) || (k_ptr->qs>1)) j_ptr->number = damroll(k_ptr->qd, k_ptr->qs);

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) && (k_ptr->level > p_ptr->depth))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - p_ptr->depth);

		/* Cheat -- peek at items */
		if (cheat_peek) object_mention(j_ptr);
	}

	/* success */
	return (TRUE);
}

/*
 * Make a treasure object
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_gold(object_type *j_ptr, int coin_type)
{
	int sval;
	int k_idx;

	s32b base;

	/* Hack -- Choose type of coin */
	if (coin_type) sval = coin_type;
	else sval = (randint(object_level + 2) / 2) + 1;

	/* Do not create "illegal" Treasure Types */
	if (sval < 1) sval = 1;
	if (sval >= MAX_GOLD) sval = MAX_GOLD;

	k_idx = lookup_kind(TV_GOLD, sval);

	/* Prepare a gold object */
	object_prep(j_ptr, k_idx);

	/* Hack -- Base coin cost */
	base = k_info[k_idx].cost;

	/* Determine how much the treasure is "worth". *2.3 the values for FayAngband. */
	j_ptr->pval = (s16b)(base + rand_int(9) + (4L * rand_int(2.3*(base + 1))));

	/* Success */
	return (TRUE);
}

/*
 * Mark an object's history
 */
void object_history(object_type *o_ptr, byte origin, s16b r_idx, s16b s_idx, s16b u_idx)
{
	o_ptr->origin_nature = origin;

	switch(origin)
	{
		case ORIGIN_DROP_KNOWN:
		{
			o_ptr->origin_r_idx = r_idx;
			o_ptr->origin_s_idx = s_idx;
			o_ptr->origin_u_idx = u_idx;

			/* Fall through */
		}
		case ORIGIN_ACQUIRE: case ORIGIN_DROP_UNKNOWN: 
		case ORIGIN_FLOOR:	 case ORIGIN_CHEST:
		case ORIGIN_RACK:	case ORIGIN_SHELF:
		case ORIGIN_CLOSET:	case ORIGIN_FOREST:
		{
			o_ptr->origin_dlvl = p_ptr->depth;
			break;
		}
	}
}

/* The possible actions */

#define KEEP_O		1
#define	KEEP_J		2
#define LOSE_M_EGO	3
#define MIX			4

/* 
 * Try to stack item histories 
 */
void stack_histories(object_type *o_ptr, const object_type *j_ptr)
{
	int action = MIX;

	/* Histories are identical anyway */
	if ((o_ptr->origin_nature == j_ptr->origin_nature) &&
		 (o_ptr->origin_dlvl == j_ptr->origin_dlvl) && 
		 (o_ptr->origin_r_idx == j_ptr->origin_r_idx) &&
  		 (o_ptr->origin_s_idx == j_ptr->origin_s_idx) &&
  		 (o_ptr->origin_u_idx == j_ptr->origin_u_idx))
	{
		action = KEEP_O;
	}

	/* Allow yourself to lose "ego" monster names */
	else if ((o_ptr->origin_nature == j_ptr->origin_nature) &&
			 (o_ptr->origin_dlvl == j_ptr->origin_dlvl) && 
			 (o_ptr->origin_r_idx == j_ptr->origin_r_idx) &&
  			 (o_ptr->origin_u_idx == j_ptr->origin_u_idx))
	{
		action = LOSE_M_EGO;
	}

	/* If the first object has an exceptionally interesting history */
	else if ((o_ptr->origin_u_idx + INTEREST_OFFSET) ||
	    (o_ptr->origin_dlvl > k_info[o_ptr->k_idx].level))
	{
		/* Use the first item's history */
		action = KEEP_O;
	}

	/* If the second object has an exceptionally interesting history */
	else if ((j_ptr->origin_u_idx + INTEREST_OFFSET) ||
	    (j_ptr->origin_dlvl > k_info[j_ptr->k_idx].level))
	{
		/* Use the second item's history */
		action = KEEP_J;
	}

	/* Do it */
	switch (action)
	{
		case (KEEP_O):
		{
			/* Nothing to do */
			break;
		}
		case (KEEP_J):
		{
			/* Copy over the second object's history */
			o_ptr->origin_nature = j_ptr->origin_nature;
			o_ptr->origin_dlvl = j_ptr->origin_dlvl;
			o_ptr->origin_r_idx = j_ptr->origin_r_idx;
  			o_ptr->origin_s_idx = j_ptr->origin_s_idx;
  			o_ptr->origin_u_idx = j_ptr->origin_u_idx;
			break;
		}
		case (LOSE_M_EGO):
		{
			o_ptr->origin_s_idx = 0;
			break;
		}
		case (MIX): default:
		{
			o_ptr->origin_nature = ORIGIN_MIXED;
			break;
		}
	}
}

/*
 * Let the floor carry an object
 */
s16b floor_carry(int y, int x, const object_type *j_ptr)
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
void drop_near(const object_type *j_ptr, int chance, int y, int x, bool room_only)
{
	int i, k, d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;

	object_type *o_ptr;

	char o_name[80];

	bool flag = FALSE;

	bool plural = FALSE;

	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Describe object */
	object_desc(o_name, sizeof(o_name), j_ptr, FALSE, 0);

	/* Handle normal "breakage" */
	if (!j_ptr->a_idx && (rand_int(100) < chance))
	{
		/* Message */
		if (j_ptr->tval != TV_POWDER) 
			message_format(MSG_ITEM_BREAK, j_ptr->k_idx, "The %s disappear%s.",
		    o_name, (plural ? "" : "s"));

		/* Debug */
		if (cheat_wizard) message(MSG_CHEAT, 0, "Breakage (breakage).");

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

			/* If searching desks, skip grids outside the room */
			if ((room_only) && (!(cave_info[ty][tx] & (CAVE_ROOM)))) continue;

			/* Require line of sight */
			if (!los(y, x, ty, tx)) continue;

			/* Require floor space */
			if (cave_feat[ty][tx] != FEAT_FLOOR) continue;

			/* No objects */
			k = 0;

			/* Scan objects in that grid */
			for (o_ptr = get_first_object(ty, tx); o_ptr; o_ptr = get_next_object(o_ptr))
			{
				/* Check for possible combination */
				if (object_similar(o_ptr, j_ptr)) comb = TRUE;

				/* Count objects */
				k++;
			}

			/* Add new object */
			if (!comb) k++;

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
	if (!flag && !j_ptr->a_idx)
	{
		/* Message */
		message_format(MSG_ITEM_BREAK, j_ptr->k_idx, "The %s disappear%s.",
		           o_name, (plural ? "" : "s"));

		/* Debug */
		if (cheat_wizard) message(MSG_CHEAT, 0, "Breakage (no floor space).");

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
			ty = rand_int(p_ptr->cur_map_hgt);
			tx = rand_int(p_ptr->cur_map_wid);
		}

		/* Require floor space */
		if (cave_feat[ty][tx] != FEAT_FLOOR) continue;

		/* If searching desks, skip grids outside the room */
		if ((room_only) && (!(cave_info[ty][tx] & (CAVE_ROOM)))) continue;

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
		message_format(MSG_ITEM_BREAK, j_ptr->k_idx, "The %s disappear%s.",
		           o_name, (plural ? "" : "s"));

		/* Debug */
		if (cheat_wizard) message(MSG_CHEAT, 0, "Breakage (too many objects).");

		/* Hack -- Preserve artifacts */
		if (j_ptr->a_idx) a_info[j_ptr->a_idx].status &= ~A_STATUS_CREATED;

		/* Failure */
		return;
	}

	/* Sound */
	sound(MSG_DROP);

	/* Mega-Hack -- no message if "dropped" by player */
	/* Message when an object falls under the player */
	if (chance && (cave_m_idx[by][bx] < 0))
	{
		message(MSG_DROP, -1, "You feel something roll beneath your feet.");
	}
}

/*
 * Scatter some "great" objects near the player
 */
void acquirement(int y1, int x1, int num, bool great, bool real_depth)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Acquirement */
	while (num--)
	{
		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make a good (or great) object (if possible) */
		if (!make_object(i_ptr, TRUE, great, real_depth)) continue;

		/* Mark history */
		object_history(i_ptr, ORIGIN_ACQUIRE, 0, 0, 0);

		/* Drop the object */
		drop_near(i_ptr, -1, y1, x1, FALSE);
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
	if (make_object(i_ptr, good, great, TRUE))
	{
		/* Mark history */
		object_history(i_ptr, ORIGIN_FLOOR, 0, 0, 0);

		/* Give it to the floor */
		if (!floor_carry(y, x, i_ptr))
		{
			/* Hack -- Preserve artifacts */
			a_info[i_ptr->a_idx].status &= ~A_STATUS_CREATED;
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
	if (make_gold(i_ptr, 0))
	{
		/* Give it to the floor */
		(void)floor_carry(y, x, i_ptr);
	}
}

/*
 * Place a secret door at the given location
 */
void place_secret_door(int y, int x)
{

	/* Create secret door */
	cave_set_feat(y, x, FEAT_SECRET);

	/* Create a locked door, 1 in 4
	if (!rand_int(4)) place_lock(y, x, FALSE, WG_DOOR_LOCK); */
}

/*
 * Place a random type of door at the given location.
 */
void place_random_door(int y, int x)
{
	int tmp;

	/* Make sure there is no trap/lock here */
	delete_trap(y, x);
	
	/* Choose an object */
	tmp = rand_int(1000);

	/* Open doors (300/1000) */
	if (tmp < 400)
	{
		/* Create open door */
		cave_set_feat(y, x, FEAT_OPEN);
	}

	/* Closed doors (700/1000) */
	else
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_CLOSED);
	}
}

/*
 * Place a random type of closed door at the given location.
 */
void place_chest(int y, int x)
{
	/* Create closed door */
	cave_set_feat(y, x, FEAT_CHEST);

	place_trap_chest(y, x);
}

/*
 * Place a quest chest at the given location.
 */
void place_quest_chest(int y, int x)
{
	/* Create closed door */
	cave_set_feat(y, x, FEAT_QST_CHEST);

	place_trap_chest(y, x);
}

/*
 * Describe an alchemical formula (place string in buf)
 */
void alchemy_describe(char *buf, size_t max, int sval)
{
	int k;
	char o_name1[80];
	char o_name2[80];
	char o_name3[80];

	object_kind *k_ptr = &k_info[0];
	object_type *i_ptr;
	object_type object_type_body;

	/* Look for it */
	for (k = 1; k < z_info->k_max; k++)
	{
		k_ptr = &k_info[k];

		/* Found a match */
		if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == sval)) break;
	}

	/* Paranoia */
	if (k == z_info->k_max) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create fake object */
	object_prep(i_ptr, k);

	/* Describe the object */
	if (k_ptr->aware) strcpy(o_name1, (k_name + k_ptr->name));
	else object_desc(o_name1, sizeof(o_name1), i_ptr, FALSE, 0);

	/* Look for first component */
	if (potion_alch[sval].known1)
	{
		for (k = 1; k < z_info->k_max; k++)
		{
			k_ptr = &k_info[k];
			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[sval].sval1)) break;
		}

		/* Get local object */
		i_ptr = &object_type_body;

		/* Create fake object */
		object_prep(i_ptr, k);

		/* Describe the object */
		if (k_ptr->aware) strcpy(o_name2,(k_name + k_ptr->name));
		else object_desc(o_name2, sizeof(o_name2), i_ptr, FALSE, 0);
	}
	else
	{
		strcpy (o_name2,"????");
	}

	/* Look for first component */
	if (potion_alch[sval].known2)
	{
		/* Look for second component */
		for (k = 1; k < z_info->k_max; k++)
		{
			k_ptr = &k_info[k];

			/* Found a match */
			if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == potion_alch[sval].sval2)) break;
		}

		/* Get local object */
		i_ptr = &object_type_body;

		/* Create fake object */
		object_prep(i_ptr, k);

		/* Describe the object */
		if (k_ptr->aware) strcpy(o_name3,(k_name + k_ptr->name));
		else object_desc(o_name3, sizeof(o_name3), i_ptr, FALSE, 0);
	}
	else
	{
		strcpy (o_name3, "????");
	}

	/* Print a message */
	strnfmt(buf, max, "%s = %s + %s", o_name1, o_name2, o_name3);
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
	message_format(MSG_DESCRIBE, 0, "You have %d charge%s remaining.", o_ptr->pval, 
		(o_ptr->pval != 1) ? "s" : "");
}

/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
	object_type *o_ptr = &inventory[item];

	char o_name[80];

	if (o_ptr->a_idx && object_known_p(o_ptr))
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, (display_insc_msg ? 3 : 2));
 
		/* Print a message */
		message_format(MSG_DESCRIBE, 0,
			"You no longer have the %s (%c).", o_name, index_to_label(item));
	}
	else
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, (display_insc_msg ? 3 : 2));

		/* Print a message */
		message_format(MSG_DESCRIBE, 0, "You have %s (%c).", o_name, index_to_label(item));
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
		p_ptr->total_weight += (num * object_weight(o_ptr));

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

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

	/* Print a message */
	message_format(MSG_DESCRIBE, 0, "There are %d charge%s remaining.", o_ptr->pval,
		(o_ptr->pval != 1) ? "s" : "");
}

/*
 * Describe an item in the inventory.
 */
void floor_item_describe(int item)
{
	object_type *o_ptr = &o_list[item];

	char o_name[80];

	/* Get a description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Print a message */
	message_format(MSG_DESCRIBE, 0, "You see %s.", o_name);
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
s16b inven_carry(const object_type *o_ptr)
{
	int i, j, k;
	int n = -1;

	object_type *j_ptr;

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
			p_ptr->total_weight += (o_ptr->number * object_weight(o_ptr));

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
			if ((item_tester_hook_spellbooks(o_ptr)) && (!item_tester_hook_spellbooks(j_ptr))) break;
			if ((item_tester_hook_spellbooks(j_ptr)) && (!item_tester_hook_spellbooks(o_ptr))) continue;

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
			if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
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
				if (o_ptr->timeout > j_ptr->timeout) break;
				if (o_ptr->timeout < j_ptr->timeout) continue;
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
	p_ptr->total_weight += (j_ptr->number * object_weight(j_ptr));

	/* Count the items */
	p_ptr->inven_cnt++;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
 * Return the inventory slot into which the item is placed,
 * unless it's a light item.
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

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 3);

	/* Took off weapon */
	if (item == INVEN_WIELD) act = "You were wielding";

	/* Took off bow */
	else if (item == INVEN_BOW) act = "You were holding";

	/* Took off light */
	else if (item == INVEN_LITE)
	{
		act = "You throw away";

		/* Reset phlogiston */
		p_ptr->phlogiston = 0;
	}

	/* Took off something */
	else act = "You were wearing";

	/* Modify, Optimize */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

	/* Carry the object unless it's a light item */
	if (!(item == INVEN_LITE)) slot = inven_carry(i_ptr);
	else slot = 0;

	/* Message */
	message_format(MSG_DESCRIBE, 0, "%s %s (%c).", act, o_name, index_to_label(slot));

	/* Return slot */
	return (slot);
}

/*
 * Draw a magical circle
 */
void draw_circle(int y, int x, int type)
{
	int feature = 0;
	int i;

	/* Draw the Edges */
	switch(type)
	{
		case 1: feature = WG_CIRCLE_LIFE_EDGE_A; break;
		case 2: feature = WG_CIRCLE_ILLU_EDGE_A; break;
		case 3: feature = WG_CIRCLE_KNOW_EDGE_A; break;
		case 4: feature = WG_CIRCLE_PERM_EDGE_A; break;
		case 5: feature = WG_CIRCLE_RECALL_EDGE_A; break;
		case 6: feature = WG_CIRCLE_SUMMON_EDGE_A; break;
		case 7: feature = WG_CIRCLE_NEXUS_EDGE_A; break;
	}
	if (feature) place_decoration(y+1, x, feature);
	if (feature) place_decoration(y, x+1, feature);
	if (feature) place_decoration(y+2, x+5, feature);
	if (feature) place_decoration(y+3, x+4, feature);

	switch(type)
	{
		case 1: feature = WG_CIRCLE_LIFE_EDGE_B; break;
		case 2: feature = WG_CIRCLE_ILLU_EDGE_B; break;
		case 3: feature = WG_CIRCLE_KNOW_EDGE_B; break;
		case 4: feature = WG_CIRCLE_PERM_EDGE_B; break;
		case 5: feature = WG_CIRCLE_RECALL_EDGE_B; break;
		case 6: feature = WG_CIRCLE_SUMMON_EDGE_B; break;
		case 7: feature = WG_CIRCLE_NEXUS_EDGE_B; break;
	}
	if (feature) place_decoration(y+2, x, feature);
	if (feature) place_decoration(y+3, x+1, feature);
	if (feature) place_decoration(y+1, x+5, feature);
	if (feature) place_decoration(y, x+4, feature);

	/* Draw the inside */
	switch(type)
	{
		case 1: feature = WG_CIRCLE_OF_LIFEFORCE; break;
		case 2: feature = WG_CIRCLE_OF_ILLUSIONS; break;
		case 3: feature = WG_CIRCLE_OF_KNOWLEDGE; break;
		case 4: feature = WG_CIRCLE_OF_PERMANENCE; break;
		case 5: feature = WG_CIRCLE_OF_RECALL; break;
		case 6: feature = WG_CIRCLE_OF_SUMMONING; break;
		case 7: feature = WG_CIRCLE_OF_NEXUS; break;
	}
	if (feature) place_decoration(y, x+2, feature);
	if (feature) place_decoration(y, x+3, feature);
	if (feature) place_decoration(y+1, x+1, feature);
	if (feature) place_decoration(y+1, x+2, feature);
	if (feature) place_decoration(y+1, x+3, feature);
	if (feature) place_decoration(y+1, x+4, feature);
	if (feature) place_decoration(y+2, x+1, feature);
	if (feature) place_decoration(y+2, x+2, feature);
	if (feature) place_decoration(y+2, x+3, feature);
	if (feature) place_decoration(y+2, x+4, feature);
	if (feature) place_decoration(y+3, x+2, feature);
	if (feature) place_decoration(y+3, x+3, feature);

	/* Record the return coordinates to Circle of Recall */
	if (type == 5)
	{
		p_ptr->recall_y = y + 1;
		p_ptr->recall_x = x + 2;
	}

	/* Place monsters to Circle of Summoning */
	if (type == 6)
	{
		for (i = 0; i < randint(3) + randint(3); i++)
		{
			/* They are real, not mist-phantasms */
			summon_specific(y + 1 + rand_int(2), x + 2 + rand_int(2), p_ptr->depth + randint(3), 0, FALSE);
		}
	}

	/* Record the coordinates to Circle of Nexus */
	if (type == 7)
	{
		p_ptr->nexus_y = y + 1;
		p_ptr->nexus_x = x + 2;
	}
}

/*
 * Drop (some of) a non-cursed inventory/equipment item
 *
 * The object will be dropped "near" the current location
 */
void inven_drop(int item, int amt)
{
	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[80];

	int sacrifice_worthy = 0;
	int altar = 0;
	int blessings = 0;

	int inc = 0;
	int res = 0;
	int i;

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

		/* Get the original object */
		o_ptr = &inventory[item];
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

	/* For possible future use, roll ritual power points */
	int power = (p_stat(A_INT) + p_stat(A_WIS) - randint(20));
	if (power < 0) power = 0;
	if (cp_ptr->flags & CF_RITUAL_EXPERT) power = power / 5;
	else power = power / 10;

	/* Are we on an altar? */
	if ((t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx >= WG_ALTAR_OBSESSION) &&
		(t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx <= WG_ALTAR_DECEIT))
	{
		altar = t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx;

		/* Find out whether the item is sacrifice-worthy */
		if (cp_ptr->flags & CF_RELIGION_EXPERT)
		{
			if (o_ptr->tval == TV_LITE) sacrifice_worthy = 1;
			if ((o_ptr->e_idx) && (!(o_ptr->tval == TV_ARROW))) sacrifice_worthy = 2;
			if (o_ptr->a_idx) sacrifice_worthy = 2;
			if ((o_ptr->tval == TV_MUSIC) && (o_ptr->pval >= 2)) sacrifice_worthy = 2;
		}
		else if ((o_ptr->e_idx) && (!(o_ptr->tval == TV_ARROW))) sacrifice_worthy = 1;
		else if (o_ptr->a_idx) sacrifice_worthy = 2;

		if (p_ptr->obsession_status >= 2) blessings ++;
		if (p_ptr->conflict_status >= 2) blessings ++;
		if (p_ptr->purity_status >= 2) blessings ++;
		if (p_ptr->transformation_status >= 2) blessings ++;
		if (p_ptr->deceit_status >= 2) blessings ++;
	}

	int circle_type = 0;

	/* Are we dropping a powder vial? It's type determines possible Magic Circle types */
	if (o_ptr->tval == TV_POWDER)
	{
		switch (o_ptr->sval)
		{
			case SV_POWDER_SLEEP:
			{
				circle_type = 2;
				break;
			}
			case SV_POWDER_CONFUSE:
			{
				switch(randint(2))
				{
					case 1: circle_type = 2; break;
					case 2: circle_type = 7; break;
				}
				break;
			}
			case SV_POWDER_FLASH:
			{
				switch(randint(2))
				{
					case 1: circle_type = 1; break;
					case 2: circle_type = 3; break;
				}
				break;
			}
			case SV_POWDER_DARKNESS:
			{
				switch(randint(2))
				{
					case 1: circle_type = 6; break;
					case 2: circle_type = 7; break;
				}
				break;
			}
			case SV_POWDER_FIRE1:
			{
				circle_type = 3;
				break;
			}
			case SV_POWDER_FIRE2:
			{
				switch(randint(2))
				{
					case 1: circle_type = 3; break;
					case 2: circle_type = 4; break;
				}
				break;
			}
			case SV_POWDER_COLD1:
			{
				circle_type = 7;
				break;
			}
			case SV_POWDER_COLD2:
			{
				circle_type = 4;
				break;
			}
			case SV_POWDER_ENERGY:
			{
				circle_type = 5;
				break;
			}
			case SV_POWDER_POISON:
			{
				circle_type = 6;
				break;
			}
			case SV_POWDER_HEAL:
			{
				circle_type = 1;
				break;
			}
			case SV_POWDER_CALM:
			{
				switch(randint(2))
				{
					case 1: circle_type = 1; break;
					case 2: circle_type = 2; break;
				}
				break;
			}
			case SV_POWDER_POLYMORPH:
			{
				switch(randint(2))
				{
					case 1: circle_type = 5; break;
					case 2: circle_type = 6; break;
				}
				break;
			}
		}
	}

	/* Are we on a missing border of a magic circle, dropping a Powder Vial? */
	if ((t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_MISSING_EDGE_A) && (o_ptr->tval == TV_POWDER))
	{
		/* Calculate success chance */
		if (rand_int(100) < p_ptr->skill[SK_ALC])
		{
			message(MSG_GENERIC, 0, "You artfully complete the pattern. The circle starts to hum with power.");

			/* Find out the top left corner of the circle */
			/* Is the square on the right inside the circle? */
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_CIRCLE_BROKEN)
			{
				/* Is the square below this one inside the circle? */
				if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_CIRCLE_BROKEN)
				{
					draw_circle(p_ptr->py, p_ptr->px - 1, circle_type);
				}
				else
				{
					draw_circle(p_ptr->py - 1, p_ptr->px, circle_type);
				}
			}
			else
			{
				/* Is the square on top inside the circle? */
				if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_CIRCLE_BROKEN)
				{
					draw_circle(p_ptr->py - 3, p_ptr->px - 4, circle_type);
				}
				else
				{
					draw_circle(p_ptr->py - 2, p_ptr->px - 5, circle_type);
				}
			}

			/* An identification was made */
			if (!object_aware_p(o_ptr))
			{
				gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

				object_aware(o_ptr);

				/* Squelch */
				if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
			}
		}
		else
		{
			message(MSG_GENERIC, 0, "You could not comprehend the alchemical pattern. The powder was wasted.");
		}
	}

	else if ((t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_MISSING_EDGE_B) && (o_ptr->tval == TV_POWDER))
	{
		/* Calculate success chance */
		if (rand_int(100) < p_ptr->skill[SK_ALC])
		{
			message(MSG_GENERIC, 0, "You artfully complete the pattern. The circle starts to hum with power.");

			/* Find out the top left corner of the circle */
			/* Is the square on the right inside the circle? */
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_CIRCLE_BROKEN)
			{
				/* Is the square on top of this one inside the circle? */
				if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_CIRCLE_BROKEN)
				{
					draw_circle(p_ptr->py - 3, p_ptr->px - 1, circle_type);
				}
				else
				{
					draw_circle(p_ptr->py - 2, p_ptr->px, circle_type);
				}
			}
			else
			{
				/* Is the square below this one inside the circle? */
				if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_CIRCLE_BROKEN)
				{
					draw_circle(p_ptr->py, p_ptr->px - 4, circle_type);
				}
				else
				{
					draw_circle(p_ptr->py - 1, p_ptr->px - 5, circle_type);
				}
			}

			/* An identification was made */
			if (!object_aware_p(o_ptr))
			{
				gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);

				object_aware(o_ptr);

				/* Squelch */
				if (squelch_itemp(o_ptr)) do_squelch_item(o_ptr);
			}
		}
		else
		{
			message(MSG_GENERIC, 0, "You could not comprehend the alchemical pattern. The powder was wasted.");
		}
	}

	/* Is the item sacrifice-worthy and dropped on an altar? */
	else if ((altar > 0) && (sacrifice_worthy >= 1))
	{
		message_format(MSG_DROP, 0, "A brilliant light consumes %s (%c).", o_name, index_to_label(item));

		/* Dropped a non-arrow ego item (or a food item if the player is a Priest) */
		if (sacrifice_worthy == 1)
		{
			if (altar == WG_ALTAR_OBSESSION)
			{
				if (p_ptr->transformation_status >= 4)
				{
					p_ptr->transformation_status = 7;
					message(MSG_GENERIC, 0, "Cyrridven gets angry!");
				}
				else if (p_ptr->purity_status >= 4)
				{
					p_ptr->purity_status = 7;
					message(MSG_GENERIC, 0, "Eostre gets angry!");
				}

				if (p_ptr->obsession_status >= 4)
				{
					p_ptr->obsession_status = 5;
					message(MSG_GENERIC, 0, "Beleth, the Queen of Hell, is very pleased with you.");
				}
				else if (p_ptr->obsession_status <= 3)
				{
					p_ptr->obsession_status = 2;
					message(MSG_GENERIC, 0, "Beleth, the Queen of Hell, blesses you.");

					/* More than three blessings? One enemy goddess draws away her blessings */
					if (blessings >= 3)
					{
						if ((p_ptr->purity_status > 1) && (p_ptr->transformation_status > 1))
						{
							if (rand_int(100) < 50)
							{
								message(MSG_GENERIC, 0, "Eostre draws away her blessing.");
								p_ptr->purity_status = 1;
							}
							else
							{
								message(MSG_GENERIC, 0, "Cyrridven draws away her blessing.");
								p_ptr->transformation_status = 1;
							}
						}
						else if (p_ptr->purity_status > 1)
						{
							message(MSG_GENERIC, 0, "Eostre draws away her blessing.");
							p_ptr->purity_status = 1;
						}
						else if (p_ptr->transformation_status > 1)
						{
							message(MSG_GENERIC, 0, "Cyrridven draws away her blessing.");
							p_ptr->transformation_status = 1;
						}
					}
				}
			}
			else if (altar == WG_ALTAR_CONFLICT)
			{
				if (p_ptr->deceit_status >= 4)
				{
					p_ptr->deceit_status = 7;
					message(MSG_GENERIC, 0, "Laverna gets angry!");
				}
				else if (p_ptr->transformation_status >= 4)
				{
					p_ptr->transformation_status = 7;
					message(MSG_GENERIC, 0, "Cyrridven gets angry!");
				}

				if (p_ptr->conflict_status >= 4)
				{
					p_ptr->conflict_status = 5;
					message(MSG_GENERIC, 0, "Discordia, the Warrior, is very pleased with you.");
				}
				else if (p_ptr->conflict_status <= 3)
				{
					p_ptr->conflict_status = 2;
					message(MSG_GENERIC, 0, "Discordia, the Warrior, blesses you.");

					/* More than three blessings? One enemy goddess draws away her blessings */
					if (blessings >= 3)
					{
						if ((p_ptr->transformation_status > 1) && (p_ptr->deceit_status > 1))
						{
							if (rand_int(100) < 50)
							{
								message(MSG_GENERIC, 0, "Cyrridven draws away her blessing.");
								p_ptr->transformation_status = 1;
							}
							else
							{
								message(MSG_GENERIC, 0, "Laverna draws away her blessing.");
								p_ptr->deceit_status = 1;
							}
						}
						else if (p_ptr->transformation_status > 1)
						{
							message(MSG_GENERIC, 0, "Cyrridven draws away her blessing.");
							p_ptr->transformation_status = 1;
						}
						else if (p_ptr->deceit_status > 1)
						{
							message(MSG_GENERIC, 0, "Laverna draws away her blessing.");
							p_ptr->deceit_status = 1;
						}
					}
				}
			}
			else if (altar == WG_ALTAR_PURITY)
			{
				if (p_ptr->deceit_status >= 4)
				{
					p_ptr->deceit_status = 7;
					message(MSG_GENERIC, 0, "Laverna gets angry!");
				}
				else if (p_ptr->obsession_status >= 4)
				{
					p_ptr->obsession_status = 7;
					message(MSG_GENERIC, 0, "Beleth gets angry!");
				}

				if (p_ptr->purity_status >= 4)
				{
					p_ptr->purity_status = 5;
					message(MSG_GENERIC, 0, "Eostre, the Maiden of Spring, is very pleased with you.");
				}
				else if (p_ptr->purity_status <= 3)
				{
					p_ptr->purity_status = 2;
					message(MSG_GENERIC, 0, "Eostre, the Maiden of Spring, blesses you.");

					/* More than three blessings? One enemy goddess draws away her blessings */
					if (blessings >= 3)
					{
						if ((p_ptr->obsession_status > 1) && (p_ptr->deceit_status > 1))
						{
							if (rand_int(100) < 50)
							{
								message(MSG_GENERIC, 0, "Beleth draws away her blessing.");
								p_ptr->obsession_status = 1;
							}
							else
							{
								message(MSG_GENERIC, 0, "Laverna draws away her blessing.");
								p_ptr->deceit_status = 1;
							}
						}
						else if (p_ptr->obsession_status > 1)
						{
							message(MSG_GENERIC, 0, "Beleth draws away her blessing.");
							p_ptr->obsession_status = 1;
						}
						else if (p_ptr->deceit_status > 1)
						{
							message(MSG_GENERIC, 0, "Laverna draws away her blessing.");
							p_ptr->deceit_status = 1;
						}
					}
				}
			}
			else if (altar == WG_ALTAR_TRANSFORMATION)
			{
				if (p_ptr->obsession_status >= 4)
				{
					p_ptr->obsession_status = 7;
					message(MSG_GENERIC, 0, "Beleth gets angry!");
				}
				else if (p_ptr->conflict_status >= 4)
				{
					p_ptr->conflict_status = 7;
					message(MSG_GENERIC, 0, "Discordia gets angry!");
				}

				if (p_ptr->transformation_status >= 4)
				{
					p_ptr->transformation_status = 5;
					message(MSG_GENERIC, 0, "Cyrridven, the Crone, is very pleased with you.");
				}
				else if (p_ptr->transformation_status <= 3)
				{
					p_ptr->transformation_status = 2;
					message(MSG_GENERIC, 0, "Cyrridven, the Crone, blesses you.");

					/* More than three blessings? One enemy goddess draws away her blessings */
					if (blessings >= 3)
					{
						if ((p_ptr->obsession_status > 1) && (p_ptr->conflict_status > 1))
						{
							if (rand_int(100) < 50)
							{
								message(MSG_GENERIC, 0, "Beleth draws away her blessing.");
								p_ptr->obsession_status = 1;
							}
							else
							{
								message(MSG_GENERIC, 0, "Discordia draws away her blessing.");
								p_ptr->conflict_status = 1;
							}
						}
						else if (p_ptr->obsession_status > 1)
						{
							message(MSG_GENERIC, 0, "Beleth draws away her blessing.");
							p_ptr->obsession_status = 1;
						}
						else if (p_ptr->conflict_status > 1)
						{
							message(MSG_GENERIC, 0, "Discordia draws away her blessing.");
							p_ptr->conflict_status = 1;
						}
					}
				}
			}
			else if (altar == WG_ALTAR_DECEIT)
			{
				if (p_ptr->conflict_status >= 4)
				{
					p_ptr->conflict_status = 7;
					message(MSG_GENERIC, 0, "Discordia gets angry!");
				}
				else if (p_ptr->purity_status >= 4)
				{
					p_ptr->purity_status = 7;
					message(MSG_GENERIC, 0, "Eostre gets angry!");
				}

				if (p_ptr->deceit_status >= 4)
				{
					p_ptr->deceit_status = 5;
					message(MSG_GENERIC, 0, "Laverna, the Mistress of the Underworld, is very pleased with you.");
				}
				else if (p_ptr->deceit_status <= 3)
				{
					p_ptr->deceit_status = 2;
					message(MSG_GENERIC, 0, "Laverna, the Mistress of the Underworld, blesses you.");

					/* More than three blessings? One enemy goddess draws away her blessings */
					if (blessings >= 3)
					{
						if ((p_ptr->conflict_status > 1) && (p_ptr->purity_status > 1))
						{
							if (rand_int(100) < 50)
							{
								message(MSG_GENERIC, 0, "Discordia draws away her blessing.");
								p_ptr->conflict_status = 1;
							}
							else
							{
								message(MSG_GENERIC, 0, "Eostre draws away her blessing.");
								p_ptr->purity_status = 1;
							}
						}
						else if (p_ptr->conflict_status > 1)
						{
							message(MSG_GENERIC, 0, "Discordia draws away her blessing.");
							p_ptr->conflict_status = 1;
						}
						else if (p_ptr->purity_status > 1)
						{
							message(MSG_GENERIC, 0, "Eostre draws away her blessing.");
							p_ptr->purity_status = 1;
						}
					}
				}
			}
		}
		else if (sacrifice_worthy == 2)
		/* Dropped an artifact (or an ego item if the player is a Priest) */
		{
			if (altar == WG_ALTAR_OBSESSION)
			{
				if (p_ptr->obsession_status >= 7)
				{
					p_ptr->obsession_status = 4;
					message(MSG_GENERIC, 0, "Beleth, the Queen of Hell, is no longer angry with you.");
				}
				else if (p_ptr->obsession_status == 4)
				{
					p_ptr->obsession_status = 5;
					message(MSG_GENERIC, 0, "Beleth, the Queen of Hell, is very pleased with you!");
				}
				else if (p_ptr->obsession_status < 4)
				{
					p_ptr->obsession_status = 4;
					message(MSG_GENERIC, 0, "You are now a follower of Beleth, the Queen of Hell.");

					if ((p_ptr->conflict_status >= 4) && (p_ptr->conflict_status <= 6))
					{
						p_ptr->conflict_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Discordia, the Warrior.");
					}
					if ((p_ptr->deceit_status >= 4) && (p_ptr->deceit_status <= 6))
					{
						p_ptr->deceit_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Laverna, the Mistress of the Underworld.");
					}
					if (p_ptr->conflict_status >= 7)
					{
						p_ptr->conflict_status = 1;
						message(MSG_GENERIC, 0, "Discordia is happy to get rid of you.");
					}
					if (p_ptr->deceit_status >= 7)
					{
						p_ptr->deceit_status = 1;
						message(MSG_GENERIC, 0, "Laverna is happy to get rid of you.");
					}
					if (p_ptr->purity_status > 3) 
					{
						p_ptr->purity_status = 1;
						p_ptr->obsession_status = 7;
						message(MSG_GENERIC, 0, "Beleth doesn't like an ex-follower of Eostre at all.");
					}
					if (p_ptr->transformation_status > 3) 
					{
						p_ptr->transformation_status = 1;
						p_ptr->obsession_status = 7;
						message(MSG_GENERIC, 0, "Beleth doesn't like an ex-follower of Cyrridven at all.");
					}
					if (p_ptr->purity_status > 1) 
					{
						p_ptr->purity_status = 1;
						message(MSG_GENERIC, 0, "Eostre withdraws her blessings.");
					}
					if (p_ptr->transformation_status > 1) 
					{
						p_ptr->transformation_status = 1;
						message(MSG_GENERIC, 0, "Cyrridven withdraws her blessings.");
					}
				}
			}

			else if (altar == WG_ALTAR_CONFLICT)
			{
				if (p_ptr->conflict_status >= 7)
				{
					p_ptr->conflict_status = 4;
					message(MSG_GENERIC, 0, "Discordia, the Warrior, is no longer angry with you.");
				}
				else if (p_ptr->conflict_status == 4)
				{
					p_ptr->conflict_status = 5;
					message(MSG_GENERIC, 0, "Discordia, the Warrior, is very pleased with you!");
				}
				else if (p_ptr->conflict_status < 4)
				{
					p_ptr->conflict_status = 4;
					message(MSG_GENERIC, 0, "You are now a follower of Discordia, the Warrior.");

					if ((p_ptr->obsession_status >= 4) && (p_ptr->obsession_status <= 6))
					{
						p_ptr->obsession_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Beleth, the Queen of Hell.");
					}
					if ((p_ptr->purity_status >= 4) && (p_ptr->purity_status <= 6))
					{
						p_ptr->purity_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Eostre, the Maiden of Spring.");
					}
					if (p_ptr->obsession_status >= 7)
					{
						p_ptr->obsession_status = 1;
						message(MSG_GENERIC, 0, "Beleth is happy to get rid of you.");
					}
					if (p_ptr->purity_status >= 7)
					{
						p_ptr->purity_status = 1;
						message(MSG_GENERIC, 0, "Eostre is happy to get rid of you.");
					}
					if (p_ptr->deceit_status > 3) 
					{
						p_ptr->deceit_status = 1;
						p_ptr->conflict_status = 7;
						message(MSG_GENERIC, 0, "Discordia doesn't like an ex-follower of Laverna at all.");
					}
					if (p_ptr->transformation_status > 3) 
					{
						p_ptr->transformation_status = 1;
						p_ptr->conflict_status = 7;
						message(MSG_GENERIC, 0, "Discordia doesn't like an ex-follower of Cyrridven at all.");
					}
					if (p_ptr->deceit_status > 1) 
					{
						p_ptr->deceit_status = 1;
						message(MSG_GENERIC, 0, "Laverna withdraws her blessings.");
					}
					if (p_ptr->transformation_status > 1) 
					{
						p_ptr->transformation_status = 1;
						message(MSG_GENERIC, 0, "Cyrridven withdraws her blessings.");
					}
				}

			}
			else if (altar == WG_ALTAR_PURITY)
			{
				if (p_ptr->purity_status >= 7)
				{
					p_ptr->purity_status = 4;
					message(MSG_GENERIC, 0, "Eostre, the Maiden of Spring, is no longer angry with you.");
				}
				else if (p_ptr->purity_status == 4)
				{
					p_ptr->purity_status = 5;
					message(MSG_GENERIC, 0, "Eostre, the Maiden of Spring, is very pleased with you!");
				}
				else if (p_ptr->purity_status < 4)
				{
					p_ptr->purity_status = 4;
					message(MSG_GENERIC, 0, "You are now a follower of Eostre, the Maiden of Spring.");

					if ((p_ptr->conflict_status >= 4) && (p_ptr->conflict_status <= 6))
					{
						p_ptr->conflict_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Discordia, the Warrior.");
					}
					if ((p_ptr->transformation_status >= 4) && (p_ptr->transformation_status <= 6))
					{
						p_ptr->transformation_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Cyrridven, the Crone.");
					}
					if (p_ptr->conflict_status >= 7)
					{
						p_ptr->conflict_status = 1;
						message(MSG_GENERIC, 0, "Discordia is happy to get rid of you.");
					}
					if (p_ptr->transformation_status >= 7)
					{
						p_ptr->transformation_status = 1;
						message(MSG_GENERIC, 0, "Cyrridven is happy to get rid of you.");
					}
					if (p_ptr->deceit_status > 3) 
					{
						p_ptr->deceit_status = 1;
						p_ptr->purity_status = 7;
						message(MSG_GENERIC, 0, "Eostre doesn't like an ex-follower of Laverna at all.");
					}
					if (p_ptr->obsession_status > 3) 
					{
						p_ptr->obsession_status = 1;
						p_ptr->purity_status = 7;
						message(MSG_GENERIC, 0, "Eostre doesn't like an ex-follower of Beleth at all.");
					}
					if (p_ptr->deceit_status > 1) 
					{
						p_ptr->deceit_status = 1;
						message(MSG_GENERIC, 0, "Laverna withdraws her blessings.");
					}
					if (p_ptr->obsession_status > 1) 
					{
						p_ptr->obsession_status = 1;
						message(MSG_GENERIC, 0, "Beleth withdraws her blessings.");
					}
				}
			}
			else if (altar == WG_ALTAR_TRANSFORMATION)
			{
				if (p_ptr->transformation_status >= 7)
				{
					p_ptr->transformation_status = 4;
					message(MSG_GENERIC, 0, "Cyrridven, the Crone, is no longer angry with you.");
				}
				else if (p_ptr->transformation_status == 4)
				{
					p_ptr->transformation_status = 5;
					message(MSG_GENERIC, 0, "Cyrridven, the Crone, is very pleased with you!");
				}
				else if (p_ptr->transformation_status < 4)
				{
					p_ptr->transformation_status = 4;
					message(MSG_GENERIC, 0, "You are now a follower of Cyrridven, the Crone.");

					if ((p_ptr->purity_status >= 4) && (p_ptr->purity_status <= 6))
					{
						p_ptr->purity_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Eostre, the Maiden of Spring.");
					}
					if ((p_ptr->deceit_status >= 4) && (p_ptr->deceit_status <= 6))
					{
						p_ptr->deceit_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Laverna, the Mistress of the Underworld.");
					}
					if (p_ptr->purity_status >= 7)
					{
						p_ptr->purity_status = 1;
						message(MSG_GENERIC, 0, "Eostre is happy to get rid of you.");
					}
					if (p_ptr->deceit_status >= 7)
					{
						p_ptr->deceit_status = 1;
						message(MSG_GENERIC, 0, "Laverna is happy to get rid of you.");
					}
					if (p_ptr->obsession_status > 3) 
					{
						p_ptr->obsession_status = 1;
						p_ptr->transformation_status = 7;
						message(MSG_GENERIC, 0, "Cyrridven doesn't like an ex-follower of Beleth at all.");
					}
					if (p_ptr->conflict_status > 3) 
					{
						p_ptr->conflict_status = 1;
						p_ptr->transformation_status = 7;
						message(MSG_GENERIC, 0, "Cyrridven doesn't like an ex-follower of Discordia at all.");
					}
					if (p_ptr->obsession_status > 1) 
					{
						p_ptr->obsession_status = 1;
						message(MSG_GENERIC, 0, "Beleth withdraws her blessings.");
					}
					if (p_ptr->conflict_status > 1) 
					{
						p_ptr->conflict_status = 1;
						message(MSG_GENERIC, 0, "Discordia withdraws her blessings.");
					}
				}

			}
			else if (altar == WG_ALTAR_DECEIT)
			{
				if (p_ptr->deceit_status >= 7)
				{
					p_ptr->deceit_status = 4;
					message(MSG_GENERIC, 0, "Laverna, the Mistress of the Underworld, is no longer angry with you.");
				}
				else if (p_ptr->deceit_status == 4)
				{
					p_ptr->deceit_status = 5;
					message(MSG_GENERIC, 0, "Laverna, the Mistress of the Underworld, is very pleased with you!");
				}
				else if (p_ptr->deceit_status < 4)
				{
					p_ptr->deceit_status = 4;
					message(MSG_GENERIC, 0, "You are now a follower of Laverna, the Mistress of the Underworld.");

					if ((p_ptr->obsession_status >= 4) && (p_ptr->obsession_status <= 6))
					{
						p_ptr->obsession_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Beleth, the Queen of Hell.");
					}
					if ((p_ptr->transformation_status >= 3) && (p_ptr->transformation_status <= 6))
					{
						p_ptr->transformation_status = 2;
						message(MSG_GENERIC, 0, "You are still blessed by Cyrridven, the Crone.");
					}
					if (p_ptr->obsession_status >= 7)
					{
						p_ptr->obsession_status = 1;
						message(MSG_GENERIC, 0, "Beleth is happy to get rid of you.");
					}
					if (p_ptr->transformation_status >= 7)
					{
						p_ptr->transformation_status = 1;
						message(MSG_GENERIC, 0, "Cyrridven is happy to get rid of you.");
					}
					if (p_ptr->purity_status > 3) 
					{
						p_ptr->purity_status = 1;
						p_ptr->deceit_status = 7;
						message(MSG_GENERIC, 0, "Laverna doesn't like an ex-follower of Eostre at all.");
					}
					if (p_ptr->conflict_status > 3) 
					{
						p_ptr->conflict_status = 1;
						p_ptr->deceit_status = 7;
						message(MSG_GENERIC, 0, "Laverna doesn't like an ex-follower of Discordia at all.");
					}
					if (p_ptr->purity_status > 1) 
					{
						p_ptr->purity_status = 1;
						message(MSG_GENERIC, 0, "Eostre withdraws her blessings.");
					}
					if (p_ptr->conflict_status > 1) 
					{
						p_ptr->conflict_status = 1;
						message(MSG_GENERIC, 0, "Discordia withdraws her blessings.");
					}
				}
			}
		}
	}

	/*
	 * Are we dropping a dungeon book on a circle of the right type to cast a ritual, and have at least one point of *Lore*?
	 */

	/* Resistance of Scarabtarices: Fortification */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MAGE7) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_LIFEFORCE))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			message(MSG_EFFECT, 0, "Your skin tingles. You feel more secure against hostile forces.");

			p_ptr->fortification += power * 3;
		}
	}
	/* Mordenkainen's Escapes: Dexterity */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MAGE8) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_NEXUS))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			res = res_stat(A_DEX);

			for (i = 0; i < power; i++)
			{
				inc = inc_stat(A_DEX);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (inc) message(MSG_EFFECT, 0, "You feel very dextrous!");
			else if (res) message(MSG_EFFECT, 0, "You feel less clumsy.");
		}
	}
	/* Kelek's Grimoire of Power: Strength */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MAGE9) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			res = res_stat(A_STR);

			for (i = 0; i < power; i++)
			{
				inc = inc_stat(A_STR);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (inc) message(MSG_EFFECT, 0, "You feel very strong!");
			else if (res) message(MSG_EFFECT, 0, "You feel less weak.");
		}
	}
	/* Raal's Tome of Destruction: Create Powder */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MAGE10) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_NEXUS))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			object_type *i_ptr;
			object_type object_type_body;
			bool object_created = FALSE;

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");

			object_level = 50;

			for (i = 0; i < power * 10; i++)
			{
				object_created = FALSE;

				while (object_created == FALSE)
				{
					/* Get local object */
					i_ptr = &object_type_body;

					/* Wipe the object */
					object_wipe(i_ptr);

					/* Make a themed object (if possible) */
					if (make_typed(i_ptr, TV_POWDER, FALSE, FALSE, FALSE))
					{
						/* Mark history */
						object_history(i_ptr, ORIGIN_ACQUIRE, 0, 0, 0);

						/* Drop the object */
						drop_near(i_ptr, -1, p_ptr->py + rand_int(5) - 2, p_ptr->px + rand_int(5) - 2, FALSE);

						/* Done */
						object_created = TRUE;
					}
				}
			}

			object_level = p_ptr->depth;
		}
	}
	/* Tenser's Transformations: Augment Body */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MAGE11) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			for (i = 0; i < power; i++)
			{
				res_stat(A_STR);
				inc_stat(A_STR);
				res_stat(A_DEX);
				inc_stat(A_DEX);
				res_stat(A_CON);
				inc_stat(A_CON);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			message(MSG_EFFECT, 0, "Your feel stronger, quicker, tougher.");
		}
	}
	/* The Lore of the Hunter: Wisdom */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_RANGER) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_RECALL))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			res = res_stat(A_WIS);

			for (i = 0; i < power; i++)
			{
				inc = inc_stat(A_WIS);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (inc) message(MSG_EFFECT, 0, "You feel very wise!");
			else if (res) message(MSG_EFFECT, 0, "You feel less naive.");
		}
	}
	/* Ethereal Openings: Charisma */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_PRIEST5) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_ILLUSIONS))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			res = res_stat(A_CHR);

			for (i = 0; i < power; i++)
			{
				inc = inc_stat(A_CHR);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (inc) message(MSG_EFFECT, 0, "You feel very cute!");
			else if (res) message(MSG_EFFECT, 0, "You feel less ugly.");
		}
	}
	/* Godly Insights: Night Sight */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_PRIEST6) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_ILLUSIONS))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			message(MSG_EFFECT, 0, "Your eyes begin to tingle!");

			p_ptr->nightsight += power;
		}
	}
	/* Purifications and Healing: Cure Wound */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_PRIEST7) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_LIFEFORCE))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");

			for (i = 0; i < power; i++)
			{
				if (p_ptr->wound_vigor)
				{
					p_ptr->wound_vigor = 0;
					message(MSG_EFFECT, 0, "Your internal wound is cured!");
					continue;
				}
				else if (p_ptr->wound_wit)
				{
					p_ptr->wound_wit = 0;
					message(MSG_EFFECT, 0, "Your brain damage is cured!");
					continue;
				}
				else if (p_ptr->wound_grace)
				{
					p_ptr->wound_grace = 0;
					message(MSG_EFFECT, 0, "You are able to straighten up once again!");
					continue;
				}
				else if (i == 0)
				{
					message(MSG_EFFECT, 0, "Nothing happens.");
				}
			}
		}
	}
	/* Holy Infusions: Acquire a Divine Armour */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_PRIEST8) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_SUMMONING))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			object_type *i_ptr;
			object_type object_type_body;
			bool object_created = FALSE;

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (power > 1) message(MSG_EFFECT, 0, "Objects materialize!");
			else message(MSG_EFFECT, 0, "An object materializes!");

			object_level = p_ptr->depth + 15;

			for (i = 0; i < power; i++)
			{
				object_created = FALSE;

				while (object_created == FALSE)
				{
					/* Get local object */
					i_ptr = &object_type_body;

					/* Wipe the object */
					object_wipe(i_ptr);

					/* Make a themed object (if possible) */
					if (make_typed(i_ptr, TV_BOOTS + (rand_int(7)), TRUE, TRUE, FALSE))
					{
						/* Mark history */
						object_history(i_ptr, ORIGIN_ACQUIRE, 0, 0, 0);

						/* Drop the object */
						drop_near(i_ptr, -1, p_ptr->py + rand_int(3) - 1, p_ptr->px + rand_int(3) - 1, FALSE);

						/* Done */
						object_created = TRUE;
					}
				}
			}

			object_level = p_ptr->depth;
		}
	}
	/* Wrath of God: Acquire a Divine Weapon */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_PRIEST9) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_SUMMONING))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			object_type *i_ptr;
			object_type object_type_body;
			bool object_created = FALSE;

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (power > 1) message(MSG_EFFECT, 0, "Objects materialize!");
			else message(MSG_EFFECT, 0, "An object materializes!");

			object_level = p_ptr->depth + 15;

			for (i = 0; i < power; i++)
			{
				object_created = FALSE;

				while (object_created == FALSE)
				{
					/* Get local object */
					i_ptr = &object_type_body;

					/* Wipe the object */
					object_wipe(i_ptr);

					/* Make a themed object (if possible) */
					if (make_typed(i_ptr, TV_BOW + (rand_int(4)), TRUE, TRUE, FALSE))
					{
						/* Mark history */
						object_history(i_ptr, ORIGIN_ACQUIRE, 0, 0, 0);

						/* Drop the object */
						drop_near(i_ptr, -1, p_ptr->py + rand_int(3) - 1, p_ptr->px + rand_int(3) - 1, FALSE);

						/* Done */
						object_created = TRUE;
					}
				}
			}

			object_level = p_ptr->depth;
		}
	}
	/* The Seven Steps to Transcendence: Constitution */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MYSTIC1) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_LIFEFORCE))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			res = res_stat(A_CON);

			for (i = 0; i < power; i++)
			{
				inc = inc_stat(A_CON);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (inc) message(MSG_EFFECT, 0, "You feel very healthy!");
			else if (res) message(MSG_EFFECT, 0, "You feel less sickly.");
		}
	}
	/* Teachings of the Ninth Master: Intelligence */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MYSTIC2) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_RECALL))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			res = res_stat(A_INT);

			for (i = 0; i < power; i++)
			{
				inc = inc_stat(A_INT);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			if (inc) message(MSG_EFFECT, 0, "You feel very smart!");
			else if (res) message(MSG_EFFECT, 0, "You feel less stupid.");
		}
	}
	/* Necronomicon: Forbidden Lore */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_NECRONOMICON) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_KNOWLEDGE))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			message(MSG_EFFECT, 0, "You feel more experienced.");

			for (i = 0; i < power; i++)
			{
				if (p_ptr->exp < PY_MAX_EXP)
				{
					s32b ee = ((p_ptr->exp / 2) + 10);
					if (ee > 1000000L) ee = 1000000L;
					gain_exp(ee);
				}
			}
		}
	}
	/* Mathemagical Calculations: Mind over Body */
	else if ((o_ptr->tval == TV_MAGIC_BOOK) && (o_ptr->sval == SV_BOOK_MATHEMAGIC) && (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_KNOWLEDGE))
	{
		if (power == 0)
		{
			message(MSG_EFFECT, 0, "You perform the ritual without fully understanding it. Nothing happens.");

		}
		else
		{
			for (i = 0; i < power; i++)
			{
				res_stat(A_INT);
				inc_stat(A_INT);
				inc_stat(A_INT);
				res_stat(A_WIS);
				inc_stat(A_WIS);
				inc_stat(A_WIS);
				res_stat(A_CHR);
				inc_stat(A_CHR);
				inc_stat(A_CHR);

				dec_stat(A_STR, 2, TRUE);
				dec_stat(A_DEX, 2, TRUE);
				dec_stat(A_CON, 2, TRUE);
			}

			message(MSG_EFFECT, 0, "With a ceremonial voice, you read aloud the ritual.");
			message(MSG_EFFECT, 0, "Your brain grows new connections. You feel your body wither.");
		}
	}

	/* Nothing special, drop normally */
	else
	{
		/* Message */
		message_format(MSG_DROP, 0, "You drop %s (%c).", o_name, index_to_label(item));

		/* Drop it near the player */
		drop_near(i_ptr, 0, p_ptr->py, p_ptr->px, FALSE);
	}

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
	if (flag) message(MSG_GENERIC, 0, "You combine some items in your pack.");
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
			if ((item_tester_hook_spellbooks(o_ptr)) && (!item_tester_hook_spellbooks(j_ptr))) break;
			if ((item_tester_hook_spellbooks(j_ptr)) && (!item_tester_hook_spellbooks(o_ptr))) continue;

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
			if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
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
				if (o_ptr->timeout > j_ptr->timeout) break;
				if (o_ptr->timeout < j_ptr->timeout) continue;
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
	if (flag) message(MSG_GENERIC, 0, "You reorder some items in your pack.");
}

/*
 * Hack -- Create a "forged" artifact, for knowledge purposes
 */
bool make_fake_artifact(object_type *o_ptr, int a_idx)
{
	int i;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return FALSE;

	/* Get the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (FALSE);

	/* Create the artifact */
	object_prep(o_ptr, i);

	/* Save the name */
	o_ptr->a_idx = a_idx;

	/* Extract the fields */
	o_ptr->pval = a_ptr->pval;
	o_ptr->to_a = a_ptr->to_a;
	o_ptr->to_h = a_ptr->to_h;
	o_ptr->pfx_idx = a_ptr->prefix_idx;

	/* It's fully know */
	o_ptr->ident |= IDENT_KNOWN;

	/* Success */
	return (TRUE);
}

/*
 * Create a quest item
 */
void create_quest_item(int ny, int nx)
{
	object_type *i_ptr;
	object_type object_type_body;
	int k_idx;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Acquire the "kind" index */
	k_idx = lookup_kind(TV_QUEST, 0);

	/* Oops */
	if (!k_idx) return;

	/* Create the artifact */
	object_prep(i_ptr, k_idx);

	/* Save the name */
	i_ptr->to_h = (byte)rand_int(QUEST_NAME_1);
	i_ptr->to_a = (byte)rand_int(QUEST_NAME_2);
	i_ptr->pfx_idx = (byte)rand_int(QUEST_NAME_3);

	/* Identify it */
	object_known(i_ptr);

	/* Mark history */
	object_history(i_ptr, ORIGIN_CHEST, 0, 0, 0);
		
	/* Drop the artifact from heaven */
	drop_near(i_ptr, -1, ny, nx, FALSE);
}
