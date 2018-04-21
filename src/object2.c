/* File: object2.c */

/* Purpose: Object code, part 2 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * James E. Wilson and Robert A. Koeneke have released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version),
 * or under the terms of the traditional Angband license.
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2),
 * or under the terms of the traditional Angband license.
 */

#include "angband.h"

extern void random_resistance (object_type * o_ptr, bool is_scroll, int specific);



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
	int i, y, x, num, cnt;

	int cur_lev, cur_dis, chance;

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

	/* Compact */
	if (size)
	{
		/* Message */
		msg_print("Compacting objects...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
	}else
	{
		/* We dont need to prune */
		return;
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

			/* Hack -- only compact artefacts in emergencies */
			if ((artefact_p(o_ptr) || o_ptr->art_name)
				&& (cnt < 1000)) chance = 100;

			/* Apply the saving throw */
			if (rand_int(100) < chance) continue;

			/* Count it, not sure this is a great idea ;)
			   I, konijn, will take the blame on this one
			*/
			num = num + o_ptr->number;

			/* Delete the object */
			delete_object_idx(i);
		}
	}

	/* Reorder objects */
	compact_objects(0);
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

		/* Mega-Hack -- preserve artefacts */
		if (!character_dungeon || preserve_mode)
		{
			/* Hack -- Preserve unknown artefacts */
			if (artefact_p(o_ptr))
			{
				/* Mega-Hack -- Preserve the artefact */
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
*
* This routine also removes any inscriptions generated by "feelings".
*/
void object_known(object_type *o_ptr , bool squelch )
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
			(streq(q, "great")) ||
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

	if(squelch)
		consider_squelch( o_ptr );
}

/*
 * The player has done a *id*, giving full awareness
 */
void object_storebought(object_type *o_ptr)
{
	/* We have "felt" it in every way */
	o_ptr->ident |= (IDENT_STOREB);
	object_aware(o_ptr);
	object_known(o_ptr,FALSE);
}

/*
* The player is now aware of the effects of the given object.
*/
void object_aware(object_type *o_ptr)
{
	/* Fully aware of the effects */
	k_info[o_ptr->k_idx].aware = TRUE;
	o_ptr->ident |= (IDENT_SENSE);
}

/*
 * The player has done a *id*, giving full awareness
 */
void object_full_id(object_type *o_ptr)
{
	char line[80];

	/* We have "felt" it in every way */
	o_ptr->ident |= (IDENT_MENTAL);
	object_aware(o_ptr);
	object_known(o_ptr,TRUE);
	/* *ID* gives the formula away */
	if(o_ptr->tval==TV_POTION)
	{
		potion_alch[o_ptr->sval].known1 = TRUE;
		potion_alch[o_ptr->sval].known2 = TRUE;
		msg_print("You have gained alchemical knowledge!");
		alchemy_describe(line, sizeof(line), o_ptr->sval);
		msg_print(line);
	}
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
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Aware item -- use template cost */
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

/* Return the value of the flags the object has... */
s32b flag_cost(object_type * o_ptr, int plusses)
{
	s32b total = 0;
	u32b f1, f2, f3;
	object_flags(o_ptr, &f1, &f2, &f3);

	if (f1 & TR1_STR) total += (1000 * plusses);
	if (f1 & TR1_INT) total += (1000 * plusses);
	if (f1 & TR1_WIS) total += (1000 * plusses);
	if (f1 & TR1_DEX) total += (1000 * plusses);
	if (f1 & TR1_CON) total += (1000 * plusses);
	if (f1 & TR1_CHA) total += (250 * plusses);
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
	if (f1 & TR1_SLAY_ANGEL) total += 3000;
	if (f1 & TR1_KILL_ANGEL) total += 4400;
	if (f1 & TR1_SLAY_GIANT) total += 3500;
	if (f1 & TR1_SLAY_DRAGON) total += 3500;
	if (f1 & TR1_KILL_DRAGON) total += 5500;
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
	if (f2 & TR2_SUST_CHA) total += 250;
	if (f2 & TR2_XXX1) total += 0;
	if (f2 & TR2_XXX2) total += 0;
	if (f2 & TR2_IM_ACID) total += 10000;
	if (f2 & TR2_IM_ELEC) total += 10000;
	if (f2 & TR2_IM_FIRE) total += 10000;
	if (f2 & TR2_IM_COLD) total += 10000;
	if (f2 & TR2_XXX3) total += 0;
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
	if (f3 & TR3_SH_FIRE) total += 5000;
	if (f3 & TR3_SH_ELEC) total += 5000;
	if (f3 & TR3_XP) total += 250000;
	if (f3 & TR3_XXX4) total += 0;
	if (f3 & TR3_NO_TELE) total += 2500;
	if (f3 & TR3_NO_MAGIC) total += 2500;
	if (f3 & TR3_WRAITH) total += 250000;
	if (f3 & TR3_TY_CURSE) total -= 15000;
	if (f3 & TR3_EASY_KNOW) total += 0;
	if (f3 & TR3_HIDE_TYPE) total += 0;
	if (f3 & TR3_SHOW_MODS) total += 0;
	if (f3 & TR3_INSTA_ART) total += 0;
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
	if (f3 & TR3_HEAVY_CURSE) total -= 12500;
	if (f3 & TR3_PERMA_CURSE) total -= 15000;

	/* Also, give some extra for activatable powers... */

	if ((o_ptr->art_name) && (o_ptr->art_flags3 & (TR3_ACTIVATE)))
	{
		int type = o_ptr->xtra2;

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
* Return the "real" price of a "known" item, not including discounts
*
* Wand and staffs get cost for each charge
*
* Armor is worth an extra 100 gold per bonus point to armour class.
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
* Armor with a negative armour bonus is worthless.
* Weapons with negative hit+damage bonuses are worthless.
*
* Every wearable item with a "pval" bonus is worth extra (see below).
*/
s32b object_value_real(object_type *o_ptr)
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

	if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3)
		value += flag_cost (o_ptr, o_ptr->pval);

	/* Artifact */
	else if (o_ptr->name1)
	{
		artefact_type *a_ptr = &a_info[o_ptr->name1];

		/* Hack -- "worthless" artefacts */
		if (!a_ptr->cost) return (0L);

		/* Hack -- Use the artefact cost instead */
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
			if (f1 & (TR1_STR)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_INT)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_WIS)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_DEX)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_CON)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_CHA)) value += (o_ptr->pval * 200L);

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
			/* Hack -- negative armour bonus */
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
		if ((o_ptr->ident & (IDENT_SENSE)) && broken_p(o_ptr)) return (0L);

		/* Hack -- Felt cursed items */
		if ((o_ptr->ident & (IDENT_SENSE)) && cursed_p(o_ptr)) return (0L);

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
* If permitted, we allow weapons/armour to stack, if fully "known".
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
		/* Require identical charges */
		/*StackingWIP*/
		if (object_known_p(o_ptr) && !object_known_p(j_ptr)) return (0);
		if (!object_known_p(o_ptr) && object_known_p(j_ptr)) return (0);
		if (o_ptr->pval != j_ptr->pval) return (0);
	    break;
	case TV_WAND:
		{
			/*StackingWIP*/
			/* Require knowledge of both or knowledge of none */
			/* if (!object_known_p(o_ptr) || !object_known_p(j_ptr)) return (0); */
			if (object_known_p(o_ptr) && !object_known_p(j_ptr)) return (0);
			if (!object_known_p(o_ptr) && object_known_p(j_ptr)) return (0);

			/*StackingWIP screw falling through*/
			/* Fall through */
			break;
		}

		/* Staffs and Wands and Rods */
	case TV_ROD:
		{
			/*StackingWIP*/
			/* Require permission, screw permission*/
			/* if (!stack_allow_wands) return (0); */
			if (object_known_p(o_ptr) && !object_known_p(j_ptr)) return (0);
			if (!object_known_p(o_ptr) && object_known_p(j_ptr)) return (0);
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

			/* Require identical "artefact" names */
			if (o_ptr->name1 != j_ptr->name1) return (FALSE);

			/* Random artefacts never stack */
			if (o_ptr->art_name || j_ptr->art_name) return (FALSE);

			/* Require identical "ego-item" names */
			if (o_ptr->name2 != j_ptr->name2) return (FALSE);

			/* Hack -- Never stack "powerful" items */
			if (o_ptr->xtra1 || j_ptr->xtra1) return (FALSE);

			/* Hack -- Never stack recharging items */
			if (o_ptr->timeout || j_ptr->timeout) return (FALSE);

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



	/* Hack -- Identical art_flags! */
	if ((o_ptr->art_flags1 != j_ptr->art_flags1) ||
		(o_ptr->art_flags2 != j_ptr->art_flags2) ||
		(o_ptr->art_flags3 != j_ptr->art_flags3))
		return (0);

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

	if(o_ptr->tval==TV_WAND)o_ptr->pval=o_ptr->pval+j_ptr->pval;

	/* Hack -- blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr,FALSE);

	/* Hack -- clear "storebought" if only one has it */
	if ( ((o_ptr->ident & IDENT_STOREB) || (j_ptr->ident & IDENT_STOREB)) &&
		(!((o_ptr->ident & IDENT_STOREB) && (j_ptr->ident & IDENT_STOREB))))
	{
		if(j_ptr->ident & IDENT_STOREB) j_ptr->ident &= 0xEF;
		if(o_ptr->ident & IDENT_STOREB) o_ptr->ident &= 0xEF;
	}

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
	value = randnor(bonus, stand);

	/* Enforce the minimum value */
	if (value < 0) return (0);

	/* Enforce the maximum value */
	if (value > max) return (max);

	/* Result */
	return (value);
}




/*
* Debug -- describe a created object for the user
*/
static void object_mention(object_type *o_ptr)
{
	char o_name[80];

	/* Describe */
	object_desc_store(o_name, o_ptr, FALSE, 0);

	/* Artifact */
	if (artefact_p(o_ptr))
	{
		/* Silly message */
		msg_format("Artifact (%s)", o_name);
	}

	else if (o_ptr->art_name)
	{
		msg_print("Random artefact");
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


void random_artefact_resistance(object_type * o_ptr)
{
	bool give_resistance = FALSE, give_power = FALSE;

	if (o_ptr->name1 == ART_MASK) /* Terror Mask is for warriors... */
	{
		if (p_ptr->pclass == CLASS_WARRIOR || p_ptr->pclass == CLASS_BLACK_KNIGHT ||
				p_ptr->pclass == CLASS_CHAOS_KNIGHT || p_ptr->pclass == CLASS_HELL_KNIGHT)
		{
			give_power = TRUE;
			give_resistance = TRUE;
		}
		else
		{
			o_ptr->art_flags3 |=
				(TR3_CURSED | TR3_HEAVY_CURSE | TR3_AGGRAVATE | TR3_TY_CURSE);
			o_ptr->ident |= IDENT_CURSED;
			return;
		}
	}

	switch(o_ptr->name1)
	{

	case ART_VEPAR: case ART_SABNOCK: case ART_PRAVUIL:
	case ART_ALASTOR: case ART_SEIRIM:
	case ART_SHIELD_MICHAEL: case ART_SHIELD_ELEMENTS: case ART_SOLOMON:
	case ART_LIFE: case ART_BLACKREAVER: case ART_SHIELD_AGES:
	case ART_COCYTUS: case ART_DAGGER_FURCIFER: case ART_DAGGER_INFERNO:
	case ART_OROBAS: case ART_JUSTICE:
	case ART_WHIRLWIND:
		{
			/* Give a resistance */
			give_resistance = TRUE;
		}
		break;
	case ART_VALEFAR: case ART_BRIGHTBLADE: case ART_BLACKICE:
	case ART_RASHAVERAK: case ART_FIRETONGUE: case ART_DRAGONSLAYER:
	case ART_SOULSWORD: case ART_BOWASSASSIN: case ART_DEATH:
		{
			/* Give a resistance OR a power */
			if (randint(2)==1) give_resistance = TRUE;
			else give_power = TRUE;
		}
		break;
	case ART_RING_MICHAEL: case ART_EMMANUEL: case ART_MISERY:
	case ART_GRIMREAPER: case ART_BARD: case ART_TRITONS:
	case ART_ATAL:
		{
			/* Give a power */
			give_power = TRUE;
		}
		break;
	case ART_FIRST: case ART_SUN: case ART_HAMMER_AMAYMON:
		{
			/* Give both */
			give_power = TRUE;
			give_resistance = TRUE;
		}
		break;
	}

	if (give_power)
	{
		o_ptr->xtra1 = EGO_XTRA_ABILITY;
		/* Randomize the "xtra" power */
		if (o_ptr->xtra1) o_ptr->xtra2 = (byte_hack)randint(256);
	}

	artefact_bias = 0;

	if (give_resistance)
	{
		random_resistance(o_ptr, FALSE, ((randint(22))+16));
	}
}


/*
* Mega-Hack -- Attempt to create one of the "Special Objects"
*
* We are only called from "make_object()", and we assume that
* "apply_magic()" is called immediately after we return.
*
* Note -- see "make_artefact()" and "apply_magic()"
*/
static bool make_artefact_special(object_type *o_ptr)
{
	int                     i;

	int                     k_idx = 0;


	/* Check the artefact list (just the "specials") */
	for (i = 0; i < ART_MIN_NORMAL; i++)
	{
		artefact_type *a_ptr = &a_info[i];

		/* Skip "empty" artefacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artefact twice */
		if (a_ptr->cur_num) continue;

		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > dun_level)
		{
			/* Acquire the "out-of-depth factor" */
			int d = (a_ptr->level - dun_level) * 2;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* Artifact "rarity roll" */
		if (rand_int(a_ptr->rarity) != 0) return (0);

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

		/* Mega-Hack -- mark the item as an artefact */
		o_ptr->name1 = i;



		/* Success */
		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}


/*
* Attempt to change an object into an artefact
*
* This routine should only be called by "apply_magic()"
*
* Note -- see "make_artefact_special()" and "apply_magic()"
*/
static bool make_artefact(object_type *o_ptr)
{
	int i;


	/* Paranoia -- no "plural" artefacts */
	if (o_ptr->number != 1) return (FALSE);

	/* Check the artefact list (skip the "specials") */
	for (i = ART_MIN_NORMAL; i < MAX_A_IDX; i++)
	{
		artefact_type *a_ptr = &a_info[i];

		/* Skip "empty" items */
		if (!a_ptr->name) continue;

		/* Cannot make an artefact twice */
		if (a_ptr->cur_num) continue;

		/* Must have the correct fields */
		if (a_ptr->tval != o_ptr->tval) continue;
		if (a_ptr->sval != o_ptr->sval) continue;

		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > dun_level)
		{
			/* Acquire the "out-of-depth factor" */
			int d = (a_ptr->level - dun_level) * 2;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0) continue;
		}

		/* We must make the "rarity roll" */
		if (rand_int(a_ptr->rarity) != 0) continue;

		/* Hack -- mark the item as an artefact */
		o_ptr->name1 = i;

		random_artefact_resistance(o_ptr); /* Hack: Some artefacts
										   get random extra powers */
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
	case SV_WAND_HEAL_MONSTER:    o_ptr->pval = rand_s16b(20) + 8; break;
	case SV_WAND_HASTE_MONSTER:   o_ptr->pval = rand_s16b(20) + 8; break;
	case SV_WAND_CLONE_MONSTER:   o_ptr->pval = rand_s16b(5)  + 3; break;
	case SV_WAND_TELEPORT_AWAY:   o_ptr->pval = rand_s16b(5)  + 6; break;
	case SV_WAND_DISARMING:       o_ptr->pval = rand_s16b(5)  + 4; break;
	case SV_WAND_TRAP_DOOR_DEST:  o_ptr->pval = rand_s16b(8)  + 6; break;
	case SV_WAND_STONE_TO_MUD:    o_ptr->pval = rand_s16b(8)  + 3; break;
	case SV_WAND_LITE:            o_ptr->pval = rand_s16b(10) + 6; break;
	case SV_WAND_SLEEP_MONSTER:   o_ptr->pval = rand_s16b(15) + 8; break;
	case SV_WAND_SLOW_MONSTER:    o_ptr->pval = rand_s16b(10) + 6; break;
	case SV_WAND_CONFUSE_MONSTER: o_ptr->pval = rand_s16b(12) + 6; break;
	case SV_WAND_FEAR_MONSTER:    o_ptr->pval = rand_s16b(5)  + 3; break;
	case SV_WAND_DRAIN_LIFE:      o_ptr->pval = rand_s16b(3)  + 3; break;
	case SV_WAND_POLYMORPH:       o_ptr->pval = rand_s16b(8)  + 6; break;
	case SV_WAND_STINKING_CLOUD:  o_ptr->pval = rand_s16b(8)  + 6; break;
	case SV_WAND_MAGIC_MISSILE:   o_ptr->pval = rand_s16b(10) + 6; break;
	case SV_WAND_ACID_BOLT:       o_ptr->pval = rand_s16b(8)  + 6; break;
	case SV_WAND_CHARM_MONSTER:   o_ptr->pval = rand_s16b(6)  + 2; break;
	case SV_WAND_FIRE_BOLT:       o_ptr->pval = rand_s16b(8)  + 6; break;
	case SV_WAND_COLD_BOLT:       o_ptr->pval = rand_s16b(5)  + 6; break;
	case SV_WAND_ACID_BALL:       o_ptr->pval = rand_s16b(5)  + 2; break;
	case SV_WAND_ELEC_BALL:       o_ptr->pval = rand_s16b(8)  + 4; break;
	case SV_WAND_FIRE_BALL:       o_ptr->pval = rand_s16b(4)  + 2; break;
	case SV_WAND_COLD_BALL:       o_ptr->pval = rand_s16b(6)  + 2; break;
	case SV_WAND_WONDER:          o_ptr->pval = rand_s16b(15) + 8; break;
	case SV_WAND_ANNIHILATION:    o_ptr->pval = rand_s16b(2)  + 1; break;
	case SV_WAND_DRAGON_FIRE:     o_ptr->pval = rand_s16b(3)  + 1; break;
	case SV_WAND_DRAGON_COLD:     o_ptr->pval = rand_s16b(3)  + 1; break;
	case SV_WAND_DRAGON_BREATH:   o_ptr->pval = rand_s16b(3)  + 1; break;
	case SV_WAND_SHARD:           o_ptr->pval = rand_s16b(2)  + 1; break;
	}
}

/*
* Charge a new staff.
*/
static void charge_staff(object_type *o_ptr)
{
	switch (o_ptr->sval)
	{
	case SV_STAFF_DARKNESS:       o_ptr->pval = rand_s16b(8)  + 8; break;
	case SV_STAFF_SLOWNESS:       o_ptr->pval = rand_s16b(8)  + 8; break;
	case SV_STAFF_HASTE_MONSTERS: o_ptr->pval = rand_s16b(8)  + 8; break;
	case SV_STAFF_SUMMONING:      o_ptr->pval = rand_s16b(3)  + 1; break;
	case SV_STAFF_TELEPORTATION:  o_ptr->pval = rand_s16b(4)  + 5; break;
	case SV_STAFF_IDENTIFY:       o_ptr->pval = rand_s16b(15) + 5; break;
	case SV_STAFF_REMOVE_CURSE:   o_ptr->pval = rand_s16b(3)  + 4; break;
	case SV_STAFF_STARLITE:       o_ptr->pval = rand_s16b(5)  + 6; break;
	case SV_STAFF_LITE:           o_ptr->pval = rand_s16b(20) + 8; break;
	case SV_STAFF_MAPPING:        o_ptr->pval = rand_s16b(5)  + 5; break;
	case SV_STAFF_DETECT_GOLD:    o_ptr->pval = rand_s16b(20) + 8; break;
	case SV_STAFF_DETECT_ITEM:    o_ptr->pval = rand_s16b(15) + 6; break;
	case SV_STAFF_DETECT_TRAP:    o_ptr->pval = rand_s16b(5)  + 6; break;
	case SV_STAFF_DETECT_DOOR:    o_ptr->pval = rand_s16b(8)  + 6; break;
	case SV_STAFF_DETECT_INVIS:   o_ptr->pval = rand_s16b(15) + 8; break;
	case SV_STAFF_DETECT_EVIL:    o_ptr->pval = rand_s16b(15) + 8; break;
	case SV_STAFF_CURE_LIGHT:     o_ptr->pval = rand_s16b(5)  + 6; break;
	case SV_STAFF_CURING:         o_ptr->pval = rand_s16b(3)  + 4; break;
	case SV_STAFF_HEALING:        o_ptr->pval = rand_s16b(2)  + 1; break;
	case SV_STAFF_THE_MAGI:       o_ptr->pval = rand_s16b(2)  + 2; break;
	case SV_STAFF_SLEEP_MONSTERS: o_ptr->pval = rand_s16b(5)  + 6; break;
	case SV_STAFF_SLOW_MONSTERS:  o_ptr->pval = rand_s16b(5)  + 6; break;
	case SV_STAFF_SPEED:          o_ptr->pval = rand_s16b(3)  + 4; break;
	case SV_STAFF_PROBING:        o_ptr->pval = rand_s16b(6)  + 2; break;
	case SV_STAFF_DISPEL_EVIL:    o_ptr->pval = rand_s16b(3)  + 4; break;
	case SV_STAFF_POWER:          o_ptr->pval = rand_s16b(3)  + 1; break;
	case SV_STAFF_HOLINESS:       o_ptr->pval = rand_s16b(2)  + 2; break;
	case SV_STAFF_GENOCIDE:       o_ptr->pval = rand_s16b(2)  + 1; break;
	case SV_STAFF_EARTHQUAKES:    o_ptr->pval = rand_s16b(5)  + 3; break;
	case SV_STAFF_DESTRUCTION:    o_ptr->pval = rand_s16b(3)  + 1; break;
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

	byte temp_byte = 0;

	artefact_bias = 0;

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
				o_ptr->pval = 0 - (5 + rand_s16b(5));
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
				switch (randint((o_ptr->tval == TV_POLEARM)?40:42))
				{
				case 1:
					{
						o_ptr->name2 = EGO_HA;
						if (randint(4)==1)
						{ o_ptr->art_flags1 |= TR1_BLOWS;
						if (o_ptr->pval > 2) o_ptr->pval =
							o_ptr->pval - (rand_s16b(2));
						}
						break;
					}

				case 2:
					{
						o_ptr->name2 = EGO_DF;
						if (randint(3)==1) o_ptr->art_flags2 |= TR2_RES_POIS;
						random_resistance(o_ptr, FALSE, ((randint(22))+16));
						break;
					}

				case 3:
					{
						o_ptr->name2 = EGO_BRAND_ACID;
						break;
					}

				case 4:
					{
						o_ptr->name2 = EGO_BRAND_ELEC;
						break;
					}

				case 5:
					{
						o_ptr->name2 = EGO_BRAND_FIRE;
						break;
					}

				case 6:
					{
						o_ptr->name2 = EGO_BRAND_COLD;
						break;
					}

				case 7: case 8:
					{
						o_ptr->name2 = EGO_SLAY_ANIMAL;
						if (rand_int(100) < 20)
						{
							o_ptr->name2 = EGO_KILL_ANIMAL;
						}
						break;
					}

				case 9: case 10:
					{
						o_ptr->name2 = EGO_SLAY_DRAGON;
						random_resistance(o_ptr, FALSE, ((randint(12))+4));
						if (rand_int(100) < 20)
						{
							if (randint(3)==1) o_ptr->art_flags2 |= TR2_RES_POIS;
							random_resistance(o_ptr, FALSE, ((randint(14))+4));
							o_ptr->name2 = EGO_KILL_DRAGON;
						}
						break;
					}

				case 11: case 12:
					{
						o_ptr->name2 = EGO_SLAY_EVIL;
						if (rand_int(100) < 20)
						{
							o_ptr->art_flags2 |= TR2_RES_FEAR;
							o_ptr->art_flags3 |= TR3_BLESSED;
							o_ptr->name2 = EGO_KILL_EVIL;
						}
						break;
					}

				case 13: case 14: case 15:
					{
						o_ptr->name2 = EGO_SLAY_UNDEAD;
						o_ptr->art_flags2 |= TR2_HOLD_LIFE;
						if (rand_int(100) < 20)
						{
							o_ptr->art_flags2 |= TR2_RES_NETHER;
							o_ptr->name2 = EGO_KILL_UNDEAD;
						}
						break;
					}

			    case 16: case 17: case 18:
					{
						o_ptr->name2 = EGO_SLAY_ANGEL;
						if (rand_int(100) < 20)
						{
							o_ptr->name2 = EGO_KILL_XANGEL;
						}
						break;
					}

				 case 19: case 20:
					{
						o_ptr->name2 = EGO_KILL_ANGEL;
						if (rand_int(100) < 20)
						{
							/*Okay, okay, this is a hack, however, what is the superlative of kill angel, execute angel ???
							there would have been to much angel bashing, so I decided to replace some of it with sinner bashing*/
							o_ptr->name2 = EGO_KILL_SINNER;
						}
						break;
					}

				case 21: case 22: case 23:
					{
						o_ptr->name2 = EGO_SLAY_GIANT;
						if (rand_int(100) < 20)
						{
							o_ptr->name2 = EGO_KILL_GIANT;
						}
						break;
					}

				case 24: case 25: case 26:
					{
						o_ptr->name2 = EGO_SLAY_DEMON;
						if (rand_int(100) < 20)
						{
							o_ptr->name2 = EGO_KILL_DEMON;
						}
						break;
					}

				case 27:
					{
						o_ptr->name2 = EGO_ANGELIC;
						if (randint(3)==1) o_ptr->art_flags2 |= TR2_RES_FEAR;
						break;
					}

				case 28:
					{
						o_ptr->name2 = EGO_BLESS_BLADE;
						break;
					}

				case 29: case 30:
					{
						o_ptr->name2 = EGO_ATTACKS;
						break;
					}

				case 31: case 32:
					{
						o_ptr->name2 = EGO_VAMPIRIC;
						break;
					}
				case 33:
					{
						o_ptr->name2 = EGO_BRAND_POIS;
						break;
					}
				case 34:
					{
						o_ptr->name2 = EGO_CHAOTIC;
						random_resistance(o_ptr, FALSE, ((randint(34))+4));
						break;
					}
				case 35:
					{
						create_artefact(o_ptr, FALSE);
						break;
					}
				case 36: case 37:
					{
						o_ptr->name2 = EGO_SLAYING_WEAPON;
						if (randint(3)==1) /* double damage */
							o_ptr->dd *= 2;
						else
						{
							do { o_ptr->dd++; } while (randint(o_ptr->dd)==1);
							do { o_ptr->ds++; } while (randint(o_ptr->ds)==1);
						}
						/* Always put the biggest number up front to increase average damage */
						if( o_ptr->ds > o_ptr->dd)
						{
							temp_byte = o_ptr->ds;
							o_ptr->ds = o_ptr->dd;
							o_ptr->dd = temp_byte;
						}
						if (randint(5)==1)
						{
							o_ptr->art_flags1 |= TR1_BRAND_POIS;
						}
						if (o_ptr->tval == TV_SWORD && (randint(3)==1))
						{
							o_ptr->art_flags1 |= TR1_VORPAL;
						}
						break;
					}
				case 38: case 39:
					{
						o_ptr->name2 = EGO_PLANAR;
						random_resistance(o_ptr, FALSE, ((randint(22))+16));
						if (randint(5)==1) o_ptr->art_flags1 |= TR1_SLAY_DEMON;
						break;
					}
				case 40:
					{
						o_ptr->name2 = EGO_PATTERN;
						if (randint(3)==1) o_ptr->art_flags2 |= TR2_HOLD_LIFE;
						if (randint(3)==1) o_ptr->art_flags1 |= TR1_DEX;
						if (randint(5)==1) o_ptr->art_flags2 |= TR2_RES_FEAR;
						random_resistance(o_ptr, FALSE, ((randint(22))+16));
						break;
					}
				default: /* 2 slots for TV_SWORD and TV_HAFTED */
					{
						if (o_ptr->tval == TV_SWORD)
						{
							o_ptr->name2 = EGO_SHARPNESS;
							o_ptr->pval = m_bonus(5, level) + 1;
						}
						else /* Hafted */
						{
							o_ptr->name2 = EGO_EARTHQUAKES;
							if (randint(3)==1) o_ptr->art_flags1 |= TR1_BLOWS;
							o_ptr->pval = m_bonus(3, level);
						}
					}
				}



				/* Hack -- Super-charge the damage dice */
				while (rand_int(10L * o_ptr->dd * o_ptr->ds) == 0) o_ptr->dd++;

				/* Hack -- Lower the damage dice */
				if (o_ptr->dd > 9) o_ptr->dd = 9;
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				if (rand_int(MAX_DEPTH) < level)
				{
					o_ptr->name2 = EGO_HELL;
					if (randint(6)==1) o_ptr->art_flags3 |= TR3_TY_CURSE;
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
				switch (randint(21))
				{
				case 1: case 11:
					{
						o_ptr->name2 = EGO_EXTRA_MIGHT;
						random_resistance(o_ptr, FALSE, ((randint(34))+4));
						break;
					}

				case 2: case 12:
					{
						o_ptr->name2 = EGO_EXTRA_SHOTS;
						break;
					}

				case 3: case 4: case 5: case 6:
				case 13: case 14: case 15: case 16:
					{
						o_ptr->name2 = EGO_VELOCITY;
						break;
					}

				case 7: case 8: case 9: case 10:
				case 17: case 18: case 19: case 20:
					{
						o_ptr->name2 = EGO_ACCURACY;
						break;
					}
				default:
					{
						create_artefact(o_ptr, FALSE);
					}
				}
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
				switch (randint(16))
				{
				case 1: case 2: case 3:
					{
						o_ptr->name2 = EGO_WOUNDING;
						break;
					}

				case 4:
					{
						o_ptr->name2 = EGO_FLAME;
						break;
					}

				case 5: case 6:
					{
						o_ptr->name2 = EGO_FROST;
						break;
					}

				 case 7:
					{
						o_ptr->name2 = EGO_HURT_ANIMAL;
						break;
					}

				case 8: case 9:
					{
						o_ptr->name2 = EGO_HURT_EVIL;
						break;
					}

				case 10:
					{
						o_ptr->name2 = EGO_HURT_DRAGON;
						break;
					}

				case 11:
					{
						o_ptr->name2 = EGO_LIGHTNING_BOLT;
						break;
					}

				case 12:
					{
						o_ptr->name2 = EGO_SLAYING_BOLT;
						o_ptr->dd++;
						break;
					}
				case 13: case 14:
				{
					o_ptr->name2 = EGO_HOLY_FLAME;
					o_ptr->dd++;
					break;
				}
				case 15: case 16:
				{
					o_ptr->name2 = EGO_ANGEL_BANE;
					o_ptr->dd++;
					break;
				}
				}

				/* Hack -- super-charge the damage dice */
				while (rand_int(10L * o_ptr->dd * o_ptr->ds) == 0) o_ptr->dd++;

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

static void dragon_resist(object_type * o_ptr)
{
	do
	{
		artefact_bias = 0;

		if (randint(4)==1)
			random_resistance(o_ptr, FALSE, ((randint(14))+4));
		else
			random_resistance(o_ptr, FALSE, ((randint(22))+16));
	}
	while (randint(2)==1);
}


/*
* Apply magic to an item known to be "armour"
*
* Hack -- note special processing for crown/helm
* Hack -- note special processing for robe of permanence
*/
static void a_m_aux_2(object_type *o_ptr, int level, int power)
{
	int toac1 = randint(5) + m_bonus(5, level);

	int toac2 = m_bonus(10, level);

	artefact_bias = 0;

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
			if (debug_peek) object_mention(o_ptr);

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
				switch (randint(21))
				{
				case 1: case 2: case 3: case 4:
					{
						o_ptr->name2 = EGO_RESIST_ACID;
						break;
					}

				case 5: case 6: case 7: case 8:
					{
						o_ptr->name2 = EGO_RESIST_ELEC;
						break;
					}

				case 9: case 10: case 11: case 12:
					{
						o_ptr->name2 = EGO_RESIST_FIRE;
						break;
					}

				case 13: case 14: case 15: case 16:
					{
						o_ptr->name2 = EGO_RESIST_COLD;
						break;
					}

				case 17: case 18:
					{
						o_ptr->name2 = EGO_HEAVEN;
						if (randint(4)==1) o_ptr->art_flags2 |= TR2_RES_POIS;
						random_resistance(o_ptr, FALSE, ((randint(22))+16));
						break;
					}

				case 20: case 21:
					{
						o_ptr->name2 = EGO_ELVENKIND;
						break;
					}
				default:
					{
						create_artefact (o_ptr, FALSE);
					}

				}
			}

			break;
		}


	case TV_SHIELD:
		{

			if (o_ptr->sval == SV_DRAGON_SHIELD)
			{
				/* Rating boost */
				rating += 5;

				/* Mention the item */
				if (debug_peek) object_mention(o_ptr);
				dragon_resist(o_ptr);
			}
			else
			{
				/* Very good */
				if (power > 1)
				{
					/* Roll for ego-item */
					switch (randint(23))
					{
					case 1: case 11:
						{
							o_ptr->name2 = EGO_ENDURE_ACID;
							break;
						}

					case 2: case 3: case 4:
					case 12: case 13: case 14:
						{
							o_ptr->name2 = EGO_ENDURE_ELEC;
							break;
						}

					case 5: case 6:
					case 15: case 16:
						{
							o_ptr->name2 = EGO_ENDURE_FIRE;
							break;
						}

					case 7: case 8: case 9:
					case 17: case 18: case 19:
						{
							o_ptr->name2 = EGO_ENDURE_COLD;
							break;
						}

					case 10: case 20:
						{
							random_resistance(o_ptr, FALSE, ((randint(34))+4));
							if (randint(4)==1) o_ptr->art_flags2 |= TR2_RES_POIS;
							o_ptr->name2 = EGO_ENDURANCE;
							break;
						}
					case 21: case 22:
						{
							o_ptr->name2 = EGO_REFLECTION;
							break;
						}
					default:
						{
							create_artefact (o_ptr, FALSE);
						}
					}
				}
			}
			break;
		}


	case TV_GLOVES:
		{
			/* Very good */
			if (power > 1)
			{
				if (randint(20)==1)
					create_artefact(o_ptr, FALSE);
				else
				{
					/* Roll for ego-item */
					switch (randint(10))
					{
					case 1: case 2: case 3: case 4:
						{
							o_ptr->name2 = EGO_FREE_ACTION;
							break;
						}

					case 5: case 6: case 7:
						{
							o_ptr->name2 = EGO_SLAYING;
							break;
						}

					case 8: case 9:
						{
							o_ptr->name2 = EGO_AGILITY;
							break;
						}

					case 10:
						{
							o_ptr->name2 = EGO_POWER;
							random_resistance(o_ptr, FALSE, ((randint(22))+16));
							break;
						}
					}
				}
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				switch (randint(2))
				{
				case 1:
					{
						o_ptr->name2 = EGO_CLUMSINESS;
						break;
					}
				default:
					{
						o_ptr->name2 = EGO_WEAKNESS;
						break;
					}
				}
			}

			break;
		}


	case TV_BOOTS:
		{
			/* Very good */
			if (power > 1)
			{
				if (randint(20)==1)
					create_artefact(o_ptr, FALSE);
				else
				{
					/* Roll for ego-item */
					switch (randint(24))
					{
					case 1:
						{
							o_ptr->name2 = EGO_SPEED;
							break;
						}

					case 2: case 3: case 4: case 5:
						{
							o_ptr->name2 = EGO_MOTION;
							break;
						}

					case 6: case 7: case 8: case 9:
					case 10: case 11: case 12: case 13:
						{
							o_ptr->name2 = EGO_QUIET;
							break;
						}

					default:
						{
							o_ptr->name2 = EGO_SLOW_DESCENT;
							if (randint (2) == 1)
							{
								random_resistance(o_ptr, FALSE, ((randint(22))+16));
							}
							break;
						}
					}
				}
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				switch (randint(3))
				{
				case 1:
					{
						o_ptr->name2 = EGO_NOISE;
						break;
					}
				case 2:
					{
						o_ptr->name2 = EGO_SLOWNESS;
						break;
					}
				case 3:
					{
						o_ptr->name2 = EGO_ANNOYANCE;
						break;
					}
				}
			}

			break;
		}


	case TV_CROWN:
		{
			/* Very good */
			if (power > 1)
			{
				if (randint(20)==1)
					create_artefact(o_ptr, FALSE);
				else
				{
					/* Roll for ego-item */
					switch (randint(8))
					{
					case 1:
						{
							o_ptr->name2 = EGO_MAGI;
							random_resistance(o_ptr, FALSE, ((randint(22))+16));
							break;
						}
					case 2:
						{
							o_ptr->name2 = EGO_MIGHT;
							random_resistance(o_ptr, FALSE, ((randint(22))+16));
							break;
						}
					case 3:
						{
							o_ptr->name2 = EGO_TELEPATHY;
							break;
						}
					case 4:
						{
							o_ptr->name2 = EGO_REGENERATION;
							break;
						}
					case 5: case 6:
						{
							o_ptr->name2 = EGO_LORDLINESS;
							random_resistance(o_ptr, FALSE, ((randint(22))+16));
							break;
						}
					default:
						{
							o_ptr->name2 = EGO_SEEING;
							if (randint(3)==1) o_ptr->art_flags3 |= TR3_TELEPATHY;
							break;
						}
					}
				}
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Roll for ego-item */
				switch (randint(7))
				{
				case 1: case 2:
					{
						o_ptr->name2 = EGO_STUPIDITY;
						break;
					}
				case 3: case 4:
					{
						o_ptr->name2 = EGO_NAIVETY;
						break;
					}
				case 5:
					{
						o_ptr->name2 = EGO_UGLINESS;
						break;
					}
				case 6:
					{
						o_ptr->name2 = EGO_SICKLINESS;
						break;
					}
				case 7:
					{
						o_ptr->name2 = EGO_TELEPORTATION;
						break;
					}
				}
			}

			break;
		}


	case TV_HELM:
		{

			if (o_ptr->sval == SV_DRAGON_HELM)
			{
				/* Rating boost */
				rating += 5;

				/* Mention the item */
				if (debug_peek) object_mention(o_ptr);
				dragon_resist(o_ptr);

			}
			else
			{
				/* Very good */
				if (power > 1)
				{
					if (randint(20)==1)
						create_artefact(o_ptr, FALSE);
					else
					{
						/* Roll for ego-item */
						switch (randint(14))
						{
						case 1: case 2:
							{
								o_ptr->name2 = EGO_INTELLIGENCE;
								break;
							}
						case 3: case 4:
							{
								o_ptr->name2 = EGO_WISDOM;
								break;
							}
						case 5: case 6:
							{
								o_ptr->name2 = EGO_BEAUTY;
								break;
							}
						case 7: case 8:
							{
								o_ptr->name2 = EGO_SEEING;
								if (randint(7)==1) o_ptr->art_flags3 |= TR3_TELEPATHY;
								break;
							}
						case 9: case 10:
							{
								o_ptr->name2 = EGO_LITE;
								break;
							}
						default:
							{
								o_ptr->name2 = EGO_INFRAVISION;
								break;
							}
						}
					}
				}

				/* Very cursed */
				else if (power < -1)
				{
					/* Roll for ego-item */
					switch (randint(7))
					{
					case 1: case 2:
						{
							o_ptr->name2 = EGO_STUPIDITY;
							break;
						}
					case 3: case 4:
						{
							o_ptr->name2 = EGO_NAIVETY;
							break;
						}
					case 5:
						{
							o_ptr->name2 = EGO_UGLINESS;
							break;
						}
					case 6:
						{
							o_ptr->name2 = EGO_SICKLINESS;
							break;
						}
					case 7:
						{
							o_ptr->name2 = EGO_TELEPORTATION;
							break;
						}
					}
				}
			}
			break;
		}


	case TV_CLOAK:
		{
			if (o_ptr->sval == SV_ELVEN_CLOAK)
				o_ptr->pval = rand_s16b(4); /* No cursed elven cloaks...? */

			/* Very good */
			if (power > 1)
			{
				if (randint(20)==1)
					create_artefact(o_ptr, FALSE);
				else
				{
					/* Roll for ego-item */
					switch (randint(19))
					{
					case 1: case 2: case 3: case 4:
					case 5: case 6: case 7: case 8:
						{
							o_ptr->name2 = EGO_PROTECTION;
							break;
						}

					case 9: case 10: case 11: case 12:
					case 13: case 14: case 15: case 16:
						{
							o_ptr->name2 = EGO_STEALTH;
							break;
						}

					case 17:
						{
							o_ptr->name2 = EGO_AMAN;
							break;
						}
					case 18:
						{
							o_ptr->name2 = EGO_AURA_ELEC;
							break;
						}
					default:
						{
							o_ptr->name2 = EGO_AURA_FIRE;
						}
					}
				}
			}

			/* Very cursed */
			else if (power < -1)
			{
				/* Choose some damage */
				switch (randint(3))
				{
				case 1:
					{
						o_ptr->name2 = EGO_IRRITATION;
						break;
					}
				case 2:
					{
						o_ptr->name2 = EGO_VULNERABILITY;
						break;
					}
				case 3:
					{
						o_ptr->name2 = EGO_ENVELOPING;
						break;
					}
				}
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

	artefact_bias = 0;

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
	case TV_RING:
		{
			/* Analyze */
			switch (o_ptr->sval)
			{
				/* Strength, Constitution, Dexterity, Intelligence */
			case SV_RING_ATTACKS:
				{
					/* Stat bonus */
					o_ptr->pval = m_bonus(3, level);
					if (o_ptr->pval < 1) o_ptr->pval = 1;

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
					o_ptr->pval = rand_s16b(5) + m_bonus(5, level);

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
					if (debug_peek) object_mention(o_ptr);

					break;
				}

			case SV_RING_LORDLY:
				{
					do
					{
						random_resistance(o_ptr, FALSE, ((randint(20))+18));
					}
					while(randint(4)==1);
					/* Bonus to armour class */
					o_ptr->to_a = 10 + rand_s16b(5) + m_bonus(10, level);
					rating += 5;
				}
				break;

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
					/* Bonus to armour class */
					o_ptr->to_a = 5 + rand_s16b(5) + m_bonus(10, level);
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
					o_ptr->to_d = 5 + rand_s16b(8) + m_bonus(10, level);

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
					o_ptr->to_h = 5 + rand_s16b(8) + m_bonus(10, level);

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
					/* Bonus to armour class */
					o_ptr->to_a = 5 + rand_s16b(8) + m_bonus(10, level);

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
					o_ptr->to_d = rand_s16b(7) + m_bonus(10, level);
					o_ptr->to_h = rand_s16b(7) + m_bonus(10, level);

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
			case SV_AMULET_BRILLIANCE:
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

			case SV_AMULET_NO_MAGIC: case SV_AMULET_NO_TELE:
				{
					if (power < 0)
					{
						o_ptr->ident |= (IDENT_CURSED);
					}
					break;
				}

			case SV_AMULET_RESISTANCE:
				{
					if (randint(3)==1) random_resistance(o_ptr, FALSE, ((randint(34))+4));
					if (randint(5)==1) o_ptr->art_flags2 |= TR2_RES_POIS;
				}
				break;

				/* Amulet of searching */
			case SV_AMULET_SEARCHING:
				{
					o_ptr->pval = rand_s16b(5) + m_bonus(5, level);

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
					o_ptr->pval = rand_s16b(5) + m_bonus(5, level);
					o_ptr->to_a = rand_s16b(5) + m_bonus(5, level);

					if (randint(3)==1) o_ptr->art_flags3 |= TR3_SLOW_DIGEST;

					/* Boost the rating */
					rating += 25;

					/* Mention the item */
					if (debug_peek) object_mention(o_ptr);

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
					o_ptr->pval = 0 - (rand_s16b(5) + m_bonus(5, level));
					o_ptr->to_a = 0 - (rand_s16b(5) + m_bonus(5, level));

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
	/* XXX XXX XXX */
	/*
	level = (0, level);
	power = (0, power);
	*/

	/* Apply magic (good or bad) according to type */

	(void)level;/*Not used??*/
	(void)power;/*Not used??*/

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

	case TV_CHEST:
		{
			/* Hack -- skip ruined chests */
			if (k_info[o_ptr->k_idx].level <= 0) break;

			/* Hack -- pick a "difficulty" */
			o_ptr->pval = rand_s16b(k_info[o_ptr->k_idx].level);

			/* Never exceed "difficulty" of 55 to 59 */
			if (o_ptr->pval > 55) o_ptr->pval = (short)(55 + rand_int(5));

			break;
		}
	}
}

/*
* Apply magic to an item known to be a light
*
* Hack -- note the special code for various items
*/
static void a_m_aux_5(object_type *o_ptr, int level, int power)
{
	int i;

	(void)level;/*Not used??*/

	/* Hack -- Torches -- random fuel */
	if (o_ptr->sval == SV_LITE_TORCH)
	{
		if (o_ptr->pval) o_ptr->pval = rand_s16b(o_ptr->pval);
		return;
	}

	/* Hack -- Lanterns -- random fuel */
	if (o_ptr->sval == SV_LITE_LANTERN)
	{
		if (o_ptr->pval) o_ptr->pval = rand_s16b(o_ptr->pval);
		return;
	}

	/* Orbs */
	if (power < 0) /* Cursed */
	{
		switch(randint(2)) /* Cursed */
		{
		case 1:
			{
				o_ptr->name2 = EGO_ORB_IRRITATION;
				/* Broken */
				o_ptr->ident |= (IDENT_BROKEN);

				/* Cursed */
				o_ptr->ident |= (IDENT_CURSED);
				break;
			}
		case 2:
			{
				o_ptr->name2 = EGO_ORB_INSTABILITY;
				/* Broken */
				o_ptr->ident |= (IDENT_BROKEN);

				/* Cursed */
				o_ptr->ident |= (IDENT_CURSED);
				break;
			}
		default:
			{
				break;
			}
		}
	}
	else if (power == 1) /* Good */
	{
		switch(randint(30))
		{
		case 1:
			{
				o_ptr->name2 = EGO_ORB_FLAME;
				break;
			}
		case 2:
			{
				o_ptr->name2 = EGO_ORB_FROST;
				break;
			}
		case 3:
			{
				o_ptr->name2 = EGO_ORB_ACID;
				break;
			}
		case 4:
			{
				o_ptr->name2 = EGO_ORB_LIGHTNING;
				break;
			}
		case 5:
			{
				o_ptr->name2 = EGO_ORB_LIGHT;
				break;
			}
		case 6:
			{
				o_ptr->name2 = EGO_ORB_DARKNESS;
				break;
			}
		case 7:
			{
				o_ptr->name2 = EGO_ORB_LIFE;
				break;
			}
		case 8:
			{
				o_ptr->name2 = EGO_ORB_SIGHT;
				break;
			}
		case 9:
			{
				o_ptr->name2 = EGO_ORB_COURAGE;
				break;
			}
		case 10:
			{
				o_ptr->name2 = EGO_ORB_COURAGE;
				break;
			}
		case 11:
			{
				o_ptr->name2 = EGO_ORB_VENOM;
				break;
			}
		case 12:
			{
				o_ptr->name2 = EGO_ORB_CLARITY;
				break;
			}
		case 13:
			{
				o_ptr->name2 = EGO_ORB_SOUND;
				break;
			}
		case 14:
			{
				o_ptr->name2 = EGO_ORB_CHAOS;
				break;
			}
		case 15:
			{
				o_ptr->name2 = EGO_ORB_SHARDS;
				break;
			}
		case 16:
			{
				o_ptr->name2 = EGO_ORB_UNLIFE;
				break;
			}
		case 17:
			{
				o_ptr->name2 = EGO_ORB_STABILITY;
				break;
			}
		case 18:
			{
				o_ptr->name2 = EGO_ORB_MAGIC;
				break;
			}
		case 19:
			{
				o_ptr->name2 = EGO_ORB_FREEDOM;
				break;
			}
		case 20:
			{
				o_ptr->name2 = EGO_ORB_STRENGTH;
				break;
			}
		case 21:
			{
				o_ptr->name2 = EGO_ORB_INTELLIGENCE;
				break;
			}
		case 22:
			{
				o_ptr->name2 = EGO_ORB_WISDOM;
				break;
			}
		case 23:
			{
				o_ptr->name2 = EGO_ORB_DEXTERITY;
				break;
			}
		case 24:
			{
				o_ptr->name2 = EGO_ORB_CONSTITUTION;
				break;
			}
		case 25:
			{
				o_ptr->name2 = EGO_ORB_CHARISMA;
				break;
			}
		case 26:
			{
				o_ptr->name2 = EGO_ORB_LIGHTNESS;
				break;
			}
		case 27:
			{
				o_ptr->name2 = EGO_ORB_INSIGHT;
				break;
			}
		case 28:
			{
				o_ptr->name2 = EGO_ORB_MIND;
				break;
			}
		case 29:
			{
				o_ptr->name2 = EGO_ORB_SUSTENANCE;
				break;
			}
		case 30:
			{
				o_ptr->name2 = EGO_ORB_HEALTH;
				break;
			}
		default:
			{
				break;
			}
		}
	}
	else if (power == 2) /* Great */
	{
		o_ptr->name2 = EGO_ORB_POWER;
		for (i=0; i<7; i++)
		{
			switch(randint(30))
			{
			case 1:
			case 2:
				{
					o_ptr->art_flags2 |= TR2_RES_DARK;
					break;
				}
			case 3:
				{
					o_ptr->art_flags2 |= TR2_RES_LITE;
					break;
				}
			case 4:
				{
					o_ptr->art_flags2 |= TR2_RES_BLIND;
					break;
				}
			case 5:
				{
					o_ptr->art_flags2 |= TR2_RES_FEAR;
					break;
				}
			case 6:
				{
					o_ptr->art_flags2 |= TR2_RES_ACID;
					break;
				}
			case 7:
				{
					o_ptr->art_flags2 |= TR2_RES_ELEC;
					break;
				}
			case 8:
				{
					o_ptr->art_flags2 |= TR2_RES_FIRE;
					break;
				}
			case 9:
				{
					o_ptr->art_flags2 |= TR2_RES_COLD;
					break;
				}
			case 10:
				{
					o_ptr->art_flags2 |= TR2_RES_POIS;
					break;
				}
			case 11:
				{
					o_ptr->art_flags2 |= TR2_RES_CONF;
					break;
				}
			case 12:
				{
					o_ptr->art_flags2 |= TR2_RES_SOUND;
					break;
				}
			case 13:
				{
					o_ptr->art_flags2 |= TR2_RES_SHARDS;
					break;
				}
			case 14:
				{
					o_ptr->art_flags2 |= TR2_RES_NETHER;
					break;
				}
			case 15:
				{
					o_ptr->art_flags2 |= TR2_RES_NEXUS;
					break;
				}
			case 16:
				{
					o_ptr->art_flags2 |= TR2_RES_CHAOS;
					break;
				}
			case 17:
				{
					o_ptr->art_flags2 |= TR2_RES_DISEN;
					break;
				}
			case 18:
				{
					o_ptr->art_flags2 |= TR2_FREE_ACT;
					break;
				}
			case 19:
				{
					o_ptr->art_flags2 |= TR2_HOLD_LIFE;
					break;
				}
			case 20:
				{
					o_ptr->art_flags2 |= TR2_SUST_STR;
					break;
				}
			case 21:
				{
					o_ptr->art_flags2 |= TR2_SUST_INT;
					break;
				}
			case 22:
				{
					o_ptr->art_flags2 |= TR2_SUST_WIS;
					break;
				}
			case 23:
				{
					o_ptr->art_flags2 |= TR2_SUST_DEX;
					break;
				}
			case 24:
				{
					o_ptr->art_flags2 |= TR2_SUST_CON;
					break;
				}
			case 25:
				{
					o_ptr->art_flags2 |= TR2_SUST_CHA;
					break;
				}
			case 26:
				{
					o_ptr->art_flags3 |= TR3_FEATHER;
					break;
				}
			case 27:
				{
					o_ptr->art_flags3 |= TR3_SEE_INVIS;
					break;
				}
			case 28:
				{
					o_ptr->art_flags3 |= TR3_TELEPATHY;
					break;
				}
			case 29:
				{
					o_ptr->art_flags3 |= TR3_SLOW_DIGEST;
					break;
				}
			case 30:
				{
					o_ptr->art_flags3 |= TR3_REGEN;
					break;
				}
			default:
				{
					break;
				}
			}
		}
	}
}


/*
* Complete the "creation" of an object by applying "magic" to the item
*
* This includes not only rolling for random bonuses, but also putting the
* finishing touches on ego-items and artefacts, giving charges to wands and
* staffs, giving fuel to lites, and placing traps on chests.
*
* In particular, note that "Instant Artifacts", if "created" by an external
* routine, must pass through this function to complete the actual creation.
*
* The base "chance" of the item being "good" increases with the "level"
* parameter, which is usually derived from the dungeon level, being equal
* to the level plus 10, up to a maximum of 75.  If "good" is true, then
* the object is guaranteed to be "good".  If an object is "good", then
* the chance that the object will be "great" (ego-item or artefact), also
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
* a chance that an artefact will be created.  This is true even if both the
* "good" and "great" arguments are false.  As a total hack, if "great" is
* true, then the item gets 3 extra "attempts" to become an artefact.
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

	/* Roll for artefacts if allowed */
	for (i = 0; i < rolls; i++)
	{
		/* Roll for an artefact */
		if (make_artefact(o_ptr)) break;
	}

	/* Hack -- analyze artefacts */
	if (o_ptr->name1)
	{
		artefact_type *a_ptr = &a_info[o_ptr->name1];

		/* Hack -- Mark the artefact as "created" */
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
		if (a_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

		/* Mega-Hack -- increase the rating */
		rating += 10;

		/* Mega-Hack -- increase the rating again */
		if (a_ptr->cost > 50000L) rating += 10;

		/* Set the good item flag */
		good_item_flag = TRUE;

		/* Debug -- peek at the item */
		if (debug_peek) object_mention(o_ptr);

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
			if (power ||
				((o_ptr->tval == TV_HELM) && (o_ptr->sval == SV_DRAGON_HELM)) ||
				((o_ptr->tval == TV_SHIELD) && (o_ptr->sval == SV_DRAGON_SHIELD))
				|| ((o_ptr->tval == TV_CLOAK) && (o_ptr->sval == SV_ELVEN_CLOAK)))
				a_m_aux_2(o_ptr, lev, power);
			break;
		}

	case TV_RING:
	case TV_AMULET:
		{
			if (!power && (rand_int(100) < 50)) power = -1;
			a_m_aux_3(o_ptr, lev, power);
			break;
		}

	case TV_LITE:
		{
			a_m_aux_5(o_ptr, lev, power);
			break;
		}

	default:
		{
			a_m_aux_4(o_ptr, lev, power);
			break;
		}
	}

	if (o_ptr->art_name) rating += 40;

	/* Hack -- analyze ego-items */
	else if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		/* Hack -- extra powers */
		switch (o_ptr->name2)
		{
			/* Weapon (Holy Avenger) */
		case EGO_HA:
			{
				o_ptr->xtra1 = EGO_XTRA_SUSTAIN;
				break;
			}

			/* Weapon (Defender) */
		case EGO_DF:
			{
				o_ptr->xtra1 = EGO_XTRA_SUSTAIN;
				break;
			}

			/* Weapon (Blessed) */
		case EGO_BLESS_BLADE:
			{
				o_ptr->xtra1 = EGO_XTRA_ABILITY;
				break;
			}

			/* Planar weapon */
		case EGO_PLANAR:
			{
				if (randint(7)==1) o_ptr->xtra1 = EGO_XTRA_ABILITY;
				break;
			}

			/* Robe of Permanance */
		case EGO_PERMANENCE:
			{
				o_ptr->xtra1 = EGO_XTRA_POWER;
				break;
			}

			/* Armor of Elvenkind */
		case EGO_ELVENKIND:
			{
				o_ptr->xtra1 = EGO_XTRA_POWER;
				break;
			}

			/* Crown of the Magi */
		case EGO_MAGI:
			{
				o_ptr->xtra1 = EGO_XTRA_ABILITY;
				break;
			}

			/* Cloak of Aman */
		case EGO_AMAN:
			{
				o_ptr->xtra1 = EGO_XTRA_POWER;
				break;
			}
		}

		/* Randomize the "xtra" power */
		if ((o_ptr->xtra1) && (!(o_ptr->art_name)))
			o_ptr->xtra2 = (byte_hack)randint(256);

		/* Hack -- acquire "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (e_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

		/* Hack -- apply extra penalties if needed */
		if (cursed_p(o_ptr) || broken_p(o_ptr))
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h) o_ptr->to_h -= rand_s16b(e_ptr->max_to_h);
			if (e_ptr->max_to_d) o_ptr->to_d -= rand_s16b(e_ptr->max_to_d);
			if (e_ptr->max_to_a) o_ptr->to_a -= rand_s16b(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval) o_ptr->pval -= rand_s16b(e_ptr->max_pval);
		}

		/* Hack -- apply extra bonuses if needed */
		else
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h) o_ptr->to_h += rand_s16b(e_ptr->max_to_h);
			if (e_ptr->max_to_d) o_ptr->to_d += rand_s16b(e_ptr->max_to_d);
			if (e_ptr->max_to_a) o_ptr->to_a += rand_s16b(e_ptr->max_to_a);

			/* Hack -- obtain pval */
			if (e_ptr->max_pval) o_ptr->pval += rand_s16b(e_ptr->max_pval);
		}

		/* Hack -- apply rating bonus */
		rating += e_ptr->rating;

		/* Debug -- describe the item */
		if (debug_peek) object_mention(o_ptr);

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
		if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);
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
	case TV_POTION:
		{
			if (k_ptr->sval >= SV_POTION_INC_STR && k_ptr->sval <= SV_POTION_INC_CHA)
			{
				return TRUE;
			}
			if( k_ptr->sval == SV_POTION_EXPERIENCE) return TRUE;
			if( k_ptr->sval == SV_POTION_INVULNERABILITY) return TRUE;
			if( k_ptr->sval == SV_POTION_AUGMENTATION) return TRUE;
			return FALSE;
		}
	/* Books -- High level books are good (except Books of Charms) */
	case TV_MIRACLES_BOOK:
	case TV_SORCERY_BOOK:
	case TV_NATURE_BOOK:
	case TV_CHAOS_BOOK:
	case TV_DEATH_BOOK:
	case TV_TAROT_BOOK:
	case TV_SOMATIC_BOOK:
    case TV_DEMONIC_BOOK:
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

		/* Amulets -- Amulets of the Magi and Resistance are good */
	case TV_AMULET:
		{
			if (k_ptr->sval == SV_AMULET_THE_MAGI) return (TRUE);
			if (k_ptr->sval == SV_AMULET_RESISTANCE) return (TRUE);
			return (FALSE);
		}
	}

	/* Assume not good */
	return (FALSE);
}



/*
* Attempt to make an object (normal or good/great)
*
* This routine plays nasty games to generate the "special artefacts".
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
	if ((rand_int(prob) != 0) || !make_artefact_special(j_ptr))
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

	/* Apply magic (allow artefacts) */
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
		(k_info[j_ptr->k_idx].level > dun_level))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - dun_level);

		/* Debug -- peek at items */
		if (debug_peek) object_mention(j_ptr);
	}

	/* Success */
	return (TRUE);
}



/*
* Attempt to place an object (normal or good/great) at the given location.
*
* This routine plays nasty games to generate the "special artefacts".
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
	else
	{
		/* Hack -- Preserve artefacts */
		if (q_ptr->name1)
		{
			a_info[q_ptr->name1].cur_num = 0;
		}
	}

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
	j_ptr->pval = (s16b)(base + (8L * rand_s16b(base)) + rand_s16b(8));

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
	if (!(j_ptr->art_name || artefact_p(j_ptr)) && (rand_int(100) < chance))
	{
		/* Message */
		msg_format("The %s disappear%s.",
			o_name, (plural ? "" : "s"));

		/* Debug */
		if (debug_mode) msg_print("(breakage)");

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
	if (!flag && !(artefact_p(j_ptr) || j_ptr->art_name))
	{
		/* Message */
		msg_format("The %s disappear%s.",
			o_name, (plural ? "" : "s"));

		/* Debug */
		if (debug_mode) msg_print("(no floor space)");

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
		if (debug_mode) msg_print("(too many objects)");

		/* Hack -- Preserve artefacts */
		if (j_ptr->name1)
		{
			a_info[j_ptr->name1].cur_num = 0;
		}

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
		if ((feat == FEAT_TRAP_HEAD + 0x00) && (dun_level >= MAX_DEPTH)) continue;

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
	object_type     *o_ptr = &inventory[item];

	char    o_name[80];

	/* Get a description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Print a message */
	msg_format("You have %s.", o_name);
}

/*
 * Distribute charges of rods, staves, or wands.
 *
 * o_ptr = source item
 * q_ptr = target item, must be of the same type as o_ptr
 * amt   = number of items that are transfered
 */
void distribute_charges(object_type *o_ptr, object_type *q_ptr, int amt)
{
	/*
	 * Hack -- If rods, staves, or wands are dropped, the total maximum
	 * timeout or charges need to be allocated between the two stacks.
	 * If all the items are being dropped, it makes for a neater message
	 * to leave the original stack's pval alone. -LM-
	 */
	if ((o_ptr->tval == TV_WAND) ||
	    (o_ptr->tval == TV_STAFF))
	{
		q_ptr->pval = o_ptr->pval * amt / o_ptr->number;

		if (amt < o_ptr->number) o_ptr->pval -= q_ptr->pval;
	}

	/*
	 * Hack -- Rods also need to have their timeouts distributed.
	 *
	 * The dropped stack will accept all time remaining to charge up to
	 * its maximum.
	 * TODO: fix this so rods work as well, this is crap...

	if (o_ptr->tval == TV_ROD)
	{
		max_time = k_info[o_ptr->k_idx].time_base * amt;

		if (o_ptr->timeout > max_time)
			q_ptr->timeout = max_time;
		else
			q_ptr->timeout = o_ptr->timeout;

		if (amt < o_ptr->number)
			o_ptr->timeout -= q_ptr->timeout;
	}
	*/
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

	/* Change the number and weight and charges*/
	if (num)
	{
		/* Change charges if we are removing some wands*/
		if(num<0 && num!=-o_ptr->number && o_ptr->tval==TV_WAND)
		{
			o_ptr->pval = o_ptr->pval + ( o_ptr->pval / o_ptr->number * num );
		}

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
	object_type     *o_ptr = &o_list[item];

	char    o_name[80];

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

	object_type     *j_ptr;


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
		s32b            o_value, j_value;

		/* Get the "value" of the item */
		o_value = object_value(o_ptr);

		/* Scan every occupied slot */
		for (j = 0; j < INVEN_PACK; j++)
		{
			j_ptr = &inventory[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;

			/* Hack -- readable books always come first */
			if ((o_ptr->tval == p_ptr->realm1+89) &&
				(j_ptr->tval != p_ptr->realm1+89)) break;
			if ((j_ptr->tval == p_ptr->realm1+89) &&
				(o_ptr->tval != p_ptr->realm1+89)) continue;

			if ((o_ptr->tval == p_ptr->realm2+89) &&
				(j_ptr->tval != p_ptr->realm2+89)) break;
			if ((j_ptr->tval == p_ptr->realm2+89) &&
				(o_ptr->tval != p_ptr->realm2+89)) continue;

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



			/* Hack:  otherwise identical rods sort by
			increasing recharge time --dsb */
			if (o_ptr->tval == TV_ROD) {
				if (o_ptr->pval < j_ptr->pval) break;
				if (o_ptr->pval > j_ptr->pval) continue;
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

	/* Modify charges for wands */
	if(o_ptr->tval==TV_WAND)
	{
		q_ptr->pval = o_ptr->pval / o_ptr->number * q_ptr->number;
	}

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
*/
void reorder_pack(void)
{
	int i, j, k;

	s32b o_value;
	s32b j_value;

	object_type     forge;
	object_type *q_ptr;

	object_type *j_ptr;
	object_type *o_ptr;

	bool flag = FALSE;


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
			if ((o_ptr->tval == p_ptr->realm1+89) &&
				(j_ptr->tval != p_ptr->realm1+89)) break;
			if ((j_ptr->tval == p_ptr->realm1+89) &&
				(o_ptr->tval != p_ptr->realm1+89)) continue;

			if ((o_ptr->tval == p_ptr->realm2+89) &&
				(j_ptr->tval != p_ptr->realm2+89)) break;
			if ((j_ptr->tval == p_ptr->realm2+89) &&
				(o_ptr->tval != p_ptr->realm2+89)) continue;

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


			/* Hack:  otherwise identical rods sort by
			increasing recharge time --dsb */
			if (o_ptr->tval == TV_ROD) {
				if (o_ptr->pval < j_ptr->pval) break;
				if (o_ptr->pval > j_ptr->pval) continue;
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
		q_ptr = &forge;

		/* Save a copy of the moving item */
		object_copy(q_ptr, &inventory[i]);

		/* Slide the objects */
		for (k = i; k > j; k--)
		{
			/* Slide the item */
			object_copy(&inventory[k], &inventory[k-1]);
		}

		/* Insert the moving item */
		object_copy(&inventory[j], q_ptr);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* Message */
	if (flag) msg_print("You reorder some items in your pack.");
}


/*
* XXX XXX XXX XXX
*/
extern void mindcraft_info(char *p, int power);

void spoil_spells( object_type *o_ptr )
{
	int		spell = -1;
	magic_type	s_magic;
	magic_type	*s_ptr = &s_magic;

	/* Wait for key */
	while (1)
	{
		char c;
		/* Get Key Press*/
		c = inkey();
		/* Lowercase it with paranoia*/
		if (isupper(c)) c = tolower(c);
		/* Extract request */
		spell = (islower(c) ? A2I(c) : -1);
		if ((spell < 0) || (spell >= 8 ))
		{
			break;
		}

		/* Clear lines, position cursor (really should use strlen here) */
		Term_erase(13, 11 , 255);
		Term_erase(13, 12 , 255);
		Term_erase(13, 13 , 255);

		/*msg_format( "Spell : %d" , spell );msg_print(NULL);*/
		/* Access the spell */
		get_extended_spell_info(  (o_ptr->tval-TV_MIRACLES_BOOK) ,  o_ptr->sval*8+spell , s_ptr );
		/*msg_format( "Spell : %s" , s_ptr->spoiler );msg_print(NULL);		*/
		/* Display that spell's information. */
		c_roff(TERM_L_BLUE, s_ptr->spoiler,13,11,13);
	}

	/* Restore the screen */
	Term_load();

}

/*
 * Hack -- Display all known spells in a window
 *
 * XXX XXX XXX Need to analyze size of the window.
 *
 * XXX XXX XXX Need more colour coding.
 */
void display_spell_list(void)
{
	int             i, j;
	int             y, x;
	int             use_realm1 = p_ptr->realm1 - 1;
	int             use_realm2 = p_ptr->realm2 - 1;
	char            name[80];
	char            out_val[160];
	char            out_val2[160];
	int             w,h,n;
    magic_type   s_magic;
    magic_type  *s_ptr = &s_magic;

	/* Get size of terminal*/
	Term_get_size(&w,&h);

	/* Erase window */
	clear_from(0);

	/* Warriors are illiterate */
	if (!mp_ptr->spell_book) return;

	/* Orphic spell-list */
	if (p_ptr->pclass == CLASS_ORPHIC)
	{
		int             i;
		int             y = 1;
		int             x = 1;
		int             minfail = 0;
		int             plev = p_ptr->lev;
		int             chance = 0;
		mindcraft_power spell;
		char            comment[80];
		char            psi_desc[80];

		/* Display a list of spells */
		prt("", y, x);
		put_str("Name", y, x + 5);
		put_str("Lv Mana Fail Info", y, x + 35);

		/* Dump the spells */
		for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
		{
			byte a  = TERM_WHITE;

			/* Access the available spell */
			spell = mindcraft_powers[i];
			if (spell.min_lev > plev) break;

			/* Get the failure rate */
			chance = spell.fail;

			/* Reduce failure rate by "effective" level adjustment */
			chance -= 3 * (p_ptr->lev - spell.min_lev);

			/* Reduce failure rate by INT/WIS adjustment */
			chance -= 3 * (adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_INTWIS] - 1);

			/* Not enough mana to cast */
			if (spell.mana_cost > p_ptr->csp)
			{
				chance += 5 * (spell.mana_cost - p_ptr->csp);
				a = TERM_ORANGE;
			}

			/* Extract the minimum failure rate */
			minfail = adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_FAILURE];

			/* Minimum failure rate */
			if (chance < minfail) chance = minfail;

			/* Stunning makes spells harder */
			if (p_ptr->stun > 50) chance += 25;
			else if (p_ptr->stun) chance += 15;

			/* Always a 5 percent chance of working */
			if (chance > 95) chance = 95;

			/* Get info */
			mindcraft_info(comment, i);

			/* Dump the spell */
			sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
					I2A(i), spell.name,
					spell.min_lev, spell.mana_cost, chance, comment);
			Term_putstr(x, y + i + 1, -1, a, psi_desc);
		}
		return;
	}

	/* Normal spellcaster with books */

	y = 0;
	x = 0;
    n = 0;
	for (j = 0; j < (use_realm2>-1?2:1); j++)
	{
		/*Get the realm, instead of trinary overload */
		u16b realm = (j<1?use_realm1:use_realm2);
		/*Get the color of the realm, used to print the realm and the prefix "book/spell) "*/
		byte a_prefix = tval_to_attr[ TV_MIRACLES_BOOK + realm ];
		/*Get the realm with square brackets in out_val*/
		sprintf(out_val, "[%s]", realm_names[realm+1].name );
		/* Dump outval on the window onto the window */
		Term_putstr(x,   y + n, -1, a_prefix, out_val);
		/* Next */
		n++;

		/* Scan spells */
		for (i = 0; i < 32; i++)
		{
			/* Use the realm color initially, this can be overwritten by the status of the spell */
			byte a =a_prefix;

			/*Tabulate spell list if needed ( most likely will always be neede ) */
			if( (y + n) == h )
			{
				x = x + 27;
				n = 0;
			}

			/* Access the spell */
			/*get_extended_spell_info( j<1?use_realm1:use_realm2, i%32 , s_ptr );*/
			get_extended_spell_info( realm, i%32 , s_ptr );
			/* Write out the spell name */
			strcpy(name, s_ptr->name);
			/* Get the attribute of the info*/
			a = s_ptr->attr_info;

			/* Dump the spell --(-- */

			sprintf(out_val, "%c/%c) ",	I2A(i/8), I2A(i%8) );
			sprintf(out_val2, "%-20.20s", name);

			/* Dump onto the window */
			Term_putstr(x,   y + n, -1, a_prefix, out_val);
			Term_putstr(x+5, y + n, -1, a,        out_val2);

			/* Next */
			n++;
		}
	}
}

/*
* Returns spell chance of failure for spell            -RAK-
*/
s16b spell_chance(int spell,int realm)
{
	int             chance, minfail;
    magic_type   s_magic;
    magic_type  *s_ptr = &s_magic;

	/* Paranoia -- must be literate */
	if (!mp_ptr->spell_book) return (100);

	/* Access the spell */
	get_extended_spell_info( realm, spell , s_ptr );

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_INTWIS] - 1);

	/* Not enough mana to cast */
	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_FAILURE];

	/* Non mage/priest characters never get too good (added high mage,
	orphic,druid) */
	if ((p_ptr->pclass != CLASS_PRIEST)
		&& (p_ptr->pclass != CLASS_DRUID)
		&& (p_ptr->pclass != CLASS_MAGE)
		&& (p_ptr->pclass != CLASS_ORPHIC)
		&& (p_ptr->pclass != CLASS_HIGH_MAGE)
		&& (p_ptr->pclass != CLASS_BLOOD_MAGE)
		&& (p_ptr->pclass != CLASS_WARLOCK))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
	if (((p_ptr->pclass == CLASS_PRIEST) || (p_ptr->pclass == CLASS_DRUID)) && (p_ptr->icky_wield)) chance += 25;

	/* Hack -- Warlock spell penalty for *any* weapons */
	if ((p_ptr->pclass == CLASS_WARLOCK) && (p_ptr->icky_wield)) chance += 25;

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
bool spell_okay(int spell, bool known, int realm)
{
    magic_type   s_magic;
    magic_type  *s_ptr = &s_magic;

	/* Access the spell */
	get_extended_spell_info( realm, spell , s_ptr );

	/*
	msg_format("TODO : Checking for %s , forgotten %d , learned %d" , s_ptr->name , s_ptr->forgotten , s_ptr->learned);
	msg_format( "Learned hardcore : %d" ,spell_learned( realm , spell ) );
	msg_format( "realm 1:%d realm2: %d realm:%d, spell:%d , shifted:%d : check %d" , p_ptr->realm1 , p_ptr->realm2 , realm , spell, 1L<<spell , (spell_learned1 & (1L << spell)) );
	*/

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev)
		return (FALSE);

	/* Spell is forgotten */
	if ( s_ptr->forgotten )
		return (FALSE);

	/* Spell is learned -> Okay to cast, not to study*/
	if ( s_ptr->learned )
		return (known);

	/* Okay to study, not to cast */
	return (!known);
}

/*
* Extra information on a spell         -DRS-
*
* We can use up to 14 characters of the buffer 'p'
*/
void spell_info_short(char *p, int spell, int realm)
{
	char *previous;
	double worstcase;
	double bestcase;
	char macro[80];

	/* Default */
	strcpy(p, "");
	/* If no macro is defined, then dont try to get info */
	if( spells[realm][spell].macro == NULL  )
		return;
	/* A macro is defined , so we need to parse it twice to get upper and lower values*/
	/* Copy the const char over to something local to prevent warnings */
	my_strcpy(macro, spells[realm][spell].macro, 80);
	/*Point at the local script*/
	script = macro;
	/*Evaluate*/
	while(*script)
	{
		/*Get worst case*/
		dice_mode = WORST_CASE;
		previous = script;
		eval_script(&worstcase);
		/*Get best case */
		dice_mode = BEST_CASE;
		script = previous;
		eval_script(&bestcase);
		/*Show & Tell*/
		/*msg_format("Answer for %s is: %.0f", variable_token, answer);	*/
		if( worstcase == -1 )
			sprintf (p, "%s%s ", p , variable_token);
		else if( worstcase != bestcase )
			sprintf (p, "%s%s %.0f-%.0f ", p , variable_token ,worstcase , bestcase);
		else
			sprintf (p, "%s%s %.0f ", p , variable_token ,worstcase);
		/*There might be more*/
		if(*script==';')script++;
	}
}

/*
* Print a list of spells (for browsing or casting or viewing)
*/
void print_spells(byte *spells, int num, int y, int x, int realm)
{
	int             i, spell;
	char            out_val[160];
    magic_type      s_magic;
    magic_type      *s_ptr = &s_magic;

	if ((realm<0 || realm>MAX_REALM - 1) && debug_mode)
		msg_print ("Warning! print_spells called with null realm");

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
		get_extended_spell_info( realm, spell , s_ptr );

		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), "(illegible)");
			prt(out_val, y + i + 1, x);
			continue;
		}

        /* Print index, name, level, mana cost, odds to cast well and info */
        sprintf(out_val, "  %c) %-30s%2d %4d %3d%% %s",	I2A(i), s_ptr->name, s_ptr->slevel, s_ptr->smana, spell_chance(spell,realm), s_ptr->info );

		prt(out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
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

	object_kind *k_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	/* Look for it */
	for (k = 1; k < MAX_K_IDX; k++)
	{
		k_ptr = &k_info[k];

		/* Found a match */
		if ((k_ptr->tval == TV_POTION) && (k_ptr->sval == sval)) break;
	}

	/* Paranoia */
	if (k == MAX_K_IDX) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create fake object */
	object_prep(i_ptr, k);

	/* Describe the object */
	if (k_ptr->aware  || debug_mode == TRUE)
	  strcpy(o_name1, (k_name + k_ptr->name));
	else
	  object_desc(o_name1, i_ptr, FALSE, 0);

	/* Look for first component */
	if (potion_alch[sval].known1 || debug_mode == TRUE)
	{
		for (k = 1; k < MAX_K_IDX; k++)
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
		if (k_ptr->aware || debug_mode == TRUE)
		  strcpy(o_name2,(k_name + k_ptr->name));
		else
		  object_desc(o_name2, i_ptr, FALSE, 0);
	}
	else
	{
		strcpy (o_name2,"????");
	}

	/* Look for first component */
	if (potion_alch[sval].known2 || debug_mode == TRUE)
	{
		/* Look for second component */
		for (k = 1; k < MAX_K_IDX; k++)
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
		if (k_ptr->aware || debug_mode == TRUE)
		  strcpy(o_name3,(k_name + k_ptr->name));
		else
		  object_desc(o_name3, i_ptr, FALSE, 0);
	}
	else
	{
		strcpy (o_name3, "????");
	}

	/* Print a message */
	strnfmt(buf, max, "  %s =  %s +  %s (odds: %d%%)", o_name1, o_name2, o_name3 , (int)(25 + (/*p_ptr->skill[SK_ALC]*/ 60) - ( k_ptr->pval /  5)) );
}
