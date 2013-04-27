/* File: object2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "option.h"
#include "tvalsval.h"

#include "POD.hpp"

using namespace zaiband;

/*
 * There is a 1/20 (5%) chance of inflating the requested object_level
 * during the creation of an object (see "get_obj_num()" in "object.c").
 * Lower values yield better objects more often.
 */
#define GREAT_OBJ	20

/*
 * There is a 1/20 (5%) chance that ego-items with an inflated base-level are
 * generated when an object is turned into an ego-item (see make_ego_item()
 * in object2.c). As above, lower values yield better ego-items more often.
 */
#define GREAT_EGO	20

/*
 * A "stack" of items is limited to less than 100 items (hard-coded).
 */
#define MAX_STACK_SIZE			100



/*
 * Excise a dungeon object from any stacks
 */
static void excise_object_idx(int o_idx)
{
	object_type *j_ptr = &o_list[o_idx];	/* Object */
	s16b this_o_idx, next_o_idx = 0;
	s16b prev_o_idx = 0;

	/* Monster */
	if (j_ptr->held_m_idx)
	{
		monster_type *m_ptr= &mon_list[j_ptr->held_m_idx];	/* Monster */

		/* Scan all objects in the grid */
		for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

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
					object_type *i_ptr = &o_list[prev_o_idx];	/* Previous object */

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
		int y = j_ptr->loc.y;
		int x = j_ptr->loc.x;

		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

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
					o_list[prev_o_idx].next_o_idx = next_o_idx;	/* Remove from list */
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
	object_type *j_ptr = &o_list[o_idx]; /* Object */

	/* Excise */
	excise_object_idx(o_idx);

	/* Dungeon floor */
	if (!(j_ptr->held_m_idx))
	{
		/* Visual update */
		lite_spot(j_ptr->loc);
	}

	/* Wipe the object */
	WIPE(j_ptr);

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
		object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Wipe the object */
		WIPE(o_ptr);

		/* Count objects */
		o_cnt--;
	}

	/* Objects are gone */
	cave_o_idx[y][x] = 0;

	/* Visual update */
	lite_spot(coord(x, y));
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
		monster_type *m_ptr = &mon_list[o_ptr->held_m_idx];	/* Get the monster */

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
		y = o_ptr->loc.y;
		x = o_ptr->loc.x;

		/* Repair grid */
		if (cave_o_idx[y][x] == i1)
		{
			/* Repair */
			cave_o_idx[y][x] = i2;
		}
	}


	
	o_list[i2] = o_list[i1];	/* move object */
	WIPE(o_ptr);				/* wipe hole */
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
	int py = p_ptr->loc.y;
	int px = p_ptr->loc.x;

	int i, num, cnt;
	coord obj_loc;

	int cur_lev, cur_dis, chance;


	/* Compact */
	if (size)
	{
		/* Message */
		msg_print("Compacting objects...");

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);
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
			object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];

			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			/* Hack -- High level objects start out "immune" */
			if (k_ptr->level > cur_lev) continue;

			/* Monster */
			if (o_ptr->held_m_idx)
			{
				monster_type *m_ptr = &mon_list[o_ptr->held_m_idx];	/* Get the monster */

				/* Get the location */
				obj_loc = m_ptr->loc;

				/* Monsters protect their objects */
				if (!one_in_(10)) continue;
			}

			/* Dungeon */
			else
			{
				/* Get the location */
				obj_loc = o_ptr->loc;
			}

			/* Nearby objects start out "immune" */
			if ((cur_dis > 0) && (distance(py, px, obj_loc.y, obj_loc.x) < cur_dis)) continue;

			/* Saving throw */
			chance = 90;

			/* Hack -- only compact artifacts in emergencies */
			if (o_ptr->is_artifact() && (cnt < 1000)) chance = 100;

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

static bool kill_object_invisibly(object_type& o)
{
	/* Mega-Hack -- preserve artifacts */
	if (!character_dungeon || OPTION(adult_preserve))
	{
		/* Hack -- Preserve unknown artifacts */
		if (o.is_artifact() && !o.known())
		{
			/* Mega-Hack -- Preserve the artifact */
			object_type::a_info[o.name1].cur_num = 0;
		}
	}

	/* Monster */
	if (o.held_m_idx)
	{
		mon_list[o.held_m_idx].hold_o_idx = 0;	/* Hack -- see below */
	}

	/* Dungeon */
	else
	{
		/* Hack -- see below */
		cave_o_idx[o.loc.y][o.loc.x] = 0;
	}

	/* Wipe the object */
	WIPE(&o);	
	return false;
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
	/* Delete the existing objects */
	object_scan(kill_object_invisibly);

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
		object_type *o_ptr = &o_list[i];	/* Get the object */

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
 */
static errr get_obj_num_prep(int_test* get_obj_num_hook)
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
		if (one_in_(GREAT_OBJ))
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
		k_ptr = &object_type::k_info[k_idx];

		/* Hack -- prevent embedded chests */
		if (opening_chest && (k_ptr->obj_id.tval == TV_CHEST)) continue;

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
 *
 * These attributes include tohit, todam, toac, cost, and pval (charges).
 *
 * Note that "knowing" an object gives you everything that an "awareness"
 * gives you, and much more.  In fact, the player is always "aware" of any
 * item which he "knows", except items in stores.
 *
 * But having knowledge of, say, one "wand of wonder", does not, by itself,
 * give you knowledge, or even awareness, of other "wands of wonder".
 * It happens that most "identify" routines (including "buying from a shop")
 * will make the player "aware" of the object as well as "know" it.
 *
 * This routine also removes any inscriptions generated by "feelings".
 */
void object_known(object_type *o_ptr)
{
	/* Remove special inscription, if any */
	o_ptr->pseudo = 0;

	/* The object is not "sensed", and isn't merely empty */
	o_ptr->ident &= ~(IDENT_SENSE | IDENT_EMPTY);

	/* Now we know about the item */
	o_ptr->ident |= (IDENT_KNOWN);
}





/*
 * The player is now aware of the effects of the given object.
 */
void object_aware(object_type *o_ptr)
{
	/* Fully aware of the effects */
	object_type::k_info[o_ptr->k_idx].aware = TRUE;

	/* MEGA-HACK - scrolls can change the graphics when becoming aware */
	if (o_ptr->obj_id.tval == TV_SCROLL) p_ptr->redraw |= (PR_MAP);
}



/*
 * Something has been "sampled"
 */
void object_tried(object_type *o_ptr)
{
	/* Mark it as tried (even if "aware") */
	object_type::k_info[o_ptr->k_idx].tried = TRUE;
}


/*
 * Determine if a weapon is 'blessed'
 */
bool is_blessed(const object_type *o_ptr)
{
	u32b f[OBJECT_FLAG_STRICT_UB];

	/* Get the flags */
	object_flags(o_ptr, f);

	/* Is the object blessed? */
	return (f[2] & TR3_BLESSED);
}



/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(const object_type *o_ptr)
{
	static const POD_pair<byte, byte> value_base[]
		=	{	{TV_FOOD, 5},
				{TV_POTION, 20},
				{TV_SCROLL, 20},
				{TV_STAFF, 70},
				{TV_WAND, 50},
				{TV_ROD, 90},
				{TV_RING, 45},
				{TV_AMULET, 45}
			};

	object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];

	/* Use template cost for aware or known objects */
	if (o_ptr->aware() || o_ptr->known()) return (k_ptr->cost);

	/* Analyze the type */
	return lookup(value_base+0, sizeof(value_base), o_ptr->obj_id.tval);
}

/* synchronize probability adjustments against enchant_table[16] */
/* problems if plus1*plus1 overflows in int */
/* this isn't a complete solution */
enum enchant_cost_list
{
	COSTPLUS1 = 100L,
	COSTPLUS2 = COSTPLUS1 + (COSTPLUS1*100L/99L),
	COSTPLUS3 = COSTPLUS2 + (COSTPLUS1*100L/95L),
	COSTPLUS4 = COSTPLUS3 + (COSTPLUS1*100L/90L),
	COSTPLUS5 = COSTPLUS4 + (COSTPLUS1*100L/80L),
	COSTPLUS6 = COSTPLUS5 + (COSTPLUS1*100L/70L),
	COSTPLUS7 = COSTPLUS6 + (COSTPLUS1*100L/60L),
	COSTPLUS8 = COSTPLUS7 + (COSTPLUS1*100L/50L),
	COSTPLUS9 = COSTPLUS8 + (COSTPLUS1*100L/30L),
	COSTPLUS10 = COSTPLUS9 + (COSTPLUS1*100L/5L),
	COSTPLUS11 = COSTPLUS10 + (COSTPLUS1*100L/1L),
	COSTPLUS12 = COSTPLUS11 + (COSTPLUS1*1000L/8L),
	COSTPLUS13 = COSTPLUS12 + (COSTPLUS1*1000L/5L),
	COSTPLUS14 = COSTPLUS13 + (COSTPLUS1*1000L/3L),
	COSTPLUS15 = COSTPLUS14 + (COSTPLUS1*1000L/1L)
};

/* Synchronize against: enchant_table[16] */
static const s32b enchant_price_table[16] =
{	0, COSTPLUS1, COSTPLUS2, COSTPLUS3, COSTPLUS4, COSTPLUS5, COSTPLUS6, COSTPLUS7, COSTPLUS8, COSTPLUS9, COSTPLUS10, COSTPLUS11, COSTPLUS12, COSTPLUS13, COSTPLUS14, COSTPLUS15 };

static s32b plus_value(s16b bonus)
{
	if (0>=bonus)
		return bonus*COSTPLUS1;				/* deduct cost of enchanting up */
	else if (15<bonus)
		return COSTPLUS15*(bonus - 14);		/* pretend unreachable pluses are as expensive as +15, each */
	else
		return enchant_price_table[bonus];	/* look up the cost */
}

/*
 * Return the "real" price of a "known" item.
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

	u32b f[OBJECT_FLAG_STRICT_UB];

	object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];


	/* Hack -- "worthless" items */
	if (!k_ptr->cost) return (0L);

	/* Base cost */
	value = k_ptr->cost;


	/* Extract some flags */
	object_flags(o_ptr, f);


	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];

		/* Hack -- "worthless" artifacts */
		if (!a_ptr->cost) return (0L);

		/* Hack -- Use the artifact cost instead */
		value = a_ptr->cost;
	}

	/* Ego-Item */
	else if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &object_type::e_info[o_ptr->name2];

		/* Hack -- "worthless" ego-items */
		if (!e_ptr->cost) return (0L);

		/* Hack -- Reward the ego-item with a bonus */
		value += e_ptr->cost;
	}


	/* Analyze pval bonus, if there is a pval */
	if (o_ptr->pval)
	{
		switch (o_ptr->obj_id.tval)
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

				/* Give credit for stat bonuses */
				if (f[0] & (TR1_STR)) value += (o_ptr->pval * 200L);
				if (f[0] & (TR1_INT)) value += (o_ptr->pval * 200L);
				if (f[0] & (TR1_WIS)) value += (o_ptr->pval * 200L);
				if (f[0] & (TR1_DEX)) value += (o_ptr->pval * 200L);
				if (f[0] & (TR1_CON)) value += (o_ptr->pval * 200L);
				if (f[0] & (TR1_CHR)) value += (o_ptr->pval * 200L);

				/* Give credit for stealth and searching */
				if (f[0] & (TR1_STEALTH)) value += (o_ptr->pval * 100L);
				if (f[0] & (TR1_SEARCH)) value += (o_ptr->pval * 100L);

				/* Give credit for infra-vision and tunneling */
				if (f[0] & (TR1_INFRA)) value += (o_ptr->pval * 50L);
				if (f[0] & (TR1_TUNNEL)) value += (o_ptr->pval * 50L);

				/* Give credit for extra attacks */
				if (f[0] & (TR1_BLOWS)) value += (o_ptr->pval * 2000L);

				/* Give credit for speed bonus */
				if (f[0] & (TR1_SPEED)) value += (o_ptr->pval * 30000L);

				break;
			}
			/* Wands/Staffs */
			case TV_WAND:
			case TV_STAFF:
			{
				/* Pay extra for charges, depending on standard number of charges */
				value += ((value / 20) * (o_ptr->pval / o_ptr->number));

				/* Done */
				break;
			}
		}
	}


	/* Analyze the item */
	switch (o_ptr->obj_id.tval)
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
			/* as these aren't enchantable... */
			value += plus_value(o_ptr->to_h);
			value += plus_value(o_ptr->to_d);
			value += plus_value(o_ptr->to_a);

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
			/* Give credit for bonuses */
			value += plus_value(o_ptr->to_h - k_ptr->to_h);
			value += plus_value(o_ptr->to_d - k_ptr->to_d);
			value += plus_value(o_ptr->to_a);

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

			/* Give credit for bonuses */
			value += plus_value(o_ptr->to_h);
			value += plus_value(o_ptr->to_d);
			value += plus_value(o_ptr->to_a);

			/* Hack -- Factor in extra damage dice */
			if ((o_ptr->d.dice > k_ptr->d.dice) && (o_ptr->d.sides == k_ptr->d.sides))
			{
				value += (o_ptr->d.dice - k_ptr->d.dice) * o_ptr->d.sides * 100L;
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
			value += plus_value(o_ptr->to_h)/20;
			value += plus_value(o_ptr->to_d)/20;

			/* Hack -- Factor in extra damage dice */
			if ((o_ptr->d.dice > k_ptr->d.dice) && (o_ptr->d.sides == k_ptr->d.sides))
			{
				value += (o_ptr->d.dice - k_ptr->d.dice) * o_ptr->d.sides * 5L;
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
 */
s32b object_value(const object_type *o_ptr)
{
	s32b value;


	/* Known items -- acquire the actual value */
	if (o_ptr->known())
	{
		/* Broken items -- worthless */
		/* Cursed items -- worthless */
		if (o_ptr->is_broken_or_cursed()) return (0L);

		/* Real value (see above) */
		value = object_value_real(o_ptr);
	}

	/* Unknown items -- acquire a base value */
	else
	{
		/* Hack -- Felt broken items */
		/* Hack -- Felt cursed items */
		if ((o_ptr->ident & (IDENT_SENSE)) && o_ptr->is_broken_or_cursed()) return (0L);

		/* Base value (see above) */
		value = object_value_base(o_ptr);
	}

	/* Return the final value */
	return (value);
}





/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow weapons/armor to stack, if "known".
 *
 * Missiles will combine if both stacks have the same "known" status.
 * This is done to make unidentified stacks of missiles useful.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests, and activatable items, except rods, never stack (for various
 * reasons).
 */
bool object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Maximal "stacking" limit */
	if (total >= MAX_STACK_SIZE) return FALSE;

	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx) return FALSE;

	/* Analyze the items */
	switch (o_ptr->obj_id.tval)
	{
		/* Chests */
		case TV_CHEST:	return FALSE;	/* Never okay */

		/* Food and Potions and Scrolls */
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:	break;		/* Assume okay */

		/* Staves and Wands */
		case TV_STAFF:
		case TV_WAND:
		{
			/* Require either knowledge or known empty for both wands/staves */
			if ((!(o_ptr->ident & (IDENT_EMPTY)) &&
				!o_ptr->known()) ||
				(!(j_ptr->ident & (IDENT_EMPTY)) &&
				!j_ptr->known())) return(0);

			/* Assume okay */
			break;
		}

		/* Rods */
		case TV_ROD:	break;	/* Assume okay */

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
		case TV_DRAG_ARMOR:	/* Fall through */

		/* Rings, Amulets, Lites */
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			/* Require both items to be known */
			if (!o_ptr->known() || !j_ptr->known()) return (0);

			/* Fall through */
		}

		/* Missiles */
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Require identical knowledge of both items */
			if (o_ptr->known() != j_ptr->known()) return (0);

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

			/* Hack -- Never stack "powerful" items */
			if (o_ptr->xtra1 || j_ptr->xtra1) return (FALSE);

			/* Hack -- Never stack recharging items */
			if (o_ptr->timeout || j_ptr->timeout) return (FALSE);

			/* Require identical "values" */
			if (o_ptr->ac != j_ptr->ac) return (FALSE);
			if (o_ptr->d != j_ptr->d) return (FALSE);

			/* Probably okay */
			break;
		}

		/* Various */
		default:
		{
			/* Require knowledge */
			if (!o_ptr->known() || !j_ptr->known()) return (0);

			/* Probably okay */
			break;
		}
	}


	/* Hack -- Require identical "cursed" and "broken" status */
	if ((o_ptr->ident & (IDENT_CURSED | IDENT_BROKEN)) != (j_ptr->ident & (IDENT_CURSED | IDENT_BROKEN)))
		return (0);


	/* Hack -- Require compatible inscriptions */
	if (o_ptr->note != j_ptr->note)
	{
		/* Normally require matching inscriptions */
		if (!OPTION(stack_force_notes)) return (0);

		/* Never combine different inscriptions */
		if (o_ptr->note && j_ptr->note) return (0);
	}

	/* Hack -- Require compatible special inscriptions */
	if (o_ptr->pseudo != j_ptr->pseudo)
	{
		/* Both are (different) special inscriptions */
		if ((o_ptr->pseudo >= INSCRIP_NULL) &&
		    (j_ptr->pseudo >= INSCRIP_NULL))
		{
			/* Normally require matching inscriptions */
			return (0);
		}

		/* One is a special inscription, one is nothing */
		else if ((o_ptr->pseudo >= INSCRIP_NULL) ||
		         (j_ptr->pseudo >= INSCRIP_NULL))
		{
			/* Normally require matching inscriptions */
			if (!OPTION(stack_force_notes)) return (0);

			/* this shouldn't be happening (load.c zeros legacy discounts), but not worth choking over */
			/* Hack -- Never merge a special inscription with a legacy discount */
			if ((o_ptr->pseudo > 0) && (j_ptr->pseudo > 0)) return (0);
		}

		/* this shouldn't be happening (load.c zeros legacy discounts), but not worth choking over */
		else
		{
			return (0);
		}
	}


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
 * The blending of the pseudo field assumes that either (1) one is a
 * special inscription and one is nothing, or (2) both are nothing.
 * In all of these cases, we can simply use the "maximum" of the two pseudo fields.
 *
 * These assumptions are enforced by the "object_similar()" code.
 */
void object_absorb(object_type *o_ptr, const object_type *j_ptr)
{
	object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];

	int total = o_ptr->number + j_ptr->number;

	/* Add together the item counts */
	o_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Hack -- Blend "known" status */
	if (j_ptr->known()) object_known(o_ptr);

	/* Hack -- Blend store status */
	if (j_ptr->ident & (IDENT_STORE)) o_ptr->ident |= (IDENT_STORE);

	/* Hack -- Blend "mental" status */
	if (j_ptr->ident & (IDENT_MENTAL)) o_ptr->ident |= (IDENT_MENTAL);

	/* Hack -- Blend "notes" */
	if (j_ptr->note != 0) o_ptr->note = j_ptr->note;

	/* Mega-Hack -- Blend special inscriptions */
	if (o_ptr->pseudo < j_ptr->pseudo) o_ptr->pseudo = j_ptr->pseudo;

	/*
	 * Hack -- if rods are stacking, re-calculate the
	 * pvals (maximum timeouts) and current timeouts together
	 */
	if (o_ptr->obj_id.tval == TV_ROD)
	{
		o_ptr->pval = total * k_ptr->pval;
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Hack -- if wands or staves are stacking, combine the charges */
	if ((o_ptr->obj_id.tval == TV_WAND) || (o_ptr->obj_id.tval == TV_STAFF))
	{
		o_ptr->pval += j_ptr->pval;
	}
}



/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_kind(tvalsval src)
{
	int k;

	/* Look for it */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &object_type::k_info[k];

		/* Found a match */
		if (k_ptr->obj_id == src) return (k);
	}

	/* Oops */
	msg_format("No object (%d,%d)", src.tval, src.sval);

	/* Oops */
	return (0);
}

s16b lookup_kind2(byte tval, byte sval)
{
	int k;

	/* Look for it */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &object_type::k_info[k];

		/* Found a match */
		if ((k_ptr->obj_id.tval == tval) && (k_ptr->obj_id.sval == sval)) return (k);
	}

	/* Oops */
	msg_format("No object (%d,%d)", tval, sval);

	/* Oops */
	return (0);
}


/*
 * Prepare an object based on an object kind.
 */
void object_prep(object_type *o_ptr, int k_idx)
{
	assert(0 < k_idx && k_idx < z_info->k_max);
	object_kind *k_ptr = &object_type::k_info[k_idx];

	/* Clear the record */
	WIPE(o_ptr);

	/* Save the kind index */
	o_ptr->k_idx = k_idx;

	/* Efficiency -- tval/sval */
	o_ptr->obj_id = k_ptr->obj_id;

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
	o_ptr->d = k_ptr->d;

	/* Hack -- worthless items are always "broken" */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags[2] & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
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
	object_desc_spoil(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_BASE);

	/* Artifact */
	if (o_ptr->is_artifact())
	{
		msg_format("Artifact (%s)", o_name);
	}

	/* Ego-item */
	else if (o_ptr->is_ego_item())
	{
		msg_format("Ego-item (%s)", o_name);
	}

	/* Normal item */
	else
	{
		msg_format("Object (%s)", o_name);
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
static int make_ego_item(object_type *o_ptr, bool force_uncursed)
{
	ego_item_type *e_ptr;
	alloc_entry *table = alloc_ego_table;
	long value;
	long total = 0L;		/* Reset total */
	int i, j;
	int level = object_level;
	int e_idx;

	/* Fail if object already is ego or artifact */
	if (o_ptr->name1) return FALSE;
	if (o_ptr->name2) return FALSE;

	/* Boost level (like with object base types) */
	if (level > 0)
	{
		/* Occasional "boost" */
		if (one_in_(GREAT_EGO))
		{
			/* The bizarre calculation again */
			level = 1 + (level * MAX_DEPTH / randint(MAX_DEPTH));
		}
	}

	/* Process probabilities */
	for (i = 0; i < alloc_ego_size; i++)
	{
		/* Default */
		table[i].prob3 = 0;

		/* Objects are sorted by depth */
		if (table[i].level > level) break;

		/* Get the actual kind */
		e_ptr = &object_type::e_info[table[i].index];

		/* Avoid cursed items if specified */
		if (force_uncursed && (e_ptr->flags[2] & TR3_LIGHT_CURSE)) continue;

		/* Test if this is a legal ego-item type for this object */
		for (j = 0; j < EGO_TVALS_MAX; j++)
		{
			/* Require identical base type tval */
			/* Require sval in range */
			if (	(o_ptr->obj_id.tval == e_ptr->tval[j])
				&&	(o_ptr->obj_id.sval >= e_ptr->min_sval[j])
				&&	(o_ptr->obj_id.sval <= e_ptr->max_sval[j]))
			{
				table[i].prob3 = table[i].prob2;	/* Accept */
				total += table[i].prob3;			/* Total */
				break;
			}
		}
	}

	/* No legal ego-items -- create a normal unenchanted one */
	if (total == 0) return 0;


	/* Pick an ego-item */
	value = rand_int(total);

	/* Find the object */
	for (i = 0; i < alloc_ego_size; i++)
	{
		if (value < table[i].prob3) break;	/* Found the entry */
		value -= table[i].prob3;			/* Decrement */
	}

	/* We have one */
	e_idx = (byte)table[i].index;
	o_ptr->name2 = e_idx;

	return ((object_type::e_info[e_idx].flags[2] & TR3_LIGHT_CURSE) ? -2 : 2);
}

/** 
 * Copy artifact data to a normal object, and set various slightly hacky 
 * globals. 
 */ 
static void copy_artifact_data(object_type& o, artifact_type& a) 
{ 
	/* Hack -- Mark the artifact as "created" */ 
	a.cur_num = 1; 

	/* Extract the other fields */ 
	o.pval = a.pval; 
	o.ac = a.ac; 
	o.d = a.d; 
	o.to_a = a.to_a; 
	o.to_h = a.to_h; 
	o.to_d = a.to_d; 
	o.weight = a.weight; 

	/* Hack -- extract the "broken" flag */
	if (!a.cost) o.ident |= (IDENT_BROKEN);

	/* Hack -- extract the "cursed" flag */
	if (a.flags[2] & (TR3_LIGHT_CURSE)) o.ident |= (IDENT_CURSED);
 
	/* Mega-Hack -- increase the rating */ 
	rating += 10; 
 
	/* Mega-Hack -- increase the rating again */ 
	if (a.cost > 50000L) rating += 10; 
 
	/* Set the good item flag */ 
	good_item_flag = TRUE; 
 
	/* Cheat -- peek at the item */ 
	if (OPTION(cheat_peek)) object_mention(&o);
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

	if (OPTION(adult_no_artifacts)) return FALSE;	/* No artifacts, do nothing */
	if (!p_ptr->depth) return FALSE;		/* No artifacts in the town */

	/* Check the special artifacts */
	for (i = 0; i < ART_MIN_NORMAL; ++i)
	{
		artifact_type* const a_ptr = &object_type::a_info[i];

		if (!a_ptr->_name) continue;	/* Skip "empty" artifacts */
		if (a_ptr->cur_num) continue;	/* Cannot make an artifact twice */

		/* Enforce minimum "depth" (loosely) */
		if (a_ptr->level > p_ptr->depth)
		{
			/* Get the "out-of-depth factor" */
			int d = (a_ptr->level - p_ptr->depth) * 2;

			/* Roll for out-of-depth creation */
			if (!one_in_(d)) continue;
		}

		if (!one_in_(a_ptr->rarity)) continue;	/* Artifact "rarity roll" */

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->obj_id);

		/* Enforce minimum "object" level (loosely) */
		if (object_type::k_info[k_idx].level > object_level)
		{
			/* Get the "out-of-depth factor" */
			int d = (object_type::k_info[k_idx].level - object_level) * 5;

			if (!one_in_(d)) continue;	/* Roll for out-of-depth creation */
		}

		object_prep(o_ptr, k_idx);	/* Assign the template */
		o_ptr->name1 = i;			/* Mark the item as an artifact */

		/* Copy across critical data from the artifact struct */
		copy_artifact_data(*o_ptr, *a_ptr); 

		return TRUE;	/* Success */
	}

	return FALSE;	/* Failure */
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

	if (OPTION(adult_no_artifacts)) return FALSE;	/* No artifacts, do nothing */
	if (!p_ptr->depth) return FALSE;		/* No artifacts in the town */

	/* Paranoia -- no "plural" artifacts */
	if (o_ptr->number != 1) return FALSE;

	/* Check the artifact list (skip the "specials") */
	for (i = ART_MIN_NORMAL; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &object_type::a_info[i];

		if (!a_ptr->_name) continue;	/* Skip "empty" items */
		if (a_ptr->cur_num) continue;	/* Cannot make an artifact twice */

		/* Must have the correct fields */
		if (a_ptr->obj_id != o_ptr->obj_id) continue;

		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > p_ptr->depth)
		{
			/* Get the "out-of-depth factor" */
			int d = (a_ptr->level - p_ptr->depth) * 2;

			/* Roll for out-of-depth creation */
			if (!one_in_(d)) continue;
		}

		/* We must make the "rarity roll" */
		if (!one_in_(a_ptr->rarity)) continue;

		o_ptr->name1 = i;	/* Mark the item as an artifact */

		/* Copy across critical data from the artifact struct */
		copy_artifact_data(*o_ptr, *a_ptr);

		return TRUE;	/* Success */
	}

	return FALSE;	/* Failure */
}


/*
 * Charge a new wand.
 */
static void charge_wand(object_type *o_ptr)
{
	switch (o_ptr->obj_id.sval)
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
	switch (o_ptr->obj_id.sval)
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
		case SV_STAFF_BANISHMENT:		o_ptr->pval = randint(2)  + 1; break;
		case SV_STAFF_EARTHQUAKES:		o_ptr->pval = randint(5)  + 3; break;
		case SV_STAFF_DESTRUCTION:		o_ptr->pval = randint(3)  + 1; break;
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
	switch (o_ptr->obj_id.tval)
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
		{
			/* Very Good */
			if (power > 1)
			{
				/* Hack -- Super-charge the damage dice, but no more than nine */
				while ((0 < o_ptr->d.maxroll()) && (9 > o_ptr->d.dice) &&
				       (one_in_(10L * o_ptr->d.maxroll())))
				{
					o_ptr->d.dice++;
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
				/* Hack -- super-charge the damage dice, but no more than nine */
				while ((0 < o_ptr->d.maxroll()) && (9 > o_ptr->d.dice)  && 
				       one_in_(10L * o_ptr->d.maxroll()))
				{
					o_ptr->d.dice++;
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
	if (TV_DRAG_ARMOR == o_ptr->obj_id.tval)
	{
		/* Rating boost */
		rating += 30;

		/* Mention the item */
		if (OPTION(cheat_peek)) object_mention(o_ptr);
	}
}



/*
 * Apply magic to an item known to be a "ring" or "amulet"
 *
 * Hack -- note special rating boost for ring of speed
 * Hack -- note special rating boost for certain amulets
 * Hack -- note special "pval boost" code for ring of speed
 * Hack -- note that some items must be cursed (or blessed)
 */
static void a_m_aux_3(object_type *o_ptr, int level, int power)
{
	/* Apply magic (good or bad) according to type */
	switch (o_ptr->obj_id.tval)
	{
		case TV_RING:
		{
			/* Analyze */
			switch (o_ptr->obj_id.sval)
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
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

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
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);

						break;
					}
					else
					{
						/* Rating boost */
						rating += 25;
					}

					/* Mention the item */
					if (OPTION(cheat_peek)) object_mention(o_ptr);

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
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

						/* Reverse pval */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Flames, Acid, Ice, Lightning */
				case SV_RING_FLAMES:
				case SV_RING_ACID:
				case SV_RING_ICE:
				case SV_RING_LIGHTNING:
				{
					/* Bonus to armor class */
					o_ptr->to_a = 5 + randint(5) + m_bonus(10, level);
					break;
				}

				/* Weakness, Stupidity */
				case SV_RING_WEAKNESS:
				case SV_RING_STUPIDITY:
				{
					/* Broken and Cursed */
					o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = 0 - (1 + m_bonus(5, level));

					break;
				}

				/* WOE, Stupidity */
				case SV_RING_WOE:
				{
					/* Broken and Cursed */
					o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

					/* Penalize */
					o_ptr->to_a = 0 - (5 + m_bonus(10, level));
					o_ptr->pval = 0 - (1 + m_bonus(5, level));

					break;
				}

				/* Ring of damage */
				case SV_RING_DAMAGE:
				{
					/* Bonus to damage */
					o_ptr->to_d = 5 + randint(3) + m_bonus(7, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

						/* Reverse bonus */
						o_ptr->to_d = 0 - (o_ptr->to_d);
					}

					break;
				}

				/* Ring of Accuracy */
				case SV_RING_ACCURACY:
				{
					/* Bonus to hit */
					o_ptr->to_h = 5 + randint(3) + m_bonus(7, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

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
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

						/* Reverse toac */
						o_ptr->to_a = 0 - (o_ptr->to_a);
					}

					break;
				}

				/* Ring of Slaying */
				case SV_RING_SLAYING:
				{
					/* Bonus to damage and to hit */
					o_ptr->to_d = randint(5) + m_bonus(5, level);
					o_ptr->to_h = randint(5) + m_bonus(5, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);


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
			switch (o_ptr->obj_id.sval)
			{
				/* Amulet of wisdom/charisma/infravision */
				case SV_AMULET_WISDOM:
				case SV_AMULET_CHARISMA:
				case SV_AMULET_INFRAVISION:
				{
					o_ptr->pval = 1 + m_bonus(5, level);

					/* Cursed */
					if (power < 0)
					{
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

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
						/* Broken and Cursed */
						o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

						/* Reverse bonuses */
						o_ptr->pval = 0 - (o_ptr->pval);
					}

					break;
				}

				/* Amulet of ESP -- never cursed */
				case SV_AMULET_ESP:
				{
					o_ptr->pval = randint(5) + m_bonus(5, level);

					break;
				}

				/* Amulet of the Magi -- never cursed */
				case SV_AMULET_THE_MAGI:
				{
					o_ptr->pval = 1 + m_bonus(3, level);
					o_ptr->to_a = randint(5) + m_bonus(5, level);

					/* Boost the rating */
					rating += 25;

					/* Mention the item */
					if (OPTION(cheat_peek)) object_mention(o_ptr);

					break;
				}

				/* Amulet of Devotion -- never cursed */
				case SV_AMULET_DEVOTION:
				{
					o_ptr->pval = 1 + m_bonus(3, level);

					/* Boost the rating */
					rating += 25;

					/* Mention the item */
					if (OPTION(cheat_peek)) object_mention(o_ptr);

					break;
				}

				/* Amulet of Weaponmastery -- never cursed */
				case SV_AMULET_WEAPONMASTERY:
				{
					o_ptr->to_h = 1 + m_bonus(4, level);
					o_ptr->to_d = 1 + m_bonus(4, level);
					o_ptr->pval = 1 + m_bonus(2, level);

					/* Boost the rating */
					rating += 25;

					/* Mention the item */
					if (OPTION(cheat_peek)) object_mention(o_ptr);

					break;
				}

				/* Amulet of Trickery -- never cursed */
				case SV_AMULET_TRICKERY:
				{
					o_ptr->pval = randint(1) + m_bonus(3, level);

					/* Boost the rating */
					rating += 25;

					/* Mention the item */
					if (OPTION(cheat_peek)) object_mention(o_ptr);

					break;
				}

				/* Amulet of Doom -- always cursed */
				case SV_AMULET_DOOM:
				{
					/* Broken and Cursed */
					o_ptr->ident |= (IDENT_BROKEN | IDENT_CURSED);

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
	/* Unused parameters */
	(void)level;
	(void)power;

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->obj_id.tval)
	{
		case TV_LITE:
		{
			/* Hack -- Torches -- random fuel */
			if (o_ptr->obj_id.sval == SV_LITE_TORCH)
			{
				if (o_ptr->pval > 0) o_ptr->pval = randint(o_ptr->pval);
			}

			/* Hack -- Lanterns -- random fuel */
			else if (o_ptr->obj_id.sval == SV_LITE_LANTERN)
			{
				if (o_ptr->pval > 0) o_ptr->pval = randint(o_ptr->pval);
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

		case TV_ROD:
		{
			object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];

			/* Transfer the pval. */
			o_ptr->pval = k_ptr->pval;
			break;
		}

		case TV_CHEST:
		{
			/* Hack -- skip ruined chests */
			if (o_ptr->level() <= 0) break;

			/* Hack -- pick a "difficulty" */
			o_ptr->pval = randint(o_ptr->level());

			/* Never exceed "difficulty" of 55 to 59 */
			if (o_ptr->pval > 55) o_ptr->pval = (s16b)(55 + rand_int(5));

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
void apply_magic(object_type *o_ptr, int lev, bool allow_artifacts, bool good, bool great)
{
	int i;

	int power = 0;	/* Assume normal */
	int rolls = 0;	/* Assume no rolls */

	/* chance of being good or great */
	const int good_chance = MIN(75, lev+10);
	const int great_chance = MIN(good_chance/2, 20);

	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

	/* Roll for "good" */
	if (good || (rand_int(100) < good_chance))
	{
		/* Assume "good" */
		power = 1;

		/* Roll for "great" */
		if (great || (rand_int(100) < great_chance)) power = 2;
	}

	/* Roll for "cursed" */
	else if (rand_int(100) < good_chance)
	{
		/* Assume "cursed" */
		power = -1;

		/* Roll for "broken" */
		if (rand_int(100) < great_chance) power = -2;
	}

	/* Are we allowed to roll to be an artifact? */
	if (!allow_artifacts || o_ptr->name1)
	{
		if (power >= 2) rolls = 1;	/* Get one roll if excellent */
		if (great) rolls = 4;		/* Get four rolls if forced great */
	}

	/* Roll for artifacts if allowed */
	for (i = 0; i < rolls; ++i)
	{
		/* Roll for an artifact */
		if (make_artifact(o_ptr)) break;
	}


	/* Hack -- analyze artifacts */
	if (o_ptr->name1)
	{
		/* Copy across critical data from the artifact struct */
		copy_artifact_data(*o_ptr, object_type::a_info[o_ptr->name1]); 

		/* Done */
		return;
	}


	/* Apply magic */
	switch (o_ptr->obj_id.tval)
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
			if ((power > 1) || (power < -1))
			{
				int ego_power = make_ego_item(o_ptr, (good || great));
				if (ego_power) power = ego_power;
			}

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
			if ((power > 1) || (power < -1))
			{
				int ego_power = make_ego_item(o_ptr, (good || great));
				if (ego_power) power = ego_power;
			}

			if (power) a_m_aux_2(o_ptr, lev, power);
			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			if (!power && one_in_(2)) power = -1;
			a_m_aux_3(o_ptr, lev, power);
			break;
		}

		case TV_LITE:
		{
			if ((power > 1) || (power < -1))
			{
				make_ego_item(o_ptr, (good || great));
			}

			/* Fuel it */
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
		ego_item_type *e_ptr = &object_type::e_info[o_ptr->name2];

		/* Extra powers */
		if (e_ptr->xtra)
		{
			o_ptr->xtra1 = e_ptr->xtra;
			switch (o_ptr->xtra1)
			{
				case OBJECT_XTRA_TYPE_SUSTAIN:
				{
					o_ptr->xtra2 = (byte)rand_int(OBJECT_XTRA_SIZE_SUSTAIN);
					break;
				}

				case OBJECT_XTRA_TYPE_RESIST:
				{
					o_ptr->xtra2 = (byte)rand_int(OBJECT_XTRA_SIZE_RESIST);
					break;
				}

				case OBJECT_XTRA_TYPE_POWER:
				{
					o_ptr->xtra2 = (byte)rand_int(OBJECT_XTRA_SIZE_POWER);
					break;
				}
			}
		}

		/* Hack -- acquire "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (e_ptr->flags[2] & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

		/* Hack -- apply extra penalties if needed */
		if (o_ptr->is_broken_or_cursed())
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
		if (OPTION(cheat_peek)) object_mention(o_ptr);

		/* Done */
		return;
	}


	/* Examine real objects */
	if (o_ptr->k_idx)
	{
		object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];

		/* Hack -- acquire "broken" flag */
		if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->flags[2] & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
	}
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
	object_kind *k_ptr = &object_type::k_info[k_idx];

	/* Analyze the item type */
	switch (k_ptr->obj_id.tval)
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
		case TV_CROWN:	return 0 <= k_ptr->to_a;

		/* Weapons -- Good unless damaged */
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			if (k_ptr->to_h < 0) return FALSE;
			if (k_ptr->to_d < 0) return FALSE;
			return TRUE;
		}

		/* Ammo -- Arrows/Bolts are good */
		case TV_BOLT:
		case TV_ARROW:	return TRUE;

		/* Books -- High level books are good */
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:	return SV_BOOK_MIN_GOOD <= k_ptr->obj_id.sval;

		/* Rings -- Rings of Speed are good */
		case TV_RING:	return SV_RING_SPEED == k_ptr->obj_id.sval;

		/* Amulets -- Amulets of the Magi are good */
		case TV_AMULET:
		{
			if (k_ptr->obj_id.sval == SV_AMULET_THE_MAGI) return (TRUE);
			if (k_ptr->obj_id.sval == SV_AMULET_DEVOTION) return (TRUE);
			if (k_ptr->obj_id.sval == SV_AMULET_WEAPONMASTERY) return (TRUE);
			if (k_ptr->obj_id.sval == SV_AMULET_TRICKERY) return (TRUE);
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
bool make_object(object_type *j_ptr, bool good, bool great)
{
	/* Chance of "special object" */
	int prob = (good ? 10 : 1000);

	/* Base level for the object */
	int base = (good ? (object_level + 10) : object_level);


	/* Generate a special artifact, or a normal object */
	if (!one_in_(prob) || !make_artifact_special(j_ptr))
	{
		int k_idx;

		/* Good objects: Prepare allocation table */
		if (good) get_obj_num_prep(&kind_is_good);

		/* Pick a random object */
		k_idx = get_obj_num(base);

		/* Good objects: Clear restriction */
		if (good) get_obj_num_prep(NULL);

		/* Handle failure */
		if (!k_idx) return FALSE;

		/* Prepare the object */
		object_prep(j_ptr, k_idx);
	}

	/* Apply magic (allow artifacts) */
	apply_magic(j_ptr, object_level, TRUE, good, great);

	/* Hack -- generate multiple spikes/missiles */
	switch (j_ptr->obj_id.tval)
	{
		case TV_SPIKE:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:	j_ptr->number = NdS(6, 7);
	}

	/* Notice "okay" out-of-depth objects */
	if (!j_ptr->is_broken_or_cursed() &&
	    (j_ptr->level() > p_ptr->depth))
	{
		/* Rating increase */
		rating += (j_ptr->level() - p_ptr->depth);

		/* Cheat -- peek at items */
		if (OPTION(cheat_peek)) object_mention(j_ptr);
	}

	/* Success */
	return TRUE;
}



/*
 * Make a treasure object
 *
 * The location must be a legal, clean, floor grid.
 */
void make_gold(object_type *j_ptr, SV_cash coin_type)
{
	int sval;
	int k_idx;
	s32b base;

	if (coin_type)
	{	/* Creeping Coins only generate "themselves" */
		sval = coin_type;
	}

	else
	{	/* Pick a Treasure variety */
		sval = 1 + (randint(object_level + 2) / 2);

		/* Apply "extra" magic */
		if (one_in_(GREAT_OBJ)) sval += randint(object_level + 1);
	}

	/* Do not create "illegal" Treasure Types */
	if (sval > MAX_GOLD) sval = MAX_GOLD;

	k_idx = lookup_kind2(TV_GOLD, sval);
	
	/* Prepare a gold object */
	object_prep(j_ptr, k_idx);

	/* Base coin cost */
	base = object_type::k_info[k_idx].cost;

	/* Determine how much the treasure is "worth" */
	j_ptr->pval = (base + (8L * randint(base)) + randint(8));
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
		object_type *o_ptr = &o_list[this_o_idx];	/* Get the object */

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
	if (n >= MAX_FLOOR_STACK) return (0);

	/* Option -- disallow stacking */
	if (OPTION(adult_no_stacking) && n) return (0);

	/* Make an object */
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr = &o_list[o_idx];	/* Get the object */

		*o_ptr = *j_ptr;

		/* Location */
		o_ptr->loc = coord(x,y);

		/* Forget monster */
		o_ptr->held_m_idx = 0;

		/* Link the object to the pile */
		o_ptr->next_o_idx = cave_o_idx[y][x];

		/* Link the floor to the object */
		cave_o_idx[y][x] = o_idx;

		/* Notice */
		note_spot(o_ptr->loc);

		/* Redraw */
		lite_spot(o_ptr->loc);
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
void drop_near(object_type *j_ptr, int chance, coord t)
{
	int i, k, s;

	int bs, bn;
	coord b,tt;
	coord_scan d;

	object_type *o_ptr;

	char o_name[80];

	bool flag = FALSE;

	bool plural = FALSE;


	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Describe object */
	object_desc(o_name, sizeof(o_name), j_ptr, FALSE, ODESC_BASE);


	/* Handle normal "breakage" */
	if (!j_ptr->is_artifact() && (rand_int(100) < chance))
	{
		/* Message */
		msg_format("The %s break%s.", o_name, PLURAL(plural));

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
	b = t;

	/* Scan local grids */
	for (d.y = -3; d.y <= 3; d.y++)
	{
		/* Scan local grids */
		for (d.x = -3; d.x <= 3; d.x++)
		{
			bool comb = FALSE;

			/* Calculate actual distance */
			int dist = (d.y * d.y) + (d.x * d.x);

			/* Ignore distant grids */
			if (dist > 10) continue;

			/* Location */
			tt = t;
			tt += d;

			/* Skip illegal grids */
			if (!in_bounds_fully(tt.y, tt.x)) continue;

			/* Require line of sight */
			if (!los(t, tt)) continue;

			/* Require floor space */
			if (cave_feat[tt.y][tt.x] != FEAT_FLOOR) continue;

			/* No objects */
			k = 0;

			/* Scan objects in that grid */
			for (o_ptr = get_first_object(tt.y, tt.x); o_ptr; o_ptr = get_next_object(o_ptr))
			{
				/* Check for possible combination */
				if (object_similar(o_ptr, j_ptr)) comb = TRUE;

				/* Count objects */
				k++;
			}

			/* Add new object */
			if (!comb) k++;

			/* Option -- disallow stacking */
			if (OPTION(adult_no_stacking) && (k > 1)) continue;
			
			/* Paranoia */
			if (k > MAX_FLOOR_STACK) continue;

			/* Calculate score */
			s = 1000 - (dist + k * 5);

			/* Skip bad values */
			if (s < bs) continue;

			/* New best value */
			if (s > bs) bn = 0;

			/* Apply the randomizer to equivalent values */
			if ((++bn >= 2) && !one_in_(bn)) continue;

			/* Keep score */
			bs = s;

			/* Track it */
			b = tt;

			/* Okay */
			flag = TRUE;
		}
	}


	/* Handle lack of space */
	if (!flag && !j_ptr->is_artifact())
	{
		/* Message */
		msg_format("The %s disappear%s.", o_name, PLURAL(plural));

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
			tt.y = rand_spread(b.y, 1);
			tt.x = rand_spread(b.x, 1);
		}

		/* Random locations */
		else
		{
			tt.y = rand_int(DUNGEON_HGT);
			tt.x = rand_int(DUNGEON_WID);
		}

		/* Require floor space */
		if (cave_feat[tt.y][tt.x] != FEAT_FLOOR) continue;

		/* Bounce to that location */
		b = tt;

		/* Require floor space */
		if (!cave_clean_bold(b.y, b.x)) continue;

		/* Okay */
		flag = TRUE;
	}


	/* Give it to the floor */
	if (!floor_carry(b.y, b.x, j_ptr))
	{
		/* Message */
		msg_format("The %s disappear%s.", o_name, PLURAL(plural));

		/* Debug */
		if (p_ptr->wizard) msg_print("Breakage (too many objects).");

		/* Hack -- Preserve artifacts */
		object_type::a_info[j_ptr->name1].cur_num = 0;

		/* Failure */
		return;
	}


	/* Sound */
	sound(MSG_DROP);

	/* Mega-Hack -- no message if "dropped" by player */
	/* Message when an object falls under the player */
	if (chance && (cave_m_idx[b.y][b.x] < 0))
	{
		msg_print("You feel something roll beneath your feet.");
	}
}


/*
 * Scatter some "great" objects near the player
 */
void acquirement(coord t, int num, bool great)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	/* Acquirement */
	while (num--)
	{
		/* Wipe the object */
		WIPE(i_ptr);

		/* Make a good (or great) object (if possible) */
		if (!make_object(i_ptr, TRUE, great)) continue;

		/* Drop the object */
		drop_near(i_ptr, -1, t);
	}
}


/*
 * Attempt to place an object (normal or good/great) at the given location.
 */
void place_object(int y, int x, bool good, bool great)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Hack -- clean floor space */
	if (!cave_clean_bold(y, x)) return;

	/* Wipe the object */
	WIPE(i_ptr);

	/* Make an object (if possible) */
	if (make_object(i_ptr, good, great))
	{
		/* Give it to the floor */
		if (!floor_carry(y, x, i_ptr))
		{
			/* Hack -- Preserve artifacts */
			object_type::a_info[i_ptr->name1].cur_num = 0;
		}
	}
}


/*
 * Places a treasure (Gold or Gems) at given location
 */
void place_gold(int y, int x)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require clean floor space */
	if (!cave_clean_bold(y, x)) return;

	/* Wipe the object */
	WIPE(i_ptr);

	/* Make some gold */
	make_gold(i_ptr, SV_CASH);

	/* Give it to the floor */
	floor_carry(y, x, i_ptr);
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
 * Place a random type of closed door at the given location.
 */
void place_closed_door(int y, int x)
{
	int tmp = rand_int(400);	/* Choose an object */

	/* Closed doors (300/400) */
	if (tmp < 300)
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
	}

	/* Locked doors (99/400) */
	else if (tmp < 399)
	{
		/* Create locked door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + randint(7));
	}

	/* Stuck doors (1/400) */
	else
	{
		/* Create jammed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x08 + rand_int(8));
	}
}


/*
 * Place a random type of door at the given location.
 */
void place_random_door(int y, int x)
{
	int tmp = rand_int(1000);	/* Choose an object */

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
	assert((0 <= item) && (item < INVEN_TOTAL) && "precondition");
	const object_type* const o_ptr = &p_ptr->inventory[item];

	/* Require staff/wand */
	if ((o_ptr->obj_id.tval != TV_STAFF) && (o_ptr->obj_id.tval != TV_WAND)) return;

	/* Require known item */
	if (!o_ptr->known()) return;

	/* Print a message */
	msg_format("You have %d charge%s remaining.", o_ptr->pval,
	           (o_ptr->pval != 1) ? "s" : "");
}


/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
	assert((0 <= item) && (item < INVEN_TOTAL) && "precondition");
	const object_type* const o_ptr = &p_ptr->inventory[item];

	char o_name[80];

	if (o_ptr->is_artifact() && o_ptr->known())
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, ODESC_FULL);

		/* Print a message */
		msg_format("You no longer have the %s (%c).", o_name, index_to_label(item));
	}
	else
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Print a message */
		msg_format("You have %s (%c).", o_name, index_to_label(item));
	}
}


/*
 * Increase the "number" of an item in the inventory
 */
void inven_item_increase(int item, int num)
{
	assert((0 <= item) && (item < INVEN_TOTAL) && "precondition");
	object_type* const o_ptr = &p_ptr->inventory[item];

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
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
	}
}

#ifndef NDEBUG
bool player_type::inven_cnt_is_strict_UB_of_nonzero_k_idx() const
{
	char buf[10];
	int i;
	for (i = 0; i < INVEN_PACK; ++i)
	{
		if (p_ptr->inven_cnt<=i)
		{
			if (0!=inventory[i].k_idx) {plog("non-zero"); plog(ltoa(i,buf,10)); return FALSE;};
		}
		else
		{
			if (0==inventory[i].k_idx) {plog("zero"); plog(ltoa(i,buf,10)); return FALSE;};
		}
	}
	return TRUE;
}
#endif

/*
 * Erase an inventory slot if it has no more items
 */
void inven_item_optimize(int item)
{
	assert(0 <= item && INVEN_TOTAL > item && "precondition");	/* range-check */

	object_type *o_ptr = &p_ptr->inventory[item];

	/* Only optimize real items */
	if (!o_ptr->k_idx) return;

	/* Only optimize empty items */
	if (o_ptr->number) return;

	/* The item is in the pack */
	if (item <= INVEN_PACK)
	{
		assert(0 < p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
		assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");
		assert(item < p_ptr->inven_cnt && "precondition");

		/* One less item */
		p_ptr->inven_cnt--;

		/* Slide everything down */
		if (item < INVEN_PACK) C_COPY(p_ptr->inventory + item, p_ptr->inventory + item + 1, INVEN_PACK - item);

		/* Hack -- wipe hole */
		WIPE(&p_ptr->inventory[INVEN_PACK]);

		assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "postcondition");

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN);
	}

	/* The item is being wielded */
	else
	{
		/* One less item */
		p_ptr->equip_cnt--;

		/* Erase the empty slot */
		WIPE(&p_ptr->inventory[item]);

		/* Recalculate bonuses, torch,  mana */
		p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);

		/* Window stuff */
		p_ptr->redraw |= (PR_EQUIP);
	}
}


/*
 * Describe the charges on an item on the floor.
 */
void floor_item_charges(int item)
{
	object_type *o_ptr = &o_list[item];

	/* Require staff/wand */
	if ((o_ptr->obj_id.tval != TV_STAFF) && (o_ptr->obj_id.tval != TV_WAND)) return;

	/* Require known item */
	if (!o_ptr->known()) return;

	/* Print a message */
	msg_format("There are %d charge%s remaining.", o_ptr->pval,
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
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

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
bool inven_carry_okay(const object_type *o_ptr)
{
	int j;

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Empty slot? */
	if (INVEN_PACK > p_ptr->inven_cnt) return TRUE;

	/* Similar slot? */
	for (j = 0; j < p_ptr->inven_cnt; j++)
	{
		/* Check if the two items can be combined */
		if (object_similar(&p_ptr->inventory[j], o_ptr)) return TRUE;
	}

	/* Nope */
	return FALSE;
}


/**
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
 *
 * \post returned slot is valid to dereference p_ptr->inventory with
 */
s16b inven_carry(object_type *o_ptr)
{
	int i, j;
	int n = -1;

	object_type *j_ptr;

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Check for combining */
	n = p_ptr->inven_cnt-1;
	j = 0;
	while(j < p_ptr->inven_cnt)
	{
		j_ptr = &p_ptr->inventory[j];

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
			p_ptr->redraw |= (PR_INVEN);

			/* Success */
			return (j);
		}
		++j;
	};

	/* p_ptr->inven_cnt points to first empty slot */
	i = p_ptr->inven_cnt;

	/* Reorder the pack */
	if (i < INVEN_PACK)
	{
		s32b o_value = object_value(o_ptr);	/* Get the "value" of the item */
		s32b j_value;

		/* Scan every occupied slot */
		j = -1;
		while(++j < p_ptr->inven_cnt)
		{
			j_ptr = &p_ptr->inventory[j];

			/* Hack -- readable books always come first */
			if (o_ptr->obj_id.tval == p_ptr->spell_book())
				{
				if (j_ptr->obj_id.tval != p_ptr->spell_book()) break;
				}
			else{
				if (j_ptr->obj_id.tval == p_ptr->spell_book()) continue;
				}

			/* Objects sort by decreasing type */
			if (o_ptr->obj_id.tval > j_ptr->obj_id.tval) break;
			if (o_ptr->obj_id.tval < j_ptr->obj_id.tval) continue;

			/* Non-aware (flavored) items always come last */
			if (!o_ptr->aware()) continue;
			if (!j_ptr->aware()) break;

			/* Objects sort by increasing sval */
			if (o_ptr->obj_id.sval < j_ptr->obj_id.sval) break;
			if (o_ptr->obj_id.sval > j_ptr->obj_id.sval) continue;

			/* Unidentified objects always come last */
			if (!o_ptr->known()) continue;
			if (!j_ptr->known()) break;

			/* Lites sort by decreasing fuel */
			if (o_ptr->obj_id.tval == TV_LITE)
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
		if (n >= i) C_COPY(p_ptr->inventory + i + 1, p_ptr->inventory + i, n - i + 1);
	}

	/* Copy the item */
	p_ptr->inventory[i] = *o_ptr;

	/* Count the items */
	++p_ptr->inven_cnt;

	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "postcondition");

	/* Get the new object */
	j_ptr = &p_ptr->inventory[i];

	j_ptr->next_o_idx = 0;	/* Forget stack */
	j_ptr->held_m_idx = 0;	/* Forget monster */
	j_ptr->loc.clear();		/* Forget location */
	j_ptr->marked = FALSE;	/* No longer marked */

	/* Increase the weight */
	p_ptr->total_weight += (j_ptr->number * j_ptr->weight);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine and Reorder pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->redraw |= (PR_INVEN);

	/* Return the slot */
	return (i);
}


/**
 * Take off (some of) a non-cursed equipment item
 *
 * Note that only one item at a time can be wielded per slot.
 *
 * Note that taking off an item when "full" may cause that item
 * to fall to the ground.
 *
 * \return the inventory slot into which the item is placed.
 *
 * \post returned slot is valid to dereference p_ptr->inventory with
 */
s16b inven_takeoff(int item, int amt)
{
	int slot;

	object_type object_type_body;
	object_type *o_ptr = &p_ptr->inventory[item];	/* Get the item to take off */
	object_type *i_ptr = &object_type_body;			/* Get local object */

	const char* act;

	char o_name[80];

	/* Paranoia */
	if (amt <= 0) return (-1);

	/* Verify */
	if (amt > o_ptr->number) amt = o_ptr->number;

	/* Obtain a local object */
	*i_ptr = *o_ptr;

	/* Modify quantity */
	i_ptr->number = amt;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, ODESC_FULL);

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
	slot = inven_carry(i_ptr);

	/* Message */
	sound(MSG_WIELD);
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
	assert((0 <= item) && (item < INVEN_TOTAL) && "precondition");

	object_type object_type_body;
	object_type *i_ptr = &object_type_body;			/* Get local object */
	object_type *o_ptr = &p_ptr->inventory[item];	/* Get the original object */

	char o_name[80];

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
		o_ptr = &p_ptr->inventory[item];
	}


	/* Obtain local object */
	*i_ptr = *o_ptr;

	/* Distribute charges of wands, staves, or rods */
	distribute_charges(o_ptr, i_ptr, amt);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Describe local object */
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, ODESC_FULL);

	/* Message */
	msg_format("You drop %s (%c).", o_name, index_to_label(item));

	/* Drop it near the player */
	drop_near(i_ptr, 0, p_ptr->loc);

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
	int i, j;

	bool flag = FALSE;

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK+1 >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Combine the pack (backwards) */
	for (i = p_ptr->inven_cnt-1; i > 0; i--)
	{
		/* Get the item */
		object_type* const o_ptr = &p_ptr->inventory[i];

		/* Scan the items above that item */
		for (j = 0; j < i; j++)
		{
			/* Get the item */
			object_type* const j_ptr = &p_ptr->inventory[j];

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
				if (i < INVEN_PACK) C_COPY(p_ptr->inventory + i, p_ptr->inventory + i + 1, INVEN_PACK - i);

				/* Hack -- wipe hole */
				WIPE(&p_ptr->inventory[INVEN_PACK]);

				assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "postcondition");

				/* Window stuff */
				p_ptr->redraw |= (PR_INVEN);

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
	int i, j;

	s32b j_value;

	object_type object_type_body;
	object_type *i_ptr = &object_type_body;	/* Get local object */

	bool flag = FALSE;

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK+1 >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Re-order the pack (forwards) */
	for (i = 1; i < p_ptr->inven_cnt; ++i)
	{
		object_type* const o_ptr = &p_ptr->inventory[i];	/* Get the item */
		const s32b o_value = object_value(o_ptr);					/* Get the "value" of the item */

		/* Scan every occupied slot */
		for (j = 0; j < i; ++j)
		{
			object_type* const j_ptr = &p_ptr->inventory[j];	/* Get the item already there */

			/* Hack -- readable books always come first */
			if (o_ptr->obj_id.tval == p_ptr->spell_book())
				{
				if (j_ptr->obj_id.tval != p_ptr->spell_book()) break;
				}
			else{
				if (j_ptr->obj_id.tval == p_ptr->spell_book()) continue;
				}

			/* Objects sort by decreasing type */
			if (o_ptr->obj_id.tval > j_ptr->obj_id.tval) break;
			if (o_ptr->obj_id.tval < j_ptr->obj_id.tval) continue;

			/* Non-aware (flavored) items always come last */
			if (!o_ptr->aware()) continue;
			if (!j_ptr->aware()) break;

			/* Objects sort by increasing sval */
			if (o_ptr->obj_id.sval < j_ptr->obj_id.sval) break;
			if (o_ptr->obj_id.sval > j_ptr->obj_id.sval) continue;

			/* Unidentified objects always come last */
			if (!o_ptr->known()) continue;
			if (!j_ptr->known()) break;

			/* Lites sort by decreasing fuel */
			if (o_ptr->obj_id.tval == TV_LITE)
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

		/* Save a copy of the moving item */
		*i_ptr = *o_ptr;

		/* Slide the objects */
		C_COPY(p_ptr->inventory+j+1, p_ptr->inventory+j, i - j);

		/* Insert the moving item */
		p_ptr->inventory[j] = *i_ptr;

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN);
	}

	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "postcondition");

	/* Message */
	if (flag) msg_print("You reorder some items in your pack.");
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
	if ((o_ptr->obj_id.tval == TV_WAND) ||
	    (o_ptr->obj_id.tval == TV_STAFF) ||
	    (o_ptr->obj_id.tval == TV_ROD))
	{
		q_ptr->pval = o_ptr->pval * amt / o_ptr->number;

		if (amt < o_ptr->number) o_ptr->pval -= q_ptr->pval;

		/*
		 * Hack -- Rods also need to have their timeouts distributed.
		 *
		 * The dropped stack will accept all time remaining to charge up to
		 * its maximum.
		 */
		if ((o_ptr->obj_id.tval == TV_ROD) && (o_ptr->timeout))
		{
			if (q_ptr->pval > o_ptr->timeout)
				q_ptr->timeout = o_ptr->timeout;
			else
				q_ptr->timeout = q_ptr->pval;

			if (amt < o_ptr->number)
				o_ptr->timeout -= q_ptr->timeout;
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
	if (((o_ptr->obj_id.tval == TV_WAND) ||
	     (o_ptr->obj_id.tval == TV_STAFF) ||
	     (o_ptr->obj_id.tval == TV_ROD)) &&
	    (amt < o_ptr->number))
	{
		o_ptr->pval -= o_ptr->pval * amt / o_ptr->number;
	}
}

static bool is_moronic_to_eat(const object_type& o)
{
	switch(o.obj_id.sval)
	{
	case SV_FOOD_DISEASE:	/* fallthrough intentional */
	case SV_FOOD_UNHEALTH:
	case SV_FOOD_NAIVETY:
	case SV_FOOD_STUPIDITY:
	case SV_FOOD_SICKNESS:
	case SV_FOOD_WEAKNESS: return false;
	case SV_FOOD_BISCUIT:	/* fallthrough intentional */
	case SV_FOOD_JERKY:
	case SV_FOOD_SLIME_MOLD:
	case SV_FOOD_PINT_OF_ALE:
	case SV_FOOD_PINT_OF_WINE: return (PY_FOOD_MAX<=p_ptr->food);
	case SV_FOOD_RATION: return (PY_FOOD_FULL<=p_ptr->food);
	case SV_FOOD_WAYBREAD: return (PY_FOOD_FULL<=p_ptr->food && p_ptr->chp==p_ptr->mhp && 0==p_ptr->timed[TMD_POISONED]);
	case SV_FOOD_RESTORING: return (PY_FOOD_WEAK<=p_ptr->food && p_ptr->stat_cur[A_STR]==p_ptr->stat_max[A_STR] && p_ptr->stat_cur[A_INT]==p_ptr->stat_max[A_INT] && p_ptr->stat_cur[A_WIS]==p_ptr->stat_max[A_WIS] && p_ptr->stat_cur[A_DEX]==p_ptr->stat_max[A_DEX] && p_ptr->stat_cur[A_CON]==p_ptr->stat_max[A_CON] && p_ptr->stat_cur[A_CHR]==p_ptr->stat_max[A_CHR] && !(p_ptr->sustain[A_STR] && p_ptr->sustain[A_INT] && p_ptr->sustain[A_WIS] && p_ptr->sustain[A_DEX] && p_ptr->sustain[A_CON] && p_ptr->sustain[A_CHR]));
	case SV_FOOD_RESTORE_CON: return (PY_FOOD_WEAK<=p_ptr->food && p_ptr->stat_cur[A_CON]==p_ptr->stat_max[A_CON] && !p_ptr->sustain[A_CON]);
	case SV_FOOD_RESTORE_STR: return (PY_FOOD_WEAK<=p_ptr->food && p_ptr->stat_cur[A_STR]==p_ptr->stat_max[A_STR] && !p_ptr->sustain[A_STR]);
	case SV_FOOD_CURE_SERIOUS: return (PY_FOOD_WEAK<=p_ptr->food && p_ptr->chp==p_ptr->mhp);
	case SV_FOOD_CURE_CONFUSION: return (PY_FOOD_WEAK<=p_ptr->food && 0==p_ptr->timed[TMD_CONFUSED] && !p_ptr->resist_confu);
	case SV_FOOD_PARALYSIS: return !p_ptr->free_act;
	case SV_FOOD_HALLUCINATION: return !p_ptr->resist_chaos;
	case SV_FOOD_CONFUSION: return !p_ptr->resist_confu;
	case SV_FOOD_PARANOIA: return !p_ptr->resist_fear;
	case SV_FOOD_POISON: return (!p_ptr->resist_pois || 0==p_ptr->timed[TMD_OPP_POIS]);
	case SV_FOOD_BLINDNESS: return (!p_ptr->resist_blind);
	default: return false;
	};
}
/*
	rest of old LUA implementation; these are more questionable

	elseif object.sval == SV_FOOD_CURE_PARANOIA then
		if player.food >= PY_FOOD_WEAK and 0==player.afraid and not player.resist_fear then is_moronic = true end
	elseif object.sval == SV_FOOD_CURE_BLINDNESS then
		if player.food >= PY_FOOD_WEAK and 0==player.blind and not player.resist_blind then is_moronic = true end
	elseif object.sval == SV_FOOD_CURE_POISON then
		if player.food >= PY_FOOD_WEAK and 0==player.poisoned and not (player.resist_pois or (player.oppose_pois > 0)) then is_moronic = true end
*/

static bool is_moronic_to_quaff(const object_type& o)
{
	switch(o.obj_id.sval)
	{
	case SV_POTION_SLOWNESS:	/* fallthrough intentional */
	case SV_POTION_DETONATIONS:
	case SV_POTION_RUINATION:
	case SV_POTION_DEATH:	return true;
	case SV_POTION_SALT_WATER:	/* fallthrough intentional */
	case SV_POTION_SLOW_POISON:
	case SV_POTION_CURE_POISON:	return 0==p_ptr->timed[TMD_POISONED];
	case SV_POTION_POISON: return !(p_ptr->resist_pois || 0<p_ptr->timed[TMD_OPP_POIS]);
	case SV_POTION_BLINDNESS: return !p_ptr->resist_blind;
	case SV_POTION_CONFUSION: return !p_ptr->resist_confu;
	case SV_POTION_SLEEP: return !p_ptr->free_act;
	case SV_POTION_LOSE_MEMORIES: return (!p_ptr->hold_life && 0<p_ptr->exp);
	case SV_POTION_DEC_STR: return !p_ptr->sustain[A_STR];
	case SV_POTION_DEC_INT: return !p_ptr->sustain[A_INT];
	case SV_POTION_DEC_WIS: return !p_ptr->sustain[A_WIS];
	case SV_POTION_DEC_DEX: return !p_ptr->sustain[A_DEX];
	case SV_POTION_DEC_CON: return !p_ptr->sustain[A_CON];
	case SV_POTION_DEC_CHR: return !p_ptr->sustain[A_CHR];
	case SV_POTION_BOLDNESS: return 0==p_ptr->timed[TMD_AFRAID];
	case SV_POTION_CURE_LIGHT: return (p_ptr->chp>=p_ptr->mhp && 0==p_ptr->timed[TMD_BLIND] && 0==p_ptr->timed[TMD_CUT]);
	case SV_POTION_CURE_SERIOUS: return (p_ptr->chp>=p_ptr->mhp && 0==p_ptr->timed[TMD_BLIND] && 0==p_ptr->timed[TMD_CONFUSED] && 0==p_ptr->timed[TMD_CUT]);
	case SV_POTION_CURE_CRITICAL:	/* fallthrough intentional */
	case SV_POTION_HEALING:
	case SV_POTION_STAR_HEALING: return (p_ptr->chp>=p_ptr->mhp && 0==p_ptr->timed[TMD_BLIND] && 0==p_ptr->timed[TMD_CONFUSED] && 0==p_ptr->timed[TMD_POISONED] && 0==p_ptr->timed[TMD_STUN] && 0==p_ptr->timed[TMD_CUT]);
	case SV_POTION_RESTORE_MANA: return (p_ptr->csp>=p_ptr->msp);
	case SV_POTION_RESTORE_EXP: return (p_ptr->exp>=p_ptr->max_exp);
	case SV_POTION_RES_STR: return (p_ptr->stat_cur[A_STR]==p_ptr->stat_max[A_STR]);
	case SV_POTION_RES_INT: return (p_ptr->stat_cur[A_INT]==p_ptr->stat_max[A_INT]);
	case SV_POTION_RES_WIS: return (p_ptr->stat_cur[A_WIS]==p_ptr->stat_max[A_WIS]);
	case SV_POTION_RES_DEX: return (p_ptr->stat_cur[A_DEX]==p_ptr->stat_max[A_DEX]);
	case SV_POTION_RES_CON: return (p_ptr->stat_cur[A_CON]==p_ptr->stat_max[A_CON]);
	case SV_POTION_RES_CHR: return (p_ptr->stat_cur[A_CHR]==p_ptr->stat_max[A_CHR]);
	case SV_POTION_INC_STR: return (18+100==p_ptr->stat_cur[A_STR]);
	case SV_POTION_INC_INT: return (18+100==p_ptr->stat_cur[A_INT]);
	case SV_POTION_INC_WIS: return (18+100==p_ptr->stat_cur[A_WIS]);
	case SV_POTION_INC_DEX: return (18+100==p_ptr->stat_cur[A_DEX]);
	case SV_POTION_INC_CON: return (18+100==p_ptr->stat_cur[A_CON]);
	case SV_POTION_INC_CHR: return (18+100==p_ptr->stat_cur[A_CHR]);
	case SV_POTION_AUGMENTATION: return (18+100==p_ptr->stat_cur[A_STR] && 18+100==p_ptr->stat_cur[A_INT] && 18+100==p_ptr->stat_cur[A_WIS] && 18+100==p_ptr->stat_cur[A_DEX] && 18+100==p_ptr->stat_cur[A_CON] && 18+100==p_ptr->stat_cur[A_CHR]);
	case SV_POTION_EXPERIENCE: return (PY_MAX_EXP<=p_ptr->exp);
	default: return false;
	};
}

static bool is_moronic_to_read(const object_type& o)
{
	switch(o.obj_id.sval)
	{
	case SV_SCROLL_DARKNESS:	/* fallthrough intentional */
	case SV_SCROLL_CURSE_ARMOR:
	case SV_SCROLL_CURSE_WEAPON:	return true;
	default: return false;
	}
}

static bool is_moronic_to_invoke(const object_type& o)
{
	switch(o.obj_id.sval)
	{
	case SV_STAFF_DARKNESS:	/* fallthrough intentional */
	case SV_STAFF_SLOWNESS:	return true;
	default: return false;
	};
}

/* this function must be applied only to identified items */
bool is_moronic_to_use(const object_type& o)
{
	/* wands are always useful in *some* circumstance; cf Moria suggestions for 
	   heal/haste monster

       no bad rods (yet; Heng has Rod of Aggravate Monster; however, there are circumstances 
	   where waking up all of the monsters is better than taking one head-on...) */
	switch(o.obj_id.tval)
	{
	case TV_FOOD: return is_moronic_to_eat(o);
	case TV_POTION: return is_moronic_to_quaff(o);
	case TV_SCROLL: return is_moronic_to_read(o);
	case TV_STAFF: return is_moronic_to_invoke(o);
	default: return false;
	};
}

