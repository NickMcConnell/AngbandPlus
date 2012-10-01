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
 * adds n flags to o_ptr chosen randomly from the masks f1..f4
 */
static void enhance_random(object_type *o_ptr,int n,u32b f1,u32b f2,u32b f3,u32b f4)
{
  int counter=0,null_mask;
  u32b *f,*t,x;

  while (n)
    {
      /* inefficient, but simple */
      x = 1L << rand_int(32);
      switch (randint(4))
	{
	case 1: 
	  null_mask = TR1_NULL_MASK;
	  f = &f1;
	  t = &o_ptr->art_flags1;
	  break;
	case 2: 
	  null_mask = TR2_NULL_MASK;
	  f = &f2;
	  t = &o_ptr->art_flags2;
	  break;
	case 3: 
	  null_mask = TR3_NULL_MASK;
	  f = &f3;
	  t = &o_ptr->art_flags3;
	  break;
	case 4: 
	  null_mask = TR4_NULL_MASK;
	  f = &f4;
	  t = &o_ptr->art_flags4;
	  break;
	default:
	  null_mask = 0;
	  f = t = NULL;
	  return;
	}
      if (++counter > 10000) break;
      if (x & null_mask) continue;
      if (!(x & *f)) continue;
      if (x & *t) continue;
      /* success */
      *f &= ~x;
      *t |= x;
      n--;
    }
}


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
			if ((artifact_p(o_ptr) || o_ptr->art_name) &&
			    (cnt < 1000)) chance = 100;

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
                                if (o_ptr->tval == TV_RANDART) {
                                        random_artifacts[o_ptr->sval].generated = FALSE;
                                }else{
                                        a_info[o_ptr->name1].cur_num = 0;
                                }
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
	if (o_max < max_o_idx)
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
	int             i, j, p;
	int             k_idx;
	long            value, total;
	object_kind     *k_ptr;
	alloc_entry     *table = alloc_kind_table;


	/* Boost level */
	/*if (level > 0)
	{
		if (rand_int(GREAT_OBJ) == 0)
		{
			level = 1 + (level * MAX_DEPTH / randint(MAX_DEPTH));
		}
	}*/


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

		/* If in a dungeon, and it's an "ICE" object, do not generate it if it's not */
		/* an "ICE" dungeon. */
		/* Don't generate ice stuff in towns either. */
		if ((k_ptr->flags4 & TR4_ICE) && (!(d_info[dungeon_type].flags1 & DF1_ICE) || dun_level == 0)) continue;

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
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Aware item -- use template cost */
        if ((object_aware_p(o_ptr))&&(o_ptr->tval!=TV_EGG)) return (k_ptr->cost);

	/* Analyze the type */
	switch (o_ptr->tval)
	{
		/* Un-aware Potions */
		case TV_POTION: return (20L);

		/* Un-aware Scrolls */
		case TV_SCROLL: return (20L);

		/* Un-aware Staffs */
		case TV_STAFF: return (70L);

		/* Un-aware Wands */
		case TV_WAND: return (50L);

		/* Un-aware Rings */
		case TV_RING: return (45L);

		/* Un-aware Amulets */
		case TV_AMULET: return (45L);

                /* Eggs */
                case TV_EGG:
		{
                        monster_type *m_ptr = &m_list[o_ptr->pval2];
                        monster_race *r_ptr = &r_info[m_ptr->r_idx];

                        /* Pay the monster level */
                        return (r_ptr->level * 100)+100;

			/* Done */
			break;
		}
	}

	/* Paranoia -- Oops */
	return (0L);
}

/* Return the value of the flags the object has... */
s32b flag_cost(object_type * o_ptr, int plusses)
{
	s32b total = 0;
        u32b f1, f2, f3, f4;

        object_flags(o_ptr, &f1, &f2, &f3, &f4);

        if (f1 & TR1_SLAY_ANIMAL) total += 1000;
        if (f1 & TR1_SLAY_EVIL) total += 1000;
        if (f1 & TR1_SLAY_UNDEAD) total += 1000;
        if (f1 & TR1_SLAY_DEMON) total += 1000;
        if (f1 & TR1_SLAY_ORC) total += 1000;
        if (f1 & TR1_SLAY_TROLL) total += 1000;
        if (f1 & TR1_SLAY_GIANT) total += 1000;
        if (f1 & TR1_SLAY_DRAGON) total += 2000;
        if (f1 & TR1_KILL_DRAGON) total += 5000;
        if (f1 & TR1_VORPAL) total += 500;
        if (f1 & TR1_IMPACT) total += 500;
        if (f2 & TR2_SUST_STR) total += 85;
        if (f2 & TR2_SUST_INT) total += 85;
        if (f2 & TR2_SUST_WIS) total += 85;
        if (f2 & TR2_SUST_DEX) total += 85;
        if (f2 & TR2_SUST_CON) total += 85;
        if (f2 & TR2_SUST_CHR) total += 25;
        if (f2 & TR2_REFLECT) total += 1000;
        if (f2 & TR2_HOLD_LIFE) total += 850;
        if (f3 & TR3_SH_FIRE) total += 100;
        if (f3 & TR3_SH_ELEC) total += 100;
	if (f3 & TR3_QUESTITEM) total += 0;
        if (f3 & TR3_DECAY) total += 0;
        if (f3 & TR3_NO_TELE) total += 250;
        if (f3 & TR3_NO_MAGIC) total += 250;
	if (f3 & TR3_WRAITH) total += 250000;
	if (f3 & TR3_EASY_KNOW) total += 0;
	if (f3 & TR3_HIDE_TYPE) total += 0;
	if (f3 & TR3_SHOW_MODS) total += 0;
	if (f3 & TR3_INSTA_ART) total += 0;
        if (f3 & TR3_LITE) total += 125;
        if (f3 & TR3_SEE_INVIS) total += 200;
        if (f3 & TR3_TELEPATHY) total += 1250;
        if (f3 & TR3_SLOW_DIGEST) total += 75;
        if (f3 & TR3_REGEN) total += 250;
        if (f3 & TR3_XTRA_MIGHT) total += 225;
        if (f3 & TR3_XTRA_SHOTS) total += 1000;
        if (f3 & TR3_IGNORE_ACID) total += 10;
        if (f3 & TR3_IGNORE_ELEC) total += 10;
        if (f3 & TR3_IGNORE_FIRE) total += 10;
        if (f3 & TR3_IGNORE_COLD) total += 10;
        if (f3 & TR3_DRAIN_EXP) total -= 1250;
	if (f3 & TR3_TELEPORT)
	{
		if (o_ptr->ident & IDENT_CURSED)
			total -= 7500;
		else
                        total += 25;
	}
        if (f3 & TR3_AGGRAVATE) total -= 1000;
        if (f3 & TR3_BLESSED) total += 75;
        if (f3 & TR3_CURSED) total -= 500;
        if (f3 & TR3_HEAVY_CURSE) total -= 1250;
        if (f3 & TR3_PERMA_CURSE) total -= 1500;
        if (f3 & TR3_FEATHER) total += 125;
        if (f4 & TR4_FLY) total += 1000;
        if (f4 & TR4_NEVER_BLOW) total -= 1500;
        if (f4 & TR4_CLONE) total -= 1000;
        if (f4 & TR4_LEVELS) total += 50000;
        if (f4 & TR4_CHARGEABLE) total += 30000;
	if (f4 & TR4_ENCHANTED) total += 10;
	if (f4 & TR4_ETERNAL) total += 3000;

	return total;
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
s32b object_value_real(object_type *o_ptr)
{
	s32b value;
	int i;

        u32b f1, f2, f3, f4;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	if (o_ptr->tval == TV_RANDART) {
                return random_artifacts[o_ptr->sval].cost;
	}

	/* Hack -- "worthless" items */
	if (!k_ptr->cost) return (0L);

	/* Base cost */
	value = k_ptr->cost;

	/* Extract some flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* Hack -- "worthless" artifacts */
		if (!a_ptr->cost) return (0L);

		/* Hack -- Use the artifact cost instead */
		value = a_ptr->cost;
	}
	else if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3 || o_ptr->art_flags4)
	{
		value += flag_cost (o_ptr, o_ptr->pval);
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


	/* Calculate value based on the bonus. */

	/* Resistances. */
	/* 10 golds per resistances. */
	for (i = 0; i < MAX_RESIST; i++)
	{
		value += o_ptr->resistances[i] * 100;
	}
	/* Stats bonus. */
	/* 50 golds per stats bonus. */
	for (i = 0; i < 6; i++)
	{
		value += o_ptr->statsbonus[i] * 300;
	}

	/* Skills bonus. */
	/* 50 golds per skills bonus. */
	for (i = 0; i < SKILL_MAX; i++)
	{
		value += o_ptr->skillsbonus[i] * 300;
	}

	/* Other bonus. */
	value += o_ptr->extrablows * 1000;
	value += o_ptr->extrashots * 5000;
	value += o_ptr->speedbonus * 750;
	value += o_ptr->lifebonus * 20;
	value += o_ptr->manabonus * 30;
	value += o_ptr->infravision * 30;
	value += o_ptr->spellbonus * 100;
	value += o_ptr->invisibility * 1000;
	value += o_ptr->light * 10;
	value += o_ptr->reflect * 30;

	if (o_ptr->branddam > 3) value += o_ptr->branddam / 3;


	/* Analyze the item */
	switch (o_ptr->tval)
	{
                /* Eggs */
                case TV_EGG:
		{
                        monster_type *m_ptr = &m_list[o_ptr->pval2];
                        monster_race *r_ptr = &r_info[m_ptr->r_idx];

                        /* Pay the monster level */
                        value += r_ptr->level * 100;

			/* Done */
			break;
		}

		/* Wands/Staffs */
		case TV_WAND:
		case TV_STAFF:
		{
			/* Pay extra for charges */
			value += ((value / 20) * o_ptr->pval);

			/* Done */
			break;
		}

		/* Licialhyd */
		case TV_LICIALHYD:
		{
			s32b licialvalue;
			call_lua("get_licialhyd_value", "O", "l", o_ptr, &licialvalue);

			value += licialvalue;

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

                case TV_RANDART: {
                        return FALSE;
                }

                case TV_INSTRUMENT:
                {
                        return FALSE;
                }

                case TV_HYPNOS:
                case TV_EGG:
		case TV_LICIALHYD:
                {
                        return FALSE;
                }

                /* Corpses*/
                case TV_CORPSE:
		{
                        return FALSE;
		}

		/* Potions and Scrolls */
		case TV_POTION:
		{
			if ((o_ptr->branddam != j_ptr->branddam) || (o_ptr->brandtype != j_ptr->brandtype) || (o_ptr->brandrad != j_ptr->brandrad)) return FALSE;
			break;
		}

		case TV_SCROLL:
		{
                        if(o_ptr->pval != j_ptr->pval) return FALSE;
                        if(o_ptr->pval2 != j_ptr->pval2) return FALSE;
			break;
		}

                case TV_CRYSTAL:
                case TV_SOUL:
		{
                        if(o_ptr->pval != j_ptr->pval) return FALSE;
			break;
		}

		case TV_ESSENCE:
		{
			return (0);
			break;
		}

		/* Staffs */
		case TV_STAFF:
		{
			/* Require either knowledge or known empty for both staffs. */
			if ((!(o_ptr->ident & (IDENT_EMPTY)) && 
				!object_known_p(o_ptr)) || 
				(!(j_ptr->ident & (IDENT_EMPTY)) && 
				!object_known_p(j_ptr))) return(0);

			/* Require identical charges, since staffs are bulky. */
			if (o_ptr->pval != j_ptr->pval) return (0);

                        /* Beware artifatcs should not combibne with "lesser" thing */
                        if (o_ptr->name1 != j_ptr->name1) return (0);

			/* Assume okay */
			break;
		}
		
		/* Wands */
		case TV_WAND:
		{
		
			/* Require either knowledge or known empty for both wands. */
			if ((!(o_ptr->ident & (IDENT_EMPTY)) && 
				!object_known_p(o_ptr)) || 
				(!(j_ptr->ident & (IDENT_EMPTY)) && 
				!object_known_p(j_ptr))) return(0);

                        /* Beware artifatcs should not combibne with "lesser" thing */
                        if (o_ptr->name1 != j_ptr->name1) return (0);

                        /* Wand charges combine in PernAngband.  */

			/* Assume okay */
			break;
		}

		/* Weapons and Armor */
		case TV_RANGED:
                case TV_THROWING:
		case TV_DIGGING:
		case TV_WEAPON:
                case TV_ROD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
                case TV_ARM_BAND:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Require permission */
                        if (!stack_allow_items || o_ptr->pval3 != j_ptr->pval3) return (0);

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
                /* Gold */
                case TV_GOLD:
                {
                        /* NEVER STACK */
                        return (0);
                } 

		/* Missiles */
		case TV_AMMO:
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

	/* Hack -- blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr);

	/* Hack -- clear "storebought" if only one has it */
	if (((o_ptr->ident & IDENT_STOREB) || (j_ptr->ident & IDENT_STOREB)) &&
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

	/* Hack -- if wands are stacking, combine the charges. -LM- */
	if (o_ptr->tval == TV_WAND)
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
	for (k = 1; k < max_k_idx; k++)
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
	int i;
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

        /* Default "pval3" for weapons */
        if (o_ptr->tval == TV_WEAPON || o_ptr->tval == TV_ROD)
        {
        o_ptr->pval3 = 20 + randint(10);
        if (o_ptr->name1 || o_ptr->name2 == 131)
        {
               o_ptr->pval3 += 50;
        }
        o_ptr->xtra1 = 0;
        }


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

	/* Default brand */
	if (k_ptr->brandtype > 0)
	{
		o_ptr->brandtype = k_ptr->brandtype;
		o_ptr->branddam = k_ptr->branddam;
		o_ptr->brandrad = k_ptr->brandrad;
	}
	else
	{
		o_ptr->brandtype = 0;
		o_ptr->branddam = 0;
		o_ptr->brandrad = 0;
	}

	/* Default resistances */
	for (i = 0; i < MAX_RESIST; i++)
	{
		o_ptr->resistances[i] = k_ptr->resistances[i];
	}

	/* Default stats bonus. */
	for (i = 0; i < 6; i++)
	{
		o_ptr->statsbonus[i] = k_ptr->statsbonus[i];
	}

	/* Default skills bonus. */
	for (i = 0; i < SKILL_MAX; i++)
	{
		o_ptr->skillsbonus[i] = k_ptr->skillsbonus[i];
	}

	/* Spells(if any) */
	for (i = 0; i < 20; i++)
	{
		strcpy(o_ptr->spell[i].name, k_ptr->spell[i].name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = k_ptr->spell[i].type;
		o_ptr->spell[i].power = k_ptr->spell[i].power;
		o_ptr->spell[i].special1 = k_ptr->spell[i].special1;
		o_ptr->spell[i].special2 = k_ptr->spell[i].special2;
		o_ptr->spell[i].special3 = k_ptr->spell[i].special3;
		o_ptr->spell[i].summchar = k_ptr->spell[i].summchar;
		o_ptr->spell[i].cost = k_ptr->spell[i].cost;
	}

	/* Other bonuses. */
	o_ptr->itemtype = k_ptr->itemtype;
	o_ptr->itemskill = k_ptr->itemskill;
	o_ptr->extrablows = k_ptr->extrablows;
	o_ptr->extrashots = k_ptr->extrashots;
	o_ptr->speedbonus = k_ptr->speedbonus;
	o_ptr->lifebonus = k_ptr->lifebonus;
	o_ptr->manabonus = k_ptr->manabonus;
	o_ptr->infravision = k_ptr->infravision;
	o_ptr->spellbonus = k_ptr->spellbonus;
	o_ptr->invisibility = k_ptr->invisibility;
	o_ptr->light = k_ptr->light;
	o_ptr->extra1 = k_ptr->extra1;
	o_ptr->extra2 = k_ptr->extra2;
	o_ptr->extra3 = k_ptr->extra3;
	o_ptr->extra4 = k_ptr->extra4;
	o_ptr->extra5 = k_ptr->extra5;
	o_ptr->reflect = k_ptr->reflect;
	o_ptr->cursed = k_ptr->cursed;

	/* Item events */
	o_ptr->event_passive_equipped = k_ptr->event_passive_equipped;
	o_ptr->event_passive_carried = k_ptr->event_passive_carried;
	o_ptr->event_passive_floor = k_ptr->event_passive_floor;
	o_ptr->event_pickup = k_ptr->event_pickup;
	o_ptr->event_drop = k_ptr->event_drop;
	o_ptr->event_destroy = k_ptr->event_destroy;
	o_ptr->event_equip = k_ptr->event_equip;
	o_ptr->event_takeoff = k_ptr->event_takeoff;
	o_ptr->event_summon = k_ptr->event_summon;
	o_ptr->event_unsummon = k_ptr->event_unsummon;
	o_ptr->event_spawn = k_ptr->event_spawn;
	o_ptr->event_misc = k_ptr->event_misc;

	/* Hack -- worthless items are always "broken" */
	if (k_ptr->cost <= 0 && o_ptr->tval != TV_LICIALHYD) o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

        /* Hack give a basic exp/exp level to an object that needs it */
        if(k_ptr->flags4 & TR4_LEVELS)
        {
                o_ptr->level = 1;
                o_ptr->kills = 0;
		o_ptr->tweakpoints = 2;
        }

	/* Ranged weapons are fully loaded. */
	if (o_ptr->tval == TV_RANGED)
	{
		o_ptr->pval2 = k_ptr->extra3;
	}

	/* This is where we run the "event_spawn" event. */
	if (o_ptr->event_spawn != 0)
	{
		call_lua("item_spawn", "(Od)", "", o_ptr, o_ptr->event_spawn);
	}
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
 * Tinker with the random artifact to make it acceptable
 * for a certain depth; also connect a random artifact to an 
 * object.
 */
static void finalize_randart(object_type* o_ptr, int lev) {
  int r;
  int i = 0;
  int foo = lev + randnor(0, 5);
  bool flag = TRUE;

  /* Paranoia */
  if (o_ptr->tval != TV_RANDART) return;

  if (foo < 1) foo = 1;
  if (foo > 100) foo = 100;

  while (flag) {
    r = rand_int(MAX_RANDARTS);
	      
    if (!(random_artifacts[r].generated) || i > 2000) {
      random_artifact* ra_ptr = &random_artifacts[r];

      o_ptr->sval = r;
      o_ptr->pval2 = ra_ptr->activation;
      o_ptr->xtra2 = activation_info[ra_ptr->activation].spell;

      ra_ptr->level = lev;
      ra_ptr->generated = TRUE;
      flag = FALSE;
    }

    i++;
  }
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

	/* Random Artifact */
	else if (o_ptr->art_name)
	{
		msg_print("Random artifact");
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


void random_artifact_resistance(object_type * o_ptr)
{
	bool give_resistance = FALSE, give_power = FALSE;

	if (give_resistance)
	{
		random_resistance(o_ptr);
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
	int i;
	int k_idx = 0;
	int tries;

	if (opening_chest && opening_chest_type == 6) tries = -30000;
	else tries = 0;


	/* No artifacts in the town */
	if (!dun_level) return (FALSE);

	/* Check the artifact list (just the "specials") */
	for (i = tries; i < ART_MIN_NORMAL; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		if (a_ptr->flags3 & TR3_QUESTITEM) continue;

                /* Cannot generate some artifacts because they can only exists in special dungeons/quests/... */
                if ((a_ptr->flags4 & TR4_SPECIAL_GENE) && !hack_allow_special) continue;

		/* Only generate "ICE" stuff in ice dungeons. */
		if ((a_ptr->flags4 & TR4_ICE) && !(d_info[dungeon_type].flags1 & DF1_ICE)) continue;

		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > object_level) continue;

		/* Artifact "rarity roll" */
		if (!(opening_chest && opening_chest_type == 6))
		{
			if (rand_int(a_ptr->rarity) != 0) return (0);
		}

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* XXX XXX Enforce minimum "object" level (loosely) */
		if (k_info[k_idx].level > object_level) continue;

		/* Assign the template */
		object_prep(o_ptr, k_idx);

		/* Mega-Hack -- mark the item as an artifact */
		o_ptr->name1 = i;



		/* Success */
		return (TRUE);
	}

	/* Check the artifact list (just the "specials") */
        for (i = ART_MIN_SPECIAL; i < max_a_idx; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		if (a_ptr->flags3 & TR3_QUESTITEM) continue;

                /* Cannot generate some artifacts because they can only exists in special dungeons/quests/... */
                if ((a_ptr->flags4 & TR4_SPECIAL_GENE) && !hack_allow_special) continue;

		/* Only generate "ICE" stuff in ice dungeons. */
		if ((a_ptr->flags4 & TR4_ICE) && !(d_info[dungeon_type].flags1 & DF1_ICE)) continue;

		/* XXX XXX Enforce minimum "depth" (loosely) */
		if (a_ptr->level > object_level) continue;

		/* Artifact "rarity roll" */
		if (!(opening_chest && opening_chest_type == 6))
		{
			if (rand_int(a_ptr->rarity) != 0) return (0);
		}

		/* Find the base object */
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* XXX XXX Enforce minimum "object" level (loosely) */
		if (k_info[k_idx].level > object_level) continue;

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
	int i;
        u32b f1, f2, f3, f4;
        object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* No artifacts in the town */
	if (!dun_level) return (FALSE);

	/* Paranoia -- no "plural" artifacts */
	if (o_ptr->number != 1) return (FALSE);

	/* Check the artifact list (skip the "specials") */
        for (i = ART_MIN_NORMAL; i < ART_MIN_SPECIAL; i++)
	{
		artifact_type *a_ptr = &a_info[i];

		/* Skip "empty" items */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		if (a_ptr->flags3 & TR3_QUESTITEM) continue;

                /* Cannot generate some artifacts because they can only exists in special dungeons/quests/... */
                if ((a_ptr->flags4 & TR4_SPECIAL_GENE) && !hack_allow_special) continue;

		/* Only generate "ICE" stuff in ice dungeons. */
		if ((a_ptr->flags4 & TR4_ICE) && !(d_info[dungeon_type].flags1 & DF1_ICE)) continue;

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
		/* Ancient chests always have an artifact if possible. */
		if (!(opening_chest && opening_chest_type == 6))
		{
			if (rand_int(a_ptr->rarity) != 0) continue;
		}

		/* Hack -- mark the item as an artifact */
		o_ptr->name1 = i;

		/* Hack: Some artifacts get random extra powers */
		random_artifact_resistance(o_ptr);

                /* Extract some flags */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);

                /* Hack give a basic exp/exp level to an object that needs it */
                if(f4 & TR4_LEVELS)
                {
                        o_ptr->level = 1;
                        o_ptr->kills = 0;
                }

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
		case SV_WAND_HEAL_MONSTER:              o_ptr->pval = randint(20) + 8; break;
		case SV_WAND_HASTE_MONSTER:             o_ptr->pval = randint(20) + 8; break;
		case SV_WAND_CLONE_MONSTER:             o_ptr->pval = randint(5)  + 3; break;
		case SV_WAND_TELEPORT_AWAY:             o_ptr->pval = randint(5)  + 6; break;
		case SV_WAND_DISARMING:                 o_ptr->pval = randint(5)  + 4; break;
		case SV_WAND_TRAP_DOOR_DEST:            o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_STONE_TO_MUD:              o_ptr->pval = randint(8)  + 3; break;
		case SV_WAND_LITE:                      o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_SLEEP_MONSTER:             o_ptr->pval = randint(15) + 8; break;
		case SV_WAND_SLOW_MONSTER:              o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_CONFUSE_MONSTER:           o_ptr->pval = randint(12) + 6; break;
		case SV_WAND_FEAR_MONSTER:              o_ptr->pval = randint(5)  + 3; break;
		case SV_WAND_DRAIN_LIFE:                o_ptr->pval = randint(3)  + 3; break;
                case SV_WAND_WALL_CREATION:             o_ptr->pval = randint(4)  + 3; break;
		case SV_WAND_POLYMORPH:                 o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_STINKING_CLOUD:            o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_MAGIC_MISSILE:             o_ptr->pval = randint(10) + 6; break;
		case SV_WAND_ACID_BOLT:                 o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_CHARM_MONSTER:             o_ptr->pval = randint(6)  + 2; break;
		case SV_WAND_FIRE_BOLT:                 o_ptr->pval = randint(8)  + 6; break;
		case SV_WAND_COLD_BOLT:                 o_ptr->pval = randint(5)  + 6; break;
		case SV_WAND_ACID_BALL:                 o_ptr->pval = randint(5)  + 2; break;
		case SV_WAND_ELEC_BALL:                 o_ptr->pval = randint(8)  + 4; break;
		case SV_WAND_FIRE_BALL:                 o_ptr->pval = randint(4)  + 2; break;
		case SV_WAND_COLD_BALL:                 o_ptr->pval = randint(6)  + 2; break;
		case SV_WAND_WONDER:                    o_ptr->pval = randint(15) + 8; break;
		case SV_WAND_ANNIHILATION:              o_ptr->pval = randint(2)  + 1; break;
		case SV_WAND_DRAGON_FIRE:               o_ptr->pval = randint(3)  + 1; break;
		case SV_WAND_DRAGON_COLD:               o_ptr->pval = randint(3)  + 1; break;
		case SV_WAND_DRAGON_BREATH:             o_ptr->pval = randint(3)  + 1; break;
		case SV_WAND_ROCKETS:                   o_ptr->pval = randint(2)  + 1; break;
	}
}



/*
 * Charge a new staff.
 */
static void charge_staff(object_type *o_ptr)
{
	switch (o_ptr->sval)
	{
		case SV_STAFF_DARKNESS:                 o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_SLOWNESS:                 o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_HASTE_MONSTERS:           o_ptr->pval = randint(8)  + 8; break;
		case SV_STAFF_SUMMONING:                o_ptr->pval = randint(3)  + 1; break;
		case SV_STAFF_TELEPORTATION:            o_ptr->pval = randint(4)  + 5; break;
		case SV_STAFF_IDENTIFY:                 o_ptr->pval = randint(15) + 5; break;
		case SV_STAFF_REMOVE_CURSE:             o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_STARLITE:                 o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_LITE:                     o_ptr->pval = randint(20) + 8; break;
		case SV_STAFF_MAPPING:                  o_ptr->pval = randint(5)  + 5; break;
		case SV_STAFF_DETECT_GOLD:              o_ptr->pval = randint(20) + 8; break;
		case SV_STAFF_DETECT_ITEM:              o_ptr->pval = randint(15) + 6; break;
		case SV_STAFF_DETECT_TRAP:              o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_DETECT_DOOR:              o_ptr->pval = randint(8)  + 6; break;
		case SV_STAFF_DETECT_INVIS:             o_ptr->pval = randint(15) + 8; break;
		case SV_STAFF_DETECT_EVIL:              o_ptr->pval = randint(15) + 8; break;
		case SV_STAFF_CURE_LIGHT:               o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_CURING:                   o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_HEALING:                  o_ptr->pval = randint(2)  + 1; break;
		case SV_STAFF_THE_MAGI:                 o_ptr->pval = randint(2)  + 2; break;
		case SV_STAFF_SLEEP_MONSTERS:           o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_SLOW_MONSTERS:            o_ptr->pval = randint(5)  + 6; break;
		case SV_STAFF_SPEED:                    o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_PROBING:                  o_ptr->pval = randint(6)  + 2; break;
		case SV_STAFF_DISPEL_EVIL:              o_ptr->pval = randint(3)  + 4; break;
		case SV_STAFF_POWER:                    o_ptr->pval = randint(3)  + 1; break;
		case SV_STAFF_HOLINESS:                 o_ptr->pval = randint(2)  + 2; break;
		case SV_STAFF_GENOCIDE:                 o_ptr->pval = randint(2)  + 1; break;
		case SV_STAFF_EARTHQUAKES:              o_ptr->pval = randint(5)  + 3; break;
		case SV_STAFF_DESTRUCTION:              o_ptr->pval = randint(3)  + 1; break;
                case SV_STAFF_WISHING:                  o_ptr->pval = 1; break;
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
				if (o_ptr->pval > 0) o_ptr->pval = randint(o_ptr->pval);
			}

			/* Hack -- Lanterns -- random fuel */
			if (o_ptr->sval == SV_LITE_LANTERN)
			{
				if (o_ptr->pval > 0) o_ptr->pval = randint(o_ptr->pval);
			}

			break;
		}

                case TV_CORPSE:
		{
                        /* Hack -- choose a monster */
                        monster_race* r_ptr;
                        int r_idx=get_mon_num(dun_level);
                        r_ptr = &r_info[r_idx];

                        if(!(r_ptr->flags1 & RF1_UNIQUE))
                                o_ptr->pval=r_idx;
                        else
                                o_ptr->pval=1;
			break;
		}

                case TV_EGG:
		{
                        /* Hack -- choose a monster */
                        monster_race* r_ptr;
                        int r_idx, count = 0;
                        bool OK = FALSE;

                        while((!OK) && (count<1000))
                        {
                                r_idx = get_mon_num(dun_level);
                                r_ptr = &r_info[r_idx];

                                if(r_ptr->flags9 & RF9_HAS_EGG)
                                {
                                        o_ptr->pval2 = r_idx;
                                        OK = TRUE;
                                }
                                count++;
                        }
                        if(count==1000) o_ptr->pval2 = 434; /* Baby mercury dragon */

                        r_ptr = &r_info[o_ptr->pval2];
                        o_ptr->weight = (r_ptr->weight + rand_int(r_ptr->weight) / 100) + 1;
                        o_ptr->pval = r_ptr->weight * 3 + rand_int(r_ptr->weight) + 1;
                        break;
		}

                case TV_HYPNOS:
		{
                        /* Hack -- choose a monster */
                        monster_race* r_ptr;
                        int r_idx=get_mon_num(dun_level);
                        r_ptr = &r_info[r_idx];

                        o_ptr->pval2 = maxroll(r_ptr->hdice, r_ptr->hside);
                        if(!(r_ptr->flags1 & RF1_NEVER_MOVE))
                                o_ptr->pval=r_idx;
                        else
                                o_ptr->pval=20;
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

                        /* Pick a trap */
                        place_trap_object(o_ptr);

			break;
		}
                case TV_POTION:
                        break;
                
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
void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, bool special)
{
	int i, rolls, f1, f2, power, w;
	char o_name[80];
        
	/* Describe */
	object_desc_store(o_name, o_ptr, FALSE, 0);

        /* Some items are NOT magical... */
	switch (o_ptr->tval)
	{
                case TV_POTION:
                case TV_SCROLL:
                case TV_EGG:
                case TV_CRYSTAL:
                case TV_JUNK:
                case TV_HYPNOS:
                case TV_SOUL:
		case TV_ESSENCE:
                case TV_PARCHEMENT:
                case TV_BATERIE:
                case TV_SKELETON:
                case TV_BOTTLE:
                case TV_SPIKE:
                case TV_CHEST:
                case TV_CORPSE:
                case TV_TOOL:
                case TV_GOLD:
                case TV_BOOK_ELEMENTAL:
                case TV_BOOK_ALTERATION:
                case TV_BOOK_CONJURATION:
                case TV_BOOK_MYSTICISM:
                case TV_BOOK_DIVINATION:
                        return;
                        break;

                /* Rings are special... */
                case TV_RING:
                {
                        if (o_ptr->sval == SV_RING_SPEED)
                        {
                                o_ptr->pval = randint(9);
                        }
                        else if (o_ptr->sval == SV_RING_PROTECTION)
                        {
                                o_ptr->pval = 0;
                                o_ptr->to_a = randint(9) + 7;
                        }
                        else if (o_ptr->sval == SV_RING_LORDLY)
                        {
                                o_ptr->pval = 1;
                                o_ptr->to_a = randint(10) + 10;
                        }
                        else if (o_ptr->sval == SV_RING_DAMAGE)
                        {
                                o_ptr->pval = 0;
                                o_ptr->to_d = randint(9) + 7;
                        }
                        else if (o_ptr->sval == SV_RING_ACCURACY)
                        {
                                o_ptr->pval = 0;
                                o_ptr->to_h = randint(9) + 7;
                        }
                        else if (o_ptr->sval == SV_RING_SLAYING)
                        {
                                o_ptr->pval = 0;
                                o_ptr->to_h = randint(10);
                                o_ptr->to_d = randint(10);
                        }

                        else if (o_ptr->sval == SV_RING_STR || o_ptr->sval == SV_RING_DEX
                        || o_ptr->sval == SV_RING_INT || o_ptr->sval == SV_RING_CON) o_ptr->pval = randint(5);
                        else if (o_ptr->sval == SV_RING_WOE || o_ptr->sval == SV_RING_AGGRAVATION
                        || o_ptr->sval == SV_RING_WEAKNESS || o_ptr->sval == SV_RING_STUPIDITY) o_ptr->pval = (0 - randint(4));
                        else o_ptr->pval = 1;
                        a_m_aux_4(o_ptr, lev, power);
                        break;
                }
                case TV_AMULET:
                {
                        if (o_ptr->sval == SV_AMULET_WISDOM || o_ptr->sval == SV_AMULET_CHARISMA || o_ptr->sval == SV_AMULET_INTELLIGENCE) o_ptr->pval = randint(2);
                        else if (o_ptr->sval == SV_AMULET_THE_MAGI || o_ptr->sval == SV_AMULET_HERO || o_ptr->sval == SV_AMULET_SACRED) o_ptr->pval = 3;
                        else if (o_ptr->sval == SV_AMULET_NINJA || o_ptr->sval == SV_AMULET_GOLEM) o_ptr->pval = 4;
                        else if (o_ptr->sval == SV_AMULET_LIFE) o_ptr->pval = 5;
                        else if (o_ptr->sval == SV_AMULET_SERPENT) o_ptr->pval = randint(3);
                        else if (o_ptr->sval == SV_AMULET_DOOM) o_ptr->pval = -3;
                        else o_ptr->pval = 1; 
                        a_m_aux_4(o_ptr, lev, power);
                        break;
                }
                case TV_STAFF:
                {
                        if (o_ptr->pval == 0)
                        {
                                if (o_ptr->sval == SV_STAFF_GENOCIDE) o_ptr->pval = randint(3);
                                else if (o_ptr->sval == SV_STAFF_WISHING || o_ptr->sval == SV_STAFF_DESTRUCTION) o_ptr->pval = 1;
                                else if (o_ptr->sval == SV_STAFF_NOTHING) o_ptr->pval = 0;
                                else o_ptr->pval = randint(9) + 3;
                        }
                        return;
                        break;
                }
                case TV_WAND:
                {
                        if (o_ptr->pval == 0)
                        {
                                if (o_ptr->sval == SV_WAND_DUNGEON_GENERATION || o_ptr->sval == SV_WAND_ANNIHILATION) o_ptr->pval = randint(2);
                                else if (o_ptr->sval == SV_WAND_NOTHING) o_ptr->pval = 0;
                                else o_ptr->pval = randint(9) + 3;
                        }
                        return;
                        break;
                }
                case TV_LITE:
                {
                        if (o_ptr->sval == SV_LITE_TORCH)
                        {
                                o_ptr->pval = randint(2000) + 1000;
                                return;
                        }
                        else if (o_ptr->sval == SV_LITE_LANTERN)
                        {
                                o_ptr->pval = randint(5000) + 5000;
                                return;
                        }
			/* Orbs, on the other hand, can be magical. */
			a_m_aux_4(o_ptr, lev, power);
                        break;
                }
		case TV_LICIALHYD:
		{
			/* If generating a Licialhyd, give it a random type. */
			/* Base it's power on it's type */
			call_lua("prepare_licialhyd", "O", "", o_ptr);
			break;
		}
		case TV_MELODY:
		case TV_HARMONY:
		case TV_RHYTHM:
		{
			if (o_ptr->pval2 == 0)
			{
				object_kind *k_ptr = &k_info[o_ptr->k_idx];
				o_ptr->pval2 = k_ptr->extra;
			}
			return;
			break;
		}
		
                default:
		    {
                	a_m_aux_4(o_ptr, lev, power);
                	break;
                }
        }

	/* There used to be a maximum for items levels... */
        /* Maximum "level" for various things */
	/* if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;*/


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

		/* Artifact Brand! */
		if (a_ptr->brandtype > 0)
		{
			o_ptr->brandtype = a_ptr->brandtype;
			o_ptr->branddam = a_ptr->branddam;
			o_ptr->brandrad = a_ptr->brandrad;
		}
		else
		{
			o_ptr->brandtype = 0;
			o_ptr->branddam = 0;
			o_ptr->brandrad = 0;
		}

		/* Default resistances */
		for (w = 0; w < MAX_RESIST; w++)
		{
			o_ptr->resistances[w] = a_ptr->resistances[w];
		}

		/* Default stats bonus. */
		for (i = 0; i < 6; i++)
		{
			o_ptr->statsbonus[i] = a_ptr->statsbonus[i];
		}

		/* Default skills bonus. */
		for (i = 0; i < SKILL_MAX; i++)
		{
			o_ptr->skillsbonus[i] = a_ptr->skillsbonus[i];
		}

		/* Spells(if any) */
		for (i = 0; i < 20; i++)
		{
			strcpy(o_ptr->spell[i].name, a_ptr->spell[i].name);
                	strcpy(o_ptr->spell[i].act, "");
			o_ptr->spell[i].type = a_ptr->spell[i].type;
			o_ptr->spell[i].power = a_ptr->spell[i].power;
			o_ptr->spell[i].special1 = a_ptr->spell[i].special1;
			o_ptr->spell[i].special2 = a_ptr->spell[i].special2;
			o_ptr->spell[i].special3 = a_ptr->spell[i].special3;
			o_ptr->spell[i].summchar = a_ptr->spell[i].summchar;
			o_ptr->spell[i].cost = a_ptr->spell[i].cost;
		}

		/* Hack give a basic exp/exp level to an object that needs it */
                if(a_ptr->flags4 & (TR4_LEVELS))
                {
                        o_ptr->level = 1;
                        o_ptr->kills = 0;
			o_ptr->tweakpoints = 2;
                }

		/* Other bonuses. */
		o_ptr->itemtype = a_ptr->itemtype;
		o_ptr->itemskill = a_ptr->itemskill;
		o_ptr->extrablows = a_ptr->extrablows;
		o_ptr->extrashots = a_ptr->extrashots;
		o_ptr->speedbonus = a_ptr->speedbonus;
		o_ptr->lifebonus = a_ptr->lifebonus;
		o_ptr->manabonus = a_ptr->manabonus;
		o_ptr->infravision = a_ptr->infravision;
		o_ptr->spellbonus = a_ptr->spellbonus;
		o_ptr->invisibility = a_ptr->invisibility;
		o_ptr->light = a_ptr->light;
		o_ptr->extra1 = a_ptr->extra1;
		o_ptr->extra2 = a_ptr->extra2;
		o_ptr->extra3 = a_ptr->extra3;
		o_ptr->extra4 = a_ptr->extra4;
		o_ptr->extra5 = a_ptr->extra5;
		o_ptr->reflect = a_ptr->reflect;
		o_ptr->cursed = a_ptr->cursed;

		/* Item events */
		o_ptr->event_passive_equipped = a_ptr->event_passive_equipped;
		o_ptr->event_passive_carried = a_ptr->event_passive_carried;
		o_ptr->event_passive_floor = a_ptr->event_passive_floor;
		o_ptr->event_pickup = a_ptr->event_pickup;
		o_ptr->event_drop = a_ptr->event_drop;
		o_ptr->event_destroy = a_ptr->event_destroy;
		o_ptr->event_equip = a_ptr->event_equip;
		o_ptr->event_takeoff = a_ptr->event_takeoff;
		o_ptr->event_summon = a_ptr->event_summon;
		o_ptr->event_unsummon = a_ptr->event_unsummon;
		o_ptr->event_spawn = a_ptr->event_spawn;
		o_ptr->event_misc = a_ptr->event_misc;

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

		/* Cheat -- peek at the item */
                if ((cheat_peek)) object_mention(o_ptr);

		/* Done */
		return;
	}


	if (o_ptr->art_name) rating += 40;

        if (good)
        {
                if (is_weapon(o_ptr) || o_ptr->tval == TV_RANGED || o_ptr->tval == TV_AMMO || o_ptr->tval == TV_DIGGING ||
                o_ptr->tval == TV_RING || o_ptr->tval == TV_THROWING)
                {
                        o_ptr->to_d = randint(10);
                        o_ptr->to_h = randint(10);
                }
                else o_ptr->to_a = randint(10);
        }

        /* Hack -- analyze ego-items */
        /* Attempt to generate random ego item */

        /* "Good" items MAY turn into "Great" items! */
        if (good)
        {
                if (randint(100) <= 10)
                {
                        great = TRUE;
                }
        }

        if (great)
        {
		ego_item_type *e_ptr = &e_info[o_ptr->name2];
                object_kind *k_ptr = &k_info[o_ptr->k_idx];
                u32b f1, f2, f3, f4;

		call_lua("make_item_magic", "(Odb)", "", o_ptr, lev, special);

		/* Cheat -- describe the item */
                if ((cheat_peek)) object_mention(o_ptr);

		/* Done */
		return;
	}


	/* Examine real objects */
	if (o_ptr->k_idx)
	{
                u32b f1, f2, f3, f4;

		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Hack -- acquire "broken" flag */
		if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

                /* Extract some flags */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);

                /* Hack give a basic exp/exp level to an object that needs it */
                if (f4 & TR4_LEVELS)
                {
                        o_ptr->level = 1;
                        o_ptr->kills = 0;
			o_ptr->tweakpoints = 2;
                }

                /* Hacccccccckkkkk attack ! :) -- To prevent som ugly crashs */
                if ((o_ptr->tval == TV_MSTAFF) && (o_ptr->sval == SV_MSTAFF) && (o_ptr->pval < 0))
                {
                        o_ptr->pval = 0;
                }

		/* Give a random activation to rods. */
		if (o_ptr->tval == TV_ROD)
		{
			call_lua("prepare_rods_activations", "Od", "", o_ptr, lev);
		}
	}
}


/*
 * Determine if an object must not be generated.
 */
static bool kind_is_legal(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

        if (k_ptr->tval == TV_CORPSE)
	{
                if (k_ptr->sval != SV_CORPSE_SKULL && k_ptr->sval != SV_CORPSE_SKELETON &&
                         k_ptr->sval != SV_CORPSE_HEAD && k_ptr->sval != SV_CORPSE_CORPSE)
		{
			  return TRUE;
		}
		else
		{
			return FALSE;
		}
	}

        if (k_ptr->tval == TV_HYPNOS) return FALSE;

	/* When opening Magic chests and higher types of chests, prevent some "junk" from being generated. */
	if (opening_chest)
	{
		if (opening_chest_type >= 4)
		{
			if (k_ptr->tval == TV_WEAPON || k_ptr->tval == TV_ROD || k_ptr->tval == TV_RANGED || k_ptr->tval == TV_AMMO
			|| k_ptr->tval == TV_SOFT_ARMOR || k_ptr->tval == TV_HARD_ARMOR || k_ptr->tval == TV_DRAG_ARMOR || k_ptr->tval == TV_GLOVES
			|| k_ptr->tval == TV_ARM_BAND || k_ptr->tval == TV_SHIELD || k_ptr->tval == TV_HELM || k_ptr->tval == TV_CLOAK
			|| k_ptr->tval == TV_BOOTS || k_ptr->tval == TV_RING || k_ptr->tval == TV_AMULET || k_ptr->tval == TV_INSTRUMENT
			|| k_ptr->tval == TV_LITE || k_ptr->tval == TV_THROWING) return TRUE;
			else return FALSE;
		}
	}

	/* Assume legal */
	return TRUE;
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
                case TV_ARM_BAND:
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
		case TV_RANGED:
		case TV_WEAPON:
                case TV_ROD:
		case TV_DIGGING:
		case TV_THROWING:
		{
			if (k_ptr->to_h < 0) return (FALSE);
			if (k_ptr->to_d < 0) return (FALSE);
			return (TRUE);
		}

		/* Ammo -- Arrows/Bolts are good */
		case TV_AMMO:
		{
			return (TRUE);
		}

                /* Books -- High level books are good */
                case TV_BOOK_ELEMENTAL:
                case TV_BOOK_ALTERATION:
                case TV_BOOK_MYSTICISM:
                case TV_BOOK_CONJURATION:
                case TV_BOOK_DIVINATION:
		{
			if (k_ptr->sval >= SV_BOOK_MIN_GOOD && !(opening_chest)) return (TRUE);
			return (FALSE);
		}

		/* Rings are good. */
		case TV_RING:
		{
			return (TRUE);
		}

		/* Amulets are good. */
		case TV_AMULET:
		{
			return (TRUE);
		}

		/* Instruments are good. */
		case TV_INSTRUMENT:
		{
			return (TRUE);
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
	int prob, base;
	int cursechance;


	/* Chance of "special object" */
	prob = (good ? 10 : 1000);

	/* Base level for the object */
	/*base = (good ? (object_level + 10) : object_level);*/
	base = object_level;

	/* Twist Fate: Items! */
	if (!opening_chest)
	{
		if (!(p_ptr->inside_quest) && p_ptr->events[29006] == 1)
		{
			base += fate_items(1);
			if (base <= 0) base = 1;
		}
	}

	/* An item may be afflicted by a curse(but not always). */
	if (p_ptr->cursed >= 4) cursechance = p_ptr->cursed / 4;
	else cursechance = 0;

	if (cursechance > 75) cursechance = 75;

	/* Opening a cursed chest? */
	if (opening_chest)
	{
		cursechance = 0;
	}

	/* Again, no side effects in quest levels. */
	if (cursechance > 0 && !(p_ptr->inside_quest))
	{
		if (randint(100) <= cursechance)
		{
			base -= p_ptr->cursed / 4;
			if (base < 1) base = 1;
		}
	}

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

		/* Activate restriction */
		get_obj_num_hook = kind_is_legal;

		/* Pick a random object */
		k_idx = get_obj_num(base);

		/* Good objects */
		/*if (good)*/
		/*{*/
			/* Clear restriction */
			/*get_obj_num_hook = NULL;*/

			/* Prepare allocation table */
			/*get_obj_num_prep();*/
		/*}*/

		/* Handle failure */
		if (!k_idx) return (FALSE);

		/* Prepare the object */
		object_prep(j_ptr, k_idx);
	}

	/* Apply magic (allow artifacts) */
	/* Twist Fate: Items! */
	if (!(p_ptr->inside_quest) && p_ptr->events[29006] == 1 && !(opening_chest))
	{
		int itype;
		itype = fate_items(2);
		fate_item_modifier = 0;
		if (itype == 1) apply_magic(j_ptr, object_level, TRUE, FALSE, FALSE, FALSE);
		if (itype >= 2)
		{
			fate_item_modifier = itype;
			apply_magic(j_ptr, object_level, TRUE, TRUE, TRUE, FALSE);
		}
		fate_item_modifier = 0;
	}
        else if (opening_chest && opening_chest_type == 7) apply_magic(j_ptr, object_level, TRUE, TRUE, TRUE, TRUE);
	else if (opening_chest && opening_chest_type == 6) apply_magic(j_ptr, object_level + 20, TRUE, TRUE, TRUE, FALSE);
	else if (opening_chest && (opening_chest_type == 4 || opening_chest_type == 5)) apply_magic(j_ptr, object_level, TRUE, TRUE, TRUE, FALSE);
	else apply_magic(j_ptr, object_level, TRUE, good, great, FALSE);

	/* Hack -- generate multiple spikes/missiles */
        /* NewAngband Hack: Give weapons a durability */
	switch (j_ptr->tval)
	{
		case TV_SPIKE:
		case TV_AMMO:
		{
			j_ptr->number = (byte)damroll(6, 7);
                        break;
		}
                        
	}

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) &&
	    (k_info[j_ptr->k_idx].level > dun_level))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - dun_level);

		/* Cheat -- peek at items */
                if ((cheat_peek)) object_mention(j_ptr);
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
	if (!(j_ptr->art_name || artifact_p(j_ptr)) && (rand_int(100) < chance))
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

			/* Require floor space (or shallow terrain) -KMW- */
			if ((c_ptr->feat != FEAT_FLOOR)&&
			    (c_ptr->feat != FEAT_SHAL_WATER) &&
			    (c_ptr->feat != FEAT_GRASS) &&
			    (c_ptr->feat != FEAT_DIRT) &&
			    (c_ptr->feat != FEAT_SHAL_LAVA) &&
                            (c_ptr->feat != FEAT_SNOW) &&
			    (c_ptr->feat != FEAT_FIRE_FIELD) &&
			    (c_ptr->feat != FEAT_COLD_FIELD) &&
			    (c_ptr->feat != FEAT_ELEC_FIELD) &&
			    (c_ptr->feat != FEAT_STORMS) &&
			    (c_ptr->feat != FEAT_THORNED_VINES) &&
			    (c_ptr->feat != FEAT_VINE_FIELD) &&
			    (c_ptr->feat != FEAT_WEBS) &&
			    (c_ptr->feat != FEAT_DARK_MIST)) continue;


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
	if (!flag && !(artifact_p(j_ptr) || j_ptr->art_name))
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

		/* Require floor space (or shallow terrain) -KMW- */
		if ((c_ptr->feat != FEAT_FLOOR)&&
		    (c_ptr->feat != FEAT_SHAL_WATER) &&
		    (c_ptr->feat != FEAT_GRASS) &&
		    (c_ptr->feat != FEAT_DIRT) &&
		    (c_ptr->feat != FEAT_SHAL_LAVA) &&
		    (c_ptr->feat != FEAT_SNOW)) continue;

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

		/* Hack -- Preserve artifacts */
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
void acquirement(int y1, int x1, int num, bool great, bool known)
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

		if (known)
		{
			object_aware(i_ptr);
			object_known(i_ptr);
		}

		/* Drop the object */
		drop_near(i_ptr, -1, y1, x1);
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
	cave_type *c_ptr = &cave[y][x];

	/* Paranoia */
	if ((c_ptr->t_idx == 0) || (c_ptr->info & CAVE_TRDT)) return;
	
	/* Activate the trap */
	c_ptr->info |= CAVE_TRDT;

	/* Notice and redraw */
	note_spot(y, x);
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
	object_type *o_ptr = &inventory[item];
	char        o_name[80];

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
	object_type *o_ptr = &o_list[item];
	char        o_name[80];

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
	int             i, j, k;
	int             n = -1;
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
		s32b    o_value, j_value;

		/* Get the "value" of the item */
		o_value = object_value(o_ptr);

		/* Scan every occupied slot */
		for (j = 0; j < INVEN_PACK; j++)
		{
			j_ptr = &inventory[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;

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
s16b inven_takeoff(int item, int amt, bool force_drop)
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

        /* Took off ammo */
        else if (item == INVEN_AMMO)
	{
                act = "You were carrying in your quiver";
	}

        /* Took off tool */
        else if (item == INVEN_TOOL)
	{
                act = "You were using";
	}

	/* Took off something */
	else
	{
		act = "You were wearing";
	}

	/* Modify, Optimize */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

	/* Possibly call an item event. */
	if (q_ptr->event_takeoff != 0)
	{
		call_lua("item_takeoff", "(Od)", "", q_ptr, q_ptr->event_takeoff);
	}

        if (force_drop)
        {
                drop_near(q_ptr,0, py,px);
                slot = -1;
        }
        else
        {
                /* Carry the object */
                slot = inven_carry(q_ptr, FALSE);
        }

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
                item = inven_takeoff(item, amt, FALSE);

		/* Access original object */
		o_ptr = &inventory[item];
	}

        if(item > -1){
	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

        /*
	 * Hack -- If rods or wands are dropped, the total maximum timeout or 
	 * charges need to be allocated between the two stacks.  If all the items 
	 * are being dropped, it makes for a neater message to leave the original 
	 * stack's pval alone. -LM-
	 */
        if ((o_ptr->tval == TV_WAND))  
	{
		if (o_ptr->tval == TV_WAND)
		{
	  	    q_ptr->pval = o_ptr->pval * amt / o_ptr->number;
		    if (amt < o_ptr->number) o_ptr->pval -= q_ptr->pval;
		}
	}

	/* Modify quantity */
	q_ptr->number = amt;

	/* Describe local object */
	object_desc(o_name, q_ptr, TRUE, 3);

	/* Message */
	msg_format("You drop %s (%c).", o_name, index_to_label(item));

	/* Drop it near the player */
        drop_near(q_ptr, 0, py, px);

	/* Possibly call an item event. */
	if (q_ptr->event_drop != 0)
	{
		call_lua("item_drop", "(Od)", "", q_ptr, q_ptr->event_drop);
	}

	/* Modify, Describe, Optimize */
	inven_item_increase(item, -amt);
	inven_item_describe(item);
	inven_item_optimize(item);
        }
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
	bool            flag = FALSE;


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
	int             i, j, k;
	s32b            o_value;
	s32b            j_value;
	object_type     forge;
	object_type     *q_ptr;
	object_type     *j_ptr;
	object_type     *o_ptr;
	bool            flag = FALSE;


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
        for (this_o_idx = cave[y][x].o_idx; this_o_idx; this_o_idx = next_o_idx)
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

			/* Result */
			return (this_o_idx);
		}

		/* Count objects */
		n++;
	}


	/* Make an object */
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[o_idx];

		/* Structure Copy */
		object_copy(o_ptr, j_ptr);

		/* Location */
		o_ptr->iy = y;
		o_ptr->ix = x;

		/* Forget monster */
		o_ptr->held_m_idx = 0;

		/* Build a stack */
                o_ptr->next_o_idx = cave[y][x].o_idx;

		/* Place the object */
                cave[y][x].o_idx = o_idx;

		/* Notice */
		note_spot(y, x);

		/* Redraw */
		lite_spot(y, x);
	}

	/* Result */
	return (o_idx);
}

/*
 *	Notice a decaying object in the pack
 */
void pack_decay(int item)
{
	object_type *o_ptr=&inventory[item];

   monster_race *r_ptr = &r_info[o_ptr->pval2];

	object_type *i_ptr;
	object_type object_type_body;

   int amt = o_ptr->number;

   s16b m_type;
   s32b wt;

	byte known = o_ptr->name1;

   byte gone = 1;

  	char desc[80];

  	/* Player notices each decaying object */
  	object_desc(desc, o_ptr, TRUE, 3);
  	msg_format("You feel %s decompose.", desc);

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

   /* Remember what creature we were */
   m_type = o_ptr->pval2;

   /* and how much we weighed */
   wt = r_ptr->weight;

   /* Get rid of decayed object */
	inven_item_increase(item, -amt);
	inven_item_optimize(item);

        if (i_ptr->tval == TV_CORPSE)
   {
      /* Monster must have a skull for its head to become one */
                if (i_ptr->sval == SV_CORPSE_HEAD)
      {
         /* Replace the head with a skull */
        object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_SKULL));
         i_ptr->weight = wt / 60 + rand_int(wt) / 600;

         /* Stay here */
         gone = 0;
      }
      /* Monster must have a skeleton for its corpse to become one */
                if ((i_ptr->sval == SV_CORPSE_CORPSE) && (r_ptr->flags3 & RF9_DROP_SKELETON))
      {
         /* Replace the corpse with a skeleton */
        object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_SKELETON));
         i_ptr->weight = wt / 4 + rand_int(wt) / 40;

         /* Stay here */
         gone = 0;
      }

      /* Don't restore if the item is gone */
      if (!gone)
      {
      	i_ptr->number = amt;
        i_ptr->pval2 = m_type;

      	/* Should become "The skull of Farmer Maggot", not "A skull" */
      	if (known)
      	{
       		object_aware(i_ptr);

         	/* Named skeletons are artifacts */
                i_ptr->name1 = 201;
      	}
        inven_carry(i_ptr,TRUE);
      }
   }
}

/*
 *	Decay an object on the floor
 */
void floor_decay(int item)
{
	object_type *o_ptr=&o_list[item];

   monster_race *r_ptr = &r_info[o_ptr->pval2];

	object_type *i_ptr;
	object_type object_type_body;

   int amt = o_ptr->number;

   s16b m_type;
	s32b wt;

	byte known = o_ptr->name1;

   /* Assume we disappear */
   byte gone = 1;

   byte x = o_ptr->ix;
   byte y = o_ptr->iy;

   /* Maybe the player sees it */
  	bool visible = player_can_see_bold(o_ptr->iy, o_ptr->ix);
  	char desc[80];

  	if (visible)
  	{
  		/* Player notices each decaying object */
		object_desc(desc, o_ptr, TRUE, 3);
     	msg_format("You see %s decompose.", desc);
  	}


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

   /* Remember what creature we were */
   m_type = o_ptr->pval2;

   /* and how much we weighed */
   wt = r_ptr->weight;

   floor_item_increase(item, -amt);
   floor_item_optimize(item);

        if (i_ptr->tval == TV_CORPSE)
   {
      /* Monster must have a skull for its head to become one */
                if (i_ptr->sval == SV_CORPSE_HEAD)
      {
         /* Replace the head with a skull */
        object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_SKULL));
         i_ptr->weight = wt / 60 + rand_int(wt) / 600;

         /* Stay here */
         gone = 0;
      }

      /* Monster must have a skeleton for its corpse to become one */
                if ((i_ptr->sval == SV_CORPSE_CORPSE) && (r_ptr->flags3 & RF9_DROP_SKELETON))
      {
         /* Replace the corpse with a skeleton */
        object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_SKELETON));
         i_ptr->weight = wt / 4 + rand_int(wt) / 40;

         /* Stay here */
         gone = 0;
      }

      /* Don't restore if the item is gone */
      if (!gone)
      {
      	i_ptr->number = amt;
        i_ptr->pval2 = m_type;

      	/* Should become "The skull of Farmer Maggot", not "A skull" */
      	if (known)
      	{
       		object_aware(i_ptr);

         	/* Named skeletons are artifacts */
                i_ptr->name1 = 201;
      	}
      	floor_carry(y, x, i_ptr);
      }
   }
}

/* Special version for stores! */
s16b get_obj_num_store(int level)
{
	int             i, j, p;
	int             k_idx;
	long            value, total;
	object_kind     *k_ptr;
	alloc_entry     *table = alloc_kind_table;

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

/* A special code used for the new ranged weapons system. */
s16b drop_near_ammo(object_type *o_ptr, int number, int y, int x)
{
	int i, k, d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;
	int chance;

	s16b o_idx;

	s16b this_o_idx, next_o_idx = 0;

	object_type forge;
	object_type *j_ptr;
	cave_type *c_ptr;

	char o_name[80];

	bool flag = FALSE;
	bool done = FALSE;

	bool plural = FALSE;
	
	/* Get local object */
        j_ptr = &forge;

        object_prep(j_ptr, o_ptr->tval);
	object_copy(j_ptr, o_ptr);
        j_ptr->number = number;
	chance = 0;

	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Describe object */
	object_desc(o_name, j_ptr, FALSE, 0);


	/* Handle normal "breakage" */
	if (!(j_ptr->art_name || artifact_p(j_ptr)) && (rand_int(100) < chance))
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

			/* Require floor space (or shallow terrain) -KMW- */
			if ((c_ptr->feat != FEAT_FLOOR)&&
			    (c_ptr->feat != FEAT_SHAL_WATER) &&
			    (c_ptr->feat != FEAT_GRASS) &&
			    (c_ptr->feat != FEAT_DIRT) &&
			    (c_ptr->feat != FEAT_SHAL_LAVA) &&
                            (c_ptr->feat != FEAT_SNOW)) continue;


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
	if (!flag && !(artifact_p(j_ptr) || j_ptr->art_name))
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

		/* Require floor space (or shallow terrain) -KMW- */
		if ((c_ptr->feat != FEAT_FLOOR)&&
		    (c_ptr->feat != FEAT_SHAL_WATER) &&
		    (c_ptr->feat != FEAT_GRASS) &&
		    (c_ptr->feat != FEAT_DIRT) &&
		    (c_ptr->feat != FEAT_SHAL_LAVA) &&
                    (c_ptr->feat != FEAT_SNOW)) continue;

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

		/* Hack -- Preserve artifacts */
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

/* A function to check if an item is identified. */
bool is_identified(object_type *o_ptr)
{
	if (o_ptr->ident & (IDENT_KNOWN)) return (TRUE);

	return (FALSE);
}

/* The pick items functions. */
/* These are functions that returns an object_type, to be used in lua scripts. */

/* Pick an item from your inventory, equip or floor. */
object_type *lua_pick_item(int tval)
{
	int item;
        object_type             *o_ptr;

        cptr q, s;

	item_tester_tval = tval;

        /* Get an item */
        q = "Choose an item.";
        s = "You have no items!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return (NULL);

        /* Get the item (in the pack) */
        if (item >= 0)
        {
                o_ptr = &inventory[item];
        }
        /* Get the item (on the floor) */
        else
        {
                o_ptr = &o_list[0 - item];
        }

	return (o_ptr);
}

/* Pick an item from your inventory or equip. */
int lua_get_item(int tval)
{
	int item;
        object_type             *o_ptr;

        cptr q, s;

	item_tester_tval = tval;

        /* Get an item */
        q = "Choose an item.";
        s = "You have no items!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return (-1);

	return (item);
}

s16b get_obj_num_tval(int tval, int level)
{
	int             i, j, p;
	int             k_idx;
	long            value, total;
	object_kind     *k_ptr;
	alloc_entry     *table = alloc_kind_table;


	/* Boost level */
	/*if (level > 0)
	{
		if (rand_int(GREAT_OBJ) == 0)
		{
			level = 1 + (level * MAX_DEPTH / randint(MAX_DEPTH));
		}
	}*/


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

		/* If in a dungeon, and it's an "ICE" object, do not generate it if it's not */
		/* an "ICE" dungeon. */
		/* Don't generate ice stuff in towns either. */
		if ((k_ptr->flags4 & TR4_ICE) && (!(d_info[dungeon_type].flags1 & DF1_ICE) || dun_level == 0)) continue;

		/* tval restriction. */
		if (k_ptr->tval != tval) continue;

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

bool make_object_tval(object_type *j_ptr, int tval, bool good, bool great)
{
	int prob, base;
	int cursechance;


	/* Chance of "special object" */
	prob = (good ? 10 : 1000);

	/* Base level for the object */
	/*base = (good ? (object_level + 10) : object_level);*/
	base = object_level;

	/* Twist Fate: Items! */
	if (!(p_ptr->inside_quest) && p_ptr->events[29006] == 1)
	{
		base += fate_items(1);
		if (base <= 0) base = 1;
	}

	/* An item may be afflicted by a curse(but not always). */
	if (p_ptr->cursed >= 4) cursechance = p_ptr->cursed / 4;
	else cursechance = 0;

	if (cursechance > 75) cursechance = 75;

	/* Again, no side effects in quest levels. */
	if (cursechance > 0 && !(p_ptr->inside_quest))
	{
		if (randint(100) <= cursechance)
		{
			base -= p_ptr->cursed / 4;
			if (base < 1) base = 1;
		}
	}


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

		/* Activate restriction */
		get_obj_num_hook = kind_is_legal;

		/* Pick a random object */
		k_idx = get_obj_num_tval(tval, base);

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
	/* Twist Fate: Items! */
	if (!(p_ptr->inside_quest) && p_ptr->events[29006] == 1)
	{
		int itype;
		itype = fate_items(2);
		fate_item_modifier = 0;
		if (itype == 1) apply_magic(j_ptr, object_level, TRUE, FALSE, FALSE, FALSE);
		if (itype >= 2)
		{
			fate_item_modifier = itype;
			apply_magic(j_ptr, object_level, TRUE, TRUE, TRUE, FALSE);
		}
		fate_item_modifier = 0;
	}
        else apply_magic(j_ptr, object_level, TRUE, good, great, FALSE);

	/* Hack -- generate multiple spikes/missiles */
        /* NewAngband Hack: Give weapons a durability */
	switch (j_ptr->tval)
	{
		case TV_SPIKE:
		case TV_AMMO:
		{
			j_ptr->number = (byte)damroll(6, 7);
                        break;
		}
                        
	}

	/* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) &&
	    (k_info[j_ptr->k_idx].level > dun_level))
	{
		/* Rating increase */
		rating += (k_info[j_ptr->k_idx].level - dun_level);

		/* Cheat -- peek at items */
                if ((cheat_peek)) object_mention(j_ptr);
	}
	
	/* Success */
	return (TRUE);
}

void place_object_tval(int y, int x, int tval, bool good, bool great)
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
	if (!make_object_tval(q_ptr, tval, good, great)) return;


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
		/* Hack -- Preserve artifacts */
		if (q_ptr->name1)
		{
			a_info[q_ptr->name1].cur_num = 0;
		}
	}
}

void drop_global_object(int x, int y)
{
	object_type	*q_ptr;

        q_ptr = &global_object;
        object_aware(q_ptr);
        object_known(q_ptr);

	drop_near(q_ptr, -1, y, x);
}

void drop_object_specific(int y, int x, int tval, int sval, int num, int magic)
{
	object_type	forge;
	object_type	*q_ptr;

	/* Get local object */
        q_ptr = &forge;

        object_prep(q_ptr, lookup_kind(tval, sval));
	if (num <= 0) num = 1;
	if (num > 99) num = 99;
        q_ptr->number = num;
        drop_near(q_ptr, -1, y, x);
}

/* Create a chest. */
bool make_chest(object_type *j_ptr)
{
	int base;
	int k_idx;
	int chesttype = 0;
	int chestroll;

	/* Base level for the object */
	base = object_level;

	/* Prepare allocation table */
	get_obj_num_prep();

	/* Pick a chest type. */
	/* This depends on the dungeon level. */
	chestroll = randint(700 + dun_level);
	if (chestroll >= 800) chesttype = CHEST_SPECIAL;
	else if (chestroll >= 700) chesttype = CHEST_ANCIENT;
	else if (chestroll >= 650) chesttype = CHEST_CURSED;
	else if (chestroll >= 550) chesttype = CHEST_MAGIC;
	else if (chestroll >= 450) chesttype = CHEST_STEEL;
	else if (chestroll >= 300) chesttype = CHEST_IRON;
	else chesttype = CHEST_WOODEN;

	k_idx = chesttype;

	/* Handle failure(shouldn't happen) */
	if (!k_idx) return (FALSE);

	/* Prepare the object */
	object_prep(j_ptr, k_idx);

	/* Give the chest a level. */
	/* It's actually the chest's pval. */
	/* Different levels of chests have different levels. */
	/* Steel and higher always have high item levels. */
	/* Iron can be high or a bit lower than the dungeon level, but it tends to be good overall. */
	/* Wooden ranged from "ok" to trash. But they are never locked. */
	if (chesttype >= CHEST_MAGIC) j_ptr->pval = multiply_divide(dun_level, 120, 100) + randint(dun_level / 2) + (dun_level / 10);
	else if (chesttype >= CHEST_STEEL) j_ptr->pval = multiply_divide(dun_level, 110, 100) + randint(dun_level / 2);
	else if (chesttype >= CHEST_IRON) j_ptr->pval = multiply_divide(dun_level, 75, 100) + randint(dun_level / 2);
	else j_ptr->pval = randint(dun_level);
	
	/* Success */
	return (TRUE);
}

/* Place a chest at given location. */
void place_chest(int y, int x)
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
	if (!make_chest(q_ptr)) return;

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