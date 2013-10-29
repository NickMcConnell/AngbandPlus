/* File: object2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
/* randname for psuedo-randart names */
#include "randname.h"


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

	/* update object list */
	p_ptr->window |= PW_OBJLIST;
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
 * Compact and reorder the object list
 *
 * This function can be very dangerous, use with caution!
 *
 * When compacting objects, we first destroy gold, on the basis that by the
 * time item compaction becomes an issue, the player really won't care.
 * We also nuke items marked as squelch.
 *
 * When compacting other objects, we base the saving throw on a combination of
 * object level, distance from player, and current "desperation".
 *
 * After compacting, we "reorder" the objects into a more compact order, and we
 * reset the allocation info, and the "live" array.
 */
void compact_objects(int size)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, cnt;

	int cur_lev, cur_dis, chance;


	/* Reorder objects when not passed a size */
	if (!size)
	{
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

		return;
	}


	/* Message */
	msg_print("Compacting objects...");

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_MAP | PW_OBJLIST);




	/*** Try destroying objects ***/

	/* First do gold */
	for (i = 1; (i < o_max) && (size); i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Nuke gold or squelched items */
		if (o_ptr->tval == TV_GOLD || squelch_item_ok(o_ptr))
		{
			delete_object_idx(i);
			size--;
		}
	}


	/* Compact at least 'size' objects */
	for (cnt = 1; size; cnt++)
	{
		/* Get more vicious each iteration */
		cur_lev = 5 * cnt;

		/* Get closer each iteration */
		cur_dis = 5 * (20 - cnt);

		/* Examine the objects */
		for (i = 1; (i < o_max) && (size); i++)
		{
			object_type *o_ptr = &o_list[i];
			object_kind *k_ptr = &k_info[o_ptr->k_idx];

			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			/* Hack -- High level objects start out "immune" */
#ifdef EFG
			/* EFGchange code cleaning */
			if (k_ptr->level > cur_lev && !(squelch_item_ok(o_ptr)))
#else
			if (k_ptr->level > cur_lev && !k_ptr->squelch)
#endif
				continue;

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
				/* EFGchange code cleaning */
				if ((rand_int(100) < 90) && !(squelch_item_ok(o_ptr)))
					continue;
			}

			/* Dungeon */
			else
			{
				/* Get the location */
				y = o_ptr->iy;
				x = o_ptr->ix;
			}

			/* Nearby objects start out "immune" */
#ifdef EFG
			/* EFGchange code cleaning */
			if ((cur_dis > 0) && (distance(py, px, y, x) < cur_dis) && !(squelch_item_ok(o_ptr)))
#else
			if ((cur_dis > 0) && (distance(py, px, y, x) < cur_dis) && !k_ptr->squelch)
#endif
				continue;

			/* Saving throw */
			chance = 90;


			/* Hack -- only compact artifacts in emergencies */
			if (artifact_p(o_ptr) && (cnt < 1000)) chance = 100;

			/* Apply the saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the object */
			delete_object_idx(i);
			size--;
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

		/* Preserve artifacts */
		if (!character_dungeon || !adult_no_preserve)
		{
			/* Preserve only unknown artifacts */
			if (artifact_p(o_ptr) && !object_known_p(o_ptr))
				a_info[o_ptr->name1].cur_num = 0;
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
 *
 * DJA: This function also checks for and rejects spellbooks of the wrong
 * magic realm.
 */
s16b get_obj_num(int level)
{
	int i, j, p;

	int k_idx;
	/* use specidx to tag the class object */
	int specidx = 0;

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
		/* slightly more common but much smaller boost */
		else if (rand_int(GREAT_OBJ) < 2)
		{
			level += randint(2);
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

		/* Hack -- prevent embedded chests */
		if (opening_chest && (k_ptr->tval == TV_CHEST)) continue;
		
		/* DJA: appropriate spellbooks much more likely to appear than others */
	    /* this is a must-have because of having several spell realms */

	    /* check for spellbook tval */
	    if ((k_ptr->tval >= TV_MAGIC_BOOK) && (k_ptr->tval < TV_GOLD) && (cp_ptr->spell_book))
	    {
           /* is it an appropriate spell book? */
           /* (occationally let them find a foreign book for flavor) */
           if ((!(k_ptr->tval == cp_ptr->spell_book))/* && (randint(100) < 91)*/)
           {
              /* reject it most of the time */
			  /* I decided it's probably better not to replace them */
              if (randint(100) < 92) continue;
#if blaho
              /* always reject alchemy books */
/* do not convert them to correct book because depths are very different */
              if (k_ptr->tval == TV_CHEM_BOOK) continue;
              
              /* Pete said this was bad */
              /* k_ptr->tval = cp_ptr->spell_book; */

              /* Change it into the correct spellbook */
              k_idx = lookup_kind(cp_ptr->spell_book, k_ptr->sval);
              
              /* change the index */
              table[i].index = k_idx;
              
		      /* Get the actual kind */
		      k_ptr = &k_info[k_idx];
#endif
           }
        }

        /* The class object (hard to tell if it works or not) */
		/* change it into something appropriate for each class */
	    if ((k_ptr->tval == TV_SPECIAL) && (k_ptr->sval == SV_CLASS_OBJ))
	    {
           int die = randint(45 + ((goodluck*3)/2));
           
           if (!cotval) continue;
			  /* Get the new object_kind */
           if ((die < 30) || (!cotvalb))
           {
			  k_idx = lookup_kind(cotval, cosval);
           }   
           else
           {
			  k_idx = lookup_kind(cotvalb, cosvalb);
           }
#if doweneedthis
           if ((k_ptr->tval == TV_STAFF) || (k_ptr->tval == TV_WAND))
           {
              k_ptr->pval = 4 + randint(3);
           }
           else if (k_ptr->tval == TV_RING) k_ptr->pval = randint(3);
#endif

			/* Valid item? */
			if (!k_idx) continue;

            /* change the index */
            table[i].index = k_idx;

			/* tag the class object */
			/* (maybe this will let me know for sure that it works) */
			specidx = table[i].index;

		    /* Get the actual kind */
		    k_ptr = &k_info[k_idx];
        }

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

	/* Try for a "better" object once (55% (was 50%)) or twice (10%) */
	if (p < 55)
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

	/* cheat message if the class object was created */
	if ((specidx == table[i].index) && (cheat_peek))
	{
		msg_print("Class object created.");
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
	if (o_ptr->pseudo) o_ptr->pseudo = 0;

	/* The object is not "sensed" */
	o_ptr->ident &= ~(IDENT_SENSE);

	/* Clear the "Empty" info */
	o_ptr->ident &= ~(IDENT_EMPTY);

	/* Now we know about the item */
	o_ptr->ident |= (IDENT_KNOWN);
#ifdef EFG
	/* EFGchange no hidden powers with any artifacts (was just standard artifacts) */
/* ??? Should also add flag to artifacts like Grond unchanged under randarts */
	/* if ((o_ptr->name1) && (!adult_randarts)) */
	if (o_ptr->name1) o_ptr->ident |= IDENT_MENTAL;
#endif

	/* multi-hued poison no longer mimmics another potion once it's been identified */
	if ((o_ptr->tval == TV_POTION) && (o_ptr->sval == SV_POTION_MULTIHUED_POISON))
		o_ptr->pval = 0;

	/** The following moved to this function from do_ident_item() **/

	/* score for finding an artifact */
    if (artifact_p(o_ptr))
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];
		int ascore = (a_ptr->level+4)/5 + (a_ptr->rarity+8)/10 + (a_ptr->artrat+4)/5;
		if (a_ptr->cost >= 100000) ascore += 14 + a_ptr->cost/50000;
		else if (a_ptr->cost >= 75000) ascore += 12;
		else if (a_ptr->cost >= 50000) ascore += 10;
		else if (a_ptr->cost >= 35000) ascore += 8;
		else if (a_ptr->cost >= 20000) ascore += 5;
		else if (a_ptr->cost >= 10000) ascore += 2;
		if (ascore < 5) ascore = 5;
		if (a_ptr->maxlvl < 60) ascore -= 3;
		else if (a_ptr->maxlvl < 90) ascore -= 1;
		p_ptr->game_score += ascore;
	}
	
	/* pseudo-randart egos */
	else if (is_ego_randart(o_ptr)) p_ptr->game_score += 1;

#ifdef yes_c_history
	/*
	 * If the item was an artifact, write a message.
	 */
    if ((artifact_p(o_ptr)) && (o_ptr->randres3 > 0))
	{
		int artifact_depth;
       	char note[120];
		char shorter_desc[120];

		/* Get a shorter description to fit the notes file */
		object_desc(shorter_desc, sizeof(shorter_desc), o_ptr, TRUE, 0);

		/* Build note and write */
       	sprintf(note, "Found %s", shorter_desc);

		/* Record the depth where the artifact was created */
		artifact_depth = o_ptr->randres3;

       	do_cmd_note(note, artifact_depth, FALSE);

		/*
		 * Mark item creation depth 0, which will indicate the artifact
		 * has been previously identified.  This prevents an artifact
		 * from showing up on the notes list twice if the artifact had
		 * been previously identified.  JG
		 */
		o_ptr->randres3 = 0;
	}
#endif /* yes_c_history */
}





/*
 * The player is now aware of the effects of the given object.
 */
void object_aware(object_type *o_ptr)
{
	bool avstaff = FALSE;
#ifdef EFG
	/* EFGchange auto-inscribe objects learned by testing */
	bool was_aware = k_info[o_ptr->k_idx].aware;
#endif
	
	/* check if we need to reveal +0 +0 on a staff */
	if (((o_ptr->pseudo == INSCRIP_AVERAGE) ||
		(o_ptr->pseudo == INSCRIP_DECENT)) && (!was_aware) &&
		(o_ptr->tval == TV_STAFF)) avstaff = TRUE;

#ifdef EFG
	/* Fully aware of the effects */
	k_info[o_ptr->k_idx].aware = TRUE;

	/* EFGchange bugfix squelching when gain awareness */
	p_ptr->notice |= PN_SQUELCH;

	/* need to autoinscribe rods etc when learn via use */
	/* ??? perhaps this should not be done on a single consumable about to vanish */
	if (!was_aware)
	{
		apply_autoinscription(o_ptr);
	}
#else
	/* Fully aware of the effects */
	k_info[o_ptr->k_idx].aware = TRUE;
#endif

	/* sensed as average, so it's indentified once it becomes aware */
    if (avstaff) object_known(o_ptr);

	/* Scrolls can change the graphics when becoming aware */
	if (o_ptr->tval == TV_SCROLL)
	{
		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_MAP | PW_OBJLIST);
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
 * Determine if a weapon is 'blessed'
 */
bool is_blessed(const object_type *o_ptr)
{
	/* Get the flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Is the object blessed? */
	return ((f3 & TR3_BLESSED) ? TRUE : FALSE);
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
	u32b f1, f2, f3, f4;
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Hack -- "worthless" items */
	if (!k_ptr->cost)
    {
       bool worthless = TRUE;

       /* if object has good ego and/or positive to-hit/to-dam bonuses */
       /* then it isn't worthless, even it is of a normally worthless type */
       /* eg "Staff of darkness of Frost" */
       if (o_ptr->name2)
       {
		   ego_item_type *e_ptr = &e_info[o_ptr->name2];
           if (e_ptr->cost) worthless = FALSE;
       }
       if (o_ptr->to_h + o_ptr->to_d + o_ptr->to_a > 3) worthless = FALSE;
       if (worthless) return (0L);
       
       value = 1;
    }
	/* Base cost */
	else value = k_ptr->cost;

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
			/* Hack -- Negative pval is (almost) always bad */
			/* two rings have negative pval but positive value */
			if ((o_ptr->tval == TV_RING) && (o_ptr->pval < 0)) return (value);
            else if (o_ptr->pval < 0) return (0L);

			/* No pval */
			if (!o_ptr->pval) break;

			/* Give credit for stat bonuses */
			if (f1 & (TR1_STR)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_INT)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_WIS)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_DEX)) value += (o_ptr->pval * 200L);
			if (f1 & (TR1_CON)) value += (o_ptr->pval * 200L);
			/* CHR used to be the same as the others */
			if (f1 & (TR1_CHR)) value += (o_ptr->pval * 50L);

			/* Give credit for stealth */
			if (f1 & (TR1_STEALTH)) value += (o_ptr->pval * 100L);

			/* Give credit for alertness and tunneling */
			if (f1 & (TR1_INFRA)) value += (o_ptr->pval * 50L);
			if (f1 & (TR1_TUNNEL)) value += (o_ptr->pval * 40L);
			if (f1 & (TR1_EQLUCK)) value += (o_ptr->pval * 35L);

			/* Give credit for extra attacks */
			if (f1 & (TR1_BLOWS)) value += (o_ptr->pval * 2000L);

			/* Give credit for speed bonus */
			if (f1 & (TR1_SPEED)) value += (o_ptr->pval * 20000L);

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
			/* Pay extra for charges, depending on standard number of charges */
			value += ((value / 20) * (o_ptr->charges / o_ptr->number));
			
			/* magic staffs are weapons now */
            if (o_ptr->tval == TV_STAFF)
			{
			   /* Factor in the bonuses */
			   value += ((o_ptr->to_d) * 85L);
			   value += ((o_ptr->to_h + o_ptr->to_a) * 65L);

			   /* Hack -- Factor in extra damage dice */
			   if ((o_ptr->dd > k_ptr->dd) && (o_ptr->ds == k_ptr->ds))
			   {
				  value += (o_ptr->dd - k_ptr->dd) * o_ptr->ds * 100L;
			   }
            }

			/* Done */
			break;
		}

		/* Rings/Amulets, Lites */
		case TV_RING:
		case TV_AMULET:
		{
			/* Hack -- negative bonuses are (usually) bad */
			if (o_ptr->pval <= 0)
			{
				if (o_ptr->to_a < 0) return (0L);
				if (o_ptr->to_h < 0) return (0L);
				if (o_ptr->to_d < 0) return (0L);
			}

			/* Give credit for bonuses */
			/* value += ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 100L); */
			value += ((o_ptr->to_d) * 100L);
			value += ((o_ptr->to_h + o_ptr->to_a) * 75L);

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
			value += ((o_ptr->to_h - k_ptr->to_h) * 90L);

			/* Give credit for damage bonus */
			value += ((o_ptr->to_d - k_ptr->to_d) * 100L);

			/* Give credit for armor bonus */
			value += (o_ptr->to_a * 90L);

			/* Done */
			break;
		}

		/* Bows/Weapons */
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		case TV_SKELETON:
		{
			/* Hack -- negative hit/damage bonuses */
			if ((o_ptr->to_h + o_ptr->to_d < 0) && (!o_ptr->name2)) return (0L);
			else if (o_ptr->to_h + o_ptr->to_d < 0) break;

			/* Factor in the bonuses */
			/* value += ((o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) * 90L); */
			value += (o_ptr->to_d * 92L);
			value += (o_ptr->to_h * 70L);
			value += (o_ptr->to_a * 75L);

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

	/* Unknown items -- acquire a base value */
	if (object_known_p(o_ptr))
	{
		/* Broken items -- worthless unless ego */
		if ((!ego_item_p(o_ptr)) && (broken_p(o_ptr))) return (0L);

		/* Cursed items -- worthless (?) */
		if (cursed_p(o_ptr)) return (0L);

		/* Real value (see above) */
		value = object_value_real(o_ptr);
	}

	/* Known items -- acquire the actual value */
	else
	{
		/* Hack -- Felt broken items */
		if ((!ego_item_p(o_ptr)) && (o_ptr->ident & (IDENT_SENSE)) && 
			broken_p(o_ptr)) return (0L);

		/* Hack -- Felt cursed items */
		if ((o_ptr->ident & (IDENT_SENSE)) && cursed_p(o_ptr)) return (0L);

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

	/* multi-hued poison potions rarely stack */
	if ((o_ptr->tval == TV_POTION) && 
		(o_ptr->sval == SV_POTION_MULTIHUED_POISON))
	{
		/* both IDed (and known to be multi-hued poison -aware isn't good enough) */
		if ((object_known_p(o_ptr)) && (object_known_p(j_ptr))) /* okay */;
		/* happen to be mimmicing the same other potion type */
		else if (o_ptr->pval == j_ptr->pval) /* okay */;
		else return (0);
	}

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

		/* Rods and wands*/
		case TV_WAND:
		case TV_ROD:
		{
			/* Assume okay */
			break;
		}

		case TV_FLASK:
		{
			if (o_ptr->sval == 0) break; /* oil always stacks */
			/* otherwise fall through */
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
		case TV_SKELETON:
		case TV_STAFF:
		{
			/* Fall through */
		}

		/* Rings, Amulets, Lites */
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			/* Fall through */
		}

		/* Missiles */
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Require identical "bonuses" */
			if (o_ptr->to_h != j_ptr->to_h) return (FALSE);
			if (o_ptr->to_d != j_ptr->to_d) return (FALSE);
			if (o_ptr->to_a != j_ptr->to_a) return (FALSE);
			/* ..and enchantments */
			if (o_ptr->thisbrand != j_ptr->thisbrand) return (FALSE);
			if (o_ptr->timedbrand != j_ptr->timedbrand) return (FALSE);
			if (o_ptr->blessed != j_ptr->blessed) return (FALSE);

            /* Require identical "pval" code */
			if (o_ptr->pval != j_ptr->pval) return (FALSE);

			/* Require identical "artifact" names */
			if (o_ptr->name1 != j_ptr->name1) return (FALSE);

			/* exception for wounding */
			if (((o_ptr->name2 == EGO_WOUNDING) && (!j_ptr->name2)) ||
			   ((j_ptr->name2 == EGO_WOUNDING) && (!o_ptr->name2)))
            {
               /* allow "of wounding" ammo to stack with normal ammo */
            }
            /* Require identical "ego-item" names */
			else if (o_ptr->name2 != j_ptr->name2) return (FALSE);

			/* Hack -- Never stack "powerful" items */
			if (o_ptr->randsus != j_ptr->randsus) return (FALSE);
			if (o_ptr->randsus2 != j_ptr->randsus2) return (FALSE);
			if (o_ptr->randres != j_ptr->randres) return (FALSE);
			if (o_ptr->randres2 != j_ptr->randres2) return (FALSE);
			if (o_ptr->randres3 != j_ptr->randres3) return (FALSE);
			if (o_ptr->randpow != j_ptr->randpow) return (FALSE);
			if (o_ptr->randpow2 != j_ptr->randpow2) return (FALSE);
			if (o_ptr->randslay != j_ptr->randslay) return (FALSE);
			if (o_ptr->randslay2 != j_ptr->randslay2) return (FALSE);
			if (o_ptr->randbon != j_ptr->randbon) return (FALSE);
			if (o_ptr->randbon2 != j_ptr->randbon2) return (FALSE);
			if (o_ptr->randplu != j_ptr->randplu) return (FALSE);
			if (o_ptr->randplu2 != j_ptr->randplu2) return (FALSE);
			if (o_ptr->randdrb != j_ptr->randdrb) return (FALSE);
			if (o_ptr->randdrb2 != j_ptr->randdrb2) return (FALSE);
			if (o_ptr->randimm != j_ptr->randimm) return (FALSE);
			if (o_ptr->randlowr != j_ptr->randlowr) return (FALSE);
			if (o_ptr->randlowr2 != j_ptr->randlowr2) return (FALSE);
			if (o_ptr->randact != j_ptr->randact) return (FALSE);

			/* Hack - Never stack recharging items */
			if ((o_ptr->timeout || j_ptr->timeout) && o_ptr->tval != TV_LITE) 
				return FALSE;
				
			/* Lites must have same amount of fuel */
			else if (o_ptr->timeout != j_ptr->timeout && o_ptr->tval == TV_LITE)
			{
				/* Stack torches if fuel timeout is within a minute */
				if ((ABS(o_ptr->timeout - j_ptr->timeout) < 7) &&
					(o_ptr->sval == SV_LITE_TORCH)) /* okay */;
				else return FALSE;
			}

			/* Require identical values */
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

			/* this because there are now ego (indestructible) books */
			if (o_ptr->name2 != j_ptr->name2) return (FALSE);

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


	/* Hack -- Require compatible inscriptions */
	if (o_ptr->note != j_ptr->note)
	{
		/* Never combine different inscriptions */
		if (o_ptr->note && j_ptr->note) return (0);
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
 */
void object_absorb(object_type *o_ptr, object_type *j_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	int total = o_ptr->number + j_ptr->number;

	/* Add together the item counts */
	o_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Hack -- Blend "known" status */
	if (object_known_p(j_ptr)) object_known(o_ptr);

	/* Hack -- Blend store status */
	if (j_ptr->ident & (IDENT_STORE)) o_ptr->ident |= (IDENT_STORE);

	/* Hack -- Blend "mental" status */
	if (j_ptr->ident & (IDENT_MENTAL)) o_ptr->ident |= (IDENT_MENTAL);

	/* Hack -- Blend "notes" */
	if (j_ptr->note != 0) o_ptr->note = j_ptr->note;

	/* Mega-Hack -- Blend "discounts" */
#ifdef EFG
	/* do not want to get identified objects with "tried" pseudo */
	if (!object_known_p(o_ptr) && (!o_ptr->pseudo || object_known_p(j_ptr)))
		o_ptr->pseudo = j_ptr->pseudo;
#else
	o_ptr->pseudo = j_ptr->pseudo;
#endif

    /* DJA: allow "of wounding" ammo to stack with normal ammo */ 
    /* (only time "(o_ptr->name2 != j_ptr->name2)" is allowed) */
    if (o_ptr->name2 != j_ptr->name2)
    {
       /* remove or add "of wounding" ego */
       /* (makes no difference because "of wounding" just gives */
       /* bigger to-hit/to-dam bonuses when it's created) */
       o_ptr->name2 = j_ptr->name2;
    }

	/*
	 * Hack -- if rods are stacking, re-calculate the
	 * pvals (maximum timeouts) and current timeouts together
	 */
	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->charges = total * k_ptr->pval;
		o_ptr->timeout += j_ptr->timeout;
	}

	/* Hack -- if wands or staves are stacking, combine the charges */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		o_ptr->charges += j_ptr->charges;
	}
	
	/* Stack torches if fuel timeout is within a minute */
	if ((ABS(o_ptr->timeout - j_ptr->timeout) < 7) &&
		(o_ptr->sval == SV_LITE_TORCH))
	{
		/* keep the lesser timeout */
		if (o_ptr->timeout > j_ptr->timeout) o_ptr->timeout = j_ptr->timeout;
		else j_ptr->timeout = o_ptr->timeout;
	}

	/* pick up the enhancement */
	o_ptr->enhance += j_ptr->enhance;
	o_ptr->enhancenum += j_ptr->enhancenum;
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

	/* Oops (don't give message if not testing?) */
	/* if ((p_ptr->wizard) || (cheat_peek) || (cheat_noid) ||
		(p_ptr->noscore & NOSCORE_DEBUG)) */
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

	/* Default pval */
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
	o_ptr->crc = k_ptr->crc;
	/* for double weapons (most objects won't have these values) */
	o_ptr->sbdd = k_ptr->sbdd;
	o_ptr->sbds = k_ptr->sbds;

	/* Hack -- worthless items are (not) always "broken" (what's the purpose of this?) */
	/* if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN); */

	/* Hack -- cursed items are always cursed */
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
	object_desc_spoil(o_name, sizeof(o_name), o_ptr, FALSE, 0);

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
 * Better only called by apply_magic().
 * The return value says if we picked a cursed item (if allowed) and is
 * passed on to a_m_aux1/2().
 * If no legal ego item is found, this routine returns 0, resulting in
 * an unenchanted item.
 * only_good (ogood) is now an int to be able to encourage bad egos
 */
static int make_ego_item(object_type *o_ptr, int ogood)
{
	int i, j, level;
	u32b f1, f2, f3, f4;
    byte ftval, fsval;
	int e_idx;
	long value, total;
	bool throwflag = FALSE;

	ego_item_type *e_ptr;
	alloc_entry *table = alloc_ego_table;

	/* Fail if object already is ego or artifact */
	if (o_ptr->name1) return (FALSE);
	if (o_ptr->name2) return (FALSE);

	level = object_level;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (f3 & TR3_THROWN) throwflag = TRUE;
	/* don't use fake tval & sval for grenades */
	if ((throwflag) && (o_ptr->tval == TV_FLASK)) throwflag = FALSE;
	
	/* use fake tval and sval so it can be modified */
    ftval = o_ptr->tval;
	fsval = o_ptr->sval;

	/* Weapons with THROWN flag can't be wielded */
	/* so they get ammo egos instead of normal weapon egos */
	/* (gets a fake sval with TV_SHOT for the purpose of chosing an ego) */
	if ((o_ptr->tval == TV_SKELETON) && (o_ptr->sval == SV_VASP_STING) && 
		(randint(100) < 35 + (100-level)))
    {
       /* (usually) prevent vasp stingers from getting branded egos */
       /* because they already have a poison brand */
       ftval = 16;
       fsval = 7;
    }
    else if ((throwflag) && (f3 & TR3_RTURN))
    {
       /* prevent boomerangs from getting 'throwing and returning' ego */
       /* because they already have both those flags */
       ftval = 16;
       fsval = 6;
    }
    else if (throwflag)
    {
       ftval = 16;
       fsval = 5;
    }

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

		/* soft cap max level for egos */
		if ((e_ptr->maxlev > 10) && (e_ptr->maxlev < level))
		{
			int ediff = (level - e_ptr->maxlev) * 2;
			if (ediff > 40) ediff = 40;
			if (rand_int(100) < ediff + 58) continue;
		}

		/* If we force good/great, don't create cursed */
		if ((ogood > 0) && (e_ptr->flags3 & TR3_LIGHT_CURSE)) continue;

		/* If we want a cursed item, try for a cursed item */
		if ((ogood < 0) && (!((e_ptr->flags3 & TR3_LIGHT_CURSE) || (e_ptr->cost <= 0))) && 
			(randint(100) < 50)) continue;

		/* Test if this is a legal ego-item type for this object */
		for (j = 0; j < EGO_TVALS_MAX; j++)
		{
			/* Require identical base type */
			if (ftval == e_ptr->tval[j])
			{
				/* Require sval in bounds, lower */
				if (fsval >= e_ptr->min_sval[j])
				{
					/* Require sval in bounds, upper */
					if (fsval <= e_ptr->max_sval[j])
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
	o_ptr->name2 = e_idx;


	/* amulet of adornment of luck: may be good or bad */
	if (o_ptr->name2 == EGO_LOOKLUCKY)
	{
		int power;

		if (ogood < 0) power = -2;
		else if ((ogood == 0) && (randint(100) < 16)) power = -2;
		else power = 2;

        if (power == -2)
		{
			/* Broken */
			o_ptr->ident |= (IDENT_BROKEN);

			/* cursed (but doesn't have LIGHT_CURSE flag) */
			o_ptr->ident |= (IDENT_CURSED);
		}
		return power;
	}

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
static bool make_artifact_special(object_type *o_ptr)
{
	int i;
	int k_idx;


	/* No artifacts, do nothing */
	if (adult_no_artifacts) return (FALSE);

	/* No artifacts in the town */
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
	u32b f1, f2, f3, f4;

	/* No artifacts, do nothing */
	if (adult_no_artifacts) return (FALSE);

	/* No artifacts in the town */
	if (!p_ptr->depth) return (FALSE);

	/* check for THROWN flag */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	/* Paranoia -- no "plural" artifacts (but allow sets of throwing artifacts) */
	if ((o_ptr->number != 1) && (!(f3 & TR3_THROWN))) return (FALSE);

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

		/* New: maximum depth */
		if (a_ptr->maxlvl < p_ptr->depth) continue;

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
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base damage dice boosting
 * Hack -- note special processing for weapon/digger
 *
 * New: the KEEP_BONUS flag makes the combat bonuses
 * stay positive even if the item is cursed.
 */
static void a_m_aux_1(object_type *o_ptr, int level, int power)
{
	int tohit1 = randint(5) + m_bonus(5, level);
	int todam1 = /* randint(5) +*/ m_bonus(5, level);
	int todice;

	int tohit2 = m_bonus(10, level);
	int todam2 = m_bonus(10, level);

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Get flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	
	/* experimental to (usually) replace + randint(5) on todam1 */
	/* (melee weapon tvals excluding staves and shovels) */
	if ((rand_int(100) < 70) && ((o_ptr->tval == TV_SKELETON) || 
		((o_ptr->tval >= TV_HAFTED) && (o_ptr->tval <= TV_SWORD))))
	{
		/* average damage from dice */
		todice = (k_ptr->dd * (k_ptr->ds+1)) / 2;
		if (todice < 1) todice = 1;
		/* factor weight */
		if (f3 & TR3_THROWN)
		{
			/* javelin is heaviest pure throwing weapon at 4lb (weight == 40) */
			/* (big dice weapons with throwing ego will get big +dam bonus) */
			if (k_ptr->weight >= 100) todice += k_ptr->weight/25;
			else if (k_ptr->weight >= 20) todice += k_ptr->weight/20;
			
			/* td==2, ta==4, jav==6, mts==5, vs==2, bb==7 */
			if (todice < 4) todam1 += randint(3);
			else if (todice < 6) todam1 += randint(todice + rand_int(2));
			else if (todice < 8) todam1 += randint(todice + 1);
			/* (for big dice weapons with throwing ego) */
			else todam1 += randint(7 + (todice/6));
		}
		else
		{
			if (todice > 4) todice -= todice/5;
			if (k_ptr->weight > 50) todice += k_ptr->weight/50;
		
			if (todice < 3) todam1 += randint(2 + rand_int(2));
			else if (todice < 5) todam1 += randint(todice);
			else if (todice < 8) todam1 += randint(5);
			else todam1 += 1 + randint((todice+1)/2);
		}
	}
	/* magic staffs depend on native level for its type instead of weight */
	else if ((rand_int(100) < 62) && (o_ptr->tval == TV_STAFF))
	{
		int stlev = (k_ptr->level + level)/2 + goodluck/3;
		if (stlev < 9) todam1 += randint(3);
		else if (stlev < 18) todam1 += randint(4);
		else if (stlev < 24) todam1 += randint(5);
		else todam1 += randint(4 + randint(stlev/12));
	}
	else todam1 += randint(5);
	
	/* To make it slightly more likely to have worthwhile broken daggers/swords */
	if ((k_ptr->to_h < 0) && (randint(100) < 31+goodluck) && (level > 1))
	{ 
		int ifh = ABS(k_ptr->to_h);
		if (ifh < 2) tohit1 += 1;
		else tohit1 += randint(ifh);
	}
	if ((k_ptr->to_d < 0) && (randint(100) < 31+goodluck) && (level > 1)) 
	{ 
		int ifd = ABS(k_ptr->to_d);
		if (ifd < 2) todam1 += 1;
		else todam1 += randint(ifd);
	}
	
	/* Good */
	if ((power > 0) || (f4 & TR4_KEEP_BONUS))
	{
		/* Enchant */
		o_ptr->to_h += tohit1;
		o_ptr->to_d += todam1;

		/* 'of shielding' ego isn't mainly for offence */
        if (o_ptr->name2 == EGO_SHIELDING)
		{
           /* force to_d <= to_h */
		   if (o_ptr->to_d > o_ptr->to_h)
           {
              /* swap bonuses */
              s16b tmp = o_ptr->to_d;
              o_ptr->to_d = o_ptr->to_h;
              o_ptr->to_h = tmp;
           }
           return; /* never get extra dice */
        }

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
			/* bad */
			if (power < 0)
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
		case TV_SKELETON:
		{
            /* Very Good */
			if (power > 1)
			{
				int bbig = 0;
				/* certain egos have a bigger chance for extra dice */
				/* (influenced by depth of creation) */
                if (o_ptr->name2 == EGO_CRITICAL_WEIGHT) bbig = 35 + (level/10);
				
				/* throwing weapons of critical weight */
                if (o_ptr->name2 == EGO_CRITICAL_AMMO) bbig = 20 + (level/10);
				
				/* bone-weapon goring ego (has EXTRA_CRIT) */	
				if (o_ptr->name2 == EGO_GORING) bbig = 9 + (level/15);
					
				/* rare stiletto ego (has EXTRA_CRIT) */
				if (o_ptr->name2 == EGO_ASSASSIN) bbig = 16 + (level/10);
				
				/* pseudo-randarts ("of randomness" egos) */
				if (is_ego_randart(o_ptr)) bbig = 10 + (level/10);
					
				/* roll for ego chance of extra dice */
				if ((o_ptr->dd == k_ptr->dd) && (rand_int(100) < bbig))
				{
					o_ptr->dd++;
				}

                /* Hack -- Super-charge the damage dice */
				while ((o_ptr->dd * o_ptr->ds > 0) &&
				       (rand_int(10L * o_ptr->dd * o_ptr->ds) == 0))
				{
					o_ptr->dd++;
				}

				/* Hack -- Max damage dice */
				if (o_ptr->dd > 9) o_ptr->dd = 9;
			}

			break;
		}

		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
        case TV_FLASK: /* grenades */
		{
			/* Very good */
			if (power > 1)
			{
				/* Hack -- super-charge the damage dice */
				while ((o_ptr->dd * o_ptr->ds > 0) &&
				       (rand_int(10L * o_ptr->dd * o_ptr->ds) == 0))
				{
					o_ptr->dd++;
				}

				/* Hack -- restrict the damage dice */
				if (o_ptr->dd > 9) o_ptr->dd = 9;
			}

			break;
		}
		case TV_BOW:
		{
			/* accuracy should have higher to-hit than to-dam */
    	    if ((o_ptr->name2 == EGO_ACCURACY) && (o_ptr->to_d > o_ptr->to_h))
			{
				/* swap bonuses */
				s16b tmp = o_ptr->to_d;
				o_ptr->to_d = o_ptr->to_h;
				o_ptr->to_h = tmp;
			}
        
			/* of power should have higher to-dam than to-hit */
    	    if ((o_ptr->name2 == EGO_VELOCITY) && (o_ptr->to_h > o_ptr->to_d))
			{
				/* swap bonuses */
				s16b tmp = o_ptr->to_d;
				o_ptr->to_d = o_ptr->to_h;
				o_ptr->to_h = tmp;
			}
		}
	}
}


/*
 * Apply magic to an item known to be "armor"
 * (This is now also applied to main gauches)
 *
 * Hack -- note special processing for crown/helm
 * Hack -- note special processing for robe of permanence
 *
 * New: the KEEP_BONUS flag makes the ac bonuses
 * stay positive even if the item is cursed.
 */
static void a_m_aux_2(object_type *o_ptr, int level, int power, bool maing)
{
	int toac1 = randint(5) + m_bonus(5, level);
	int toac2 = m_bonus(10, level);

	/* Get flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (maing)
	{
		toac1 = randint(4) + m_bonus(4, level);
		toac2 = m_bonus(7, level);
	}


	/* Good */
	if ((power > 0) || (f4 & TR4_KEEP_BONUS))
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



/*
 * Apply magic to an item known to be a ring or amulet
 *
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
					break;
				}

				/* Alertness */
				case SV_RING_ALERTNESS:
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

				/* Forgetfulnes */
				case SV_RING_FORGETFULNESS:
				{
					/* Broken */
					o_ptr->ident |= (IDENT_BROKEN);

					/* Cursed */
					o_ptr->ident |= (IDENT_CURSED);

					/* Penalize */
					o_ptr->pval = 0 - (1 + m_bonus(5, level));

					break;
				}

				/* WOE */
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
					o_ptr->to_d = 5 + randint(3) + m_bonus(7, level);

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
					o_ptr->to_h = 5 + randint(3) + m_bonus(7, level);

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
					o_ptr->to_d = 1 + randint(4) + m_bonus(5, level);
					o_ptr->to_h = 1 + randint(4) + m_bonus(5, level);

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
   			    case SV_AMULET_ALERTNESS:
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
					break;
				}

				/* Amulet of Devotion -- never cursed */
				case SV_AMULET_DEVOTION:
				{
					o_ptr->pval = 1 + m_bonus(3, level);
					break;
				}

				/* Amulet of Weaponmastery -- never cursed */
				case SV_AMULET_WEAPONMASTERY:
				{
					o_ptr->to_h = 1 + m_bonus(4, level);
					o_ptr->to_d = 1 + m_bonus(4, level);
					o_ptr->pval = 1 + m_bonus(2, level);
					break;
				}

				/* Amulet of Trickery -- never cursed */
				case SV_AMULET_TRICKERY:
				{
					o_ptr->pval = randint(1) + m_bonus(3, level);
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
 * Apply magic to various other items:
 * lights, wands, staffs, rods & chests
 */
static void a_m_aux_4(object_type *o_ptr, int level, int power)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Unused parameters */
	(void)level;
	(void)power;

	/* Apply magic (good or bad) according to type */
	switch (o_ptr->tval)
	{
		case TV_LITE:
		{
			/* Hack -- Torches & lanterns -- random fuel at 1/2 max */
			if (o_ptr->sval == SV_LITE_TORCH)
			{
				/* torches shouldn't be generated with less than a minute of light */
				/* was o_ptr->timeout = randint(FUEL_TORCH/2);  (FUEL_TORCH is 5760 is 16 hrs) */
				/* amounts to similar range except never less than 60 */
                o_ptr->timeout = FUEL_TORCH/96 * randint(48); /* 5760/96 = 10 minutes, *48 = 8hours */
                /* rarely may find a bigger torch (was FUEL_TORCH/4 + randint(FUEL_TORCH/4); ) */
				if (randint(100) < (goodluck+5)/2) o_ptr->timeout += FUEL_TORCH/8 + (FUEL_TORCH/96 * randint(36));
				
				/* to prevent it saying (charging) in the description) */
                if (o_ptr->name2 == EGO_EVERBURNING) o_ptr->timeout = 0;
			}

			else if (o_ptr->sval == SV_LITE_LANTERN)
			{
				/* (FUEL_LAMP is 14400 is 40 hrs, /120 is 20 minutes, *60 is 20 hours) */
				/* was o_ptr->timeout = randint(FUEL_LAMP/2); */
				o_ptr->timeout = FUEL_LAMP/120 * randint(60);
				
				/* to prevent it saying (charging) in the description) */
                if (o_ptr->name2 == EGO_EVERBURNING) o_ptr->timeout = 0;
			}

			break;
		}
		case TV_FLASK:
		{
			/* fuel oil */
			if (o_ptr->sval == 0) o_ptr->timeout = FUEL_LAMP/2;
			break;
		}

		case TV_WAND:
		case TV_STAFF:
		{
			/* Charge staves and wands */
			o_ptr->charges = k_ptr->charge_base;

			if (k_ptr->charge_dd && k_ptr->charge_ds)
				o_ptr->charges += damroll(k_ptr->charge_dd, k_ptr->charge_ds);

			break;
		}

		case TV_ROD:
		{
			object_kind *k_ptr = &k_info[o_ptr->k_idx];

			/* Transfer the pval. */
			o_ptr->charges = k_ptr->pval;

			break;
		}

		case TV_CHEST:
		{
			/* Hack -- skip ruined chests */
			if (k_info[o_ptr->k_idx].level <= 0) break;

			/* Hack -- pick a "difficulty" */
			o_ptr->pval = randint(k_info[o_ptr->k_idx].level);

			/* Never exceed "difficulty" of 55 to 59 */
			if (o_ptr->pval > 55) o_ptr->pval = (s16b)(55 + rand_int(5));

			break;
		}
	}
}

/* return whether an item is one of the pseudo-randart egos */
bool is_ego_randart(const object_type *o_ptr)
{
	ego_item_type *e_ptr = &e_info[o_ptr->name2];
	switch (o_ptr->name2)
	{
		case EGO_RANDOM1:
		case EGO_RANDOM2:
		case EGO_RANDOM3:
		case EGO_RANDOM4:
		case EGO_BIG_RANDOM1:
		case EGO_BIG_RANDOM2:
		case EGO_RANDOM_AC1:
		case EGO_RANDOM_AC2:
		case EGO_RANDOM_AC3:
		case EGO_RANDOM_BOW:
		{
			/* check the name */
            /* to make sure someone hasn't messed with ego_item.txt */
			cptr ego_text = (e_name + e_ptr->name);
			if (strstr(ego_text, "andom"))
			{
				return TRUE;
			}
		}
	}
	
	/* else */ return FALSE;
}

/* convert e_ptr->esprace from an ego to put on an object 
 * see esprace in object type in types.h 
 */
byte convert_racialesp(byte egocode, const object_type *o_ptr)
{
	bool threefive = FALSE;
	bool rarer = FALSE;
	bool matchslay = FALSE;
	u32b f1, f2, f3, f4;
	byte useslay;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* no conversion needed */
	if ((egocode) && (egocode <= 13)) return egocode;
	if ((egocode == 16) || (egocode == 15)) return egocode;

	if (rand_int(100) < 35 + goodluck/3) threefive = TRUE;
	if (rand_int(100) < 10 + goodluck/3) rarer = TRUE;
	
	/* 10 = animals */
	if ((egocode == 30) && (threefive)) return 10;
	
	/* 35% chance of racial ESP */
    if (((egocode >= 31) && (egocode <= 39)) &&
		(threefive)) return (egocode - 30);
		
	if ((egocode == 46) && (threefive)) return 16;
		
	/* 10% chance of racial ESP */
    if (((egocode >= 51) && (egocode <= 59)) &&
		(rarer)) return (egocode - 50);
		
	/* random racial ESP */
    if ((egocode == 40) || ((egocode == 43) &&
		(threefive))) return (byte)randint(13);
		
	if ((egocode == 41) || ((egocode == 42) && 
		(threefive))) matchslay = TRUE;

    if ((matchslay) && (o_ptr->randslay))
	{
		int die = rand_int(99);
		if (!o_ptr->randslay3) die = rand_int(66);
		if (!o_ptr->randslay2) die = rand_int(33);
		
		if (die < 33) useslay = o_ptr->randslay;
		else if (die < 66) useslay = o_ptr->randslay2;
		else useslay = o_ptr->randslay3;
		
		if (useslay == 1) return 16; /* HURT_SILV */
		if (useslay == 2) return 4; /* undead */
		if (useslay == 3) return 6; /* demons */
		if (useslay == 4) return 1; /* orcs */
		if (useslay == 5) return 2; /* trolls */
		if (useslay == 6) return 3; /* giants */
		if (useslay == 7) return 5; /* dragons */
		if (useslay == 8) return 9; /* grepse */
		if (useslay == 9) return 8; /* bugs */
		if (useslay == 10) return 7; /* fairies */
	}
	else if (matchslay)
	{
		if (f1 & (TR1_SLAY_ORC)) return 1;
		if (f1 & (TR1_SLAY_TROLL)) return 2;
		if (f1 & (TR1_SLAY_GIANT)) return 3;
		if (f1 & (TR1_SLAY_BUG)) return 8;
		if (f2 & (TR2_SLAY_ANIMAL)) return 10;
		if (f1 & (TR1_SLAY_UNDEAD)) return 4;
		if (f1 & (TR1_SLAY_DRAGON)) return 5;
		if (f1 & (TR1_SLAY_DEMON)) return 6;
		if (f1 & (TR1_SLAY_SILVER)) return 9;
		if (f1 & (TR1_SLAY_WERE)) return 16;
		if (f1 & (TR1_SLAY_LITE)) return 7;
		if (f1 & (TR1_SLAY_EVIL))
		{
			useslay = (byte)randint(9);
			if (useslay == 7) return 9;
			if (useslay == 8) return 13; /* dark elves */
			if (useslay == 9) return 16;
			/* else */return useslay;
		}
		/* no slays to match */
		return (byte)randint(13);
	}
	
	/* no racial ESP */
    return 0;
}


/*
 * Complete the creation of an object by applying "magic" to the item
 *
 * This includes not only rolling for random bonuses, but also putting the
 * finishing touches on ego-items and artifacts, giving charges to wands and
 * staffs, giving fuel to lites, and placing traps on chests.
 *
 * In particular, note that "Instant Artifacts", if "created" by an external
 * routine, must pass through this function to complete the actual creation.
 *
 * The base chance of the item being "good" increases with the "level"
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
 *
 * I've added a uniqdrop global hack to make artifacts more likely to
 * appear in certain places:
 * UNIQUES >= dL23, gold chests, or 'T' or '8' symbols in vaults means
 *          uniqdrop == 3
 * shallow uniques or minor uniques (maxpop < 10), silver chests, or
 * other vault spaces means  uniqdrop == 2
 * other monster drops mean uniqdrop == 1
 * other non-dropped items uniqdrop == 0
 * (currently no difference between effects of uniqdrop 0 and 1)
 */
void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great)
{
	int i, rolls, g1, g2, power;
	int ogood = 0;
	/* allow forced cursed items in wizmode for testing */
	bool curseit = FALSE;

	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* forced cursed: used -2 because -1 is used for Morgoth's artifacts */
	if (lev == -2)
	{
		curseit = TRUE;
		lev = p_ptr->depth;
	}

	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

     /* increased odds */
	/* Base chance of being "good" */
	g1 = lev + 11 + goodluck/2;

	/* Maximal chance of being "good" */
	if (g1 > 80) g1 = 80;

	/* Base chance of being "great" */
	g2 = g1 / 2;

	/* Maximal chance of being "great" */
	if (g2 > 22) g2 = 22;

	/* anything less than an ego at depth > 75 is junk */
	if (p_ptr->depth >= 75) g2 += 2;

	/* certain objects more likely to be good */
	if (((f3 & TR3_THROWN) || (o_ptr->tval == TV_SKELETON)) && (lev > 14 - goodluck))
	{
		if ((f3 & TR3_THROWN) && (lev > 74)) g2 += lev/37;
		else if ((f3 & TR3_THROWN) || (lev > 74)) g2 += 1;
		if (lev < 69) g1 += 2;
		else g1 += lev/23;
	}
	
	/* uniques, vaults, and special chests are more likely */
	/* to have good stuff */
	if (uniqdrop == 3) good = TRUE;
	if ((uniqdrop == 2) && (randint(100) < 30+goodluck)) good = TRUE;

	/* Assume normal */
	power = 0;

	/* Roll for "good" */
	if (good || ((rand_int(100) < g1) && (!curseit)))
	{
		/* Assume "good" */
		power = 1;

		/* Roll for "great" */
		if (great || (rand_int(100) < g2)) power = 2;
	}

	/* Roll for "cursed" */
	else if ((rand_int(100) < g1) || (curseit))
	{
		/* Assume "cursed" */
		power = -1;

		/* Roll for "broken" (more likely if forced cursed) */
		if ((rand_int(100) < 33) && (curseit)) power = -2;
		if (rand_int(100) < g2) power = -2;
	}

	/* Assume no rolls */
	rolls = 0;

	/* Get one roll if excellent */
	if (power >= 2) rolls = 1;
	
	/* uniques more likely to drop artifacts */
	if (uniqdrop == 3) rolls += 1;
	if ((uniqdrop == 2) && (!rolls) && (randint(100) < 15 + goodluck))
		rolls = 1;

	/* Get four (or even five) rolls if forced great */
	if ((uniqdrop == 3) && (great) && (randint(100) < 12 + goodluck)) 
		rolls = 5;
	else if (great) rolls = 4;
	
	/* non-vault / nonuniques less likely */
	if ((uniqdrop < 2) && (rolls) && 
	    (randint(100) < 25 - goodluck/2)) rolls -= 1;

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
		/* artifact rating (each artifact has their own now) */
		int arat;

		/* Hack -- Mark the artifact as "created" */
		a_ptr->cur_num = 1;

		/* Extract the other fields */
		o_ptr->pval = a_ptr->pval;
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;
		o_ptr->sbdd = a_ptr->sbdd;
		o_ptr->sbds = a_ptr->sbds;
		o_ptr->to_a = a_ptr->to_a;
		o_ptr->to_h = a_ptr->to_h;
		o_ptr->to_d = a_ptr->to_d;
		o_ptr->weight = a_ptr->weight;
		o_ptr->esprace = a_ptr->esprace;

#ifdef EFGH /* (H: undoing a change Eddie made) */
		if (!obviously_excellent(o_ptr, FALSE, NULL))
		{
			/* would be better to do this at birth */
			a_ptr->flags3 |= TR3_LITE;
printf("adding lite to artifact %d\n", o_ptr->name1);
		}
#endif

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- extract the "cursed" flag */
		if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

        if (adult_randarts)
		{
			/* do ratings by cost only for randarts */
			if (a_ptr->cost > 250000L) arat = 30;
			else if (a_ptr->cost > 100000L) arat = 25;
			else if (a_ptr->cost > 50000L) arat = 20;
			else if (a_ptr->cost > 30000L) arat = 14;
			else arat = 10;
		}
		else 
		{
			/* artifact rating (each artifact has their own now) */
			arat = a_ptr->artrat;
		
			/* artifact rating modified by depth (sometimes) */
			if ((a_ptr->level < 40) && (p_ptr->depth > 50))
			    arat -= (p_ptr->depth - a_ptr->level) / 10;
			else if ((a_ptr->level >= 50) && (p_ptr->depth < 40))
			    arat += (a_ptr->level - p_ptr->depth) / 10;

			/* any artifact is very nice at low depths */
			if (p_ptr->depth < 13) arat += 3;
			else if ((p_ptr->depth < 25) && (p_ptr->depth < a_ptr->level)) arat += 2;
			else if (p_ptr->depth < 25) arat += 1;
			    
			if (arat < 1) arat = 1;
		}
		
		/* actually boost the rating */
		rating += arat;

		/* Set the good item flag */
		good_item_flag = TRUE;

		/* Cheat -- peek at the item */
		if (cheat_peek) object_mention(o_ptr);

#ifdef yes_c_history
		/* Hack - mark the depth of artifact creation for the notes function
		 * probably a bad idea to use this flag.  It is used when making ego-items,
		 * which currently fails when an item is an artifact.  If this was changed
		 * this would be the cause of some major bugs.
		 */
		if (p_ptr->depth)
		{
			o_ptr->randres3 = p_ptr->depth;
		}
#endif /* yes_c_history */

		/* Done */
		return;
	}

	/* deep cursed staffs just get annoying so they shouldn't be common */
	if ((power < 0) && (o_ptr->tval == TV_STAFF))
	{
		int min = 74 - goodluck*4;
		if (min < 38) min = 38;
		if ((randint(100) < 38 + (goodluck+1)/2 + lev/3) && (lev >= min))
			power = 0;
	}

	if (great) ogood = 2;
	else if (good) ogood = 1;
	else if (curseit) ogood = -1;

	/* Apply magic */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SKELETON:
		case TV_SWORD:
		case TV_BOW:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			if ((power > 1) || (power < -1))
			{
				int ego_power;

				ego_power = make_ego_item(o_ptr, ogood);

				if (ego_power) power = ego_power;
			}

			if (power) a_m_aux_1(o_ptr, lev, power);

			if ((power) && (o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_MAIN_GAUCHE))
				a_m_aux_2(o_ptr, lev, power, TRUE);

			break;
		}
		/* Magic staffs can get egos now (and can have pvals now) */
		case TV_STAFF:
		{
			/* ego staffs less common (was 80 before staves could have pvals) */
			int egoodd = 75;
			if (p_ptr->depth > 50) egoodd -= (4 + (p_ptr->depth-47)/3);

			a_m_aux_4(o_ptr, lev, power);

			if (((power > 1) || (power < -1)) && (randint(100) < egoodd))
			{
				int ego_power;

				ego_power = make_ego_item(o_ptr, ogood);

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
			/* DSMs have smaller chance for egos */
			if ((o_ptr->tval == TV_DRAG_ARMOR) && (randint(100) < 60)) /* skip egos */;
			else if ((power > 1) || (power < -1))
			{
				int ego_power;

				ego_power = make_ego_item(o_ptr, ogood);

				if (ego_power) power = ego_power;
			}
            
            if (power) a_m_aux_2(o_ptr, lev, power, FALSE);

            /* elven cloak pval shouldn't always be 2 */
            if ((o_ptr->tval == TV_CLOAK) && (o_ptr->sval == SV_ELVEN_CLOAK) && (power < 2) && (o_ptr->pval > 1)) o_ptr->pval = randint(2);

			break;
		}

		case TV_RING:
		case TV_AMULET:
		{
			/* ego jewelry less common (was 70 instead of 63) */
			if (((power > 1) || (power < -1)) && (randint(100) < 62))
			{
				int ego_power;

				ego_power = make_ego_item(o_ptr, ogood);

				if (ego_power) power = ego_power;
			}
			a_m_aux_3(o_ptr, lev, power);
			break;
		}

		case TV_LITE:
		{
			/* ego lights less common (was 100, then 80) */
			if (((power > 1) || (power < -1)) && (randint(100) < 74))
			{
				make_ego_item(o_ptr, ogood);
			}

			/* Fuel it */
			a_m_aux_4(o_ptr, lev, power);
			break;
		}

		case TV_POTION:
		{
			/* each potion of multi-hued poison choses another potion to mimmic */
			if (o_ptr->sval == SV_POTION_MULTIHUED_POISON)
			{
				/* its pval is the k_idx of the potion that it mimmics */
				/* occationally mimmic an especially tempting potion */
				if (rand_int(100) < badluck + 6) 
					o_ptr->pval = lookup_kind(75, 48 + rand_int(11));
				else o_ptr->pval = lookup_kind(75, 23 + rand_int(22));
				/* mult-hued poison is sval 13: bad things would probably */
				/* happen if it mimmics itself, but it doesn't. */
			}
			/* fall through */
		}

		default:
		{
			a_m_aux_4(o_ptr, lev, power);

			/* grenades */
			if ((power) && (o_ptr->tval == TV_FLASK) && (f3 & TR3_THROWN))
			{
				/* very rarely cursed */
				if ((power < 1) && (rand_int(100) < 68 + (goodluck*2))) 
					power = rand_int(2);

				/* ego grenades now possible */
				if (((power > 1) || (power < -1)) && (rand_int(100) < 80))
				{
					make_ego_item(o_ptr, ogood);
				}

				a_m_aux_1(o_ptr, lev, power);
			}
			break;
		}
	}
	
/* #if ifthis */
	/* chance for indestructible ego books */
	if ((o_ptr->tval >= TV_MAGIC_BOOK) && (o_ptr->tval < TV_GOLD))
	{
		/* ego books less common */
		if (((power > 1) || (power < -1)) && (randint(100) < 50))
		{
			make_ego_item(o_ptr, ogood);
		}
	}
/* #endif */

	/* Hack -- analyze ego-items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];
		bool escapedrb = FALSE;
		int xrandskill, xrandplu;

		if (e_ptr->randsus)
        {
			o_ptr->randsus = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_SUSTAIN);
			if ((e_ptr->randsus == 2) || ((e_ptr->randsus == 9) && (rand_int(100) < 50)))
				o_ptr->randsus2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_SUSTAIN);
		}
		if (e_ptr->randres)
        {
			int howmanywow = e_ptr->randres;
			if (e_ptr->randres == 9) howmanywow = randint(3);
            o_ptr->randres = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_RESIST);
			if (howmanywow > 1)
				o_ptr->randres2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_RESIST);
			if (howmanywow == 3)
				o_ptr->randres3 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_RESIST);
		}
		if (e_ptr->randlowr)
        {
			o_ptr->randlowr = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_LOWRES);
			if ((e_ptr->randlowr == 2) || ((e_ptr->randlowr == 9) && (rand_int(100) < 50)))
				o_ptr->randlowr2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_LOWRES);
		}
		/* randpow 11 sometimes has a power */
		if ((e_ptr->randpow == 11) && (rand_int(100) < 60)) /* skip */;
		else if (e_ptr->randpow)
        {
			o_ptr->randpow = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_POWER);
			if ((e_ptr->randpow == 2) || ((e_ptr->randpow == 9) && (rand_int(100) < 50)))
				o_ptr->randpow2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_POWER);
		}
		if (e_ptr->randslay)
        {
			int howmanywow = e_ptr->randslay;
			bool edgedw = FALSE;
			/* only edged weapons (or shovels which are metal) may get SLAY_WERE */
			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM) || 
				(o_ptr->tval == TV_DIGGING)) edgedw = TRUE;
			if (e_ptr->randslay == 9) howmanywow = randint(3);
			/* The old slays should still be more common than others */
			/* ~1/3 chance to restrict the 1st slay to orc/troll/giant/undead/demon/dragon */
            if (rand_int(100) < 33) 
				o_ptr->randslay = 2 + (byte)rand_int(6);
			/* only edged weapons can get SLAY_WERE (silver-bladed) */
			else if (edgedw)
				o_ptr->randslay = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY);
			else /* anything except SLAY_WERE (which is the 1st one) */
				o_ptr->randslay = 2 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY - 1);
			if (howmanywow > 1)
			{
				if (edgedw)
					o_ptr->randslay2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY);
				else /* anything except SLAY_WERE (which is the 1st one) */
					o_ptr->randslay2 = 2 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY - 1);
			}
			if (howmanywow == 3)
			{
				if (edgedw)
					o_ptr->randslay3 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY);
				else /* anything except SLAY_WERE (which is the 1st one) */
					o_ptr->randslay3 = 2 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY - 1);
			}
		}
		/* chance of switching to kill slay for pseudo-randarts */
		if (is_ego_randart(o_ptr))
		{
			if ((o_ptr->randslay == 2) && (randint(100) < 25 + (p_ptr->depth/10))) 
    	        o_ptr->randslay = 53;
			if ((o_ptr->randslay == 3) && (randint(100) < 25 + (p_ptr->depth/10))) 
        	    o_ptr->randslay = 52;
			if ((o_ptr->randslay == 7) && (randint(100) < 25 + (p_ptr->depth/10))) 
            	o_ptr->randslay = 51;
		}
		/* An object should never have both SLAY_SILVER and SLAY_WERE */
		/* because weapons which SLAY_WERE ARE silver */
		if (((o_ptr->randslay == 1) && (o_ptr->randslay2 == 8)) ||
			((o_ptr->randslay == 8) && (o_ptr->randslay2 == 1)))
		{
			if (o_ptr->randslay2 == 1)
			{
				while (TRUE)
				{
					o_ptr->randslay2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY);
					if (!(o_ptr->randslay2 == 1)) break;
				}
			}
			else if (o_ptr->randslay2 == 8)
			{
				while (TRUE)
				{
					o_ptr->randslay2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_SLAY);
					if (!(o_ptr->randslay2 == 8)) break;
				}
			}
		}
		/* randplu = 18 means 1d2 random stat OR skill bonuses */
		xrandskill = 0; xrandplu = 0;
		if (e_ptr->randplu == 18)
		{
			if (rand_int(100) < 50) xrandskill = 1;
			else xrandplu = 1;
			if (rand_int(100) < 71 + goodluck)
			{
				if (rand_int(100) < 50) xrandskill += 1;
				else xrandplu += 1;
			}
		}
		/* randbon == 11 means sometimes has a random skill bonus (and rarely has 2) */
		if ((e_ptr->randbon == 11) && (randint(100) < 50)) /* skip */;
		else if ((e_ptr->randbon) || (xrandskill))
        {
			int blowchance = 3;
			/* EGO_RANDOM 1-4 not supposed to be too powerful (and 3 already has blows) */
			if ((o_ptr->name2 >= EGO_RANDOM1) && (o_ptr->name2 <= EGO_RANDOM4)) blowchance = 0;
			/* no blows if we have a random brand or an unusually high ego-pval */
			else if ((e_ptr->max_pval > 4) || (e_ptr->randbran)) blowchance = 0;
			/* for the stonger ego randart weapons */
			else if ((wield_slot(o_ptr) == INVEN_WIELD) && (is_ego_randart(o_ptr))) blowchance = 7;
			/* no extra blows on non-weapons */
			else if (!(wield_slot(o_ptr) == INVEN_WIELD)) blowchance = 0;
			/* chance of extra blows as a random skill bonus */
			/* (pval is capped at 2 later if it gets blows) */
			if (rand_int(100) < blowchance) o_ptr->randbon = 7; /* blows */

			else o_ptr->randbon = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_BONUS);

			if ((e_ptr->randbon == 2) || ((e_ptr->randbon == 9) && (rand_int(100) < 50)) ||
				(xrandskill == 2))
			{
				o_ptr->randbon2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_BONUS);

				/* sometimes reroll */
				if (((xrandskill == 2) && ((o_ptr->randbon == 2) && (o_ptr->randbon2 == 6)) ||
					((o_ptr->randbon == 6) && (o_ptr->randbon2 == 2))) ||
					(o_ptr->randbon == o_ptr->randbon2))
						o_ptr->randbon2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_BONUS);
			}
		}
		if (e_ptr->randbran) /* never more than one random brand */
        {
			o_ptr->randbran = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_BRAND);
		}
		if ((e_ptr->randplu == 18) && (!xrandplu)) /* skip */;
		else if (e_ptr->randplu)
        {
			o_ptr->randplu = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_PLUS);

			if ((e_ptr->randplu == 2) || ((e_ptr->randplu == 9) && (rand_int(100) < 50)) ||
				(xrandplu == 2)) o_ptr->randplu2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_PLUS);
		}
		/* randdrb == 11 means often have a drawback */
		/* randdrb == 12 means rarely have a drawback */
		if ((e_ptr->randdrb == 11) || (e_ptr->randdrb == 12))
		{
			int drbchance = 46 + goodluck/2;
			if (e_ptr->randdrb == 12)
			{
				drbchance = 88 + goodluck/3;
				if (o_ptr->pval > 0) drbchance -= o_ptr->pval * 6;
			}
			else /* 11 */
			{
				if (o_ptr->pval > 0) drbchance -= o_ptr->pval * 3;
			}
			if (rand_int(100) < drbchance) escapedrb = TRUE;
		}
		if ((e_ptr->randdrb) && (!escapedrb))
        {
			int howmanywow = e_ptr->randdrb;
			if (e_ptr->randdrb == 9) howmanywow = randint(2);
			if ((e_ptr->randdrb == 11) || (e_ptr->randdrb == 12)) howmanywow = 1;
            o_ptr->randdrb = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_DRAWBACK);
			if (howmanywow > 1)
				o_ptr->randdrb2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_DRAWBACK);
				
			/* may only have a random immunity if it also has drawbacks */
			if (e_ptr->randdrb == 3)
			{
				o_ptr->randimm = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_IMMU);
			/* make sure we have two different drawbacks if there's an immunity */
				if (o_ptr->randdrb == o_ptr->randdrb2)
				{
					while (TRUE)
					{
						o_ptr->randdrb2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_DRAWBACK);
						if (!(o_ptr->randdrb == o_ptr->randdrb2)) break;
					}
				}
			}
			/* if it has SLAY_LITE or two random slays, it shouldn't have PEACE */
			if ((o_ptr->randslay == 10) || (o_ptr->randslay2))
			{
				if (o_ptr->randdrb == 4) /* PEACE is drb #4 */
				{
					while (TRUE)
					{
						o_ptr->randdrb = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_DRAWBACK);
						if (!(o_ptr->randdrb == 4)) break;
					}
				}
				if (o_ptr->randdrb2 == 4)
				{
					while (TRUE)
					{
						o_ptr->randdrb2 = 1 + (byte)rand_int(OBJECT_XTRA_SIZE_DRAWBACK);
						if (!(o_ptr->randdrb2 == 4)) break;
						/* aggravation slightly more common on items with immunities */
						if (o_ptr->randimm) { o_ptr->randdrb2 = 7; break; }
					}
				}
			}
		}
		/* must be done after o_ptr->randslay is assigned */
		if (e_ptr->esprace)
		{
			o_ptr->esprace = convert_racialesp(e_ptr->esprace, o_ptr);
		}

		/* Hack -- acquire "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (e_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

#if staffofslowness
		/* Constant activation on staff of slowness should be cursed (that staff was removed) */
		if ((o_ptr->tval == TV_STAFF) && (e_ptr->flags2 & (TR2_CONSTANTA)) && (o_ptr->sval == SV_STAFF_SLOWNESS))
		{
			o_ptr->ident |= (IDENT_CURSED);
		}
#endif

		/* Hack -- apply extra penalties if needed */
		if (cursed_p(o_ptr))
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->flags4 & (TR4_KEEP_BONUS))
			{
				if (e_ptr->max_to_h > 7) o_ptr->to_h += 4 + randint(e_ptr->max_to_h - 4);
				else if (e_ptr->max_to_h > 0) o_ptr->to_h += randint(e_ptr->max_to_h);
				else if (e_ptr->max_to_h < 0) o_ptr->to_h = 0 - randint(ABS(e_ptr->max_to_h));
				if (e_ptr->max_to_d > 7) o_ptr->to_d += 4 + randint(e_ptr->max_to_d - 4);
				else if (e_ptr->max_to_d > 0) o_ptr->to_d += randint(e_ptr->max_to_d);
				else if (e_ptr->max_to_d < 0) o_ptr->to_d = 0 - randint(ABS(e_ptr->max_to_d));
				if (e_ptr->max_to_a > 7) o_ptr->to_a += 4 + randint(e_ptr->max_to_a - 4);
				else if (e_ptr->max_to_a > 0) o_ptr->to_a += randint(e_ptr->max_to_a);
				else if (e_ptr->max_to_a < 0) o_ptr->to_a = 0 - randint(ABS(e_ptr->max_to_a));
			}
			else
			{
				if (e_ptr->max_to_h > 0) o_ptr->to_h -= randint(e_ptr->max_to_h);
				if (e_ptr->max_to_d > 0) o_ptr->to_d -= randint(e_ptr->max_to_d);
				if (e_ptr->max_to_a > 0) o_ptr->to_a -= randint(e_ptr->max_to_a);
			}

			/* Hack -- obtain pval */
			if (e_ptr->max_pval > 0) o_ptr->pval = 0 - randint(e_ptr->max_pval);
			/* double negative */
			else if (e_ptr->max_pval < -5) o_ptr->pval += 2 + m_bonus(ABS(e_ptr->max_pval)-2, MAX(lev, 5));
			else if (e_ptr->max_pval < 0) o_ptr->pval += randint(ABS(e_ptr->max_pval));
		}

		/* Hack -- apply extra bonuses if needed */
		else
		{
			/* Hack -- obtain bonuses */
			if (e_ptr->max_to_h > 7) o_ptr->to_h += 4 + randint(e_ptr->max_to_h - 4);
			else if (e_ptr->max_to_h > 0) o_ptr->to_h += randint(e_ptr->max_to_h);
			else if (e_ptr->max_to_h < 0) o_ptr->to_h = 0 - randint(ABS(e_ptr->max_to_h));
			if (e_ptr->max_to_d > 7) o_ptr->to_d += 4 + randint(e_ptr->max_to_d - 4);
			else if (e_ptr->max_to_d > 0) o_ptr->to_d += randint(e_ptr->max_to_d);
			else if (e_ptr->max_to_d < 0) o_ptr->to_d = 0 - randint(ABS(e_ptr->max_to_d));
			if (e_ptr->max_to_a > 7) o_ptr->to_a += 4 + randint(e_ptr->max_to_a - 4);
			else if (e_ptr->max_to_a > 0) o_ptr->to_a += randint(e_ptr->max_to_a);
			else if (e_ptr->max_to_a < 0) o_ptr->to_a = 0 - randint(ABS(e_ptr->max_to_a));

			/* Hack -- obtain pval  (note: m_bonus can return 0) */
			/* (boots of speed are currently the only ego with max_pval > 5) */
			if (e_ptr->max_pval > 5) o_ptr->pval += 2 + m_bonus(e_ptr->max_pval-2, MAX(lev, 5));
			else if (e_ptr->max_pval > 0) o_ptr->pval += randint(e_ptr->max_pval);
			else if (e_ptr->max_pval < 0) o_ptr->pval = 0 - randint(ABS(e_ptr->max_pval));

			/* never allow more than two extra blows as a random skill bonus */
			if ((o_ptr->randbon == 7) && (o_ptr->pval > 2)) o_ptr->pval = 2;

			/* check luck bonus on amulets of luck */
			if ((o_ptr->name2 == EGO_LOOKLUCKY) && (o_ptr->pval < 3))
			{
				if (randint(100) < 5) o_ptr->pval = 2 + randint(5);
				else o_ptr->pval = 2 + randint(3);
			}

			/* Hack: prevent insane stealth bonuses */
			/* on armor that already has extra stealth */
            if (((o_ptr->tval == TV_HELM) && (o_ptr->sval == SV_ELVEN_LEATHER_CAP)) ||
			   ((o_ptr->tval == TV_CLOAK) && (o_ptr->sval == SV_ELVEN_CLOAK)))
            {
                if ((o_ptr->pval > 3) && (randint(100) > goodluck + 4)) o_ptr->pval = randint(3);
                else if (o_ptr->pval > 4) o_ptr->pval = randint(3) + 2;

                /* don't reduce pval too much for elven cloaks */
                /* which have a pval of 2 without an ego */
                if ((o_ptr->tval == TV_CLOAK) && (o_ptr->pval < 2)) o_ptr->pval = 2;
            }

			/* Hack for gauntlets of throwing */
			if ((e_ptr->flags3 & (TR3_THROWMULT)) && (o_ptr->tval == TV_GLOVES))
			{
				/* minimum p_ptr->throwmult is 2 */
				if ((o_ptr->pval == 1) && (randint(100) < 35)) o_ptr->pval += 2;
				else if (o_ptr->pval == 1) o_ptr->pval += 1;
				/* pval of 4 should be very rare */
				if ((o_ptr->pval == 4) && (randint(100) < 75)) o_ptr->pval -= randint(2);
			}
			/* other egos which happened to get the THROWMULT flag (by random power only) */
			else if (e_ptr->flags3 & (TR3_THROWMULT))
			{
				/* pval of 4 should be very rare */
				if ((o_ptr->pval >= 4) && (randint(100) < 70)) o_ptr->pval -= randint(2);
				/* pval of 1 should be allowed, but uncommon (minimum p_ptr->throwmult is 2 anyway) */
				if ((o_ptr->pval == 1) && (randint(100) < 55)) o_ptr->pval += randint(2);
				/* a lot of egos with random powers usually have no pval */
				if (!o_ptr->pval) o_ptr->pval = 1 + randint(2);
			}
			/* I found an ego with throwmult as a random power, but it had no pval */
			/* so the above code doesn't do what I thought it would do. Let's try this: */
			/* if ((!o_ptr->pval) && (o_ptr->xtra2 == TR3_THROWMULT)) o_ptr->pval = 1 + randint(2); */
			/* ..that doesn't work either */
			/* (get a compiler message saying comparison is always false due to limited range of data type) */
			/* how about this: */
	        /* update flags to get random power flag (hopefully..) */
            object_flags(o_ptr, &f1, &f2, &f3, &f4);
			if ((!o_ptr->pval) && (f3 & TR3_THROWMULT))
			{
				if (randint(100) < 20 + goodluck) o_ptr->pval = 1 + randint(3);
				else o_ptr->pval = 1 + randint(2);
			}
			
			/* Magic Mastery (now uses pval so make sure it has one) */
			if (e_ptr->flags1 & (TR1_MAGIC_MASTERY))
			{
				if (!o_ptr->pval) o_ptr->pval = randint(3);
			}

			/* chance for double ego 'of lightness' on heavy armor */
			/* (because otherwise a full plate mail of resist cold is worthless) */
			if ((o_ptr->tval == TV_HARD_ARMOR) && (rand_int(100) < 2 + (goodluck+1)/3))
			{
				if (!((o_ptr->name2 == EGO_LIGHTNESS) || (o_ptr->name2 == EGO_ARMR_DWARVEN)))
				{
					/* 10lb weight reduction */
					o_ptr->weight -= 100;
					if (o_ptr->to_h < -1) o_ptr->to_h += 1;
				}
			}
		}

		/* Eregion should never have a negative speed bonus (even when cursed) */
		/* (no rings which otherwise get a pval should be able to have eregion ego) */
		if (((o_ptr->name2 == EGO_EREGION1) || (o_ptr->name2 == EGO_EREGION2)) &&
			(o_ptr->pval < 0))
		{
			o_ptr->pval = randint(2);
		}

        /* Hack for ego "of lightness" armor */
        if (e_ptr->flags2 & (TR2_LIGHTNESS))
		{
			/* make sure we don't make something weightless */
            if (o_ptr->weight - e_ptr->weight >= 40) o_ptr->weight -= e_ptr->weight;
			else if (o_ptr->weight > 40) o_ptr->weight = 40;
			/* armor of lightness also reduces to-hit penalty */
			if (o_ptr->to_h < 0) o_ptr->to_h += 1;
		}
		else if (e_ptr->weight > 0)
		{
			o_ptr->weight += e_ptr->weight;
		}

		/* DSM basic 4 ego of IMMUNITY */
		if (o_ptr->name2 == EGO_IMMUNITY)
		{
			/* o_ptr->randimm not random in this case */
			if (o_ptr->sval == SV_DRAGON_BLACK) o_ptr->randimm = 1;
			else if (o_ptr->sval == SV_DRAGON_BLUE) o_ptr->randimm = 2;
			else if (o_ptr->sval == SV_DRAGON_WHITE) o_ptr->randimm = 4;
			else if (o_ptr->sval == SV_DRAGON_RED) o_ptr->randimm = 3;
			else if (o_ptr->sval == SV_DRAGON_MULTIHUED) o_ptr->randimm = randint(4);
			/* this ego shouldn't be on anything else */
			else o_ptr->name2 = 0;
		}

		/* copied from randart.c: give the psuedo-randart a name */
		if (is_ego_randart(o_ptr))
		{
			char buf[40];
			char word[11];
			if (randint(100) < 35) randname_make(RANDNAME_TOLKIEN, 4, 10, word, sizeof word);
			else randname_make(RANDNAME_SILLY, 4, 10, word, sizeof word);
			/* I have no idea what this line is for */
			word[0] = toupper((unsigned char) word[0]);

			if (rand_int(3) == 0)
				strnfmt(buf, sizeof(buf), "'%s'", word);
			else
				strnfmt(buf, sizeof(buf), "of %s", word);

			strnfmt(o_ptr->randego_name, sizeof(o_ptr->randego_name), buf);
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

		/* not broken even if kind is worthless */
		if ((o_ptr->to_h + o_ptr->to_d > 3) || (o_ptr->to_a > 3)) /* skip */;
        /* Hack -- acquire "broken" flag */
		else if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

		/* minimum pval is 2 (for non-ego stat rings and amulets) */
		if (((o_ptr->tval == TV_RING) && ((o_ptr->sval >= SV_RING_STR) &&
			(o_ptr->sval <= SV_RING_CON))) || ((o_ptr->tval == TV_AMULET) &&
			((o_ptr->sval == SV_AMULET_WISDOM) || (o_ptr->sval == SV_AMULET_CHARISMA))))
		{
			if ((o_ptr->pval >= 0) && (o_ptr->pval <= 1) && 
				(p_ptr->depth > 14 + badluck - goodluck/3))
			{
				o_ptr->pval += randint(2);
			}
		}

		/* (+3 +3) =slaying is junk (especially with ego rings of warfare) */
		if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_SLAYING) &&
			(lev > 40 + (badluck*2)))
		{
			if (o_ptr->to_d + o_ptr->to_h < 8)
			{
				o_ptr->to_d += randint(3);
				o_ptr->to_h += randint(3);
			}
			/* maybe a little bit more */
			if ((o_ptr->to_d + o_ptr->to_h < 9 + (goodluck+1)/3) && 
				(goodluck > 2) && (randint(100) < (lev+9)/10 + goodluck*2))
			{
				o_ptr->to_d += randint(2);
				o_ptr->to_h += randint(3);
			}
		}
		/* Eregion ego has +1-2 speed, so =speed should be at least +3 */
		if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_SPEED) &&
			(lev > 45 + badluck))
		{
			/* min is 7 with maximum goodluck (which is very rare) */
			int min = 2 + ((goodluck+2)/4);
			if ((o_ptr->pval >= 0) && (o_ptr->pval <= min))
				o_ptr->pval = min + randint(4 + (goodluck+1)/3);
		}
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

		/* Ammo is good */
		case TV_BOLT:
		case TV_SHOT:
		case TV_ARROW:
		{
			return (TRUE);
		}

		/* Books -- High level books are good (if it's the correct realm) */
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_NEWM_BOOK:
		case TV_LUCK_BOOK:
		case TV_CHEM_BOOK:
		case TV_DARK_BOOK:
		/* case TV_MIND_BOOK: */
		{
			if (k_ptr->tval == cp_ptr->spell_book)
			{
				if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return (TRUE);
				/* book 4 considered good in ironman games */
				if ((adult_ironman) && (k_ptr->sval >= SV_BOOK_MIN_GOOD-1)) return (TRUE);
            }
			return (FALSE);
		}

		/* Rings -- Rings of Speed are good */
		case TV_RING:
		{
			if (k_ptr->sval == SV_RING_SPEED) return (TRUE);
			return (FALSE);
		}

		/* Amulets */
		case TV_AMULET:
		{
			if (k_ptr->sval == SV_AMULET_THE_MAGI) return (TRUE);
			if (k_ptr->sval == SV_AMULET_DEVOTION) return (TRUE);
			if (k_ptr->sval == SV_AMULET_WEAPONMASTERY) return (TRUE);
			if (k_ptr->sval == SV_AMULET_TRICKERY) return (TRUE);
			return (FALSE);
		}
		
		/* Potions -- Potions of life, healing, *healing* are good,
		 * restore mana for spell casters are good,
		 * as are stat potions, when the stat is not maximised,
		 * as is augmentation (acts as a potion of 'restoration' if all
		 * stats are maximised).
		 * 
		 * XXX If we make too many useful items 'good' we may want to
		 * consider limiting the total number of good drops to uniques
		 * and truely nasty monsters.
		 */
		case TV_POTION:
		{
			if (k_ptr->sval == SV_POTION_HEALING) return (TRUE);
			if (k_ptr->sval == SV_POTION_STAR_HEALING) return (TRUE);
			if (k_ptr->sval == SV_POTION_LIFE) return (TRUE);
			if ((k_ptr->sval == SV_POTION_RESTORE_MANA) && (p_ptr->msp > 34)) return (TRUE);
			if ((k_ptr->sval >= SV_POTION_INC_STR) && (k_ptr->sval <= SV_POTION_INC_CHR))
			{
				if (p_ptr->stat_cur[k_ptr->sval - SV_POTION_INC_STR] < 18+100) return (TRUE);
			}
			if (k_ptr->sval == SV_POTION_AUGMENTATION) return (TRUE);
			/* !Super spellcasting is good for full casters */
			if ((k_ptr->sval == SV_POTION_AUTO_BRAIL) && (cp_ptr->flags & CF_ZERO_FAIL)) return (TRUE);
			/* Purity counts as good only if you need it */
			if (k_ptr->sval == SV_POTION_PURITY)
			{
				if ((p_ptr->silver >= PY_SILVER_LEVELONE) || (p_ptr->slime >= PY_SLIME_LEVELONE) ||
					(p_ptr->corrupt > 8))
					return (TRUE);
			}
			return (FALSE);
		}

		/* certain rods should be considered good */
		/* some dependant on device skill */
		case TV_ROD:
		{
			if (k_ptr->sval == SV_ROD_SPEED) return (TRUE);
			if (k_ptr->sval == SV_ROD_DETECTION) return (TRUE);
			if (k_ptr->sval == SV_ROD_TELEPORT_AWAY) return (TRUE);
			if (p_ptr->skills[SKILL_DEV] > 75)
			{
				if (k_ptr->sval == SV_ROD_DRAIN_LIFE) return (TRUE);
				if (k_ptr->sval == SV_ROD_FIRE_BALL) return (TRUE);
				if (k_ptr->sval == SV_ROD_RIFT) return (TRUE);
				if (k_ptr->sval == SV_ROD_HEALING) return (TRUE);
			}
			return (FALSE);
		}
	}

	/* Assume not good */
	return (FALSE);
}

/* 
 * increase the level's treasure rating based on the object created
 * (applies to base objects only- ego and artifact ratings are done elsewhere.)
 * original boosts from V3.0.9:
 * any uncursed speed ring +25
 * any dragon scale mail +30
 * certain amulets (devotion, trickery, weaponmastery, magi) +25
 */
bool do_rating(object_type *j_ptr, bool tracking)
{
	int nlev = k_info[j_ptr->k_idx].level;
	bool notrack = FALSE;
	int rboost = 0;
		
    /* cursed or broken non-ego, non-artifacts never raise the rating */
    if ((cursed_p(j_ptr)) || (broken_p(j_ptr))) return FALSE;
	
	/* Analyze type */
	switch (j_ptr->tval)
	{
		case TV_RING:
		{
			switch (j_ptr->sval)
			{
				case SV_RING_SPEED:
				{
					rboost = 15 + (j_ptr->pval * 2);
					if (rboost > 31) rboost = 31;
					break;
				}
				case SV_RING_LIGHTNING:
				case SV_RING_ACID:
				{
					rboost = 6;
					break;
				}
				case SV_RING_FLAMES:
				case SV_RING_ICE:
				{
					rboost = 4;
					break;
				}
				case SV_RING_STR:
				case SV_RING_CON:
				{
                    if (j_ptr->pval > 1) rboost = j_ptr->pval-1;
					break;
				}
			}
			break;
        }
		case TV_AMULET:
		{
			switch (j_ptr->sval)
			{
				case SV_AMULET_SUSTENANCE:
				{
					rboost = 5;
					notrack = TRUE;
					break;
				}
				case SV_AMULET_THE_MAGI:
				case SV_AMULET_ESP:
				case SV_AMULET_DEVOTION:
				{
					rboost = 8;
                    if (j_ptr->pval > 1) rboost += j_ptr->pval-1;
					break;
				}
				case SV_AMULET_WEAPONMASTERY:
				case SV_AMULET_TRICKERY:
				{
					rboost = 15 + j_ptr->pval;
					break;
				}
			}
			break;
        }
		case TV_ROD:
		{
			switch (j_ptr->sval)
			{
				case SV_ROD_DETECTION:
				case SV_ROD_TELEPORT_AWAY:
				case SV_ROD_RIFT:
				case SV_ROD_SPEED:
				{
					if (p_ptr->depth < nlev) rboost = 3;
					else rboost = 2;
					break;
				}
			}
			break;
		}
		case TV_SCROLL:
		{
			if (j_ptr->sval == SV_SCROLL_ACQUIREMENT) rboost = 2;
			if (j_ptr->sval == SV_SCROLL_STAR_ACQUIREMENT) rboost = 5;
			break;
        }
		case TV_POTION:
		{
			switch (j_ptr->sval)
			{
                case SV_POTION_INC_WIS:
				{
					if (cp_ptr->spell_stat == 2) rboost = 2;
					break;
				}
                case SV_POTION_INC_INT:
				{
					if (cp_ptr->spell_stat == 1) rboost = 2;
					break;
				}
                case SV_POTION_INC_DEX:
                case SV_POTION_HEALING:
				{
					rboost = 1;
					break;
				}
                case SV_POTION_FOUR_LEAF: /* this should make you feel lucky.. */
                case SV_POTION_STAR_HEALING:
                case SV_POTION_LIFE: /* not much difference between *healing* and life */
                case SV_POTION_INC_CON:
                case SV_POTION_INC_STR:
                case SV_POTION_STAR_ENLIGHTENMENT:
				{
					rboost = 3;
					break;
				}
                case SV_POTION_AUGMENTATION:
				{
					rboost = 5;
					break;
				}
			}
			break;
        }
#if 0
		/* these only appear in vaults, so the vault rating is enough */
        case TV_CHEST:
		{
			switch (j_ptr->sval)
			{
                case SV_SP_SILVER_CHEST: /* DROP_GOOD chest */
				{
					rboost = 5;
					break;
				}
                case SV_SP_GOLD_CHEST: /* DROP_GREAT chest */
				{
					rboost = 10;
					break;
				}
			}
			break;
        }
#endif
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
            if ((j_ptr->sval == SV_AMMO_HEAVY) || 
               (j_ptr->sval == SV_AMMO_SILVER)) 
                rboost = 2;
			break;
        }
		case TV_SKELETON:
		{
			switch (j_ptr->sval)
			{
				/* ego-brand weapons get a significant rating boost so the ones*/
				/* which are intrinsticly branded should also boost the rating */
				/* (weakest ego-brands have rating boost of 15, so this is low) */
                case SV_VASP_STING:
                case SV_FIRE_TOOTH:
				{
					rboost = 6;
					notrack = TRUE;
					break;
				}
				case SV_WYVERN_STING:
				{
					rboost = 9;
					notrack = TRUE;
					break;
				}
			}
			/* fall through to other weapons */
        }
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOW:
		{
			/* ego item boost covers the combat bonuses */
			if (!ego_item_p(j_ptr))
			{
				if (j_ptr->to_d + j_ptr->to_h >= 10)
				    rboost += (j_ptr->to_d + j_ptr->to_h) / 9; /* usually +1 */
				break;
			}
			break;
		}
		case TV_HELM:
		{
			/* stealth cloak ego is worth rating boost of 10, stealth boots 12 */
			if (j_ptr->sval == SV_ELVEN_LEATHER_CAP) rboost = 4;
			notrack = TRUE;
			break;
        }
		case TV_CLOAK:
		{
			/* Stealth */
            if (j_ptr->sval == SV_ELVEN_CLOAK) rboost = 4;
			/* Rcold */
            if (j_ptr->sval == SV_FUR_CLOAK) rboost = 1;
			/* weighs nothing & ignores elements */
            if (j_ptr->sval == SV_ETHEREAL_CLOAK) rboost = 1;
			notrack = TRUE;
			break;
        }
        /* DSMs are certainly not all worth a rating of 30 */
		case TV_DRAG_ARMOR:
		{
			switch (j_ptr->sval)
			{
                case SV_DRAGON_GOLD:
				{
					rboost = 12;
					break;
				}
                case SV_DRAGON_BRONZE:
				{
					rboost = 16;
					break;
				}
                case SV_DRAGON_MULTIHUED:
				{
					rboost = 20;
					break;
				}
                case SV_DRAGON_LAW: /* violet: has speed & Rnexus */
				{
					rboost = 27;
					break;
				}
                case SV_DRAGON_CHAOS:
                case SV_DRAGON_ETHEREAL:
				{
					rboost = 30;
					break;
				}
                case SV_DRAGON_BALANCE:
				{
					rboost = 34;
					break;
				}
                case SV_DRAGON_POWER:
				{
					rboost = 37;
					break;
				}
				default: /* (the lesser DSMs, was 8) */
				{
					rboost = 9;
					break;
				}
			}
			break;
		}
	}

	/* dungeon spellbooks */
	if ((j_ptr->tval == cp_ptr->spell_book) && (j_ptr->sval >= SV_BOOK_MIN_GOOD))
		rboost = j_ptr->sval - SV_BOOK_MIN_GOOD + 1;

	/* tracking means this function was only called to decide whether to track the item */
	if ((!notrack) && (rboost >= 4) && (tracking)) return TRUE;
	else if (tracking) return FALSE;

	/* object has gotten boring at this depth */
	/* (rating gets to a certain level before it starts penalizing for this) */
    if ((p_ptr->depth > nlev + 15) && (((rating > 9) && (randint(100) < 40)) || (rating > 20)))
		rboost -= (p_ptr->depth - nlev) / 15;
    /* make sure we don't lower the rating */
    if (rboost < 0) rboost = 0;
    
	/* Mention the item (no longer mentions lesser DSMs) */
    if ((rboost >= 10) && (cheat_peek)) object_mention(j_ptr);
	
	/* actually boost the rating */
	rating += rboost;

	if (rboost > 0) return TRUE;
	else return FALSE;
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
 * uniq = 0 if not dropped by a monster
 * uniq = 1 if dropped by a non-unique
 * uniq = 2 if dropped by a minor unique
 * uniq = 3 if dropped by a unique
 */
bool make_object(object_type *j_ptr, bool good, bool great)
{
	int prob, base;
	object_kind *k_ptr;
	u32b f1, f2, f3, f4;
	bool goodcheck = FALSE;

	/* Chance of "special object" (used only for the special artifacts) */
	prob = (good ? 10 : 1000);
	if ((uniqdrop > 1) && (prob == 1000)) prob -= uniqdrop * 10;

	/* Base level for the object */
	base = (good ? (object_level + 10) : object_level);


	/* Generate a special artifact, or a normal object */
	if ((rand_int(prob) != 0) || !make_artifact_special(j_ptr))
	{
		int k_idx;

		/* Good objects */
		/* kind restriction doesn't always apply (still raises obj level) */
		if ((good) && (randint(100) < 80))
		{
			/* Activate restriction */
			get_obj_num_hook = kind_is_good;

			/* Prepare allocation table */
			get_obj_num_prep();

			goodcheck = TRUE;
		}

		/* Pick a random object */
		k_idx = get_obj_num(base);

		/* Good objects */
		if (goodcheck)
		{
			/* Clear restriction */
			get_obj_num_hook = NULL;

			/* Prepare allocation table */
			get_obj_num_prep();
		}

		/* Handle failure */
		if (!k_idx) return (FALSE);

		/* get k_ptr early to check value */
		k_ptr = &k_info[k_idx];

		/* kind restriction no longer applies but worthless objects are never good */
		/* so reroll a restricted good kind */
		if (((good) || (great)) && (!k_ptr->cost))
		{
			/* Activate restriction */
			get_obj_num_hook = kind_is_good;

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
		}

		/* Prepare the object */
		object_prep(j_ptr, k_idx);
	}

	/* Apply magic (allow artifacts) */
	apply_magic(j_ptr, object_level, TRUE, good, great);
	
	/* Remove broken flag from staffs with good egos */
	/* even if their object kind is normally broken. */
    if ((broken_p(j_ptr)) && (j_ptr->tval == TV_STAFF))
    {
		if (j_ptr->name2)
		{
           ego_item_type *e_ptr = &e_info[j_ptr->name2];

		   /* not broken */
		   if (e_ptr->cost) j_ptr->ident &= ~(IDENT_BROKEN);
        }
        /* also not broken if it has good to-hit and to-dam bonuses */
        else if ((j_ptr->to_h > 2) && (j_ptr->to_d > 2))
        {
           j_ptr->ident &= ~(IDENT_BROKEN);
        }
    }

	/* Extract the flags (check for THROWN & EXPLODE_A flags) */
	object_flags(j_ptr, &f1, &f2, &f3, &f4);

	/* Generate multiple items */
	/* Imported from Steamband and Sangband */
	k_ptr = &k_info[j_ptr->k_idx];
	
	if (k_ptr->gen_mult_prob >= 100 ||
	    k_ptr->gen_mult_prob >= randint(100))
	{
		j_ptr->number = damroll(k_ptr->gen_dice, k_ptr->gen_side);
		/* exploding ammo always dissapears when it explodes, so enlarge the */
		/* stacks a little. (most ammo stacks are 6d7 so this would add 3d7) */
		if (f3 & TR3_THROWN) /* skip grenades */;
		else if ((f4 & TR4_EXPLODE_A) && (j_ptr->number < 30))
			j_ptr->number += damroll(((k_ptr->gen_dice+1)/2), k_ptr->gen_side);
		else if (f4 & TR4_EXPLODE_A) /* or 1d7 + 1 */
			j_ptr->number += 1 + damroll(1, k_ptr->gen_side);
	}

	/* allow artifact sets of throwing weapons (grenades aren't allowed) */
	/* throwing weapons sets should probably have a number set in artifact.txt */
	if ((artifact_p(j_ptr)) && (j_ptr->number > 1) && 
		(f3 & TR3_THROWN) && (!(j_ptr->tval == TV_FLASK)))
		j_ptr->number = randint(3) + 1;
	/* prevent creating multiple artifacts */
	else if (artifact_p(j_ptr)) j_ptr->number = 1;

    /* Notice "okay" out-of-depth objects */
	if (!cursed_p(j_ptr) && !broken_p(j_ptr) &&
	    (k_info[j_ptr->k_idx].level > p_ptr->depth))
	{
        /* Rating increase (was /3) */
		/* rating += (k_info[j_ptr->k_idx].level - p_ptr->depth); */
        rating += (k_info[j_ptr->k_idx].level - p_ptr->depth + 1)/2;

		/* Cheat -- peek at items */
		if (cheat_peek) object_mention(j_ptr);
	}

	/* let the base object affect the rating */
	/* artifacts have their ratings separate */
	if (!artifact_p(j_ptr)) do_rating(j_ptr, FALSE);

	/* cheat: completely identify everything on creation (good for testing). */
	/* (exclude artifacts because of preserve mode and because you */
	/* get score on finding an artifact in object_known(), and that */
	/* shouldn't happen until you've gained possesion of it.) */
    /* (you'll know it's an artifact because it isn't IDed) */
    if ((cheat_noid) && (!artifact_p(j_ptr)))
	{
    	object_aware(j_ptr);
		object_known(j_ptr);
		j_ptr->ident |= (IDENT_MENTAL);
    }

	/* Success */
	return (TRUE);
}



/*
 * XXX XXX XXX Do not use these hard-coded values.
 */
#define MAX_GOLD	18	/* Number of "gold" entries */

/*
 * Make a treasure object
 *
 * The location must be a legal, clean, floor grid.
 * good (from DROP_GOOD or DROP_GREAT) increases gold value
 */
bool make_gold(object_type *j_ptr, int good)
{
	int sval;
	int k_idx;
	s32b base;

	/* Hack -- Pick a Treasure variety */
	sval = ((randint(object_level + 2) + 2) / 2);
	
	/* to offset increased good/great gold drops */
	if ((randint(100) > 10+goodluck/2) && (sval > 4)) sval -= 1;

	/* good = DROP_GOOD (1), DROP_GREAT (2), normally 0 */
	if (good) sval += good;

	/* Apply "extra" magic */
	if (rand_int(GREAT_OBJ) == 0)
	{
		sval += randint(object_level + 1);
	}

	/* Hack -- Creeping Coins only generate "themselves" */
	if (coin_type) sval = coin_type;

	/* Do not create "illegal" Treasure Types */
	if (sval > MAX_GOLD) sval = MAX_GOLD;

	k_idx = lookup_kind(TV_GOLD, sval);
	
	/* Prepare a gold object */
	object_prep(j_ptr, k_idx);

	/* Hack -- Base coin cost */
	base = k_info[k_idx].cost;

	/* Determine how much the treasure is "worth" */
	j_ptr->pval = (base + (8L * randint(base)) + randint(8));
	if (j_ptr->pval > 12) j_ptr->pval -= (badluck+1)/2;
/* #ifdef EFG */
        if (adult_cansell)
        {
           return (TRUE);        
        }
        else
        {
	/* EFGchange no selling to stores */
#define GOLD_DROP_MULT	4
	int gold_mult;
	if (object_level < GOLD_DROP_MULT)
		gold_mult = object_level + 1;
	else
		gold_mult = GOLD_DROP_MULT;
	/* DROP_GREAT gold */
	if (good == 2) gold_mult += 1;
	/* luck factor */
	if ((goodluck < 8) && (gold_mult > 2))
    {
        if (rand_int(100 + (goodluck*4)) < (badluck+3)*2) gold_mult -= 1;
    }

	/* avoid overflow */
	if ((j_ptr->pval + 1)* gold_mult >= 0)
		j_ptr->pval = j_ptr->pval * gold_mult + randint(gold_mult);
	/* ??? should have an else */
/* #endif */
         }

	/* Success */
	return (TRUE);
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

		/* Count objects (but not squelched items) */
		if (!squelch_hide_item(o_ptr)) n++;
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
	int i, k, n, d, s;
	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;

	object_type *o_ptr;
	u32b f1, f2, f3, f4;

	char o_name[80];

	bool flag = FALSE;
	bool plural = FALSE;
	bool unbreakable = FALSE;


	/* Extract plural */
	if (j_ptr->number != 1) plural = TRUE;

	/* Get flags (to check for EXPLODE_A) */
	object_flags(j_ptr, &f1, &f2, &f3, &f4);

	/* Describe object */
	object_desc(o_name, sizeof(o_name), j_ptr, FALSE, 0);
	
	/* Even artifacts break if they explode  */
    if (artifact_p(j_ptr)) unbreakable = TRUE;
    if (f4 & TR4_EXPLODE_A) unbreakable = FALSE;

	/* Handle normal "breakage" */
	if ((rand_int(100) < chance) && (!unbreakable))
	{
		/* Message */
		msg_format("The %s disappear%s.",
		           o_name, (plural ? "" : "s"));

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

			/* object can be dropped in a doorway */
			if (cave_feat[ty][tx] == FEAT_RUBBLE) /* okay */;
			/* object can be in the same space as rubble */
			if (cave_feat[ty][tx] == FEAT_RUBBLE)
			{
				/* often rolls off the rubble */
				if (randint(100) < 60) continue;
			}
			/* Require floor space */
			/* else if (cave_feat[ty][tx] != FEAT_FLOOR) continue; */
			else if (!cave_floor_bold(ty, tx)) continue;
			/* certain other non-wall features that shouldn't have an object: */
			/* (glyph, stairs, and shop doors) */
			if ((cave_feat[ty][tx] == FEAT_GLYPH) || 
        	    ((cave_feat[ty][tx] >= FEAT_LESS) &&
				(cave_feat[ty][tx] <= FEAT_SHOP_TAIL))) continue;
			/* (traps and doorways CAN have objects on them now) */

			/* No objects */
			k = 0;
			n = 0;

			/* Scan objects in that grid */
			for (o_ptr = get_first_object(ty, tx); o_ptr; o_ptr = get_next_object(o_ptr))
			{
				/* Check for possible combination */
				if (object_similar(o_ptr, j_ptr)) comb = TRUE;

				/* Count objects */
				if (!squelch_hide_item(o_ptr))
					k++;
				else
					n++;
			}

			/* Add new object */
			if (!comb) k++;

			/* Option -- disallow stacking */
			if (adult_no_stacking && (k > 1)) continue;
			
			/* Paranoia? */
			if ((k + n) > MAX_FLOOR_STACK) continue;

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

		/* Require floor space */
		if (cave_feat[ty][tx] != FEAT_FLOOR) continue;

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


	/* Sound */
	sound(MSG_DROP);

	/* Mega-Hack -- no message if "dropped" by player */
	/* Message when an object falls under the player */
	if (chance && (cave_m_idx[by][bx] < 0))
	{
		msg_print("You feel something roll beneath your feet.");
	}

	/* update object list */
	p_ptr->window |= PW_OBJLIST;
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
		if (spellswitch == 5) make_object(i_ptr, FALSE, FALSE);
        else make_object(i_ptr, TRUE, great);

		/* only track certain categories of items */
		if ((ego_item_p(i_ptr)) || (artifact_p(i_ptr)) ||
			(i_ptr->tval == TV_SPECIAL) || (i_ptr->tval == TV_CHEST) ||
			(do_rating(i_ptr, TRUE)))
		{
			/* track origin */
			i_ptr->dlevel = p_ptr->depth;
			/* created by ?aquirement (this function is used by other things but if */
			/* great is true then it was either ?aquirement or the tourist aquirement spell) */
			if (great) i_ptr->vcode = 9;
			else i_ptr->vcode = 10;
		}

		/* Drop the object */
		drop_near(i_ptr, -1, y1, x1);
	}
}

/*
 * create big rocks in a space with rubble
 * mostly copied from the previous function
 */
void big_rocks(int y, int x)
{
	object_type *i_ptr;
	object_type object_type_body;
	bool make = TRUE;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Make the big rocks */
	/* the following copied from parts of make_object() */
	if (make)
	{
		int k_idx;

		k_idx = lookup_kind(TV_SKELETON, SV_BIG_ROCK);
		object_prep(i_ptr, k_idx);
		i_ptr->number = damroll(1, 3);
	}

	/* Drop the object (never let it roll to another space) */
	(void)floor_carry(y, x, i_ptr);
	/* drop_near(i_ptr, -1, y1, x1); */
}


/*
 * place a chest in a vault
 * modes: 0=ruined chest, 1=small chest, 2=large chest, 
 *  3=special DROP_GOOD chest, 4=special DROP_GREAT chest.
 */
void place_chest(int y, int x, int mode)
{
	object_type *i_ptr;
	object_type object_type_body;
	bool make = TRUE;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Make an appropriate chest */
	/* the following copied from parts of make_object() */
	if (make)
	{
		int k_idx, sval;

		if (mode == 1) sval = 1 + rand_int(3);
		else if (mode == 0) sval = SV_RUINED_CHEST; /* empty vaults only */
		else if (mode == 3) sval = SV_SP_SILVER_CHEST;
		else if (mode == 4) sval = SV_SP_GOLD_CHEST;
		else sval = 5 + rand_int(3);

		k_idx = lookup_kind(TV_CHEST, sval);
		object_prep(i_ptr, k_idx);

		/* give it a trap (unless ruined) */
		if (mode) a_m_aux_4(i_ptr, 0, 0);
	}

	/* track origin */
	i_ptr->dlevel = p_ptr->depth;
	if (cave_info[y][x] & (CAVE_ICKY)) i_ptr->vcode = 1;

	/* Drop the object (never let it roll to another space) */
	(void)floor_carry(y, x, i_ptr);
	/* drop_near(i_ptr, -1, y1, x1); */
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

	/* allow objects to be placed in rubble, pits, or water (?) */
	if (((cave_feat[y][x] == FEAT_RUBBLE) || (cave_feat[y][x] == FEAT_OPEN_PIT) ||
		(cave_feat[y][x] == FEAT_WATER)) && (cave_o_idx[y][x] == 0)) /* */;

	/* Hack -- clean floor space */
	else if (!cave_clean_bold(y, x)) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Wipe the object */
	object_wipe(i_ptr);

	/* Make an object (if possible) */
	if (make_object(i_ptr, good, great))
	{
		/* only track certain categories of items */
		if ((ego_item_p(i_ptr)) || (artifact_p(i_ptr)) ||
			(i_ptr->tval == TV_SPECIAL) || (i_ptr->tval == TV_CHEST) ||
			(do_rating(i_ptr, TRUE)))
		{
			/* track origin */
			i_ptr->dlevel = p_ptr->depth;
			/* generated in a vault */
			if (cave_info[y][x] & (CAVE_ICKY)) i_ptr->vcode = 1;
			/* this can only ever happen in an '8' grid of an empty vault */
			else if (great) i_ptr->vcode = 11;
		}

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
	if (make_gold(i_ptr, 0))
	{
		/* Give it to the floor */
		(void)floor_carry(y, x, i_ptr);
	}
}



/*
 * Hack -- instantiate a trap
 *
 * XXX XXX XXX Actually, it is not this routine, but the "trap instantiation"
 * code, which should also check for trap doors on quest levels.
 */
void pick_trap(int y, int x, bool noleave)
{
	int feat;

	/* Paranoia */
	if (cave_feat[y][x] != FEAT_INVIS) return;

	/* Pick a trap */
	while (1)
	{
		/* Hack -- pick a trap */
		feat = FEAT_TRAP_HEAD + rand_int(17);

		/* Hack -- no trap doors on quest levels */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && is_quest(p_ptr->depth)) continue;

		/* Hack -- no trap doors on the deepest level */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && (p_ptr->depth >= MAX_DEPTH-1)) continue;

		/* no instantly creating a trap door underneath you */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && (noleave)) continue;
		
		   /* minimum levels for traps: */
		/* trap doors minL3 */
		if ((feat == FEAT_TRAP_HEAD + 0x00) && (p_ptr->depth < 3)) continue;
		/* spiked pits minL3 */
		if ((feat == FEAT_TRAP_HEAD + 0x02) && (p_ptr->depth < 3)) continue;
		/* poison spiked pit minL6 */
		if ((feat == FEAT_TRAP_HEAD + 0x03) && (p_ptr->depth < 6)) continue;
		/* summon monster trap minL3 */
		if ((feat == FEAT_TRAP_HEAD + 0x04) && (p_ptr->depth < 3)) continue;
		/* fire trap minL2 (damage is reduced for L2-3) */
		if ((feat == FEAT_TRAP_HEAD + 0x07) && (p_ptr->depth < 2)) continue;
		/* acid trap minL4 */
		if ((feat == FEAT_TRAP_HEAD + 0x07) && (p_ptr->depth < 4)) continue;
		/* DEX drain dart trap minL2 */
		if ((feat == FEAT_TRAP_HEAD + 0x0A) && (p_ptr->depth < 2)) continue;
		/* CON drain dart trap minL3 */
		if ((feat == FEAT_TRAP_HEAD + 0x0B) && (p_ptr->depth < 3)) continue;
		/* STR drain dart trap minL4 */
		if ((feat == FEAT_TRAP_HEAD + 0x09) && (p_ptr->depth < 4)) continue;

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
	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x)) return;

	/* Place an invisible trap */
	cave_set_feat(y, x, FEAT_INVIS);
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
	int tmp;

	/* Choose an object */
	tmp = rand_int(400);

	/* Closed doors (300/400) */
	if (tmp < 300)
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_DOOR_CLOSE);
	}

	/* Locked doors (99/400) */
	else if (tmp < 399)
	{
		/* Create locked door */
		cave_set_feat(y, x, FEAT_DOOR_LOCKD);
	}

	/* Stuck doors (1/400) */
	else
	{
		/* Create jammed door */
		cave_set_feat(y, x, FEAT_DOOR_STUCK);
	}
}


/*
 * Place a random type of door at the given location.
 */
void place_random_door(int y, int x, bool mtvault)
{
	int tmp;

	/* Choose an object */
	tmp = rand_int(1000);
	
	/* in empty vaults, doors are much less likely to be left open */
	/* and there's a separate symbol for secret doors so bias against secret doors. */
	if (mtvault) tmp = rand_int(620) + 260;

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

	/* Closed, locked, or stuck doors (400/1000) */
	else if (tmp < 800)
	{
		/* Create closed door */
		place_closed_door(y, x);
	}

	/* Secret doors (200/1000) */
	else
	{
		/* Create secret door */
		cave_set_feat(y, x, FEAT_SECRET);
	}
}

/*
 * sweep the floor (delete all squelched items in a space)
 */
bool sweep_floor(int y, int x, bool killall)
{
	s16b this_o_idx, next_o_idx = 0;
	object_type *o_ptr;
	
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object and the next */
		o_ptr = &o_list[this_o_idx];
		next_o_idx = o_ptr->next_o_idx;

		if ((squelch_item_ok(o_ptr)) || (killall))
		{
			/* Delete the object */
			delete_object_idx(this_o_idx);
		}
		else if ((o_ptr->tval == TV_SKELETON) && (o_ptr->sval == SV_BIG_ROCK) &&
			(squelch_hide_item(o_ptr)))
		{
			/* Delete the object */
			delete_object_idx(this_o_idx);
		}
		
		continue;
	}
	
	/* true if no objects left in the space */
	if (cave_o_idx[y][x] == 0) return TRUE;
	return FALSE;
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
	msg_format("You have %d charge%s remaining.", o_ptr->charges,
	           (o_ptr->charges != 1) ? "s" : "");
}


/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
	object_type *o_ptr = &inventory[item];

	char o_name[80];

	if (artifact_p(o_ptr) && object_known_p(o_ptr))
	{
		/* Get a description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

		/* Print a message */
		msg_format("You no longer have the %s (%c).", o_name, index_to_label(item));
	}
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

		/* Update "p_ptr->pack_size_reduce" */
		if (IS_QUIVER_SLOT(item)) find_quiver_size();

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate mana XXX */
		p_ptr->update |= (PU_MANA);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_OBJLIST);
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
		/* Reorder the quiver if necessary */
		if (IS_QUIVER_SLOT(item)) p_ptr->notice |= (PN_REORDER);

		/* One less item */
		else p_ptr->equip_cnt--;

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

	/* Print a message */
	msg_format("There are %d charge%s remaining.", o_ptr->charges,
	           (o_ptr->charges != 1) ? "s" : "");
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
	msg_format("You see %s.", o_name);

	/* update object list */
	p_ptr->window |= PW_OBJLIST;
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
	/* tekekinesis may not move special vault chests */
	if (spellswitch == 24)
	{
		if ((o_ptr->tval == TV_CHEST) && ((o_ptr->sval == SV_SP_GOLD_CHEST) ||
           (o_ptr->sval == SV_SP_SILVER_CHEST))) return (FALSE);
	}

	/* Empty slot? */
	if (p_ptr->inven_cnt < INVEN_PACK - p_ptr->pack_size_reduce)
	{
		return (TRUE);
	}

	/* Check if it can stack */
	if (inven_stack_okay(o_ptr, FALSE)) return TRUE;

	/* Nope */
	return FALSE;
}

/*
 * Check to see if an item is stackable in the inventory
 */
bool inven_stack_okay(object_type *o_ptr, bool autopu)
{
	int j;

	/* Similar slot? */
	for (j = 0; j < INVEN_PACK; j++)
	{
		object_type *j_ptr = &inventory[j];

		/* Skip non-objects */
		if (!j_ptr->k_idx) continue;

		/* Check if the two items can be combined */
		if (object_similar(j_ptr, o_ptr)) 
		{
			/* Never autopickup items known to be cursed */
			if ((cursed_p(j_ptr)) && (autopu) &&
				((object_known_p(j_ptr)) || (j_ptr->ident & (IDENT_SENSE))))
			{
				/* sense that it is like the others.  (If o_ptr is cursed and */
				/* already IDed or pseudo'ed then this function isn't called) */
				o_ptr->pseudo = INSCRIP_CURSED;
				o_ptr->ident |= (IDENT_SENSE);
				return (FALSE);
			}

			/* otherwise fine */
			return (TRUE);
		}
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
s16b inven_carry(object_type *o_ptr)
{
	int i, j, k;
	int n = -1;

	object_type *j_ptr;

	/* Apply an autoinscription */
	apply_autoinscription(o_ptr);

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


	/* Find an empty slot */
	for (j = 0; j <= INVEN_PACK; j++)
	{
		/*
		 * Hack -- Force pack overflow if we reached the slots of the
		 * inventory reserved for the quiver. -DG-
		 */
		if (j >= INVEN_PACK - p_ptr->pack_size_reduce)
		{
			/* Jump to INVEN_PACK to not mess up pack reordering */
			j = INVEN_PACK;

			break;
		}

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
			if ((o_ptr->tval == cp_ptr->spell_book) &&
			    (j_ptr->tval != cp_ptr->spell_book)) break;
			if ((j_ptr->tval == cp_ptr->spell_book) &&
			    (o_ptr->tval != cp_ptr->spell_book)) continue;

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
	p_ptr->total_weight += (j_ptr->number * j_ptr->weight);

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
 * Return the inventory slot into which the item is placed.
 */
s16b inven_takeoff(int item, int amt)
{
	int slot;

	object_type *o_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	cptr act, act2 = "";

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
	object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 6);

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

	else if (IS_QUIVER_SLOT(item))
	{
		act = "You removed";
		act2 = " from your quiver";
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

	/* Message, sound if not the quiver */
	if (!(IS_QUIVER_SLOT(item))) sound(MSG_WIELD);
	msg_format("%s %s (%c)%s.", act, o_name, index_to_label(slot), act2);

#ifdef EFG
	/* EFGchange drop squelched wielded items immediately when taken off */
	p_ptr->notice |= PN_SQUELCH;
#endif
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

		/* Get the original object */
		o_ptr = &inventory[item];
	}


	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

	/* Distribute charges of wands, staves, or rods */
	distribute_charges(o_ptr, i_ptr, amt);

	/* Modify quantity */
	i_ptr->number = amt;

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
 * Also "pick up" any gold in the inventory by accident
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
		bool slide = FALSE;

		/* Get the item */
		o_ptr = &inventory[i];

		/* Skip empty items */
		if (!o_ptr->k_idx) continue;

		/* Absorb gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Count the gold */
			slide = TRUE;

			if (adult_cansell)
			{
				p_ptr->au += o_ptr->pval;  /* copied from #else */
			}
			else
			{
				/* EFGchange larger money objects */
				p_ptr->au += o_ptr->number * ((long) o_ptr->pval);
			}
		}

		/* Scan the items above that item */
		else for (j = 0; j < i; j++)
		{
			/* Get the item */
			j_ptr = &inventory[j];

			/* Skip empty items */
			if (!j_ptr->k_idx) continue;

			/* Can we drop "o_ptr" onto "j_ptr"? */
			if (object_similar(j_ptr, o_ptr))
			{
				/* Take note */
				flag = slide = TRUE;

				/* Add together the item counts */
				object_absorb(j_ptr, o_ptr);

				break;
			}
		}


		/* Compact the inventory */
		if (slide)
		{
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
			if ((o_ptr->tval == cp_ptr->spell_book) &&
			    (j_ptr->tval != cp_ptr->spell_book)) break;
			if ((j_ptr->tval == cp_ptr->spell_book) &&
			    (o_ptr->tval != cp_ptr->spell_book)) continue;

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
	    (o_ptr->tval == TV_STAFF) ||
	    (o_ptr->tval == TV_ROD))
	{
		q_ptr->charges = o_ptr->charges * amt / o_ptr->number;

		if (amt < o_ptr->number) o_ptr->charges -= q_ptr->charges;

		/*
		 * Hack -- Rods also need to have their timeouts distributed.
		 *
		 * The dropped stack will accept all time remaining to charge up to
		 * its maximum.
		 */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			if (q_ptr->charges > o_ptr->timeout)
				q_ptr->timeout = o_ptr->timeout;
			else
				q_ptr->timeout = q_ptr->charges;

			if (amt < o_ptr->number)
				o_ptr->timeout -= q_ptr->timeout;
		}
	}

	/* distribute enhancement */
	if ((o_ptr->enhance) && (o_ptr->number - amt < o_ptr->enhancenum))
	{
		int qenhance, oenhnum = o_ptr->enhancenum;
		q_ptr->enhancenum = o_ptr->enhancenum - (o_ptr->number - amt);
		o_ptr->enhancenum = o_ptr->number - amt;

		qenhance = (o_ptr->enhance / oenhnum) * q_ptr->enhancenum;
		o_ptr->enhance -= qenhance;
		q_ptr->enhance = qenhance;
	}
	/* none of the dropped items are enhanced */
	else
	{
		q_ptr->enhancenum = 0;
		q_ptr->enhance = 0;
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
	     (o_ptr->tval == TV_STAFF) ||
	     (o_ptr->tval == TV_ROD)) &&
	    (amt < o_ptr->number))
	{
		o_ptr->charges -= o_ptr->charges * amt / o_ptr->number;

		/* reduce enhancement */
		if ((o_ptr->enhance) && (o_ptr->number - amt < o_ptr->enhancenum))
		{
			int oenhnum = o_ptr->enhancenum;
			o_ptr->enhancenum = o_ptr->number - amt;
			o_ptr->enhance -= (o_ptr->enhance / oenhnum) * amt;
		}
	}
}


/*
 * Returns in tag_num the numeric value of the inscription tag associated with
 * an object in the inventory.
 * Valid tags have the form "@n" or "@xn" where "n" is a number (0-9) and "x"
 * is cmd. If cmd is 0 then "x" can be anything.
 * Returns FALSE if the object doesn't have a valid tag.
 */
int get_tag_num(int o_idx, int cmd, byte *tag_num)
{
	object_type *o_ptr = &inventory[o_idx];
	char *s;

	/* Ignore empty objects */
	if (!o_ptr->k_idx) return FALSE;

	/* Ignore objects without notes */
	if (!o_ptr->note) return FALSE;

	/* Find the first '@' */
	s = strchr(quark_str(o_ptr->note), '@');

	while (s)
	{
		/* Found "@n"? */
		if (isdigit((unsigned char)s[1]))
		{
			/* Convert to number */
			*tag_num = D2I(s[1]);
			return TRUE;
		}

		/* Found "@xn"? */
		if ((!cmd || ((unsigned char)s[1] == cmd)) &&
			isdigit((unsigned char)s[2]))
		{
			/* Convert to number */
			*tag_num = D2I(s[2]);
			return TRUE;
		}

		/* Find another '@' in any other case */
		s = strchr(s + 1, '@');
	}

	return FALSE;
}


/*
 * Calculate and apply the reduction in pack size due to use of the
 * Quiver.
 */
void find_quiver_size(void)
{
	int ammo_num, i;
	object_type *i_ptr;

	/*
	 * Items in the quiver take up space which needs to be subtracted
	 * from space available elsewhere.
	 */
	ammo_num = 0;

	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* Get the item */
		i_ptr = &inventory[i];

		/* Ignore empty. */
		if (!i_ptr->k_idx) continue;

		/* Tally up missiles. */
		ammo_num += i_ptr->number * quiver_space_per_unit(i_ptr);
	}

	/* Every 99 missiles in the quiver takes up one backpack slot. */
	p_ptr->pack_size_reduce = (ammo_num + 98) / 99;
}


/*
 * Combine ammo in the quiver.
 */
void combine_quiver(void)
{
	int i, j, k;

	object_type *i_ptr;
	object_type *j_ptr;

	bool flag = FALSE;

	/* Combine the quiver (backwards) */
	for (i = END_QUIVER - 1; i > INVEN_QUIVER; i--)
	{
		/* Get the item */
		i_ptr = &inventory[i];

		/* Skip empty items */
		if (!i_ptr->k_idx) continue;

		/* Scan the items above that item */
		for (j = INVEN_QUIVER; j < i; j++)
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

				/* Slide everything down */
				for (k = i; k < (END_QUIVER - 1); k++)
				{
					/* Hack -- slide object */
					COPY(&inventory[k], &inventory[k+1], object_type);
				}

				/* Hack -- wipe hole */
				object_wipe(&inventory[k]);

				/* Done */
				break;
			}
		}
	}

	if (flag)
	{

		/*
		 * Prevent the use of CTRL-V with the 'f'ire and 't'hrow commands when the
		 * quiver changes -DG-
		 * repeat_reset_command('f');
		 * repeat_reset_command('v');
		 */

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Message */
		msg_print("You combine your quiver.");
	}
}

/*
 * Sort ammo in the quiver.  If requested, track the given slot and return
 * its final position.
 *
 * Items marked with inscriptions of the form "@ [f] [any digit]"
 * ("@f4", "@4", etc.) will be locked. It means that the ammo will be always
 * fired using that digit. Different locked ammo can share the same tag.
 * Unlocked ammo can be fired using its current pseudo-tag (shown in the
 * equipment window). In that case the pseudo-tag can change depending on
 * ammo consumption. -DG- (based on code from -LM- and -BR-)
 */
int reorder_quiver(int slot)
{
	int i, j, k;

	s32b i_value;
	byte i_group;

	object_type *i_ptr;
	object_type *j_ptr;

	bool flag = FALSE;

	byte tag;

	/* This is used to sort locked and unlocked ammo */
	struct ammo_info_type {
		byte group;
		s16b idx;
		byte tag;
		s32b value;
	} ammo_info[MAX_QUIVER];
	/* Position of the first locked slot */
	byte first_locked;
	/* Total size of the quiver (unlocked + locked ammo) */
	byte quiver_size;

	/*
	 * This will hold the final ordering of ammo slots.
	 * Note that only the indexes are stored
	 */
	s16b sorted_ammo_idxs[MAX_QUIVER];

	/*
	 * Reorder the quiver.
	 *
	 * First, we sort the ammo *indexes* in two sets, locked and unlocked
	 * ammo. We use the same table for both sets (ammo_info).
	 * Unlocked ammo goes in the beginning of the table and locked ammo
	 * later.
	 * The sets are merged later to produce the final ordering.
	 * We use "first_locked" to determine the bound between sets. -DG-
	 */
	first_locked = quiver_size = 0;

	/* Traverse the quiver */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* Get the object */
		i_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!i_ptr->k_idx) continue;

		/* Get the value of the object */
		i_value = object_value(i_ptr);

		/* Note that we store all throwing weapons in the same group */
		i_group = quiver_get_group(i_ptr);

		/* Get the real tag of the object, if any */
		if (get_tag_num(i, quiver_group[i_group].cmd, &tag))
		{
			/* Determine the portion of the table to be used */
			j = first_locked;
			k = quiver_size;
		}
		/*
		 * If there isn't a tag we use a special value
		 * to separate the sets.
		 */
		else
		{
			tag = 10;
			/* Determine the portion of the table to be used */
			j = 0;
			k = first_locked;

			/* We know that all locked ammo will be displaced */
			++first_locked;
		}

		/* Search for the right place in the table */
		for (; j < k; j++)
		{
			/* Get the other ammo */
			j_ptr = &inventory[ammo_info[j].idx];

			/* Objects sort by increasing group */
			if (i_group < ammo_info[j].group) break;
			if (i_group > ammo_info[j].group) continue;

			/*
			 * Objects sort by increasing tag.
			 * Note that all unlocked ammo have the same tag (10)
			 * so this step is meaningless for them -DG-
			 */
			if (tag < ammo_info[j].tag) break;
			if (tag > ammo_info[j].tag) continue;

			/* Objects sort by decreasing tval */
			if (i_ptr->tval > j_ptr->tval) break;
			if (i_ptr->tval < j_ptr->tval) continue;

			/* Non-aware items always come last */
			if (!object_aware_p(i_ptr)) continue;
			if (!object_aware_p(j_ptr)) break;

			/* Objects sort by increasing sval */
			if (i_ptr->sval < j_ptr->sval) break;
			if (i_ptr->sval > j_ptr->sval) continue;

			/* Unidentified objects always come last */
			if (!object_known_p(i_ptr)) continue;
			if (!object_known_p(j_ptr)) break;

			/* Objects sort by decreasing value */
			if (i_value > ammo_info[j].value) break;
			if (i_value < ammo_info[j].value) continue;
		}

		/*
		 * We found the place. Displace the other slot
		 * indexes if neccesary
		 */
		for (k = quiver_size; k > j; k--)
		{
			COPY(&ammo_info[k], &ammo_info[k - 1],
				struct ammo_info_type);
		}

		/* Cache some data from the slot in the table */
		ammo_info[j].group = i_group;
		ammo_info[j].idx = i;
		ammo_info[j].tag = tag;
		/* We cache the value of the object too */
		ammo_info[j].value = i_value;

		/* Update the size of the quiver */
		++quiver_size;
	}

	/*
	 * Second, we merge the two sets to find the final ordering of the
	 * ammo slots.
	 * What this step really does is to place unlocked ammo in
	 * the slots which aren't used by locked ammo. Again, we only work
	 * with indexes in this step.
	 */
	i = 0;
	j = first_locked;
	tag = k = 0;

	/* Compare ammo between the sets */
	while ((i < first_locked) && (j < quiver_size))
	{
		/* Groups are different. Add the smallest first */
		if (ammo_info[i].group > ammo_info[j].group)
		{
			sorted_ammo_idxs[k++] = ammo_info[j++].idx;
			/* Reset the tag */
			tag = 0;
		}

		/* Groups are different. Add the smallest first */
		else if (ammo_info[i].group < ammo_info[j].group)
		{
			sorted_ammo_idxs[k++] = ammo_info[i++].idx;
			/* Reset the tag */
			tag = 0;
		}

		/*
		 * Same group, and the tag is unlocked. Add some unlocked
		 * ammo first
		 */
		else if (tag < ammo_info[j].tag)
		{
			sorted_ammo_idxs[k++] = ammo_info[i++].idx;
			/* Increment the pseudo-tag */
			++tag;
		}
		/*
		 * The tag is locked. Perhaps there are several locked
		 * slots with the same tag too. Add the locked ammo first
		 */
		else
		{
			/*
			 * The tag is incremented only at the first ocurrence
			 * of that value
			 */
			if (tag == ammo_info[j].tag)
			{
				++tag;
			}
			/* But the ammo is always added */
			sorted_ammo_idxs[k++] = ammo_info[j++].idx;
		}
	}

	/* Add remaining unlocked ammo, if neccesary */
	while (i < first_locked)
	{
		sorted_ammo_idxs[k++] = ammo_info[i++].idx;
	}

	/* Add remaining locked ammo, if neccesary */
	while (j < quiver_size)
	{
		sorted_ammo_idxs[k++] = ammo_info[j++].idx;
	}

	/* Determine if we have to reorder the real ammo */
	for (k = 0; k < quiver_size; k++)
	{
		if (sorted_ammo_idxs[k] != (INVEN_QUIVER + k))
		{
			flag = TRUE;

			break;
		}
	}

	/* Reorder the real quiver, if necessary */
	if (flag)
	{
		/* Temporary copy of the quiver */
		object_type quiver[MAX_QUIVER];

		/*
		 * Copy the real ammo into the temporary quiver using
		 * the new order
		 */
		for (i = 0; i < quiver_size; i++)
		{
			/* Get the original slot */
			k = sorted_ammo_idxs[i];

			/* Note the indexes */
			object_copy(&quiver[i], &inventory[k]);

			/*
			 * Hack - Adjust the temporary slot if necessary.
			 * Note that the slot becomes temporarily negative
			 * to avoid multiple matches (indexes are positive).
			 */
			if (slot == k) slot = 0 - (INVEN_QUIVER + i);
		}

		/*
		 * Hack - The loop has ended and slot was changed only once
		 * like we wanted. Now we make slot positive again. -DG-
		 */
		if (slot < 0) slot = 0 - slot;

		/*
		 * Now dump the temporary quiver (sorted) into the real quiver
		 */
		for (i = 0; i < quiver_size; i++)
		{
			object_copy(&inventory[INVEN_QUIVER + i],
					&quiver[i]);
		}

		/* Clear unused slots */
		for (i = INVEN_QUIVER + quiver_size; i < END_QUIVER; i++)
		{
			object_wipe(&inventory[i]);
		}

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/*
		 * Prevent the use of CTRL-V with the 'f'ire and 't'hrow commands when the
		 * quiver changes -DG-
		 *
		 * repeat_reset_command('f');
		 * repeat_reset_command('v');
		 */

		/* Message */
		if (!slot) msg_print("You reorganize your quiver.");
	}

	return (slot);
}

/*
 * Returns TRUE if an object is a throwing weapon and can be put in the quiver
 */
bool is_throwing_weapon(const object_type *o_ptr)
{
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	
	/* certain classes may put damaging potions in the quiver */
	if (cp_ptr->flags & CF_THROW_POTIONS)
	{
		if (((o_ptr->tval == TV_POTION) || (o_ptr->tval == TV_FLASK)) && 
			((f3 & (TR3_THROWN)) || (f3 & (TR3_PTHROW))) &&
			(object_aware_p(o_ptr)))
			return TRUE;
	}
	/* certain classes may put damaging mushrooms (and hard biscuits) in the quiver */
	if (cp_ptr->flags & CF_THROW_SHROOMS)
	{
		if ((o_ptr->tval == TV_FOOD) && 
			((f3 & (TR3_THROWN)) || (f3 & (TR3_PTHROW))) &&
			(object_aware_p(o_ptr)))
			return TRUE;
	}
	
	/* grenades may go in the quiver */
	if ((o_ptr->tval == TV_FLASK) && (f3 & (TR3_THROWN))) /* okay */;
	/* some potions and stuff have PTHROW, but shouldn't go in the quiver */
	/* all weapons & iron spikes are <= tval 23 */
	else if (o_ptr->tval > 23) return FALSE;
	
	/* small semi-throwers can be put into the quiver */
	/* (weight of a spear or lighter) */
    if ((f3 & (TR3_PTHROW)) && (o_ptr->weight <= 52)) return TRUE;

	return ((f3 & (TR3_THROWN)) ? TRUE: FALSE);
}

/*
 * Returns the number of quiver units an object will consume when it's stored in the quiver.
 * Every 99 quiver units we consume an inventory slot
 */
int quiver_space_per_unit(const object_type *o_ptr)
{
	/* weight * 2 (throwing dagger- 2, throwing axe- 4, javelin- 8) */
	int mass = (((o_ptr->weight > 10) ? o_ptr->weight : 10) / 10) * 2;
	
	return (ammo_p(o_ptr) ? 1: mass);
}

/*
 * Returns TRUE if the specified number of objects of type o_ptr->k_idx can be hold in the quiver.
 * Hack: if you are moving objects from the inventory to the quiver pass the inventory slot occupied by the object in
 * "item". This will help us to determine if we have one free inventory slot more. You can pass -1 to ignore this feature.
 */
bool quiver_carry_okay(const object_type *o_ptr, int num, int item)
{
	int i;
	int ammo_num = 0;
	int have;
	int need;

	/* Paranoia */
	if ((num <= 0) || (num > o_ptr->number)) num = o_ptr->number;

	/* Count ammo in the quiver */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* Get the object */
		object_type *i_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!i_ptr->k_idx) continue;

		/* Increment the ammo count */
		ammo_num += (i_ptr->number * quiver_space_per_unit(i_ptr));
	}

	/* Add the requested amount of objects to be put in the quiver */
	ammo_num += (num * quiver_space_per_unit(o_ptr));

	/* We need as many free inventory as: */
	need = (ammo_num + 98) / 99;

	/* Calculate the number of available inventory slots */
	have = INVEN_PACK - p_ptr->inven_cnt;

	/* If we are emptying an inventory slot we have one free slot more */
	if ((item >= 0) && (item < INVEN_PACK) && (num == o_ptr->number)) ++have;

	/* Compute the result */
	return (need <= have);
}

/*
 * Returns the quiver group associated to an object. Defaults to throwing weapons
 */
byte quiver_get_group(const object_type *o_ptr)
{
	u32b f1, f2, f3, f4;
	/* check for thrown flag */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	switch (o_ptr->tval)
	{
		case TV_BOLT: return (QUIVER_GROUP_BOLTS);
		case TV_ARROW: return (QUIVER_GROUP_ARROWS);
		case TV_SHOT: return (QUIVER_GROUP_SHOTS);
	}

	if (f3 & (TR3_THROWN)) return (QUIVER_GROUP_THROWING_WEAPONS);

	/* PTHROW flag */
    return (QUIVER_GROUP_SEMI_THROWER);
}

