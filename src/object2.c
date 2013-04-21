/* File: object2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/**************************** Removal functions. ***/


static bool object_slots[50];


/*
 * Get the next object slot.
 */
void next_tag(object_type * o_ptr)
{
	/* Hack -- transparent intialization. */
	static bool initted = FALSE;
	int i;

	if (!initted)
	{
		for (i = 0; i < 50; i++)
		{
			object_slots[i] = FALSE;
		}

		initted = TRUE;
	}

	/* Object already has a preferred tag, try to use it first. */
	if (o_ptr->tag && isalpha(o_ptr->tag))
	{

		if (o_ptr->tag <= 'Z' && !(object_slots[(o_ptr->tag - 'A') + 25]))
		{
			object_slots[(o_ptr->tag - 'A') + 25] = TRUE;
			return;

		}
		else if (!object_slots[o_ptr->tag - 'a'])
		{
			object_slots[o_ptr->tag - 'a'] = TRUE;
			return;
		}
	}

	/* Find the next available tag. */
	for (i = 0; i < 50; i++)
	{
		if (!object_slots[i])
		{
			object_slots[i] = TRUE;

			if (i > 25)
				o_ptr->tag = ('A' + (i - 25));
			else
				o_ptr->tag = ('a' + i);

			return;
		}
	}

	/* Hack -- we ran out of space. */
	o_ptr->tag = 'Z';
}


/*
 * Remove the given tag.
 */
void remove_tag(object_type * o_ptr)
{
	/* Paranoia. */
	if (!isalpha(o_ptr->tag))
		return;

	if (o_ptr->tag <= 'Z')
	{
		object_slots[(o_ptr->tag - 'A') + 25] = FALSE;

	}
	else
	{
		object_slots[o_ptr->tag - 'a'] = FALSE;
	}
}


/*
 * Remove the object from the player's inventory.
 * This is like ``remove_from_stack'', but will also modify the
 * equipment array.
 */
static void remove_from_inventory(object_type * o_ptr)
{
	int i;

	/* Fix the equipment array. */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i] == o_ptr)
			equipment[i] = NULL;
	}

	if (o_ptr == inventory)
		inventory = o_ptr->next;
	
	link_remove(o_ptr);

	/* No need for this tag anymore. */
	remove_tag(o_ptr);

	o_ptr->tag = 0;

	p_ptr->total_weight -= o_ptr->weight;

	p_ptr->redraw |= (PR_SPEED);
	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	handle_stuff();
}


/*
 * Remove ``o_ptr'' from the global list ``stack''.
 */

void remove_from_global_list(object_type * o_ptr, object_type ** stack)
{
	if (o_ptr == *stack)
		*stack = o_ptr->next_global;

	link_remove_glob(o_ptr);

	o_ptr->world = WORLD_NONE;
}


/*
 * Helper for remove_from_stack.
 */
static void remove_from_stack_aux(object_type * o_ptr,
	object_type ** stack)
{
	if (o_ptr == *stack)
		*stack = o_ptr->next;

	link_remove(o_ptr);
}


/*
 * Automatically determine which stack the item belongs to, and remove it 
 * from the stack. This is the function to use if you want to transfer items
 * between stacks.
 */
void remove_from_stack(object_type * o_ptr)
{
	switch (o_ptr->stack)
	{
		case STACK_INVEN:
			remove_from_inventory(o_ptr);
			break;

		case STACK_MON_INVEN:
			remove_from_stack_aux(o_ptr, &(o_ptr->owner->inventory));
			break;

		case STACK_FLOOR:
			remove_from_stack_aux(o_ptr,
				&(cave_o_idx[o_ptr->iy][o_ptr->ix]));
			break;
	}

	o_ptr->stack = STACK_NONE;
}


/*
 * Split a pile of objects into two parts.
 *
 */
object_type *object_unabsorb(object_type * o_ptr, int num)
{
	if (num == 0 || num >= o_ptr->number)
	{
		return NULL;
	}
	else
	{
		object_type *nw;

		int wgt_one = o_ptr->weight / o_ptr->number;

		MAKE(nw, object_type);
		COPY(nw, o_ptr, object_type);

		nw->in_list = nw->in_global = FALSE;

		switch (o_ptr->world)
		{
			case WORLD_MAIN:
				insert_to_global_list(nw, &(o_list), WORLD_MAIN);
				break;

			case WORLD_HOME:
				insert_to_global_list(nw, &(store[7].stock), WORLD_HOME);
				break;

			case WORLD_STORE:
				insert_to_global_list(nw, &(store[p_ptr->s_idx].stock),
					WORLD_STORE);
				break;
		}

		nw->number = num;
		nw->weight = num * wgt_one;

		o_ptr->number -= num;
		o_ptr->weight -= (wgt_one * num);

		nw->next = NULL;
		nw->prev = NULL;
		nw->stack = STACK_NONE;

		return nw;
	}
}


/*
 * Remove the object from the world. 
 *
 * The object WILL be deleted.
 */
void remove_object(object_type * o_ptr)
{
	/* Remove from any stack */
	remove_from_stack(o_ptr);

	/* Remove from the global list. */
	switch (o_ptr->world)
	{
		case WORLD_MAIN:
		{
			remove_from_global_list(o_ptr, &(o_list));
			break;
		}

		case WORLD_HOME:
		{
			remove_from_global_list(o_ptr, &(store[7].stock));
			break;
		}

		case WORLD_STORE:
		{
			/* Oh-oh, player just destroyed the shopkeeper's stuff! */
			if (hack_punish_theft)
			{
				player_theft();
			}

			remove_from_global_list(o_ptr, &(store[p_ptr->s_idx].stock));
			break;
		}
	}

	/* Delete the object. */
	KILL(o_ptr, object_type);
}


/*
 * Remove all objects. 
 */
void wipe_o_list(void)
{
	object_type *o_ptr;
	object_type *o_nxt;

	o_ptr = o_list;

	/* Delete all dungeon items. */
	while (o_ptr)
	{
		/* Access the "next" object */
		o_nxt = o_ptr->next_global;

		/* Object not currently in player's inventory. */
		if (o_ptr->stack != STACK_INVEN)
		{
			/* Hack -- Preserve unknown artifacts. */
			if ((p_ptr->preserve || !character_dungeon) &&
				artifact_p(o_ptr) && !object_known_p(o_ptr))
			{
				if (o_ptr->tval == TV_RANDART)
				{
					random_artifacts[o_ptr->sval].generated = FALSE;
				}
				else
				{
					a_info[o_ptr->name1].cur_num = 0;
				}
			}

			remove_object(o_ptr);
		}

		o_ptr = o_nxt;
	}

	/* Unstack all store stock. */
	if (p_ptr->inside_special == SPECIAL_STORE)
	{
		o_ptr = store[p_ptr->s_idx].stock;

		while (o_ptr)
		{
			remove_from_stack(o_ptr);

			o_ptr = o_ptr->next_global;
		}
	}
}


/**************************** Insertion functions. ***/


/*
 * Order of items by tval. For the most part, this is the reverse of
 * the actual tval, but some things where reordered.
 */
static byte tval_order_init[] = {
	TV_GOLD,
	TV_SPELLBOOK,
	TV_MIMIC_BOOK,
	TV_FOOD,
	TV_FLASK,
	TV_POTION,
	TV_SCROLL,
	TV_TEXT,
	TV_ROD,
	TV_WAND,
	TV_STAFF,
	TV_RING,
	TV_AMULET,
	TV_LITE,
	TV_DRAG_ARMOR,
	TV_HARD_ARMOR,
	TV_SOFT_ARMOR,
	TV_CLOAK,
	TV_SHIELD,
	TV_CROWN,
	TV_HELM,
	TV_GLOVES,
	TV_BOOTS,
	TV_SWORD,
	TV_POLEARM,
	TV_HAFTED,
	TV_DIGGING,
	TV_BOW,
	TV_BOLT,
	TV_ARROW,
	TV_SHOT,
	TV_CHEST,
	TV_SPIKE,
	TV_RANDART,
	TV_INGRED,
	TV_CORPSE,
	TV_JUNK,
	TV_BOTTLE,
	TV_SKELETON,
	MAX_TVAL
};

static byte tval_order[MAX_TVAL];


/*
 * Initialize tval priority table.
 */
void init_tval_order(void)
{
	int i, j;

	for (i = 0; i < MAX_TVAL; i++)
	{
		for (j = 0; tval_order_init[j] < MAX_TVAL; j++)
		{
			if (i == tval_order_init[j])
			{
				tval_order[i] = j;
				break;
			}
		}
	}
}


/*
 * Inventory sort comparator. Returns TRUE when o_ptr is ``less than'' i_ptr.
 */
static bool stop_sorting_objects(object_type * o_ptr, object_type * i_ptr)
{
	u32b o_value, i_value;
	bool o_book, i_book;
	int o_tval, i_tval;

	/* Is this a readable book? */
	o_book = (o_ptr->tval == TV_SPELLBOOK) &&
		(o_ptr->sval == cp_ptr->spell_book);

	/* Is this a readable book? */
	i_book = (i_ptr->tval == TV_SPELLBOOK) &&
		(i_ptr->sval == cp_ptr->spell_book);

	/* Hack -- readable books always come first */
	if (o_book && !i_book)
		return TRUE;
	if (i_book && !o_book)
		return FALSE;

	/* Get the tval priority */
	o_tval = tval_order[o_ptr->tval];
	i_tval = tval_order[i_ptr->tval];

	/* Objects sort by decreasing tval priority */
	if (o_tval < i_tval)
		return TRUE;
	if (o_tval > i_tval)
		return FALSE;

	/* Non-aware (flavored) items always come last */
	if (!object_aware_p(o_ptr))
		return FALSE;
	if (!object_aware_p(i_ptr))
		return TRUE;

	/* Objects sort by increasing sval */
	if (o_ptr->sval < i_ptr->sval)
		return TRUE;
	if (o_ptr->sval > i_ptr->sval)
		return FALSE;

	/* Unidentified objects always come last */
	if (!object_known_p(o_ptr))
		return FALSE;
	if (!object_known_p(i_ptr))
		return TRUE;

	/* Objects sort by increasing material */
	if (o_ptr->stuff < i_ptr->stuff) return TRUE;
	if (o_ptr->stuff > i_ptr->stuff) return FALSE;

	/* Rods sort by increasing recharge time */
	if (o_ptr->tval == TV_ROD)
	{
		if (o_ptr->timeout < i_ptr->timeout) return TRUE;
		if (o_ptr->timeout > i_ptr->timeout) return FALSE;
	}

	/* Wands/Staffs sort by decreasing charges */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		if (o_ptr->pval > i_ptr->pval) return TRUE;
		if (o_ptr->pval < i_ptr->pval) return FALSE;
	}

	/* Lites sort by decreasing fuel */
	if (o_ptr->tval == TV_LITE)
	{
		if (o_ptr->pval > i_ptr->pval) return TRUE;
		if (o_ptr->pval < i_ptr->pval) return FALSE;
	}

	/* Objects sort by decreasing health */
	if (o_ptr->chp > i_ptr->chp) return TRUE;
	if (o_ptr->chp < i_ptr->chp) return FALSE;

	/* Determine the "value" */
	o_value = object_value(o_ptr);
	i_value = object_value(i_ptr);

	/* Objects sort by decreasing value */
	if (o_value > i_value)
		return TRUE;
	if (o_value < i_value)
		return FALSE;

	/* Assume greater than */
	return FALSE;
}


/*
 * Inventory sort comparator. Returns TRUE when o_ptr is ``less than'' i_ptr.
 */
static bool stop_sorting_objects_store(object_type * o_ptr, object_type * i_ptr)
{
	u32b o_value, i_value;
	int o_tval, i_tval;

	/* Get the tval priority */
	o_tval = tval_order[o_ptr->tval];
	i_tval = tval_order[i_ptr->tval];

	/* Objects sort by decreasing tval priority */
	if (o_tval < i_tval)
		return TRUE;
	if (o_tval > i_tval)
		return FALSE;

	/* Objects sort by increasing sval */
	if (o_ptr->sval < i_ptr->sval)
		return TRUE;
	if (o_ptr->sval > i_ptr->sval)
		return FALSE;

	/* Objects sort by incresing material */
	if (o_ptr->stuff < i_ptr->stuff) return TRUE;
	if (o_ptr->stuff > i_ptr->stuff) return FALSE;

	/* Rods sort by increasing recharge time */
	if (o_ptr->tval == TV_ROD)
	{
		if (o_ptr->pval < i_ptr->pval) return TRUE;
		if (o_ptr->pval > i_ptr->pval) return FALSE;
	}

	/* Wands/Staffs sort by decreasing charges */
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
	{
		if (o_ptr->pval > i_ptr->pval) return TRUE;
		if (o_ptr->pval < i_ptr->pval) return FALSE;
	}

	/* Lites sort by decreasing fuel */
	if (o_ptr->tval == TV_LITE)
	{
		if (o_ptr->pval > i_ptr->pval) return TRUE;
		if (o_ptr->pval < i_ptr->pval) return FALSE;
	}

	/* Objects sort by decreasing health */
	if (o_ptr->chp > i_ptr->chp) return TRUE;
	if (o_ptr->chp < i_ptr->chp) return FALSE;

	/* Determine the "value" */
	o_value = object_value(o_ptr);
	i_value = object_value(i_ptr);

	/* Objects sort by decreasing value */
	if (o_value > i_value)
		return TRUE;
	if (o_value < i_value)
		return FALSE;

	/* Assume greater than */
	return FALSE;
}


/* 
 * Insert an object ``o_ptr'' to a global list ``stack''.
 */
void insert_to_global_list(object_type * o_ptr, object_type ** stack,
	byte world)
{
	object_type *insert_root = NULL;
	object_type *iter;


	/* Normal stores do special stuff */
	if ((world == WORLD_STORE) && store_combine_flag)
	{
		/* Try to merge two objects together, if possible. */
		for (iter = (*stack); iter != NULL; iter = iter->next_global)
		{
			/* Don't combine worlds */
			if (iter->world != world) continue;
	
			/* Compare the objects */
			if (store_object_similar(iter, o_ptr))
			{
				/* Combine them. */
				store_object_absorb(iter, o_ptr);
	
				/* Delete the object. */
				remove_object(o_ptr);

				/* Done */
				return;
			}
		}
	}

	/* The "home" acts like the player */
	else if ((world == WORLD_HOME) && store_combine_flag)
	{
		/* Try to merge two objects together, if possible. */
		for (iter = (*stack); iter != NULL; iter = iter->next_global)
		{
			/* Don't combine worlds */
			if (iter->world != world) continue;
	
			/* Compare the objects */
			if (object_similar(iter, o_ptr))
			{
				/* Combine them. */
				object_absorb(iter, o_ptr);
	
				/* Delete the object. */
				remove_object(o_ptr);

				/* Done */
				return;
			}
		}
	}


	/* Ugly hack beyond all hacks -- sort store items if required. */
	if (sort_items)
	{
		/* Normal stores do special stuff */
		if (world == WORLD_STORE)
		{
			object_type *iter;
	
			for (iter = (*stack); iter != NULL; iter = iter->next_global)
			{
				if (stop_sorting_objects_store(o_ptr, iter))
				{
					break;
				}
	
				insert_root = iter;
			}
		}

		/* The "home" acts like the player */
		else if (world == WORLD_HOME)
		{
			object_type *iter;
	
			for (iter = (*stack); iter != NULL; iter = iter->next_global)
			{
				if (stop_sorting_objects(o_ptr, iter))
				{
					break;
				}
	
				insert_root = iter;
			}
		}
	}

	if (insert_root)
	{
		link_insert_glob(insert_root, insert_root->next_global, o_ptr);
	}
	else
	{
		link_insert_glob(NULL, *stack, o_ptr);
		*stack = o_ptr;
	}

	o_ptr->world = world;
}


/*
 * Create and return a new object.
 */
object_type *new_object(void)
{
	object_type *ret;

	MAKE(ret, object_type);

	insert_to_global_list(ret, &(o_list), WORLD_MAIN);

	return ret;
}


/*
 * Insert an object into a stack.
 *
 * This function returns TRUE if the object was inserted, and
 * FALSE if the object was absorbed and then deleted.
 *
 * Do not use this function unless you really need to. The ones below
 * are much nicer and safer.
 */
bool insert_to_stack(object_type * o_ptr, object_type ** stack)
{
	object_type *insert_root = NULL;
	object_type *iter;

	/* Oops -- Object is in a stack. */
	if (o_ptr->stack != STACK_NONE)
	{
		remove_from_stack(o_ptr);
	}

	/* Try to merge two objects together, if possible. */
	for (iter = (*stack); iter != NULL; iter = iter->next)
	{
		/* Don't combine worlds */
		if (iter->world != o_ptr->world) continue;

		/* Compare the objects */
		if (object_similar(iter, o_ptr))
		{
			/* Combine them. */
			object_absorb(iter, o_ptr);

			/* Delete the object. */
			remove_object(o_ptr);

			/* Done */
			return FALSE;
		}
	}

	/* Try to sort the items. */
	if (sort_items)
	{
		for (iter = (*stack); iter != NULL; iter = iter->next)
		{
			if (stop_sorting_objects(o_ptr, iter))
			{
				break;
			}

			insert_root = iter;
		}
	}

	if (insert_root)
	{
		link_insert(insert_root, insert_root->next, o_ptr);
	}
	else
	{
		link_insert(NULL, *stack, o_ptr);
		*stack = o_ptr;
	}

	return TRUE;
}


/*
 * Give the object to the floor stack.
 */
bool floor_carry(int y, int x, object_type * o_ptr)
{
	bool ret = FALSE;

	/* Sell items if in a store. */
	if (p_ptr->inside_special == SPECIAL_STORE &&
		o_ptr->world == WORLD_MAIN)
	{
		/* Access the store */
		store_type *st_ptr = &store[p_ptr->s_idx];

		/* Sell it. */
		if (store_sell_item(o_ptr, st_ptr))
		{
			/* Destroy worthless objects, unless in the home */
			if ((p_ptr->s_idx != 7) && (object_value(o_ptr) <= 0))
			{
				remove_object(o_ptr);
				return FALSE;
			}

			remove_from_global_list(o_ptr, &(o_list));
			insert_to_global_list(o_ptr, &(st_ptr->stock),
				(p_ptr->s_idx == 7 ? WORLD_HOME : WORLD_STORE));
		}
	}

	if (insert_to_stack(o_ptr, &(cave_o_idx[y][x])))
	{
		o_ptr->iy = y;
		o_ptr->ix = x;

		o_ptr->stack = STACK_FLOOR;

		ret = TRUE;
	}

	note_spot(y, x);
	lite_spot(y, x);

	return ret;
}

/*
 * Give the object to the player inventory stack.
 */
bool inven_carry(object_type * o_ptr)
{
	bool ret = FALSE;


	/* Buy items if in a store. */
	if ((o_ptr->world == WORLD_HOME) || (o_ptr->world == WORLD_STORE))
	{
		/* Access the store */
		store_type *st_ptr = &store[p_ptr->s_idx];

		/* Purchase store items */
		if ((o_ptr->world == WORLD_STORE) && !store_buy_item(o_ptr, st_ptr))
		{
			ret = TRUE;

			/* XXX Hack -- Don't punish theft when combining objects */
			hack_punish_theft = FALSE;

			/* Inside a vault store */
			if (p_ptr->inside_special == SPECIAL_STORE)
			{
				/* Give it back to the shop. */
				ret = floor_carry(p_ptr->py, p_ptr->px, o_ptr);
			}

			/* Re-combine the wares */
			else
			{
				store_combine(st_ptr);

				/* ret = ??? */
			}

			/* XXX Hack -- undo the hack above */
			hack_punish_theft = TRUE;

			return ret;
		}

		/* Remove the item from the store inventory */
		remove_from_global_list(o_ptr, &(st_ptr->stock));

		/* Insert the item into the list of objects */
		insert_to_global_list(o_ptr, &(o_list), WORLD_MAIN);
	}

	/* Mega-hack: automatically keep track of gold. */
	if (o_ptr->tval == TV_GOLD)
	{
		/* Collect gold */
		p_ptr->au += o_ptr->pval;

		/* Redraw gold */
		p_ptr->redraw |= PR_GOLD;

		/* Destroy the object */
		remove_object(o_ptr);

		/* */
		return FALSE;
	}

	/* Handle fate. */
	fate_effect(o_ptr->fate, FATE_CARRY);

	if (insert_to_stack(o_ptr, &(inventory)))
	{
		o_ptr->stack = STACK_INVEN;

		/* Add the weight */
		p_ptr->total_weight += o_ptr->weight;

		/* Redraw speed */
		p_ptr->redraw |= (PR_SPEED);

		/* Calculate bonus */
		p_ptr->update |= (PU_BONUS);

		/* Handle stuff */
		handle_stuff();

		/* */
		ret = TRUE;
	}

	next_tag(o_ptr);

	return ret;
}

/* 
 * Give the object to the monster's inventory stack.
 */
bool monster_inven_carry(monster_type * m_ptr, object_type * o_ptr)
{
	if (insert_to_stack(o_ptr, &(m_ptr->inventory)))
	{
		o_ptr->owner = m_ptr;
		o_ptr->stack = STACK_MON_INVEN;

		return TRUE;
	}

	return FALSE;
}



/***************************/


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
		if (!get_obj_num_hook || (*get_obj_num_hook) (table[i].index))
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
		if (table[i].level > level)
			break;

		/* Default */
		table[i].prob3 = 0;

		/* Access the index */
		k_idx = table[i].index;

		/* Access the actual kind */
		k_ptr = &k_info[k_idx];

		/* Hack -- prevent embedded chests */
		if (opening_chest && (k_ptr->tval == TV_CHEST))
			continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Total */
		total += table[i].prob3;
	}

	/* No legal objects */
	if (total <= 0)
		return (0);


	/* Pick an object */
	value = rand_int(total);

	/* Find the object */
	for (i = 0; i < alloc_kind_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3)
			break;

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
			if (value < table[i].prob3)
				break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level)
			i = j;
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
			if (value < table[i].prob3)
				break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level)
			i = j;
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
void object_known(object_type * o_ptr)
{
	/* Remove "default inscriptions" */
	if (o_ptr->note && (o_ptr->ident & (IDENT_SENSE)))
	{
		/* Access the inscription */
		cptr q = quark_str(o_ptr->note);

		/* Hack -- Remove auto-inscriptions */
		if ((streq(q, "cursed")) || (streq(q, "broken")) ||
			(streq(q, "good")) || (streq(q, "average")) ||
			(streq(q, "excellent")) || (streq(q, "worthless")) ||
			(streq(q, "special")) || (streq(q, "terrible")))
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
void object_aware(object_type * o_ptr)
{
	/* Fully aware of the effects */
	k_info[o_ptr->k_idx].aware = TRUE;
}



/*
 * Something has been "sampled"
 */
void object_tried(object_type * o_ptr)
{
	/* Mark it as tried (even if "aware") */
	k_info[o_ptr->k_idx].tried = TRUE;
}



/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
static s32b object_value_base(object_type * o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Aware item -- use template cost */
	if (object_aware_p(o_ptr))
		return (k_ptr->cost);

	/* Analyze the type */
	switch (o_ptr->tval)
	{
			/* Un-aware Food */
		case TV_FOOD:
			return (5L);

			/* Un-aware Potions */
		case TV_POTION:
			return (20L);

			/* Un-aware Scrolls */
		case TV_SCROLL:
			return (20L);

			/* Un-aware Staffs */
		case TV_STAFF:
			return (70L);

			/* Un-aware Wands */
		case TV_WAND:
			return (50L);

			/* Un-aware Rods */
		case TV_ROD:
			return (90L);

			/* Un-aware Rings */
		case TV_RING:
			return (45L);

			/* Un-aware Amulets */
		case TV_AMULET:
			return (45L);

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
static s32b object_value_real(object_type * o_ptr)
{
	s32b value;

	u32b f1, f2, f3;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	if (o_ptr->tval == TV_RANDART)
	{
		return random_artifacts[o_ptr->sval].cost;
	}

	/* Hack -- "worthless" items */
	if (!k_ptr->cost)
		return (0L);

	/* Base cost */
	value = k_ptr->cost;


	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* Hack -- "worthless" artifacts */
		if (!a_ptr->cost)
			return (0L);

		/* Hack -- Use the artifact cost instead */
		value = a_ptr->cost;
	}

	/* Ego-Item */
	else if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		/* Hack -- "worthless" ego-items */
		if (!e_ptr->cost)
			return (0L);

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
			if (o_ptr->pval < 0)
				return (0L);

			/* No pval */
			if (!o_ptr->pval)
				break;

			/* Give credit for stat bonuses */
			if (f1 & (TR1_STR))
				value += (o_ptr->pval * 200L);
			if (f1 & (TR1_INT))
				value += (o_ptr->pval * 200L);
			if (f1 & (TR1_WIS))
				value += (o_ptr->pval * 200L);
			if (f1 & (TR1_DEX))
				value += (o_ptr->pval * 200L);
			if (f1 & (TR1_CON))
				value += (o_ptr->pval * 200L);
			if (f1 & (TR1_CHR))
				value += (o_ptr->pval * 200L);

			/* Give credit for stealth and searching */
			if (f1 & (TR1_STEALTH))
				value += (o_ptr->pval * 100L);
			if (f1 & (TR1_SEARCH))
				value += (o_ptr->pval * 100L);

			/* Give credit for infra-vision and tunneling */
			if (f1 & (TR1_INFRA))
				value += (o_ptr->pval * 50L);
			if (f1 & (TR1_TUNNEL))
				value += (o_ptr->pval * 50L);

			/* Give credit for extra attacks */
			if (f1 & (TR1_BLOWS))
				value += (o_ptr->pval * 2000L);

			/* Give credit for speed bonus */
			if (f1 & (TR1_SPEED))
				value += (o_ptr->pval * 30000L);

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
			if (o_ptr->to_a < 0)
				return (0L);
			if (o_ptr->to_h < 0)
				return (0L);
			if (o_ptr->to_d < 0)
				return (0L);

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
			if (o_ptr->to_a < 0)
				return (0L);

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
			if (o_ptr->to_h + o_ptr->to_d < 0)
				return (0L);

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
			if (o_ptr->to_h + o_ptr->to_d < 0)
				return (0L);

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
 * Helper function for calculating transmutation factors.
 */
static u32b transmute_aux(u32b input, int old, int new, bool zero_p)
{
	int foo = (zero_p ? 0 : 1);

	input = ((input * new) / old);

	if (input < foo)
		input = foo;

	return input;
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
s32b object_value(object_type * o_ptr)
{
	s32b value;

	s32b f1, f2, f3;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	object_flags(o_ptr, &f1, &f2, &f3);

	/* Gold -- always costs the same. */
	if (o_ptr->tval == TV_GOLD)
	{
		return o_ptr->pval;
	}

	/* Known items -- acquire the actual value */
	else if (object_known_p(o_ptr))
	{
		/* Broken items -- worthless */
		if (broken_p(o_ptr))
			return (0L);

		/* Cursed items -- worthless */
		if (cursed_p(o_ptr))
			return (0L);

		/* Real value (see above) */
		value = object_value_real(o_ptr);
	}

	/* Unknown items -- acquire a base value */
	else
	{
		/* Hack -- Felt broken items */
		if ((o_ptr->ident & (IDENT_SENSE)) && broken_p(o_ptr))
			return (0L);

		/* Hack -- Felt cursed items */
		if ((o_ptr->ident & (IDENT_SENSE)) && cursed_p(o_ptr))
			return (0L);

		/* Base value (see above) */
		value = object_value_base(o_ptr);
	}

	/* Apply discount (if any) */
	if (o_ptr->discount)
		value -= (value * o_ptr->discount / 100L);

	/* Apply material factor. */
	if (!(f1 & TR1_TRANSMUTE) && o_ptr->stuff != k_ptr->stuff)
	{
		material *m1_ptr = &materials[k_ptr->stuff];
		material *m2_ptr = &materials[o_ptr->stuff];

		value = transmute_aux(value, m1_ptr->cost_factor, 
				      m2_ptr->cost_factor, TRUE);
	}

	/* Return the final value */
	return (value);
}



/*
 * Determine if the given tval could potantially stack. 
 */
bool tval_can_stack(s16b tval)
{
	switch (tval)
	{
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		case TV_CORPSE:
		case TV_FLASK:
		case TV_SPIKE:
			return TRUE;
			break;

			/* Hack -- torches and such are allowed to be stacked, but will never
			 * be stacked under normal circumstances. */
		case TV_LITE:
			return TRUE;
			break;

		default:
			return FALSE;
	}
}


/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * Most items _do not_ stack; Potions, food and scrolls will, corpses sometimes
 * will. Arrows and bolts will stack only if they are identical.
 */
bool object_similar(object_type * o_ptr, object_type * j_ptr)
{
	int total = o_ptr->number + j_ptr->number;
	bool ret = FALSE;


	/* Require identical object types */
	if (o_ptr->k_idx != j_ptr->k_idx)
		return (0);

	/* Don't combine dead items. */
	if (!o_ptr->k_idx)
		return FALSE;

#if 0
	if (o_ptr->world != j_ptr->world)
		return FALSE;
#endif

	if (o_ptr->stuff != j_ptr->stuff)
		return FALSE;

	/* Analyze the items */
	switch (o_ptr->tval)
	{
			/* Corpses. */
		case TV_CORPSE:
		{
			/* OK if sval and pval match. */
			if (o_ptr->sval != j_ptr->sval || o_ptr->pval != j_ptr->pval)
				return FALSE;

			ret = TRUE;
			break;
		}

			/* Food and Potions and Scrolls */
		case TV_FOOD:
		case TV_POTION:
		case TV_FLASK:
		case TV_SPIKE:
		case TV_SCROLL:
		{
			/* Assume okay */
			ret = TRUE;
			break;
		}

			/* Missiles */
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			/* Require identical knowledge of both items */
			if (object_known_p(o_ptr) != object_known_p(j_ptr))
				return (0);

			/* Require identical "bonuses" */
			if (o_ptr->to_h != j_ptr->to_h)
				return (FALSE);
			if (o_ptr->to_d != j_ptr->to_d)
				return (FALSE);
			if (o_ptr->to_a != j_ptr->to_a)
				return (FALSE);

			/* Require identical "pval" code */
			if (o_ptr->pval != j_ptr->pval)
				return (FALSE);

			/* Require identical "artifact" names */
			if (o_ptr->name1 != j_ptr->name1)
				return (FALSE);

			/* Require identical "ego-item" names */
			if (o_ptr->name2 != j_ptr->name2)
				return (FALSE);

			/* Hack -- Never stack recharging items */
			if (o_ptr->timeout || j_ptr->timeout)
				return (FALSE);

			/* Require identical health. */
			if (o_ptr->chp != j_ptr->chp)
				return FALSE;

			/* Require identical "values" */
			if (o_ptr->ac != j_ptr->ac)
				return (FALSE);
			if (o_ptr->dd != j_ptr->dd)
				return (FALSE);
			if (o_ptr->ds != j_ptr->ds)
				return (FALSE);

			/* Probably okay */
			ret = TRUE;
			break;
		}

		default:
			/* Not okay. */
			return FALSE;
			break;
	}


	/* Hack -- Require identical "cursed" status */
	if ((o_ptr->ident & (IDENT_CURSED)) != (j_ptr->ident & (IDENT_CURSED)))
		return (0);

	/* Hack -- Require identical "broken" status */
	if ((o_ptr->ident & (IDENT_BROKEN)) != (j_ptr->ident & (IDENT_BROKEN)))
		return (0);


	/* Hack -- require semi-matching "inscriptions" */
	if (o_ptr->note && j_ptr->note && (o_ptr->note != j_ptr->note))
		return (0);

	/* Hack -- normally require matching "inscriptions" */
	if (!stack_force_notes && (o_ptr->note != j_ptr->note))
		return (0);

	/* Hack -- normally require matching "discounts" */
	if (!stack_force_costs && (o_ptr->discount != j_ptr->discount))
		return (0);


	/* Maximal "stacking" limit */
	if (total >= MAX_STACK_SIZE)
		return (0);

	return ret;
}


/*
 * Allow one item to "absorb" another, assuming they are similar
 * 
 * Warning: You'll need to delete j_ptr, since it won't be needed anymore.
 *
 */
void object_absorb(object_type * o_ptr, object_type * j_ptr)
{
	int total = o_ptr->number + j_ptr->number;

	/* Add together the item counts. */
	o_ptr->number =
		((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

	/* Add together the weights. */
	o_ptr->weight += j_ptr->weight;

	/* Hack -- blend "known" status */
	if (object_known_p(j_ptr))
		object_known(o_ptr);

	/* Hack -- blend "rumour" status */
	if (j_ptr->ident & (IDENT_RUMOUR))
		o_ptr->ident |= (IDENT_RUMOUR);

	/* Hack -- blend "mental" status */
	if (j_ptr->ident & (IDENT_MENTAL))
		o_ptr->ident |= (IDENT_MENTAL);

	/* Hack -- blend "inscriptions" */
	if (j_ptr->note)
		o_ptr->note = j_ptr->note;

	/* Hack -- could average discounts XXX XXX XXX */
	/* Hack -- save largest discount XXX XXX XXX */
	if (o_ptr->discount < j_ptr->discount)
		o_ptr->discount = j_ptr->discount;
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
		if ((k_ptr->tval == tval) && (k_ptr->sval == sval))
			return (k);
	}

	/* Oops */
	msg_format("No object (%d,%d)", tval, sval);

	/* Oops */
	return (0);
}



/*
 * Prepare an object based on an object kind.
 */
void object_prep(object_type * o_ptr, int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	int i;

	/* Save the kind index */
	o_ptr->k_idx = k_idx;

	/* Efficiency -- tval/sval */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;

	/* Pval will be adjusted by ``apply_magic''. */
	o_ptr->pval = k_ptr->pval;

	o_ptr->discount = 0;

	/* Default number */
	o_ptr->number = 1;

	/* Default weight */
	o_ptr->weight = k_ptr->weight;

	/* Maximum hitpoints. */
	o_ptr->mhp = k_ptr->mhp;
	o_ptr->chp = k_ptr->mhp;

	/* Save stuff. */
	o_ptr->stuff = k_ptr->stuff;

	o_ptr->name1 = 0;
	o_ptr->name2 = 0;

	/* Default magic */
	o_ptr->to_h = k_ptr->to_h;
	o_ptr->to_d = k_ptr->to_d;
	o_ptr->to_a = k_ptr->to_a;

	/* Default power */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	o_ptr->timeout = 0;
	o_ptr->ident = 0;
	o_ptr->marked = 0;
	o_ptr->note = 0;


	/* No flags. */
	o_ptr->flags1 = 0L;
	o_ptr->flags2 = 0L;
	o_ptr->flags3 = 0L;

	/* Unequip the item. */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (o_ptr == equipment[i])
			equipment[i] = NULL;
	}

	/* Hack -- worthless items are always "broken" */
	if (k_ptr->cost <= 0)
		o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags3 & (TR3_LIGHT_CURSE))
		o_ptr->ident |= (IDENT_CURSED);
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
	if (level > MAX_DEPTH - 1)
		level = MAX_DEPTH - 1;


	/* The "bonus" moves towards the max */
	/* bonus = ((max * level) / MAX_DEPTH); */
	bonus = ((2 * max * level) / MAX_DEPTH);

	/* Hack -- determine fraction of error */
	/* extra = ((max * level) % MAX_DEPTH); */
	extra = ((2 * max * level) % MAX_DEPTH);

	/* Hack -- simulate floating point computations */
	if (rand_int(MAX_DEPTH) < extra)
		bonus++;


	/* The "stand" is equal to one quarter of the max */
	stand = (max / 4);

	/* Hack -- determine fraction of error */
	extra = (max % 4);

	/* Hack -- simulate floating point computations */
	if (rand_int(4) < extra)
		stand++;


	/* Choose an "interesting" value */
	value = randnor(bonus, stand);

	/* Enforce the minimum value */
	if (value < 0)
		return 0;

	/* Enforce the maximum value */
	if (value > max)
		return (max);

	/* Result */
	return (value);
}




/*
 * Cheat -- describe a created object for the user
 */
static void object_mention(object_type * o_ptr)
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
 * The core of artifact creation. This allows to create specific
 * artifacts.
 */

bool make_artifact_named(object_type * o_ptr, s16b i, byte depth,
	bool sure)
{
	artifact_type *a_ptr = &a_info[i];

	int special = (a_ptr->flags3 & TR3_SPECIAL);

	/* Paranoia -- no "plural" artifacts */
	if (o_ptr->number != 1)
		return (FALSE);

	/* Skip "empty" items */
	if (!a_ptr->name)
		return FALSE;

	/* Cannot make an artifact twice */
	if (a_ptr->cur_num)
		return FALSE;

	/* Must have the correct fields, if not a ``special''
	 * artifact */

	if (!special)
	{
		if (a_ptr->tval != o_ptr->tval)
			return FALSE;
		if (a_ptr->sval != o_ptr->sval)
			return FALSE;
	}

	/* XXX XXX Enforce minimum "depth" (loosely) */
	if (!sure && a_ptr->level > depth)
	{

		/* Acquire the "out-of-depth factor" */
		int d = (a_ptr->level - depth) * 2;

		/* Roll for out-of-depth creation */
		if (rand_int(d) != 0)
			return FALSE;
	}

	/* We must make the "rarity roll" */
	if (!sure && rand_int(a_ptr->rarity) != 0)
		return FALSE;

	/* Is this a ``special'' artifact? */
	if (special)
	{
		/* Find the base object */
		s16b k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* XXX XXX Enforce minimum "object" level (loosely) */
		if (!sure && k_info[k_idx].level > depth)
		{

			/* Acquire the "out-of-depth factor" */
			int d = (k_info[k_idx].level - depth) * 5;

			/* Roll for out-of-depth creation */
			if (rand_int(d) != 0)
				return FALSE;
		}

		/* Assign the template */
		object_prep(o_ptr, k_idx);

		/* Lite the current location. */
		if (o_ptr->stack == STACK_FLOOR)
		{
			lite_spot(o_ptr->iy, o_ptr->ix);
			note_spot(o_ptr->iy, o_ptr->ix);

		}
		else if (o_ptr->stack == STACK_INVEN)
		{
			p_ptr->window |= (PW_INVEN | PW_EQUIP);
		}
	}

	/* Hack -- mark the item as an artifact */
	o_ptr->name1 = i;

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
	if (!a_ptr->cost)
		o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- extract the "cursed" flag */
	if (a_ptr->flags3 & (TR3_LIGHT_CURSE))
		o_ptr->ident |= (IDENT_CURSED);

	/* Mega-Hack -- increase the rating */
	rating += 10;

	/* Mega-Hack -- increase the rating again */
	if (a_ptr->cost > 50000L)
		rating += 10;

	/* Set the good item flag */
	good_item_flag = TRUE;

	/* Cheat -- peek at the item */
	if (cheat_peek)
		object_mention(o_ptr);

	/* Success */
	return (TRUE);
}

/*
 * Attempt to change an object into an artifact
 *
 * Note: This routine is safe for use anywhere. Also, it will
 * mutate your object if a ``special'' artifact is requested.
 *
 */
bool make_artifact(object_type * o_ptr, byte depth)
{
	int i, foo;

	/* Check the artifact list.
	 *
	 * (See ``make_ego_item'' for rationale.
	 */
	for (foo = 0; foo < 2000; foo++)
	{
		i = rand_int(MAX_A_IDX);
		if (make_artifact_named(o_ptr, i, depth, FALSE))
			return TRUE;
	}

	/* Failure */
	return (FALSE);
}


/*
 * Generate a specific ego-item. 
 */
bool make_ego_item_named(object_type * o_ptr, s16b i, byte depth,
	bool spec)
{
	int j;
	ego_item_type *e_ptr;
	bool sure = FALSE;


	/* Paranoia -- no "plural" ego items */
	if (o_ptr->number != 1)
		return (FALSE);

	e_ptr = &e_info[i];

	/* Skip "empty" items */
	if (!e_ptr->name)
		return FALSE;

	/* Do we want special ego-items? */
	if (e_ptr->flags2 & TR2_SPECIAL_GEN)
	{
		if (!spec)
			return FALSE;
		else
			sure = TRUE;
	}

	/* Object must be the right type. */
	/* You can use 0 to mean ``any''. */

	if (e_ptr->tval && e_ptr->tval != o_ptr->tval)
		return FALSE;
	if (e_ptr->sval && e_ptr->sval != o_ptr->sval)
		return FALSE;

	/* XXX XXX Enforce minimum "depth" (loosely) */
	if (e_ptr->level > depth && !sure)
	{
		/* Acquire the "out-of-depth factor" */
		int d = ABS(e_ptr->level - depth) * 2;

		/* Roll for out-of-depth creation */
		if (rand_int(d) != 0)
			return FALSE;
	}

	/* We must make the "rarity roll" */
	if (rand_int(e_ptr->rarity + 2) != 0 && !sure)
		return FALSE;

	/* Hack -- mark the item as an ego-item */
	o_ptr->name2 = i;

	/* Hack -- acquire "broken" flag */
	if (!e_ptr->cost)
		o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- acquire "cursed" flag */
	if (e_ptr->flags3 & (TR3_LIGHT_CURSE))
		o_ptr->ident |= (IDENT_CURSED);

	/* Hack -- apply extra penalties if needed */
	if (cursed_p(o_ptr) || broken_p(o_ptr))
	{
		/* Hack -- obtain bonuses */
		if (e_ptr->max_to_h)
			o_ptr->to_h -= randint(e_ptr->max_to_h);
		if (e_ptr->max_to_d)
			o_ptr->to_d -= randint(e_ptr->max_to_d);
		if (e_ptr->max_to_a)
			o_ptr->to_a -= randint(e_ptr->max_to_a);

		/* Hack -- obtain pval */
		if (e_ptr->max_pval)
			o_ptr->pval -= randint(e_ptr->max_pval);
	}

	/* Hack -- apply extra bonuses if needed */
	else
	{
		/* Hack -- obtain bonuses */
		if (e_ptr->max_to_h)
			o_ptr->to_h += randint(e_ptr->max_to_h);
		if (e_ptr->max_to_d)
			o_ptr->to_d += randint(e_ptr->max_to_d);
		if (e_ptr->max_to_a)
			o_ptr->to_a += randint(e_ptr->max_to_a);

		/* Hack -- obtain pval */
		if (e_ptr->max_pval)
			o_ptr->pval += randint(e_ptr->max_pval);
	}

	/* Add the random flags. */
	for (j = 0; j < 32; j++)
	{
		if (magik(30))
		{

			switch (randint(3))
			{
				case 1:
					o_ptr->flags1 |= (e_ptr->maybe_flags1 & (1L << j));
					break;

				case 2:
					o_ptr->flags2 |= (e_ptr->maybe_flags2 & (1L << j));
					break;

				case 3:
					o_ptr->flags3 |= (e_ptr->maybe_flags3 & (1L << j));
					break;
			}
		}
	}

	/* Transmute the object, if required. */
	if (e_ptr->flags1 & TR1_TRANSMUTE)
	{
		transmute(o_ptr, e_ptr->stuff);
	}

	/* Mega-hack -- increase the rating. */
	rating += e_ptr->rating;

	/* Set the good item flag. */
	good_item_flag = TRUE;

	/* Cheating options. */
	if (cheat_peek)
		object_mention(o_ptr);

	/* Success */
	return (TRUE);
}



/*
 * Change an item into an ego-item.
 */
bool make_ego_item(object_type * o_ptr, byte depth)
{
	int i, foo;

	/* Paranoia -- no "plural" ego items */
	if (o_ptr->number != 1)
		return (FALSE);

	/* Check the ego-item list 
	 *
	 * Note: We really need to check the list randomly, even though it's
	 * slower. If we don't, then the ego-items that are at the 
	 * front of the list will have a much higher frequency.
	 */
	for (foo = 0; foo < 2000; foo++)
	{
		i = rand_int(MAX_E_IDX);

		if (make_ego_item_named(o_ptr, i, depth, FALSE))
			return TRUE;
	}

	/* Failure */
	return (FALSE);
}


s16b make_corpse_sval = 0;

static bool make_corpse_hook(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Uniques and such don't have corpses. */
	if (r_ptr->flags7 & RF7_NO_CORPSE || r_ptr->flags1 & RF1_UNIQUE)
		return FALSE;

	switch (make_corpse_sval)
	{

			/* Every monster has a body, a heart, and a liver. */
		case SV_CORPSE_BODY:
		case SV_CORPSE_HEART:
		case SV_CORPSE_LIVER:
			return TRUE;
			break;

			/* Headless monsters have no head, tongue, or skin. */
		case SV_CORPSE_HEAD:
		case SV_CORPSE_SKIN:
		case SV_CORPSE_TONGUE:
			if (!(r_ptr->flags7 & RF7_HEADLESS))
				return TRUE;
			break;

			/* Monster has scales. */
		case SV_CORPSE_SCALE:
			if (r_ptr->flags7 & RF7_HAS_SCALES)
				return TRUE;
			break;

			/* Monster has wings. */
		case SV_CORPSE_WING:
			if (r_ptr->flags7 & RF7_HAS_WINGS)
				return TRUE;
			break;

			/* Hair only gets generated for humanoid monsters. */
		case SV_CORPSE_HAIR:
			if (r_ptr->flags7 & RF7_HUMANOID)
				return TRUE;
			break;
	}

	/* Assume that the corpse can't be generated. */
	return FALSE;
}



/*
 * Find an appropriate monster for a generated corpse.
 */
static void make_corpse(object_type * o_ptr, byte depth)
{
	s16b r_idx = 0;

	/* Hack -- corpse's sval. */
	make_corpse_sval = o_ptr->sval;

	/* Hook for figuring out appropriate monster types. */
	get_mon_num_hook = make_corpse_hook;

	/* Prepare the table. */
	get_mon_num_prep();

	/* Get a monster. */
	while (r_idx == 0 && depth < MAX_DEPTH)
	{
		r_idx = get_mon_num(depth);
		depth += 5;
	}

	o_ptr->pval = r_idx;

	/* Undo the filtering. */
	get_mon_num_hook = NULL;
	get_mon_num_prep();
}




/*
 * Apply magic to an item known to be a "weapon"
 *
 */
static void a_m_aux_1(object_type * o_ptr, int level, int power)
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
		if (o_ptr->to_h + o_ptr->to_d < 0)
			o_ptr->ident |= (IDENT_CURSED);
	}

}

/*
 * Apply magic to an item known to be "armor"
 */

static void a_m_aux_2(object_type * o_ptr, int level, int power)
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
		if (o_ptr->to_a < 0)
			o_ptr->ident |= (IDENT_CURSED);
	}
}


/*
 * Tinker with the random artifact to make it acceptable
 * for a certain depth; also connect a random artifact to an 
 * object.
 */

static void finalize_randart(object_type * o_ptr, int lev)
{
	int r;
	int i = 0;
	int foo = lev + randnor(0, 5);
	bool flag = TRUE;

	/* Paranoia */
	if (o_ptr->tval != TV_RANDART)
		return;

	if (foo < 1)
		foo = 1;
	if (foo > 100)
		foo = 100;

	while (flag)
	{
		r = rand_int(MAX_RANDARTS);

		if (!(random_artifacts[r].generated) || i > 2000)
		{
			random_artifact *ra_ptr = &random_artifacts[r];

			o_ptr->sval = r;
			ra_ptr->generated = TRUE;
			flag = FALSE;
		}

		i++;
	}
}

/*
 * Like m_bonus, but with different rules. (i.e. give lots of bonus even
 * at low levels, and really huge bonuses at high levels.
 */
static s16b pval_bonus(int max, int level, s16b k_idx)
{
	int tmp;
	int lev2 = k_info[k_idx].level;
	int stand = 20;

	if (stand == 0)
		stand = 1;

	/* Paranoia -- enforce maximal "level" */
	if (level > MAX_DEPTH - 1)
		level = MAX_DEPTH - 1;

	/* tmp is the percentage of max to keep, with 100% somewhere at lev. 64. */
	tmp = 100 + (level - lev2 / 2) / 2;

	if (tmp * 2 < stand)
		stand = tmp / 2;

	tmp = randnor(tmp, stand);

	return tmp * max / 100;
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
void apply_magic(object_type * o_ptr, int lev, bool okay, bool good,
	bool great)
{
	int i, rolls, rolls2, f1, f2, power, lev_bak = lev;

	u32b fl1, fl2, fl3;

	/* Hack -- split off to corpse generation for corpses. */
	if (o_ptr->tval == TV_CORPSE)
	{
		make_corpse(o_ptr, lev);
		return;
	}

	/* Maximum "level" for various things */
	if (lev > MAX_DEPTH - 1)
		lev = MAX_DEPTH - 1;


	/* Base chance of being "good" */
	f1 = lev + 10;

	/* Maximal chance of being "good" */
	if (f1 > 75)
		f1 = 75;

	/* Base chance of being "great" */
	f2 = f1 / 2;

	/* Maximal chance of being "great" */
	if (f2 > 20)
		f2 = 20;


	/* Assume normal */
	power = 0;

	/* Roll for "good" */
	if (good || magik(f1))
	{
		/* Assume "good" */
		power = 1;

		/* Roll for "great" */
		if (great || magik(f2))
			power = 2;
	}

	/* Roll for "cursed" */
	else if (magik(f1))
	{
		/* Assume "cursed" */
		power = -1;

		/* Roll for "broken" */
		if (magik(f2))
			power = -2;
	}


	/* Assume no rolls */
	rolls = 0;

	/* Get one roll if excellent */
	if (power >= 2)
		rolls = 1;

	/* Hack -- Get four rolls if forced great */
	if (great)
		rolls = 4;

	/* Hack -- Get no rolls if not allowed */
	if (!okay || o_ptr->name1)
		rolls = 0;

	/* Always get one roll for ego-items. */
	rolls2 = 1;

	if (!okay)
		rolls2 = 0;

	/* Add one more if excellent. */
	if (power >= 2)
		rolls2++;

	/* Get four rolls is forced great. */
	if (great)
		rolls2 *= 2;


	/* Roll for artifacts if allowed */
	for (i = 0; i < rolls; i++)
	{
		/* Roll for an artifact */
		if (make_artifact(o_ptr, lev))
			return;
	}

	/* Roll for ego items. */
	if (!o_ptr->name1)
	{
		for (i = 0; i < rolls2; i++)
		{
			make_ego_item(o_ptr, lev);
		}
	}

	/* Get the item's flags. */
	object_flags(o_ptr, &fl1, &fl2, &fl3);

	/* Big boost to pvals. */
	if (fl2 & TR2_EASY_PVAL)
	{
		lev = 100;
	}


	/* Item is intrinsically ``cursed''. */
	if (o_ptr->pval < 0)
	{
		o_ptr->pval = -pval_bonus(-o_ptr->pval, lev, o_ptr->k_idx);

		/* Item is never cursed. */
	}
	else if (fl2 & TR2_UNCURSED)
	{
		o_ptr->pval = pval_bonus(o_ptr->pval, lev, o_ptr->k_idx);

		if (o_ptr->pval <= 0)
			o_ptr->pval = 1;

		if (power < 0)
			power = -power;

		o_ptr->pval *= power + 1;

		/* Item could be potentially cursed. */
	}
	else
	{
		o_ptr->pval = pval_bonus(o_ptr->pval, lev, o_ptr->k_idx);

		if (power < 0)
		{
			o_ptr->pval *= power - 1;

			if (o_ptr->pval >= 0)
				o_ptr->pval = -1;

		}
		else
		{
			o_ptr->pval *= power + 1;

			if (o_ptr->pval <= 0)
				o_ptr->pval = 1;
		}

	}


	if (fl2 & TR2_EASY_PVAL)
	{
		lev = lev_bak;
	}

	/* Apply magic */
	switch (o_ptr->tval)
	{
		case TV_RANDART:
		{
			finalize_randart(o_ptr, lev);
			break;
		}

		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOW:
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			if (power)
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
			if (power)
				a_m_aux_2(o_ptr, lev, power);
			break;
		}
	}


	/* About 10% of the items will be made of a different material. */
	if (magik(10))
	{
		transmute_random(o_ptr, lev);
	}

	/* Twiddle the HP's a bit. */
	o_ptr->chp = (o_ptr->chp * randnor(25, 3)) / 25;

	if (o_ptr->chp <= 0)
		o_ptr->chp = 1;

	/* Examine real objects */
	if (o_ptr->k_idx)
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* Hack -- acquire "broken" flag */
		if (!k_ptr->cost)
			o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- acquire "cursed" flag */
		if (k_ptr->flags3 & (TR3_LIGHT_CURSE))
			o_ptr->ident |= (IDENT_CURSED);
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
			if (k_ptr->to_a < 0)
				return (FALSE);
			return (TRUE);
		}

			/* Weapons -- Good unless damaged */
		case TV_BOW:
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			if (k_ptr->to_h < 0)
				return (FALSE);
			if (k_ptr->to_d < 0)
				return (FALSE);
			return (TRUE);
		}

			/* Ammo -- Arrows/Bolts are good unless damaged. From GJW -KMW- */
		case TV_BOLT:
		case TV_ARROW:
		{
			if (k_ptr->to_h < 0)
				return (FALSE);
			if (k_ptr->to_d < 0)
				return (FALSE);
			return (TRUE);
		}

			/* Rings -- Rings of Speed are good */
		case TV_RING:
		{
			if (k_ptr->sval == SV_RING_SPEED)
				return (TRUE);
			return (FALSE);
		}

			/* Amulets -- Amulets of the Magi are good */
		case TV_AMULET:
		{
			if (k_ptr->sval == SV_AMULET_THE_MAGI)
				return (TRUE);
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
bool make_object(object_type * j_ptr, bool good, bool great)
{
	int base, k_idx;


	/* Base level for the object */
	base = object_level;

	if (good || great)
		base = object_level + 10;

	/* Good objects */
	if (good || great)
	{
		/* Activate restriction */
		get_obj_num_hook = kind_is_good;

		/* Prepare allocation table */
		get_obj_num_prep();
	}

	/* Pick a random object */
	k_idx = get_obj_num(base);

	/* Good objects */
	if (good || great)
	{
		/* Clear restriction */
		get_obj_num_hook = NULL;

		/* Prepare allocation table */
		get_obj_num_prep();
	}

	/* Handle failure */
	if (!k_idx)
		return (FALSE);

	/* Prepare the object */
	object_prep(j_ptr, k_idx);

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
		if (cheat_peek)
			object_mention(j_ptr);
	}

	/* Success */
	return (TRUE);
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
void drop_near(object_type * j_ptr, bool do_dam, int y, int x)
{
	int i, d;

	int ty = 0, tx = 0;

	char o_name[80];

	bool flag = FALSE;

	bool plural = FALSE;

	bool hack_dead = FALSE;

	/* Extract plural */
	if (j_ptr->number != 1)
		plural = TRUE;

	/* Describe object */
	object_desc(o_name, j_ptr, FALSE, 0);


	/* Try to drop it in a sane way first. */
	if (in_bounds_fully(y, x) && cave_floor_bold(y, x))
	{

		ty = y;
		tx = x;

	}
	else
	{

		d = 1;

		/* Scan local grids */
		for (i = 0; d <= 100; i++)
		{
			ty = randnor(y, d);
			tx = randnor(x, d);

			/* OK, too many tries -- bail out. */
			if (d > 100)
				break;

			/* Try ten times. */
			if (i % 10 == 0)
				d++;

			/* Skip illegal grids */
			if (!in_bounds_fully(ty, tx))
				continue;

			/* Require line of sight */
			if (!los(y, x, ty, tx))
				continue;

			/* Require floor space (or shallow terrain) -KMW- */
			if (!cave_floor_bold(ty, tx))
				continue;

			flag = TRUE;
			break;
		}


		/* Handle lack of space */
		if (!flag && !artifact_p(j_ptr))
		{
			/* Message */
			msg_format("The %s disappear%s.", o_name, (plural ? "" : "s"));

			/* Debug */
			if (p_ptr->wizard)
				msg_print("Breakage (no floor space).");

			remove_object(j_ptr);
			/* Failure */
			return;
		}


		/* Find a grid */
		for (i = 0; !flag; i++)
		{
			/* Bounce around */
			if (i < 1000)
			{
				ty = rand_spread(y, 1);
				tx = rand_spread(x, 1);
			}

			/* Random locations */
			else
			{
				ty = rand_int(DUNGEON_HGT);
				tx = rand_int(DUNGEON_WID);
			}

			/* Require floor space or shallow terrain -KMW- */
			if (!cave_floor_bold(ty, tx))
				continue;

			/* Require floor space */
			if (!cave_clean_bold(ty, tx))
				continue;

			/* Okay */
			flag = TRUE;
			break;
		}
	}

	/* Handle normal "breakage" */
	if (do_dam && hates_impact(j_ptr))
	{
		/* Hack. */
		j_ptr->iy = ty;
		j_ptr->ix = tx;
		j_ptr->stack = STACK_NONE;

		hack_dead = object_take_hit(j_ptr, damroll(2, 5), NULL);
	}

	/* Item isn't dead yet. */
	if (!hack_dead)
	{

		/* Give it to the floor */
		floor_carry(ty, tx, j_ptr);

		/* Sound */
		sound(SOUND_DROP);

		/* Message when an object falls under the player */
		if (cave_m_idx[ty][tx] < 0)
		{
			mprint(MSG_BONUS,
				"You feel something roll beneath your feet.");
		}
	}
}


/*
 * Scatter some "great" objects near the player
 */
void acquirement(int y1, int x1, int num, bool great)
{
	object_type *i_ptr;

	/* Acquirement */
	while (num--)
	{
		/* Allocate space for the new object. */
		i_ptr = new_object();

		/* Make a good (or great) object (if possible) */
		if (!make_object(i_ptr, TRUE, great))
			continue;

		/* Drop the object */
		drop_near(i_ptr, FALSE, y1, x1);
	}
}


/*
 * Attempt to place an object (normal or good/great) at the given location.
 */
void place_object(int y, int x, bool good, bool great)
{
	object_type *i_ptr;

	bool quest_in_progress = FALSE;

	/* Set to true if the quest has been visited at least once. */
	if (p_ptr->which_quest &&
		quest_status[p_ptr->which_quest - 1] == QUEST_IN_PROGRESS)
	{
		quest_in_progress = TRUE;
	}

	/* Paranoia */
	if (!in_bounds(y, x))
		return;

	/* Mega-hack -- No objects if in quest for the second time! */
	if (p_ptr->inside_special == SPECIAL_QUEST && quest_in_progress)
		return;

	/* Allocate space for the object. */
	i_ptr = new_object();

	/* Make an object (if possible) */
	if (make_object(i_ptr, good, great))
	{

		/* Drop the object */
		drop_near(i_ptr, FALSE, y, x);
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

	/* Paranoia */
	if (cave_feat[y][x] != FEAT_INVIS)
		return;

	/* Pick a trap */
	while (1)
	{
		/* Hack -- pick a trap */
		feat = FEAT_TRAP_HEAD + rand_int(16);

		/* Hack -- no trap doors on quest levels,
		 * bottoms of dungeons or wilderness. */
		if ((feat == FEAT_TRAP_HEAD + 0x00) &&
			(p_ptr->inside_special == SPECIAL_QUEST ||
				p_ptr->depth >= MAX_DEPTH - 1 ||
				p_ptr->inside_special == SPECIAL_WILD)) continue;

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
	if (!in_bounds(y, x))
		return;

	/* Require empty, clean, floor grid */
	if (!cave_naked_bold(y, x))
		return;

	/* Place an invisible trap */
	cave_set_feat(y, x, FEAT_INVIS);
}


/*
 * Combine items in the pack
 */
void combine_pack(void)
{
	object_type *iter1;
	object_type *iter2;
	object_type *tmp;

	bool flag = FALSE;


	/* Scan every item */
	for (iter1 = inventory; iter1 != NULL; iter1 = iter1->next)
	{
		/* Scan every following item */
		for (iter2 = iter1; iter2->next != NULL; iter2 = iter2->next)
		{
			tmp = iter2->next;

			/* Don't combine worlds */
			if (iter1->world != tmp->world) continue;
	
			/* Compare the objects */
			if (object_similar(iter1, tmp))
			{
				/* Combine them. */
				object_absorb(iter1, tmp);

				/* Delete absorbed object */
				remove_object(tmp);

				/* Take note */
				flag = TRUE;
			}
		}
	}

	/* An item combined */
	if (flag)
	{
		/* Window stuff */
		p_ptr->window |= (PW_INVEN);

		/* Message */
		mprint(MSG_TEMP, "You combine some items in your pack.");
	}
}


/*
 * Reorder items in the pack
 */
void reorder_pack(void)
{
	object_type *o_ptr, *o_nxt;
	object_type *j_ptr;

	bool flag = FALSE;


	/* Scan every item */
	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_nxt)
	{
		/* Access the "next" item */
		o_nxt = o_ptr->next;

		/* Scan every item */
		for (j_ptr = inventory; j_ptr != NULL; j_ptr = j_ptr->next)
		{
			if (o_ptr == j_ptr)
				break;

			if (stop_sorting_objects(o_ptr, j_ptr))
				break;
		}

		/* Never move down */
		if (o_ptr == j_ptr) continue;

		/* Unlink */
		link_remove(o_ptr);

		/* Link */
		link_insert(j_ptr->prev, j_ptr, o_ptr);

		/* Take note */
		flag = TRUE;
	}

	/* An item moved */
	if (flag)
	{
		/* Window stuff */
		p_ptr->window |= (PW_INVEN);

		/* Message */
		mprint(MSG_TEMP, "You reorder some items in your pack.");
	}
}


/***************** Monster generator functions ***/


/*
 * Place a monster generator in the dungeon.
 */
void create_generator(s16b r_idx, s16b y, s16b x)
{
	generator *g_ptr;
	int i;

	/* Check to see if generator exists */
	for (i = 0; i < m_generators; i++)
	{
		g_ptr = &gen_list[i];

		if (g_ptr->y == y && g_ptr->x == x &&
			g_ptr->inside_special == p_ptr->inside_special) return;
	}

	/* Limit reached */

	if (m_generators >= MAX_GENERATORS)
		return;

	g_ptr = &gen_list[m_generators++];

	g_ptr->r_idx = r_idx;
	g_ptr->timeout = 0;
	g_ptr->y = y;
	g_ptr->x = x;

	g_ptr->inside_special = p_ptr->inside_special;
	g_ptr->extra = 0;
}


/*
 * Activate all the generators. (At once.)
 */

void activate_generators(void)
{
	int d, h, i, j, x, y;
	int loc_y, loc_x, r_idx;
	int num;

	generator *g_ptr;

	bool did_summ = FALSE;

	for (h = 0; h < m_generators; h++)
	{
		g_ptr = &gen_list[h];

		r_idx = g_ptr->r_idx;
		loc_y = g_ptr->y;
		loc_x = g_ptr->x;

		/* Check that we're on the right level. */
		if (g_ptr->inside_special != p_ptr->inside_special)
			continue;

		if (g_ptr->timeout == 0)
		{
			num = randint(10);
			did_summ = TRUE;

			/* Number of monsters summoned. */
			for (i = 0; i < num; i++)
			{

				/* Look for a location */
				for (j = 0; j < 20; j++)
				{

					/* Pick a distance */
					d = (i / 15) + 1;

					/* Pick a location */
					scatter(&y, &x, loc_y, loc_x, d, 0);

					/* Require "empty" floor grid */
					if (!cave_empty_bold(y, x))
						continue;

					/* Hack -- no summon on glyph of warding */
					if (cave_feat[y][x] == FEAT_GLYPH)
						continue;

					/* Okay */
					break;
				}

				place_monster_aux(y, x, r_idx, 0);
			}

			g_ptr->timeout = 100;
		}
	}

	if (did_summ)
	{
		mprint(MSG_WARNING, "You hear a shrill whistling sound.");
	}
}


/* 
 * Decrease generator timeout.
 */

void process_generators(void)
{
	int i;
	generator *gen;

	for (i = 0; i < m_generators; i++)
	{
		gen = &gen_list[i];

		if (gen->timeout > 0)
		{
			gen->timeout--;
		}
	}
}



/*
 * Explode an object -- potions, wands, staves, rods and flasks explode.
 * Note the special handling for flasks.
 *
 * Also note the way ``dead'' items are handled. Instead of deleting them
 * outright, we flag them ``dead'' and delete them in the main loop.
 *
 * The reason for this is somewhat complicated -- when objects explode, they
 * affect other objects, which in turn could affect yet more objects. In 
 * effect, an intricate recursive loop is formed. The problem occurs when
 * an exploding object loops through a list (say, player inventory) exploding
 * yet more objects. These newly exploded objects affect deleted elements 
 * on the original loop. The original object then segfaults when it tries to
 * explode an already deleted item.
 *
 * There's probably a less hackish way to handle this, but the way it's done
 * gives maximum flexibility.
 */
bool explode_object(object_type * o_ptr, int y, int x)
{
	char o_name[80];

	int k_idx = 0;

	u32b fl1, fl2, fl3;

	if (!o_ptr->k_idx)
	{
		return FALSE;
	}

	object_desc(o_name, o_ptr, FALSE, 3);

	object_flags(o_ptr, &fl1, &fl2, &fl3);

	/* Item is ``dead''. */
	k_idx = o_ptr->k_idx;
	o_ptr->k_idx = 0;

	p_ptr->notice |= PN_CLEAN_EXPLOSION;


	/* MEGA-HACK: Handle fate here. */
	fate_effect(o_ptr->fate, FATE_KILL);


	if (!(fl1 & TR1_EXPLODES) && o_ptr->tval != TV_POTION &&
		o_ptr->tval != TV_WAND && o_ptr->tval != TV_STAFF &&
		o_ptr->tval != TV_ROD && o_ptr->tval != TV_FLASK)
	{
		return FALSE;
	}

	mformat(MSG_WARNING, "The %s explode%s!", o_name,
		(o_ptr->number > 1 ? "" : "s"));

	if (o_ptr->tval == TV_FLASK)
	{
		fire_explosion(y, x, GF_FIRE, 5, o_ptr->number * damroll(2, 6));
	}
	else
	{
		object_kind *k_ptr = &k_info[k_idx];
		spell *sp_ptr = &activations[k_ptr->activation];
		proj_node *pnode;

		for (pnode = sp_ptr->proj_list; pnode != NULL; pnode = pnode->next)
		{
			fire_explosion(y, x, pnode->attack_kind, ((pnode->radius &&
						pnode->proj_flags & PROJECT_STOP) ? pnode->
					radius : 4), o_ptr->number * damroll(pnode->dam_dice,
					pnode->dam_sides));
		}
	}

	return TRUE;
}

/*
 * Damage the given object. If the object has absorbed any other objects,
 * damage those too. If any object runs out of HP, destroy it. Return the
 * number of objects destroyed.
 *
 * The parameter "verb" indicates what message to print.
 *
 * i.e. if verb == "burned" then the message "10 of your scrolls burned!"
 * will be printed.
 *
 * Another example:  verb == "broke"; "Your Sword of Melting (+3, +5) broke!"
 *
 * Note that artifacts don't normally get destroyed, but ego items are 
 * treated like normal objects.
 *
 * Return true if the given object was deleted. (IMPORTANT!)
 */

bool object_take_hit(object_type * o_ptr, s16b dam, cptr verb)
{
	int amt = 0;
	char o_name[80];
	bool ret = FALSE;
	bool hacky_damage = FALSE;
	cptr pron;

	s16b slot;

	/* Paranoia */
	if (!o_ptr || !o_ptr->k_idx || dam <= 0)
		return FALSE;

	slot = wield_slot(o_ptr);

	if (o_ptr->name2 != EGO_DAMAGED && (slot == EQUIP_WIELD ||
			slot >= EQUIP_BODY) && o_ptr->number == 1)
	{
		hacky_damage = TRUE;
	}

	/* Artifacts don't get destroyed, usually. */
	if (artifact_p(o_ptr))
	{
		/* If the option is enabled, artifacts get worn out 10 times as slow
		 * as regular objects. Note that the figure 10 is completely arbitrary. */
		if (artifact_damage)
		{
			dam /= 10;

			if (dam <= 0)
				return FALSE;
		}
		else
		{
			return FALSE;
		}
	}

	/* Loop until either the damage or the objects run out. */
	while (TRUE)
	{
		/* Kill it. */
		if (dam > o_ptr->chp)
		{
			dam -= o_ptr->chp;
			o_ptr->chp = o_ptr->mhp;
			amt++;
		}
		else
		{
			o_ptr->chp -= dam;
			break;
		}

		/* No more damage. */
		if (dam <= 0)
			break;
	}

	/* Something was destroyed. */
	if (amt > 0)
	{
		object_type *i_ptr;
		int wx, wy;

		object_desc(o_name, o_ptr, TRUE, 3);

		if (o_ptr->stack == STACK_INVEN)
		{
			pron = "your";

			wy = p_ptr->py;
			wx = p_ptr->px;
		}
		else if (o_ptr->stack == STACK_MON_INVEN)
		{
			wy = o_ptr->owner->fy;
			wx = o_ptr->owner->fx;
			pron = "the";
		}
		else
		{
			wy = o_ptr->iy;
			wx = o_ptr->ix;
			pron = "the";
		}

		/* Destroy a few items. */
		if (amt < o_ptr->number)
		{
			i_ptr = object_unabsorb(o_ptr, amt);

			if (verb == NULL)
			{
				verb = (amt > 1) ? "were destroyed" : "was destroyed";
			}

			if ((o_ptr->marked && o_ptr->stack == STACK_FLOOR) ||
				o_ptr->stack == STACK_INVEN)
			{
				mformat(MSG_WARNING, "%d of %s %s %s!", amt, pron, o_name,
					verb);
				disturb(0, 0);
			}
		}
		else
		{
			i_ptr = o_ptr;

			if (verb == NULL)
			{
				verb =
					(o_ptr->number >
					1) ? "were destroyed" : "was destroyed";
			}

			if ((o_ptr->marked && o_ptr->stack == STACK_FLOOR) ||
				o_ptr->stack == STACK_INVEN)
			{
				mformat(MSG_WARNING, "%^s %s!", o_name, verb);
				disturb(0, 0);
			}

			ret = TRUE;
		}

		/* Hack -- convert the item to a ``damaged'' ego-item if it's
		 * a weapon or armor. */
		if (hacky_damage)
		{
			make_ego_item_named(i_ptr, EGO_DAMAGED, 100, TRUE);

			object_known(i_ptr);
			ret = FALSE;
		}
		else
		{
			explode_object(i_ptr, wy, wx);
		}

		/* Calculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP | PW_INVEN | PW_SPELL | PW_PLAYER);

		note_spot(wy, wx);
		lite_spot(wy, wx);

		handle_stuff();
	}

	return ret;
}



/*
 * Repair an item if it is damaged. 
 *
 * ``Damaged'' items will be converted to plain old items, without any
 * special bonuses.
 *
 * Return TRUE if something was repaired.
 */

bool repair_object(object_type * o_ptr, s16b dam)
{
	/* Paranoia. */
	if (!o_ptr || !o_ptr->k_idx || dam <= 0 || o_ptr->chp > o_ptr->mhp)
		return FALSE;

	/* Is this a ``damaged'' ego-item? */
	if (o_ptr->name2 == EGO_DAMAGED)
	{
		/* Make a brand-new item. */
		object_prep(o_ptr, o_ptr->k_idx);

		/* Was the item fully repaired? */
		if (dam < o_ptr->mhp)
		{
			o_ptr->chp = dam;
		}

	}
	else
	{
		if (o_ptr->chp + dam > o_ptr->mhp)
		{
			o_ptr->chp = o_ptr->mhp;

		}
		else
		{
			o_ptr->chp += dam;
		}
	}

	/* Success. */
	return TRUE;
}



/*
 * Convert an item from one material to another.
 */
bool transmute(object_type * o_ptr, byte stuff)
{
	int i;
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	material *m1_ptr = &materials[k_ptr->stuff];
	material *m2_ptr = &materials[stuff];

	if (artifact_p(o_ptr))
		return FALSE;

	/* Don't transmute gold or ingredients. (They are ``pure''.) */
	if (o_ptr->tval == TV_GOLD || o_ptr->tval == TV_INGRED)
		return FALSE;

	if (o_ptr->stuff == stuff)
		return FALSE;

	o_ptr->stuff = stuff;

	o_ptr->mhp =
		transmute_aux(k_ptr->mhp, m1_ptr->hp_factor, m2_ptr->hp_factor,
		FALSE);

	/* Hack -- preserve total weight. */
	if (o_ptr->stack == STACK_INVEN) {
	  p_ptr->total_weight -= o_ptr->weight;
	}


	o_ptr->weight =
	  transmute_aux(k_ptr->weight * o_ptr->number, m1_ptr->weight_factor,
			m2_ptr->weight_factor, FALSE);

	if (o_ptr->stack == STACK_INVEN) {
	  p_ptr->total_weight += o_ptr->weight;
	}

	/* Affect armor class if it's armor. */
	if (wield_slot(o_ptr) >= EQUIP_BODY) {
	  o_ptr->ac = transmute_aux(k_ptr->ac, m1_ptr->ac_factor, 
				    m2_ptr->ac_factor, FALSE);

	  for (i = 0; i < EQUIP_MAX; i++) {

	    if (equipment[i] == o_ptr) {
	      p_ptr->update |= (PU_BONUS | PU_MANA);
	      p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	      break;
	    }
	  }
	}

	/* Hack -- automatically heal the item. */
	o_ptr->chp = o_ptr->mhp;

	return TRUE;
}


/*
 * Convert an item from one material to another random material.
 */
bool transmute_random(object_type * o_ptr, int lev)
{
	int i, j, k;

	/* Number of times to try. */
	k = (lev / 5) + randint(5);

	for (j = 0; j < k; j++)
	{
		i = rand_int(STUFF_MAX);

		/* Make the rarity roll. */
		if (rand_int(materials[i].rarity) == 0)
		{
			return transmute(o_ptr, i);
		}
	}

	return FALSE;
}


void link_error(void)
{
	plog("link_error");
	*(char *) 0 = 0;
	quit("link_error");
}

/* Insert c between a and b */
void link_insert(object_type *a, object_type *b, object_type *c)
{
	if (a && !a->in_list) link_error();
	if (b && !b->in_list) link_error();
	if (c->in_list) link_error();

	if (a) a->next = c;
	if (b) b->prev = c;
	c->prev = a;
	c->next = b;

	c->in_list = TRUE;
}

void link_remove(object_type *a)
{
	if (!a->in_list) link_error();

	if (a->prev) a->prev->next = a->next;
	if (a->next) a->next->prev = a->prev;
	a->prev = NULL;
	a->next = NULL;

	a->in_list = FALSE;
}

void link_insert_glob(object_type *a, object_type *b, object_type *c)
{
	if (a && !a->in_global) link_error();
	if (b && !b->in_global) link_error();
	if (c->in_global) link_error();

	if (a) a->next_global = c;
	if (b) b->prev_global = c;
	c->prev_global = a;
	c->next_global = b;

	c->in_global = TRUE;
}

void link_remove_glob(object_type *a)
{
	if (!a->in_global) link_error();

	if (a->prev_global) a->prev_global->next_global = a->next_global;
	if (a->next_global) a->next_global->prev_global = a->prev_global;
	a->prev_global = NULL;
	a->next_global = NULL;

	a->in_global = FALSE;
}

