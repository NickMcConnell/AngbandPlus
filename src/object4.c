/* File: object4.c */


/* Purpose: misc code for objects - low level: deleting, reordering etc. */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

void set_floor_item(s16b item, s16b value)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][py][px];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item] = value;
   }
   else
   {
      c_ptr->i_idx=value;
   }
}

void set_floor_item_xy(s16b item, s16b value, s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item] = value;
   }
   else if (item!=0)
   {
       quit(format("object4.c: set_floor_item_xy: setting unused entry %d value %d at x %d y %d\n",item,value,x,y));
   }
   else
   {
      c_ptr->i_idx=value;
   }
}

s16b get_floor_item(s16b item)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][py][px];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      return (is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item]);
   }
   else if (item!=0)
   {
      quit(format("object4.c: get_floor_item: requested unused entry %d\n",item));
      return(-1); /* else gcc complains */
   }
   else
   {
      return (c_ptr->i_idx);
   }
}

s16b get_floor_item_xy(s16b item, s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      return (is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item]);
   }
   else
   {
      return (c_ptr->i_idx);
   }
}

vptr get_item_pointer(s16b item)
{
   if (item < INVEN_TOTAL)
   {
      return (&inventory[item]);
   }
   else
   {
      cave_cell_type *c_ptr = &dungeon.level[sublevel][py][px];
      if (c_ptr->i_idx & ITEM_SET_FLAG)
      {
         return (&i_list[is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item-INVEN_TOTAL]]);
      }
      else if (item!=INVEN_TOTAL)
      {
         quit(format("object4.c: get_item_pointer: requested unused pointer %d\n",item));
         return(NULL); /* else gcc complains */
      }
      else
      {
         return (&i_list[c_ptr->i_idx]);
      }
   }
}

vptr get_item_pointer_xy(s16b item, s16b x, s16b y)
{
   if (item < INVEN_TOTAL)
   {
      return (&inventory[item]);
   }
   else
   {
      cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
      if (c_ptr->i_idx & ITEM_SET_FLAG)
      {
         return (&i_list[is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item-INVEN_TOTAL]]);
      }
      else if (item!=INVEN_TOTAL)
      {
         quit(format("object4.c: get_item_pointer_xy: requested unused item %d at x %d y %d\n",item,x,y));
         return(NULL); /* else gcc complains */
      }
      else
      {
         return (&i_list[c_ptr->i_idx]);
      }
   }
}

vptr get_item_pointer_floor(s16b item)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][py][px];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      return (&i_list[is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item]]);
   }
   else if (item!=0)
   {
      quit(format("object4.c: get_item_pointer_floor: requested unused item %d\n",item));
      return(NULL); /* else gcc complains */
   }
   else
   {
      return (&i_list[c_ptr->i_idx]);
   }
}

object_type *get_item_pointer_floor_xy(s16b item, s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      return (&i_list[is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item]]);
   }
   else if (item!=0)
   {
      quit(format("object4.c: get_item_pointer_floor_xy: requested unused item %d at x %d y %d\n",item,x,y));
      return(NULL); /* else gcc complains */
   }
   else
   {
      return (&i_list[c_ptr->i_idx]);
   }
}

bool floor_item(s16b item)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][py][px];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      return (is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item]!=0);
   }
   else
   {
      if (item==0)
      {
         return (c_ptr->i_idx!=0);
      }
      else
      {
         return (FALSE);
      }
   }
}

bool floor_item_xy(s16b item, s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      return (is_list[c_ptr->i_idx & ITEM_SET_MASK].index[item]!=0);
   }
   else
   {
      if (item==0)
      {
         return (c_ptr->i_idx!=0);
      }
      else
      {
         return (FALSE);
      }
   }
}

void wipe_floor_items_traps(s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      is_list[c_ptr->i_idx & ITEM_SET_MASK].inuse = FALSE;
   }
   c_ptr->i_idx=0;
   c_ptr->t_idx=0;
}

/*
 * Delete a dungeon object
 */
/* jk - i says which object n the floor */
/* if i_idx is passed on and looked up here, something is done double? */

void delete_object_idx(s16b i_idx)
{
   object_type *i_ptr = &i_list[i_idx];
   s16b x = i_ptr->ix;
   s16b y = i_ptr->iy;
   s16b z = i_ptr->iz;
/* jk */
   s16b i,j;

   /* object doesn't have to be on this sublevel! */
   cave_cell_type *c_ptr = &dungeon.level[z][y][x];

   if (c_ptr->i_idx & ITEM_SET_FLAG)
   {
      item_set_type *is_ptr = &is_list[c_ptr->i_idx & ITEM_SET_MASK];
      s16b           cnt = 0;
      s16b           where = 0;

       /* Object is gone */
  /* jk - be sure to wipe to correct one, which has the correct entry */
      i=0;
      while (is_ptr->index[i]!=i_idx)
      {
         i++;
         if (i==ITEM_SET_SIZE)
         {
dlog(DEBUGALWAYS,"object4.c: delete_object_idx: deleting non-existing object %d at x,y,z %d,%d,%d",
                  i_idx, x, y, z);
            return;
         }
      }

      is_ptr->index[i]=0;
      /* now shift the rest down */
      for (j=i;j<ITEM_SET_SIZE-1;j++)
      {
         is_ptr->index[j]=is_ptr->index[j+1];
      }
      /* and zero the top one */
      is_ptr->index[ITEM_SET_SIZE-1]=0;

      /* now see if we need the set again */
      for (j=0;j<ITEM_SET_SIZE;j++)
      {
         if (is_ptr->index[j]!=0) /* count the used entries */
         {
            cnt++;
            where = j;             /* remember where incase it's only 1 */
         }
      }
      /* if cnt==0, a previous time cnt==1 should have happened */
      if (cnt==0)
      {
         is_ptr->inuse = FALSE;
         c_ptr->i_idx = 0;
      }
      if (cnt==1)
      {
         is_ptr->inuse = FALSE;   /* ready to be reused */
         c_ptr->i_idx = is_ptr->index[where];
      }
   }
   else
   {
      if (c_ptr->i_idx != i_idx)
      {
dlog(DEBUGALWAYS,"object4.c: delete_object_idx: deleting non-existing object %d at x,y %d,%d",
               i_idx, x, y);
         return;
      }
      c_ptr->i_idx = 0;
   }
    /* Visual update - if it's on the same level as us! */
   if (z == sublevel) lite_spot(x, y);

   /* Wipe the object */
   if (i_ptr->spell_set)
   {
dlog(DEBUGITEMS,"object4.c: delete_object_idx: wiping spell set %d\n",
                i_ptr->spell_set);
      s_list[i_ptr->spell_set].inuse = FALSE;
   }
   WIPE(i_ptr, object_type);
}

/*
 * Deletes object from given location
 */
/* jwk - i says which object */
void delete_object(s16b x, s16b y, s16b item)
{
   cave_cell_type *c_ptr;

   /* Refuse "illegal" locations */
   if ((item<-1) || (item>=MAX_FLOOR_ITEMS)) return;

   if (!in_bounds(x, y)) return;

   /* Find where it was */
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Delete the object */
/* jk - -1 means delete all */
   if (item==-1)
   {
      s16b j;
      for (j=objects_on_floor(x,y)-1;j>=0;j++)
      {
         delete_object_idx(get_floor_item_xy(j,x,y));
      }
   }
   else
   {
      if (floor_item_xy(item,x,y))
      {
         delete_object_idx(get_floor_item_xy(item,x,y));
      }
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
void compact_objects(s16b size, bool monsters_too)
{
   s16b                 i, x, y, z, num, cnt;
/* jk */
   s16b j,m_idx;

   s16b                 cur_lev, cur_dis, chance;

dlog(DEBUGITEMS,"object4.c: compact_objects: size %d monsters %d\n",
                size, monsters_too);
   /* Compact */
   if (size)
   {
      /* Message */
      msg_print("Compacting objects...");

      /* Redraw map */
      p_ptr->redraw1 |= (PR1_MAP);

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
      for (i = 1; i < i_max; i++)
      {
         object_type *i_ptr = &i_list[i];

         object_kind *k_ptr = &k_info[i_ptr->k_idx];

         m_idx = -1; /* assume it's not in a monster_inventory */
         /* Skip dead objects */
         if (!i_ptr->k_idx) continue;
         /* Hack -- High level objects start out "immune" */
         if (k_ptr->level > cur_lev) continue;
         /* Get the location */
         x = i_ptr->ix;
         y = i_ptr->iy;
         z = i_ptr->iz;

         if (x==-1)
         {
            /* this item belongs to a monster */
            if (!monsters_too) continue;
            m_idx = y;
            if (m_idx==-1)
            {
               quit("object with ix -1 not in monster inventory!");
            }
dlog(DEBUGITEMS,"object4.c: compact_objects: item %d k_idx %d (%s) monster %d (%s) @ %d,%d,%d\n",
               i, i_ptr->k_idx, k_name + k_info[i_ptr->k_idx].name, m_idx,
               r_name + r_info[mn_list[m_idx].r_idx].name,
               mn_list[m_idx].fx, mn_list[m_idx].fy, mn_list[m_idx].fz);
            x = mn_list[m_idx].fx; /* use these values to compact! */
            y = mn_list[m_idx].fy;
            z = mn_list[m_idx].fz;
         }

         /* Nearby objects start out "immune" */
         if ((cur_dis > 0) && (distance(px, py, x, y) < cur_dis)) continue;
         /* Saving throw */
         chance = 90;

         /* Hack -- only compact artifacts in emergencies */
         if (artifact_p(i_ptr) && (cnt < 1000)) chance = 100;

         /* Apply the saving throw */
         if (rand_int(100) < chance) continue;

         /* jk - don't compact objects in monster inventories unless */
         /* asked to (after writing them to disk in save.c */
         if (m_idx!=-1)
         {
            s16b is_idx = item_set_this_monster(m_idx);
            s16b num_in_set = items_in_set(is_idx);
            s16b k;
dlog(DEBUGITEMS,"object4.c: compact_objects: item %d (%s) removed in inventory of monster %d (%s)\n",
               i, k_name + k_info[i_ptr->k_idx].name, m_idx,
               r_name + r_info[mn_list[m_idx].r_idx].name);

            for (k=0; k < num_in_set; k++)
            {
               if (is_list[is_idx].index[k] == i)
               {
                  break;
               }
            }
            if (k == num_in_set)
            {
dlog(DEBUGEXTRA,"object4.c: compact_objects: item %d (%s) to be removed from monster %d (%s)\n",
               i, k_name + k_info[i_ptr->k_idx].name, m_idx,
               r_name + r_info[mn_list[m_idx].r_idx].name);
dlog(DEBUGEXTRA,"object4.c: item_set %d, number %d but no entry in item_set found pointing at item? Stack Dumped!\n",
               is_idx, i);
               msg_print("Serious inconsistency discovered while compactig objects - dumping stack");
               dump_stack(NULL);
            }
            else
            {
               monster_inven_increase(m_idx, is_idx, i, -99);
            }
         }
         else                       /* we may not touch monster_inven */
         {                          /* items, so continue */
            delete_object_idx(i);
         }

         /* Count it */
         num++;
      }
   }

dlog(DEBUGITEMS,"object4.c: compact_objects: excising dead objects\n");
   /* Excise dead objects (backwards!) */
   for (i = i_max - 1; i >= 1; i--)
   {
      /* Get the i'th object */
      object_type *i_ptr = &i_list[i];

      /* Skip real objects */
      if (i_ptr->k_idx) continue;

dlog(DEBUGITEMS,"object4.c: compact_objects: dead object @ %d i_max %d\n", i, i_max);
      /* One less object */
      i_max--;

      /* Reorder */
      if (i != i_max)
      {
         /* i is the dead object, i_max the live one          */
         /* find any references to i_max and change them to i */
         s16b nx = i_list[i_max].ix;
         s16b ny = i_list[i_max].iy;
         s16b nz = i_list[i_max].iz;
dlog(DEBUGITEMS,"object4.c: compact_objects: dead object %d @ %d,%d,%d\n", i, nz, nx, ny);

         /* is it on the floor? */
         if (nx!=-1)
         {
            s16b old_sublevel = -1;

            if (!dungeon.level_used[nz])
            {
               quit("object4.c: compact_objects: object exists on unknown sublevel!\n");
            }
            else
            {
               old_sublevel = sublevel;
               sublevel = nz;
            }

            for (j=0;j<objects_on_floor(nx,ny);j++)
            {
               if (get_floor_item_xy(j,nx,ny)==i_max)
               {
dlog(DEBUGITEMS,"object4.c: compact_objects: dead object found as item %d on floor, replacing it with object %d\n",
                j, i);
                  set_floor_item_xy(j,i,nx,ny);
                  break;
               }
            }
            if (nz)
            {
               sublevel = old_sublevel;
            }
         }
         else
         {
            /* i is the dead object, i_max the live one          */
            /* find any references to i_max and change them to i */
            s16b m_idx = i_list[i_max].iy;
            s16b is_idx = item_set_this_monster(m_idx);
            s16b j;
            s16b number = items_in_set(is_idx);

dlog(DEBUGITEMS,"object4.c: compact_objects: dead item %d monster %d (%s) is_idx %d number %d\n",
                i, m_idx, r_name + r_info[mn_list[m_idx].r_idx].name, is_idx, number);

            for (j=0;j<number;j++)
            {
              if (is_list[is_idx].index[j]==i_max)
              {
dlog(DEBUGITEMS,"object4.c: compact_objects: dead object found as item %d in monster inventory, replacing it with object %d\n",
                j, i);
                 is_list[is_idx].index[j]=i;
              }
            }
         }
dlog(DEBUGITEMS,"object4.c: compact_objects: replacing item %d with %d\n", i, i_max);
         /* Structure copy */
         i_list[i] = i_list[i_max];

         /* Wipe the hole */
         if (i_list[i_max].spell_set)
         {
dlog(DEBUGITEMS,"object4.c: compact_objects: wiping spell_set %d\n",
                i_ptr->spell_set);
            s_list[i_list[i_max].spell_set].inuse = FALSE;
         }
dlog(DEBUGITEMS,"object4.c: compact_objects: wiping item %d\n", i_max);
         WIPE(&i_list[i_max], object_type);
      }
   }

   /* Reset "i_nxt" */
   i_nxt = i_max;

   /* Reset "i_top" */
   i_top = 0;

   /* Collect "live" objects */
   for (i = 0; i < i_max; i++)
   {
      /* Collect indexes */
      i_fast[i_top++] = i;
   }
}

/*
 * Delete all the items when player leaves the level
 * Note -- we do NOT visually reflect these (irrelevant) changes
 */
void wipe_i_list()
{
   s16b i;

   /* Delete the existing objects */
   for (i = 1; i < i_max; i++)
   {
      object_type *i_ptr = &i_list[i];

      /* Skip dead objects */
      if (!i_ptr->k_idx) continue;

      /* Preserve unknown artifacts */
      if (artifact_p(i_ptr))
      {
         if (!object_known_p(i_ptr))
         {
            a_info[i_ptr->name1].cur_num = 0;
         }
         else
         {
            a_info[i_ptr->name1].cur_num = 99; /* signaleer dat deze gezien is,
                                                  maar nu niet meer in het spel is */
	 }
      }

/* here was code to wipe it out of every c_ptr, but I think that's not needed */
      /* Wipe the object */
      if (i_ptr->spell_set)
      {
dlog(DEBUGITEMS,"object4.c: wipe_i_list: wiping spell_set %d\n",
              i_ptr->spell_set);
         s_list[i_ptr->spell_set].inuse = FALSE;
      }
      WIPE(i_ptr, object_type);
   }

   /* Restart free/heap pointers */
   i_nxt = i_max = 1;

   /* No more objects */
   i_top = 0;
}

/*
 * jk - try for a free spell set. As there are relatively few of them, just do
 * a linear search for any with inuse FALSE
 */
s16b s_pop(void)
{
   s16b i,j;
   for (i=1; i<MAX_SPELL_SET_IDX; i++)
   {
      if (s_list[i].inuse == FALSE)
      {
         /* XXX XXX XXX it seems we can still use existing spellsets for new items */
         /* thus clearing away their previous content!!                            */
         for (j=0; j < MAX_I_IDX; j++)
         {
            if (i_list[j].spell_set == i)
            {
               s16b spell_set, spell_bit, k;
               
               dlog(DEBUGALWAYS,"Unused spell_set %d is still used by item %d.\n", i, j);
               for (k=0; k < s_number; k++)
               {
                  spell_set = (k / 16);
                  spell_bit = (k % 16);

                  if ( (s_list[i].spells[spell_set] & (1<<spell_bit) ) > 0)
                  {
                     dlog(DEBUGALWAYS,"object4.c: s_pop: spell %s found\n", s_name + s_info[k].name);
                  }
               }
#if 0
               dump_stack(NULL);
               quit("Unused spell_set still in use?");
#endif
            }
         }
         for (j=0; j < INVEN_TOTAL; j++)
         {
            if (inventory[j].spell_set == i)
            {
               s16b spell_set, spell_bit, k;

               dlog(DEBUGALWAYS,"Unused spell_set %d is still used by inventory %d.\n", i, j);
               for (k=0; k < s_number; k++)
               {
                  spell_set = (k / 16);
                  spell_bit = (k % 16);

                  if ( (s_list[i].spells[spell_set] & (1<<spell_bit) ) > 0)
                  {
                     dlog(DEBUGALWAYS,"object4.c: s_pop: spell %s found\n", s_name + s_info[k].name);
                  }
               }
#if 0
               dump_stack(NULL);
               quit("Unused spell_set still in use?");
#endif
            }
         }

dlog(DEBUGITEMS,"object4.c: s_pop: returning set %d after wiping\n", i);
         for (j=0; j < (MAX_SPELLS_PER_ITEM/16)+1; j++)
         {
            s_list[i].spells[j] = 0;
         }
         return i;
      }
      else
      {
dlog(DEBUGITEMS,"object4.c: s_pop: spell_set %d in use\n", i);
      }
         
   }
   return 0;
}

/*
 * Acquires and returns the index of a "free" object.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 *
 * Note that this function must maintain the special "i_fast"
 * array of pointers to "live" objects.
 */
s16b i_pop(void)
{
   s16b i, n, k;

   /* Initial allocation */
   if (i_max < MAX_I_IDX)
   {
      /* Get next space */
      i = i_max;

      /* Expand object array */
      i_max++;

      /* Update "i_fast" */
      i_fast[i_top++] = i;

      /* default: no spell_set */
      i_list[i].spell_set = 0;

      /* Use this object */
      return (i);
   }
   /* Check for some space */
   for (n = 1; n < MAX_I_IDX; n++)
   {
      /* Get next space */
      i = i_nxt;

      /* Advance (and wrap) the "next" pointer */
      if (++i_nxt >= MAX_I_IDX) i_nxt = 1;

      /* Skip objects in use */
      if (i_list[i].k_idx) continue;

      /* Verify space XXX XXX */
      if (i_top >= MAX_I_IDX) continue;

      /* Verify not allocated */
      for (k = 0; k < i_top; k++)
      {
         /* Hack -- Prevent errors */
         if (i_fast[k] == i) i = 0;
      }

      /* Oops XXX XXX */
      if (!i) continue;

      /* Update "i_fast" */
      i_fast[i_top++] = i;

      /* default: no spell_set */
      i_list[i].spell_set = 0;

      /* Use this object */
      return (i);
   }

   /* Warn the player */
   if (character_dungeon) msg_print("Too many objects!");

   /* Oops */
/* we assume the calling routine tests for this, so we don't quit here */
   return (0);
}

/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow wands/staffs (if they are known to have equal
 * charges) and rods (if fully charged) to combine.
 *
 * Note that rods/staffs/wands are then unstacked when they are used.
 *
 * If permitted, we allow weapons/armor to stack, if they both known.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests never stack (for various reasons).
 *
 * We do NOT allow activatable items (artifacts or dragon scale mail)
 * to stack, to keep the "activation" code clean.  Artifacts may stack,
 * but only with another identical artifact (which does not exist).
 *
 * Ego items may stack as long as they have the same ego-item type.
 * This is primarily to allow ego-missiles to stack.
 */
bool object_similar(object_type *i_ptr, object_type *j_ptr)
{
   s16b total = i_ptr->number + j_ptr->number;

   /* Require identical object types */
   if (i_ptr->k_idx != j_ptr->k_idx) return (0);

   /* spellset's don't stack */
   if ( (i_ptr->spell_set > 0) || (j_ptr->spell_set > 0) ) return (0);
   
   /* Analyze the items */
   switch (i_ptr->tval)
   {
      /* Chests */
      case TV_CHEST:

          /* Never okay */
          return (0);
      /* corpses should be of the same species! */
      case TV_CORPSE:
         if (i_ptr->sval != j_ptr->sval) return(0);
         /* and possibly be of the same age */
         if ( (!stack_allow_corpses) && (i_ptr->xtra2 != j_ptr->xtra2)) return(0);

      /* Food and Potions and Scrolls */
      case TV_FOOD:
      case TV_POTION:
      case TV_SCROLL:
      case TV_SPELL:

          /* Assume okay */
          break;

      case TV_BOOK:
      {
         /* the case with different spellsets has been caught above! */
         break;
      }

      /* Staffs and Wands and Rods */
      case TV_STAFF:
      case TV_WAND:
      case TV_ROD:

          /* Require knowledge */

          if (object_known_p(i_ptr) != object_known_p(j_ptr)) return (0);
          /* Probably okay */

          break;

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

          /* Require permission */
          if (!stack_allow_items) return (0);

          /* XXX XXX XXX Require identical "sense" status */
          /* if ((i_ptr->ident & ID_SENSE) != */
          /*     (j_ptr->ident & ID_SENSE)) return (0); */

          /* Fall through */

      /* Rings, Amulets, Lites */
      case TV_RING:
      case TV_AMULET:
      case TV_LITE:

          /* Require full knowledge of both items */
          if (!object_known_p(i_ptr) || !object_known_p(j_ptr)) return (0);

          /* Fall through */

      /* Missiles */
      case TV_BOLT:
      case TV_ARROW:
      case TV_SHOT:

          /* Require identical "bonuses" */
          if (i_ptr->to_h != j_ptr->to_h) return (FALSE);
          if (i_ptr->to_d != j_ptr->to_d) return (FALSE);
          if (i_ptr->to_a != j_ptr->to_a) return (FALSE);

          /* Require identical "p1val" code */
          if (i_ptr->p1val != j_ptr->p1val) return (FALSE);
          if (i_ptr->p2val != j_ptr->p2val) return (FALSE);

          /* Require identical "artifact" names */
          if (i_ptr->name1 != j_ptr->name1) return (FALSE);

          /* Require identical "ego-item" names */
          if (i_ptr->name2 != j_ptr->name2) return (FALSE);

          /* Hack -- Never stack "powerful" items */
          if (i_ptr->xtra1 || j_ptr->xtra1) return (FALSE);

          /* Hack -- Never stack recharging items */
          if (i_ptr->timeout || j_ptr->timeout) return (FALSE);

          /* Require identical "values" */
          if (i_ptr->ac != j_ptr->ac) return (FALSE);
          if (i_ptr->dd != j_ptr->dd) return (FALSE);
          if (i_ptr->ds != j_ptr->ds) return (FALSE);

          /* Probably okay */
          break;

      /* Various */
      default:

          /* Require knowledge */
          if (!object_known_p(i_ptr) || !object_known_p(j_ptr)) return (0);

          /* Probably okay */
          break;
   }


   /* Hack -- Require identical "cursed" status */
   if ((i_ptr->ident & ID_CURSED) != (j_ptr->ident & ID_CURSED)) return (0);

   /* Hack -- Require identical "broken" status */
   if ((i_ptr->ident & ID_BROKEN) != (j_ptr->ident & ID_BROKEN)) return (0);


   /* Hack -- require semi-matching "inscriptions" */
   if (i_ptr->note && j_ptr->note && (i_ptr->note != j_ptr->note)) return (0);

   /* Hack -- normally require matching "inscriptions" */
   if (!stack_force_notes && (i_ptr->note != j_ptr->note)) return (0);

   if (!stack_ignore_logs)
   {
      if (i_ptr->log.mlevel != j_ptr->log.mlevel) return(0);
      if (i_ptr->log.slevel != j_ptr->log.slevel) return(0);
      if (i_ptr->log.whose != j_ptr->log.whose) return(0);
      if (i_ptr->log.where != j_ptr->log.where) return(0);
   }

   /* Hack -- normally require matching "discounts" */
   if (!stack_force_costs && (i_ptr->discount != j_ptr->discount)) return (0);

   /* Maximal "stacking" limit */
   if (total >= MAX_STACK_SIZE) return (0);

   /* They match, so they must be similar */
   return (TRUE);
}


/*
 * Allow one item to "absorb" another, assuming they are similar
 */
void object_absorb(object_type *i_ptr, object_type *j_ptr, s16b amt)
{
   s16b total = i_ptr->number + amt;

   /* Add together the item counts */
   i_ptr->number = ((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

   /* Hack -- blend "known" status */
   if (object_known_p(j_ptr)) object_known(i_ptr);

   /* Hack -- blend "rumour" status */
   if (j_ptr->ident & ID_RUMOUR) i_ptr->ident |= ID_RUMOUR;

   /* Hack -- blend "mental" status */
   if (j_ptr->ident & ID_MENTAL) i_ptr->ident |= ID_MENTAL;

   /* Hack -- blend "inscriptions" */
   if (j_ptr->note) i_ptr->note = j_ptr->note;

   /* Hack -- average discounts XXX XXX XXX */
   if (i_ptr->discount < j_ptr->discount)
   {
      i_ptr->discount = (i_ptr->discount + j_ptr->discount) / 2;
   }
   if (!stacking_wipes_logs)
   {
      i_ptr->log.mlevel = 0;
      i_ptr->log.slevel = 0;
      i_ptr->log.whose = 0;
      i_ptr->log.where = 0;
   }
   else
   {
      if (j_ptr->log.mlevel > i_ptr->log.mlevel)
      {
         i_ptr->log.mlevel = j_ptr->log.mlevel;
         /* say it was found on level xxx, not which sub-level */
         i_ptr->log.slevel = 0;
         i_ptr->log.whose = j_ptr->log.whose;
         i_ptr->log.where = (j_ptr->log.where | OBJ_FOUND_SOME);
      }
   }
   if ( (i_ptr->tval == TV_ROD) || (i_ptr->tval == TV_STAFF) || (i_ptr->tval == TV_WAND) )
   {
      i_ptr->p1val += j_ptr->p1val;
   }
}

/*
 * Find the index of the object_kind with the given tval and sval
 */
s16b lookup_kind(s16b tval, s16b sval)
{
   s16b k;

   /* Look for it */
   for (k = 1; k < k_number; k++)
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
 * Clear an item
 */
void invwipe(object_type *i_ptr)
{
   if (i_ptr->spell_set)
   {
      s_list[i_ptr->spell_set].inuse=FALSE;
   }
   /* Clear the record */
   WIPE(i_ptr, object_type);
}

/*
 * Make "i_ptr" a "clean" copy of the given "kind" of object
 */
void invcopy(object_type *i_ptr, s16b k_idx)
{
   object_kind *k_ptr = &k_info[k_idx];

   /* Clear the record */
   WIPE(i_ptr, object_type);

   /* make sure no spell set is defined */
   i_ptr->spell_set = 0;

   /* Save the kind index */
   i_ptr->k_idx = k_idx;

   /* Efficiency -- tval/sval */
   i_ptr->tval = k_ptr->tval;
   i_ptr->sval = k_ptr->sval;

   /* Default "p1val" */
   i_ptr->p1val = k_ptr->p1val;
   i_ptr->p2val = k_ptr->p2val;

   /* Default number */
   i_ptr->number = 1;

   /* Default weight */
   i_ptr->weight = k_ptr->weight;

   /* Default magic */
   i_ptr->to_h = k_ptr->to_h;
   i_ptr->to_d = k_ptr->to_d;
   i_ptr->to_a = k_ptr->to_a;

   /* Default power */
   i_ptr->ac = k_ptr->ac;
   i_ptr->dd = k_ptr->dd;
   i_ptr->ds = k_ptr->ds;

   /* Hack -- worthless items are always "broken" */
   if (k_ptr->cost <= 0) i_ptr->ident |= ID_BROKEN;

   /* Hack -- cursed items are always "cursed" */
   if (k_ptr->flags3 & TR3_CURSED) i_ptr->ident |= ID_CURSED;
}

/*
 * Increase the "number" of an item in the inventory
 * the x,y is necessary for reading a scroll of phase door - the
 * item then points at an item elsewhere in the dungeon to be decreased...
 */
void item_increase(s16b item, s16b num, s16b x, s16b y)
{
   object_type *i_ptr = NULL;

   i_ptr=get_item_pointer_xy(item, x, y);

dlog(DEBUGITEMS,"object4.c: item_increase: 2 current num %d, change %d, total %d\n", i_ptr->number, num, i_ptr->number+num);

   /* Bounds check */
   if ((i_ptr->number + num ) < 0)
   {
      num = 0-i_ptr->number; /* make sure we end up with 0 items */
   }
   else if ((i_ptr->number + num ) >99)
   {
      num = 99 - i_ptr->number; /* make sure we end up with 99 items */
   }
   /* doing nothing */ 
   if (num == 0) return;

   /* Add the weight */
   p_ptr->total_weight += (num * i_ptr->weight);
   if (item >= INVEN_WIELD)
   {
      p_ptr->equip_weight += (num * i_ptr->weight);
   }

   /* if we decrease the number of rods, take care of the maximum recharge-time */
   if ((i_ptr->tval == TV_ROD) && (i_ptr->p1val>0))
   {
      i_ptr->p1val = max(i_ptr->number * get_rod_charge(i_ptr), i_ptr->p1val);
   }

   /* if we decrease the number of charged staffs/wands, take care of the maximum charge */
   if ( ( (i_ptr->tval == TV_WAND) || (i_ptr->tval == TV_STAFF) ) && (i_ptr->p1val>0) && (num < 0) )
   {
      /* note: the factor 2 is necessary for rounding - give the player the benefit of the doubt */
      s16b new_charge = 2 * (i_ptr->p1val * (i_ptr->number + num) ) / i_ptr->number;
      i_ptr->p1val = (new_charge/2) + (new_charge & 1);
   }

dlog(DEBUGITEMS,"object4.c: item_increase: 2 current num %d, change %d, total %d\n", i_ptr->number, num, i_ptr->number+num);
   /* Add the number */
   i_ptr->number += num;

   /* Window stuff */
   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Recalculate mana XXX */
   p_ptr->update |= (PU_MANA);

   /* Combine the pack */
   p_ptr->notice |= (PN_COMBINE);
}

/*
 * Erase an inventory slot if it has no more items
 */
void item_optimize(s16b item, s16b x, s16b y)
{
   object_type *i_ptr = get_item_pointer_xy(item,x,y);

   /* Only optimize real items */
   if (!i_ptr->k_idx) return;

   /* Only optimize empty items */
   if (i_ptr->number) return;

   /* The item is in the pack */
   if (item < INVEN_WIELD)
   {
      s16b i;

      /* One less item */
      inven_cnt--;

      /* Slide everything down */
      for (i = item; i < INVEN_PACK; i++)
      {
         /* Structure copy */
         inventory[i] = inventory[i+1];
      }

      /* Erase the "final" slot */
      invwipe(&inventory[i]);

      /* Window stuff */
      p_ptr->window |= (PW_INVEN);
   }

   /* The item is being wielded */
   else if (item<INVEN_TOTAL)
   {
      /* One less item */
      equip_cnt--;

      /* Erase the empty slot */
      invwipe(&inventory[item]);

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Recalculate torch */
      p_ptr->update |= (PU_TORCH);

      /* Recalculate mana XXX */
      p_ptr->update |= (PU_MANA);

      /* Window stuff */
      p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
   }
   /* the item is on the floor */
   else
   {
      delete_object(x,y,item-INVEN_TOTAL);
   }
}

/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(object_type *i_ptr)
{
   s16b i;
   object_type *j_ptr;

   /* Empty slot? */
   if (inven_cnt < INVEN_PACK) return (TRUE);

   /* Similar slot? */
   for (i = 0; i < INVEN_PACK; i++)
   {
      /* Get that item */
      j_ptr = &inventory[i];

      /* Check if the two items can be combined */
      if (object_similar(j_ptr, i_ptr)) return (TRUE);
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
 */
s16b inven_carry(object_type *i_ptr, s16b amt)
{
   s16b      i, j, k;
   s16b      n = -1;

   object_type *j_ptr;

   /* Check for combining */
   for (j = 0; j < INVEN_PACK; j++)
   {
      j_ptr = &inventory[j];

      /* Skip empty items */
      if (!j_ptr->k_idx) continue;

      /* Hack -- track last item */
      n = j;

      /* Check if the two items can be combined */
      if (object_similar(j_ptr, i_ptr))
      {
         /* Combine the items */
         object_absorb(j_ptr, i_ptr, amt);

         i_ptr->number-=amt;

         /* Increase the weight */
         p_ptr->total_weight += (i_ptr->number * i_ptr->weight * amt);

         /* Window stuff */
         p_ptr->window |= (PW_INVEN);

         /* Recalculate bonuses */
         p_ptr->update |= (PU_BONUS);

         /* Success */
         return (j);
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
   if (i < INVEN_PACK)
   {
      s32b        i_value, j_value;

      /* Get the "value" of the item */
      i_value = object_value(i_ptr);

      /* Scan every occupied slot */
      for (j = 0; j < INVEN_PACK; j++)
      {
         j_ptr = &inventory[j];

         /* Use empty slots */
         if (!j_ptr->k_idx) break;

         /* Objects sort by decreasing type */
         if (i_ptr->tval > j_ptr->tval) break;
         if (i_ptr->tval < j_ptr->tval) continue;

         /* Non-aware (flavored) items always come last */
         if (!object_aware_p(i_ptr)) continue;
         if (!object_aware_p(j_ptr)) break;

         /* Objects sort by increasing sval */
         if (i_ptr->sval < j_ptr->sval) break;
         if (i_ptr->sval > j_ptr->sval) continue;

         /* Unidentified objects always come last */
         if (!object_known_p(i_ptr)) continue;
         if (!object_known_p(j_ptr)) break;

         /* Hack -- otherwise identical rods sort by increasing
            recharge time  --dsb */
         if (i_ptr->tval == TV_ROD)
         {
            if (i_ptr->p1val < j_ptr->p1val) break;
            if (i_ptr->p1val > j_ptr->p1val) continue;
         }

         /* Determine the "value" of the pack item */
         j_value = object_value(j_ptr);

         /* Objects sort by decreasing value */
         if (i_value > j_value) break;
         if (i_value < j_value) continue;
      }

      /* Use that slot */
      i = j;

      /* Structure slide (make room) */
      for (k = n; k >= i; k--)
      {
         /* Hack -- Slide the item */
         inventory[k+1] = inventory[k];

         /* XXX XXX hack: if we make a *shared* spell_set,   */
         /* it is wiped below with invwipe(inventory[k])!!   */
         if (inventory[k+1].spell_set > 0)
         {
            inventory[k].spell_set = 0;
         }
      }

      /* Paranoia -- Wipe the new slot */
      invwipe(&inventory[i]);
   }

   if (amt==i_ptr->number)
   {
      /* Structure copy to insert the new item */
      inventory[i] = (*i_ptr);

      /* Forget the old location */
      inventory[i].iy = inventory[i].ix = 0;
   }
   else
   {
      /* Structure copy to insert the new item */
      inventory[i] = (*i_ptr);

      /* Forget the old location */
      inventory[i].iy = inventory[i].ix = 0;

      inventory[i].number=amt;
      i_ptr->number -=amt;
   }
   /* Increase the weight, prepare to redraw */
   p_ptr->total_weight += (i_ptr->number * i_ptr->weight);

   /* Count the items */
   inven_cnt++;

   /* Window stuff */
   p_ptr->window |= (PW_INVEN);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Reorder pack */
   p_ptr->notice |= (PN_REORDER);
   /* XXX XXX XXX FOR DEBUGGING ONLY! */
   for (j = 0; j < INVEN_PACK; j++)
   {
      j_ptr = &inventory[j];
      if ((j_ptr->number == 0) && (j_ptr->k_idx > 0))
      {
         msg_format("Item with 'No More ....' detected in inventory (%d)! - stack dumped", j);
         dlog(DEBUGALWAYS,"object4.c: inven_carry: item %d in inventory has ->number == 0 (%s)\n",
                          j, k_name + k_info[j_ptr->k_idx].name);
         dump_stack(NULL);
      }
   }

   /* Return the slot */
   return (i);
}

/* jk */
/* return TRUE if object i should come before j */
bool floor_compare(object_type *i_ptr, object_type *j_ptr)
{
   /* Objects sort by decreasing type */
   if (i_ptr->tval > j_ptr->tval) return (TRUE);
   if (i_ptr->tval < j_ptr->tval) return (FALSE);

   /* Non-aware (flavored) items always come last */
   if (!object_aware_p(i_ptr)) return (FALSE);
   if (!object_aware_p(j_ptr)) return (TRUE);

   /* Objects sort by increasing sval */
   if (i_ptr->sval < j_ptr->sval) return (TRUE);
   if (i_ptr->sval > j_ptr->sval) return (FALSE);

   /* Unidentified objects always come last */
   if (!object_known_p(i_ptr)) return (FALSE);
   if (!object_known_p(j_ptr)) return (TRUE);

   /* Hack -- otherwise identical rods sort by increasing
      recharge time  --dsb */
   if (i_ptr->tval == TV_ROD)
   {
      return (i_ptr->p1val < j_ptr->p1val);
   }

   /* Determine the "value" of the items */
   /* Objects sort by decreasing value */
   return (object_value(i_ptr)>=object_value(j_ptr));
}

/* jk */
/*
 * Combine items on the floor
 *
 * Note special handling of the "overflow" slot
 */
void optimize_floor(s16b x, s16b y)
{
   s16b         i, j, objs;

   object_type *i_ptr;
   object_type *j_ptr;

   bool        flag = FALSE;

   objs = objects_on_floor(x,y);
   if (!objs) return;

   /* first see to it there are no gaps in the indexes, ie */
   /* item_set: 12 34 0 45 67 should be 12 34 45 67 0 */
   for (i=0;i<MAX_FLOOR_ITEMS;i++)
   {
      if (!floor_item_xy(i,x,y))
      /* we found a zero-item */
      {
         j = i+1;
         /* test until a non-zero item found, or index too high */
         while ((j<MAX_FLOOR_ITEMS) && !floor_item_xy(j,x,y)) j++;
         /* if index too high, break */
         if (j==MAX_FLOOR_ITEMS) break;
         /* transport it down */
         set_floor_item_xy(i,get_floor_item_xy(j,x,y),x,y);
         /* and zero the old one */
         set_floor_item_xy(j,0,x,y);
      }
   }
/* one object can't be combined */
   if (objs==1) return;
   /* Combine the floor (backwards) */
   for (i = objs-1; i > 0; i--)
   {
      /* Get the item */
      i_ptr = get_item_pointer_floor_xy(i,x,y);
      /* Scan the items above that item */
      for (j = 0; j < i; j++)
      {
         /* Get the item */
         j_ptr = get_item_pointer_floor_xy(j,x,y);

         /* Can we drop "i_ptr" onto "j_ptr"? */
         if (object_similar(j_ptr, i_ptr))
         {
            /* Take note */
            flag = TRUE;

            /* Add together the item counts */
            object_absorb(j_ptr, i_ptr,i_ptr->number);

            /* Erase the last object */
            delete_object(x,y,i);

            break;
         }
      }
   }

   /* Message - perhaps confusion, hallucination etc should be handled too */
   if (flag && player_has_los_bold(x, y))
   {
      if ((y==py) && (x==px))
      {
         msg_print("You feel the pile beneath your feet shift.");
      }
      else
      {
         if (!p_ptr->blind)
         {
            msg_print("You see a pile change shape.");
         }
      }
   }
}

/* jk - sort the items on the floor at x,y */
/* we depend on there being no gaps in the indexes */
/* ie n objects on floor in indexes 0,1, ... , n-1 */
void floor_sort(s16b x,s16b y)
{
   object_type *i_ptr,*j_ptr;
   s16b i,j;
   s16b n = objects_on_floor(x,y);
   if (n==1) return;

   for (i=0;i<n-1;i++) {
      for (j=0;j<n-1;j++) {
         i_ptr=get_item_pointer_floor_xy(j,x,y);
         j_ptr=get_item_pointer_floor_xy(j+1,x,y);
         if (!floor_compare(i_ptr,j_ptr))
         {
            s16b tmp = get_floor_item_xy(j,x,y);
            set_floor_item_xy(j,get_floor_item_xy(j+1,x,y),x,y);
            set_floor_item_xy(j+1,tmp,x,y);
         }
      }
   }
}

/* jk */
/* see if we can combine some items, then renumber the item index from 0 */
/* return -1 in case of error */
s16b floor_carry(object_type *i_ptr,s16b x, s16b y)
{
   s16b         i = 0;
   s16b         j;
   s16b         n;
   s16b         i_idx;
   object_type *j_ptr;

   if (!in_bounds(x, y))
   {
      dlog(DEBUGALWAYS,"object4.c: floor_carry: %d,%d out of bounds\n", x, y);
      return -1;
   }
   n = objects_on_floor(x,y);

   /* if more items, check for combining */
   if (n>0)
   {
      /* Check for combining */
      for (j=n-1; j >= 0; j--)
      {
          j_ptr = get_item_pointer_floor_xy(j,x,y);

          /* Check if the two items can be combined */
          if (object_similar(j_ptr, i_ptr))
          {
              object_absorb(j_ptr, i_ptr,i_ptr->number);
              p_ptr->update |= (PU_BONUS);
              return (j);
          }
      }
      /* Find an empty slot */
      i = n;
   } /* object already present */
   /* if we can't combine, and the floor is 'full', return -1 */
   if (n==MAX_FLOOR_ITEMS) return (-1);
   if (objects_on_floor(x,y)==1)
   {
      s16b is_idx = is_pop();
      if (is_idx==-1)
      {
         quit("floor_carry: no room in is_list");
      }
      is_list[is_idx].index[0] = dungeon.level[sublevel][y][x].i_idx;
      dungeon.level[sublevel][y][x].i_idx = (is_idx | ITEM_SET_FLAG);
      is_list[is_idx].x = x;
      is_list[is_idx].y = y;
      is_list[is_idx].z = sublevel;
      i = 1;
   }
   /* now get a free index into i_list and copy the new item there */
   i_idx = i_pop();
   if (i_idx==0)
   {
      quit("floor_carry: i_pop() failed");
   }

   set_floor_item_xy(i,i_idx,x,y);
   i_list[i_idx]=(*i_ptr);
   i_list[i_idx].ix = x;
   i_list[i_idx].iy = y;
   i_list[i_idx].iz = sublevel;

   /* we must keep this to be able to return the slot after sorting */

   /* Window stuff */
   p_ptr->window |= (PW_INVEN);

   /* now sort the floor */
   floor_sort(x,y);

   /* and search for the newly inserted member, the return that slot */
   i=0;
   while (get_floor_item_xy(i,x,y)!=i_idx) i++;
   return (i);
}

/*
 * Combine items in the pack
 *
 * Note special handling of the "overflow" slot
 */
void combine_pack(void)
{
   s16b         i, j;

   object_type *i_ptr;
   object_type *j_ptr;

   bool        flag = FALSE;


   /* Combine the pack (backwards) */
   for (i = INVEN_PACK; i > 0; i--)
   {
      /* Get the item */
      i_ptr = &inventory[i];

      /* Skip empty items */
      if (!i_ptr->k_idx) continue;

      /* Scan the items above that item */
      for (j = 0; j < i; j++)
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
            object_absorb(j_ptr, i_ptr,i_ptr->number);

            /* One object is gone */
            inven_cnt--;

            /* Erase the last object */
            invwipe(&inventory[i]);

            /* Window stuff */
            p_ptr->window |= (PW_INVEN | PW_EQUIP);

            /* XXX XXX XXX Reorder the pack */
            p_ptr->notice |= (PN_REORDER);

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
   s16b         i, j, k;

   s32b        i_value;
   s32b        j_value;

   object_type *i_ptr;
   object_type *j_ptr;

   object_type temp;

   bool        flag = FALSE;


   /* Re-order the pack (forwards) */
   for (i = 0; i < INVEN_PACK; i++)
   {
      /* Mega-Hack -- allow "proper" over-flow */
      if ((i == INVEN_PACK) && (inven_cnt == INVEN_PACK)) break;

      /* Get the item */
      i_ptr = &inventory[i];

      /* Skip empty slots */
      if (!i_ptr->k_idx) continue;

      /* Get the "value" of the item */
      i_value = object_value(i_ptr);

      /* Scan every occupied slot */
      for (j = 0; j < INVEN_PACK; j++)
      {
         /* Get the item already there */
         j_ptr = &inventory[j];

         /* Use empty slots */
         if (!j_ptr->k_idx) break;

         /* Objects sort by decreasing type */
         if (i_ptr->tval > j_ptr->tval) break;
         if (i_ptr->tval < j_ptr->tval) continue;

         /* Non-aware (flavored) items always come last */
         if (!object_aware_p(i_ptr)) continue;
         if (!object_aware_p(j_ptr)) break;

         /* Objects sort by increasing sval */
         if (i_ptr->sval < j_ptr->sval) break;
         if (i_ptr->sval > j_ptr->sval) continue;

         /* Unidentified objects always come last */
         if (!object_known_p(i_ptr)) continue;
         if (!object_known_p(j_ptr)) break;

         /* Determine the "value" of the pack item */
         j_value = object_value(j_ptr);

         /* Objects sort by decreasing value */
         if (i_value > j_value) break;
         if (i_value < j_value) continue;
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
}

/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "arg_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 *
 * The features, objects, and monsters, should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files.  XXX XXX XXX
 *
 * The "prefs" parameter is no longer meaningful.  XXX XXX XXX
 */
void reset_visuals(void)
{
   int i;

   /* Extract default attr/char code for features */
   for (i = 0; i < f_number; i++)
   {
      feature_type *f_ptr = &f_info[i];

      /* Assume we will use the underlying values */
      f_ptr->x_attr = f_ptr->d_attr;
      f_ptr->x_char = f_ptr->d_char;
   }

   /* Extract default attr/char code for objects */
   for (i = 0; i < k_number; i++)
   {
      object_kind *k_ptr = &k_info[i];

      /* Default attr/char */
      k_ptr->x_char = k_ptr->d_char;
      k_ptr->x_attr = k_ptr->d_attr;
   }

   /* Extract default attr/char code for monsters */
   for (i = 0; i < r_number_total; i++)
   {
      monster_race *r_ptr = &r_info[i];

      /* Default attr/char */
      r_ptr->x_attr = r_ptr->d_attr;
      r_ptr->x_char = r_ptr->d_char;
   }


   /* Extract attr/chars for inventory objects (by tval) */
   for (i = 0; i < 128; i++)
   {
      /* Default to white */
      tval_to_attr[i] = TERM_WHITE;
   }


   /* Graphic symbols */
   if (use_graphics)
   {
      /* Process "graf.prf" */
      process_pref_file("graf.prf");
   }

   /* Normal symbols */
   else
   {
      /* Process "font.prf" */
      process_pref_file("font.prf");
   }
}

/*
 * Obtain the "flags" for an item
 */
void object_flags(object_type *i_ptr, u64b *f1, u64b *f2, u64b *f3)
{
   object_kind *k_ptr = &k_info[i_ptr->k_idx];

   /* Base object */
   (*f1) = k_ptr->flags1;
   (*f2) = k_ptr->flags2;
   (*f3) = k_ptr->flags3;

   /* Artifact */
   if (i_ptr->name1)
   {
      artifact_type *a_ptr = &a_info[i_ptr->name1];

      (*f1) = a_ptr->flags1;
      (*f2) = a_ptr->flags2;
      (*f3) = a_ptr->flags3;
   }

   /* Ego-item */
   if (i_ptr->name2)
   {
      ego_item_type *e_ptr = &e_info[i_ptr->name2];
      (*f1) |= e_ptr->flags1;
      (*f2) |= e_ptr->flags2;
      (*f3) |= e_ptr->flags3;
   }

   /* Extra powers */
   switch (i_ptr->xtra1)
   {
      case EGO_XTRA_SUSTAIN:

         /* Choose a sustain */
         switch (i_ptr->xtra2 % 6)
         {
            case 0: (*f2) |= TR2_SUST_STR; break;
            case 1: (*f2) |= TR2_SUST_INT; break;
            case 2: (*f2) |= TR2_SUST_WIS; break;
            case 3: (*f2) |= TR2_SUST_DEX; break;
            case 4: (*f2) |= TR2_SUST_CON; break;
            case 5: (*f2) |= TR2_SUST_CHR; break;
         }

         break;

/* jk */
      case EGO_XTRA_LOWRESIST:

         /* Choose a power */
         switch (i_ptr->xtra2 % 4)
         {
            case 0: (*f2) |= TR2_RES_ACID; break;
            case 1: (*f2) |= TR2_RES_ELEC; break;
            case 2: (*f2) |= TR2_RES_FIRE; break;
            case 3: (*f2) |= TR2_RES_COLD; break;
         }

         break;

      case EGO_XTRA_HIGHRESIST:

         /* Choose a power */
         switch (i_ptr->xtra2 % 9)
         {
            case 0: (*f2) |= TR2_RES_BLIND; break;
            case 1: (*f2) |= TR2_RES_CONF; break;
            case 2: (*f2) |= TR2_RES_SOUND; break;
            case 3: (*f2) |= TR2_RES_SHARDS; break;
            case 4: (*f2) |= TR2_RES_NETHER; break;
            case 5: (*f2) |= TR2_RES_NEXUS; break;
            case 6: (*f2) |= TR2_RES_CHAOS; break;
            case 7: (*f2) |= TR2_RES_DISEN; break;
            case 8: (*f2) |= TR2_RES_POIS; break;
         }

         break;

      case EGO_XTRA_ABILITY:

         /* Choose an ability */
         switch (i_ptr->xtra2 % 8)
         {
            case 0: (*f3) |= TR3_FEATHER; break;
            case 1: (*f1) |= TR3_SOMELITE; break;
            case 2: (*f3) |= TR3_SEE_INVIS; break;
            case 3: (*f3) |= TR3_TELEPATHY; break;
            case 4: (*f3) |= TR3_SLOW_DIGEST; break;
            case 5: (*f3) |= TR3_REGEN; break;
            case 6: (*f2) |= TR2_FREE_ACT; break;
            case 7: (*f2) |= TR2_HOLD_LIFE; break;
         }

         break;

   }
}

void handle_corpse_rot_world(object_type *i_ptr, s16b i_list_idx)
{
   char i_name[80];
   s16b durability;

   i_ptr->xtra2 -= 5;
   if (i_ptr->xtra2 > 0) return;
   /* it's a corpse - and it's rotten :-) */

   object_desc(i_name, i_ptr, FALSE, 1);
   if (r_info[i_ptr->sval].corpse_spoiling<250)
      durability = 1;
   else if (r_info[i_ptr->sval].corpse_spoiling<1000)
      durability = 3;
   else if (r_info[i_ptr->sval].corpse_spoiling<5000)
      durability = 6;
   else
      durability = 10;

   if (randint(100)>r_info[i_ptr->sval].corpse_chance_bones)
   {
      if (i_ptr->ix==-1)
      /* it's in a monsters inventory */
      {
         s16b is_idx = item_set_this_monster(i_ptr->iy);
         s16b index  = which_item_in_set(is_idx, i_list_idx);
         s16b m_idx  = i_ptr->iy;
         monster_type *m_ptr = &mn_list[m_idx];

         if (index<0)
         {
            /* this shouldn't happen - this corpse is marked as 'in monster inventory' */
            /* but the monster doesn't exist? We silently delete it                    */
            dlog(DEBUGALWAYS, "corpse with ix==-1 found which isn't known in monster inventory");
            (void)delete_object_idx(i_list_idx);
         }
         else if  ( (distance(px,py,m_ptr->fx,m_ptr->fy)<MAX_SIGHT) &&
                     (dungeon.level[sublevel][m_ptr->fy][m_ptr->fx].fdat & CAVE_VIEW) &&
                     !p_ptr->blind && los(px, py, m_ptr->fx, m_ptr->fy))
         {
            if (corpse_messages)
               msg_format("You see that the monsters %s %s destroyed!",
                          i_name, (i_ptr->number > 1) ? "are" : "is");
         }

         /* Destroy "amt" items in monster inventory */
         monster_inven_increase(m_idx,is_idx,index,-1);
         monster_inven_optimize(m_idx,is_idx);
      }
      else
      {
         if ((i_ptr->ix == px) && (i_ptr->iy == py) && (i_ptr->iz == sublevel))
         {
            if (p_ptr->blind && corpse_messages)
               msg_print("You feel something disappear beneath you.");
            else if (corpse_messages)
               msg_format("You feel %s %s disappear beneath you.",
                          i_name, is_a_vowel(i_name[0])?"an":"a");
         }
         else if  ( (distance(px,py,i_ptr->ix,i_ptr->iy)<MAX_SIGHT) &&
                     (dungeon.level[sublevel][i_ptr->iy][i_ptr->ix].fdat & CAVE_VIEW) &&
                     !p_ptr->blind && los(px, py, i_ptr->ix, i_ptr->iy))
         {
            if (corpse_messages)
               msg_format("You see %s %s rot away.",
                          is_a_vowel(i_name[0])?"an":"a", i_name);
         }
         delete_object_idx(i_list_idx);
      }
   }
   else
   {
      /* only notice this if not in monster inventory */
      if (i_ptr->ix!=-1)
      {
         if ((i_ptr->ix == px) && (i_ptr->iy == py) && (i_ptr->iz == sublevel))
         {
            if (corpse_messages && p_ptr->blind)
               msg_print("You feel something change beneath you.");
            else if (corpse_messages)
               msg_format("You feel %s %s change beneath you.", i_name,
                          is_a_vowel(i_name[0])?"an":"a");
         }
         else if  (  (i_ptr->iz == sublevel) &&
                     (distance(px,py,i_ptr->ix,i_ptr->iy)<MAX_SIGHT) &&
                     (dungeon.level[sublevel][i_ptr->iy][i_ptr->ix].fdat & CAVE_VIEW) &&
                     !p_ptr->blind && los(px, py, i_ptr->ix, i_ptr->iy))
         {
            if (corpse_messages)
               msg_format("You see %s change into a skeleton.",
                          i_name);
            note_spot(i_ptr->ix, i_ptr->iy);
            lite_spot(i_ptr->ix, i_ptr->iy);
         }
         i_ptr->tval = TV_SKELETON;
         i_ptr->sval = SV_SKELETON_BROKEN_END + i_ptr->sval;
      }
   }
}

void handle_corpse_rot_inventory(void)
{
   s16b        i;
   object_type *i_ptr;

   for (i=0; i<INVEN_PACK; i++)
   {
      if (inventory[i].tval == TV_CORPSE)
      {
         i_ptr = &inventory[i];
         i_ptr->xtra2 -= 5;
         if (i_ptr->xtra2 < 0)
         {
            char i_name[80];
            s16b durability;
            object_desc(i_name, i_ptr, TRUE, 1);
            if (r_info[i_ptr->sval].corpse_spoiling<250)
               durability = 1;
            else if (r_info[i_ptr->sval].corpse_spoiling<1000)
               durability = 3;
            else if (r_info[i_ptr->sval].corpse_spoiling<5000)
               durability = 6;
            else
               durability = 10;
            if (randint(100)>r_info[i_ptr->sval].corpse_chance_bones)
            {
               if (corpse_messages) msg_format("Your %s rots away!", i_name);
               item_increase(i, -i_ptr->number, px, py);
               item_optimize(i, px, py);
            }
            else
            {
               msg_print("Your pack feels lighter!");
               i_ptr->tval = TV_SKELETON;
               i_ptr->sval = SV_SKELETON_BROKEN_END + i_ptr->sval;
            }
         }
      }
   }
}

/*
 * Hack -- process the objects
 */
void process_objects(void)
{
   s16b i, k;

   object_type *i_ptr;

   /* Hack -- only every ten game turns */
   if ((turn % 10) != 5) return;

   /* Process objects */
   for (k = i_top - 1; k >= 0; k--)
   {
      /* Access index */
      i = i_fast[k];

      /* Access object */
      i_ptr = &i_list[i];

      /* Excise dead objects */
      if (!i_ptr->k_idx)
      {
         /* Excise it */
         i_fast[k] = i_fast[--i_top];

         /* Skip */
         continue;
      }

      /* Recharge rods on the ground */
      if ((i_ptr->tval == TV_ROD) && (i_ptr->p1val)) i_ptr->p1val--;
      if (i_ptr->tval == TV_CORPSE) handle_corpse_rot_world(i_ptr, i);
   }
   handle_corpse_rot_inventory();
}

