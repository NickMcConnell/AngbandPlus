/* File: object3.c */

/* Purpose: handle traps */

/* the below copyright probably still applies, but it is heavily changed
 * copied, adapted & re-engineered by JK.
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

void init_trap(trap_item_type *tr_ptr)
{
   s16b i;
   tr_ptr->inuse = FALSE;
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      tr_ptr->type[i]=0;
      tr_ptr->found[i]=FALSE;
   }
}

void set_trap_found_ptr(trap_item_type *tr_ptr, s16b trap)
{
   s16b i;
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      if (tr_ptr->type[i]==trap)
      {
         tr_ptr->found[i]=TRUE;
      }
   }
}

void set_trap_notfound_ptr(trap_item_type *tr_ptr, s16b trap)
{
   s16b i;
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      if (tr_ptr->type[i]==trap)
      {
         tr_ptr->found[i]=FALSE;
      }
   }
}

bool set_trap(trap_item_type *tr_ptr,s16b trap,bool on)
{
dlog(DEBUGTRAPS,"object3.c: set_trap: setting trap %d val %d\n", trap, on);
   if (on)
   {
      s16b i;
      bool success=FALSE;
      for (i=0; i < MAX_TRAPS_IN_SET; i++)
      {
         if (tr_ptr->type[i]==0)
         {
            tr_ptr->type[i] = trap;
            success=TRUE;
            break;
         }
      }
      return (success);
   }
   else
   {
      s16b i;
      bool success=FALSE;
      bool others=FALSE;
      for (i=0; i < MAX_TRAPS_IN_SET; i++)
      {
         if (tr_ptr->type[i]==trap)
         {
            tr_ptr->type[i] = 0;
            tr_ptr->found[i] = FALSE;
            success=TRUE;
         }
         /* if any other is non-zero, the trap has other traps! */
         others |= (tr_ptr->type[i]!=0);
dlog(DEBUGTRAPS,"object3.c: set_trap: off %d type %d found %d success %d others %d\n",
                i, tr_ptr->type[i], tr_ptr->found[i], success, others);
      }
      if (!others) /* tr_ptr empty? */
      {
         init_trap(tr_ptr);
      }
      return (success);
   }
}

bool get_trap(trap_item_type *tr_ptr,s16b trap)
{
   s16b i;
   bool success=FALSE;
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      if (tr_ptr->type[i]==trap)
      {
         success=TRUE;
         break;
      }
   }
   return (success);
}

s16b first_trap(trap_item_type *tr_ptr)
{
   s16b i;
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      if (tr_ptr->type[i]!=0)
      {
         return (tr_ptr->type[i]);
      }
   }
   return (-1);
}

s16b first_found_trap(trap_item_type *tr_ptr)
{
   s16b i;
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      if ((tr_ptr->type[i]!=0) && tr_ptr->found[i])
      {
         return (tr_ptr->type[i]);
      }
   }
   return (-1);
}


/* jk */
void delete_trap_idx(s16b idx)
{
   trap_item_type *tr_ptr = &t_list[idx];
   s16b y = tr_ptr->ty;
   s16b x = tr_ptr->tx;
   s16b j;

   if (x==-1)
   {
      for (j=0;j<MAX_I_IDX;j++)
      {
         object_type *j_ptr = &i_list[j];
         if ((j_ptr->tval==TV_CHEST) && (j_ptr->xtra2==idx))
         {
            j_ptr->xtra2 = 0; /* signals untrapped */
         }
      }
   }
   else
   {
      if (dungeon.level[sublevel][y][x].t_idx==idx)
      {
         dungeon.level[sublevel][y][x].t_idx = 0;
      }
      if (player_has_los_bold(x, y))
      {
         lite_spot(x, y);
      }
   }
   WIPE(tr_ptr, trap_item_type);
}

/*
 * this gives an index into knowstr[] from the experience
 * a player has with a trap
 */
s16b trap_experience(trap_type *t_ptr)
{
   s16b result;

   if (t_ptr->known==0) result=0;
   else if (t_ptr->known<10) result=1;
   else if (t_ptr->known<25) result=2;
   else if (t_ptr->known<75) result=3;
   else if (t_ptr->known<150) result=4;
   else if (t_ptr->known<300) result=5;
   else if (t_ptr->known<750) result=6;
   else if (t_ptr->known<2000) result=7;
   else if (t_ptr->known<5000) result=8;
   else result=9;
   return (result);
}

/* jk - this is copied from i_pop, but works on the t_list trap array */
/* now completely revamped */
s16b t_pop(void)
{
   s16b i = 1;
dlog(DEBUGTRAPS,"object3.c: t_pop step 0\n");
   while (i<(MAX_TR_IDX-1))
   {
      if (!t_list[i].inuse)
      {
         if (i==t_max)
         {
            t_max = i+1;
         }
         WIPE(&t_list[i],trap_item_type);
         t_list[i].inuse=TRUE;
dlog(DEBUGTRAPS,"returning from t_pop with idx %d t_max %d\n", i, t_max);
         return(i);
      }
      i++;
   }
   if (character_dungeon)
   {
      msg_print("Tried to allocate a trap when t_list is already full!");
      dlog(DEBUGTRAPS,"returning from t_pop with %d\n",i);
   }
   return(-1);
}

/* jk - this is copied from i_pop, but works on the is_list item set array */
/* now completely revamped */
s16b is_pop(void)
{
   s16b i = 0;
   while (i<MAX_IS_IDX)
   {
      if (!is_list[i].inuse)
      {
         if (i>=is_max)
         {
            is_max=i+1;
         }

         WIPE (&is_list[i],item_set_type);
         is_list[i].inuse=TRUE;

         return(i);
      }
      i++;
   }
   if (character_dungeon)
   {
      msg_print("Tried to allocate an item set when is_list is already full!");
   }
   return(-1);
}

/*
 * Delete all the item_sets when player leaves the level
 * Note -- we do NOT visually reflect these (irrelevant) changes
 */
void wipe_is_list()
{
   s16b          i;
   item_set_type *is_ptr;

   /* Delete the existing objects */
   for (i = 0; i < is_max; i++)
   {
      is_ptr = &is_list[i];
      WIPE(is_ptr, item_set_type);
   }
   is_max = 0;
}

/*
 * Delete all the traps on floors/doors/chests not in possession of the player
 * when player leaves the level
 * Note -- we do NOT visually reflect these (irrelevant) changes
 */
void wipe_t_list()
{
   s16b          i,j,cnt = 0;
   object_type  *i_ptr;
   s16b          index[INVEN_TOTAL-INVEN_WIELD-1];

/* find all chests in the players inventory */
   for (i=0;i<INVEN_PACK;i++)
   {
      i_ptr=&inventory[i];
      if ((i_ptr->tval==TV_CHEST) && (i_ptr->xtra2!=-1)) index[cnt++]=i_ptr->xtra2;
   }

   /* Delete the existing objects */
   for (i = 0; i < t_max; i++)
   {
      trap_item_type *tr_ptr = &t_list[i];
      bool skip = FALSE;

      /* Skip dead objects */
      if (!tr_ptr->inuse) continue; /* dead traps stay dead */
      for (j=0;j<cnt;j++)
      {
         if (i==index[j])
         {
            skip=TRUE;  /* trap on a chest in inventory? */
            break;
         }
      }
      if (skip) continue;

      /* Wipe the object */
      WIPE(tr_ptr, trap_item_type);
   }

   /* Restart free/heap pointers */
   t_max = 1;
}

/*
 * this weeds out unused traps
 * and fixes the pointers to all changed traps in both squares and items
 * after this function, t_list has only used traps, and
 * t_max = 1 + the number of traps
 */
void compact_traps(void)
{
   cave_cell_type *c_ptr;
   s16b i, j, k;

dlog(DEBUGTRAPS,"object3.c: compact_traps: starting with t_max at %d\n",t_max);
   for (i=1; i<t_max;i++)
   {
      trap_item_type *tr_ptr = &t_list[i]; /* check a trap in t_list */
      if (tr_ptr->inuse==FALSE)            /* if it's unused         */
      {
dlog(DEBUGTRAPS,"object3.c: compact_traps: found disused trap at %d\n",i);
         j = i;                             /* check the traps above it */
         while ((tr_ptr->inuse==FALSE) && (j<=t_max))
         {
            j++;
            if (j>t_max) break;
dlog(DEBUGTRAPS,"object3.c: compact_traps: checking for used traps at %d\n",j);
            tr_ptr = &t_list[j];             /* continue checking until an */
         }                                  /* used one is found */
dlog(DEBUGTRAPS,"object3.c: compact_traps: finished looking for used traps; j now %d\n",j);
         if (j>t_max)
         {
            t_max = i-1; /* since there are no valid traps above i */
                         /* and i itself is invalid, set t_max=i-1 */
            if (t_max==0) t_max = 1;
dlog(DEBUGTRAPS,"object3.c: compact_traps: no used traps found: t_max now %d\n", t_max);
            break;
         }
dlog(DEBUGTRAPS,"object3.c: compact_traps: switching unused trap %d with used trap %d\n", i, j);
         t_list[i] = t_list[j];             /* move that down */
         t_list[j].inuse = FALSE;           /* free the original one */
         t_max--;                           /* we now have less traps */
dlog(DEBUGTRAPS,"object3.c: compact_traps: after switching t_max now %d\n",t_max);
         /* change all references */
dlog(DEBUGTRAPS,"object3.c: compact_traps: searching for trap %d @ %d,%d,%d\n",
                j, t_list[j].tx, t_list[j].ty, t_list[j].tz);
         if (t_list[j].tx != -1)
         {
            c_ptr = &dungeon.level[t_list[j].tz][t_list[j].ty][t_list[j].tx];
            if (c_ptr->t_idx != j)
            {
               dlog(DEBUGEXTRA,"object3.c: compact_traps: trap %d point to cave x,y,z %d,%d,%d - cave t_idx = %d?\n",
                                j, t_list[j].tx, t_list[j].ty, t_list[j].tz, c_ptr->t_idx);
            }
            else
            {
               /* make the cell point to the new trap location in t_list */
               c_ptr->t_idx = i;
               continue;
            }
         }
         /* Examine the objects - for trapped chests */
         for (k = 1; k < i_max; k++)
         {
            object_type *i_ptr = &i_list[k];

            if ((i_ptr->tval == TV_CHEST) && (i_ptr->xtra2 == j))
            {
dlog(DEBUGTRAPS,"object3.c: compact_traps: t_idx at chest i_idx %d from %d to %d\n", k, j, i);
               i_ptr->xtra2 = i;
            }
         }
dlog(DEBUGTRAPS,"object3.c: compact_traps: exit 1 with t_max at %d\n",t_max);
      }
dlog(DEBUGTRAPS,"object3.c: compact_traps: exit 2 with t_max at %d i %d\n",t_max,i);
   }
dlog(DEBUGTRAPS,"object3.c: compact_traps: exit 3 with t_max at %d\n",t_max);
}

void compact_item_sets(void)
{
   cave_cell_type *c_ptr;
   s16b y, x;
   s16b i, j;
   s16b cnt;

   is_max = 0;
   for (i=0; i<MAX_IS_IDX;i++)
   {
      item_set_type *is_ptr = &is_list[i]; /* check an item set in is_list */
      if (is_ptr->inuse==FALSE)            /* if it's unused         */
      {
         j = i;                             /* check the sets above it */
         while ((is_ptr->inuse==FALSE) && (j<MAX_IS_IDX))
         {
           j++;
           if (j==MAX_IS_IDX) break;
           is_ptr = &is_list[j];            /* continue checking until an */
         }                                  /* used one is found */
         if (j>is_max)
         {
            break;
         }
         is_list[i] = is_list[j];           /* move that down */
         is_list[j].inuse = FALSE;          /* free the original one */
         is_max++;                          /* we now have a real traps */
         cnt = 0;
         for (x=0;x<cur_wid;x++)
         {
            for (y=0;y<cur_hgt;y++)
            {
               c_ptr = &dungeon.level[sublevel][y][x];           /* change all references */
               if ((c_ptr->i_idx & ITEM_SET_FLAG) &&
                   ((c_ptr->i_idx & ITEM_SET_MASK)== j))
               {
                  c_ptr->i_idx = i;
                  c_ptr->i_idx |= ITEM_SET_FLAG;
                  cnt++;
               }
            }
         }
      }
      else
      {
         is_max++; /* we have found a used item set */
      }
   }
}

bool trap_found_ptr(trap_item_type *tr_ptr, s16b trap)
{
   s16b i;
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      if (tr_ptr->type[i]==trap)
      {
dlog(DEBUGTRAPS,"object3.c: trap_found_ptr: trap found, returning %d\n", tr_ptr->found[i]);
         return (tr_ptr->found[i]);
      }
   }
dlog(DEBUGTRAPS,"object3.c: trap_found_ptr: returning FALSE\n");
   return (FALSE);
}

bool trap_found_xy(s16b x, s16b y, s16b trap)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   trap_item_type *tr_ptr;

   if (!c_ptr->t_idx) return (FALSE);
   tr_ptr = &t_list[c_ptr->t_idx];
   return (trap_found_ptr(tr_ptr, trap));
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
void place_trap(s16b x, s16b y, s16b level, s16b chance_one, s16b chance_all)
{
   trap_item_type *tr_ptr;
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   s16b tr_idx;
   s16b i;

   /* Paranoia -- verify location */
   if (!in_bounds(x, y)) return;

   /* Require empty, clean, floor grid */
   if (!naked_grid_bold(x, y))
   {
dlog(DEBUGTRAPS,"object3.c: place_trap: grid %d,%d not naked_grid_bold\n", x, y);
      return;
   }

   if (!p_ptr->mdepth) return;

   /* Access the grid */
   c_ptr = &dungeon.level[sublevel][y][x];

   if ( (c_ptr->mtyp == DUNG_WILDN) ||
        (c_ptr->mtyp == DUNG_SHRUB) ||
        (c_ptr->mtyp == DUNG_WATER) )
   {
dlog(DEBUGTRAPS,"object3.c: place_trap: grid %d,%d unsuitable terrain mtyp %d\n", x, y, c_ptr->mtyp);
      return;
   }

   /* Place an invisible trap */

   tr_idx = t_pop(); /* get a free trap_item */
   if (tr_idx!=-1)
   {
      bool retval;

      tr_ptr = &t_list[tr_idx];
dlog(DEBUGTRAPS,"object3.c: place_trap: about to call set_traps @ %d,%d\n",x,y);

      retval = set_traps(tr_ptr, &i, level, FTRAP_DOOR, chance_one, chance_all);

dlog(DEBUGTRAPS,"object3.c: place_trap: set_traps called, returned %d, FALSE %d\n", retval, FALSE);

      if (retval == FALSE)
      {
dlog(DEBUGTRAPS,"object3.c: place_trap: no correct traps (floor) %d,%d\n",x,y);
         tr_ptr->inuse = FALSE;
         c_ptr->t_idx = 0;
         (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
         return;
      }
      /* paranoia time! */
      if (num_traps_ptr(tr_ptr, TRAP_EXISTS)==0)
      {
dlog(DEBUGTRAPS,"object3.c: place_trap: no existing traps (floor) %d,%d\n",x,y);
         tr_ptr->inuse = FALSE;
         c_ptr->t_idx = 0;
         (void)set_grid_type(x, y, DUNG_FLOOR,
                            DUNG_FLOOR_NORMAL, GRID_ADD, 0);
         return;
      }
      else
      {
         s16b numfnd = num_traps_ptr(tr_ptr, TRAP_FOUND);
         if (numfnd>0)
         {
            (void)set_grid_type(x, y, DUNG_TRAP, (numfnd>1)?DUNG_TRAP_FNDONE:DUNG_TRAP_FNDMORE,
                                GRID_ADD, 0);
         }
         else
         {
            (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_TRAP, GRID_ADD, 0);
         }
         c_ptr->t_idx = tr_idx; /* point towards our special trap item */
dlog(DEBUGTRAPS,"object3.c: place_trap 0x02 at %d,%d t_idx %d\n",x,y,tr_idx);
dlog(DEBUGTRAPS,"object3.c: place_trap: floor mt %d st %d fdat %08lx\n",
                c_ptr->mtyp, c_ptr->styp, c_ptr->fdat);
{
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      dlog(DEBUGTRAPS,"object3.c: place_trap: type[%d]=%d, found[%d]=%d\n",
                       i, tr_ptr->type[i], i, tr_ptr->found[i]);
   }
}
dlog(DEBUGTRAPS,"object3.c: %d,%d: %d traps known, %d traps unknown, %d traps total\n",
                x, y, num_traps_xy(x, y, TRAP_FOUND),
                num_traps_xy(x, y, TRAP_NOTFOUND),
                num_traps_xy(x, y, TRAP_EXISTS));

         tr_ptr->tx = x;
         tr_ptr->ty = y;
      }
   }
   else
   {
      quit("Creating trap for floor failed");
   }
}

/* jk */
void set_traps_chest(object_type *i_ptr, s16b level,
                     s16b chance_one, s16b chance_all)
{
   trap_item_type *tr_ptr;
   s16b tr_idx = t_pop();
   s16b p1valinc;

dlog(DEBUGTRAPS,"About to set traps for chest at level %d\n",level);
   if (tr_idx!=-1)
   {
      tr_ptr = &t_list[tr_idx];
      if (!set_traps(tr_ptr, &p1valinc, level, FTRAP_CHEST,
                     chance_one, chance_all))
      {
dlog(DEBUGTRAPS,"There are no correct traps for this chest (level %d)?\n", level);
         tr_ptr->inuse = FALSE;
         i_ptr->xtra2 = 0;
         return;
      }
      /* paranoia time! */
      if (num_traps_ptr(tr_ptr, TRAP_EXISTS)==0)
      {
         tr_ptr->inuse = FALSE;
         i_ptr->xtra2 = 0;
         return;
      }
      else
      {
dlog(DEBUGTRAPS,"object3.c: place_trap_chest t_idx %d\n",tr_idx);
         i_ptr->xtra2 = tr_idx; /* point towards our special trap item */
         i_ptr->p1val += p1valinc;
      }
   }
   else
   {
      quit("Creating trap for chest failed");
   }
}

/*
 * this function tries to add one or more traps to tr_ptr
 *
 * chance_one is the chance that one of them is known to you
 * chance_all is the chance that all of them are known to you
 *
 * it returns TRUE if any traps were added
 */
bool set_traps(trap_item_type *tr_ptr,s16b *p1valinc, s16b level, u32b flags,
               s16b chance_one, s16b chance_all)
{
   bool           more       = TRUE;
   bool           done_first = FALSE;
   bool           find_all   = FALSE;
   bool           result;
   s16b           trap;
   trap_type     *t_ptr;
   s16b           cnt        = 0;

dlog(DEBUGTRAPS,"object3.c: set_traps: level %d flags %08lx chance1 %d chance_all %d\n",
                level, flags, chance_one, chance_all);

   if (chance_one<0) chance_one = 0;
   if (chance_all<0) chance_all = 0;
   find_all = (randint(100)<chance_all);
   (*p1valinc) = 0;

   init_trap(tr_ptr);

   if (level<=1) return (FALSE); /* no traps in town or on first level */
   while ((more) && (cnt++)<100) /* try 100 times */
   {
      trap = rand_int(t_number);
      t_ptr = &t_info[trap];
      if (t_ptr->minlevel>level) continue; /* no traps below their minlevel */
      if (get_trap(tr_ptr,trap)) continue; /* no use in installing twice    */
      if (!(t_ptr->flags & flags)) continue; /* is this a correct trap now?   */
      if (rand_int(100)<t_ptr->probability) /* how probable is this trap   */
      {
         set_trap(tr_ptr,trap,TRUE);
dlog(DEBUGTRAPS,"object3.c: set_traps: setting trap %d, prob %d prob next %d minlevel %d = %s\n",
                trap, t_ptr->probability, t_ptr->another, t_ptr->minlevel, t_name + t_ptr->name);
         if (find_all ||
             (!done_first && (randint(100)<chance_one)))
         {
            done_first = TRUE;
            set_trap_found_ptr(tr_ptr, trap);
         }
         (*p1valinc) +=t_ptr->p1valinc;
         if (rand_int(100)>t_ptr->another)
         {
            more=FALSE;
dlog(DEBUGTRAPS,"object3.c: set_traps: no more\n");
         }
      } /* probability in range */
   } /* add more traps */

   /* did we make any? */
   result = (first_trap(tr_ptr) != -1);

   if (result) tr_ptr->inuse = TRUE;

dlog(DEBUGTRAPS,"object3.c: set_traps: first_trap %d, returning %d\n",
                first_trap(tr_ptr), result);
for (trap=0; trap < MAX_TRAPS_IN_SET; trap++)
{
   dlog(DEBUGTRAPS,"object3.c: set_traps: type[%d] %d found[%d] %d\n",
                   trap, tr_ptr->type[trap], trap, tr_ptr->found[trap]);
}
   return (result);
}

s16b num_traps_ptr(trap_item_type *tr_ptr, s16b found_status)
{
   s16b            i, result=0;
dlog(DEBUGTRAPS,"object3.c: num_traps_ptr: searching for %d\n",found_status);

   for (i=0;i<MAX_TRAPS_IN_SET;i++)
   {
      if (tr_ptr->type[i]!=0)
      {
dlog(DEBUGTRAPS,"object3.c: num_traps_ptr: type[%d]=%d found %d\n",
                i, tr_ptr->type[i], tr_ptr->found[i]);
         switch(found_status)
         {
            case TRAP_EXISTS:   result++;
dlog(DEBUGTRAPS,"object3.c: TRAP_EXISTS   : result now %d\n", result);
                                break;
            case TRAP_FOUND:    if (tr_ptr->found[i]) result++;
dlog(DEBUGTRAPS,"object3.c: TRAP_FOUND    : result now %d\n", result);
                                break;
            case TRAP_NOTFOUND: if (!tr_ptr->found[i]) result++;
dlog(DEBUGTRAPS,"object3.c: TRAP_NOTFOUND : result now %d\n", result);
                                break;
         }
dlog(DEBUGTRAPS,"object3.c: num_traps_ptr: trap %d after test result %d\n",
                i, result);
      }
   }
dlog(DEBUGTRAPS,"object3.c: num_traps_ptr: returning %d\n", result);
   return (result);
}

s16b num_traps_xy(s16b x, s16b y, s16b found_status)
{
   cave_cell_type      *c_ptr = &dungeon.level[sublevel][y][x];
   trap_item_type *tr_ptr;
   if (!c_ptr->t_idx) return (0);
   tr_ptr = &t_list[c_ptr->t_idx];
   return (num_traps_ptr(tr_ptr, found_status));
}

/*
 * this function describes the traps on a chest in buf
 *
 * it returns FALSE on any errors
 * only one error is supported: if the items is not a chest.
 *
 * please note that found traps needn't be known traps,
 */
bool describe_trap(char *buf, trap_item_type *tr_ptr)
{
   trap_type       *t_ptr = NULL;
   s16b             cnt_found, cnt_identified, i;
   s16b             found[MAX_TRAPS_IN_SET];
   char            *t = buf;

   cnt_found = 0;
   cnt_identified = 0;
   for (i=0; i<MAX_TRAPS_IN_SET; i++)
   {
      /* is there a trap here that we have found? */
      if ((tr_ptr->type[i]!=0) && tr_ptr->found[i])
      {
         found[cnt_found++]=tr_ptr->type[i];
         if (t_info[tr_ptr->type[i]].ident) cnt_identified++;
      }
   }
dlog(DEBUGTRAPS,"object3.c: describe_trap: %d found, %d identified\n",
                cnt_found, cnt_identified);

   /* one found, and one identified */
   if ((cnt_found==1) && (cnt_identified==1))
   {
      t_ptr=&t_info[found[0]];
      t=object_desc_str(t,format("with a %s",t_name+t_ptr->name));
   }
   /* one found, and not identified */
   else if (cnt_found==1)
   {
      t=object_desc_str(t, "(trapped)");
   }
   /* we cannot describe more known traps, so 'l'ook at it for more info! */
   else if (cnt_found>1)
   {
      t=object_desc_str(t, "with multiple traps");
   }
   *t = '\0';
dlog(DEBUGTRAPS,"object3.c: describe_trap: returning\n");
   return (TRUE);
}

bool inspect_trap_chest(object_type *i_ptr)
{
   trap_type *t_ptr;
   trap_item_type *tr_ptr;

   s16b       cnt_found,cnt_unfound,i;
   s16b       index_found[MAX_TRAPS_IN_SET],index_unfound[MAX_TRAPS_IN_SET];

   if (i_ptr->tval!=TV_CHEST) return (FALSE);
   if (!i_ptr->xtra2)
   {
      msg_print("The chest is not trapped");
      return(TRUE);
   }

   tr_ptr = &t_list[i_ptr->xtra2];
   cnt_unfound = 0;
   cnt_found = 0;
   for (i=1;i<t_number;i++)
   {
      if (get_trap(tr_ptr,i))
      {
         t_ptr=&t_info[i];
         if (t_ptr->known)
         {
            index_found[cnt_found]=i;
            cnt_found++;
         }
         else
         {
            index_unfound[cnt_unfound]=i;
            cnt_unfound++;
         }
      }
   }
   if (cnt_unfound==1)
   {
      msg_print("You see one unknown trap.");
   }
   if (cnt_unfound>1)
   {
      msg_print("You see some unknown traps.");
   }
   for (i=0;i<cnt_found;i++)
   {
      t_ptr = &t_info[index_found[i]];
      msg_print(format("You see a %s trap",t_name+t_ptr->name));
   }
   return (TRUE);
}

bool inspect_trap_door(s16b x, s16b y)
{
   trap_type *t_ptr;
   trap_item_type *tr_ptr;

   s16b       cnt_found,cnt_unfound,i;
   s16b       index_found[MAX_TRAPS_IN_SET],index_unfound[MAX_TRAPS_IN_SET];

   if (!dungeon.level[sublevel][y][x].t_idx)
   {
     msg_print("The door is not trapped");
     return(TRUE);
   }

   tr_ptr = &t_list[dungeon.level[sublevel][y][x].t_idx];
   cnt_unfound = 0;
   cnt_found = 0;
   for (i=1;i<t_number;i++)
   {
      if (get_trap(tr_ptr,i))
      {
         t_ptr=&t_info[i];
         if (t_ptr->known)
         {
            index_found[cnt_found]=i;
            cnt_found++;
         }
         else
         {
            index_unfound[cnt_unfound]=i;
            cnt_unfound++;
         }
      }
   }
   if (cnt_unfound==1)
   {
      msg_print("You see one unknown trap.");
   }
   if (cnt_unfound>1)
   {
      msg_print("You see some unknown traps.");
   }
   for (i=0;i<cnt_found;i++)
   {
      t_ptr = &t_info[index_found[i]];
      msg_print(format("You see a %s trap",t_name+t_ptr->name));
   }
   return (TRUE);
}

bool test_activate_trap_dex(s16b trap)
{
   s16b k;
   s16b power;
   s16b chance;
   trap_type *t_ptr = &t_info[trap];

   k = rand_int(100);
   power = t_ptr->difficulty;
   chance = adj_dex_safe[p_ptr->stat_ind[A_DEX]]/2;
/* dex 3 - 0 to dex 18/220+ - 100 ie 0 to 20 */
/* power ranges from 0 to 12 I think */

   /* Hack -- 5% hit, 5% miss */
   if (k < 10) return (k < 5);

   return ((k+power)>chance);
}

bool test_activate_trap_knowledge(s16b trap)
{
   s16b k;
   s16b sq;
   trap_type *t_ptr = &t_info[trap];

   k = rand_int(100);
   sq = 0;
   while ((sq*sq)<t_ptr->known) sq++; /* sq approaches sqrt(known)+1 */
/* this makes sq range from 1 to 173 for 0 to 30000 disarmings */
/* for a change of 50 we need to have disarmed a trap 2500 times */
/* so we multiply k by the difficulty/4 - 1 to 3 */
   k *= (t_ptr->difficulty/3);

   return (k>sq);
}

s16b get_random_trap(trap_item_type *tr_ptr, s16b found_status)
{
   s16b trap, k, cnt = 0;
   s16b index[t_number];

   for (k=1;k<t_number;k++)    /* find out which traps exist in tr_ptr */
   {
      if (get_trap(tr_ptr,k))
      {
         switch(found_status)
         {
           case TRAP_EXISTS: index[cnt++]=k;
                                break;
           case TRAP_FOUND:   if (trap_found_ptr(tr_ptr, k)) index[cnt++]=k;
                                break;
           case TRAP_NOTFOUND:    if (!trap_found_ptr(tr_ptr, k)) index[cnt++]=k;
                                break;
         }
      }
   }
   if (cnt==0) return (-1);
   trap = index[rand_int(cnt)]; /* choose a random trap */
   return (trap);
}

bool do_player_trap_call_out(void)
{
   s16b          i,sn,cx,cy;
   s16b          h_index = 0;
   s16b          h_level = 0;
   monster_type  *m_ptr;
   monster_race  *r_ptr;
   char          m_name[80];
   bool          ident = FALSE;

   for (i = 1; i < mn_max; i++)
   {
       m_ptr = &mn_list[i];
       r_ptr = &r_info[m_ptr->r_idx];

       /* Paranoia -- Skip dead monsters */
       if (!m_ptr->r_idx) continue;
       /* don't call them from other sublevels! */
       if (m_ptr->fz != sublevel) continue;

       if (r_ptr->level>=h_level)
       {
         h_level = r_ptr->level;
         h_index = i;
       }
   }
   /* if the level is empty of monsters, h_index will be 0 */
   if (!h_index) return(FALSE);

   m_ptr = &mn_list[h_index];

   sn = 0;
   for (i = 0; i < 8; i++)
   {
      cx = px + ddx[i];
      cy = py + ddy[i];
      /* Skip non-empty grids */
      if (!empty_grid_bold(cx, cy)) continue;
      if (test_grid(cx, cy, DUNG_FLOOR, DUNG_FLOOR_GLYPH)) continue;
      if ((cx==px) && (cy==py)) continue;
      sn++;
      /* Randomize choice */
      if (rand_int(sn) > 0) continue;
      dungeon.level[sublevel][cy][cx].m_idx=h_index;
      dungeon.level[sublevel][m_ptr->fy][m_ptr->fx].m_idx=0;
      m_ptr->fx = cx;
      m_ptr->fy = cy;
      /* we do not change the sublevel! */
      ident=TRUE;
      update_mon(h_index, TRUE);
      monster_desc(m_name, m_ptr, 0x08);
      msg_format("You hear a rapid-shifting wail, and %s appears!",m_name);
      break;
   }
   return (ident);
}

static bool do_trap_teleport_away(object_type *i_ptr, s16b item, s16b x, s16b y)
{
   bool more = TRUE;
   bool ident = FALSE;

   s16b  x1  = rand_int(cur_wid);
   s16b  y1  = rand_int(cur_hgt);

   while (more)
   {
      if (clean_enough_floor(x1,y1))
      {
         i_ptr->ix = x1;
         i_ptr->iy = y1;
         set_floor_item_xy(floor_free_index(x1,y1),
                           get_floor_item_xy(item-INVEN_TOTAL,x,y),x1,y1);
         set_floor_item_xy(item-INVEN_TOTAL,0,x,y);
         optimize_floor(x,y);
         optimize_floor(x1,y1);
         more=FALSE; /* we may now stop */
         if (!p_ptr->blind)
         {
            note_spot(x,y);
            lite_spot(x,y);
            ident=TRUE;
            if (player_has_los_bold(x1, y1))
            {
               lite_spot(x1, y1);
               msg_print("The chest suddenly stands elsewhere");

            }
            else
            {
               msg_print("You suddenly don't see the chest anymore!");
            }
         }
         else
         {
            msg_print("You hear something move");
         }
      }
      else
      {
         x1 = rand_int(cur_wid);
         y1 = rand_int(cur_hgt);
      }
   }
   return (ident);
}

/*
 * this function handles arrow & dagger traps, in various types.
 * num = number of missiles
 * tval, sval = kind of missiles
 * dd,ds = damage roll for missiles
 * poison_dam = additional poison damage
 * name = name given if you should die from it...
 *
 * return value = ident (always TRUE)
 */
static bool player_handle_missile_trap(s16b num, s16b tval, s16b sval, s16b dd, s16b ds,
                                     s16b pdam, cptr name)
{
   object_type forge;
   s16b        i, k_idx = lookup_kind(tval, sval);
   char        i_name[80];

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   for (i=0; i < num; i++)
   {
      invcopy(&forge, k_idx);
      apply_magic(&forge, p_ptr->mdepth, FALSE, FALSE, FALSE);
      forge.log.where = OBJ_FOUND_TRAP;
      forge.log.whose = 0;
      forge.log.mlevel = p_ptr->mdepth;
      forge.log.slevel = p_ptr->sdepth;
      object_desc(i_name, &forge, TRUE, 3);
      switch (randint(4))
      {
         case 1: msg_format("Suddenly %s hits you!", i_name); break;
         case 2: msg_format("Suddenly %s hits you in a tender spot!", i_name); break;
         case 3: msg_format("Suddenly %s flies at you!", i_name); break;
         case 4: i_name[0] = toupper(i_name[0]);
                 msg_format("%s embeds itself in your flesh.", i_name);
                 break;
      }

      take_hit(damroll(dd, ds), name);
      redraw_stuff();
      if (pdam > 0)
      {
         if (p_ptr->resist_pois || p_ptr->oppose_pois)
         {
            (void)set_poisoned(pdam / 9);
         }
         else
         {
            set_poisoned(pdam);
         }
         (void)poison_dam(pdam);
      }
      drop_near(&forge, 0, px, py, drop_how(&forge), FALSE, FALSE);
   }
   return TRUE;
}

/*
 * this function handles arrow & dagger traps, in various types.
 * num = number of missiles
 * tval, sval = kind of missiles
 * dd,ds = damage roll for missiles
 * poison_dam = additional poison damage
 * name = name given if you should die from it...
 *
 * return value = ident (always TRUE)
 */
static bool monster_handle_missile_trap(s16b m_idx, s16b num, s16b tval, s16b sval, s16b dd, s16b ds,
                                        s16b pdam, char *name)
{
   object_type   forge;
   monster_type *m_ptr = &mn_list[m_idx];
   char          m_name[80];
   s16b          i, k_idx = lookup_kind(tval, sval);
   bool          fear = FALSE, visible = TRUE;

   if (p_ptr->blind) visible = FALSE;
   if (!player_has_los_bold(m_ptr->fx, m_ptr->fy)) visible = FALSE;
   if (!m_ptr->ml) visible = FALSE;

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   monster_desc(m_name, &mn_list[m_idx], 0x4);

dlog(DEBUGTRAPS,"object3.c: monster_handle_missile_trap: m_idx %d num %d tv %d sv %d %dd%d dam %d name %s\n",
                m_idx, num, tval, sval, dd, ds, pdam, name);

   for (i=0; i < num; i++)
   {
      project_who_type who;

      invcopy(&forge, k_idx);
      apply_magic(&forge, p_ptr->mdepth, FALSE, FALSE, FALSE);
      forge.log.where = OBJ_FOUND_TRAP;
      forge.log.whose = 0;
      forge.log.mlevel = p_ptr->mdepth;
      forge.log.slevel = p_ptr->sdepth;
      if (visible) msg_format("%^s hits a %s.", m_name, name);
      /* did we kill this monster? */
      who.type = WHO_MONSTER;
      who.index = m_idx;
      who.index_race = m_ptr->r_idx;

      if (!mon_take_hit(&who, m_idx, damroll(dd, ds), &fear, NULL))
      {
         if (pdam>0)
         {
            test_monster_inven_damage(m_idx, GF_POIS, pdam);
         }
      }
      else
      {
         i = num; /* no more daggers, please! */
      }
      drop_near(&forge, 0, m_ptr->fx, m_ptr->fy, drop_how(&forge), FALSE, FALSE);
   }
   return TRUE;
}


/*
 * this handles a trap that places walls around the player
 */
static bool player_handle_trap_of_walls(void)
{
   bool ident;

   {
      s16b dx,dy,cx,cy;
      s16b sx=0,sy=0,sn,i;
      cave_cell_type *cv_ptr;
      bool map[5][5] = { {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE} };
      for (dy = -2; dy <= 2; dy++)
      {
         for (dx = -2; dx <= 2; dx++)
         {
            cx = px+dx;
            cy = py+dy;
            if (!in_bounds(cx, cy)) continue;
            cv_ptr = &dungeon.level[sublevel][cy][cx];
            /* Lose room and vault */
            cv_ptr->fdat &= ~(CAVE_ROOM | CAVE_VAULT);
            /* Lose light and knowledge */
            cv_ptr->fdat &= ~(CAVE_GLOW | CAVE_MARK);
            /* Skip the center */
            if (!dx && !dy) continue;
            /* test for dungeon level */
            if (randint(100) > 10+p_ptr->mdepth) continue;
            /* Damage this grid */
            map[2+dx][2+dy] = TRUE;
         }
      }
      for (dy = -2; dy <= 2; dy++)
      {
         for (dx = -2; dx <= 2; dx++)
         {
            cx = px+dx;
            cy = py+dy;
            if (!map[2+dx][2+dy]) continue;
            cv_ptr = &dungeon.level[sublevel][cy][cx];

            if (cv_ptr->m_idx)
            {
               monster_type *m_ptr = &mn_list[cv_ptr->m_idx];
               monster_race *r_ptr = &r_info[m_ptr->r_idx];
               /* Most monsters cannot co-exist with rock */
               if (!(r_ptr->flags2 & RF2_KILL_WALL) &&
                   !(r_ptr->flags2 & RF2_PASS_WALL))
               {
                  char m_name[80];

                  /* Assume not safe */
                  sn = 0;
                  /* Monster can move to escape the wall */
                  if (!(r_ptr->flags1 & RF1_NEVER_MOVE))
                  {
                     /* Look for safety */
                     for (i = 0; i < 8; i++)
                     {
                        /* Access the grid */
                        cx = px + ddx[i];
                        cy = py + ddy[i];

                        /* Skip non-empty grids */
                        if (!empty_grid_bold(cx, cy)) continue;

                        /* Hack -- no safety on glyph of warding */
                        if (test_grid(cx, cy, DUNG_FLOOR, DUNG_FLOOR_GLYPH))
                           continue;

                        /* Important -- Skip "quake" grids */
                        if (map[2+(cx-px)][2+(cy-py)]) continue;
                        /* Count "safe" grids */
                        sn++;
                        /* Randomize choice */
                        if (rand_int(sn) > 0) continue;
                        /* Save the safe grid */
                        sx = cx; sy = cy;
                        ident=TRUE;
                        break; /* discontinue for loop - safe grid found */
                     }
                  }
                  /* Describe the monster */
                  monster_desc(m_name, m_ptr, 0x4);
                  /* Scream in pain */
                  msg_format("%^s wails out in pain!", m_name);
                  /* Monster is certainly awake */
                  m_ptr->csleep = 0;
                  /* Apply damage directly */
                  m_ptr->hp -= (sn ? damroll(4, 8) : 200);
                  /* Delete (not kill) "dead" monsters */
                  if (m_ptr->hp < 0)
                  {
                     /* Message */
                     msg_format("%^s is entombed in the rock!", m_name);
                     /* Delete the monster */
dlog(DEBUGMONST,"object3.c: player_handle_trap_of_walls: m_idx %d (%s) just got entombed @ %d,%d\n",
                cv_ptr->m_idx,r_name+r_ptr->name, m_ptr->fx, m_ptr->fy);
                     delete_monster_idx(dungeon.level[sublevel][cy][cx].m_idx);
                     /* No longer safe */
                     sn = 0;
                  }
                  /* Hack -- Escape from the rock */
                  if (sn)
                  {
                     s16b m_idx = dungeon.level[sublevel][cy][cx].m_idx;
                     /* Update the new location */
                     dungeon.level[sublevel][sy][sx].m_idx = m_idx;
                     /* Update the old location */
                     dungeon.level[sublevel][cy][cx].m_idx = 0;
                     /* Move the monster */
                     m_ptr->fy = sy;
                     m_ptr->fx = sx;
                     /* do not change fz */
                     /* don't make rock on that square! */
                     if ( (sx>=(px-2)) &&
                        (sx<=(px+2)) &&
                        (sy>=(py-2)) &&
                        (sy<=(py+2)) )
                     {
                        map[2+(sx-px)][2+(sy-py)]=FALSE;
                     }
                     /* Update the monster (new location) */
                     update_mon(m_idx, TRUE);
                     /* Redraw the old grid */
                     lite_spot(cx, cy);
                     /* Redraw the new grid */
                     lite_spot(sx, sy);
                  } /* if sn */
               } /* if monster can co-exist with rock */
            } /* if monster on square */
         } /* for dx */
      } /* for dy */

      /* Examine the quaked region */
      for (dy = -2; dy <= 2; dy++)
      {
         for (dx = -2; dx <= 2; dx++)
         {
            /* Extract the location */
            cx = px + dx;
            cy = py + dy;

            /* Skip unaffected grids */
            if (!map[2+dx][2+dy]) continue;

            /* Access the cave grid */
            cv_ptr = &dungeon.level[sublevel][cy][cx];

            /* Paranoia -- never affect player */
            if (!dy && !dx) continue;

            /* Destroy location (if valid) */
            if (valid_grid(cx, cy))
            {
               bool floor = floor_grid_bold(cx, cy);

               /* Delete any object that is still there */
               delete_object(cx, cy, -1);

               if (floor)
               {
                  place_wall(cx, cy);
               }
               else
               {
                  /* Clear previous contents, add floor */
                  (void)set_grid_type(cx, cy, DUNG_FLOOR,
                                      DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
               }
            } /* valid */
         } /* dx */
      } /* dy */
      /* Mega-Hack -- Forget the view and lite */
      p_ptr->update |= PU_UN_VIEW;

      /* Update stuff */
      p_ptr->update |= (PU_VIEW | PU_FLOW);

      /* Update the monsters */
      p_ptr->update |= (PU_DISTANCE);

      /* Update the health bar */
      p_ptr->redraw1 |= (PR1_HEALTH);

      /* Redraw map */
      p_ptr->redraw1 |= (PR1_MAP);

      /* Window stuff */
      p_ptr->window |= (PW_OVERHEAD);
      handle_stuff();
      msg_print("Suddenly the cave shifts around you. The air is getting stale!");
      ident=TRUE;
   }
   return (ident);
}

/*
 * this function handles a "breath" type trap - acid bolt, lightning balls etc.
 */
static bool player_handle_breath_trap(s16b x, s16b y, s16b rad, s16b type, u16b trap)
{
   trap_type *t_ptr = &t_info[trap];
   project_who_type who;

   bool       ident;
   s16b       my_dd, my_ds, dam;

   my_dd = t_ptr->dd;
   my_ds = t_ptr->ds;

   /* these traps gets nastier as levels progress */
   if (p_ptr->mdepth > (2* t_ptr->minlevel))
   {
      my_dd += (p_ptr->mdepth / 15);
      my_ds += (p_ptr->mdepth / 15);
   }
   dam = damroll(my_dd, my_ds);

dlog(DEBUGTRAPS,"object3.c: player_handle_breath_trap: @ %d,%d rad %d type %d trap %d dam %d\n",
                x, y, rad, type, trap, dam);
   who.type = WHO_TRAPBYPLAYER;
   who.index = trap;
   ident = project(&who, rad, x, y, dam, type, PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
   return (ident);
}

/*
 * this function handles a "breath" type trap - acid bolt, lightning balls etc.
 */
static bool monster_handle_breath_trap(s16b m_idx, s16b rad, s16b type, u16b trap)
{
   trap_type        *t_ptr = &t_info[trap];
   monster_type     *m_ptr = &mn_list[m_idx];
   project_who_type  who;

   bool              ident, visible;
   s16b              my_dd, my_ds, dam;
   char              m_name[80];

   my_dd = t_ptr->dd;
   my_ds = t_ptr->ds;

   /* these traps gets nastier as levels progress */
   if (p_ptr->mdepth > (2* t_ptr->minlevel))
   {
      my_dd += (p_ptr->mdepth / 15);
      my_ds += (p_ptr->mdepth / 15);
   }
   dam = damroll(my_dd, my_ds);

dlog(DEBUGTRAPS,"object3.c: monster_handle_breath_trap: step 1\n");
   visible = TRUE;
   if (p_ptr->blind) visible = FALSE;
   if (!player_has_los_bold(m_ptr->fx, m_ptr->fy)) visible = FALSE;
   if (!m_ptr->ml) visible = FALSE;

   if (visible)
   {
      monster_desc(m_name, m_ptr, 0x4);
      msg_format("%^s triggers a trap.", m_name);
   }
dlog(DEBUGTRAPS,"object3.c: monster_handle_breath_trap: step 2\n");

   who.type = WHO_TRAPBYMONSTER;
   who.index = trap;
   who.trigger = m_idx;
   who.trigger_race = m_ptr->r_idx;

   ident = project(&who, rad, m_ptr->fx, m_ptr->fy, dam, type, PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
dlog(DEBUGTRAPS,"object3.c: monster_handle_breath_trap: step 3\n");
   return (ident);
}

/*
 * this handles forced dropping of any items in the inventory of a monster */
static bool handle_monster_drop_item(s16b m_idx, s16b chance)
{
   monster_type *m_ptr = &mn_list[m_idx];
   char          m_name[80];
   bool          visible, found = FALSE;

   visible = TRUE;
   monster_desc(m_name, m_ptr, 0x10);

   if (p_ptr->blind) visible = FALSE;
   if (!player_has_los_bold(m_ptr->fx, m_ptr->fy)) visible = FALSE;
   if (!m_ptr->ml) visible = FALSE;


   if (m_ptr->has_drop &&
       (!(r_info[m_ptr->r_idx].flags1 & RF1_DROP_CHOSEN)) )
   {
      s16b number, i;
      s16b is_idx = item_set_this_monster(m_idx);
      if (is_idx == -1)
      {
dlog(DEBUGALWAYS,"object3.c: handle_monster_drop_item: has_drop true, is_idx -1?\n");
         return (FALSE);
      }
      number = items_in_set(is_idx);
      if (number==0)
      {
dlog(DEBUGALWAYS,"object3.c: handle_monster_drop_item: has_drop true, number == 0?\n");
         return (FALSE);
      }
      /* Scan through the slots backwards */
      for (i = number-1; i >= 0; i--)
      {
         /* Get the item in that slot */
         object_type *j_ptr = &i_list[is_list[is_idx].index[i]];

         /* Hack -- for now, skip artifacts */
         if (artifact_p(j_ptr)) continue;

         /* Give this item slot a shot at death */
         if (randint(100)<chance)
         {
            object_type forge;

            /* strange things happen otherwise, you have been warned! */
            forge.spell_set = 0;
            invwipe(&forge);

            /* increase chest rating */
            forge = i_list[is_list[is_idx].index[i]];
            forge.log.where = OBJ_FOUND_TRAP;
            forge.log.whose = m_ptr->r_idx;
            forge.log.mlevel = p_ptr->mdepth;
            forge.log.slevel = p_ptr->sdepth;
            drop_near(&forge, 0, m_ptr->fx, m_ptr->fy, 0, FALSE, FALSE);

            /* Destroy "amt" items */
            monster_inven_increase(m_idx,is_idx,i,-99);
            monster_inven_optimize(m_idx,is_idx);

            /* more items is not so probable */
            found = TRUE;
         }
      }
   }
   if (visible)
   {
      msg_format("%^s triggers a trap.", m_name);
   }
   return (found);
}

/*
 * this function activates one trap type, and returns
 * a bool indicating if this trap is now identified
 */
static bool player_activate_trap_type(s16b x, s16b y, object_type *i_ptr,
                               s16b item, trap_item_type *tr_ptr, s16b trap)
{
   bool ident = FALSE;

   s16b k;
dlog(DEBUGTRAPS,"object3.c: player_activate_trap_type: trap %d\n", trap);

   switch(trap)
   {
      /* stat traps */

      case TRAP_OF_WEAKNESS_I:       ident=do_dec_stat(A_STR, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_WEAKNESS_II:      ident=do_dec_stat(A_STR, STAT_DEC_NORMAL); break;
      case TRAP_OF_WEAKNESS_III:     ident=do_dec_stat(A_STR, STAT_DEC_PERMANENT); break;
      case TRAP_OF_INTELLIGENCE_I:   ident=do_dec_stat(A_INT, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_INTELLIGENCE_II:  ident=do_dec_stat(A_INT, STAT_DEC_NORMAL); break;
      case TRAP_OF_INTELLIGENCE_III: ident=do_dec_stat(A_INT, STAT_DEC_PERMANENT); break;
      case TRAP_OF_WISDOM_I:         ident=do_dec_stat(A_WIS, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_WISDOM_II:        ident=do_dec_stat(A_WIS, STAT_DEC_NORMAL); break;
      case TRAP_OF_WISDOM_III:       ident=do_dec_stat(A_WIS, STAT_DEC_PERMANENT); break;
      case TRAP_OF_FUMBLING_I:       ident=do_dec_stat(A_DEX, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_FUMBLING_II:      ident=do_dec_stat(A_DEX, STAT_DEC_NORMAL); break;
      case TRAP_OF_FUMBLING_III:     ident=do_dec_stat(A_DEX, STAT_DEC_PERMANENT); break;
      case TRAP_OF_WASTING_I:        ident=do_dec_stat(A_CON, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_WASTING_II:       ident=do_dec_stat(A_CON, STAT_DEC_NORMAL); break;
      case TRAP_OF_WASTING_III:      ident=do_dec_stat(A_CON, STAT_DEC_PERMANENT); break;
      case TRAP_OF_BEAUTY_I:         ident=do_dec_stat(A_CHR, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_BEAUTY_II:        ident=do_dec_stat(A_CHR, STAT_DEC_NORMAL); break;
      case TRAP_OF_BEAUTY_III:       ident=do_dec_stat(A_CHR, STAT_DEC_PERMANENT); break;

      /* Trap of Curse Weapon */
      case TRAP_OF_CURSE_WEAPON:
         ident = curse_weapon();
         break;
      /* Trap of Curse Armor */
      case TRAP_OF_CURSE_ARMOR:
         ident = curse_armor();
         break;
      /* Earthquake Trap */
      case TRAP_OF_EARTHQUAKE:
         msg_print("As you touch the trap, the ground starts to shake.");
         earthquake(x, y, 10);
         ident=TRUE;
         break;
      /* Poison Needle Trap */
      case TRAP_OF_POISON_NEEDLE:
         if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
         {
            msg_print("You prick yourself on a poisoned needle.");
            (void)set_poisoned(p_ptr->poisoned + rand_int(15) + 10);
            ident=TRUE;
         }
         else
         {
            msg_print("You prick yourself on a needle.");
         }
         break;
      /* Summon Monster Trap */
      case TRAP_OF_SUMMON_MONSTER:
         {
            msg_print("A spell hangs in the air.");
            for (k = 0; k < randint(3); k++) ident |= summon_specific(x, y, p_ptr->mdepth, 0, 0);
            break;
         }
      /* Summon Undead Trap */
      case TRAP_OF_SUMMON_UNDEAD:
         {
            msg_print("A mighty spell hangs in the air.");
            for (k = 0; k < randint(3); k++) ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_UNDEAD, 0);
            break;
         }
      /* Summon Greater Undead Trap */
      case TRAP_OF_SUMMON_GREATER_UNDEAD:
         {
            msg_print("An old and evil spell hangs in the air.");
            for (k = 0; k < randint(3); k++) ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_HI_UNDEAD, 0);
            break;
         }
      /* Teleport Trap */
      case TRAP_OF_TELEPORT:
         msg_print("The world whirls around you.");
         teleport_player(RATIO*67);
         ident=TRUE;
         break;
      /* Paralyzing Trap */
      case TRAP_OF_PARALYZING:
         if (!p_ptr->free_act)
         {
            msg_print("You touch a poisoned part and can't move.");
            (void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10);
            ident=TRUE;
         }
         else
         {
            msg_print("You prick yourself on a needle.");
         }
         break;
      /* Explosive Device */
      case TRAP_OF_EXPLOSIVE_DEVICE:
         {
            project_who_type who;
            msg_print("An hidden explosive device explodes in your face.");
            take_hit(damroll(5, 8), "an explosive trap");

            who.type = WHO_TRAPBYPLAYER;
            (void)aggravate_monsters(&who, -1);
            ident=TRUE;
            break;
         }
      /* Teleport Away Trap */
      case TRAP_OF_TELEPORT_AWAY:
         if (item<0)
         {
dlog(DEBUGALWAYS,"object3.c: player_activate_trap_type: TRAP_OF_TELEPORT_AWAY on non item %d @ %d,%d\n",
                 item, x, y);
         }
         else
         {
            ident = do_trap_teleport_away(i_ptr, item, x, y);
         }
         break;
      /* Lose Memory Trap */
      case TRAP_OF_LOSE_MEMORY:
         ident = wiz_forget_some_map(50+rand_int(50));
         ident |= lose_some_info(20+rand_int(20)); /* 20 % = 5, 40% is 10 items */
         ident |= dec_stat(A_WIS, rand_int(20)+10, STAT_DEC_NORMAL);
         ident |= dec_stat(A_INT, rand_int(20)+10, STAT_DEC_NORMAL);
         if (!p_ptr->resist_conf)
         {
            if (set_confused(p_ptr->confused + rand_int(100) + 50))
            ident=TRUE;
         }
         if (ident)
            msg_print("You suddenly don't remember what you were doing.");
         else
            msg_print("You feel an alien force probing your mind.");
         break;
      /* Bitter Regret Trap */
      case TRAP_OF_BITTER_REGRET:
         msg_print("An age-old and hideous sounding spell reverbs of the walls.");
         ident |= dec_stat(A_DEX, 25, TRUE);
         ident |= dec_stat(A_WIS, 25, TRUE);
         ident |= dec_stat(A_CON, 25, TRUE);
         ident |= dec_stat(A_STR, 25, TRUE);
         ident |= dec_stat(A_CHR, 25, TRUE);
         ident |= dec_stat(A_INT, 25, TRUE);
         break;
      /* Bowel Cramps Trap */
      case TRAP_OF_BOWEL_CRAMPS:
         msg_print("A wretched smelling gas cloud upsets your stomach.");
         (void)set_food(PY_FOOD_STARVE - 1);
         (void)set_poisoned(0);
         if (!p_ptr->free_act)
         {
            (void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10);
         }
         ident=TRUE;
         break;
      /* Blindness/Confusion Trap */
      case TRAP_OF_BLINDNESS_CONFUSION:
         msg_print("A powerfull magic protected this.");
         if (!p_ptr->resist_blind)
         {
            (void)set_blind(p_ptr->blind + rand_int(100) + 100);
            ident=TRUE;
         }
         if (!p_ptr->resist_conf)
         {
            if (set_confused(p_ptr->confused + rand_int(20) + 15))
            ident=TRUE;
         }
         break;
      /* Aggravation Trap */
      case TRAP_OF_AGGRAVATION:
         { 
            project_who_type who;
            msg_print("You hear a hollow noise echoing through the dungeons.");
            who.type = WHO_TRAPBYPLAYER;
            ident = aggravate_monsters(&who, -1);
            break;
         }
      /* Multiplication Trap */
      case TRAP_OF_MULTIPLICATION:
         {
            s16b mtyp[3][3];
            s16b styp[3][3];
            s16b x1,y1;
            project_who_type who;
            who.type = WHO_PLAYER;
            for (x1=x-1;x1<=x+1;x1++)     /* save dungeon state */
            {
               for (y1=y-1;y1<=y+1;y1++)
               {
                  mtyp[y1-y+1][x1-x+1] = dungeon.level[sublevel][y1][x1].mtyp;
                  styp[y1-y+1][x1-x+1] = dungeon.level[sublevel][y1][x1].styp;
               }
            }
            msg_print("You hear a loud click.");
            trap_creation(&who, px, py);
            do_cmd_search();
            for (x1=x-1;x1<=x+1;x1++)     /* compare */
            {
               for (y1=y-1;y1<=y+1;y1++)
               {
                  ident |= ( (mtyp[y1-y+1][x1-x+1] !=dungeon.level[sublevel][y1][x1].mtyp ) &&
                             (styp[y1-y+1][x1-x+1] !=dungeon.level[sublevel][y1][x1].styp ) );
               }
            }
         }
         break;
      /* Steal Item Trap */
      case TRAP_OF_STEAL_ITEM:
         {
            /* please note that magical stealing is not so easily circumvented */
            if (!p_ptr->paralyzed &&
                (rand_int(160) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
                                  p_ptr->lev)))
            {
               /* Saving throw message */
               msg_print("Your backpack seems to vibrate strangely!");
               break;
            }
            /* Find an item */
            for (k = 0; k < rand_int(10); k++)
            {
               char i_name[80];

               /* Pick an item */
               s16b i = rand_int(INVEN_PACK);

               /* Obtain the item */
               object_type *j_ptr = &inventory[i];

               /* Accept real items */
               if (!j_ptr->k_idx) continue;

               /* Don't steal artifacts  -CFT */
               if (artifact_p(j_ptr)) continue;

               /* Get a description */
               object_desc(i_name, j_ptr, FALSE, 3);

               /* Message */
               msg_format("%sour %s (%c) was stolen!",
                          ((j_ptr->number > 1) ? "One of y" : "Y"),
                          i_name, index_to_label(i));

               /* Steal the items */
               i_ptr->p1val += (object_value(j_ptr)/10000)+3; /* chest rating increases! */
               item_increase(i, -1, px, py);
               item_optimize(i, px, py);
               ident=TRUE;
            }
            /* this trap is not set on doors, so */
            if (ident)
            {
               msg_print("The chest seems so grow!");
            }
            else
            {
               msg_print("The chest vibrates strangely.");
            }
            break;
         }
      /* Summon Fast Quylthulgs Trap */
      case TRAP_OF_SUMMON_FAST_QUYLTHULGS:
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_QUYLTHULG, 0);
         }
         if (ident)
         {
            msg_print("You suddenly have company.");
            (void)set_slow(p_ptr->slow + randint(25) + 15);
         }
         break;
      /* Summon Fast Quylthulgs Trap */
      case TRAP_OF_SUMMON_HOUNDS:
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_HOUND, 2 + (p_ptr->mdepth / 7));
         }
         if (ident)
         {
            msg_print("You suddenly have company.");
         }
         break;
      /* Trap of Sinking */
      case TRAP_OF_SINKING:
         msg_print("You fell through a trap door!");
         if (p_ptr->ffall)
         {
            msg_print("You float gently down to the next level.");
         }
         else
         {
            take_hit(damroll(2,8), "a trap door");
         }
         new_level_flag = TRUE;
         /* we can only fall to a lower level if we're on the main level or in a cellar */
         if (p_ptr->sdepth<=0)
         {
            p_ptr->new_mdepth = p_ptr->mdepth + randint(3) + 1;
            p_ptr->new_sdepth = 0;
         }
         /* else fall back to the main level */
         else if (p_ptr->sdepth>0)
         {
            p_ptr->sdepth = 0;
         }
         break;
      /* Trap of Mana Drain */
      case TRAP_OF_MANA_DRAIN:
         if (p_ptr->csp>0)
         {
            p_ptr->csp = 0;
            p_ptr->csp_frac = 0;
            p_ptr->redraw1 |= (PR1_MANA);
            msg_print("You sense a great loss.");
            ident=TRUE;
         }
         else
         {
            if (p_ptr->msp==0) /* no sense saying this unless you never have mana */
            {
               msg_format("Suddenly you feel glad you're only a %s.",cp_ptr->title);
            }
            else
            {
               msg_print("Your head feels dizzy for a moment.");
            }
         }
         break;
      /* Trap of Missing Money */
      case TRAP_OF_MISSING_MONEY:
         {
            u32b gold = (p_ptr->au / 10) + randint(25);

            if (gold < 2) gold = 2;
            if (gold > 5000) gold = (p_ptr->au / 20) + randint(3000);
            if (gold > p_ptr->au) gold = p_ptr->au;
            p_ptr->au -= gold;
            if (gold <= 0)
            {
                msg_print("You feel something touching you.");
            }
            else if (p_ptr->au)
            {
                msg_print("Your purse feels lighter.");
                msg_format("%ld coins were stolen!", (long)gold);
                ident=TRUE;
            }
            else
            {
                msg_print("Your purse feels empty.");
                msg_print("All of your coins were stolen!");
                ident=TRUE;
            }
            p_ptr->redraw1 |= (PR1_GOLD);
         }
         break;
      /* Trap of No Return */
      case TRAP_OF_NO_RETURN:
         {
            object_type *j_ptr;
            s16b j;

            for (j=0;j<INVEN_WIELD;j++)
            {
               if (!inventory[j].k_idx) continue;
               j_ptr = &inventory[j];
               if ((j_ptr->tval==TV_SCROLL)
                    && (j_ptr->sval==SV_SCROLL_WORD_OF_RECALL))
               {
                  item_increase(j, -randint(10), px, py);
                  item_optimize(j,px,py);
                  combine_pack();
                  reorder_pack();
                  j=-1; /* start all over */
                  if (!ident)
                     msg_print("A small fire works it's way through your backpack. Some scrolls are burnt.");
                  else
                     msg_print("The fire hasn't finished.");
                  ident=TRUE;
               }
               else if ((j_ptr->tval==TV_ROD) && (j_ptr->sval == SV_ROD_RECALL))
               {
                  j_ptr->p1val = 5000; /* a long time */
                  if (!ident) msg_print("You have a feeling of staying.");
                  ident=TRUE;
               }
            }
            if ((!ident) && (p_ptr->word_recall == 0))
            {
               msg_print("You feel like leaving against your will!");
               p_ptr->word_recall = 15 + randint(20);
               ident=TRUE;
            }
            else if ((!ident) && (p_ptr->word_recall))
            {
               msg_print("You feel like staying around.");
               p_ptr->word_recall = 0;
               ident=TRUE;
            }
         }
         break;
      /* Trap of Silent Switching */
      case TRAP_OF_SILENT_SWITCHING:
         {
            s16b i,j,slot1,slot2;
            object_type *j_ptr, *k_ptr;
            for (i=INVEN_WIELD;i<INVEN_TOTAL;i++)
            {
               j_ptr = &inventory[i];
               if (!j_ptr->k_idx) continue;
               slot1=wield_slot(j_ptr);
               for (j=0;j<INVEN_WIELD;j++)
               {
                  k_ptr = &inventory[j];
                  if (!k_ptr->k_idx) continue;
                  /* this is a crude hack, but it prevent wielding 6 torches... */
                  if (k_ptr->number > 1) continue;
                  slot2=wield_slot(k_ptr);
                  /* a chance of 4 in 5 of switching something, then 2 in 5 to do it again */
                  if ((slot1==slot2) && (rand_int(100)<(80-ident*40)))
                  {
                     object_type tmp_obj;
                     tmp_obj = inventory[j];
                     inventory[j] = inventory[i];
                     inventory[i] = tmp_obj;
                     ident=TRUE;
                  }
               }
            }
            if (ident)
            {
               p_ptr->update |= (PU_BONUS);
               p_ptr->update |= (PU_TORCH);
               p_ptr->update |= (PU_MANA);
               msg_print("You somehow feel an other person.");
            }
            else
            {
               msg_print("You feel a lack of useful items.");
            }
         }
         break;
      /* Trap of Walls */
      case TRAP_OF_WALLS:
         ident = player_handle_trap_of_walls();
         break;
      /* Trap of Calling Out */
      case TRAP_OF_CALLING_OUT:
         {
            ident=do_player_trap_call_out();
            if (!ident)
            {
               /* Increase "afraid" */
               if (p_ptr->resist_fear)
               {
                   msg_print("You feel as if you had a nightmare!");
               }
               else if (rand_int(100) < p_ptr->skill_sav)
               {
                   msg_print("You remember having a nightmare!");
               }
               else
               {
                  if (set_afraid(p_ptr->afraid + 3 + randint(40)))
                  {
                     msg_print("You have a vision of a powerfull enemy.");
                  }
               }
            }
         }
         break;
      /* Trap of Sliding */
      case TRAP_OF_SLIDING:
         if (set_sliding(20+rand_int(20)))
         {
            msg_print("This trap really moves you!");
            p_ptr->sliding_now=TRUE;
            ident=TRUE;
         }
         else
         {
            msg_print("The ground seems slippery for a moment.");
         }
         break;
      /* Trap of Charges Drain */
      case TRAP_OF_CHARGES_DRAIN:
         {
            s16b         i;
            object_type *j_ptr;
            /* Find an item */
            for (k = 0; k < 10; k++)
            {
               i = rand_int(INVEN_PACK);
               j_ptr = &inventory[i];
               /* Drain charged wands/staffs */
               if (((j_ptr->tval == TV_STAFF) || (j_ptr->tval == TV_WAND)) &&
                   (j_ptr->p1val))
               {
                  ident = TRUE;
                  j_ptr->p1val = j_ptr->p1val / (randint(4)+1);
                  /* Window stuff */
                  p_ptr->window |= PW_INVEN;
                  /* Combine / Reorder the pack */
                  p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  if (randint(10)>3) break; /* 60% chance of only 1 */
               }
            }
            if (ident)
              msg_print("Your backpack seems to be turned upside down.");
            else
              msg_print("You hear a wail of great disappointment.");
         }
         break;
      /* Trap of Stair Movement */
      case TRAP_OF_STAIR_MOVEMENT:
         {
            s16b cx,cy,i,j;
            s16b cnt = 0;
            s16b cnt_seen = 0;
            s16b tmpm, tmps, tmpx;
            u32b tmpf;
            bool seen = FALSE;
            s16b index_x[20],index_y[20]; /* 20 stairs per level is enough? */
            cave_cell_type *cv_ptr;

            if (in_maze(px, py))
            {
               /* don't move the stairs in a maze */
               msg_print("You have a feeling that this trap could be dangerous.");
               break;
            }

            if (p_ptr->mdepth!=99) /* no sense in relocating that stair! */
            {
               for (cx=0;cx<MAX_WID;cx++)
               {
                  for (cy=0;cy<MAX_HGT;cy++)
                  {
                     cv_ptr = &dungeon.level[sublevel][cy][cx];
                     if (cv_ptr->mtyp!=DUNG_STAIR) continue;

                     /* don't you dare move the stairs in sublevel-vaults ! */
                     if (cv_ptr->fdat & CAVE_VAULT) continue;

                     /* strange stairs are not used yet, but they shouldn't be moved */
                     if (cv_ptr->styp==DUNG_STAIR_STRANGE) continue;

                     index_x[cnt]=cx;
                     index_y[cnt]=cy;
                     cnt++;
                  }
               }
               if (cnt==0)
               {
                  quit("Executing moving stairs trap on level with no stairs!");
               }
               else
               {
                  for (i=0;i<cnt;i++)
                  {
                     for (j=0;j<10;j++) /* try 10 times to relocate */
                     {
                        cave_cell_type *c_ptr2 = &dungeon.level[sublevel][index_y[i]][index_x[i]];

                        cx=rand_int(cur_wid);
                        cy=rand_int(cur_hgt);

                        if ((cx==index_x[i]) || (cy==index_y[i])) continue;
                        if (!clean_grid_bold(cx, cy)) continue;

                        /* don't put anything in vaults */
                        if (dungeon.level[sublevel][cy][cx].fdat & CAVE_VAULT) continue;

                        tmpm = dungeon.level[sublevel][cy][cx].mtyp;
                        tmps = dungeon.level[sublevel][cy][cx].styp;
                        tmpx = dungeon.level[sublevel][cy][cx].extra;
                        tmpf = dungeon.level[sublevel][cy][cx].fdat;
                        dungeon.level[sublevel][cy][cx].mtyp = c_ptr2->mtyp;
                        dungeon.level[sublevel][cy][cx].styp = c_ptr2->styp;
                        dungeon.level[sublevel][cy][cx].extra = c_ptr2->extra;
                        dungeon.level[sublevel][cy][cx].fdat = c_ptr2->fdat;
                        c_ptr2->mtyp  = tmpm;
                        c_ptr2->styp  = tmps;
                        c_ptr2->extra = tmpx;
                        c_ptr2->fdat  = tmpf;

                        /* if we are placing walls in rooms, make them rubble instead */
                        if ((c_ptr2->fdat & CAVE_ROOM) && (c_ptr2->mtyp == DUNG_WALL))
                        {
                           (void)set_grid_type(index_x[i], index_y[i],
                                               DUNG_WALL, DUNG_WALL_RUBBLE,
                                               GRID_KEEP, 0);
                        }
                        if (player_has_los_bold(cx, cy))
                        {
                           note_spot(cx,cy);
                           lite_spot(cx, cy);
                           seen=TRUE;
                        }
                        else
                        {
                           dungeon.level[sublevel][cy][cx].fdat &=~CAVE_MARK;
                        }
                        if (player_has_los_bold(index_x[i],index_y[i]))
                        {
                           note_spot(index_x[i],index_y[i]);
                           lite_spot(index_x[i],index_y[i]);
                           seen = TRUE;
                        }
                        else
                        {
                           dungeon.level[sublevel][index_y[i]][index_x[i]].fdat &=~CAVE_MARK;
                        }
                        break;
                     }
                     if (seen) cnt_seen++;
                     seen = FALSE;
                  } /* cnt loop */
                  ident = (cnt_seen>0);
                  if ((ident) && (cnt_seen>1))
                  {
                     msg_print("You see some stairs move.");
                  }
                  else if (ident)
                  {
                     msg_print("You see a stair move.");
                  }
                  else
                  {
                     msg_print("You hear distant scraping noises.");
                  }
               } /* any stairs found */
            }
            else /* are we on level 99 */
            {
               msg_print("You have a feeling that this trap could be dangerous.");
            }
         }
         break;
      /* Trap of New Trap */
      case TRAP_OF_NEW:
         {
            s16b i;
            /* if we're on a floor or on a door, place a new trap */
            if ((item == -1) || (item == -2))
            {
               init_trap(tr_ptr);
               if (set_traps(tr_ptr,&i,p_ptr->mdepth,FTRAP_DOOR, 0, 0))
               {

                  if (dungeon.level[sublevel][y][x].mtyp == DUNG_TRAP)
                  {
                     place_trap(x, y, p_ptr->mdepth, 0, 0);
                  }
                  if (player_has_los_bold(x, y))
                  {
                     note_spot(x, y);
                     lite_spot(x, y);
                  }
               }
            }
            else
            {
               /* re-trap the chest */
               set_traps(tr_ptr,&i,p_ptr->mdepth,FTRAP_CHEST, 0, 0);
               i_ptr->p1val += i; /* this trap itself has p1val+0, so it's ok here */
               forget_item(i_ptr);
            }
            msg_print("You hear a noise, and then it's echo.");
            ident=FALSE;
         }
         break;
      /* Trap of Scatter Items */
      case TRAP_OF_SCATTER_ITEMS:
         {
             s16b i,j;
            bool message = FALSE;
            for (i=0;i<INVEN_PACK;i++)
            {
               if (!inventory[i].k_idx) continue;
               if (rand_int(10)<3) continue;
               for (j=0;j<10;j++)
               {
                  object_type tmp_obj;
                  s16b cx = px+15-rand_int(30);
                  s16b cy = py+15-rand_int(30);
                  if (!in_bounds(cx,cy)) continue;
                  if (!naked_grid_bold(cx,cy)) continue;
                  tmp_obj = inventory[i];
                  item_increase(i, -999, px, py);
                  item_optimize(i, px, py);
                  p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  (void)floor_carry(&tmp_obj,cx,cy);
                  if (!message)
                  {
                     msg_print("You feel light-footed.");
                     message = TRUE;
                  }
                  if (player_has_los_bold(cx, cy))
                  {
                     char i_name[80];
                     object_desc(i_name, &tmp_obj, TRUE, 3);
                     note_spot(cx, cy);
                     lite_spot(cx, cy);
                     ident=TRUE;
                     msg_format("Suddenly %s appear%s!",i_name, (tmp_obj.number>1)?"":"s");
                  }
                  break;
               }
               ident = message;
            }
         }
         break;
      /* Trap of Decay */
      case TRAP_OF_DECAY:
         if (poison_dam(randint(40)))
         {
            msg_print("Decaying slime streams from your backpack!");
            ident = TRUE;
         }
         else
         {
            msg_print("You're overwhelmed by a smell of decay!");
         }
         break;
      /* Trap of Wasting Wands */
      case TRAP_OF_WASTING_WANDS:
         {
            s16b i;
            object_type *j_ptr;
            for (i=0;i<INVEN_PACK;i++)
            {
               if (!inventory[i].k_idx) continue;
               j_ptr = &inventory[i];
               if (j_ptr->tval==TV_WAND)
               {
                  if ((j_ptr->sval>=SV_WAND_NASTY_WAND) && (rand_int(5)==1))
                  {
                     if (object_known_p(j_ptr)) ident=TRUE;
                     j_ptr->sval = rand_int(SV_WAND_NASTY_WAND);
                     j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
                     p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  }
                  if ((j_ptr->sval>=SV_STAFF_NASTY_STAFF) && (rand_int(5)==1))
                  {
                     if (object_known_p(j_ptr)) ident=TRUE;
                     j_ptr->sval = rand_int(SV_STAFF_NASTY_STAFF);
                     j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
                     p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  }
               }
            }
         }
         if (ident)
         {
            msg_print("You have lost trust in your backpack!");
         }
         else
         {
            msg_print("You hear an echoing cry of rage.");
         }
         break;
      /* Trap of Filling */
      case TRAP_OF_FILLING:
         {
            s16b nx, ny;
            for (nx=x-8;nx<=x+8;nx++)
            {
               for (ny=y-8;ny<=y+8;ny++)
               {
                  if (!in_bounds (nx, ny)) continue;

                  if ((rand_int(distance(nx,ny,px,py))==0) && (randint(3)==1))
                  {
                     place_trap(nx,ny,p_ptr->mdepth+rand_int(5), 10, 0);
                  }
               }
            }
            msg_print("The floor vibrates in a strange way.");
            /* it should be identified sometimes */
            if (randint(15)==1)
            {
               ident = TRUE;
            }
            else
            {
               ident = FALSE;
            }
         }
         break;
      case TRAP_OF_DRAIN_SPEED:
         {
            object_type *j_ptr;
            s16b j, chance = 75;
            u64b f1, f2, f3;

            for (j=0;j<INVEN_TOTAL;j++)
            {
               /* don't bother the overflow slot */
               if (j==INVEN_PACK) continue;

               if (!inventory[j].k_idx) continue;
               j_ptr = &inventory[j];
               object_flags(j_ptr, &f1, &f2, &f3);

               /* is it a non-artifact speed item? */
               if ((!j_ptr->name1) && (f1 & TR1_SPEED1))
               {
                  if (randint(100)<chance)
                  {
                     j_ptr->p1val = j_ptr->p1val / 2;
                     if (j_ptr->p1val == 0)
                     {
                        j_ptr->p1val--;
                        j_ptr->ident |= ID_CURSED;
                        j_ptr->ident |= ID_SENSE;
                     }
                     chance /= 2;
                     ident = TRUE;
                  }
                  item_optimize(j,px,py);
               }
               else if ((!j_ptr->name1) && (f1 & TR1_SPEED2))
               {
                  if (randint(100)<chance)
                  {
                     j_ptr->p2val = j_ptr->p2val / 2;
                     if (j_ptr->p2val == 0)
                     {
                        j_ptr->p2val--;
                        j_ptr->ident |= ID_CURSED;
                        j_ptr->ident |= ID_SENSE;
                     }
                     chance /= 2;
                     ident = TRUE;
                  }
                  item_optimize(j,px,py);
               }
            }
            if (!ident)
               msg_print("You feel some things in your pack vibrating.");
            else
            {
               combine_pack();
               reorder_pack();
               msg_print("You suddenly feel you have time for self-reflection.");
               /* Recalculate bonuses */
               p_ptr->update |= (PU_BONUS);

               /* Recalculate mana */
               p_ptr->update |= (PU_MANA);

               /* Window stuff */
               p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
            }
         }
         break;

      /*
       * single missile traps
       */

      case TRAP_OF_ARROW_I:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_ARROW_NORMAL,
                                            4, 8, 0, "an arrow trap"); break;
      case TRAP_OF_ARROW_II:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_ARROW_NORMAL,
                                            10, 8, 0, "a bolt trap"); break;
      case TRAP_OF_ARROW_III:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_ARROW_HEAVY,
                                            12, 12, 0, "a seeker arrow trap"); break;
      case TRAP_OF_ARROW_IV:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_BOLT_HEAVY,
                                            12, 16, 0, "a seeker bolt trap"); break;
      case TRAP_OF_POISON_ARROW_I:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_ARROW_NORMAL,
                                            4, 8, 10+randint(20), "a poisoned arrow trap"); break;
      case TRAP_OF_POISON_ARROW_II:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_BOLT_NORMAL,
                                            10, 8, 15+randint(30), "a poisoned bolt trap"); break;
      case TRAP_OF_POISON_ARROW_III:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_ARROW_HEAVY,
                                            12, 12, 30+randint(50), "a poisoned seeker arrow trap"); break;
      case TRAP_OF_POISON_ARROW_IV:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_BOLT_HEAVY,
                                            12, 16, 40+randint(70), "a poisoned seeker bolt trap"); break;
      case TRAP_OF_DAGGER_I:
         ident = player_handle_missile_trap(1, TV_SWORD, SV_BROKEN_DAGGER,
                                            4, 8, 0, "a dagger trap"); break;
      case TRAP_OF_DAGGER_II:
         ident = player_handle_missile_trap(1, TV_SWORD, SV_DAGGER,
                                            10, 8, 0, "a dagger trap"); break;
      case TRAP_OF_POISON_DAGGER_I:
         ident = player_handle_missile_trap(1, TV_SWORD, SV_BROKEN_DAGGER,
                                            4, 8, 15+randint(20), "a poisoned dagger trap"); break;
      case TRAP_OF_POISON_DAGGER_II:
         ident = player_handle_missile_trap(1, TV_SWORD, SV_DAGGER,
                                            10, 8, 20+randint(30), "a poisoned dagger trap"); break;
      /*
       * multiple missile traps
       * numbers range from 2 (level 0 to 14) to 10 (level 120 and up)
       */
      case TRAP_OF_ARROWS_I:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_NORMAL,
                                            4, 8, 0, "an arrow trap"); break;
      case TRAP_OF_ARROWS_II:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_BOLT, SV_ARROW_NORMAL,
                                            10, 8, 0, "a bolt trap"); break;
      case TRAP_OF_ARROWS_III:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_HEAVY,
                                            12, 12, 0, "a seeker arrow trap"); break;
      case TRAP_OF_ARROWS_IV:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_BOLT, SV_BOLT_HEAVY,
                                            12, 16, 0, "a seeker bolt trap"); break;
      case TRAP_OF_POISON_ARROWS_I:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_NORMAL,
                                            4, 8, 10+randint(20), "a poisoned arrow trap"); break;
      case TRAP_OF_POISON_ARROWS_II:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_BOLT, SV_BOLT_NORMAL,
                                            10, 8, 15+randint(30), "a poisoned bolt trap"); break;
      case TRAP_OF_POISON_ARROWS_III:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_HEAVY,
                                            12, 12, 30+randint(50), "a poisoned seeker arrow trap"); break;
      case TRAP_OF_POISON_ARROWS_IV:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_BOLT, SV_BOLT_HEAVY,
                                            12, 16, 40+randint(70), "a poisoned seeker bolt trap"); break;
      case TRAP_OF_DAGGERS_I:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_SWORD, SV_BROKEN_DAGGER,
                                            4, 8, 0, "a dagger trap"); break;
      case TRAP_OF_DAGGERS_II:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_SWORD, SV_DAGGER,
                                            10, 8, 0, "a dagger trap"); break;
      case TRAP_OF_POISON_DAGGERS_I:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_SWORD, SV_BROKEN_DAGGER,
                                            4, 8, 15+randint(20), "a poisoned dagger trap"); break;
      case TRAP_OF_POISON_DAGGERS_II:
         ident = player_handle_missile_trap(2+(p_ptr->mdepth / 15), TV_SWORD, SV_DAGGER,
                                            10, 8, 20+randint(30), "a poisoned dagger trap"); break;

      case TRAP_OF_DROP_ITEMS:
         {
            s16b i;
            bool message = FALSE;
            for (i=0;i<INVEN_PACK;i++)
            {
               object_type tmp_obj;
               s16b amt;
               if (!inventory[i].k_idx) continue;
               if (randint(100)<0) continue;
               amt=randint(inventory[i].number);
dlog(DEBUGITEMS,"object3.c: player_activate_trap_type: inven %d number %d, amt %d\n", i, inventory[i].number, amt);
               tmp_obj = inventory[i];
               tmp_obj.number=amt;
               /* drop carefully */
               drop_near(&tmp_obj, 0, px, py, 0, FALSE, FALSE);
               item_increase(i,-amt, px, py);
dlog(DEBUGITEMS,"object3.c: player_activate_trap_type: inven %d number now %d\n", i, inventory[i].number);
               item_optimize(i,px,py);
               p_ptr->notice |= (PN_COMBINE | PN_REORDER);
               if (!message)
               {
                  msg_print("You are startled by a sudden sound.");
                  message = TRUE;
               }
               ident = TRUE;
            }
            if (!ident)
            {
               msg_print("You hear a sudden, strange sound.");
            }
         }
         break;
      case TRAP_OF_DROP_ALL_ITEMS:
         {
            s16b i;
            bool message = FALSE;
            for (i=0;i<INVEN_PACK;i++)
            {
               object_type tmp_obj;
               if (!inventory[i].k_idx) continue;
               if (randint(100)<10) continue;
               tmp_obj = inventory[i];
               /* drop carefully */
               drop_near(&tmp_obj, 0, px, py, 0, FALSE, FALSE);
               item_increase(i,-999, px, py);
               item_optimize(i,px,py);
               p_ptr->notice |= (PN_COMBINE | PN_REORDER);
               if (!message)
               {
                  msg_print("You are startled a lot by a sudden sound.");
                  message = TRUE;
               }
               ident = TRUE;
            }
            if (!ident)
            {
               msg_print("You hear a sudden, strange sound.");
            }
         }
         break;
      case TRAP_OF_DROP_EVERYTHING:
         {
            s16b i;
            bool message = FALSE;
            for (i=0;i<INVEN_TOTAL;i++)
            {
               object_type tmp_obj;
               if (!inventory[i].k_idx) continue;
               if (randint(100)<30) continue;
               tmp_obj = inventory[i];
dlog(DEBUGEXTRA,"object3.c: player_activate_trap_type: item %d of %d (%s): i_ptr->number %d tmp_obj.number %d\n",
                i, INVEN_TOTAL, k_name + k_info[inventory[i].k_idx].name, inventory[i].number, tmp_obj.number);
               /* drop carefully */
               drop_near(&tmp_obj, 0, px, py, 0, FALSE, FALSE);
               item_increase(i,-999, px, py);
               item_optimize(i,px,py);
dlog(DEBUGEXTRA,"object3.c: player_activate_trap_type: item %d of %d (%s): i_ptr->number %d tmp_obj.number %d\n",
                i, INVEN_TOTAL, k_name + k_info[inventory[i].k_idx].name, inventory[i].number, tmp_obj.number);
               p_ptr->notice |= (PN_COMBINE | PN_REORDER);
               if (!message)
               {
                  msg_print("You are completely startled by a sudden sound.");
                  message = TRUE;
               }
               ident = TRUE;
            }
            if (!ident)
            {
               msg_print("You hear a sudden, strange sound.");
            }
         }
         break;

      /* Bolt Trap */
      case TRAP_OF_ELEC_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_ELEC, TRAP_OF_ELEC_BOLT); break;
      case TRAP_OF_POIS_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_POIS, TRAP_OF_POIS_BOLT); break;
      case TRAP_OF_ACID_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_ACID, TRAP_OF_ACID_BOLT); break;
      case TRAP_OF_COLD_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_COLD, TRAP_OF_COLD_BOLT); break;
      case TRAP_OF_FIRE_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_FIRE, TRAP_OF_FIRE_BOLT); break;
      case TRAP_OF_PLASMA_BOLT:     ident=player_handle_breath_trap(x, y, 1, GF_PLASMA, TRAP_OF_PLASMA_BOLT); break;
      case TRAP_OF_WATER_BOLT:      ident=player_handle_breath_trap(x, y, 1, GF_WATER, TRAP_OF_WATER_BOLT); break;
      case TRAP_OF_LITE_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_LITE, TRAP_OF_LITE_BOLT); break;
      case TRAP_OF_DARK_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_DARK, TRAP_OF_DARK_BOLT); break;
      case TRAP_OF_SHARDS_BOLT:     ident=player_handle_breath_trap(x, y, 1, GF_SHARDS, TRAP_OF_SHARDS_BOLT); break;
      case TRAP_OF_SOUND_BOLT:      ident=player_handle_breath_trap(x, y, 1, GF_SOUND, TRAP_OF_SOUND_BOLT); break;
      case TRAP_OF_CONFUSION_BOLT:  ident=player_handle_breath_trap(x, y, 1, GF_CONFUSION, TRAP_OF_CONFUSION_BOLT); break;
      case TRAP_OF_FORCE_BOLT:      ident=player_handle_breath_trap(x, y, 1, GF_FORCE, TRAP_OF_FORCE_BOLT); break;
      case TRAP_OF_INERTIA_BOLT:    ident=player_handle_breath_trap(x, y, 1, GF_INERTIA, TRAP_OF_INERTIA_BOLT); break;
      case TRAP_OF_MANA_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_MANA, TRAP_OF_MANA_BOLT); break;
      case TRAP_OF_ICE_BOLT:        ident=player_handle_breath_trap(x, y, 1, GF_ICE, TRAP_OF_ICE_BOLT); break;
      case TRAP_OF_CHAOS_BOLT:      ident=player_handle_breath_trap(x, y, 1, GF_CHAOS, TRAP_OF_CHAOS_BOLT); break;
      case TRAP_OF_NETHER_BOLT:     ident=player_handle_breath_trap(x, y, 1, GF_NETHER, TRAP_OF_NETHER_BOLT); break;
      case TRAP_OF_DISENCHANT_BOLT: ident=player_handle_breath_trap(x, y, 1, GF_DISENCHANT, TRAP_OF_DISENCHANT_BOLT); break;
      case TRAP_OF_NEXUS_BOLT:      ident=player_handle_breath_trap(x, y, 1, GF_NEXUS, TRAP_OF_NEXUS_BOLT); break;
      case TRAP_OF_TIME_BOLT:       ident=player_handle_breath_trap(x, y, 1, GF_TIME, TRAP_OF_TIME_BOLT); break;
      case TRAP_OF_GRAVITY_BOLT:    ident=player_handle_breath_trap(x, y, 1, GF_GRAVITY, TRAP_OF_GRAVITY_BOLT); break;

      /* Ball Trap */
      case TRAP_OF_ELEC_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_ELEC, TRAP_OF_ELEC_BALL); break;
      case TRAP_OF_POIS_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_POIS, TRAP_OF_POIS_BALL); break;
      case TRAP_OF_ACID_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_ACID, TRAP_OF_ACID_BALL); break;
      case TRAP_OF_COLD_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_COLD, TRAP_OF_COLD_BALL); break;
      case TRAP_OF_FIRE_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_FIRE, TRAP_OF_FIRE_BALL); break;
      case TRAP_OF_PLASMA_BALL:     ident=player_handle_breath_trap(x, y, 3, GF_PLASMA, TRAP_OF_PLASMA_BALL); break;
      case TRAP_OF_WATER_BALL:      ident=player_handle_breath_trap(x, y, 3, GF_WATER, TRAP_OF_WATER_BALL); break;
      case TRAP_OF_LITE_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_LITE, TRAP_OF_LITE_BALL); break;
      case TRAP_OF_DARK_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_DARK, TRAP_OF_DARK_BALL); break;
      case TRAP_OF_SHARDS_BALL:     ident=player_handle_breath_trap(x, y, 3, GF_SHARDS, TRAP_OF_SHARDS_BALL); break;
      case TRAP_OF_SOUND_BALL:      ident=player_handle_breath_trap(x, y, 3, GF_SOUND, TRAP_OF_SOUND_BALL); break;
      case TRAP_OF_CONFUSION_BALL:  ident=player_handle_breath_trap(x, y, 3, GF_CONFUSION, TRAP_OF_CONFUSION_BALL); break;
      case TRAP_OF_FORCE_BALL:      ident=player_handle_breath_trap(x, y, 3, GF_FORCE, TRAP_OF_FORCE_BALL); break;
      case TRAP_OF_INERTIA_BALL:    ident=player_handle_breath_trap(x, y, 3, GF_INERTIA, TRAP_OF_INERTIA_BALL); break;
      case TRAP_OF_MANA_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_MANA, TRAP_OF_MANA_BALL); break;
      case TRAP_OF_ICE_BALL:        ident=player_handle_breath_trap(x, y, 3, GF_ICE, TRAP_OF_ICE_BALL); break;
      case TRAP_OF_CHAOS_BALL:      ident=player_handle_breath_trap(x, y, 3, GF_CHAOS, TRAP_OF_CHAOS_BALL); break;
      case TRAP_OF_NETHER_BALL:     ident=player_handle_breath_trap(x, y, 3, GF_NETHER, TRAP_OF_NETHER_BALL); break;
      case TRAP_OF_DISENCHANT_BALL: ident=player_handle_breath_trap(x, y, 3, GF_DISENCHANT, TRAP_OF_DISENCHANT_BALL); break;
      case TRAP_OF_NEXUS_BALL:      ident=player_handle_breath_trap(x, y, 3, GF_NEXUS, TRAP_OF_NEXUS_BALL); break;
      case TRAP_OF_TIME_BALL:       ident=player_handle_breath_trap(x, y, 3, GF_TIME, TRAP_OF_TIME_BALL); break;
      case TRAP_OF_GRAVITY_BALL:    ident=player_handle_breath_trap(x, y, 3, GF_GRAVITY, TRAP_OF_GRAVITY_BALL); break;

      default:
      {
         msg_print(format("Executing unknown trap %d",trap));
      }
   }
   return ident;
}

/*
 * this function activates one trap type upon a monster
 *
 * if a trap is found, found = TRUE at the end
 * if a trap is also identified, the function returns TRUE
 *
 */
static bool monster_activate_trap_type(s16b m_idx, s16b x, s16b y,
                                       object_type *i_ptr,
                                       s16b item, trap_item_type *tr_ptr,
                                       s16b trap, bool *found)
{
   bool ident, visible;

   s16b                 k;
   s16b                 mx, my;
   char                 m_name[80];

   monster_type        *m_ptr = &mn_list[m_idx];
   project_who_type     who;
   
   monster_desc(m_name, m_ptr, 0x4);

   mx = mn_list[m_idx].fx;
   my = mn_list[m_idx].fy;

   (*found) = FALSE;
   ident = FALSE;

   visible = TRUE;
   if (p_ptr->blind) visible = FALSE;
   if (!player_has_los_bold(mx, my)) visible = FALSE;
   if (!m_ptr->ml) visible = FALSE;

dlog(DEBUGTRAPS,"object3.c: monster_activate_trap_type: trap %d visible %d monster_name %s = %s\n",
                trap, visible, m_name, r_name + r_info[m_ptr->r_idx].name);

   switch(trap)
   {
      /* Weakness Trap */
      case TRAP_OF_WEAKNESS_I:
      case TRAP_OF_INTELLIGENCE_I:
      case TRAP_OF_WISDOM_I:
      case TRAP_OF_FUMBLING_I:
      case TRAP_OF_WASTING_I:
      case TRAP_OF_BEAUTY_I:
      case TRAP_OF_POISON_NEEDLE:
      case TRAP_OF_DECAY:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("A cloud of gas erupts.");
            (*found) = TRUE;
         }
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (void)project(&who, 1, mx, my, damroll(6,8),
                       GF_POIS,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         ident = FALSE;
         break;

      /* Weakness Trap */
      case TRAP_OF_WEAKNESS_II:
      case TRAP_OF_INTELLIGENCE_II:
      case TRAP_OF_WISDOM_II:
      case TRAP_OF_FUMBLING_II:
      case TRAP_OF_WASTING_II:
      case TRAP_OF_BEAUTY_II:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("An intense cloud of gas erupts.");
            (*found) = TRUE;
         }
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (void)project(&who, 1, mx, my, damroll(12,8),
                       GF_POIS,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         ident = FALSE;
         break;

      /* Weakness Trap */
      case TRAP_OF_WEAKNESS_III:
      case TRAP_OF_INTELLIGENCE_III:
      case TRAP_OF_WISDOM_III:
      case TRAP_OF_FUMBLING_III:
      case TRAP_OF_WASTING_III:
      case TRAP_OF_BEAUTY_III:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("An sizzling cloud of gas erupts.");
            (*found) = TRUE;
         }
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (void)project(&who, 1, mx, my, damroll(16,12),
                       GF_POIS,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         ident = FALSE;
         break;

      /* Bolt Trap */
      case TRAP_OF_ELEC_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_ELEC, TRAP_OF_ELEC_BOLT); ident=(*found); break;
      case TRAP_OF_POIS_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_POIS, TRAP_OF_POIS_BOLT); ident=(*found); break;
      case TRAP_OF_ACID_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_ACID, TRAP_OF_ACID_BOLT); ident=(*found); break;
      case TRAP_OF_COLD_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_COLD, TRAP_OF_COLD_BOLT); ident=(*found); break;
      case TRAP_OF_FIRE_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_FIRE, TRAP_OF_FIRE_BOLT); ident=(*found); break;
      case TRAP_OF_PLASMA_BOLT:     (*found)=monster_handle_breath_trap(m_idx, 1, GF_PLASMA, TRAP_OF_PLASMA_BOLT); ident=(*found); break;
      case TRAP_OF_WATER_BOLT:      (*found)=monster_handle_breath_trap(m_idx, 1, GF_WATER, TRAP_OF_WATER_BOLT); ident=(*found); break;
      case TRAP_OF_LITE_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_LITE, TRAP_OF_LITE_BOLT); ident=(*found); break;
      case TRAP_OF_DARK_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_DARK, TRAP_OF_DARK_BOLT); ident=(*found); break;
      case TRAP_OF_SHARDS_BOLT:     (*found)=monster_handle_breath_trap(m_idx, 1, GF_SHARDS, TRAP_OF_SHARDS_BOLT); ident=(*found); break;
      case TRAP_OF_SOUND_BOLT:      (*found)=monster_handle_breath_trap(m_idx, 1, GF_SOUND, TRAP_OF_SOUND_BOLT); ident=(*found); break;
      case TRAP_OF_CONFUSION_BOLT:  (*found)=monster_handle_breath_trap(m_idx, 1, GF_CONFUSION, TRAP_OF_CONFUSION_BOLT); ident=(*found); break;
      case TRAP_OF_FORCE_BOLT:      (*found)=monster_handle_breath_trap(m_idx, 1, GF_FORCE, TRAP_OF_FORCE_BOLT); ident=(*found); break;
      case TRAP_OF_INERTIA_BOLT:    (*found)=monster_handle_breath_trap(m_idx, 1, GF_INERTIA, TRAP_OF_INERTIA_BOLT); ident=(*found); break;
      case TRAP_OF_MANA_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_MANA, TRAP_OF_MANA_BOLT); ident=(*found); break;
      case TRAP_OF_ICE_BOLT:        (*found)=monster_handle_breath_trap(m_idx, 1, GF_ICE, TRAP_OF_ICE_BOLT); ident=(*found); break;
      case TRAP_OF_CHAOS_BOLT:      (*found)=monster_handle_breath_trap(m_idx, 1, GF_CHAOS, TRAP_OF_CHAOS_BOLT); ident=(*found); break;
      case TRAP_OF_NETHER_BOLT:     (*found)=monster_handle_breath_trap(m_idx, 1, GF_NETHER, TRAP_OF_NETHER_BOLT); ident=(*found); break;
      case TRAP_OF_DISENCHANT_BOLT: (*found)=monster_handle_breath_trap(m_idx, 1, GF_DISENCHANT, TRAP_OF_DISENCHANT_BOLT); ident=(*found); break;
      case TRAP_OF_NEXUS_BOLT:      (*found)=monster_handle_breath_trap(m_idx, 1, GF_NEXUS, TRAP_OF_NEXUS_BOLT); ident=(*found); break;
      case TRAP_OF_TIME_BOLT:       (*found)=monster_handle_breath_trap(m_idx, 1, GF_TIME, TRAP_OF_TIME_BOLT); ident=(*found); break;
      case TRAP_OF_GRAVITY_BOLT:    (*found)=monster_handle_breath_trap(m_idx, 1, GF_GRAVITY, TRAP_OF_GRAVITY_BOLT); ident=(*found); break;

      /* Ball Trap */
      case TRAP_OF_ELEC_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_ELEC, TRAP_OF_ELEC_BALL); ident=(*found); break;
      case TRAP_OF_POIS_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_POIS, TRAP_OF_POIS_BALL); ident=(*found); break;
      case TRAP_OF_ACID_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_ACID, TRAP_OF_ACID_BALL); ident=(*found); break;
      case TRAP_OF_COLD_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_COLD, TRAP_OF_COLD_BALL); ident=(*found); break;
      case TRAP_OF_FIRE_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_FIRE, TRAP_OF_FIRE_BALL); ident=(*found); break;
      case TRAP_OF_PLASMA_BALL:     (*found)=monster_handle_breath_trap(m_idx, 3, GF_PLASMA, TRAP_OF_PLASMA_BALL); ident=(*found); break;
      case TRAP_OF_WATER_BALL:      (*found)=monster_handle_breath_trap(m_idx, 3, GF_WATER, TRAP_OF_WATER_BALL); ident=(*found); break;
      case TRAP_OF_LITE_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_LITE, TRAP_OF_LITE_BALL); ident=(*found); break;
      case TRAP_OF_DARK_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_DARK, TRAP_OF_DARK_BALL); ident=(*found); break;
      case TRAP_OF_SHARDS_BALL:     (*found)=monster_handle_breath_trap(m_idx, 3, GF_SHARDS, TRAP_OF_SHARDS_BALL); ident=(*found); break;
      case TRAP_OF_SOUND_BALL:      (*found)=monster_handle_breath_trap(m_idx, 3, GF_SOUND, TRAP_OF_SOUND_BALL); ident=(*found); break;
      case TRAP_OF_CONFUSION_BALL:  (*found)=monster_handle_breath_trap(m_idx, 3, GF_CONFUSION, TRAP_OF_CONFUSION_BALL); ident=(*found); break;
      case TRAP_OF_FORCE_BALL:      (*found)=monster_handle_breath_trap(m_idx, 3, GF_FORCE, TRAP_OF_FORCE_BALL); ident=(*found); break;
      case TRAP_OF_INERTIA_BALL:    (*found)=monster_handle_breath_trap(m_idx, 3, GF_INERTIA, TRAP_OF_INERTIA_BALL); ident=(*found); break;
      case TRAP_OF_MANA_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_MANA, TRAP_OF_MANA_BALL); ident=(*found); break;
      case TRAP_OF_ICE_BALL:        (*found)=monster_handle_breath_trap(m_idx, 3, GF_ICE, TRAP_OF_ICE_BALL); ident=(*found); break;
      case TRAP_OF_CHAOS_BALL:      (*found)=monster_handle_breath_trap(m_idx, 3, GF_CHAOS, TRAP_OF_CHAOS_BALL); ident=(*found); break;
      case TRAP_OF_NETHER_BALL:     (*found)=monster_handle_breath_trap(m_idx, 3, GF_NETHER, TRAP_OF_NETHER_BALL); ident=(*found); break;
      case TRAP_OF_DISENCHANT_BALL: (*found)=monster_handle_breath_trap(m_idx, 3, GF_DISENCHANT, TRAP_OF_DISENCHANT_BALL); ident=(*found); break;
      case TRAP_OF_NEXUS_BALL:      (*found)=monster_handle_breath_trap(m_idx, 3, GF_NEXUS, TRAP_OF_NEXUS_BALL); ident=(*found); break;
      case TRAP_OF_TIME_BALL:       (*found)=monster_handle_breath_trap(m_idx, 3, GF_TIME, TRAP_OF_TIME_BALL); ident=(*found); break;
      case TRAP_OF_GRAVITY_BALL:    (*found)=monster_handle_breath_trap(m_idx, 3, GF_GRAVITY, TRAP_OF_GRAVITY_BALL); ident=(*found); break;

      /*
       * single missile traps
       */

      case TRAP_OF_ARROW_I:
         ident = monster_handle_missile_trap(m_idx, 1, TV_ARROW, SV_ARROW_NORMAL, 4, 8, 0, "Arrow Trap"); break;
      case TRAP_OF_ARROW_II:
         ident = monster_handle_missile_trap(m_idx, 1, TV_BOLT, SV_ARROW_NORMAL, 10, 8, 0, "Bolt Trap"); break;
      case TRAP_OF_ARROW_III:
         ident = monster_handle_missile_trap(m_idx, 1, TV_ARROW, SV_ARROW_HEAVY, 12, 12, 0, "Seeker Arrow Trap"); break;
      case TRAP_OF_ARROW_IV:
         ident = monster_handle_missile_trap(m_idx, 1, TV_BOLT, SV_BOLT_HEAVY, 12, 16, 0, "Seeker Bolt Trap"); break;
      case TRAP_OF_POISON_ARROW_I:
         ident = monster_handle_missile_trap(m_idx, 1, TV_ARROW, SV_ARROW_NORMAL, 4, 8, 10+randint(20), "Poison Arrow Trap"); break;
      case TRAP_OF_POISON_ARROW_II:
         ident = monster_handle_missile_trap(m_idx, 1, TV_BOLT, SV_BOLT_NORMAL, 10, 8, 15+randint(30), "Poison Bolt Trap"); break;
      case TRAP_OF_POISON_ARROW_III:
         ident = monster_handle_missile_trap(m_idx, 1, TV_ARROW, SV_ARROW_HEAVY, 12, 12, 30+randint(50), "Poison Seeker Arrow Trap"); break;
      case TRAP_OF_POISON_ARROW_IV:
         ident = monster_handle_missile_trap(m_idx, 1, TV_BOLT, SV_BOLT_HEAVY, 12, 16, 40+randint(70), "Poison Seeker Bolt Trap"); break;
      case TRAP_OF_DAGGER_I:
         ident = monster_handle_missile_trap(m_idx, 1, TV_SWORD, SV_BROKEN_DAGGER, 4, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_DAGGER_II:
         ident = monster_handle_missile_trap(m_idx, 1, TV_SWORD, SV_DAGGER, 10, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGER_I:
         ident = monster_handle_missile_trap(m_idx, 1, TV_SWORD, SV_BROKEN_DAGGER, 4, 8, 15+randint(20), "Poison Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGER_II:
         ident = monster_handle_missile_trap(m_idx, 1, TV_SWORD, SV_DAGGER, 10, 8, 20+randint(30), "Poison Dagger Trap"); break;

      /*
       * multiple missile traps
       * numbers range from 2 (level 0 to 14) to 10 (level 120 and up)
       */

      case TRAP_OF_ARROWS_I:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_NORMAL, 4, 8, 0, "Arrow Trap"); break;
      case TRAP_OF_ARROWS_II:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_BOLT, SV_ARROW_NORMAL, 10, 8, 0, "Bolt Trap"); break;
      case TRAP_OF_ARROWS_III:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_HEAVY, 12, 12, 0, "Seeker Arrow Trap"); break;
      case TRAP_OF_ARROWS_IV:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_BOLT, SV_BOLT_HEAVY, 12, 16, 0, "Seeker Bolt Trap"); break;
      case TRAP_OF_POISON_ARROWS_I:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_NORMAL, 4, 8, 10+randint(20), "Poison Arrow Trap"); break;
      case TRAP_OF_POISON_ARROWS_II:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_BOLT, SV_BOLT_NORMAL, 10, 8, 15+randint(30), "Poison Bolt Trap"); break;
      case TRAP_OF_POISON_ARROWS_III:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_ARROW, SV_ARROW_HEAVY, 12, 12, 30+randint(50), "Poison Seeker Arrow Trap"); break;
      case TRAP_OF_POISON_ARROWS_IV:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_BOLT, SV_BOLT_HEAVY, 12, 16, 40+randint(70), "Poison Seeker Bolt Trap"); break;
      case TRAP_OF_DAGGERS_I:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_SWORD, SV_BROKEN_DAGGER, 4, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_DAGGERS_II:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_SWORD, SV_DAGGER, 10, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGERS_I:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_SWORD, SV_BROKEN_DAGGER, 4, 8, 15+randint(20), "Poison Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGERS_II:
         ident = monster_handle_missile_trap(m_idx, 2+(p_ptr->mdepth / 15), TV_SWORD, SV_DAGGER, 10, 8, 20+randint(30), "Poison Dagger Trap"); break;

      case TRAP_OF_DROP_ITEMS:
         (*found)=handle_monster_drop_item(m_idx, 30);
         ident=(*found);
         break;
      case TRAP_OF_DROP_ALL_ITEMS:
         (*found)=handle_monster_drop_item(m_idx, 80);
         ident=(*found);
         break;
      case TRAP_OF_DROP_EVERYTHING:
         (*found)=handle_monster_drop_item(m_idx, 100);
         ident=(*found);
         break;

      /* Trap of Curse Weapon */
      /* monsters have no mana, so confusion reigns supreme */
      case TRAP_OF_CURSE_WEAPON:
      case TRAP_OF_MANA_DRAIN:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_format("%^s appears confused.");
            (*found) = TRUE;
         }
         m_ptr->confused += randint(30) + 15;
         ident = FALSE;
         break;
      case TRAP_OF_CURSE_ARMOR:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_format("%^s appears to slow down.");
            (*found) = TRUE;
         }
         if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
         ident = FALSE;
         break;
      /* Earthquake Trap */
      case TRAP_OF_EARTHQUAKE:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("As %s touches the trap, the ground starts to shake.");
         }
         earthquake(x, y, 10);
         (*found) = TRUE;
         ident = TRUE;
         break;
      /* Summon Monster Trap */
      case TRAP_OF_SUMMON_MONSTER:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("A spell hangs in the air.");
         }
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(x, y, p_ptr->mdepth, 0, 0);
         }
         (*found) = ident;
         break;
      /* Summon Undead Trap */
      case TRAP_OF_SUMMON_UNDEAD:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("A mighty spell hangs in the air.");
         }
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_UNDEAD, 0);
         }
         (*found) = ident;
         break;

      /* Summon Greater Undead Trap */
      case TRAP_OF_SUMMON_GREATER_UNDEAD:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("An old and evil spell hangs in the air.");
         }
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_HI_UNDEAD, 0);

         }
         (*found) = ident;
         break;

      /* Teleport Trap */
      case TRAP_OF_TELEPORT:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_format("%^s promptly disappears.", m_name);
            (*found) = TRUE;
            ident = TRUE;
         }
         teleport_away(m_idx, randint(40) + 20);
         break;
      /* Paralyzing Trap */
      case TRAP_OF_PARALYZING:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_format("%^s appears stunned.", m_name);
            ident = TRUE;
            (*found) = TRUE;
         }
         m_ptr->stun += randint(30) + 15;
         break;
      /* Explosive Device */
      case TRAP_OF_EXPLOSIVE_DEVICE:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_format("%^s is hit by an explosive device.", m_name);
            ident = TRUE;
            (*found) = TRUE;
         }
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (void)project(&who, 1, x, y, damroll(6,8),
                       GF_MANA,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         break;
      /* Teleport Away Trap & also trap door */
      case TRAP_OF_TELEPORT_AWAY:
      case TRAP_OF_SINKING:
         if (visible) msg_format("%^s triggers a trap.", m_name);
         if (item<0)
         {
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: TRAP_OF_TELEPORT_AWAY on non item %d @ %d,%d\n",
                 item, x, y);
         }
         else
         {
            (*found) = do_trap_teleport_away(i_ptr, item, x, y);
         }
         ident = FALSE;
         break;
      /* Lose Memory Trap */
      case TRAP_OF_LOSE_MEMORY:
         msg_format("%^s triggers a trap.", m_name);
         if (visible)
         {
            msg_format("%^s appears very confused.");
            (*found)=TRUE;
         }
         m_ptr->confused += randint(30) + 15;
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (void)project(&who, 3, x, y, damroll(8,10),
                       GF_DARK,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         ident=FALSE;
         break;
      /* Bitter Regret Trap */
      case TRAP_OF_BITTER_REGRET:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("An age-old and hideous sounding spell reverbs of the walls.");
            (*found)=TRUE;
         }
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (void)project(&who, 3, x, y, damroll(8,10),
                       GF_CONFUSION,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         m_ptr->stun += randint(30) + 45;
         ident=TRUE;
         break;
      /* Bowel Cramps Trap */
      case TRAP_OF_BOWEL_CRAMPS:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("A gas cloud emits.");
            (*found)=TRUE;
         }
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (void)project(&who, 3, x, y, damroll(4,10),
                       GF_POIS,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         ident=FALSE;
         break;
      /* Blindness/Confusion Trap */
      case TRAP_OF_BLINDNESS_CONFUSION:
         if (visible) msg_format("%^s triggers a trap.", m_name);
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (*found)=project(&who, 3, x, y, damroll(8,10),
                          GF_CONFUSION,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);

         (*found)|=project(&who, 3, x, y, damroll(8,10),
                           GF_LITE,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         ident=FALSE;
         break;
      /* Aggravation Trap */
      case TRAP_OF_AGGRAVATION:
         {
            project_who_type who;
            if (visible) msg_format("%^s triggers a trap.", m_name);
            msg_print("You hear a hollow noise echoing through the dungeons.");
            who.type = WHO_TRAPBYMONSTER;
            who.index = trap;
            who.trigger = m_idx;
            who.trigger_race = m_ptr->r_idx;
            ident = aggravate_monsters(&who, -1);
            (*found) = ident;
            break;
         }
      /* Multiplication Trap */
      case TRAP_OF_MULTIPLICATION:
      {
         project_who_type who;
         who.type = WHO_MONSTER;
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            (*found)=TRUE;
         }
         msg_print("You hear a loud click.");
         trap_creation(&who, m_ptr->fx, m_ptr->fy);
         break;
      }
      /* Steal Item Trap */
      case TRAP_OF_STEAL_ITEM:
         /* RF1_DROP_CHOSEN is set with the ultimate foe, he who shall */
         /* not be named, and we cannot steal from the Valar....       */
         if (m_ptr->has_drop &&
             (!(r_info[m_ptr->r_idx].flags1 & RF1_DROP_CHOSEN)) )
         {
            s16b number, i, chance;
            s16b is_idx = item_set_this_monster(m_idx);
            if (is_idx == -1)
            {
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: TRAP_OF_STEAL_ITEM: has_drop true, is_idx -1?\n");
               break;
            }
            number = items_in_set(is_idx);
            if (number==0)
            {
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: TRAP_OF_STEAL_ITEM: has_drop true, number == 0?\n");
               break;
            }
            chance = 100 / number;
            /* Scan through the slots backwards */
            for (i = number-1; i >= 0; i--)
            {
               /* Get the item in that slot */
               object_type *j_ptr = &i_list[is_list[is_idx].index[i]];

               /* Hack -- for now, skip artifacts */
               if (artifact_p(j_ptr)) continue;

               /* Give this item slot a shot at death */
               if (randint(100)<chance)
               {
                  char m_name[80];
                  monster_type *m_ptr = &mn_list[m_idx];
                  object_type forge;

                  /* strange things happen otherwise, you have been warned! */
                  forge.spell_set = 0;
                  invwipe(&forge);

                  monster_desc(m_name, m_ptr, 0x14);

                  /* increase chest rating */
                  i_ptr->p1val += (object_value(j_ptr)/10000)+3; /* chest rating increases! */

                  forge = i_list[is_list[is_idx].index[i]];

                  /* Destroy "amt" items */
                  monster_inven_increase(m_idx,is_idx,i,-99);
                  monster_inven_optimize(m_idx,is_idx);

                  /* more items is not so probable */
                  chance *= 2;
                  (*found) = TRUE;
               }
            }
         }
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            if ((*found))
            {
               msg_print("The chest seems so grow!");
               ident = TRUE;
            }
         }
         break;
      /* Summon Fast Quylthulgs Trap */
      case TRAP_OF_SUMMON_FAST_QUYLTHULGS:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
         }
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_QUYLTHULG, 0);
         }
         (*found) = ident;
         if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
         break;
      /* Summon Hounds Trap */
      case TRAP_OF_SUMMON_HOUNDS:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
         }
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(x, y, p_ptr->mdepth, SUMMON_QUYLTHULG, 2 + (p_ptr->mdepth / 7));
         }
         (*found) = ident;
         break;
      /* some traps which cannot be translated to monster effects */
      case TRAP_OF_CHARGES_DRAIN:
      case TRAP_OF_MISSING_MONEY:
      case TRAP_OF_NO_RETURN:
      case TRAP_OF_SILENT_SWITCHING:
      case TRAP_OF_WASTING_WANDS:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("Nothing seems to happen.");
            (*found) = TRUE;
         }
      case TRAP_OF_WALLS:
         {
            s16b dx,dy,cx,cy;
            s16b sx=0,sy=0,sn,i;
            cave_cell_type *cv_ptr;
            bool map[5][5] = { {FALSE,FALSE,FALSE,FALSE,FALSE},
                               {FALSE,FALSE,FALSE,FALSE,FALSE},
                               {FALSE,FALSE,FALSE,FALSE,FALSE},
                               {FALSE,FALSE,FALSE,FALSE,FALSE},
                               {FALSE,FALSE,FALSE,FALSE,FALSE} };
            for (dy = -2; dy <= 2; dy++)
            {
               for (dx = -2; dx <= 2; dx++)
               {
                  cx = mx+dx;
                  cy = my+dy;
                  if (!in_bounds(cx, cy)) continue;
                  cv_ptr = &dungeon.level[sublevel][cy][cx];
                  /* Lose room and vault */
                  cv_ptr->fdat &= ~(CAVE_ROOM | CAVE_VAULT);
                  /* Lose light and knowledge */
                  cv_ptr->fdat &= ~(CAVE_GLOW | CAVE_MARK);
                  /* Skip the center */
                  if (!dx && !dy) continue;
                  /* test for dungeon level */
                  if (randint(100) > 10+p_ptr->mdepth) continue;
                  /* Damage this grid */
                  map[2+dx][2+dy] = TRUE;
               }
            }
            for (dy = -2; dy <= 2; dy++)
            {
               for (dx = -2; dx <= 2; dx++)
               {
                  cx = mx+dx;
                  cy = my+dy;
                  if (!map[2+dx][2+dy]) continue;
                  cv_ptr = &dungeon.level[sublevel][cy][cx];

                  if (cv_ptr->m_idx)
                  {
                     monster_type *m_ptr = &mn_list[cv_ptr->m_idx];
                     monster_race *r_ptr = &r_info[m_ptr->r_idx];
                     /* Most monsters cannot co-exist with rock */
                     if (!(r_ptr->flags2 & RF2_KILL_WALL) &&
                         !(r_ptr->flags2 & RF2_PASS_WALL))
                     {
                        char m_name[80];

                        /* Assume not safe */
                        sn = 0;
                        /* Monster can move to escape the wall */
                        if (!(r_ptr->flags1 & RF1_NEVER_MOVE))
                        {
                           /* Look for safety */
                           for (i = 0; i < 8; i++)
                           {
                              /* Access the grid */
                              cy = my + ddy[i];
                              cx = mx + ddx[i];

                              /* Skip non-empty grids */
                              if (!empty_grid_bold(cx, cy)) continue;

                              /* Hack -- no safety on glyph of warding */
                              if (test_grid(cx, cy, DUNG_FLOOR, DUNG_FLOOR_GLYPH))
                                 continue;

                              /* Important -- Skip "quake" grids */
                              if (map[2+(cx-mx)][2+(cy-my)]) continue;
                              /* Count "safe" grids */
                              sn++;
                              /* Randomize choice */
                              if (rand_int(sn) > 0) continue;
                              /* Save the safe grid */
                              sx = cx; sy = cy;
                              ident=TRUE;
                              break; /* discontinue for loop - safe grid (*found) */
                           }
                        }
                        /* Describe the monster */
                        monster_desc(m_name, m_ptr, 0x4);
                        /* Scream in pain */
                        msg_format("%^s wails out in pain!", m_name);
                        /* Monster is certainly awake */
                        m_ptr->csleep = 0;
                        /* Apply damage directly */
                        m_ptr->hp -= (sn ? damroll(4, 8) : 200);
                        /* Delete (not kill) "dead" monsters */
                        if (m_ptr->hp < 0)
                        {
                           /* Message */
                           msg_format("%^s is entombed in the rock!", m_name);
                           /* Delete the monster */
dlog(DEBUGMONST,"object3.c: monster_activate_trap_type: m_idx %d (%s) just got entombed @ %d,%d\n",
                cv_ptr->m_idx,r_name+r_ptr->name, m_ptr->fx, m_ptr->fy);
                           delete_monster_idx(dungeon.level[sublevel][cy][cx].m_idx);
                           /* No longer safe */
                           sn = 0;
                        }
                        /* Hack -- Escape from the rock */
                        if (sn)
                        {
                           s16b m_idx = dungeon.level[sublevel][cy][cx].m_idx;
                           /* Update the new location */
                           dungeon.level[sublevel][sy][sx].m_idx = m_idx;
                           /* Update the old location */
                           dungeon.level[sublevel][cy][cx].m_idx = 0;
                           /* Move the monster */
                           m_ptr->fy = sy;
                           m_ptr->fx = sx;
                           /* do not change fz */
                           /* don't make rock on that square! */
                           if ( (sx>=(mx-2)) &&
                              (sx<=(mx+2)) &&
                              (sy>=(my-2)) &&
                              (sy<=(my+2)) )
                           {
                              map[2+(sx-mx)][2+(sy-my)]=FALSE;
                           }
                           /* Update the monster (new location) */
                           update_mon(m_idx, TRUE);
                           /* Redraw the old grid */
                           lite_spot(cx, cy);
                           /* Redraw the new grid */
                           lite_spot(sx, sy);
                        } /* if sn */
                     } /* if monster can co-exist with rock */
                  } /* if monster on square */
               } /* for dx */
            } /* for dy */

            /* Examine the quaked region */
            for (dy = -2; dy <= 2; dy++)
            {
               for (dx = -2; dx <= 2; dx++)
               {
                  /* Extract the location */
                  cx = mx + dx;
                  cy = my + dy;

                  /* Skip unaffected grids */
                  if (!map[2+dx][2+dy]) continue;

                  /* Access the cave grid */
                  cv_ptr = &dungeon.level[sublevel][cy][cx];

                  /* Paranoia -- never affect player */
                  if (!dy && !dx) continue;

                  /* Destroy location (if valid) */
                  if (valid_grid(cx, cy))
                  {
                     bool floor = floor_grid_bold(cx, cy);

                     /* Delete any object that is still there */
                     delete_object(cx, cy, -1);

                     if (floor)
                     {
                        place_wall(cx, cy);
                     }
                     else
                     {
                        /* Clear previous contents, add floor */
                        (void)set_grid_type(cx, cy, DUNG_FLOOR,
                                            DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
                     }
                  } /* valid */
               } /* dx */
            } /* dy */
            /* Mega-Hack -- Forget the view and lite */
            p_ptr->update |= PU_UN_VIEW;

            /* Update stuff */
            p_ptr->update |= (PU_VIEW | PU_FLOW);

            /* Update the monsters */
            p_ptr->update |= (PU_DISTANCE);

            /* Update the health bar */
            p_ptr->redraw1 |= (PR1_HEALTH);

            /* Redraw map */
            p_ptr->redraw1 |= (PR1_MAP);

            /* Window stuff */
            p_ptr->window |= (PW_OVERHEAD);
            handle_stuff();
            if (visible)
            {
               msg_format("%^s triggers a trap.", m_name);
               (*found) = TRUE;
            }
            else
            {
               msg_format("You feel a shudder going through the walls of the dungeon.");
            }
            ident = FALSE;
         }
         break;
      /* Trap of Calling Out */
      case TRAP_OF_CALLING_OUT:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
         }
         ident = teleport_monster_to(m_idx, px, py);
         (*found) = ident;
         break;
      /* Trap of Sliding */
      case TRAP_OF_SLIDING:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("A large, dence ball of ice erupts.");
         }
         who.type = WHO_TRAPBYMONSTER;
         who.index = trap;
         who.trigger = m_idx;
         who.trigger_race = m_ptr->r_idx;

         (*found)=project(&who, 1, mx, my, damroll(16,8), GF_COLD,PROJECT_KILL_MONSTER | PROJECT_KILL_PLAYER | PROJECT_JUMP);
         ident = (*found);
         break;
      /* Trap of Stair Movement */
      case TRAP_OF_STAIR_MOVEMENT:
         {
            s16b cx,cy,i,j;
            s16b cnt = 0;
            s16b cnt_seen = 0;
            s16b tmpm, tmps, tmpx;
            u32b tmpf;
            bool seen = FALSE;
            s16b index_x[20],index_y[20]; /* 20 stairs per level is enough? */
            cave_cell_type *cv_ptr;
            if (p_ptr->mdepth!=99) /* no sense in relocating that stair! */
            {
               for (cx=0;cx<MAX_WID;cx++)
               {
                  for (cy=0;cy<MAX_HGT;cy++)
                  {
                     cv_ptr = &dungeon.level[sublevel][cy][cx];
                     if (cv_ptr->mtyp!=DUNG_STAIR) continue;
                     /* strange stairs are not used yet, but they shouldn't be moved */
                     if (cv_ptr->styp==DUNG_STAIR_STRANGE) continue;
                     index_x[cnt]=cx;
                     index_y[cnt]=cy;
                     cnt++;
                  }
               }
               if (cnt==0)
               {
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: TRAP_OF_STAIR_MOVEMENT - no stairs (*found)!\n");
               }
               else
               {
                  for (i=0;i<cnt;i++)
                  {
                     for (j=0;j<10;j++) /* try 10 times to relocate */
                     {
                        cave_cell_type *c_ptr2 = &dungeon.level[sublevel][index_y[i]][index_x[i]];

                        cx=rand_int(cur_wid);
                        cy=rand_int(cur_hgt);
                        if ((cx==index_x[i]) || (cy==index_y[i])) continue;
                        if (!clean_grid_bold(cx, cy)) continue;

                        tmpm = dungeon.level[sublevel][cy][cx].mtyp;
                        tmps = dungeon.level[sublevel][cy][cx].styp;
                        tmpx = dungeon.level[sublevel][cy][cx].extra;
                        tmpf = dungeon.level[sublevel][cy][cx].fdat;
                        dungeon.level[sublevel][cy][cx].mtyp = c_ptr2->mtyp;
                        dungeon.level[sublevel][cy][cx].styp = c_ptr2->styp;
                        dungeon.level[sublevel][cy][cx].extra = c_ptr2->extra;
                        dungeon.level[sublevel][cy][cx].fdat = c_ptr2->fdat;
                        c_ptr2->mtyp  = tmpm;
                        c_ptr2->styp  = tmps;
                        c_ptr2->extra = tmpx;
                        c_ptr2->fdat  = tmpf;

                        /* if we are placing walls in rooms, make them rubble instead */
                        if ((c_ptr2->fdat & CAVE_ROOM) && (c_ptr2->mtyp == DUNG_WALL))
                        {
                           (void)set_grid_type(index_x[i], index_y[i],
                                               DUNG_WALL, DUNG_WALL_RUBBLE,
                                               GRID_KEEP, 0);
                        }
                        if (player_has_los_bold(cx, cy))
                        {
                           note_spot(cx,cy);
                           lite_spot(cx, cy);
                           seen=TRUE;
                        }
                        else
                        {
                           dungeon.level[sublevel][cy][cx].fdat &=~CAVE_MARK;
                        }
                        if (player_has_los_bold(index_x[i],index_y[i]))
                        {
                           note_spot(index_x[i],index_y[i]);
                           lite_spot(index_x[i],index_y[i]);
                           seen = TRUE;
                        }
                        else
                        {
                           dungeon.level[sublevel][index_y[i]][index_x[i]].fdat &=~CAVE_MARK;
                        }
                        break;
                     }
                     if (seen) cnt_seen++;
                     seen = FALSE;
                  } /* cnt loop */
                  ident = (cnt_seen>0);
                  if (visible)
                  {
                     msg_format("%^s triggers a trap.", m_name);
                     if ((ident) && (cnt_seen>1))
                     {
                        msg_print("You see some stairs move.");
                        (*found) = TRUE;
                        ident = TRUE;
                     }
                     else if (ident)
                     {
                        msg_print("You see a stair move.");
                        (*found) = TRUE;
                        ident = TRUE;
                     }
                  }
                  else
                  {
                     msg_print("You hear distant scraping noises.");
                  }
               } /* any stairs (*found) */
            }
            else
            {
               if (visible)
               {
                  msg_format("%^s triggers a trap.", m_name);
                  msg_print("Nothing seems to happen.");
                  (*found) = TRUE;
               }
            }
         }
         break;
      /* Trap of New Trap */
      case TRAP_OF_NEW:
         {
            s16b i;
            /* if we're on a floor or on a door, place a new trap */
            if ((item == -1) || (item == -2))
            {
               init_trap(tr_ptr);
               if (set_traps(tr_ptr,&i,p_ptr->mdepth,FTRAP_DOOR, 0, 0))
               {
                  if (dungeon.level[sublevel][y][x].mtyp == DUNG_TRAP)
                  {
                     place_trap(x, y, p_ptr->mdepth, 0, 0);
                  }
                  if (player_has_los_bold(x, y))
                  {
                     note_spot(x, y);
                     lite_spot(x, y);
                  }
               }
            }
            else
            {
               /* re-trap the chest */
               set_traps(tr_ptr,&i,p_ptr->mdepth,FTRAP_CHEST, 0, 0);
               i_ptr->p1val += i; /* this trap itself has p1val+0, so it's ok here */
               forget_item(i_ptr);
            }
            if (visible)
            {
               msg_format("%^s triggers a trap.", m_name);
               msg_print("You hear a noise, and then it's echo.");
               (*found) = TRUE;
            }
            ident=FALSE;
         }
         break;
      /* Trap of Scatter Items */
      case TRAP_OF_SCATTER_ITEMS:
         {
            s16b i,j;
            if (m_ptr->has_drop &&
                (!(r_info[m_ptr->r_idx].flags1 & RF1_DROP_CHOSEN)) )
            {
               s16b number;
               s16b is_idx = item_set_this_monster(m_idx);
               if (is_idx == -1)
               {
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: TRAP_OF_SCATTER_ITEMS: has_drop true, is_idx -1?\n");
                  break;
               }
               number = items_in_set(is_idx);
               if (number==0)
               {
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: TRAP_OF_SCATTER_ITEMS: has_drop true, number == 0?\n");
                  break;
               }
               if (visible) msg_format("%^s triggers a trap.", m_name);
               for (i=0;i<number;i++)
               {
                  object_type *j_ptr = &i_list[is_list[is_idx].index[i]];
                  if (!j_ptr->k_idx) continue;
                  if (rand_int(10)<3) continue;
                  for (j=0;j<10;j++)
                  {
                     object_type tmp_obj;
                     s16b cx = mx+15-rand_int(30);
                     s16b cy = my+15-rand_int(30);
                     if (!in_bounds(cx,cy)) continue;
                     if (!naked_grid_bold(cx,cy)) continue;
                     tmp_obj = *j_ptr;
                     monster_inven_increase(m_idx, is_idx, i, -999);
                     monster_inven_optimize(m_idx, is_idx);
                     if (floor_carry(&tmp_obj,cx,cy) == -1)
                     {
                        /* some error occured, silently delete this item */
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: TRAP_OF_SCATTER_ITEMS: floor_carry failed, item killed\n");
                        tmp_obj.k_idx = 0;
                     }
                     if (player_has_los_bold(cx, cy))
                     {
                        char i_name[80];
                        object_desc(i_name, &tmp_obj, TRUE, 3);
                        note_spot(cx, cy);
                        lite_spot(cx, cy);
                        ident=TRUE;
                        msg_format("Suddenly %s appears!",i_name);
                     }
                     break; /* valid location (*found) & used */
                  } /* try 10 times to find a location */
               } /* loop over all items */
            } /* monster with drop & not Valar */
            (*found) = ident;
         }
         break;
      /* Trap of Filling */
      case TRAP_OF_FILLING:
         {
            s16b nx, ny;
            for (nx=x-8;nx<=x+8;nx++)
            {
               for (ny=y-8;ny<=y+8;ny++)
               {
                  if (!in_bounds (nx, ny)) continue;
                  if (rand_int(distance(nx,ny,mx,my))>3)
                  {
                     place_trap(nx,ny,p_ptr->mdepth+rand_int(5), 10, 0);
                  }
               }
            }
            if (visible)
            {
               msg_format("%^s triggers a trap.", m_name);
               msg_print("The floor vibrates in a strange way.");
               (*found) = TRUE;
            }
            ident = FALSE;
         }
         break;
      case TRAP_OF_DRAIN_SPEED:
         if (visible)
         {
            msg_format("%^s triggers a trap.", m_name);
            msg_print("Nothing seems to happen.");
         }
         if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
         break;
      default:
      {
         msg_print(format("Executing unknown trap %d",trap));
      }
   }

   return ident;
}

void handle_unused_traps(trap_item_type *tr_ptr, s16b x, s16b y, s16b item,
                         object_type *i_ptr)
{
   if (!tr_ptr->inuse)
   {
      if (item<0)
      {
         cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

         c_ptr->t_idx = 0;    /* no longer any traps */
         /* set floor traps to empty floor, door traps to closed doors */
         if ( (c_ptr->mtyp == DUNG_FLOOR) ||
              (c_ptr->mtyp == DUNG_TRAP) )
         {
            (void)set_grid_type(x, y, DUNG_FLOOR,
                                DUNG_FLOOR_NORMAL, GRID_ADD, 0);
         }
         else if (c_ptr->mtyp!=DUNG_DOOR)
         {
            quit(format("Unknown floor type %d,%d at %x,%y with disintegrating trap",
                        c_ptr->mtyp,c_ptr->styp,x,y));
         }
         note_spot(x,y);
         lite_spot(x,y);
      }
      else
      {
         if (!tr_ptr->inuse) i_ptr->xtra2 = 0; /* no longer any traps */
      }
   }
   else if (item==-1)
   /* do we need to change the floor type because less traps exists on this */
   /* square? */
   {
      s16b traps = num_traps_ptr(tr_ptr, TRAP_FOUND);
dlog(DEBUGTRAPS,"object3.c: handle_unused_traps: %d traps left @ %d,%d\n",
                traps, tr_ptr->tx, tr_ptr->ty);
      handle_floor_with_traps(tr_ptr, x, y);
   }
}

/*
 * this functions makes sure a floor has the correct feature-type for the
 * amount of known/existing traps on it
 */
void handle_floor_with_traps(trap_item_type *tr_ptr, s16b x, s16b y)
{

   /* if it's still a normal floor*/
   s16b num_found = num_traps_ptr(tr_ptr, TRAP_FOUND);
   s16b num_exist = num_traps_ptr(tr_ptr, TRAP_EXISTS);

dlog(DEBUGTRAPS,"object3.c: player_execute_trap: %d traps found, %d existing\n",
             num_found, num_exist);

   if (num_exist==0)
   {
      /* signify we have no traps */
      (void)set_grid_type(x, y, DUNG_FLOOR,
                          DUNG_FLOOR_NORMAL, GRID_ADD, 0);
      tr_ptr->inuse = FALSE;
      dungeon.level[sublevel][y][x].t_idx = 0;
   }
   else
   {
      if (num_found==0)
      {
         /* signify we found no traps */
         (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_TRAP, GRID_ADD, 0);
      }
      else if (num_found==1)
      {
         (void)set_grid_type(x, y, DUNG_TRAP,
                             DUNG_TRAP_FNDONE, GRID_ADD, 0);
      }
      else
      {
         (void)set_grid_type(x, y, DUNG_TRAP,
                             DUNG_TRAP_FNDMORE, GRID_ADD, 0);
      }
   }

   note_spot(x, y);
   lite_spot(x, y);
}


/* there's a difference between chests and doors - chests affect i_ptr, */
/* doors affect c_ptr, etc.) */
/* item -1 means a floor     */
/* item -2 means a door      */
bool player_execute_trap(trap_item_type *tr_ptr,s16b item, s16b x, s16b y, bool bash)
{
   s16b                k, trap, chance;
   bool                test1, test2;
   bool                better_known = FALSE;
   bool                ident = FALSE;
   trap_type          *t_ptr;
   cave_cell_type     *c_ptr = NULL;
   object_type        *i_ptr = NULL;

dlog(DEBUGTRAPS,"object3.c: player_execute_trap at %d, %d item %d\n", x, y, item);
   if (item<=-1)
   {
      c_ptr = &dungeon.level[sublevel][y][x];
      if (!c_ptr->t_idx) return (FALSE);
   }
   else
   {
/* tr_ptr x and y are not valid when it's a carried chest */
      i_ptr = get_item_pointer_xy(item,x,y);
      if (!i_ptr->xtra2) return (FALSE);
   }

   trap = get_random_trap(tr_ptr, TRAP_EXISTS);
dlog(DEBUGTRAPS,"object3.c: player_execute_trap at %d, %d trap %d\n", x, y, trap);

   if (trap<0)
   {
      msg_print("object3.c: player_execute_trap: No traps found - error!");
      if (item<=-1)
      {
         c_ptr->t_idx=0;
      }
      else
      {
         i_ptr->xtra2=0;
      }

      tr_ptr->inuse = FALSE;
      return (FALSE);
   }
   t_ptr = &t_info[trap];

   set_trap_found_ptr(tr_ptr, trap); /* we have now found this trap! - even if */
                                     /* we evade it later on */
dlog(DEBUGTRAPS,"object3.c: player_execute_traps: after setting trap %d to known, num_traps %d\n",
                trap, num_traps_ptr(tr_ptr, TRAP_FOUND));
   /* make sure the floor is changed */
   if (item==-1)
   {

dlog(DEBUGTRAPS,"object3.c: player_execute_trap at %d,%d: i_idx %d m_idx %d t_idx %d\n", x, y,
           c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
      handle_floor_with_traps(tr_ptr, x, y);
   }

   test1=test_activate_trap_dex(trap);
   test2=test_activate_trap_knowledge(trap);

   if (t_ptr->known==0) k=0;
   else if (t_ptr->known<10) k=1;
   else if (t_ptr->known<25) k=2;
   else if (t_ptr->known<75) k=3;
   else if (t_ptr->known<150) k=4;
   else if (t_ptr->known<300) k=5;
   else if (t_ptr->known<750) k=6;
   else if (t_ptr->known<2000) k=7;
   else if (t_ptr->known<5000) k=8;
   else k=9;
   if (test1 && test2)
      msg_format("You clumsily touch a trap that you %s.",knowstr[k]);
   else if (test1)
      msg_format("You blunder into a trap you %s.",knowstr[k]);
   else if (test2)
      msg_format("You fail to avoid this trap that you %s.",knowstr[k]);
   else
   {
      msg_format("You nimbly avoid activating a trap, that you %s.",knowstr[k]);
      return (FALSE);
   }

   t_ptr->known++;            /* we hit this trap once */

   chance=p_ptr->skill_srh+adj_mag_stat[p_ptr->stat_ind[A_INT]];
   chance+=(t_ptr->known/100)-t_ptr->difficulty*2;

   /* chance ranges from 0 to src 13+adj 20+known-2*difficulty 15= -7+known */
   if (rand_int(100+chance)>95)
   {
      /* we use the opportunity to study this trap */
      t_ptr->known+=rand_int(30);
      better_known=TRUE;
   }

   ident = player_activate_trap_type(x, y, i_ptr, item, tr_ptr, trap);

   if (better_known)
   {
      msg_print("You study the design of this trap thoroughly.");
   }

/* hack - trap 44 is a trap of new trap - that shouldn't disintegrate */
   if (bash ||
       ((rand_int(10*t_ptr->difficulty)<5) && (trap!=44)) )
   {
      msg_print("The trap seems to disintegrate.");
      set_trap(tr_ptr,trap,FALSE);
dlog(DEBUGTRAPS,"object3.c: after disintegrating trap %d\n", trap);
   }
   else /* if the trap doesn't disintegrate, and it's an item, */
   {    /* we now know there's a trap on it!                   */
        /* if it does disintegrate, we know nothing again..... */
     if (item>=0)
     {
       object_known(i_ptr);
     }
   }

   handle_unused_traps(tr_ptr, x, y, item, i_ptr);

   if (!t_ptr->ident && ident)
   {
      cptr name = t_name + t_ptr->name;

      t_ptr->ident |= ident;
      msg_format("You now recognize this trap as %s %s.",
                 is_a_vowel(name[0])?"an":"a", name);
   }
   return (TRUE);
}

/* there's a difference between chests and doors - chests affect i_ptr, */
/* doors affect c_ptr, etc.) */
/* item -1 means a floor     */
/* item -2 means a door      */
bool monster_execute_trap(s16b m_idx, trap_item_type *tr_ptr, s16b item,
                          s16b x, s16b y, bool bash)
{
   s16b                trap, mon_int;
   bool                found, ident, bashed, visible;
   trap_type          *t_ptr;
   cave_cell_type          *c_ptr = NULL;
   object_type        *i_ptr = NULL;
   monster_type       *m_ptr = &mn_list[m_idx];
   monster_race       *r_ptr = &r_info[m_ptr->r_idx];

dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: m_idx %d at %d, %d item %d bash %d\n",
                m_idx, x, y, item, bash);
   if (item<=-1)
   {
      c_ptr = &dungeon.level[sublevel][y][x];
      if (!c_ptr->t_idx) return (FALSE);
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: valid floor tile.\n");
   }
   else
   {
/* tr_ptr x and y are not valid when it's a carried chest */
      s16b is_idx = item_set_this_monster(m_idx);
      if (is_idx == -1)
      {
dlog(DEBUGALWAYS,"object3.c: monster_activate_trap_type: monster_execute_trap: item>=0, is_idx -1?\n");
         return FALSE;
      }
      i_ptr = &i_list[is_list[is_idx].index[item]];
      if (!i_ptr->xtra2) return (FALSE);
   }

   /* monster walks on air? */
   if ((item == -1) && (r_ptr->flags3 & RF3_NO_FLOOR))
   {
      return (FALSE);
   }
   /* monster moves through doors? */
   if ((item == -2) && (r_ptr->flags2 & RF2_PASS_WALL))
   {
      return (FALSE);
   }

   trap = get_random_trap(tr_ptr, TRAP_EXISTS);
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: %d,%d chosen trap %d\n",
                x, y, trap);

   if (trap<0)
   {
      msg_print("object3.c: monster_execute_trap: No traps found - error!");
      if (item<=-1)
      {
         c_ptr->t_idx=0;
      }
      else
      {
         i_ptr->xtra2=0;
      }

      tr_ptr->inuse = FALSE;
      return (FALSE);
   }

   visible = TRUE;
   if (p_ptr->blind) visible = FALSE;
   if (!player_has_los_bold(m_ptr->fx, m_ptr->fy)) visible = FALSE;
   if (!m_ptr->ml) visible = FALSE;

   t_ptr = &t_info[trap];

   /* monster is evidently ethereal :-) */
   if (r_ptr->flags2 & RF2_PASS_WALL)
   {
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: monster is ethereal\n");

      return(FALSE);
   }

   /* floor trap in vault is known to monster */
   if ((item==-1) && (c_ptr->fdat & CAVE_VAULT)) return (FALSE);

   if (r_ptr->flags2 & (RF2_KILL_WALL))
   {
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: monster is will bash always (RF2_KILL_WALL)\n");
      bash = TRUE;
   }

   mon_int = r_ptr->level;
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: base mon_int %d stup %d smart %d traps %d\n",
                mon_int, (r_ptr->flags2 & RF2_STUPID), (r_ptr->flags2 & RF2_SMART));
   /* take into account the intelligence of this monster */
   if (r_ptr->flags2 & RF2_STUPID) mon_int /= 2;
   if (r_ptr->flags2 & RF2_SMART) mon_int *= 2;

   /* ah, an expert */
   if (r_ptr->flags6 & RF6_TRAPS) mon_int *= 2;

   /* it's their home, you see. They *know* where they are... */
   if (sublevel != 0) mon_int *= 10;

   /* they don't look... */
   if (m_ptr->confused || m_ptr->stun) mon_int = -1;

dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: mon_int %d know traps %d conf %d stun %d\n",
                mon_int, (r_ptr->flags6 & RF6_TRAPS), m_ptr->confused, m_ptr->stun);

   /* if they move erratically - cumulative on purpose! */
   if (r_ptr->flags1 & RF1_RAND_25) mon_int /= 2;
   if (r_ptr->flags1 & RF1_RAND_50) mon_int /= 2;

   /* a faster monster steps more easily on a trap */
   /* monster speeds range from 90 to 140 so we divide by 0.9 to 1.4 here */
   if (m_ptr->mspeed != 0)
   {
      mon_int = (100 * mon_int) / m_ptr->mspeed;
   }
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: mon_int %d rand25 %d rand50 %d spd %d\n",
                mon_int, (r_ptr->flags1 & RF1_RAND_25), (r_ptr->flags1 & RF1_RAND_50),
                m_ptr->mspeed);

   /* the traps isn't difficult enough to trouble this monster */
   if ((5*t_ptr->difficulty+10*randint(t_ptr->difficulty)) < mon_int)
   {
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: diff %d, mon_int %d resisted %d+10*randint(%d)\n",
                t_ptr->difficulty, mon_int, 5*t_ptr->difficulty, t_ptr->difficulty);

      return (FALSE);
   }

   bashed = FALSE;
   if ( bash ||
        ( (randint(30) < 100) && (trap != TRAP_OF_NEW ) ) )
   {
      set_trap(tr_ptr,trap,FALSE);
      bashed = TRUE;
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: after disintegrating trap %d\n", trap);
   }
   ident = monster_activate_trap_type(m_idx, x, y, i_ptr, item, tr_ptr, trap, &found);
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: after executing trap %d ident %d found %d\n",
                trap, ident, found);


   /* found signifies the player should know there is a trap where the monster is */
   /* ident signifies the player has some indication as to what trap that may be  */

   /* currently, if found = TRUE, and the player has id'ed this trap-type before */
   /* he will know (recognize) what trap it is */
   /* if only ident = TRUE, we know nothing at all - not even that a trap is there */

   if (found)
   {
      set_trap_found_ptr(tr_ptr, trap); /* we have now found this trap! - even if */
                                        /* we evade it later on */
dlog(DEBUGTRAPS,"object3.c: monster_execute_traps: after setting trap %d to known, %d traps found\n",
                trap, num_traps_ptr(tr_ptr, TRAP_FOUND));
      /* make sure the floor is changed */
      if (item==-1)
      {

dlog(DEBUGTRAPS,"object3.c: monster_execute_trap at %d,%d: i_idx %d m_idx %d t_idx %d\n", x, y,
           c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);

         /* if it's still a normal floor*/
         if (c_ptr->mtyp==DUNG_FLOOR)
         {
            /* signify we found one trap */
            (void)set_grid_type(x, y, DUNG_TRAP,
                                DUNG_TRAP_FNDONE, GRID_ADD, 0);

dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: changing floor to DUNG_TRAP_FNDONE\n");
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap after at %d,%d: i_idx %d m_idx %d t_idx %d\n", x, y,
           c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);

            /* it could be argued we should test for m_ptr->ml here */
            /* a ghost that triggers a trap -> visible trap or not? */
            note_spot(x, y);
            lite_spot(x, y);
         }
         else if (test_grid_ptr(c_ptr, DUNG_TRAP, DUNG_TRAP_FNDONE))
         {
            /* signify more traps known */
            (void)set_grid_type(x, y, DUNG_TRAP,
                                DUNG_TRAP_FNDMORE, GRID_ADD, 0);

dlog(DEBUGTRAPS,"object3.c: monster_execute_trap: changing floor to DUNG_TRAP_FNDMORE\n");
dlog(DEBUGTRAPS,"object3.c: monster_execute_trap after at %d,%d: i_idx %d m_idx %d t_idx %d\n", x, y,
           c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);

            note_spot(x, y);
            lite_spot(x, y);
         }
      }
   } /* found */

   handle_unused_traps(tr_ptr, x, y, item, i_ptr);
   if (bashed && visible) msg_print("The trap seems to disintegrate.");
   return (TRUE);

}

/*
 * create trap with spell
 * return FALSE on failure or abortion
 */
bool make_trap_from_spell(s16b level, bool complex)
{
   cave_cell_type *c_ptr;
   trap_type *t_ptr;
   trap_item_type *tr_ptr;
   s16b       index[t_number];
   bool       chosen[t_number];
   bool       cont, redisplay;
   s16b       i, cnt, cur_page, cur_item, t_idx;
   s16b       dir, x, y, max, num;
   char       which;
   char       buf[80];

   /* init arrays */
   for (i=0; i < t_number; i++)
   {
      index[i]=0;
      chosen[i]=FALSE;
   }
   if (p_ptr->blind)
   {
      msg_print("You cannot see!");
      return (FALSE);
   }
   if (p_ptr->confused)
   {
      msg_print("That would not be wise!");
      return (FALSE);
   }
   if (p_ptr->stun)
   {
      msg_print("Your hands shake too much!");
      return (FALSE);
   }

   /* find appropriate traps */
   cnt = 0;
   for (i=0; i < t_number; i++)
   {
      t_ptr = &t_info[i];
      /* some simple traps are known to everybody */
      if ( (i == TRAP_OF_ARROW_I) || (i == TRAP_OF_DAGGER_I) ||
           (i == TRAP_OF_POISON_NEEDLE) || (i == TRAP_OF_ELEC_BOLT) )
      {
         index[cnt++]=i;
dlog(DEBUGTRAPS,"object3.c: make_trap_from_spell: trap %d %s diff %d added as %d\n",
                i, t_name + t_ptr->name, t_ptr->difficulty, cnt-1);
         continue;
      }

      if (!t_ptr->name) continue;
      /* no traps for chests only */
      if (!t_ptr->flags & FTRAP_DOOR) continue;
      /* rogues need to have seen this trap at least 25 times */
      if ((p_ptr->pclass == CLASS_ROGUE) && (trap_experience(t_ptr)<2)) continue;
      /* non-rogues at least 300 times */
      if ((p_ptr->pclass != CLASS_ROGUE) && (trap_experience(t_ptr)<5)) continue;

      /* if it's the right level, choose it - level 3 gets all traps... */
      if ( (t_ptr->difficulty<=(2+level*2)) || (level==3))
      {
         index[cnt++]=i;
dlog(DEBUGTRAPS,"object3.c: make_trap_from_spell: trap %d %s diff %d added as %d\n",
                i, t_name + t_ptr->name, t_ptr->difficulty, cnt-1);
      }
   }

   cont = TRUE;
   Term_save();
   cur_page = 0;
   redisplay = TRUE;

dlog(DEBUGTRAPS,"object3.c: make_trap_from_spell: %d traps found\n", cnt);

   while (cont==TRUE)
   {
      Term_clear();
      if (cnt > 20)
      {
         prt(format("Known traps:               (page %d of %d - press < or > to switch pages)",
                    cur_page+1, 1+(cnt / 20)), 0, 0);
      }
      else
      {
         prt("Known traps:", 0, 0);
      }
      prt("Press a letter to select a trap, z to create the trap", 0, 22);

      max = 20 + (cur_page * 20);
      if (max > cnt) max = cnt;

dlog(DEBUGTRAPS,"object3.c: displaying entries %d to %d\n", cur_page*20, max);
      for (i=(0+(cur_page * 20)); i < max; i++)
      {
         sprintf(buf,"%c%c - %s", (chosen[i])?'*':' ',
                                  I2A((i%20)), t_name + t_info[index[i]].name);
         prt(buf, 0, 2+(i % 20) );
      }

      which = inkey();                           /* Get a key */
      switch (which)
      {
         /* left or previous page */
         case '<':
            cur_page--;
            if (cur_page < 0) cur_page = ((cnt-1) / 20);
            redisplay = TRUE;
            break;
         /* right or next page */
         case '>':
            cur_page++;
            if (cur_page > ( (cnt-1) / 20) ) cur_page = 0;
            redisplay = TRUE;
            break;
         case 'a':
         case 'b':
         case 'c':
         case 'd':
         case 'e':
         case 'f':
         case 'g':
         case 'h':
         case 'i':
         case 'j':
         case 'k':
         case 'l':
         case 'm':
         case 'n':
         case 'o':
         case 'p':
         case 'q':
         case 'r':
         case 's':
         case 't':
            cur_item = A2I(which+cur_page*20);
            if (cur_item>=max)
            {
               if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);
               prt("Trying to choose too many traps!                                ", 0, 22);
               delay(200);
            }
            else
            {
               /* adding a single trap wipes all other chosen ones */
               if (!complex)
               {
                  for (i=0; i < cnt; i++)
                  {
                     chosen[i]=FALSE;
                  }
                  chosen[cur_item]=TRUE;
               }
               else
               /* we can keep adding until we've had enough! */
               {
                  s16b tmp = 0;
                  /* count already chosen traps */
                  for (i=0; i < cnt; i++)
                  {
                     if (chosen[i]) tmp++;
                  }
                  if (tmp > MAX_TRAPS_IN_SET)
                  {
                     if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);
                     prt("Trying to choose too many traps!                                ", 0, 22);
                     delay(200);
                  }
                  else
                  {
                     chosen[cur_item]=TRUE;
                  }
               }
            }
            break;
         case 'z':
            cont = FALSE;
            break;
         case ESCAPE:
            Term_load();
            return (FALSE); /* we haven't allocated anything, so just quit */
         default:
            if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);
            prt("Illegal key!                                      ", 0, 22);
            delay(200);
            break;
      } /* end switch on key */
   } /* end while */
   Term_load();

   t_idx = t_pop();
   if (t_idx==-1)
   {
      dlog(DEBUGALWAYS,"object3.c: make_trap_from_spell: unable to create trap_item_type\n");
      return (FALSE);
   }
   /* paranoia - did we ask to create an empty trap? */
   num=0;
   for (i=0; i < cnt; i++)
   {
      if (chosen[i]) num++;
   }
   if (num==0)
   {
      msg_print("You waste your precious mana.");
      return (TRUE);
   }

   tr_ptr = &t_list[t_idx];
   tr_ptr->inuse = TRUE;

   for (i=0; i < cnt; i++)
   {
      if (chosen[i])
      {
         set_trap(tr_ptr, index[i], TRUE);
         set_trap_found_ptr(tr_ptr, index[i]);
      }
   }
for (i=0; i < MAX_TRAPS_IN_SET; i++)
{
  dlog(DEBUGTRAPS,"object3.c: make_trap_from_spell: tr_ptr->type[%d]=%d, found %d\n",
                  i, tr_ptr->type[i], tr_ptr->found[i]);
}
   cont = TRUE;
   while (cont)
   {
      if (get_rep_dir(&dir))
      {
         /* Get requested location */
         x = px + ddx[dir];
         y = py + ddy[dir];

         if (!in_bounds(x, y)) continue;

         c_ptr = &dungeon.level[sublevel][y][x];
         p_ptr->command_dir = 0;
         if (c_ptr->t_idx)
         {
            msg_print("There is already a trap here!");
            p_ptr->command_dir = 0;
            continue;
         }
         /* this is possible spoilerish if it's an invisible monster! */
         if (c_ptr->m_idx)
         {
            msg_print("Kill the monster first!");
            continue;
         }

         if ((c_ptr->mtyp == DUNG_FLOOR) && (c_ptr->styp == DUNG_FLOOR_NORMAL))
         {
            if (complex && num_traps_ptr(tr_ptr, TRAP_EXISTS) > 1)
            {
dlog(DEBUGTRAPS,"object3.c: make_trap_from_spell: complex %d exists %d for %d,%d\n",
                complex, num_traps_ptr(tr_ptr, TRAP_EXISTS), x, y);
               (void)set_grid_type(x, y, DUNG_TRAP, DUNG_TRAP_FNDMORE, GRID_ADD, CAVE_PLAYERTRAP);
            }
            else
            {
dlog(DEBUGTRAPS,"object3.c: make_trap_from_spell: adding 1 trap to %d,%d\n", x, y);
               (void)set_grid_type(x, y, DUNG_TRAP, DUNG_TRAP_FNDONE, GRID_ADD, CAVE_PLAYERTRAP);

            }
            cont = FALSE;
         }
         /* only on closed, locked or jammed doors */
         else if ((c_ptr->mtyp == DUNG_DOOR) &&
                  ( (c_ptr->styp == DUNG_DOOR_CLOSED) ||
                    (c_ptr->styp == DUNG_DOOR_JAMMED) ||
                    (c_ptr->styp == DUNG_DOOR_LOCKED) ) )
         {
            cont = FALSE;
         }
         /* now we know x, y */
         if (cont == FALSE)
         c_ptr->t_idx = t_idx;
         tr_ptr->tx = x;
         tr_ptr->ty = y;
dlog(DEBUGTRAPS,"object3.c: make_trap_from_spell: %d found, %d unfound, %d total @ %d,%d\n",
                num_traps_ptr(tr_ptr, TRAP_FOUND), num_traps_ptr(tr_ptr, TRAP_NOTFOUND),
                num_traps_ptr(tr_ptr, TRAP_EXISTS), x, y);
         note_spot(x, y);
         lite_spot(x, y);
         p_ptr->update |= (PU_VIEW);
         update_stuff();
         msg_print("You made a beautiful trap.");
      }
      else /* press escape when asked for dir? */
      {
         cont = FALSE;
         /* free this trap-item */
         tr_ptr->inuse = FALSE;
         return (FALSE);
      }
   }
   return (TRUE);
}
