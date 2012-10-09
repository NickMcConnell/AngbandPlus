/* File: monster3.c */

/* Purpose: everything to do with monster inventories */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 * extensive changes Jurriaan 1996
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

int monster_inven_damage(s16b m_idx, s16b is_idx, inven_func typ, s16b perc)
{
   s16b          i, j, k, amt;
   object_type  *i_ptr;
   char          i_name[80];
   s16b          number = items_in_set(is_idx);

   /* Count the casualties */
   k = 0;

   /* Scan through the slots backwards */
   for (i = number-1; i >= 0; i--)
   {
      /* Get the item in that slot */
      i_ptr = &i_list[is_list[is_idx].index[i]];

      /* Hack -- for now, skip artifacts */
      if (artifact_p(i_ptr)) continue;

      /* Give this item slot a shot at death */
      if ((*typ)(i_ptr))
      {
         /* Count the casualties */
         for (amt = j = 0; j < i_ptr->number; ++j)
         {
            if (rand_int(100) < perc) amt++;
         }

         /* Some casualities */
         if (amt)
         {
            char m_name[80];
            monster_type *m_ptr = &mn_list[m_idx];
            monster_desc(m_name, m_ptr, 0x02);

            /* Get a description */
            object_desc(i_name, i_ptr, FALSE, 3);

            /* Message */
            if ((!p_ptr->blind) &&
                (player_has_los_bold(mn_list[m_idx].fx,mn_list[m_idx].fy)))
            {
               msg_format("You see that%s the %s of %s %s destroyed!",
                          (amt < i_ptr->number)?" some of":"", i_name, m_name,
                          (i_ptr->number > 1) ? "are" : "is");
            }

            /* Destroy "amt" items */
            monster_inven_increase(m_idx,is_idx,i,-amt);
            monster_inven_optimize(m_idx,is_idx);
            /* Count the casualties */
            k += amt;
         }
      }
   }

   /* Return the casualty count */
   return (k);
}

void monster_poison_dam(s16b m_idx, s16b is_idx, s16b dam)
{
   s16b chance = (dam < 30) ? 2 : (dam < 60) ? 4 : 7;
   s16b mychance;

   s16b i;
   s16b number = items_in_set(is_idx);
   object_type *j_ptr;
   for (i=0;i<number;i++)
   {
      j_ptr = &i_list[is_list[is_idx].index[i]];
      mychance = chance * j_ptr->number;
      if ((j_ptr->tval==TV_FOOD) && (j_ptr->sval!=SV_FOOD_CURE_POISON))
      {
         if (rand_int(100)<mychance)
         {
            char i_name[80];
            object_desc(i_name, j_ptr, FALSE, 0x40);
            if (j_ptr->number==1)
            {
               char m_name[80];
               monster_type *m_ptr = &mn_list[m_idx];
               monster_desc(m_name, m_ptr, 0x10);
               j_ptr->sval = SV_FOOD_POISON;
               j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
               if ((!p_ptr->blind) &&
                   (player_has_los_bold(mn_list[m_idx].fx,mn_list[m_idx].fy)))
               {
                  msg_format("You see that the %s of %s is changed!",
                             i_name, m_name);
               }
            }
            else
            {
               object_type tmp_obj;

               object_desc(i_name, j_ptr, FALSE, 3);
               tmp_obj = *j_ptr;
               tmp_obj.number = rand_int(j_ptr->number/(9-chance));
               j_ptr->number -= tmp_obj.number;
               tmp_obj.sval = SV_FOOD_POISON;
               j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
               if (monster_inven_carry(m_idx,&tmp_obj)==-1)
               {
                  monster_type *m_ptr = &mn_list[m_idx];
                  (void)drop_near(&tmp_obj,0,m_ptr->fx,m_ptr->fy,
                                  drop_how(&tmp_obj), FALSE, FALSE);
               }
               else
               {
                  if ((!p_ptr->blind) &&
                      (player_has_los_bold(mn_list[m_idx].fx,mn_list[m_idx].fy)))
                  {
                     char m_name[80];
                     monster_type *m_ptr = &mn_list[m_idx];
                     monster_desc(m_name, m_ptr, 0x10);
                     msg_format("You see that the %s of %s %s changed!",
                                i_name, m_name, (j_ptr->number > 1) ? "are" : "is");
                  }
               }
            }
         }
      }
      else if ((j_ptr->tval==TV_POTION) && (j_ptr->sval!=SV_POTION_CURE_POISON))
      {
         if (rand_int(100)<mychance)
         {
            char i_name[80];
            object_desc(i_name, j_ptr, FALSE, 0x40);
            if (j_ptr->number==1)
            {
               j_ptr->sval = SV_POTION_POISON;
               j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
               if ((!p_ptr->blind) &&
                   (player_has_los_bold(mn_list[m_idx].fx,mn_list[m_idx].fy)))
               {
                  char m_name[80];
                  monster_type *m_ptr = &mn_list[m_idx];
                  monster_desc(m_name, m_ptr, 0x10);
                  msg_format("You see that the %s of %s is changed!",
                             i_name, m_name);
               }
            }
            else
            {
               object_type tmp_obj;
               tmp_obj = *j_ptr;
               tmp_obj.number = rand_int(j_ptr->number/(9-chance));
               j_ptr->number -= tmp_obj.number;
               tmp_obj.sval = SV_POTION_POISON;
               j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
               if (monster_inven_carry(m_idx,&tmp_obj)==-1)
               {
                  monster_type *m_ptr = &mn_list[m_idx];
                  (void)drop_near(&tmp_obj,0,m_ptr->fx,m_ptr->fy,
                                  drop_how(&tmp_obj), FALSE, FALSE);
               }
               else
               {
                  if ((!p_ptr->blind) &&
                      (player_has_los_bold(mn_list[m_idx].fx,mn_list[m_idx].fy)))
                  {
                     char m_name[80];
                     monster_type *m_ptr = &mn_list[m_idx];
                     monster_desc(m_name, m_ptr, 0x10);
                     msg_format("You see that the %s of %s %s changed!",
                                i_name, m_name, (j_ptr->number > 1) ? "are" : "is");
                  }
               }
            }
         }
      }
   }
}

void monster_disenchant_dam(s16b m_idx, s16b is_idx, s16b dam)
{
   s16b chance = (dam < 30) ? 2 : (dam < 60) ? 4 : 7;
   s16b mychance;

   s16b i;
   s16b number = items_in_set(is_idx);
   object_type *i_ptr;
   for (i=0;i<number;i++)
   {
      i_ptr = &i_list[is_list[is_idx].index[i]];
      mychance = chance * i_ptr->number;
      if (artifact_p(i_ptr) && (rand_int(100) < 60)) continue;
      if ((i_ptr->tval==TV_HAFTED) ||
          (i_ptr->tval==TV_POLEARM) ||
          (i_ptr->tval==TV_SWORD) ||
          (i_ptr->tval==TV_BOW) ||
          (i_ptr->tval==TV_ARROW) ||
          (i_ptr->tval==TV_BOLT) ||
          (i_ptr->tval==TV_DIGGING) ||
          (i_ptr->tval==TV_SOFT_ARMOR) ||
          (i_ptr->tval==TV_HARD_ARMOR) ||
          (i_ptr->tval==TV_DRAG_ARMOR) ||
          (i_ptr->tval==TV_SHIELD) ||
          (i_ptr->tval==TV_HELM) ||
          (i_ptr->tval==TV_CROWN) ||
          (i_ptr->tval==TV_BOOTS) ||
          (i_ptr->tval==TV_CLOAK) ||
          (i_ptr->tval==TV_GLOVES))
      {
         u64b f1, f2, f3;
         char i_name[80];

         object_flags(i_ptr, &f1, &f2, &f3);
         if (f2 & TR2_RES_DISEN) continue; /* that wouldn't do */
         if ((i_ptr->to_h <= 0) &&
             (i_ptr->to_d <= 0) &&
             (i_ptr->to_a <= 0)) continue;
         object_desc(i_name, i_ptr, FALSE, 0x40);
         if (rand_int(100)>mychance) continue;
         if (i_ptr->number==1)
         {
            if (i_ptr->to_h > 0) i_ptr->to_h--;
            if ((i_ptr->to_h > 5) && (rand_int(100) < 20)) i_ptr->to_h--;

            /* Disenchant todam */
            if (i_ptr->to_d > 0) i_ptr->to_d--;
            if ((i_ptr->to_d > 5) && (rand_int(100) < 20)) i_ptr->to_d--;

            /* Disenchant toac */
            if (i_ptr->to_a > 0) i_ptr->to_a--;
            if ((i_ptr->to_a > 5) && (rand_int(100) < 20)) i_ptr->to_a--;
         }
         else
         {
            object_type tmp_obj;
            tmp_obj = *i_ptr;
            tmp_obj.number = rand_int(i_ptr->number/(9-chance));
            i_ptr->number -= tmp_obj.number;
            if (tmp_obj.to_h > 0) tmp_obj.to_h--;
            if ((tmp_obj.to_h > 5) && (rand_int(100) < 20)) tmp_obj.to_h--;

            /* Disenchant todam */
            if (tmp_obj.to_d > 0) tmp_obj.to_d--;
            if ((tmp_obj.to_d > 5) && (rand_int(100) < 20)) tmp_obj.to_d--;

            /* Disenchant toac */
            if (tmp_obj.to_a > 0) tmp_obj.to_a--;
            if ((tmp_obj.to_a > 5) && (rand_int(100) < 20)) tmp_obj.to_a--;
            if (monster_inven_carry(m_idx,&tmp_obj)==-1)
            {
               monster_type *m_ptr = &mn_list[m_idx];
               (void)drop_near(&tmp_obj,0,m_ptr->fx,m_ptr->fy,
                               drop_how(&tmp_obj), FALSE, FALSE);
            }
            else
            {
               if ((!p_ptr->blind) &&
                   (player_has_los_bold(mn_list[m_idx].fx,mn_list[m_idx].fy)))
               {
                  char m_name[80];
                  monster_type *m_ptr = &mn_list[m_idx];
                  monster_desc(m_name, m_ptr, 0x10);
                  msg_format("You see that the %s of %s %s changed!",
                             i_name, m_name, (i_ptr->number > 1) ? "are" : "is");
               }
            }
         }
      }
   }
}

void test_monster_inven_damage(s16b m_idx, s16b typ, s16b dam)
{
   monster_race *r_ptr = &r_info[mn_list[m_idx].r_idx];

   switch (typ)
   {
      case GF_ACID:
         if (!(r_ptr->flags3 & RF3_IM_ACID))
         {
            s16b is_idx = item_set_this_monster(m_idx);
            s16b chance = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
            if (is_idx != -1) monster_inven_damage(m_idx, is_idx, set_acid_destroy, chance);
         }
         break;
      /* Electricity */
      case GF_ELEC:
         if (!(r_ptr->flags3 & RF3_IM_ELEC))
         {
            s16b is_idx = item_set_this_monster(m_idx);
            s16b chance = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
            if (is_idx != -1) monster_inven_damage(m_idx, is_idx, set_elec_destroy, chance);
          }
         break;
      /* Fire damage */
      case GF_FIRE:
         if (!(r_ptr->flags3 & RF3_IM_FIRE))
         {
            s16b is_idx = item_set_this_monster(m_idx);
            s16b chance = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
            if (is_idx != -1) monster_inven_damage(m_idx, is_idx, set_fire_destroy, chance);
         }
         break;
      /* Cold */
      case GF_COLD:
      case GF_ICE:  /* I'm not sure players use ice or cold */
         if (!(r_ptr->flags3 & RF3_IM_COLD))
         {
            s16b is_idx = item_set_this_monster(m_idx);
            s16b chance = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;
            if (is_idx != -1) monster_inven_damage(m_idx, is_idx, set_cold_destroy, chance);
         }
         break;
      /* Poison */
      case GF_POIS:
         if (!(r_ptr->flags3 & RF3_IM_POIS))
         {
            s16b is_idx = item_set_this_monster(m_idx);
            if (is_idx != -1) monster_poison_dam(m_idx, is_idx, dam);
         }
         break;

      /* Disenchantment -- Breathers and Disenchanters resist */
      case GF_DISENCHANT:
         if (!(r_ptr->flags4 & RF4_BR_DISE))
         {
           s16b is_idx = item_set_this_monster(m_idx);
           if (is_idx != -1) monster_disenchant_dam(m_idx, is_idx, dam);
         }
         break;
   }
}

/* jk - this should exist, called when things disappear because of damage */
void monster_inven_increase(s16b m_idx, s16b is_idx, s16b item_in_index_set, s16b amt)
{
   object_type *i_ptr;
   s16b item;
   s16b number = items_in_set(is_idx);

   item = is_list[is_idx].index[item_in_index_set];
   i_ptr = &i_list[item];

   if (amt<0)
   {
      amt = -amt;
      if (i_ptr->number>amt)
      {
         i_ptr->number -=amt;
      }
      else
      {
         i_ptr->k_idx = 0; /* dead object */
         if (item_in_index_set==0) /* last item -> dead index_set? */
         {
            is_list[is_idx].inuse = FALSE;
            is_list[is_idx].x = 0; /* these are probably paranoia */
            is_list[is_idx].y = 0;
            mn_list[m_idx].has_drop = FALSE; /* the monster doesn't have zip */
         }
         else if (item_in_index_set<(number-1)) /* do we need to shift the index down */
         {
            s16b j;
            for (j=item_in_index_set;j<(number-1);j++)
            {
              is_list[is_idx].index[j]=is_list[is_idx].index[j+1];
            }
         }
      }
   }
   else
   {
      i_ptr->number +=amt;
      if (i_ptr->number>99) i_ptr->number = 99;
   }
}

s16b monster_inven_absorb(s16b m_idx, object_type *i_ptr)
{
   s16b number, i, is_idx;

   is_idx = item_set_this_monster(m_idx);
   number = items_in_set(is_idx);
   for (i=0;i<number;i++)
   {
      object_type *j_ptr = &i_list[is_list[is_idx].index[i]];
      if (object_similar(i_ptr,j_ptr))
      {
         return(i);
      }
   }
   return(-1);
}

s16b monster_inven_carry(s16b m_idx, object_type *i_ptr)
{
   s16b is_idx = 0;
   s16b number, fx, fy;

   if (mn_list[m_idx].has_drop)
   {
      /* can we absorb this to a similar item in the monster's inventory? */
      number=monster_inven_absorb(m_idx, i_ptr);

      if (number!=-1)
      {
         /* yes we can! */
         monster_inven_increase(m_idx,is_idx,number,i_ptr->number);
         return (number);
      }
      is_idx = item_set_this_monster(m_idx);
      number = items_in_set(is_idx);
   }
   else
   {
     is_idx = is_pop(); /* this functions doesn't fail :-) */
                        /* it just aborts the program!     */
     is_list[is_idx].x = -1;
     is_list[is_idx].y = m_idx;
     mn_list[m_idx].has_drop = TRUE;
     number = 0;
   }

   /* log this item */
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;
   i_ptr->log.whose = mn_list[m_idx].r_idx;
   fx = mn_list[m_idx].fx;
   fy = mn_list[m_idx].fy;
   i_ptr->log.where = OBJ_FOUND_MONSTER;
   if (dungeon.level[sublevel][fy][fx].fdat & CAVE_ROOM)
   {
      i_ptr->log.where |= OBJ_FOUND_ROOM;
   }
   else if (dungeon.level[sublevel][fy][fx].fdat & CAVE_VAULT)
   {
      i_ptr->log.where |= OBJ_FOUND_VAULT;
   }
   else if (dungeon.level[sublevel][fy][fx].fdat & CAVE_MAZE)
   {
      i_ptr->log.where |= OBJ_FOUND_MAZE;
   }
   else if (dungeon.level[sublevel][fy][fx].fdat & CAVE_PIT)
   {
      i_ptr->log.where |= OBJ_FOUND_PIT;
   }
   else if (dungeon.level[sublevel][fy][fx].fdat & CAVE_NEST)
   {
      i_ptr->log.where |= OBJ_FOUND_NEST;
   }
   else
   {
      i_ptr->log.where |= OBJ_FOUND_TUNNEL;
   }

   if (number<ITEM_SET_SIZE)
   {
      s16b i_idx = i_pop();                   /* get a free index    */

      /* in 66% of the cases, the player forgets this object.... */
      if (!artifact_p(i_ptr) && (randint(3)!=3))
      {
         forget_item(i_ptr);
      }

      i_list[i_idx] = (*i_ptr);               /* copy the item       */
      i_list[i_idx].ix = -1;                  /* it's not on the     */
      i_list[i_idx].iy = m_idx;               /* ground anymore      */
      is_list[is_idx].index[number] = i_idx;  /* point the set at it */
      is_list[is_idx].x = -1;                 /* simpler to do than  */
      is_list[is_idx].y = m_idx;              /* to test for number  */
      return (number);
   }
 
   /* something went wrong */
   return (-1);
}

/* jk - since they don't need to be sorted, it's not so difficult */
void monster_inven_optimize(s16b m_idx, s16b is_idx)
{
   s16b number = items_in_set(is_idx);
   s16b i,j;

   if (number<=1) return; /* nothing to optimize */
   for (i = number-1; i > 0; i--)
   {
      /* Get the item */
      object_type *i_ptr = &i_list[is_list[is_idx].index[i]];

      /* Scan the items above that item */
      for (j = 0; j < i; j++)
      {
         /* Get the item */
         object_type *j_ptr = &i_list[is_list[is_idx].index[j]];
             /* Can we drop "i_ptr" onto "j_ptr"? */
         if (object_similar(j_ptr, i_ptr))
         {
            monster_inven_increase(m_idx,is_idx,j,i_ptr->number);
            /* now kill the old object */
            i_list[is_list[is_idx].index[j]].k_idx = 0;
            i_list[is_list[is_idx].index[j]].ix = 0;
            i_list[is_list[is_idx].index[j]].iy = 0;
            /* and kill the entry in the item_set */
            is_list[is_idx].index[j] = 0;
            break;
         }
      }
   }
}

/* jk */
s16b item_set_this_monster(s16b m_idx)
{
   s16b j;
   for (j=0;j<is_max;j++)
   {
      if (!is_list[j].inuse) continue;
      if ((is_list[j].x == -1) && (is_list[j].y == m_idx))
      {
         break;
      }
   }
   if (j==is_max) return (-1);
   return (j);
}

/* jk */
s16b items_in_set(s16b is_idx)
{
   s16b j, number = 0;
   for (j=0;j<ITEM_SET_SIZE;j++)
   {
      if (is_list[is_idx].index[j]!=0) number++;
   }
   return (number);
}

bool correct_item(s16b r_idx, object_type *i_ptr)
{
   u64b f1, f2, f3;
   u32b flg3=0L;

   monster_race *r_ptr = &r_info[r_idx];

   object_flags(i_ptr, &f1, &f2, &f3);

   if (r_ptr->flags2 & RF2_SMART)
   {
      /* Ignore "worthless" items */
      if (object_value(i_ptr) <= 0) return (FALSE);
   }

   /* monsters don't pick up their own corpses! */
   if ((i_ptr->tval == TV_CORPSE) && (i_ptr->sval == r_idx))
   {
      return FALSE;
   }

   /* priests shouldn't pick up unpriestly weapons */
   if ((i_ptr->tval == TV_SWORD) || (i_ptr->tval == TV_POLEARM))
   {
      if (cmp_strngs(r_name + r_info[r_idx].name, "priest"))
      {
         return (FALSE);
      }
   }

   if (f1 & TR1_KILL_DRAGON) flg3 |= RF3_DRAGON;
   if (f1 & TR1_SLAY_DRAGON) flg3 |= RF3_DRAGON;
   if (f1 & TR1_SLAY_TROLL) flg3 |= RF3_TROLL;
   if (f1 & TR1_SLAY_GIANT) flg3 |= RF3_GIANT;
   if (f1 & TR1_SLAY_ORC) flg3 |= RF3_ORC;
   if (f1 & TR1_SLAY_DEMON) flg3 |= RF3_DEMON;
   if (f1 & TR1_SLAY_UNDEAD) flg3 |= RF3_UNDEAD;
   if (f1 & TR1_SLAY_ANIMAL) flg3 |= RF3_ANIMAL;
   if (f1 & TR1_SLAY_EVIL) flg3 |= RF3_EVIL;

/* non-smart monsters don't carry much spells */
   if ((i_ptr->tval == TV_SPELL) && (!(f2 & RF2_SMART)) && (randint(5)<5))
   {
      return (FALSE);
   }

   /* The object cannot be picked up by the monster */
   return ((r_ptr->flags3 & flg3)==0);
}

/*
 * make some smart guesses about what a monster should be carrying
 */
static bool monster_inventory_first_item_tval(s16b r_idx, s16b tv, s16b sv)
{
   monster_race *r_ptr = &r_info[r_idx];
   if (r_ptr->flags4 & (RF4_ARROW_1 | RF4_ARROW_2))
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_SHORT_BOW));
      }
      else
      {
         return (tv == TV_ARROW) && ((sv == SV_ARROW_LIGHT));
      }
   }
   if (r_ptr->flags4 & (RF4_ARROW_3 | RF4_ARROW_4))
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_LONG_BOW));
      }
      else
      {
         return (tv == TV_ARROW) && ((sv == SV_ARROW_NORMAL));
      }
   }
   if (r_ptr->flags4 & RF4_ARROW_5)
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_ELVEN_BOW));
      }
      else
      {
         return (tv == TV_ARROW) && ((sv == SV_ARROW_HEAVY));
      }
   }
   if (r_ptr->flags4 & (RF4_BOLT_1 | RF4_BOLT_2))
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_LIGHT_XBOW));
      }
      else
      {
         return (tv == TV_BOLT) && ((sv == SV_BOLT_LIGHT));
      }
   }
   if (r_ptr->flags4 & (RF4_BOLT_3 | RF4_BOLT_4))
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_LIGHT_XBOW));
      }
      else
      {
         return (tv == TV_BOLT) && ((sv == SV_BOLT_NORMAL));
      }
   }
   if (r_ptr->flags4 & RF4_BOLT_5)
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_HEAVY_XBOW));
      }
      else
      {
         return (tv == TV_BOLT) && ((sv == SV_BOLT_HEAVY));
      }
   }
   if (r_ptr->flags4 & (RF4_SHOT_1 | RF4_SHOT_2))
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_SLING));
      }
      else
      {
         return (tv == TV_SHOT) && ((sv == SV_SHOT_LIGHT));
      }
   }
   if (r_ptr->flags4 & (RF4_SHOT_3 | RF4_SHOT_4))
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_SLING));
      }
      else
      {
         return (tv == TV_SHOT) && ((sv == SV_SHOT_NORMAL));
      }
   }
   if (r_ptr->flags4 & RF4_SHOT_5)
   {
      if (randint(10) == 1)
      {
         return ((tv == TV_BOW) && (sv == SV_HOBBIT_SLING));
      }
      else
      {
         return (tv == TV_SHOT) && ((sv == SV_SHOT_HEAVY));
      }
   }
   if (r_ptr->flags3 & RF3_WYRM)
   {
      return TV_DRAG_ARMOR;
   }
   if ((r_ptr->d_char == 'p') && (r_ptr->freq_spell > 0))
   {
      return TV_SPELL;
   }
   return -1;
}   

void create_monster_inventory(s16b m_idx, s16b drop)
{
   s16b                 j, x, y,is_idx;

   s16b                 number = 0;

   monster_type        *m_ptr = &mn_list[m_idx];

   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   bool good = (r_ptr->flags1 & RF1_DROP_GOOD) ? TRUE : FALSE;
   bool great = (r_ptr->flags1 & RF1_DROP_GREAT) ? TRUE : FALSE;

   bool do_gold = (!(r_ptr->flags1 & RF1_ONLY_ITEM));
   bool do_item = (!(r_ptr->flags1 & RF1_ONLY_GOLD));

   s16b force_coin = get_coin_type(r_ptr);

   bool did_artifact = FALSE;
   /* Get the location */
   y = m_ptr->fy;
   x = m_ptr->fx;

   /* Determine how much we can drop */
   if ((r_ptr->flags1 & RF1_DROP_60) && (rand_int(100) < 60)) number++;
   if ((r_ptr->flags1 & RF1_DROP_90) && (rand_int(100) < 90)) number++;
   if (r_ptr->flags1 & RF1_DROP_1D2) number += damroll(1, 2);
   if (r_ptr->flags1 & RF1_DROP_2D2) number += damroll(2, 2);
   if (r_ptr->flags1 & RF1_DROP_3D2) number += damroll(3, 2);
   if (r_ptr->flags1 & RF1_DROP_4D2) number += damroll(4, 2);

   if (drop) number++; /* if we are in a vault, we need one extra good item */

/* this is truly a nasty hack, but it overwrote the next is_list.inuse */
/* which led to is_pop failing. 14 treasure + grond+crown should be    */
/* enough for Morgoth, BTW                                             */
/* 2 + 4 + 6 + 8 = 20 = too much.                                      */

   if (r_ptr->flags1 & RF1_DROP_CHOSEN)
   {
      number = ITEM_SET_SIZE-2;
dlog(DEBUGITEMS,"monster3.c: create_monster_inventory: %d items for Morgoth.\n", number);
   }

   /* paranoia */
   if (number>ITEM_SET_SIZE) number = ITEM_SET_SIZE;

   if (number==0)
   {
     m_ptr->has_drop = FALSE;
     return;
   }

   /* create an item_set */
   is_idx = is_pop();

   if (is_idx==-1)
   {
      m_ptr->has_drop = FALSE;
      return;
   }
   is_list[is_idx].x = -1;
   is_list[is_idx].y = m_idx;

   /* Add some objects to the item_set */
   coin_type = force_coin;
   /* Average dungeon and monster levels */
   object_level = (p_ptr->mdepth+ r_ptr->level) / 2;
dlog(DEBUGITEMS,"monster3.c: create_monster_inventory: m_idx %d is_idx %d number %d drop %d good %d great %d\n",
     m_idx,is_idx,number,drop, good, great);

   for (j = 0; j < number; j++)
   {
      s16b i_idx = i_pop();
      object_type *i_ptr;
      if (!i_idx)
         quit("i_pop() failed in create_monster_inventory");

      i_ptr = &i_list[i_idx];
      if (drop)
      {
         s16b old_obj_level = object_level;
         s16b first_tries = 0;
         object_level = drop;
         create_item(i_ptr, good, great, FALSE);
dlog(DEBUGITEMS,"monster3.c: create_monster_inventory: created drp1 item i_idx %d k_idx %d tv %d sv %d pv %ld\n",i_idx,
         i_ptr->k_idx,i_ptr->tval,i_ptr->sval,i_ptr->p1val);
         /* if it just isn't correct or if we haven't tried for 100 times to get the correct first item */
         /* try it again                                                                                */
         while ( (!correct_item(m_ptr->r_idx, i_ptr)) ||
                 ( (j == 0) && (first_tries++ < 100) && 
                   (!monster_inventory_first_item_tval(m_ptr->r_idx, i_ptr->tval, i_ptr->sval)) ) )
         {
            if (i_ptr->name1)
            {
               a_info[i_ptr->name1].cur_num = 0;
            }

            create_item(i_ptr,good,great, FALSE);
dlog(DEBUGITEMS,"monster3.c: create_monster_inventory: created drp2 item i_idx %d k_idx %d tv %d sv %d pv %ld\n",i_idx,
                i_ptr->k_idx,i_ptr->tval,i_ptr->sval,i_ptr->p1val);
         }
         object_level = old_obj_level;
         drop = 0; /* we have done the one great item */
      }
      /* Place Gold */
      else if (do_gold && (!do_item || (rand_int(100) < 50)))
      {
         create_gold_item(i_ptr);
dlog(DEBUGITEMS,"monster3.c: create_monster_inventory: created gold item i_idx %d k_idx %d tv %d sv %d pv %ld\n",i_idx,
      i_ptr->k_idx,i_ptr->tval,i_ptr->sval,i_ptr->p1val);
      }

      /* Place Object */
      else
      {
         create_item(i_ptr,good,great, FALSE);

         while (!correct_item(m_ptr->r_idx,i_ptr))
         {
            if (i_ptr->name1)
            {
               a_info[i_ptr->name1].cur_num = 0;
            }
           create_item(i_ptr,good,great, FALSE);
dlog(DEBUGITEMS,"Creating norm item i_idx %d k_idx %d tv %d sv %d pv %ld\n",i_idx,
      i_ptr->k_idx,i_ptr->tval,i_ptr->sval,i_ptr->p1val);
         }
      }
      i_ptr->ix = -1;
      i_ptr->iy = m_idx;
      is_list[is_idx].index[j] = i_idx;
      if (artifact_p(i_ptr))
      {
         did_artifact = TRUE;
      }
      i_ptr->log.mlevel = p_ptr->mdepth;
      i_ptr->log.slevel = sublevel;
      i_ptr->log.whose = mn_list[m_idx].r_idx;
      i_ptr->log.where = OBJ_FOUND_MONSTER;
      if (dungeon.level[sublevel][y][x].fdat & CAVE_ROOM)
      {
         i_ptr->log.where |= OBJ_FOUND_ROOM;
      }
      else if (dungeon.level[sublevel][y][x].fdat & CAVE_VAULT)
      {
         i_ptr->log.where |= OBJ_FOUND_VAULT;
      }
      else if (dungeon.level[sublevel][y][x].fdat & CAVE_MAZE)
      {
         i_ptr->log.where |= OBJ_FOUND_MAZE;
      }
      else if (dungeon.level[sublevel][y][x].fdat & CAVE_PIT)
      {
         i_ptr->log.where |= OBJ_FOUND_PIT;
      }
      else if (dungeon.level[sublevel][y][x].fdat & CAVE_NEST)
      {
         i_ptr->log.where |= OBJ_FOUND_NEST;
      }
      else
      {
         i_ptr->log.where |= OBJ_FOUND_TUNNEL;
      }
   }
   if (did_artifact && !monsters_with_artifacts)
   {
      char m_name[80];
      monster_desc(m_name, m_ptr, 0x88);
      monsters_with_artifacts=TRUE;
      if (rand_int(100)<90)
      {
         msg_print("You feel there's an important monster on this level.");
      }
      else
      {
         msg_format("You suddenly feel like hunting for %s!",m_name);
      }
   }

   /* Reset the object level */
   object_level = p_ptr->mdepth;

   /* Reset "coin" type */
   coin_type = 0;

   if (items_in_set(is_idx)>0)
   {
      m_ptr->has_drop = TRUE;
   }

   /* Mega-Hack -- drop "winner" treasures */
   if (r_ptr->flags1 & RF1_DROP_CHOSEN)
   {
      /* Hack -- an "object holder" */
      s16b i_idx = i_pop();

      if (i_idx)
      {
         object_type *i_ptr = &i_list[i_idx];
         invcopy(i_ptr,lookup_kind(TV_HAFTED, SV_GROND));
         i_ptr->name1 = ART_GROND;
         /* Mega-Hack -- Actually create "Grond" */
         apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);
         j=monster_inven_carry(m_idx, i_ptr);
         if (j == -1)
         {
            dlog(DEBUGALWAYS,"monster3.c: create_monster_inventory: creating Grond for Morgoth failed!\n");
         }
      }
      else
      {
         quit("i_pop() failed when creating Grond for Morgoth");
      }       

      i_idx = i_pop();
      if (i_idx)
      {
         object_type *i_ptr = &i_list[i_idx];
         invcopy(i_ptr,lookup_kind(TV_CROWN, SV_MORGOTH));
         i_ptr->name1 = ART_MORGOTH;
         /* Mega-Hack -- Actually create "Crown of Morgoth" */
         apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);
         j=monster_inven_carry(m_idx, i_ptr);
         if (j == -1)
         {
            dlog(DEBUGALWAYS,"monster3.c: create_monster_inventory: creating the Crown for Morgoth failed!\n");
         }
      }
      else
      {
         quit("i_pop() failed when creating Crown for Morgoth");
      }
   }

   if (items_in_set(is_idx)>0)
   {
      m_ptr->has_drop = TRUE;
   }
}

void create_ghost_inventory(s16b m_idx)
{
   s16b                j, x, y,is_idx;
   s16b                number = 0, ghost_idx;
   monster_type        *m_ptr = &mn_list[m_idx];

   /* Get the location */
   y = m_ptr->fy;
   x = m_ptr->fx;

   ghost_idx = m_ptr->r_idx - r_number;
dlog(DEBUGGHOST,"monster3.c: create_ghost_inventory: ghost_idx %d\n",
                ghost_idx);

   number = 0;
   for (j=0; j<ITEM_SET_SIZE; j++)
   {
      if (ghost_info[ghost_idx].inv[j].k_idx)
         number++;
      else
         break;
   }
dlog(DEBUGGHOST,"monster3.c: create_ghost_inventory: number %d\n",
                number);

   /* paranoia */
   if (number>ITEM_SET_SIZE)
   {
      number = ITEM_SET_SIZE;
   }
   else if (number==0)
   {
      m_ptr->has_drop = FALSE;
      return;
   }

   /* create an item_set */
   is_idx = is_pop();

   if (is_idx==-1)
   {
      /* exit gracefully */
      m_ptr->has_drop = FALSE;
      return;
   }
dlog(DEBUGGHOST,"monster3.c: create_ghost_inventory: is_idx %d\n",
                is_idx);
   is_list[is_idx].x = -1;
   is_list[is_idx].y = m_idx;
   for (j = 0; j < number; j++)
   {
       s16b i_idx = i_pop();
       object_type *i_ptr;

       if (!i_idx)
          quit("i_pop() failed in create_monster_inventory");
       i_ptr = &i_list[i_idx];

       *i_ptr = ghost_info[ghost_idx].inv[j];
       i_ptr->ix = -1;
       i_ptr->iy = m_idx;
       is_list[is_idx].index[j] = i_idx;
   }

   if (items_in_set(is_idx)>0)
   {
      m_ptr->has_drop = TRUE;
dlog(DEBUGGHOST,"monster3.c: create_ghost_inventory: %d items in set\n",
                items_in_set(is_idx));
   }
}

s16b which_item_in_set(s16b is_idx, s16b i_list_idx)
{
   s16b i;
   s16b num = items_in_set(is_idx);
   for (i=0; i<num; i++)
   {
      if (is_list[is_idx].index[i]==i_list_idx) return (i);
   }
   return (-1);
}

/*
 * Display the monster inventory in is_idx.
 */
void show_monster_inventory(s16b is_idx)
{
   s16b                i, l;
   s16b                col, len;
   s16b                num = items_in_set(is_idx);
   object_type        *i_ptr;
   char                i_name[80];
   byte                out_color[23];
   char                out_desc[23][80];

   /* Maximal length */
   len = 0;

   /* Scan the monster inventory */
   for (i = 0; i < num; i++)
   {
      i_ptr = &i_list[is_list[is_idx].index[i]];

      /* Description */
      object_desc(i_name, i_ptr, TRUE, 0);

      /* Save the color */
      out_color[i] = tval_to_attr[i_ptr->tval % 128];
      (void)strcpy(out_desc[i], i_name);

      /* Extract the maximal length (see below) */
      l = strlen(out_desc[i]);

      /* Maintain the max-length */
      if (l > len) len = l;
   }

   /* Hack -- Find a column to start in */
   col = (len > 76) ? 0 : (79 - len);

   /* Output each entry */
   for (i = 0; i < num; i++)
   {
      /* Get the index */
      i_ptr = &i_list[is_list[is_idx].index[i]];

      /* Clear the line */
      prt("", col ? col-2 : col, i + 2);

      /* Display the entry itself */
      c_put_str(out_color[i], out_desc[i], col, i + 2);
   }

   /* Make a "shadow" below the list (only if needed) */
   if ((num>0) && (num < 21)) prt("", col ? col - 2 : col, num + 3);
}
