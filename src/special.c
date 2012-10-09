/* File: special.c */

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

/* jk - this doesn't require los */
static bool scatter_unseen(s16b *xp, s16b *yp, s16b x, s16b y, s16b d)
{
   s16b nx, ny;
   u32b iteration = 0;

   /* Pick a location */
   while (TRUE)
   {
      iteration++;
      /* don't go on too far with trying */
      if (iteration==100) return (FALSE);

      /* Pick a new location */
      nx = rand_spread(x, d);
      ny = rand_spread(y, d);
      /* Ignore illegal locations and outer walls */
      if (!in_bounds(nx, ny)) continue;
      if (!dungeon.level[sublevel][ny][nx].fdat & CAVE_AREN) continue;
      if (!empty_grid_bold(nx, ny)) continue;  /* Require empty grid */

      /* Ignore "excessively distant" locations */
      if ((d > 1) && (distance(x, y, nx, ny) > d)) continue;

      break;
   }

   /* Save the location */
   (*xp) = nx;
   (*yp) = ny;
   return (TRUE);
}

static void arena_insert_monsters(s32b monster_amount, s32b monster_level)
{
   cptr name;
   int  index1[MAX_R_IDX],index2[MAX_R_IDX];
   s16b i, x, y, r_idx, num1, num2;
   s16b min1, max1, min2, max2;
   s16b nr_summoned = 0;
   bool result = FALSE, cont = TRUE;
   bool first_summoner = TRUE, first_breeder = TRUE;
   s32b amount = monster_amount;
dlog(DEBUGARENA,"special.c: arena_insert_monsters: monster_amount %ld monster_level %ld amount %ld\n",
       monster_amount, monster_level, amount);

   /* amount        - how many monster_level_points should be summoned    */
   /* arena_monster_level - how high level should the monsters be               */
   /* amount 200 arena_monster_level 10 -> 20 level 10 monsters should be ideal */
   /* amount 20 arena_monster_level 30 -> 1 level 30 monster should be ideal    */
   /* amount 20 arena_monster_level 8  -> 2 or 3 level 8 monsters etc.          */
dlog(DEBUGARENA,"special.c: arena_insert_monsters starting with amount %ld\n",amount);

   switch (rand_int(10))
   {
      case 1: case 2: case 3: case 4: case 5:
         msg_print("The cheers dim somewhat.");
         arena_monster_level = monster_level;
         /* go for the monster one at a time */
         break;
      case 6: case 7: case 8:
         msg_print("The crowd sounds enthousiastic.");
         /* try for 2 amount*/
         amount = amount*1.5;
         arena_monster_level = monster_level;
         break;
      case 9:
         msg_print("An ominous silence falls.");
         /* try for 3 monsters, but harder ones */
         amount= amount * 3;
         arena_monster_level = (monster_level * 3) / 2;
         break;
      case 10:
         msg_print("You see a lot of the crowd leave in fear!");
         /* try for 4 monsters */
         amount = amount * 4;
         arena_monster_level = monster_level * 2;
         break;
   }
dlog(DEBUGARENA,"special.c: arena_insert_monsters: after crowd amount %ld arena_monster_level %ld\n",
           amount, arena_monster_level);
   /* don't get the wimpy town amount*/
   if (arena_monster_level==0) arena_monster_level=1;
   /* monsters over level 100 are replaced with double numbers of lesser monsters */
   while (arena_monster_level>100)
   {
      arena_monster_level = arena_monster_level/2; /* leave 50% of the arena_monster_level */
      amount = amount*2;
   }
dlog(DEBUGARENA,"special.c: arena_insert_monsters arena_monster_level now %ld amount %ld after\n",
           arena_monster_level, amount);

   /* now create the arrays with likely candidate-monsters */
   num1 = 0;
   num2 = 0;
   min1 = (arena_monster_level*2)/3 + 3;
   min2 = 0;
   max1 = (arena_monster_level*5)/4;
   max2 = arena_monster_level*2 + 6;
   cont = TRUE;
   while (cont)
   {
dlog(DEBUGARENA,"special.c: arena_insert_monsters: level1 min %03d max %03d    level2 min %03d max %03d\n", min1, max1, min2, max2);
      for (i=0; i<r_number; i++)
      {
         /* don't select uniques */
         if (r_info[i].flags1 & RF1_UNIQUE) continue;
         /* select monsters between 66% of arena_monster_level and 125% */
         if ( (r_info[i].level>min1) &&
              (r_info[i].level<max1) )
         {
            name = (r_name + r_info[i].name);
            index1[num1++]=i;
            index2[num2++]=i;
         }
         if ( (r_info[i].level>min2) &&
              (r_info[i].level<max2) )
         {
            name = (r_name + r_info[i].name);
            index2[num2++]=i;
         }
      }
dlog(DEBUGARENA,"special.c: insert_arena_monsters %03d in index1 %03d in index2\n",
                num1, num2);
      /* did we find any monsters? */
      if ((num1>0) && (num2>0))
      {
         cont = FALSE;
      }
      /* if not, select tougher monsters */
      else
      {
         max1+=2;
         max2+=2;
      }
   }
   if (amount==0)
   {
      amount = randint(p_ptr->lev); /* since we bet very low, do something */
      msg_print("You see the arena-master frown deeply.");
   }
   while (amount>0)
   {
dlog(DEBUGARENA,"special.c: insert_arena_monsters amount now %ld\n", amount);
      /* Try to place it */
      for (i = 0; i < 40; i++)
      {
         s16b d = (i / 15) + 3;               /* Pick a distance */
         x=px+20; y=py;

         if (!scatter_unseen(&x, &y, px + 10, py, d))    /* location */
         {
            continue;
         }
         if ((i<38) && (num1>0))
         {
            r_idx = index1[rand_int(num1)]; /* Pick a monster */
dlog(DEBUGARENA,"special.c: arena_insert_monsters: picked r_idx %d from array1\n", r_idx);
         }
         else
         {
            r_idx = index2[rand_int(num2)]; /* Pick a monster */
dlog(DEBUGARENA,"special.c: arena_insert_monsters: picked r_idx %d from array2\n", r_idx);
         }
         name = (r_name + r_info[r_idx].name);
dlog(DEBUGARENA,"special.c: arena_insert_monsters %s level %d arena_monster_level %ld at i %d\n",
           name, r_info[r_idx].level,arena_monster_level,i);

         /* Attempt to place the monster (awake, allow groups) */
         result = place_monster_aux(x, y, r_idx, FALSE, TRUE, 0, 0);

         if (result)
         {
            amount-= (r_info[r_idx].level+1);
            /* summoning monsters count double */
            if (r_info[r_idx].flags6 & RF6_SUMMON_MASK)
            {
               amount -= (r_info[r_idx].level+1);
               if (first_summoner) msg_print("Be sure to defeat ALL opponents!");
               first_summoner = FALSE;
            }
            /* breeding monsters count double */
            else if (r_info[r_idx].flags6 & RF2_MULTIPLY)
            {
               amount -= (r_info[r_idx].level+1);
               if (first_breeder) msg_print("You fear a long, tedious fight!");
               first_breeder = FALSE;
            }
            else if (r_info[r_idx].flags1 & RF1_NEVER_MOVE)
            {
               /* a stationary monster that doesn't summon doesn't count */
               /* so the amount doesn't decrease with the minimum amount. */
               amount--;
               msg_print("You hear a lot of Boo sounds from the crowd.");
            }
            else
            {
               switch(randint(5))
               {
                  case 1:
                     msg_format("You hear the crowds welcome%s opponent!",
                                (nr_summoned>0)?" another":" an");
                     break;
                  case 2:
                     msg_format("The crowds cheer as%s opponent enters!",
                                (nr_summoned>0)?" another":" an");
                     break;
                  case 3:
                     msg_format("The arena master introduces%s opponent!",
                                (nr_summoned>0)?" another":" an");
                     break;
                  case 4:
                     msg_format("You hear the crowds hush as%s opponent enters the arena!",
                                (nr_summoned>0)?" another":" an");
                     break;
                  case 5:
                     msg_format("You hear lots of excitement as%s opponent comes into the arena!",
                                (nr_summoned>0)?" another":" an");
                     break;
               }
               nr_summoned++;
            }
         }
         else
            /* if we had 40 tries, perhaps something tougher */
            arena_monster_level+=5;

         /* Done */
         break;
      } /* 40 tries */
   } /* monsters > 0 */
}

/* if you use the cheating option to live on when killed, there should be */
/* a way to remove all monsters in the arena */
void kill_arena_monsters(void)
{
/* jk - note that due to the calling sequence, the dying monster is always */
/* counted here, so only >1 means some still exist */
   s16b       x1,y1;
   cave_cell_type *c_ptr;

dlog(DEBUGARENA,"kill_arena_monsters: step 00\n px,py %d,%d fdat & CAVE_AREN %04lx\n",
                px,py,dungeon.level[sublevel][py][px].fdat & CAVE_AREN);
   /* we test from 70,8 to 86, 14 */
   for (x1=SCREEN_WID+4; x1<SCREEN_WID+21; x1++)
   {
      for (y1=SCREEN_HGT-14; y1<SCREEN_HGT-7; y1++)
      {
         monster_type *m_ptr;
         c_ptr = &dungeon.level[sublevel][y1][x1];
         if (! (c_ptr->fdat & CAVE_AREN)) continue;
         if ((x1==px) && (y1==py)) continue;
         m_ptr = &mn_list[c_ptr->m_idx];
         if (c_ptr->m_idx>0)
         {
            if (m_ptr->hp>0) delete_monster_idx(c_ptr->m_idx);
         }
      }
   }
}

bool arena_monsters_left(void)
{
/* jk - note that due to the calling sequence, the dying monster is always */
/* counted here, so only >1 means some still exist */
   s16b       x1,y1;
   s16b       anyleft = 0;
   cave_cell_type *c_ptr;

dlog(DEBUGARENA,"arena_monsters_left: step 00 px,py %d,%d fdat & CAVE_AREN %04lx\n",
                px,py,dungeon.level[sublevel][py][px].fdat & CAVE_AREN);
   /* we test from 70,8 to 86, 14 */
   for (x1=SCREEN_WID+4; x1<SCREEN_WID+21; x1++)
   {
      for (y1=SCREEN_HGT-14; y1<SCREEN_HGT-6; y1++)
      {
         c_ptr = &dungeon.level[sublevel][y1][x1];
         if (! (c_ptr->fdat & CAVE_AREN)) continue;
         if ((x1==px) && (y1==py)) continue;
         if (c_ptr->m_idx>0)
         {
             monster_type *m_ptr = &mn_list[c_ptr->m_idx];
             if (m_ptr->hp>0)
             {
                anyleft++;
dlog(DEBUGARENA,"arena_monsters_left: found %d at %d,%d\n",anyleft,x1,y1);
             }
         }
         if (anyleft>0)
         {
dlog(DEBUGARENA,"arena_monsters_left: found at %d,%d\n",x1,y1);
            break;
         }
      }
      if (anyleft>0) break;
   }
   return (anyleft>0);
}

/* jk - anything left behind will fatten the arena-master :-) */
static void arena_handle_items(void)
{
   s16b        x,y;
   s16b        objs, j;
   s16b        found = 0;
   s32b        val;
   object_type *i_ptr;
   object_kind *k_ptr;

   /* we test from 70,8 to 86, 14 */
   for (x=SCREEN_WID+4; x<SCREEN_WID+21; x++)
   {
      for (y=SCREEN_HGT-14; y<SCREEN_HGT-6; y++)
      {
         objs = objects_on_floor(x,y);
         if (!objs) continue;
         for (j = objs-1;j>=0;j--)
         {
            i_ptr = get_item_pointer_floor_xy(j,x,y);
            k_ptr = &k_info[i_ptr->k_idx];
            val = k_ptr->cost;
            /* make sure we mention something the first time, but */
            /* not every time if there's forty items left */
            if (randint(found+1)==1)
            {
               char i_name[80];
               object_desc(i_name, i_ptr, TRUE, 3);
               msg_format("The arena master picks up %s.", i_name);
               switch(randint(5))
               {
                  case 1: msg_print("Your hear the arena master murmur.");
                          break;
                  case 2: msg_print("Your hear the arena master say something.");
                          break;
                  case 3: msg_print("Your hear the arena master shout at you as you leave.");
                          break;
                  case 4: msg_print("Your hear the arena master utter something.");
                          break;
                  case 5: msg_print("Your hear the arena master make a comment.");
                          break;
               }

               if (val<(arena_reward<10))
                  msg_print(arena_leave_items[1][rand_int(4)]);
               else if (val<(arena_reward/4))
                  msg_print(arena_leave_items[2][rand_int(4)]);
               else if (val<(arena_reward/2))
                  msg_print(arena_leave_items[3][rand_int(4)]);
               else if (val<arena_reward)
                  msg_print(arena_leave_items[4][rand_int(4)]);
               else if (val<(arena_reward*2))
                  msg_print(arena_leave_items[5][rand_int(4)]);
               else
                  msg_print(arena_leave_items[6][rand_int(4)]);
            }
            found++;
            delete_object(x,y,j);
         }
      }
   }
   if (found==0)
   {
      switch(randint(5))
      {
         case 1: msg_print("Your hear the arena master murmur.");
                 break;
         case 2: msg_print("Your hear the arena master say something.");
                 break;
         case 3: msg_print("Your hear the arena master shout at you as you leave.");
                 break;
         case 4: msg_print("Your hear the arena master utter something.");
                 break;
         case 5: msg_print("Your hear the arena master make a comment.");
                 break;
      }
      msg_print(arena_leave_items[0][rand_int(4)]);
   }
}

/*
 * this procedure is called on entering the arena
 */
static void enter_arena(s32b standard)
{

   msg_print("You enter the arena!");
   msg_print(NULL);

   arena_reward = get_quantity("How much money do you want to bet on your survival:",
                               99999999,standard);
dlog(DEBUGARENA,"special.c: handle_arena: arena_visit_level[%d]=%d\n",
   p_ptr->lev, arena_visit_level[p_ptr->lev]);
   if ( (arena_visit_level[p_ptr->lev]>3) && (arena_reward>0) )
   {
      msg_print("The arena master says betting doesn't justify this amount.");
      arena_reward = arena_reward / (arena_visit_level[p_ptr->lev]/2);
      if (arena_reward>0)
      {
         arena_reward = (get_check(format("Do you agree to %d gold pieces?", arena_reward))?arena_reward:-1);
      }
      else
      {
         msg_print("It seems the public doesn't want to see you at this time.");
         arena_reward = -1;
      }
dlog(DEBUGARENA,"special.c: handle_arena: arena_reward = %ld after entering\n",arena_reward);
   }
   /* if we like this visit : print a nice cynic message such as you */
   /* would expext from an arena-master */
   else if ( (arena_reward < (s32b)(standard/10) ) && (arena_reward > 0) )
   {
      msg_print("The arena-master laughs in your face.");
      arena_reward = -1;
   }
   else if (arena_reward < (s32b)(standard/5))
      msg_print(arena_welcome[0][rand_int(4)]);
   else if (arena_reward < standard)
      msg_print(arena_welcome[1][rand_int(4)]);
   else if (arena_reward == standard)
      msg_print(arena_welcome[2][rand_int(4)]);
   else if (arena_reward < (s32b)(standard*1.5))
      msg_print(arena_welcome[3][rand_int(4)]);
   else if (arena_reward < (s32b)(standard*4))
      msg_print(arena_welcome[4][rand_int(4)]);
   else if (arena_reward < (s32b)(standard*8))
      msg_print(arena_welcome[5][rand_int(4)]);
   else if (arena_reward>0)
   {
      msg_print(arena_welcome[6][rand_int(4)]);
      arena_reward = standard;
      msg_format("The arena master suggests %d gold pieces.", standard);
      arena_reward = (get_check("Do you agree?")?arena_reward:-1);
   }

   if (arena_reward>0)
   {
      arena_visit_level[p_ptr->lev]++;
      arena_monsters = ( 4 * p_ptr->lev *
                        (arena_reward / standard ) + 5);
dlog(DEBUGARENA,"special.c: handle_arena: amount %ld arena_monsters %ld\n",
                arena_reward, arena_monsters);
      set_arena_state(ARENA_ACCEPTED);
   }
   else
   {
      msg_print("The Arena-Master agrees to let you look around a bit.");
      set_arena_state(ARENA_LOOKING);
   }
}

/*
 * this function creates the monsters in the arena
 */
static void arena_entrance_last_square(s32b standard)
{
   /* p_ptr->level from 1 to 50 gives base level 4 to 200 */
   /* normal betting gives just that */
   /* heavy betting get's you more */

dlog(DEBUGARENA,"special.c: arena_entrance_last_square: we just entered with amount %ld left %d been_in_arena %d\n",
                arena_reward, arena_monsters_left(), been_in_arena);

   /* are we getting ready to fight? */
   if (p_ptr->arena_state == ARENA_ACCEPTED)
   {
      s16b nx, ny;

      /* no less monsters than the previous time */
      if (arena_monsters<arena_previous_monsters)
      {
         arena_monsters = arena_previous_monsters;
         msg_print("The public seems disappointed - you hear jeering and hissing.");
      }
      /* make sure next time we ask some more monsters */
      arena_previous_monsters = arena_monsters+
                                  (p_ptr->lev*(arena_reward/standard));

      msg_format("The arena-master announces: You bet %ld monster points",arena_monsters);
      been_in_arena = TRUE;

      nx = SCREEN_WID;
      ny = SCREEN_HGT/2;
      (void)set_grid_type(nx,ny, DUNG_PERWALL, DUNG_PERWALL_ARDOOR,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN | CAVE_VIEW);
      note_spot(nx, ny);
      lite_spot(nx, ny);

      arena_insert_monsters(arena_monsters, p_ptr->lev);
      msg_print("Now fight for your life.");
      set_arena_state(ARENA_FIGHTING);
   }
   /* or are we done fighting? */
   else if (p_ptr->arena_state == ARENA_FIGHTING)
   {
      if (arena_monsters_left() == 0)
      {
         set_arena_state(ARENA_VICTOR);
      }
      else
      {
         msg_print("The crowds roar!");
      }
   }
   /* or are we doing something else? */
   else if ( (p_ptr->arena_state == ARENA_LOOKING) ||
             (p_ptr->arena_state == ARENA_VICTOR) ||
             (p_ptr->arena_state == ARENA_REWARDED) )
   {
      /* do nothing */
      ;
   }
}

/*
 * this handles the special locations
 *
 * the entrance to the arena looks like this:
 *
 *  #####                                #####
 *  ..... which with door closed becomes .+...
 *  #####                                #####
 *
 * the first floor to the left is @ SCREEN_WID-1, SCREEN_HGT/2
 * so the door is @ SCREEN_WID, SCREEN_HGT/2
 * and the last square is @ SCREEN_WID+3, SCREEN_HGT/2
 */
static void handle_arena(s16b oldx, s16b oldy, s16b newx, s16b newy)
{
   s32b       standard; /* the amount of money proposed for betting */
   s32b level_factor[PY_MAX_LEVEL+1] =
                       {  0, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48,
                             50, 52, 54, 56, 58, 60, 62, 64, 66, 68,
                             70, 72, 74, 76, 78, 80, 82, 84, 86, 88,
                             90, 92, 94, 96, 98,100,100,100,100,100,
                            100,100,100,100,100,100,100,100,100,100};


   standard = p_ptr->lev * level_factor[p_ptr->lev] * 3;

   /* entrance */
   if ( (oldx==SCREEN_WID) && (oldy==SCREEN_HGT/2) &&
        (newx==SCREEN_WID+1) && (oldy==SCREEN_HGT/2))
   {
      enter_arena(standard);
   }
   else if ( (oldx==SCREEN_WID+1) && (oldy==SCREEN_HGT/2) &&
             (newx==SCREEN_WID) && (oldy==SCREEN_HGT/2))
   {
      if (p_ptr->arena_state == ARENA_ACCEPTED)
      {
         msg_print("The Arena-Master whispers 'coward' as you slink away.");
         set_arena_state(ARENA_NONE);
      }
   }

   /* one square beyond the entrance */
   else if ( (oldx==SCREEN_WID+1) && (oldy==SCREEN_HGT/2) &&
             (newx==SCREEN_WID+2) && (oldy==SCREEN_HGT/2))
   {
      if (p_ptr->arena_state == ARENA_ACCEPTED )
      {
         msg_print("Warning - if you go further, you can't return without killing!");
      }
      else if (p_ptr->arena_state == ARENA_FIGHTING)
      {
         msg_print("Go on - hit them hard!");
      }
      else if (p_ptr->arena_state == ARENA_VICTOR)
      {
          msg_print("You've won - there's no point in returning");
      }
      else if (p_ptr->arena_state == ARENA_REWARDED)
      {
          msg_print("You've won & you've been paid - see you next time!");
      }
      /* we may also be ARENA_LOOKING, but there's no extra comment for that */
   }
   /* we just entered */
   else if ( (newx==SCREEN_WID+3) && (oldy==SCREEN_HGT/2) )
   {
      arena_entrance_last_square(standard);
   }
   /* almost last square */
   else if ( (newx==SCREEN_WID+1) && (oldy==SCREEN_HGT/2))
   {
      if (p_ptr->arena_state == ARENA_VICTOR)
      {
         s16b nx, ny;

         msg_print("You leave as victor, and a rich one at that!");
         set_arena_state(ARENA_REWARDED);
         p_ptr->au += arena_reward;
         p_ptr->redraw1 |= (PR1_GOLD);   /* Redraw gold */
         arena_reward = -1;
         been_in_arena = FALSE;
         nx = SCREEN_WID;
         ny = SCREEN_HGT/2;
         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN | CAVE_VIEW);
         note_spot(nx, ny);
         lite_spot(nx, ny);
         arena_handle_items();
      }
      else if (p_ptr->arena_state == ARENA_FIGHTING)
      {
         msg_print("You can't leave - kill 'em first");
      }
      else
      {
         if (arena_visit_level[p_ptr->lev]>3)
         {
            msg_print("Please return and fight a round or two next time!");
         }
         else
         {
            msg_print("Chicken!");
         }
      }
   }
   else if ((rand_int(3)==0) && arena_monsters_left())
   {
      msg_print("You hear the crowds cheer you on!");
   }
}

void move_special_location(s16b mlevel, s16b slevel, s16b oldx, s16b oldy,
                           s16b newx, s16b newy)
{
   if ((mlevel==0) && (slevel==0))
   {
     if (dungeon.level[sublevel][oldy][oldx].fdat & CAVE_AREN) handle_arena(oldx,oldy,newx,newy);
   }
}

