/* File: gentown.c */

/* Purpose: Town generation */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 * extensive alterations by Jurriaan W Kalkman 1997.
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Builds a store at a given (row, column)
 *
 * Note that the solid perma-walls are at x=0/65 and y=0/21
 *
 * As of 2.7.4 (?) the stores are placed in a more "user friendly"
 * configuration, such that the four "center" buildings always
 * have at least four grids between them, to allow easy running,
 * and the store doors tend to face the middle of town.
 *
 * The stores now lie inside boxes from 3-9 and 12-18 vertically,
 * and from 7-17, 21-31, 35-45, 49-59.  Note that there are thus
 * always at least 2 open grids between any disconnected walls.
 *           1         2         3         4         5         6      
 *  12345678901234567890123456789012345678901234567890123456789012345
 *  2
 *  3
 *  4  xxxxxxxxxx  xxxxxxxxxx  xxxxxxxxxx  xxxxxxxxxx  xxxxxxxxxx
 *  5  x
 *  6  x
 *  7  x
 *  8  x
 *  9  x
 * 10
 *  1
 *  2
 *  3  x
 *  4  x
 *  5  x
 *  6  x
 *  7  x
 *  8  x
 *  9
 * 20
 *  1

 */
static void build_store(s16b store_num, s16b store_type, s16b xx, s16b yy)
{
   s16b                 x, y, x0, y0, x1, y1, x2, y2;
   s16b                 xd, yd, tmp;
   cave_cell_type      *c_ptr;

   /* Find the "center" of the store */
#if 0
   x0 = xx * 14 + 12; /* 12, 26, 40, 54 */
   y0 = yy * 8 + 5;   /*  5, 13         */
#endif
   x0 = xx * 12 + 8; /*  8, 20, 32, 44, 56, max width 4 to left, 5 to right, 10 total */
   y0 = yy * 9 + 5; /*  5, 14              max height 2 to top, 3 to bottom, 6 total */
   
   /* Determine the store boundaries */
#if 0
   x1 = x0 - randint(4) - 1;
   y1 = y0 - randint((yy == 0) ? 3 : 2);
   x2 = x0 + randint(4) + 1;
   y2 = y0 + randint((yy == 1) ? 3 : 2);
#endif
   x1 = x0 - randint(3) - 1;
   y1 = y0 - randint(1) - 1;
   x2 = x0 + randint(4) + 1;
   y2 = y0 + randint(2) + 1;

dlog(DEBUGGENER,"gentown.c: build_store: %d,%d  = %d,%d to %d,%d\n",
                xx, yy, x1, y1, x2, y2);
   /* Build an invulnerable rectangular building */
   for (y = y1; y <= y2; y++)
   {
      for (x = x1; x <= x2; x++)
      {
         /* Get the grid */
         c_ptr = &dungeon.level[sublevel][y][x];

         /* Clear previous contents, add "basic" perma-wall */
         /* The buildings are illuminated and known         */
         (void)set_grid_type(x, y, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK);
      }
   }

   /* Pick a door direction (S,N,E,W) */
   tmp = rand_int(4);

   /* Re-roll "annoying" doors */
   if (((tmp == 0) && (yy == 1)) ||
       ((tmp == 1) && (yy == 0)) ||
       ((tmp == 2) && (xx == 3)) ||
       ((tmp == 3) && (xx == 0)))
   {

      /* Pick a new direction */
      tmp = rand_int(4);
   }

   /* Extract a "door location" */
   switch (tmp)
   {
      /* Bottom side */
      case 0:
         x = rand_range(x1,x2);
         if (x == x2)              xd = x - 1;
         else if (x == x1)         xd = x + 1;
         else if (randint(2) == 1) xd = x + 1;
         else                      xd = x - 1;
         y = y2;
         yd = y;
         break;

      /* Top side */
      case 1:
         x = rand_range(x1,x2);
         if (x == x2)              xd = x - 1;
         else if (x == x1)         xd = x + 1;
         else if (randint(2) == 1) xd = x + 1;
         else                      xd = x - 1;
         y = y1;
         yd = y;
         break;

      /* Right side */
      case 2:
         x = x2;
         xd = x;
         y = rand_range(y1,y2);
         if (y == y2)              yd = y - 1;
         else if (y == y1)         yd = y + 1;
         else if (randint(2) == 1) yd = y + 1;
         else                      yd = y - 1;
         break;

      /* Left side */
      default:
         x = x1;
         xd = x;
         y = rand_range(y1,y2);
         if (y == y2)              yd = y - 1;
         else if (y == y1)         yd = y + 1;
         else if (randint(2) == 1) yd = y + 1;
         else                      yd = y - 1;
         break;
   }
dlog(DEBUGGENER,"gentown.c: build_store: door dir %d, @ %d,%d\n", tmp, x, y);

   /* Clear previous contents, add a store door */
   create_town_store(store_type, x, y);
   /* and create a sign, if this isn't the home */
   if (store_type != DUNG_ENTR_HOME)
   { 
      (void)set_grid_type(xd, yd, DUNG_SIGN, store_type,
                          GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
   }
}

static void town_place_stairs(void)
{
   s16b x = 0, y = 0;
   s16b i, num;
   /* place 1+randint(3) stairs in town */
   num = 1 + randint(3);
   for (i=0; i<num; i++)
   {
      bool good_loc = FALSE;
      while (!good_loc)
      {
         /* choose a random place near the walls */
         switch (randint(4))
         {
            case 1: x = 1;
                    y = 1+rand_int(TOWN_HGT-2);
                    break;
            case 2: x = TOWN_WID-2;
                    y = 1+rand_int(TOWN_HGT-2);
                    break;
            case 3: x = 1+rand_int(TOWN_WID-2);
                    y = 1;
                    break;
            case 4: x = 1+rand_int(TOWN_WID-2);
                    y = TOWN_HGT-2;
                    break;
         }
         /* don't superimpose stairs */
         good_loc = (dungeon.level[sublevel][y][x].mtyp != DUNG_STAIR);
      }
dlog(DEBUGGENER,"gentown.c: town_place_stairs: attempting stair @ %d,%d\n", x, y);
      place_main_down_stair(x,y,((num>1) && (i>0)));
   }
   /* place 1+randint(3) stairs out of town */
   num = 1 + randint(3);
   for (i=0; i<num; i++)
   {
      bool good_loc = FALSE;
      while (!good_loc)
      {
         /* choose a random place near the walls */
         x = rand_int(cur_wid-1)+1;
         y = rand_int(cur_hgt-1)+1;
         if ((x<TOWN_WID) || (y<TOWN_HGT)) continue;
         if (dungeon.level[sublevel][y][x].fdat & CAVE_AREN) continue;
         if (dungeon.level[sublevel][y][x].fdat & CAVE_AREN_CROWD) continue;
         if (dungeon.level[sublevel][y][x].mtyp == DUNG_STAIR) continue;
         good_loc = TRUE;
      }
      place_main_down_stair(x,y,TRUE);
   }
}

/*
 * Generate the "consistent" town features
 *
 */
static void create_town_features(void)
{
   s16b x = 0, y = 0, k = 0, i;
   bool built_type[8], built_there[10];

   s16b rooms[MAX_STORES];

dlog(DEBUGGENER,"gentown.c: create_town_features: starting\n");
   /* 8 stores in town */
   for (i = 0; i < 8; i++)
   {
      built_type[i] = FALSE;
   }
   /* and 8 locations */
   for (i = 0; i < 10; i++)
   {
      built_there[i] = FALSE;
   }

   /* first build the 8 normal stores */
   for (i = 0; i < 8; i++)
   {
      bool ok_type = FALSE, ok_there = FALSE;
      /* find an un-built store */
      while ((!ok_type) || (!ok_there))
      {
         /* force home as first store built, ever! */
         if (i==0)
         {
            k = 7;
         }
         else
         {
            k = rand_int(8);
         }
         /* if this type hasn't been built before, it's ok */
         ok_type = (built_type[k] == FALSE);

         while (!ok_there)
         {
            x = rand_int(5);
            y = rand_int(2);
            ok_there = (built_there[x+5*y] == FALSE);
         }
      }
      /* Build that store at the proper location */
dlog(DEBUGGENER,"gentown.c: create_town_features: building store %d as type %d @ %d,%d\n",
                i, k, x, y);
      build_store(k, k, x, y);
      built_type[k] = TRUE;
      built_there[x+5*y] = TRUE;
      rooms[i] = i;
      ok_type = FALSE;
      ok_there = FALSE;
   }
   /* now for the two 'bonus' stores */
   for (i = 8; i < 10; i++)
   {
      s16b type_done = -1;
      bool ok_there = FALSE;
      while (!ok_there)
      {
         x = rand_int(5);
         y = rand_int(2);
         ok_there = (built_there[x+5*y] == FALSE);
      }
      /* choose a type that is not a plain town store */
      k = randint(DUNG_ENTR_EXT_END-DUNG_ENTR_EXT_STRT)+DUNG_ENTR_EXT_STRT;
      /* and make sure we don't make two */
      if ((type_done != -1) && (k == type_done))
      {
         while (k == type_done)
         {
            k = randint(DUNG_ENTR_EXT_END-DUNG_ENTR_EXT_STRT)+DUNG_ENTR_EXT_STRT;
         }
      }
      if (type_done == -1)
      {
         type_done = k;
      } 

      /* Build that store at the proper location */
dlog(DEBUGGENER,"gentown.c: create_town_features: building store %d as type %d @ %d,%d\n",
                i, k, x, y);
      build_store(i, k, x, y);
      built_there[x+5*y] = TRUE;
      rooms[i] = i;
      ok_there = FALSE;
   }
dlog(DEBUGGENER,"gentown.c: create_town_features: placing stairs\n");

   /* Place the stairs */
   town_place_stairs();
dlog(DEBUGGENER,"gentown.c: create_town_features: returning\n");
}

static bool planting(s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   /* don't build inside the arena */
   if (c_ptr->fdat & CAVE_AREN) return (FALSE);
   if (dungeon.level[sublevel][y][x].fdat & CAVE_AREN_CROWD) return(FALSE);
   /* don't build inside the town */
   if ((x<TOWN_WID) && (y<TOWN_HGT)) return (FALSE);
   /* don't build on the walls around the arena */
   if (test_grid_ptr(c_ptr, DUNG_PERWALL, DUNG_PERWALL_NORMAL)) return (FALSE);
   return (TRUE);
}

static void place_copse(s16b x, s16b y, s16b size, bool trees)
{
   s16b nx, ny;
   s16b dirplus, dirmin, maxbonus;
   s16b treetype = 0;

   if (trees) treetype=randint(DUNG_SHRUB_END)+DUNG_SHRUB_START;

   /* the copse grows somewhat in dirplus, shrinks some in dirmin */
   dirplus=randint(8)+1;
   dirmin=randint(8)+1;
   while (dirmin==dirplus) dirplus=randint(8)+1;
   maxbonus = (size/3)+1;

   for (nx=x-size-maxbonus; nx<x+size+maxbonus; nx++)
   {
      for (ny=y-size-maxbonus; ny<y+size+maxbonus; ny++)
      {
         s16b dir;
         s16b bonus = 0;

         if (!in_bounds(nx, ny)) continue;
         if (dungeon.level[sublevel][ny][nx].fdat & CAVE_AREN) continue;
         if (dungeon.level[sublevel][ny][nx].fdat & CAVE_AREN_CROWD) continue;

         dir = what_dir(x, y, nx, ny);

         if (dir == dirplus)
            bonus = randint(size/3)+1;
         else if (dir == dirmin)
            bonus = -randint(size/3)-1;

         /* are we in range on a suitable square */
         if ((distance(x, y, nx, ny) <= (size + bonus)) && planting(nx, ny))
         {
            if ((dungeon.level[sublevel][ny][nx].mtyp == DUNG_SHRUB) && (randint(3)==1)) continue;

            if (trees)
            {
               if (randint(15)==1)
                  set_grid_type(nx, ny, DUNG_SHRUB,
                                randint(DUNG_SHRUB_END-DUNG_SHRUB_START)+DUNG_SHRUB_START,
                                GRID_KEEP, 0);
               else
                  set_grid_type(nx, ny, DUNG_SHRUB,treetype,GRID_KEEP, 0);
               if ((randint(10)<(size/3)) && (randint(5)==1))
               {
                  (void)summon_specific(nx, ny, 10, SUMMON_ANIMAL, 0);
               }
            }
            else
               set_grid_type(nx,ny,DUNG_WATER,DUNG_WATER_POOL,GRID_KEEP, 0);
         } /* suitable square */
      } /* ny */
   } /* nx */
}

static void place_tree_line(void)
{
   s16b x, nx, y, ny;
   s16b dir,i;

   x=rand_int(cur_wid-1)+1;
   y=rand_int(cur_hgt-1)+1;

dlog(DEBUGGENER,"gentown.c: place_tree_line: start @ %d,%d\n", x, y);
   if (randint(3)==1)
   {
      i = 0;
      /* search 100 times for a square with a 3 in 4 chance of trees on it */
      while ((i<100) && !planting(x, y) &&
             ((randint(4)<4)?(dungeon.level[sublevel][y][x].mtyp != DUNG_SHRUB):TRUE))
      {
         x=rand_int(cur_wid-1)+1;
         y=rand_int(cur_hgt-1)+1;
dlog(DEBUGGENER,"gentown.c: place_tree_line: cont @ %d,%d\n", x, y);
         i++;
      }
      if (i==100) return;
   }
dlog(DEBUGGENER,"gentown.c: place_tree_line: found @ %d,%d\n", x, y);
   /* try for some squares */
   while (randint(80)!=1)
   {
      for (i=0; i<10; i++)
      {
         if (i==5) i++;
         nx = x + ddx[i];
         ny = y + ddy[i];
         if (!in_bounds(nx, ny)) continue;
         /* have we found a tree? */
         if (dungeon.level[sublevel][ny][nx].mtyp==DUNG_SHRUB) break;
      }
      /* we didn't find a tree -> choose a random direction */
      if (i==10)
      {
         dir = randint(9);
         while (dir==5) dir = randint(9);
         nx = x + ddx[dir];
         ny = y + ddy[dir];
      }
      if (!in_bounds(nx, ny)) continue;
      /* don't plant trees in the arena */
      if (dungeon.level[sublevel][ny][nx].fdat & CAVE_AREN) continue;
      /* if we connected to a tree, there's a chance of stopping */
      if ((dungeon.level[sublevel][ny][nx].mtyp==DUNG_SHRUB) && (randint(5)==1)) break;

      if (planting(nx, ny))
      {
         set_grid_type(nx, ny, DUNG_SHRUB,
                       randint(DUNG_SHRUB_END)+DUNG_SHRUB_START,
                       GRID_KEEP, 0);
      }
      x = nx;
      y = ny;
   }
}

static s16b dir_choose3(s16b a, s16b b, s16b c)
{
   switch (randint(3))
   {
      case 1: return(a);
      case 2: return(b);
      case 3: return(c);
   }
   quit("randint out of range in dir_choose3");
   return (-1);
}

static s16b dir_choose5(s16b a, s16b b, s16b c, s16b d, s16b e)
{
   switch (randint(5))
   {
      case 1: return(a);
      case 2: return(b);
      case 3: return(c);
      case 4: return(d);
      case 5: return(e);
   }
   quit("randint out of range in dir_choose5");
   return (-1);
}

static void build_brook(s16b x, s16b y, s16b prefdir, s16b maxlen)
{
   s16b nx, ny;
   s16b len, i, treetype;
   s16b dir = 0;

   treetype = randint(DUNG_SHRUB_END)+DUNG_SHRUB_START;

   while (maxlen>0)
   {
      if (maxlen<10)
         len = randint(3);
      else
         len = randint(5)+1;

      /* normally: choose an appropriate dir: if prefdir = N, choose NW, N, NE */
      switch (prefdir)
      {
         case DIR_N:  dir = dir_choose3(DIR_NE, DIR_N,  DIR_NW); break;
         case DIR_NW: dir = dir_choose3(DIR_W,  DIR_NW, DIR_N ); break;
         case DIR_W:  dir = dir_choose3(DIR_SW, DIR_W,  DIR_NW); break;
         case DIR_SW: dir = dir_choose3(DIR_S,  DIR_SW, DIR_W ); break;
         case DIR_S:  dir = dir_choose3(DIR_SE, DIR_S,  DIR_SW); break;
         case DIR_SE: dir = dir_choose3(DIR_E,  DIR_SE, DIR_S ); break;
         case DIR_E:  dir = dir_choose3(DIR_NE, DIR_E,  DIR_SE); break;
         case DIR_NE: dir = dir_choose3(DIR_N,  DIR_NE, DIR_E ); break;
      }
      /* sometimes, choose more: from N, choose W, NW, N, NE, E */
      if (randint(8)==1)
      {
         switch (prefdir)
         {
            case DIR_N:  dir = dir_choose5(DIR_E,  DIR_NE, DIR_N,  DIR_NW, DIR_W );
                         break;
            case DIR_NW: dir = dir_choose5(DIR_SW, DIR_W,  DIR_NW, DIR_N,  DIR_NE);
                         break;
            case DIR_W:  dir = dir_choose5(DIR_S,  DIR_SW, DIR_W,  DIR_NW, DIR_N );
                         break;
            case DIR_SW: dir = dir_choose5(DIR_SE, DIR_S,  DIR_SW, DIR_W,  DIR_NW );
                         break;
            case DIR_S:  dir = dir_choose5(DIR_E,  DIR_SE, DIR_S,  DIR_SW, DIR_W );
                         break;
            case DIR_SE: dir = dir_choose5(DIR_NE, DIR_E,  DIR_SE, DIR_S,  DIR_SW);
                         break;
            case DIR_E:  dir = dir_choose5(DIR_N,  DIR_NE, DIR_E,  DIR_SE, DIR_S );
                         break;
            case DIR_NE: dir = dir_choose5(DIR_NW, DIR_N,  DIR_NE, DIR_E,  DIR_SE );
                         break;
         }
      }
      /* sometimes branch off */
      if ((randint(10)==1) && (dir!=prefdir))
      {
         build_brook(x, y, prefdir, maxlen/2);
      }
      for (i=0; i<len; i++)
      {
         nx = x + ddx[dir];
         ny = y + ddy[dir];
         if (in_bounds(nx, ny) && planting(nx, ny))
         {
            set_grid_type(nx,ny,DUNG_WATER,DUNG_WATER_BROOK,GRID_KEEP, 0);
            /* now plant some trees at the water's edge */
            x = nx;
            y = ny;
            for (i=1; i<10; i++)
            {
               if (i==5) i++;
               nx = x + ddx[i];
               ny = y + ddy[i];
               if (in_bounds(nx, ny) && planting(nx, ny) && (randint(12)==1) &&
                   (dungeon.level[sublevel][ny][nx].mtyp != DUNG_WATER))
               {
                  set_grid_type(nx, ny, DUNG_SHRUB, treetype, GRID_KEEP, 0);
                  if (randint(20)==1)
                  {
                    treetype = randint(DUNG_SHRUB_END)+DUNG_SHRUB_START;
                  }
               }
            }
         }
         else
         {
            /* since we either went out of bounds or bumped on a permwall */
            /* let the loop be over */
            break;
         }
      }
      maxlen -= len;
   }
}

static void start_brook(s16b *nx, s16b *ny, s16b x, s16b y)
{
   switch (randint(5))
   {
      case 1: (*nx) = x;                /* W wall */
              (*ny) = rand_int(cur_hgt-1)+1;
              break;
      case 2: (*nx) = cur_wid-x;        /* E wall */
              (*ny) = rand_int(cur_hgt-1)+1;
              break;
      case 3: (*nx) = rand_int(cur_wid-1)+1; /* N wall */
              (*ny) = y;
              break;
      case 4: (*nx) = rand_int(cur_wid-1)+1; /* S wall */
              (*ny) = cur_hgt-y;
              break;
      case 5: (*nx) = cur_wid/2 - cur_wid/20 + x; /* center */
              (*ny) = cur_hgt/2 - cur_hgt/20 + y;
              break;
   }
}

static void place_brook(void)
{
   s16b x, y, nx, ny;
   s16b prefdir = 0;
   s16b i;
   bool dirok = FALSE;

   /* we try to let a brook start either in the center, or close to one of */
   /* the N, E, S or W walls */
   x = randint(cur_wid/10);
   y = randint(cur_hgt/10);
dlog(DEBUGGENER,"gentown.c: place_brook: @ %d,%d\n", x, y);
   start_brook(&nx, &ny, x, y);
   /* we need a valid square and fit for planting */
   while (!in_bounds(nx, ny) && !planting(nx, ny) && !(dungeon.level[sublevel][ny][nx].fdat & CAVE_AREN))
   {
      x = nx;
      y = ny;
      start_brook(&nx, &ny, x, y);
   }
   x = nx;
   y = ny;
   /* we want a direction from 1 to 4 and from 6 to 9 */
   while (dirok==FALSE)
   {
      prefdir = (randint(2)-1)?randint(4):randint(4)+5;
      nx = x;
      ny = y;
      /* see if we can walk some squares in the correct direction */
      for (i=0; i<((cur_wid+cur_hgt)/10); i++)
      {
         nx = nx+ddx[prefdir];
         ny = ny+ddy[prefdir];
      }
      if (in_bounds(nx, ny) && planting(nx, ny)) dirok=TRUE;
   }
   /* we have a starting point in x,y and a preferred direction in prefdir */
   /* the rest is done in a function, so facilitate the split of brooks    */
   /* which requires recursive calling */
   /* build a brook, cur_wid+cur_hgt /5 in length */
   build_brook(x, y, prefdir, (cur_wid+cur_hgt)/5);
}

static void town_gen_wilderness(void)
{
   s16b            x, y, i;
   s16b            chance;
   cave_cell_type *c_ptr;

   for (i=0; i<60;i++)
   {
dlog(DEBUGGENER,"gentown.c: town_gen_wilderness: i %d\n", i);
      x=rand_int(cur_wid-1)+1;
      y=rand_int(cur_hgt-1)+1;
      if (!in_bounds(x, y)) continue;
      /* trees in the arena look very stupid! */
      if (dungeon.level[sublevel][y][x].fdat & CAVE_AREN) continue;
      if (dungeon.level[sublevel][y][x].fdat & CAVE_AREN_CROWD) continue;

      c_ptr = &dungeon.level[sublevel][y][x];
      chance=randint(50);
dlog(DEBUGGENER,"gentown.c: town_gen_wilderness: chance %d at %d,%d\n", chance, x, y);

      /* build small copse of trees */
      if (chance<15)
      {
         place_copse(x, y, randint(5)+3, TRUE);
      }
      else if (chance<25)
      /* build medium copse of trees */
      {
         place_copse(x, y, randint(15)+7, TRUE);
      }
      /* place a pool */
      else if (chance<35)
      {
         place_copse(x, y, randint(2)+3, FALSE);
      }
      /* place a tree line */
      else if (chance<45)
      {
         place_tree_line();
      }
      else
      /* build a brook */
      {
         place_brook();
      }

   }
}

/*
 * this finds a 't' town inhabitant to populate the arena crowd
 * if we ever change those monsters from t to something else,
 * this will hang...
 */
static s16b create_arena_crowd_monster(void)
{
   s16b r_idx;
   r_idx = get_mon_num(0); /* Pick a town monster */

   /* Handle failure */
  if (!r_idx) quit ("gentown.c: create_arena_crowd_monster: summon level 0 monster failed town_gen_arena");

   while ( r_info[r_idx].d_char != 't' )
   {
      r_idx = get_mon_num(0); /* Pick a town monster */
      if (!r_idx) quit ("gentown.c: create_arena_crowd_monster: summon level 0 monster failed town_gen_arena");
   }
   return (r_idx);
}

static void town_gen_arena(void)
{
   s16b            x, y, nx, ny, r_idx;
   cave_cell_type       *c_ptr;

   /* build access tunnel */
   for (x=0; x<5; x++)
   {
      nx = x+TOWN_WID-1;
      ny = TOWN_HGT/2;

      (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK);
      if (x>0) dungeon.level[sublevel][ny][nx].fdat |= CAVE_AREN;

      /* walls above and below the tunnel */
      (void)set_grid_type(nx, ny-1, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK);
      if (x>0) dungeon.level[sublevel][ny-1][nx].fdat |= CAVE_AREN;

      (void)set_grid_type(nx, ny+1, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK);
      if (x>0) dungeon.level[sublevel][ny+1][nx].fdat |= CAVE_AREN;
   }
   /* create arena room */
   for (x=0; x<=16; x++)
   {
      for (y=-3; y<=3; y++)
      {
         nx = x+TOWN_WID+4;
         ny = y+TOWN_HGT/2;
         c_ptr=&dungeon.level[sublevel][ny][nx];

         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN);
      }
   }
   /* create arena crowd upper/lower */
   for (x=0;x<=16;x++)
   {
      nx = x+TOWN_WID+4;
      ny = TOWN_HGT/2+4;
      (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_GLASS,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN | CAVE_AREN_CROWD);
      ny = TOWN_HGT/2-4;
      (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_GLASS,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN | CAVE_AREN_CROWD);
      for (y=-1;y<=1;y++)
      {
         /* lower half */
         ny = y+TOWN_HGT/2+6;
         c_ptr=&dungeon.level[sublevel][ny][nx];

         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
         r_idx = create_arena_crowd_monster(); /* Pick a town monster */

         /* Attempt to place the monster (awake, allow groups) */
         /* failures are quietly ignored here */
         (void)place_monster_aux(nx, ny, r_idx, FALSE, FALSE, 0, 0);

         /* upper half */
         ny = y+TOWN_HGT/2-6;
         c_ptr=&dungeon.level[sublevel][ny][nx];

         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
         r_idx = create_arena_crowd_monster(); /* Pick a town monster */

         /* Attempt to place the monster (awake, allow groups) */
         /* failures are quietly ignored here */

         (void)place_monster_aux(nx, ny, r_idx, FALSE, FALSE, 0, 0);
      }
   }
   /* create arena upper/lower */
   for (x=0;x<=16;x++)
   {
      /* build walls above and below them */
      nx = x+TOWN_WID+4;
      ny = TOWN_HGT/2+8;
      c_ptr=&dungeon.level[sublevel][ny][nx];
      (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
      ny = TOWN_HGT/2-8;
      c_ptr=&dungeon.level[sublevel][ny][nx];
      (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
      if (x==0)
      {
         for ( y = 0 ; y < 5 ; y++ )
         {
            nx = TOWN_WID + 3;
            ny = TOWN_HGT/2 - 4 - y;
            c_ptr=&dungeon.level[sublevel][ny][nx];
            (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                                GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
            ny = TOWN_HGT/2 + 4 + y;
            c_ptr=&dungeon.level[sublevel][ny][nx];
            (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                                GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
            nx = TOWN_WID + 21;
            ny = TOWN_HGT/2 - 4 - y;
            c_ptr=&dungeon.level[sublevel][ny][nx];
            (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                                GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
            ny = TOWN_HGT/2 + 4 + y;
            c_ptr=&dungeon.level[sublevel][ny][nx];
            (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                                GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
         }
      }
   }

   /* create arena crowd to the right */
   for (y=-3;y<=3;y++)
   {
      nx = TOWN_WID+21;
      ny = y+TOWN_HGT/2;
      (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_GLASS,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
      nx = TOWN_WID+25;
      (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
      for (x=0;x<3;x++)
      {
         /* lower half */
         nx = x+TOWN_WID+22;
         c_ptr=&dungeon.level[sublevel][ny][nx];

         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
         r_idx = create_arena_crowd_monster(); /* Pick a town monster */

         /* Attempt to place the monster (awake, allow groups) */
         /* failures are quietly ignored here */
         (void)place_monster_aux(nx, ny, r_idx, FALSE, FALSE, 0, 0);
      }
   }
   /* light up the walls of the right public */
   for (y=-3;y<=3;y++)
   {
      nx = TOWN_WID+3;
      ny = y+TOWN_HGT/2;
      if (y!=0)
      {
         c_ptr=&dungeon.level[sublevel][ny][nx];
         (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
      }
      if (y==-3)
      {
         for (x=0;x<4;x++)
         {
            nx = x+TOWN_WID+22;
            ny = y+TOWN_HGT/2-1;
            c_ptr=&dungeon.level[sublevel][ny][nx];
            (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                                GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
         }
      }
      if (y==3)
      {
         for (x=0;x<4;x++)
         {
            nx = x+TOWN_WID+22;
            ny = y+TOWN_HGT/2+1;
            c_ptr=&dungeon.level[sublevel][ny][nx];
            (void)set_grid_type(nx, ny, DUNG_PERWALL, DUNG_PERWALL_NORMAL,
                                GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
         }
      }
   }
   /* place a random door for the upper crowd */
   nx = TOWN_WID+4+randint(16);
   ny = TOWN_HGT/2+8;
   c_ptr=&dungeon.level[sublevel][ny][nx];
   (void)set_grid_type(nx, ny, DUNG_DOOR, DUNG_DOOR_JAMMED,
                       GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
   c_ptr->extra = 15;
   /* place a random door for the lower crowd */
   nx = TOWN_WID+4+randint(16);
   ny = TOWN_HGT/2-8;
   c_ptr=&dungeon.level[sublevel][ny][nx];
   (void)set_grid_type(nx, ny, DUNG_DOOR, DUNG_DOOR_JAMMED,
                       GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
   c_ptr->extra = 15;
   /* and place a door for the right crowd */
   nx = TOWN_WID+25;
   ny = (TOWN_HGT/2)-4+randint(7);
   c_ptr=&dungeon.level[sublevel][ny][nx];
   (void)set_grid_type(nx, ny, DUNG_DOOR, DUNG_DOOR_JAMMED,
                       GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN_CROWD);
   c_ptr->extra = 15;
}

void town_recrowd_arena(void)
{
   s16b            x, y, nx, ny, r_idx;
   cave_cell_type       *c_ptr;

   /* create arena crowd upper/lower */
   for (x=0;x<=16;x++)
   {
      nx = x+TOWN_WID+4;
      for (y=-1;y<=1;y++)
      {
         /* lower half */
         ny = y+TOWN_HGT/2+6;
         c_ptr=&dungeon.level[sublevel][ny][nx];

         if (c_ptr->m_idx) continue; /* there is already a monster there */

         r_idx = create_arena_crowd_monster(); /* Pick a town monster */

         /* Attempt to place the monster (awake, allow groups) */
         /* failures are quietly ignored here */
         (void)place_monster_aux(nx, ny, r_idx, FALSE, FALSE, 0, 0);

         /* upper half */
         ny = y+TOWN_HGT/2-6;
         c_ptr=&dungeon.level[sublevel][ny][nx];

         if (c_ptr->m_idx) continue; /* there is already a monster there */

         r_idx = create_arena_crowd_monster(); /* Pick a town monster */

         /* Attempt to place the monster (awake, allow groups) */
         /* failures are quietly ignored here */
         (void)place_monster_aux(nx, ny, r_idx, FALSE, FALSE, 0, 0);
      }
   }

   /* create arena crowd to the right */
   for (y=-3;y<=3;y++)
   {
      ny = y+TOWN_HGT/2;
      for (x=0;x<3;x++)
      {
         /* lower half */
         nx = x+TOWN_WID+22;
         c_ptr=&dungeon.level[sublevel][ny][nx];

         if (c_ptr->m_idx) continue; /* there is already a monster there */

         r_idx = create_arena_crowd_monster(); /* Pick a town monster */

         /* Attempt to place the monster (awake, allow groups) */
         /* failures are quietly ignored here */
         (void)place_monster_aux(nx, ny, r_idx, FALSE, FALSE, 0, 0);
      }
   }
}

/*
 * build the stores in town 
 */
static void town_gen_stores(void)
{
   s16b       x,y,i;
   cave_cell_type *c_ptr;

dlog(DEBUGGENER,"town_gen_stores: step 0\n");
   /* Perma-walls -- North/South */
   for (x = 0; x < TOWN_WID; x++)
   {
       /* North wall */
       c_ptr = &dungeon.level[sublevel][0][x];

       /* Clear previous contents, add "solid" perma-wall */
       /* Illuminate and memorize the walls */
       (void)set_grid_type(x, 0, DUNG_PERWALL, DUNG_PERWALL_SOLID,
                           GRID_KEEP, CAVE_GLOW | CAVE_MARK);

       /* South wall */
       c_ptr = &dungeon.level[sublevel][TOWN_HGT-1][x];

       /* Clear previous contents, add "solid" perma-wall */
      (void)set_grid_type(x, TOWN_HGT-1, DUNG_PERWALL, DUNG_PERWALL_SOLID,
                          GRID_KEEP, CAVE_GLOW | CAVE_MARK);
   }
dlog(DEBUGGENER,"town_gen_stores: step 1\n");
   /* Perma-walls -- West/East */
   for (y = 0; y < TOWN_HGT; y++)
   {
       /* West wall */
       c_ptr = &dungeon.level[sublevel][y][0];

       /* Clear previous contents, add "solid" perma-wall */
       (void)set_grid_type(0, y, DUNG_PERWALL, DUNG_PERWALL_SOLID,
                           GRID_KEEP, CAVE_GLOW | CAVE_MARK);

       /* East wall */
       c_ptr = &dungeon.level[sublevel][y][TOWN_WID-1];

       /* Clear previous contents, add "solid" perma-wall */
       (void)set_grid_type(TOWN_WID-1, y, DUNG_PERWALL, DUNG_PERWALL_SOLID,
                           GRID_KEEP, CAVE_GLOW | CAVE_MARK);
   }
dlog(DEBUGGENER,"town_gen_stores: step 2\n");
   /* don't touch the edges, perma-walls have been set there */
   for (y = 1; y < TOWN_HGT-1; y++)
   {
      for (x = 1; x < TOWN_WID-1; x++)
      {
         /* Clear all features, set to "empty floor" */
         (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
         wipe_floor_items_traps(x, y);
      }
   }
dlog(DEBUGGENER,"town_gen_stores: step 3, accessing %d,%d\n", TOWN_WID/2, TOWN_HGT-1);
   (void)set_grid_type(TOWN_WID/2, TOWN_HGT-1, DUNG_DOOR, DUNG_DOOR_LOCKED,
                       GRID_KEEP, CAVE_GLOW | CAVE_MARK);
   dungeon.level[sublevel][TOWN_HGT-1][TOWN_WID/2].extra = 15;

   /* Hack -- Build the buildings/stairs (from memory) */
   create_town_features();
dlog(DEBUGGENER,"town_gen_stores: step 4\n");

   new_player_spot(TRUE);

   /* Day Light */
   if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
   {
      /* Lite up the town */
      for (y = 0; y < TOWN_HGT; y++)
      {
         for (x = 0; x < TOWN_WID; x++)
         {
            c_ptr = &dungeon.level[sublevel][y][x];

            /* Perma-Lite */
            c_ptr->fdat |= CAVE_GLOW;

            /* Memorize */
            if (view_perma_grids) c_ptr->fdat |= CAVE_MARK;
         }
      }
dlog(DEBUGGENER,"town_gen_stores: step 4a\n");
      /* Make some day-time residents */
      for (i = 0; i < MIN_M_ALLOC_TD; i++) (void)alloc_monster(3, TRUE, TRUE);
dlog(DEBUGGENER,"town_gen_stores: step 4a2\n");
   }

   /* Night Time */
   else
   {
      /* Make some night-time residents */
dlog(DEBUGGENER,"town_gen_stores: step 4b\n");
      for (i = 0; i < MIN_M_ALLOC_TN; i++) (void)alloc_monster(3, TRUE, TRUE);
dlog(DEBUGGENER,"town_gen_stores: step 4b2\n");
   }
dlog(DEBUGGENER,"town_gen_stores: step end\n");
}

/*
 * Town logic flow for generation of new town
 *
 * We start with a fully wiped cave of normal floors.
 *
 * This function does NOT do anything about the owners of the stores,
 * nor the contents thereof.  It only handles the physical layout.
 *
 * We place the player on the stairs at the same time we make them.
 *
 * Hack -- since the player always leaves the dungeon by the stairs,
 * he is always placed on the stairs, even if he left the dungeon via
 * word of recall or teleport level.
 */
void town_gen(void)
{
   s16b        x, y;

   /* we only build full-sized towns on sublevel 0 */
   dungeon.level_wid[0] = MAX_WID;
   dungeon.level_hgt[0] = MAX_HGT;

   /* Hack -- Start with basic floors */
   for (y = 0; y < cur_hgt; y++)
   {
      for (x = 0; x < cur_wid; x++)
      {
         /* Clear all features, set to "perma-wall" */
         (void)set_grid_type(x, y, DUNG_WILDN,
                             DUNG_WILD_GRASS, GRID_KEEP, 0);
         wipe_floor_items_traps(x, y);
      }
   }

   /* Perma-walls -- North/South */
   for (x = 0; x < cur_wid; x++)
   {
      /* North wall */
      (void)set_grid_type(x, 0, DUNG_PERWALL,
                          DUNG_PERWALL_SOLID, GRID_KEEP, 0);
     (void)set_grid_type(x, cur_hgt-1, DUNG_PERWALL,
                         DUNG_PERWALL_SOLID, GRID_KEEP, 0);
   }

   /* Perma-walls -- West/East */
   for (y = 0; y < cur_hgt; y++)
   {
     (void)set_grid_type(0, y, DUNG_PERWALL,
                         DUNG_PERWALL_SOLID, GRID_KEEP, 0);
     (void)set_grid_type(cur_wid-1, y, DUNG_PERWALL,
                         DUNG_PERWALL_SOLID, GRID_KEEP, 0);
   }
   dlog(DEBUGGENER,"gentown.c: gen_town: about to generate stores num_stores %d\n", num_stores);
   msg_print("Stores. ");
   town_gen_stores();
   dlog(DEBUGGENER,"gentown.c: gen_town: about to generate arena\n");
   msg_print("Arena. ");
   town_gen_arena();
   dlog(DEBUGGENER,"gentown.c: gen_town: about to generate wilderness\n");
   msg_print("Wilderness. ");
   town_gen_wilderness();
   store_built = TRUE;
}
