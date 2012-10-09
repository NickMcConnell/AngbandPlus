/* File: cave.c */

/* Purpose: low level dungeon routines -BEN- */


#include "angband.h"

/* jk */
s16b objects_on_floor(s16b x, s16b y)
{
   s16b i;
   s16b result=0;
/* quick hack: if there can be no objects, or the index is zero, return quickly */
   if (!floor_grid_bold(x,y) || (dungeon.level[sublevel][y][x].i_idx==0)) return (0);

   for (i=0;i<MAX_FLOOR_ITEMS;i++)
   {
     if (floor_item_xy(i,x,y)) result++;
   }
   return(result);
}

/* jk */
bool floor_absorb(object_type *i_ptr,s16b x, s16b y,bool emergency)
{
   s16b j;
   object_type *j_ptr;
   cave_cell_type *c_ptr;
   s16b objs = objects_on_floor(x,y);

   if (objs<MAX_PILE_FLOOR) return (TRUE);
   if (emergency && (objs<MAX_FLOOR_ITEMS)) return (TRUE);
   c_ptr = &dungeon.level[sublevel][y][x];
  /* Check for combining */
   for (j = 0; j < objs; j++)
   {
      if (!floor_item_xy(j,x,y)) continue;
      j_ptr = get_item_pointer_floor_xy(j,x,y);
      if (object_similar(j_ptr, i_ptr)) return (TRUE);
   }
   return (FALSE);
}

s16b objects_on_floor_absorb(object_type *i_ptr,s16b x, s16b y)
{
   s16b            i;
   object_type    *j_ptr;
   s16b            result = objects_on_floor(x,y);

   if (result==0) return (result);

   for (i = 0; i < result; i++)
   {
      j_ptr = get_item_pointer_floor_xy(i,x,y);
      if (object_similar(j_ptr, i_ptr))
      {
         return (result);
      }
   }
   return (i+1);
}

/*
 * this function determines if this grid could hold more items
 * without an emergency
 */

bool clean_enough_floor(s16b x, s16b y)
{
   /* can this grid hold items */
   if (!floor_grid_bold(x,y)) return (FALSE);
   /* are there less than the soft maximum? */
   if (objects_on_floor(x,y)<MAX_PILE_FLOOR) return (TRUE);
   return (FALSE);
}

/*
 * this function determines if this grid could hold more items
 */
bool clean_enough_floor_absorb(s16b x, s16b y,object_type *i_ptr,bool emergency)
{
   /* can this grid hold items */
   if (!floor_grid_bold(x,y)) return (FALSE);
   /* are there less than the soft maximum? */
   if (objects_on_floor(x,y)<MAX_PILE_FLOOR) return (TRUE);
   return (floor_absorb(i_ptr,x,y,emergency));
}

/*
 * this function determines if a square can hold another item in if it *REALLY*
 * needs to
 */
bool clean_enough_floor_emergency(s16b x, s16b y)
{
   if (!floor_grid_bold(x,y)) return (FALSE);
   if (objects_on_floor(x,y)<MAX_FLOOR_ITEMS) return (TRUE);
   return (FALSE);
}

/* jk */
/* this function returns the free index in the dungeon.level[sublevel][y][x].i_ptr[] array */
/* it depends on a correct floor grid */
/* -1 signifies no free indexes (which shouldn't happen! */
s16b floor_free_index(s16b x, s16b y)
{
   s16b i;

   for (i=0;i<MAX_FLOOR_ITEMS;i++)
   {
      if (!floor_item_xy(i,x,y)) return ((s16b)i);
   }
   return(-1);
}

u16b get_f_idx(u16b mtyp, u16b styp)
{
   u16b       i;

   for ( i=f_info_index[mtyp]; i<f_number ; i++)
   {
      if (f_info[i].mtyp == mtyp)
      {
         if (f_info[i].styp == styp) break;
      }
   }
   /* something went wrong! */
   if (i==f_number)
   {
dlog(DEBUGALWAYS,"cave.c: get_f_idx: unknown feature (mtyp %d, styp %d)\n", mtyp, styp);
      return (get_f_idx(DUNG_NOTHING, DUNG_NOTHING_NORMAL));
   }
   return i;
}

s16b get_f_idx_tolerant(u16b mtyp, u16b styp)
{
   u16b       i;

   for (i=f_info_index[mtyp];i<f_number;i++)
   {
      if (f_info[i].mtyp == mtyp)
      {
         if (f_info[i].styp == styp) break;
      }
   }
   if (i==f_number)
   {
      return -1;
   }
   return (s16b)i;
}

/*
 * make a stair point to another level
 * x,y - location of stair
 * baselevel - point to baselevel %d
 * sublevel - point to sublevel %d
 */
void point_stair_to_level(s16b x, s16b y, s16b to_baselevel, s16b to_sublevel)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

   if (to_baselevel < 0) to_baselevel = 0;
   if (to_baselevel >= MAX_LEVEL) to_baselevel = MAX_LEVEL;

   if (to_sublevel < 0) quit("cave.c: point_stair_to_level: to_sublevel < 0");
   /* we cannot test for the maximum here, assume the calling routine knows its stuff */

   c_ptr->extra = to_baselevel * 1000 + to_sublevel;
}

/*
 * where does a stair lead?
 */
void get_stair_target(s16b x, s16b y, s16b *new_baselevel, s16b *new_sublevel)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   (*new_baselevel) = (c_ptr->extra / 1000);
   (*new_sublevel) = (c_ptr->extra % 1000);
}

/*
 * this changes a grid's feature to a new feature index.
 * for a grid that is in the current view (has CAVE_VIEW)
 * this also changes the memory of that grid
 */
void set_grid_idx(s16b x, s16b y,u16b f_idx, byte type, u32b flags)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

#if 0
   if (c_ptr->fdat & CAVE_VIEW)
   {
      c_ptr->memory_mtyp = f_info[f_idx].mtyp;
      c_ptr->memory_styp = f_info[f_idx].styp;
   }
#endif

   c_ptr->mtyp = f_info[f_idx].mtyp;
   c_ptr->styp = f_info[f_idx].styp;

   /* flags for square are the standard flags for that square and */
   /* the flags we defined extra */
   flags |= f_info[f_idx].flags;

   switch (type)
   {
      /* chance the 'easy' options that depend on the feature type for a grid */
      /* keep the options in CAVE_FLAGS_PERSISTENT */
      case GRID_KEEP :
dlog(DEBUGGENER2,"cave.c: set_grid_idx: persist %08lx fdat %08lx keeping %08lx flags %08lx or %08lx result %08lx\n",
                 CAVE_FLAGS_PERSISTENT, c_ptr->fdat,
                 (c_ptr->fdat & CAVE_FLAGS_PERSISTENT), flags, (flags & ~(CAVE_FLAGS_PERSISTENT)),
                 (c_ptr->fdat & CAVE_FLAGS_PERSISTENT) | (flags & ~(CAVE_FLAGS_PERSISTENT)));

                         c_ptr->fdat = (c_ptr->fdat & CAVE_FLAGS_PERSISTENT) |
                                       (flags & ~(CAVE_FLAGS_PERSISTENT));
                         break;
      /* change all options, including 'part of room' etc. */
      case GRID_REPLACE: c_ptr->fdat = flags;
                         break;
      /* add flags to existing flags */
      case GRID_ADD    : c_ptr->fdat |= (flags & ~(CAVE_FLAGS_PERSISTENT));
                         break;
   }
}

/*
 * this changes a grid's feature to a new mtyp,styp pair.
 * for a grid that is in the current view (has CAVE_VIEW)
 * this also changes the memory of that grid
 */
u16b set_grid_type(s16b x, s16b y,u16b mtyp, u16b styp, byte type, u32b flags)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   u16b       f_idx;

#if 0
   if (c_ptr->fdat & CAVE_VIEW)
   {
      c_ptr->memory_mtyp = f_info[f_idx].mtyp;
      c_ptr->memory_styp = f_info[f_idx].styp;
   }
#endif
   c_ptr->mtyp = mtyp;
   c_ptr->styp = styp;

   f_idx = get_f_idx(mtyp, styp);

   set_grid_idx(x, y, f_idx, type, flags);
   return (f_idx);
}

s16b wall_art(s16b styp)
{
   switch (styp)
   {
      case DUNG_WALL_GRANITE:
      case DUNG_WALL_GRAINNR:
      case DUNG_WALL_GRAOUTR:
      case DUNG_WALL_GRHIDTR:
      case DUNG_WALL_GRTREAS:
      case DUNG_WALL_GRSOLID: return (DUNG_WALL_ART_GRANITE);

      case DUNG_WALL_QUARTZ:
      case DUNG_WALL_QUAINNR:
      case DUNG_WALL_QUAOUTR:
      case DUNG_WALL_QUHIDTR:
      case DUNG_WALL_QUTREAS:
      case DUNG_WALL_QUSOLID: return (DUNG_WALL_ART_QUARTZ);

      case DUNG_WALL_RUBBLE:  return (DUNG_WALL_ART_RUBBLE);

      case DUNG_WALL_MAGMA:
      case DUNG_WALL_MGAINNR:
      case DUNG_WALL_MGAOUTR:
      case DUNG_WALL_MGHIDTR:
      case DUNG_WALL_MGTREAS:
      case DUNG_WALL_MGSOLID: return (DUNG_WALL_ART_MAGMA);

      case DUNG_WALL_CHALK:
      case DUNG_WALL_CHKINNR:
      case DUNG_WALL_CHKOUTR:
      case DUNG_WALL_CHHIDTR:
      case DUNG_WALL_CHTREAS:
      case DUNG_WALL_CHSOLID: return (DUNG_WALL_ART_CHALK);
   }
   return (-1);
}

bool inner_wall(s16b mtyp, s16b styp)
{
   if ((mtyp == DUNG_PERWALL) && (styp == DUNG_PERWALL_INNER)) return (TRUE);
   if (mtyp != DUNG_WALL) return FALSE;

   switch (styp)
   {
      case DUNG_WALL_GRAINNR:
      case DUNG_WALL_QUAINNR:
      case DUNG_WALL_MGAINNR:
      case DUNG_WALL_CHKINNR: return (TRUE);
   }
   return (FALSE);
}

bool hidden_treasure(s16b x, s16b y)
{
   if (test_grid(x, y, DUNG_WALL, DUNG_WALL_QUHIDTR) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_GRHIDTR) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_MGHIDTR) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_CHHIDTR)) return(TRUE);
   return(FALSE);
}


bool treasure(s16b x, s16b y)
{
   if (hidden_treasure(x, y) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_QUTREAS) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_GRTREAS) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_MGTREAS) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_CHTREAS)) return(TRUE);
   return(FALSE);
}

bool known_treasure(s16b x, s16b y)
{
   if (test_grid(x, y, DUNG_WALL, DUNG_WALL_QUTREAS) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_GRTREAS) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_MGTREAS) ||
       test_grid(x, y, DUNG_WALL, DUNG_WALL_CHTREAS)) return(TRUE);
   return(FALSE);
}


/* jk */
/*
 * Is a given location "valid" for placing things?
 *
 * Permanent walls, stairs, store doors are never "valid".
 *
 * Hack -- a grid with an artifact in it is never valid.
 *
 * This function is often "combined" with "floor_grid_bold(x,y)"
 * or one of the other similar "functions".
 *
 * Line 1 -- forbid perma-walls
 * Line 2 -- forbid stairs
 * Line 3 -- forbid store doors
 * Line 4 -- forbid artifact grids
 */
bool valid_grid_bold(s16b x, s16b y)
{
   s16b j;
   object_type *i_ptr;
   s16b objs = objects_on_floor(x,y);

   if (dungeon.level[sublevel][y][x].mtyp==DUNG_PERWALL) return (FALSE);
   if (dungeon.level[sublevel][y][x].mtyp==DUNG_STAIR) return (FALSE);
   if (dungeon.level[sublevel][y][x].mtyp==DUNG_ENTR) return (FALSE);
   if (objs == 0) return TRUE;
   j=0;
   while (j<objs)
   {
     i_ptr = get_item_pointer_floor_xy(j,x,y);
     if (artifact_p(i_ptr)) return(FALSE);
     j++;
   }
   return (TRUE);
}

/*
 * Approximate Distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
 */
u16b distance(s16b x1, s16b y1, s16b x2, s16b y2)
{
   u16b dx, dy, d;

   /* Find the absolute x/y distance components */
   dx = ((u16b)x1 > (u16b)x2) ? ((u16b)x1 - (u16b)x2) : ((u16b)x2 - (u16b)x1);
   dy = ((u16b)y1 > (u16b)y2) ? ((u16b)y1 - (u16b)y2) : ((u16b)y2 - (u16b)y1);

   /* Hack -- approximate the distance */
   d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

   /* Return the distance */
   return (d);
}

/*
 * A simple, fast, integer-based line-of-sight algorithm.  By Joseph Hall,
 * 4116 Brewster Drive, Raleigh NC 27606.  Email to jnh@ecemwl.ncsu.edu.
 *
 * Returns TRUE if a line of sight can be traced from (x1,y1) to (x2,y2).
 *
 * The LOS begins at the center of the tile (x1,y1) and ends at the center of
 * the tile (x2,y2).  If los() is to return TRUE, all of the tiles this line
 * passes through must be floor tiles, except for (x1,y1) and (x2,y2).
 *
 * We assume that the "mathematical corner" of a non-floor tile does not
 * block line of sight.
 *
 * Because this function uses (short) ints for all calculations, overflow may
 * occur if dx and dy exceed 90.
 *
 * Once all the degenerate cases are eliminated, the values "qx", "qy", and
 * "m" are multiplied by a scale factor "f1 = abs(dx * dy * 2)", so that
 * we can use integer arithmetic.
 *
 * We travel from start to finish along the longer axis, starting at the border
 * between the first and second tiles, where the y offset = .5 * slope, taking
 * into account the scale factor.  See below.
 *
 * Also note that this function and the "move towards target" code do NOT
 * share the same properties.  Thus, you can see someone, target them, and
 * then fire a bolt at them, but the bolt may hit a wall, not them.  However,
 * by clever choice of target locations, you can sometimes throw a "curve".
 *
 * Note that "line of sight" is not "reflexive" in all cases.
 *
 * Use the "projectable()" routine to test "spell/missile line of sight".
 *
 * Use the "update_view()" function to determine player line-of-sight.
 */
bool los(s16b x1, s16b y1, s16b x2, s16b y2)
{
   s16b dx, dy;    /* Delta */
   u16b ax, ay;    /* Absolute */
   s16b sx, sy;    /* Signs */
   s16b qx, qy;    /* Fractions */
   s16b tx, ty;    /* Scanners */
   u16b f1, f2;    /* Scale factors */
   u16b m;         /* Slope, or 1/Slope, of LOS */
   dy = y2 - y1;   /* Extract the offset */
   dx = x2 - x1;
   ay = (u16b)ABS(dy);    /* Extract the absolute offset */
   ax = (u16b)ABS(dx);

   /* Handle adjacent (or identical) grids */
   if ((ax < 2) && (ay < 2)) return (TRUE);

   /* Paranoia -- require "safe" origin */
   /* if (!in_bounds(x1, y1)) return (FALSE); */

dlog(DEBUGLOS,"cave.c: los: %d,%d to %d,%d dx %d dy %d\n",
              x1, y1, x2, y2, dx, dy);
   if (dx==0)               /* Directly South/North */
   {
      if (dy > 0)        /* South -- check for walls */
      {
         for (ty = y1 + 1; ty < y2; ty++)
         {
dlog(DEBUGLOS,"cave.c: los: N/S,S %d,%d (%s) bold %d\n",
              x1, ty, feat_name(x1,ty), floor_grid_bold(x1, ty));
            if (!floor_grid_bold(x1,ty)) return (FALSE);
         }
      }
      else               /* North -- check for walls */
      {
         for (ty = y1 - 1; ty > y2; ty--)
         {
dlog(DEBUGLOS,"cave.c: los: N/S,N %d,%d (%s) bold %d\n",
              x1, ty, feat_name(x1,ty), floor_grid_bold(x1, ty));
            if (!floor_grid_bold(x1,ty)) return (FALSE);
         }
      }
      return (TRUE);        /* Assume los */
   }

   if (dy==0)    /* Directly East/West */
   {
      if (dx > 0)        /* East -- check for walls */
      {
         for (tx = x1 + 1; tx < x2; tx++)
         {
dlog(DEBUGLOS,"cave.c: los: E/W,E %d,%d (%s) bold %d\n",
              tx, y1, feat_name(tx,y1), floor_grid_bold(tx, y1));
            if (!floor_grid_bold(tx,y1)) return (FALSE);
         }
      }

      else        /* West -- check for walls */
      {
         for (tx = x1 - 1; tx > x2; tx--)
         {
dlog(DEBUGLOS,"cave.c: los: E/W,W %d,%d (%s) bold %d\n",
              tx, y1, feat_name(tx,y1), floor_grid_bold(tx, y1));
            if (!floor_grid_bold(tx,y1)) return (FALSE);
         }
      }

      return (TRUE);        /* Assume los */
   }

   sx = (dx < 0) ? (s16b)-1 : (s16b)1;    /* Extract some signs */
   sy = (dy < 0) ? (s16b)-1 : (s16b)1;

dlog(DEBUGLOS,"cave.c: los: sx %d sy %d ax %d ay %d\n", sx, sy, ax, ay);

   if (ax == 1)    /* Vertical "knights" */
   {
      if (ay == 2)
      {
dlog(DEBUGLOS,"cave.c: los: VKN %d,%d (%s) bold %d\n",
              x1, y1+sy, feat_name(x1,y1+sy), floor_grid_bold(x1, y1+sy));
         if (floor_grid_bold(x1,y1 + sy)) return (TRUE);
      }
   }

   else if (ay == 1)    /* Horizontal "knights" */
   {
      if (ax == 2)
      {
dlog(DEBUGLOS,"cave.c: los: VKN %d,%d (%s) bold %d\n",
              x1+sx, y1, feat_name(x1+sx,y1), floor_grid_bold(x1+sx, y1));
         if (floor_grid_bold(x1 + sx,y1)) return (TRUE);
      }
   }

   f2 = (ax * ay);    /* Calculate scale factor div 2 */
   f1 = f2 << 1;    /* Calculate scale factor */

dlog(DEBUGLOS,"cave.c: los: f1 %d f2 %d ax %d ay %d\n", f1, f2, ax, ay);
   if (ax >= ay)    /* Travel horizontally */
   {
      /* Let m = dy / dx * 2 * (dy * dx) = 2 * dy * dy */
      qy = ay * ay;
      m = qy << 1;

      tx = x1 + sx;
dlog(DEBUGLOS,"cave.c: los: horizontal: qy %d m %d tx %d\n", qy, m, tx);

      /* Consider the special case where slope == 1. */
      if (qy == f2)
      {
         ty = y1 + sy;
         qy -= f1;
      }
      else
      {
         ty = y1;
      }

      /* Note (below) the case (qy == f2), where */
      /* the LOS exactly meets the corner of a tile. */
      while ((x2 - tx)!=0)
      {
dlog(DEBUGLOS,"cave.c: los: %d-%d !=0: hor1 %d,%d (%s) bold %d\n",
              x2, tx, tx, ty, feat_name(tx,ty), floor_grid_bold(tx, ty));
         if (!floor_grid_bold(tx,ty)) return (FALSE);

         qy += m;
dlog(DEBUGLOS,"cave.c: los: qy %d f2 %d\n", qy, f2);

         if (qy < f2)
         {
            tx += sx;
         }
         else if (qy > f2)
         {
            ty += sy;
dlog(DEBUGLOS,"cave.c: los: hor2 %d,%d (%s) bold %d\n",
              tx, ty, feat_name(tx,ty), floor_grid_bold(tx, ty));
            if (!floor_grid_bold(tx,ty)) return (FALSE);
            qy -= f1;
            tx += sx;
         }
         else
         {
            ty += sy;
            qy -= f1;
            tx += sx;
         }
      }
   }

   /* Travel vertically */
   else
   {
      /* Let m = dx / dy * 2 * (dx * dy) = 2 * dx * dx */
      qx = ax * ax;
      m = qx << 1;

      ty = y1 + sy;

dlog(DEBUGLOS,"cave.c: los: vertical: qx %d m %d ty %d f2 %d\n", qx, m, ty, f2);
      if (qx == f2)
      {
         tx = x1 + sx;
         qx -= f1;
      }
      else
      {
         tx = x1;
      }

      /* Note (below) the case (qx == f2), where */
      /* the LOS exactly meets the corner of a tile. */
      while ((y2 - ty)!=0)
      {
dlog(DEBUGLOS,"cave.c: los: ver1 %d,%d (%s) bold %d\n",
              tx, ty, feat_name(tx,ty), floor_grid_bold(tx, ty));
         if (!floor_grid_bold(tx,ty)) return (FALSE);

         qx += m;
dlog(DEBUGLOS,"cave.c: los: qx %d f2 %d\n", qx, f2);

         if (qx < f2)
         {
            ty += sy;
         }
         else if (qx > f2)
         {
            tx += sx;
dlog(DEBUGLOS,"cave.c: los: ver2 %d,%d (%s) bold %d\n",
              tx, ty, feat_name(tx,ty), floor_grid_bold(tx, ty));
            if (!floor_grid_bold(tx,ty)) return (FALSE);
            qx -= f1;
            ty += sy;
         }
         else
         {
            tx += sx;
            qx -= f1;
            ty += sy;
         }
      }
   }

dlog(DEBUGLOS,"cave.c: los: TRUE\n");

   /* Assume los */
   return (TRUE);
}

/*
 * Can the player "see" the given grid in detail?
 *
 * He must have vision, illumination, and line of sight.
 *
 * Note -- "CAVE_LITE" is only set if the "torch" has "los()".
 * So, given "CAVE_LITE", we know that the grid is "fully visible".
 *
 * Note that "CAVE_GLOW" makes little sense for a wall, since it would mean
 * that a wall is visible from any direction.  That would be odd.  Except
 * under wizard light, which might make sense.  Thus, for walls, we require
 * not only that they be "CAVE_GLOW", but also, that they be adjacent to a
 * grid which is not only "CAVE_GLOW", but which is a non-wall, and which is
 * in line of sight of the player.
 *
 * This extra check is expensive, but it provides a more "correct" semantics.
 *
 * Note that we should not run this check on walls which are "outer walls" of
 * the dungeon, or we will induce a memory fault, but actually verifying all
 * of the locations would be extremely expensive.
 *
 * Thus, to speed up the function, we assume that all "perma-walls" which are
 * "CAVE_GLOW" are "illuminated" from all sides.  This is correct for all cases
 * except "vaults" and the "buildings" in town.  But the town is a hack anyway,
 * and the player has more important things on his mind when he is attacking a
 * monster vault.  It is annoying, but an extremely important optimization.
 *
 * We could check the four adjacent neighbors instead of all eight, but this
 * would cause the "corners" of illuminated rooms to appear "dark".
 */
bool player_can_see_bold(s16b x, s16b y)
{
   s16b xx,yy;

   cave_cell_type *c_ptr;

   /* Blind players see nothing */
   if (p_ptr->blind>0) return (FALSE);

   /* Access the cave grid */
   c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGLIGHT,"cave.c: see_bold @ %d,%d player at %d,%d\n", x, y, px, py);
dlog(DEBUGLIGHT,"                 mtyp %d styp %d fdat %08lx n %s\n",
                c_ptr->mtyp, c_ptr->styp, c_ptr->fdat,
                f_name+f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name);
   /* Note that "torch-lite" yields "illumination" */
   if ((c_ptr->fdat & CAVE_LITE)>0L) return (TRUE);
dlog(DEBUGLIGHT,"cave.c: see_bold: not CAVE_LITE\n");
   /* Require line of sight to the grid */
   if (!player_has_los_bold(x, y)) return (FALSE);
dlog(DEBUGLIGHT,"cave.c: see_bold: not los\n");
   /* Require "perma-lite" of the grid */
   if ((c_ptr->fdat & CAVE_GLOW)==0L) return (FALSE);
dlog(DEBUGLIGHT,"cave.c: see_bold: CAVE_GLOW\n");
   /* Floors are simple */
   if (floor_grid_bold(x, y)) return (TRUE);
dlog(DEBUGLIGHT,"cave.c: see_bold: not floor_grid_bold\n");
   /* Hack -- move towards player */
   xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;
   yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;

   /* Check for "local" illumination */
   if ((dungeon.level[sublevel][yy][xx].fdat & CAVE_GLOW)>0L)
   {
      /* Assume the wall is really illuminated */
      return (TRUE);
   }
dlog(DEBUGLIGHT,"cave.c: see_bold: not logical CAVE_GLOW\n");
   /* Assume not visible */
   return (FALSE);
}

/*
 * Returns true if the player's grid is dark
 */
bool no_lite(void)
{
   return (!player_can_see_bold(px, py));
}

/*
 * Hack -- Legal monster codes
 */
static cptr image_monster_hack = \
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

/*
 * Mega-Hack -- Hallucinatory monster
 */
static void image_monster(byte *ap, char *cp, byte *sp)
{
   s16b i;

   /* Pick a symbol */
   i = (s16b)rand_int((s32b)strlen(image_monster_hack));

   /* Random letter */
   (*cp) = (image_monster_hack[i]);

   /* Random color */
   (*ap) = (byte)randint(15);
/* jk */
/* this will probably not be right, but the only other option is to choose */
/* a random, existing object - which would be slower                       */
   (*sp) = 0;
}

/*
 * Hack -- Legal object codes
 */
static cptr image_object_hack = \
    "?/|\\\"!$()_-=[]{},~";

/*
 * Mega-Hack -- Hallucinatory object
 */
static void image_object(byte *ap, char *cp, byte *sp)
{
   s16b i;

   /* Pick a symbol */
   i = (s16b)rand_int((s32b)strlen(image_object_hack));

   /* Random letter */
   (*cp) = (image_object_hack[i]);

   /* Random color */
   (*ap) = (byte)randint(15);

/* jk */
/* this will probably not be right, but the only other option is to choose */
/* a random, existing object - which would be slower                       */
   (*sp) = 0;
}

/*
 * Hack -- Random hallucination
 */
static void image_random(byte *ap, char *cp, byte *sp)
{
   /* Normally, assume monsters */
   if (rand_int(100) < 75)
   {
      image_monster(ap, cp, sp);
   }

   /* Otherwise, assume objects */
   else
   {
      image_object(ap, cp, sp);
   }
}

static byte handle_view_bright_lite(s16b x, s16b y, s16b ap, u32b fdat)
{
   byte result;

   result = (byte)ap;
   /* Not viewable */
   if ((fdat & CAVE_VIEW)==0L)
   {
      /* Use "gray" */
      result = (byte)TERM_SLATE;
   }

   /* Not glowing */
   else if (!(fdat & (CAVE_GLOW)))
   {
      /* Use "gray" */
      result = (byte)TERM_SLATE;
   }

   /* Not glowing correctly */
   else
   {
      s16b xx, yy;

      /* Hack -- move towards player */
      yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
      xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;

      /* Check for "local" illumination */
      if (!(dungeon.level[sublevel][yy][xx].fdat & (CAVE_GLOW)))
      {
         /* Use "gray" */
         result = (byte)TERM_SLATE;
      }
   }
   return(result);
}

/*
 * Extract the attr/char to display at the given (legal) map location
 *
 * Basically, we "paint" the chosen attr/char in several passes, starting
 * with any known "terrain features" (defaulting to darkness), then adding
 * any known "objects", and finally, adding any known "monsters".  This
 * is not the fastest method but since most of the calls to this function
 * are made for grids with no monsters or objects, it is fast enough.
 *
 * Note that this function, if used on the grid containing the "player",
 * will return the attr/char of the grid underneath the player, and not
 * the actual player attr/char itself, allowing a lot of optimization
 * in various "display" functions.
 *
 * Note that the "zero" entry in the feature/object/monster arrays are
 * used to provide "special" attr/char codes, with "monster zero" being
 * used for the player attr/char, "object zero" being used for the "stack"
 * attr/char, and "feature zero" being used for the "nothing" attr/char,
 * though this function makes use of only "feature zero".
 *
 * Note that monsters can have some "special" flags, including "ATTR_MULTI",
 * which means their color changes, and "ATTR_CLEAR", which means they take
 * the color of whatever is under them, and "CHAR_CLEAR", which means that
 * they take the symbol of whatever is under them.  Technically, the flag
 * "CHAR_MULTI" is supposed to indicate that a monster looks strange when
 * examined, but this flag is currently ignored.  All of these flags are
 * ignored if the "avoid_other" option is set, since checking for these
 * conditions is expensive and annoying on some systems.
 *
 * Currently, we do nothing with multi-hued objects, because there are
 * not any.  If there were, they would have to set "shimmer_objects"
 * when they were created, and then new "shimmer" code in "dungeon.c"
 * would have to be created handle the "shimmer" effect, and the code
 * in "cave.c" would have to be updated to create the shimmer effect.
 *
 * Note the effects of hallucination.  Objects always appear as random
 * "objects", monsters as random "monsters", and normal grids occasionally
 * appear as random "monsters" or "objects", but note that these random
 * "monsters" and "objects" are really just "colored ascii symbols".
 *
 * Note that "floors" and "invisible traps" (and "zero" features) are
 * drawn as "floors" using a special check for optimization purposes,
 * and these are the only features which get drawn using the special
 * lighting effects activated by "view_special_lite".
 *
 * Note the use of the "mimic" field in the "terrain feature" processing,
 * which allows any feature to "pretend" to be another feature.  This is
 * used to "hide" secret doors, and to make all "doors" appear the same,
 * and all "walls" appear the same, and "hidden" treasure stay hidden.
 * It is possible to use this field to make a feature "look" like a floor,
 * but the "special lighting effects" for floors will not be used.
 *
 * Note the use of the new "terrain feature" information.  Note that the
 * assumption that all interesting "objects" and "terrain features" are
 * memorized allows extremely optimized processing below.  Note the use
 * of separate flags on objects to mark them as memorized allows a grid
 * to have memorized "terrain" without granting knowledge of any object
 * which may appear in that grid.
 *
 * Note the efficient code used to determine if a "floor" grid is
 * "memorized" or "viewable" by the player, where the test for the
 * grid being "viewable" is based on the facts that (1) the grid
 * must be "lit" (torch-lit or perma-lit), (2) the grid must be in
 * line of sight, and (3) the player must not be blind, and uses the
 * assumption that all torch-lit grids are in line of sight.
 *
 * Note that floors (and invisible traps) are the only grids which are
 * not memorized when seen, so only these grids need to check to see if
 * the grid is "viewable" to the player (if it is not memorized).  Since
 * most non-memorized grids are in fact walls, this induces *massive*
 * efficiency, at the cost of *forcing* the memorization of non-floor
 * grids when they are first seen.  Note that "invisible traps" are
 * always treated exactly like "floors", which prevents "cheating".
 *
 * Note the "special lighting effects" which can be activated for floor
 * grids using the "view_special_lite" option (for "white" floor grids),
 * causing certain grids to be displayed using special colors.  If the
 * player is "blind", we will use "dark gray", else if the grid is lit
 * by the torch, and the "view_yellow_lite" option is set, we will use
 * "yellow", else if the grid is "dark", we will use "dark gray", else
 * if the grid is not "viewable", and the "view_bright_lite" option is
 * set, and the we will use "slate" (gray).  We will use "white" for all
 * other cases, in particular, for illuminated viewable floor grids.
 *
 * Note the "special lighting effects" which can be activated for wall
 * grids using the "view_granite_lite" option (for "white" wall grids),
 * causing certain grids to be displayed using special colors.  If the
 * player is "blind", we will use "dark gray", else if the grid is lit
 * by the torch, and the "view_yellow_lite" option is set, we will use
 * "yellow", else if the "view_bright_lite" option is set, and the grid
 * is not "viewable", or is "dark", or is glowing, but not when viewed
 * from the player's current location, we will use "slate" (gray).  We
 * will use "white" for all other cases, in particular, for correctly
 * illuminated viewable wall grids.
 *
 * Note that, when "view_granite_lite" is set, we use an inline version
 * of the "player_can_see_bold()" function to check the "viewability" of
 * grids when the "view_bright_lite" option is set, and we do NOT use
 * any special colors for "dark" wall grids, since this would allow the
 * player to notice the walls of illuminated rooms from a hallway that
 * happened to run beside the room.  The alternative, by the way, would
 * be to prevent the generation of hallways next to rooms, but this
 * would still allow problems when digging towards a room.
 *
 * Note that bizarre things must be done when the "attr" and/or "char"
 * codes have the "high-bit" set, since these values are used to encode
 * various "special" pictures in some versions, and certain situations,
 * such as "multi-hued" or "clear" monsters, cause the attr/char codes
 * to be "scrambled" in various ways.
 *
 * Note that eventually we may use the "&" symbol for embedded treasure,
 * and use the "*" symbol to indicate multiple objects, though this will
 * have to wait for Angband 2.8.0 or later.  Note that currently, this
 * is not important, since only one object or terrain feature is allowed
 * in each grid.  If needed, "k_info[0]" will hold the "stack" attr/char.
 *
 * Note the assumption that doing "x_ptr = &x_info[x]" plus a few of
 * "x_ptr->xxx", is quicker than "x_info[x].xxx", if this is incorrect
 * then a whole lot of code should be changed...  XXX XXX
 */
static void map_info(s16b x, s16b y, byte *ap, char *cp, byte *sp, u16b mode)
{
   cave_cell_type *c_ptr;
   s16b f_idx;
   u16b mtyp, styp;
   c_ptr = &dungeon.level[sublevel][y][x];

   /* initialise: Normal attr */
   (*ap) = (f_info[DUNG_NOTHING].x_attr & 0x0F);
   (*sp) = (f_info[DUNG_NOTHING].x_attr & 0xF0);

   /* Normal char */
   (*cp) = f_info[DUNG_NOTHING].x_char;
dlog(DEBUGLIGHT,"cave.c: map_info: px, py %d,%d @ %d,%d entering with cp %c=%d, ap = %d mt %d st %d\n",
          px, py, x, y, (*cp), (*cp), (*ap), c_ptr->mtyp, c_ptr->styp);
dlog(DEBUGLIGHT,"                  fdat %08lx %s m_idx %d (%s) t_idx %d\n", c_ptr->fdat,
          f_name+f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name, c_ptr->m_idx,
          c_ptr->m_idx>0?(r_name + r_info[mn_list[c_ptr->m_idx].r_idx].name):"none", c_ptr->t_idx);

   /* unseen square? */
   mtyp = c_ptr->mtyp;
   styp = c_ptr->styp;

   /* first find out what feature we have */
   f_idx = get_f_idx(mtyp, styp);
dlog(DEBUGLIGHT,"cave.c: map_info: mt %d st %d f_idx %d\n", mtyp, styp, f_idx);

   /* and if it should look like something else */
   if (f_info[f_idx].flags & CAVE_MIMIC)
   {
      f_idx = get_f_idx(f_info[f_idx].mim_m, f_info[f_idx].mim_s);
dlog(DEBUGLIGHT,"cave.c: map_info: f_idx now %d\n", f_idx);
   }

/* jk - lets start out with the terrain feature for floors and wilderness */
   if ((f_info[f_idx].mtyp==DUNG_FLOOR) &&
       (f_info[f_idx].styp==DUNG_FLOOR_NORMAL))
   {
dlog(DEBUGLIGHT,"cave.c: map_info: normal floor\n");
      /* if we can see and we know something is there */
      if ((!p_ptr->blind) && (c_ptr->fdat & (CAVE_MARK|CAVE_LITE)))
      {
         /* Normal char */
         (*cp) = f_info[f_idx].x_char;

         /* Normal attr */
         (*ap) = f_info[f_idx].x_attr & 0x0F;
         (*sp) = f_info[f_idx].x_attr & 0xF0;

         /* closest by: torch-lit floor */
         if (c_ptr->fdat & CAVE_LITE)
         {
            if (view_yellow_lite) (*ap)=TERM_YELLOW;
            else if (view_bright_lite) (*ap)=TERM_L_WHITE;
dlog(DEBUGLIGHT,"cave.c: map_info: floor CAVE_LITE: ap %d yellow %d bright %d\n",
                (*ap), view_yellow_lite, view_bright_lite);
         }
         else if (c_ptr->fdat & CAVE_VIEW)
         {
            if (view_bright_lite) (*ap)=TERM_L_WHITE;
dlog(DEBUGLIGHT,"cave.c: map_info: floor CAVE_VIEW: ap %d bright %d\n",
                (*ap), view_bright_lite);
         }
      }
   }

   /* Non floors */
/* jk - note this code will go wrong if you know there's a trap on */
/* a secret door: you will get a wall colored like a trap */
   else
   {
      /* Memorized grids */
#if 0
      if (c_ptr->fdat & (CAVE_MARK))
#endif
      if (c_ptr->fdat & (CAVE_MARK))
      {
         /* Normal char */
         (*cp) = f_info[f_idx].x_char;

         /* Normal attr */
         (*ap) = f_info[f_idx].x_attr & 0x0F;
         (*sp) = f_info[f_idx].x_attr & 0xF0;

dlog(DEBUGLIGHT,"cave.c: map_info: non floor cp %c=%d ap %d\n", (*cp), (*cp), (*ap));
         /* traps have their own colors */
         if (c_ptr->t_idx)
         {
            trap_item_type *tr_ptr;
            s16b trap;
            tr_ptr = &t_list[c_ptr->t_idx];

            /* if we have found more, we use the color with that feature! */
            if (!test_grid_ptr(c_ptr, DUNG_TRAP, DUNG_TRAP_FNDMORE))
            {

               /* note that squares with different known traps will have */
               /* different colors each square */
               trap = first_found_trap(tr_ptr);
               if (trap>=0) /* else <0 no trap found */
               {
                  (*ap)=t_info[trap].color;
dlog(DEBUGLIGHT,"cave.c: map_info: known trap %d typ %08Lx fd %08Lx ap %d\n",
                                   trap, tr_ptr->type, tr_ptr->found, (*ap));
               }
            }
         }

         /* Special lighting effects for walls */
         if (view_granite_lite && is_wall_ptr(c_ptr))
         {
            if (p_ptr->blind)                   /* Handle "blind" */
            {
               (*ap) = TERM_L_DARK;             /* Use "dark gray" */

dlog(DEBUGLIGHT,"cave.c: map_info: view_granite_lite blind ap=%d\n", (*ap));
            }
            else if (c_ptr->fdat & (CAVE_LITE)) /* Handle "torch-lit" grids */
            {
               if (view_yellow_lite)            /* Torch lite */
               {
                  (*ap) = TERM_YELLOW;          /* Use "yellow" */

dlog(DEBUGLIGHT,"cave.c: map_info: view_granite_lite cave_lite view_yellow_lite ap=%d\n", (*ap));
               }
            }
            else if (view_bright_lite)          /* Handle "view_bright_lite" */
            {
               handle_view_bright_lite(x, y, (*ap), c_ptr->fdat);
dlog(DEBUGLIGHT,"cave.c: map_info: view_granite_lite view_bright_lite ap=%d\n", (*ap));
            }
         }
      }
   }

   /* Hack -- rare random hallucination, except on outer dungeon walls */
   if ( p_ptr->image && (!rand_int(256)) &&
        (mtyp != DUNG_PERWALL) && (styp != DUNG_PERWALL_SOLID) )
   {
      /* Hallucinate */
      image_random(ap, cp, sp);

dlog(DEBUGLIGHT,"cave.c: map_info: hallucinate cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
   }

   /* Handle "player" */
   if ((x == px) && (y == py))
   {
      monster_race *r_ptr = &r_info[0];

      /* Get the "player" char */
      (*cp) = r_ptr->x_char;

      /* Get the "player" attr */
      (*ap) = r_ptr->x_attr & 0x0F;
      (*sp) = r_ptr->x_attr & 0xF0;

dlog(DEBUGLIGHT,"cave.c: map_info: player cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));

      /* Done */
dlog(DEBUGLIGHT,"cave.c: map_info: returning1 with cp %c=%d ap %d\n", (*cp), (*cp), (*ap));
      return;
   }

   /* in the wilderness, only view 1 square away, unless mode & LITE_DETECT */
   /* which is used by detect()                                             */
   /* BUGBUGBUG: this also interferes with telepathy, I think               */
   if (!(mode & LITE_DETECT) && (mtyp==DUNG_SHRUB))
   {
      if (distance(x,y,px,py)>1)
      {
dlog(DEBUGLIGHT,"cave.c: map_info: returning2 with cp %c=%d ap %d\n", (*cp), (*cp), (*ap));
         return;
      }
   }

   /* Objects */
/* jk */
/* if no item_set_flag, we have 1 or 0 object */
   if (!(c_ptr->i_idx & ITEM_SET_FLAG) && (c_ptr->i_idx))
   {
      object_type *i_ptr = get_item_pointer_floor_xy(0,x,y);
dlog(DEBUGLIGHT,"cave.c: map_info: single object found at %d,%d\n", x, y);
      /* Memorized objects */
      if (i_ptr->marked)
      {
         /* Normal char */
         (*cp) = object_char(i_ptr);

         /* Normal attr */
         (*ap) = object_attr(i_ptr) & 0x0F;
         (*sp) = object_attr(i_ptr) & 0xF0;
dlog(DEBUGLIGHT,"cave.c: map_info: object marked normal k_idx %d cp %d ap=%d (%s) flavor %d\n",
                i_ptr->k_idx, (*cp), (*ap), k_name + k_info[i_ptr->k_idx].name, k_info[i_ptr->k_idx].flavor);
dlog(DEBUGLIGHT,"cave.c: map_info:                      k_ptr->d_char %d k_ptr->x_char %d aware %d\n",
                k_info[i_ptr->k_idx].d_char, k_info[i_ptr->k_idx].x_char, k_info[i_ptr->k_idx].aware);
dlog(DEBUGLIGHT,"cave.c: map_info:                      k_ptr->d_attr %d (%s) k_ptr->x_attr %d (%s)\n",
                k_info[i_ptr->k_idx].d_attr, color_names[k_info[i_ptr->k_idx].d_attr],
                k_info[i_ptr->k_idx].x_attr, color_names[k_info[i_ptr->k_idx].x_attr]);
if (k_info[i_ptr->k_idx].flavor)
{
   s16b flavor = k_info[i_ptr->k_idx].flavor;
dlog(DEBUGLIGHT,"cave.c: map_info: flavor %d            misc_to_char %d misc_to_attr %d object_attr %d\n",
                flavor, misc_to_char[flavor], misc_to_attr[flavor], object_attr(i_ptr));
dlog(DEBUGLIGHT,"cave.c: map_info: my_test_object_attr: %d\n",
                ( (k_info[i_ptr->k_idx].flavor) ? (misc_to_attr[flavor]) : \
                ( (k_info[i_ptr->k_idx].x_attr) ? (k_info[i_ptr->k_idx].x_attr) : (k_info[i_ptr->k_idx].d_attr) ) ) );
                
}


         /* Hack -- Handle hallucination on "normal" objects */
         if (p_ptr->image)
         {
            /* Hallucinate */
            image_object(ap, cp, sp);
dlog(DEBUGLIGHT,"cave.c: map_info: object marked hallucination cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
         }

/* make sure 1 item corpse gets a different color for each 'flavour' */
/* else we need 500+ items 'corpse' - which seems a waste */
         if (i_ptr->tval == TV_CORPSE)
         {
            (*ap) = r_info[i_ptr->sval].x_attr & 0x0F;
            (*sp) = r_info[i_ptr->sval].x_attr & 0xF0;

dlog(DEBUGLIGHT,"cave.c: map_info: object marked corpse cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
         }

         else if (i_ptr->tval == TV_SKELETON)
         {
            if (i_ptr->sval >= SV_SKELETON_BROKEN_END)
            {
               (*ap) = r_info[i_ptr->sval-SV_SKELETON_BROKEN_END].x_attr & 0x0F;
               (*sp) = r_info[i_ptr->sval-SV_SKELETON_BROKEN_END].x_attr & 0xF0;

dlog(DEBUGLIGHT,"cave.c: map_info: object marked skeleton cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
            }
         }
      }
   }
   /* we have a stack of items */
   else if (c_ptr->i_idx>=ITEM_SET_FLAG)
   {
      object_type *i_ptr = get_item_pointer_floor_xy(0,x,y);

      /* Normal char */
/* jk - this really is a hack */
      (*cp) = '*';

      /* Normal attr */
/* this wil give every * the color of the first item */
      (*ap) = object_attr(i_ptr) & 0x0F;
      (*sp) = object_attr(i_ptr) & 0xF0;

dlog(DEBUGLIGHT,"cave.c: map_info: objects normal cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));

      if (p_ptr->image)
      {
         /* Hallucinate */
         image_object(ap, cp, sp);

dlog(DEBUGLIGHT,"cave.c: map_info: objects hallucination cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
      }
   }

   /* Handle monsters, skip invisible ones */
   if (c_ptr->m_idx)
   {
      monster_type *m_ptr = &mn_list[c_ptr->m_idx];
dlog(DEBUGLIGHT,"cave.c: map_info: monster m_idx %d ml %d\n", c_ptr->m_idx, m_ptr->ml);
      if (m_ptr->ml)
      {
         monster_race *r_ptr = &r_info[m_ptr->r_idx];
         byte da, ds;
         char dc;

         /* Normal char/attr/special attr */
         dc = r_ptr->x_char;
         da = r_ptr->x_attr & 0x0F;
         ds = r_ptr->x_attr & 0xF0;

         /* hallucinated monster */
         if (p_ptr->image)
         {
            /* Hallucinatory monster */
            image_monster(ap, cp, sp);
dlog(DEBUGLIGHT,"cave.c: map_info: hallucinated monster cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
         }

         /* multi-hued monster */
         else if (r_ptr->flags1 & RF1_ATTR_MULTI)
         {
            /* Multi-hued attr, the rest is normal */
            (*cp) = dc;
            (*ap) = randint(15);
            (*sp) = ds;
dlog(DEBUGLIGHT,"cave.c: map_info: multi-color monster cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
         }

         /* normal monster */
         else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR | RF1_CHAR_CLEAR)))
         {
            (*cp) = dc;
            (*ap) = da;
            (*sp) = ds;
dlog(DEBUGLIGHT,"cave.c: map_info: normal monster cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
         }

         /* Normal char, Clear attr, monster */
         else if (!(r_ptr->flags1 & (RF1_CHAR_CLEAR)))
         {
            (*cp) = dc;
dlog(DEBUGLIGHT,"cave.c: map_info: !CHAR_CLEAR monster cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
         }

         /* Normal attr, Clear char, monster */
         else if (!(r_ptr->flags1 & (RF1_ATTR_CLEAR)))
         {
            (*ap) = da;
            (*sp) = ds;
dlog(DEBUGLIGHT,"cave.c: map_info: !ATTR_CLEAR monster cp %c=%d ap=%d\n", (*cp), (*cp), (*ap));
         }
      }
      return;
   }

dlog(DEBUGLIGHT,"cave.c: map_info: returning11 with cp %c=%d ap %d\n", (*cp), (*cp), (*ap));
}

/*
 * Moves the cursor to a given MAP (x,y) location
 */
void move_cursor_relative(s16b col, s16b row)
{
   /* Real co-ords convert to screen positions */
   col -= panel_prt_col;
   row -= panel_prt_row;

   /* Go there */
   Term_gotoxy(col, row+MAP_ROW);
}

/*
 * Place an attr/char pair at the given map coordinate, if legal.
 */
void print_rel(char c, byte a, s16b x, s16b y)
{
   /* Only do "legal" locations */
   if (panel_contains(x, y))
   {

#ifdef USE_COLOR
      if (!use_color) a = TERM_WHITE;        /* Run-time color choosing */
      /* Draw the char using the attr */
      Term_draw(x-panel_prt_col, (y-panel_prt_row)+MAP_ROW, a, c);
#else
      /* Draw the char (always white) */
      Term_draw(x-panel_prt_col, (y-panel_prt_row)+MAP_ROW, TERM_WHITE, c);
#endif
   }
}

/*
 * This function should only be called on "legal" grids.
 *
 * This function should be called every time the "memorization" of
 * a grid (or the object in a grid) is called into question.
 *
 * Note that the player always memorized all "objects" which are seen,
 * using a different method than the one used for terrain features,
 * which not only allows a lot of optimization, but also prevents the
 * player from "knowing" when objects are dropped out of sight but in
 * memorized grids.
 *
 * Note that the player always memorizes "interesting" terrain features
 * (everything but floors and invisible traps).  This allows incredible
 * amounts of optimization in various places.
 *
 * Note that the player is allowed to memorize floors and invisible
 * traps under various circumstances, and with various options set.
 *
 * This function is slightly non-optimal, since it memorizes objects
 * and terrain features separately, though both are dependant on the
 * "player_can_see_bold()" macro.
 */
void note_spot(s16b x, s16b y)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
/* jk */
   s16b j = objects_on_floor(x,y)-1;

   /* requre CAVE_LIGHT flag */
   if (!(c_ptr->fdat & CAVE_LITE))
   {
dlog(DEBUGLIGHT,"note_spot: @ %d,%d no CAVE_LITE, returning\n", x, y);
      return;
   }

   /* Hack -- memorize objects */
   for (;j>=0;j--)
   {
      object_type *i_ptr = get_item_pointer_floor_xy(j,x,y);

      /* Memorize */
      i_ptr->marked = TRUE;
   }

dlog(DEBUGLIGHT,"cave.c: note_spot: px, py %d,%d @ %d,%d m %d s %d  fdat %08lx\n",
           px, py, x, y, c_ptr->mtyp, c_ptr->styp, c_ptr->fdat);

   /* Hack -- memorize grids */
   if (!(c_ptr->fdat & (CAVE_MARK)))
   {
      /* Memorize some "boring" grids */
      if (is_floor_ptr(c_ptr))
      {
         /* Option -- memorize certain floors */
         if (((c_ptr->fdat & CAVE_GLOW) && view_perma_grids) || view_torch_grids)
         {
            /* Memorize */
            c_ptr->fdat |= CAVE_MARK;
         }
      }

      /* Memorize all "interesting" grids */
      else
      {
         /* Memorize */
         c_ptr->fdat |= (CAVE_MARK);
      }
   }
}

/*
 * Redraw (on the screen) a given MAP location
 */
void lite_spot_sub(s16b x, s16b y, u16b mode)
{
   /* Redraw if on screen */
   if (panel_contains(x, y))
   {
      byte a, s;
      char c;

      /* Examine the contents of that grid */
dlog(DEBUGLIGHT,"cave.c: lite_spot_sub: about to call map_info %d,%d mode %d\n", x, y, mode);
      map_info(x, y, &a, &c, &s, mode);

#ifdef USE_COLOR
      /* Fake mono-chrome */
      if (!use_color) a = TERM_WHITE;
#else
      /* Always mono-chrome */
      a = TERM_WHITE;
#endif

      /* Efficiency -- immitate "print_rel()" */
dlog(DEBUGLIGHT,"cave.c: lite_spot: printing char %d=%c attr %d spec %d at %d,%d (scr)=%d,%d\n",
     c, c, a, s, x-panel_prt_col, y-panel_prt_row, x, y);
      Term_draw(x-panel_prt_col, (y-panel_prt_row)+MAP_ROW, a | s, c);
dlog(DEBUGLIGHT,"cave.c: lite_spot: printed\n");
   }
}

/* the normal routine, used almost everywhere */
void lite_spot(s16b x, s16b y)
{
   lite_spot_sub(x, y, LITE_NORMAL);
}

/* the routine for detect() */
void lite_spot_detect(s16b x, s16b y)
{
   lite_spot_sub(x, y, LITE_DETECT);
}

/*
 * Prints the map of the dungeon
 *
 * Note that, for efficiency, we contain an "optimized" version
 * of both "lite_spot()" and "print_rel()".
 */
void prt_map(void)
{
   byte a, s;
   char c;

   int x, y;

   /* Dump the map */
   for (y = panel_min_row; y <= panel_max_row; y++)
   {
      /* Scan the columns of row "y" */
      for (x = panel_min_col; x <= panel_max_col; x++)
      {
         /* Determine what is there */
         map_info(x, y, &a, &c, &s, LITE_NORMAL);

         /* Hack -- Queue it */
         Term_queue_char(x-panel_prt_col, y-panel_prt_row + MAP_ROW, a | s, c);
      }
   }
}

/*
 * Hack -- priority array (see below)
 */

/*
 * Hack -- a priority function
 */
static byte get_priority(s16b mtyp, s16b styp)
{

   s16b f_idx = get_f_idx(mtyp, styp);

   if (!f_idx) return 0;

   if (f_info[f_idx].flags & CAVE_MIMIC)
   {
      f_idx = get_f_idx(f_info[f_idx].mim_m, f_info[f_idx].mim_s);
   }

   return f_info[f_idx].priority;
}

/*
 * Display a scrolling map of the dungeon in the active Term
 *
 * Note that we must cancel the "lighting" options during this
 * function or the "priority()" code will not work correctly.
 *
 * Note that the "map_info()" function must return fully colorized
 * data or this function will not work correctly.
 *
 * Note the use of a specialized "priority" function to allow this
 * function to work with any graphic attr/char mappings, and the
 * attempts to optimize this function where possible.
 */
void do_cmd_view_map(bool interactive)
{
   s16b i, j, x, y, offx = 0, offy = 0, dir;
   s16b ratio = 6;
   s16b map_hgt = 0, map_wid = 0;
   bool compute = TRUE;

   byte ta, ts;
   char tc;
   char c;

   byte ma[MAX_HGT + 2][MAX_WID + 2];
   char mc[MAX_HGT + 2][MAX_WID + 2];
   byte mp[MAX_HGT + 2][MAX_WID + 2];

   bool old_view_yellow_lite = view_yellow_lite;
   bool old_view_bright_lite = view_bright_lite;

   /* Hack -- Cancel the options */
   view_yellow_lite = FALSE;
   view_bright_lite = FALSE;

   /* Enter "icky" mode */
   character_icky = TRUE;

   /* Save the screen */
   if (interactive) Term_save();

   /* Determine optimum display ratio */
   if ( (cur_wid / VISIBLE_SCREEN_WID) > (cur_hgt / VISIBLE_SCREEN_HGT))
   {
      ratio = (cur_wid / VISIBLE_SCREEN_WID)+1;
   }
   else
   {
      ratio = (cur_hgt / VISIBLE_SCREEN_HGT)+1;
   }

   while (TRUE)
   {
      if (compute)
      {
         compute = FALSE;                  /* don't do it again */
         map_hgt = cur_hgt / ratio;
         map_wid = cur_wid / ratio;
         for (y = 0; y < map_hgt + 2; ++y) /* Clear the chars and attributes */
         {
             for (x = 0; x < map_wid + 2; ++x)
             {
                 ma[y][x] = TERM_WHITE; /* Nothing here */
                 mc[y][x] = ' ';
                 mp[y][x] = 0;          /* No priority */
             }
         }

         /* Fill in the map */
         for (i = 0; i < cur_wid; ++i)
         {
            for (j = 0; j < cur_hgt; ++j)
            {
               cave_cell_type *c_ptr = &dungeon.level[sublevel][j][i];
               byte      priority;

               x = i / ratio + 1; /* Index into mc/ma */
               y = j / ratio + 1;

               /* Extract the current attr/char at that map location */
#if (debuglevel & DEBUGGENER)
{
   /* when debugging cave generation, show full info in the map */
   u32b tmp_fdat = dungeon.level[sublevel][j][i].fdat;
   dungeon.level[sublevel][j][i].fdat |= (CAVE_MARK | CAVE_VIEW);
               map_info(i, j, &ta, &tc, &ts, LITE_NORMAL);
   if (dungeon.level[sublevel][j][i].cost !=0) ta = dungeon.level[sublevel][j][i].cost;
   dungeon.level[sublevel][j][i].fdat = tmp_fdat;
   if (dungeon.level[sublevel][j][i].extra == 1)
   {
      ta = TERM_RED;
dlog(DEBUGGENER,"cave.c: do_cmd_view_map: red @ %d,%d\n", i, j);
   }
   else if (dungeon.level[sublevel][j][i].extra == 2)
   {
      ta = TERM_YELLOW;
dlog(DEBUGGENER,"cave.c: do_cmd_view_map: yellow @ %d,%d\n", i, j);
   }

}
#else
               map_info(i, j, &ta, &tc, &ts, LITE_NORMAL);
#endif

               /* Extract the priority of that attr/char */
               priority = get_priority(c_ptr->mtyp, c_ptr->styp);
               /* if we memorized this feature, count it as 'a little better' */
               if (c_ptr->fdat & CAVE_MARK) priority++;

               if (mp[y][x] < priority)           /* Save "best" */
               {
                  mc[y][x] = tc;                  /* Save the char */
                  ma[y][x] = ta | ts;             /* Save the attr */
                  mp[y][x] = priority;            /* Save priority */
               }
               /* the player always wins over other features :-) */
               if ((x == (px / ratio) + 1) && (y == (py / ratio) + 1))
               {
                  mc[y][x] = '@';
                  ma[y][x] = TERM_YELLOW;
                  mp[y][x] = 100;
               }
            }
         }

         mc[0][0] = mc[0][map_wid+1] = '+';
         mc[map_hgt+1][0] = mc[map_hgt+1][map_wid+1] = '+'; /* Draw the corners */
         ma[0][0] = ma[0][map_wid+1] = TERM_WHITE;
         ma[map_hgt+1][0] = ma[map_hgt+1][map_wid+1] = TERM_WHITE; /* Draw the corners */

         /* Draw the horizontal & vertical edges */
         for (x = 1; x <= map_wid; x++)
         {
            mc[0][x] = mc[map_hgt+1][x] = '-';
            ma[0][x] = ma[map_hgt+1][x] = TERM_WHITE;
         }
         for (y = 1; y <= map_hgt; y++)
         {
            mc[y][0] = mc[y][map_wid+1] = '|';
            ma[y][0] = ma[y][map_wid+1] = TERM_WHITE;
         }
         /* make sure the player is displayed as much as possible in the */
         /* middle of the map initially                                  */

         while ((px/ratio)!=(offx+(VISIBLE_SCREEN_WID/2))) offx++;
         if (offx>(map_wid-VISIBLE_SCREEN_WID + 2))
            offx = (map_wid - VISIBLE_SCREEN_WID + 2);
         if (offx<0) offx=0;
         while ((py/ratio)!=(offy+(VISIBLE_SCREEN_HGT/2))) offy++;
         if (offy>(map_hgt-VISIBLE_SCREEN_HGT + 2))
            offy = (map_hgt - VISIBLE_SCREEN_HGT + 2);
         if (offy<0) offy=0;
      }

      clear_screen();

      for (y = 0; y < VISIBLE_SCREEN_HGT; y++) /* Display each map line in order */
      {
         /* Start a new line */
         Term_gotoxy(0, y + MAP_ROW + 1);
         /* don't display anything that isn't in the current map */
         if (y>map_hgt+1) continue;

         /* Display the line */
         for (x = 0; x < VISIBLE_SCREEN_WID; x++)
         {
            /* don't display anything that isn't in the current map */
            if (x>map_wid+1) continue;

            ta = ma[y+offy][x+offx];
            tc = mc[y+offy][x+offx];

#ifdef USE_COLOR
            /* Fake Monochrome */
            if (!use_color) ta = TERM_WHITE;
#else
            /* Monochrome */
            ta = TERM_WHITE;
#endif

            /* Add the character */
            Term_addch(ta, tc);
         }
      }

      if (!interactive) 
      {
         /* Hack -- Restore the options */
         view_yellow_lite = old_view_yellow_lite;
         view_bright_lite = old_view_bright_lite;

         /* Leave "icky" mode */
         character_icky = FALSE;
         
         return;
      }

#if (debuglevel & DEBUGGENER)
      prt(format("res 1:%d showing %d,%d to %d,%d",
                 ratio, offx, offy, offx+VISIBLE_SCREEN_WID, offy+VISIBLE_SCREEN_HGT), 0, MESSAGE_ROW);
#else
      if ( ((cur_wid / ratio) < VISIBLE_SCREEN_WID) && ((cur_hgt / ratio) < VISIBLE_SCREEN_HGT) )
      {
         prt(format("Press +- to change resolution (now 1:%d), Escape to end.", ratio), 0, MESSAGE_ROW);
      }
      else
      {
         prt(format("Press <dir> to scroll, +- to change resolution (now 1:%d), Escape to end.", ratio), 0, MESSAGE_ROW);
      }

#endif
      c=inkey();

      if (c==ESCAPE) break;
      else if ((c>='1') && (c<='9'))
      {
         dir = c-'0';
         offx += ddx[dir];
         offy += ddy[dir];

         if (offx>(map_wid-VISIBLE_SCREEN_WID + 2))
         {
            bell("Map already at far right end.");
            offx = (map_wid - VISIBLE_SCREEN_WID + 2);
         }
         if (offy>(map_hgt-VISIBLE_SCREEN_HGT + 2))
         {
            bell("Map already at far bottom end.");
            offy = (map_hgt - VISIBLE_SCREEN_HGT + 2);
         }
         if (offx<0)
         {
            bell("Map already at far left end.");
            offx=0;
         }
         if (offy<0)
         {
            bell("Map already at far upper end.");
            offy=0;
         }
      }
      else if ( (c=='+') &&
                ( ((cur_wid / ratio) >= VISIBLE_SCREEN_WID) || ((cur_hgt / ratio) >= VISIBLE_SCREEN_HGT) ) ) 
      {
         ratio++;
         if (ratio>6)
         {
            bell("Ratio maximum is 6");
            ratio = 6;
         }
         else
         {
            compute = TRUE;
            clear_screen();
         }
      }
      else if (c=='-')
      {
         ratio--;
         if (ratio<1)
         {
            bell("Ratio cannot be lower than 1");
            ratio = 1;
         }
         else
         {
            compute = TRUE;
            clear_screen();
         }
      }
   }

   /* Hack -- Restore the options */
   view_yellow_lite = old_view_yellow_lite;
   view_bright_lite = old_view_bright_lite;

   /* Restore the screen */
   Term_load();

   /* Leave "icky" mode */
   character_icky = FALSE;
}

/*
 * Some comments on the dungeon related data structures and functions...
 *
 * Angband is primarily a dungeon exploration game, and it should come as
 * no surprise that the internal representation of the dungeon has evolved
 * over time in much the same way as the game itself, to provide semantic
 * changes to the game itself, to make the code simpler to understand, and
 * to make the executable itself faster or more efficient in various ways.
 *
 * There are a variety of dungeon related data structures, and associated
 * functions, which store information about the dungeon, and provide methods
 * by which this information can be accessed or modified.
 *
 * Some of this information applies to the dungeon as a whole, such as the
 * list of unique monsters which are still alive.  Some of this information
 * only applies to the current dungeon level, such as the current depth, or
 * the list of monsters currently inhabiting the level.  And some of the
 * information only applies to a single grid of the current dungeon level,
 * such as whether the grid is illuminated, or whether the grid contains a
 * monster, or whether the grid can be seen by the player.  If Angband was
 * to be turned into a multi-player game, some of the information currently
 * associated with the dungeon should really be associated with the player,
 * such as whether a given grid is viewable by a given player.
 *
 * One of the major bottlenecks in ancient versions of Angband was in the
 * calculation of "line of sight" from the player to various grids, such
 * as those containing monsters, using the relatively expensive "los()"
 * function.  This was such a nasty bottleneck that a lot of silly things
 * were done to reduce the dependancy on "line of sight", for example, you
 * could not "see" any grids in a lit room until you actually entered the
 * room, at which point every grid in the room became "illuminated" and
 * all of the grids in the room were "memorized" forever.  Other major
 * bottlenecks involved the determination of whether a grid was lit by the
 * player's torch, and whether a grid blocked the player's line of sight.
 * These bottlenecks led to the development of special new functions to
 * optimize issues involved with "line of sight" and "torch lit grids".
 * These optimizations led to entirely new additions to the game, such as
 * the ability to display the player's entire field of view using different
 * colors than were used for the "memorized" portions of the dungeon, and
 * the ability to memorize dark floor grids, but to indicate by the way in
 * which they are displayed that they are not actually illuminated.  And
 * of course many of them simply made the game itself faster or more fun.
 * Also, over time, the definition of "line of sight" has been relaxed to
 * allow the player to see a wider "field of view", which is slightly more
 * realistic, and only slightly more expensive to maintain.
 *
 * Currently, a lot of the information about the dungeon is stored in ways
 * that make it very efficient to access or modify the information, while
 * still attempting to be relatively conservative about memory usage, even
 * if this means that some information is stored in multiple places, or in
 * ways which require the use of special code idioms.  For example, each
 * monster record in the monster array contains the location of the monster,
 * and each cave grid has an index into the monster array, or a zero if no
 * monster is in the grid.  This allows the monster code to efficiently see
 * where the monster is located, while allowing the dungeon code to quickly
 * determine not only if a monster is present in a given grid, but also to
 * find out which monster.  The extra space used to store the information
 * twice is inconsequential compared to the speed increase.
 *
 * Some of the information about the dungeon is used by functions which can
 * constitute the "critical efficiency path" of the game itself, and so the
 * way in which they are stored and accessed has been optimized in order to
 * optimize the game itself.  For example, the "update_view()" function was
 * originally created to speed up the game itself (when the player was not
 * running), but then it took on extra responsibility as the provider of the
 * new "special effects lighting code", and became one of the most important
 * bottlenecks when the player was running.  So many rounds of optimization
 * were performed on both the function itself, and the data structures which
 * it uses, resulting eventually in a function which not only made the game
 * faster than before, but which was responsible for even more calculations
 * (including the determination of which grids are "viewable" by the player,
 * which grids are illuminated by the player's torch, and which grids can be
 * "seen" in some way by the player), as well as for providing the guts of
 * the special effects lighting code, and for the efficient redisplay of any
 * grids whose visual representation may have changed.
 *
 * Several pieces of information about each cave grid are stored in various
 * two dimensional arrays, with one unit of information for each grid in the
 * dungeon.  Some of these arrays have been intentionally expanded by a small
 * factor to make the two dimensional array accesses faster by allowing the
 * use of shifting instead of multiplication.
 *
 * Several pieces of information about each cave grid are stored in the
 * "cave_info" array, which is a special two dimensional array of bytes,
 * one for each cave grid, each containing eight separate "flags" which
 * describe some property of the cave grid.  These flags can be checked and
 * modified extremely quickly, especially when special idioms are used to
 * force the compiler to keep a local register pointing to the base of the
 * array.  Special location offset macros can be used to minimize the number
 * of computations which must be performed at runtime.  Note that using a
 * byte for each flag set may be slightly more efficient than using a larger
 * unit, so if another flag (or two) is needed later, and it must be fast,
 * then the two existing flags which do not have to be fast should be moved
 * out into some other data structure and the new flags should take their
 * place.  This may require a few minor changes in the savefile code.
 *
 * The "CAVE_ROOM" flag is saved in the savefile and is used to determine
 * which grids are part of "rooms", and thus which grids are affected by
 * "illumination" spells.  This flag does not have to be very fast.
 *
 * The "CAVE_VAULT" flag is saved in the savefile and is used to determine
 * which grids are part of "vaults", and thus which grids cannot serve as
 * the destinations of player teleportation.  This flag does not have to
 * be very fast.
 *
 * The "CAVE_MARK" flag is saved in the savefile and is used to determine
 * which grids have been "memorized" by the player.  This flag is used by
 * the "map_info()" function to determine if a grid should be displayed.
 * This flag is used in a few other places to determine if the player can
 * "know" about a given grid.  This flag must be very fast.
 *
 * The "CAVE_GLOW" flag is saved in the savefile and is used to determine
 * which grids are "permanently illuminated".  This flag is used by the
 * "update_view()" function to help determine which viewable flags may
 * be "seen" by the player.  This flag is used by the "map_info" function
 * to determine if a grid is only lit by the player's torch.  This flag
 * has special semantics for wall grids (see "update_view()").  This flag
 * must be very fast.
 *
 * The "CAVE_LIGHT" flag is used to determine which grids block the player's
 * line of sight.  This flag is used by the "update_view()" function to
 * determine which grids block line of sight, and to help determine which
 * grids can be "seen" by the player.  This flag must be very fast.
 *
 * The "CAVE_VIEW" flag is used to determine which grids are currently in
 * line of sight of the player.  This flag is set by (and used by) the
 * "update_view()" function.  This flag is used by any code which needs to
 * know if the player can "view" a given grid.  This flag is used by the
 * "map_info()" function for some optional special lighting effects.  The
 * "player_has_los_bold()" macro wraps an abstraction around this flag, but
 * certain code idioms are much more efficient.  This flag is used to check
 * if a modification to a terrain feature might affect the player's field of
 * view.  This flag is used to see if certain monsters are "visible" to the
 * player.  This flag is used to allow any monster in the player's field of
 * view to "sense" the presence of the player.  This flag must be very fast.
 *
 * The "CAVE_LITE" flag is used to determine which grids are currently in
 * line of sight of the player and also illuminated in some way.  This flag
 * is set by the "update_view()" function, using computations based on the
 * "CAVE_VIEW" and "CAVE_LIGHT" and "CAVE_GLOW" flags of various grids.  This
 * flag is used by any code which needs to know if the player can "see" a
 * given grid.  This flag is used by the "map_info()" function both to see
 * if a given "boring" grid can be seen by the player, and for some optional
 * special lighting effects.  The "player_can_see_bold()" macro wraps an
 * abstraction around this flag, but certain code idioms are much more
 * efficient.  This flag is used to see if certain monsters are "visible" to
 * the player.  This flag is never set for a grid unless "CAVE_VIEW" is also
 * set for the grid.  Whenever the "CAVE_LIGHT" or "CAVE_GLOW" flag changes
 * for a grid which has the "CAVE_VIEW" flag set, the "CAVE_LITE" flag must
 * be recalculated.  The simplest way to do this is to call "forget_view()"
 * and "update_view()" whenever the "CAVE_LIGHT" or "CAVE_GLOW" flags change
 * for a grid which has "CAVE_VIEW" set.  This flag must be very fast.
 *
 * The "CAVE_OLD_LITE" flag is used for a variety of temporary purposes.  This
 * flag is used to determine if the "CAVE_LITE" flag for a grid has changed
 * during the "update_view()" function.  This flag is used to "spread" light
 * or darkness through a room.  This flag is used by the "monster flow code".
 * This flag must always be cleared by any code which sets it, often, this
 * can be optimized by the use of the special "old_lite_g", "temp_y", "temp_x"
 * arrays (and the special "old_lite_n" global).  This flag must be very fast.
 *
 * Note that the "CAVE_MARK" flag is used for many reasons, some of which
 * are strictly for optimization purposes.  The "CAVE_MARK" flag means that
 * even if the player cannot "see" the grid, he "knows" about the terrain in
 * that grid.  This is used to "memorize" grids when they are first "seen" by
 * the player, and to allow certain grids to be "detected" by certain magic.
 * Note that most grids are always memorized when they are first "seen", but
 * "boring" grids (floor grids) are only memorized if the "view_torch_grids"
 * option is set, or if the "view_perma_grids" option is set, and the grid
 * in question has the "CAVE_GLOW" flag set.
 *
 * Objects are "memorized" in a different way, using a special "marked" flag
 * on the object itself, which is set when an object is observed or detected.
 * This allows objects to be "memorized" independant of the terrain features.
 *
 * The "update_view()" function is an extremely important function.  It is
 * called only when the player moves, significant terrain changes, or the
 * player's blindness or torch radius changes.  Note that when the player
 * is resting, or performing any repeated actions (like digging, disarming,
 * farming, etc), there is no need to call the "update_view()" function, so
 * even if it was not very efficient, this would really only matter when the
 * player was "running" through the dungeon.  It sets the "CAVE_VIEW" flag
 * on every cave grid in the player's field of view, and maintains an array
 * of all such grids in the global "view_g" array.  It also checks the torch
 * radius of the player, and sets the "CAVE_LITE" flag for every grid which
 * is in the "field of view" of the player and which is also "illuminated",
 * either by the players torch (if any) or by any permanent light source.
 * It could use and help maintain information about multiple light sources,
 * which would be helpful in a multi-player version of Angband.
 *
 * The "update_view()" function maintains the special "view_g" array, which
 * contains exactly those grids which have the "CAVE_VIEW" flag set.  This
 * array is used by "update_view()" to (only) memorize grids which become
 * newly "seen", and to (only) redraw grids whose "seen" value changes, which
 * allows the use of some interesting (and very efficient) "special lighting
 * effects".  In addition, this array could be used elsewhere to quickly scan
 * through all the grids which are in the player's field of view.
 *
 * Note that the "update_view()" function allows, among other things, a room
 * to be "partially" seen as the player approaches it, with a growing cone
 * of floor appearing as the player gets closer to the door.  Also, by not
 * turning on the "memorize perma-lit grids" option, the player will only
 * "see" those floor grids which are actually in line of sight.  And best
 * of all, you can now activate the special lighting effects to indicate
 * which grids are actually in the player's field of view by using dimmer
 * colors for grids which are not in the player's field of view, and/or to
 * indicate which grids are illuminated only by the player's torch by using
 * the color yellow for those grids.
 *
 * The old "update_view()" algorithm uses the special "CAVE_EASY" flag as a
 * temporary internal flag to mark those grids which are not only in view,
 * but which are also "easily" in line of sight of the player.  This flag
 * is actually just the "CAVE_LITE" flag, and the "update_view()" function
 * makes sure to clear it for all old "CAVE_LITE" grids, and then use it in
 * the algorithm as "CAVE_EASY", and then clear it for all "CAVE_EASY" grids,
 * and then reset it as appropriate for all new "CAVE_LITE" grids.  This is
 * kind of messy, but it works.  The old algorithm may disappear eventually.
 *
 * The new "update_view()" algorithm uses a faster and more mathematically
 * correct algorithm, assisted by a large machine generated static array, to
 * determine the "CAVE_VIEW" and "CAVE_LITE" flags simultaneously.  See below.
 *
 * It seems as though slight modifications to the "update_view()" functions
 * would allow us to determine "reverse" line-of-sight as well as "normal"
 * line-of-sight", which would allow monsters to have a more "correct" way
 * to determine if they can "see" the player, since right now, they "cheat"
 * somewhat and assume that if the player has "line of sight" to them, then
 * they can "pretend" that they have "line of sight" to the player.  But if
 * such a change was attempted, the monsters would actually start to exhibit
 * some undesirable behavior, such as "freezing" near the entrances to long
 * hallways containing the player, and code would have to be added to make
 * the monsters move around even if the player was not detectable, and to
 * "remember" where the player was last seen, to avoid looking stupid.
 *
 * Note that the "CAVE_GLOW" flag means that a grid is permanently lit in
 * some way.  However, for the player to "see" the grid, as determined by
 * the "CAVE_LITE" flag, the player must not be blind, the grid must have
 * the "CAVE_VIEW" flag set, and if the grid is a "wall" grid, and it is
 * not lit by the player's torch, then it must touch a grid which does not
 * have the "CAVE_LIGHT" flag set, but which does have both the "CAVE_GLOW"
 * and "CAVE_VIEW" flags set.  This last part about wall grids is induced
 * by the semantics of "CAVE_GLOW" as applied to wall grids, and checking
 * the technical requirements can be very expensive, especially since the
 * grid may be touching some "illegal" grids.  Luckily, it is more or less
 * correct to restrict the "touching" grids from the eight "possible" grids
 * to the (at most) three grids which are touching the grid, and which are
 * closer to the player than the grid itself, which eliminates more than
 * half of the work, including all of the potentially "illegal" grids, if
 * at most one of the three grids is a "diagonal" grid.  In addition, in
 * almost every situation, it is possible to ignore the "CAVE_VIEW" flag
 * on these three "touching" grids, for a variety of technical reasons.
 * Finally, note that in most situations, it is only necessary to check
 * a single "touching" grid, in fact, the grid which is strictly closest
 * to the player of all the touching grids, and in fact, it is normally
 * only necessary to check the "CAVE_GLOW" flag of that grid, again, for
 * various technical reasons.  However, one of the situations which does
 * not work with this last reduction is the very common one in which the
 * player approaches an illuminated room from a dark hallway, in which the
 * two wall grids which form the "entrance" to the room would not be marked
 * as "CAVE_LITE", since of the three "touching" grids nearer to the player
 * than each wall grid, only the farthest of these grids is itself marked
 * "CAVE_GLOW".
 *
 *
 * Here are some pictures of the legal "light source" radius values, in
 * which the numbers indicate the "order" in which the grids could have
 * been calculated, if desired.  Note that the code will work with larger
 * radiuses, though currently yields such a radius, and the game would
 * become slower in some situations if it did.
 *
 *       Rad=0     Rad=1      Rad=2        Rad=3
 *      No-Lite  Torch,etc   Lantern     Artifacts
 *
 *                                          333
 *                             333         43334
 *                  212       32123       3321233
 *         @        1@1       31@13       331@133
 *                  212       32123       3321233
 *                             333         43334
 *                                          333
 *
 *
 * Here is an illustration of the two different "update_view()" algorithms,
 * in which the grids marked "%" are pillars, and the grids marked "?" are
 * not in line of sight of the player.
 *
 *
 *                    Sample situation
 *
 *                  #####################
 *                  ############.%.%.%.%#
 *                  #...@..#####........#
 *                  #............%.%.%.%#
 *                  #......#####........#
 *                  ############........#
 *                  #####################
 *
 *
 *          New Algorithm             Old Algorithm
 *
 *      ########?????????????    ########?????????????
 *      #...@..#?????????????    #...@..#?????????????
 *      #...........?????????    #.........???????????
 *      #......#####.....????    #......####??????????
 *      ########?????????...#    ########?????????????
 *
 *      ########?????????????    ########?????????????
 *      #.@....#?????????????    #.@....#?????????????
 *      #............%???????    #...........?????????
 *      #......#####........?    #......#####?????????
 *      ########??????????..#    ########?????????????
 *
 *      ########?????????????    ########?????%???????
 *      #......#####........#    #......#####..???????
 *      #.@..........%???????    #.@..........%???????
 *      #......#####........#    #......#####..???????
 *      ########?????????????    ########?????????????
 *
 *      ########??????????..#    ########?????????????
 *      #......#####........?    #......#####?????????
 *      #............%???????    #...........?????????
 *      #.@....#?????????????    #.@....#?????????????
 *      ########?????????????    ########?????????????
 *
 *      ########?????????%???    ########?????????????
 *      #......#####.....????    #......####??????????
 *      #...........?????????    #.........???????????
 *      #...@..#?????????????    #...@..#?????????????
 *      ########?????????????    ########?????????????
 */

/*
 * Maximum number of grids in a single octant
 */
#define VINFO_MAX_GRIDS 161

/*
 * Maximum number of slopes in a single octant
 */
#define VINFO_MAX_SLOPES 126


/*
 * Mask of bits used in a single octant
 */
#define VINFO_BITS_3 0x3FFFFFFF
#define VINFO_BITS_2 0xFFFFFFFF
#define VINFO_BITS_1 0xFFFFFFFF
#define VINFO_BITS_0 0xFFFFFFFF


/*
 * Forward declare
 */
typedef struct vinfo_type vinfo_type;


/*
 * The 'vinfo_type' structure
 */
struct vinfo_type
{
   s16b grid_0;
   s16b grid_1;
   s16b grid_2;
   s16b grid_3;
   s16b grid_4;
   s16b grid_5;
   s16b grid_6;
   s16b grid_7;

   u32b bits_3;
   u32b bits_2;
   u32b bits_1;
   u32b bits_0;

   vinfo_type *next_0;
   vinfo_type *next_1;

   byte y;
   byte x;
   byte d;
   byte r;
};

/*
 * The array of "vinfo" objects, initialized by "vinfo_init()"
 */
static vinfo_type vinfo[VINFO_MAX_GRIDS];

/*
 * Slope scale factor
 */
#define SCALE 100000L

/*
 * The actual slopes (for reference)
 */

/* Bit :     Slope   Grids */
/* --- :     -----   ----- */
/*   0 :      2439      21 */
/*   1 :      2564      21 */
/*   2 :      2702      21 */
/*   3 :      2857      21 */
/*   4 :      3030      21 */
/*   5 :      3225      21 */
/*   6 :      3448      21 */
/*   7 :      3703      21 */
/*   8 :      4000      21 */
/*   9 :      4347      21 */
/*  10 :      4761      21 */
/*  11 :      5263      21 */
/*  12 :      5882      21 */
/*  13 :      6666      21 */
/*  14 :      7317      22 */
/*  15 :      7692      20 */
/*  16 :      8108      21 */
/*  17 :      8571      21 */
/*  18 :      9090      20 */
/*  19 :      9677      21 */
/*  20 :     10344      21 */
/*  21 :     11111      20 */
/*  22 :     12000      21 */
/*  23 :     12820      22 */
/*  24 :     13043      22 */
/*  25 :     13513      22 */
/*  26 :     14285      20 */
/*  27 :     15151      22 */
/*  28 :     15789      22 */
/*  29 :     16129      22 */
/*  30 :     17241      22 */
/*  31 :     17647      22 */
/*  32 :     17948      23 */
/*  33 :     18518      22 */
/*  34 :     18918      22 */
/*  35 :     20000      19 */
/*  36 :     21212      22 */
/*  37 :     21739      22 */
/*  38 :     22580      22 */
/*  39 :     23076      22 */
/*  40 :     23809      22 */
/*  41 :     24137      22 */
/*  42 :     24324      23 */
/*  43 :     25714      23 */
/*  44 :     25925      23 */
/*  45 :     26315      23 */
/*  46 :     27272      22 */
/*  47 :     28000      23 */
/*  48 :     29032      23 */
/*  49 :     29411      23 */
/*  50 :     29729      24 */
/*  51 :     30434      23 */
/*  52 :     31034      23 */
/*  53 :     31428      23 */
/*  54 :     33333      18 */
/*  55 :     35483      23 */
/*  56 :     36000      23 */
/*  57 :     36842      23 */
/*  58 :     37142      24 */
/*  59 :     37931      24 */
/*  60 :     38461      24 */
/*  61 :     39130      24 */
/*  62 :     39393      24 */
/*  63 :     40740      24 */
/*  64 :     41176      24 */
/*  65 :     41935      24 */
/*  66 :     42857      23 */
/*  67 :     44000      24 */
/*  68 :     44827      24 */
/*  69 :     45454      23 */
/*  70 :     46666      24 */
/*  71 :     47368      24 */
/*  72 :     47826      24 */
/*  73 :     48148      24 */
/*  74 :     48387      24 */
/*  75 :     51515      25 */
/*  76 :     51724      25 */
/*  77 :     52000      25 */
/*  78 :     52380      25 */
/*  79 :     52941      25 */
/*  80 :     53846      25 */
/*  81 :     54838      25 */
/*  82 :     55555      24 */
/*  83 :     56521      25 */
/*  84 :     57575      26 */
/*  85 :     57894      25 */
/*  86 :     58620      25 */
/*  87 :     60000      23 */
/*  88 :     61290      25 */
/*  89 :     61904      25 */
/*  90 :     62962      25 */
/*  91 :     63636      25 */
/*  92 :     64705      25 */
/*  93 :     65217      25 */
/*  94 :     65517      25 */
/*  95 :     67741      26 */
/*  96 :     68000      26 */
/*  97 :     68421      26 */
/*  98 :     69230      26 */
/*  99 :     70370      26 */
/* 100 :     71428      25 */
/* 101 :     72413      26 */
/* 102 :     73333      26 */
/* 103 :     73913      26 */
/* 104 :     74193      27 */
/* 105 :     76000      26 */
/* 106 :     76470      26 */
/* 107 :     77777      25 */
/* 108 :     78947      26 */
/* 109 :     79310      26 */
/* 110 :     80952      26 */
/* 111 :     81818      26 */
/* 112 :     82608      26 */
/* 113 :     84000      26 */
/* 114 :     84615      26 */
/* 115 :     85185      26 */
/* 116 :     86206      27 */
/* 117 :     86666      27 */
/* 118 :     88235      27 */
/* 119 :     89473      27 */
/* 120 :     90476      27 */
/* 121 :     91304      27 */
/* 122 :     92000      27 */
/* 123 :     92592      27 */
/* 124 :     93103      28 */
/* 125 :    100000      13 */

/*
 * Forward declare
 */
typedef struct vinfo_hack vinfo_hack;

/*
 * Temporary data used by "vinfo_init()"
 *
 * - Number of grids
 *
 * - Number of slopes
 *
 * - Slope values
 *
 * - Slope range per grid
 */
struct vinfo_hack {

   s16b num_slopes;

   long slopes[VINFO_MAX_SLOPES];

   long slopes_min[MAX_SIGHT+1][MAX_SIGHT+1];
   long slopes_max[MAX_SIGHT+1][MAX_SIGHT+1];
};

/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
bool ang_sort_comp_hook_longs(vptr u, vptr v, s16b a, s16b b)
{
   long *x = (long*)(u);

   return (x[a] <= x[b]);
}

/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
void ang_sort_swap_hook_longs(vptr u, vptr v, s16b a, s16b b)
{
   long *x = (long*)(u);

   long temp;

   /* Swap */
   temp = x[a];
   x[a] = x[b];
   x[b] = temp;
}

/*
 * Save a slope
 */
static void vinfo_init_aux(vinfo_hack *hack, s16b x, s16b y, long m)
{
   s16b i;

   /* Handle "legal" slopes */
   if ((m > 0) && (m <= SCALE))
   {
      /* Look for that slope */
      for (i = 0; i < hack->num_slopes; i++)
      {
         if (hack->slopes[i] == m) break;
      }

      /* New slope */
      if (i == hack->num_slopes)
      {
         /* Paranoia */
         if (hack->num_slopes >= VINFO_MAX_SLOPES)
         {
            quit_fmt("Too many slopes (%d)!",
                     VINFO_MAX_SLOPES);
         }

         /* Save the slope, and advance */
         hack->slopes[hack->num_slopes++] = m;
      }
   }

   /* Track slope range */
   if (hack->slopes_min[y][x] > m) hack->slopes_min[y][x] = m;
   if (hack->slopes_max[y][x] < m) hack->slopes_max[y][x] = m;
}

/*
 * Initialize the "vinfo" array
 *
 * Full Octagon (radius 20), Grids=1149
 *
 * Quadrant (south east), Grids=308, Slopes=251
 *
 * Octant (east then south), Grids=161, Slopes=126
 *
 * This function assumes that VINFO_MAX_GRIDS and VINFO_MAX_SLOPES
 * have the correct values, which can be derived by setting them to
 * a number which is too high, running this function, and using the
 * error messages to obtain the correct values.
 */
errr vinfo_init(void)
{
   s16b i, g;
   u16b x, y;

   long m;

   vinfo_hack *hack;

   s16b num_grids = 0;

   s16b queue_head = 0;
   s16b queue_tail = 0;
   vinfo_type *queue[VINFO_MAX_GRIDS*2];

dlog(DEBUGLOS,"cave.c: vinfo_init starting\n");

   /* Make hack */
   MAKE(hack, vinfo_hack);

   /* Analyze grids */
   for (y = 0; y <= MAX_SIGHT; ++y)
   {
      for (x = y; x <= MAX_SIGHT; ++x)
      {
         /* Skip grids which are out of sight range */
         if (distance(0, 0, x, y) > MAX_SIGHT) continue;

         /* Default slope range */
         hack->slopes_min[y][x] = 999999999;
         hack->slopes_max[y][x] = 0;

         /* Paranoia */
         if (num_grids >= VINFO_MAX_GRIDS)
         {
            quit_fmt("Too many grids (%d >= %d)!",
                     num_grids, VINFO_MAX_GRIDS);
         }

         /* Count grids */
         num_grids++;

         /* Slope to the top right corner */
         m = SCALE * (1000L * y - 500) / (1000L * x + 500);

         /* Handle "legal" slopes */
         vinfo_init_aux(hack, x, y, m);

         /* Slope to top left corner */
         m = SCALE * (1000L * y - 500) / (1000L * x - 500);

         /* Handle "legal" slopes */
         vinfo_init_aux(hack, x, y, m);

         /* Slope to bottom right corner */
         m = SCALE * (1000L * y + 500) / (1000L * x + 500);

         /* Handle "legal" slopes */
         vinfo_init_aux(hack, x, y, m);

         /* Slope to bottom left corner */
         m = SCALE * (1000L * y + 500) / (1000L * x - 500);

         /* Handle "legal" slopes */
         vinfo_init_aux(hack, x, y, m);
      }
   }
dlog(DEBUGLOS,"cave.c: vinfo_init grids analyzed\n");

   /* Enforce maximal efficiency */
   if (num_grids < VINFO_MAX_GRIDS)
   {
      quit_fmt("Too few grids (%d < %d)!",
               num_grids, VINFO_MAX_GRIDS);
   }

   /* Enforce maximal efficiency */
   if (hack->num_slopes < VINFO_MAX_SLOPES)
   {
      quit_fmt("Too few slopes (%d < %d)!",
               hack->num_slopes, VINFO_MAX_SLOPES);
   }

   /* Sort slopes numerically */
   ang_sort_comp = ang_sort_comp_hook_longs;

   /* Sort slopes numerically */
   ang_sort_swap = ang_sort_swap_hook_longs;

   /* Sort the (unique) slopes */
   ang_sort(hack->slopes, NULL, hack->num_slopes);
dlog(DEBUGLOS,"cave.c: vinfo_init sorted\n");

   /* Enqueue player grid */
   queue[queue_tail++] = &vinfo[0];
dlog(DEBUGLOS,"cave.c: vinfo_init step 0\n");

   /* Process queue */
   while (queue_head < queue_tail)
   {
      s16b e;

      vinfo_type *p;

      /* Index */
      e = queue_head;

      /* Dequeue next grid */
      p = queue[queue_head++];

      /* Main Grid */
      g = vinfo[e].grid_0;

      /* Location */
      x = GRID_X(g);
      y = GRID_Y(g);

      /* Compute grid offsets */
      vinfo[e].grid_0 = GRID(+x,+y);
      vinfo[e].grid_1 = GRID(+y,+x);
      vinfo[e].grid_2 = GRID(-y,+x);
      vinfo[e].grid_3 = GRID(-x,+y);
      vinfo[e].grid_4 = GRID(-x,-y);
      vinfo[e].grid_5 = GRID(-y,-x);
      vinfo[e].grid_6 = GRID(+y,-x);
      vinfo[e].grid_7 = GRID(+x,-y);

      /* Analyze slopes */
      for (i = 0; i < hack->num_slopes; ++i)
      {
         m = hack->slopes[i];

         /* Memorize intersection slopes (for non-player-grids) */
         if ((e > 0) &&
             (hack->slopes_min[y][x] < m) &&
             (m < hack->slopes_max[y][x]))
         {
            switch (i / 32)
            {
               case 3: vinfo[e].bits_3 |= (1L << (i % 32)); break;
               case 2: vinfo[e].bits_2 |= (1L << (i % 32)); break;
               case 1: vinfo[e].bits_1 |= (1L << (i % 32)); break;
               case 0: vinfo[e].bits_0 |= (1L << (i % 32)); break;
            }
         }
      }

      /* Default */
      vinfo[e].next_0 = &vinfo[0];

      /* Grid next child */
      if (distance(0, 0, x+1, y) <= MAX_SIGHT)
      {
         g = GRID(x+1, y);

         if (queue[queue_tail-1]->grid_0 != g)
         {
            vinfo[queue_tail].grid_0 = g;
            queue[queue_tail] = &vinfo[queue_tail];
            queue_tail++;
         }

         vinfo[e].next_0 = &vinfo[queue_tail - 1];
      }

      /* Default */
      vinfo[e].next_1 = &vinfo[0];

      /* Grid diag child */
      if (distance(0, 0, x+1, y+1) <= MAX_SIGHT)
      {
         g = GRID(x+1,y+1);

         if (queue[queue_tail-1]->grid_0 != g)
         {
            vinfo[queue_tail].grid_0 = g;
            queue[queue_tail] = &vinfo[queue_tail];
            queue_tail++;
         }

         vinfo[e].next_1 = &vinfo[queue_tail - 1];
      }

      /* Hack -- main diagonal has special children */
      if (y == x) vinfo[e].next_0 = vinfo[e].next_1;

      /* Extra values */
      vinfo[e].y = y;
      vinfo[e].x = x;
      vinfo[e].d = ((y > x) ? (y + x/2) : (x + y/2));
      vinfo[e].r = ((!y) ? x : (!x) ? y : (y == x) ? y : 0);
   }

   /* Verify maximal bits XXX XXX XXX */
   if (((vinfo[1].bits_3 | vinfo[2].bits_3) != VINFO_BITS_3) ||
       ((vinfo[1].bits_2 | vinfo[2].bits_2) != VINFO_BITS_2) ||
       ((vinfo[1].bits_1 | vinfo[2].bits_1) != VINFO_BITS_1) ||
       ((vinfo[1].bits_0 | vinfo[2].bits_0) != VINFO_BITS_0))
   {
      quit("Incorrect bit masks!");
   }

   /* Kill hack */
   KILL(hack, vinfo_hack);

dlog(DEBUGLOS,"cave.c: vinfo_init ending\n");

   /* Success */
   return (0);
}

/*
 * Forget the "CAVE_VIEW" grids, redrawing as needed
 */
void forget_view(void)
{
   s16b i, g;

dlog(DEBUGLIGHT,"cave.c: forgetview starting, view_n %d\n", view_n);

   /* None to forget */
   if (!view_n)
   {
      dlog(DEBUGLIGHT,"cave.c: forgetview: view_n=0, returning\n");
      return;
   }

   /* Clear them all */
   for (i = 0; i < view_n; i++)
   {
      s16b y, x;

      /* Grid */
      g = view_g[i];

      /* Location */
      x = GRID_X(g);
      y = GRID_Y(g);
dlog(DEBUGLIGHT,"cave.c: forget_view: i %d of %d, g %04x x,y %d,%d\n", i, view_n, g, x, y);
dlog(DEBUGLIGHT,"cave.c: forget_view: g >> 9 %04x\n", g >> 9);

      /* Clear "CAVE_VIEW" and "CAVE_LITE" flags */
      dungeon.level[sublevel][y][x].fdat &= ~(CAVE_VIEW | CAVE_LITE);

      /* Redraw */
dlog(DEBUGLIGHT,"cave.c: forget_view: calling lite_spot %d,%d\n", x, y);
      lite_spot(x, y);
   }

   /* None left */
   view_n = 0;
dlog(DEBUGLIGHT,"cave.c: forgetview ending\n");
}

/*
 * Calculate the complete field of view using a new algorithm
 *
 * If "view_g" and "old_lite_g" were global pointers to arrays of grids, as
 * opposed to actual arrays of grids, then we could be more efficient by
 * using "pointer swapping".
 *
 * Note the following idiom, which is used in the function below.
 * This idiom processes each "octant" of the field of view, in a
 * clockwise manner, starting with the east strip, south side,
 * and for each octant, allows a simple calculation to set "g"
 * equal to the proper grids, relative to "pg", in the octant.
 *
 *   for (o2 = 0; o2 < 16; o2 += 2)
 *   ...
 *         g = pg + *((s16b*)(((byte*)(p))+o2));
 *   ...
 *
 *
 * Normally, vision along the major axes is more likely than vision
 * along the diagonal axes, so we check the bits corresponding to
 * the lines of sight near the major axes first.
 *
 * We use the "old_lite_g" array (and the "CAVE_OLD_LITE" flag) to keep track of
 * which grids were previously marked "CAVE_LITE", since only those grids
 * whose "CAVE_LITE" value changes during this routine must be redrawn.
 *
 * This function is now responsible for maintaining the "CAVE_LITE"
 * flags as well as the "CAVE_VIEW" flags, which is good, because
 * the only grids which normally need to be memorized and/or redrawn
 * are the ones whose "CAVE_LITE" flag changes during this routine.
 *
 * Basically, this function divides the "octagon of view" into octants of
 * grids (where grids on the main axes and diagonal axes are "shared" by
 * two octants), and processes each octant one at a time, processing each
 * octant one grid at a time, processing only those grids which "might" be
 * viewable, and setting the "CAVE_VIEW" flag for each grid for which there
 * is an (unobstructed) line of sight from the center of the player grid to
 * any internal point in the grid (and collecting these "CAVE_VIEW" grids
 * into the "view_g" array), and setting the "CAVE_LITE" flag for the grid
 * if, in addition, the grid is "illuminated" in some way.
 *
 * This function relies on a theorem (suggested and proven by Mat Hostetter)
 * which states that in each octant of a field of view, a given grid will
 * be "intersected" by one or more unobstructed "lines of sight" from the
 * center of the player grid if and only if it is "intersected" by at least
 * one such unobstructed "line of sight" which passes directly through some
 * corner of some grid in the octant which is not shared by any other octant.
 * The proof is based on the fact that there are at least three significant
 * lines of sight involving any non-shared grid in any octant, one which
 * intersects the grid and passes though the corner of the grid closest to
 * the player, and two which "brush" the grid, passing through the "outer"
 * corners of the grid, and that any line of sight which intersects a grid
 * without passing through the corner of a grid in the octant can be "slid"
 * slowly towards the corner of the grid closest to the player, until it
 * either reaches it or until it brushes the corner of another grid which
 * is closer to the player, and in either case, the existance of a suitable
 * line of sight is thus demonstrated.
 *
 * It turns out that in each octant of the radius 20 "octagon of view",
 * there are 161 grids (with 128 not shared by any other octant), and there
 * are exactly 126 distinct "lines of sight" passing from the center of the
 * player grid through any corner of any non-shared grid in the octant.  To
 * determine if a grid is "viewable" by the player, therefore, you need to
 * simply show that one of these 126 lines of sight intersects the grid but
 * does not intersect any wall grid closer to the player.  So we simply use
 * a bit vector with 126 bits to represent the set of interesting lines of
 * sight which have not yet been obstructed by wall grids, and then we scan
 * all the grids in the octant, moving outwards from the player grid.  For
 * each grid, if any of the lines of sight which intersect that grid have not
 * yet been obstructed, then the grid is viewable.  Furthermore, if the grid
 * is a wall grid, then all of the lines of sight which intersect the grid
 * should be marked as obstructed for future reference.  Also, we only need
 * to check those grids for whom at least one of the "parents" was a viewable
 * non-wall grid, where the parents include the two grids touching the grid
 * but closer to the player grid (one adjacent, and one diagonal).  For the
 * bit vector, we simply use 4 32-bit integers.  All of the static values
 * which are needed by this function are stored in the large "vinfo" array
 * (above), which is machine generated by another program.  XXX XXX XXX
 *
 * Hack -- The queue must be able to hold more than VINFO_MAX_GRIDS grids
 * because the grids at the edge of the field of view use "grid zero" as
 * their children, and the queue must be able to hold several of these
 * special grids.  Because the actual number of required grids is bizarre,
 * we simply allocate twice as many as we would normally need.  XXX XXX XXX
 */
void update_view(void)
{
   s16b pg = GRID(px,py);
   s16b i, o2;
   u16b g;
   s16b radius;
   u32b *info;

   old_lite_n = 0;

dlog(DEBUGLOS,"cave.c: update_view: starting, view_n %d\n", view_n);

   /*** Step 0 -- Begin ***/

   /* Save the old "view" grids for later */
   for (i = 0; i < view_n; i++)
   {
      /* Grid */
      g = view_g[i];

      /* Get grid info */
      info = &dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].fdat;
dlog(DEBUGLOS,"cave.c: update_view: old view grid %d g %d x %d y %d\n", i, g, GRID_X(g), GRID_Y(g));
      /* Save "CAVE_LITE" grids */
      if (*info & (CAVE_LITE))
      {
         /* Set "CAVE_OLD_LITE" flag */
         *info |= (CAVE_OLD_LITE);

         /* Save grid for later */
         old_lite_g[old_lite_n++] = g;
      }

      /* Clear "CAVE_VIEW" and "CAVE_LITE" flags */
      *info &= ~(CAVE_VIEW | CAVE_LITE);
   }

   view_n = 0;            /* Reset the "view" array */
   radius = p_ptr->cur_lite;   /* Extract "radius" value */
   if (radius > 0) ++radius;   /* Handle real light */

dlog(DEBUGLOS,"cave.c: update_view step 1 starting\n");

   /*** Step 1 -- player grid ***/

   g = pg;                        /* Player grid */

   /* Get grid info */
   info = &dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].fdat;

dlog(DEBUGLOS, "cave.c: update_view: pg %d, px,py %d,%d, GRID_X,GRID_Y %d,%d\n", pg, px, py, GRID_X(g), GRID_Y(g));

   if (0 < radius)               /* Torch-lit grid */
   {
      *info |= (CAVE_LITE|CAVE_VIEW); /* Mark as "CAVE_LITE" and "CAVE_VIEW" */
   }

   else if (*info & (CAVE_GLOW)) /* Perma-lit grid */
   {
      *info |= (CAVE_VIEW|CAVE_LITE);      /* Mark as "CAVE_VIEW" */
   }

   /* Save in array if we can see it */
   if (*info & CAVE_VIEW)
   {
      view_g[view_n++] = g;
dlog(DEBUGLOS, "cave.c: update_view: %d,%d added to view_g, info now %08lx\n", GRID_X(g), GRID_Y(g), *info);
   }

dlog(DEBUGLOS,"cave.c: update_view step 2 starting\n");

   /*** Step 2 -- octants ***/

   for (o2 = 0; o2 < 16; o2 += 2)     /* Scan each octant */
   {
      vinfo_type *p;
      vinfo_type *last = &vinfo[0];   /* Last added */

      s16b queue_head = 0;            /* Grid queue */
      s16b queue_tail = 0;
      vinfo_type *queue[VINFO_MAX_GRIDS*2];

      u32b bits0 = VINFO_BITS_0;      /* Slope bit vector */
      u32b bits1 = VINFO_BITS_1;
      u32b bits2 = VINFO_BITS_2;
      u32b bits3 = VINFO_BITS_3;
dlog(DEBUGLOS,"cave.c: update_view step 2 starting with octant %d\n", o2);

      queue_head = queue_tail = 0;    /* Reset queue */

      /* Initial grids */
      queue[queue_tail++] = &vinfo[1];
      queue[queue_tail++] = &vinfo[2];
dlog(DEBUGLOS,"cave.c: update_view: step 2 starting head %d tail %d\n", queue_head, queue_tail);

      while (queue_head < queue_tail) /* Process queue */
      {
         p = queue[queue_head++];     /* Dequeue next grid */

         if ((bits0 & (p->bits_0)) || /* Check bits */
             (bits1 & (p->bits_1)) ||
             (bits2 & (p->bits_2)) ||
             (bits3 & (p->bits_3)))
         {
            /* Extract grid value XXX XXX XXX */
            g = pg + *((s16b*)(((byte*)(p))+o2));

            /* Get grid info */
            info = &dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].fdat;
dlog(DEBUGLOS,"cave.c: update_view: step 2 @ %d,%d g %d at %d,%d rel %d,%d fdat %08lx name %s\n", px, py, g,
             GRID_X(g), GRID_Y(g), GRID_X(g)-px, GRID_Y(g)-py, *info,
             f_name + f_info[get_f_idx(dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].mtyp, dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].styp)].name);
            /* Handle light blocking feature */
            if (!(*info & CAVE_LIGHT))
            {
dlog(DEBUGLOS,"cave.c: update_view: feature blocks light\n");
               /* Clear bits */
               bits0 &= ~(p->bits_0);
               bits1 &= ~(p->bits_1);
               bits2 &= ~(p->bits_2);
               bits3 &= ~(p->bits_3);
dlog(DEBUGLOS,"cave.c: update_view: feature blocks light 2, p->d %d radius %d VIEW %d GLOW %d\n",
              p->d, radius, (*info) & CAVE_VIEW, (*info) & CAVE_GLOW);

               /* Newly viewable wall */
               if (!(*info & (CAVE_VIEW)))
               {
dlog(DEBUGLOS,"cave.c: update_view: newly visible wall gets CAVE_VIEW\n");
                  *info |= CAVE_VIEW;

                  /* Torch-lit grids */
                  if (p->d < radius)
                  {
dlog(DEBUGLOS,"cave.c: update_view: wall <radius gets CAVE_LITE|CAVE_VIEW\n");
                     /* Mark as "CAVE_LITE" */
                     *info |= (CAVE_LITE | CAVE_VIEW);
dlog(DEBUGLOS,"cave.c: update_view: feature fdat now %016Lx\n", *info);
                  }

                  /* Perma-lit grids */
                  else if ((*info) & (CAVE_GLOW))
                  {
                     s16b x = GRID_X(g);
                     s16b y = GRID_Y(g);

                     /* Hack -- move towards player */
                     s16b xx = (x < px) ? (x + 1) : (x > px) ? (x - 1) : x;
                     s16b yy = (y < py) ? (y + 1) : (y > py) ? (y - 1) : y;
dlog(DEBUGLOS,"cave.c: update_view: perma-lit, accessing %d,%d\n", xx, yy);

                     /* Check for "simple" illumination */
                     if (dungeon.level[sublevel][yy][xx].fdat & (CAVE_GLOW))
                     {
dlog(DEBUGLOS,"cave.c: update_view: simple illumination: %d,%d gets CAVE_VIEW\n", x, y);
                        /* Mark as seen */
                        *info |= (CAVE_VIEW|CAVE_LITE);
                     }

                  }

dlog(DEBUGLOS,"cave.c: update_view: adding at view_n %d\n", view_n);

                  /* Save in array */
                  if (*info & (CAVE_VIEW)) view_g[view_n++] = g;
               }
            }

            /* Handle non-wall */
            else
            {
dlog(DEBUGLOS,"cave.c: update_view: feature doesn't block light\n");
               /* Enqueue child */
               if (last != p->next_0)
               {
                  queue[queue_tail++] = last = p->next_0;
               }

               /* Enqueue child */
               if (last != p->next_1)
               {
                  queue[queue_tail++] = last = p->next_1;
               }

               /* Newly viewable non-wall */
               if (!(*info & (CAVE_VIEW)))
               {

                  *info |= CAVE_VIEW;

dlog(DEBUGLOS,"cave.c: update_view: newly visible non-wall gets CAVE_VIEW\n");
                  /* Torch-lit grids */
                  if (p->d < radius)
                  {
dlog(DEBUGLOS,"cave.c: update_view: non-wall <radius gets CAVE_LITE|CAVE_VIEW\n");
                     /* Mark as "CAVE_LITE" */
                     *info |= (CAVE_LITE | CAVE_VIEW);
                  }

                  /* Perma-lit grids */
                  else if (*info & (CAVE_GLOW))
                  {
dlog(DEBUGLOS,"cave.c: update_view: non-wall CAVE_GLOW gets CAVE_VIEW\n");
                     *info |= (CAVE_VIEW|CAVE_LITE);
                  }

                  /* Save in array */
                  if (*info | CAVE_VIEW) view_g[view_n++] = g;
               }
            }
         }
      }
   }

   /*** Step 3 -- Complete the algorithm ***/

   /* Handle blindness */
   if (p_ptr->blind)
   {
      /* Process "new" grids */
      for (i = 0; i < view_n; i++)
      {
         /* Grid */
         g = view_g[i];

         /* Grid cannot be "CAVE_LITE" */
         dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].fdat &= ~(CAVE_LITE);
      }
   }

   /* Process "new" grids */
   for (i = 0; i < view_n; i++)
   {
      /* Grid */
      g = view_g[i];

      /* Get grid info */
      info = &dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].fdat;

      /* Was not "CAVE_LITE", is now "CAVE_LITE" */
      /* Was not "CAVE_VIEW", is now "CAVE_VIEW" */
      if ( ((*info & (CAVE_LITE)) && !(*info & (CAVE_OLD_LITE))) ||
           ((*info & (CAVE_VIEW)) && !(*info & (CAVE_OLD_VIEW))) )
      {
         s16b x, y;

         /* Location */
         x = GRID_X(g);
         y = GRID_Y(g);

         /* Note */
         note_spot(x, y);

         /* Redraw */
         lite_spot(x, y);
      }
   }

   /* Process "old" grids */
   for (i = 0; i < old_lite_n; i++)
   {
      /* Grid */
      g = old_lite_g[i];

      /* Get grid info */
      info = &dungeon.level[sublevel][GRID_Y(g)][GRID_X(g)].fdat;

      /* Clear "CAVE_OLD_LITE" flag */
      *info &= ~(CAVE_OLD_LITE | CAVE_OLD_VIEW);

      /* Was "CAVE_LITE", is now not "CAVE_LITE" */
      if (!(*info & (CAVE_LITE | CAVE_VIEW)))
      {
         s16b x, y;

         /* Location */
         x = GRID_X(g);
         y = GRID_Y(g);

         /* Redraw */
         lite_spot(x, y);
      }
   }
dlog(DEBUGLOS,"cave.c: update_view step ending\n");

}

/*
 * Hack -- provide some "speed" for the "flow" code
 * This entry is the "current index" for the "when" field
 * Note that a "when" value of "zero" means "not used".
 *
 * Note that the "cost" indexes from 1 to 127 are for
 * "old" data, and from 128 to 255 are for "new" data.
 *
 * This means that as long as the player does not "teleport",
 * then any monster up to 128 + MONSTER_FLOW_DEPTH will be
 * able to track down the player, and in general, will be
 * able to track down either the player or a position recently
 * occupied by the player.
 */

static s16b flow_n = 0;

/*
 * Hack -- forget the "flow" information
 */
void forget_flow(void)
{

#ifdef MONSTER_FLOW

   s16b x, y;

   /* Nothing to forget */
   if (!flow_n) return;

   /* Check the entire dungeon */
   for (y = 0; y < cur_hgt; y++)
   {
      for (x = 0; x < cur_wid; x++)
      {
         /* Forget the old data */
         dungeon.level[sublevel][y][x].cost = 0;
         dungeon.level[sublevel][y][x].when = 0;
      }
   }

   /* Start over */
   flow_n = 0;

#endif

}

#ifdef MONSTER_FLOW

/*
 * Hack -- Allow us to treat the "seen" array as a queue
 */
static s16b flow_head = 0;
static s16b flow_tail = 0;

/*
 * Take note of a reachable grid.  Assume grid is legal.
 */
static void update_flow_aux(s16b x, s16b y, s16b n)
{
   cave_cell_type *c_ptr;

   s16b old_head = flow_head;


   /* Get the grid */
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Ignore "pre-stamped" entries */
   if (c_ptr->when == flow_n) return;

   /* Ignore "walls" and "rubble" */
   if (is_wall_ptr(c_ptr)) return;

   /* Save the time-stamp */
   c_ptr->when = flow_n;

   /* Save the flow cost */
   c_ptr->cost = n;

   /* Hack -- limit flow depth */
   if (n == MONSTER_FLOW_DEPTH) return;

   /* Enqueue that entry */
   temp_y[flow_head] = y;
   temp_x[flow_head] = x;

   /* Advance the queue */
   if (++flow_head == TEMP_MAX) flow_head = 0;

   /* Hack -- notice overflow by forgetting new entry */
   if (flow_head == flow_tail) flow_head = old_head;
}

#endif


/*
 * Hack -- fill in the "cost" field of every grid that the player
 * can "reach" with the number of steps needed to reach that grid.
 * This also yields the "distance" of the player from every grid.
 *
 * In addition, mark the "when" of the grids that can reach
 * the player with the incremented value of "flow_n".
 *
 * Hack -- use the "seen" array as a "circular queue".
 *
 * We do not need a priority queue because the cost from grid
 * to grid is always "one" and we process them in order.
 */
void update_flow(void)
{

#ifdef MONSTER_FLOW

   s16b x, y, d;

   /* Hack -- disabled */
   if (!flow_by_sound) return;

   /* Paranoia -- make sure the array is empty */
   if (old_lite_n) return;

   /* Cycle the old entries (once per 128 updates) */
   if (flow_n == 255)
   {
      /* Rotate the time-stamps */
      for (y = 0; y < cur_hgt; y++)
      {
         for (x = 0; x < cur_wid; x++)
         {
            s16b w = dungeon.level[sublevel][y][x].when;
            dungeon.level[sublevel][y][x].when = (w > 128) ? (w - 128) : 0;
         }
      }

      /* Restart */
      flow_n = 127;
   }

   /* Start a new flow (never use "zero") */
   flow_n++;


   /* Reset the "queue" */
   flow_head = flow_tail = 0;

   /* Add the player's grid to the queue */
   update_flow_aux(px, py, 0);

   /* Now process the queue */
   while (flow_head != flow_tail)
   {
      /* Extract the next entry */
      x = temp_x[flow_tail];
      y = temp_y[flow_tail];

      /* Forget that entry */
      if (++flow_tail == TEMP_MAX) flow_tail = 0;

      /* Add the "children" */
      for (d = 0; d < 8; d++)
      {
         /* Add that child if "legal" */
         update_flow_aux(x+ddx_ddd[d], y+ddy_ddd[d], dungeon.level[sublevel][y][x].cost+1);
      }
   }

   /* Forget the flow info */
   flow_head = flow_tail = 0;

#endif

}

/*
 * Hack -- map the current panel (plus some) ala "magic mapping"
 */
void map_area(void)
{
   s16b         i, x, y, y1, y2, x1, x2;

   cave_cell_type   *c_ptr;

   /* Pick an area to map */
   x1 = panel_min_col - randint(20);
   x2 = panel_max_col + randint(20);
   y1 = panel_min_row - randint(10);
   y2 = panel_max_row + randint(10);

   /* Speed -- shrink to fit legal bounds */
   if (y1 < 1) y1 = 1;
   if (y2 > cur_hgt-2) y2 = cur_hgt-2;
   if (x1 < 1) x1 = 1;
   if (x2 > cur_wid-2) x2 = cur_wid-2;

   /* Scan that area */
   for (y = y1; y <= y2; y++)
   {
      for (x = x1; x <= x2; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];

         if (!is_wall_ptr(c_ptr))                /* All non-walls are "checked" */
         {
            /* Memorize landmarks */
            if (!is_floor_ptr(c_ptr))
            {
               c_ptr->fdat |= CAVE_MARK;     /* Memorize the object */
               remember_grid(x, y);
            }

            /* Memorize "useful" walls */
            for (i = 0; i < 8; i++)
            {
               c_ptr = &dungeon.level[sublevel][y+ddy_ddd[i]][x+ddx_ddd[i]];

               /* Memorize the "interesting" walls */
               if ( (c_ptr->mtyp == DUNG_PERWALL) ||
                    (c_ptr->mtyp == DUNG_WALL) )
               {
                   c_ptr->fdat |= CAVE_MARK; /* Memorize the walls */
                   remember_grid(x+ddx_ddd[i], y+ddy_ddd[i]);
               }
            }
         }
      }
   }

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

void remember_grid(s16b x, s16b y)
{
#if 0
   cave_cell_type   *c_ptr = &dungeon.level[sublevel][y][x];
   c_ptr->memory_mtyp = c_ptr->mtyp;
   c_ptr->memory_styp = c_ptr->styp;
#endif
}

/*
 * Light up the dungeon.
 *
 */
void wiz_lite(void)
{
/* jk */
   s16b         yy, xx, y, x;
   cave_cell_type   *c_ptr;

   /* Perma-light all open space and adjacent walls */
   for (y = 1; y < cur_hgt-1; y++)
   {
      for (x = 1; x < cur_wid-1; x++)
      {
         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGLIGHT,"cave.c: wiz_lite at %d,%d\n", x, y);
         (void)detect_treasure_xy(x, y);
         (void)detect_sdoor_xy(x, y);
         (void)detect_trap_xy(x, y);
         (void)detect_object_xy(x, y);
         if (c_ptr->m_idx)
         {
            monster_type   *m_ptr = &mn_list[c_ptr->m_idx];
            char m_name[80];

            /* Paranoia -- Skip dead monsters */
            if (!m_ptr->r_idx) continue;
            if (randint(100)<20) continue;

            /* necessary to re-calculate m_ptr->ml here */
            update_mon(c_ptr->m_idx, TRUE);

            if (m_ptr->csleep)
            {
               /* Wake up */
               m_ptr->csleep = 0;
               /* Acquire the monster name */
               monster_desc(m_name, m_ptr, 0);

               if (m_ptr->ml)
               {
                  msg_format("%s wakes up.", m_name);
               }
            }
         }   

         if (!is_wall_ptr(c_ptr)) /* Process all non-walls */
         {

dlog(DEBUGLIGHT,"cave.c: wiz_lite at %d,%d not wall\n", x, y);
            /* Perma-lite all grids touching those grids */
            for (yy = y - 1; yy <= y + 1; yy++)
            {
                for (xx = x - 1; xx <= x + 1; xx++)
                {
                   /* Get the grid */
                   c_ptr = &dungeon.level[sublevel][yy][xx];

                   /* Perma-lite the grid */
                   c_ptr->fdat |= (CAVE_GLOW);

dlog(DEBUGLIGHT,"cave.c: wiz_lite at %d,%d touching, adding CAVE_GLOW\n", xx, yy);
                   /* XXX XXX XXX Hack -- memorize landmarks */
                   if (!is_floor_ptr(c_ptr))
                   {

dlog(DEBUGLIGHT,"cave.c: wiz_lite at %d,%d touching, non_floor adding CAVE_MARK\n", xx, yy);
                      c_ptr->fdat |= CAVE_MARK;
                      remember_grid(xx, yy);
                   }
                   /* XXX XXX XXX Hack -- memorize if requested */
                   if (view_perma_grids) c_ptr->fdat |= CAVE_MARK;

dlog(DEBUGLIGHT,"cave.c: wiz_lite at %d,%d touching, perma_grids adding CAVE_MARK\n", xx, yy);
               }
            }
         }
      }
   }

   /* Update the monsters */
   p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void wiz_dark(void)
{
/* jk */
   s16b        x, y, j;

   /* Forget every grid */
   for (y = 0; y < cur_hgt; y++)
   {
      for (x = 0; x < cur_wid; x++)
      {
         cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

         /* Process the grid */
         c_ptr->fdat &= ~CAVE_MARK;

         /* Forget the object */
         for (j = objects_on_floor(x,y)-1;j>=0;j--)
         {
            object_type *i_ptr = get_item_pointer_floor_xy(j,x,y);

            /* Memorize */
            i_ptr->marked = FALSE;
         }
      }
   }

    /* Update the view & the monsters */
   p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/* jk - a chance % chance of forgetting map squares */
/* the functions makes circular spots of memory loss on the map until */
/* we have forgotten enough squares. Overlapping spots are not detected */
bool wiz_forget_some_map(s16b chance)
{
   s16b        x,y,x1,y1,rad,j;
   cave_cell_type  *c_ptr;
   bool       result = FALSE;

   s32b total = cur_hgt*cur_wid*chance/100; /* total squares to forget */

   s32b done  = 0;
   while (done<total)
   {
      x = rand_int(cur_wid);
      y = rand_int(cur_hgt);
      rad = rand_int(30);
      if ((x-rad<0) || ((x+rad)>=cur_wid)) continue;
      if ((y-rad<0) || ((y+rad)>=cur_hgt)) continue;
      for (x1=x-rad;x1<=x+rad;x1++)
      {
         for (y1=y-rad;y1<=y+rad;y1++)
         {
            if (distance(x1,y1,x,y)>=rad) continue; /* enforce circular */
            done++;
            c_ptr = &dungeon.level[sublevel][y1][x1];
            /* Process the grid */
            result |= (c_ptr->fdat & CAVE_MARK);
            c_ptr->fdat &= ~CAVE_MARK;
            if (!floor_grid_bold(x1,y1) || (!c_ptr->i_idx)) continue;
            /* Forget the object */
            for (j = objects_on_floor(x1,y1)-1;j>=0;j--)
            {
               object_type *i_ptr = get_item_pointer_floor_xy(j,x1,y1);
               result |=(i_ptr->marked==TRUE);
               i_ptr->marked = FALSE;
            }
         }
      }
      done++;
   }

   /* Mega-Hack -- Forget the view and lite */
   p_ptr->update |= PU_UN_VIEW;

   /* Update the view and lite */
   p_ptr->update |= PU_VIEW;

   /* Update the monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
   return (result);
}

/*
 * Calculate "incremental motion". Used by project() and shoot().
 * Assumes that (*y,*x) lies on the path from (y1,x1) to (y2,x2).
 */
void mmove2(s16b *x, s16b *y, s16b x1, s16b y1, s16b x2, s16b y2)
{
   s16b dy, dx, dist, shift;

   /* Extract the distance travelled */
   dy = (*y < y1) ? y1 - *y : *y - y1;
   dx = (*x < x1) ? x1 - *x : *x - x1;

   /* Number of steps */
   dist = (dy > dx) ? dy : dx;

   /* We are calculating the next location */
   dist++;

   /* Calculate the total distance along each axis */
   dy = (y2 < y1) ? (y1 - y2) : (y2 - y1);
   dx = (x2 < x1) ? (x1 - x2) : (x2 - x1);

   /* Paranoia -- Hack -- no motion */
   if (!dy && !dx) return;

   /* Move mostly vertically */
   if (dy > dx)
   {
      /* Extract a shift factor */
      shift = (dist * dx + (dy-1) / 2) / dy;

      /* Sometimes move along the minor axis */
      (*x) = (x2 < x1) ? (x1 - shift) : (x1 + shift);

      /* Always move along major axis */
      (*y) = (y2 < y1) ? (y1 - dist) : (y1 + dist);
   }

   /* Move mostly horizontally */
   else
   {
      /* Extract a shift factor */
      shift = (dist * dy + (dx-1) / 2) / dx;

      /* Sometimes move along the minor axis */
      (*y) = (y2 < y1) ? (y1 - shift) : (y1 + shift);

      /* Always move along major axis */
      (*x) = (x2 < x1) ? (x1 - dist) : (x1 + dist);
   }
}

/*
 * Determine the path taken by a projection.
 *
 * The projection will always start from the grid (y1,x1), and will travel
 * towards the grid (y2,x2), touching one grid per unit of distance along
 * the major axis, and stopping when it enters the destination grid or a
 * wall grid, or has travelled the maximum legal distance of "range".
 *
 * Note that "distance" in this function (as in the "update_view()" code)
 * is defined as "MAX(dy,dx) + MIN(dy,dx)/2", which means that the player
 * actually has an "octagon of projection" not a "circle of projection".
 *
 * The path grids are saved into the grid array pointed to by "gp", and
 * there should be room for at least "range" grids in "gp".  Note that
 * due to the way in which distance is calculated, this function normally
 * uses fewer than "range" grids for the projection path, so the result
 * of this function should never be compared directly to "range".  Note
 * that the initial grid (y1,x1) is never saved into the grid array, not
 * even if the initial grid is also the final grid.  XXX XXX XXX
 *
 * The "flg" flags can be used to modify the behavior of this function.
 *
 * In particular, the "PROJECT_STOP" and "PROJECT_THRU" flags have the same
 * semantics as they do for the "project" function, namely, that the path
 * will stop as soon as it hits a monster, or that the path will continue
 * through the destination grid, respectively.
 *
 * The "PROJECT_JUMP" flag, which for the "project()" function means to
 * start at a special grid (which makes no sense in this function), means
 * that the path should be "angled" slightly if needed to avoid any wall
 * grids, allowing the player to "target" any grid which is in "view".
 * This flag is non-trivial and has not yet been implemented, but could
 * perhaps make use of the "vinfo" array (above).  XXX XXX XXX
 *
 * This function returns the number of grids (if any) in the path.  This
 * function will return zero if and only if (y1,x1) and (y2,x2) are equal.
 *
 * This algorithm is similar to, but slightly different from, the one used
 * by "update_view_los()", and very different from the one used by "los()".
 */
sint project_path(u16b *gp, int range, int x1, int y1, int x2, int y2, int flg)
{
   int x, y;

   int n = 0;
   int k = 0;

   /* Absolute */
   int ax, ay;

   /* Offsets */
   int sx, sy;

   /* Fractions */
   int frac;

   /* Scale factors */
   int full, half;

   /* Slope */
   int m;

   /* No path necessary (or allowed) */
   if ((x1 == x2) && (y1 == y2)) return (0);

   /* Analyze "dy" */
   if (y2 < y1)
   {
      ay = (y1 - y2);
      sy = -1;
   }
   else
   {
      ay = (y2 - y1);
      sy = 1;
   }

   /* Analyze "dx" */
   if (x2 < x1)
   {
      ax = (x1 - x2);
      sx = -1;
   }
   else
   {
      ax = (x2 - x1);
      sx = 1;
   }


   /* Number of "units" in one "half" grid */
   half = (ay * ax);

   /* Number of "units" in one "full" grid */
   full = half << 1;


   /* Vertical */
   if (ay > ax)
   {
      /* Start at tile edge */
      frac = ax * ax;

      /* Let m = ((dx/dy) * full) = (dx * dx * 2) = (frac * 2) */
      m = frac << 1;

      /* Start */
      y = y1 + sy;
      x = x1;

      /* Create the projection path */
      while (1)
      {
         /* Save grid */
         gp[n++] = GRID(x,y);

         /* Hack -- Check maximum range */
         if ((n + (k >> 1)) >= range) break;

         /* Sometimes stop at destination grid */
         if (!(flg & (PROJECT_THRU)))
         {
            if ((x == x2) && (y == y2)) break;
         }

         /* Always stop at non-initial wall grids */
         if ((n > 0) && !floor_grid_bold(x, y)) break;

         /* Sometimes stop at non-initial monsters/players */
         if (flg & (PROJECT_STOP))
         {
            if ((n > 0) && (dungeon.level[sublevel][y][x].m_idx != 0)) break;
         }

         /* Slant */
         if (m)
         {
            /* Advance (X) part 1 */
            frac += m;

            /* Horizontal change */
            if (frac >= half)
            {
               /* Advance (X) part 2 */
               x += sx;

               /* Advance (X) part 3 */
               frac -= full;

               /* Track distance */
               k++;
            }
         }

         /* Advance (Y) */
         y += sy;
      }
   }

   /* Horizontal */
   else if (ax > ay)
   {
      /* Start at tile edge */
      frac = ay * ay;

      /* Let m = ((dy/dx) * full) = (dy * dy * 2) = (frac * 2) */
      m = frac << 1;

      /* Start */
      y = y1;
      x = x1 + sx;

      /* Create the projection path */
      while (1)
      {
         /* Save grid */
         gp[n++] = GRID(x,y);

         /* Hack -- Check maximum range */
         if ((n + (k >> 1)) >= range) break;

         /* Sometimes stop at destination grid */
         if (!(flg & (PROJECT_THRU)))
         {
            if ((x == x2) && (y == y2)) break;
         }

         /* Always stop at non-initial wall grids */
         if ((n > 0) && !floor_grid_bold(x, y)) break;

         /* Sometimes stop at non-initial monsters/players */
         if (flg & (PROJECT_STOP))
         {
            if ((n > 0) && (dungeon.level[sublevel][y][x].m_idx != 0)) break;
         }

         /* Slant */
         if (m)
         {
            /* Advance (Y) part 1 */
            frac += m;

            /* Vertical change */
            if (frac >= half)
            {
               /* Advance (Y) part 2 */
               y += sy;

               /* Advance (Y) part 3 */
               frac -= full;

               /* Track distance */
               k++;
            }
         }

         /* Advance (X) */
         x += sx;
      }
   }

   /* Diagonal */
   else
   {
      /* Start */
      y = y1 + sy;
      x = x1 + sx;

      /* Create the projection path */
      while (1)
      {
         /* Save grid */
         gp[n++] = GRID(x,y);

         /* Hack -- Check maximum range */
         if ((n + (n >> 1)) >= range) break;

         /* Sometimes stop at destination grid */
         if (!(flg & (PROJECT_THRU)))
         {
            if ((x == x2) && (y == y2)) break;
         }

         /* Always stop at non-initial wall grids */
         if ((n > 0) && !floor_grid_bold(x, y)) break;

         /* Sometimes stop at non-initial monsters/players */
         if (flg & (PROJECT_STOP))
         {
            if ((n > 0) && (dungeon.level[sublevel][y][x].m_idx != 0)) break;
         }

         /* Advance (Y) */
         y += sy;

         /* Advance (X) */
         x += sx;
      }
   }


   /* Length */
   return (n);
}

/*
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming no monster gets in the way.
 *
 * This is slightly (but significantly) different from "los(x1,y1,x2,y2)".
 */
bool projectable(s16b x1, s16b y1, s16b x2, s16b y2)
{
   s16b dist, x, y;

   /* Start at the initial location */
   x = x1;
   y = y1;

   /* See "project()" */
   for (dist = 0; dist < MAX_RANGE; dist++)
   {
      /* Never pass through walls */
      if (dist && !floor_grid_bold(x, y)) break;

      /* Check for arrival at "final target" */
      if ((x == x2) && (y == y2)) return (TRUE);

      /* Calculate the new location */
      mmove2(&x, &y, x1, y1, x2, y2);
   }

   /* Assume obstruction */
   return (FALSE);
}

/*
 * Standard "find me a location" function
 *
 * Obtains a legal location within the given distance of the initial
 * location, and with "los()" from the source to destination location
 *
 * This function is often called from inside a loop which searches for
 * locations while increasing the "d" distance.
 *
 * Currently the "m" parameter is unused.
 */
void scatter(s16b *xp, s16b *yp, s16b x, s16b y, s16b d, s16b m)
{
   s16b nx, ny;

   /* Unused */
   m = m;

   /* Pick a location */
   while (TRUE)
   {
      /* Pick a new location */
      nx = rand_spread(x, d);
      ny = rand_spread(y, d);
      /* Ignore illegal locations and outer walls */
      if (!in_bounds(nx, ny)) continue;

      /* Ignore "excessively distant" locations */
      if ((d > 1) && (distance(x, y, nx, ny) > d)) continue;
      /* Require "line of sight" */
      if (los(x, y, nx, ny)) break;
   }

   /* Save the location */
   (*xp) = nx;
   (*yp) = ny;
}

/*
 * Track a new monster
 */
void health_track(s16b m_idx)
{
   /* Track a new guy */
   health_who = m_idx;

   /* Redraw (later) */
   p_ptr->redraw1 |= (PR1_HEALTH);
}

/*
 * Hack -- track the given monster race
 */
void monster_race_track(s16b r_idx)
{
   /* Save this monster ID */
   p_ptr->monster_race_idx = r_idx;

   /* Window stuff */
   p_ptr->window |= (PW_MONSTER);
}

/*
 * Hack -- track the given object kind
 */
void object_kind_track(s16b k_idx)
{
   /* Save this object ID */
   p_ptr->object_kind_idx = k_idx;

   /* Window stuff */
   p_ptr->window |= (PW_OBJECT);
}

/*
 * Something has happened to disturb the player.
 *
 * The first arg indicates a major disturbance, which affects search.
 *
 * The second arg is currently unused, but could induce output flush.
 *
 * All disturbance cancels repeated commands, resting, and running.
 */
void disturb(s16b stop_search, s16b unused_flag)
{
   /* Unused */
   unused_flag = unused_flag;

   /* Cancel auto-commands */
   /* command_new = 0; */

   /* Cancel repeated commands */
   if (p_ptr->command_rep)
   {
       /* Cancel */
       p_ptr->command_rep = 0;

       /* Redraw the state (later) */
       p_ptr->redraw1 |= (PR1_STATE);
   }

   /* Cancel Resting */
   if (p_ptr->resting)
   {
       /* Cancel */
       p_ptr->resting = 0;

       /* Redraw the state (later) */
       p_ptr->redraw1 |= (PR1_STATE);
   }

/* jk */
   /* don't disturb on the first turn */
   if ((p_ptr->reading) && (p_ptr->reading<max_reading))
   {
      p_ptr->reading=0;
      read_spell(NULL, 0);
   }

   /* Cancel running */
   if (p_ptr->running)
   {
dlog(DEBUGMOVES,"cave.c: disturb: stopped running\n");
       /* Cancel */
       p_ptr->running = 0;

       /* Calculate torch radius */
       p_ptr->update |= (PU_TORCH);
   }

   /* Cancel searching if requested */
   if (stop_search && p_ptr->searching)
   {
       /* Cancel */
       p_ptr->searching = FALSE;

       /* Recalculate bonuses */
       p_ptr->update |= (PU_BONUS);

       /* Redraw the state */
       p_ptr->redraw1 |= (PR1_STATE);
   }

   /* Flush the input if requested */
   if (flush_disturb) flush();
}

/*
 * Hack -- Check if a level is a "quest" level
 */
bool is_quest(s16b level)
{
   s16b i;

   /* Town is never a quest */
   if (!level) return (FALSE);

   /* Check quests */
   for (i = 0; i < MAX_Q_IDX; i++)
   {
       /* Check for quest */
       if (q_list[i].level == level) return (TRUE);
   }

   /* Nope */
   return (FALSE);
}

void wipe_old_level(s16b level)
{
   s16b i,j;

   wipe_i_list();
   wipe_mn_list();
   wipe_t_list();
   wipe_is_list();
   /* wipe all stores except home, which always lives at 0 */
   for (i = 1; i < MAX_STORES_LEVEL; i++)
   {
      store_init(i);
   }

   /*
    * if we build a town, start with 0 stores
    * if we build a normal level, start with 1 store to keep (home)
    */
   num_stores = (p_ptr->mdepth == 0) ? 0 : 1;

   for (i = 0; i < MAX_SUB_LEVELS; i++)
   {
      if (i==0)
      {
         for (j = 0; j < MAX_HGT; j++)
         {
            C_WIPE(dungeon.level[i][j], MAX_WID, cave_cell_type);
         }
      }
      else if (dungeon.level_used[i])
      {
         for (j = 0; j < MAX_HGT; j++)
         {
            /* un-allocate rows of this sublevel */
            C_FREE(dungeon.level[i][j], SCREEN_WID, cave_cell_type);
         }
      }
      dungeon.level_used[i] = FALSE;
   }
}

void lava_cool_down(void)
{
   s16b x, y;
   for (y = 0; y < cur_hgt; y++)
   {
      for (x = 0; x < cur_wid; x++)
      {
         /* Clear all features, set to granite */
         if (test_grid_ptr(&dungeon.level[sublevel][y][x], DUNG_LAVA, DUNG_LAVA_NORMAL))
         {
            if (randint(5)==1)
            {
               (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_ADD, 0);
               if (player_has_los_bold(x,y))
               {
                  msg_print("You see the lava cool down.");
                  /* Notice */
                  note_spot(x, y);
   
                  /* Redraw */
                  lite_spot(x, y);
                  p_ptr->update |= PU_VIEW;
               }
            }
         }
      }
   }
}
