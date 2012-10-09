/* generate.c */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Note that Level generation is *not* an important bottleneck,
 * though it can be annoyingly slow on older machines...  Thus
 * we emphasize "simplicity" and "correctness" over "speed".
 *
 * This entire file is only needed for generating levels.
 * This may allow smart compilers to only load it when needed.
 *
 * Consider the "v_info.txt" file for vault generation.
 *
 * In this file, we use the "special" granite and perma-wall sub-types,
 * where "basic" is normal, "inner" is inside a room, "outer" is the
 * outer wall of a room, and "solid" is the outer wall of the dungeon
 * or any walls that may not be pierced by corridors.  Thus the only
 * wall type that may be pierced by a corridor is the "outer granite"
 * type.  The "basic granite" type yields the "actual" corridors.
 *
 * Note that we use the special "solid" granite wall type to prevent
 * multiple corridors from piercing a wall in two adjacent locations,
 * which would be messy, and we use the special "outer" granite wall
 * to indicate which walls "surround" rooms, and may thus be "pierced"
 * by corridors entering or leaving the room.
 *
 * Note that a tunnel which attempts to leave a room near the "edge"
 * of the dungeon in a direction toward that edge will cause "silly"
 * wall piercings, but will have no permanently incorrect effects,
 * as long as the tunnel can *eventually* exit from another side.
 * And note that the wall may not come back into the room by the
 * hole it left through, so it must bend to the left or right and
 * then optionally re-enter the room (at least 2 grids away).  This
 * is not a problem since every room that is large enough to block
 * the passage of tunnels is also large enough to allow the tunnel
 * to pierce the room itself several times.
 *
 * Note that no two corridors may enter a room through adjacent grids,
 * they must either share an entryway or else use entryways at least
 * two grids apart.  This prevents "large" (or "silly") doorways.
 *
 * To create rooms in the dungeon, we first divide the dungeon up
 * into "blocks" of 11x10 grids each, and require that all rooms
 * occupy a rectangular group of blocks.  As long as each room type
 * reserves a sufficient number of blocks, the room building routines
 * will not need to check bounds.  Note that most of the normal rooms
 * actually only use 23x10 grids, and so reserve 33x10 grids.
 *
 * Note that the use of 11x10 blocks (instead of the old 33x10 blocks)
 * allows more variability in the horizontal placement of rooms, and
 * at the same time has the disadvantage that some rooms (two thirds
 * of the normal rooms) may be "split" by panel boundaries.  This can
 * induce a situation where a player is in a room and part of the room
 * is off the screen.  It may be annoying enough to go back to 33x10
 * blocks to prevent this visual situation.
 *
 * Note that the dungeon generation routines are much different (2.7.5)
 * and perhaps "DUN_ROOMS" should be less than 50.
 *
 * XXX XXX XXX Note that it is possible to create a room which is only
 * connected to itself, because the "tunnel generation" code allows a
 * tunnel to leave a room, wander around, and then re-enter the room.
 *
 * XXX XXX XXX Note that it is possible to create a set of rooms which
 * are only connected to other rooms in that set, since there is nothing
 * explicit in the code to prevent this from happening.  But this is less
 * likely than the "isolated room" problem, because each room attempts to
 * connect to another room, in a giant cycle, thus requiring at least two
 * bizarre occurances to create an isolated section of the dungeon.
 *
 * Note that (2.7.9) monster pits have been split into monster "nests"
 * and monster "pits".  The "nests" have a collection of monsters of a
 * given type strewn randomly around the room (jelly, animal, or undead),
 * while the "pits" have a collection of monsters of a given type placed
 * around the room in an organized manner (orc, troll, giant, dragon, or
 * demon).  Note that both "nests" and "pits" are now "level dependant",
 * and both make 16 "expensive" calls to the "get_mon_num()" function.
 *
 * Note that the cave grid flags changed in a rather drastic manner
 * for Angband 2.8.0 (and 2.7.9+), in particular, dungeon terrain
 * features, such as doors and stairs and traps and rubble and walls,
 * are all handled as a set of 64 possible "terrain features", and
 * not as "fake" objects (440-479) as in pre-2.8.0 versions.
 *
 * The 64 new "dungeon features" will also be used for "visual display"
 * but we must be careful not to allow, for example, the user to display
 * hidden traps in a different way from floors, or secret doors in a way
 * different from granite walls, or even permanent granite in a different
 * way from granite.  XXX XXX XXX
 */


/*
 * Dungeon generation values
 */
/* jk - this was 50 with ratio 3*/
/* jk - 060199 18 -> 30 */
#define DUN_ROOMS       30*RATIO      /* Number of rooms to attempt                                    */
#define DUN_UNUSUAL     30*RATIO      /* 1/chance of unusual room                                      */
#define DUN_DEST        15            /* 1/chance of having a destroyed level                          */
#define DUN_OVERLAP     10            /* 1/chance of having a overlapping level                        */
#define OVERLAP_CHANCE  10            /* 1/chance of allowing an overlapping room in overlapping level */
#define DUN_MAZE        85            /* 1/chance of having a large part of the dungeon as maze        */
#define DUN_STORES      100           /* per 1000 rooms, if we pass dun_unusal!                        */
#define DUN_INRVT_RATIO 10            /* number of rooms needed to try one inner vault                 */
/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND     15      /* Chance of random direction          */
#define DUN_TUN_CHG     20      /* Chance of changing direction        */
#define DUN_TUN_CON     10      /* Chance of extra tunneling           */
#define DUN_TUN_PEN     25      /* Chance of doors at room entrances   */
#define DUN_TUN_JCT     70      /* Chance of doors at tunnel junctions */
/* jk */
#define DUN_TUN_WID      5      /* chance of widened tunnels           */
                                /* this affects whole tunnels          */
#define DUN_TUN_DIA      0      /* chance of diagonal tunnels          */
#define DUN_TUN_TRAP     2      /* chance of random traps in tunnels   */

/*
 * Dungeon streamer generation values
 */
/* jk - was 5 2 3 90 2 40 */
#define DUN_STR_DEN     3       /* Density of streamers */
#define DUN_STR_RNG     2       /* Width of streamers */
#define DUN_STR_MAG     5       /* Number of magma streamers */
#define DUN_STR_MGC     30      /* 1/chance of treasure per magma */
#define DUN_STR_QUA     5       /* Number of quartz streamers */
#define DUN_STR_QZC     30      /* 1/chance of treasure per quartz */
#define DUN_STR_OTH     1       /* Number of other streamers */
#define DUN_STR_OTC     30      /* 1/chance of treasure per quartz */


/*
 * Dungeon treasure allocation values
 */
 /* jk - this was 9 3 3 */
#define DUN_AMT_ROOM    (8*RATIO*RATIO)/15  /* Amount of objects for rooms */
#define DUN_AMT_ITEM    (3*RATIO*RATIO)/15  /* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD    (3*RATIO*RATIO)/15  /* Amount of treasure for rooms/corridors */

/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR          1       /* Hallway */
#define ALLOC_SET_ROOM          2       /* Room */
#define ALLOC_SET_BOTH          3       /* Anywhere */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE        1       /* Rubble */
#define ALLOC_TYP_TRAP          3       /* Trap */
#define ALLOC_TYP_GOLD          4       /* Gold */
#define ALLOC_TYP_OBJECT        5       /* Object */

/*
 * Maximum numbers of rooms along each axis (currently 6x6)
 */
#define MAX_ROOMS_ROW   (MAX_HGT / BLOCK_HGT)
#define MAX_ROOMS_COL   (MAX_WID / BLOCK_WID)

/*
 * Bounds on some arrays used in the "dun_data" structure.
 * These bounds are checked, though usually this is a formality.
 */
/* jk - this was 100 200 500 900 */
#define CENT_MAX         80*RATIO
#define DOOR_MAX        160*RATIO
#define WALL_MAX        400*RATIO
#define TUNN_MAX        720*RATIO

/*
 * Maximal number of room types
 */
#define ROOM_MAX        19

/*
 * chance in 100 times of round rooms being generated
 */
#define ROOM_ROUND_CHANCE 40

/*
 * how big is the chance that if a door pierces a wall such that
 * you can go around the door by moving diagonally, a new wall
 * is built instead of the piercing not allowed.
 *
 * I suggest setting this high enough so as to not create long
 * generation times
 */
#define DUN_TUN_NEW_WALL 90

/*
 * Simple structure to hold a map location
 */

/*
 * Room type information
 */

typedef struct room_data room_data;

struct room_data
{
   /* Required size in blocks */
   s16b dy1, dy2, dx1, dx2;

   /* Hack -- minimum level */
   s16b level;

   /* is this a normal room, not a vault */
   bool normal;
};

/*
 * Structure to hold all "dungeon generation" data
 */

typedef struct dun_data dun_data;

struct dun_data
{
   /* Array of centers of rooms */
   s16b cent_n;
   coord cent[CENT_MAX];

   /* Array of possible door locations */
   s16b door_n;
   coord door[DOOR_MAX];

   /* Array of wall piercing locations */
   s16b wall_n;
   coord wall[WALL_MAX];

   /* Array of tunnel grids */
   s16b tunn_n;
   coord tunn[TUNN_MAX];

   /* Number of blocks along each axis */
   s16b row_rooms;
   s16b col_rooms;

   /* Array of which blocks are used */
   /* 0 = not used yet                         */
   /* 1 = used for room, may overlap           */
   /* 2 = used for maze/vault, may not overlap */
   byte room_map[MAX_ROOMS_COL][MAX_ROOMS_ROW];

   /* Hack -- there is a pit/nest on this level */
   bool crowded;
};

/*
 * Dungeon generation data -- see "cave_gen()"
 */
static dun_data *dun;

/*
 * Array of room types (assumes 11x10 blocks)
 *
 *  dy1, dy2, dx1, dx2, minimum level, normal
*/

static room_data room[ROOM_MAX] =
{
   {  0, 0,  0, 0,  0, FALSE },        /* 0 = Nothing                                 */
   {  0, 0, -1, 1,  1, TRUE  },        /* 1 = Simple (33x10)                          */
   {  0, 0, -1, 1,  1, TRUE  },        /* 2 = Overlapping (33x10)                     */
   {  0, 0, -1, 1,  3, TRUE  },        /* 3 = Crossed (33x10)                         */
   { -1, 1,  0, 1,  3, TRUE  },        /* 4 = Large (33x20)                           */
   {  0, 0, -1, 1,  5, FALSE },        /* 5 = Monster nest (33x10)                    */
   {  0, 0, -1, 1,  5, FALSE },        /* 6 = Monster pit (33x10)                     */
   {  0, 1, -1, 1,  5, FALSE },        /* 7 = Lesser vault (33x20)                    */
   { -1, 2, -2, 3, 10, FALSE },        /* 8 = Greater vault (66x40)                   */
   {  0, 1, -1, 1,  2, FALSE },        /* 9 = smaller treasure chamber  33x20         */
   {  0, 1, -1, 2,  5, FALSE },        /* 10 = middle treasure chamber  35x12         */
   { -1, 1, -1, 1, 20, FALSE },        /* 11 = greater treasure chamber 23x24         */
   {  0, 0,  0, 0,  2, FALSE },        /* 12 = smaller graveyard 9x6 = 11x10          */
   {  0, 0,  0, 1,  5, FALSE },        /* 13 = middle graveyard 19x6 = 22x10          */
   {  0, 1,  0, 1, 20, FALSE },        /* 14 = greater graveyard 19x13 = 22x20        */
   {  0, 0,  0, 0, 10, FALSE },        /* 15 = store 9x9 = 11x10                      */
   {  0, 1,  0, 1,  1, TRUE  },        /* 16 = Round (22x20)                          */
   {  0, 1, -2, 3,  0, FALSE },        /* 17 = multi-level room (66 x 20)             */
   {  0, 0,  0, 0,  1, FALSE }         /* 18 = inner room filling (11x10)             */
};

s16b rooms_built[ROOM_MAX];

s16b roomn_max[ROOM_MAX][2] =
/* entry 1: max rooms before looking at chance */
/* entry 2: chance in 1000 that another room of this type will succeed */
{
  {  0,  0 },          /* 0 is an unused entry, since no rooms of type 0 exist */
  { DUN_ROOMS, 1000 }, /* Type  1 -- Trivial              */
  { 10, 200 },         /* Type  2 -- Overlapping          */
  { 10, 200 },         /* Type  3 -- Cross room           */
  {  5,  50 },         /* Type  4 -- Large room           */
  {  2,  50 },         /* Type  5 -- Monster nest         */
  {  1,  50 },         /* Type  6 -- Monster pit          */
  {  2,  40 },         /* Type  7 -- Lesser vault         */
  {  1,  20 },         /* Type  8 -- Greater vault        */
  {  2, 100 },         /* Type  9 -- Small Treasure Room  */
  {  1,  50 },         /* Type 10 -- Middle Treasure Room */
  {  1,  20 },         /* Type 11 -- Large Treasure Room  */
  {  2, 100 },         /* Type 12 -- Lesser Graveyard     */
  {  1, 500 },         /* Type 13 -- Middle Graveyard     */
  {  1, 200 },         /* Type 14 -- Greater Graveyard    */
  {  1,   5 },         /* Type 15 -- Store                */
  { DUN_ROOMS, 1000 }, /* Type 16 -- Round                */
  { 3, 100 }           /* type 17 -- multi-level vault    */
};

/*
 * this is used to convey the current vault number to
 * vault_satisfy_monster_flags()
 */
s16b current_vault = 0;

/*
 * Always picks a correct direction
 *
 * note that diagonal is now possible, since weird_passages() removes
 * any weird diagonal pass-through situations.
 */
static void correct_dir(s16b *dx, s16b *dy,
                        s16b x_cur, s16b y_cur,
                        s16b x_end, s16b y_end)
{
   /* Extract vertical and horizontal directions */
   *dx = (x_cur == x_end) ? 0 : ( (x_cur < x_end) ? 1 : -1);
   *dy = (y_cur == y_end) ? 0 : ( (y_cur < y_end) ? 1 : -1);

dlog(DEBUGGENER2,"generate.c: correct_dir: dx,dy %d,%d x_cur,y_cur %d,%d x_end,y_end %d,%d\n",
                 (*dx), (*dy), x_cur, y_cur, x_end, y_end);
   /*
    * not always move diagonally, but do move diagonally if we are currently
    * out of bounds
    */
   if ( (*dx && *dy) && (randint(100)>DUN_TUN_DIA) && in_bounds(x_cur, y_cur))
   {
      if (rand_int(100) < 50)
      {
         *dx = 0;
      }
      else
      {
         *dy = 0;
      }
dlog(DEBUGGENER2,"generate.c: correct_dir: undiagonalized, dx,dy %d,%d\n", (*dx), (*dy));
   }

   if (x_cur < 5)
   {
dlog(DEBUGGENER2,"generate.c: correct_dir: too close to left border, dx,dy 1,0 \n");
      (*dx)=1;
      (*dy)=0;
   }
   else if (x_cur > (cur_wid - 5))
   {
dlog(DEBUGGENER2,"generate.c: correct_dir: too close to right border, dx,dy -1,0 \n");
      (*dx)=-1;
      (*dy)=0;
   }
   else if (y_cur < 5)
   {
dlog(DEBUGGENER2,"generate.c: correct_dir: too close to top border, dx,dy 0,1 \n");
      (*dx)=0;
      (*dy)=1;
   }
   else if (y_cur > (cur_hgt - 5))
   {
dlog(DEBUGGENER2,"generate.c: correct_dir: too close to bottom border, dx,dy 0,-1 \n");
      (*dx)=0;
      (*dy)=-1;
   }
}

/*
 * Pick a random direction
 */
static void rand_dir(s16b *dx, s16b *dy)
{
   /* Pick a random direction */
   s16b i = rand_int(4);

   /* Extract the dy/dx components */
   *dx = ddy_ddd[i];
   *dy = ddx_ddd[i];
}

/*
 * Returns random co-ordinates for player/monster/object
 */
void new_player_spot(bool in_town)
{
   s16b        x, y;
   u32b        count = 0;

   /* Place the player */
   while (count++ < 10000)
   {
      /* Pick a legal spot */
      if (in_town)
      {
         x = rand_range(1, SCREEN_WID - 2);
         y = rand_range(1, SCREEN_HGT - 2);
      }
      else
      {
         x = rand_range(1, cur_wid - 2);
         y = rand_range(1, cur_hgt - 2);
      }
      /* Must be a "naked" floor grid */
      if (!naked_grid_bold(x, y)) continue;

      /* Refuse to start on anti-teleport grids */
      if (dungeon.level[sublevel][y][x].fdat & CAVE_VAULT) continue;

      /* Done */
      break;
   }

   /* Save the new player grid */
   px = x;
   py = y;
}

/*
 * Convert existing terrain type to rubble
 */
static void place_rubble(s16b x, s16b y)
{
   if (!in_bounds(x, y)) return;

   /* Clear previous contents, add rubble */
   (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_RUBBLE, GRID_KEEP, 0);
}

/*
 * Place a locked door at the given location
 */
static void place_locked_door(s16b x, s16b y)
{
   cave_cell_type *c_ptr;

   if (!in_bounds(x, y)) return;

   c_ptr = &dungeon.level[sublevel][y][x];

   /* don't place doors where monsters already exist */
   if (c_ptr->m_idx != 0)
   {
      return;
   }

   /* Clear previous contents, add locked door */
   (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_LOCKED, GRID_KEEP, 0); c_ptr->extra = rand_int(15);
   if (rand_int(10000)<CHANCE_TRAP_DOOR)
   {
      /* make sure lower levels get a less traps */
      if ((p_ptr->mdepth<10) || (randint(p_ptr->mdepth)!=1))
      {
         place_traps_door(x,y,p_ptr->mdepth, (40-p_ptr->mdepth), (40-p_ptr->mdepth/4));
      }
   }
}


/*
 * Convert existing terrain type to "up stairs"
 */
void place_main_up_stair(s16b x, s16b y, bool deep)
{
   if (!in_bounds(x, y)) return;

   if (!deep)
   {
      (void)set_grid_type(x, y, DUNG_STAIR, DUNG_STAIR_UP, GRID_KEEP, 0);
      point_stair_to_level(x, y, p_ptr->mdepth - 1, 0);
   }
   else
   {
      s16b target = p_ptr->mdepth - randint(3) - 1;

      /* can we make this shaft here? */
      if (target >= 0)
      {
         (void)set_grid_type(x, y, DUNG_STAIR,
                             DUNG_STAIR_SHAFTUP, GRID_KEEP, 0);
         point_stair_to_level(x, y, target, 0);
      }
      else
      {
         /* can we make a shaft here */
         if (p_ptr->mdepth > 1)
         {
            (void)set_grid_type(x, y, DUNG_STAIR,
                                DUNG_STAIR_SHAFTUP, GRID_KEEP, 0);
         }
         else
         {
            (void)set_grid_type(x, y, DUNG_STAIR,
                                DUNG_STAIR_UP, GRID_KEEP, 0);
         }
         point_stair_to_level(x, y, 0, 0);
      }
   }
}

/*
 * Convert existing terrain type to "down stairs"
 */
void place_main_down_stair(s16b x, s16b y, bool deep)
{
   if (!in_bounds(x, y)) return;

   if (!deep)
   {
      (void)set_grid_type(x, y, DUNG_STAIR, DUNG_STAIR_DOWN, GRID_KEEP, 0);
      point_stair_to_level(x, y, p_ptr->mdepth + 1, 0);
   }
   else
   {
      s16b target = p_ptr->mdepth + randint(3) + 1;

      /* can we make this shaft here? */
      if (target < MAX_LEVEL)
      {
         (void)set_grid_type(x, y, DUNG_STAIR,
                             DUNG_STAIR_SHAFTDN, GRID_KEEP, 0);
         point_stair_to_level(x, y, target, 0);
      }
      else
      {
         /* can we make a shaft here */
         if (p_ptr->mdepth < (MAX_LEVEL-1))
         {
            (void)set_grid_type(x, y, DUNG_STAIR,
                                DUNG_STAIR_SHAFTDN, GRID_KEEP, 0);
         }
         else
         {
            (void)set_grid_type(x, y, DUNG_STAIR,
                                DUNG_STAIR_DOWN, GRID_KEEP, 0);
         }
         point_stair_to_level(x, y, MAX_LEVEL - 1, 0);
      }
   }
}

/*
 * Place a secret door at the given location
 */
static void place_secret_door(s16b x, s16b y)
{
   cave_cell_type *c_ptr;

   if (!in_bounds(x, y)) return;

   c_ptr = &dungeon.level[sublevel][y][x];

   /* don't place doors where monsters already exist */
   if (c_ptr->m_idx != 0)
   {
      return;
   }

   /* Clear previous contents, add secret door */
   (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_SECRET, GRID_KEEP, 0);
   /* traps on secret doors shouldn't be known! */
   if (rand_int(10000)<CHANCE_TRAP_SECRET_DOOR)
      place_traps_door(x,y,p_ptr->mdepth, 0, 0);
}

/* jk (40-level), (40-level/4) */
/* place one or more traps on a door */
void place_traps_door(s16b x, s16b y, s16b level,
                      s16b chance_one, s16b chance_all)
{
   trap_item_type *tr_ptr;
   s16b i;
   s16b tr_idx;

   if (!in_bounds(x, y)) return;

   tr_idx = t_pop(); /* get a free trap_item */

   if (tr_idx!=-1)
   {
      tr_ptr = &t_list[tr_idx];
      if (!set_traps(tr_ptr, &i, level, FTRAP_DOOR, chance_one, chance_all))
      {
         tr_ptr->inuse = FALSE; /* we allocated it for nothing, give it back */
         return;
      }
      /* paranoia time! */
      if (num_traps_ptr(tr_ptr, TRAP_EXISTS)==0)
      {
         tr_ptr->inuse = FALSE;
         return;
      }
      else
      {
         dungeon.level[sublevel][y][x].t_idx = tr_idx; /* point towards our special trap item */
         tr_ptr->tx = x;            /* this isn't done in set_traps */
         tr_ptr->ty = y;
      }
   }
   else
      quit("Creating trap for door failed");
}

/*
 * Place a random type of door at the given location
 */
static void place_random_door(s16b x, s16b y)
{
   s16b tmp;
   bool had_glow = FALSE;

   cave_cell_type *c_ptr;

   if (!in_bounds(x, y)) return;

   c_ptr = &dungeon.level[sublevel][y][x];

   /* don't place doors where monsters already exist */
   if (c_ptr->m_idx != 0)
   {
      return;
   }
   if (c_ptr->fdat & CAVE_GLOW) had_glow = TRUE;

   /* Choose an object */
   tmp = rand_int(1000);

   /* Open doors (300/1000) */
   if (tmp < 300)
   {
      /* Clear previous contents, add open door */
      (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_OPEN, GRID_KEEP, had_glow ? CAVE_GLOW : 0);
   }

   /* Broken doors (100/1000) */
   else if (tmp < 400)
   {
      /* Clear previous contents, add broken door */
      (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_BROKEN, GRID_KEEP, had_glow ? CAVE_GLOW : 0 );
   }

   /* Secret doors (200/1000) */
   else if (tmp < 600)
   {
      /* Clear previous contents, add secret door */
      (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_SECRET, GRID_KEEP, had_glow ? CAVE_GLOW : 0);
      if (rand_int(10000)<CHANCE_TRAP_SECRET_DOOR)
      {
         place_traps_door(x,y,p_ptr->mdepth, 0, 0);
      }
   }

   /* Closed doors (300/1000) */
   else if (tmp < 900)
   {
      /* Clear previous contents, add closed door */
      (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_CLOSED, GRID_KEEP, had_glow ? CAVE_GLOW : 0);
      if (rand_int(10000)<CHANCE_TRAP_DOOR)
      {
         place_traps_door(x,y,p_ptr->mdepth, (40-p_ptr->mdepth), (40-p_ptr->mdepth/4));
      }
   }

   /* Locked doors (99/1000) */
   else if (tmp < 999)
   {
      /* Clear previous contents, add locked door */
      (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_LOCKED, GRID_KEEP, had_glow ? CAVE_GLOW : 0);
      c_ptr->extra = rand_int(15);
      if (rand_int(10000)<CHANCE_TRAP_LOCKED_DOOR)
      {
         place_traps_door(x, y, p_ptr->mdepth, (40-p_ptr->mdepth), (40-p_ptr->mdepth/4));
      }
   }

   /* Jammed doors (1/1000) */
   else
   {
      /* Clear previous contents, add jammed door */
      (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_JAMMED, GRID_KEEP, had_glow ? CAVE_GLOW : 0);
      c_ptr->extra = rand_int(15);
      if (rand_int(10000)<CHANCE_TRAP_JAMMED_DOOR)
      {
         place_traps_door(x,y,p_ptr->mdepth, (40-p_ptr->mdepth), (40-p_ptr->mdepth/4));
      }
   }
dlog(DEBUGGENER,"generate.c: place_random_door: %d,%d tmp %d mtyp %d styp %d fdat %08lx t_idx %d\n",
           x, y, tmp, c_ptr->mtyp, c_ptr->styp, c_ptr->fdat, c_ptr->t_idx);
}

bool is_granite(s16b x, s16b y)
{
   cave_cell_type *c_ptr;

   c_ptr = &dungeon.level[sublevel][y][x];

   if ( (c_ptr->mtyp==DUNG_WALL) &&
        ( (c_ptr->styp==DUNG_WALL_GRANITE) ||
          (c_ptr->styp==DUNG_WALL_GRAINNR) ||
          (c_ptr->styp==DUNG_WALL_GRAOUTR) ||
          (c_ptr->styp==DUNG_WALL_GRHIDTR) ||
          (c_ptr->styp==DUNG_WALL_GRTREAS) ||
          (c_ptr->styp==DUNG_WALL_GRSOLID) ) ) return (TRUE);
   return (FALSE);
}

/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(x, y)"
 *
 * We count only granite walls and permanent walls.
 */
static s16b next_to_walls(s16b x, s16b y)
{
   s16b        k = 0;

   if ((is_granite(x, y+1)) || (dungeon.level[sublevel][y+1][x].mtyp == DUNG_PERWALL)) k++;
   if ((is_granite(x, y-1)) || (dungeon.level[sublevel][y-1][x].mtyp == DUNG_PERWALL)) k++;
   if ((is_granite(x+1, y)) || (dungeon.level[sublevel][y][x+1].mtyp == DUNG_PERWALL)) k++;
   if ((is_granite(x-1, y)) || (dungeon.level[sublevel][y][x-1].mtyp == DUNG_PERWALL)) k++;

   return (k);
}

/*
 * Places some staircases near walls
 */
/* jk - town: 0 means in town 1 means out of town 2 means anywhere */
void alloc_stairs(s16b typ, s16b num, s16b walls, s16b town)
{
   s16b                 x, y, i, j, flag;

   cave_cell_type               *c_ptr;
   /* Place "num" stairs */
   for (i = 0; i < num; i++)
   {
      /* Place some stairs */
      for (flag = FALSE; !flag; )
      {
         /* Try several times, then decrease "walls" */
         for (j = 0; !flag && j <= 3000; j++)
         {
            /* Pick a random grid */
            /* take care not to choose on the edges, like 0 or curwid-2 */
            /* because next_to_walls crashes on it, and it will be a wall */
            /* anyway */
            if (!town)
            {
               x = randint(SCREEN_WID-2);
               y = randint(SCREEN_HGT-2);
            }
            x = randint(cur_wid-2);
            y = randint(cur_hgt-2);

            if (town==1)
            {
               if ((x<SCREEN_WID) || (y<SCREEN_HGT)) continue;
            }
            /* Require "naked" floor grid */
            if (!naked_grid_bold(x, y))
               continue;

            /* Require a certain number of adjacent walls */
            if (next_to_walls(x, y) < walls) continue;

            /* Access the grid */
            c_ptr = &dungeon.level[sublevel][y][x];

            /* don't build in vaults */
            if (c_ptr->fdat & CAVE_VAULT) continue;

            /* Town -- must go down */
            if (!p_ptr->mdepth)
            {
               /* Clear previous contents, add down stairs */
               /* first stair of many should be only one level! */
               /* the only stairs should also be only one level */
               place_main_down_stair(x,y,((num>1) && (i>0)));
            }

            /* Quest -- sometimes must go up */
            else if (is_quest(p_ptr->mdepth) || (p_ptr->mdepth >= MAX_LEVEL-1))
            {
               /* Clear previous contents, add up stairs */
               place_main_up_stair(x,y,((num>1) && (i>0)));
            }

            /* Requested type */
            else
            {
               /* Clear previous contents, add stairs */
               if (typ==0x06) place_main_up_stair(x,y,((num>1) && (i>0)));
               if (typ==0x07) place_main_down_stair(x,y,((num>1) && (i>0)));
            }

            /* All done */
            flag = TRUE;
         }

         /* Require fewer walls */
         if (walls>0)
         {
            walls--;
         }
         else
         {
            dlog(DEBUGEXTRA,"generate.c: alloc_stairs: no stairs could be allocated?\n");
            break;
         }
      }
   }
}

/*
 * Allocates some objects (using "place" and "type")
 */
static void alloc_object(s16b set, s16b typ, s16b num)
{
   s16b x = 0, y = 0, k, cnt = 0;


   /* Place some objects */
   for (k = 0; k < num; k++)
   {
      /* Pick a "legal" spot */
      while (cnt<1000)
      {
         bool room;
         cnt++;

         /* Location */
         x = rand_int(cur_wid);
         y = rand_int(cur_hgt);

         /* Require "naked" floor grid */
         if (!naked_grid_bold(x, y)) continue;

         /* don't build in vaults */
         if (dungeon.level[sublevel][y][x].fdat & CAVE_VAULT) continue;

         /* Check for "room" */
         room = (dungeon.level[sublevel][y][x].fdat & CAVE_ROOM) ? TRUE : FALSE;

         /* Require corridor? */
         if ((set == ALLOC_SET_CORR) && room) continue;

         /* Require room? */
         if ((set == ALLOC_SET_ROOM) && !room) continue;

         /* Accept it */
         break;
      }
      /* if we failed, no object */
      if (cnt==1000) continue;

      /* Place something */
      switch (typ)
      {
         case ALLOC_TYP_RUBBLE:
             place_rubble(x, y);
             break;

         case ALLOC_TYP_TRAP:
             place_trap(x, y, p_ptr->mdepth, (40-p_ptr->mdepth), (40-p_ptr->mdepth/4));
             break;

         case ALLOC_TYP_GOLD:
             place_gold(x, y);
             break;

         case ALLOC_TYP_OBJECT:
             place_object(x, y, FALSE, FALSE, FALSE);
             break;
      }
   }
}

/*
 * Places "streamers" of rock through dungeon
 *
 * Note that their are actually six different terrain features used
 * to represent streamers.  Three each of magma and quartz, one for
 * basic vein, one with hidden gold, and one with known gold.  The
 * hidden gold types are currently unused.
 */
static void build_streamer(s16b type, s16b chance)
{
   s16b         i, tx, ty;
   s16b         x, y, dir;
   s16b         wall_type = type;

   cave_cell_type *c_ptr;

   /* Hack -- Choose starting point */
   x = rand_spread(cur_wid / 2, 15);
   y = rand_spread(cur_hgt / 2, 10);

   if (!type)
   {
      /* random wall type */
      wall_type = rand_int(5);
   }

   /* Choose a random compass direction */
   dir = ddd[rand_int(8)];

   /* Place streamer into dungeon */
   while (TRUE)
   {
      s16b d = DUN_STR_RNG;

      /* One grid per density */
      for (i = 0; i < DUN_STR_DEN; i++)
      {

         /* Pick a nearby grid */
         while (1)
         {
            tx = rand_spread(x, d);
            ty = rand_spread(y, d);
            if (!in_bounds2(tx, ty)) continue;

            break;
         }

         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][ty][tx];

         /* don't build streamers in rooms.... */
         if (c_ptr->fdat & CAVE_ROOM) continue;

         /* don't build in anything but walls */

         if (c_ptr->mtyp != DUNG_WALL) continue;

         if (rand_int(chance)!=0)
         {
            /* build normal wall type */

            switch (wall_type)
            {
               case DUNG_WALL_ART_GRANITE:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_GRANITE, GRID_KEEP, 0);
                    break;
               case DUNG_WALL_ART_CHALK:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_CHALK, GRID_KEEP, 0);
                    break;
               case DUNG_WALL_ART_QUARTZ:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_QUARTZ, GRID_KEEP, 0);
                    break;
               case DUNG_WALL_ART_MAGMA:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_MAGMA, GRID_KEEP, 0);
                    break;
            }
         }
         else
         {
            /* Clear previous contents, add proper vein type */
            switch (wall_type)
            {
               case DUNG_WALL_ART_GRANITE:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_GRHIDTR, GRID_KEEP, 0);
                    break;
               case DUNG_WALL_ART_CHALK:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_CHHIDTR, GRID_KEEP, 0);
                    break;
               case DUNG_WALL_ART_QUARTZ:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_QUHIDTR, GRID_KEEP, 0);
                    break;
               case DUNG_WALL_ART_MAGMA:
                    (void)set_grid_type(tx, ty, DUNG_WALL,
                                        DUNG_WALL_MGHIDTR, GRID_KEEP, 0);
                    break;
            }
         }
      }

      /* Advance the streamer, but if we build wider streamers (d>1) we have */
      /* a chance of staying here and growing in width as it were            */
      if ((d==1) || randint(d)==1)
      {
         x += ddx[dir];
         y += ddy[dir];
      }
      /* Quit before leaving the dungeon */
      if (!in_bounds(x, y)) break;
   }
}

void place_wall(s16b x, s16b y)
{
   /* Granite */
   (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRANITE,GRID_KEEP, 0);
}

/*
 * Build a destroyed level
 */
static void destroy_level()
{
   s16b y1, x1, y, x, k, n;

   cave_cell_type *c_ptr;

   /* Note destroyed levels */

   /* Drop a few epi-centers (usually about two) */
   for (n = 0; n < randint(5); n++)
   {
      /* Pick an epi-center */
      x1 = rand_range(5, cur_wid-1 - 5);
      y1 = rand_range(5, cur_hgt-1 - 5);

      /* Big area of affect */
      for (y = (y1 - 15); y <= (y1 + 15); y++)
      {
         for (x = (x1 - 15); x <= (x1 + 15); x++)
         {
            /* Skip illegal grids */
            if (!in_bounds(x, y)) continue;

            /* Extract the distance */
            k = distance(x1, y1, x, y);

            /* Stay in the circle of death */
            if (k >= 16) continue;

            /* Delete the monster (if any) */
            delete_monster(x, y);

            /* Destroy valid grids */
            if (valid_grid(x, y))
            {
               /* Delete the object (if any) */
               delete_object(x, y, -1);

               /* Access the grid */
               c_ptr = &dungeon.level[sublevel][y][x];

               place_wall(x, y);

               /* No longer part of a room or vault */
               c_ptr->fdat &= ~(CAVE_ROOM | CAVE_VAULT);

               /* No longer illuminated or known */
               c_ptr->fdat &= ~(CAVE_MARK | CAVE_GLOW);
            }
         }
      }
      /* now let loose a player ghost */
      if (randint(10)<4) summon_ghost(x1, y1);
   }
}

/*
 * fill the current level with granite
 */
static void fill_with_granite(void)
{
   s16b x, y;
   /* Hack -- Start with basic granite */
   for (y = 0; y < cur_hgt; y++)
   {
      for (x = 0; x < cur_wid; x++)
      {
         /* Clear all features, set to granite */
         place_wall(x, y);
      }
   }
}

/*
 * Create up to "num" objects near the given coordinates
 * Only really called by some of the "vault" routines.
 */
static void vault_objects(s16b x, s16b y, s16b num)
{
   s16b        i, j, k;

   /* Attempt to place 'num' objects */
   for (; num > 0; --num)
   {
      /* Try up to 11 spots looking for empty space */
      for (i = 0; i < 11; ++i)
      {
         /* Pick a random location */
         while (1)
         {
            j = rand_spread(y, 2);
            k = rand_spread(x, 3);
            if (!in_bounds(k,j)) continue;
            break;
         }

         /* Require "clean" floor space */
         if (!clean_grid_bold(k,j)) continue;

         /* Place an item */
         if (rand_int(100) < 75)
         {
            place_object(k, j, FALSE, FALSE, FALSE);
         }

         /* Place gold */
         else
         {
            place_gold(k, j);
         }

         /* Placement accomplished */
         break;
      }
   }
}

/*
 * Place a trap with a given displacement of point
 */
static void vault_trap_aux(s16b x, s16b y, s16b xd, s16b yd)
{
   s16b         count, y1, x1;

   /* Place traps */
   for (count = 0; count <= 5; count++)
   {
      /* Get a location */
      while (1)
      {
         y1 = rand_spread(y, yd);
         x1 = rand_spread(x, xd);
         if (!in_bounds(x1, y1)) continue;
         break;
      }

      /* Require "naked" floor grids */
      if (!naked_grid_bold(x1, y1)) continue;

      /* Place the trap */
      place_trap(x1, y1, p_ptr->mdepth, 10, 5);

      /* Done */
      break;
   }
}

/*
 * Place some traps with a given displacement of given location
 */
static void vault_traps(s16b x, s16b y, s16b xd, s16b yd, s16b num)
{
   s16b i;

   for (i = 0; i < num; i++)
   {
      vault_trap_aux(x, y, xd, yd);
   }
}

/*
 * Hack -- Place some sleeping monsters near the given location
 */
static void vault_monsters(s16b x1, s16b y1, s16b num)
{
   s16b          k, i, x, y;

   /* Try to summon "num" monsters "near" the given location */
   for (k = 0; k < num; k++)
   {
      /* Try nine locations */
      for (i = 0; i < 9; i++)
      {
         s16b d = 1;

         /* Pick a nearby location */
         scatter(&x, &y, x1, y1, d, 0);

         /* Require "empty" floor grids */
         if (!empty_grid_bold(x, y)) continue;

         /* Place the monster (allow groups) */
         monster_level = p_ptr->mdepth + 2;
         (void)place_monster(x, y, TRUE, TRUE,0);
         monster_level = p_ptr->mdepth + 2;
      }
   }
}

/*
 * Room building routines.
 *
 * Six basic room types:
 *   1 -- normal
 *   2 -- overlapping
 *   3 -- cross shaped
 *   4 -- large room with features
 *   5 -- monster nests
 *   6 -- monster pits
 *   7 -- simple vaults
 *   8 -- greater vaults
 *   9 -- smaller treasure chamber
 *   10-- middle treasure chamber
 *   11-- bigger treasure chamber
 *   12-- smaller graveyard
 *   13-- middle graveyard
 *   14-- greater graveyard
 */

/*
 * Type 1 -- normal rectangular rooms
 */
static void build_type1(s16b xval, s16b yval)
{
   s16b   x, y, x2, y2;
   s16b   x1, y1;

   bool   light;

   /* Choose lite or dark */
   light = (p_ptr->mdepth <= randint(25));
   /* we reserve 33x10 space */
   /* we go from x-11-1,y-4 to x+11+1,y+5 */

   /* Pick a room size */
   x1 = xval - randint(11);
   x2 = xval + randint(11);
   y1 = yval - randint(4);
   y2 = yval + randint(3);
   /* we really go from y1-2, x1-2 to x2+2, y2+2 with recesses */

dlog(DEBUGGENER,"generate.c: build_type1: room %d,%d to %d,%d\n", x1-1,y1-1,x2+1, y2+1);
   /* Place a full floor under the room */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }

   /* Walls around the room */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0 );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }

   /* Hack -- Occasional pillar room */
   if (rand_int(20) == 0)
   {
      for (y = y1; y <= y2; y += 2)
      {
         for (x = x1; x <= x2; x += 2)
         {
            (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
      }
      /* then add some traps to this place... */
      for (y = y1 + 1; y <= y2 - 1; y += 2)
      {
         for (x = x1 + 1; x <= x2 + 1; x += 2)
         {
            if (randint(10)==1) place_trap(x, y, p_ptr->mdepth, 15, 5);
         }
      }

   }

   /* Hack -- Occasional ragged-edge room */
   else if (rand_int(50) == 0)
   {
      for (y = y1 + 2; y <= y2 - 2; y += 2)
      {
         (void)set_grid_type(x1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         (void)set_grid_type(x2, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
      }
      for (x = x1 + 2; x <= x2 - 2; x += 2)
      {
         (void)set_grid_type(x, y1, DUNG_WALL, DUNG_WALL_GRAINNR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         (void)set_grid_type(x, y2, DUNG_WALL, DUNG_WALL_GRAINNR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
      }
   }
   /* sometimes build 'recessed edges' like these: */
   /* XXXXXXXXXXXX */
   /* X..XXXXXX..X */
   /* X..........X */
   /* XX........XX */
   /* X..........X */
   /* X..XXXXXX..X */
   /* XXXXXXXXXXXX */
   /* this looks like often, but we try even if the room is too small */
   else if ( randint(4) == 1)
   {
      s16b dx1 = (xval-x1);
      s16b dx2 = (x2-xval);
      s16b dy1 = (yval-y1);
      s16b dy2 = (y2-yval);

dlog(DEBUGGENER,"generate.c: build_type1: recessed corner: dx1 %d dy1 %d dx2 %d dy2 %d\n",
                 dx1, dy1, dx2, dy2);

      /* recessed edge to the North-West */
      if ( (dx1 > 2) && (dx1 < 11) && (dy1 > 2) && (dy1 < 4) )
      {
         /* we are stretching the limits, after all */
         if (!in_bounds(x1-2, y1-2) ) return;

         for (x = x1-1; x < x1 + (dx1 - 2); x++)
         {
            /* 722223XXXXXX */
            /* 51111XXXX..X */
            /* 54.........X */
            /* 6X........XX */
            /* X..........X */
            /* X..XXXXXX..X */
            /* XXXXXXXXXXXX */

dlog(DEBUGGENER,"generate.c: build_type1: NW recess upper @ %d,%d\n", x, y1-1);
            /* place new floor (1) */
            (void)set_grid_type(x, y1-1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (2) */
            (void)set_grid_type(x, y1-2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (3) */
         (void)set_grid_type(x1+ (dx1 - 2), y1-2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         /* place new wall (7) */
         (void)set_grid_type(x1-2, y1-2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         for (y = y1-1; y < y1 + (dy1 - 2); y++)
         {
dlog(DEBUGGENER,"generate.c: build_type1: NW recess left @ %d,%d\n", x1-1, y);
            /* place new floor (4) */
            (void)set_grid_type(x1-1, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (5) */
            (void)set_grid_type(x1-2, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (6) */
         (void)set_grid_type(x1-2, y1+ (dy1 - 2), DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
      }

      /* recessed edge to the South-West */
      if ( (dx1 > 2) && (dx1 < 11) && (dy2 > 2) && (dy2 < 4) )
      {
         /* we are stretching the limits, after all */
         if (!in_bounds(x1-2, y2+2) ) return;

         for (x = x1-1; x < x1 + (dx1 - 2); x++)
         {
            /* XXXXXXXXXXXX */
            /* XXXXXXXXXXXX */
            /* XX........XX */
            /* 7X........XX */
            /* 65........XX */
            /* 611XXXXXXXXX */
            /* 4223XXXXXXXX */

dlog(DEBUGGENER,"generate.c: build_type1: SW recess upper @ %d,%d\n", x, y2+1);
            /* place new floor (1) */
            (void)set_grid_type(x, y2+1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (2) */
            (void)set_grid_type(x, y2+2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (3) */
         (void)set_grid_type(x1 + (dx1 - 2), y2+2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         /* place new wall (4) */
         (void)set_grid_type(x1-2, y2+2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         for (y = y1 + (dy2 - 2) + 1; y <= y2 + 1; y++)
         {
dlog(DEBUGGENER,"generate.c: build_type1: SW recess left @ %d,%d\n", x1-1, y);
            /* place new floor (5) */
            (void)set_grid_type(x1-1, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (6) */
            (void)set_grid_type(x1-2, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (7) */
         (void)set_grid_type(x1-2, y1 + (dy2 - 2), DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
      }

      /* recessed edge to the North-East */
      if ( (dx2 > 2) && (dx2 < 11) && (dy1 > 2) && (dy1 < 4) )
      {
         /* we are stretching the limits, after all */
         if (!in_bounds(x2+2, y1-2) ) return;

         for (x = x2 - (dx2-2)+1; x <= x2 + 1; x++)
         {
            /* XXXXXX322224 */
            /* XXXXXXX11116 */
            /* XX........56 */
            /* XX........X7 */
            /* XX........XX */
            /* XXXXXXXXXXXX */
            /* XXXXXXXXXXXX */

dlog(DEBUGGENER,"generate.c: build_type1: NE recess upper @ %d,%d\n", x, y1-1);
            /* place new floor (1) */
            (void)set_grid_type(x, y1-1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (2) */
            (void)set_grid_type(x, y1-2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (3) */
         (void)set_grid_type(x1+ (dx2 - 2), y1-2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         /* place new wall (4) */
         (void)set_grid_type(x2+2, y1-2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         for (y = y1-1; y < y1 + (dy1 - 2); y++)
         {
dlog(DEBUGGENER,"generate.c: build_type1: NE recess left @ %d,%d\n", x2+1, y);
            /* place new floor (5) */
            (void)set_grid_type(x2+1, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (6) */
            (void)set_grid_type(x2+2, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (7) */
         (void)set_grid_type(x2+2, y1+ (dy1 - 2), DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
      }

      /* recessed edge to the South-East */
      if ( (dx2 > 2) && (dx2 < 11) && (dy2 > 2) && (dy2 < 4) )
      {
         /* we are stretching the limits, after all */
         if (!in_bounds(x2+2, y2+2) ) return;

         for (x = x2 - (dx2-2)+1; x <= x2 + 1; x++)
         {
            /* XXXXXXXXXXXX */
            /* XXXXXXXXXXXX */
            /* XX........XX */
            /* XX........XX */
            /* XX........56 */
            /* XXXXXXXX1116 */
            /* XXXXXXX32224 */

dlog(DEBUGGENER,"generate.c: build_type1: SE recess upper @ %d,%d\n", x, y2+1);
            /* place new floor (1) */
            (void)set_grid_type(x, y2+1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (2) */
            (void)set_grid_type(x, y2+2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (3) */
         (void)set_grid_type(x1+ (dx2 - 2), y2+2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         /* place new wall (4) */
         (void)set_grid_type(x2+2, y2+2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
         for (y = y2 - (dy2 - 2) + 1; y <= y2 + 1; y++)
         {
dlog(DEBUGGENER,"generate.c: build_type1: SE recess left @ %d,%d\n", x2+1, y);
            /* place new floor (5) */
            (void)set_grid_type(x2+1, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
            /* place new wall (6) */
            (void)set_grid_type(x2+2, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         /* place new wall (7) */
         (void)set_grid_type(x2+2, y2 + 2, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
      }
   }
}

/*
 * Type 2 -- Overlapping rectangular rooms
 */
static void build_type2(s16b xval, s16b yval)
{
   s16b            x, y;
   s16b            x1a, y1a, x2a, y2a;
   s16b            x1b, y1b, x2b, y2b;

   bool           light;

   cave_cell_type *c_ptr;

   /* Choose lite or dark */
   light = (p_ptr->mdepth <= randint(25));
   /* we reserve 33x10 space */
   /* we go from x-11-1,y-4 to x+11+1,y+5 */

   /* Determine extents of the first room */
   x1a = xval - randint(11);
   y1a = yval - randint(3);
   x2a = xval + randint(10);
   y2a = yval + randint(3);

   /* Determine extents of the second room */
   x1b = xval - randint(10);
   y1b = yval - randint(3);
   x2b = xval + randint(11);
   y2b = yval + randint(3);
dlog(DEBUGGENER,"generate.c: build_type2: room1 %d,%d to %d,%d room2 %d,%d to %d,%d\n",
     x1a-1,y1a-1, x2a+1,y2a+1, x1b-1, y2b-1, x2b+1, y2b+1);

   /* Place a full floor for room "a" */
   for (y = y1a - 1; y <= y2a + 1; y++)
   {
      for (x = x1a - 1; x <= x2a + 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }

   /* Place a full floor for room "b" */
   for (y = y1b - 1; y <= y2b + 1; y++)
   {
      for (x = x1b - 1; x <= x2b + 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }

   /* Place the walls around room "a" */
   for (y = y1a - 1; y <= y2a + 1; y++)
   {
      (void)set_grid_type(x1a-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x2a+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }
   for (x = x1a - 1; x <= x2a + 1; x++)
   {
      (void)set_grid_type(x, y1a-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x, y2a+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }

   /* Place the walls around room "b" */
   for (y = y1b - 1; y <= y2b + 1; y++)
   {
      (void)set_grid_type(x1b-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x2b+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }
   for (x = x1b - 1; x <= x2b + 1; x++)
   {
      (void)set_grid_type(x, y1b-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x, y2b+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }

   /* Replace the floor for room "a" */
   for (y = y1a; y <= y2a; y++)
   {
      for (x = x1a; x <= x2a; x++)
      {
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }

   /* Replace the floor for room "b" */
   for (y = y1b; y <= y2b; y++)
   {
      for (x = x1b; x <= x2b; x++)
      {
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }
}

/*
 * Type 3 -- Cross shaped rooms
 *
 * Builds a room at a row, column coordinate
 *
 * Room "a" runs north/south, and Room "b" runs east/east
 * So the "central pillar" runs from x1a,y1b to x2a,y2b.
 *
 * Note that currently, the "center" is always 3x3, but I think that
 * the code below will work (with "bounds checking") for 5x5, or even
 * for unsymetric values like 4x3 or 5x3 or 3x4 or 3x5, or even larger.
 */
static void build_type3(s16b xval, s16b yval)
{
   s16b            y, x, dy, dx, wy, wx;
   s16b            x1a, y1a, x2a, y2a;
   s16b            x1b, y1b, x2b, y2b;
/* jk - to hold return values from place_object */

   bool           light;

   cave_cell_type *c_ptr;

   /* Choose lite or dark */
   light = (p_ptr->mdepth <= randint(25));

   /* For now, always 3x3 */
   wx = wy = 1;

   /* we reserve 33x10 space */
   /* we use from x-12, y-5 to x+12, y+5 */

   /* Pick max horizontal size (at most 15) */
   dx = rand_range(3, 11);

   /* Pick max vertical size (at most 4) */
   dy = rand_range(2, 3);

   /* Determine extents of the north/south room */
   x1a = xval - wx;
   x2a = xval + wx;
   y1a = yval - dy;
   y2a = yval + dy;

   /* Determine extents of the east/west room */
   x1b = xval - dx;
   x2b = xval + dx;
   y1b = yval - wy;
   y2b = yval + wy;

dlog(DEBUGGENER,"generate.c: build_type3: building %d,%d to %d,%d and %d,%d to %d,%d\n",
                 x1a-1, y1a-1, x2a+2, y2a+2, x1b-1, y1b-1, x2b+1, y2b+1);
   /* Place a full floor for room "a" */
   for (y = y1a - 1; y <= y2a + 1; y++)
   {
      for (x = x1a - 1; x <= x2a + 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }
   /* Place a full floor for room "b" */
   for (y = y1b - 1; y <= y2b + 1; y++)
   {
      for (x = x1b - 1; x <= x2b + 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }

   /* Place the walls around room "a" */
   for (y = y1a - 1; y <= y2a + 1; y++)
   {
      (void)set_grid_type(x1a-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                           GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x2a+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                           GRID_REPLACE, light?CAVE_GLOW:0L );
   }
   for (x = x1a - 1; x <= x2a + 1; x++)
   {
      (void)set_grid_type(x, y1a-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x, y2a+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }

   /* Place the walls around room "b" */
   for (y = y1b - 1; y <= y2b + 1; y++)
   {
      (void)set_grid_type(x1b-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x2b+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }
   for (x = x1b - 1; x <= x2b + 1; x++)
   {
      (void)set_grid_type(x, y1b-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x, y2b+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }

   /* Replace the floor for room "a" */
   for (y = y1a; y <= y2a; y++)
   {
      for (x = x1a; x <= x2a; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }

   /* Replace the floor for room "b" */
   for (y = y1b; y <= y2b; y++)
   {
      for (x = x1b; x <= x2b; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }

   /* Special features (3/4) */
   switch (rand_int(4))
   {
      /* Large solid middle pillar */
      case 1:
         for (y = y1b; y <= y2b; y++)
         {
            for (x = x1a; x <= x2a; x++)
            {
               (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
            }
         }
         break;

      /* Inner treasure vault */
      case 2:

         /* Build the vault */
         for (y = y1b; y <= y2b; y++)
         {
            (void)set_grid_type(x1a, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
            (void)set_grid_type(x2a, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         for (x = x1a; x <= x2a; x++)
         {
            (void)set_grid_type(x, y1b, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
            (void)set_grid_type(x, y2b, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }

         /* Place a secret door on the inner room */
         switch (rand_int(4))
         {
            case 0: place_secret_door(xval, y1b); break;
            case 1: place_secret_door(xval, y2b); break;
            case 2: place_secret_door(x1a, yval); break;
            case 3: place_secret_door(x2a, yval); break;
         }

         /* Place a treasure in the vault */
         place_object(xval, yval, FALSE, FALSE, FALSE);

         /* Let's guard the treasure well */
         vault_monsters(xval, yval, rand_int(2) + 3);

         /* Traps naturally */
         vault_traps(xval, yval, 4, 4, rand_int(3) + 2);

         break;

      /* Something else */
      case 3:

         /* Occasionally pinch the center shut */
         if (rand_int(3) == 0)
         {

            /* Pinch the east/west sides */
            for (y = y1b; y <= y2b; y++)
            {
               if (y == yval) continue;
               (void)set_grid_type(x1a-1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
               (void)set_grid_type(x2a+1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
            }

            /* Pinch the north/south sides */
            for (x = x1a; x <= x2a; x++)
            {
               if (x == xval) continue;
               (void)set_grid_type(x, y1b-1, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
               (void)set_grid_type(x, y2b+1, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
            }

            /* Sometimes shut using secret doors */
            if (rand_int(3) == 0)
            {
               place_secret_door(x1a - 1, yval);
               place_secret_door(x2a + 1, yval);
               place_secret_door(xval, y1b - 1);
               place_secret_door(xval, y2b - 1);
            }
         }

         /* Occasionally put a "plus" in the center */
         else if (rand_int(3) == 0)
         {
            (void)set_grid_type(xval, yval, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
            (void)set_grid_type(xval, y1b, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
            (void)set_grid_type(xval, y2b, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
            (void)set_grid_type(x1a, yval, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
            (void)set_grid_type(x2a, yval, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }

         /* Occasionally put a pillar in the center */
         else if (rand_int(3) == 0)
         {
            (void)set_grid_type(xval, yval, DUNG_WALL, DUNG_WALL_GRAINNR,
                           GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         break;
   }
}

/*
 * Type 4 -- Large room with inner features
 *
 * Possible sub-types:
 *      1 - Just an inner room with one door
 *      2 - An inner room within an inner room
 *      3 - An inner room with pillar(s)
 *      4 - Inner room has a maze
 *      5 - A set of four inner rooms
 */
static void build_type4(s16b xval, s16b yval)
{
   s16b           x, y, x1, y1;
   s16b           x2, y2, tmp;

   bool           light;

   cave_cell_type *c_ptr;

   /* Choose lite or dark */
   light = (p_ptr->mdepth <= randint(25));

   /* we reserve 33x20 space */
   /* we use from x-10, y-10 to x+10, y+9 */

   /* Large room */
   x1 = xval - 9;
   y1 = yval - 5;
   x2 = xval + 9;
   y2 = yval + 5;
   /* xval-11, yval-5 to xval +11, yval+5 */
dlog(DEBUGGENER,"generate.c: build_type4: building %d,%d to %d,%d bounds %d,%d to %d,%d\n",
                 x1-1, y1-1, x2+1, y2+1, 0, 0, cur_wid-1, cur_hgt-1);

   /* Place a full floor under the room */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }
   /* xval-11, yval-6 to xval +11, yval+6 */

   /* Outer Walls */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }

   /* The inner room */
   y1 = y1 + 2;
   y2 = y2 - 2;
   x1 = x1 + 2;
   x2 = x2 - 2;
   /* xval-10, yval-4 to xval +10, yval+4 */

   /* The inner walls */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
   }

   /* Inner room variations */
   switch (randint(5))
   {
      /* Just an inner room with a monster */
      case 1:

         /* Place a secret door */
         switch (randint(4))
         {
            case 1: place_secret_door(xval, y1 - 1); break;
            case 2: place_secret_door(xval, y2 + 1); break;
            case 3: place_secret_door(x1 - 1, yval); break;
            case 4: place_secret_door(x2 + 1, yval); break;
         }

         /* Place a monster in the room */
         vault_monsters(xval, yval, 1);

         break;

      /* Treasure Vault (with a door) */
      case 2:

         /* Place a secret door */
         switch (randint(4))
         {
            case 1: place_secret_door(xval, y1 - 1); break;
            case 2: place_secret_door(xval, y2 + 1); break;
            case 3: place_secret_door(x1 - 1, yval); break;
            case 4: place_secret_door(x2 + 1, yval); break;
         }

         /* Place another inner room */
         for (y = yval - 1; y <= yval + 1; y++)
         {
            for (x = xval -  1; x <= xval + 1; x++)
            {
               if ((x == xval) && (y == yval)) continue;
               (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
            }
         }

         /* Place a locked door on the inner room */
         switch (randint(4))
         {
            case 1: place_locked_door(xval, yval - 1  ); break;
            case 2: place_locked_door(xval, yval + 1  ); break;
            case 3: place_locked_door(xval - 1, yval - 1); break;
            case 4: place_locked_door(xval + 1, yval + 1); break;
         }

         /* Monsters to guard the "treasure" */
         vault_monsters(xval, yval, randint(3) + 2);

         /* Object (80%) */
         if (rand_int(100) < 80)
         {
            place_object(xval, yval, FALSE, FALSE, FALSE);
         }

         /* Chest (20%) */
         else
         {
            get_obj_num_hook = kind_is_chest;
            get_obj_num_prep();
            place_object(x,y, FALSE, FALSE, FALSE);
            get_obj_num_hook = NULL;
            get_obj_num_prep();
         }

         /* Traps to protect the treasure */
         vault_traps(xval, yval, 4, 10, 2 + randint(3));

         break;

      /* Inner pillar(s). */
      case 3:

         /* Place a secret door */
         switch (randint(4))
         {
            case 1: place_secret_door(xval, y1 - 1); break;
            case 2: place_secret_door(xval, y2 + 1); break;
            case 3: place_secret_door(x1 - 1, yval); break;
            case 4: place_secret_door(x2 + 1, yval); break;
         }

         /* Large Inner Pillar */
         for (y = yval - 1; y <= yval + 1; y++)
         {
            for (x = xval - 1; x <= xval + 1; x++)
            {
               (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, light?CAVE_GLOW:0L );
            }
         }

         /* Occasionally, two more Large Inner Pillars */
         if (rand_int(2) == 0)
         {
            tmp = randint(2);
            for (y = yval - 1; y <= yval + 1; y++)
            {
               for (x = xval - 5 - tmp; x <= xval - 3 - tmp; x++)
               {
                  (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                      GRID_REPLACE, light?CAVE_GLOW:0L );
               }
               for (x = xval + 3 + tmp; x <= xval + 5 + tmp; x++)
               {
                  c_ptr = &dungeon.level[sublevel][y][x];
                  (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                      GRID_REPLACE, light?CAVE_GLOW:0L );
               }
            }
         }

         /* Occasionally, some Inner rooms */
         if (rand_int(3) == 0)
         {
   /* xval-15, yval-9 to xval +15, yval+9 */
            /* Long horizontal walls */
            for (x = xval - 5; x <= xval + 5; x++)
            {
               (void)set_grid_type(x, yval-1, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
               (void)set_grid_type(x, yval+1, DUNG_WALL, DUNG_WALL_GRAINNR,
                                   GRID_REPLACE, light?CAVE_GLOW:0L );
            }

            /* Close off the left/right edges */
            (void)set_grid_type(x, yval-5, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
            (void)set_grid_type(x, yval+5, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );

            /* Secret doors (random top/bottom) */
            place_secret_door(xval - 3, yval - 3 + (randint(2) * 2));
            place_secret_door(xval + 3, yval - 3 + (randint(2) * 2));

            /* Monsters */
            vault_monsters(xval - 2, yval, randint(2));
            vault_monsters(xval + 2, yval, randint(2));

            /* Objects */
            if (rand_int(3) == 0) place_object(xval -2, yval, FALSE, FALSE, FALSE);
            if (rand_int(3) == 0) place_object(xval +2, yval, FALSE, FALSE, FALSE);
         }

         break;


      /* Maze inside. */
      case 4:

         /* Place a secret door */
         switch (randint(4))
         {
            case 1: place_secret_door(xval, y1 - 1); break;
            case 2: place_secret_door(xval, y2 + 1); break;
            case 3: place_secret_door(x1 - 1, yval); break;
            case 4: place_secret_door(x2 + 1, yval); break;
         }

         /* Maze (really a checkerboard) */
         for (y = y1; y <= y2; y++)
         {
            for (x = x1; x <= x2; x++)
            {
               if (0x1 & (x + y))
               {
                  (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                      GRID_REPLACE, light?CAVE_GLOW:0L );
               }
            }
         }

         /* Monsters just love mazes. */
         vault_monsters(xval - 5, yval, randint(3));
         vault_monsters(xval + 5, yval, randint(3));

         /* Traps make them entertaining. */
         vault_traps(xval - 3, yval, 2, 8, randint(3));
         vault_traps(xval + 3, yval, 2, 8, randint(3));

         /* Mazes should have some treasure too. */
         vault_objects(xval, yval, 3);

         break;

      /* Four small rooms. */
      case 5:

         /* Inner "cross" */
         for (y = y1; y <= y2; y++)
         {
            c_ptr = &dungeon.level[sublevel][y][xval];
            (void)set_grid_type(xval, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
         for (x = x1; x <= x2; x++)
         {
            c_ptr = &dungeon.level[sublevel][yval][x];
            (void)set_grid_type(x, yval, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }

         /* Doors into the rooms */
         if (rand_int(100) < 50)
         {
            s16b i = randint(10);
            place_secret_door(xval - i, y1 - 1);
            place_secret_door(xval + i, y1 - 1);
            place_secret_door(xval - i, y2 + 1);
            place_secret_door(xval + i, y2 + 1);
         }
         else
         {
            s16b i = randint(3);
            place_secret_door(x1 - 1, yval + i);
            place_secret_door(x1 - 1, yval - i);
            place_secret_door(x2 + 1, yval + i);
            place_secret_door(x2 + 1, yval - i);
         }

         /* Treasure, centered at the center of the cross */
         vault_objects(xval, yval, 2 + randint(2));

         /* Gotta have some monsters. */
         vault_monsters(xval - 4, yval + 1, randint(4));
         vault_monsters(xval + 4, yval + 1, randint(4));
         vault_monsters(xval - 4, yval - 1, randint(4));
         vault_monsters(xval + 4, yval - 1, randint(4));
   /* xval-14, yval-6 to xval +14, yval+6 */

         break;
   }
}

/*
 * Type 16 -- round or elliptical rooms up to 22x20
 */
static void build_type16(s16b xval, s16b yval)
{
   s16b   x, y, x2, y2, dx, dy;
   s16b   x1, y1, r1, r2, i, tx, ty;

   bool   light;

   cave_cell_type *c_ptr;

   /* Choose lite or dark */
   light = (p_ptr->mdepth <= randint(25));

   /* sometimes build a horizontal ellipse, somethimes vertical */
   if (randint(2)==1)
   {
      /* an elliptical room */
      if (randint(2)==1)
      {
         r1=randint(5)+3; /* 5 to 8 */
         r2=randint(7)+1; /* 2 to 8 */
      }
      else
      {
         r1=randint(7)+1;
         r2=randint(5)+3;
      }
   }
   else
   {
      /* a round room */
      if (randint(2)==1)
      {
         r1=randint(5)+3;
         r2=r1;
      }
      else
      {
         r1=randint(3)+2;
         r2=r1;
      }
   }

dlog(DEBUGGENER,"generate.c: build_type16: round/ellipse @ %d,%d r1 %d r2 %d\n", xval, yval, r1, r2);

   /* Pick a room size */
   x1 = xval - r1;
   x2 = xval + r1;
   y1 = yval - r2;
   y2 = yval + r2;

   /* Place a full floor under the room */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         /* only build if it's within limits                                        */
         /* should be (x*x)/(r1*r1) + (y*y)/(r2*r2) > 1, but this works better with */
         /* integers                                                                */

         dx = x-xval;
         dy = y-yval;

         if ( (s32b)(dx*dx)*(s32b)(r2*r2) + (s32b)(dy*dy)*(s32b)(r1*r1) > (s32b)(r1*r1)*(s32b)(r2*r2)) continue;

         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
      }
   }
   /*
    * Place a full wall around the room. Because integer mathematics don't allow us to
    * correctly computer where it should be, we simply search for all walls that touch the
    * floor
    */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         dx = x-xval;
         dy = y-yval;

         /* only build if it's within limits                                        */
         /* should be (x*x)/(r1*r1) + (y*y)/(r2*r2) > 1, but this works better with */
         /* integers                                                                */

         dx = x-xval;
         dy = y-yval;

         if ( (s32b)(dx*dx)*(s32b)(r2*r2) + (s32b)(dy*dy)*(s32b)(r1*r1) > (s32b)(r1*r1)*(s32b)(r2*r2)) continue;

         /* now find all walls touching this square */
         for (i=0; i < 10; i++)
         {
            if (i==5) continue;
            tx = x + ddx[i];
            ty = y + ddy[i];

            if (!in_bounds(tx,ty)) continue;

            c_ptr = &dungeon.level[sublevel][ty][tx];
            if ( (c_ptr->mtyp != DUNG_WALL) && (c_ptr->mtyp != DUNG_PERWALL) ) continue;

            (void)set_grid_type(tx, ty, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
      }
   }
   /* Hack -- Occasional pillar room  */
   /* more occasional in bigger rooms */
   if ((rand_int(10) == 0) || (((r1+r2) > 12) && (randint(3)==1)))
   {
      s16b r_pillar = ((r1+r2) / 6);
      /* don't create pillars that are too big if r1>>r2 or r2>>r1 */
      if (r_pillar>=r1) r_pillar = r1-1;
      if (r_pillar>=r2) r_pillar = r2-1;

dlog(DEBUGGENER,"generate.c: build_type16: r_pillar %d\n", r_pillar);
      for (y = (yval - r_pillar); y <= (yval + r_pillar); y ++)
      {
         for (x = (xval - r_pillar); x <= (xval + r_pillar); x ++)
         {
            (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                GRID_REPLACE, light?CAVE_GLOW:0L );
         }
      }
   }
}

/*
 * Helper for plasma generation.
 */
static void perturb_point_mid(s16b x1, s16b x2, s16b x3, s16b x4,
                              s16b xmid, s16b ymid, s16b rough, s16b depth_max)
{
   /*
    * Average the four corners & perturb it a bit.
    * tmp is a random int +/- rough
    */
   s16b tmp2 = rough*2 + 1;
   s16b tmp = randint(tmp2) - (rough + 1);

   s16b avg = ((x1 + x2 + x3 + x4) / 4) + tmp;

   /* Division always rounds down, so we round up again */
   if (((x1 + x2 + x3 + x4) % 4) > 1)
      avg++;

   /* Normalize */
   if (avg < 0) avg = 0;
   if (avg > depth_max) avg = depth_max;

   /* Set the new value. */
   dungeon.level[sublevel][ymid][xmid].extra = avg;
}

static void perturb_point_end(s16b x1, s16b x2, s16b x3,
                              s16b xmid, s16b ymid, s16b rough, s16b depth_max)
{
   /*
    * Average the three corners & perturb it a bit.
    * tmp is a random s16b +/- rough
    */
   s16b tmp2 = rough*2 + 1;
   s16b tmp = randint(tmp2) - (rough + 1);

   s16b avg = ((x1 + x2 + x3) / 3) + tmp;

   /* Division always rounds down, so we round up again */
   if ((x1 + x2 + x3) % 3) avg++;

   /* Normalize */
   if (avg < 0) avg = 0;
   if (avg > depth_max) avg = depth_max;

   /* Set the new value. */
   dungeon.level[sublevel][ymid][xmid].extra = avg;
}

/*
 * A generic function to generate the plasma fractal.
 * Note that it uses ``cave_feat'' as temporary storage.
 * The values in ``cave_feat'' after this function
 * are NOT actual features; They are raw heights which
 * need to be converted to features.
 */
static void plasma_recursive(s16b x1, s16b y1, s16b x2, s16b y2,
                             s16b depth_max, s16b rough)
{
   /* Find middle */
   s16b xmid = (x2-x1)/2 + x1;
   s16b ymid = (y2-y1)/2 + y1;

   /* Are we done? */
   if (x1+1 == x2) return;

   perturb_point_mid(dungeon.level[sublevel][y1][x1].extra,
                     dungeon.level[sublevel][y2][x1].extra,
                     dungeon.level[sublevel][y1][x2].extra,
                     dungeon.level[sublevel][y2][x2].extra, xmid, ymid, rough, depth_max);

   perturb_point_end(dungeon.level[sublevel][y1][x1].extra,
                     dungeon.level[sublevel][y1][x2].extra,
                     dungeon.level[sublevel][ymid][xmid].extra, xmid, y1, rough, depth_max);

   perturb_point_end(dungeon.level[sublevel][y1][x2].extra,
                     dungeon.level[sublevel][y2][x2].extra,
                     dungeon.level[sublevel][ymid][xmid].extra, x2, ymid, rough, depth_max);

   perturb_point_end(dungeon.level[sublevel][y2][x2].extra,
                     dungeon.level[sublevel][y2][x1].extra,
                     dungeon.level[sublevel][ymid][xmid].extra, xmid, y2, rough, depth_max);

   perturb_point_end(dungeon.level[sublevel][y2][x1].extra,
                     dungeon.level[sublevel][y1][x1].extra,
                     dungeon.level[sublevel][ymid][xmid].extra, x1, ymid, rough, depth_max);


   /* Recurse the four quadrants */
   plasma_recursive(x1, y1, xmid, ymid, depth_max, rough);
   plasma_recursive(xmid, y1, x2, ymid, depth_max, rough);
   plasma_recursive(x1, ymid, xmid, y2, depth_max, rough);
   plasma_recursive(xmid, ymid, x2, y2, depth_max, rough);
}

/*
 * The rooms are sometimes much too small (1 square)
 * and also, the square wall at the outer edges is not good for building
 * tunnels - they stop at the outer edge, or become confused when there is
 * no room directly behind that wall....
 */

/*
 * Type 18 -- fractal shaped rooms up to 33x30
 */
static void build_type18(s16b xval, s16b yval)
{
   s16b   x, y, x2, y2, dx, dy;
   s16b   x1, y1;
   s16b   roughness = 1;
   bool   light;

   s16b   terrain_table[10] =
   {
      (DUNG_WALL*1000 + DUNG_WALL_GRANITE),  (DUNG_WALL*1000 + DUNG_WALL_GRANITE),
      (DUNG_WALL*1000 + DUNG_WALL_QUARTZ),   (DUNG_WALL*1000 + DUNG_WALL_MAGMA),
      (DUNG_WALL*1000 + DUNG_WALL_CHALK),    (DUNG_SHRUB*1000 + DUNG_SHRUB_OAK),
      (DUNG_FLOOR*1000 + DUNG_FLOOR_NORMAL), (DUNG_FLOOR*1000 + DUNG_FLOOR_NORMAL),
      (DUNG_FLOOR*1000 + DUNG_FLOOR_NORMAL), (DUNG_FLOOR*1000 + DUNG_FLOOR_NORMAL)
   };

   /* Choose lite or dark */
   light = (p_ptr->mdepth <= randint(25));

   dx=randint(10)+4;
   dy=randint(10)+3;

   /* Pick a room size */
   x1 = xval - dx;
   x2 = xval + dx;
   y1 = yval - dy;
   y2 = yval + dy;

dlog(DEBUGGENER,"generate.c: build_type18: room %d,%d to %d,%d\n", x1, y1, x2, y2);

   /* initialize a background in the room */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         dungeon.level[sublevel][y][x].extra = 5;
      }
   }

   plasma_recursive(x1, y1, x2, y2, 9, roughness);

   for (y = y1; y <= y2; y++)
   {
      for (x = x1; x <= x2; x++)
      {
         s16b mtyp, styp;
         mtyp = terrain_table[dungeon.level[sublevel][y][x].extra] / 1000;
         styp = terrain_table[dungeon.level[sublevel][y][x].extra] % 1000;
         set_grid_type(x, y, mtyp, styp, GRID_REPLACE, (light?(CAVE_ROOM | CAVE_GLOW):CAVE_ROOM) );
dlog(DEBUGGENER,"generate.c: built_type18: %d,%d feature %d,%d %s\n", x, y, mtyp, styp,
                f_name + f_info[get_f_idx(mtyp, styp)].name);
         dungeon.level[sublevel][y][x].extra = 0;
      }
   }

   /* Place a full wall around the room */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                             GRID_REPLACE, light?CAVE_GLOW:0L );
      }
   }
}

/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a monster nest or monster pit or
 * the given type.
 *
 * None of the pits/nests are allowed to include "unique" monsters,
 * or monsters which can "multiply".
 *
 * Some of the pits/nests are asked to avoid monsters which can blink
 * away or which are invisible.  This is probably a hack.
 *
 * The old method made direct use of monster "names", which is bad.
 *
 * Note the use of Angband 2.7.9 monster race pictures in various places.
 */

/*
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_jelly(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Require icky thing, jelly, mold, or mushroom */
   if (!strchr("ijm,", r_ptr->d_char)) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}

/*
 * Helper function for "monster nest (animal)"
 */
static bool vault_aux_animal(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Require "animal" flag */
   if (!(r_ptr->flags3 & RF3_ANIMAL)) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}


/*
 * Helper function for "monster nest (undead)"
 */
static bool vault_aux_undead(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Require Undead */
   if (!(r_ptr->flags3 & RF3_UNDEAD)) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}

/*
 * Helper function for "monster pit (orc)"
 */
static bool vault_aux_orc(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Hack -- Require "o" monsters */
   if (!strchr("o", r_ptr->d_char)) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}

/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_troll(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Hack -- Require "T" monsters */
   if (!strchr("T", r_ptr->d_char)) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}

/*
 * Helper function for "monster pit (giant)"
 */
static bool vault_aux_giant(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Hack -- Require "P" monsters */
   if (!strchr("P", r_ptr->d_char)) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}

/*
 * Hack -- breath type for "vault_aux_dragon()"
 */
static u32b vault_aux_dragon_mask4;

/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Hack -- Require "d" or "D" monsters */
   if (!strchr("Dd", r_ptr->d_char)) return (FALSE);

   /* Hack -- Require correct "breath attack" */
   if (r_ptr->flags4 != vault_aux_dragon_mask4) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}

/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_demon(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Hack -- Require "U" monsters */
   if (!strchr("U", r_ptr->d_char)) return (FALSE);

   /* Hack -- Skip unique monsters */
   if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);

   /* Okay */
   return (TRUE);
}

/*
 * Type 5 -- Monster nests
 *
 * A monster nest is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type strewn about the room.
 *
 * Hack -- we only pick 16 base monster types for efficiency, since
 * the "get_mon_num()" function is still (very) slow.  XXX XXX XXX
 *
 * A "better" method would be to create a "secondary" monster/object
 * allocation array, into which specialized "allocator" information
 * could be placed before calling the allocator.  XXX XXX XXX  This
 * secondary array could be constructed by the "get_mon_num()" code.
 *
 * Actually, splitting the "unique" monsters out of the "normal"
 * monsters would probably also speed the function a lot.
 *
 * Currently, a monster nest is one of
 *   a nest of "jelly" monsters   (Dungeon level 5 and deeper)
 *   a nest of "animal" monsters  (Dungeon level 30 and deeper)
 *   a nest of "undead" monsters  (Dungeon level 50 and deeper)
 *
 * Note that the "get_mon_num()" function may fail, in which case
 * the nest will be empty and will not effect the level rating.
 */
static void build_type5(s16b xval, s16b yval)
{
   s16b            x, y, x1, y1, x2, y2;

   s16b            tmp, i, what[16];

   bool            empty = FALSE;

   cave_cell_type      *c_ptr;

   /* Large room */
   y1 = yval - 4;
   y2 = yval + 3;
   x1 = xval - 11;
   x2 = xval + 11;

   /* Place the floor area */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                          GRID_REPLACE, CAVE_ROOM | CAVE_NEST );
      }
   }

   /* Place the outer walls */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
   }

   /* Advance to the center room */
   y1 = y1 + 2;
   y2 = y2 - 2;
   x1 = x1 + 2;
   x2 = x2 - 2;

   /* The inner walls */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
   }

   /* Place a secret door */
   switch (randint(4))
   {
      case 1: place_secret_door(xval, y1 - 1); break;
      case 2: place_secret_door(xval, y2 + 1); break;
      case 3: place_secret_door(x1 - 1, yval); break;
      case 4: place_secret_door(x2 + 1, yval); break;
   }

   /* Hack -- Choose a nest type */
   tmp = randint(p_ptr->mdepth);

   /* Monster nest (jelly) */
   if (tmp < 30)
   {
      /* Describe */
      if (cheat_room) msg_print("Monster nest (jelly)");

      /* Restrict to jelly */
      get_mon_num_hook = vault_aux_jelly;
   }

   /* Monster nest (animal) */
   else if (tmp < 50)
   {
      /* Describe */
      if (cheat_room) msg_print("Monster nest (animal)");

      /* Restrict to animal */
      get_mon_num_hook = vault_aux_animal;
   }

   /* Monster nest (undead) */
   else
   {
      /* Describe */
      if (cheat_room) msg_print("Monster nest (undead)");

      /* Restrict to undead */
      get_mon_num_hook = vault_aux_undead;
   }
   /* Increase depth allowance */
   monster_level = p_ptr->mdepth + 10;

   /* Prepare allocation table */
   get_mon_num_prep();

   /* Pick some monster types */
   for (i = 0; i < 16; i++)
   {
      /* Get (and save) a monster type */
      what[i] = get_mon_num(monster_level);

      /* Notice failure */
      if (!what[i]) empty = TRUE;
   }

   /* Restore depth allowance */
   monster_level = p_ptr->mdepth;

   /* Remove restriction */
   get_mon_num_hook = NULL;

   /* Prepare allocation table */
   get_mon_num_prep();

   /* Oops */
   if (empty) return;

   /* Increase the level rating */
   rating += 10;

   /* (Sometimes) Cause a "special feeling" (for "Monster Nests") */
   if ((p_ptr->mdepth <= 40) && (randint(p_ptr->mdepth*p_ptr->mdepth + 1) < 300))
   {
      good_item_flag = TRUE;
   }

   /* Place some monsters */
   for (y = yval - 2; y <= yval + 2; y++)
   {
      for (x = xval - 9; x <= xval + 9; x++)
      {
         s16b r_idx = what[rand_int(16)];

         /* Place that "random" monster (no groups) */
         (void)place_monster_aux(x, y, r_idx, FALSE, FALSE, 0, 0);
      }
   }
}

/*
 * Type 6 -- Monster pits
 *
 * A monster pit is a "big" room, with an "inner" room, containing
 * a "collection" of monsters of a given type organized in the room.
 *
 * Monster types in the pit
 *  orc pit (Dungeon Level 5 and deeper)
 *  troll pit  (Dungeon Level 20 and deeper)
 *  giant pit  (Dungeon Level 40 and deeper)
 *  dragon pit (Dungeon Level 60 and deeper)
 *  demon pit  (Dungeon Level 80 and deeper)
 *
 * The inside room in a monster pit appears as shown below, where the
 * actual monsters in each location depend on the type of the pit
 *
 *  #####################
 *  #0000000000000000000#
 *  #0112233455543322110#
 *  #0112233467643322110#
 *  #0112233455543322110#
 *  #0000000000000000000#
 *  #####################
 *
 * Note that it is now possible to "genericize" the monster pit creation
 * function, by requesting 16 "appropriate" monsters, bubble sorting them
 * by level, and then using a subset of those 16 monsters for the 8 types
 * of monster in the room, say, the middle entries, or the even entries.
 *
 * Hack -- all of the "dragons" in a "dragon" pit must be the same "color",
 * which is handled by requiring a specific "breath" attack for all of the
 * dragons.  This may include "multi-hued" breath.
 *
 * Currently, we are using every other entry.  Note that "wyrms" may be
 * present in many of the dragon pits, if they have the proper breath.
 *
 * Note that the "get_mon_num()" function may fail, in which case
 * the pit will be empty and will not effect the level rating.
 */
static void build_type6(s16b xval, s16b yval)
{
   s16b            tmp, what[16];
   s16b            i, j, y, x, y1, x1, y2, x2;
   bool            empty = FALSE, result = FALSE;
   cave_cell_type         *c_ptr;

   /* Large room */
   y1 = yval - 4;
   y2 = yval + 3;
   x1 = xval - 11;
   x2 = xval + 11;

   /* Place the floor area */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      for (x = x1 - 1; x <= x2 + 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, CAVE_ROOM | CAVE_PIT );
      }
   }

   /* Place the outer walls */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, 0L );
   }

   /* Advance to the center room */
   y1 = y1 + 2;
   y2 = y2 - 2;
   x1 = x1 + 2;
   x2 = x2 - 2;

   /* The inner walls */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAINNR,
                          GRID_REPLACE, 0L );
   }

   /* Place a secret door */
   switch (randint(4))
   {
      case 1: place_secret_door(xval, y1 - 1); break;
      case 2: place_secret_door(xval, y2 + 1); break;
      case 3: place_secret_door(x1 - 1, yval); break;
      case 4: place_secret_door(x2 + 1, yval); break;
   }

   /* Choose a pit type */
   tmp = randint(p_ptr->mdepth);

   /* Orc pit */
   if (tmp < 20)
   {
      /* Message */
      if (cheat_room) msg_print("Orc Pit");

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_orc;
   }

   /* Troll pit */
   else if (tmp < 40)
   {
      /* Message */
      if (cheat_room) msg_print("Troll Pit");

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_troll;
   }

   /* Giant pit */
   else if (tmp < 60)
   {
      /* Message */
      if (cheat_room) msg_print("Giant Pit");

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_giant;
   }

   /* Dragon pit (blue/elec) */
   else if (tmp < 63)
   {
      /* Message */
      if (cheat_room) msg_print("Dragon Pit (blue/elec)");

      /* Restrict dragon breath type */
      vault_aux_dragon_mask4 = RF4_BR_ELEC;

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_dragon;
   }

   /* Dragon pit (white/cold) */
   else if (tmp < 66)
   {
      /* Message */
      if (cheat_room) msg_print("Dragon Pit (white/cold)");

      /* Restrict dragon breath type */
      vault_aux_dragon_mask4 = RF4_BR_COLD;

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_dragon;
   }

   /* Dragon pit (green/poison) */
   else if (tmp < 69)
   {
      /* Message */
      if (cheat_room) msg_print("Dragon Pit (green/poison)");

      /* Restrict dragon breath type */
      vault_aux_dragon_mask4 = RF4_BR_POIS;

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_dragon;
   }

   /* Dragon pit (black/acid) */
   else if (tmp < 72)
   {
      /* Message */
      if (cheat_room) msg_print("Dragon Pit (black/acid)");

      /* Restrict dragon breath type */
      vault_aux_dragon_mask4 = RF4_BR_ACID;

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_dragon;
   }

   /* Dragon pit (red/fire) */
   else if (tmp < 75)
   {
      /* Message */
      if (cheat_room) msg_print("Dragon Pit (red/fire)");

      /* Restrict dragon breath type */
      vault_aux_dragon_mask4 = RF4_BR_FIRE;

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_dragon;
   }

   /* Dragon pit (multi-hued/multi) */
   else if (tmp < 80)
   {
      /* Message */
      if (cheat_room) msg_print("Dragon Pit (multi-hued/multi)");

      /* Restrict dragon breath type */
      vault_aux_dragon_mask4 = (RF4_BR_ACID | RF4_BR_ELEC |
                          RF4_BR_FIRE | RF4_BR_COLD | RF4_BR_POIS);

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_dragon;
   }

   /* Demon pit */
   else
   {
      /* Message */
      if (cheat_room) msg_print("Demon Pit");

      /* Restrict monster selection */
      get_mon_num_hook = vault_aux_demon;
   }

   /* Increase monster depth */
   monster_level = p_ptr->mdepth + 10;

   /* Prepare allocation table */
   get_mon_num_prep();

dlog(DEBUGGENER,"generate.c: build_type6: before selecting races: empty %d\n", empty);
   /* Pick some monster types */
   for (i = 0; i < 16; i++)
   {
      /* Get (and save) a monster type */
      what[i] = get_mon_num(monster_level);

dlog(DEBUGGENER,"generate.c: build_type6: 16 - monster %d r_idx %d %s\n", i, what[i], r_name + r_info[what[i]].name);
      /* Notice failure */
      if (!what[i])
      {
dlog(DEBUGGENER,"generate.c: build_type6: setting empty\n");
         empty = TRUE;
      }
   }

   /* Restore depth allowance */
   monster_level = p_ptr->mdepth;

   /* Remove restriction */
   get_mon_num_hook = NULL;

   /* Prepare allocation table */
   get_mon_num_prep();

   /* Oops */
   if (empty)
   {
      return;
dlog(DEBUGGENER,"generate.c: build_type6: empty!\n");
   }

   /* XXX XXX XXX */
   /* Sort the entries */
   for (i = 0; i < 16 - 1; i++)
   {
      /* Sort the entries */
      for (j = 0; j < 16 - 1; j++)
      {
         s16b i1 = j;
         s16b i2 = j + 1;

         s16b p1 = r_info[what[i1]].level;
         s16b p2 = r_info[what[i2]].level;

         /* Bubble */
         if (p1 > p2)
         {
            s16b tmp = what[i1];
            what[i1] = what[i2];
            what[i2] = tmp;
         }
      }
   }

   /* Select the entries */
   for (i = 0; i < 8; i++)
   {
      /* Every other entry */
      what[i] = what[i * 2];

      /* Message */
/* jk - this was cheat_room, but room creation is explicit with */
/* just telling what kind of pit it is, not what monsters are in it */
      if (cheat_hear) msg_print(r_name + r_info[what[i]].name);
dlog(DEBUGGENER,"generate.c: build_type6: monster %d %s\n", i, r_name + r_info[what[i]].name);
   }

   /* Increase the level rating */
   rating += 10;

   /* (Sometimes) Cause a "special feeling" (for "Monster Pits") */
   if ((p_ptr->mdepth <= 40) && (randint(p_ptr->mdepth*p_ptr->mdepth + 1) < 300))
   {
      good_item_flag = TRUE;
   }

   /* Top and bottom rows */
   for (x = xval - 9; x <= xval + 9; x++)
   {
dlog(DEBUGGENER,"generate.c: build_type6: trying to place r_idx %d %s at %d,%d\n",
                what[0], r_name + r_info[what[0]].name, x, yval-2);
      result |= place_monster_aux(x, yval - 2, what[0], FALSE, FALSE, 0, 0);
      c_ptr = &dungeon.level[sublevel][yval-2][x];
dlog(DEBUGGENER,"generate.c: build_type6: step 1 @ %d,%d - m_idx now %d r_idx %d %s\n",
                x, yval-2, c_ptr->m_idx, mn_list[c_ptr->m_idx].r_idx,
                r_name + r_info[mn_list[c_ptr->m_idx].r_idx].name);
      result |= place_monster_aux(x, yval + 2, what[0], FALSE, FALSE, 0, 0);
   }
dlog(DEBUGGENER,"generate.c: build_type6: pit top/bottom row: result %d\n", result);

   /* Middle columns */
   for (y = yval - 1; y <= yval + 1; y++)
   {
      result |= place_monster_aux(xval - 9, y, what[0], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 9, y, what[0], FALSE, FALSE, 0, 0);

      result |= place_monster_aux(xval - 8, y, what[1], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 8, y, what[1], FALSE, FALSE, 0, 0);

      result |= place_monster_aux(xval - 7, y, what[1], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 7, y, what[1], FALSE, FALSE, 0, 0);

      result |= place_monster_aux(xval - 6, y, what[2], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 6, y, what[2], FALSE, FALSE, 0, 0);

      result |= place_monster_aux(xval - 5, y, what[2], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 5, y, what[2], FALSE, FALSE, 0, 0);

      result |= place_monster_aux(xval - 4, y, what[3], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 4, y, what[3], FALSE, FALSE, 0, 0);

      result |= place_monster_aux(xval - 3, y, what[3], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 3, y, what[3], FALSE, FALSE, 0, 0);

      result |= place_monster_aux(xval - 2, y, what[4], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(xval + 2, y, what[4], FALSE, FALSE, 0, 0);
   }
dlog(DEBUGGENER,"generate.c: build_type6: pit middle columns: result %d\n", result);
   /* Above/Below the center monster */
   for (x = xval - 1; x <= xval + 1; x++)
   {
      result |= place_monster_aux(x, yval + 1, what[5], FALSE, FALSE, 0, 0);
      result |= place_monster_aux(x, yval - 1, what[5], FALSE, FALSE, 0, 0);
   }
dlog(DEBUGGENER,"generate.c: build_type6: pit above/below: result %d\n", result);

   /* Next to the center monster */
   result |= place_monster_aux(xval + 1, yval, what[6], FALSE, FALSE, 0, 0);
   result |= place_monster_aux(xval - 1, yval, what[6], FALSE, FALSE, 0, 0);
dlog(DEBUGGENER,"generate.c: build_type6: pit next to center: result %d\n", result);

   /* Center monster  */
   result |= place_monster_aux(xval, yval, what[7], FALSE, FALSE, 0, 0);
dlog(DEBUGGENER,"generate.c: build_type6: pit center: result %d\n", result);
}

/* jk - find a chest */
bool kind_is_chest(s16b k_idx)
{
   return (k_info[k_idx].tval==TV_CHEST);
}

/* jk - find a graveyard item */
bool kind_is_graveyard_item(s16b k_idx)
{
   s16b tval = k_info[k_idx].tval;
/* certain items don't belong in graveyards - they decay in time */
   if (tval==TV_NOTHING) return (FALSE);
   if (tval==TV_BOTTLE) return (FALSE);
   if (tval==TV_JUNK) return (FALSE);
   if (tval==TV_SPIKE) return (FALSE);
   if (tval==TV_CLOAK) return (FALSE);
   if (tval==TV_SOFT_ARMOR) return (FALSE);
   if (tval==TV_STAFF) return (FALSE);
   if (tval==TV_WAND) return (FALSE);
   if (tval==TV_SCROLL) return (FALSE);
   if (tval==TV_POTION) return (FALSE);
   if (tval==TV_FLASK) return (FALSE);
   if (tval==TV_FOOD) return (FALSE);
   return (TRUE);
}

/*
 * does a given monster race fit the constraints of this vault?
 * all = TRUE : test all flags
 * all = FALSE: it should have one of the flags the vaults wants
 */
static s16b monster_fits_vault(s16b r_idx, s16b v_num, bool all)
{
   bool ok1 = TRUE, ok2 = TRUE;

   /* should all flags from the vault match exactly? */
   if (all)
   {
      ok1 &= ((r_info[r_idx].flags1 & v_info[v_num].flags1) == v_info[v_num].flags1);
      ok1 &= ((r_info[r_idx].flags2 & v_info[v_num].flags2) == v_info[v_num].flags2);
      ok1 &= ((r_info[r_idx].flags3 & v_info[v_num].flags3) == v_info[v_num].flags3);
      ok1 &= ((r_info[r_idx].flags4 & v_info[v_num].flags4) == v_info[v_num].flags4);
      ok1 &= ((r_info[r_idx].flags5 & v_info[v_num].flags5) == v_info[v_num].flags5);
      ok1 &= ((r_info[r_idx].flags6 & v_info[v_num].flags6) == v_info[v_num].flags6);
#if 0
dlog(DEBUGGENER,"generate.c: monster_fits_vault: monst %08lx %08lx %08lx %08lx %08lx %08lx\n",
                r_info[r_idx].flags1, r_info[r_idx].flags2, r_info[r_idx].flags3,
                r_info[r_idx].flags4, r_info[r_idx].flags5, r_info[r_idx].flags6);
dlog(DEBUGGENER,"generate.c: monster_fits_vault: vault %08lx %08lx %08lx %08lx %08lx %08lx\n",
                v_info[v_num].flags1, v_info[v_num].flags2, v_info[v_num].flags3,
                v_info[v_num].flags4, v_info[v_num].flags5, v_info[v_num].flags6);
#endif
      ok2 &= ((r_info[r_idx].flags1 & v_info[v_num].nflags1) == 0L);
      ok2 &= ((r_info[r_idx].flags2 & v_info[v_num].nflags2) == 0L);
      ok2 &= ((r_info[r_idx].flags3 & v_info[v_num].nflags3) == 0L);
      ok2 &= ((r_info[r_idx].flags4 & v_info[v_num].nflags4) == 0L);
      ok2 &= ((r_info[r_idx].flags5 & v_info[v_num].nflags5) == 0L);
      ok2 &= ((r_info[r_idx].flags6 & v_info[v_num].nflags6) == 0L);
   }
   /* or are some enough? */
   else
   {
      ok1 = FALSE;
      ok1 |= (r_info[r_idx].flags1 & v_info[v_num].flags1);
      ok1 |= (r_info[r_idx].flags2 & v_info[v_num].flags2);
      ok1 |= (r_info[r_idx].flags3 & v_info[v_num].flags3);
      ok1 |= (r_info[r_idx].flags4 & v_info[v_num].flags4);
      ok1 |= (r_info[r_idx].flags5 & v_info[v_num].flags5);
      ok1 |= (r_info[r_idx].flags6 & v_info[v_num].flags6);
   }
#if 0
dlog(DEBUGGENER,"generate.c: monster_fits_vault: all %d name %s vault %s subl %d ok1 %d ok2 %d\n",
                all, r_name + r_info[r_idx].name,
                v_name + v_info[v_num].name, v_info[v_num].sublevel, ok1, ok2);
#endif
   return (ok1 && ok2);
}

/* Make the monster allocation table preparer aware of a racial restriction. -LM- */
/* Also use vault-option-flags, if any                                            */
static bool vault_aux_racial(s16b r_idx)
{
   bool          result= FALSE;
   monster_race *r_ptr = &r_info[r_idx];

   if (required_race != r_ptr->d_char) return (FALSE);
   result = monster_fits_vault(r_idx, current_vault, TRUE);
   return (result);
}

/*
 * Helper function for build_vault.  Takes a monster race, standard
 * generation level, and chance for level boosting as inputs, and returns
 * the highest-level legal monster of that race it can find.  Code is
 * adapted from function build_type6. -LM-
 */
static s16b create_specific_race(char race, s16b gen_level, s16b level_boost, s16b v_num, bool all, bool some)
{
   s16b what[8], index[8];
   s16b i, j, result, found_num = 0;

   bool empty = FALSE;

   /* Activate the racial restriction. */
   required_race = race;

   /* Make the monster allocation table preparer aware of this restriction. */
   get_mon_num_hook = vault_aux_racial;

   /* Prepare allocation table */
   get_mon_num_prep();

   /* Clear the racial restriction. */
   required_race = '\0';

   /* Two possible rounds of generation level boosting. */
   if (randint(100) < level_boost)
   {
      gen_level += 3;
      if (randint(100) < level_boost) gen_level += 3;
   }
dlog(DEBUGGENER,"generate.c: create_specific_race: starting, race %c level %d all %d some %d\n",
                race, gen_level, all, some);

   /* Pick some monster types */
   for (i = 0; i < 8; i++)
   {
      bool   ok = FALSE;
      s16b   cnt = 0;

      while (!ok)
      {
         cnt++;
         if (cnt==10)
         {
            break;
         }
         /* Get a monster type */
         what[i] = get_mon_num(gen_level);

         /* Notice failure */
         if (!what[i])
         {
            continue;
         }
      }
   }

   for (i=0; i<8; i++)
   {
      if (what[i] == 0)
      {
         empty = TRUE;
      }
      else
      {
         index[found_num++]=what[i];
      }
   }

   /* Remove restriction */
   get_mon_num_hook = NULL;

   /* Prepare allocation table */
   get_mon_num_prep();

   /* Oops */
   if (empty)
   {
      return(0);
   }

   /* Sort the entries XXX XXX XXX */
   for (i = 0; i < 8 - 1; i++)
   {
      /* Sort the entries */
      for (j = 0; j < 8 - 1; j++)
      {
         int i1 = j;
         int i2 = j + 1;

         int p1 = r_info[what[i1]].level;
         int p2 = r_info[what[i2]].level;

         /* Bubble */
         if (p1 > p2)
         {
            int tmp = what[i1];
            what[i1] = what[i2];
            what[i2] = tmp;
         }
      }
   }

   /* Return the toughest (or next toughest, for variety) monster. */
   result = (what[5 + randint(2)]);
dlog(DEBUGGENER,"generate.c: create_specific_race: monster r_idx %d = %s chosen\n",
                result, r_name+r_info[result].name);
   return (result);
}

/*
 * count how many unused sublevels we still can use in this level
 */
static s16b find_total_free_sublevels(s16b baselevel)
{
   s16b i, result = 0;
   for (i = 1; i < MAX_SUB_LEVELS; i++)
   {
      if (!dungeon.level_used[i])
      {
         result++;
      }
   }
   return (result);
}

/*
 * return TRUE if we have enough free sub-levels to build this vault
 */
static bool test_sublevels(s16b baselevel, s16b vault_num)
{
   s16b i, total = 0;
   vault_type      *v_ptr = &v_info[vault_num];

   for (i = 0; i < v_number; i++)
   {
      if (v_info[i].typ != v_ptr->typ) continue;
      if (v_info[i].id == v_ptr->id)
      {
         total++;
      }
   }
   if (total > find_total_free_sublevels(baselevel))
   {
      return (FALSE);
   }
   else
   {
      return (TRUE);
   }
}

/*
 * return the number of the first unused sublevel in this level
 */
static s16b find_free_sublevel(s16b baselevel)
{
   s16b i;
   for (i = 1; i < MAX_SUB_LEVELS; i++)
   {
      if (!dungeon.level_used[i])
      {
         return (i);
      }
   }
   return (-1);
}

/*
 * allocate memory for a sublevel
 */
void allocate_sublevel(s16b sublevel)
{
   s16b j;

   /* Allocate and wipe each line of the sub-level */
   for (j = 0; j < SCREEN_HGT; j++)
   {
      /* Allocate one row of the cave */
      C_MAKE(dungeon.level[sublevel][j], SCREEN_WID, cave_cell_type);
   }
}

/*
 * we call this function from handle_sublevel() below, which in turn is called
 * from build_vault, so forward-declare build_vault() here.
 */
static void build_vault(s16b xval, s16b yval, s16b vault_num, bool light,
                        s16b *cent_dx, s16b *cent_dy);

/*
 * if we find a staircase in a vault, we may have to built sub-level vaults
 * test if this can be done, and return TRUE if it is possible
 *
 * we allocate space, and setup variables, then call build_vault again to
 * build the next sub-level.
 *
 * Note: this may happen more times, recursively, until no more staircases
 *       are found!
 */
static bool handle_sublevel(s16b baselevel, s16b vault_num)
{
   s16b             i, old_cur_hgt, old_cur_wid, free_sublevel;
   s16b             next_vault_num = -1, light_chance;
   s16b             x, y, old_sublevel = -1;
   s16b             cent_x, cent_y;
   vault_type      *v_ptr = &v_info[vault_num];

dlog(DEBUGGENER,"generate.c: handle_sublevel: starting baselevel %d, vaultnum %d\n",
                baselevel, vault_num);
   /* find the sub-level-vault that should be built with this vault */
   /* this is the vault with typ == v_ptr->typ, id == v_ptr->id     */
   /* and sublevel == v_ptr->sublevel+1                             */
   for (i = 0; i < v_number; i++)
   {
      if (v_info[i].typ != v_ptr->typ) continue;
      if (v_info[i].id == v_ptr->id)
      {
dlog(DEBUGGENER,"generate.c: handle_sublevel: vault %d typ %d id %d sublevel %d found\n",
                i, v_info[i].typ, v_info[i].id, v_info[i].sublevel);
         if ( ( (v_info[i].options & VAULT_GOING_DOWN) && !(v_ptr->options & VAULT_GOING_DOWN) ) ||
              ( (v_info[i].options & VAULT_GOING_UP) && !(v_ptr->options & VAULT_GOING_UP) ) )
         {
            dlog(DEBUGEXTRA,"generate.c: handle_sublevel: vault direction not constant type %d id %d\n",
                             v_info[i].typ, v_info[i].id);
            quit("Vault direction not constant");
         }
         if (v_info[i].sublevel==(v_ptr->sublevel + 1))
         {
            break;
         }
      }
   }
   /* no luck */
   if (i == v_number)
   {
      msg_format("generate.c: handle_sublevel: vault not found: type %d id %d, start %d, max %d, wanted %d\n",
                 v_ptr->typ, v_ptr->id, vault_num, v_number, v_ptr->sublevel+1);
dlog(DEBUGGENER,"generate.c: handle_sublevel: vault not found: type %d id %d, start %d, max %d, wanted %d\n",
                 v_ptr->typ, v_ptr->id, vault_num, v_number, v_ptr->sublevel+1);
      quit("Out-of-order sub-level vault found.");
   }
   else
   {
      next_vault_num = i;
   }
dlog(DEBUGGENER,"generate.c: handle_sublevel: vault %d continues as %d\n",
                vault_num, next_vault_num);

   free_sublevel = find_free_sublevel(baselevel);

   if (free_sublevel==-1)
   {
      quit("generate.c: test_and_build_sublevel: no free sublevels?");
   }

dlog(DEBUGGENER,"generate.c: handle_sublevel: using sublevel %d\n",
                sublevel);

   old_sublevel = sublevel;
   sublevel = free_sublevel;

   /* set things up for this sublevel */
   old_cur_wid = cur_wid;
   old_cur_hgt = cur_hgt;
   cur_wid = SCREEN_WID;
   cur_hgt = SCREEN_HGT;

   /* index[i] is now the v_info number for sub-level i */
   dungeon.level_used[sublevel] = TRUE;

   if (v_ptr->options & VAULT_GOING_DOWN)
   {
      dungeon.level_depth[sublevel] = v_info[next_vault_num].sublevel;
   }
   else
   {
      dungeon.level_depth[sublevel] = -v_info[next_vault_num].sublevel;
   }

   dungeon.level_name[sublevel] = v_info[next_vault_num].name;
   dungeon.level_wid[sublevel] = SCREEN_WID;
   dungeon.level_hgt[sublevel] = SCREEN_HGT;

dlog(DEBUGGENER,"generate.c: handle_sublevel: sublevel %d has depth %d name %s\n",
                sublevel, dungeon.level_depth[sublevel], v_name + dungeon.level_name[sublevel]);

   allocate_sublevel(sublevel);

dlog(DEBUGGENER,"generate.c: handle_sublevel: sublevel %d allocated\n",
                sublevel);

   /* Start with basic granite */
   for (y = 0; y < cur_hgt; y++)
   {
      for (x = 0; x < cur_wid; x++)
      {
         /* Clear all features, set to granite */
         place_wall(x, y);
      }
   }

   /* the dungeon is cur_wid x cur_hgt cells, build in the middle */
   x = (cur_wid / 2);
   y = (cur_hgt / 2);
dlog(DEBUGGENER,"generate.c: handle_sublevel: cur_wid %d, cur_hgt %d, x,y %d,%d\n",
                cur_wid, cur_hgt, x, y);

   light_chance = 0;
   if (v_info[next_vault_num].options & VAULT_LIGHT10) light_chance += 10;
   if (v_info[next_vault_num].options & VAULT_LIGHT25) light_chance += 25;
   if (v_info[next_vault_num].options & VAULT_LIGHT50) light_chance += 50;

dlog(DEBUGGENER,"generate.c: handle_sublevel: calling build_vault main %d sub %d @ %d,%d vault_num %d\n",
                baselevel, sublevel, x, y, next_vault_num);

   build_vault(x, y, next_vault_num, (rand_int(100)<light_chance), &cent_x, &cent_y);

   /* restore the old level */
   cur_wid = old_cur_wid;
   cur_hgt = old_cur_hgt;

   sublevel = old_sublevel;

   return (TRUE);
}


/*
 * Helper function for vaults with monsters and defined flags
 */
static bool vault_satisfy_monster_flags(s16b r_idx)
{
   return (monster_fits_vault(r_idx, current_vault, TRUE));
}

/*
 * this function tries to place a monster
 */
static void place_vault_monster(s16b vault_num, char race_char, s16b x, s16b y, bool light)
{
   s16b tmp_race, try, mon_level = 0, v_max_mon_level;
   vault_type      *v_ptr = &v_info[vault_num];

dlog(DEBUGGENER,"generate.c: place_vault_monster: step1: char %c @ %d,%d subl %d\n", race_char, x, y, sublevel);
   /* Place a monster, awake, no groups, of the race indicated by the letter given,
    * biased towards current depth + 3, with a 30% chance of being an additional 3
    * levels out of depth, and 9% of being another 3 (for a total of 9 out of depth,
    * plus any bonus the mon_num_gen function grants).
    * (A little hack:  interesting rooms don't have quite as tough monsters).
    */
   /* first try to get a race with all the asked for flags         */
   /* if this doesn't succeed, and we have VAULT_ALL_MONSTERS set, */
   /* try to get some other monsters                               */

   tmp_race = create_specific_race(race_char, light ? p_ptr->mdepth : p_ptr->mdepth + 3,
                                   30, vault_num, TRUE, FALSE);
dlog(DEBUGGENER,"generate.c: place_vault_monster - placing monster, all flags ok, race %s\n",
                r_name + r_info[tmp_race].name);

   if (place_monster_aux(x, y, tmp_race, FALSE, FALSE, 1, 0) &&
       (v_ptr->options & VAULT_RACE_ALL))
   {
      return;
   }

   /* it didn't work - get higher level monsters first */

   try = 0;
   mon_level = light ? p_ptr->mdepth : (p_ptr->mdepth + 3);
   v_max_mon_level = p_ptr->mdepth + 3;
   if (v_ptr->options & VAULT_RACE_MAX4) v_max_mon_level += 4;
   if (v_ptr->options & VAULT_RACE_MAX8) v_max_mon_level += 8;
   if (v_ptr->options & VAULT_RACE_MAX16) v_max_mon_level += 16;
   if (v_ptr->options & VAULT_RACE_MAX32) v_max_mon_level += 32;
   if (v_max_mon_level > MAX_LEVEL) v_max_mon_level = MAX_LEVEL;

   while (try < 5)
   {
      try++;
      mon_level += 10;
      if (mon_level > v_max_mon_level)
      {
         try = 5;
         mon_level = v_max_mon_level;
      }

      tmp_race = create_specific_race(race_char, mon_level, 30, vault_num, TRUE, FALSE);
dlog(DEBUGGENER,"generate.c: place_vault_monster - placing monster try %d, all flags ok, race %s, level %d of %d\n",
                try, r_name + r_info[tmp_race].name, mon_level, v_max_mon_level);
      if (place_monster_aux(x, y, tmp_race, FALSE, FALSE, 1, 0))
      {
         return;
      }
   }

   /* even that didn't work - some strange flags in this vault, if no monsters can be found */
   /* to satisfy all of them                                                                */
   if (v_ptr->options & VAULT_RACE_SOME)
   {
      tmp_race = create_specific_race(race_char, light ? p_ptr->mdepth : p_ptr->mdepth + 3,
                                   30, vault_num, FALSE, TRUE);
dlog(DEBUGGENER,"generate.c: place_vault_monster - placing monster, some flags ok, race %s\n",
                r_name + r_info[tmp_race].name);
      if (place_monster_aux(x, y, tmp_race, FALSE, FALSE, 1, 0))
      {
         return;
      }
   }

   /* can we try a random other monster? if not, give an error */
   if (!v_ptr->options & VAULT_RACE_ALL)
   {
      dlog(DEBUGEXTRA,"generate.c: place_vault_monster: no correct monster found for vault %d, char %c\n",
                       vault_num, race_char);
      return;
   }

   tmp_race = create_specific_race(race_char, light ? p_ptr->mdepth : p_ptr->mdepth + 3,
                                   30, vault_num, FALSE, FALSE);
dlog(DEBUGGENER,"generate.c: place_vault_monster - placing monster, random, race %s\n",
                r_name + r_info[tmp_race].name);
   /* we are not interested in return codes anymore */
   (void)place_monster_aux(x, y, tmp_race, FALSE, FALSE, 1, 0);
}

/*
 * this function mirrors a vault-layout if possible
 */
static void prepare_vault(s16b vault_num, char *layout)
{
   cptr        t;
   vault_type *v_ptr = &v_info[vault_num];
   s16b        dx, dy, i;

   if ( (randint(4)==1) && (v_ptr->options & VAULT_MIRROR_X) && (v_ptr->options & VAULT_MIRROR_Y) )
   /* try to mirror around x-axis and y-axis */
   {
      for (t=v_text+v_ptr->text, i=0, dy = 0; dy < v_ptr->hgt; dy++)
      {
         for (dx = 0; dx < v_ptr->wid; dx++, t++)
         {
            layout[((v_ptr->hgt-dy-1)*v_ptr->wid) + (v_ptr->wid-dx-1)] = *t;
         }
      }
      return;
   }

   /* try to mirror around x-axis */
   if ( (randint(2)==1) && (v_ptr->options & VAULT_MIRROR_X) )
   {
      for (t=v_text+v_ptr->text, i=0, dy = 0; dy < v_ptr->hgt; dy++)
      {
         for (dx = 0; dx < v_ptr->wid; dx++, t++)
         {
            layout[(dy*v_ptr->wid) + (v_ptr->wid-dx-1)] = *t;
         }
      }
      return;
   }
   /* try to mirror around y-axis */
   if ( (randint(2)==1) && (v_ptr->options & VAULT_MIRROR_Y) )
   {
      for (t=v_text+v_ptr->text, i=0, dy = 0; dy < v_ptr->hgt; dy++)
      {
         for (dx = 0; dx < v_ptr->wid; dx++, t++)
         {
            layout[((v_ptr->hgt-dy-1)*v_ptr->wid) + dx] = *t;
         }
      }
      return;
   }
   /* straight copy */
   for (t=v_text+v_ptr->text, i=0, dy = 0; dy < v_ptr->hgt; dy++)
   {
      for (dx = 0; dx < v_ptr->wid; dx++, t++)
      {
         layout[dy*v_ptr->wid + dx] = *t;
      }
   }
   return;
}

/*
 * make a vault from info in lib/edit/v_info.txt.
 * x,y are real screen coordinates, not block numbers!
 *
 * we assume we can allocate sub-level in continuous blocks.
 *
 * the legenda is:
 *
 * ~   chest               * 1   ordinary monster or object or trap
 * !   potion              * 2   slightly out of depth monster
 * @   lava floor          * 3   slightly out of object
 * #   inner granite wall  * 4   slightly out of depth monster and/or object
 * $   gold                * 5   out of depth object
 * %   outer granite wall  * 6   out of depth monster
 * ^   trap                * 7   very out of depth object
 * &   treasure or trap    * 8   very out of depth monster
 * *   treasure seam       * 9   very out of depth monster + out of depth object
 * (                       * 0   way out of depth monster + way out of depth object
 * )                       * "   amulet
 * _   staff               * ;   tree
 * +   secret door         * '   door in vaults that attracts tunnels - place only at the edges!!!!
 * `                       * <   staircase in multi-level vaults (type 17)
 * -   wand or rod         * >   staircase in multi-level vaults (type 17)
 * =   ring                * ?   scroll
 * {                       * ,   food or mushroom
 * }                       * .   normal floor
 * [                       * /
 * ]   armour              * |   weapon
 * '   secret door, which is used to let tunnels approach vaults on the right side.
 * X   permanent wall, outer on the outside, inner on the inside
 * x   water
 * :   rubble
 * a-z,A-Z not X means monster with that char
 * Many, many lines of this code comes from Leon Marrick in his Oangband.
 * - LM - points to him.
 */
static void build_vault(s16b xval, s16b yval, s16b vault_num, bool light,
                        s16b *cent_dx, s16b *cent_dy)
{
   s16b             dx, dy, x, y, obj_boost = 0;
   cptr             t;
   cave_cell_type  *c_ptr;
   vault_type      *v_ptr = &v_info[vault_num];
   char             vault_layout[4096]; /* this should be enough to hold the largest vault! */
                                        /* which currently is 66x40 maximum                 */
   s16b             xmax = v_ptr->wid;
   s16b             ymax = v_ptr->hgt;
   bool             do_sublevel = FALSE;

dlog(DEBUGGENER,"generate.c: build_vault: @ %d,%d to %d,%d (bounds %d,%d to %d,%d) light %d\n",
                xval - (xmax / 2), yval - (ymax / 2),
                xval + (xmax / 2), yval + (ymax / 2),
                0, 0, cur_wid - 1, cur_hgt - 1, light);
dlog(DEBUGGENER,"generate.c: build_vault: options %04Lx\n", v_ptr->options);

   prepare_vault(vault_num, vault_layout);
#if (debuglevel & DEBUGGENER)
{
   dlog(DEBUGGENER,"generate.c: build_vault: after prepare_vault:\n");
   /* Place dungeon features and objects */
   for (t = &vault_layout[0], dy = 0; dy < ymax; dy++)
   {
      for (dx = 0; dx < xmax; dx++, t++)
      {
         /* Extract the location */
         x = xval - (xmax / 2) + dx;
         y = yval - (ymax / 2) + dy;

         /* Hack -- skip "non-grids" */
         dlog(DEBUGGENER,"%c", *t);
      }
      dlog(DEBUGGENER,"\n");
   }
}
#endif

   obj_boost += ( (v_ptr->options & VAULT_BOOSTOBJ1) ? 1 : 0);
   obj_boost += ( (v_ptr->options & VAULT_BOOSTOBJ2) ? 2 : 0);
   obj_boost += ( (v_ptr->options & VAULT_BOOSTOBJ4) ? 4 : 0);
   obj_boost += ( (v_ptr->options & VAULT_BOOSTOBJ8) ? 8 : 0);

   current_vault = vault_num;

dlog(DEBUGGENER,"generate.c: build_vault: all v_flags: %04x\n",
                (v_ptr->flags1 | v_ptr->flags2 | v_ptr->flags3 |
                v_ptr->flags4 | v_ptr->flags5 | v_ptr->flags6 |
                v_ptr->nflags1 | v_ptr->nflags2 | v_ptr->nflags3 |
                v_ptr->nflags4 | v_ptr->nflags5 | v_ptr->nflags6));
   if ( (v_ptr->flags1 | v_ptr->flags2 | v_ptr->flags3 |
         v_ptr->flags4 | v_ptr->flags5 | v_ptr->flags6 |
         v_ptr->nflags1 | v_ptr->nflags2 | v_ptr->nflags3 |
         v_ptr->nflags4 | v_ptr->nflags5 | v_ptr->nflags6) != 0L)
   {
      get_mon_num_hook = vault_satisfy_monster_flags;
   }

   /* Place dungeon features and objects */
   for (t = &vault_layout[0], dy = 0; dy < ymax; dy++)
   {
      for (dx = 0; dx < xmax; dx++, t++)
      {
         /* Extract the location */
         x = xval - (xmax / 2) + dx;
         y = yval - (ymax / 2) + dy;
dlog(DEBUGGENER2,"generate.c: build_vault: char %c real %d,%d\n", *t, x, y);
         /* Hack -- skip "non-grids" */
         if (*t == ' ') continue;

         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][y][x];

         /* Lay down a floor, part of a vault                                         */
         /* don't built sublevels like all parts of the floor are vaults, it prevents */
         /* teleporting around etc.                                                   */
         if (sublevel == 0)
         {
            (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, CAVE_ROOM | CAVE_VAULT );
dlog(DEBUGGENER2,"generate.c: build_vault: floor (CAVE_ROOM|CAVE_VAULT) laid in\n");
         }
         else
         {
            (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_REPLACE, CAVE_ROOM );
dlog(DEBUGGENER2,"generate.c: build_vault: floor (CAVE_ROOM, sublevel<>0) laid in\n");
         }
         if (light) c_ptr->fdat |= CAVE_GLOW;

         /* Analyze the grid */
         switch (*t)
         {
            /* Granite wall (outer) */
            case '%':
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                                    GRID_REPLACE, CAVE_VAULT );
                break;

            /* Granite wall (inner) */
            case '#':
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRAINNR,
                                    GRID_REPLACE, CAVE_VAULT );
                break;

            /* Permanent wall: outer if on the edge, else inner */
            /* the edge: the top and bottom lines, the first and last columns, and every
             * cell that has a space before it or after it. Vaults shouldn't have spaces
             * inside them, after all a space builds nothing.
             */
            case 'X':
                if ( (dy == 0) || (dy == (ymax -1)) || (dx == 0) || (dx == (xmax - 1)) ||
                     ( (dx>0) && (*(t-1)==' ')) || ( (dx<(xmax-1)) && (*(t+1)==' ') ) )
                {
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d outer \n", *t, dx, dy, x, y);
                   (void)set_grid_type(x, y, DUNG_PERWALL, DUNG_PERWALL_OUTER,
                                       GRID_REPLACE, CAVE_VAULT );
                }
                else
                {
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d outer \n", *t, dx, dy, x, y);
                   (void)set_grid_type(x, y, DUNG_PERWALL, DUNG_PERWALL_INNER,
                                       GRID_REPLACE, CAVE_VAULT );
                }
                break;

            /* Treasure/trap */
            case '*':
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRTREAS,
                                    GRID_REPLACE, CAVE_VAULT );
                break;
            /* Rubble. -LM- */
            case ':':
            {
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
               (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_RUBBLE,
                                   GRID_REPLACE, CAVE_ROOM | CAVE_VAULT);
               break;
            }
            /* Lava. -LM- */
            case '@':
            {
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
               (void)set_grid_type(x, y, DUNG_LAVA, DUNG_LAVA_NORMAL,
                                   GRID_REPLACE, CAVE_ROOM | CAVE_VAULT);
               break;
            }
            /* Water. -LM- */
            case 'x':
            {
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
               (void)set_grid_type(x, y, DUNG_WATER, DUNG_WATER_POOL,
                                   GRID_REPLACE, CAVE_ROOM | CAVE_VAULT);
               break;
            }

            /* Tree. -LM- */
            case ';':
            {
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
               (void)set_grid_type(x, y, DUNG_SHRUB,
                                   DUNG_SHRUB_START + randint(DUNG_SHRUB_END),
                                   GRID_REPLACE, CAVE_ROOM | CAVE_VAULT);
               break;
            }
            /* Treasure/trap */
            case '&':
            {
               if (rand_int(100) < 50)
               {
                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, FALSE, FALSE, 0);
                  if (obj_boost) object_level -= obj_boost;
               }
               else
               {
                  place_trap(x, y, p_ptr->mdepth, 15, 5);
               }
               break;
            }

            /* Secret doors */
            /* this is used to lead a tunnel to a vault */
            case '+':
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                place_secret_door(x, y);
                break;

            /* Secret doors that attract tunnels */
            case '\'':
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                place_secret_door(x, y);

                /* now try to get a tunnel to here */
                if (dun->cent_n < CENT_MAX)
                {
                   /* make sure the new tunnel starts *outside* the room
                    * first we handle the 4 cases where doors are on the edge of vaults
                    */
                   if ((dy == 0) && (in_bounds(x, y-1)))
                   {
                      (*cent_dx) = dx;
                      (*cent_dy) = dy-1;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d N @ %d,%d\n", dun->cent_n-1, x, y-1);
                   }
                   else if ((dy == (ymax-1)) && (in_bounds(x, y+1)))
                   {
                      (*cent_dx) = dx;
                      (*cent_dy) = dy+1;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d S @ %d,%d\n", dun->cent_n-1, x, y+1);
                   }
                   else if ((dx == 0) && (in_bounds(x-1, y)))
                   {
                      (*cent_dx) = dx-1;
                      (*cent_dy) = dy;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d W @ %d,%d\n", dun->cent_n-1, x-1, y);
                   }
                   else if ((dx == (xmax-1)) && (in_bounds(x+1, y)))
                   {
                      (*cent_dx) = dx+1;
                      (*cent_dy) = dy;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d E @ %d,%d\n", dun->cent_n-1, x+1, y);
                   }
                   /* it could be possible that a door is not on the edge of a vault, yet valid.
                    *               X
                    *               X
                    *               X
                    *               X
                    *               X
                    *               X
                    * XXXXXXXXXX'XXXX
                    *
                    * would be valid, for example
                    */
                   else if ( (dx>0) && (*(t-1)==' '))
                   {
                      (*cent_dx) = dx-1;
                      (*cent_dy) = dy;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d W(2) @ %d,%d\n", dun->cent_n-1, x-1, y);
                   }
                   else if ( (dx<(xmax-1)) && (*(t+1)==' ')) 
                   {
                      (*cent_dx) = dx+1;
                      (*cent_dy) = dy;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d E(2) @ %d,%d\n", dun->cent_n-1, x+1, y);
                   }
                   else if ( (dy>0) && (*(t-xmax)==' '))
                   {
                      (*cent_dx) = dx;
                      (*cent_dy) = dy-1;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d N(2) @ %d,%d\n", dun->cent_n-1, x, y-1);
                   }
                   else if ( (dy<(ymax-1)) && (*(t+xmax)==' ')) 
                   {
                      (*cent_dx) = dx;
                      (*cent_dy) = dy+1;
dlog((DEBUGEXTRA|DEBUGGENER2),"generate.c: build_vault: adding new tunnel focus %d S(2) @ %d,%d\n", dun->cent_n-1, x, y+1);
                   }
                   else
                   {
                      dlog(DEBUGALWAYS,"generate.c: build_vault: vault %d (%s) strange door detected @ %d,%d (xmax,ymax %d,%d)\n",
                                       vault_num, v_name + v_ptr->name, dx, dy, xmax, ymax);
                   }
                }
                break;


            /* up stair */
            case '<':
dlog(DEBUGGENER,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                /* if we are at the beginning (on the main level) and */
                /* we don't have enough sublevels available, skip it! */
                if ((sublevel == 0) && !test_sublevels(baselevel, vault_num)) continue;
                /* does this point to the next or the previous sublevel? */
                if (v_ptr->options & VAULT_GOING_DOWN)
                {
                   /* the previous */
                   place_main_up_stair(x, y, FALSE);
                   /* are we one level from the main level? */
                   /* we can have more sets of sublevels per level :-) */
dlog(DEBUGGENER,"generate.c: build_vault:sublevel %d, depth[%d] %d\n",
                sublevel, dungeon.level_depth[sublevel]);
                   /* does this stair point back to the main level?       */
                   /* in that case, this depth = 1                        */
                   if (dungeon.level_depth[sublevel] == 1)
                   {
                      point_stair_to_level(x, y, baselevel, 0);
dlog(DEBUGGENER,"generate.c: build_vault: < found in vault GOING_DOWN level %d,%d, pointing back at %d,%d (baselevel)\n",
                baselevel, sublevel, baselevel, 0);
                   }
                   else /* we point back to an earlier sublevel */
                   {
                      point_stair_to_level(x, y, baselevel, sublevel - 1);
dlog(DEBUGGENER,"generate.c: build_vault: < found in vault GOING_DOWN level %d,%d, pointing back at %d,%d\n",
                baselevel, sublevel, baselevel, sublevel - 1);
                   }
                }
                else /* we're going up, so this should point to the next sublevel */
                {
                   s16b next_free_sublevel = find_free_sublevel(baselevel);
                   do_sublevel = TRUE;

                   place_main_up_stair(x, y, FALSE);
                   point_stair_to_level(x, y, baselevel, next_free_sublevel);
dlog(DEBUGGENER,"generate.c: build_vault: < found in vault GOING_UP level %d,%d, pointing back at %d,%d\n",
                baselevel, sublevel, baselevel, next_free_sublevel);
                }
                break;

            /* down stair */
            case '>':
dlog(DEBUGGENER,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                /* if we are at the beginning (on the main level) and */
                /* we don't have enough sublevels available, skip it! */
                if ((sublevel == 0) && !test_sublevels(baselevel, vault_num)) continue;
                /* does this point to the next or the previous sublevel? */
                if (v_ptr->options & VAULT_GOING_UP)
                {
                   /* the previous */
                   place_main_up_stair(x, y, FALSE);
                   /* are we one level from the main level?            */
                   /* we are then at depth -1                          */
                   /* we can have more sets of sublevels per level :-) */
                   if (dungeon.level_depth[sublevel] == -1 )
                   {
dlog(DEBUGGENER,"generate.c: build_vault: > found in vault GOING_UP level %d,%d, pointing back at %d,%d\n",
                baselevel, sublevel, baselevel, 0);
                      point_stair_to_level(x, y, baselevel, 0);
                   }
                   else /* we point back to an earlier sublevel */
                   {
                      point_stair_to_level(x, y, baselevel, sublevel - 1);
dlog(DEBUGGENER,"generate.c: build_vault: > found in vault GOING_UP level %d,%d, pointing back at %d,%d\n",
                baselevel, sublevel, baselevel, sublevel - 1);
                   }
                }
                else
                {
                   s16b next_free_sublevel = find_free_sublevel(baselevel);
                   do_sublevel = TRUE;

                   place_main_down_stair(x, y, FALSE);
                   point_stair_to_level(x, y, baselevel, next_free_sublevel);
dlog(DEBUGGENER,"generate.c: build_vault: > found in vault GOING_DOWN level %d,%d, pointing back at %d,%d\n",
                baselevel, sublevel, baselevel, next_free_sublevel);
                }
                break;

            /* Trap */
            case '^':
dlog(DEBUGGENER2,"generate.c: build_vault: step1: char %c vault %d,%d real %d,%d\n", *t, dx, dy, x, y);
                place_trap(x, y, p_ptr->mdepth, 10, 2);
                break;
         }
      }
   }

   if (do_sublevel)
   {
dlog(DEBUGGENER,"generate.c: build_vault: about to call handle_sublevel\n");
      handle_sublevel(baselevel, vault_num);
      do_sublevel = FALSE;
      /* reset this */
      current_vault = vault_num;
   }

   /* Place dungeon features and objects */
   for (t = &vault_layout[0], dy = 0; dy < ymax; dy++)
   {
      for (dx = 0; dx < xmax; dx++, t++)
      {
         s16b tmpx, tmpy;
         s16b temp;

         /* Extract the location */
         x = xval - (xmax / 2) + dx;
         y = yval - (ymax / 2) + dy;
         tmpx = x;
         tmpy = y;


         /* Hack -- skip "non-grids" */
         if (*t == ' ') continue;

         /* Access the grid */
         c_ptr = &dungeon.level[sublevel][y][x];

         /* All alphabetic characters, other than 'X' and 'x',
          * signify monster races. -LM-
          */

         if (isalpha((int)*t) && (*t != 'x') && (*t != 'X'))
         {
            place_vault_monster(vault_num, *t, x, y, light);
         }

         /* Otherwise, analyze the symbol */
         else
         {
            /* install the right hook if necessary */
            if ( (v_ptr->flags1 | v_ptr->flags2 | v_ptr->flags3 |
                  v_ptr->flags4 | v_ptr->flags5 | v_ptr->flags6 |
                  v_ptr->nflags1 | v_ptr->nflags2 | v_ptr->nflags3 |
                  v_ptr->nflags4 | v_ptr->nflags5 | v_ptr->nflags6) != 0L)
                  {
                     get_mon_num_hook = vault_satisfy_monster_flags;

                     /* Prepare allocation table */
                     get_mon_num_prep();
                  }
            switch (*t)
            {
               /* An ordinary monster, object (sometimes good), or trap. -LM- */
               case '1':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  if (randint(3) == 1)
                  {
                     place_monster(x, y, TRUE, TRUE, TRUE);
                  }
                  /* I had not intended this function to create
                   * guaranteed "good" quality objects, but perhaps
                   * it's better that it does at least sometimes.
                   */
                  else if (randint(2) == 1)
                  {
                     if (randint(4) == 1)
                     {
                        if (obj_boost) object_level += obj_boost;
                        place_object(x, y, TRUE, FALSE, FALSE);
                        if (obj_boost) object_level -= obj_boost;
                     }
                     else
                     {
                        if (obj_boost) object_level += obj_boost;
                        place_object(x, y, FALSE, FALSE, FALSE);
                        if (obj_boost) object_level -= obj_boost;
                     }
                  }
                  else
                  {
                     place_trap(x, y, p_ptr->mdepth, 15, 5);
                  }
                  break;
               }
               /* Slightly out of depth monster. -LM- */
               case '2':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  monster_level = p_ptr->mdepth + 3;
                  place_monster(x, y, TRUE, TRUE, TRUE);
                  monster_level = p_ptr->mdepth;
                  break;
               }
               /* Slightly out of depth object. -LM- */
               case '3':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, FALSE, FALSE, 0);
                  object_level = p_ptr->mdepth;
                  break;
               }
               /* Monster and/or object */
               case '4':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  if (rand_int(100) < 50)
                  {
                     monster_level = p_ptr->mdepth + 4;
                     place_monster(x, y, TRUE, TRUE, TRUE);
                     monster_level = p_ptr->mdepth;
                  }
                  if (rand_int(100) < 50)
                  {
                     object_level = p_ptr->mdepth + 4;
                     if (obj_boost) object_level += obj_boost;
                     place_object(x, y, FALSE, FALSE, 0);
                     object_level = p_ptr->mdepth;
                  }
                  break;
               }
               /* Out of depth object. -LM- */
               case '5':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  object_level = p_ptr->mdepth + 7;
                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, FALSE, FALSE, 0);
                  object_level = p_ptr->mdepth;
                  break;
               }
               /* Out of depth monster. -LM- */
               case '6':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  monster_level = p_ptr->mdepth + 7;
                  place_monster(x, y, TRUE, TRUE, TRUE);
                  monster_level = p_ptr->mdepth;
                  break;
               }
               /* Very out of depth object. -LM- */
               case '7':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  object_level = p_ptr->mdepth + 15;
                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, FALSE, FALSE, 0);
                  object_level = p_ptr->mdepth;
                  break;
               }
               /* Very out of depth monster. -LM- */
               case '8':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  monster_level = p_ptr->mdepth + 20;
                  place_monster(x, y, TRUE, TRUE, TRUE);
                  monster_level = p_ptr->mdepth;
                  break;
               }
               /* Meaner monster, plus "good" (or better) object */
               case '9':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  monster_level = p_ptr->mdepth + 15;
                  place_monster(x, y, TRUE, TRUE, TRUE);
                  monster_level = p_ptr->mdepth;
                  object_level = p_ptr->mdepth + 5;
                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, TRUE, FALSE, 0);
                  object_level = p_ptr->mdepth;
                  break;
               }

               /* Nasty monster and "great" (or better) object */
               case '0':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  monster_level = p_ptr->mdepth + 30;
                  place_monster(x, y, TRUE, TRUE, TRUE);
                  monster_level = p_ptr->mdepth;
                  object_level = p_ptr->mdepth + 15;
                  place_object(x, y, TRUE, TRUE, 0);
                  if (obj_boost) object_level += obj_boost;
                  object_level = p_ptr->mdepth;
                  break;
               }

               /* A chest. -LM- */
               case '~':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  required_tval = TV_CHEST;

                  object_level = p_ptr->mdepth + 5;
                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, FALSE, FALSE, TRUE);
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Treasure. -LM- */
               case '$':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  place_gold(x, y);
                  break;
               }
               /* Armour. -LM- */
               case ']':
               {

dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  object_level = p_ptr->mdepth + 3;

                  if (randint(3) == 1)
                  {
                     temp = randint(9);
                  }
                  else
                  {
                     temp = randint(8);
                  }

                  if (temp == 1) required_tval = TV_BOOTS;
                  else if (temp == 2) required_tval = TV_GLOVES;
                  else if (temp == 3) required_tval = TV_HELM;
                  else if (temp == 4) required_tval = TV_CROWN;
                  else if (temp == 5) required_tval = TV_SHIELD;
                  else if (temp == 6) required_tval = TV_CLOAK;
                  else if (temp == 7) required_tval = TV_SOFT_ARMOR;
                  else if (temp == 8) required_tval = TV_HARD_ARMOR;
                  else required_tval = TV_DRAG_ARMOR;

                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, TRUE, FALSE, TRUE);
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Weapon. -LM- */
               case '|':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  object_level = p_ptr->mdepth + 3;

                  temp = randint(3);

                  if (temp == 1) required_tval = TV_SWORD;
                  else if (temp == 2) required_tval = TV_POLEARM;
                  else if (temp == 3) required_tval = TV_HAFTED;

                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, TRUE, FALSE, TRUE);
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Ring. -LM- */
               case '=':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  required_tval = TV_RING;

                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  if (randint(4) == 1)
                  {
                     place_object(x, y, TRUE, FALSE, TRUE);
                  }
                  else
                  {
                     place_object(x, y, FALSE, FALSE, TRUE);
                  }
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Amulet. -LM- */
               case '"':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  required_tval = TV_AMULET;

                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  if (randint(4) == 1)
                  {
                     place_object(x, y, TRUE, FALSE, TRUE);
                  }
                  else
                  {
                     place_object(x, y, FALSE, FALSE, TRUE);
                  }
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Potion. -LM- */
               case '!':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  required_tval = TV_POTION;

                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  if (randint(4) == 1)
                  {
                     place_object(x, y, TRUE, FALSE, TRUE);
                  }
                  else
                  {
                     place_object(x, y, FALSE, FALSE, TRUE);
                  }
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Scroll. -LM- */
               case '?':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  required_tval = TV_SCROLL;

                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  if (randint(4) == 1)
                  {
                     place_object(x, y, TRUE, FALSE, TRUE);
                  }
                  else
                  {
                     place_object(x, y, FALSE, FALSE, TRUE);
                  }
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Staff. -LM- */
               case '_':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  required_tval = TV_STAFF;

                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  if (randint(4) == 1)
                  {
                     place_object(x, y, TRUE, FALSE, TRUE);
                  }
                  else
                  {
                     place_object(x, y, FALSE, FALSE, TRUE);
                  }
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Wand or rod. -LM- */
               case '-':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  if (rand_int(100) < 50) required_tval = TV_WAND;
                  else required_tval = TV_ROD;

                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  if (randint(4) == 1)
                  {
                     place_object(x, y, TRUE, FALSE, TRUE);
                  }
                  else
                  {
                     place_object(x, y, FALSE, FALSE, TRUE);
                   }
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
               /* Food or mushroom. -LM- */
               case ',':
               {
dlog(DEBUGGENER,"generate.c: build_vault: step2: char %c @ %d,%d\n", *t, x, y);
                  required_tval = TV_FOOD;

                  object_level = p_ptr->mdepth + 3;
                  if (obj_boost) object_level += obj_boost;
                  place_object(x, y, FALSE, FALSE, TRUE);
                  object_level = p_ptr->mdepth;

                  required_tval = 0;

                  break;
               }
            }
         }
      }
   }
   current_vault = 0;
   get_mon_num_hook = NULL;

   /* Prepare allocation table */
   get_mon_num_prep();
}

/*
 * Type 15 -- store
 */
static void build_type15(s16b xval, s16b yval, s16b *cent_dx, s16b *cent_dy)
{
   s16b x, y, x2, y2;
   s16b x1, y1;
   s16b store_type, store_identity;

   cave_cell_type *c_ptr;

   /* we reserved 1 11x10 block */
   /* we use x-5, y-5, x+5, y+5 */
   /* Pick a room size */
   x1 = xval - randint(3)-1;
   x2 = xval + randint(3)+1;
   y1 = yval - randint(3)-1;
   y2 = yval + randint(2)+1;

   store_type = randint(DUNG_ENTR_EXT_END-DUNG_ENTR_EXT_STRT)+DUNG_ENTR_EXT_STRT;
   store_identity = store_type;

dlog(DEBUGGENER,"generate.c: build_type15: type %d at %d,%d\n", store_type, xval, yval);
dlog(DEBUGGENER,"generate.c: build_type15: from %d,%d to %d,%d\n", x1-1, y1-1, x2+1, y2+1);

   /* Place a square store */
   for (y = y1 + 1; y <= y2 - 1; y++)
   {
      for (x = x1 + 1; x <= x2 - 1; x++)
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         (void)set_grid_type(x, y, DUNG_PERWALL, DUNG_PERWALL_SOLID,
                             GRID_REPLACE, CAVE_GLOW );
      }
   }
   /* now place a 1 square wide floor around it */
   for (y = y1 ; y <= y2 ; y++)
   {
      (void)set_grid_type(x1, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                          GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
      (void)set_grid_type(x2, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                          GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
   }
   for (x = x1 ; x <= x2 ; x++)
   {
      (void)set_grid_type(x, y1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
      (void)set_grid_type(x, y2, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
   }

   /* Walls around the floor around the store */
   for (y = y1 - 1; y <= y2 + 1; y++)
   {
      (void)set_grid_type(x1-1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, CAVE_GLOW );
      (void)set_grid_type(x2+1, y, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, CAVE_GLOW );
   }
   for (x = x1 - 1; x <= x2 + 1; x++)
   {
      (void)set_grid_type(x, y1-1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, CAVE_GLOW );
      (void)set_grid_type(x, y2+1, DUNG_WALL, DUNG_WALL_GRAOUTR,
                          GRID_REPLACE, CAVE_GLOW );
   }
   switch(randint(4))
   {
      case 1: /* built entrance to the north in the middle */
              /* this is a temple with a north entrance */
              /*                 xxxxx+xxxxxx */
              /*                 x          x */
              /*                 x xx? xxxx x */
              /*                 x xxx xxxx x */
              /*                 x xxx4xxxx x */
              /*                 x xxxxxxxx x */
              /*                 x xxxxxxxx x */
              /*                 x          x */
              /*                 xxxxxxxxxxxx */
              {
                 /* make entrance corridor */
                 (void)set_grid_type((x1+x2)/2, y1+1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 create_store(store_type, (x1+x2)/2, y1+2);
                 /* place symbol to the left of the start of the entrance corridor */
                 (void)set_grid_type((x1+x2)/2-1,y1+1, DUNG_SIGN, store_identity,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 /* make sure the center of the room is in front of the store */
                 (*cent_dx) = 0;
                 (*cent_dy) = ((y1+y2)/2)-y1;
dlog(DEBUGGENER,"generate.c: build_type15: cent_dx,dy %d,%d\n", *cent_dx, *cent_dy);
              }
              break;
           case 2: /* make east entrance */
              {
                 /* make entrance corridor */
                 (void)set_grid_type(x2-1, (y1+y2)/2, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 /* make entrance door */
                 create_store(store_type, x2-2, (y1+y2)/2);
                 (void)set_grid_type(x2-1, (y1+y2)/2 + 1, DUNG_SIGN, store_identity,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 /* make sure the center of the room is in front of the store */
                 (*cent_dx) = ((x1+x2)/2)-x2;
                 (*cent_dy) = 0;
              }
              break;
           case 3: /* make south entrance */
              {
                 /* make entrance corridor */
                 (void)set_grid_type((x1+x2)/2, y2-1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 /* make entrance door */
                 create_store(store_type, (x1+x2)/2, y2-1);
                 (void)set_grid_type((x1+x2)/2 + 1, y2-1, DUNG_SIGN, store_identity,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 (*cent_dx) = 0;
                 (*cent_dy) = ((y1+y2)/2)-y2;
              }
              break;
           case 4: /* make west entrance */
              {
                 /* make entrance corridor */
                 (void)set_grid_type(x1+1, (y1+y2)/2, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 /* make entrance door */
                 create_store(store_type, x1+1, (y1+y2)/2);
                 (void)set_grid_type(x1+1, (y1+y2)/2 + 1, DUNG_SIGN, store_identity,
                                     GRID_REPLACE, CAVE_ROOM | CAVE_GLOW );
                 (*cent_dx) = ((x1+x2)/2)-x1;
                 (*cent_dy) = 0;
              }
              break;
   } /* end switch randint(4) */
}

/*
 * this function tries to find a vault of a certain type
 * it also uses p_ptr->mdepth to find out if the vault is
 * allowed at this depth
 */
static s16b find_vault(s16b type)
{
   s16b         max_v, i;
   s16b         index[MAX_V_IDX];
   vault_type  *v_ptr = NULL;

   /* paranoia - do we have this kind of vault? */
   max_v = 0;
   for (i=0; i < v_number; i++)
   {
      v_ptr = &v_info[i];
dlog(DEBUGGENER2,"generate.c: find_vault: req type %d lev %d - found typ %d subl %d min_lev %d max_lev %d %s\n",
                type, p_ptr->mdepth, v_ptr->typ, v_ptr->sublevel, v_ptr->min_lev, v_ptr->max_lev,
                v_name + v_ptr->name);
      if ((v_ptr->typ == type) && (v_ptr->sublevel == 0) &&
          (v_ptr->min_lev <= p_ptr->mdepth) && (v_ptr->max_lev >= p_ptr->mdepth))
      {
         index[max_v++]=i;
      }
   }
   if (!max_v)
   {
      dlog(DEBUGEXTRA,"generate.c: find_vault: room type %d requested, not in v_info?\n", type);
      return(-1);
   }

   /* now choose a vault */
   i=index[rand_int(max_v)];
dlog(DEBUGGENER,"generate.c: find_vault: found %d\n", i);
   return i;
}

/*
 * this function tries to build a certain vault-type
 */
static bool build_type(s16b type, s16b xval, s16b yval, s16b *cent_dx, s16b *cent_dy)
{
   s16b         v_num;
   vault_type  *v_ptr = NULL;

   v_num = find_vault(type);

   if (v_num == -1) return (FALSE);

   v_ptr = &v_info[v_num];

dlog(DEBUGGENER,"generate.c: built_type: type %d name %s\n", type, v_name + v_ptr->name);
   /* Message */
   if (cheat_room) msg_format("%s",v_name + v_ptr->name);

   /* Boost the rating */
   rating += v_ptr->rat;

   /* (Sometimes) Cause a special feeling */
   /* but only if the vault has a positive rating */
   if ( (p_ptr->mdepth <= 50) ||
        (randint((p_ptr->mdepth-40) * (p_ptr->mdepth-40) + 1) < 400))
   {
      if (v_ptr->rat>0) good_item_flag = TRUE;
   }

   /* Hack -- Build the vault */
   build_vault(xval, yval, v_num, FALSE, cent_dx, cent_dy);
   return (TRUE);
}

/*
 * Constructs a tunnel between two points
 *
 * This function must be called BEFORE any streamers are created,
 * since we use the special "granite wall" sub-types to keep track
 * of legal places for corridors to pierce rooms.
 *
 * We use "door_flag" to prevent excessive construction of doors
 * along overlapping corridors.
 *
 * We queue the tunnel grids to prevent door creation along a corridor
 * which intersects itself.
 *
 * We queue the wall piercing grids to prevent a corridor from leaving
 * a room and then coming back in through the same entrance.
 *
 * We "pierce" grids which are "outer" walls of rooms, and when we
 * do so, we change all adjacent "outer" walls of rooms into "solid"
 * walls so that no two corridors may use adjacent grids for exits.
 *
 * The "solid" wall check prevents corridors from "chopping" the
 * corners of rooms off, as well as "silly" door placement, and
 * "excessively wide" room entrances.
 *
 * return TRUE if we connected row1,col1 to row2, col2
 */
static bool build_tunnel(s16b col1, s16b row1, s16b col2, s16b row2, bool termination)
{
   s16b       i, x, y;
   s16b       tmp_row, tmp_col;
   s16b       dx, dy;
   s16b       start_row, start_col;
   s16b       main_loop_count = 0;
   bool       door_flag = FALSE;
   cave_cell_type *c_ptr;

   /* Reset the arrays */
   dun->tunn_n = 0;
   dun->wall_n = 0;

   /* Save the starting location */
   start_row = row1;
   start_col = col1;
dlog(DEBUGGENER,"generate.c: build_tunnel: starting at %d,%d towards %d,%d\n",
            col1, row1, col2, row2);
   if (!in_bounds(col1, row1) || !in_bounds(col1, row1)) 
   {
      dlog(DEBUGALWAYS,"generate.c: build_tunnel: cannot build %d,%d to %d,%d - out of bounds\n",
                       col1, row1, col2, row2);
      return (FALSE);
   }


   c_ptr = &dungeon.level[sublevel][start_row][start_col];

   /* if we start in a wall, we started using an apostrophe (\') in a vault */
   /* make sure we start with a floor!                                      */
   if ((c_ptr->mtyp == DUNG_WALL) || (c_ptr->mtyp == DUNG_PERWALL))
   {
dlog(DEBUGGENER2,"generate.c: build_tunnel: starting in wall @ %d,%d\n", start_col, start_row);

      /* Save the tunnel location */
      if (dun->tunn_n < TUNN_MAX)
      {
         dun->tunn[dun->tunn_n].y = start_row;
         dun->tunn[dun->tunn_n].x = start_col;
         dun->tunn_n++;
      }
   }

   /* Start out in the correct direction */
   correct_dir(&dx, &dy, col1, row1, col2, row2);
dlog(DEBUGGENER2,"generate.c: build_tunnel: correct_dir dx,dy %d,%d col1,row1 %d,%d col2,row2 %d,%d\n",
                dx, dy, col1, row1, col2, row2);

   /* Keep going until done (or bored) */
   while ((row1 != row2) || (col1 != col2))
   {
      /* Mega-Hack -- Paranoia -- prevent infinite loops */
      /* jk - this count was 2000 */
      /* if we have tried 250 times and haven't gotten anywhere yet, stop it */
      main_loop_count++;
      if ((distance(start_col, start_row, col1, row1) < 15) && (main_loop_count > 250))
      {
dlog(DEBUGGENER,"generate.c: build_tunnel: not making enough progress in 250 turns\n");
         break;
      }
      /* if we have tried more than enough and not finished, break */
      if (main_loop_count > 2000)
      {
dlog(DEBUGGENER,"generate.c: build_tunnel: not making enough progress in 2000 turns\n");
         break;
      }

dlog(DEBUGGENER2,"generate.c: build_tunnel: main loop count %d (dx,dy %d,%d pointing toward %d,%d)\n",
                main_loop_count, dx, dy, col1+dx, row1+dy);

      /* Allow bends in the tunnel */
      if (rand_int(100) < DUN_TUN_CHG)
      {
         /* Acquire the correct direction */
         correct_dir(&dx, &dy, col1, row1, col2, row2);

         /* Random direction */
         if (rand_int(100) < DUN_TUN_RND)
         {
            rand_dir(&dx, &dy);
dlog(DEBUGGENER2,"generate.c: build_tunnel: bending random to %d,%d\n",
                dx, dy);
         }
      }

      /* Get the next location */
      tmp_col = col1 + dx;
      tmp_row = row1 + dy;

      /* Extremely Important -- do not leave the dungeon */
      while (!in_bounds(tmp_col, tmp_row))
      {
         /* Acquire the correct direction */
         correct_dir(&dx, &dy, col1, row1, col2, row2);

         /* Random direction */
         if (rand_int(100) < DUN_TUN_RND)
         {
            rand_dir(&dx, &dy);
         }

         /* Get the next location */
         tmp_col = col1 + dx;
         tmp_row = row1 + dy;
      }

      /* Access the location */
      c_ptr = &dungeon.level[sublevel][tmp_row][tmp_col];

dlog(DEBUGGENER2,"generate.c: build_tunnel: testing %d,%d (mt %d st %d %s)\n",
                tmp_col, tmp_row, c_ptr->mtyp, c_ptr->styp, f_name + f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name);
      /* Avoid the edge of the dungeon */
      if ((c_ptr->mtyp == DUNG_PERWALL) &&
          (c_ptr->styp == DUNG_PERWALL_SOLID)) continue;

      /* Avoid the edge of vaults */
      if ((c_ptr->mtyp == DUNG_PERWALL) &&
          (c_ptr->styp == DUNG_PERWALL_OUTER)) continue;

      /* Avoid "solid" granite walls */
      if ((c_ptr->mtyp == DUNG_WALL) &&
          (c_ptr->styp == DUNG_WALL_GRSOLID)) continue;

      /* Avoid secret doors, which give an entrance to certain vaults */
      if ((c_ptr->mtyp == DUNG_DOOR) &&
          (c_ptr->styp == DUNG_DOOR_SECRET)) continue;

      /* don't tunnel inside a maze */
      if (c_ptr->fdat & CAVE_MAZE) continue;

      /* don't pierce inner walls (checkerboards in rooms etc) */
      if (inner_wall(c_ptr->mtyp, c_ptr->styp)) continue;

      /* Pierce "outer" walls of rooms */
      if ((c_ptr->mtyp == DUNG_WALL) &&
          (c_ptr->styp == DUNG_WALL_GRAOUTR))
      {

dlog(DEBUGGENER2,"generate.c: build_tunnel: graoutr wall found at %d,%d\n",
           tmp_col, tmp_row);

         /* Acquire the "next" location */
         x = tmp_col + dx;
         y = tmp_row + dy;

         /* Hack -- Avoid if the next square is outer/solid permanent walls */
         if (test_grid(x,y,DUNG_PERWALL,DUNG_PERWALL_SOLID)) continue;
         if (test_grid(x,y,DUNG_PERWALL,DUNG_PERWALL_OUTER)) continue;

         /* Hack -- Avoid if the next square is outer/solid granite walls */
         if (test_grid(x,y,DUNG_WALL,DUNG_WALL_GRSOLID)) continue;

         /* Accept this location */
         row1 = tmp_row;
         col1 = tmp_col;
dlog(DEBUGGENER2,"generate.c: build_tunnel: %d,%d accepted, dun->wall_n %d max %d\n",
                col1, row1, dun->wall_n, WALL_MAX);

         /* Save the wall location */
         if (dun->wall_n < WALL_MAX)
         {
             dun->wall[dun->wall_n].y = row1;
             dun->wall[dun->wall_n].x = col1;
             dun->wall_n++;
         }

         /* Forbid re-entry near this piercing */
         for (y = row1 - 1; y <= row1 + 1; y++)
         {
            for (x = col1 - 1; x <= col1 + 1; x++)
            {
               /* Convert adjacent "outer" walls as "solid" walls */
               if (test_grid(x,y,DUNG_WALL,DUNG_WALL_GRAOUTR))
               {
                  /* Change the wall to a "solid" wall */
                  /* if we use GRID_KEEP here, lighting will be wrong */
dlog(DEBUGGENER2,"generate.c: build_tunnel: setting %d,%d to grsolid wall fdat was %08lx\n",
           x, y, c_ptr->fdat);
                  (void)set_grid_type(x, y, DUNG_WALL, DUNG_WALL_GRSOLID,
                                      GRID_ADD, 0);
dlog(DEBUGGENER2,"generate.c: build_tunnel: setting %d,%d to grsolid wall fdat now %08lx\n",
           x, y, c_ptr->fdat);

               }
            }
         }
      }

      /* Travel quickly through rooms */
      else if (c_ptr->fdat & CAVE_ROOM)
      {
dlog(DEBUGGENER2,"generate.c: build_tunnel: in room @ %d,%d\n", tmp_col, tmp_row);
         /* Accept the location */
         row1 = tmp_row;
         col1 = tmp_col;

         /*
          * If we are in a room but in a tunnel, make sure we excavate that
          * square also!
          */
         if (is_wall_ptr(c_ptr) && (dun->tunn_n < TUNN_MAX))
         {
            dun->tunn[dun->tunn_n].y = row1;
            dun->tunn[dun->tunn_n].x = col1;
            dun->tunn_n++;
         }

dlog(DEBUGGENER2,"generate.c: build_tunnel: %d,%d accepted\n", col1, row1);
      }

      /* Tunnel through all other walls */
      else if ((c_ptr->mtyp == DUNG_WALL) || (c_ptr->mtyp == DUNG_PERWALL))
      {
dlog(DEBUGGENER2,"generate.c: build_tunnel: other wall @ %d,%d\n", tmp_col, tmp_row);
         /* Accept this location */
         row1 = tmp_row;
         col1 = tmp_col;
dlog(DEBUGGENER2,"generate.c: build_tunnel: %d,%d accepted\n", col1, row1);

         /* Save the tunnel location */
         if (dun->tunn_n < TUNN_MAX)
         {
            dun->tunn[dun->tunn_n].y = row1;
            dun->tunn[dun->tunn_n].x = col1;
            dun->tunn_n++;
         }

         /* Allow door in next grid */
         door_flag = FALSE;
      }

      /* Handle corridor intersections or overlaps */
      else
      {
dlog(DEBUGGENER2,"generate.c: build_tunnel: other something @ %d,%d\n", tmp_col, tmp_row);
         /* Accept the location */
         row1 = tmp_row;
         col1 = tmp_col;
dlog(DEBUGGENER2,"generate.c: build_tunnel: %d,%d accepted\n", col1, row1);

         /* Collect legal door locations */
         if (!door_flag)
         {
            /* Save the door location */
            if (dun->door_n < DOOR_MAX)
            {
               dun->door[dun->door_n].y = row1;
               dun->door[dun->door_n].x = col1;
               dun->door_n++;
            }

            /* No door in next grid */
            door_flag = TRUE;
         }

         /* Hack -- allow pre-emptive tunnel termination */
         if ((rand_int(100) >= DUN_TUN_CON) && (termination))
         {
            /* Distance between row1 and start_row */
            tmp_row = row1 - start_row;
            if (tmp_row < 0) tmp_row = (-tmp_row);

            /* Distance between col1 and start_col */
            tmp_col = col1 - start_col;
            if (tmp_col < 0) tmp_col = (-tmp_col);

            /* Terminate the tunnel */
            if ((tmp_row > 10) || (tmp_col > 10))
            {
dlog(DEBUGGENER,"generate.c: build_tunnel: preemtive termination @ %d,%d\n", col1, row1);
               break;
            }
         }
      }
   }

   /* Turn the tunnel into corridor */
   for (i = 0; i < dun->tunn_n; i++)
   {
      /* Access the grid */
      y = dun->tunn[i].y;
      x = dun->tunn[i].x;

      /* Access the grid */
      c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGGENER2,"generate.c: build_tunnel: turning tunnel in corridor %d of %d @ %d,%d\n", i, dun->tunn_n, x, y);
      /* don't destroy pillars and other interior features in rooms! */
      if (inner_wall(c_ptr->mtyp, c_ptr->styp)) continue;

      /* Clear previous contents, add a floor */
      /* GRID_KEEP keeps the CAVE_GLOW flag if any */
      (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_ADD, 0);
      if (rand_int(100) < DUN_TUN_TRAP)
      {
         place_trap(x, y, p_ptr->mdepth, (40 - p_ptr->mdepth), (40 - p_ptr->mdepth));
      }
   }

   /* Apply the piercings that we found */
   for (i = 0; i < dun->wall_n; i++)
   {
      u32b keep_flags = 0L;
      /* Access the grid */
      x = dun->wall[i].x;
      y = dun->wall[i].y;

      /* Access the grid */
      c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGGENER2,"generate.c: build_tunnel: applying piercing %d of %d @ %d,%d\n", i, dun->wall_n, x, y);

      /* don't destroy pillars and other interior features in rooms! */
      if (inner_wall(c_ptr->mtyp, c_ptr->styp)) continue;

      /* Clear previous contents, add up floor */
      /* GRID_REPLACE replaces the CAVE_ROOM, and CAVE_GLOW is one to keep also */
      keep_flags=(c_ptr->fdat & (CAVE_GLOW | CAVE_ROOM));
      (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_REPLACE, keep_flags);

      /* Occasional doorway */
      if (rand_int(100) < DUN_TUN_PEN)
      {
         /* Place a random door */
         place_random_door(x, y);
      }
   }
   if ((row1 == row2) && (col1 == col2))
   {
      return (TRUE);
   }
   else
   {
      return (FALSE);
   }
}

/*
 * this helper function tests if a cell should be included in the current flood-fill
 */
static bool test_fill(s16b x, s16b y)
{
   if (!in_bounds(x, y))
   {
      return (FALSE);
   }
   if (dungeon.level[sublevel][y][x].cost != 0)
   {
       return (FALSE);
   }
   if (really_is_wall(x, y))
   {
      return (FALSE);
   }
   return (TRUE);
}

/*
 * this recursively fills the whole dungeon with value in the .cost field
 */
static void flood_fill(s16b x, s16b y, byte value)
{
   dungeon.level[sublevel][y][x].cost = value;
   if (test_fill(x-1, y-1))   flood_fill(x-1, y-1, value);
   if (test_fill(x-1, y))     flood_fill(x-1, y,   value);
   if (test_fill(x-1, y+1))   flood_fill(x-1, y+1, value);
   if (test_fill(x,   y-1))   flood_fill(x,   y-1, value);
   if (test_fill(x,   y+1))   flood_fill(x,   y+1, value);
   if (test_fill(x+1, y-1))   flood_fill(x+1, y-1, value);
   if (test_fill(x+1, y))     flood_fill(x+1, y,   value);
   if (test_fill(x+1, y+1))   flood_fill(x+1, y+1, value);
   return;
}

/*
 * this function checks if there are any unconnected rooms in the dungeon
 * using cost, which is not needed during generation, and zeroed afterwards.
 *
 * in case something goes wrong, it always starts at a different offset.
 *
 * return TRUE if we did connect something.
 */
static bool test_all_connected()
{
   s16b x = 0, y = 0, mindist, i=0, ox=0, oy=0, sx = 0, sy = 0;
   s16b x1best = 0, y1best = 0, x2best = 0, y2best = 0;
   s16b area1x = 0, area1y = 0, area2x = 0, area2y = 0;
   bool found = FALSE;

   /* vaults should look out for themselves */
   if (sublevel != 0) return (FALSE);

   /* clean up the cost entry for the entire dungeon */
   for (y=0; y < cur_hgt; y++)
   {
      for (x=0; x < cur_wid; x++)
      {
         dungeon.level[sublevel][y][x].cost = 0;
      }
   }

dlog(DEBUGGENER,"generate.c: test_all_connected: starting\n");
   /* now we search for a starting point, a piece of floor in the dungeon */
   for (sy=0; sy < cur_hgt; sy++)
   {
      for (sx=0; sx < cur_wid; sx++)
      {

         x=(sx+ox) % cur_wid;
         y=(sy+oy) % cur_hgt;

         if ( !really_is_wall(x, y))
         {
            found = TRUE;
            break;
         }
      }
      if ( found==TRUE )
      {
         break;
      }
   }
   /* there should be 1 piece of floor in the dungeon */
   if ( (sx==cur_wid) && (sy==cur_hgt) )
   {
      quit("generate.c: test_all_connected: no starting cell found?");
   }
dlog(DEBUGGENER,"generate.c: test_all_connected: found first floor @ %d,%d\n", x, y);
   area1x = x;
   area1y = y;

   /* we start here */
   flood_fill(x, y, 1);

   /* now find a piece of dungeon that is a floor, outside a maze, and has cost != 1 */
   /* that piece is unconnected!                                                     */
   found = FALSE;
   for (sy=0; sy < cur_hgt; sy++)
   {
      for (sx=0; sx < cur_wid; sx++)
      {
         x=(sx+ox) % cur_wid;
         y=(sy+oy) % cur_hgt;

         if ( !really_is_wall(x, y))
         {
            if (dungeon.level[sublevel][y][x].cost != 1)
            {
               s16b mtyp;
               s16b styp;

               mtyp = dungeon.level[sublevel][y][x].mtyp;
               styp = dungeon.level[sublevel][y][x].styp;

dlog(DEBUGGENER,"generate.c: test_all_connected: cost !=1 @ %d,%d (%s) (%d,%d) - really_is_wall %d\n",
                x, y, f_name+f_info[get_f_idx(mtyp, styp)].name, mtyp, styp, really_is_wall(x, y) );
               found = TRUE;
               break;
            }
         }
      }
      if (found == TRUE)
      {
         break;
      }
   }
dlog(DEBUGGENER,"generate.c: test_all_connected: after fill x,y %d,%d\n", x, y);

   /* we didn't find any floors with cost != 1, so everything is connected! */
   if ( (sx==cur_wid) && (sy==cur_hgt) )
   {
      /* clean up the cost entry */
      for (y=0; y < cur_hgt; y++)
      {
         for (x=0; x < cur_wid; x++)
         {
            dungeon.level[sublevel][y][x].cost = 0;
         }
      }

      return (FALSE);
   }
   area2x = x;
   area2y = y;

   /* we did find an unconnected part, in x, y */
   flood_fill(x, y, 2);
   /* now we want to make a short tunnel between area 1 and area 2 */
   /* problem: where is a short tunnel? */
   /* solution: we only test some random points */
   mindist=distance(0, 0, MAX_WID, MAX_HGT) + 10;

   /* test for random distances between the known point in area 1 and random points in area 2 */
   x1best = area1x;
   y1best = area1y;
   x2best = area2x;
   y2best = area2y;
   for (i=0; i < 100; i++)
   {
      x = randint(cur_wid-1);
      y = randint(cur_hgt-1);
      if (dungeon.level[sublevel][y][x].cost != 2) continue;
      if (distance(x, y, area1x, area1y)<mindist)
      {
         x1best = x;
         y1best = y;
         x2best = area1x;
         y2best = area1y;
         mindist=distance(x, y, area1x, area1y);
      }
   }
   /* test for random distances between random points in area 1 and the known point in area 2 */
   for (i=0; i < 100; i++)
   {
      x = randint(cur_wid-1);
      y = randint(cur_hgt-1);
      if (dungeon.level[sublevel][y][x].cost != 1) continue;
      if (distance(x, y, area2x, area2y)<mindist)
      {
         x1best = x;
         y1best = y;
         x2best = area2x;
         y2best = area2y;
         mindist=distance(x, y, area2x, area2y);
      }
   }

dlog(DEBUGGENER,"generate.c: test_all_connected: tunnel: from %d,%d to %d,%d dist %d\n",
                x1best, y1best, x2best, y2best, mindist);

   /* build a tunnel to connect them. Try it maximum 4 times, from both ends */
   for (i=0; i < 4; i++)
   {
      if ((i%2) == 1)
      {
         if (build_tunnel(x1best, y1best, x2best, y2best, FALSE)) break;
      }
      else
      {
         if (build_tunnel(x2best, y2best, x1best, y1best, FALSE)) break;
      }
   }

   return (TRUE);
}


/*
 * now prevent situations where doors in to round rooms can be circum-
 * vented by diagonal moves
 *       #         we have to build another wall at X here
 * room X######
 *      +   tunnel
 *      #######
 *      #
 *      #
 *
 */
static void check_doors(void)
{
   s16b x, y;

   for (x=1; x < cur_wid-1; x++)
   {
      for (y=1; y < cur_hgt-1; y++)
      {
         if (dungeon.level[sublevel][y][x].mtyp!=DUNG_DOOR) continue;

         /* door from left to right */
         if (!is_wall(x-1, y) && !is_wall(x+1, y))
         {
            if (!is_wall(x, y-1) && naked_grid_bold(x, y-1)) /* no upper wall */
            {
               (void)set_grid_type(x, y-1, DUNG_WALL,
                                   DUNG_WALL_GRSOLID, GRID_KEEP, 0);
dlog(DEBUGGENER,"generate.c: check_doors: left-right door @ %d,%d wall added @ %d,%d\n",
                 x, y, x, y-1);
            }
            if (!is_wall(x, y+1) && !is_wall(x, y+1)) /* no lower wall */
            {
               (void)set_grid_type(x, y+1, DUNG_WALL,
                                   DUNG_WALL_GRSOLID, GRID_KEEP, 0);
dlog(DEBUGGENER,"generate.c: check_doors: left-right door @ %d,%d wall added @ %d,%d\n",
                 x, y, x, y+1);
            }
         }
         /* door from up to down */
         else if (!is_wall(x, y-1) && !is_wall(x, y+1))
         {
            if (!is_wall(x-1, y) && !is_wall(x-1, y)) /* no left wall */
            {
               (void)set_grid_type(x-1, y, DUNG_WALL,
                                   DUNG_WALL_GRSOLID, GRID_KEEP, 0);
dlog(DEBUGGENER,"generate.c: check_doors: up-down door @ %d,%d wall added @ %d,%d\n",
                 x, y, x-1, y);
            }
            if (!is_wall(x+1, y) && !is_wall(x+1, y)) /* no left wall */
            {
               (void)set_grid_type(x+1, y, DUNG_WALL,
                                   DUNG_WALL_GRSOLID, GRID_KEEP, 0);
dlog(DEBUGGENER,"generate.c: check_doors: up-down door @ %d,%d wall added @ %d,%d\n",
                 x, y, x+1, y);
            }
         }
         else /* door in corner: remove it */
         {
            cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGGENER,"generate.c: check_doors: weird door @ %d,%d removed, fdat was %08lx\n",
                 x, y, c_ptr->fdat);
            (void)set_grid_type(x, y, DUNG_FLOOR,
                                DUNG_FLOOR_NORMAL, GRID_KEEP, 0);

dlog(DEBUGGENER,"generate.c: check_doors: weird door @ %d,%d removed, fdat now %08lx\n",
                 x, y, c_ptr->fdat);

            /* any traps present? */
            if (c_ptr->t_idx)
            {
               trap_item_type  *tr_ptr;

               tr_ptr = &t_list[c_ptr->t_idx];
               /* free trap entry */
               tr_ptr->inuse = FALSE;
               /* untrap this floor */
               c_ptr->t_idx = 0;
            }
         }
      }
   }
}
static bool weird_passage_wall(s16b x1, s16b y1, s16b x2, s16b y2)
{
   cave_cell_type *c_ptr;
 
   if (!is_wall(x1, y1)) return FALSE;
   if (!is_wall(x2, y2)) return FALSE;
   c_ptr = &dungeon.level[sublevel][y1][x1];  
   if (inner_wall(c_ptr->mtyp, c_ptr->styp)) return FALSE;
   c_ptr = &dungeon.level[sublevel][y2][x2];  
   if (inner_wall(c_ptr->mtyp, c_ptr->styp)) return FALSE;
   return TRUE;
}

/*
 * we here test for things like     # #
 *                                ### #
 * X are walls now, but one of        X#####
 * them should be deleted         ###X
 *                                   # #####
 *                                   # #
 *
 *
 */
static void check_weird_passages(void)
{
   s16b x, y;

   for (x=1; x < cur_wid-1; x++)
   {
      for (y=1; y < cur_hgt-1; y++)
      {
         /*
          *   #
          *    #
          */
         if (weird_passage_wall(x, y, x+1, y+1))
         {
            if (!is_wall(x, y+1) && !is_wall(x+1,y))
            {
               if (randint(100)<50)
               {
                  (void)set_grid_type(x, y, DUNG_FLOOR,
                                      DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
               }
               else
               {
                  (void)set_grid_type(x+1, y+1, DUNG_FLOOR,
                                      DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
               }
            }
         }
         /*
          *    #
          *   #
          */
         else if (weird_passage_wall(x+1, y, x, y+1))
         {
            if (!is_wall(x, y) && !is_wall(x+1,y+1))
            {
               if (randint(100)<50)
               {
                  (void)set_grid_type(x+1, y, DUNG_FLOOR,
                                      DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
               }
               else
               {
                  (void)set_grid_type(x, y+1, DUNG_FLOOR,
                                      DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
               }
            }
         }
      }
   }
}

/*
 * Count the number of "corridor" grids adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds(x1, y1)"
 *
 * XXX XXX This routine currently only counts actual "empty floor"
 * grids which are not in rooms.  We might want to also count stairs,
 * open doors, closed doors, etc.
 */
static s16b next_to_corr(s16b x1, s16b y1)
{
   s16b i, y, x, k = 0;

   cave_cell_type *c_ptr;

   /* Scan adjacent grids */
   for (i = 0; i < 4; i++)
   {
      /* Extract the location */
      x = x1 + ddx_ddd[i];
      y = y1 + ddy_ddd[i];

      /* Skip non floors */
      if (!floor_grid_bold(x, y)) continue;

      /* Access the grid */
      c_ptr = &dungeon.level[sublevel][y][x];

      /* Skip non "empty floor" grids */
      if (c_ptr->mtyp != DUNG_FLOOR) continue;

      /* Skip grids inside rooms */
      if (c_ptr->fdat & CAVE_ROOM) continue;

      /* Count these grids */
      k++;
   }

   /* Return the number of corridors */
   return (k);
}

/*
 * Determine if the given location is "between" two walls,
 * and "next to" two corridor spaces.
 *
 * Assumes "in_bounds(x,y)"
 */
static bool possible_doorway(s16b x, s16b y)
{
   /* Count the adjacent corridors */
   if (next_to_corr(x, y) >= 2)
   {
      /* Check Vertical */
      if (is_wall(x, y-1) && is_wall(x, y+1)) return(TRUE);

      /* Check Horizontal */
      if (is_wall(x-1, y) && is_wall(x+1, y)) return(TRUE);
   }

   /* No doorway */
   return (FALSE);
}

/*
 * Places door at y, x position if at least 2 walls found
 */
static void try_door(s16b x, s16b y)
{
   /* Paranoia */
   if (!in_bounds(x, y)) return;

   /* Ignore walls */
   if (is_wall(x, y)) return;

   /* Ignore room grids */
   if (dungeon.level[sublevel][y][x].fdat & CAVE_ROOM) return;

   /* Occasional door (if allowed) */
   if ((rand_int(100) < DUN_TUN_JCT) && possible_doorway(x, y))
   {
      /* Place a door */
      place_random_door(x, y);
   }
}

/*
 * Attempt to build a room of the given type at the given block
 *
 * Note that we restrict the number of "crowded" rooms to reduce
 * the chance of overflowing the monster list during level creation.
 */
static bool room_build(s16b x0, s16b y0, s16b typ, bool level_overlap)
{
   s16b x, y, x1, y1, x2, y2, cent_dx, cent_dy;
   bool ok;

dlog(DEBUGGENER,"generate.c: room_build: trying typ %d @ block %d,%d (real %d,%d)\n",
                typ, x0, y0, x0*BLOCK_WID, y0*BLOCK_HGT);

   /* Restrict level */
   if (p_ptr->mdepth < room[typ].level) return (FALSE);

   /* we try another room of type 'typ' against the maximum*/
   if (rooms_built[typ]>=roomn_max[typ][0])
   {
      s16b toomany = rooms_built[typ]-roomn_max[typ][0];
      s16b i = 0;
      /* this means if we want max 2 and have 3, the chance for the 4th */
      /* succeeding is the chance in roomn_max[typ][1] squared!          */
      while ((i<=toomany) && (rand_int(100)<roomn_max[typ][1]))
      {
         i++;
      }
      if (i<toomany) return (FALSE); /* room can't be built */
   }
   /* Restrict "crowded" rooms */
/* jk - now bigger levels (with bigger ratios) get more crowded rooms */
   if (dun->crowded &&
      ((typ == 5) || (typ == 6)) &&
      (rand_int(RATIO)>(RATIO-3))) return (FALSE);

   /* Extract blocks */
   x1 = x0 + room[typ].dx1;
   y1 = y0 + room[typ].dy1;
   x2 = x0 + room[typ].dx2;
   y2 = y0 + room[typ].dy2;

   /* Never run off the screen */
   if ((x1 < 0) || (x2 >= dun->col_rooms)) return (FALSE);
   if ((y1 < 0) || (y2 >= dun->row_rooms)) return (FALSE);

   /*
    * most of the time, we don't build rooms in the dungeon which might overlap
    * might means that the maximum dimensions are used.
    *
    * in a small number of cases, we allow this to happen. This leads to bigger
    * rooms in irregular shapes
    */
   /* Verify open space */
   for (y = y1; y <= y2; y++)
   {
      for (x = x1; x <= x2; x++)
      {
         if (dun->room_map[x][y]>0)
         {
dlog(DEBUGGENER,"generate.c: room_build: overlap found; block %d,%d already used\n", x, y);
            if ( ( (y==y1) || (x==x1) || (y==y2) || (x==x2) ) &&
                ( room[typ].normal == TRUE))
            {
dlog(DEBUGGENER,"generate.c: room_build: overlap at edge found; block %d,%d already used\n", x, y);
               if (level_overlap && (dun->room_map[x][y]<2) && (randint(OVERLAP_CHANCE) == 0))
               {
dlog(DEBUGGENER,"generate.c: room_build: overlap at edge OK; level_overlap true\n");
                  continue;
               }
               if ((randint(20)==1) && (dun->room_map[x][y]<2))
               {
dlog(DEBUGGENER,"generate.c: room_build: overlap at edge OK; random chance.\n");
                  continue;
               }
dlog(DEBUGGENER,"generate.c: room_build: overlap at edge not OK.\n");
               return (FALSE);
            }
            else
            {
               return (FALSE);
            }
         }
      }
   }

   /* XXX XXX XXX It is *extremely* important that the following */
   /* calculation is *exactly* correct to prevent memory errors */

   /* Acquire the location of the room */
   x = ((x1 + x2 + 1) * BLOCK_WID) / 2;
   y = ((y1 + y2 + 1) * BLOCK_HGT) / 2;

   rooms_built[typ]++;
   cent_dx = 0;
   cent_dy = 0;

dlog(DEBUGGENER,"generate.c: room_build: dungeon blocks 0,0 to %d,%d real 0,0 to %d,%d\n",
                 dun->col_rooms, dun->row_rooms, cur_wid, cur_hgt);
dlog(DEBUGGENER,"generate.c: room_build: typ %d @ %d,%d space needed blocks %d,%d to %d,%d\n",
                typ, x, y, x1, y1, x2, y2);
   ok = TRUE;

   /* Build a room */
   switch (typ)
   {
      case 18: build_type18(x, y);
               break;
      /* Build an appropriate room */
      case 16: build_type16(x, y); break;
      /* first the vaults, treasure chambers and graveyards */
      case 15: build_type15(x, y, &cent_dx, &cent_dy); break;

      case 17:
      case 14:
      case 13:
      case 12:
      case 11:
      case 10:
      case 9 :
      case 8 :
      case 7 : ok = build_type(typ, x, y, &cent_dx, &cent_dy); break;

      case 6 : build_type6(x, y); break;
      case 5 : build_type5(x, y); break;
      case 4 : build_type4(x, y); break;
      case 3 : build_type3(x, y); break;
      case 2 : build_type2(x, y); break;
      case 1 : build_type1(x, y); break;

      /* Paranoia */
      default: return (FALSE);
   }

   /* something went wrong during the building of this room? */
   if (!ok) return (FALSE);

   /* Save the room location */
   if (dun->cent_n < CENT_MAX)
   {
dlog(DEBUGGENER,"generate.c: room_build: storing room in dun_cent item %d @ %d,%d\n",
   dun->cent_n, x, y);
      dun->cent[dun->cent_n].x = x+cent_dx;
      dun->cent[dun->cent_n].y = y+cent_dy;
      dun->cent_n++;
   }

   /* Reserve some blocks */
   for (y = y1; y <= y2; y++)
   {
      for (x = x1; x <= x2; x++)
      {
         if (room[typ].normal)
         {
            dun->room_map[x][y] = 1;
         }
         else
         {
            dun->room_map[x][y] = 2;
         }
      }
   }

   /* Count "crowded" rooms */
   if ((typ == 5) || (typ == 6)) dun->crowded = TRUE;

   /* Success */
   return (TRUE);
}

/* jk - try to expand some tunnels to a wider tunnel */
static void expand_tunnel(void)
{
   s16b        dx = 1-2*rand_int(2);
   s16b        dy = 1-2*rand_int(2);
   s16b        nx,ny;
   cave_cell_type  *c_ptr;
   s16b        i = 0;

   while (i<dun->tunn_n)
   {
      nx = dun->tunn[i].x+dx;
      ny = dun->tunn[i].y+dy;

      i++; /* get to the next grid */

      c_ptr = &dungeon.level[sublevel][ny][nx];
      if (test_grid_ptr(c_ptr,DUNG_PERWALL,DUNG_PERWALL_SOLID))
      {
         continue; /* Avoid the edge of the dungeon */
      }
      if (test_grid_ptr(c_ptr,DUNG_PERWALL,DUNG_PERWALL_OUTER))
      {
         continue; /* Avoid the edge of vaults */
      }
      if (test_grid_ptr(c_ptr,DUNG_WALL,DUNG_WALL_GRSOLID))
      {
         continue; /* Avoid "solid" granite walls */
      }
      if (test_grid_ptr(c_ptr,DUNG_WALL,DUNG_WALL_GRAOUTR))
      {
         continue; /* Pierce "outer" walls of rooms */
      }
      if (test_grid_ptr(c_ptr,DUNG_PERWALL,DUNG_PERWALL_NORMAL))
      {
         continue; /* Avoid other permanent walls */
      }
      if (c_ptr->mtyp==DUNG_DOOR)
      {
          continue; /* doors */
      }
      /* we have found an acceptable grid */
      if (c_ptr->fdat & CAVE_GLOW)
      {
         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_REPLACE, CAVE_GLOW);
      }
      else
      {
         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_REPLACE, 0);
      }
      if (rand_int(1000)<CHANCE_TRAP_FLOOR)
      {
         place_trap(nx, ny, p_ptr->mdepth, 10, 2);
      }
   }
}

/*
 * this routine builds a large maze,
 * it is based on (make that copied from!)
 * a maze-generator by jleonard@slimy.com.
 * he was kind enough to give permission to use this code.
 */
#define MAX_MAZE (MAX_WID * MAX_HGT)
char maze_hedges[MAX_MAZE];
char maze_vedges[MAX_MAZE];

/*
 * Each location in the maze points to some other location connected to it.
 * Because how this is constructed, these interrelations form a tree, and
 * you can check to see if two nodes are connected by going up to the roots,
 * and seeing if the roots are the same.
 *
 * This function adds one tree to the other if they're not connected.
 * As a side effect, it flattens the tree a bit for efficiency.
 */

s16b maze_do_set(s16b locations[MAX_MAZE], s16b loc1, s16b loc2)
{
   s16b temp;

   /* chase loc1 down to the root */
   while (locations[loc1] != loc1)
   {
      temp = loc1;
      loc1 = locations[loc1];
      locations[temp] = locations[loc1];  /* flatten */
   }
   /* chase loc2 down to the root */
   while (locations[loc2] != loc2)
   {
      temp = loc2;
      loc2 = locations[loc2];
      locations[temp] = locations[loc2];  /* flatten */
   }
   /* are they connected somehow? */
   if (loc1 == loc2)
   {
      /* Yup, they were connected */
      return(1);
   } else {
      /* connect them */
      locations[loc2] = loc1;
      /* and say that they weren't connected before */
      return(0);
   }
}

/* Scramble for a new game */
void maze_randomize(s16b x_count, s16b y_count)
{
   s16b x, y;
   s16b squares[MAX_MAZE];     /* the maze data */
   s16b hedgelist[MAX_MAZE];   /* h edges left to check */
   s16b vedgelist[MAX_MAZE];   /* v edges left to check */
   s16b hedgecount;            /* h edges count */
   s16b vedgecount;            /* v edges count */
   s16b index, curedge;

   /* set up the variables */
   hedgecount = 0;   /* haven't checked any horizontal edges yet */
   vedgecount = 0;   /* haven't checked any vertical edges yet */
   /* Initialize arrays of all horizontal edges left to check */
   for (x=0; x<x_count; x++)
   {
      for (y=1; y<y_count; y++)
      {
         hedgelist[hedgecount++] = x*y_count+y;
      }
   }

   /* Initialize arrays of all vertical edges left to check */
   for (x=1; x<x_count; x++)
   {
      for (y=0; y<y_count; y++)
      {
         vedgelist[vedgecount++] = x*y_count+y;
      }
   }

   /* label squares by what they're connected to: just self */
   for (x=0; x<x_count; x++)
   {
      for (y=0; y<y_count; y++)
      {
         squares[x*y_count+y] = x*y_count+y;
      }
   }

   /* and the interesting loop -- punch holes as necessary */
   while ((hedgecount > 0) || (vedgecount > 0)) /* all edges checked? */
   {
      /* do a horizontal edge if ... */
      if ( (vedgecount == 0) ||   /* that's all that's left */
           ( (rand_int(2)==1) && /* or 50/50 chance and */
             (hedgecount > 0) ) )    /* there are some left */
      {
         /* horizontal edge */
         /* pick one at random from the unchecked */
         index = rand_int(hedgecount);
         curedge = hedgelist[index];
         /* and remove it from the "to check" list */
         hedgelist[index] = hedgelist[--hedgecount];
         /* if the stuff on either side of it is already
          * connected, leave it alone.  If they're not,
          * then do_set connectes them, and we punch a hole
          * in that wall */
         maze_hedges[curedge] =
            maze_do_set(squares,curedge,curedge-1);
      }
      else
      {
         /* vertical edge */
         /* pick one at random from the unchecked */
         index = rand_int(vedgecount);
         curedge = vedgelist[index];
         /* and remove it from the "to check" list */
         vedgelist[index] = vedgelist[--vedgecount];
         /* if the stuff on either side of it is already
          * connected, leave it alone.  If they're not,
          * then do_set connectes them, and we punch a hole
          * in that wall */
         maze_vedges[curedge] =
            maze_do_set(squares,curedge,curedge-y_count);
      }
   }
   /* Finish up the horizontal edges of the maze */
   for (x=0; x<x_count; x++)
   {
      maze_hedges[x*y_count] = 1;
   }
   /* and the vertical edges too */
   for (y=0; y<y_count; y++)
   {
      maze_vedges[y] = 1;
   }
   /* and make one entrance/exit */
   x = rand_int(x_count);
   maze_hedges[x*y_count] = 0;
}

/* and a function to display it using ascii graphics */
void copy_maze(s16b x_origin, s16b y_origin, s16b x_count, s16b y_count, bool light)
{
   s16b x = 0, y = 0;

   /* first the entrance */
   for (x=0; x<x_count; x++)
   {
      if (maze_hedges[x*y_count] == 0)
      {
         if (dun->cent_n < CENT_MAX)
         {
            (void)set_grid_type(x_origin + 2*x + 1, y_origin, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_KEEP, light ? CAVE_GLOW:0 );
dlog(DEBUGGENER,"generate.c: copy_maze: entrance added as room center %d @ %d,%d\n",
                dun->cent_n, x_origin+2*x+1, y_origin);
            dun->cent[dun->cent_n].x = x_origin+2*x+1;
            dun->cent[dun->cent_n].y = y_origin;
            dun->cent_n++;
         }
      }
   }
   /*
    * the maze has a size of 3x3 when it has 1 cell
    *                        5x5             2x2 cells
    *                        (cell*2+1)x(cell*2+1) in general
    *
    * each odd-coordinate pair has a wall, we build that first, and also
    * initialize the whole maze as 1x1 floors surrounded by walls, so we
    * later only need to break open the walls between connected cells
    */
dlog(DEBUGGENER,"generate.c: generate_maze: adding walls from %d,%d to %d,%d\n",
                x_origin, y_origin, x_origin + 2*x_count, y_origin + 2*y_count);
   for (y=y_origin; y <= (y_origin + 2 * y_count); y++)
   {
      for (x=x_origin; x <= (x_origin + 2 * x_count); x++)
      {
         if ( (x==x_origin) || (y == y_origin) ||
              (x == (x_origin + 2 * x_count + 1)) || (y == (y_origin + 2 * y_count + 1) ) )
         {
            (void)set_grid_type(x, y, DUNG_PERWALL, DUNG_PERWALL_OUTER,
                                GRID_KEEP, (light?(CAVE_MAZE | CAVE_GLOW):CAVE_MAZE) );
dlog(DEBUGGENER,"generate.c: generate_maze: adding perwall_outer @ %d,%d\n", x, y);
         }
         else
         {
            /* if this is an odd square, it should be a floor, else a wall */
            if ( ( ((x-x_origin) % 2) == 1 ) && ( ((y-y_origin) % 2) == 1) )
            {
               (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                   GRID_KEEP, (light?(CAVE_MAZE | CAVE_GLOW):CAVE_MAZE) );
dlog(DEBUGGENER,"generate.c: generate_maze: adding floor @ %d,%d\n", x, y);
            }
            else
            {
               (void)set_grid_type(x, y, DUNG_PERWALL, DUNG_PERWALL_OUTER,
                                   GRID_KEEP, (light?(CAVE_MAZE | CAVE_GLOW):CAVE_MAZE) );
dlog(DEBUGGENER,"generate.c: generate_maze: adding perwall_outer (inner) @ %d,%d\n", x, y);
            }
         }
      }
   }
   /* now we break open the walls where needed */
   for (y=0; y<y_count; y++)
   {
      /* horizontal openings */
      for (x=0; x<x_count; x++)
      {
         if (maze_hedges[x*y_count+y] == 0)
         {
            (void)set_grid_type(x_origin+x*2+1, y_origin+y*2, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_KEEP, (light?(CAVE_MAZE | CAVE_GLOW):CAVE_MAZE) );
         }
      }
      /* vertital openings */
      for (x=0; x<x_count; x++)
      {
         if (maze_vedges[x*y_count+y] == 0)
         {
            (void)set_grid_type(x_origin+x*2, y_origin+y*2+1, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_KEEP, (light?(CAVE_MAZE | CAVE_GLOW):CAVE_MAZE) );
         }
      }
   }
   /* and now the exit from the maze */
   for (x=0; x<x_count; x++)
   {
      if (maze_hedges[x*y_count] == 0)
      {
         if (dun->cent_n < CENT_MAX)
         {
            (void)set_grid_type(x_origin + 2*x + 1, y_origin + 2*y_count, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                                GRID_KEEP, light ? CAVE_GLOW:0 );
dlog(DEBUGGENER,"generate.c: copy_maze: exit added as room center %d @ %d,%d\n",
                dun->cent_n, x_origin+2*x+1, y_origin+2*y_count);
            dun->cent[dun->cent_n].x = x_origin+2*x+1;
            dun->cent[dun->cent_n].y = y_origin+2*y_count;
            dun->cent_n++;
         }
      }
   }
}

/*
 * The main function just calls the pieces of the maze generation.
 * it returns the number of rooms that probably could have been built
 * in the space the maze occupies
 */
s16b build_maze(void)
{
   s16b x, y, rooms;

   /* minimum size 25% of dungeon, max 75% */
   /* realize that the maze is twice as wide and twice as high as */
   /* x_count,y_count seem to suggest....                         */
   s16b x_count = (cur_wid / 2) - 8;
   s16b y_count = (cur_hgt / 2) - 8;

   /* position at least 10 from the rim, so a tunnel can pass */
   s16b x_origin = 2;
   s16b y_origin = 2;

   s16b mx, my;

   /* don't code this maze if we are out of bounds after it, since that */
   /* will lead to out-of-bounds tunnel destinations that will loop the */
   /* tunnel building process                                           */
   if (!in_bounds(x_origin + 2*x_count +1, y_origin+2*y_count+1))
   {
dlog(DEBUGGENER,"generate.c: build_maze: out-of-bounds; returing (size %d,%d, offset %d,%d)\n",
                x_origin+2*x_count+1, y_origin+2*y_count+1, cur_wid-1, cur_hgt-1);
      return 0;
   }

   /* decide on a maze */
dlog(DEBUGGENER,"generate.c: build_maze: before randomize, size %d,%d, offset %d,%d\n",
                x_count, y_count, x_origin, y_origin);
   maze_randomize(x_count, y_count);

   /* and draw it. */
   copy_maze(x_origin, y_origin, x_count, y_count, TRUE);

   /* now claim this space in the map */
   for (x=x_origin; x <= (x_origin + 2*x_count); x += BLOCK_WID)
   {
      for (y=y_origin; y <= (y_origin + 2*y_count); y+= BLOCK_HGT)
      {
         mx = (x / BLOCK_WID);
         my = (y / BLOCK_WID);
dlog(DEBUGGENER,"generate.c: build_maze: declaring room_map %d,%d as used\n", mx, my);
         dun->room_map[mx][my] = 2;
      }
   }
   for (x=x_origin; x <= (x_origin + 2*x_count); x++)
   {
      for (y=y_origin; y <= (y_origin + 2*y_count); y++)
      {
          cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGGENER,"generate.c: build_maze: finished, %d,%d type %d,%d %s\n",
                x, y, c_ptr->mtyp, c_ptr->styp, f_name + f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name);
      }
   }
   /* done! */
   /* let's assume each BLOCK_WID * BLOCK_HGT block has 1/2 room on average... */
   rooms =  (x_count * y_count * 4 / (BLOCK_WID * BLOCK_HGT * 2));
   return rooms;
}


static void build_normal_rooms(bool level_destroyed, bool level_overlap, bool level_maze)
{
   s16b x, y, i, k, typ, chance;
   s16b max_rooms = DUN_ROOMS;

dlog(DEBUGGENER,"generate.c: build_normal_rooms: cave is %d x %d\n", cur_wid, cur_hgt);
   /* Actual maximum number of rooms on this level */
   dun->row_rooms = cur_hgt / BLOCK_HGT;
   dun->col_rooms = cur_wid / BLOCK_WID;

   /* Initialize the room table */
   for (x = 0; x < dun->col_rooms; x++)
   {
      for (y = 0; y < dun->row_rooms; y++)
      {
         dun->room_map[x][y] = 0;
      }
   }

   msg_print("Rooms. ");
   /* No "crowded" rooms yet */
   dun->crowded = FALSE;

   /* No rooms yet */
   dun->cent_n = 0;

   /* Build some rooms */
   for (i = 0; i < max_rooms; i++)
   {
dlog(DEBUGGENER,"generate.c: build_normal_rooms: building room %d of %d\n", i, DUN_ROOMS);
      /* Pick a block for the room */
      x = rand_int(dun->col_rooms);
      y = rand_int(dun->row_rooms);

      /* Align dungeon rooms */
      if (dungeon_align)
      {
         /* Slide some rooms right */
         if ((x % 3) == 0) x++;

         /* Slide some rooms left */
         if ((x % 3) == 2) x--;
      }

      /* Destroyed levels are boring */
      if (level_destroyed)
      {
         if (randint(100)>ROOM_ROUND_CHANCE)
         {
            /* Attempt a "trivial" room */
            if (room_build(x,y,1, level_overlap)) continue;
         }
         else
         {
            /* Attempt a "trivial round" room */
            if (room_build(x,y,16, level_overlap)) continue;
         }

         /* Never mind */
         continue;
      }

      /* build the room a wizard wanted */
      /* but only try the first 10% in case it doesn't work */
      if ((wizard_target != 0L) && (i < (DUN_ROOMS/10) ) )
      {
         bool wizard_test_done = room_build(x, y, (s16b)(wizard_target >> 24), level_overlap);
         /* test for others */
         if (wizard_test_done)
         {
            msg_format("Wizard room type %d built at block %d,%d (approx %d,%d)\n",
                       (s16b)(wizard_target>>24), x, y,
                       x*BLOCK_WID + (BLOCK_WID/2), y*BLOCK_HGT + (BLOCK_HGT/2)),
            wizard_target=(wizard_target << 8);
         }
         continue;
      }

      /* Attempt an "unusual" room */
      chance=rand_int(DUN_UNUSUAL/(scum_always?(scum_verygood?3:2):1));
      if (chance < p_ptr->mdepth)
      {
         /* Roll for room type */
         k = rand_int(1000);

         /* build store - don't depend on p_ptr->mdepth */
         if (k<DUN_STORES)
         {
            /* don't overlap stores *ever* */
            if (room_build(x, y, 15, FALSE))
            {
               store_built = TRUE;
               continue;
            }
         }
         k = rand_int(100);

         /* Attempt a very unusual room */
         chance=rand_int(DUN_UNUSUAL/(scum_always?(scum_verygood?3:2):1));
         if (chance < p_ptr->mdepth)
         {
            /* Treasure rooms: overall 20%, small 10%, middle 7%, large 3% */
            if (k < 20)
            {
               if ( (k < 10) && room_build(x, y, 9, FALSE)) continue; /* small treasure room */
               if ( (k < 17) && room_build(x, y, 10, FALSE)) continue; /* middle treasure room */
               if ( (k < 20) && room_build(x, y, 11, FALSE)) continue; /* large treasure room */
            }
            /* Graveyards: overall 25%, small 15%, middle 7%, large 3% */
            else if (k < 45)
            {
               if ( (k < 35) && room_build(x, y, 12, FALSE)) continue;
               if ( (k < 42) && room_build(x, y, 13, FALSE)) continue;
               if ( (k < 45) && room_build(x, y, 14, FALSE)) continue;
            }
            /* pits/nests: overall 35%, pit 18%, nest 17% */
            else if (k < 80)
            {
               if ( (k < 63) && room_build(x, y, 6, FALSE)) continue;
               if ( (k < 80) && room_build(x, y, 5, FALSE)) continue;
            }
            /* vaults: overall 20%, lesser vault 10%, multi-level 7%, greater 3% */
            else
            {
               if ( (k < 90) && room_build(x, y, 7, FALSE)) continue;
               if ( (k < 97) && room_build(x, y, 17, FALSE)) continue;
               if ( room_build(x, y, 8, FALSE)) continue;
            }
         }


         /* Type 4 -- Large room (25%) */
         if ((k < 25) && room_build(x,y,4, level_overlap)) continue;

         /* Type 3 -- Cross room (25%) */
         if ((k < 50) && room_build(x,y,3, level_overlap)) continue;

         /* Type 2 -- Overlapping (50%) */
         if ((k < 100) && room_build(x,y,2, level_overlap)) continue;

      }

      /* Attempt a trivial room - type 1 or 16 */
      typ=(randint(100)<ROOM_ROUND_CHANCE)?16:1;
      if (room_build(x,y,typ, level_overlap)) continue;
   }
}

/*
 * this function builds some type 18 vaults inside rooms
 */
static void build_inner_vaults(void)
{
   s16b         i, v_num;
   vault_type  *v_ptr = NULL;
   s16b         x, y, sx, sy;
   s16b         ox, oy, x1 = 0, y1 = 0;
   s16b         need_x, need_y, cent_x, cent_y;
   bool         light;

dlog(DEBUGGENER,"generate.c: build_inner_vaults: trying for %d inner vaults\n", (DUN_ROOMS / DUN_INRVT_RATIO) );
   for (i=0; i < (DUN_ROOMS / DUN_INRVT_RATIO); i++)
   {
      v_num = find_vault(18);
      if (v_num == -1) return;
      v_ptr = &v_info[v_num];

      /* randomize the searching with offset ox, oy */
      ox = rand_int(cur_wid);
      oy = rand_int(cur_hgt);

      /* for a 5x5 inner vault, we need 7x7 space */
      need_x = v_ptr->wid+2;
      need_y = v_ptr->hgt+2;

dlog(DEBUGGENER,"generate.c: build_inner_vaults: trying for %dx%d open space vptr->typ %d v_ptr->id %d\n",
                 need_x, need_y, v_ptr->typ, v_ptr->id);
      for (sy=0; sy < cur_hgt; sy++)
      {
         for (sx=0; sx < cur_wid; sx++)
         {
            x=(sx+ox) % cur_wid;
            y=(sy+oy) % cur_hgt;

            /* keep within the dungeon! */
            if ((x + need_x) > cur_wid) continue;
            if ((y + need_y) > cur_hgt) continue;

            /* did we find a possible winner? */
            if (is_floor(x, y))
            {
               for (y1=0; y1 <= need_y; y1++)
               {
                  for (x1=0; x1 <= need_x; x1++)
                  {
                     if (!is_floor(x+x1, y+y1))
                     {
                       /* leave this loop, and make sure we don't  */
                       /* think we have found enough space here    */
                        x1 = need_x+1;
                        y1 = need_y+1;
                        break;
                     }
dlog(DEBUGGENER2,"generate.c: build_inner_vaults: found floor @ %d,%d\n", x+x1, y+y1);
                  }
               }
               /* have we found all needed squares as floor? */

               if ( (x1 == need_x) && (y1 == need_y) )
               {
                  cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
                  light = (c_ptr->fdat & CAVE_GLOW) ? TRUE : FALSE;
dlog(DEBUGGENER,"generate.c: build_inner_vaults: found %dx%d floor cels @ %d,%d\n", need_x, need_y, x, y );

                  /* now build that vault here */
                  build_vault( x + (need_x / 2),y + (need_y / 2), v_num, light, &cent_x, &cent_y);

                  /* and skip all of this loop */
                  sx = cur_wid;
                  sy = cur_hgt;
               }
            }
         }
      }
   }
}


static void build_normal_tunnels(void)
{
   s16b x, y, x1, y1, i;

   /* Hack -- Scramble the room order */
   for (i = 0; i < dun->cent_n; i++)
   {
      s16b pick1 = rand_int(dun->cent_n);
      s16b pick2 = rand_int(dun->cent_n);
      y1 = dun->cent[pick1].y;
      x1 = dun->cent[pick1].x;
      dun->cent[pick1].y = dun->cent[pick2].y;
      dun->cent[pick1].x = dun->cent[pick2].x;
      dun->cent[pick2].y = y1;
      dun->cent[pick2].x = x1;
dlog(DEBUGGENER,"generate.c: build_normal_tunnels: switching dun_cent %d with %d\n",
           pick1, pick2);
   }

   msg_print("Tunnels. ");
   /* Start with no tunnel doors */
   dun->door_n = 0;

   /* Hack -- connect the first room to the last room */
   x = dun->cent[dun->cent_n-1].x;
   y = dun->cent[dun->cent_n-1].y;
dlog(DEBUGGENER,"generate.c: build_normal_tunnels: tunnel building starts at %d,%d\n", x, y);
   /* Connect all the rooms together */
   for (i = 0; i < dun->cent_n; i++)
   {
      /* Connect the room to the previous room */
dlog(DEBUGGENER,"generate.c: build_normal_tunnels: building tunnel from %d,%d to %d,%d i %d\n",
           dun->cent[i].x, dun->cent[i].y, x, y, i);
      /* try to build a tunnel, if this does not work */
      /* try to build it the other way around.....    */
      if (!build_tunnel(dun->cent[i].x, dun->cent[i].y, x, y, TRUE))
      {
         /* if this doesn't work, we are out of luck  */
         /* try again in test_all_connected if needed */
         (void)build_tunnel(x, y, dun->cent[i].x, dun->cent[i].y, TRUE);
      }

      if (rand_int(100)<DUN_TUN_WID) expand_tunnel();
      /* Remember the "previous" room */
      x = dun->cent[i].x;
      y = dun->cent[i].y;
   }

   /* Place intersection doors  */
   for (i = 0; i < dun->door_n; i++)
   {
      /* Extract junction location */
      y = dun->door[i].y;
      x = dun->door[i].x;

      /* Try placing doors */
      try_door(x - 1, y    );
      try_door(x + 1, y    );
      try_door(x    , y - 1);
      try_door(x    , y + 1);
      /* jk - throw in a monster/trap in the junction.... */
      if (rand_int(100)<30)
         (void)place_monster(x, y, TRUE, TRUE,0);
      else if (rand_int(100)<30)
         place_trap(x, y, p_ptr->mdepth, 20, 5);
   }
   /* check for weird doors in corners etc. */
   check_doors();
   /* check for weird passages allowing diagonal movement */
   /* from tunnel to tunnel                               */
   check_weird_passages();
   /* test if everything is connected, and loop while doing so */
   msg_print("Checking layout. ");
   if (dungeon_connected)
   {
      s16b tries = 0;
    
      while ((test_all_connected() == TRUE) && (tries < 5))
      {
         tries++;
      }
       
   }
}

/*
 * Generate a new dungeon level
 */
static void cave_gen(void)
{
   s16b i, k, x, y;
   s16b fact = 2, size;

   /* Assume not destroyed */
   bool level_destroyed = FALSE;
   bool level_maze = FALSE;
   bool level_overlap = FALSE;

dlog(DEBUGGENER,"generate.c: cave_gen: num_stores now %d at start\n", num_stores);
   fill_with_granite();

   store_built = FALSE;

   /* Possible "destroyed" level */
   if ((p_ptr->mdepth > 10) && (rand_int(DUN_DEST) == 0))
   {
      if (cheat_room) msg_print("Destroyed level.");
      level_destroyed = TRUE;
   }

   /* possible "overlapping" level */
   if ((p_ptr->mdepth > 4) && (rand_int(DUN_OVERLAP) == 0) && (level_destroyed == FALSE))
   {
      level_overlap = TRUE;
      if (cheat_room) msg_print("Level with large rooms.");
   }

   /* possible "maze" level */
   if ((p_ptr->mdepth > 15) && (rand_int(DUN_MAZE) == 0) &&
       (level_destroyed == FALSE) && (level_overlap == FALSE))
   {
      level_overlap = FALSE;
      level_destroyed = FALSE;
      level_maze = TRUE;
      if (cheat_room) msg_print("Maze level.");
   }

   /* Hack -- No destroyed "quest" levels */
   if (is_quest(p_ptr->mdepth))
   {
      level_destroyed = FALSE;
      level_overlap = FALSE;
      level_maze = FALSE;
   }

   /* Allocate the "dungeon" data */
   MAKE(dun, dun_data);

   switch(randint(10))
   {
      case  1:
      case  2:
      case  3:
      case  4:
      case  5:
      case  6: fact = 9999; break;
      case  7: fact = 6; break;
      case  8: fact = 4; break;
      case  9: fact = 3; break;
      case 10: fact = 2; break;
   }
   

   cur_wid = cur_wid - (cur_wid / fact);
   cur_hgt = cur_hgt - (cur_hgt / fact);
   if (!generate_large_levels) 
   {
      cur_hgt = 3*SCREEN_HGT;
      cur_wid = 3*SCREEN_WID;
   }

   dungeon.level_wid[0] = cur_wid;
   dungeon.level_hgt[0] = cur_hgt;


   if (level_maze)
   {
      build_maze();
   }
   else
   {
      build_normal_rooms(level_destroyed, level_overlap, level_maze);
      build_inner_vaults();
   }

   /* Special boundary walls -- Top & Bottom */
   for (x = 0; x < cur_wid; x++)
   {
      /* Clear previous contents, add "solid" perma-wall */
      (void)set_grid_type(x, 0, DUNG_PERWALL,
                          DUNG_PERWALL_SOLID, GRID_ADD, 0);
      (void)set_grid_type(x, cur_hgt-1, DUNG_PERWALL,
                          DUNG_PERWALL_SOLID, GRID_ADD, 0);
   }

   /* Special boundary walls -- Left & Right */
   for (y = 0; y < cur_hgt; y++)
   {
      (void)set_grid_type(0, y, DUNG_PERWALL,
                          DUNG_PERWALL_SOLID, GRID_ADD, 0);
      (void)set_grid_type(cur_wid-1, y, DUNG_PERWALL,
                          DUNG_PERWALL_SOLID, GRID_ADD, 0);
   }

   if (!level_maze)
   {
      build_normal_tunnels();
   }


   /* Add some quartz streamers */
   for (i = 0; i < DUN_STR_MAG; i++)
   {
      build_streamer(DUNG_WALL_ART_QUARTZ, DUN_STR_QZC);
   }

   /* Add some magma streamers */
   for (i = 0; i < DUN_STR_MAG; i++)
   {
      build_streamer(DUNG_WALL_ART_MAGMA, DUN_STR_MGC);
   }

   /* build some random streamers */
   for ( i = 0; i < DUN_STR_OTH; i++)
   {
      build_streamer(0, DUN_STR_OTC);
   }

   /* Destroy the level if necessary */
   if (level_destroyed) destroy_level();

   /* this puts size from 10 (big level) to 5 (small level) */
   size = 10 - ( 10 / fact );

dlog(DEBUGGENER,"generate.c: cave_gen: p_ptr->mdepth,sdepth %d,%d main_level %d sublevel %d\n",
                p_ptr->mdepth, p_ptr->sdepth,baselevel, sublevel);
   msg_print("Stairs. ");

   if (!level_maze)
   {
      /* Place 3 or 4 down stairs near some walls */
      alloc_stairs(0x07, 2+rand_range(size/2 - 1, size / 2), 3, 2);

      /* Place 1 or 2 up stairs near some walls */
      alloc_stairs(0x06, 1+rand_range(size/2 - 1 , size / 2), 3, 2);
   }
   else
   {
dlog(DEBUGGENER,"generate.c: cave_gen: maze, placing stairs @ %d,%d and %d,%d\n",
                 dun->cent[0].x, dun->cent[0].y, dun->cent[1].x, dun->cent[1].y);
      place_main_up_stair(dun->cent[0].x, dun->cent[0].y, FALSE);
      place_main_down_stair(dun->cent[1].x, dun->cent[1].y, FALSE);
   }

   /* Release the "dungeon" data - do it here, since we need it for the */
   /* staircases in mazes, see above                                    */
   KILL(dun, dun_data);

   /* Determine the character location */
   new_player_spot(FALSE);

   /* Basic "amount" */
   k = (p_ptr->mdepth / 3);
   if (k > 10) k = 10;
   if (k < 2) k = 2;

   /* Pick a base number of monsters */
   i = MIN_M_ALLOC_LEVEL + randint(10);

   msg_print("Monsters. ");

   /* Put some monsters in the dungeon */
   for (i = i + k; i > 0; i--)
   {
      (void)alloc_monster(0, TRUE, 2);
   }

   msg_print("Traps. ");

   /* Place some traps in the dungeon */
   if (level_maze)
   {
      alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, 3*randint(k));
   }
   else
   {
      alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP, randint(k));
   }

   /* Put some rubble in corridors */
   alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(k));

   msg_print("Objects.");

   /* Put some objects in rooms */
   if (!level_maze)
   {
      alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ROOM, 3));
   }

   /* Put some objects/gold in the dungeon */
   if (level_maze)
   {
      alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ITEM*3, 3));
      alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, randnor(DUN_AMT_GOLD*3, 3));
   }
   else
   {
      alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT, randnor(DUN_AMT_ITEM, 3));
      alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD, randnor(DUN_AMT_GOLD, 3));
   }
}

/*
 * reset some parameters before generating a cave on a level
 */
static void reset_cave(void)
{
   s16b i;
   /* Start with a blank cave */
   for (i = 0; i < MAX_HGT; i++)
   {
      /* Wipe a whole row at a time */
      C_WIPE(dungeon.level[sublevel][i], MAX_WID, cave_cell_type);
   }

   for (i=0;i<ROOM_MAX;i++)
   {
      rooms_built[i] = 0;
   }

   /* Mega-Hack -- no panel yet */
   panel_min_row = 0;
   panel_max_row = 0;
   panel_min_col = 0;
   panel_max_col = 0;

   /* Reset the monster generation level */
   monster_level = p_ptr->mdepth;

   /* Reset the object generation level */
   object_level = p_ptr->mdepth;

   /* Nothing special here yet */
   good_item_flag = FALSE;

   monsters_with_artifacts = FALSE;

   /* nothing good here yet */
   rating = 0;
   cur_hgt = MAX_HGT;
   cur_wid = MAX_WID;

   /* Determine number of panels */
   panel_max_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
   panel_max_cols = (cur_wid / SCREEN_WID) * 2 - 2;

   /* Assume illegal panel */
   p_ptr->wy = panel_max_rows;
   p_ptr->wx = panel_max_cols;
}

/*
 * Generates a random dungeon level                     -RAK-
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Hack -- allow auto-scumming via a gameplay option.
 */
void generate_cave(void)
{
   s16b num;
   u32b old_wizard_target = 0L;

/* jk */
   bool save_inkey_scan;

   /* No dungeon yet */
   character_dungeon = FALSE;
/* jk */
   save_inkey_scan = inkey_scan;
   inkey_scan=TRUE;

   msg_print("Generating dungeon:");

   if (wizard_target != 0L)
   {
      old_wizard_target = wizard_target;
   }

   if (sublevel != 0)
   {
dlog(DEBUGEXTRA,"generate.c: generate_cave: sublevel = %d, setting to 0!\n", sublevel);
      sublevel = 0;
   }
   /* Generate */
   for (num = 0; TRUE; num++)
   {
      bool okay = TRUE;

      cptr why = NULL;

dlog(DEBUGGENER,"generate.c: generate_cave: starting in loop %d\n", num);
      /* Hack -- Reset heaps */
      wipe_old_level(p_ptr->mdepth);
dlog(DEBUGGENER,"generate.c: generate_cave: level wiped\n", num);

      i_max = 1;
      mn_max = 1;

      reset_cave();

      dungeon.level_used[0] = TRUE;           /* we now use the main level */
      dungeon.level_depth[0] = p_ptr->mdepth; /* and it's here!            */

      /* Build the town */
      if (!p_ptr->mdepth)
      {
         /* Make a town */
dlog(DEBUGGENER,"generate.c: generate_cave: about to generate town\n");
         town_gen();
      }

      /* Build a real level */
      else
      {
         /* Make a dungeon */
dlog(DEBUGGENER,"generate.c: generate_cave: about to generate cave\n");
         cave_gen();
      }

/* jk - if scumming for very good levels, create these! */
      if (scum_verygood)
      {
         rating /= 2;
      }

      /* Extract the feeling */
      if (rating > (40*RATIO)) feeling = 2;
      else if (rating > (32*RATIO)) feeling = 3;
      else if (rating > (26*RATIO)) feeling = 4;
      else if (rating > (20*RATIO)) feeling = 5;
      else if (rating > (14*RATIO)) feeling = 6;
      else if (rating > (8*RATIO)) feeling = 7;
      else if (rating > (4*RATIO)) feeling = 8;
      else if (rating > 0) feeling = 9;
      else feeling = 10;

      /* It takes 1000 game turns for "feelings" to recharge */
/* jk - not in wizard mode */
      if ((!wizard) && ((turn - old_turn) < 1000)) feeling = 0;

      /* Hack -- no feeling in the town */
      if (!p_ptr->mdepth)
      {
         feeling = 0;
      }

      /* Mega-Hack -- allow "auto-scum" */
      if ( (scum_always || (scum_sometimes && rand_int(4)==1)) &&
           (num < 500) )
      {
         /* Require "goodness" */

         if ((feeling > 9) ||
             ((p_ptr->mdepth >= 5) && (feeling > 8)) ||
             ((p_ptr->mdepth >= 10) && (feeling > 7)) ||
             ((p_ptr->mdepth >= 20) && (feeling > 6)) ||
             ((p_ptr->mdepth >= 40) && (feeling > 5)))
         {

            /* Give message to cheaters */
            if (cheat_room || cheat_hear ||
                cheat_peek || cheat_xtra)
            {
               /* Message */
               why = "boring level";
            }

            /* Try again */
            okay = FALSE;
         }
      }

      /* Prevent object over-flow */
      if (i_max >= MAX_I_IDX)
      {
         /* Message */
         why = "too many objects";

         /* Message */
         okay = FALSE;
      }
      /* Prevent monster over-flow */
      if (mn_max >= MAX_M_IDX)
      {
         /* Message */
         why = "too many monsters";

         /* Message */
         okay = FALSE;
      }

      /* Accept */
      if (okay) break;

      /* Message */
      if (why) msg_format("Generation restarted (%s) %d", why, num);

      /* Wipe the objects */
      wipe_i_list();

      /* Wipe the monsters */
      wipe_mn_list();

/* jk - wipe the traps, item_sets */
      wipe_t_list();
      wipe_is_list();
   }

   if (old_wizard_target != 0)
   {
      msg_format("Tried to generate wizard rooms type %d, %d, %d and %d\n",
                 (s16b)((old_wizard_target & 0xff000000L)>>24),
                 (s16b)((old_wizard_target & 0xff0000L)>>16),
                 (s16b)((old_wizard_target & 0xff00L)>>8),
                 (s16b)(old_wizard_target & 0xffL) );
   }

   /* Dungeon level ready */
   character_dungeon = TRUE;
#if (debuglevel & DEBUGGENER2)
{
   int x, y;
   for (x=0; x<cur_wid; x++)
   {
      for (y=0; y<cur_hgt; y++)
      {
         cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
         dlog(DEBUGGENER2,"generate.c: generate_cave: %d,%d m %d s %d ex %d fdat %08lx n %s\n",
                    x, y, c_ptr->mtyp, c_ptr->styp, c_ptr->extra, c_ptr->fdat,
                    f_name+f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name);
      }
   }
}
#endif
#if (debuglevel & DEBUGGENER)
{
   s16b old_sublevel = sublevel, x, y;
   for (sublevel = 0; sublevel < MAX_SUB_LEVELS; sublevel++)
   {
      if (!dungeon.level_used[sublevel]) continue;

      for (x=0; x<dungeon.level_wid[sublevel]; x++)
      {
         for (y=0; y<dungeon.level_hgt[sublevel]; y++)
         {
            cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
            if (c_ptr->mtyp == DUNG_STAIR)
            {
               s16b tmp_main, tmp_sub;
               get_stair_target(x, y, &tmp_main, &tmp_sub);
               dlog(DEBUGGENER,"generate.c: generate_cave: lev %02d,%02d x,y %03d,%03d stair typ %d = %s points to %d,%d\n",
                               baselevel, sublevel, x, y, c_ptr->styp,
                               f_name+f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name,
                               tmp_main, tmp_sub);
            }
         }
      }
   }
   sublevel = old_sublevel;
}
#endif

   /* Remember when this level was "created" */
   old_turn = turn;
/* jk */
   inkey_scan=save_inkey_scan;
}
