/* File: zborg2.h */
/* Purpose: Header file for "borg2.c" -BEN- */

#ifndef INCLUDED_BORG2_H
#define INCLUDED_BORG2_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg2.c".
 */

#include "zborg1.h"

/*
 * Determine if a "legal" grid is a "floor" grid
 */
#define borg_cave_floor_grid(C) \
    (!(f_info[(C)->feat].flags & FF_BLOCK))
/*
 * Determine if a "legal" grid is a "wall" grid
 */
#define borg_cave_wall_grid(C) \
    (f_info[(C)->feat].flags & FF_BLOCK)

/*
 * True half the time for trees. (Block line of sight half the time.)
 */
#define borg_cave_half_grid(C) \
    ((f_info[(C)->feat].flags & FF_HALF_LOS) && (quick_rand()))

/*
 * Grid will block LOS.
 */
#define borg_cave_los_grid(C) \
   ((borg_cave_floor_grid(C)) || (borg_cave_half_grid(C)))


/*
 * Bold version of borg_cave_floor_grid
 * (with bounds checking)
 */
#define borg_cave_floor_bold(Y, X) \
    (map_in_bounds((X),(Y)) && \
	 borg_cave_floor_grid(map_loc((X),(Y))))


/* Types of monster list */
#define BORG_MON_USED	1
#define BORG_MON_NEW	2
#define BORG_MON_OLD	3
#define BORG_MON_MOVE	4

/* Useful typedef for los_general() */
typedef bool (*map_hook_type) (map_block *mb_ptr);

/* Recalculate danger */
extern bool borg_danger_wipe;


extern void borg_mmove_init(int x1, int y1, int x2, int y2);
extern void borg_mmove(int *x, int *y, int x1, int y1);
extern bool borg_los(int x1, int y1, int x2, int y2);
extern bool borg_los_pure(int x1, int y1, int x2, int y2);
extern bool borg_bolt_los(int x1, int y1, int x2, int y2);
extern bool borg_bolt_los_pure(int x1, int y1, int x2, int y2);
extern bool borg_projectable(int x1, int y1, int x2, int y2);
extern void borg_forget_view(void);
extern void borg_update_view(void);
extern void borg_delete_kill(int i, cptr reason);
extern void borg_add_dungeon(int x, int y, int min_depth, int max_depth, bool bottom);
extern int  borg_add_town(int x, int y, cptr town_name);
extern void borg_map_info(map_block *mb_ptr, const term_map *map, vptr dummy);
extern void borg_map_erase(vptr dummy);
extern void borg_update(void);
extern void borg_react(cptr msg, cptr buf);
extern void borg_player_move(int x, int y, vptr dummy);

/*
 * Initialize this file
 */
extern void borg_init_2(void);


#endif


#endif
