/*
 * File: grid.h
 * Purpose: header file for grid.c, used only in dungeon generation
 * files (generate.c, rooms.c)
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


/* Macros */

#define set_cave_feat(Y,X,F)    (cave[(Y)][(X)].feat = (F))
#define add_cave_info(Y,X,I)    (cave[(Y)][(X)].info |= (I))

/* This should not be used */
/*#define set_cave_info(Y,X,I)    (cave[(Y)][(X)].info = (I)) */

#define place_rubble(Y,X)       set_cave_feat(Y,X,FEAT_RUBBLE)
#define place_up_stairs(Y,X)    set_cave_feat(Y,X,FEAT_LESS)
#define place_down_stairs(Y,X)  set_cave_feat(Y,X,FEAT_MORE)
#define place_locked_door(Y,X)  set_cave_feat(Y,X,FEAT_DOOR_HEAD+randint1(7))
#define place_secret_door(Y,X)  set_cave_feat(Y,X,FEAT_SECRET)
#define place_inner_wall(Y,X)   set_cave_feat(Y,X,FEAT_WALL_INNER)
#define place_outer_wall(Y,X)   set_cave_feat(Y,X,FEAT_WALL_OUTER)
#define place_invis_wall(Y,X)   set_cave_feat(Y,X,FEAT_WALL_INVIS)

/* Externs */

extern bool new_player_spot(void);

extern void place_random_stairs(int y, int x);
extern void place_random_door(int y, int x);
extern void place_closed_door(int y, int x);
extern void vault_monsters(int y1, int x1, int num);
extern void vault_objects(int y, int x, int num);
extern void vault_trap_aux(int y, int x, int yd, int xd);
extern void vault_traps(int y, int x, int yd, int xd, int num);

extern int next_to_walls(int y, int x);

extern void generate_room(int y1, int x1, int y2, int x2, int light);
extern void generate_vault(int y1, int x1, int y2, int x2);
extern void clear_vault(int y1, int x1, int y2, int x2);
extern void generate_fill(int y1, int x1, int y2, int x2, int feat);
extern void generate_draw(int y1, int x1, int y2, int x2, int feat);
extern void generate_plus(int y1, int x1, int y2, int x2, int feat);
extern void generate_open(int y1, int x1, int y2, int x2, int feat);
extern void generate_hole(int y1, int x1, int y2, int x2, int feat);
extern void generate_door(int y1, int x1, int y2, int x2, bool secret);

extern void correct_dir(int *rdir, int *cdir, int y1, int x1, int y2, int x2);
extern void rand_dir(int *rdir, int *cdir);

extern bool get_is_floor(int x, int y);
extern void set_floor(int x, int y);

extern void build_tunnel(int row1, int col1, int row2, int col2);
extern bool build_tunnel2(int x1, int y1, int x2, int y2, int type, int cutoff);
