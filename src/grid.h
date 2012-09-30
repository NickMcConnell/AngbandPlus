/* CVS: Last edit by $Author: sfuerst $ on $Date: 2000/10/04 10:32:05 $
 *
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

#define place_locked_door(Y,X)  make_lockjam_door(Y, X, randint(10) + dun_level / 10, FALSE)
#define place_secret_door(Y,X)  cave[(Y)][(X)].feat = FEAT_SECRET

#define LAKE_LAVA		1
#define LAKE_WATER		2
#define LAKE_DESTROY	3
#define LAKE_EEARTH		4
#define LAKE_EAIR		5
#define LAKE_EWATER		6
#define LAKE_EFIRE		7
#define LAKE_CAVERN		8


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

extern void generate_hmap(int y0, int x0, int xsiz, int ysiz, int grd,
	 int roug, int cutoff);
extern bool generate_fracave(int y0, int x0, int xsize, int ysize, int cutoff,
	 bool light);
extern bool generate_lake(int y0, int x0, int xsize, int ysize,
	 int c1, int c2, int c3, int type);
