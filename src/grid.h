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

/*
 * Access the cave data for the current region
 */
#define cave_p(X, Y) \
	(&cave_data[Y][X])

/*
 * Access the cave data for the given region
 */
#define access_region(X, Y, R) \
	(&rg_list[R][Y][X])

/*
 * Set feature on a square
 */
#define set_feat_bold(X, Y, F) \
	(cave_p(X, Y)->feat=(F))

/*
 * Grid-based version of the above
 */
#define set_feat_grid(C, F) \
	((C)->feat=(F))

/* Helpful macros */
#define place_secret_door(X,Y) \
	set_feat_bold((X), (Y), (FEAT_SECRET))


/* Externs */
extern bool new_player_spot(void);

extern void place_random_stairs(int x, int y);
extern void place_random_door(int x, int y);

extern void vault_objects(int x, int y, int num);
extern void vault_traps(int x, int y, int xd, int yd, int num);
extern void vault_monsters(int x1, int y1, int num);

extern int next_to_walls(int x, int y);

extern void generate_room(int x1, int y1, int x2, int y2, int light);
extern void generate_vault(int x1, int y1, int x2, int y2);
extern void clear_vault(int x1, int y1, int x2, int y2);
extern void generate_fill(int x1, int y1, int x2, int y2, int feat);
extern void generate_draw(int x1, int y1, int x2, int y2, int feat);
extern void generate_line(int x1, int y1, int x2, int y2, int feat);
extern void generate_plus(int x1, int y1, int x2, int y2, int feat);
extern void generate_door(int x1, int y1, int x2, int y2, bool secret);


extern bool get_is_floor(int x, int y);
extern void set_floor(int x, int y);

extern void build_tunnel(int col1, int row1, int col2, int row2);
extern bool build_tunnel2(int x1, int y1, int x2, int y2, int type, int cutoff);

extern void generate_hmap(int x0, int y0, int xsiz, int ysiz, int grd,
						  int roug, int cutoff);
extern bool generate_fracave(int x0, int y0, int xsize, int ysize, int cutoff,
							 bool light);
extern bool generate_lake(int x0, int y0, int xsize, int ysize,
						  int c1, int c2, int c3,
						  byte f1, byte f2, byte f3);
