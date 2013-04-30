/*
 * File: wilderness.h
 * Purpose: Wilderness interface
 */

#ifndef WILDERNESS_H
#define WILDERNESS_H

#include "cave.h"

extern int world_index(int world_x, int world_y);
extern void init_wild_info(void);
extern int determine_wilderness_type(int depth);
extern bool wilderness_gen(struct cave *c, struct player *p);
extern void wild_add_monster(struct player *p, struct cave *c);
extern void wild_cat_depth(int depth, char *buf, int len);
extern bool wild_is_explored(struct player *p, int idx);
extern void wild_set_explored(struct player *p, int idx);
extern void wild_add_crop(int depth, int x, int y, int type);

#endif /* WILDERNESS_H */
