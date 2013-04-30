/*
 * File: target.h
 * Purpose: Target interface
 */

#ifndef TARGET_H
#define TARGET_H

/* target.c */
extern bool target_able(struct player *p, int m_idx);
extern bool target_okay(struct player *p);
extern void target_set_monster(int Ind, int m_idx);
extern void target_set_location(int Ind, int y, int x);
extern bool target_set_interactive_accept(struct player *p, int y, int x);
extern bool target_set_closest(int Ind, int mode);
extern bool target_set_interactive(int Ind, int mode, u32b query);
extern void target_get(struct player *p, s16b *col, s16b *row);
extern s16b target_get_monster(int Ind);
extern void draw_path_grid(struct player *p, int y, int x, byte a, char c);
extern void flush_path_grid(struct player *p, int depth, int y, int x, byte a, char c);

extern void load_path(struct player *p, u16b path_n, u16b *path_g);
extern int draw_path(struct player *p, u16b path_n, u16b *path_g, int y1, int x1);

#endif /* TARGET_H */
