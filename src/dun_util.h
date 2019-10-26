#ifndef INCLUDED_DUN_UTIL_H
#define INCLUDED_DUN_UTIL_H

#include "rect.h"

/* fractal height map */
typedef struct dun_frac_s dun_frac_t, *dun_frac_ptr;
struct dun_frac_s
{
    rect_t rect;
    byte   max;
    byte   rough;
    byte   cutoff;
    int    grid;
    int  (*perturb_f)(dun_frac_ptr frac, int scale);
    int  (*init_f)(dun_frac_ptr frac);
    byte  *map;
};
extern dun_frac_ptr dun_frac_alloc(rect_t rect, int max);
extern void         dun_frac_free(dun_frac_ptr frac);
extern void         dun_frac_calc_aux(dun_frac_ptr frac, int x1, int y1, int x2, int y2);
extern void         dun_frac_calc(dun_frac_ptr frac);
extern void         dun_frac_reset(dun_frac_ptr frac);
extern byte         dun_frac_get(dun_frac_ptr frac, point_t pos);
extern void         dun_frac_set(dun_frac_ptr frac, point_t pos, byte h);

/* store 32bits extra info per dungeon grid
 * use for wilderness seeds or if you want a bunch of extra flags
 * for dungeon grids */
typedef struct dun_u32b_s dun_u32b_t, *dun_u32b_ptr;
struct dun_u32b_s
{
    rect_t rect;
    u32b  *map;
};
extern dun_u32b_ptr dun_u32b_alloc(rect_t rect);
extern void         dun_u32b_free(dun_u32b_ptr map);
extern void         dun_u32b_clear(dun_u32b_ptr map);
extern u32b         dun_u32b_get(dun_u32b_ptr map, point_t pos);
extern u32b_ptr     dun_u32b_get_ptr(dun_u32b_ptr map, point_t pos);
extern void         dun_u32b_set(dun_u32b_ptr map, point_t pos, u32b val);

/* store a single bit per dungeon grid */
typedef struct dun_bmp_s dun_bmp_t, *dun_bmp_ptr;
struct dun_bmp_s
{
    rect_t rect;
    u32b  *bits;
};
extern dun_bmp_ptr dun_bmp_alloc(rect_t rect);
extern void        dun_bmp_free(dun_bmp_ptr map);
extern void        dun_bmp_clear(dun_bmp_ptr map);
extern bool        dun_bmp_test(dun_bmp_ptr map, point_t pos);
extern void        dun_bmp_set(dun_bmp_ptr map, point_t pos);
extern void        dun_bmp_unset(dun_bmp_ptr map, point_t pos);
extern void        dun_bmp_iter(dun_bmp_ptr map, void (*f)(point_t pos));
#endif
