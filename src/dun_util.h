#ifndef INCLUDED_DUN_UTIL_H
#define INCLUDED_DUN_UTIL_H

#include "rect.h"

/* fractal height map */
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
extern int          dun_frac_get(dun_frac_ptr frac, point_t pos);
extern void         dun_frac_set(dun_frac_ptr frac, point_t pos, byte h);
extern void         dun_frac_save(dun_frac_ptr frac, savefile_ptr file);
extern dun_frac_ptr dun_frac_load(savefile_ptr file);
extern void         dun_frac_dump(dun_frac_ptr frac);

/* store 8bits extra info per dungeon grid */
typedef struct dun_byte_s dun_byte_t, *dun_byte_ptr;
struct dun_byte_s
{
    rect_t rect;
    byte  *map;
    byte   init; /* default value after alloc and clear */
};
extern dun_byte_ptr dun_byte_alloc(rect_t rect);
extern dun_byte_ptr dun_byte_alloc_aux(rect_t rect, byte init);
extern void         dun_byte_free(dun_byte_ptr map);
extern void         dun_byte_clear(dun_byte_ptr map);
extern byte         dun_byte_get(dun_byte_ptr map, point_t pos);
extern void         dun_byte_set(dun_byte_ptr map, point_t pos, byte val);

/* helper for dun_update_lite: store lighting levels around plr
 * lighting levels are small signed integers (so negative => dark)
 * but we'll store things in bytes (offset ... use _get|set()) */
typedef struct dun_light_s dun_light_t, *dun_light_ptr;
struct dun_light_s
{
    dun_ptr dun;
    rect_t  rect;
    byte   *map;
};
extern dun_light_ptr dun_light_alloc(dun_ptr dun, rect_t rect);
extern void         dun_light_free(dun_light_ptr map);
extern void         dun_light_clear(dun_light_ptr map);
extern int          dun_light_get(dun_light_ptr map, point_t pos);
extern void         dun_light_set(dun_light_ptr map, point_t pos, int light);
extern void         dun_light_add(dun_light_ptr map, point_t pos, int light);
extern void         dun_light_iter(dun_light_ptr map, void (*f)(dun_ptr dun, point_t pos, int light));
extern void         dun_light_copy(dun_light_ptr src, dun_light_ptr dest);
extern rect_t       dun_light_bounding_rect(dun_light_ptr map);

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
    dun_ptr dun;
    rect_t  rect;
    u32b   *bits;
};
extern dun_bmp_ptr dun_bmp_alloc(dun_ptr dun, rect_t rect);
extern void        dun_bmp_free(dun_bmp_ptr map);
extern void        dun_bmp_clear(dun_bmp_ptr map);
extern int         dun_bmp_count(dun_bmp_ptr map);
extern bool        dun_bmp_test(dun_bmp_ptr map, point_t pos);
extern void        dun_bmp_set(dun_bmp_ptr map, point_t pos);
extern void        dun_bmp_unset(dun_bmp_ptr map, point_t pos);
extern void        dun_bmp_union(dun_bmp_ptr l, dun_bmp_ptr r); /* l |= r */
extern void        dun_bmp_difference(dun_bmp_ptr l, dun_bmp_ptr r); /* l &= ~r */
extern void        dun_bmp_intersection(dun_bmp_ptr l, dun_bmp_ptr r); /* l &= r */
extern void        dun_bmp_iter(dun_bmp_ptr map, void (*f)(dun_ptr dun, point_t pos));
extern rect_t      dun_bmp_bounding_rect(dun_bmp_ptr map);
#endif
