#ifndef INCLUDED_RECT_H
#define INCLUDED_RECT_H

#include "h-basic.h"

struct point_s
{
    int x, y;
};
typedef struct point_s point_t, *point_ptr;

struct rect_s
{
    int  x,  y;
    int cx, cy;
};
typedef struct rect_s rect_t, *rect_ptr;

extern point_t point_create(int x, int y);
extern point_t point_add(point_t p1, point_t p2);
extern point_t point_step(point_t p1, int dir);   /* take a single step in a direction */
extern point_t point_jump(point_t p1, int dir, int ct); /* take multiple steps in a direction */
extern point_t point_subtract(point_t p1, point_t p2);
extern int     point_fast_distance(point_t p1, point_t p2);
extern int     point_step_dir(point_t p1, point_t p2); /* returns d such that point_step(p1, d) == p2 or 5 on error */
extern int     point_compare(point_t p1, point_t p2);
extern bool    point_equals(point_t p1, point_t p2);
extern point_t point_random_jump(point_t p, int radius);
extern point_t point_random_step(point_t p);
extern point_t point_random_walk(point_t p, int steps);
extern point_t point_midpoint(point_t p1, point_t p2);
extern point_t point_abs(point_t p);
extern point_t point_perturbed_midpoint_aux(point_t p1, point_t p2, point_t pct);
extern point_t point_perturbed_midpoint(point_t p1, point_t p2);

extern point_t size_create(int cx, int cy);

extern rect_t  rect_create(int x, int y, int cx, int cy);
extern rect_t  rect_create_centered(point_t center, int dx, int dy);
extern rect_t  rect_invalid(void);

extern int     rect_left(rect_t r);
extern int     rect_right(rect_t r);
extern int     rect_top(rect_t r);
extern int     rect_bottom(rect_t r);

/* four-corners */
extern point_t rect_top_left(rect_t r);
extern point_t rect_top_right(rect_t r);
extern point_t rect_bottom_left(rect_t r);
extern point_t rect_bottom_right(rect_t r);

extern point_t rect_center(rect_t r);

/* side mid-points */
extern point_t rect_top_center(rect_t r);
extern point_t rect_bottom_center(rect_t r);
extern point_t rect_left_center(rect_t r);
extern point_t rect_right_center(rect_t r);

extern rect_t  rect_recenter(rect_t r, point_t c);
extern rect_t  rect_deflate(rect_t r, int bx, int by);
extern rect_t  rect_inflate(rect_t r, int bx, int by);
extern bool    rect_is_valid(rect_t r);
extern bool    rect_contains_xy(rect_t r, int x, int y);
extern bool    rect_contains_point(rect_t r, point_t pt);
extern bool    rect_contains(rect_t r1, rect_t r2);
extern bool    rect_equals(rect_t r1, rect_t r2);
extern rect_t  rect_intersect(rect_t r1, rect_t r2);
extern rect_t  rect_translate(rect_t r, int dx, int dy);
extern rect_t  rect_interior(rect_t r);
extern int     rect_area(rect_t r);
extern point_t rect_random_point(rect_t r);

extern int     interpolate(int x, point_ptr tbl, int ct);

typedef struct point_vec_s point_vec_t, *point_vec_ptr;
extern point_vec_ptr  point_vec_alloc(void);
extern void           point_vec_free(point_vec_ptr v);
extern point_t        point_vec_get(point_vec_ptr v, int i);
extern void           point_vec_delete(point_vec_ptr v, int i);
extern void           point_vec_add(point_vec_ptr v, point_t pt);              
extern void           point_vec_clear(point_vec_ptr v);
extern int            point_vec_length(point_vec_ptr v);
extern void           point_vec_push(point_vec_ptr v, point_t pt);              
extern point_t        point_vec_pop(point_vec_ptr v);
extern void           point_vec_shuffle(point_vec_ptr v);
extern void           point_vec_swap(point_vec_ptr v, int i, int j);

typedef struct point_queue_s point_queue_t, *point_queue_ptr;
extern point_queue_ptr point_queue_alloc(void);
extern void            point_queue_free(point_queue_ptr q);
extern void            point_queue_add(point_queue_ptr q, point_t p);
extern point_t         point_queue_remove(point_queue_ptr q);
extern int             point_queue_count(point_queue_ptr q);
#endif
