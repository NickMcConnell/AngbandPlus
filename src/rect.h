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

extern point_t point(int x, int y);
extern point_t point_add(point_t p1, point_t p2);
extern point_t point_subtract(point_t p1, point_t p2);
extern int     point_compare(point_t p1, point_t p2);

extern point_t size(int cx, int cy);

extern rect_t  rect(int x, int y, int cx, int cy);
extern rect_t  rect_invalid(void);
extern point_t rect_topleft(rect_t r);
extern point_t rect_center(rect_t r);
extern bool    rect_is_valid(rect_t r);
extern bool    rect_contains_pt(rect_t r, int x, int y);
extern bool    rect_contains(rect_t r1, rect_t r2);
extern rect_t  rect_intersect(rect_t r1, rect_t r2);
extern rect_t  rect_translate(rect_t r, int dx, int dy);
extern int     rect_area(rect_t r);

extern int     interpolate(int x, point_ptr tbl, int ct);

#endif
