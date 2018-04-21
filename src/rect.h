#ifndef INCLUDED_RECT_H
#define INCLUDED_RECT_H

#include "h-basic.h"

struct point_s
{
    int x, y;
};
typedef struct point_s point_t;

struct rect_s
{
    int  x,  y;
    int cx, cy;
};
typedef struct rect_s rect_t;

extern point_t point_create(int x, int y);
extern point_t point_add(point_t p1, point_t p2);
extern point_t point_subtract(point_t p1, point_t p2);
extern int point_compare(point_t p1, point_t p2);

extern point_t rect_topleft(const rect_t *r);
extern point_t rect_center(const rect_t *r);
extern rect_t rect_create(int x, int y, int cx, int cy);
extern bool rect_is_valid(const rect_t *r);
extern bool rect_contains_pt(const rect_t *r, int x, int y);
extern bool rect_contains(const rect_t *r1, const rect_t *r2);
extern rect_t rect_intersect(const rect_t *r1, const rect_t *r2);
extern rect_t rect_translate(const rect_t *r, int dx, int dy);
extern int rect_area(const rect_t *r);

#endif
