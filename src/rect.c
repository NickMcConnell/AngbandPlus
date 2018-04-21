#include "rect.h"

point_t point_create(int x, int y)
{
    point_t result;
    result.x = x;
    result.y = y;
    return result;
}
point_t point_add(point_t p1, point_t p2)
{
    return point_create(
        p1.x + p2.x,
        p1.y + p2.y
    );
}
point_t point_subtract(point_t p1, point_t p2)
{
    return point_create(
        p1.x - p2.x,
        p1.y - p2.y
    );
}
int point_compare(point_t p1, point_t p2)
{
    if (p1.y < p2.y)
        return -1;
    if (p1.y > p2.y)
        return 1;
    if (p1.x < p2.x)
        return -1;
    if (p1.x > p2.x)
        return 1;
    return 0;
}

/* Trivial rectangle utility to make code a bit more readable */
point_t rect_topleft(const rect_t *r)
{
    return point_create(r->x, r->y);
}

point_t rect_center(const rect_t *r)
{
    return point_create(r->x + r->cx/2, r->y + r->cy/2);
}

rect_t rect_create(int x, int y, int cx, int cy)
{
    /* rect_t r = { x, y, cx, cy }; is not ANSI legal C ...
       You can only initialize with constants and statics, or now with
       rect_t r = rect_create(x, y, cx, cy); */
    rect_t result;
    result.x = x;
    result.y = y;
    result.cx = cx;
    result.cy = cy;
    return result;
}

bool rect_is_valid(const rect_t *r)
{
    return r->cx > 0 && r->cy > 0;
}

bool rect_contains_pt(const rect_t *r, int x, int y)
{
    return rect_is_valid(r)
        && (r->x <= x && x < r->x + r->cx)
        && (r->y <= y && y < r->y + r->cy);
}

bool rect_contains(const rect_t *r1, const rect_t *r2)
{
    return rect_is_valid(r1)
        && rect_is_valid(r2)
        && rect_contains_pt(r1, r2->x, r2->y)
        && rect_contains_pt(r1, r2->x + r2->cx - 1, r2->y + r2->cy - 1);
}

rect_t rect_intersect(const rect_t *r1, const rect_t *r2)
{
    rect_t result = {0};

    if (rect_is_valid(r1) && rect_is_valid(r2))
    {
        int     left = MAX(r1->x, r2->x);
        int     right = MIN(r1->x + r1->cx, r2->x + r2->cx);
        int     top = MAX(r1->y, r2->y);
        int     bottom = MIN(r1->y + r1->cy, r2->y + r2->cy);
        int     cx = right - left;
        int     cy = bottom - top;

        if (cx > 0 && cy > 0)
            result = rect_create(left, top, cx, cy);
    }
    return result;
}

rect_t rect_translate(const rect_t *r, int dx, int dy)
{
    rect_t result = {0};
    if (rect_is_valid(r))
        result = rect_create(r->x + dx, r->y + dy, r->cx, r->cy);
    return result;
}

int rect_area(const rect_t *r)
{
    int result = 0;
    if (rect_is_valid(r))
        result = r->cx * r->cy;
    return result;
}

