#include "rect.h"

#include <assert.h>

point_t point(int x, int y)
{
    point_t p;
    p.x = x;
    p.y = y;
    return p;
}
point_t point_add(point_t p1, point_t p2)
{
    return point(
        p1.x + p2.x,
        p1.y + p2.y
    );
}
point_t point_subtract(point_t p1, point_t p2)
{
    return point(
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

point_t size(int cx, int cy)
{
    return point(cx, cy);
}

/* Interpolate a function value based on sample points. Interpolation is
 * linear, of course, but you can approximate highly non-linear functions
 * by adding a suitable number of points. For example, here is a candidate
 * for gaining weapon proficiency where the amount gained decays exponentially
 * with the current skill:
        static point_t tbl[9] = {
            {0, 1280}, {1000, 640}, {2000, 320}, {3000, 160}, {4000, 80},
            {5000, 40}, {6000, 20}, {7000, 10}, {8000, 1} };
        int step = interpolate(skill, tbl, 9);
        skill += step/10;
        if ((step % 10) && randint0(10) < (step % 10))
            skill++;
*/
int interpolate(int x, point_ptr tbl, int ct)
{
    point_t prev, current;
    int     i;

    assert(ct);
    assert(tbl);

    for (i = 0; i < ct; i++)
    {
        current = tbl[i];
        if (x < current.x) break;
        prev = current;
    }
    if (i == 0) return current.y;  /* Out of bounds: x too low for tbl */
    if (i == ct) return current.y; /* Out of bounds: x too high for tbl */
    return prev.y + (x - prev.x)*(current.y - prev.y)/(current.x - prev.x);
}

/* Trivial rectangle utility to make code a bit more readable */
rect_t rect(int x, int y, int cx, int cy)
{
    rect_t r;
    r.x = x;
    r.y = y;
    r.cx = cx;
    r.cy = cy;
    return r;
}

rect_t rect_invalid(void)
{
    return rect(0, 0, 0, 0);
}

point_t rect_topleft(rect_t r)
{
    return point(r.x, r.y);
}

point_t rect_center(rect_t r)
{
    return point(r.x + r.cx/2, r.y + r.cy/2);
}

bool rect_is_valid(rect_t r)
{
    return r.cx > 0 && r.cy > 0;
}

bool rect_contains_pt(rect_t r, int x, int y)
{
    return rect_is_valid(r)
        && (r.x <= x && x < r.x + r.cx)
        && (r.y <= y && y < r.y + r.cy);
}

bool rect_contains(rect_t r1, rect_t r2)
{
    return rect_is_valid(r1)
        && rect_is_valid(r2)
        && rect_contains_pt(r1, r2.x, r2.y)
        && rect_contains_pt(r1, r2.x + r2.cx - 1, r2.y + r2.cy - 1);
}

rect_t rect_intersect(rect_t r1, rect_t r2)
{
    rect_t result = {0};

    if (rect_is_valid(r1) && rect_is_valid(r2))
    {
        int left = MAX(r1.x, r2.x);
        int right = MIN(r1.x + r1.cx, r2.x + r2.cx);
        int top = MAX(r1.y, r2.y);
        int bottom = MIN(r1.y + r1.cy, r2.y + r2.cy);
        int cx = right - left;
        int cy = bottom - top;

        if (cx > 0 && cy > 0)
            result = rect(left, top, cx, cy);
    }
    return result;
}

rect_t rect_translate(rect_t r, int dx, int dy)
{
    rect_t result = {0};
    if (rect_is_valid(r))
        result = rect(r.x + dx, r.y + dy, r.cx, r.cy);
    return result;
}

int rect_area(rect_t r)
{
    int result = 0;
    if (rect_is_valid(r))
        result = r.cx * r.cy;
    return result;
}

