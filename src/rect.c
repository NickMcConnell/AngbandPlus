#include "angband.h" /* ddx and ddy */

#include <assert.h>

/************************************************************************
 * Point
 ************************************************************************/
point_t point_create(int x, int y)
{
    point_t p;
    p.x = x;
    p.y = y;
    return p;
}
point_t point_add(point_t p1, point_t p2)
{
    return point_create(
        p1.x + p2.x,
        p1.y + p2.y
    );
}
point_t point_step(point_t p1, int dir)
{
    point_t p = p1;
    p.x += ddx[dir];
    p.y += ddy[dir];
    return p;
}
point_t point_scale(point_t p, int scale) /* i.e. scaling a vector */
{
    point_t q;
    q.x = p.x * scale;
    q.y = p.y * scale;
    return q;
}
point_t point_scale_Q(point_t p, int num, int denom)
{
    point_t q;
    q.x = (p.x*num + denom - 1)/denom;
    q.y = (p.y*num + denom - 1)/denom;
    return q;
}
int point_norm(point_t p)
{
    point_t o = {0};
    return point_distance(p, o);
}
point_t point_jump(point_t p1, int dir, int ct)
{
    point_t p;
    p.x = p1.x + ct*ddx[dir];
    p.y = p1.y + ct*ddy[dir];
    return p;
}
point_t point_jump_clipped(point_t p1, int dir, int ct, rect_t r)
{
    point_t p = p1;
    int i;
    assert(rect_is_valid(r));
    assert(rect_contains_point(r, p1));
    for (i = 0; i < ct; i++)
    {
        point_t n = point_step(p, dir);
        if (!rect_contains_point(r, n)) break;
        p = n;
    }
    assert(rect_contains_point(r, p));
    return p;
}
int point_step_dir(point_t p1, point_t p2) /* XXX assumes adjacent points */
{
    int i;
    for (i = 0; i < 9; i++)
    {
        point_t p = point_step(p1, ddd[i]);
        if (point_equals(p, p2)) return ddd[i];
    }
    return 0;
}
#define _UP    0x08
#define _RIGHT 0x04
#define _VERT  0x02
#define _HORIZ 0x01
int point_step_dirs(point_t p1, point_t p2, int mm[8]) /* XXX for mon_ai ... need not be adjacent */
{
    point_t v = point_subtract(p2, p1); /* vector from p1 to p2 */
    int ax, ay, dir = 0;

    /* return number of choices computed ... currently 0 or 5 */
    if (v.x == 0 && v.y == 0) return 0;

    /* build a set of numpad directions to try moving from p1 to p2.
     * this code is a slightly altered version of code formerly in get_moves (melee2.c) */
    ax = ABS(v.x);
    ay = ABS(v.y);

    /* we'll keep the bit encoding with the giant switch for now */
    if (v.y < 0) dir |= _UP; /* want to go up (note the TEXT mapping mode where y increases in downward direction) */
    if (v.x > 0) dir |= _RIGHT; /* want to go right */
    if (ay > (ax << 1)) dir |= _VERT; /* bias vertical movement */
    if (ax > (ay << 1)) dir |= _HORIZ; /* bias horizontal movement */

    /* here, there will be one block for each of the 8 keypad directions.
     * pure diagonals (9, 7, 1, 3) lack a bias. Note that _LEFT simply means
     * _RIGHT was not set and _DOWN that _UP was not set. */
    switch (dir)
    {
    case 12: /* _RIGHT | _UP with no bias to horizontal or vertical */
        mm[0] = 9;
        if (ay > ax) /* favor up */
        {
            mm[1] = 8;
            mm[2] = 6;
            mm[3] = 7;
            mm[4] = 3;
        }
        else /* favor right */
        {
            mm[1] = 6;
            mm[2] = 8;
            mm[3] = 3;
            mm[4] = 7;
        }
        break;
    case 13: /* _UP | _RIGHT | _HORIZ */
    case 5: /* _DOWN | _RIGHT | _HORIZ */
        mm[0] = 6; /* _RIGHT | _HORIZ means ignore dy and go right */
        if (v.y > 0) /* then down */
        {
            mm[1] = 3;
            mm[2] = 9;
            mm[3] = 2;
            mm[4] = 8;
        }
        else /* then up */
        {
            mm[1] = 9;
            mm[2] = 3;
            mm[3] = 8;
            mm[4] = 2;
        }
        break;
    case 10: /* _UP | _LEFT | _VERT */
    case 14: /* _UP | _RIGHT | _VERT */
        mm[0] = 8; /* _UP | _VERT means ignore dx and go up */
        if (v.x > 0) /* then right */
        {
            mm[1] = 9;
            mm[2] = 7;
            mm[3] = 6;
            mm[4] = 4;
        }
        else /* then left */
        {
            mm[1] = 7;
            mm[2] = 9;
            mm[3] = 4;
            mm[4] = 6;
        }
        break;
    case 8: /* _UP | _LEFT with no horizontal or vertical bias */
        mm[0] = 7;
        if (ay > ax) /* favor up */
        {
            mm[1] = 8;
            mm[2] = 4;
            mm[3] = 9;
            mm[4] = 1;
        }
        else /* favor left */
        {
            mm[1] = 4;
            mm[2] = 8;
            mm[3] = 1;
            mm[4] = 9;
        }
        break;
    case 9: /* _UP | _LEFT | _HORIZ */
    case 1: /* _DOWN | _LEFT | _HORIZ */
        mm[0] = 4; /* _LEFT | _HORIZ means ignore dy and go left */
        if (v.y > 0) /* then down */
        {
            mm[1] = 1;
            mm[2] = 7;
            mm[3] = 2;
            mm[4] = 8;
        }
        else /* then up */
        {
            mm[1] = 7;
            mm[2] = 1;
            mm[3] = 8;
            mm[4] = 2;
        }
        break;
    case 4: /* _DOWN | _RIGHT with no vertical or horizontal bias */
        mm[0] = 3;
        if (ay > ax) /* favor down */
        {
            mm[1] = 2;
            mm[2] = 6;
            mm[3] = 1;
            mm[4] = 9;
        }
        else /* favor right */
        {
            mm[1] = 6;
            mm[2] = 2;
            mm[3] = 9;
            mm[4] = 1;
        }
        break;
    case 2: /* _DOWN | _LEFT | _VERT */
    case 6: /* _DOWN | _RIGHT | _VERT */
        mm[0] = 2; /* _DOWN | _VERT means ignore dx and go down */
        if (v.x > 0) /* then right */
        {
            mm[1] = 3;
            mm[2] = 1;
            mm[3] = 6;
            mm[4] = 4;
        }
        else /* then left */
        {
            mm[1] = 1;
            mm[2] = 3;
            mm[3] = 4;
            mm[4] = 6;
        }
        break;
    case 0:  /* _DOWN | _LEFT with no horizontal or vertical bias */
        mm[0] = 1;
        if (ay > ax) /* favor down */
        {
            mm[1] = 2;
            mm[2] = 4;
            mm[3] = 3;
            mm[4] = 7;
        }
        else /* favor left */
        {
            mm[1] = 4;
            mm[2] = 2;
            mm[3] = 7;
            mm[4] = 3;
        }
        break;
    }
    return 5;
}
point_t point_random_jump(point_t p, int rng)
{
    return point_create(rand_spread(p.x, rng), rand_spread(p.y, rng));
}
point_t point_random_step(point_t p)
{
    return point_step(p, ddd[randint0(8)]);
}
point_t point_random_walk(point_t start, int steps)
{
    point_t pos = start;
    int i;
    for (i = 0; i < steps; i++)
        pos = point_random_step(pos);
    return pos;
}
point_t point_subtract(point_t p1, point_t p2)
{
    return point_create(
        p1.x - p2.x,
        p1.y - p2.y
    );
}
int point_fast_distance(point_t p1, point_t p2)
{
    point_t p = point_subtract(p1, p2);
    int     dx = ABS(p.x);
    int     dy = ABS(p.y);
    int     d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

    if (d > 255) d = 255;
    if (!d) d = 1;
    
    return d;
}
point_t point_midpoint(point_t p1, point_t p2)
{
    return point_create((p1.x + p2.x)/2, (p1.y + p2.y)/2);
}
point_t point_abs(point_t p)
{
    return point_create(abs(p.x), abs(p.y));
}
point_t point_sign(point_t p)
{
    return point_create(SGN(p.x), SGN(p.y));
}
point_t point_perturbed_midpoint_aux(point_t p1, point_t p2, point_t pct)
{
    point_t mp = point_midpoint(p1, p2);
    point_t v = point_abs(point_subtract(mp, p1));
    point_t p = {0};
    if (v.y) p.x = rand_range(-v.y, v.y) * pct.x / 100;
    if (v.x) p.y = rand_range(-v.x, v.x) * pct.y / 100;
    return point_add(mp, p);
}
point_t point_perturbed_midpoint(point_t p1, point_t p2)
{
    return point_perturbed_midpoint_aux(p1, p2, point_create(100, 100));
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
bool point_equals(point_t p1, point_t p2)
{
    return p1.x == p2.x && p1.y == p2.y;
}
bool point_is_adjacent(point_t p1, point_t p2)
{
    int dx = p2.x - p1.x;
    int dy = p2.y - p1.y;
    return (-2 < dx && dx < 2)
        && (-2 < dy && dy < 2)
        && (dx || dy);
}
point_t size_create(int cx, int cy)
{
    return point_create(cx, cy);
}

/************************************************************************
 * Interpolation
 ************************************************************************/
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
    point_t prev, current = {0};
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

/************************************************************************
 * Rect
 ************************************************************************/
rect_t rect_create(int x, int y, int cx, int cy)
{
    rect_t r;
    r.x = x;
    r.y = y;
    r.cx = cx;
    r.cy = cy;
    return r;
}
rect_t rect_create_centered(point_t center, int dx, int dy)
{
    int left = center.x - dx;
    int right = center.x + dx;
    int top = center.y - dy;
    int bottom = center.y + dy;
    return rect_create(left, top, right - left + 1, bottom - top + 1);
}
rect_t rect_invalid(void)
{
    return rect_create(0, 0, 0, 0);
}
int rect_left(rect_t r)
{
    assert(rect_is_valid(r));
    return r.x;
}
int rect_right(rect_t r)
{
    assert(rect_is_valid(r));
    return r.x + r.cx - 1;
}
int rect_top(rect_t r)
{
    assert(rect_is_valid(r));
    return r.y;
}
int rect_bottom(rect_t r)
{
    assert(rect_is_valid(r));
    return r.y + r.cy - 1;
}
point_t rect_top_left(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x, r.y);
}
point_t rect_top_right(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x + r.cx - 1, r.y);
}
point_t rect_bottom_right(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x + r.cx - 1, r.y + r.cy - 1);
}
point_t rect_bottom_left(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x, r.y + r.cy - 1);
}
point_t rect_center(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x + r.cx/2, r.y + r.cy/2);
}
point_t rect_top_center(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x + r.cx/2, r.y);
}
point_t rect_bottom_center(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x + r.cx/2, r.y + r.cy - 1);
}
point_t rect_left_center(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x, r.y + r.cy/2);
}
point_t rect_right_center(rect_t r)
{
    assert(rect_is_valid(r));
    return point_create(r.x + r.cx - 1, r.y + r.cy/2);
}
rect_t rect_recenter(rect_t r, point_t c)
{
    point_t oc = rect_center(r);
    point_t v = point_subtract(c, oc);
    return rect_translate(r, v);
}
rect_t rect_deflate(rect_t r, int bx, int by)
{
    assert(rect_is_valid(r));
    return rect_create(r.x + bx, r.y + by, r.cx - 2*bx, r.cy - 2*by);
}
rect_t rect_inflate(rect_t r, int bx, int by)
{
    assert(rect_is_valid(r));
    return rect_create(r.x - bx, r.y - by, r.cx + 2*bx, r.cy + 2*by);
}
bool rect_is_valid(rect_t r)
{
    return r.cx > 0 && r.cy > 0;
}
bool rect_contains_xy(rect_t r, int x, int y)
{
    return rect_is_valid(r)
        && (r.x <= x && x < r.x + r.cx)
        && (r.y <= y && y < r.y + r.cy);
}
bool rect_contains_point(rect_t r, point_t pt)
{
    return rect_is_valid(r)
        && (r.x <= pt.x && pt.x < r.x + r.cx)
        && (r.y <= pt.y && pt.y < r.y + r.cy);
}
point_t rect_first(rect_t r)
{
    return rect_top_left(r);
}
point_t rect_next(rect_t r, point_t p)
{
    if (p.x < rect_right(r)) p.x++;
    else
    {
        p.x = rect_left(r);
        p.y++;
    }
    return p;
}
bool rect_contains(rect_t r1, rect_t r2)
{
    return rect_is_valid(r1)
        && rect_is_valid(r2)
        && rect_contains_xy(r1, r2.x, r2.y)
        && rect_contains_xy(r1, r2.x + r2.cx - 1, r2.y + r2.cy - 1);
}
bool rect_equals(rect_t r1, rect_t r2)
{
    return r1.x == r2.x && r1.y == r2.y && r1.cx == r2.cx && r1.cy == r2.cy;
}

rect_t rect_intersect(rect_t r1, rect_t r2)
{
    rect_t result = {0};

    if (rect_is_valid(r1) && rect_is_valid(r2))
    {
        int left = MAX(r1.x, r2.x);
        int right = MIN(r1.x + r1.cx - 1, r2.x + r2.cx - 1);
        int top = MAX(r1.y, r2.y);
        int bottom = MIN(r1.y + r1.cy - 1, r2.y + r2.cy - 1);
        int cx = right - left + 1;
        int cy = bottom - top + 1;

        if (cx > 0 && cy > 0)
            result = rect_create(left, top, cx, cy);
    }
    return result;
}
rect_t rect_union(rect_t r1, rect_t r2)  /* return bounding rect of r1 U r2 */
{
    rect_t result = {0};

    if (rect_is_valid(r1) && rect_is_valid(r2))
    {
        int left = MIN(r1.x, r2.x);
        int right = MAX(r1.x + r1.cx - 1, r2.x + r2.cx - 1);
        int top = MIN(r1.y, r2.y);
        int bottom = MAX(r1.y + r1.cy - 1, r2.y + r2.cy - 1);
        int cx = right - left + 1;
        int cy = bottom - top + 1;

        if (cx > 0 && cy > 0)
            result = rect_create(left, top, cx, cy);
    }
    return result;
}

rect_t rect_translate_xy(rect_t r, int dx, int dy)
{
    rect_t result = {0};
    if (rect_is_valid(r))
        result = rect_create(r.x + dx, r.y + dy, r.cx, r.cy);
    return result;
}
rect_t rect_translate(rect_t r, point_t v)
{
    rect_t result = {0};
    if (rect_is_valid(r))
        result = rect_create(r.x + v.x, r.y + v.y, r.cx, r.cy);
    return result;
}

int rect_area(rect_t r)
{
    int result = 0;
    if (rect_is_valid(r))
        result = r.cx * r.cy;
    return result;
}

rect_t rect_interior(rect_t r)
{
    rect_t result = {0};
    if (rect_is_valid(r) && r.cx > 2 && r.cy > 2)
        result = rect_create(r.x + 1, r.y + 1, r.cx - 2, r.cy - 2);
    return result;
}

point_t rect_random_point(rect_t r)
{
    int left = r.x;
    int right = r.x + r.cx - 1;
    int top = r.y;
    int bottom = r.y + r.cy - 1;

    assert(rect_is_valid(r));
    return point_create(rand_range(left, right), rand_range(top, bottom));
}

/************************************************************************
 * Triangle
 ************************************************************************/
triangle_t triangle_create(point_t a, point_t b, point_t c)
{
    triangle_t t;
    t.a = a;
    t.b = b;
    t.c = c;
    return t;
}
rect_t triangle_bounding_rect(triangle_t t)
{
    int left = MIN(MIN(t.a.x, t.b.x), t.c.x);
    int right = MAX(MAX(t.a.x, t.b.x), t.c.x);
    int top = MIN(MIN(t.a.y, t.b.y), t.c.y);
    int bottom = MAX(MAX(t.a.y, t.b.y), t.c.y);
    int cx = right - left + 1;
    int cy = bottom - top + 1;

    if (cx > 0 && cy > 0)
        return rect_create(left, top, cx, cy);
    return rect_invalid();
}
static int _cross_product(point_t v1, point_t v2)
{
    return v1.x*v2.y - v1.y*v2.x;
}
static int _orientation(point_t p, line_t l)
{
    point_t pa = point_subtract(l.a, p);
    point_t pb = point_subtract(l.b, p);
    int     z = _cross_product(pa, pb);
    return SGN(z);
}
bool triangle_is_valid(triangle_t t)
{
    /* XXX non-zero area by determinant expansion? */
    /* 3 distinct vertices */
    if (point_equals(t.a, t.b) || point_equals(t.a, t.c) || point_equals(t.b, t.c)) return FALSE;
    /* not a straight line */
    return _orientation(t.a, line_create(t.b, t.c)) != 0;
}

/************************************************************************
 * Line (Segment)
 ************************************************************************/
line_t line_create(point_t a, point_t b)
{
    line_t l;
    l.a = a;
    l.b = b;
    return l;
}
line_t line_create_xy(int ax, int ay, int bx, int by)
{
    line_t l;
    l.a.x = ax;
    l.a.y = ay;
    l.b.x = bx;
    l.b.y = by;
    return l;
}
bool line_is_valid(line_t l)
{
    return !point_equals(l.a, l.b);
}
rect_t line_bounding_rect(line_t l)
{
    assert(line_is_valid(l));
    if (line_is_valid(l))
    {
        int left = MIN(l.a.x, l.b.x);
        int right = MAX(l.a.x, l.b.x);
        int top = MIN(l.a.y, l.b.y);
        int bottom = MAX(l.a.y, l.b.y);
        int cx = right - left + 1;
        int cy = bottom - top + 1;

        if (cx > 0 && cy > 0)
            return rect_create(left, top, cx, cy);
    }
    return rect_invalid();
}

/* Line Generator: Spits out points along line segment, starting with a,
 * and continuing as long as you ask for more points. This is useful for
 * extending an existing line segment (cf PROJECT_THRU) */
line_gen_ptr line_gen_alloc(line_t l)
{
    line_gen_ptr gen = malloc(sizeof(line_gen_t));
    line_gen_create(gen, l);
    return gen;
}
void line_gen_create(line_gen_ptr gen, line_t l)
{
    assert(line_is_valid(l));
    gen->l = l;
    gen->v = point_subtract(l.b, l.a); /* vector from a to b */
    gen->a = point_abs(gen->v);
    gen->s = point_sign(gen->v);

    gen->half = gen->a.y * gen->a.x; /* Number of "units" in one "half" grid */
    gen->full = gen->half << 1; /* Number of "units" in one "full" grid */

    gen->m = gen->frac = gen->n = gen->k = 0;
    gen->p.x = gen->p.y = 0;
}
void line_gen_destroy(line_gen_ptr gen)
{
}
void line_gen_free(line_gen_ptr gen)
{
    if (!gen) return;
    line_gen_destroy(gen);
    free(gen);
}
point_t line_gen_first(line_gen_ptr gen)
{
    gen->n = 1;
    if (gen->a.y > gen->a.x) /* Vertical */
    {
        gen->m = gen->a.x * gen->a.x * 2; /* Let m = ((dx/dy) * full) = (dx * dx * 2) */
        gen->p.y = gen->l.a.y + gen->s.y;
        gen->p.x = gen->l.a.x;
        gen->frac = gen->m;
        if (gen->frac > gen->half)
        {
            gen->p.x += gen->s.x;
            gen->frac -= gen->full;
            gen->k++;
        }
    }
    else if (gen->a.x > gen->a.y) /* Horizontal */
    {
        gen->m = gen->a.y * gen->a.y * 2; /* Let m = ((dy/dx) * full) = (dy * dy * 2) */
        gen->p.x = gen->l.a.x + gen->s.x;
        gen->p.y = gen->l.a.y;
        gen->frac = gen->m;
        if (gen->frac > gen->half)
        {
            gen->p.y += gen->s.y;
            gen->frac -= gen->full;
            gen->k++;
        }
    }
    else /* Diagonal */
    {
        gen->p.x = gen->l.a.x + gen->s.x;
        gen->p.y = gen->l.a.y + gen->s.y;
        gen->k++;
    }
    return gen->l.a;
}
point_t line_gen_next(line_gen_ptr gen)
{
    point_t last = gen->p;
    gen->n++;
    if (gen->a.y > gen->a.x) /* Vertical */
    {
        gen->p.y += gen->s.y;
        if (gen->m)
        {
            gen->frac += gen->m;
            if (gen->frac > gen->half)
            {
                gen->p.x += gen->s.x;
                gen->frac -= gen->full;
                gen->k++;
            }
        }
    }
    else if (gen->a.x > gen->a.y) /* Horizontal */
    {
        gen->p.x += gen->s.x;
        if (gen->m)
        {
            gen->frac += gen->m;
            if (gen->frac > gen->half)
            {
                gen->p.y += gen->s.y;
                gen->frac -= gen->full;
                gen->k++;
            }
        }
    }
    else /* Diagonal */
    {
        gen->p.x += gen->s.x;
        gen->p.y += gen->s.y;
        gen->k++;
    }
    return last;
}
int line_gen_distance(line_gen_ptr gen)
{
    /*assert(point_fast_distance(gen->p, gen->l.a) == gen->n + (gen->k >> 1));*/
    return gen->n + (gen->k >> 1);
}
/* based on project_path_aux ... _los_aux might yield a different point set
 * iteration can optionally omit the starting point (e.g. compute "projectability")
 * iterating from a->b will generally yield a different point set then b->a
 * iteration stops if "f" returns false
 * iteration is successful if all points from a->b are visited and "accepted" by "f"
 */
bool line_iter(line_t l, bool (*f)(point_t p)) { return line_iter_aux(l, f, FALSE); }
extern bool line_iter_aux(line_t l, bool (*f)(point_t p), bool skip_first)
{
    line_gen_t gen;
    rect_t r;
    point_t p;
    bool quit = FALSE;
    
    if (!line_is_valid(l)) return FALSE;
    line_gen_create(&gen, l);
    r = line_bounding_rect(l); /* defensive programming */
    p = line_gen_first(&gen);
    if (!skip_first && !f(p)) quit = TRUE;
    while (!quit)
    {
        p = line_gen_next(&gen);
        assert(rect_contains_point(r, p));
        if (!rect_contains_point(r, p)) quit = TRUE;
        else if (!f(p)) quit = TRUE;
        else if (point_equals(p, l.b)) break;
    }
    line_gen_destroy(&gen);
    return !quit;
}
line_t line_extend(line_t l, int d)
{
    line_gen_t gen;
    line_t nl = {{0}};
    
    if (!line_is_valid(l)) return l;
    line_gen_create(&gen, l);
    nl.a = line_gen_first(&gen);
    for (;;)
    {
        nl.b = line_gen_next(&gen);
        if (point_distance(nl.a, nl.b) >= d) break;
    }
    line_gen_destroy(&gen);
    return nl;
}
static point_vec_ptr _pv;
static bool _accumulate(point_t p)
{
    assert(_pv);
    point_vec_push(_pv, p);
    return TRUE;
}
point_vec_ptr line_points(line_t l) { return line_points_aux(l, FALSE); }
point_vec_ptr line_points_aux(line_t l, bool skip_first)
{
    _pv = point_vec_alloc();
    line_iter_aux(l, _accumulate, skip_first);
    return _pv;
}
line_t line_invalid(void)
{
    return line_create_xy(0, 0, 0, 0);
}
line_t line_clip(line_t l, rect_t r)
{
    point_vec_ptr pv;
    line_t nl = {{0}};
    bool enter = FALSE;
    bool exit = FALSE;
    int i;
    if (!line_is_valid(l)) return l;
    if (!rect_is_valid(r)) return line_invalid();
    if (rect_contains_point(r, l.a) && rect_contains_point(r, l.b)) return l; /* optimism */

    /* at least one of the line endpoints is outside of r. walk the
     * line and notice if/when we enter and/or exit the region */
    pv = line_points(l);
    for (i = 0; i < point_vec_length(pv); i++)
    {
        point_t p = point_vec_get(pv, i);
        if (!enter && rect_contains_point(r, p))
        {
            nl.a = p;
            enter = TRUE;
        }
        else if (enter && !rect_contains_point(r, p))
        {
            nl.b = p;
            exit = TRUE;
            break; /* we can stop now */
        }
    }
    if (enter && !exit)
        nl.b = l.b;
    point_vec_free(pv);
    return nl;
}
int line_length(line_t l)
{
    if (!line_is_valid(l)) return 0;
    return point_distance(l.a, l.b);
}
line_t line_translate(line_t l, point_t v)
{
    if (!line_is_valid(l)) return l;
    return line_create(point_add(l.a, v), point_add(l.b, v));
}
static point_t _rotate(point_t v) /* counterclockwise rotation by pi/2 */
{
    /* r: (0 -1)(x)=(-y)
     *    (1  0)(y) ( x) */
    return point_create(-v.y, v.x);
}
static int _dot_product(point_t v1, point_t v2)
{
    return v1.x * v2.x + v1.y * v2.y;
}
static int _norm(point_t v)
{
    point_t o = {0};
    return point_distance(v, o);
}
int point_distance_to_line(point_t p, line_t l)
{
    assert(line_is_valid(l));
    if (line_is_valid(l))
    {
        point_t pa = point_subtract(l.a, p); /* vector from p to l.a */
        point_t lv = point_subtract(l.b, l.a); /* line vector: l.a to l.b */
        point_t nv = _rotate(lv); /* normal vector: rotate lv by pi/2 */
        int nd = _norm(nv);

        assert(nd);
        if (!nd) return 0;
        /* projecting on to unit normal vector gives length of normal component of pa */
        return ABS(_dot_product(pa, nv) / nd);
    } 
    return 0;
}

/************************************************************************
 * Point Vector
 ************************************************************************/
struct point_vec_s
{
    int count;
    int allocated;
    point_ptr points;
};

point_vec_ptr point_vec_alloc(void)
{
    point_vec_ptr v = malloc(sizeof(point_vec_t));
    v->count = 0;
    v->allocated = 0;
    v->points = NULL;
    return v;
}
void point_vec_free(point_vec_ptr v)
{
    if (!v) return;
    if (v->points) free(v->points);
    free(v);
}
point_t point_vec_get(point_vec_ptr v, int i)
{
    assert(0 <= i && i < v->count);
    return v->points[i];
}
void point_vec_delete(point_vec_ptr v, int i)
{
    int j;
    assert(0 <= i && i < v->count);
    for (j = i; j < v->count - 1; j++)
        v->points[j] = v->points[j+1];
    v->count--;
}
static void _grow(point_vec_ptr v, int n)
{
    if (n > v->allocated)
    {
        point_ptr old_pts = v->points;
        if (!v->allocated) v->allocated = 10;
        else v->allocated *= 2;
        if (v->allocated < n) v->allocated = n;
        v->points = malloc(v->allocated * sizeof(point_t));
        if (old_pts)
            memcpy(v->points, old_pts, v->count*sizeof(point_t));
        free(old_pts);
    }
}
void point_vec_add(point_vec_ptr v, point_t pt) { point_vec_push(v, pt); }
void point_vec_push(point_vec_ptr v, point_t pt)
{
    int i = v->count;
    if (i >= v->allocated) _grow(v, i+1);
    v->count++;
    v->points[i] = pt;
}
point_t point_vec_pop(point_vec_ptr v)
{
    assert(v->count);
    if (v->count)
        return v->points[--v->count];
    return point_create(0, 0);
}
void point_vec_clear(point_vec_ptr v)
{
    if (!v->count) return;
    v->count = 0;
}
int point_vec_length(point_vec_ptr v) { return v->count; }
void point_vec_shuffle(point_vec_ptr v)
{
    int i;
    for (i = 0; i < v->count - 1; i++) /* Skiena: _Algorithm_Design_Manual_ p248 */
    {
        int j = rand_range(i, v->count - 1);
        point_t t = v->points[i];
        v->points[i] = v->points[j];
        v->points[j] = t;
    }
}
static void _swap(point_ptr pts, int i, int j)
{
    point_t tmp;
    if (i == j) return;
    tmp = pts[i];
    pts[i] = pts[j];
    pts[j] = tmp;
}
void point_vec_swap(point_vec_ptr v, int i, int j)
{
    assert(0 <= i && i < v->count);
    assert(0 <= j && j < v->count);
    if (i == j) return;
    _swap(v->points, i, j);
}
/* quick sort: cf vec_sort for discussion of algortithm */
static void _insertion_sort(point_ptr pts, int left, int right, point_cmp_f f)
{
    int j;
    for (j = left + 1; j <= right; j++)
    {
        point_t key = pts[j];
        int  i = j - 1;
        while (i >= left && f(pts[i], key) > 0)
        {
            pts[i + 1] = pts[i];
            i--;
        }
        pts[i + 1] = key;
    }
}
static int _median3_partition(point_ptr pts, int left, int right, point_cmp_f f)
{
    int center = (left + right) / 2;

    assert(right - left + 1 >= 3);

    /* sort <v[l], v[c], v[r]> in place */
    if (f(pts[left], pts[center]) > 0)
        _swap(pts, left, center);
    if (f(pts[center], pts[right]) > 0)
    {
        _swap(pts, center, right);
        if (f(pts[left], pts[center]) > 0)
            _swap(pts, left, center);
    }

    /* v[c] is the median, put it in v[r-1] */
    _swap(pts, center, right - 1);

    /* "burn the candle at both ends" ... */
    {
        point_t pivot = pts[right - 1];
        int  i = left + 1;
        int  j = right - 2;
        while (1)
        {
            while (f(pts[i], pivot) < 0) i++;
            while (f(pts[j], pivot) > 0) j--;

            if (i < j)
                _swap(pts, i, j);
            else
                break;

            i++;
            j--;
        }
        _swap(pts, i, right - 1);

        return i;
    }
}
static void _quick_sort(point_ptr pts, int left, int right, point_cmp_f f)
{
    if (right - left + 1 < 20)
        _insertion_sort(pts, left, right, f);
    else
    {
        int i = _median3_partition(pts, left, right, f);
        _quick_sort(pts, left, i - 1, f);
        _quick_sort(pts, i + 1, right, f);
    }
}
void point_vec_sort(point_vec_ptr v, point_cmp_f f)
{
    _quick_sort(v->points, 0, v->count - 1, f);
}
/************************************************************************
 * Point Queue
 ************************************************************************/
struct point_queue_s
{
    int head, tail, count, allocated;
    point_ptr points;
};
static void _overflow(point_queue_ptr q)
{
    point_queue_t nq = {0};
    int i;
    assert(q->allocated);
    assert(q->count);
    nq.count = q->count;
    nq.allocated = q->allocated*2;
    nq.points = malloc(nq.allocated*sizeof(point_t));
    for (i = 0; i < q->count; i++) /* XXX q->tail == q->head triggers _overflow */
    {
        point_t p = q->points[q->tail++];
        if (q->tail == q->allocated) q->tail = 0;
        nq.points[nq.head++] = p;
        assert(nq.head < nq.allocated);
    }
    free(q->points);
    *q = nq;
}
point_queue_ptr point_queue_alloc(void)
{
    point_queue_ptr q = malloc(sizeof(point_queue_t));
    memset(q, 0, sizeof(point_queue_t));
    q->allocated = 20;
    q->points = malloc(q->allocated*sizeof(point_t));
    return q;
}
void point_queue_free(point_queue_ptr q)
{
    if (!q) return;
    free(q->points);
    free(q);
}
void point_queue_add(point_queue_ptr q, point_t p)
{
    assert(q->allocated);
    q->points[q->head++] = p;
    if (q->head == q->allocated) q->head = 0;
    q->count++;
    if (q->tail == q->head) _overflow(q);
}
point_t point_queue_remove(point_queue_ptr q)
{
    point_t p;
    assert(q->count);
    assert(q->tail != q->head); /* empty q */
    p = q->points[q->tail++];
    if (q->tail == q->allocated) q->tail = 0;
    q->count--;
    return p;
}
int point_queue_count(point_queue_ptr q)
{
    return q->count;
}
