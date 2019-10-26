#include "angband.h" /* ddx and ddy */

#include <assert.h>

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
point_t point_jump(point_t p1, int dir, int ct)
{
    point_t p = p1;
    int i;
    for (i = 0; i < ct; i++)
    {
        p.x += ddx[dir];
        p.y += ddy[dir];
    }
    return p;
}
int point_step_dir(point_t p1, point_t p2) /* XXX assumes adjacent points */
{
    int dir;
    for (dir = 0; dir <= 9; dir++)
    {
        point_t p = point_step(p1, dir);
        if (point_equals(p, p2)) return dir;
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
point_t size_create(int cx, int cy)
{
    return point_create(cx, cy);
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

/* Trivial rectangle utility to make code a bit more readable */
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
    return rect_translate(r, v.x, v.y);
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

rect_t rect_translate(rect_t r, int dx, int dy)
{
    rect_t result = {0};
    if (rect_is_valid(r))
        result = rect_create(r.x + dx, r.y + dy, r.cx, r.cy);
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
void point_vec_swap(point_vec_ptr v, int i, int j)
{
    point_t tmp;
    assert(0 <= i && i < v->count);
    assert(0 <= j && j < v->count);
    if (i == j) return;
    tmp = v->points[i];
    v->points[i] = v->points[j];
    v->points[j] = tmp;
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
