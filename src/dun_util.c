#include "angband.h"
#include "dun_util.h"
#include <assert.h>
/************************************************************************
 * Plasma Fractal
 * Algorithm from old wild.c. The recursion is simple:
 * [1] Start with a rectangle whose four corners are set.
 * [2] Recurse on each of 4 subrectangles. To do so, we need to set
 *     each of their 4 corners, which requires setting the center
 *     and 4 side-midpoints of the original rect. The center is computed
 *     from the 4 corners and the side midpoints from the 3 vertices
 *     of an interior triangle (2 diagonals divide the rect into 4
 *     triangles ... Draw a picture).
 * [3] Each such average is perturbed randomly.
 * [4] Clients then use the resulting height map to convert to terrain.
 ************************************************************************/
static void _frac_set(dun_frac_ptr frac, int x, int y, byte h)
{
    int xx = x - frac->rect.x;
    int yy = y - frac->rect.y;
    int i = yy * frac->rect.cx + xx;
    assert(rect_contains_xy(frac->rect, x, y));
    frac->map[i] = h;
}
static int _frac_get(dun_frac_ptr frac, int x, int y)
{
    int xx = x - frac->rect.x;
    int yy = y - frac->rect.y;
    int i = yy * frac->rect.cx + xx;
    assert(rect_contains_xy(frac->rect, x, y));
    return frac->map[i];
}
static int _perturb(dun_frac_ptr frac, int scale)
{
    if (!frac->grid) return rand_range(-frac->rough, frac->rough);
    return rand_range(-scale, scale) * frac->rough / 16;
}
static int _init(dun_frac_ptr frac)
{
    return randint1(frac->max);
}
dun_frac_ptr dun_frac_alloc(rect_t rect, int max_height)
{
    dun_frac_ptr frac = malloc(sizeof(dun_frac_t));
    memset(frac, 0, sizeof(dun_frac_t));

    frac->rect = rect;
    frac->max = max_height;
    frac->rough = 16;
    frac->perturb_f = _perturb;
    frac->init_f = _init;

    frac->map = malloc(rect.cx * rect.cy * sizeof(byte));
    dun_frac_reset(frac);
    return frac;
}
void dun_frac_reset(dun_frac_ptr frac)
{
    point_t min = rect_top_left(frac->rect);
    point_t max = rect_bottom_right(frac->rect);
    byte h = (frac->max + 1)/2;

    memset(frac->map, 0xFF, frac->rect.cx * frac->rect.cy * sizeof(byte)); /* mark as unset */

    /* seed the algorithm by setting the 4 initial corners.
     * clients may adjust these seeds before calling "calc". */
    _frac_set(frac, max.x, min.y, h);
    _frac_set(frac, min.x, min.y, h);
    _frac_set(frac, min.x, max.y, h);
    _frac_set(frac, max.x, max.y, h);
}
void dun_frac_free(dun_frac_ptr frac)
{
    if (!frac) return;
    free(frac->map);
    free(frac);
}
static int _perturb4(dun_frac_ptr frac, int h1, int h2, int h3, int h4, int perturb)
{
    int sum = h1 + h2 + h3 + h4;
    int avg = sum/4 + perturb;
    if (sum % 4 > 1) avg++;
    if (avg < 0) avg = 0;
    if (avg > frac->max) avg = frac->max;
    return avg;
}
static int _perturb3(dun_frac_ptr frac, int h1, int h2, int h3, int perturb)
{
    int sum = h1 + h2 + h3;
    int avg = sum/3 + perturb;
    if (sum % 3) avg++;
    if (avg < 0) avg = 0;
    if (avg > frac->max) avg = frac->max;
    return avg;
}
void dun_frac_calc_aux(dun_frac_ptr frac, int x1, int y1, int x2, int y2)
{
    int h1, h2, h3, h4, hc, h, xm, ym;
    int gx = x2 - x1;
    int gy = y2 - y1;
    int grid = MAX(gx, gy); /* XXX */

    if (x1 + 1 >= x2 && y1 + 1 >= y2) return; /* don't assume square */
    xm = (x2 - x1)/2 + x1;
    ym = (y2 - y1)/2 + y1;

    /* heights for 4 corners (beginning in "quadrant I" and moving counter-clockwise) */
    h1 = _frac_get(frac, x2, y1);
    h2 = _frac_get(frac, x1, y1);
    h3 = _frac_get(frac, x1, y2);
    h4 = _frac_get(frac, x2, y2);

    /* center */
    hc = _frac_get(frac, xm, ym);
    if (hc == 0xFF)
    {
        if (frac->grid && frac->grid < grid)
            hc = frac->init_f(frac);
        else
            hc = _perturb4(frac, h1, h2, h3, h4, frac->perturb_f(frac, grid));
        _frac_set(frac, xm, ym, hc);
    }
    /* top side */
    h = _frac_get(frac, xm, y1);
    if (h == 0xFF)
    {
        if (frac->grid && frac->grid < grid)
            h = frac->init_f(frac);
        else
            h = _perturb3(frac, h1, h2, hc, frac->perturb_f(frac, gx));
        _frac_set(frac, xm, y1, h);
    }
    /* left side */
    h = _frac_get(frac, x1, ym);
    if (h == 0xFF)
    {
        if (frac->grid && frac->grid < grid)
            h = frac->init_f(frac);
        else
            h = _perturb3(frac, h2, h3, hc, frac->perturb_f(frac, gy));
        _frac_set(frac, x1, ym, h);
    }
    /* bottom side */
    h = _frac_get(frac, xm, y2);
    if (h == 0xFF)
    {
        if (frac->grid && frac->grid < grid)
            h = frac->init_f(frac);
        else
            h = _perturb3(frac, h3, h4, hc, frac->perturb_f(frac, gx));
        _frac_set(frac, xm, y2, h);
    }
    /* right side */
    h = _frac_get(frac, x2, ym);
    if (h == 0xFF)
    {
        if (frac->grid && frac->grid < grid)
            h = frac->init_f(frac);
        else
            h = _perturb3(frac, h4, h1, hc, frac->perturb_f(frac, gy));
        _frac_set(frac, x2, ym, h);
    }

    /* recurse */
    dun_frac_calc_aux(frac, xm, y1, x2, ym); /* QI: Top Right */
    dun_frac_calc_aux(frac, x1, y1, xm, ym); /* QII: Top Left */
    dun_frac_calc_aux(frac, x1, ym, xm, y2); /* QIII: Bottom Left */
    dun_frac_calc_aux(frac, xm, ym, x2, y2); /* QIV: Bottom Right */
}
void dun_frac_calc(dun_frac_ptr frac)
{
    point_t min = rect_top_left(frac->rect);
    point_t max = rect_bottom_right(frac->rect);

    dun_frac_calc_aux(frac, min.x, min.y, max.x, max.y);
}
byte dun_frac_get(dun_frac_ptr frac, point_t pos)
{
    return _frac_get(frac, pos.x, pos.y);
}
void dun_frac_set(dun_frac_ptr frac, point_t pos, byte h)
{
    _frac_set(frac, pos.x, pos.y, h);
}
/************************************************************************
 * Extra Dungeon Info (u32b)
 ************************************************************************/
dun_u32b_ptr dun_u32b_alloc(rect_t rect)
{
    int cb = rect.cx * rect.cy * sizeof(u32b);
    dun_u32b_ptr map = malloc(sizeof(dun_u32b_t));
    map->rect = rect;
    map->map = malloc(cb);
    memset(map->map, 0, cb);
    return map;
}
void dun_u32b_free(dun_u32b_ptr map)
{
    if (!map) return;
    free(map->map);
    free(map);
}
void dun_u32b_clear(dun_u32b_ptr map)
{
    int cb = map->rect.cx * map->rect.cy * sizeof(u32b);
    memset(map->map, 0, cb);
}
u32b dun_u32b_get(dun_u32b_ptr map, point_t pos)
{
    return *dun_u32b_get_ptr(map, pos);
}
u32b_ptr dun_u32b_get_ptr(dun_u32b_ptr map, point_t pos)
{
    int x = pos.x - map->rect.x;
    int y = pos.y - map->rect.y;
    int i = y*map->rect.cx + x;
    assert(rect_contains_point(map->rect, pos));
    return &map->map[i];
}
void dun_u32b_set(dun_u32b_ptr map, point_t pos, u32b val)
{
    *dun_u32b_get_ptr(map, pos) = val;
}

/************************************************************************
 * Bitmap
 ************************************************************************/
dun_bmp_ptr dun_bmp_alloc(rect_t rect)
{
    int ct_bits = rect.cx * rect.cy;
    int ct_words = (ct_bits + 32 - 1)/32;
    dun_bmp_ptr bmp = malloc(sizeof(dun_bmp_t));

    bmp->rect = rect;
    bmp->bits = malloc(ct_words * sizeof(u32b));
    memset(bmp->bits, 0, ct_words * sizeof(u32b));

    return bmp;
}
void dun_bmp_free(dun_bmp_ptr bmp)
{
    if (!bmp) return;
    free(bmp->bits);
    free(bmp);
}
void dun_bmp_clear(dun_bmp_ptr bmp)
{
    int ct_bits = bmp->rect.cx * bmp->rect.cy;
    int ct_words = (ct_bits + 32 - 1)/32;
    memset(bmp->bits, 0, ct_words * sizeof(u32b));
}
static bool dun_bmp_test_aux(dun_bmp_ptr bmp, point_t pos)
{
    int x = pos.x - bmp->rect.x;
    int y = pos.y - bmp->rect.y;
    int bi = y*bmp->rect.cx + x;
    int wi = bi / 32;
    int mask = 1 << bi % 32;

    return BOOL(bmp->bits[wi] & mask);
}
extern bool dun_bmp_test(dun_bmp_ptr bmp, point_t pos)
{
    if (rect_contains_point(bmp->rect, pos))
        return dun_bmp_test_aux(bmp, pos);
    return FALSE;
}
void dun_bmp_iter(dun_bmp_ptr bmp, void (*f)(point_t pos))
{
    #if 1
    int ct_bits = bmp->rect.cx * bmp->rect.cy;
    int ct_words = (ct_bits + 32 - 1)/32;
    int i, j;

    for (i = 0; i < ct_words; i++)
    {
        u32b word = bmp->bits[i];
        if (!word) continue;
        for (j = 0; j < 32; j++)
        {
            if (word & 01)
            {
                int k = 32*i + j;
                point_t p;
                p.y = bmp->rect.y + k / bmp->rect.cx;
                p.x = bmp->rect.x + k % bmp->rect.cx;

                assert(rect_contains_point(bmp->rect, p));
                assert(dun_bmp_test_aux(bmp, p));

                f(p);
            }
            word >>= 1;
        }
    }
    #else
    point_t min = rect_top_left(bmp->rect);
    point_t max = rect_bottom_right(bmp->rect);
    point_t p;
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            if (dun_bmp_test_aux(bmp, p))
                f(p);
        }
    }
    #endif
}
void dun_bmp_set(dun_bmp_ptr bmp, point_t pos)
{
    int x = pos.x - bmp->rect.x;
    int y = pos.y - bmp->rect.y;
    int bi = y*bmp->rect.cx + x;
    int wi = bi / 32;
    int mask = 1 << bi % 32;
    assert(rect_contains_point(bmp->rect, pos));
    bmp->bits[wi] |= mask;
}
void dun_bmp_unset(dun_bmp_ptr bmp, point_t pos)
{
    int x = pos.x - bmp->rect.x;
    int y = pos.y - bmp->rect.y;
    int bi = y*bmp->rect.cx + x;
    int wi = bi / 32;
    int mask = 1 << bi % 32;
    assert(rect_contains_point(bmp->rect, pos));
    bmp->bits[wi] &= ~mask;
}
/************************************************************************
 * Flow
 ************************************************************************/
struct dun_flow_s
{
    dun_ptr       dun;
    point_t       pos;
    int           radius;
    dun_grid_p    filter;
    rect_t        rect;
    flow_data_ptr map;    
};

static flow_data_ptr _flow_data_at(dun_flow_ptr flow, point_t pos)
{
    int x = pos.x - flow->rect.x;
    int y = pos.y - flow->rect.y;
    int i = y*flow->rect.cx + x;
    assert(rect_contains_point(flow->rect, pos));
    return &flow->map[i];
}
static void _flow_calc(dun_flow_ptr flow)
{
    point_queue_ptr q = point_queue_alloc();

    assert(rect_contains_point(flow->rect, flow->pos));

    /* breadth-first traversal thru (implicit) adjacency graph */
    point_queue_add(q, flow->pos);
    while (point_queue_count(q))
    {
        point_t       p = point_queue_remove(q);
        int           i;
        flow_data_ptr f = _flow_data_at(flow, p);

        for (i = 0; i < 8; i++)
        {
            point_t       p2 = point_step(p, ddd[i]);
            int           c2 = f->cost + 1;
            int           d2 = f->dist + 1;
            dun_grid_ptr  g2;
            flow_data_ptr f2;
            bool          closed_door;

            if (c2 > 250) continue; /* byte overflow on travel */
            if (!dun_pos_interior(flow->dun, p2)) continue;
            if (!rect_contains_point(flow->rect, p2)) continue;
            if (point_equals(p2, flow->pos)) continue;

            g2 = dun_grid_at(flow->dun, p2);
            closed_door = is_closed_door(g2->feat);
            if (closed_door) c2 += 3;

            f2 = _flow_data_at(flow, p2);
            if (f2->dist && f2->dist <= d2 && f2->cost <= c2) continue; /* already processed */

            if (flow->filter) /* plr version */
            {
                /* XXX Custom filters should check for walls. This allows plr travel
                 * to fly over DARK_PITs and the like, which lack FF_MOVE */
                if (!flow->filter(p2, g2)) continue;
            }
            else /* monster version */
            {
                if (!have_flag(dun_grid_feat(g2)->flags, FF_MOVE) && !closed_door)
                    continue; /* wall, etc. */
            }

            if (f2->cost == 0 || f2->cost > c2) f2->cost = c2;
            if (f2->dist == 0 || f2->dist > d2) f2->dist = d2;

            if (d2 >= 3*flow->radius/2) continue;

            point_queue_add(q, p2);
        }
    }
    point_queue_free(q);
}
dun_flow_ptr dun_flow_calc(dun_ptr dun, point_t pos, int radius, dun_grid_p filter)
{
    dun_flow_ptr flow = malloc(sizeof(dun_flow_t));
    int cb;

    memset(flow, 0, sizeof(dun_flow_t));
    flow->dun = dun;
    flow->pos = pos;
    flow->radius = radius;
    flow->filter = filter;
    flow->rect = rect_create_centered(pos, radius, radius);
    if (rect_area(flow->rect) > rect_area(dun->rect)/2)
        flow->rect = dun->rect;

    cb = rect_area(flow->rect) * sizeof(flow_data_t);
    flow->map = malloc(cb);
    memset(flow->map, 0, cb);

    _flow_calc(flow);
    return flow;
}
void dun_flow_free(dun_flow_ptr flow)
{
    if (!flow) return;
    free(flow->map);
    free(flow);
}
void dun_flow_recalc(dun_flow_ptr flow, point_t pos)
{
    if ((running || travel.run) && flow->dun->dun_id == p_ptr->dun_id)
    {
        if (plr_los(flow->pos)) return;
    }

    if (!rect_equals(flow->rect, flow->dun->rect))
    {
        point_t v = point_subtract(pos, flow->pos);
        flow->rect = rect_translate(flow->rect, v.x, v.y);
        assert(rect_contains_point(flow->rect, pos));
    }
    flow->pos = pos;
    memset(flow->map, 0, rect_area(flow->rect)*sizeof(flow_data_t));
    _flow_calc(flow);
}
flow_data_t dun_flow_at(dun_flow_ptr flow, point_t pos)
{
    flow_data_t data = {0};
    /* XXX Accept a null flow since it might not always be computed */
    if (flow && rect_contains_point(flow->rect, pos))
        data = *_flow_data_at(flow, pos);
    return data;
}
