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
int dun_frac_get(dun_frac_ptr frac, point_t pos)
{
    return _frac_get(frac, pos.x, pos.y);
}
void dun_frac_set(dun_frac_ptr frac, point_t pos, byte h)
{
    _frac_set(frac, pos.x, pos.y, h);
}
void dun_frac_dump(dun_frac_ptr frac)
{
    FILE        *fp;
    char         buf[1024];
    point_t      min = rect_top_left(frac->rect);
    point_t      max = rect_bottom_right(frac->rect), p;


    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "frac.txt");
    fp = my_fopen(buf, "w");
    if (!fp) return;

    fprintf(fp, "Fractal (%d, %d, %d, %d)\n", min.x, min.y, max.x, max.y);

    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
            fprintf(fp, "%3d ", _frac_get(frac, p.x, p.y));
        fprintf(fp, "\n");
    }
    my_fclose(fp);
    msg_format("Created fractal file %s.", buf);
}
/************************************************************************
 * Extra Dungeon Info (byte)
 ************************************************************************/
dun_byte_ptr dun_byte_alloc(rect_t rect) { return dun_byte_alloc_aux(rect, 0); }
dun_byte_ptr dun_byte_alloc_aux(rect_t rect, byte init)
{
    int cb = rect.cx * rect.cy * sizeof(byte);
    dun_byte_ptr map = malloc(sizeof(dun_byte_t));
    map->rect = rect;
    map->map = malloc(cb);
    map->init = init;
    memset(map->map, init, cb);
    return map;
}
void dun_byte_free(dun_byte_ptr map)
{
    if (!map) return;
    free(map->map);
    free(map);
}
void dun_byte_clear(dun_byte_ptr map)
{
    int cb = map->rect.cx * map->rect.cy * sizeof(byte);
    memset(map->map, map->init, cb);
}
byte dun_byte_get(dun_byte_ptr map, point_t pos)
{
    if (rect_contains_point(map->rect, pos))
    {
        int x = pos.x - map->rect.x;
        int y = pos.y - map->rect.y;
        int i = y*map->rect.cx + x;
        return map->map[i];
    }
    return map->init;
}
void dun_byte_set(dun_byte_ptr map, point_t pos, byte val)
{
    int x = pos.x - map->rect.x;
    int y = pos.y - map->rect.y;
    int i = y*map->rect.cx + x;
    assert(rect_contains_point(map->rect, pos));
    map->map[i] = val;
}

/************************************************************************
 * Light
 ************************************************************************/
#define _LIGHT_OFFSET 100
dun_light_ptr dun_light_alloc(dun_ptr dun, rect_t rect)
{
    int cb = rect.cx * rect.cy * sizeof(byte);
    dun_light_ptr map = malloc(sizeof(dun_light_t));
    map->dun = dun;
    map->rect = rect;
    map->map = malloc(cb);
    memset(map->map, _LIGHT_OFFSET, cb);
    return map;
}
void dun_light_free(dun_light_ptr map)
{
    if (!map) return;
    free(map->map);
    free(map);
}
void dun_light_clear(dun_light_ptr map)
{
    int cb = map->rect.cx * map->rect.cy * sizeof(byte);
    memset(map->map, _LIGHT_OFFSET, cb);
}
int dun_light_get(dun_light_ptr map, point_t pos)
{
    if (rect_contains_point(map->rect, pos))
    {
        int x = pos.x - map->rect.x;
        int y = pos.y - map->rect.y;
        int i = y*map->rect.cx + x;
        int l = map->map[i];
        return l - _LIGHT_OFFSET;
    }
    return 0;
}
void dun_light_set(dun_light_ptr map, point_t pos, int light)
{
    int x = pos.x - map->rect.x;
    int y = pos.y - map->rect.y;
    int i = y*map->rect.cx + x;
    byte l = light + _LIGHT_OFFSET;
    assert(rect_contains_point(map->rect, pos));
    assert(-50 <= light && light <= 50);
    map->map[i] = l;
}
extern void dun_light_add(dun_light_ptr map, point_t pos, int light)
{
    int x = pos.x - map->rect.x;
    int y = pos.y - map->rect.y;
    int i = y*map->rect.cx + x;
    assert(rect_contains_point(map->rect, pos));
    assert(50 <= map->map[i] + light);
    assert(map->map[i] + light <= 150);
    map->map[i] += light;
}
void dun_light_iter(dun_light_ptr map, void (*f)(dun_ptr dun, point_t pos, int light))
{
    int ct = map->rect.cx * map->rect.cy;
    int i;
    for (i = 0; i < ct; i++)
    {
        int v = map->map[i];
        if (v != _LIGHT_OFFSET)
        {
            point_t p;
            p.y = map->rect.y + i / map->rect.cx;
            p.x = map->rect.x + i % map->rect.cx;
            f(map->dun, p, v - _LIGHT_OFFSET);
        }
    }
}
static int _count, _left, _right, _top, _bottom;
static void _light_sizer(dun_ptr dun, point_t pos, int light)
{
    if (pos.x < _left) _left = pos.x;
    if (pos.x > _right) _right = pos.x;
    if (pos.y < _top) _top = pos.y;
    if (pos.y > _bottom) _bottom = pos.y;
}
rect_t dun_light_bounding_rect(dun_light_ptr map)
{
    point_t p = rect_center(map->rect);
    int cx, cy;

    _left = _right = p.x;
    _top = _bottom = p.y;
    dun_light_iter(map, _light_sizer);

    cx = _right - _left + 1;
    cy = _bottom - _top + 1;
    if (cx > 0 && cy > 0)
        return rect_create(_left, _top, cx, cy);
    return rect_invalid();
}
void dun_light_copy(dun_light_ptr src, dun_light_ptr dest)
{
    int cb = src->rect.cx * src->rect.cy * sizeof(byte);
    int cb2 = dest->rect.cx * dest->rect.cy * sizeof(byte);
    assert(cb == cb2);
    dest->dun = src->dun;
    dest->rect = src->rect;
    memcpy(dest->map, src->map, cb);
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
dun_bmp_ptr dun_bmp_alloc(dun_ptr dun, rect_t rect)
{
    int ct_bits = rect.cx * rect.cy;
    int ct_words = (ct_bits + 32 - 1)/32;
    dun_bmp_ptr bmp = malloc(sizeof(dun_bmp_t));

    bmp->dun = dun;
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
    u32b mask = 1U << bi % 32;

    return BOOL(bmp->bits[wi] & mask);
}
extern bool dun_bmp_test(dun_bmp_ptr bmp, point_t pos)
{
    if (rect_contains_point(bmp->rect, pos))
        return dun_bmp_test_aux(bmp, pos);
    return FALSE;
}
void dun_bmp_union(dun_bmp_ptr l, dun_bmp_ptr r)
{
    int ct_bits = l->rect.cx * l->rect.cy;
    int ct_words = (ct_bits + 32 - 1)/32;
    int i;
    assert(rect_equals(l->rect, r->rect));
    for (i = 0; i < ct_words; i++)
        l->bits[i] |= r->bits[i];
}
void dun_bmp_intersection(dun_bmp_ptr l, dun_bmp_ptr r)
{
    int ct_bits = l->rect.cx * l->rect.cy;
    int ct_words = (ct_bits + 32 - 1)/32;
    int i;
    assert(rect_equals(l->rect, r->rect));
    for (i = 0; i < ct_words; i++)
        l->bits[i] &= r->bits[i];
}
void dun_bmp_difference(dun_bmp_ptr l, dun_bmp_ptr r)
{
    int ct_bits = l->rect.cx * l->rect.cy;
    int ct_words = (ct_bits + 32 - 1)/32;
    int i;
    assert(rect_equals(l->rect, r->rect));
    for (i = 0; i < ct_words; i++)
        l->bits[i] &= ~r->bits[i];
}
void dun_bmp_iter(dun_bmp_ptr bmp, void (*f)(dun_ptr dun, point_t pos))
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

                f(bmp->dun, p);
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
                f(bmp->dun, p);
        }
    }
    #endif
}
static void _counter(dun_ptr dun, point_t pos) { _count++; }
int dun_bmp_count(dun_bmp_ptr map) /* diagnostics ... e.g. what is the size of current view? */
{
    _count = 0;
    dun_bmp_iter(map, _counter);
    return _count;
}
static void _sizer(dun_ptr dun, point_t pos)
{
    if (pos.x < _left) _left = pos.x;
    if (pos.x > _right) _right = pos.x;
    if (pos.y < _top) _top = pos.y;
    if (pos.y > _bottom) _bottom = pos.y;
}
rect_t dun_bmp_bounding_rect(dun_bmp_ptr map)
{
    point_t p = rect_center(map->rect);
    int cx, cy;

    _left = _right = p.x;
    _top = _bottom = p.y;
    dun_bmp_iter(map, _sizer);

    cx = _right - _left + 1;
    cy = _bottom - _top + 1;
    if (cx > 0 && cy > 0)
        return rect_create(_left, _top, cx, cy);
    return rect_invalid();
}
void dun_bmp_set(dun_bmp_ptr bmp, point_t pos)
{
    int x = pos.x - bmp->rect.x;
    int y = pos.y - bmp->rect.y;
    int bi = y*bmp->rect.cx + x;
    int wi = bi / 32;
    u32b mask = 1U << bi % 32;
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
static int _flow_get(dun_flow_ptr flow, point_t pos)
{
    int x = pos.x - flow->rect.x;
    int y = pos.y - flow->rect.y;
    int i = y*flow->rect.cx + x;
    assert(rect_contains_point(flow->rect, pos));
    return flow->map[i];
}
static void _flow_set(dun_flow_ptr flow, point_t pos, int dis)
{
    int x = pos.x - flow->rect.x;
    int y = pos.y - flow->rect.y;
    int i = y*flow->rect.cx + x;
    assert(rect_contains_point(flow->rect, pos));
    assert(0 <= dis && dis <= DUN_FLOW_MAX);
    flow->map[i] = dis;
}
static void _flow_calc(dun_flow_ptr flow)
{
    dun_mgr_ptr     dm = dun_mgr();
    point_queue_ptr q = point_queue_alloc();

    assert(rect_contains_point(flow->rect, flow->pos));

    if (dm->prof)
        z_timer_resume(&dm->prof->flow_timer);

    /* reset */
    assert(0xFF == DUN_FLOW_NULL);
    memset(flow->map, 0xFF, rect_area(flow->rect)*sizeof(byte));

    /* breadth-first traversal thru (implicit) adjacency graph */
    _flow_set(flow, flow->pos, 0); /* mark */
    point_queue_add(q, flow->pos); /* q */
    while (point_queue_count(q))
    {
        point_t p = point_queue_remove(q);
        int     i;
        int     d = _flow_get(flow, p);

        assert (0 <= d && d <= DUN_FLOW_MAX); /* marked */

        for (i = 0; i < 8; i++)
        {
            point_t       p2 = point_step(p, ddd[i]);
            int           d2 = d + 1;
            dun_grid_ptr  g2;

            if (d2 > DUN_FLOW_MAX) continue; /* default limit */
            if (flow->flow_max && d2 > flow->flow_max) continue; /* custom limit */
            if (!dun_pos_interior(flow->dun, p2)) continue; /* out of bounds */
            if (!rect_contains_point(flow->rect, p2)) continue; /* out of bounds */
            if (_flow_get(flow, p2) != DUN_FLOW_NULL) continue; /* marked */

            g2 = dun_grid_at(flow->dun, p2);
            if (flow->filter) /* plr version */
            {
                /* XXX Custom filters should check for walls. This allows plr travel
                 * to fly over DARK_PITs and the like, which lack FF_MOVE */
                if (!flow->filter(p2, g2)) continue;
            }
            else /* monster version: note that monsters are fooled by illusory walls */
            {
                /* we can skip all walls, especially surface mountains (which allow
                 * monster placement and would otherwise induce a *big* flow calculation) */
                if (g2->type == FEAT_WALL)
                {
                    /* non-stupid monsters can reveal secret doors (_try_move_aux) */
                    if (!wall_has_secret_door(g2)) continue;
                }
                /* otherwise check for *general* monster access; not every mon will
                 * be able to follow the flow ... */
                else if (!illusion_allow_mon(g2, NULL) && !door_is_closed(g2))
                    continue;
            }

            _flow_set(flow, p2, d2); /* mark */
            if (d2 >= 3*flow->radius) continue; /* XXX was MONSTER_FLOW_DEPTH=100 */
            point_queue_add(q, p2);  /* q */
        }
    }
    point_queue_free(q);
    if (dm->prof)
        z_timer_pause(&dm->prof->flow_timer);
}
dun_flow_ptr dun_flow_calc(dun_ptr dun, point_t pos, int radius, dun_grid_p filter)
{
    dun_flow_ptr flow = malloc(sizeof(dun_flow_t));

    memset(flow, 0, sizeof(dun_flow_t));
    flow->dun = dun;
    flow->pos = pos;
    flow->radius = radius;
    flow->filter = filter;
    flow->rect = rect_create_centered(pos, radius, radius);
    if (rect_area(flow->rect) > rect_area(dun->rect)/2)
        flow->rect = dun->rect;

    flow->lazy = TRUE;
    return flow;
}
void dun_flow_free(dun_flow_ptr flow)
{
    if (!flow) return;
    if (flow->map) free(flow->map);
    free(flow);
}
void dun_flow_recalc(dun_flow_ptr flow, point_t pos)
{
    if (!rect_equals(flow->rect, flow->dun->rect))
    {
        point_t v = point_subtract(pos, flow->pos);
        flow->rect = rect_translate_xy(flow->rect, v.x, v.y);
        assert(rect_contains_point(flow->rect, pos));
    }
    flow->pos = pos;
    flow->lazy = TRUE;
}
int dun_flow_at(dun_flow_ptr flow, point_t pos)
{
    /* XXX Accept a null flow since it might not always be computed */
    if (flow && rect_contains_point(flow->rect, pos))
    {
        if (flow->lazy)
        {
            if (!flow->map)
                flow->map = malloc(rect_area(flow->rect) * sizeof(byte));
            _flow_calc(flow);
            flow->lazy = FALSE;
        }
        return _flow_get(flow, pos);
    }
    return DUN_FLOW_NULL;
}
int dun_flow_at_plr(dun_flow_ptr flow)
{
    if (flow && plr->dun_id == flow->dun->id)
        return dun_flow_at(flow, plr->pos);
    return DUN_FLOW_NULL;
}
int dun_flow_at_mon(dun_flow_ptr flow, mon_ptr mon)
{
    if (flow && mon->dun == flow->dun)
        return dun_flow_at(flow, mon->pos);
    return DUN_FLOW_NULL;
}
