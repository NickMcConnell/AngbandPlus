#include "angband.h"
#include "dun.h"
#include <assert.h>

/************************************************************************
 * Page
 ************************************************************************/
dun_page_ptr dun_page_alloc(rect_t rect)
{
    int cb = rect.cx * rect.cy * sizeof(dun_grid_t);
    dun_grid_ptr grids = malloc(cb);
    dun_page_ptr page;

    /* try to allocate the grids first in case it fails */
    if (!grids) return NULL;
    memset(grids, 0, cb);

    /* now we can do the page, which is small enough to never fail */
    page = malloc(sizeof(dun_page_t));
    memset(page, 0, sizeof(dun_page_t));
    page->rect = rect;
    page->grids = grids;
    return page;
}

/************************************************************************
 * Dungeon
 ************************************************************************/
static void _mon_free(mon_ptr mon)
{
    if (mon)
    {
        mon_race_ptr race = mon_true_race(mon);

        /*assert(race->cur_num > 0); XXX wizard commands ... */
        race->alloc.cur_num--;

        if (who_is_mon_id(plr->target, mon->id))
            plr->target = who_create_null();
        if (who_is_mon_id(plr->pet_target, mon->id))
            plr->pet_target = who_create_null();
        if (who_is_mon_id(plr->riding_target, mon->id))
            plr->riding_target = who_create_null();
        if (who_is_mon_id(plr->duelist_target, mon->id))
            plr->duelist_target = who_create_null();

        if (mon->pack)
            mon_pack_remove(mon->pack, mon);
        mon_free(mon);
    }
}
dun_ptr dun_alloc_aux(int id) /* for savefiles and D_SURFACE */
{
    dun_ptr d = malloc(sizeof(dun_t));
    memset(d, 0, sizeof(dun_t));
    d->id = id;
    d->mon = int_map_alloc((int_map_free_f)_mon_free);
    d->graveyard = vec_alloc((vec_free_f)_mon_free);
    d->obj = int_map_alloc((int_map_free_f)obj_free);
    d->junkpile = vec_alloc((vec_free_f)obj_free);
    d->mon_pos = point_map_alloc(NULL);
    d->obj_pos = point_map_alloc(NULL);
    d->flows = point_map_alloc((point_map_free_f)dun_flow_free);
    return d;
}
dun_ptr dun_alloc(int id, rect_t rect)
{
    dun_ptr d = dun_alloc_aux(id);
    d->rect = rect;
    d->page = dun_page_alloc(rect);
    return d;
}
static void _clear_mon(dun_ptr dun)
{
    int_map_clear(dun->mon);
    vec_clear(dun->graveyard);
}
static void _clear_obj(dun_ptr dun)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->obj);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        obj_ptr obj = int_map_iter_current(iter);

        if (obj_is_known(obj)) continue;

        if (obj->art_id)
            arts_lookup(obj->art_id)->generated = FALSE;
        if (obj->replacement_art_id)
            arts_lookup(obj->replacement_art_id)->generated = FALSE;
    }
    int_map_iter_free(iter);
    int_map_clear(dun->obj);
    vec_clear(dun->junkpile);
}
static void _clear_stairs(dun_ptr dun)
{
    dun_stairs_ptr stairs;
    for (stairs = dun->stairs; stairs; )
    {
        dun_stairs_ptr x = stairs;
        stairs = stairs->next;
        if (x->flow) dun_flow_free(x->flow);
        free(x);
    }
    dun->stairs = NULL; 
}
void _clear_grid(point_t pos, dun_grid_ptr grid)
{
    /* XXX */
    memset(grid, 0, sizeof(dun_grid_t));
    grid->flags |= CELL_PROJECT | CELL_LOS;
}
void dun_clear(dun_ptr dun)
{
    _clear_stairs(dun);
    point_map_clear(dun->obj_pos);
    point_map_clear(dun->mon_pos);
    _clear_obj(dun);
    _clear_mon(dun);
    dun_iter_grids(dun, _clear_grid);
}
static void _clear_mon_page(dun_ptr dun, dun_page_ptr page)
{
    vec_ptr v = vec_alloc(NULL);
    point_map_iter_ptr iter;
    int i;
    for (iter = point_map_iter_alloc(dun->mon_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        mon_ptr mon = point_map_iter_current(iter);
        if (rect_contains_point(page->rect, mon->pos))
            vec_add(v, mon);
    }
    point_map_iter_free(iter);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        /* XXX targets ... cf _mon_free */
        dun_delete_mon(dun, mon->id);
    }
    vec_free(v);
}
static void _clear_obj_page(dun_ptr dun, dun_page_ptr page)
{
    point_vec_ptr pts = point_vec_alloc();
    point_map_iter_ptr iter;
    int i;
    for (iter = point_map_iter_alloc(dun->obj_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        point_t pos = point_map_iter_current_key(iter);
        if (rect_contains_point(page->rect, pos))
            point_vec_add(pts, pos);
    }
    point_map_iter_free(iter);
    for (i = 0; i < point_vec_length(pts); i++)
    {
        point_t pos = point_vec_get(pts, i);
        dun_destroy_obj_at(dun, pos);
    }
    point_vec_free(pts);
}
static void _clear_stairs_page(dun_ptr dun, dun_page_ptr page)
{
    dun_stairs_ptr stairs, next, keep = NULL;
    for (stairs = dun->stairs; stairs; stairs = next)
    {
        next = stairs->next;
        if (rect_contains_point(page->rect, stairs->pos_here))
        {
            free(stairs);
            continue;
        }
        stairs->next = keep;
        keep = stairs;
    }
    dun->stairs = keep;
}
void dun_free_page(dun_ptr dun, dun_page_ptr page)
{
    _clear_mon_page(dun, page);
    _clear_obj_page(dun, page);
    _clear_stairs_page(dun, page);
    free(page->grids);
    free(page);
}
void dun_free(dun_ptr dun)
{
    dun_page_ptr p;
    if (!dun) return;
    if (dun->town)
    {
        town_free(dun->town);
        dun->town = NULL;
    }
    dun_clear(dun);
    point_map_free(dun->flows);
    point_map_free(dun->obj_pos);
    point_map_free(dun->mon_pos);
    vec_free(dun->junkpile);
    int_map_free(dun->obj);
    vec_free(dun->graveyard);
    int_map_free(dun->mon);
    for (p = dun->page; p; )
    {
        dun_page_ptr x = p;
        p = p->next;
        free(x->grids);
        free(x);
    }
    dun->page = NULL;
    if (dun->mon_alloc_tbl) vec_free(dun->mon_alloc_tbl);
    if (dun->obj_alloc_tbl) vec_free(dun->obj_alloc_tbl);
    if (dun->flow) dun_flow_free(dun->flow);
    free(dun);
}
bool dun_pos_valid(dun_ptr dun, point_t pos)
{
    return rect_contains_point(dun->rect, pos);
}
bool dun_pos_boundary(dun_ptr dun, point_t pos)
{
    return dun_pos_valid(dun, pos) && !dun_pos_interior(dun, pos);
}
bool dun_pos_interior(dun_ptr dun, point_t pos)
{
    return dun->rect.x < pos.x && pos.x < dun->rect.x + dun->rect.cx - 1 &&
           dun->rect.y < pos.y && pos.y < dun->rect.y + dun->rect.cy - 1;
}
rect_t dun_clip_rect(dun_ptr dun, rect_t rect)
{
    rect_t r = rect_interior(dun->rect);
    return rect_intersect(r, rect);
}
dun_grid_ptr _page_grid_at(dun_page_ptr page, point_t pos)
{
    int x, y, i;
    assert(rect_contains_point(page->rect, pos));
    x = pos.x - page->rect.x;
    y = pos.y - page->rect.y;
    i = y * page->rect.cx + x;
    return &page->grids[i];
}
dun_grid_ptr dun_grid_at(dun_ptr dun, point_t pos)
{
    return dun_cell_at(dun, pos);
}
dun_cell_ptr dun_cell_at(dun_ptr dun, point_t pos)
{
    assert(rect_contains_point(dun->rect, pos));
    if ((dun->flags & DF_PAGELOCK) || 0)
    {
        dun_page_ptr p;
        for (p = dun->page; p; p = p->next)
        {
            assert(rect_contains(dun->rect, p->rect));
            if (rect_contains_point(p->rect, pos))
                return _page_grid_at(p, pos);
        }
    }
    else
    {
        /* Locality of reference optimization for D_SURFACE. We'll check the first
         * page for a hit. If we miss, then we'll locate the correct page and move
         * it to the front. This optimization is stable (ie does not reorder other
         * pages in the list). Thus a cluster of "hot" pages will stay at the front. 
         * This optimization is blocked if DF_PAGELOCK is set (cf dun_world.c) */
        dun_page_ptr p = dun->page, prev;
        if (rect_contains_point(p->rect, pos))
            return _page_grid_at(p, pos);
        assert(p->next);
        prev = p;
        p = p->next;
        while (p)
        {
            if (rect_contains_point(p->rect, pos))
            {
                dun_grid_ptr g = _page_grid_at(p, pos);

                assert(prev);
                assert(p != dun->page); /* we checked the first page above */

                /* unlink p */
                prev->next = p->next;
                if (p->next)
                    p->next->prev = prev;
                /* move p to front of page list */
                p->next = dun->page;
                dun->page->prev = p;
                dun->page = p;

                return g;
            }
            prev = p;
            p = p->next;
        }
    }
    assert(0);
    return NULL; /* XXX no storage for this pos */
}
point_t dun_find_grid(dun_ptr dun, dun_grid_p p)
{
    point_t pos = {0};
    point_vec_ptr pts = dun_filter_grids(dun, p);
    if (point_vec_length(pts))
        pos = point_vec_get(pts, 0);
    point_vec_free(pts);
    return pos;
}
point_vec_ptr dun_filter_grids(dun_ptr dun, dun_grid_p p)
{
    point_vec_ptr pts = point_vec_alloc();
    assert(p);
    /* optimize the common case */
    assert(dun->page);
    if (rect_equals(dun->rect, dun->page->rect))
    {
        dun_grid_ptr grid = dun->page->grids;
        point_t pos;
        for (pos.y = dun->rect.y; pos.y < dun->rect.y + dun->rect.cy; pos.y++)
        {
            for (pos.x = dun->rect.x; pos.x < dun->rect.x + dun->rect.cx; pos.x++)
            {
                if (p(pos, grid)) point_vec_push(pts, pos);
                grid++;
            }
        }
    }
    else
    {
        point_t pos;
        for (pos.y = dun->rect.y; pos.y < dun->rect.y + dun->rect.cy; pos.y++)
        {
            for (pos.x = dun->rect.x; pos.x < dun->rect.x + dun->rect.cx; pos.x++)
            {
                dun_grid_ptr grid = dun_grid_at(dun, pos);
                if (p(pos, grid)) point_vec_push(pts, pos);
            }
        }
    }
    return pts;
}
void dun_iter_grids(dun_ptr dun, void (*f)(point_t pos, dun_grid_ptr grid))
{
    dun_page_ptr page;
    /* optimized for D_SURFACE (cf wiz_lite) */
    dun->flags |= DF_PAGELOCK; /* XXX essential in case f calls dun_grid_at, say, an adjacent pos */
    for (page = dun->page; page; page = page->next)
    {
        dun_grid_ptr grid = page->grids;
        point_t pos;
        for (pos.y = page->rect.y; pos.y < page->rect.y + page->rect.cy; pos.y++)
        {
            for (pos.x = page->rect.x; pos.x < page->rect.x + page->rect.cx; pos.x++)
            {
                f(pos, grid);
                grid++;
            }
        }
    }
    dun->flags &= ~DF_PAGELOCK;
}
void dun_iter_boundary(dun_ptr dun, dun_grid_f f)
{
    dun_iter_rect_boundary(dun, dun->rect, f);
}
void dun_iter_interior(dun_ptr dun, dun_grid_f f)
{
    point_t pos;
    assert(dun->page);
    /* optimize the common case */
    if (rect_equals(dun->rect, dun->page->rect))
    {
        for (pos.y = dun->rect.y + 1; pos.y < dun->rect.y + dun->rect.cy - 1; pos.y++)
        {
            dun_grid_ptr grid;
            pos.x = dun->rect.x + 1;
            grid = dun_grid_at(dun, pos);
            for (; pos.x < dun->rect.x + dun->rect.cx - 1; pos.x++, grid++)
            {
                f(pos, grid);
            }
        }
    }
    else dun_iter_rect_interior(dun, dun->rect, f);
}
void dun_iter_adjacent(dun_ptr dun, point_t pos, dun_grid_f f)
{
    int i;
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        if (dun_pos_interior(dun, p))
            f(p, dun_grid_at(dun, p));
    }
}
static void _page_iter_rect(dun_ptr dun, dun_page_ptr page, rect_t rect, dun_grid_f f)
{
    point_t min = rect_top_left(rect);
    point_t max = rect_bottom_right(rect), p;
    dun_grid_ptr row, g;

    assert(rect_contains(page->rect, rect));

    row = _page_grid_at(page, rect_top_left(rect));
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        g = row;
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            /*assert(_page_grid_at(page, p) == g);*/
            f(p, g++);
        }
        row += page->rect.cx;
    }
}
void dun_iter_rect(dun_ptr dun, rect_t rect, dun_grid_f f)
{
    #if 1
    dun_page_ptr page;
    /* optimized for dun_update_light (_terrain) on D_SURFACE */
    dun->flags |= DF_PAGELOCK;
    for (page = dun->page; page; page = page->next)
    {
        rect_t r = rect_intersect(page->rect, rect);
        if (rect_is_valid(r))
            _page_iter_rect(dun, page, r, f);
    }
    dun->flags &= ~DF_PAGELOCK;
    #else
    rect_t r = rect_intersect(dun->rect, rect);
    point_t p;
    if (!rect_is_valid(r)) return;
    for (p.y = r.y; p.y < r.y + r.cy; p.y++)
    {
        for (p.x = r.x; p.x < r.x + r.cx; p.x++)
        {
            /*if (dun_pos_interior(dun, p))*/
                f(p, dun_grid_at(dun, p));
        }
    }
    #endif
}
void dun_iter_rect_interior(dun_ptr dun, rect_t rect, dun_grid_f f)
{
    point_t p;
    assert(rect_contains(dun->rect, rect));
    for (p.y = rect.y + 1; p.y < rect.y + rect.cy - 1; p.y++)
    {
        for (p.x = rect.x + 1; p.x < rect.x + rect.cx - 1; p.x++)
        {
            dun_grid_ptr grid = dun_grid_at(dun, p);
            f(p, grid);
        }
    }
}
void dun_iter_rect_boundary(dun_ptr dun, rect_t rect, dun_grid_f f)
{
    int x,y;
    int left = rect.x;
    int right = rect.x + rect.cx - 1;
    int top = rect.y;
    int bottom = rect.y + rect.cy - 1;
    assert(rect_contains(dun->rect, rect));
    for (x = left; x <= right; x++)
    {
        point_t pos = point_create(x, top);
        f(pos, dun_grid_at(dun, pos));
        pos.y = bottom;
        f(pos, dun_grid_at(dun, pos));
    }
    for (y = top; y <= bottom; y++)
    {
        point_t pos = point_create(left, y);
        f(pos, dun_grid_at(dun, pos));
        pos.x = right;
        f(pos, dun_grid_at(dun, pos));
    }
}
static point_t _random_grid_fast(dun_ptr dun, rect_t rect, dun_grid_weight_f weight)
{
    point_t pos;
    int total = 0, roll;
    assert(weight);
    /* most of the time, dun has a single page and dun->page->rect == dun->rect.
     * in this case, we can optimize using pointer increments. */
    assert(rect_contains(dun->page->rect, rect));
    for (pos.y = rect.y + 1; pos.y < rect.y + rect.cy - 1; pos.y++)
    {
        dun_grid_ptr grid;
        pos.x = rect.x + 1;
        grid = dun_grid_at(dun, pos);
        for (; pos.x < rect.x + rect.cx - 1; pos.x++)
        {
            total += weight(pos, grid);
            grid++;
        }
    }
    if (!total) return point_create(-1, -1); /* client should check dun_pos_interior on the result */
    roll = randint0(total);
    for (pos.y = rect.y + 1; pos.y < rect.y + rect.cy - 1; pos.y++)
    {
        dun_grid_ptr grid;
        pos.x = rect.x + 1;
        grid = dun_grid_at(dun, pos);
        for (; pos.x < rect.x + rect.cx - 1; pos.x++)
        {
            roll -= weight(pos, grid);
            if (roll < 0) return pos;
            grid++;
        }
    }
    return point_create(-1, -1);  /* bug */
}
static point_t _random_grid_safe(dun_ptr dun, rect_t rect, dun_grid_weight_f weight)
{
    point_t pos;
    int total = 0, roll;
    assert(weight);
    for (pos.y = rect.y + 1; pos.y < rect.y + rect.cy - 1; pos.y++)
    {
        pos.x = rect.x + 1;
        for (; pos.x < rect.x + rect.cx - 1; pos.x++)
        {
            dun_grid_ptr grid = dun_grid_at(dun, pos);
            total += weight(pos, grid);
        }
    }
    if (!total) return point_create(-1, -1); /* client should check dun_pos_interior on the result */
    roll = randint0(total);
    for (pos.y = rect.y + 1; pos.y < rect.y + rect.cy - 1; pos.y++)
    {
        pos.x = rect.x + 1;
        for (; pos.x < rect.x + rect.cx - 1; pos.x++)
        {
            dun_grid_ptr grid = dun_grid_at(dun, pos);
            roll -= weight(pos, grid);
            if (roll < 0) return pos;
        }
    }
    return point_create(-1, -1);  /* bug */
}
point_t dun_random_grid_in_rect(dun_ptr dun, rect_t rect, dun_grid_weight_f weight)
{
    rect_t r = rect_intersect(dun->rect, rect);
    if (rect_contains(dun->page->rect, r))
        return _random_grid_fast(dun, r, weight);
    return _random_grid_safe(dun, r, weight);
}
point_t dun_random_grid(dun_ptr dun, dun_grid_weight_f weight)
{
    return dun_random_grid_in_rect(dun, rect_interior(dun->rect), weight);
}
dun_stairs_ptr dun_random_stairs(dun_ptr dun)
{
    dun_stairs_ptr stairs;
    int tot = 0, roll;
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        /* XXX if (filter && !filter(stairs)) continue; */
        tot++;
    }
    if (!tot) return NULL;
    roll = randint0(tot);
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        /* XXX if (filter && !filter(stairs)) continue; */
        roll--;
        if (roll < 0) return stairs;
    }
    return NULL;
}
dun_grid_ex_t dun_grid_ex_at(dun_ptr dun, point_t pos)
{
    dun_grid_ex_t gx;
    gx.grid = dun_grid_at(dun, pos);
    gx.mon = dun_mon_at(dun, pos);
    gx.obj = dun_obj_at(dun, pos);
    gx.pos = pos;
    gx.plr = dun_plr_at(dun, pos);
    return gx;
}
bool dun_allow_drop_at(dun_ptr dun, point_t pos)
{
    if (!dun_pos_interior(dun, pos)) return FALSE;
    return cell_allow_obj(dun_grid_at(dun, pos));
}
bool dun_allow_mon_at(dun_ptr dun, point_t pos)
{
    if (!dun_pos_interior(dun, pos)) return FALSE;
    if (dun_mon_at(dun, pos)) return FALSE;
    if (dun_plr_at(dun, pos)) return FALSE;
    return cell_allow_mon(dun_grid_at(dun, pos), NULL);
}
bool dun_allow_plr_at(dun_ptr dun, point_t pos)
{
    if (!dun_pos_interior(dun, pos)) return FALSE;
    if (dun_mon_at(dun, pos)) return FALSE;
    return cell_allow_plr(dun_grid_at(dun, pos));
}
bool dun_clean_at(dun_ptr dun, point_t pos)
{
    if (!dun_pos_interior(dun, pos)) return FALSE;
    if (dun_obj_at(dun, pos)) return FALSE;
    return floor_is_clean(dun_grid_at(dun, pos));
}
bool dun_naked_at(dun_ptr dun, point_t pos)
{
    if (!dun_pos_interior(dun, pos)) return FALSE;
    if (dun_obj_at(dun, pos)) return FALSE;
    if (dun_plr_at(dun, pos)) return FALSE;
    if (dun_mon_at(dun, pos)) return FALSE;
    return floor_is_clean(dun_grid_at(dun, pos));
}
extern bool dun_allow_trap_at(dun_ptr dun, point_t pos)
{
    return dun_clean_at(dun, pos);
}
point_t dun_scatter(dun_ptr dun, point_t pos, int spread)
{
    return dun_scatter_aux(dun, pos, spread, NULL);
}
point_t dun_scatter_aux(dun_ptr dun, point_t pos, int spread, bool (*filter)(dun_ptr dun, point_t pos))
{
    point_t p;
    assert(spread >= 0);
    assert(dun_pos_interior(dun, pos));
    if (spread == 0) return pos;
    for (;;)
    {
        p = point_random_jump(pos, spread);
        if (!dun_pos_interior(dun, p)) continue;
        if (spread > 1 && point_fast_distance(pos, p) > spread) continue;
        if (filter && !filter(dun, p)) continue;
        if (dun_project(dun, pos, p)) break;
    }
    return p;
}

bool dun_stop_disintegration_at(dun_ptr dun, point_t pos)
{
    return cell_stop_disintegrate(dun_grid_at(dun, pos));
}
bool dun_allow_los_at(dun_ptr dun, point_t pos)
{
    return cell_los(dun_grid_at(dun, pos));
}
bool dun_allow_project_at(dun_ptr dun, point_t pos)
{
    return cell_project(dun_grid_at(dun, pos));
}
void dun_forget_flow(dun_ptr dun)
{
    if (dun->flow)
    {
        dun_flow_free(dun->flow);
        dun->flow = NULL;
    }
}
void dun_note_pos(dun_ptr dun, point_t pos)
{
    if (dun->id == plr->dun_id && (dun->flags & DF_GENERATED))
    {
        assert(cave == dun);
        note_pos(pos);
    }
}
void dun_draw_pos(dun_ptr dun, point_t pos)
{
    if (dun->id == plr->dun_id && (dun->flags & DF_GENERATED))
    {
        assert(cave == dun);
        draw_pos(pos);
    }
}
/* player management */
void dun_move_plr(dun_ptr dun, point_t pos)
{
    point_t old_pos = plr->pos;
    assert(plr->dun_id == dun->id);
    assert(dun_pos_interior(dun, pos));
    plr->pos = pos;
    dun->flow_pos = pos;
    dun_update_flow(dun);
    dun_draw_pos(dun, old_pos);
    dun_draw_pos(dun, pos);

    if (dun->type->id == D_SURFACE)
        dun_world_move_plr(dun, pos);
    /* XXX cell_accept_plr is handled higher up in move_plr_effect
     * XXX this seems a cleaner place for it, though ...
    cell_accept_plr(dun, pos, ... ); */
}
bool dun_plr_at(dun_ptr dun, point_t pos)
{
    if (dun->id != plr->dun_id) return FALSE;
    return point_equals(plr->pos, pos);
}
/* stairs */
static bool _valid_stair_pos(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid;
    if (!dun_pos_interior(dun, pos)) return FALSE;
    /* blocked by objects, glyphs and permanent walls */
    if (dun_obj_at(dun, pos)) return FALSE;
    grid = dun_grid_at(dun, pos);
    if (grid->flags & CELL_PERM) return FALSE;
    if (floor_has_object(grid)) return FALSE;
    /* other features will be aggressively destroyed as
     * quest stairs *must* be generated at all costs! */
    return TRUE;
}
void dun_quest_stairs(dun_ptr dun, point_t pos, int lvl)
{
    dun_stairs_ptr stairs;

    while (!_valid_stair_pos(dun, pos))
        pos = scatter(pos, 1);

    cmsg_print(TERM_L_BLUE, "A magical staircase appears...");
    dun_place_downstairs(dun, pos);

    stairs = malloc(sizeof(dun_stairs_t));
    memset(stairs, 0, sizeof(dun_stairs_t));
    stairs->pos_here = pos;
    stairs->dun_type_id = dun->type->id;
    stairs->dun_lvl = lvl;
    dun_add_stairs(dun, stairs);

    dun->flags |= DF_UPDATE_FLOW;
}
void dun_quest_travel(dun_ptr dun, point_t pos)
{
    while (!_valid_stair_pos(dun, pos))
        pos = scatter(pos, 1);

    cmsg_print(TERM_L_BLUE, "A magical portal appears...");
    assert(dun == cave); /* XXX */
    dun_place_travel(dun, pos);
}
void dun_add_stairs(dun_ptr dun, dun_stairs_ptr info)
{
    info->next = dun->stairs;
    dun->stairs = info;
}
static dun_stairs_ptr _find_stairs(dun_ptr dun, point_t pos)
{
    dun_stairs_ptr stairs;
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
        if (point_equals(stairs->pos_here, pos)) return stairs;
    return NULL;
}
static bool _check_pos_plr(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid = dun_grid_at(dun, pos);
    if (dun_mon_at(dun, pos)) return FALSE;
    if (!cell_allow_plr(grid)) return FALSE;
    if (floor_has_trap(grid)) return FALSE;
    return TRUE;
}
static point_t _stairs_loc_plr(dun_ptr dun, point_t pos)
{
    int i;
    if (_check_pos_plr(dun, pos)) return pos;  /* postion of stairs (normal case) */
    for (i = 0; i < 9; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        if (_check_pos_plr(dun, p)) return p;  /* adjacent position if monster is blocking stairs */
    }
    return point_create(-1, -1);
}
bool dun_take_stairs_plr(dun_ptr dun)
{
    dun_stairs_ptr stairs;
    dun_ptr new_dun = NULL;
    point_t new_pos;

    assert(plr->dun_id == dun->id);
    assert(dun_pos_interior(dun, plr->pos));

    stairs = _find_stairs(dun, plr->pos);
    assert(stairs);

    if (stairs->dun_type_id == D_QUEST)
    {
        assert(!stairs->dun_id);
        dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        assert(new_dun->quest_id == stairs->dun_lvl);
        new_pos = plr->new_pos; /* quest maps *should* locate the player */
        if (!dun_pos_interior(new_dun, new_pos))
            new_pos = dun_random_plr_pos(new_dun); /* panic */
    }
    else
    {
        if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
            dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        new_pos = _stairs_loc_plr(new_dun, stairs->pos_there);
        if (!dun_pos_interior(new_dun, new_pos)) return FALSE;
    }
    dun->flow_pos = stairs->pos_here;
    dun->flags |= DF_UPDATE_FLOW;
    dun_mgr_plr_change_dun(new_dun, new_pos);
    if (dun->type->id == D_QUEST) /* XXX */
        dun_regen_town(new_dun);
    return TRUE;
}
static bool _check_pos_mon(dun_ptr dun, mon_ptr mon, point_t pos)
{
    dun_grid_ptr grid = dun_grid_at(dun, pos);
    if (dun_mon_at(dun, pos)) return FALSE;
    if (dun_plr_at(dun, pos)) return FALSE;
    if (!cell_allow_mon(grid, mon)) return FALSE;
    return TRUE;
}
static point_t _stairs_loc_mon(dun_ptr dun, mon_ptr mon, point_t pos)
{
    int i;
    if (_check_pos_mon(dun, mon, pos)) return pos;  /* postion of stairs (normal case) */
    for (i = 0; i < 9; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        if (_check_pos_mon(dun, mon, p)) return p;  /* adjacent position if monster/plr is blocking stairs */
    }
    return point_create(-1, -1);
}
bool dun_take_stairs_mon(dun_ptr dun, mon_ptr mon)
{
    dun_stairs_ptr stairs;
    dun_ptr new_dun, old_cave = cave;
    point_t new_pos;

    if (plr_is_riding_(mon))
        return dun_take_stairs_plr(dun);

    stairs = _find_stairs(dun, mon->pos);
    if (!stairs) return FALSE; /* paranoia */
    if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
    {
        /* XXX Note monsters can now flee the current level. Make sure a connected level exists! */
        dun_gen_connected(dun, stairs);
    }
    if (stairs->dun_type_id != dun->type->id)
    {
        /* XXX Questors can now chase the player off level, but not on to the surface. */
        if (mon->mflag2 & MFLAG2_QUESTOR) return FALSE;
    }

    new_dun = dun_mgr_dun(stairs->dun_id);
    if (!new_dun) return FALSE; /* garbage collected (should not happen) */

    new_pos = _stairs_loc_mon(new_dun, mon, stairs->pos_there);
    if (!dun_pos_interior(new_dun, new_pos)) return FALSE;

    dun_detach_mon(dun, mon->id);
    cave = new_dun; /* for draw_pos ... we need to see the monster chasing us! */
    mon_clear_target(mon);
    dun_place_mon(new_dun, mon, new_pos);
    cell_accept_mon(new_dun, new_pos, dun_cell_at(new_dun, new_pos), mon);
    cave = old_cave;
    if (mon->mflag2 & MFLAG2_HUNTED)
    {
        if (mon->flow) dun_flow_free(mon->flow);
        mon->flow = dun_flow_calc(new_dun, new_pos, MON_HUNT_RAD, NULL);
    }
    if (new_dun->id == plr->dun_id && (mon->race->light || mon->race->lantern))
        plr->update |= PU_MON_LIGHT;

    return TRUE;
}
#define _UP   0x01
#define _DOWN 0x02
static dun_stairs_ptr _random_stairs(dun_ptr dun, int options)
{
    dun_stairs_ptr stairs;
    int ct = 0, n;
    assert(options);
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        assert(stairs->dun_lvl != dun->dun_lvl);
        if (stairs->dun_type_id == D_QUEST) continue;
        if (!(options & _DOWN) && stairs->dun_lvl > dun->dun_lvl) continue;
        if (!(options & _UP) && stairs->dun_lvl < dun->dun_lvl) continue;
        ct++;
    }
    if (!ct) return NULL; /* panic */
    n = randint0(ct);
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        assert(stairs->dun_lvl != dun->dun_lvl);
        if (stairs->dun_type_id == D_QUEST) continue;
        if (!(options & _DOWN) && stairs->dun_lvl > dun->dun_lvl) continue;
        if (!(options & _UP) && stairs->dun_lvl < dun->dun_lvl) continue;
        n--;
        if (n < 0) return stairs;
    }
    assert(FALSE);
    return NULL;
}
void dun_trap_door_plr(dun_ptr dun)
{
    dun_stairs_ptr stairs = _random_stairs(dun, _DOWN);
    if (stairs)
    {
        dun_ptr new_dun;
        point_t pos;

        if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
            dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        pos = dun_random_plr_pos(new_dun);
        dun_mgr_plr_change_dun(new_dun, pos);
    }
}
void dun_trap_door_mon(dun_ptr dun, mon_ptr mon)
{
    dun_stairs_ptr stairs;
    assert(mon->dun == dun);

    if (plr_is_riding_(mon))
    {
        dun_trap_door_plr(dun);
        return;
    }

    stairs = _random_stairs(dun, _DOWN);
    if (stairs)
    {
        dun_ptr new_dun, old_cave = cave;
        point_t pos;

        if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
            dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        pos = dun_random_mon_pos(new_dun, mon->race);
        if (!dun_pos_interior(new_dun, pos)) return; /* panic */
        if (mon_show_msg(mon))
        {
            char name[MAX_NLEN_MON];
            monster_desc(name, mon, 0);
            msg_format("%^s falls through the trap door.", name);
        }
        dun_detach_mon(dun, mon->id);
        cave = new_dun; /* for draw_pos in case the monster lands on plr_dun near plr */
        dun_place_mon(new_dun, mon, pos);
        cave = old_cave;
    }
}
void dun_teleport_level_plr(dun_ptr dun)
{
    dun_stairs_ptr stairs;
    assert(plr->dun_id == dun->id);
    assert(cave == dun);

    if (plr->anti_tele)
    {
        msg_print("A mysterious force prevents you from teleporting!");
        equip_learn_flag(OF_NO_TELE);
        return;
    }

    stairs = _random_stairs(dun, _UP | _DOWN);
    if (stairs)
    {
        dun_ptr new_dun;
        point_t pos;

        if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
            dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        pos = dun_random_plr_pos(new_dun);
        if (new_dun->dun_lvl > dun->dun_lvl)
            msg_print("You sink through the floor.");
        else
            msg_print("You rise up through the ceiling.");
        dun_mgr_plr_change_dun(new_dun, pos);
    }
    else msg_print("Nothing happens."); /* e.g. D_SURFACE with no stairs in dun */
}
void dun_teleport_level_mon(dun_ptr dun, mon_ptr mon)
{
    dun_stairs_ptr stairs;
    assert(mon->dun == dun);

    stairs = _random_stairs(dun, _UP | _DOWN);
    if (plr_is_riding_(mon))
    {
        dun_teleport_level_plr(dun);
        return;
    }
    stairs = _random_stairs(dun, _UP | _DOWN);
    if (stairs)
    {
        dun_ptr new_dun, old_cave = cave;
        point_t pos;

        if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
            dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        pos = dun_random_mon_pos(new_dun, mon->race);
        if (!dun_pos_interior(new_dun, pos)) return; /* panic */
        if (mon_show_msg(mon))
        {
            char name[MAX_NLEN_MON];
            monster_desc(name, mon, 0);
            if (new_dun->dun_lvl > dun->dun_lvl)
                msg_format("%^s sinks through the floor.", name);
            else
                msg_format("%^s rises up through the ceiling.", name);
        }
        dun_detach_mon(dun, mon->id);
        cave = new_dun; /* for draw_pos in case the monster lands on plr_dun near plr */
        dun_place_mon(new_dun, mon, pos);
        cave = old_cave;
    }
    else if (mon_show_msg(mon)) msg_print("Failed!");
}
bool dun_create_stairs(dun_ptr dun, bool down_only)
{
    int            options = down_only ? _DOWN : _UP | _DOWN;
    dun_stairs_ptr stairs, other_stairs;
    dun_ptr        other_dun;

    assert(plr->dun_id == dun->id);
    assert(cave == dun);

    if (quests_get_current() || dun->dun_lvl == dun->type->max_dun_lvl)
        options &= ~_DOWN;

    if (!options || dun->type->id == D_SURFACE || dun->type->id == D_QUEST)
    {
        msg_print("There is no effect.");
        return FALSE;
    }

    stairs = _random_stairs(dun, options);
    if (!stairs)
    {
        msg_print("There is no effect.");
        return FALSE;
    }

    if (!dun_can_destroy_obj_at(dun, plr->pos))
    {
        msg_print("The object resists the spell.");
        return FALSE;
    }

    if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
        dun_gen_connected(dun, stairs);
    other_dun = dun_mgr_dun(stairs->dun_id);
    other_stairs = _find_stairs(other_dun, stairs->pos_there);

    dun_destroy_obj_at(dun, plr->pos);
    {
        dun_grid_ptr grid = dun_grid_at(dun, stairs->pos_here);
        if (stairs_go_down(grid))
            dun_place_downstairs(dun, plr->pos);
        else
            dun_place_upstairs(dun, plr->pos);
        dun->type->place_floor(dun, stairs->pos_here);
    }
    stairs->pos_here = plr->pos;
    other_stairs->pos_there = plr->pos;
    if (stairs->flow)
    {
        dun_flow_free(stairs->flow);
        stairs->flow = NULL;
    }

    return TRUE;
}

/************************************************************************
 * Processing
 ************************************************************************/
static void _process_mon(dun_ptr dun, mon_ptr mon)
{
    assert(cave == dun);
    assert(mon->dun == dun);
    if (mon_tim_find(mon, T_PARALYZED)) return;
    if (!fear_process_m(mon)) return;
    if (mon->mflag2 & MFLAG2_TRIPPED)
    {
        char m_name[80];
        monster_desc(m_name, mon, 0);
        msg_format("%^s gets back up and looks mad as hell.", m_name);
        mon->mflag2 &= ~MFLAG2_TRIPPED;
        mon_anger(mon);
        return;
    }
    msg_boundary();
    mon_process(mon);
}

static vec_ptr _monsters = NULL; /* monsters getting a move this turn */
static void _preprocess_mon(dun_ptr dun, mon_ptr mon)
{
    int  radius = 0, speed;
    bool test = FALSE;

    assert(cave == dun);
    if (!mon_is_valid(mon)) return; /* XXX bug */

    if (dun->turn % TURNS_PER_TICK == 0)
    {
        mon_tim_tick(mon);
        if (!mon_is_valid(mon)) return; /* XXX this can happen */
    }

    /* Require proximity */
    if (mon->cdis >= (plr->action == ACTION_GLITTER ? AAF_LIMIT_RING : AAF_LIMIT))
        return;

    /* Hack -- Monsters are automatically aware of the player (except for mimics) */
    if (!is_aware(mon))
    {
        if (plr->prace != RACE_MON_RING)
            mon->mflag2 |= MFLAG2_AWARE;
        else if (plr->riding && plr_view(mon->pos) && !mon_tim_find(mon, MT_SLEEP))
        {
            /* Player has a ring bearer, which this monster can see */
            mon->mflag2 |= MFLAG2_AWARE;
        }
    }

    /* Assume no move */
    test = FALSE;

    /* Handle "sensing radius" */
    radius = mon_move_range(mon);

    if ( plr->prace == RACE_MON_RING
      && !plr->riding
      && !is_aware(mon)
      && mon_is_type(mon->race, SUMMON_RING_BEARER) )
    {
        radius = AAF_LIMIT_RING;
    }

    if (mon->cdis + dun->plr_dis <= radius)
        test = TRUE;
    else if (mon->cdis <= MAX_SIGHT && (point_los(mon->pos, dun->flow_pos) || (plr->cursed & OFC_AGGRAVATE)))
        test = TRUE;
    else if (mon_has_valid_target(mon)) test = TRUE;
    else if (mon->pack)
    {
        if (mon->pack->ai == AI_WANDER || mon->pack->ai == AI_HUNT)
            test = TRUE;
    }

    /* Do nothing */
    if (!test) return;

    if (plr->riding == mon->id)
        speed = plr->pspeed;
    else
        speed = mon->mspeed;

    /* Give this monster some energy */
    mon->energy_need -= speed_to_energy(speed);

    /* Check for SI now in case the monster is sleeping or lacks enough
     * energy to move. Should the monster move or attack, we'll make another
     * (overriding) check again later. */
    if (mon_is_invisible(mon) && plr->see_inv)
        update_mon(mon, FALSE);

    /* Not enough energy to move */
    if (mon->energy_need > 0) return;

    vec_push(_monsters, mon);
}
static void dun_process_monsters(dun_ptr dun)
{
    int_map_iter_ptr iter;

    mon_fight = FALSE; /* XXX what is this? */

    if (!_monsters) _monsters = vec_alloc(NULL);
    assert(vec_length(_monsters) == 0);

    /* Phase 1: Gather all monsters that get a move this game turn.
     * This is optimized, usually avoiding any mallocs, and safe,
     * as no new monsters are added and no existing monsters removed
     * during phase 1.*/
    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        _preprocess_mon(dun, mon);
    }
    int_map_iter_free(iter);

    /* Phase 2: Process monsters having enough energy to move.
     * Here, unprocessed monsters may be killed before we get to them
     * (mon_is_dead), but newly summoned monsters won't be noticed
     * until the next game turn. */
    while (vec_length(_monsters))
    {
        mon_ptr mon = vec_pop(_monsters);

        if (!mon_is_valid(mon)) continue;
        if (dun != mon->dun) continue; /* earlier processed monster did teleport level on this guy? (e.g. Nodens) */

        mon->energy_need += ENERGY_NEED();
        _process_mon(dun, mon);
        mon->turns++; /* XXX restore old MFLAG_NICE behaviour ... cf _can_cast */
        if (mon->dun == dun) /* XXX check if monster took stairs */
            mon_tim_fast_tick(mon); /* timers go after monster moves (e.g. T_PARALYZED) */

        mon->pain = 0; /* XXX pain cancels fear hack; avoid msg spam and bool *fear parameters ... */

        if (!plr->playing || plr->is_dead) break;
        if (cave != dun) break; /* XXX nexus travel or some such ... need to remove "cave" */
    }
    vec_clear(_monsters);
}
static void dun_process_plr(dun_ptr dun)
{
    assert(cave == dun);
    process_player();
}
static void _regen_mon(int id, mon_ptr mon)
{
    if (mon->hp < mon->maxhp)
    {
        int amt = mon->maxhp / 100;

        if (!amt) if (one_in_(2)) amt = 1;
        if (mon_can_regen(mon)) amt *= 2;
        if (amt >= 400) amt = 400;

        mon->hp += amt;
        if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;
        check_mon_health_redraw(mon);
    }
}
static void dun_regen_monsters(dun_ptr dun) { dun_iter_mon(dun, _regen_mon); }
static void dun_process_world(dun_ptr dun)
{
    if (dun->turn % TURNS_PER_TICK == 0)
        dun_tim_tick(dun);

    if (dun->id == plr->dun_id)
    {
        assert(cave == dun);
        if (cave == dun)
            process_world();
    }
    else
    {
        if (dun->turn % (TURNS_PER_TICK*10) == 0)
            dun_regen_monsters(dun);
    }
}
static void _hack_stuff(dun_ptr dun)
{
    if (plr->dun_id == dun->id)
    {
        notice_stuff();
        handle_stuff();
        move_cursor_relative(plr->pos);
        if (fresh_after) Term_fresh();
    }
}
static void _update_mon_distance(dun_ptr dun)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        /*mon->cdis = point_fast_distance(dun->flow_pos, mon->pos);*/
        update_mon(mon, TRUE); /* XXX need mon->ml updated as well */
    }
    int_map_iter_free(iter);
}
void dun_update_flow(dun_ptr dun)
{
    _update_mon_distance(dun);
    if (!dun->flow) dun->flow = dun_flow_calc(dun, dun->flow_pos, 30, NULL);
    else
    {
        bool skip = FALSE;
        if (dun->id == plr->dun_id && (running || travel.run))
        {
            if ( dun_pos_interior(dun, dun->flow->pos)
              && plr_view(dun->flow->pos)
              && point_fast_distance(plr->pos, dun->flow->pos) < 10 )
            {
                skip = TRUE;
            }
        }
        if (!skip)
            dun_flow_recalc(dun->flow, dun->flow_pos);
    }
    dun->flags &= ~DF_UPDATE_FLOW;
}
static void _pack_reflow(int id, mon_pack_ptr pack)
{
    assert(pack->flow);
    dun_flow_recalc(pack->flow, pack->flow->pos);
}
static void _update_flows(dun_ptr dun)
{
    point_map_iter_ptr iter;
    for (iter = point_map_iter_alloc(dun->flows);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        dun_flow_ptr flow = point_map_iter_current(iter);
        assert(point_equals(flow->pos, point_map_iter_current_key(iter)));
        dun_flow_recalc(flow, flow->pos);
    }
    point_map_iter_free(iter);
}
void dun_update_mon_flow(dun_ptr dun)
{
    /* dungeon pathfinding has been invalidated due to terrain alteration (PU_MON_FLOW).
     * We need to recalculate (lazily) any monster or pack flows */
    int_map_ptr packs = int_map_alloc(NULL);
    int_map_iter_ptr iter;
    dun_stairs_ptr stairs;

    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (mon->flow) dun_flow_recalc(mon->flow, mon->pos);
        if (mon->pack)
        {
            if (mon->pack->flow) int_map_add(packs, mon->pack->id, mon->pack);
        }
    }
    int_map_iter_free(iter);
    int_map_iter(packs, (int_map_iter_f)_pack_reflow);
    int_map_free(packs);

    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        if (stairs->flow)
        {
            assert(point_equals(stairs->pos_here, stairs->flow->pos));
            dun_flow_recalc(stairs->flow, stairs->pos_here);
        }
    }
    _update_flows(dun); /* dun->flows */
}
dun_flow_ptr dun_find_flow_at(dun_ptr dun, point_t pos)
{
    dun_flow_ptr flow = point_map_find(dun->flows, pos);
    if (!flow)
    {
        flow = dun_flow_calc(dun, pos, MON_WANDER_RAD, NULL);
        point_map_add(dun->flows, pos, flow);
    }
    return flow;
}
void dun_forget_flow_at(dun_ptr dun, point_t pos)
{
    point_map_delete(dun->flows, pos);
}
static void dun_process_aux(dun_ptr dun)
{
    assert(cave == dun);

    if (dun->flags & DF_AUTOSAVE)
    {
        do_cmd_save_game(TRUE);
        dun->flags &= ~DF_AUTOSAVE;
    }

    dun->turn++;
    if (dun->flags & DF_UPDATE_FLOW)
        dun_update_flow(dun);

    if (plr->dun_id == dun->id)
    {
        dun_process_plr(dun);
        if (cave != dun) /* detect plr level change (stairs, trapdoor, etc.) */
        {
            _hack_stuff(cave);
            return; /* we abort because old code needs cave == dun to work */
        }
        _hack_stuff(dun);
    }
    if (!plr->playing || plr->is_dead) return;
    dun_process_monsters(dun);
    if (cave != dun) /* detect plr level change (nexus travel or teleport level) */
    {
        _hack_stuff(cave);
        return; /* we abort because old code needs cave == dun to work */
    }
    _hack_stuff(dun);
    if (!plr->playing || plr->is_dead) return;
    dun_process_world(dun);
    _hack_stuff(dun);
}
void dun_process(dun_ptr dun)
{
    cave = dun;
    dun_process_aux(dun);
    if (cave == dun) /* e.g. dun_mgr_teleport_town */
    {
        vec_clear(dun->graveyard);
        vec_clear(dun->junkpile);
    }
}
void dun_mgr_process(void)
{
    dun_mgr_ptr dm = dun_mgr();
    plr_hook_startup();
    for (;;)
    {
        dun_ptr plr_dun = dun_mgr_dun(plr->dun_id);
        dun_stairs_ptr stairs;

        cave = plr_dun;
        dm->turn++;
        if (plr_dun->flags & DF_GC)
        {
            dun_mgr_gc(TRUE);
            plr_dun->flags &= ~DF_GC;
        }
        else if (dm->turn % 100 == 0)
            dun_mgr_gc(FALSE);

        /* process plr's level */
        plr_dun->plr_dis = 0;
        dun_process(plr_dun);
        if (plr->is_dead || !plr->playing) break;
        if (dun_mgr_dun(plr->dun_id) != plr_dun) continue; /* detect level change (safely) */

        /* process nearby levels */
        for (stairs = plr_dun->stairs; stairs; stairs = stairs->next)
        {
            int dis = point_fast_distance(stairs->pos_here, plr->pos);
            dun_ptr adj_dun = NULL;
            if (dis > MAX_SIGHT) continue;
            if (stairs->dun_type_id == D_QUEST) continue;
            if (stairs->dun_id) adj_dun = dun_mgr_dun(stairs->dun_id);
            if (!adj_dun)
            {
                adj_dun = dun_gen_connected(plr_dun, stairs);
                assert(stairs->dun_id);
            }
            assert(adj_dun);
            adj_dun->plr_dis = dis;
            dun_process(adj_dun);
        }
    }
    cave = dun_mgr_dun(plr->dun_id); /* XXX */
}

/************************************************************************
 * Garbage Collection
 ************************************************************************/
static void _unmark(int id, dun_ptr dun) { dun->flags &= ~DF_MARK; }
static void _mark(dun_ptr dun, int depth)
{
    dun_stairs_ptr stairs;
    dun->flags |= DF_MARK;
    if (depth > 1) return;
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        dun_ptr child;
        if (!stairs->dun_id) continue;
        child = dun_mgr_dun(stairs->dun_id);
        if (!child) continue;
        _mark(child, depth + 1);
    }
}
static void _sweep(void)
{
    dun_mgr_ptr      dm = dun_mgr();
    vec_ptr          v = vec_alloc(NULL);
    int_map_iter_ptr iter;
    int              i, ct;

    for (iter = int_map_iter_alloc(dm->dungeons);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_ptr dun = int_map_iter_current(iter);
        if (!(dun->flags & (DF_MARK | DF_LOCKED))) vec_push(v, dun);
    }
    int_map_iter_free(iter);

    ct = vec_length(v);
    if (ct)
    {
        for (i = 0; i < ct; i++)
        {
            dun_ptr dun = vec_get(v, i);

            assert(dun != dm->world);
            assert(!(dun->flags & DF_MARK));
            assert(!(dun->flags & DF_LOCKED));
            assert(dun != cave);

            if (dun == dm->surface) dm->surface = NULL;
            int_map_delete(dm->dungeons, dun->id);
        }
    }
    vec_free(v);
}
void dun_mgr_gc(bool force)
{
    dun_mgr_ptr dm = dun_mgr();
    int i;

    if (!force && int_map_count(dm->dungeons) < 20) return;

    int_map_iter(dm->dungeons, (int_map_iter_f)_unmark);
    for (i = dm->mru_tail; i != dm->mru_head; )
    {
        int id = dm->mru[i];
        dun_ptr dun = int_map_find(dm->dungeons, id);
        if (dun) dun->flags |= DF_MARK;
        if (++i == _MRU_LEN) i = 0;
    }
    _mark(dun_mgr_dun(plr->dun_id), 0); /* mark accessible dungeons to a fixed depth */
    _sweep();
}

/************************************************************************
 * Manager
 ************************************************************************/
static void _mru_nq(dun_mgr_ptr dm, int dun_id)
{
    dm->mru[dm->mru_head++] = dun_id;
    if (dm->mru_head == _MRU_LEN) dm->mru_head = 0;
    if (dm->mru_head == dm->mru_tail) /* overflow forgets oldest entry */
    {
        dm->mru_tail++;
        if (dm->mru_tail == _MRU_LEN) dm->mru_tail = 0;
    }
}
#if 0
static int _mru_dq(dun_mgr_ptr dm)
{
    int id;
    assert(dm->mru_head != dm->mru_tail);
    id = dm->mru[dm->mru_tail++];
    if (dm->mru_tail == _MRU_LEN) dm->mru_tail = 0;
    return id;
}
#endif
void _mru_clear(dun_mgr_ptr dm) /* XXX needed by dun_gen.c (dun_mgr_teleport_town) */
{
    int i;
    dm->mru_head = 0;
    dm->mru_tail = 0;
    for (i = 0; i < _MRU_LEN; i++)
        dm->mru[i] = 0;
}
static dun_mgr_ptr _dm = NULL; /* XXX scoped outside dun_mgr() for debugging */
dun_mgr_ptr dun_mgr(void)
{
    if (!_dm)
    {
        _dm = malloc(sizeof(dun_mgr_t));
        memset(_dm, 0, sizeof(dun_mgr_t));
        _dm->next_dun_id = 1;
        _dm->next_mon_id = 1;
        _dm->next_obj_id = 1;
        _dm->next_pack_id = 1;
        _dm->dungeons = int_map_alloc((int_map_free_f)dun_free);
        _dm->packs = int_map_alloc((int_map_free_f)mon_pack_free);
        #ifdef DEVELOPER
        _dm->prof = malloc(sizeof(dun_mgr_prof_t));
        memset(_dm->prof, 0, sizeof(dun_mgr_prof_t));
        _dm->prof->run_timer = z_timer_create(); /* always running */
        _dm->prof->view_timer.paused = TRUE;
        _dm->prof->light_timer.paused = TRUE;
        _dm->prof->flow_timer.paused = TRUE;
        _dm->prof->gen_timer.paused = TRUE;
        _dm->prof->redraw_timer.paused = TRUE;
        _dm->prof->wall_time = time(NULL);
        /* XXX Note: Adding a timer for mon_process will overlap with the flow_timer,
         * giving some misleading results. (Flows are "lazy", and are generally realized
         * via mon_ai). */
        #endif
    }
    return _dm;
}
static void _doc_timer(doc_ptr doc, z_timer_ptr timer, double total)
{
    double elapsed = z_timer_elapsed(timer);
    doc_printf(doc, "%4.1f%%", elapsed * 100./total);
    if (timer->counter)
        doc_printf(doc, " %6.3fms (%d)", elapsed/timer->counter, timer->counter);
    else
        doc_printf(doc, " %6.3fms", elapsed);
}

void dun_mgr_doc(doc_ptr doc)
{
    #ifdef DEVELOPER
    dun_mgr_ptr dm = dun_mgr();
    doc_ptr cols[2];
    cols[0] = doc_alloc(30);
    cols[1] = doc_alloc(50);
    doc_insert(cols[0], " <color:U>Dungeon Manager Statistics</color>\n");
    doc_printf(cols[0], "   next_dun_id = %d\n", dm->next_dun_id);
    doc_printf(cols[0], "   next_mon_id = %d\n", dm->next_mon_id);
    doc_printf(cols[0], "   next_obj_id = %d\n", dm->next_obj_id);
    doc_printf(cols[0], "   turn        = %d\n", dm->turn);
    doc_printf(cols[0], "   active duns = %d\n", int_map_count(dm->dungeons));
    if (dm->prof)
    {
        double total = z_timer_elapsed(&dm->prof->run_timer);
        double used = z_timer_elapsed(&dm->prof->view_timer)
                    + z_timer_elapsed(&dm->prof->light_timer)
                    + z_timer_elapsed(&dm->prof->flow_timer)
                    + z_timer_elapsed(&dm->prof->gen_timer)
                    + z_timer_elapsed(&dm->prof->redraw_timer);
        double wall_time = difftime(time(NULL), dm->prof->wall_time);

        doc_insert(cols[1], " <color:U>Performance Statistics</color>\n");

        doc_insert(cols[1], "   View  = ");
        _doc_timer(cols[1], &dm->prof->view_timer, used);
        if (dm->prof->view_timer.counter)
            doc_printf(cols[1], " (<color:R>%.1f</color>)", (double)dm->prof->view_count/dm->prof->view_timer.counter);
        doc_newline(cols[1]);

        doc_insert(cols[1], "   Light = ");
        _doc_timer(cols[1], &dm->prof->light_timer, used);
        doc_newline(cols[1]);

        doc_insert(cols[1], "   Flow  = ");
        _doc_timer(cols[1], &dm->prof->flow_timer, used);
        doc_newline(cols[1]);

        doc_insert(cols[1], "   Gen   = ");
        _doc_timer(cols[1], &dm->prof->gen_timer, used);
        doc_newline(cols[1]);

        doc_insert(cols[1], "   Draw  = ");
        _doc_timer(cols[1], &dm->prof->redraw_timer, used);
        doc_newline(cols[1]);

        doc_printf(cols[1], "   Sum   = %6.3fs <color:R>%4.1f%%</color>\n", used / 1000., used * 100. / total); 
        doc_printf(cols[1], "   Total = %6.3fs <color:R>%4.1f%%</color>\n", total / 1000., total * 100. / (1000. * wall_time)); 
     }
    doc_insert_cols(doc, cols, 2, 0);
    doc_free(cols[0]);
    doc_free(cols[1]);
    #endif
}
int dun_mgr_next_mon_id(void)
{
    dun_mgr_ptr dm = dun_mgr();
    int id = dm->next_mon_id++;
    if (!id) id = dm->next_mon_id++;
    return id;
}
int dun_mgr_next_obj_id(void)
{
    dun_mgr_ptr dm = dun_mgr();
    int id = dm->next_obj_id++;
    if (!id) id = dm->next_obj_id++;
    return id;
}
int dun_mgr_next_dun_id(void)
{
    dun_mgr_ptr dm = dun_mgr();
    int id = dm->next_dun_id++;
    if (!id) id = dm->next_dun_id++;
    return id;
}
int dun_mgr_next_pack_id(void)
{
    dun_mgr_ptr dm = dun_mgr();
    int id = dm->next_pack_id++;
    if (!id) id = dm->next_pack_id++;
    return id;
}
dun_ptr dun_mgr_dun(int id)
{
    return int_map_find(dun_mgr()->dungeons, id);
}
mon_pack_ptr dun_mgr_pack(int id)
{
    return int_map_find(dun_mgr()->packs, id);
}
mon_pack_ptr plr_pack(void)
{
    dun_mgr_ptr dm = dun_mgr();
    mon_pack_ptr pack = NULL;
    if (dm->plr_pack_id)
        pack = int_map_find(dm->packs, dm->plr_pack_id);
    if (!pack)
    {
        pack = mon_pack_alloc();
        pack->ai = AI_PETS;
        pack->id = dun_mgr_next_pack_id();
        dm->plr_pack_id = pack->id;
        int_map_add(dm->packs, pack->id, pack);
    }
    return pack;
}
dun_ptr dun_mgr_alloc_dun(rect_t rect)
{
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr     dun;

    if (rect_is_valid(rect))
        dun = dun_alloc(dun_mgr_next_dun_id(), rect);
    else
        dun = dun_alloc_aux(dun_mgr_next_dun_id());

    int_map_add(dm->dungeons, dun->id, dun);
    return dun;
}
void dun_mgr_delete_surface(void)
{
    dun_mgr_ptr dm = dun_mgr();
    int i;
    if (!dm->surface) return;
    for (i = 0; i < _MRU_LEN; i++)
    {
        if (dm->mru[i] == dm->surface->id)
            dm->mru[i] = 0; /* lazy */
    }
    int_map_delete(dm->dungeons, dm->surface->id);
    dm->surface = NULL;
}
mon_pack_ptr dun_mgr_alloc_pack(void)
{
    dun_mgr_ptr dm = dun_mgr();
    mon_pack_ptr pack = mon_pack_alloc();
    pack->id = dun_mgr_next_pack_id();
    int_map_add(dm->packs, pack->id, pack);
    return pack;
}
void dun_mgr_free_pack(mon_pack_ptr pack)
{
    assert(pack->ai != AI_PETS);
    if (pack->ai == AI_PETS) return; /* keep this pack around forever */
    int_map_delete(dun_mgr()->packs, pack->id);
}
static mon_ptr _find_unique(int race_id)
{
    mon_ptr mon = NULL;
    dun_mgr_ptr dm = dun_mgr();
    int_map_iter_ptr i, j;
    for (i = int_map_iter_alloc(dm->dungeons);
            int_map_iter_is_valid(i) && !mon;
            int_map_iter_next(i))
    {
        dun_ptr dun = int_map_iter_current(i);
        for (j = int_map_iter_alloc(dun->mon);
                int_map_iter_is_valid(j) && !mon;
                int_map_iter_next(j))
        {
            mon_ptr m = int_map_iter_current(j);
            if (m->race->id == race_id) mon = m;
        }
        int_map_iter_free(j);
    }
    int_map_iter_free(i);
    return mon;
}
mon_ptr dun_mgr_relocate_unique(int race_id, dun_ptr dun, point_t pos)
{
    mon_ptr mon = _find_unique(race_id);
    if (!mon) return NULL;
    dun_detach_mon(mon->dun, mon->id);
    dun_place_mon(dun, mon, pos);
    return mon;
}
int dun_mgr_count_mon_race(int race_id)
{
    dun_mgr_ptr dm = dun_mgr();
    int_map_iter_ptr i, j;
    int ct = 0, k;
    for (i = int_map_iter_alloc(dm->dungeons);
            int_map_iter_is_valid(i);
            int_map_iter_next(i))
    {
        dun_ptr dun = int_map_iter_current(i);
        for (j = int_map_iter_alloc(dun->mon);
                int_map_iter_is_valid(j);
                int_map_iter_next(j))
        {
            mon_ptr m = int_map_iter_current(j);
            if (mon_true_race(m)->id == race_id)
                ct++;
        }
        int_map_iter_free(j);
        for (k = 0; k < vec_length(dun->graveyard); k++)
        {
            mon_ptr m = vec_get(dun->graveyard, k);
            if (mon_true_race(m)->id == race_id)
                ct++;
        }
    }
    int_map_iter_free(i);
    return ct;
}
bool dun_mgr_recall_plr(void)
{
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr     current = int_map_find(dm->dungeons, plr->dun_id);
    dun_ptr     recall = NULL;

    if (!plr_can_recall())
    {
        msg_print("Nothing happens!");
        return FALSE;
    }
    if (!quests_check_leave()) return FALSE;
    if (current->type->id == D_SURFACE)
    {
        dun_type_ptr type = dun_types_choose("Recall to Which Dungeon?", FALSE);
        if (!type) return FALSE;
        if (!dun_pos_interior(dm->world, type->world_pos))
        {
            msg_format("%s does not exist in this world.", type->name);
            return FALSE;
        }

        plr->old_pos = plr->pos; /* remember for return recall */
        _mru_clear(dm);
        recall = dun_gen_wizard(type->id, type->plr_max_lvl);
        energy_use = 0; /* replaces MFLAG_NICE */
    }
    else
    {
        _mru_clear(dm);
        recall = dun_gen_surface(dun_world_pos(plr->old_pos));
        assert(dm->surface == recall);
        dun_mgr_plr_change_dun(recall, plr->old_pos);
    }
    assert(recall);
    recall->flags |= DF_GC;
    return TRUE;
}
void dun_mgr_wizard_jump(int dun_type_id, int dun_lvl)
{
    dun_type_ptr dun_type = dun_types_lookup(dun_type_id);

    if (!dun_pos_interior(dun_mgr()->world, dun_type->world_pos))
    {
        msg_format("%s does not exist in this world.", dun_type->name);
        return;
    }

    if (plr->dun_id && dun_mgr_dun(plr->dun_id)->type->id == D_SURFACE)
        plr->old_pos = plr->pos;

    if (dun_type_id == D_SURFACE)
    {
        dun_mgr_recall_plr();
        return;
    }

    if (dun_lvl < dun_type->min_dun_lvl) dun_lvl = dun_type->min_dun_lvl;
    if (dun_lvl > dun_type->max_dun_lvl) dun_lvl = dun_type->max_dun_lvl;

    _mru_clear(dun_mgr());
    dun_gen_wizard(dun_type_id, dun_lvl);
}
void dun_mgr_plr_change_dun(dun_ptr new_dun, point_t new_pos)
{
    dun_type_ptr type = NULL;
    dun_mgr_ptr  dm = dun_mgr();
    mon_ptr      mount = plr_riding_mon();

    assert(dun_pos_valid(new_dun, new_pos));
    if (cave)
    {
        quests_on_leave();

        /* XXX hacks for old system ... needs cleanup */

        command_cmd = 0;
        command_rep = 0;
        command_arg = 0;
        command_dir = 0;
        travel.pos = point_create(0, 0);
        travel.last_pos = point_create(0, 0);
        plr->health_who = 0;
        shimmer_monsters = TRUE;
        shimmer_objects = TRUE;
        repair_monsters = TRUE;
        repair_objects = TRUE;
        disturb(1, 0);
        if (plr->max_plv < plr->lev) plr->max_plv = plr->lev; /* XXX Really? */

        /* XXX plr_hook_change_dun ... eg, plr->painted_target_idx, duelist?
         * (aside: fleeing the level with an un-finished duel should trash your honor!) */
    }
    /* Track Max Depth */
    switch (new_dun->type->id)
    {
    case D_SURFACE:
    case D_QUEST:
    case D_WORLD: break;
    default:
        type = dun_types_lookup(new_dun->type->id);
        if (new_dun->dun_lvl > type->plr_max_lvl)
        {
            type->plr_max_lvl = new_dun->dun_lvl;
            type->flags.plr |= DF_PLR_ENTERED;
        }
    }

    /* keep mru up to date */
    _mru_nq(dm, new_dun->id);

    /* Re-locate the player. This should be the only place in the system
     * that actually places the plr in a dungeon (setting plr->dun_id),
     * except for dun_mgr_load, of course. */
    cave = new_dun;
    plr->dun_id = new_dun->id;
    plr->pos = new_pos;
    plr->last_pos = plr->pos;
    if (mount)
    {
        dun_detach_mon(mount->dun, mount->id);
        dun_place_mon(new_dun, mount, new_pos);
    }
    new_dun->flow_pos = new_pos;
    dun_update_flow(new_dun);
    mon_alloc_clear_filters();
    viewport_verify();

    if (!(new_dun->flags & DF_VISITED))
    {
        new_dun->flags |= DF_VISITED;
        new_dun->feeling_delay = plr_feeling_delay(new_dun);
    }
    if (type)
    {
        if (new_dun->flags & DF_RECALL)
            type->last_recall = 0;
        if (new_dun->flags & DF_SHOP)
            type->last_shop = 0;
    }

    /* hook for dungeon guardians or special restrictions */
    if (type && type->change_dun_f)
        type->change_dun_f(type, new_dun);

    /* hook for quest monsters */
    if (new_dun->type->id == D_QUEST)
        quests_on_enter_fixed(new_dun->quest_id);
    else
        quests_on_enter(new_dun->type->id, new_dun->dun_lvl);

    if (autosave_l && !statistics_hack)
        new_dun->flags |= DF_AUTOSAVE; /* later ... once the stack unwinds to the next dun_mgr_process() */

    /* XXX do_cmd_save_game does a handle_stuff ... Saving currently *requires* forget_view et. al. */
    plr->window |= PW_MONSTER_LIST | PW_OBJECT_LIST | PW_MONSTER | PW_OVERHEAD | PW_DUNGEON | PW_WORLD_MAP;
    plr->redraw |= PR_BASIC | PR_EXTRA | PR_EQUIPPY | PR_MSG_LINE | PR_MAP;
    plr->update |= PU_VIEW | PU_LIGHT | PU_MON_LIGHT | PU_TORCH;
    plr->update |= PU_MONSTERS;
}
/************************************************************************
 * Savefiles
 ************************************************************************/
/* 1. Save dun_page_t using RLE encoding for terrain. Rely on page->grids allocation
 *    as a single contiguous block. */
typedef struct {
    int  index, count;
    dun_grid_t grid;
} _template_t, *_template_ptr;
static int _compare_template(_template_ptr l, _template_ptr r) /*descending on count */
{
    if (l->count < r->count) return 1;
    if (l->count > r->count) return -1;
    return 0;
}
static _template_ptr _find_template(vec_ptr v, dun_grid_ptr grid) /* O(n) */
{
    int i;
    _template_ptr t;
    for (i = 0; i < vec_length(v); i++)
    {
        t = vec_get(v, i);
        /* if (grid == t->grid) ... */
        if ( t->grid.type == grid->type
          && t->grid.subtype == grid->subtype
          && t->grid.parm1 == grid->parm1
          && t->grid.parm2 == grid->parm2
          && t->grid.flags == grid->flags )
        {
            return t;
        }
    }
    t = malloc(sizeof(_template_t));
    t->index = -1;    /* XXX index is set after sorting */
    t->grid = *grid;
    t->count = 0;
    vec_add(v, t);
    return t;
}
static void _point_save(point_t pos, savefile_ptr file)
{
    savefile_write_s16b(file, pos.x);
    savefile_write_s16b(file, pos.y);
}
static point_t _point_load(savefile_ptr file)
{
    point_t pos;
    pos.x = savefile_read_s16b(file);
    pos.y = savefile_read_s16b(file);
    return pos;
}
static void _rect_save(rect_t rect, savefile_ptr file)
{
    savefile_write_s16b(file, rect.x);
    savefile_write_s16b(file, rect.y);
    savefile_write_s16b(file, rect.cx);
    savefile_write_s16b(file, rect.cy);
}
static rect_t _rect_load(savefile_ptr file)
{
    rect_t rect;
    rect.x = savefile_read_s16b(file);
    rect.y = savefile_read_s16b(file);
    rect.cx = savefile_read_s16b(file);
    rect.cy = savefile_read_s16b(file);
    return rect;
}
static void _grid_save(dun_grid_ptr grid, savefile_ptr file)
{
    savefile_write_byte(file, grid->type);
    savefile_write_byte(file, grid->subtype);
    savefile_write_byte(file, grid->parm1);
    savefile_write_byte(file, grid->parm2);
    savefile_write_u32b(file, grid->flags);
}
static dun_grid_t _grid_load(savefile_ptr file)
{
    dun_grid_t g;
    g.type = savefile_read_byte(file);
    g.subtype = savefile_read_byte(file);
    g.parm1 = savefile_read_byte(file);
    g.parm2 = savefile_read_byte(file);
    g.flags = savefile_read_u32b(file);
    return g;
}
static void _dun_page_load(dun_page_ptr page, savefile_ptr file)
{
    vec_ptr v = vec_alloc(free);
    int i, ct, index;
    byte tmp;
    u32b check;
    _template_ptr t;
    dun_grid_ptr grid = page->grids; /* XXX assumes single block */

    assert(rect_is_valid(page->rect)); /* already done by dun_load */
    assert(page->grids);

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        t = malloc(sizeof(_template_t));
        t->index = i;
        t->grid = _grid_load(file);
        t->count = 0;
        vec_add(v, t);
    }

    for (;;)
    {
        ct = savefile_read_byte(file);
        if (!ct) break; /* 0 count ends RLE encoding */
        index = 0;
        do
        {
            tmp = savefile_read_byte(file);
            index += tmp;
        } while (tmp == MAX_UCHAR);
        t = vec_get(v, index);

        for (i = 0; i < ct; i++)
        {
            *grid = t->grid;
            grid++;
        }
    }
    check = savefile_read_u32b(file);
    assert(check == 0xdeadbeef); /* paranoia */
    if (check != 0xdeadbeef) quit("Corrupted savefile in _dun_page_load");
    vec_free(v);
}
static void _dun_page_save(dun_page_ptr page, savefile_ptr file)
{
    vec_ptr v = vec_alloc(free);
    point_t pt;
    int i, count = 0;
    _template_ptr prev = NULL;
    dun_grid_ptr grid = page->grids; /* XXX assumes single block */

    _rect_save(page->rect, file);
    for (pt.y = page->rect.y; pt.y < page->rect.y + page->rect.cy; pt.y++)
    {
        for (pt.x = page->rect.x; pt.x < page->rect.x + page->rect.cx; pt.x++)
        {
            _find_template(v, grid)->count++;
            grid++;
        }
    }
    vec_sort(v, (vec_cmp_f)_compare_template);
    savefile_write_s16b(file, vec_length(v));
    for (i = 0; i < vec_length(v); i++)
    {
        _template_ptr t = vec_get(v, i);
        t->index = i; /* XXX set the id after sorting */
        _grid_save(&t->grid, file);
    }
    grid = page->grids;
    for (pt.y = page->rect.y; pt.y < page->rect.y + page->rect.cy; pt.y++)
    {
        for (pt.x = page->rect.x; pt.x < page->rect.x + page->rect.cx; pt.x++)
        {
            _template_ptr t = _find_template(v, grid);

            if (!prev) prev = t; /* first */
            if (prev != t || count == MAX_UCHAR)
            {
                int index = prev->index;
                assert(count);
                savefile_write_byte(file, count);
                while (index >= MAX_UCHAR)
                {
                    savefile_write_byte(file, MAX_UCHAR);
                    index -= MAX_UCHAR;
                }
                savefile_write_byte(file, index);
                prev = t;
                count = 1;
            }
            else count++;
            grid++;
        }
    }
    if (count)
    {
        int index = prev->index;
        savefile_write_byte(file, count);
        while (index >= MAX_UCHAR)
        {
            savefile_write_byte(file, MAX_UCHAR);
            index -= MAX_UCHAR;
        }
        savefile_write_byte(file, index);
    }
    savefile_write_byte(file, 0);
    savefile_write_u32b(file, 0xdeadbeef); /* paranoia */
    vec_free(v);
}
/* 2. Save dun_t */
static int _obj_id(obj_ptr obj)
{
    switch (obj->loc.where)
    {
    case INV_FLOOR:
        return obj->loc.v.floor.obj_id;
    case INV_MON_PACK:
        return obj->loc.v.mon_pack.obj_id;
    }
    return 0; /* bug */
}
void dun_load(dun_ptr dun, savefile_ptr file)
{
    int ct = 0, i;

    dun->type = dun_types_lookup(savefile_read_s16b(file));
    dun->dun_lvl = savefile_read_s16b(file);
    dun->difficulty = savefile_read_s16b(file);
    dun->quest_id = savefile_read_s16b(file);
    dun->turn = savefile_read_u32b(file);
    dun->rect = _rect_load(file);

    /* pages */
    ct = savefile_read_s16b(file);
    assert(!dun->page);
    for (i = 0; i < ct; i++)
    {
        rect_t rect = _rect_load(file);
        dun_page_ptr p = dun_page_alloc(rect);
        _dun_page_load(p, file);
        p->next = dun->page;
        if (dun->page)
            dun->page->prev = p;
        dun->page = p;
    }
    /* objects */
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        obj_ptr obj = obj_alloc();
        obj_load(obj, file);
        int_map_add(dun->obj, _obj_id(obj), obj);
    }
    /* object positions (piles) for INV_FLOOR */
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        point_t pos = _point_load(file);
        obj_ptr pile = NULL;
        for (;;)
        {
            obj_ptr obj;
            int id = savefile_read_s32b(file);
            if (!id) break;
            obj = int_map_find(dun->obj, id);
            assert(obj);
            obj->next = pile;
            pile = obj;
        }
        assert(pile);
        assert(pile->loc.where == INV_FLOOR);
        point_map_add(dun->obj_pos, pos, pile);
    }
    /* monsters */
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        mon_ptr mon = mon_alloc();
        mon_load(mon, file);
        mon->dun = dun;
        mon_true_race(mon)->alloc.cur_num++; /* XXX cur_num is not stored in the savefile */
        int_map_add(dun->mon, mon->id, mon);
        if (mon->mflag2 & MFLAG2_HUNTED)
            mon->flow = dun_flow_calc(dun, mon->pos, MON_HUNT_RAD, NULL);
        if (mon->id == plr->duelist_target_idx)
            plr->duelist_target = who_create_mon(mon);
    }
    /* monster positions (and carried objects) */
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        point_t pos = _point_load(file);
        int     id = savefile_read_s32b(file);
        mon_ptr mon = int_map_find(dun->mon, id);
        obj_ptr pile = NULL;
        for (;;)
        {
            obj_ptr obj;
            id = savefile_read_s32b(file);
            if (!id) break;
            obj = int_map_find(dun->obj, id);
            assert(obj);
            obj->next = pile;
            pile = obj;
        }
        assert(mon);
        mon->obj = pile;
        point_map_add(dun->mon_pos, pos, mon);
    }
    /* stairs */
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        dun_stairs_ptr stairs = malloc(sizeof(dun_stairs_t));
        memset(stairs, 0, sizeof(dun_stairs_t));
        stairs->pos_here = _point_load(file);
        stairs->pos_there = _point_load(file);
        stairs->dun_id = savefile_read_s16b(file);
        stairs->dun_type_id = savefile_read_s16b(file);
        stairs->dun_lvl = savefile_read_s16b(file);
        stairs->next = dun->stairs;
        dun->stairs = stairs;
    }
    dun->flow_pos = _point_load(file);
    dun->flags = savefile_read_u32b(file);
    dun->breed_ct = savefile_read_s16b(file);
    dun->breed_kill_ct = savefile_read_s16b(file);
    dun->feeling = savefile_read_byte(file);
    dun->feeling_delay = savefile_read_s16b(file);
    dun->ambient_light = savefile_read_s16b(file);

    if (dun_pos_interior(dun, dun->flow_pos)) /* XXX *after* reading savefile flags! */
        dun->flags |= DF_UPDATE_FLOW;

    if (dun->flags & DF_SHOP)
    {
        dun->town = town_alloc(TOWN_RANDOM, "Random");
        town_load(dun->town, file);
    }
    dun_tim_load(dun, file);

    /* XXX Detect and recover broken savefiles. For example, I received a file
     * where an object pile was in dun->obj_pos under two different locations. */
    if (!dun_obj_integrity(dun))
        dun_obj_panic(dun);
}
static savefile_ptr _file = NULL;
static void _obj_save(int id, obj_ptr obj) { obj_save(obj, _file); }
static void _mon_save(int id, mon_ptr mon) { mon_save(mon, _file); }
static void _obj_pos_save(point_t pos, obj_ptr obj)
{
    _point_save(pos, _file);
    for (; obj; obj = obj->next)
        savefile_write_s32b(_file, _obj_id(obj));
    savefile_write_s32b(_file, 0);
}
static void _mon_pos_save(point_t pos, mon_ptr mon)
{
    obj_ptr obj;
    _point_save(pos, _file);
    savefile_write_s32b(_file, mon->id);
    for (obj = mon->obj; obj; obj = obj->next)
        savefile_write_s32b(_file, _obj_id(obj));
    savefile_write_s32b(_file, 0);
}
static void _stairs_save(dun_ptr dun, savefile_ptr file)
{
    dun_stairs_ptr stairs;
    int ct = 0;
    for (stairs = dun->stairs; stairs; stairs = stairs->next) ct++;
    savefile_write_s16b(file, ct);
    for (stairs = dun->stairs; stairs; stairs = stairs->next)
    {
        _point_save(stairs->pos_here, file);
        _point_save(stairs->pos_there, file);
        savefile_write_s16b(file, stairs->dun_id);
        savefile_write_s16b(file, stairs->dun_type_id);
        savefile_write_s16b(file, stairs->dun_lvl);
    }
}
void dun_save(dun_ptr dun, savefile_ptr file)
{
    dun_page_ptr p;
    int ct = 0;

    savefile_write_s16b(file, dun->type->id);
    savefile_write_s16b(file, dun->dun_lvl);
    savefile_write_s16b(file, dun->difficulty);
    savefile_write_s16b(file, dun->quest_id);
    savefile_write_u32b(file, dun->turn);
    _rect_save(dun->rect, file);

    /* pages */
    for (p = dun->page; p; p = p->next)
        ct++;
    savefile_write_s16b(file, ct);
    for (p = dun->page; p; p = p->next)
        _dun_page_save(p, file);

    /* object and monster maps */
    _file = file;
    savefile_write_s16b(file, int_map_count(dun->obj));
    int_map_iter(dun->obj, (int_map_iter_f)_obj_save);

    savefile_write_s16b(file, point_map_count(dun->obj_pos));
    point_map_iter(dun->obj_pos, (point_map_iter_f)_obj_pos_save);

    savefile_write_s16b(file, int_map_count(dun->mon));
    int_map_iter(dun->mon, (int_map_iter_f)_mon_save);

    savefile_write_s16b(file, point_map_count(dun->mon_pos));
    point_map_iter(dun->mon_pos, (point_map_iter_f)_mon_pos_save);
    _file = NULL;

    /* stairs */
    _stairs_save(dun, file);

    _point_save(dun->flow_pos, file);
    savefile_write_u32b(file, dun->flags);
    savefile_write_s16b(file, dun->breed_ct);
    savefile_write_s16b(file, dun->breed_kill_ct);
    savefile_write_byte(file, dun->feeling);
    savefile_write_s16b(file, dun->feeling_delay);
    savefile_write_s16b(file, dun->ambient_light);

    if (dun->flags & DF_SHOP)
    {
        assert(dun->town);
        town_save(dun->town, file);
    }
    dun_tim_save(dun, file);
}
/* 2. Save dun_mgr_t */
static void _gc_packs(void) /* paranoia */
{
    dun_mgr_ptr dm = dun_mgr();
    vec_ptr v = vec_alloc(NULL);
    int_map_iter_ptr iter;
    int i;
    for (iter = int_map_iter_alloc(dm->packs);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_pack_ptr pack = int_map_iter_current(iter);
        if (pack->ai == AI_PETS) continue;
        if (!vec_length(pack->members))
            vec_add_int(v, pack->id);
    }
    int_map_iter_free(iter);

    for (i = 0; i < vec_length(v); i++)
    {
        int id = vec_get_int(v, i);
        int_map_delete(dm->packs, id);
    }
    vec_free(v);
}
static void _rebuild_packs(void)
{
    /* rebuild pack->members */
    dun_mgr_ptr dm = dun_mgr();
    mon_pack_ptr pets = plr_pack(); /* upgrade */
    int_map_iter_ptr i, j;
    for (i = int_map_iter_alloc(dm->dungeons);
            int_map_iter_is_valid(i);
            int_map_iter_next(i))
    {
        dun_ptr dun = int_map_iter_current(i);
        for (j = int_map_iter_alloc(dun->mon);
                int_map_iter_is_valid(j);
                int_map_iter_next(j))
        {
            mon_ptr mon = int_map_iter_current(j);
            if (mon_is_pet(mon)) /* upgrade ... mon->pack_id might be 0 or some pack other than pets */
            {                    /* all pets need to go into plr_pack() for dismissal, upkeep, mon_ai, etc. */
                mon->pack = pets;
                vec_push(pets->members, mon);
            }
            else if (mon->pack)
                vec_push(mon->pack->members, mon);
        }
        int_map_iter_free(j);
    }
    int_map_iter_free(i);
    _gc_packs();
    for (i = int_map_iter_alloc(dm->packs);
            int_map_iter_is_valid(i);
            int_map_iter_next(i))
    {
        mon_pack_ptr pack = int_map_iter_current(i);
        if (pack->ai == AI_WANDER)
        {
            mon_ptr mon = mon_pack_representative(pack);
            if (dun_pos_interior(mon->dun, pack->pos)) /* savefile upgrade */
                pack->flow = dun_flow_calc(mon->dun, pack->pos, MON_WANDER_RAD, NULL);
        }
    }
    int_map_iter_free(i);
}
void dun_mgr_load(savefile_ptr file)
{
    dun_mgr_ptr dm = dun_mgr();
    int i, ct;

    plr->initial_world_id = savefile_read_u16b(file);
    plr->world_id = savefile_read_u16b(file);
    plr->dun_id = savefile_read_u16b(file);
    plr->pos = _point_load(file);
    plr->last_pos = plr->pos;
    plr->old_pos = _point_load(file);
    plr->turn = savefile_read_u32b(file);

    dm->next_dun_id = savefile_read_u16b(file);
    dm->next_mon_id = savefile_read_u32b(file);
    dm->next_obj_id = savefile_read_u16b(file);
    dm->next_pack_id = savefile_read_u16b(file);
    dm->plr_pack_id = savefile_read_u16b(file);
    dm->turn = savefile_read_u32b(file);
    dm->world_seed = savefile_read_u32b(file);
    dm->world_frac = dun_frac_load(file);

    dun_types_load(file); /* dun->type requires this go first */
    dun_worlds_load(file);

    ct = savefile_read_s16b(file); /* mon->pack needs pointers to exist */
    for (i = 0; i < ct; i++)
    {
        mon_pack_ptr pack = mon_pack_alloc();
        mon_pack_load(pack, file);
        int_map_add(dm->packs, pack->id, pack);
    }

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        dun_ptr dun;
        int id = savefile_read_u16b(file);
        dun = dun_alloc_aux(id);
        dun_load(dun, file);
        int_map_add(dm->dungeons, dun->id, dun);
        if (dun->type->id == D_WORLD) dm->world = dun;
        if (dun->type->id == D_SURFACE) dm->surface = dun;
    }
    for (;;)
    {
        int id = savefile_read_u16b(file);
        if (!id) break;
        if (!int_map_find(dm->dungeons, id)) continue;
        _mru_nq(dm, id);
    }

    dun_world_reseed(dm->world_seed);
    _rebuild_packs(); /* pack->members needs to be populated */
    cave = int_map_find(dm->dungeons, plr->dun_id);
}
void dun_frac_save(dun_frac_ptr frac, savefile_ptr file)
{
    int cb, i;
    if (!frac)
    {
        _rect_save(rect_invalid(), file);
        return;
    }
    _rect_save(frac->rect, file);
    savefile_write_byte(file, frac->max);
    cb = frac->rect.cx * frac->rect.cy;
    for (i = 0; i < cb; i++)
        savefile_write_byte(file, frac->map[i]);
    savefile_write_u32b(file, 0xdeadbeef); /* paranoia */
}
dun_frac_ptr dun_frac_load(savefile_ptr file)
{
    dun_frac_ptr frac = NULL;
    rect_t       r = _rect_load(file);
    byte         max;
    int          cb, i;
    u32b         check;

    if (!rect_is_valid(r)) return NULL;
    max = savefile_read_byte(file);
    frac = dun_frac_alloc(r, max);
    cb = r.cx * r.cy;
    for (i = 0; i < cb; i++)
        frac->map[i] = savefile_read_byte(file);
    check = savefile_read_u32b(file);
    assert(check == 0xdeadbeef); /* paranoia */
    if (check != 0xdeadbeef) quit("Corrupted savefile in dun_frac_load");
    return frac;
}
void dun_mgr_save(savefile_ptr file)
{
    dun_mgr_ptr dm = dun_mgr();
    int_map_iter_ptr iter;
    int i;

    /*dun_mgr_gc(TRUE);*/

    savefile_write_u16b(file, plr->initial_world_id);
    savefile_write_u16b(file, plr->world_id);
    savefile_write_u16b(file, plr->dun_id);
    _point_save(plr->pos, file);
    _point_save(plr->old_pos, file);
    savefile_write_u32b(file, plr->turn);

    savefile_write_u16b(file, dm->next_dun_id);
    savefile_write_u32b(file, dm->next_mon_id);
    savefile_write_u16b(file, dm->next_obj_id);
    savefile_write_u16b(file, dm->next_pack_id);
    savefile_write_u16b(file, dm->plr_pack_id);
    savefile_write_u32b(file, dm->turn);
    savefile_write_u32b(file, dm->world_seed);
    dun_frac_save(dm->world_frac, file);

    dun_types_save(file); /* dun->type requires this go first */
    dun_worlds_save(file);
    savefile_write_s16b(file, int_map_count(dm->packs)); /* mon->pack will need pointers to exist */
    for (iter = int_map_iter_alloc(dm->packs);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_pack_ptr pack = int_map_iter_current(iter);
        mon_pack_save(pack, file);
    }
    int_map_iter_free(iter);

    savefile_write_s16b(file, int_map_count(dm->dungeons));
    for (iter = int_map_iter_alloc(dm->dungeons);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_ptr dun = int_map_iter_current(iter);
        savefile_write_u16b(file, dun->id);
        dun_save(dun, file);
    }
    int_map_iter_free(iter);

    for (i = dm->mru_tail; i != dm->mru_head; )
    {
        int id = dm->mru[i];
        if (int_map_find(dm->dungeons, id))
            savefile_write_u16b(file, dm->mru[i]);
        if (++i == _MRU_LEN) i = 0;
    }
    savefile_write_u16b(file, 0);
}

/************************************************************************
 * Debugging Utilities
 ************************************************************************/
static str_ptr _get_dump(dun_ptr dun)
{
    str_ptr s = str_alloc_size(dun->rect.cx * dun->rect.cy);
    point_t min = rect_top_left(dun->rect);
    point_t max = rect_bottom_right(dun->rect);
    point_t p;
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        int  current_a = -1;
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            dun_grid_ptr g = dun_grid_at(dun, p);
            term_char_t tc = term_char_create(' ', TERM_WHITE);
            bool mark = BOOL(g->flags & CELL_MAP);

            if (plr->wizard && !mark) /* e.g. ^Am for testing dun generation */
                mark = BOOL(g->flags & CELL_AWARE);

            if (mark)
                tc = cell_ascii(g);

            if (tc.a != current_a)
            {
                if (current_a >= 0 && current_a != TERM_WHITE)
                {
                    str_append_s(s, "</color>");
                }
                if (tc.a != TERM_WHITE)
                {
                    str_printf(s, "<color:%c>", attr_to_attr_char(tc.a));
                }
                current_a = tc.a;
            }
            str_append_c(s, tc.c);
        }
        if (current_a >= 0 && current_a != TERM_WHITE)
            str_append_s(s, "</color>");
        str_append_c(s, '\n');
    }
    return s;
}
void dun_dump(dun_ptr dun, cptr file, int format)
{
    str_ptr s = _get_dump(dun);
    doc_ptr    doc = doc_alloc(dun->rect.cx);
    char       buf[1024];
    FILE      *fff;

    doc_insert(doc, "<style:screenshot>");
    doc_insert(doc, str_buffer(s));
    doc_insert(doc, "</style>");

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, file);
    fff = my_fopen(buf, "w");
    if (fff)
    {
        doc_write_file(doc, fff, format);
        my_fclose(fff);
    }
    str_free(s);
    doc_free(doc);
}

/************************************************************************
 * Temporary Hacks so old code can migrate to new system
 ************************************************************************/
dun_ptr cave = NULL;
