#include "angband.h"
#include "dun.h"
#include <assert.h>

static dun_feat_ptr _find_feature(int id)
{
    return &f_info[id];
}
static dun_feat_ptr _find_feature_mimic(dun_grid_ptr g)
{
    int id = g->mimic ? g->mimic : g->feat;
    dun_feat_ptr f = _find_feature(id);
    return _find_feature(f->mimic);
}
/************************************************************************
 * Grid
 ************************************************************************/
bool dun_grid_allow_drop(dun_grid_ptr grid)
{
    if (!grid) return FALSE;
    if (grid->info & CAVE_OBJECT) return FALSE;
    return have_flag(_find_feature(grid->feat)->flags, FF_DROP);
}
dun_feat_ptr dun_grid_feat(dun_grid_ptr grid)
{
    return _find_feature(grid->feat);
}
dun_feat_ptr dun_grid_feat_mimic(dun_grid_ptr grid)
{
    return _find_feature_mimic(grid);
}
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

        assert(race->cur_num > 0);
        race->cur_num--;
        if (mon->id == target_who) target_who = 0;
        if (mon->pack_idx)
        {
            dun_mgr_ptr dm = dun_mgr();
            mon_pack_ptr pack = int_map_find(dm->packs, mon->pack_idx);
            assert(pack);
            if (pack)
            {
                pack->count--;
                if (pack->count <= 0)
                    int_map_delete(dm->packs, mon->pack_idx);
            }
        }
        mon_free(mon);
    }
}
dun_ptr dun_alloc_aux(int id) /* for savefiles and D_SURFACE */
{
    dun_ptr d = malloc(sizeof(dun_t));
    memset(d, 0, sizeof(dun_t));
    d->dun_id = id;
    d->mon = int_map_alloc((int_map_free_f)_mon_free);
    d->graveyard = vec_alloc((vec_free_f)_mon_free);
    d->obj = int_map_alloc((int_map_free_f)obj_free);
    d->junkpile = vec_alloc((vec_free_f)obj_free);
    d->mon_pos = point_map_alloc(NULL);
    d->obj_pos = point_map_alloc(NULL);
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

        if (obj_is_std_art(obj) && !obj_is_known(obj))
            a_info[obj->name1].generated = FALSE;
        if (random_artifacts && obj->name3 && !obj_is_known(obj))
            a_info[obj->name3].generated = FALSE;
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
        free(x);
    }
    dun->stairs = NULL; 
}
void _clear_grid(point_t pos, cave_ptr grid)
{
    grid->mimic = 0;
    grid->special = 0;
    grid->feat = feat_floor;
    grid->info = 0;
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
        if (mon->id == target_who) target_who = 0;
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
    dun_clear(dun);
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
dun_grid_ptr dun_grid_at(dun_ptr dun, point_t pos)
{
    dun_page_ptr p;
    assert(rect_contains_point(dun->rect, pos));
    for (p = dun->page; p; p = p->next)
    {
        assert(rect_contains(dun->rect, p->rect));
        if (rect_contains_point(p->rect, pos))
        {
            int x = pos.x - p->rect.x;
            int y = pos.y - p->rect.y;
            int i = y * p->rect.cx + x;
            return &p->grids[i];
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
                f(pos, grid);
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
                f(pos, grid);
            }
        }
    }
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
void dun_iter_rect(dun_ptr dun, rect_t rect, dun_grid_f f)
{
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
point_t dun_random_grid(dun_ptr dun, dun_grid_weight_f weight)
{
    point_t pos;
    int total = 0, roll;
    assert(weight);
    for (pos.y = dun->rect.y + 1; pos.y < dun->rect.y + dun->rect.cy - 1; pos.y++)
    {
        dun_grid_ptr grid;
        pos.x = dun->rect.x + 1;
        grid = dun_grid_at(dun, pos);
        for (; pos.x < dun->rect.x + dun->rect.cx - 1; pos.x++)
        {
            total += weight(pos, grid);
            grid++;
        }
    }
    if (!total) return point_create(-1, -1); /* client should check dun_pos_interior on the result */
    roll = randint0(total);
    for (pos.y = dun->rect.y + 1; pos.y < dun->rect.y + dun->rect.cy - 1; pos.y++)
    {
        dun_grid_ptr grid;
        pos.x = dun->rect.x + 1;
        grid = dun_grid_at(dun, pos);
        for (; pos.x < dun->rect.x + dun->rect.cx - 1; pos.x++)
        {
            roll -= weight(pos, grid);
            if (roll < 0) return pos;
            grid++;
        }
    }
    return point_create(-1, -1);  /* bug */
}
dun_grid_ex_t dun_grid_ex_at(dun_ptr dun, point_t pos)
{
    dun_grid_ex_t gx;
    gx.grid = dun_grid_at(dun, pos);
    gx.feat = _find_feature(gx.grid->feat);
    gx.feat_mimic = _find_feature_mimic(gx.grid);
    gx.mon = dun_mon_at(dun, pos);
    gx.obj = dun_obj_at(dun, pos);
    gx.pos = pos;
    return gx;
}
dun_feat_ptr dun_feat_at(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid = dun_grid_at(dun, pos);
    return _find_feature(grid->feat);
}
dun_feat_ptr dun_feat_mimic_at(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid = dun_grid_at(dun, pos);
    return _find_feature_mimic(grid);
}
bool dun_allow_drop_at(dun_ptr dun, point_t pos)
{
    if (!dun_pos_interior(dun, pos)) return FALSE;
    return dun_grid_allow_drop(dun_grid_at(dun, pos));
}
bool dun_allow_mon_at(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid;
    if (!dun_pos_interior(dun, pos)) return FALSE;
    if (dun_mon_at(dun, pos)) return FALSE;
    if (dun_plr_at(dun, pos)) return FALSE;
    grid = dun_grid_at(dun, pos);
    if (grid->info & CAVE_OBJECT) return FALSE; /* XXX */
    if (!have_flag(dun_grid_feat(grid)->flags, FF_PLACE)) return FALSE;
    return TRUE;
}
bool dun_stop_disintegration_at(dun_ptr dun, point_t pos)
{
    dun_feat_ptr feat = dun_feat_at(dun, pos);
    if (have_flag(feat->flags, FF_PROJECT)) return FALSE;
    if (have_flag(feat->flags, FF_PERMANENT)) return TRUE;
    if (have_flag(feat->flags, FF_HURT_DISI)) return FALSE;
    return TRUE;
}
bool dun_allow_los_at(dun_ptr dun, point_t pos)
{
    dun_feat_ptr feat = dun_feat_at(dun, pos);
    return have_flag(feat->flags, FF_LOS);
}
bool dun_allow_project_at(dun_ptr dun, point_t pos)
{
    dun_feat_ptr feat = dun_feat_at(dun, pos);
    return have_flag(feat->flags, FF_PROJECT);
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
    if (dun->dun_id == p_ptr->dun_id && (dun->flags & DF_GENERATED))
    {
        assert(cave == dun);
        note_pos(pos);
    }
}
void dun_lite_pos(dun_ptr dun, point_t pos)
{
    if (dun->dun_id == p_ptr->dun_id && (dun->flags & DF_GENERATED))
    {
        assert(cave == dun);
        lite_pos(pos);
    }
}

static point_vec_ptr _copy_path(point_ptr path, int ct)
{
    point_vec_ptr result = point_vec_alloc();
    int i;
    assert(ct >= 0);
    for (i = 0; i < ct; i++)
        point_vec_add(result, path[i]);
    return result;
}
static point_vec_ptr _reverse_path(point_ptr path, int ct, point_t p2)
{
    point_vec_ptr result = point_vec_alloc();
    int i;
    assert(ct > 0);
    for (i = ct - 2; i >= 0; i--) /* skip true p1 */
        point_vec_add(result, path[i]);
    point_vec_add(result, p2); /* add true p2 */
    return result;
}
point_vec_ptr dun_project_path(dun_ptr dun, int range, point_t p1, point_t p2, int flags)
{
    point_t path1[32], path2[32];
    int     ct1 = project_path_aux(dun, path1, range, p1, p2, flags), ct2;

    if (ct1 >= 0) return _copy_path(path1, ct1);

    /* Try reverse path from target to source. This ensures reflexivity, but messes
     * up beam projections. Note the reverse path will omit the true target and include
     * the true source, which we fix up in _reverse_path(). XXX */
    flags &= ~PROJECT_THRU;
    ct2 = project_path_aux(dun, path2, range, p2, p1, flags);
    if (ct2 > 0) return _reverse_path(path2, ct2, p2);

    /*if both paths fail, take the foward path */
    return _copy_path(path1, -ct1);
}

/* player management */
void dun_move_plr(dun_ptr dun, point_t pos)
{
    point_t old_pos = p_ptr->pos;
    assert(p_ptr->dun_id == dun->dun_id);
    assert(dun_pos_interior(dun, pos));
    p_ptr->pos = pos;
    dun->flow_pos = pos;
    dun_update_flow(dun);
    dun_lite_pos(dun, old_pos);
    dun_lite_pos(dun, pos);

    if (dun->dun_type_id == D_SURFACE)
        dun_world_move_plr(dun, pos);
}
bool dun_plr_at(dun_ptr dun, point_t pos)
{
    if (dun->dun_id != p_ptr->dun_id) return FALSE;
    return point_equals(p_ptr->pos, pos);
}
/* stairs */
static bool _valid_stair_pos(dun_ptr dun, point_t pos)
{
    dun_grid_ptr grid;
    dun_feat_ptr feat;
    if (!dun_pos_interior(dun, pos)) return FALSE;
    grid = dun_grid_at(dun, pos);
    feat = dun_grid_feat(grid);
    if (have_flag(feat->flags, FF_PERMANENT)) return FALSE;
    if (dun_obj_at(dun, pos)) return FALSE;
    if (grid->info & CAVE_OBJECT) return FALSE;
    return TRUE;
}
void dun_quest_stairs(dun_ptr dun, point_t pos, int lvl)
{
    dun_stairs_ptr stairs;

    while (!_valid_stair_pos(dun, pos))
        pos = scatter(pos, 1);

    cmsg_print(TERM_L_BLUE, "A magical staircase appears...");
    assert(dun == cave); /* XXX */
    cave_set_feat(pos.y, pos.x, feat_down_stair); /* XXX */

    stairs = malloc(sizeof(dun_stairs_t));
    memset(stairs, 0, sizeof(dun_stairs_t));
    stairs->pos_here = pos;
    stairs->dun_type_id = dun->dun_type_id;
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
    cave_set_feat(pos.y, pos.x, feat_travel); /* XXX */
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
    if (!player_can_enter(grid->feat, 0)) return FALSE;
    if (have_flag(dun_grid_feat(grid)->flags, FF_HIT_TRAP)) return FALSE;
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

    assert(p_ptr->dun_id == dun->dun_id);
    assert(dun_pos_interior(dun, p_ptr->pos));

    stairs = _find_stairs(dun, p_ptr->pos);
    assert(stairs);

    if (stairs->dun_type_id == D_QUEST)
    {
        assert(!stairs->dun_id);
        dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        assert(new_dun->quest_id == stairs->dun_lvl);
        new_pos = p_ptr->new_pos; /* quest maps *should* locate the player */
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
    if (dun->dun_type_id == D_QUEST) /* XXX */
        dun_regen_town(new_dun);
    return TRUE;
}
static bool _check_pos_mon(dun_ptr dun, mon_ptr mon, point_t pos)
{
    dun_grid_ptr grid = dun_grid_at(dun, pos);
    if (dun_mon_at(dun, pos)) return FALSE;
    if (dun_plr_at(dun, pos)) return FALSE;
    if (!monster_can_cross_terrain(grid->feat, mon_race(mon), 0)) return FALSE;
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


    stairs = _find_stairs(dun, mon->pos);
    if (!stairs) return FALSE; /* paranoia */
    if (!stairs->dun_id) return FALSE; /* not generated (should not happen) */
    if (stairs->dun_type_id != dun->dun_type_id)
    {
        /* XXX Questors can now chase the player off level, but not on to the surface. */
        if (mon->mflag2 & MFLAG2_QUESTOR) return FALSE;
    }

    new_dun = dun_mgr_dun(stairs->dun_id);
    if (!new_dun) return FALSE; /* garbage collected (should not happen) */

    new_pos = _stairs_loc_mon(new_dun, mon, stairs->pos_there);
    if (!dun_pos_interior(new_dun, new_pos)) return FALSE;

    dun_detach_mon(dun, mon->id);
    cave = new_dun; /* for lite_pos ... we need to see the monster chasing us! */
    mon->target = point_create(0, 0);
    dun_place_mon(new_dun, mon, new_pos);
    cave = old_cave;

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
void dun_teleport_level_plr(dun_ptr dun)
{
    dun_stairs_ptr stairs = _random_stairs(dun, _UP | _DOWN);
    assert(p_ptr->dun_id == dun->dun_id);
    assert(cave == dun);
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
    dun_stairs_ptr stairs = _random_stairs(dun, _UP | _DOWN);
    assert(mon->dun_id == dun->dun_id);
    if (stairs)
    {
        dun_ptr new_dun, old_cave = cave;
        point_t pos;

        if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
            dun_gen_connected(dun, stairs);
        new_dun = dun_mgr_dun(stairs->dun_id);
        pos = dun_random_mon_pos(new_dun, mon_race(mon));
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
        cave = new_dun; /* for lite_pos in case the monster lands on plr_dun near plr */
        dun_place_mon(new_dun, mon, pos);
        cave = old_cave;
    }
    else if (mon_show_msg(mon)) msg_print("Failed!");
}
bool dun_create_stairs(dun_ptr dun, bool down_only)
{
    int            options = down_only ? _DOWN : _UP | _DOWN;
    dun_type_ptr   type = dun_types_lookup(dun->dun_type_id);
    dun_stairs_ptr stairs, other_stairs;
    dun_grid_ptr   grid;
    dun_ptr        other_dun;

    assert(p_ptr->dun_id == dun->dun_id);
    assert(cave == dun);

    if (quests_get_current() || dun->dun_lvl == type->max_dun_lvl)
        options &= ~_DOWN;

    if (!options || dun->dun_type_id == D_SURFACE || dun->dun_type_id == D_QUEST)
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

    if (!cave_valid_bold(p_ptr->pos.y, p_ptr->pos.x))
    {
        msg_print("The object resists the spell.");
        return FALSE;
    }

    if (!stairs->dun_id || !dun_mgr_dun(stairs->dun_id))
        dun_gen_connected(dun, stairs);
    other_dun = dun_mgr_dun(stairs->dun_id);
    other_stairs = _find_stairs(other_dun, stairs->pos_there);

    dun_destroy_obj_at(dun, p_ptr->pos);
    grid = dun_grid_at(dun, stairs->pos_here);
    cave_set_feat(p_ptr->pos.y, p_ptr->pos.x, grid->feat);
    cave_set_feat(stairs->pos_here.y, stairs->pos_here.x, type->floor_type[randint0(100)]);
    stairs->pos_here = p_ptr->pos;
    other_stairs->pos_there = p_ptr->pos;

    return TRUE;
}

/************************************************************************
 * Processing
 ************************************************************************/
static void _process_mon(dun_ptr dun, mon_ptr mon)
{
    assert(cave == dun);
    assert(dun->dun_id == mon->dun_id);
    if (mon_tim_find(mon, T_PARALYZED)) return;
    if (!fear_process_m(mon->id)) return;
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
    hack_m_idx = mon->id;
    process_monster(mon);
    hack_m_idx = 0;
}

static vec_ptr _monsters = NULL; /* monsters getting a move this turn */
static void _preprocess_mon(dun_ptr dun, mon_ptr mon)
{
    mon_race_ptr race;
    int          radius = 0, speed;
    bool         test = FALSE;

    assert(cave == dun);
    if (mon_is_dead(mon)) return; /* XXX bug */

    if (dun->turn % TURNS_PER_TICK == 0)
    {
        mon_tim_tick(mon);
        if (mon_is_dead(mon)) return; /* XXX this can happen */
    }

    /* Require proximity */
    if (mon->cdis >= (p_ptr->action == ACTION_GLITTER ? AAF_LIMIT_RING : AAF_LIMIT))
        return;

    /* Hack -- Monsters are automatically aware of the player (except for mimics) */
    if (!is_aware(mon))
    {
        if (p_ptr->prace != RACE_MON_RING)
            mon->mflag2 |= MFLAG2_AWARE;
        else if (p_ptr->riding && plr_los(mon->pos) && !mon_tim_find(mon, MT_SLEEP))
        {
            /* Player has a ring bearer, which this monster can see */
            mon->mflag2 |= MFLAG2_AWARE;
        }
    }

    /* Flow by smell is allowed */
    if (!p_ptr->no_flowed)
        mon->mflag2 &= ~MFLAG2_NOFLOW;

    /* Assume no move */
    test = FALSE;

    /* Handle "sensing radius" */
    race = mon_race(mon);
    radius = race->aaf;
    if (mon_is_pet(mon) && radius > MAX_SIGHT)
        radius = MAX_SIGHT;
    else if ( p_ptr->prace == RACE_MON_RING
           && !p_ptr->riding
           && !is_aware(mon)
           && mon_is_type(mon->r_idx, SUMMON_RING_BEARER) )
    {
        radius = AAF_LIMIT_RING;
    }
    else if (plr_on_surface())
        radius *= 3;

    if (mon->cdis + dun->plr_dis <= radius)
        test = TRUE;
    else if (mon->cdis <= MAX_SIGHT && (point_los(mon->pos, dun->flow_pos) || (p_ptr->cursed & OFC_AGGRAVATE)))
        test = TRUE;
    else if (dun_pos_interior(dun, mon->target)) test = TRUE;

    /* Do nothing */
    if (!test) return;

    if (p_ptr->riding == mon->id)
        speed = p_ptr->pspeed;
    else
        speed = mon->mspeed;

    /* Give this monster some energy */
    mon->energy_need -= SPEED_TO_ENERGY(speed);

    /* Check for SI now in case the monster is sleeping or lacks enough
     * energy to move. Should the monster move or attack, we'll make another
     * (overriding) check again later. */
    if ((race->flags2 & RF2_INVISIBLE) && p_ptr->see_inv)
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

        if (mon_is_dead(mon)) continue;

        /* _monsters = {Banor, Rupart, ...}; Banor combines, deleting Rupart, but
         * the deleted mon is still in the monsters stack to be processed this round. */
        if (mon_is_deleted(mon)) continue;

        mon->energy_need += ENERGY_NEED();
        _process_mon(dun, mon);
        mon->turns++; /* XXX restore old MFLAG_NICE behaviour ... cf _can_cast */
        if (mon->dun_id == dun->dun_id) /* XXX check if monster took stairs */
            mon_tim_fast_tick(mon); /* timers go after monster moves (e.g. T_PARALYZED) */

        mon->pain = 0; /* XXX pain cancels fear hack; avoid msg spam and bool *fear parameters ... */
        reset_target(mon);
        if (p_ptr->no_flowed && one_in_(3)) /* Give up flow_by_smell when it might useless */
            mon->mflag2 |= MFLAG2_NOFLOW;

        if (!p_ptr->playing || p_ptr->is_dead) break;
        if (cave != dun) break; /* XXX nexus travel or some such ... need to remove "cave" */
    }
    vec_clear(_monsters);
}
static void dun_process_plr(dun_ptr dun)
{
    assert(cave->dun_id == dun->dun_id);
    process_player();
}
static void _regen_mon(int id, mon_ptr mon)
{
    if (mon->hp < mon->maxhp)
    {
        mon_race_ptr race = mon_race(mon);
        int          amt = mon->maxhp / 100;

        if (!amt) if (one_in_(2)) amt = 1;
        if (race->flags2 & RF2_REGENERATE) amt *= 2;
        if (amt >= 400) amt = 400;

        mon->hp += amt;
        if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;
        check_mon_health_redraw(mon->id);
    }
}
static void dun_regen_monsters(dun_ptr dun) { dun_iter_mon(dun, _regen_mon); }
static void dun_process_world(dun_ptr dun)
{
    if (dun->dun_id == p_ptr->dun_id)
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
    if (p_ptr->dun_id == dun->dun_id)
    {
        notice_stuff();
        handle_stuff();
        move_cursor_relative(p_ptr->pos);
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
    else dun_flow_recalc(dun->flow, dun->flow_pos);
    dun->flags &= ~DF_UPDATE_FLOW;
}
static void dun_process_aux(dun_ptr dun)
{
    vec_clear(dun->graveyard);
    vec_clear(dun->junkpile);

    assert(cave == dun);

    if (dun->flags & DF_AUTOSAVE)
    {
        do_cmd_save_game(TRUE);
        dun->flags &= ~DF_AUTOSAVE;
    }

    dun->turn++;
    if (dun->flags & DF_UPDATE_FLOW)
        dun_update_flow(dun);

    if (p_ptr->dun_id == dun->dun_id)
    {
        dun_process_plr(dun);
        if (cave != dun) /* detect plr level change (stairs, trapdoor, etc.) */
        {
            _hack_stuff(cave);
            return; /* we abort because old code needs cave == dun to work */
        }
        _hack_stuff(dun);
    }
    if (!p_ptr->playing || p_ptr->is_dead) return;
    dun_process_monsters(dun);
    if (cave != dun) /* detect plr level change (nexus travel or teleport level) */
    {
        _hack_stuff(cave);
        return; /* we abort because old code needs cave == dun to work */
    }
    _hack_stuff(dun);
    if (!p_ptr->playing || p_ptr->is_dead) return;
    dun_process_world(dun);
    _hack_stuff(dun);
}
void dun_process(dun_ptr dun)
{
    cave = dun;
    dun_process_aux(dun);
}
void dun_mgr_process(void)
{
    dun_mgr_ptr dm = dun_mgr();
    hack_mind = TRUE; /* we are playing now ... cf do_cmd_save_game */
    for (;;)
    {
        dun_ptr plr_dun = dun_mgr_dun(p_ptr->dun_id);
        dun_stairs_ptr stairs;

        cave = plr_dun;
        dm->turn++;
        dun_mgr_gc(FALSE);

        plr_dun->plr_dis = 0;
        dun_process(plr_dun);
        if (p_ptr->is_dead || !p_ptr->playing) break;
        if (dun_mgr_dun(p_ptr->dun_id) != plr_dun) continue; /* detect level change (safely) (e.g. plr_dun may have been gc'd) */

        for (stairs = plr_dun->stairs; stairs; stairs = stairs->next)
        {
            int dis = point_fast_distance(stairs->pos_here, p_ptr->pos);
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
    cave = dun_mgr_dun(p_ptr->dun_id); /* XXX */
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
            int_map_delete(dm->dungeons, dun->dun_id);
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
    _mark(dun_mgr_dun(p_ptr->dun_id), 0); /* mark accessible dungeons to a fixed depth */
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
static void _mru_clear(dun_mgr_ptr dm)
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
        _dm->packs = int_map_alloc(free);
    }
    return _dm;
}
void dun_mgr_doc(doc_ptr doc)
{
    dun_mgr_ptr dm = dun_mgr();
    doc_insert(doc, " <color:U>Dungeon Manager Statistics</color>\n");
    doc_printf(doc, "   next_dun_id = %d\n", dm->next_dun_id);
    doc_printf(doc, "   next_mon_id = %d\n", dm->next_mon_id);
    doc_printf(doc, "   next_obj_id = %d\n", dm->next_obj_id);
    doc_printf(doc, "   turn        = %d\n", dm->turn);
    doc_printf(doc, "   active duns = %d\n", int_map_count(dm->dungeons));
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
dun_ptr dun_mgr_alloc_dun(rect_t rect)
{
    dun_mgr_ptr dm = dun_mgr();
    dun_ptr     dun;

    if (rect_is_valid(rect))
        dun = dun_alloc(dun_mgr_next_dun_id(), rect);
    else
        dun = dun_alloc_aux(dun_mgr_next_dun_id());

    int_map_add(dm->dungeons, dun->dun_id, dun);
    return dun;
}
void dun_mgr_delete_surface(void)
{
    dun_mgr_ptr dm = dun_mgr();
    int i;
    if (!dm->surface) return;
    for (i = 0; i < _MRU_LEN; i++)
    {
        if (dm->mru[i] == dm->surface->dun_id)
            dm->mru[i] = 0; /* lazy */
    }
    int_map_delete(dm->dungeons, dm->surface->dun_id);
    dm->surface = NULL;
}
mon_pack_ptr dun_mgr_alloc_pack(void)
{
    dun_mgr_ptr dm = dun_mgr();
    mon_pack_ptr pack = malloc(sizeof(mon_pack_t));
    memset(pack, 0, sizeof(mon_pack_t));
    pack->pack_idx = dun_mgr_next_pack_id();
    int_map_add(dm->packs, pack->pack_idx, pack);
    return pack;
}
void dun_mgr_free_pack(mon_pack_ptr pack)
{
    int_map_delete(dun_mgr()->packs, pack->pack_idx);
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
            if (m->r_idx == race_id) mon = m;
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
    dun_detach_mon(dun_mgr_dun(mon->dun_id), mon->id);
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
    dun_ptr     current = int_map_find(dm->dungeons, p_ptr->dun_id);
    dun_ptr     recall = NULL;

    if (!plr_can_recall())
    {
        msg_print("Nothing happens!");
        return FALSE;
    }
    if (!quests_check_leave()) return FALSE;
    if (current->dun_type_id == D_SURFACE)
    {
        dun_type_ptr type = dun_types_choose("Recall to Which Dungeon?", FALSE);
        if (!type) return FALSE;
        if (!dun_pos_interior(dm->world, type->world_pos))
        {
            msg_format("%s does not exist in this world.", type->name);
            return FALSE;
        }

        p_ptr->old_pos = p_ptr->pos; /* remember for return recall */
        _mru_clear(dm);
        recall = dun_gen_wizard(type->id, type->plr_max_lvl);
        energy_use = 0; /* replaces MFLAG_NICE */
    }
    else
    {
        _mru_clear(dm);
        recall = dun_gen_surface(dun_world_pos(p_ptr->old_pos));
        assert(dm->surface == recall);
        dun_mgr_plr_change_dun(recall, p_ptr->old_pos);
    }
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

    if (p_ptr->dun_id && dun_mgr_dun(p_ptr->dun_id)->dun_type_id == D_SURFACE)
        p_ptr->old_pos = p_ptr->pos;

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
        target_who = 0; /* XXX Code is assuming dun_mon(cave, target_who) will work. */
        target_row = 0;
        target_col = 0;
        travel.pos = point_create(0, 0);
        p_ptr->health_who = 0;
        shimmer_monsters = TRUE;
        shimmer_objects = TRUE;
        repair_monsters = TRUE;
        repair_objects = TRUE;
        disturb(1, 0);
        if (p_ptr->max_plv < p_ptr->lev) p_ptr->max_plv = p_ptr->lev; /* XXX Really? */

        forget_lite();
        forget_view();
        clear_mon_lite();
    }
    /* Track Max Depth */
    switch (new_dun->dun_type_id)
    {
    case D_SURFACE:
    case D_QUEST:
    case D_WORLD: break;
    default:
        type = dun_types_lookup(new_dun->dun_type_id);
        if (new_dun->dun_lvl > type->plr_max_lvl)
        {
            type->plr_max_lvl = new_dun->dun_lvl;
            type->plr_flags |= DFP_ENTERED;
        }
    }

    /* keep mru up to date */
    _mru_nq(dm, new_dun->dun_id);

    /* Re-locate the player. This should be the only place in the system
     * that actually places the plr in a dungeon (setting p_ptr->dun_id),
     * except for dun_mgr_load, of course. */
    cave = new_dun;
    p_ptr->dun_id = new_dun->dun_id;
    p_ptr->pos = new_pos;
    if (mount)
    {
        dun_detach_mon(mon_dun(mount), mount->id);
        if (mount->id > dm->next_mon_id) /* paranoia: the mon_id sequence usually wraps once per game */
        {
            mount->id = dun_mgr_next_mon_id();
            p_ptr->riding = mount->id;
        }
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
    if (new_dun->dun_type_id == D_QUEST)
        quests_on_enter_fixed(new_dun->quest_id);
    else
        quests_on_enter(new_dun->dun_type_id, new_dun->dun_lvl);

    if (autosave_l && !statistics_hack)
        new_dun->flags |= DF_AUTOSAVE; /* later ... once the stack unwinds to the next dun_mgr_process() */

    /* XXX do_cmd_save_game does a handle_stuff ... Saving currently *requires* forget_view et. al. */
    p_ptr->window |= PW_MONSTER_LIST | PW_OBJECT_LIST | PW_MONSTER | PW_OVERHEAD | PW_DUNGEON | PW_WORLD_MAP;
    p_ptr->redraw |= PR_BASIC | PR_EXTRA | PR_EQUIPPY | PR_MSG_LINE | PR_MAP;
    p_ptr->update |= PU_VIEW | PU_LITE | PU_MON_LITE | PU_TORCH;
    p_ptr->update |= PU_MONSTERS;
}
/************************************************************************
 * Savefiles
 ************************************************************************/
/* 1. Save dun_page_t using RLE encoding for terrain. Rely on page->grids allocation
 *    as a single contiguous block. RLE compression is very good for typical dungeons,
 *    around 20x in the few cases I looked at. */
typedef struct {
    int  index;
    u16b info;
    s16b feat, mimic, special, count;
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
        if (t->info == grid->info && t->feat == grid->feat && t->mimic == grid->mimic && t->special == grid->special)
            return t;
    }
    t = malloc(sizeof(_template_t));
    /* XXX index is set after sorting */
    t->info = grid->info;
    t->feat = grid->feat;
    t->mimic = grid->mimic;
    t->special = grid->special;
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
        t->info = savefile_read_u16b(file);
        t->feat = savefile_read_s16b(file);
        t->mimic = savefile_read_s16b(file);
        t->special = savefile_read_s16b(file);
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
            grid->info = t->info;
            grid->feat = t->feat;
            grid->mimic = t->mimic;
            grid->special = t->special;
            grid++;
        }
    }
    check = savefile_read_u32b(file);
    assert(check == 0xdeadbeef); /* paranoia */
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
            assert(grid->feat);
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
        assert(t->feat);
        savefile_write_u16b(file, t->info);
        savefile_write_s16b(file, t->feat);
        savefile_write_s16b(file, t->mimic);
        savefile_write_s16b(file, t->special);
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

    dun->dun_type_id = savefile_read_s16b(file);
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
        mon_true_race(mon)->cur_num++; /* XXX cur_num is not stored in the savefile */
        int_map_add(dun->mon, mon->id, mon);
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

    if (dun_pos_interior(dun, dun->flow_pos)) /* XXX *after* reading savefile flags! */
        dun->flags |= DF_UPDATE_FLOW;

    if (dun->flags & DF_SHOP)
    {
        dun->town = town_alloc(TOWN_RANDOM, "Random");
        town_load(dun->town, file);
    }
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

    savefile_write_s16b(file, dun->dun_type_id);
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

    if (dun->flags & DF_SHOP)
    {
        assert(dun->town);
        town_save(dun->town, file);
    }
}
/* 2. Save dun_mgr_t */
static void mon_pack_save(mon_pack_ptr pack, savefile_ptr file)
{
    savefile_write_u16b(file, pack->pack_idx);
    savefile_write_u16b(file, pack->leader_idx);
    savefile_write_s16b(file, pack->count);
    savefile_write_s16b(file, pack->ai);
    savefile_write_u16b(file, pack->guard_idx);
    savefile_write_s16b(file, pack->guard_x);
    savefile_write_s16b(file, pack->guard_y);
    savefile_write_s16b(file, pack->distance);
}
static void mon_pack_load(mon_pack_ptr pack, savefile_ptr file)
{
    pack->pack_idx = savefile_read_u16b(file);
    pack->leader_idx = savefile_read_u16b(file);
    pack->count = savefile_read_s16b(file);
    pack->ai = savefile_read_s16b(file);
    pack->guard_idx = savefile_read_u16b(file);
    pack->guard_x = savefile_read_s16b(file);
    pack->guard_y = savefile_read_s16b(file);
    pack->distance = savefile_read_s16b(file);
}
void dun_mgr_load(savefile_ptr file)
{
    dun_mgr_ptr dm = dun_mgr();
    int i, ct;

    if (savefile_is_older_than(file, 7, 1, 2, 1))
        p_ptr->initial_world_id = W_SMAUG;
    else
        p_ptr->initial_world_id = savefile_read_u16b(file);
    p_ptr->world_id = savefile_read_u16b(file);
    p_ptr->dun_id = savefile_read_u16b(file);
    p_ptr->pos = _point_load(file);
    p_ptr->old_pos = _point_load(file);
    p_ptr->turn = savefile_read_u32b(file);

    dm->next_dun_id = savefile_read_u16b(file);
    dm->next_mon_id = savefile_read_u16b(file);
    dm->next_obj_id = savefile_read_u16b(file);
    dm->next_pack_id = savefile_read_u16b(file);
    dm->turn = savefile_read_u32b(file);
    dm->world_seed = savefile_read_u32b(file);
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        dun_ptr dun;
        int id = savefile_read_u16b(file);
        dun = dun_alloc_aux(id);
        dun_load(dun, file);
        int_map_add(dm->dungeons, dun->dun_id, dun);
        if (dun->dun_type_id == D_WORLD) dm->world = dun;
        if (dun->dun_type_id == D_SURFACE) dm->surface = dun;
    }
    for (;;)
    {
        int id = savefile_read_u16b(file);
        if (!id) break;
        if (!int_map_find(dm->dungeons, id)) continue;
        _mru_nq(dm, id);
    }
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        mon_pack_ptr pack = malloc(sizeof(mon_pack_t));
        memset(pack, 0, sizeof(mon_pack_t));
        mon_pack_load(pack, file);
        int_map_add(dm->packs, pack->pack_idx, pack);
    }
    dun_types_load(file);
    dun_worlds_load(file);

    dun_world_reseed(dm->world_seed);
    cave = int_map_find(dm->dungeons, p_ptr->dun_id);

    #if 0
    if (p_ptr->initial_world_id == W_AMBER) /* XXX fixup savefile for my playtesting char */
    {
        vec_ptr v = world_dun_types();
        for (i = 0; i < vec_length(v); i++)
        {
            dun_type_ptr t = vec_get(v, i);
            if (t->init_f)
                t->init_f(t);
        }
        vec_free(v);
    }
    #endif
}
void dun_mgr_save(savefile_ptr file)
{
    dun_mgr_ptr dm = dun_mgr();
    int_map_iter_ptr iter;
    int i;

    /* XXX XXX XXX */
    /* Forget old "cave" info before saving current dun;
     * these guys all use temp arrays to forget/track current
     * values, and these temp arrays are *not* saved! So, if
     * we saved CAVE_VIEW flags, for example, they would never
     * ever be cleared after a reload (unless you got lucky).
     * BTW, I'd like to replace this sort of stuff. Move flags
     * out of dun_grid_t.info into "dun_flags" and then you could
     * have p_ptr->los and dun_t.lite. Save 'em if you want, or
     * let a null pointer signal the need to re-calc.
     * XXX p_ptr->los added. Still thinking on lite ... */
    forget_lite();
    clear_mon_lite();
    p_ptr->update |= PU_LITE | PU_MON_LITE;
    p_ptr->update |= PU_MONSTERS;
    /* XXX XXX XXX */

    dun_mgr_gc(TRUE);

    savefile_write_u16b(file, p_ptr->initial_world_id);
    savefile_write_u16b(file, p_ptr->world_id);
    savefile_write_u16b(file, p_ptr->dun_id);
    _point_save(p_ptr->pos, file);
    _point_save(p_ptr->old_pos, file);
    savefile_write_u32b(file, p_ptr->turn);

    savefile_write_u16b(file, dm->next_dun_id);
    savefile_write_u16b(file, dm->next_mon_id);
    savefile_write_u16b(file, dm->next_obj_id);
    savefile_write_u16b(file, dm->next_pack_id);
    savefile_write_u32b(file, dm->turn);
    savefile_write_u32b(file, dm->world_seed);

    savefile_write_s16b(file, int_map_count(dm->dungeons));
    for (iter = int_map_iter_alloc(dm->dungeons);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_ptr dun = int_map_iter_current(iter);
        savefile_write_u16b(file, dun->dun_id);
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

    savefile_write_s16b(file, int_map_count(dm->packs));
    for (iter = int_map_iter_alloc(dm->packs);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_pack_ptr pack = int_map_iter_current(iter);
        mon_pack_save(pack, file);
    }
    int_map_iter_free(iter);
    dun_types_save(file);
    dun_worlds_save(file);
}

/************************************************************************
 * Temporary Hacks so old code can migrate to new system
 ************************************************************************/
dun_ptr cave = NULL;
