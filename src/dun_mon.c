#include "angband.h"
#include "dun.h"
#include <assert.h>

mon_ptr dun_alloc_mon(dun_ptr dun)
{
    mon_ptr mon = mon_alloc();
    mon->id = dun_mgr_next_mon_id();
    mon->dun = dun;
    return mon;
}
mon_ptr dun_mon(dun_ptr dun, u32b id)
{
    return int_map_find(dun->mon, id);
}
static void _unmark(int id, dun_ptr dun) { dun->flags &= ~DF_MARK; }
static mon_ptr _find_mon(dun_ptr dun, u32b id)
{
    mon_ptr mon = int_map_find(dun->mon, id);
    if (!mon)
    {
        dun_stairs_ptr stairs;
        dun->flags |= DF_MARK;
        for (stairs = dun->stairs; stairs; stairs = stairs->next)
        {
            dun_ptr adj_dun;
            if (!stairs->dun_id) continue;
            adj_dun = dun_mgr_dun(stairs->dun_id);
            if (!adj_dun) continue;
            if (adj_dun->flags & DF_MARK) continue; /* dungeons form a tree ... but let's not rely on this */
            mon = _find_mon(adj_dun, id);
            if (mon) break;
        }
    }
    return mon;
}
mon_ptr dun_mon_ex(dun_ptr dun, u32b id)
{
    dun_mgr_ptr dm = dun_mgr();
    int_map_iter(dm->dungeons, (int_map_iter_f)_unmark);
    return _find_mon(dun, id);
}
vec_ptr dun_mon_filter(dun_ptr dun, mon_p p)
{
    vec_ptr v = vec_alloc(NULL);
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (p && !p(mon)) continue;
        vec_add(v, mon);
    }
    int_map_iter_free(iter);
    return v;
}
vec_ptr dun_adjacent_mon(dun_ptr dun, point_t pos)
{
    vec_ptr v = vec_alloc(NULL);
    int i;
    assert(dun_pos_interior(dun, pos));
    for (i = 0; i < 8; i++)
    {
        point_t p = point_create(pos.x + ddx_cdd[i], pos.y + ddy_cdd[i]);
        mon_ptr m = dun_mon_at(dun, p);
        if (m) vec_add(v, m);
    }
    return v;
}
mon_ptr dun_mon_at(dun_ptr dun, point_t pos)
{
    return point_map_find(dun->mon_pos, pos);
}
void dun_attach_mon(dun_ptr dun, mon_ptr mon)
{
    obj_ptr obj;

    assert(dun_pos_interior(dun, mon->pos));
    assert(mon->id);
    assert(!int_map_find(dun->mon, mon->id));

    mon->dun = dun;
    int_map_add(dun->mon, mon->id, mon);
    point_map_add(dun->mon_pos, mon->pos, mon);

    for (obj = mon->obj; obj; obj = obj->next)
    {
        assert(obj->loc.where == INV_MON_PACK);
        assert(obj->loc.v.mon_pack.obj_id);
        assert(obj->loc.v.mon_pack.mon_id == mon->id);
        assert(!int_map_find(dun->obj, obj->loc.v.mon_pack.obj_id));

        obj->loc.v.mon_pack.dun_id = dun->id;
        int_map_add(dun->obj, obj->loc.v.mon_pack.obj_id, obj);
    }
}
void dun_place_mon(dun_ptr dun, mon_ptr mon, point_t pos)
{
    assert(dun_pos_interior(dun, pos));
    mon->pos = pos;
    dun_attach_mon(dun, mon);
    if (plr->dun_id == dun->id && (dun->flags & DF_GENERATED))
    {
        update_mon(mon, TRUE);
        dun_draw_pos(dun, pos);
    }
}
void dun_move_mon(dun_ptr dun, mon_ptr mon, point_t pos)
{
    point_t old_pos = mon->pos;
    mon_ptr swap_mon;

    assert(dun_pos_valid(dun, mon->pos));
    assert(dun_pos_valid(dun, pos));
    assert(mon->id);
    assert(dun == mon->dun);
    assert(int_map_find(dun->mon, mon->id));

    swap_mon = point_map_find(dun->mon_pos, pos);
    if (swap_mon) /* change places */
    {
        swap_mon->pos = old_pos;
        point_map_add(dun->mon_pos, old_pos, swap_mon);
        if (plr->dun_id == dun->id && (dun->flags & DF_GENERATED))
            update_mon(swap_mon, TRUE);
    }
    else
        point_map_delete(dun->mon_pos, old_pos);

    mon->pos = pos;
    point_map_add(dun->mon_pos, pos, mon);
    if (mon->mflag2 & MFLAG2_HUNTED)
    {
        if (!mon->flow) mon->flow = dun_flow_calc(dun, pos, MON_HUNT_RAD, NULL);
        else dun_flow_recalc(mon->flow, pos);
    }

    if (plr->dun_id == dun->id && (dun->flags & DF_GENERATED))
    {
        dun_draw_pos(dun, old_pos);
        dun_draw_pos(dun, pos);
        update_mon(mon, TRUE);
        plr->window |= PW_MONSTER_LIST;
        if (mon->race->light || mon->race->lantern)
            plr->update |= PU_MON_LIGHT;
    }
}
/* move a monster (and all of its carried objects) between levels:
 * [1] dun_detach_mon(old_dun, mon->id)
 * [2] dun_place_mon(new_dun, mon, new_pos); */
mon_ptr dun_detach_mon(dun_ptr dun, u32b id)
{
    mon_ptr mon = int_map_find(dun->mon, id);
    obj_ptr obj;
    point_t pos = mon->pos;

    assert(mon);
    assert(mon->id == id);
    assert(mon->dun == dun);
    assert(point_map_find(dun->mon_pos, mon->pos) == mon);

    point_map_delete(dun->mon_pos, mon->pos);
    mon->dun = NULL;
    mon->pos.x = 0;
    mon->pos.y = 0;
    mon_clear_target(mon);
    mon->mflag2 &= ~(MFLAG2_MARK | MFLAG2_SHOW); /* XXX take stairs while "detected" (cf _repair_aux) */

    for (obj = mon->obj; obj; obj = obj->next)
    {
        assert(obj->loc.where == INV_MON_PACK);
        assert(obj->loc.v.mon_pack.dun_id == dun->id);
        assert(obj->loc.v.mon_pack.mon_id == mon->id);
        assert(int_map_find(dun->obj, obj->loc.v.mon_pack.obj_id));

        int_map_detach(dun->obj, obj->loc.v.mon_pack.obj_id);
        obj->loc.v.mon_pack.dun_id = 0;
    }
    int_map_detach(dun->mon, id);
    dun_draw_pos(dun, pos);
    return mon;
}
void dun_delete_mon(dun_ptr dun, u32b id)
{
    mon_ptr mon = dun_mon(dun, id);
    obj_ptr obj;

    assert(mon);
    assert(mon->id == id);
    assert(mon->dun == dun);
    assert(point_map_find(dun->mon_pos, mon->pos) == mon);

    point_map_delete(dun->mon_pos, mon->pos);
    for (obj = mon->obj; obj; obj = obj->next)
    {
        assert(obj->loc.where == INV_MON_PACK);
        assert(obj->loc.v.mon_pack.dun_id == dun->id);
        assert(obj->loc.v.mon_pack.mon_id == mon->id);
        assert(int_map_find(dun->obj, obj->loc.v.mon_pack.obj_id));

        int_map_detach(dun->obj, obj->loc.v.mon_pack.obj_id);
        obj->loc.where = INV_JUNK;
        vec_push(dun->junkpile, obj);
    }
    int_map_detach(dun->mon, id);
    mon->obj = NULL;
    mon->dun = NULL; /* mon_is_deleted() */
    if (mon_is_pet(mon)) /* now rather than in _mon_free since pet commands do not use energy */
        mon_pack_remove(plr_pack(), mon);
    vec_push(dun->graveyard, mon);
}
void dun_iter_mon(dun_ptr dun, void (*f)(int id, mon_ptr mon))
{
    int_map_iter(dun->mon, (int_map_iter_f)f);
}
vec_ptr dun_filter_mon(dun_ptr dun, mon_p p)
{
    vec_ptr v = vec_alloc(NULL);
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (p && !p(mon)) continue;
        vec_add(v, mon);
    }
    int_map_iter_free(iter);
    return v;
}

