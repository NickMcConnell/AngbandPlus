#include "angband.h"
#include "dun.h"
#include <assert.h>

mon_ptr dun_alloc_mon(dun_ptr dun)
{
    mon_ptr mon = mon_alloc();
    mon->id = dun_mgr_next_mon_id();
    mon->dun_id = dun->dun_id;
    return mon;
}
mon_ptr dun_mon(dun_ptr dun, int id)
{
    return int_map_find(dun->mon, id);
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

    mon->dun_id = dun->dun_id;
    int_map_add(dun->mon, mon->id, mon);
    point_map_add(dun->mon_pos, mon->pos, mon);

    for (obj = mon->obj; obj; obj = obj->next)
    {
        assert(obj->loc.where == INV_MON_PACK);
        assert(obj->loc.v.mon_pack.obj_id);
        assert(obj->loc.v.mon_pack.mon_id == mon->id);
        assert(!int_map_find(dun->obj, obj->loc.v.mon_pack.obj_id));

        obj->loc.v.mon_pack.dun_id = dun->dun_id;
        int_map_add(dun->obj, obj->loc.v.mon_pack.obj_id, obj);
    }
}
void dun_place_mon(dun_ptr dun, mon_ptr mon, point_t pos)
{
    assert(dun_pos_interior(dun, pos));
    mon->pos = pos;
    dun_attach_mon(dun, mon);
    if (p_ptr->dun_id == dun->dun_id && (dun->flags & DF_GENERATED))
    {
        update_mon(mon, TRUE);
        dun_lite_pos(dun, pos);
    }
}
void dun_move_mon(dun_ptr dun, mon_ptr mon, point_t pos)
{
    point_t old_pos = mon->pos;
    mon_ptr swap_mon;

    assert(dun_pos_valid(dun, mon->pos));
    assert(dun_pos_valid(dun, pos));
    assert(mon->id);
    assert(dun->dun_id == mon->dun_id);
    assert(int_map_find(dun->mon, mon->id));

    swap_mon = point_map_find(dun->mon_pos, pos);
    if (swap_mon) /* change places */
    {
        swap_mon->pos = old_pos;
        point_map_add(dun->mon_pos, old_pos, swap_mon);
        if (p_ptr->dun_id == dun->dun_id && (dun->flags & DF_GENERATED))
            update_mon(swap_mon, TRUE);
    }
    else
        point_map_delete(dun->mon_pos, old_pos);

    mon->pos = pos;
    point_map_add(dun->mon_pos, pos, mon);

    if (p_ptr->dun_id == dun->dun_id && (dun->flags & DF_GENERATED))
    {
        dun_lite_pos(dun, old_pos);
        dun_lite_pos(dun, pos);
        update_mon(mon, TRUE);
        p_ptr->window |= PW_MONSTER_LIST;
    }
}
/* move a monster (and all of its carried objects) between levels:
 * [1] dun_detach_mon(old_dun, mon->id)
 * [2] dun_place_mon(new_dun, mon, new_pos); */
mon_ptr dun_detach_mon(dun_ptr dun, int id)
{
    mon_ptr mon = int_map_find(dun->mon, id);
    obj_ptr obj;
    point_t pos = mon->pos;

    assert(mon);
    assert(mon->id == id);
    assert(mon->dun_id == dun->dun_id);
    assert(point_map_find(dun->mon_pos, mon->pos) == mon);

    point_map_delete(dun->mon_pos, mon->pos);
    mon->dun_id = 0;
    mon->pos.x = 0;
    mon->pos.y = 0;
    mon->target.x = 0;
    mon->target.y = 0;

    for (obj = mon->obj; obj; obj = obj->next)
    {
        assert(obj->loc.where == INV_MON_PACK);
        assert(obj->loc.v.mon_pack.dun_id == dun->dun_id);
        assert(obj->loc.v.mon_pack.mon_id == mon->id);
        assert(int_map_find(dun->obj, obj->loc.v.mon_pack.obj_id));

        int_map_detach(dun->obj, obj->loc.v.mon_pack.obj_id);
        obj->loc.v.mon_pack.dun_id = 0;
    }
    int_map_detach(dun->mon, id);
    dun_lite_pos(dun, pos);
    return mon;
}
void dun_delete_mon(dun_ptr dun, int id)
{
    mon_ptr mon = dun_mon(dun, id);
    obj_ptr obj;

    assert(mon);
    assert(mon->id == id);
    assert(mon->dun_id == dun->dun_id);
    assert(point_map_find(dun->mon_pos, mon->pos) == mon);

    point_map_delete(dun->mon_pos, mon->pos);
    for (obj = mon->obj; obj; obj = obj->next)
    {
        assert(obj->loc.where == INV_MON_PACK);
        assert(obj->loc.v.mon_pack.dun_id == dun->dun_id);
        assert(obj->loc.v.mon_pack.mon_id == mon->id);
        assert(int_map_find(dun->obj, obj->loc.v.mon_pack.obj_id));

        int_map_detach(dun->obj, obj->loc.v.mon_pack.obj_id);
        obj->loc.where = INV_JUNK;
        vec_push(dun->junkpile, obj);
    }
    int_map_detach(dun->mon, id);
    mon->obj = NULL;
    mon->dun_id = 0; /* mon_is_deleted() */
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

