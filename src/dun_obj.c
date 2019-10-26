#include "angband.h"

#include <assert.h>

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

#ifndef NDEBUG
static void _assert_obj(dun_ptr dun)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->obj);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        obj_ptr obj = int_map_iter_current(iter);
        assert(obj->loc.where == INV_FLOOR || obj->loc.where == INV_MON_PACK);
        if (obj->loc.where == INV_FLOOR)
        {
            point_t pos = point_create(obj->loc.v.floor.x, obj->loc.v.floor.y);
            obj_ptr pile = point_map_find(dun->obj_pos, pos);
            bool    found = FALSE;
            for (; pile; pile = pile->next)
            {
                if (pile == obj) found = TRUE;
                assert(pile->loc.where == INV_FLOOR);
                assert(pile->loc.v.floor.dun_id == dun->dun_id);
                assert(pile->loc.v.floor.x == pos.x);
                assert(pile->loc.v.floor.y == pos.y);
            }
            assert(found);
        }
        else
        {
            mon_ptr mon = int_map_find(dun->mon, obj->loc.v.mon_pack.mon_id);
            obj_ptr pile = mon->obj;
            bool    found = FALSE;
            for (; pile; pile = pile->next)
            {
                if (pile == obj) found = TRUE;
                assert(pile->loc.where == INV_MON_PACK);
                assert(pile->loc.v.mon_pack.dun_id == dun->dun_id);
                assert(pile->loc.v.mon_pack.mon_id == mon->id);
            }
            assert(found);
        }
    }
    int_map_iter_free(iter);
}
static void _assert_pile(dun_ptr dun, obj_ptr pile, point_t pos)
{
    for (; pile; pile = pile->next)
    {
        assert(pile->loc.where == INV_FLOOR);
        assert(pile->loc.v.floor.dun_id == dun->dun_id);
        assert(pile->loc.v.floor.x == pos.x);
        assert(pile->loc.v.floor.y == pos.y);
    }
}
static void _assert_obj_pos(dun_ptr dun)
{
    point_map_iter_ptr iter;
    for (iter = point_map_iter_alloc(dun->obj_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        obj_ptr pile = point_map_iter_current(iter);
        point_t pos = point_map_iter_current_key(iter);
        _assert_pile(dun, pile, pos);
    }
    point_map_iter_free(iter);
}
static void _assert_mon_pack(dun_ptr dun)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        obj_ptr pile = mon->obj;
        for (; pile; pile = pile->next)
        {
            assert(pile->loc.where == INV_MON_PACK);
            assert(pile->loc.v.mon_pack.dun_id == dun->dun_id);
            assert(pile->loc.v.mon_pack.mon_id == mon->id);
        }
    }
    int_map_iter_free(iter);
}
static void dun_assert_obj(dun_ptr dun)
{
    _assert_obj(dun);
    _assert_obj_pos(dun);
    _assert_mon_pack(dun);
}
#else
static void dun_assert_obj(dun_ptr dun)
{
}
#endif

/************************************************************************
 * Dungeon Object Management
 ************************************************************************/
obj_ptr dun_obj(dun_ptr dun, int id)
{
    return int_map_find(dun->obj, id);
}
obj_ptr dun_obj_at(dun_ptr dun, point_t pos)
{
    return point_map_find(dun->obj_pos, pos);
}
vec_ptr dun_pile_at(dun_ptr dun, point_t pos)
{
    vec_ptr v = vec_alloc(NULL);
    obj_ptr obj;
    for (obj = dun_obj_at(dun, pos); obj; obj = obj->next)
    {
        assert(obj->loc.where == INV_FLOOR);
        vec_add(v, obj);
    }
    return v;
}
void dun_place_obj(dun_ptr dun, obj_ptr obj, point_t pos)
{
    obj_ptr pile;
    
    assert(dun_pos_interior(dun, pos));
    assert(dun_allow_drop_at(dun, pos));
    assert(!obj->next);

    obj = obj_copy(obj);
    obj->loc.where = INV_FLOOR;
    obj->loc.v.floor.dun_id = dun->dun_id;
    obj->loc.v.floor.obj_id = dun_mgr_next_obj_id();
    obj->loc.v.floor.x = pos.x;
    obj->loc.v.floor.y = pos.y;

    int_map_add(dun->obj, _obj_id(obj), obj);
    pile = point_map_find(dun->obj_pos, pos);
    if (pile)
        obj->next = pile;
    point_map_add(dun->obj_pos, pos, obj);
}
bool dun_fetch_obj(dun_ptr dun, point_t from, point_t to)
{
    obj_ptr src = point_map_find(dun->obj_pos, from);
    obj_ptr dest = point_map_find(dun->obj_pos, to);

    if (!src) return FALSE;
    if (src->next) point_map_add(dun->obj_pos, from, src->next);
    else point_map_delete(dun->obj_pos, from);

    src->next = dest;
    src->loc.v.floor.x = to.x;
    src->loc.v.floor.y = to.y;
    point_map_add(dun->obj_pos, to, src);
    return TRUE;
}
static bool _mon_ignore_pickup(obj_ptr obj)
{
    if (obj->tval == TV_GOLD) return TRUE;
    if (obj->tval == TV_CORPSE) return TRUE;
    if (obj->tval == TV_STATUE) return TRUE;
    return FALSE;
}
static bool _mon_can_pickup(mon_ptr mon, obj_ptr obj)
{
    if (_mon_ignore_pickup(obj)) return FALSE;
    if (obj_is_art(obj)) return FALSE;
    if (obj_slays_mon(obj, mon)) return FALSE;
    return TRUE;
}
void dun_mon_pickup(dun_ptr dun, mon_ptr mon)
{
    obj_ptr pile, leave = NULL;
    bool    msg = FALSE;
    char    mon_name[MAX_NLEN_MON];
    char    obj_name[MAX_NLEN_OBJ];

    assert(dun_pos_interior(dun, mon->pos));
    assert(dun->dun_id == mon->dun_id);
    assert(int_map_find(dun->mon, mon->id));
    dun_assert_obj(dun);

    pile = point_map_find(dun->obj_pos, mon->pos);
    if (!pile) return;

    if (p_ptr->dun_id == dun->dun_id && plr_can_see(mon->pos))
    {
        msg = TRUE;
        monster_desc(mon_name, mon, MD_INDEF_HIDDEN);
    }

    while (pile)
    {
        obj_ptr next = pile->next;

        assert(pile->loc.where == INV_FLOOR);
        if (_mon_can_pickup(mon, pile))
        {
            u16b obj_id = _obj_id(pile);
            if (msg) /* invisible monsters still generate this msg */
            {
                object_desc(obj_name, pile, OD_COLOR_CODED);
                msg_format("%^s picks up %s.", mon_name, obj_name);
            }
            pile->loc.where = INV_MON_PACK;
            pile->loc.v.mon_pack.dun_id = dun->dun_id;
            pile->loc.v.mon_pack.obj_id = obj_id; 
            pile->loc.v.mon_pack.mon_id = mon->id;

            pile->marked &= OM_PICKUP_MASK;
            pile->next = mon->obj;
            mon->obj = pile;
        }
        else
        {
            if (mon->ml && msg && !_mon_ignore_pickup(pile)) /* invisible monsters should skip this msg */
            {
                object_desc(obj_name, pile, OD_COLOR_CODED);
                msg_format("%^s tries to pick up %s, but fails.", mon_name, obj_name);
            }
            pile->next = leave;
            leave = pile;
        }
        pile = next;
    }
    if (leave)
        point_map_add(dun->obj_pos, mon->pos, leave);
    else
        point_map_delete(dun->obj_pos, mon->pos);

    dun_assert_obj(dun);
}
void dun_mon_steal_plr(dun_ptr dun, mon_ptr mon, obj_ptr obj)
{
    assert(dun->dun_id == mon->dun_id);
    assert(int_map_find(dun->mon, mon->id));

    obj = obj_copy(obj);
    obj->loc.where = INV_MON_PACK;
    obj->loc.v.mon_pack.dun_id = dun->dun_id;
    obj->loc.v.mon_pack.obj_id = dun_mgr_next_obj_id();
    obj->loc.v.mon_pack.mon_id = mon->id;

    int_map_add(dun->obj, _obj_id(obj), obj);
    obj->marked &= OM_PICKUP_MASK;
    obj->next = mon->obj;
    mon->obj = obj;
}
static bool _mon_can_destroy(mon_ptr mon, obj_ptr obj)
{
    if (obj->tval == TV_GOLD) return FALSE;
    if (obj->name2 == EGO_AMMO_ENDURANCE) return FALSE; /* XXX OF_NO_DESTROY? */
    if (obj_is_art(obj)) return FALSE;
    if (obj_slays_mon(obj, mon)) return FALSE;
    return TRUE;
}
void dun_mon_destroy(dun_ptr dun, mon_ptr mon)
{
    obj_ptr pile, leave = NULL;
    bool    msg = FALSE;
    char    mon_name[MAX_NLEN_MON];
    char    obj_name[MAX_NLEN_OBJ];

    assert(dun_pos_interior(dun, mon->pos));
    assert(dun->dun_id == mon->dun_id);
    assert(int_map_find(dun->mon, mon->id));
    dun_assert_obj(dun);

    pile = point_map_find(dun->obj_pos, mon->pos);
    if (!pile) return;

    if (p_ptr->dun_id == dun->dun_id && plr_can_see(mon->pos))
    {
        msg = TRUE;
        monster_desc(mon_name, mon, MD_INDEF_HIDDEN);
    }

    while (pile)
    {
        obj_ptr next = pile->next;
        assert(pile->loc.where == INV_FLOOR);
        if (_mon_can_destroy(mon, pile))
        {
            if (msg) /* invisible monsters still generate this msg */
            {
                object_desc(obj_name, pile, OD_COLOR_CODED);
                msg_format("%^s destroys %s.", mon_name, obj_name);
            }
            int_map_detach(dun->obj, _obj_id(pile));
            pile->loc.where = INV_JUNK;
            vec_push(dun->junkpile, pile);
        }
        else
        {
            if (mon->ml && msg) /* e.g. invisible monsters should skip this msg */
            {
                object_desc(obj_name, pile, OD_COLOR_CODED);
                msg_format("%^s tries to destroy %s, but fails.", mon_name, obj_name);
            }
            pile->next = leave;
            leave = pile;
        }
        pile = next;
    }
    if (leave)
        point_map_add(dun->obj_pos, mon->pos, leave);
    else
        point_map_delete(dun->obj_pos, mon->pos);
    dun_assert_obj(dun);
}
obj_ptr dun_detach_obj(dun_ptr dun, int id)
{
    obj_ptr obj = int_map_find(dun->obj, id);

    assert(obj);
    assert(obj->loc.where == INV_FLOOR);  /* XXX not expecting a monster carried object */
    assert(obj->loc.v.floor.dun_id == dun->dun_id);
    assert(obj->loc.v.floor.obj_id == id);

    if (obj->loc.where == INV_FLOOR)
    {
        point_t pos = point_create(obj->loc.v.floor.x, obj->loc.v.floor.y);
        obj_ptr pile = point_map_find(dun->obj_pos, pos);
        obj_ptr cur, prev = NULL;
        assert(pile);

        /* unlink obj from pile */
        for (cur = pile; cur; prev = cur, cur = cur->next)
        {
            if (cur == obj)
            {
                if (!prev) pile = cur->next;
                else prev->next = cur->next;
                break;
            }
        }
        /* update obj_pos map with modified pile */
        if (pile) point_map_add(dun->obj_pos, pos, pile);  /* XXX could be skipped if obj wasn't head */
        else point_map_delete(dun->obj_pos, pos);
        obj->next = NULL;
    }
    int_map_detach(dun->obj, id);
    memset(&obj->loc, 0, sizeof(obj_loc_t));
    return obj;
}
void dun_delete_obj(dun_ptr dun, int id)
{
    obj_ptr obj = dun_detach_obj(dun, id); /* re-use pile handling */
    obj->loc.where = INV_JUNK;
    vec_push(dun->junkpile, obj);
}
void dun_destroy_obj_at(dun_ptr dun, point_t pos)
{
    obj_ptr obj;
    dun_assert_obj(dun);
    for (obj = point_map_find(dun->obj_pos, pos); obj; obj = obj->next)
    {
        int_map_detach(dun->obj, obj->loc.v.floor.obj_id);
        obj->loc.where = INV_JUNK;
        if (!(dun->flags & DF_GENERATED))
        {
            if (obj->name1)
                a_info[obj->name1].generated = FALSE;
            else if (random_artifacts && obj->name3)
                a_info[obj->name3].generated = FALSE;
        }
        vec_push(dun->junkpile, obj);
    }
    point_map_delete(dun->obj_pos, pos);
    dun_assert_obj(dun);
}
void dun_iter_obj(dun_ptr dun, void (*f)(int id, obj_ptr obj))
{
    int_map_iter(dun->obj, (int_map_iter_f)f);
}
void dun_iter_floor_obj(dun_ptr dun, void (*f)(point_t pos, obj_ptr pile))
{
    point_map_iter(dun->obj_pos, (point_map_iter_f)f);
}
vec_ptr dun_filter_obj(dun_ptr dun, obj_p p)
{
    vec_ptr v = vec_alloc(NULL);
    point_map_iter_ptr iter;
    for (iter = point_map_iter_alloc(dun->obj_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        obj_ptr obj = point_map_iter_current(iter);
        if (p && !p(obj)) continue;
        vec_add(v, obj);
    }
    point_map_iter_free(iter);

    return v;
}

/************************************************************************
 * Drop Near
 ************************************************************************/
static void _combine(dun_ptr dun, obj_ptr obj, point_t pos)
{
    obj_ptr pile;
    for (pile = point_map_find(dun->obj_pos, pos); pile; pile = pile->next)
    {
        if (obj_can_combine(pile, obj, 0))
        {
            obj_combine(pile, obj, 0);
            if (obj->number <= 0) break;
        }
    }
}
static int _drop_pos_weight(point_t pos, dun_grid_ptr grid)
{
    if (!dun_grid_allow_drop(grid)) return 0;
    return 1;
}
static point_t _get_drop_pos(dun_ptr dun, obj_ptr obj, point_t pos) /* from drop_near with slight mods */
{
    point_t d, bp = pos;
    int bs = -1, bn = 0, i;
    bool flag = FALSE;

    /* look for a local drop position */
    for (d.y = -3; d.y <= 3; d.y++)
    {
        for (d.x = -3; d.x <= 3; d.x++)
        {
            point_t p = point_add(pos, d);
            int dis = d.x*d.x + d.y*d.y;
            bool comb = FALSE;
            int ct = 0, s;
            obj_ptr pile;

            if (dis > 10) continue;
            if (!dun_pos_interior(dun, p)) continue;
            if (!point_project(pos, p)) continue;
            if (!dun_allow_drop_at(dun, p)) continue;

            for (pile = point_map_find(dun->obj_pos, p); pile; pile = pile->next)
            {
                if (obj_can_combine(pile, obj, 0)) comb = TRUE;
                ct++;
            }
            if (!comb) ct++;
            if (ct > 99) continue; /* grid is full! */
            s = 1000 - (dis + ct*5); /* score */
            if (s < bs) continue;
            if (s > bs) bn = 0;    /* new best score */
            if ((++bn >= 2) && !one_in_(bn)) continue; /* randomly choose equivalent scores */
            bs = s;
            bp = p;
            flag = TRUE;
        }
    }
    if (!flag && !obj_is_art(obj))
        return point_create(-1, -1);

    /* for artifacts, we'll try hard to find a place for it.
     * [1] random walk from starting pos: */
    for (i = 0; !flag && i < 1000; i++)
    {
        point_t p = point_step(bp, randint1(9));
        if (!dun_pos_interior(dun, p)) continue;
        bp = p;
        if (!dun_allow_drop_at(dun, bp)) continue;
        flag = TRUE;
    }
    /* [2] globally random pos */
    if (!flag)
    {
        bp = dun_random_grid(dun, _drop_pos_weight);
        flag = dun_pos_interior(dun, bp);
    }
    
    /* [3] give up */
    if (!flag)
    {
        if (obj_is_std_art(obj) && !obj_is_known(obj))
            a_info[obj->name1].generated = FALSE;
        if (random_artifacts && obj->name3 && !obj_is_known(obj))
            a_info[obj->name3].generated = FALSE;
        return point_create(-1, -1);
    }

    return bp;
}
static void _msg_drop_disappears(dun_ptr dun, obj_ptr obj, point_t pos)
{
    if (p_ptr->dun_id == dun->dun_id && plr_can_see(pos))
    {
        char name[MAX_NLEN_OBJ];
        bool plural = obj->number > 1;
        object_desc(name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
        cmsg_format(TERM_L_RED, "The %s disappear%s.", name, plural ? "" : "s");
    }
}
bool dun_drop_near(dun_ptr dun, obj_ptr obj, point_t pos)
{
    point_t drop_pos;
    dun_assert_obj(dun);
    if (dun->dun_type_id == D_MOUNT_DOOM && obj->name1 == ART_POWER)
    {
        dun_grid_ptr g = dun_grid_at(dun, pos);
        if (g->feat == feat_deep_lava)
        {
            char name[MAX_NLEN_OBJ];
            object_desc(name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
            cmsg_format(TERM_L_RED, "The %s is destroyed! You have rid Middle Earth of a great evil!", name);
            p_ptr->fame += 25;
            return FALSE;
        }
    }
    drop_pos = _get_drop_pos(dun, obj, pos);
    if (!dun_pos_interior(dun, drop_pos))
    {
        _msg_drop_disappears(dun, obj, pos);
        return FALSE;
    }
    _combine(dun, obj, drop_pos);
    if (obj->number > 0)
    {
        obj_ptr pile;
        obj = obj_copy(obj);
        obj->loc.where = INV_FLOOR;
        obj->loc.v.floor.dun_id = dun->dun_id;
        obj->loc.v.floor.obj_id = dun_mgr_next_obj_id();
        obj->loc.v.floor.x = drop_pos.x;
        obj->loc.v.floor.y = drop_pos.y;

        int_map_add(dun->obj, _obj_id(obj), obj);
        pile = point_map_find(dun->obj_pos, drop_pos);
        if (pile) obj->next = pile;
        point_map_add(dun->obj_pos, drop_pos, obj);

        dun_note_pos(dun, drop_pos);
        dun_lite_pos(dun, drop_pos);
        p_ptr->window |= PW_OBJECT_LIST;
        if (dun_plr_at(dun, drop_pos)) /* XXX skip if do_cmd_drop */
            msg_print("You feel something roll beneath your feet.");
    }
    dun_assert_obj(dun);
    return TRUE;
}
bool dun_drop_break_near(dun_ptr dun, obj_ptr obj, point_t pos, int break_pct)
{
    if (randint0(100) < break_pct)
    {
        _msg_drop_disappears(dun, obj, pos);
        stats_on_m_destroy(obj, 1);
        return FALSE;
    }
    return dun_drop_near(dun, obj, pos);
}
void dun_mon_drop_carried_obj(dun_ptr dun, mon_ptr mon)
{
    obj_ptr obj, next;
    dun_assert_obj(dun);
    for (obj = mon->obj; obj; obj = next)
    {
        point_t drop_pos = _get_drop_pos(dun, obj, mon->pos);
        next = obj->next;
        if (dun_pos_interior(dun, drop_pos))
        {
            _combine(dun, obj, drop_pos);
            if (obj->number > 0)
            {
                obj_ptr pile = point_map_find(dun->obj_pos, drop_pos);
                u16b    obj_id = _obj_id(obj);

                obj->loc.where = INV_FLOOR;
                obj->loc.v.floor.dun_id = dun->dun_id;
                obj->loc.v.floor.obj_id = obj_id;
                obj->loc.v.floor.x = drop_pos.x;
                obj->loc.v.floor.y = drop_pos.y;

                if (pile) obj->next = pile;
                else obj->next = NULL;
                point_map_add(dun->obj_pos, drop_pos, obj);

                dun_note_pos(dun, drop_pos);
                dun_lite_pos(dun, drop_pos);
                p_ptr->window |= PW_OBJECT_LIST;
                if (dun_plr_at(dun, drop_pos))
                    msg_print("You feel something roll beneath your feet.");
            }
            else
            {
                int_map_detach(dun->obj, _obj_id(obj));
                obj->loc.where = INV_JUNK;
                vec_push(dun->junkpile, obj);
            }
        }
        else
        {
            assert(!obj_is_art(obj));
            int_map_detach(dun->obj, _obj_id(obj));
            obj->loc.where = INV_JUNK;
            vec_push(dun->junkpile, obj);
            _msg_drop_disappears(dun, obj, mon->pos);
        }
    }
    mon->obj = NULL;
    dun_assert_obj(dun);
}
