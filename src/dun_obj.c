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
static point_t _obj_pos(obj_ptr obj)
{
    point_t pos = point_create(-1, -1);
    if (obj->loc.where == INV_FLOOR)
    {
        pos.x = obj->loc.v.floor.x;
        pos.y = obj->loc.v.floor.y;
    }
    return pos;
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
            point_t pos = _obj_pos(obj);
            obj_ptr pile = point_map_find(dun->obj_pos, pos);
            bool    found = FALSE;
            assert(pile);
            for (; pile; pile = pile->next)
            {
                point_t pos2 = _obj_pos(pile);
                if (pile == obj) found = TRUE;
                assert(pile->loc.where == INV_FLOOR);
                assert(pile->loc.v.floor.dun_id == dun->id);
                if (!point_equals(pos2, pos))
                {
                    obj_ptr pile2 = point_map_find(dun->obj_pos, pos2);
                    /* this check failed once. same pile under 2 positions! */
                    assert(!pile2);
                }
                assert(point_equals(pos, pos2));
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
                assert(pile->loc.v.mon_pack.dun_id == dun->id);
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
        assert(pile->loc.v.floor.dun_id == dun->id);
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
            assert(pile->loc.v.mon_pack.dun_id == dun->id);
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
    assert(dun_pos_interior(dun, pos));
    assert(dun_allow_drop_at(dun, pos));
    assert(!obj->next);

    obj = obj_copy(obj);
    obj->loc.where = INV_FLOOR;
    obj->loc.v.floor.dun_id = dun->id;
    obj->loc.v.floor.obj_id = dun_mgr_next_obj_id();
    obj->loc.v.floor.x = pos.x;
    obj->loc.v.floor.y = pos.y;

    int_map_add(dun->obj, _obj_id(obj), obj);

    obj->next = point_map_find(dun->obj_pos, pos);
    point_map_add(dun->obj_pos, pos, obj);

    dun_assert_obj(dun);
}
bool dun_fetch_obj(dun_ptr dun, point_t from, point_t to)
{
    obj_ptr src = point_map_find(dun->obj_pos, from);

    if (!src) return FALSE;

    if (src->next) point_map_add(dun->obj_pos, from, src->next);
    else point_map_delete(dun->obj_pos, from);

    src->loc.v.floor.x = to.x;
    src->loc.v.floor.y = to.y;

    src->next = point_map_find(dun->obj_pos, to);
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
bool dun_mon_pickup(dun_ptr dun, mon_ptr mon)
{
    obj_ptr pile, leave = NULL;
    bool    msg = FALSE, notice = FALSE;
    char    mon_name[MAX_NLEN_MON];
    char    obj_name[MAX_NLEN_OBJ];

    assert(dun_pos_interior(dun, mon->pos));
    assert(dun == mon->dun);
    assert(int_map_find(dun->mon, mon->id));
    dun_assert_obj(dun);

    pile = point_map_find(dun->obj_pos, mon->pos);
    if (!pile) return notice;

    if (plr->dun_id == dun->id && plr_can_see(mon->pos))
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
                mon_lore_pickup_obj(mon);
                notice = TRUE;
            }
            pile->loc.where = INV_MON_PACK;
            pile->loc.v.mon_pack.dun_id = dun->id;
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
                mon_lore_pickup_obj(mon);
                notice = TRUE;
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
    return notice;
}
void dun_mon_steal_plr(dun_ptr dun, mon_ptr mon, obj_ptr obj)
{
    assert(dun == mon->dun);
    assert(int_map_find(dun->mon, mon->id));

    obj = obj_copy(obj);
    obj->loc.where = INV_MON_PACK;
    obj->loc.v.mon_pack.dun_id = dun->id;
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
bool dun_mon_destroy(dun_ptr dun, mon_ptr mon)
{
    obj_ptr pile, leave = NULL;
    bool    msg = FALSE, notice = FALSE;
    char    mon_name[MAX_NLEN_MON];
    char    obj_name[MAX_NLEN_OBJ];

    assert(dun_pos_interior(dun, mon->pos));
    assert(dun == mon->dun);
    assert(int_map_find(dun->mon, mon->id));
    dun_assert_obj(dun);

    pile = point_map_find(dun->obj_pos, mon->pos);
    if (!pile) return notice;

    if (plr->dun_id == dun->id && plr_can_see(mon->pos))
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
                mon_lore_destroy_obj(mon);
                notice = TRUE;
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
                mon_lore_destroy_obj(mon);
                notice = TRUE;
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
    return notice;
}
obj_ptr dun_detach_obj(dun_ptr dun, int id)
{
    obj_ptr obj = int_map_find(dun->obj, id);

    assert(obj);
    assert(obj->loc.where == INV_FLOOR);  /* XXX not expecting a monster carried object */
    assert(obj->loc.v.floor.dun_id == dun->id);
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
    }
    int_map_detach(dun->obj, id);
    memset(&obj->loc, 0, sizeof(obj_loc_t));
    obj->next = NULL;
    dun_assert_obj(dun);
    return obj;
}
void dun_delete_obj(dun_ptr dun, int id)
{
    obj_ptr obj = dun_detach_obj(dun, id); /* re-use pile handling */
    obj->loc.where = INV_JUNK;
    vec_push(dun->junkpile, obj);
}
bool dun_can_destroy(dun_ptr dun, point_t pos)
{
    dun_grid_ptr g = dun_grid_at(dun, pos);
    if (g->flags & CELL_PERM) return FALSE;
    return dun_can_destroy_obj_at(dun, pos);
}
bool dun_can_destroy_obj_at(dun_ptr dun, point_t pos)
{
    obj_ptr obj;
    for (obj = point_map_find(dun->obj_pos, pos); obj; obj = obj->next)
    {
        if (obj_is_art(obj)) return FALSE;
    }
    return TRUE;
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
            if (obj->art_id)
                arts_lookup(obj->art_id)->generated = FALSE;
            if (obj->replacement_art_id)
                arts_lookup(obj->replacement_art_id)->generated = FALSE;
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
    if (!cell_allow_obj(grid)) return 0;
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
        if (!obj_is_known(obj))
        {
            if (obj->art_id)
                arts_lookup(obj->art_id)->generated = FALSE;
            if (obj->replacement_art_id)
                arts_lookup(obj->replacement_art_id)->generated = FALSE;
        }
        return point_create(-1, -1);
    }

    return bp;
}
static void _msg_drop_disappears(dun_ptr dun, obj_ptr obj, point_t pos)
{
    if (plr->dun_id == dun->id && plr_can_see(pos))
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
    if (dun->type->id == D_MOUNT_DOOM && obj_is_specified_art(obj, "=.Power"))
    {
        dun_grid_ptr g = dun_grid_at(dun, pos);
        if (lava_is_deep(g))
        {
            char name[MAX_NLEN_OBJ];
            object_desc(name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
            cmsg_format(TERM_L_RED, "The %s is destroyed! You have rid Middle Earth of a great evil!", name);
            plr->fame += 25;
            return FALSE;
        }
    }
    if (object_is_(obj, TV_POTION, SV_POTION_BLOOD))
    {
        msg_print("The potion goes sour.");
        obj->sval = SV_POTION_SALT_WATER;
        obj->k_idx = lookup_kind(TV_POTION, SV_POTION_SALT_WATER);
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
        obj = obj_copy(obj);
        obj->loc.where = INV_FLOOR;
        obj->loc.v.floor.dun_id = dun->id;
        obj->loc.v.floor.obj_id = dun_mgr_next_obj_id();
        obj->loc.v.floor.x = drop_pos.x;
        obj->loc.v.floor.y = drop_pos.y;

        int_map_add(dun->obj, _obj_id(obj), obj);
        obj->next = point_map_find(dun->obj_pos, drop_pos);
        point_map_add(dun->obj_pos, drop_pos, obj);

        dun_note_pos(dun, drop_pos);
        dun_draw_pos(dun, drop_pos);
        plr->window |= PW_OBJECT_LIST;
        if (obj->marked & OM_NO_MSG)
            obj->marked &= ~OM_NO_MSG;
        else if (dun_plr_at(dun, drop_pos))
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
        stats_on_m_destroy(obj, obj->number);
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
                u16b obj_id = _obj_id(obj);

                obj->loc.where = INV_FLOOR;
                obj->loc.v.floor.dun_id = dun->id;
                obj->loc.v.floor.obj_id = obj_id;
                obj->loc.v.floor.x = drop_pos.x;
                obj->loc.v.floor.y = drop_pos.y;

                obj->next = point_map_find(dun->obj_pos, drop_pos);
                point_map_add(dun->obj_pos, drop_pos, obj);

                dun_note_pos(dun, drop_pos);
                dun_draw_pos(dun, drop_pos);
                plr->window |= PW_OBJECT_LIST;
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

/************************************************************************
 * Fix Broken Savefiles (XXX Need to find the Bug(s)! XXX)
 * XXX This duplicates dun_assert_obj. When I play, it is a checked
 * build inside the debugger, and I want execution to halt at the point
 * of inconsistency. But for an end-user, we need to just detect and
 * fix a corrupted savefile. Of course, code just works when I play ;D
 * XXX Objects may be lost during dun_obj_panic, but at least the user
 * can keep playing!
 ************************************************************************/
static bool _valid_obj_loc(obj_ptr obj)
{
    switch (obj->loc.where)
    {
    case INV_FLOOR:
    case INV_MON_PACK:
        return TRUE;
    }
    return FALSE;
}
static bool _check_obj(dun_ptr dun)
{
    bool ok = TRUE;
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->obj);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        obj_ptr obj = int_map_iter_current(iter);
        if (!_valid_obj_loc(obj)) ok = FALSE;
        if (obj->loc.where == INV_FLOOR)
        {
            point_t pos = _obj_pos(obj);
            obj_ptr pile = point_map_find(dun->obj_pos, pos);
            bool    found = FALSE;
            if (!pile) ok = FALSE;
            for (; pile; pile = pile->next)
            {
                point_t pos2 = _obj_pos(pile);
                if (pile == obj) found = TRUE;
                if (pile->loc.where != INV_FLOOR) ok = FALSE;
                if (!point_equals(pos2, pos)) ok = FALSE;
            }
            if (!found) ok = FALSE;
        }
        else
        {
            mon_ptr mon = int_map_find(dun->mon, obj->loc.v.mon_pack.mon_id);
            obj_ptr pile = mon->obj;
            bool    found = FALSE;
            for (; pile; pile = pile->next)
            {
                if (pile == obj) found = TRUE;
                if (pile->loc.where != INV_MON_PACK) ok = FALSE;
                if (pile->loc.v.mon_pack.mon_id != mon->id) ok = FALSE;
            }
            if (!found) ok = FALSE;
        }
    }
    int_map_iter_free(iter);
    return ok;
}
static bool _check_pile(dun_ptr dun, obj_ptr pile, point_t pos)
{
    bool ok = TRUE;
    for (; pile; pile = pile->next)
    {
        point_t pos2 = _obj_pos(pile);
        if (!point_equals(pos2, pos)) ok = FALSE;
    }
    return ok;
}
static bool _check_obj_pos(dun_ptr dun)
{
    bool ok = TRUE;
    point_map_iter_ptr iter;
    for (iter = point_map_iter_alloc(dun->obj_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        obj_ptr pile = point_map_iter_current(iter);
        point_t pos = point_map_iter_current_key(iter);
        if (!_check_pile(dun, pile, pos)) ok = FALSE;
    }
    point_map_iter_free(iter);
    return ok;
}
static bool _check_mon_pack(dun_ptr dun)
{
    bool ok = TRUE;
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        obj_ptr pile = mon->obj;
        for (; pile; pile = pile->next)
        {
            if (pile->loc.where != INV_MON_PACK) ok = FALSE;
            else if (pile->loc.v.mon_pack.mon_id != mon->id) ok = FALSE;
        }
    }
    int_map_iter_free(iter);
    return ok;
}
bool dun_obj_integrity(dun_ptr dun)
{
    if (!_check_obj_pos(dun)) return FALSE;
    if (!_check_mon_pack(dun)) return FALSE;
    if (!_check_obj(dun)) return FALSE;
    return TRUE;
}
static void _mon_clear_obj(int id, mon_ptr mon) { mon->obj = NULL; }
void dun_obj_panic(dun_ptr dun)
{
    vec_ptr garbage = vec_alloc(NULL);
    int_map_iter_ptr iter;
    int i;

    /* clear floor piles and monster packs */
    point_map_clear(dun->obj_pos);
    int_map_iter(dun->mon, (int_map_iter_f)_mon_clear_obj);

    /* rebuild everything */
    for (iter = int_map_iter_alloc(dun->obj);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        obj_ptr obj = int_map_iter_current(iter);
        int     id = int_map_iter_current_key(iter);
        if (obj->loc.where == INV_FLOOR)
        {
            point_t pos = _obj_pos(obj);
            obj->next = point_map_find(dun->obj_pos, pos);
            point_map_add(dun->obj_pos, pos, obj);
        }
        else if (obj->loc.where == INV_MON_PACK)
        {
            mon_ptr mon = int_map_find(dun->mon, obj->loc.v.mon_pack.mon_id);
            if (mon)
            {
                obj->next = mon->obj;
                mon->obj = obj;
            }
            else vec_add_int(garbage, id);
        }
        else vec_add_int(garbage, id);
    }
    int_map_iter_free(iter);

    /* delete garbage */
    for (i = 0; i < vec_length(garbage); i++)
    {
        int id = vec_get_int(garbage, i);
        int_map_delete(dun->obj, id);
    }
    vec_free(garbage);
    dun_assert_obj(dun);
}
