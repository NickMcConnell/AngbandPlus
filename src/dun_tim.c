#include "angband.h"

#include <assert.h>

/************************************************************************
 * Default Timers
 ************************************************************************/
static dun_tim_info_ptr _bomb(void);
static dun_tim_info_ptr _illusion(void);
static dun_tim_info_ptr _klaxon(void);

/************************************************************************
 * Registered Timer Info
 ************************************************************************/
static void _register(int_map_ptr map, dun_tim_info_ptr info)
{
    int_map_add(map, info->id, info);
}
static int_map_ptr _info(void)
{
    static int_map_ptr map = NULL;
    if (!map)
    {
        map = int_map_alloc(free);
        _register(map, _bomb());
        _register(map, _klaxon());
        _register(map, _illusion());
    }
    return map;
}
dun_tim_info_ptr dun_tim_info_alloc(int id, cptr name)
{
    dun_tim_info_ptr info = malloc(sizeof(dun_tim_info_t));
    memset(info, 0, sizeof(dun_tim_info_t));
    info->id = id;
    info->name = name;
    return info;
}
void dun_tim_register(dun_tim_info_ptr info)
{
    assert(info);
    assert(info->id);
    assert(info->name);
    assert(info->on_f);
    assert(info->off_f);
    _register(_info(), info);
}
dun_tim_info_ptr dun_tim_info_lookup(int id)
{
    dun_tim_info_ptr info = int_map_find(_info(), id);
    assert(info);
    assert(info->id == id);
    return info;
}
/************************************************************************
 * Timer Queue
 ************************************************************************/
static bool _tick = FALSE;

static dun_tim_ptr _find(dun_ptr dun, int id)
{
    dun_tim_ptr t;
    for (t = dun->timers; t; t = t->next)
    {
        if (t->id == id) return t;
    }
    return NULL;
}
static dun_tim_ptr _find_at(dun_ptr dun, point_t pos, int id)
{
    dun_tim_ptr t;
    for (t = dun->timers; t; t = t->next)
    {
        if (t->id == id && point_equals(pos, t->pos)) return t;
    }
    return NULL;
}
static int _count(dun_ptr dun)
{
    int ct = 0;
    dun_tim_ptr t;
    for (t = dun->timers; t; t = t->next) ct++;
    return ct;
}
static void _off(dun_tim_ptr t)
{
    dun_tim_info_ptr info = dun_tim_info_lookup(t->id);
    info->off_f(t);
}
static void _add_tail(dun_ptr dun, dun_tim_ptr tim)
{
    dun_tim_ptr t = dun->timers;
    assert(tim->dun == dun);
    while (t && t->next) t = t->next;
    if (!t) dun->timers = tim;
    else t->next = tim;
}
static void _add_head(dun_ptr dun, dun_tim_ptr tim)
{
    assert(tim->dun == dun);
    tim->next = dun->timers;
    dun->timers = tim;
}
static void _add(dun_ptr dun, dun_tim_ptr t)
{
    dun_tim_info_ptr info = dun_tim_info_lookup(t->id);
    _add_head(dun, t);
    t->flags |= info->flags;
    info->on_f(t);
}
static void _remove(dun_ptr dun, dun_tim_ptr tim)
{
    dun_tim_ptr t;
    dun_tim_ptr p = NULL;
    if (_tick)
    {   /* wait for _cleanup() */
        tim->count = 0;
        return;
    }
    for (t = dun->timers; t; p = t, t = t->next)
    {
        if (t == tim)
        {
            if (!p) dun->timers = t->next;
            else p->next = t->next;
            _off(t);
            free(t);
            break;
        }
    }
}
static dun_tim_ptr _alloc(dun_ptr dun, int id, int count)
{
    dun_tim_ptr t = malloc(sizeof(dun_tim_t));
    memset(t, 0, sizeof(dun_tim_t));
    t->dun = dun;
    t->id = id;
    t->count = count;
    return t;
}
static dun_tim_ptr _alloc_at(dun_ptr dun, point_t pos, int id, int count)
{
    dun_tim_ptr t = _alloc(dun, id, count);
    t->pos = pos;
    return t;
}
static void _cleanup(dun_ptr dun)
{
    dun_tim_ptr t = dun->timers, p = NULL;
    while (t)
    {
        if (t->count <= 0)
        {
            dun_tim_ptr x = t;
            if (!p) dun->timers = t->next;
            else p->next = t->next;
            _off(t);
            t = t->next;
            free(x);
        }
        else
        {
            p = t;
            t = t->next;
        }
    }
}
/************************************************************************
 * Public
 ************************************************************************/
dun_tim_ptr dun_tim_add_at(dun_ptr dun, point_t pos, int id, int count)
{
    dun_tim_ptr t;
    assert(count > 0);
    if (count <= 0) return NULL;
    t = _find_at(dun, pos, id);
    if (t)
        t->count = MAX(t->count, count);
    else
    {
        t = _alloc_at(dun, pos, id, count);
        _add(dun, t);
    }
    return t;
}
bool dun_tim_remove_at(dun_ptr dun, point_t pos, int id)
{
    dun_tim_ptr t = _find_at(dun, pos, id);
    if (t)
    {
        _remove(dun, t);
        return TRUE;
    }
    return FALSE;
}
dun_tim_ptr dun_tim_add(dun_ptr dun, int id, int count)
{
    dun_tim_ptr t;
    assert(count > 0);
    if (count <= 0) return NULL;
    t = _find(dun, id);
    if (t)
        t->count = MAX(t->count, count);
    else
    {
        t = _alloc(dun, id, count);
        _add(dun, t);
    }
    return t;
}
bool dun_tim_remove(dun_ptr dun, int id)
{
    dun_tim_ptr t = _find(dun, id);
    if (t)
    {
        _remove(dun, t);
        return TRUE;
    }
    return FALSE;
}
void dun_tim_tick(dun_ptr dun)
{
    dun_tim_ptr t;
    if (plr->is_dead) return;
    _tick = TRUE;
    for (t = dun->timers; t; t = t->next)
    {
        if (t->count > 0)
        {
            dun_tim_info_ptr info = dun_tim_info_lookup(t->id);
            if (info->tick_f)
            {
                info->tick_f(t);
                if (plr->is_dead) return;
            }
            else
                t->count--;
        }
    }
    _cleanup(dun);
    _tick = FALSE;
}

/************************************************************************
 * Savefiles
 ************************************************************************/
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
void dun_tim_load(dun_ptr dun, savefile_ptr file)
{
    int ct, i;
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        s16b id = savefile_read_s16b(file);
        s16b count = savefile_read_s16b(file);
        dun_tim_ptr t = _alloc(dun, id, count);
        t->pos = _point_load(file);
        t->flags = savefile_read_u16b(file);
        t->parm1 = savefile_read_s16b(file);
        t->parm2 = savefile_read_s16b(file);
        t->old_feat = savefile_read_s16b(file);
        _add_tail(dun, t);
    }
}
void dun_tim_save(dun_ptr dun, savefile_ptr file)
{
    dun_tim_ptr t;
    savefile_write_s16b(file, _count(dun));
    for (t = dun->timers; t; t = t->next)
    {
        savefile_write_s16b(file, t->id);
        savefile_write_s16b(file, t->count);
        _point_save(t->pos, file);
        savefile_write_u16b(file, t->flags);
        savefile_write_s16b(file, t->parm1);
        savefile_write_s16b(file, t->parm2);
        savefile_write_s16b(file, t->old_feat);
    }
}
/************************************************************************
 * Utilities
 ************************************************************************/
static bool _show_msg(dun_tim_ptr timer)
{
    if (!dun_pos_interior(timer->dun, timer->pos)) return FALSE;
    if (!plr_can_see(timer->pos)) return FALSE;
    return TRUE;
}
/************************************************************************
 * DT_BOMB
 ************************************************************************/
static void _bomb_on(dun_tim_ptr timer)
{
}
static void _bomb_off(dun_tim_ptr timer)
{
    if (timer->parm1 != GF_NONE && timer->parm2 > 0)
    {
        if (_show_msg(timer))
            msg_print("The bomb goes off!");
        dun_burst(timer->dun, who_create_trap(timer->pos), 5, timer->pos, timer->parm1, timer->parm2);
    }
}
static void _bomb_disarm(dun_tim_ptr timer)
{
    /* XXX occasionally go off anyway? */
    timer->parm1 = GF_NONE;
    timer->parm2 = 0;
    timer->count = 0;
}
static dun_tim_info_ptr _bomb(void)
{
    dun_tim_info_ptr info = dun_tim_info_alloc(DT_BOMB, "Bomb");
    info->desc = "Its bound to go off sooner or later!";
    info->on_f = _bomb_on;
    info->off_f = _bomb_off;
    info->disarm_f = _bomb_disarm;
    return info;
}
/************************************************************************
 * DT_ILLUSION (XXX This is a proof of concept ... needs improvement)
 ************************************************************************/
static void _illusion_stuff(void)
{
    plr->update |= PU_FLOW | PU_MON_FLOW;
    plr->redraw |= PR_MAP;
}
static void _illusion_on(dun_tim_ptr timer)
{
    dun_place_illusory_wall(timer->dun, timer->pos);
    _illusion_stuff();
    if (_show_msg(timer)) msg_print("You conjure a wall of illusion.");
}
static void _illusion_off(dun_tim_ptr timer)
{
    if (dun_pos_valid(timer->dun, timer->pos)) /* D_SURFACE scrolling ... */
    {
        dun_grid_ptr grid = dun_grid_at(timer->dun, timer->pos);
        floor_remove_illusion(grid);
        dun_draw_pos(timer->dun, timer->pos);
        _illusion_stuff();
        if (_show_msg(timer)) msg_print("Your illusion dissipates.");
    }
}
static dun_tim_info_ptr _illusion(void)
{
    dun_tim_info_ptr info = dun_tim_info_alloc(DT_ILLUSION, "Illusion");
    info->desc = "Nothing lasts forever ...";
    info->on_f = _illusion_on;
    info->off_f = _illusion_off;
    return info;
}
/************************************************************************
 * DT_KLAXON
 ************************************************************************/
static point_t _klaxon_pos;
static void _lure(int id, mon_ptr mon)
{
    int d;
    if (mon_is_pet(mon)) return; /* don't lure pets */
    if (point_equals(mon->target_pos, _klaxon_pos)) return; /* already lured to this klaxon */
    d = point_fast_distance(mon->pos, _klaxon_pos);
    if (dun_pos_interior(mon->dun, mon->target_pos))
    {
        int d2 = point_fast_distance(mon->pos, mon->target_pos);
        if (d2 < d) return; /* don't replace a better lure */
    }
    if (d < MAX_SIGHT * 5) /* It's *really* loud! */
    {
        mon_tim_remove(mon, MT_SLEEP);
        mon->target_pos = _klaxon_pos;
    }
}
static void _unlure(int id, mon_ptr mon)
{
    if (point_equals(mon->target_pos, _klaxon_pos))
    {
        mon->target_pos.x = 0;
        mon->target_pos.y = 0;
    }
}
static void _klaxon_on(dun_tim_ptr timer)
{
}
static void _klaxon_tick(dun_tim_ptr timer)
{
    timer->count--;
    if (one_in_(3))
    {
        if (timer->dun->id == plr->dun_id)
            msg_print("<color:R>Your Klaxon emits a shrill wail!</color>");
        _klaxon_pos = timer->pos;
        int_map_iter(timer->dun->mon, (int_map_iter_f)_lure);
    }
}
static void _klaxon_off(dun_tim_ptr timer)
{
    _klaxon_pos = timer->pos;
    int_map_iter(timer->dun->mon, (int_map_iter_f)_unlure);
    dun_forget_flow_at(timer->dun, timer->pos);
}
static dun_tim_info_ptr _klaxon(void)
{
    dun_tim_info_ptr info = dun_tim_info_alloc(DT_KLAXON, "Klaxon");
    info->desc = "It's emitting a terrific screech!";
    info->on_f = _klaxon_on;
    info->off_f = _klaxon_off;
    info->tick_f = _klaxon_tick;
    return info;
}

