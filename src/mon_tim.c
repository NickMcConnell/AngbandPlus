#include "angband.h"

#include <assert.h>

/************************************************************************
 * Default Timers
 ************************************************************************/
static mon_tim_info_ptr _berserk(void);
static mon_tim_info_ptr _confused(void);
static mon_tim_info_ptr _ego_whip(void);
static mon_tim_info_ptr _fast(void);
static mon_tim_info_ptr _fear(void);
static mon_tim_info_ptr _paralyzed(void);
static mon_tim_info_ptr _invuln(void);
static mon_tim_info_ptr _sleep(void);
static mon_tim_info_ptr _slow(void);
static mon_tim_info_ptr _stun(void);

mon_tim_info_ptr mon_tim_info_alloc(int id, cptr name)
{
    mon_tim_info_ptr info = malloc(sizeof(mon_tim_info_t));
    memset(info, 0, sizeof(mon_tim_info_t));
    info->id = id;
    info->name = name;
    return info;
}

/************************************************************************
 * Registered Timer Info
 ************************************************************************/
static void _register(int_map_ptr map, mon_tim_info_ptr info)
{
    int_map_add(map, info->id, info);
}
static int_map_ptr _info(void)
{
    static int_map_ptr map = NULL;
    if (!map)
    {
        map = int_map_alloc(free);
        _register(map, _berserk());
        _register(map, _confused());
        _register(map, _ego_whip());
        _register(map, _fast());
        _register(map, _fear());
        _register(map, _invuln());
        _register(map, _paralyzed());
        _register(map, _sleep());
        _register(map, _slow());
        _register(map, _stun());
    }
    return map;
}
static mon_tim_info_ptr _find_info(int id)
{
    mon_tim_info_ptr info = int_map_find(_info(), id);
    assert(info);
    assert(info->id == id);
    return info;
}
void mon_tim_register(mon_tim_info_ptr info)
{
    assert(info);
    assert(info->id);
    assert(info->name);
    assert(info->on_f);
    assert(info->off_f);
    _register(_info(), info);
}

/************************************************************************
 * Timer Queue
 ************************************************************************/
static mon_tim_ptr _find(mon_ptr mon, int id)
{
    mon_tim_ptr t;
    for (t = mon->timers; t; t = t->next)
    {
        if (t->id == id) return t;
    }
    return NULL;
}
static int _count(mon_ptr mon)
{
    int ct = 0;
    mon_tim_ptr t;
    for (t = mon->timers; t; t = t->next) ct++;
    return ct;
}
static void _off(mon_ptr mon, mon_tim_ptr t)
{
    mon_tim_info_ptr info = _find_info(t->id);
    info->off_f(mon, t);
}
static void _add_aux(mon_ptr mon, mon_tim_ptr tim)
{
    /* add to the head ... not that tick_f might add a new
     * timer and we don't want that timer processed until the
     * next tick. */
    tim->next = mon->timers;
    mon->timers = tim;
}
static void _add(mon_ptr mon, mon_tim_ptr t)
{
    mon_tim_info_ptr info = _find_info(t->id);
    _add_aux(mon, t);
    t->flags |= info->flags;
    check_mon_health_redraw(mon->id);
    info->on_f(mon, t);
}
static void _remove(mon_ptr mon, mon_tim_ptr tim)
{
    mon_tim_ptr t;
    mon_tim_ptr p = NULL;
    if (mon->mflag & MFLAG_TICK)
    {   /* wait for _cleanup() */
        tim->count = 0;
        return;
    }
    for (t = mon->timers; t; p = t, t = t->next)
    {
        if (t == tim)
        {
            if (!p) mon->timers = t->next;
            else p->next = t->next;
            _off(mon, t);
            free(t);
            check_mon_health_redraw(mon->id);
            break;
        }
    }
}
static mon_tim_ptr _alloc(int id, int count)
{
    mon_tim_ptr t = malloc(sizeof(mon_tim_t));
    memset(t, 0, sizeof(mon_tim_t));
    t->id = id;
    t->count = count;
    return t;
}

/************************************************************************
 * Manipulate Monster Timers
 ************************************************************************/
void mon_tim_add(mon_ptr mon, int id, int count)
{
    mon_tim_add_aux(mon, id, count, 0);
}
void mon_tim_add_aux(mon_ptr mon, int id, int count, int parm)
{
    mon_tim_ptr t;
    if (count <= 0) return;
    if (mon_is_dead(mon)) return;
    t = _find(mon, id);
    if (t)
    {
        if (!(t->flags & TF_IGNORE))
        {
            if (t->flags & TF_AUGMENT)
                t->count += count;
            else
                t->count = MAX(t->count, count);
            t->parm = parm;
        }
    }
    else
    {
        t = _alloc(id, count);
        t->parm = parm;
        _add(mon, t);
    }
}
void mon_tim_subtract(mon_ptr mon, int id, int count)
{
    mon_tim_ptr t = _find(mon, id);
    assert(count > 0);
    if (!t) return;
    t->count = MAX(0, t->count - count);
    if (t->count <= 0 && !(t->flags & TF_LOCKED))
        _remove(mon, t);
}
void mon_tim_recover(mon_ptr mon, int id, int pct, int min)
{
    mon_tim_ptr t = _find(mon, id);
    assert(pct >= 0);
    assert(min >= 0);
    if (t)
    {   /* use the best recovery method */
        int x = MAX(0, t->count - min);
        int y = t->count * pct / 100;
        t->count = MIN(x, y);
        if (t->count <= 0 && !(t->flags & TF_LOCKED))
            _remove(mon, t);
    }
}
void mon_tim_remove(mon_ptr mon, int id)
{
    mon_tim_ptr t = _find(mon, id);
    if (t) _remove(mon, t);
}
void mon_tim_delete(mon_ptr mon, int id)
{
    mon_tim_ptr t = _find(mon, id);
    if (t)
    {
        t->flags |= TF_NO_MSG;
        _remove(mon, t);
    }
}
void mon_tim_dispel(mon_ptr mon)
{
    mon_tim_ptr t = mon->timers, p = NULL;
    assert (!(mon->mflag & MFLAG_TICK));
    while (t)
    {
        if (!(t->flags & (TF_NO_DISPEL | TF_LOCKED)))
        {
            mon_tim_ptr x = t;
            if (!p) mon->timers = t->next;
            else p->next = t->next;
            _off(mon, t);
            t = t->next;
            free(x);
            check_mon_health_redraw(mon->id);
        }
        else
        {
            p = t;
            t = t->next;
        }
    }
}
void mon_tim_disenchant(mon_ptr mon)
{
    mon_tim_ptr t;
    int tot = 0, n;
    assert (!(mon->mflag & MFLAG_TICK));
    for (t = mon->timers; t; t = t->next)
    {
        if (t->flags & TF_NO_DISPEL) continue;
        if (t->flags & TF_LOCKED) continue;
        tot++;
    }
    if (!tot) return;
    n = randint0(tot);
    for (t = mon->timers; t; t = t->next)
    {
        if (t->flags & TF_NO_DISPEL) continue;
        if (t->flags & TF_LOCKED) continue;
        n--;
        if (n < 0)
        {
             _remove(mon, t);
             break;
        }
    }
}
bool mon_tim_find(mon_ptr mon, int id)
{
    return _find(mon, id) != NULL;
}
int mon_tim_parm(mon_ptr mon, int id)
{
    mon_tim_ptr t = _find(mon, id);
    if (t) return t->parm;
    return 0;
}
int mon_tim_amount(mon_ptr mon, int id)
{
    mon_tim_ptr t = _find(mon, id);
    if (t) return t->count;
    return 0;
}
int mon_tim_count(mon_ptr mon)
{
    return _count(mon);
}

/************************************************************************
 * System Hooks
 ************************************************************************/
static void _cleanup(mon_ptr mon)
{
    mon_tim_ptr t = mon->timers, p = NULL;
    while (t)
    {
        if (t->count <= 0 && !(t->flags & TF_LOCKED))
        {
            mon_tim_ptr x = t;
            if (!p) mon->timers = t->next;
            else p->next = t->next;
            _off(mon, t);
            t = t->next;
            free(x);
            check_mon_health_redraw(mon->id);
        }
        else
        {
            p = t;
            t = t->next;
        }
    }
}
/* [1] Note that tick_f might add a new timer:
 *     e.g. _ego_whip_tick->mon_take_hit->fear_p_hurt_m->mon_tim_add(T_FEAR)
 *     So we process all the ticks in pass one, and _cleanup() in pass two.
 *     New timers are added to the head so won't be processed until the next tick.
 *     cf _add_aux.
 * [2] Note that tick_f might kill the monster, which deletes its timer queue. */
void mon_tim_tick(mon_ptr mon)
{
    mon_tim_ptr t;
    if (mon_is_dead(mon)) return;
    mon->mflag |= MFLAG_TICK;
    for (t = mon->timers; t; t = t->next)
    {
        if (t->count > 0 && !(t->flags & TF_FAST_TICK))
        {
            mon_tim_info_ptr info = _find_info(t->id);
            if (info->tick_f)
            {
                info->tick_f(mon, t);
                if (mon_is_dead(mon)) return; /* dead */
            }
            else
                t->count--;
        }
    }
    _cleanup(mon);
    mon->mflag &= ~MFLAG_TICK;
}
void mon_tim_fast_tick(mon_ptr mon)
{
    mon_tim_ptr t;
    if (mon_is_dead(mon)) return;
    mon->mflag |= MFLAG_TICK;
    for (t = mon->timers; t; t = t->next)
    {
        if (t->count > 0 && (t->flags & TF_FAST_TICK))
        {
            mon_tim_info_ptr info = _find_info(t->id);
            if (info->tick_f)
            {
                info->tick_f(mon, t);
                if (mon_is_dead(mon)) return; /* dead */
            }
            else
                t->count--;
        }
    }
    _cleanup(mon);
    mon->mflag &= ~MFLAG_TICK;
}
void mon_tim_clear(mon_ptr mon)
{
    while (mon->timers)
    {
        mon_tim_ptr x = mon->timers;
        mon->timers = mon->timers->next;
        free(x);
    }
    if (mon->r_idx) check_mon_health_redraw(mon->id);
}
void mon_tim_display(mon_ptr mon, doc_ptr doc)
{
    mon_tim_ptr t;
    for (t = mon->timers; t; t = t->next)
    {
        mon_tim_info_ptr info = _find_info(t->id);
        if (info->display_f)
            info->display_f(mon, t, doc);
    }
}
void mon_tim_probe(mon_ptr mon, doc_ptr doc)
{
    mon_tim_ptr t;
    for (t = mon->timers; t; t = t->next)
    {
        mon_tim_info_ptr info = _find_info(t->id);
        if (info->probe_f)
            info->probe_f(mon, t, doc);
    }
}
/************************************************************************
 * Savefiles
 ************************************************************************/
void mon_tim_load(mon_ptr mon, savefile_ptr file)
{
    int ct = savefile_read_s16b(file), i;
    for (i = 0; i < ct; i++)
    {
        s16b e = savefile_read_s16b(file);
        s16b ct = savefile_read_s16b(file);
        mon_tim_ptr t = _alloc(e, ct);
        t->flags = savefile_read_u16b(file);
        t->parm = savefile_read_s16b(file);
        _add_aux(mon, t);
    }
}

void mon_tim_save(mon_ptr mon, savefile_ptr file)
{
    mon_tim_ptr t;
    savefile_write_s16b(file, _count(mon));
    for (t = mon->timers; t; t = t->next)
    {
        savefile_write_s16b(file, t->id);
        savefile_write_s16b(file, t->count);
        savefile_write_u16b(file, t->flags);
        savefile_write_s16b(file, t->parm);
    }
}

/************************************************************************
 * Timer Helpers
 ************************************************************************/
static bool _mon_show_msg(mon_ptr mon, mon_tim_ptr timer)
{
    if (!(cave->flags & DF_GENERATED)) return FALSE;
    if (timer->flags & TF_NO_MSG) return FALSE;
    return mon_show_msg(mon);
}
static cptr _mon_name_aux(mon_ptr mon, int options)
{
    static char buf[80];
    monster_desc(buf, mon, options);
    return buf;
}
static cptr _mon_name(mon_ptr mon)
{
    if (plr_attack_current())
        return _mon_name_aux(mon, MD_PRON_VISIBLE);
    return _mon_name_aux(mon, 0);
}
/************************************************************************
 * T_BERSERK
 ************************************************************************/
static void _berserk_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("RAAAARRRRGH! %^s is in a rage!", _mon_name(mon));
    mon->mspeed += 3;
    mon->ac_adj -= 20;
    mon_tim_delete(mon, T_FEAR);
}
static void _berserk_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s finally calms down.", _mon_name(mon));
    mon->mspeed -= 3;
    mon->ac_adj += 20;
}
static void _berserk_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_RED, 'B');
}
static void _berserk_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:r>Berserk (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _berserk(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_BERSERK, "Berserk");
    info->on_f = _berserk_on;
    info->off_f = _berserk_off;
    info->display_f = _berserk_display;
    info->probe_f = _berserk_probe;
    return info;
}
/************************************************************************
 * T_CONFUSED
 ************************************************************************/
static void _confused_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s looks <color:u>confused</color>.", _mon_name(mon));
}
static void _confused_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is no longer confused.", _mon_name(mon));
}
static void _confused_tick(mon_ptr mon, mon_tim_ptr timer)
{
    int lvl = mon_race(mon)->level;
    int dec = randint1(lvl/20 + 1);

    timer->count = MAX(0, timer->count - dec);
}
static void _confused_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_UMBER, 'C');
}
static void _confused_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:u>Confused (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _confused(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_CONFUSED, "Confused");
    info->on_f = _confused_on;
    info->off_f = _confused_off;
    info->tick_f = _confused_tick;
    info->display_f = _confused_display;
    info->probe_f = _confused_probe;
    info->flags = TF_BIFF;
    return info;
}
/************************************************************************
 * T_EGO_WHIP (cf psion.c for _EGO_WHIP. This is for the possessor.)
 ************************************************************************/
static void _ego_whip_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is lashed by an ego whip!", _mon_name(mon));
}
static void _ego_whip_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s shakes off the ego whip.", _mon_name(mon));
}
static void _ego_whip_tick(mon_ptr mon, mon_tim_ptr timer)
{
    int rlev = timer->parm;
    int dam = 120*rlev/100;
    bool fear = FALSE;
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is lashed!", _mon_name(mon));
    if (mon_take_hit(mon->id, dam, &fear, NULL)) return; /* dead */
    do {
        timer->count--;
    } while (timer->count > 0 && mon_save_aux(mon->r_idx, rlev));
}
static void _ego_whip_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_L_BLUE, 'W');
}
static void _ego_whip_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:B>Ego Whip (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _ego_whip(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_EGO_WHIP, "Ego Whip");
    info->on_f = _ego_whip_on;
    info->off_f = _ego_whip_off;
    info->tick_f = _ego_whip_tick;
    info->display_f = _ego_whip_display;
    info->probe_f = _ego_whip_probe;
    info->flags = TF_NO_DISPEL | TF_IGNORE | TF_FAST_TICK;
    return info;
}
/************************************************************************
 * T_FAST
 ************************************************************************/
static void _fast_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s starts moving faster.", _mon_name(mon));
    mon->mspeed += 10;
    if (p_ptr->riding == mon->id && !p_ptr->leaving)
        p_ptr->update |= PU_BONUS;
}
static void _fast_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is no longer fast.", _mon_name(mon));
    mon->mspeed -= 10;
    if (p_ptr->riding == mon->id && !p_ptr->leaving)
        p_ptr->update |= PU_BONUS;
}
static void _fast_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:y>Fast (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _fast(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_FAST, "Fast");
    info->on_f = _fast_on;
    info->off_f = _fast_off;
    info->probe_f = _fast_probe;
    return info;
}
/************************************************************************
 * T_FEAR
 ************************************************************************/
static void _fear_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is <color:R>frightened</color>.", _mon_name(mon));
}
static void _fear_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
    {
        char m_poss[80];
        monster_desc(m_poss, mon, MD_PRON_VISIBLE | MD_POSSESSIVE);
        msg_format("%^s recovers %s <color:G>courage</color>.", _mon_name(mon), m_poss);
    }
}
static void _fear_tick(mon_ptr mon, mon_tim_ptr timer)
{
    /* XXX fear_process_m and _process_monster and process_monster and mon_take_hit and ... */
}
static void _fear_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_VIOLET, 'F');
}
static void _fear_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:v>Fear (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _fear(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_FEAR, "Fear");
    info->on_f = _fear_on;
    info->off_f = _fear_off;
    info->tick_f = _fear_tick;
    info->display_f = _fear_display;
    info->probe_f = _fear_probe;
    info->flags = TF_BIFF;
    return info;
}
/************************************************************************
 * T_INVULN
 ************************************************************************/
static void _invuln_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s seems invincible!", _mon_name(mon));
}
static void _invuln_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s no longer seems invincible.", _mon_name(mon));
}
static void _invuln_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_WHITE, 'I');
}
static void _invuln_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:W>Invulnerable (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _invuln(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_INVULN, "Invulnerability");
    info->on_f = _invuln_on;
    info->off_f = _invuln_off;
    info->display_f = _invuln_display;
    info->probe_f = _invuln_probe;
    return info;
}
/************************************************************************
 * T_PARALYZED
 ************************************************************************/
static void _paralyzed_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
    {
        if (weaponmaster_is_(WEAPONMASTER_CLUBS) && plr_attack_current())
            msg_format("%^s is <color:b>knocked out</color>!", _mon_name(mon));
        else
            msg_format("%^s is <color:v>paralyzed</color>!", _mon_name(mon));
    }
}
static void _paralyzed_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s can move again.", _mon_name(mon));
}
static void _paralyzed_tick(mon_ptr mon, mon_tim_ptr timer)
{
    do {
        timer->count--;
    } while (timer->count > 0 && (mon_is_unique(mon) || mon_save_aux(mon->r_idx, 100)));
}
static void _paralyzed_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_VIOLET, 'P');
}
static void _paralyzed_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:v>Paralyzed (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _paralyzed(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_PARALYZED, "Paralyzed");
    info->on_f = _paralyzed_on;
    info->off_f = _paralyzed_off;
    info->tick_f = _paralyzed_tick;
    info->display_f = _paralyzed_display;
    info->probe_f = _paralyzed_probe;
    info->flags = TF_NO_DISPEL | TF_IGNORE | TF_FAST_TICK;
    return info;
}
/************************************************************************
 * MT_SLEEP
 ************************************************************************/
static void _sleep_change(mon_ptr mon)
{
    if (mon_race(mon)->flags7 & RF7_HAS_LD_MASK)
        p_ptr->update |= PU_MON_LITE;
    p_ptr->window |= PW_MONSTER_LIST;
}
static void _sleep_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s falls asleep.", _mon_name(mon));
    _sleep_change(mon);
}
static void _sleep_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s wakes up.", _mon_name(mon));
    _sleep_change(mon);
}
static int _sleep_odds(int stealth)
{
    static int _odds[31] = {  /* from spreadsheet: cube root of 2^(30-S) */
        1024, 813, 645, 512, 406, 323, 256, 203, 161, 128, 102,   /* Bad(0)        -> Superb(10) */
               81,  64,  51,  40,  32,  25,  20,  16,  13,  10,   /* Superb(11)    -> Amber[7](20) */
                8,   6,   5,   4,   3,   3,   2,   2,   1,   1 }; /* Amber[10](21) -> Amber[32](30) */
    if (stealth < 0) stealth = 0;
    if (stealth > 30) stealth = 30;
    return _odds[stealth];
}
static bool _sleep_check(int stealth)
{
    return randint0(1024) <= _sleep_odds(stealth);
}
static void _sleep_tick(mon_ptr mon, mon_tim_ptr timer)
{
    int aaf;
    int dis = mon->cdis + cave->plr_dis;
    mon_race_ptr race;

    if (dis >= AAF_LIMIT) return;
    race = mon_race(mon);
    aaf = race->aaf;
    if (is_pet(mon)) aaf = MIN(MAX_SIGHT, aaf);

    if (dis <= aaf || (dis <= MAX_SIGHT && plr_los(mon->pos)))
    {
        #if 0
        u32b notice = randint0(1024);
        u32b noise = (1L << (30 - p_ptr->skills.stl));

        if (notice * notice * notice <= noise)
        #endif
        if (_sleep_check(p_ptr->skills.stl))
        {
            /* Wake up faster near the player */
            int d = (dis < AAF_LIMIT / 2) ? (AAF_LIMIT / dis) : 1;

            /* Hack -- amount of "waking" is affected by speed of player */
            d = (d * SPEED_TO_ENERGY(p_ptr->pspeed)) / 10;
            if (d < 0) d = 1;

            #if 0
            msg_boundary();
            msg_format("<color:R>%^s (<color:D>%d, %d</color>)</color> notices you (<color:D>%d.%2d%%</color>): %d-%d=%d.",
                _mon_name_aux(mon, MD_ASSUME_VISIBLE), dis, race->aaf,
                _sleep_odds(p_ptr->skills.stl) * 100 / 1024,
                (_sleep_odds(p_ptr->skills.stl) * 10000 / 1024) % 100,
                timer->count, d, MAX(0, timer->count - d));
            msg_boundary();
            #endif
            timer->count = MAX(0, timer->count - d);
            if (timer->count)
            {
                if (is_original_ap_and_seen(mon))
                {
                    if (race->r_ignore < MAX_UCHAR) race->r_ignore++;
                }
            }
            else if (is_original_ap_and_seen(mon))
            {
                if (race->r_wake < MAX_UCHAR) race->r_wake++;
            }
        }
    }
}
static void _sleep_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_BLUE, 'Z'); /* ZZZ */
}
static void _sleep_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:b>Sleep (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _sleep(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(MT_SLEEP, "Sleep");
    info->on_f = _sleep_on;
    info->off_f = _sleep_off;
    info->tick_f = _sleep_tick;
    info->display_f = _sleep_display;
    info->probe_f = _sleep_probe;
    info->flags = TF_BIFF;
    return info;
}

/************************************************************************
 * T_SLOW
 ************************************************************************/
static void _slow_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s starts moving slower.", _mon_name(mon));
    mon->mspeed -= 10;
    if (p_ptr->riding == mon->id && !p_ptr->leaving)
        p_ptr->update |= PU_BONUS;
}
static void _slow_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is no longer slow.", _mon_name(mon));
    mon->mspeed += 10;
    if (p_ptr->riding == mon->id && !p_ptr->leaving)
        p_ptr->update |= PU_BONUS;
}
static void _slow_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_UMBER, 'S');
}
static void _slow_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:u>Slow (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _slow(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_SLOW, "Slow");
    info->on_f = _slow_on;
    info->off_f = _slow_off;
    info->probe_f = _slow_probe;
    info->display_f = _slow_display;
    /* XXX Historically, not a TF_BIFF. It can be dispelled! */
    info->flags = TF_AUGMENT; /* cf Rage-Mage armor of fury, for example */
    return info;
}

/************************************************************************
 * T_STUN
 ************************************************************************/
static void _stun_on(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is <color:B>stunned</color>.", _mon_name(mon));
}
static void _stun_off(mon_ptr mon, mon_tim_ptr timer)
{
    if (_mon_show_msg(mon, timer))
        msg_format("%^s is no longer stunned.", _mon_name(mon));
}
static void _stun_tick(mon_ptr mon, mon_tim_ptr timer)
{
    int rlev = mon_race(mon)->level;
    int dec = 1 + rlev/10;
    if (randint0(10000) < rlev * rlev) /* shake it off ... */
        dec = MAX(dec, timer->count/2);

    timer->count = MAX(0, timer->count - dec);
}
static void _stun_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:B>%d%%</color> ", timer->count);
}
static void _stun_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:B>Stunned (<color:w>%d%%</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _stun(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(T_STUN, "Stun");
    info->on_f = _stun_on;
    info->off_f = _stun_off;
    info->tick_f = _stun_tick;
    info->display_f = _stun_display;
    info->probe_f = _stun_probe;
    info->flags = TF_BIFF;
    return info;
}
